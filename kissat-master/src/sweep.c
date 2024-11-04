#include "sweep.h"
#include "dense.h"
#include "inline.h"
#include "kitten.h"
#include "logging.h"
#include "print.h"
#include "promote.h"
#include "propdense.h"
#include "proprobe.h"
#include "rank.h"
#include "report.h"
#include "terminate.h"

#include <inttypes.h>
#include <string.h>

typedef struct sweeper sweeper;

struct sweeper {
  kissat *solver;
  unsigned *depths;
  unsigned *reprs;
  unsigned *prev;
  unsigned *next;
  unsigned first;
  unsigned last;
  unsigned encoded;
  unsigned save;
  unsigneds vars;
  references refs;
  unsigneds clause;
  unsigneds backbone;
  unsigneds partition;
  unsigneds core[2];
  struct {
    uint64_t ticks;
    unsigned clauses, depth, vars;
  } limit;
};

static int sweep_solve (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  kitten *kitten = solver->kitten;
  kitten_randomize_phases (kitten);
  INC (sweep_solved);
  int res = kitten_solve (kitten);
  if (res == 10)
    INC (sweep_sat);
  if (res == 20)
    INC (sweep_unsat);
  return res;
}

static void set_kitten_ticks_limit (sweeper *sweeper) {
  uint64_t remaining = 0;
  kissat *solver = sweeper->solver;
  if (solver->statistics.kitten_ticks < sweeper->limit.ticks)
    remaining = sweeper->limit.ticks - solver->statistics.kitten_ticks;
  LOG ("'kitten_ticks' remaining %" PRIu64, remaining);
  kitten_set_ticks_limit (solver->kitten, remaining);
}

static bool kitten_ticks_limit_hit (sweeper *sweeper, const char *when) {
  kissat *solver = sweeper->solver;
  if (solver->statistics.kitten_ticks >= sweeper->limit.ticks) {
    LOG ("'kitten_ticks' limit of %" PRIu64 " ticks hit after %" PRIu64
         " ticks during %s",
         sweeper->limit.ticks, solver->statistics.kitten_ticks, when);
    return true;
  }
#ifndef LOGGING
  (void) when;
#endif
  return false;
}

static void init_sweeper (kissat *solver, sweeper *sweeper) {
  sweeper->solver = solver;
  sweeper->encoded = 0;
  CALLOC (sweeper->depths, VARS);
  NALLOC (sweeper->reprs, LITS);
  for (all_literals (lit))
    sweeper->reprs[lit] = lit;
  NALLOC (sweeper->prev, VARS);
  NALLOC (sweeper->next, VARS);
  for (all_variables (idx))
    sweeper->prev[idx] = sweeper->next[idx] = INVALID_IDX;
  sweeper->first = sweeper->last = INVALID_IDX;
  INIT_STACK (sweeper->vars);
  INIT_STACK (sweeper->refs);
  INIT_STACK (sweeper->clause);
  INIT_STACK (sweeper->backbone);
  INIT_STACK (sweeper->partition);
  INIT_STACK (sweeper->core[0]);
  INIT_STACK (sweeper->core[1]);
  assert (!solver->kitten);
  solver->kitten = kitten_embedded (solver);
  kitten_track_antecedents (solver->kitten);
  kissat_enter_dense_mode (solver, 0);
  kissat_connect_irredundant_large_clauses (solver);

  unsigned completed = solver->statistics.sweep_completed;
  if (completed > 32)
    completed = 32;

  uint64_t vars_limit = GET_OPTION (sweepvars);
  vars_limit <<= completed;
  const unsigned max_vars_limit = GET_OPTION (sweepmaxvars);
  if (vars_limit > max_vars_limit)
    vars_limit = max_vars_limit;
  sweeper->limit.vars = vars_limit;
  kissat_extremely_verbose (solver, "sweeper variable limit %u",
                            sweeper->limit.vars);

  uint64_t depth_limit = solver->statistics.sweep_completed;
  depth_limit += GET_OPTION (sweepdepth);
  const unsigned max_depth = GET_OPTION (sweepmaxdepth);
  if (depth_limit > max_depth)
    depth_limit = max_depth;
  sweeper->limit.depth = depth_limit;
  kissat_extremely_verbose (solver, "sweeper depth limit %u",
                            sweeper->limit.depth);

  uint64_t clause_limit = GET_OPTION (sweepclauses);
  clause_limit <<= completed;
  const unsigned max_clause_limit = GET_OPTION (sweepmaxclauses);
  if (clause_limit > max_clause_limit)
    clause_limit = max_clause_limit;
  sweeper->limit.clauses = clause_limit;
  kissat_extremely_verbose (solver, "sweeper clause limit %u",
                            sweeper->limit.clauses);

  SET_EFFORT_LIMIT (ticks_limit, sweep, kitten_ticks);
  sweeper->limit.ticks = ticks_limit;
  set_kitten_ticks_limit (sweeper);
}

static unsigned release_sweeper (sweeper *sweeper) {
  kissat *solver = sweeper->solver;

  unsigned merged = 0;
  for (all_variables (idx)) {
    if (!ACTIVE (idx))
      continue;
    const unsigned lit = LIT (idx);
    if (sweeper->reprs[lit] != lit)
      merged++;
  }
  DEALLOC (sweeper->depths, VARS);
  DEALLOC (sweeper->reprs, LITS);
  DEALLOC (sweeper->prev, VARS);
  DEALLOC (sweeper->next, VARS);
  RELEASE_STACK (sweeper->vars);
  RELEASE_STACK (sweeper->refs);
  RELEASE_STACK (sweeper->clause);
  RELEASE_STACK (sweeper->backbone);
  RELEASE_STACK (sweeper->partition);
  RELEASE_STACK (sweeper->core[0]);
  RELEASE_STACK (sweeper->core[1]);
  kitten_release (solver->kitten);
  solver->kitten = 0;
  kissat_resume_sparse_mode (solver, false, 0);
  return merged;
}

static void clear_sweeper (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  LOG ("clearing sweeping environment");
  kitten_clear (solver->kitten);
  kitten_track_antecedents (solver->kitten);
  for (all_stack (unsigned, idx, sweeper->vars)) {
    assert (sweeper->depths[idx]);
    sweeper->depths[idx] = 0;
  }
  CLEAR_STACK (sweeper->vars);
  for (all_stack (reference, ref, sweeper->refs)) {
    clause *c = kissat_dereference_clause (solver, ref);
    assert (c->sweeped);
    c->sweeped = false;
  }
  CLEAR_STACK (sweeper->refs);
  CLEAR_STACK (sweeper->backbone);
  CLEAR_STACK (sweeper->partition);
  sweeper->encoded = 0;
  set_kitten_ticks_limit (sweeper);
}

static unsigned sweep_repr (sweeper *sweeper, unsigned lit) {
  unsigned res;
  {
    unsigned prev = lit;
    while ((res = sweeper->reprs[prev]) != prev)
      prev = res;
  }
  if (res == lit)
    return res;
#if defined(LOGGING) || !defined(NDEBUG)
  kissat *solver = sweeper->solver;
#endif
  LOG ("sweeping repr[%s] = %s", LOGLIT (lit), LOGLIT (res));
  {
    const unsigned not_res = NOT (res);
    unsigned next, prev = lit;
    ;
    while ((next = sweeper->reprs[prev]) != res) {
      const unsigned not_prev = NOT (prev);
      sweeper->reprs[not_prev] = not_res;
      sweeper->reprs[prev] = res;
      prev = next;
    }
    assert (sweeper->reprs[NOT (prev)] == not_res);
  }
  return res;
}

static void add_literal_to_environment (sweeper *sweeper, unsigned depth,
                                        unsigned lit) {
  const unsigned repr = sweep_repr (sweeper, lit);
  if (repr != lit)
    return;
  kissat *solver = sweeper->solver;
  const unsigned idx = IDX (lit);
  if (sweeper->depths[idx])
    return;
  assert (depth < UINT_MAX);
  sweeper->depths[idx] = depth + 1;
  PUSH_STACK (sweeper->vars, idx);
  LOG ("sweeping[%u] adding literal %s", depth, LOGLIT (lit));
}

static void sweep_clause (sweeper *sweeper, unsigned depth) {
  kissat *solver = sweeper->solver;
  assert (SIZE_STACK (sweeper->clause) > 1);
  for (all_stack (unsigned, lit, sweeper->clause))
    add_literal_to_environment (sweeper, depth, lit);
  kitten_clause (solver->kitten, SIZE_STACK (sweeper->clause),
                 BEGIN_STACK (sweeper->clause));
  CLEAR_STACK (sweeper->clause);
  sweeper->encoded++;
}

static void sweep_binary (sweeper *sweeper, unsigned depth, unsigned lit,
                          unsigned other) {
  if (sweep_repr (sweeper, lit) != lit)
    return;
  if (sweep_repr (sweeper, other) != other)
    return;
  kissat *solver = sweeper->solver;
  LOGBINARY (lit, other, "sweeping[%u]", depth);
  value *values = solver->values;
  assert (!values[lit]);
  const value other_value = values[other];
  if (other_value > 0) {
    LOGBINARY (lit, other, "skipping satisfied");
    return;
  }
  const unsigned *depths = sweeper->depths;
  const unsigned other_idx = IDX (other);
  const unsigned other_depth = depths[other_idx];
  const unsigned lit_idx = IDX (lit);
  const unsigned lit_depth = depths[lit_idx];
  if (other_depth && other_depth < lit_depth) {
    LOGBINARY (lit, other, "skipping depth %u copied", other_depth);
    return;
  }
  assert (!other_value);
  assert (EMPTY_STACK (sweeper->clause));
  PUSH_STACK (sweeper->clause, lit);
  PUSH_STACK (sweeper->clause, other);
  sweep_clause (sweeper, depth);
}

static void sweep_reference (sweeper *sweeper, unsigned depth,
                             reference ref) {
  assert (EMPTY_STACK (sweeper->clause));
  kissat *solver = sweeper->solver;
  clause *c = kissat_dereference_clause (solver, ref);
  if (c->sweeped)
    return;
  if (c->garbage)
    return;
  LOGCLS (c, "sweeping[%u]", depth);
  value *values = solver->values;
  for (all_literals_in_clause (lit, c)) {
    const value value = values[lit];
    if (value > 0) {
      kissat_mark_clause_as_garbage (solver, c);
      CLEAR_STACK (sweeper->clause);
      return;
    }
    if (value < 0)
      continue;
    PUSH_STACK (sweeper->clause, lit);
  }
  PUSH_STACK (sweeper->refs, ref);
  c->sweeped = true;
  sweep_clause (sweeper, depth);
}

static void save_core_clause (void *state, bool learned, size_t size,
                              const unsigned *lits) {
  sweeper *sweeper = state;
  kissat *solver = sweeper->solver;
  if (solver->inconsistent)
    return;
  const value *const values = solver->values;
  unsigneds *core = sweeper->core + sweeper->save;
  size_t saved = SIZE_STACK (*core);
  const unsigned *end = lits + size;
  unsigned non_false = 0;
  for (const unsigned *p = lits; p != end; p++) {
    const unsigned lit = *p;
    const value value = values[lit];
    if (value > 0) {
      LOGLITS (size, lits, "extracted %s satisfied lemma", LOGLIT (lit));
      RESIZE_STACK (*core, saved);
      return;
    }
    PUSH_STACK (*core, lit);
    if (value < 0)
      continue;
    if (!learned && ++non_false > 1) {
      LOGLITS (size, lits, "ignoring extracted original clause");
      RESIZE_STACK (*core, saved);
      return;
    }
  }
#ifdef LOGGING
  unsigned *saved_lits = BEGIN_STACK (*core) + saved;
  size_t saved_size = SIZE_STACK (*core) - saved;
  LOGLITS (saved_size, saved_lits, "saved core[%u]", sweeper->save);
#endif
  PUSH_STACK (*core, INVALID_LIT);
}

static void add_core (sweeper *sweeper, unsigned core_idx) {
  kissat *solver = sweeper->solver;
  if (solver->inconsistent)
    return;
  LOG ("check and add extracted core[%u] lemmas to proof", core_idx);
  assert (core_idx == 0 || core_idx == 1);
  unsigneds *core = sweeper->core + core_idx;
  const value *const values = solver->values;

  unsigned *q = BEGIN_STACK (*core);
  const unsigned *const end_core = END_STACK (*core), *p = q;

  while (p != end_core) {
    const unsigned *c = p;
    while (*p != INVALID_LIT)
      p++;
#ifdef LOGGING
    size_t old_size = p - c;
    LOGLITS (old_size, c, "simplifying extracted core[%u] lemma", core_idx);
#endif
    bool satisfied = false;
    unsigned unit = INVALID_LIT;

    unsigned *d = q;

    for (const unsigned *l = c; !satisfied && l != p; l++) {
      const unsigned lit = *l;
      const value value = values[lit];
      if (value > 0) {
        satisfied = true;
        break;
      }
      if (!value)
        unit = *q++ = lit;
    }

    size_t new_size = q - d;
    p++;

    if (satisfied) {
      q = d;
      LOG ("not adding satisfied clause");
      continue;
    }

    if (!new_size) {
      LOG ("sweeping produced empty clause");
      CHECK_AND_ADD_EMPTY ();
      ADD_EMPTY_TO_PROOF ();
      solver->inconsistent = true;
      CLEAR_STACK (*core);
      return;
    }

    if (new_size == 1) {
      q = d;
      assert (unit != INVALID_LIT);
      LOG ("sweeping produced unit %s", LOGLIT (unit));
      CHECK_AND_ADD_UNIT (unit);
      ADD_UNIT_TO_PROOF (unit);
      kissat_assign_unit (solver, unit, "sweeping backbone reason");
      INC (sweep_units);
      continue;
    }

    *q++ = INVALID_LIT;

    assert (new_size > 1);
    LOGLITS (new_size, d, "adding extracted core[%u] lemma", core_idx);
    CHECK_AND_ADD_LITS (new_size, d);
    ADD_LITS_TO_PROOF (new_size, d);
  }
  SET_END_OF_STACK (*core, q);
#ifndef LOGGING
  (void) core_idx;
#endif
}

static void save_core (sweeper *sweeper, unsigned core) {
  kissat *solver = sweeper->solver;
  LOG ("saving extracted core[%u] lemmas", core);
  assert (core == 0 || core == 1);
  assert (EMPTY_STACK (sweeper->core[core]));
  sweeper->save = core;
  kitten_compute_clausal_core (solver->kitten, 0);
  kitten_traverse_core_clauses (solver->kitten, sweeper, save_core_clause);
}

static void clear_core (sweeper *sweeper, unsigned core_idx) {
  kissat *solver = sweeper->solver;
  if (solver->inconsistent)
    return;
#if defined(LOGGING) || !defined(NDEBUG) || !defined(NPROOFS)
  assert (core_idx == 0 || core_idx == 1);
  LOG ("clearing core[%u] lemmas", core_idx);
#endif
  unsigneds *core = sweeper->core + core_idx;
#ifdef CHECKING_OR_PROVING
  LOG ("deleting sub-solver core clauses");
  const unsigned *const end = END_STACK (*core);
  const unsigned *c = BEGIN_STACK (*core);
  for (const unsigned *p = c; c != end; c = ++p) {
    while (*p != INVALID_LIT)
      p++;
    const size_t size = p - c;
    assert (size > 1);
    REMOVE_CHECKER_LITS (size, c);
    DELETE_LITS_FROM_PROOF (size, c);
  }
#endif
  CLEAR_STACK (*core);
}

static void save_add_clear_core (sweeper *sweeper) {
  save_core (sweeper, 0);
  add_core (sweeper, 0);
  clear_core (sweeper, 0);
}

#define LOGBACKBONE(MESSAGE) \
  LOGLITSET (SIZE_STACK (sweeper->backbone), \
             BEGIN_STACK (sweeper->backbone), MESSAGE)

#define LOGPARTITION(MESSAGE) \
  LOGLITPART (SIZE_STACK (sweeper->partition), \
              BEGIN_STACK (sweeper->partition), MESSAGE)

static void init_backbone_and_partition (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  LOG ("initializing backbone and equivalent literals candidates");
  for (all_stack (unsigned, idx, sweeper->vars)) {
    if (!ACTIVE (idx))
      continue;
    const unsigned lit = LIT (idx);
    const unsigned not_lit = NOT (lit);
    const signed char tmp = kitten_value (solver->kitten, lit);
    const unsigned candidate = (tmp < 0) ? not_lit : lit;
    LOG ("sweeping candidate %s", LOGLIT (candidate));
    PUSH_STACK (sweeper->backbone, candidate);
    PUSH_STACK (sweeper->partition, candidate);
  }
  PUSH_STACK (sweeper->partition, INVALID_LIT);

  LOGBACKBONE ("initialized backbone candidates");
  LOGPARTITION ("initialized equivalence candidates");
}

static void sweep_empty_clause (sweeper *sweeper) {
  assert (!sweeper->solver->inconsistent);
  save_add_clear_core (sweeper);
  assert (sweeper->solver->inconsistent);
}

static void sweep_refine_partition (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  LOG ("refining partition");
  kitten *kitten = solver->kitten;
  unsigneds old_partition = sweeper->partition;
  unsigneds new_partition;
  INIT_STACK (new_partition);
  const value *const values = solver->values;
  const unsigned *const old_begin = BEGIN_STACK (old_partition);
  const unsigned *const old_end = END_STACK (old_partition);
#ifdef LOGGING
  unsigned old_classes = 0;
  unsigned new_classes = 0;
#endif
  for (const unsigned *p = old_begin, *q; p != old_end; p = q + 1) {
    unsigned assigned_true = 0, other;
    for (q = p; (other = *q) != INVALID_LIT; q++) {
      if (sweep_repr (sweeper, other) != other)
        continue;
      if (values[other])
        continue;
      signed char value = kitten_value (kitten, other);
      if (!value)
        LOG ("dropping sub-solver unassigned %s", LOGLIT (other));
      else if (value > 0) {
        PUSH_STACK (new_partition, other);
        assigned_true++;
      }
    }
#ifdef LOGGING
    LOG ("refining class %u of size %zu", old_classes, (size_t) (q - p));
    old_classes++;
#endif
    if (assigned_true == 0)
      LOG ("no positive literal in class");
    else if (assigned_true == 1) {
#ifdef LOGGING
      other =
#else
      (void)
#endif
          POP_STACK (new_partition);
      LOG ("dropping singleton class %s", LOGLIT (other));
    } else {
      LOG ("%u positive literal in class", assigned_true);
      PUSH_STACK (new_partition, INVALID_LIT);
#ifdef LOGGING
      new_classes++;
#endif
    }

    unsigned assigned_false = 0;
    for (q = p; (other = *q) != INVALID_LIT; q++) {
      if (sweep_repr (sweeper, other) != other)
        continue;
      if (values[other])
        continue;
      signed char value = kitten_value (kitten, other);
      if (value < 0) {
        PUSH_STACK (new_partition, other);
        assigned_false++;
      }
    }

    if (assigned_false == 0)
      LOG ("no negative literal in class");
    else if (assigned_false == 1) {
#ifdef LOGGING
      other =
#else
      (void)
#endif
          POP_STACK (new_partition);
      LOG ("dropping singleton class %s", LOGLIT (other));
    } else {
      LOG ("%u negative literal in class", assigned_false);
      PUSH_STACK (new_partition, INVALID_LIT);
#ifdef LOGGING
      new_classes++;
#endif
    }
  }
  RELEASE_STACK (old_partition);
  sweeper->partition = new_partition;
  LOG ("refined %u classes into %u", old_classes, new_classes);
  LOGPARTITION ("refined equivalence candidates");
}

static void sweep_refine_backbone (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  LOG ("refining backbone candidates");
  const unsigned *const end = END_STACK (sweeper->backbone);
  unsigned *q = BEGIN_STACK (sweeper->backbone);
  const value *const values = solver->values;
  kitten *kitten = solver->kitten;
  for (const unsigned *p = q; p != end; p++) {
    const unsigned lit = *p;
    if (values[lit])
      continue;
    signed char value = kitten_value (kitten, lit);
    if (!value)
      LOG ("dropping sub-solver unassigned %s", LOGLIT (lit));
    else if (value >= 0)
      *q++ = lit;
  }
  SET_END_OF_STACK (sweeper->backbone, q);
  LOGBACKBONE ("refined backbone candidates");
}

static void sweep_refine (sweeper *sweeper) {
#ifdef LOGGING
  kissat *solver = sweeper->solver;
#endif
  if (EMPTY_STACK (sweeper->backbone))
    LOG ("no need to refine empty backbone candidates");
  else
    sweep_refine_backbone (sweeper);
  if (EMPTY_STACK (sweeper->partition))
    LOG ("no need to refine empty partition candidates");
  else
    sweep_refine_partition (sweeper);
}

static void flip_backbone_literals (struct sweeper *sweeper) {
  struct kissat *solver = sweeper->solver;
  const unsigned max_rounds = GET_OPTION (sweepfliprounds);
  if (!max_rounds)
    return;
  assert (!EMPTY_STACK (sweeper->backbone));
  struct kitten *kitten = solver->kitten;
  if (kitten_status (kitten) != 10)
    return;
#ifdef LOGGING
  unsigned total_flipped = 0;
#endif
  unsigned flipped, round = 0;
  do {
    round++;
    flipped = 0;
    unsigned *begin = BEGIN_STACK (sweeper->backbone), *q = begin;
    const unsigned *const end = END_STACK (sweeper->backbone), *p = q;
    while (p != end) {
      const unsigned lit = *p++;
      if (kitten_flip_literal (kitten, lit)) {
        LOG ("flipping backbone candidate %s succeeded", LOGLIT (lit));
#ifdef LOGGING
        total_flipped++;
#endif
        flipped++;
      } else {
        LOG ("flipping backbone candidate %s failed", LOGLIT (lit));
        *q++ = lit;
      }
    }
    SET_END_OF_STACK (sweeper->backbone, q);
    LOG ("flipped %u backbone candidates in round %u", flipped, round);

    if (TERMINATED (sweep_terminated_1))
      break;
    if (solver->statistics.kitten_ticks > sweeper->limit.ticks)
      break;
  } while (flipped && round < max_rounds);
  LOG ("flipped %u backbone candidates in total in %u rounds",
       total_flipped, round);
}

static bool sweep_backbone_candidate (sweeper *sweeper, unsigned lit) {
  kissat *solver = sweeper->solver;
  LOG ("trying backbone candidate %s", LOGLIT (lit));
  kitten *kitten = solver->kitten;
  if (kitten_status (kitten) == 10 && kitten_flip_literal (kitten, lit)) {
    LOG ("flipping %s succeeded", LOGLIT (lit));
    LOGBACKBONE ("refined backbone candidates");
    return false;
  }

  LOG ("flipping %s failed", LOGLIT (lit));
  const unsigned not_lit = NOT (lit);
  kitten_assume (kitten, not_lit);
  int res = sweep_solve (sweeper);
  if (res == 10) {
    LOG ("sweeping backbone candidate %s failed", LOGLIT (lit));
    sweep_refine (sweeper);
    return false;
  }

  if (res == 20) {
    LOG ("sweep unit %s", LOGLIT (lit));
    save_add_clear_core (sweeper);
    return true;
  }

  LOG ("sweeping backbone candidate %s failed", LOGLIT (lit));
  return false;
}

static void add_binary (kissat *solver, unsigned lit, unsigned other) {
  kissat_new_binary_clause (solver, lit, other);
}

static void enqueue_variable_last (sweeper *sweeper, unsigned idx) {
  assert (sweeper->prev[idx] == INVALID_IDX);
  assert (sweeper->next[idx] == INVALID_IDX);

  const unsigned last = sweeper->last;
  if (last == INVALID_IDX) {
    assert (sweeper->first == INVALID_IDX);
    sweeper->first = idx;
  } else {
    assert (sweeper->next[last] == INVALID_IDX);
    sweeper->next[last] = idx;
  }
  sweeper->prev[idx] = last;
  sweeper->last = idx;
}

static void enqueue_variable_first (sweeper *sweeper, unsigned idx) {
  assert (sweeper->prev[idx] == INVALID_IDX);
  assert (sweeper->next[idx] == INVALID_IDX);

  const unsigned first = sweeper->first;
  if (first == INVALID_IDX) {
    assert (sweeper->last == INVALID_IDX);
    sweeper->last = idx;
  } else {
    assert (sweeper->prev[first] == INVALID_IDX);
    sweeper->prev[first] = idx;
  }
  sweeper->next[idx] = first;
  sweeper->first = idx;
}

static void dequeue_variable (sweeper *sweeper, unsigned idx) {
  const unsigned prev = sweeper->prev[idx];
  const unsigned next = sweeper->next[idx];
  if (prev == INVALID_IDX) {
    assert (sweeper->first == idx);
    sweeper->first = next;
  } else {
    assert (sweeper->next[prev] == idx);
    sweeper->next[prev] = next;
    sweeper->prev[idx] = INVALID_IDX;
  }
  if (next == INVALID_IDX) {
    assert (sweeper->last == idx);
    sweeper->last = prev;
  } else {
    assert (sweeper->prev[next] == idx);
    sweeper->prev[next] = prev;
    sweeper->next[idx] = INVALID_IDX;
  }
}

static bool scheduled_variable (sweeper *sweeper, unsigned idx) {
  if (sweeper->last == idx)
    return true;
  return sweeper->next[idx] != INVALID_IDX;
}

static bool schedule_variable (sweeper *sweeper, unsigned idx) {
  kissat *solver = sweeper->solver;
  assert (idx < VARS);
  if (!ACTIVE (idx))
    return false;
  if (sweeper->last == idx)
    return true;
  if (sweeper->next[idx] == INVALID_IDX) {
    LOG ("front scheduling sweeping candidate %s", LOGVAR (idx));
    enqueue_variable_last (sweeper, idx);
  } else {
    LOG ("moving sweeping candidate %s to front", LOGVAR (idx));
    dequeue_variable (sweeper, idx);
    enqueue_variable_last (sweeper, idx);
  }
  return true;
}

static void schedule_literal (sweeper *sweeper, unsigned lit) {
#ifndef NDEBUG
  kissat *solver = sweeper->solver;
#endif
  assert (lit < LITS);
  schedule_variable (sweeper, IDX (lit));
}

static void substitute_connected_clauses (sweeper *sweeper, unsigned lit,
                                          unsigned repr) {
  kissat *solver = sweeper->solver;
  if (solver->inconsistent)
    return;
  value *const values = solver->values;
  if (values[lit])
    return;
  if (values[repr])
    return;
  LOG ("substituting %s with %s in all irredundant clauses", LOGLIT (lit),
       LOGLIT (repr));

  assert (lit != repr);
  assert (lit != NOT (repr));

#ifdef CHECKING_OR_PROVING
  const bool checking_or_proving = kissat_checking_or_proving (solver);
  assert (EMPTY_STACK (solver->added));
  assert (EMPTY_STACK (solver->removed));
#endif

  unsigneds *const delayed = &solver->delayed;
  assert (EMPTY_STACK (*delayed));

  {
    watches *lit_watches = &WATCHES (lit);
    watch *const begin_watches = BEGIN_WATCHES (*lit_watches);
    const watch *const end_watches = END_WATCHES (*lit_watches);

    watch *q = begin_watches;
    const watch *p = q;

    while (p != end_watches) {
      const watch head = *q++ = *p++;
      if (head.type.binary) {
        const unsigned other = head.binary.lit;
        const value other_value = values[other];
        if (other == NOT (repr))
          continue;
        if (other_value < 0)
          break;
        if (other_value > 0)
          continue;
        if (other == repr) {
          CHECK_AND_ADD_UNIT (lit);
          ADD_UNIT_TO_PROOF (lit);
          kissat_assign_unit (solver, lit, "substituted binary clause");
          INC (sweep_units);
          break;
        }
        CHECK_AND_ADD_BINARY (repr, other);
        ADD_BINARY_TO_PROOF (repr, other);
        REMOVE_CHECKER_BINARY (lit, other);
        DELETE_BINARY_FROM_PROOF (lit, other);
        PUSH_STACK (*delayed, head.raw);
        watch src = {.raw = head.raw};
        watch dst = {.raw = head.raw};
        src.binary.lit = lit;
        dst.binary.lit = repr;
        watches *other_watches = &WATCHES (other);
        kissat_substitute_large_watch (solver, other_watches, src, dst);
        q--;
      } else {
        const reference ref = head.large.ref;
        assert (EMPTY_STACK (sweeper->clause));
        clause *c = kissat_dereference_clause (solver, ref);
        if (c->garbage)
          continue;

        bool satisfied = false;
        bool repr_already_watched = false;
        const unsigned not_repr = NOT (repr);
#ifndef NDEBUG
        bool found = false;
#endif
        for (all_literals_in_clause (other, c)) {
          if (other == lit) {
#ifndef NDEBUG
            assert (!found);
            found = true;
#endif
            PUSH_STACK (solver->clause, repr);
            continue;
          }
          assert (other != NOT (lit));
          if (other == repr) {
            assert (!repr_already_watched);
            repr_already_watched = true;
            continue;
          }
          if (other == not_repr) {
            satisfied = true;
            break;
          }
          const value tmp = values[other];
          if (tmp < 0)
            continue;
          if (tmp > 0) {
            satisfied = true;
            break;
          }
          PUSH_STACK (solver->clause, other);
        }

        if (satisfied) {
          CLEAR_STACK (solver->clause);
          kissat_mark_clause_as_garbage (solver, c);
          continue;
        }
        assert (found);

        const unsigned new_size = SIZE_STACK (solver->clause);

        if (new_size == 0) {
          LOGCLS (c, "substituted empty clause");
          assert (!solver->inconsistent);
          solver->inconsistent = true;
          CHECK_AND_ADD_EMPTY ();
          ADD_EMPTY_TO_PROOF ();
          break;
        }

        if (new_size == 1) {
          LOGCLS (c, "reduces to unit");
          const unsigned unit = POP_STACK (solver->clause);
          CHECK_AND_ADD_UNIT (unit);
          ADD_UNIT_TO_PROOF (unit);
          kissat_assign_unit (solver, unit, "substituted large clause");
          INC (sweep_units);
          break;
        }

        CHECK_AND_ADD_STACK (solver->clause);
        ADD_STACK_TO_PROOF (solver->clause);
        REMOVE_CHECKER_CLAUSE (c);
        DELETE_CLAUSE_FROM_PROOF (c);

        if (!c->redundant)
          kissat_mark_added_literals (solver, new_size,
                                      BEGIN_STACK (solver->clause));

        if (new_size == 2) {
          const unsigned second = POP_STACK (solver->clause);
          const unsigned first = POP_STACK (solver->clause);
          LOGCLS (c, "reduces to binary clause %s %s", LOGLIT (first),
                  LOGLIT (second));
          assert (first == repr || second == repr);
          const unsigned other = first ^ second ^ repr;
          const watch src = {.raw = head.raw};
          watch dst = kissat_binary_watch (repr);
          watches *other_watches = &WATCHES (other);
          kissat_substitute_large_watch (solver, other_watches, src, dst);
          assert (solver->statistics.clauses_irredundant);
          solver->statistics.clauses_irredundant--;
          assert (solver->statistics.clauses_binary < UINT64_MAX);
          solver->statistics.clauses_binary++;
          dst.binary.lit = other;
          PUSH_STACK (*delayed, dst.raw);
          const size_t bytes = kissat_actual_bytes_of_clause (c);
          ADD (arena_garbage, bytes);
          c->garbage = true;
          q--;
          continue;
        }

        assert (2 < new_size);
        const unsigned old_size = c->size;
        assert (new_size <= old_size);

        const unsigned *const begin = BEGIN_STACK (solver->clause);
        const unsigned *const end = END_STACK (solver->clause);

        unsigned *lits = c->lits;
        unsigned *q = lits;

        for (const unsigned *p = begin; p != end; p++) {
          const unsigned other = *p;
          *q++ = other;
        }

        if (new_size < old_size) {
          c->size = new_size;
          c->searched = 2;
          if (c->redundant && c->glue >= new_size)
            kissat_promote_clause (solver, c, new_size - 1);
          if (!c->shrunken) {
            c->shrunken = true;
            lits[old_size - 1] = INVALID_LIT;
          }
        }

        LOGCLS (c, "substituted");

        if (!repr_already_watched)
          PUSH_STACK (*delayed, head.raw);
        CLEAR_STACK (solver->clause);
        q--;
      }
    }
    while (p != end_watches)
      *q++ = *p++;
    SET_END_OF_WATCHES (*lit_watches, q);
  }
  {
    const unsigned *const begin_delayed = BEGIN_STACK (*delayed);
    const unsigned *const end_delayed = END_STACK (*delayed);
    for (const unsigned *p = begin_delayed; p != end_delayed; p++) {
      const watch head = {.raw = *p};
      watches *repr_watches = &WATCHES (repr);
      PUSH_WATCHES (*repr_watches, head);
    }

    CLEAR_STACK (*delayed);
  }

#ifdef CHECKING_OR_PROVING
  if (checking_or_proving) {
    CLEAR_STACK (solver->added);
    CLEAR_STACK (solver->removed);
  }
#endif
}

static void sweep_remove (sweeper *sweeper, unsigned lit) {
  kissat *solver = sweeper->solver;
  assert (sweeper->reprs[lit] != lit);
  unsigneds *partition = &sweeper->partition;
  unsigned *const begin_partition = BEGIN_STACK (*partition), *p;
  const unsigned *const end_partition = END_STACK (*partition);
  for (p = begin_partition; *p != lit; p++)
    assert (p + 1 != end_partition);
  unsigned *begin_class = p;
  while (begin_class != begin_partition && begin_class[-1] != INVALID_LIT)
    begin_class--;
  const unsigned *end_class = p;
  while (*end_class != INVALID_LIT)
    end_class++;
  const unsigned size = end_class - begin_class;
  LOG ("removing non-representative %s from equivalence class of size %u",
       LOGLIT (lit), size);
  assert (size > 1);
  unsigned *q = begin_class;
  if (size == 2) {
    LOG ("completely squashing equivalence class of %s", LOGLIT (lit));
    for (const unsigned *r = end_class + 1; r != end_partition; r++)
      *q++ = *r;
  } else {
    for (const unsigned *r = begin_class; r != end_partition; r++)
      if (r != p)
        *q++ = *r;
  }
  SET_END_OF_STACK (*partition, q);
#ifndef LOGGING
  (void) solver;
#endif
}

static void flip_partition_literals (struct sweeper *sweeper) {
  struct kissat *solver = sweeper->solver;
  const unsigned max_rounds = GET_OPTION (sweepfliprounds);
  if (!max_rounds)
    return;
  assert (!EMPTY_STACK (sweeper->partition));
  struct kitten *kitten = solver->kitten;
  if (kitten_status (kitten) != 10)
    return;
#ifdef LOGGING
  unsigned total_flipped = 0;
#endif
  unsigned flipped, round = 0;
  do {
    round++;
    flipped = 0;
    unsigned *begin = BEGIN_STACK (sweeper->partition), *dst = begin;
    const unsigned *const end = END_STACK (sweeper->partition), *src = dst;
    while (src != end) {
      const unsigned *end_src = src;
      while (assert (end_src != end), *end_src != INVALID_LIT)
        end_src++;
      unsigned size = end_src - src;
      assert (size > 1);
      unsigned *q = dst;
      for (const unsigned *p = src; p != end_src; p++) {
        const unsigned lit = *p;
        if (kitten_flip_literal (kitten, lit)) {
          LOG ("flipping equivalence candidate %s succeeded", LOGLIT (lit));
#ifdef LOGGING
          total_flipped++;
#endif
          flipped++;
          if (--size < 2)
            break;
        } else {
          LOG ("flipping equivalence candidate %s failed", LOGLIT (lit));
          *q++ = lit;
        }
      }
      if (size > 1) {
        *q++ = INVALID_LIT;
        dst = q;
      }
      src = end_src + 1;
    }
    SET_END_OF_STACK (sweeper->partition, dst);
    LOG ("flipped %u equivalence candidates in round %u", flipped, round);

    if (TERMINATED (sweep_terminated_2))
      break;
    if (solver->statistics.kitten_ticks > sweeper->limit.ticks)
      break;
  } while (flipped && round < max_rounds);
  LOG ("flipped %u equivalence candidates in total in %u rounds",
       total_flipped, round);
}

static bool sweep_equivalence_candidates (sweeper *sweeper, unsigned lit,
                                          unsigned other) {
  kissat *solver = sweeper->solver;
  LOG ("trying equivalence candidates %s = %s", LOGLIT (lit),
       LOGLIT (other));
  const unsigned not_other = NOT (other);
  const unsigned not_lit = NOT (lit);
  kitten *kitten = solver->kitten;
  const unsigned *const begin = BEGIN_STACK (sweeper->partition);
  unsigned *const end = END_STACK (sweeper->partition);
  assert (begin + 3 <= end);
  assert (end[-3] == lit);
  assert (end[-2] == other);
  const unsigned third = (end - begin == 3) ? INVALID_LIT : end[-4];
  const int status = kitten_status (kitten);
  if (status == 10 && kitten_flip_literal (kitten, lit)) {
    LOG ("flipping %s succeeded", LOGLIT (lit));
    if (third == INVALID_LIT) {
      LOG ("squashing equivalence class of %s", LOGLIT (lit));
      SET_END_OF_STACK (sweeper->partition, end - 3);
    } else {
      LOG ("removing %s from equivalence class of %s", LOGLIT (lit),
           LOGLIT (other));
      end[-3] = other;
      end[-2] = INVALID_LIT;
      SET_END_OF_STACK (sweeper->partition, end - 1);
    }
    LOGPARTITION ("refined equivalence candidates");
    return false;
  } else if (status == 10 && kitten_flip_literal (kitten, other)) {
    LOG ("flipping %s succeeded", LOGLIT (other));
    if (third == INVALID_LIT) {
      LOG ("squashing equivalence class of %s", LOGLIT (lit));
      SET_END_OF_STACK (sweeper->partition, end - 3);
    } else {
      LOG ("removing %s from equivalence class of %s", LOGLIT (other),
           LOGLIT (lit));
      end[-2] = INVALID_LIT;
      SET_END_OF_STACK (sweeper->partition, end - 1);
    }
    LOGPARTITION ("refined equivalence candidates");
    return false;
  }
  LOG ("flipping %s and %s both failed", LOGLIT (lit), LOGLIT (other));
  kitten_assume (kitten, not_lit);
  kitten_assume (kitten, other);
  int res = sweep_solve (sweeper);
  if (res == 10) {
    LOG ("first sweeping implication %s -> %s failed", LOGLIT (other),
         LOGLIT (lit));
    sweep_refine (sweeper);
  } else if (!res) {
    LOG ("first sweeping implication %s -> %s hit ticks limit",
         LOGLIT (other), LOGLIT (lit));
  }

  if (res != 20)
    return false;

  LOG ("first sweeping implication %s -> %s succeeded", LOGLIT (other),
       LOGLIT (lit));

  save_core (sweeper, 0);

  kitten_assume (kitten, lit);
  kitten_assume (kitten, not_other);
  res = sweep_solve (sweeper);
  if (res == 10) {
    LOG ("second sweeping implication %s <- %s failed", LOGLIT (other),
         LOGLIT (lit));
    sweep_refine (sweeper);
  } else if (!res) {
    LOG ("second sweeping implication %s <- %s hit ticks limit",
         LOGLIT (other), LOGLIT (lit));
  }

  if (res != 20) {
    CLEAR_STACK (sweeper->core[0]);
    return false;
  }

  LOG ("second sweeping implication %s <- %s succeeded too", LOGLIT (other),
       LOGLIT (lit));

  save_core (sweeper, 1);

  LOG ("sweep equivalence %s = %s", LOGLIT (lit), LOGLIT (other));
  INC (sweep_equivalences);

  add_core (sweeper, 0);
  add_binary (solver, lit, not_other);
  clear_core (sweeper, 0);

  add_core (sweeper, 1);
  add_binary (solver, not_lit, other);
  clear_core (sweeper, 1);

  unsigned repr;
  if (lit < other) {
    repr = sweeper->reprs[other] = lit;
    sweeper->reprs[not_other] = not_lit;
    substitute_connected_clauses (sweeper, other, lit);
    substitute_connected_clauses (sweeper, not_other, not_lit);
    sweep_remove (sweeper, other);
  } else {
    repr = sweeper->reprs[lit] = other;
    sweeper->reprs[not_lit] = not_other;
    substitute_connected_clauses (sweeper, lit, other);
    substitute_connected_clauses (sweeper, not_lit, not_other);
    sweep_remove (sweeper, lit);
  }
  schedule_literal (sweeper, repr);

  return true;
}

static const char *sweep_variable (sweeper *sweeper, unsigned idx) {
  kissat *solver = sweeper->solver;
  assert (!solver->inconsistent);
  if (!ACTIVE (idx))
    return "inactive variable";
  const unsigned start = LIT (idx);
  if (sweeper->reprs[start] != start)
    return "non-representative variable";
  assert (EMPTY_STACK (sweeper->vars));
  assert (EMPTY_STACK (sweeper->refs));
  assert (EMPTY_STACK (sweeper->backbone));
  assert (EMPTY_STACK (sweeper->partition));
  assert (!sweeper->encoded);

  INC (sweep_variables);

  LOG ("sweeping %s", LOGVAR (idx));
  assert (!VALUE (start));
  LOG ("starting sweeping[0]");
  add_literal_to_environment (sweeper, 0, start);
  LOG ("finished sweeping[0]");
  LOG ("starting sweeping[1]");

  bool limit_reached = false;
  size_t expand = 0, next = 1;
  bool success = false;
  unsigned depth = 1;

  while (!limit_reached) {
    if (sweeper->encoded >= sweeper->limit.clauses) {
      LOG ("environment clause limit reached");
      limit_reached = true;
      break;
    }
    if (expand == next) {
      LOG ("finished sweeping[%u]", depth);
      if (depth >= sweeper->limit.depth) {
        LOG ("environment depth limit reached");
        break;
      }
      next = SIZE_STACK (sweeper->vars);
      if (expand == next) {
        LOG ("completely copied all clauses");
        break;
      }
      depth++;
      LOG ("starting sweeping[%u]", depth);
    }
    const unsigned idx = PEEK_STACK (sweeper->vars, expand);
    LOG ("traversing and adding clauses of %s", LOGVAR (idx));
    for (unsigned sign = 0; sign < 2; sign++) {
      const unsigned lit = LIT (idx) + sign;
      watches *watches = &WATCHES (lit);
      for (all_binary_large_watches (watch, *watches)) {
        if (watch.type.binary) {
          const unsigned other = watch.binary.lit;
          sweep_binary (sweeper, depth, lit, other);
        } else {
          reference ref = watch.large.ref;
          sweep_reference (sweeper, depth, ref);
        }
        if (SIZE_STACK (sweeper->vars) >= sweeper->limit.vars) {
          LOG ("environment variable limit reached");
          limit_reached = true;
          break;
        }
      }
      if (limit_reached)
        break;
    }
    expand++;
  }
  kissat_extremely_verbose (solver,
                            "sweeping variable %d environment of "
                            "%zu variables %u clauses depth %u",
                            kissat_export_literal (solver, LIT (idx)),
                            SIZE_STACK (sweeper->vars), sweeper->encoded,
                            depth);
  int res = sweep_solve (sweeper);
  LOG ("sub-solver returns '%d'", res);
  if (res == 10) {
    init_backbone_and_partition (sweeper);
#ifndef QUIET
    uint64_t units = solver->statistics.sweep_units;
    uint64_t solved = solver->statistics.sweep_solved;
#endif
    while (!EMPTY_STACK (sweeper->backbone)) {
      if (solver->inconsistent || TERMINATED (sweep_terminated_3) ||
          kitten_ticks_limit_hit (sweeper, "backbone refinement")) {
        limit_reached = true;
        goto DONE;
      }
      flip_backbone_literals (sweeper);
      if (TERMINATED (sweep_terminated_4) ||
          kitten_ticks_limit_hit (sweeper, "backbone refinement")) {
        limit_reached = true;
        goto DONE;
      }
      if (EMPTY_STACK (sweeper->backbone))
        break;
      const unsigned lit = POP_STACK (sweeper->backbone);
      if (!ACTIVE (IDX (lit)))
        continue;
      if (sweep_backbone_candidate (sweeper, lit))
        success = true;
    }
#ifndef QUIET
    units = solver->statistics.sweep_units - units;
    solved = solver->statistics.sweep_solved - solved;
    kissat_extremely_verbose (
        solver,
        "complete swept variable %d backbone with %" PRIu64
        " units in %" PRIu64 " solver calls",
        kissat_export_literal (solver, LIT (idx)), units, solved);
#endif
    assert (EMPTY_STACK (sweeper->backbone));
#ifndef QUIET
    uint64_t equivalences = solver->statistics.sweep_equivalences;
    solved = solver->statistics.sweep_solved;
#endif
    while (!EMPTY_STACK (sweeper->partition)) {
      if (solver->inconsistent || TERMINATED (sweep_terminated_5) ||
          kitten_ticks_limit_hit (sweeper, "partition refinement")) {
        limit_reached = true;
        goto DONE;
      }
      flip_partition_literals (sweeper);
      if (TERMINATED (sweep_terminated_6) ||
          kitten_ticks_limit_hit (sweeper, "backbone refinement")) {
        limit_reached = true;
        goto DONE;
      }
      if (EMPTY_STACK (sweeper->partition))
        break;
      if (SIZE_STACK (sweeper->partition) > 2) {
        const unsigned *end = END_STACK (sweeper->partition);
        assert (end[-1] == INVALID_LIT);
        unsigned lit = end[-3];
        unsigned other = end[-2];
        if (sweep_equivalence_candidates (sweeper, lit, other))
          success = true;
      } else
        CLEAR_STACK (sweeper->partition);
    }
#ifndef QUIET
    equivalences = solver->statistics.sweep_equivalences - equivalences;
    solved = solver->statistics.sweep_solved - solved;
    if (equivalences)
      kissat_extremely_verbose (
          solver,
          "complete swept variable %d partition with %" PRIu64
          " equivalences in %" PRIu64 " solver calls",
          kissat_export_literal (solver, LIT (idx)), equivalences, solved);
#endif
  } else if (res == 20)
    sweep_empty_clause (sweeper);

DONE:
  if (!success) {
#ifndef QUIET
    size_t unscheduled = 0;
#endif
    for (all_stack (unsigned, idx, sweeper->vars))
      if (scheduled_variable (sweeper, idx)) {
        LOG ("unscheduled %s", LOGVAR (idx));
        dequeue_variable (sweeper, idx);
#ifndef QUIET
        unscheduled++;
#endif
      }
    kissat_extremely_verbose (solver, "unscheduled %zu sweeping variables",
                              unscheduled);
  }
  clear_sweeper (sweeper);

  if (!solver->inconsistent && !kissat_propagated (solver))
    (void) kissat_dense_propagate (solver);

  if (success && limit_reached)
    return "successfully despite reaching limit";
  if (!success && !limit_reached)
    return "unsuccessfully without reaching limit";
  else if (success && !limit_reached)
    return "successfully without reaching limit";
  assert (!success && limit_reached);
  return "unsuccessfully and reached limit";
}

typedef struct sweep_candidate sweep_candidate;

struct sweep_candidate {
  unsigned rank;
  unsigned idx;
};

// clang-format off

typedef STACK(sweep_candidate) sweep_candidates;

// clang-format on

#define RANK_SWEEP_CANDIDATE(CAND) (CAND).rank

static bool scheduable_variable (sweeper *sweeper, unsigned idx,
                                 size_t *occ_ptr) {
  kissat *solver = sweeper->solver;
  const unsigned lit = LIT (idx);
  const size_t pos = SIZE_WATCHES (WATCHES (lit));
  if (!pos)
    return false;
  const unsigned max_occurrences = sweeper->limit.clauses;
  if (pos > max_occurrences)
    return false;
  const unsigned not_lit = NOT (lit);
  const size_t neg = SIZE_WATCHES (WATCHES (not_lit));
  if (!neg)
    return false;
  if (neg > max_occurrences)
    return false;
  *occ_ptr = pos + neg;
  return true;
}

static unsigned schedule_fresh (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  flags *flags = solver->flags;
  sweep_candidates fresh;
  INIT_STACK (fresh);
  const unsigned variables = VARS;
  for (unsigned idx = 0; idx != variables; idx++) {
    struct flags *f = flags + idx;
    if (!f->active)
      continue;
    if (f->sweep)
      continue;
    if (scheduled_variable (sweeper, idx))
      continue;
    size_t occ;
    if (!scheduable_variable (sweeper, idx, &occ))
      continue;
    sweep_candidate cand;
    cand.rank = occ;
    cand.idx = idx;
    PUSH_STACK (fresh, cand);
  }
  unsigned size = SIZE_STACK (fresh);
  RADIX_STACK (sweep_candidate, unsigned, fresh, RANK_SWEEP_CANDIDATE);
  for (all_stack (sweep_candidate, cand, fresh))
    enqueue_variable_first (sweeper, cand.idx);
  RELEASE_STACK (fresh);
  return size;
}

static unsigned reschedule_previously_remaining (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  flags *flags = solver->flags;
  unsigned rescheduled = 0;
  for (all_stack (unsigned, idx, solver->sweep)) {
    struct flags *f = flags + idx;
    if (!f->active)
      continue;
    size_t occ;
    if (!scheduable_variable (sweeper, idx, &occ))
      continue;
    assert (!scheduled_variable (sweeper, idx));
    enqueue_variable_last (sweeper, idx);
    rescheduled++;
  }
  CLEAR_STACK (solver->sweep);
  return rescheduled;
}

static unsigned incomplete_variables (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  flags *flags = solver->flags;
  unsigned res = 0;
  for (unsigned idx = sweeper->first; idx != INVALID_IDX;
       idx = sweeper->next[idx])
    if (flags[idx].sweep)
      res++;
  return res;
}

static void mark_all_scheduled_variables_as_incomplete (sweeper *sweeper) {
  kissat *solver = sweeper->solver;
  kissat_extremely_verbose (
      solver, "marking scheduled sweeping variables as incomplete");
  flags *flags = solver->flags;
  for (unsigned idx = sweeper->first; idx != INVALID_IDX;
       idx = sweeper->next[idx])
    flags[idx].sweep = true;
}

static unsigned schedule_sweeping (sweeper *sweeper) {
  assert (sweeper->first == INVALID_IDX);
  assert (sweeper->last == INVALID_IDX);
  unsigned rescheduled = reschedule_previously_remaining (sweeper);
  unsigned fresh = schedule_fresh (sweeper);
  unsigned scheduled = fresh + rescheduled;
  unsigned incomplete = incomplete_variables (sweeper);
#ifndef QUIET
  kissat *solver = sweeper->solver;
  kissat_phase (solver, "sweep", GET (sweep),
                "scheduled %u variables %.0f%% "
                "(%u rescheduled %.0f%%, %u incomplete %.0f%%)",
                scheduled,
                kissat_percent (scheduled, sweeper->solver->active),
                rescheduled, kissat_percent (rescheduled, scheduled),
                incomplete, kissat_percent (incomplete, scheduled));
#endif
  if (!incomplete)
    mark_all_scheduled_variables_as_incomplete (sweeper);
  return scheduled;
}

static void unschedule_sweeping (sweeper *sweeper, unsigned swept,
                                 unsigned scheduled) {
  kissat *solver = sweeper->solver;
#ifdef QUIET
  (void) scheduled, (void) swept;
#endif
  assert (EMPTY_STACK (solver->sweep));
  flags *flags = solver->flags;
  for (unsigned idx = sweeper->first; idx != INVALID_IDX;
       idx = sweeper->next[idx])
    if (flags[idx].active)
      PUSH_STACK (solver->sweep, idx);
  unsigned remain = SIZE_STACK (solver->sweep);
  if (remain) {
    const unsigned incomplete = incomplete_variables (sweeper);
    if (incomplete)
      kissat_extremely_verbose (solver,
                                "need to sweep %u more variables "
                                "for completion",
                                incomplete);
    else {
      kissat_extremely_verbose (solver,
                                "no variables needed to complete sweep");
      INC (sweep_completed);
    }
  } else {
    kissat_extremely_verbose (solver, "all variables swept");
    INC (sweep_completed);
  }
  kissat_phase (solver, "sweep", GET (sweep),
                "swept %u variables (%u remain %.0f%%)", swept, remain,
                kissat_percent (remain, scheduled));
}

static bool empty_schedule (sweeper *sweeper) {
  return sweeper->last == INVALID_IDX;
}

static unsigned pop_schedule (sweeper *sweeper) {
  unsigned res = sweeper->last;
  if (res != INVALID_IDX)
    dequeue_variable (sweeper, res);
  return res;
}

void kissat_sweep (kissat *solver) {
  if (!GET_OPTION (sweep))
    return;
  if (solver->inconsistent)
    return;
  assert (!solver->level);
  assert (!solver->unflushed);
  START (sweep);
  INC (sweep);
  statistics *statistics = &solver->statistics;
  uint64_t equivalences = statistics->sweep_equivalences;
  uint64_t units = statistics->sweep_units;
  sweeper sweeper;
  init_sweeper (solver, &sweeper);
  const unsigned scheduled = schedule_sweeping (&sweeper);
  unsigned swept = 0;
  while (!empty_schedule (&sweeper)) {
    if (solver->inconsistent)
      break;
    if (TERMINATED (sweep_terminated_7))
      break;
    if (solver->statistics.kitten_ticks > sweeper.limit.ticks)
      break;
    const unsigned idx = pop_schedule (&sweeper);
    assert (idx != INVALID_IDX);
    FLAGS (idx)->sweep = false;
#ifndef QUIET
    const char *res =
#endif
        sweep_variable (&sweeper, idx);
    kissat_extremely_verbose (
        solver, "swept[%u] external variable %d %s", swept,
        kissat_export_literal (solver, LIT (idx)), res);
    swept++;
  }
  equivalences = statistics->sweep_equivalences - equivalences;
  units = solver->statistics.sweep_units - units;
  kissat_phase (solver, "sweep", GET (sweep),
                "found %" PRIu64 " equivalences and %" PRIu64 " units",
                equivalences, units);

  unschedule_sweeping (&sweeper, swept, scheduled);
  unsigned inactive = release_sweeper (&sweeper);

  if (!solver->inconsistent) {
    solver->propagate = solver->trail.begin;
    kissat_probing_propagate (solver, 0, true);
  }

#ifndef QUIET
  uint64_t eliminated = equivalences + units;
  assert (solver->active >= inactive);
  solver->active -= inactive;
  REPORT (!eliminated, '=');
  solver->active += inactive;
#else
  (void) inactive;
#endif
  STOP (sweep);
}
