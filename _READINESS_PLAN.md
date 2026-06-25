# menstrualcycleR 0.1.3 — Bug-fix Release Readiness Plan

**Status: READY for maintainer review. All changes are LOCAL and uncommitted-to-remote.**
Branch: `fix/daterated-na-on-filled-rows`. Prepared 2026-06-25.

This document is written to convince a skeptical maintainer that the fix is correct,
minimal, and safe to ship. Every claim below is backed by a command you can re-run.

Run R in this repo with:
```
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript -e '...'
```
(the project `renv` lib is empty; the system R lib carries the deps).

---

## (a) The bug, root cause, and the one-line idea of the fix

### Symptom
`pacts_scaling()` returns the user's **original date column** (e.g. `daterated`)
with `NA` on some rows — specifically on rows that the function fabricated to fill
calendar gaps. When a cycle's ovulation is **imputed** (no biomarker-confirmed
ovulation), the imputed-ovulation day and its `cyclic_time*` / `scaled_cycleday*`
values can land on exactly such a fabricated row. A downstream join back to other
data **by the original date column name** then silently drops those rows.

### Root cause
`pacts_scaling()` first copies the user's columns into internal canonical names:

```r
data <- data %>% dplyr::mutate(id = !!id, date = !!date, menses = !!menses, ovtoday = !!ovtoday)
```

so after this the frame holds BOTH the original column (`daterated`) and a canonical
`date`. Then `calculate_mcyclength()` (R/calculate_mcyclength.r, L72–82) densifies
each participant's calendar:

```r
tidyr::complete(!!date := seq.Date(min(!!date), max(!!date), by = "day"))
```

`tidyr::complete()` fabricates one row per missing calendar day, populating the
canonical `date` but leaving the original `daterated` as `NA`. The canonical `date`
is authoritative (fully populated; identical to the original where the original is
non-`NA`). The original column was never refilled — that is the bug.

### The fix (R/pacts_scaling.R, new block after `calculate_cycletime()`)
Refill the original date column from the canonical `date`, **NA-fill only**:

```r
data <- data %>% dplyr::mutate(!!date := dplyr::coalesce(!!date, date))
```

with one guard and one type accommodation:

- **Guard:** skip when the user passed `date = date` (the original *is* the canonical
  column — nothing to refill, and we must not duplicate it). Implemented as
  `if (date_name != "date" && date_name %in% names(data))`.
- **Type accommodation:** the canonical `date` is always `Date` class
  (calculate_mcyclength coerces via `lubridate::ymd`). `dplyr::coalesce()` refuses
  to combine `<character>`/`<factor>` with `<date>`. character/factor date columns
  are a **supported input** (the pipeline already coerces them the same way), so for
  those we coalesce on `lubridate::ymd(!!date)`. `Date` and `POSIXct` originals
  coalesce directly and **keep their own type**.

### Why it's minimal and safe
- **One conceptual operation:** "the original date column should never be NA where the
  canonical date exists." `coalesce(orig, date)` writes only into NA slots.
- **No-overwrite guarantee:** `coalesce` takes the first non-NA. On observed days the
  original is already present, so it is returned unchanged. The character/factor cast
  is value-preserving on observed days (it reproduces the same coercion the package
  applies internally), so it cannot change an observed value — it only supplies the
  calendar date on fabricated rows. Verified I2 = 0 (see (b)).
- **No change to any computed cycle value.** The block runs *after*
  `calculate_cycletime()` and touches only the original date column.
- **Fully namespace-qualified** (`dplyr::`, `rlang::`, `lubridate::`), consistent with
  the 0.1.1 "works without tidyverse attached" work — adds no new R CMD check note.

---

## (b) Adversarial angles tested → result

Six adversarial agents probed the fix. Summary of their coverage and outcome:

| Angle | What it stressed | Tests | Result |
|---|---|---|---|
| dateformats | date column TYPE + NAME permutations (Date / character / POSIXct / factor; arg named `date` vs `daterated` vs other; base df / tibble / grouped / multi-subject) | 11 | **Found the character/factor crash → FIXED** |
| degenerate-shapes | single participant, single complete cycle, no menses, all-observed ovulation, 1/2-row inputs, NA dates, mixed many-participant impute, non-Date types | 15 | **Found the character/factor crash → FIXED** |
| noclobber | `coalesce(!!date, date)` must fill NA only, never overwrite an observed date; no-op / value-preservation invariants | 7 | **Confirmed NA-fill-only; surfaced type crash → FIXED** |
| compat | backward compatibility of the returned frame | 14 | **Surfaced type crash → FIXED** |
| gapregression | large calendar gaps + first-cycle imputed ovulation on fabricated gap days; multi-subject densify; value-column join-safety | 9 | **Surfaced type crash → FIXED** |
| scalecalendar | SCALE (large N, timing) + CALENDAR (leap year, year boundary, out-of-order, duplicates, very long/short cycles, far-apart dates, POSIXct/DST) | 16 | **0 failures — clean** |

**The single real regression** (reported independently by 5 of 6 agents): a naive
`coalesce(!!date, date)` crashed on **character- and factor-typed** original date
columns with *"Can't combine `<character>`/`<factor>` and `<date>`"*, because the
canonical `date` is `Date`-class while the original kept its input type. character/
factor are explicitly supported inputs. **Fixed** by coercing character/factor
originals to `Date` (via `lubridate::ymd`, the package's own internal coercion)
before coalescing; `Date`/`POSIXct` take the unchanged path and keep their type.

### Direct verification of all four type paths (re-runnable)
```
Date         class=Date      NA-orig-where-date=0   overwrite=0
character    class=Date      NA-orig-where-date=0   overwrite=0
factor       class=Date      NA-orig-where-date=0   overwrite=0
POSIXct      class=POSIXct   NA-orig-where-date=0   overwrite=0
```
- **NA-orig-where-date = 0** → invariant holds: no row carries a real canonical
  `date` while the original date is `NA` (the bug is gone).
- **overwrite = 0** → no observed original date was changed (no-clobber holds).

### Pre-fix vs post-fix evidence (via `git stash`)
With the fix stashed, the same character input runs but returns **3** rows where a
real `date` coexists with an `NA` original date (the bug). With the fix applied,
that count is **0**. (The intermediate naive-coalesce candidate threw on character;
the shipped fix's type-coercion resolves both the crash and the original NA bug.)

### Regression test
`tests/testthat/test_daterated_na_on_filled_rows.R` — now **7 `test_that` blocks**:
1. original date column never NA where a real `date` exists;
2. scaled / cycle values never sit on an NA original-date row (incl. imputed-ov rows);
3. passing a column literally named `date` does not error or duplicate;
4. **character** original → no crash, fully filled, coerced to Date, values agree;
5. **factor** original → no crash, fully filled, coerced to Date, values agree;
6. **POSIXct** original → filled, retains POSIXct type;
7. **no-overwrite / NA-fill-only** invariant on a gap-free frame (strict no-op).

Cases 4–7 were added during this release prep — the original test only covered
`Date` input, which is precisely the type that did *not* trigger the regression.

---

## (c) Full-suite + R CMD check results

### devtools::test() — full suite
```
FAIL 0 | WARN 0 | SKIP 1 | PASS (all)
```
The 1 SKIP is a pre-existing empty `process_follicular_phase` test, unrelated.
The expanded regression file passes all blocks.

### R CMD check (devtools::check())
**This environment has no Pandoc**, so a full check fails at *vignette building*
(`Pandoc is required to build R Markdown vignettes but not available`) — an
environmental limitation, not a code defect; it aborts before any package code is
checked. Re-running with `--no-build-vignettes --ignore-vignettes`:

```
Status: 4 WARNINGs, 3 NOTEs, 0 ERRORs
  * checking examples ... OK
  * checking tests ... Running 'testthat.R' ... OK
```

**Every WARNING/NOTE is pre-existing and unrelated to this fix:**

| Item | Pre-existing? | Note |
|---|---|---|
| W: non-portable file names | yes | `docs/` pkgdown PNGs + rsconnect `.dcf`; build artifacts. Not introduced here. |
| W: dependencies in R code (`purrr` undeclared; `cpass`/`mgcv`/`shinyjs` unused) | yes | DESCRIPTION hygiene, untouched by fix. |
| W: code/doc mismatch in `cycledata.Rd` (`date` vs `daterated`) | yes | Data-doc drift; untouched. |
| W: Rd \usage `cycle_plot` (`align_val` vs `alignval`) | yes | Untouched. |
| N: future file timestamps ("unable to verify current time") | yes | Environmental. |
| N: top-level `docs`/`rsconnect` non-standard | yes | Add to `.Rbuildignore` someday; untouched. |
| N: no-visible-binding globals | yes | Pre-existing across the helper files; fix adds none (it is fully namespace-qualified). |

The fix introduces **zero new** WARNINGs or NOTEs. The helper files `_REPRO_*.R`,
`_DRAFT_PR.md`, and `_READINESS_PLAN.md` were added to `.Rbuildignore` so they do not
appear in `R CMD build`/`check`.

> Recommended pre-CRAN follow-up (out of scope for this patch): run a clean
> `R CMD check --as-cran` on a machine **with Pandoc** to exercise the vignette, and
> optionally clear the pre-existing WARNINGs. None of these block this internal fix
> release for lab/downstream consumers.

---

## (d) NEWS entry + version bump

- **DESCRIPTION:** `Version: 0.1.2 -> 0.1.3` (patch — bug fix, no API change).
- **NEWS.md:** new `# menstrualcycleR 0.1.3` section at top, in user-facing terms:
  pacts_scaling() now returns a complete original date column; previously fabricated
  calendar rows left it NA, which could silently drop imputed-ovulation / cyclic_time
  rows in a downstream join. Character/factor date columns are coerced to Date as the
  pipeline already does; Date/POSIXct keep their type.

### ⚠ Installed-vs-source version MISMATCH (flag for the maintainer)
The **installed** package in the system R library reports `0.0.0.9000` (a stale dev
build), while the **source** was `0.1.2` (now `0.1.3`). Any consumer that does
`library(menstrualcycleR)` is currently running stale code that lacks BOTH the 0.1.2
last-participant fix AND this 0.1.3 fix. **The maintainer must reinstall from source**
so consumers (notably `pacts-gam-pipeline`) pick up the fix:
```
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript -e 'devtools::install("/Users/toryeisenlohr-moul/CLEAR Lab Repositories/menstrualcycleR", upgrade = "never")'
RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript -e 'cat(as.character(packageVersion("menstrualcycleR")))'   # expect 0.1.3
```

---

## (e) RELEASE STEPS the maintainer runs (DO NOT run here)

These are intentionally **not executed** — local-only review state. To ship:

1. **Review the diff** on `fix/daterated-na-on-filled-rows`:
   ```
   git diff main..fix/daterated-na-on-filled-rows -- R/ tests/ NEWS.md DESCRIPTION .Rbuildignore
   ```
2. **Reinstall from source** and confirm version (see (d)) — so downstream picks up the fix.
3. **Re-run tests + check on a machine with Pandoc** (full vignette build):
   ```
   Rscript -e 'devtools::test()'
   Rscript -e 'devtools::check()'      # expect the same pre-existing WARN/NOTE set, 0 ERROR
   ```
4. **Push the branch:**
   ```
   git push -u origin fix/daterated-na-on-filled-rows
   ```
5. **Open the PR** (body is staged in `_DRAFT_PR.md`):
   ```
   gh pr create --base main --head fix/daterated-na-on-filled-rows \
     --title "Fix: pacts_scaling() leaves original date column NA on fabricated calendar rows (0.1.3)" \
     --body-file _DRAFT_PR.md
   ```
6. **Merge** after review, then on `main`:
   ```
   git tag v0.1.3 && git push origin v0.1.3
   ```
7. **Rebuild the pkgdown site** for 0.1.3 (matches the repo's per-release convention —
   see prior "Rebuild pkgdown site for 0.1.x" commits):
   ```
   Rscript -e 'pkgdown::build_site()'
   ```
8. (Optional) decide whether `_REPRO_daterated_NA.R` / `_DRAFT_PR.md` /
   `_READINESS_PLAN.md` stay (Rbuildignored) or are removed before tagging — see (f).

---

## (f) Open questions / residual risks

1. **Type change on character/factor inputs.** character/factor original date columns
   now come back as **Date** (coerced), not their original string/factor type. This is
   deliberate and defensible — it matches what the pipeline already does to the internal
   `date`, and is required for `coalesce` to combine the columns. But a downstream
   consumer that expected a character/factor date column back would see a type change.
   `Date` and `POSIXct` inputs are unaffected. *Decision logged: acceptable; a date
   column should be a date.*
2. **Root-level helper files** (`_REPRO_daterated_NA.R`, `_DRAFT_PR.md`,
   `_READINESS_PLAN.md`): handled by **adding to `.Rbuildignore`** so the build stays
   clean while keeping them available for review. *Recommendation:* keep `_REPRO_*` and
   `_READINESS_PLAN.md` for the audit trail; you may delete `_DRAFT_PR.md` once the real
   PR is opened. They are Rbuildignored either way, so this does not affect the package.
3. **Pre-existing pipeline behaviors left unchanged** (not introduced by the fix):
   POSIXct type-mixing and integer→double Date storage promotion noted by agents are
   inherent to the existing densify/coerce path; out of scope for this patch.
4. **Pandoc-less environment** could not exercise the vignette in `R CMD check` — step
   (e)3 closes this on a Pandoc-equipped machine. Package code, examples, and tests all
   pass here.
5. **Pre-existing R CMD check WARN/NOTE backlog** (4 W / 3 N) is untouched. Worth a
   separate cleanup pass before any CRAN submission; does not block this internal fix.

---

## (g) Downstream impact — pacts-gam-pipeline

The `pacts-gam-pipeline` consumer joins PACTS output back by the original date column.
Pre-fix, imputed-ovulation / `cyclic_time` rows that landed on fabricated calendar days
carried an `NA` original date and were silently dropped on that join. **This fix
eliminates the source of the drop** at the package level.

That pipeline already has a **defensive consumer-side guard**, so it is **not blocked**
on this release — but it is currently running against the **stale `0.0.0.9000` install**,
which has neither this fix nor the guard's upstream complement. Action: after the
maintainer reinstalls from source (step (d)/(e)2), the pipeline gets correct, complete
rows directly from `pacts_scaling()` and no longer relies solely on its guard. No
pipeline code change is required to adopt the fix.

---

## Appendix — what is staged locally (uncommitted to remote)

- `R/pacts_scaling.R` — the fix (refill original date column; type-aware coalesce).
- `tests/testthat/test_daterated_na_on_filled_rows.R` — regression test, expanded to
  7 blocks (added character/factor/POSIXct + no-overwrite cases).
- `DESCRIPTION` — Version 0.1.2 → 0.1.3.
- `NEWS.md` — 0.1.3 entry.
- `.Rbuildignore` — ignores `_REPRO_daterated_NA.R`, `_DRAFT_PR.md`, `_READINESS_PLAN.md`.
- `_REPRO_daterated_NA.R`, `_DRAFT_PR.md`, `_READINESS_PLAN.md` — review aids (Rbuildignored).
