# CLAUDE.md

Context for Claude Code sessions working on this repo. Read first.

## What this is

`menstrualcycleR` — the CLEAR Lab's published R package for menstrual-cycle time-series analysis.
Primary exports: `pacts_scaling()` (PACTS cycle-time rescaling), `menstrual_cycle_plot()` family,
and supporting helpers. Installed / cited as `menstrualcycleR`; that name is a public contract
(install instruction, citation keys, CRAN mirror) and must not change.

This public repository carries **released, reviewed changes only**. All development, incubation,
and lab-internal pipeline work happens in the lab's private repositories and reaches this package
exclusively through the graduation/release process — never add in-development code here directly.

## Docs site & the Bibliography link

The pkgdown site is the tracked `docs/` folder (GitHub Pages serves **main → /docs**; no CI, so
rebuild `docs/` locally and commit to republish). The navbar **Bibliography** item and a line in
`README.md` both link to <https://menstrualcycler.base44.app> — a **lab-owned base44 app**
(auto-generated annotated bibliography of papers citing `menstrualcycleR` / PACTS). To move or
retire it, edit the `biblio` component in `_pkgdown.yml` **and** the README line, then rebuild.

**Rebuild the site with `dev/build_docs.R` — do not call `pkgdown::build_site()` directly.**
pkgdown renders **every** root `*.md` (incl. `CLAUDE.md` and the private `CLAUDE.local.md`) onto
the public site, so a raw build leaks them into `docs/`. `dev/build_docs.R` hides all `CLAUDE*.md`
for the build, rebuilds, then **errors if any `docs/` file still references "claude"** — so the
machine, not you, catches a leak:

```
Rscript --vanilla dev/build_docs.R            # hide → build_site → leak-check (from the repo root)
Rscript --vanilla dev/build_docs.R --selftest # prove the leak scanner works, no build
```

Backstop: `dev/git-hooks/pre-commit` blocks committing any `docs/` file that references "claude"
even if the site was rebuilt in RStudio/GitHub Desktop. It's tracked but not auto-installed —
run `sh dev/install-hooks.sh` **once per clone** (it's machine-local, so re-run it after re-cloning
or on another lab machine).

## API contract

The package API is a **published contract**: additive-only changes, version-bumped on every
user-facing change. Never rename, remove, or silently change the behavior of an exported function
without a major version bump and deprecation cycle. New functionality arrives only by graduation
from the lab's private development repositories once its interface has stabilized and it carries
tests and documentation.

## Key analysis variable

Analyze `cyclic_time` / `cyclic_time_impute`. `scaled_cycleday*` is **deprecated** (legacy reference only).
