# CLAUDE.md

Context for Claude Code sessions working on this repo. Read first.

## What this is

`menstrualcycleR` — the CLEAR Lab's published R package for menstrual-cycle time-series analysis.
Primary exports: `pacts_scaling()` (PACTS cycle-time rescaling), `menstrual_cycle_plot()` family,
and supporting helpers. Installed / cited as `menstrualcycleR`; that name is a public contract
(install instruction, citation keys, CRAN mirror) and must not change.

This is **stage 03 of the menstrualcycleR pipeline** (PACTS scaling / the package itself).
Upstream: `mcycr-02-anchor-prep` (final anchor selection). Downstream: `mcycr-04-horm-est-build`,
`mcycr-05-*` (pacts-gam-pipeline).

## Docs site & the Bibliography link

The pkgdown site is the tracked `docs/` folder (GitHub Pages serves **main → /docs**; no CI, so
rebuild `docs/` locally and commit to republish). The navbar **Bibliography** item and a line in
`README.md` both link to <https://menstrualcycler.base44.app> — a **lab-owned base44 app**
(auto-generated annotated bibliography of papers citing `menstrualcycleR` / PACTS). To move or
retire it, edit the `biblio` component in `_pkgdown.yml` **and** the README line, then rebuild.
Always **rebuild with every `CLAUDE*.md` hidden** — pkgdown renders any root `*.md` onto the
public site.

## API contract

The package API is a **published contract**: additive-only changes, version-bumped on every
user-facing change. Never rename, remove, or silently change the behavior of an exported function
without a major version bump and deprecation cycle. Shared pipeline helpers mature **upstream**
in `pacts-gam-pipeline`, then graduate here when stable — do not short-circuit by adding
incubator-stage code directly to this package.

## Ovulation coding

EDO = day before the **sustained** P4/PdG rise above follicular baseline.
LH cross-check: positive test ≥40 mIU/L + 1 = ovulation (LH+1 convention; surge onset, not peak).
Saliva studies (ADHD/UKAlc): immunoassay E2 invalid — code from LH + P4 only.
r(E2, P4) > 0.8 cross-reactivity exclusion applies lab-wide.
Full protocol: `pacts-gam-pipeline/ov_coding/OV_CODING_PROTOCOL.md`
Notion SOP: https://app.notion.com/p/38c62cf9b39381cd8b77fd19adece7b2

## Pipeline ecosystem (mcycr- naming scheme)

Same name across GitHub / Box / Notion / Slack.

| Stage | Repo | Notion SOP |
|---|---|---|
| 01a · Ov from hormones | mcycr-01-ov-est-horm | https://app.notion.com/p/38c62cf9b39381cd8b77fd19adece7b2 |
| 01b · Ov from LH testing | mcycr-01-ov-est-lh | https://app.notion.com/p/38c62cf9b39381f187b0f690517494ce |
| 02 · Anchor prep | mcycr-02-anchor-prep | https://app.notion.com/p/38c62cf9b39381178f50e79e391f75fd |
| **03 · PACTS scaling (this repo)** | **menstrualcycleR** | https://app.notion.com/p/38c62cf9b39381429226e00b07024476 |
| 04a · Horm est build | mcycr-04-horm-est-build | https://app.notion.com/p/38c62cf9b39381069323de7b2f5df7fb |
| 04b · Horm est validate | mcycr-04-horm-est-validate | https://app.notion.com/p/38c62cf9b39381069323de7b2f5df7fb |
| 05 · Modeling | pacts-gam-pipeline | https://app.notion.com/p/38c62cf9b39381efaf1cdcddabf084c3 |
| 06 · Merge | mcycr-06-merge | https://app.notion.com/p/38c62cf9b393813b97e4f29834813d3a |

Full pipeline map: https://app.notion.com/p/38c62cf9b39381a29053c653ea82e4f1
Decisions + rationale: `pacts-gam-pipeline/PIPELINE_MAP.md`

## Key analysis variable

Analyze `cyclic_time` / `cyclic_time_impute`. `scaled_cycleday*` is **deprecated** (legacy reference only).
