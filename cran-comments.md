# CRAN submission comments

## Submission

This is a new release. `menstrualcycleR` implements Phase-Aligned Cycle Time
Scaling (PACTS), a method for placing menstrual cycle observations onto a
continuous, phase-aligned timeline. The method is published in
Nagpal et al. (2025), *Psychoneuroendocrinology*,
<https://doi.org/10.1016/j.psyneuen.2025.107584>, and the DOI is cited in the
DESCRIPTION Description field.

## R CMD check results

0 errors | 0 warnings | 1 note

Checked with `R CMD check --as-cran` on macOS 26.6 (aarch64), R 4.5.1, with pandoc
3.11 installed. Also checked on R 4.6.1 (Ubuntu 24.04) via GitHub Actions.

### Note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Tory Eisenlohr-Moul <temo@uchicago.edu>'

New submission
```

This is expected for a first submission.

## Notes for the reviewer

* **`launch_app()` and its optional dependencies.** The package ships a Shiny
  app under `inst/shiny`. `launch_app()` requires `shinyjs` and `writexl` (both
  suggested dependencies, both on CRAN) and `cpass` (not on CRAN; installed from
  GitHub). None is needed by `pacts_scaling()` or any other exported function.
  `launch_app()` checks for all three with `requireNamespace()` and returns an
  informative error naming any that are missing, rather than failing part-way
  through. `cpass` is deliberately **not** declared in `Suggests`, and no
  `Remotes` field is present.

* **Examples and vignettes.** All examples and both vignettes run against
  `cycledata`, a small example dataset bundled with the package. Nothing
  downloads data, writes outside `tempdir()`, or requires network access.

* **Spelling.** "PACTS", "menses", "ovulation", "periovulatory", "follicular",
  "luteal" and "cyclicity" are domain terms, spelled as intended.

* **AI coding tools.** Claude Code was used for parts of this release, including the
  packaging changes and the new "Preparing Your Data for PACTS" vignette. No AI tool was
  used in the package's initial development. The decision to use these tools was made by
  the maintainer, Dr. Tory Eisenlohr-Moul, who reviewed and approved every AI-assisted
  change; the co-author's contributions predate all such use. This is disclosed in the
  README and NEWS and marked in the git history. No AI system is listed in `Authors@R`.
