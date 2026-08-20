#' menstrualcycleR: Phase-Aligned Cycle Time Scaling (PACTS)
#'
#' Tools for standardizing menstrual cycle data onto a continuous, phase-aligned
#' timeline (PACTS), so cycle-time predictors can be modeled the same way across
#' participants and cycles despite variability in cycle length and ovulation timing.
#'
#' @section Where to start:
#' \code{\link{pacts_scaling}} is the main entry point -- it takes a long-format
#' diary (one row per person-day) with menses and ovulation markers and returns
#' the same data with `cyclic_time`, `cyclic_time_impute`, `cyclic_time_ov`, and
#' `cyclic_time_imp_ov` added (see `?pacts_scaling`'s `@return` for what each
#' means and which to use). For a full worked example, including GAMM modeling
#' of the resulting cycle-time variables, see the "Getting Started" vignette:
#' `vignette("menstrualcycleR-overview", package = "menstrualcycleR")`, or the
#' hosted copy at
#' \url{https://eisenlohrmoullab.github.io/menstrualcycleR/articles/menstrualcycleR-overview.html}
#' -- installing via `remotes::install_github()` does not build vignettes
#' locally unless you pass `build_vignettes = TRUE`.
#'
#' For a visual explainer of *why* PACTS realigns cycles the way it does, see
#' \url{https://eisenlohrmoullab.github.io/menstrualcycleR/pacts-explainer.html}.
#'
#' @section Other exported functions:
#' \itemize{
#'   \item \code{\link{cycledata_check}}: summarize per-symptom, per-phase data availability before scaling.
#'   \item \code{\link{summary_ovulation}}: summarize confirmed-vs-imputed ovulation rates after scaling.
#'   \item \code{\link{cycle_plot}} / \code{\link{cycle_plot_individual}}: visualize a symptom across phase-aligned cycle time, across the whole sample or for one individual.
#'   \item \code{\link{launch_app}}: an interactive Shiny app covering the same workflow (requires the \pkg{shinyjs} and \pkg{cpass} packages, installed separately -- see `?launch_app`).
#' }
#'
#' @references
#' Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper, A.,
#' Knol, L., Failenschmid, J., Kiesner, J., Peters, J. R., & Eisenlohr-Moul, T. A.
#' (2025). Studying the Menstrual Cycle as a Continuous Variable: Implementing
#' Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package.
#' *Psychoneuroendocrinology*, 107584. \doi{10.1016/j.psyneuen.2025.107584}
#'
#' @keywords internal
"_PACKAGE"
