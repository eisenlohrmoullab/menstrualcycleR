#' Phase-Aligned Cycle Time Scaling (PACTS)
#'
#' This function uses the Phase-Aligned Cycle Time Scaling (PACTS) method, as described in Nagpal et al. (2025), to compute continuous, biologically meaningful cycle time variables in a long-format dataset, where each row represents a unique observation date for a specific naturally-cycling individual.
#'
#' The PACTS method aligns observations across cycles by centering time either on menses onset or ovulation. This allows researchers to model menstrual cycle dynamics as continuous functions of time, improving sensitivity and interpretability. The function requires identification of menses onset (`menses`) and the estimated day of ovulation (`ovtoday`), which may be determined via biomarker (preferred) or imputed based on typical luteal phase length when unavailable.
#'
#' When ovulation is not directly assessed, the function imputes ovulation as 15 days prior to the next menses onset (i.e., the last day of the follicular phase), based on the population-average luteal phase length. Imputed ovulation days are recorded in a new binary column, `ovtoday_impute`.
#'
#' Reporting how often ovulation was confirmed using biomarkers versus imputed is important for transparency and scientific rigor. Whenever possible, researchers should use objective biomarkers such as LH tests or basal body temperature (BBT) to identify ovulation, as these methods provide greater precision. This function supports both confirmed and imputed ovulation, allowing analyses to flexibly account for variable data availability across participants and cycles.
#'
#' For further guidance on ovulation identification and justification of the -15 day imputation approach, see:
#' - Nagpal et al. (2025). *Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package*. https://doi.org/10.31219/osf.io/hd5xw_v1  
#' - Schmalenberger et al. (2021). *How to study the menstrual cycle: Practical tools and recommendations*. *Psychoneuroendocrinology, 123*, 104895. https://doi.org/10.1016/j.psyneuen.2020.104895
#'
#' @param data A data frame containing the required input variables.
#' @param id A unique identifier for each naturally-cycling individual in the dataset.
#' @param date A date column corresponding to the day of observation. Ensure all id-date combinations are unique. You may wish to reclassify post-midnight survey entries to the previous day to maintain alignment with sleep or daily tracking data.
#' @param menses A binary column (`0`/`1`) indicating the *first* day of menses onset, where `1` marks onset. All subsequent bleeding days should be set to `0`. Periovulatory spotting should also be excluded.
#' @param ovtoday A binary column (`0`/`1`) indicating the day of estimated ovulation. Required even if biomarkers were not collected—use a column of all `0`s or `NA`s in that case. Accepted ovulation determination methods include:
#' \itemize{
#'   \item \strong{Urinary LH surge tests}: Code `ovtoday == 1` on the day *after* the first positive test (LH +1). Specify the brand and threshold (e.g., 40 mIU/ml).
#'   \item \strong{Basal body temperature (BBT)}: Code `ovtoday == 1` on the day after the BBT nadir (BBT +1). Specify measurement method.
#'   \item \strong{Daily hormone assays}: See Nagpal et al. (2025) for guidance.
#' }
#' @param lower_cyclength_bound Numeric lower bound of cycle lengths to include in scaling. Default is 21.
#' @param upper_cyclength_bound Numeric upper bound of cycle lengths to include in scaling. Default is 35.
#' @param impute_next_menses Logical; default `FALSE`. When `TRUE`, opt in to imputing a *next-menses onset* from a biomarker-confirmed ovulation that has no recorded closing menses, so that cycle becomes scalable instead of being dropped for a missing anchor. This is the mirror image of the built-in ovulation imputation (which imputes ovulation *backward* from an observed menses at menses minus 15): here a menses onset is imputed *forward* from a confirmed ovulation, at `ovulation + next_menses_luteal_days`. Leaving `FALSE` keeps all previous behavior byte-for-byte identical. Only the general rule is applied; any study-specific gating (for example, blocking imputation across treatment phases or documented off-study breaks, or trust-ordered de-duplication of anchors) is the caller's responsibility to apply on top.
#' @param next_menses_luteal_days Numeric; days after a confirmed ovulation at which to place the imputed next-menses onset when `impute_next_menses = TRUE`. Default `14` (the population-average luteal length; ovulation + 14 is the last follicular day, i.e. the "LH+15" convention).
#' @param next_menses_max_window Not currently used -- kept for argument-signature stability, has no effect on the result. See `?impute_next_menses_onsets`, the `max_window` entry, for why: through 0.1.6 this bounded the search for a closing menses, which was a real bug (a genuine luteal phase just past the window got a fabricated onset that overwrote actually-observed data). As of 0.1.7 a real closing menses at any distance always prevents imputation.
#' @param luteal_phase_min_days,luteal_phase_max_days Numeric bounds (days) on how long a **confirmed** ovulation's luteal phase (ovulation to next menses) may be for `cyclic_lut`/`cyclic_time`/`luteal_length` to scale it. Defaults `7` and `18`, from Bull et al. (2019) norms in 21-35 day cycles. Independent of `lower_cyclength_bound`/`upper_cyclength_bound`, which bound the *whole cycle*, not this one phase -- widen these directly if your study population has longer or shorter luteal phases than the default norms assume. See the "Internal phase-length caps" section below.
#' @param follicular_phase_min_days,follicular_phase_max_days Numeric bounds (days) on how long a **confirmed** ovulation's follicular phase (menses to ovulation) may be for `cyclic_fol`/`cyclic_time` to scale it. Defaults `8` and `25`, from Bull et al. (2019) norms in 21-35 day cycles. Same independence from `lower_cyclength_bound`/`upper_cyclength_bound` as the luteal pair above. See the "Internal phase-length caps" section below.
#'
#' @return The original data frame with the following additional columns:
#' \itemize{
#'   \item \code{scaled_cycleday}: A continuous cycle time variable centered on menses onset (`menses == 1` → 0), ranging from -1 (start of luteal phase) to +1 (ovulation). Only includes cycles with biomarker-confirmed ovulation.
#'   \item \code{scaled_cycleday_impute}: Same as above, but includes cycles where ovulation was imputed using day -15. Offers broader coverage across the dataset, at the cost of lower precision.
#'   \item \code{scaled_cycleday_ov}: A cycle time variable centered on ovulation day (`ovtoday == 1` → 0), ranging from -1 (start of follicular phase) to +1 (end of luteal phase). Only includes cycles with confirmed ovulation.
#'   \item \code{scaled_cycleday_imp_ov}: Same as above, but uses imputed ovulation (`ovtoday_impute == 1`) for cycles lacking biomarker confirmation. Centered on either confirmed or imputed ovulation.
#'   \item \code{ovtoday_impute}: A binary column indicating imputed ovulation days (value `1`) for cycles without confirmed ovulation, estimated as 15 days before menses onset.
#'   \item \code{menses_impute}: Present only when `impute_next_menses = TRUE`. A binary column marking menses onsets that were *imputed forward* from a confirmed ovulation (value `1`) versus observed onsets (`0`). Report the imputed-versus-observed onset rate for transparency, just as you would for imputed ovulation.
#'   \item \code{cyclic_time_impute_extended_phase}: Binary column (`0`/`1`) marking rows where \code{cyclic_time_impute} was filled by the phase-cap fallback described below (value `1`), versus the normal confirmed or imputed-ovulation paths (`0`). Report this rate alongside the imputed-ovulation rate for full transparency about coverage.
#'   \item \code{cyclic_time_imp_ov_extended_phase}: The same flag as \code{cyclic_time_impute_extended_phase}, for the ovulation-centered \code{cyclic_time_imp_ov} column.
#' }
#'
#' @section Internal phase-length caps, and how they differ from the cycle-length bounds: a confirmed ovulation's luteal phase (ovulation to next menses) is only scaled by \code{cyclic_lut}/\code{cyclic_time} when it falls within \code{[luteal_phase_min_days, luteal_phase_max_days]} (default 7-18 days), and its follicular phase (menses to ovulation) only within \code{[follicular_phase_min_days, follicular_phase_max_days]} (default 8-25 days) -- both from Bull et al. (2019) norms in 21-35 day cycles. \strong{As of this version these four bounds are caller-adjustable, independently of \code{lower_cyclength_bound}/\code{upper_cyclength_bound}}, which bound the whole cycle, not either phase individually. In every version through 0.1.6 they were hardcoded (not arguments at all), so a cycle could fall entirely inside the caller's accepted overall length (e.g. the whole \code{[20,43]}-day CLEAR lab standard) and still have one phase silently uncovered by \code{cyclic_time}, because that phase alone exceeded the fixed 18 or 25 day limit with no way to widen it. A caller whose study population has genuinely longer or shorter phases than the Bull et al. norms should now widen these four arguments directly, rather than relying on the fallback described next. \code{cyclic_time_impute} additionally falls back, for exactly the days a capped phase would otherwise leave with no coverage at all (a confirmed ovulation already suppresses the ordinary imputed-ovulation fallback -- see \code{ovtoday_impute}; and a still-open trailing cycle, \code{cycle_incomplete == 1}, never gets this fallback either, matching every other imputed column), to the same phase fraction computed \emph{without} the phase-length ceiling (the floor still applies), gated only on the overall cycle falling within the caller's own \code{[lower_cyclength_bound, upper_cyclength_bound]}. That fallback exists for whatever still runs past even a caller-widened phase bound, trading phase-length plausibility for coverage in the imputed column only -- it never touches \code{cyclic_time} itself. Those filled rows (excluding the ovulation-anchor day itself, whose value is always pinned by a separate, pre-existing override) are flagged in \code{cyclic_time_impute_extended_phase}. \code{cyclic_time_imp_ov}, the ovulation-centered sibling of \code{cyclic_time_impute}, receives the identical fallback, flagged in \code{cyclic_time_imp_ov_extended_phase}.
#'
#' \strong{History.} These caps existed as hardcoded literals in every released version of this package (v0.1.0 onward; git history traces the specific thresholds back further, to before \code{cyclic_time} existed at all) until they became adjustable arguments in this version. The published Nagpal et al. (2025) paper and its supplement do not mention either cap. The package's own "Getting Started" vignette discloses the luteal cap only. Both caps, and the fixed-vs-adjustable distinction (as it stood before this version), were stated at \url{https://eisenlohrmoullab.github.io/menstrualcycleR/pacts-explainer.html} ("Inclusion defaults") -- an automated documentation pass describing the code's actual behavior at the time, not a deliberate methods decision recorded anywhere else. See \code{NEWS.md} for the full history.
#' @keywords menstrual cycle ovulation cycle phase scaling time-varying covariate
#' @importFrom rlang :=
#' @export
#'
#' @examples 
#' # Load the example dataset
#' cycle_df = cycledata
#'
#' data_with_scaling <- pacts_scaling(
#'   cycle_df, 
#'   id = id, 
#'   date = daterated, 
#'   menses = menses, 
#'   ovtoday = ovtoday, 
#'   lower_cyclength_bound = 21, 
#'   upper_cyclength_bound = 35
#' )
#'
#'
#' # View the result
#' print(data_with_scaling)
#' 

pacts_scaling <- function(data, id, date, menses, ovtoday, lower_cyclength_bound = 21, upper_cyclength_bound = 35,
                          impute_next_menses = FALSE, next_menses_luteal_days = 14, next_menses_max_window = 20,
                          luteal_phase_min_days = 7, luteal_phase_max_days = 18,
                          follicular_phase_min_days = 8, follicular_phase_max_days = 25) {
  `%>%` <- magrittr::`%>%`
  id <- rlang::enquo(id)
  date <- rlang::enquo(date)
  menses <- rlang::enquo(menses)
  ovtoday <- rlang::enquo(ovtoday)

  # OPT-IN next-menses imputation (default FALSE keeps published behavior identical).
  # Synthesizes a menses onset at ovulation + next_menses_luteal_days for a confirmed
  # ovulation with NO observed closing menses recorded anywhere afterward, so the
  # cycle becomes scalable. A real closing menses at any distance always wins --
  # next_menses_max_window is accepted but not used (see ?impute_next_menses_onsets).
  # General rule only; study-specific gating (treatment phases, breaks, trust order)
  # is the caller's responsibility.
  if (isTRUE(impute_next_menses)) {
    data <- impute_next_menses_onsets(data, !!id, !!date, !!menses, !!ovtoday,
                                      luteal_days = next_menses_luteal_days,
                                      max_window  = next_menses_max_window)
  }

  data = data %>%
   dplyr:: mutate(
      id = !!id,
      date = !!date,
      menses = !!menses,
      ovtoday = !!ovtoday
    )
  
  data = calculate_mcyclength(data, id, date, menses, ovtoday)
  data = calculate_cycletime(data, id, date, menses, ovtoday, lower_cyclength_bound, upper_cyclength_bound,
                              luteal_phase_min_days = luteal_phase_min_days,
                              luteal_phase_max_days = luteal_phase_max_days,
                              follicular_phase_min_days = follicular_phase_min_days,
                              follicular_phase_max_days = follicular_phase_max_days)

  # --- Keep the user's original date column consistent with the internal `date`.
  # calculate_mcyclength() densifies the calendar with tidyr::complete(date = ...),
  # which fabricates a row for every missing calendar day. On those fabricated
  # rows the canonical `date` column is filled, but the user's ORIGINAL date
  # column (e.g. `daterated`) is left NA -- so a consumer joining results back by
  # the original date name silently drops imputed-ovulation / scaled rows that
  # landed on a filled day. `date` is authoritative (always fully populated and
  # identical to the original where the original is non-NA), so refill the
  # original column from it. Skipped when the user already passed `date = date`
  # (the original column IS the canonical one -- nothing to refill).
  # The internal `date` is always Date class (calculate_mcyclength coerces it via
  # lubridate::ymd). The user's original column, however, keeps whatever type was
  # passed in. character / factor date columns are a supported input
  # (calculate_mcyclength coerces them with lubridate::ymd), but dplyr::coalesce()
  # refuses to combine <character>/<factor> with <date> and errors. So for those
  # types, coerce the original to Date (the same coercion the package already
  # applies internally) before coalescing. Date and POSIXct originals coalesce
  # with `date` directly and keep their own type. The coercion is value-preserving
  # on observed days, so this only supplies the calendar date on fabricated rows
  # where the original was NA -- it never overwrites an observed value.
  #
  # The SAME densify leaves the user's original ID column NA on fabricated rows
  # whenever it is not literally named `id`. complete()/fill() keep the canonical
  # `id` populated (it is the group key), but a differently-named original id
  # column (e.g. `record_id`, `subject`) rides along as a passenger and is NA on
  # those rows -- so a downstream join on id + date would drop them just like the
  # date case. The canonical `id` is a plain copy of the original (no coercion),
  # so its type always matches and we coalesce directly -- no cast needed.
  # NA-fill only, and skipped when the user already passed `id = id`.
  id_name <- rlang::as_name(id)
  if (id_name != "id" && id_name %in% names(data)) {
    data <- data %>%
      dplyr::mutate(!!id := dplyr::coalesce(!!id, id))
  }

  date_name <- rlang::as_name(date)
  if (date_name != "date" && date_name %in% names(data)) {
    orig <- dplyr::pull(data, !!date)
    if (is.character(orig) || is.factor(orig)) {
      data <- data %>%
        dplyr::mutate(!!date := dplyr::coalesce(lubridate::ymd(!!date), date))
    } else {
      data <- data %>%
        dplyr::mutate(!!date := dplyr::coalesce(!!date, date))
    }
  }

  return(data)
}
