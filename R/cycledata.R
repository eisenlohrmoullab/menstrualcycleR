#' Example longitudinal daily dataset with cycle measures 
#'
#' This dataset contains daily symptom and cycle information used to test and demonstrate the package.
#'
#' @format A data frame with 614 rows and 5 variables:
#' \describe{
#'   \item{id}{Participant ID}
#'   \item{date}{Date of symptom observation}
#'   \item{symptom}{Daily rating of symptom outcome of interest}
#'   \item{menses}{`1` indicating first day of menstruation, menses onset}
#'   \item{ovtoday}{`1` indicating estimated day of ovulation in study, based on biomarkers of ovulation.}
#' }
#' @source Simulated or anonymized data
"cycledata"
