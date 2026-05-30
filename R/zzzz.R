utils::globalVariables(c("ovtoday_impute", "scaled_cycleday", "scaled_cycleday_ov", "scaled_cycleday_impute", "scaled_cycleday_imp_ov", "m2mcount", "cycle_incomplete", "cycle_group", "percentlut", "percentfol", "percfol_ov", "lutperc_ov", "lutperc_imp_ov", "group", "perclut_impute", "lutperc1", "percfol_impute", "folperc", "percentfol_impute", "percentlut_impute", "percfol_ov_imp", "perclut_ov_imp", "mean_dev", "mean_dev_roll", "mean_sx", "has_data", "folmax", "follength", "foldaycount", "percfol", "foldaycount_impute", "folmax_impute", "lutdaycount1", "lutmax", "lutdaycount", "lutperc", "lutdaycount_ov", "lutdaycount1_impute", "lutlength1_impute", "lutmax_impute", "lutdaycount_impute", "cycles_outside_norm", "confirmed_ovulation", "imputed_ovulation", ".data", "cycleday_perc", "cycleday_5perc", "lutdaycount_imp_ov", "id", "count", "total", "forwardcount"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the menstrualcycleR package!\n",
    "If you use this package, please cite:\n",
    "Nagpal, A., Schmalenberger, K. M., Barone, J. C., Mulligan, E., Stumper, A., Knol, L., ",
    "Failenschmid, J., Kiesner, J., Peters, J. R., & Eisenlohr-Moul, T. A. (2025). ",
    "Studying the Menstrual Cycle as a Continuous Variable: Implementing Phase-Aligned Cycle Time Scaling (PACTS) with the `menstrualcycleR` package. ",
    "Psychoneuroendocrinology, 107584."
  )
}
