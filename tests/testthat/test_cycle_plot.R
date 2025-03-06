library(testthat)
library(menstrualcycleR)

# data("cycledata")
# data = calculate_mcyclength(data, id, daterated, menses, ovtoday)
# data = calculate_cycletime(data, id, daterated, menses, ovtoday)
# 
# test_that("cycle_plot with 'person-centered_roll' does not lead to an error", {
#   # Call cycle_plot with the specific option
#   expect_error(
#     cycle_plot(
#       data = data,
#       symptom = "symptom",
#       centering = "menses",
#       include_impute = TRUE,
#       y_scale = "person-centered_roll"
#     ),
#     NA, # Ensures no error is thrown
#     info = "cycle_plot should handle the 'person-centered_roll' option without errors"
#   )
# })
# 
# test_that("cycle_plot outputs mean_dev_roll with non-NA values in p$summary", {
#   # Call cycle_plot
#   p <- cycle_plot(
#     data = data,
#     symptom = "symptom",
#     centering = "menses",
#     include_impute = TRUE,
#     y_scale = "person-centered_roll"
#   )
#   
#   # Check if p$summary exists and is a data frame
#   expect_true(is.data.frame(p$summary), info = "p$summary should be a data frame")
#   
#   # Check if p$summary contains a column named mean_dev_roll
#   expect_true("mean_dev_roll" %in% colnames(p$summary), 
#               info = "p$summary should contain a column named 'mean_dev_roll'")
#   
#   # Check if mean_dev_roll contains non-NA values
#   expect_true(any(!is.na(p$summary$mean_dev_roll)), 
#               info = "'mean_dev_roll' column should contain non-NA values")
#   
#   # Print p$summary for verification
#   print(p$summary)
#   print(p$plot)
# })
