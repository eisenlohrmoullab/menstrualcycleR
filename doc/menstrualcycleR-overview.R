## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  cache = FALSE
)

## ----remotes, eval = F--------------------------------------------------------
# install.packages("remotes")

## ----loading-remotes, warning = F, eval = F-----------------------------------
# library(remotes)

## ----menstrualcycleR-package, eval = F----------------------------------------
# remotes::install_github("eisenlohrmoullab/menstrualcycleR")

## ----loading menstrualcycleR-package------------------------------------------
library(menstrualcycleR)

## ----tidyverse, eval=FALSE----------------------------------------------------
# install.packages("tidyverse")

## ----loading-tidyverse, warning=FALSE-----------------------------------------
library(tidyverse)

## ----loading-pcks, warning=FALSE, eval = F------------------------------------
# library(dplyr)
# library(ggplot2)
# library(purrr)
# library(tibble)
# library(tidyr)
# library(stringr)
# library(magrittr)

## ----gamm-packages-install, eval = F------------------------------------------
# install.packages("mgcv")
# install.packages("gam.hp")
# install.packages("marginaleffects")

## ----gamm-packages-libraries--------------------------------------------------
library(mgcv)
library(gam.hp)
library(marginaleffects)

## ----demo dataset-------------------------------------------------------------
cycle_df = cycledata
dim(cycle_df)
head(cycle_df)

## ----pacts-scaling------------------------------------------------------------
cycle_df_scaled = pacts_scaling(data = cycle_df, id = id, date = daterated, menses = menses, ovtoday = ovtoday, lower_cyclength_bound = 21, upper_cyclength_bound = 35)

## ----view-data----------------------------------------------------------------
names(cycle_df_scaled)

## ----cycledata_check----------------------------------------------------------
checkdata = cycledata_check(cycle_df_scaled, symptom_columns = c("symptom"))

## ----cycledata_check_by_id----------------------------------------------------
checkdata$by_id

## ----cycledata_check_overall--------------------------------------------------
checkdata$overall

## ----cycledata_check_plot ,fig.width=7, fig.height=6--------------------------
checkdata$data_symptom_plots

## ----ovulation_summary--------------------------------------------------------
ov_summary = summary_ovulation(cycle_df_scaled)

## ----ovulation_summary_total--------------------------------------------------
ov_summary$ovstatus_total

## ----ovulation_summary_id-----------------------------------------------------
ov_summary$ovstatus_id

## ----eval = F-----------------------------------------------------------------
# cycle_plot(
#   data,
#   symptom,
#   centering = "menses",
#   include_impute = TRUE,
#   y_scale = "person-centered_roll",
#   rollingavg = 5,
#   align_val = "center",
#   se = FALSE
# )

## ----eval = F-----------------------------------------------------------------
# zoo::rollapply(
#   variable,         # vector of values
#   rollingavg,       # size of the moving window (default = 5 days)
#   FUN = function(x) mean(x, na.rm = TRUE), # specifies a mean function to apply to each rolling window of values, ignoring missing values
#   align = "center", # align the output to the center of the window
#   fill = NA,        # fill edges with NA where full window is not available
#   partial = TRUE    # allow smaller windows at the beginning and end
# )

## ----cycle_plot_menses_raw----------------------------------------------------
cycle_plot_df_menses <- cycle_plot(
  cycle_df_scaled,
  "symptom",
  centering = "menses",
  include_impute = TRUE,
  y_scale = "person-centered_roll", 
  se = T
)


## ----cycle_plot_menses_raw_data-----------------------------------------------
cycle_plot_df_menses$data

## ----cycle_plot_menses_raw_summary--------------------------------------------
cycle_plot_df_menses$summary

## ----cycle_plot_menses_raw_plot, fig.width=7, fig.height=6--------------------
cycle_plot_df_menses$plot

## ----cycle_plot_ov_roll_plot--------------------------------------------------
cycle_plot_df_ov <- cycle_plot(
  cycle_df_scaled,
  "symptom",
  centering = "ovulation",
  include_impute = TRUE,
  y_scale = "person-centered_roll", 
  se = T
)


## ----cycle_plot_ov_roll_plot_access, fig.width=7, fig.height=6----------------
cycle_plot_df_ov$plot

## ----eval = F-----------------------------------------------------------------
# cycle_plot_individual(
#   data,
#   id,
#   symptoms,
#   centering = "menses",
#   y_scale = "person-centered",
#   include_impute = TRUE,
#   rollingavg = 5
# )

## ----cycle_plot_menses_id_2_raw_plot, fig.width=7, fig.height=5---------------
cycle_plot_menses_id_2 <- cycle_plot_individual(
  cycle_df_scaled,
  id = 2, 
  "symptom",
  centering = "menses",
  y_scale = "raw",
  include_impute = TRUE
  
)

cycle_plot_menses_id_2$symptom$Cycle_1$plot

## ----cycle_plot_menses_id_2_roll_plot, fig.width=7, fig.height=5--------------
cycle_plot_menses_id_2 <- cycle_plot_individual(
  cycle_df_scaled,
  id = 2, 
  "symptom",
  centering = "menses",
  y_scale = "roll",
  include_impute = TRUE,
  rollingavg = 3
)

cycle_plot_menses_id_2$symptom$Cycle_1$plot

## ----eval = F-----------------------------------------------------------------
# zoo::rollapply(
#   variable,         # vector of values
#   rollingavg,       # size of the moving window (default = 5 days)
#   FUN = function(x) mean(x, na.rm = TRUE), # specifies a mean function to apply to each rolling window of values, ignoring missing values
#   align = "center", # align the output to the center of the window
#   fill = NA,        # fill edges with NA where full window is not available
#   partial = TRUE    # allow smaller windows at the beginning and end
# )

## ----cycle_plot_menses_id_2_summary-------------------------------------------
cycle_plot_menses_id_2$symptom$Cycle_1$summary

## ----cycle_plot_menses_id_2_plot_access, fig.width=7, fig.height=5------------
cycle_plot_menses_id_2$symptom$Cycle_1$plot

## ----cycle_plot_ov_id_2_summary, fig.width=7, fig.height=5--------------------
cycle_plot_ov_id_2 <- cycle_plot_individual(
  cycle_df_scaled,
  id = 2, 
  "symptom",
  centering = "ovulation",
  y_scale = "roll",
  include_impute = TRUE,
  rollingavg = 3
)

cycle_plot_ov_id_2$symptom$Cycle_1$plot

## -----------------------------------------------------------------------------
cycle_df_scaled$symptom_log = log(cycle_df_scaled$symptom + 1) #log-transforming our outcome variable symptom

## -----------------------------------------------------------------------------
selected_vars <- c("scaled_cycleday_impute", "symptom_log" )
datSX <- cycle_df_scaled[complete.cases(cycle_df_scaled[selected_vars]), ]

## -----------------------------------------------------------------------------
datSX$id = as.factor(datSX$id) # ALWAYS factor id before putting it in a gam formula

gamm1 <- mgcv::gam(
  symptom_log ~ 
    s(scaled_cycleday_impute) + 
    s(id, bs = 're') + 
    s(scaled_cycleday_impute, id, bs = 're'),
  data = datSX, 
  method = 'REML'
)

## -----------------------------------------------------------------------------
plotdat <- expand.grid(scaled_cycleday_impute = seq(-1, 1, by = 0.05),
                      id = 0) # setting id = 0 suppresses random effects, to model the just the fixed effect (sample-wide) of your outcome across the cycle

# Predict using the model for each dataset and add predictions
pred <- marginaleffects::predictions(gamm1, newdata = plotdat, type = "response", transform = function(x) exp(x) - 1) # applying a transform function, to undo to the log transformation on symptom. The transform can be removed if your outcome was not log-transformed 
plotdat$estimate = pred$estimate
plotdat$conf.low = pred$conf.low
plotdat$conf.high = pred$conf.high

## ----gam_plot, fig.width=7, fig.height=5--------------------------------------
# Plotting
gamplot <- ggplot(plotdat, aes(x = scaled_cycleday_impute, y = estimate)) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.50), 
                     labels = c("0%L", "50%L", "Menses Onset", "50%F", "Ovulation")) +
  labs(x = "", y = "Symptom") + # You can change the y-axis label to reflect your outcome
  
  geom_rect(xmin = -0.04, xmax = 0.04, ymin = -Inf, ymax = Inf,
            fill = "grey70", alpha = 0.2, color = "white") +
  geom_rect(xmin = 0.92, xmax = 1, ymin = -Inf, ymax = Inf,
            fill = "grey87", alpha = 0.2, color = "white") +
  geom_line(size = 1, show.legend = TRUE) +
  # Adding CI ribbon with translucent light grey color
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = 0.3) +
  theme_minimal()

# Print the plot
print(gamplot)

## ----multilevel-effects-table, echo=FALSE, results='asis'---------------------
knitr::kable(
  data.frame(
    Level = c("Level-1/Within-person", "Level-1/Within-person", "Level-2/Between-person", "Level-2/Between-person"),
    Effect_Type = c("Fixed effect", "Random slope", "Intercept", "Random intercept"),
    GAMM_Component = c("s(scaled_cycleday_impute)", "s(scaled_cycleday_impute, id, bs = 're')", "Parametric Intercept", "`s(id, bs = 're')`"),
    Description = c(
      "Average within-person change across the time",
      "Individual differences in trajectories of outcome across time",
      "Average value outcome value when all other predictors are at their reference level",
      "Between-person differences in average outcome levels"
    ),
    Relevance_to_Cycle_Research = c(
      "Captures overall dynamic pattern across the cycle",
      "Accounts for heterogeneity/individual differences in outcomes across the cycle",
      "Average level of outcome in the data",
      "These differences in between-person traits cannot be explained by cycle predictors and can be considered a type of `error`. They may be explained by other between-person variables not included in the model, such as age or BMI"
    )
  ),
  caption = "Understanding Multilevel Effects in Menstrual Cycle Modeling",
  col.names = c("Level", "Effect Type", "GAMM Parameter", "Description", "Relevance to Cycle Research")
)

## -----------------------------------------------------------------------------
var.part = gam.hp::gam.hp(gamm1)
var.part$hierarchical.partitioning 

## ----total_within_person, fig.width=6, fig.height=4---------------------------
# Step 1: Create the data frame
variance_df <- data.frame(
  Component = c("Fixed effect of the cycle", 
                "Random slope of the cycle", 
                "Within-person residual"),
  Term = c("s(scaled_cycleday_impute)", 
           "s(scaled_cycleday_impute, id)", 
           "Residual (1 - Deviance Explained)"),
  Proportion = c(0.5460, 0.0212, 0.4328)  
)

# Normalize 
variance_df <- variance_df %>%
  mutate(Percent = round(Proportion / sum(Proportion) * 100, 1),
         Label = paste0(Component, "\n", Percent, "%"))

# Step 2: Display table
knitr::kable(variance_df[, c("Component", "Term", "Proportion")], 
      col.names = c("Component", "Model Term", "Proportion of Within-Person Variance"))

# Step 3: Create pie chart
ggplot(variance_df, ggplot2::aes(x = "", y = Proportion, fill = Label)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Total Within-person Variance") +
  theme(legend.title = element_blank())


## ----total_within_person_cycle, fig.width=6, fig.height=4---------------------
# Step 1: Create the data frame
variance_df <- data.frame(
  Component = c("Fixed effect of the cycle", 
                "Random slope of the cycle"),
  Term = c("s(scaled_cycleday_impute)", 
           "s(scaled_cycleday_impute, id)"),
  Proportion = c(0.5460, 0.0212)  
)

# Normalize 
variance_df <- variance_df %>%
  mutate(Percent = round(Proportion / sum(Proportion) * 100, 1),
         Label = paste0(Component, "\n", Percent, "%"))

# Step 2: Display table
knitr::kable(variance_df[, c("Component", "Term", "Proportion")], 
      col.names = c("Component", "Model Term", "Within-Person Variance Accounted for by the Cycle"))

# Step 3: Create pie chart
ggplot(variance_df,aes(x = "", y = Proportion, fill = Label)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Within-person Variance accounted for by the cycle") +
  theme(legend.title = element_blank())


