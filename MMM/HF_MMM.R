# https://dbc-7c975f4c-57fa.cloud.databricks.com/?o=0#notebook/4293428/command/4293432


library(dplyr)

#Loading The Data 
summary(hf_data)
colnames(hf_data)
hf_data$total_conv <- hf_data$total_conv...115

colnames(hf_data) <- gsub(" ", ".", colnames(hf_data))

data <- hf_data[,c("week_date",
           "TOP_FUNNEL_spend","LOWER_FUNNEL_spend","DIRECT_MAIL_spend","NATIVE_spend",
           "GDN_spend","TV_LINEAR_spend","PAID_SOCIAL_FB_spend","TikTok_spend","PAID_SOCIAL_NFB_spend","total_conv")]

#data <- data  %>% rename("High Profile_spend" = "High_Profile_spend", "First Media_spend" = "First_Media_spend")


#install.packages("remotes") # Install remotes first if you haven't already
#remotes::install_github("facebookexperimental/Robyn/R")
library(Robyn)

#install.packages("Robyn")
#install.packages("reticulate")
library(reticulate)

#virtualenv_create("r-reticulate")
#py_install("nevergrad", pip = TRUE)
use_virtualenv("r-reticulate", required = TRUE)

packageVersion("Robyn")

## Force multicore when using RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tipp: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory where you want to export results to (will create new folders)
robyn_object <- "C:/Users/03973881155331856642/MyProjects/MMM"


InputCollect <- robyn_inputs(
  dt_input = data,
  dt_holidays = dt_prophet_holidays,
  date_var = "week_date",                                                                 # date format must be "2020-01-01"
  dep_var = "total_conv",                                                               # there should be only one dependent variable
  dep_var_type = "conversion",                                                          # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"),                                    # "trend","season", "weekday" & "holiday"
  prophet_country = "US",                                                            # input one country. dt_prophet_holidays includes 59 countries by default
  #context_vars = c("competitor_sales_B", "events"),                                  # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("TOP_FUNNEL_spend","LOWER_FUNNEL_spend","DIRECT_MAIL_spend","NATIVE_spend",
                        "GDN_spend","TV_LINEAR_spend","PAID_SOCIAL_FB_spend","TikTok_spend","PAID_SOCIAL_NFB_spend"),       # mandatory input
  paid_media_vars = c("TOP_FUNNEL_spend","LOWER_FUNNEL_spend","DIRECT_MAIL_spend","NATIVE_spend",
                      "GDN_spend","TV_LINEAR_spend","PAID_SOCIAL_FB_spend","TikTok_spend","PAID_SOCIAL_NFB_spend"),  # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  #organic_vars = "newsletter",                                                       # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2020-01-06",
  window_end = "2022-12-26",
  adstock = "geometric"                                                              # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)


#### 2a-2: Second, define and add hyperparameters

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## Default media variable for modelling has changed from paid_media_vars to paid_media_spends.
## hyperparameter names needs to be base on paid_media_spends names. Run:
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
## to see correct hyperparameter names. Check GitHub homepage for background of change.
## Also calibration_input are required to be spend names.
## ------------------------------------------------------------------------------------------ ##

## Guide to setup & understand hyperparameters

## Robyn's hyperparameters have four components:
## Adstock parameters (theta or shape/scale).
## Saturation parameters (alpha/gamma).
## Regularisation parameter (lambda). No need to specify manually.
## Time series validation parameter (train_size).

## 1. IMPORTANT: set plot = TRUE to create example plots for adstock & saturation
## hyperparameters and their influence in curve transformation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

hyper_limits()

# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  DIRECT_MAIL_spend_alphas = c(0.5, 3),
  DIRECT_MAIL_spend_gammas = c(0.3, 1),
  DIRECT_MAIL_spend_thetas = c(0, 0.3),
  GDN_spend_alphas = c(0.5, 3),
  GDN_spend_gammas = c(0.3, 1),
  GDN_spend_thetas = c(0.1, 0.4),
  LOWER_FUNNEL_spend_alphas = c(0.5, 3),
  LOWER_FUNNEL_spend_gammas = c(0.3, 1),
  LOWER_FUNNEL_spend_thetas = c(0.3, 0.8),
  NATIVE_spend_alphas = c(0.5, 3),
  NATIVE_spend_gammas = c(0.3, 1),
  NATIVE_spend_thetas = c(0, 0.3),
  PAID_SOCIAL_FB_spend_alphas = c(0.5, 3),
  PAID_SOCIAL_FB_spend_gammas = c(0.3, 1),
  PAID_SOCIAL_FB_spend_thetas = c(0.1, 0.4),
  
  PAID_SOCIAL_NFB_spend_alphas = c(0.5, 3),
  PAID_SOCIAL_NFB_spend_gammas = c(0.3, 1),
  PAID_SOCIAL_NFB_spend_thetas = c(0.1, 0.4),
  
  TikTok_spend_alphas = c(0.5, 3),
  TikTok_spend_gammas = c(0.3, 1),
  TikTok_spend_thetas = c(0.1, 0.4),
  
  TOP_FUNNEL_spend_alphas = c(0.5, 3),
  TOP_FUNNEL_spend_gammas = c(0.3, 1),
  TOP_FUNNEL_spend_thetas = c(0.1, 0.4),
  
  TV_LINEAR_spend_alphas = c(0.5, 3),
  TV_LINEAR_spend_gammas = c(0.3, 1),
  TV_LINEAR_spend_thetas = c(0.1, 0.4)
  #train_size = c(0.5, 0.8)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)


#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}


## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to max available - 1
  iterations = 1000, # 2000 recommended for the dummy dataset with no calibration
  trials = 3, # 5 recommended for the dummy dataset
  ts_validation = FALSE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot



## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates
  #min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object, # path for plots export
  export = TRUE # this will create files locally
)
print(OutputCollect)



################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "1_119_5" # Pick one of the models from OutputCollect to proceed

#### Since 3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model)
print(ExportedModel)

###### DEPRECATED (<3.7.1) (might work)
# ExportedModelOld <- robyn_save(
#   robyn_object = robyn_object, # model object location and name
#   select_model = select_model, # selected model ID
#   InputCollect = InputCollect,
#   OutputCollect = OutputCollect
# )
# print(ExportedModelOld)
# # plot(ExportedModelOld)

################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition
# Run the "max_historical_response" scenario: "What's the revenue lift potential with the
# same historical spend level and what is the spend mix?"
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_historical_response",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7,0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  export = TRUE,
  date_min = "2020-01-06",
  date_max = "2022-12-26"
)
print(AllocatorCollect1)
plot(AllocatorCollect1)

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7,0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  expected_spend = 1000000, # Total spend to be simulated
  expected_spend_days = 30, # Duration of expected_spend in days
  export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut
plot(AllocatorCollect2)

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  expected_spend = 1000000, # Total spend to be simulated
  expected_spend_days = 30, # Duration of expected_spend in days
  export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut
plot(AllocatorCollect2)
