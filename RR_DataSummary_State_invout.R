#================================================
#      STATE data summary - ALL
#================================================

# Summarise data for the states
# na.RM = TRUE excludes empty data values from the calculation of the summary
# n is calculated on indoor observations. This may be lower for outdoor observations
# Thresholds don't seem to work when added at the end of the list (???)
#
# 1% and 99% quantiles are used for min/max calculations to remove small numbers of outliers observed in both indoor and outdoor measurement 
# (e.g. handling the tracker may result in a temperature spike)

# Define threshold for outdoor temperature
temperature_threshold <- 21

State_summary <- I_data %>%  group_by(State) %>%
  dplyr::summarise(
    n_State = n(),


    # Calculate the proportion of time T_out > 21
    Proportion_T_out_gt_21 = round(sum(T_out_mean > temperature_threshold, na.rm = TRUE) / n(), 3),
    #Calculate the proportion of time T_out > 21
    Proportion_T_out_gt_21_b = round(sum(T_out_mean > temperature_threshold, na.rm = TRUE) / n_State, 1),
    # Calculate the number of times T_in is greater than T_out, while outside is 21 or above
    In_v_out_n = sum(T_in_mean > T_out_mean & T_out_mean > temperature_threshold, na.rm = TRUE),
    # Calculate the percentage of times T_in is greater than T_out
    In_v_out = round(In_v_out_n*100/n_State,1),
    # Calculate the mean difference between T_in and T_out when T_out > 21
    T_diff_mean = round(mean(ifelse(T_out_mean > temperature_threshold, T_in_mean - T_out_mean, NA), na.rm = TRUE), 1),
    # Calculate the mean difference between T_in and T_out when T_out > 21 AND T_in > T_out
    T_diff_mean_21 = round(mean(ifelse(T_out_mean > temperature_threshold & T_in_mean > T_out_mean, T_in_mean - T_out_mean, NA), na.rm = TRUE), 1),
    # Calculate mean outdoor temperature for T_out > 21
    T_out_hot_mean = round(mean(ifelse(T_out_mean > temperature_threshold, T_out_mean, NA), na.rm = TRUE), 1),
    
    
    #Basic stats

    T_in_mean = round(mean(T_in_mean),1),
    T_out_mean = round(mean(T_out_mean, na.rm = TRUE),1),
    
   
    )

# Check if data frame exists.
if (exists("State_summary") && nrow(State_summary) > 0) {
  cat("State_summary exists and is not empty. Proceeding to write CSV.\n")
} else {
  cat("State_summary does not exist or is empty. Cannot write CSV.\n")
  stop("Stopping script due to missing or empty State_summary.")
}

# Check that target directory exists
if (!dir.exists("data_summaries/State/")) {
  cat("Directory 'data_summaries/State/' does not exist. Creating it now.\n")
  dir.create("data_summaries/State/", recursive = TRUE)
}

# Export CSV
tryCatch({
  write.csv(State_summary, "data_summaries/State/S_State_summary_invout_25.csv", row.names = FALSE)
  cat("Successfully wrote State_summary_invout to CSV.\n")
}, error = function(e) {
  cat("Failed to write State_summary_invout to CSV. Error: ", e$message, "\n")
})