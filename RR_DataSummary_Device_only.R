#================================================
#      DEVICE data summary - ALL
#================================================

# Summarise data for the states
# na.RM = TRUE excludes empty data values from the calculation of the summary
# n is calculated on indoor observations. This may be lower for outdoor observations
# Thresholds don't seem to work when added at the end of the list (???)
#
# 1% and 99% quantiles are used for min/max calculations to remove small numbers of outliers observed in both indoor and outdoor measurement 
# (e.g. handling the tracker may result in a temperature spike)

Device_summary <- I_data %>%  group_by(ID_Device, State) %>%
  dplyr::summarise(
    
    n_Device = n(),
    
    # Convert the 'Date' column to Date objects with the correct format
    Earliest_Date = min(as.Date(Date, format="%d/%m/%Y")),
    Latest_Date = max(as.Date(Date, format="%d/%m/%Y")),
    
    #Time below threshold temp (n then %)
    Thresh_T35_n = sum(T_in_mean>35, na.rm = TRUE),
    Thresh_T30_n = sum(T_in_mean>30, na.rm = TRUE),
    Thresh_T25_n = sum(T_in_mean>25, na.rm = TRUE),

    Thresh_T35 = round(Thresh_T35_n*100/n_Device,1),
    Thresh_T30 = round(Thresh_T30_n*100/n_Device,1),
    Thresh_T25 = round(Thresh_T25_n*100/n_Device,1),
    
    #Basic stats
    T_in_min = quantile(T_in_mean, probs = 0.01),
    T_in_q25 = quantile(T_in_mean, probs = 0.25), 
    T_in_median = quantile(T_in_mean, probs = 0.5),
    T_in_q75 = quantile(T_in_mean, probs = 0.75),
    T_in_max_99 = quantile(T_in_mean, probs = 0.99), #This is a rounded max - removes outliers
    T_in_max_max = max(T_in_max), #This is the absolute max
    T_in_mean = round(mean(T_in_mean),1),
    H_in_median = quantile(H_in_mean, probs = 0.5),
    H_in_max = max(H_in_max),
    H_in_mean = round(mean(H_in_mean),1),
    
    T_out_median = round(quantile(T_out_mean, probs = 0.5, na.rm = TRUE),1),
    T_out_max = round(quantile(T_out_max, probs = 0.99, na.rm = TRUE),1),
    T_out_mean = round(mean(T_out_mean, na.rm = TRUE),1),
  )

assign(paste("Summary_D_",i,"_ALL", sep = ""),df )


# Check if data frame exists.
if (exists("Device_summary") && nrow(Device_summary) > 0) {
  cat("Device_summary exists and is not empty. Proceeding to write CSV.\n")
} else {
  cat("Device_summary does not exist or is empty. Cannot write CSV.\n")
  stop("Stopping script due to missing or empty Device_summary.")
}

# Check that target directory exists
if (!dir.exists("data_summaries/State/")) {
  cat("Directory 'data_summaries/State/' does not exist. Creating it now.\n")
  dir.create("data_summaries/State/", recursive = TRUE)
}

# Export CSV
tryCatch({
  write.csv(Device_summary, "data_summaries/State/S_State_device_summary_only.csv", row.names=FALSE)
  cat("Successfully wrote State_device_summary_only to CSV.\n")
}, error = function(e) {
  cat("Failed to write State_device_summary_only to CSV. Error: ", e$message, "\n")
})
