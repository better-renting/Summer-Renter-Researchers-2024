#================================================
#      STATE data summary - Night
#================================================

# Convert the "Time" column to POSIXct time objects
I_data$Time <- as.POSIXct(I_data$Time, format="%I:%M %p")

# Define the nighttime as being from 10:00 PM to 6:00 AM
# Note: You might need to adjust the "origin" parameter based on the actual dates in your dataset
night_start <- as.POSIXct("22:00", format="%H:%M", origin="1970-01-01")
night_end <- as.POSIXct("06:00", format="%H:%M", origin="1970-01-01")

# Filter I_data to only include nighttime data
nighttime_data <- I_data %>%
  filter((Time >= night_start & Time <= as.POSIXct("23:59:59", format="%H:%M:%S", origin="1970-01-01")) |
           (Time >= as.POSIXct("00:00", format="%H:%M", origin="1970-01-01") & Time <= night_end))

# Proceed with summarization only on nighttime data
State_summary <- nighttime_data %>% group_by(State) %>%
  dplyr::summarise(
    n_State = n(),
    #Time below threshold temp (n then %)
    Thresh_T35_n = sum(T_in_mean>35, na.rm = TRUE),
    Thresh_T30_n = sum(T_in_mean>30, na.rm = TRUE),
    Thresh_T26_n = sum(T_in_mean>26, na.rm = TRUE),
    Thresh_T25_n = sum(T_in_mean>25, na.rm = TRUE),
    
    Thresh_T35 = round(Thresh_T35_n*100/n_State,1),
    Thresh_T30 = round(Thresh_T30_n*100/n_State,1),
    SleepThresh_T26 = round(Thresh_T26_n*100/n_State,1),
    Thresh_T25 = round(Thresh_T25_n*100/n_State,1),
    
    # Calculate the number of times T_in is greater than T_out, while outside is 21 or above
    In_v_out_n = sum(T_in_mean > T_out_mean & T_out_mean > 21, na.rm = TRUE),
    # Calculate the percentage of times T_in is greater than T_out
    In_v_out = round(In_v_out_n*100/n_State,1),

    #Basic stats
    T_in_min = quantile(T_in_mean, probs = 0.01),
    T_in_q25 = quantile(T_in_mean, probs = 0.25), 
    T_in_median = quantile(T_in_mean, probs = 0.5),
    T_in_q75 = quantile(T_in_mean, probs = 0.75),
    T_in_max = quantile(T_in_mean, probs = 0.99),
    T_in_max_max = max(T_in_max),
    T_in_mean = round(mean(T_in_mean),1),
    H_in_median = quantile(H_in_mean, probs = 0.5),
    H_in_max = max(H_in_max),
    H_in_mean = round(mean(H_in_mean),1),
 
    T_out_median = round(quantile(T_out_mean, probs = 0.5, na.rm = TRUE),1),
    T_out_max = round(quantile(T_out_max, probs = 0.99, na.rm = TRUE),1),
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
  write.csv(State_summary, "data_summaries/State/S_State_summary_Night.csv", row.names = FALSE)
  cat("Successfully wrote State_summary_Night to CSV.\n")
}, error = function(e) {
  cat("Failed to write State_summary_Night to CSV. Error: ", e$message, "\n")
})
