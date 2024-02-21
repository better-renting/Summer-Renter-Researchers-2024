#=========================================
#   Prepare data
#=========================================

# Function to safely read CSV and return NULL on failure
safe_read_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    cat("File not found:", file_path, "\n")
    return(NULL)
  }
  cat("Reading file:", file_path, "\n")
  result <- tryCatch(
    {
      read.csv(file_path, header = TRUE)
    },
    error = function(e) {
      cat("Error reading file:", file_path, "\nError message:", e$message, "\n")
      return(NULL)
    },
    warning = function(w) {
      cat("Warning while reading file:", file_path, "\nWarning message:", w$message, "\n")
    }
  )
  cat("Finished reading file:", file_path, "\n")
  return(result)
}



# Import CSV from relative path
cat("Loading RawData.csv...\n")
I_data <- safe_read_csv("data/RawData.csv")

# Check if I_data is loaded correctly
if (is.null(I_data)) {
  cat("Failed to load I_data. Exiting script.\n")
} else {
  cat("I_data loaded successfully.\n")
  
  #==== Base data for Indoor observations ====
  # Remove blank indoor temperature/humidity observations
  cat("Cleaning I_data for indoor observations...\n")
  I_data <- I_data %>% tidyr::drop_na(T_in_mean) %>% tidyr::drop_na(H_in_mean)
  
  # List States and Trackers present in data
  I_States <- unique(I_data$State)
  I_Trackers <- unique(I_data$ID_Device)
  cat("Unique states and trackers identified in I_data.\n")
  
  # Commenting out this section as it no longer works (due to changes in the source data)
  # Rental Type data
  # RT_data <- I_data %>% tidyr::drop_na(RType)
  # List states with RentalTypes
  # RT_States <- unique(RT_data$State)
  # cat("Rental type data processed.\n")
  
  #==== Base data for Outdoor observations (with corresponding indoor observations) ====
  # Create data files for available outside temperature observations
  IO_data <- I_data %>% tidyr::drop_na(T_out_mean) %>% tidyr::drop_na(H_out_mean)
  # List States and Trackers present in data
  IO_States <- unique(IO_data$State)
  IO_Trackers <- unique(IO_data$ID_Device)
  cat("Outdoor observation data prepared.\n")
  
  # Labels for archery target plots
  cat("Loading ArcheryTargetLabels.csv...\n")
  TargetLabels <- safe_read_csv("data/ArcheryTargetLabels.csv")
  if (is.null(TargetLabels)) {
    cat("Failed to load TargetLabels. Check the file and try again.\n")
  } else {
    cat("TargetLabels loaded successfully.\n")
  }
}

