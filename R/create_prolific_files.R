#' Create Prolific Payment Files
#'
#' This function writes payment files for Prolific participants.
#' @param df The dataframe containing Prolific participant data.
#' @param ProlificIDColumn The column containing Prolific IDs.
#' @param bonusColumn The column containing bonus amounts.
#' @param output_folder The folder where the payment files will be saved.
#' @return NULL
#' @export
append_prolific_file_title <- function(file_path, current_time) {
  current_date_formatted <- format(current_time, "%d %B %Y %H:%M:%S")
  string_title <- paste0("====== ParticipantPay: Prolific Bulk Files ===\n", "Current Date: ", current_date_formatted, "\n \n")
  write(string_title, file=file_path, append=TRUE)
}

# Main function to create Prolific files
create_prolific_files <- function(df, ProlificIDColumn, bonusColumn, output_folder=".", current_time = Sys.time()) {
  current_time_formatted <- format(current_time, "%Y%m%d_%H%M%S")

  # Define file names
  report_file <- paste0(output_folder, "/Prolific_", current_time_formatted, "_report.txt")
  bulk_approval_file <- paste0(output_folder, "/Prolific_", current_time_formatted, "_bulk_approval.txt")
  bulk_payment_file <- paste0(output_folder, "/Prolific_", current_time_formatted, "_bulk_payment.txt")

  ## append current file to text file
  append_prolific_file_title(report_file, current_time)

  # Round bonus column to 2 decimal places
  original_bonus <- df[[bonusColumn]]
  df[[bonusColumn]] <- round(df[[bonusColumn]], 2)
  
  # Warn if values are rounded
  if(!identical(original_bonus, df[[bonusColumn]])) {
    warning_message <- "Values in the bonus column have been rounded to 2 decimal places."
    warning(warning_message)
    write(paste0("Warning: ", warning_message, "\n"), file=report_file, append=TRUE)
  }

  # Write ProlificIDColumn to bulk_approval file
  df %>%
    dplyr::select({{ProlificIDColumn}}) %>%
    readr::write_csv(bulk_approval_file, col_names = FALSE)

  # Print ProlificIDColumn and bonusColumn and append to report file
  report_data <- df %>%
    dplyr::select({{ProlificIDColumn}}, {{bonusColumn}}) %>%
    print()

  # Append report data to report file
  write.table(report_data, file = report_file, append = TRUE, sep = "\t", col.names = FALSE, quote = FALSE)

  # Calculate total bonus
  total_bonus <- sum(df[[bonusColumn]], na.rm = TRUE)

  # Calculate total bonus with 25%, 30%, and 40% fees
  total_bonus_25 <- total_bonus * 1.25
  total_bonus_30 <- total_bonus * 1.30
  total_bonus_40 <- total_bonus * 1.40

  # Calculate rounding error
  rounding_error <- total_bonus - sum(original_bonus, na.rm = TRUE)

  # Display and append total bonus, fees, and rounding error to report file
  string_total_bonus <- paste0("\nTotal bonus to pay: ", total_bonus, "  (Avg: ", total_bonus/nrow(df), ") \n",
                               "   ➡️ Total bonus with 25% fee: ", total_bonus_25, "\n", 
                               "   ➡️ Total bonus with 30% fee: ", total_bonus_30, "\n",
                               "   ➡️ Total bonus with 40% fee: ", total_bonus_40, "\n",
                               "Rounding error: ", rounding_error, "\n")
  message(string_total_bonus)
  write(string_total_bonus, file=report_file, append=TRUE)

  ## append to report file
  df %>%
    dplyr::select({{ProlificIDColumn}}, {{bonusColumn}}) %>%
    readr::write_csv(bulk_payment_file, col_names = FALSE)
}
