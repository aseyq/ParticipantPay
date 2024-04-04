#' Create Prolific Payment Files
#'
#' This function writes payment files for Prolific participants.
#' @param df The dataframe containing Prolific participant data.
#' @param ProlificIDColumn The column containing Prolific IDs.
#' @param bonusColumn The column containing bonus amounts.
#' @param output_folder The folder where the payment files will be saved.
#' @return NULL
#' @export
create_prolific_files <- function(df, ProlificIDColumn, bonusColumn, output_folder=".") {
  current_time <- Sys.time()
  current_time_formatted <- format(current_time, "%Y%m%d_%H%M%S")

  ## append current file to text file
  string_title <- "====== ParticipantPay: Prolific Bulk Files === "
  write(string_title, file=paste0(output_folder, "/Prolific_", current_time_formatted, "_report.txt"), append=TRUE)
  string_date <- paste0("Current Date: ", format(current_time, "%d %B %Y %H:%M:%S"))
  write(string_date, file=paste0(output_folder, "/Prolific_", current_time_formatted, "_report.txt"), append=TRUE)

  # round bonus column to 2 decimal places
  df <- df %>%
    dplyr::mutate(!!rlang::sym(as.character(ensym(bonusColumn))) := round(!!rlang::sym(as.character(ensym(bonusColumn))), 2))

  dplyr::select(df, {{ProlificIDColumn}}) %>%
    readr::write_csv(paste0(output_folder, "/Prolific_", format(current_time_formatted, "%Y%m%d_%H%M%S"), "bulk_approval.txt"), col_names = FALSE)

  dplyr::select(df, {{ProlificIDColumn}}, {{bonusColumn}}) %>%
    print(n=50000) 

  total_bonus <- df %>%
    dplyr::summarise(total_bonus = sum(!!rlang::sym(as.character(ensym(bonusColumn))), na.rm = TRUE)) %>%
    dplyr::pull(total_bonus)

  message(paste0("Total bonus to pay: ", total_bonus))

  df %>%
    dplyr::select({{ProlificIDColumn}}, {{bonusColumn}}) %>%
    readr::write_csv(paste0(output_folder, "/Prolific_", current_time_formatted, "%Y%m%d_%H%M%S"), "bulk_payment.txt"), col_names = FALSE)
}
