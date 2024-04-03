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

  # round bonus column to 2 decimal places
  df <- df %>%
    dplyr::mutate(!!rlang::sym(as.character(ensym(bonusColumn))) := round(!!rlang::sym(as.character(ensym(bonusColumn))), 2))

  dplyr::select(df, {{ProlificIDColumn}}) %>%
    readr::write_csv(paste0(output_folder, "/bulk_approval_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"), col_names = FALSE)

  dplyr::select(df, {{ProlificIDColumn}}, {{bonusColumn}}) %>%
    print(n=500) 

  total_bonus <- df %>%
    dplyr::summarise(total_bonus = sum(!!rlang::sym(as.character(ensym(bonusColumn))), na.rm = TRUE)) %>%
    dplyr::pull(total_bonus)

  message(paste0("Total bonus to pay: ", total_bonus))

  df %>%
    dplyr::mutate(total_bonus_with_fees = !!rlang::sym(as.character(ensym(bonusColumn))) * 1.4) %>%
    dplyr::select({{ProlificIDColumn}}, total_bonus_with_fees) %>%
    readr::write_csv(paste0(output_folder, "/bulk_payment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"), col_names = FALSE)
}
