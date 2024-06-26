% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_prolific_files.R
\name{create_prolific_files}
\alias{create_prolific_files}
\title{Create Prolific Payment Files}
\usage{
create_prolific_files(df, ProlificIDColumn, bonusColumn, output_folder = ".")
}
\arguments{
\item{df}{The dataframe containing Prolific participant data.}

\item{ProlificIDColumn}{The column containing Prolific IDs.}

\item{bonusColumn}{The column containing bonus amounts.}

\item{output_folder}{The folder where the payment files will be saved.}
}
\description{
This function writes payment files for Prolific participants.
}
