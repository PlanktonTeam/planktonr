% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{pr_apply_Flags}
\alias{pr_apply_Flags}
\title{Remove flagged data in df}
\usage{
pr_apply_Flags(df, flag_col)
}
\arguments{
\item{df}{A dataframe containing data with associated flags}

\item{flag_col}{A string specifying the column with the flag. Optional. If specified, all rows will be filter by the flag.}
}
\value{
A dataframe with flagged data removed
}
\description{
Remove flagged data in df
}
\examples{
df <- data.frame(SST = c(27.4, 28.9, 45), SST_Flag = c(1, 1, 4))
df <- pr_apply_Flags(df)
}
