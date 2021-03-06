% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df2neo4j.R
\name{unique_sep_values_of_col}
\alias{unique_sep_values_of_col}
\title{unique_sep_values_of_col}
\usage{
unique_sep_values_of_col(df, col, label = "df_col_name", sep = ";", ...)
}
\arguments{
\item{df}{The data.frame containing the column of data.}

\item{col}{The name of the data column, as a string.}

\item{label}{The new column name. As a string.}

\item{sep}{The separator between items in a cell of the column, eg "," or ";"}
}
\description{
create a new single-column data.frame of unique values from a column of data that may
include multiple values in a cell separated by a separator character.
}
\details{
This takes a column of data that may include multiple values in a cell separated
by a separator character and creates a new single-column data.frame of unique values.
}
