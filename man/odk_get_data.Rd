% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odk_get_data.R
\name{odk_get_data}
\alias{odk_get_data}
\title{ODK get data}
\usage{
odk_get_data(
  url = "https://bohemia.systems",
  id = "recon",
  id2 = NULL,
  unknown_id2 = FALSE,
  uuids = NULL,
  exclude_uuids = NULL,
  user = NULL,
  password = NULL,
  widen = TRUE
)
}
\arguments{
\item{url}{The URL of the ODK Aggregate server, default being https://bohemia.systems, without a trailing dash}

\item{id}{The primary id of the form}

\item{id2}{The secondary id of the form}

\item{unknown_id2}{Set to TRUE only if the id2 is unknown. If it is known to be the same as id, set to FALSE. If it is known to be different from id, and is known, set to FALSE and make the id2 argument non-null.}

\item{uuids}{The uuid(s) (including the "uuid:" prefix) of the particular submission(s) to be retrieved. If NULL (the default), all uuids available will be retrieved}

\item{exclude_uuids}{The uuid(s) to exclude from retrieval. If NULL (the default), no uuids will be excluded}

\item{user}{The ODK Aggregate username}

\item{password}{The ODK Aggregate password}

\item{widen}{Whether to widen}
}
\value{
A list of dataframes
}
\description{
Retrieve data from the ODK server, parse, organize, and return as dataframes
}
