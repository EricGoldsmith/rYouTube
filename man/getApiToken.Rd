% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getToken.R
\name{getApiToken}
\alias{getApiToken}
\title{Get access token}
\usage{
getApiToken(key, secret)
}
\arguments{
\item{key}{Access key}

\item{secret}{Access secret}
}
\value{
token An access token
}
\description{
Authenticate and generate access token
}
\examples{
\dontrun{
# Credentials with "content owner" role, with access to all the needed YT channels
token <- getApiToken(key = getOption("ga.clientId"), secret = getOption("ga.clientSecret"))
}

}
