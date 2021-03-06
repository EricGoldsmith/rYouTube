% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getVideoMetrics.R
\name{getVideoMetrics}
\alias{getVideoMetrics}
\title{Get metrics for a list of videos}
\usage{
getVideoMetrics(token, contentOwner, from, to, videos, metrics = NULL)
}
\arguments{
\item{token}{Access token}

\item{contentOwner}{Content owner}

\item{from}{Starting date}

\item{to}{Ending date}

\item{videos}{List of videos}

\item{metrics}{List of metrics. Defaults to \code{c("views", "comments", "likes", "dislikes", "shares", "averageViewDuration", "averageViewPercentage")}}
}
\value{
Returns a \code{\link{data.frame}} of results
}
\description{
Get metrics for a list of videos

  \href{https://developers.google.com/youtube/analytics/content_owner_reports}{https://developers.google.com/youtube/analytics/content_owner_reports}
}
\examples{
\dontrun{
videoMetrics <- getVideoMetrics(token, contentOwner = "ContentOwner",
                               from = "2019-02-03", to = "2019-02-09", videos$video)
}

}
