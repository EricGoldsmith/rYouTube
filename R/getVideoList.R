#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom utils head


#' @title Get list of videos from a channel
#'
#' @description Get list of videos associated with a \code{playlistId}
#'
#'   \href{https://developers.google.com/youtube/v3/docs/playlistItems/list}{https://developers.google.com/youtube/v3/docs/playlistItems/list}
#'
#' @param token Access token
#' @param playlistId Playlist ID
#' @param limit Number of videos to return. Set to \code{Inf} for all
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' videos <- getVideoList(token, playlistId = ch$uploadsPlaylist, limit = Inf)
#' }
#'
#' @export

getVideoList <- function(token, playlistId, limit = 50) {
  host <- "https://www.googleapis.com"
  endpoint <- "/youtube/v3/playlistItems"

  # Limit set by YT
  maxResultsPerQuery <- 50

  # Probe query to determine how many results are available
  url <- sprintf("%s%s?part=snippet&maxResults=1&playlistId=%s",
                 host, endpoint, playlistId)

  results <- queryAPI(token, url)

  if (is.null(results)) {
    message(sprintf("getVideoList() - No videos for playlist id: %s.", playlistId))
    return(NULL)
  }

  availableResults <- results$pageInfo$totalResults
  desiredResults <- min(limit, availableResults)

  message(sprintf("Retrieving list of %s videos, from %s available ...",
                  scales::comma(desiredResults), scales::comma(availableResults)))

  nIterations <- ceiling(desiredResults / maxResultsPerQuery)

  # Initialize list of results
  dfList <- vector("list", nIterations)

  p <- dplyr::progress_estimated(nIterations)
  for (i in seq_along(1:nIterations)) {
    query <- sprintf("%s%s?part=snippet&maxResults=%s&playlistId=%s",
                     host, endpoint, maxResultsPerQuery, playlistId)

    if (i > 1) {
      # Append next page token
      query <- sprintf("%s&pageToken=%s", query, nextPageToken)
    }

    results <- queryAPI(token, query)

    if (!is.null(results)) {
      df <- results$items %>%
        dplyr::select(video = .data$snippet.resourceId.videoId,
                      pubDate = .data$snippet.publishedAt,
                      title = .data$snippet.title) %>%
        dplyr::mutate(pubDate = lubridate::ymd_hms(.data$pubDate, tz = "UTC"))

      # Add to results df list
      dfList[[i]] <- df

      # Get next page token
      nextPageToken <- results$nextPageToken

      p$tick()$print()
    }
  }

  message("\ndone.")

  # Bind all results into data frame
  return(dplyr::bind_rows(dfList) %>% head(limit))
}

