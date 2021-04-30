#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get list of channels controlled by the \code{contentOwner}
#'
#' @description Get list of channels controlled by the \code{contentOwner}
#'
#'   \href{https://developers.google.com/youtube/v3/docs/channels/list}{https://developers.google.com/youtube/v3/docs/channels/list}
#'
#' @param token Access token
#' @param contentOwner Content owner
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' channels <- getChannels(token, contentOwner = "ContentOwner")
#' }
#'
#' @export

getChannels <- function(token, contentOwner) {
  host <- "https://www.googleapis.com"
  endpoint <- "/youtube/v3/channels"

  url <- sprintf("%s%s?part=snippet,contentDetails&managedByMe=true&onBehalfOfContentOwner=%s&maxResults=50",
                 host, endpoint, contentOwner)

  results <- queryAPI(token, url)

  if (is.null(results)) {
    message(sprintf("getChannels() - No channels controlled by '%s'", contentOwner))
    return(NULL)
  }

  if (results$pageInfo$totalResults > results$pageInfo$resultsPerPage) {
    message(sprintf("getChannels() - More than a single page of results (total: %d, per-page: %d) - refactor function to handle this.",
                    results$pageInfo$totalResults, results$pageInfo$resultsPerPage))
  }

  return(results$items %>% dplyr::select(channelId = .data$id,
                                         name = .data$snippet.title,
                                         uploadsPlaylist = .data$contentDetails.relatedPlaylists.uploads))
}
