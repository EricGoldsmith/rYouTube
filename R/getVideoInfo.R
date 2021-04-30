#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get information on a list of videos
#'
#' @description Get information on a list of videos
#'
#'   \href{https://developers.google.com/youtube/v3/docs/videos/list}{https://developers.google.com/youtube/v3/docs/videos/list}
#'
#' @param token Access token
#' @param videos List of videos
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' videoInfo <- getVideoInfo(token, videos$video)
#' }
#'
#' @export

getVideoInfo <- function(token, videos) {

  getInfo <- function(token, videos) {

    host <- "https://www.googleapis.com"
    endpoint <- "/youtube/v3/videos"

    url <- sprintf("%s%s?part=snippet,contentDetails&id=%s",
                   host, endpoint, paste0(videos, collapse = ","))

    results <- queryAPI(token, url)

    if (results$pageInfo$totalResults < 1) {
      return(tibble::as_tibble(NULL))
    }

    df <- results$items %>%
      dplyr::transmute(video = .data$id,
                       channel = .data$snippet.channelTitle,
                       pubDate = .data$snippet.publishedAt,
                       title = .data$snippet.title,
                       duration = gsub("^PT", "", .data$contentDetails.duration) %>% lubridate::duration() %>% as.integer(),
                       tags = .data$snippet.tags) %>%
      dplyr::mutate(pubDate = lubridate::ymd_hms(.data$pubDate, tz = "UTC"),
             tags = lapply(.data$tags, function(x) paste0(x, collapse = ",")) %>% unlist())

    return(df)
  }

  message(sprintf("Retrieving info for %s videos ... ", scales::comma(length(videos))))

  # Add column indicating grouping for chunked retrieval
  chunkSize <- 50   # Hard limit set by YT
  nChunks <- ceiling(length(videos) / chunkSize)
  videoList <- tibble::tibble(video = videos) %>%
    dplyr::mutate(chunk = rep(1:nChunks, length.out = dplyr::n()))

  # Retrieve video info
  results <- videoList %>%
    dplyr::group_by(.data$chunk) %>%
    dplyr::do(getInfo(token, .data$video)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$chunk)

  message("\ndone.")

  return(results)
}
