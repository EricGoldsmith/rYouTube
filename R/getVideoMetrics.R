#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get metrics for a list of videos
#'
#' @description Get metrics for a list of videos
#'
#'   \href{https://developers.google.com/youtube/analytics/content_owner_reports}{https://developers.google.com/youtube/analytics/content_owner_reports}
#'
#' @param token Access token
#' @param contentOwner Content owner
#' @param from Starting date
#' @param to Ending date
#' @param videos List of videos
#' @param metrics List of metrics. Defaults to \code{c("views", "comments", "likes", "dislikes", "shares", "averageViewDuration", "averageViewPercentage")}
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' videoMetrics <- getVideoMetrics(token, contentOwner = "ContentOwner",
#'                                from = "2019-02-03", to = "2019-02-09", videos$video)
#' }
#'
#' @export

getVideoMetrics <- function(token, contentOwner, from, to, videos, metrics = NULL) {

  getMetrics <- function(token, contentOwner, from, to, videos, metrics) {
    host <- "https://youtubeanalytics.googleapis.com"
    endpoint <- "/v2/reports"

    url <- sprintf("%s%s?ids=contentOwner==%s&startDate=%s&endDate=%s&metrics=%s&dimensions=video&filters=video==%s",
                   host, endpoint, contentOwner, from, to,
                   paste0(metrics, collapse = ","),
                   paste0(videos, collapse = ","))

    results <- queryAPI(token, url)

    if (is.null(results) || NROW(results$rows) < 1) {
      return(tibble::as_tibble(NULL))
    }

    columnHeaders <- results$columnHeaders
    if (!exists("columnHeaders")) {
      stop("getVideoMetrics() - Missing columnHeaders")
    }

    # Put results in data frame
    df <- as.data.frame(results$rows, stringsAsFactors = FALSE)
    names(df) <- columnHeaders$name

    # Convert columns to correct data types
    # There must be a more 'functional' way of doing this ...
    typeList <- list("STRING", "INTEGER", "FLOAT")
    funcList <- list(as.character, as.integer, as.numeric)

    for (i in 1:ncol(df)) {
      df[ , i] <- funcList[[match(columnHeaders$dataType[i], typeList)]](df[ , i])
    }

    return(df)
  }

  if (length(videos) > 1) {
    message(sprintf("Retrieving metrics for %s videos ... ", scales::comma(length(videos))))
    showStatus <- TRUE
  } else {
    showStatus <- FALSE
  }

  if (is.null(metrics)) {
    # Default metrics
    metrics <- c("views", "comments", "likes", "dislikes", "shares", "averageViewDuration", "averageViewPercentage")
  }

  # Add column indicating grouping for chunked retrieval
  chunkSize <- 200   # Hard limit set by YT
  nChunks <- ceiling(length(videos) / chunkSize)
  videoList <- tibble::tibble(video = videos) %>%
    dplyr::mutate(chunk = rep(1:nChunks, length.out = dplyr::n()))

  # Retrieve video metrics
  results <- videoList %>%
    dplyr::group_by(.data$chunk) %>%
    dplyr::do(getMetrics(token, contentOwner, from, to, .data$video, metrics)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$chunk)

  if (showStatus) {
    message("\ndone.")
  }

  return(results)
}

