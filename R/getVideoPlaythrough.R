#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get play-through metrics for a list of videos
#'
#' @description Get audience retention, a.k.a. play-through metrics for a list of videos
#'
#'   \href{https://developers.google.com/youtube/analytics/metrics#Audience_Retention_Metrics}{https://developers.google.com/youtube/analytics/metrics#Audience_Retention_Metrics}
#'
#' @param token Access token
#' @param contentOwner Content owner
#' @param from Starting date
#' @param to Ending date
#' @param videos List of videos
#' @param metrics List of audience retention metrics. Defaults to \code{c("audienceWatchRatio")}
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' videoMetrics <- getVideoPlaythrough(token, contentOwner = "ContentOwner",
#'                                from = "2019-02-03", to = "2019-02-09", videos$video)
#' }
#'
#' @export

getVideoPlaythrough <- function(token, contentOwner, from, to, videos, metrics = NULL) {

  getMetrics <- function(token, contentOwner, from, to, video, metrics) {
    host <- "https://youtubeanalytics.googleapis.com"
    endpoint <- "/v2/reports"

    url <- sprintf("%s%s?ids=contentOwner==%s&startDate=%s&endDate=%s&metrics=%s&dimensions=video,elapsedVideoTimeRatio&filters=video==%s",
                   host, endpoint, contentOwner, from, to,
                   paste0(metrics, collapse = ","),
                   video)

    results <- queryAPI(token, url)

    if (is.null(results) || NROW(results$rows) < 1) {
      return(tibble::as_tibble(NULL))
    }

    columnHeaders <- results$columnHeaders
    if (!exists("columnHeaders")) {
      stop("getVideoPlaythrough() - Missing columnHeaders")
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
    message(sprintf("Retrieving play-through metrics for %s videos ... ", scales::comma(length(videos))))
    showStatus <- TRUE
  } else {
    showStatus <- FALSE
  }

  if (is.null(metrics)) {
    # Default metrics
    metrics <- c("audienceWatchRatio")
  }

  # Can only retreive audience retention metrics for one video at a time
  videoList <- tibble::tibble(video = videos)
  results <- videoList %>%
    dplyr::rowwise() %>%
    dplyr::do(getMetrics(token, contentOwner, from, to, .data$video, metrics)) %>%
    dplyr::ungroup()

  if (showStatus) {
    message("\ndone.")
  }

  return(results)
}

