#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get metrics for an owner
#'
#' @description Get metrics for an owner
#'
#'   \href{https://developers.google.com/youtube/analytics/content_owner_reports}{https://developers.google.com/youtube/analytics/content_owner_reports}
#'
#' @param token Access token
#' @param contentOwner Content owner.
#' @param from Starting date.
#' @param to Ending date.
#' @param metrics List of metrics. Defaults to \code{c("views")}.
#' @param dimensions List of dimensions. Defaults to \code{c("month")}.
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' ownerMetrics <- getOwnerMetrics(token, contentOwner = "ContentOwner",
#'                                 from = startDate, to = endDate)
#' }
#'
#' @export

getOwnerMetrics <- function(token, contentOwner, from, to, metrics = NULL, dimensions = NULL) {

  host <- "https://youtubeanalytics.googleapis.com"
  endpoint <- "/v2/reports"

  if (is.null(metrics)) {
    # Default metrics
    metrics <- c("views")
  }

  if (is.null(dimensions)) {
    # Default dimensions
    dimensions <- c("month")
  }

  message("Retrieving owner metrics ... ")

  url <- sprintf("%s%s?ids=contentOwner==%s&startDate=%s&endDate=%s&metrics=%s&dimensions=%s&filters=claimedStatus==claimed",
                 host, endpoint, contentOwner, from, to,
                 paste0(metrics, collapse = ","),
                 paste0(dimensions, collapse = ","))

  results <- queryAPI(token, url)

  if (is.null(results) || nrow(results$rows) < 1) {
    return(tibble::as_tibble(NULL))
  }

  columnHeaders <- results$columnHeaders
  if (!exists("columnHeaders")) {
    stop("getChannelMetrics() - Missing columnHeaders")
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

  message("\ndone.")

  return(df)
}
