#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


#' @title Get list of captions for a video
#'
#' @description Get list of captions for a video
#'
#'   \href{https://developers.google.com/youtube/v3/docs/captions/list}{https://developers.google.com/youtube/v3/docs/captions/list}
#'
#' @param token Access token
#' @param videoId Video ID
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' captionList <- getVideoCaptionsList(token, video)
#' }
#'
#' @export
#'
getVideoCaptionsList <- function(token, videoId) {
  host <- "https://www.googleapis.com"
  endpoint <- "/youtube/v3/captions"

  url <- sprintf("%s%s?videoId=%s&part=snippet",
                 host, endpoint, videoId)

  result <- queryAPI(token, url)

  if (length(result$items) < 1) {
    message(sprintf("getVideoCaptionsList() - No captions found for video %s", videoId))
    # Cast to data frame to satisfy being called by dplyr::do()
    return(tibble::as_tibble(NULL))
  }

  return(result$items %>%
           dplyr::select(captionId = .data$id,
                         videoId = .data$snippet.videoId,
                         lang = .data$snippet.language,
                         type = .data$snippet.trackKind,
                         isCC = .data$snippet.isCC,
                         isLarge = .data$snippet.isLarge))
}


#' @title Get caption details for a video
#'
#' @description Get caption details for a video
#'
#'   \href{https://developers.google.com/youtube/v3/docs/captions/list}{https://developers.google.com/youtube/v3/docs/captions/list}
#'
#' @param token Access token
#' @param contentOwner Content owner
#' @param captionInfo Caption info from prior call to \link{getVideoCaptionsList}
#' @param format format
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' message("Retrieving caption info for each video ...")
#' captions <- videos %>% rowwise() %>% do(getVideoCaptionsList(token, .$video)) %>%
#'   filter(lang == "en", type %in% c("ASR", "standard"))
#'
#' message("Retrieving captions for each video ...")
#' captions <- captions %>% rowwise() %>%
#'   do(getCaptionData(token = token, contentOwner = "ContentOwner", caption = .))
#' }
#'
#' @export
#'

getCaptionData <- function(token, contentOwner, captionInfo, format = "vtt") {
  host <- "https://www.googleapis.com"
  endpoint <- "/youtube/v3/captions"

  # Cast to data frame to handle case of being called from dplyr::do()
  captionInfo <- tibble::as_tibble(captionInfo)

  url <- sprintf("%s%s/%s?tfmt=%s&onBehalfOfContentOwner=%s",
                 host, endpoint, captionInfo$captionId, format, contentOwner)

  results <- getRawResponseFromAPI(token, url)

  if (length(results) < 1) {
    message(sprintf("getCaptionData() - No caption data found for caption %s", captionInfo$captionId))
    # Cast to data frame to satisfy being called by dplyr::do()
    return(tibble::as_tibble(NULL))
  }

  captionInfo <- captionInfo %>%
    dplyr::mutate(captionData = results)

  return(captionInfo)
}


#' @title Clean up caption data
#'
#' @description Clean up caption data, by removing:
#'     \itemize{
#'       \item{inline tags and annotations}
#'       \item{timing indicators for phrases}
#'       \item{initial header information}
#'       \item{styling instructions}
#'       \item{content start tag}
#'       \item{extra newlines}
#'     }
#'
#'     and, optionally remove non-spoken content annotation is in square brackets or parenthesis
#'
#'   \href{https://developers.google.com/youtube/v3/docs/captions/list}{https://developers.google.com/youtube/v3/docs/captions/list}
#'
#' @param captionData Caption data from prior call to \link{getCaptionData}
#' @param removeNonSpokenAnnotations Remove non-spoken annotations (default TRUE)
#'
#' @return Returns a \code{\link{data.frame}} of results
#'
#' @examples
#' \dontrun{
#' cleanData = cleanCaptionData(captionData)
#' }
#'
#' @export
#'
#'
cleanCaptionData <- function(captionData, removeNonSpokenAnnotations = TRUE) {

  cleaned <- captionData %>%
    # remove all inline tags and annotations
    stringr::str_replace_all("<[A-Za-z0-9\\.\\/\\:]+>|align\\:start|position\\:\\d+\\%", "") %>%
    # remove all timing indicators for phrases
    stringr::str_replace_all(
      "\\n\\d{2}\\:\\d{2}\\:\\d{2}\\.\\d{3} \\-\\-> \\d{2}\\:\\d{2}\\:\\d{2}\\.\\d{3}\\s*?\\n", ""
    ) %>%
    # remove initial header information
    stringr::str_replace("WEBVTT\\nKind\\: captions\\nLanguage\\: [A-Za-z]{2}\\n", "") %>%
    # remove styling instructions
    stringr::str_replace_all("Style:\\n|\\:\\:cue.*?\\n \\}\\n", "") %>%
    # remove content start tag
    stringr::str_replace("##\\n", "") %>%
    # remove extra newlines
    stringr::str_replace_all("\\n", " ")

  if (removeNonSpokenAnnotations) {
    cleaned <- cleaned %>%
      # The standard is that non-spoken content annotation is in square brackets,
      stringr::str_replace_all("\\[.+?\\]", "") %>%
      # but we've run into cases where it's in parenthesis, too.
      stringr::str_replace_all("\\(.+?\\)", "")
  }

  return(cleaned)
}
