#
# Utility functions, only used internally
#
# Vignette for accessing Google APIs with OAuth from: https://github.com/hadley/httr/blob/master/demo/oauth2-google.r


getAndRetry <- function(token, url, nTries){
  tryNum <- 1

  while (tryNum <= nTries) {

    response <- httr::GET(url, httr::config(token = token))

    if (httr::http_status(response)$category == "Success")
      return(response)

    message(sprintf("%s: GET() failed: %s",
                    # Name of calling function
                    deparse(sys.call(-1)),
                    # Failure response
                    httr::http_status(response)$message),
            appendLF = FALSE)
    if (tryNum < nTries) {
      message(sprintf(" - retrying (%i of %i)", tryNum, nTries - 1))
    } else {
      message(".")
    }

    tryNum <- tryNum + 1
  }

  return(NULL)
}

#
# getRawResponseFromAPI()
#
getRawResponseFromAPI <- function(token, url) {
  response <- getAndRetry(token = token, url = url, nTries = 3)

  httr::warn_for_status(response)

  if (httr::http_status(response)$category == "Success") {
    return(httr::content(response, as = "text", encoding = "UTF-8"))
  } else {
    return(NULL)
  }
}


#
# queryAPI() -
#
queryAPI <- function(token, url) {
  response <- getRawResponseFromAPI(token, url)

  if (is.null(response)) {
    return(NULL)
  } else {
    return(jsonlite::fromJSON(response, flatten = TRUE))
  }
}
