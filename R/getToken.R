# Scopes needed by various function herein
# https://developers.google.com/identity/protocols/googlescopes
scopes <- c("https://www.googleapis.com/auth/youtube.readonly",
            "https://www.googleapis.com/auth/yt-analytics.readonly",
            "https://www.googleapis.com/auth/youtube.force-ssl",
            "https://www.googleapis.com/auth/youtubepartner")


#' @title Get access token
#'
#' @description Authenticate and generate access token
#'
#' @param key Access key
#' @param secret Access secret
#'
#' @return token An access token
#'
#' @examples
#' \dontrun{
#' # Credentials with "content owner" role, with access to all the needed YT channels
#' token <- getApiToken(key = getOption("ga.clientId"), secret = getOption("ga.clientSecret"))
#' }
#'
#' @export

getApiToken <- function(key, secret) {

  # OAuth settings for google:
  #    https://developers.google.com/accounts/docs/OAuth2InstalledApp
  myapp <- httr::oauth_app("google", key = key, secret = secret)
  endpoint <- httr::oauth_endpoints("google")
  scope = paste0(scopes, collapse = " ")

  # Get OAuth credentials
  google_token <- httr::oauth2.0_token(endpoint, myapp, scope)

  return(google_token)
}


#' @title Get service account token
#'
#' @description Authenticate and generate service account token
#'
#' @param service_token_creds_file Credentials file
#'
#' @return token A service account token
#'
#' @examples
#' \dontrun{
#' # Credentials with "content owner" role, with access to all the needed YT channels
#' token <- getServiceAccountToken("ServiceAccount-e8a41d405bf8.json")
#' }
#'
#' @export

getServiceAccountToken <- function(service_token_creds_file) {
  service_token <- jsonlite::read_json(service_token_creds_file)
  endpoint <- httr::oauth_endpoints("google")
  scope <- paste0(scopes, collapse = " ")

  cred <- httr::oauth_service_token(endpoint, service_token, scope)

  return(cred)
}
