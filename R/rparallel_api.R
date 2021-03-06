
.RParallelURL <- function() {
  Sys.getenv("RPARALLEL_URL", unset="https://r-parallel.herokuapp.com/")
}

.RParallelHEAD <- function(path, ..., query=list(), accessToken=.PLLAccessToken()) {
  req <- HEAD(.RParallelURL(), path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.RParallelGET <- function(path, ..., query=list(), accessToken=.PLLAccessToken()) {
  req <- GET(.RParallelURL(), path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.RParallelPOST <- function(path, ..., query=list(), accessToken=.PLLAccessToken()) {
  req <- POST(.RParallelURL(), path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.RParallelPUT <- function(path, ..., query=list(), accessToken=.PLLAccessToken()) {
  req <- PUT(.RParallelURL(), path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.PUTFile <- function(url, fileLocation, ...) {
  req = PUT(url,
            body=upload_file(fileLocation),
            add_headers("Content-Type"=""),
            ...)
  req
}

.RParallelAuthorizationHeader <- function() {
  return(paste("Token", .PLLAccessToken()))
}

.RParallelCheck <- function(req) {
  if (req$status_code < 400) return(invisible())
  
  if (req$status_code == 401) { # Unauthenticated
    stop("HTTP authentication failed. Please set your API Access Token using the `PLLSetAccessToken` function. Type `?PLLSetAccessToken` for more information.", call. = FALSE)
  } else {
    message <- .RParallelParse(req)$message
    stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
  }
}

.RParallelParse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Set an HTTP Access Token for use in requests to the R-Parallel service.
#' By default, this parameter is read from the RPARALLEL_PAT environment variable, but can be overwritten using this function.
#'
#' @param accessToken Your Private Access Token (PAT), which can be found under account details at https://r-parallel.herokuapp.com/
#' @examples
#' PLLSetAccessToken("bd53d890b16773848fff3d226a761ed4") # This is an example key. Your key will be different.
#' @export
PLLSetAccessToken <- function(accessToken) {
  Sys.setenv(RPARALLEL_PAT = accessToken)
}

.PLLAccessToken <- function() {
  env <- Sys.getenv('RPARALLEL_PAT')
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set env var RPARALLEL_PAT to your RParallel personal access token",
      call. = FALSE)
  }

  message("Couldn't find environment variable RPARALLEL_PAT. See ?.PLLAccessToken for more details")
  message("Please enter your PAT and press enter:")
  accessToken <- readline(": ")

  if (identical(accessToken, "")) {
    stop("RParallel personal access token entry failed", call. = FALSE)
  }

  message("Updating RPARALLEL_PAT env var to PAT")
  PLLSetAccessToken(accessToken)

  accessToken
}


