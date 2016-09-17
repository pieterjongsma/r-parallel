
RParallelURL <- Sys.getenv("RPARALLEL_URL", unset="https://r-parallel.herokuapp.com/") # FIXME Make this https

.RParallelGET <- function(path, ..., query=list(), accessToken=.RParallelAccessToken()) {
  req <- GET(RParallelURL, path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.RParallelPOST <- function(path, ..., query=list(), accessToken=.RParallelAccessToken()) {
  req <- POST(RParallelURL, path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
  .RParallelCheck(req)
  req
}
.RParallelPUT <- function(path, ..., query=list(), accessToken=.RParallelAccessToken()) {
  req <- PUT(RParallelURL, path=path, query=query, add_headers(Authorization=.RParallelAuthorizationHeader()), ...)
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
  return(paste("Token", .RParallelAccessToken()))
}

.RParallelCheck <- function(req) {
  if (req$status_code < 400) return(invisible())
  
  message <- .RParallelParse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

.RParallelParse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

PLLSetAccessToken <- function(accessToken) {
  Sys.setenv(RPARALLEL_PAT = accessToken)
}

.RParallelAccessToken <- function() {
  env <- Sys.getenv('RPARALLEL_PAT')
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set env var RPARALLEL_PAT to your RParallel personal access token",
      call. = FALSE)
  }

  message("Couldn't find environment variable RPARALLEL_PAT. See ?.RParallelAccessToken for more details")
  message("Please enter your PAT and press enter:")
  accessToken <- readline(": ")

  if (identical(accessToken, "")) {
    stop("RParallel personal access token entry failed", call. = FALSE)
  }

  message("Updating RPARALLEL_PAT env var to PAT")
  PLLSetAccessToken(accessToken)

  accessToken
}


