
.howardURL <- Sys.getenv("HOWARD_URL", unset="https://r-parallel.herokuapp.com/") # FIXME Make this https

.HowardGET <- function(path, ..., query=list(), accessToken=.HowardAccessToken()) {
  req <- GET(.howardURL, path=path, query=query, add_headers(Authorization=.HowardAuthorizationHeader()), ...)
  .HowardCheck(req)
  req
}
.HowardPOST <- function(path, ..., query=list(), accessToken=.HowardAccessToken()) {
  req <- POST(.howardURL, path=path, query=query, add_headers(Authorization=.HowardAuthorizationHeader()), ...)
  .HowardCheck(req)
  req
}
.HowardPUT <- function(path, ..., query=list(), accessToken=.HowardAccessToken()) {
  req <- PUT(.howardURL, path=path, query=query, add_headers(Authorization=.HowardAuthorizationHeader()), ...)
  .HowardCheck(req)
  req
}
.PUTFile <- function(url, fileLocation, ...) {
  req = PUT(url,
            body=upload_file(fileLocation),
            add_headers("Content-Type"=""),
            ...)
  req
}

.HowardAuthorizationHeader <- function() {
  return(paste("Token", .HowardAccessToken()))
}

.HowardCheck <- function(req) {
  if (req$status_code < 400) return(invisible())
  
  message <- .HowardParse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

.HowardParse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

.HowardAccessToken <- function() {
  env <- Sys.getenv('HOWARD_PAT')
  if (!identical(env, "")) return(env)

  if (!interactive()) {
    stop("Please set env var HOWARD_PAT to your Howard personal access token",
      call. = FALSE)
  }

  message("Couldn't find environment variable HOWARD_PAT. See ?.HowardAccessToken for more details")
  message("Please enter your PAT and press enter:")
  accessToken <- readline(": ")

  if (identical(accessToken, "")) {
    stop("Howard personal access token entry failed", call. = FALSE)
  }

  message("Updating HOWARD_PAT env var to PAT")
  Sys.setenv(HOWARD_PAT = accessToken)

  accessToken
}


