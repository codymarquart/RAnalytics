#' Convert JS Milliseconds to DateTime
#' @export
MilliSecondsToDate = function(x, timezone="GMT") {
  as.POSIXct(x, origin="1970-01-01", tz=timezone)
}