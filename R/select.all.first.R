select.all.first <- function(x, lpnr = 'lpnr', date = 'diadat')
{
  stopifnot(is.data.frame(x))
  stopifnot(lpnr %in% names(x))
  stopifnot(date %in% names(x))
  x <- x[order(x[, lpnr], x[, date]),]
  lpnr.date <- paste(x[,lpnr], x[,date])
  first.record <- lpnr.date[!duplicated(x[,lpnr])]
  result <- x[lpnr.date %in% first.record,]
  return(result)
}
