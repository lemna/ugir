report.date.errors <- function(datevar)
{
  datevar.char <- as.character(datevar)
  wrong.format <- which(nchar(datevar.char) != 8)
  no.day <- which(nchar(datevar.char) == 6)
  no.month <- which(nchar(datevar.char) == 4)
  empty <- which(is.na(datevar.char) | datevar.char == '')
  datevar.date <- ymd(datevar.char)
  not.parsable <- which(is.na(datevar.date))
  wrong.date <- setdiff(not.parsable, union(empty, union(no.day, no.month)))
  result <- list(wrong.format = wrong.format,
                 no.day = no.day,
                 no.month = no.month,
                 empty = empty,
                 not.parsable = not.parsable,
                 wrong.date = wrong.date)                  
  return(result)
}
