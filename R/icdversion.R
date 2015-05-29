icdversion <- function(datevar)
{
  ICD7 <- new_interval(start = ymd('1958-01-01'), end = ymd('1968-12-31'))
  ICD8 <- new_interval(start = ymd('1969-01-01'), end = ymd('1986-12-31'))
  ICD9 <- new_interval(start = ymd('1987-01-01'), end = ymd('1996-12-31'))
  ICD10 <- new_interval(start = ymd('1997-01-01'), end = today())
  result <- rep(NA, times = length(datevar))
  result[datevar %within% ICD7] <- 'icd7'
  result[datevar %within% ICD8] <- 'icd8'
  result[datevar %within% ICD9] <- 'icd9'
  result[datevar %within% ICD10] <- 'icd10'
  result <- factor(result, levels = paste('icd', 7:10, sep = ''))
  return(result)
}
