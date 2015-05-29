check.sex <- function(sex){
  sex <- as.character(sex)
  unique.sex <- unique(sex)
  wrong.sex <- !(unique.sex %in% c('1','2'))
  result <- rep(NA, times = length(sex))
  result[sex == '1'] <- 'male'
  result[sex == '2'] <- 'female'
  result <- factor(result)
  if(any(wrong.sex)) warning(paste('\n', unique.sex[wrong.sex],
                                   'interpreted as missing value and set to NA'))
  return(result)
}
