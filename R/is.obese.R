is.obese <- function(dataframe, icd, pattern = 'dia')
{
  stopifnot(all(icd %in% paste('icd', 7:10, sep = '')))
  temp.df <- subset(dataframe, select = grepl(x = names(dataframe), pattern = pattern))
  
  icd7 <- 	do.call(cbind, lapply(temp.df[icd == 'icd7',], grepl, pattern = '^287'))		# ICD-7
  icd8 <- 	do.call(cbind, lapply(temp.df[icd == 'icd8',], grepl, pattern = '^277'))		# ICD-8
  icd9 <- 	do.call(cbind, lapply(temp.df[icd == 'icd9',], grepl, pattern = '^278A'))	# ICD-9
  icd10 <-	do.call(cbind, lapply(temp.df[icd == 'icd10',], grepl, pattern = '^E66'))	# ICD-10
  
  icd7 <- apply(icd7, 1, any)
  icd8 <- apply(icd8, 1, any)
  icd9 <- apply(icd9, 1, any)
  icd10 <- apply(icd10, 1, any)
  
  obese <- rep(NA, times = nrow(temp.df))
  obese[icd == 'icd7'] <- icd7
  obese[icd == 'icd8'] <- icd8
  obese[icd == 'icd9'] <- icd9
  obese[icd == 'icd10'] <- icd10
  return(obese)
}
