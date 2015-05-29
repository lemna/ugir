add.node <- function(dataframe, flowchart, label, title, id = 'lpnr', from = NA, display.r = TRUE, display.n = TRUE)
{
  if(!anyNA(from) & !all(from %in% flowchart$label)) stop('unknown labels: ', 
                                                          paste(from[!from %in% flowchart$label],
                                                                collapse = ','), call. = FALSE)
  r <- nrow(dataframe)
  n <- length(unique(dataframe[,id]))
  node <- data.frame(label = label,
                     title = title,
                     display.r = display.r,
                     display.n = display.n,
                     r = r,
                     n = n,
                     from = from)
  flowchart <- rbind(flowchart, node)
  return(flowchart)
}
