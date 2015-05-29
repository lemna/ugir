create.flowchart <- function()
{
  empty.flowchart <- data.frame(label = character(),
                                title = character(),
                                display.r = logical(),
                                display.n = logical(),
                                r = integer(),
                                n = integer(),
                                from = character(),
                                stringsAsFactors = FALSE)
  return(empty.flowchart)
}
