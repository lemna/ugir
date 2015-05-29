create.diagram <- function(flowchart, preamble = 'digraph {\nnode[shape=box, fontname = Helvetica]')
{
  flowchart.nostart <- na.omit(flowchart)
  to.nodes.present <- nrow(flowchart.nostart) > 0
  if(to.nodes.present) {
    from.edges <- paste('\'@@1-', match(flowchart.nostart$from, flowchart$label), '\'', sep = '')
    to.edges <- paste('\'@@2-', 1:nrow(flowchart.nostart), '\'', sep = '')
    edge.statement <- paste(from.edges, to.edges, sep = '->', collapse = ';')
  } else {
    from.edges <- paste('\'@@1-', 1:nrow(flowchart), '\'', sep = '')
    edge.statement <- paste(from.edges, sep = '', collapse = ';')
  }
  
  from.nodes <- as.character(flowchart$title)
  from.nodes[flowchart$display.n] <- paste(from.nodes[flowchart$display.n],
                                           '\n',
                                           flowchart$n[flowchart$display.n],
                                           ' patients',
                                           sep = '')
  from.nodes[flowchart$display.r] <- paste(from.nodes[flowchart$display.r],
                                           '\n',
                                           flowchart$r[flowchart$display.r],
                                           ' records',
                                           sep = '')
  if(to.nodes.present){
    to.nodes <- as.character(flowchart.nostart$title)
    to.nodes[flowchart.nostart$display.n] <- paste(to.nodes[flowchart.nostart$display.n],
                                                   '\n',
                                                   flowchart.nostart$n[flowchart.nostart$display.n],
                                                   ' patients',
                                                   sep = '')
    to.nodes[flowchart.nostart$display.r] <- paste(to.nodes[flowchart.nostart$display.r],
                                                   '\n',
                                                   flowchart.nostart$r[flowchart.nostart$display.r],
                                                   ' records',
                                                   sep = '')
  } else {
    to.nodes <- character(length = 0)
  }
  node.statement <- paste(paste('\'@@1-', 1:length(from.nodes), '\'', sep = ''), collapse = ';')
  diagram <- paste(preamble, node.statement, edge.statement, '}',
                   paste('[1]: c(', paste(from.nodes, collapse = ','), ')', sep = '', collapse = ''),
                   paste('[2]: c(', paste(to.nodes, collapse = ','), ')', sep = '', collapse = ''),
                   sep = '\n')
  result <- list(diagram = diagram, from.nodes = from.nodes, to.nodes = to.nodes)
  return(result)
}
