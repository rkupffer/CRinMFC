#' longOrder
#'
#' Determine longest rank order for every observation and each item order (e.g., 123, 321, ...).
#'
#' @param IDs vector with ID variables
#' @param d.b.o data frame with ordered rank orders
#' @param no.b number of blocks
#'
#' @return matrix rows = observations, columns = rank orders,
#' cells = counts how often a rank order (col) was repeated by the participant (row)
#' @export
#'
#'
longOrder <- function(IDs, d.b.o, no.b)
{
  #1#longOrder with starting value 0
  lc <- rep(0,length(IDs))

  #2#Matrix for all LongCombis with ID-Variables
  lcm.out <- data.frame(matrix(0,length(IDs),7))
  colnames(lcm.out) <- c("ID","213", "132", "321", "231", "123", "312")
  lcm.out$ID <- IDs

  #3#count combis
  for (b in 1:(no.b-1)) {
    for(p in 1:length(IDs)) {
      pat <- d.b.o[p,b+1]
      if(is.na(pat) | is.na(d.b.o[p,b])) {
        lc[p] <- 0
      }
      else if(pat == d.b.o[p,b]) {
        lc[p] <- lc[p] + 1
        if(lc[p] > lcm.out[p,pat]) lcm.out[p,pat] <- lc[p]
      } else {
        lc[p] <- 0
      }
    }
  }
  colnames(lcm.out) <- c("ID","c213", "c132", "c321", "c231", "c123", "c312")
  return(lcm.out)
}
