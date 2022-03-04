#' longOrderAvg
#'
#' Determine average over all six longOrder values for every person.
#'
#' @param m.lcm matrix with long combinations (from longOrder())
#'
#' @export
#'
longOrderAvg <- function(m.lcm)
{
  #m.lcm - matrix of longOrders of fb

  #1#calculate avg for each person
  avg <- rowMeans(m.lcm[,2:7],na.rm = T)
  return(avg)
}
