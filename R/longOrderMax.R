#' longOrderMax
#'
#'
#' Determine longest rank order for every observation.
#' Which rank order (e.g., 123, 321, ...) was repeated most often?
#'
#' @param m.lcm matrix with long rank orders (from longOrder())
#'
#' @export
#'
longOrderMax <- function(m.lcm)
{
  #m.lcm - matrix of longOrder of fb

  #1#calculate max for each person
  maxi <- apply(m.lcm[,2:7],1,max)
  return(maxi)
}
