#' sameOrder
#'
#' Determine proportion of triplets for which a person copied the presented order.
#'
#' @param d.b.o data frame with ordered combinations
#' @param no.b number of blocks
#'
#' @export
#'
sameOrder <- function(d.b.o, no.b){
  out <- rowSums(d.b.o == "123", na.rm = TRUE)/no.b
  }
