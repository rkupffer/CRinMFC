#' tripletVariance
#'
#' Variance of a person’s ranking patterns across all triplets.
#' (Relativer Informationsgehalt; Eid,  Gollwitzer, & Schmitt, 2015)
#' H = -(1/ln(k)*sum(h_j *ln(h_j)))
#' k = number of possible combinations (in case of triplets 6)
#' h_j = proportion
#'
#' @param d.b.o data frame with ordered rank orders (resulting from sortOrders())
#'
#' @return vector (length = observations) with triplet variance for each participant
#' @export
#'
tripletVariance <- function(d.b.o)
{
  out <- apply(d.b.o,1, function(d.b.o) Hcat <-  -1/log(6) * sum((table(d.b.o)/length(complete.cases(d.b.o)))*log(table(d.b.o)/length(complete.cases(d.b.o)))))
}
