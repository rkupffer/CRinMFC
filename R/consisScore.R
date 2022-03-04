#' Consistency Score
#'
#' Ratio of the frequency of items with a probability higher than chance level
#' of being preferred to the number of items (Brown & Bartram, 2011).
#'
#' Function depends on: load2mat(), designMatrix(), designLoadings().
#'
#' @param quest.pars TIRT parameters of the questionnaire (e.g., Mplus output)
#' @param no.b number of blocks in the questionnaire
#' @param no.traits number of traits measured by the questionnaire
#' @param all.items item coding table (item-trait correspondence, coding of items, ...)
#' @param quest which questionnaire "BT"/"SD"/"BI"/"OS"... (name in all.items)
#' @param m.theta thetas (estimates on traits)
#' @param d.bi data frame with binary outcomes (same length as m.fscores, hence sometimes a subsample)
#'
#' @return consistency score: vector (length = observations)
#' @export
#'
consisScore <- function(quest.pars, no.b, no.traits, all.items, quest, m.theta, d.bi)
{
  no.i <- no.b*3

  #1# Coding per design
  #design loadings (60x5 matrix)
  m.design.load <- designLoadings(all.items, quest, no.traits)

  #design matrix (60x60 matrix)
  m.design <- designMatrix(no.b)

  #2# Parameters from Mplus
  #pair thresholds (vector with 60 elements)
  v.thresh <- quest.pars[quest.pars$paramHeader=="Thresholds",]$est

  #loadings (60x5 matrix)
  m.load <- TirtAutomation::load2mat(quest.pars, m.design.load, m.design)

  #uniqenesses (vector with 60 elements)
  v.uni <- quest.pars[quest.pars$paramHeader=="Residual.Variances",]$est

  #3# Prepare for matrix algebra
  #3.1# combine both loadings of a pair-wise comparison in one row (60x5 matrix)
  # and change the sign of the element which comes second in the pairwise comparison
  m.loadcomp <- m.design %*% m.load

  #4# calculate probabilities
  #in case the person was not preferring the item in the pair-wise comparison the cs formula
  #begins with 1-...
  prob <- ifelse(is.na(d.bi)==TRUE, NA,
                 ifelse(d.bi==1, pnorm(t((- v.thresh + tcrossprod(m.loadcomp, as.matrix(m.theta)))/sqrt(as.vector(v.uni)))),
                        1 - pnorm(t((- v.thresh + tcrossprod(m.loadcomp, as.matrix(m.theta)))/sqrt(as.vector(v.uni))))))

  #Consistency Score: how many of the probabilities are above 0.5?
  out <- rowSums(((prob>.5)/no.i))

  return(out)
}
