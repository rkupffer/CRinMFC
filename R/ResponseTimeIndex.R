#' Response Time Index
#'
#'
#' To calculate the RTI, first, the coded response times C_{pt} are obtained from the response time of
#' each participant p on every triplet page t of the questionnaire.
#' Second, the RTI for each participant is determined as:
#' RTI_p=\frac{\sum_{t=1}^{T}C_{pt}}{k}
#' where k is the number of values of C_{pt} without the ones coded as missing (NA)
#' and T is the number of triplets in the questionnaire.
#'
#'
#' @param df.time.vars data frame with time measures per page or triplet
#' @param t_upper upper threshold
#' @param t_lower lower threshold
#'
#' @return data frame with binary coded data
#' @export
#'
#'

responseTimeIndex <- function(df.time.vars, t_upper, t_lower){
  m.C <- matrix(0, nrow = nrow(df.time.vars), ncol = ncol(df.time.vars))
  m.C[df.time.vars >= t_upper] <- NA
  m.C[df.time.vars < t_lower] <- 1
  m.C[is.na(df.time.vars)] <- NA

  # means of the coded time variables:
  # divided by k (only the ones without missing values)
  RTI.mean <- rowMeans(m.C, na.rm = TRUE)
  RTI <- ifelse(is.nan(RTI.mean), NA, RTI.mean)
}





#end
