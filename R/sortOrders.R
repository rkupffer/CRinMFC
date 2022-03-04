#' sortOrders
#'
#' sort the rank orders of the raw answers to the randomized item blocks
#'
#' @param no.b number of blocks in the questionnaire of interest
#' @param no.m number of items per block (for triplets no.m = 3)
#' @param d.r data frame with raw data (unrecoded answers to triplets)
#' @param d.k data frame with key variables (order of triplets)
#'
#' @return  data frame with rank orders of all blocks, rows = persons, columns = blocks
#' @export
#'
#' @examples bl.com <- sortOrders(no.b = 20, no.m = 3, d.r = rawdata, d.k = keydata)
sortOrders <- function(no.b, no.m, d.r, d.k)
{
  #1#create block indices for the number of blocks and items per block
  blocks <- matrix(seq_len(no.b*no.m), no.m, no.b)

  #2#combine all blocks (triplets) to combinations
  dat.b <- apply(blocks, 2, function(bn, d.r) apply(d.r[,bn], 1, paste, collapse=""), d.r)
  dat.b[grep("NA",dat.b)] <- NA #recode patterns including NA (e.g.1NANA) as NA

  #3#order the blocks by their keys
  df.out <- do.call(rbind, lapply(1:nrow(dat.b), function(ind, db, ke) db[ind,order(ke[ind,])], db=dat.b, ke=as.matrix(d.k)))
  return(df.out)
}
