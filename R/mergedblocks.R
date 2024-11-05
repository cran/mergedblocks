#' Merged block randomization
#'
#' @name mergedblocks
#'
#' @description Function to carry out merged block randomization, a restricted randomization
#' method designed for small clinical trials (at most 100 subjects) or trials
#' with small strata, for example in multicentre trials. It can be used for
#' more than two groups or unequal randomization ratios.
#'
#' @param n The number of subjects to randomize.
#' @param ratio The desired randomization ratio, given as a vector. Default is 1:1
#' randomization, but more groups or unequal ratios are possible as well. E.g. for
#' 1:1:2 randomization, use c(1, 1, 2).
#' @param labels The labels for the assignments, given as a vector, e.g. c("treatment", "placebo"). The
#' length of \code{labels} should match the length of \code{ratio}. Default is to use numeric labels.
#' @return Allocation of the subjects, given as a vector.
#'
#' @references S.L. van der Pas (2019). Merged block randomisation: a novel randomisation procedure for small clinical trials.
#' Clinical Trials 16(3):246-252.
#'
#' @seealso \code{\link{mergedblocksmulti}} to create allocations for multiple strata at the same time.
#'
#' @examples
#'#Randomize 20 patients, 1:1 allocation, with labels "1" and "2".
#' mergedblocks(20)
#'
#'#Randomize 50 patients, 1:1 allocation with labels "treatment" and "placebo".
#' mergedblocks(100, labels = c("treatment", "placebo"))
#'
#'#Randomize 100 patients, 1:2:2 allocation with labels "placebo",
#'#"treatment 1", and "treatment 2".
#'mergedblocks(100, c(1, 2, 2), c("placebo", "treatment 1", "treatment 2"))
#'
#'@importFrom randomizeR getRandList
#'@importFrom randomizeR genSeq
#'@importFrom randomizeR pbrPar
#'
#' @export


mergedblocks <- function(n, ratio = c(1, 1), labels = as.character(1:length(ratio))){
  length.blocks <- sum(ratio)
  nr.basis.blocks <- ceiling(n/length.blocks)

  basis1 <- getRandList( genSeq( pbrPar(rep(length.blocks, ceiling(n/length.blocks)), K = length(ratio), ratio = ratio, groups = labels) ) )  [1:n]

  basis2 <- getRandList( genSeq( pbrPar(rep(length.blocks, ceiling(n/length.blocks)), K = length(ratio), ratio = ratio, groups = labels) ) )  [1:n]

  #now merge
  res <- rep(0, n)
  row1or2 <- sample(c(1, 2), size = n, prob = c(1/2, 1/2), replace = T)

  taken.from.2 <- 0

  for(i in 1:n){

    if(row1or2[i] == 1){res[i] <- basis1[i - taken.from.2]}

    if(row1or2[i] == 2){
      taken.from.2 <- (taken.from.2 + 1)
      res[i] <- basis2[taken.from.2]
    }
  }
  return(res)
}

