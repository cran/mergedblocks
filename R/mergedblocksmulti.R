#' Merged block randomization for multiple strata
#'
#' @name mergedblocksmulti
#'
#' @description Function to carry out merged block randomization, for multiple strata.
#'
#' @param K The number of strata.
#' @param n The number of subjects to randomize. May be given as a single number, for the same number
#' of subjects per stratum, or as a vector or length \code{K} in case the desired
#' sample size varies per stratum
#' @param ratio The desired randomization ratio, given as a vector. Default is 1:1
#' randomization, but more groups or unequal ratios are possible as well. E.g. for
#' 1:1:2 randomization, use c(1, 1, 2).
#' @param labels The labels for the assignments, given as a vector, e.g. c("treatment", "placebo"). The
#' length of \code{labels} should match the length of \code{ratio}. Default is to use numeric labels.
#' @return Allocation of the subjects, given as a dataframe, with one column per stratum. Padded with NAs in case of different sample sizes per stratum.
#'
#' @references S.L. van der Pas (2019). Merged block randomisation: a novel randomisation procedure for small clinical trials.
#' Clinical Trials 16(3):246-252.
#'
#' @seealso \code{\link{mergedblocks}} for a version for a single stratum.
#'
#'
#' @examples
#'#Four strata, randomize 20 patients for each stratum, 1:1 allocation,
#'#with labels "0" and "1".
#' mergedblocksmulti(K = 4, n = 20)
#'
#'#Three strata, randomize 30, 40 and 50 patients for each stratum,
#'#1:2 allocation, with labels "placebo" and "treatment".
#' mergedblocksmulti(K = 3, n = c(30, 40, 50), ratio = c(1, 2), labels = c("placebo", "treatment"))
#'
#' @export


mergedblocksmulti <- function(K, n, ratio = c(1, 1), labels = as.character(1:length(ratio))){
  if(length(n) == 1){n.vec <- rep(n, K)}
  if(length(n) > 1){n.vec <- n}

  res <- matrix(data = NA, nrow = max(n.vec), ncol = K)

  for(k in 1:K){
    n.strat <- n.vec[k]
    res[1:n.strat, k] <- mergedblocks(n.strat, ratio, labels)
  }

  return(as.data.frame(res))
}

