% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergedblocksmulti.R
\name{mergedblocksmulti}
\alias{mergedblocksmulti}
\title{Merged block randomization for multiple strata}
\usage{
mergedblocksmulti(
  K,
  n,
  ratio = c(1, 1),
  labels = as.character(1:length(ratio))
)
}
\arguments{
\item{K}{The number of strata.}

\item{n}{The number of subjects to randomize. May be given as a single number, for the same number
of subjects per stratum, or as a vector or length \code{K} in case the desired
sample size varies per stratum}

\item{ratio}{The desired randomization ratio, given as a vector. Default is 1:1
randomization, but more groups or unequal ratios are possible as well. E.g. for
1:1:2 randomization, use c(1, 1, 2).}

\item{labels}{The labels for the assignments, given as a vector, e.g. c("treatment", "placebo"). The
length of \code{labels} should match the length of \code{ratio}. Default is to use numeric labels.}
}
\value{
Allocation of the subjects, given as a dataframe, with one column per stratum. Padded with NAs in case of different sample sizes per stratum.
}
\description{
Function to carry out merged block randomization, for multiple strata.
}
\examples{
#Four strata, randomize 20 patients for each stratum, 1:1 allocation,
#with labels "0" and "1".
mergedblocksmulti(K = 4, n = 20)

#Three strata, randomize 30, 40 and 50 patients for each stratum,
#1:2 allocation, with labels "placebo" and "treatment".
mergedblocksmulti(K = 3, n = c(30, 40, 50), ratio = c(1, 2), labels = c("placebo", "treatment"))

}
\references{
S.L. van der Pas (2019). Merged block randomisation: a novel randomisation procedure for small clinical trials.
Clinical Trials 16(3):246-252.
}
\seealso{
\code{\link{mergedblocks}} for a version for a single stratum.
}
