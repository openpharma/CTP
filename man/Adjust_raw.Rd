% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Adjust_raw.R
\name{Adjust_raw}
\alias{Adjust_raw}
\title{Adjusting raw p-values of a CTP}
\usage{
Adjust_raw(
  ctp.struc,
  p.value,
  dataset.name = NULL,
  factor.name = NULL,
  factor.levels = NULL,
  model = NULL,
  test.name = NULL
)
}
\arguments{
\item{ctp.struc}{Object generated by \code{\link{IntersectHypotheses}}}

\item{p.value}{Vector of raw p-values in the order of the hypotheses created by \code{\link{summary.ctp.str}}}

\item{dataset.name}{Character string naming the analysis dataset (optional - only for documentation purposes).}

\item{factor.name}{Character string naming the factor whose levels are compared (optional - only for documentation purposes).}

\item{factor.levels}{Vector of type "character" containing the levels of the treatment factor
(optional - only for documentation purposes).}

\item{model}{Model used in the analysis (optional - only for documentation purposes).}

\item{test.name}{Character string naming the statistical test applied.}
}
\value{
An object of \code{oldClass = "ctp"} to be used for summarizing and plotting the results.
}
\description{
Function that adjusts the raw p-values of the elementary hypotheses of a closed testing procedure.
The raw p-values are adjusted according to the closure principle.
The adjusted p-value is calculated as the maximum of the raw p-value from the current hypothesis in question and the raw p-values from
all subsequent hypotheses that contain the current hypothesis.
}
\examples{

Pairwise <- IntersectHypotheses(list(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4)))
Display(Pairwise)
summary(Pairwise)

# the vector of p-values calculated by another software

  p.val <- c(
            0.4374,
            0.6485,
            0.4103,
            0.2203,
            0.1302,
            0.6725,
            0.4704,
            0.3173,
            0.6762,
            0.7112,
            0.2866,
            0.3362,
            0.2871,
            0.4633)

 result <- Adjust_raw(ctp.struc=Pairwise, p.value=p.val)

 ## details may be documented

 result <- Adjust_raw(Pairwise, p.value=p.val
          ,dataset.name="my Data", factor.name="Factor"
          ,factor.levels=c("A","B","C","D"), model=y~Factor
          ,test.name="my Test")

 summary(result)
 Display(result)


}
\seealso{
\code{\link{IntersectHypotheses}}, \code{\link{AnalyseCTP}}, \code{\link{Display}},
\code{\link{summary.ctp}}
}
