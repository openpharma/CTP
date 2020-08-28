#' Data from a Psoriasis Study
#'
#' The dataframe \code{pasi} comprises the changes in PASI-score (Psoirasis Area and Severity Index)
#' from Baseline within two months of 72 patients treated with different
#' doses of Etretin or Placebo in a double blind study.
#' 
#' @format A data frame with 72 observations and 3 variables:
#' \describe{
#'     \item{treatment}{a factor with levels \code{Etretin10mg} \code{Etretin25mg} \code{Etretin50mg} \code{Placebo}}
#'     \item{pasi.ch}{Changes in PASI score within two months}
#'     \item{dose}{Dose of Etretin as ordered factor with levels
#'           \code{Placebo} < \code{ET.10mg} < \code{ET.25mg} < \code{ET.50mg}}
#'     }
#' @usage data(pasi)     
"pasi"

#' Data from a Study in Diabetes II patients
#'
#' The dataframe \code{glucose} comprises the baseline values of fasting plasma glucose (mmol/L)
#' and their changes from baseline to the end of the study. 
#' 
#'	@format A data frame with 89 observations and 3 variables:
#'	\describe{
#'		\item{\code{GLUCOSE.BLA}}{Fasting plasma glucose at baseline}
#'		\item{\code{GLUCOSE.CHANGE}}{Changes in fasting plasma glucose from baseline}
#'		\item{\code{DOSE}}{An ordered factor with levels \code{PLACEBO} < \code{LOW} < \code{MEDIUM} < \code{HIGH}}
#'	}
#'	@usage data(glucose)
"glucose"

#' Data from a Study in Colorectal Cancer
#' 
#' A dataframe containing the responders to the treatment with two doses of
#' an experimental drug or a standard treatment. 
#' 
#' @format A data frame with 104 observations and 2 variables:
#' 	\describe{
#' 		 \item{\code{dose}}{a factor with levels \code{control} \code{low} \code{high}}
#' 		 \item{\code{responder}}{a factor with levels \code{no} \code{yes}}
#' 	}
#' @usage data(colorectal)
"colorectal"