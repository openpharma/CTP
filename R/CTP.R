#' CTP, a package for closed testing procedures
#' 
#' 	Library of functions to design and analyze closed testing procedures for
#' 	the comparison of population parameters based on independent samples.
#' 	
#' @docType package
#' @name CTP
#' @details 
#' 	Library of functions to design and analyze closed testing procedures for
#' 	the comparison of population parameters based on independent samples. The function \code{\link{IntersectHypotheses}}
#' 	creates the hypothesis tree (closure set) of a closed testing procedure. All possible intersecting hypotheses derived from
#' 	the list of elementary hypotheses(i.e. the hypotheses to be tested) are produced. The resultant hypothesis tree
#' 	will consist of the elementary hypotheses as well as all subsequent intersected hypotheses. The procedure ends when
#' 	one hypothesis (the global hypothesis) remains. In this way, for each elementary hypothesis all hypotheses implying
#' 	it can be found.\cr\cr
#' 	The analysis is performed using \code{\link{AnalyseCTP}}. First the raw p-values are computed for all hypotheses of the
#' 	hypothesis tree, then these are adjusted according to the closure principle i.e. the adjusted p-value is
#' 	calculated as the maximum of the raw p-value of the hypothesis in question and of the raw p-values of all
#' 	hypotheses implying it.\cr\cr
#' 	
#' 	Instead of applying \code{\link{AnalyseCTP}}, the raw p-values can be computed
#' 	using any other available software. For this purpose, the functions \code{\link{Adjust_raw}} is provided. \cr \cr and
#' 	\code{\link{summary.ctp.str}} generates a data frame comprising all the hypothesis of the hypothesis tree.
#' 	The function \code{\link{Adjust_raw}} calculates the p-values for the given hypothesis tree from the raw p-values
#' 	provided in the same order as the hypotheses occur in the data frame created by \code{\link{summary.ctp.str}}. \cr\cr
#' 	
#' 	The results are presented using the generic functions \code{summary} and \code{Display}.
#' 	
#' 	
#' @author  	J. Bock & P.Jordan
#' 
#' @importFrom MASS ginv
#' @importFrom shape Arrows
#' @importFrom survival survdiff Surv
#' @importFrom dplyr select left_join group_by arrange rename mutate summarise desc n ungroup bind_cols
#' @import magrittr 
#' @import stats
#' @import clinfun
#' @import diagram
#' @import graphics
#' @import Gmisc
#' @import grid
NULL
