
#'	Intersection of hypotheses
#'
#' Generation of the hypotheses tree of a closed testing procedure (CTP). The function returns an object of \code{oldClass "ctp.str"};
#'	\code{summary} and \code{Display} can be applied. 
#'
#'
#'	@param hyplst A list of integer vectors representing the elementary hypotheses. 
#'
#' @details
#'	Sets of elementary hypotheses are described by lists of integer vectors eg. \code{list(1:2, c(1,3), c(1,4))}
#'	if the populations 2, 3 and 4 have to be compared to population 1.
#'	For the generation of the hypothesis tree of a closed testing procedure first all intersections of the elementary hypothese;
#'	then all intersections of these intersections etc. have to be created. The set of hypotheses at each intersection level must be
#'	reduced by deleting double hypotheses and creating unions. The size of the hypothesis tree increases rapidly with growing number of elementary hypotheses,
#'	which can lead to memory and/or time problems!
#'		The intersection procedure ends if a single hypothesis (the global hypothesis) is left.
#'
#' @return
#'  \itemize{
#'	\item{\code{hypothesis}}{ The hypothesis tree described as lists hypotheses at each intersection level.}
#'	\item{\code{relations}}{ The relation structure of the hypotheses tree used for plotting.}
#'	\item{\code{hypnames}}{ Character representations of the hypotheses used for summary tables and plotting.}
#'	\item{\code{connections}}{ The connections used for calculating the adjusted p-values.}
#' }
#' 
#' @note
#'	This procedure is constructed for null-hypotheses describing the equality of the same parameter for different populations. 
#'  \cr
#'  \cr
#' \emph{Warning}:
#'	The size of the hypothesis tree increases rapidly with growing number of elementary hypotheses. This can lead to memory and/or time problems! 
#'
#' @seealso
#'	\code{\link{AnalyseCTP}} \code{\link{Adjust_raw}}
#'
#' @examples
#' 
#'	hlist  <- list(1:2, c(1,3), c(1,4))
#'	htree  <- IntersectHypotheses(hlist)
#'	summary(htree)
#'	Display(htree)
#'	
#'	
#'	# compare in a set of six means all others to the fourth mean
#'	#
#'	five.to.fourth <- IntersectHypotheses(list(c(1,4),c(2,4),c(3,4),c(4,5),c(4,6)))
#'	Display(five.to.fourth)
#'	
#'	@export	
	IntersectHypotheses <- function(hyplst)
{
	#generates the hypothesis tree and relationsships
	if(!(mode(hyplst) == "list")) stop("Argument must be a list")
	if(length(hyplst) == 1.)
		stop("Number of hypotheses must be > 1")
	if(min(sapply(hyplst, length)) < 2.)
		stop("Hypothesis of length<2 detected.")
	ab <- unlist(lapply(hyplst, function(lst)
	lapply(lst, mode)))
	if(!all(ab == "numeric"))
		stop("Invalid hypothesis included.")
	nonint <- any(floor(unlist(hyplst)) - unlist(hyplst) < 0.)
	neg <- any(unlist(hyplst) < 0.)
	if(nonint | neg)
		stop("Only positive integers allowed")
	un <- ctp.unique(hyplst)
	if(un > 0.)
		stop("Duplicated hypotheses are not allowed.")
	hyplst <- lapply(hyplst, ctp.sort.hyp)
	hyptree <- vector("list", 50.)
	relations <- vector("list", 50.)
	hyptree[[1.]] <- hyplst
	len <- length(hyplst)
	zza <- 1.
	##generate character-representation
	while(len > 1. && zza < 1000.) {
		nlst <- ctp.intersect.hyplst(hyptree[[zza]])
		zza <- zza + 1.
		hyptree[[zza]] <- nlst[[1.]]
		relations[[zza]] <- nlst[[2.]]
		len <- length(nlst[[1.]])
	}
	chreprlst <- lapply(hyptree[1.:zza], FUN = function(x)
	lapply(x, ctp.chm))
	chaa <- sapply(hyptree[1.:zza], FUN = function(x)
	1.:length(x))
	chbb <- rep(1.:length(chaa), sapply(chaa, length))
	chab <- cbind(hyp.no = unlist(chaa), level = chbb)
	##generate connections (hypotheses connecting line segments)
	chrepr <- data.frame(chab, hypothesis.name = unlist(chreprlst))
	xx <- relations[1.:zza]
	xlen <- length(xx)
	yy <- vector("list", xlen)
	for(i in 2.:xlen) {
		rell <- xx[[i]]
		ha <- data.frame(c(rell[, 1.], rell[, 2.]), rep(rell[, 3.], 2.))
		hlen <- length(ha[, 1.])
		ha$levold <- rep(i - 1., hlen)
		hb <- aggregate(ha$levold, list(hypold = ha[, 1.], hypnew = ha[, 2.]), mean)
		names(hb)[3.] <- "levold"
		hb[, 1.] <- as.numeric(as.character(hb[, 1.]))
		hb[, 2.] <- as.numeric(as.character(hb[, 2.]))
		hb$levnew <- hb$levold + 1.
		yy[[i]] <- hb
	}
	ctplst <- list(hypothesis = hyptree[1.:zza], relations = relations[1.:zza], hypnames = chrepr, connections = yy[-1.])
	oldClass(ctplst) <- "ctp.str"	
	ctplst

}
