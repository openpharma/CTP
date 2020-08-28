#' Adjusting p-values, auxiliary function
#'
#' @param ctp.struc Object of class \code{ctp.str}.
#' @param ctp.pval Data frame with hypothesis names and unadjusted p-values. Output from \code{CTPcompare}. 
#'
#' @return Adjusted p-values
#' @export
Adjust_p <-function(ctp.struc, ctp.pval)
{
	hypl <- ctp.struc[[1]]
	pval <- ctp.pval
	pval$pvalue.adj <- pval[, "pvalue"]
	maxlev <- max(pval[, "level"])
	for(i in (maxlev - 1):1) {
		hypli <- hypl[[i]]
		leni <- length(hypli)
		pvip <- pval[pval["level"] == i, "pvalue"]
		pvia <- pvip
		txt <- as.character(pval[pval["level"] == (i + 1), "hypothesis.name"])
		ppp <- pval[pval["level"] == (i + 1), "pvalue.adj"]
		for(j in 1:leni) 
		{
		hypj <- as.character(unlist(hypli[[j]]))
			lenj <- length(hypj)
		indj <- grep(hypj[1], txt)
			for(k in 2:lenj) {
				indk <- grep(hypj[k], txt)
				indj <- intersect(indj, indk)
			}
			pvia[j] <- max(c(pvip[j], ppp[indj]))
		}
		pval[pval["level"] == i, "pvalue.adj"] <- pvia
	}
	pval
}
