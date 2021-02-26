#' Adjusting p-values, auxiliary function
#'
#' @param ctp.struc Object of class \code{ctp.str}.
#' @param ctp.pval Data frame with hypothesis names and unadjusted p-values. Output from \code{CTPcompare}.
#'
#' @return Adjusted p-values
#' @export
Adjust_p <-function(ctp.struc,ctp.pval)
{
  Hyp  <- ctp.struc$hypnames
  nHyp <- length(Hyp$hypothesis.name)
  p_adj <- c()
  for (i in 1:nHyp)
  {
    hyp <- as.character(Hyp$hypothesis.name[i])
     if(Hyp$level[i]==1)
     {
       TS <- TestingSet(ctp.struc,hyp)
       YYY <- subset(ctp.pval,hypothesis.name %in% TS)
       # print(TS)
       # print(YYY)
       p_adj[i] <- max(YYY$pvalue)
     }else p_adj[i] <- NA

  }
 ctp.pval %>% mutate(pvalue.adj=p_adj)
}
