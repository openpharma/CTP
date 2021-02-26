# Auxiliary functions
#
# Extraxting p-values from several test functions. For small contingency tables (binary variable),
# Fisher's exact test is applied for larger tables a chisquare test.
#
#
#
#

ctp.chisq   <- function(resp, fac) chisq.test(x=resp, y=fac)$p.value

ctp.kruskal <- function(resp, fac,...) kruskal.test(resp~fac,...)$p.value


ctp.jonckheere <-function(resp, fac,nperm=5000,...)
	{
  #oldoptions <- options()
  #options(warn = -1)
		xfac	<- ordered(fac)
		pval <- jonckheere.test(resp, xfac,...)$p.value
		#options(oldoptions)
	pval
	}

ctp.lgrank <- function(resp, fac)
	{
		survd <- survdiff(resp ~ fac)
		1. - pchisq(survd$chisq[1.], (length(survd$obs) - 1.))
}



ctp.prob <-
	function(resp, fac,...)
	{
		if(length(resp) < 200.) fisher.test(resp, fac)$p.value else chisq.test(resp, fac,...)$p.value
	}

# Linear hypotheses (F-Test)
ctp.linHyp <- function(CTPparms)
{
  Mod    <- CTPparms$model
  environment(Mod) <- environment() # scope in df and not where model was created
  Fac    <- CTPparms$facname
  
  df <- CTPparms$data
  lm1 <- lm(formula = Mod, data = df)
  
  em_lm1 <- emmeans(lm1, Fac)
  struc  <- CTPparms$hyplist
  CC     <- mkContrasts(struc)
  p1 <- c()

  for(i in 1:length(CC)) p1[i] <- test(contrast(em_lm1,list(c=CC[i])),joint=TRUE)$p.value

  pvalues <- data.frame(CTPparms$hypnames, pvalue = p1)
  pvalues
}
