# Auxiliary functions
#
#  Extraxting p-values from several test functions. For small contingency tables (binary variable),
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
ctp.lin.hyp <- function(CTPparms, Lhyp.mat)
{
  
  obj <- CTPparms$lm.obj
  factor.name <- CTPparms$facname
  
  
  Coefficients <- obj$coefficients
  
  oo	<- attr(obj$term,"term.lab")
  
  #print(oo)
  
  Loo	<- length(oo)
  Noo	<- (1:Loo)[oo==factor.name]
  subscripts <- obj$assign==Noo
  f.levels <- obj$xlevels		
  
  #print(subscripts) #$$$
  #print(f.levels);print(Coefficients)	#$$$
  
  Coeff 	<- Coefficients[subscripts]
  
  # cat("\n\nHallo Paule! -- 8\n")
  # print(obj$contr)
  
  CC	 	<- obj$contr[[factor.name]]
  
  ## print(Con)
  #cn		<- length(obj$xlevels$G)
  #cn		<- length(obj$xlevels[[factor.name]])
  #CC		<- eval(call(Con,cn))
  ##print(Coeff);print(CC)
  est <- CC %*% Coeff
  #print(est)
  lhyp <- t(Lhyp.mat) %*% est
  #print(lhyp)
  #########################################
  # Covariance Matrix of linear Hpothesis #
  #########################################

    
    Subs <- unlist(subscripts)
    obj.sum <- summary(obj)
    sR <- obj.sum$sigma
    DF <- obj$df.residual
    xcov <- obj.sum$cov.unscaled
    acov <- xcov[Subs, Subs] * sR * sR
    ycov <- CC %*% acov %*% t(CC)
    #print(ginverse(t(Lhyp.mat) %*% ycov %*% Lhyp.mat))
    fc <- t(lhyp) %*% ginv(t(Lhyp.mat) %*% ycov %*% Lhyp.mat) %*% lhyp
    stdErr <- sqrt(t(Lhyp.mat) %*% ycov %*% Lhyp.mat)
    dfc <- ncol(Lhyp.mat)
    fc <- fc/dfc
    cpvalue <- 1. - pf(fc, dfc, DF)
    #result <- list(call = obj$call, factor.names = factor.name, factor.levels = f.levels, Lin.hyp = Lhyp.mat, stdErr = stdErr, dfc = dfc, DF = DF, estimates = lhyp, F = fc, p.value = cpvalue)
    #oldClass(result) <- "Lin.hyp"
    ## print(result)
    #result
    cpvalue
}