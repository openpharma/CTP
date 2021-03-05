# Auxiliary functions
#
# Extraxting p-values from several test functions. For small contingency tables (binary variable),
# Fisher's exact test is applied for larger tables a chisquare test.
#
#

ctp.chisq   <- function(resp, fac) chisq.test(x=resp, y=fac)$p.value

ctp.kruskal <- function(resp, fac,...) kruskal.test(resp~fac,...)$p.value


ctp.jonckheere <-function(resp, fac,nperm=5000,...)
	{
		xfac	<- ordered(fac)
		pval <- jonckheere.test(resp, xfac,...)$p.value
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

#######################################
### (Generalized) Linear hypotheses ###
#######################################

ctp.linHyp <- function(CTPparms,family="gaussian")
{
  Mod    <- CTPparms$model
  environment(Mod) <- environment() # scope in df and not where model was created
  Fac    <- CTPparms$facname
  
  
  if(family=="gaussian") Normal <- TRUE
     else Normal <- FALSE
       
  if (is.character(family)) 
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  
  df  <- CTPparms$data
  
  if(Normal) obj <- lm(formula = Mod, data = df)
   else      obj <- glm(formula = Mod, data = df,family=family)
  
  em_obj <- emmeans(obj, Fac)
  struc  <- CTPparms$hyplist
  CC     <- mkContrasts(struc)
  p1     <- c()

  for(i in 1:length(CC)) p1[i] <- test(contrast(em_obj,list(c=CC[i])),joint=TRUE)$p.value

  pvalues <- data.frame(CTPparms$hypnames, pvalue = p1)
  pvalues
}
