
#' Auxiliary function
#' 
#' Extracting CTP - parameters 
#'
#' @param ctp.struc Object of class \code{ctp.str}.
#' @param model Formula of the form response~treatment.
#' @param dataset Dataframe. 
#' @param factor.name 
#'    Character string naming the factor whose levels are compared (treatment factor).
#'		By default the first variable of the right-hand side of the model formula is used.
#' @param test.name
#'		One of the following strings \itemize{
#'			\item \code{"F"} - F-Test (ANOVA, default)
#'			\item \code{"kruskal"} -Kruskal-Wallis-Test
#'			\item \code{"chisq"} - Chi square test
#'			\item \code{"prob"} - Fisher's exact test for total number of observations <200 else Chi square test
#'			\item \code{"lgrank"} - Logrank-test 
#'			\item \code{"jonckheere"} - Jonckheere-Terpstra test of ordered alternatives
#'		}

#'
#' @return A list with CTP parameters
#'

getCTPparms <- function(ctp.struc, model, dataset, factor.name = NULL, test.name = "F")
{
  hyplist       <- ctp.struc$hypothesis
	hh.test       <- paste("ctp.", test.name, sep = "")
	hypnames      <- ctp.struc$hypnames
  connections   <- ctp.struc$connections
	hh.model      <- model
	hh.dataset    <- data.frame(lapply(dataset, ctp.change.contr))
	hh.respname   <- names(attributes(terms(model))$factors[, 1])[1]
	 if(is.null(factor.name))
	  	hh.facname 		  <- attributes(terms(model))$term.labels[1]
	  else hh.facname 	<- factor.name
	  hh.fac			      <- hh.dataset[, hh.facname]
	  hh.level      <- levels(hh.fac)
	  hh.nlevel     <- length(hh.level)
	  hh.resp       <- getResponse(model,dataset)

	 if(hh.test == "ctp.F")  hh.lm.obj <- lm(formula = eval(model), data = hh.dataset)
	   else hh.lm.obj <- NA

	    Parms <- list(hyplist=hyplist,hypnames=hypnames,connections=connections
	                  ,model=model,lm.obj=hh.lm.obj,data=hh.dataset,test=hh.test,fac=hh.fac,facname=hh.facname
	    							,level=hh.level,nlevel=hh.nlevel,resp=hh.resp,respname=hh.respname)
	    oldClass(Parms) <- "CTPparms"
	    Parms
}

