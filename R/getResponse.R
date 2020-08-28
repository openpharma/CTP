

#' Auxiliary function
#' 
#' Getting the response vector out of an object of class 'formula'
#'
#' @param formula Model formula
#' @param data Dataframe 
#'
#' @return Vector of response 
#' @export
#' 
#' @examples 
#' 
#' data(pasi)
#' 
#' data(pasi)
#' getResponse(pasi.ch ~ dose, data=pasi)
#' 
#' library(survival)
#' data(ovarian)
#' ovarian$subgroups <- as.factor(10*ovarian$ecog.ps+ovarian$rx)
#' 
#' surv <- getResponse(Surv(futime,fustat)~rx, data=ovarian)
#' print(surv)
#' class(surv)
#' 

getResponse <- function(formula, data) {
  
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  

  if (attr(terms(mf), "response") == 1) {
    mf[[1]]
  } else {
    NULL
  }
}




