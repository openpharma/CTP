#' Testing set for an elementary hypothesis
#'
#' @param \code{ctp.struc} Object of class \code{ctp.str}.
#' @param \code{Hyp} Elementary hypothesis (character variable).
#'
#' @return The testing set for the elementary hypothesis (character vector).
#'
#' @examples
#'
#'		three.to.first <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
#'    Set13  <- TestingSet(three.to.first,Hyp="[13]")
#'		Set=13
#'
#' @export

TestingSet <- function(ctp.struc,Hyp)
{
  HypNam   <- ctp.struc$hypnames
  nHyp     <- dim(HypNam)[1]
  con      <- ctp.struc$connections
  ncon     <- length(con)
  HypNo    <- HypNam$hyp.no[HypNam$hypothesis.name==Hyp]
  HypLev   <- HypNam$level[HypNam$hypothesis.name==Hyp]
  Con  <- con[[1]]
  max_lev  <- ncon+1

  if (ncon >1)
  {
    for (i in 2:ncon) Con <- rbind(Con,con[[i]])
  }

  From   <- Con %>% dplyr::select(level=levold,hyp.no=hypold) %>%
    left_join(HypNam,by = c("level", "hyp.no")) %>% suppressMessages() %>%
    rename(Hypothesis_1 =hypothesis.name,Level_1=level )

  To   <- Con %>% dplyr::select(level=levnew,hyp.no=hypnew) %>% suppressMessages() %>%
    left_join(HypNam,by = c("level", "hyp.no"))  %>%
    rename(Hypothesis_2 =hypothesis.name,Level_2=level )


  Connections  <- cbind(From,To) %>%
    dplyr::select(Level_1,Hypothesis_1,Level_2,Hypothesis_2) %>%
    arrange(Level_1,Hypothesis_1)
  XX <- list()
  XX[[1]] <- Connections %>% filter(Level_1==1 & Hypothesis_1==Hyp)
  if(max_lev > 2)
  {
    for(i in 2:(max_lev-1)) XX[[i]] <- subset(Connections,Level_1==i & Hypothesis_1 %in% XX[[i-1]]$Hypothesis_2)

  }
  XXfin   <- NULL
  for(i in 1:(max_lev-1)) XXfin <- rbind(XXfin,XX[[i]])
  HypGlob  <- HypNam$hypothesis.name[nHyp]
  SetA     <- sort(unique(c(XXfin$Hypothesis_1,XXfin$Hypothesis_2)))
  SetA
}


