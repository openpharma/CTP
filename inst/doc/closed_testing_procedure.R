## ----setup, include = FALSE---------------------------------------------------
library(knitr);library(CTP)
knitr::opts_chunk$set(collapse = TRUE, out.width="100%"
											 ,fig.width=12,fig.height=8,dev.args = list(pointsize=25) )

## -----------------------------------------------------------------------------
library(CTP)

data(pasi)
three.to.first <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
Display(three.to.first,Type="s",arrow=TRUE)
pasi.ctp.F1    <- AnalyseCTP(three.to.first,pasi.ch~dose,pasi)
xsx            <- summary(three.to.first)

summary(pasi.ctp.F1)
Display(pasi.ctp.F1)

## -----------------------------------------------------------------------------

data(pasi)
three.to.first <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
Display(three.to.first,Type="s",arrow=TRUE)
pasi.ctp.F1    <- AnalyseCTP(three.to.first,pasi.ch~dose,pasi)
xsx            <- summary(three.to.first)

summary(pasi.ctp.F1)
Display(pasi.ctp.F1)

## -----------------------------------------------------------------------------
dose.steps4 <- IntersectHypotheses(list(1:2,2:3,3:4))
Display(dose.steps4,arr=TRUE)



pasi.ctp.F2 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi)
summary(pasi.ctp.F2)
Display(pasi.ctp.F2)

## -----------------------------------------------------------------------------
pasi.ctp.K <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="kruskal")
summary(pasi.ctp.K)
Display(pasi.ctp.K)

## -----------------------------------------------------------------------------
pasi.ctp.J1 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="jonckheere",alternative="increasing")
pasi.ctp.J2 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="jonckheere",alternative="two.sided")
summary(pasi.ctp.J1)
summary(pasi.ctp.J2)
pasi.ctp.J3 <- AnalyseCTP(dose.steps4,pasi.ch~dose,pasi, test="jonckheere")
summary(pasi.ctp.J3)
Display(pasi.ctp.J1)

## -----------------------------------------------------------------------------
	two.to.first<- IntersectHypotheses(list(1:2,c(1,3)))

	Display(two.to.first,Type="s",main="two vs control",arrow=TRUE)

	#The two elementary hypotheses  are tested after comparing the three proportions globally.

	data(colorectal)
	colorectal.ctp <-AnalyseCTP(two.to.first,responder~dose,data=colorectal, test="prob")
	summary(colorectal.ctp)
	Display(colorectal.ctp,Type="t")

	colorectal.chisq <-AnalyseCTP(two.to.first,responder~dose,data=colorectal, test="chisq")
	summary(colorectal.chisq,digits=1)


## -----------------------------------------------------------------------------
library(survival)
		data(ovarian)
		
		print(survdiff(Surv(futime,fustat)~rx, data=ovarian))

## -----------------------------------------------------------------------------
		ovarian$subgroups <- as.factor(10*ovarian$ecog.ps+ovarian$rx)
		print(head(ovarian))

## -----------------------------------------------------------------------------
		comb.sub  <- IntersectHypotheses(list(c(1,2),c(3,4)))
		#Display(comb.sub)
		ovar.ctp  <-AnalyseCTP(comb.sub,Surv(futime,fustat)~subgroups, ovarian, test="lgrank")
		summary(ovar.ctp)
		Display(ovar.ctp)

## -----------------------------------------------------------------------------
				data(glucose)
				glucose.ctp <- AnalyseCTP(three.to.first,GLUCOSE.CHANGE~GLUCOSE.BLA+DOSE,
				                         data=glucose, factor.name="DOSE")
				summary(glucose.ctp)
				Display(glucose.ctp,Type="s")
								

## -----------------------------------------------------------------------------
				G <- factor(rep(1:5,each=4)	)			
				y <- rnorm(20)
				Y <- data.frame(G,y)
				
xxx <- IntersectHypotheses(list(1:2,c(1,3),c(1,4),c(1,5),c(2,5),c(3,4)))
summary(xxx)
Display(xxx)

## -----------------------------------------------------------------------------
	xxx <- IntersectHypotheses(list(1:2,c(1,3),c(1,4)))
  xxx$hypothesis
  data.frame(xxx$hypnames)
  xxx$hypnames
  p.val<-c(0.05,0.04,0.02,0.08,0.03,0.03,0.04)

  Adjust_raw(xxx, p.val)
	# the vector of p-values calculated by another software
	# you may supply the hypothesis names as names of the vector
	
  result <- Adjust_raw(ctp.struc=three.to.first, p.value=p.val)
	
	summary(result)
	
	# details may be documented
	
	result<-Adjust_raw(ctp.struc=three.to.first, p.value=p.val
	          ,dataset.name="mydata", factor.name="treatment"
	          ,factor.levels=c("A","B","C","D"), model=y~treatment
	          ,test.name="F")
	
	summary(result)
	Display(result)
 

