# Auxiliary functions
#

ctp.single.test <-function(hypo,CTPparms,...)
{
  
  FT0 <- sapply(hypo, FUN = function(x) x == as.numeric(CTPparms$fac))
  FT  <- as.logical(apply(FT0, 1., sum))
  #print(FT)
  if(oldClass(CTPparms$fac)[1] == "ordered")
    newfac <- ordered(CTPparms$fac[FT])
  else newfac <- factor(CTPparms$fac[FT])
  # 
  if(CTPparms$test == "ctp.lgrank") re <- CTPparms$resp[FT,  ] else re <- CTPparms$resp[FT]
  psingle <- do.call(CTPparms$test, list(re, newfac,...))
  psingle
}

# Auxiliary function
# 
# Changes contrast of a factor to 'contr.sum'
#
# @param x a factor

ctp.change.contr <-
  function(x)
  {
    y <- x
    if(is.factor(x)) {
      p <- length(levels(y))
      if(p > 1)
        contrasts(y) <- contr.sum(p)
    }
    invisible(y)
  }

ctp.g.contr <-
  function(hypo, nlevel)
  {
    #cat("\n\n ctp.g.contr: hypo = ", hypo,"\n\n")
    #hypo is an 
    #nlevel is the 
    diag(nlevel)[, hypo] %*% contr.sum(length(hypo))
  }

ctp.gen.contr <- function(hyplist, nlevel)
{
  if(length(hyplist) < length(unlist(hyplist)))
  {
    xhyplist <- unlist(lapply(hyplist, ctp.g.contr, nlevel))
    matrix(unlist(lapply(hyplist, ctp.g.contr, nlevel)), nrow = nlevel)
  } else
    ctp.g.contr(hypo = hyplist, nlevel = nlevel)
}




# Auxiliary character function
#
ctp.chm <- function(x)
{
  if(length(x) == length(unlist(x)))
    paste("[", paste(x, collapse = "", sep = ""), "]", sep = "")
  else paste(unlist(lapply(x, ctp.ch1)), collapse = "")
}


# Auxiliary character function
#
# @param x 

ctp.ch1 <- function(x) 
{
  paste("[", paste(x, collapse = "", sep = ""), "]", sep = "")
}


# Auxiliary function
#
# @param i 
# @param L 
# @param P 
# 
ctp.max <-
  function(i, L, P)
  {
    hh <- rbind(L$relations[[i]][, -2.], L$relations[[i]][, -1.])
    pp <- apply(hh, 1., function(x, i, P)
      max(P[[i - 1.]][[x[1.]]], P[[i]][[x[2.]]]), i, P)
    qq <- aggregate(pp, as.character(hh[, 1.]), max)
    qord <- order(qq[, 1.])
    qq[, 2.][qord]
  }

################################################################
######### Auxiliary functions for list intersecrtions ##########
################################################################

# Auxiliary function
# 
# checks for intersections in lists of integer vectors
#
# @param i 
# @param L 
# @export
# 
# 
# 
ctp.check.lst.i <-
  function(i, L)
  {
    #checks for intersections in lists of integer vectors
    len <- length(L)
    numinsect <- 0.
    if(i < len) {
      intsec <- logical(len - i)
      for(j in (i + 1.):len)
        intsec[j - i] <- !(length(intersect(L[[i]], L[[j]])) == 0.)
      numinsect <- sum(intsec)
    }
    numinsect
  }
# Auxiliary function
#
# deletes doubles in lists of max. 2 levels
# 
# @param L list  of max. 2 levels
#


ctp.clean.lst <-
  function(L)
  {
    LL <- L
    couples <- matrix(c(0., 0.), 1., 2.)
    if(length(L) > 1.) {
      x <- matrix(1.:length(L), length(L), length(L))
      aa <- cbind(row(x)[row(x) < col(x)], col(x)[row(x) < col(x)])
      iord <- order(aa[, 1.], aa[, 2.])
      compare.what <- matrix(aa[iord,  ], nrow = length(iord))
      identical <- apply(compare.what, 1., function(i, L)
      {
        ctp.is.equal.lst(L[[i[1.]]], L[[i[2.]]])
      }
      , L)
      if(sum(identical) > 0.) {
        doubles <- compare.what[identical, 2.]
        couples <- matrix(compare.what[identical,  ], nrow = sum(identical))
        LL <- L[ - doubles]
      }
    }
    list(couples = couples, LL = LL)
  }
# Auxiliary funcction
# 
# intersection of two hypotheses
#
# @param lx 1st hypothesis (list)
# @param ly 2nd hypothesis (list)


ctp.intersect.hyp <-
  function(lx, ly)
  {
    #
    xlen <- length(lx)
    ylen <- length(ly)
    xvec <- xlen == length(unlist(lx))
    yvec <- ylen == length(unlist(ly))
    if(xvec)
      xlen <- 1.
    if(yvec)
      ylen <- 1.
    len <- xlen + ylen
    lxy <- vector("list", len)
    if(xvec)
      lxy[[1.]] <- lx
    else lxy[1.:xlen] <- lx
    if(yvec)
      lxy[[xlen + 1.]] <- ly
    else lxy[(xlen + 1.):len] <- ly
    lxy <- ctp.reduce.lst(lxy)
    lxy <- ctp.sort.hyp(lxy)
    lxy
  }

# Auxiliary function
#
#pairwise intersection of all elements of a hypothesis list
#
# @param L a hyplist  
#


ctp.intersect.hyplst <-
  function(L)
  {
    
    x <- matrix(1.:length(L), length(L), length(L))
    aa <- cbind(row(x)[row(x) < col(x)], col(x)[row(x) < col(x)])
    iord <- order(aa[, 1.], aa[, 2.])
    intersect.what <- matrix(aa[iord,  ], nrow = length(iord))
    LA <- apply(intersect.what, 1., function(i, L)
    {
      ctp.intersect.hyp(L[[i[1.]]], L[[i[2.]]])
    }
    , L)
    LA <- lapply(LA, function(x)
    {
      if(length(x) == 1.)
        unlist(x)
      else x
    }
    )
    LL <- ctp.clean.lst(LA)
    nhypnum <- 1.:length(intersect.what[, 1.])
    if(sum(LL$couples[, 1.]) > 0.) {
      cleng <- length(LL$couples[, 1.])
      for(i in cleng:1.)
        nhypnum <- ifelse(nhypnum == LL$couples[i, 2.], LL$couples[i, 1.], nhypnum)
      unum <- unique(nhypnum)
      ulen <- length(unum)
      for(j in 1.:ulen)
        nhypnum <- ifelse(nhypnum == unum[j], j, nhypnum)
    }
    inherit <- cbind(intersect.what, nhypnum)
    dimnames(inherit)[[2.]] <- c("oldno1", "oldno2", "newno")
    list(hyp = LL[[2.]], inherit)
  }

# Auxiliary function
#
#compares lists of max. 2 levels
#
# @param lsti 
# @param lstj 

ctp.is.equal.lst <-
  function(lsti, lstj)
  {
    li <- sapply(lsti, length)
    lj <- sapply(lstj, length)
    length(lsti) == length(lstj) && all(li == lj) && all(unlist(lsti) == unlist(lstj))
  }
# Auxiliary function
#
# @param i 
# @param L 

ctp.reduce.lst.i <-
  function(i, L)
  {
    nlst <- L
    zaehler <- 0.
    chk <- ctp.check.lst.i(i, nlst)
    while(chk > 0. && zaehler < 10.) {
      nlst <- ctp.reducelem(i, nlst)
      chk <- ctp.check.lst.i(i, nlst)
      zaehler <- zaehler + 1.
    }
    nlst
  }
# Auxiliary function
#
#reduces lists of integer vectors by union of all intersecting vectors
#
# @param L list of integer vectors
#
# @return

ctp.reduce.lst <-
  function(L)
  {
    #
    len <- length(L)
    LL <- L
    for(i in 1.:len)
      LL <- ctp.reduce.lst.i(i, LL)
    nlst <- lapply(LL, sort)
    nlst
  }
# Auxiliary function
#
# @param i 
# @param L 

ctp.reducelem <-
  function(i, L)
  {
    len <- length(L)
    LL <- L
    keep <- logical(len)
    for(k in 1.:i)
      keep[k] <- T
    eli <- LL[[i]]
    for(j in (i + 1.):len) {
      tr <- length(intersect(eli, LL[[j]])) == 0.
      if(!tr)
        eli <- union(eli, LL[[j]])
      else eli
      keep[j] <- tr
    }
    LL[[i]] <- eli
    LL[keep]
  }
# Auxiliary function
#
# @param L 
#

ctp.sort.hyp <-
  function(L)
  {
    #sorts a hypothesis
    xlen <- length(L)
    if(xlen == length(unlist(L)))
      sort(L)
    else {
      L <- lapply(L, sort)
      lord <- order(sapply(L, min))
      L <- L[lord]
    }
  }
# Auxiliary function
#
# @param hyplst 

ctp.unique.lst <-
  function(hyplst)
  {
    kk <- length(hyplst)
    doubl <- 0.
    if(kk > 1.) {
      for(i in 1.:(kk - 1.)) {
        for(j in (i + 1.):kk)
          doubl <- doubl + ctp.is.equal.lst(hyplst[[i]], hyplst[[j]])
      }
    }
    doubl
  }
# Auxiliary function
#
# @param hlst 

ctp.unique <-
  function(hlst)
  {
    doub <- ctp.unique.lst(hlst)
    ltyp <- sapply(hlst, mode)
    lhlst <- hlst[ltyp == "list"]
    len <- length(lhlst)
    doubv <- 0.
    if(len > 0.)
      doubv <- sum(sapply(lhlst, ctp.unique.lst))
    doub + doubv
  }
