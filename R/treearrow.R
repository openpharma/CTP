# Auxiliary functions for plotting segments

 Treearrow <- function (from, to, lwd = 1, lty = 1, lcol = "black", arr.col = lcol, 
          arr.side = 2, arr.pos = 0.35, line.pos = 0.5, path = "H", arrow=FALSE,...) 
{
  sarr <- function(p1, p2, drawarr) {
    if (drawarr) 
      m1 <- rbind(m1, straightarrow(from = p1, to = p2,
                                     arr.pos = arr.pos, lwd = lwd, lty = lty, lcol = lcol, 
                                     arr.col = arr.col, ...)) 
    else segments(p1[1], p1[2], p2[1], p2[2], lwd = lwd, 
                  lty = lty, col = lcol)
  }

  From <- matrix(ncol = 2, data = from)
  To <- matrix(ncol = 2, data = to)
  m1 <- NULL
  ifelse(path == "H", ii <- 2, ii <- 1)
  rF <- range(From[, ii])
  rT <- range(To[, ii])
  ifelse(abs(min(rF) - max(rT)) <= abs(max(rF) - min(rT)), 
         m2 <- min(rF) + line.pos * (max(rT) - min(rF)), m2 <- max(rF) + 
           line.pos * (max(rF) - min(rT)))
  if (path == "H") {
    Line <- range(c(From[, 1], To[, 1]))
    segments(Line[1], m2, Line[2], m2, lwd = lwd, lty = lty, 
             col = lcol)
     for (i in 1:nrow(From)) sarr(From[i, ], c(From[i, 1], 
                                               m2), arrow)
     for (i in 1:nrow(To)) sarr(c(To[i, 1], m2), To[i, ], arrow)
  }
  else {
    Line <- range(c(From[, 2], To[, 2]))
    segments(m2, Line[1], m2, Line[2], lwd = lwd, lty = lty, 
             col = lcol)
     for (i in 1:nrow(From)) sarr(From[i, ], c(m2, From[i,2]),draw.arr=arrow)
     for (i in 1:nrow(To)) sarr(c(m2, To[i, 2]), To[i, ], draw.arr=arrow)
  }
  treearrow <- m1
}

#' @importFrom graphics segments
Segments <- function (from, to, lwd = 1, lty = 1, lcol = "black", arr.col = lcol, 
          arr.pos = 0.35, endhead = FALSE, segment = c(0, 1), arrow=FALSE, ...) 
{
  if (segment[1] != 0) 
    From <- segment[1] * to + (1 - segment[1]) * from
  else From <- from
  if (segment[2] != 1) 
    To <- segment[2] * to + (1 - segment[2]) * from
  else To <- to
  meanpi <- arr.pos * To + (1 - arr.pos) * From
  if (endhead) 
    To <- meanpi
  if(arrow==FALSE) segments(From[1], From[2], To[1], To[2], lwd = lwd, lty = lty, col = lcol)
  if(arrow==TRUE) arrows(From[1], From[2], To[1], To[2], lwd = lwd, lty = lty, col = lcol)
  
  mid2 <- c((arr.pos - 0.01) * to + (1 - arr.pos + 0.01) * 
              from)
  mid1 <- c((arr.pos) * to + (1 - arr.pos) * from)

}
Straightarrow <- function (from, to, lwd = 1, lty = 1, lcol = "black", arr.col = lcol, 
                           arr.pos = 0.35, endhead = FALSE, segment = c(0, 1), arrow=FALSE, ...) 
{
  if (segment[1] != 0) 
    From <- segment[1] * to + (1 - segment[1]) * from
  else From <- from
  if (segment[2] != 1) 
    To <- segment[2] * to + (1 - segment[2]) * from
  else To <- to
  meanpi <- arr.pos * To + (1 - arr.pos) * From
  if (endhead) 
    To <- meanpi
  segments(From[1], From[2], To[1], To[2], lwd = lwd, lty = lty, 
           col = lcol)
  mid2 <- c((arr.pos - 0.01) * to + (1 - arr.pos + 0.01) * 
              from)
  mid1 <- c((arr.pos) * to + (1 - arr.pos) * from)
  if(arrow) Arrows(mid2[1], mid2[2], mid1[1], mid1[2], lcol = lcol, arr.col = arr.col,...)
  straightarrow <- mid1
}