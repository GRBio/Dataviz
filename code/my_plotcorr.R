my.plotcorr <- function (corr, outline = FALSE, col = "grey", 
                         upper.panel = c("ellipse", "number", "none"), 
                         lower.panel = c("ellipse", "number", "none"),
                         # added "hist"
                         diag = c("none", "ellipse", "number","hist"), 
                         digits = 2, bty = "n", axes = FALSE, 
                         xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), 
                         cex = 0.75 * par("cex"), mar = 0.1 + c(2, 2, 4, 2), 
                         # added data
                         dataset=NA,labels=NA,...)
{
  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar, font.lab=2, font.axis=2)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  ##-- New
  if(is.na(labels[1])){
    R_LABS <- rowlabs[rows]
    C_LABS <- collabs[cols]
  }else{
    R_LABS <- labels
    C_LABS <- labels
  }
  
  text(rep(0, length(rows)), length(rows):1, labels = R_LABS, adj = 1, cex = cex.lab, font=2,srt=-45)
  text(cols, rep(length(rows) + 1, length(cols)), labels = C_LABS, srt = 90, adj = 0, cex = cex.lab, font=2,srt=45)
  mtext(xlab, 1, 0, font=2)
  mtext(ylab, 2, 0, font=2)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
        ########################
        # Modification --> Add option "hist": 
        # round(corr[i, j], digits=digits)
        # by:
        # formatC(corr[i, j], format='f' ,digits=digits)
        ########################
      } else if (diag == 'hist'){
        for(i in 1:5){
          subplot(hist(as.numeric(dataset[,i]),xlab='',ylab='',main='',col='#445BC1',border = 'black'),
                  grconvertX(c(.400, .490)+(i-1)*.115,"npc"), 
                  grconvertY(c(.580, .680)-(i-1)*.115,"npc"),
                  type='plt',
                  pars=list(xaxt='n',yaxt='n',bty='o',mar=c(0,0,0,0),oma=c(0,0,0,0),tcl=NA))
        }
        par(bty=bty)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        ########################
        # Modification --> Change: 
        # round(corr[i, j], digits=digits)
        # by:
        # formatC(corr[i, j], format='f' ,digits=digits)
        ########################
        text(j + 0.3, length(rows) + 1 - i, formatC(corr[i, j], format='f' ,digits=digits), adj = 1, cex = cex, font=2)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex, font=2)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible()
}

## modification of function of the package corrplot
colorlegend2 <- function (colbar, labels, at = NULL, xlim = c(0, 1), 
                         ylim = c(0,1), vertical = TRUE, ratio.colbar = 0.4, lim.segment = "auto", 
                         align = c("c", "l", "r"), addlabels = TRUE,...){
  if (is.null(at) && addlabels) {
    at <- seq(0L, 1L, length = length(labels))
  }
  if (is.null(lim.segment) || lim.segment == "auto") {
    lim.segment <- ratio.colbar + c(0, ratio.colbar * 0.2)
  }
  if (any(at < 0L) || any(at > 1L)) {
    stop("at should be between 0 and 1")
  }
  if (length(lim.segment) != 2) {
    stop("lim.segment should be a vector of length 2")
  }
  if (any(lim.segment < 0L) || any(lim.segment > 1L)) {
    stop("lim.segment should be between 0 and 1")
  }
  align <- match.arg(align)
  xgap <- diff(xlim)
  ygap <- diff(ylim)
  len <- length(colbar)
  rat1 <- ratio.colbar
  rat2 <- lim.segment
  if (vertical) {
    at <- at * ygap + ylim[1]
    yyy <- seq(ylim[1], ylim[2], length = len + 1)
    rect(rep(xlim[1], len), yyy[1:len], rep(xlim[1] + xgap *rat1, len), yyy[-1], col = colbar, border = colbar)
    rect(xlim[1], ylim[1], xlim[1] + xgap * rat1, ylim[2], border = "black")
    segments(xlim[1] + xgap * rat2[1], at, xlim[1] + xgap * rat2[2], at)
    if (addlabels) {
      pos.xlabel <- rep(xlim[1] + xgap * max(rat2, rat1),length(at))
      switch(align, l = text(pos.xlabel, y = at, labels = labels,pos = 4, ...), 
                    r = text(xlim[2], y = at, labels = labels, pos = 2, ...),
             ########################
             # Modification --> Adding: 
             # + seq(0.04,0,length.out = length(at))
             # in ordre to match the labels
             ########################
                    c = text((pos.xlabel + xlim[2])/2 + 0.1,y = at + seq(0.04,0,length.out = length(at)), labels = labels, ...),
                    stop("programming error - should not have reached this line!"))
    }
  }
  else {
    at <- at * xgap + xlim[1]
    xxx <- seq(xlim[1], xlim[2], length = len + 1)
    rect(xxx[1:len], rep(ylim[2] - rat1 * ygap, len), xxx[-1], 
         rep(ylim[2], len), col = colbar, border = colbar)
    rect(xlim[1], ylim[2] - rat1 * ygap, xlim[2], ylim[2], 
         border = "black")
    segments(at, ylim[2] - ygap * rat2[1], at, ylim[2] - 
               ygap * rat2[2])
    if (addlabels) {
      pos.ylabel <- rep(ylim[2] - ygap * max(rat2, rat1), 
                        length(at))
      switch(align, l = text(x = at, y = pos.ylabel, labels = labels, 
                             pos = 1, ...), r = text(x = at, y = ylim[1], 
                                                     labels = labels, pos = 2, ...), c = text(x = at, 
                                                                                              y = (pos.ylabel + ylim[1])/2, labels = labels, 
                                                                                              ...), stop("programming error - should not have reached this line!"))
    }
  }
}

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}