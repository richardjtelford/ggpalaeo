#' secondary.scale
#' @title Secondary scale for strat.plot
#' @description Adds a secondary scale for \code{\link[rioja]{strat.plot}}
#'
#' @param x	a stratigraphic diagram object produced by strat.plot.
#' @param g matrix or data.frame with summary groups. Column names will be used as labels.
#' @param yvar y variable. Same as main plot.
#' @param xLeft Position of plot. See \code{\link[rioja]{strat.plot}}
#' @param xRight ditto
#' @param y.axis ditto
#' @param ylabel ditto
#' @param cex.ylabel ditto
#' @param x.name.pos numeric. Position of names. Defaults to midpoint of uppermost sample.
#' @param srt.xlabel See \code{\link[rioja]{strat.plot}}
#' @param cex.xlabel ditto
#' @param fill vector of colours. Defaults to RcolourBrewer Set1
#'
#' @importFrom rioja figCnvt
#' @importFrom graphics axis mtext par plot polygon text
#' @importFrom purrr walk
#' @importFrom RColorBrewer brewer.pal
#' @examples
#'  library(rioja)
#'  library(tidyverse)
#'  data(RLGH)
#'  fos <- RLGH$spec
#'  chron <- RLGH$depths
#'
#'  groups <- case_when(
#'    substr(names(fos), 1, 1) %in% c("A", "E") ~ "Vowel",
#'    substr(names(fos), 1, 1) %in% c("B", "C", "F") ~ "BCF",
#'    TRUE ~ "NPT"
#'  ) %>%
#'    factor(levels = c("Vowel", "BCF", "NPT"))
#'
#'   g <- sapply(levels(groups), function(lev){
#'     rowSums(fos[, groups == lev, drop = FALSE])
#'     })
#'
#'
#'  pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = .1, xRight = 0.8, scale.percent = TRUE)
#'
#'  summary_plot(pt, g = g, yvar = chron$Depth, xLeft = 0.8)
#' @export

summary_plot <- function(x, g, yvar,
                         xLeft = 0.8, xRight = 1,
                         y.axis = FALSE, ylabel = "", cex.ylabel = 1,
                         x.name.pos, srt.xlabel = 90, cex.xlabel = 1, fill){
  if(nrow(g) != length(yvar)){
    stop("Length of yvar must equal number of rows in g")
  }
  if(missing(fill)){
    fill <- brewer.pal(n = ncol(g), name = "Set1")
  }
  if(ncol(g) != length(fill)){
    stop("Length of fill must equal number of columns in g")
  }

  #x.names
  x.names <- colnames(g)
  if(missing(x.name.pos)){
    if(x$y.rev){
      row <- which.min(yvar)
    } else {
      row <- which.max(yvar)
    }
    x.name.pos <- cumsum(c(0, g[row, ]))[seq_len(ncol(g))] + g[row, ]/2
  }

  xlim <- c(0, max(rowSums(g)))

  orig.fig <- par("fig")
  par(mai = c(0, 0, 0, 0))
  par(
    fig = figCnvt(orig.fig, c(xLeft, min(xLeft + 0.4, 0.9), x$box["yBottom"], x$box["yTop"])),
    new = TRUE
  )

  plot(0, type = "n", axes = FALSE, ann = FALSE, yaxs = "r",
       ylim = x$ylim, xlim = xlim)

  if(isTRUE(y.axis)){
    axis(side = 2, las= 2, xpd = NA)
    mtext(ylabel, side = 2, line = 2.5, cex = cex.ylabel)
  }
  axis(1)
  g <- cbind(zero = 0, g)
  walk(2:ncol(g), function(n){
    start <- rowSums(g[, 1:(n - 1), drop = FALSE])
    end <- rowSums(g[, 1:n, drop = FALSE])
    polygon(y = c(yvar, rev(yvar)), x = c(start, rev(end)), col = fill[n - 1])
  })

  usr1 <- par("usr")

  r <- (usr1[4] - usr1[3]) * 0.01
  pos <- usr1[4] + r
  if (x$y.rev){
    pos <- usr1[4] - r
  }
  pos <- rep(pos, length(x.name.pos))
  text(x.name.pos, pos,
       labels = x.names, adj = c(0, 0.5),
       srt = srt.xlabel, cex = cex.xlabel, xpd = NA
       )
  par("fig" = orig.fig) #reset "fig"
  invisible()
}


