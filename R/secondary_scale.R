#' secondary_scale
#' @title Secondary scale for strat.plot
#' @description Adds a secondary scale for \code{\link[rioja]{strat.plot}}
#' @param x	a stratigraphic diagram object produced by strat.plot.
#' @param yvar y variable. Same as main plot.
#' @param yvar2 Secondary y variable
#' @param ylim y limits. Defaults to range of data.
#' @param n Suggested number of labels on secondary y axis. Defaults to five.
#' @param xLeft Position of plot. See \code{\link[rioja]{strat.plot}}
#' @param ylabel2 character. Label for secondary scale
#' @param cex.ylabel2 numeric. Size of axis label.
#' @importFrom rioja figCnvt
#' @importFrom graphics axis mtext par plot
#' @importFrom stats approx
#' @examples
#' library(rioja)
#' data(RLGH)
#' fos <- RLGH$spec
#' chron <- RLGH$depths
#' pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = .22, scale.percent = TRUE)

#' secondary_scale(pt, yvar = chron$Depth, yvar2 = chron$Age, n = 5, ylabel2 = "Years")
#' @export

secondary_scale <- function(x, yvar, yvar2, n = 5,
                            xLeft = 0.11,
                            ylabel2 = "", cex.ylabel2 = 1){
  orig.fig <- par("fig")
  agedepth <- approx(yvar2, yvar, xout = pretty(yvar2, n = n))
  par(mai = c(0, 0, 0, 0))
  par(
    fig = figCnvt(orig.fig, c(xLeft, min(xLeft + 0.4, 0.9), x$box["yBottom"], x$box["yTop"])),
    new = TRUE
    )

  plot(0, type = "n", axes = FALSE, ann = FALSE, yaxs = "r", ylim = x$ylim)
  axis(side = 2, at = agedepth$y, labels = agedepth$x, las= 2, xpd = NA)
  mtext(ylabel2, side = 2, line = 2.5, cex = cex.ylabel2)
  par("fig" = orig.fig) #reset "fig"
}


