#' secondary.scale
#' @title Secondary scale for strat.plot
#' @description Adds a secondary scale for \code{\link[rioja]{strat.plot}}
#' @param yvar y variable. Same as main plot.
#' @param yvar2 Secondary y variable
#' @param ylim y limits. Defaults to range of data.
#' @param n Suggested number of labels on secondary y axis. Defaults to five.
#' @param y.rev logical. Reverse y-axis. Do as in main plot.
#' @param xLeft Position of plot. See \code{\link[rioja]{strat.plot}}
#' @param yBottom ditto
#' @param yTop ditto
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
#' strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = .22, scale.percent = TRUE)

#' secondary.scale(yvar = chron$Depth, yvar2 = chron$Age, n = 5, y.rev = TRUE, ylabel2 = "Years")
#' @export

secondary.scale <- function(yvar, yvar2, ylim = NULL, n = 5, y.rev = FALSE,
                            xLeft = 0.11, yBottom = 0.07, yTop = 0.8,
                            ylabel2 = "", cex.ylabel2 = 1){
  if (is.null(ylim)){
    ylim <- range(yvar, na.rm = TRUE)
  }
  if (y.rev) {
    ylim <- rev(ylim)
  }

  orig.fig <- par("fig")
  agedepth <- approx(yvar2, yvar, xout = pretty(yvar2, n = n))
  par(mai = c(0, 0, 0, 0))
  par(
    fig = figCnvt(orig.fig, c(xLeft, min(xLeft + 0.4, 0.9), yBottom, yTop)),
    new = TRUE
    )


  plot(0, type = "n", axes = FALSE, ann = FALSE, yaxs = "r", ylim = ylim)
  axis(side = 2, at = agedepth$y, labels = agedepth$x, las= 2, xpd = NA)
  mtext(ylabel2, side = 2, line = 2.5, cex = cex.ylabel2)
  par("fig" = orig.fig) #reset "fig"
}


