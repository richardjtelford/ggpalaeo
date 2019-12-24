#' secondary_scale
#' @title Secondary scale for strat.plot
#' @description Adds a secondary scale for \code{\link[rioja]{strat.plot}}
#' @param x	a stratigraphic diagram object produced by strat.plot.
#' @param yvar y variable. Same as main plot, probably depth
#' @param dates data.frame with columns `up``, `down`, `age`, `error`
#' @param xLeft Position of plot. See \code{\link[rioja]{strat.plot}}
#' @param cex font size
#' @importFrom rioja figCnvt
#' @importFrom graphics par plot rect
#' @importFrom stats approx
#' @examples
#' library(rioja)
#' library(tibble)
#' data(RLGH)
#' fos <- RLGH$spec
#' chron <- RLGH$depths
#' dates <- tribble(~up, ~down, ~age, ~error,
#'                   4,      5,   1970,   5,
#'                   15,     20,  1900,  20)
#' pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = 0.3, scale.percent = TRUE)

#' plot_chron_info(pt, yvar = chron$Depth, dates = dates)
#' @export

plot_chron_info <- function(
  x, yvar, dates, xLeft = 0,
  cex = 0.8){
  orig.fig <- par("fig")
  par(mai = c(0, 0, 0, 0))
  par(
    fig = figCnvt(orig.fig, c(xLeft, min(xLeft + 0.2, 0.9), x$box["yBottom"], x$box["yTop"])),
    new = TRUE
    )

  plot(0, type = "n", axes = FALSE, ann = FALSE, yaxs = "r", ylim = x$ylim, xlim = c(0, 10))
  rect(xleft = rep(9, nrow(dates)),
       xright = rep(10, nrow(dates)),
       ybottom = dates$down,
       ytop = dates$up,
       col = "black")
    text(x = rep(0, nrow(dates)), y = (dates$up + dates$down) / 2,
         labels = paste0(dates$age, "\U00B1", dates$error),
         pos = 4, cex = cex)

  par("fig" = orig.fig) #reset "fig"
}


