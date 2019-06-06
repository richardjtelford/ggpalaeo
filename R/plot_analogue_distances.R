##' @name analogue_distances
##' @rdname analogue_distances
##' @title ggplot-based plot for analogue distance
##'
##' @description
##' Calculate and plot the distance to nearest analogue.
##'
##'
##' ...
##'
##' @param spp data.frame or matrix of calibration set species abundances
##' @param fos data.frame or matrix of fossil species abundances
##' @param df data.frame containing e.g. ages or depths of samples
##' @param x_axis character; name of variable for x-axis. Defaults to sample number.
##' @param quantiles numeric; quantiles of the training set residual lengths to plot
##' @param fill character; colours for the background fill for each quality categories
##' @param categories character; names of residuals length quality categories.
##' @param ... extra arguments to \code{\link[rioja]{MAT}}. \code{dist.method} is the only relevant one.
##' @param object analogue_distance object
##'
##' @return Returns a ggplot object.
##'
##' @author Richard J. Telford
##'
##' @export
##'
##' @importFrom ggplot2 autoplot ggplot geom_point geom_rect ylab aes fortify scale_fill_manual
##' @importFrom dplyr tibble bind_cols mutate n
##' @importFrom magrittr %>%
##' @importFrom rioja MAT
##' @importFrom stats as.dist quantile predict
##'
##' @examples
##'require("ggplot2")
##'data(ImbrieKipp, V12.122, package = "analogue")
##'## squared residual lengths for Core V12.122
##'AD <- analogue_distances(ImbrieKipp, V12.122)
##'autoplot(AD) +
##'  labs(y = "Squared-chord distance", x = "n")
##' @export
NULL

##' @name analogue_distances
##' @rdname analogue_distances
##' @export
analogue_distances <- function(spp, fos, df, x_axis,
                               quantiles = c(0.05, 0.1), ...){

  mod <- MAT(y = spp, x = rep(1, nrow(spp)), lean = FALSE, ...)
  pred <- predict(mod, fos)

  goodpoorbad <- mod$dist %>%
    as.dist() %>%
    quantile(probs = quantiles)

  x <- tibble(minD = pred$diagnostics$minD) %>%
    mutate(n = 1:n())

  res <- list(x = x, goodpoorbad = goodpoorbad, quantiles = quantiles)
  class(res) <- "analogue_distances"
  return(res)
}

##' @name analogue_distances
##' @rdname analogue_distances
##' @export
autoplot.analogue_distances <- function(object,
                                        df,
                                        x_axis,
                                        fill = c("salmon", "lightyellow",
                                                 "skyblue"),
                                        categories = c("Good", "Fair", "Poor"),
                                        ...) {


  x <- object$x

  if(!missing(df)){
    x <- bind_cols(df, x)
  }
  if(missing(x_axis)){
    x_axis <- "n"
  }
  plot_diagnostics(x = x, x_axis = x_axis, y_axis = "minD",
                   goodpoorbad = object$goodpoorbad,
                   fill = fill, categories = categories)
}

plot_diagnostics <- function(x, x_axis, y_axis, goodpoorbad,
                             fill = c("salmon", "lightyellow", "skyblue"),
                             categories){

  if(!length(fill) == length(categories)) {
    stop("Must have the same number of colours in fill as categories")
  }
  if(length(categories) != length(goodpoorbad) + 1) {
    stop("Must have one more category than quantile")
  }

  qualitybands <- tibble(
    xmin = rep(-Inf, length(categories)),
    xmax = rep(Inf, length(categories)),
    ymax = c(goodpoorbad, Inf),
    ymin = c(-Inf, goodpoorbad),
    fill = factor(categories, levels = rev(categories))
  )

  g <- ggplot(x, aes(x = .data[[x_axis]], y  = .data[[y_axis]])) +
    geom_rect(
      aes(xmin = .data$xmin, xmax = .data$xmax,
          ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
      qualitybands, alpha = .5, inherit.aes = FALSE) +
    scale_fill_manual(values = fill, name = "Quality") +
    geom_point()
  return(g)
}

