##' @name autoplot.reslen
##' @rdname autoplot.reslen
##' @title ggplot-based plot for objects of class \code{'reslen'}
##'
##' @description
##' Produces a ggplot object representing the output of objects produced by \code{\link[analogue]{residLen}}.
##'
##'
##' @param object an object of class \code{"reslen"}, the result of a call to \code{\link[analogue]{residLen}}
##' @param df data.frame containing e.g. ages or depths of samples
##' @param x_axis character; name of variable for x-axis. Defaults to sample number.
##' @param quantiles numeric; quantiles of the training set residual lengths to plot
##' @param fill character; colours for the background fill for each quality categories
##' @param categories character; names of residuals length quality categories.
##'
##' @return Returns a ggplot object.
##'
##' @author Richard J. Telford
##'
##' @export
##'
##' @importFrom ggplot2 autoplot ggplot geom_point geom_rect ylab aes_string
##' @importFrom dplyr filter data_frame
##' @importFrom magrittr %>%
##' @examples
##'
##'data(ImbrieKipp, SumSST, V12.122, package = "analogue")
##'## squared residual lengths for Core V12.122
##'rlens <- residLen(ImbrieKipp, SumSST, V12.122)
##'autoplot(rlens, df = data_frame(age = as.numeric(rownames(V12.122))), x_axis = "age")
##'
NULL


##' @rdname autoplot.reslen
##' @export
fortify.residLen <- function(object, df){

  passive <- data_frame(sq_res_len = object$passive)

  if(!missing(df)){
    passive <- bind_cols(df, passive)
  } else {
    passive <- passive %>% mutate(n = 1:n())
  }

  train <- data_frame(sq_res_len = object$train)

  passive_train <- bind_rows(passive = passive, train = train, .id = "what")

  return(passive_train)
}

##' @rdname autoplot.reslen
##' @export
autoplot.reslen <- function(object, df, x_axis, quantiles = c(0.9, 0.95), fill = c("salmon", "lightyellow", "skyblue"), categories = c("Good", "Fair", "Poor")){

  if(!length(fill) == length(categories)) {
    stop("Must have the same number of colours in fill as categories")
  }
  if(length(categories) != length(quantiles) + 1) {
    stop("Must have one more category than quantile")
  }

  x <- fortify(object = object, df = df)
  if(missing(x_axis)){
    x_axis <- "n"
  }

  goodpoorbad <- filter(x, what == "train")$sq_res_len %>%
    quantile(probs = quantiles)
  qualitybands <- data_frame(
    xmin = rep(-Inf, length(categories)),
    xmax = rep(Inf, length(categories)),
    ymax = c(goodpoorbad, Inf),
    ymin = c(-Inf, goodpoorbad),
    fill = factor(categories, levels = rev(categories))
  )

  g <- x %>% filter(what == "passive") %>%
    ggplot(aes_string(x = x_axis, y  = "sq_res_len")) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), qualitybands, alpha = .5, inherit.aes = FALSE) +
      scale_fill_manual(values = fill, name = "Goodness of fit") +
      geom_point() +
      ylab("Squared Residual Length")
  return(g)
}

