##' @name autoplot.residLen
##' @title ggplot-based plot for objects of class \code{'reslen'}
##'
##' @description
##' Produces a ggplot object representing the output of objects produced by \code{\link[analogue]{residLen}}.
##'
##'
##' @param object an object of class \code{"residLen"}, the result of a call to \code{\link[analogue]{residLen}}
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
##' @importFrom ggplot2 autoplot ggplot geom_point geom_rect ylab aes fortify
##' @importFrom dplyr filter tibble bind_cols bind_rows n
##' @importFrom magrittr %>%
##' @importFrom stats quantile
##' @examples
##'require("analogue")
##'require("ggplot2")
##'data(ImbrieKipp, SumSST, V12.122, package = "analogue")
##'## squared residual lengths for Core V12.122
##'rlens <- residLen(ImbrieKipp, SumSST, V12.122)
##'autoplot(rlens, df = data.frame(age = as.numeric(rownames(V12.122))), x_axis = "age") +
##'labs(x = "Age", y = "Squared residual distance", fill = "Goodness of fit")
NULL


##' @rdname autoplot.residLen
##' @export
fortify.residLen <- function(object, df){

  passive <- tibble(sq_res_len = object$passive)

  if(!missing(df)){
    passive <- bind_cols(df, passive)
  } else {
    passive <- passive %>% mutate(n = 1:n())
  }

  train <- tibble(sq_res_len = object$train)

  passive_train <- bind_rows(passive = passive, train = train, .id = "what")

  return(passive_train)
}

##' @rdname autoplot.residLen
##' @export
autoplot.residLen <- function(object, df, x_axis,
                              quantiles = c(0.9, 0.95),
                              fill = c("salmon", "lightyellow", "skyblue"),
                              categories = c("Good", "Fair", "Poor")){

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

  goodpoorbad <- filter(x, x$what == "train")$sq_res_len %>%
    quantile(probs = quantiles)

  plot_diagnostics(
    x = filter(x, x$what == "passive"),
    x_axis = x_axis,
    y_axis = "sq_res_len",
    goodpoorbad = goodpoorbad,
    fill = fill,
    categories = categories
  )
}
