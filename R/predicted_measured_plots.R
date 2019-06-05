##' @rdname autoplot.WA
##' @title autoplot for rioja transfer functions
##' @description Plots of predicted against measured environmental variables for transfer functions made with the rioja package.
##' @param object transfer function object from rioja
##' @param column character, name of column to plot
##' @param npls integer, WAPLS component to plot
##' @param k integer, MAT number of analogues to plot
##' @param weighted logical, plot weighted MAT results?
##' @param show_apparent logical, show apparent performance as well as cross-validated
##' @param residuals logical, show cross-validated residuals
##' @param smooth logical, add smooth to plot
##' @param ... Arguments specific to different models.
##' @examples
##' require("rioja")
##' require("ggplot2")
##' data(ImbrieKipp, SumSST, V12.122, package = "analogue")
##' mod <- MAT(ImbrieKipp, SumSST)
##' autoplot(mod)
##' @export
##' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth facet_wrap geom_abline geom_hline coord_equal as_labeller fortify
##' @importFrom dplyr data_frame
##' @importFrom tidyr gather
##' @importFrom magrittr %>%
##' @importFrom rlang .data
##'

##' @rdname autoplot.WA
##' @export
fortify.WA <- function(object, column = "WA.inv"){
  fortify_tf(object, column)
}

##' @rdname autoplot.WA
##' @export
fortify.WAPLS <- function(object, npls = 1) {
  column <- paste0("Comp", ifelse(npls > 10, "", "0"), npls)
  fortify_tf(object, column = column)
}

##' @rdname autoplot.WA
##' @export
fortify.MLRC <- function(object){
  fortify_tf(object, column = "MLRC")
}


##' @rdname autoplot.WA
##' @export
fortify.MAT <- function(object, k = 5, weighted = FALSE){
  column <- paste0("N", ifelse(k > 10, "", "0"), k, ifelse(weighted, ".wm", ""))
  x <- data_frame(
    measured = object$x,
    predicted = object$fitted.values[, column],
    residuals = .data$predicted - .data$measured
  )
  if(!is.null(object$predicted)){
     warning("cross-validated MAT results (other than LOO) not extracted")
  }
  return(x)
}

##' @rdname autoplot.WA
##'
fortify_tf <- function(object, column){
  x <- data_frame(
    measured = object$x,
    fitted = object$fitted.values[, column]
    )
  if(!is.null(object$predicted)){
    x <- x %>%
      mutate(
        predicted = object$predicted[, column],
        residuals = object$residuals.cv[, column]
      )
  } else {
    warning("No cross-validated predictions")
  }

  return(x)
}


##' @rdname autoplot.WA
##' @export
autoplot.WAPLS <- function(object, npls, ...){
  autoplot_tf(object = object, npls = npls, ...)
}

##' @rdname autoplot.WA
##' @export
autoplot.WA <- function(object, ...){
  autoplot_tf(object = object, ...)
}

##' @rdname autoplot.WA
##' @export
autoplot.MAT <- function(object, k = 5, weighted = FALSE, ...){
  autoplot_tf(object = object, k = k, weighted = weighted, ...)
}

##' @rdname autoplot.WA
autoplot.MLRC <- function(object, ...){
  autoplot_tf(object = object, ...)
}

##' @rdname autoplot.WA
##'
autoplot_tf <- function(object, show_apparent = FALSE, residuals = FALSE,
                        smooth = TRUE, ...){
  x <- fortify(object = object, ...)

  if(residuals){
    g <- ggplot(x, aes_string(x = "measured", y = "residuals")) +
      geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
      geom_point()
  } else{
    if(show_apparent){
     g <- x %>% gather(key = "what", value = "predicted", -.data$measured) %>%
       filter(.data$what %in% c("fitted", "predicted")) %>%
       ggplot(aes_string(x = "measured", y = "predicted")) +
       facet_wrap(~ what,
                  labeller = as_labeller(
                    c(fitted = "Apparent", predicted = "Cross-validated")))
    } else {
      g <- ggplot(x, aes_string(x = "measured", y = "predicted"))
    }
    g <- g + geom_abline(colour = "grey50", linetype = "dashed") +
      geom_point() +
      coord_equal()
  }

  if(smooth){
    g <- g + geom_smooth(se = FALSE)
  }

  return(g)
}
