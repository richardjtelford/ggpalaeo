##' @rdname autoplot.tf
##' @title autoplot for rioja transfer functions
##' @param object transfer function object from rioja
##' @param column character, name of column to plot
##' @param npls integer, WAPLS component to plot
##' @param k integer, MAT number of analogues to plot
##' @param weighted logical, plot weighted MAT results?
##' @param show_apparent logical, show apparent performance as well as cross-validated
##' @param residuals logical, show cross-validated residuals
##' @param smooth logical, add smooth to plot
##' @export
##' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap geom_abline geom_hline coord_equal as_labeller
##' @importFrom dplyr data_frame
##' @importFrom magrittr %>%
##'

##' @rdname autoplot.tf
##' @export
fortify.WA <- function(object, column = "WA.inv", ...){
  fortify.tf(object, column, ...)
}

##' @rdname autoplot.tf
##' @export
fortify.WAPLS <- function(object, npls = 1, ...) {
  column <- paste0("Comp", ifelse(npls > 10, "", "0"), npls)
  fortify.tf(object, column = column, ...)
}

##' @rdname autoplot.tf
##' @export
fortify.MLRC <- function(object, ...){
  fortify.tf(object, column = "MLRC", ...)
}


##' @rdname autoplot.tf
##' @export
fortify.MAT <- function(object, k = 5, weighted = FALSE...){
  column <- paste0("N", ifelse(k > 10, "", "0"), ifelse(weighed, ".wm", ""))
  x <- data_frame(
    measured = object$x,
    predicted = object$fitted.values[, column],
    residuals = predicted - measured
  )
  if(!is.null(object$predicted)){
     warning("cross-validated MAT results (other than LOO) not extracted")
  }
}

##' @rdname autoplot.tf
##' @export
fortify.tf <- function(object, column, ...){
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


##' @rdname autoplot.tf
##' @export
autoplot.WAPLS <- function(object, npls, ...){
  autoplot.tf(object = object, npls = npls, ...)
}

##' @rdname autoplot.tf
##' @export
autoplot.WA <- function(object, ...){
  autoplot.tf(object = object, ...)
}

##' @rdname autoplot.tf
##' @export
autoplot.MAT <- function(object, k = 5, weighted = FALSE, ...){
  autoplot.tf(object = object, k = k, weighted = weighted, ...)
}

##' @rdname autoplot.tf
autoplot.MLRC <- function(object, ...){
  autoplot.tf(object = object, ...)
}

##' @rdname autoplot.tf
##' @export
autoplot.tf <- function(object, show_apparent = FALSE, residuals = FALSE, smooth = TRUE, ...){
  x <- fortify(object = object, ...)

  if(residuals){
    g <- ggplot(x, aes(x = measured, y = residuals)) +
      geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
      geom_point()
  } else{
    if(show_apparent){
     g <- x %>% gather(key = what, value = predicted, -measured) %>%
       filter(what %in% c("fitted", "predicted")) %>%
       ggplot(aes(x = measured, y = predicted)) +
       facet_wrap(~ what, labeller = as_labeller(c(fitted = "Apparent", predicted = "Cross-validated")))
    } else {
      g <- ggplot(x, aes(x = measured, y = predicted))
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
