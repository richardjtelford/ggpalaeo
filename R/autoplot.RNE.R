##' @title autoplot for palaeosig rne
##' @description Plots of r2 against fraction of sites deleted randomly, by neighbourhood or by environmental similarity.
##' @param object RNE object from palaeosig
##' @param which which reconstruction
##' @param ylim numeric vector length 2. y limits
##' @param unit character. Neighbourhood units
##' @examples
##'   require(fields)
##'   require(palaeoSig)
##'   require(rioja)
##'   require(ggplot2)
##'   data(arctic.env)
##'   data(arctic.pollen)
##'
##'   #using just the first 100 sites so code runs in an reasonable time, but still slow
##'   arctic.d <- rdist.earth(arctic.env[1:100,c("Longitude","Latitude")], miles = FALSE)
##'   arctic.rne <- rne(y = arctic.pollen[1:100,], env = arctic.env$tjul[1:100],
##'     geodist = arctic.d, fun = MAT, neighbours = c(0,50,200),
##'     subsets = c(1,.5,.1), k = 5)
##' autoplot(arctic.rne)
##' @export
##' @importFrom ggplot2 aes autoplot ggplot geom_point geom_line coord_cartesian fortify scale_shape_manual scale_linetype_manual scale_colour_manual labs theme
##' @importFrom purrr map_df
##' @importFrom tibble tibble
##' @importFrom ggrepel geom_text_repel
##'

##' @rdname autoplot.RNE
##' @export
fortify.RNE <- function(object, which = 1, unit = ""){
    random <- object$random[, c(1, 1 + which)]
    random <- as.data.frame(random)
    names(random) <- c("prop", "r2")
    random$prop <- 1 - random$prop

    neighbour <- map_df(
      object$neigh,
      ~tibble(neighbour = .$neighbour, prop = .$effn, r2 = .$hb.r2[which])
    )
    environ <- map_df(
      object$neigh,
      ~tibble(neighbour = .$neighbour, prop = .$effn, r2 = .$eb.r2[which])
    )
    fort <- bind_rows(
      random = random,
      neighbour = neighbour,
      environ = environ,
      .id = "type"
    )
    fort$neighbour <- paste(fort$neighbour, unit)
    fort$type <- factor(fort$type, levels = c("random", "environ", "neighbour"))
    return(fort)
}

##' @rdname autoplot.RNE
##' @export
autoplot.RNE <- function(object, which = 1, ylim, unit = "km"){
  fort <- fortify(object, which, unit)
  if (missing(ylim)){
    ylim <- range(fort[fort$type != "environ", "r2"])
  }
  g <-  ggplot(fort,
               aes(x = .data$prop, y = .data$r2,
                   colour = .data$type, shape = .data$type,
                   linetype = .data$type, label = .data$neighbour)) +
    geom_line() +
    geom_point(fill = "white") +
    geom_text_repel(data = fort[fort$type == "neighbour", ], hjust = "right") +
    labs(x = "Fraction of sites deleted", y = expression("r"^2)) +
    coord_cartesian(ylim = ylim) +
    scale_colour_manual(values = c("black", "red", "black")) +
    scale_linetype_manual(values = c("solid", "dashed", "solid")) +
    scale_shape_manual(values = c(21, 3, 16)) +
    theme(legend.position = "none")

 return(g)
}

