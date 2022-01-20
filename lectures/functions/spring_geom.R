library(grid) 

springGrob <- function(x0 = unit(0, "npc"), y0 = unit(0, "npc"), 
                       x1 = unit(1, "npc"), y1 = unit(1, "npc"), 
                       diameter = unit(0.1, "npc"), tension = 0.75,
                       n = 50, default.units = "npc", name = NULL, 
                       gp = gpar(), vp = NULL) {
  if (!is.unit(x0)) x0 <- unit(x0, default.units)
  if (!is.unit(x1)) x1 <- unit(x1, default.units)
  if (!is.unit(y0)) y0 <- unit(y0, default.units)
  if (!is.unit(y1)) y1 <- unit(y1, default.units)
  if (!is.unit(diameter)) diameter <- unit(diameter, default.units)
  gTree(x0 = x0, y0 = y0, x1 = x1, y1 = y1, diameter = diameter, 
        tension = tension, n = n, name = name, gp = gp, vp = vp, 
        cl = "spring")
}

makeContent.spring <- function(x) {
  x0 <- convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- convertY(x$y1, "mm", valueOnly = TRUE)
  diameter <- convertWidth(x$diameter, "mm", valueOnly = TRUE)
  tension <- x$tension
  n <- x$n
  springs <- lapply(seq_along(x0), function(i) {
    cbind(
      create_spring(x0[i], y0[i], x1[i], y1[i], diameter[i], tension[i], n),
      id = i
    )
  })
  springs <- do.call(rbind, springs)
  spring_paths <- polylineGrob(springs$x, springs$y, springs$id, 
                               default.units = "mm", gp = x$gp)
  setChildren(x, gList(spring_paths))
}

GeomSpring <- ggproto("GeomSpring", Geom,
                      setup_params = function(data, params) {
                        if (is.null(params$n)) {
                          params$n <- 50
                        } else if (params$n <= 0) {
                          rlang::abort("Springs must be defined with `n` greater than 0")
                        }
                        params
                      },
                      draw_panel = function(data, panel_params, coord, n = 50, lineend = "butt", 
                                            na.rm = FALSE) {
                        data <- remove_missing(data, na.rm = na.rm,
                                               c("x", "y", "xend", "yend", "linetype", "size"),
                                               name = "geom_spring")
                        if (is.null(data) || nrow(data) == 0) return(zeroGrob())
                        if (!coord$is_linear()) {
                          rlang::warn("spring geom only works correctly on linear coordinate systems")
                        }
                        coord <- coord$transform(data, panel_params)
                        return(springGrob(coord$x, coord$y, coord$xend, coord$yend,
                                          default.units = "native", diameter = unit(coord$diameter, "cm"),
                                          tension = coord$tension, n = n,
                                          gp = gpar(
                                            col = alpha(coord$colour, coord$alpha),
                                            lwd = coord$size * .pt,
                                            lty = coord$linetype,
                                            lineend = lineend
                                          )
                        ))
                      },
                      required_aes = c("x", "y", "xend", "yend"),
                      default_aes = aes(
                        colour = "black", 
                        size = 0.5, 
                        linetype = 1L, 
                        alpha = NA, 
                        diameter = 0.35, 
                        tension = 0.75
                      )
)
geom_spring <- function(mapping = NULL, data = NULL, stat = "identity", 
                        position = "identity", ..., n = 50, lineend = "butt", 
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomSpring, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      n = n, 
      lineend = lineend, 
      na.rm = na.rm, 
      ...
    )
  )
}

scale_tension_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale(
    aesthetics = "tension", 
    scale_name = "tension_c", 
    palette = scales::rescale_pal(range), 
    ...
  )
}

scale_tension <- scale_tension_continuous

scale_tension_discrete <- function(...) {
  rlang::abort("Tension cannot be used with discrete data")
}

create_spring <- function(x, y, xend, yend, diameter = 1, tension = 0.75, n = 50) {
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  # Calculate direct length of segment
  length <- sqrt((x - xend)^2 + (y - yend)^2)
  
  # Figure out how many revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions
  
  # Calculate sequence of radians and x and y offset
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)
  
  # Create the new data
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}