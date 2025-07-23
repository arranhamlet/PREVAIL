#' Combine Two ggplot2 Plots with Consistent Style, Geom, and Axis Expansion
#'
#' This function overlays two ggplot2 plots of the same type (e.g., line, point, bar) into a single plot,
#' automatically extracting aesthetic mappings, primary geom types, axis expansions (including custom `expand` or limits),
#' and other style elements from the first plot.
#'
#' It is fully generic for common plot types and preserves theming, labels, and color/fill scales. The output plot
#' displays both lines/bars/points and uses a `Source` variable for coloring.
#'
#' @param plot1 A ggplot2 plot object (e.g., from `ggplot()`). Style, axis expansion, and theme are taken from this plot.
#' @param plot2 A second ggplot2 plot object to overlay on the first.
#' @param plot_names Optional character vector of length 2 to label the two sources in the legend.
#'
#' @return A ggplot2 object combining the two plots, with consistent styling and axis expansions.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point geom_bar geom_col labs scale_y_continuous expand_limits ggplot_build
#' @importFrom rlang get_expr
#' @export
combine_ggplot <- function(plot1, plot2, plot_names = c("Plot 1", "Plot 2")) {

  get_mapped_col <- function(mapping, aesthetic) {
    aes_val <- mapping[[aesthetic]]
    if (is.null(aes_val)) return(NULL)
    if (inherits(aes_val, "name")) return(as.character(aes_val))
    if (inherits(aes_val, "call")) return(as.character(aes_val[[2]]))
    if (inherits(aes_val, "formula")) return(all.vars(aes_val))
    if (inherits(aes_val, "quosure")) return(as.character(rlang::get_expr(aes_val)))
    as.character(aes_val)
  }

  get_geom_type <- function(layer) {
    if (!is.null(layer$geom)) class(layer$geom)[1] else NULL
  }

  mapping <- plot1$mapping
  xcol <- get_mapped_col(mapping, "x")
  ycol <- get_mapped_col(mapping, "y")
  colorcol <- get_mapped_col(mapping, "colour")
  groupcol <- get_mapped_col(mapping, "group")
  if (is.null(xcol) && length(plot1$layers) > 0) xcol <- get_mapped_col(plot1$layers[[1]]$mapping, "x")
  if (is.null(ycol) && length(plot1$layers) > 0) ycol <- get_mapped_col(plot1$layers[[1]]$mapping, "y")
  if (is.null(colorcol) && length(plot1$layers) > 0) colorcol <- get_mapped_col(plot1$layers[[1]]$mapping, "colour")
  if (is.null(groupcol) && length(plot1$layers) > 0) groupcol <- get_mapped_col(plot1$layers[[1]]$mapping, "group")

  d1 <- plot1$data
  if ((is.null(d1) || nrow(d1) == 0) && length(plot1$layers) > 0) d1 <- plot1$layers[[1]]$data
  d2 <- plot2$data
  if ((is.null(d2) || nrow(d2) == 0) && length(plot2$layers) > 0) d2 <- plot2$layers[[1]]$data
  if (is.null(d1) | is.null(d2)) stop("Unable to extract data from one or both plots.")

  d1$Source <- plot_names[1]
  d2$Source <- plot_names[2]
  df <- rbind(d1, d2)

  mapping_args <- list(
    x = xcol,
    y = ycol,
    color = "Source",
    group = if (!is.null(groupcol)) groupcol else NULL
  )
  mapping_args <- mapping_args[!sapply(mapping_args, is.null)]
  aes_call <- do.call(ggplot2::aes_string, mapping_args)

  geom_map <- list(
    "GeomLine"   = ggplot2::geom_line,
    "GeomPoint"  = ggplot2::geom_point,
    "GeomBar"    = function(...) ggplot2::geom_bar(stat = "identity", position = "dodge", ...),
    "GeomCol"    = ggplot2::geom_col
  )
  geom1 <- if (length(plot1$layers) > 0) get_geom_type(plot1$layers[[1]]) else NULL
  geom2 <- if (length(plot2$layers) > 0) get_geom_type(plot2$layers[[1]]) else NULL
  geom1_fun <- geom_map[[geom1]]
  geom2_fun <- geom_map[[geom2]]

  p <- ggplot2::ggplot(df, aes_call)
  if (!is.null(geom1_fun) && !is.null(geom2_fun) && geom1 != geom2) {
    p <- p + geom1_fun(data = df[df$Source == plot_names[1], ], size = 1.2)
    p <- p + geom2_fun(data = df[df$Source == plot_names[2], ], size = 1.2, linetype = 2)
  } else if (!is.null(geom1_fun)) {
    p <- p + geom1_fun(size = 1.2)
  } else if (!is.null(geom2_fun)) {
    p <- p + geom2_fun(size = 1.2)
  } else {
    stop("Unable to automatically determine geom type. Please specify or add to geom_map.")
  }

  if (!is.null(plot1$theme)) p <- p + plot1$theme
  if (!is.null(plot1$labels$title))    p <- p + ggplot2::labs(title = plot1$labels$title)
  if (!is.null(plot1$labels$subtitle)) p <- p + ggplot2::labs(subtitle = plot1$labels$subtitle)
  if (!is.null(plot1$labels$x))        p <- p + ggplot2::labs(x = plot1$labels$x)
  if (!is.null(plot1$labels$y))        p <- p + ggplot2::labs(y = plot1$labels$y)
  if (!is.null(plot1$labels$caption))  p <- p + ggplot2::labs(caption = plot1$labels$caption)

  # 1. Apply all plot1 scales for y/x/color/fill (these may include expansion/limits)
  for (s in plot1$scales$scales) {
    if ("y" %in% s$aesthetics) p <- p + s
    if ("x" %in% s$aesthetics) p <- p + s
    if ("colour" %in% s$aesthetics) p <- p + s
    if ("fill" %in% s$aesthetics) p <- p + s
  }

  # 2. Add expand_limits if present (these are in plot1$coordinates$limits)
  if (!is.null(plot1$coordinates$limits)) {
    if (!is.null(plot1$coordinates$limits$y)) {
      p <- p + ggplot2::expand_limits(y = plot1$coordinates$limits$y)
    }
    if (!is.null(plot1$coordinates$limits$x)) {
      p <- p + ggplot2::expand_limits(x = plot1$coordinates$limits$x)
    }
  }

  # 3. If no custom y scale, recover expansion/limits from ggplot_build
  build <- ggplot2::ggplot_build(plot1)
  yscale_built <- build$layout$panel_params[[1]]$y$scale
  if (!any(vapply(plot1$scales$scales, function(s) "y" %in% s$aesthetics, logical(1)))) {
    expand_val <- tryCatch(yscale_built$get_expand(), error = function(e) NULL)
    lims_val <- tryCatch(yscale_built$get_limits(), error = function(e) NULL)
    if (!is.null(expand_val)) {
      p <- p + ggplot2::scale_y_continuous(expand = expand_val)
    }
    if (!is.null(lims_val) && any(!is.na(lims_val))) {
      p <- p + ggplot2::scale_y_continuous(limits = lims_val)
    }
  }

  p
}
