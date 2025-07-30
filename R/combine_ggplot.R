#' Combine Two ggplot2 Plots with Consistent Style, Geom, and Axis Expansion
#'
#' This function overlays two ggplot2 plots of the same type (e.g., line, point, bar) into a single plot,
#' preserving the original geoms used in each layer (e.g., lines and points together). It automatically extracts
#' aesthetic mappings, axis expansions (including custom `expand` or limits), and theming from the first plot.
#' It also combines legends using a `SourceGroup` that distinguishes both the type and origin of the data.
#'
#' @param plot1 A \code{ggplot} object. Style, axis expansions, and theme are taken from this plot.
#' @param plot2 A second \code{ggplot} object to overlay on the first.
#' @param plot_names Optional character vector of length 2, naming the sources (defaults to \code{c("Plot 1", "Plot 2")}).
#'
#' @return A combined \code{ggplot} object with geoms, groups, and axis settings from both plots.
#'
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point geom_bar geom_col labs scale_y_continuous expand_limits ggplot_build
#' @importFrom rlang get_expr
#' @importFrom purrr map compact
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

  get_distinguishing_vars <- function(mapping) {
    aes_names <- c("group", "colour", "fill", "linetype")
    mapped <- purrr::map(aes_names, ~ get_mapped_col(mapping, .x)) %>%
      purrr::compact() %>%
      unlist(use.names = FALSE)
    unique(mapped)
  }

  mapping <- plot1$mapping
  distinguishing_vars <- get_distinguishing_vars(mapping)

  xcol <- get_mapped_col(mapping, "x")
  ycol <- get_mapped_col(mapping, "y")
  if (is.null(xcol) && length(plot1$layers) > 0) xcol <- get_mapped_col(plot1$layers[[1]]$mapping, "x")
  if (is.null(ycol) && length(plot1$layers) > 0) ycol <- get_mapped_col(plot1$layers[[1]]$mapping, "y")

  d1 <- plot1$data
  if ((is.null(d1) || nrow(d1) == 0) && length(plot1$layers) > 0) d1 <- plot1$layers[[1]]$data
  d2 <- plot2$data
  if ((is.null(d2) || nrow(d2) == 0) && length(plot2$layers) > 0) d2 <- plot2$layers[[1]]$data
  if (is.null(d1) | is.null(d2)) stop("Unable to extract data from one or both plots.")

  d1$Source <- plot_names[1]
  d2$Source <- plot_names[2]
  if (length(distinguishing_vars) > 0) {
    d1$SourceGroup <- do.call(interaction, c(d1[distinguishing_vars], list(d1$Source, drop = TRUE, sep = " - ")))
    d2$SourceGroup <- do.call(interaction, c(d2[distinguishing_vars], list(d2$Source, drop = TRUE, sep = " - ")))
  } else {
    d1$SourceGroup <- d1$Source
    d2$SourceGroup <- d2$Source
  }

  df <- rbind(d1, d2)

  split_levels <- strsplit(levels(df$SourceGroup), " - ", fixed = TRUE)
  labels <- vapply(split_levels, `[`, character(1), 1)
  sources <- vapply(split_levels, `[`, character(1), 2)
  new_levels <- levels(df$SourceGroup)[order(labels, sources)]
  df$SourceGroup <- factor(df$SourceGroup, levels = new_levels)

  mapping_args <- list(
    x = xcol,
    y = ycol,
    color = "SourceGroup",
    group = "SourceGroup"
  )
  aes_call <- do.call(ggplot2::aes_string, mapping_args)

  geom_map <- list(
    "GeomLine"   = ggplot2::geom_line,
    "GeomPoint"  = ggplot2::geom_point,
    "GeomBar"    = function(...) ggplot2::geom_bar(stat = "identity", position = "dodge", ...),
    "GeomCol"    = ggplot2::geom_col
  )

  p <- ggplot2::ggplot(df, aes_call)

  for (i in seq_along(list(plot1, plot2))) {
    plt <- list(plot1, plot2)[[i]]
    src <- plot_names[[i]]
    layers <- plt$layers

    for (layer in layers) {
      geom_class <- get_geom_type(layer)
      geom_fun <- geom_map[[geom_class]]
      if (is.null(geom_fun)) next

      data_layer <- layer$data
      if (is.null(data_layer) || nrow(data_layer) == 0) data_layer <- plt$data
      if (is.null(data_layer) || nrow(data_layer) == 0) next

      data_layer$Source <- src
      if (length(distinguishing_vars) > 0) {
        data_layer$SourceGroup <- do.call(interaction, c(data_layer[distinguishing_vars], list(data_layer$Source, drop = TRUE, sep = " - ")))
      } else {
        data_layer$SourceGroup <- data_layer$Source
      }
      split_vals <- strsplit(as.character(df$SourceGroup), " - ", fixed = TRUE)
      labels <- vapply(split_vals, `[`, character(1), 1)
      sources <- vapply(split_vals, `[`, character(1), 2)

      # Build globally ordered levels
      unique_combos <- unique(data.frame(label = labels, source = sources, combo = as.character(df$SourceGroup)))
      ordered_combos <- unique_combos[order(unique_combos$label, unique_combos$source), ]
      ordered_levels <- ordered_combos$combo

      # Apply to df and use these levels inside the loop later
      df$SourceGroup <- factor(df$SourceGroup, levels = ordered_levels)

      p <- p + geom_fun(
        data = data_layer,
        mapping = ggplot2::aes_string(
          x = xcol,
          y = ycol,
          colour = "SourceGroup",
          group = "SourceGroup"
        ),
        size = 1.2
      )
    }
  }

  p$data$SourceGroup <- factor(p$data$SourceGroup, levels = new_levels)

  if (!is.null(plot1$theme)) p <- p + plot1$theme

  p <- p + ggplot2::labs(
    title = plot1$labels$title,
    subtitle = plot1$labels$subtitle,
    x = plot1$labels$x,
    y = plot1$labels$y,
    caption = plot1$labels$caption
  ) +
    ggplot2::scale_color_discrete(limits = unique(p$data$SourceGroup))

  for (s in plot1$scales$scales) {
    if ("y" %in% s$aesthetics) p <- p + s
    if ("x" %in% s$aesthetics) p <- p + s
    if ("colour" %in% s$aesthetics) p <- p + s
    if ("fill" %in% s$aesthetics) p <- p + s
  }

  if (!is.null(plot1$coordinates$limits)) {
    if (!is.null(plot1$coordinates$limits$y)) {
      p <- p + ggplot2::expand_limits(y = plot1$coordinates$limits$y)
    }
    if (!is.null(plot1$coordinates$limits$x)) {
      p <- p + ggplot2::expand_limits(x = plot1$coordinates$limits$x)
    }
  }

  build <- ggplot2::ggplot_build(plot1)
  yscale_built <- build$layout$panel_params[[1]]$y$scale
  if (!any(vapply(plot1$scales$scales, function(s) "y" %in% s$aesthetics, logical(1)))) {
    expand_val <- tryCatch(yscale_built$get_expand(), error = function(e) NULL)
    lims_val <- tryCatch(yscale_built$get_limits(), error = function(e) NULL)
    if (!is.null(expand_val)) p <- p + ggplot2::scale_y_continuous(expand = expand_val)
    if (!is.null(lims_val) && any(!is.na(lims_val))) p <- p + ggplot2::scale_y_continuous(limits = lims_val)
  }

  p
}
