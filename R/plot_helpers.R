
#' @description
#' A list of the available colour brewer palettes available for heat maps.
#' @returns A character vector of available colour palette names.
#' @export
get_colour_palette_names <- function() {
  return(c("YlOrRd", "YlOrBl", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GbBu", "BuPu", "BuGn", "Blues", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"))
}
#' @description
#' A list of the available colour brewer palettes available for discrete plot colours.
#' @returns A character vector of available colour palette names.
#' @export
get_RColorBrewer_pal_names <- function() {
  return(c("Accent", "Dark2", "Paired", "Pastel1",  "Pastel2",  "Set1",  "Set2",  "Set3"))
}
#' @description
#' A list of the available viridis palettes available for discrete plot colours.
#' @returns A character vector of available colour palette names.
#' @export
get_viridis_palette_names <- function() {
  return(c("A", "B", "C", "D", "E", "F", "G", "H", "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"))
}

#' @description
#' Get the number of colours a colour palette contains to help sort bin counts in heatmaps.
#' @param brew_colour_name - the colour palette name
#' @returns An integer with the number of palette colours.
#' @export
get_palette_colour_count <- function(brew_colour_name) {
  stopifnot(brew_colour_name %in% get_colour_palette_names())
  return(RColorBrewer::brewer.pal.info[brew_colour_name,][["maxcolors"]])
}
#' @description
#' Get standard triad canvas
#' @returns The standard triad png file object
#' @export
get_triad_standard_canvas <- function() {
  # if the image is not in the global envirionment add it
  if (!exists("triad_standard_canvas", envir = .GlobalEnv)) {
    assign("triad_standard_canvas", png::readPNG("./data/triad_standard.png"), envir = .GlobalEnv)
  }
  return(triad_standard_canvas)
}

#' @description
#' Get triad zone labeled canvas as a useful annotation explaining zone labels.
#' @returns The triad zone labeled canvas
#' @export
get_triad_zone_canvas <- function() {
  # if the image is not in the global envirionment add it
  if (!exists("triad_zone_canvas", envir = .GlobalEnv)) {
    assign("triad_zone_canvas", png::readPNG("./data/zone_zone.png"), envir = .GlobalEnv)
  }
  return(triad_zone_canvas)
}

#' @description
#' Get zone demarcated triad canvas - used when slowing zone counts and percentages.
#' @returns The zone demarcated triad canvas
#' @export
get_triad_count_canvas <- function() {
  # if the image is not in the global envirionment add it
  if (!exists("triad_count_canvas", envir = .GlobalEnv)) {
    assign("triad_count_canvas", png::readPNG("./data/triad_zone.png"), envir = .GlobalEnv)
  }
  return(triad_count_canvas)
}

#' @description
#'  Plot a SenseMaker® defined triad with ggplot (note requires the sensemakerframeworkr object)
#' @param filtered_data - Must be supplied. Data frame that includes the triad x and y columns with filtered (if any) signifiers to plot.
#' @param full_data - Must be supplied. Data frame that includes the triad x and y columns with all signifiers for the capture.
#' @param sig_id - Must be supplied. The triad_id to be plotted.
#' @param framework_object - Must be supplied. The framework object from the sensemakerdatar instance object. (this is sensemakerdatar::sm_framework)
#' @param dot_size - default 0.6. The graph dot size.
#' @param dot_colour - default "black". The colour of the graph dots. A Character string of any valid R colour format, such as hex values or colour names.
#' @param dot_transparency - default 1. The transparency (or alpha) value for the dots. A numeric value between 0 and 1.
#' @param opaque_filtered -  default FALSE. If TRUE, the graph will display filtered out data as well as the current filtered data.
#' @param opaque_filter_dot_size - default 0.4. The size of the filtered out data displayed if opaque_filtered set to TRUE.
#' @param opaque_filter_dot_transparency - default 0.5. The transparency (or alpha) value for the filtered out data displayed if opaque_filtered set to TRUE. A numeric value between 0 and 1.
#' @param opaque_filter_dot_colour - default "blue". The colour of the filtered out data displayed if the opaque_filtered set to TRUE. A character string of any valid R colour format, such as hex values or colour names.
#' @param display_anchor_means - default FALSE. If TRUE, will display the anchor means with the anchor titles.
#' @param mean_type - default "geometric", otherwise "arithmetic".
#' @param show_percentages - default FALSE. If TRUE, the graph will replace dots with zone percentages.
#' @param show_totals - default FALSE. If TRUE, the graph will replace dots with zone counts. Both show_percentages and show_totals can be TRUE and then both will show.
#' @param percentage_type - default "Triad". If Triad, the percentages are calcuated based on the count within the triad, otherwise "Total", where the percentage is calculated on the total fragment count. Used if show_percentages or show_totals or both set to TRUE.
#' @param zone_font_size - default 4. The size of the zone count/percentage text if show_percentages or show_totals or both set to TRUE.
#' @param zone_display_colour - default "black". The colour of the zone count/percentage text if show_percentages or show_totals or both set to TRUE. A Character string of any valid R colour format, such as hex values or colour names.
#' @param zone_dots - default FALSE. If TRUE, display the triad dots as well as the zone percentages or counts or both. Used if show_percentages or show_totals or both set to TRUE.
#' @param zone_dot_transparency - default 0.25 the dot transparency in the zone display if zone_dots set to TRUE.
#' @param display_stats_caption - default TRUE. If TRUE, the caption area of the graph will display various counts and percentages relevant to the graph.
#' @param caption_size -  default 8. The size of the caption text if display_stats_caption set to TRUE.
#' @param caption_colour - default "black". The colour of the caption if the display_stats_caption set to TRUE. A Character string of any valid R colour format, such as hex values or colour names.
#' @param graph_title - default. NULL. A title for the graph. if NULL the triad title will be used.
#' @param title_colour - default "black". The colour of the graph title. A Character string of any valid R colour format, such as hex values or colour names.
#' @param title_size - default 12. The size of the graph title.
#' @param contours - default FALSE. If TRUE, probability contour lines will display in the graph. Uses a Gaussian Kernel Smoothing Density Estimation method.
#' @param contour_fill - default FALSE. If TRUE, a heat map is displayed in the graph.
#' @param fill_transparency - default 0.5. The transparency (alpha) value of the contour_fill if countour_fill set to TRUE.
#' @param fill_legend - default FALSE. Display the fill legend for the counter fill if contour_fill set to TRUE.
#' @param contour_size - default 0.5. The line size of the contour lines if contours set to TRUE.
#' @param contour_colour - default "blue". The colour of the contour lines if counters set to TRUE. A Character string of any valid R colour format, such as hex values or colour names.
#' @param brew_colour_select - default "Spectral". A character string with any valid RColorBrewer palette name. Use sensemakerhelpersr::get_colour_palette_names() to see all the palette names available
#' @param colour_sig_id - default NULL, don't colour by a list signifier, otherwise the signifier id of the list to colour by.
#' @param colour_vector - default NULL, a vector of valid R colour codes (alpha, hex etc.) of length the number of list items in the signifier id used for the colour.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer Set1 palette. values of "Dark2", "Set1", "Set2" or "Set3" recommended but others can also work. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param viridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param viridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param show_colour_legend - default TRUE, show the colour legend if colour by list (MCQ). FALSE to remove the legend.
#' @param legend_title_colour - default "black". The colour legend title. A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_title_size - default 8. The size of the legend title.
#' @param legend_text_colour - default "black". The colour legend text A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_text_size - default 8. The size of the legend text.
#' @returns A ggplot graph object of the triad.
#' @export
plot_triad <- function(filtered_data, full_data, sig_id, framework_object, dot_size = 0.6, dot_colour = "black",
                       dot_transparency = 1, opaque_filtered = FALSE, opaque_filter_dot_size = 0.4,
                       opaque_filter_dot_transparency = 0.5, opaque_filter_dot_colour = "blue",
                       display_anchor_means = FALSE, mean_type = "geometric", show_percentages = FALSE, show_totals = FALSE,  percentage_type = "Triad",
                       zone_font_size = 4, zone_display_colour = "black", zone_dots = FALSE, zone_dot_transparency = 0.25, display_stats_caption = TRUE, caption_size = 8, caption_colour = "black",
                       graph_title = NULL, title_colour = "black", title_size = 12, contours = FALSE, contour_fill = FALSE, fill_transparency = 0.5,
                       fill_legend = FALSE, contour_size = 0.5, contour_colour = "blue", brew_colour_select = "Spectral", colour_sig_id = NULL, colour_vector = NULL,
                       colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0, viridis_default_end = 0.6, show_colour_legend = TRUE,
                       legend_title_colour = "black", legend_title_size = 8, legend_text_colour = "black", legend_text_size = 8

) {
  # data frame (from CSV export) names for x and y values.
  x_column <- framework_object$get_triad_x_column_name(id = sig_id)
  y_column <- framework_object$get_triad_y_column_name(id = sig_id)
  # whether N/A allowed is set for this triad
  allow_na <- framework_object$get_signifier_allow_na(sig_id)

  # Get the anchor means.
  anchor_means <- calculate_triad_means(filtered_data, sig_id, mean_type, framework_object)
  # Anchor (Top, Left and Right) titles and calculate the size that would enable them to berst fit.
  anchor_titles <- get_anchor_plot_titles(sig_id, display_anchor_means, anchor_means, framework_object)
  anchor_size <- get_anchor_plot_size(anchor_titles)
  # pull out titles - makes code a little easier to read.
  left_title <- anchor_titles[["left_title"]]
  right_title <-  anchor_titles[["right_title"]]
  top_title <- anchor_titles[["top_title"]]
  # If a graph title hasn't been passed in, then use the triad title as graph title.
  if (is.null(graph_title)) {
    graph_title <- framework_object$get_signifier_title(sig_id)
  }
  # Calculate a size that will fit for this title.
  title_size <- get_graph_title_size(graph_title)
  # Caption values are the data counts.
  caption_values <- get_caption_values(filtered_data, full_data, sig_id, framework_object)
  # Start the plot with what we wil always do.
  p <- ggplot2::ggplot(filtered_data, ggplot2::aes(x = .data[[x_column]], y = .data[[y_column]])) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                   panel.background= ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(limits=c(-.2, 1), breaks = seq(0,.866,by=.866)) +
    ggplot2::scale_x_continuous(limits=c(-.2, 1.2),breaks = seq(0,1,by=1)) + ggplot2::coord_fixed() +
    ggplot2::annotate("text", label = c(top_title, left_title , right_title), x =  c(.5, -.2, 1.2), y = c(1.00, -.1, -.1), hjust = c(.5, 0, 1), vjust = c(0,1,1), size= anchor_size, colour = title_colour) +
    ggplot2::labs(title = paste(wrap_text(graph_title, tlength = 56), "\n", sep = "")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size= title_size, face="bold.italic", hjust = 0.5, colour = title_colour))


  # we are doing normal dot plot (that is not zone counts or percentages)
  if (!show_percentages & !show_totals) {
    p <- p + ggplot2::annotation_custom(grid::rasterGrob(get_triad_standard_canvas(), width = ggplot2::unit(1.1, "npc"), height = ggplot2::unit(1.1, "npc")), 0.02, .98, 0.025, .866)
    # Do the point geom - i.e. plot the dots with passed colours, size and transparency plus the background image.
    # if we are not doing list colouring
    if (is.null(colour_sig_id)) {
      p <- p + ggplot2::geom_point(size = dot_size, colour = dot_colour, alpha = dot_transparency)

    } else {
      # we are doing colouring by an MCQ (list)
      # use the colour vector if it is provided
      if (!is.null(colour_vector)) {
        p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes(colour = factor(!! sym(colour_sig_id)))) +
          scale_color_manual(values = colour_vector,
                             breaks = framework_object$get_list_items_ids(colour_sig_id),
                             labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
      } else {
        # nothing provided for colours so use a palette
        if (colour_package == "viridis") {
          p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes(colour = factor(!! sym(colour_sig_id)))) +
            viridis::scale_color_viridis(discrete = TRUE, option = package_palette, direction = colour_direction, begin = viridis_default_start, end = viridis_default_end,
                                         breaks = framework_object$get_list_items_ids(colour_sig_id),
                                         labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
        } else {
          if (colour_package == "RColorBrewer") {
            p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes(colour = factor(!! sym(colour_sig_id)))) +
              scale_color_brewer(palette = package_palette, direction = colour_direction,
                                 breaks = framework_object$get_list_items_ids(colour_sig_id),
                                 labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
          }
        }
      }
      p <- p +  theme(legend.title = element_text(color = legend_title_colour, size = legend_title_size), legend.text = element_text(color = legend_text_colour, size = legend_text_size))
      if (!show_colour_legend) {
        p <- p + ggplot2::theme(legend.position = "none")
      }
    }
    # opaque the dots not in the filter - this allows for a filtered plot to contain all the dots, the filtered out dots can have a different size and opacity and colour.
    if (opaque_filtered) {
      p <- p + ggplot2::geom_point(data = full_data[!full_data[["FragmentID"]] %in% filtered_data[["FragmentID"]],], size= opaque_filter_dot_size, colour = opaque_filter_dot_colour,  alpha = opaque_filter_dot_transparency)
    }

    # if contour and/or contour fill requested then process the stat density plots.
    # contour lines without fil.
    if (contours & !contour_fill) {
      p <- p + ggplot2::geom_density_2d(colour = contour_colour, size = contour_size, bins = 12)
    }
    # contour fill (which might have lines)
    if (contour_fill) {
      # We set the number of bins in the kernel function to match the number of colours available in the palette.
      fill_bins <- get_palette_colour_count(brew_colour_select)
      p <- p +  ggplot2::stat_density_2d(geom = "polygon", contour = TRUE,
                                         ggplot2::aes(fill = ggplot2::after_stat(level)), alpha = fill_transparency, colour = contour_colour, size = contour_size,
                                         bins = fill_bins, show.legend = fill_legend) +
        ggplot2::scale_fill_distiller(palette =  RColorBrewer::brew_colour_select, direction = -1)
    }

  } else {
    # We have a percentage or count plot.
    # Use a background image that has the zones more clearly marked.
    p <- p + ggplot2::annotation_custom(grid::rasterGrob(get_triad_count_canvas(), width = ggplot2::unit(1.1, "npc"), height = ggplot2::unit(1.1, "npc")), 0.02, .98, 0.025, .866)
    # Set up a temporary data frame as we are going to play with the levels - this is legacy and could be fixed to avoid.
    # todo - alter the order of xVal and yVal to match the default levels or alter the default level calculation when the zone data is calculated.
    filtered_data_temp <- filtered_data
    filtered_data_temp[[framework_object$get_triad_zone_name(sig_id)]] <-
      factor(filtered_data_temp[[framework_object$get_triad_zone_name(sig_id)]],
             levels = c("T", "L", "R", "LT", "TR", "LR", "Centre"), labels = c("T", "L", "R", "LT", "TR", "LR", "Centre"), ordered =  TRUE)
    # Group by the zones calculating the totals
    totals <- filtered_data_temp %>%  dplyr::group_by(.data[[framework_object$get_triad_zone_name(sig_id)]]) %>% dplyr::summarise(count = dplyr::n()) %>% tidyr::drop_na()
    colnames(totals) <- c("zone", "count")

    # Calculate the percentages - will be either based on the Total data count or Triad dots (number of actual dots displayed so excluding NA and non answered)
    # todo - we might save some processing time with a big data count if we only do this if the percentages are calculated - another if statement!
    if (percentage_type == "Total") {
      totals <- totals %>% dplyr::mutate(percentage = round((count / caption_values[["N"]]) * 100, digits = 0))
    } else {
      totals <- totals %>% dplyr::mutate(percentage = round((count / (caption_values[["numDataPointsMu"]] - caption_values[["numNonEntries"]])) * 100, digits = 0))
    }
    # we need to align the counts, percentages or both into the zones, slightly different if both.
    show_both <- FALSE
    if (show_totals & show_percentages) {
      show_both <- TRUE
    }
    if (!show_both) {
      # positions in vector are A, B, C, AB, BC, CA, Centre
      xVal <- c(.18, 0.48, 0.80, 0.31, 0.68, 0.48, 0.50)
      yVal <- c(0.13, 0.67, 0.13, 0.41, 0.39, 0.11, 0.32 )
      if (show_percentages) {
        labelVal <- paste0(totals[["percentage"]], "%")
      }
      if (show_totals) {
        labelVal <- totals[["count"]]
      }
      # Plot the counts or percentages depending which set.
      p <- p + ggplot2::annotate(geom = "text", x = xVal, y = yVal, label = labelVal, colour = zone_display_colour, size = zone_font_size)

    } else {
      # Plot both as both set
      xValC <- c(0.18, 0.50, 0.80, 0.31, 0.68, 0.48, 0.48)
      yValC <- c(0.15, 0.70, 0.15, 0.41, 0.39, 0.13, 0.34)

      xValP <- c(0.20, 0.50, 0.83, 0.31, 0.70, 0.50, 0.50)
      yValP <- c(0.06, 0.64, 0.06, 0.35, 0.33, 0.04, 0.28)

      p <- p + ggplot2::annotate(geom = "text", x = xValC, y = yValC, label = totals[["count"]], colour = zone_display_colour, size = zone_font_size)
      p <- p + ggplot2::annotate(geom = "text", x = xValP, y = yValP, label = paste0(totals[["percentage"]], "%"), colour = zone_display_colour, size = zone_font_size)

    }
    # If plotting the dots with the zones has been set, then plot the dots too
    if (zone_dots) {
      p <- p + ggplot2::geom_point(size = dot_size, colour = dot_colour, alpha = zone_dot_transparency)
    }

  }


  # Display the caption (counts) line at the bottom of the graph if this has been selected.
  if (display_stats_caption) {

    p <- p + ggplot2::labs(caption  = paste0("N = ", caption_values[["N"]], " n = ", caption_values[["numDataPointsMu"]],
                                             ifelse(allow_na, paste0("  nN/A = ", caption_values[["numNADataPointsMu"]], ""), ""),
                                             ifelse(caption_values[["numNonEntries"]] >0, paste("  Skipped = ", caption_values[["numNonEntries"]]), ""), "  filter n = ",
                                             caption_values[["numDataPoints"]], "  %age = ", paste0(caption_values[["perToData"]], "%"),
                                             ifelse(allow_na, paste0("  filter N/A = ", caption_values[["numNADataPoints"]]), ""),
                                             " ",  "\U003BC",  " = L:", anchor_means[["left_mean"]], " T: ", anchor_means[["top_mean"]], " R: ",
                                             anchor_means[["right_mean"]])) +
      ggplot2::theme(plot.caption = ggplot2::element_text(family = "Times", size = caption_size, colour = caption_colour))

  }
  return(p)
}

#' @description
#' Plot triad means of passed in data frame data and confidence intervals for comparison - requires the sensemakerframeworkr object
#'  description
#' @param filtered_data - The dataframe of data to print.
#' @param triad_id - The triad to be plotted.
#' @param list_id - The signifier id of the list to have printed.
#' @param framework_object - The framework object pertaining to the collection.
#' @param list_item_ids - default NULL, list item ids for the list_id. If NULL all the items are plotted.
#' @param colour_vector - default NULL, if provided, a vector of the same length as df_list with the colours to plot each of the data frames passed.
#' @param show_mean - default TRUE, display the mean or not of the data.
#' @param mean_colour - default NULL, if provided, a vector of the same length as df_list with the colours for the means. If NULL, same colour used as in colour_vector.
#' @param mean_shape - default NULL, if provided, a vector of the same length as df_list, with the shapes of the means. If NULL, then "circle" used.
#' @param mean_zero_logic - default "small_value" otherwise "remove" can be specified to remove NA records.
#' @param mean_size - default 2, the dot size of the mean.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer palette, use sensemakerhelpersr::get_RColorBrewer_pal_names() to get a list of RColorBrewer names. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param viridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param viridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param legend_title_size - default 8. The size of the legend title.
#' @param legend_text_colour - default "black". The colour legend text A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_text_size - default 8. The size of the legend text.
#' @param dot_size - default 0.5, the size of the graph dots
#' @param dot_transparency - default 1, the alpha (transparency value of the dits)
#' @param show_confidence_intervals - default TRUE, display the confidence intervals.
#' @param confidence_value - default 0.05 - the confidence interval to draw around the mean.
#' @param confidence_size - default 2, the size of the confidence line
#' @param title_colour - default "black", the title colour, a character string of any valid R colour format, such as hex values or colour names.
#' @param title_size - default 12, the size of the graph title.
#' @param anchor_colour - default "black" the colour of the anchor titles, a character string of any valid R colour format, such as hex values or colour names.
#' @param anchor_size - default 10, the size of the anchor titles.
#' @returns A ggplot graph object of the triad.
#' @export
plot_tern_means_by_list_id <- function(filtered_data, triad_id, list_id, framework_object, list_item_ids = NULL, colour_vector = NULL, show_mean = TRUE, mean_colour = NULL,
                                       mean_shape = NULL, mean_zero_logic = "small_value",
                                       mean_size = 2, colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0,
                                       viridis_default_end = 0.6, show_colour_legend = TRUE, legend_title_colour = "black", legend_title_size = 8, legend_text_colour = "black",
                                       legend_text_size = 8,  dot_size = 0.5, dot_transparency = 1, show_confidence_intervals = TRUE, confidence_value = 0.05, confidence_size = 2,
                                       title_colour = "black", title_size = 12,
                                       anchor_colour = "black", anchor_size = 10) {
  stopifnot(!is.null(colour_package))
  stopifnot(length(colour_package) == 1)
  stopifnot(colour_package %in% c("RColorBrewer", "viridis"))
  if (colour_package == "RColorBrewer") {
    stopifnot(colour_package == "RColorBrewer" & package_palette %in% get_RColorBrewer_pal_names())
  }
  if (colour_package == "viridis") {
    stopifnot(colour_package == "viridis" & package_palette %in% get_viridis_palette_names())
  }
  stopifnot(all(class(framework_object) %in% c("Signifiers", "R6")))
  stopifnot(list_id %in% framework_object$get_single_select_list_ids())


  # if the list item ids are not passed then get them
  if (!is.null(list_item_ids)) {
    stopifnot(all(list_item_ids %in% framework_object$get_list_items_ids(list_id)))
  } else {
    list_item_ids <- framework_object$get_list_items_ids(list_id)
  }

  df_list <- purrr::map(list_item_ids, function(item_id) {
    (filtered_data %>% dplyr::filter(!! sym(list_id) == item_id))
  })
  data_titles <-  unlist(purrr::map(list_item_ids, ~ {framework_object$get_list_item_title(list_id, .x)}))
  names(df_list) <-data_titles

  r <- plot_tern_means(df_list, triad_id, data_titles, framework_object,  colour_vector, show_mean, mean_colour, mean_shape, mean_zero_logic, mean_size, colour_package, package_palette, colour_direction,
                       viridis_default_start, viridis_default_end, show_colour_legend, legend_title_colour, legend_title_size, legend_text_colour, legend_text_size, dot_size,
                       dot_transparency, show_confidence_intervals, confidence_value,
                       confidence_size, title_colour, title_colour)
  return(r)

}

#' @description
#' Plot triad means of passed in data frame data and confidence intervals for comparison - requires the sensemakerframeworkr object
#'  description
#' @param df_list - A named list of filtered data frames containing the same capture data.
#' @param triad_id - The triad to be plotted.
#' @param data_titles - A list of the same length as the df_list giving titles for the data plot
#' @param framework_object - The framework object pertaining to the collection.
#' @param colour_vector - default NULL, if provided, a vector of the same length as df_list with the colours to plot each of the data frames passed.
#' @param show_mean - default TRUE, display the mean or not of the data.
#' @param mean_colour - default NULL, if provided, a vector of the same length as df_list with the colours for the means. If NULL, same colour used as in colour_vector.
#' @param mean_shape - default NULL, if provided, a vector of the same length as df_list, with the shapes of the means. If NULL, then "circle" used.
#' @param mean_zero_logic - default "small_value" otherwise "remove" can be specified to remove NA records.
#' @param mean_size - default 2, the dot size of the mean.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer palette, use sensemakerhelpersr::get_RColorBrewer_pal_names() to get a list of RColorBrewer names. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param viridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param viridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param legend_title_size - default 8. The size of the legend title.
#' @param legend_text_colour - default "black". The colour legend text A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_text_size - default 8. The size of the legend text.
#' @param dot_size - default 0.5, the size of the graph dots
#' @param dot_transparency - default 1, the alpha (transparency value of the dits)
#' @param show_confidence_intervals - default TRUE, whether to show the confidence intervals.
#' @param confidence_value - default 0.05 - the confidence interval to draw around the mean.
#' @param confidence_size - default 2, the size of the confidence line
#' @param title_colour - default "black", the title colour, a character string of any valid R colour format, such as hex values or colour names.
#' @param title_size - default 12, the size of the graph title.
#' @param anchor_colour - default "black" the colour of the anchor titles, a character string of any valid R colour format, such as hex values or colour names.
#' @param anchor_size - default 10, the size of the anchor titles.
#' @returns A ggplot graph object of the triad.
#' @export
plot_tern_means <- function(df_list, triad_id, data_titles, framework_object, colour_vector = NULL, show_mean = TRUE, mean_colour = NULL, mean_shape = NULL, mean_zero_logic = "small_value",
                            mean_size = 2, colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0,
                            viridis_default_end = 0.6, show_colour_legend = TRUE, legend_title_colour = "black", legend_title_size = 8, legend_text_colour = "black",
                            legend_text_size = 8,  dot_size = 0.5, dot_transparency = 1, show_confidence_intervals = TRUE, confidence_value = 0.05, confidence_size = 2, title_colour = "black", title_size = 12,
                            anchor_colour = "black", anchor_size = 10) {


  # there must be more than one entry to make a comparison.
  stopifnot(length(data_titles) > 1)
  stopifnot(length(df_list) > 1)
  # The length of the passed in parameters must be the same
  stopifnot(length(df_list) == length(data_titles))
  if (!is.null(colour_vector)) {
    stopifnot(length(colour_vector) == length(data_titles))
  }

  # if the colour array empty then get colour array to be the brewer etc.

  p <- ggtern::ggtern()

  col_names <- fwd$sm_framework$get_triad_compositional_column_names(triad_id)
  title <- clean_string_of_html(str_replace_all(framework_object$get_signifier_title(triad_id), "&amp;", "&"))
  leftTitle <-  clean_string_of_html(str_replace_all(framework_object$get_triad_left_anchor_text(triad_id), "&amp;", "&"))
  rightTitle <-  clean_string_of_html(str_replace_all(framework_object$get_triad_right_anchor_text(triad_id), "&amp;", "&"))
  topTitle <-    clean_string_of_html(str_replace_all(framework_object$get_triad_top_anchor_text(triad_id), "&amp;", "&"))

  if (is.null(colour_vector)) {

    if (colour_package == "RColorBrewer") {
      colour_vector <- RColorBrewer::brewer.pal(n = length(df_list), name = package_palette)
    } else {
      if (colour_package == "viridis") {
        viridis_object <- viridis::scale_colour_viridis(alpha = 1, begin = viridis_default_start, end = viridis_default_end, direction = colour_direction, discrete = TRUE, option = package_palette)
        colour_vector <- viridis_object$palette(n = length(df_list))
      }
    }

  }
  if (is.null(mean_colour)) {
    mean_colour <- colour_vector
  }

  for (i in seq_along(data_titles)) {

    plot_data <- df_list[[i]] %>% dplyr::filter(!is.na(!! sym(col_names[["left"]]))) %>% dplyr::filter(!! sym(framework_object$get_triad_left_column_name(triad_id)) > 0) %>%
      dplyr::filter(!! sym(framework_object$get_triad_top_column_name(triad_id)) > 0) %>% dplyr::filter(!! sym(framework_object$get_triad_right_column_name(triad_id)) > 0)
    # calculate the mean
    data_means <- calculate_triad_means(plot_data, triad_id, "geometric", framework_object, zero_logic = mean_zero_logic, for_ggtern = TRUE)

    plot_data[["col_by"]] <- rep_len(data_titles[[i]], length.out = nrow(plot_data))

    p <- p + geom_point(data = plot_data, aes(x = .data[[framework_object$get_triad_left_column_name(triad_id)]],
                                              y = .data[[framework_object$get_triad_top_column_name(triad_id)]],
                                              z = .data[[framework_object$get_triad_right_column_name(triad_id)]], colour = col_by), size = dot_size, alpha = dot_transparency)
    if (show_mean) {
      p <- p + geom_point(data = data_means, aes(x = x, y = y, z = z), colour = mean_colour[[i]], size = mean_size, shape =
                            ifelse(is.null(mean_shape), "circle", mean_shape[[i]]), fill = ifelse(!is.null(mean_colour), mean_colour[[i]],  colour_vector[[i]]))
    }
    if (show_confidence_intervals) {
      p <- p +   ggtern::geom_confidence_tern(data = plot_data, aes(x = .data[[framework_object$get_triad_left_column_name(triad_id)]],
                                                                    y = .data[[framework_object$get_triad_top_column_name(triad_id)]],
                                                                    z = .data[[framework_object$get_triad_right_column_name(triad_id)]], colour = col_by), breaks = confidence_value, size = confidence_size)
    }


  }


  p <- p +  labs(title = title) +
    ggtern::Llab(wrap_text(leftTitle, tlength = 45)) +
    ggtern::Tlab(wrap_text(topTitle, tlength = 75)) +
    ggtern::Rlab(wrap_text(rightTitle, tlength = 45))  +
    theme(plot.title = element_text(colour = title_colour, size = title_size, family = "Helvetica", hjust = 0.5)) +
    theme(tern.axis.title.L = element_text(hjust=0, vjust=1, colour = anchor_colour, size = anchor_size, family = "Helvetica")) +
    theme(tern.axis.title.T = element_text(colour = anchor_colour, size = anchor_size, family = "Helvetica"))  +
    theme(tern.axis.title.R = element_text(hjust=1, vjust=1, colour = anchor_colour, size = anchor_size, family = "Helvetica")) +
    theme(legend.title = element_text(color = legend_title_colour, size = legend_title_size), legend.text = element_text(color = legend_text_colour, size = legend_text_size)) +
    ggtern::theme_nogrid() +
    ggtern::theme_hidelabels() +
    ggtern::theme_hideticks() +
    ggplot2::theme(tern.panel.expand = 0.40)  +
    ggplot2::theme(plot.margin = margin(0, 0, 0, 0, "cm")) + theme(axis.title.x = element_blank(),  axis.title.y = element_blank()) +
    ggplot2::scale_color_manual(name='Mean Intervals',
                                breaks= data_titles,
                                values= colour_vector)

  if (!show_colour_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)

}



#' @description
#' Plot triad using ggtern
#' @param filtered_data - Must be supplied. Data frame that includes the triad x and y columns with filtered (if any) signifiers to plot.
#' @param full_data - Must be supplied. Data frame that includes the triad x and y columns with all signifiers for the capture.
#' @param triad_id - Must be supplied. The triad_id to be plotted.
#' @param framework_object - Must be supplied. The framework object from the sensemakerdatar instance object. (this is sensemakerdatar::sm_framework)
#' @param dot_size - default 0.5, the size of the graph dots
#' @param dot_colour - default black, the dot colour.
#' @param dot_transparency - default 1, the transparency (alpha value) of the dots. Numeric between 0 and 1.
#' @param opaque_filtered -  default FALSE. If TRUE, the graph will display filtered out data as well as the current filtered data.
#' @param opaque_filter_dot_size - default 0.4. The size of the filtered out data displayed if opaque_filtered set to TRUE.
#' @param opaque_filter_dot_transparency - default 0.5. The transparency (or alpha) value for the filtered out data displayed if opaque_filtered set to TRUE. A numeric value between 0 and 1.
#' @param opaque_filter_dot_colour - default "blue". The colour of the filtered out data displayed if the opaque_filtered set to TRUE. A character string of any valid R colour format, such as hex values or colour names.
#' @param anchor_label_size - default "default" otherwise a numeric value to specify the size of the anchor labels. If "default" the size is calculated based on the length of the text.
#' @param anchor_label_size_print - default "default" otherwise a numeric value to specify the size of the anchor labels on a print. If "default" the size is calculated based on the length of the text.
#' @param show_mean - default TRUE, otherwse FALSE. If TRUE, displays the geometric mean in the ternary graph.
#' @param mean_size - default 2, the dot size of the mean.
#' @param mean_colour - default "blue", the colour of the mean if displayed.
#' @param mean_shape - default "circle", otherwise "diamond", the shope of the mean being displayed.
#' @param mean_zero_logic - default "small_value" otherwise "remove" can be specified to remove NA records. "small_value" sets zero anchors to a very small value.
#' @param graph_title - default NULL, if NULL the signifier title is used for the graph title, otherwise the value passed here.
#' @param title_colour - default black, The colour of the graph title. A Character string of any valid R colour format, such as hex values or colour names.
#' @param title_size - default 12, the size of the graph title.
#' @param show_confidence_intervals = default FALSE, if TRUE, show confidence intervals in the graph.
#' @param confidence_interval_colour - default blue, a Character string of any valid R colour format, such as hex values or colour names.
#' @param confidence_interval_Size - default 0.55, the size of the confidence interval lines.
#' @param confidence_intervals - default 0.05, a vector of confidence intervals to display with each value between 0 and 1.
#' @param show_variance - default FALSE, if TRUE, show the variance around the mean as a shaded area.
#' @param variance_colour - default hex "#08E5F5",  a character string of any valid R colour format, such as hex values or colour names.
#' @param variance_transparency - default 0.3, the transparency (alpha value) of the variance shaded area. A value between 0 and 1.
#' @param show_contours - default FALSE, if TRUE show contour lines based on kernel smoothing function.
#' @param countour_size - default 0.5, the size of the contour lines.
#' @param contour_colour - defaykt "blue", the colour of the contour lines, a character string of any valid R colour format, such as hex values or colour names.
#' @param show_contour_fill - default FALSE, show heat map, or filled contour based on kernel smoothing function.
#' @param fill_transparency - default 0.5, the transparency (alpha value) of the contour fill regions.
#' @param show_fill_legend - default FALSE, if TRUE, show the legend with the probability values for the contour/fill levels.
#' @param fill_brewer_palette - default "Spectral", the RColorBrewer palette used to colour the contour fills. Use sensemakerhelpersr::get_colour_palette_names() to see all the palette names available.
#' @param colour_sig_id - default NULL, A single select list (multi-choice question) id to use as a colour aesthetic in the graph.
#' @param colour_vector - default NULL, a vector of length the number of list items providing the colours to use for each item in the colour_sig_id colouring.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer Set1 palette. values of "Dark2", "Set1", "Set2" or "Set3" recommended but others can also work. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param viridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param viridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "viridis" selected as the colour package.
#' @param show_colour_legend - default TRUE, show the colour legend.
#' @param legend_title_colour - default "black", the colour of the colour legend title, a character string of any valid R colour format, such as hex values or colour names.
#' @param legend_title_size - default 8. The size of the colour legend title.
#' @param legend_text_colour - default "black". The colour legend text A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_text_size - default 8. The size of the legend text.
#' @param return_as_print - default FALSE, returns the ggtern object otherwise a print of the object.
#' @returns A ggtern graph object of the triad either as a plot object or printed object (depending on the return_as_print parameter value).
#' @export
plot_tern_triad <- function(filtered_data, full_data, triad_id, framework_object, dot_size = 0.5, dot_colour = "black", dot_transparency = 1, opaque_filtered = FALSE, opaque_filter_dot_size = 0.4,
                            opaque_filter_dot_transparency = 0.5, opaque_filter_dot_colour = "blue", anchor_label_size = "default", anchor_label_size_print = "default",
                            show_mean = TRUE, mean_size = 2, mean_colour = "blue", mean_shape = "circle", mean_zero_logic = "small_value", graph_title = NULL, title_colour = "black", title_size = 12,
                            show_confidence_intervals = FALSE, confidence_interval_colour = "blue", confidence_interval_Size = 0.55, confidence_intervals = 0.05, show_variance = FALSE, variance_colour = '#08E5F5',
                            variance_transparency = 0.3, show_contours = FALSE, countour_size = 0.5, contour_colour = "blue", show_contour_fill = FALSE, fill_transparency = 0.5,
                            show_fill_legend = FALSE, fill_brewer_palette = "Spectral", colour_sig_id = NULL, colour_vector = NULL, colour_package = "RColorBrewer", package_palette = "Set1",
                            colour_direction = 1, viridis_default_start = 0, viridis_default_end = 0.6, show_colour_legend = TRUE, legend_title_colour = "black", legend_title_size = 8,
                            legend_text_colour = "black", legend_text_size = 8, return_as_print = FALSE) {
  #
  # get column names
  col_names <- framework_object$get_triad_compositional_column_names(triad_id)

  # Get the anchor means.
  # remove NA from the data and any values < 0 (extremely rare but some legacy)
  data_clean <- filtered_data %>% dplyr::filter(!is.na(!! sym(col_names[["left"]]))) %>% dplyr::filter(!! sym(framework_object$get_triad_left_column_name(triad_id)) > 0) %>%
   dplyr::filter(!! sym(framework_object$get_triad_top_column_name(triad_id)) > 0) %>% dplyr::filter(!! sym(framework_object$get_triad_right_column_name(triad_id)) > 0)

  #full_data_no_na <- full_data %>% dplyr::filter(!is.na(!! sym(col_names[["left"]]))) %>% dplyr::filter(!! sym(framework_object$get_triad_left_column_name(triad_id)) > 0) %>%
  #  dplyr::filter(!! sym(framework_object$get_triad_top_column_name(triad_id)) > 0) %>% dplyr::filter(!! sym(framework_object$get_triad_right_column_name(triad_id)) > 0)

  anchor_means <- calculate_triad_means(data_clean, triad_id, "geometric", framework_object, zero_logic = mean_zero_logic)
  mean_df <- data.frame(x = anchor_means[["left_mean"]], y = anchor_means[["top_mean"]], z = anchor_means[["right_mean"]])

  if (is.null(graph_title)) {
    graph_title <- wrap_text(framework_object$get_signifier_title(triad_id), tlength = 75)
  }
  leftTitle <-  clean_string_of_html(str_replace_all(framework_object$get_triad_left_anchor_text(triad_id), "&amp;", "&"))
  rightTitle <-  clean_string_of_html(str_replace_all(framework_object$get_triad_right_anchor_text(triad_id), "&amp;", "&"))
  topTitle <-    clean_string_of_html(str_replace_all(framework_object$get_triad_top_anchor_text(triad_id), "&amp;", "&"))

  # use auto layout if no manual layout commands
  titles <- wrap_tern_title(leftTitle, rightTitle)
  leftTitle <- titles[["left_title"]]
  rightTitle <- titles[["right_title"]]


  left_column_name <- col_names$left
  top_column_name <-  col_names$top
  right_column_name <- col_names$right


  font_size <- get_tern_title_size(leftTitle, rightTitle, anchor_label_size, anchor_label_size_print, return_as_print)

  p <- ggtern::ggtern(data = data_clean, ggplot2::aes_string(x = paste0("`", left_column_name, "`"),
                      y = paste0("`", top_column_name, "`"),
                      z = paste0("`", right_column_name, "`")))

  if (is.null(colour_sig_id)) {
    p <- p + ggplot2::geom_point(colour = dot_colour, size = dot_size, alpha = dot_transparency)
  } else {
    # we are doing colouring by an MCQ (list)
    # use the colour vector if it is provided
    if (!is.null(colour_vector)) {
      p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes_string( colour = paste0("`", colour_sig_id, "`"))) +
        ggplot2::scale_color_manual(values = colour_vector,
                           breaks = framework_object$get_list_items_ids(colour_sig_id),
                           labels = framework_object$get_list_items_titles(colour_sig_id)) +
        ggplot2::labs(color = framework_object$get_signifier_title(colour_sig_id))
    } else {
      # nothing provided for colours so use a palette
      if (colour_package == "viridis") {
        p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes_string( colour = paste0("`", colour_sig_id, "`"))) +
          viridis::scale_color_viridis(discrete = TRUE, option = package_palette, direction = colour_direction, begin = viridis_default_start, end = viridis_default_end,
                                       breaks = framework_object$get_list_items_ids(colour_sig_id),
                                       labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
      } else {
        if (colour_package == "RColorBrewer") {

          p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes_string( colour = paste0("`", colour_sig_id, "`"))) +
            ggplot2::scale_color_brewer(palette = package_palette, direction = colour_direction,
                               breaks = framework_object$get_list_items_ids(colour_sig_id),
                               labels = framework_object$get_list_items_titles(colour_sig_id)) +
            ggplot2::labs(color = framework_object$get_signifier_title(colour_sig_id))
        }
      }
    }
    p <- p +  ggplot2::theme(legend.title = element_text(color = legend_title_colour, size = legend_title_size), legend.text = element_text(color = legend_text_colour, size = legend_text_size))

    if (!show_colour_legend) {
      p <- p + ggplot2::theme(legend.position = "none")
    }
  }

  p <- p + ggplot2::labs(title = graph_title) +
    ggtern::Llab(wrap_text(leftTitle, tlength = 45)) +
    ggtern::Tlab(wrap_text(topTitle, tlength = 75)) +
    ggtern::Rlab(wrap_text(rightTitle, tlength = 45)) +
    theme(plot.title = element_text(colour = title_colour, size = title_size, family = "Helvetica", hjust = 0.5)) +
    theme(tern.axis.title.L = element_text(hjust=0, vjust=1, colour = "black", size = font_size, family = "Helvetica")) +
    theme(tern.axis.title.T = element_text(colour = "black", size = font_size, family = "Helvetica"))  +
    theme(tern.axis.title.R = element_text(hjust=1, vjust=1, colour = "black", size = font_size, family = "Helvetica")) +
    theme_nogrid() +
    theme_hidelabels() +
    theme_hideticks() +
    theme(tern.panel.expand = 0.40)  +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) + theme(axis.title.x = element_blank(),  axis.title.y = element_blank())

  if (show_mean) {
    p <- p + geom_point(data = mean_df, aes(x = x, y = y, z = z), colour = mean_colour, size = mean_size, shape = ifelse(mean_shape == "circle", 21, 23), fill = mean_colour)
  }

  if (show_confidence_intervals) {
    p <- p + ggtern::geom_confidence_tern(breaks = confidence_intervals, size = confidence_interval_Size, colour = confidence_interval_colour)
  }

  if (show_variance) {
    p <- p + ggtern::stat_mean_ellipse(geom='polygon', steps=500, fill= variance_colour, alpha = variance_transparency)
  }


  if (show_contour_fill) {
    if (nrow(data_clean) > 10) {
      fill_bins <- get_palette_colour_count(fill_brewer_palette)
      p <- p + ggtern::stat_density_tern(geom = 'polygon', contour = TRUE,
                                 n  = 200, bdl = .0450,
                                 ggplot2::aes_string(z = paste0("`", left_column_name, "`"),
                                            x = paste0("`", top_column_name, "`"),
                                            y = paste0("`", right_column_name, "`"), fill  = "..level.."),
                             #   ggtern::aes(x = .data[[left_column]], y = .data[[top_column]], z = .data[[right_column]], fill = ggplot2::after_stat(level)),
                                 alpha = fill_transparency, colour = contour_colour, size = countour_size,
                                 bins = fill_bins, show.legend = show_fill_legend) +
        ggplot2::scale_fill_distiller(palette = fill_brewer_palette, direction = -1)
    }
  }

  if (show_contours) {
    if (nrow(data_clean) > 10) {
      if (!show_contour_fill) {
        p <- p + ggtern::stat_density_tern(mapping = ggplot2::aes_string(x = paste0("`", left_column_name, "`"),
                                                        y = paste0("`", top_column_name, "`"),
                                                        z = paste0("`", right_column_name, "`")),
                                           bdl = .0450, n = 200,  bins = 12, size = countour_size, colour = contour_colour)
      }
    }
  }
  if (return_as_print) {
    return(print(p))
  } else {
    return(p)
  }



}

# some helper functions specific to the above ggtern plot functions.

get_tern_title_size = function(leftTitle, rightTitle, anchor_label_size, anchor_label_size_print, tPrintReturn) {

  font_size <- 12
  if (tPrintReturn) {
    if (anchor_label_size == "default") {
      num_char <- max(nchar(leftTitle), nchar(rightTitle))
      if (num_char > 35) {font_size <- 10}
      if (num_char > 50) {font_size <- 9}
      if (num_char > 60) {font_size <- 8}
    } else {
      font_size <- as.numeric(anchor_label_size)
    }
  } else {
    if (anchor_label_size_print == "default") {
      font_size <- font_size #+ 5
    } else {
      font_size <- as.numeric(anchor_label_size_print)
    }
  }

}

wrap_tern_title <- function(leftTitle, rightTitle) {
  if (!(grepl("\n", leftTitle, fixed = TRUE)  || grepl("\n", rightTitle, fixed = TRUE))) {
    if (stringr::str_detect(leftTitle, "\\s")) {
      if (nchar(leftTitle) > 19) {
        if (is.na(stringr::str_locate(stringr::str_sub(leftTitle, round(nchar(leftTitle) / 2, digits = 0), nchar(leftTitle)), " ")[[1]])) {
          stringi::stri_sub(leftTitle, stringi::stri_locate_last_fixed(leftTitle, " ") + 1, 1) <- "\n"
        } else {
          stringi::stri_sub(leftTitle, stringr::str_locate(stringr::str_sub(leftTitle, round(nchar(leftTitle) / 2, digits = 0), nchar(leftTitle)), " ")[[1]] + round(nchar(leftTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }


    if (stringr::str_detect(rightTitle, "\\s")) {
      if (nchar(rightTitle) > 23) {
        if (is.na(stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]])) {
          stringi::stri_sub(rightTitle, stringi::stri_locate_last_fixed(rightTitle, " ") + 1, 1) <- "\n"
        } else {
          stringi::stri_sub(rightTitle, stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]] + round(nchar(rightTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }
  }

  return(list(left_title = leftTitle, right_title = rightTitle))
}

produce_tern_pair_means_graphs <- function(tern_pairs, framework_data, triads_to_plot = NULL) {



  if (class(tern_pairs) == "character") {
    stopifnot(file.exists(tern_pairs))
    tern_pairs <- read.csv(tern_pairs, stringsAsFactors = FALSE)
  }

  from_ids <- tern_pairs[, "from_id"]
  to_ids <- tern_pairs[, "to_id"]
  from_colours <- tern_pairs[, "from_colour"]
  to_colours <- tern_pairs[, "to_colour"]

  stopifnot(all(colnames(tern_pairs) %in% c("from_id", "to_id", "from_colour", "to_colour")))
  filters_used <- unique(append(from_ids, to_ids))
  stopifnot(all(filters_used %in% framework_data$get_data_list_names()))
  stopifnot(all(areColors(from_colours)))
  stopifnot(all(areColors(to_colours)))

  if (is.null(triads_to_plot)) {
    triads_to_plot <- framework_data$sm_framework$get_triad_ids(keep_only_include = TRUE)
  } else {
    stopifnot(all(triads_to_plot %in% framework_data$sm_framework$get_triad_ids(keep_only_include = TRUE)))
  }


  out_results <- vector("list", length = length(from_ids))
  names(out_results) <- paste0(from_ids, "_", to_ids)

  purrr::pwalk(list(from_ids, to_ids, from_colours, to_colours), function(from_id, to_id, from_colour, to_colour) {

    out_plots <- vector("list", length = length(triads_to_plot))
    names(out_plots) <- triads_to_plot

    df_list <- vector("list", length = 2)
    names(df_list) <- c(from_id, to_id)
    df_list[[1]] <- framework_data$data[[from_id]]
    df_list[[2]] <- framework_data$data[[to_id]]
    data_titles <- c(from_id, to_id)
    colour_vector <- c(from_colour, to_colour)

    purrr::walk(triads_to_plot, function(triad_id) {

      out_plots[[triad_id]] <<- plot_tern_means(df_list = df_list, triad_id = triad_id, data_titles = data_titles,
                                                framework_object = fwd$sm_framework, colour_vector = colour_vector, dot_size = .08, dot_transparency = .3,
                                                confidence_size = 2)

    })

    out_results[[paste0(from_id, "_", to_id)]] <<- out_plots


  })
  return(out_results)

}


produce_keyness_pair_graphs <- function(keyness_pairs, data_object, freetext_ids = NULL) {


  if (class(keyness_pairs) == "character") {
    stopifnot(file.exists(keyness_pairs))
    keyness_pairs <- read.csv(keyness_pairs, stringsAsFactors = FALSE)
  }

  if (!is.null(freetext_ids)) {
    stopifnot(all(freetext_ids %in% data_object$sm_framework$get_freetext_fragments()))
  } else {
    freetext_ids <- data_object$sm_framework$get_freetext_fragments()
  }

  from_ids <- keyness_pairs[, "from_id"]
  to_ids <- keyness_pairs[, "to_id"]
  from_colours <- keyness_pairs[, "from_colour"]
  to_colours <- keyness_pairs[, "to_colour"]

  filters_used <- unique(append(from_ids, to_ids))
  stopifnot(all(filters_used %in% data_object$get_data_list_names()))
  stopifnot(all(areColors(from_colours)))
  stopifnot(all(areColors(to_colours)))

  # Add new data-frames that have the from and to values added and appended (bind_rows) for quanteda keyness processing ready.
  purrr::pwalk(list(from_ids, to_ids, from_colours, to_colours), function(from_id, to_id, from_colour, to_colour) {
    #tmp_from and with the quanteda keyness processing doc_var added then row-bind into a single data frame and add to the data list. Add colours to it too.
    tmp_from <- data_object$data[[from_id]]
    tmp_from[["doc_var"]] <- rep_len(x = from_id, length.out = nrow(tmp_from))
    tmp_from[["plot_col"]] <- rep_len(x = from_colour, length.out = nrow(tmp_from))
    tmp_to <- data_object$data[[to_id]]
    tmp_to[["doc_var"]] <- rep_len(x = to_id, length.out = nrow(tmp_to))
    tmp_to[["plot_col"]] <- rep_len(x = to_colour, length.out = nrow(tmp_to))
    data_object$add_data_data_frame(dplyr::bind_rows(tmp_from, tmp_to), name = paste0(from_id, "_", to_id), add_to_export_list_names = TRUE)
  })

  out_results <- vector("list", length = length(from_ids))
  names(out_results) <- paste0(from_ids, "_", to_ids)

  # perform the keyness processing
  purrr::pwalk(list(from_ids, to_ids, from_colours, to_colours), function(from_id, to_id, from_colour, to_colour) {

    out_plots <- vector("list", length = length(freetext_ids))
    names(out_plots) <- freetext_ids

    purrr::walk(freetext_ids, function(freetext_id) {

      df <- data_object$data[[paste0(from_id, "_", to_id)]]
      fragment_text_corpus <- quanteda::corpus(df[[freetext_id]], docvars = data.frame(doc_var = df[["doc_var"]]))
      tokens <- quanteda::tokens(fragment_text_corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE,
                                 remove_url = TRUE, remove_separators = TRUE, split_hyphens = TRUE, split_tags = TRUE)
      fragment_token <- quanteda::tokens_wordstem(tokens)

      fragment_token <- quanteda::tokens_remove(fragment_token, c(stopwords("english"), data_object$stop_words))

      dtm <- quanteda::dfm(fragment_token, tolower = TRUE)
      dtm.trim <- quanteda::dfm_trim(dtm, min_termfreq = 3)
      keyness_doc_var <- quanteda.textstats::textstat_keyness(dtm.trim,
                                                              docvars(fragment_text_corpus, "doc_var") == from_id,
                                                              sort = TRUE, measure = "chi2")

      out_plots[[freetext_id]] <<- quanteda.textplots::textplot_keyness(keyness_doc_var, color = c(from_colour, to_colour), show_reference = TRUE, show_legend = FALSE, margin = 0.05, n = 20L, min_count = 2L) +
        scale_fill_discrete(name="", labels= c(from_id, to_id)) +
        ggtitle(paste("KEYNESS PLOT for", from_id, "and", to_id)) +
        theme(legend.position = c(0.6, 0.3)) + ylim(0, 40)

    })
    out_results[[paste0(from_id, "_", to_id)]] <<- out_plots

  })

  return(out_results)
}


plot_sentiment_bars <- function(sentiment_filters, freetexts_to_plot, data_object) {

  # freetexts_to_plot can be either a vector of 1 or more free text ids, a file name of a csv file containing the freetext ids or a parsed version (data.frame)
  if (stringr::str_ends(string = freetexts_to_plot, ".csv")) {
    stopifnot(file.exists(freetexts_to_plot))
    df <- read.csv(freetexts_to_plot, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    freetexts_to_plot <- df[["id"]]
  } else {
    if (class(freetexts_to_plot) == "character") {
      stopifnot(trimws(freetexts_to_plot) != "")
    } else {
      if (class(freetexts_to_plot) == "data.frame") {
        stopifnot(nrow(freetexts_to_plot) > 0)
        stopifnot("id" %in% colnames(freetexts_to_plot))
        freetexts_to_plot <- freetexts_to_plot[["id"]]
      }
    }
  }

    # so now we have our freetexts_to_plot as a vector of one or more characters - check that they are free texts
    stopifnot(all(freetexts_to_plot %in% data_object$sm_framework$get_freetext_fragments()))

    if (stringr::str_ends(string = sentiment_filters, ".csv")) {
      stopifnot(file.exists(sentiment_filters))
      df <- read.csv(sentiment_filters, stringsAsFactors = FALSE)
      stopifnot(nrow(df) > 0)
      stopifnot("id" %in% colnames(df))
      stopifnot("title" %in% colnames(df))
      sentiment_filters <- df[["id"]]
      sentiment_titles <- df[["title"]]
    } else {
      stopifnot(sentiment_filters == "data.frame")
      stopifnot(nrow(sentiment_filters) > 0)
      stopifnot("id" %in% colnames(sentiment_filters))
      stopifnot("title" %in% colnames(sentiment_filters))
      sentiment_filters <- sentiment_filters[["id"]]
      sentiment_titles <- sentiment_filters[["title"]]
    }

    # we create separate cleaned dataframes for the sentiment analysis so add if they are not already there
    add_clean_freetext_to_data(data_object, freetexts_to_plot)


    out_results <- vector("list", length = length(freetexts_to_plot))
    names(out_results) <- freetexts_to_plot

    purrr::walk(freetexts_to_plot, function(frag_id) {
      # each column

      out_plots <- vector("list", length = length(sentiment_filters))
      names(out_plots) <- sentiment_filters

      purrr::walk2(sentiment_filters, sentiment_titles, function(filter_id, filter_title) {
        # each data set - this is the data clean for this column filtered by the data filter in use
        data_use <-  data_object$data[[paste0("data_clean_", frag_id)]] %>% dplyr::filter(.data[["FragmentID"]] %in% data_object$data[[filter_id]][["FragmentID"]])
        # do the sentiment stuff
        data_to_plot <- apply_standard_emotions(data_use, data_object$stop_words)
        # plot
        out_plots[[filter_id]] <<- ggplot(data_to_plot, aes(x = sent_emotion, y = n, fill = sent_emotion)) +
                geom_bar(stat = "identity") +
                theme_minimal() +
                labs(title = paste("Sentiment Distribution : ", data_object$sm_framework$get_signifier_title(frag_id), " : ", filter_title),
                     x = "Sentiment Category",
                     y = "Count") +
                scale_fill_manual(values = c("Very Positive" = "darkgreen",
                                             "Positive" = "green",
                                             "Neutral" = "gray",
                                             "Negative" = "orange",
                                             "Very Negative" = "red")) +
                theme(title = element_text(colour = "black", size = 8, family = "Helvetica"))


      })

      out_results[[frag_id]] <<- out_plots

    })

  return(out_results)

}

plot_sentiment_valence <- function(sentiment_filters, freetexts_to_plot, data_object) {

  # freetexts_to_plot can be either a vector of 1 or more free text ids, a file name of a csv file containing the freetext ids or a parsed version (data.frame)
  if (stringr::str_ends(string = freetexts_to_plot, ".csv")) {
    stopifnot(file.exists(freetexts_to_plot))
    df <- read.csv(freetexts_to_plot, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    freetexts_to_plot <- df[["id"]]
  } else {
    if (class(freetexts_to_plot) == "character") {
      stopifnot(trimws(freetexts_to_plot) != "")
    } else {
      if (class(freetexts_to_plot) == "data.frame") {
        stopifnot(nrow(freetexts_to_plot) > 0)
        stopifnot("id" %in% colnames(freetexts_to_plot))
        freetexts_to_plot <- freetexts_to_plot[["id"]]
      }
    }
  }

  # so now we have our freetexts_to_plot as a vector of one or more characters - check that they are free texts
  stopifnot(all(freetexts_to_plot %in% data_object$sm_framework$get_freetext_fragments()))

  if (stringr::str_ends(string = sentiment_filters, ".csv")) {
    stopifnot(file.exists(sentiment_filters))
    df <- read.csv(sentiment_filters, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    stopifnot("title" %in% colnames(df))
    sentiment_filters <- df[["id"]]
    sentiment_titles <- df[["title"]]
  } else {
    stopifnot(sentiment_filters == "data.frame")
    stopifnot(nrow(sentiment_filters) > 0)
    stopifnot("id" %in% colnames(sentiment_filters))
    stopifnot("title" %in% colnames(sentiment_filters))
    sentiment_filters <- sentiment_filters[["id"]]
    sentiment_titles <- sentiment_filters[["title"]]
  }

  # we create separate cleaned dataframes for the sentiment analysis so add if they are not already there
  add_clean_freetext_to_data(data_object, freetexts_to_plot)


  out_results <- vector("list", length = length(freetexts_to_plot))
  names(out_results) <- freetexts_to_plot

  purrr::walk(freetexts_to_plot, function(frag_id) {
    # each column

    out_plots <- vector("list", length = length(sentiment_filters))
    names(out_plots) <- sentiment_filters

    purrr::walk2(sentiment_filters, sentiment_titles, function(filter_id, filter_title) {
      # each data set - this is the data clean for this column filtered by the data filter in use
      data_use <-  data_object$data[[paste0("data_clean_", frag_id)]] %>% dplyr::filter(.data[["FragmentID"]] %in% data_object$data[[filter_id]][["FragmentID"]])
      # do the sentiment stuff
      text_column <- data_use[["fragment"]]
      # Create a corpus using quanteda
      corpus <- quanteda::corpus(text_column)
      # Tokenize the text into sentences
      tokens <- quanteda::tokens(corpus, remove_punct = TRUE)
      # remove stop words
      tokens <- quanteda::tokens_remove(tokens, c(stopwords("en"), data_object$stop_words))
      # Perform sentiment analysis using sentimentr
      sentiment_scores <- sentimentr::sentiment(text_column)

      out_plots[[filter_id]] <<- ggplot2::ggplot(sentiment_scores, aes(x = sentiment)) + geom_density(colour = "blue", size = 1) +
        labs(title = paste("Sentiment Density : ", data_object$sm_framework$get_signifier_title(frag_id), " : ", filter_title),
             x = "Sentiment Range",
             y = "") +
        ggplot2::labs(caption = paste("Data count = ", nrow(data_use))) +
        theme(title = element_text(colour = "black", size = 8, family = "Helvetica"))

    })

    out_results[[frag_id]] <<- out_plots

  })

  return(out_results)

}


plot_emotions_over_time <- function(sentiment_filters, freetexts_to_plot, data_object) {

  # freetexts_to_plot can be either a vector of 1 or more free text ids, a file name of a csv file containing the freetext ids or a parsed version (data.frame)
  if (stringr::str_ends(string = freetexts_to_plot, ".csv")) {
    stopifnot(file.exists(freetexts_to_plot))
    df <- read.csv(freetexts_to_plot, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    freetexts_to_plot <- df[["id"]]
  } else {
    if (class(freetexts_to_plot) == "character") {
      stopifnot(trimws(freetexts_to_plot) != "")
    } else {
      if (class(freetexts_to_plot) == "data.frame") {
        stopifnot(nrow(freetexts_to_plot) > 0)
        stopifnot("id" %in% colnames(freetexts_to_plot))
        freetexts_to_plot <- freetexts_to_plot[["id"]]
      }
    }
  }

  # so now we have our freetexts_to_plot as a vector of one or more characters - check that they are free texts
  stopifnot(all(freetexts_to_plot %in% data_object$sm_framework$get_freetext_fragments()))

  if (stringr::str_ends(string = sentiment_filters, ".csv")) {
    stopifnot(file.exists(sentiment_filters))
    df <- read.csv(sentiment_filters, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    stopifnot("title" %in% colnames(df))
    sentiment_filters <- df[["id"]]
    sentiment_titles <- df[["title"]]
  } else {
    stopifnot(sentiment_filters == "data.frame")
    stopifnot(nrow(sentiment_filters) > 0)
    stopifnot("id" %in% colnames(sentiment_filters))
    stopifnot("title" %in% colnames(sentiment_filters))
    sentiment_filters <- sentiment_filters[["id"]]
    sentiment_titles <- sentiment_filters[["title"]]
  }

  # we create separate cleaned dataframes for the sentiment analysis so add if they are not already there
  add_clean_freetext_to_data(data_object, freetexts_to_plot)


  out_results <- vector("list", length = length(freetexts_to_plot))
  names(out_results) <- freetexts_to_plot

  purrr::walk(freetexts_to_plot, function(frag_id) {
    # each column

    out_plots <- vector("list", length = length(sentiment_filters))
    names(out_plots) <- sentiment_filters

    purrr::walk2(sentiment_filters, sentiment_titles, function(filter_id, filter_title) {
      # each data set - this is the data clean for this column filtered by the data filter in use
      data_use <-   data_object$data[[paste0("data_clean_", frag_id)]] %>% dplyr::filter(.data[["FragmentID"]] %in% data_object$data[[filter_id]][["FragmentID"]]) %>% filter(!is.na(fragment))
      # do the sentiment stuff
      text_column <- data_use[["fragment"]]


      nrc_emotions <- syuzhet::get_nrc_sentiment(text_column)

      nrc_emotions_with_fragments <- nrc_emotions
      nrc_emotions_with_fragments$FragmentID <- data_use$FragmentID
      # Add the 'EntryYrMth' column to this dataframe for further analysis
      nrc_emotions$EntryYrMth <- data_use$EntryYrMth
      nrc_emotions_with_fragments$EntryYrMth <- data_use$EntryYrMth
      # Summarize the emotions by 'EntryYrMth'
      emotion_over_time <- nrc_emotions %>%
        group_by(EntryYrMth) %>%
        summarise(across(everything(), sum, na.rm = TRUE))

      # Exclude 'positive' and 'negative' from the plot
      emotion_over_time <- emotion_over_time %>% select(-positive, -negative)

      # Calculate the percentage of each emotion per month
      emotion_percentages <- emotion_over_time %>%
        rowwise() %>%
        mutate(across(-EntryYrMth, ~ . / sum(c_across(-EntryYrMth))))

      # Reshape data for plotting
      emotion_long <- emotion_percentages %>%
        pivot_longer(cols = -EntryYrMth,
                     names_to = "Emotion",
                     values_to = "Percentage")

      emotion_long$Emotion <- factor(emotion_long$Emotion, levels = c("anger", "disgust", "fear", "sadness", "surprise", "anticipation", "trust", "joy"))
      ## myColors <- c("#FF0000", "#b5a82c", "#000000", "#00008B", "#55656F", "#FFA500", "#0000FF","#FFC0CB")
      # myColors <- c("#E74C3C", "#27AE60", "#8E44AD", "#3498DB", "#E67E22", "#F1C40F", "#1ABC9C","#F39C12")
      # names(myColors) <- levels(emotion_long$Emotion)
      myColors <-  c(
        "anger" = "#D62728",       # Dark Red
        "disgust" = "#2CA02C",     # Lime Green
        "fear" = "#9467BD",        # Dark Purple
        "sadness" = "#1F77B4",     # Navy Blue
        "surprise" = "#FF7F0E",    # Bright Orange
        "anticipation" = "#FFD700",# Gold
        "trust" = "#17BECF",       # Teal
        "joy" = "#FF1493"          # Hot Pink
      )
      colScale <- scale_colour_manual(name = "grp",values = myColors)
      # Plotting the percentage of emotions over time with points
      out_plots[[filter_id]] <<- ggplot(emotion_long, aes(x = as.Date(paste0(EntryYrMth, "01"), format="%Y%m%d"), y = Percentage, color = Emotion)) +
              geom_line(size=1) +
              geom_point(size=2) +  # Add points to the lines
              colScale +
              labs(title= paste("%age of emotions over time : ", data_object$sm_framework$get_signifier_title(frag_id), " : ", filter_title),
                   x="Time (Year-Month)", y="Percentage of Emotions") +
              scale_y_continuous(labels = scales::percent_format()) +
              theme_minimal() +
              scale_x_date(date_labels = "%Y-%m", date_breaks = "1 months") +
              ggplot2::labs(caption = paste("Data count = ", nrow(data_use))) +
              theme(axis.text.x = element_text(angle=45, hjust=1), title = element_text(colour = "black", size = 8, family = "Helvetica"))


    })

    out_results[[frag_id]] <<- out_plots

  })

  return(out_results)

}

word_frequency_plot <- function(frequency_graph_pairs, freetexts_to_plot, data_object, use_stem = FALSE) {

  stopifnot(all(class(data_object) %in% c("Data", "R6")))

  # freetexts_to_plot can be either a vector of 1 or more free text ids, a file name of a csv file containing the freetext ids or a parsed version (data.frame)
  if (stringr::str_ends(string = freetexts_to_plot, ".csv")) {
    stopifnot(file.exists(freetexts_to_plot))
    df <- read.csv(freetexts_to_plot, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot("id" %in% colnames(df))
    freetexts_to_plot <- df[["id"]]
  } else {
    if (class(freetexts_to_plot) == "character") {
      stopifnot(trimws(freetexts_to_plot) != "")
    } else {
      if (class(freetexts_to_plot) == "data.frame") {
        stopifnot(nrow(freetexts_to_plot) > 0)
        stopifnot("id" %in% colnames(freetexts_to_plot))
        freetexts_to_plot <- freetexts_to_plot[["id"]]
      }
    }
  }


  # so now we have our freetexts_to_plot as a vector of one or more characters - check that they are free texts
  stopifnot(all(freetexts_to_plot %in% data_object$sm_framework$get_freetext_fragments()))

  if (stringr::str_ends(string = frequency_graph_pairs, ".csv")) {
    stopifnot(file.exists(frequency_graph_pairs))
    df <- read.csv(frequency_graph_pairs, stringsAsFactors = FALSE)
    stopifnot(nrow(df) > 0)
    stopifnot(all(c("from_id", "to_id") %in% colnames(df)))
    graph_pairs_from_ids <- df[["from_id"]]
    graph_pairs_to_ids <- df[["to_id"]]
  } else {
    if (class(frequency_graph_pairs) == "data.frame") {
      stopifnot(nrow(frequency_graph_pairs) > 0)
      stopifnot(all(c("from_id", "to_id") %in% colnames(frequency_graph_pairs)))
      graph_pairs_from_ids <- frequency_graph_pairs[["from_id"]]
      graph_pairs_to_ids <- frequency_graph_pairs[["to_id"]]
    }
  }

  # we create separate cleaned dataframes for the sentiment analysis so add if they are not already there
  add_clean_freetext_to_data(data_object, freetexts_to_plot)

  # todo - this might not work

  stop_words <- tidytext::stop_words
  remove_tibble <- tibble(word = data_object$stop_words, lexicon = rep_len("CUSTOM", length(data_object$stop_words)))


  out_results <- vector("list", length = length(freetexts_to_plot))
  names(out_results) <- freetexts_to_plot

  purrr::walk(freetexts_to_plot, function(text_id) {


    out_plots <- vector("list", length = length(graph_pairs_from_ids))
    names(out_plots) <- paste0(graph_pairs_from_ids, "_", graph_pairs_to_ids)


    purrr::walk2(graph_pairs_from_ids, graph_pairs_to_ids, function(from_id, to_id) {

      data_from <- data_object$get_data_dataframe(from_id, as_tibble = TRUE)
      data_to <- data_object$get_data_dataframe(to_id, as_tibble = TRUE)
      data_clean <- data_object$get_data_dataframe(paste0("data_clean_", text_id), as_tibble = TRUE)
      data_from_plot <- data_clean %>% dplyr::filter(FragmentID %in% data_from[["FragmentID"]])
      data_to_plot <- data_clean %>% dplyr::filter(FragmentID %in% data_to[["FragmentID"]])

      data_from_plot_corpus <- data_from_plot  %>%  tidytext::unnest_tokens(word, fragment) %>% dplyr::anti_join(dplyr::rows_append(stop_words, remove_tibble))

      if (use_stem) {
        data_from_plot_corpus <-  data_from_plot_corpus %>% mutate(word = SnowballC::wordStem(word))
      }

      data_to_plot_corpus <- data_to_plot  %>%  tidytext::unnest_tokens(word, fragment) %>% dplyr::anti_join(dplyr::rows_append(stop_words, remove_tibble))

      if (use_stem) {
        data_to_plot_corpus <-  data_to_plot_corpus %>% mutate(word = SnowballC::wordStem(word))
      }

      data_to_plot_combined <- dplyr::bind_rows(dplyr::mutate(data_from_plot_corpus, data_set = stringr::str_replace_all(from_id, "_", " ")),
                                   dplyr::mutate(data_to_plot_corpus, data_set = stringr::str_replace_all(to_id, "_", " ")))

       frequency <- data_to_plot_combined %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(data_set, word) %>%
        group_by(data_set) %>%
        mutate(proportion = n / sum(n)) %>%
        select(-n) %>%
        spread(data_set, proportion)
       #, color = abs(`Actively Use` - `Actively Not`)
       out_plots[[paste0(from_id, "_", to_id)]] <<- ggplot(frequency, aes(x = .data[[stringr::str_replace_all(from_id, "_", " ")]], y = .data[[stringr::str_replace_all(to_id, "_", " ")]],
                             color = abs(.data[[stringr::str_replace_all(from_id, "_", " ")]] - .data[[stringr::str_replace_all(to_id, "_", " ")]]))) +
         geom_abline(color = "gray40", lty = 2) +
         geom_jitter(alpha = 0.1, size = 2.4, width = 0.3, height = 0.3) +
         geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
         scale_x_log10(labels = percent_format()) +
         scale_y_log10(labels = percent_format()) +
         scale_colour_gradient(limits = c(0, 0.001),
                               low = "darkslategray4", high = "gray75") +
         ggplot2::theme(legend.position = "none")

    })

    out_results[[text_id]] <<- out_plots

  })
return(out_results)

}

add_clean_freetext_to_data <- function(data_object, freetexts_to_plot) {

  purrr::walk(freetexts_to_plot, function(x) {
    if (!(paste0("data_clean_", x) %in% data_object$get_data_list_names())) {
      data_object$add_data_data_frame((data_object$data$df1 %>% dplyr::select(fragment = all_of(x), "FragmentID", "EntryYrMth") %>% dplyr::filter(!is.na(fragment))),
                                      name = paste0("data_clean_", x), add_to_export_list_names = TRUE)
    }
  })

}
