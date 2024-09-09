
#' @description
#' A list of the available colour brewer palettes available for heat maps.
#' @returns A character vector of available colour palette names.
#' @export
get_colour_pallete_names <- function() {
  return(cs <- c("YlOrRd", "YlOrBl", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GbBu", "BuPu", "BuGn", "Blues", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"))
}
#' @description
#' Get the number of colours a colour palette contains to help sort bin counts in heatmaps.
#' @param brew_colour_name - the colour palette name
#' @returns An integer with the number of palette colours.
#' @export
get_palette_colour_count <- function(brew_colour_name) {
  stopifnot(brew_colour_name %in% get_colour_pallete_names())
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
#' @param fill_alpha - default 0.5. The transparency (alpha) value of the contour_fill if countour_fill set to TRUE.
#' @param fill_bins - default 12. The number of bins to use in the calculation of the contour fill (if contour_fill set to TRUE). The value is then re-calculated based on the colour palette selected (see brew_colour_select paramater below).
#' @param fill_legend - default FALSE. Display the fill legend for the counter fill if contour_fill set to TRUE.
#' @param contour_size - default 0.5. The line size of the contour lines if contours set to TRUE.
#' @param contour_colour - default "blue". The colour of the contour lines if counters set to TRUE. A Character string of any valid R colour format, such as hex values or colour names.
#' @param brew_colour_select - default "Spectral". A character string with any valid RColorBrewer palette name.
#' @param use_mcq_for_colours - default FALSE. Don'mt colour by MCQ
#' @returns A ggplot graph object of the triad.
#' @export
plot_triad <- function(filtered_data, full_data, sig_id, framework_object, dot_size = 0.6, dot_colour = "black",
                       dot_transparency = 1, opaque_filtered = FALSE, opaque_filter_dot_size = 0.4,
                       opaque_filter_dot_transparency = 0.5, opaque_filter_dot_colour = "blue",
                       display_anchor_means = FALSE, mean_type = "geometric", show_percentages = FALSE, show_totals = FALSE,  percentage_type = "Triad",
                       zone_font_size = 4, zone_display_colour = "black", zone_dots = FALSE, zone_dot_transparency = 0.25, display_stats_caption = TRUE, caption_size = 8, caption_colour = "black",
                       graph_title = NULL, title_colour = "black", title_size = 12, contours = FALSE, contour_fill = FALSE, fill_alpha = 0.5, fill_bins = 12,
                       fill_legend = FALSE, contour_size = 0.5, contour_colour = "blue", brew_colour_select = "Spectral"

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
    # Do the point geom - i.e. plot the dots with passed colours, size and transparency plus the background image.
    p <- p + ggplot2::geom_point(size = dot_size, colour = dot_colour, alpha = dot_transparency) +
      ggplot2::annotation_custom(grid::rasterGrob(get_triad_standard_canvas(), width = ggplot2::unit(1.1, "npc"), height = ggplot2::unit(1.1, "npc")), 0.02, .98, 0.025, .866)

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
      fillBins <- get_palette_colour_count(brew_colour_select)
      p <- p +  ggplot2::stat_density_2d(geom = "polygon", contour = TRUE,
                                         ggplot2::aes(fill = ggplot2::after_stat(level)), alpha = fill_alpha, colour = contour_colour, size = contour_size,
                                         bins = fill_bins, show.legend = fill_legend) +
        ggplot2::scale_fill_distiller(palette = brew_colour_select, direction = -1)
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
                                             " mu = L:", anchor_means[["left_mean"]], " T: ", anchor_means[["top_mean"]], " R: ",
                                             anchor_means[["right_mean"]])) +
      ggplot2::theme(plot.caption = ggplot2::element_text(family = "Times", size = caption_size, colour = caption_colour))

  }

  return(p)

}

