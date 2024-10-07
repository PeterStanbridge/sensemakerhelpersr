
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
#' @param colour_sig_id - default NULL, don't colour by a list signifier, otherwise the signifier id of the list to colour by.
#' @param colour_vector - default NULL, a vector of valid R colour codes (alpha, hex etc.) of length the number of list items in the signifier id used for the colour.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer Set1 palette. values of "Dark2", "Set1", "Set2" or "Set3" recommended but others can also work. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param veridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "veridis" selected as the colour package.
#' @param veridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "veridis" selected as the colour package.
#' @param show_legend - default TRUE, show the colour legend if colour by list (MCQ). FALSE to remove the legend.
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
                       graph_title = NULL, title_colour = "black", title_size = 12, contours = FALSE, contour_fill = FALSE, fill_alpha = 0.5, fill_bins = 12,
                       fill_legend = FALSE, contour_size = 0.5, contour_colour = "blue", brew_colour_select = "Spectral", colour_sig_id = NULL, colour_vector = NULL,
                       colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0, viridis_default_end = 0.6, show_legend = TRUE,
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
     if (!show_legend) {
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
                                            " ",  "\U003BC",  " = L:", anchor_means[["left_mean"]], " T: ", anchor_means[["top_mean"]], " R: ",
                                             anchor_means[["right_mean"]])) +
      ggplot2::theme(plot.caption = ggplot2::element_text(family = "Times", size = caption_size, colour = caption_colour))

  }
  return(p)
}



#' @description
#' Plot triad means of passed in data frame data and confidence intervals for comparison
#'  description
#' @param df_list - A named list of filtered data frames containing the same capture data.
#' @param triad_id - The triad to be plotted.
#' @param data_titles - A list of the same length as the df_list giving titles for the data plot
#' @param framework_object - The framework object pertaining to the collection.
#' @param colour_vector - default NULL, if provided, a vector of the same length as df_list with the colours to plot each of the data frames passed.
#' @param display_mean - default TRUE, display the mean or not of the data.
#' @param mean_colour - default NULL, if provided, a vector of the same length as df_list with the colours for the means. If NULL, same colour used as in colour_vector.
#' @param mean_shape - default NULL, if provided, a vector of the same length as df_list, with the shapes of the means. If NULL, then "cicle" used.
#' @param mean_zero_logic - default "small_value" otherwise "remove" can be specified to remove NA records.
#' @param mean_size - default 2, the dot soze of the mean.
#' @param colour_package - default "RColorBrewer". Can also use "viridis". If the colour_vector is null then the plot function will assign values from the colour package.
#' @param package_palette - default "Set1" for the RColorBrewer Set1 palette. values of "Dark2", "Set1", "Set2" or "Set3" recommended but others can also work. Viridis has "A" - "H".
#' @param colour_direction - default 1. 1 for left to right colour selection from the palette, -1 for reverse.
#' @param veridis_default_start - default 0, A value between 0-1 for start colour value in the selected viridis palette. Only applicable if "veridis" selected as the colour package.
#' @param veridis_default_end - default 0.6. A value between 0-1 for end colour value in the selected viridis palette. Only applicable if "veridis" selected as the colour package.
#' @param legend_title_size - default 8. The size of the legend title.
#' @param legend_text_colour - default "black". The colour legend text A Character string of any valid R colour format, such as hex values or colour names.
#' @param legend_text_size - default 8. The size of the legend text.
#' @param dot_size - default 0.5, the size of the graph dots
#' @param dot_transparency - default 1, the alpha (transparency value of the dits)
#' @param confidence_value - default 0.05 - the confidence interval to draw around the mean.
#' @param confidence_size - default 2, the size of the confidence line
#' @returns A ggplot graph object of the triad.
#' @export
plot_tern_means <- function(df_list, triad_id, data_titles, framework_object, colour_vector = NULL, display_mean = TRUE, mean_colour = NULL, mean_shape = NULL, mean_zero_logic = "small_value",
                            mean_size = 2, colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0,
                            viridis_default_end = 0.6, show_legend = TRUE, legend_title_colour = "black", legend_title_size = 8, legend_text_colour = "black",
                            legend_text_size = 8,  dot_size = 0.5, dot_transparency = 1, confidence_value = 0.05, confidence_size = 2, title_colour = "black", title_size = 12,
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

  for (i in seq_along(data_titles)) {

    # calculate the mean
    data_means <- calculate_triad_means(df_list[[i]], triad_id, "geometric", framework_object, zero_logic = mean_zero_logic, for_ggtern = TRUE)

    plot_data <- df_list[[i]]
    plot_data[["col_by"]] <- rep_len(data_titles[[i]], length.out = nrow(plot_data))

    p <- p + geom_point(data = plot_data, aes(x = .data[[framework_object$get_triad_left_column_name(triad_id)]],
                                              y = .data[[framework_object$get_triad_top_column_name(triad_id)]],
                                              z = .data[[framework_object$get_triad_right_column_name(triad_id)]], colour = col_by), size = dot_size, alpha = dot_transparency) +
      geom_point(data = data_means, aes(x = x, y = y, z = z), colour = ifelse(!is.null(mean_colour), mean_colour[[i]],  colour_vector[[i]]), size = mean_size, shape =
                   ifelse(is.null(mean_shape), "circle", mean_shape[[i]]), fill = ifelse(!is.null(mean_colour), mean_colour[[i]],  colour_vector[[i]])) +
      ggtern::geom_confidence_tern(data = plot_data, aes(x = .data[[framework_object$get_triad_left_column_name(triad_id)]],
                                                         y = .data[[framework_object$get_triad_top_column_name(triad_id)]],
                                                         z = .data[[framework_object$get_triad_right_column_name(triad_id)]], colour = col_by), breaks = confidence_value, size = confidence_size)



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

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)

}


#' @param graph_title - default. NULL. A title for the graph. if NULL the triad title will be used.
#' @param title_colour - default "black". The colour of the graph title. A Character string of any valid R colour format, such as hex values or colour names.
#' @param title_size - default 12. The size of the graph title.

plot_tern_triad <- function(filtered_data, full_data, triad_id, framework_object, dot_size = 0.5, dot_colour = "black", dot_transparency = 1, opaque_filtered = FALSE, opaque_filter_dot_size = 0.4,
                            opaque_filter_dot_transparency = 0.5, opaque_filter_dot_colour = "blue", ternAnchorLabelSizeDisplay = "default", ternAnchorLabelSizePrint = "default", ternTitleLabelSize = "default",
                            show_mean = TRUE, mean_dot_size = 0.5, mean_colour = "blue", mean_shape = "circle", mean_zero_logic = "small_value", graph_title = NULL, title_colour = "black", title_size = 12,
                            show_confidence_intervals = FALSE, confidence_interval_colour = "#0000FF", confidence_interval_Size = 0.55, confidence_intervals = 0.05, show_variance = FALSE, variance_colour = '#08E5F5',
                            variance_transparency = 0.3, show_contours = FALSE, countour_size = 0.5, contour_colour = "blue", show_contour_fill = FALSE, fill_alpha = 0.5,
                            show_fill_legend = FALSE, fill_bins = 10, fill_brewer_palette = "Spectral",
                             colour_sig_id = NULL, colour_vector = NULL, colour_package = "RColorBrewer", package_palette = "Set1", colour_direction = 1, viridis_default_start = 0,
                            viridis_default_end = 0.6, show_legend = TRUE, legend_title_colour = "black", legend_title_size = 8, legend_text_colour = "black", legend_text_size = 8, return_as_print = FALSE) {
  #
  # get column names
  col_names <- framework_object$get_triad_compositional_column_names(triad_id)

  # Get the anchor means.
  # remove NA from the data
  filter_data_no_na <- filtered_data %>% dplyr::filter(!is.na(!! sym(col_names[["left"]]))) %>% dplyr::filter(!! sym(framework_object$get_triad_left_column_name(triad_id)) > 0) %>%
    dplyr::filter(!! sym(framework_object$get_triad_top_column_name(triad_id)) > 0) %>% dplyr::filter(!! sym(framework_object$get_triad_right_column_name(triad_id)) > 0)

  full_data_no_na <- full_data %>% dplyr::filter(!is.na(!! sym(col_names[["left"]])))
  anchor_means <- calculate_triad_means(filtered_data, triad_id, "geometric", framework_object, zero_logic = mean_zero_logic)
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

  font_size <- get_tern_title_size(leftTitle, rightTitle, ternAnchorLabelSizeDisplay, ternAnchorLabelSizePrint, return_as_print)
  title_size <- 12
  if (ternTitleLabelSize != "default") {
    title_size <- as.numeric(ternTitleLabelSize)
  }

  p <- ggtern::ggtern(data = filter_data_no_na, aes(x = .data[[framework_object$get_triad_left_column_name(triad_id)]],
                                                    y = .data[[framework_object$get_triad_top_column_name(triad_id)]],
                                                    z = .data[[framework_object$get_triad_right_column_name(triad_id)]]))
  if (is.null(colour_sig_id)) {
    p <- p + geom_point(colour = dot_colour, size = dot_size, alpha = dot_transparency)
  } else {
    # we are doing colouring by an MCQ (list)
    # use the colour vector if it is provided
    if (!is.null(colour_vector)) {
      p = p +  ggplot2::geom_point(size = dot_size, alpha = dot_transparency, aes_string( colour = paste0("`", colour_sig_id, "`"))) +
        scale_color_manual(values = colour_vector,
                           breaks = framework_object$get_list_items_ids(colour_sig_id),
                           labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
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
            scale_color_brewer(palette = package_palette, direction = colour_direction,
                               breaks = framework_object$get_list_items_ids(colour_sig_id),
                               labels = framework_object$get_list_items_titles(colour_sig_id)) +  labs(color = framework_object$get_signifier_title(colour_sig_id))
        }
      }
    }
    p <- p +  theme(legend.title = element_text(color = legend_title_colour, size = legend_title_size), legend.text = element_text(color = legend_text_colour, size = legend_text_size))
    if (!show_legend) {
      p <- p + ggplot2::theme(legend.position = "none")
    }
  }

  p <- p + labs(title = graph_title) +
    Llab(wrap_text(leftTitle, tlength = 45)) +
    Tlab(wrap_text(topTitle, tlength = 75)) +
    Rlab(wrap_text(rightTitle, tlength = 45)) +
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
    p <- p + geom_point(data = mean_df, aes(x = x, y = y, z = z), colour = mean_colour, size = mean_dot_size, shape = ifelse(mean_shape == "circle", 21, 23), fill = mean_colour)
  }

  if (show_confidence_intervals) {
    p <- p + geom_confidence_tern(breaks = confidence_intervals, size = confidence_interval_Size, colour = confidence_interval_colour)
  }

  if (show_variance) {
    p <- p + ggtern::stat_mean_ellipse(geom='polygon', steps=500, fill= variance_colour, alpha = variance_transparency)
  }


  if (show_contour_fill) {
    if (nrow(filter_data_no_na) > 10) {

      ternFillBins <- RColorBrewer::brewer.pal.info[fill_brewer_palette,][["maxcolors"]]

      p <- p + stat_density_tern(geom = 'polygon', contour = TRUE,
                                 n  = 200, bdl = .0450,
                                 aes_string(z = paste0("`", framework_object$get_triad_left_column_name(triad_id), "`"),
                                            x = paste0("`", framework_object$get_triad_top_column_name(triad_id), "`"),
                                            y = paste0("`", framework_object$get_triad_right_column_name(triad_id), "`"), fill  = "..level.."),
                                 alpha = fill_alpha, colour = contour_colour, size = countour_size,
                                 bins = fill_bins, show.legend = show_fill_legend) +
        scale_fill_distiller(palette = fill_brewer_palette, direction = -1)
    }
  }

  if (show_contours) {
    if (nrow(filter_data_no_na) > 10) {
      if (!show_contour_fill) {
        p <- p + stat_density_tern(mapping = aes_string(z = paste0("`", framework_object$get_triad_left_column_name(triad_id), "`"),
                                                        x = paste0("`", framework_object$get_triad_top_column_name(triad_id), "`"),
                                                        y = paste0("`", framework_object$get_triad_right_column_name(triad_id), "`")),
                                   bdl = .0450, n = 200, bins = 12, size = countour_size, colour = contour_colour)
      }
    }
  }
  if (return_as_print) {
    return(print(p))
  } else {

    return(p)
  }



}



# This is the workbench ggtern - we will be replacing it
plotTernTriads <- function(tsettings, tprojectLabels, tnew_json, tSigID, data, canvas, colsIn, colourIndex, xdimv , ydimv,  dotColour, dotSize, dotsTransparency, opaqueFilter, showPercentages,
                           showCounts, zoneDots, percentageCanvas, tdf1, useIntDotSize, tConfidenceIntervals, tPrintReturn, ternConfidenceColour, ternConfidenceLineSize,
                           ternMeanDotSize, ternMeanDisplay, ternMeanColour, ternMeanDotShape, showVariance, varianceColour, varianceTransparency, ternContours, ternContourFill, ternFillAlpha,
                           ternContourFillLegend, ternFillBins, ternContourSize, ternContourColour, ternBrewColSel, ternAnchorLabelSizeDisplay = "default",
                           ternAnchorLabelSizePrint = "default", ternTitleLabelSize = "default") {


  # DataSmooth1 <-  data[!is.na(data[, getTriadLeftColName(tsettings, tprojectLabels, tSigID)]) ,]

  DataSmooth <-   data[!is.na(data[, tnew_json$get_triad_left_column_name(id = tSigID)]) ,]

  #DataSmooth <- as.data.frame(compositions::acomp(DataSmooth1, total = 1))

  # if (tSigID == "2c15121d-45df-477e-9f92-cba4808b0961") {

  # }

  # calculate the mean

  # gMean <- data.frame(transpose(data.frame(c(compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[getTriadLeftColName(tsettings, tprojectLabels, tSigID)]]),
  #                                           getTriadLeftColName(tsettings, tprojectLabels, tSigID)]),
  #                                           compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[getTriadTopColName(tsettings, tprojectLabels, tSigID)]]),
  #                                          getTriadTopColName(tsettings, tprojectLabels, tSigID)]),
  #                                          compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[getTriadRightColName(tsettings, tprojectLabels, tSigID)]]),
  #                                          getTriadRightColName(tsettings, tprojectLabels, tSigID)])))))

  # gMean <- data.frame(transpose(data.frame(c(compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[tnew_json$get_triad_left_column_name(tSigID)]]),
  #                                                                                   tnew_json$get_triad_left_column_name(tSigID)]),
  #                                            compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[tnew_json$get_triad_top_column_name(tSigID)]]),
  #                                                                                   tnew_json$get_triad_top_column_name(tSigID)]),
  #                                            compositions::geometricmean(DataSmooth[!is.na(DataSmooth[[tnew_json$get_triad_right_column_name(tSigID)]]),
  #                                                                                   tnew_json$get_triad_right_column_name(tSigID)])))))
  gMean <- data.frame(transpose(data.frame(c(compositions::geometricmean(DataSmooth[, tnew_json$get_triad_left_column_name(tSigID)]),
                                             compositions::geometricmean(DataSmooth[, tnew_json$get_triad_top_column_name(tSigID)]),
                                             compositions::geometricmean(DataSmooth[, tnew_json$get_triad_right_column_name(tSigID)])))))
  names(gMean) <- c("x", "y", "z")


  #get_signifier_title

  #leftTitle <-  cleanFun(str_replace_all(getTriadLeftLabel(tsettings, tprojectLabels, tSigID), "&amp;", "&"))
  # rightTitle <-  cleanFun(str_replace_all(getTriadRightLabel(tsettings, tprojectLabels, tSigID), "&amp;", "&"))
  # topTitle <-    cleanFun(str_replace_all(getTriadTopLabel(tsettings, tprojectLabels, tSigID), "&amp;", "&"))

  leftTitle <-  cleanFun(str_replace_all(tnew_json$get_triad_left_anchor_text(tSigID), "&amp;", "&"))
  rightTitle <-  cleanFun(str_replace_all(tnew_json$get_triad_right_anchor_text(tSigID), "&amp;", "&"))
  topTitle <-    cleanFun(str_replace_all(tnew_json$get_triad_top_anchor_text(tSigID), "&amp;", "&"))

  # use auto layout if no manual layout commands
  if (!(grepl("\n", leftTitle, fixed = TRUE)  || grepl("\n", rightTitle, fixed = TRUE))) {
    if (stringr::str_detect(leftTitle, "\\s")) {
      if (nchar(leftTitle) > 19) {
        if (is.na(stringr::str_locate(stringr::str_sub(leftTitle, round(nchar(leftTitle) / 2, digits = 0), nchar(leftTitle)), " ")[[1]])) {
          stringi::stri_sub(leftTitle, stringi::stri_locate_last_fixed(leftTitle, " ") + 1, 1) <- "\n"
        } else {
          stri_sub(leftTitle, stringr::str_locate(stringr::str_sub(leftTitle, round(nchar(leftTitle) / 2, digits = 0), nchar(leftTitle)), " ")[[1]] + round(nchar(leftTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }


    if (stringr::str_detect(rightTitle, "\\s")) {
      if (nchar(rightTitle) > 23) {
        if (is.na(stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]])) {
          stringi::stri_sub(rightTitle, stringi::stri_locate_last_fixed(rightTitle, " ") + 1, 1) <- "\n"
        } else {
          stri_sub(rightTitle, stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]] + round(nchar(rightTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }
  }
  font_size <- 12
  if (tPrintReturn) {
    if (ternAnchorLabelSizeDisplay == "default") {
      num_char <- max(nchar(leftTitle), nchar(rightTitle))
      if (num_char > 35) {font_size <- 10}
      if (num_char > 50) {font_size <- 9}
      if (num_char > 60) {font_size <- 8}
    } else {
      font_size <- as.numeric(ternAnchorLabelSizeDisplay)
    }
  } else {
    if (ternAnchorLabelSizePrint == "default") {
      font_size <- font_size + 5
    } else {
      font_size <- as.numeric(ternAnchorLabelSizePrint)
    }
  }

  title_size <- 9
  if (ternTitleLabelSize != "default") {
    title_size <- as.numeric(ternTitleLabelSize)
  }
  p <- ggtern::ggtern(data = DataSmooth, aes_string(x = paste0("`", tnew_json$get_triad_left_column_name(tSigID), "`"),
                                                    y = paste0("`", tnew_json$get_triad_top_column_name(tSigID), "`"),
                                                    z = paste0("`", tnew_json$get_triad_right_column_name(tSigID), "`"))) +
    geom_point(size = dotSize, colour = dotColour, alpha = dotsTransparency)  +
    labs(title = wrap_text(tnew_json$get_signifier_title(tSigID), tlength = 75)) +
    Llab(wrap_text(leftTitle, tlength = 45)) +
    Tlab(wrap_text(topTitle, tlength = 75)) +
    Rlab(wrap_text(rightTitle, tlength = 45)) +
    theme(title = element_text(colour = "black", size = title_size, family = "Helvetica")) +
    theme(tern.axis.title.L = element_text(hjust=0, vjust=1, colour = "black", size = font_size, family = "Helvetica")) +
    theme(tern.axis.title.T = element_text(colour = "black", size = font_size, family = "Helvetica"))  +
    theme(tern.axis.title.R = element_text(hjust=1, vjust=1, colour = "black", size = font_size, family = "Helvetica")) +
    theme_nogrid() +
    theme_hidelabels() +
    theme_hideticks() +
    theme(tern.panel.expand = 0.40)  +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) + theme(axis.title.x = element_blank(),  axis.title.y = element_blank())


  if (ternMeanDisplay) {
    p <- p + geom_point(data = gMean, aes(x = x, y = y, z = z), colour = ternMeanColour, size = ternMeanDotSize, shape = ifelse(ternMeanDotShape == "circle", 21, 23), fill = ternMeanColour)
  }

  if (!is.null(tConfidenceIntervals)) {
    p <- p + geom_confidence_tern(breaks = tConfidenceIntervals, size = ternConfidenceLineSize, colour = ternConfidenceColour)
  }

  if (showVariance) {
    p <- p + ggtern::stat_mean_ellipse(geom='polygon', steps=500, fill= varianceColour, alpha = varianceTransparency)
  }


  if (ternContourFill) {
    if (nrow(DataSmooth) > 10) {

      ternFillBins <- brewer.pal.info[ternBrewColSel,][["maxcolors"]]

      p <- p + stat_density_tern(geom = 'polygon', contour = TRUE,
                                 n  = 200, bdl = .0450,
                                 aes_string(z = paste0("`", tnew_json$get_triad_left_column_name(tSigID), "`"),
                                            x = paste0("`", tnew_json$get_triad_top_column_name(tSigID), "`"),
                                            y = paste0("`", tnew_json$get_triad_right_column_name(tSigID), "`"), fill  = "..level.."),
                                 alpha = ternFillAlpha, colour = ternContourColour, size = ternContourSize,
                                 bins = ternFillBins, show.legend = ternContourFillLegend) +
        scale_fill_distiller(palette = ternBrewColSel, direction = -1)
    }
  }

  if (ternContours) {
    if (nrow(DataSmooth) > 10) {
      if (ternContourFill) {
        #  ternFillBins <- brewer.pal.info[ternBrewColSel,][["maxcolors"]]
        #  p <- p + stat_density_tern(mapping = aes_string(z = paste0("`", tnew_json$get_triad_left_column_name(tSigID), "`"),
        #                                                  x = paste0("`", tnew_json$get_triad_top_column_name(tSigID), "`"),
        #                                                  y = paste0("`", tnew_json$get_triad_right_column_name(tSigID), "`")),
        #                             bdl = .0450, n = 200, bins = ternFillBins)
      } else {


        p <- p + stat_density_tern(mapping = aes_string(z = paste0("`", tnew_json$get_triad_left_column_name(tSigID), "`"),
                                                        x = paste0("`", tnew_json$get_triad_top_column_name(tSigID), "`"),
                                                        y = paste0("`", tnew_json$get_triad_right_column_name(tSigID), "`")),
                                   bdl = .0450, n = 200, bins = 12, size = ternContourSize, colour = ternContourColour)
      }
    }
  }


  # geom_smooth_tern(method=lm,fullrange=TRUE,colour='red') +
  if (tPrintReturn) {
    return(print(p))
  } else {

    return(p)
  }
}


get_tern_title_size = function(leftTitle, rightTitle, ternAnchorLabelSizeDisplay, ternAnchorLabelSizePrint, tPrintReturn) {

  font_size <- 12
  if (tPrintReturn) {
    if (ternAnchorLabelSizeDisplay == "default") {
      num_char <- max(nchar(leftTitle), nchar(rightTitle))
      if (num_char > 35) {font_size <- 10}
      if (num_char > 50) {font_size <- 9}
      if (num_char > 60) {font_size <- 8}
    } else {
      font_size <- as.numeric(ternAnchorLabelSizeDisplay)
    }
  } else {
    if (ternAnchorLabelSizePrint == "default") {
      font_size <- font_size #+ 5
    } else {
      font_size <- as.numeric(ternAnchorLabelSizePrint)
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
          stri_sub(leftTitle, stringr::str_locate(stringr::str_sub(leftTitle, round(nchar(leftTitle) / 2, digits = 0), nchar(leftTitle)), " ")[[1]] + round(nchar(leftTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }


    if (stringr::str_detect(rightTitle, "\\s")) {
      if (nchar(rightTitle) > 23) {
        if (is.na(stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]])) {
          stringi::stri_sub(rightTitle, stringi::stri_locate_last_fixed(rightTitle, " ") + 1, 1) <- "\n"
        } else {
          stri_sub(rightTitle, stringr::str_locate(stringr::str_sub(rightTitle, round(nchar(rightTitle) / 2, digits = 0), nchar(rightTitle)), " ")[[1]] + round(nchar(rightTitle) / 2, digits = 0), 1) <- "\n"
        }
      }
    }
  }

  return(list(left_title = leftTitle, right_title = rightTitle))
}

