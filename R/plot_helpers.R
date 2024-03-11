

# get the standard triad canvas
get_triad_standard_canvas <- function() {
  # if the image is not in the global envirionment add it
  if (!exists("triad_standard_canvas", envir = .GlobalEnv)) {
    assign("triad_standard_canvas", png::readPNG("./data/triad_standard.png"), envir = .GlobalEnv)
  }
  return(triad_standard_canvas)
}

# get the zone triad canvas
get_triad_zone_canvas <- function() {
  # if the image is not in the global envirionment add it
  if (!exists("triad_zone_canvas", envir = .GlobalEnv)) {
    assign("triad_zone_canvas", png::readPNG("./data/zone_zone.png"), envir = .GlobalEnv)
  }
  return(triad_zone_canvas)
}

plot_triad <- function(filtered_data, full_data, sig_id, framework_object, dot_size = 1.0, dot_colour = "black",
                       dot_transparency = TRUE, opaque_filtered = FALSE, opaque_filter_dot_size = 0.6,
                       opaque_filter_dot_transparency = 0.5, opaque_filter_dot_colour = "blue",
                       display_anchor_means = FALSE, mean_type = "geometric",
                       show_percentages = FALSE, show_totals = FALSE, display_stats_caption = TRUE, caption_size = 10, caption_colour = "black",
                       percentage_type = "Triad", zone_font_size = 4, zone_display_colour = "black", zone_dots = FALSE,
                       graph_title = NULL, title_colour = "black", contours = FALSE, contour_fill = FALSE, fill_alpha = 0.5, fill_bins = 12,
                       fill_legend = FALSE, countour_size = 0.5, contour_colour = "blue", brew_colour_select = "Spectral"

) {


  x_column <- framework_object$get_triad_x_column_name(id = sig_id)
  y_column <- framework_object$get_triad_y_column_name(id = sig_id)
  allow_na <- framework_object$get_signifier_allow_na(sig_id)

  anchor_means <- calculate_triad_means(filtered_data, sig_id, mean_type, framework_object)
  anchor_titles <- get_anchor_titles(sig_id, display_anchor_means, anchor_means, framework_object)
  anchor_size <- get_anchor_size(anchor_titles)
  title_size <- get_graph_title_size(graph_title)

  left_title <- anchor_titles[["left_title"]]
  right_title <-  anchor_titles[["right_title"]]
  top_title <- anchor_titles[["top_title"]]

  caption_values <- get_caption_values(filtered_data, full_data, sig_id, framework_object)



  p <- ggplot2::ggplot(filtered_data, ggplot2::aes(x = .data[[x_column]], y = .data[[y_column]])) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                   panel.background= ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(limits=c(-.2, 1), breaks = seq(0,.866,by=.866)) +
    ggplot2::scale_x_continuous(limits=c(-.2, 1.2),breaks = seq(0,1,by=1)) + ggplot2::coord_fixed() +
    ggplot2::annotate("text", label = c(top_title, left_title , right_title), x =  c(.5, -.2, 1.2), y = c(1.00, -.1, -.1), hjust = c(.5, 0, 1), vjust = c(0,1,1), size= anchor_size, colour = title_colour) +
    ggplot2::labs(title = paste(wrap_text(graph_title, tlength = 56), "\n", sep = "")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size= title_size, face="bold.italic", hjust = 0.5, colour = title_colour))


  # we are doing normal dots
  if (!show_percentages & !show_totals) {

    p <- p + ggplot2::geom_point(size= dot_size,  colour = dot_colour,  alpha = dot_transparency) +
      ggplot2::annotation_custom(grid::rasterGrob(get_triad_standard_canvas(), width = ggplot2::unit(1.1, "npc"), height = ggplot2::unit(1.1, "npc")), 0.02, .98, 0.025, .866)

    # opaque the dots not in the filter
    if (opaque_filtered) {
      p <- p + ggplot2::geom_point(data = full_data[!full_data[["FragmentID"]] %in% filtered_data[["FragmentID"]],], size= opaque_filter_dot_size, colour = opaque_filter_dot_colour,  alpha = opaque_filter_dot_transparency)
    }

    # if contour and/or contour fill requested.
   # contours = FALSE, contour_fill = FALSE, fill_alpha = 0.5, fill_bins = 12,
   # fill_legend = FALSE, countour_size = 0.5, contour_colour = "blue", brew_colour_select = "Spectral"
    if (contours & !contour_fill) {
      p <- p + ggplot2::geom_density_2d(colour = contour_colour, size = countour_size, bins = 12)
    }

    if (contour_fill) {
      fillBins <- RColorBrewer::brewer.pal.info[brew_colour_select,][["maxcolors"]]
      p <- p +  ggplot2::stat_density_2d(geom = "polygon", contour = TRUE,
                                         ggplot2::aes(fill = ggplot2::after_stat(level)), alpha = fill_alpha, colour = contour_colour, size = countour_size,
                                bins = fill_bins, show.legend = fill_legend) +
        ggplot2::scale_fill_distiller(palette = brew_colour_select, direction = -1)
    }

  }

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

