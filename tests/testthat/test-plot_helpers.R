# ----- Helper functions that enable plots -------

# Standard plot triads for workbench.

plotTriads <- function(tsettings, tprojectLabels, tnew_json, sigID, data, canvas, colsIn, colourIndex, xdimv , ydimv,  dotColour, dotSize, dotsTransparency, opaqueFilter,
                       showPercentages, showCounts, percentageType, zoneFontSize, zoneColour, zoneDots, percentageCanvas, triadDisplayStats, meanType, displayAnchorMean, tdf1,
                       useIntDotSize, triadTitle, ttitlesize, showGuides, tlegendTitleDisplayColour, tlegendTextDisplayColour, tlegendSize, tcaptionSize, tPrint,
                       contours, contourFill, fillAlpha, fillBins, contourFillLegend, contourSize, contourColour, brewColSel) {

  printAdj <- 1
  if (tPrint) {
    printAdj <- 1 #0.70
    # ttitlesize <- round(ttitlesize * printAdj, digits = 0)
  }

  if (useIntDotSize) {
    dotSize <- data[["pointSize"]]
    shape <- data[["shape"]]
    dotColour <- data[["dotColour"]]
    dotsTransparency <- data[["dotTransparency"]]
  } else {
    dotSize <- as.numeric(dotSize)
    shape <- "circle"
    dotColour <- dotColour
    dotsTransparency <- dotsTransparency
  }


  dotsTransparency <- as.numeric(dotsTransparency)

  llShowPoints <- TRUE

  # Don't do transparency if asked if the data to display is the full dataset
  if (nrow(data) == sum(is.na(data[[paste0(sigID, "X")]]))) { llShowPoints <- FALSE}



  zones <- FALSE

  if (showPercentages | showCounts) {
    zones <- TRUE
    zoneData <- getZoneCount(tsettings, tprojectLabels, sigID, data,  percentageType, tnew_json)
    dotsTransparency <- dotsTransparency * 0.25 # if the dots are going to be displayed - make transparent
    canvas <- percentageCanvas
  }

  triad_col_names <- tnew_json$get_triad_anchor_column_names(sigID)

  if (meanType == "Geometric") {
    geom_mean <- data.frame(transpose(data.frame(compositions::clo(c(compositions::geometricmean(data[!is.na(data[[triad_col_names[["left"]]]]),
                                                                                                      triad_col_names[["left"]]]), compositions::geometricmean(data[!is.na(data[[triad_col_names[["top"]]]]),
                                                                                                                                                                    triad_col_names[["top"]]]), compositions::geometricmean(data[!is.na(data[[triad_col_names[["right"]]]]),
                                                                                                                                                                                                                                 triad_col_names[["right"]]])), total = 100))))
    colnames(geom_mean) <- c("x", "y", "z")
    left_mean <- round(geom_mean[[1]], digits = 0)
    top_mean <- round(geom_mean[[2]], digits = 0)
    right_mean <- round(geom_mean[[3]], digits = 0)
  } else {
    left_mean <- round(mean(data[!is.na(data[[triad_col_names[["left"]]]]), triad_col_names[["left"]]], na.rm = TRUE), digits = 0)
    top_mean <- round(mean(data[!is.na(data[[triad_col_names[["top"]]]]), triad_col_names[["top"]]], na.rm = TRUE), digits = 0)
    right_mean <- round(mean(data[!is.na(data[[triad_col_names[["right"]]]]), triad_col_names[["right"]]], na.rm = TRUE), digits = 0)
  }

  # left_geom_mean <- compositions::geometricmean(as.vector(data %>% dplyr::filter_(!is.na(aes_string(x = paste0("`",getTriadXLabel(sigID), "`")))) %>% dplyr::select_(paste0("`", getTriadLeftColName(tsettings, tprojectLabels, sigID), "`"))))
  # top_geom_mean <- compositions::geometricmean(as.vector(data %>% dplyr::filter_(!is.na(aes_string(x = paste0("`",getTriadXLabel(sigID), "`")))) %>% dplyr::select_(paste0("`", getTopRightColName(tsettings, tprojectLabels, sigID), "`"))))
  #right_geom_mean <- compositions::geometricmean(as.vector(data %>% dplyr::filter_(!is.na(aes_string(x = paste0("`",getTriadXLabel(sigID), "`")))) %>% dplyr::select_(paste0("`", getTriadRightColName(tsettings, tprojectLabels, sigID), "`"))))
  # closed_means <- compositions::clo(left_geom_mean, top_geom_mean, right_geom_mean)
  dspMu <- expression(mu)
  leftTitle <- clearBetweenHTMLTags(paste(str_replace_all(tnew_json$get_triad_left_anchor_text(sigID), "&amp;", "&"),  ifelse(displayAnchorMean,  paste("mu =", left_mean), "")), " ")
  rightTitle <-  clearBetweenHTMLTags(paste(str_replace_all(tnew_json$get_triad_right_anchor_text(sigID), "&amp;", "&"), ifelse(displayAnchorMean, paste("mu =", right_mean), "")), " ")
  topTitle <- clearBetweenHTMLTags(paste(str_replace_all(tnew_json$get_triad_top_anchor_text(sigID), "&amp;", "&"),  ifelse(displayAnchorMean, paste("mu =", top_mean), "")), " ")

  # use auto layout if no manual layout commands

  if (!(grepl("\n", leftTitle, fixed = TRUE)  || grepl("\n", rightTitle, fixed = TRUE))) {
    if (stringr::str_detect(leftTitle, "\\s")) {
      if (nchar(leftTitle) > 23) {
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


  titleLength <- max(c(nchar(leftTitle), nchar(rightTitle)))

  nAnchorSize <- 4.5
  if (titleLength > 25) {
    nAnchorSize <- 4.0
  }
  if (titleLength > 50) {
    nAnchorSize <- 3.5
  }
  if (titleLength > 55) {
    nAnchorSize <- 3.0
  }
  if (titleLength > 65) {
    nAnchorSize <- 2.8
  }
  if (titleLength > 90) {
    nAnchorSize <- 2.0
  }
  if(colsIn$cs != "None" & length(colourIndex) != 0 & showGuides) {
    printAdj <- printAdj *.75
  }
  nAnchorSize <- nAnchorSize * printAdj
  # nAnchorSize <- 4.0

  # ToDo - put in function form
  # naAllowed <- getNAAllowed(tsettings, tprojectLabels, sigID)
  naAllowed <- tnew_json$get_signifier_allow_na(sigID)
  numNADataPoints <- 0
  numNADataPointsMu <- 0
  if (naAllowed) {
    # colNAName <- getTriadNAColName(tsettings, tprojectLabels, sigID, colnames(tdf1))
    colNAName <- tnew_json$get_triad_na_column_name(sigID)
    if (colNAName %in% names(tdf1)) {
      numNADataPointsMu <- length(tdf1[!is.na(tdf1[,colNAName]),colNAName])
      numNADataPoints <- length(data[!is.na(data[,colNAName]),colNAName])
    }
  }

  # colLeftName <- getTriadLeftColName(tsettings, tprojectLabels, sigID)
  colLeftName <- tnew_json$get_triad_anchor_column_names(sigID)[["left"]]
  numDataPointsMu <- length(tdf1[!is.na(tdf1[,colLeftName]) , colLeftName])

  numDataPoints <- length(data[!is.na(data[,colLeftName]) , colLeftName])

  numNonEntries <- nrow(data) - (numDataPoints + numNADataPoints)
  perToData <- round((numDataPoints / numDataPointsMu) * 100, digits = 0)

  p <- ggplot2::ggplot(data = data, aes_string(x = paste0("`", tnew_json$get_triad_x_column_name(sigID), "`"), y= paste0("`", tnew_json$get_triad_y_column_name(sigID), "`"))) +
    ggplot2::theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
                   axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
                   panel.background= element_blank()) +
    ggplot2::annotation_custom(rasterGrob(canvas, width=unit(1.1, "npc"), height=unit(1.1, "npc")), 0.02, .98, 0.025, .866) +
    ggplot2::scale_y_continuous(limits=c(-.2, 1), breaks = seq(0,.866,by=.866)) +
    ggplot2::scale_x_continuous(limits=c(-.2, 1.2),breaks = seq(0,1,by=1)) +
    ggplot2::annotate("text", label = c(topTitle, leftTitle , rightTitle), x = xdimv, y = ydimv, hjust = c(.5, 0, 1), vjust = c(0,1,1), size= nAnchorSize) +
    ggplot2::coord_fixed()

  # doing colours and there is data to colour

  if(colsIn$cs != "None" & length(colourIndex) != 0){

    colMult <- 0.7
    if (!showGuides) {colMult <- 1}
    if (tPrint) {colMult <- 0.60}
    ttitlesize <- round(ttitlesize * colMult, digits = 0)

    colourManual <- c()
    colourManualLabel <- c()
    colourManual <- colsIn$colours
    colourManualLabel <- colsIn$colours

    names(colourManual) <- getMCQIDs(tsettings, tprojectLabels, getMCQIDFromLabel(tprojectLabels, colsIn$cs))
    list_item_names <- getMCQLabels(tsettings, tprojectLabels, getMCQIDFromLabel(tprojectLabels, colsIn$cs))
    list_item_names <- wrap_text(list_item_names, tlength = 25)
    names(colourManualLabel) <- list_item_names
    colourText <- getMCQIDFromLabel(tprojectLabels, colsIn$cs)
    col_label <- wrap_text(getSigLabel(tsettings, tprojectLabels, colourText), tlength = 30)




    if (llShowPoints) {
      if (!zones | (zones & zoneDots)) {

        # p <- p + geom_point(color = colsIn$assign[colourIndex], size = dotSize, shape = shape, alpha = dotsTransparency)
        # geom_histogram(aes_string(y = "..count..", fill = paste0("`", colourText, "`")
        p <- p + ggplot2::geom_point(aes_string(colour = paste0("`", colourText, "`")), size = dotSize, shape = shape, alpha = dotsTransparency) +
          ggplot2::scale_colour_manual(values = unlist(colourManual), breaks = names(colourManual), labels = names(colourManualLabel)) +
          labs(colour = col_label)  +
          theme(legend.title = element_text(color = tlegendTitleDisplayColour, size = tlegendSize), legend.text = element_text(color = tlegendTextDisplayColour, size = tlegendSize))


        if (opaqueFilter) {
          p <- p + ggplot2::geom_point(data = tdf1[!tdf1[["FragmentID"]] %in% data[["FragmentID"]],], size= dotSize - 0.5, colour = dotColour,  alpha = dotsTransparency * 0.333)

        }
      }
    }
    #  p <- p + theme(legend.position = c(0,1))
  }
  # else we are not doing colours
  else {

    if (llShowPoints) {

      if (!zones | (zones & zoneDots)) {

        p <- p + ggplot2::geom_point(size= dotSize, shape = shape, colour = dotColour,  alpha = dotsTransparency)

        if (opaqueFilter) {

          p <- p + ggplot2::geom_point(data = tdf1[!tdf1[["FragmentID"]] %in% data[["FragmentID"]],], size= dotSize - 0.5, colour = dotColour,  alpha = dotsTransparency * 0.333)
        }
      }
    }
  }

  if (zones) {
    showBoth <- FALSE
    if (showPercentages & showCounts) {showBoth <- TRUE}

    fontSize = zoneFontSize

    # if ((max(zoneData[["zoneCount"]]) > 1000) &showCounts)  {
    #   fontSize = 4
    # }

    #   zoneData <- getZoneCount(tsettings, tprojectLabels, sigID, data)


    # we are displaying either the count OR the percentage but not both.
    if (!showBoth) {
      # positions in vector are A, B, C, AB, BC, CA, Centre
      xVal <- c(.18, 0.48, 0.80, 0.31, 0.68, 0.48, 0.50)
      yVal <- c(0.13, 0.67, 0.13, 0.41, 0.39, 0.11, 0.32 )
      if (showPercentages) {
        labelVal <- paste0(zoneData[["zonePercentage"]], "%")
      }
      if (showCounts) {
        labelVal <- zoneData[["zoneCount"]]
      }

      p <- p + annotate(geom = "text", x = xVal, y = yVal, label = labelVal, colour = zoneColour, size = fontSize)

    }


    if (showBoth) {

      xValC <- c(0.18, 0.50, 0.80, 0.31, 0.68, 0.48, 0.48)
      yValC <- c(0.15, 0.70, 0.15, 0.41, 0.39, 0.13, 0.34)

      xValP <- c(0.20, 0.50, 0.83, 0.31, 0.70, 0.50, 0.50)
      yValP <- c(0.06, 0.64, 0.06, 0.35, 0.33, 0.04, 0.28)

      p <- p + annotate(geom = "text", x = xValC, y = yValC, label = zoneData[["zoneCount"]], colour = zoneColour, size = fontSize)
      p <- p + annotate(geom = "text", x = xValP, y = yValP, label = paste0(zoneData[["zonePercentage"]], "%"), colour = zoneColour, size = fontSize)

    }

  }

  #  p <- p + ggplot2::labs(title = paste(triadTitle, "\n", sep = ""))
  p <- p + ggplot2::labs(title = paste(wrap_text(triadTitle, tlength = 56), "\n", sep = ""))

  if (triadDisplayStats) {
    p <- p + ggplot2::labs(caption  = paste0("N = ", nrow(tdf1), " n = ", numDataPointsMu, ifelse(naAllowed, paste0("  nN/A = ", numNADataPointsMu, ""), ""), ifelse(numNonEntries >0, paste("  Skipped = ", numNonEntries), ""), "  filter n = ", numDataPoints, "  %age = ", paste0(perToData, "%"), ifelse(naAllowed, paste0("  filter N/A = ", numNADataPoints), ""), " mu = L:", left_mean, " T: ", top_mean, " R: ", right_mean))
  }

  capFontSize = tcaptionSize *  printAdj

  p <- p + ggplot2::theme(plot.title = element_text(size= ttitlesize, face="bold.italic", hjust = 0.5), plot.caption = element_text(family = "Times", size = capFontSize))

  if (colsIn$cs != "None" & !showGuides) {

    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (contours) {
    if (numDataPointsMu > 10) {
      if (contourFill) {
        fillBins <- brewer.pal.info[brewColSel,][["maxcolors"]]
        p <- p + geom_density_2d(colour = contourColour, size = contourSize, bins = fillBins)
      } else {
        p <- p + geom_density_2d(colour = contourColour, size = contourSize, bins = 12)
      }
    }
  }

  if (contourFill | (contourFill & contourFillLegend)) {
    # the count should be the number of entries in the triad not the number in the dataset

    if (numDataPointsMu > 10) {

      #p <- p + geom_density_2d_filled() + scale_fill_brewer()
      fillBins <- brewer.pal.info[brewColSel,][["maxcolors"]]
      p <- p +  stat_density_2d(geom = "polygon", contour = TRUE,
                                aes(fill = after_stat(level)), alpha = fillAlpha, colour = contourColour, size = contourSize,
                                bins = fillBins, show.legend = contourFillLegend) +
        scale_fill_distiller(palette = brewColSel, direction = -1)
    }
  }

  return(p)
}
