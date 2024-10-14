#' @title wrap a text string at a given length
#' @param text_string - the string to be wrapped.
#' @param wrap_length - Default 30, the position length for the text to be wrapped.
#' @param replacement_value - A string to use as a replacement in the wrap break - i.e. instead of breaking.
#' @return The wrapped text_string
#' @export
wrap_text <- function(text_string, wrap_length = 30, replacement_value = "") {
  ret_val <- stringr::str_wrap(text_string, width = wrap_length)
  if (replacement_value != "") {
    ret_val <- stringr::str_replace_all(ret_val, "\\n", replacement_value)
  }
  return(ret_val)
}

#' @title generic remove html helper function
#' @description
#' Remove html string from an input string
#' @param input_string - the string to remove html tags.
#' @returns The string with any html removed.
#' @export
removeHTML <- function(input_string) {
  ret_val <- gsub("<.*?>", " ", input_string)
  return(stringr::str_replace_all(ret_val, "   ", " "))
}

clearHTML <- function(tString) {
  return(removeHTML(tString))
}

clearBetweenHTMLTags <- function(htmlString, repVal) {
  return(gsub("<[^>]+>", repVal, htmlString))
}

#' @title remove dashes from a string
#' @description
#' Remove dashes from a string
#' @param input_string - the string to remove dash.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with dash removed.
#' @export
clearDash <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("-", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove long dashes from a string
#' @description
#' Remove long dashes from a string
#' @param input_string - the string to remove long dash.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with long dash removed.
#' @export
clearLongDash <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("–", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove plus sign from a string
#' @description
#' remove plus sign from a string
#' @param input_string - the string to remove plus sign.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with plus sign removed.
#' @export
clearPlus <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("\\+", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove brackets  from a string
#' @description
#' remove brackets  from a string
#' @param input_string - the string to remove brackets.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with brackets removed.
#' @export
clearBrackets <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("\\]", "", gsub("\\[", "", gsub("[()]", "", input_string)))
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove spaces  from a string
#' @description
#' remove spaces  from a string
#' @param input_string - the string to remove spaces.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with spaces removed.
#' @export
clearSpace <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub(" ", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove commas  from a string
#' @description
#' remove commas  from a string
#' @param input_string - the string to remove commas.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with commas removed.
#' @export
clearComma <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub(",", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove single quotes  from a string
#' @description
#' remove single quotes  from a string
#' @param input_string - the string to remove single quotes.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with single quotes removed.
#' @export
clearQuote <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("\\'", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove points  from a string
#' @description
#' remove points  from a string
#' @param input_string - the string to remove points.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with points removed.
#' @export
clearPoint <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("\\.", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove semi colons  from a string
#' @description
#' remove semicolons  from a string
#' @param input_string - the string to remove semicolon.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with semi colons removed.
#' @export
clearSemiColon <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub(";", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove forward slashes  from a string
#' @description
#' remove forward slashes  from a string
#' @param input_string - the string to remove forward slashes.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with forward slashes removed.
#' @export
clearSlash <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("/", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove colons  from a string
#' @description
#' remove colons  from a string
#' @param input_string - the string to remove colons.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with colons removed.
#' @export
clearColon <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub(":", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove apostrophes  from a string
#' @description
#' remove apolstrophes  from a string
#' @param input_string - the string to remove apostrophes.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with apostrophes removed.
#' @export
clearApos <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("’", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove ampersands  from a string
#' @description
#' remove ampersands  from a string
#' @param input_string - the string to remove ampersands.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with ampersands removed.
#' @export
clearAmps <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("&", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove percentages  from a string
#' @description
#' remove percentages  from a string
#' @param input_string - the string to remove percentages.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with percentages removed.
#' @export
clearPer <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("%", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove close triple dots  from a string
#' @description
#' remove close triple dots  from a string
#' @param input_string - the string to remove close triple dots.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with close triple dots removed.
#' @export
clearCloseDots <- function(input_string, double_space_to_single = TRUE) {
  ret_val <- gsub("…", "", input_string)
  if (double_space_to_single) {
    ret_val <- gsub("  ", " ", ret_val)
  }
  return(ret_val)
}

#' @title remove all of the characters  from a string
#' @description
#' remove HTML, dash, brackets, close dots, commas, quotes, points, semicolons, forward slashes, apostrophes, ampersands, percentages, long dashes, plus and colons.
#' @param input_string - the string to remove all the tags.
#' @param double_space_to_single - Default TRUE, will replace any remaining double space with a single space.
#' @returns The string with brackets removed.
#' @export
clearAll <- function(input_string, double_space_to_single = TRUE) {
  return(input_string %>% clearHTML()  %>% clearDash()  %>% clearBrackets() %>% clearCloseDots()   %>% clearComma() %>% clearQuote() %>% clearPoint() %>% clearSemiColon() %>% clearSlash() %>% clearApos() %>%
           clearAmps() %>% clearPer() %>% clearLongDash() %>% clearPlus() %>% clearColon())
}

#' @title generic is not blank, null or na helper function.
#' @description
#' Is a string NOT blank or null or NA
#' @param x - the string to check
#' @returns TRUE or FALSE - TRUE if the text is not NA, NULL or blank.
#' @export
is_not_blank_null_na <- function(x) {
  if (length(trimws(x)) == 0) {return(FALSE)}
  if (is.null(x)) {return(FALSE)}
  if (is.na(x)) {return(FALSE)}
  if (trimws(x) == "") {return(FALSE)}
  return(TRUE)
}

#' @title generic is  blank, null or na helper function.
#' @description
#' Is a string blank or null or NA
#' @param x - the string to check
#' @returns TRUE or FALSE - TRUE if the text is  NA, NULL or blank.
#' @export
is_blank_null_na <- function(x) {
  if (length(trimws(x)) == 0) {return(TRUE)}
  if (is.null(x)) {return(TRUE)}
  if (is.na(x)) {return(TRUE)}
  if (trimws(x) == "") {return(TRUE)}
  return(FALSE)
}

clean_string_of_html <- function(htmlString) {
  # x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
  # str_replace_all(htmlString, "[[:punct:]]", " ")
  # return(gsub(x, "", htmlString))
  #return(gsub("/<.*?>", "", htmlString))
  #return(str_replace_all(htmlString, "[^[:alnum:]_]", " "))
  # return(gsub("<.*?>", " ", htmlString))
  return(str_replace_all(gsub("<.*?>", " ", htmlString), "  ", " " ))
}

createBrewerColourArray <- function() {
  cs <- c("YlOrRd", "YlOrBl", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GbBu", "BuPu", "BuGn", "Blues", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
  bins <- c(rep(9, each = 18), rep(11, each = 9))
  brewCols <- as.list(bins)
  names(brewCols) <- cs
  return(brewCols)
}

#' @title apply standard emotions to data.
#' @description
#' This function returns the standard "Very Positive", "Positive", "Neutral", "Negative", "Very Negative" sentiments based on the passed in text and stop words.
#' @param data - A data frame that must contain the "FragmentID" column and "fragment" column (with this spelling/case) containing the text.
#' @returns A data frame with the sentiments and counts to plot.
#' @export
apply_standard_emotions = function(data, remove_words) {

  # Create a quanteda corpus
  corpus_data <- quanteda::corpus(data, text_field = "fragment", docid_field = "FragmentID")

  # Tokenize the corpus
  tokens_data <- quanteda::tokens(corpus_data, remove_punct = TRUE, remove_symbols = TRUE)
  tokens_data <- quanteda::tokens_remove(tokens_data, stopwords("en"))
  tokens_data <- quanteda::tokens_remove(tokens_data, remove_words)
  # Define the sentiment dictionary, e.g., Loughran-McDonald dictionary
  sentiment_dict <- quanteda.sentiment::data_dictionary_LSD2015

  # Apply the dictionary to perform polarity sentiment analysis
  # Use dfm_lookup to match tokens against the sentiment dictionary
  dfm_data <- quanteda::dfm(tokens_data)

  # Apply sentiment scores based on the dictionary
  sentiment_scores <- quanteda::dfm_lookup(dfm_data, dictionary = sentiment_dict)

  # Compute the polarity score (positive - negative) for each document
  sentiment_polarity <- sentiment_scores[, "positive"] - sentiment_scores[, "negative"]
  sp <- as.vector(sentiment_polarity[,"positive"])
  # Add sentiment polarity scores back to the original data
  data_with_sentiment <- data %>%
    mutate(sentiment_polarity = sp)

 # full_data <- fwd$data$df1
  #full_data_join <- dplyr::inner_join(full_data, data_with_sentiment, by = c(FragmentID =  "fragmentID"))

  full_data_join <- data_with_sentiment %>% dplyr::mutate(sent_emotion = case_when(
    sentiment_polarity >= 2  ~ "Very Positive",
    sentiment_polarity > 0   ~ "Positive",
    sentiment_polarity == 0  ~ "Neutral",
    sentiment_polarity < 0   ~ "Negative",
    sentiment_polarity <= -2 ~ "Very Negative"
  ))
  sentiment_summary <- full_data_join %>%
    count(sent_emotion)
  return(sentiment_summary)

}

#' @title Is the colour code or name valid.
#' @description Determines whether colour characters are valid colour codes/names
#' @param x A vector of colour codes/characters to test
#' @export
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}
