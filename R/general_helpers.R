#' @export
wrap_text <- function(ttext, tlength = 30, treplacement = "") {
  ret_val <- stringr::str_wrap(ttext, width = tlength)
  if (treplacement != "") {
    ret_val <- stringr::str_replace_all(ret_val, "\\n", treplacement)
  }
  return(ret_val)
}

# generic remove html helper function
removeHTML <- function(tString) {
  ret_val <- gsub("<.*?>", " ", tString)
  return(stringr::str_replace_all(ret_val, "  ", " "))
}

clearHTML <- function(tString) {
  return(removeHTML(tString))
}

clearBetweenHTMLTags <- function(htmlString, repVal) {
  return(gsub("<[^>]+>", repVal, htmlString))
}

clearDash <- function(htmlString) {
  return(gsub("-", "", htmlString))
}

clearLongDash <- function(htmlString) {
  return(gsub("–", "", htmlString))
}

clearPlus <- function(htmlString) {
  return(gsub("\\+", "", htmlString))
}

clearBrackets <- function(htmlString) {
  return(gsub("[()]", "", htmlString))
}
clearSpace <- function(htmlString) {
  return(gsub(" ", "", htmlString))
}
clearComma <- function(htmlString) {
  return(gsub(",", "", htmlString))
}
clearQuote <- function(htmlString) {
  return(gsub("\\'", "", htmlString))
}
clearPoint <- function(htmlString) {
  return(gsub("\\.", "", htmlString))
}
clearSemiColon <- function(htmlString) {
  return(gsub(";", "", htmlString))
}

clearSlash <- function(htmlString) {
  return(gsub("/", "", htmlString))
}

clearColon <- function(htmlString) {
  return(gsub(":", "", htmlString))
}

clearApos <- function(htmlString) {
  return(gsub("’", "", htmlString))
}

clearAmps <- function(htmlString) {
  return(gsub("&", "", htmlString))
}

clearPer <- function(htmlString) {
  return(gsub("%", "", htmlString))
}
clearCloseDots <- function(htmlString) {
  return(gsub("…", "", htmlString))
}

clearAll <- function(htmlString) {
  return(htmlString %>% clearHTML()  %>% clearDash()  %>% clearBrackets() %>% clearCloseDots()   %>% clearComma() %>% clearQuote() %>% clearPoint() %>% clearSemiColon() %>% clearSlash() %>% clearApos() %>% clearAmps() %>% clearPer() %>% clearLongDash() %>% clearPlus() %>% clearColon())
}

# gendric is not blank, null or na helper function.
is_not_blank_null_na <- function(x) {
  if (length(x) == 0) {return(FALSE)}
  if (is.null(x)) {return(FALSE)}
  if (is.na(x)) {return(FALSE)}
  if (x == "") {return(FALSE)}
  return(TRUE)
}

# gendric is  blank, null or na helper function.
is_blank_null_na <- function(x) {
  if (length(x) == 0) {return(TRUE)}
  if (is.null(x)) {return(TRUE)}
  if (is.na(x)) {return(TRUE)}
  if (x == "") {return(TRUE)}
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



