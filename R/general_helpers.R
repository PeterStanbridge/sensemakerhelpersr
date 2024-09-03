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

createBrewerColourArray <- function() {
  cs <- c("YlOrRd", "YlOrBl", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GbBu", "BuPu", "BuGn", "Blues", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
  bins <- c(rep(9, each = 18), rep(11, each = 9))
  brewCols <- as.list(bins)
  names(brewCols) <- cs
  return(brewCols)
}



