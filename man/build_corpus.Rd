% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_helpers.R
\name{build_corpus}
\alias{build_corpus}
\title{Build a corpus from a list of character data}
\usage{
build_corpus(
  df,
  framework_data,
  freetext_id,
  doc_var,
  min_term_freq = 3,
  languages = "en"
)
}
\arguments{
\item{df}{\itemize{
\item The data frame to create the corpus from - must include columns named in freetext_id and doc_var parameters.
}}

\item{framework_data}{\itemize{
\item The framework data object.
}}

\item{freetext_id}{\itemize{
\item The freetext column name containing the text to create the corpus from
}}

\item{doc_var}{\itemize{
\item The column name to use as the document variable.
}}

\item{min_term_freq}{\itemize{
\item Default 3, number of occurrence of a term before it it is accepted into the corpus.
}}

\item{languages}{\itemize{
\item Default "en", a vector of supported 2 character language codes for use in stop words and stemming.
}}
}
\value{
Returns a named list of tokens_stem (the stemmed tokens with stop words removed), tokens_unstem (the unstemmed tokens with stop words removed), dtm (the document term matrix not trimmed) and dtm_trim, the document term matrix trimmed to the min_term_freq value.
}
\description{
This function uses the library quanteda to create a token list (stemmed and unstemmed both with stop words removed), the document term matrix and the trimmed document term matrix.
The data is returned as a list of lists.
}
