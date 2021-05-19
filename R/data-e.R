#' Wav file of "e" in the international phonetic alphabet
#'
#' This is a wav file which contains the pronunciation of "e" in the international phonetic alphabet.
#' 
#' @source \url{http://www.phonetics.ucla.edu/course/chapter1/vowels.html}
#' @docType data
#' @name e
#' @format wav file.
#' @examples
#' i_audio <- system.file("extdata", "i.wav", package="dialectR")
#' e_audio <- system.file("extdata", "e.wav", package="dialectR")
#' try(acoustic_distance(i_audio, e_audio),
#'  message("Python not available for testing"),
#'  silent = TRUE)
NULL
