#' Acoustic distance based on Mel-Frequency Cepstral Coefficients
#'
#' This function implements an acoustic distance based on Mel-Frequency Cepstral Coefficients, upon which dynamic time warping is used to produce the results, as proposed by Bartelds et al. (2020). With an input of two audio files in the Waveform Audio File Format (i.e. wav), the function will return a distance between the two audios.
#'
#' @param file1 The file to compare, which should be in the Waveform Audio File Format (i.e. wav).
#' @param file2 The other audio file to compare against, again as a wav.
#'
#' @references
#' Bartelds, M., Richter, C., Liberman, M., and Wieling, M. 2020. A New Acoustic-Based
#' Pronunciation Distance Measure. **Frontiers in Artificial Intelligence**, 3, May. \doi{10.3389/frai.2020.00039}
#' 
#' @return A number, indicating the distance between the two audio files.
#' @export
#'
#' @examples 
#' # Example 1: The acoustic distance between i and e
#' 
#' i_audio <- system.file("extdata", "i.wav", package="dialectR")
#' e_audio <- system.file("extdata", "e.wav", package="dialectR")
#' try(acoustic_distance(i_audio, e_audio),
#'  message("Python not available for testing"),
#'  silent = TRUE)
acoustic_distance <- function(file1, file2){
  file1 <- .pkg.env$scipy$io$wavfile$read(file1)
  file2 <- .pkg.env$scipy$io$wavfile$read(file2)
  mfcc1 <- .pkg.env$python_speech_features$mfcc(file1[[2]],
                                file1[[1]],
                                winlen = 0.025,
                                winstep = 0.01,
                                preemph = 0.97,
                                numcep = as.integer(12),
                                appendEnergy = TRUE,
                                winfunc = .pkg.env$numpy$hamming,
                                nfft=as.integer(1024))
  mfcc2 <- .pkg.env$python_speech_features$mfcc(file2[[2]],
                                       file2[[1]],
                                       winlen = 0.025,
                                       winstep = 0.01,
                                       preemph = 0.97,
                                       numcep = as.integer(12),
                                       appendEnergy = TRUE,
                                       winfunc = .pkg.env$numpy$hamming,
                                       nfft=as.integer(1024))
  deltas1 <- .pkg.env$python_speech_features$delta(mfcc1, as.integer(2))
  double_deltas1 <- .pkg.env$python_speech_features$delta(deltas1, as.integer(2))
  deltas2 <- .pkg.env$python_speech_features$delta(mfcc2, as.integer(2))
  double_deltas2 <- .pkg.env$python_speech_features$delta(deltas2, as.integer(2))
  combined1 <- .pkg.env$numpy$hstack(list(mfcc1, deltas1, double_deltas1))
  combined2 <- .pkg.env$numpy$hstack(list(mfcc2, deltas2, double_deltas2))
  combined1 <- .pkg.env$speechpy$processing$cmvn(combined1, variance_normalization = TRUE)
  combined2 <- .pkg.env$speechpy$processing$cmvn(combined2, variance_normalization = TRUE)
  res <- dtw::dtw(combined1, combined2, window.type = "slantedband", window.size = 200, distance.only = TRUE)
  res$distance / (ncol(combined1) + ncol(combined2))
  }
