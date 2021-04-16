numpy <- NULL
sklearn <- NULL
scipy <- NULL
python_speech_features <- NULL
speechpy <- NULL

.onLoad <- function(libname, pkgname) {
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
  python_speech_features <<- reticulate::import("python_speech_features", delay_load = TRUE)
  speechpy <<- reticulate::import("speechpy", delay_load = TRUE)
}

acoustic_distance <- function(file1, file2){
  file1 <- scipy$io$wavfile$read(file1)
  file2 <- scipy$io$wavfile$read(file2)
  mfcc1 <- python_speech_features$mfcc(file1[[2]],
                                file1[[1]],
                                winlen = 0.025,
                                winstep = 0.01,
                                preemph = 0.97,
                                numcep = as.integer(12),
                                appendEnergy = TRUE,
                                winfunc = numpy$hamming,
                                nfft=as.integer(1024))
  mfcc2 <- python_speech_features$mfcc(file2[[2]],
                                       file2[[1]],
                                       winlen = 0.025,
                                       winstep = 0.01,
                                       preemph = 0.97,
                                       numcep = as.integer(12),
                                       appendEnergy = TRUE,
                                       winfunc = numpy$hamming,
                                       nfft=as.integer(1024))
  deltas1 <- python_speech_features$delta(mfcc1, as.integer(2))
  double_deltas1 <- python_speech_features$delta(deltas1, as.integer(2))
  deltas2 <- python_speech_features$delta(mfcc2, as.integer(2))
  double_deltas2 <- python_speech_features$delta(deltas2, as.integer(2))
  combined1 <- numpy$hstack(list(mfcc1, deltas1, double_deltas1))
  combined2 <- numpy$hstack(list(mfcc2, deltas2, double_deltas2))
  combined1 <- speechpy$processing$cmvn(combined1, variance_normalization = TRUE)
  combined2 <- speechpy$processing$cmvn(combined2, variance_normalization = TRUE)
  res <- dtw::dtw(combined1, combined2, window.type = "slantedband", window.size = 200, distance.only = TRUE)
  res$distance / (ncol(combined1) + ncol(combined2))
  }