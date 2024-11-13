library(tuneR)
setwd("C:/Users/61436/Dropbox/Hobbies/Wavs/envelopes")
source("wav2h.R")

all_wavs <-
  list.files("_wav out/", pattern = "\\.wav", full.names = TRUE)

lapply(all_wavs, function(wav_file) {
  # read in wav and rectify
  wav <- mono(readWave(wav_file))
  wav <- abs(wav)
  wav <- scale(wav)
  wav <- trim(wav)
  plot(wav)
  
  # add some zeros so envelope doesn't rise too sharply...
  wav@left <- c(rep(0, 100), wav@left)
  to_output <- moving_average(wav, 100)
  plot(to_output)
  
  # save C++ object
  sink(paste0("_h out/", gsub(".wav", ".h", basename(wav_file))))
  cat(as_cpp(
    to_output,
    gsub(".wav", "", basename(wav_file)),
    reduction_factor = 100,
    max_int_size = 3999
  ))
  sink()
})
