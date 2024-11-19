library(tuneR)
setwd("C:/Users/61436/Dropbox/Hobbies/Electronics/GitHub/wav2h")
source("wav2h.R")

MOVING_AVERAGE_WINDOW = 100
SAVE_ONE_IN_EVERY = 50
PROJECT_FOLDER = "toy-animal-sounds/"
WAV_FOLDER = "_wav out/"
OUTPUT_DIR = "_h out/"
OUTPUT_FILE = "sample_bank.h"
CPP_OBJECT_TYPE = "byte"
MAX_VALUE = 255

###
# Set up

dir.create(PROJECT_FOLDER %+% OUTPUT_DIR)
sink(PROJECT_FOLDER %+% OUTPUT_DIR %+% OUTPUT_FILE) # erase file
cat("")
sink()

###
# Load in all WAV files

all_wavs <- list.files(PROJECT_FOLDER %+% WAV_FOLDER,
                       pattern = "\\.wav",
                       full.names = TRUE)

###
# Convert to C++ code

list_of_all_object_names = vector("list", length(all_wavs))
list_of_all_object_sizes = vector("list", length(all_wavs))
i = 1
lapply(all_wavs, function(wav_file) {
  file_prefix = gsub(".wav", "", basename(wav_file))
  
  # read in wav and rectify
  wav <- mono(readWave(wav_file))
  wav <- abs(wav)
  wav <- scale(wav)
  wav <- trim(wav)
  plot(wav)
  
  # add some zeros so envelope doesn't rise too sharply...
  wav@left <- c(rep(0, MOVING_AVERAGE_WINDOW), wav@left)
  to_output <- moving_average(wav, MOVING_AVERAGE_WINDOW)
  plot(to_output)
  
  cpp_code <-
    as_cpp(
      to_output,
      file_prefix,
      reduction_factor = SAVE_ONE_IN_EVERY,
      object_type = CPP_OBJECT_TYPE,
      max_int_size = MAX_VALUE
    )
  
  # save C++ object
  sink(PROJECT_FOLDER %+% OUTPUT_DIR %+% OUTPUT_FILE, append = TRUE)
  cat(cpp_code$cpp_code)
  cat("\n")
  sink()
  
  # save object details for later
  list_of_all_object_names[[i]] <<- cpp_code$object_name
  list_of_all_object_sizes[[i]] <<- cpp_code$object_size
  i <<- i + 1
})

###
# Finish with pointer array

sink(PROJECT_FOLDER %+% OUTPUT_DIR %+% OUTPUT_FILE, append = TRUE)
cat(CPP_OBJECT_TYPE %+% "* sample_array[] = {")
cat(paste0(list_of_all_object_names, collapse = ", "))
cat("};")
cat("\n")
cat("\n")

cat("int sample_sizes[] = {")
cat(paste0(list_of_all_object_sizes, collapse = ", "))
cat("};")
cat("\n")
sink()
