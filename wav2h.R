`%+%` <- function(a, b)
  paste0(a, b)

setGeneric("moving_average",
           function(x, n = 5) {
             res <- as.numeric(filter(x, rep(1 / n, n), sides = 2))
             res <- res[!is.na(res)]
             res
           })

# compute a moving average
setMethod("moving_average", "Wave",
          function(x, n = 5) {
            x@left <- c(rep(0, n), x@left, rep(0, n))
            x@left = moving_average(x@left, n = n)
            x
          })

# down-sample wave by taking every k-th value
down_sample <- function(wav, reduction_factor) {
  reduction_flag = 1:length(wav) %% reduction_factor
  include = which(reduction_flag == 0)
  wav@left = wav@left[include]
  wav
}

# scale between -1 and 1
scale.Wave <- function(x, center = NA, scale = NA) {
  x@left = x@left / max(abs(x@left), na.rm = TRUE)
  x
}

# take absolute value
abs.Wave <- function(x) {
  x@left = ifelse(x@left <= 0, 0, x@left)
  x
}

# remove leading and lagging silence
trim <- function(x, min_volume = 0.01) {
  start = head(which(x@left > min_volume), 1)
  end = tail(which(x@left > min_volume), 1)
  x = x[start:end]
  x
}

# split a wave into a list waves, similar to strsplit() method:
#  > strips out any audio quieter than min_volume argument
#  > removes any segments shorter than min_wave_size
wavsplit <-
  function(x,
           min_volume = 0,
           min_wave_size = x@samp.rate / 10) {
    
    # assign every element in wave a group:
    #  when zero is encountered, move on to next group
    group_vector = vector("numeric", length(x@left))
    group_label = 1
    for (i in 2:length(x@left)) {
      if (abs(x@left[i]) <= min_volume) {
        group_vector[i] = 0
        if (abs(x@left[i - 1]) > min_volume) {
          group_label = group_label + 1
        }
      } else{
        group_vector[i] = group_label
      }
    }
    
    # tally how big each group is
    #  (ignore group 0 since that is space between groups)
    # then select the large groups
    group_sizes = table(group_vector[group_vector != 0])
    large_groups = group_sizes[group_sizes > min_wave_size]
    large_groups = as.numeric(names(large_groups))
    
    # split up the wav and return a list of fragments
    lapply(large_groups, function(g) {
      x[which(group_vector == g)]
    })
  }

# print values of wave as a C++ object definition
as_cpp <- function(wav,
                   label,
                   object_type = "byte",
                   max_int_size = 255) {
  
  # convert wave values to range from -max_int_size to +max_int_size
  v = wav@left
  values_to_write = round(v / max(abs(v)) * max_int_size)
  int_size = length(values_to_write)
  
  # print!!
  list(
    "object_name" = "sample_" %+% label,
    
    "object_size" = int_size,
    
    "cpp_code" = paste0(
      "const ",
      object_type,
      " sample_",
      label,
      "[",
      int_size,
      "] = {",
      paste0(values_to_write, collapse = ", "),
      "};\n"
    )
  )
}
