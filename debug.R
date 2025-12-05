range_fun <- function(ids, ranges) {
  num_in_range <- 0
  for (i in 1:length(ids)) {
    curr_id <- ids[[i]] 
    in_a_range <- 0 
    for (j in 1:nrow(ranges)) {
      if (curr_id >= ranges$range_start[j] & curr_id <= ranges$range_end[j]) {
        in_a_range <- in_a_range + 1
        break
      }
    }
    if (in_a_range > 0) {
      num_in_range <- num_in_range + 1
    }
  }
  return(num_in_range)
}