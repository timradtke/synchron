
get_weights <- function(alpha, window_size) {
  steps <- (window_size-1):0
  weight <- exp(-alpha * steps) / sum(exp(-alpha * steps))
  
  return(weight)
}

#' Create vector sample paths
#' 
#' @param df
#' @param n_samples
#' @param window_size
#' @param na_fill
#' 
#' @examples
sample_paths <- function(df, 
                       h, 
                       window_size, 
                       alpha = 0.1, 
                       n_samples = 1000, 
                       na_fill = 0) {
  
  if(!all(c("id", "date", "value") %in% names(df))) {
    stop("df must have columns: `id`, `date`, `value`.")
  }
  
  max_date <- max(df$date)
  
  n_dates <- length(unique(df$date))
  tmp_window_size <- window_size
  if(n_dates < tmp_window_size) {
    tmp_window_size <- n_dates
  }
  
  tmp_weights <- get_weights(alpha = alpha, window_size = tmp_window_size)
  
  df_paths <- df %>%
    dplyr::arrange(id, date) %>%
    tidyr::spread(id, value) %>%
    dplyr::group_by(date) %>%
    dplyr::sample_n(size = n_samples, replace = TRUE) %>%
    dplyr::mutate(path = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date)
  
  for(i in 1:h) {
    tmp_samples <- df_paths %>%
      dplyr::group_by(path) %>%
      dplyr::slice_tail(n = context_length) %>%
      dplyr::mutate(weight = tmp_weights) %>%
      dplyr::sample_n(size = 1, replace = FALSE, weight = weight) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(date = max_date + months(i)) %>%
      dplyr::select(-weight)
    
    df_paths <- dplyr::bind_rows(df_paths, tmp_samples)
    
    if((i != h) & (tmp_window_size < window_size)) {
      tmp_window_size <- tmp_window_size + 1
      tmp_weights <- get_weights(alpha = alpha, window_size = tmp_window_size)
    }
  }
  
  df_paths <- df_paths[df_paths$date > max_date,]
  
  return(df_paths)
}
