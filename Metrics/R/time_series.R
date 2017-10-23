#' Compute the mean absolute scaled error
#' 
#' This function computes the mean absolute scaled error between two numeric
#' vectors. This function is only intended for time series data, where
#' \code{actual} and \code{numeric} are numeric vectors ordered by time.
#' @param actual ground truth numeric vector ordered in time, with most recent
#'               observation at the end of the vector
#' @param predicted predicted numeric vector ordered in time.
#' @param step_size an integer that specifies how to many observations to look back
#'                  in order to compute the naive forecast.
#' @export
mase <- function(actual, predicted, step_size = 1) {
    
    naive_start <- step_size + 1
    n <- length(actual)
    naive_end <- n - step_size
    
    sum_errors <- sum(ae(actual, predicted))
    naive_errors <- sum(ae(actual[naive_start:n], actual[1:naive_end]))
    return(sum_errors / (n * naive_errors / naive_end))
}