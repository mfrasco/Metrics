#' Mean Absolute Scaled Error
#' 
#' \code{mase} computes the mean absolute scaled error between two numeric
#' vectors. This function is only intended for time series data, where
#' \code{actual} and \code{numeric} are numeric vectors ordered by time.
#' 
#' @param actual ground truth numeric vector ordered in time, with most recent
#'               observation at the end of the vector
#' @param predicted predicted numeric vector ordered in time.
#' @param step_size an integer that specifies how to many observations to look back
#'                  in order to compute the naive forecast.
#' @export
#' @seealso \code{\link{smape}} \code{\link{mape}}
#' @examples 
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' step_size <- 1
#' mase(actual, predicted, step_size)
mase <- function(actual, predicted, step_size = 1) {
    
    naive_start <- step_size + 1
    n <- length(actual)
    naive_end <- n - step_size
    
    sum_errors <- sum(ae(actual, predicted))
    naive_errors <- sum(ae(actual[naive_start:n], actual[1:naive_end]))
    return(sum_errors / (n * naive_errors / naive_end))
}