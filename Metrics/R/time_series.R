#' Mean Absolute Scaled Error
#' 
#' \code{mase} computes the mean absolute scaled error between two numeric
#' vectors. This function is only intended for time series data, where
#' \code{actual} and \code{numeric} are numeric vectors ordered by time.
#' 
#' @param actual The ground truth numeric vector ordered in time, with most recent
#'               observation at the end of the vector.
#' @param predicted The predicted numeric vector ordered in time, where each element
#'                  of the vector represents a prediction for the corresponding
#'                  element of \code{actual}.
#' @param step_size A positive integer that specifies how many observations to look back
#'                  in time in order to compute the naive forecast. The default is
#'                  \code{1}, which means that the naive forecast for the current time
#'                  period is the actual value of the previous period.
#'                  
#'                  However, if \code{actual} and \code{predictions} were quarterly
#'                  predictions over many years, letting \code{step_size = 4}, would
#'                  mean that the naive forecast for the current time period would
#'                  be the actual value from the same quarter last year. In this way,
#'                  \code{mase} can account for seasonality.
#' @export
#' @seealso \code{\link{smape}} \code{\link{mape}}
#' @examples 
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' step_size <- 1
#' mase(actual, predicted, step_size)
mase <- function(actual, predicted, step_size = 1) {
    
    naive_start <- step_size + 1
    n <- as.numeric(length(actual))
    naive_end <- n - step_size
    
    sum_errors <- sum(ae(actual, predicted))
    naive_errors <- sum(ae(actual[naive_start:n], actual[1:naive_end]))
    return(sum_errors / (n * naive_errors / naive_end))
}
