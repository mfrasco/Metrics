#' @title Inherit documentation for regression metrics
#' @name params_regression
#' @description This object provides the documentation for the parameters of functions
#'              that provide regression metrics
#' @param actual ground truth numeric vector
#' @param predicted predicted numeric vector
NULL

#' Compute the squared error
#'
#' This function computes the elementwise squared error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' se(actual, predicted)
#' @export
se <- function(actual, predicted) {
    return((actual - predicted) ^ 2)
}

#' Compute the mean squared error
#'
#' This function computes the mean squared error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mse(actual, predicted)
#' @export
mse <- function(actual, predicted) {
    return(mean(se(actual, predicted)))
}

#' Compute the root mean squared error
#'
#' This function computes the root mean squared error
#' between two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rmse(actual, predicted)
#' @export
rmse <- function(actual, predicted) {
    return(sqrt(mse(actual, predicted)))
}

#' Compute the absolute error
#'
#' This function computes the elementwise absolute error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' ae(actual, predicted)
#' @export
ae <- function(actual, predicted) {
    return(abs(actual - predicted))
}

#' Compute the mean absolute error
#'
#' This function computes the mean absolute error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mae(actual, predicted)
#' @export
mae <- function(actual, predicted) {
    return(mean(ae(actual, predicted)))
}

#' Compute the absolute percent error
#' 
#' This function computes the elementwise percent absolute error between
#' two numeric vectors
#' 
#' @inheritParams params_regression
#' @examples 
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' ape(actual, predicted)
#' @export
ape <- function(actual, predicted) {
    return(ae(actual, predicted) / actual)
}

#' Compute the mean absolute percent error
#' 
#' This function computes the mean absolute percent error between two numeric
#' vectors
#' 
#' @inheritParams params_regression
#' @examples 
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mape(actual, predicted)
#' @export
mape <- function(actual, predicted) {
    return(mean(ape(actual, predicted)))
}

#' Compute the symmetric mean absolute percentage error
#' 
#' This function computes the symmetric mean absolute percentage error between
#' two numeric vectors
#' 
#' @inheritParams params_regression
#' @examples 
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' smape(actual, predicted)
#' @export
smape <- function(actual, predicted) {
    return(2 * mean(ae(actual, predicted) / (abs(actual) + abs(predicted))))
}

#' Compute the squared log error
#'
#' This function computes the elementwise squared log error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' sle(actual, predicted)
#' @export
sle <- function(actual, predicted) {
    return((log(1 + actual) - log(1 + predicted)) ^ 2)
}

#' Compute the mean squared log error
#'
#' This function computes the mean squared log error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' msle(actual, predicted)
#' @export
msle <- function(actual, predicted) {
    mean(sle(actual, predicted))
}

#' Compute the root mean squared log error
#'
#' This function computes the root mean squared log error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rmsle(actual, predicted)
#' @export
rmsle <- function(actual, predicted) {
    sqrt(msle(actual, predicted))
}

#' Compute the relative squared error
#'
#' This function computes the relative squared error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rse(actual, predicted)
#' @export
rse <- function (actual, predicted) {
    return(sum(se(actual, predicted)) / sum(se(actual, mean(actual))))
}

#' Compute the root relative squared error
#'
#' This function computes the root relative squared error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rrse(actual, predicted)
#' @export
rrse <- function(actual, predicted) {
    return(sqrt(rse(actual, predicted)))
}

#' Compute the relative absolute error
#'
#' This function computes the relative absolute error between
#' two numeric vectors
#'
#' @inheritParams params_regression
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' rae(actual, predicted)
#' @export
rae <- function(actual, predicted) {
    return(sum(ae(predicted, actual)) / sum(ae(actual, mean(actual))))
}
