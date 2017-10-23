#' @title Inherit Documentation for Binary Classification Metrics
#' @name params_binary
#' @description This object provides the documentation for the parameters of functions
#'              that provide binary classification metrics
#' @param actual ground truth binary numeric vector
#' @param predicted predicted numeric vector
NULL

#' Compute the area under the ROC curve (AUC)
#'
#' This function computes the area under the receiver-operator
#' characteristic curve (AUC)
#'
#' @inheritParams params_binary
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' auc(actual, predicted)
auc <- function(actual, predicted) {
    r <- rank(predicted)
    n_pos <- as.numeric(sum(actual == 1))
    n_neg <- length(actual) - n_pos
    return((sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg))
}

#' Compute the log loss
#'
#' This function computes the elementwise log loss between
#' two numeric vectors
#'
#' @inheritParams params_binary
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' ll(actual, predicted)
#' @export
ll <- function(actual, predicted) {
    score <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    score[actual == predicted] <- 0
    score[is.nan(score)] <- Inf
    return(score)
}

#' Compute the mean log loss
#'
#' This function computes the mean log loss between
#' two numeric vectors
#'
#' @inheritParams params_binary
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' logLoss(actual, predicted)
#' @export
logLoss <- function(actual, predicted) {
    return(mean(ll(actual, predicted)))
}