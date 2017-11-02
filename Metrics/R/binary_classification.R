#' @title Inherit Documentation for Binary Classification Metrics
#' @name params_binary
#' @description This object provides the documentation for the parameters of functions
#'              that provide binary classification metrics
#' @param actual The ground truth binary numeric vector containing 1 for the positive
#'               class and 0 for the negative class.
#' @param predicted The predicted binary numeric vector containing 1 for the positive
#'                  class and 0 for the negative class.
NULL

#' Area under the ROC curve (AUC)
#'
#' This function computes the area under the receiver-operator characteristic curve (AUC)
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the smallest values correspond
#'                  to the observations most believed to be in the negative class
#'                  and the largest values correspond to the observations most believed
#'                  to be in the positive class.
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

#' Log Loss
#'
#' This function computes the elementwise log loss between two numeric vectors
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the values correspond
#'                  to the probabilities that each observation belongs to the positive
#'                  class
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

#' Mean Log Loss
#'
#' This function computes the average log loss between two numeric vectors
#'
#' @inheritParams ll
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' logLoss(actual, predicted)
#' @export
logLoss <- function(actual, predicted) {
    return(mean(ll(actual, predicted)))
}
