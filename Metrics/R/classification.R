#' @title Inherit Documentation for Classification Metrics
#' @name params_classification
#' @description This object provides the documentation for the parameters of functions
#'              that provide classification metrics
#' @param actual ground truth vector
#' @param predicted predicted vector
params_classification <- NULL

#' Compute the average precision at k
#'
#' This function computes the average precision at k
#' between two sequences
#'
#' @param k max length of predicted sequence
#' @param actual ground truth set (vector)
#' @param predicted predicted sequence (vector)
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' apk(4, actual, predicted)
#' @export
apk <- function(k, actual, predicted) {
    score <- 0.0
    cnt <- 0.0
    for (i in 1:min(k,length(predicted))) {
        if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)])) {
            cnt <- cnt + 1
            score <- score + cnt/i
        }
    }
    return(score / min(length(actual), k))
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two sequences
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' mapk(4, actual, predicted)
#' @export
mapk <- function(k, actual, predicted) {
    if (length(actual) == 0 || length(predicted) == 0) {
        return(0.0)
    }
    
    scores <- rep(0, length(actual))
    for (i in 1:length(scores)) {
        scores[i] <- apk(k, actual[[i]], predicted[[i]])
    }
    return(mean(scores))
}

#' Compute the classification error
#'
#' This function computes the classification error
#' between two vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c('a', 'a', 'c', 'b', 'c')
#' predicted <- c('a', 'b', 'c', 'b', 'a')
#' ce(actual, predicted)
#' @export
ce <- function(actual, predicted) {
    return(mean(actual != predicted))
}

#' Compute the accuracy
#' 
#' This function computes the accuracy score between two vectors
#' 
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @export
#' @examples
#' actual <- c('a', 'a', 'c', 'b', 'c')
#' predicted <- c('a', 'b', 'c', 'b', 'a')
#' accuracy(actual, predicted)
accuracy <- function(actual, predicted) {
    return(1 - ce(actual, predicted))
}

#' Compute the quadratic weighted kappa
#'
#' This function computes the quadratic weighted kappa
#' between two vectors of integers
#'
#' @param rater.a is the first rater's ratings
#' @param rater.b is the second rater's ratings
#' @param min.rating is the minimum possible rating
#' @param max.rating is the maximum possible rating
#' @examples
#' rater.a <- c(1, 4, 5, 5, 2, 1)
#' rater.b <- c(2, 2, 4, 5, 3, 3)
#' ScoreQuadraticWeightedKappa(rater.a, rater.b, 1, 5)
#' @export
ScoreQuadraticWeightedKappa <- function(rater.a
                                        , rater.b
                                        , min.rating = min(c(rater.a, rater.b))
                                        , max.rating = max(c(rater.a, rater.b))
) {
    
    rater.a <- factor(rater.a, levels = min.rating:max.rating)
    rater.b <- factor(rater.b, levels = min.rating:max.rating)
    
    #pairwise frequencies
    confusion.mat <- table(data.frame(rater.a, rater.b))
    confusion.mat <- confusion.mat / sum(confusion.mat)
    
    #get expected pairwise frequencies under independence
    histogram.a <- table(rater.a) / length(table(rater.a))
    histogram.b <- table(rater.b) / length(table(rater.b))
    expected.mat <- histogram.a %*% t(histogram.b)
    expected.mat <- expected.mat / sum(expected.mat)
    
    #get weights
    labels <- as.numeric( as.vector (names(table(rater.a))))
    weights <- outer(labels, labels, FUN = function(x, y) (x - y) ^ 2)
    
    #calculate kappa
    return(1 - sum(weights * confusion.mat) / sum(weights * expected.mat))
}

#' Compute the mean quadratic weighted kappa
#'
#' This function computes the mean quadratic weighted
#' kappa, which can optionally be weighted
#'
#' @param kappas is a vector of possible kappas
#' @param weights is an optional vector of ratings
#' @examples
#' kappas <- c(0.3,0.2,0.2,0.5,0.1,0.2)
#' weights <- c(1.0,2.5,1.0,1.0,2.0,3.0)
#' MeanQuadraticWeightedKappa(kappas, weights)
#' @export
MeanQuadraticWeightedKappa <- function(kappas
                                       , weights = rep(1, length(kappas))
) {
    
    weights <- weights / mean(weights)
    
    max999 <- function(x) sign(x)*min(0.999,abs(x))
    min001 <- function(x) sign(x)*max(0.001,abs(x))
    kappas <- sapply(kappas, max999)
    kappas <- sapply(kappas, min001)
    
    r2z <- function(x) 0.5*log((1+x)/(1-x))
    z2r <- function(x) (exp(2*x)-1) / (exp(2*x)+1)
    kappas <- sapply(kappas, r2z)
    kappas <- kappas * weights
    kappas <- mean(kappas)
    return(z2r(kappas))
}

#' Compute the f1 score
#'
#' This function computes the f1 score between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' f1(actual, predicted)
#' @export
f1 <- function (actual, predicted) {
    act <- unique(actual)
    pred <- unique(predicted)
    
    tp <- length(intersect(act, pred))
    fp <- length(setdiff(pred, act))
    fn <- length(setdiff(act, pred))
    
    precision <- ifelse(tp == 0 && fp==0, 0, tp / (tp + fp))
    recall <- ifelse(tp == 0 && fn==0, 0, tp / (tp + fn))
    return(ifelse(precision == 0 && recall == 0, 0, 2 * precision * recall / (precision + recall)))
}
