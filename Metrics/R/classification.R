#' @title Inherit Documentation for Classification Metrics
#' @name params_classification
#' @description This object provides the documentation for the parameters of functions
#'              that provide classification metrics
#' @param actual The ground truth vector, where elements of the vector can be any variable type.
#' @param predicted The predicted vector, where elements of the vector represent a
#'                  prediction for the corresponding value in \code{actual}.
NULL

#' Classification Error
#'
#' \code{ce} is defined as the proportion of elements in \code{actual} that are not equal
#' to the corresponding element in \code{predicted}.
#'
#' @inheritParams params_classification
#' @export
#' @seealso \code{\link{accuracy}}
#' @examples
#' actual <- c('a', 'a', 'c', 'b', 'c')
#' predicted <- c('a', 'b', 'c', 'b', 'a')
#' ce(actual, predicted)
ce <- function(actual, predicted) {
    return(mean(actual != predicted))
}

#' Accuracy
#' 
#' \code{accuracy} is defined as the proportion of elements in \code{actual} that are
#' equal to the corresponding element in \code{predicted}
#' 
#' @inheritParams params_classification
#' @export
#' @seealso \code{\link{ce}}
#' @examples
#' actual <- c('a', 'a', 'c', 'b', 'c')
#' predicted <- c('a', 'b', 'c', 'b', 'a')
#' accuracy(actual, predicted)
accuracy <- function(actual, predicted) {
    return(1 - ce(actual, predicted))
}

#' Quadratic Weighted Kappa
#'
#' \code{ScoreQuadraticWeightedKappa} computes the quadratic weighted kappa between
#' two vectors of integers
#'
#' @param rater.a An integer vector of the first rater's ratings.
#' @param rater.b An integer vector of the second rater's ratings.
#' @param min.rating The minimum possible rating.
#' @param max.rating The maximum possible rating.
#' @export
#' @seealso \code{\link{MeanQuadraticWeightedKappa}}
#' @examples
#' rater.a <- c(1, 4, 5, 5, 2, 1)
#' rater.b <- c(2, 2, 4, 5, 3, 3)
#' ScoreQuadraticWeightedKappa(rater.a, rater.b, 1, 5)
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

#' Mean Quadratic Weighted Kappa
#'
#' \code{MeanQuadraticWeightedKappa} computes the mean quadratic weighted kappa,
#' which can optionally be weighted
#'
#' @param kappas A numeric vector of possible kappas.
#' @param weights An optional numeric vector of ratings.
#' @export
#' @seealso \code{\link{ScoreQuadraticWeightedKappa}}
#' @examples
#' kappas <- c(0.3 ,0.2, 0.2, 0.5, 0.1, 0.2)
#' weights <- c(1.0, 2.5, 1.0, 1.0, 2.0, 3.0)
#' MeanQuadraticWeightedKappa(kappas, weights)
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
