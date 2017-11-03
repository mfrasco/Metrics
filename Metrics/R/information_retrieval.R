#' F1 Score
#'
#' \code{f1} computes the F1 Score in the context of information retrieval problems.
#' 
#' \code{f1} is defined as \eqn{2 * precision * recall / (precision + recall)}. In the
#' context of information retrieval problems, precision is the proportion of retrieved
#' documents that are relevant to a query and recall is the proportion of relevant
#' documents that are successfully retrieved by a query. If there are zero relevant
#' documents that are retrieved, zero relevant documents, or zero predicted documents,
#' \code{f1} is defined as \code{0}.
#' 
#' @export
#' @seealso \code{\link{apk}} \code{\link{mapk}}
#' @param actual The ground truth vector of relevant documents. The vector can contain
#'               any numeric or character values, order does not matter, and the
#'               vector does not need to be the same length as \code{predicted}.
#' @param predicted The predicted vector of retrieved documents. The vector can contain
#'                  any numeric or character values, order does not matter, and the
#'                  vector does not need to be the same length as \code{actual}.
#' @examples
#' actual <- c('a', 'c', 'd')
#' predicted <- c('d', 'e')
#' f1(actual, predicted)
f1 <- function (actual, predicted) {
    act <- unique(actual)
    pred <- unique(predicted)
    
    tp <- length(intersect(act, pred))
    fp <- length(setdiff(pred, act))
    fn <- length(setdiff(act, pred))
    
    if (tp == 0) {
        return(0)
    } else {
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        return(2 * precision * recall / (precision + recall))
    }
}

#' Average Precision at k
#'
#' \code{apk} computes the average precision at k, in the context of information
#' retrieval problems.
#' 
#' \code{apk} loops over the first k values of \code{predicted}. For each value, if
#' the value is contained within \code{actual} and has not been predicted before,
#' we increment the number of sucesses by one and increment our score by the number
#' of successes divided by k. Then, we return our final score divided by the number
#' of relevant documents (i.e. the length of \code{actual}).
#' 
#' \code{apk} will return \code{NaN} if \code{length(actual)} equals \code{0}.
#'
#' @param k The number of elements of \code{predicted} to consider in the calculation.
#' @inheritParams f1
#' @param predicted The predicted vector of retrieved documents. The vector can
#'                  contain any numeric of character values. However, unlike \code{actual},
#'                  order does matter, with the most documents deemed most likely to
#'                  be relevant at the beginning.
#' @export
#' @seealso \code{\link{apk}} \code{\link{f1}}
#' @examples
#' actual <- c('a', 'b', 'd')
#' predicted <- c('b', 'c', 'a', 'e', 'f')
#' apk(3, actual, predicted)
apk <- function(k, actual, predicted) {
    score <- 0.0
    cnt <- 0.0
    for (i in 1:min(k,length(predicted))) {
        if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)])) {
            cnt <- cnt + 1
            score <- score + cnt / i
        }
    }
    return(score / min(length(actual), k))
}

#' Mean Average Precision at k
#'
#' \code{mapk} computes the mean average precision at k for a set of predictions, in
#' the context of information retrieval problems.
#' 
#' \code{mapk} evaluates \code{apk} for each pair of elements from \code{actual} and
#' \code{predicted}.
#'
#' @inheritParams apk
#' @param actual A list of vectors, where each vector represents a ground truth vector
#'               of relevant documents. In each vector, the elements can be numeric
#'               or character values, and the order of the elements does not matter.
#' @param predicted A list of vectors, where each vector represents the predicted vector
#'                  of retrieved documents for the corresponding element of \code{actual}.
#'                  In each vector, the order of the elements does matter, with the
#'                  elements believed most likely to be relevant at the beginning.
#' @export
#' @seealso \code{\link{apk}} \code{\link{f1}}
#' @examples
#' actual <- list(c('a', 'b'), c('a'), c('x', 'y', 'b'))
#' predicted <- list(c('a', 'c', 'd'), c('x', 'b', 'a', 'b'), c('y'))
#' mapk(2, actual, predicted)
#' 
#' actual <- list(c(1, 5, 7, 9), c(2, 3), c(2, 5, 6))
#' predicted <- list(c(5, 6, 7, 8, 9), c(1, 2, 3), c(2, 4, 6, 8))
#' mapk(3, actual, predicted)
mapk <- function(k, actual, predicted) {
    if (length(actual) == 0 || length(predicted) == 0) {
        return(0.0)
    }
    
    convert_to_list <- function(x) {
        if (is.list(x)) {
            return(x)
        } else {
            arg_name <- deparse(substitute(x))
            warning(paste(arg_name, 'should be a list of vectors. Converting to a list.'))
            return(list(x))
        }
    }
    
    act <- convert_to_list(actual)
    pred <- convert_to_list(predicted)
    
    scores <- rep(0, length(act))
    for (i in 1:length(scores)) {
        scores[i] <- apk(k, act[[i]], pred[[i]])
    }
    return(mean(scores))
}

