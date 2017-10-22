#' Compute the squared error
#'
#' This function computes the elementwise squared error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' se(actual, predicted)
#' @export
se <- function (actual, predicted) (actual-predicted)^2

#' Compute the mean squared error
#'
#' This function computes the mean squared error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' mse(actual, predicted)
#' @export
mse <- function (actual, predicted) mean(se(actual, predicted))

#' Compute the root mean squared error
#'
#' This function computes the root mean squared error
#' between two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rmse(actual, predicted)
#' @export
rmse <- function (actual, predicted) sqrt(mse(actual, predicted))

#' Compute the absolute error
#'
#' This function computes the elementwise absolute error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' ae(actual, predicted)
#' @export
ae <- function (actual, predicted) abs(actual-predicted)

#' Compute the mean absolute error
#'
#' This function computes the mean absolute error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' mae(actual, predicted)
#' @export
mae <- function (actual, predicted) mean(ae(actual, predicted))

#' Compute the squared log error
#'
#' This function computes the elementwise squared log error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' sle(actual, predicted)
#' @export
sle <- function (actual, predicted) (log(1+actual)-log(1+predicted))^2

#' Compute the mean squared log error
#'
#' This function computes the mean squared log error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' msle(actual, predicted)
#' @export
msle <- function (actual, predicted) mean(sle(actual, predicted))

#' Compute the root mean squared log error
#'
#' This function computes the root mean squared log error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rmsle(actual, predicted)
#' @export
rmsle <- function (actual, predicted) sqrt(msle(actual, predicted))

#' Compute the relative squared error
#'
#' This function computes the relative squared error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rse(actual, predicted)
#' @export
rse <- function (actual, predicted) {
  sum(se(actual, predicted)) / sum(se(actual, mean(actual)))
}

#' Compute the root relative squared error
#'
#' This function computes the root relative squared error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' rrse(actual, predicted)
#' @export
rrse <- function (actual, predicted) sqrt(rse(actual, predicted))

#' Compute the relative absolute error
#'
#' This function computes the relative absolute error between
#' two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' rae(actual, predicted)
#' @export
rae <- function (actual, predicted) {
  sum(ae(predicted, actual)) / sum(ae(actual, mean(actual)))
}

#' Compute the area under the ROC curve (AUC)
#'
#' This function computes the area under the receiver-operator
#' characteristic curve (AUC)
#'
#' @param actual binary ground truth vector
#' @param predicted predicted vector (defines the ranking)
#' @export
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(0.9,0.8,0.4,0.5,0.3,0.2)
#' auc(actual, predicted)
auc <- function(actual, predicted) {
  r <- rank(predicted)
  n_pos <- sum(actual==1)
  n_neg <- length(actual) - n_pos
  auc <- (sum(r[actual==1]) - n_pos*(n_pos+1)/2) / (n_pos*n_neg)
  auc
}

#' Compute the log loss
#'
#' This function computes the elementwise log loss between
#' two numeric vectors
#'
#' @param actual binary ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' ll(actual, predicted)
#' @export
ll <- function(actual, predicted) {
  score <- -(actual*log(predicted) + (1-actual)*log(1-predicted))
  score[actual==predicted] <- 0
  score[is.nan(score)] <- Inf
  score
}

#' Compute the mean log loss
#'
#' This function computes the mean log loss between
#' two numeric vectors
#'
#' @param actual binary ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' logLoss(actual, predicted)
#' @export
logLoss <- function(actual, predicted) mean(ll(actual, predicted))

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
  score <- score / min(length(actual), k)
  score
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
mapk <- function (k, actual, predicted) {
  if ( length(actual)==0 || length(predicted)==0 ) {
    return(0.0)
  }

  scores <- rep(0, length(actual))
  for (i in 1:length(scores)) {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}

#' Compute the classification error
#'
#' This function computes the classification error
#' between two numeric vectors
#'
#' @param actual ground truth vector
#' @param predicted predicted vector
#' @examples
#' actual <- c(1,1,1,0,0,0)
#' predicted <- c(1,0,1,1,0,0)
#' ce(actual, predicted)
#' @export
ce <- function (actual, predicted) {
  cntError <- 0.0
  for (i in 1:length(actual)) {
    if (actual[i] != predicted[i]) {
      cntError <- cntError + 1
    }
  }
  score <- cntError / length(actual)
  score
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
#' rater.a <- c(1,4,5,5,2,1)
#' rater.b <- c(2,2,4,5,3,3)
#' ScoreQuadraticWeightedKappa(rater.a, rater.b, 1, 5)
#' @export
ScoreQuadraticWeightedKappa <- function(rater.a ,
                                        rater.b,
                                        min.rating,
                                        max.rating) {

  if (missing(min.rating)) {
    min.rating <- min(min(rater.a),min(rater.b))
  }
  if (missing(max.rating)) {
    max.rating <- max(max(rater.a),max(rater.b))
  }

  rater.a <- factor(rater.a, levels<-min.rating:max.rating)
  rater.b <- factor(rater.b, levels<-min.rating:max.rating)

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
  weights <- outer(labels, labels, FUN <- function(x,y) (x-y)^2)

  #calculate kappa
  kappa <- 1 - sum(weights*confusion.mat)/sum(weights*expected.mat)
  kappa
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
MeanQuadraticWeightedKappa <- function(kappas, weights) {

  if (missing(weights)) {
    weights <- rep(1, length(kappas))
  } else {
    weights <- weights / mean(weights)
  }

  max999 <- function(x) sign(x)*min(0.999,abs(x))
  min001 <- function(x) sign(x)*max(0.001,abs(x))
  kappas <- sapply(kappas, max999)
  kappas <- sapply(kappas, min001)

  r2z <- function(x) 0.5*log((1+x)/(1-x))
  z2r <- function(x) (exp(2*x)-1) / (exp(2*x)+1)
  kappas <- sapply(kappas, r2z)
  kappas <- kappas * weights
  kappas <- mean(kappas)
  kappas <- z2r(kappas)
  kappas
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

  tp <- length(intersect(act,pred))
  fp <- length(setdiff(pred,act))
  fn <- length(setdiff(act,pred))

  precision <- ifelse ((tp==0 & fp==0), 0, tp/(tp+fp))
  recall <- ifelse ((tp==0 & fn==0), 0, tp/(tp+fn))

  score <- ifelse ((precision==0 & recall==0), 0, 2*precision*recall/(precision+recall))
  score
 }
