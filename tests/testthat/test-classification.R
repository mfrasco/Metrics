context('Classification metrics')

test_that('classification error is calculated correctly', {
    expect_equal(ce(c(1,1,1,0,0,0),c(1,1,1,0,0,0)), 0.0)
    expect_equal(ce(c(1,1,1,0,0,0),c(1,1,1,1,0,0)), 1/6)
    expect_equal(ce(c(1,2,3,4),c(1,2,3,3)), 1/4)
    expect_equal(ce(c("cat","dog","bird"),c("cat","dog","fish")), 1/3)
    expect_equal(ce(c("cat","dog","bird"),c("caat","doog","biird")), 1.0)
})

test_that('quadratic weighted kappa is calculated correctly', {
    rater.a <- c(1, 2, 1)
    rater.b <- c(1, 2, 2)
    kappa <- ScoreQuadraticWeightedKappa(rater.a, rater.b)
    expect_equal(kappa, 0.4)
    
    rater.a <- c(1, 2, 3, 1, 2, 3)
    rater.b <- c(1, 2, 3, 1, 3, 2)
    kappa <- ScoreQuadraticWeightedKappa(rater.a, rater.b)
    expect_equal(kappa, 0.75)
    
    rater.a <- c(1, 2, 3)
    rater.b <- c(1, 2, 3)
    kappa <- ScoreQuadraticWeightedKappa(rater.a, rater.b)
    expect_equal(kappa, 1.0)
    
    rater.a <- c(1, 3, 5)
    rater.b <- c(2, 4, 6)
    kappa <- ScoreQuadraticWeightedKappa(rater.a, rater.b)
    expect_equal(kappa, 0.8421052631578947)
    
    rater.a <- c(1, 3, 3, 5)
    rater.b <- c(2, 4, 5, 6)
    kappa <- ScoreQuadraticWeightedKappa(rater.a, rater.b, 1, 6)
    expect_equal(kappa, 0.6956521739130435)
})

test_that('mean quadratic weighted kappa is calculated correctly', {
    kappa <- MeanQuadraticWeightedKappa( c(1, 1) )
    expect_equal(kappa, 0.999)
    
    kappa <- MeanQuadraticWeightedKappa( c(1, -1) )
    expect_equal(kappa, 0.0)
    
    kappa <- MeanQuadraticWeightedKappa( c(.5, .8), c(1.0, .5) )
    expect_equal(kappa, 0.624536446425734)
})