context('Metrics functions provide expected results.')

test_that('average precision at k is calculated correctly', {
    expect_equal(apk(2, 1:5, c(6,4,7,1,2)), 1/4)
    expect_equal(apk(5, 1:5, c(1,1,1,1,1)), 0.2)
    expect_equal(apk(20, 1:100, c(1:20,200:600)), 1)
    expect_equal(apk(3, c(1,3), 1:5), 5/6)
    expect_equal(apk(3, 1:3, c(1,1,1)), 1/3)
    expect_equal(apk(3, 1:3, c(1,2,1)), 2/3)
})

test_that('mean average precision at k is calculated correctly', {
    expect_equal(mapk(10, list(1:5,1:3), list(1:10,c(1:2,4:11,3))), 5/6)
    expect_equal(mapk(3, list(1:4), list(1:4)), 1.0)
    expect_equal(mapk(3, list(c(1,3,4),c(1,2,4),c(1,3)), list(1:5,1:5,1:5)), 0.685185185185185)
    expect_equal(mapk(5, list(1:5,1:5), list(c(6,4,7,1,2),c(1,1,1,1,1))), 0.26)
    expect_equal(mapk(3, list(c(1,3),1:3,1:3), list(1:5,c(1,1,1),c(1,2,1))), 11/18)
})

test_that('area under ROC curve is calculated correctly', {
    expect_equal(auc(c(1,0,1,1), c(.32,.52,.26,.86)), 1/3)
    expect_equal(auc(c(1,0,1,0,1),c(.9,.1,.8,.1,.7)), 1)
    expect_equal(auc(c(0,1,1,0),c(.2,.1,.3,.4)), 1/4)
    expect_equal(auc(c(1,1,1,1,0,0,0,0,0,0),0*(1:10)), 0.5)
})

test_that('classification error is calculated correctly', {
    expect_equal(ce(c(1,1,1,0,0,0),c(1,1,1,0,0,0)), 0.0)
    expect_equal(ce(c(1,1,1,0,0,0),c(1,1,1,1,0,0)), 1/6)
    expect_equal(ce(c(1,2,3,4),c(1,2,3,3)), 1/4)
    expect_equal(ce(c("cat","dog","bird"),c("cat","dog","fish")), 1/3)
    expect_equal(ce(c("cat","dog","bird"),c("caat","doog","biird")), 1.0)
})

test_that('f1 score is calculated correctly', {
    expect_equal(f1(c(3,4,5),c(3,4)), 0.8)
    expect_equal(f1(7,1), 0)
    expect_equal(f1(7,c(1,1)), 0)
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

test_that('mean quadratic weighted kappa is calculated correctly', {
    expect_equal(ll(1,1), 0)  
    expect_equal(ll(1,0), Inf)
    expect_equal(ll(0,1), Inf)
    expect_equal(ll(1,0.5), -log(0.5))
})

test_that('mean quadratic weighted kappa is calculated correctly', {
    expect_equal(logLoss(c(1,1,0,0),c(1,1,0,0)), 0)
    expect_equal(logLoss(c(1,1,0,0),c(1,1,1,0)), Inf)
    expect_equal(logLoss(c(1,1,1,0,0,0),c(.5,.1,.01,.9,.75,.001)), 1.881797068998267)
})
