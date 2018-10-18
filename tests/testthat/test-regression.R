context('Regression metrics.')

test_that('bias is calculated correctly', {
    expect_equal(bias(1, 1), 0)
    expect_equal(bias(c(-1, -100, 17.5), c(0, 0, 0)), mean(c(-1, -100, 17.5)))
})

test_that('percent_bias is calculated correctly', {
    expect_equal(percent_bias(c(1, 2, 3), c(1, 3, 2)), mean(c(0, -1/2, 1/3)))
    expect_equal(percent_bias(c(1, 2, 0), c(1, 2, 1)), -Inf)
    expect_equal(percent_bias(0, 0), NaN)
    expect_equal(percent_bias(c(-1.1, 1.1), c(-1, 1)), 0)
})

test_that('squared error is calculated correctly', {
    expect_equal(se(3.4, 4.4), 1)
    expect_equal(se(9:11, 11:9), c(4, 0, 4))
})

test_that('sum of squared errors is calculated correctly', {
    expect_equal(sse(c(1, 3, 2), c(2, 3, 4)), 5)
})

test_that('mean squared error is calculated correctly', {
    expect_equal(mse(1:4, c(2, 3, 4, 4)), 3 / 4)
})

test_that('root mean squared error is calculated correctly', {
    expect_equal(rmse(1:4, c(1, 2, 3, 5)), sqrt(1 / 4))
    expect_equal(rmse(1:4, 1:4), 0)
})

test_that('absolute error is calculated correctly', {
    expect_equal(ae(3.4, 4.4), 1)
    expect_equal(ae(9:11, 11:9), c(2, 0, 2))
})

test_that('mean absolute error is calculated correctly', {
    expect_equal(mae(1:4, c(1, 2, 3, 5)), 0.25)
})

test_that('median absolute error is calculated correctly', {
    expect_equal(mdae(1:4, c(1, 2, 4, 50)), 0.5)
})

test_that('absolute percent error is calculated correctly', {
    expect_equal(ape(0:3, 1:4), c(Inf, 1, 1/2, 1/3))
    expect_equal(ape(0:2, c(0, 0, 0)), c(NaN, 1, 1))
    expect_equal(ape(c(-1.1, 1.1), c(-1, 1)), c(1 / 11, 1 / 11))
})

test_that('mean absolute percent error is calculated correctly', {
    expect_equal(mape(1:3, 2:4), mean(c(1, 1/2, 1/3)))
    expect_equal(mape(c(-1.1, 1.1), c(-1, 1)), 1 / 11)
})

test_that('symmetric mean absolute percent error is calculated correctly', {
    expect_equal(smape(0, 0), NaN)
    expect_equal(smape(1, -1), 2)
    expect_equal(smape(1, 0), 2)
    expect_equal(smape(c(1, 2, 3), c(2, 5, 4)), smape(c(2, 5, 4), c(1, 2, 3)))
})

test_that('squared log error is calculated correctly', {
    expect_equal(sle(c(0, 1, 3.4), c(1, 0, 3.4)), c(log(2), log(2), 0) ^ 2)
    expect_equal(sle(exp(2) - 1,exp(1) - 1), 1)
})

test_that('mean squared log error is calculated correctly', {
    expect_equal(msle(c(1, 2, exp(1) - 1),c(1, 2, exp(2)-1)), 1 / 3)
})

test_that('root mean squared log error is calculated correctly', {
    expect_equal(rmsle(c(exp(5) - 1), c(exp(1) - 1)), 4)
})

test_that('relative absolute error is calculated correctly', {
    expect_equal(rae(0:10, 30:40), 11)
    expect_equal(rae(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rae(1:4, c(1,2,3,5)), 0.25)
})

test_that('root relative squared error is calculated correctly', {
    expect_equal(rrse(0:10, 2:12), sqrt(0.4))
    expect_equal(rrse(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rrse(1:4, c(1,2,3,5)), sqrt(0.2))
})

test_that('relative squared error is calculated correctly', {
    expect_equal(rse(0:10, 2:12), 0.4)
    expect_equal(rse(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rse(1:4, c(1,2,3,5)), 0.2)
})


test_that('explained variation is calculated correctly', {
  expect_equal(explained_variation(0:10, 2:12), 0.6)
  expect_equal(explained_variation(seq(0,2,0.5), seq(0,2,0.5)), 1.0)
  expect_equal(explained_variation(1:4, c(1,2,3,5)), 0.8)
})
