context('Time series metrics.')

test_that('mean absolute scaled error is computed correctly', {
    actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
    predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
    step_size <- 1
    expect_equal(mase(actual, predicted, step_size), 1.5 / 5.4)
    
    step_size <- 2
    expect_equal(mase(actual, predicted, step_size), 1.5 / 11.4)
})