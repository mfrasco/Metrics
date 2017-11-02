context('Information Retrieval')

test_that('f1 score is calculated correctly', {
    expect_equal(f1(c(3,4,5),c(3,4)), 0.8)
    expect_equal(f1(7,1), 0)
    expect_equal(f1(7,c(1,1)), 0)
})

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
