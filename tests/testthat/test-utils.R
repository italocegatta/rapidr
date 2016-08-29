context("Utils")

test_that("doy works as expected",{
  expect_equal(doy(as.Date("2016-02-02")), 33)
})

test_that("declin works as expected",{
  expect_equal(declin(33), -0.298060828233258)
})

test_that("te works as expected",{
  expect_equal(te(33), -0.228953006453459)
})

test_that("to works as expected",{
  expect_equal(to(-45.414, -0.228953006453459), 12.25655301)
})

test_that("zh works as expected",{
  expect_equal(zh(-19.695, -0.298060828233258, 13, 12.25655301), 10.8985162226049)
})

test_that("sundist2 works as expected",{
  expect_equal(sundist2(33), 0,968055731)
})

