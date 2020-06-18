context("triangulation")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, filter = "-keep_class 2 -keep_every_nth 10")

test_that("Delaunay produces the correct output", {

  ps <- data.frame(
    X = c(0,  1, -1, 1, -1),
    Y = c(0, -1, 1, 1, -1))

  expected = structure(c(3L, 1L, 2L, 1L, 5L, 2L, 1L, 4L, 1L, 4L, 5L, 3L), .Dim = 4:3)

  ts <- lidR:::tDelaunay(ps, scales = c(1,1), offsets = c(0,0))

  expect_is(ts, "matrix")
  expect_equal(ts, expected)
})

test_that("Delaunay works with a LAS", {

  ts <- lidR:::tDelaunay(las)

  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(1608,3))
})


test_that("Delaunay trimming option works", {

  set.seed(42)
  X <- round(runif(50,0,10),2)
  set.seed(123)
  Y <- round(runif(50,0,10),2)
  ps <- as.matrix(data.frame(X, Y, 0))

  ts <- lidR:::tDelaunay(ps, trim = 0)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(89,3))

  ts <- lidR:::tDelaunay(ps, trim = 2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(41,3))

  ts <- lidR:::tDelaunay(ps, trim = -2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(48,3))
})

test_that("Delaunay fails when necessary", {

  ps <- data.frame(X = 1, Y = 2)
  expect_error(lidR:::tDelaunay(ps), "cannot triangulate less than 3 points")

  #ps <- data.frame( A = c(0, 1, 1), B = c(0, 0, 1))
  #expect_error(lidR:::tDelaunay(ps), "Columns are not named XY")

  ps <- 2
  expect_error(lidR:::tDelaunay(ps), "No method to triangulate this input")
})
