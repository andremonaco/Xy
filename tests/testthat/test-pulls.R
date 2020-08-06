context("Test Pulls")

test_that("pull_x", {

  # simulation
  x <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100) %>%
    pull_x()

  # class
  expect_s3_class(x, "tbl_df")

  # dim
  expect_equal(nrow(x), 100)
  expect_equal(ncol(x), 5)
})

test_that("pull_y", {

  # simulation
  y <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100) %>%
    pull_y()

  # class
  expect_s3_class(y, "tbl_df")

  # dim
  expect_equal(nrow(y), 100)
  expect_equal(ncol(y), 1)
})

test_that("pull_e", {

  # simulation
  e <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100) %>%
    pull_e()

  # class
  expect_s3_class(e, "tbl_df")

  # dim
  expect_equal(nrow(e), 100)
  expect_equal(ncol(e), 1)
})

test_that("pull_xy", {

  # simulation
  xy <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100) %>%
    pull_xy()

  # class
  expect_s3_class(xy, "tbl_df")

  # dim
  expect_equal(nrow(xy), 100)
  expect_equal(ncol(xy), 6)
})

test_that("pull_xye", {

  # simulation
  xye <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100) %>%
    pull_xye()

  # class
  expect_s3_class(xye, "tbl_df")

  # dim
  expect_equal(nrow(xye), 100)
  expect_equal(ncol(xye), 7)
})
