context("Test Xy Families")

test_that("xy_normal", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_binom", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_binom()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_cauchy", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_cauchy()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_poisson", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_poisson()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_chisq", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_chisq()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_exp", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_exp()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_f", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_f()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_gamma", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_gamma()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_geometric", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_geometric()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_lognormal", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_lognormal()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_hypergeometric", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_hypergeometric()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_logistic", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_logistic()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_t", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_t()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_signrank", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_signrank()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_uniform", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_uniform()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_weibull", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_weibull()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})

test_that("xy_wilcox", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_wilcox()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)
})
