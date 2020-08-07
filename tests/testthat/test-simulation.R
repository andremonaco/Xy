context("Test Xy Simulation")

# setting seed
set.seed(1234)

test_that("regression", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)

  # psi
  psi <- sim$psi
  expect_equal(nrow(psi), 7)
  expect_equal(nrow(psi), ncol(psi))
  expect_that(psi, is_a("matrix"))
  expect_equal(sum(psi[lower.tri(psi, diag = FALSE)]), 0)
})

test_that("classification", {

  # simulation
  sim <- Xy(task = "classification") %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)

  # psi
  psi <- sim$psi
  expect_equal(nrow(psi), 7)
  expect_equal(nrow(psi), ncol(psi))
  expect_that(psi, is_a("matrix"))
  expect_equal(sum(psi[lower.tri(psi, diag = FALSE)]), 0)
})

test_that("user specified correlation matrix", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(
      n = 100,
      cor_matrix = copula::p2P(stats::runif(
        6 * (6 - 1) / 2,
        min(-0.5), max(0.5)
      ))
    )

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)

  # psi
  psi <- sim$psi
  expect_equal(nrow(psi), 7)
  expect_equal(nrow(psi), ncol(psi))
  expect_that(psi, is_a("matrix"))
  expect_equal(sum(psi[lower.tri(psi, diag = FALSE)]), 0)
})

test_that("full_blown", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_nonlinear(p = 5, family = xy_normal()) %>%
    add_discrete(p = 2, level = 2) %>%
    add_uninformative(p = 5, family = xy_uniform()) %>%
    add_interactions() %>%
    add_intercept() %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 20)

  # psi
  psi <- sim$psi
  expect_equal(nrow(psi), 20)
  expect_equal(nrow(psi), ncol(psi))
  expect_that(psi, is_a("matrix"))
  expect_true(any(psi[lower.tri(psi, diag = FALSE)] != 0))
})

test_that("interactions", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100)

  # class
  expect_s3_class(sim, "xy_sim")

  # data
  expect_equal(nrow(sim$data), 100)
  expect_equal(ncol(sim$data), 7)

  # psi
  psi <- sim$psi
  expect_equal(nrow(psi), 7)
  expect_equal(nrow(psi), ncol(psi))
  expect_that(psi, is_a("matrix"))
  expect_true(any(psi[lower.tri(psi, diag = FALSE)] != 0))
})

test_that("print", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    simulate(n = 100)

  expect_output(print(sim))
})

test_that("plot", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100) %>%
    plot()

  # data
  expect_s3_class(sim, "gg")
})

test_that("importance", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100) %>%
    importance()

  # class
  expect_s3_class(sim, "tbl_df")
})

test_that("coefficients", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100)
  coefs <- sim %>% coef()

  # class
  expect_that(coefs, is_a("numeric"))
  expect_length(coefs, 5)
  expect_equal(names(coefs), sim %>% pull_x() %>% names())
})

test_that("transform", {

  # simulation
  sim <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100)
  trans <- sim %>% transform()

  # class
  expect_that(trans, is_a("matrix"))
  expect_equal(nrow(trans), 100)
  expect_equal(ncol(trans), 7)
  expect_equal(colnames(trans), colnames(sim$data))
})

test_that("formula", {

  # simulation
  eq <- Xy() %>%
    add_linear(p = 5, family = xy_normal()) %>%
    add_interactions() %>%
    simulate(n = 100) %>%
    formula.xy_sim()

  # class
  expect_is(eq, "formula")
})
