context("Xy Testing")

test_that("Standard Case", {
  
  unit <- Xy()
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$dgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$family, is_a("list"))
  expect_that(length(unit), equals(5))
  
})

test_that("Collinearity Case", {
  
  unit <- Xy(noise.coll = TRUE)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$dgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$family, is_a("list"))
  expect_that(length(unit), equals(5))
  
})

test_that("Custom Family", {
  
  unit <- Xy(family = Xy_family('test learning task', 
                                link = function(x) {x},
                                cutoff = function(x) {x}))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$dgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$family, is_a("list"))
  expect_that(length(unit), equals(5))
  
})

test_that("No Intercept", {
  
  unit <- Xy(intercept = FALSE)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$dgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$family, is_a("list"))
  expect_that(length(unit), equals(5))
  
})

test_that("Ill-defined Categoricals", {
  
  expect_that(Xy(catvars = 1), throws_error())
  
})