context("Xy Testing")


# REGRESSION / CLASSIFICATION ---------------------------------------------

test_that("Standard Case", {
  require(data.table)
  unit <- Xy()
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("Classification", {
  
  unit <- Xy(task = Xy_task(name = 'classification'))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_true(all(unit$data$y == 1 | unit$data$y == 0))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("Collinearity Case", {
  
  unit <- Xy(noise_coll = TRUE)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("Custom task", {
  
  unit <- Xy(task = Xy_task('test learning task', 
                                link = function(x) {x},
                                cutoff = function(x) {x}))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("Ill-defined Categoricals", {
  
  expect_that(Xy(catvars = 1), throws_error())
  
})


# EXCLUDING VARIABLES -----------------------------------------------------

test_that("No Intercept", {
  
  unit <- Xy(intercept = FALSE)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})


test_that("No linear variable", {
  
  unit <- Xy(numvars = c(2,0))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("No nonlinear variable", {
  
  unit <- Xy(numvars = c(0,2))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("No categorical variable", {
  
  unit <- Xy(catvars = c(0,2))
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

test_that("No noise variable", {
  
  unit <- Xy(noisevars = 0)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})

# INTERACTIONS -----------------------------------------------------

test_that("Up to 6 interactions", {
  
  unit <- Xy(interactions = 6)
  
  expect_that(unit, is_a("Xy_sim"))
  expect_that(unit$data, is_a("data.table"))
  expect_that(unit$tgp, is_a("character"))
  expect_that(unit$control, is_a("list"))
  expect_that(unit$psi, is_a("Matrix"))
  expect_that(unit$task, is_a("list"))
  expect_that(length(unit), equals(6))
  
})
