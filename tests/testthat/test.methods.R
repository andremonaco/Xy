context("Method Testing")


test_that("Print", {
  expect_output(print(Xy()))
})


test_that("Plot", {
  p <- plot(Xy(n = 100))
  expect_true(is.ggplot(p))
})

test_that("Coef", {
  expect_that(coef(Xy()), is_a("numeric"))
})

test_that("Transform", {
  sim <- Xy()
  trans <- transform(sim)
  expect_that(trans, is_a("data.table"))
})