context("Test Xy Recipe")

test_that("class", {
  recipe <- Xy()

  expect_equal(nrow(recipe$book), 0)
  expect_s3_class(recipe, "xy_recipe")
  expect_s3_class(recipe$book, "tbl_df")
  expect_false(recipe$interactions)
  expect_false(recipe$intercept)
  expect_that(recipe$task, is_a("list"))
  expect_length(recipe$task, 3)
})

test_that("add_effects", {

  # noise
  recipe <- Xy() %>%
    add_noise()

  expect_equal(nrow(recipe$book), 1)
  expect_false(recipe$interactions)
  expect_false(recipe$intercept)

  # linear
  recipe <- recipe %>%
    add_linear(p = 5, family = xy_beta())

  expect_equal(nrow(recipe$book), 6)

  # nonlinear
  recipe <- recipe %>%
    add_nonlinear(p = 3, nlfun = function(x) x^2, family = xy_normal())

  expect_equal(nrow(recipe$book), 9)

  # discrete
  recipe <- recipe %>%
    add_discrete(p = 1, levels = 3)

  expect_equal(nrow(recipe$book), 10)

  # uninformative
  recipe <- recipe %>%
    add_discrete(p = 10, levels = 5)

  expect_equal(nrow(recipe$book), 20)

  # interactions
  recipe <- recipe %>%
    add_interactions()

  expect_true(recipe$interactions)

  # interactions
  recipe <- recipe %>%
    add_intercept()

  expect_true(recipe$interactions)

  # output object
  expect_s3_class(recipe, "xy_recipe")
  expect_s3_class(recipe$book, "tbl_df")
  expect_that(recipe$task, is_a("list"))
  expect_length(recipe$task, 3)
})

test_that("methods", {
  recipe <- Xy() %>%
    add_noise()
  expect_output(print(recipe))
})
