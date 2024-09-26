test_that("prevalence works for valid inputs", {
  expect_equal(prevalence(5, 10), 0.5)
  expect_equal(prevalence(0, 10), 0)
  expect_equal(prevalence(10, 10), 1)
})

test_that("prevalence handles edge cases", {
  # Test when `total` is zero or negative (should trigger an error)
  expect_error(prevalence(5, 0), "Total must be greater than zero.")
  expect_error(prevalence(5, -5), "Total must be greater than zero.")

  # Test when `exposed` exceeds `total` (should trigger an error)
  expect_error(prevalence(11, 10), "Exposed count cannot be greater than the total population.")
})

test_that("prevalence handles non-numeric inputs", {
  # Non-numeric inputs should trigger an error
  expect_error(prevalence("five", 10), "is.numeric")
  expect_error(prevalence(5, "ten"), "is.numeric")
})
