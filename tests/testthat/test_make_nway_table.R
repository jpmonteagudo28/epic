test_that("make_nway_table with data.frame works", {
  df <- data.frame(
    Gender = c("Female", "Male", "Female", "Male"),
    Smoker = c("Yes", "No", "No", "Yes")
  )

  result <- make_nway_table(df, "Gender", "Smoker")
  expected <- table(df[, c("Gender", "Smoker")])

  expect_equal(result, expected)
})

test_that("make_nway_table with matrix works", {
 data <- matrix(c("Female", "Male", "Female", "Male", "Yes", "No", "No", "Yes"),
                ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("Gender", "Smoker")))

  result <- make_nway_table(mat, 1, 2, as_data_frame = TRUE)
  expected <- table(as.data.frame(mat))

  expect_equal(result, expected)
})

test_that("make_nway_table with matrix works", {
  data <- matrix(c("Female", "Male", "Female", "Male", "Yes", "No", "No", "Yes"),
                ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("Gender", "Smoker")))

  result <- make_nway_table(data)
  expected <- table(data)

  expect_equal(result, expected)
})

test_that("make_nway_table with list works", {
  data <- list(
    Gender = c("Female", "Male", "Female", "Male"),
    Smoker = c("Yes", "No", "No", "Yes")
  )

  result <- make_nway_table(data, "Gender", "Smoker")
  expected <- table(data.frame(data[1:2]))

  expect_equal(result, expected)
})

test_that("make_nway_table handles errors", {
  data <- data.frame(
    Gender = c("Female", "Male", "Female", "Male"),
    Smoker = c("Yes", "No", "No", "Yes")
  )

  expect_error(make_nway_table(data, "NonexistentVar"), "Some variables are not present in the data frame.")
  expect_error(make_nway_table(matrix(1:4, ncol = 2), 3), "Some column indices are out of bounds.")
  expect_error(make_nway_table(list(), "NonexistentVar"), "Some variables are not present in the list.")
  expect_error(make_nway_table(1:10), "Unsupported data type. Please provide a data frame, matrix, or list.")
})
