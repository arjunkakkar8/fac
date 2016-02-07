context("Reading Ages from Text File")

test_that("readBAyear picks up years in a reasonable range",{
  expect_that(max(readBAyear("list_2014.txt", 2014, 2011, 1958)$Age), is_less_than(85))
  expect_that(min(readBAyear("list_2014.txt", 2014, 2011, 1958)$Age), is_more_than(20))
})

test_that("readBAyear outputs a dataframe with numeric columns", {
  expect_that(readBAyear("list_2014.txt", 2014, 2011, 1958), is_a("data.frame"))
  expect_that(readBAyear("list_2014.txt", 2014, 2011, 1958)[,1], is_a("numeric"))
  expect_that(readBAyear("list_2014.txt", 2014, 2011, 1958)[,2], is_a("numeric"))
})
