test_that("cleanImport works with csv", {

  ages_csv <- cleanImport(fileName = "ages.csv")

  expect_equal(class(ages_csv), "data.frame")
  expect_equal(nrow(ages_csv), 6)
  expect_equal(ncol(ages_csv), 2)

})

test_that("cleanImport works with txt", {

  ages_txt <- cleanImport(fileName = "ages.txt")

  expect_equal(class(ages_txt), "data.frame")
  expect_equal(nrow(ages_txt), 6)
  expect_equal(ncol(ages_txt), 2)

})

test_that("cleanImport works with xls", {

  ages_xls <- cleanImport(fileName = "ages.xls", sheet = "ages")

  expect_equal(class(ages_xls), "data.frame")
  expect_equal(nrow(ages_xls), 6)
  expect_equal(ncol(ages_xls), 2)

})

test_that("cleanImport works with xlsx", {

  ages_xlsx <- cleanImport(fileName = "ages.xlsx", sheet = "ages")

  expect_equal(class(ages_xlsx), "data.frame")
  expect_equal(nrow(ages_xlsx), 6)
  expect_equal(ncol(ages_xlsx), 2)

})

test_that("cleanImport succesfully completes data cleaning tasks", {

  ages_altered_xlsx <- cleanImport(fileName = "ages_altered.xlsx", sheet = "ages")

  expect_equal(class(ages_altered_xlsx), "data.frame")
  expect_equal(nrow(ages_altered_xlsx), 6)
  expect_equal(ncol(ages_altered_xlsx), 2)
  expect_equal(names(ages_altered_xlsx)[1], "x1name1")

})
