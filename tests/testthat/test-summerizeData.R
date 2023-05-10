test_that("summarizeData works", {
  ages <- cleanImport(fileName = "ages.csv")

  expect_equal(summarizeData(ages, "age", "name"),
               c(summary(ages$age), prop.table(table(ages$name))))
})

test_that("summarizeData doesn't accept invalid column names", {
  ages <- cleanImport(fileName = "ages.csv")

  expect_error(summarizeData(ages, "age", "lastName"))
  expect_error(summarizeData(ages, "Title", "name"))
  expect_error(summarizeData(ages, "age", ""))
})

