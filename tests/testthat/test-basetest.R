test_that("name_file", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

test_that("load_file", {
  expect_error(is.data.frame(fars_read(2015)))
})
