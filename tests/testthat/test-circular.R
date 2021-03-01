
test_that("hdg_circular() works", {
  skip_if_not_installed("circular")
  expect_true(circular::is.circular(hdg_circular(1:10)))
})
