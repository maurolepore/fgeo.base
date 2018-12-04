context("test-guess_plotdim.R")

test_that("outputs expected plotdim and message", {
  skip_if_not_installed("fgeo.data")

  x <- data.frame(gx = c(0, 300, 981), gy = c(0, 300, 499))
  expect_equal(guess_plotdim(x), c(1000, 500))

  x <- data.frame(gx = c(0, 981), gy = c(0, 479))
  expect_equal(
    expect_message(guess_plotdim(x), "Guessing"),
    c(1000, 480)
  )

  expect_equal(guess_plotdim(fgeo.data::luquillo_stem6_random), c(320, 500))
})
