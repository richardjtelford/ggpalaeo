test_that("Simple cases work", {
  x <- "Navicula elkab"
  expect_equal("italic(Navicula)~italic(elkab)", species_to_italics(x, text = TRUE))
})

test_that("modifiers ignored", {
  x <- "Navicula elkab agg"
  expect_equal("italic(Navicula)~italic(elkab)~agg", species_to_italics(x, text = TRUE))
})
