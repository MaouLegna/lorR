test_that("Negate %in% works", {
	expect_identical(all(`%!in%`(c("a","b"),"a")),FALSE)
	expect_identical(all(c("a","b") %!in% "a"),FALSE)
})
