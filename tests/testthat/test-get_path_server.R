test_that("get_path_server works", {
	expect_identical(get_path_server("europe"), "https://europe.api.riotgames.com")
})
