test_that("get_path_server works", {
	expect_identical(lorR:::get_path_server("europe"), "https://europe.api.riotgames.com")
})
