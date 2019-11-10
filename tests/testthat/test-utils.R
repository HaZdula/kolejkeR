testset <- list(
  USC_Andersa = "5d2e698a-9c31-456b-8452-7ce33e7deb94",
  UD_Bialoleka = "95fee469-79db-4b4b-9ddc-91d49d1f0f51",
  UD_Bielany = "9c3d5770-57d8-4365-994c-69c5ac4186ee",
  UD_Wlochy = "05e32b8b-273b-4684-8dd0-8cd5c04dbb81",
  UD_Ursus = "06396204-e8c1-4139-80c0-a099ee3c448f",
  UD_Targowek = "9b8e3980-2d7d-47b7-8c44-4b5e0ca452fe",
  UD_Srodmiescie_1 = "78f6290a-0a19-482c-9641-8ac06f49c1c2",
  UD_Mokotow_2 = "bc83ab5a-0ccc-4e4a-b58d-b821e16df176",
  USC_Smyczkowa = "b03cf70a-cda7-4fc1-86d3-b9257e78033f"
)


httptest::with_mock_api({
  test_that("get_available_offices correctly", {
    result <- get_available_offices()
    expected <- office_ids$office
    
    expect_equal(result, expected)
  })
})

httptest::with_mock_api({
  test_that("get_data returns data.frame", {
    sapply(names(testset), function(x){expect_equal(class(get_data(x)),"data.frame")})
  })
})

httptest::with_mock_api({
  test_that("get_district_id works correctly", {
    sapply(names(testset), function(x){expect_true(get_district_id(x) == testset[x])})
  })
})

httptest::with_mock_api({
  test_that("get_district_id returns null if office name is unrecognized", {
    expect_null(get_district_id("No_such_office"))
    expect_null(get_district_id("adfg"))
  })
})

httptest::with_mock_api({
  test_that("get_get_request_url works correctly", {
    sapply(names(testset), function(x){expect_true(get_request_url(testset[x]) == paste0("https://api.um.warszawa.pl/api/action/wsstore_get", "/?id=", testset[x]))})
  })
})




#test_that("API call responds with no error", {
#  expect_error(GetQueue(), NA)
#})
#
#test_that("API call responds with no error", {
#  expect_error(GetQueue("sadasd"), "Unrecognized office acronym!")
#})
#
#test_that("Every acronym gives valid id", {
#  for(acronym in GetAvailableOfficeAcronyms()){
#    expect_error(GetQueue(acronym), NA)
#  }
#})   