httptest::with_mock_api({
  test_that("get_available_offices correctly", {
    result <- get_available_offices()
    expected <- office_ids$office
    
    expect_equal(result, expected)
  })
})

httptest::with_mock_api({
  test_that("get_data", {
    sapply(office_ids$office, function(x){expect_equal(class(get_data(x)),"data.frame")})
  })
})



