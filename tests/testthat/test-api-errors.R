context("Api error tests")

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
  test_that("get_raw_data throws error if incorrect argument is given", {
    expect_error(get_raw_data("wRonGQnamE"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_available_queues throws error if incorrect argument is given", {
    expect_error(get_available_queues("wRonGQnamE"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_waiting_time throws error if incorrect office is given", {
    expect_error(get_waiting_time("wRonGQnamE"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_waiting_time throws error if incorrect queue is given", {
    sapply(names(testset), function(x){
      expect_error(get_waiting_time(x, "wRonGQnamE"), "Unrecognized queue name!")
    })
  })
})

httptest::with_mock_api({
  test_that("get_open_counters throws error if incorrect office is given", {
    expect_error(get_open_counters("wRonGQnamE"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_open_counters throws error if incorrect queue is given", {
    sapply(names(testset), function(x){
      expect_error(get_open_counters(x, "wRonGQnamE"), "Unrecognized queue name!")
    })
  })
})

httptest::with_mock_api({
  test_that("get_current_ticket_number throws error if incorrect office is given", {
    expect_error(get_current_ticket_number("wRonGQnamE", "Kasa"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_current_ticket_number throws error if incorrect queue is given", {
    sapply(names(testset), function(x){
      expect_error(get_current_ticket_number(x, "wRonGQnamE"), "Unrecognized queue name!")
    })
  })
})

httptest::with_mock_api({
  test_that("get_number_of_people throws error if incorrect queue is given", {
    sapply(names(testset), function(x){
      expect_error(get_number_of_people(x, "wRonGQnamE"), "Unrecognized queue name!")
    })
  })
})
