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
    expect_error(get_number_of_people("wRonGQnamE", "Kasa"), "Unrecognized office acronym!")
  })
})

httptest::with_mock_api({
  test_that("get_number_of_people throws error if incorrect queue is given", {
    sapply(names(testset), function(x){
      expect_error(get_number_of_people(x, "wRonGQnamE"), "Unrecognized queue name!")
    })
  })
})
