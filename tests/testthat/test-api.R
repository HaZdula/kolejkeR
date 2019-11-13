context("Api functionality tests")

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
  test_that("get_available_offices works correctly", {
    result <- get_available_offices()
    expected <- office_ids[["office"]]
    expect_equal(result, expected)
  })
})


httptest::with_mock_api({
  test_that("get_raw_data returns data.frame", {
    sapply(names(testset), function(x){expect_equal(class(get_raw_data(x)),"data.frame")})
  })
})


httptest::with_mock_api({
  test_that("get_available_queues works correctly", {
    sapply(names(testset), function(x){expect_equal(class(get_available_queues(x)),"character")})
    sapply(names(testset), function(x){expect_gte(length(get_available_queues(x)),0)})
  })
})


httptest::with_mock_api({
  test_that("get_waiting_time works correctly", {
    sapply(names(testset), function(x){
      queue_name <- get_available_queues(x)[1]
      msg <- get_waiting_time(x, queue_name)
      expect_equal(class(msg),"character")
      msg_vec <- unlist(strsplit(msg, " "))
      waitng_time <- msg_vec[tail(grep("[0-9]", msg_vec),n=1)]
      expect_true(as.numeric(waitng_time) >= 0)
    })
  })
})


httptest::with_mock_api({
  test_that("get_open_counters works correctly", {
    sapply(names(testset), function(x){
      queue_name <- get_available_queues(x)[1]
      msg <- get_open_counters(x, queue_name)
      expect_equal(class(msg),"character")
      msg_vec <- unlist(strsplit(msg, " "))
      n_counters <- msg_vec[grep("[0-9]", msg_vec)[1]]
      expect_true(as.numeric(n_counters) >= 0)
    })
  })
})


# this may not work in opening hours
httptest::with_mock_api({
  test_that("get_current_ticket_number works correctly", {
    sapply(names(testset), function(x){
      queue_name <- get_available_queues(x)[1]
      msg <- get_current_ticket_number(x, queue_name)
      expect_equal(class(msg),"character")
      msg_vec <- unlist(strsplit(msg, " "))
      n_ticket <- msg_vec[tail(grep("[0-9]", msg_vec), n=1)]
      chars <- strsplit(n_ticket, "")[[1]]
      n_ticket2 <- do.call(paste0, as.list(chars[chars %in% as.character(0:9)]))
      expect_true(as.numeric(n_ticket2) >= 0 | length(as.numeric(n_ticket2)) == 0)
    })
  })
})


httptest::with_mock_api({
  test_that("get_number_of_people works correctly", {
    sapply(names(testset), function(x){
      queue_name <- get_available_queues(x)[1]
      msg <- get_number_of_people(x, queue_name)
      expect_equal(class(msg),"character")
      msg_vec <- unlist(strsplit(msg, " "))
      n_ticket <- msg_vec[grep("[0-9]", msg_vec)[1]]
      expect_true(as.numeric(n_ticket) >= 0)
    })
  })
})
