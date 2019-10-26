test_that("API call responds with no error", {
  expect_error(GetQueue(), NA)
})

test_that("API call responds with no error", {
  expect_error(GetQueue("sadasd"), "Unrecognized office acronym!")
})

test_that("Every acronym gives valid id", {
  for(acronym in GetAvailableOfficeAcronyms()){
    expect_error(GetQueue(acronym), NA)
  }
}) 