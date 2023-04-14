test_that("compare_results(category = NULL)", {
  temp_path <- tempdir()
  test_file <-
    file.path(temp_path, "Test_ResultsTable_cleaned_Top30.png")
  announce_snapshot_file(test_file)
  compare_results(input_path = test_path("testdata"),
                  output_path = temp_path)
  expect_snapshot_file(test_file)
})



test_that("compare_results(top = 50)", {
  temp_path <- tempdir()
  test_file <-
    file.path(temp_path, "Test_ResultsTable_cleaned_Top50.png")
  announce_snapshot_file(test_file)
  compare_results(
    input_path = test_path("testdata"),
    output_path = temp_path,
    top = 50
  )
  expect_snapshot_file(test_file)
})



test_that("compare_results(category = 'Tissue')", {
  temp_path <- tempdir()
  test_file <-
    file.path(temp_path, "Tissue_Test_ResultsTable_cleaned_Top30.png")
  compare_results(
    input_path = test_path("testdata"),
    output_path = temp_path,
    category = "Tissue"
  )
  expect_snapshot_file(test_file)
})



test_that("compare_results(top = 50, category = 'Tissue')", {
  temp_path <- tempdir()
  test_file <-
    file.path(temp_path, "Tissue_Test_ResultsTable_cleaned_Top50.png")
  compare_results(
    input_path = test_path("testdata"),
    output_path = temp_path,
    top = 50,
    category = "Tissue"
  )
  expect_snapshot_file(test_file)
})
