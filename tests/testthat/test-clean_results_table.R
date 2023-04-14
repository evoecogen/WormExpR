test_that("clean_results_table()", {
  temp_path <- tempdir()
  test_file <-
    file.path(temp_path, "Test_ResultsTable_cleaned.txt")
  announce_snapshot_file(test_file)
  clean_results_table(
    input_file = test_path("testdata", "Test_ResultsTable.txt"),
    output_path = temp_path
  )
  expect_snapshot_file(test_file)
})
