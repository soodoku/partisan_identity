test_that("no free-text, timestamp or location columns ship in data/", {
  banned <- c("_oe", "startdt", "enddt", "state", "text", "comment", "email", "name")
  for (f in list.files(data_path(), pattern = "\\.csv$", full.names = TRUE)) {
    cols <- tolower(names(readr::read_csv(f, n_max = 0, show_col_types = FALSE)))
    expect_false(any(map_lgl(banned, \(b) any(grepl(b, cols, fixed = TRUE)))), label = basename(f))
  }
})

test_that("every shipped column is documented in the codebook", {
  codebook <- readLines(data_path("codebook.md"))
  documented <- unlist(regmatches(codebook, gregexpr("`[a-z0-9_]+`", codebook)))
  documented <- gsub("`", "", documented)
  for (f in c("tess_2018.csv", "benchmarks.csv")) {
    cols <- names(readr::read_csv(data_path(f), n_max = 0, show_col_types = FALSE))
    expect_setequal(setdiff(cols, documented), character(0))
  }
})
