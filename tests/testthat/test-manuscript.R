tex <- readLines(project_file("ms", "main.tex"))
defined <- sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", readLines(table_path("macros.tex")))

test_that("every macro used in the manuscript is generated, and every generated macro is used", {
  body <- paste(tex[!grepl("^\\\\(usepackage|newcommand|input|documentclass)", tex)], collapse = "\n")
  used <- sub("^\\\\", "", unique(regmatches(body, gregexpr("\\\\[A-Za-z]+", body))[[1]]))
  # Generated macros are camelCase; LaTeX commands in the manuscript are lowercase.
  candidates <- setdiff(used[grepl("[a-z][A-Z]", used)], "TableStyle")
  expect_setequal(setdiff(candidates, defined), character(0))
  expect_setequal(setdiff(defined, used), character(0))
})

test_that("prose contains no hand-typed decimals other than significance thresholds", {
  body <- tex[!grepl("^\\\\", tex) & !grepl("Stars mark", tex)]
  decimals <- unlist(regmatches(body, gregexpr("[^\\\\{A-Za-z][0-9]+\\.[0-9]+", body)))
  expect_length(decimals, 0)
})
