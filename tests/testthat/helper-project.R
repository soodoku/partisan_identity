source(file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), "scripts", "00_config.R"))

read_result <- function(name) readr::read_csv(table_path(paste0(name, ".csv")), show_col_types = FALSE)
