# Shared style and writers for figures, tables and manuscript macros.

colours <- c(dem = "#2a78d6", rep = "#d63c2a", pooled = "grey30")
party_names <- c(dem = "Democrats", rep = "Republicans", pooled = "Pooled")
outcome_names <- c(
  expressive = "Expressive partisanship (0-1)",
  in_party_id = "In-party identification (1-7)",
  claims_independent = "Calls self independent"
)
figure_dpi <- 200

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

save_figure <- function(plot, name, width, height) {
  ggplot2::ggsave(figure_path(paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggplot2::ggsave(
    figure_path(paste0(name, ".png")), plot,
    width = width, height = height, dpi = figure_dpi, bg = "white"
  )
}

pct <- function(x, digits = 0) formatC(100 * x, format = "f", digits = digits)
num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
interval <- function(lo, hi, digits = 2) paste0("[", num(lo, digits), ", ", num(hi, digits), "]")
p_value <- function(p) if_else(p < 0.001, "<0.001", num(p, 3))
p_stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", .default = "")
estimate_cell <- function(estimate, std_error, p, digits = 3) {
  paste0(num(estimate, digits), p_stars(p), " (", num(std_error, digits), ")")
}

# A booktabs tabular for \input{} inside a table float. `groups` names consecutive
# blocks of rows by their sizes, as in kableExtra::pack_rows(index = ).
write_table <- function(df, path, align, groups = NULL) {
  tab <- knitr::kable(df, format = "latex", booktabs = TRUE, align = align, linesep = "", escape = TRUE)
  if (!is.null(groups)) {
    tab <- kableExtra::pack_rows(tab, index = groups, bold = FALSE, italic = TRUE)
  }
  writeLines(tab, path)
}

write_macros <- function(values, path) {
  stopifnot(!anyDuplicated(names(values)), all(grepl("^[A-Za-z]+$", names(values))))
  writeLines(sprintf("\\newcommand{\\%s}{%s}", names(values), unlist(values)), path)
}

# One cell of a results table; fails loudly unless exactly one row matches.
the <- function(df, col, ...) {
  value <- df |>
    filter(...) |>
    pull({{ col }})
  stopifnot(length(value) == 1)
  value
}
