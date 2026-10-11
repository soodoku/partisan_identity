effects <- read_result("effects")
party_differences <- read_result("party_differences")
memo_contrast <- read_result("memo_contrast")
macros <- readLines(table_path("macros.tex"))
macro <- function(name) sub(".*\\}\\{(.*)\\}$", "\\1", grep(paste0("\\{\\\\", name, "\\}"), macros, value = TRUE))

test_that("the recoded data reproduce the 2019 memo's contrast", {
  # The memo reported b = .03, p < .01 for Republicans and b = .01, p = .64 for Democrats
  # from OLS of the expressive index on tell vs. the other two arms pooled.
  rep <- filter(memo_contrast, party == "rep")
  dem <- filter(memo_contrast, party == "dem")
  expect_equal(round(rep$estimate, 2), 0.03)
  expect_lt(rep$p_value, 0.01)
  expect_equal(round(dem$p_value, 2), 0.64)
})

test_that("the party-difference model reproduces the by-party estimates", {
  by_party <- effects |>
    filter(sample != "pooled") |>
    select(outcome, contrast, sample, estimate) |>
    pivot_wider(names_from = sample, values_from = estimate) |>
    inner_join(party_differences, by = c("outcome", "contrast"))
  expect_equal(by_party$estimate, by_party$rep - by_party$dem, tolerance = 1e-10)
})

test_that("tell vs. ask is the sum of the other two contrasts' difference", {
  # Same blocks and arms, so the three contrasts are linearly related.
  wide <- effects |>
    select(outcome, sample, contrast, estimate) |>
    pivot_wider(names_from = contrast, values_from = estimate)
  expect_equal(wide$tell_ask, wide$tell_control - wide$ask_control, tolerance = 1e-10)
})

test_that("Holm adjustment covers all 18 main-effect tests", {
  expect_equal(nrow(effects), 18)
  expect_equal(effects$p_holm, p.adjust(effects$p_value, "holm"))
})

test_that("manuscript macros come from the results tables", {
  rep_tell_ask <- filter(effects, outcome == "expressive", sample == "rep", contrast == "tell_ask")
  expect_equal(macro("repTellAsk"), num(rep_tell_ask$estimate, 3))
  expect_equal(macro("nTotal"), "2,222")
})
