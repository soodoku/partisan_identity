tess <- prepare_tess()

test_that("the branched party question maps onto the seven-point scale", {
  # Democrat strong/weak, Republican strong/weak, then independents by lean.
  expect_equal(pid7_post(c(1, 1), c(1, 2), NA, NA), c(1, 2))
  expect_equal(pid7_post(c(2, 2), NA, c(1, 2), NA), c(7, 6))
  expect_equal(pid7_post(c(3, 3, 3), NA, NA, c(3, 2, 1)), c(3, 4, 5))
  # "Other" and skipped first questions were routed to the lean question.
  expect_equal(pid7_post(c(4, NA), NA, NA, c(1, 3)), c(5, 3))
  expect_true(is.na(pid7_post(1, NA, NA, NA)))
})

test_that("each expressive item runs from 0 (weakest) to 1 (strongest)", {
  expect_equal(expressive_index(1, 1, 1, 1), 1)
  expect_equal(expressive_index(4, 5, 5, 5), 0)
  expect_equal(expressive_index(2, 3, NA, NA), mean(c(2 / 3, 1 / 2)))
  expect_true(is.na(expressive_index(NA, NA, NA, NA)))
})

test_that("in-party identification points toward the pre-survey party", {
  # Weak partisans and leaners only, so everyone starts at 5 or 6.
  expect_setequal(unique(tess$in_party_id_pre), c(5, 6))
  dem_strong <- filter(tess, party == "dem", party_post == 1, strength_dem == 1)
  rep_strong <- filter(tess, party == "rep", party_post == 2, strength_rep == 1)
  expect_true(all(dem_strong$in_party_id == 7))
  expect_true(all(rep_strong$in_party_id == 7))
  dem_to_rep <- filter(tess, party == "dem", party_post == 2, strength_rep == 1)
  expect_true(all(dem_to_rep$in_party_id == 1))
})

test_that("group flags follow the definitions respondents were told about", {
  flags <- tibble(
    party = "dem", religion = c(9, 10, 11), age = c(34, 35, 65), race_ethnicity = c(1, 4, 1),
    born_again = NA, income = 18
  ) |>
    add_group_membership()
  expect_equal(flags$member_atheist_agnostic, c(1, 1, 0))
  expect_equal(flags$member_age_18_34, c(1, 0, 0))
  expect_equal(flags$member_age_65_plus, c(0, 0, 1))
  expect_equal(flags$member_nonwhite, c(0, 1, 0))
})

test_that("guess error is the guess minus the share shown, for the respondent's own party", {
  benchmarks <- read_data("benchmarks.csv")
  shown_rich <- benchmarks$shown[benchmarks$group == "income_250k"]
  rep_row <- filter(tess, party == "rep", !is.na(guess_income_250k)) |> slice(1)
  expect_equal(rep_row$error_income_250k, rep_row$guess_income_250k - shown_rich)
  expect_true(all(is.na(tess$guess_income_250k[tess$party == "dem"])))
  expect_true(all(is.na(tess$guess_nonwhite[tess$party == "rep"])))
})

test_that("arms and blocks match the field report", {
  expect_equal(nrow(tess), 2222)
  expect_equal(as.vector(table(tess$arm)), c(754, 717, 751))
  expect_setequal(unique(tess$pid7_pre), c(2, 3, 5, 6))
})
