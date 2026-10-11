# Reading the survey and building the analysis variables. Codes are NORC's, listed in
# data/codebook.md; skips are already missing in data/tess_2018.csv.

read_data <- function(name) readr::read_csv(data_path(name), show_col_types = FALSE)

arms <- c("control", "ask", "tell")

# The three groups each party was asked about, in questionnaire order.
groups <- tribble(
  ~party, ~group, ~label,
  "dem", "atheist_agnostic", "Atheist or agnostic",
  "dem", "age_18_34", "Age 18-34",
  "dem", "nonwhite", "Nonwhite",
  "rep", "evangelical", "Evangelical",
  "rep", "age_65_plus", "Age 65+",
  "rep", "income_250k", "Earn $250,000+"
)

# Post-treatment seven-point party ID from the branched question: Q4 party, then strength
# (Q5_A, Q5_B) for partisans or lean (Q5_C) for everyone else. Respondents who skipped Q4
# were routed to the lean question, so they are placed by it too.
pid7_post <- function(party_post, strength_dem, strength_rep, lean) {
  independent <- party_post %in% c(3, 4) | is.na(party_post)
  case_when(
    party_post == 1 & strength_dem == 1 ~ 1,
    party_post == 1 & strength_dem == 2 ~ 2,
    independent & lean == 3 ~ 3,
    independent & lean == 2 ~ 4,
    independent & lean == 1 ~ 5,
    party_post == 2 & strength_rep == 2 ~ 6,
    party_post == 2 & strength_rep == 1 ~ 7
  )
}

# Expressive partisanship: each item scored 0 (weakest) to 1 (strongest), then averaged
# over the items answered. All four response scales run from strongest to weakest.
score_item <- function(x, n_options) (n_options - x) / (n_options - 1)
expressive_index <- function(criticize_upsets, meet_connected, we_not_they, polls_happy) {
  scores <- cbind(
    score_item(criticize_upsets, 4), score_item(meet_connected, 5),
    score_item(we_not_they, 5), score_item(polls_happy, 5)
  )
  if_else(rowSums(!is.na(scores)) > 0, rowMeans(scores, na.rm = TRUE), NA)
}

# Membership in the groups a respondent was asked about. The panel's top income bracket
# is $200,000 or more, the nearest available cut to the $250,000 respondents were told
# about. Only Christians were asked the born-again question; everyone else is not
# evangelical.
add_group_membership <- function(tess) {
  tess |>
    mutate(
      member_atheist_agnostic = as.integer(religion %in% c(9, 10)),
      member_age_18_34 = as.integer(age <= 34),
      member_nonwhite = as.integer(race_ethnicity != 1),
      member_evangelical = case_when(
        born_again == 1 ~ 1L,
        religion %in% c(1:4, 12, 14) & is.na(born_again) ~ NA,
        .default = 0L
      ),
      member_age_65_plus = as.integer(age >= 65),
      member_income_250k = as.integer(income == 18),
      stereotype_share = if_else(
        party == "dem",
        (member_atheist_agnostic + member_age_18_34 + member_nonwhite) / 3,
        (member_evangelical + member_age_65_plus + member_income_250k) / 3
      )
    )
}

# Guess error in percentage points against the share respondents were shown. Each
# respondent answered the three items for their own party; the mean over those is their
# overall misperception. Positive means the party looks more stereotypical than it is.
add_guess_error <- function(tess, benchmarks) {
  shown <- deframe(select(benchmarks, group, shown))
  tess |>
    mutate(
      error_atheist_agnostic = guess_atheist_agnostic - shown[["atheist_agnostic"]],
      error_age_18_34 = guess_age_18_34 - shown[["age_18_34"]],
      error_nonwhite = guess_nonwhite - shown[["nonwhite"]],
      error_evangelical = guess_evangelical - shown[["evangelical"]],
      error_age_65_plus = guess_age_65_plus - shown[["age_65_plus"]],
      error_income_250k = guess_income_250k - shown[["income_250k"]],
      mean_error = if_else(
        party == "dem",
        rowMeans(pick(error_atheist_agnostic, error_age_18_34, error_nonwhite)),
        rowMeans(pick(error_evangelical, error_age_65_plus, error_income_250k))
      )
    )
}

# Party is the panel party ID that set the randomization block and named the party in
# the outcome questions. Everyone sampled was a weak partisan (2, 6) or leaner (3, 5).
prepare_tess <- function(tess = read_data("tess_2018.csv"), benchmarks = read_data("benchmarks.csv")) {
  tess |>
    mutate(
      party = if_else(pid7_pre %in% 2:3, "dem", "rep"),
      arm = factor(arm, arms),
      leaner = as.integer(pid7_pre %in% c(3, 5)),
      pid7_post = pid7_post(party_post, strength_dem, strength_rep, lean),
      # Higher means closer to the pre-survey party: 7 is a strong identifier, 1 a strong
      # identifier with the other party. Before the survey everyone was at 5 or 6.
      in_party_id = if_else(party == "dem", 8 - pid7_post, pid7_post),
      in_party_id_pre = if_else(party == "dem", 8 - pid7_pre, pid7_pre),
      expressive = expressive_index(criticize_upsets, meet_connected, we_not_they, polls_happy),
      # Claims no party on the first question; defined for everyone who answered it.
      claims_independent = if_else(is.na(party_post), NA, as.integer(party_post %in% c(3, 4))),
      college = as.integer(education >= 12),
      ideology7 = na_if(ideology, 8),
      white = as.integer(race_ethnicity == 1)
    ) |>
    add_group_membership() |>
    add_guess_error(benchmarks)
}
