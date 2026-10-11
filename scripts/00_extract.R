# Writes data/tess_2018.csv and data/benchmarks.csv. Not part of the build: the sources
# live outside this repository.
#
#   Rscript scripts/00_extract.R <tess_sav> <anes_2016_dta>
#
# <tess_sav> is TESS13Ahler_4May2018.sav, the respondent file NORC delivered for TESS 013
# (fielded April 10-29, 2018). The SPSS file is the source because it keeps NORC's value
# labels; the Stata export of the same data renumbers codes (PARTYID7 is shifted by one,
# web skips become 6 or 7). <anes_2016_dta> is anes_timeseries_2016.dta from
# electionstudies.org, used to recompute the shares shown in the tell condition.
#
# Free-text fields (Q4_OE, RELIG_OE, Q15A_OE), interview timestamps, state and household
# roster counts are dropped. Column names follow data/codebook.md.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(haven)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
write_data <- function(df, name) readr::write_csv(df, file.path("data", name), na = "")

# NORC codes web skips as 98 and unanswered numeric boxes as 998; neither is an answer.
answered <- function(x) if_else(x %in% c(77, 98, 99, 998), NA, x)

tess <- read_sav(args[1]) |> zap_labels()
stopifnot(nrow(tess) == 2222, !anyDuplicated(tess$CaseId))

# The correction screens were assigned by comparing each guess with the true share, so
# they carry no information beyond the guess. Check that before dropping them: every
# respondent shown "you overestimated" had guessed above the truth, and so on.
truth_shown <- c(A = 14, B = 30, C = 43)
truth_shown_rep <- c(A = 37, B = 21, C = 4)
correction_matches_guess <- function(guess, under, over, exact, truth) {
  guess <- answered(guess)
  shown <- !is.na(guess)
  all(xor(!is.na(under), guess >= truth)[shown]) &&
    all(xor(!is.na(over), guess <= truth)[shown]) &&
    all(xor(!is.na(exact), guess != truth)[shown])
}
tell <- filter(tess, P_TESS13 == 2)
for (item in names(truth_shown)) {
  dem <- \(v) tell[[sprintf(v, item)]]
  stopifnot(
    correction_matches_guess(
      dem("Q1_D_%s1"), dem("Q2_D_%s1"), dem("Q2_D_%s2"), dem("Q2_D_%s3"), truth_shown[[item]]
    ),
    correction_matches_guess(
      dem("Q1_R_%s1"), dem("Q2_R_%s1"), dem("Q2_R_%s2"), dem("Q2_R_%s3"), truth_shown_rep[[item]]
    )
  )
}
# The expressive battery named the party from the panel party ID, not from answers here.
stopifnot(all((tess$Q6A_INS == 1) == (tess$PARTYID7 %in% 2:3)))

tess |>
  transmute(
    case_id = CaseId,
    weight = WEIGHT1,
    weight_within_party = WEIGHT2,
    arm = c("ask", "tell", "control")[P_TESS13],
    pid7_pre = PARTYID7,
    # Ask and tell respondents gave guesses before the outcomes (Q1_*1); control
    # respondents gave them at the end (Q1_*2). Each respondent has one set.
    guess_atheist_agnostic = answered(coalesce(Q1_D_A1, Q1_D_A2)),
    guess_age_18_34 = answered(coalesce(Q1_D_B1, Q1_D_B2)),
    guess_nonwhite = answered(coalesce(Q1_D_C1, Q1_D_C2)),
    guess_evangelical = answered(coalesce(Q1_R_A1, Q1_R_A2)),
    guess_age_65_plus = answered(coalesce(Q1_R_B1, Q1_R_B2)),
    guess_income_250k = answered(coalesce(Q1_R_C1, Q1_R_C2)),
    reaction = answered(coalesce(Q3_D, Q3_R)),
    reaction_debrief = answered(coalesce(Q3_D_2, Q3_R_2)),
    party_post = answered(Q4),
    strength_dem = answered(Q5_A),
    strength_rep = answered(Q5_B),
    lean = answered(Q5_C),
    criticize_upsets = answered(Q6_A),
    meet_connected = answered(Q6_B),
    we_not_they = answered(Q6_C),
    polls_happy = answered(Q6_D),
    ideology = answered(IDEO),
    religion = answered(RELIG),
    born_again = answered(BORN),
    attendance = answered(ATTEND),
    political_interest = answered(Q6PA),
    turnout_2016 = answered(Q14A),
    vote_2016 = answered(Q15A),
    duration_minutes = duration,
    device = Device,
    female = as.integer(GENDER == 2),
    age = AGE,
    race_ethnicity = RACETHNICITY,
    education = EDUC,
    education4 = EDUC4,
    marital = MARITAL,
    employment = EMPLOY,
    income = INCOME,
    region4 = REGION4,
    metro = as.integer(METRO == 1)
  ) |>
  write_data("tess_2018.csv")

# Benchmarks ------------------------------------------------------------------------
# Shares of each group among Democrats and Republicans in the ANES 2016 pre-election
# wave, weighted by the full-sample pre-election weight. Partisans include leaners, which
# reproduces the numbers respondents were shown. The atheist-or-agnostic share shown (14)
# also counted ANES's "secular" identifiers; `strict` counts only atheists and agnostics.
anes <- read_dta(args[2], col_select = c(
  V160101, V161158x, V161310x, V161266a, V161266b, V161266c, V161266d,
  V161266j, V161266k, V161266m, V161267, V161361x
)) |>
  zap_labels() |>
  mutate(
    party = case_when(V161158x %in% 1:3 ~ "dem", V161158x %in% 5:7 ~ "rep"),
    age = if_else(V161267 > 0, V161267, NA),
    atheist_agnostic = V161266k == 1 | V161266m == 1 | V161266j == 1,
    atheist_agnostic_strict = V161266k == 1 | V161266m == 1,
    age_18_34 = age <= 34,
    nonwhite = if_else(V161310x > 0, V161310x != 1, NA),
    evangelical = V161266a == 1 | V161266b == 1 | V161266c == 1 | V161266d == 1,
    age_65_plus = age >= 65,
    income_250k = if_else(V161361x > 0, V161361x == 28, NA)
  ) |>
  filter(!is.na(party))

shown <- tribble(
  ~party, ~group, ~shown,
  "dem", "atheist_agnostic", 14,
  "dem", "age_18_34", 30,
  "dem", "nonwhite", 43,
  "rep", "evangelical", 37,
  "rep", "age_65_plus", 21,
  "rep", "income_250k", 4
)

anes_shares <- anes |>
  summarise(
    across(atheist_agnostic:income_250k, \(member) round(100 * weighted.mean(member, V160101, na.rm = TRUE), 1)),
    .by = party
  ) |>
  pivot_longer(-party, names_to = "group", values_to = "anes_2016")
strict_shares <- anes_shares |>
  filter(group == "atheist_agnostic_strict") |>
  transmute(party, group = "atheist_agnostic", anes_2016_strict = anes_2016)

shown |>
  left_join(anes_shares, by = c("party", "group")) |>
  left_join(strict_shares, by = c("party", "group")) |>
  write_data("benchmarks.csv")
