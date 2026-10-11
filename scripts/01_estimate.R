source("scripts/00_config.R")

tess <- prepare_tess()
benchmarks <- read_data("benchmarks.csv")
write_result <- function(df, name) readr::write_csv(df, table_path(paste0(name, ".csv")))

samples <- list(
  dem = filter(tess, party == "dem"),
  rep = filter(tess, party == "rep"),
  pooled = tess
)
outcomes <- c("expressive", "in_party_id")

over_samples <- function(f, outcomes, samples) {
  expand_grid(outcome = outcomes, sample = names(samples)) |>
    mutate(result = map2(outcome, sample, \(o, s) f(samples[[s]], o))) |>
    unnest(result)
}

count(tess, party, arm) |>
  write_result("samples")

# Main effects. Holm adjustment runs over all 18 tests (2 outcomes x 3 samples x 3
# contrasts), the family the proposal's main-effect hypotheses define.
over_samples(arm_effects, outcomes, samples) |>
  mutate(p_holm = p.adjust(p_value, "holm")) |>
  write_result("effects")

over_samples(\(d, o) arm_effects(d, o, weighted = TRUE), outcomes, samples) |>
  write_result("effects_weighted")

map(outcomes, \(o) mutate(party_differences(tess, o), outcome = o)) |>
  list_rbind() |>
  write_result("party_differences")

# Leaners only: does correcting perceptions bring "undercover" partisans out?
over_samples(arm_effects, "claims_independent", map(samples, \(d) filter(d, leaner == 1))) |>
  write_result("undercover")

# Moderation by the size of the guess error and by membership in the stereotyped groups,
# tell vs. ask. Holm adjustment runs over the eight interactions (and, separately, the
# eight conditional effects).
expand_grid(party = c("dem", "rep"), outcome = outcomes, moderator = c("mean_error", "stereotype_share")) |>
  mutate(result = pmap(list(party, outcome, moderator), \(p, o, m) moderation(samples[[p]], o, m))) |>
  select(party, result) |>
  unnest(result) |>
  mutate(p_holm = p.adjust(p_value, "holm"), .by = term) |>
  write_result("moderation")

balance_covariates <- c("age", "female", "white", "college", "ideology7", "income", "leaner", "stereotype_share")
map(c("dem", "rep"), \(p) mutate(balance(samples[[p]], balance_covariates), party = p)) |>
  list_rbind() |>
  write_result("balance")

# Guesses made before the outcomes (ask and tell arms), against the shares shown.
tess |>
  filter(arm %in% c("ask", "tell")) |>
  pivot_longer(starts_with("guess_"), names_to = "group", names_prefix = "guess_", values_to = "guess") |>
  inner_join(benchmarks, by = c("party", "group")) |>
  filter(!is.na(guess)) |>
  summarise(
    median_guess = median(guess), mean_guess = mean(guess),
    share_over = mean(guess > shown), n = n(), .by = c(party, group)
  ) |>
  write_result("guesses")

# Reactions to the corrections: in the tell arm before the outcomes, and in the ask and
# control arms at the end of the survey.
bind_rows(
  tess |> filter(arm == "tell") |> count(party, reaction) |> mutate(timing = "before"),
  tess |> filter(arm != "tell") |> count(party, reaction = reaction_debrief) |> mutate(timing = "after")
) |>
  filter(!is.na(reaction)) |>
  mutate(share = n / sum(n), .by = c(party, timing)) |>
  write_result("reactions")

# The 2019 memo compared tell with ask and control pooled, by OLS without block
# adjustment. Kept to show the recoded data reproduce it.
map(c("dem", "rep"), \(p) {
  fit <- lm(expressive ~ tell, data = mutate(samples[[p]], tell = as.integer(arm == "tell")))
  broom::tidy(fit) |>
    filter(term == "tell") |>
    transmute(party = p, estimate, std_error = std.error, p_value = p.value)
}) |>
  list_rbind() |>
  write_result("memo_contrast")
