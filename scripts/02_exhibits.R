source("scripts/00_config.R")

read_result <- function(name) readr::read_csv(table_path(paste0(name, ".csv")), show_col_types = FALSE)

benchmarks <- read_data("benchmarks.csv")
samples <- read_result("samples")
effects <- read_result("effects")
effects_weighted <- read_result("effects_weighted")
party_differences <- read_result("party_differences")
undercover <- read_result("undercover")
moderation <- read_result("moderation")
balance <- read_result("balance")
guesses <- read_result("guesses")
reactions <- read_result("reactions")

group_label <- deframe(select(groups, group, label))
contrast_label <- deframe(select(contrasts, contrast, label))

# Figure: the three contrasts for each party and outcome.
effects_plot <- effects |>
  filter(sample != "pooled") |>
  mutate(
    contrast = factor(contrast_label[contrast], rev(contrasts$label)),
    party = factor(party_names[sample], party_names[c("rep", "dem")]),
    outcome = factor(outcome_names[outcome], outcome_names)
  ) |>
  ggplot(aes(x = estimate, y = contrast, xmin = conf_low, xmax = conf_high, colour = party)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_errorbar(orientation = "y", width = 0, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(
    values = c(Democrats = colours[["dem"]], Republicans = colours[["rep"]]),
    breaks = c("Democrats", "Republicans")
  ) +
  facet_wrap(~outcome, scales = "free_x") +
  labs(x = "Difference in means with 95% CI", y = NULL) +
  theme_paper()
save_figure(effects_plot, "effects", 6.5, 2.8)

# Table 1: shares shown, ANES 2016 shares, and guesses made before the outcomes.
guesses |>
  inner_join(benchmarks, by = c("party", "group")) |>
  transmute(
    Group = group_label[group],
    Shown = num(shown, 0),
    `ANES 2016` = num(anes_2016, 1),
    `Median guess` = num(median_guess, 0),
    `Mean guess` = num(mean_guess, 1),
    `Share overestimating` = pct(share_over),
    n = n
  ) |>
  write_table(table_path("guesses.tex"), "lrrrrrr", groups = c(
    "Democrats asked about Democrats" = 3, "Republicans asked about Republicans" = 3
  ))

# Table 2: main effects by party, the pooled sample and the Republican-Democratic gap.
main_effects_table <- function(effects, party_differences) {
  by_sample <- effects |>
    mutate(cell = estimate_cell(estimate, std_error, p_value)) |>
    select(outcome, contrast, sample, cell) |>
    pivot_wider(names_from = sample, values_from = cell)
  gap <- party_differences |>
    transmute(outcome, contrast, gap = estimate_cell(estimate, std_error, p_value))
  by_sample |>
    left_join(gap, by = c("outcome", "contrast")) |>
    transmute(
      Contrast = contrast_label[contrast], Democrats = dem, Republicans = rep, Pooled = pooled,
      `Rep. - Dem.` = gap
    )
}
main_effects_table(effects, party_differences) |>
  write_table(table_path("effects.tex"), "lrrrr", groups = c(
    "Expressive partisanship (0-1)" = 3, "In-party identification (1-7)" = 3
  ))

# Table 3: moderation, tell vs. ask. Interactions are per standard deviation of the
# moderator.
moderator_names <- c(mean_error = "Mean guess error", stereotype_share = "Share of stereotyped groups")
moderation |>
  mutate(
    estimate = if_else(term == "tell_x_moderator", estimate * moderator_sd, estimate),
    std_error = if_else(term == "tell_x_moderator", std_error * moderator_sd, std_error)
  ) |>
  select(party, outcome, moderator, term, estimate, std_error, p_value, p_holm, n) |>
  pivot_wider(names_from = term, values_from = c(estimate, std_error, p_value, p_holm)) |>
  arrange(outcome, moderator, party) |>
  transmute(
    Party = party_names[party],
    Moderator = moderator_names[moderator],
    `Tell vs. ask at mean` = estimate_cell(estimate_tell, std_error_tell, p_value_tell),
    `Tell x moderator (per SD)` = estimate_cell(
      estimate_tell_x_moderator, std_error_tell_x_moderator, p_value_tell_x_moderator
    ),
    `Holm p` = num(p_holm_tell_x_moderator, 2),
    n = n
  ) |>
  write_table(table_path("moderation.tex"), "llrrrr", groups = c(
    "Expressive partisanship (0-1)" = 4, "In-party identification (1-7)" = 4
  ))

# Appendix tables.
covariate_names <- c(
  age = "Age", female = "Female", white = "White, non-Hispanic", college = "Bachelor's degree",
  ideology7 = "Ideology (1-7, conservative high)", income = "Income (1-18)", leaner = "Leaner",
  stereotype_share = "Share of stereotyped groups"
)
balance |>
  transmute(
    Covariate = covariate_names[covariate],
    Control = num(control), Ask = num(ask), Tell = num(tell), `Joint p` = num(p_value, 2)
  ) |>
  write_table(table_path("balance.tex"), "lrrrr", groups = c(Democrats = 8, Republicans = 8))

bind_rows(
  effects_weighted |> mutate(spec = "Weighted"),
  undercover |> mutate(spec = "Leaners")
) |>
  mutate(cell = estimate_cell(estimate, std_error, p_value)) |>
  select(spec, outcome, contrast, sample, cell) |>
  pivot_wider(names_from = sample, values_from = cell) |>
  transmute(
    Contrast = contrast_label[contrast], Democrats = dem, Republicans = rep, Pooled = pooled
  ) |>
  write_table(table_path("robustness.tex"), "lrrr", groups = c(
    "Weighted: expressive partisanship" = 3, "Weighted: in-party identification" = 3,
    "Leaners only: calls self independent" = 3
  ))

reaction_names <- c("More like me", "As much like me", "Less like me")
reactions |>
  mutate(column = paste(party_names[party], if_else(timing == "before", "(tell)", "(ask, control)"))) |>
  select(reaction, column, share) |>
  pivot_wider(names_from = column, values_from = share, values_fn = pct) |>
  mutate(Reaction = reaction_names[reaction], .before = 1) |>
  select(-reaction) |>
  write_table(table_path("reactions.tex"), "lrrrr")

# Manuscript numbers.
effect <- function(outcome_name, sample_name, contrast_name, col) {
  the(effects, {{ col }}, outcome == outcome_name, sample == sample_name, contrast == contrast_name)
}
moderation_term <- function(party_name, moderator_name, col) {
  the(
    moderation, {{ col }},
    party == party_name, outcome == "expressive", moderator == moderator_name, term == "tell_x_moderator"
  )
}
guess <- function(group_name, col) the(guesses, {{ col }}, group == group_name)
benchmark <- function(group_name, col) the(benchmarks, {{ col }}, group == group_name)
reaction_share <- function(party_name, timing_name) {
  the(reactions, share, party == party_name, timing == timing_name, reaction == 1)
}
effect_interval <- function(sample_name) {
  interval(
    effect("expressive", sample_name, "tell_ask", conf_low), effect("expressive", sample_name, "tell_ask", conf_high), 3
  )
}
other_expressive <- filter(effects, outcome == "expressive", !(sample == "rep" & contrast == "tell_ask"))
in_party_effects <- filter(effects, outcome == "in_party_id")
weighted_rep_tell_ask <- filter(effects_weighted, outcome == "expressive", sample == "rep", contrast == "tell_ask")
gap_tell_ask <- filter(party_differences, outcome == "expressive", contrast == "tell_ask")
dem_error_sd <- moderation_term("dem", "mean_error", moderator_sd)

write_macros(list(
  nTotal = format(sum(samples$n), big.mark = ","),
  nDem = format(sum(samples$n[samples$party == "dem"]), big.mark = ","),
  nRep = format(sum(samples$n[samples$party == "rep"]), big.mark = ","),
  shownRich = num(benchmark("income_250k", shown), 0),
  anesRich = num(benchmark("income_250k", anes_2016), 1),
  shownAtheist = num(benchmark("atheist_agnostic", shown), 0),
  anesAtheistStrict = num(benchmark("atheist_agnostic", anes_2016_strict), 1),
  guessRich = num(guess("income_250k", median_guess), 0),
  overRich = pct(guess("income_250k", share_over)),
  guessOld = num(guess("age_65_plus", median_guess), 0),
  overOld = pct(guess("age_65_plus", share_over)),
  overAtheist = pct(guess("atheist_agnostic", share_over)),
  overNonwhite = pct(guess("nonwhite", share_over)),
  repTellAsk = num(effect("expressive", "rep", "tell_ask", estimate), 3),
  repTellAskCI = effect_interval("rep"),
  repTellAskP = p_value(effect("expressive", "rep", "tell_ask", p_value)),
  repTellAskHolm = num(effect("expressive", "rep", "tell_ask", p_holm), 2),
  repAskControl = num(effect("expressive", "rep", "ask_control", estimate), 3),
  repAskControlP = num(effect("expressive", "rep", "ask_control", p_value), 2),
  repTellControl = num(effect("expressive", "rep", "tell_control", estimate), 3),
  repTellControlP = num(effect("expressive", "rep", "tell_control", p_value), 2),
  repControlMean = num(effect("expressive", "rep", "tell_control", reference_mean), 2),
  demTellAsk = num(effect("expressive", "dem", "tell_ask", estimate), 3),
  demTellAskCI = effect_interval("dem"),
  gapTellAsk = num(gap_tell_ask$estimate, 3),
  gapTellAskP = num(gap_tell_ask$p_value, 2),
  minExpressiveHolm = num(min(other_expressive$p_holm), 2),
  minInPartyP = num(min(in_party_effects$p_value), 2),
  weightedRepTellAsk = num(weighted_rep_tell_ask$estimate, 3),
  weightedRepTellAskP = num(weighted_rep_tell_ask$p_value, 2),
  minUndercoverP = num(min(undercover$p_value), 2),
  demErrorSD = num(dem_error_sd, 0),
  demErrorInteraction = num(moderation_term("dem", "mean_error", estimate) * dem_error_sd, 3),
  demErrorInteractionP = num(moderation_term("dem", "mean_error", p_value), 2),
  demErrorInteractionHolm = num(moderation_term("dem", "mean_error", p_holm), 2),
  repStereoInteractionP = num(moderation_term("rep", "stereotype_share", p_value), 2),
  moreLikeDem = pct(reaction_share("dem", "before")),
  moreLikeRep = pct(reaction_share("rep", "before")),
  moreLikeDebriefRep = pct(reaction_share("rep", "after"))
), table_path("macros.tex"))
