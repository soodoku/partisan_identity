# Estimators. Arms were randomized with equal probability within each panel party-ID
# category (2, 3, 5, 6), so every model conditions on that block. Standard errors are
# HC2, estimatr's default and the Neyman variance estimator for a randomized experiment.

# The proposal's three comparisons: being asked for guesses (ask vs. control), being
# told the truth after guessing (tell vs. ask, the information effect), and both
# together (tell vs. control).
contrasts <- tribble(
  ~contrast, ~treatment, ~reference, ~label,
  "ask_control", "ask", "control", "Ask vs. control",
  "tell_ask", "tell", "ask", "Tell vs. ask",
  "tell_control", "tell", "control", "Tell vs. control"
)

# Within a single block (e.g. Democratic leaners only) there is nothing to condition on.
block_term <- function(data) if (n_distinct(data$pid7_pre) > 1) " + factor(pid7_pre)" else ""

fit_arms <- function(data, outcome, reference, weighted = FALSE, by_party = FALSE) {
  # Blocks are party x leaner, so the interaction model spans them with party and
  # party:leaner rather than block dummies that would be collinear with party.
  rhs <- if (by_party) "arm * party + party:leaner" else paste0("arm", block_term(data))
  data <- mutate(data, arm = relevel(arm, reference))
  # lm_robust captures `weights` unevaluated, so the column is chosen by name.
  if (weighted) {
    estimatr::lm_robust(as.formula(paste(outcome, "~", rhs)), data = data, weights = weight)
  } else {
    estimatr::lm_robust(as.formula(paste(outcome, "~", rhs)), data = data)
  }
}

tidy_term <- function(fit, term) {
  estimatr::tidy(fit) |>
    filter(.data$term == !!term) |>
    transmute(estimate, std_error = std.error, conf_low = conf.low, conf_high = conf.high, p_value = p.value)
}

# All three contrasts for one outcome in one sample.
arm_effects <- function(data, outcome, weighted = FALSE) {
  data <- filter(data, !is.na(.data[[outcome]]))
  contrasts |>
    mutate(
      fit = map(reference, \(r) fit_arms(data, outcome, r, weighted)),
      result = map2(fit, treatment, \(f, t) tidy_term(f, paste0("arm", t))),
      reference_mean = map_dbl(reference, \(r) mean(data[[outcome]][data$arm == r])),
      n = map2_int(treatment, reference, \(t, r) sum(data$arm %in% c(t, r)))
    ) |>
    select(-fit) |>
    unnest(result)
}

# Republican minus Democratic effect for each contrast, from one model with arm-by-party
# interactions.
party_differences <- function(data, outcome) {
  data <- filter(data, !is.na(.data[[outcome]]))
  contrasts |>
    mutate(
      fit = map(reference, \(r) fit_arms(data, outcome, r, by_party = TRUE)),
      result = map2(fit, treatment, \(f, t) tidy_term(f, paste0("arm", t, ":partyrep")))
    ) |>
    select(-fit) |>
    unnest(result)
}

# Tell vs. ask, with the treatment interacted with a moderator measured before the
# outcomes. Control respondents gave their guesses after the outcomes, so moderation by
# guesses uses only these two arms. The moderator is centered, so `tell` is the effect
# for a respondent at the sample mean.
moderation <- function(data, outcome, moderator) {
  data <- data |>
    filter(arm %in% c("ask", "tell"), !is.na(.data[[outcome]]), !is.na(.data[[moderator]])) |>
    mutate(tell = as.integer(arm == "tell"), moderator = .data[[moderator]] - mean(.data[[moderator]]))
  fit <- estimatr::lm_robust(
    as.formula(paste0(outcome, " ~ tell * moderator", block_term(data))),
    data = data
  )
  bind_rows(
    tidy_term(fit, "tell") |> mutate(term = "tell"),
    tidy_term(fit, "tell:moderator") |> mutate(term = "tell_x_moderator")
  ) |>
    mutate(moderator = moderator, outcome = outcome, n = nrow(data), moderator_sd = sd(data$moderator))
}

# Mean of each pre-treatment covariate by arm, with the robust Wald test (chi-squared,
# 2 df) that the two arm coefficients are jointly zero given the blocks.
balance <- function(data, covariates) {
  covariates |>
    map(\(covariate) {
      d <- filter(data, !is.na(.data[[covariate]]))
      fit <- estimatr::lm_robust(as.formula(paste0(covariate, " ~ arm", block_term(d))), data = d)
      d |>
        summarise(mean = mean(.data[[covariate]]), .by = arm) |>
        pivot_wider(names_from = arm, values_from = mean) |>
        mutate(covariate = covariate, p_value = arm_wald_p(fit))
    }) |>
    list_rbind()
}

arm_wald_p <- function(fit) {
  terms <- c("armask", "armtell")
  b <- coef(fit)[terms]
  wald <- drop(t(b) %*% solve(vcov(fit)[terms, terms]) %*% b)
  pchisq(wald, df = length(terms), lower.tail = FALSE)
}
