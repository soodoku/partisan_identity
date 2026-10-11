# Codebook

## `tess_2018.csv`

One row per respondent (n = 2,222) from TESS 013, fielded by NORC on AmeriSpeak, April 10–29, 2018. Codes are NORC's; `docs/questionnaire.pdf` has the wording and `docs/codebook_norc.xlsx` the original variable names. Web skips (98), refusals and unanswered number boxes (998) are blank.

| Column | NORC name | Contents |
|---|---|---|
| `case_id` | CaseId | NORC case ID |
| `weight` | WEIGHT1 | Post-stratification weight, controlled within party, scaled to n = 2,222 |
| `weight_within_party` | WEIGHT2 | Post-stratification weight, controlled within and scaled to each party |
| `arm` | P_TESS13 | `ask`, `tell` or `control` |
| `pid7_pre` | PARTYID7 | Panel party ID before the survey: 2 moderate Democrat, 3 lean Democrat, 5 lean Republican, 6 moderate Republican. Randomization block. |
| `guess_atheist_agnostic`, `guess_age_18_34`, `guess_nonwhite` | Q1_D_A–C | Democrats' guess of the % of Democrats in each group (0–100) |
| `guess_evangelical`, `guess_age_65_plus`, `guess_income_250k` | Q1_R_A–C | Republicans' guess of the % of Republicans in each group (0–100) |
| `reaction` | Q3_D, Q3_R | Tell arm, on the correction screen before the outcomes: party is 1 more, 2 as much, 3 less like me than I had thought |
| `reaction_debrief` | Q3_D_2, Q3_R_2 | The same question for ask and control respondents, after the debrief correction at the end of the survey |
| `party_post` | Q4 | 1 Democrat, 2 Republican, 3 independent, 4 other |
| `strength_dem` | Q5_A | 1 strong, 2 not very strong Democrat |
| `strength_rep` | Q5_B | 1 strong, 2 not very strong Republican |
| `lean` | Q5_C | 1 closer to Republicans, 2 neither, 3 closer to Democrats |
| `criticize_upsets` | Q6_A | When people criticize [party]: 1 upsets me a lot … 4 not at all |
| `meet_connected` | Q6_B | When I meet [party supporters]: 1 strongly connected … 5 strongly disconnected |
| `we_not_they` | Q6_C | Use "we" instead of "they": 1 all of the time … 5 never |
| `polls_happy` | Q6_D | When [party] does well in polls: 1 very happy … 5 very sad |
| `ideology` | IDEO | 1 extremely liberal … 7 extremely conservative; 8 haven't thought about it |
| `religion` | RELIG | 1 Protestant, 2 Catholic, 3 Mormon, 4 Orthodox, 5 Jewish, 6 Muslim, 7 Buddhist, 8 Hindu, 9 atheist, 10 agnostic, 11 nothing in particular, 12 just Christian, 13 Unitarian, 14 other |
| `born_again` | BORN | 1 yes, 2 no; asked of religion 1–4, 12, 14 |
| `attendance` | ATTEND | Religious services, 1 never … 9 several times a week |
| `political_interest` | Q6PA | 1 not interested at all … 5 extremely interested |
| `turnout_2016` | Q14A | 1 did not vote; 2–5 voted (election day, early in person, mail, other); 6 not sure |
| `vote_2016` | Q15A | 1 Clinton, 2 Trump, 3 other, 4 did not vote for president |
| `duration_minutes` | duration | Interview length as recorded by NORC (includes negative and multi-day values) |
| `device` | Device | Desktop, Smartphone or Tablet |
| `female` | GENDER | 1 female, 0 male |
| `age` | AGE | Years |
| `race_ethnicity` | RACETHNICITY | 1 white, 2 Black, 3 other, 4 Hispanic, 5 two or more, 6 Asian (all but 4 non-Hispanic) |
| `education` | EDUC | 1 no formal education … 9 high school graduate, 10 some college, 11 associate, 12 bachelor's, 13 master's, 14 professional or doctorate |
| `education4` | EDUC4 | NORC's four-category education |
| `marital` | MARITAL | NORC marital status code |
| `employment` | EMPLOY | 1 employee, 2 self-employed, 3 temporary layoff, 4 looking for work, 5 retired, 6 disabled, 7 other |
| `income` | INCOME | Household income, 1 under $5,000 … 18 $200,000 or more |
| `region4` | REGION4 | Census region |
| `metro` | METRO | 1 metropolitan area |

Dropped from the NORC file: free-text answers (Q4_OE, RELIG_OE, Q15A_OE), interview timestamps, state, household roster counts, and the correction screens shown (Q2_*), which `scripts/00_extract.R` checks are fully determined by the guesses.

## `benchmarks.csv`

| Column | Contents |
|---|---|
| `party`, `group` | The group asked about and the party it was asked about |
| `shown` | The % shown in the tell arm and the debrief |
| `anes_2016` | Weighted ANES 2016 share among identifiers and leaners, coded as when the shares were chosen |
| `anes_2016_strict` | Atheist or agnostic only, without ANES's "secular" identifiers |
