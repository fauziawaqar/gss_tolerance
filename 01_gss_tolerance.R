library(tidyverse)
library(haven)
library(srvyr)


gss <- read_dta("data/gss7224_r1.dta",
                col_select = c(year, wtssps,
                               spkath, colath, libath, spkrac, colrac, librac,
                               spkcom, colcom, libcom, spkmil, colmil, libmil,
                               spkhomo, colhomo, libhomo, happy, tvhours))


rc <- function(x, ok) case_when(x == ok ~ 0, !is.na(x) ~ 1, na_tag(x) == "d" ~ 1)

gss <- gss |>
  filter(year >= 1976) |>
  mutate(
    across(c(spkath, spkrac, spkcom, spkmil, spkhomo), \(x) rc(x, 1), .names = "{.col}_i"),  # speech: 1 = allow
    across(c(colath, colrac, colcom, colmil, colhomo), \(x) rc(x, 4), .names = "{.col}_i"),  # teach: 4 = allow
    across(c(libath, librac, libcom, libmil, libhomo), \(x) rc(x, 2), .names = "{.col}_i"),  # library: 2 = keep book
    score = rowSums(across(ends_with("_i"))),                        # 0-15 (Figure 1)
    rac   = rowSums(across(c(spkrac_i, colrac_i, librac_i))),        # 0-3 (Figure 2)
    homo  = rowSums(across(c(spkhomo_i, colhomo_i, libhomo_i))),     # 0-3 (Figure 2)
    intol = as.numeric(score > 0)                                    # 1 = intolerant (Figure 5)
  )


gss |> count(old = as.numeric(spkath), new = spkath_i)
gss |> count(old = as.numeric(colath), new = colath_i)
gss |> count(old = as.numeric(libath), new = libath_i)
gss |> group_by(year) |> summarise(N = n(), complete_score = sum(!is.na(score)))


by_year <- gss |>
  filter(!is.na(score), !is.na(wtssps)) |>
  as_survey_design(weights = wtssps) |>
  group_by(year) |>
  summarise(mean_score  = survey_mean(score, vartype = NULL),
            racists     = survey_mean(rac,   vartype = NULL),
            homosexuals = survey_mean(homo,  vartype = NULL),
            intolerant  = survey_mean(intol, vartype = NULL))

fig2_long <- by_year |>
  pivot_longer(c(racists, homosexuals), names_to = "group", values_to = "intolerance")


fig1_rep <- by_year |> filter(year <= 1998) |>
  ggplot(aes(year, mean_score)) + geom_line() + geom_point() + ylim(0, 15) +
  labs(title = "Figure 1: The 0-15 GSS Tolerance Battery, 1976-1998",
       subtitle = "High scores represent intolerance", x = "Year", y = "Mean intolerance score",
       caption = "Source: GSS, weighted by WTSSPS") + theme_minimal()

fig2_rep <- fig2_long |> filter(year <= 1998) |>
  ggplot(aes(year, intolerance, linetype = group)) + geom_line() + geom_point() + ylim(0, 3) +
  labs(title = "Figure 2: Intolerance Toward Racists and Homosexuals, 1976-1998",
       x = "Year", y = "Mean intolerance score (0-3)", linetype = "",
       caption = "Source: GSS, weighted by WTSSPS") + theme_minimal()

fig5_rep <- by_year |> filter(year <= 1998) |>
  ggplot(aes(year, intolerant)) + geom_line() + geom_point() + ylim(0, 1) +
  labs(title = "Figure 5: A Dichotomous Measure of Intolerance, 1976-1998",
       x = "Year", y = "Proportion intolerant",
       caption = "Source: GSS, weighted by WTSSPS") + theme_minimal()


fig1_ext <- ggplot(by_year, aes(year, mean_score)) + geom_line() + geom_point() +
  geom_vline(xintercept = 1998, linetype = "dotted") + ylim(0, 15) +
  labs(title = "Figure 1 Extended", x = "Year", y = "Mean intolerance score") + theme_minimal()

fig2_ext <- ggplot(fig2_long, aes(year, intolerance, linetype = group)) + geom_line() + geom_point() +
  geom_vline(xintercept = 1998, linetype = "dotted") + ylim(0, 3) +
  labs(title = "Figure 2 Extended", x = "Year", y = "Mean intolerance score (0-3)", linetype = "") + theme_minimal()

fig5_ext <- ggplot(by_year, aes(year, intolerant)) + geom_line() + geom_point() +
  geom_vline(xintercept = 1998, linetype = "dotted") + ylim(0, 1) +
  labs(title = "Figure 5 Extended", x = "Year", y = "Proportion intolerant") + theme_minimal()

d24 <- gss |>
  filter(year == 2024) |>
  mutate(happy_lbl = factor(case_when(happy == 1 ~ "Very happy",
                                      happy == 2 ~ "Pretty happy",
                                      happy == 3 ~ "Not too happy"),
                            levels = c("Very happy", "Pretty happy", "Not too happy")),
         tv = as.numeric(tvhours))

happy_plot <- d24 |> filter(!is.na(happy_lbl)) |>
  ggplot(aes(happy_lbl)) + geom_bar() +
  labs(title = "How Happy Are Americans?", x = "Answer", y = "Number of people",
       caption = "Source: GSS 2024") + theme_minimal()

tv_stats <- d24 |> summarise(mean = mean(tv, na.rm = TRUE), median = median(tv, na.rm = TRUE), sd = sd(tv, na.rm = TRUE))
tv_stats

tv_plot <- d24 |> filter(!is.na(tv)) |>
  ggplot(aes(tv)) + geom_histogram(binwidth = 1, center = 0) +
  geom_vline(xintercept = tv_stats$mean, linewidth = 1) +
  geom_vline(xintercept = tv_stats$median, linetype = "dashed") +
  labs(title = "Hours of TV Watched Per Day",
       subtitle = "Solid line = mean, dashed line = median",
       x = "Hours per day", y = "Number of people", caption = "Source: GSS 2024") + theme_minimal()

fig1_rep; fig2_rep; fig5_rep; fig1_ext; fig2_ext; fig5_ext; happy_plot; tv_plot
