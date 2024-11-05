library(tidyverse)
library(readxl)

# Can you visually represent partisan sorting giving the impression of increased polarization?

###############################################
# Take 3

# Status 1 - Political parties as diverse coalitions of many interests
# per Fiorina and Abrams, imagine four issues and all voters are lib on 2 and con on 2 but they aren't correlated
# Per the General Social Survey (GSS): In the 1970s, there was not much difference between Democrats and Republicans, in terms of their political views (see https://www.allendowney.com/blog/2022/09/11/polarization-and-partisan-sorting/)
# IN OTHER WORDS, party ID was not a strong predictor of ideology (e.g. plenty of conservatives in the Dems and liberals in the Repubs)
# SHOW: Two parties with significant overlap in their distributions

# Status 2 - Replacement and Sorting
# polarization between the parties is not caused by people changing their minds; it is caused by changes in the composition of the groups.
# Generational replacement: In the conveyor belt of demography, when old people die, they are replaced by young people, and
# Partisan sorting: No one is born Democrat or Republican; rather, they choose a party (or not) at some point in their lives, and this sorting process has changed over time.

# voters1 <- tibble(
#   economics = rnorm(n = 500, mean = 0, sd = 3),
#   social = rnorm(n = 500, mean = 0, sd = 3),
#   party1 = sample(size = 500, x = c("Democratic", "Republican"), replace = TRUE, prob = c(.5, .5)),
#   party2 = case_when(
#     economics > 0 & social >= 0 ~ "Republican",
#     economics <= 0 & social <= 0 ~ "Democratic",
#     economics >= 2 & social >= -2 ~ "Republican",
#     social >= 2 & economics >= -2 ~ "Republican",
#     social <= -3 & economics <=2 ~ "Democratic",
#     social <= 2 & economics <= -3 ~ "Democratic")
# )
# 
# write_excel_csv(voters1, "13-1-simulated_voters.csv")

# Voters on Economic Dimension
voters1 <- read_csv("13-1-simulated_voters.csv")

voters1 |>
  ggplot(aes(x = economics)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  labs(x = "", y = "",
       title = "The distribution of Americans in terms of preferred economic policies") +
  #theme(axis.title.x=element_blank(), axis.text.x=element_blank())
  scale_x_continuous(breaks = c(-9, 8.5), labels = c("Less\nEconomic\nRegulation", "More\nEconomic\nRegulation"))

# Voters on Social Dimension
voters1 |>
  ggplot(aes(x = social)) +
  geom_histogram(bins = 20) +
  theme_bw() +
  labs(x = "", y = "",
       title = "The distribution of Americans in terms of preferred social policies") +
  scale_x_continuous(breaks = c(-9, 9), labels = c("Progressive\nValues", "Traditional\nValues"))

# View both
voters1 |>
  ggplot(aes(x = economics, y = social)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues"))

# US parties in the 1970s
# LOTS of room for compromise between the parties
voters1 |>
  ggplot(aes(x = economics, y = social, color = party1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red"))


# US parties have been sorting since the 1970s

# Illustrate the sorting step-by-step
# 1. Dems with conservative views on econ and social, convert them to republicans!
voters1 |>
  ggplot(aes(x = economics, y = social, color = party1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red")) +
  annotate("rect", xmin = 2.7, xmax = 5.1, ymin = 4.9, ymax = 10.2, alpha = .2)

# 2. convert them to republicans
voters1 |>
  mutate(
    party1a = case_when(
      party1 == "Democratic" & economics >= 2.65 & social >=4.8 ~ "Republican",
      TRUE ~ party1)
  ) |>
  ggplot(aes(x = economics, y = social, color = party1a)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red")) +
  annotate("rect", xmin = 2.7, xmax = 5.1, ymin = 4.9, ymax = 10.2, alpha = .2)

# 3. Repeat for more voters in this quadrant
voters1 |>
  mutate(
    party1a = case_when(
      party1 == "Democratic" & economics > 0 & social > 0 ~ "Republican",
      TRUE ~ party1)
  ) |>
  ggplot(aes(x = economics, y = social, color = party1a)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red"))

# 4. Repeat for more liberal voters
voters1 |>
  mutate(
    party1a = case_when(
      party1 == "Democratic" & economics > 0 & social > 0 ~ "Republican",
      party1 == "Republican" & economics < 0 & social < 0 ~ "Democratic",
      TRUE ~ party1)
  ) |>
  ggplot(aes(x = economics, y = social, color = party1a)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red"))

# 5. Sorted parties appear polarized WITHOUT moving ANY voters
voters1 |>
  ggplot(aes(x = economics, y = social, color = party2)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .6) +
  theme_void() +
  labs(x = "", y = "") +
  annotate("text", x = c(-10, 10), y = -1.5, label = c("More\nEconomic\nRegulation", "Less\nEconomic\nRegulation")) +
  annotate("text", x = 1, y = c(-10, 10), label = c("Progressive\nValues", "Traditional\nValues")) +
  guides(color = "none") + 
  scale_color_manual(values = c("blue", "red")) +
  annotate("rect", xmin = -1, xmax = 10, ymin = -1, ymax = 10, fill = "red", alpha = .2) +
  annotate("rect", xmin = 1, xmax = -10, ymin = 1, ymax = -10, fill = "blue", alpha = .2)






###############################################
# Take 2
# Use a single dimension (one aggregate left-right scale)
# Simply move the extremes into the other party and show how it makes polarization appear to have increased

system1 <- tibble(
  party = c(rep("Party Left", 500), rep("Party Right", 500)),
  position1 = c(rnorm(n = 500, mean = -1, sd = .4), rnorm(n = 500, mean = 1, sd = .4))
)

# # View the wider society
# system1 |>
#   ggplot(aes(x = position1)) +
#   geom_density() +
#   theme_bw()

# View the party divide
system1 |>
  ggplot(aes(x = position1, fill = party)) +
  #geom_histogram() +
  geom_density(alpha = .8) +
  theme_bw() +
  scale_fill_brewer(type = "qual", palette = 4, direction = -1) +
  guides(fill = "none") +
  labs(x = "Left-Right Ideology Scale", y = "")

# Move the 10% most positive scores from party b to party a
# Move the 10% most negative scores from party a to party b
# Trying to recode both in one mutate doesn't work (after first change some rows get moved back by second recode)
change_left_party <- quantile(system1$position1[system1$party == "Party Left"], probs = .9)
change_right_party <- quantile(system1$position1[system1$party == "Party Right"], probs = .1)

# system2 <- system1 |>
#   mutate(
#     move1 = if_else(party == "Party Left" & position1 <= move_a[[1]], "Party B", "0"),
#     move2 = if_else(party == "Party B" & position1 >= move_b[[1]], "Party Left", "0"),
#     party1 = case_when(
#       move1 == "Party B" ~ "Party B",
#       move2 == "Party A" ~ "Party A",
#       TRUE ~ party)
#   )

# Move one party's extremes
# e.g. blue dog dems join the republicans
system2 <- system1 |>
  mutate(
    party1 = case_when(
      party == "Party Left" & position1 >= change_left_party[[1]] ~ "Party Right",
      TRUE ~ party)
  )

system2 |>
  ggplot(aes(x = position1, fill = party1)) +
  geom_histogram(bins=15) +
  #geom_density(alpha = .8) +
  theme_bw() +
  scale_fill_brewer(type = "qual", palette = 4, direction = -1) +
  guides(fill = "none") +
  labs(x = "Left-Right Ideology Scale", y = "")


###############################################
# This version mirrors the vague description in Fiorina and Abrams but the results are less impressive than they need to be to help the class see what happened

# 1000 voters
# 4 polarized issues but each voter holds 2 lib positions and 2 cons positions at random

# 6 possible arrangments of 2x2 (100 each = 600 voters)
# lib 1 and 2
# lib 1 and 3
# lib 1 and 4
# lib 2 and 3
# lib 2 and 4
# lib 3 and 4
population1 <- tibble(
  Issue1 = c(rep("Lib", 300), rep("Con", 300)),
  Issue2 = c(rep("Lib", 100), rep("Con", 200), rep("Lib", 200), rep("Con", 100)),
  Issue3 = c(rep("Con", 100), rep("Lib", 100), rep("Con", 100), rep("Lib", 100), rep("Con", 100), rep("Lib", 100)),
  Issue4 = c(rep("Con", 200), rep("Lib", 100), rep("Con", 100), rep("Lib", 200))
)

# Convert positions to random numbers 
population2 <- population1 |>
  mutate(
    Issue1a = case_when(
      Issue1 == "Lib" ~ rnorm(n = 600, mean = -1, sd = .3),
      Issue1 == "Con" ~ rnorm(n = 600, mean = 1, sd = .3)),
    Issue2a = case_when(
      Issue2 == "Lib" ~ rnorm(n = 600, mean = -1, sd = .3),
      Issue2 == "Con" ~ rnorm(n = 600, mean = 1, sd = .3)),
    Issue3a = case_when(
      Issue3 == "Lib" ~ rnorm(n = 600, mean = -1, sd = .3),
      Issue3 == "Con" ~ rnorm(n = 600, mean = 1, sd = .3)),
    Issue4a = case_when(
      Issue4 == "Lib" ~ rnorm(n = 600, mean = -1, sd = .3),
      Issue4 == "Con" ~ rnorm(n = 600, mean = 1, sd = .3))
  )

# Create two random political parties
party1 <- population2 |>
  slice_sample(n = 1000, replace = TRUE) |>
  mutate(Party = "Party A")

party2 <- population2 |>
  slice_sample(n = 1000, replace = TRUE) |>
  mutate(Party = "Party B")

# Combine and examine
d1 <- rbind(party1, party2)

d1 |>
  count(Issue1)

d1 |>
  count(Issue3)

# Society is polarized on an issue
ggplot(d1, aes(x = Issue1a)) +
  geom_histogram(bins = 25) +
  theme_bw()

# But pre-sorting there is 


ggplot(d1, aes(x = Issue1a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1) +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

# Partisan sorting DRAMATICALLY impacts that issue
# Move all Issue1 "Con" in Party A to Party B
# Move all Issue1 "Lib" in Party B to Party A
d1 |> count(Party)

d2 <- d1 |>
  mutate(
    Party = case_when(
      Party == "Party A" & Issue1 == "Con" ~ "Party B",
      Party == "Party B" & Issue1 == "Lib" ~ "Party A",
      TRUE ~ Party)
  )

d2 |> count(Party)

ggplot(d2, aes(x = Issue1a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1) +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

# Does it also impact the other issues? A bit!
d2 |> count(Issue1)
d2 |> count(Issue2)
d2 |> count(Party, Issue2)

ggplot(d1, aes(x = Issue2a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

ggplot(d2, aes(x = Issue2a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

# Sort on a second issue too!
# Move all Issue2 "Con" in Party A to Party B
# Move all Issue2 "Lib" in Party B to Party A
d3 <- d2 |>
  mutate(
    Party = case_when(
      Party == "Party A" & Issue2 == "Con" ~ "Party B",
      Party == "Party B" & Issue2 == "Lib" ~ "Party A",
      TRUE ~ Party)
  )

d3 |> count(Party)

ggplot(d1, aes(x = Issue2a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

ggplot(d3, aes(x = Issue2a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")


ggplot(d1, aes(x = Issue3a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")

ggplot(d3, aes(x = Issue3a, fill = Party)) +
  geom_histogram(bins = 25) +
  theme_bw() +
  facet_wrap(~ Party, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual") +
  guides(fill = "none")
