library(pacman)
p_load(tidyverse, janitor, glue, datapasta, formattable, extrafont, WriteXLS, explore, readxl, lubridate)

source("scripts/functions/helpers.r")

data_orig <- read_csv("data_in/openroles.csv", name_repair = make_clean_names)

data_orig |> tabyl(jobs_senior_leadership_team_member)

data_scored <- data_orig |>
  mutate(comp_grade = str_extract(jobs_compensation_grade, ".."),
         months_in_system = as.period(interval(jobs_creation_date_2, Sys.Date()), unit = "months")$month,
         location_simple = case_when(str_detect(jobs_location, "Canada|USA|United States|BC|14th|LA|Corwall|Drake|Burrard|3rd|Portland") ~ "North America"),
         score_grade = case_when(str_detect(comp_grade, "B") ~ 1,
                                 str_detect(comp_grade, "C[1-3]") ~2,
                                 str_detect(comp_grade, "C[4-6]") ~3,
                                 str_detect(comp_grade, "M[1-2]") ~5,
                                 str_detect(comp_grade, "M[3-4]") ~6,
                                 str_detect(comp_grade, "M[5-6]") ~7,
                                 str_detect(comp_grade, "E[1-3]") ~8,
                                 .default = 0),
         score_reason = case_when(jobs_reason_for_opening == "New Position" ~3,
                                  .default = 1),
         score_posted = if_else(jobs_is_posted == "Yes", 1, -1),
         score_core = case_when(jobs_senior_leadership_team_member %in% c("Burgoyne, Celeste", 
                                                                          "Cheung, Jonathan",
                                                                          "Choe, Sun",
                                                                          "Dagnese, Ted",
                                                                          "Maestrini, Andre",
                                                                          "Neuburger, Nikki") ~ 3,
                                str_detect(jobs_name, "Merchandise") ~4,
                                .default = 1),
         score_intake_meeting = if_else(is.na(jobs_intake_meeting_date), 0, 2),
         score_months_in_system = case_when(months_in_system >=12 ~ -1,
                                            months_in_system <12 & months_in_system >= 6 ~ 1,
                                            months_in_system <6 & months_in_system >= 3 ~ 3,
                                            .default = 6),
         score_location = if_else(is.na(location_simple), 3, 0)
         ) |>
  rowwise() |> 
  mutate(total = sum(score_grade, score_reason, score_posted, score_core, score_intake_meeting, score_months_in_system, score_location))

data_scored |> 
  ungroup() |> 
  filter(#score_reason == 3,
         !jobs_senior_leadership_team_member %in% c("Cheung, Jonathan",
                                                   "Choe, Sun",
                                                   "Maestrini, Andre",
                                                   "Ng, San Yan"),
         !jobs_recruitment_status == "Offer in Alignment",
         jobs_senior_leadership_team_member == "Frank, Meghan") |> 
  select(jobs_senior_leadership_team_member, jobs_name, comp_grade, total) |> 
  arrange(desc(total)) |> 
  head(50) |> 
  view()

data_orig |> tabyl(jobs_location)
