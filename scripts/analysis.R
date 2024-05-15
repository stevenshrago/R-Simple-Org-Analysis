library(pacman)
p_load(tidyverse, janitor, glue, datapasta, formattable, extrafont, WriteXLS)

source("scripts/functions/helpers.r")

## Load data  ---- 

data_orig <- readxl::read_xlsx("data_in/230206.xlsx", skip = 3) |> 
  clean_names() |>
  mutate(date = "2023-02-06") |> 
  bind_rows(readxl::read_xlsx("data_in/240422.xlsx", skip = 3) |> 
              clean_names() |> 
              mutate(date = "2024-04-22"))

## Clean and prepare  ---- 

grade_levels <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5", "C6", "M1", "M2", "M3", "M4", "M5" ,"E1", "E2", "E3")

grade_levels_overlap <- c("B1", "B2", "B3-C1", "B4-C2", "C3-M1", "C4-M2", "C5-M3", "C6-M4", "C7-M5", "E1", "E2")

finance <- data_orig |>
  mutate(supervisory_organization_level_2 = if_else(supervisory_organization_level_2 == "Supply Chain, Distribution, Sourcing (Ted Dagnese)", 
                                                    "Supply Chain (Ted Dagnese)",
                                                    supervisory_organization_level_2),
         compensation_grade = str_remove_all(compensation_grade, "L|N|P|S|T"),
         compensation_grade = if_else(str_starts(compensation_grade, "R"), 
                                      str_trunc(compensation_grade, width = 2, side = "right", ellipsis = ""), 
                                      compensation_grade)) |> 
  filter(
    supervisory_organization_level_2 == "Legal (Shannon Higginson)",
         currently_active == "Yes",
         is.na(on_leave)) |> 
  mutate(compensation_grade = factor(compensation_grade, levels = grade_levels),
         comp_grade_simple = factor(case_when(str_detect(compensation_grade, "B") ~ "B",
                                       str_detect(compensation_grade, "C") ~ "C",
                                       str_detect(compensation_grade, "M") ~ "M",
                                       str_detect(compensation_grade, "E") ~ "E"), levels = c("B", "C", "M", "E")),
         comp_grade_overlap = factor(case_when(str_detect(compensation_grade, "B3|C1") ~ "B3-C1",
                                        str_detect(compensation_grade, "B4|C2") ~ "B4-C2",
                                        str_detect(compensation_grade, "C3|M1") ~ "C3-M1",
                                        str_detect(compensation_grade, "C4|M2") ~ "C4-M2",
                                        str_detect(compensation_grade, "C5|M3") ~ "C5-M3",
                                        str_detect(compensation_grade, "C6|M4") ~ "C6-M4",
                                        str_detect(compensation_grade, "C7|M5") ~ "C7-M5",
                                        .default = compensation_grade), levels = grade_levels_overlap)) 

managers <- finance |> 
  select(employee_id, employee, workforce_segment, job_family_group, job_family, job_profile, job_title, position_id, management_level, employee_type, time_type, gender, fte_percent, manager_id, managers_position, compensation_grade, comp_grade_simple, comp_grade_overlap, date)


finance_full <- finance |> 
  left_join(managers, by = c("date", "manager_id" = "employee_id"), suffix = c("", "_mgr"))


## **Viz 1 Manager Percentages  ---- 

finance |> 
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |> 
  mutate(is_manager_b = if_else(direct_reports >0, "yes", NA)) |> 
  tabyl(supervisory_organization_level_3, is_manager_b) |> 
  rowwise() |> 
  mutate(perc = percent(yes/(yes+NA_), digits = 0)) |> 
  drop_na(supervisory_organization_level_3) |>
  filter(yes > 0) |> 
  arrange(desc(perc)) |> 
  mutate(high_ok = if_else(perc <= 0.25, "ok", "high")) |> 
  ggplot(aes(x = reorder(supervisory_organization_level_3, perc), y = perc)) +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 0.15, ymax = .25), fill = neutral_5) +
  geom_col(aes(fill = high_ok)) +
  geom_text(aes(y = perc/2, label = perc), color = offwhite, family = lulu_font, fontface = "bold") +
  geom_hline(yintercept = 0.15, color = offblack, linetype = "dashed") +
  geom_hline(yintercept = 0.25, color = offblack, linetype = "dashed") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("high" = neutral_3, "ok" = green)) +
  scale_x_discrete(label = scales::label_wrap(20)) +
  coord_flip() +
  labs(title = "Line managers as a percentage of total team headcount",
       x = "L3 Area",
       y = "Manager percentage") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  annotate("text", x = 0.5, y = 0.15, label = "15%", color = hotheat, size = 2, family = lulu_font, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.25, label = "25%", color = hotheat, size = 2, family = lulu_font, fontface = "bold")


## Viz 2 Spans distribution  ---- 

finance |>
  filter(date == "2024-04-22") |>
  left_join(finance |>
             filter(date == "2024-04-22") |> 
             count(manager_id) |> 
             rename(direct_reports = n),
           by = c("employee_id" = "manager_id")) |>
  mutate(low_span = if_else(direct_reports < 4, "low", "ok")) |> 
  drop_na(supervisory_organization_level_3) |> 
  filter(direct_reports <11) |> 
  ggplot(aes(x = direct_reports)) +
  geom_bar(aes(fill = low_span)) +
  geom_text(stat='count', aes(label=..count..), family = lulu_font, size = 3, vjust = -1) +
  facet_wrap(~ supervisory_organization_level_3, ncol = 2) +
  scale_x_continuous(breaks = c(1:10)) +
  labs(title = "Frequency of number of direct reports",
       subtitle = "Excluding managers with greater than 10 direct reports",
       x = "Number of direct reports",
       y = "Number of managers") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_fill_manual(values = c("low" = hotheat, "ok" = neutral_3)) +
  standard_bar_lims(hi = 19)


## Viz 3 Cumulative percentage of low spans  ---- 

finance |>
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |>
  mutate(low_span = if_else(direct_reports < 4, "low", "ok")) |> 
  drop_na(supervisory_organization_level_3, direct_reports) |> 
  count(supervisory_organization_level_3, direct_reports) |> 
  group_by(supervisory_organization_level_3) |> 
  mutate(no_managers = sum(n),
         cum_sum = cumsum(n),
         cum_perc = percent(cum_sum/no_managers, digits = 0),
         label = if_else(direct_reports == 3, str_wrap(glue("{cum_perc} of managers with 3 or fewer direct reports"), width = 25), NA)) |> 
  filter(direct_reports <11) |> 
  ggplot(aes(x = direct_reports, y = cum_perc, group = supervisory_organization_level_3)) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1.1), fill = neutral_5) +
  scale_x_continuous(breaks = c(1:10))+
  scale_y_continuous(labels = scales::percent, breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  geom_line(size = 1, color = offblack) +
  geom_text(aes(label = label), family = lulu_font, color = offblack, size = 3, hjust = -0.1, fontface = "bold") +
  geom_vline(xintercept = 3, colour = hotheat, linetype = "dashed") +
  facet_wrap(~ supervisory_organization_level_3) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  labs(title = "Cumulative percentage of managers at increasing span levels",
       subtitle = "Excluding L3 managers")

## Viz 4 Average span of control  ---- 

finance |>
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |> 
  group_by(supervisory_organization_level_3) |> 
  summarise(average_span = round(mean(direct_reports, na.rm = TRUE),1.1)) |> 
  arrange(desc(average_span)) |>
  drop_na(supervisory_organization_level_3, average_span) |> 
  mutate(low_ok = if_else(average_span >= 4 & average_span <= 7, "ok", "low")) |> 
  ggplot(aes(x = reorder(supervisory_organization_level_3, average_span), y = average_span)) +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 4, ymax = 7), fill = neutral_5) +
  geom_col(aes(fill = low_ok)) +
  geom_hline(yintercept = 4, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 7, color = neutral_3, linetype = "dashed") +
 
  geom_text(aes(label = average_span, y = average_span/2), color = offwhite, family = lulu_font, fontface = "bold") +
  coord_flip() +
  labs(title = "Average span of control",
       subtitle = "Excluding L3 leader",
       x = "L3 area",
       y = "Average span of control") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  scale_x_discrete(labels = label_wrap_gen()) +
  scale_fill_manual(values = c("ok" = green, "low" = neutral_3)) +
  annotate("text", x = 0.5, y = 4, label = "4 direct\n reports", color = hotheat, size = 2, family = lulu_font, fontface = "bold") +
  annotate("text", x = 0.5, y = 7, label = "7 direct\n reports", color = hotheat, size = 2, family = lulu_font, fontface = "bold")


## Viz 5 Compression  ---- 

finance_full |> 
  filter(comp_grade_overlap == comp_grade_overlap_mgr,
         date == "2024-04-22") |> 
  count(supervisory_organization_level_4, comp_grade_overlap) |> 
  ggplot(aes(x = comp_grade_overlap, y = n)) +
  geom_col(fill = neutral_3) +
  geom_text(aes(y = n/2, label = n), family = lulu_font, colour = offwhite, fontface = "bold") +
  facet_wrap(~ supervisory_organization_level_4) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  labs(title = "Grade compression",
       subtitle = "Employees reporting to a manager at the same contribution level",
       x = "Compensation grade levels",
       y = "Number of compressed reporting relationships")





## Vix 6 Grade development  ---- 

finance |> 
  tabyl(comp_grade_overlap, date, show_na = FALSE) |> 
  rename("Feb2023" = "2023-02-06",
         "Apr2024" = "2024-04-22") |> 
  mutate(across(2:3, ~ percent(.x/sum(.x), digits = 1))) |> 
  pivot_longer(2:3) |> 
  ggplot(aes(x = comp_grade_overlap, y = value, group = desc(name))) +
  geom_col(aes(fill = name), position = position_dodge(width = 1)) +
  geom_text(aes(label = value, y = value/2), position = position_dodge(width = 1), family = lulu_font, color = offblack, size = 3) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_fill_manual(values = c("Feb2023" = neutral_4, 
                              "Apr2024" = neutral_3)) + 
  standard_legend()+
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Grade development in the last year",
       subtitle = "Percentage of total organization at each comp grade level between 2023 and 2024",
       x = "Compensation grade levels")
  



## Data packs  ---- 

# Low spans
finance |>
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |>
  filter(direct_reports >0 & direct_reports < 4) |> 
  select(employee_id, employee, compensation_grade, direct_reports) |> 
  arrange(desc(compensation_grade), desc(direct_reports)) |> 
  WriteXLS()

# Compression
finance_full |> 
  filter(date == "2024-04-22",
         comp_grade_overlap == comp_grade_overlap_mgr) |>
  select(supervisory_organization_level_3, employee_id, employee, compensation_grade, manager_id, employee_mgr, compensation_grade_mgr) |> 
  arrange(desc(supervisory_organization_level_3), desc(compensation_grade))
  

  


data_orig |> 
  distinct(compensation_grade) |> 
  arrange(compensation_grade) |>  view()




## zOther  ---- 
 

data_orig |> distinct(supervisory_organization_level_2)


finance |> 
  tabyl(comp_grade_simple, date)

finance |> tabyl(date)

# Grade percentages

finance |> 
  group_by(date, comp_grade_simple) |> 
  summarise(n = n()) |> 
  drop_na(comp_grade_simple) |> 
  mutate(perc = percent(n/sum(n), digits = 1)) |> 
  pivot_wider(date, names_from = comp_grade_simple, values_from = c("n", "perc"))


# Project managers

finance_full |> filter(str_detect(job_title, "(P|p)ro(g|j)")) |> view()
no stores
business parters
countries and currencies
number of stores


# Span checks

finance |>
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |> 
  mutate(is_manager_b = if_else(direct_reports >0, "yes", NA)) |> 
  filter(is_manager == "Yes" & is.na(is_manager_b)) |> view()

finance |> tabyl(is_manager)


finance |> 
  filter(date == "2024-04-22") |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |> 
  mutate(is_manager_b = if_else(direct_reports >0, "yes", NA)) |> 
  tabyl(is_manager_b)
