library(pacman)
p_load(tidyverse, janitor, glue, datapasta, formattable, extrafont, WriteXLS, explore, readxl, lubridate)

source("scripts/functions/helpers.r")

## Load data  ---- 

# data_orig <- readxl::read_xlsx("data_in/230206.xlsx", skip = 3) |> 
#   clean_names() |>
#   mutate(date = "2023-02-06") |> 
#   bind_rows(readxl::read_xlsx("data_in/240422.xlsx", skip = 3) |> 
#               clean_names() |> 
#               mutate(date = "2024-04-22"))
# 
# new_data <- readxl::read_xlsx("data_in/240503.xlsx") |> 
#   clean_names() |> 
#   mutate(date = "2024-05-03")
# 
# janitor::compare_df_cols(data_orig, new_data)


data_orig <- readxl::read_xlsx("data_in/3ydata.xlsx", sheet = 2, .name_repair = make_clean_names)

data_orig |> glimpse()

data_orig |> 
  tabyl()

## Clean and prepare  ---- 

grade_levels <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "M1", "M2", "M3", "M4", "M5" ,"E1", "E2", "E3", "E4")

grade_levels_overlap <- c("B1", "B2", "B3-C1", "B4-C2", "C3-M1", "C4-M2", "C5-M3", "C6-M4", "C7-M5", "E1", "E2", "E3", "E4")

data_ready <- data_orig |>
  mutate(supervisory_organization_level_2 = if_else(supervisory_organization_level_2 == "Supply Chain, Dist, Sourcing (Ted Dagnese)", 
                                                    "Supply Chain (Ted Dagnese)",
                                                    supervisory_organization_level_2),
         compensation_grade = str_remove_all(compensation_grade, "L|N|P|S|T|I")) |> 
  filter(currently_active == "Yes",
         leave_on_leave == "No") |> 
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
                                        .default = compensation_grade), levels = grade_levels_overlap),
         report_effective_date = paste0(month(report_effective_date, label = TRUE), " ", year(report_effective_date)))

data_ready |> tabyl(report_effective_date)


managers <- data_ready |> 
  select(report_effective_date, position_id_worker, position_id_manager, job_title, compensation_grade, comp_grade_overlap, worker_name)

direct_reports <- data_ready |> 
  count(report_effective_date, position_id_manager) |> 
  rename(direct_reports = n)

data_full <- data_ready |> 
  left_join(managers, by = c("report_effective_date", "position_id_manager" = "position_id_worker"), suffix = c("", "_mgr")) |> 
  left_join(direct_reports, by = c("report_effective_date", "position_id_manager"))


c(
  "Americas and Global Guest Innovation (Celeste Burgoyne)",
  "CFO (Meghan Frank)",
  "Design and Merchandising (Sun Choe)",
  "Technology (Julie Averill)",
  "International (Andre Maestrini)",
  "People & Culture (Susan Gelinas)",
  "Supply Chain (Ted Dagnese)",
  "Legal (Shannon Higginson)",
  "Brand & Creative Content (Nikki Neuburger)"
  )


focus <- "People & Culture (Susan Gelinas)"

data_focus <- data_full |> 
  filter(supervisory_organization_level_2 == focus)

## 1 Manager Percentages  ---- 

data_focus |>
  group_by(report_effective_date, supervisory_organization_level_3, is_manager) |>
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> 
  pivot_wider(id_cols = c(report_effective_date, supervisory_organization_level_3), names_from = is_manager, values_from = n) |> 
  summarize(perc = percent(Yes/(Yes+No), digits = 0))|> 
  drop_na(supervisory_organization_level_3) |>
  filter(perc > 0) |> 
  arrange(desc(perc)) |> 
  mutate(high_ok_low = case_when(perc >= 0.25 ~ "high",
                                 perc <= 0.15 ~ "low",
                                 .default = "ok")) |> 
  ggplot(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_3)) +
  geom_line() +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 0.15, ymax = .25), fill = neutral_5) +
  facet_wrap(~ supervisory_organization_level_3) +
  geom_hline(yintercept = 0.15, color = offblack, linetype = "dashed") +
  geom_hline(yintercept = 0.25, color = offblack, linetype = "dashed") +
  geom_label(aes(label = perc, fill = high_ok_low, color = high_ok_low), family = lulu_font, fontface = "bold") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Line managers as a percentage of total team headcount",
       subtitle = "Development over time",
       x = "Date",
       y = "Manager perentage") +
  scale_fill_manual(values = c("high" = hotheat,
                               "ok" = offwhite,
                               "low" = blue)) +
  scale_color_manual(values = c("high" = offwhite,
                                "ok" = offblack,
                                "low" = offblack))


## 2 Average span of control  ---- 

data_focus |>
  group_by(report_effective_date, supervisory_organization_level_3) |> 
  summarise(average_span = round(mean(direct_reports, na.rm = TRUE),1.1)) |> 
  arrange(desc(average_span)) |>
  drop_na(supervisory_organization_level_3, average_span) |> 
  mutate(low_ok = if_else(average_span >= 5 & average_span <= 8, "ok", "low")) |> 
  ggplot(aes(x = report_effective_date, y = average_span, group = supervisory_organization_level_3)) +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 5, ymax = 8), fill = neutral_5) +
  geom_line() +
  facet_wrap(~ supervisory_organization_level_3) +
  geom_hline(yintercept = 5, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 8, color = neutral_3, linetype = "dashed") +
  geom_label(aes(label = average_span, fill = low_ok, color = low_ok), family = lulu_font, fontface = "bold") +
  labs(title = "Average span of control over time",
       subtitle = "Excluding L3 leader",
       x = "Date",
       y = "Average span of control") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE) +
  scale_fill_manual(values = c("ok" = offwhite,
                               "low" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "low" = offwhite))
 

## 3 Compression  ---- 


data_focus |> 
  filter(comp_grade_overlap == comp_grade_overlap_mgr) |> 
  count(report_effective_date, supervisory_organization_level_3) |> 
  left_join(data_focus |> 
              count(report_effective_date, supervisory_organization_level_3),
            by = c("report_effective_date", "supervisory_organization_level_3")) |> 
  group_by(report_effective_date, supervisory_organization_level_3) |> 
  reframe(perc = percent(n.x/n.y, digits = 1)) |> 
  ggplot(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_3)) +
  geom_line() +
  facet_wrap(~supervisory_organization_level_3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_label(aes(label = perc), family = lulu_font, colour = offblack, fontface = "bold") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  # standard_text_y(bold = FALSE) +
  labs(title = "Grade compression as a percentage of all roles over time",
       subtitle = "Employees reporting to a manager at the same contribution level")


## 4 Executive development  ---- 

data_focus |> 
  mutate(exec = if_else(str_detect(compensation_grade, "E"), "exec", "non")) |> 
  group_by(report_effective_date, supervisory_organization_level_2, exec) |> 
  summarise(n = n()) |>
  drop_na(exec) |> 
  pivot_wider(id_cols = c("report_effective_date", "supervisory_organization_level_2"), names_from = exec, values_from = n) |> 
  summarise(exec_perc = percent(exec/(exec+non), digits = 1)) |>
  mutate(vsbm = if_else(exec_perc > 0.02, "high", "ok")) |> 
  ggplot(aes(x = report_effective_date, y = exec_perc, group = supervisory_organization_level_2)) +
  geom_hline(aes(yintercept = 0.02), color = offblack, linetype = "dashed") +
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0, ymax = .02), fill = neutral_5) +
  geom_line(color = offblack, size = 1) +
  geom_label(aes(label = exec_perc, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
  labs(title = "Executive roles percentage over time",
       subtitle = "VP+ roles within supervisory organization") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05)) +

  scale_fill_manual(values = c("ok" = offwhite,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))


## 5 Project Managers  ----

data_focus |>
  mutate(pjm = if_else(str_detect(job_title, "(P|p)ro(g|j)"), "Yes", "No")) |> 
  count(report_effective_date, supervisory_organization_level_2, pjm) |> 
  pivot_wider(id_cols = 1:2, names_from = pjm, values_from = n) |> 
  group_by(report_effective_date, supervisory_organization_level_2) |> 
  summarize(perc = percent(Yes/(Yes+No), digits = 1)) |> 
  mutate(vsbm = if_else(perc > 0.01, "high", "ok")) |>
  ggplot(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_2)) +
  geom_hline(aes(yintercept = 0.02), color = offblack, linetype = "dashed") +
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0, ymax = .02), fill = neutral_5) +
  geom_line(color = offblack, size = 1) +
  geom_label(aes(label = perc, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
  labs(title = "Project & program management roles percentage over time",
       subtitle = "") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("ok" = offwhite,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))


## Vix 6 Grade development  ---- 




finance |> 
  filter(supervisory_organization_level_2 == target) |> 
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


slt_areas <- c("Americas and Global Guest Innovation (Celeste Burgoyne)", "CFO (Meghan Frank)", "People & Culture (Susan Gelinas)", "Design and Merchandising (Sun Choe)", "Supply Chain (Ted Dagnese)", "Brand & Creative Content (Nikki Neuburger)", "Technology (Julie Averill)", "International (Andre Maestrini)", "Legal (Shannon Higginson)")

# Low spans

for (i in slt_areas) {

finance |>
  filter(date == "2024-04-22",
         supervisory_organization_level_2 == i) |>
  left_join(finance |>
              filter(date == "2024-04-22") |> 
              count(manager_id) |> 
              rename(direct_reports = n),
            by = c("employee_id" = "manager_id")) |>
  filter(direct_reports >0 & direct_reports < 4) |> 
  select(supervisory_organization_level_3, employee_id, employee, compensation_grade, direct_reports) |> 
  arrange(supervisory_organization_level_3, desc(compensation_grade), desc(direct_reports)) |> 
  write_excel_csv(glue("data_out/low_spans_{i}.csv"))

}
  
# Compression

for (i in slt_areas) {

finance_full |> 
  filter(date == "2024-04-22",
         supervisory_organization_level_2 == i,
         comp_grade_overlap == comp_grade_overlap_mgr) |>
  select(supervisory_organization_level_3, employee_id, employee, compensation_grade, manager_id, employee_mgr, compensation_grade_mgr) |> 
  arrange(desc(supervisory_organization_level_3), desc(compensation_grade)) |> 
    write_excel_csv(glue("data_out/compression_{i}.csv"))
  

}


# Project managers

for (i in slt_areas) {
  
  finance_full |> 
    filter(date == "2024-04-22",
           supervisory_organization_level_2 == i,
           str_detect(job_title, "(P|p)ro(g|j)")) |>
    select(supervisory_organization_level_3, employee_id, employee, job_title) |> 
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |> 
    write_excel_csv(glue("data_out/project_managers_{i}.csv"))
  
  
}



## zOther  ---- 
 

data_orig |> distinct(supervisory_organization_level_2)


data_orig |> 
  tabyl(compensation_grade, date)

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
