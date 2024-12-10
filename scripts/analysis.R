library(pacman)
p_load(tidyverse, janitor, glue, datapasta, formattable, showtext, WriteXLS, explore, readxl, lubridate, tidygraph, ggraph, shadowtext, gghighlight)

source("scripts/functions/helpers.r")



## Load data  ---- 

# data_orig <- readxl::read_xlsx("data_in/3ydata.xlsx", sheet = 2, .name_repair = make_clean_names) # read in the orgiinal excel file and standardize the variable names (snake case)

data_orig <- readxl::read_xlsx("data_in/quarterly_data_original_w_country.xlsx", sheet = 2, .name_repair = make_clean_names) 

# extract_date <-  "2024-05-03"

## Clean & prepare data ----

# Still need to figure out a simple way of determining depth


data_clean <- data_orig |> 
  mutate(report_effective_date = lubridate::as_date(report_effective_date),
         readable_date = format(report_effective_date, "%B %Y"),
         position_id_manager = if_else(position_id_worker == "83-093759", NA_character_, position_id_manager),
         compensation_grade = str_remove_all(compensation_grade, "L|N|P|S|T|I"),
         compensation_grade = factor(compensation_grade, levels = grade_levels),
         compensation_grade_name = case_when(compensation_grade == "M2" ~ "Manager",
                                             compensation_grade == "M3" ~ "Senior Manager",
                                             compensation_grade == "M4" ~ "Director",
                                             compensation_grade == "M5" ~ "Senior Director",
                                             compensation_grade == "E1" ~ "VP",
                                             compensation_grade == "E2" ~ "SVP",
                                             compensation_grade == "E3" ~ "EVP",
                                             compensation_grade == "E4" ~ "SLT",
                                             .default = "Individual Contributor")) |>
  left_join(grade_scoring, by = "compensation_grade") |> 
  filter(leave_on_leave == "No")


oth_clean <- data_clean |> 
  count(report_effective_date, position_id_workday) |> 
  filter(n>1) |> 
  select(-n) |> 
  inner_join(data_clean, by = c("report_effective_date", "position_id_workday")) |> 
  filter(str_detect(position_id_worker, "OTH"))

data_clean_oth <- data_clean |> 
  anti_join(data_clean |> 
              count(report_effective_date, position_id_workday) |> 
              filter(n>1) |> 
              select(-n), by =  c("report_effective_date", "position_id_workday")) |> 
  bind_rows(oth_clean) |> 
  mutate(position_id_worker = if_else(str_detect(position_id_worker, "OTH"), position_id_workday, position_id_worker))


data_clean_drs <- data_clean_oth |> 
  left_join(data_clean |> 
              count(report_effective_date, position_id_manager), by = c("report_effective_date", "position_id_worker" = "position_id_manager")) |> 
  rename(direct_reports = n) 
  
data_clean_managers <- data_clean_drs |>
  left_join(data_clean |> 
              select(report_effective_date, readable_date, position_id_worker, position_id_manager, job_title, worker_name, compensation_grade, grade_score), 
            by = c("report_effective_date", "position_id_manager" = "position_id_worker"), suffix = c("_wkr", "_mgr")) |> 
  mutate(same_grade_reports = if_else(grade_score_wkr == grade_score_mgr, "Yes", NA))


alerts <- data_clean_managers |> 
  # filter(compensation_grade_mgr %in% c("M4", "M5","E1", "E2", "E3", "E4")) |> 
  select(report_effective_date, position_id_worker, compensation_grade_wkr, grade_score_wkr, position_id_manager, compensation_grade_mgr, grade_score_mgr) |> 
  mutate(gap_raw = grade_score_mgr - grade_score_wkr,
         gap_comment = case_when(gap_raw == 0 ~ "Compressed",
                                 gap_raw == 1 ~ "Successor",
                                 gap_raw == 2 | gap_raw == 3 ~ "ok",
                                 gap_raw >3 ~ "Gap")) |> 
  arrange(position_id_manager) |>
  count(report_effective_date, position_id_manager, gap_comment) |>
  group_by(report_effective_date, position_id_manager) |> 
  mutate(percentage = percent(n/sum(n), digits = 0)) |>
  select(-n) |> 
  pivot_wider(id_cols = c(report_effective_date, position_id_manager), names_from = gap_comment, values_from = percentage) |>
  replace_na(list(ok = percent(0, digits = 0),
                  Gap = percent(0, digits = 0),
                  Successor = percent(0, digits = 0),
                  Compressed = percent(0, digits = 0))) |>
  mutate(alert_successors = if_else(Successor == percent(0, digits = 0), "No successor", NA),
         alert_compression  = if_else(Successor >= percent(0.33, digits = 0) | Compressed > percent(0, digits = 0), "Team compression", NA),
         alert_gaps = if_else(Gap >= percent(0.25, digits = 0), "Team gaps", NA))


data_clean_alerts <- data_clean_managers |> 
  left_join(alerts |> 
              select(report_effective_date, position_id_manager, alert_successors:alert_gaps),
            by = c("report_effective_date", "position_id_worker" = "position_id_manager")) 


dates_formatted <- data_clean_alerts |> 
  distinct(report_effective_date) |> 
  mutate(readable = format(report_effective_date, "%b %Y")) |> 
  arrange(report_effective_date) |> 
  pull(readable)

## Data fixes  ----

data_clean_alerts_fixed <- data_clean_alerts |> 
  mutate(supervisory_organization_level_2 = case_when(supervisory_organization_level_2 == "Supply Chain, Distribution, Sourcing (Ted Dagnese)" ~ "Supply Chain (Ted Dagnese)",
                                                      supervisory_organization_level_2 == "Design and Merchandising (Sun Choe)" ~ "Creative - Design and Concept (Jonathan Cheung)",
                                                      .default = supervisory_organization_level_2),
         
         supervisory_organization_level_3 = case_when(supervisory_organization_level_3 == "Raw Material Developments - Advanced Materials Innovation (Yogendra Dandapure)" ~ "Raw Materials Innovation (Yogendra Dandapure)",
                                                      supervisory_organization_level_3 == "Quality Assurance, Strategy, Testing & Compliance (Rene Wickham)" ~ "Product Integrity (Rene Wickham)",
                                                      supervisory_organization_level_3 == "Quality (Rene Wickham)" ~ "Product Integrity (Rene Wickham)",
                                                      supervisory_organization_level_3 == "Raw Material Developments (Patty Stapp)" ~ "Global Raw Materials (Patty Stapp)",
                                                      supervisory_organization_level_3 == "Supply Chain Strategy & Planning (Ravi Chowdary)" ~ "Supply Chain Strategy & Planning (Andrew Polins)",
                                                      
                                                      
                                                      supervisory_organization_level_3 == "Talent Acquisition (Steve Clyne)" ~ "Talent Acquisition (Marc Wendorf)",
                                                      supervisory_organization_level_3 == "People & Culture Operations (Susan Gelinas (Inherited))" ~ "People & Culture Operations (Mandy Whiting)",
                                                      str_detect(supervisory_organization_level_3, "Business Partnering - International") ~ "Business Partnering – Global Corporate Functions (Jacqueline Misshula)",
                                                      str_detect(supervisory_organization_level_3, "Business Partnering - SSC") ~ "Business Partnering - Product & Supply Chain (Stewart Angus)",
                                                      
                                                      
                                                      supervisory_organization_level_3 == "Ecommerce (Justin Richmond)" ~ "Ecommerce (Danny Ryder)",
                                                      supervisory_organization_level_3 == "Member Engagement (Madeline Thompson)" ~ "Member Engagement (Jiamei Bai)",
                                                      supervisory_organization_level_3 == "Member Engagement (TJ Whitmell)" ~ "Member Engagement (Jiamei Bai)",
                                                      str_detect(supervisory_organization_level_3, "MIRROR & Digital Fitness") ~ "MIRROR & Digital Fitness",
                                                      
                                                      str_detect(supervisory_organization_level_3, "Financial Planning & Analysis") ~ "Financial Planning & Analysis",
                                                      str_detect(supervisory_organization_level_3, "Strategic Finance") ~ "Financial Planning & Analysis",
                                                      str_detect(supervisory_organization_level_3, "Financial Reporting Accounting") ~ "Finance, Accounting and Tax (Alex Grieve)",
                                                      
                                                      str_detect(supervisory_organization_level_3, "Integrated GTM") ~ "Business Transformation (Ana Badell)",
                                                      str_detect(supervisory_organization_level_3, "Operations Excellence") ~ "Business Transformation (Ana Badell)",
                                                      
                                                      supervisory_organization_level_3 == "Strategy and Ops (Andrea Heckbert, Christine Turner (On Leave))" ~ "Strategy and Ops (Christine Turner)",
                                                      
                                                      supervisory_organization_level_3 == "Technology Security (Alexander Padilla)" ~ "Cyber Defense & Incident Response (Brian Seaford)",
                                                      supervisory_organization_level_3 == "Technology Security (Brian Seaford)" ~ "Cyber Defense & Incident Response (Brian Seaford)",
                                                  
                                                      supervisory_organization_level_3 == "Tech Priorities (Diane Cañate)" ~ "Strategy & Operations (Diane Cañate)",
                                                      supervisory_organization_level_3 == "Global Digital Technology (Jamie Turnbull)" ~ "Global Digital Technology (Jebin Zacharia)",
                                                      
                                                      supervisory_organization_level_3 == "Product & Distribution Technology (Deb Huntting)" ~ "Global Technology Services (Venki Krishnababu)",
                                                      supervisory_organization_level_3 == "Technology Enterprise Planning and Operations (Justin Walton)" ~ "Global Technology Services (Venki Krishnababu)",
                                                     
                                                      
                                                      .default = supervisory_organization_level_3),
         
         supervisory_organization_level_4 = case_when(supervisory_organization_level_4 == "Global Workplace Design (Greg Smith (Inherited))" ~ "Global Workplace Design (Carol Waldmann)",
                                                      supervisory_organization_level_4 == "Facility Implementation (Shaleena Uppal)" ~ "Global Workplace Design (Carol Waldmann)",
                                                      supervisory_organization_level_4 == "Facility Implementation (Greg Smith (Inherited))" ~ "Global Workplace Design (Carol Waldmann)",
                                                      .default = supervisory_organization_level_4
         ))






## Prepare data for analysis ----

data_clean_alerts_fixed |> glimpse()

data_clean_alerts_fixed |> 
  tabyl(report_effective_date)

data_full <- data_clean_alerts_fixed |> 
  filter(report_effective_date > "2023-05-05")

data_focus <- data_clean_alerts_fixed 

title_level <- "supervisory_organization_level_2"
so_level <- "supervisory_organization_level_3"

(title_name <- data_focus |> 
  distinct(.data[[title_level]]) |> 
  pull(.data[[title_level]]))



## EXPERIMENTS ----  

manager_id <- data_clean_alerts_fixed |> 
  filter(str_detect(worker_name_wkr, "Borsoi")) |> 
  pull(position_id_worker)


(subordinates <- find_all_subordinates(manager_id, data_clean_alerts_fixed))

datapasta::vector_paste(subordinates)c("83-131964", "83-089245", "83-110204", "83-077266", "83-153444", "83-225395", "83-166735", "83-153443", "83-186937", "83-109632", "83-151737", "00-0002", "83-153962")


c("83-167854", "83-133921", "83-224957", "83-225420", "83-225048", "83-225419", "83-161700", "83-065853", "83-152141", "83-152157", "83-112902", "83-224869", "83-110204", "83-153444", "83-166735", "83-130955")


data_full |>
  mutate(job_title_wkr = tolower(job_title_wkr)) |> 
  filter(report_effective_date == max(data_full$report_effective_date),
         str_detect(job_title_wkr, regex_data)) |> 
  count(supervisory_organization_level_4) |> 
  mutate(total = sum(n)) |> view()
  

  data_full |> 
    filter(report_effective_date == max(data_full$report_effective_date),
           supervisory_organization_level_4 == "People Analytics (Kami Tilmann)") |> view()



regex_data <- "data|insight|\b(?<!business\\s)analy|scient|report"


data_full |> 
  filter(report_effective_date == max(data_full$report_effective_date)) |> 
  count(supervisory_organization_level_2, supervisory_organization_level_3, name = "employees") |> 
  group_by(supervisory_organization_level_2) |> 
  arrange(desc(employees), .by_group = TRUE) |>
  left_join(data_full |> 
              filter(report_effective_date == max(data_full$report_effective_date),
                     compensation_grade_wkr %in% c("M5", "E1", "E2", "E3", "E4")) |> 
              count(supervisory_organization_level_2, supervisory_organization_level_3, name = "execs"),
            by = c("supervisory_organization_level_2", "supervisory_organization_level_3")) |>
  drop_na(supervisory_organization_level_2, supervisory_organization_level_3) |> 
  filter(employees < 50) |> 
  view()
  
  


data_full |> 
  filter(report_effective_date == max(data_full$report_effective_date)) |> 
  count(supervisory_organization_level_2, supervisory_organization_level_3, country, name = "employees") |> 
  group_by(supervisory_organization_level_2, supervisory_organization_level_3) |> 
  summarise(countries = sum(length(country))) |> 
  group_by(supervisory_organization_level_2) |> 
  arrange(desc(countries), .by_group = TRUE) |> 
  drop_na(supervisory_organization_level_2, supervisory_organization_level_3) |> view()
  filter(countries > 5) |> 
  view()







calculate_depth <- function(employee_id, employees_df) {
  depth <- 0
  visited <- c()
  current_id <- employee_id
  
  while (!is.na(current_id) && !(current_id %in% visited)) {
    depth <- depth + 1
    visited <- c(visited, current_id)
    current_id <- employees_df$position_id_manager[employees_df$position_id_worker == current_id]
    if (length(current_id) == 0) break
    current_id <- current_id[1]  # In case of multiple matches, take the first one
  }
  
  # if (current_id %in% visited) {
  #   warning("Circular reference detected for employee ", employee_id)
  #   return(NA)
  # }
  
  return(depth)
}

data_clean_alerts_fixed |> 
  filter(report_effective_date == max(data_clean_alerts_fixed$report_effective_date)) |> 
  rowwise() |> 
  mutate(depth = calculate_depth(position_id_worker, data_clean_alerts_fixed)) |> view()


## 1 Manager Percentages  ---- 


data_focus |>
  group_by(report_effective_date, .data[[so_level]], is_manager) |> #group and count number of non-/managers by sup org and date
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> # fill in blank/NA 
  pivot_wider(id_cols = c(report_effective_date, .data[[so_level]]), names_from = is_manager, values_from = n) |> # reshape
  mutate(Yes = Yes+1) |> # adjustment to count the manager of the sup org in the numbers
  summarize(perc = percent(Yes/(Yes+No), digits = 0))|> # percetages of non-/managers
  drop_na(.data[[so_level]]) |>
  filter(perc > 0) |> # filter non-zero percentages
  
  left_join(data_full |> # calculate lululemon overall percentages
              group_by(report_effective_date, is_manager) |> 
              summarise(n = n()) |> 
              mutate(is_manager = replace_na(is_manager, "No")) |> 
              pivot_wider(id_cols = c(report_effective_date), names_from = is_manager, values_from = n) |> 
              summarize(ll_perc = percent(Yes/(Yes+No), digits = 0)),
            by = c("report_effective_date")) |> 
  
  right_join(data_focus |> # next few lines remove records for historical sup orgs that no longer exist in 2024
               distinct(report_effective_date, .data[[so_level]]) |> 
               expand(report_effective_date, .data[[so_level]])) |>
  ungroup() |> 
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(perc), .data[[so_level]], NA)) |>
  filter(!.data[[so_level]] %in% trim) |> 
  mutate(high_ok_low = case_when(perc >= 0.25 ~ "high", # categorization of percentage levels against best practice
                                 perc <= 0.15 ~ "low",
                                 .default = "ok"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = length(unique(data_focus$report_effective_date))+1, ymin = 0.15, ymax = .25), fill = neutral_1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = .data[[so_level]]), color = offblack, size = 0.5, linetype = "dashed") +
  geom_line(aes(x = report_effective_date, y = perc, group = .data[[so_level]]), color = neutral_3, size = 1) +
  facet_wrap(~ .data[[so_level]]) +
  geom_hline(yintercept = 0.15, color = neutral_3, linetype = "dotted") +
  geom_hline(yintercept = 0.25, color = neutral_3, linetype = "dotted") +
  geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = high_ok_low, color = high_ok_low), family = lulu_font, fontface = "bold") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Percentage of line managers - {title_name}"),
       subtitle = "Line managers as a percentage of roles over time vs lululemon average and external benchmark (15-25%)",
       x = "Date",
       y = "Manager perentage",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("high" = hotheat,
                               "ok" = pale_green,
                               "low" = blue)) +
  scale_color_manual(values = c("high" = offwhite,
                                "ok" = offblack,
                                "low" = offblack))

###### 1.1 Manager percentages ALL  ----

data_full |>
  group_by(report_effective_date, supervisory_organization_level_2, is_manager) |> #group and count number of non-/managers by sup org and date
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> # fill in blank/NA 
  pivot_wider(id_cols = c(report_effective_date, supervisory_organization_level_2), names_from = is_manager, values_from = n) |> # reshape
  mutate(Yes = Yes+1) |> # adjustment to count the manager of the sup org in the numbers
  summarize(perc = percent(Yes/(Yes+No), digits = 0))|> # percetages of non-/managers
  drop_na(supervisory_organization_level_2) |>
  filter(perc > 0) |> # filter non-zero percentages
  
  bind_rows(data_full |> # calculate lululemon overall percentages
              group_by(report_effective_date, is_manager) |> 
              summarise(n = n()) |> 
              mutate(is_manager = replace_na(is_manager, "No")) |> 
              pivot_wider(id_cols = c(report_effective_date), names_from = is_manager, values_from = n) |> 
              summarize(perc = percent(Yes/(Yes+No), digits = 0)) |> 
              mutate(supervisory_organization_level_2 = "lululemon average")) |> 
  
  
  
  mutate(high_ok_low = case_when(perc >= 0.25 ~ "high", # categorization of percentage levels against best practice
                                 perc <= 0.15 ~ "low",
                                 .default = "ok"),
         label = if_else(report_effective_date == max(data_full$report_effective_date) & supervisory_organization_level_2 == "lululemon average", perc, NA),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_2)) +
  # annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 0.15, ymax = .25, fill = neutral_1) +
  geom_line(size = 1, color = hotheat) +
  gghighlight(supervisory_organization_level_2 == "lululemon average", line_label_type = "ggrepel_text", 
              label_params = list(color = offwhite),
              unhighlighted_params = list(colour = neutral_1)) +
  geom_hline(yintercept = 0.15, color = neutral_3, linetype = "dotted") +
  geom_hline(yintercept = 0.25, color = neutral_3, linetype = "dotted") +
  annotate("text", x = 1, y = 0.255, label = "Upper benchmark range", family = lulu_font, colour = neutral_2) +
  annotate("text", x = 1, y = 0.155, label = "Lower benchmark range", family = lulu_font, color = neutral_2) +
  
  geom_shadowtext(aes(x = report_effective_date, y = perc, label = label), family = lulu_font, fontface = "bold", size = 5) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Percentage of line managers"),
       subtitle = "Line managers as a percentage of roles over time vs external benchmark (15-25%)",
       x = "Date",
       y = "Manager perentage",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("high" = hotheat,
                               "ok" = pale_green,
                               "low" = blue)) +
  scale_color_manual(values = c("high" = offwhite,
                                "ok" = offblack,
                                "low" = offblack))


## 2 Average span of control  ---- 



data_focus |>
  group_by(report_effective_date, .data[[so_level]]) |> # calculate average span of control
  summarise(average_span = round(mean(direct_reports, na.rm = TRUE),1.1)) |> 
  drop_na(.data[[so_level]]) |> 
  
  right_join(data_focus |>  # next few lines remove records for historical sup orgs that no longer exist in 2024
               distinct(report_effective_date, .data[[so_level]]) |>
               expand(report_effective_date, .data[[so_level]])) |>
  ungroup() |>
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(average_span), .data[[so_level]], NA)) |>
  filter(!.data[[so_level]] %in% trim) |>
  
  drop_na(.data[[so_level]], average_span) |> 
    
  left_join(data_full |> 
                group_by(report_effective_date) |> 
                summarise(average_span_ll = round(mean(direct_reports, na.rm = TRUE),1.1)),
            by = "report_effective_date"
            ) |> 
  
  
  mutate(low_ok = if_else(average_span >= 5 & average_span <= 8, "ok", "low"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> # categorize span ranges for plots
  ggplot(aes(x = report_effective_date)) +
  geom_rect(aes(xmin = 0, xmax = length(unique(data_focus$report_effective_date))+1, ymin = 5, ymax = 8), fill = neutral_1) +
  geom_line(aes(y = average_span, group = .data[[so_level]])) +
  geom_line(aes(y = average_span_ll, group = .data[[so_level]]), color = neutral_3, size = 0.5, linetype = "dashed") +
  facet_wrap(~ .data[[so_level]]) +
  geom_hline(yintercept = 5, color = neutral_3, linetype = "dotted") +
  geom_hline(yintercept = 8, color = neutral_3, linetype = "dotted") +
  
  geom_label(aes(y = average_span_ll, label = average_span_ll), fill = offwhite, color = neutral_3, family = lulu_font, fontface = "bold", label.size = 0) +
  geom_label(aes(y = average_span, label = average_span, fill = low_ok, color = low_ok), family = lulu_font, fontface = "bold") +
  labs(title = glue("Average span of control - {title_name}"),
       subtitle = "Average number of direct reports over time vs lululemon average (dotted line) and best practice guidelines (5 to 8)",
       x = "Date",
       y = "Average span of control") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_fill_manual(values = c("ok" = pale_green,
                               "low" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "low" = offwhite))
 


## 3 Executive development - L2 only ---- 

data_full |>
  filter(str_detect(supervisory_organization_level_2, "MIRROR|CEO", negate = TRUE),
         leave_on_leave == "No") |> 
  mutate(exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")) |> # classifiy exec roles
  group_by(report_effective_date, supervisory_organization_level_2, exec) |> # group and count exec roles
  summarise(n = n()) |>
  drop_na(exec) |> 
  pivot_wider(id_cols = c("report_effective_date", "supervisory_organization_level_2"), names_from = exec, values_from = n) |> 
  summarise(exec_perc = percent(exec/(exec+non), digits = 1)) |> # percentages of exec roles
  
  left_join(data_full |> # lululemon averages
              mutate(exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")) |> 
              group_by(report_effective_date, exec) |> 
              summarise(n = n()) |> 
              drop_na(exec) |> 
              pivot_wider(id_cols = c("report_effective_date"), names_from = exec, values_from = n) |> 
              summarise(ll_exec_perc = percent(exec/(exec+non), digits = 1)), by = c("report_effective_date")
            ) |> 
  
  mutate(vsbm = if_else(exec_perc > 0.02, "high", "ok"),,
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted),
         label = if_else(report_effective_date == "Nov 2024", exec_perc, NA),
         label_ll = if_else(report_effective_date == "Nov 2024", ll_exec_perc, NA)) |># classification of ranges for plot
  ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = length(unique(data_focus$report_effective_date))+0.5, ymin = 0, ymax = .02), fill = neutral_1) +
  geom_hline(aes(yintercept = 0.02), color = neutral_4, linetype = "dotted") +
  geom_line(aes(x = report_effective_date, y = exec_perc, group = supervisory_organization_level_2), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_exec_perc, group = supervisory_organization_level_2), color = neutral_3, size = 0.5, linetype = "dashed") +
  geom_label(aes(x = report_effective_date, y = ll_exec_perc, label = label_ll), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = exec_perc, label = label, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
  labs(title = glue("Executive roles percentage"),
       subtitle = "Percentage of VP+ roles vs lululemon average (dotted line) and external benchmark (<=2%)",
       y = "Percentage of VP+ roles",
       x = "Date (quarters)",
       caption = "Excludes leaves and non-SSC workforce.") +
  theme_clean_lulu() +
  facet_wrap(~supervisory_organization_level_2) +
  
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07)) +

  scale_fill_manual(values = c("ok" = pale_green,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))


### 3.1 Executive development (all lululemon)  ----

data_full |> 
  mutate(exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")) |> # classifiy exec roles
  group_by(report_effective_date, supervisory_organization_level_2, exec) |> # group and count exec roles
  summarise(n = n()) |>
  drop_na(exec) |> 
  pivot_wider(id_cols = c("report_effective_date", "supervisory_organization_level_2"), names_from = exec, values_from = n) |> 
  summarise(exec_perc = percent(exec/(exec+non), digits = 1)) |>
  bind_rows(data_full |> 
              mutate(exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")) |> # classifiy exec roles
              group_by(report_effective_date,  exec) |> # group and count exec roles
              summarise(n = n()) |>
              drop_na(exec) |> 
              pivot_wider(id_cols = c("report_effective_date"), names_from = exec, values_from = n) |> 
              summarise(exec_perc = percent(exec/(exec+non), digits = 1)) |> 
              mutate(supervisory_organization_level_2 = "lululemon average")) |>
  drop_na(supervisory_organization_level_2) |> 
  filter(str_detect(supervisory_organization_level_2, "CEO", negate = TRUE)) |> 
  mutate(label = if_else(supervisory_organization_level_2 == "lululemon average" & report_effective_date == max(data_full$report_effective_date), exec_perc, NA),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |>  # classification of ranges for plot
  ggplot(aes(x = report_effective_date, y = exec_perc, group = supervisory_organization_level_2)) +
  
  # geom_rect(aes(xmin = 0.5, xmax = length(unique(data_full$report_effective_date))+0.5, ymin = 0, ymax = .02), fill = neutral_1) +
  
  geom_hline(aes(yintercept = 0.02), color = neutral_4, linetype = "dotted") +

  geom_line(color = hotheat, size = 1) +
  gghighlight(supervisory_organization_level_2 == "lululemon average", line_label_type = "ggrepel_text", 
              label_params = list(color = offwhite),
              unhighlighted_params = list(colour = neutral_1)) +
  geom_shadowtext(aes(x = report_effective_date, y = exec_perc, label = label), family = lulu_font, fontface = "bold") +

  labs(title = glue("Executive roles percentage"),
       subtitle = "Percentage of VP+ roles vs lululemon average (dotted line) and external benchmark (<=2%)",
       y = "Percentage of VP+ roles",
       x = "Date (quarters)",
       caption = "Excludes leaves and non-SSC workforce.") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.07)) +
  
  scale_fill_manual(values = c("ok" = pale_green,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))



## 4 Project Managers  ----


data_focus |> 
  mutate(pjm = if_else(str_detect(job_title, "(P|p)ro(g|j)"), "Yes", "No")) |> # classify project managers by job title
  count(report_effective_date, supervisory_organization_level_2, pjm) |> # group and count project managers
  pivot_wider(id_cols = 1:2, names_from = pjm, values_from = n) |>
  group_by(report_effective_date, supervisory_organization_level_2) |>
  summarize(perc = percent(Yes/(Yes+No), digits = 1)) |> # percentages
  left_join(data_full |>  # lululemon averages
              mutate(pjm = if_else(str_detect(job_title, "(P|p)ro(g|j)"), "Yes", "No")) |>
              count(report_effective_date, pjm) |>
              pivot_wider(id_cols = 1, names_from = pjm, values_from = n) |>
              group_by(report_effective_date) |>
              summarize(ll_perc = percent(Yes/(Yes+No), digits = 1)), by = c("report_effective_date")) |>
  mutate(vsbm = if_else(perc > 0.01, "high", "ok")) |> # classsify ranges for plot
  ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0, ymax = .01), fill = neutral_5) +
  geom_hline(aes(yintercept = 0.01), color = neutral_4, linetype = "dashed") +

  geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_2), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_2), color = neutral_4, size = 0.5, linetype = "dashed") +

  geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
  geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  labs(title = glue("Project & Program management roles: {focus}"),
       subtitle = "Percentage of Project and Program Management roles over time vs lululemon average (dotted line) and external benchmark (<=1%)") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("ok" = lightgreen,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))


## 5 Grade development  ---- 

data_focus |> 
  count(report_effective_date, supervisory_organization_level_2, grade_score_wkr) |> # count records by contribution level, date and sup org
  drop_na(grade_score_wkr) |> 
  group_by(report_effective_date) |> 
  mutate(perc = percent(n/sum(n, na.rm = TRUE), digits = 1)) |> # percentages of each comp level by date
  
  # Add lululemon average
  left_join(data_full |> 
              count(report_effective_date, grade_score_wkr) |>
              drop_na(grade_score_wkr) |> 
              group_by(report_effective_date) |> 
              mutate(ll_perc = percent(n/sum(n, na.rm = TRUE), digits = 1)), by = c("report_effective_date", "grade_score_wkr")
              ) |>
  mutate(above_ave = if_else(perc > ll_perc, "above", "ok"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot() +
  geom_line(aes(x = report_effective_date, y = perc, group = grade_score_wkr), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = grade_score_wkr), color = neutral_3, size = 0.5, linetype = "dashed") +
  geom_label(aes(label = ll_perc, x = report_effective_date, y = ll_perc), fill = offwhite, color = neutral_3, label.size = 0, family = lulu_font) +
  geom_label(aes(label = perc, x = report_effective_date, y = perc, fill = above_ave, color = above_ave), fontface = "bold", family = lulu_font) +
  facet_wrap(~ grade_score_wkr) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  labs(title = glue("Grade development"),
       subtitle = "Percentage of roles at each grade level over time vs lululemon average",
       x = "Compensation grade levels") +
  scale_fill_manual(values = c("above" = hotheat,
                               "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite,
                                "ok" = offblack))

###### 5.1 Grade development all lululemon ----

data_full |>
  mutate(compensation_grade_name = case_when(compensation_grade_name %in% c("Manager", "Senior Manager") ~ "Senior/Manager",
                                compensation_grade_name %in% c("Director", "Senior Director") ~ "Senior/Director",
                                compensation_grade_name %in% c("VP", "SVP", "EVP", "SLT") ~   "Executive",
                                .default = "Individual Contributor"
                                
                                ),
         compensation_grade_name = factor(compensation_grade_name, levels = c("Executive","Senior/Director","Senior/Manager","Individual Contributor"))) |> 
  count(report_effective_date, compensation_grade_name) |>
  drop_na(compensation_grade_name) |> 
  group_by(report_effective_date) |> 
  mutate(perc = percent(n/sum(n, na.rm = TRUE), digits = 1)) |> 
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  filter(compensation_grade_name != "Executive") |> 
  
  ggplot() +
  geom_line(aes(x = report_effective_date, y = perc, group = compensation_grade_name), color = offblack, size = 1) +
  geom_shadowtext(aes(label = perc, x = report_effective_date, y = perc), fontface = "bold", family = lulu_font) +
  facet_wrap(~ compensation_grade_name, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  labs(title = glue("Grade development"),
       subtitle = "Percentage of roles at each grade level over time vs lululemon average",
       x = "Compensation grade levels") +
  scale_fill_manual(values = c("above" = hotheat,
                               "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite,
                                "ok" = offblack))

data_full |> 
  filter(compensation_grade_name == "Individual Contributor") |> 
  count(report_effective_date, compensation_grade_wkr) |> 
  mutate(label = if_else(report_effective_date == max(data_full$report_effective_date) & compensation_grade_wkr %in% c("C1", "C2", "C3"), n, NA)) |> 
  ggplot(aes(x = report_effective_date, y = n, group = compensation_grade_wkr)) +
  geom_line(color = hotheat, size = 1) +
  gghighlight(str_detect(compensation_grade_wkr, "C1|C2|C3"),
              line_label_type = "ggrepel_text", 
              label_params = list(family = lulu_font),
              unhighlighted_params = list(colour = neutral_1)) +
  geom_shadowtext(aes(label = label), family = lulu_font) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  labs(title = glue("Grade development at individual contributor level"),
       subtitle = "Number of roles at C1, C2 and C3 contribution levels over time",
       x = "Compensation grade levels")


## 6 Low spans over time  ---- 

data_focus |>
  mutate(low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")) |> 
  count(report_effective_date, .data[[so_level]], low_span, is_manager) |> 
  drop_na(is_manager) |> 
  select(-is_manager) |> 
  pivot_wider(id_cols = c(report_effective_date, .data[[so_level]]), names_from = low_span, values_from = n) |> 
  drop_na(.data[[so_level]]) |>
  mutate(across(low:ok, ~replace_na(.x, 0))) |> 
  mutate(perc = percent(low/(low+ok) ,digits = 0)) |> 
  
  # Remove old teams
  right_join(data_focus |>
               distinct(report_effective_date, .data[[so_level]]) |>
               expand(report_effective_date, .data[[so_level]])) |>
  ungroup() |>
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(perc), .data[[so_level]], NA)) |>
  filter(!.data[[so_level]] %in% trim) |>
  drop_na(.data[[so_level]], perc) |> 
  
  left_join(data_full |> 
              mutate(low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")) |> 
              count(report_effective_date, low_span, is_manager) |> 
              drop_na(is_manager) |> 
              select(-is_manager) |> 
              pivot_wider(id_cols = c(report_effective_date), names_from = low_span, values_from = n) |> 
              mutate(across(low:ok, ~replace_na(.x, 0))) |> 
              mutate(ll_perc = percent(low/(low+ok) ,digits = 0)), by = c("report_effective_date")
    
  ) |> 
  
  mutate(above_ave = if_else(perc > ll_perc, "above", "ok"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot() +
  geom_line(aes(x = report_effective_date, y = perc, group = .data[[so_level]]), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = .data[[so_level]]), color = neutral_3, size = 0.5, linetype = "dotted") +
  
  geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), color = neutral_3, fill = offwhite, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = above_ave, color = above_ave), family = lulu_font, fontface = "bold") +
  
  facet_wrap(~ .data[[so_level]]) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Low spans of control - {title_name}"),
       subtitle = "Percentage of managers with 3 or fewer direct reports over time vs lululemon average (dotted line)",
       x = "Compensation grade levels",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("above" = hotheat,
                               "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite,
                                "ok" = offblack))

###### 6.1 Low spans all lululemon  ----

data_full |>
  mutate(low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")) |> 
  count(report_effective_date, supervisory_organization_level_2, low_span, is_manager) |> 
  drop_na(is_manager) |> 
  select(-is_manager) |> 
  pivot_wider(id_cols = c(report_effective_date, supervisory_organization_level_2), names_from = low_span, values_from = n) |> 
  drop_na(supervisory_organization_level_2) |>
  mutate(across(low:ok, ~replace_na(.x, 0))) |> 
  mutate(perc = percent(low/(low+ok) ,digits = 0)) |> 
  
  
  left_join(data_full |> 
              mutate(low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")) |> 
              count(report_effective_date, low_span, is_manager) |> 
              drop_na(is_manager) |> 
              select(-is_manager) |> 
              pivot_wider(id_cols = c(report_effective_date), names_from = low_span, values_from = n) |> 
              mutate(across(low:ok, ~replace_na(.x, 0))) |> 
              mutate(ll_perc = percent(low/(low+ok) ,digits = 0)), by = c("report_effective_date")
            
  ) |> 
  
  mutate(above_ave = if_else(perc > ll_perc, "above", "ok"),
         label = if_else(report_effective_date == max(data_full$report_effective_date), perc, NA),
         label_ll = if_else(report_effective_date == max(data_full$report_effective_date), ll_perc, NA),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot() +
  geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_2), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_2), color = neutral_3, size = 0.5, linetype = "dotted") +
  
  geom_label(aes(x = report_effective_date, y = ll_perc, label = label_ll), color = neutral_3, fill = offwhite, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = perc, label = label, fill = above_ave, color = above_ave), family = lulu_font, fontface = "bold") +
  
  facet_wrap(~ supervisory_organization_level_2) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Low spans of control - {title_name}"),
       subtitle = "Percentage of managers with 3 or fewer direct reports over time vs lululemon average (dotted line)",
       x = "Compensation grade levels",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("above" = hotheat,
                               "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite,
                                "ok" = offblack))




## 7 Fill rate ----

data_focus |> 
  count(report_effective_date, .data[[so_level]], currently_active) |> 
  replace_na(list(currently_active = "No")) |> 
  group_by(report_effective_date, .data[[so_level]]) |> 
  summarise(filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)) |>
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |>
  drop_na(.data[[so_level]]) |> 
  
  left_join(data_full |> 
              count(report_effective_date, currently_active) |> 
              replace_na(list(currently_active = "No")) |> 
              group_by(report_effective_date) |> 
              summarise(ll_filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)) |>
              mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)),
            by = "report_effective_date") |> 
  
  mutate(hi_lo = case_when(filled <0.8 ~ "lo",
                           filled >0.9 ~ "hi",
                           .default = "ok")) |> 
  
  
  ggplot(aes(x = report_effective_date, group = .data[[so_level]])) +
  geom_rect(aes(xmin = 0.5, xmax = length(unique(data_focus$report_effective_date))+0.5, ymin = 0.8, ymax = .9), fill = neutral_1) +
  geom_hline(yintercept = 0.9, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = neutral_3, linetype = "dashed") +
  geom_line(aes(y = filled)) +
  # geom_line(aes(y = ll_filled), linetype = "dashed") +  
  geom_label(aes(label = filled, y = filled, fill = hi_lo, color = hi_lo), family = lulu_font, fontface = "bold") +
  facet_wrap(~ .data[[so_level]]) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 8) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Fill rate of the organization over time - {title_name}"),
       subtitle = "Active roles as a percentage of the fully funded organization against ideal target range (80-90% filled)",
       x = "Date",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("lo" = hotheat,
                               "ok" = offwhite,
                               "hi"  = yellow)) +
  scale_colour_manual(values = c("lo" = offwhite,
                               "ok" = offblack,
                               "hi"  = offblack))


###### Fill rate all lululemon  ----

data_full |> 
  filter(str_detect(supervisory_organization_level_2, "CEO", negate = TRUE)) |> 
  count(report_effective_date, supervisory_organization_level_2, currently_active) |> 
  replace_na(list(currently_active = "No")) |> 
  group_by(report_effective_date, supervisory_organization_level_2) |> 
  summarise(filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)) |>
  drop_na(supervisory_organization_level_2) |> 
  
  left_join(data_full |> 
              count(report_effective_date, currently_active) |> 
              replace_na(list(currently_active = "No")) |> 
              group_by(report_effective_date) |> 
              summarise(ll_filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)),
            by = "report_effective_date") |> 
  
  mutate(hi_lo = case_when(filled <0.8 ~ "lo",
                           filled >0.9 ~ "hi",
                           .default = "ok"),
         label = if_else(report_effective_date == max(data_full$report_effective_date), filled, NA),
         label_ll = if_else(report_effective_date == max(data_full$report_effective_date), ll_filled, NA),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  
  ggplot(aes(x = report_effective_date, group = supervisory_organization_level_2)) +
  geom_rect(aes(xmin = 0.5, xmax = length(unique(data_full$report_effective_date))+0.5, ymin = 0.8, ymax = .9), fill = neutral_1) +
  # geom_hline(yintercept = 0.9, color = neutral_3, linetype = "dashed") +
  # geom_hline(yintercept = 0.8, color = neutral_3, linetype = "dashed") +
  geom_line(aes(y = filled)) +
  geom_line(aes(y = ll_filled), color = neutral_3, linetype = "dashed") +

  geom_label(aes(label = label_ll, y = ll_filled), family = lulu_font, fontface = "bold", fill = neutral_1, alpha = 0.5) +
  geom_label(aes(label = label, y = filled, fill = hi_lo, color = hi_lo), family = lulu_font, fontface = "bold") +
  facet_wrap(~ supervisory_organization_level_2) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 8) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Fill rate of the organization over time by SLT area"),
       subtitle = "Active roles as a percentage of the fully funded organization against ideal target range (80-90% filled)",
       x = "Date",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("lo" = hotheat,
                               "ok" = offwhite,
                               "hi"  = yellow)) +
  scale_colour_manual(values = c("lo" = offwhite,
                                 "ok" = offblack,
                                 "hi"  = offblack))


## 8 Succession gaps L2 only ----

order <- c("E2", "E1", "M5", "M4")

data_full |> 
  filter(compensation_grade_wkr %in% c("M5","E1", "E2")) |> 
  group_by(report_effective_date, supervisory_organization_level_2, compensation_grade_wkr, alert_successors) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_successors = "Potential successor")) |> 
  summarise(succession_risk =  percent(sum(total[alert_successors == "No successor"]) / sum(total),digits = 0)) |>
  group_by(supervisory_organization_level_2) |> 
  mutate(label = if_else(report_effective_date == max(report_effective_date), succession_risk, NA),
         hi_lo = case_when(label <0.25 ~ "lo",
                           label >= 0.25 & label < 0.5 ~ "mid",
                           label >= 0.5 ~ "hi"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted),
         compensation_grade_wkr = factor(compensation_grade_wkr, levels = order),
         supervisory_organization_level_2 = str_wrap(supervisory_organization_level_2, width = 20),
         compensation_grade_wkr = case_when(compensation_grade_wkr == "M5" ~ "Senior Director",
                                            compensation_grade_wkr == "E1" ~ "VP",
                                            compensation_grade_wkr == "E2" ~ "SVP"),
         compensation_grade_wkr = factor(compensation_grade_wkr, levels = c("Senior Director","VP","SVP"))) |>
  drop_na(supervisory_organization_level_2) |>
  ggplot(aes(x = report_effective_date, y = succession_risk, group = compensation_grade_wkr)) +
  geom_line(colour = neutral_3) +
  geom_label(aes(label = label, fill = hi_lo, color = hi_lo), family = lulu_font, fontface = "bold") +
  facet_grid(rows = vars(compensation_grade_wkr), cols = vars(supervisory_organization_level_2)) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = c("Aug 2023", "Aug 2024")) +
  theme(axis.text.y = element_blank()) +
  labs(title = glue("Strutural succession risk by grade over time"),
       subtitle = "Percentage of Senior Director+ roles without a direct succession path one grade down from the manager",
       x = "Date",
       y = "Percentage of roles without a clear succession path",
       caption = "Excludes leaves and non-SSC workforce.") +
  scale_fill_manual(values = c("lo" = offwhite,
                              "mid" = yellow,
                              "hi" = hotheat)) +
  scale_colour_manual(values = c("lo" = offblack,
                               "mid" = offblack,
                               "hi" = offwhite))

### 8.1 Succession gaps  ----

data_focus |>
  filter(compensation_grade_wkr %in% c("M4", "M5","E1", "E2", "E3", "E4")) |> 
  group_by(report_effective_date, compensation_grade_wkr, alert_successors) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_successors = "Potential successor",
                  supervisory_organization_level_3 = "Exec leadership")) |> 
  summarise(succession_risk =  percent(sum(total[alert_successors == "No successor"]) / sum(total),digits = 0)) |>
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  ggplot(aes(x = report_effective_date, y = succession_risk, group = compensation_grade_wkr)) +
  geom_line(colour = neutral_3) +
  geom_label(aes(label = succession_risk), family = lulu_font, fill = yellow, fontface = "bold") +
  facet_wrap(~ compensation_grade_wkr) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Strutural succession risk by grade over time"),
       subtitle = "Percentage of Director+ roles without a succession path one grade down from the manager",
       x = "Date",
       y = "Percentage of roles without a clear succession path",
       caption = "Excludes leaves and non-SSC workforce.") 

### 8.2 Succession gaps detail  ----

data_focus |>
  filter(compensation_grade_wkr %in% c("M4", "M5","E1", "E2", "E3", "E4"),
         report_effective_date == max(data_focus$report_effective_date)) |> 
  group_by(report_effective_date, .data[[so_level]], compensation_grade_wkr, alert_successors) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_successors = "Potential successor",
                  supervisory_organization_level_3 = "Exec leadership")) |> 
  summarise(succession_risk =  percent(sum(total[alert_successors == "No successor"]) / sum(total),digits = 0)) |> 
  filter(succession_risk > 0) |> 
  mutate(compensation_grade_wkr = factor(compensation_grade_wkr, levels = c("M4", "M5","E1", "E2", "E3", "E4")),
         hi_lo = case_when(succession_risk >0 & succession_risk <= 0.25 ~ "lo",
                           succession_risk >0.25 & succession_risk <= 0.5 ~ "mid",
                           succession_risk >0.5 & succession_risk <= 0.75 ~ "hi",
                           succession_risk >0.75  ~ "vhi")) |> 
  ggplot(aes(x = compensation_grade_wkr, y = succession_risk, group = .data[[so_level]])) +
  geom_bar(aes(fill = hi_lo), stat = "identity", position = position_dodge(), colour = offwhite, width = 0.9, alpha = 0.8) +
  geom_text(aes(label = .data[[so_level]], y = succession_risk, color = hi_lo), stat = "identity", position = position_dodge(width = 0.9), family = lulu_font, hjust = 1) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("lo" = neutral_2,
                               "mid" = neutral_3,
                               "hi" = dark_brown,
                               "vhi" = hotheat)) +
  scale_color_manual(values = c("lo" = offblack,
                               "mid" = offwhite,
                               "hi" = offwhite,
                               "vhi" = offblack)) +
  
  labs(title = glue("Strutural succession risk"),
       subtitle = "Percentage of Director+ roles without a succession path one grade down from the manager",
       x = "Grade level",
       y = "Percentage of roles without a clear succession path",
       caption = "Excludes leaves and non-SSC workforce.") 
  

## 9 Compression/friction ----


data_focus |>
  filter(compensation_grade_wkr %in% c("M2", "M3", "M4", "M5","E1", "E2", "E3", "E4")) |> 
  group_by(report_effective_date, compensation_grade_name, alert_compression) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_compression = "Ok",
                  supervisory_organization_level_3 = "Exec leadership")) |> 
  summarise(compression_risk =  percent(sum(total[alert_compression == "Team compression"]) / sum(total),digits = 0)) |>
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted),
         compensation_grade_name = factor(compensation_grade_name, levels = c("Manager", "Senior Manager", "Director", "Senior Director","VP", "SVP", "EVP", "SLT"))
         ) |> 
  ggplot(aes(x = report_effective_date, y = compression_risk, group = compensation_grade_name)) +
  geom_line(colour = neutral_3) +
  geom_label(aes(label = compression_risk), family = lulu_font, fill = blue, fontface = "bold") +
  facet_wrap(~ compensation_grade_name) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Structural compression risk by grade over time - {title_name}"),
       subtitle = "Percentage of teams featuring same contribution grade reporting and/or more than 30% of roles within one grade level of the manager",
       x = "Date",
       y = "Percentage of compressed teams") 


###### 9.1 Compression all lululemon  ----

data_full |>
  filter(compensation_grade_wkr %in% c("M4", "M5","E1", "E2")) |> 
  group_by(report_effective_date, supervisory_organization_level_2, alert_compression) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_compression = "Ok",
                  supervisory_organization_level_3 = "Exec leadership")) |> 
  summarise(compression_risk =  percent(sum(total[alert_compression == "Team compression"]) / sum(total),digits = 0)) |>
  mutate(label = if_else(report_effective_date == max(data_full$report_effective_date), compression_risk, NA),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  drop_na(supervisory_organization_level_2) |> 
  ggplot(aes(x = report_effective_date, y = compression_risk, group = supervisory_organization_level_2)) +
  geom_line(colour = neutral_3) +
  geom_label(aes(label = label), family = lulu_font, fill = blue, fontface = "bold") +
  facet_wrap(~ supervisory_organization_level_2) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Role compression by SLT area"),
       subtitle = "Percentage of teams featuring same contribution grade reporting and/or more than 30% of roles within one grade level of the manager",
       x = "Date",
       y = "Percentage of compressed teams") 



## 10 Operating leverage  ----

data_full |> 
  tabyl(supervisory_organization_level_2) 

"Americas and Global Guest Innovation (Celeste Burgoyne)" ok
   "Brand & Creative Content (Nikki Neuburger)" ok
"CFO (Meghan Frank)" ok
   "Creative - Design and Concept (Jonathan Cheung)"
   "International (Andre Maestrini)" ok
   "Legal (Shannon Higginson)" ok
   "People & Culture (Susan Gelinas)" ok
   "Supply Chain (Ted Dagnese)" ok
   "Technology (Julie Averill)"





data_full |> 
  filter(
    # vacancy == "No",
         supervisory_organization_level_2 == "Technology (Julie Averill)",
         str_detect(supervisory_organization_level_3, "Assistants", negate = TRUE)) |> 
  group_by(supervisory_organization_level_3, report_effective_date) |>
  summarise(n = n()) |> 
  mutate(growth_rate = percent((n - lag(n, n = 1))/lag(n, n = 1), digits = 1)) |>
  drop_na(supervisory_organization_level_3) |> 
  left_join(data_full |> 
              filter(vacancy == "No") |> 
              count(report_effective_date) |>
              mutate(growth_rate_ll = percent((n - lag(n, n = 1))/lag(n, n = 1), digits = 1)), by = "report_effective_date") |> 
  mutate(team_perc = n.x/n.y,
         weighted_growth = team_perc * growth_rate,
         relative_growth = growth_rate - growth_rate_ll,
         growth_significance = weighted_growth/mean(team_perc, na.rm = TRUE),
         label = if_else(relative_growth > 0, relative_growth, NA)) |>
  
  ggplot(aes(x = report_effective_date,
             y = relative_growth,
             size = team_perc,
             color = supervisory_organization_level_3)) +
  geom_line() +
  geom_point(shape = 21, fill = offwhite) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = label), size = 4, vjust = -1, color = offblack, fontface = "bold", family = lulu_font) +
  scale_size_continuous(range = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  facet_wrap(~supervisory_organization_level_3) +
  # lims(y =c(-0.5, 0.5)) +
  labs(title = "Team headcount growth rates relative to overall heacount growth",
       subtitle = "SLT-1 department growth by quarter; line thickness shows relative team size",
       y = "Relative Growth Rate",
       size = "Team Proportion")
  
# whole org
data_full |>
  filter(str_detect(supervisory_organization_level_2, "CEO|MIRROR", negate = TRUE)) |> 
  group_by(supervisory_organization_level_2, report_effective_date) |>
  summarise(n = n()) |> 
  mutate(growth_rate = percent((n - lag(n, n = 1))/lag(n, n = 1), digits = 1)) |>
  drop_na(supervisory_organization_level_2) |> 
  left_join(data_full |> 
              filter(vacancy == "No") |> 
              count(report_effective_date) |>
              mutate(growth_rate_ll = percent((n - lag(n, n = 1))/lag(n, n = 1), digits = 1)), by = "report_effective_date") |> 
  mutate(team_perc = n.x/n.y,
         weighted_growth = team_perc * growth_rate,
         relative_growth = growth_rate - growth_rate_ll,
         growth_significance = weighted_growth/mean(team_perc, na.rm = TRUE),
         label = if_else(relative_growth > 0, relative_growth, NA)) |>
  
  ggplot(aes(x = report_effective_date,
             y = relative_growth,
             size = team_perc,
             color = supervisory_organization_level_2)) +
  geom_line() +
  geom_point(shape = 21, fill = offwhite) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = label), size = 4, vjust = -1, color = offblack, fontface = "bold", family = lulu_font) +
  scale_size_continuous(range = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  facet_wrap(~supervisory_organization_level_2) +
  # lims(y =c(-0.5, 0.5)) +
  labs(title = "Team headcount growth rates relative to overall heacount growth",
       subtitle = "SLT-1 department growth by quarter; line thickness shows relative team size",
       y = "Relative Growth Rate",
       size = "Team Proportion")







## Data packs  ---- 


slt_areas <- c("Americas and Global Guest Innovation (Celeste Burgoyne)", "CFO (Meghan Frank)", "People & Culture (Susan Gelinas)", "Design and Merchandising (Sun Choe)", "Supply Chain (Ted Dagnese)", "Brand & Creative Content (Nikki Neuburger)", "Technology (Julie Averill)", "International (Andre Maestrini)", "Legal (Shannon Higginson)")

# Low spans

for (i in slt_areas) {

data_full |>
  filter(report_effective_date == "May 2024",
         supervisory_organization_level_2 == i) |>
  filter(direct_reports > 0 & direct_reports < 4) |> 
  select(supervisory_organization_level_3, position_id_worker, worker_name, compensation_grade, direct_reports) |> 
  arrange(supervisory_organization_level_3, desc(compensation_grade), desc(direct_reports)) |> 
  write_excel_csv(glue("data_out/low_spans_{i}.csv"))

}
  
# Compression

for (i in slt_areas) {

data_full |> 
  filter(report_effective_date == "May 2024",
         supervisory_organization_level_2 == i,
         comp_grade_overlap == comp_grade_overlap_mgr) |>
  select(supervisory_organization_level_3, position_id_worker, worker_name, compensation_grade, position_id_manager, worker_name_mgr, compensation_grade_mgr) |> 
  arrange(desc(supervisory_organization_level_3), desc(compensation_grade)) |> 
    write_excel_csv(glue("data_out/compression_{i}.csv"))
  

}


# Project managers

for (i in slt_areas) {
  
  data_full |> 
    filter(report_effective_date == "May 2024",
           supervisory_organization_level_2 == i,
           str_detect(job_title, "(P|p)ro(g|j)")) |>
    select(supervisory_organization_level_3, position_id_worker, worker_name, job_title) |> 
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |> 
    write_excel_csv(glue("data_out/project_managers_{i}.csv"))
  
  
}


# Analysts

for (i in slt_areas) {
  

  data_full |> 
    filter(report_effective_date == "May 2024",
           supervisory_organization_level_2 == i,
           str_detect(job_title, "(A|a)naly")) |>
    select(supervisory_organization_level_3, position_id_worker, worker_name, job_title) |> 
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |> 
    write_excel_csv(glue("data_out/analysts_{i}.csv"))

}


# Product Managers

for (i in slt_areas) {
  

  data_full |> 
    filter(report_effective_date == "May 2024",
           supervisory_organization_level_2 == i,
           str_detect(job_title, "(P|r)oduct Manager")) |>
    select(supervisory_organization_level_3, position_id_worker, worker_name, job_title) |> 
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |> 
    write_excel_csv(glue("data_out/product_managers_{i}.csv"))
  
}



data_full |> 
  filter(report_effective_date == "May 2024",
         # str_detect(job_title, "(C|c)hange"),
         # job_family == "Project Management",
         # str_detect(job_title, "Program", negate = TRUE),
         str_detect(job_title, "(S|s)trategic")
         ) |>
  select(supervisory_organization_level_3, position_id_worker, worker_name, job_title, job_family) |> 
  arrange(desc(supervisory_organization_level_3), desc(job_title)) |> 
  print(n=40)
