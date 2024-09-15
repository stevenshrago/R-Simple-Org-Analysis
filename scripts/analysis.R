library(pacman)
p_load(tidyverse, janitor, glue, datapasta, formattable, extrafont, WriteXLS, explore, readxl, lubridate, tidygraph, ggraph)

source("scripts/functions/helpers.r")

## Load data  ---- 

# data_orig <- readxl::read_xlsx("data_in/3ydata.xlsx", sheet = 2, .name_repair = make_clean_names) # read in the orgiinal excel file and standardize the variable names (snake case)

data_orig <- readxl::read_xlsx("data_in/quarterly_data_original.xlsx", sheet = 2, .name_repair = make_clean_names) 

# extract_date <-  "2024-05-03"

## Clean & prepare data ----

# Still need to figure out a simple way of determining depth


data_clean <- data_orig |> 
  mutate(report_effective_date = lubridate::as_date(report_effective_date),
         readable_date = format(report_effective_date, "%B %Y"),
         position_id_manager = if_else(position_id_worker == "83-093759", NA_character_, position_id_manager),
         compensation_grade = str_remove_all(compensation_grade, "L|N|P|S|T|I"),
         compensation_grade = factor(compensation_grade, levels = grade_levels),
         supervisory_organization_level_2 = case_when(supervisory_organization_level_2 == "Supply Chain, Distribution, Sourcing (Ted Dagnese)" ~ "Supply Chain (Ted Dagnese)",
                                                      .default = supervisory_organization_level_2),
         supervisory_organization_level_3 = case_when(supervisory_organization_level_3 == "Raw Material Developments - Advanced Materials Innovation (Yogendra Dandapure)" ~ "Raw Materials Innovation (Yogendra Dandapure)",
                                                      supervisory_organization_level_3 == "Quality Assurance, Strategy, Testing & Compliance (Rene Wickham)" ~ "Product Integrity (Rene Wickham)",
                                                      supervisory_organization_level_3 == "Quality (Rene Wickham)" ~ "Product Integrity (Rene Wickham)",
                                                      supervisory_organization_level_3 == "Raw Material Developments (Patty Stapp)" ~ "Global Raw Materials (Patty Stapp)",
                                                      supervisory_organization_level_3 == "Supply Chain Strategy & Planning (Ravi Chowdary)" ~ "Supply Chain Strategy & Planning (Andrew Polins)",
                                                      .default = supervisory_organization_level_3)) |>
  left_join(grade_scoring, by = "compensation_grade")

data_clean_drs <- data_clean |> 
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
            by = c("report_effective_date", "position_id_worker" = "position_id_manager")) |> 
  filter(report_effective_date > "2023-05-05")


dates_formatted <- data_clean_alerts |> 
  distinct(report_effective_date) |> 
  mutate(readable = format(report_effective_date, "%b %Y")) |> 
  arrange(report_effective_date) |> 
  pull(readable)


## Prepare data for analysis ----

data_full <- data_clean_alerts

data_focus <- data_clean_alerts |> 
  filter(supervisory_organization_level_2 == "Supply Chain (Ted Dagnese)")


## Graph data ----
                 
graph_data <- data_clean |>
  select(from = position_id_manager, to = position_id_workday, job_title, -report_effective_date)
  
nodes <- graph_data  |> 
    select(from, to) |> 
    unlist() |> 
    unique() |> 
    as_tibble() |> 
    rename(name = value) |> 
    left_join(data_clean, by = c("name" = "position_id_workday"))
  
edges <- graph_data %>%
    mutate(
      from = match(from, nodes$name),
      to = match(to, nodes$name)
    )
  
graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) |> 
  activate(nodes) |> 
  mutate(direct_positions = local_size(order = 1, mindist = 1, mode = "out"),
         total_positions = local_size(order = 10, mindist = 1, mode = "out"),
         depth = bfs_dist(which(.N()$name == "83-093759")))


  





# Apply data to child nodes


# find_manager_from_name <- data_graph %>%
#   activate(nodes) %>% 
#   select(name, om_position_title, employee_name, direct_chief_position) %>%
#   filter(str_detect(employee_name, "PEYER")) %>%
#   print()
# 
# find_manager_from_position <- data_graph %>%
#   select(name, om_position_title, employee_name, direct_chief_position) %>%
#   filter(str_detect(om_position_title, "General Counsel")) %>%
#   print()
# 
# find_manager_from_id <- data_graph %>% 
#   select(name, om_position_title, employee_name, direct_chief_position) %>%
#   filter(str_detect(name, "10335681")) %>%
#   print()
# 
# leadership_team <- data_graph %>%
#   activate(nodes) %>% 
#   filter(direct_chief_position == "10244451") %>%
#   select(name, employee_name, om_position_title, fte_position, fte_person, tot_fte_pos) %>% 
#   as_tibble() %>% 
#   print(n=26)


graph %>% 
  convert(to_local_neighborhood,
          # node = which(.N()$name == "83-093759"),
          node = which(.N()$name == "83-080777"),
          order = 1,
          mode = "out") %>% 
  ggraph(layout = "partition", circular = T) +
  geom_edge_link() +
  coord_flip() +
  geom_node_text(aes(label = paste0(name,"\n",job_title,"\n", worker_name))) +
  theme_graph()


## 1 Manager Percentages  ---- 

data_focus |>
  group_by(report_effective_date, supervisory_organization_level_3, is_manager) |> #group and count number of non-/managers by sup org and date
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> # fill in blank/NA 
  pivot_wider(id_cols = c(report_effective_date, supervisory_organization_level_3), names_from = is_manager, values_from = n) |> # reshape
  mutate(Yes = Yes+1) |> # adjustment to count the manager of the sup org in the numbers
  summarize(perc = percent(Yes/(Yes+No), digits = 0))|> # percetages of non-/managers
  drop_na(supervisory_organization_level_3) |>
  filter(perc > 0) |> # filter non-zero percentages
  
  left_join(data_full |> # calculate lululemon overall percentages
              group_by(report_effective_date, is_manager) |> 
              summarise(n = n()) |> 
              mutate(is_manager = replace_na(is_manager, "No")) |> 
              pivot_wider(id_cols = c(report_effective_date), names_from = is_manager, values_from = n) |> 
              summarize(ll_perc = percent(Yes/(Yes+No), digits = 0)),
            by = c("report_effective_date")) |> 
  
  right_join(data_focus |> # next few lines remove records for historical sup orgs that no longer exist in 2024
               distinct(report_effective_date, supervisory_organization_level_3) |> 
               expand(report_effective_date, supervisory_organization_level_3)) |>
  ungroup() |> 
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(perc), supervisory_organization_level_3, NA)) |>
  filter(!supervisory_organization_level_3 %in% trim) |> 
  mutate(high_ok_low = case_when(perc >= 0.25 ~ "high", # categorization of percentage levels against best practice
                                 perc <= 0.15 ~ "low",
                                 .default = "ok"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = length(unique(data_focus$report_effective_date))+1, ymin = 0.15, ymax = .25), fill = neutral_1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_3), color = offblack, size = 0.5, linetype = "dashed") +
  geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_3), color = neutral_3, size = 1) +
  facet_wrap(~ supervisory_organization_level_3) +
  geom_hline(yintercept = 0.15, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.25, color = neutral_3, linetype = "dashed") +
  geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = high_ok_low, color = high_ok_low), family = lulu_font, fontface = "bold") +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Percentage of line managers "),
       subtitle = "Line managers as a percentage of roles over time vs lululemon average and external benchmark (15-25%)",
       x = "Date",
       y = "Manager perentage") +
  scale_fill_manual(values = c("high" = hotheat,
                               "ok" = pale_green,
                               "low" = blue)) +
  scale_color_manual(values = c("high" = offwhite,
                                "ok" = offblack,
                                "low" = offblack))


## 2 Average span of control  ---- 



data_focus |>
  # left_join(data_focus |> # this code ensures that the manager of the sup org unit is classified as part of the org unit
  #             distinct(report_effective_date, supervisory_organization_level_3) |>
  #             mutate(worker_name = str_extract(supervisory_organization_level_3, "(?<=\\()[^()]+(?=\\))")) |>
  #             drop_na(supervisory_organization_level_3),
  #           by = c("worker_name", "report_effective_date"), suffix = c("", ".b")) |>
  # mutate(supervisory_organization_level_3 = coalesce(supervisory_organization_level_3, supervisory_organization_level_3.b)) |>
  
  group_by(report_effective_date, supervisory_organization_level_3) |> # calculate average span of control
  summarise(average_span = round(mean(direct_reports, na.rm = TRUE),1.1)) |>
  drop_na(supervisory_organization_level_3) |> 
  
  right_join(data_focus |>  # next few lines remove records for historical sup orgs that no longer exist in 2024
               distinct(report_effective_date, supervisory_organization_level_3) |>
               expand(report_effective_date, supervisory_organization_level_3)) |>
  ungroup() |>
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(average_span), supervisory_organization_level_3, NA)) |>
  filter(!supervisory_organization_level_3 %in% trim) |>
  
  drop_na(supervisory_organization_level_3, average_span) |> 
  
  
  mutate(low_ok = if_else(average_span >= 5 & average_span <= 8, "ok", "low"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> # categorize span ranges for plots
  ggplot(aes(x = report_effective_date, y = average_span, group = supervisory_organization_level_3)) +
  geom_rect(aes(xmin = 0, xmax = length(unique(data_focus$report_effective_date))+1, ymin = 5, ymax = 8), fill = neutral_1) +
  geom_line() +
  facet_wrap(~ supervisory_organization_level_3) +
  geom_hline(yintercept = 5, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 8, color = neutral_3, linetype = "dashed") +
  geom_label(aes(label = average_span, fill = low_ok, color = low_ok), family = lulu_font, fontface = "bold") +
  labs(title = "Average span of control",
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
 

## 3 Compression  ---- 


# data_focus |> 
#   filter(comp_grade_overlap == comp_grade_overlap_mgr) |> # find compressed roles
#   count(report_effective_date, supervisory_organization_level_3) |>  # count by sup org and date
#   left_join(data_focus |> 
#               count(report_effective_date, supervisory_organization_level_3),
#             by = c("report_effective_date", "supervisory_organization_level_3")) |> 
#   group_by(report_effective_date, supervisory_organization_level_3) |> 
#   reframe(perc = percent(n.x/n.y, digits = 1)) |> # summarise compression percentage, but in a weird way
#   
#   # Remove old teams
#   right_join(data_focus |>
#               distinct(report_effective_date, supervisory_organization_level_3) |>
#               expand(report_effective_date, supervisory_organization_level_3)) |>
#   ungroup() |>
#   mutate(trim = if_else(report_effective_date == "May 2024" & is.na(perc), supervisory_organization_level_3, NA)) |>
#   filter(!supervisory_organization_level_3 %in% trim) |>
#   drop_na(supervisory_organization_level_3, perc) |> 
#   
#   left_join(data_full |> # add lululemon average
#               mutate(compression = if_else(comp_grade_overlap == comp_grade_overlap_mgr, "compressed", "ok")) |>
#               count(report_effective_date, compression) |> 
#               drop_na(compression) |> 
#               pivot_wider(id_cols = report_effective_date, names_from = compression, values_from = n) |> 
#               mutate(ll_perc = percent(compressed/(compressed+ok), digits = 1)), by = c("report_effective_date")) |> 
#   mutate(above_ave = if_else(perc > ll_perc, "above", "ok")) |> 
#   
#   ggplot() +
#   geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_3), color = offblack, size = 1) +
#   geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_3), color = neutral_3, linetype = "dashed", size = 0.5) +
#   facet_wrap(~supervisory_organization_level_3) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   geom_label(aes(label = ll_perc, x = report_effective_date, y = ll_perc), family = lulu_font, colour = neutral_3, label.size = 0) +
#   geom_label(aes(label = perc, x = report_effective_date, y = perc, fill = above_ave, color = above_ave), family = lulu_font, fontface = "bold") +
#   theme_clean_lulu() +
#   standard_text_x(bold = FALSE) +
#   # standard_text_y(bold = FALSE) +
#   labs(title = glue("Grade compression: {focus}"),
#        subtitle = "Compressed roles as a percentage of total roles compared to the lululemon average (dotted line)") +
#   scale_fill_manual(values = c("above" = hotheat,
#                                "ok" = offwhite)) +
#   scale_color_manual(values = c("above" = offwhite,
#                                 "ok" = offblack))


## 4 Executive development  ---- 

data_focus |> 
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
  
  mutate(vsbm = if_else(exec_perc > 0.02, "high", "ok"),
         report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |>  # classification of ranges for plot
  ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = length(unique(data_focus$report_effective_date))+0.5, ymin = 0, ymax = .02), fill = neutral_1) +
  geom_hline(aes(yintercept = 0.02), color = neutral_4, linetype = "dashed") +
  geom_line(aes(x = report_effective_date, y = exec_perc, group = supervisory_organization_level_2), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_exec_perc, group = supervisory_organization_level_2), color = neutral_3, size = 0.5, linetype = "dashed") +
  geom_label(aes(x = report_effective_date, y = ll_exec_perc, label = ll_exec_perc), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = exec_perc, label = exec_perc, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
  labs(title = glue("Executive roles percentage"),
       subtitle = "Percentage of VP+ roles vs lululemon average (dotted line) and external benchmark (<=2%)") +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.05)) +

  scale_fill_manual(values = c("ok" = pale_green,
                               "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack,
                                "high" = offwhite))


## 5 Project Managers  ----


# data_focus |>
#   mutate(pjm = if_else(str_detect(job_title, "(P|p)ro(g|j)"), "Yes", "No")) |> # classify project managers by job title
#   count(report_effective_date, supervisory_organization_level_2, pjm) |> # group and count project managers
#   pivot_wider(id_cols = 1:2, names_from = pjm, values_from = n) |> 
#   group_by(report_effective_date, supervisory_organization_level_2) |> 
#   summarize(perc = percent(Yes/(Yes+No), digits = 1)) |> # percentages
#   left_join(data_full |>  # lululemon averages
#               mutate(pjm = if_else(str_detect(job_title, "(P|p)ro(g|j)"), "Yes", "No")) |> 
#               count(report_effective_date, pjm) |> 
#               pivot_wider(id_cols = 1, names_from = pjm, values_from = n) |> 
#               group_by(report_effective_date) |> 
#               summarize(ll_perc = percent(Yes/(Yes+No), digits = 1)), by = c("report_effective_date")) |> 
#   mutate(vsbm = if_else(perc > 0.01, "high", "ok")) |> # classsify ranges for plot
#   ggplot() +
#   geom_rect(aes(xmin = 0.5, xmax = 3.5, ymin = 0, ymax = .01), fill = neutral_5) +
#   geom_hline(aes(yintercept = 0.01), color = neutral_4, linetype = "dashed") +
#   
#   geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_2), color = offblack, size = 1) +
#   geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_2), color = neutral_4, size = 0.5, linetype = "dashed") +
#   
#   geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = vsbm, color = vsbm), family = lulu_font, fontface = "bold") +
#   geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
#   labs(title = glue("Project & Program management roles: {focus}"),
#        subtitle = "Percentage of Project and Program Management roles over time vs lululemon average (dotted line) and external benchmark (<=1%)") +
#   theme_clean_lulu() +
#   standard_text_x(bold = FALSE) +
#   standard_text_y(bold = FALSE) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_fill_manual(values = c("ok" = lightgreen,
#                                "high" = hotheat)) +
#   scale_color_manual(values = c("ok" = offblack,
#                                 "high" = offwhite))


## 6 Grade development  ---- 

data_focus |> 
  count(report_effective_date, supervisory_organization_level_2, grade_score_wkr) |> # count records by contriubution level, date and sup org
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



## 7 Low spans over time  ---- 

data_focus |>
  # Add L3 leaders to the calculation
  # left_join(data_focus |> # this code ensures that the manager of the sup org unit is classified as part of the org unit
  #             distinct(report_effective_date, supervisory_organization_level_3) |>
  #             mutate(worker_name = str_extract(supervisory_organization_level_3, "(?<=\\()[^()]+(?=\\))")) |>
  #             drop_na(supervisory_organization_level_3),
  #           by = c("worker_name", "report_effective_date"), suffix = c("", ".b")) |>
  # mutate(supervisory_organization_level_3 = coalesce(supervisory_organization_level_3, supervisory_organization_level_3.b)) |>
  
  mutate(low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")) |> 
  count(report_effective_date, supervisory_organization_level_3, low_span, is_manager) |> 
  drop_na(is_manager) |> 
  select(-is_manager) |> 
  pivot_wider(id_cols = c(report_effective_date, supervisory_organization_level_3), names_from = low_span, values_from = n) |> 
  drop_na(supervisory_organization_level_3) |>
  mutate(across(low:ok, ~replace_na(.x, 0))) |> 
  mutate(perc = percent(low/(low+ok) ,digits = 0)) |> 
  
  # Remove old teams
  right_join(data_focus |>
               distinct(report_effective_date, supervisory_organization_level_3) |>
               expand(report_effective_date, supervisory_organization_level_3)) |>
  ungroup() |>
  mutate(trim = if_else(report_effective_date == max(data_full$report_effective_date) & is.na(perc), supervisory_organization_level_3, NA)) |>
  filter(!supervisory_organization_level_3 %in% trim) |>
  drop_na(supervisory_organization_level_3, perc) |> 
  
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
  geom_line(aes(x = report_effective_date, y = perc, group = supervisory_organization_level_3), color = offblack, size = 1) +
  geom_line(aes(x = report_effective_date, y = ll_perc, group = supervisory_organization_level_3), color = neutral_3, size = 0.5, linetype = "dotted") +
  
  geom_label(aes(x = report_effective_date, y = ll_perc, label = ll_perc), color = neutral_3, fill = offwhite, family = lulu_font, label.size = 0) +
  geom_label(aes(x = report_effective_date, y = perc, label = perc, fill = above_ave, color = above_ave), family = lulu_font, fontface = "bold") +
  
  facet_wrap(~ supervisory_organization_level_3) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Low spans of control"),
       subtitle = "Percentage of managers with 3 or fewer direct reports over time vs lululemon average (dotted line)",
       x = "Compensation grade levels") +
  scale_fill_manual(values = c("above" = hotheat,
                               "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite,
                                "ok" = offblack))





## 8 Fill rate ----

data_focus |> 
  count(report_effective_date, supervisory_organization_level_3, currently_active) |> 
  replace_na(list(currently_active = "No")) |> 
  group_by(report_effective_date, supervisory_organization_level_3) |> 
  summarise(filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)) |>
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |>
  drop_na(supervisory_organization_level_3) |> 
  
  left_join(data_full |> 
              count(report_effective_date, currently_active) |> 
              replace_na(list(currently_active = "No")) |> 
              group_by(report_effective_date) |> 
              summarise(ll_filled = percent(sum(n[currently_active == "Yes"])/ sum(n), digits = 0)) |>
              mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)),
            by = "report_effective_date") |> 
  
  
  ggplot(aes(x = report_effective_date, group = supervisory_organization_level_3)) +
  geom_rect(aes(xmin = 0.5, xmax = length(unique(data_focus$report_effective_date))+0.5, ymin = 0.8, ymax = .9), fill = pale_green) +
  geom_hline(yintercept = 0.9, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = neutral_3, linetype = "dashed") +
  geom_line(aes(y = filled)) +
  geom_line(aes(y = ll_filled), linetype = "dashed") +  
  facet_wrap(~ supervisory_organization_level_3) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 8) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Fill rate of the organization over time"),
       subtitle = "Active roles as a percentage of the fully funded organization",
       x = "Date")


## 9 Succession gaps ----

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
       y = "Percentage of roles without a clear succession path") 


data_focus |>
  filter(compensation_grade_wkr %in% c("M4", "M5","E1", "E2", "E3", "E4"),
         report_effective_date == max(data_focus$report_effective_date)) |> 
  group_by(report_effective_date, supervisory_organization_level_3, compensation_grade_wkr, alert_successors) |> 
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
  ggplot(aes(x = compensation_grade_wkr, y = succession_risk, group = supervisory_organization_level_3)) +
  geom_bar(aes(fill = hi_lo), stat = "identity", position = position_dodge(), colour = offwhite, width = 0.9) +
  geom_text(aes(label = supervisory_organization_level_3, y = succession_risk/2), stat = "identity", position = position_dodge(width = 1), angle = 90, family = lulu_font) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("lo" = neutral_2,
                               "mid" = neutral_3,
                               "hi" = dark_brown,
                               "vhi" = hotheat)) +
  labs(title = glue("Strutural succession risk"),
       subtitle = "Percentage of Director+ roles without a succession path one grade down from the manager",
       x = "Grade level",
       y = "Percentage of roles without a clear succession path") 
  

## Compression/friction


data_focus |>
  filter(compensation_grade_wkr %in% c("M4", "M5","E1", "E2", "E3", "E4")) |> 
  group_by(report_effective_date, compensation_grade_wkr, alert_compression) |> 
  summarise(total = n()) |> 
  replace_na(list(alert_compression = "Ok",
                  supervisory_organization_level_3 = "Exec leadership")) |> 
  summarise(compression_risk =  percent(sum(total[alert_compression == "Team compression"]) / sum(total),digits = 0)) |>
  mutate(report_effective_date = factor(format(report_effective_date, "%b %Y"), levels = dates_formatted)) |> 
  ggplot(aes(x = report_effective_date, y = compression_risk, group = compensation_grade_wkr)) +
  geom_line(colour = neutral_3) +
  geom_label(aes(label = compression_risk), family = lulu_font, fill = blue, fontface = "bold") +
  facet_wrap(~ compensation_grade_wkr) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = glue("Strutural compression risk by grade over time"),
       subtitle = "Percentage of teams featuring same comtribution grade reporting and/or more than 30% of roles within one grade level of the manager",
       x = "Date",
       y = "Percentage of compressed teams") 




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
