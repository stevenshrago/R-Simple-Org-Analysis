library(pacman)
p_load(
  tidyverse,
  janitor,
  glue,
  datapasta,
  scales,
  formattable,
  showtext,
  WriteXLS,
  explore,
  readxl,
  lubridate,
  tidygraph,
  ggraph,
  shadowtext,
  gghighlight
)

source("scripts/functions/helpers.r")


## Load data  ----

# data_orig <- readxl::read_xlsx("data_in/3ydata.xlsx", sheet = 2, .name_repair = make_clean_names) # read in the orgiinal excel file and standardize the variable names (snake case)

data_orig <- readxl::read_xlsx(
  "data_in/quarterly_data_original_w_country_q4.xlsx",
  sheet = 2,
  .name_repair = make_clean_names
)


## Clean & prepare data ----

# Still need to figure out a simple way of determining depth

data_clean <- data_orig |>
  mutate(
    report_effective_date = lubridate::as_date(report_effective_date),
    readable_date = format(report_effective_date, "%B %Y"),
    position_id_manager = if_else(
      position_id_worker == "83-093759",
      NA_character_,
      position_id_manager
    ),
    compensation_grade = str_remove_all(compensation_grade, "L|N|P|S|T|I"),
    compensation_grade = factor(compensation_grade, levels = grade_levels),
    compensation_grade_name = case_when(
      compensation_grade == "M2" ~ "Manager",
      compensation_grade == "M3" ~ "Senior Manager",
      compensation_grade == "M4" ~ "Director",
      compensation_grade == "M5" ~ "Senior Director",
      compensation_grade == "E1" ~ "VP",
      compensation_grade == "E2" ~ "SVP",
      compensation_grade == "E3" ~ "EVP",
      compensation_grade == "E4" ~ "SLT",
      .default = "Individual Contributor"
    )
  ) |>
  left_join(grade_scoring, by = "compensation_grade") |>
  filter(leave_on_leave == "No")


oth_clean <- data_clean |> # Identifies all OTH flagged roles for removal from the dataset
  count(report_effective_date, position_id_workday) |>
  filter(n > 1) |>
  select(-n) |>
  inner_join(
    data_clean,
    by = c("report_effective_date", "position_id_workday")
  ) |>
  filter(str_detect(position_id_worker, "OTH"))

data_clean_oth <- data_clean |> # Removes OTH roles
  anti_join(
    data_clean |>
      count(report_effective_date, position_id_workday) |>
      filter(n > 1) |>
      select(-n),
    by = c("report_effective_date", "position_id_workday")
  ) |>
  bind_rows(oth_clean) |>
  mutate(
    position_id_worker = if_else(
      str_detect(position_id_worker, "OTH"),
      position_id_workday,
      position_id_worker
    )
  )


data_clean_drs <- data_clean_oth |> # Calculates direct report numbers to people managers
  left_join(
    data_clean |>
      count(report_effective_date, position_id_manager),
    by = c(
      "report_effective_date",
      "position_id_worker" = "position_id_manager"
    )
  ) |>
  rename(direct_reports = n)

data_clean_managers <- data_clean_drs |> # Adds DR information to dataset
  left_join(
    data_clean |>
      select(
        report_effective_date,
        readable_date,
        position_id_worker,
        position_id_manager,
        job_title,
        worker_name,
        compensation_grade,
        grade_score
      ),
    by = c(
      "report_effective_date",
      "position_id_manager" = "position_id_worker"
    ),
    suffix = c("_wkr", "_mgr")
  ) |>
  mutate(
    same_grade_reports = if_else(grade_score_wkr == grade_score_mgr, "Yes", NA)
  )


alerts <- data_clean_managers |> # Creates flags for compression and gaps
  select(
    report_effective_date,
    position_id_worker,
    compensation_grade_wkr,
    grade_score_wkr,
    position_id_manager,
    compensation_grade_mgr,
    grade_score_mgr
  ) |>
  mutate(
    gap_raw = grade_score_mgr - grade_score_wkr,
    gap_comment = case_when(
      gap_raw == 0 ~ "Compressed",
      gap_raw == 1 ~ "Successor",
      gap_raw == 2 | gap_raw == 3 ~ "ok",
      gap_raw > 3 ~ "Gap"
    )
  ) |>
  arrange(position_id_manager) |>
  count(report_effective_date, position_id_manager, gap_comment) |>
  group_by(report_effective_date, position_id_manager) |>
  mutate(percentage = percent(n / sum(n), digits = 0)) |>
  select(-n) |>
  pivot_wider(
    id_cols = c(report_effective_date, position_id_manager),
    names_from = gap_comment,
    values_from = percentage
  ) |>
  replace_na(list(
    ok = percent(0, digits = 0),
    Gap = percent(0, digits = 0),
    Successor = percent(0, digits = 0),
    Compressed = percent(0, digits = 0)
  )) |>
  mutate(
    alert_successors = if_else(
      Successor == percent(0, digits = 0),
      "No successor",
      NA
    ),
    alert_compression = if_else(
      Successor >= percent(0.33, digits = 0) |
        Compressed > percent(0, digits = 0),
      "Team compression",
      NA
    ),
    alert_gaps = if_else(Gap >= percent(0.25, digits = 0), "Team gaps", NA)
  )


data_clean_alerts <- data_clean_managers |> # Add compression and gaops flags to main dataset
  left_join(
    alerts |>
      select(
        report_effective_date,
        position_id_manager,
        alert_successors:alert_gaps
      ),
    by = c(
      "report_effective_date",
      "position_id_worker" = "position_id_manager"
    )
  )


dates_formatted <- data_clean_alerts |>
  distinct(report_effective_date) |>
  mutate(readable = format(report_effective_date, "%b %Y")) |>
  arrange(report_effective_date) |>
  pull(readable)

## Data fixes  ----

data_clean_alerts_fixed <- data_clean_alerts |> # various fixes for changes to the organization over time - mainly L2 and L3
  filter(str_detect(
    supervisory_organization_level_2,
    "MIRROR|CEO",
    negate = TRUE
  )) |>

  mutate(
    supervisory_organization_level_2 = case_when(
      supervisory_organization_level_2 ==
        "Supply Chain, Distribution, Sourcing (Ted Dagnese)" ~
        "Supply Chain (Ted Dagnese)",
      supervisory_organization_level_2 ==
        "Design and Merchandising (Sun Choe)" |
        supervisory_organization_level_2 ==
          "Creative - Design and Concept (Jonathan Cheung)" ~
        "Design (Jonathan Cheung)",

      .default = supervisory_organization_level_2
    ),

    supervisory_organization_level_3 = case_when(
      supervisory_organization_level_3 ==
        "Raw Material Developments - Advanced Materials Innovation (Yogendra Dandapure)" ~
        "Raw Materials Innovation (Yogendra Dandapure)",
      supervisory_organization_level_3 ==
        "Quality Assurance, Strategy, Testing & Compliance (Rene Wickham)" ~
        "Product Integrity (Rene Wickham)",
      supervisory_organization_level_3 == "Quality (Rene Wickham)" ~
        "Product Integrity (Rene Wickham)",
      supervisory_organization_level_3 ==
        "Raw Material Developments (Patty Stapp)" ~
        "Global Raw Materials (Patty Stapp)",
      supervisory_organization_level_3 ==
        "Supply Chain Strategy & Planning (Ravi Chowdary)" ~
        "Supply Chain Strategy & Planning (Andrew Polins)",

      supervisory_organization_level_3 == "Talent Acquisition (Steve Clyne)" ~
        "Talent Acquisition (Marc Wendorf)",
      supervisory_organization_level_3 ==
        "People & Culture Operations (Susan Gelinas (Inherited))" ~
        "People & Culture Operations (Mandy Whiting)",
      str_detect(
        supervisory_organization_level_3,
        "Business Partnering - International"
      ) ~
        "Business Partnering – Global Corporate Functions (Jacqueline Misshula)",
      str_detect(
        supervisory_organization_level_3,
        "Business Partnering - SSC"
      ) ~
        "Business Partnering - Product & Supply Chain (Stewart Angus)",

      supervisory_organization_level_3 == "Ecommerce (Justin Richmond)" ~
        "Ecommerce (Danny Ryder)",
      supervisory_organization_level_3 ==
        "Member Engagement (Madeline Thompson)" ~
        "Member Engagement (Jiamei Bai)",
      supervisory_organization_level_3 == "Member Engagement (TJ Whitmell)" ~
        "Member Engagement (Jiamei Bai)",
      str_detect(supervisory_organization_level_3, "MIRROR & Digital Fitness") ~
        "MIRROR & Digital Fitness",

      str_detect(
        supervisory_organization_level_3,
        "Financial Planning & Analysis"
      ) ~
        "Financial Planning & Analysis",
      str_detect(supervisory_organization_level_3, "Strategic Finance") ~
        "Financial Planning & Analysis",
      str_detect(
        supervisory_organization_level_3,
        "Financial Reporting Accounting"
      ) ~
        "Finance, Accounting and Tax (Alex Grieve)",

      str_detect(supervisory_organization_level_3, "Integrated GTM") ~
        "Business Transformation (Ana Badell)",
      str_detect(supervisory_organization_level_3, "Operations Excellence") ~
        "Business Transformation (Ana Badell)",

      supervisory_organization_level_3 ==
        "Strategy and Ops (Andrea Heckbert, Christine Turner (On Leave))" ~
        "Strategy and Ops (Christine Turner)",

      supervisory_organization_level_3 ==
        "Technology Security (Alexander Padilla)" ~
        "Cyber Defense & Incident Response (Brian Seaford)",
      supervisory_organization_level_3 ==
        "Technology Security (Brian Seaford)" ~
        "Cyber Defense & Incident Response (Brian Seaford)",

      supervisory_organization_level_3 == "Tech Priorities (Diane Cañate)" ~
        "Strategy & Operations (Diane Cañate)",
      supervisory_organization_level_3 ==
        "Global Digital Technology (Jamie Turnbull)" ~
        "Global Digital Technology (Jebin Zacharia)",

      supervisory_organization_level_3 ==
        "Product & Distribution Technology (Deb Huntting)" ~
        "Global Technology Services (Venki Krishnababu)",
      supervisory_organization_level_3 ==
        "Technology Enterprise Planning and Operations (Justin Walton)" ~
        "Global Technology Services (Venki Krishnababu)",

      .default = supervisory_organization_level_3
    ),

    supervisory_organization_level_4 = case_when(
      supervisory_organization_level_4 ==
        "Global Workplace Design (Greg Smith (Inherited))" ~
        "Global Workplace Design (Carol Waldmann)",
      supervisory_organization_level_4 ==
        "Facility Implementation (Shaleena Uppal)" ~
        "Global Workplace Design (Carol Waldmann)",
      supervisory_organization_level_4 ==
        "Facility Implementation (Greg Smith (Inherited))" ~
        "Global Workplace Design (Carol Waldmann)",
      .default = supervisory_organization_level_4
    )
  )


## Prepare data for analysis ----

data_clean_alerts_fixed |>
  tabyl(supervisory_organization_level_2, report_effective_date) |>
  view()

data_full <- data_clean_alerts_fixed |> # full dataset filtered for 13 month view
  filter(report_effective_date >= "2024-02-02")

focus_target <- "International (Andre Maestrini)"

data_focus <- data_full |> # dataset ready for filtering by L2 or L3
  filter(supervisory_organization_level_2 == focus_target)

title_level <- "supervisory_organization_level_2"
so_level <- "supervisory_organization_level_3"

(title_name <- data_focus |>
  distinct(.data[[title_level]]) |>
  pull(.data[[title_level]]))


## 1 Manager Percentages (by L3 area) ----

data_focus |>
  group_by(report_effective_date, .data[[so_level]], is_manager) |> #group and count number of non-/managers by sup org and date
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> # fill in blank/NA
  pivot_wider(
    id_cols = c(report_effective_date, .data[[so_level]]),
    names_from = is_manager,
    values_from = n
  ) |> # reshape
  mutate(Yes = Yes + 1) |> # adjustment to count the manager of the sup org in the numbers
  summarize(perc = percent(Yes / (Yes + No), digits = 0)) |> # percetages of non-/managers
  drop_na(.data[[so_level]]) |>
  filter(perc > 0) |> # filter non-zero percentages

  left_join(
    data_full |> # calculate lululemon overall percentages
      group_by(report_effective_date, is_manager) |>
      summarise(n = n()) |>
      mutate(is_manager = replace_na(is_manager, "No")) |>
      pivot_wider(
        id_cols = c(report_effective_date),
        names_from = is_manager,
        values_from = n
      ) |>
      summarize(ll_perc = percent(Yes / (Yes + No), digits = 0)),
    by = c("report_effective_date")
  ) |>

  right_join(
    data_focus |> # next few lines remove records for historical sup orgs that no longer exist in 2024
      distinct(report_effective_date, .data[[so_level]]) |>
      expand(report_effective_date, .data[[so_level]])
  ) |>
  ungroup() |>
  mutate(
    trim = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        is.na(perc),
      .data[[so_level]],
      NA
    )
  ) |>
  filter(!.data[[so_level]] %in% trim) |>
  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot(aes(x = report_effective_date, y = perc)) +
  geom_rect(
    aes(
      xmin = 0,
      xmax = length(unique(data_focus$report_effective_date)) + 1,
      ymin = 0.15,
      ymax = .25
    ),
    fill = neutral_1
  ) +
  geom_line(
    aes(y = ll_perc, group = .data[[so_level]]),
    color = offblack,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_line(aes(group = .data[[so_level]]), color = hotheat, linewidth = 1) +
  facet_wrap(~ .data[[so_level]]) +
  geom_hline(yintercept = 0.15, color = neutral_3, linetype = "dotted") +
  geom_hline(yintercept = 0.25, color = neutral_3, linetype = "dotted") +
  geom_shadowtext(
    aes(label = label),
    color = offwhite,
    bg.colour = offblack,
    family = lulu_font,
    fontface = "bold",
    size = 5
  ) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Percentage of line managers - {title_name}"),
    subtitle = "Line managers as a percentage of roles over time vs lululemon average and external benchmark (15-25%)",
    x = "Date",
    y = "Manager perentage",
    caption = "Excludes leaves and non-SSC workforce."
  )

###### 1.1 Manager percentages (L2/SLT area)  ----

data_full |>
  group_by(
    report_effective_date,
    supervisory_organization_level_2,
    is_manager
  ) |> #group and count number of non-/managers by sup org and date
  summarize(n = n()) |>
  mutate(is_manager = replace_na(is_manager, "No")) |> # fill in blank/NA
  pivot_wider(
    id_cols = c(report_effective_date, supervisory_organization_level_2),
    names_from = is_manager,
    values_from = n
  ) |> # reshape
  mutate(Yes = Yes + 1) |> # adjustment to count the manager of the sup org in the numbers
  summarize(perc = percent(Yes / (Yes + No), digits = 0)) |> # percetages of non-/managers
  drop_na(supervisory_organization_level_2) |>
  filter(perc > 0) |> # filter non-zero percentages

  bind_rows(
    data_full |> # calculate lululemon overall percentages
      group_by(report_effective_date, is_manager) |>
      summarise(n = n()) |>
      mutate(is_manager = replace_na(is_manager, "No")) |>
      pivot_wider(
        id_cols = c(report_effective_date),
        names_from = is_manager,
        values_from = n
      ) |>
      summarize(perc = percent(Yes / (Yes + No), digits = 0)) |>
      mutate(supervisory_organization_level_2 = "lululemon average")
  ) |>

  mutate(
    high_ok_low = case_when(
      perc >= 0.25 ~ "high", # categorization of percentage levels against best practice
      perc <= 0.15 ~ "low",
      .default = "ok"
    ),
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        supervisory_organization_level_2 == focus_target,
      perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  #filter(report_effective_date != "Aug 2023") |>

  ggplot(aes(
    x = report_effective_date,
    y = perc,
    group = supervisory_organization_level_2
  )) +
  # annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 0.15, ymax = .25, fill = neutral_1) +
  geom_line(linewidth = 1, color = hotheat) +
  gghighlight(
    supervisory_organization_level_2 == focus_target,
    line_label_type = "ggrepel_text",
    label_params = list(color = offwhite),
    unhighlighted_params = list(colour = neutral_1)
  ) +
  geom_hline(yintercept = 0.15, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.25, color = neutral_3, linetype = "dashed") +
  annotate(
    "text",
    x = 1,
    y = 0.255,
    label = "Upper benchmark range 25%",
    family = lulu_font,
    colour = neutral_2
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.155,
    label = "Lower benchmark range 15%",
    family = lulu_font,
    color = neutral_2
  ) +

  geom_shadowtext(
    aes(x = report_effective_date, y = perc, label = label),
    family = lulu_font,
    fontface = "bold",
    size = 7
  ) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Percentage of line managers"),
    subtitle = "Line managers as a percentage of all SSC roles over time vs external benchmark (15-25%)",
    x = "Date",
    y = "Manager perentage",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  scale_fill_manual(
    values = c("high" = hotheat, "ok" = pale_green, "low" = blue)
  ) +
  scale_color_manual(
    values = c("high" = offwhite, "ok" = offblack, "low" = offblack)
  )


## 3 Executive development - all SLT functions only ----

data_full |>
  mutate(
    exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")
  ) |> # classifiy exec roles
  group_by(report_effective_date, supervisory_organization_level_2, exec) |> # group and count exec roles
  summarise(n = n()) |>
  drop_na(exec) |>
  pivot_wider(
    id_cols = c("report_effective_date", "supervisory_organization_level_2"),
    names_from = exec,
    values_from = n
  ) |>
  summarise(exec_perc = percent(exec / (exec + non), digits = 1)) |> # percentages of exec roles

  left_join(
    data_full |> # lululemon averages
      mutate(
        exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")
      ) |>
      group_by(report_effective_date, exec) |>
      summarise(n = n()) |>
      drop_na(exec) |>
      pivot_wider(
        id_cols = c("report_effective_date"),
        names_from = exec,
        values_from = n
      ) |>
      summarise(ll_exec_perc = percent(exec / (exec + non), digits = 1)),
    by = c("report_effective_date")
  ) |>

  mutate(
    vsbm = if_else(exec_perc > 0.02, "high", "ok"),
    ,
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    ),

    # ERROR FIX THIS
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      exec_perc,
      NA
    ),
    label_ll = if_else(
      report_effective_date == max(data_full$report_effective_date),
      ll_exec_perc,
      NA
    )
    # FIX THIS ERROR
  ) |> # classification of ranges for plot
  ggplot() +
  geom_rect(
    aes(
      xmin = 0.5,
      xmax = length(unique(data_full$report_effective_date)) + 0.5,
      ymin = 0,
      ymax = .02
    ),
    fill = neutral_1
  ) +
  geom_hline(aes(yintercept = 0.02), color = neutral_4, linetype = "dotted") +
  geom_line(
    aes(
      x = report_effective_date,
      y = exec_perc,
      group = supervisory_organization_level_2
    ),
    color = offblack,
    size = 1
  ) +
  # geom_line(aes(x = report_effective_date, y = ll_exec_perc, group = supervisory_organization_level_2), color = neutral_3, size = 0.5, linetype = "dashed") +
  # geom_label(aes(x = report_effective_date, y = ll_exec_perc, label = label_ll), fill = offwhite, color = neutral_3, family = lulu_font, label.size = 0) +
  geom_label(
    aes(
      x = report_effective_date,
      y = exec_perc,
      label = label,
      fill = vsbm,
      color = vsbm
    ),
    family = lulu_font,
    fontface = "bold"
  ) +
  labs(
    title = glue("Executive roles percentage"),
    subtitle = "Percentage of VP+ roles vs lululemon average (dotted line) and external benchmark (<=2%)",
    y = "Percentage of VP+ roles",
    x = "Date (quarters)",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  theme_clean_lulu() +
  facet_wrap(~supervisory_organization_level_2) +

  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.07)
  ) +

  scale_fill_manual(values = c("ok" = pale_green, "high" = hotheat)) +
  scale_color_manual(values = c("ok" = offblack, "high" = offwhite))


### 3.1 Executive development (L2 focus/highlight)  ----

data_full |>
  mutate(
    exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")
  ) |> # classifiy exec roles
  group_by(report_effective_date, supervisory_organization_level_2, exec) |> # group and count exec roles
  summarise(n = n()) |>
  drop_na(exec) |>
  pivot_wider(
    id_cols = c("report_effective_date", "supervisory_organization_level_2"),
    names_from = exec,
    values_from = n
  ) |>
  summarise(exec_perc = percent(exec / (exec + non), digits = 1)) |>

  # add lululemon overall average
  bind_rows(
    data_full |>
      mutate(
        exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")
      ) |> # classifiy exec roles
      group_by(report_effective_date, exec) |> # group and count exec roles
      summarise(n = n()) |>
      drop_na(exec) |>
      pivot_wider(
        id_cols = c("report_effective_date"),
        names_from = exec,
        values_from = n
      ) |>
      summarise(exec_perc = percent(exec / (exec + non), digits = 1)) |>
      mutate(supervisory_organization_level_2 = "lululemon average")
  ) |>

  drop_na(supervisory_organization_level_2) |>
  mutate(
    label = if_else(
      supervisory_organization_level_2 == focus_target &
        report_effective_date == max(data_full$report_effective_date),
      exec_perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot(aes(
    x = report_effective_date,
    y = exec_perc,
    group = supervisory_organization_level_2
  )) +

  # geom_rect(aes(xmin = 0.5, xmax = length(unique(data_full$report_effective_date))+0.5, ymin = 0, ymax = .02), fill = neutral_1) +

  geom_hline(aes(yintercept = 0.02), color = neutral_4, linetype = "dashed") +

  geom_line(color = hotheat, size = 1) +
  gghighlight(
    supervisory_organization_level_2 == focus_target,
    line_label_type = "ggrepel_text",
    label_params = list(color = offwhite),
    unhighlighted_params = list(colour = neutral_1)
  ) +
  geom_shadowtext(
    aes(label = label),
    family = lulu_font,
    fontface = "bold",
    size = 7
  ) +

  labs(
    title = glue("Executive roles percentage: {focus_target}"),
    subtitle = "VP+ roles as percentage of all SSC roles vs external benchmark (<=2%)",
    y = "Percentage of VP+ roles",
    x = "Date (quarters)",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 12) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.07)
  )


## 5 Grade development  ----

data_focus |>
  count(
    report_effective_date,
    supervisory_organization_level_2,
    grade_score_wkr
  ) |> # count records by contribution level, date and sup org
  drop_na(grade_score_wkr) |>
  group_by(report_effective_date) |>
  mutate(perc = percent(n / sum(n, na.rm = TRUE), digits = 1)) |> # percentages of each comp level by date

  # Add lululemon average
  left_join(
    data_full |>
      count(report_effective_date, grade_score_wkr) |>
      drop_na(grade_score_wkr) |>
      group_by(report_effective_date) |>
      mutate(ll_perc = percent(n / sum(n, na.rm = TRUE), digits = 1)),
    by = c("report_effective_date", "grade_score_wkr")
  ) |>
  mutate(
    above_ave = if_else(perc > ll_perc, "above", "ok"),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot() +
  geom_line(
    aes(x = report_effective_date, y = perc, group = grade_score_wkr),
    color = offblack,
    linewidth = 1
  ) +
  geom_line(
    aes(x = report_effective_date, y = ll_perc, group = grade_score_wkr),
    color = neutral_3,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_label(
    aes(label = ll_perc, x = report_effective_date, y = ll_perc),
    fill = offwhite,
    color = neutral_3,
    label.size = 0,
    family = lulu_font
  ) +
  geom_label(
    aes(
      label = perc,
      x = report_effective_date,
      y = perc,
      fill = above_ave,
      color = above_ave
    ),
    fontface = "bold",
    family = lulu_font
  ) +
  facet_wrap(~grade_score_wkr) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  labs(
    title = glue("Grade development"),
    subtitle = "Percentage of roles at each grade level over time vs lululemon average",
    x = "Compensation grade levels"
  ) +
  scale_fill_manual(values = c("above" = hotheat, "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite, "ok" = offblack))

###### 5.1 Grade development (L2 specific) ----

# focus on directors, managers and ICs
data_full |>
  filter(supervisory_organization_level_2 == focus_target) |>
  mutate(
    compensation_grade_name = case_when(
      compensation_grade_name %in% c("Manager", "Senior Manager") ~
        "Senior/Manager",
      compensation_grade_name %in% c("Director", "Senior Director") ~
        "Senior/Director",
      compensation_grade_name %in% c("VP", "SVP", "EVP", "SLT") ~ "Executive",
      .default = "Individual Contributor"
    ),
    compensation_grade_name = factor(
      compensation_grade_name,
      levels = c(
        "Executive",
        "Senior/Director",
        "Senior/Manager",
        "Individual Contributor"
      )
    )
  ) |>
  count(report_effective_date, compensation_grade_name) |>
  drop_na(compensation_grade_name) |>
  group_by(report_effective_date) |>
  mutate(
    perc = percent(n / sum(n, na.rm = TRUE), digits = 1),
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) |
        report_effective_date == min(data_full$report_effective_date),
      perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>
  filter(compensation_grade_name != "Executive") |>

  ggplot(aes(x = report_effective_date, y = perc)) +
  geom_line(aes(group = compensation_grade_name), color = hotheat, size = 1) +
  geom_shadowtext(
    aes(label = label),
    fontface = "bold",
    family = lulu_font,
    size = 5
  ) +
  facet_wrap(~compensation_grade_name, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  theme(strip.background = element_rect(fill = neutral_1)) +
  standard_text_x(bold = FALSE) +
  labs(
    title = glue("Grade development: {focus_target}"),
    subtitle = "Percentage of roles at IC, manager and director contribution levels over time",
    x = "Compensation grade levels"
  ) +
  scale_fill_manual(values = c("above" = hotheat, "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite, "ok" = offblack))

# Focus  ICs
data_full |>
  filter(
    compensation_grade_name == "Individual Contributor",
    supervisory_organization_level_2 == focus_target
  ) |>
  count(report_effective_date, compensation_grade_wkr) |>
  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        compensation_grade_wkr %in% c("C1", "C2", "C3"),
      n,
      NA
    )
  ) |>
  mutate(
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot(aes(
    x = report_effective_date,
    y = n,
    group = compensation_grade_wkr
  )) +
  geom_line(color = hotheat, size = 1) +
  gghighlight(
    str_detect(compensation_grade_wkr, "C1|C2|C3"),
    line_label_type = "ggrepel_label",
    label_params = list(family = lulu_font, nudge_x = 0.25),
    unhighlighted_params = list(colour = neutral_1)
  ) +
  geom_shadowtext(aes(label = label), family = lulu_font, size = 7) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 12) +
  standard_text_y(bold = FALSE) +
  labs(
    title = glue("Grade development at individual contributor level"),
    subtitle = "Number of roles at C1, C2 and C3 contribution levels over time",
    x = "Compensation grade levels"
  )


## 6 Low spans over time  ----

data_focus |>
  mutate(
    low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")
  ) |>
  count(report_effective_date, .data[[so_level]], low_span, is_manager) |>
  drop_na(is_manager) |>
  select(-is_manager) |>
  pivot_wider(
    id_cols = c(report_effective_date, .data[[so_level]]),
    names_from = low_span,
    values_from = n
  ) |>
  drop_na(.data[[so_level]]) |>
  mutate(across(low:ok, ~ replace_na(.x, 0))) |>
  mutate(perc = percent(low / (low + ok), digits = 0)) |>

  # Remove old teams
  right_join(
    data_focus |>
      distinct(report_effective_date, .data[[so_level]]) |>
      expand(report_effective_date, .data[[so_level]])
  ) |>
  ungroup() |>
  mutate(
    trim = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        is.na(perc),
      .data[[so_level]],
      NA
    )
  ) |>
  filter(!.data[[so_level]] %in% trim) |>
  drop_na(.data[[so_level]], perc) |>

  left_join(
    data_full |>
      mutate(
        low_span = if_else(
          is_manager == "Yes" & direct_reports <= 3,
          "low",
          "ok"
        )
      ) |>
      count(report_effective_date, low_span, is_manager) |>
      drop_na(is_manager) |>
      select(-is_manager) |>
      pivot_wider(
        id_cols = c(report_effective_date),
        names_from = low_span,
        values_from = n
      ) |>
      mutate(across(low:ok, ~ replace_na(.x, 0))) |>
      mutate(ll_perc = percent(low / (low + ok), digits = 0)),
    by = c("report_effective_date")
  ) |>

  mutate(
    above_ave = if_else(perc > ll_perc, "above", "ok"),
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot(aes(x = report_effective_date, y = perc)) +
  geom_line(aes(group = .data[[so_level]]), color = hotheat, linewidth = 1) +
  geom_line(
    aes(y = ll_perc, group = .data[[so_level]]),
    color = neutral_3,
    linewidth = 0.5,
    linetype = "dotted"
  ) +
  geom_shadowtext(
    aes(label = label),
    color = offwhite,
    bg.colour = offblack,
    family = lulu_font,
    fontface = "bold",
    size = 5
  ) +

  facet_wrap(~ .data[[so_level]]) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Low spans of control - {title_name}"),
    subtitle = "Percentage of managers with 3 or fewer direct reports over time vs lululemon average (dotted line)",
    x = "Compensation grade levels",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  scale_fill_manual(values = c("above" = hotheat, "ok" = offwhite)) +
  scale_color_manual(values = c("above" = offwhite, "ok" = offblack))

###### 6.1 Low spans all lululemon  ----

data_full |>
  mutate(
    low_span = if_else(is_manager == "Yes" & direct_reports <= 3, "low", "ok")
  ) |>
  count(
    report_effective_date,
    supervisory_organization_level_2,
    low_span,
    is_manager
  ) |>
  drop_na(is_manager) |>
  select(-is_manager) |>
  pivot_wider(
    id_cols = c(report_effective_date, supervisory_organization_level_2),
    names_from = low_span,
    values_from = n
  ) |>
  drop_na(supervisory_organization_level_2) |>
  mutate(across(low:ok, ~ replace_na(.x, 0))) |>
  mutate(perc = percent(low / (low + ok), digits = 0)) |>

  bind_rows(
    data_full |>
      mutate(
        low_span = if_else(
          is_manager == "Yes" & direct_reports <= 3,
          "low",
          "ok"
        )
      ) |>
      count(report_effective_date, low_span, is_manager) |>
      drop_na(is_manager) |>
      select(-is_manager) |>
      pivot_wider(
        id_cols = c(report_effective_date),
        names_from = low_span,
        values_from = n
      ) |>
      mutate(across(low:ok, ~ replace_na(.x, 0))) |>
      mutate(
        perc = percent(low / (low + ok), digits = 0),
        supervisory_organization_level_2 = "lululemon average"
      )
  ) |>

  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        supervisory_organization_level_2 == "lululemon average",
      perc,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  filter(report_effective_date != "Aug 2023") |>

  ggplot(aes(
    x = report_effective_date,
    y = perc,
    group = supervisory_organization_level_2
  )) +
  geom_line(color = hotheat, size = 1) +
  gghighlight(
    supervisory_organization_level_2 == "lululemon average",
    line_label_type = "ggrepel_text",
    label_params = list(color = offwhite),
    unhighlighted_params = list(colour = neutral_1)
  ) +
  geom_shadowtext(
    aes(x = report_effective_date, y = perc, label = label),
    family = lulu_font,
    fontface = "bold",
    size = 7
  ) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 12) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Low spans of control"),
    subtitle = "Percentage of people managers with 3 or fewer direct reports over time",
    x = "Compensation grade levels",
    caption = "Excludes leaves and non-SSC workforce."
  )


## 7 Fill rate (L3 area) ----

data_focus |>
  count(report_effective_date, .data[[so_level]], currently_active) |>
  replace_na(list(currently_active = "No")) |>
  group_by(report_effective_date, .data[[so_level]]) |>
  summarise(
    filled = percent(sum(n[currently_active == "Yes"]) / sum(n), digits = 0)
  ) |>
  drop_na(.data[[so_level]]) |>

  left_join(
    data_full |>
      count(report_effective_date, currently_active) |>
      replace_na(list(currently_active = "No")) |>
      group_by(report_effective_date) |>
      summarise(
        ll_filled = percent(
          sum(n[currently_active == "Yes"]) / sum(n),
          digits = 0
        )
      ),
    by = "report_effective_date"
  ) |>

  mutate(
    hi_lo = case_when(
      filled < 0.8 ~ "lo",
      filled > 0.9 ~ "hi",
      .default = "ok"
    ),
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      filled,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  ggplot(aes(x = report_effective_date, group = .data[[so_level]])) +
  geom_rect(
    aes(
      xmin = 0.5,
      xmax = length(unique(data_focus$report_effective_date)) + 0.5,
      ymin = 0.8,
      ymax = .9
    ),
    fill = neutral_1
  ) +
  geom_hline(yintercept = 0.9, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = neutral_3, linetype = "dashed") +
  geom_line(aes(y = filled), color = hotheat, linewidth = 1) +
  # geom_line(aes(y = ll_filled), linetype = "dashed") +
  geom_shadowtext(
    aes(label = filled, y = label, color = hi_lo),
    bg.colour = offblack,
    family = lulu_font,
    fontface = "bold",
    size = 5
  ) +
  facet_wrap(~ .data[[so_level]]) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 8) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Fill rate of the organization over time - {title_name}"),
    subtitle = "Active roles as a percentage of the fully funded organization against ideal target range (80-90% filled)",
    x = "Date",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  scale_fill_manual(
    values = c("lo" = hotheat, "ok" = offwhite, "hi" = yellow)
  ) +
  scale_colour_manual(
    values = c("lo" = hotheat, "ok" = offwhite, "hi" = yellow)
  )


###### 7.1 Fill rate all lululemon  ----

data_full |>
  filter(str_detect(supervisory_organization_level_2, "CEO", negate = TRUE)) |>
  count(
    report_effective_date,
    supervisory_organization_level_2,
    currently_active
  ) |>
  replace_na(list(currently_active = "No")) |>
  group_by(report_effective_date, supervisory_organization_level_2) |>
  summarise(
    filled = percent(sum(n[currently_active == "Yes"]) / sum(n), digits = 0)
  ) |>
  drop_na(supervisory_organization_level_2) |>

  bind_rows(
    data_full |>
      count(report_effective_date, currently_active) |>
      replace_na(list(currently_active = "No")) |>
      group_by(report_effective_date) |>
      summarise(
        filled = percent(sum(n[currently_active == "Yes"]) / sum(n), digits = 0)
      ) |>
      mutate(supervisory_organization_level_2 = "lululemon average")
  ) |>

  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) &
        supervisory_organization_level_2 == "lululemon average",
      filled,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>
  filter(report_effective_date != "Aug 2023") |>

  ggplot(aes(
    x = report_effective_date,
    y = filled,
    group = supervisory_organization_level_2
  )) +
  # geom_rect(aes(xmin = 0.5, xmax = length(unique(data_full$report_effective_date))+0.5, ymin = 0.8, ymax = .9), fill = neutral_1) +
  geom_hline(yintercept = 0.9, color = neutral_3, linetype = "dashed") +
  geom_hline(yintercept = 0.85, color = neutral_3, linetype = "dashed") +
  geom_line(color = hotheat, size = 1) +
  geom_shadowtext(
    aes(label = label, y = filled),
    family = lulu_font,
    fontface = "bold",
    size = 7
  ) +
  gghighlight(
    supervisory_organization_level_2 == "lululemon average",
    line_label_type = "ggrepel_text",
    label_params = list(color = NA),
    unhighlighted_params = list(colour = neutral_1)
  ) +

  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 12) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Fill rate of the organization over time"),
    subtitle = "Active roles as a percentage of the fully funded organization against ideal target range (85-90% filled)",
    x = "Date",
    caption = "Excludes leaves and non-SSC workforce."
  )


## 8 Succession gaps L2 only ----

order <- c("E2", "E1", "M5", "M4")

data_full |>
  filter(compensation_grade_wkr %in% c("M5", "E1", "E2")) |>
  group_by(report_effective_date, compensation_grade_wkr, alert_successors) |>
  summarise(total = n()) |>
  replace_na(list(alert_successors = "Potential successor")) |>
  summarise(
    succession_risk = percent(
      sum(total[alert_successors == "No successor"]) / sum(total),
      digits = 0
    )
  ) |>
  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      succession_risk,
      NA
    ),
    hi_lo = case_when(
      label < 0.25 ~ "lo",
      label >= 0.25 & label < 0.5 ~ "mid",
      label >= 0.5 ~ "hi"
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    ),
    compensation_grade_wkr = factor(compensation_grade_wkr, levels = order),
    compensation_grade_wkr = case_when(
      compensation_grade_wkr == "M5" ~ "Senior Director",
      compensation_grade_wkr == "E1" ~ "VP",
      compensation_grade_wkr == "E2" ~ "SVP"
    ),
    compensation_grade_wkr = factor(
      compensation_grade_wkr,
      levels = c("SVP", "VP", "Senior Director")
    )
  ) |>
  filter(report_effective_date != "Aug 2023") |>
  ggplot(aes(
    x = report_effective_date,
    y = succession_risk,
    group = compensation_grade_wkr
  )) +
  geom_line(colour = hotheat, size = 1) +
  geom_label(
    aes(label = label, fill = hi_lo, color = hi_lo),
    family = lulu_font,
    fontface = "bold",
    size = 5
  ) +
  facet_wrap(~compensation_grade_wkr, ncol = 3) +
  theme_clean_lulu() +

  standard_text_x(bold = FALSE, size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  standard_text_y(bold = FALSE) +
  scale_x_discrete(breaks = c("Nov 2023", "Nov 2024")) +
  # theme(axis.text.y = element_blank()) +
  labs(
    title = glue("Strutural succession risk by grade over time"),
    subtitle = "Percentage of Senior Director+ roles without a direct succession path one grade down from the manager",
    x = "Date",
    y = "Percentage of roles without a clear succession path",
    caption = "Excludes leaves and non-SSC workforce."
  ) +
  scale_fill_manual(
    values = c("lo" = offwhite, "mid" = yellow, "hi" = hotheat)
  ) +
  scale_colour_manual(
    values = c("lo" = offblack, "mid" = offblack, "hi" = offwhite)
  )

### 8.1 Succession gaps  ----

data_focus |>
  filter(compensation_grade_wkr %in% c("M4", "M5", "E1", "E2", "E3", "E4")) |>
  group_by(report_effective_date, compensation_grade_wkr, alert_successors) |>
  summarise(total = n()) |>
  replace_na(list(
    alert_successors = "Potential successor",
    supervisory_organization_level_3 = "Exec leadership"
  )) |>
  summarise(
    succession_risk = percent(
      sum(total[alert_successors == "No successor"]) / sum(total),
      digits = 0
    )
  ) |>
  mutate(
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>
  ggplot(aes(
    x = report_effective_date,
    y = succession_risk,
    group = compensation_grade_wkr
  )) +
  geom_line(colour = neutral_3) +
  geom_label(
    aes(label = succession_risk),
    family = lulu_font,
    fill = yellow,
    fontface = "bold"
  ) +
  facet_wrap(~compensation_grade_wkr) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Strutural succession risk by grade over time"),
    subtitle = "Percentage of Director+ roles without a succession path one grade down from the manager",
    x = "Date",
    y = "Percentage of roles without a clear succession path",
    caption = "Excludes leaves and non-SSC workforce."
  )

### 8.2 Succession gaps detail  ----

data_focus |>
  filter(
    compensation_grade_wkr %in% c("M4", "M5", "E1", "E2", "E3", "E4"),
    report_effective_date == max(data_focus$report_effective_date)
  ) |>
  group_by(
    report_effective_date,
    .data[[so_level]],
    compensation_grade_wkr,
    alert_successors
  ) |>
  summarise(total = n()) |>
  replace_na(list(
    alert_successors = "Potential successor",
    supervisory_organization_level_3 = "Exec leadership"
  )) |>
  summarise(
    succession_risk = percent(
      sum(total[alert_successors == "No successor"]) / sum(total),
      digits = 0
    )
  ) |>
  filter(succession_risk > 0) |>
  mutate(
    compensation_grade_wkr = factor(
      compensation_grade_wkr,
      levels = c("M4", "M5", "E1", "E2", "E3", "E4")
    ),
    hi_lo = case_when(
      succession_risk > 0 & succession_risk <= 0.25 ~ "lo",
      succession_risk > 0.25 & succession_risk <= 0.5 ~ "mid",
      succession_risk > 0.5 & succession_risk <= 0.75 ~ "hi",
      succession_risk > 0.75 ~ "vhi"
    )
  ) |>
  ggplot(aes(
    x = compensation_grade_wkr,
    y = succession_risk,
    group = .data[[so_level]]
  )) +
  geom_bar(
    aes(fill = hi_lo),
    stat = "identity",
    position = position_dodge(),
    colour = offwhite,
    width = 0.9,
    alpha = 0.8
  ) +
  geom_text(
    aes(label = .data[[so_level]], y = succession_risk, color = hi_lo),
    stat = "identity",
    position = position_dodge(width = 0.9),
    family = lulu_font,
    hjust = 1
  ) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "lo" = neutral_2,
      "mid" = neutral_3,
      "hi" = dark_brown,
      "vhi" = hotheat
    )
  ) +
  scale_color_manual(
    values = c(
      "lo" = offblack,
      "mid" = offwhite,
      "hi" = offwhite,
      "vhi" = offblack
    )
  ) +

  labs(
    title = glue("Strutural succession risk"),
    subtitle = "Percentage of Director+ roles without a succession path one grade down from the manager",
    x = "Grade level",
    y = "Percentage of roles without a clear succession path",
    caption = "Excludes leaves and non-SSC workforce."
  )


## 9 Compression/friction ----

data_focus |>
  filter(
    compensation_grade_wkr %in%
      c("M2", "M3", "M4", "M5", "E1", "E2", "E3", "E4")
  ) |>
  group_by(report_effective_date, compensation_grade_name, alert_compression) |>
  summarise(total = n()) |>
  replace_na(list(
    alert_compression = "Ok",
    supervisory_organization_level_3 = "Exec leadership"
  )) |>
  summarise(
    compression_risk = percent(
      sum(total[alert_compression == "Team compression"]) / sum(total),
      digits = 0
    )
  ) |>
  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      compression_risk,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    ),
    compensation_grade_name = factor(
      compensation_grade_name,
      levels = c(
        "Manager",
        "Senior Manager",
        "Director",
        "Senior Director",
        "VP",
        "SVP",
        "EVP",
        "SLT"
      )
    )
  ) |>
  ggplot(aes(
    x = report_effective_date,
    y = compression_risk,
    group = compensation_grade_name
  )) +
  geom_line(colour = hotheat, linewidth = 1) +
  geom_shadowtext(
    aes(label = label),
    color = offwhite,
    bg.colour = offblack,
    family = lulu_font,
    fontface = "bold",
    size = 5
  ) +
  facet_wrap(~compensation_grade_name) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue(
      "Structural compression risk - {title_name}"
    ),
    subtitle = glue(
      "Percentage of teams featuring same contribution grade reporting
    and/or more than 30% of roles within one grade level of the manager"
    ),
    x = "Date",
    y = "Percentage of compressed teams",
    caption = "Excludes leaves. SSC only."
  )


###### 9.1 Compression all lululemon  ----

data_full |>
  filter(compensation_grade_wkr %in% c("M4", "M5", "E1", "E2")) |>
  group_by(
    report_effective_date,
    supervisory_organization_level_2,
    alert_compression
  ) |>
  summarise(total = n()) |>
  replace_na(list(
    alert_compression = "Ok",
    supervisory_organization_level_3 = "Exec leadership"
  )) |>
  summarise(
    compression_risk = percent(
      sum(total[alert_compression == "Team compression"]) / sum(total),
      digits = 0
    )
  ) |>

  bind_rows(
    data_full |>
      filter(compensation_grade_wkr %in% c("M4", "M5", "E1", "E2")) |>
      group_by(report_effective_date, alert_compression) |>
      summarise(total = n()) |>
      replace_na(list(alert_compression = "Ok")) |>
      summarise(
        compression_risk = percent(
          sum(total[alert_compression == "Team compression"]) / sum(total),
          digits = 0
        )
      ) |>
      mutate(supervisory_organization_level_2 = "lululemon average")
  ) |>

  mutate(
    label = if_else(
      report_effective_date == max(data_full$report_effective_date),
      compression_risk,
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  filter(report_effective_date != "Aug 2023") |>
  drop_na(supervisory_organization_level_2) |>
  ggplot(aes(
    x = report_effective_date,
    y = compression_risk,
    group = supervisory_organization_level_2
  )) +
  geom_line(colour = hotheat, size = 1) +
  gghighlight(
    supervisory_organization_level_2 == "lululemon average",
    line_label_type = "ggrepel_text",
    label_params = list(color = NA),
    unhighlighted_params = list(colour = neutral_1)
  ) +
  geom_shadowtext(
    aes(label = label),
    family = lulu_font,
    fill = blue,
    fontface = "bold",
    size = 7
  ) +
  # facet_wrap(~ supervisory_organization_level_2) +
  theme_clean_lulu() +
  standard_text_y(bold = FALSE) +
  standard_text_x(bold = FALSE, size = 12) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = glue("Role compression for Director+ roles"),
    subtitle = glue(
      "Percentage of teams featuring same contribution grade reporting and/or
                       more than 30% of roles within one grade level of the manager"
    ),
    x = "Date",
    y = "Percentage of compressed teams"
  )


## 10 Operating leverage  ----

data_full |>
  tabyl(supervisory_organization_level_2)

"Americas and Global Guest Innovation (Celeste Burgoyne)"
"Brand & Creative Content (Nikki Neuburger)"
"CFO (Meghan Frank)"
"Creative - Design and Concept (Jonathan Cheung)"
"International (Andre Maestrini)"
"Legal (Shannon Higginson)"
"People & Culture (Susan Gelinas)"
"Supply Chain (Ted Dagnese)"
"Technology (Julie Averill)"


data_full |>
  filter(
    # vacancy == "No",
    supervisory_organization_level_2 == "Technology (Julie Averill)",
    str_detect(supervisory_organization_level_3, "Assistants", negate = TRUE)
  ) |>
  group_by(supervisory_organization_level_3, report_effective_date) |>
  summarise(n = n()) |>
  mutate(
    growth_rate = percent((n - lag(n, n = 1)) / lag(n, n = 1), digits = 1)
  ) |>
  drop_na(supervisory_organization_level_3) |>
  left_join(
    data_full |>
      filter(vacancy == "No") |>
      count(report_effective_date) |>
      mutate(
        growth_rate_ll = percent(
          (n - lag(n, n = 1)) / lag(n, n = 1),
          digits = 1
        )
      ),
    by = "report_effective_date"
  ) |>
  mutate(
    team_perc = n.x / n.y,
    weighted_growth = team_perc * growth_rate,
    relative_growth = growth_rate - growth_rate_ll,
    growth_significance = weighted_growth / mean(team_perc, na.rm = TRUE),
    label = if_else(relative_growth > 0, relative_growth, NA)
  ) |>

  ggplot(aes(
    x = report_effective_date,
    y = relative_growth,
    size = team_perc,
    color = supervisory_organization_level_3
  )) +
  geom_line() +
  geom_point(shape = 21, fill = offwhite) +

  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = label),
    size = 4,
    vjust = -1,
    color = offblack,
    fontface = "bold",
    family = lulu_font
  ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  facet_wrap(~supervisory_organization_level_3) +
  # lims(y =c(-0.5, 0.5)) +
  labs(
    title = "Team headcount growth rates relative to overall heacount growth",
    subtitle = "SLT-1 department growth by quarter; line thickness shows relative team size",
    y = "Relative Growth Rate",
    size = "Team Proportion"
  )

# whole org
data_full |>
  filter(str_detect(
    supervisory_organization_level_2,
    "CEO|MIRROR",
    negate = TRUE
  )) |>
  group_by(supervisory_organization_level_2, report_effective_date) |>
  summarise(n = n()) |>
  mutate(
    growth_rate = percent((n - lag(n, n = 1)) / lag(n, n = 1), digits = 1)
  ) |>
  drop_na(supervisory_organization_level_2) |>
  left_join(
    data_full |>
      filter(vacancy == "No") |>
      count(report_effective_date) |>
      mutate(
        growth_rate_ll = percent(
          (n - lag(n, n = 1)) / lag(n, n = 1),
          digits = 1
        )
      ),
    by = "report_effective_date"
  ) |>
  mutate(
    team_perc = n.x / n.y,
    weighted_growth = team_perc * growth_rate,
    relative_growth = growth_rate - growth_rate_ll,
    growth_significance = weighted_growth / mean(team_perc, na.rm = TRUE),
    label = if_else(relative_growth > 0, relative_growth, NA)
  ) |>

  ggplot(aes(
    x = report_effective_date,
    y = relative_growth,
    size = team_perc,
    color = supervisory_organization_level_2
  )) +
  geom_line() +
  geom_point(shape = 21, fill = offwhite) +

  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = label),
    size = 4,
    vjust = -1,
    color = offblack,
    fontface = "bold",
    family = lulu_font
  ) +
  scale_size_continuous(range = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE) +
  standard_text_y(bold = FALSE) +
  facet_wrap(~supervisory_organization_level_2) +
  # lims(y =c(-0.5, 0.5)) +
  labs(
    title = "Team headcount growth rates relative to overall heacount growth",
    subtitle = "SLT-1 department growth by quarter; line thickness shows relative team size",
    y = "Relative Growth Rate",
    size = "Team Proportion"
  )


## 11 People cost creep ----

data_focus |>
  filter(vacancy == "No") |>
  group_by(report_effective_date, supervisory_organization_level_3) |>
  summarise(
    cost = sum(annualized_fully_loaded_cost_usd, na.rm = TRUE),
    hc = n()
  ) |>
  mutate(
    ave_cost = round((cost / hc) / 1000, digits = 0),
    label = if_else(
      report_effective_date == max(data_full$report_effective_date) |
        report_effective_date == min(data_full$report_effective_date),
      glue("${ave_cost}K"),
      NA
    ),
    report_effective_date = factor(
      format(report_effective_date, "%b %Y"),
      levels = dates_formatted
    )
  ) |>

  select(-cost, -hc) |>
  drop_na(supervisory_organization_level_3) |>

  ggplot(aes(
    x = report_effective_date,
    y = ave_cost,
    group = supervisory_organization_level_3
  )) +
  geom_line(color = hotheat, linewidth = 1) +
  geom_shadowtext(aes(label = label), size = 5) +
  facet_wrap(~supervisory_organization_level_3) +
  theme_clean_lulu() +
  standard_text_x(bold = FALSE, size = 10) +
  standard_text_y(bold = FALSE) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "K")) +
  labs(
    title = glue("People cost over time: {focus_target}"),
    subtitle = "Average total cost of employment (CAD), excluding vacant roles",
    y = "Relative Growth Rate",
    size = "Team Proportion",
    caption = "Excluding vacant roles and leaves. SSC roles only."
  )
