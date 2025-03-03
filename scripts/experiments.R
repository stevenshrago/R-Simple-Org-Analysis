## EXPERIMENTS ----

data_full |> # lululemon averages
  filter(vacancy == "No") |>
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
  summarise(ll_exec_perc = percent(exec / (exec + non), digits = 1))

execs <- data_full |>
  filter(report_effective_date == "2024-11-01") |>
  mutate(
    exec = if_else(str_detect(compensation_grade_wkr, "E"), "exec", "non")
  ) |>
  group_by(report_effective_date, exec) |>
  summarise(n = n()) |>
  filter(exec == "exec") |>
  pull(n)

# Current, excluding vacancies 2.5% (2% = 109)
data_full |>
  filter(report_effective_date == "2024-11-01", vacancy == "No") |>
  group_by()
summarise(n = n()) |>
  mutate(n = n - execs, exec_perc = percent(execs / n, digits = 2))

# Current, including vacancies 2.3% (2% = 120)
data_full |>
  filter(
    report_effective_date == "2024-11-01",
    # vacancy == "No"
  ) |>
  summarise(n = n()) |>
  mutate(n = n - execs, exec_perc = percent(execs / n, digits = 2))


# Future reduction of 100 roles, +8 VPs, excluding vacancies
data_full |>
  filter(report_effective_date == "2024-11-01", vacancy == "No") |>
  summarise(n = n()) |>
  mutate(
    n = (n - 100) - (execs + 8),
    exec_perc = percent(execs / n, digits = 2)
  )


# Future reduction of 100 roles, +8 VPs, including vacancies
data_full |>
  filter(
    report_effective_date == "2024-11-01",
    # vacancy == "No"
  ) |>
  summarise(n = n()) |>
  mutate(
    n = (n - 100) - (execs + 8),
    exec_perc = percent(execs / n, digits = 2)
  )


## Data packs  ----

slt_areas <- c(
  "Americas and Global Guest Innovation (Celeste Burgoyne)",
  "CFO (Meghan Frank)",
  "People & Culture (Susan Gelinas)",
  "Design and Merchandising (Sun Choe)",
  "Supply Chain (Ted Dagnese)",
  "Brand & Creative Content (Nikki Neuburger)",
  "Technology (Julie Averill)",
  "International (Andre Maestrini)",
  "Legal (Shannon Higginson)"
)

# Low spans

for (i in slt_areas) {
  data_full |>
    filter(
      report_effective_date == "May 2024",
      supervisory_organization_level_2 == i
    ) |>
    filter(direct_reports > 0 & direct_reports < 4) |>
    select(
      supervisory_organization_level_3,
      position_id_worker,
      worker_name,
      compensation_grade,
      direct_reports
    ) |>
    arrange(
      supervisory_organization_level_3,
      desc(compensation_grade),
      desc(direct_reports)
    ) |>
    write_excel_csv(glue("data_out/low_spans_{i}.csv"))
}

# Compression

for (i in slt_areas) {
  data_full |>
    filter(
      report_effective_date == "May 2024",
      supervisory_organization_level_2 == i,
      comp_grade_overlap == comp_grade_overlap_mgr
    ) |>
    select(
      supervisory_organization_level_3,
      position_id_worker,
      worker_name,
      compensation_grade,
      position_id_manager,
      worker_name_mgr,
      compensation_grade_mgr
    ) |>
    arrange(desc(supervisory_organization_level_3), desc(compensation_grade)) |>
    write_excel_csv(glue("data_out/compression_{i}.csv"))
}


# Project managers

for (i in slt_areas) {
  data_full |>
    filter(
      report_effective_date == "May 2024",
      supervisory_organization_level_2 == i,
      str_detect(job_title, "(P|p)ro(g|j)")
    ) |>
    select(
      supervisory_organization_level_3,
      position_id_worker,
      worker_name,
      job_title
    ) |>
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |>
    write_excel_csv(glue("data_out/project_managers_{i}.csv"))
}


# Analysts

for (i in slt_areas) {
  data_full |>
    filter(
      report_effective_date == "May 2024",
      supervisory_organization_level_2 == i,
      str_detect(job_title, "(A|a)naly")
    ) |>
    select(
      supervisory_organization_level_3,
      position_id_worker,
      worker_name,
      job_title
    ) |>
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |>
    write_excel_csv(glue("data_out/analysts_{i}.csv"))
}


# Product Managers

for (i in slt_areas) {
  data_full |>
    filter(
      report_effective_date == "May 2024",
      supervisory_organization_level_2 == i,
      str_detect(job_title, "(P|r)oduct Manager")
    ) |>
    select(
      supervisory_organization_level_3,
      position_id_worker,
      worker_name,
      job_title
    ) |>
    arrange(desc(supervisory_organization_level_3), desc(job_title)) |>
    write_excel_csv(glue("data_out/product_managers_{i}.csv"))
}


data_full |>
  filter(
    report_effective_date == "May 2024",
    # str_detect(job_title, "(C|c)hange"),
    # job_family == "Project Management",
    # str_detect(job_title, "Program", negate = TRUE),
    str_detect(job_title, "(S|s)trategic")
  ) |>
  select(
    supervisory_organization_level_3,
    position_id_worker,
    worker_name,
    job_title,
    job_family
  ) |>
  arrange(desc(supervisory_organization_level_3), desc(job_title)) |>
  print(n = 40)
