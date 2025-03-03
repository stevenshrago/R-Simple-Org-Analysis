## Plot themes  ----

font_add(
  family = "saans",
  regular = "/Users/sshrago/Library/Fonts/Saans-Regular.otf",
  bold = "/Users/sshrago/Library/Fonts/Saans-Bold.otf"
)

showtext::showtext_auto()

# remove gridlines, and axis titles and lines
theme_clean_lulu <- function(th) {
  th <- theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.caption = element_text(family = lulu_font, size = 10),
    title = element_text(
      family = lulu_font,
      size = 22,
      color = offblack,
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = lulu_font,
      size = 16,
      color = offblack
    ),
    strip.background = element_rect(fill = neutral_1),
    strip.text = element_text(
      family = lulu_font,
      size = 8,
      face = "bold",
      color = offblack
    )
  )

  return(th)
}

# Standard axis text
standard_text_x <- function(th, size = 12, bold = TRUE) {
  if (bold == "TRUE") {
    th <- theme(
      axis.text.x = element_text(
        size = size,
        family = lulu_font,
        colour = offblack,
        face = "bold"
      )
    )
  } else if (bold == "FALSE") {
    th <- theme(
      axis.text.x = element_text(
        size = size,
        family = lulu_font,
        colour = offblack
      )
    )
  }

  return(th)
}

standard_text_y <- function(th, size = 10, bold = TRUE) {
  if (bold == "TRUE") {
    th <- theme(
      axis.text.y = element_text(
        size = size,
        family = lulu_font,
        colour = offblack,
        face = "bold"
      )
    )
  } else if (bold == "FALSE") {
    th <- theme(
      axis.text.y = element_text(
        size = size,
        family = lulu_font,
        colour = offblack
      )
    )
  }

  return(th)
}

standard_legend <- function(th) {
  th <- theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = lulu_font, color = offblack, size = 12),
    legend.background = element_rect(fill = offwhite)
  )
}

standard_bar_lims <- function(th, lo = 0, hi = 1.1) {
  th <- ylim(c(lo, hi))
}


standard_line_lims <- function(th, lo = 0.3, hi = 1) {
  th <- ylim(c(lo, hi))
}


# lulu Font
lulu_font <- "saans"

# lulu colours
offwhite <- "#fdfdf8"
offblack <- "#140f0f"
hotheat <- "#ff4546"
neutral_1 <- "#efeeec"
neutral_2 <- "#c8c2b8"
neutral_3 <- "#7f746c"
neutral_4 <- "#2b1f1e"

dark_green <- "#142a0e"
blue <- "#c0ddff"
pale_green <- "#f3fed7"
yellow <- "#f4ff8e"
dark_brown <- "#524a43"


# geom_label and geom_text sizes

linegraph_label_size = 7 #Values on the line itself
linegraph_text_size = 5 #Legend for lines
bargraph_text_size = 12
bargraph_text_size_s = 8


grade_levels <- c(
  "B1",
  "B2",
  "B3",
  "B4",
  "C1",
  "C2",
  "C3",
  "C4",
  "C5",
  "C6",
  "C7",
  "M1",
  "M2",
  "M3",
  "M4",
  "M5",
  "E1",
  "E2",
  "E3",
  "E4"
) # set grade levels for each com grade

grade_levels_overlap <- c(
  "B1",
  "B2",
  "B3-C1",
  "B4-C2",
  "C3-M1",
  "C4-M2",
  "C5-M3",
  "C6-M4",
  "C7-M5",
  "E1",
  "E2",
  "E3",
  "E4"
) # set grade levels for overlapping grades

grade_scoring <- tibble(
  compensation_grade = c(
    "B1",
    "B2",
    "B3",
    "B4",
    "C1",
    "C2",
    "C3",
    "C4",
    "C5",
    "C6",
    "C7",
    "M1",
    "M2",
    "M3",
    "M4",
    "M5",
    "E1",
    "E2",
    "E3",
    "E4"
  ),
  grade_score = c(
    1,
    2,
    3,
    4,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13
  )
)


slt_lookup <- tibble(
  worker_name = c(
    "Celeste Burgoyne",
    "Susan Gelinas",
    "Shannon Higginson",
    "Ted Dagnese",
    "Sun Choe",
    "Meghan Frank",
    "Julie Averill",
    "Nikki Neuburger",
    "Andre Maestrini"
  ),
  missing_data = c(
    "Americas and Global Guest Innovation (Celeste Burgoyne)",
    "People & Culture (Susan Gelinas)",
    "Legal (Shannon Higginson)",
    "Supply Chain (Ted Dagnese)",
    "Design and Merchandising (Sun Choe)",
    "CFO (Meghan Frank)",
    "Technology (Julie Averill)",
    "Brand & Creative Content (Nikki Neuburger)",
    "International (Andre Maestrini)"
  )
)

remove_fields <- c(
  "vacancy",
  "leave_on_leave",
  "position_id_worker",
  "currently_active",
  "is_manager"
)


root <- "83-093759"

find_all_subordinates <- function(manager_id, employees_df) {
  subordinates <- manager_id
  new_subordinates <- manager_id

  while (length(new_subordinates) > 0) {
    new_subordinates <- employees_df$position_id_worker[
      employees_df$position_id_manager %in% new_subordinates
    ]
    subordinates <- c(subordinates, new_subordinates)
  }

  unique(subordinates)
}


clean_dates <- function(df) {
  df %>%
    mutate(
      report_effective_date = lubridate::as_date(report_effective_date),
      readable_date = format(report_effective_date, "%B %Y")
    )
}

adjust_manager <- function(df, worker_id = "83-093759") {
  df %>%
    mutate(
      position_id_manager = if_else(
        position_id_worker == worker_id,
        NA_character_,
        position_id_manager
      )
    )
}


clean_comp_grade <- function(df) {
  df %>%
    mutate(
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
    )
}


fix_sup_org <- function(df) {
  df %>%
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
        str_detect(
          supervisory_organization_level_3,
          "MIRROR & Digital Fitness"
        ) ~
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
}


#  Function to extract OTH flagged roles
get_oth_clean <- function(df, mult_pos) {
  mult_pos |>
    inner_join(df, by = c("report_effective_date", "position_id_workday")) |>
    filter(str_detect(position_id_worker, "OTH"))
}

# Function to remove OTH roles and adjust worker IDs
remove_oth_roles <- function(df, mult_pos) {
  df |>
    anti_join(
      mult_pos,
      by = c("report_effective_date", "position_id_workday")
    ) |>
    bind_rows(get_oth_clean(df, mult_pos)) |>
    mutate(
      position_id_worker = if_else(
        str_detect(position_id_worker, "OTH"),
        position_id_workday,
        position_id_worker
      )
    )
}

# Function to calculate direct reports
add_direct_reports <- function(df, orig_df) {
  df |>
    left_join(
      orig_df |>
        count(report_effective_date, position_id_manager),
      by = c(
        "report_effective_date",
        "position_id_worker" = "position_id_manager"
      )
    ) |>
    rename(direct_reports = n)
}

# Function to add manager information
add_manager_info <- function(df, orig_df) {
  orig_manager_info <- orig_df |>
    select(
      report_effective_date,
      readable_date,
      position_id_worker,
      position_id_manager,
      job_title,
      worker_name,
      compensation_grade,
      grade_score
    )

  df |>
    left_join(
      orig_manager_info,
      by = c(
        "report_effective_date",
        "position_id_manager" = "position_id_worker"
      ),
      suffix = c("_wkr", "_mgr")
    ) |>
    mutate(
      same_grade_reports = if_else(
        grade_score_wkr == grade_score_mgr,
        "Yes",
        NA
      )
    )
}

# Function to create alerts
create_alerts <- function(df) {
  df |>
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
        gap_raw %in% c(2, 3) ~ "ok",
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
}

# Function to merge alerts back into the main manager dataset
merge_alerts <- function(manager_df, alerts_df) {
  manager_df |>
    left_join(
      alerts_df |>
        select(
          report_effective_date,
          position_id_manager,
          alert_successors,
          alert_compression,
          alert_gaps
        ),
      by = c(
        "report_effective_date",
        "position_id_worker" = "position_id_manager"
      )
    )
}
