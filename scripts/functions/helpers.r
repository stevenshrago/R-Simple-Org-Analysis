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
