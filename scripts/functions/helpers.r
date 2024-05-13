## Plot themes  ----

# remove gridlines, and axis titles and lines
theme_clean_lulu <- function(th){ 
  
  th <- theme(axis.text.x = element_blank(),
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
              title = element_text(family = lulu_font, size = 18, face = "bold", color = offblack),
              plot.subtitle = element_text(family = lulu_font, size = 12, color = offblack),
              strip.background = element_rect(fill = neutral_4),
              strip.text = element_text(family = lulu_font, size = 8, face = "bold", color = offblack))
  
  return (th)
}

# Standard axis text
standard_text_x <- function(th, size = 12, bold = TRUE ) {
  
  if (bold == "TRUE") {
    th <- theme(axis.text.x = element_text(size = size, 
                                           family = lulu_font, 
                                           colour = offblack,
                                           face = "bold"))
  } else if (bold == "FALSE") {
    th <- theme(axis.text.x = element_text(size = size, 
                                           family = lulu_font, 
                                           colour = offblack))
  }
  
  return(th)
  
}

standard_text_y <- function(th, size = 10, bold = TRUE ) {
  
  if (bold == "TRUE") {
    th <- theme(axis.text.y = element_text(size = size, 
                                           family = lulu_font, 
                                           colour = offblack,
                                           face = "bold"))
  } else if (bold == "FALSE") {
    th <- theme(axis.text.y = element_text(size = size, 
                                           family = lulu_font, 
                                           colour = offblack))
  }
  
  return(th)
  
}

standard_legend <- function(th) {
  
  
  th <- theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(family = lulu_font, color = offblack, size = 12),
              legend.background = element_rect(fill = offwhite))
  
}

standard_bar_lims <- function(th, lo = 0, hi = 1.1) {
  
  th <- ylim(c(lo, hi))
  
}


standard_line_lims <- function(th, lo = 0.3, hi = 1) {
  
  th <- ylim(c(lo, hi))
  
}


# lulu Font
lulu_font <- "Saans"

# lulu colours
offwhite <-  "#fffff5"
offblack <-  "#1e140e"
hotheat <- "#ff4546"

blue = "#6094ef"
yellow = "#f2ff91"
green = "#6bb2a0"

neutral_1 = "#130a0a"
neutral_2 = "#493c3b"
neutral_3 = "#8c8077"
neutral_4 = "#ddd8d1"
neutral_5 = "#efeeec"
neutral_6 = "#fffff2"

# geom_label and geom_text sizes

linegraph_label_size = 7 #Values on the line itself
linegraph_text_size = 5 #Legend for lines
bargraph_text_size = 12
bargraph_text_size_s = 8


