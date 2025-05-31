# renv --------------------------------------------------------------------
print("setting virtual environment...")

#renv::restore()

# Packages required -------------------------------------------------------
print("loading packages...")
library(readr, include.only = "read_csv")
library(renv)
# library(rtweet) - the package was discontinued in 2024
library(tidyverse)
library(tidytext)
library(qdapDictionaries)
library(lubridate)
library(showtext)
library(ggdark)
library(grid, include.only = "rasterGrob") # if the background_image method fails
library(png, include.only = "readPNG")
library(ggforce, include.only = "facet_zoom")
library(ggrepel)
library(patchwork)
library(flextable)
library(textdata) 
library(RCurl) # Download PNG

# visuals -----------------------------------------------------------------
print("setting fonts...")

font_add_google("Proza Libre", "Proza")
font_add_google("Inconsolata", "Inconsolata")
font_add_google("Inconsolata", "InconsolataBold")
showtext_auto()


# theme_fnc ---------------------------------------------------------------
# does not cover all arguments of theme but this is sufficient for the current use
print("setting theme...")

theme_100daysofwriting <- function(default_size = 15, title_family = "Proza", text_family = "Inconsolata") {
  sub_size <- round(default_size / 1.14, digits = 0)
  axis_size <- round(default_size / 1.33, digits = 0)
  label_size <- round(default_size / 2, digits = 0)

  dark_theme_minimal() +
    theme(
      plot.title = element_text(
        family = title_family, hjust = .5, size = default_size,
        margin = margin(t = default_size * 0.5),
        vjust = 1
      ),
      plot.subtitle = element_text(family = title_family, hjust = .5, size = sub_size),
      axis.text = element_text(family = text_family, size = axis_size),
      axis.title = element_text(family = text_family, face = "bold", size = default_size),
      axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = label_size),
      axis.text.y = element_text(margin = margin(r = 0.5), size = label_size),
      plot.background = element_rect(fill = "grey10"),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey30", size = 0.2, linetype = "longdash"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(
        size = rel(0.9), hjust = 0, vjust = 1,
        margin = margin(t = default_size * 0.5)
      ),
      plot.caption.position = "panel"
    )
}

wrap_caption <- function(text, max_width = 100) {
  lapply(text, function(x, width = max_width) {
    x <- strwrap(x, width, simplify = FALSE)
    vapply(x, paste, character(1), collapse = "\n")
  })
}

capitalisation_labeller <- function(x) {
  if (!is.null(x)) {
    x <- tolower(x)
    s <- tools::toTitleCase(x)
    return(s)
  }
}

labs_100daysofwriting <- function(x = NULL, y = NULL, title = NULL,
                                  subtitle = NULL, caption = NULL,
                                  caption_width = 100,
                                  tag = NULL) {
  labs(
    x = capitalisation_labeller(x),
    y = capitalisation_labeller(y),
    title = capitalisation_labeller(title),
    subtitle = capitalisation_labeller(subtitle),
    caption = wrap_caption(text = caption, max_width = caption_width),
    tag = tag
  )
}
