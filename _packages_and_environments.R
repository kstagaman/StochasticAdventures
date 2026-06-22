### Packages
library(here)
library(autoNumCaptions)
library(RColorBrewer)
library(grid)
library(rlang)
library(shiny)
library(bslib)
library(bsicons)
library(magrittr)
library(stringr)
library(ggplot2)
library(cowplot)
library(ggbeeswarm)
library(ggstats)
library(patchwork)
library(purrr)
library(data.table)

### Housekeeping ###
dirs <- list(
  figs = here("Figures"),
  posts = here("Posts"),
  data = here("Data"),
  temp = here("Temp")
)
for (dir in dirs) {
  if (!dir.exists(dir)) { dir.create(dir) }
}

if (file.exists(here("tracker.rda"))) {
  load(here("tracker.rda"))
} else {
  tracker <- new.env()
  tracker$posts <- data.table()
}
post.files <- list.files(path = "Posts", full.names = TRUE, pattern = ".Rmd$")
post.info <- file.info(post.files)
if (length(post.files) > nrow(tracker$posts)) {
  tracker$posts <- data.table(
    Post.file = basename(post.files[order(post.info$ctime)]),
    Created = sort(post.info$ctime),
    Post.num = seq_along(post.files)
  )
  setkey(tracker$posts, Post.file)
}
save(tracker, file = here("tracker.rda"))

### Aesthetics
theme_set(theme_cowplot())

my.theme <- theme_update(
  legend.position = "top",
  legend.box.just = "left",
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 9),
  legend.justification = "left",
  legend.key.size = unit(0.7, "line"),
  plot.caption = element_text(hjust = 0, size = 8),
  axis.text = element_text(size = 8),
  axis.title = element_text(size = 10)
)



