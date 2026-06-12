library(rlang)
library(this.path)
library(shiny)
library(bslib)
library(bsicons)
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(cowplot)
library(ggbeeswarm)
library(RColorBrewer)
library(ggstats)
library(purrr)

# library(ffbase)
theme_set(theme_cowplot())

my.theme <- theme_update(
  legend.position = "top",
  legend.box.just = "left",
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 10),
  legend.justification = "left",
  legend.key.size = unit(0.7, "line"),
  plot.caption = element_text(hjust = 0, size = 8)
)

if (file.exists("tracker.rda")) {
  load("tracker.rda")
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

save(tracker, file = "tracker.rda")
