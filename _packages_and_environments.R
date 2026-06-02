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
