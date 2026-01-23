if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(lme4, marginaleffects, memoise, vegan, tidycomm, furrr, here, kableExtra,  fontawesome, tinythemes, tidyverse)

theme_set(tinythemes::theme_ipsum_rc()+
            theme(legend.position = "top", plot.title.position = "plot",
                  plot.margin = margin(t = 0,  # Top margin
                                       r = 10,  # Right margin
                                       b = 0,  # Bottom margin
                                       l = 0))
)

options(marginaleffects_safe = FALSE)
plan(multicore)