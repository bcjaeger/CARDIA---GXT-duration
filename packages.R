## library() calls go here
# library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)

## data prep/presentation
library(haven)
library(tidyverse)
library(table.glue)
library(glue)
library(flextable)
library(gtsummary)
library(Hmisc)
library(ggdist)

# Multiple imputation
library(miceRanger)

## data analysis
library(lme4)
library(rsample)
library(lmerTest)
library(splines)
library(ggeffects)
library(broom)
library(rms)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lmer", "lmerTest")
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("as_flextable", "flextable")
conflicted::conflict_prefer("lag", "dplyr")
