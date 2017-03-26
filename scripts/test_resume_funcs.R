library(tidyverse)
library(stringr)
library(forcats)
library(plotly)
library(httr)
library(cranlogs)
source("scripts/helper_functions.R")
source("scripts/resume_function.R")

resume_data  <- read_csv("data/resume.csv", col_types = "ccccclc")
project_data <- read_csv("data/projects.csv", col_types = "ccccl")

plot_resume(resume_data, project_data)

plot_resume_lexRankr_annotation(resume_data,project_data)
