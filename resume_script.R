library(tidyverse)
library(stringr)
library(plotly)
library(xml2)
library(rvest)
library(lubridate)
library(forcats)
source("helper_functions.R")

#annotate lexRankr pkg release
lexRankr_release <- as.Date("2016-07-01")

# read in resume and data
# convert yyyy-mm chr dates to yyyy-mm-01 dates
# set dates passed as "current" to Sys.Date()
set.seed(42)
resume_data <- read_csv("resume.csv",
                        col_types = "ccccclc") %>% 
  mutate(job_i = rank(start_date,"first")) %>% 
  mutate(start_date = convert_dates(start_date)) %>% 
  mutate(end_date   = convert_dates(end_date)) %>% 
  mutate(month = map2(start_date, end_date, ~seq(.x,.y,by="1 month"))) %>% 
  unnest() %>% 
  select(-start_date, -end_date) %>% 
  # mutate(data = .2*job_i+sample(seq(.1,.5,length.out=nrow(.)))) %>% 
  nest(-month) %>% 
  mutate(month_i = row_number()) %>% 
  unnest()

project_df <- read_csv("projects.csv", col_types = "ccccl") %>% 
  mutate(date = convert_dates(date)) %>% 
  rename(detail_proj = detail) %>% 
  mutate(proj_i = rank(date,"first")) %>% 
  right_join(resume_data %>% 
              ungroup() %>% 
              mutate(organization=as.character(organization)),
            by=c("date"="month", "organization", "education")) %>% 
  mutate(detail_proj = ifelse(detail_proj=="proprietary",
                              "",
                              detail_proj)) %>% 
  mutate(proj_i = myImpute(proj_i)) %>%
  mutate(data=proj_i + sample(seq(-.5,.5,length.out=nrow(.)))) %>% 
  mutate(organization = as.factor(organization)) %>% 
  mutate(organization = fct_reorder(organization, job_i))
  

colors <- resume_data %>% 
  group_by(organization,color) %>% 
  summarise() %>% 
  ungroup() %>% 
  .[["color"]]

time_labels <- resume_data %>% 
  ungroup() %>% 
  filter(str_sub(month, 6,7) == "01") %>%
  mutate(year=str_sub(month,1,4)) %>% 
  select(month_i, month, year)

plot_ly(project_df,
        x      = ~month_i,
        y      = ~data,
        color  = ~organization,
        colors = colors,
        type   = "scatter",
        mode   = "lines",
        line   = list(width=3),
        hoverinfo = "text",
        text = ~paste0(paste0(organization),
                       "<br>",
                       paste0(position),
                       ifelse(is.na(detail),
                              "",
                              paste0("<br>",detail)))) %>% 
  add_trace(project_df,
            x     = ~month_i,
            y     = ~proj_i,
            showlegend = FALSE,
            name  = "projects",
            line = list(
              color="black",
              dash="dash"
              ),
            opacity=.3,
            hoverinfo="none") %>% 
  layout(title="",
         xaxis=list(
           title="",
           tickmode="array",
           tickvals=time_labels$month_i,
           ticktext=time_labels$year,
           tickangle = 45
         ),
         yaxis=list(
           range = ~c(min(data)-2, 
                      max(data)+2),
           title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE
         ),
         margin=list(b=75),
         annotations=list(x = ~month_i[which(date==lexRankr_release)],
                          y = ~data[which(date==lexRankr_release)],
                          text = "lexRankr released on CRAN",
                          xref = "x",
                          yref = "y",
                          showarrow = TRUE,
                          arrowhead = 1,
                          ax = -40,
                          ay = -30))

plot_ly(resume_data,
        x      = ~month_i,
        y      = ~data,
        color  = ~organization,
        colors = colors,
        type   = "scatter",
        mode   = "lines",
        line   = list(width=3),
        hoverinfo = "text",
        text = ~paste0(paste0(organization),
                       "<br>",
                       paste0(position),
                       ifelse(is.na(detail),
                              "",
                              paste0("<br>",detail)))) %>% 
  layout(title="",
         xaxis=list(
           title="",
           tickmode="array",
           tickvals=time_labels$month_i,
           ticktext=time_labels$year,
           tickangle = 45
           ),
         yaxis=list(
           range = ~c(min(data)-2, 
                      max(data)+2),
           title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE
         ),
         margin=list(b=75),
         annotations=list(x = ~month_i[which(month==lexRankr_release)],
                          y = ~data[which(month==lexRankr_release)],
                          text = "lexRankr released on CRAN",
                          xref = "x",
                          yref = "y",
                          showarrow = TRUE,
                          arrowhead = 1,
                          ax = -40,
                          ay = -30)) %>% 
  add_trace(x    = project_df$month_i,
            y    = project_df$project_count,
            type = "scatter",
            mode = "lines")


