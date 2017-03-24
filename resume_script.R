library(tidyverse)
library(stringr)
library(forcats)
library(plotly)
source("helper_functions.R")

#annotate lexRankr pkg release
lexRankr_release <- as.Date("2016-07-01")

#why not
set.seed(42)
########################################
# read in resume data and clean data
########################################
resume_data <- read_csv("resume.csv", col_types = "ccccclc") %>%
  # index jobs by start date (tie breaker goes to lowest row num & non-zero)
  mutate(job_i = rank(start_date,"first")) %>% 
  # convert dates from YYYY-MM chr to YYYY-MM-DD date
  mutate(start_date = convert_dates(start_date)) %>% 
  mutate(end_date   = convert_dates(end_date)) %>% 
  # get full list of months at each job and expand to 1 row per month/org
  mutate(month = map2(start_date, 
                      end_date, 
                      ~seq(.x,.y,by="1 month"))) %>% 
  unnest() %>% 
  #drop start & end dates
  select(-start_date, -end_date) %>% 
  # index all months (nesting data by month to account for shared months by mult orgs)
  nest(-month) %>% 
  mutate(month_i = row_number()) %>% 
  unnest()

########################################
# read in project data and clean data
########################################
project_df <- read_csv("projects.csv", col_types = "ccccl") %>% 
  # convert dates from YYYY-MM chr to YYYY-MM-DD date
  mutate(date  = convert_dates(date)) %>% 
  rename(month = date) %>% 
  # index jobs by start date (tie breaker goes to lowest row num & non-zero)
  mutate(proj_i = rank(month,"first")) %>% 
  # blank out detail where == proprietary
  mutate(detail = ifelse(detail=="proprietary","",detail)) %>% 
  # rename proj detail to avoid conflict with resume detail
  rename(detail_proj = detail)

########################################
# join & prep data for plotting
########################################
plot_data <- resume_data %>% 
  # join data on month 
  left_join(project_df, by=c("month", "organization", "education")) %>% 
  mutate(proj_i       = myImpute(proj_i)) %>%
  # create dummy data centered on project line for time series effect
  mutate(simul_data   = proj_i + runif(length(proj_i),-.5,.5)) %>% 
  # make org facter and reorder for appearance in legend/colors
  mutate(organization = as_factor(organization)) %>% 
  mutate(organization = fct_reorder(organization, job_i))

# get colors ordered by org factor level
colors <- plot_data %>% 
  group_by(organization,color) %>% 
  summarise() %>% 
  ungroup() %>% 
  .[["color"]]

# create df of month inds and labels for month and year
# only lable year starts
time_labels <- tibble(month = seq(year_ceiling(min(plot_data$month)),
                                  year_floor(max(plot_data$month)),
                                  by = "1 year")) %>% 
  left_join(plot_data, by=c("month")) %>% 
  select(month_i, month) %>% 
  distinct() %>% 
  mutate(year = str_sub(month, end=4))

plot_ly(plot_data,
        x      = ~month_i,
        y      = ~simul_data,
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
  add_trace(x     = ~month_i,
            y     = ~proj_i,
            name = "Project Count",
            line = list(
              color="black",
              dash="dot"
              ),
            opacity=.3,
            hoverinfo="none",
            showlegend = FALSE) %>% 
  layout(title="",
         xaxis=list(
           title="",
           tickmode="array",
           tickvals=time_labels$month_i,
           ticktext=time_labels$year,
           tickangle = 45
         ),
         yaxis=list(
           range = ~c(min(simul_data)-2, 
                      max(simul_data)+2),
           title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE
         ),
         margin=list(b=75),
         annotations=list(x = ~month_i[which(month==lexRankr_release)],
                          y = ~simul_data[which(month==lexRankr_release)],
                          text = "lexRankr released on CRAN",
                          xref = "x",
                          yref = "y",
                          showarrow = TRUE,
                          arrowhead = 1,
                          ax = -40,
                          ay = -30))
