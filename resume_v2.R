library(tidyverse)
library(stringr)
library(forcats)
library(plotly)
source("helper_functions.R")

#annotate lexRankr pkg release
lexRankr_release <- as.Date("2016-07-01")

########################################
# read in resume data and clean data
########################################
resume_data <- read_csv("resume.csv", col_types = "ccccclc") %>%
  # index jobs by start date (tie breaker goes to lowest row num & non-zero)
  mutate(job_i = rank(start_date,"first")) %>% 
  # convert dates from YYYY-MM chr to YYYY-MM-DD date
  mutate(start_date = convert_dates(start_date)) %>% 
  mutate(end_date   = convert_dates(end_date))

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
# simulate time series data for each org and get on same timeline
########################################
# get timeline of all months at each org
org_timelines <- resume_data %>% 
  # get full list of months at each job and expand to 1 row per month/org
  mutate(months = map2(start_date, 
                       end_date, 
                       ~seq(.x,.y,by="1 month"))) %>%
  select(organization, months) %>% 
  spread(organization, months) %>% 
  as.list() %>% 
  map(1)

# simulate data by sampling from uniform
# name each time series as org name
set.seed(42)
org_sim_ts <- map2(org_timelines,
                   names(org_timelines),
                   ~tibble(date     = .x,
                           sim_data = runif(length(.x),min = -.5,max=.5)) %>% 
                     set_names(c("date", .y)))

#make full month timeline of all orgs dates
min_date <- min(resume_data$start_date)
max_date <- max(resume_data$end_date)
full_timeline <- seq(min_date, max_date, by="1 month")

# df of overkill info on date for convenience
full_timeline_df <- tibble(date    = full_timeline,
                           yyyymm  = str_sub(full_timeline, end=7),
                           yyyy    = str_sub(full_timeline, end=4),
                           mm      = str_sub(full_timeline, 6, 7)) %>% 
  mutate(month_i = row_number())

# reduce all simulated org ts data to single dataframe on single timeline
# create imputed version of project timeline to center org data around proj count
plot_df <- reduce(org_sim_ts, 
                  left_join, by="date", 
                  .init=resume_ts_df) %>% 
  left_join(project_df, by=c("date"="month")) %>% 
  mutate(proj_i_imp = myImpute(proj_i))

# center all org simulated data around imputed project count
plot_df[,names(org_timelines)] <- plot_df[,names(org_timelines)]+plot_df$proj_i_imp

########################################
# plot resume
########################################
# will only label years in plot x axis
# extract month index for all januarys and get year for label
time_labels <- full_timeline_df %>% 
  filter(date >= year_ceiling(min(date))) %>% 
  filter(date <= year_floor(max(date))) %>% 
  filter(mm == "01")

no_annotations_plotly <-
  plot_ly(plot_df, 
        x = ~month_i,
        y = ~proj_i,
        connectgaps=TRUE,
        type = "scatter",
        mode = "lines+markers",
        name = "Data Projects",
        line = list(color="black", 
                    dash="dot"),
        hoverinfo="text",
        text = ~paste0(project,"<br>",detail_proj)) %>% 
  add_org_traces(resume_data) %>% 
  layout(title="",
         xaxis=list(
           title="",
           tickmode="array",
           tickvals=time_labels$month_i,
           ticktext=time_labels$yyyy,
           tickangle = 45
         ),yaxis=list(
           title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE
         ))

no_annotations_plotly

########################################
# add manual annotations for certain projects
########################################
lexRankr_release <- as.Date("2016-07-01")
annotated_plotly <- no_annotations_plotly %>%
  layout(
    annotations=list(x = ~month_i[which(date==lexRankr_release)],
                     y = ~proj_i_imp[which(date==lexRankr_release)],
                     text = "lexRankr released on CRAN",
                     xref = "x",
                     yref = "y",
                     showarrow = TRUE,
                     arrowhead = 1,
                     ax = -100,
                     ay = -40,
                     align = "left")
    )

annotated_plotly
