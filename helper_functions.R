library(parallel)
library(httr)

# convert yyyy-mm chr dates to yyyy-mm-01 dates
# set dates passed as "current" to Sys.Date()
convert_dates <- function(yyyymm_col) {
  map_chr(yyyymm_col,
          ~ifelse(.x=="current", 
                  as.character(Sys.Date()),
                  paste0(.x,"-01"))) %>% 
    as.Date()
}

