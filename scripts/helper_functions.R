
# convert yyyy-mm chr dates to yyyy-mm-01 dates
# set dates passed as "current" to Sys.Date()
convert_dates <- function(yyyymm_col) {
  map_chr(yyyymm_col,
          ~ifelse(.x=="current", 
                  as.character(Sys.Date()),
                  paste0(.x,"-01"))) %>% 
    as.Date()
}

# add 'noise' to a vector i.e. add 
# random nums in a range to each elemen in vector
year_ceiling <- function(date) {
  year <- substr(date,1,4) %>% 
    as.numeric()
  next_year <- year + 1
  
  paste0(next_year, "-01-01") %>% 
    as.Date()
}

year_floor <- function(date) {
  year <- substr(date,1,4) %>% 
    as.numeric()

  paste0(year, "-01-01") %>% 
    as.Date()
}

# Custom imputation function that interpolates missing values linearly and
# replaces extreme NAs by the extreme values
myImpute <- function(vec){
  nonNAIds <- which(!is.na(vec))
  if(length(nonNAIds)==0) return(vec)
  
  # Impute extreme values by replacing them with extreme non NA values
  firstNonNA <- nonNAIds[1]
  lastNonNA <- nonNAIds[length(nonNAIds)]
  if(firstNonNA>1){
    vec[1:firstNonNA] <- vec[firstNonNA]
  }
  n <- length(vec)
  if(lastNonNA < n){
    vec[lastNonNA:n] <- vec[lastNonNA]
  }
  
  # Interpolate any remaining missing values linearly
  if(any(is.na(vec))){
    nonNAIds <- which(!is.na(vec))
    vec <- approx(nonNAIds, y = vec[nonNAIds], xout = 1:n)$y
  }
  
  vec
}

add_org_traces <- function(p, resume_data) {
  resume_data <- resume_data %>% 
    arrange(job_i)
  orgs     <- resume_data$organization
  colors   <- resume_data$color
  position <- resume_data$position
  detail   <- resume_data$detail
  
  code_str <- paste0("add_trace(y=~`",orgs,"`,",
                     "mode='lines',",
                     "line=list(color='",colors,"', dash='none'),",
                     "name='", orgs,"',",
                     "hoverinfo = 'text',",
                     "text = ~paste0(paste0('",orgs,"'),",
                     "'<br>',",
                     "paste0('",position,"'),",
                     "ifelse(is.na('",detail,"'),",
                     "'',",
                     "paste0('<br>','",detail,"'))))") %>%
    paste(collapse=" %>% \n")
  
  eval(parse(text=paste0("p %>% ",code_str)))
}

###########
# functions to get cran package downloads
###########
get_pkg_rel_date <- function(pkg) {
    pkg_data <- httr::GET(paste0("http://crandb.r-pkg.org/", pkg, "/all")) %>% 
      httr::content()
    pkg_data$timeline[[1]] %>% 
      as.Date()
}

get_total_pkg_downloads <- function(pkg, from="release", to=Sys.Date()) {
  if (from=="release") from <- get_pkg_rel_date(pkg)
  download_df <- cranlogs::cran_downloads(pkg, from=from, to=to)
  sum(download_df$count)
}

