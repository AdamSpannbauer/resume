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
