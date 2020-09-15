# Training
tapply(mtcars[['mpg']], mtcars[['cyl']], mean)

function_standard <- function(x, y){
  if (x > y) {
    z <- x
  } else {
    z <- y
  }
 
  return(z) 
}
function_standard(10,20)


library(dplyr)
function_dplyr <- function(x,y){
  z <- if_else(x > y, x, y)
  return(z)
}
function_dplyr(10,20)


# Lets get back to the grouping followed by an averaging .
mtcars

norm_dplyr <- function(vector, method = "L1"){
  
  # Give your error message when there is an error with the argument 
  if(!(method %in% c("L1", "L2"))){
    return("Method must be either 'L1' or 'L2'")
  }
  
  L1 <- vector %>% abs() %>% sum() 
  L2 <- vector^2 %>% sum() %>% sqrt()
  norm_ <- ifelse(method == "L1", 
                  L1,
                  L2)
  return(norm_)
  
}

A <- c(3, 4,5,6,7)
norm_dplyr(A, "L2")


##################### Similarity score ############################
source("Functions_RShiny.R")
B <- Agg_filter_OECDasylum(Migrationdata,
                    origin = "Syria",
                    destination = "Germany",
                    Year_from = 2005,
                    Year_till = 2018)
A <- Agg_filter_migration(Migrationdata,
                     origin = "Syria",
                     destination = "Germany",
                     Year_from = 2005,
                     Year_till = 2018)
C <- Agg_UNHCR_asylum(asylum_applications_UNHCR,
                 "Syrian Arab Rep.",
                 "Germany")

GT <- GT_dataframe(serach_terms = 'Germany', origin_code = 'SY',
         language = "ar",
         category = 555)

A
B
GT[2:12,]$Approach_1_hits
A$migration_flow_total
## Function for similarity
norm_dplyr <- function(TS1, TS2, method = "L2"){
  
  # Give your error message when there is an error with the argument 
  if(!(method %in% c("L1", "L2"))){
    return("Method must be either 'L1' or 'L2'")
  }
  
  # L1 <- vector %>% abs() %>% sum() 
  L2 <- (TS1 - TS2)^2 %>% sum() %>% sqrt()
  norm_ <- ifelse(method == "L2", 
                  L2,
                  L1)
  return(norm_)
  
}

norm_dplyr(A$migration_flow_total,GT[2:13,]$Approach_1_hits)

########################################## Prediction ########################
DT_GT
library("tseries")
library("forecast")
tsdata <- DT_GT %>% ungroup() %>% 
  select(migration_flow_total) %>% 
  ts(start = 2006)

fit <- auto.arima(tsdata,
                  trace=T, 
                  stepwise = T,
                  stationary=T,
                  approximation=FALSE,
                  seasonal = F,
                  lambda = 0)

checkresiduals(fit)
arima_forcast100 <- forecast::forecast(fit, h = 3, lambda = 0, biasadj = TRUE)
autoplot(tsdata) + autolayer(arima_forcast100)



