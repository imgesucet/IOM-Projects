# There are some functions I built for facilitating the repetitive work.
# Do not hesitate to ask me if there is any question. -Murtiza

# Function for aggregating the GT data as yearly basis.
Agg_yearly <- function(GTdata){
  
  Yearly_data <- GTdata[[1]]  %>% 
    mutate(Year =str_extract(date, "^.{4}")) %>% 
    mutate(hits = gsub("[^0-9.-]", "", hits)) %>% 
    group_by(Year, keyword,geo) %>% 
    summarise(hits_average = mean(as.numeric(hits))) %>% ungroup()
  
  return(Yearly_data)
  
  
}

# Funciton for Migration data filtering
Filtering_migration <- function(mig_data, origin,
                                destination, Year_from, Year_till){
  
  mig_data_New <- mig_data %>%  
    filter(Country.of.birth.nationality == origin,
           Country == destination,
           Year > Year_from, Year < Year_till)
  
  return(mig_data_New)
  
}

# Generate the comparison plot
Generate_comparison <- function(serach_terms, origin_code,
                                language, origin,destination){

  # GT data
  GT<-gtrends(serach_terms,geo=c(origin_code),
                       time='2004-01-01 2017-12-31',hl=language,
                       gprop = "web", onlyInterest = TRUE)
  
  GT_year <- Agg_yearly(GT)  %>%
    ungroup() %>% 
    select(Year, hits_average)
  # Migration data
  Migration <- Filtering_migration(Migrationdata_agg,
                               origin,
                               destination, 
                               Year_from = 2005,
                               Year_till = 2018)
  Migration_1 <- Migration %>%  select(Year, Value) %>% 
    mutate_if(is.integer, as.character)
  
  # combine data
  Migration_GT_bind <- Migration_1 %>% 
    left_join(GT_year, by = "Year")

  p <- ggplot(data=Migration_GT_bind, aes(x=Year)) +
    geom_line(aes(y=Value),color = "blue", group = 1) +
    labs(y="Migration Inflow")
  # GT data
  q <- ggplot(data=Migration_GT_bind, aes(x=Year)) +
    geom_line(aes(y = hits_average), color = "red",group = 1) +
    labs(y = "Yearly search hits")
  
  ggarrange(p, q, 
            labels = c("Migration flow", "Search hits"),
            ncol = 1, nrow = 2)

}

