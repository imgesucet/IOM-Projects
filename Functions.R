# There are some functions I built for facilitating the repetitive work.
# Do not hesitate to ask me if there is any question.---Murtiza

# Function for aggregating the GT data as yearly basis.
Agg_yearly <- function(GTdata){
  
  Yearly_data <- GTdata[[1]]  %>% 
    mutate(Year =str_extract(date, "^.{4}")) %>% 
    mutate(hits = gsub("[^0-9.-]", "", hits)) %>% 
    group_by(Year, geo) %>% 
    summarise(hits_average = mean(as.numeric(hits))) %>% ungroup()
  
  return(Yearly_data)
  
  
}

# Function for Migration data filtering
Filtering_migration <- function(mig_data, origin,
                                destination, Year_from, Year_till){
  
  mig_data_New <- mig_data %>%  
    filter(Country.of.birth.nationality == origin,
           Country == destination,
           Year > Year_from, Year < Year_till)
  
  return(mig_data_New)
  
}

# Funciton for Assuylum filtering
Filtering_asylum<- function(Asylum_UNHCR_agg, origin_asylum,
                            destination_asylum){
  
  aslyum_data_New <- Asylum_UNHCR_agg %>%  
    filter(`Country of origin` == origin_asylum,
           `Country of asylum` == destination_asylum)
  
  return(aslyum_data_New)
  
}

# Generate the comparison plot
Generate_comparison <- function(serach_terms = NA, origin_code,
                                language = "en", origin,destination,
                                category =0,
                                origin_asylum, destination_asylum){

  # GT data
  GT<-gtrends(keyword = serach_terms,geo=c(origin_code), category = category,
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
  
  # Assuylum data
  Asylum_UNHCR <- Filtering_asylum(Asylum_UNHCR_agg,
                             origin_asylum,
                             destination_asylum)
  Asylum <- Asylum_UNHCR %>%  select(Year, Value_asylum) %>% 
    mutate(Year = as.character(Year))
  
  
  # combine data
  Migration_GT_bind <- Migration_1 %>% 
    left_join(GT_year, by = "Year") %>% 
    left_join(Asylum, by = "Year")

  p <- ggplot(data=Migration_GT_bind, aes(x=Year)) +
    geom_line(aes(y=Value),color = "blue", group = 1) +
    labs(y="Migration Inflow")
  # GT data
  q <- ggplot(data=Migration_GT_bind, aes(x=Year)) +
    geom_line(aes(y = hits_average), color = "red",group = 1) +
    labs(y = "Yearly search hits")
  
  # Assuylum
  l <- ggplot(data=Migration_GT_bind, aes(x=Year)) +
    geom_line(aes(y = Value_asylum), color = "green",group = 1) +
    labs(y = "Assuylum flow")
  
  ggarrange(p, q, l,
            labels = c("Migration flow", "Search hits", "Assuylum application"),
            ncol = 1, nrow = 3)

}

