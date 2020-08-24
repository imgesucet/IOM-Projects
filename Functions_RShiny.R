# Here I updated and added some features/plots/data structure into 
# my old function which we can use for our RShiny app in the future.
# Contact me if you have any questions- Murtiza.Nurmamat

# normalize to scale 0-100
normalized <- function(x){
  ((x-min(x))/(max(x)-min(x)))*100
}



####################  Total migration OECD  ####################
# Migration total - OECD
Migrationdata <- read.csv("migrationData.csv")

# Function for Aggregating and filtering the Migration data.
Agg_filter_migration <- function(mig_data, origin,
                                destination, Year_from, Year_till){
  # Detect the Inflows and aggregate
  Migrationdata_agg <- mig_data %>%  
    filter(str_detect(Variable, "^Inflows")) %>% 
    group_by(Country.of.birth.nationality, Country, Year) %>% 
    summarise(Value = sum(Value))
  # Filter by country of origin and destination
  mig_data_1 <- Migrationdata_agg %>%  
    filter(Country.of.birth.nationality == origin,
           Country == destination,
           Year > Year_from, Year < Year_till)
  # Normalize the migration data into 0-100 scale.
  mig_data_2 <- mig_data_1 %>%  ungroup() %>% 
    select(Year, Value) %>%
    mutate_if(is.integer, as.character) %>% 
    mutate(migration_flow_total = normalized(Value)) %>% 
    select(-Value)
  
  return(mig_data_2)
  
}

######################### Asylum OECD   ####################
# Function for Aggregating and filtering the Asylum-OECD
Agg_filter_OECDasylum <- function(mig_data, origin,
                                 destination, Year_from, Year_till){
  # Detect the Inflows and aggregate
  Asylum_OEDC <-  mig_data %>% 
    filter(str_detect(Variable, "^Inflows of asylum")) %>% 
    group_by(Country.of.birth.nationality, Country, Year) %>% 
    summarise(Value = sum(Value))
  
  # Filter by country of origin and destination
  Asylum_OEDC_1 <- Asylum_OEDC %>%  
    filter(Country.of.birth.nationality == origin,
           Country == destination,
           Year > Year_from, Year < Year_till)
  
  # Normalize the migration data into 0-100 scale.
  Asylum_OEDC_2 <- Asylum_OEDC_1 %>%  ungroup() %>% 
    select(Year, Value) %>%
    mutate_if(is.integer, as.character) %>% 
    mutate(Asylum_OEDC = normalized(Value)) %>% 
    select(-Value)
  
  return(Asylum_OEDC_2)
  
}
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

########################## Asylum - UNHCR ###################
asylum_applications_UNHCR<-read_csv("asylum-applications_data.csv")


Agg_UNHCR_asylum<- function(Asylum_UNHCR, origin_asylum,
                            destination_asylum){
  
  # Detect the Inflows and aggregate
  Asylum_UNHCR_agg<-Asylum_UNHCR%>%
    group_by(`Country of origin`,`Country of asylum`,Year) %>%
    summarise(Value_asylum=sum(applied))
  
  # Filter by country of origin and destination
  aslyum_UNHCR_1 <- Asylum_UNHCR_agg %>%  
    filter(`Country of origin` == origin_asylum,
           `Country of asylum` == destination_asylum)
  
  # Normalize the migration data into 0-100 scale.
  aslyum_UNHCR_2 <- aslyum_UNHCR_1 %>% ungroup() %>% 
    select(Year, Value_asylum) %>%
    mutate(Year = as.character(Year),
           Asylum_UNHCR = normalized(Value_asylum)) %>% 
    select(-Value_asylum)
  
  return(aslyum_UNHCR_2)
  
}

C <- Agg_UNHCR_asylum(asylum_applications_UNHCR,
                 "Syrian Arab Rep.",
                 "Germany")



######################### Google trends data ######################

# Function for aggregating the GT data as yearly basis.
Agg_yearly <- function(GTdata){
  
  Yearly_data <- GTdata[[1]]  %>% 
    mutate(Year =str_extract(date, "^.{4}")) %>% 
    mutate(hits = gsub("[^0-9.-]", "", hits)) %>% 
    group_by(Year, geo) %>% 
    summarise(hits_average = mean(as.numeric(hits))) %>% ungroup()
  
  return(Yearly_data)
  
  
}



GT_dataframe <- function(serach_terms = NA, origin_code,
                                language = "en",
                                category =0){
  
  # Make a dataframe first.
  
  # Approach 1 (Destination as keywords, category as 555)
  GT_1<-gtrends(keyword = serach_terms,geo=c(origin_code), 
                category = category,
                time='2005-01-01 2017-12-31',hl=language,
                gprop = "web", onlyInterest = TRUE)
  GT_1_agg <- Agg_yearly(GT_1) %>% ungroup() %>%
    select(Year, hits_average) %>% 
    rename(Approach_1_hits = hits_average)
  # Approach 2 (Destination as keywords, without category)
  GT_2<-gtrends(keyword = serach_terms,geo=c(origin_code),
                time='2005-01-01 2017-12-31',hl=language,
                gprop = "web", onlyInterest = TRUE)
  GT_2_agg <- Agg_yearly(GT_2) %>% ungroup() %>%
    select(Year, hits_average) %>% 
    rename(Approach_2_hits = hits_average)
  # Aprroach 3 (Category as 555 Without any keywords)
  GT_3<-gtrends(geo=c(origin_code), 
                category = category,
                time='2005-01-01 2017-12-31',hl=language,
                gprop = "web", onlyInterest = TRUE)
  GT_3_agg <- Agg_yearly(GT_3) %>% ungroup() %>%
    select(Year, hits_average) %>% 
    rename(Approach_3_hits = hits_average)
  
  GT_data <- GT_1_agg %>% 
    left_join(GT_2_agg, by = "Year") %>% 
    left_join(GT_3_agg, by = "Year")
  
  return(GT_data)
}

D <- GT_dataframe(serach_terms = 'Germany', origin_code = 'SY',
         language = "ar",
         category = 555)




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
