# Here I updated and added some features/plots/data structure into 
# my old function which we can use for our RShiny app in the future.
# Contact me if you have any questions- Murtiza.Nurmamat

###############################################################
library(gtrendsR)
library("dplyr")
library("stringr")
library(ggplot2)
library("tidyverse")
library(ggpubr)
library(shiny)

###############################################################


# rm(list = ls())
# normalize to scale 0-100
normalized <- function(x){
  ((x-min(x))/(max(x)-min(x)))*100
}

# Migration total - OECD
Migrationdata <- read.csv("migrationData.csv")

#Asylum UNHCR
asylum_applications_UNHCR<-read_csv("asylum-applications_data.csv")



# Gtrends countries VS code
Countries_gtrends <- gtrendsR::countries %>%  group_by(country_code) %>% 
  mutate(country_name = first(name)) %>% 
  ungroup() %>% select(-sub_code, -name) %>% 
  distinct() %>% mutate_if(is.factor, as.character)

# UNHCR country name VS OECD country name

UNHCR_country <- asylum_applications_UNHCR %>% select(`Country of origin`, `Country of origin (ISO)`) %>% rename(UNHCR_name = `Country of origin`)
OECD_country <- Migrationdata %>% select(Country.of.birth.nationality, CO2) %>% 
  rename(OECD_name = Country.of.birth.nationality)

Country_migration <- UNHCR_country %>% 
  left_join(OECD_country, by = c("Country of origin (ISO)" = "CO2" )) %>% 
  select(-`Country of origin (ISO)`) %>% 
  distinct()

# Category
Cat <- force(categories)
Cat_interest <- Cat %>% filter(Cat$name %>% str_detect("migration")) %>% 
  distinct() %>% mutate(id = as.integer(id))




####################  Total migration OECD  ####################


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

# Without normalizing #
Agg_filter_migration_without_norm <- function(mig_data, origin,
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
    mutate(migration_flow_total = Value) %>% 
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

# Without normalizing #
Agg_filter_OECDasylum_without_norm <- function(mig_data, origin,
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
    mutate(Asylum_OEDC = Value) %>% 
    select(-Value)
  
  return(Asylum_OEDC_2)
  
}



# B <- Agg_filter_OECDasylum(Migrationdata,
#                     origin = "Syria",
#                     destination = "Germany",
#                     Year_from = 2005,
#                     Year_till = 2018)
# A <- Agg_filter_migration(Migrationdata,
#                      origin = "Syria",
#                      destination = "Germany",
#                      Year_from = 2005,
#                      Year_till = 2018)

########################## Asylum - UNHCR ###################

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
# Without normalizing
Agg_UNHCR_asylum_without_norm<- function(Asylum_UNHCR, origin_asylum,
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
           Asylum_UNHCR = Value_asylum) %>% 
    select(-Value_asylum)
  
  return(aslyum_UNHCR_2)
  
}




# C <- Agg_UNHCR_asylum(asylum_applications_UNHCR,
#                  "Syrian Arab Rep.",
#                  "Germany")
######################## Migration flows Data frame (normalized) ################
Migration_DT <- function(Migrationdata, asylum_applications_UNHCR,
                         origin,destination,
                         Year_from,Year_till,
                         origin_asylum, destination_asylum){
  
  Migration_OECD <- Agg_filter_migration(Migrationdata, origin,
                                         destination, Year_from, Year_till)
  
  Asylum_OECD <- Agg_filter_OECDasylum(Migrationdata, origin,
                                      destination, Year_from, Year_till)
  
  Asuylum_UNHCR <- Agg_UNHCR_asylum(asylum_applications_UNHCR,
                                    origin_asylum,
                                     destination_asylum)
  
  DT <- Migration_OECD %>% 
    left_join(Asylum_OECD, by = "Year") %>% 
    left_join(Asuylum_UNHCR, by = "Year")
  
  return(DT)
}
# 
# DT <- Migration_DT(Migrationdata,asylum_applications_UNHCR,
#              origin = "Syria",destination = "Germany",
#              Year_from = 2005,Year_till = 2018,
#              origin_asylum = "Syrian Arab Rep.",
#              destination_asylum = "Germany")


######################## Migration flows Data frame (without normalized) ################
Migration_DT_without_norm <- function(Migrationdata, asylum_applications_UNHCR,
                         origin,destination,
                         Year_from,Year_till,
                         origin_asylum, destination_asylum){
  
  Migration_OECD <- Agg_filter_migration_without_norm(Migrationdata, origin,
                                         destination, Year_from, Year_till)
  
  Asylum_OECD <- Agg_filter_OECDasylum_without_norm(Migrationdata, origin,
                                       destination, Year_from, Year_till)
  
  Asuylum_UNHCR <- Agg_UNHCR_asylum_without_norm(asylum_applications_UNHCR,
                                    origin_asylum,
                                    destination_asylum)
  
  DT <- Migration_OECD %>% 
    left_join(Asylum_OECD, by = "Year") %>% 
    left_join(Asuylum_UNHCR, by = "Year")
  
  return(DT)
}


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
# 
# A <- GT_dataframe(serach_terms = 'Germany', origin_code = 'SY',
#          language = "ar",
#          category = 555)
# 
# 
# B <- Migration_DT(Migrationdata,asylum_applications_UNHCR,
#                    origin = "Syria",destination = "Germany",
#                    Year_from = 2005,Year_till = 2018,
#                    origin_asylum = "Syrian Arab Rep.",
#                    destination_asylum = "Germany")
# 
# 
# # full data frame
# DT_GT <- B %>% left_join(A, by = "Year")


# Generate the comparison dashboard with normalized migration data

Dashboard_DT <- function(Migrationdata,asylum_applications_UNHCR,
                      origin,destination,
                      Year_from,Year_till,
                      origin_asylum, destination_asylum,
                      serach_terms,
                      origin_code,
                      language ,category){
  
  # Matching the Origin Country code
  origin_code_1 <- Countries_gtrends[which(Countries_gtrends$country_name ==  origin_code),][[1]]
  # Matching the Origin asylum Country code
  origin_asylum_1 <- Country_migration[which(Country_migration$OECD_name ==  origin_asylum),][[1]]
  # Matching the Destination Country code
  destination_asylum_1 <- Country_migration[which(Country_migration$OECD_name ==  destination_asylum),][[1]]
  # Matching the Category code
  category_1 <- Cat_interest[which(Cat_interest$name == category),][[2]]
  
  
  DT <- Migration_DT(Migrationdata, asylum_applications_UNHCR,
                     origin,destination,
                     Year_from,Year_till,
                     origin_asylum_1, destination_asylum_1)
  
  # Google trends
  GT <- GT_dataframe(serach_terms, origin_code_1,
         language, category_1)
  
  # full dataframe
  DT_GT <- DT %>% left_join(GT, by = "Year")
  
  return(DT_GT)
  
  
}

# DT_GT <- Dashboard_DT(Migrationdata, asylum_applications_UNHCR,
#           origin = "Syria",destination = "Germany",
#           Year_from = 2005,Year_till = 2018,
#           origin_asylum = "Syria",
#           destination_asylum = "Germany",
#           serach_terms = 'Germany', origin_code = 'SYRIA',
#           language = "ar",category = "Immigration Policy & Border Issues")




# Generate the comparison dashboard without normalized migration data
Dashboard_DT_without_norm <- function(Migrationdata,asylum_applications_UNHCR,
                         origin,destination,
                         Year_from,Year_till,
                         origin_asylum, destination_asylum,
                         serach_terms,
                         origin_code,
                         language ,category){
  
  # Matching the Origin Country code
  origin_code_1 <- Countries_gtrends[which(Countries_gtrends$country_name ==  origin_code),][[1]]
  # Matching the Origin asylum Country code
  origin_asylum_1 <- Country_migration[which(Country_migration$OECD_name ==  origin_asylum),][[1]]
  # Matching the Destination Country code
  destination_asylum_1 <- Country_migration[which(Country_migration$OECD_name ==  destination_asylum),][[1]]
  # Matching the Category code
  category_1 <- Cat_interest[which(Cat_interest$name == category),][[2]]
  
  
  DT <- Migration_DT_without_norm(Migrationdata, asylum_applications_UNHCR,
                     origin,destination,
                     Year_from,Year_till,
                     origin_asylum_1, destination_asylum_1)
  
  # Google trends
  GT <- GT_dataframe(serach_terms, origin_code_1,
                     language, category_1)
  
  # full dataframe
  DT_GT <- DT %>% left_join(GT, by = "Year")
  
  return(DT_GT)
  
  
}


######################################## Visualization ###########################################
# with normalized
Dashboard_plot <- function(DT_GT){
  # x axis scale
  Scale_x <- scale_x_continuous(breaks = waiver(), n.breaks = 7)
  # Migraiton total
  A <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=migration_flow_total),color = "blue", group = 1) + 
    labs(title ="Migration Inflows", y = "", x= "") + 
    ylim(0,100) + Scale_x
  # Asylum OECD
  B <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Asylum_OEDC),color = "red", group = 1) +
    labs(title ="Asylum OECD", y = "",x= "", size = 8) + ylim(0,100)+ Scale_x
  # Asylum UNHCR
  C <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Asylum_UNHCR),color = "red", group = 1) +
    labs(title ="Asylum UNHCR", y = "",x= "") + 
    ylim(0,100) + xlim(2014,2017)
  
  ######## Google Trends ##########
  # Approach 1
  p <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Approach_1_hits),color = "Green", group = 1) +
    ylim(0,100) 
  # Approach 2
  q <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y = Approach_2_hits), color = "black",group = 1) +
    labs(y = "A2", x= "") + ylim(0,100)
  # Approach 3
  d <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y = Approach_3_hits), color = "navy",group = 1) +
    labs(y = "A3", x= "") + ylim(0,100)
  
  
  ggarrange(A,B,C,
            p +labs(y="A1", x= "")+ Scale_x,
            p +labs(y="", x= "")+ Scale_x,
            p + labs(y="", x= "") + xlim(2014,2017),
            q +labs(y="A2", x= "")+ Scale_x,
            q +labs(y="", x= "")+ Scale_x,
            q + labs(y="", x= "") + xlim(2014,2017),
            d +labs(y="A3", x= "")+ Scale_x,
            d +labs(y="", x= "")+ Scale_x,
            d + labs(y="", x= "") + xlim(2014,2017),
            heights = c(2, 2),
            ncol = 3, nrow = 4)
  }

# Dashboard_plot(DT_GT)
# annotate_figure(Dashboard_plot(DT_GT),
#                 top = text_grob("Visualizing Migration flow and Google Trends", color = "red", face = "bold", size = 16),
#                 bottom = text_grob("Data source: \n OECD \n UNHCR", color = "blue",
#                                    hjust = 0, x = 0, face = "italic", size = 10),
#                 # left = text_grob("", color = "green", rot = 90),
#                 # right = text_grob(bquote("Superscript: (""), rot = 90),
#                 fig.lab = "Syria to Germany", fig.lab.face = "bold"
# )

# without normalized
Dashboard_plot_without_norm <- function(DT_GT){
  # x axis scale
  Scale_x <- scale_x_continuous(breaks = waiver(), n.breaks = 7)
  # Migraiton total
  A <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=migration_flow_total),color = "blue", group = 1) + 
    labs(title ="Migration Inflows", y = "", x= "") + Scale_x +
    ylim(0,5e+5)
  # Asylum OECD
  B <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Asylum_OEDC),color = "red", group = 1) +
    labs(title ="Asylum OECD", y = "",x= "", size = 8) + Scale_x+
    ylim(0,5e+5)
  # Asylum UNHCR
  C <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Asylum_UNHCR),color = "red", group = 1) +
    labs(title ="Asylum UNHCR", y = "",x= "") + xlim(2014,2017)+
    ylim(0,5e+5)
  
  ######## Google Trends ##########
  # Approach 1
  p <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y=Approach_1_hits),color = "Green", group = 1) +
    ylim(0,100) 
  # Approach 2
  q <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y = Approach_2_hits), color = "black",group = 1) +
    labs(y = "A2", x= "") + ylim(0,100)
  # Approach 3
  d <- ggplot(data=DT_GT, aes(x=as.integer(Year))) +
    geom_line(aes(y = Approach_3_hits), color = "navy",group = 1) +
    labs(y = "A3", x= "") + ylim(0,100)
  
  
  ggarrange(A,B,C,
            p +labs(y="A1", x= "")+ Scale_x,
            p +labs(y="", x= "")+ Scale_x,
            p + labs(y="", x= "") + xlim(2014,2017),
            q +labs(y="A2", x= "")+ Scale_x,
            q +labs(y="", x= "")+ Scale_x,
            q + labs(y="", x= "") + xlim(2014,2017),
            d +labs(y="A3", x= "")+ Scale_x,
            d +labs(y="", x= "")+ Scale_x,
            d + labs(y="", x= "") + xlim(2014,2017),
            heights = c(2, 2),
            ncol = 3, nrow = 4)
}
# 
Dashboard_plot_without_norm(DT_GT)


#################################################################################################

Origin_code <- list("World", "State", "Multiple regions", "Afghanistan (AF)", "Albania (AL)",
                 "Algeria (DZ)", "American Samoa (AS)", "Andorra (AD)", "Angola (AO)",
                 "Anguilla (AI)", "Antarctica (AQ)", "Antigua and Barbuda (AG)", "Argentina 
                 (AR)", "Armenia (AM)", "Aruba (AW)", "Australia (AU)", "Austria (AT)", 
                 "Azerbaijan (AZ)", "Bahamas (BS)", "Bahrain (BH)", "Bangladesh (BD)", 
                 "Barbados (BB)", "Belarus (BY)", "Belgium (BE)", "Belize (BZ)", "Benin (BJ)", 
                 "Bermuda (BM)", "Bhutan (BT)", "Bolivia, Plurinational State of (BO)",
                 "Bonaire, Sint Eustatius and Saba (BQ)", "Bosnia and Herzegovina (BA)", 
                 "Botswana (BW)", "Bouvet Island (BV)", "Brazil (BR)", 
                 "British Indian Ocean Territory (IO)", "Brunei Darussalam (BN)", 
                 "Bulgaria (BG)", "Burkina Faso (BF)", "Burundi (BI)", "Cabo Verde (CV)", 
                 "Cambodia (KH)", "Cameroon (CM)", "Canada (CA)", "Cayman Islands (KY)", 
                 "Central African Republic (CF)", "Chad (TD)", "Chile (CL)", "China (CN)", 
                 "Christmas Island (CX)", "Cocos (Keeling) Islands (CC)", "Colombia (CO)", 
                 "Comoros (KM)", "Congo (CG)", "Congo, the Democratic Republic of the (CD)",
                 "Cook Islands (CK)", "Costa Rica (CR)", "Cote d'Ivoire !Côte d'Ivoire (CI)", 
                 "Croatia (HR)", "Cuba (CU)", "Curaçao (CW)", "Cyprus (CY)", "Czechia (CZ)", 
                 "Denmark (DK)", "Djibouti (DJ)", "Dominica (DM)", "Dominican Republic (DO)", 
                 "Ecuador (EC)", "Egypt (EG)", "El Salvador (SV)", "Equatorial Guinea (GQ)", 
                 "Eritrea (ER)", "Estonia (EE)", "Ethiopia (ET)", "Falkland Islands (Malvinas) (FK)", 
                 "Faroe Islands (FO)", "Fiji (FJ)", "Finland (FI)", "France (FR)", "French Guiana (GF)", 
                 "French Polynesia (PF)", "French Southern Territories (TF)", "Gabon (GA)", "Gambia (GM)",
                 "Georgia (GE)", "Germany (DE)", "Ghana (GH)", "Gibraltar (GI)", "Greece (GR)", "Greenland (GL)", 
                 "Grenada (GD)", "Guadeloupe (GP)", "Guam (GU)", "Guatemala (GT)", "Guernsey (GG)", "Guinea (GN)", 
                 "Guinea-Bissau (GW)", "Guyana (GY)", "Haiti (HT)", "Heard Island and McDonald Islands (HM)", 
                 "Holy See (Vatican City State) (VA)", "Honduras (HN)", "Hong Kong (HK)", "Hungary (HU)", "Iceland (IS)", 
                 "India (IN)", "Indonesia (ID)", "Iran, Islamic Republic of (IR)", "Iraq (IQ)", "Ireland (IE)", "Isle of Man (IM)",
                 "Israel (IL)", "Italy (IT)", "Jamaica (JM)", "Japan (JP)", "Jersey (JE)", "Jordan (JO)", "Kazakhstan (KZ)", "Kenya (KE)", 
                 "Kiribati (KI)", "Korea (the Democratic People's Republic of) (KP)", "Korea (the Republic of) (KR)", "Kuwait (KW)", 
                 "Kyrgyzstan (KG)", "Lao People's Democratic Republic (LA)", "Latvia (LV)", "Lebanon (LB)", "Lesotho (LS)", 
                 "Liberia (LR)", "Libya (LY)", "Liechtenstein (LI)", "Lithuania (LT)", "Luxembourg (LU)", "Macao (MO)", 
                 "Macedonia, the former Yugoslav Republic of (MK)", "Madagascar (MG)", "Malawi (MW)", "Malaysia (MY)", 
                 "Maldives (MV)", "Mali (ML)", "Malta (MT)", "Marshall Islands (MH)", "Martinique (MQ)", "Mauritania (MR)",
                 "Mauritius (MU)", "Mayotte (YT)", "Mexico (MX)", "Micronesia, Federated States of (FM)", "Moldova, Republic of (MD)",
                 "Monaco (MC)", "Mongolia (MN)", "Montenegro (ME)", "Montserrat (MS)", "Morocco (MA)", "Mozambique (MZ)", 
                 "Myanmar (MM)", "Namibia (NA)", "Nauru (NR)", "Nepal (NP)", "Netherlands[note 1] (NL)", "New Caledonia (NC)", 
                 "New Zealand (NZ)", "Nicaragua (NI)", "Niger (NE)", "Nigeria (NG)", "Niue (NU)", "Norfolk Island (NF)", 
                 "Northern Mariana Islands (MP)", "Norway (NO)", "Oman (OM)", "Pakistan (PK)", "Palau (PW)",
                 "Palestine, State of (PS)", "Panama (PA)", "Papua New Guinea (PG)", "Paraguay (PY)", "Peru (PE)",
                 "Philippines (PH)", "Pitcairn (PN)", "Poland (PL)", "Portugal (PT)", "Puerto Rico (PR)", "Qatar (QA)",
                 "Reunion !Réunion (RE)", "Romania (RO)", "Russian Federation (RU)", "Rwanda (RW)", "Saint Barthélemy (BL)",
                 "Saint Helena, Ascension and Tristan da Cunha (SH)", "Saint Kitts and Nevis (KN)", "Saint Lucia (LC)", 
                 "Saint Martin (French part) (MF)", "Saint Pierre and Miquelon (PM)", "Saint Vincent and the Grenadines (VC)", 
                 "Samoa (WS)", "San Marino (SM)", "Sao Tome and Principe (ST)", "Saudi Arabia (SA)", "Senegal (SN)",
                 "Serbia (RS)", "Seychelles (SC)", "Sierra Leone (SL)", "Singapore (SG)", "Sint Maarten (Dutch part) (SX)", 
                 "Slovakia (SK)", "Slovenia (SI)", "Solomon Islands (SB)", "Somalia (SO)", "South Africa (ZA)", 
                 "South Georgia and the South Sandwich Islands (GS)", "South Sudan (SS)", "Spain (ES)", 
                 "Sri Lanka (LK)", "Sudan (SD)", "Suriname (SR)", "Svalbard and Jan Mayen (SJ)", "Swaziland (SZ)",
                 "Sweden (SE)", "Switzerland (CH)", "Syrian Arab Republic (SY)", "Taiwan, Province of China [note 2] (TW)",
                 "Tajikistan (TJ)", "Tanzania, United Republic of (TZ)", "Thailand (TH)", "Timor-Leste (TL)", "Togo (TG)",
                 "Tokelau (TK)", "Tonga (TO)", "Trinidad and Tobago (TT)", "Tunisia (TN)", "Turkey (TR)", "Turkmenistan (TM)", 
                 "Turks and Caicos Islands (TC)", "Tuvalu (TV)", "Uganda (UG)", "Ukraine (UA)", "United Arab Emirates (AE)",
                 "United Kingdom (GB)", "United States (US)", "United States Minor Outlying Islands (UM)", "Uruguay (UY)", 
                 "Uzbekistan (UZ)", "Vanuatu (VU)", "Venezuela, Bolivarian Republic of (VE)", "Viet Nam (VN)", "Virgin Islands,
                 British (VG)", "Virgin Islands, U.S. (VI)", "Wallis and Futuna (WF)", "Western Sahara (EH)",
                 "Yemen (YE)", "Zambia (ZM)", "Zimbabwe (ZW)")

