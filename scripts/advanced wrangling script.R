psu_data <- read.csv("data/PSU_Fish_Density.csv")
hab_data <- read.csv("data/sttstj_ncrmp_hab.csv")
taxon_data <- read.csv("data/taxonomic_data.csv")

library("tidyverse")
library("dplyr")

#1.

#Adding common name to density data set as a new field

density_data <- psu_data %>% 
  left_join(taxon_data %>% select(SPECIES_CD, COMNAME), 
            by = "SPECIES_CD")
#Reorder the columns to be more readable

reordered_density_data <- density_data %>%
  select(YEAR, REGION, SPECIES_CD,COMNAME,density,STRAT,PRIMARY_SAMPLE_UNIT, PROT)

#2. Add a new field to the density data set named “fishery_target” at set all to TRUE

reordered_density_data <- reordered_density_data %>% 
  mutate(fishery_target = TRUE)

#3. Add a new field named “group” at set the value based on fish species where…
#grouper = Coney, Red hind,
#sanpper = Mutton, Gray, Yellowtail
#parrotfish = Stoplight
#other = Triggerfish, Hogfish

reordered_density_data <- reordered_density_data %>% 
  mutate(group = case_when(
    COMNAME %in% c("coney", "red hind")~"grouper",
    COMNAME %in% c("mutton snapper", "gray snapper", "yellowtail snapper")~"snapper",
    COMNAME %in% c("stoplight parrotfish")~"parrotfish",
    COMNAME %in% c("queen triggerfish", "hogfish")~"other",
    TRUE ~ "Unknown"
  ))

#4. Using the group_by function, how many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR?

yearly_density_data <- reordered_density_data %>% 
  group_by(YEAR) %>% 
  summarise(unique_primary_sample_units = n_distinct(PRIMARY_SAMPLE_UNIT))

#5. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR and PROT combination?

yearly_density_data <- reordered_density_data %>% 
  group_by(YEAR, PROT) %>% 
  summarise(unique_primary_sample_units = n_distinct(PRIMARY_SAMPLE_UNIT))

#6. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR, PROT and STRAT combination?

yearly_density_data <- reordered_density_data %>% 
  group_by(YEAR, PROT, STRAT) %>% 
  summarise(unique_primary_sample_units = n_distinct(PRIMARY_SAMPLE_UNIT))

#7. Summarize gives you just the data you requested and mutate would add the data you're requesting to the existing dataframe its from 

#8. Create a new dataframe that shows mean density of each species per year…hint
mean_density_yearly <- reordered_density_data %>% 
  group_by(YEAR, SPECIES_CD) %>% 
  summarise(meanDensity = mean(density))

#9. Create a new dataframe that shows mean density of each species in each PROT per year…hint

mean_density_prot_year <- reordered_density_data %>% 
  group_by(YEAR, SPECIES_CD, PROT) %>% 
  summarise(meanDensity = mean(density))

#10. not sure what im doing here tbh 
park_data <- reordered_density_data %>% 
  group_by(group, PROT) %>% 
  arrange(desc(density)) %>% 
  mutate(group = case_when(
    PROT < 1 ~ "Inside Park",
    PROT > 1 ~ "Outside Park",
    TRUE ~ "Unknown"
  ))
