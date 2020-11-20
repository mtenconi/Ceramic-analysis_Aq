## analisi statistica materiali Arqua':

# set working directory:
setwd("...")
getwd()

# import data
data_xrf <- read.csv("arqua_xrf.csv", header = TRUE, sep = ",")
data_samples <- read.csv("arqua_cronologia.csv", header = TRUE, sep = ",")
data_fabric <- read.csv("arqua_fabric.csv", header = TRUE, sep = ",")

# rename data_samples; data_xrf; data_fabric
data_samples <- subset(data_samples, select = -c(Cronologie.pre.def,anno.scavi) ) %>%
  rename(sample_id = ï..samples,
         vessel_type = tipologia,
         chronology = Cronologie.pre.def.1)

data_xrf <- data_xrf %>%
  rename(sample_id = ï..)

data_fabric <- data_fabric %>%
  rename(sample_id = ï..campioni)

# change NA data
data_samples[is.na(data_samples)] <- "unknown"
data_samples <- data_samples %>%
  mutate(vessel_type = replace(as.character(vessel_type), vessel_type == "", "unknown"), 
         chronology =  replace(as.character(chronology), chronology == "", "unknown")) %>%
  mutate(vessel_type = as.factor(vessel_type), chronology = as.factor(chronology))

## Combine data_samples with data_xrf
# function left_join() to combine the information from two tables
# to join two tables together we need one column that is exactly the same in each table:
identical(data_samples$sample_id, data_xrf$sample_id)
identical(data_samples$sample_id, data_fabric$sample_id)

new_data <- left_join(data_samples, data_fabric,  by = "sample_id")
new_data <- left_join(new_data, data_xrf,  by = "sample_id") %>%
  mutate(fabric = as.factor(fabric))

new_data %>% kable() %>% kable_styling()

