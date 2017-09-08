library(dplyr)
library(datapkg)
library(readxl)
library(data.table)
library(stringr)

##################################################################
#
# Processing Script for DPH Annual Population Estimates by County
# Created by Jenna Daly
# On 09/06/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xls") 

# iterate through files, getting county data
county_pop <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read_excel(paste0(path_to_raw, "/", raw_data[i]), sheet=1, range = cell_rows(4:9))
  names(current_file) <- c("County", "Value", "County", "Value")
  current_file <- current_file[-1,]
  current_file1 <- current_file[,1:2]
  current_file2 <- current_file[,3:4]
  current_file <- rbind(current_file1, current_file2)
  # cleanup county names
  current_file$County <- paste(str_trim(current_file$County), "County")
  # Add year as column in dataset
  get_year <- unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")
  current_file$Year <- get_year
  # bind this year's data to main container.
  county_pop <- rbind(current_file, county_pop)
}

#Remove any commas from value column
county_pop$Value <- gsub(",", "", county_pop$Value)

# iterate through files, getting state data
state_pop <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read_excel(paste0(path_to_raw, "/", raw_data[i]), sheet=1, range = cell_rows(2:3))
  current_file <- current_file[,1]
  current_file$County <- "Connecticut"
  names(current_file) <- c("Value", "County")
  #Extract population value 
  current_file$Value <- as.numeric(gsub("[^\\d]+", "", current_file$Value, perl=TRUE))
  # Add year as column in dataset
  get_year <- unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")
  current_file$Year <- get_year
  # bind this year's data to main container.
  state_pop <- rbind(current_file, state_pop)
}

#Remove any commas from value column
state_pop$Value <- gsub(",", "", state_pop$Value)

#Combine county and state values
total_pop <- rbind(county_pop, state_pop)

# derive percent of state total
percents <- merge(total_pop, state_pop, by = "Year")

percents <- percents %>% 
  mutate(Value = round((as.numeric(Value.x) / Value.y)*100, 2)) %>% 
  select(Year, County.x, Value) %>% 
  rename(County = County.x)

percents$`Measure Type` <- "Percent"
total_pop$`Measure Type` <- "Number"

# bind all the data
total_pop <- rbind(total_pop, percents)

#Merge in FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
fips <- (county_fips_dp$data[[1]])

total_pop_fips <- merge(total_pop, fips, all=T)

total_pop_fips$Variable <- "Estimated Population"

total_pop_fips <- total_pop_fips %>% 
  select(County, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, `Measure Type`)

# Write collated data to file.
write.table(
  total_pop_fips,
  file.path(getwd(), "data", "dph-population-by-county_2016.csv"),
  sep = ",",
  row.names = F
)

