
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

### target duration: from offense to filing 
duration_function <- function(dataset, date_received_column, date_final_column)
{
  dataset %>% 
    group_by(case_nbr, party_id) %>% 
    summarise(
      start_time  = min({{ date_received_column }}, na.rm = TRUE),
      end_time    = max({{ date_final_column }},    na.rm = TRUE),
      duration    = as.numeric(difftime(max({{ date_final_column }}), 
                                        min({{ date_received_column }}))),
      .groups = "drop"
    ) 
}

#####  EXAMPLE #####
## sample dataset: cases charged
cases_charged <- read.csv("Cases Charged Data.csv")
# clean and wrangle cases data, to retrieve filing date
cases_charged <- cases_charged %>% 
  mutate(case_nbr = Case.Number,
         clerk_id = Clerk.ID,
         DA_id = DA.ID,
         party_id = Party.ID,
         offense_code = Offense.Code,
         date_offense = as.Date(ifelse(Offense.Date == "" | is.na(Offense.Date),
                                       NA, Offense.Date), 
                                format = "%m/%d/%y"),
         date_arrest = as.Date(ifelse(Arrest.Date == "" | is.na(Arrest.Date), 
                                      NA, Arrest.Date), 
                               format = "%m/%d/%y"), 
         date_received = as.Date(ifelse(Received.Date == ""|is.na(Received.Date),
                                        NA, Received.Date), 
                                 format = "%m/%d/%y"),
         date_filing = as.Date(ifelse(Event.Date == ""|is.na(Event.Date),
                                      NA, Event.Date), 
                               format = "%m/%d/%y") # Event date was the date of filing
  ) %>% 
  filter(Unit == "Trial Division") %>% 
  select(case_nbr, clerk_id, DA_id, Defendant, party_id, offense_code,
         Offense, Unit, date_offense, 
         date_arrest, date_received, date_filing) #58137

results = duration_function(cases_charged, date_offense, date_filing) # duration, unit in days 
