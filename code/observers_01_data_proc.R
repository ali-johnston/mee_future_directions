
library(auk)
library(tidyverse)
library(lubridate)

data_root <- "data/"
figures_root <- "figures/"


# --------------------------------------------------------------------
# read in all sampling events

samp <- auk_sampling(file = "ebd_sampling_relFeb-2021/ebd_sampling_relFeb-2021.txt")

# filter to only stationary and traveling counts within New York state

samp_filters <- samp %>% 
  auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete() %>%
  auk_state("US-NY")

samp_out <- str_glue("{data_root}/checklist_data_NY_state_only.txt")
samp2 <- samp_filters %>%
		auk_filter(file = samp_out)


# this is the richness data. it's from the eBird reference database, 
# calculated by Shawn in 2021. 
# there is one row for each group checklist and groups are 
# assigned to the primary observer. 
# the geographic region is bounded by the min and max lat-long for 
# NY state. so there are some extras outside NY state that we need to 
# remove 
# richness is the number of distinct species recorded to at least species level
# (subspecies are rolled up to their species, to avoid double counting)
# hybrids and spuhs and slashes are removed. 

# if starting fresh with EBD, this dataset will need to be calculated 
# from the raw data.

rd <- read_csv("data/new_york_richness.csv")

# this is the checklist data for only NY state. 

checklists <- read_sampling(samp_out, unique = FALSE)
ny_checklists <- checklists %>%
		dplyr::select(sampling_event_identifier, observer_id)

richness <- rd %>% 
	mutate(sampling_event_identifier = paste0("S", sampling_event_id)) %>%
	rename(observer_id_number = observer_id) %>%
	mutate(observer_id = paste0("obs", observer_id_number)) %>%
	inner_join(ny_checklists, by=c("sampling_event_identifier" = "sampling_event_identifier", "observer_id" = "observer_id"))

# --------------------------------------------------------------------
# reduce to may 2002-2020 to keep file size as small as possible

richness_may <- richness %>% 
	filter(month == 5) %>%
	mutate(effort_hrs_sqrt = sqrt(effort_hrs)) %>%
	mutate(effort_hrs_2 = effort_hrs^2) %>%
	filter(year > 2001, year < 2021) %>%
	mutate(year_seq = year - 2001)


data_loc <- str_glue("{data_root}/richness_ny_processed_may.csv")
write_csv(richness_may, data_loc)

# --------------------------------------------------------------------
# plot locations to check the filter is working. 

rd_all_locs <- rd %>%
		dplyr::select(latitude, longitude, loc_id) %>%
		distinct()

ny_all_locs <- richness_may %>%
	dplyr::select(latitude, longitude, loc_id) %>%
	distinct()


plot(rd_all_locs$longitude, rd_all_locs$latitude, pch=".")
points(ny_all_locs$longitude, ny_all_locs$latitude, pch=".", col="red")





