
library(tidyverse)
library(lubridate)
library(data.table)
library(auk)

data_root <- "data/"
figures_root <- "figures/"


# --------------------------------------------------------------------
# read in checklists complete or incomplete
cl_loc <- str_glue("{data_root}/sampling_data_NY_state_slim.csv")
checklists <- read_csv(cl_loc)


f <- list.files(str_glue("{data_root}/NY_ebd_split_species/"))

species <- no_complete <- no_incomplete <- vector(length = length(f))

# loop through each species and summarise proportion of 
# complete and incomplete checklists with that species

for(i in 1:length(f)){

		f1 <- f[i]
		print(paste(i, f1))

		# --------------------------------------------------------------------
		# read in data

		d_loc <- str_glue("{data_root}/NY_ebd_split_species/{f1}")
		d <- read_ebd(d_loc) %>%
				dplyr::select(observation_count, sampling_event_identifier, group_identifier, observer_id, observation_date) %>%
				mutate(species_observed = 1) %>%
				filter(observation_date > "2010-01-01", observation_date < "2021-01-01")

		checklists_d <- d %>% 
#				filter(group_identifier == "") %>%
				left_join(checklists, by=c("sampling_event_identifier"))

		no_complete[i] <- sum(checklists_d$all_species_reported == TRUE, na.rm = TRUE)
		no_incomplete[i] <- sum(checklists_d$all_species_reported == FALSE, na.rm = TRUE)
		species[i] <- gsub(".txt", "", f1) 

}


complete_counts <- checklists %>% 
			filter(observation_date > "2010-01-01", observation_date < "2021-01-01") %>% 
			dplyr::select(all_species_reported) %>%
			table()


d2 <- data.frame(species, no_complete, no_incomplete) %>%
			mutate(prop_complete = no_complete/complete_counts["TRUE"]) %>%
			mutate(prop_incomplete = no_incomplete/complete_counts["TRUE"])

res_loc <- str_glue("{data_root}/species_complete_incomplete_NY_2010_2020.txt")
write.table(d2, res_loc, row.names = FALSE)


