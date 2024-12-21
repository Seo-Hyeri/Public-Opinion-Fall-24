################################################################################
# Link ESS partyvote to Partyfacts ID, CHES
# setwd()
################################################################################

rm(list=ls())
library(rio)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)

#merging - ESS partyfacts, election dates
ess_party <- import("Data/essprt-all.csv")
ess_party <- ess_party %>% filter(str_detect(ess_variable, "prtv")) #leave only party vote choice variable
ess_election <- import("Data/ess_election_dates_JH.csv") #election dates data

ess_party  <- ess_party %>% rename(cntry = ess_cntry) #change varname to cntry
ess_party  <- ess_party %>% left_join(ess_election, by=c("cntry", "essround")) #1:m merging

ess_party_nosplit <- ess_party %>% filter(split_wave==FALSE) %>% mutate(vote_date = recent_election)

ess_party_split <- ess_party %>% filter(split_wave==TRUE)  %>% mutate(vote_date = recent_election)
ess_party_split <- rbind(ess_party_split, ess_party_split) %>%  arrange(cntry, essround, ess_variable, ess_party_id) # duplicate

ess_party_split <- ess_party_split %>% group_by(cntry, essround, ess_variable, ess_party_id) %>% 
  mutate(vote_date = case_when(
    row_number() == 1 ~ vote_date,
    row_number() == 2 ~ recent_election_split1,
  ))

ess_party2 <- rbind(ess_party_nosplit, ess_party_split)
ess_party2 <- ess_party2 %>%  arrange(cntry, essround, ess_variable, ess_party_id)
rm(ess_party_nosplit, ess_party_split, ess_party)

ess_party2 <- ess_party2 %>% mutate(vote_date = mdy(vote_date)) %>% mutate(vote_year = year(vote_date))

# Linking to CHES
# download and read Party Facts mapping table
pf_info_raw <- read_csv("Data/partyfacts-core-parties.csv")
pf_mapping_raw <- read_csv("Data/partyfacts-external-parties.csv", guess_max = 50000)
pf_ess_raw <- import("Data/essprt-all.csv")

pf_info <-
  pf_info_raw |>
  dplyr::select(partyfacts_id, partyfacts_name = name_short, technical)

pf_mapping <-
  pf_mapping_raw |>
  filter(!is.na(partyfacts_id))

## ESS ----

# link datasets (select only linked parties)
ess <-
  pf_mapping |>
  filter(dataset_key == "essprtv") |>
  dplyr::select(
    partyfacts_id,
    first_ess_id = dataset_party_id
  )

# CHES ----

ches <-
  pf_mapping |>
  filter(dataset_key == "ches") |>
  mutate(ches_id = as.integer(dataset_party_id)) |>
  dplyr::select(partyfacts_id, ches_id, ches_name = name_short) |>
  distinct(partyfacts_id, .keep_all = TRUE)

# Manifesto ----

manifesto <-
  pf_mapping |>
  filter(dataset_key == "manifesto") |>
  mutate(cmp_id = as.integer(dataset_party_id)) |>
  dplyr::select(partyfacts_id, cmp_id, cmp_name = name_short) |>
  distinct(partyfacts_id, .keep_all = TRUE)

## Merged dataset ----

ess_id <-
  pf_ess_raw |>
  filter(str_detect(ess_variable, "prtv")) |>
  dplyr::select(ess_id, first_ess_id)

link_table <-
  ess_id |>
  left_join(ess, by = c("first_ess_id" = "first_ess_id")) |>
  left_join(ches, by = c("partyfacts_id" = "partyfacts_id")) |>
  left_join(manifesto, by = c("partyfacts_id" = "partyfacts_id")) |>
  left_join(pf_info, by = c("partyfacts_id" = "partyfacts_id")) |>
  filter(is.na(technical) | partyfacts_name == "ally") |>
  dplyr::select(!technical) |>
  relocate(partyfacts_name, .after = partyfacts_id)

link_table_technical <-
  ess_id |>
  left_join(ess, by = c("first_ess_id" = "first_ess_id")) |>
  left_join(pf_info, by = c("partyfacts_id" = "partyfacts_id")) |>
  filter(!is.na(technical))

ess_party3 <-ess_party2 %>% left_join(link_table %>% dplyr::select(-partyfacts_id),
                                      by = c("ess_id", "first_ess_id")) %>% mutate(id = row_number())

## Left-Right from CHES
link_table2 <- ess_party3 %>% distinct()

ches <- import("Data/final_ches_imputed_parlgov_added.rds")
ches_cmp  <- ches %>% filter(!is.na(cmp_id)) %>% dplyr::select(lrgen, galtan, cmp_id, vote_date = election_date, everything()) %>% distinct()

ches_cmp_dup <- ches_cmp %>%
  filter(
    duplicated(ches_cmp[, c("cmp_id", "vote_date")]) |
      duplicated(ches_cmp[, c("cmp_id", "vote_date")], fromLast = TRUE)
  ) %>% arrange(cmp_id, vote_date, ches_eval_year)

ches_cmp_dup <- ches_cmp_dup %>% group_by(cmp_id, vote_date) %>% slice_head(n = 1) %>% ungroup()

ches_cmp_nodup <- ches_cmp %>%
  filter(
    !duplicated(ches_cmp[, c("cmp_id", "vote_date")]) &
      !duplicated(ches_cmp[, c("cmp_id", "vote_date")], fromLast = TRUE)
  )

ches_cmp2 <- rbind(ches_cmp_dup, ches_cmp_nodup) %>% dplyr::select(lrgen, galtan, cmp_id, vote_date)
link_table3 <- link_table2 %>% left_join(ches_cmp2, by=c("cmp_id", "vote_date"))

ches_ches <- ches %>% filter(is.na(cmp_id)) %>% dplyr::select(lrgen, galtan, 
                                                              ches_id, vote_date = election_date, everything()) %>% distinct()
ches_ches_nodup <-ches_ches[!duplicated(ches_ches[,c("ches_id", "vote_date")]),]
ches_ches_nodup <- ches_ches_nodup%>% dplyr::select(lrgen, ches_id, vote_date)
link_table4 <- link_table3 %>% left_join(ches_ches_nodup, by=c("ches_id", "vote_date"))
link_table4 <- link_table4 %>% mutate(lrgen = rowMeans(cbind(lrgen.x, lrgen.y),
                                                       na.rm = TRUE)) %>% dplyr::select(-lrgen.x, -lrgen.y)
table(link_table4$lrgen)
hist(link_table4$lrgen)
link_table4 <- link_table4 %>% mutate(left_right = case_when(
  lrgen <=3 ~ -1,
  lrgen >=7 ~ 1,
  TRUE ~ 0
))
link_table4 <- link_table4 %>% dplyr::select(id, lrgen, galtan, left_right)
table(link_table4$left_right)
table(link_table4$galtan)
ess_party4 <- ess_party3 %>% left_join(link_table4, by ="id")
ess_party4 <- ess_party4 %>% dplyr::select(-id)
saveRDS(ess_party4, "Data/ess_vote_partyfacts_linked.rds")
