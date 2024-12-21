################################################################################
# Cleaning dates ESS 1 - 10 
# ESS - partyfacts - CHES
# ESS 11 date cleaning
# setwd("/Users/hyeriseo/Documents/GitHub/Public-Opinion-Fall-24")
# Input: ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC.csv
#        ess_election_dates_JH.csv
#        ess_vote_partyfacts_linked.rds
#        ESS11.csv
# Output: ess_Datecleaned_Votecoded.rds <- round 1 to 10
#         ess11_Datecleaned.rds <- round 11
# No party data for round 11 - so I don't link CHES with ESS round 11 data
################################################################################

rm(list=ls())
library(dplyr)
library(rio)
library(lubridate)
library(SGmisc)
library(ltm)
library(missForest)
library(mice)
library(ggplot2)
library(gridExtra)
library(stargazer)

#bring original ESS dataset
data <- import("Data/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC.csv")


#cntry, essround
table(data$cntry)
table(data$essround)


##### ESS round 1 - 2 cleaning dates
table(data$inwyr, data$essround) # THe numbers match well
table(data$inwmm, data$essround) # THe numbers match well
table(data$inwdd, data$essround) # THe numbers match well

ess_1_2 <- data %>%
         filter(essround == 1 | essround == 2) %>%
         mutate(
           inwyr = case_when(inwyr == 9999 ~ NA_integer_, TRUE ~ inwyr),
           inwmm = case_when(inwmm == 99 ~ NA_integer_, TRUE ~ inwmm),
           inwdd = case_when(inwdd == 99 ~ NA_integer_, TRUE ~ inwdd),
           interview_date = paste0(inwyr, "-", inwmm, "-", inwdd)
           ) %>%
         mutate(
           interview_date = ymd(interview_date)
           )

    #Most common year/month/date
         # Create the function.
         getmode <- function(v) {
           v <- v[!is.na(v)]  # Remove NA values
           if (length(v) == 0) return(NA)  # Return NA if all values are NA
           uniqv <- unique(v)
           uniqv[which.max(tabulate(match(v, uniqv)))]
           }
 
         mode_interview = ess_1_2 %>% group_by(cntry, essround) %>% summarise(year = getmode(inwyr),
                                                                              month = getmode(inwmm),
                                                                              date = getmode(inwdd),
                                                                              .groups = 'drop') #Drop all levels of grouping for downstream

    # Join the mode values back to the main data
                  ess_1_2_filled <- ess_1_2 %>%
                    left_join(mode_interview, by = c("cntry", "essround")) %>%
                    mutate(
                     inwyr = ifelse(is.na(inwyr), year, inwyr),
                     inwmm = ifelse(is.na(inwmm), month, inwmm),
                     inwdd = ifelse(is.na(inwdd), date, inwdd),
                     interview_date = paste0(inwyr, "-", inwmm, "-", inwdd)
                     ) %>%
                     mutate(
                     interview_date = ymd(interview_date))
     # Inspect rows where the date parsing still failed
                   invalid_dates <- ess_1_2_filled %>%
                   filter(is.na(interview_date)) %>%
                   arrange(inwyr, inwmm, inwdd) %>%
                   dplyr::select(inwyr, inwmm, inwdd, everything())
                   
     #Invalid dates exist
          invalid_dates <-invalid_dates %>% mutate(inwdd = case_when(
             inwdd == 31 & inwmm !=2 ~ 30,  #Months with 30 have 31 dates
             inwdd == 31 & inwmm ==2 ~ 28,
             TRUE ~ inwdd
             ))

           invalid_dates <-invalid_dates %>% mutate(interview_date = paste0(inwyr, "-", inwmm, "-", inwdd)) %>%
            mutate(interview_date = ymd(interview_date))

     #Combine
          ess_1_2_combined <- rbind(ess_1_2_filled %>% filter(!is.na(interview_date)), invalid_dates)
          ess_1_2_combined$interview_date <- ymd(ess_1_2_combined$interview_date)
          ess_1_2_combined <- ess_1_2_combined %>% dplyr::select(-year,-month, -date)
          rm(ess_1_2, ess_1_2_filled)
          
##### ESS 3 - 9 cleaning dates
          ess_3_9 <- data %>%
          filter(essround %in% c(3:9)) %>%
          mutate(
          inwyys = case_when(inwyys == 9999 ~ NA_integer_, TRUE ~ inwyys),
          inwmms = case_when(inwmms == 99 ~ NA_integer_, TRUE ~ inwmms),
          inwdds = case_when(inwdds == 99 ~ NA_integer_, TRUE ~ inwdds),
          interview_date = paste0(inwyys, "-", inwmms, "-", inwdds)
          ) %>%
          mutate(
          interview_date = ymd(interview_date)
          )

          mode_interview = ess_3_9 %>% group_by(cntry, essround) %>% summarise(year = getmode(inwyys),
                                                                              month = getmode(inwmms),
                                                                              date = getmode(inwdds),
                                                                             .groups = 'drop') #Drop all levels of grouping for downstream
         # Join the mode values back to the main data
         ess_3_9_filled <- ess_3_9 %>%
         left_join(mode_interview, by = c("cntry", "essround")) %>%
         mutate(
         inwyys = ifelse(is.na(inwyys), year, inwyys),
         inwmms = ifelse(is.na(inwmms), month, inwmms),
         inwdds = ifelse(is.na(inwdds), date, inwdds),
         interview_date = paste0(inwyys, "-", inwmms, "-", inwdds)
         ) %>%
         mutate(
         interview_date = ymd(interview_date)
         )

         # Inspect rows where the date parsing still failed
         invalid_dates <- ess_3_9_filled %>%
         filter(is.na(interview_date)) %>%
         arrange(inwyys, inwmms, inwdds) %>%
         dplyr::select(inwyys, inwmms, inwdds, everything())

         invalid_dates_ee <- invalid_dates %>% filter(cntry == "EE") # All values are missing. Need to use supplementary date
          ## There is no big difference between interview dates and supplementary dates
         invalid_dates_ee <- invalid_dates_ee %>% mutate(
         supqyr = case_when(supqyr == 9999 ~ NA_integer_, TRUE ~ supqyr),
         supqmm = case_when(supqmm == 99 ~ NA_integer_, TRUE ~ supqmm),
         supqdd = case_when(supqdd == 99 ~ NA_integer_, TRUE ~ supqdd),
         interview_date = paste0(supqyr, "-", supqmm, "-", supqdd)
         ) %>%
         mutate(
         interview_date = ymd(interview_date)
         ) %>%
         arrange(interview_date)

         invalid_dates_bg <- invalid_dates %>% filter(cntry == "BG") # All dates are missing. So, I set them on First
         invalid_dates_bg <- invalid_dates_bg %>% mutate(inwdds = 1) %>%
         mutate(interview_date = paste0(inwyys, "-", inwmms, "-", inwdds)) %>% mutate(interview_date = ymd(interview_date))

         invalid_dates <- invalid_dates %>%
         filter(!(cntry == "EE" | cntry == "BG"))

         invalid_dates <-invalid_dates %>% mutate(inwdds = case_when(
         inwmms ==2 ~ 28,  #Months with 30 have 31 dates
         inwmms ==11 ~ 30,
         TRUE ~ inwdds
         ))

         invalid_dates <-invalid_dates %>% mutate(interview_date = paste0(inwyys, "-", inwmms, "-", inwdds)) %>%
         mutate(interview_date = ymd(interview_date))

         #Combine
         ess_3_9_combined <- rbind(ess_3_9_filled %>% filter(!is.na(interview_date)), invalid_dates, invalid_dates_ee, invalid_dates_bg)
         ess_3_9_combined$interview_date <- ymd(ess_3_9_combined$interview_date)
         ess_3_9_combined <- ess_3_9_combined %>% dplyr::select(-year,-month, -date) %>% arrange(interview_date)

         rm(ess_3_9_filled, invalid_dates, invalid_dates_ee, invalid_dates_bg, ess_3_9, mode_interview)

##### ESS 10: inwyr-inwmm-inwdd
         ess_10 <- data %>% filter(essround ==10)
         ess_10<-ess_10 %>% mutate(interview_date = gsub(" .*", "", inwds))
         ess_10$interview_date <- ymd(ess_10$interview_date)
 
         common_date <- ess_10 %>% group_by(cntry) %>% summarise(date = getmode(interview_date)) # Find the common dates

         #Check the ESs 10 country book and find the dates of interview. Use the median value
         common_date <- common_date %>% mutate(date = case_when(
         cntry == "AT" ~ mid_date(ymd("2021-08-29"), ymd("2021-12-05")),
         cntry == "CY" ~ mid_date(ymd("2022-03-08"), ymd("2022-08-18")),
         cntry == "DE" ~ mid_date(ymd("2021-10-04"), ymd("2022-01-03")),
         cntry == "ES" ~ mid_date(ymd("2021-06-06"), ymd("2021-12-30")),
         cntry == "IL" ~ mid_date(ymd("2021-01-31"), ymd("2022-07-16")),
         cntry == "LV" ~ mid_date(ymd("2021-10-31"), ymd("2022-01-30")),
         cntry == "PL" ~ mid_date(ymd("2022-01-24"), ymd("2022-05-24")),
         cntry == "RS" ~ mid_date(ymd("2022-01-10"), ymd("2022-05-24")),
         cntry == "SE" ~ mid_date(ymd("2021-12-09"), ymd("2022-01-16")),
         TRUE ~ date
         ))

         invalid_dates <- ess_10 %>% filter(is.na(interview_date)==TRUE)
         invalid_dates <- invalid_dates %>% left_join(common_date, by = "cntry")
         invalid_dates$interview_date <- ymd(invalid_dates$date)

         ess_10_combined <- rbind(ess_10 %>% filter(is.na(interview_date)!=TRUE), invalid_dates %>% dplyr::select(-date))
         ess_10_combined$interview_date <- ymd(ess_10_combined$interview_date)

####### combine
         data_date <- rbind(ess_1_2_combined, ess_3_9_combined, ess_10_combined)
         rm(ess_1_2_combined, ess_3_9_combined, ess_10, data, invalid_dates, ess_10_combined, common_date)

         sum(is.na(data_date$interview_date))

####### combine with ess election_date

        ess_election <- import("Data/ess_election_dates_JH.csv")

        data_date <- left_join(data_date, ess_election, by=c("cntry", "essround"))
        data_date$recent_election <-mdy(data_date$recent_election)
        data_date$recent_election_split1 <-mdy(data_date$recent_election_split1)
        sum(is.na(data_date$recent_election)) #Everything merge well

        data_date <- data_date %>% mutate(
        vote_date = case_when(
        split_wave == FALSE ~ recent_election,
        split_wave == TRUE & interview_date < recent_election ~ recent_election_split1,
        split_wave == TRUE & interview_date >=recent_election ~ recent_election,
        )
        )
        
       sum(is.na(data_date$vote_date))

       data_check <- data_date %>% filter(cntry == "BE" & split_wave==TRUE) %>% dplyr::select(interview_date, vote_date,
                                                                                recent_election, recent_election_split1, everything())
 
       saveRDS(data_date, "Data/ess_Datecleaned.rds")

       
####### combine with votecode
       
rm(list=ls())
data1 <- readRDS("Data/ess_Datecleaned.rds")

# Which party they vote (from Fred Solt)

       # find the FIRST country-specific vote variable
       start <- head(grep("prtv", colnames(data1)), value = TRUE)
       
       # find LAST country-specific vote variable
       end <- tail(grep("prtv", colnames(data1)), value = TRUE)
       
       # mini dataset of party choice vars
       es.vote <- data1 %>% dplyr::select(start:end)
       es.vote <- es.vote %>%  dplyr::select(-ends_with("de1")) # drop 1st German vote
       
       data1$party.vote <- as.vector(do.call(coalesce, es.vote)) # Looking at the first if there are multiple (except for Germany)
       
       data1$party.vote.ess <- ifelse(is.na(data1$party.vote), NA,
                                      paste0(data1$cntry, "-", data1$essround, "-", data1$party.vote))
       
       ess_party_vote <- readRDS("Data/ess_vote_partyfacts_linked.rds") 
       ess_party_vote$party.vote.ess <- paste0(ess_party_vote$cntry, "-", ess_party_vote$essround, "-", ess_party_vote$ess_party_id)
       
       ess_party_vote <- ess_party_vote %>%
         group_by(party.vote.ess, vote_date) %>%
         mutate(
           lrgen = mean(lrgen, na.rm = TRUE),
           galtan = mean(galtan, na.rm = TRUE),
           num = n()
         ) %>%
         ungroup()
       ess_party_vote <- ess_party_vote %>% dplyr::select(party.vote.ess, vote_date, lrgen, galtan) %>% distinct()
       
       data2 <- left_join(data1, ess_party_vote, by=c("party.vote.ess", "vote_date"))
       
       saveRDS(data2, "Data/ess_Datecleaned_Votecoded.rds")
       
 
       
####### cleaning dates for ESS 11   
       rm(list=ls())
       
       ESS11 <- read.csv("Data/ESS11.csv")
       ESS11 <- ESS11 %>% mutate(interview_date = gsub(" .*", "", inwds))
       ESS11$interview_date <- ymd(ESS11$interview_date)
       
       
       #Most common year/month/date
       # Create the function.
       getmode <- function(v) {
         v <- v[!is.na(v)]  # Remove NA values
         if (length(v) == 0) return(NA)  # Return NA if all values are NA
         uniqv <- unique(v)
         uniqv[which.max(tabulate(match(v, uniqv)))]
       }
       
       
       common_date <- ESS11 %>% group_by(cntry) %>% summarise(date = getmode(interview_date)) 
       
       
       common_date <- common_date %>% mutate(date = case_when(
         cntry == "AT" ~ mid_date(ymd("2023-06-13"), ymd("2023-12-03")),
         cntry == "HR" ~ mid_date(ymd("2023-06-24"), ymd("2024-01-19")),
         cntry == "FI" ~ mid_date(ymd("2023-07-31"), ymd("2024-01-29")),
         cntry == "DE" ~ mid_date(ymd("2023-05-09"), ymd("2023-12-21")),
         cntry == "HU" ~ mid_date(ymd("2023-05-05"), ymd("2023-11-10")),
         cntry == "IE" ~ mid_date(ymd("2023-06-26"), ymd("2024-01-02")),
         cntry == "LT" ~ mid_date(ymd("2023-09-04"), ymd("2023-12-31")),
         cntry == "NL" ~ mid_date(ymd("2023-03-31"), ymd("2023-11-07")),
         cntry == "NO" ~ mid_date(ymd("2023-04-16"), ymd("2023-11-29")),
         cntry == "SK" ~ mid_date(ymd("2023-09-08"), ymd("2023-12-12")),
         cntry == "SI" ~ mid_date(ymd("2023-03-20"), ymd("2023-08-13")),
         cntry == "CH" ~ mid_date(ymd("2023-03-09"), ymd("2024-01-31")),
         TRUE ~ date
       ))
       
       
       invalid_dates <- ESS11 %>% filter(is.na(interview_date)==TRUE)
       invalid_dates <- invalid_dates %>% left_join(common_date, by = "cntry")
       invalid_dates$interview_date <- ymd(invalid_dates$date)
       
       ESS11 <- rbind(ESS11 %>% filter(is.na(interview_date)!=TRUE), invalid_dates %>% dplyr::select(-date))
       ESS11$interview_date <- ymd(ESS11$interview_date)
       
       
       saveRDS(ESS11, "Data/ess11_Datecleaned.rds")