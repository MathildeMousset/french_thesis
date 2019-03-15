# theses.fr
# rvest

# On the 14/12/2018
# Theses soutenues
# Pas de limite de date

library(RCurl)
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(scales)
library(hrbrthemes)


# 370031 

i <- 1:400
i <- i*1000

URL <- paste0("https://www.theses.fr/?q=&fq=dateSoutenance:[1965-01-01T23:59:59Z%2BTO%2B2018-12-31T23:59:59Z]&checkedfacets=&start=",i, "&sort=none&status=status:soutenue&access=&prevision=&filtrepersonne=&zone1=titreRAs&val1=&op1=AND&zone2=auteurs&val2=&op2=AND&zone3=etabSoutenances&val3=&op3=AND&zone4=dateSoutenance&val4a=&val4b=&type=lng=&checkedfacets=&format=csv")


# map(URL, getURL) %>% write.csv(.,"./1_raw_data/SERP_2.csv")

thesis <- read.csv("./1_raw_data/SERP_2.csv",
                   sep = ";", quote = "", skip = 1,
                   stringsAsFactors = F)

# Improve colnames
colnames(thesis) <- c("author", "author_id", "title", 
                      "thesis_advisor1", "thesis_advisor2", "thesis_advisors_id", 
                      "university", "university_id", "discipline", 
                      "status", "date_first_registration", "date_defense", 
                      "language", "thesis_id", 
                      "online", "date_upload", "date_update", "whatever")




# Remove crappy column in the end
# Get rid of duplicated header rows & crappy lines
# Put the names of authors or advisors in lower case plus majuscule
# Get date in YMD format
# Get year, month and week onf the day
thesis2 <- thesis %>% 
  select(-whatever) %>% 
  filter(!str_detect(online, "Accessible en ligne")) %>% 
  mutate(author          = str_to_title(author),
         thesis_advisor1 = str_to_title(thesis_advisor1),
         thesis_advisor2 = str_to_title(thesis_advisor2)) %>% 
  filter(title != "",
         status == "soutenue",
         !str_detect("discipline")) %>% 
  mutate(date_first_registration = dmy(date_first_registration),
         date_defense            = dmy(date_defense),
         date_update             = dmy(date_update),
         date_upload             = dmy(date_upload)) %>% 
  mutate(year_defense  = year(date_defense),
         month_defense = month(date_defense, label = TRUE, abbr = FALSE),
         day_defense   = wday(date_defense,  label = TRUE, abbr = FALSE)) %>% 
  mutate(title = str_replace(title, "\"\"", "")) %>% 
  mutate(title = str_replace(title, "\"", "")) %>% 
  
  filter(!str_detect(title, "Fa yan kan zhong guo"))


thesis_sampled <- sample_n(thesis2, 4000)


# Save
write.table(thesis2, 
            "./2_clean_data/thesis.csv",
            quote = FALSE,
            sep = ";",
            dec = ".",
            row.names = FALSE)


write.table(thesis_sampled, 
            "./2_clean_data/thesis_sampled.csv",
            quote = FALSE,
            sep = ";",
            dec = ".",
            row.names = FALSE)






