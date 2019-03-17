
# Setup -------------------------------------------------------------------

# Packages
library(tidyverse)

# Load data
thesis <- read_csv2("./2_clean_data/thesis.csv")
thesis_sampled <- read_csv2("./2_clean_data/thesis_sampled.csv")

# Path output
path_output <- "./4_output/"



# what about the domains
unique(thesis$discipline)

thesis %>% 
  group_by(discipline) %>% 
  count() %>% 
  arrange(desc(n)) %>% View

# OMG, the disciplines are messed up...
# Let's try to improve it

thesis_clean <- thesis_clean %>% 
  mutate(discipline = str_to_lower(discipline)) %>% 
  mutate(discipline_clean1 = case_when(
    str_detect(discipline, "chimi|biochim") ~ "Chimie",
    str_detect(discipline, "medecine|medic|clinique|cancero|epidemi") ~ "Medecine",
    str_detect(discipline, "informatique") ~ "Informatique",
    str_detect(discipline, "biolo|physio|population|ecolog|evolut|vie|microbio|génét|genet|natur|neuro|envir|zool") ~ "Biologie",
    str_detect(discipline, "physique|fluides|univers|meca|electro|techni|génie|genie|matér|mater|robot|matie|hydro|signal|astro") ~ "Physique",
    str_detect(discipline, "hist") ~ "Histoire",
    str_detect(discipline, "philo") ~ "Philosophie",
    str_detect(discipline, "psycho") ~ "Psychologie",
    str_detect(discipline, "mathem|stat") ~ "Mathematiques",
    str_detect(discipline, "polit") ~ "Sciences politiques",
    str_detect(discipline, "etudes|ethno|humaines|lingui|litte|lang|art") ~ "Humanities",
    str_detect(discipline, "droit") ~ "Droit",
    str_detect(discipline, "geogr") ~ "Geographie",
    TRUE ~ "Other")) 


thesis_clean %>% 
  group_by(discipline_clean1) %>% 
  count() %>% 
  arrange(desc(n)) %>% View

# C'est l'horreur.


# Est-ce qu'il y a des gens qui ont soutenu deux fois?
thesis %>% 
  filter(!is.na(author_id)) %>% 
  filter(author_id != ",") %>% 
  filter(duplicated(author_id)) %>% View


thesis %>% 
  filter(!is.na(author_id)) %>% 
  filter(author_id != ",") %>% 
  filter(!str_detect(author_id, ",")) %>% 
  arrange(author_id) %>% 
  group_by(author_id) %>% 
  mutate(count = n()) %>% 
  filter(count > 2) %>%
  arrange(count) %>% View

# Well, there may be some legitimate individuals here, but there is also a lot of 
# dubious things (some people defended > than two thesis on very different subjects,
# which strikes me as unlikely).


# There are many thesis with two authors. Wtf?
  
thesis_sampled %>% 
  filter(str_detect(author_id, ",")) %>% 
  View()

# Ok, let's ignore them.



# Timelines ---------------------------------------------------------------

thesis %>% 
  filter(year_defense > 1990) %>% 
  group_by(year_defense, discipline_clean2) %>% 
  count() %>% 
  ggplot(aes(x = year_defense, y = discipline_clean2, 
             fill = n)) +
  geom_tile()

thesis %>% 
  ggplot(aes(x = year_defense)) +
  geom_histogram(binwidth = 1)



thesis %>% 
  group_by(day_defense, discipline_clean2) %>% 
  count() %>% 
  ggplot(aes(x = day_defense, y = discipline_clean2, fill = n)) +
  geom_tile()

# Should probably take the proportions per discipline...
# and reorder the damned factors.


# Evolution data ----------------------------------------------------------

# Theses en évolution qui ont été soutenues.
# "Evolut dans discipline : des fois, c'est beaucoup plus large. Should I do it by title?

# 638? That seem not a lot.
thesis_evolution <- thesis %>% 
  filter(str_detect(discipline, pattern = "evolut")) %>% 
  drop_na(date_defense)



