## Récupération des données des groupes ensae et isep

setwd("C:\\Users\\ibrah\\Desktop\\ISEP_3_ENSAE\\COURS_R\\coursdeR")

# la base de l'ensae 
library(dplyr)
library(rwhatsapp)

ensae <- rwa_read("ensae.txt")%>% filter(!is.na(author))
colnames(ensae)

# la base des isep
isep <- rwa_read("isep.txt")%>% filter(!is.na(author))
colnames(ensae)

## chronologie des messages entre les deux groupes 

library(lubridate)
library(ggplot2)

ecole <- bind_rows(ensae %>% 
                      mutate(source = "ensae"),
                    isep %>% 
                      mutate(source = "isep")) %>%
  mutate(Temps = ymd_hms(time))

ggplot(ecole, aes(x = Temps, fill = source)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~source, ncol = 1)

## classement des personnes selon les messages  

# isep 

isep %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n, fill=author)) +
  geom_bar(stat = "identity") + xlab("Auteur") + ylab("Nombre de Messages") +
  coord_flip() + theme_bw() +
  ggtitle("classement des personnes")

# ensae 

ensae %>%
  count(author) %>%
  top_n(10, wt = n) %>%
  ggplot(aes(x = reorder(author, n), y = n, fill = author)) +
  geom_bar(stat = "identity") +
  xlab("Auteur") + ylab("Nombre de Messages") + coord_flip() + theme_bw() +
  ggtitle("classement des personnes")

## les mots fréquements utilisés par ces personnes 

# isep 


library(stopwords)
library(tidytext)
vide <- c(stopwords(language = 'fr'),"sticker","omis","absente","image",
          "document","message","supprimé","manquant","c'est","oui","a","ok",
          "pa","ça","hein","va","là")
isepnew <- isep%>% unnest_tokens(input = text,output = word) %>%
  filter(!word %in% vide)
isepnew %>% count(author, word, sort = TRUE) %>% 
  group_by(author) %>% 
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("Mots") +
  xlab("fréquence") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Mots fréquemment utilisés par chaque personne")


## etudions maintenant la base ecole
## Fréquences des mots

# notoyons d'abord la base ecole

ecolenew <- ecole%>% unnest_tokens(input = text,output = word) %>%
  filter(!word %in% vide)

# fréquences de mots pour selon la source 

fréquence <- ecolenew %>% 
  count(source, word, sort = TRUE) %>% 
  left_join(ecolenew %>% 
              count(source, name = "total")) %>%
  mutate(freq = n/total)

fréquence

# cadre de données de forme différente

library(tidyr)
fréquence <- fréquence %>% 
  select(source, word, freq) %>% 
  pivot_wider(names_from = source, values_from = freq) %>%
  arrange(isep, ensae)

fréquence

# compararaison des fréquences des mots entre les deux groupes 

library(scales)

ggplot(fréquence, aes(isep, ensae)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


## compararaison de l'utilisation des mots


word_ratios <- ecolenew %>%
  count(word, source) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = source, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(ensae /isep)) %>%
  arrange(desc(logratio))

word_ratios %>% arrange(abs(logratio))


# graphique 

word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("rapport de cotes logarithmique (ensae/isep)") +
  scale_fill_discrete(name = "", labels = c("ensae", "isep"))

# 

words_time <- ecolenew %>%
  mutate(time_floor = floor_date(time, unit = "1 month")) %>%
  count(time_floor, source, word) %>%
  group_by(source, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(source, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_time

# 

nested_data <- words_time %>%
  nest(data = c(-word, -source)) 

nested_data


# 

library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))
nested_models


#


library(broom)

slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>%filter(adjusted.p.value < 0.05)

top_slopes

# isep 

words_time %>%
  inner_join(top_slopes, by = c("word", "source")) %>%
  filter(source == "isep") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "mots frequents")

# ensae


words_time %>%
  inner_join(top_slopes, by = c("word", "source")) %>%
  filter(source == "ensae") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "mots frequents")       


      