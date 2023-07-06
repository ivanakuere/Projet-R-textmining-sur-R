getwd()
setwd("C:\\Users\\LENOVO\\Desktop\\projet_R")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("textclean")
installed.packages("ggplot2")
installed.packages("igraph")
installed.packages("ggraph")
install.packages("stringr")
library("readxl")
library("dplyr")
library("tidytext")
library("plyr")
library("sentimentr")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("tidyr")
library("igraph")
library("ggraph")
set.seed(2017)


# importation  de la base
df<-read_xlsx("C:\\Users\\LENOVO\\Desktop\\projet_R\\lait.xlsx")

#definition de la base 

base <- df %>% dplyr:: distinct(text, .keep_all = TRUE)
base <- df[1:100,] %>% dplyr::select(text)
                                     

##Numérotation des éléments
base1<-tibble(index_person=1:100, opinion=base$text)
View(base1)


##creation des tokens des mots indexés

base_mot<-base1 %>% 
  tidytext::unnest_tokens(word, opinion)

##Suppression des mots vides
base_mot<-base_mot%>%anti_join(stop_words)

#creations du dictionnaire des mots vides
dictionnaire<-data.frame(word =c("t.co","to","i","are","https","the","their","his","to","for","i","I","l","we","my","of","from","a","at","he","that","so","if","our"))
                         
texte_filtre<-anti_join(base_mot,dictionnaire,by="word")
ivi<-base_mot %>%
  dplyr::filter(!word %in%dictionnaire )
#SUpprimer les chiffres
texte_filtre<-texte_filtre %>% filter(!grepl("\\d",word))


## fréquence des mots
frequence_mot<-base_mot%>%dplyr::count(word,sort=TRUE)%>%
  filter(!word %in% dictionnaire$word) %>% 
  filter(n > 20)

##histogramme des mots
base_mot %>%
  dplyr::count(word, sort = TRUE) %>%filter(!word %in% dictionnaire$word) %>% 
  filter(n > 35) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  ylab(" mots")+
  xlab("fréquence des mots")+
  ggtitle("Histogramme des mots les plus fréquents")

##Mots intéressant
mot_inter<-frequence_mot[1:7,] %>% dplyr::select(word)

### nuages de mots avec tm
dtm <- TermDocumentMatrix(base_mot%>%dplyr::mutate(word=stringr::str_replace_all(.$word, "’", " ")) %>% filter(!word %in% dictionnaire$word))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)



set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10000,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
title(main = "Nuages des mots fréquents")

#nuages des mots


# base2 <- base1 %>%
#   dplyr::mutate(text = stringr::str_replace_all(.$opinion, "’", " ")) 
# base2<- base2 %>%  tidytext::unnest_tokens(word, text) %>% 
#   filter(!word %in% dictionnaire$word) 
# base2 <- base2 %>%dplyr::count(word, sort = TRUE)%>% 
#   filter(n > 20)

base2 <- base1 %>%
  dplyr::mutate(text = stringr::str_replace_all(.$opinion, "’", " "))  %>%
  tidytext::unnest_tokens(word, text) %>% 
  filter(!word %in% dictionnaire$word) %>%
  dplyr::count(word, sort = TRUE)%>% 
  filter(n > 20)


wordcloud(base2$word, base2$n, max.words = 200, rot.per = FALSE, colors = c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))
title(main = "Nuages des mots fréquents")

