query <- "carrio"

# funciones y librerias ---------------------------------------------------
source("src/load_librerias.R")
source("src/funciones.R")

# ?? ----------------------------------------------------------------------
# carga bases de tweets
base_t <- readRDS("data/working/tweets_raw.rds")
t_raw <- base_t$text
# limpia los tweets y saca la palabra carrio
t_clean <- clean_text(t_raw) %>% 
  stringr::str_replace_all('carrio', '') %>% trimws
# corpus (para que cazzo sirve)
corpus <- t_clean %>%
  # enc2utf8 %>% # que hace?
  tm::VectorSource(x=.) %>% tm::Corpus(.)
# termdocmatrix
tdm <- tm::TermDocumentMatrix(corpus)
# frecuencias por palabra
fr <- as.matrix(tdm) %>% rowSums %>% sort(decreasing=T)
# wordcloud
set.seed(123)
png(filename="output/wordcloud.png", bg="transparent",
    width=12, height=8, units="in", res=300)
wordcloud::wordcloud(words=names(fr),
                     freq=fr,
                     scale=c(3,0.5),
                     random.order=F,
                     min.freq=6,
                     colors=RColorBrewer::brewer.pal(8,"Dark2"),
                     rot.per=0.35)
dev.off()
# grafico de frecuencias
fr_df <- data.frame(word=names(fr),freq=fr) %>% 
  set_rownames(NULL) %>%
  dplyr::top_n(15)
word_freq <- ggplot(data=fr_df) +
  geom_col(aes(x=reorder(word,freq), y=freq)) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frecuencia",
       x = "Palabras")
ggsave(filename="output/word_freq.png",plot=word_freq)
