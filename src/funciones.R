source("src/load_librerias.R")
# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# limpia strings guardados en vector
clean_text <- function(char_vec, rem_mention=T, rem_numbers=T, rem_hashtag=F) {
  library(magrittr)
  
  patt <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
  repl <- c('e', '...', '-', "'", '~', '＞', '+', 'and')
  
  # stopwords sin tildes:
  stopw <- c(tm::stopwords("es"),
             "si","van","asi","veo","tal","dijo","voy") %>%
    stringi::stri_trans_general(., "Latin-ASCII")
  
  clean <- char_vec %>% 
    # pasa de utf8 a ascii (no se si sirve)
    qdap::mgsub(pattern=patt, replacement=repl, .) %>% 
    # saca html symbols (no se si sirve)
    stringr::str_replace_all('&[a-z]{1,6};', ' ') %>% 
    # saca emojis (o cosas con formato <U+...>)
    stringr::str_replace_all('<U\\+\\w+>', ' ') %>%
    # saca cualquier cosa entre <> (porque quedaban otras!)
    stringr::str_replace_all('<[:graph:]+>', ' ') %>% 
    # saca urls
    stringr::str_replace_all('http[^[:space:]]*', ' ') %>%  
    # saca menciones
    {if (rem_mention==T) stringr::str_replace_all(.,'@\\S+', ' ') else .} %>% 
    # saca hashtags
    {if (rem_hashtag==T) stringr::str_replace_all(.,'#\\S+', ' ') else .} %>%
    # saca numeros aislados
    {if (rem_numbers==T) stringr::str_replace_all(.,'\\b\\d+\\b', ' ') else .} %>% 
    # saca puntuacion
    stringr::str_replace_all('[[:punct:] ]+', ' ') %>%
    # saca todo lo que no sea numeros, letras y espacios
    stringr::str_replace_all('[^[:alnum:][:space:]]', ' ') %>%
    # lower case
    tolower(.) %>% 
    # saca cualquier caracter aislado -entre espacios-
    stringr::str_replace_all('[:space:].[:space:]', ' ') %>%
    # reemplaza acentos y eñes
    stringi::stri_trans_general(., "Latin-ASCII") %>% 
    # saca stopwords castellano
    tm::removeWords(., stopw) %>%
    # saca whitespace irrelevante y ws ppio y final
    tm::stripWhitespace(.) %>% trimws(.)
  return(clean)
} 

# largo de un vector sin contar NA:
length_naomit <- function(x) {
  length(x[!is.na(x)])
}

# boxplot de una variable segun niveles de un factor (vbles quoted):
graf_box <- function(dframe, y_var, x_var) {
  xq <- as.name(x_var); yq <- as.name(y_var)
  g <- ggplot(dframe) + 
    geom_boxplot(aes_q(x=xq, y=yq)) +
    labs(title=y_var) + theme(plot.title=element_text(size=22))
}

# transformacion minmax:
minmax <- function(x) (x-min(x))/(max(x)-min(x))

# guarda dataframe como png
png_save <- function(object, file, round=4) {
  library(gridExtra); library(dplyr)
  df <- dplyr::mutate_if(object, is.numeric, function(x) round(x,round))
  if (!is.null(dev.list())) dev.off()
  png(filename=file, bg="transparent")
  grid.table(df, rows=NULL)
  dev.off()
}

# transforma factor en numueric
fac_to_num <- function(x) as.numeric(levels(x))[x] 

# tabla frecuencias para var categórica:
tabfreq <- function(database,var) {
  dat <- database[[var]]
  f <- table(dat, useNA="always")
  fr <- f/length(dat)*100
  f_cum <- cumsum(f)
  fr_cum <- cumsum(fr)
  return(rbind(f,fr,f_cum,fr_cum) %>% round(1))
}
