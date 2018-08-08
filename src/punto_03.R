# funciones y librerias ---------------------------------------------------
source("src/load_librerias.R")
source("src/funciones.R")

# variables candidatas ----------------------------------------------------

# carga base merge de tweets
base_m <- readRDS("data/working/merged_raw.rds")
# crea variables nuevas
base3 <- base_m %>% 
  dplyr::mutate(
    # variable a predecir: pop (favorite_count y retweet_count: 
    # 1 si tiene ambas, 0 caso contrario)
    pop=dplyr::case_when(
      favorite_count>0 | retweet_count>0 ~ 1, 
      TRUE ~ 0
      ), 
    # categoricas candidatas transformadas para predecir pop:
    fuente=dplyr::case_when(
      source=="Twitter for Android" ~ "Android", 
      source=="Twitter Web Client" ~ "Web Client", 
      source=="Twitter for iPhone" ~ "iPhone", 
      TRUE ~ "Otros"
    ),
    media_type=unlist(media_type),
    tiene_media=dplyr::case_when(
      is.na(media_type) ~ 0L, 
      TRUE ~ 1L
    ),
    url_desc=dplyr::case_when(
      is.na(url) ~ 0L, 
      TRUE ~ 1L
    ),
    url_user=dplyr::case_when(
      is.na(profile_url) ~ 0L, 
      TRUE ~ 1L
    ),
    tiene_back=dplyr::case_when(
      is.na(profile_background_url) ~ 0L, 
      TRUE ~ 1L
    ),
    # numericas candidatas transformadas para predecir pop:
    num_char_text=nchar(text),
    num_char_desc=nchar(description),
    num_hash=map_dbl(hashtags, length_naomit),
    num_mentions=map_dbl(mentions_user_id, length_naomit),
    antig=difftime(created_at,account_created_at,"days") %>% floor %>% unclass
  )

# saveRDS(base3, file="data/working/base_punto3.rds")

# clase
clase <- base3$pop %>% as.factor
# base con clase y categoricas
base_cat <- base3 %>%
  dplyr::select(fuente,is_quote,
                tiene_media,url_desc,url_user,
                verified,account_lang,tiene_back,
                pop) %>% dplyr::mutate(pop=as.factor(pop))
  # sin clase
base_cat_noc <- base_cat %>% dplyr::select(-pop)
# base con clase y numericas
base_num <- base3 %>%
  dplyr::select(num_char_text, num_char_desc, num_hash, num_mentions, 
                followers_count, friends_count, listed_count, 
                statuses_count, antig, 
                favourites_count, pop) %>% dplyr::mutate(pop=as.factor(pop))
base_num_noc <- base_num %>% dplyr::select(-pop)
### para categoricas:
# gain ratio
df_gr <- FSelector::gain.ratio(pop~., data=base_cat) %>%
  dplyr::mutate(var=rownames(.)) %>% dplyr::select(2:1) %>% 
  dplyr::arrange(-attr_importance)
names(df_gr) <- c("Variable", "Attribute importance")
png_save(object=df_gr, file="output/gain_ratio.png")
# chisquare test
chi_list <- map(base_cat_noc, 
                function(x) table(x, as.factor(clase)) %>% chisq.test)
df_chi <- map_dfr(chi_list, function(x) x[3], .id="variable") %>%
  dplyr::arrange(p.value) %>% setNames(c("Variable","p-value"))
png_save(object=df_chi, file="output/chisq.png")

### para numericas:

# correlacion
cor_list <- map(base_num_noc, 
              function(x) cor.test(x, clase))
df_cor <- map_dfr(cor_list, broom::tidy, .id="variable") %>% 
  dplyr::select(variable, estimate, p.value) %>% dplyr::arrange(p.value) %>% 
  setNames(c("Variable", "Coefficient", "p-value")) 
png_save(object=df_cor, file="output/cor.png")

# solo con las var con cor significativa:
# boxplots
no_sig <- df_cor[df_cor$`p-value`>0.05,"Variable"]
sig <- base::setdiff(names(base_num_noc), no_sig)
box <- names(base_num_noc)[names(base_num_noc) %in% sig] %>% 
  map(function(v) graf_box(dframe=base_num, y_var=v, x_var="pop"))
box

# boxplots con bins por deciles
base_num_bin <- base_num_noc %>% dplyr::select(sig) %>% 
  # dplyr::select(favourites_count, statuses_count, listed_count, 
  #                 friends_count, followers_count, num_mentions, 
  #                 num_hash) %>% 
  dplyr::mutate_all(function(x) Hmisc::cut2(x, g=10) %>% as.numeric) %>% 
  cbind(as.factor(clase))
box_bin <- names(base_num_bin[-length(base_num_bin)]) %>% 
  map(function(v) graf_box(dframe=base_num_bin, y_var=v, x_var="clase"))
walk2("output/box_bins_"%+%seq_along(box_bin)%+%".png", box_bin, ggsave,
      width=8,height=5)

# boxplots eliminando outliers
n_obs <- 0.80*nrow(base_num) %>% floor
out_index <- base_num_noc %>% dplyr::select(sig) %>% 
  map(MASS::cov.rob, quantile.used=n_obs) %>% map(function(x) x$best)
base_num_nout <- map2(base_num_noc %>% dplyr::select(sig),
                          out_index, function(x,y) cbind(x[y],as.factor(clase)[y] %>% 
                                                           as.data.frame))
box_out <- list()
for (i in seq_along(out_index)) {
  names(base_num_nout[[i]]) <- c(names(base_num_nout[i]),"popular")
  box_out[[i]] <- graf_box(base_num_nout[[i]],
                           y_var=names(base_num_nout[i]), x_var="popular")
}
box_out

# correlacion eliminando outliers bivariados
bases_bi <- map(base_num_noc[sig], function(x) cbind(x,clase))
out_i_bi <- bases_bi %>% map(MASS::cov.rob, quantile.used=n_obs) %>%
  map(function(x) x$best)
df_corout <- map2(base_num_noc[sig], out_i_bi,
                  function(x,y) cor.test(x[y],clase[y])) %>% 
  map_dfr(broom::tidy, .id="variable") %>% 
  dplyr::select(variable, estimate, p.value) %>% dplyr::arrange(p.value) %>% 
  setNames(c("Variable", "Coefficient", "p-value")) 
png_save(object=df_corout, file="output/cor_outliers.png")
  
# logistic regression para cada variable
df_models <- list()
models <- list()
for (i in sig) {
  df_models[[i]] <- cbind(base_num_noc[[i]], clase) %>% as.data.frame
  models[[i]] <- glm(clase~., data=df_models[[i]])  
}
df_log <- map_dfr(models, broom::tidy, .id="Variable") %>% 
  dplyr::filter(term=="V1") %>% 
  dplyr::select(Variable, estimate, p.value) %>% dplyr::arrange(p.value)
png_save(df_log, file="output/logistic.png")


# # regresion lineal multiple
# base_lm <- cbind(base_cat, base_num)
# model <- lm(formula=pop~., base_lm) %>% summary
# df_reg <- data.frame(variables=rownames(model$coefficients),
#                      estimate=model$coefficients[,2],
#                      p.value=model$coefficients[,4]) %>% set_rownames(NULL) %>% 
#   dplyr::arrange(p.value)
# df_reg
# # algunas p.valores dan cualquiera por los outliers?






### popularidad: kmeans
  # usar caractiristicas del tweet o tmbn del usuario?
    # para mi lo mas razonable es tweet
# var_usuario: followers_count,friends_count,favourites_count,listed_count,statuses_count
# var_tweet: favorite_count,retweet_count
base_k <- base3 %>%
  dplyr::select(favorite_count,retweet_count)
base_k_s <- base_k %>% map_dfr(scale) # scale o minmax???
# indice HC para ver optimo (No lo usé al final)
# km_nb <- NbClust::NbClust(base_k_s, method="kmeans", max.nc=30)
# km_nb$All.index[,2] %>% sort(decreasing=T) # indice HC
# 5 clusters (muy baja - baja - media - alta - muy alta)
set.seed(1)
km <- kmeans(base_k_s,centers=5,nstart=50)
  # recodifico
oldv <- sort(unique(km$cluster)); newv <- c(5,2,1,4,3)
km$cluster %<>% {newv[match(.,oldv)]}
pop_k <- km$cluster %>% factor
k_plot <- factoextra::fviz_cluster(km, data=base_k, geom="point", show.clust.cent=F)
ggsave(filename="output/kmeans.png",plot=k_plot, width=7, height=4)
# indice HC posta
# fpc::calinhara(base_k_s, km$cluster)
table(pop_k)
# boxplots por variable (caracteristicas de cada cluster)
bplots_k <- vector("list")
base_box_k <- cbind(base_k_s, pop_k)
for (i in seq_along(base_box_k)) {
  nombre <- names(base_box_k[i]) 
  bplots_k[[nombre]] <- graf_box(dframe=base_box_k, y_var=nombre, x_var="pop_k")
  bplots_k[[length(base_box_k)]] <- NULL
}
walk2("output/box_kmeans_"%+%seq_along(bplots_k)%+%".png", bplots_k, ggsave,
      width=8,height=5)


### popularidad: PCA (scale o minmax?) - cp1 es el indicador
pca <- prcomp(base_k, scale=T)
# grafico pca
biplot_pca <- factoextra::fviz_pca_biplot(pca,label="var") 
ggsave(filename="output/pca_biplot.png", plot=biplot_pca, width=7, height=4)
# PC1
cp1 <- pca$x[,1]*(-1)
# 5 bins: equalwidth
pop_pcaw <- infotheo::discretize(cp1, "equalwidth", nbins=5) %>% unlist %>% factor
table(pop_pcaw)
# boxplots por variable (caracteristicas de cada cluster)
bplots_pcaw <- vector("list")
base_box_pcaw <- cbind(base_k_s, pop_pcaw)
for (i in seq_along(base_box_pcaw)) {
  nombre <- names(base_box_pcaw[i]) 
  bplots_pcaw[[nombre]] <- graf_box(dframe=base_box_pcaw, y_var=nombre, x_var="pop_pcaw")
  bplots_pcaw[[length(base_box_pcaw)]] <- NULL
}
walk2("output/box_pca_"%+%seq_along(bplots_pcaw)%+%".png", bplots_pcaw, ggsave,
      width=8,height=5)



# 5 bins: equalfreq -> no tiene sentido por alta variabilidad de la variable, se pierde info
# pop_pcaf <- infotheo::discretize(cp1, "equalfreq", nbins=3)
# table(pop_pcaf)
# plot(x=1:nrow(base_k), sort(cp1))
# la mayoria tiene 0 rt 0 fav
# para ver los intervalos:
# hpc <- hist(cp1, breaks=seq(-5,25,by=6))
# hpc$counts






# aclarar que se puede hacer PCA sobre variables binarias para incluirlas en kmeans


# para predecir pop :
# categoricas:
  # fuente (transformada)
  # is_quote
  # tiene_media (transformada)
  # url_desc (transformada: url en la descripcion (si o no))
  # url_user (transformada: url del user (si o no))
  # verified
  # account_lang 
  # tiene_back (transformada: 1 si tiene profile_background_url)
      # tweet con 100 palabras mas usadas (si o no)
# numericas:
  # num_char_text (cantidad de caracteres del tweet)
  # num_char_desc (cantidad de caracteres de descripcion)
  # num_hash (cantidad de hashtags (ver hashtags))
  # num_mentions (cantidad de menciones a otros users)
  # followers_count
  # friends_count
  # listed_count
  # statuses_count
  # antig (antiguedad de la cuenta: created_at - account_created_at)
  # favourites_count (cantidad de likes totales del usuario)
    # cantidad de palabras en tweet
    # cantidad de signos de puntuacion en tweet


# notas:
  # urls_url, urls_t_co y urls_expanded_url te mandan al mismo link
# para poner en la rpta:
# is_retweet no sirve porque no tiene variabilidad
# que es favourites_count?? diferencia con favorite_count
