# TOKEN BUSCADO - Aqui se establece el token que dio origen a la DB
query <- "Carrió"
# ruta de mongod.exe
mongo_path <- "C:/Program Files/MongoDB/Server/3.6/bin"

# cargo librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# conecta con MongoDB
shell.exec(mongo_path%+%"/mongod.exe")

# conecta/crea colecciones y DB con MongoDB
  # tweets
colec_tweets <- mongolite::mongo(collection="tweets_mongo-"%+%query,
                                db="maestriadm_datamining")
  # usuarios
colec_users <- mongolite::mongo(collection="users_mongo-"%+%query,
                                db="maestriadm_datamining")
# Inserta los json como documentos en cada coleccion si no existen
if (colec_tweets$count==0) {
  colec_tweets$import(file(here("data","raw",query%+%"-dump_tweets.json")))
}
if (colec_users$count==0) {
colec_users$import(file(here("data","raw",query%+%"-dump_users.json")))
}
# genera y dataframes de tweets y de usuarios y los guarda como objetos
base_t <- colec_tweets$find()
saveRDS(base_t, file=here("data","working","tweets_raw.rds"))
base_u <- colec_users$find() %>% unique 
saveRDS(base_u, file=here("data","working","users_raw.rds"))
    # habia filas repetidas en users asi que las sacamos 
# merge de bases en base a user_id
base_merge <- base_t %>% dplyr::left_join(base_u, by="user_id")
saveRDS(base_merge, file=here("data","working","merged_raw.rds"))


