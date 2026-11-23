
#-----------------0. Carga de librerías--------------------------------

install.packages("datamedios")
install.packages(c("dplyr", "purrr"))

library(datamedios)
library(dplyr)
library(purrr)


#------------------------------------------------------------------------
# ----------------1. Creación del corpus---------------------------------
#------------------------------------------------------------------------

# 1. Definir términos de búsqueda y rango temporal y fuentes-------------
terminos <- c("balacera", "encerrona", "robo armado")
inicio <- "2022-01-01"
fin <- "2025-12-31"
medios <- c("bbcl", "emol-todas", "latercera", "t13", "meganoticias", "chvnoticias")

# 2. Descargar y etiquetar cada término----------------------------------
descargar_termino <- function(termino) {
  message("Descargando noticias para: ", termino)
  df <- extraer_noticias_fecha(
    search_query = termino,
    fecha_inicio = inicio,
    fecha_fin = fin,
    fuentes = "todas"
  )
  # Agregamos una columna para saber qué término originó la captura
  if (nrow(df) > 0) {
    df$termino_busqueda <- termino
  }
  return(df)
}

# 3. Ejecutar la extracción iterativa y consolidar en una sola tabla----------
corpus_bruto <- map_dfr(terminos, descargar_termino)

# 4. Limpieza inicial: eliminar noticias duplicadas (misma URL o mismo titular)---
corpus_limpio <- corpus_bruto %>%
  distinct(url, .keep_all = TRUE) # Verifica el nombre exacto de la columna de enlace (ej. url o link)

# 5. Guardar el corpus congelado para asegurar reproducibilidad en el .zip final----
saveRDS(corpus_limpio, file = "corpus_prensa_2022_2025.rds")

# Revisar qué columnas son de tipo lista (opcional, para que sepas cuál es)
str(corpus_limpio)

# Aplanar cualquier columna que sea lista uniéndola con " | "
corpus_csv <- corpus_limpio %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.x), collapse = " | "))))


# Guardar el CSV ahora sin errores
write.csv(corpus_csv, file = "corpus_prensa_2022_2025.csv", row.names = FALSE)
