# ---- PASO 0 · Paquetes ------------------------------------------------
# Toda la unidad usa la misma familia de paquetes: quanteda.
install.packages(c("quanteda", "quanteda.textstats", "quanteda.textplots", "readtext"))
rm (list = ls())
library(quanteda)            # corpus, tokens, matriz documento-término
library(quanteda.textstats)  # frecuencias y palabras distintivas
library(quanteda.textplots)  # gráficos
library(readtext)            # leer archivos de texto (txt, pdf, docx...)
library(stringr)             # limpieza de texto
library(dplyr)

# ---- PASO 1 · Leer el corpus y sus metadatos --------------------------
datos_prensa <- readRDS("input/corpus_prensa_2022_2025.rds")
names(datos_prensa)

# No es necesario codificar a UTF-8 porque ya vienen codificados desde el web scraping desde datamedios

#revisar nombres de columnas
names(datos_prensa)

#Definir metadatos y criterios de inclusión
noticias <- datos_prensa %>%
  filter(!is.na(contenido), nchar(trimws(contenido)) > 0) %>%
  mutate(
    #identificador único para cada documento
    doc_id = paste0("not_", row_number()),
    #incorporación de título y cuerpo para analisis
    texto_completo = paste(titulo, contenido, sep = ". "),
    # Extraer el año
    anio = as.integer(str_extract(fecha, "\\d{4}")),
    medio = as.character(medio)
  ) %>%
  # Asegurar que solo entren noticias del rango 2022-2025
filter(
    anio >= 2022 & anio <= 2025,
    medio %in% c("bbcl", "emol", "mediosregionales") # Excluye ciper (n=1) y guioteca (n=31)
  )

# Control 
substr(noticias$texto_completo, 1, 80)
table(noticias$medio, noticias$anio)


# ---- PASO 2 · Limpiar ruido y armar el corpus ----

#Se aplican criterios de limpieza distintos a taller 3 pq fuentes son digitales
limpiar_web <- function(x) {
  x <- gsub("<[^>]+>", " ", x)                      # 1. Elimina etiquetas HTML (<p>, <br>, etc.)
  x <- gsub("&#8220;|&#8221;|&quot;", " ", x)       # 2. Elimina códigos HTML de comillas
  x <- gsub("&nbsp;|&#160;", " ", x)                # 3. Elimina espacios duros HTML (nbsp)
  x <- gsub("&[a-zA-Z0-9#]+;", " ", x)              # 4. Elimina cualquier entidad HTML residual 
  x <- gsub("https?://\\S+|www\\.\\S+", " ", x)     # 5. Elimina URLs incrustadas
  x <- gsub("\\s+", " ", x)                         # 6. Colapsa espacios múltiples
  trimws(x)
}
noticias$texto_completo <- limpiar_web(noticias$texto_completo)

# Crear el objeto corpus de quanteda
corp <- corpus(noticias, docid_field = "doc_id", text_field = "texto_completo")
summary(corp, n = 10)

# Comprobación: ¿quedó bien unida la palabra?
kwic(tokens(corp), "balacera", window = 4)  

# ---- PASO 3 · Filtrar unidad de análisis (Noticias válidas) -----------
# A diferencia de los mensajes presidenciales, aquí cada NOTICIA es un documento.
# Eliminamos notas de menos de 30 tokens (suelen ser videos sin cuerpo o avisos breves)
corp_valido <- corpus_subset(corp, ntoken(corp) >= 30)

cat("Noticias iniciales:", ndoc(corp), "\n")
cat("Noticias válidas (>= 30 tokens):", ndoc(corp_valido), "\n") #total 4088
table(docvars(corp_valido, "medio"))  #total 4088, queda igual


# ---- PASO 4 · Tokenizar y preprocesar --------------------------------
toks <- tokens(corp_valido,
               remove_punct   = TRUE,   # quita puntuación
               remove_numbers = TRUE,   # quita cifras y fechas
               remove_symbols = TRUE,   # quita símbolos ($, %, etc.)
               remove_url     = TRUE)
toks <- tokens_tolower(toks)

# 4.1. Frases musicales que se relacionan al género urbano
frases_musicales <- c(
  "música urbana", "musica urbana",
  "género urbano", "genero urbano",
  "cantante urbano", "cantantes urbanos", "cantante urbana",
  "artista urbano", "artistas urbanos", "artista urbana",
  "escena urbana", "movimiento urbano", "exponente urbano", "exponentes urbanos"
)
# 4.2. Frases urbanas que son RUIDO (se unen para que no se confundan con "urbano/a" suelto)
# esto se aplica posterior a la revisión del kwic, donde se ve la utilización de "urbano" con este significado

  frases_ruido_urbano <- c(
  "zona urbana", "zonas urbanas",
  "área urbana", "area urbana", "áreas urbanas",
  "radio urbano", "sector urbano", "sectores urbanos",
  "espacio urbano", "espacios urbanos",
  "autopista urbana", "autopistas urbanas",
  "transporte urbano", "locomoción urbana", "bus urbano", "buses urbanos",
  "mobiliario urbano", "parque urbano", "centro urbano", "entorno urbano",
  "desarrollo urbano", "planificación urbana", "suelo urbano", "límite urbano"
)

toks <- tokens_compound(toks, phrase(c(frases_musicales, frases_ruido_urbano)))



# Palabras funcionales (stopwords) del castellano
ruido_prensa <- c(stopwords("es"), "lee", "también", "además", "tras", "según", "foto", "agencia")
toks_sin <- tokens_remove(toks, ruido_prensa)

# Comprobar corpus con y sin stopwords
sum(ntoken(toks))      # 1.262.994 palabras con stopwords
sum(ntoken(toks_sin))  # 655.161 sin ellas: se fue el 51%

# ---- PASO 5 · La matriz documento-término ----------------------------
# Filas = párrafos, columnas = palabras, celdas = cuántas veces aparece.
dtm <- dfm(toks_sin)
dtm                                   # dimensiones y porcentaje de ceros

dtm <- dfm_trim(dtm, min_termfreq = 5)   # palabras que aparecen 5+ veces
dtm #la nueva matriz tiene 10.751 features

#Para mirar la matriz dtm por dentro:
dtm[1:5, 1:8]           # las 5 primeras filas y 8 primeras columnas
topfeatures(dtm, 25)    # las 25 palabras más frecuentes de todo el corpus
nfeat(dtm); ndoc(dtm)   # número de columnas y de filas


# ---- PASO 6 · Explorar: lo mismo que Voyant, pero reproducible -------
# 6a. Las 12 palabras más frecuentes de cada medio
textstat_frequency(dtm, n = 12, groups = medio)

#Nube de palabras general
set.seed(2026)
textplot_wordcloud(dtm, max_words = 80,
                   color = rev(hcl.colors(6, "viridis"))) 

# 6b. Palabras en contexto (KWIC) 
# Buscamos menciones a lo urbano/musical dentro de las noticias policiales
kwic_urbano <- kwic(toks, pattern = c("música_urbana", "musica_urbana", "género_urbano", 
                                      "trap", "reggaetón", "reggaeton", "urbano", "urbana"), 
                    window = 10)
kwic_urbano
# se identifican multiples utilizaciones de "urbano" con otros significados, se realiza una modificación en 
# el paso 4 para quitar este ruido


# 6d. Evolución temporal (2022-2025) por cada 10.000 palabras
dtm_anio <- dfm_group(dfm(toks), groups = anio)
relativa <- dfm_weight(dtm_anio, scheme = "prop") * 10000

# Revisa qué términos de tu lista existen efectivamente en la matriz antes de graficar
terminos_interes <- intersect(
  c("música_urbana", "trap", "reggaetón", "menores_de_edad", "jóvenes", "balacera", "encerrona"),
  featnames(relativa)
)

m <- as.matrix(relativa[, terminos_interes])
round(m, 2)

anios <- as.integer(rownames(m))
colores <- hcl.colors(ncol(m), "Dark 3")

matplot(anios, m, type = "b", pch = 19, lty = 1, col = colores,
        xlab = "Año", ylab = "Frecuencia por cada 10.000 palabras",
        main = "Evolución de términos clave en prensa policial (2022-2025)",
        xaxt = "n")
axis(1, at = anios, labels = anios)
legend("topright", colnames(m), col = colores, lty = 1, pch = 19, cex = 0.8)


# Pares de palabras que aparecen juntos más de lo esperable por azar.
textstat_collocations(toks_sin, size = 2, min_count = 10) |> head(15) #atención: usamos toks_sin. 

# ---- 7. Guardar corpus procesado ------------------------------------------------
saveRDS(corp_valido, "input/corpus_prensa_proc.rds")
saveRDS(toks,        "input/toks_prensa.rds")
saveRDS(toks_sin,    "input/toks_sin_prensa.rds")
saveRDS(dtm, "input/dtm_prensa_urbana.rds")
