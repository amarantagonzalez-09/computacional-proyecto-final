
rm(list=ls())

# ---- PASO 0 · Paquetes -------------------------------------------------

#install.packages(c("quanteda", "seededlda", "readtext", "quanteda.textplots"))

library(quanteda)
library(seededlda)
library(readtext)
library(quanteda.textplots)
library(quanteda.textstats)

getwd()                                          
file.exists("input/corpus_prensa_proc.rds")       #TRUE

# ---- PASO 1 · Leer el corpus, unidad de analisis y DFM --------------------------
# Cargar objetos ya limpios y tokenizados desde el Script 1
corp     <- readRDS("input/corpus_prensa_proc.rds")
toks     <- readRDS("input/toks_prensa.rds")
toks_sin <- readRDS("input/toks_sin_prensa.rds")
dtm      <- readRDS("input/dtm_prensa_urbana.rds")

texto        <- as.character(corp)
orden_medios <- c("bbcl", "emol", "mediosregionales")


# ---- PASO 3 · Diccionario de semillas ----------
# Hipótesis temática: Relación entre  música urbana y violencia armada juvenil

dic_prensa <- dictionary(list(
  musica_urbana = c("música_urbana", "musica_urbana", "género_urbano", "genero_urbano",
                    "cantante_urbano", "cantantes_urbanos", "artista_urbano", "artistas_urbanos",
                    "escena_urbana", "trap", "reggaetón", "reggaeton", "reguetón", "videoclip*", 
                    "narcocultura"),
  violencia_juvenil = c("menor_de_edad", "menores_de_edad", "adolescent*", "jóvenes", "joven",
                        "escolar*", "liceo*", "colegio*", "pandilla*", "inimputable*")
))


dic <- dic_prensa
marcos <- names(dic)
dic

# ¿Qué palabras de la matriz calzan con cada lista? ¡Revíselo siempre!
# Si una semilla tiene 0 apariciones, no sirve: cámbiela.
for (m in marcos) {
  cat("\n", m, "\n")
  print(sort(colSums(dfm_select(dtm, pattern = dic[m])), decreasing = TRUE))
}
semillas <- featnames(dfm_select(dtm, pattern = dic))

#Para revisar si están las palabras en el corpus
kwic(toks, pattern = c("cantante_urbano"), case_insensitive = TRUE)

# ---- PASO 4 · Estimar el modelo de tópicos semillado --------------------
# residual = 12: además de sus tópicos sembrados, 12 tópicos libres para todo
# lo demás

set.seed(2026)                                   

#slda <- textmodel_seededlda(dtm, dictionary = dic, residual = 12)
#saveRDS(slda, "input/slda_prensa_urbana.rds")  

# Para volver a cargarlo otro día sin estimarlo de nuevo:
slda <- readRDS("input/slda_prensa_urbana.rds")


# ---- PASO 5 · Interpretar: semillas y palabras descubiertas -------------
terms(slda, 15)[, marcos]     # las 15 palabras más probables de sus tópicos

# Las palabras que el modelo AGREGÓ (entre las 20 más probables, sin ser semillas)
top20 <- terms(slda, 20)
for (m in marcos) cat("\n", m, "->", paste(setdiff(top20[, m], semillas), collapse = ", "), "\n")

# Pregunta para el informe: ¿las palabras descubiertas confirman su concepto,
# lo amplían o muestran que el tópico mide OTRA cosa?

# ---- PASO 6. Cruzar Diccionario (Música-urbana) + Tópico (Juventud) ----
# Extraer medio y anio desde los metadatos de dtm (detecta si la columna se llama "medio" o "medio_consultado")
col_medio <- ifelse("medio" %in% names(docvars(dtm)), "medio", "medio_consultado")
orden_medios <- c("bbcl", "emol", "mediosregionales")

medio <- factor(docvars(dtm, col_medio), levels = orden_medios)
anio  <- docvars(dtm, "anio")

# 1. Identificar noticias que mencionan explícitamente música urbana (Diccionario exacto)
menciones_musica <- as.numeric(dfm_lookup(dtm, dic["musica_urbana"]))
tiene_musica <- ifelse(menciones_musica > 0, "Con_Musica_Urbana", "Sin_Musica_Urbana")
docvars(dtm, "tiene_musica") <- tiene_musica

# A) ¿En qué medida aparece la música urbana por medio y año? (Frecuencias exactas)
tabla_medio <- prop.table(table(medio, tiene_musica), 1) * 100
round(tabla_medio, 2)

tabla_anio <- prop.table(table(anio, tiene_musica), 1) * 100
round(tabla_anio, 2)

# B) ¿Se asocia con la violencia juvenil? Comparamos el peso (theta) del tópico
#    violencia_juvenil entre noticias CON mención musical vs SIN mención musical
theta_juv <- slda$theta[, "violencia_juvenil"]
tapply(theta_juv, tiene_musica, mean) * 100
tapply(theta_juv, tiene_musica, median) * 100
wilcox.test(theta_juv ~ tiene_musica) # Prueba no paramétrica de diferencia de grupos

# C) ¿Bajo qué encuadres discursivos? Palabras distintivas (Keyness) de las noticias
#    que mencionan música urbana frente al resto de noticias policiales
dtm_grupo_mus <- dfm_group(dtm, groups = tiene_musica)
key_mus <- textstat_keyness(dtm_grupo_mus, target = "Con_Musica_Urbana")
head(key_mus, 20)
textplot_keyness(key_mus, n = 15, color = c("#4E3239", "#2A5260"), labelsize = 3.5)


# ---- PASO 7 · Validación manual sobre noticias reales ------------
# a) Validación manual
# Muestra aleatoria de 25 noticias que SÍ contienen palabras del diccionario musical
set.seed(2026)
candidatos <- which(menciones_musica > 0)
elegidos <- sample(candidatos, min(25, length(candidatos)))

muestra <- data.frame(
  noticia = names(texto)[elegidos],
  medio   = medio[elegidos],
  anio    = anio[elegidos],
  menciones_dic = menciones_musica[elegidos],
  theta_juvenil = round(theta_juv[elegidos], 3),
  texto   = texto[elegidos],
  atribuye_violencia = "" # Codificar en Excel: 1 = atribuye delito a música urbana, 0 = mención incidental
)

#write.csv(muestra, "output/muestra_validacion_musica_urbana.csv", row.names = FALSE,
 #         fileEncoding = "UTF-8")


cod <- read.csv("output/muestra_validacion_musica_urbana.csv", encoding = "UTF-8")

# Tabla de frecuencias ( 23 ceros y 2 unos)
table(cod$atribuye_violencia)

# Porcentaje de atribución efectiva (8.0 %)
round(mean(cod$atribuye_violencia == 1) * 100, 1)

# b) Palabra AMBIGUA en contexto (KWIC de "urbano" y "urbana" residuales)
palabra <- c("urbano", "urbana")
k_amb <- kwic(toks, palabra, window = 2)
sort(table(k_amb$pre), decreasing = TRUE)[1:15]
sort(table(k_amb$post), decreasing = TRUE)[1:15]


# c) Prueba de robustez: aumentar tópicos libres 
## c.1 Prueba de robustez de violencia_juvenil

# #se opta por cambiar número de tópicos libres a 18 en vez de 12
set.seed(2026)
slda_rob <- textmodel_seededlda(dtm, dictionary = dic, residual = 18)

# 1. Verificar que el tópico violencia_juvenil conserva sus palabras clave
terms(slda_rob, 10)[, "violencia_juvenil"]

# 2. Comparar el peso promedio (%) de violencia_juvenil por medio (original vs. robustez)
prev_juv_orig <- tapply(slda$theta[, "violencia_juvenil"], medio, mean) * 100
prev_juv_rob  <- tapply(slda_rob$theta[, "violencia_juvenil"], medio, mean) * 100
round(cbind(original = prev_juv_orig, robustez = prev_juv_rob), 2)

# 3. Verificar que la comparación entre noticias con y sin música urbana no cambia
tapply(slda_rob$theta[, "violencia_juvenil"], tiene_musica, mean) * 100
wilcox.test(slda_rob$theta[, "violencia_juvenil"] ~ tiene_musica)

# ---- PASO 8 · Coocurrencias: la red alrededor de música urbana ----------

# Asegurar que toks_sin tenga la variable "medio" con los niveles ordenados
docvars(toks_sin, "medio") <- medio

# Usamos el diccionario completo de música urbana para no perder casos en medios pequeños
clave_dic <- dic["musica_urbana"]

red_clave <- function(sub_toks, n = 25) {
  cerca <- tokens_select(sub_toks, pattern = clave_dic, window = 8)
  f <- fcm(cerca, context = "window", window = 8)
  top_f <- names(topfeatures(dfm(cerca), n))
  fcm_select(f, pattern = top_f)
}

# a) Vecinos más frecuentes en todo el corpus y por medio (bbcl vs emol)
cerca_total <- tokens_remove(tokens_select(toks_sin, pattern = clave_dic, window = 8),
                             pattern = clave_dic)
cat("\n--- Vecinos en TODO EL CORPUS ---\n")
print(topfeatures(dfm(cerca_total), 15))

for (med in c("bbcl", "emol")) {
  cerca_med <- tokens_remove(
    tokens_select(tokens_subset(toks_sin, medio == med), pattern = clave_dic, window = 8),
    pattern = clave_dic
  )
  cat("\n--- Vecinos en:", med, "---\n")
  print(topfeatures(dfm(cerca_med), 15))
}

# b) Red de coocurrencia general del corpus (recomendada para el informe por mayor densidad)
set.seed(2026)
textplot_network(red_clave(toks_sin, n = 30), min_freq = 0.3,
                 edge_color = "#2A5260", vertex_color = "#4E3239")

# c) Redes comparadas por medio (usando min_freq bajo en emol por tener pocos casos)
set.seed(2026)
textplot_network(red_clave(tokens_subset(toks_sin, medio == "bbcl"), n = 25),
                 min_freq = 0.3, edge_color = "#2A5260", vertex_color = "#4E3239")

set.seed(2026)
textplot_network(red_clave(tokens_subset(toks_sin, medio == "emol"), n = 20),
                 min_freq = 0.1, edge_color = "#2A5260", vertex_color = "#4E3239")


# ---- PASO 9 · Guardar resultados y dejar constancia (Anexo A) ---------
dir.create("output", showWarnings = FALSE)

write.csv(as.data.frame.matrix(round(tabla_medio, 2)),
          "output/resultados_musica_por_medio.csv", row.names = TRUE, fileEncoding = "UTF-8")

write.csv(as.data.frame.matrix(round(tabla_anio, 2)),
          "output/resultados_musica_por_anio.csv", row.names = TRUE, fileEncoding = "UTF-8")

write.csv(head(key_mus, 30),
          "output/resultados_keyness_musica.csv", row.names = FALSE, fileEncoding = "UTF-8")

tab_robustez <- round(cbind(original = prev_juv_orig, robustez = prev_juv_rob), 2)
write.csv(as.data.frame(tab_robustez),
          "output/resultados_robustez_juvenil.csv", row.names = TRUE, fileEncoding = "UTF-8")
