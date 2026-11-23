

#NO ESTÁ LISTOOOO


rm(list=ls())

# ---- PASO 0 · Paquetes -------------------------------------------------
# Los mismos de las sesiones 8, 9 y 10. Si falta alguno, quite el # y corra UNA vez:
install.packages(c("quanteda", "seededlda", "readtext", "quanteda.textplots"))

library(quanteda)
library(seededlda)
library(readtext)
library(quanteda.textplots)

getwd()                                          
file.exists("input/corpus_prensa_proc.rds")       #TRUE

# ---- PASO 1 · Leer el corpus, unidad de analisis y DFM --------------------------
# Cargar objetos ya limpios y tokenizados desde el Script 1
corp     <- readRDS("corp_prensa_valido.rds")
toks     <- readRDS("toks_prensa.rds")
toks_sin <- readRDS("toks_sin_prensa.rds")
dtm      <- readRDS("dtm_prensa_urbana.rds")

texto        <- as.character(corp)
orden_medios <- c("bbcl", "emol", "mediosregionales")





# ---- PASO 3 · Diccionario de semillas ----------
# Hipótesis temática: contrastamos el encuadre cultural (música urbana) y etario
# (violencia juvenil) frente al encuadre criminalístico/policial estándar.

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
#kwic(toks, pattern = c("derechos_humanos"), case_insensitive = TRUE)

# ---- PASO 4 · Estimar el modelo de tópicos semillado --------------------
# residual = 12: además de sus tópicos sembrados, 12 tópicos libres para todo
# lo demás

set.seed(2026)                                   
slda <- textmodel_seededlda(dtm, dictionary = dic, residual = 12)
saveRDS(slda, "input/slda_prensa_urbana.rds")  

# Para volver a cargarlo otro día sin estimarlo de nuevo:
# slda <- readRDS("input/slda_prensa_urbana.rds")


# ---- PASO 5 · Interpretar: semillas y palabras descubiertas -------------
terms(slda, 15)[, marcos]     # las 15 palabras más probables de sus tópicos

# Las palabras que el modelo AGREGÓ (entre las 20 más probables, sin ser semillas)
top20 <- terms(slda, 20)
for (m in marcos) cat("\n", m, "->", paste(setdiff(top20[, m], semillas), collapse = ", "), "\n")

# Pregunta para el informe: ¿las palabras descubiertas confirman su concepto,
# lo amplían o muestran que el tópico mide OTRA cosa?


# ---- PASO 6 · Medir: ¿cuánto pesa cada tópico, en quién y cuándo? -------
theta <- slda$theta
gob   <- factor(docvars(dtm, "medio"), levels = orden_medios)
anio  <- docvars(dtm, "anio")

# a) Peso promedio (theta, en %) por medio de comunicación
prev_medio <- aggregate(theta[, marcos, drop = FALSE], by = list(medio = medio), FUN = mean)
prev_medio[, marcos] <- round(prev_medio[, marcos] * 100, 2)
prev_medio

# b) % de párrafos cuyo tópico PRINCIPAL es cada uno de los tópicos sembrados
principal <- topics(slda)
round(prop.table(table(medio, principal), 1)[, marcos] * 100, 2)

# c) Gráfico por año (2022-2025)
prev_anio <- aggregate(theta[, marcos, drop = FALSE], by = list(anio = anio), FUN = mean)
colores <- c("#4E3239", "#2A5260", "#9FB4BD")[seq_along(marcos)]

matplot(prev_anio$anio, prev_anio[, marcos] * 100, type = "b", pch = 19, lty = 1,
        col = colores, xlab = "Año", ylab = "Peso promedio del tópico (%)",
        main = "Prevalencia de marcos en noticias de delitos armados (2022-2025)",
        xaxt = "n")
axis(1, at = prev_anio$anio, labels = prev_anio$anio)
legend("topright", marcos, col = colores, lty = 1, pch = 19, bty = "n")


# d) Contraste teoría-first: el diccionario solo (palabras por cada 1.000)
cuenta_dic  <- dfm_group(dfm_lookup(dtm, dic), groups = medio)
total_medio <- ntoken(dfm_group(dtm, groups = medio))
round(as.matrix(cuenta_dic) / total_medio * 1000, 2)[orden_medios, ]


# ---- PASO 7 · Validar ------------------------------------------------------
m <- "musica_urbana"   #validación de tópico central

for (i in order(theta[, m], decreasing = TRUE)[1:5]) {
  cat("\n#", names(texto)[i], " (", docvars(dtm, "medio")[i], docvars(dtm, "anio")[i], ")",
      " theta =", round(theta[i, m], 3), "\n", substr(texto[i], 1, 500), "...\n")
}

# b) Muestra al azar para CODIFICAR A MANO: 20 párrafos cuyo tópico principal es m.
#    Se guarda en un archivo .csv que pueden abrir en Excel. En la columna
#    "codigo" escriban 1 (sí trata del concepto), 0 (no) o 9 (duda).

#    (Como musica_urbana puede ser un tópico minoritario en el total policial, tomamos las
#     noticias donde es tópico principal o está en el percentil superior de theta)

set.seed(2026)
candidatos <- which(principal == m | theta[, m] >= quantile(theta[, m], 0.98))
elegidos <- sample(candidatos, min(25, length(candidatos)))

muestra <- data.frame(
  noticia = names(texto)[elegidos],
  medio   = medio[elegidos],
  anio    = anio[elegidos],
  theta   = round(theta[elegidos, m], 3),
  texto   = texto[elegidos],
  atribuye_violencia = ""  # Codificar en Excel: 1 = atribuye violencia a música urbana, 0 = no atribuye
)

write.csv(muestra, "output/muestra_validacion_musica_urbana.csv", row.names = FALSE,
          fileEncoding = "UTF-8")




# c) Una palabra AMBIGUA en contexto (como "seguridad" en la Sesión 9)
#    >>> CAMBIAR AQUÍ <<< la palabra y los gobiernos que quieran comparar
palabra <- "verdad"
kwic(tokens_subset(toks, gobierno == "Aylwin"), palabra, window = 5) |> head(10)
kwic(tokens_subset(toks, gobierno == "Boric"),  palabra, window = 5) |> head(10)

# ¿Qué palabra viene justo después? Una tabla rápida de sus "apellidos":
k <- kwic(toks, palabra, window = 2)
sort(table(k$post), decreasing = TRUE)[1:15]   # las dos palabras que siguen

# d) Robustez: cambien UNA decisión y vuelvan a estimar. Elijan una:
#    - otra semilla:         set.seed(1)
#    - otros tópicos libres: residual = 10 o 20
#    - quitar la semilla más dudosa de su diccionario


# Se opta por quitar la semilla más dudosa ("verdad")

dic_rob <- dictionary(list(
  memoria    = c("derechos_humanos", "reconciliación", "víctimas",
                 "memoria", "detenidos_desaparecidos", "tortura*",
                 "reparaci*", "exilio", "desapar*"),
  transicion = dic$transicion
))


set.seed(2026)                                        # >>> CAMBIAR AQUÍ <<<
slda_rob <- textmodel_seededlda(dtm, dictionary = dic_rob, residual = 15)

terms(slda_rob, 10)[, marcos]
rob_gob <- aggregate(slda_rob$theta[, marcos, drop = FALSE], by = list(gobierno = gob), FUN = mean)
rob_gob[, marcos] <- round(rob_gob[, marcos] * 100, 1)
names(rob_gob)[-1] <- paste0(marcos, "_robustez")

cbind(prev_gob, rob_gob[, -1, drop = FALSE])   # original y robustez, lado a lado

# ¿Se mantiene el ORDEN de los gobiernos? Eso importa más que las cifras exactas.


# ---- PASO 8 · Coocurrencias: la red de una palabra clave -----------------
# Las palabras que aparecen a 5 o menos posiciones de su palabra clave, en dos
# gobiernos. El * incluye los compuestos (seguridad_pública, seguridad_ciudadana...).
clave <- "derechos_humanos"                  # >>> CAMBIAR AQUÍ <<<  (p. ej., "famili*", "derechos_humanos", "libertad")
gob_1 <- "Aylwin"                      # >>> CAMBIAR AQUÍ <<<
gob_2 <- "Boric"                       # >>> CAMBIAR AQUÍ <<<

red_clave <- function(g, n = 25) {
  cerca <- tokens_select(tokens_subset(toks_sin, gobierno == g), clave, window = 5)
  f <- fcm(cerca, context = "window", window = 5)
  fcm_select(f, pattern = names(topfeatures(dfm(cerca), n)))
}
# a) Los vecinos más frecuentes, en tabla
for (g in c(gob_1, gob_2)) {
  cerca <- tokens_remove(tokens_select(tokens_subset(toks_sin, gobierno == g),
                                       clave, window = 5), clave)
  cat("\n", g, "\n"); print(topfeatures(dfm(cerca), 12))
}
# b) Las dos redes (guárdenlas con Export > Save as Image...)
set.seed(2026); textplot_network(red_clave(gob_1), min_freq = 0.5,
                                 edge_color = "#2A5260", vertex_color = "#4E3239")
set.seed(2026); textplot_network(red_clave(gob_2), min_freq = 0.5,
                                 edge_color = "#2A5260", vertex_color = "#4E3239")
# Si la red sale vacía o con muy pocas palabras, la palabra clave es muy poco
# frecuente en ese gobierno: elijan otra o bajen min_freq a 0.2.


# ---- PASO 9 · Guardar y dejar constancia ---------------------------------
write.csv(prev_gob, "resultados_por_gobierno.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(prev_anio, "resultados_por_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")
sessionInfo()      # copien en el anexo del informe las versiones de R, quanteda y seededlda