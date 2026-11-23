

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





# ---- PASO 3 · Elegir la pregunta y su diccionario de semillas ----------
# Hay cuatro preguntas. Cada grupo elige UNA (ver la pauta). Cada diccionario es
# una hipótesis: "estas palabras indican este concepto". Pueden AGREGAR o QUITAR
# palabras (máximo 5 cambios por lista), pero deben justificarlo en el informe.


# C. Memoria y transición  (el pasado dictatorial y la política de los acuerdos)

#Se realizan cambios a diccionario de semillas original, se borra valech y rettig
# por tener frecuencia = 0, se agrega exilio y se cambia reparación, tortura y 
# desaparecidos por reparaci*, tortura* y desapar* respectivamente
# En transición se agrega democracia y concertación, se borra acuerdos por ser 
# muy general

dic_C <- dictionary(list(
  memoria    = c("derechos_humanos", "reconciliación", "verdad", "víctimas",
                 "memoria", "detenidos_desaparecidos", "tortura*",
                 "reparaci*", "exilio", "desapar*"),
  transicion = c("consenso*", "transición", "gobernabilidad",
                 "diálogo", "unidad", "convivencia", "paz", "entendimiento", 
                 "democracia", "concertación")))

dic <- dic_C         # >>> CAMBIAR AQUÍ <<<  dic_A, dic_B, dic_C o dic_D
marcos <- names(dic)  # los nombres de sus tópicos sembrados
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
# residual = 15: además de sus tópicos sembrados, 15 tópicos libres para todo
# lo demás (salud, vivienda, política exterior...). Con pocos tópicos libres,
# los sembrados se "tragan" palabras ajenas. Tarda alrededor de 1 minuto.
set.seed(2026)                                   # >>> puede cambiarla en el Paso 7d
slda <- textmodel_seededlda(dtm, dictionary = dic, residual = 15)
saveRDS(slda, "slda_taller3.rds")   # guárdelo: lo necesitarán para reproducir sus cifras
# Para volver a cargarlo otro día sin estimarlo de nuevo:
# slda <- readRDS("slda_taller3.rds")


# ---- PASO 5 · Interpretar: semillas y palabras descubiertas -------------
terms(slda, 15)[, marcos]     # las 15 palabras más probables de sus tópicos

# Las palabras que el modelo AGREGÓ (entre las 20 más probables, sin ser semillas)
top20 <- terms(slda, 20)
for (m in marcos) cat("\n", m, "->", paste(setdiff(top20[, m], semillas), collapse = ", "), "\n")
# Pregunta para el informe: ¿las palabras descubiertas confirman su concepto,
# lo amplían o muestran que el tópico mide OTRA cosa?


# ---- PASO 6 · Medir: ¿cuánto pesa cada tópico, en quién y cuándo? -------
theta <- slda$theta
gob   <- factor(docvars(dtm, "gobierno"), levels = orden_gob)
anio  <- docvars(dtm, "anio")

# a) Peso promedio (theta, en %) por gobierno
prev_gob <- aggregate(theta[, marcos, drop = FALSE], by = list(gobierno = gob), FUN = mean)
prev_gob[, marcos] <- round(prev_gob[, marcos] * 100, 1)
prev_gob

# b) % de párrafos cuyo tópico PRINCIPAL es cada uno de sus tópicos
principal <- topics(slda)
round(prop.table(table(gob, principal), 1)[, marcos] * 100, 1)

# c) Gráfico por año (una línea por tópico sembrado)
prev_anio <- aggregate(theta[, marcos, drop = FALSE], by = list(anio = anio), FUN = mean)
colores <- c("#4E3239", "#2A5260", "#9FB4BD")[seq_along(marcos)]
matplot(prev_anio$anio, prev_anio[, marcos] * 100, type = "b", pch = 19, lty = 1,
        col = colores, xlab = "Año", ylab = "Peso promedio del tópico (%)")
legend("topleft", marcos, col = colores, lty = 1, pch = 19, bty = "n")
abline(v = c(1993.5, 1999.5, 2005.5, 2009.5, 2013.5, 2017.5, 2021.5, 2025.5),
       lty = 3, col = "grey60")             # cambios de gobierno
# Para guardarlo: en la pestaña Plots, Export > Save as Image...

# d) Solo las PRIMERAS cuentas de cada mandato (la comparación más justa con Kast,
#    que tiene una sola).
primeras <- docvars(dtm, "primera_cuenta") == "sí"
prev_primeras <- aggregate(theta[primeras, marcos, drop = FALSE],
                           by = list(gobierno = gob[primeras]), FUN = mean)
prev_primeras[, marcos] <- round(prev_primeras[, marcos] * 100, 1)
prev_primeras

# e) Contraste teoría-first: el diccionario solo (palabras por cada 1.000)
cuenta_dic <- dfm_group(dfm_lookup(dtm, dic), groups = gobierno)
total_gob  <- ntoken(dfm_group(dtm, groups = gobierno))
round(as.matrix(cuenta_dic) / total_gob * 1000, 1)[orden_gob, ]


# ---- PASO 7 · Validar ------------------------------------------------------
m <- marcos[1]   # >>> CAMBIAR AQUÍ <<<  el tópico que van a validar (marcos[1] o marcos[2]...)

# a) Los 5 párrafos más típicos del tópico (theta más alto): ¿tratan de lo que dice su nombre?
for (i in order(theta[, m], decreasing = TRUE)[1:5]) {
  cat("\n#", names(texto)[i], " theta =", round(theta[i, m], 2), "\n", texto[i], "\n")
}

# b) Muestra al azar para CODIFICAR A MANO: 20 párrafos cuyo tópico principal es m.
#    Se guarda en un archivo .csv que pueden abrir en Excel. En la columna
#    "codigo" escriban 1 (sí trata del concepto), 0 (no) o 9 (duda).
set.seed(3)                                       # no la cambien: así su muestra es reproducible
candidatos <- which(principal == m)
elegidos <- sample(candidatos, min(20, length(candidatos)))
muestra <- data.frame(parrafo = names(texto)[elegidos],
                      gobierno = gob[elegidos],
                      theta = round(theta[elegidos, m], 2),
                      texto = texto[elegidos],
                      codigo = "")
write.csv(muestra, paste0("muestra_validacion_", m, ".csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
# Después de codificar en Excel, guárdenlo como .csv y léanlo de vuelta:
cod <- read.csv(paste0("muestra_validacion_", m, ".csv"), encoding = "UTF-8")
mean(cod$codigo == 1)                         # precisión: proporción de "sí"
tapply(cod$codigo == 1, cod$theta >= 0.4, mean) # ¿mejora con theta alto?


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