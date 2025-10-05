# 2.R — Rango/bins, eliminación de duplicadas/constantes, mapeo categórico→numérico y guardados

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringi)
  library(tidyr)
  library(purrr)
})

# -----------------------------
# Rutas
# -----------------------------
archivo_original <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/1_sin_acentos.csv"
archivo_final    <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/2_datos_numericos.csv"
archivo_mapeo    <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/mapeo_categorico.csv"

# -----------------------------
# Funciones auxiliares
# -----------------------------
quitar_acentos <- function(x) if (is.character(x)) stri_trans_general(x, "Latin-ASCII") else x

rango_generico <- function(columna, bins, labels) {
  x <- suppressWarnings(as.numeric(columna))
  x[is.na(x)] <- 0
  cut(x, breaks = bins, labels = labels, include.lowest = TRUE, right = TRUE)
}

rango_diascita <- function(columna) {
  x <- suppressWarnings(as.numeric(columna))
  x[is.na(x)] <- 0
  etiquetas <- c("1 semana","1 mes","3 meses","6 meses","1 año","+1 año")
  cut(x, breaks = c(-Inf,7,30,90,180,365,Inf), labels = etiquetas, include_lowest = TRUE, right = TRUE)
}

# -----------------------------
# Leer y normalizar acentos
# -----------------------------
df <- readr::read_csv(archivo_original, show_col_types = FALSE)
names(df) <- vapply(names(df), quitar_acentos, character(1))
char_cols <- names(df)[vapply(df, is.character, logical(1))]
for (col in char_cols) df[[col]] <- quitar_acentos(df[[col]])

# -----------------------------
# Reemplazar vacíos irrelevantes por NA
# -----------------------------
to_na <- c("", "NO SABE/NO RESPONDE", "NO SABE/ NO RESPONDE")
for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]][df[[col]] %in% to_na] <- NA_character_
  }
}

# -----------------------------
# Eliminar columnas duplicadas (idéntico contenido)
# -----------------------------
dup_idx <- duplicated(as.list(df))
col_dup <- names(df)[dup_idx]
if (length(col_dup)) df <- df[, !names(df) %in% col_dup, drop = FALSE]
cat(sprintf("Se eliminaron %d columnas duplicadas.\n", length(col_dup)))

# -----------------------------
# Aplicar rangos a columnas específicas (si existen)
# -----------------------------
if ("diascita" %in% names(df)) {
  df$diascita <- rango_diascita(df$diascita)
}
if ("antescita" %in% names(df)) {
  df$antescita <- rango_generico(
    df$antescita,
    bins = c(0,7,30,90,180,365,720,Inf),
    labels = c("1 semana","1 mes","3 meses","6 meses","1 año","2 años","+2 años")
  )
}
if ("tmesphoci" %in% names(df)) {
  df$tmesphoci <- rango_generico(
    df$tmesphoci,
    bins = c(0,10,20,30,60,120,365,Inf),
    labels = c("10","20","30","60","120","1 año","+1 año")
  )
}
if ("antusuario" %in% names(df)) {
  df$antusuario <- rango_generico(
    df$antusuario,
    bins = c(0,1,5,10,20,50,100,Inf),
    labels = c("1","5","10","20","50","100","+100")
  )
}
if ("tiempollegar" %in% names(df)) {
  df$tiempollegar <- rango_generico(
    df$tiempollegar,
    bins = c(0,5,10,20,30,60,120,Inf),
    labels = c("5","10","20","30","60","120","+120")
  )
}
if ("promedio_diario_2024" %in% names(df)) {
  df$promedio_diario_2024 <- rango_generico(
    df$promedio_diario_2024,
    bins = c(0,100,500,1000,2000,5000,Inf),
    labels = c("100","500","1000","2000","5000","+5000")
  )
}
if ("FE_FinalNR" %in% names(df)) {
  df$FE_FinalNR <- rango_generico(
    df$FE_FinalNR,
    bins = c(0,40,80,120,160,200,Inf),
    labels = c("40","80","120","160","200","+240")
  )
}

# -----------------------------
# Eliminar columnas irrelevantes o constantes
# -----------------------------
columnas_a_eliminar <- c("fecha")
const_cols <- names(df)[vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE) == 1, logical(1))]
columnas_a_eliminar <- unique(c(columnas_a_eliminar, const_cols))
df <- df[, setdiff(names(df), columnas_a_eliminar), drop = FALSE]
cat("Columnas eliminadas:", paste(columnas_a_eliminar, collapse = ", "), "\n")

# -----------------------------
# Mapear todas las columnas a números, excepto la primera (folio)
# -----------------------------
mapeos <- list()
if (ncol(df) >= 2) {
  cols_a_mapear <- names(df)[-1]  # omite la primera columna (folio)
} else {
  cols_a_mapear <- character(0)
}

# función de mapeo 0..K-1 y NA→-1
mapear_col <- function(vec) {
  vals <- vec
  # Convertir a carácter para mapeo uniforme
  if (!is.character(vals) && !is.factor(vals)) vals <- as.character(vals)
  # Niveles ordenados como cadenas (para reproducir Python)
  unicos <- sort(unique(vals[!is.na(vals)]))
  codigos <- seq_along(unicos) - 1L  # 0-based
  mapa <- setNames(codigos, unicos)
  # Aplicar
  out <- ifelse(is.na(vals), -1L, ifelse(vals %in% unicos, mapa[vals], -1L))
  list(vector = as.integer(out), mapping = mapa)
}

mapeo_rows <- list()
for (col in cols_a_mapear) {
  res <- mapear_col(df[[col]])
  df[[col]] <- res$vector
  # construir filas para CSV de mapeo
  if (length(res$mapping)) {
    mapeo_rows[[col]] <- tibble::tibble(
      Columna = col,
      Valor   = names(res$mapping),
      Codigo  = as.integer(unname(res$mapping))
    )
  }
}

if (length(mapeo_rows)) {
  mapeo_df <- dplyr::bind_rows(mapeo_rows)
  readr::write_csv(mapeo_df, archivo_mapeo)
  cat("Archivo de mapeo categórico guardado en:", archivo_mapeo, "\n")
} else {
  cat("No hubo columnas a mapear.\n")
}

# -----------------------------
# Guardar CSV final
# -----------------------------
readr::write_csv(df, archivo_final)
cat("Archivo final con categorías convertidas a números guardado en:", archivo_final, "\n")


