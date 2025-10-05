# 1.R — Limpieza de acentos, resumen de columnas y PDF de gráficas

# Paquetes
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringi)
  library(ggplot2)
})

# -----------------------------
# Rutas (ajusta según tu entorno)
# -----------------------------
archivo_original   <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/2024_Ago_ENCal_1erNivel_CSV.csv"
archivo_sin_acentos <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/1_sin_acentos.csv"
resumen_csv        <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/resumen_columnas.csv"
pdf_graficas       <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/Graficas_exploracion.pdf"

# -----------------------------
# Función para quitar acentos
# -----------------------------
quitar_acentos <- function(x) {
  if (is.character(x)) {
    return(stri_trans_general(x, "Latin-ASCII"))
  }
  x
}

# -----------------------------
# Leer CSV original
# -----------------------------
df <- readr::read_csv(archivo_original, show_col_types = FALSE)

# -----------------------------
# Quitar acentos en nombres de columnas y valores de texto
# -----------------------------
names(df) <- vapply(names(df), quitar_acentos, character(1))
char_cols <- names(df)[vapply(df, is.character, logical(1))]
for (col in char_cols) {
  df[[col]] <- quitar_acentos(df[[col]])
}

# -----------------------------
# Guardar CSV limpio
# -----------------------------
readr::write_csv(df, archivo_sin_acentos)
cat("Archivo sin acentos guardado\n")

# -----------------------------
# Resumen de columnas
# -----------------------------
resumen_df <- tibble::tibble(
  Columna        = names(df),
  Tipo           = vapply(df, function(x) class(x)[1], character(1)),
  `Valores únicos` = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1)),
  Faltantes      = vapply(df, function(x) sum(is.na(x)), integer(1))
)
readr::write_csv(resumen_df, resumen_csv)
cat("Resumen de columnas terminado\n")

# -----------------------------
# PDF: gráficas por columna (valores únicos vs faltantes)
# -----------------------------
grDevices::pdf(pdf_graficas, width = 6, height = 4)
on.exit(grDevices::dev.off(), add = TRUE)

for (i in seq_along(df)) {
  colname <- names(df)[i]
  unique_count  <- dplyr::n_distinct(df[[i]], na.rm = TRUE)
  missing_count <- sum(is.na(df[[i]]))
  dat <- data.frame(Metrica = c("Valores únicos", "Valores faltantes"),
                    Cantidad = c(unique_count, missing_count))
  p <- ggplot(dat, aes(x = Metrica, y = Cantidad)) +
    geom_col() +
    ggtitle(sprintf("Columna %d: %s", i, colname)) +
    ylab("Cantidad") + xlab(NULL) +
    theme_minimal()
  print(p)
}
cat("Todas las gráficas se han guardado en un solo PDF\n")
