# 3.R — Árbol de decisión (estratificado por clase), métricas, importancias y visualizaciones

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(rpart)
  library(rpart.plot)
  library(reshape2)
})

# -----------------------------
# Rutas
# -----------------------------
data_path  <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/2_datos_numericos.csv"
ruta_arbol <- "C:/Users/Zainos/Downloads/Practica 3/Corregido/arbol_decision.png"

# -----------------------------
# Cargar dataset
# -----------------------------
df <- readr::read_csv(data_path, show_col_types = FALSE)
target <- "FE_FinalNR"

stopifnot(target %in% names(df))
X <- df %>% dplyr::select(-all_of(target))
y <- df[[target]]

# -----------------------------
# Dummies para nominales (si hubiera columnas no numéricas)
# -----------------------------
# Si todo ya está numérico (por el script 2), esto no cambia nada.
X_model <- if (any(!vapply(X, is.numeric, logical(1)))) {
  mm <- model.matrix(~ . - 1, data = mutate(X, across(!where(is.numeric), as.factor)))
  as.data.frame(mm)
} else {
  as.data.frame(X)
}

# -----------------------------
# Split estratificado por clase (80% train, de eso 20% val, resto test)
# -----------------------------
set.seed(42)
clases <- sort(unique(y))
train_list <- list(); val_list <- list(); test_list <- list()
train_frac <- 0.8
val_frac_within_train <- 0.2

distribucion <- list()

for (cl in clases) {
  idx <- which(y == cl)
  idx <- sample(idx, length(idx))  # shuffle
  
  n_total <- length(idx)
  n_train_full <- floor(n_total * train_frac)
  n_test <- n_total - n_train_full
  n_val <- floor(n_train_full * val_frac_within_train)
  n_train <- n_train_full - n_val
  
  tr_idx  <- idx[seq_len(n_train)]
  va_idx  <- idx[seq(n_train + 1, n_train + n_val)]
  te_idx  <- idx[(n_train + n_val + 1):n_total]
  
  train_list[[as.character(cl)]] <- list(X = X_model[tr_idx, , drop = FALSE], y = y[tr_idx])
  val_list[[as.character(cl)]]   <- list(X = X_model[va_idx, , drop = FALSE], y = y[va_idx])
  test_list[[as.character(cl)]]  <- list(X = X_model[te_idx, , drop = FALSE], y = y[te_idx])
  
  distribucion[[as.character(cl)]] <- data.frame(
    Clase = cl, Train = n_train, Validation = n_val, Test = n_test
  )
}

X_train <- do.call(rbind, lapply(train_list, `[[`, "X"))
y_train <- unlist(lapply(train_list, `[[`, "y"))

X_val   <- do.call(rbind, lapply(val_list, `[[`, "X"))
y_val   <- unlist(lapply(val_list, `[[`, "y"))

X_test  <- do.call(rbind, lapply(test_list, `[[`, "X"))
y_test  <- unlist(lapply(test_list, `[[`, "y"))

df_distribucion <- dplyr::bind_rows(distribucion)
cat("Distribución por clase en cada conjunto:\n"); print(df_distribucion)

# -----------------------------
# Entrenar árbol de decisión
# -----------------------------
train_df <- cbind(X_train, .y = as.factor(y_train))
fit <- rpart(.y ~ ., data = train_df,
             method = "class",
             control = rpart.control(maxdepth = 5))

# -----------------------------
# Predicción y evaluación
# -----------------------------
pred <- predict(fit, newdata = X_test, type = "class")
cm <- table(Real = y_test, Pred = pred)
acc <- sum(diag(cm)) / sum(cm)
cat("\nMatriz de confusión:\n"); print(cm)
cat(sprintf("\nPrecisión: %.3f\n", acc))

# -----------------------------
# Importancia de variables
# -----------------------------
if (!is.null(fit$variable.importance)) {
  importancias <- data.frame(
    Variable = names(fit$variable.importance),
    Importancia = as.numeric(fit$variable.importance),
    row.names = NULL
  ) %>% arrange(desc(Importancia))
  cat("\nVariables más importantes (top 10):\n")
  print(head(importancias, 10))
} else {
  cat("\nEl modelo no reporta importancias.\n")
}

# -----------------------------
# Visualización del árbol como imagen
# -----------------------------
png(filename = ruta_arbol, width = 2000, height = 1200, res = 150)
rpart.plot::rpart.plot(fit, type = 2, extra = 106, under = TRUE, faclen = 0)
dev.off()
cat(sprintf("Árbol de decisión guardado en: %s\n", ruta_arbol))

# -----------------------------
# Visualizaciones exploratorias (opcionales, si existen columnas)
# -----------------------------
# Distribución de FE_FinalNR en test
ggplot(data.frame(FE_FinalNR = y_test), aes(x = FE_FinalNR)) +
  geom_bar() + theme_minimal() +
  ggtitle("Distribución de FE_FinalNR en el conjunto de prueba") +
  xlab("FE_FinalNR") + ylab("Cantidad")

# Boxplot de edad por FE_FinalNR (si existe 'edad' en X_test o df)
if ("edad" %in% names(df)) {
  ggplot(df, aes(x = .data[[target]], y = .data[["edad"]])) +
    geom_boxplot() + theme_minimal() +
    ggtitle("Edad vs Nivel de satisfacción") +
    xlab(target) + ylab("Edad")
}

# Distribución por entidad (si existe 'entidad')
if ("entidad" %in% names(df)) {
  ggplot(df, aes(x = entidad, fill = .data[[target]])) +
    geom_bar(position = "stack") + theme_minimal() +
    ggtitle("Distribución de FE_FinalNR por entidad") +
    xlab("Entidad") + ylab("Cantidad de observaciones") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Mapa de correlación de variables numéricas
num_df <- dplyr::select(df, where(is.numeric))
if (ncol(num_df) >= 2) {
  corr <- stats::cor(num_df, use = "pairwise.complete.obs")
  molten <- reshape2::melt(corr)
  ggplot(molten, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1, 1)) +
    theme_minimal() +
    ggtitle("Mapa de correlación de variables numéricas") +
    xlab(NULL) + ylab(NULL)
}

# Ejemplo: boxplot edad vs cal1 (si existen)
if (all(c("edad", "cal1") %in% names(df))) {
  ggplot(df, aes(x = as.factor(cal1), y = edad)) +
    geom_boxplot() + theme_minimal() +
    ggtitle("Edad vs Nivel de satisfacción") +
    xlab("cal1") + ylab("Edad")
}

# Promedio de satisfacción por sexo (si existen)
if (all(c("sexo", "cal1") %in% names(df))) {
  ggplot(df, aes(x = as.factor(sexo), y = cal1)) +
    stat_summary(fun = mean, geom = "bar") +
    theme_minimal() +
    ggtitle("Promedio de satisfacción por sexo") +
    xlab("sexo") + ylab("Promedio cal1")
}

# Histograma de edad (si existe)
if ("edad" %in% names(df)) {
  ggplot(df, aes(x = edad)) +
    geom_histogram(bins = 20) +
    theme_minimal() +
    ggtitle("Distribución de la edad") +
    xlab("Edad") + ylab("Frecuencia")
}
