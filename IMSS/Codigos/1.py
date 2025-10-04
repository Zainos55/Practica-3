import pandas as pd
import unicodedata
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import os

# -----------------------------
# Función para quitar acentos
# -----------------------------
def quitar_acentos(texto):
    if isinstance(texto, str):
        return ''.join(c for c in unicodedata.normalize('NFKD', texto) if not unicodedata.combining(c))
    return texto

# -----------------------------
# Rutas de archivos y carpetas
# -----------------------------
archivo_original = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\2024_Ago_ENCal_1erNivel_CSV.csv"
archivo_sin_acentos = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\1_sin_acentos.csv"
pdf_graficas = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\Graficas_exploracion.pdf"

# -----------------------------
# Leer CSV original
# -----------------------------
df = pd.read_csv(archivo_original, encoding='utf-8')

# -----------------------------
# Quitar acentos en los nombres de columnas
# -----------------------------
df.columns = [quitar_acentos(col) for col in df.columns]

# -----------------------------
# Quitar acentos en los valores de texto
# -----------------------------
for col in df.select_dtypes(include='object').columns:
    df[col] = df[col].apply(quitar_acentos)

# -----------------------------
# Guardar CSV limpio
# -----------------------------
df.to_csv(archivo_sin_acentos, index=False, encoding='utf-8')
print(f"Archivo sin acentos guardado")

# -----------------------------
# Crear resumen de columnas
# -----------------------------
resumen = []
for col in df.columns:
    resumen.append({
        "Columna": col,
        "Tipo": df[col].dtype,
        "Valores únicos": df[col].nunique(dropna=True),
        "Faltantes": df[col].isna().sum()
    })

resumen_df = pd.DataFrame(resumen)
resumen_csv = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\resumen_columnas.csv"
resumen_df.to_csv(resumen_csv, index=False, encoding='utf-8')
print(f"Resumen de columnas terminado")

# -----------------------------
# Generar todas las gráficas en un solo PDF
# -----------------------------
with PdfPages(pdf_graficas) as pdf:
    for idx, col in enumerate(df.columns, start=1):
        unique_count = df[col].nunique(dropna=True)
        missing_count = df[col].isna().sum()
        
        plt.figure(figsize=(6,4))
        plt.bar(["Valores únicos", "Valores faltantes"], [unique_count, missing_count], color=['skyblue','salmon'])
        plt.title(f"Columna {idx}: {col}")
        plt.ylabel("Cantidad")
        plt.tight_layout()
        
        pdf.savefig()  # Guarda la figura en el PDF
        plt.close()    # Cierra la figura para no mostrarla ni acumular memoria

print(f"Todas las gráficas se han guardado en un solo PDF")
