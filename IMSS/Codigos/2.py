import pandas as pd
import numpy as np
import unicodedata

# -----------------------------
# Función para quitar acentos
# -----------------------------
def quitar_acentos(texto):
    if isinstance(texto, str):
        return ''.join(c for c in unicodedata.normalize('NFKD', texto) if not unicodedata.combining(c))
    return texto

# -----------------------------
# Funciones de rangos
# -----------------------------
def rango_generico(columna, bins, labels):
    columna = pd.to_numeric(columna, errors="coerce").fillna(0)
    return pd.cut(columna, bins=bins, labels=labels, include_lowest=True).astype("object").fillna("NA")

def rango_diascita(columna):
    columna = pd.to_numeric(columna, errors="coerce").fillna(0)
    etiquetas = ["1 semana","1 mes","3 meses","6 meses","1 año","+1 año"]
    return pd.cut(columna, bins=[-np.inf,7,30,90,180,365,np.inf], labels=etiquetas, include_lowest=True).astype("object").fillna("NA")

# -----------------------------
# Rutas
# -----------------------------
archivo_original = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\1_sin_acentos.csv"
archivo_final = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\2_datos_numericos.csv"
archivo_mapeo = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\mapeo_categorico.csv"

# -----------------------------
# Leer CSV
# -----------------------------
df = pd.read_csv(archivo_original, encoding="utf-8")

# -----------------------------
# Quitar acentos
# -----------------------------
df.columns = [quitar_acentos(col) for col in df.columns]
for col in df.select_dtypes(include='object').columns:
    df[col] = df[col].apply(quitar_acentos)

# -----------------------------
# Reemplazar valores vacíos irrelevantes
# -----------------------------
df.replace(["", "NO SABE/NO RESPONDE", "NO SABE/ NO RESPONDE"], np.nan, inplace=True)

# -----------------------------
# Detectar y eliminar columnas duplicadas
# -----------------------------
columnas_iguales = []
for i, col1 in enumerate(df.columns):
    for j, col2 in enumerate(df.columns[i+1:], start=i+1):
        if df[col1].equals(df[col2]):
            columnas_iguales.append((col1, col2))
            if col2 in df.columns:
                df.drop(columns=[col2], inplace=True)

print(f"Se eliminaron {len(columnas_iguales)} columnas duplicadas.")

# -----------------------------
# Aplicar rangos a columnas específicas
# -----------------------------
columnas_rango = [
    "hr_ini_h", "hr_ini_m", "hr_fin_h", "hr_fin_m",
    "diascita", "antescita", "tmesphoci",
    "antusuario", "tiempollegar", 
    "promedio_diario_2024", "FE_FinalNR"
]

if "diascita" in df.columns:
    df["diascita"] = rango_diascita(df["diascita"])

if "antescita" in df.columns:
    df["antescita"] = rango_generico(
        df["antescita"],
        bins=[0,7,30,90,180,365,720,np.inf],
        labels=["1 semana","1 mes","3 meses","6 meses","1 año","2 años","+2 años"]
    )

if "tmesphoci" in df.columns:
    df["tmesphoci"] = rango_generico(
        df["tmesphoci"],
        bins=[0,10,20,30,60,120,365,np.inf],
        labels=["10","20","30","60","120","1 año","+1 año"]
    )

if "antusuario" in df.columns:
    df["antusuario"] = rango_generico(
        df["antusuario"],
        bins=[0,1,5,10,20,50,100,np.inf],
        labels=["1","5","10","20","50","100","+100"]
    )

if "tiempollegar" in df.columns:
    df["tiempollegar"] = rango_generico(
        df["tiempollegar"],
        bins=[0,5,10,20,30,60,120,np.inf],
        labels=["5","10","20","30","60","120","+120"]
    )

if "promedio_diario_2024" in df.columns:
    df["promedio_diario_2024"] = rango_generico(
        df["promedio_diario_2024"],
        bins=[0,100,500,1000,2000,5000,np.inf],
        labels=["100","500","1000","2000","5000","+5000"]
    )

if "FE_FinalNR" in df.columns:
    df["FE_FinalNR"] = rango_generico(
        df["FE_FinalNR"],
        bins=[0,40,80,120,160,200,np.inf],
        labels=["40","80","120","160","200","+240"]
    )

# -----------------------------
# Eliminar columnas irrelevantes o constantes
# -----------------------------
columnas_a_eliminar = ["fecha"]
for col in df.columns:
    if df[col].nunique() == 1 and col not in columnas_a_eliminar:
        columnas_a_eliminar.append(col)

df.drop(columns=columnas_a_eliminar, inplace=True)
print("Columnas eliminadas:", columnas_a_eliminar)

# -----------------------------
# Mapear todas las columnas a números, excepto la primera columna (folio)
# -----------------------------
mapeos = {}
columnas_a_mapear = df.columns[1:]  # omitir folio
for col in columnas_a_mapear:
    valores_unicos = df[col].fillna("NaN").unique()
    valores_unicos = sorted(valores_unicos, key=lambda x: str(x))
    mapeo = {valor: idx for idx, valor in enumerate(valores_unicos)}
    mapeos[col] = mapeo
    df[col] = df[col].map(mapeo).fillna(-1).astype(int)

# Guardar CSV de mapeo
filas_csv = []
for col, mapping in mapeos.items():
    for valor, numero in mapping.items():
        filas_csv.append({"Columna": col, "Valor": valor, "Codigo": numero})

pd.DataFrame(filas_csv).to_csv(archivo_mapeo, index=False)
print("Archivo de mapeo categórico guardado en:", archivo_mapeo)

# -----------------------------
# Guardar CSV final
# -----------------------------
df.to_csv(archivo_final, index=False, encoding="utf-8")
print("Archivo final con categorías convertidas a números guardado en:", archivo_final)
