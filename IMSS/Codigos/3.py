import pandas as pd
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.metrics import confusion_matrix, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns

# 1. Cargar dataset
data_path = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\2_datos_numericos.csv"
df = pd.read_csv(data_path)

target = "FE_FinalNR"
X = df.drop(columns=[target])
y = df[target]

# 2. Codificación dummies para nominales
nominales = X.select_dtypes(include=["object"]).columns.tolist()
X = pd.get_dummies(X, columns=nominales, drop_first=True)

# 3. Split manual por clase
train_list = []
val_list = []
test_list = []

train_frac = 0.8
val_frac_within_train = 0.2

distribucion = []

for clase in sorted(y.unique()):
    idx = y[y == clase].index
    df_clase = X.loc[idx].copy()
    y_clase = y.loc[idx].copy()
    
    df_clase = df_clase.sample(frac=1, random_state=42)
    y_clase = y_clase.loc[df_clase.index]
    
    n_total = len(df_clase)
    n_train_full = int(n_total * train_frac)
    n_test = n_total - n_train_full
    n_val = int(n_train_full * val_frac_within_train)
    n_train = n_train_full - n_val
    
    train_list.append((df_clase.iloc[:n_train], y_clase.iloc[:n_train]))
    val_list.append((df_clase.iloc[n_train:n_train+n_val], y_clase.iloc[n_train:n_train+n_val]))
    test_list.append((df_clase.iloc[n_train+n_val:], y_clase.iloc[n_train+n_val:]))
    
    distribucion.append({
        "Clase": clase,
        "Train": n_train,
        "Validation": n_val,
        "Test": n_test
    })

X_train = pd.concat([t[0] for t in train_list])
y_train = pd.concat([t[1] for t in train_list])

X_val = pd.concat([v[0] for v in val_list])
y_val = pd.concat([v[1] for v in val_list])

X_test = pd.concat([t[0] for t in test_list])
y_test = pd.concat([t[1] for t in test_list])

# 4. Mostrar distribución por clase
df_distribucion = pd.DataFrame(distribucion)
print("Distribución por clase en cada conjunto:")
print(df_distribucion)

# 5. Entrenar árbol de decisión
clf = DecisionTreeClassifier(random_state=42, max_depth=5)
clf.fit(X_train, y_train)

# 6. Predicción y evaluación
y_pred = clf.predict(X_test)
cm = confusion_matrix(y_test, y_pred)
acc = accuracy_score(y_test, y_pred)

print("\nMatriz de confusión:\n", cm)
print(f"\nPrecisión: {acc:.3f}")

# 7. Importancia de variables
importancias = pd.DataFrame({
    "Variable": X_train.columns,
    "Importancia": clf.feature_importances_
}).sort_values(by="Importancia", ascending=False)

print("\nVariables más importantes:\n", importancias.head(10))

# 8. Visualización del árbol como imagen
labels = [str(c) for c in sorted(y.unique())]

plt.figure(figsize=(50,20))
plot_tree(clf, feature_names=X_train.columns, class_names=labels, filled=True, fontsize=12)
ruta_arbol = r"C:\Users\Zainos\Downloads\Practica 3\Corregido\arbol_decision.png"
plt.savefig(ruta_arbol, dpi=300, bbox_inches='tight')
plt.close()
print(f"Árbol de decisión guardado en: {ruta_arbol}")

# 9. Visualización exploratoria
plt.figure(figsize=(10,5))
sns.countplot(x=y_test)
plt.title("Distribución de FE_FinalNR en el conjunto de prueba")
plt.xlabel("FE_FinalNR")
plt.ylabel("Cantidad de observaciones")
plt.show()

if 'edad' in X_test.columns:
    plt.figure(figsize=(10,6))
    sns.boxplot(x=y_test, y=X_test['edad'])
    plt.title("Boxplot de edad por FE_FinalNR")
    plt.xlabel("FE_FinalNR")
    plt.ylabel("Edad")
    plt.show()

if 'entidad' in df.columns:
    plt.figure(figsize=(12,6))
    sns.countplot(x=df['entidad'], hue=df[target])
    plt.title("Distribución de FE_FinalNR por entidad")
    plt.xlabel("Entidad")
    plt.ylabel("Cantidad de observaciones")
    plt.xticks(rotation=90)
    plt.legend(title='FE_FinalNR')
    plt.show()

plt.figure(figsize=(15,10))
sns.heatmap(df.select_dtypes(include='number').corr(), cmap="coolwarm", annot=False, cbar=True)
plt.title("Mapa de correlación de variables numéricas")
plt.show()

plt.figure(figsize=(8, 6))
sns.boxplot(x="cal1", y="edad", data=df)
plt.title("Edad vs Nivel de satisfacción")
plt.show()

plt.figure(figsize=(8, 6))
sns.barplot(x="sexo", y="cal1", data=df, estimator="mean", ci=None)
plt.title("Promedio de satisfacción por sexo")
plt.show()

df["edad"].hist(bins=20)
plt.title("Distribución de la edad")
plt.xlabel("Edad")
plt.ylabel("Frecuencia")
plt.show()

