# Módulo IA y Programación — Material en Quarto

Material del segundo módulo, escrito en Quarto (`.qmd`) con código en **R y
Python**, estructurado como: Motivación → Revisión teórica → Ejercicios
resueltos → Ejercicios propuestos.

## Estructura del repositorio

```
modulo-curso/
├── _quarto.yml                      # configuración global (tema, TOC, formato)
├── styles.css                       # estilos
├── _plantilla_tema.qmd              # plantilla en blanco
├── 01_introduccion_programacion_.qmd   # Introducción a la programación en R y Python
├── 02_limpieza-visualizacion-datos.qmd # Limpieza, tratamiento y visualización de datos
├── 03_agentes_IA.qmd                   # Agentes de IA
├── 04_aplicaciones.qmd                 # Aplicaciones en banca y seguros
└── .gitignore
```

## 1. Requisitos previos (una sola vez)

En Positron necesitas ambos lenguajes activos en el mismo proyecto:

1. **R**: instala el paquete `reticulate`, que es el puente que permite usar
   chunks `{r}` y `{python}` en un mismo documento.
   ```r
   install.packages("reticulate")
   ```
2. **Python**: ten un entorno (venv o conda) con las librerías que uses
   (`pandas`, `matplotlib`, `numpy`, etc.). En Positron, selecciona ese
   intérprete de Python desde el selector de intérpretes (arriba a la
   derecha).
3. Si `reticulate` no detecta automáticamente tu entorno, fija la ruta en el
   chunk `setup` de cada `.qmd`:
   ```r
   library(reticulate)
   use_virtualenv("ruta/o/nombre/del/entorno", required = TRUE)
   ```
4. Instala **Quarto CLI** (si no viene ya con Positron):
   https://quarto.org/docs/get-started/

## 2. Versionamiento con GitHub

```bash
git init
git add .
git commit -m "Estructura inicial del módulo"
git branch -M main
git remote add origin https://github.com/<usuario>/<repo>.git
git push -u origin main
```

Flujo por tema (recomendado):
```bash
git checkout -b tema-02-probabilidad
# ... trabajas y renderizas ...
git add 02_probabilidad.qmd
git commit -m "Agrega tema 2: probabilidad"
git push -u origin tema-02-probabilidad
# luego Pull Request -> main
```

El `.gitignore` ya excluye los `.html` generados y las carpetas de caché
(`.quarto/`, `_freeze/`, `*_files/`), para no versionar archivos pesados o
regenerables. Si prefieres versionar también los `.html` finales, quita esa línea
del `.gitignore`.