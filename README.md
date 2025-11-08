# EasyTesis - Análisis Estadístico para Psicología

[![R-project](https://img.shields.io/badge/R-4.0+-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-0062ff?style=flat&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 📋 Descripción

**EasyTesis** es una aplicación Shiny interactiva diseñada para facilitar el análisis estadístico de datos en investigaciones de psicología y ciencias del comportamiento. Proporciona herramientas profesionales para análisis descriptivos, pruebas de normalidad, correlaciones y comparaciones de grupos, con formato APA 7ª edición.

### Características principales:

✅ **Análisis Descriptivos** - Estadísticas descriptivas completas (media, desviación estándar, rango, etc.)

✅ **Pruebas de Normalidad** - Shapiro-Wilk univariada y Mardia multivariada

✅ **Análisis de Correlaciones** - Pearson y Spearman con matriz visual

✅ **Comparación de Grupos** - T-test, Mann-Whitney U, ANOVA de Welch, Kruskal-Wallis

✅ **Pruebas Post-hoc** - Games-Howell (ANOVA) y Dunn's Test (Kruskal-Wallis)

✅ **Análisis de Confiabilidad** - Alfa de Cronbach y Omega de McDonald

✅ **Visualización de Datos** - Boxplots interactivos para comparación de distribuciones

✅ **Exportación Profesional** - Tablas en formato Excel con estilo APA

✅ **Descargas de Alta Resolución** - Gráficos en JPG 2400x2400px @ 300dpi

## 🚀 Comenzar

### Requisitos previos

- **R 4.0** o superior
- **RStudio** (recomendado)

### Instalación local

1. **Clonar el repositorio:**
   ```bash
   git clone https://github.com/CristopherLino/EasyTesis.git
   cd EasyTesis
   ```

2. **Instalar dependencias:**
   ```r
   # En RStudio o R Console
   source("install_dependencies.R")
   ```

   O instalar manualmente:
   ```r
   packages <- c("shiny", "shinydashboard", "shinyjs", "DT", "readxl",
                 "openxlsx", "dplyr", "tidyr", "ggplot2", "psych", "rstatix",
                 "writexl", "corrplot", "semTools", "MVN")

   install.packages(packages)
   ```

3. **Ejecutar la aplicación:**
   ```r
   shiny::runApp()
   ```

   O en RStudio: Click en **"Run App"** (esquina superior derecha del editor)

## 📊 Uso

### Flujo básico:

1. **Cargar datos** - Importa un archivo Excel (.xlsx, .xls)
2. **Seleccionar variables** - Elige variables continuas para análisis
3. **Seleccionar variable groupadora** - Para comparaciones de grupos (opcional)
4. **Ejecutar análisis** - Selecciona el tipo de análisis que deseas
5. **Revisar resultados** - Visualiza tablas y gráficos
6. **Descargar** - Exporta resultados en Excel o imágenes JPG

### Tablas de Datos Esperadas:

La aplicación espera archivos Excel con:
- **Primera fila**: Nombres de variables
- **Datos**: Valores numéricos para variables continuas
- **Categoría**: Puede incluir variables categóricas para agrupación

**Ejemplo:**
| Edad | Depresión | Ansiedad | Género |
|------|-----------|----------|--------|
| 22   | 15        | 12       | M      |
| 28   | 8         | 9        | F      |
| 25   | 18        | 15       | M      |

## 🔧 Configuración

### Variables sociodemográficas vs. Ítems

La aplicación detecta automáticamente:
- **Variables continuas sociodemográficas** - Edad, ingresos, etc.
- **Ítems de escala** - Variables con patrones A1, P2, Q5, etc.

### Métodos estadísticos

- **Normales**: ANOVA de Welch (robusto ante varianzas desiguales)
- **No normales**: Kruskal-Wallis H
- **Post-hoc ANOVA**: Games-Howell (robusto)
- **Post-hoc Kruskal-Wallis**: Dunn's Test

## 📦 Publicar en Posit Connect Cloud

### Opción 1: Publicar desde RStudio (Recomendado)

1. Instala Posit Connect Agent:
   ```r
   install.packages("rsconnect")
   ```

2. Configura tu cuenta:
   ```r
   rsconnect::setAccountInfo(
     account = "tu_cuenta",
     token = "tu_token",
     secret = "tu_secret"
   )
   ```

3. Publica desde RStudio:
   ```
   Click en "Publish" → "Publish to Posit Connect"
   ```

### Opción 2: Publicar desde línea de comandos

```r
rsconnect::deployApp(
  appDir = getwd(),
  appName = "EasyTesis",
  account = "tu_cuenta",
  server = "posit.cloud"
)
```

### Opción 3: Usar GitHub

1. Conecta Posit Connect a tu repositorio GitHub
2. Selecciona la rama `main`
3. Configura el despliegue automático

## 📁 Estructura del Proyecto

```
EasyTesis/
├── app.R                          # Aplicación principal
├── install_dependencies.R         # Script para instalar paquetes
├── README.md                      # Este archivo
├── .gitignore                     # Archivos a ignorar en git
├── rsconnect/                     # Configuración de despliegue
│   └── deployment.json            # Metadata de Posit Connect
├── CHANGELOG_COMPARACIONES_v2.1.md # Historial de cambios
└── Referencias EasyTesis.bib      # Referencias bibliográficas
```

## 🔒 Requisitos de Privacidad y Seguridad

- Los datos se procesan completamente en la sesión local
- No se almacenan datos en el servidor de Posit Connect
- Se requiere autenticación para acceder a la aplicación publicada
- Asegúrate de configurar permisos apropiados en Posit Connect

## 🐛 Solución de Problemas

### Error: "Paquete no encontrado"
```r
install.packages("nombre_paquete")
```

### Error: "Mardia test - valor ausente donde TRUE/FALSE es necesario"
- Asegúrate de tener MVN actualizado: `install.packages("MVN")`
- Verifica que tengas al menos 3 variables continuas

### La app se ejecuta lentamente
- Reduce el tamaño de la base de datos (máximo recomendado: 10,000 filas)
- Cierra otras aplicaciones que consuman recursos

## 📚 Referencias

- [Documentación Shiny](https://shiny.rstudio.com/)
- [Publicar en Posit Connect](https://docs.posit.co/connect/user/publishing/)
- [APA Style 7ª edición](https://apastyle.apa.org/)
- [Análisis estadístico en R](https://statsandr.com/)

## 👤 Autor

**Cristopher Lino-Cruz** - Psicólogo | Investigador
- GitHub: [@CristopherLino](https://github.com/CristopherLino)
- Email: cristopherlinoc@gmail.com

## 📄 Licencia

Este proyecto está bajo la licencia MIT. Ver el archivo [LICENSE](LICENSE) para más detalles.

## 🙏 Agradecimientos

- Comunidad R y Shiny
- Paquetes: psych, rstatix, semTools, MVN
- Ayuda en análisis estadístico: [R for Data Science](https://r4ds.had.co.nz/)

---

**Última actualización**: 2025-11-08
**Versión**: 2.1
