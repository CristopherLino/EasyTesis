# EasyTesis - Análisis Estadístico para Psicología

[![R-project](https://img.shields.io/badge/R-4.0+-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-0062ff?style=flat&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 📋 Descripción

**EasyTesis** es una aplicación interactiva para análisis estadístico de datos en investigaciones de psicología. Proporciona herramientas profesionales con salidas en formato APA 7ª edición.

## ✨ Características principales

- 📊 **Análisis Descriptivos** - Media, desviación estándar, rango, etc.
- 🔍 **Pruebas de Normalidad** - Shapiro-Wilk y Mardia multivariada
- 📈 **Correlaciones** - Pearson y Spearman con visualización
- 🔄 **Comparación de Grupos** - T-test, Mann-Whitney U, ANOVA de Welch, Kruskal-Wallis
- ⚙️ **Pruebas Post-hoc** - Games-Howell y Dunn's Test
- 🎯 **Confiabilidad** - Alfa de Cronbach y Omega de McDonald
- 📦 **Visualización** - Boxplots interactivos
- 💾 **Exportación** - Tablas Excel y gráficos JPG 2400x2400px @ 300dpi

## 🚀 Instalación

### Requisitos
- **R 4.0** o superior
- **RStudio** (recomendado)

### Pasos

1. **Clona el repositorio:**
   ```bash
   git clone https://github.com/CristopherLino/EasyTesis.git
   cd EasyTesis
   ```

2. **Instala las dependencias:**
   ```r
   source("install_dependencies.R")
   ```

3. **Ejecuta la aplicación:**
   ```r
   shiny::runApp()
   ```

## 📊 Uso

1. Carga un archivo Excel con tus datos
2. Selecciona las variables para análisis
3. (Opcional) Selecciona una variable groupadora para comparaciones
4. Ejecuta los análisis deseados
5. Descarga los resultados en Excel o imágenes JPG

### Formato de datos esperado

Archivo Excel con:
- Primera fila: Nombres de variables
- Filas siguientes: Datos

**Ejemplo:**
```
| Edad | Depresión | Ansiedad | Género |
|------|-----------|----------|--------|
| 22   | 15        | 12       | M      |
| 28   | 8         | 9        | F      |
```

## 🎯 Métodos estadísticos utilizados

- **Normalidad**: Shapiro-Wilk (univariada) y Mardia (multivariada)
- **Comparaciones paramétricas**: ANOVA de Welch (robusto ante varianzas desiguales)
- **Comparaciones no paramétricas**: Kruskal-Wallis H
- **Post-hoc paramétricas**: Games-Howell
- **Post-hoc no paramétricas**: Dunn's Test
- **Confiabilidad**: Alfa de Cronbach y Omega de McDonald

## 📝 Formato de salida

Todas las tablas se exportan en formato **APA 7ª edición**:
- Símbolos estadísticos (t, F, H, U, etc.)
- Grados de libertad entre paréntesis
- Efectos de tamaño como símbolos (d, ω², η², r_rb)
- Valores p con 4 decimales

## 👤 Autor

**Cristopher Lino-Cruz** - Psicólogo | Investigador
- GitHub: [@CristopherLino](https://github.com/CristopherLino)
- Email: cristopherlinoc@gmail.com

## 📄 Licencia

Este proyecto está bajo la licencia MIT. Ver [LICENSE](LICENSE) para más detalles.

## 📚 Referencias

- Documentación de [Shiny](https://shiny.rstudio.com/)
- Paquetes: psych, rstatix, semTools, MVN
- Estilo APA: [Publication Manual of the APA](https://apastyle.apa.org/)

---

**¿Necesitas ayuda?** Revisa [DEPLOYMENT.md](DEPLOYMENT.md) para publicar en Posit Connect Cloud.
