# ================================================================================
# EASYTESIS - Script para ejecutar la aplicación Shiny
# ================================================================================
#
# Autor: Lino-Cruz, C.J. (2025)
#
# INSTRUCCIONES DE USO:
# 1. Abre este archivo en RStudio
# 2. Haz clic en "Run App" o ejecuta: shiny::runApp()
# 3. La aplicación se abrirá en tu navegador por defecto
#
# ================================================================================

# Establecer directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Cargar la aplicación
shiny::runApp("Aplicativo Tesis pregrado.R", launch.browser = TRUE)
