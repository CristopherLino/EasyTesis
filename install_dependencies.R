# ================================================================================
# SCRIPT DE INSTALACIÓN DE DEPENDENCIAS
# EasyTesis - Análisis Estadístico para Psicología
# ================================================================================
#
# Este script instala todos los paquetes necesarios para ejecutar la aplicación
# EasyTesis de manera segura y eficiente.
#
# Uso: source("install_dependencies.R")
#
# ================================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    INSTALADOR DE DEPENDENCIAS - EasyTesis                  ║\n")
cat("║                   Análisis Estadístico para Psicología                      ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Lista de paquetes necesarios
required_packages <- c(
  "shiny",           # Framework para aplicaciones web interactivas
  "shinydashboard",  # Interfaz dashboard profesional
  "shinyjs",         # JavaScript en Shiny
  "DT",              # Tablas interactivas
  "readxl",          # Leer archivos Excel
  "openxlsx",        # Escribir archivos Excel formateados
  "dplyr",           # Manipulación de datos
  "tidyr",           # Limpieza de datos
  "ggplot2",         # Visualización de gráficos
  "psych",           # Análisis psicométrico
  "rstatix",         # Estadística robusta
  "writexl",         # Escribir Excel simple
  "corrplot",        # Matriz de correlaciones
  "semTools",        # Análisis de factor confirmatorio y fiabilidad
  "MVN",             # Pruebas de normalidad multivariada
  "FSA"              # Paquete para análisis de datos de peces (contiene dunnTest)
)

# Paquetes opcionales para mejor desarrollo
optional_packages <- c(
  "tidyverse",       # Conjunto completo de herramientas de datos
  "car",             # Diagnósticos de regresión
  "lmtest"           # Pruebas estadísticas
)

cat("📦 Verificando paquetes necesarios...\n")
cat("─────────────────────────────────────────────────────────────────────────────\n\n")

# Función para instalar un paquete si no está disponible
install_if_needed <- function(package, required = TRUE) {
  type <- if (required) "REQUERIDO" else "OPCIONAL"

  if (!requireNamespace(package, quietly = TRUE)) {
    cat(sprintf("[%s] Instalando %s... ", type, package))

    tryCatch({
      install.packages(package, dependencies = TRUE, quiet = TRUE)

      if (requireNamespace(package, quietly = TRUE)) {
        cat("✓ Instalado\n")
        return(TRUE)
      } else {
        cat("✗ Error en instalación\n")
        return(FALSE)
      }
    }, error = function(e) {
      cat(sprintf("✗ Error: %s\n", e$message))
      return(FALSE)
    })
  } else {
    cat(sprintf("[%s] %s ya está instalado ✓\n", type, package))
    return(TRUE)
  }
}

# Instalar paquetes requeridos
cat("\n📥 PAQUETES REQUERIDOS:\n")
cat("─────────────────────────────────────────────────────────────────────────────\n")

required_status <- sapply(required_packages, install_if_needed, required = TRUE)

# Instalar paquetes opcionales
cat("\n📥 PAQUETES OPCIONALES (para mejor experiencia):\n")
cat("─────────────────────────────────────────────────────────────────────────────\n")

optional_status <- sapply(optional_packages, install_if_needed, required = FALSE)

# Resumen
cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                         RESUMEN DE INSTALACIÓN                             ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n\n")

required_installed <- sum(required_status)
required_total <- length(required_packages)
optional_installed <- sum(optional_status)
optional_total <- length(optional_packages)

cat(sprintf("✓ Paquetes requeridos: %d/%d instalados\n", required_installed, required_total))
cat(sprintf("✓ Paquetes opcionales: %d/%d instalados\n\n", optional_installed, optional_total))

if (required_installed == required_total) {
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("✓ ¡ÉXITO! Todos los paquetes requeridos están instalados.\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  cat("🚀 Puedes ejecutar EasyTesis con:\n")
  cat("   shiny::runApp()\n\n")
} else {
  cat("⚠️  ADVERTENCIA: Algunos paquetes requeridos no se pudieron instalar.\n")
  cat("Por favor, intenta instalarlos manualmente:\n\n")

  failed_packages <- names(required_status)[!required_status]
  for (pkg in failed_packages) {
    cat(sprintf("   install.packages('%s')\n", pkg))
  }
  cat("\n")
}

cat("Para más información, visita: https://github.com/CristopherLino/EasyTesis\n\n")
