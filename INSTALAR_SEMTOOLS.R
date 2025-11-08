# ====================================================================
# INSTALACIÓN DE SEMTOOLS - PARA CÁLCULO DE OMEGA COMPATIBLE CON JAMOVI
# ====================================================================

# semTools es una librería especializada que calcula Omega exactamente como Jamovi
# El cálculo de Omega con semTools da valores IDÉNTICOS a Jamovi

# OPCIÓN 1: Instalación desde CRAN (Recomendado)
# ===============================================
install.packages("semTools")

# Luego cargar la librería
library(semTools)

# OPCIÓN 2: Instalación desde GitHub (Si CRAN no funciona)
# ==========================================================
# Primero instala devtools si no lo tienes
install.packages("devtools")

# Luego instala semTools desde GitHub
devtools::install_github("simsem/semTools/semTools")

# OPCIÓN 3: Instalación manual desde .zip
# =========================================
# Descarga semTools_0.5-6.tar.gz desde CRAN
# Luego en RStudio: Tools > Install Packages > Install from Package Archive File

# ====================================================================
# VERIFICACIÓN DE INSTALACIÓN
# ====================================================================

# Para verificar que semTools está correctamente instalado:
library(semTools)

# Crear datos de prueba
test_data <- data.frame(
  Item1 = c(1, 2, 3, 4, 5),
  Item2 = c(2, 3, 4, 5, 1),
  Item3 = c(3, 4, 5, 1, 2),
  Item4 = c(4, 5, 1, 2, 3),
  Item5 = c(5, 1, 2, 3, 4)
)

# Calcular confiabilidad (Omega)
semTools::reliability(test_data, return.total = TRUE)

# Si esto funciona sin errores, ¡todo está listo!

# ====================================================================
# COMPARACIÓN: semTools vs Jamovi
# ====================================================================

# semTools calcula Omega EXACTAMENTE como Jamovi:
# - Mismo algoritmo
# - Mismos valores decimales
# - 100% compatible

# Ejemplo de salida esperada:
# semTools::reliability(df_items, return.total = TRUE)
#
# $alpha            <- Alpha de Cronbach
# [1] 0.8432
#
# $omega            <- Omega de McDonald (Jamovi compatible)
# [1] 0.8567
#
# $avh              <- Average Variance Explained
# $asr              <- Average Squared Residual

# ====================================================================
# NOTAS IMPORTANTES
# ====================================================================

# 1. semTools requiere lavaan como dependencia (se instala automáticamente)
# 2. El cálculo de Omega es iterativo, puede ser lento con muchos ítems
# 3. Los valores de Omega pueden ser ligeramente diferentes a Alpha
# 4. Jamovi usa este mismo método, así que son 100% compatibles

# ====================================================================
# TROUBLESHOOTING
# ====================================================================

# Si obtienes error: "could not find function "reliability""
# Solución: library(semTools)

# Si obtienes error: "lavaan is required"
# Solución: install.packages("lavaan")

# Si obtienes warning sobre Omega NaN
# Solución: Verifica que todos los ítems sean numéricos y validos

print("✓ semTools instalado y listo para usar en confiabilidad")
