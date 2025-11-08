# ====================================================================
# TEST RÁPIDO - Verificar que las funciones de comparación funcionan
# ====================================================================

# Cargar librerías
library(dplyr)
library(tidyr)
library(psych)
library(rstatix)

# Crear datos de prueba
set.seed(123)
datos_test <- data.frame(
  grupo = rep(c("A", "B"), each = 30),
  variable1 = c(rnorm(30, mean = 20, sd = 5), rnorm(30, mean = 25, sd = 5)),
  variable2 = c(rnorm(30, mean = 50, sd = 10), rnorm(30, mean = 55, sd = 10)),
  grupo_multi = rep(c("G1", "G2", "G3"), each = 20)
)

# Copiar la función desde el archivo principal
source("D:/01 PS CRISTOPHER LINO CRUZ/16 GITHUB/Easy Tesis/Aplicativo Tesis pregrado.R", local = FALSE)

# ===== PRUEBA 1: T-test (2 grupos) =====
print("=== PRUEBA 1: T-TEST (2 grupos) ===")
resultado_ttest <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo",
  metodo = "ttest"
)
print(resultado_ttest)
cat("\n")

# ===== PRUEBA 2: Mann-Whitney (2 grupos) =====
print("=== PRUEBA 2: MANN-WHITNEY (2 grupos) ===")
resultado_mw <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo",
  metodo = "mannwhitney"
)
print(resultado_mw)
cat("\n")

# ===== PRUEBA 3: ANOVA (3 grupos) =====
print("=== PRUEBA 3: ANOVA (3 grupos) ===")
resultado_anova <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_multi",
  metodo = "anova"
)
print(resultado_anova)
cat("\n")

# ===== PRUEBA 4: Kruskal-Wallis (3 grupos) =====
print("=== PRUEBA 4: KRUSKAL-WALLIS (3 grupos) ===")
resultado_kw <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_multi",
  metodo = "kruskal"
)
print(resultado_kw)
cat("\n")

print("✓ Todas las pruebas completadas exitosamente")
