# ====================================================================
# TEST - Verificar que las comparaciones muestren todos los campos
# ====================================================================

# Cargar librerías
library(dplyr)
library(tidyr)
library(psych)
library(rstatix)

# Crear datos de prueba
set.seed(123)
datos_test <- data.frame(
  grupo_2 = rep(c("A", "B"), each = 30),
  grupo_3 = rep(c("G1", "G2", "G3"), each = 20),
  variable1 = c(rnorm(30, mean = 20, sd = 5), rnorm(30, mean = 25, sd = 5)),
  variable2 = c(rnorm(30, mean = 50, sd = 10), rnorm(30, mean = 55, sd = 10))
)

# Cargar la función comparar_grupos desde el archivo principal
source("D:/01 PS CRISTOPHER LINO CRUZ/16 GITHUB/Easy Tesis/Aplicativo Tesis pregrado.R", local = FALSE)

cat("\n")
cat("============================================\n")
cat("TEST 1: T-TEST (2 grupos)\n")
cat("============================================\n")
resultado_ttest <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_2",
  metodo = "ttest"
)
print(resultado_ttest)
cat("\nColumnas presentes en T-test:\n")
print(names(resultado_ttest))

cat("\n")
cat("============================================\n")
cat("TEST 2: MANN-WHITNEY U (2 grupos)\n")
cat("============================================\n")
resultado_mw <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_2",
  metodo = "mannwhitney"
)
print(resultado_mw)
cat("\nColumnas presentes en Mann-Whitney:\n")
print(names(resultado_mw))

cat("\n")
cat("============================================\n")
cat("TEST 3: ANOVA (3 grupos)\n")
cat("============================================\n")
resultado_anova <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_3",
  metodo = "anova"
)
print(resultado_anova)
cat("\nColumnas presentes en ANOVA:\n")
print(names(resultado_anova))

cat("\n")
cat("============================================\n")
cat("TEST 4: KRUSKAL-WALLIS (3 grupos)\n")
cat("============================================\n")
resultado_kw <- comparar_grupos(
  data = datos_test,
  variables = c("variable1", "variable2"),
  var_grupo = "grupo_3",
  metodo = "kruskal"
)
print(resultado_kw)
cat("\nColumnas presentes en Kruskal-Wallis:\n")
print(names(resultado_kw))

cat("\n")
cat("✓ Test completado - Verifica que todos los campos aparezcan\n")
