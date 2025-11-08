# 🤝 Guía de Contribución - EasyTesis

¡Gracias por tu interés en contribuir a EasyTesis! Este documento te guiará sobre cómo reportar problemas, sugerir mejoras y contribuir código.

## 📋 Código de Conducta

Por favor, sé respetuoso y constructivo en todas las interacciones. Cualquier comportamiento abusivo será reportado.

---

## 🐛 Reportar Bugs

Si encuentras un bug:

1. **Revisa los issues existentes** - Busca si ya fue reportado
2. **Crea un nuevo issue** con:
   - Título descriptivo
   - Descripción detallada del problema
   - Pasos para reproducir
   - Resultado esperado vs. actual
   - Tu versión de R y paquetes

### Ejemplo de reporte:

```
Título: Error en Mardia test con datos faltantes

Descripción:
Cuando tengo columnas con NA en más del 30% de los datos,
la prueba Mardia falla con el error "valor ausente donde TRUE/FALSE".

Pasos para reproducir:
1. Cargar datos con columnas incompletas
2. Seleccionar 3+ variables continuas
3. Ejecutar "Normalidad Multivariada"

Versión R: 4.3.0
Paquetes: MVN 1.8.0, dplyr 1.1.2

Error adjunto: [imagen o código]
```

---

## 💡 Sugerir Mejoras

Para sugerir nuevas características:

1. **Abre un issue** con etiqueta `enhancement`
2. Incluye:
   - Descripción clara de la mejora
   - Por qué sería útil
   - Ejemplos de uso
   - Referencias (papers, otros software)

### Ejemplo:

```
Título: Agregar análisis de poder estadístico

Descripción:
Sería útil incluir análisis de poder post-hoc para evaluar
si el tamaño de muestra fue suficiente.

Utilidad:
- Researchers suelen necesitar esto para reportes
- Cumple con estándares de reportería APA

Paquetes sugeridos: pwr, powerAnalysis
```

---

## 💻 Contribuir Código

### Requisitos previos

1. Fork el repositorio en GitHub
2. Clona tu fork localmente:
   ```bash
   git clone https://github.com/TU_USUARIO/EasyTesis.git
   cd EasyTesis
   git checkout -b feature/tu-nueva-feature
   ```

### Flujo de trabajo

1. **Crea una rama** con nombre descriptivo:
   ```bash
   git checkout -b feature/nueva-prueba-estadistica
   git checkout -b fix/error-descarga-excel
   ```

2. **Haz cambios** siguiendo el estilo de código:
   - Documenta funciones nuevas con comentarios claros
   - Usa nombres descriptivos para variables
   - Mantén líneas bajo 100 caracteres
   - Sigue el formato R existente

3. **Prueba localmente**:
   ```r
   shiny::runApp()
   ```

4. **Commit con mensajes claros**:
   ```bash
   git commit -m "Fix: Corregir error en Mardia test con datos NA"
   git commit -m "Feature: Agregar análisis de poder estadístico"
   git commit -m "Docs: Actualizar README con instrucciones"
   ```

5. **Push a tu fork**:
   ```bash
   git push origin feature/tu-nueva-feature
   ```

6. **Abre un Pull Request** en GitHub:
   - Describe qué cambiaste
   - Referencia los issues relacionados (#123)
   - Incluye screenshots si hay cambios UI

---

## 🎨 Estilo de Código R

### Ejemplo de buen estilo:

```r
# ✓ CORRECTO
calcular_estadisticos <- function(datos, variable, grupos) {
  # Validar entrada
  if (!is.data.frame(datos)) {
    stop("datos debe ser un data.frame")
  }

  # Preparar datos
  df_clean <- datos %>%
    filter(!is.na(.data[[variable]])) %>%
    select(all_of(c(grupos, variable)))

  # Calcular
  resultado <- df_clean %>%
    group_by(.data[[grupos]]) %>%
    summarise(
      Media = mean(.data[[variable]]),
      DE = sd(.data[[variable]]),
      N = n(),
      .groups = "drop"
    )

  return(resultado)
}

# ✗ EVITAR
calcularEstadisticos<-function(d,v,g){
  r<-d[!is.na(d[[v]]),]
  m<-tapply(r[[v]],r[[g]],mean)
  de<-tapply(r[[v]],r[[g]],sd)
  n<-tapply(r[[v]],r[[g]],length)
  return(list(m=m,de=de,n=n))
}
```

### Guía rápida:

- **Nombres de funciones**: `palabra_palabra()` (snake_case)
- **Nombres de variables**: `mi_variable` (snake_case)
- **Indentación**: 2 espacios
- **Comentarios**: `#` para líneas, `# ---` para secciones
- **Documentación**: Comenta el propósito y parámetros

---

## 🧪 Testing

### Prueba tu código localmente:

```r
# 1. Prueba funciones individuales
source("app.R")

# 2. Prueba con datos de ejemplo
datos_test <- data.frame(
  Edad = c(22, 28, 25, 30, 23),
  Depresion = c(15, 8, 18, 12, 14),
  Genero = c("M", "F", "M", "F", "M")
)

resultado <- comparar_grupos(
  datos_test,
  c("Depresion", "Edad"),
  "Genero",
  "anova"
)

# 3. Ejecuta la app completa
shiny::runApp()
```

---

## 📦 Estructura de cambios

### Para bug fixes:
```
- Descripción concisa del error
- Archivo(s) afectados
- Líneas de código cambiadas
- Testing realizado
```

### Para nuevas features:
```
- Descripción de la feature
- Funcionalidad agregada
- Archivos nuevos/modificados
- Testing realizado
- Documentación actualizada
```

---

## 🔄 Proceso de Review

Después de abrir un PR:

1. Verifica que todos los tests pasen
2. Revisa que el código siga el estilo
3. Espera retroalimentación del mantenedor
4. Haz cambios si se piden
5. ¡Celebra cuando sea merged! 🎉

---

## 📚 Documentación

Si agregas una feature, por favor:

1. **Actualiza README.md** si es una feature mayor
2. **Agrega comentarios en el código** explicando la lógica
3. **Documenta en DEPLOYMENT.md** si afecta el despliegue
4. **Incluye ejemplos de uso**

---

## 🚀 Sugerencias de contribución

Si no sabes por dónde empezar, busca issues etiquetados:
- `good first issue` - Para principiantes
- `help wanted` - Donde se busca contribución
- `documentation` - Mejoras de docs

---

## ❓ Preguntas?

- Abre un **Discussion** en GitHub
- Contacta a [Cristopher Lino](mailto:cristopherlinoc@gmail.com)
- Revisa los issues existentes

---

## 📄 Licencia

Al contribuir, aceptas que tu código estará bajo la licencia MIT.

---

¡Gracias por contribuir! 🙌

**Última actualización**: 2025-11-08
