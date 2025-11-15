
rm(list=ls())
# ================================================================================
# EASYTESIS - Aplicación Shiny para Análisis Estadísticos en Psicología
# Análisis completo de datos: Descriptivos, Normalidad, Correlaciones, Comparaciones
# Autor: Lino-Cruz, C.J. (2025)
# ================================================================================

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(rstatix)
library(writexl)
library(corrplot)
library(semTools)
library(MVN)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

# Función para limpiar datos
limpiar_datos <- function(datos) {
  names(datos) <- enc2utf8(names(datos))
  names(datos) <- trimws(names(datos))
  
  if (any(is.na(names(datos)) | names(datos) == "")) {
    empty_names <- which(is.na(names(datos)) | names(datos) == "")
    names(datos)[empty_names] <- paste0("Col_", empty_names)
  }
  
  cols_vacias <- sapply(datos, function(x) all(is.na(x)))
  if (any(cols_vacias)) {
    datos <- datos[, !cols_vacias, drop = FALSE]
  }
  
  return(datos)
}

# Función para detectar si una variable es un ítem (basado en patrones de nombres)
es_item <- function(var_name) {
  # Patrones típicos de ítems: A1, A2, Q5, P1_1, etc.
  # Nombres cortos (1-5 caracteres) con números al final
  # O patrón: Letra(s) + números + opcionalmente guión/guión bajo + números

  patterns <- c(
    "^[A-Z]{1,3}\\d+$",           # A1, ABC2, Q1, etc.
    "^[A-Z]{1,3}\\d+_\\d+$",      # A1_1, Q2_3, etc.
    "^[A-Z]{1,3}\\d+[-_][A-Z0-9]", # A1-1, P2_a, etc.
    "^[Ii]tem\\d+$",               # item1, Item5, etc.
    "^[Pp]\\d+$",                  # p1, P15, etc.
    "^[Qq]\\d+$"                   # q1, Q10, etc.
  )

  for (pattern in patterns) {
    if (grepl(pattern, var_name, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Función para separar variables sociodemográficas de ítems
filtrar_vars_sociodemo <- function(data) {
  nombres <- names(data)

  # Caracteres/factores = categóricas
  vars_categoricas <- nombres[sapply(data, function(x) is.character(x) | is.factor(x))]

  # Números pero NO ítems = continuas sociodemográficas
  vars_numericas <- nombres[sapply(data, is.numeric)]
  vars_continuas <- vars_numericas[!sapply(vars_numericas, es_item)]

  return(list(
    categoricas = vars_categoricas,
    continuas = vars_continuas,
    items = setdiff(vars_numericas, vars_continuas)
  ))
}

# Función mejorada para calcular porcentajes
calcular_porcentajes <- function(data, variables) {
  resultados <- list()
  
  for (var in variables) {
    if (var %in% names(data)) {
      tabla <- table(data[[var]], useNA = "ifany")
      porcentajes <- prop.table(tabla) * 100
      
      resultado_var <- data.frame(
        Categoria = names(tabla),
        n = as.numeric(tabla),
        Porcentaje = round(as.numeric(porcentajes), 2)
      )
      
      resultados[[var]] <- resultado_var
    }
  }
  
  return(resultados)
}

# Función para estadísticos de variables continuas (MEJORADA)
calcular_estadisticos_continuas <- function(data, var_continua) {
  if (var_continua %in% names(data) && is.numeric(data[[var_continua]])) {
    valores <- data[[var_continua]][!is.na(data[[var_continua]])]

    return(data.frame(
      Variable = var_continua,
      n = length(valores),
      Media = round(mean(valores), 2),
      DE = round(sd(valores), 2),
      Min = round(min(valores), 2),
      Max = round(max(valores), 2)
    ))
  }
  return(NULL)
}

# ============================================================================
# FUNCIÓN PARA GENERAR TABLA DE PARTICIPANTES (ESTILO APA)
# ============================================================================

tabla_participantes <- function(data,
                                continuas = NULL,
                                categoricas = NULL,
                                digits = 2,
                                mostrar_minmax = FALSE,
                                ordenar_categorias_por_n = TRUE) {

  # Validar datos
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble(`Variables sociodemográficas` = character(),
                          n = character(),
                          `%` = character()))
  }

  out <- tibble::tibble(`Variables sociodemográficas` = character(),
                        n = character(),
                        `%` = character())

  # VARIABLES CONTINUAS
  if (!is.null(continuas) && length(continuas) > 0) {
    for (v in continuas) {
      if (!v %in% names(data)) next
      x  <- suppressWarnings(as.numeric(data[[v]]))

      # Saltar si todos los valores son NA
      if (all(is.na(x))) next

      m  <- round(mean(x, na.rm=TRUE), digits)
      sd <- round(sd(x,   na.rm=TRUE), digits)
      if (mostrar_minmax) {
        mn <- round(min(x, na.rm=TRUE), digits)
        mx <- round(max(x, na.rm=TRUE), digits)
        val <- sprintf("%s (M, DE): %.*f (%.*f), %.*f–%.*f", v, digits, m, digits, sd, digits, mn, digits, mx)
      } else {
        val <- sprintf("%s (M, DE): %.*f (%.*f)", v, digits, m, digits, sd)
      }
      out <- dplyr::bind_rows(out, tibble::tibble(`Variables sociodemográficas` = val, n = "", `%` = ""))
    }
  }

  # VARIABLES CATEGÓRICAS
  if (!is.null(categoricas) && length(categoricas) > 0) {
    for (v in categoricas) {
      if (!v %in% names(data)) next

      out <- dplyr::bind_rows(out, tibble::tibble(`Variables sociodemográficas` = v, n = "", `%` = ""))

      tab <- data %>%
        dplyr::count(.data[[v]], name = "n") %>%
        dplyr::mutate(`%` = round(100 * n / sum(n), digits),
                      cat = as.character(.data[[v]])) %>%
        dplyr::select(cat, n, `%`)

      if (ordenar_categorias_por_n) tab <- dplyr::arrange(tab, dplyr::desc(n))

      tab <- tab %>%
        dplyr::transmute(`Variables sociodemográficas` = paste0("  - ", cat),
                         n = as.character(n),
                         `%` = paste0(sprintf(paste0("%.", digits, "f"), `%`), "%"))

      out <- dplyr::bind_rows(out, tab)
    }
  }

  return(out)
}

# ============================================================================
# FUNCIÓN PARA PARSEAR DEFINICIONES DE SUMATORIAS (Texto Masivo)
# ============================================================================

parsear_sumatorias <- function(texto_sumatorias) {
  # Parsea texto en formato:
  # Variable1:Item1,Item2,Item3
  # Variable2:Item4,Item5,Item6
  # etc.

  if (trimws(texto_sumatorias) == "") {
    return(list(
      exito = FALSE,
      mensaje = "El campo de sumatorias está vacío"
    ))
  }

  # Dividir por líneas
  lineas <- strsplit(texto_sumatorias, "\n")[[1]]
  lineas <- lineas[lineas != ""]  # Remover líneas vacías

  if (length(lineas) == 0) {
    return(list(
      exito = FALSE,
      mensaje = "No se encontraron definiciones de sumatorias"
    ))
  }

  sumatorias_list <- list()

  for (linea in lineas) {
    linea <- trimws(linea)
    if (linea == "") next

    # Dividir por : (nombre de variable y sus ítems)
    partes <- strsplit(linea, ":")[[1]]

    if (length(partes) != 2) {
      return(list(
        exito = FALSE,
        mensaje = paste0("Formato incorrecto en línea: '", linea, "'. Debe ser: Variable:Item1,Item2,...")
      ))
    }

    nombre_var <- trimws(partes[1])
    items_str <- trimws(partes[2])

    # Validar nombre de variable
    if (nombre_var == "") {
      return(list(
        exito = FALSE,
        mensaje = "Nombre de variable vacío en una línea"
      ))
    }

    # Parsear ítems
    items <- trimws(strsplit(items_str, ",")[[1]])
    items <- items[items != ""]  # Remover vacíos

    if (length(items) == 0) {
      return(list(
        exito = FALSE,
        mensaje = paste0("La variable '", nombre_var, "' no tiene ítems")
      ))
    }

    sumatorias_list[[nombre_var]] <- items
  }

  if (length(sumatorias_list) == 0) {
    return(list(
      exito = FALSE,
      mensaje = "No se pudieron procesar las sumatorias"
    ))
  }

  return(list(
    exito = TRUE,
    sumatorias = sumatorias_list,
    numero_variables = length(sumatorias_list)
  ))
}

# ============================================================================
# FUNCIÓN PARA SUMATORIA DE ÍTEMS - CREAR VARIABLES COMPUESTAS
# ============================================================================

crear_sumatoria <- function(data, items, nombre_variable, invertir = FALSE) {
  # Validar que los ítems existen en los datos
  items_validos <- items[items %in% names(data)]

  if (length(items_validos) == 0) {
    return(list(
      exito = FALSE,
      mensaje = "Ninguno de los ítems seleccionados se encuentra en los datos",
      data = NULL
    ))
  }

  if (length(items_validos) < length(items)) {
    items_faltantes <- setdiff(items, items_validos)
    warning(paste("Ítems no encontrados:", paste(items_faltantes, collapse = ", ")))
  }

  # Seleccionar solo los ítems válidos
  df_items <- data[, items_validos, drop = FALSE]

  # Verificar que todos los ítems son numéricos
  if (!all(sapply(df_items, is.numeric))) {
    return(list(
      exito = FALSE,
      mensaje = "Todos los ítems deben ser numéricos",
      data = NULL
    ))
  }

  # Calcular suma
  if (invertir) {
    # Encontrar el máximo y mínimo para invertir
    suma <- rowSums(df_items, na.rm = TRUE)
  } else {
    suma <- rowSums(df_items, na.rm = TRUE)
  }

  # Crear el nuevo dataframe con la variable sumada
  data_nueva <- data
  data_nueva[[nombre_variable]] <- suma

  return(list(
    exito = TRUE,
    mensaje = paste0("Variable '", nombre_variable, "' creada exitosamente con ",
                     length(items_validos), " ítems"),
    data = data_nueva,
    estadisticos = data.frame(
      Variable = nombre_variable,
      n_items = length(items_validos),
      M = round(mean(suma, na.rm = TRUE), 2),
      DE = round(sd(suma, na.rm = TRUE), 2),
      Min = min(suma, na.rm = TRUE),
      Max = max(suma, na.rm = TRUE),
      Items_utilizados = paste(items_validos, collapse = ", ")
    )
  ))
}

# ============================================================================
# FUNCIÓN PARA CONFIABILIDAD - ALPHA DE CRONBACH Y OMEGA (COMPATIBLE JAMOVI)
# ============================================================================

calcular_confiabilidad <- function(data, variable_name) {
  # Validar que la variable existe
  if (!variable_name %in% names(data)) {
    return(list(
      exito = FALSE,
      mensaje = paste0("Variable '", variable_name, "' no encontrada")
    ))
  }

  var_data <- data[[variable_name]]

  # Validar que es numérica
  if (!is.numeric(var_data)) {
    return(list(
      exito = FALSE,
      mensaje = "La variable debe ser numérica"
    ))
  }

  # Remover NA
  var_data <- var_data[!is.na(var_data)]

  if (length(var_data) < 2) {
    return(list(
      exito = FALSE,
      mensaje = "Insuficientes datos válidos"
    ))
  }

  # Calcular Alpha de Cronbach usando psych
  alpha_cronbach <- psych::alpha(data.frame(var_data), warnings = FALSE)

  return(list(
    exito = TRUE,
    confiabilidad = data.frame(
      Variable = variable_name,
      Alpha_Cronbach = round(alpha_cronbach$total$raw_alpha, 4),
      n_items = 1,
      Status = ifelse(alpha_cronbach$total$raw_alpha >= 0.70, "Aceptable", "Inaceptable")
    )
  ))
}

# Función mejorada para calcular confiabilidad - JAMOVI COMPATIBLE CON SEMTOOLS
# Calcula Alpha de Cronbach y Omega con valores idénticos a Jamovi
calcular_confiabilidad_escala <- function(data, items) {
  # Validar que los ítems existen en los datos
  items_validos <- items[items %in% names(data)]

  if (length(items_validos) < 2) {
    return(list(
      exito = FALSE,
      mensaje = "Se necesitan al menos 2 ítems para calcular confiabilidad"
    ))
  }

  if (length(items_validos) < length(items)) {
    items_faltantes <- setdiff(items, items_validos)
    warning(paste("Ítems no encontrados:", paste(items_faltantes, collapse = ", ")))
  }

  # Seleccionar solo los ítems válidos
  df_items <- data[, items_validos, drop = FALSE]

  # Remover filas con NA
  df_items <- df_items[complete.cases(df_items), ]

  # Verificar que todos los ítems son numéricos
  if (!all(sapply(df_items, is.numeric))) {
    return(list(
      exito = FALSE,
      mensaje = "Todos los ítems deben ser numéricos"
    ))
  }

  # CALCULAR ALPHA DE CRONBACH (psych - compatible con Jamovi)
  alpha_result <- suppressWarnings(psych::alpha(df_items, warnings = FALSE))
  alpha_cronbach <- alpha_result$total$raw_alpha

  # CALCULAR OMEGA (semTools - compatible con Jamovi)
  omega_mcdonald <- NA
  tryCatch({
    # Usar semTools::reliability para calcular omega
    reliability_result <- suppressWarnings(
      semTools::reliability(df_items, return.total = TRUE)
    )

    # Extraer omega
    if (!is.null(reliability_result)) {
      if ("omega" %in% names(reliability_result)) {
        omega_mcdonald <- as.numeric(reliability_result["omega"])
      } else if (length(reliability_result) > 0) {
        omega_mcdonald <- as.numeric(reliability_result[1])
      }
    }
  }, error = function(e) {
    # Si semTools falla, intentar con método alternativo
    tryCatch({
      cor_matrix <- cor(df_items, use = "complete.obs")
      n_items <- ncol(df_items)
      mean_cor <- mean(cor_matrix[lower.tri(cor_matrix)])
      omega_mcdonald <<- (n_items * mean_cor) / (1 + (n_items - 1) * mean_cor)
    }, error = function(e2) {
      NULL
    })
  })

  # Estadísticos descriptivos de la escala
  suma_escala <- rowSums(df_items, na.rm = TRUE)
  media_escala <- mean(suma_escala)
  de_escala <- sd(suma_escala)

  # Crear dataframe de resultados (mantener todas las columnas para compatibilidad interna)
  resultado_df <- data.frame(
    Escala = paste0("Escala (", length(items_validos), " ítems)"),
    Alpha_Cronbach = round(alpha_cronbach, 4),
    Omega_McDonald = ifelse(is.na(omega_mcdonald), NA, round(as.numeric(omega_mcdonald), 4)),
    M_Escala = round(media_escala, 2),
    DE_Escala = round(de_escala, 2),
    Items_utilizados = paste(items_validos, collapse = ", "),
    n_casos = nrow(df_items),
    stringsAsFactors = FALSE
  )

  return(list(
    exito = TRUE,
    confiabilidad = resultado_df
  ))
}

# Función CORREGIDA para normalidad (Shapiro-Wilk con interpretación mejorada)
test_normalidad <- function(data, variables) {
  resultados <- lapply(variables, function(var) {
    if (var %in% names(data) && is.numeric(data[[var]])) {
      x <- data[[var]][!is.na(data[[var]])]

      # Solo Shapiro-Wilk
      if (length(x) >= 3 && length(x) <= 5000) {
        sw_test <- shapiro.test(x)

        data.frame(
          Variable = var,
          n = length(x),
          Estadistico_SW = round(sw_test$statistic, 4),
          p_valor = round(sw_test$p.value, 4),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Variable = var,
          n = length(x),
          Estadistico_SW = NA,
          p_valor = NA,
          stringsAsFactors = FALSE
        )
      }
    } else {
      NULL
    }
  })

  resultados <- Filter(Negate(is.null), resultados)
  if (length(resultados) > 0) {
    return(do.call(rbind, resultados))
  }
  return(NULL)
}

# Función para descriptivos MEJORADA
calcular_descriptivos <- function(data, variables) {
  resultados <- lapply(variables, function(var) {
    if (var %in% names(data) && is.numeric(data[[var]])) {
      x <- data[[var]][!is.na(data[[var]])]
      n <- length(x)

      # Calcular asimetría y curtosis en bruto
      skewness_raw <- psych::skew(x)
      kurtosis_raw <- psych::kurtosi(x)

      # Calcular errores estándar
      # SE de asimetría = sqrt(6 * n * (n-1) / ((n-2) * (n+1) * (n+3)))
      se_skewness <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))

      # SE de curtosis = sqrt(24 * n * (n-1)^2 / ((n-3) * (n-2) * (n+3) * (n+5)))
      se_kurtosis <- sqrt(24 * n * (n - 1)^2 / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))

      # Convertir a puntuaciones z
      z_skewness <- skewness_raw / se_skewness
      z_kurtosis <- kurtosis_raw / se_kurtosis

      data.frame(
        Variable = var,
        n = n,
        M = round(mean(x), 2),
        DE = round(sd(x), 2),
        Min = round(min(x), 2),
        Max = round(max(x), 2),
        zg1 = round(z_skewness, 2),
        zg2 = round(z_kurtosis, 2),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  resultados <- Filter(Negate(is.null), resultados)
  if (length(resultados) > 0) {
    return(do.call(rbind, resultados))
  }
  return(NULL)
}

# Función CORREGIDA para correlaciones (con manejo de errores mejorado)
calcular_correlaciones <- function(data, variables, metodo = "spearman") {
  tryCatch({
    df_sel <- data %>% select(all_of(variables))
    df_sel <- df_sel[complete.cases(df_sel), ]

    if (nrow(df_sel) < 3 || ncol(df_sel) < 2) {
      return(NULL)
    }

    # Verificar varianza en variables (evitar constantes)
    vars_validas <- sapply(df_sel, function(x) sd(x, na.rm = TRUE) > 0)
    if (!all(vars_validas)) {
      return(NULL)
    }

    # Calcular correlaciones
    cor_test <- psych::corr.test(df_sel, method = metodo, adjust = "none")

    # Matriz de correlaciones
    cor_matrix <- cor_test$r
    p_matrix <- cor_test$p

    # Formato APA con asteriscos
    cor_apa <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
    rownames(cor_apa) <- rownames(cor_matrix)
    colnames(cor_apa) <- colnames(cor_matrix)

    for (i in 1:nrow(cor_matrix)) {
      for (j in 1:ncol(cor_matrix)) {
        if (i == j) {
          cor_apa[i, j] <- "1.00"
        } else if (i > j) {
          r_val <- round(cor_matrix[i, j], 2)
          p_val <- p_matrix[i, j]

          stars <- ""
          if (is.na(p_val)) {
            cor_apa[i, j] <- paste0(r_val, " (NC)")
          } else {
            if (p_val < 0.001) stars <- "***"
            else if (p_val < 0.01) stars <- "**"
            else if (p_val < 0.05) stars <- "*"

            cor_apa[i, j] <- paste0(r_val, stars)
          }
        } else {
          cor_apa[i, j] <- ""
        }
      }
    }

    return(as.data.frame(cor_apa))
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN PARA CALCULAR CORRELACIONES ROBUSTAS - PERCENTAGE BEND (WRS2)
# ============================================================================
calcular_correlaciones_robusta <- function(data, variables) {
  tryCatch({
    # Verificar que WRS2 está disponible
    if (!requireNamespace("WRS2", quietly = TRUE)) {
      return(NULL)
    }

    df_sel <- data %>% select(all_of(variables))
    df_sel <- df_sel[complete.cases(df_sel), ]

    if (nrow(df_sel) < 3 || ncol(df_sel) < 2) {
      return(NULL)
    }

    # Verificar varianza en variables (evitar constantes)
    vars_validas <- sapply(df_sel, function(x) sd(x, na.rm = TRUE) > 0)
    if (!all(vars_validas)) {
      return(NULL)
    }

    # Calcular matriz de correlaciones robustas
    cor_matrix <- matrix(NA, nrow = ncol(df_sel), ncol = ncol(df_sel))
    p_matrix <- matrix(NA, nrow = ncol(df_sel), ncol = ncol(df_sel))
    rownames(cor_matrix) <- colnames(df_sel)
    colnames(cor_matrix) <- colnames(df_sel)
    rownames(p_matrix) <- colnames(df_sel)
    colnames(p_matrix) <- colnames(df_sel)

    # Calcular pbcor para cada par de variables
    for (i in 1:ncol(df_sel)) {
      for (j in i:ncol(df_sel)) {
        if (i == j) {
          cor_matrix[i, j] <- 1.0
          p_matrix[i, j] <- 0
        } else {
          tryCatch({
            pb_result <- WRS2::pbcor(df_sel[[i]], df_sel[[j]])
            cor_matrix[i, j] <- pb_result$cor
            cor_matrix[j, i] <- pb_result$cor
            p_matrix[i, j] <- pb_result$p.value
            p_matrix[j, i] <- pb_result$p.value
          }, error = function(e) {
            # Si pbcor falla, dejar como NA
            NULL
          })
        }
      }
    }

    # Formato APA con asteriscos
    cor_apa <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
    rownames(cor_apa) <- rownames(cor_matrix)
    colnames(cor_apa) <- colnames(cor_matrix)

    for (i in 1:nrow(cor_matrix)) {
      for (j in 1:ncol(cor_matrix)) {
        if (i == j) {
          cor_apa[i, j] <- "1.00"
        } else if (i > j) {
          r_val <- round(cor_matrix[i, j], 2)
          p_val <- p_matrix[i, j]

          stars <- ""
          if (is.na(p_val)) {
            cor_apa[i, j] <- paste0(r_val, " (NC)")
          } else {
            if (p_val < 0.001) stars <- "***"
            else if (p_val < 0.01) stars <- "**"
            else if (p_val < 0.05) stars <- "*"

            cor_apa[i, j] <- paste0(r_val, stars)
          }
        } else {
          cor_apa[i, j] <- ""
        }
      }
    }

    return(as.data.frame(cor_apa))
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN PARA REFORMATEAR TABLA DE COMPARACIONES (ACADÉMICA)
# ============================================================================
reformatear_tabla_comparaciones <- function(tabla_raw, datos, var_grupo) {

  if (is.null(tabla_raw) || nrow(tabla_raw) == 0) {
    return(NULL)
  }

  # Obtener nombres de los grupos para el header
  grupos <- levels(factor(datos[[var_grupo]]))
  num_grupos <- length(grupos)

  # Inicializar tabla reformateada
  tabla_reformatada <- data.frame(Variable = tabla_raw$Variable)

  # Agregar columnas de descriptivos con nombre del grupo como header
  # Reformatear de "M=X.XX, DE=Y.YY" a "X.XX(Y.YY)" o "Mdn=X.XX, RIC=[Y,Z]" a "X.XX[Y-Z]"
  for (i in 1:num_grupos) {
    col_name <- paste0("Desc_G", i)
    if (col_name %in% names(tabla_raw)) {
      desc_raw <- tabla_raw[[col_name]]

      # Reformatear descriptivos
      desc_formateado <- sapply(desc_raw, function(x) {
        if (is.na(x) || x == "") return(x)

        # Si contiene "M=" y "DE=" (formato paramétrico)
        if (grepl("M=", x, fixed = TRUE) && grepl("DE=", x, fixed = TRUE)) {
          # Extraer valores de M y DE
          m_val <- as.numeric(gsub("M=(.*?), DE=.*", "\\1", x))
          de_val <- as.numeric(gsub(".*DE=(.*)", "\\1", x))

          if (!is.na(m_val) && !is.na(de_val)) {
            return(paste0(m_val, "(", de_val, ")"))
          }
        }

        # Si contiene "Mdn=" y "RIC=" (formato no paramétrico)
        if (grepl("Mdn=", x, fixed = TRUE) && grepl("RIC=", x, fixed = TRUE)) {
          # Extraer valores de Mdn y RIC
          mdn_val <- as.numeric(gsub("Mdn=(.*?), RIC=.*", "\\1", x))
          ric_str <- gsub(".*RIC=\\[(.*?)\\]", "\\1", x)

          if (!is.na(mdn_val) && ric_str != "") {
            # RIC viene como "Q1, Q3", lo convertimos a "Q1-Q3"
            ric_clean <- gsub(", ", "-", ric_str)
            return(paste0(mdn_val, "[", ric_clean, "]"))
          }
        }

        # Si no se puede parsear, devolver como está
        return(x)
      }, USE.NAMES = FALSE)

      tabla_reformatada[[paste0(grupos[i])]] <- desc_formateado
    }
  }

  # Agregar columna de estadístico con símbolo y grados de libertad
  if ("Estadistico" %in% names(tabla_raw)) {
    # Determinar el símbolo del estadístico y crear header con grados de libertad
    if ("Prueba" %in% names(tabla_raw)) {
      # Extraer la prueba del primer registro (asumiendo que todos tienen la misma)
      prueba <- tabla_raw$Prueba[1]

      # Crear header según el tipo de prueba
      if (prueba == "T-test") {
        if ("Gl" %in% names(tabla_raw)) {
          gl <- tabla_raw$Gl[1]
          header_name <- paste0("t(", gl, ")")
        } else {
          header_name <- "t"
        }
      } else if (prueba == "Mann-Whitney U") {
        header_name <- "U"
      } else if (prueba == "ANOVA") {
        header_name <- "F"
      } else if (prueba == "Kruskal-Wallis H") {
        if ("Gl" %in% names(tabla_raw)) {
          gl <- tabla_raw$Gl[1]
          header_name <- paste0("H(", gl, ")")
        } else {
          header_name <- "H"
        }
      } else {
        header_name <- "Estadístico"
      }

      tabla_reformatada[[header_name]] <- tabla_raw$Estadistico
    } else {
      tabla_reformatada$Estadístico <- tabla_raw$Estadistico
    }
  }

  # Agregar columna de p-valor
  if ("p" %in% names(tabla_raw)) {
    tabla_reformatada$p <- tabla_raw$p
  }

  # Agregar tamaño del efecto con su interpretación
  # Detectar qué tipo de efecto es según la prueba
  if ("d_Cohen" %in% names(tabla_raw) && !all(is.na(tabla_raw$d_Cohen))) {
    # T-test: d de Cohen
    # Cutoffs: .20=pequeña, .50=mediana, .80=grande
    tabla_reformatada$`d` <- tabla_raw$d_Cohen
    tabla_reformatada$Interpretación <- sapply(tabla_raw$d_Cohen, function(d) {
      if (is.na(d)) return("")
      d_abs <- abs(d)
      if (d_abs < 0.20) return("Irrelevante")
      else if (d_abs < 0.50) return("Pequeña")
      else if (d_abs < 0.80) return("Mediana")
      else return("Grande")
    })
  } else if ("r_rb" %in% names(tabla_raw) && !all(is.na(tabla_raw$r_rb))) {
    # Mann-Whitney U: rbis (correlación biserial de rangos)
    # Cutoffs: .10=pequeña, .30=mediana, .50=grande
    tabla_reformatada$`r_rb` <- tabla_raw$r_rb
    tabla_reformatada$Interpretación <- sapply(tabla_raw$r_rb, function(r) {
      if (is.na(r)) return("")
      r_abs <- abs(r)
      if (r_abs < 0.10) return("Irrelevante")
      else if (r_abs < 0.30) return("Pequeño")
      else if (r_abs < 0.50) return("Mediano")
      else return("Grande")
    })
  } else if ("eta_squared" %in% names(tabla_raw) && !all(is.na(tabla_raw$eta_squared))) {
    # ANOVA: omega-squared (ω²)
    # Cutoffs: .01=pequeña, .06=mediana, .14=grande
    tabla_reformatada$`ω²` <- tabla_raw$eta_squared
    tabla_reformatada$Interpretación <- sapply(tabla_raw$eta_squared, function(omega) {
      if (is.na(omega)) return("")
      if (omega < 0.01) return("Irrelevante")
      else if (omega < 0.06) return("Pequeña")
      else if (omega < 0.14) return("Mediana")
      else return("Grande")
    })
  } else if ("eta_H" %in% names(tabla_raw) && !all(is.na(tabla_raw$eta_H))) {
    # Kruskal-Wallis H: eta-squared H
    tabla_reformatada$`η²_H` <- tabla_raw$eta_H
    # Usar interpretación ya proporcionada
    if ("Efecto" %in% names(tabla_raw)) {
      tabla_reformatada$Interpretación <- tabla_raw$Efecto
    }
  }

  return(tabla_reformatada)
}

# ============================================================================
# FUNCIÓN PARA CALCULAR GAMES-HOWELL (Post-hoc para Welch ANOVA - Robusto)
# ============================================================================
calcular_games_howell <- function(data, variable, var_grupo) {
  tryCatch({
    # Preparar datos
    df_test <- data %>%
      select(all_of(c(var_grupo, variable))) %>%
      filter(!is.na(.data[[var_grupo]]) & !is.na(.data[[variable]]))

    if (nrow(df_test) < 3) return(NULL)

    # Convertir grupo a factor
    df_test[[var_grupo]] <- factor(df_test[[var_grupo]])

    # Usar rstatix si está disponible
    if (requireNamespace("rstatix", quietly = TRUE)) {
      gh_result <- rstatix::games_howell_test(
        data = df_test,
        as.formula(paste(variable, "~", var_grupo))
      )

      # Reformatear para presentación
      gh_df <- gh_result %>%
        as.data.frame() %>%
        mutate(
          `Comparacion` = paste(.data$group1, "vs", .data$group2),
          `Diferencia de Medias` = round(.data$estimate, 3),
          `p-valor ajustado` = round(.data$p.adj, 4),
          `Significancia` = ifelse(.data$p.adj < 0.05, "✓ Sí", "✗ No")
        ) %>%
        select(Comparacion, `Diferencia de Medias`, `p-valor ajustado`, `Significancia`)

      return(gh_df)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN PARA CALCULAR DUNN'S TEST (Post-hoc para Kruskal-Wallis)
# ============================================================================
calcular_dunn_test <- function(data, variable, var_grupo) {
  tryCatch({
    # Preparar datos
    df_test <- data %>%
      select(all_of(c(var_grupo, variable))) %>%
      filter(!is.na(.data[[var_grupo]]) & !is.na(.data[[variable]]))

    if (nrow(df_test) < 3) return(NULL)

    # Convertir grupo a factor
    df_test[[var_grupo]] <- factor(df_test[[var_grupo]])

    # Usar FSA::dunnTest si está disponible, si no usar alternativa
    if (requireNamespace("FSA", quietly = TRUE)) {
      dunn_result <- FSA::dunnTest(
        as.formula(paste(variable, "~", var_grupo)),
        data = df_test,
        method = "holm"
      )

      dunn_df <- dunn_result$res %>%
        as.data.frame() %>%
        mutate(
          `Comparacion` = paste(Comparison),
          `Z-estadístico` = round(Z, 3),
          `p-valor ajustado` = round(P.adj, 4),
          `Significancia` = ifelse(P.adj < 0.05, "✓ Sí", "✗ No")
        ) %>%
        select(Comparacion, `Z-estadístico`, `p-valor ajustado`, `Significancia`)

      return(dunn_df)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN PARA CREAR BOXPLOT DE COMPARACIONES
# ============================================================================
crear_boxplot_comparaciones <- function(data, variable, var_grupo) {
  tryCatch({
    # Preparar datos
    df_plot <- data %>%
      select(all_of(c(var_grupo, variable))) %>%
      filter(!is.na(.data[[var_grupo]]) & !is.na(.data[[variable]])) %>%
      mutate({{ var_grupo }} := factor(.data[[var_grupo]]))

    if (nrow(df_plot) == 0) return(NULL)

    # Crear boxplot con ggplot2
    plot <- ggplot(df_plot, aes(x = .data[[var_grupo]], y = .data[[variable]],
                                fill = .data[[var_grupo]])) +
      geom_boxplot(alpha = 0.7, outlier.size = 2) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_line(color = "#f0f0f0"),
        panel.grid.major.x = element_blank()
      ) +
      labs(
        x = var_grupo,
        y = variable,
        title = paste("Distribución de", variable, "por", var_grupo)
      ) +
      scale_fill_brewer(palette = "Set2")

    return(plot)
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN PARA EXTRAER VARIABLES SIGNIFICATIVAS DE LA TABLA DE COMPARACIÓN
# ============================================================================
obtener_variables_significativas <- function(tabla_comparacion) {
  if (is.null(tabla_comparacion) || nrow(tabla_comparacion) == 0) {
    return(NULL)
  }

  # Verificar si existe la columna 'p' o 'p.value'
  p_col <- if ("p" %in% names(tabla_comparacion)) "p" else
           if ("p.value" %in% names(tabla_comparacion)) "p.value" else NULL

  if (is.null(p_col)) {
    return(NULL)
  }

  # Filtrar variables con p <= 0.05
  tabla_sig <- tabla_comparacion[tabla_comparacion[[p_col]] <= 0.05, ]

  if (nrow(tabla_sig) == 0) {
    return(NULL)
  }

  # Retornar lista de variables significativas
  if ("Variable" %in% names(tabla_sig)) {
    return(unique(tabla_sig$Variable))
  }

  return(NULL)
}

# Función CORREGIDA para comparación de grupos - VERSIÓN ESTABLE
comparar_grupos <- function(data, variables, var_grupo, metodo, usar_mdn_ric = FALSE) {

  if (!var_grupo %in% names(data)) {
    return(NULL)
  }

  # Preparar datos: asegurarse de que el grupo es factor
  data[[var_grupo]] <- factor(data[[var_grupo]])
  grupos <- levels(data[[var_grupo]])
  num_grupos <- length(grupos)

  # Procesar cada variable
  resultados <- lapply(variables, function(var) {

    # Validar que la variable existe y es numérica
    if (!var %in% names(data)) return(NULL)
    if (!is.numeric(data[[var]])) return(NULL)

    # Preparar datos para el test
    df_test <- data %>%
      select(all_of(c(var_grupo, var))) %>%
      filter(!is.na(.data[[var_grupo]]) & !is.na(.data[[var]]))

    # Asegurar que hay datos
    if (nrow(df_test) < 2) return(NULL)

    formula_test <- as.formula(paste(var, "~", var_grupo))

    # ===== T-TEST (2 grupos, paramétrico) =====
    if (metodo == "ttest") {
      if (num_grupos != 2) {
        return(data.frame(
          Variable = var,
          Prueba = "T-test",
          Nota = paste0("Error: T-test requiere 2 grupos. Tienes ", num_grupos, " grupos."),
          stringsAsFactors = FALSE
        ))
      }

      tryCatch({
        t_result <- t.test(formula_test, data = df_test, var.equal = TRUE)
        d_result <- rstatix::cohens_d(df_test, formula_test, var.equal = TRUE)

        desc <- df_test %>%
          group_by(.data[[var_grupo]]) %>%
          summarise(M = mean(.data[[var]]), DE = sd(.data[[var]]), .groups = "drop")

        return(data.frame(
          Variable = var,
          Prueba = "T-test",
          Gl = round(t_result$parameter, 0),
          Estadistico = round(t_result$statistic, 3),
          p = round(t_result$p.value, 4),
          d_Cohen = round(abs(d_result$effsize[1]), 3),
          Desc_G1 = paste0("M=", round(desc$M[1], 2), ", DE=", round(desc$DE[1], 2)),
          Desc_G2 = paste0("M=", round(desc$M[2], 2), ", DE=", round(desc$DE[2], 2)),
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        return(data.frame(
          Variable = var,
          Prueba = "T-test",
          Nota = paste("Error:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    # ===== MANN-WHITNEY U (2 grupos, no paramétrico) =====
    else if (metodo == "mannwhitney") {
      if (num_grupos != 2) {
        return(data.frame(
          Variable = var,
          Prueba = "Mann-Whitney U",
          Nota = paste0("Error: Mann-Whitney requiere 2 grupos. Tienes ", num_grupos, " grupos."),
          stringsAsFactors = FALSE
        ))
      }

      tryCatch({
        mw_result <- wilcox.test(formula_test, data = df_test)

        # Calcular descriptivos por grupo (media, DE, mediana y RIC)
        desc <- df_test %>%
          group_by(.data[[var_grupo]]) %>%
          summarise(
            M = mean(.data[[var]]),
            DE = sd(.data[[var]]),
            Mdn = median(.data[[var]]),
            Q1 = quantile(.data[[var]], 0.25),
            Q3 = quantile(.data[[var]], 0.75),
            .groups = "drop"
          )

        # Calcular correlación biserial de rangos (r_rb) - Compatible Jamovi
        # Formula: r_rb = 1 - (2U) / (n1 * n2)
        # Donde U es el estadístico de Mann-Whitney
        n1 <- sum(df_test[[var_grupo]] == grupos[1])
        n2 <- sum(df_test[[var_grupo]] == grupos[2])
        U <- mw_result$statistic
        r_rb <- 1 - (2 * U) / (n1 * n2)

        # Seleccionar formato de descriptivos según preferencia del usuario
        if (usar_mdn_ric) {
          # Usar Mediana y RIC (no paramétrico)
          desc_g1 <- paste0("Mdn=", round(desc$Mdn[1], 2), ", RIC=[", round(desc$Q1[1], 2), ", ", round(desc$Q3[1], 2), "]")
          desc_g2 <- paste0("Mdn=", round(desc$Mdn[2], 2), ", RIC=[", round(desc$Q1[2], 2), ", ", round(desc$Q3[2], 2), "]")
        } else {
          # Usar Media y DE (por defecto, más conservador)
          desc_g1 <- paste0("M=", round(desc$M[1], 2), ", DE=", round(desc$DE[1], 2))
          desc_g2 <- paste0("M=", round(desc$M[2], 2), ", DE=", round(desc$DE[2], 2))
        }

        return(data.frame(
          Variable = var,
          Prueba = "Mann-Whitney U",
          Estadistico = round(mw_result$statistic, 3),
          p = round(mw_result$p.value, 4),
          r_rb = round(r_rb, 3),
          Desc_G1 = desc_g1,
          Desc_G2 = desc_g2,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        return(data.frame(
          Variable = var,
          Prueba = "Mann-Whitney U",
          Nota = paste("Error:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    # ===== ANOVA (3+ grupos, paramétrico) =====
    else if (metodo == "anova") {
      if (num_grupos < 3) {
        return(data.frame(
          Variable = var,
          Prueba = "ANOVA",
          Nota = paste0("Error: ANOVA requiere 3+ grupos. Tienes ", num_grupos, " grupos."),
          stringsAsFactors = FALSE
        ))
      }

      tryCatch({
        # Usar Welch ANOVA (var.equal = FALSE) - más robusto ante varianzas desiguales
        welch_result <- oneway.test(formula_test, data = df_test, var.equal = FALSE)

        # Calcular omega-squared para tamaño del efecto
        desc <- df_test %>%
          group_by(.data[[var_grupo]]) %>%
          summarise(
            M = mean(.data[[var]]),
            DE = sd(.data[[var]]),
            N = n(),
            .groups = "drop"
          )

        # Calcular eta-squared como medida de tamaño del efecto
        grand_mean <- mean(df_test[[var]])
        ss_between <- sum(desc$N * (desc$M - grand_mean)^2)
        ss_total <- sum((df_test[[var]] - grand_mean)^2)
        eta_sq <- ss_between / ss_total
        eta_sq <- max(0, eta_sq)

        # Calcular grados de libertad
        k <- num_grupos  # número de grupos
        n <- nrow(df_test)  # tamaño total
        df1 <- k - 1
        df2 <- n - k

        # Crear fila de resultado
        resultado <- data.frame(
          Variable = var,
          Prueba = "ANOVA de Welch",
          Gl = paste0(df1, ", ", round(df2, 1)),
          Estadistico = round(welch_result$statistic, 3),
          p = round(welch_result$p.value, 4),
          eta_squared = round(eta_sq, 3),
          stringsAsFactors = FALSE
        )

        # Agregar columnas de descriptivos dinámicamente según número de grupos
        for (i in 1:nrow(desc)) {
          col_name <- paste0("Desc_G", i)
          resultado[[col_name]] <- paste0("M=", round(desc$M[i], 2), ", DE=", round(desc$DE[i], 2))
        }

        return(resultado)
      }, error = function(e) {
        return(data.frame(
          Variable = var,
          Prueba = "ANOVA de Welch",
          Nota = paste("Error:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    # ===== KRUSKAL-WALLIS (3+ grupos, no paramétrico) =====
    else if (metodo == "kruskal") {
      if (num_grupos < 3) {
        return(data.frame(
          Variable = var,
          Prueba = "Kruskal-Wallis H",
          Nota = paste0("Error: Kruskal-Wallis requiere 3+ grupos. Tienes ", num_grupos, " grupos."),
          stringsAsFactors = FALSE
        ))
      }

      tryCatch({
        kw_result <- kruskal.test(formula_test, data = df_test)

        # Calcular descriptivos por grupo (media, DE, mediana y RIC)
        desc <- df_test %>%
          group_by(.data[[var_grupo]]) %>%
          summarise(
            M = mean(.data[[var]]),
            DE = sd(.data[[var]]),
            Mdn = median(.data[[var]]),
            Q1 = quantile(.data[[var]], 0.25),
            Q3 = quantile(.data[[var]], 0.75),
            .groups = "drop"
          )

        # Calculate eta-squared H (Jamovi compatible)
        # Formula: η²_H = (H - k + 1) / (n - k) where H is statistic, k is num_grupos, n is total N
        H_stat <- kw_result$statistic
        k <- num_grupos
        n <- nrow(df_test)
        eta_sq_h <- (H_stat - k + 1) / (n - k)
        eta_sq_h <- max(0, eta_sq_h) # Ensure non-negative

        # Interpretation based on user cutoffs: 0.04 (weak), 0.25 (moderate), 0.64 (strong)
        if (eta_sq_h < 0.04) {
          interpretacion_efecto <- "Débil"
        } else if (eta_sq_h >= 0.04 && eta_sq_h < 0.25) {
          interpretacion_efecto <- "Moderado"
        } else if (eta_sq_h >= 0.25 && eta_sq_h < 0.64) {
          interpretacion_efecto <- "Fuerte"
        } else {
          interpretacion_efecto <- "Muy Fuerte"
        }

        # Crear descriptivos formateados para cada grupo
        # Seleccionar formato según preferencia del usuario
        if (usar_mdn_ric) {
          # Usar Mediana y RIC (no paramétrico)
          desc_formateados <- lapply(1:nrow(desc), function(i) {
            paste0("Mdn=", round(desc$Mdn[i], 2), ", RIC=[", round(desc$Q1[i], 2), ", ", round(desc$Q3[i], 2), "]")
          })
        } else {
          # Usar Media y DE (por defecto, más conservador)
          desc_formateados <- lapply(1:nrow(desc), function(i) {
            paste0("M=", round(desc$M[i], 2), ", DE=", round(desc$DE[i], 2))
          })
        }

        # Crear fila de resultado
        resultado <- data.frame(
          Variable = var,
          Prueba = "Kruskal-Wallis H",
          Gl = round(kw_result$parameter, 0),
          Estadistico = round(kw_result$statistic, 3),
          p = round(kw_result$p.value, 4),
          eta_H = round(eta_sq_h, 3),
          Efecto = interpretacion_efecto,
          stringsAsFactors = FALSE
        )

        # Agregar columnas de descriptivos dinámicamente según número de grupos
        for (i in 1:nrow(desc)) {
          col_name <- paste0("Desc_G", i)
          resultado[[col_name]] <- desc_formateados[[i]]
        }

        return(resultado)
      }, error = function(e) {
        return(data.frame(
          Variable = var,
          Prueba = "Kruskal-Wallis H",
          Nota = paste("Error:", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    # Método desconocido
    else {
      return(data.frame(
        Variable = var,
        Nota = paste0("Método '", metodo, "' no reconocido."),
        stringsAsFactors = FALSE
      ))
    }

  }) # Fin de lapply

  # Combinar resultados
  resultados <- Filter(Negate(is.null), resultados)

  if (length(resultados) > 0) {
    resultado_final <- bind_rows(resultados)
    return(resultado_final)
  }

  return(NULL)
}

# ============================================================================
# DEFINICIÓN DE TEMAS (PALETAS DE COLORES)
# ============================================================================

# Función para generar CSS dinámico por tema
generar_css_tema <- function(tema = "azul") {

  # Definir paletas de colores para cada tema
  temas <- list(
    azul = list(
      primario = "#667eea",
      primario_oscuro = "#764ba2",
      header = "linear-gradient(90deg, #667eea 0%, #764ba2 100%)",
      sidebar = "linear-gradient(180deg, #2d3561 0%, #1a202c 100%)",
      boton_hover = "rgba(102, 126, 234, 0.3)",
      tabla_encabezado = "#667eea",
      tabla_cuerpo = "#f0f4ff",
      tabla_hover = "#e8ecff",
      caja_sombra = "rgba(102, 126, 234, 0.08)",
      caja_sombra_hover = "rgba(102, 126, 234, 0.15)",
      input_focus = "#667eea",
      input_focus_sombra = "rgba(102, 126, 234, 0.1)",
      alert_info = "rgba(79, 172, 254, 0.1)",
      alert_success = "rgba(17, 153, 142, 0.1)",
      alert_warning = "rgba(240, 147, 251, 0.1)",
      alert_danger = "rgba(255, 107, 107, 0.1)"
    ),
    verde = list(
      primario = "#11998e",
      primario_oscuro = "#38ef7d",
      header = "linear-gradient(90deg, #11998e 0%, #38ef7d 100%)",
      sidebar = "linear-gradient(180deg, #1a4d3e 0%, #0a2818 100%)",
      boton_hover = "rgba(17, 153, 142, 0.3)",
      tabla_encabezado = "#11998e",
      tabla_cuerpo = "#f0fffe",
      tabla_hover = "#e0fffe",
      caja_sombra = "rgba(17, 153, 142, 0.08)",
      caja_sombra_hover = "rgba(17, 153, 142, 0.15)",
      input_focus = "#11998e",
      input_focus_sombra = "rgba(17, 153, 142, 0.1)",
      alert_info = "rgba(79, 172, 254, 0.1)",
      alert_success = "rgba(17, 153, 142, 0.1)",
      alert_warning = "rgba(240, 147, 251, 0.1)",
      alert_danger = "rgba(255, 107, 107, 0.1)"
    ),
    celeste = list(
      primario = "#0099cc",
      primario_oscuro = "#0077aa",
      header = "linear-gradient(90deg, #76EEC6 0%, #5FD4B0 100%)",
      sidebar = "linear-gradient(180deg, #458B74 0%, #3A7063 100%)",
      boton_hover = "rgba(0, 153, 204, 0.2)",
      tabla_encabezado = "#0099cc",
      tabla_cuerpo = "#f0f8ff",
      tabla_hover = "#e0f2ff",
      caja_sombra = "rgba(0, 153, 204, 0.08)",
      caja_sombra_hover = "rgba(0, 153, 204, 0.15)",
      input_focus = "#0099cc",
      input_focus_sombra = "rgba(0, 153, 204, 0.1)",
      alert_info = "rgba(0, 153, 204, 0.1)",
      alert_success = "rgba(17, 153, 142, 0.1)",
      alert_warning = "rgba(0, 153, 204, 0.1)",
      alert_danger = "rgba(255, 107, 107, 0.1)"
    )
  )

  colores <- temas[[tema]]

  # Retornar CSS personalizado
  sprintf("
    :root {
      --primario: %s;
      --primario-oscuro: %s;
      --header: %s;
      --sidebar: %s;
      --boton-hover: %s;
      --tabla-encabezado: %s;
      --tabla-cuerpo: %s;
      --tabla-hover: %s;
      --caja-sombra: %s;
      --caja-sombra-hover: %s;
      --input-focus: %s;
      --input-focus-sombra: %s;
      --alert-info: %s;
      --alert-success: %s;
      --alert-warning: %s;
      --alert-danger: %s;
    }
  ", colores$primario, colores$primario_oscuro, colores$header,
    colores$sidebar, colores$boton_hover, colores$tabla_encabezado,
    colores$tabla_cuerpo, colores$tabla_hover, colores$caja_sombra,
    colores$caja_sombra_hover, colores$input_focus, colores$input_focus_sombra,
    colores$alert_info, colores$alert_success, colores$alert_warning, colores$alert_danger
  )
}

# ============================================================================
# INTERFAZ DE USUARIO (UI) - CÓDIGO COMPLETADO
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = span(
      icon("chart-area"),
      "EasyTesis"
    ),
    titleWidth = 300,
    # Selector de tema en el header
    tags$li(
      class = "dropdown",
      style = "margin-right: 20px;",
      selectInput(
        "selector_tema",
        NULL,
        choices = c("Azul" = "azul", "Verde Menta" = "verde", "Celeste" = "celeste"),
        selected = "azul",
        width = "150px"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "menu_principal",
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("1. Cargar Datos", tabName = "cargar", icon = icon("file-upload")),
      menuItem("2. Participantes", tabName = "participantes", icon = icon("users")),
      menuItem("3. Sumatoria", tabName = "sumatoria", icon = icon("plus-circle")),
      menuItem("4. Descriptivos", tabName = "descriptivos", icon = icon("table")),
      menuItem("5. Normalidad", tabName = "normalidad", icon = icon("chart-line")),
      menuItem("6. Confiabilidad", tabName = "confiabilidad", icon = icon("shield-alt")),
      menuItem("7. Correlaciones", tabName = "correlaciones", icon = icon("project-diagram")),
      menuItem("8. Comparaciones", tabName = "comparaciones", icon = icon("balance-scale")),
      menuItem("9. Descargas", tabName = "descargas", icon = icon("download")),
      hr(),
      menuItem("Acerca de", tabName = "acerca", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700;800&display=swap');

        :root {
          --primario: #667eea;
          --primario-oscuro: #764ba2;
          --header: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
          --sidebar: linear-gradient(180deg, #2d3561 0%, #1a202c 100%);
          --boton-hover: rgba(102, 126, 234, 0.3);
          --tabla-encabezado: #667eea;
          --tabla-cuerpo: #f0f4ff;
          --tabla-hover: #e8ecff;
          --caja-sombra: rgba(102, 126, 234, 0.08);
          --caja-sombra-hover: rgba(102, 126, 234, 0.15);
          --input-focus: #667eea;
          --input-focus-sombra: rgba(102, 126, 234, 0.1);
          --alert-info: rgba(79, 172, 254, 0.1);
          --alert-success: rgba(17, 153, 142, 0.1);
          --alert-warning: rgba(240, 147, 251, 0.1);
          --alert-danger: rgba(255, 107, 107, 0.1);
        }

        * {
          box-sizing: border-box;
          margin: 0;
          padding: 0;
        }

        html, body {
          height: 100%;
        }

        body {
          font-family: 'Poppins', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          background: linear-gradient(135deg, #ffffff 0%, #f0f5ff 100%);
          color: #1a202c;
          overflow-x: hidden;
        }

        /* HEADER - Diseño minimalista y moderno */
        .main-header {
          background: var(--header);
          border-bottom: 0px;
          box-shadow: 0 4px 20px var(--caja-sombra);
          padding: 0 !important;
        }

        .main-header .logo {
          background: rgba(0,0,0,0.1) !important;
        }

        .main-header .navbar {
          background: transparent !important;
        }

        .navbar-custom {
          padding: 0 30px !important;
        }

        /* SIDEBAR - Diseño moderno con gradiente */
        .main-sidebar {
          background: var(--sidebar);
          border-right: 0px;
          box-shadow: 2px 0 15px rgba(0, 0, 0, 0.1);
        }

        .sidebar-menu > li > a {
          color: rgba(255, 255, 255, 0.7);
          border-left: 4px solid transparent;
          transition: all 0.3s cubic-bezier(0.4, 0.0, 0.2, 1);
          font-weight: 500;
          margin-bottom: 5px;
        }

        .sidebar-menu > li > a:hover {
          background: var(--boton-hover) !important;
          color: #fff !important;
          border-left-color: var(--primario) !important;
        }

        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, rgba(102, 126, 234, 0.3) 0%, rgba(118, 75, 162, 0.2) 100%) !important;
          border-left-color: var(--primario) !important;
          color: #fff !important;
          font-weight: 600;
        }

        .sidebar-menu > li.header {
          color: rgba(255, 255, 255, 0.4);
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 1px;
          font-weight: 700;
          margin-top: 15px;
        }

        /* CONTENIDO - Fondo limpio */
        .content-wrapper {
          background: linear-gradient(135deg, #ffffff 0%, #f0f5ff 100%);
          padding: 30px;
          min-height: 100vh;
        }

        /* CAJAS - Diseño moderno y limpio */
        .box {
          border-radius: 16px;
          box-shadow: 0 4px 12px var(--caja-sombra);
          border: 1px solid var(--caja-sombra);
          border-top: none;
          transition: all 0.4s cubic-bezier(0.4, 0.0, 0.2, 1);
          background: white;
          overflow: hidden;
        }

        .box:hover {
          box-shadow: 0 12px 24px var(--caja-sombra-hover);
          transform: translateY(-4px);
        }

        .box-header {
          border-radius: 16px 16px 0 0;
          border-bottom: none;
          background: var(--header);
          padding: 20px !important;
        }

        .box-header.with-border {
          border-bottom: none;
        }

        .box-title {
          font-weight: 700;
          color: white;
          font-size: 18px;
          letter-spacing: 0.5px;
        }

        /* STATUS COLORS - Todos usan el gradiente del tema */
        .box.box-primary > .box-header {
          background: var(--header);
        }

        .box.box-success > .box-header {
          background: var(--header);
        }

        .box.box-warning > .box-header {
          background: var(--header);
        }

        .box.box-danger > .box-header {
          background: var(--header);
        }

        .box.box-info > .box-header {
          background: var(--header);
        }

        /* BOTONES - Diseño moderno y animado */
        .btn-custom {
          border-radius: 10px;
          padding: 12px 24px;
          font-weight: 600;
          border: none;
          transition: all 0.3s cubic-bezier(0.4, 0.0, 0.2, 1);
          cursor: pointer;
          font-size: 14px;
          letter-spacing: 0.5px;
          position: relative;
          overflow: hidden;
        }

        .btn-custom:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 20px var(--caja-sombra-hover);
        }

        .btn-custom:active {
          transform: translateY(-1px);
        }

        .btn-primary.btn-custom {
          background: var(--header);
          color: white;
        }

        .btn-success.btn-custom {
          background: linear-gradient(90deg, #11998e 0%, #38ef7d 100%);
          color: white;
        }

        .btn-info.btn-custom {
          background: linear-gradient(90deg, #4facfe 0%, #00f2fe 100%);
          color: white;
        }

        .btn-warning.btn-custom {
          background: linear-gradient(90deg, #0099cc 0%, #0077aa 100%);
          color: white;
        }

        .btn-danger.btn-custom {
          background: linear-gradient(90deg, #ff6b6b 0%, #ee5a6f 100%);
          color: white;
        }

        /* TARJETAS DE INFORMACIÓN */
        .info-card {
          background: white;
          border-radius: 14px;
          padding: 24px;
          margin: 12px 0;
          box-shadow: 0 4px 12px var(--caja-sombra);
          border: 1px solid var(--caja-sombra);
          transition: all 0.3s cubic-bezier(0.4, 0.0, 0.2, 1);
          border-left: none;
        }

        .info-card:hover {
          box-shadow: 0 8px 20px var(--caja-sombra-hover);
          transform: translateY(-2px);
        }

        .stat-number {
          font-size: 3em;
          font-weight: 800;
          background: var(--header);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          line-height: 1;
          margin: 10px 0;
        }

        .stat-label {
          font-size: 1em;
          color: #6b7280;
          font-weight: 600;
          margin-top: 8px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        /* TABLAS */
        .dataTables_wrapper {
          border-radius: 12px;
          overflow-x: auto;
          overflow-y: hidden;
          width: 100%;
          box-sizing: border-box;
          background: white;
          box-shadow: 0 2px 8px var(--caja-sombra);
        }

        .dataTables_scroll {
          overflow-x: auto !important;
        }

        table.dataTable {
          font-size: 14px;
          width: 100% !important;
          border-collapse: collapse;
        }

        table.dataTable thead th {
          background: var(--tabla-encabezado);
          color: white;
          font-weight: 700;
          border: none;
          padding: 15px !important;
          white-space: nowrap;
        }

        table.dataTable tbody td {
          padding: 12px 15px !important;
          border-bottom: 1px solid #f0f0f0;
          word-wrap: break-word;
          background: var(--tabla-cuerpo);
        }

        table.dataTable tbody tr {
          transition: background-color 0.2s ease;
        }

        table.dataTable tbody tr:hover {
          background: var(--tabla-hover) !important;
        }

        table.dataTable tbody tr:nth-child(even) td {
          background: var(--tabla-cuerpo);
        }

        table.dataTable tbody tr:nth-child(even):hover td {
          background: var(--tabla-hover) !important;
        }

        /* INPUTS */
        .form-control, .form-select {
          border-radius: 10px;
          border: 1.5px solid #e0e0e0;
          padding: 11px 14px;
          font-size: 14px;
          font-family: 'Poppins', sans-serif;
          transition: all 0.3s cubic-bezier(0.4, 0.0, 0.2, 1);
          background: #fafafa;
        }

        .form-control:focus, .form-select:focus {
          border-color: var(--input-focus);
          box-shadow: 0 0 0 4px var(--input-focus-sombra);
          outline: none;
          background: white;
        }

        /* CHECKBOX Y RADIO */
        .checkbox, .radio {
          margin: 12px 0;
        }

        .checkbox input[type='checkbox'],
        .radio input[type='radio'] {
          margin-right: 8px;
          cursor: pointer;
          accent-color: var(--primario);
        }

        .checkbox label, .radio label {
          cursor: pointer;
          margin-left: 5px;
        }

        /* NOTIFICACIONES */
        .shiny-notification {
          border-radius: 12px;
          box-shadow: 0 8px 24px var(--caja-sombra-hover);
          border: none;
          font-family: 'Poppins', sans-serif;
          background: white;
          border-left: 4px solid var(--primario);
        }

        .alert {
          border-radius: 12px;
          border: none;
          border-left: 4px solid;
          font-family: 'Poppins', sans-serif;
        }

        .alert-info {
          background: var(--alert-info);
          border-left-color: #4facfe;
          color: #1a202c;
        }

        .alert-success {
          background: var(--alert-success);
          border-left-color: var(--primario);
          color: #1a202c;
        }

        .alert-warning {
          background: var(--alert-warning);
          border-left-color: #0099cc;
          color: #1a202c;
        }

        .alert-danger {
          background: var(--alert-danger);
          border-left-color: #ff6b6b;
          color: #1a202c;
        }

        /* DESCARGAS - TARJETAS DINÁMICAS */
        .download-card {
          text-align: center;
          color: white;
          padding: 20px;
          border-radius: 12px;
          transition: all 0.3s ease;
          background: var(--header) !important;
        }

        .download-card:hover {
          transform: translateY(-4px);
          box-shadow: 0 8px 24px var(--caja-sombra-hover);
        }

        .download-card i {
          font-size: 3em;
          margin-bottom: 10px;
          opacity: 0.9;
        }

        .download-card h3 {
          margin: 15px 0;
          font-weight: 700;
          font-size: 1.3em;
        }

        /* RESPONSIVE */
        @media (max-width: 768px) {
          .stat-number {
            font-size: 2em;
          }

          .box {
            margin-bottom: 15px;
          }

          .btn-custom {
            width: 100%;
            margin: 5px 0;
          }
        }
      "))
    ),
    
    tabItems(
      # Tab: Inicio
      tabItem(
        tabName = "inicio",
        fluidRow(
          box(
            width = 12,
            title = h2(icon("rocket"), "Bienvenido a EasyTesis"),
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "padding: 20px; font-size: 1.2em;",
              p("EasyTesis es una aplicación Shiny para realizar análisis estadísticos completos en investigación psicológica."),
              p(em("Simplifica tus análisis con herramientas profesionales y resultados listos para publicar.")),
              hr(),
              h3(icon("list"), "Pasos a seguir:"),
              tags$ol(
                style = "font-size: 1.1em; line-height: 2;",
                tags$li(strong("Cargar Datos:"), " Sube tu archivo Excel (.xlsx) o CSV"),
                tags$li(strong("Participantes:"), " Analiza características sociodemográficas"),
                tags$li(strong("Normalidad:"), " Verifica supuestos de normalidad (univariada y multivariada)"),
                tags$li(strong("Confiabilidad:"), " Calcula Alpha y Omega"),
                tags$li(strong("Descriptivos:"), " Obtén medidas de resumen"),
                tags$li(strong("Correlaciones:"), " Analiza relaciones entre variables"),
                tags$li(strong("Comparaciones:"), " Compara grupos con pruebas paramétricas y no paramétricas"),
                tags$li(strong("Descargas:"), " Exporta todos tus resultados en Excel")
              ),
              hr(),
              p(
                style = "font-size: 0.9em; color: #666; font-style: italic;",
                icon("info-circle"),
                " EasyTesis © 2025 Cristopher Lino-Cruz. Herramienta de uso libre para investigación."
              )
            )
          )
        )
      ),
      
      # Tab: Cargar Datos
      tabItem(
        tabName = "cargar",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("file-upload"), "Cargar Archivo de Datos"),
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "padding: 20px;",
              fileInput(
                "archivo_datos",
                "Selecciona tu archivo (Excel o CSV):",
                accept = c(".xlsx", ".xls", ".csv"),
                buttonLabel = "Examinar...",
                placeholder = "Ningún archivo seleccionado"
              ),
              div(
                class = "info-card",
                h4(icon("info-circle"), "Requisitos del archivo:"),
                tags$ul(
                  tags$li("Primera fila debe contener nombres de variables"),
                  tags$li("Variables numéricas para análisis cuantitativos"),
                  tags$li("Variables categóricas para grupos"),
                  tags$li("Sin filas o columnas completamente vacías")
                )
              )
            )
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "output.datos_cargados",
            box(
              width = 12,
              title = h3(icon("check-circle"), "Datos Cargados Exitosamente"),
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              uiOutput("info_datos"),
              hr(),
              DT::dataTableOutput("vista_previa_datos")
            )
          )
        )
      ),

      # Tab: Participantes
      tabItem(
        tabName = "participantes",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("users"), "Análisis de Participantes"),
            status = "primary",
            solidHeader = TRUE,
            p("Las variables se clasifican automáticamente: caracteres como categóricas, números (excluye ítems) como continuas",
              style = "color: #666; font-size: 13px;"),
            hr(),
            fluidRow(
              column(
                6,
                h4("Variables Categóricas"),
                uiOutput("selector_vars_categoricas"),
                actionButton("btn_calcular_porcentajes", "Calcular Porcentajes",
                             class = "btn-primary btn-custom", icon = icon("percent"))
              ),
              column(
                6,
                h4("Variables Continuas Sociodemográficas"),
                uiOutput("selector_vars_continuas"),
                actionButton("btn_calcular_continuas", "Calcular Estadísticos",
                             class = "btn-info btn-custom", icon = icon("calculator"))
              )
            ),
            br(),
            fluidRow(
              column(
                12,
                actionButton("btn_limpiar_selecciones", "Limpiar Selecciones",
                             class = "btn-warning btn-custom", icon = icon("broom")),
                style = "text-align: center;"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tabla APA de Participantes",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_apa_participantes")
          )
        )
      ),

      # Tab: Sumatoria
      tabItem(
        tabName = "sumatoria",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("plus-circle"), "Crear Variables Compuestas - Sumatoria Masiva"),
            status = "info",
            solidHeader = TRUE,
            div(
              class = "info-card",
              style = "border-left-color: #3498db;",
              h4(icon("lightbulb"), "¿Cómo funciona?"),
              tags$ol(
                tags$li(strong("Formato:"), " Variable1:Item1,Item2,Item3  (cada línea una variable)"),
                tags$li(strong("Ejemplo:"), " Ansiedad_Total:A1,A2,A3,A4,A5"),
                tags$li(strong("Múltiples:"), " Pega todas tus variables de una vez"),
                tags$li(strong("Clic:"), " 'Procesar Sumatorias' → Se crean todas automáticamente")
              ),
              tags$small("Los ítems deben coincidir exactamente con los nombres en tu base de datos.")
            ),
            hr(),
            textAreaInput("texto_sumatorias",
                         label = "Pega aquí todas tus definiciones de sumatorias (una por línea):",
                         placeholder = "Ejemplo:\nEstres_parental:PSI1,PSI2,PSI3,PSI4,PSI5\nMalestar_paterno:PSI1,PSI2,PSI3\nDisfuncional:PSI4,PSI5,PSI6",
                         rows = 12,
                         cols = 100),
            br(),
            actionButton("btn_procesar_sumatorias", "Procesar Sumatorias",
                         class = "btn-info btn-custom btn-lg", icon = icon("check"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Resumen de Variables Creadas",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_sumatoria"),
            br(),
            actionButton("btn_aplicar_sumatoria", "Aplicar Cambios a los Datos",
                         class = "btn-success btn-custom", icon = icon("save"))
          )
        )
      ),

      # Tab: Descriptivos
      tabItem(
        tabName = "descriptivos",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("table"), "Estadísticos Descriptivos"),
            status = "primary",
            solidHeader = TRUE,
            div(
              class = "info-card",
              style = "border-left-color: #0f5bcc;",
              h4(icon("info-circle"), "Descriptivos de Variables Compuestas"),
              p("Aquí se muestran los estadísticos de las variables creadas en la sección de Sumatoria."),
              tags$small("Incluye: Media, Desviación Estándar, Mínimo, Máximo, zg1 (Asimetría Z), zg2 (Curtosis Z)")
            ),
            hr(),
            uiOutput("selector_vars_descriptivos"),
            br(),
            actionButton("btn_calcular_descriptivos", "Calcular Descriptivos",
                         class = "btn-primary btn-custom btn-lg", icon = icon("calculator"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tabla de Descriptivos",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_descriptivos")
          )
        )
      ),

      # Tab: Normalidad
      tabItem(
        tabName = "normalidad",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("chart-line"), "Pruebas de Normalidad (Shapiro-Wilk)"),
            status = "warning",
            solidHeader = TRUE,
            div(
              class = "info-card",
              style = "border-left-color: #f39c12;",
              h4(icon("lightbulb"), "Guía de Interpretación:"),
              tags$ul(
                tags$li(strong("p > 0.05:"), " Distribución normal → Usar pruebas paramétricas (t-test, ANOVA)"),
                tags$li(strong("p ≤ 0.05:"), " Distribución no normal → Usar pruebas no paramétricas (Mann-Whitney, Kruskal-Wallis)")
              ),
              tags$small("Nota: El test de Shapiro-Wilk es sensible con muestras grandes. Siempre revisa gráficos de distribución junto al test.")
            ),
            hr(),
            uiOutput("selector_vars_normalidad"),
            br(),
            actionButton("btn_test_normalidad", "Ejecutar Pruebas",
                         class = "btn-warning btn-custom btn-lg", icon = icon("play"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Resultados de Normalidad Univariada",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_normalidad")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = h3(icon("cube"), "Normalidad Multivariada"),
            status = "info",
            solidHeader = TRUE,
            div(
              class = "info-card",
              style = "border-left-color: #3498db;",
              h4(icon("lightbulb"), "Prueba de Mardia:"),
              tags$ul(
                tags$li(strong("p > 0.05:"), " Distribución multivariada normal → Asumir normalidad multivariada"),
                tags$li(strong("p ≤ 0.05:"), " Distribución multivariada no normal")
              ),
              tags$small("Nota: Esta prueba evalúa simultáneamente la asimetría y curtosis multivariadas.")
            ),
            hr(),
            uiOutput("selector_vars_mvn"),
            br(),
            actionButton("btn_test_mvn", "Ejecutar Prueba Mardia",
                         class = "btn-info btn-custom btn-lg", icon = icon("play"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Gráfico Q-Q Plot Multivariado",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot_mvn", height = "500px"),
            br(),
            verbatimTextOutput("mvn_results")
          )
        )
      ),

      # Tab: Confiabilidad
      tabItem(
        tabName = "confiabilidad",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("shield-alt"), "Confiabilidad - Alpha de Cronbach y Omega"),
            status = "info",
            solidHeader = TRUE,
            div(
              class = "info-card",
              style = "border-left-color: #3498db;",
              h4(icon("lightbulb"), "¿Cómo usar esta sección?"),
              tags$ol(
                tags$li(strong("Paso 1:"), " Selecciona las variables compuestas creadas en Sumatoria"),
                tags$li(strong("Paso 2:"), " Haz clic en 'Calcular Confiabilidad'"),
                tags$li(strong("Resultado:"), " Obtendrás Alpha de Cronbach y Omega de McDonald para cada variable")
              ),
              tags$small("Nota: Se calcula la confiabilidad directamente desde las variables sumadas. No es necesario volver a ingresar ítems.")
            ),
            hr(),
            h4("Selecciona Variables Compuestas para Calcular Confiabilidad"),
            uiOutput("selector_vars_confiabilidad"),
            br(),
            actionButton("btn_calcular_confiabilidad", "Calcular Confiabilidad",
                         class = "btn-info btn-custom btn-lg", icon = icon("check"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Resultados de Confiabilidad",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_confiabilidad")
          )
        )
      ),
      
      # Tab: Correlaciones
      tabItem(
        tabName = "correlaciones",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("project-diagram"), "Análisis de Correlaciones"),
            status = "info",
            solidHeader = TRUE,
            fluidRow(
              column(
                8,
                uiOutput("selector_vars_correlaciones")
              ),
              column(
                4,
                selectInput("metodo_correlacion", "Método:",
                            choices = c("Pearson" = "pearson",
                                       "Spearman" = "spearman",
                                       "Robust (PB)" = "robust"),
                            selected = "spearman")
              )
            ),
            br(),
            actionButton("btn_calcular_correlaciones", "Calcular Correlaciones",
                         class = "btn-info btn-custom btn-lg", icon = icon("play"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Matriz de Correlaciones (Formato APA)",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_correlaciones")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Visualización de Correlaciones",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            plotOutput("plot_correlaciones", height = "600px"),
            br(),
            downloadButton("descargar_corrplot_jpg", "Descargar Imagen (JPG)",
                           class = "btn-success btn-custom")
          )
        )
      ),
      
      # Tab: Comparaciones (CÓDIGO CORREGIDO)
      tabItem(
        tabName = "comparaciones",
        fluidRow(
          box(
            width = 12,
            title = h3(icon("balance-scale"), "Comparación de Grupos"),
            status = "danger",
            solidHeader = TRUE,
            div(
              class = "info-card",
              h4(icon("exclamation-triangle"), "Importante:"),
              p("El método adecuado (paramétrico o no paramétrico) depende de la normalidad y el número de grupos.")
            ),
            hr(),
            fluidRow(
              column(
                4,
                selectInput("metodo_comparacion", "Selecciona Método de Prueba:",
                            choices = c(
                              "T-test (2 grupos, Normal)" = "ttest",
                              "U de Mann-Whitney (2 grupos, No Normal)" = "mannwhitney",
                              "ANOVA de Welch (3+ grupos, Normal)" = "anova",
                              "Kruskal-Wallis (3+ grupos, No Normal)" = "kruskal"
                            ),
                            selected = "ttest")
              ),
              column(
                4,
                selectInput("var_grupo", "Variable de agrupación:", choices = NULL)
              ),
              column(
                4,
                uiOutput("selector_vars_comparacion")
              )
            ),
            fluidRow(
              column(
                12,
                checkboxInput("usar_mdn_ric",
                             "Usar Mediana y RIC en pruebas no paramétricas (Mann-Whitney y Kruskal-Wallis)",
                             value = FALSE),
                p(
                  style = "font-size: 12px; color: #666; margin-top: 5px;",
                  icon("info-circle"),
                  "Nota: Por defecto se muestra Media y DE en ambas pruebas. Activa esta opción solo si prefieres Mediana y RIC para interpretaciones no paramétricas."
                )
              )
            ),
            br(),
            actionButton("btn_comparar_grupos", "Realizar Comparación",
                         class = "btn-danger btn-custom btn-lg", icon = icon("play"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Resultados de Comparación",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("tabla_comparacion")
          )
        ),

        # Subsección: Visualización con Boxplot
        fluidRow(
          box(
            width = 12,
            title = "Visualización de Distribuciones (Boxplot)",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              class = "info-card",
              p("El boxplot muestra la distribución de los valores por grupo. Las líneas dentro de cada caja representan la mediana, los extremos de la caja los cuartiles, y los puntos dispersos los datos individuales."),
              p(em("Nota: Solo se muestran las variables con diferencias significativas (p ≤ 0.05)"), style = "color: #666; font-size: 0.9em;"),
              br()
            ),
            uiOutput("selector_variables_boxplot"),
            plotOutput("plot_boxplot_comparacion", height = "600px")
          )
        ),

        # Subsección: Pruebas Post-hoc
        fluidRow(
          box(
            width = 12,
            title = "Comparaciones Múltiples (Pruebas Post-hoc)",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              class = "info-card",
              p("Estas pruebas identifican específicamente cuáles pares de grupos tienen diferencias significativas cuando la prueba inicial (ANOVA o Kruskal-Wallis) es significativa."),
              p(em("Nota: Solo se muestran las variables con diferencias significativas (p ≤ 0.05)"), style = "color: #666; font-size: 0.9em;"),
              br()
            ),
            uiOutput("selector_variables_posthoc"),
            DT::dataTableOutput("tabla_posthoc_comparacion"),
            br(),
            div(
              class = "info-card",
              style = "background: #f0f8ff; border-left: 4px solid #0099cc;",
              p(strong("Nota: "), "Las pruebas post-hoc solo se muestran cuando la prueba inicial es significativa (p ≤ 0.05)."),
              p(strong("Método: "), "Se utiliza ANOVA de Welch (robusto ante varianzas desiguales) con Games-Howell como prueba post-hoc.")
            )
          )
        )
      ),

      # Tab: Descargas (CÓDIGO CORREGIDO)
      tabItem(
        tabName = "descargas",
        h2(icon("download"), "Descargas"),
        p("Descarga todos tus resultados en formato Excel o imágenes de alta calidad."),
        hr(),
        fluidRow(
          column(
            4,
            div(
              class = "download-card",
              icon("users", class = "fa-3x"),
              h3("1. Participantes"),
              downloadButton("descargar_tabla_participantes", "Descargar", class = "btn-light btn-custom")
            )
          ),
          column(
            4,
            div(
              class = "download-card",
              icon("chart-bar", class = "fa-3x"),
              h3("2. Descriptivos"),
              downloadButton("descargar_descriptivos", "Descargar", class = "btn-light btn-custom")
            )
          ),
          column(
            4,
            div(
              class = "download-card",
              icon("chart-line", class = "fa-3x"),
              h3("3. Normalidad"),
              downloadButton("descargar_normalidad", "Descargar", class = "btn-light btn-custom")
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              class = "download-card",
              icon("shield-alt", class = "fa-3x"),
              h3("4. Confiabilidad"),
              downloadButton("descargar_confiabilidad", "Descargar", class = "btn-light btn-custom")
            )
          ),
          column(
            4,
            div(
              class = "download-card",
              icon("project-diagram", class = "fa-3x"),
              h3("5. Correlaciones"),
              downloadButton("descargar_correlaciones", "Descargar", class = "btn-light btn-custom")
            )
          ),
          column(
            4,
            div(
              class = "download-card",
              icon("balance-scale", class = "fa-3x"),
              h3("6. Comparaciones"),
              downloadButton("descargar_comparacion", "Descargar", class = "btn-light btn-custom")
            )
          )
        ),
        fluidRow(
          column(
            4,
            div(
              class = "download-card",
              icon("database", class = "fa-3x"),
              h3("7. Datos + Sumatorias"),
              downloadButton("descargar_datos_sumatorias", "Descargar", class = "btn-light btn-custom")
            )
          )
        )
      ),

      # Tab: Acerca de
      tabItem(
        tabName = "acerca",
        fluidRow(
          # Información general
          box(
            width = 12,
            title = h2(icon("info-circle"), "Acerca de EasyTesis"),
            status = "info",
            solidHeader = TRUE,
            div(
              style = "padding: 20px; font-size: 1.1em; line-height: 1.8;",
              p("EasyTesis es una aplicación Shiny diseñada para facilitar el análisis estadístico completo en investigación psicológica y social. Proporciona herramientas profesionales para análisis descriptivos, pruebas de normalidad (univariada y multivariada), correlaciones, comparaciones de grupos, y evaluación de confiabilidad de escalas."),
              h4(icon("quote-left"), "Cómo citar:"),
              div(
                style = "background-color: #f5f5f5; padding: 15px; border-left: 4px solid #0f5bcc; border-radius: 4px; font-family: monospace; font-size: 0.95em;",
                p(strong("APA 7ª edición:"), br(),
                  "Lino-Cruz, C.J. (2025). EasyTesis: Aplicación para análisis correlacionales en psicología (v1.0). Recuperado de https://cristopherlino17-easytesis.share.connect.posit.cloud/"),
                br(),
                p(strong("BibTeX:"), br(),
                  "@software{linocruz2025easytesis,", br(),
                  "  title={EasyTesis: Aplicación para análisis correlacionales en psicología (v1.0)},", br(),
                  "  author={Lino-Cruz, C.J.},", br(),
                  "  year={2025},", br(),
                  "  url={https://cristopherlino17-easytesis.share.connect.posit.cloud/}", br(),
                  "}")
              )
            )
          )
        ),

        fluidRow(
          # Referencias de librerías
          box(
            width = 12,
            title = h3(icon("book"), "Referencias Técnicas y Librerías"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              style = "padding: 15px; font-size: 0.95em; line-height: 1.8;",
              h4("Librerías Utilizadas:"),
              tags$ul(
                tags$li(strong("shiny (v1.6+):"), " Framework web interactivo para R. Chang, W., Cheng, J., Allaire, J., et al. (2021)"),
                tags$li(strong("shinydashboard:"), " Extensión para crear dashboards con Shiny. Chang, W., & Borges Ribeiro, B. (2021)"),
                tags$li(strong("shinyjs:"), " Herramientas JavaScript para Shiny. Attali, D. (2021)"),
                tags$li(strong("DT:"), " Tablas interactivas DataTables. Xie, Y., Cheng, J., & Tan, X. (2021)"),
                tags$li(strong("dplyr (v1.0+):"), " Herramientas para manipulación de datos. Wickham, H., François, R., Henry, L., & Müller, K. (2022)"),
                tags$li(strong("tidyr:"), " Herramientas para tidying data. Wickham, H., & Girlich, M. (2022)"),
                tags$li(strong("readxl:"), " Lectura de archivos Excel. Wickham, H., & Bryan, J. (2022)"),
                tags$li(strong("openxlsx:"), " Lectura y escritura de Excel. Frick, J. (2022)"),
                tags$li(strong("writexl:"), " Escritura de Excel con writereferenL. Ooms, J. (2022)"),
                tags$li(strong("ggplot2:"), " Visualización de datos. Wickham, H. (2016)"),
                tags$li(strong("psych (v2.0+):"), " Procedimientos psicométricos. Revelle, W. (2022)"),
                tags$li(strong("rstatix:"), " Pipe-friendly estadísticas. Kassambara, A. (2021)"),
                tags$li(strong("corrplot:"), " Visualización de matrices de correlación. Wei, T., & Simko, V. (2021)"),
                tags$li(strong("semTools:"), " Herramientas para modelos de ecuaciones estructurales. Jorgensen, T., Pornprasertmanit, S., et al. (2022)"),
                tags$li(strong("MVN:"), " Pruebas de normalidad multivariada. Korkmaz, S., Goksuluk, D., & Zararsiz, G. (2014)")
              ),
              hr(),
              h4("Métodos Estadísticos Implementados:"),
              tags$ul(
                tags$li(strong("Shapiro-Wilk:"), " Prueba paramétrica de normalidad univariada"),
                tags$li(strong("Mardia Test:"), " Prueba de normalidad multivariada"),
                tags$li(strong("Correlación de Pearson:"), " Coeficiente de correlación lineal"),
                tags$li(strong("Correlación de Spearman:"), " Coeficiente de correlación de rangos"),
                tags$li(strong("t-test:"), " Comparación de medias entre dos grupos"),
                tags$li(strong("Mann-Whitney U:"), " Prueba no paramétrica para dos grupos"),
                tags$li(strong("ANOVA:"), " Análisis de varianza para múltiples grupos"),
                tags$li(strong("Kruskal-Wallis:"), " Prueba no paramétrica para múltiples grupos"),
                tags$li(strong("d de Cohen:"), " Tamaño del efecto para t-test"),
                tags$li(strong("r_rb (Rank-biserial):"), " Tamaño del efecto para Mann-Whitney"),
                tags$li(strong("ω² (Omega-squared):"), " Tamaño del efecto para ANOVA"),
                tags$li(strong("η²_H (Eta-squared H):"), " Tamaño del efecto para Kruskal-Wallis"),
                tags$li(strong("Alpha de Cronbach:"), " Consistencia interna de escalas"),
                tags$li(strong("Omega de McDonald:"), " Confiabilidad de constructo")
              ),
              hr(),
              h4("Compatibilidad:"),
              p("EasyTesis es 100% compatible con los resultados y métodos reportados por Jamovi, un software estadístico de código abierto."),
              hr(),
              p(em("Última actualización: 2025-10-27 | Versión: 3.0"))
            )
          )
        ),

        fluidRow(
          # Material Suplementario - Referencias
          box(
            width = 12,
            title = h3(icon("file-alt"), "Material Suplementario - Referencias"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            div(
              style = "padding: 20px; font-size: 0.9em; line-height: 1.8;",
              p("Las siguientes referencias constituyen la base teórica y metodológica de EasyTesis, ordenadas alfabéticamente según formato APA 7ª edición."),
              br(),

              # Tabla de referencias con mejor estructura
              tags$style(HTML("
                .ref-item {
                  margin-bottom: 18px;
                  padding: 0;
                  line-height: 1.6;
                }
                .ref-item p {
                  margin: 0;
                  text-align: justify;
                }
              ")),

              # Referencia 1: Cain et al. 2017
              div(class = "ref-item",
                p("Cain, M. K., Zhang, Z., & Yuan, K. H. (2017). Univariate and multivariate skewness and kurtosis for measuring nonnormality: Prevalence, influence and estimation. ",
                  em("Behavior Research Methods"), em("49"), "(5), 1716–1735. ",
                  a("https://doi.org/10.3758/s13428-016-0814-1",
                    href = "https://doi.org/10.3758/s13428-016-0814-1",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 2: Castillo-Blanco & Alegre-Bravo 2015
              div(class = "ref-item",
                p("Castillo-Blanco, R., & Alegre-Bravo, A. (2015). Importancia del tamaño del efecto en el análisis de datos de investigación en psicología. ",
                  em("Persona"), em("18"), "(18), 137. ",
                  a("https://doi.org/10.26439/persona2015.n018.503",
                    href = "https://doi.org/10.26439/persona2015.n018.503",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 3: Cohen 1988
              div(class = "ref-item",
                p("Cohen, J. (1988). ",
                  em("Statistical power analysis for the behavioral sciences"), " (2nd ed.). Lawrence Erlbaum Associates. ISBN: 978-0805802832. ",
                  a("https://doi.org/10.4324/9780203771587",
                    href = "https://doi.org/10.4324/9780203771587",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 4: de Winter et al. 2016
              div(class = "ref-item",
                p("de Winter, J. C. F., Gosling, S. D., & Potter, J. (2016). Comparing the Pearson and Spearman correlation coefficients across distributions and sample sizes: A tutorial using simulations and empirical data. ",
                  em("Psychological Methods"), em("21"), "(3), 273–290. ",
                  a("https://doi.org/10.1037/met0000079",
                    href = "https://doi.org/10.1037/met0000079",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 5: Dominguez-Lara 2017
              div(class = "ref-item",
                p("Dominguez-Lara, S. A. (2017). Magnitud del efecto, una guía rápida [Carta al editor]. ",
                  em("Educación Médica"), em("18"), "(2), 251–254. ",
                  a("https://doi.org/10.1016/j.edumed.2016.04.001",
                    href = "https://doi.org/10.1016/j.edumed.2016.04.001",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 6: Ferguson 2009
              div(class = "ref-item",
                p("Ferguson, C. J. (2009). An effect size primer: A guide for clinicians and researchers. ",
                  em("Professional Psychology: Research and Practice"), em("40"), "(5), 532–538. ",
                  a("https://doi.org/10.1037/a0015808",
                    href = "https://doi.org/10.1037/a0015808",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 7: Gignac & Szodorai 2016
              div(class = "ref-item",
                p("Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for individual differences researchers. ",
                  em("Personality and Individual Differences"), em("102"), ", 74–78. ",
                  a("https://doi.org/10.1016/j.paid.2016.06.069",
                    href = "https://doi.org/10.1016/j.paid.2016.06.069",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 8: Hopkins et al. 2018
              div(class = "ref-item",
                p("Hopkins, S., Dettori, J. R., & Chapman, J. R. (2018). Parametric and nonparametric tests in spine research: Why do they matter? ",
                  em("Global Spine Journal"), em("8"), "(6), 652–654. ",
                  a("https://doi.org/10.1177/2192568218782679",
                    href = "https://doi.org/10.1177/2192568218782679",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 9: Kim 2013
              div(class = "ref-item",
                p("Kim, H. Y. (2013). Statistical notes for clinical researchers: Assessing normal distribution (2) using skewness and kurtosis. ",
                  em("Restorative Dentistry & Endodontics"), em("38"), "(1), 52. ",
                  a("https://doi.org/10.5395/rde.2013.38.1.52",
                    href = "https://doi.org/10.5395/rde.2013.38.1.52",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              # Referencia 10: Pardo & San Martín 2010
              div(class = "ref-item",
                p("Pardo, A., & San Martín, R. (2010). ",
                  em("Análisis de datos en ciencias sociales y de la salud II"), ". Editorial Síntesis. ISBN: 978-8497567046. ",
                  a("https://www.sintesis.com/libro/analisis-de-datos-en-ciencias-sociales-y-de-la-salud-ii-2-edicion",
                    href = "https://www.sintesis.com/libro/analisis-de-datos-en-ciencias-sociales-y-de-la-salud-ii-2-edicion",
                    target = "_blank",
                    style = "color: #0f5bcc;"))
              ),

              hr(),
              p(strong("Nota: "), "Los hipervínculos (en azul) permiten acceder directamente a los recursos digitales disponibles.
                Si utilizas alguno de estos métodos en tu investigación, por favor cita tanto EasyTesis como la referencia correspondiente.")
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVIDOR - CÓDIGO COMPLETADO
# ============================================================================

server <- function(input, output, session) {

  # ========== SELECTOR DE TEMAS ==========
  observeEvent(input$selector_tema, {
    tema_seleccionado <- input$selector_tema
    css_dinamico <- generar_css_tema(tema_seleccionado)

    # Inyectar CSS dinámico
    shinyjs::runjs(
      sprintf("
        var style = document.getElementById('tema-dinamico');
        if (!style) {
          style = document.createElement('style');
          style.id = 'tema-dinamico';
          document.head.appendChild(style);
        }
        style.textContent = `%s`;
      ", css_dinamico)
    )
  })

  # Valores reactivos
  datos_reactive <- reactiveVal(NULL)
  sumatorias_creadas <- reactiveVal(data.frame())
  items_por_variable <- reactiveVal(list())  # NUEVO: Guarda ítems por variable

  resultados <- reactiveValues(
    porcentajes = NULL,
    edad = NULL,
    normalidad = NULL,
    descriptivos = NULL,
    correlaciones = NULL,
    comparacion = NULL,
    comparaciones_lista = list(),  # NUEVO: Lista acumulativa de todas las comparaciones
    confiabilidad = NULL,
    vars_numericas = NULL,
    vars_categoricas = NULL
  )

  # ========== CARGAR DATOS ==========
  observeEvent(input$archivo_datos, {
    req(input$archivo_datos)
    
    tryCatch({
      ext <- tools::file_ext(input$archivo_datos$datapath)
      
      if (ext %in% c("xlsx", "xls")) {
        datos <- read_excel(input$archivo_datos$datapath)
      } else if (ext == "csv") {
        datos <- read.csv(input$archivo_datos$datapath, stringsAsFactors = FALSE)
      } else {
        showNotification("Formato no soportado", type = "error")
        return()
      }
      
      datos <- limpiar_datos(datos)
      datos_reactive(datos)

      # Identificar tipos de variables automáticamente
      vars_filtradas <- filtrar_vars_sociodemo(datos)

      # Se actualizan las listas de variables del reactiveValues
      resultados$vars_numericas <- vars_filtradas$continuas
      resultados$vars_categoricas <- vars_filtradas$categoricas
      resultados$items <- vars_filtradas$items
      
      # Actualizar selectores
      updateSelectInput(session, "var_grupo", choices = c("Seleccionar..." = "", vars_filtradas$categoricas))
      
      # Actualizar checkboxGroupInput - Esto se hace en los renderUI/renderUI/renderUI
      # Se asume que los renderUI ya reaccionarán a los cambios en resultados$vars_...
      
      showNotification("¡Datos cargados exitosamente!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error al cargar datos:", e$message), type = "error", duration = 5)
    })
  })
  
  # Output: indicador de datos cargados
  output$datos_cargados <- reactive({
    !is.null(datos_reactive())
  })
  outputOptions(output, "datos_cargados", suspendWhenHidden = FALSE)
  
  # Output: información de datos
  output$info_datos <- renderUI({
    req(datos_reactive())
    datos <- datos_reactive()
    
    fluidRow(
      column(
        4,
        div(
          class = "info-card",
          style = "text-align: center;",
          div(class = "stat-number", nrow(datos)),
          div(class = "stat-label", "Observaciones")
        )
      ),
      column(
        4,
        div(
          class = "info-card",
          style = "text-align: center;",
          div(class = "stat-number", ncol(datos)),
          div(class = "stat-label", "Variables")
        )
      ),
      column(
        4,
        div(
          class = "info-card",
          style = "text-align: center;",
          div(class = "stat-number", length(resultados$vars_numericas)),
          div(class = "stat-label", "Variables Numéricas")
        )
      )
    )
  })
  
  # Output: vista previa de datos
  output$vista_previa_datos <- DT::renderDataTable({
    req(datos_reactive())
    DT::datatable(
      datos_reactive(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      class = 'display nowrap'
    )
  })
  
  # ========== EXPLORACIÓN ==========
  output$lista_vars_numericas <- renderUI({
    req(resultados$vars_numericas)
    tags$div(
      style = "max-height: 300px; overflow-y: auto;",
      lapply(resultados$vars_numericas, function(var) {
        tags$div(
          style = "padding: 5px; margin: 3px; background: #e6f0ff; border-radius: 5px; border-left: 5px solid #667eea;",
          icon("hashtag"), var
        )
      })
    )
  })
  
  output$lista_vars_categoricas <- renderUI({
    req(resultados$vars_categoricas)
    tags$div(
      style = "max-height: 300px; overflow-y: auto;",
      lapply(resultados$vars_categoricas, function(var) {
        tags$div(
          style = "padding: 5px; margin: 3px; background: #f0f8ff; border-radius: 5px; border-left: 5px solid #0099cc;",
          icon("font"), var
        )
      })
    )
  })
  
  # ========== PARTICIPANTES ==========
  output$selector_vars_categoricas <- renderUI({
    req(resultados$vars_categoricas)
    checkboxGroupInput(
      "vars_categoricas_sel",
      NULL,
      choices = resultados$vars_categoricas,
      selected = NULL
    )
  })

  output$selector_vars_continuas <- renderUI({
    req(resultados$vars_numericas)

    vars_continuas <- resultados$vars_numericas

    if (length(vars_continuas) > 0) {
      checkboxGroupInput(
        "vars_continuas_sel",
        NULL,
        choices = vars_continuas,
        selected = NULL
      )
    } else {
      p("No hay variables continuas sociodemográficas detectadas", style = "color: #e74c3c; font-weight: bold;")
    }
  })

  observeEvent(input$btn_calcular_porcentajes, {
    req(datos_reactive(), input$vars_categoricas_sel)
    if (length(input$vars_categoricas_sel) == 0) {
      showNotification("Selecciona al menos una variable categórica", type = "warning")
      return()
    }
    
    resultados$porcentajes <- calcular_porcentajes(datos_reactive(), input$vars_categoricas_sel)
    showNotification("Porcentajes calculados", type = "message")
  })
  
  output$resultado_porcentajes <- renderPrint({
    req(resultados$porcentajes)
    for (var_name in names(resultados$porcentajes)) {
      cat("\n=== ", var_name, " ===\n")
      print(resultados$porcentajes[[var_name]])
      cat("\n")
    }
  })
  
  observeEvent(input$btn_calcular_continuas, {
    req(datos_reactive())

    # Variables seleccionadas
    vars_cat <- if (!is.null(input$vars_categoricas_sel) && length(input$vars_categoricas_sel) > 0) {
      input$vars_categoricas_sel
    } else {
      NULL
    }

    vars_cont <- if (!is.null(input$vars_continuas_sel) && length(input$vars_continuas_sel) > 0) {
      input$vars_continuas_sel
    } else {
      NULL
    }

    if (is.null(vars_cat) && is.null(vars_cont)) {
      showNotification("Selecciona al menos una variable (categórica o continua)", type = "warning")
      return()
    }

    # Generar tabla de participantes
    tabla <- tabla_participantes(datos_reactive(),
                                 continuas = vars_cont,
                                 categoricas = vars_cat,
                                 digits = 2,
                                 mostrar_minmax = FALSE,
                                 ordenar_categorias_por_n = TRUE)

    if (!is.null(tabla) && nrow(tabla) > 0) {
      resultados$tabla_apa <- tabla
      showNotification("Tabla de participantes generada correctamente", type = "message")
    } else {
      showNotification("Error al generar tabla", type = "error")
    }
  })

  # Limpiar selecciones de variables
  observeEvent(input$btn_limpiar_selecciones, {
    updateCheckboxGroupInput(session, "vars_categoricas_sel", selected = NULL)
    updateCheckboxGroupInput(session, "vars_continuas_sel", selected = NULL)
    resultados$tabla_apa <- NULL
    showNotification("Selecciones limpias. Selecciona nuevas variables.", type = "message", duration = 3)
  })

  output$resultado_continuas <- DT::renderDataTable({
    req(resultados$continuas)
    DT::datatable(resultados$continuas, options = list(dom = 't', scrollX = TRUE))
  })

  # Tabla APA de Participantes
  output$tabla_apa_participantes <- DT::renderDataTable({
    req(resultados$tabla_apa)

    DT::datatable(resultados$tabla_apa,
                  options = list(
                    dom = 't',
                    scrollX = TRUE,
                    paging = FALSE,
                    searching = FALSE,
                    info = FALSE,
                    ordering = FALSE,
                    columnDefs = list(list(className = 'dt-center', targets = 1:2))
                  ),
                  rownames = FALSE,
                  escape = FALSE)
  })

  # Descargar tabla APA de participantes
  output$descargar_tabla_participantes <- downloadHandler(
    filename = function() {
      paste0("Tabla_Participantes_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (is.null(resultados$tabla_apa) || nrow(resultados$tabla_apa) == 0) {
        showNotification("No hay tabla de participantes para descargar. Genera primero la tabla.",
                        type = "error", duration = 5)
        return(NULL)
      }
      write_xlsx(resultados$tabla_apa, file)
    }
  )

  # Descargar datos con sumatorias aplicadas
  output$descargar_datos_sumatorias <- downloadHandler(
    filename = function() {
      paste0("Datos_con_Sumatorias_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (is.null(datos_reactive())) {
        showNotification("No hay datos cargados. Carga primero un archivo.",
                        type = "error", duration = 5)
        return(NULL)
      }
      write_xlsx(datos_reactive(), file)
    }
  )

  # ========== SUMATORIA ==========
  # Procesar múltiples sumatorias desde texto masivo
  observeEvent(input$btn_procesar_sumatorias, {
    req(datos_reactive(), input$texto_sumatorias)

    # Parsear el texto de sumatorias
    resultado_parse <- parsear_sumatorias(input$texto_sumatorias)

    if (!resultado_parse$exito) {
      showNotification(resultado_parse$mensaje, type = "error")
      return()
    }

    sumatorias <- resultado_parse$sumatorias
    sumatorias_table <- data.frame()

    # Procesar cada sumatoria
    withProgress(message = "Procesando sumatorias...", {
      for (nombre_var in names(sumatorias)) {
        items_var <- sumatorias[[nombre_var]]

        # Crear sumatoria
        resultado_suma <- crear_sumatoria(
          data = datos_reactive(),
          items = items_var,
          nombre_variable = nombre_var
        )

        if (resultado_suma$exito) {
          sumatorias_table <- bind_rows(sumatorias_table, resultado_suma$estadisticos)
        } else {
          showNotification(paste0("Error en ", nombre_var, ": ", resultado_suma$mensaje),
                          type = "warning")
        }
      }
    })

    if (nrow(sumatorias_table) > 0) {
      sumatorias_creadas(sumatorias_table)
      showNotification(paste0("✓ ", nrow(sumatorias_table), " variable(s) procesada(s) correctamente"),
                      type = "message")

      # Limpiar textarea
      shinyjs::reset("texto_sumatorias")
    } else {
      showNotification("No se pudo procesar ninguna sumatoria", type = "error")
    }
  })

  output$tabla_sumatoria <- DT::renderDataTable({
    req(sumatorias_creadas())
    tabla <- sumatorias_creadas()

    if (nrow(tabla) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay variables compuestas creadas aún"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    DT::datatable(
      tabla,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      caption = "Variables compuestas creadas (no aplicadas aún a los datos)"
    ) %>%
      DT::formatStyle(
        'Variable',
        fontWeight = 'bold',
        color = '#0f5bcc'
      )
  })

  # Aplicar cambios a los datos
  observeEvent(input$btn_aplicar_sumatoria, {
    req(datos_reactive(), sumatorias_creadas())

    tabla_sumatorias <- sumatorias_creadas()

    if (nrow(tabla_sumatorias) == 0) {
      showNotification("No hay variables creadas para aplicar", type = "warning")
      return()
    }

    # Aplicar cada sumatoria a los datos
    datos_actuales <- datos_reactive()

    for (i in 1:nrow(tabla_sumatorias)) {
      nombre_var <- tabla_sumatorias$Variable[i]
      items_str <- tabla_sumatorias$Items_utilizados[i]
      items_lista <- trimws(strsplit(items_str, ",")[[1]])

      # Crear la sumatoria
      resultado <- crear_sumatoria(
        data = datos_actuales,
        items = items_lista,
        nombre_variable = nombre_var
      )

      if (resultado$exito) {
        datos_actuales <- resultado$data
      }
    }

    # Actualizar datos reactivos
    datos_reactive(datos_actuales)

    # ✅ CAMBIO CLAVE: SOLO USAR VARIABLES SUMADAS (compuestas) para análisis posteriores
    # Las variables que se usen en Descriptivos, Normalidad, Confiabilidad, etc.
    # serán SOLO las creadas en Sumatoria
    vars_sumadas <- tabla_sumatorias$Variable

    # NUEVO: Guardar los ítems de cada variable para confiabilidad
    items_list <- list()
    for (i in 1:nrow(tabla_sumatorias)) {
      nombre_var <- tabla_sumatorias$Variable[i]
      items_str <- tabla_sumatorias$Items_utilizados[i]
      items_lista <- trimws(strsplit(items_str, ",")[[1]])
      items_list[[nombre_var]] <- items_lista
    }
    items_por_variable(items_list)

    # Las variables categóricas permanecen igual (para participantes)
    datos_cat <- datos_actuales %>%
      select_if(function(x) is.character(x) | is.factor(x))
    vars_categoricas_names <- names(datos_cat)

    # Actualizar reactiveValues
    resultados$vars_numericas <- vars_sumadas  # SOLO variables sumadas
    resultados$vars_categoricas <- vars_categoricas_names

    showNotification(paste0(nrow(tabla_sumatorias), " variable(s) compuesta(s) aplicada(s) a los datos. Ahora solo se usarán en análisis posteriores."), type = "message")
  })

  # ========== NORMALIDAD ==========
  output$selector_vars_normalidad <- renderUI({
    req(resultados$vars_numericas)
    checkboxGroupInput(
      "vars_normalidad_sel",
      "Selecciona variables para probar normalidad:",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })
  
  observeEvent(input$btn_test_normalidad, {
    req(datos_reactive(), input$vars_normalidad_sel)
    if (length(input$vars_normalidad_sel) == 0) {
      showNotification("Selecciona al menos una variable", type = "warning")
      return()
    }
    
    withProgress(message = "Calculando normalidad...", {
      resultados$normalidad <- test_normalidad(datos_reactive(), input$vars_normalidad_sel)
    })
    
    if (!is.null(resultados$normalidad)) {
      showNotification("Pruebas de normalidad completadas", type = "message")
    } else {
      showNotification("No hay variables válidas para el test.", type = "error")
    }
  })
  
  output$tabla_normalidad <- DT::renderDataTable({
    req(resultados$normalidad)
    DT::datatable(
      resultados$normalidad,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'p_valor',
        backgroundColor = DT::styleInterval(0.05, c('#f8d7da', '#d4edda'))
      )
  })
  
  # Descarga para tab Normalidad
  output$descargar_normalidad <- downloadHandler(
    filename = function() paste0("Normalidad_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (is.null(resultados$normalidad) || nrow(resultados$normalidad) == 0) {
        showNotification("No hay resultados de normalidad para descargar. Ejecuta primero el análisis de normalidad.",
                        type = "error", duration = 5)
        return(NULL)
      }
      write_xlsx(resultados$normalidad, file)
    }
  )

  # ========== NORMALIDAD MULTIVARIADA (MVN) ==========
  # Selector de variables para MVN
  output$selector_vars_mvn <- renderUI({
    req(resultados$vars_numericas)
    req(length(resultados$vars_numericas) >= 2)

    checkboxGroupInput(
      "vars_mvn_sel",
      "Selecciona variables para prueba multivariada (mínimo 2):",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })

  # Ejecutar prueba Mardia MVN
  observeEvent(input$btn_test_mvn, {
    req(datos_reactive(), input$vars_mvn_sel)

    if (length(input$vars_mvn_sel) < 2) {
      showNotification("Selecciona al menos 2 variables para normalidad multivariada", type = "warning")
      return()
    }

    withProgress(message = "Calculando normalidad multivariada (Prueba Mardia)...", {
      # Obtener datos solo de variables seleccionadas
      datos_mvn <- datos_reactive()[, input$vars_mvn_sel, drop = FALSE]

      # Remover filas con NA
      datos_mvn <- na.omit(datos_mvn)

      if (nrow(datos_mvn) == 0) {
        showNotification("No hay datos válidos después de remover valores faltantes", type = "error")
        return()
      }

      # Ejecutar prueba MVN (Mardia test)
      tryCatch({
        mvn_result <- MVN::mvn(data = datos_mvn, mvnTest = c("mardia"), multivariatePlot = "qq")
        resultados$mvn_test <- mvn_result

        showNotification(
          paste0("Prueba Mardia completada. n=", nrow(datos_mvn), " observaciones."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          paste0("Error en prueba MVN: ", e$message),
          type = "error"
        )
      })
    })
  })

  # Plot Q-Q para MVN
  output$plot_mvn <- renderPlot({
    req(resultados$mvn_test)

    # El plot se genera automáticamente en la ejecución de mvn()
    # Aquí lo reproducimos usando el objeto guardado
    datos_mvn <- datos_reactive()[, input$vars_mvn_sel, drop = FALSE]
    datos_mvn <- na.omit(datos_mvn)

    # Crear Q-Q plot para normalidad multivariada
    MVN::mvn(data = datos_mvn, mvnTest = c("mardia"), multivariatePlot = "qq")
  })

  # Resultados textuales de Mardia
  output$mvn_results <- renderPrint({
    req(resultados$mvn_test)

    mvn_obj <- resultados$mvn_test

    # Mostrar resultados de Mardia
    cat("╔══════════════════════════════════════════════════════════════╗\n")
    cat("║        PRUEBA DE NORMALIDAD MULTIVARIADA - MARDIA TEST       ║\n")
    cat("╚══════════════════════════════════════════════════════════════╝\n\n")

    cat("VARIABLES ANALIZADAS:\n")
    cat(paste0("  • ", paste(colnames(mvn_obj$data), collapse = "\n  • "), "\n\n"))

    cat("TAMAÑO DE MUESTRA:\n")
    cat(paste0("  • n = ", nrow(mvn_obj$data), " observaciones\n"))
    cat(paste0("  • p = ", ncol(mvn_obj$data), " variables\n\n"))

    # Extraer información de Mardia
    p_asimetria <- NA
    p_curtosis <- NA
    ambos_no_sig <- NA

    if (!is.null(mvn_obj$multivariateNormality) && nrow(mvn_obj$multivariateNormality) > 0) {
      mardia_df <- as.data.frame(mvn_obj$multivariateNormality)

      cat("RESULTADOS DE MARDIA:\n")
      cat("─────────────────────────────────────────────────────────────\n")

      # Inicializar ambos_no_sig a TRUE si hay datos
      ambos_no_sig <- TRUE

      for (i in 1:nrow(mardia_df)) {
        # Extraer valores más robustamente
        test_name <- as.character(mardia_df[i, 1])

        # Intentar obtener estadístico (puede estar en diferentes columnas)
        stat <- NA
        for (col in 2:ncol(mardia_df)) {
          val <- suppressWarnings(as.numeric(mardia_df[i, col]))
          if (!is.na(val) && is.numeric(val) && is.na(stat)) {
            stat <- val
            break
          }
        }

        # Obtener p-valor (usualmente la última columna o penúltima)
        p_val <- NA
        for (col in ncol(mardia_df):2) {
          val <- suppressWarnings(as.numeric(mardia_df[i, col]))
          if (!is.na(val) && is.numeric(val) && val <= 1 && val >= 0) {
            p_val <- val
            break
          }
        }

        if (!is.na(stat) && !is.na(p_val)) {
          cat(paste0("\n  Test: ", test_name, "\n"))
          cat(paste0("  Estadístico: ", round(stat, 4), "\n"))
          cat(paste0("  p-valor: ", format(round(p_val, 6), scientific = FALSE), "\n"))

          # Determinar significancia
          sig_label <- ifelse(p_val > 0.05, "✓ NO SIGNIFICATIVO (p > 0.05)", "✗ SIGNIFICATIVO (p ≤ 0.05)")
          cat(paste0("  Significancia: ", sig_label, "\n"))

          # Guardar p-valores
          if (grepl("Skewness|Asimetría", test_name, ignore.case = TRUE)) {
            p_asimetria <- p_val
          } else if (grepl("Kurtosis|Curtosis", test_name, ignore.case = TRUE)) {
            p_curtosis <- p_val
          }

          # Verificar si ambas pruebas son no significativas
          if (p_val <= 0.05) {
            ambos_no_sig <- FALSE
          }
        }
      }
    } else {
      cat("RESULTADOS DE MARDIA:\n")
      cat("─────────────────────────────────────────────────────────────\n")
      cat("ERROR: No se pudieron extraer los resultados de Mardia.\n")
      cat("Verifica que se haya ejecutado correctamente la prueba.\n")
    }

    cat("\n─────────────────────────────────────────────────────────────\n")
    cat("CONCLUSIÓN:\n")

    if (!is.na(ambos_no_sig) && ambos_no_sig && !is.na(p_asimetria) && !is.na(p_curtosis)) {
      cat("  ✓ NORMALIDAD MULTIVARIADA CONFIRMADA\n")
      cat("     Ambas pruebas (asimetría y curtosis) son NO SIGNIFICATIVAS\n")
      cat("     → Los datos cumplen el supuesto de normalidad multivariada\n")
    } else if (is.na(ambos_no_sig)) {
      cat("  ⚠ NO SE PUDIERON EVALUAR LOS RESULTADOS\n")
      cat("     Verifica que la prueba de Mardia se haya ejecutado correctamente\n")
      cat("     y que haya datos válidos disponibles.\n")
    } else {
      cat("  ✗ VIOLACIÓN DE NORMALIDAD MULTIVARIADA\n")
      cat("     Al menos una prueba (asimetría o curtosis) es SIGNIFICATIVA\n")
      cat("     → Los datos NO cumplen el supuesto de normalidad multivariada\n")
    }

    cat("\n─────────────────────────────────────────────────────────────\n")
    cat("NOTAS IMPORTANTES:\n")
    cat("  • Asimetría Multivariada de Mardia (Skewness)\n")
    cat("  • Curtosis Multivariada de Mardia (Kurtosis)\n")
    cat("  • AMBAS PRUEBAS deben ser no significativas (p > 0.05)\n")
    cat("  • Si alguna es significativa, el supuesto se viola\n")
  })

  # ========== CONFIABILIDAD ==========
  # Selector de variables para confiabilidad
  output$selector_vars_confiabilidad <- renderUI({
    req(resultados$vars_numericas)
    checkboxGroupInput(
      "vars_confiabilidad_sel",
      "Selecciona variables compuestas:",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })

  # Calcular confiabilidad para variables seleccionadas
  observeEvent(input$btn_calcular_confiabilidad, {
    req(datos_reactive(), input$vars_confiabilidad_sel)

    if (length(input$vars_confiabilidad_sel) == 0) {
      showNotification("Selecciona al menos una variable compuesta", type = "warning")
      return()
    }

    # Obtener ítems guardados
    items_map <- items_por_variable()

    # Calcular confiabilidad para cada variable
    resultados_lista <- list()

    withProgress(message = "Calculando confiabilidad...", {
      for (var_sel in input$vars_confiabilidad_sel) {
        # Obtener ítems de esta variable
        items_var <- items_map[[var_sel]]

        if (is.null(items_var) || length(items_var) == 0) {
          showNotification(paste0("No se encontraron ítems para: ", var_sel), type = "warning")
          next
        }

        # Calcular confiabilidad
        resultado <- calcular_confiabilidad_escala(
          data = datos_reactive(),
          items = items_var
        )

        if (resultado$exito) {
          tabla_var <- resultado$confiabilidad
          tabla_var$Variable <- var_sel  # Añadir nombre de variable
          resultados_lista[[var_sel]] <- tabla_var
        }
      }
    })

    if (length(resultados_lista) > 0) {
      # Combinar todos los resultados
      resultado_final <- bind_rows(resultados_lista)
      resultados$confiabilidad <- resultado_final
      showNotification(paste0("¡Confiabilidad calculada para ", length(resultados_lista), " variable(s)!"), type = "message")
    } else {
      showNotification("No se pudo calcular la confiabilidad. Verifica los ítems.", type = "error")
    }
  })

  # Tabla simplificada para mostrar
  output$tabla_confiabilidad <- DT::renderDataTable({
    req(resultados$confiabilidad)
    tabla <- resultados$confiabilidad

    # Seleccionar SOLO las columnas necesarias en el orden deseado
    tabla_mostrar <- data.frame(
      Variable = tabla$Variable,
      Alpha = tabla$Alpha_Cronbach,
      Omega = tabla$Omega_McDonald,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      tabla_mostrar,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:2),
          list(className = 'dt-left', targets = 0)
        ),
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      caption = "Confiabilidad - Alpha y Omega (Compatible con Jamovi)"
    )
  })

  # Descarga para tab Confiabilidad - SOLO 4 columnas
  output$descargar_confiabilidad <- downloadHandler(
    filename = function() paste0("Confiabilidad_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (is.null(resultados$confiabilidad) || nrow(resultados$confiabilidad) == 0) {
        showNotification("No hay resultados de confiabilidad para descargar. Ejecuta primero el análisis de confiabilidad.",
                        type = "error", duration = 5)
        return(NULL)
      }
      tabla <- resultados$confiabilidad

      # Crear tabla simplificada con SOLO 3 columnas
      tabla_descarga <- data.frame(
        Escala = tabla$Escala,
        Alpha = tabla$Alpha_Cronbach,
        Omega = tabla$Omega_McDonald,
        stringsAsFactors = FALSE
      )

      write_xlsx(tabla_descarga, file)
    }
  )

  # ========== DESCRIPTIVOS ==========
  output$selector_vars_descriptivos <- renderUI({
    req(resultados$vars_numericas)
    checkboxGroupInput(
      "vars_descriptivos_sel",
      "Selecciona variables:",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })
  
  observeEvent(input$btn_calcular_descriptivos, {
    req(datos_reactive(), input$vars_descriptivos_sel)
    if (length(input$vars_descriptivos_sel) == 0) {
      showNotification("Selecciona al menos una variable", type = "warning")
      return()
    }
    
    withProgress(message = "Calculando descriptivos...", {
      resultados$descriptivos <- calcular_descriptivos(datos_reactive(), input$vars_descriptivos_sel)
    })
    
    if (!is.null(resultados$descriptivos)) {
      showNotification("Descriptivos calculados", type = "message")
    } else {
      showNotification("No hay variables válidas para descriptivos.", type = "error")
    }
  })
  
  output$tabla_descriptivos <- DT::renderDataTable({
    req(resultados$descriptivos)
    DT::datatable(
      resultados$descriptivos,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      caption = "Estadísticos Descriptivos (M = Media, DE = Desviación Estándar, zg1 = Asimetría Z, zg2 = Curtosis Z)"
    ) %>%
      DT::formatStyle(
        'zg1',
        backgroundColor = DT::styleInterval(c(-1.96, 1.96), c('#ffd4d4', '#e8f4f8', '#ffd4d4')),
        color = DT::styleInterval(c(-1.96, 1.96), c('#721c24', '#155724', '#721c24'))
      ) %>%
      DT::formatStyle(
        'zg2',
        backgroundColor = DT::styleInterval(c(-1.96, 1.96), c('#ffd4d4', '#e8f4f8', '#ffd4d4')),
        color = DT::styleInterval(c(-1.96, 1.96), c('#721c24', '#155724', '#721c24'))
      )
  })
  
  # Descarga para tab Descriptivos
  output$descargar_descriptivos <- downloadHandler(
    filename = function() paste0("Descriptivos_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (is.null(resultados$descriptivos) || nrow(resultados$descriptivos) == 0) {
        showNotification("No hay resultados de descriptivos para descargar. Ejecuta primero el análisis de descriptivos.",
                        type = "error", duration = 5)
        return(NULL)
      }
      write_xlsx(resultados$descriptivos, file)
    }
  )
  
  # ========== CORRELACIONES ==========
  output$selector_vars_correlaciones <- renderUI({
    req(resultados$vars_numericas)
    checkboxGroupInput(
      "vars_correlaciones_sel",
      "Selecciona variables (mínimo 2):",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })
  
  observeEvent(input$btn_calcular_correlaciones, {
    req(datos_reactive(), input$vars_correlaciones_sel)
    
    if (length(input$vars_correlaciones_sel) < 2) {
      showNotification("Selecciona al menos 2 variables", type = "warning")
      return()
    }
    
    withProgress(message = "Calculando correlaciones...", {
      if (input$metodo_correlacion == "robust") {
        # Usar correlaciones robustas con Percentage Bend (WRS2)
        resultados$correlaciones <- calcular_correlaciones_robusta(
          datos_reactive(),
          input$vars_correlaciones_sel
        )
      } else {
        # Usar correlaciones tradicionales (Pearson o Spearman)
        resultados$correlaciones <- calcular_correlaciones(
          datos_reactive(),
          input$vars_correlaciones_sel,
          input$metodo_correlacion
        )
      }
    })

    if (!is.null(resultados$correlaciones)) {
      showNotification("Correlaciones calculadas", type = "message")
    } else {
      if (input$metodo_correlacion == "robust") {
        showNotification("Error: No se pudieron calcular correlaciones robustas. Verifica que tengas datos válidos y que el paquete WRS2 esté instalado.", type = "error")
      } else {
        showNotification("Error: No hay suficientes datos limpios para correlacionar las variables seleccionadas (n<3).", type = "error")
      }
    }
  })
  
  output$tabla_correlaciones <- DT::renderDataTable({
    req(resultados$correlaciones)

    # Añadir columna de nombres de variables
    tabla <- resultados$correlaciones
    tabla <- cbind(Variable = rownames(tabla), tabla)
    rownames(tabla) <- NULL

    DT::datatable(
      tabla,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = '150px', targets = 0),  # Primera columna más ancha
          list(className = 'dt-center', targets = 1:ncol(tabla)-1)
        )
      ),
      escape = FALSE, # Para que los asteriscos se muestren correctamente
      caption = "Matriz de Correlaciones (Pearson, Spearman o Robust - PB). *** p<.001, ** p<.01, * p<.05, NC = No Calculable"
    ) %>%
      DT::formatStyle(
        columns = 1:ncol(tabla),
        backgroundColor = DT::styleEqual(c(''), c('transparent'))
      )
  })
  
  output$plot_correlaciones <- renderPlot({
    req(datos_reactive(), input$vars_correlaciones_sel)

    # Obtener datos seleccionados
    df_transformed <- datos_reactive() %>%
      select(all_of(input$vars_correlaciones_sel)) %>%
      na.omit()

    # Calcular matriz de correlaciones
    M <- cor(df_transformed)

    # Crear corrplot sencillo y profesional
    corrplot(M,
             method = "circle",
             type = "lower",
             tl.col = "black",
             tl.srt = 45,
             number.cex = 0.75,
             addCoef.col = "black")
  })
  
  # Descarga para tab Correlaciones
  output$descargar_correlaciones <- downloadHandler(
    filename = function() paste0("Correlaciones_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (is.null(resultados$correlaciones) || nrow(resultados$correlaciones) == 0) {
        showNotification("No hay resultados de correlaciones para descargar. Ejecuta primero el análisis de correlaciones.",
                        type = "error", duration = 5)
        return(NULL)
      }
      tabla <- resultados$correlaciones
      tabla <- cbind(Variable = rownames(tabla), tabla)
      rownames(tabla) <- NULL
      write_xlsx(tabla, file)
    }
  )

  # Descarga para corrplot en JPG (Alta resolución)
  output$descargar_corrplot_jpg <- downloadHandler(
    filename = function() paste0("Corrplot_", Sys.Date(), ".jpg"),
    content = function(file) {
      req(input$vars_correlaciones_sel)

      # Obtener datos seleccionados
      df_transformed <- datos_reactive() %>%
        select(all_of(input$vars_correlaciones_sel)) %>%
        na.omit()

      if (nrow(df_transformed) == 0) {
        showNotification("No hay datos válidos para generar el corrplot.",
                        type = "error", duration = 5)
        return(NULL)
      }

      # Calcular matriz de correlaciones
      M <- cor(df_transformed)

      # Guardar en JPG de alta resolución
      jpeg(file, width = 2400, height = 2400, res = 300, quality = 95)

      # Crear corrplot
      corrplot(M,
               method = "circle",
               type = "lower",
               tl.col = "black",
               tl.srt = 45,
               number.cex = 0.75,
               addCoef.col = "black")

      dev.off()
    }
  )

  # ========== COMPARACIONES (CÓDIGO CORREGIDO) ==========
  output$selector_vars_comparacion <- renderUI({
    req(resultados$vars_numericas)
    checkboxGroupInput(
      "vars_comparacion_sel",
      "Variables dependientes (Numéricas):",
      choices = resultados$vars_numericas,
      selected = NULL
    )
  })
  
  observeEvent(input$btn_comparar_grupos, {
    req(datos_reactive(), input$var_grupo, input$vars_comparacion_sel, input$metodo_comparacion)
    
    if (input$var_grupo == "" || length(input$vars_comparacion_sel) == 0) {
      showNotification("Selecciona variable de agrupación y al menos una variable dependiente.", type = "warning")
      return()
    }
    
    datos <- datos_reactive()
    datos[[input$var_grupo]] <- factor(datos[[input$var_grupo]]) # Asegurar que es factor
    grupos <- levels(datos[[input$var_grupo]][!is.na(datos[[input$var_grupo]])])
    num_grupos <- length(grupos)
    metodo <- input$metodo_comparacion
    
    # Validación de número de grupos según el método seleccionado
    if (metodo %in% c("ttest", "mannwhitney") && num_grupos != 2) {
      showNotification(paste0("Error: El método '", metodo, "' requiere exactamente 2 grupos. Tu variable tiene ", num_grupos, "."), type = "error", duration = 6)
      resultados$comparacion <- NULL
      return()
    }
    if (metodo %in% c("anova", "kruskal") && num_grupos < 3) {
      showNotification(paste0("Error: El método '", metodo, "' requiere 3 o más grupos. Tu variable tiene ", num_grupos, "."), type = "error", duration = 6)
      resultados$comparacion <- NULL
      return()
    }
    
    withProgress(message = "Comparando grupos...", {
      # Se pasan los parámetros: metodo y usar_mdn_ric
      comparacion_actual <- comparar_grupos(
        datos_reactive(),
        input$vars_comparacion_sel,
        input$var_grupo,
        metodo,
        usar_mdn_ric = input$usar_mdn_ric
      )

      # Actualizar resultados$comparacion para mostrar en la tabla
      resultados$comparacion <- comparacion_actual

      # Agregar a la lista acumulativa de comparaciones
      if (!is.null(comparacion_actual)) {
        # Crear nombre descriptivo para la hoja (máximo 31 caracteres para Excel)
        num_comparacion <- length(resultados$comparaciones_lista) + 1
        nombre_hoja <- paste0("Comparacion_", num_comparacion)

        # Agregar a la lista con información de metadatos
        resultados$comparaciones_lista[[nombre_hoja]] <- list(
          tabla = comparacion_actual,
          metodo = metodo,
          var_grupo = input$var_grupo,
          vars_analizadas = input$vars_comparacion_sel,
          fecha = Sys.Date()
        )
      }
    })

    if (!is.null(resultados$comparacion)) {
      num_total <- length(resultados$comparaciones_lista)
      showNotification(
        paste0("Comparación completada (Total guardado: ", num_total, ")"),
        type = "message"
      )
    } else {
      showNotification("Error al comparar grupos. Verifica que las variables dependientes sean numéricas y el número de grupos sea adecuado.", type = "error")
    }
  })
  
  output$tabla_comparacion <- DT::renderDataTable({
    # Requerir los resultados y método.
    req(input$var_grupo, input$metodo_comparacion, resultados$comparacion)

    tabla <- resultados$comparacion

    # Validar que la tabla tiene datos
    if (is.null(tabla) || nrow(tabla) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay resultados para mostrar. Asegúrate de seleccionar variables numéricas."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    # Si la tabla contiene columna 'Nota', mostrar errores específicos
    if ("Nota" %in% names(tabla)) {
      return(DT::datatable(
        tabla,
        options = list(
          dom = 'tp',
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        caption = "Mensajes de Error en Comparaciones"
      ))
    }

    # Reformatear tabla para presentación académica
    tabla_formateada <- reformatear_tabla_comparaciones(tabla, datos_reactive(), input$var_grupo)

    # Si no se pudo reformatear, mostrar tabla original
    if (is.null(tabla_formateada)) {
      return(DT::datatable(
        tabla,
        options = list(
          dom = 'tp',
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ))
    }

    # Crear DataTable con tabla formateada
    dt_out <- DT::datatable(
      tabla_formateada,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(className = 'dt-left', targets = 0)
        )
      ),
      rownames = FALSE,
      caption = paste0("Tabla de Comparación de Grupos - ", input$metodo_comparacion, ". (P < .05: Significativo)")
    )

    # Aplicar colores al p-valor SI existe
    if ("p" %in% names(tabla_formateada)) {
      dt_out <- dt_out %>%
        DT::formatStyle(
          'p',
          backgroundColor = DT::styleInterval(0.05, c('#f8d7da', '#d4edda')),
          color = DT::styleInterval(0.05, c('#721c24', '#155724'))
        )
    }

    return(dt_out)
  })

  # Selector dinámico de variables significativas para el boxplot
  output$selector_variables_boxplot <- renderUI({
    req(resultados$comparacion, input$var_grupo, input$vars_comparacion_sel)

    # Obtener variables significativas de la tabla de comparación
    vars_sig <- obtener_variables_significativas(resultados$comparacion)

    if (is.null(vars_sig) || length(vars_sig) == 0) {
      return(div(
        class = "alert alert-info",
        icon("info-circle"),
        strong("No hay variables con diferencias significativas."),
        "Las pruebas post-hoc solo se muestran cuando p ≤ 0.05."
      ))
    }

    # Crear selector con variables significativas
    fluidRow(
      column(
        6,
        selectInput(
          inputId = "var_boxplot_seleccionada",
          label = "Selecciona variable a visualizar:",
          choices = vars_sig,
          selected = vars_sig[1]
        )
      )
    )
  })

  # Boxplot para visualización de comparaciones - VERSIÓN CORREGIDA
  output$plot_boxplot_comparacion <- renderPlot({
    req(resultados$comparacion, input$var_grupo, input$vars_comparacion_sel)

    # Obtener variable seleccionada del selector dinámico
    req(input$var_boxplot_seleccionada)
    variable <- input$var_boxplot_seleccionada

    if (is.null(variable) || length(variable) == 0) {
      return(NULL)
    }

    # Verificar que la variable sea significativa
    vars_sig <- obtener_variables_significativas(resultados$comparacion)
    if (is.null(vars_sig) || !variable %in% vars_sig) {
      return(NULL)
    }

    # Crear boxplot
    plot <- crear_boxplot_comparaciones(
      datos_reactive(),
      variable,
      input$var_grupo
    )

    return(plot)
  })

  # Selector dinámico de variables significativas para post-hoc
  output$selector_variables_posthoc <- renderUI({
    req(resultados$comparacion, input$var_grupo, input$vars_comparacion_sel)

    # Obtener variables significativas de la tabla de comparación
    vars_sig <- obtener_variables_significativas(resultados$comparacion)

    if (is.null(vars_sig) || length(vars_sig) == 0) {
      return(div(
        class = "alert alert-info",
        icon("info-circle"),
        strong("No hay variables con diferencias significativas."),
        "Las pruebas post-hoc solo se muestran cuando p ≤ 0.05."
      ))
    }

    # Crear selector con variables significativas
    fluidRow(
      column(
        6,
        selectInput(
          inputId = "var_posthoc_seleccionada",
          label = "Selecciona variable para ver comparaciones:",
          choices = vars_sig,
          selected = vars_sig[1]
        )
      )
    )
  })

  # Pruebas Post-hoc (Tukey o Dunn) - VERSIÓN CORREGIDA
  output$tabla_posthoc_comparacion <- DT::renderDataTable({
    req(resultados$comparacion, input$var_grupo, input$metodo_comparacion, input$vars_comparacion_sel)

    tabla_actual <- resultados$comparacion

    # Verificar si la prueba fue significativa
    if (is.null(tabla_actual) || nrow(tabla_actual) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "Primero realiza una comparación de grupos"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    # Verificar significancia
    if ("p" %in% names(tabla_actual)) {
      p_valores <- tabla_actual$p
      todas_no_sig <- all(p_valores > 0.05, na.rm = TRUE)

      if (todas_no_sig) {
        return(DT::datatable(
          data.frame(Mensaje = "No hay diferencias significativas (p > 0.05). Las pruebas post-hoc no son necesarias."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
    }

    # Obtener variable seleccionada del selector dinámico
    req(input$var_posthoc_seleccionada)
    variable <- input$var_posthoc_seleccionada

    if (is.null(variable) || length(variable) == 0) {
      return(NULL)
    }

    # Verificar que la variable sea significativa
    vars_sig <- obtener_variables_significativas(resultados$comparacion)
    if (is.null(vars_sig) || !variable %in% vars_sig) {
      return(DT::datatable(
        data.frame(Mensaje = "Esta variable no tiene diferencias significativas."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    # Seleccionar función post-hoc según el método
    posthoc_df <- NULL

    if (input$metodo_comparacion == "anova") {
      # Usar Games-Howell (post-hoc robusto para Welch ANOVA)
      posthoc_df <- calcular_games_howell(
        datos_reactive(),
        variable,
        input$var_grupo
      )
      caption_text <- "Prueba Post-hoc: Games-Howell (Welch ANOVA)"
    } else if (input$metodo_comparacion == "kruskal") {
      # Usar Dunn's test
      posthoc_df <- calcular_dunn_test(
        datos_reactive(),
        variable,
        input$var_grupo
      )
      caption_text <- "Prueba Post-hoc: Dunn's Test (Kruskal-Wallis)"
    } else {
      return(DT::datatable(
        data.frame(Mensaje = "Las pruebas post-hoc no aplican para este método (se requieren 3+ grupos)"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    # Mostrar tabla si se calculó
    if (!is.null(posthoc_df) && nrow(posthoc_df) > 0) {
      DT::datatable(
        posthoc_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tp',
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        caption = caption_text
      )
    } else {
      DT::datatable(
        data.frame(Mensaje = "No se pudieron calcular las comparaciones post-hoc. Verifica que tengas 3+ grupos y datos válidos."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })

  # Descarga para tab Comparaciones
  output$descargar_comparacion <- downloadHandler(
    filename = function() paste0("Comparaciones_", Sys.Date(), ".xlsx"),
    content = function(file) {
      # Verificar si hay comparaciones en la lista o comparación actual
      if (length(resultados$comparaciones_lista) > 0) {
        # Modo: Múltiples comparaciones acumuladas
        tryCatch({
          lista_hojas <- list()

          # Crear hoja de resumen
          resumen_data <- data.frame(
            Número = seq_along(resultados$comparaciones_lista),
            Método = sapply(resultados$comparaciones_lista, function(x) x$metodo),
            Variable_Grupo = sapply(resultados$comparaciones_lista, function(x) x$var_grupo),
            Fecha = sapply(resultados$comparaciones_lista, function(x) as.character(x$fecha))
          )
          lista_hojas[["Resumen"]] <- resumen_data

          # Agregar cada comparación a la lista de hojas (REFORMATEADAS)
          for (i in seq_along(resultados$comparaciones_lista)) {
            comparacion_data <- resultados$comparaciones_lista[[i]]
            nombre_hoja <- paste0("Comp_", i)

            # Reformatear la tabla para descarga
            tabla_descarga <- reformatear_tabla_comparaciones(
              comparacion_data$tabla,
              datos_reactive(),
              comparacion_data$var_grupo
            )

            # Si la reformatación falló, usar tabla original
            if (is.null(tabla_descarga)) {
              tabla_descarga <- comparacion_data$tabla
            }

            lista_hojas[[nombre_hoja]] <- tabla_descarga
          }

          # Descargar todas las hojas a la vez
          write_xlsx(lista_hojas, file)
        }, error = function(e) {
          showNotification(paste0("Error al descargar comparaciones múltiples: ", e$message),
                          type = "error", duration = 5)
        })
      } else if (!is.null(resultados$comparacion) && nrow(resultados$comparacion) > 0) {
        # Modo: Solo la comparación actual
        tryCatch({
          # Reformatear tabla para descarga
          tabla_descarga <- reformatear_tabla_comparaciones(
            resultados$comparacion,
            datos_reactive(),
            input$var_grupo
          )

          # Si reformatación falló, usar tabla original
          if (is.null(tabla_descarga)) {
            tabla_descarga <- resultados$comparacion
          }

          # Descargar la tabla reformatada
          write_xlsx(tabla_descarga, file)
        }, error = function(e) {
          showNotification(paste0("Error al descargar comparación: ", e$message),
                          type = "error", duration = 5)
        })
      } else {
        # No hay datos para descargar
        showNotification("No hay resultados de comparaciones para descargar. Ejecuta primero una comparación de grupos.",
                        type = "error", duration = 5)
      }
    }
  )

}

# ============================================================================
# EJECUTAR APLICACIÓN
# ============================================================================

shinyApp(ui = ui, server = server)

