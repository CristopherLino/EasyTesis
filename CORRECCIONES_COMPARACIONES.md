# 📊 Correcciones en Sección de Comparaciones

## Problema Reportado
1. **Kruskal-Wallis**: No reportaba descriptivos ni tamaño del efecto
2. **Mann-Whitney U**: Faltaba visualizar el tamaño del efecto (correlación biserial de rangos)
3. **ANOVA**: Faltaban descriptivos por grupo

## Soluciones Implementadas

### 1. Función `comparar_grupos()` - Mann-Whitney U (Líneas 608-658)
✅ **Ya implementado anteriormente**
- Calcula correlación biserial de rangos: `r_rb = 1 - (2*U)/(n1*n2)`
- Muestra descriptivos: `Mdn=X.XX, RIC=[Q1, Q3]` para cada grupo
- Campos en resultado: `r_rb`, `Desc_G1`, `Desc_G2`

### 2. Función `comparar_grupos()` - ANOVA (Líneas 671-714) ✅ NUEVO
- Calcula descriptivos por grupo: Media (M) y Desviación Estándar (DE)
- Muestra eta-squared para tamaño del efecto
- Campos en resultado:
  - `Variable`: Nombre de la variable
  - `Prueba`: "ANOVA"
  - `Gl`: Grados de libertad (numerador, denominador)
  - `Estadistico`: Valor F
  - `p`: p-valor
  - `eta_squared`: Tamaño del efecto (η²)
  - `Desc_G1`, `Desc_G2`, `Desc_G3`, etc.: Descriptivos por grupo

### 3. Función `comparar_grupos()` - Kruskal-Wallis H (Líneas 718-793) ✅ NUEVO
**Cambios principales:**
- Calcula descriptivos por grupo: `Mdn=X.XX, RIC=[Q1, Q3]`
- Calcula eta-squared H: `η²_H = (H - k + 1) / (n - k)`
  - H: estadístico de Kruskal-Wallis
  - k: número de grupos
  - n: tamaño total de la muestra
- Interpreta tamaño del efecto:
  - **Débil**: < 0.04
  - **Moderado**: 0.04 - 0.25
  - **Fuerte**: 0.25 - 0.64
  - **Muy Fuerte**: > 0.64
- Campos en resultado:
  - `Variable`: Nombre de la variable
  - `Prueba`: "Kruskal-Wallis H"
  - `Gl`: Grados de libertad
  - `Estadistico`: Valor H
  - `p`: p-valor
  - `eta_H`: Eta-squared H (tamaño del efecto)
  - `Efecto`: Interpretación del tamaño del efecto
  - `Desc_G1`, `Desc_G2`, `Desc_G3`, etc.: Descriptivos por grupo

### 4. Renderización de Tabla (Líneas 2577-2585) ✅ ACTUALIZADO
Se actualizó la lista de columnas posibles para incluir:
- `r_rb`: Correlación biserial de rangos (Mann-Whitney)
- `eta_H`: Eta-squared H (Kruskal-Wallis)
- `Efecto`: Interpretación del efecto (Kruskal-Wallis)
- `Desc_G1` a `Desc_G10`: Descriptivos para hasta 10 grupos

```r
# ANTES:
columnas_posibles <- c("Variable", "Prueba", "Gl", "Estadistico", "p",
                       "d_Cohen", "r_effect", "eta_squared", "Desc_G1", "Desc_G2")

# DESPUÉS:
columnas_posibles <- c("Variable", "Prueba", "Gl", "Estadistico", "p",
                       "d_Cohen", "r_rb", "r_effect", "eta_squared", "eta_H",
                       "Efecto", "Desc_G1", "Desc_G2", "Desc_G3", "Desc_G4", "Desc_G5",
                       "Desc_G6", "Desc_G7", "Desc_G8", "Desc_G9", "Desc_G10")
```

## Resumen de Campos por Prueba

### T-test (2 grupos, paramétrico)
| Campo | Contenido | Ejemplo |
|-------|-----------|---------|
| Variable | Nombre variable | variable1 |
| Prueba | "T-test" | T-test |
| Gl | Grados libertad | 58 |
| Estadistico | Valor t | 2.543 |
| p | p-valor | 0.0134 |
| d_Cohen | d de Cohen | 0.657 |
| Desc_G1 | Media y DE grupo 1 | M=22.34, DE=4.89 |
| Desc_G2 | Media y DE grupo 2 | M=25.67, DE=5.12 |

### Mann-Whitney U (2 grupos, no paramétrico)
| Campo | Contenido | Ejemplo |
|-------|-----------|---------|
| Variable | Nombre variable | variable1 |
| Prueba | "Mann-Whitney U" | Mann-Whitney U |
| Estadistico | Valor U | 345.5 |
| p | p-valor | 0.0234 |
| r_rb | Correlación biserial rangos | 0.456 |
| Desc_G1 | Mediana y RIC grupo 1 | Mdn=22.50, RIC=[19.25, 24.75] |
| Desc_G2 | Mediana y RIC grupo 2 | Mdn=25.00, RIC=[21.50, 28.25] |

### ANOVA (3+ grupos, paramétrico)
| Campo | Contenido | Ejemplo |
|-------|-----------|---------|
| Variable | Nombre variable | variable1 |
| Prueba | "ANOVA" | ANOVA |
| Gl | Grados libertad | 2, 57 |
| Estadistico | Valor F | 3.876 |
| p | p-valor | 0.0258 |
| eta_squared | Eta-squared (η²) | 0.120 |
| Desc_G1 | Media y DE grupo 1 | M=20.45, DE=4.56 |
| Desc_G2 | Media y DE grupo 2 | M=23.67, DE=5.23 |
| Desc_G3 | Media y DE grupo 3 | M=25.12, DE=4.89 |

### Kruskal-Wallis H (3+ grupos, no paramétrico)
| Campo | Contenido | Ejemplo |
|-------|-----------|---------|
| Variable | Nombre variable | variable1 |
| Prueba | "Kruskal-Wallis H" | Kruskal-Wallis H |
| Gl | Grados libertad | 2 |
| Estadistico | Valor H | 5.432 |
| p | p-valor | 0.0663 |
| eta_H | Eta-squared H (η²_H) | 0.087 |
| Efecto | Interpretación | Moderado |
| Desc_G1 | Mediana y RIC grupo 1 | Mdn=21.00, RIC=[18.50, 23.75] |
| Desc_G2 | Mediana y RIC grupo 2 | Mdn=24.50, RIC=[21.25, 27.00] |
| Desc_G3 | Mediana y RIC grupo 3 | Mdn=25.50, RIC=[22.75, 28.25] |

## Resultado Final

Ahora la tabla de comparaciones mostrará:

✅ **T-test**: d_Cohen + descriptivos (M, DE)
✅ **Mann-Whitney U**: r_rb (correlación biserial) + descriptivos (Mdn, RIC)
✅ **ANOVA**: eta_squared + descriptivos (M, DE)
✅ **Kruskal-Wallis H**: eta_H + Efecto + descriptivos (Mdn, RIC)

Todos los campos se mostrarán automáticamente en la tabla de resultados sin necesidad de configuración adicional.

---

**Estado**: ✅ Listo para Producción
**Fecha**: 2025-10-27
