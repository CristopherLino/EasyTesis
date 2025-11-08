# 🚀 Guía de Despliegue - EasyTesis en Posit Connect Cloud

Esta guía te ayudará a publicar la aplicación **EasyTesis** en **Posit Connect Cloud** de manera rápida y segura.

---

## 📋 Requisitos previos

- ✅ Cuenta en [Posit Cloud](https://posit.cloud/)
- ✅ R instalado (versión 4.0 o superior)
- ✅ RStudio instalado (recomendado)
- ✅ Paquete `rsconnect` instalado
- ✅ Repositorio GitHub clonado localmente

---

## 🔐 Paso 1: Obtener credenciales de Posit Connect Cloud

### 1.1 Acceder a tu cuenta

1. Ve a [posit.cloud](https://posit.cloud/)
2. Inicia sesión con tu cuenta
3. Haz clic en tu nombre de usuario (esquina superior derecha)
4. Selecciona **"Account Settings"** o **"Workspace Settings"**

### 1.2 Generar token de acceso

1. Ve a **"API Tokens"** o **"Publishing Settings"**
2. Haz clic en **"Create New Token"** o **"New Token"**
3. Dale un nombre descriptivo: `"EasyTesis-Deployment"`
4. Copia el token completo (aparecerá una sola vez)
5. Guárdalo en un lugar seguro (gestor de contraseñas)

---

## 📥 Paso 2: Configurar rsconnect en RStudio

### 2.1 Instalar paquete rsconnect

```r
install.packages("rsconnect")
```

### 2.2 Configurar tu cuenta

En RStudio, ejecuta:

```r
library(rsconnect)

rsconnect::setAccountInfo(
  account = "tu_nombre_usuario",  # Reemplazar con tu usuario de Posit Cloud
  token = "tu_token_aqui",        # Reemplazar con el token generado
  secret = "tu_secret_aqui"       # Si está disponible en las credenciales
)
```

**Alternativa**: Si Posit Cloud solo proporciona un token sin secret:

```r
rsconnect::setAccountInfo(
  account = "tu_nombre_usuario",
  token = "tu_token_aqui"
)
```

---

## 🚀 Paso 3: Publicar desde RStudio

### 3.1 Opción A: Usar el botón "Publish" (Recomendado)

1. Abre el archivo `app.R` en RStudio
2. Haz clic en el botón **"Publish"** (esquina superior derecha del editor)
3. Selecciona **"Publish to Posit Connect"** o similar
4. Verifica la información de la app:
   - **App Name**: `easytesis` (sin espacios)
   - **Server**: Selecciona tu cuenta de Posit Cloud
5. Haz clic en **"Publish"**
6. Espera a que se complete el despliegue (2-5 minutos)

### 3.2 Opción B: Usar línea de comandos

En RStudio console o terminal R:

```r
rsconnect::deployApp(
  appDir = getwd(),  # Directorio actual debe ser la raíz del proyecto
  appName = "easytesis",
  account = "tu_nombre_usuario",
  server = "posit.cloud"
)
```

---

## ✅ Paso 4: Verificar la publicación

### 4.1 Acceder a tu app publicada

1. Ve a [posit.cloud](https://posit.cloud/)
2. En el dashboard, deberías ver "easytesis" en tu lista de aplicaciones
3. Haz clic en la app para abrirla

### 4.2 Compartir la URL

Tu aplicación estará disponible en:
```
https://posit.cloud/content/[ID]/easytesis/
```

O una URL similar según tu workspace.

---

## 🔒 Configurar privacidad y seguridad

### 5.1 Control de acceso

1. En el dashboard de Posit Cloud, haz clic en tu app "easytesis"
2. Selecciona **"Sharing"** o **"Access Settings"**
3. Configura quién puede acceder:
   - **Private**: Solo tú
   - **Workspace**: Cualquier miembro del workspace
   - **Public**: Cualquiera con el link

### 5.2 Variables de entorno (si es necesario)

Si tu app necesita variables de entorno (API keys, etc.):

1. En la configuración de la app en Posit Cloud
2. Busca **"Environment Variables"** o **"Settings"**
3. Agrega las variables necesarias (nunca hardcodees credenciales)

---

## 🔄 Actualizar la app publicada

### Después de hacer cambios en app.R:

1. Salva los cambios locales
2. En RStudio, haz clic en **"Publish"** nuevamente
3. O ejecuta:

```r
rsconnect::deployApp(
  appDir = getwd(),
  appName = "easytesis",
  account = "tu_nombre_usuario",
  server = "posit.cloud"
)
```

---

## 📊 Monitorear el rendimiento

### En Posit Cloud:

1. Accede a tu app en el dashboard
2. Selecciona **"Metrics"** o **"Analytics"** si está disponible
3. Revisa:
   - Usuarios activos
   - Tiempo de respuesta
   - Errores

---

## 🐛 Solución de problemas

### Error: "Authentication failed"

```
Solución:
1. Verifica que copiaste correctamente el token
2. Ejecuta rsconnect::setAccountInfo() nuevamente
3. Asegúrate de no haber expirado el token
```

### Error: "Permission denied"

```
Solución:
1. Verifica que tu cuenta tenga permisos de publicación
2. Intenta crear una app en Posit Cloud primero
3. Contacta al administrador del workspace
```

### Error: "Package not found"

```
Solución:
1. Ejecuta source("install_dependencies.R") localmente
2. Asegúrate de tener internet durante el despliegue
3. Revisa los logs en Posit Cloud para más detalles
```

### App carga lentamente

```
Solución:
1. Reduce el tamaño de datos (máx. 10,000 filas recomendado)
2. Optimiza el código con req() y reactive()
3. Revisa el uso de memoria en Posit Cloud
```

### Datos no se cargan correctamente

```
Solución:
1. Verifica que los archivos Excel tengan formato correcto
2. Asegúrate de que los nombres de columnas sean únicos
3. Prueba la app localmente primero: shiny::runApp()
```

---

## 🛠️ Configuración avanzada

### Usar variables de entorno locales

En RStudio:

```r
# En tu app.R o archivo .Renviron
Sys.setenv("TU_VARIABLE" = "valor")
```

En Posit Cloud:

1. Crea un archivo `.Renviron` en tu proyecto:
```
TU_VARIABLE=valor
```

2. **NO lo commits a GitHub** (agrega a .gitignore)

### Usar base de datos

Si necesitas conectar a una BD:

```r
library(DBI)
library(RPostgres)  # u otro driver

con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  dbname = Sys.getenv("DB_NAME")
)
```

---

## 📝 Checklist de despliegue final

- [ ] Instalaste rsconnect: `install.packages("rsconnect")`
- [ ] Configuraste credenciales: `rsconnect::setAccountInfo()`
- [ ] Probaste localmente: `shiny::runApp()`
- [ ] Actualizaste .gitignore si es necesario
- [ ] Removiste datos sensibles de la app
- [ ] Documentaste variables de entorno necesarias
- [ ] Publicaste desde RStudio o línea de comandos
- [ ] Verificaste que la app funciona en Posit Cloud
- [ ] Configuraste control de acceso apropiadamente
- [ ] Compartiste el link con usuarios finales

---

## 📚 Recursos adicionales

- [Documentación oficial de Posit Connect](https://docs.posit.co/connect/)
- [Publicar en Posit Cloud](https://docs.posit.co/cloud/)
- [Referencia de rsconnect](https://docs.posit.co/rsconnect-r/)
- [Best practices en Shiny](https://shiny.rstudio.com/articles/)

---

## 💬 Soporte y ayuda

Si encuentras problemas:

1. Revisa los logs en Posit Cloud (sección de app)
2. Ejecuta `rsconnect::showLogs()` en RStudio
3. Prueba la app localmente primero
4. Consulta la [comunidad de Posit](https://community.rstudio.com/)

---

**Última actualización**: 2025-11-08
**Versión**: 1.0
**Autor**: Cristopher Lino-Cruz
