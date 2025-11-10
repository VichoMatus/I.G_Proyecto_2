README - Lazarus (Problema1)
=================================

Resumen
-------
Este README documenta los archivos y unidades del proyecto Lazarus ubicado en `Problema1/Lazarus/`. El proyecto implementa un servidor de monitoreo que utiliza SQLite para persistencia local y una interfaz para mostrar datos (similar a una arquitectura con modelos, repositorios, servicios, controladores y vistas).

Ubicación
---------
Raíz del proyecto Lazarus: `Problema1/Lazarus/`
Código fuente principal: `Problema1/Lazarus/src/`

Archivos importantes en la raíz de `Problema1/Lazarus/`
-----------------------------------------------------
- `ServidorMonitoreo.lpi`
  - Archivo de proyecto de Lazarus (IDE). Contiene la configuración del proyecto, rutas y la lista de unidades incluidas.
- `ServidorMonitoreo.lpr`
  - Programa principal (entry point) en Pascal. Crea la aplicación y muestra el formulario principal.
- `ServidorMonitoreo.lps`
  - Script de proyecto (opcional), usado por Lazarus en tareas de empaquetado o configuraciones.
- `ServidorMonitoreo.res`
  - Recursos del proyecto (iconos, etc.).
- `ServidorMonitoreo.exe`
  - Ejecutable compilado (si está presente). Puedes eliminarlo del repositorio y compilar localmente si prefieres.
- `sqlite3.dll` y `sqlite3.def`
  - Bibliotecas y definiciones necesarias para usar SQLite en Windows con Lazarus. Ver `INSTALAR_SQLITE.md` para instrucciones de instalación/registro.
- `clima.db`
  - Base de datos SQLite de ejemplo usada por el servidor (puede contener datos de sensores para monitoreo).
- `lib/`
  - Binarios y unidades compiladas (.ppu, .compiled) que el proyecto usa. Contiene paquetes y objetos compilados.
- `backup/`
  - Carpeta de respaldos (puede contener archivos temporales o versiones antiguas del proyecto).

Estructura de `src/` (código organizado)
---------------------------------------
Dentro de `Problema1/Lazarus/src/` hay subcarpetas que siguen la separación por responsabilidad:

- `controllers/`
  - Contiene controladores que conectan los servicios con la vista.
  - Ejemplo: `UMonitoreoController.pas` — coordina acceso a datos, actualiza la vista y gestiona lógica de negocio del monitoreo.

- `models/`
  - Contiene modelos de dominio que representan entidades como una estación, lecturas, etc.
  - Ejemplo: `UEstacionModel.pas` — define la estructura de una estación y métodos de utilidad.

- `repositories/`
  - Encapsulan acceso a datos persistentes (SQLite).
  - Ejemplo: `UEstacionRepository.pas` — contiene métodos para CRUD en la base de datos `clima.db`.

- `services/`
  - Lógica de infraestructura y servicios auxiliares.
  - Ejemplos:
    - `UChartService.pas` — genera o prepara datos para gráficas en la UI.
    - `UHTTPService.pas` — servicio HTTP (si aplica) o utilidades relacionadas con comunicación.

- `views/`
  - Formas y recursos visuales (interfaces de usuario).
  - Ejemplos: `UMainForm.lfm` y `UMainForm.pas` — interfaz principal del monitor.

Descripción de unidades claves (ejemplos típicos en este proyecto)
----------------------------------------------------------------
- `UEstacionModel.pas`
  - Define la estructura de una estación de monitoreo (ID, nombre, coordenadas, lecturas, timestamp).
  - Responsabilidad: representar el dominio y validar datos.

- `UEstacionRepository.pas`
  - Implementa acceso a SQLite para persistir y recuperar las estaciones y lecturas.
  - Usa `sqlite3.dll` y las funciones de FPC/Delphi para abrir la base `clima.db`, ejecutar consultas y mapear resultados a `TEstacion`.
  - Importante: seguir `INSTALAR_SQLITE.md` para colocar `sqlite3.dll` en el mismo directorio del ejecutable o en PATH.

- `UHTTPService.pas` (si existe)
  - Implementa funcionalidades HTTP (por ejemplo endpoints para consultar datos) o utilidades de red.

- `UChartService.pas`
  - Encapsula la lógica para crear y actualizar gráficas en la UI (por ejemplo usando componentes de Lazarus para dibujar líneas/series).

- `UMonitoreoController.pas`
  - Coordina llamadas a `UEstacionRepository` para obtener datos y luego invoca `UChartService` o actualiza `UMainForm`.
  - Exposición de métodos como `IniciarMonitoreo`, `DetenerMonitoreo`, `RefrescarDatos`.

- `UMainForm.pas` y `UMainForm.lfm`
  - Formulario principal que contiene controles visuales: gráficos, listas de estaciones, botones para iniciar/parar monitoreo y un area de status.
  - `FormCreate` suele inicializar repositorios y servicios y cargar datos iniciales.

Archivos y notas específicas
---------------------------
- `INSTALAR_SQLITE.md`
  - Contiene instrucciones para instalar y registrar `sqlite3.dll` en Windows para que Lazarus pueda enlazar con la biblioteca SQLite. Siga esos pasos antes de compilar/ejecutar.

- `clima.db`
  - Base de datos SQLite pre-cargada con datos de ejemplo; útil para pruebas.

Cómo compilar y ejecutar
------------------------
Requisitos mínimos:
- Lazarus + Free Pascal instalados (ej. Lazarus 2.2.x / FPC 3.2+).
- En Windows: tener `sqlite3.dll` disponible en la carpeta del ejecutable o en PATH.

Pasos (IDE):
1. Abrir `Problema1/Lazarus/ServidorMonitoreo.lpi` en Lazarus IDE.
2. Confirmar que las rutas de búsqueda del proyecto incluyen `src/` y sus subcarpetas (normalmente configuradas en el `.lpi`).
3. Compilar (Build) y ejecutar (Run). El formulario principal aparecerá y se conectará a la base de datos `clima.db`.

Pasos (línea de comandos):
- Usando `lazbuild`:

```powershell
cd "Problema1\Lazarus"
lazbuild ServidorMonitoreo.lpi
# Ejecutar el binario generado: ServidorMonitoreo.exe
```

Notas para SQLite
-----------------
- Si obtienes errores al enlazar con SQLite, revisa `INSTALAR_SQLITE.md` y asegúrate de que `sqlite3.dll` esté en la misma carpeta que el ejecutable o en una ruta del sistema.
- En algunos casos, puede ser necesario copiar `sqlite3.dll` desde la carpeta `lib/x86_64-win64/` a la raíz del proyecto o al directorio del ejecutable.

Pruebas y debugging
-------------------
- Para probar el acceso a la base de datos, puedes ejecutar pequeñas utilidades Python o SQLite CLI apuntando a `clima.db`.
- Para depurar lógica de repositorios, agrega `ShowMessage` o `WriteLn` en puntos claves o usa el depurador del IDE.

Contacto y seguimiento
----------------------
Si quieres que documente unidad por unidad (lista exacta de files `.pas` y su breve descripción), dime y haré un README aún más detallado que incluya el contenido exacto de cada unit y ejemplos de uso (por ejemplo funciones/métodos públicos importantes).