README - UMainForm.pas / UMainForm.lfm
======================================

Propósito
--------
Formulario principal de la aplicación de monitoreo. Inicializa la conexión a SQLite (`clima.db`), crea el repositorio, servicios y el controlador. Provee la interfaz para iniciar/detener servidor, seleccionar estaciones, ver logs, exportar gráficos y limpiar base de datos.

Estructura / Secciones importantes
---------------------------------
- Interfaz (componentes visuales declarados en `.pas` y `.lfm`):
  - Componentes: `btnIniciar`, `btnDetener`, `btnExportar`, `btnLimpiar`, `btnLimpiarBD`, `Chart1`, `ComboBox1`, `Memo1`, `Labels` de estado y `SQLite3Connection1`, `SQLQuery1`, `SQLTransaction1`.
  - `UMainForm.lfm` contiene la definición visual (posiciones, tamaños, align, etc.).

- Implementación:
  - `FormCreate` / `FormDestroy`:
    - `FormCreate` llama a `InicializarComponentes` y `ActualizarEstadoUI`.
    - `FormDestroy` libera instancias creadas.
  - `InicializarComponentes`:
    - Configura la ruta a `clima.db`, inicializa la conexión SQLite y las estructuras de BD.
    - Crea `FRepository`, `FHTTPService`, `FChartService` y `FController`.
    - Inicializa UI (ComboBox) y carga datos históricos mediante `FController.CargarDatosIniciales`.
  - Manejadores de interacción:
    - `btnIniciarClick` / `btnDetenerClick` — Controlan el servidor HTTP a través del controlador.
    - `ComboBox1Change` — Cambia la estación seleccionada, actualiza visibilidad y recarga histórico.
    - `btnExportarClick` — Exporta el gráfico actual mediante `FController.ExportarGrafico`.
    - `btnLimpiarBDClick` — Pide confirmación y llama a `FController.LimpiarBaseDatos`.
  - `OnControllerLog` — Agrega entradas al `Memo1` y limita su tamaño.

Notas de implementación
-----------------------
- `InicializarComponentes` falla con `Application.Terminate` si la conexión a SQLite no puede abrirse; revisar que `sqlite3.dll` esté disponible en Windows.
- Los componentes de BD (`SQLite3Connection1`, `SQLTransaction1`, `SQLQuery1`) están ligados al formulario y son reutilizados para pasar al `TEstacionRepository`.
- Asegúrate de que `FController.OnDatosActualizados` esté conectado para que la UI reciba actualizaciones en tiempo real.

Cómo probar / puntos de verificación
-----------------------------------
- Abrir el proyecto en Lazarus y compilar
- Ejecutar, conectar a la BD (`clima.db`) y pulsar "Iniciar" para levantar el servidor HTTP
- Enviar POSTs con JSON al endpoint `/datos` y verificar que los gráficos y los labels de la estación seleccionada se actualizan
- Probar exportar gráfico y limpiar BD desde la UI

