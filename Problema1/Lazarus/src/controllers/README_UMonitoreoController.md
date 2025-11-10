README - UMonitoreoController.pas
=================================

Propósito
--------
Esta unidad contiene `TMonitoreoController`, el coordinador principal que conecta el repositorio (persistencia), el servicio HTTP (entrada de datos) y el servicio de gráficos (visualización). Gestiona el ciclo de vida del servidor HTTP, la recepción de datos, el encolado/guardado en BD y la notificación a la UI.

Estructura / Secciones importantes
---------------------------------
- Interfaz (declaraciones):
  - Declaración de eventos: `TLogEvent`, `TDatosActualizadosEvent`.
  - Declaración de `TMonitoreoController` con los métodos públicos y privados.

- Implementación (constructor / destructor y métodos):
  - `constructor Create(ARepository, AHTTPService, AChartService)`
    - Inicializa dependencias, configura `FHTTPService.OnDatoRecibido` y `FHTTPService.OnLog`, y establece estaciones visibles.
  - `destructor Destroy`
    - Si el servidor está activo, lo detiene; registra cierre.

Métodos clave y responsabilidades
--------------------------------
- Control del servidor:
  - `IniciarServidor` — Llama a `FHTTPService.Iniciar` y registra el resultado.
  - `DetenerServidor` — Llama a `FHTTPService.Detener`.
  - `ServidorActivo` — Devuelve estado del servicio.

- Procesamiento de datos:
  - `OnDatoRecibido(AEstacion: TEstacionMonitoreo)` — Punto central: actualiza gráficos si la estación es visible, notifica `OnDatosActualizados` y delega guardado en BD (`FRepository.Guardar`). Implementado en modo "no bloqueante" (captura excepciones).
  - `CargarDatosIniciales` — Recupera registros históricos desde el repositorio y rellena las series del gráfico.

- Utilidades y Exportación:
  - `ExportarGrafico` / `GenerarNombreExportacion` — Exportan el gráfico a PNG y generan nombres únicos.
  - `ObtenerTotalRegistros`, `ObtenerRegistrosPorEstacion` — Delegan al repositorio.

Eventos públicos
----------------
- `OnLog` — Recibe mensajes de log con timestamp para que la UI los muestre.
- `OnDatosActualizados` — Notifica a la UI que llegaron datos para una estación concreta.

Notas de implementación
-----------------------
- `OnDatoRecibido` realiza guardado "fire-and-forget" (intenta guardar y captura/ignora errores) para no bloquear el procesamiento en tiempo real.
- Asegúrate de que las referencias (`FRepository`, `FHTTPService`, `FChartService`) sean válidas y gestionadas por quien crea el controlador (normalmente `UMainForm`).

Cómo probar / puntos de verificación
-----------------------------------
- Iniciar la aplicación y usar `btnIniciar` para arrancar el servidor HTTP.
- Enviar un POST JSON a `/datos` y confirmar que `OnDatoRecibido` actualiza las series y que `FRepository` inserta registros.
- Verificar el log en la UI (Memo1) para ver las entradas generadas por `RegistrarLog`.
