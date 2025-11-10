README - UChartService.pas
===========================

Propósito
--------
Gestiona la visualización de series en un `TChart` (TAGraph). Soporta 5 variables por estación (Temperatura, Humedad, Presión, MP, P10) y 10 estaciones (50 series en total). Provee métodos para agregar puntos, limpiar series y exportar a PNG.

Estructura / Secciones importantes
---------------------------------
- Interfaz:
  - `TChartService` con campos: `FSeries[1..5,1..10]`, `FMaxPuntos`, `FColores`, `FEstacionActiva`.
  - Métodos públicos: `Inicializar`, `AgregarPuntos`, `LimpiarSerie`, `LimpiarTodo`, `MostrarEstacion`, `ExportarAPNG`.

- Implementación:
  - `Inicializar` — Crea las 50 series (5 variables × 10 estaciones), configura títulos, ejes y leyenda; por defecto muestra la estación 1.
  - `AgregarPuntos` — Añade un punto a cada una de las 5 series de la estación activa; implementa eliminación de puntos antiguos para mantener `FMaxPuntos`.
  - `MostrarEstacion` — Activa/desactiva series para mostrar solo la estación seleccionada.
  - `ExportarAPNG` — Guarda el chart a PNG usando `FPImage` y `FPWritePNG`.

Notas de implementación
-----------------------
- Las series se crean con `TLineSeries` y se añaden al `TChart` pasado en el constructor.
- `FMaxPuntos` por defecto es 50 (configurable), controla la ventana temporal mostrada en el gráfico.

Cómo probar / puntos de verificación
-----------------------------------
- Iniciar la aplicación, seleccionar una estación y enviar datos para esa estación; verificar que las 5 series se actualizan.
- Probar `ExportarAPNG` desde la UI y comprobar que el archivo PNG se genera en la carpeta del ejecutable.
