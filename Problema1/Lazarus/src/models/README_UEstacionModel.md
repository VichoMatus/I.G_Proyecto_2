README - UEstacionModel.pas
============================

Propósito
--------
Define el modelo de dominio `TEstacionMonitoreo` que representa una lectura de una estación (ID, fecha/hora, temperatura, humedad, presión, PM, P10). Proporciona serialización JSON, validación y herramientas de ayuda para convertir entre JSON y objetos.

Estructura / Secciones importantes
---------------------------------
- Interfaz:
  - Declaración de `TEstacionMonitoreo` con propiedades: `Ide`, `SFe`, `SHo`, `MP`, `P10`, `NTe`, `NHr`, `NPa`.
  - Métodos públicos: `FromJSON`, `FromJSONObject`, `ToJSON`, `ToJSONObject`, `Validar`, `ObtenerErrores`.

- Implementación:
  - `constructor Create` / `destructor Destroy` — Inicialización por defecto.
  - `FromJSON` / `FromJSONObject` — Parseo desde JSON usando `fpjson`/`jsonparser`.
  - `ToJSON` / `ToJSONObject` — Serialización a JSON.
  - `Validar` — Reglas de negocio (ID entre 1 y 10, campos no vacíos, rangos válidos para humedad y no-negatividad para partículas).
  - `ObtenerErrores` — Devuelve mensajes detallados cuando la validación falla.

Notas de implementación
-----------------------
- `FromJSON` captura excepciones y devuelve False si el parseo falla.
- `Validar` es usado por el repositorio antes de insertar en BD para asegurar consistencia.

Cómo probar / puntos de verificación
-----------------------------------
- Ejecutar `TEstacionMonitoreo.FromJSON('{...}')` con JSON válido y verificar `Validar` devuelve True.
- Probar con JSON incompleto y comprobar `ObtenerErrores` devuelve mensajes claros.
- Asegurarse de liberar objetos `TEstacionMonitoreo` cuando se devuelven en listas (evitar fugas de memoria).
