README - UHTTPService.pas
==========================

Propósito
--------
Implementa un servicio HTTP basado en `TFPHTTPServer` que expone el endpoint `POST /datos`. Recibe JSON con la estructura `TEstacionMonitoreo`, responde rápidamente al cliente y procesa los datos en memoria (notifica al controlador a través de `OnDatoRecibido`).

Estructura / Secciones importantes
---------------------------------
- Interfaz:
  - Tipos: `TDatoRecibidoEvent`.
  - `THTTPServerThread` — hilo que encapsula `TFPHTTPServer` y mantiene el servidor activo.
  - `THTTPService` — clase que gestiona inicio/parada del servidor y eventos `OnDatoRecibido`, `OnLog`.

- Implementación:
  - `THTTPServerThread.Create` / `Destroy` / `Execute` — Crea y mantiene activo `TFPHTTPServer` en un loop mientras el thread no sea `Terminated`.
  - `THTTPService.Iniciar` / `Detener` — Controlan la creación/liberación del thread y notifican por `OnLog`.
  - `HandleRequest` — Maneja las peticiones; cuando recibe `POST /datos`:
    - Lee `ARequest.Content`.
    - Responde inmediatamente `{status: "ok"}` con código 200 (para no bloquear al cliente).
    - Crea un objeto `TEstacionMonitoreo`, lo parsea con `FromJSON`, valida y si es correcto dispara `FOnDatoRecibido(Estacion)`.

Notas de implementación
-----------------------
- Se prioriza la respuesta rápida al cliente; el procesamiento puede ocurrir después de haber enviado la respuesta.
- `THTTPServerThread` configura `FServer.Threaded := False` porque la concurrencia es gestionada por el `TThread` que lo contiene.
- `HandleRequest` captura excepciones y devuelve 500 en errores graves, 404 en endpoints no soportados.

Cómo probar / puntos de verificación
-----------------------------------
- Iniciar el servidor desde la UI y enviar un POST JSON a `http://localhost:8080/datos` usando `curl` o `Postman`.
  Ejemplo `curl`:

```bash
curl -X POST -H "Content-Type: application/json" -d '{"ide":1,"sFe":"2025-11-09","sHo":"12:00:00","MP":12.5,"P10":5.2,"nTe":22.3,"nHr":55.0,"nPa":1013.2}' http://localhost:8080/datos
```

- Verificar que la UI recibe la actualización (si la estación está visible) y que la base de datos recibe la inserción.
