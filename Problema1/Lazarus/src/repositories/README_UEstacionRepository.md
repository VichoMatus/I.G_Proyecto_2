README - UEstacionRepository.pas
=================================

Propósito
--------
Encapsula el acceso a la base de datos SQLite (`clima.db`) usando `TSQLite3Connection`, `TSQLTransaction` y `TSQLQuery`. Ofrece inicialización, creación de tablas, operaciones CRUD y utilidades para contar/limpiar registros.

Estructura / Secciones importantes
---------------------------------
- Interfaz:
  - Declaración de `TEstacionRepository` con constructores sobrecargados (`Create(AConnection, ATransaction)` y `Create(const ADatabasePath: String)`), destructor y métodos públicos (`InicializarBaseDatos`, `CrearTablas`, `Guardar`, `ObtenerPorId`, `ObtenerUltimosPorEstacion`, `ContarRegistros`, `LimpiarDatos`, etc.).

- Implementación:
  - `constructor Create(const ADatabasePath: String)` — Crea y configura `TSQLite3Connection`, `TSQLTransaction`, `TSQLQuery` y llama `InicializarBaseDatos`.
  - `InicializarBaseDatos` / `CrearTablas` — Conexión y creación de la tabla `estaciones` si no existe.
  - `Guardar(AEstacion)` — Inserta usando parámetros; valida el modelo antes de insertar. Implementa commits parciales para rendimiento.
  - `ObtenerUltimosPorEstacion` / `ObtenerTodosLosUltimos` — Ejecutan SELECT con `ORDER BY timestamp DESC` y devuelven `TList` de `TEstacionMonitoreo`.
  - `ContarRegistros`, `ContarPorEstacion` — Queries de conteo seguras.
  - `LimpiarDatos` — Borra todos los registros (DELETE) y hace commit.

Notas de implementación
-----------------------
- Cuando `Create` recibe `ADatabasePath`, el repositorio 'posee' la conexión (`FOwnsConnection := True`) y es responsable de cerrarla y liberarla en `Destroy`.
- `Guardar` evita hacer rollback en cada error para no impactar el rendimiento, sin embargo el método retorna `False` si la inserción falla.
- Los métodos que devuelven `TList` crean objetos `TEstacionMonitoreo` y devuelven la lista: el llamante es responsable de liberar cada objeto y la lista.

Cómo probar / puntos de verificación
-----------------------------------
- Instanciar `TEstacionRepository.Create('clima.db')` y llamar `ContarRegistros`, `ObtenerUltimosPorEstacion`.
- Insertar registros con `Guardar` y verificar que `ContarRegistros` aumenta.
- Ejecutar `LimpiarDatos` y comprobar que la tabla queda vacía.
- Verificar que `clima.db` existe y que `sqlite3.dll` está disponible si se ejecuta en Windows.
