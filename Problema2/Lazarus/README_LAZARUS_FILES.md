README - Lazarus (Problema2)
=================================

Resumen
-------
Este README describe los archivos principales del servidor Lazarus incluido en `Problema2/Lazarus/`. El proyecto implementa un servidor HTTP simple que recibe imágenes vía POST y las muestra en una grilla 5x5. El diseño sigue una separación al estilo "arquitectura limpia": unidades de modelos, servicios, controladores y vistas.

Ubicación
---------
Raíz del proyecto Lazarus: `Problema2/Lazarus/`
Código fuente principal: `Problema2/Lazarus/src/`

Archivos importantes en la raíz de `Problema2/Lazarus/`
-----------------------------------------------------
- `ServidorImagenes.lpi`
  - Archivo de proyecto de Lazarus (IDE). Contiene la configuración del proyecto, rutas y la lista de unidades incluidas.
- `ServidorImagenes.lpr`
  - Programa principal (entry point) en Pascal. Crea la aplicación y muestra el formulario principal.
- `ServidorImagenes.lps`
  - Script de proyecto (opcional), usado por Lazarus en tareas de empaquetado o configuraciones.
- `ServidorImagenes.res`
  - Recursos del proyecto (iconos, etc.).
- `ServidorImagenes.ico`
  - Icono del ejecutable.
- `ServidorImagenes.exe`
  - Ejecutable compilado (generado). No es necesario en el repositorio si prefieres compilar localmente.
- `lib/`
  - Binarios y unidades compiladas (.ppu, .compiled) usadas por el proyecto. Suele contener paquetes generados o dependencias internas.

Estructura de `src/` (código organizado)
---------------------------------------
Dentro de `Problema2/Lazarus/src/` hay subcarpetas clave que siguen la separación por responsabilidad:

- `controllers/`
  - Unidades encargadas de coordinar la lógica entre servicios y la vista.
  - Ejemplo: `UImagenController.pas` — recibe notificaciones del servicio HTTP y solicita al `GridService` mostrar la imagen.

- `models/`
  - Unidades que contienen las estructuras de datos del dominio (modelos).
  - Ejemplo: `UImagenModel.pas` — representa una imagen recibida, con métodos para cargar desde stream/bytes y liberar memoria.

- `services/`
  - Unidades con la lógica de infraestructura: servidor HTTP, parsing multipart/form-data, y control del grid.
  - Ejemplos:
    - `UHTTPImagenService.pas` — implementa un servidor HTTP (basado en `fphttpserver` o similar) que escucha en puerto 8080 y expone `POST /imagen`. Al recibir una imagen crea un `TImagenRecibida` y notifica al controlador usando un evento/callback.
    - `UGridService.pas` — maneja la grilla 5x5 (25 `TImage`), la inicialización de los controles dentro del `Panel` de la vista y la lógica de reemplazo aleatorio cuando la grilla está llena.

- `views/`
  - Unidades relacionadas con la interfaz de usuario.
  - Ejemplos:
    - `UMainForm.pas` — formulario principal que instancia servicios y controlador, y contiene los `Panel`s y el `Button` "Salir".
    - `UMainForm.lfm` — archivo de recursos con la definición visual del formulario (posición de panels, botón, labels, etc.).

Descripción de unidades claves (ejemplos típicos en este proyecto)
----------------------------------------------------------------
- `UImagenModel.pas`
  - Clase TImagenRecibida: encapsula `TMemoryStream` con los bytes de la imagen y métodos para cargar/convertir/limpiar la imagen.
  - Responsabilidad: modelo puro, sin lógica visual ni de red.

- `UHTTPImagenService.pas`
  - Clase THTTPImagenService / THTTPImagenThread: implementa el servidor HTTP en un hilo separado para no bloquear la UI.
  - Maneja la extracción del `boundary` y del `multipart/form-data`, reconstrucción del archivo recibido en memoria y disparo del evento `OnImagenRecibida`.
  - Importante: la notificación desde el hilo al hilo principal usa `Synchronize` con un método nombrado (patrón Lazarus) para evitar usar procedimientos anónimos en `Synchronize`.

- `UGridService.pas`
  - Clase TGridService: crea dinámicamente 25 `TImage` en un `TPanel`, ofrece `MostrarImagen(TImagenRecibida)` y funciones para limpiar la grilla o buscar una celda disponible/aleatoria.
  - Se encarga de mantener la interfaz visual y liberar recursos gráficos.

- `UImagenController.pas`
  - Clase TImagenController: actúa como coordinador entre `UHTTPImagenService` y `UGridService`.
  - Se suscribe al evento `OnImagenRecibida` del servicio HTTP y llama a `GridService.MostrarImagen`.
  - También puede exponer métodos `IniciarServidor`, `DetenerServidor`, `LimpiarGrid`, etc.

- `UMainForm.pas` y `UMainForm.lfm`
  - Formulario que instancia los servicios y el controlador en `FormCreate`.
  - Inicia el servidor automáticamente (según la versión actual) y contiene el botón "Salir" que detiene el servicio y cierra la aplicación.
  - El `*.lfm` contiene la disposición de `Panel`s y el botón.

Cómo compilar y ejecutar
------------------------
Requisitos mínimos:
- Lazarus + Free Pascal instalados (versiones compatibles con los archivos `.lpi` y `.lfm` usados; por ejemplo Lazarus 2.2.x / FPC 3.2+).

Pasos (IDE):
1. Abrir `Problema2/Lazarus/ServidorImagenes.lpi` en Lazarus IDE.
2. Asegurarse de que las rutas de búsqueda del proyecto incluyan `src/` y subcarpetas (suele estar configurado en el `.lpi`).
3. Compilar (Build) y ejecutar (Run). El formulario principal aparecerá y, según la configuración, el servidor HTTP puede iniciarse automáticamente.

Pasos (línea de comandos):
- Usando `lazbuild` (instalado con Lazarus):

```powershell
cd "Problema2\Lazarus"
lazbuild ServidorImagenes.lpi
# Luego ejecutar el binario generado: ServidorImagenes.exe
```

Cómo probar con el cliente Python (ya incluido en el repo):
1. Ejecutar el servidor Lazarus (desde IDE o ejecutable generado).
2. En el repositorio Python (Problema2), ejecutar el cliente:

```powershell
python main.py
```

El cliente enviará imágenes por HTTP POST al endpoint `http://localhost:8080/imagen` y la aplicación Lazarus debería mostrar las imágenes en la grilla.

Notas y recomendaciones
-----------------------
- Si prefieres que el servidor no se inicie automáticamente, modifica `UMainForm.InicializarComponentes` o la llamada a `FController.IniciarServidor`.
- Para debugging de red, puedes usar `curl` o `Postman` para enviar una petición `multipart/form-data` al endpoint y ver la respuesta del servidor.
- Mantén fuera del control de versiones los ejecutables compilados si prefieres que otros compilen localmente.

Contacto y autor
----------------
Este README fue generado automáticamente para documentar las unidades Lazarus del ejemplo "Problema2". Si falta alguna unidad específica, dime cuál y la agrego con más detalle.
