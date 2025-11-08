# Servidor HTTP de Monitoreo Ambiental - Lazarus

## Descripción
Servidor HTTP desarrollado en Lazarus Pascal que recibe datos ambientales en formato JSON de múltiples estaciones de monitoreo, los almacena en una base de datos SQLite y muestra gráficos en tiempo real.

## Características Implementadas

### ✓ 1. Servidor HTTP
- Puerto: 8080
- Endpoint: POST /datos
- Recibe estructuras JSON con datos de estaciones
- Respuestas en formato JSON

### ✓ 2. Base de Datos SQLite
- Archivo: `clima.db`
- Tabla: `estaciones`
- Almacena todos los parámetros ambientales

### ✓ 3. Visualización TChart
- Gráfico de líneas para temperatura (nTe)
- Hasta 10 series simultáneas (una por estación)
- Colores diferenciados para cada estación
- Left Scrolling automático (mantiene últimos 50 puntos)
- Selección de estaciones mediante CheckListBox

### ✓ 4. Exportación a PNG
- Exportación secuencial: `grafico_001.png`, `grafico_002.png`, etc.
- Botón de exportación manual

### ✓ 5. Interfaz de Usuario
- Panel de control con botones Iniciar/Detener
- CheckListBox para seleccionar estaciones visibles (1-10)
- Log de eventos en tiempo real
- Barra de estado con información actual

## Requisitos Previos

### Lazarus IDE
- Versión: 2.2.0 o superior
- Free Pascal Compiler (FPC) 3.2.0 o superior

### Paquetes Necesarios
Los siguientes paquetes deben estar instalados en Lazarus:
1. **TAChartLazarusPkg** - Para gráficos
2. **FCL** - Free Component Library
3. **SQLDBLaz** - Para SQLite
4. **LCL** - Lazarus Component Library

Para instalar paquetes en Lazarus:
1. Menú: `Package` → `Install/Uninstall Packages`
2. Buscar el paquete en la lista de la derecha
3. Hacer clic en `Install selection`
4. Reconstruir Lazarus cuando se solicite

## Instalación y Compilación

### 1. Abrir el Proyecto
```
Lazarus IDE → Proyecto → Abrir Proyecto
Seleccionar: ServidorMonitoreo.lpi
```

### 2. Compilar
```
Menú: Ejecutar → Compilar (Ctrl+F9)
```

### 3. Ejecutar
```
Menú: Ejecutar → Ejecutar (F9)
```

## Uso del Sistema

### Iniciar el Servidor
1. Hacer clic en el botón **"Iniciar Servidor"**
2. El estado cambiará a "Activo" (en verde)
3. El servidor escuchará en el puerto 8080

### Seleccionar Estaciones
- Marcar/desmarcar las estaciones en el CheckListBox
- Solo las estaciones marcadas se mostrarán en el gráfico
- Se pueden seleccionar de 1 a 10 estaciones simultáneamente

### Exportar Gráficos
1. Hacer clic en **"Exportar a PNG"**
2. Se generará un archivo PNG secuencial en el directorio del ejecutable
3. Formato: `grafico_001.png`, `grafico_002.png`, etc.

### Detener el Servidor
1. Hacer clic en **"Detener Servidor"**
2. El estado cambiará a "Detenido" (en rojo)

## Estructura de Datos JSON

El servidor espera recibir datos en el siguiente formato:

```json
{
    "ide": 1,
    "sFe": "2025-11-08 14:30:45",
    "sHo": "14:30:45",
    "MP": 25.5,
    "P10": 30.2,
    "nTe": 22.5,
    "nHr": 65.0,
    "nPa": 1013.25
}
```

### Campos:
- **ide**: ID de estación (1-10)
- **sFe**: Fecha sistema (YYYY-MM-DD HH:MM:SS)
- **sHo**: Hora sistema (HH:MM:SS)
- **MP**: Material Particulado (μg/m³)
- **P10**: Material Particulado 10 (μg/m³)
- **nTe**: Temperatura (°C) - **Graficada**
- **nHr**: Humedad Relativa (%)
- **nPa**: Presión Atmosférica (hPa)

## Base de Datos

### Ubicación
```
[Directorio del ejecutable]/clima.db
```

### Esquema de la Tabla
```sql
CREATE TABLE estaciones (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ide INTEGER NOT NULL,
    sFe TEXT NOT NULL,
    sHo TEXT NOT NULL,
    MP REAL NOT NULL,
    P10 REAL NOT NULL,
    nTe REAL NOT NULL,
    nHr REAL NOT NULL,
    nPa REAL NOT NULL,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Consultar Datos
Puedes usar cualquier herramienta SQLite para consultar los datos:
```bash
cd Problema1/Lazarus
sqlite3 clima.db "SELECT * FROM estaciones ORDER BY timestamp DESC LIMIT 10;"
```

O usar el script Python incluido:
```bash
cd Problema1
python scripts/consultar_db.py
```

## Integración con Cliente Python

### 1. Iniciar el Servidor Lazarus
```
Ejecutar ServidorMonitoreo.exe
Clic en "Iniciar Servidor"
```

### 2. Ejecutar el Cliente Python
```bash
cd Problema1
python main.py
```

El cliente Python enviará datos simulados cada segundo al servidor.

## Funcionalidad Left Scrolling

El gráfico implementa **Left Scrolling** automático:
- Mantiene visible un máximo de 50 puntos por serie
- Cuando se supera este límite, elimina el punto más antiguo
- Crea el efecto de desplazamiento hacia la izquierda
- Permite monitoreo continuo sin saturar el gráfico

## Archivos del Proyecto

```
Lazarus/
├── ServidorMonitoreo.lpr     # Programa principal
├── ServidorMonitoreo.lpi     # Archivo de proyecto
├── Unit1.pas                 # Código del formulario principal
├── Unit1.lfm                 # Diseño del formulario
├── clima.db                  # Base de datos (se crea automáticamente)
├── grafico_*.png             # Exportaciones PNG
└── README_LAZARUS.md         # Este archivo
```

## Solución de Problemas

### El servidor no inicia
- Verificar que el puerto 8080 esté libre
- Cerrar otras aplicaciones que puedan usar el puerto
- Ejecutar como administrador si es necesario

### No se reciben datos
- Verificar que el cliente Python esté ejecutándose
- Verificar que la URL en el cliente sea `http://localhost:8080/datos`
- Revisar el log de eventos en la ventana

### Error de base de datos
- Verificar permisos de escritura en el directorio
- Eliminar `clima.db` y dejar que se cree automáticamente
- Revisar el log de eventos para mensajes específicos

### No se ven las series en el gráfico
- Verificar que las estaciones estén marcadas en el CheckListBox
- Verificar que se estén recibiendo datos (revisar log)
- Esperar unos segundos para que lleguen datos

## Personalización

### Cambiar el número máximo de puntos visibles
En `Unit1.pas`, línea ~87:
```pascal
FMaxPuntos := 50; // Cambiar este valor
```

### Cambiar el puerto del servidor
En `btnIniciarClick`, línea ~207:
```pascal
FHttpServer.Port := 8080; // Cambiar el puerto
```

### Modificar colores de las series
En `InicializarChart`, línea ~151:
```pascal
Colors: array[1..10] of TColor = (
    clRed, clBlue, clGreen, // ... modificar colores
);
```

### Graficar otra variable
En `ActualizarChart`, cambiar:
```pascal
FSeries[i].AddXY(X, FUltimosDatos[i]);
```

Y en `ProcesarJSON`, cambiar:
```pascal
FUltimosDatos[ide] := nTe; // Cambiar por otra variable
```

## Notas Técnicas

- **Thread Safety**: El servidor HTTP usa threading interno para manejar múltiples peticiones
- **Performance**: Optimizado para 10 estaciones enviando datos cada segundo
- **Memoria**: El Left Scrolling mantiene el uso de memoria constante
- **Escalabilidad**: Puede modificarse para más estaciones o variables

## Autor
Proyecto desarrollado para el curso de Sistemas Inteligentes y Interfaces Gráficas
Universidad Católica de Temuco

## Licencia
Uso académico
