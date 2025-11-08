# Instalación de SQLite para Lazarus - Windows

## El proyecto compiló correctamente ✓

## Problema: Falta la librería SQLite3

### Solución 1: Instalar paquete SQLDBLaz en Lazarus

1. **Menú Lazarus:**
   ```
   Package → Install/Uninstall Packages
   ```

2. **Buscar en la lista derecha:**
   - `SQLDBLaz`

3. **Instalar:**
   - Seleccionar `SQLDBLaz`
   - Clic en botón `→`
   - Clic en `Save and rebuild IDE`
   - Esperar que Lazarus se reconstruya

### Solución 2: Descargar sqlite3.dll (si el paquete ya está instalado)

Si el paquete ya está instalado pero falta la DLL:

#### Descargar sqlite3.dll:

1. **Ir a:** https://www.sqlite.org/download.html

2. **Buscar:** "Precompiled Binaries for Windows"
   - `sqlite-dll-win64-x64-XXXXXXX.zip` (para 64 bits)
   - o `sqlite-dll-win32-x86-XXXXXXX.zip` (para 32 bits)

3. **Descargar y extraer** el archivo ZIP

4. **Copiar `sqlite3.dll` a una de estas ubicaciones:**
   - Directorio del ejecutable: `Problema1\Lazarus\`
   - O: `C:\Windows\System32` (64 bits)
   - O: `C:\Windows\SysWOW64` (32 bits en Windows 64)

#### Forma rápida con PowerShell:

Ejecuta esto en PowerShell como Administrador:

```powershell
# Ir al directorio del proyecto
cd "D:\UCT\Cuarto Semestre\Sistemas Inteligentes y Interfaces Graficas\I.G_Proyecto_2\Problema1\Lazarus"

# Descargar sqlite3.dll (64 bits)
Invoke-WebRequest -Uri "https://www.sqlite.org/2024/sqlite-dll-win-x64-3450000.zip" -OutFile "sqlite3.zip"

# Extraer
Expand-Archive -Path "sqlite3.zip" -DestinationPath "." -Force

# Limpiar
Remove-Item "sqlite3.zip"

echo "sqlite3.dll instalado correctamente"
```

### Verificar instalación:

Después de instalar, ejecuta el programa y verifica que:
- No aparezca el error de "Can not load SQLite client library"
- El programa inicie correctamente
- La base de datos `clima.db` se cree automáticamente

### Si persisten los errores:

1. Verifica la arquitectura (32/64 bits) de tu Lazarus
2. Asegúrate que sqlite3.dll coincida con la arquitectura
3. Reinicia Lazarus después de instalar paquetes

## Siguiente paso:

Una vez resuelto el problema de SQLite:
1. Ejecuta el programa Lazarus (F9)
2. Clic en "Iniciar Servidor"
3. En otra terminal, ejecuta el cliente Python:
   ```bash
   cd Problema1
   python main.py
   ```

¡El sistema debería funcionar correctamente!
