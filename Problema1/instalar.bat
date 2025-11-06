@echo off
REM Script de instalación - Sistema de Monitoreo Ambiental
REM Este script instala todas las dependencias necesarias

echo ====================================================================
echo Instalacion - Sistema de Monitoreo Ambiental
echo Empresa: Tengo Cara de Pepino S.A.
echo ====================================================================
echo.

REM Verifica Python
echo [1/3] Verificando Python...
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python no esta instalado
    echo.
    echo Descarga Python desde: https://www.python.org/
    echo Asegurate de marcar "Add Python to PATH" durante la instalacion
    pause
    exit /b 1
)

python --version
echo OK: Python instalado correctamente
echo.

REM Instala dependencias
echo [2/3] Instalando dependencias de Python...
pip install -r requirements.txt
if errorlevel 1 (
    echo ERROR: No se pudieron instalar las dependencias
    pause
    exit /b 1
)
echo OK: Dependencias instaladas
echo.

REM Ejecuta pruebas
echo [3/3] Ejecutando pruebas del sistema...
python tests\test_sistema.py
if errorlevel 1 (
    echo.
    echo ADVERTENCIA: Algunas pruebas fallaron
    echo Revisa los errores arriba
) else (
    echo.
    echo OK: Todas las pruebas pasaron
)
echo.

echo ====================================================================
echo INSTALACION COMPLETADA
echo ====================================================================
echo.
echo Proximos pasos:
echo   1. Compila y ejecuta el servidor Lazarus
echo   2. Ejecuta: iniciar_cliente.bat
echo.
echo O manualmente:
echo   python main.py
echo.
echo Scripts utiles:
echo   - python scripts/consultar_db.py    (Consultar base de datos)
echo   - python scripts/crear_db.py        (Crear BD con datos de prueba)
echo   - python tests/test_sistema.py      (Ejecutar pruebas)
echo ====================================================================

pause
