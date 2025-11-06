@echo off
REM Script de inicio - Cliente HTTP
REM Sistema de Monitoreo Ambiental

echo ====================================================================
echo Cliente HTTP - Sistema de Monitoreo Ambiental
echo Empresa: Tengo Cara de Pepino S.A.
echo ====================================================================
echo.

REM Verifica que Python esté instalado
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python no esta instalado o no esta en el PATH
    echo Descarga Python desde: https://www.python.org/
    pause
    exit /b 1
)

echo [1/3] Verificando Python...
python --version
echo.

echo [2/3] Instalando dependencias...
pip install -r requirements.txt
if errorlevel 1 (
    echo ERROR: No se pudieron instalar las dependencias
    pause
    exit /b 1
)
echo.

echo [3/3] Ejecutando pruebas del sistema...
python test_sistema.py
if errorlevel 1 (
    echo.
    echo ADVERTENCIA: Algunas pruebas fallaron
    echo Puedes continuar pero puede haber problemas
    echo.
    pause
)
echo.

echo ====================================================================
echo Iniciando Cliente HTTP...
echo ====================================================================
echo NOTA: Asegurate de que el servidor Lazarus este ejecutandose
echo       en http://localhost:8080/datos
echo.
echo Presiona Ctrl+C para detener el cliente
echo ====================================================================
echo.

REM Ejecuta el cliente
python main.py

pause
