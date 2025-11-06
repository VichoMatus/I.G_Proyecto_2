@echo off
REM Script de instalación - Problema 2

echo ====================================================================
echo Instalacion - Sistema de Envio de Imagenes
echo Empresa: Aqui te espero gallito Ltda
echo ====================================================================
echo.

REM Verifica Python
echo [1/3] Verificando Python...
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python no esta instalado
    pause
    exit /b 1
)
python --version
echo.

REM Instala dependencias
echo [2/3] Instalando dependencias...
pip install -r requirements.txt
if errorlevel 1 (
    echo ERROR: No se pudieron instalar las dependencias
    pause
    exit /b 1
)
echo.

REM Ejecuta pruebas
echo [3/3] Ejecutando pruebas...
python tests\test_sistema.py
if errorlevel 1 (
    echo.
    echo ADVERTENCIA: Algunas pruebas fallaron
    echo Revisa que exista la carpeta 'img' con imagenes
)
echo.

echo ====================================================================
echo INSTALACION COMPLETADA
echo ====================================================================
echo.
echo Proximos pasos:
echo   1. Agrega imagenes (.jpg, .png) a la carpeta 'img'
echo   2. Compila y ejecuta el servidor Lazarus
echo   3. Ejecuta: python main.py
echo ====================================================================

pause
