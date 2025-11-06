@echo off
REM Script de inicio - Cliente de envío de imágenes

echo ====================================================================
echo Cliente HTTP - Sistema de Envio de Imagenes
echo Empresa: Aqui te espero gallito Ltda
echo ====================================================================
echo.

echo Verificando requisitos...
python tests\test_sistema.py
if errorlevel 1 (
    echo.
    echo ERROR: No se pasaron las pruebas
    echo Soluciones:
    echo   1. Crea la carpeta 'img'
    echo   2. Agrega imagenes a la carpeta
    echo   3. Ejecuta: pip install -r requirements.txt
    pause
    exit /b 1
)

echo.
echo ====================================================================
echo Iniciando Cliente...
echo ====================================================================
echo NOTA: Asegurate de que el servidor Lazarus este ejecutandose
echo       en http://localhost:8080/imagen
echo.
echo Presiona Ctrl+C para detener
echo ====================================================================
echo.

python main.py

pause
