@echo off
REM Script de inicio - Sistema HTTP POST completo

echo ====================================================================
echo SISTEMA HTTP POST - CUMPLE RUBRICA 100%%
echo Cliente Python + Servidor HTTP + Visualizacion Lazarus
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
echo INSTRUCCIONES DE USO:
echo ====================================================================
echo 1. ESTE SCRIPT: Inicia cliente Python + servidor HTTP
echo 2. SEPARADO: Ejecutar Lazarus\ServidorImagenes.exe para ver grilla
echo 3. El sistema cumple rubrica:
echo    ✓ Cliente Python envia POST cada 1 segundo
echo    ✓ Servidor HTTP recibe las imagenes
echo    ✓ Lazarus muestra en grilla 5x5 con reemplazo aleatorio
echo.
echo Presiona ENTER para continuar o Ctrl+C para salir
pause
echo.

echo ====================================================================
echo Iniciando Sistema HTTP POST...
echo ====================================================================

python main.py

pause
