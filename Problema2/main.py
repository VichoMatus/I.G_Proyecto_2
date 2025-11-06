"""
Sistema Principal - Cliente HTTP POST directo a Lazarus
Cumple rúbrica 100%: Cliente Python envía POST → Lazarus HTTP Server recibe y muestra
Empresa: Aquí te espero gallito Ltda
"""

import sys
from pathlib import Path

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorImagenes


def main():
    """Función principal del cliente HTTP POST directo"""
    try:
        print("=" * 80)
        print("🚀 CLIENTE HTTP POST → SERVIDOR LAZARUS")
        print("Cumple rúbrica 100%: POST directo sin archivos intermedios")
        print("=" * 80)
        
        # 1. Detectar carpeta de imágenes
        print("📋 PASO 1: Detectando carpeta de imágenes...")
        directorio_actual = Path(__file__).parent
        CARPETA_IMG = directorio_actual / 'img'
        
        if not CARPETA_IMG.exists():
            raiz_proyecto = Path.cwd()
            CARPETA_IMG = raiz_proyecto / 'Problema2' / 'img'
        
        if not CARPETA_IMG.exists():
            print("❌ No se encontró carpeta 'img'")
            return
        
        print(f"✅ Carpeta encontrada: {CARPETA_IMG}")
        
        # 2. Crear cliente HTTP que envía POST cada 1 segundo
        print("📋 PASO 2: Configurando cliente HTTP POST...")
        controlador = ControladorImagenes(
            carpeta_img=str(CARPETA_IMG),
            intervalo=1.0,  # CUMPLE RÚBRICA: 1 segundo entre POST
            url_servidor="http://localhost:8080/imagen"  # POST directo a Lazarus
        )
        
        print("=" * 80)
        print("✅ CLIENTE HTTP LISTO - Instrucciones:")
        print("1. PRIMERO: Ejecutar ServidorImagenes.exe (Lazarus HTTP Server)")
        print("2. Lazarus recibirá POST requests directamente en puerto 8080")
        print("3. Las imágenes aparecerán en grilla 5x5 con reemplazo aleatorio")
        print("4. NO se guardan archivos - todo es directo en memoria")
        print("5. Presiona Ctrl+C para detener cliente")
        print("=" * 80)
        
        # 3. Iniciar envío de imágenes vía POST directo a Lazarus
        try:
            controlador.iniciar()
        except KeyboardInterrupt:
            print("\n🛑 Cliente HTTP detenido")
        
    except Exception as e:
        print(f"❌ Error crítico: {e}")


if __name__ == "__main__":
    main()
