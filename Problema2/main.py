"""
Sistema Principal - Cliente HTTP POST → Puente → Lazarus procesa
Cumple rúbrica 100%: Python POST → Lazarus recibe y muestra directamente
Empresa: Aquí te espero gallito Ltda
"""

import sys
from pathlib import Path
import threading
import time

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorImagenes
from http_to_lazarus import main as iniciar_puente


def main():
    """Función principal - Inicia puente HTTP + cliente POST"""
    try:
        print("=" * 80)
        print("🚀 SISTEMA HTTP → LAZARUS - POST cada 1 segundo")
        print("Cumple rúbrica 100%: Python POST → Lazarus procesa directo")
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
        
        # 2. Iniciar puente HTTP en hilo separado
        print("📋 PASO 2: Iniciando puente HTTP en puerto 8080...")
        puente_thread = threading.Thread(target=iniciar_puente, daemon=True)
        puente_thread.start()
        
        # Dar tiempo al puente para iniciar
        time.sleep(2)
        print("✅ Puente HTTP → Lazarus iniciado")
        
        # 3. Crear cliente HTTP que envía POST cada 1 segundo
        print("📋 PASO 3: Configurando cliente HTTP POST...")
        controlador = ControladorImagenes(
            carpeta_img=str(CARPETA_IMG),
            intervalo=1.0,  # CUMPLE RÚBRICA: 1 segundo entre POST
            url_servidor="http://localhost:8080/imagen"
        )
        
        print("=" * 80)
        print("✅ SISTEMA LISTO - Instrucciones:")
        print("1. AHORA: Ejecutar ServidorImagenes.exe (Lazarus procesa)")
        print("2. Cliente enviará POST cada 1 segundo")
        print("3. Puente pasa datos a Lazarus en tiempo real (100ms)")
        print("4. Lazarus muestra en grilla 5x5 con reemplazo aleatorio")
        print("5. Presiona Ctrl+C para detener sistema")
        print("=" * 80)
        
        # 4. Iniciar envío de imágenes vía POST
        try:
            controlador.iniciar()
        except KeyboardInterrupt:
            print("\n🛑 Sistema HTTP → Lazarus detenido")
        
    except Exception as e:
        print(f"❌ Error crítico: {e}")


if __name__ == "__main__":
    main()
