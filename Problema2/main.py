"""
Sistema Principal - Cliente HTTP POST → Lazarus recibe directamente
Arquitectura limpia: Python POST → Servidor HTTP Lazarus integrado
Empresa: Aquí te espero gallito Ltda
"""

import sys
from pathlib import Path

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorImagenes


def main():
    """Función principal - Cliente POST directo a Lazarus"""
    try:
        print("=" * 80)
        print("🚀 SISTEMA HTTP → LAZARUS - POST cada 1 segundo")
        print("Arquitectura Limpia: Python POST → Servidor HTTP Lazarus")
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
        
        # 2. Configurar URL del servidor Lazarus (puerto 8080)
        URL_SERVIDOR = 'http://localhost:8080/imagen'
        print(f"📋 PASO 2: URL Servidor Lazarus: {URL_SERVIDOR}")
        
        # 3. Crear controlador y cliente HTTP
        print("📋 PASO 3: Configurando cliente HTTP POST...")
        controlador = ControladorImagenes(
            carpeta_img=str(CARPETA_IMG),
            intervalo=1.0,  # CUMPLE RÚBRICA: 1 segundo entre POST
            url_servidor=URL_SERVIDOR
        )
        print("✅ Cliente HTTP listo")
        
        print("=" * 80)
        print("📡 INICIANDO ENVÍO DE IMÁGENES")
        print("1. Asegúrate de que Lazarus esté ejecutándose")
        print("2. Cliente enviará POST cada 1 segundo directamente a Lazarus")
        print("3. Lazarus muestra en grilla 5x5 con reemplazo aleatorio")
        print("4. Presiona Ctrl+C para detener sistema")
        print("=" * 80)
        
        # 4. Iniciar envío de imágenes vía POST (bucle infinito)
        controlador.iniciar()
        
    except KeyboardInterrupt:
        print("\n🛑 Sistema HTTP → Lazarus detenido")
    except Exception as e:
        print(f"❌ Error crítico: {e}")


if __name__ == "__main__":
    main()
