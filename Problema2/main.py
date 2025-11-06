"""
Punto de Entrada - Cliente de Envío de Imágenes
Empresa: Aquí te espero gallito Ltda
"""

import sys
from pathlib import Path

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorImagenes


def main():
    """Función principal"""
    # Configuración
    URL_SERVIDOR = 'http://localhost:8080/imagen'  # Endpoint del servidor Lazarus
    
    # Busca la carpeta img automáticamente
    directorio_actual = Path(__file__).parent  # Directorio del archivo main.py
    CARPETA_IMG = directorio_actual / 'img'    # Problema2/img
    
    # Si no existe, busca desde la raíz del proyecto
    if not CARPETA_IMG.exists():
        raiz_proyecto = Path.cwd()  # Directorio actual donde se ejecuta
        CARPETA_IMG = raiz_proyecto / 'Problema2' / 'img'
    
    INTERVALO = 1  # Segundos entre cada envío
    
    try:
        print(f"📁 Buscando imágenes en: {CARPETA_IMG}")
        
        controlador = ControladorImagenes(
            url_servidor=URL_SERVIDOR,
            carpeta_img=str(CARPETA_IMG),
            intervalo=INTERVALO
        )
        controlador.iniciar()
    except FileNotFoundError as e:
        print(f"✗ Error: {e}")
        print(f"\n💡 Asegúrate de que existe la carpeta: {CARPETA_IMG}")
        print("   con imágenes (.jpg, .png, .bmp, .gif)")
        sys.exit(1)
    except ValueError as e:
        print(f"✗ Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"✗ Error fatal: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
