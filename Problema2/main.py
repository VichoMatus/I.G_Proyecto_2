"""
Sistema de Comunicación Python → Lazarus
Copia imágenes periódicamente para que Lazarus las detecte y muestre en grilla 5x5
Empresa: Aquí te espero gallito Ltda
"""

import sys
from pathlib import Path

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorImagenes


def main():
    """Función principal"""
    # Busca la carpeta img automáticamente
    directorio_actual = Path(__file__).parent  # Directorio del archivo main.py
    CARPETA_IMG = directorio_actual / 'img'    # Problema2/img
    
    # Si no existe, busca desde la raíz del proyecto
    if not CARPETA_IMG.exists():
        raiz_proyecto = Path.cwd()  # Directorio actual donde se ejecuta
        CARPETA_IMG = raiz_proyecto / 'Problema2' / 'img'
    
    INTERVALO = 1  # Segundos entre cada copia
    
    try:
        print(f"📁 Buscando imágenes en: {CARPETA_IMG}")
        
        controlador = ControladorImagenes(
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
