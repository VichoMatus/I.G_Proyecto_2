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
    CARPETA_IMG = 'img'  # Carpeta con las imágenes
    INTERVALO = 1  # Segundos entre cada envío
    
    try:
        controlador = ControladorImagenes(
            url_servidor=URL_SERVIDOR,
            carpeta_img=CARPETA_IMG,
            intervalo=INTERVALO
        )
        controlador.iniciar()
    except FileNotFoundError as e:
        print(f"✗ Error: {e}")
        print("\nAsegúrate de que existe la carpeta 'img' con imágenes")
        sys.exit(1)
    except ValueError as e:
        print(f"✗ Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"✗ Error fatal: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
