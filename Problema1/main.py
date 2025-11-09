"""
Punto de Entrada - Cliente HTTP
Sistema de Monitoreo Ambiental
"""

import sys
from pathlib import Path

# Agrega el directorio raíz al path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.controlador import ControladorMonitoreo


def main():
    """Función principal"""
    # Configuración
    URL_SERVIDOR = 'http://localhost:8080/datos'
    NUM_ESTACIONES = 10
    INTERVALO = 3  # segundos
    
    try:
        controlador = ControladorMonitoreo(
            url_servidor=URL_SERVIDOR,
            num_estaciones=NUM_ESTACIONES,
            intervalo=INTERVALO
        )
        controlador.iniciar()
    except ValueError as e:
        print(f"Error de configuración: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error fatal: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
