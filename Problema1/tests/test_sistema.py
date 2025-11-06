"""
Suite de Pruebas - Sistema de Monitoreo
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.models.estacion import EstacionMonitoreo
from src.services.generador import GeneradorDatos
from src.utils.validador import Validador


def test_modelo_estacion():
    """Prueba el modelo de datos"""
    print("Test 1: Modelo Estación...", end=" ")
    
    estacion = EstacionMonitoreo(
        ide=5,
        sFe="2025-11-06 14:00:00",
        sHo="14:00:00",
        MP=12.5,
        P10=16.3,
        nTe=21.0,
        nHr=68.0,
        nPa=1005.0
    )
    
    assert estacion.validar() == True
    assert estacion.to_dict()['ide'] == 5
    assert isinstance(estacion.to_dict(), dict)
    
    print("✓ OK")
    return True


def test_generador():
    """Prueba el generador de datos"""
    print("Test 2: Generador de Datos...", end=" ")
    
    generador = GeneradorDatos()
    estacion = generador.generar(3)
    
    assert estacion.ide == 3
    assert estacion.validar() == True
    assert 0 <= estacion.nHr <= 100
    assert estacion.MP >= 0
    assert estacion.P10 >= 0
    
    print("✓ OK")
    return True


def test_validador():
    """Prueba el validador"""
    print("Test 3: Validador...", end=" ")
    
    # Datos válidos
    datos_validos = {
        'ide': 5,
        'sFe': '2025-11-06 14:00:00',
        'sHo': '14:00:00',
        'MP': 12.5,
        'P10': 16.3,
        'nTe': 21.0,
        'nHr': 68.0,
        'nPa': 1005.0
    }
    assert Validador.validar_estructura_json(datos_validos) == True
    
    # Datos inválidos (falta campo)
    datos_invalidos = {'ide': 5, 'sFe': '2025-11-06'}
    assert Validador.validar_estructura_json(datos_invalidos) == False
    
    # ID fuera de rango
    datos_id_malo = datos_validos.copy()
    datos_id_malo['ide'] = 15
    assert Validador.validar_estructura_json(datos_id_malo) == False
    
    print("✓ OK")
    return True


def test_importaciones():
    """Prueba las importaciones"""
    print("Test 4: Importaciones...", end=" ")
    
    try:
        import requests
        import random
        import time
        from datetime import datetime
        print("✓ OK")
        return True
    except ImportError as e:
        print(f"✗ FAIL: {e}")
        return False


def main():
    """Ejecuta todas las pruebas"""
    print("=" * 60)
    print("SUITE DE PRUEBAS - Sistema de Monitoreo")
    print("=" * 60 + "\n")
    
    resultados = []
    
    try:
        resultados.append(test_importaciones())
        resultados.append(test_modelo_estacion())
        resultados.append(test_generador())
        resultados.append(test_validador())
    except Exception as e:
        print(f"\n✗ Error en pruebas: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
    
    # Resumen
    print("\n" + "=" * 60)
    exitosas = sum(resultados)
    total = len(resultados)
    print(f"Resultados: {exitosas}/{total} pruebas pasadas")
    
    if exitosas == total:
        print("✓ TODAS LAS PRUEBAS PASARON")
        print("=" * 60)
        sys.exit(0)
    else:
        print("✗ ALGUNAS PRUEBAS FALLARON")
        print("=" * 60)
        sys.exit(1)


if __name__ == "__main__":
    main()
