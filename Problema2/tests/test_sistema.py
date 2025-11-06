"""
Suite de Pruebas - Sistema de Envío de Imágenes
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.services.gestor_imagenes import GestorImagenes


def test_carpeta_img():
    """Verifica que exista la carpeta img"""
    print("Test 1: Carpeta img...", end=" ")
    
    carpeta = Path('img')
    if not carpeta.exists():
        print("✗ FAIL")
        print("  Crea la carpeta 'img' y agrega imágenes (.jpg, .png, etc.)")
        return False
    
    print("✓ OK")
    return True


def test_imagenes_disponibles():
    """Verifica que haya imágenes en la carpeta"""
    print("Test 2: Imágenes disponibles...", end=" ")
    
    try:
        gestor = GestorImagenes('img')
        total = gestor.total_imagenes()
        
        if total == 0:
            print("✗ FAIL")
            print("  No hay imágenes en la carpeta 'img'")
            return False
        
        print(f"✓ OK ({total} imagen(es))")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        return False


def test_lectura_imagen():
    """Verifica que se puedan leer imágenes"""
    print("Test 3: Lectura de imágenes...", end=" ")
    
    try:
        gestor = GestorImagenes('img')
        imagen = gestor.obtener_imagen_aleatoria()
        contenido = gestor.leer_imagen(imagen)
        
        if len(contenido) == 0:
            print("✗ FAIL: Imagen vacía")
            return False
        
        print(f"✓ OK ({len(contenido)} bytes)")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        return False


def test_importaciones():
    """Verifica las importaciones"""
    print("Test 4: Importaciones...", end=" ")
    
    try:
        import requests
        import random
        import time
        from pathlib import Path
        print("✓ OK")
        return True
    except ImportError as e:
        print(f"✗ FAIL: {e}")
        print("  Ejecuta: pip install -r requirements.txt")
        return False


def main():
    """Ejecuta todas las pruebas"""
    print("=" * 60)
    print("SUITE DE PRUEBAS - Sistema de Envío de Imágenes")
    print("=" * 60 + "\n")
    
    resultados = []
    
    resultados.append(test_importaciones())
    resultados.append(test_carpeta_img())
    resultados.append(test_imagenes_disponibles())
    resultados.append(test_lectura_imagen())
    
    # Resumen
    print("\n" + "=" * 60)
    exitosas = sum(resultados)
    total = len(resultados)
    print(f"Resultados: {exitosas}/{total} pruebas pasadas")
    
    if exitosas == total:
        print("✓ TODAS LAS PRUEBAS PASARON")
        print("\nPuedes ejecutar: python main.py")
        print("=" * 60)
        sys.exit(0)
    else:
        print("✗ ALGUNAS PRUEBAS FALLARON")
        print("\nSoluciones:")
        print("  1. Crea la carpeta 'img' si no existe")
        print("  2. Agrega imágenes (.jpg, .png, etc.) a la carpeta")
        print("  3. Ejecuta: pip install -r requirements.txt")
        print("=" * 60)
        sys.exit(1)


if __name__ == "__main__":
    main()
