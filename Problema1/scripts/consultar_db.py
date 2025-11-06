"""
Script: Consultar Base de Datos
Herramienta interactiva para consultar datos
"""

import sqlite3
import sys
from pathlib import Path


def conectar(ruta_db: str):
    """Conecta a la base de datos"""
    if not Path(ruta_db).exists():
        print(f"✗ No existe: {ruta_db}")
        print("\nCrea la BD con: python scripts/crear_db.py")
        sys.exit(1)
    return sqlite3.connect(ruta_db)


def estadisticas(conn: sqlite3.Connection):
    """Muestra estadísticas generales"""
    cursor = conn.cursor()
    
    print("\n" + "=" * 70)
    print("ESTADÍSTICAS")
    print("=" * 70)
    
    total = cursor.execute("SELECT COUNT(*) FROM estaciones").fetchone()[0]
    print(f"Total registros: {total}")
    
    cursor.execute("SELECT ide, COUNT(*) FROM estaciones GROUP BY ide ORDER BY ide")
    print("\nPor estación:")
    for row in cursor.fetchall():
        print(f"  Estación {row[0]:2d}: {row[1]:5d} registros")
    
    cursor.execute("""
        SELECT ide, AVG(nTe), AVG(nHr), AVG(nPa)
        FROM estaciones GROUP BY ide ORDER BY ide
    """)
    print("\nPromedios:")
    print(f"{'Est':<5} {'Temp(°C)':<12} {'HR(%)':<12} {'PA(hPa)':<12}")
    print("-" * 45)
    for row in cursor.fetchall():
        print(f"{row[0]:<5} {row[1]:>10.2f}   {row[2]:>10.2f}   {row[3]:>10.2f}")


def ultimos_registros(conn: sqlite3.Connection, n: int = 10):
    """Muestra últimos N registros"""
    cursor = conn.cursor()
    
    print("\n" + "=" * 70)
    print(f"ÚLTIMOS {n} REGISTROS")
    print("=" * 70)
    
    cursor.execute("""
        SELECT ide, sFe, sHo, nTe, nHr, nPa
        FROM estaciones ORDER BY timestamp DESC LIMIT ?
    """, (n,))
    
    print(f"{'Est':<5} {'Fecha':<20} {'Hora':<10} {'Temp':<8} {'HR':<8} {'PA':<10}")
    print("-" * 70)
    for row in cursor.fetchall():
        print(f"{row[0]:<5} {row[1]:<20} {row[2]:<10} {row[3]:>6.2f}°C {row[4]:>6.2f}% {row[5]:>8.2f}")


def menu():
    """Menú principal"""
    print("\n" + "=" * 70)
    print("CONSULTA BASE DE DATOS")
    print("=" * 70)
    print("1. Estadísticas generales")
    print("2. Últimos N registros")
    print("0. Salir")
    return input("\nOpción: ")


if __name__ == "__main__":
    # Busca la base de datos
    rutas = ['Lazarus/clima.db', 'clima.db']
    ruta_db = None
    
    for r in rutas:
        if Path(r).exists():
            ruta_db = r
            break
    
    if not ruta_db:
        print("✗ No se encontró clima.db")
        print("Crea la BD con: python scripts/crear_db.py")
        sys.exit(1)
    
    print(f"Usando: {ruta_db}")
    conn = conectar(ruta_db)
    
    try:
        while True:
            op = menu()
            
            if op == '1':
                estadisticas(conn)
            elif op == '2':
                n = input("Cantidad (default: 10): ").strip()
                n = int(n) if n else 10
                ultimos_registros(conn, n)
            elif op == '0':
                break
            else:
                print("Opción inválida")
            
            input("\nPresiona Enter...")
    finally:
        conn.close()
        print("\n✓ Cerrado")
