"""
Script: Crear Base de Datos
Inicializa la base de datos con datos de prueba
"""

import sys
import sqlite3
import random
from pathlib import Path
from datetime import datetime, timedelta

sys.path.insert(0, str(Path(__file__).parent.parent))
from src.services.generador import GeneradorDatos


def crear_base_datos(ruta_db: str):
    """Crea la estructura de la base de datos"""
    conn = sqlite3.connect(ruta_db)
    cursor = conn.cursor()
    
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS estaciones (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            ide INTEGER NOT NULL,
            sFe TEXT NOT NULL,
            sHo TEXT NOT NULL,
            MP REAL NOT NULL,
            P10 REAL NOT NULL,
            nTe REAL NOT NULL,
            nHr REAL NOT NULL,
            nPa REAL NOT NULL,
            timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
        )
    ''')
    
    conn.commit()
    print(f"✓ Base de datos creada: {ruta_db}")
    return conn


def llenar_datos(conn: sqlite3.Connection, num_registros: int):
    """Llena la base de datos con datos de ejemplo"""
    cursor = conn.cursor()
    generador = GeneradorDatos()
    ahora = datetime.now()
    
    print(f"\nGenerando {num_registros} registros por estación...")
    
    for i in range(num_registros):
        fecha_hora = ahora - timedelta(seconds=(num_registros - i))
        
        for id_est in range(1, 11):
            est = generador.generar(id_est)
            
            cursor.execute('''
                INSERT INTO estaciones (ide, sFe, sHo, MP, P10, nTe, nHr, nPa, timestamp)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (est.ide, est.sFe, est.sHo, est.MP, est.P10, est.nTe, est.nHr, est.nPa, fecha_hora))
    
    conn.commit()
    total = cursor.execute("SELECT COUNT(*) FROM estaciones").fetchone()[0]
    print(f"✓ {total} registros insertados")


if __name__ == "__main__":
    print("=" * 60)
    print("CREAR BASE DE DATOS")
    print("=" * 60 + "\n")
    
    ruta = input("Ruta de BD (default: Lazarus/clima.db): ").strip()
    if not ruta:
        ruta = "Lazarus/clima.db"
    
    conn = crear_base_datos(ruta)
    
    resp = input("\n¿Llenar con datos de prueba? (s/n): ").lower()
    if resp == 's':
        num = input("Registros por estación (default: 100): ").strip()
        num = int(num) if num else 100
        llenar_datos(conn, num)
    
    conn.close()
    print("\n✓ Completado")
