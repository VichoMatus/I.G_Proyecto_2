"""
Generador de Datos Ambientales
Genera datos simulados usando distribución gaussiana
"""

import random
from datetime import datetime
from src.models.estacion import EstacionMonitoreo


class GeneradorDatos:
    """Genera datos ambientales simulados para estaciones de monitoreo"""
    
    @staticmethod
    def obtener_fecha() -> str:
        """Retorna fecha y hora actual en formato YYYY-MM-DD HH:MM:SS"""
        return datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    @staticmethod
    def obtener_hora() -> str:
        """Retorna hora actual en formato HH:MM:SS"""
        return datetime.now().strftime('%H:%M:%S')
    
    @staticmethod
    def generar(id_estacion: int) -> EstacionMonitoreo:
        """
        Genera datos simulados para una estación
        Usa distribución gaussiana para valores realistas
        """
        if not (1 <= id_estacion <= 10):
            raise ValueError(f"ID debe estar entre 1-10, recibido: {id_estacion}")
        
        # Material Particulado: μ=10, σ=3
        mp = max(0, random.gauss(mu=10, sigma=3))
        
        # Material Particulado 10: μ=15, σ=4
        p10 = max(0, random.gauss(mu=15, sigma=4))
        
        # Temperatura: μ=20°C, σ=4
        temperatura = random.gauss(mu=20, sigma=4)
        
        # Humedad Relativa: μ=70%, σ=2, rango [0-100]
        humedad = max(0, min(100, random.gauss(mu=70, sigma=2)))
        
        # Presión Atmosférica: μ=1000 hPa, σ=10
        presion = random.gauss(mu=1000, sigma=10)
        
        return EstacionMonitoreo(
            ide=id_estacion,
            sFe=GeneradorDatos.obtener_fecha(),
            sHo=GeneradorDatos.obtener_hora(),
            MP=round(mp, 2),
            P10=round(p10, 2),
            nTe=round(temperatura, 2),
            nHr=round(humedad, 2),
            nPa=round(presion, 2)
        )
