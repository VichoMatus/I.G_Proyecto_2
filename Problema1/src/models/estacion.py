"""
Modelo de Datos - Estación de Monitoreo
Representa una estación de monitoreo ambiental con sus mediciones
"""

from dataclasses import dataclass
from datetime import datetime
from typing import Dict


@dataclass
class EstacionMonitoreo:
    """
    Modelo de datos para una estación de monitoreo ambiental
    Representa las mediciones ambientales en un momento específico
    """
    ide: int  # ID de estación (1-10)
    sFe: str  # Fecha sistema (YYYY-MM-DD HH:MM:SS)
    sHo: str  # Hora sistema (HH:MM:SS)
    MP: float  # Material Particulado (μg/m³)
    P10: float  # Material Particulado 10 (μg/m³)
    nTe: float  # Temperatura (°C)
    nHr: float  # Humedad Relativa (%)
    nPa: float  # Presión Atmosférica (hPa)
    
    def to_dict(self) -> Dict:
        """Convierte el modelo a diccionario para JSON"""
        return {
            'ide': self.ide,
            'sFe': self.sFe,
            'sHo': self.sHo,
            'MP': self.MP,
            'P10': self.P10,
            'nTe': self.nTe,
            'nHr': self.nHr,
            'nPa': self.nPa
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'EstacionMonitoreo':
        """Crea una instancia desde un diccionario"""
        return cls(
            ide=data['ide'],
            sFe=data['sFe'],
            sHo=data['sHo'],
            MP=data['MP'],
            P10=data['P10'],
            nTe=data['nTe'],
            nHr=data['nHr'],
            nPa=data['nPa']
        )
    
    def validar(self) -> bool:
        """Valida que los datos sean correctos"""
        if not (1 <= self.ide <= 10):
            return False
        if self.MP < 0 or self.P10 < 0:
            return False
        if not (0 <= self.nHr <= 100):
            return False
        if not self.sFe or not self.sHo:
            return False
        return True
