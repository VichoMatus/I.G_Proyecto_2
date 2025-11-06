"""
Validador de Datos
Valida estructuras JSON y datos de estaciones
"""

from typing import Dict, Any


class Validador:
    """Valida datos y estructuras JSON"""
    
    CAMPOS_REQUERIDOS = ['ide', 'sFe', 'sHo', 'MP', 'P10', 'nTe', 'nHr', 'nPa']
    
    @staticmethod
    def validar_estructura_json(datos: Dict[str, Any]) -> bool:
        """Valida que el JSON tenga la estructura correcta"""
        # Verifica campos requeridos
        for campo in Validador.CAMPOS_REQUERIDOS:
            if campo not in datos:
                return False
        
        try:
            # Valida tipos y rangos
            if not isinstance(datos['ide'], int):
                return False
            if not (1 <= datos['ide'] <= 10):
                return False
            if not isinstance(datos['sFe'], str) or not datos['sFe']:
                return False
            if not isinstance(datos['sHo'], str) or not datos['sHo']:
                return False
            if not isinstance(datos['MP'], (int, float)) or datos['MP'] < 0:
                return False
            if not isinstance(datos['P10'], (int, float)) or datos['P10'] < 0:
                return False
            if not isinstance(datos['nTe'], (int, float)):
                return False
            if not isinstance(datos['nHr'], (int, float)):
                return False
            if not (0 <= datos['nHr'] <= 100):
                return False
            if not isinstance(datos['nPa'], (int, float)):
                return False
            
            return True
        except:
            return False
