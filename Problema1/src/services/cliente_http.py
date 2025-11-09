"""
Cliente HTTP para envío de datos
Maneja la comunicación con el servidor
"""

import requests
from typing import Optional, List
from concurrent.futures import ThreadPoolExecutor, as_completed
from src.models.estacion import EstacionMonitoreo


class ClienteHTTP:
    """Cliente HTTP para enviar datos al servidor"""
    
    def __init__(self, url_servidor: str, timeout: float = 2.0):
        self.url_servidor = url_servidor
        self.timeout = timeout
        self.exitosos = 0
        self.fallidos = 0
    
    def enviar(self, estacion: EstacionMonitoreo) -> bool:
        """
        Envía datos de una estación al servidor
        Retorna True si exitoso, False si falla
        """
        if not estacion.validar():
            print(f"✗ Error: Datos inválidos para estación {estacion.ide}")
            self.fallidos += 1
            return False
        
        try:
            headers = {'Content-Type': 'application/json'}
            response = requests.post(
                self.url_servidor,
                json=estacion.to_dict(),
                headers=headers,
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                self._log_exito(estacion)
                self.exitosos += 1
                return True
            else:
                self._log_error(estacion, f"Status {response.status_code}")
                self.fallidos += 1
                return False
                
        except requests.exceptions.ConnectionError:
            self._log_error(estacion, "Error de conexión")
            self.fallidos += 1
            return False
        except requests.exceptions.Timeout:
            self._log_error(estacion, "Timeout")
            self.fallidos += 1
            return False
        except Exception as e:
            self._log_error(estacion, str(e))
            self.fallidos += 1
            return False
    
    def _log_exito(self, est: EstacionMonitoreo):
        """Registra envío exitoso"""
        print(f"✓ Est.{est.ide:2d} | T:{est.nTe:5.2f}°C | "
              f"HR:{est.nHr:5.2f}% | PA:{est.nPa:7.2f}hPa | {est.sHo}")
    
    def _log_error(self, est: EstacionMonitoreo, mensaje: str):
        """Registra error de envío"""
        print(f"✗ Est.{est.ide:2d} | {mensaje}")
    
    def enviar_lote(self, estaciones: List[EstacionMonitoreo]) -> None:
        """
        Envía múltiples estaciones con paralelismo controlado
        """
        import time
        with ThreadPoolExecutor(max_workers=3) as executor:
            # Enviar en grupos de 3 para no saturar
            for i in range(0, len(estaciones), 3):
                lote = estaciones[i:i+3]
                list(executor.map(self.enviar, lote))
                time.sleep(0.01)  # Pequeña pausa entre lotes
    
    def obtener_estadisticas(self) -> dict:
        """Retorna estadísticas de envíos"""
        total = self.exitosos + self.fallidos
        tasa = (self.exitosos / total * 100) if total > 0 else 0
        return {
            'exitosos': self.exitosos,
            'fallidos': self.fallidos,
            'total': total,
            'tasa_exito': tasa
        }
