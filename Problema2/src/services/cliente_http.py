"""
Cliente HTTP para envío de imágenes
Envía imágenes al servidor Lazarus mediante POST
"""

import requests
from pathlib import Path
from typing import Optional


class ClienteImagenes:
    """Cliente HTTP para enviar imágenes al servidor"""
    
    def __init__(self, url_servidor: str, timeout: int = 10):
        self.url_servidor = url_servidor
        self.timeout = timeout
        self.enviadas = 0
        self.fallidas = 0
    
    def enviar_imagen(self, ruta_imagen: Path, contenido: bytes) -> bool:
        """
        Envía una imagen al servidor HTTP
        Retorna True si exitoso, False si falla
        """
        try:
            # Prepara el archivo para envío multipart/form-data
            files = {
                'imagen': (ruta_imagen.name, contenido, self._obtener_mime_type(ruta_imagen))
            }
            
            # Envía la imagen mediante POST
            response = requests.post(
                self.url_servidor,
                files=files,
                timeout=self.timeout
            )
            
            if response.status_code == 200:
                self._log_exito(ruta_imagen)
                self.enviadas += 1
                return True
            else:
                self._log_error(ruta_imagen, f"Status {response.status_code}")
                self.fallidas += 1
                return False
        
        except requests.exceptions.ConnectionError:
            self._log_error(ruta_imagen, "Error de conexión")
            self.fallidas += 1
            return False
        except requests.exceptions.Timeout:
            self._log_error(ruta_imagen, "Timeout")
            self.fallidas += 1
            return False
        except Exception as e:
            self._log_error(ruta_imagen, str(e))
            self.fallidas += 1
            return False
    
    def _obtener_mime_type(self, ruta: Path) -> str:
        """Retorna el tipo MIME según la extensión"""
        extensiones = {
            '.jpg': 'image/jpeg',
            '.jpeg': 'image/jpeg',
            '.png': 'image/png',
            '.bmp': 'image/bmp',
            '.gif': 'image/gif'
        }
        return extensiones.get(ruta.suffix.lower(), 'application/octet-stream')
    
    def _log_exito(self, ruta: Path):
        """Registra envío exitoso"""
        print(f"✓ Enviada: {ruta.name}")
    
    def _log_error(self, ruta: Path, mensaje: str):
        """Registra error de envío"""
        print(f"✗ Error {ruta.name}: {mensaje}")
    
    def obtener_estadisticas(self) -> dict:
        """Retorna estadísticas de envíos"""
        total = self.enviadas + self.fallidas
        tasa = (self.enviadas / total * 100) if total > 0 else 0
        return {
            'enviadas': self.enviadas,
            'fallidas': self.fallidas,
            'total': total,
            'tasa_exito': tasa
        }
