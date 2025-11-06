"""
Gestor de Imágenes
Maneja la lectura y selección de imágenes desde la carpeta img/
"""

import os
import random
from pathlib import Path
from typing import List, Optional


class GestorImagenes:
    """Gestiona las imágenes de la carpeta img/"""
    
    EXTENSIONES_VALIDAS = ['.jpg', '.jpeg', '.png', '.bmp', '.gif']
    
    def __init__(self, carpeta_img: str = 'img'):
        self.carpeta_img = Path(carpeta_img)
        self.imagenes: List[Path] = []
        self._cargar_imagenes()
    
    def _cargar_imagenes(self):
        """Carga la lista de imágenes disponibles"""
        if not self.carpeta_img.exists():
            raise FileNotFoundError(f"Carpeta no encontrada: {self.carpeta_img}")
        
        # Busca archivos con extensiones válidas
        self.imagenes = [
            f for f in self.carpeta_img.iterdir()
            if f.is_file() and f.suffix.lower() in self.EXTENSIONES_VALIDAS
        ]
        
        if not self.imagenes:
            raise ValueError(f"No hay imágenes en: {self.carpeta_img}")
        
        print(f"✓ {len(self.imagenes)} imagen(es) encontrada(s) en {self.carpeta_img}")
    
    def obtener_imagen_aleatoria(self) -> Path:
        """Retorna una imagen al azar de la carpeta"""
        return random.choice(self.imagenes)
    
    def leer_imagen(self, ruta: Path) -> bytes:
        """Lee el contenido binario de una imagen"""
        with open(ruta, 'rb') as f:
            return f.read()
    
    def obtener_lista(self) -> List[str]:
        """Retorna la lista de nombres de imágenes"""
        return [img.name for img in self.imagenes]
    
    def total_imagenes(self) -> int:
        """Retorna el total de imágenes disponibles"""
        return len(self.imagenes)
