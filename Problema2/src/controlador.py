"""
Controlador Principal - Sistema de Comunicación con Lazarus
Copia imágenes periódicamente para que Lazarus las detecte y muestre
"""

import time
import shutil
import os
from datetime import datetime
from pathlib import Path
from src.services.gestor_imagenes import GestorImagenes


class ControladorImagenes:
    """Controla el envío periódico de imágenes"""
    
    def __init__(self, carpeta_img: str = 'img', intervalo: float = 1.0):
        self.carpeta_img = carpeta_img
        self.intervalo = intervalo
        
        # Inicializa servicios
        self.gestor = GestorImagenes(carpeta_img)
        
        # Configurar carpeta para comunicación con Lazarus
        self.carpeta_lazarus = self._configurar_carpeta_lazarus()
        
        self.ciclos = 0
        self.imagenes_enviadas = 0
    
    def iniciar(self):
        """Inicia el sistema de envío de imágenes"""
        self._mostrar_banner()
        
        try:
            self._ejecutar_ciclo()
        except KeyboardInterrupt:
            self._finalizar()
        except Exception as e:
            print(f"\n✗ Error inesperado: {e}")
            self._finalizar()
    
    def _mostrar_banner(self):
        """Muestra información inicial"""
        print("=" * 80)
        print("Sistema de Comunicación Python → Lazarus")
        print("Empresa: Aquí te espero gallito Ltda")
        print("=" * 80)
        print(f"📂 Carpeta imágenes origen: {self.carpeta_img}")
        print(f"📁 Carpeta para Lazarus: {self.carpeta_lazarus}")
        print(f"🖼 Total imágenes disponibles: {self.gestor.total_imagenes()}")
        print(f"⏱ Frecuencia: {self.intervalo}s por imagen")
        print("=" * 80)
        print("Imágenes disponibles:")
        for img in self.gestor.obtener_lista():
            print(f"  - {img}")
        print("=" * 80)
        print("Presiona Ctrl+C para detener\n")
    
    def _ejecutar_ciclo(self):
        """Ejecuta el ciclo principal de envío"""
        while True:
            self.ciclos += 1
            inicio = time.time()
            
            print(f"--- Copia #{self.ciclos} - {datetime.now().strftime('%H:%M:%S')} ---")
            
            # Selecciona una imagen al azar
            imagen = self.gestor.obtener_imagen_aleatoria()
            
            # Copia la imagen para que Lazarus la detecte
            if self._copiar_para_lazarus(imagen):
                self.imagenes_enviadas += 1
                print(f"✓ Enviada a Lazarus: {imagen}")
            else:
                print(f"✗ Error enviando: {imagen}")
            
            # Ajusta tiempo de espera para mantener frecuencia exacta de 1 segundo
            tiempo_usado = time.time() - inicio
            espera = max(0, self.intervalo - tiempo_usado)
            
            if espera > 0:
                time.sleep(espera)
            elif tiempo_usado > self.intervalo:
                print(f"⚠ Envío tardó {tiempo_usado:.2f}s (>{self.intervalo}s)")
    
    def _finalizar(self):
        """Finaliza el sistema y muestra estadísticas"""
        print("\n" + "=" * 80)
        print("🛑 Sistema detenido")
        print(f"📊 Total de copias realizadas: {self.ciclos}")
        print(f"✅ Imágenes enviadas a Lazarus: {self.imagenes_enviadas}")
        print(f"❌ Copias fallidas: {self.ciclos - self.imagenes_enviadas}")
        
        if self.ciclos > 0:
            tasa_exito = (self.imagenes_enviadas / self.ciclos) * 100
            print(f"📈 Tasa de éxito: {tasa_exito:.1f}%")
        
        print("=" * 80)
    
    def _configurar_carpeta_lazarus(self):
        """Configura la carpeta para comunicación con Lazarus"""
        # Usar carpeta temporal en C: para evitar problemas de rutas largas
        carpeta_lazarus = Path("C:/temp/lazarus_imgs")
        
        # Crear carpeta si no existe
        carpeta_lazarus.mkdir(parents=True, exist_ok=True)
        
        return carpeta_lazarus
    
    def _copiar_para_lazarus(self, nombre_imagen):
        """Copia una imagen para que Lazarus la detecte"""
        try:
            origen = Path(self.carpeta_img) / nombre_imagen
            
            if not origen.exists():
                print(f"⚠ Imagen no encontrada: {origen}")
                return False
            
            # Crear nombre único con timestamp
            timestamp = int(time.time() * 1000)  # Milisegundos para evitar colisiones
            nombre_unico = f"{timestamp}_{nombre_imagen}"
            destino = self.carpeta_lazarus / nombre_unico
            
            # Copiar archivo
            shutil.copy2(origen, destino)
            
            # Actualizar tiempo de modificación para que Lazarus lo detecte como nuevo
            os.utime(destino, None)  # Establece tiempo actual
            
            return True
            
        except Exception as e:
            print(f"⚠ Error copiando: {e}")
            return False
