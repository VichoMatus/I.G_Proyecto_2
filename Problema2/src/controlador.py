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
        # Probar primero carpeta local para evitar problemas de permisos
        carpeta_base = Path(self.carpeta_img).parent
        carpeta_lazarus = carpeta_base / "recibidas"
        
        try:
            carpeta_lazarus.mkdir(exist_ok=True)
            print(f"📁 Carpeta Lazarus creada: {carpeta_lazarus}")
            return carpeta_lazarus
        except Exception as e:
            print(f"⚠ Error creando carpeta local: {e}")
            # Fallback a carpeta temporal
            carpeta_temp = Path("C:/temp/imgs")
            carpeta_temp.mkdir(parents=True, exist_ok=True)
            print(f"📁 Usando carpeta temporal: {carpeta_temp}")
            return carpeta_temp
    
    def _copiar_para_lazarus(self, nombre_imagen):
        """Copia una imagen para que Lazarus la detecte"""
        try:
            # Convertir a strings absolutos para debug
            origen_str = str(Path(self.carpeta_img) / nombre_imagen)
            
            print(f"🔍 DEBUG - Origen: {origen_str}")
            print(f"🔍 DEBUG - Destino base: {self.carpeta_lazarus}")
            
            if not os.path.exists(origen_str):
                print(f"⚠ Imagen no encontrada: {origen_str}")
                return False
            
            # Crear nombre muy simple sin caracteres especiales
            timestamp = int(time.time())
            nombre_simple = f"img_{timestamp}.jpg"
            destino_str = str(self.carpeta_lazarus / nombre_simple)
            
            print(f"🔍 DEBUG - Destino final: {destino_str}")
            
            # Usar shutil.copy con strings simples
            shutil.copy(origen_str, destino_str)
            
            # Verificar que se copió
            if os.path.exists(destino_str):
                print(f"✅ Copiado exitosamente: {nombre_simple}")
                return True
            else:
                print(f"❌ Archivo no se creó: {destino_str}")
                return False
            
        except Exception as e:
            print(f"⚠ Error copiando: {e}")
            print(f"⚠ Tipo de error: {type(e).__name__}")
            return False
