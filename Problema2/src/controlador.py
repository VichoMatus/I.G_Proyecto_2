"""
Controlador Principal - Sistema de Comunicación con Lazarus
Copia imágenes periódicamente para que Lazarus las detecte y muestre
"""

import time
import shutil
import os
import glob
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
        
        # Limpiar carpeta de imágenes previas
        self._limpiar_carpeta_lazarus()
        
        # Control de envío secuencial
        self.indice_imagen_actual = 0
        self.lista_imagenes = self.gestor.obtener_lista()
        
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
        print("🔄 Imágenes (orden secuencial):")
        for i, img in enumerate(self.lista_imagenes, 1):
            print(f"  {i}. {img}")
        print("=" * 80)
        print("Presiona Ctrl+C para detener\n")
    
    def _ejecutar_ciclo(self):
        """Ejecuta el ciclo principal de envío"""
        while True:
            self.ciclos += 1
            inicio = time.time()
            
            print(f"--- Copia #{self.ciclos} - {datetime.now().strftime('%H:%M:%S')} ---")
            
            # Selecciona imagen de forma secuencial
            imagen = self._obtener_imagen_secuencial()
            
            # Copia la imagen para que Lazarus la detecte
            if self._copiar_para_lazarus(imagen):
                self.imagenes_enviadas += 1
                print(f"✓ Enviada secuencial #{self.indice_imagen_actual}: {imagen}")
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
            # Convertir a strings absolutos
            origen_str = str(Path(self.carpeta_img) / nombre_imagen)
            
            if not os.path.exists(origen_str):
                print(f"⚠ Imagen no encontrada: {origen_str}")
                return False
            
            # Crear nombre simple con contador secuencial
            nombre_simple = f"img_{self.ciclos:04d}.jpg"  # img_0001.jpg, img_0002.jpg, etc.
            destino_str = str(self.carpeta_lazarus / nombre_simple)
            
            # Usar shutil.copy con strings simples
            shutil.copy(origen_str, destino_str)
            
            # Verificar que se copió
            if os.path.exists(destino_str):
                return True
            else:
                print(f"❌ Archivo no se creó: {destino_str}")
                return False
            
        except Exception as e:
            print(f"⚠ Error copiando: {e}")
            return False
    
    def _limpiar_carpeta_lazarus(self):
        """Limpia todas las imágenes previas de la carpeta de Lazarus"""
        try:
            # Buscar todos los archivos de imagen
            patrones = ['*.jpg', '*.jpeg', '*.png', '*.bmp', '*.gif']
            archivos_eliminados = 0
            
            for patron in patrones:
                archivos = glob.glob(str(self.carpeta_lazarus / patron))
                for archivo in archivos:
                    try:
                        os.remove(archivo)
                        archivos_eliminados += 1
                    except Exception as e:
                        print(f"⚠ No se pudo eliminar {archivo}: {e}")
            
            if archivos_eliminados > 0:
                print(f"🧹 Limpieza: {archivos_eliminados} imagen(es) anterior(es) eliminada(s)")
            else:
                print(f"🧹 Carpeta ya estaba limpia")
                
        except Exception as e:
            print(f"⚠ Error limpiando carpeta: {e}")
    
    def _obtener_imagen_secuencial(self):
        """Obtiene la siguiente imagen de forma secuencial"""
        if not self.lista_imagenes:
            return None
        
        # Obtener imagen actual
        imagen = self.lista_imagenes[self.indice_imagen_actual]
        
        # Avanzar al siguiente índice (con wrap-around)
        self.indice_imagen_actual = (self.indice_imagen_actual + 1) % len(self.lista_imagenes)
        
        return imagen
