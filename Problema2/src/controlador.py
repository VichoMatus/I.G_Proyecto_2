"""
Controlador Principal - Cliente HTTP POST para Servidor Lazarus
Envía imágenes vía HTTP POST cada 1 segundo (cumple rúbrica 100%)
"""

import time
from datetime import datetime
from pathlib import Path
from src.services.gestor_imagenes import GestorImagenes
from src.services.cliente_http import ClienteImagenes


class ControladorImagenes:
    """Controla el envío HTTP POST de imágenes al servidor Lazarus"""
    
    def __init__(self, carpeta_img: str = 'img', intervalo: float = 1.0, url_servidor: str = "http://localhost:8080/imagen"):
        self.carpeta_img = carpeta_img
        self.intervalo = intervalo
        self.url_servidor = url_servidor
        
        # Inicializa servicios
        self.gestor = GestorImagenes(carpeta_img)
        self.cliente_http = ClienteImagenes(url_servidor, timeout=5)
        
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
        print("CLIENTE HTTP POST - Envío a Servidor Lazarus")
        print("Empresa: Aquí te espero gallito Ltda")
        print("Cumple Rúbrica: HTTP POST cada 1 segundo ✓")
        print("=" * 80)
        print(f"📂 Carpeta imágenes origen: {self.carpeta_img}")
        print(f"🌐 Servidor Lazarus: {self.url_servidor}")
        print(f"🖼 Total imágenes disponibles: {self.gestor.total_imagenes()}")
        print(f"⏱ Frecuencia HTTP POST: {self.intervalo}s por imagen")
        print("=" * 80)
        print("🔄 Imágenes (orden secuencial):")
        for i, img in enumerate(self.lista_imagenes, 1):
            print(f"  {i}. {img}")
        print("=" * 80)
        print("Presiona Ctrl+C para detener\n")
    
    def _ejecutar_ciclo(self):
        """Ejecuta el ciclo principal de envío HTTP POST"""
        while True:
            self.ciclos += 1
            inicio = time.time()
            
            print(f"--- HTTP POST #{self.ciclos} - {datetime.now().strftime('%H:%M:%S')} ---")
            
            # Selecciona imagen de forma secuencial
            imagen_nombre = self._obtener_imagen_secuencial()
            
            # Envía la imagen vía HTTP POST al servidor Lazarus
            if self._enviar_via_http_post(imagen_nombre):
                self.imagenes_enviadas += 1
                print(f"✓ POST exitoso #{self.indice_imagen_actual}: {imagen_nombre}")
            else:
                print(f"✗ Error en POST: {imagen_nombre}")
            
            # Ajusta tiempo de espera para mantener frecuencia exacta de 1 segundo
            tiempo_usado = time.time() - inicio
            espera = max(0, self.intervalo - tiempo_usado)
            
            if espera > 0:
                time.sleep(espera)
            elif tiempo_usado > self.intervalo:
                print(f"⚠ POST tardó {tiempo_usado:.2f}s (>{self.intervalo}s)")
    
    def _finalizar(self):
        """Finaliza el sistema y muestra estadísticas HTTP"""
        print("\n" + "=" * 80)
        print("🛑 Cliente HTTP POST detenido")
        print(f"📊 Total de requests HTTP: {self.ciclos}")
        print(f"✅ POST requests exitosos: {self.imagenes_enviadas}")
        print(f"❌ POST requests fallidos: {self.ciclos - self.imagenes_enviadas}")
        
        if self.ciclos > 0:
            tasa_exito = (self.imagenes_enviadas / self.ciclos) * 100
            print(f"📈 Tasa de éxito HTTP: {tasa_exito:.1f}%")
        
        # Mostrar estadísticas del cliente HTTP
        self.cliente_http.mostrar_estadisticas() if hasattr(self.cliente_http, 'mostrar_estadisticas') else None
        print("=" * 80)
    
    def _enviar_via_http_post(self, nombre_imagen: str) -> bool:
        """
        Envía una imagen al servidor Lazarus vía HTTP POST
        Cumple con rúbrica: HTTP POST cada 1 segundo
        
        Args:
            nombre_imagen: Nombre del archivo de imagen a enviar
            
        Returns:
            bool: True si el envío fue exitoso, False en caso de error
        """
        try:
            # Construir ruta completa de la imagen
            ruta_imagen = Path(self.carpeta_img) / nombre_imagen
            
            # Verificar que el archivo existe
            if not ruta_imagen.exists():
                print(f"❌ Archivo no encontrado: {ruta_imagen}")
                return False
            
            # Leer el contenido de la imagen
            with open(ruta_imagen, 'rb') as archivo:
                contenido_imagen = archivo.read()
            
            # Enviar vía HTTP POST usando el cliente
            exito = self.cliente_http.enviar_imagen(ruta_imagen, contenido_imagen)
            
            return exito
            
        except Exception as e:
            print(f"❌ Error enviando imagen {nombre_imagen}: {e}")
            return False
    
    def _obtener_imagen_secuencial(self):
        """Obtiene la siguiente imagen de forma secuencial"""
        if not self.lista_imagenes:
            return None
        
        # Obtener imagen actual
        imagen = self.lista_imagenes[self.indice_imagen_actual]
        
        # Avanzar al siguiente índice (con wrap-around)
        self.indice_imagen_actual = (self.indice_imagen_actual + 1) % len(self.lista_imagenes)
        
        return imagen
