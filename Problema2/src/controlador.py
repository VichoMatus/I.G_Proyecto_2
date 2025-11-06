"""
Controlador Principal - Sistema de Envío de Imágenes
Coordina el envío periódico de imágenes al servidor
"""

import time
from datetime import datetime
from src.services.gestor_imagenes import GestorImagenes
from src.services.cliente_http import ClienteImagenes


class ControladorImagenes:
    """Controla el envío periódico de imágenes"""
    
    def __init__(self, url_servidor: str, carpeta_img: str = 'img', intervalo: float = 1.0):
        self.url_servidor = url_servidor
        self.carpeta_img = carpeta_img
        self.intervalo = intervalo
        
        # Inicializa servicios
        self.gestor = GestorImagenes(carpeta_img)
        self.cliente = ClienteImagenes(url_servidor)
        
        self.ciclos = 0
    
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
        print("Cliente HTTP - Sistema de Envío de Imágenes")
        print("Empresa: Aquí te espero gallito Ltda")
        print("=" * 80)
        print(f"Servidor: {self.url_servidor}")
        print(f"Carpeta imágenes: {self.carpeta_img}")
        print(f"Total imágenes: {self.gestor.total_imagenes()}")
        print(f"Frecuencia: {self.intervalo}s por imagen")
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
            
            print(f"--- Envío #{self.ciclos} - {datetime.now().strftime('%H:%M:%S')} ---")
            
            # Selecciona una imagen al azar
            imagen = self.gestor.obtener_imagen_aleatoria()
            
            # Lee el contenido de la imagen
            contenido = self.gestor.leer_imagen(imagen)
            
            # Envía la imagen al servidor
            self.cliente.enviar_imagen(imagen, contenido)
            
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
        print("Sistema detenido")
        print(f"Total de envíos: {self.ciclos}")
        
        stats = self.cliente.obtener_estadisticas()
        print(f"Imágenes enviadas: {stats['enviadas']}")
        print(f"Envíos fallidos: {stats['fallidas']}")
        print(f"Tasa de éxito: {stats['tasa_exito']:.1f}%")
        print("=" * 80)
