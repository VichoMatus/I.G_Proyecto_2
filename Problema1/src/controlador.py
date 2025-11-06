"""
Controlador Principal del Sistema de Monitoreo
Coordina el envío de datos de múltiples estaciones
"""

import time
from datetime import datetime
from src.services.generador import GeneradorDatos
from src.services.cliente_http import ClienteHTTP


class ControladorMonitoreo:
    """Controla el ciclo de envío de datos de estaciones"""
    
    def __init__(self, url_servidor: str, num_estaciones: int = 10, intervalo: float = 1.0):
        self.url_servidor = url_servidor
        self.num_estaciones = num_estaciones
        self.intervalo = intervalo
        self.cliente = ClienteHTTP(url_servidor)
        self.generador = GeneradorDatos()
        self.ciclos = 0
        
        self._validar_parametros()
    
    def _validar_parametros(self):
        """Valida los parámetros de configuración"""
        if not (1 <= self.num_estaciones <= 10):
            raise ValueError("num_estaciones debe estar entre 1 y 10")
        if self.intervalo <= 0:
            raise ValueError("intervalo debe ser mayor a 0")
    
    def iniciar(self):
        """Inicia el sistema de monitoreo"""
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
        print("Cliente HTTP - Sistema de Monitoreo Ambiental")
        print("Empresa: Tengo Cara de Pepino S.A.")
        print("=" * 80)
        print(f"Servidor: {self.url_servidor}")
        print(f"Estaciones: {self.num_estaciones}")
        print(f"Frecuencia: {self.intervalo}s")
        print("=" * 80)
        print("Presiona Ctrl+C para detener\n")
    
    def _ejecutar_ciclo(self):
        """Ejecuta el ciclo principal de envío"""
        while True:
            self.ciclos += 1
            inicio = time.time()
            
            print(f"\n--- Ciclo #{self.ciclos} - {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} ---")
            
            # Envía datos de todas las estaciones secuencialmente
            for id_estacion in range(1, self.num_estaciones + 1):
                estacion = self.generador.generar(id_estacion)
                self.cliente.enviar(estacion)
            
            # Ajusta tiempo de espera para mantener frecuencia exacta
            tiempo_usado = time.time() - inicio
            espera = max(0, self.intervalo - tiempo_usado)
            
            if espera > 0:
                time.sleep(espera)
            elif tiempo_usado > self.intervalo:
                print(f"⚠ Ciclo tardó {tiempo_usado:.2f}s (>{self.intervalo}s)")
    
    def _finalizar(self):
        """Finaliza el sistema y muestra estadísticas"""
        print("\n" + "=" * 80)
        print("Sistema detenido")
        print(f"Ciclos completados: {self.ciclos}")
        
        stats = self.cliente.obtener_estadisticas()
        print(f"Envíos exitosos: {stats['exitosos']}")
        print(f"Envíos fallidos: {stats['fallidos']}")
        print(f"Tasa de éxito: {stats['tasa_exito']:.1f}%")
        print("=" * 80)
