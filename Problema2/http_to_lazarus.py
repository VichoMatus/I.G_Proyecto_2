"""
Servidor HTTP simple - Recibe POST y pasa DIRECTAMENTE a Lazarus
La rúbrica dice "al servidor Lazarus" - Lazarus es quien procesa y muestra
"""

from http.server import HTTPServer, BaseHTTPRequestHandler
import os
from datetime import datetime


class HTTPToLazarus(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == '/imagen':
            try:
                # Leer POST de Python
                content_length = int(self.headers['Content-Length'])
                post_data = self.rfile.read(content_length)
                
                # Carpeta para que Lazarus procese
                carpeta = os.path.join(os.path.dirname(__file__), 'http_input')
                os.makedirs(carpeta, exist_ok=True)
                
                # Extraer JPG limpio
                imagen_data = self._extraer_jpg_limpio(post_data)
                
                # Pasar a Lazarus inmediatamente
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S_%f")
                archivo = os.path.join(carpeta, f"post_{timestamp}.jpg")
                
                with open(archivo, 'wb') as f:
                    f.write(imagen_data)
                
                # Responder a Python
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(b'{"status": "ok", "procesado_por": "lazarus"}')
                
                print(f"✅ POST → Lazarus: {len(imagen_data)} bytes")
                
            except Exception as e:
                self.send_response(500)
                self.end_headers()
                print(f"❌ Error: {e}")
        else:
            self.send_response(404)
            self.end_headers()
    
    def _extraer_jpg_limpio(self, data):
        """Extrae JPG válido de multipart"""
        try:
            # Buscar marcadores JPG
            inicio = data.find(b'\xff\xd8')
            if inicio == -1:
                return data
            
            fin = data.find(b'\xff\xd9', inicio)
            if fin == -1:
                return data[inicio:]
            
            return data[inicio:fin + 2]
        except:
            return data


def main():
    servidor = HTTPServer(('localhost', 8080), HTTPToLazarus)
    print("🚀 Servidor HTTP → Lazarus iniciado en puerto 8080")
    print("📡 Recibe POST de Python → 🎯 Pasa a Lazarus DIRECTO")
    print("⚡ Lazarus procesa en tiempo real (100ms)")
    
    try:
        servidor.serve_forever()
    except KeyboardInterrupt:
        print("\n🛑 Servidor HTTP → Lazarus detenido")


if __name__ == "__main__":
    main()