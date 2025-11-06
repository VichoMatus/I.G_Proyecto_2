program ServidorMonitoreo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fphttpapp, httpdefs, httproute, fpjson, jsonparser, 
  sqldb, sqlite3conn, TAGraph, TASeries, TADrawerCanvas, Graphics, FPImage, 
  FPCanvas, FPWritePNG;

type
  { Clase para manejar el servidor HTTP }
  TServidorMonitoreo = class
  private
    FConexionDB: TSQLite3Connection;  // Conexión a la base de datos SQLite
    FTransaccion: TSQLTransaction;    // Transacción para operaciones DB
    FQuery: TSQLQuery;                // Query para ejecutar SQL
    FChart: TChart;                   // Componente para gráfico
    FContadorExportacion: Integer;    // Contador para nombres de PNG
    
    procedure InicializarBaseDatos;   // Crea la base de datos y tablas
    procedure InicializarGrafico;     // Configura el gráfico TChart
    procedure AgregarDatoGrafico(IDEstacion: Integer; Temperatura: Double);  // Agrega punto al gráfico
    procedure ExportarGrafico;        // Exporta el gráfico a PNG
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ProcesarDatosJSON(const DatosJSON: String);  // Procesa JSON recibido
    procedure ActualizarSeriesGrafico(EstacionesVisibles: array of Integer);  // Actualiza qué estaciones mostrar
  end;

var
  Servidor: TServidorMonitoreo;

{ Implementación de TServidorMonitoreo }

constructor TServidorMonitoreo.Create;
begin
  inherited Create;
  FContadorExportacion := 0;
  
  // Inicializa conexión a base de datos
  FConexionDB := TSQLite3Connection.Create(nil);
  FTransaccion := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  
  FConexionDB.DatabaseName := 'clima.db';  // Nombre de la base de datos
  FConexionDB.Transaction := FTransaccion;
  FTransaccion.DataBase := FConexionDB;
  FQuery.Database := FConexionDB;
  
  InicializarBaseDatos;
  InicializarGrafico;
  
  WriteLn('Servidor inicializado correctamente');
end;

destructor TServidorMonitoreo.Destroy;
begin
  // Libera recursos
  FQuery.Free;
  FTransaccion.Free;
  FConexionDB.Free;
  FChart.Free;
  inherited Destroy;
end;

procedure TServidorMonitoreo.InicializarBaseDatos;
var
  SQL: String;
begin
  try
    FConexionDB.Open;  // Abre la conexión a la base de datos
    
    // Crea la tabla estaciones si no existe
    SQL := 'CREATE TABLE IF NOT EXISTS estaciones (' +
           'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
           'ide INTEGER NOT NULL, ' +              // ID Estación (1-10)
           'sFe TEXT NOT NULL, ' +                 // Fecha Sistema
           'sHo TEXT NOT NULL, ' +                 // Hora Sistema
           'MP REAL NOT NULL, ' +                  // Material Particulado
           'P10 REAL NOT NULL, ' +                 // Material Particulado 10
           'nTe REAL NOT NULL, ' +                 // Temperatura
           'nHr REAL NOT NULL, ' +                 // Humedad Relativa
           'nPa REAL NOT NULL, ' +                 // Presión Atmosférica
           'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)';  // Timestamp automático
    
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    FTransaccion.Commit;
    
    WriteLn('Base de datos inicializada: clima.db');
  except
    on E: Exception do
      WriteLn('Error al inicializar base de datos: ', E.Message);
  end;
end;

procedure TServidorMonitoreo.InicializarGrafico;
var
  i: Integer;
  Serie: TLineSeries;
begin
  FChart := TChart.Create(nil);
  FChart.Title.Text.Text := 'Monitoreo Ambiental - Temperatura por Estación';
  FChart.LeftAxis.Title.Caption := 'Temperatura (°C)';
  FChart.BottomAxis.Title.Caption := 'Tiempo (muestras)';
  
  // Crea una serie de línea para cada estación (1 a 10)
  for i := 1 to 10 do
  begin
    Serie := TLineSeries.Create(FChart);
    Serie.Title := 'Estación ' + IntToStr(i);
    Serie.ShowPoints := True;  // Muestra puntos en la línea
    FChart.AddSeries(Serie);
  end;
  
  WriteLn('Gráfico inicializado con 10 series');
end;

procedure TServidorMonitoreo.AgregarDatoGrafico(IDEstacion: Integer; Temperatura: Double);
var
  Serie: TLineSeries;
  MaxPuntos: Integer;
begin
  MaxPuntos := 50;  // Número máximo de puntos visibles (Left Scrolling)
  
  if (IDEstacion >= 1) and (IDEstacion <= 10) then
  begin
    Serie := TLineSeries(FChart.Series[IDEstacion - 1]);
    
    // Agrega el nuevo punto
    Serie.AddXY(Serie.Count, Temperatura);
    
    // Si excede el máximo, elimina el primer punto (Left Scrolling)
    if Serie.Count > MaxPuntos then
      Serie.Delete(0);
  end;
end;

procedure TServidorMonitoreo.ExportarGrafico;
var
  NombreArchivo: String;
  Imagen: TFPMemoryImage;
  Escritor: TFPWriterPNG;
  Drawer: TFPImageCanvas;
begin
  try
    Inc(FContadorExportacion);
    NombreArchivo := Format('grafico_%4.4d.png', [FContadorExportacion]);
    
    // Crea imagen en memoria
    Imagen := TFPMemoryImage.Create(800, 600);
    try
      Drawer := TFPImageCanvas.Create(Imagen);
      try
        // Dibuja el gráfico en la imagen
        FChart.Draw(Drawer, Rect(0, 0, 800, 600));
        
        // Guarda como PNG
        Escritor := TFPWriterPNG.Create;
        try
          Imagen.SaveToFile(NombreArchivo, Escritor);
          WriteLn('Gráfico exportado: ', NombreArchivo);
        finally
          Escritor.Free;
        end;
      finally
        Drawer.Free;
      end;
    finally
      Imagen.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al exportar gráfico: ', E.Message);
  end;
end;

procedure TServidorMonitoreo.ProcesarDatosJSON(const DatosJSON: String);
var
  Parser: TJSONParser;
  JSONObject: TJSONObject;
  SQL: String;
  ide: Integer;
  sFe, sHo: String;
  MP, P10, nTe, nHr, nPa: Double;
begin
  try
    // Parsea el JSON recibido
    Parser := TJSONParser.Create(DatosJSON);
    try
      JSONObject := TJSONObject(Parser.Parse);
      try
        // Extrae los valores del JSON
        ide := JSONObject.Get('ide', 0);
        sFe := JSONObject.Get('sFe', '');
        sHo := JSONObject.Get('sHo', '');
        MP := JSONObject.Get('MP', 0.0);
        P10 := JSONObject.Get('P10', 0.0);
        nTe := JSONObject.Get('nTe', 0.0);
        nHr := JSONObject.Get('nHr', 0.0);
        nPa := JSONObject.Get('nPa', 0.0);
        
        // Inserta los datos en la base de datos
        SQL := Format('INSERT INTO estaciones (ide, sFe, sHo, MP, P10, nTe, nHr, nPa) ' +
                      'VALUES (%d, ''%s'', ''%s'', %.2f, %.2f, %.2f, %.2f, %.2f)',
                      [ide, sFe, sHo, MP, P10, nTe, nHr, nPa]);
        
        FQuery.SQL.Text := SQL;
        FQuery.ExecSQL;
        FTransaccion.Commit;
        
        // Agrega el dato al gráfico
        AgregarDatoGrafico(ide, nTe);
        
        WriteLn(Format('Datos guardados - Estación %d | Temp: %.2f°C', [ide, nTe]));
        
      finally
        JSONObject.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al procesar JSON: ', E.Message);
  end;
end;

procedure TServidorMonitoreo.ActualizarSeriesGrafico(EstacionesVisibles: array of Integer);
var
  i, j: Integer;
  Visible: Boolean;
begin
  // Actualiza qué series están visibles en el gráfico
  for i := 1 to 10 do
  begin
    Visible := False;
    for j := Low(EstacionesVisibles) to High(EstacionesVisibles) do
    begin
      if EstacionesVisibles[j] = i then
      begin
        Visible := True;
        Break;
      end;
    end;
    FChart.Series[i - 1].Active := Visible;  // Activa o desactiva la serie
  end;
  
  ExportarGrafico;  // Exporta el gráfico actualizado
end;

{ Manejador de ruta HTTP POST /datos }
procedure ManejadorDatos(ARequest: TRequest; AResponse: TResponse);
var
  DatosJSON: String;
begin
  if ARequest.Method = 'POST' then  // Solo acepta método POST
  begin
    DatosJSON := ARequest.Content;  // Obtiene el contenido JSON del request
    
    if DatosJSON <> '' then
    begin
      Servidor.ProcesarDatosJSON(DatosJSON);  // Procesa los datos
      
      // Responde con código 200 OK
      AResponse.Code := 200;
      AResponse.Content := '{"status": "ok", "message": "Datos recibidos"}';
      AResponse.ContentType := 'application/json';
    end
    else
    begin
      // Responde con error 400 si no hay datos
      AResponse.Code := 400;
      AResponse.Content := '{"status": "error", "message": "No se recibieron datos"}';
      AResponse.ContentType := 'application/json';
    end;
  end
  else
  begin
    // Responde con error 405 si el método no es POST
    AResponse.Code := 405;
    AResponse.Content := '{"status": "error", "message": "Método no permitido"}';
    AResponse.ContentType := 'application/json';
  end;
end;

{ Programa principal }
begin
  try
    WriteLn('=============================================================');
    WriteLn('Servidor HTTP - Sistema de Monitoreo Ambiental');
    WriteLn('Empresa: Tengo Cara de Pepino S.A.');
    WriteLn('=============================================================');
    
    // Crea la instancia del servidor
    Servidor := TServidorMonitoreo.Create;
    try
      // Registra la ruta /datos
      HTTPRouter.RegisterRoute('/datos', @ManejadorDatos);
      
      // Configura el servidor HTTP
      Application.Port := 8080;  // Puerto del servidor
      Application.Title := 'Servidor Monitoreo';
      Application.Threaded := True;  // Permite múltiples conexiones
      
      WriteLn('Servidor iniciado en http://localhost:8080');
      WriteLn('Esperando datos en: http://localhost:8080/datos');
      WriteLn('Presiona Ctrl+C para detener');
      WriteLn('=============================================================');
      
      // Inicia el servidor (bloqueante)
      Application.Run;
      
    finally
      Servidor.Free;
    end;
    
  except
    on E: Exception do
      WriteLn('Error fatal: ', E.Message);
  end;
end.
