program ServidorMonitoreo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, 
  fphttpapp, httpdefs, httproute, fpjson, jsonparser;

type
  { Clase simple para manejar el servidor HTTP }
  TServidorMonitoreo = class
  private
    FContadorDatos: Integer;
    FDatosRecibidos: TStringList; // Almacena los datos en memoria en lugar de SQLite
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ProcesarDatosJSON(const DatosJSON: String);
    procedure MostrarEstadisticas;
  end;

var
  Servidor: TServidorMonitoreo;

{ Implementación de TServidorMonitoreo }

constructor TServidorMonitoreo.Create;
begin
  inherited Create;
  FContadorDatos := 0;
  FDatosRecibidos := TStringList.Create;
  
  WriteLn('=============================================================');
  WriteLn('Servidor HTTP - Sistema de Monitoreo Ambiental');
  WriteLn('Empresa: Tengo Cara de Pepino S.A.');
  WriteLn('=============================================================');
  WriteLn('Servidor inicializado correctamente');
  WriteLn('Base de datos: En memoria (sin SQLite por compatibilidad)');
end;

destructor TServidorMonitoreo.Destroy;
begin
  FDatosRecibidos.Free;
  inherited Destroy;
end;

procedure TServidorMonitoreo.ProcesarDatosJSON(const DatosJSON: String);
var
  Parser: TJSONParser;
  JSONObject: TJSONObject;
  ide: Integer;
  sFe, sHo: String;
  MP, P10, nTe, nHr, nPa: Double;
  DatosTexto: String;
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
        
        // Almacena los datos en memoria como texto
        DatosTexto := Format('Est:%d|Fecha:%s|Hora:%s|Temp:%.2f|HR:%.2f|PA:%.2f|MP:%.2f|P10:%.2f',
                           [ide, sFe, sHo, nTe, nHr, nPa, MP, P10]);
        FDatosRecibidos.Add(DatosTexto);
        
        // Incrementa contador
        Inc(FContadorDatos);
        
        // Muestra información en consola
        WriteLn(Format('[%d] Estación %d | Temp: %.2f°C | HR: %.2f%% | PA: %.2f hPa | %s',
                      [FContadorDatos, ide, nTe, nHr, nPa, sHo]));
        
        // Cada 10 datos, muestra estadísticas
        if (FContadorDatos mod 10) = 0 then
          MostrarEstadisticas;
        
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

procedure TServidorMonitoreo.MostrarEstadisticas;
var
  i, j: Integer;
  ContadorPorEstacion: array[1..10] of Integer;
begin
  WriteLn('');
  WriteLn('--- ESTADÍSTICAS ---');
  WriteLn('Total datos recibidos: ', FContadorDatos);
  
  // Inicializa contadores
  for i := 1 to 10 do
    ContadorPorEstacion[i] := 0;
  
  // Cuenta datos por estación (simplificado)
  for j := 0 to FDatosRecibidos.Count - 1 do
  begin
    for i := 1 to 10 do
    begin
      if Pos('Est:' + IntToStr(i) + '|', FDatosRecibidos[j]) > 0 then
      begin
        Inc(ContadorPorEstacion[i]);
        Break;
      end;
    end;
  end;
  
  // Muestra estadísticas por estación
  for i := 1 to 10 do
    WriteLn(Format('Estación %2d: %3d datos', [i, ContadorPorEstacion[i]]));
  
  WriteLn('');
end;

{ Manejador de ruta HTTP POST /datos }
procedure ManejadorDatos(ARequest: TRequest; AResponse: TResponse);
var
  DatosJSON: String;
begin
  if ARequest.Method = 'POST' then
  begin
    DatosJSON := ARequest.Content;
    
    if DatosJSON <> '' then
    begin
      Servidor.ProcesarDatosJSON(DatosJSON);
      
      AResponse.Code := 200;
      AResponse.Content := '{"status": "ok", "message": "Datos recibidos"}';
      AResponse.ContentType := 'application/json';
    end
    else
    begin
      AResponse.Code := 400;
      AResponse.Content := '{"status": "error", "message": "No se recibieron datos"}';
      AResponse.ContentType := 'application/json';
    end;
  end
  else
  begin
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
      Application.Port := 8080;
      Application.Title := 'Servidor Monitoreo';
      Application.Threaded := True;
      
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
