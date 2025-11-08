unit UHTTPService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  UEstacionModel;

type
  { TDatoRecibidoEvent }
  TDatoRecibidoEvent = procedure(AEstacion: TEstacionMonitoreo) of object;
  
  { THTTPService }
  { Servicio HTTP - Maneja servidor y peticiones }
  THTTPService = class
  private
    FServer: TFPHTTPServer;
    FActive: Boolean;
    FPort: Word;
    FOnDatoRecibido: TDatoRecibidoEvent;
    FOnLog: TNotifyEvent;
    FUltimoMensaje: String;
    
    procedure HandleRequest(Sender: TObject; 
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure ProcesarDatos(const AData: String);
  public
    constructor Create(APort: Word = 8080);
    destructor Destroy; override;
    
    { Control del servidor }
    procedure Iniciar;
    procedure Detener;
    
    { Estado }
    function EstaActivo: Boolean;
    
    { Propiedades }
    property Port: Word read FPort write FPort;
    property Active: Boolean read FActive;
    property UltimoMensaje: String read FUltimoMensaje;
    
    { Eventos }
    property OnDatoRecibido: TDatoRecibidoEvent read FOnDatoRecibido write FOnDatoRecibido;
    property OnLog: TNotifyEvent read FOnLog write FOnLog;
  end;

implementation

uses
  fpjson, jsonparser;

{ THTTPService }

constructor THTTPService.Create(APort: Word = 8080);
begin
  inherited Create;
  FPort := APort;
  FActive := False;
  FOnDatoRecibido := nil;
  FOnLog := nil;
  FUltimoMensaje := '';
end;

destructor THTTPService.Destroy;
begin
  Detener;
  inherited Destroy;
end;

procedure THTTPService.Iniciar;
begin
  if FActive then
    Exit;
  
  try
    FServer := TFPHTTPServer.Create(nil);
    FServer.Port := FPort;
    FServer.OnRequest := @HandleRequest;
    FServer.Threaded := True;
    FServer.Active := True;
    
    FActive := True;
    FUltimoMensaje := Format('Servidor HTTP iniciado en puerto %d', [FPort]);
    
    if Assigned(FOnLog) then
      FOnLog(Self);
  except
    on E: Exception do
    begin
      FActive := False;
      FUltimoMensaje := 'Error al iniciar servidor: ' + E.Message;
      if Assigned(FOnLog) then
        FOnLog(Self);
      raise;
    end;
  end;
end;

procedure THTTPService.Detener;
begin
  if not FActive then
    Exit;
  
  try
    if Assigned(FServer) then
    begin
      FServer.Active := False;
      FreeAndNil(FServer);
    end;
    
    FActive := False;
    FUltimoMensaje := 'Servidor HTTP detenido';
    
    if Assigned(FOnLog) then
      FOnLog(Self);
  except
    on E: Exception do
    begin
      FUltimoMensaje := 'Error al detener servidor: ' + E.Message;
      if Assigned(FOnLog) then
        FOnLog(Self);
    end;
  end;
end;

function THTTPService.EstaActivo: Boolean;
begin
  Result := FActive and Assigned(FServer) and FServer.Active;
end;

procedure THTTPService.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  PostData: String;
begin
  AResponse.ContentType := 'application/json';
  
  // Endpoint POST /datos
  if (ARequest.Method = 'POST') and (ARequest.URI = '/datos') then
  begin
    try
      PostData := ARequest.Content;
      ProcesarDatos(PostData);
      
      AResponse.Code := 200;
      AResponse.Content := '{"status": "ok", "message": "Datos recibidos"}';
    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := Format('{"status": "error", "message": "%s"}', [E.Message]);
        FUltimoMensaje := 'Error procesando datos: ' + E.Message;
        if Assigned(FOnLog) then
          FOnLog(Self);
      end;
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"status": "error", "message": "Endpoint no encontrado"}';
  end;
end;

procedure THTTPService.ProcesarDatos(const AData: String);
var
  Estacion: TEstacionMonitoreo;
begin
  Estacion := TEstacionMonitoreo.Create;
  try
    if not Estacion.FromJSON(AData) then
    begin
      FUltimoMensaje := 'Error: JSON inválido';
      if Assigned(FOnLog) then
        FOnLog(Self);
      Exit;
    end;
    
    if not Estacion.Validar then
    begin
      FUltimoMensaje := 'Error: Datos inválidos - ' + Estacion.ObtenerErrores;
      if Assigned(FOnLog) then
        FOnLog(Self);
      Exit;
    end;
    
    // Notificar datos recibidos
    if Assigned(FOnDatoRecibido) then
      FOnDatoRecibido(Estacion);
    
    FUltimoMensaje := Format('Datos recibidos: Estación %d - Temp: %.1f°C', 
      [Estacion.Ide, Estacion.NTe]);
    
    if Assigned(FOnLog) then
      FOnLog(Self);
  finally
    Estacion.Free;
  end;
end;

end.
