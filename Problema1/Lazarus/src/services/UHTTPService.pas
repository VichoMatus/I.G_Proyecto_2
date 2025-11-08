unit UHTTPService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  UEstacionModel;

type
  { TDatoRecibidoEvent }
  TDatoRecibidoEvent = procedure(AEstacion: TEstacionMonitoreo) of object;
  
  { THTTPServerThread }
  THTTPServerThread = class(TThread)
  private
    FServer: TFPHTTPServer;
    FPort: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word; AOnRequest: THTTPServerRequestHandler);
    destructor Destroy; override;
    procedure Stop;
  end;
  
  { THTTPService }
  { Servicio HTTP - Maneja servidor y peticiones }
  THTTPService = class
  private
    FServerThread: THTTPServerThread;
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

{ THTTPServerThread }

constructor THTTPServerThread.Create(APort: Word; AOnRequest: THTTPServerRequestHandler);
begin
  inherited Create(True); // Crear suspendido
  FreeOnTerminate := False;
  FPort := APort;
  
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := AOnRequest;
  FServer.Threaded := False; // El thread ya maneja la concurrencia
end;

destructor THTTPServerThread.Destroy;
begin
  Stop;
  if Assigned(FServer) then
    FreeAndNil(FServer);
  inherited Destroy;
end;

procedure THTTPServerThread.Execute;
begin
  try
    FServer.Active := True;
    while not Terminated do
    begin
      // El servidor procesa peticiones automáticamente
      Sleep(100);
    end;
  except
    // Silenciar excepciones del thread
  end;
end;

procedure THTTPServerThread.Stop;
begin
  Terminate;
  if Assigned(FServer) and FServer.Active then
    FServer.Active := False;
  WaitFor;
end;

{ THTTPService }

constructor THTTPService.Create(APort: Word = 8080);
begin
  inherited Create;
  FPort := APort;
  FActive := False;
  FServerThread := nil;
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
    FServerThread := THTTPServerThread.Create(FPort, @HandleRequest);
    FServerThread.Start;
    
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
      
      if Assigned(FServerThread) then
        FreeAndNil(FServerThread);
      
      raise;
    end;
  end;
end;

procedure THTTPService.Detener;
begin
  if not FActive then
    Exit;
  
  try
    if Assigned(FServerThread) then
    begin
      FServerThread.Stop;
      FreeAndNil(FServerThread);
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
  Result := FActive and Assigned(FServerThread);
end;

procedure THTTPService.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  PostData: String;
  Estacion: TEstacionMonitoreo;
begin
  AResponse.ContentType := 'application/json';
  
  // Endpoint POST /datos
  if (ARequest.Method = 'POST') and (ARequest.URI = '/datos') then
  begin
    try
      PostData := ARequest.Content;
      
      // Responder inmediatamente sin bloquear
      AResponse.Code := 200;
      AResponse.Content := '{"status": "ok"}';
      
      // Procesar datos en segundo plano (no bloqueante)
      try
        Estacion := TEstacionMonitoreo.Create;
        try
          if Estacion.FromJSON(PostData) and Estacion.Validar then
          begin
            if Assigned(FOnDatoRecibido) then
              FOnDatoRecibido(Estacion);
          end;
        finally
          Estacion.Free;
        end;
      except
        // Ignorar errores de procesamiento para no bloquear la respuesta
      end;
    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := Format('{"status": "error", "message": "%s"}', [E.Message]);
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
begin
  // Este método ya no se usa, el procesamiento se hace directamente en HandleRequest
end;

end.
