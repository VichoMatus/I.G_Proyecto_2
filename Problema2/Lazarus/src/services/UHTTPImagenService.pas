unit UHTTPImagenService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  UImagenModel;

type
  { TImagenRecibidaEvent }
  TImagenRecibidaEvent = procedure(AImagen: TImagenRecibida) of object;
  
  { THTTPImagenThread }
  THTTPImagenThread = class(TThread)
  private
    FServer: TFPHTTPServer;
    FPort: Word;
    FOnImagenRecibida: TImagenRecibidaEvent;
    FUltimoMensaje: String;
    FImagenPendiente: TImagenRecibida;
    
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure NotificarImagenRecibida;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Word);
    destructor Destroy; override;
    
    procedure Detener;
    
    property OnImagenRecibida: TImagenRecibidaEvent read FOnImagenRecibida write FOnImagenRecibida;
    property UltimoMensaje: String read FUltimoMensaje;
  end;
  
  { THTTPImagenService }
  { Servicio HTTP - Maneja recepción de imágenes via POST }
  THTTPImagenService = class
  private
    FThread: THTTPImagenThread;
    FPort: Word;
    FActivo: Boolean;
    FOnImagenRecibida: TImagenRecibidaEvent;
    FUltimoMensaje: String;
    
    procedure OnThreadImagenRecibida(AImagen: TImagenRecibida);
  public
    constructor Create(APort: Word = 8080);
    destructor Destroy; override;
    
    procedure Iniciar;
    procedure Detener;
    function EstaActivo: Boolean;
    
    property Port: Word read FPort;
    property OnImagenRecibida: TImagenRecibidaEvent read FOnImagenRecibida write FOnImagenRecibida;
    property UltimoMensaje: String read FUltimoMensaje;
  end;

implementation

{ THTTPImagenThread }

constructor THTTPImagenThread.Create(APort: Word);
begin
  inherited Create(True); // Crear suspendido
  FreeOnTerminate := False;
  FPort := APort;
  FServer := nil;
  FImagenPendiente := nil;
end;

destructor THTTPImagenThread.Destroy;
begin
  if Assigned(FServer) then
  begin
    FServer.Active := False;
    FServer.Free;
  end;
  inherited Destroy;
end;

procedure THTTPImagenThread.NotificarImagenRecibida;
begin
  if Assigned(FOnImagenRecibida) and Assigned(FImagenPendiente) then
  begin
    FOnImagenRecibida(FImagenPendiente);
    FImagenPendiente := nil;
  end;
end;

procedure THTTPImagenThread.Execute;
begin
  try
    FServer := TFPHTTPServer.Create(nil);
    FServer.Port := FPort;
    FServer.OnRequest := @HandleRequest;
    FServer.Active := True;
    
    FUltimoMensaje := Format('Servidor HTTP iniciado en puerto %d', [FPort]);
    
    // Mantener el servidor activo
    while not Terminated do
    begin
      Sleep(100);
    end;
  except
    on E: Exception do
      FUltimoMensaje := 'Error en servidor HTTP: ' + E.Message;
  end;
end;

procedure THTTPImagenThread.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Imagen: TImagenRecibida;
  ImagenStream: TMemoryStream;
  Boundary, ContentType: String;
  PartStart, PartEnd: Integer;
  RawData: String;
begin
  try
    // Solo aceptar POST
    if ARequest.Method <> 'POST' then
    begin
      AResponse.Code := 405;
      AResponse.Content := 'Método no permitido. Use POST.';
      Exit;
    end;
    
    // Verificar endpoint /imagen
    if ARequest.URI <> '/imagen' then
    begin
      AResponse.Code := 404;
      AResponse.Content := 'Endpoint no encontrado. Use /imagen';
      Exit;
    end;
    
    // Crear imagen desde datos recibidos
    Imagen := TImagenRecibida.Create;
    try
      ImagenStream := TMemoryStream.Create;
      try
        // Obtener Content-Type para verificar si es multipart
        ContentType := ARequest.ContentType;
        
        if Pos('multipart/form-data', ContentType) > 0 then
        begin
          // Extraer boundary del Content-Type
          PartStart := Pos('boundary=', ContentType);
          if PartStart > 0 then
          begin
            Boundary := '--' + Copy(ContentType, PartStart + 9, Length(ContentType));
            
            // Obtener datos raw
            RawData := ARequest.Content;
            
            // Buscar inicio de datos binarios (después de headers del part)
            // Los datos de imagen empiezan después de dos saltos de línea (#13#10#13#10)
            PartStart := Pos(#13#10#13#10, RawData);
            if PartStart > 0 then
            begin
              Inc(PartStart, 4); // Saltar los 4 caracteres
              
              // Buscar final de datos (boundary final)
              PartEnd := Pos(#13#10 + Boundary, RawData, PartStart);
              if PartEnd > 0 then
              begin
                // Extraer datos binarios
                ImagenStream.Write(RawData[PartStart], PartEnd - PartStart);
                ImagenStream.Position := 0;
              end;
            end;
          end;
        end
        else
        begin
          // Datos directos (raw binary)
          if ARequest.Content <> '' then
          begin
            ImagenStream.Write(ARequest.Content[1], Length(ARequest.Content));
            ImagenStream.Position := 0;
          end;
        end;
        
        if ImagenStream.Size > 0 then
        begin
          Imagen.CargarDesdeDatos(ImagenStream);
          Imagen.NombreArchivo := Format('img_%s.jpg', [FormatDateTime('yyyymmdd_hhnnsszzz', Now)]);
          
          // Notificar imagen recibida (sincronizado con el hilo principal)
          if Assigned(FOnImagenRecibida) then
          begin
            FImagenPendiente := Imagen;
            Synchronize(@NotificarImagenRecibida);
          end
          else
          begin
            Imagen.Free;
          end;
          
          FUltimoMensaje := Format('Imagen recibida: %d bytes', [ImagenStream.Size]);
          
          AResponse.Code := 200;
          AResponse.Content := '{"status":"ok","message":"Imagen recibida"}';
        end
        else
        begin
          Imagen.Free;
          AResponse.Code := 400;
          AResponse.Content := '{"status":"error","message":"No se recibieron datos de imagen"}';
        end;
      finally
        ImagenStream.Free;
      end;
    except
      on E: Exception do
      begin
        Imagen.Free;
        AResponse.Code := 500;
        AResponse.Content := Format('{"status":"error","message":"%s"}', [E.Message]);
      end;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := 'Error interno del servidor';
    end;
  end;
end;

procedure THTTPImagenThread.Detener;
begin
  Terminate;
  if Assigned(FServer) then
    FServer.Active := False;
end;

{ THTTPImagenService }

constructor THTTPImagenService.Create(APort: Word = 8080);
begin
  inherited Create;
  FPort := APort;
  FActivo := False;
  FThread := nil;
end;

destructor THTTPImagenService.Destroy;
begin
  Detener;
  inherited Destroy;
end;

procedure THTTPImagenService.Iniciar;
begin
  if FActivo then
    Exit;
  
  try
    FThread := THTTPImagenThread.Create(FPort);
    FThread.OnImagenRecibida := @OnThreadImagenRecibida;
    FThread.Start;
    FActivo := True;
    FUltimoMensaje := Format('Servidor iniciado en puerto %d', [FPort]);
  except
    on E: Exception do
    begin
      FActivo := False;
      FUltimoMensaje := 'Error al iniciar: ' + E.Message;
      raise;
    end;
  end;
end;

procedure THTTPImagenService.Detener;
begin
  if not FActivo then
    Exit;
  
  try
    if Assigned(FThread) then
    begin
      FThread.Detener;
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;
    end;
    FActivo := False;
    FUltimoMensaje := 'Servidor detenido';
  except
    on E: Exception do
      FUltimoMensaje := 'Error al detener: ' + E.Message;
  end;
end;

function THTTPImagenService.EstaActivo: Boolean;
begin
  Result := FActivo;
end;

procedure THTTPImagenService.OnThreadImagenRecibida(AImagen: TImagenRecibida);
begin
  if Assigned(FOnImagenRecibida) then
    FOnImagenRecibida(AImagen);
end;

end.
