unit UImagenController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  UImagenModel, UHTTPImagenService, UGridService;

type
  { TLogEvent }
  TLogEvent = procedure(const AMensaje: String) of object;
  
  { TImagenController }
  { Controlador principal - Coordina servicios HTTP y Grid }
  TImagenController = class
  private
    FHTTPService: UHTTPImagenService.THTTPImagenService;
    FGridService: TGridService;
    FOnLog: TLogEvent;
    
    procedure OnImagenRecibida(AImagen: TImagenRecibida);
    procedure RegistrarLog(const AMensaje: String);
  public
    constructor Create(AHTTPService: UHTTPImagenService.THTTPImagenService;
                       AGridService: TGridService);
    destructor Destroy; override;
    
    { Control del servidor }
    procedure IniciarServidor;
    procedure DetenerServidor;
    function ServidorActivo: Boolean;
    
    { Gestión del grid }
    procedure LimpiarGrid;
    
    { Estadísticas }
    function ObtenerTotalImagenes: Integer;
    
    { Eventos }
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

{ TImagenController }

constructor TImagenController.Create(AHTTPService: UHTTPImagenService.THTTPImagenService;
  AGridService: TGridService);
begin
  inherited Create;
  
  if not Assigned(AHTTPService) then
    raise Exception.Create('HTTPService no puede ser nil');
  if not Assigned(AGridService) then
    raise Exception.Create('GridService no puede ser nil');
  
  FHTTPService := AHTTPService;
  FGridService := AGridService;
  
  // Conectar evento
  FHTTPService.OnImagenRecibida := @OnImagenRecibida;
  
  RegistrarLog('Controlador inicializado');
end;

destructor TImagenController.Destroy;
begin
  RegistrarLog('Controlador finalizado');
  inherited Destroy;
end;

procedure TImagenController.IniciarServidor;
begin
  try
    FHTTPService.Iniciar;
    RegistrarLog(Format('Servidor HTTP iniciado en puerto %d', [FHTTPService.Port]));
  except
    on E: Exception do
      RegistrarLog('Error al iniciar servidor: ' + E.Message);
  end;
end;

procedure TImagenController.DetenerServidor;
begin
  try
    FHTTPService.Detener;
    RegistrarLog('Servidor HTTP detenido');
  except
    on E: Exception do
      RegistrarLog('Error al detener servidor: ' + E.Message);
  end;
end;

function TImagenController.ServidorActivo: Boolean;
begin
  Result := FHTTPService.EstaActivo;
end;

procedure TImagenController.OnImagenRecibida(AImagen: TImagenRecibida);
begin
  try
    // Mostrar imagen en el grid
    FGridService.MostrarImagen(AImagen);
    
    RegistrarLog(Format('Imagen #%d recibida y mostrada', [FGridService.ContadorImagenes]));
  except
    on E: Exception do
      RegistrarLog('Error procesando imagen: ' + E.Message);
  end;
  
  // Liberar la imagen (ya fue copiada al grid)
  AImagen.Free;
end;

procedure TImagenController.LimpiarGrid;
begin
  try
    FGridService.LimpiarGrid;
    RegistrarLog('Grid limpiado');
  except
    on E: Exception do
      RegistrarLog('Error limpiando grid: ' + E.Message);
  end;
end;

function TImagenController.ObtenerTotalImagenes: Integer;
begin
  Result := FGridService.ContadorImagenes;
end;

procedure TImagenController.RegistrarLog(const AMensaje: String);
var
  MensajeCompleto: String;
begin
  MensajeCompleto := FormatDateTime('hh:nn:ss', Now) + ' - ' + AMensaje;
  
  if Assigned(FOnLog) then
    FOnLog(MensajeCompleto);
end;

end.
