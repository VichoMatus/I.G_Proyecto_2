unit UMonitoreoController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  UEstacionModel, UEstacionRepository, UHTTPService, UChartService;

type
  { TLogEvent }
  TLogEvent = procedure(const AMensaje: String) of object;
  
  { TMonitoreoController }
  { Controlador principal - Coordina servicios y flujo de datos }
  TMonitoreoController = class
  private
    FRepository: TEstacionRepository;
    FHTTPService: THTTPService;
    FChartService: TChartService;
    FOnLog: TLogEvent;
    FContadorExportaciones: Integer;
    FEstacionesVisibles: array[1..10] of Boolean;
    
    { Manejadores de eventos }
    procedure OnDatoRecibido(AEstacion: TEstacionMonitoreo);
    procedure OnHTTPLog(Sender: TObject);
    
    { Utilidades }
    procedure RegistrarLog(const AMensaje: String);
    function GenerarNombreExportacion: String;
  public
    constructor Create(ARepository: TEstacionRepository; 
                       AHTTPService: THTTPService;
                       AChartService: TChartService);
    destructor Destroy; override;
    
    { Control del servidor }
    procedure IniciarServidor;
    procedure DetenerServidor;
    function ServidorActivo: Boolean;
    
    { Gestión de visualización }
    procedure MostrarEstacion(AEstacionId: Integer; AVisible: Boolean);
    function EstacionVisible(AEstacionId: Integer): Boolean;
    procedure LimpiarGraficos;
    
    { Exportación }
    function ExportarGrafico: String;
    
    { Estadísticas }
    function ObtenerTotalRegistros: Integer;
    function ObtenerRegistrosPorEstacion(AEstacionId: Integer): Integer;
    
    { Eventos }
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  DateUtils;

{ TMonitoreoController }

constructor TMonitoreoController.Create(ARepository: TEstacionRepository;
  AHTTPService: THTTPService; AChartService: TChartService);
var
  i: Integer;
begin
  inherited Create;
  
  FRepository := ARepository;
  FHTTPService := AHTTPService;
  FChartService := AChartService;
  FContadorExportaciones := 0;
  
  // Inicializar estaciones visibles
  for i := 1 to 10 do
    FEstacionesVisibles[i] := True;
  
  // Configurar eventos
  FHTTPService.OnDatoRecibido := @OnDatoRecibido;
  FHTTPService.OnLog := @OnHTTPLog;
  
  RegistrarLog('Controlador inicializado');
end;

destructor TMonitoreoController.Destroy;
begin
  if ServidorActivo then
    DetenerServidor;
  
  RegistrarLog('Controlador finalizado');
  inherited Destroy;
end;

procedure TMonitoreoController.IniciarServidor;
begin
  try
    FHTTPService.Iniciar;
    RegistrarLog(Format('Servidor HTTP iniciado en puerto %d', [FHTTPService.Port]));
  except
    on E: Exception do
      RegistrarLog('Error al iniciar servidor: ' + E.Message);
  end;
end;

procedure TMonitoreoController.DetenerServidor;
begin
  try
    FHTTPService.Detener;
    RegistrarLog('Servidor HTTP detenido');
  except
    on E: Exception do
      RegistrarLog('Error al detener servidor: ' + E.Message);
  end;
end;

function TMonitoreoController.ServidorActivo: Boolean;
begin
  Result := FHTTPService.EstaActivo;
end;

procedure TMonitoreoController.OnDatoRecibido(AEstacion: TEstacionMonitoreo);
begin
  try
    // Guardar en base de datos
    if FRepository.Guardar(AEstacion) then
    begin
      RegistrarLog(Format('✓ Est.%d | T:%.1f°C | HR:%.1f%% | PA:%.2fhPa | %s',
        [AEstacion.Ide, AEstacion.NTe, AEstacion.NHr, AEstacion.NPa, AEstacion.SHo]));
      
      // Actualizar gráfico si la estación es visible
      if FEstacionesVisibles[AEstacion.Ide] then
        FChartService.AgregarPunto(AEstacion.Ide, AEstacion.NTe);
    end
    else
      RegistrarLog(Format('✗ Error guardando datos de estación %d', [AEstacion.Ide]));
  except
    on E: Exception do
      RegistrarLog('Error procesando datos: ' + E.Message);
  end;
end;

procedure TMonitoreoController.OnHTTPLog(Sender: TObject);
begin
  if Assigned(FHTTPService) then
    RegistrarLog(FHTTPService.UltimoMensaje);
end;

procedure TMonitoreoController.RegistrarLog(const AMensaje: String);
var
  MensajeCompleto: String;
begin
  MensajeCompleto := FormatDateTime('hh:nn:ss', Now) + ' - ' + AMensaje;
  
  if Assigned(FOnLog) then
    FOnLog(MensajeCompleto);
end;

procedure TMonitoreoController.MostrarEstacion(AEstacionId: Integer; AVisible: Boolean);
begin
  if (AEstacionId >= 1) and (AEstacionId <= 10) then
  begin
    FEstacionesVisibles[AEstacionId] := AVisible;
    FChartService.MostrarSerie(AEstacionId, AVisible);
    
    if AVisible then
      RegistrarLog(Format('Estación %d: VISIBLE', [AEstacionId]))
    else
      RegistrarLog(Format('Estación %d: OCULTA', [AEstacionId]));
  end;
end;

function TMonitoreoController.EstacionVisible(AEstacionId: Integer): Boolean;
begin
  Result := False;
  if (AEstacionId >= 1) and (AEstacionId <= 10) then
    Result := FEstacionesVisibles[AEstacionId];
end;

procedure TMonitoreoController.LimpiarGraficos;
begin
  FChartService.LimpiarTodo;
  RegistrarLog('Gráficos limpiados');
end;

function TMonitoreoController.ExportarGrafico: String;
begin
  Inc(FContadorExportaciones);
  Result := GenerarNombreExportacion;
  
  try
    if FChartService.ExportarAPNG(Result) then
      RegistrarLog(Format('Gráfico exportado: %s', [ExtractFileName(Result)]))
    else
    begin
      RegistrarLog('Error: No se pudo exportar el gráfico');
      Result := '';
    end;
  except
    on E: Exception do
    begin
      RegistrarLog('Error exportando: ' + E.Message);
      Result := '';
    end;
  end;
end;

function TMonitoreoController.GenerarNombreExportacion: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + 
            Format('grafico_%3.3d.png', [FContadorExportaciones]);
end;

function TMonitoreoController.ObtenerTotalRegistros: Integer;
begin
  try
    Result := FRepository.ContarRegistros;
  except
    Result := 0;
  end;
end;

function TMonitoreoController.ObtenerRegistrosPorEstacion(AEstacionId: Integer): Integer;
begin
  try
    Result := FRepository.ContarPorEstacion(AEstacionId);
  except
    Result := 0;
  end;
end;

end.
