unit UChartService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAGraph, TASeries,
  UEstacionModel;

type
  { TChartService }
  { Servicio de gráficos - Maneja visualización en TChart }
  TChartService = class
  private
    FChart: TChart;
    FSeries: array[1..10] of TLineSeries;
    FMaxPuntos: Integer;
    FColores: array[1..10] of TColor;
    
    procedure InicializarColores;
  public
    constructor Create(AChart: TChart);
    destructor Destroy; override;
    
    { Configuración }
    procedure Inicializar;
    procedure ConfigurarEjes(const ATituloX, ATituloY: String);
    
    { Manejo de datos }
    procedure AgregarPunto(AEstacionId: Integer; AValor: Double);
    procedure LimpiarSerie(AEstacionId: Integer);
    procedure LimpiarTodo;
    
    { Visibilidad }
    procedure MostrarSerie(AEstacionId: Integer; AVisible: Boolean);
    function SerieEsVisible(AEstacionId: Integer): Boolean;
    
    { Exportación }
    function ExportarAPNG(const ANombreArchivo: String): Boolean;
    
    { Propiedades }
    property MaxPuntos: Integer read FMaxPuntos write FMaxPuntos;
  end;

implementation

uses
  FPImage, FPCanvas, FPWritePNG;

{ TChartService }

constructor TChartService.Create(AChart: TChart);
begin
  inherited Create;
  FChart := AChart;
  FMaxPuntos := 50;
  InicializarColores;
end;

destructor TChartService.Destroy;
begin
  // Las series se liberan automáticamente con el Chart
  inherited Destroy;
end;

procedure TChartService.InicializarColores;
begin
  FColores[1] := clRed;
  FColores[2] := clBlue;
  FColores[3] := clGreen;
  FColores[4] := clFuchsia;
  FColores[5] := clMaroon;
  FColores[6] := clNavy;
  FColores[7] := clOlive;
  FColores[8] := clPurple;
  FColores[9] := clTeal;
  FColores[10] := clLime;
end;

procedure TChartService.Inicializar;
var
  i: Integer;
begin
  // Limpiar series existentes
  FChart.ClearSeries;
  
  // Configurar título
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add('Monitoreo de Temperatura - Estaciones Ambientales');
  FChart.Title.Font.Style := [fsBold];
  FChart.Title.Font.Size := 12;
  
  // Configurar ejes
  ConfigurarEjes('Tiempo', 'Temperatura (°C)');
  
  // Configurar leyenda
  FChart.Legend.Visible := True;
  
  // Crear series para cada estación
  for i := 1 to 10 do
  begin
    FSeries[i] := TLineSeries.Create(FChart);
    FSeries[i].Title := Format('Estación %d', [i]);
    FSeries[i].SeriesColor := FColores[i];
    FSeries[i].ShowPoints := True;
    FSeries[i].LinePen.Width := 2;
    FChart.AddSeries(FSeries[i]);
  end;
end;

procedure TChartService.ConfigurarEjes(const ATituloX, ATituloY: String);
begin
  FChart.BottomAxis.Title.Caption := ATituloX;
  FChart.BottomAxis.Title.LabelFont.Style := [fsBold];
  
  FChart.LeftAxis.Title.Caption := ATituloY;
  FChart.LeftAxis.Title.LabelFont.Style := [fsBold];
  FChart.LeftAxis.Title.LabelFont.Orientation := 900;
end;

procedure TChartService.AgregarPunto(AEstacionId: Integer; AValor: Double);
var
  X: Double;
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  // Agregar punto con X incremental
  X := FSeries[AEstacionId].Count;
  FSeries[AEstacionId].AddXY(X, AValor);
  
  // Implementar Left Scrolling - eliminar puntos antiguos
  if FSeries[AEstacionId].Count > FMaxPuntos then
    FSeries[AEstacionId].Delete(0);
  
  FChart.Invalidate;
end;

procedure TChartService.LimpiarSerie(AEstacionId: Integer);
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  FSeries[AEstacionId].Clear;
  FChart.Invalidate;
end;

procedure TChartService.LimpiarTodo;
var
  i: Integer;
begin
  for i := 1 to 10 do
    FSeries[i].Clear;
  FChart.Invalidate;
end;

procedure TChartService.MostrarSerie(AEstacionId: Integer; AVisible: Boolean);
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  FSeries[AEstacionId].Active := AVisible;
  FChart.Invalidate;
end;

function TChartService.SerieEsVisible(AEstacionId: Integer): Boolean;
begin
  Result := False;
  if (AEstacionId >= 1) and (AEstacionId <= 10) then
    Result := FSeries[AEstacionId].Active;
end;

function TChartService.ExportarAPNG(const ANombreArchivo: String): Boolean;
begin
  Result := False;
  try
    FChart.SaveToFile(TPortableNetworkGraphic, ANombreArchivo);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

end.
