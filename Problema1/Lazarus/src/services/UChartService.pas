unit UChartService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAGraph, TASeries,
  UEstacionModel;

type
  { TChartService }
  { Servicio de gráficos - Maneja 5 variables por estación en un solo gráfico }
  TChartService = class
  private
    FChart: TChart;
    // Series: [Variable 1-5, Estación 1-10]
    // Variable: 1=Temp, 2=Hum, 3=Pres, 4=MP, 5=P10
    FSeries: array[1..5, 1..10] of TLineSeries;
    FMaxPuntos: Integer;
    FColores: array[1..5] of TColor;
    FEstacionActiva: Integer;
    
    procedure InicializarColores;
  public
    constructor Create(AChart: TChart);
    destructor Destroy; override;
    
    { Configuración }
    procedure Inicializar;
    
    { Manejo de datos }
    procedure AgregarPuntos(AEstacionId: Integer; ATemp, AHum, APres, AMP, AP10: Double);
    procedure LimpiarSerie(AEstacionId: Integer);
    procedure LimpiarTodo;
    
    { Visibilidad }
    procedure MostrarEstacion(AEstacionId: Integer);
    function EstacionVisible: Integer;
    
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
  FEstacionActiva := 1;
  InicializarColores;
end;

destructor TChartService.Destroy;
begin
  // Las series se liberan automáticamente con el Chart
  inherited Destroy;
end;

procedure TChartService.InicializarColores;
begin
  FColores[1] := clRed;      // Temperatura
  FColores[2] := clBlue;     // Humedad
  FColores[3] := clGreen;    // Presión
  FColores[4] := clFuchsia;  // Material Particulado
  FColores[5] := clMaroon;   // P10
end;

procedure TChartService.Inicializar;
var
  i, j: Integer;
  NombresSeries: array[1..5] of String;
begin
  // Limpiar series existentes
  FChart.ClearSeries;
  
  // Configurar título
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add('Datos de la Estación Seleccionada');
  FChart.Title.Font.Style := [fsBold];
  FChart.Title.Font.Size := 12;
  
  // Configurar ejes
  FChart.BottomAxis.Title.Caption := 'Tiempo';
  FChart.BottomAxis.Title.LabelFont.Style := [fsBold];
  FChart.LeftAxis.Title.Caption := 'Valores';
  FChart.LeftAxis.Title.LabelFont.Style := [fsBold];
  FChart.LeftAxis.Title.LabelFont.Orientation := 900;
  
  // Configurar leyenda
  FChart.Legend.Visible := True;
  
  // Nombres de las series
  NombresSeries[1] := 'Temperatura (°C)';
  NombresSeries[2] := 'Humedad (%)';
  NombresSeries[3] := 'Presión (hPa)';
  NombresSeries[4] := 'Mat. Particulado';
  NombresSeries[5] := 'P10';
  
  // Crear 5 series para cada estación (50 series totales)
  for i := 1 to 5 do  // Variables
  begin
    for j := 1 to 10 do  // Estaciones
    begin
      FSeries[i, j] := TLineSeries.Create(FChart);
      FSeries[i, j].Title := Format('E%d - %s', [j, NombresSeries[i]]);
      FSeries[i, j].SeriesColor := FColores[i];
      FSeries[i, j].ShowPoints := True;
      FSeries[i, j].LinePen.Width := 2;
      FSeries[i, j].Active := False; // Por defecto ocultas
      FChart.AddSeries(FSeries[i, j]);
    end;
  end;
  
  // Mostrar solo la estación 1 por defecto
  MostrarEstacion(1);
end;

procedure TChartService.AgregarPuntos(AEstacionId: Integer; ATemp, AHum, APres, AMP, AP10: Double);
var
  X: Double;
  i: Integer;
  Valores: array[1..5] of Double;
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  // Solo actualizar si es la estación activa
  if AEstacionId <> FEstacionActiva then
    Exit;
  
  // Asignar valores
  Valores[1] := ATemp;
  Valores[2] := AHum;
  Valores[3] := APres;
  Valores[4] := AMP;
  Valores[5] := AP10;
  
  // Agregar puntos a las 5 series de la estación
  for i := 1 to 5 do
  begin
    X := FSeries[i, AEstacionId].Count;
    FSeries[i, AEstacionId].AddXY(X, Valores[i]);
    
    // Implementar Left Scrolling
    if FSeries[i, AEstacionId].Count > FMaxPuntos then
      FSeries[i, AEstacionId].Delete(0);
  end;
  
  // Repintar el gráfico
  FChart.Repaint;
end;

procedure TChartService.LimpiarSerie(AEstacionId: Integer);
var
  i: Integer;
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  for i := 1 to 5 do
    FSeries[i, AEstacionId].Clear;
  
  FChart.Invalidate;
end;

procedure TChartService.LimpiarTodo;
var
  i, j: Integer;
begin
  for i := 1 to 5 do
    for j := 1 to 10 do
      FSeries[i, j].Clear;
  
  FChart.Invalidate;
end;

procedure TChartService.MostrarEstacion(AEstacionId: Integer);
var
  i, j: Integer;
begin
  if (AEstacionId < 1) or (AEstacionId > 10) then
    Exit;
  
  FEstacionActiva := AEstacionId;
  
  // Ocultar todas las series
  for i := 1 to 5 do
    for j := 1 to 10 do
      FSeries[i, j].Active := False;
  
  // Mostrar solo las 5 series de la estación seleccionada
  for i := 1 to 5 do
    FSeries[i, AEstacionId].Active := True;
  
  FChart.Invalidate;
end;

function TChartService.EstacionVisible: Integer;
begin
  Result := FEstacionActiva;
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
