program ServidorMonitoreo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, Classes, SysUtils, StdCtrls, Controls, Graphics, ExtCtrls, LCLType;

type
  { Datos de cada estación }
  TDatoEstacion = record
    Timestamp: TDateTime;
    Temperatura: Double;
    Humedad: Double;
    Presion: Double;
  end;
  
  { Lista de datos por estación }
  TListaDatos = array of TDatoEstacion;

  { Formulario principal con gráfico visual }
  TFormMonitoreo = class(TForm)
  private
    FLabelsEstacion: array[1..10] of TLabel;
    FLabelsContador: array[1..10] of TLabel;
    FPanelGrafico: TPanel;          // Panel principal para el gráfico
    FGraficos: array[1..10] of TPanel;  // Panel individual por estación  
    FLineas: array[1..10] of TLabel;    // Líneas de datos
    FLabelEstado: TLabel;
    FLabelTotal: TLabel;
    FMemoHistorial: TMemo;
    FContadorPorEstacion: array[1..10] of Integer;
    FContadorTotal: Integer;
    FDatosPorEstacion: array[1..10] of TListaDatos;  // Historial de datos
    FTimer: TTimer;  // Para simular datos inicialmente
    
    procedure CrearInterfaz;
    procedure ActualizarGrafico;
    procedure DibujarGrafico;
    procedure OnTimerEvent(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AgregarDato(IDEstacion: Integer; Temperatura, Humedad, Presion: Double);
    procedure IniciarDemo;
  end;

var
  FormularioPrincipal: TFormMonitoreo;
  ContadorDemo: Integer = 0;

{ Implementación de TFormMonitoreo }

constructor TFormMonitoreo.Create(AOwner: TComponent);
var
  i, j: Integer;
begin
  inherited Create(AOwner);
  
  // Inicializa contadores
  FContadorTotal := 0;
  for i := 1 to 10 do
  begin
    FContadorPorEstacion[i] := 0;
    SetLength(FDatosPorEstacion[i], 0);  // Lista vacía
  end;
    
  CrearInterfaz;
  
  // Habilitar eventos de teclado
  KeyPreview := True;
  
  // Crear timer para demostración
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 3000;  // 3 segundos
  FTimer.OnTimer := @OnTimerEvent;
  FTimer.Enabled := False;  // Se activa después
end;

destructor TFormMonitoreo.Destroy;
begin
  if Assigned(FTimer) then
    FTimer.Free;
  inherited Destroy;
end;

procedure TFormMonitoreo.CrearInterfaz;
var
  i: Integer;
  PanelFondo: TLabel;
  ColoresEstacion: array[1..10] of TColor = (
    clRed, clBlue, clGreen, clPurple, clOlive,
    clNavy, clMaroon, clTeal, clFuchsia, clAqua
  );
begin
  // Configuración del formulario
  Caption := 'Sistema de Monitoreo Ambiental - Tengo Cara de Pepino S.A.';
  Width := 1400;
  Height := 900;
  Position := poScreenCenter;
  Color := $F0F0F0;  // Gris claro
  BorderStyle := bsSingle;
  
  // Panel de fondo para el título
  PanelFondo := TLabel.Create(Self);
  PanelFondo.Parent := Self;
  PanelFondo.Left := 10;
  PanelFondo.Top := 10;
  PanelFondo.Width := 1370;
  PanelFondo.Height := 60;
  PanelFondo.Color := $003366;  // Azul oscuro
  PanelFondo.Font.Color := clWhite;
  PanelFondo.Font.Size := 16;
  PanelFondo.Font.Style := [fsBold];
  PanelFondo.Alignment := taCenter;
  PanelFondo.Layout := tlCenter;
  PanelFondo.Caption := 'GRÁFICO DE MONITOREO EN TIEMPO REAL - DEMOSTRACIÓN DE VISUALIZACIÓN';
  
  // Panel principal para el gráfico
  FPanelGrafico := TPanel.Create(Self);
  FPanelGrafico.Parent := Self;
  FPanelGrafico.Left := 20;
  FPanelGrafico.Top := 80;
  FPanelGrafico.Width := 900;
  FPanelGrafico.Height := 600;
  FPanelGrafico.Color := clWhite;
  FPanelGrafico.BevelOuter := bvRaised;
  FPanelGrafico.Caption := '';
  
  // Crear área de gráfico para cada estación
  for i := 1 to 10 do
  begin
    // Panel individual para cada línea de estación
    FGraficos[i] := TPanel.Create(Self);
    FGraficos[i].Parent := FPanelGrafico;
    FGraficos[i].Left := 50;
    FGraficos[i].Top := 30 + (i-1) * 55;
    FGraficos[i].Width := 800;
    FGraficos[i].Height := 50;
    FGraficos[i].Color := $F8F8F8;
    FGraficos[i].BevelOuter := bvLowered;
    FGraficos[i].Caption := '';
    
    // Línea de datos (inicialmente invisible)
    FLineas[i] := TLabel.Create(Self);
    FLineas[i].Parent := FGraficos[i];
    FLineas[i].Left := 5;
    FLineas[i].Top := 5;
    FLineas[i].Width := 5;
    FLineas[i].Height := 40;
    FLineas[i].Color := ColoresEstacion[i];
    FLineas[i].Caption := '';
    FLineas[i].Visible := False;
    
    // Label nombre de estación (a la izquierda del gráfico)
    FLabelsEstacion[i] := TLabel.Create(Self);
    FLabelsEstacion[i].Parent := Self;
    FLabelsEstacion[i].Left := 950;
    FLabelsEstacion[i].Top := 110 + (i-1) * 55;
    FLabelsEstacion[i].Width := 100;
    FLabelsEstacion[i].Height := 25;
    FLabelsEstacion[i].Caption := 'ESTACIÓN ' + IntToStr(i);
    FLabelsEstacion[i].Font.Style := [fsBold];
    FLabelsEstacion[i].Font.Size := 9;
    FLabelsEstacion[i].Color := ColoresEstacion[i];
    FLabelsEstacion[i].Font.Color := clWhite;
    FLabelsEstacion[i].Alignment := taCenter;
    
    // Label contador
    FLabelsContador[i] := TLabel.Create(Self);
    FLabelsContador[i].Parent := Self;
    FLabelsContador[i].Left := 1060;
    FLabelsContador[i].Top := 110 + (i-1) * 55;
    FLabelsContador[i].Width := 120;
    FLabelsContador[i].Height := 25;
    FLabelsContador[i].Caption := '0 datos';
    FLabelsContador[i].Font.Style := [fsBold];
    FLabelsContador[i].Font.Size := 9;
    FLabelsContador[i].Color := $F0F0F0;
    FLabelsContador[i].Alignment := taCenter;
  end;
  
  // Label de estado principal
  FLabelEstado := TLabel.Create(Self);
  FLabelEstado.Parent := Self;
  FLabelEstado.Left := 950;
  FLabelEstado.Top := 700;
  FLabelEstado.Width := 420;
  FLabelEstado.Height := 40;
  FLabelEstado.Caption := 'DEMOSTRACIÓN: Presiona F5 para ver datos simulados en el gráfico';
  FLabelEstado.Font.Style := [fsBold];
  FLabelEstado.Font.Size := 11;
  FLabelEstado.Color := $80FF80;  // Verde claro
  FLabelEstado.WordWrap := True;
  FLabelEstado.Alignment := taCenter;
  
  // Label total
  FLabelTotal := TLabel.Create(Self);
  FLabelTotal.Parent := Self;
  FLabelTotal.Left := 950;
  FLabelTotal.Top := 750;
  FLabelTotal.Width := 420;
  FLabelTotal.Height := 30;
  FLabelTotal.Caption := 'Total de datos recibidos: 0';
  FLabelTotal.Font.Style := [fsBold];
  FLabelTotal.Font.Size := 12;
  FLabelTotal.Color := $80FF80;  // Verde claro
  FLabelTotal.Alignment := taCenter;
  
  // Memo para historial (más pequeño)
  FMemoHistorial := TMemo.Create(Self);
  FMemoHistorial.Parent := Self;
  FMemoHistorial.Left := 20;
  FMemoHistorial.Top := 700;
  FMemoHistorial.Width := 900;
  FMemoHistorial.Height := 150;
  FMemoHistorial.Font.Name := 'Courier New';
  FMemoHistorial.Font.Size := 8;
  FMemoHistorial.ScrollBars := ssVertical;
  FMemoHistorial.ReadOnly := True;
  FMemoHistorial.Color := $000040;  // Azul muy oscuro
  FMemoHistorial.Font.Color := $00FF00;  // Verde brillante
  FMemoHistorial.Lines.Add('=== DEMOSTRACIÓN DEL GRÁFICO VISUAL ===');
  FMemoHistorial.Lines.Add('Presiona F5 para iniciar simulación de datos...');
end;

procedure TFormMonitoreo.OnTimerEvent(Sender: TObject);
var
  IDEstacion: Integer;
  Temperatura, Humedad, Presion: Double;
begin
  // Simula datos cada 3 segundos
  Inc(ContadorDemo);
  IDEstacion := ((ContadorDemo - 1) mod 10) + 1;  // 1-10 rotativo
  Temperatura := 15.0 + Random(25);  // 15-40°C
  Humedad := 30.0 + Random(60);      // 30-90%
  Presion := 980.0 + Random(50);     // 980-1030 hPa
  
  AgregarDato(IDEstacion, Temperatura, Humedad, Presion);
end;

procedure TFormMonitoreo.DibujarGrafico;
var
  i, j, x, y, UltimoX: Integer;
  PuntoAnterior, PuntoActual: TPoint;
  Temperatura: Double;
  LineaGrafico: TLabel;
  MaxTemp, MinTemp, RangoTemp: Double;
begin
  MaxTemp := 50.0;  // Temperatura máxima esperada
  MinTemp := -10.0; // Temperatura mínima esperada
  RangoTemp := MaxTemp - MinTemp;
  
  // Dibujar líneas para cada estación que tenga datos
  for i := 1 to 10 do
  begin
    if Length(FDatosPorEstacion[i]) > 0 then
    begin
      FLineas[i].Visible := True;
      
      // Limpiar líneas anteriores (eliminar componentes anteriores)
      for j := FGraficos[i].ComponentCount - 1 downto 0 do
        if FGraficos[i].Components[j] is TLabel then
          if TLabel(FGraficos[i].Components[j]) <> FLineas[i] then
            FGraficos[i].Components[j].Free;
      
      // Dibujar nueva línea de datos
      UltimoX := 10;
      for j := 0 to High(FDatosPorEstacion[i]) do
      begin
        if j < 50 then  // Máximo 50 puntos visibles
        begin
          Temperatura := FDatosPorEstacion[i][j].Temperatura;
          
          // Calcular posición Y basada en temperatura
          y := 40 - Round(((Temperatura - MinTemp) / RangoTemp) * 35);
          if y < 5 then y := 5;
          if y > 40 then y := 40;
          
          x := UltimoX + 15;  // Separación entre puntos
          
          // Crear punto en el gráfico
          LineaGrafico := TLabel.Create(FGraficos[i]);
          LineaGrafico.Parent := FGraficos[i];
          LineaGrafico.Left := x;
          LineaGrafico.Top := y;
          LineaGrafico.Width := 8;
          LineaGrafico.Height := 8;
          LineaGrafico.Color := FLabelsEstacion[i].Color;
          LineaGrafico.Caption := '';
          LineaGrafico.Hint := Format('Est.%d: %.1f°C', [i, Temperatura]);
          LineaGrafico.ShowHint := True;
          
          UltimoX := x;
        end;
      end;
    end
    else
    begin
      FLineas[i].Visible := False;
    end;
  end;
end;

procedure TFormMonitoreo.ActualizarGrafico;
var
  i: Integer;
begin
  // Actualiza contadores
  for i := 1 to 10 do
  begin
    FLabelsContador[i].Caption := Format('%d datos', [FContadorPorEstacion[i]]);
  end;
  
  // Actualiza total
  FLabelTotal.Caption := Format('Total de datos recibidos: %d', [FContadorTotal]);
  
  // Redibuja el gráfico
  DibujarGrafico;
end;

procedure TFormMonitoreo.AgregarDato(IDEstacion: Integer; Temperatura, Humedad, Presion: Double);
var
  NuevoDato: TDatoEstacion;
  Info: String;
begin
  if (IDEstacion >= 1) and (IDEstacion <= 10) then
  begin
    // Crear nuevo dato
    NuevoDato.Timestamp := Now;
    NuevoDato.Temperatura := Temperatura;
    NuevoDato.Humedad := Humedad;
    NuevoDato.Presion := Presion;
    
    // Agregar a la lista de datos de la estación
    SetLength(FDatosPorEstacion[IDEstacion], Length(FDatosPorEstacion[IDEstacion]) + 1);
    FDatosPorEstacion[IDEstacion][High(FDatosPorEstacion[IDEstacion])] := NuevoDato;
    
    // Mantener máximo 100 datos por estación (eliminar más antiguos)
    if Length(FDatosPorEstacion[IDEstacion]) > 100 then
    begin
      // Mover datos hacia adelante
      Move(FDatosPorEstacion[IDEstacion][1], FDatosPorEstacion[IDEstacion][0], 
           99 * SizeOf(TDatoEstacion));
      SetLength(FDatosPorEstacion[IDEstacion], 100);
    end;
    
    // Incrementa contadores
    Inc(FContadorPorEstacion[IDEstacion]);
    Inc(FContadorTotal);
    
    // Actualiza estado
    FLabelEstado.Caption := Format('ÚLTIMO DATO RECIBIDO: Estación %d | %.1f°C, %.1f%% H, %.1f hPa | Total: %d', 
                                  [IDEstacion, Temperatura, Humedad, Presion, FContadorTotal]);
    
    // Añade al historial
    Info := Format('%s - Est.%d: %.1f°C, %.1f%% H, %.1f hPa [Dato #%d]', 
                   [TimeToStr(Now), IDEstacion, Temperatura, Humedad, Presion, FContadorTotal]);
    FMemoHistorial.Lines.Add(Info);
    
    // Mantiene solo últimas 20 líneas en historial
    while FMemoHistorial.Lines.Count > 22 do  // 20 + 2 headers
      FMemoHistorial.Lines.Delete(2);
    
    // Scroll automático al final
    FMemoHistorial.SelStart := Length(FMemoHistorial.Text);
    
    // Fuerza actualización inmediata del gráfico
    ActualizarGrafico;
  end;
end;

procedure TFormMonitoreo.IniciarDemo;
begin
  try
    FMemoHistorial.Lines.Add('=== INICIANDO DEMOSTRACIÓN DEL GRÁFICO ===');
    FMemoHistorial.Lines.Add('Simulando datos de 10 estaciones cada 3 segundos...');
    FLabelEstado.Caption := 'DEMOSTRACIÓN ACTIVA: Simulando datos ambientales';
    FTimer.Enabled := True;
  except
    on E: Exception do
    begin
      FMemoHistorial.Lines.Add('ERROR: ' + E.Message);
      FLabelEstado.Caption := 'ERROR en la demostración';
      FLabelEstado.Color := clRed;
    end;
  end;
end;

{ Manejador de teclas }
procedure TFormMonitoreo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_F5 then
  begin
    if not FTimer.Enabled then
      IniciarDemo
    else
    begin
      FTimer.Enabled := False;
      FLabelEstado.Caption := 'DEMOSTRACIÓN DETENIDA - Presiona F5 para reiniciar';
      FMemoHistorial.Lines.Add('=== DEMOSTRACIÓN DETENIDA ===');
    end;
  end;
end;

{ Programa principal }
begin
  Randomize;  // Para datos aleatorios en demo
  
  Application.Initialize;
  Application.Title := 'Monitor Ambiental con Gráfico - Tengo Cara de Pepino S.A.';
  
  // Crea el formulario principal
  Application.CreateForm(TFormMonitoreo, FormularioPrincipal);
  
  // Ejecuta la aplicación
  Application.Run;
end.
