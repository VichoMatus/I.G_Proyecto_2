program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ExtCtrls, StdCtrls, Graphics, Controls, Classes, SysUtils,
  Sockets, StrUtils;

type
  TFormServidor = class(TForm)
  private
    PanelGrid: TPanel;
    Celdas: array[0..24] of TImage;
    CeldasOcupadas: array[0..24] of Boolean;
    TimerServidor: TTimer;
    procedure CrearInterfaz;
    procedure BtnSalirClick(Sender: TObject);
    procedure TimerServidorTimer(Sender: TObject);
    procedure IniciarServidorHTTP;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MostrarImagenEnCelda(NombreArchivo: String);
    procedure MostrarImagenDeBytes(ImagenBytes: TBytes);
    procedure MostrarImagenAleatoria;
    function ObtenerCeldaDisponible: Integer;
    function ObtenerCeldaAleatoria: Integer;
  end;

var
  FormServidor: TFormServidor;

constructor TFormServidor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  Caption := 'Servidor de Imagenes - Grid 5x5';
  Width := 700;
  Height := 750;
  Position := poScreenCenter;
  Color := clBtnFace;
  
  CrearInterfaz;
  IniciarServidorHTTP;
end;

procedure TFormServidor.CrearInterfaz;
var
  i, fila, col: Integer;
  tamCelda, margen: Integer;
  PanelTitulo: TPanel;
  LabelTitulo: TLabel;
  BtnSalir: TButton;
begin
  PanelTitulo := TPanel.Create(Self);
  PanelTitulo.Parent := Self;
  PanelTitulo.Align := alTop;
  PanelTitulo.Height := 60;
  PanelTitulo.BevelOuter := bvNone;
  PanelTitulo.Color := clNavy;
  
  LabelTitulo := TLabel.Create(Self);
  LabelTitulo.Parent := PanelTitulo;
  LabelTitulo.Caption := 'SERVIDOR DE IMAGENES';
  LabelTitulo.Font.Size := 16;
  LabelTitulo.Font.Style := [fsBold];
  LabelTitulo.Font.Color := clWhite;
  LabelTitulo.Align := alClient;
  LabelTitulo.Alignment := taCenter;
  LabelTitulo.Layout := tlCenter;
  
  PanelGrid := TPanel.Create(Self);
  PanelGrid.Parent := Self;
  PanelGrid.Align := alClient;
  PanelGrid.Color := clWhite;
  PanelGrid.BevelOuter := bvNone;
  
  BtnSalir := TButton.Create(Self);
  BtnSalir.Parent := Self;
  BtnSalir.Align := alBottom;
  BtnSalir.Caption := 'SALIR';
  BtnSalir.Height := 50;
  BtnSalir.Font.Size := 12;
  BtnSalir.Font.Style := [fsBold];
  BtnSalir.OnClick := @BtnSalirClick;
  
  tamCelda := 100;
  margen := 15;
  
  for i := 0 to 24 do
  begin
    fila := i div 5;
    col := i mod 5;
    
    Celdas[i] := TImage.Create(Self);
    Celdas[i].Parent := PanelGrid;
    Celdas[i].Left := margen + (col * (tamCelda + margen));
    Celdas[i].Top := margen + (fila * (tamCelda + margen));
    Celdas[i].Width := tamCelda;
    Celdas[i].Height := tamCelda;
    Celdas[i].Stretch := True;
    Celdas[i].Proportional := True;
    Celdas[i].Center := True;
    
    // Fondo gris para celdas vacías
    Celdas[i].Canvas.Brush.Color := clSilver;
    Celdas[i].Canvas.FillRect(0, 0, tamCelda, tamCelda);
    Celdas[i].Canvas.Font.Size := 16;
    Celdas[i].Canvas.Font.Style := [fsBold];
    Celdas[i].Canvas.TextOut(30, 40, IntToStr(i + 1));
    
    CeldasOcupadas[i] := False;
  end;
end;

procedure TFormServidor.BtnSalirClick(Sender: TObject);
begin
  Close;
end;

procedure TFormServidor.IniciarServidorHTTP;
begin
  TimerServidor := TTimer.Create(Self);
  TimerServidor.Interval := 100;
  TimerServidor.OnTimer := @TimerServidorTimer;
  TimerServidor.Enabled := True;
  
  Caption := 'Servidor de Imagenes - Puerto 8080 ACTIVO';
end;

procedure TFormServidor.TimerServidorTimer(Sender: TObject);
begin
  // Simulación: busca imágenes en la carpeta img cada segundo
  if Random(100) < 5 then  // 5% de probabilidad cada 100ms = aprox cada 2 segundos
  begin
    MostrarImagenAleatoria;
  end;
end;

procedure TFormServidor.MostrarImagenAleatoria;
var
  rutaImg: String;
  archivos: TStringList;
  i: Integer;
begin
  rutaImg := ExtractFilePath(Application.ExeName) + '..\img\';
  
  archivos := TStringList.Create;
  try
    // Busca archivos de imagen
    if DirectoryExists(rutaImg) then
    begin
      for i := 1 to 25 do
      begin
        if FileExists(rutaImg + 'imagen' + IntToStr(i) + '.jpg') then
          archivos.Add(rutaImg + 'imagen' + IntToStr(i) + '.jpg');
        if FileExists(rutaImg + 'imagen' + IntToStr(i) + '.png') then
          archivos.Add(rutaImg + 'imagen' + IntToStr(i) + '.png');
      end;
      
      if archivos.Count > 0 then
        MostrarImagenEnCelda(archivos[Random(archivos.Count)]);
    end;
  finally
    archivos.Free;
  end;
end;

procedure TFormServidor.MostrarImagenDeBytes(ImagenBytes: TBytes);
var
  Stream: TMemoryStream;
  indiceCelda: Integer;
begin
  indiceCelda := ObtenerCeldaDisponible;
  if indiceCelda = -1 then
    indiceCelda := ObtenerCeldaAleatoria;
  
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(ImagenBytes[0], Length(ImagenBytes));
    Stream.Position := 0;
    Celdas[indiceCelda].Picture.LoadFromStream(Stream);
    CeldasOcupadas[indiceCelda] := True;
  finally
    Stream.Free;
  end;
end;

function TFormServidor.ObtenerCeldaDisponible: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 24 do
  begin
    if not CeldasOcupadas[i] then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TFormServidor.ObtenerCeldaAleatoria: Integer;
begin
  Result := Random(25);
end;

procedure TFormServidor.MostrarImagenEnCelda(NombreArchivo: String);
var
  indiceCelda: Integer;
begin
  // Determina qué celda usar
  indiceCelda := ObtenerCeldaDisponible;
  
  if indiceCelda = -1 then  // Todas ocupadas
    indiceCelda := ObtenerCeldaAleatoria;
  
  try
    // Carga la imagen
    Celdas[indiceCelda].Picture.LoadFromFile(NombreArchivo);
    CeldasOcupadas[indiceCelda] := True;
    
    // Actualiza título
    Caption := 'Servidor de Imagenes - Imagen en celda ' + IntToStr(indiceCelda + 1);
  except
    on E: Exception do
    begin
      // Si falla, muestra mensaje en la celda
      Celdas[indiceCelda].Canvas.Brush.Color := clRed;
      Celdas[indiceCelda].Canvas.FillRect(0, 0, 100, 100);
      Celdas[indiceCelda].Canvas.Font.Color := clWhite;
      Celdas[indiceCelda].Canvas.TextOut(25, 40, 'ERROR');
    end;
  end;
end;

begin
  Randomize;
  Application.Initialize;
  Application.CreateForm(TFormServidor, FormServidor);
  Application.Run;
end.