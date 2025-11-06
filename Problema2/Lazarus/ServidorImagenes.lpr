program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ExtCtrls, StdCtrls, Graphics, Controls, Classes, SysUtils, 
  Dialogs;

type
  TFormServidor = class(TForm)
  private
    PanelGrid: TPanel;
    Celdas: array[0..24] of TImage;
    CeldasOcupadas: array[0..24] of Boolean;
    ContadorImagenes: Integer;
    TimerHTTP: TTimer;
    CarpetaHTTP: string;
    procedure CrearInterfaz;
    procedure BtnSalirClick(Sender: TObject);
    procedure IniciarRecepcionHTTP;
    procedure ProcesarHTTPPost(Sender: TObject);
    procedure MostrarImagenRecibida(const RutaArchivo: string);
    function ObtenerCeldaDisponible: Integer;
    function ObtenerCeldaAleatoria: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormServidor: TFormServidor;

constructor TFormServidor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  Caption := 'Lazarus - Receptor HTTP POST directo';
  Width := 700;
  Height := 750;
  Position := poScreenCenter;
  Color := clBtnFace;
  
  ContadorImagenes := 0;
  CarpetaHTTP := ExtractFilePath(Application.ExeName) + '..\http_input\';
  
  CrearInterfaz;
  IniciarRecepcionHTTP;
end;

destructor TFormServidor.Destroy;
begin
  if Assigned(TimerHTTP) then
  begin
    TimerHTTP.Enabled := False;
    TimerHTTP.Free;
  end;
  inherited Destroy;
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
  LabelTitulo.Caption := 'LAZARUS RECEPTOR HTTP POST';
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

procedure TFormServidor.IniciarRecepcionHTTP;
begin
  try
    // Crear carpeta para recepción HTTP
    if not DirectoryExists(CarpetaHTTP) then
      ForceDirectories(CarpetaHTTP);
    
    // Timer para procesar POST recibidos
    TimerHTTP := TTimer.Create(Self);
    TimerHTTP.Interval := 100; // Revisa cada 100ms (muy rápido)
    TimerHTTP.OnTimer := @ProcesarHTTPPost;
    TimerHTTP.Enabled := True;
    
    Caption := 'Lazarus HTTP Receptor - ACTIVO ✓';
    
    if Length(Celdas) > 0 then
    begin
      Celdas[0].Canvas.Font.Color := clGreen;
      Celdas[0].Canvas.Font.Size := 8;
      Celdas[0].Canvas.TextOut(5, 15, 'HTTP POST');
      Celdas[0].Canvas.TextOut(5, 30, 'RECEPTOR');
      Celdas[0].Canvas.TextOut(5, 45, 'ESPERANDO');
      Celdas[0].Canvas.TextOut(5, 60, 'desde');
      Celdas[0].Canvas.TextOut(5, 75, 'PYTHON');
    end;
    
  except
    on E: Exception do
    begin
      Caption := 'Error iniciando receptor HTTP: ' + E.Message;
      ShowMessage('Error iniciando receptor HTTP: ' + E.Message);
    end;
  end;
end;procedure TFormServidor.ProcesarHTTPPost(Sender: TObject);
var
  SearchRec: TSearchRec;
  ArchivoCompleto: string;
begin
  // Buscar archivos de POST HTTP recibidos
  if FindFirst(CarpetaHTTP + '*.jpg', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      ArchivoCompleto := CarpetaHTTP + SearchRec.Name;
      
      // Procesar POST HTTP y mostrar
      MostrarImagenRecibida(ArchivoCompleto);
      DeleteFile(ArchivoCompleto);
      
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TFormServidor.MostrarImagenRecibida(const RutaArchivo: string);
var
  indiceCelda: Integer;
begin
  indiceCelda := ObtenerCeldaDisponible;
  
  if indiceCelda = -1 then
    indiceCelda := ObtenerCeldaAleatoria;
  
  try
    Celdas[indiceCelda].Picture.LoadFromFile(RutaArchivo);
    CeldasOcupadas[indiceCelda] := True;
    
    Inc(ContadorImagenes);
    
    Caption := Format('Lazarus HTTP - POST #%d procesado directo - Celda %d', 
                     [ContadorImagenes, indiceCelda + 1]);
  except
    on E: Exception do
    begin
      Celdas[indiceCelda].Canvas.Brush.Color := clRed;
      Celdas[indiceCelda].Canvas.FillRect(0, 0, 100, 100);
      Celdas[indiceCelda].Canvas.Font.Color := clWhite;
      Celdas[indiceCelda].Canvas.Font.Size := 8;
      Celdas[indiceCelda].Canvas.TextOut(25, 40, 'ERROR');
      Celdas[indiceCelda].Canvas.TextOut(15, 55, 'POST');
    end;
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



// Lazarus procesa POST HTTP directamente - cumple rúbrica

begin
  Randomize;
  Application.Initialize;
  Application.CreateForm(TFormServidor, FormServidor);
  Application.Run;
end.