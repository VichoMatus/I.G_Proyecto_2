program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ExtCtrls, StdCtrls, Graphics, Controls, Classes, SysUtils, Dialogs;

type
  TFormServidor = class(TForm)
  private
    PanelGrid: TPanel;
    Celdas: array[0..24] of TImage;
    CeldasOcupadas: array[0..24] of Boolean;
    TimerBuscarImagenes: TTimer;
    ContadorImagenes: Integer;
    UltimaVerificacion: TDateTime;
    procedure CrearInterfaz;
    procedure BtnSalirClick(Sender: TObject);
    procedure IniciarMonitoreoImagenes;
    procedure TimerBuscarImagenesTimer(Sender: TObject);
    procedure BuscarNuevasImagenes;
    procedure MostrarImagenEnCelda(NombreArchivo: String);
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
  
  Caption := 'Servidor de Imagenes Lazarus - Grid 5x5';
  Width := 700;
  Height := 750;
  Position := poScreenCenter;
  Color := clBtnFace;
  
  ContadorImagenes := 0;
  
  CrearInterfaz;
  IniciarMonitoreoImagenes;
end;

destructor TFormServidor.Destroy;
begin
  if Assigned(TimerBuscarImagenes) then
  begin
    TimerBuscarImagenes.Enabled := False;
    TimerBuscarImagenes.Free;
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

procedure TFormServidor.IniciarMonitoreoImagenes;
begin
  try
    // Configurar timer para monitorear carpeta de imágenes
    TimerBuscarImagenes := TTimer.Create(Self);
    TimerBuscarImagenes.Interval := 1000;  // Verificar cada segundo
    TimerBuscarImagenes.OnTimer := @TimerBuscarImagenesTimer;
    TimerBuscarImagenes.Enabled := True;
    
    UltimaVerificacion := Now;
    
    Caption := 'Lazarus Monitor - Esperando imágenes de Python ✓';
    
    // Mostrar mensaje en la primera celda
    if Length(Celdas) > 0 then
    begin
      Celdas[0].Canvas.Font.Color := clBlue;
      Celdas[0].Canvas.Font.Size := 8;
      Celdas[0].Canvas.TextOut(5, 35, 'ESPERANDO');
      Celdas[0].Canvas.TextOut(5, 50, 'PYTHON');
      Celdas[0].Canvas.TextOut(5, 65, 'CLIENTE');
      Celdas[0].Canvas.TextOut(5, 80, 'ACTIVO');
    end;
    
  except
    on E: Exception do
    begin
      Caption := 'Error iniciando monitoreo: ' + E.Message;
      ShowMessage('Error iniciando monitoreo: ' + E.Message);
    end;
  end;
end;

procedure TFormServidor.TimerBuscarImagenesTimer(Sender: TObject);
begin
  BuscarNuevasImagenes;
end;

procedure TFormServidor.BuscarNuevasImagenes;
var
  RutaImg: String;
  SearchRec: TSearchRec;
  CeldaIdx: Integer;
  ArchivoTiempo: TDateTime;
begin
  try
    // Ruta de la carpeta donde Python guarda las imágenes
    RutaImg := 'C:\temp\lazarus_imgs\';
    if not DirectoryExists(RutaImg) then
      RutaImg := ExtractFilePath(Application.ExeName) + '..\img\';
    
    if not DirectoryExists(RutaImg) then
      Exit; // No existe carpeta img
    
    // Buscar archivos de imagen modificados recientemente
    if FindFirst(RutaImg + '*.jpg', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        ArchivoTiempo := FileDateToDateTime(SearchRec.Time);
        
        // Si el archivo fue modificado después de la última verificación
        if ArchivoTiempo > UltimaVerificacion then
        begin
          // Cargar y mostrar la imagen
          MostrarImagenEnCelda(RutaImg + SearchRec.Name);
          Inc(ContadorImagenes);
          
          // Actualizar título
          Caption := Format('Lazarus Monitor - Imágenes: %d | Última: %s', 
                           [ContadorImagenes, SearchRec.Name]);
        end;
        
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    
    // También buscar PNG
    if FindFirst(RutaImg + '*.png', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        ArchivoTiempo := FileDateToDateTime(SearchRec.Time);
        
        if ArchivoTiempo > UltimaVerificacion then
        begin
          MostrarImagenEnCelda(RutaImg + SearchRec.Name);
          Inc(ContadorImagenes);
        end;
        
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    
    // Actualizar tiempo de última verificación
    UltimaVerificacion := Now;
    
  except
    on E: Exception do
    begin
      Caption := 'Error buscando imágenes: ' + E.Message;
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