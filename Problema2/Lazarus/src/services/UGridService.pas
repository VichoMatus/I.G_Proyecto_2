unit UGridService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,
  UImagenModel;

type
  { TGridService }
  { Servicio de Grid - Maneja la visualización de imágenes en grid 5x5 }
  TGridService = class
  private
    FPanel: TPanel;
    FCeldas: array[0..24] of TImage;
    FCeldasOcupadas: array[0..24] of Boolean;
    FContadorImagenes: Integer;
    
    procedure InicializarCelda(AIndex: Integer);
    function ObtenerCeldaDisponible: Integer;
    function ObtenerCeldaAleatoria: Integer;
  public
    constructor Create(APanel: TPanel);
    destructor Destroy; override;
    
    procedure Inicializar;
    procedure MostrarImagen(AImagen: TImagenRecibida);
    procedure LimpiarGrid;
    procedure LimpiarCelda(AIndex: Integer);
    
    property ContadorImagenes: Integer read FContadorImagenes;
  end;

implementation

{ TGridService }

constructor TGridService.Create(APanel: TPanel);
begin
  inherited Create;
  FPanel := APanel;
  FContadorImagenes := 0;
end;

destructor TGridService.Destroy;
var
  i: Integer;
begin
  for i := 0 to 24 do
  begin
    if Assigned(FCeldas[i]) then
      FCeldas[i].Free;
  end;
  inherited Destroy;
end;

procedure TGridService.Inicializar;
var
  i, fila, col: Integer;
  tamCelda, margen: Integer;
begin
  tamCelda := 100;
  margen := 15;
  
  for i := 0 to 24 do
  begin
    fila := i div 5;
    col := i mod 5;
    
    FCeldas[i] := TImage.Create(nil);
    FCeldas[i].Parent := FPanel;
    FCeldas[i].Left := margen + (col * (tamCelda + margen));
    FCeldas[i].Top := margen + (fila * (tamCelda + margen));
    FCeldas[i].Width := tamCelda;
    FCeldas[i].Height := tamCelda;
    FCeldas[i].Stretch := True;
    FCeldas[i].Proportional := True;
    FCeldas[i].Center := True;
    
    InicializarCelda(i);
    FCeldasOcupadas[i] := False;
  end;
end;

procedure TGridService.InicializarCelda(AIndex: Integer);
begin
  // Fondo gris para celdas vacías
  FCeldas[AIndex].Canvas.Brush.Color := clSilver;
  FCeldas[AIndex].Canvas.FillRect(0, 0, FCeldas[AIndex].Width, FCeldas[AIndex].Height);
  FCeldas[AIndex].Canvas.Font.Size := 16;
  FCeldas[AIndex].Canvas.Font.Style := [fsBold];
  FCeldas[AIndex].Canvas.Font.Color := clBlack;
  FCeldas[AIndex].Canvas.TextOut(30, 40, IntToStr(AIndex + 1));
  FCeldasOcupadas[AIndex] := False;
end;

procedure TGridService.MostrarImagen(AImagen: TImagenRecibida);
var
  indiceCelda: Integer;
  TempStream: TMemoryStream;
begin
  indiceCelda := ObtenerCeldaDisponible;
  
  if indiceCelda = -1 then
    indiceCelda := ObtenerCeldaAleatoria;
  
  try
    TempStream := TMemoryStream.Create;
    try
      // Copiar datos de la imagen
      AImagen.Datos.Position := 0;
      TempStream.CopyFrom(AImagen.Datos, 0);
      TempStream.Position := 0;
      
      // Cargar en la celda
      FCeldas[indiceCelda].Picture.LoadFromStream(TempStream);
      FCeldasOcupadas[indiceCelda] := True;
      
      Inc(FContadorImagenes);
    finally
      TempStream.Free;
    end;
  except
    on E: Exception do
    begin
      // Mostrar error en la celda
      FCeldas[indiceCelda].Canvas.Brush.Color := clRed;
      FCeldas[indiceCelda].Canvas.FillRect(0, 0, FCeldas[indiceCelda].Width, FCeldas[indiceCelda].Height);
      FCeldas[indiceCelda].Canvas.Font.Color := clWhite;
      FCeldas[indiceCelda].Canvas.Font.Size := 8;
      FCeldas[indiceCelda].Canvas.TextOut(25, 40, 'ERROR');
      FCeldas[indiceCelda].Canvas.TextOut(15, 55, 'IMAGEN');
    end;
  end;
end;

procedure TGridService.LimpiarGrid;
var
  i: Integer;
begin
  for i := 0 to 24 do
  begin
    InicializarCelda(i);
  end;
  FContadorImagenes := 0;
end;

procedure TGridService.LimpiarCelda(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= 24) then
  begin
    InicializarCelda(AIndex);
  end;
end;

function TGridService.ObtenerCeldaDisponible: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 24 do
  begin
    if not FCeldasOcupadas[i] then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TGridService.ObtenerCeldaAleatoria: Integer;
begin
  Result := Random(25);
end;

end.
