program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, Classes, SysUtils, fphttpapp, httpdefs, httproute,
  ExtCtrls, Graphics, StdCtrls, Controls, Dialogs;

type
  { Formulario principal del servidor }
  TFormServidor = class(TForm)
    PanelTitulo: TPanel;
    LabelTitulo: TLabel;
    GridPanel: TPanel;
    BtnSalir: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnSalirClick(Sender: TObject);
  private
    Celdas: array[0..24] of TImage;  // 25 celdas (5x5)
    CeldasOcupadas: array[0..24] of Boolean;  // Estado de cada celda
    procedure CrearGrid;
    procedure MostrarImagenEnCelda(ImagenStream: TMemoryStream);
    function ObtenerCeldaDisponible: Integer;
    function ObtenerCeldaAleatoria: Integer;
  end;

var
  FormServidor: TFormServidor;

{ Implementación del formulario }

procedure TFormServidor.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Configura el formulario
  Caption := 'IMAGE HTTP SERVER';
  Width := 800;
  Height := 700;
  Position := poScreenCenter;
  
  // Panel de título
  PanelTitulo := TPanel.Create(Self);
  PanelTitulo.Parent := Self;
  PanelTitulo.Align := alTop;
  PanelTitulo.Height := 60;
  PanelTitulo.BevelOuter := bvNone;
  
  // Etiqueta del título
  LabelTitulo := TLabel.Create(Self);
  LabelTitulo.Parent := PanelTitulo;
  LabelTitulo.Caption := 'IMAGE HTTP SERVER';
  LabelTitulo.Font.Size := 18;
  LabelTitulo.Font.Style := [fsBold];
  LabelTitulo.Align := alClient;
  LabelTitulo.Alignment := taCenter;
  LabelTitulo.Layout := tlCenter;
  
  // Panel del grid
  GridPanel := TPanel.Create(Self);
  GridPanel.Parent := Self;
  GridPanel.Align := alClient;
  GridPanel.BevelOuter := bvNone;
  GridPanel.Color := clWhite;
  
  // Botón salir
  BtnSalir := TButton.Create(Self);
  BtnSalir.Parent := Self;
  BtnSalir.Align := alBottom;
  BtnSalir.Caption := 'Salir';
  BtnSalir.Height := 40;
  BtnSalir.OnClick := @BtnSalirClick;
  
  // Inicializa estado de celdas
  for i := 0 to 24 do
    CeldasOcupadas[i] := False;
  
  // Crea el grid de imágenes
  CrearGrid;
  
  WriteLn('Servidor GUI inicializado');
end;

procedure TFormServidor.CrearGrid;
var
  i, fila, col: Integer;
  tamCelda, margen, x, y: Integer;
begin
  tamCelda := 120;  // Tamaño de cada celda
  margen := 10;     // Margen entre celdas
  
  // Crea 25 celdas (5x5)
  for i := 0 to 24 do
  begin
    fila := i div 5;  // Calcula fila (0-4)
    col := i mod 5;   // Calcula columna (0-4)
    
    // Calcula posición
    x := margen + (col * (tamCelda + margen));
    y := margen + (fila * (tamCelda + margen));
    
    // Crea TImage para cada celda
    Celdas[i] := TImage.Create(Self);
    Celdas[i].Parent := GridPanel;
    Celdas[i].Left := x;
    Celdas[i].Top := y;
    Celdas[i].Width := tamCelda;
    Celdas[i].Height := tamCelda;
    Celdas[i].Stretch := True;  // Ajusta imagen al tamaño
    Celdas[i].Proportional := True;  // Mantiene proporción
    Celdas[i].Center := True;  // Centra imagen
    
    // Borde para visualizar celda
    Celdas[i].Canvas.Pen.Color := clBlack;
    Celdas[i].Canvas.Pen.Width := 2;
    Celdas[i].Canvas.Rectangle(0, 0, tamCelda, tamCelda);
    
    // Texto "Imagen" en celda vacía
    Celdas[i].Canvas.Font.Size := 10;
    Celdas[i].Canvas.TextOut(40, 55, 'Imagen');
  end;
end;

function TFormServidor.ObtenerCeldaDisponible: Integer;
var
  i: Integer;
begin
  // Busca la primera celda vacía
  for i := 0 to 24 do
  begin
    if not CeldasOcupadas[i] then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;  // Todas ocupadas
end;

function TFormServidor.ObtenerCeldaAleatoria: Integer;
begin
  // Retorna un índice aleatorio entre 0 y 24
  Result := Random(25);
end;

procedure TFormServidor.MostrarImagenEnCelda(ImagenStream: TMemoryStream);
var
  indiceCelda: Integer;
  jpg: TJPEGImage;
  png: TPortableNetworkGraphic;
  bmp: TBitmap;
  ext: String;
begin
  // Determina qué celda usar
  indiceCelda := ObtenerCeldaDisponible;
  
  if indiceCelda = -1 then  // Todas ocupadas
  begin
    indiceCelda := ObtenerCeldaAleatoria;
    WriteLn('Todas las celdas ocupadas. Reemplazando celda ', indiceCelda);
  end
  else
  begin
    WriteLn('Mostrando en celda disponible: ', indiceCelda);
  end;
  
  try
    // Intenta cargar como JPEG
    try
      jpg := TJPEGImage.Create;
      try
        ImagenStream.Position := 0;
        jpg.LoadFromStream(ImagenStream);
        Celdas[indiceCelda].Picture.Assign(jpg);
        CeldasOcupadas[indiceCelda] := True;
        WriteLn('Imagen JPEG cargada en celda ', indiceCelda);
        Exit;
      finally
        jpg.Free;
      end;
    except
      // No es JPEG, intenta PNG
    end;
    
    // Intenta cargar como PNG
    try
      png := TPortableNetworkGraphic.Create;
      try
        ImagenStream.Position := 0;
        png.LoadFromStream(ImagenStream);
        Celdas[indiceCelda].Picture.Assign(png);
        CeldasOcupadas[indiceCelda] := True;
        WriteLn('Imagen PNG cargada en celda ', indiceCelda);
        Exit;
      finally
        png.Free;
      end;
    except
      // No es PNG, intenta BMP
    end;
    
    // Intenta cargar como BMP
    try
      bmp := TBitmap.Create;
      try
        ImagenStream.Position := 0;
        bmp.LoadFromStream(ImagenStream);
        Celdas[indiceCelda].Picture.Assign(bmp);
        CeldasOcupadas[indiceCelda] := True;
        WriteLn('Imagen BMP cargada en celda ', indiceCelda);
      finally
        bmp.Free;
      end;
    except
      WriteLn('Error: Formato de imagen no soportado');
    end;
    
  except
    on E: Exception do
      WriteLn('Error al cargar imagen: ', E.Message);
  end;
end;

procedure TFormServidor.BtnSalirClick(Sender: TObject);
begin
  Application.Terminate;
end;

{ Manejador HTTP para recibir imágenes }
procedure ManejadorImagen(ARequest: TRequest; AResponse: TResponse);
var
  ImagenStream: TMemoryStream;
  PartesMultipart: TStrings;
  i: Integer;
begin
  if ARequest.Method = 'POST' then
  begin
    WriteLn('Recibiendo imagen...');
    
    try
      // Crea stream para la imagen
      ImagenStream := TMemoryStream.Create;
      try
        // Lee el contenido del campo 'imagen'
        if ARequest.Files.Count > 0 then
        begin
          // Multipart/form-data con archivo
          ARequest.Files[0].Stream.Position := 0;
          ImagenStream.CopyFrom(ARequest.Files[0].Stream, ARequest.Files[0].Stream.Size);
          
          WriteLn('Imagen recibida: ', ARequest.Files[0].FileName, 
                  ' (', ImagenStream.Size, ' bytes)');
          
          // Muestra la imagen en el formulario
          if Assigned(FormServidor) then
            FormServidor.MostrarImagenEnCelda(ImagenStream);
          
          // Responde OK
          AResponse.Code := 200;
          AResponse.Content := '{"status": "ok", "message": "Imagen recibida"}';
        end
        else
        begin
          WriteLn('Error: No se recibió archivo');
          AResponse.Code := 400;
          AResponse.Content := '{"status": "error", "message": "No file received"}';
        end;
      finally
        ImagenStream.Free;
      end;
      
    except
      on E: Exception do
      begin
        WriteLn('Error al procesar imagen: ', E.Message);
        AResponse.Code := 500;
        AResponse.Content := '{"status": "error", "message": "' + E.Message + '"}';
      end;
    end;
  end
  else
  begin
    AResponse.Code := 405;
    AResponse.Content := '{"status": "error", "message": "Method not allowed"}';
  end;
  
  AResponse.ContentType := 'application/json';
end;

{ Programa principal }
begin
  Randomize;  // Inicializa generador aleatorio
  
  Application.Initialize;
  Application.CreateForm(TFormServidor, FormServidor);
  
  // Registra ruta HTTP
  HTTPRouter.RegisterRoute('/imagen', @ManejadorImagen);
  
  // Configura servidor HTTP
  Application.Port := 8080;
  Application.Threaded := True;
  
  WriteLn('=============================================================');
  WriteLn('Servidor HTTP de Imágenes');
  WriteLn('Empresa: Aquí te espero gallito Ltda');
  WriteLn('=============================================================');
  WriteLn('Servidor iniciado en http://localhost:8080');
  WriteLn('Esperando imágenes en: http://localhost:8080/imagen');
  WriteLn('=============================================================');
  
  Application.Run;
end.
