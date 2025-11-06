program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ExtCtrls, StdCtrls, Graphics, Controls, Classes, SysUtils, 
  Dialogs, fphttpapp, httpdefs, fpweb;

type
  // Forward declaration
  TFormServidor = class;

  TImagenModule = class(TFPWebModule)
  private
    FormServidor: TFormServidor;
  public
    procedure HandleRequest(ARequest: TFPHTTPServerRequest; AResponse: TFPHTTPServerResponse); override;
  end;

  TFormServidor = class(TForm)
  private
    PanelGrid: TPanel;
    Celdas: array[0..24] of TImage;
    CeldasOcupadas: array[0..24] of Boolean;
    ContadorImagenes: Integer;
    HTTPApp: TFPHTTPApplication;
    ImagenModule: TImagenModule;
    procedure CrearInterfaz;
    procedure BtnSalirClick(Sender: TObject);
    procedure IniciarServidorHTTP;
    procedure MostrarImagenDesdeMemoria(ImagenStream: TMemoryStream);
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
  
  Caption := 'Servidor HTTP Lazarus :8080 - Recibe POST directo';
  Width := 700;
  Height := 750;
  Position := poScreenCenter;
  Color := clBtnFace;
  
  ContadorImagenes := 0;
  
  CrearInterfaz;
  IniciarServidorHTTP;
end;

destructor TFormServidor.Destroy;
begin
  if Assigned(HTTPApp) then
  begin
    HTTPApp.Terminate;
    HTTPApp.Free;
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
  LabelTitulo.Caption := 'SERVIDOR HTTP LAZARUS :8080';
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
  try
    // Crear aplicación HTTP
    HTTPApp := TFPHTTPApplication.Create(nil);
    
    // Crear módulo para manejar POST requests
    ImagenModule := TImagenModule.Create(nil);
    ImagenModule.FormServidor := Self;
    
    // Registrar módulo
    HTTPApp.RegisterModule('imagen', ImagenModule);
    
    // Configurar puerto
    HTTPApp.Port := 8080;
    HTTPApp.Threaded := True;
    
    // Iniciar servidor HTTP
    HTTPApp.Initialize;
    
    Caption := 'Servidor HTTP Lazarus :8080 - ACTIVO ✓';
    
    if Length(Celdas) > 0 then
    begin
      Celdas[0].Canvas.Font.Color := clGreen;
      Celdas[0].Canvas.Font.Size := 8;
      Celdas[0].Canvas.TextOut(5, 15, 'HTTP SERVER');
      Celdas[0].Canvas.TextOut(5, 30, 'PUERTO: 8080');
      Celdas[0].Canvas.TextOut(5, 45, 'ESPERANDO');
      Celdas[0].Canvas.TextOut(5, 60, 'POST desde');
      Celdas[0].Canvas.TextOut(5, 75, 'PYTHON');
    end;
    
  except
    on E: Exception do
    begin
      Caption := 'Error iniciando servidor HTTP: ' + E.Message;
      ShowMessage('Error iniciando servidor HTTP: ' + E.Message);
    end;
  end;
end;procedure TFormServidor.MostrarImagenDesdeMemoria(ImagenStream: TMemoryStream);
var
  indiceCelda: Integer;
begin
  indiceCelda := ObtenerCeldaDisponible;
  
  if indiceCelda = -1 then
    indiceCelda := ObtenerCeldaAleatoria;
  
  try
    ImagenStream.Position := 0;
    Celdas[indiceCelda].Picture.LoadFromStream(ImagenStream);
    CeldasOcupadas[indiceCelda] := True;
    
    Inc(ContadorImagenes);
    
    Caption := Format('Servidor HTTP Lazarus - POST #%d recibido - Celda %d', 
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



// Implementación del módulo HTTP
procedure TImagenModule.HandleRequest(ARequest: TFPHTTPServerRequest; AResponse: TFPHTTPServerResponse);
var
  ImagenStream: TMemoryStream;
  PostData: string;
begin
  try
    // Solo manejar POST en ruta /imagen
    if (ARequest.Method = 'POST') and (ARequest.PathInfo = '/imagen') then
    begin
      PostData := ARequest.Content;
      if Length(PostData) = 0 then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Sin contenido"}';
        Exit;
      end;
      
      ImagenStream := TMemoryStream.Create;
      try
        ImagenStream.WriteBuffer(PostData[1], Length(PostData));
        
        if Assigned(FormServidor) then
          FormServidor.MostrarImagenDesdeMemoria(ImagenStream);
        
        AResponse.Code := 200;
        AResponse.Content := '{"status": "ok", "mensaje": "Imagen recibida"}';
        
      finally
        ImagenStream.Free;
      end;
    end
    else if (ARequest.Method = 'GET') and (ARequest.PathInfo = '/health') then
    begin
      AResponse.Code := 200;
      AResponse.Content := '{"status": "ok", "servidor": "Lazarus HTTP Server"}';
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.Content := '{"error": "Ruta no encontrada"}';
    end;
    
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := '{"error": "' + E.Message + '"}';
    end;
  end;
end;

begin
  Randomize;
  Application.Initialize;
  Application.CreateForm(TFormServidor, FormServidor);
  Application.Run;
end.