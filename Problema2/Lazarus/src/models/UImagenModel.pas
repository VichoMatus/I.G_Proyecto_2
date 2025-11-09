unit UImagenModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TImagenRecibida }
  { Modelo de dominio - Representa una imagen recibida }
  TImagenRecibida = class
  private
    FNombreArchivo: String;
    FDatos: TMemoryStream;
    FTimestamp: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure CargarDesdeArchivo(const ARuta: String);
    procedure CargarDesdeDatos(ADatos: TStream);
    function GuardarEnArchivo(const ARuta: String): Boolean;
    
    property NombreArchivo: String read FNombreArchivo write FNombreArchivo;
    property Datos: TMemoryStream read FDatos;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

implementation

{ TImagenRecibida }

constructor TImagenRecibida.Create;
begin
  inherited Create;
  FDatos := TMemoryStream.Create;
  FTimestamp := Now;
  FNombreArchivo := '';
end;

destructor TImagenRecibida.Destroy;
begin
  FDatos.Free;
  inherited Destroy;
end;

procedure TImagenRecibida.CargarDesdeArchivo(const ARuta: String);
begin
  try
    FDatos.Clear;
    FDatos.LoadFromFile(ARuta);
    FNombreArchivo := ExtractFileName(ARuta);
    FTimestamp := Now;
  except
    on E: Exception do
      raise Exception.Create('Error cargando imagen: ' + E.Message);
  end;
end;

procedure TImagenRecibida.CargarDesdeDatos(ADatos: TStream);
begin
  try
    FDatos.Clear;
    FDatos.CopyFrom(ADatos, 0);
    FDatos.Position := 0;
    FTimestamp := Now;
  except
    on E: Exception do
      raise Exception.Create('Error cargando datos: ' + E.Message);
  end;
end;

function TImagenRecibida.GuardarEnArchivo(const ARuta: String): Boolean;
begin
  Result := False;
  try
    FDatos.Position := 0;
    FDatos.SaveToFile(ARuta);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

end.
