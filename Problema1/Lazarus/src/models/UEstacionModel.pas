unit UEstacionModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  { TEstacionMonitoreo }
  { Modelo de dominio - Representa una estación de monitoreo ambiental }
  TEstacionMonitoreo = class
  private
    FIde: Integer;        // ID de estación (1-10)
    FSFe: String;         // Fecha sistema (YYYY-MM-DD HH:MM:SS)
    FSHo: String;         // Hora sistema (HH:MM:SS)
    FMP: Double;          // Material Particulado (μg/m³)
    FP10: Double;         // Material Particulado 10 (μg/m³)
    FNTe: Double;         // Temperatura (°C)
    FNHr: Double;         // Humedad Relativa (%)
    FNPa: Double;         // Presión Atmosférica (hPa)
  public
    constructor Create;
    destructor Destroy; override;
    
    { Carga datos desde JSON }
    function FromJSON(const AJSONString: String): Boolean;
    function FromJSONObject(AJSONObj: TJSONObject): Boolean;
    
    { Convierte a JSON }
    function ToJSON: String;
    function ToJSONObject: TJSONObject;
    
    { Validación de datos }
    function Validar: Boolean;
    function ObtenerErrores: String;
    
    { Propiedades }
    property Ide: Integer read FIde write FIde;
    property SFe: String read FSFe write FSFe;
    property SHo: String read FSHo write FSHo;
    property MP: Double read FMP write FMP;
    property P10: Double read FP10 write FP10;
    property NTe: Double read FNTe write FNTe;
    property NHr: Double read FNHr write FNHr;
    property NPa: Double read FNPa write FNPa;
  end;

implementation

{ TEstacionMonitoreo }

constructor TEstacionMonitoreo.Create;
begin
  inherited Create;
  FIde := 0;
  FSFe := '';
  FSHo := '';
  FMP := 0.0;
  FP10 := 0.0;
  FNTe := 0.0;
  FNHr := 0.0;
  FNPa := 0.0;
end;

destructor TEstacionMonitoreo.Destroy;
begin
  inherited Destroy;
end;

function TEstacionMonitoreo.FromJSON(const AJSONString: String): Boolean;
var
  JSONData: TJSONData;
begin
  Result := False;
  try
    JSONData := GetJSON(AJSONString);
    try
      if JSONData is TJSONObject then
        Result := FromJSONObject(TJSONObject(JSONData));
    finally
      JSONData.Free;
    end;
  except
    Result := False;
  end;
end;

function TEstacionMonitoreo.FromJSONObject(AJSONObj: TJSONObject): Boolean;
begin
  Result := False;
  try
    FIde := AJSONObj.Get('ide', 0);
    FSFe := AJSONObj.Get('sFe', '');
    FSHo := AJSONObj.Get('sHo', '');
    FMP := AJSONObj.Get('MP', 0.0);
    FP10 := AJSONObj.Get('P10', 0.0);
    FNTe := AJSONObj.Get('nTe', 0.0);
    FNHr := AJSONObj.Get('nHr', 0.0);
    FNPa := AJSONObj.Get('nPa', 0.0);
    Result := True;
  except
    Result := False;
  end;
end;

function TEstacionMonitoreo.ToJSON: String;
var
  JSONObj: TJSONObject;
begin
  JSONObj := ToJSONObject;
  try
    Result := JSONObj.AsJSON;
  finally
    JSONObj.Free;
  end;
end;

function TEstacionMonitoreo.ToJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('ide', FIde);
  Result.Add('sFe', FSFe);
  Result.Add('sHo', FSHo);
  Result.Add('MP', FMP);
  Result.Add('P10', FP10);
  Result.Add('nTe', FNTe);
  Result.Add('nHr', FNHr);
  Result.Add('nPa', FNPa);
end;

function TEstacionMonitoreo.Validar: Boolean;
begin
  Result := True;
  
  // Validar ID de estación
  if (FIde < 1) or (FIde > 10) then
    Result := False;
  
  // Validar campos obligatorios
  if (FSFe = '') or (FSHo = '') then
    Result := False;
  
  // Validar rangos
  if (FMP < 0) or (FP10 < 0) then
    Result := False;
  
  if (FNHr < 0) or (FNHr > 100) then
    Result := False;
end;

function TEstacionMonitoreo.ObtenerErrores: String;
var
  Errores: TStringList;
begin
  Errores := TStringList.Create;
  try
    if (FIde < 1) or (FIde > 10) then
      Errores.Add('ID de estación debe estar entre 1 y 10');
    
    if FSFe = '' then
      Errores.Add('Fecha no puede estar vacía');
    
    if FSHo = '' then
      Errores.Add('Hora no puede estar vacía');
    
    if FMP < 0 then
      Errores.Add('Material Particulado no puede ser negativo');
    
    if FP10 < 0 then
      Errores.Add('Material Particulado 10 no puede ser negativo');
    
    if (FNHr < 0) or (FNHr > 100) then
      Errores.Add('Humedad debe estar entre 0 y 100');
    
    Result := Errores.Text;
  finally
    Errores.Free;
  end;
end;

end.
