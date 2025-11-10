unit UEstacionRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, db,
  UEstacionModel;

type
  { TEstacionRepository }
  { Repositorio - Capa de acceso a datos }
  TEstacionRepository = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FOwnsConnection: Boolean;
  public
    constructor Create(AConnection: TSQLite3Connection; ATransaction: TSQLTransaction); overload;
    constructor Create(const ADatabasePath: String); overload;
    destructor Destroy; override;
    
    { Inicialización }
    procedure InicializarBaseDatos;
    procedure CrearTablas;
    
    { CRUD Operations }
    function Guardar(AEstacion: TEstacionMonitoreo): Boolean;
    function ObtenerPorId(AId: Integer): TEstacionMonitoreo;
    function ObtenerUltimosPorEstacion(AEstacionId: Integer; ALimit: Integer = 50): TList;
    function ObtenerTodosLosUltimos(ALimit: Integer = 50): TList; // Para cargar datos al inicio
    function ContarRegistros: Integer;
    function ContarPorEstacion(AEstacionId: Integer): Integer;
    
    { Utilidades }
    function ConexionActiva: Boolean;
    procedure LimpiarDatos;
    
    property Connection: TSQLite3Connection read FConnection;
  end;

implementation

{ TEstacionRepository }

constructor TEstacionRepository.Create(AConnection: TSQLite3Connection; 
  ATransaction: TSQLTransaction);
begin
  inherited Create;
  
  if not Assigned(AConnection) then
    raise Exception.Create('Connection no puede ser nil');
  if not Assigned(ATransaction) then
    raise Exception.Create('Transaction no puede ser nil');
  
  FConnection := AConnection;
  FTransaction := ATransaction;
  FOwnsConnection := False;
  
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
end;

constructor TEstacionRepository.Create(const ADatabasePath: String);
begin
  inherited Create;
  FOwnsConnection := True;
  
  // Crear componentes de BD
  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := ADatabasePath;
  
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
  
  InicializarBaseDatos;
end;

destructor TEstacionRepository.Destroy;
begin
  FQuery.Free;
  
  if FOwnsConnection then
  begin
    if FConnection.Connected then
      FConnection.Close;
    FTransaction.Free;
    FConnection.Free;
  end;
  
  inherited Destroy;
end;

procedure TEstacionRepository.InicializarBaseDatos;
begin
  try
    if not FConnection.Connected then
      FConnection.Connected := True;
    
    CrearTablas;
  except
    on E: Exception do
      raise Exception.Create('Error inicializando BD: ' + E.Message);
  end;
end;

procedure TEstacionRepository.CrearTablas;
begin
  try
    FQuery.SQL.Text := 
      'CREATE TABLE IF NOT EXISTS estaciones (' +
      'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'ide INTEGER NOT NULL, ' +
      'sFe TEXT NOT NULL, ' +
      'sHo TEXT NOT NULL, ' +
      'MP REAL NOT NULL, ' +
      'P10 REAL NOT NULL, ' +
      'nTe REAL NOT NULL, ' +
      'nHr REAL NOT NULL, ' +
      'nPa REAL NOT NULL, ' +
      'timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)';
    
    FQuery.ExecSQL;
    FTransaction.Commit;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Error creando tablas: ' + E.Message);
    end;
  end;
end;

function TEstacionRepository.Guardar(AEstacion: TEstacionMonitoreo): Boolean;
begin
  Result := False;
  
  if not AEstacion.Validar then
    Exit;
  
  try
    FQuery.Close;
    FQuery.SQL.Text := 
      'INSERT INTO estaciones (ide, sFe, sHo, MP, P10, nTe, nHr, nPa) ' +
      'VALUES (:ide, :sFe, :sHo, :MP, :P10, :nTe, :nHr, :nPa)';
    
    FQuery.ParamByName('ide').AsInteger := AEstacion.Ide;
    FQuery.ParamByName('sFe').AsString := AEstacion.SFe;
    FQuery.ParamByName('sHo').AsString := AEstacion.SHo;
    FQuery.ParamByName('MP').AsFloat := AEstacion.MP;
    FQuery.ParamByName('P10').AsFloat := AEstacion.P10;
    FQuery.ParamByName('nTe').AsFloat := AEstacion.NTe;
    FQuery.ParamByName('nHr').AsFloat := AEstacion.NHr;
    FQuery.ParamByName('nPa').AsFloat := AEstacion.NPa;
    
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      Result := False;
    end;
  end;
end;

function TEstacionRepository.ObtenerPorId(AId: Integer): TEstacionMonitoreo;
begin
  Result := nil;
  
  try
    FQuery.Close;
    FQuery.SQL.Text := 
      'SELECT * FROM estaciones WHERE id = :id';
    FQuery.ParamByName('id').AsInteger := AId;
    FQuery.Open;
    
    if not FQuery.EOF then
    begin
      Result := TEstacionMonitoreo.Create;
      Result.Ide := FQuery.FieldByName('ide').AsInteger;
      Result.SFe := FQuery.FieldByName('sFe').AsString;
      Result.SHo := FQuery.FieldByName('sHo').AsString;
      Result.MP := FQuery.FieldByName('MP').AsFloat;
      Result.P10 := FQuery.FieldByName('P10').AsFloat;
      Result.NTe := FQuery.FieldByName('nTe').AsFloat;
      Result.NHr := FQuery.FieldByName('nHr').AsFloat;
      Result.NPa := FQuery.FieldByName('nPa').AsFloat;
    end;
    
    FQuery.Close;
  except
    on E: Exception do
      raise Exception.Create('Error obteniendo registro: ' + E.Message);
  end;
end;

function TEstacionRepository.ObtenerUltimosPorEstacion(AEstacionId: Integer; 
  ALimit: Integer = 50): TList;
var
  Estacion: TEstacionMonitoreo;
begin
  Result := TList.Create;
  
  try
    FQuery.Close;
    FQuery.SQL.Text := 
      'SELECT * FROM estaciones ' +
      'WHERE ide = :ide ' +
      'ORDER BY timestamp DESC ' +
      'LIMIT :limit';
    FQuery.ParamByName('ide').AsInteger := AEstacionId;
    FQuery.ParamByName('limit').AsInteger := ALimit;
    FQuery.Open;
    
    while not FQuery.EOF do
    begin
      Estacion := TEstacionMonitoreo.Create;
      Estacion.Ide := FQuery.FieldByName('ide').AsInteger;
      Estacion.SFe := FQuery.FieldByName('sFe').AsString;
      Estacion.SHo := FQuery.FieldByName('sHo').AsString;
      Estacion.MP := FQuery.FieldByName('MP').AsFloat;
      Estacion.P10 := FQuery.FieldByName('P10').AsFloat;
      Estacion.NTe := FQuery.FieldByName('nTe').AsFloat;
      Estacion.NHr := FQuery.FieldByName('nHr').AsFloat;
      Estacion.NPa := FQuery.FieldByName('nPa').AsFloat;
      
      Result.Add(Estacion);
      FQuery.Next;
    end;
    
    FQuery.Close;
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.Create('Error obteniendo registros: ' + E.Message);
    end;
  end;
end;

function TEstacionRepository.ContarRegistros: Integer;
begin
  Result := 0;
  try
    FQuery.Close;
    FQuery.SQL.Text := 'SELECT COUNT(*) as total FROM estaciones';
    FQuery.Open;
    Result := FQuery.FieldByName('total').AsInteger;
    FQuery.Close;
  except
    Result := 0;
  end;
end;

function TEstacionRepository.ContarPorEstacion(AEstacionId: Integer): Integer;
begin
  Result := 0;
  try
    FQuery.Close;
    FQuery.SQL.Text := 'SELECT COUNT(*) as total FROM estaciones WHERE ide = :ide';
    FQuery.ParamByName('ide').AsInteger := AEstacionId;
    FQuery.Open;
    Result := FQuery.FieldByName('total').AsInteger;
    FQuery.Close;
  except
    Result := 0;
  end;
end;

function TEstacionRepository.ObtenerTodosLosUltimos(ALimit: Integer = 50): TList;
var
  Estacion: TEstacionMonitoreo;
begin
  Result := TList.Create;
  
  try
    FQuery.Close;
    FQuery.SQL.Text := 
      'SELECT * FROM estaciones ' +
      'ORDER BY ide, timestamp DESC ' +
      'LIMIT :limit';
    FQuery.ParamByName('limit').AsInteger := ALimit;
    FQuery.Open;
    
    while not FQuery.EOF do
    begin
      Estacion := TEstacionMonitoreo.Create;
      Estacion.Ide := FQuery.FieldByName('ide').AsInteger;
      Estacion.SFe := FQuery.FieldByName('sFe').AsString;
      Estacion.SHo := FQuery.FieldByName('sHo').AsString;
      Estacion.MP := FQuery.FieldByName('MP').AsFloat;
      Estacion.P10 := FQuery.FieldByName('P10').AsFloat;
      Estacion.NTe := FQuery.FieldByName('nTe').AsFloat;
      Estacion.NHr := FQuery.FieldByName('nHr').AsFloat;
      Estacion.NPa := FQuery.FieldByName('nPa').AsFloat;
      
      Result.Add(Estacion);
      FQuery.Next;
    end;
    
    FQuery.Close;
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.Create('Error obteniendo registros: ' + E.Message);
    end;
  end;
end;

function TEstacionRepository.ConexionActiva: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TEstacionRepository.LimpiarDatos;
begin
  try
    FQuery.Close;
    FQuery.SQL.Text := 'DELETE FROM estaciones';
    FQuery.ExecSQL;
    FTransaction.Commit;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Error limpiando datos: ' + E.Message);
    end;
  end;
end;

end.
