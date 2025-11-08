unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, CheckLst, TAGraph, TASeries, sqldb, sqlite3conn,
  UMonitoreoController, UEstacionRepository, UHTTPService, UChartService;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnIniciar: TButton;
    btnDetener: TButton;
    btnExportar: TButton;
    btnLimpiar: TButton;
    Chart1: TChart;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblEstado: TLabel;
    lblTotalRegistros: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    
    procedure btnDetenerClick(Sender: TObject);
    procedure btnExportarClick(Sender: TObject);
    procedure btnIniciarClick(Sender: TObject);
    procedure btnLimpiarClick(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TMonitoreoController;
    FRepository: TEstacionRepository;
    FHTTPService: THTTPService;
    FChartService: TChartService;
    
    procedure InicializarComponentes;
    procedure InicializarCheckListBox;
    procedure OnControllerLog(const AMensaje: String);
    procedure ActualizarEstadoUI;
    procedure ActualizarEstadisticas;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InicializarComponentes;
  ActualizarEstadoUI;
  OnControllerLog('Sistema iniciado correctamente');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FController.Free;
  FChartService.Free;
  FHTTPService.Free;
  FRepository.Free;
end;

procedure TMainForm.InicializarComponentes;
var
  DBPath: String;
begin
  // Configurar ruta de base de datos
  DBPath := ExtractFilePath(Application.ExeName) + 'clima.db';
  
  // Configurar componentes de base de datos
  SQLite3Connection1.DatabaseName := DBPath;
  SQLTransaction1.DataBase := SQLite3Connection1;
  SQLQuery1.DataBase := SQLite3Connection1;
  SQLQuery1.Transaction := SQLTransaction1;
  
  // Conectar la base de datos
  try
    if not SQLite3Connection1.Connected then
      SQLite3Connection1.Connected := True;
  except
    on E: Exception do
    begin
      ShowMessage('Error conectando a la base de datos: ' + E.Message + #13#10 + 
                  'Asegúrese de que sqlite3.dll está en el directorio del programa.');
      Application.Terminate;
      Exit;
    end;
  end;
  
  // Crear componentes de arquitectura limpia
  FRepository := TEstacionRepository.Create(SQLite3Connection1, SQLTransaction1);
  FHTTPService := THTTPService.Create(8080);
  FChartService := TChartService.Create(Chart1);
  
  // Inicializar servicios
  try
    FRepository.InicializarBaseDatos;
  except
    on E: Exception do
    begin
      ShowMessage('Error inicializando BD: ' + E.Message);
      Application.Terminate;
      Exit;
    end;
  end;
  
  FChartService.Inicializar;
  
  // Crear controlador
  FController := TMonitoreoController.Create(FRepository, FHTTPService, FChartService);
  FController.OnLog := @OnControllerLog;
  
  // Inicializar UI
  InicializarCheckListBox;
  
  OnControllerLog('Base de datos: ' + DBPath);
  OnControllerLog('Puerto HTTP: 8080');
  OnControllerLog('Sistema listo para usar');
end;

procedure TMainForm.InicializarCheckListBox;
var
  i: Integer;
begin
  CheckListBox1.Clear;
  for i := 1 to 10 do
  begin
    CheckListBox1.Items.Add(Format('Estación %d', [i]));
    CheckListBox1.Checked[i-1] := True; // Por defecto todas visibles
  end;
end;

procedure TMainForm.btnIniciarClick(Sender: TObject);
begin
  try
    FController.IniciarServidor;
    ActualizarEstadoUI;
  except
    on E: Exception do
      ShowMessage('Error al iniciar servidor: ' + E.Message);
  end;
end;

procedure TMainForm.btnDetenerClick(Sender: TObject);
begin
  try
    FController.DetenerServidor;
    ActualizarEstadoUI;
  except
    on E: Exception do
      ShowMessage('Error al detener servidor: ' + E.Message);
  end;
end;

procedure TMainForm.btnExportarClick(Sender: TObject);
var
  NombreArchivo: String;
begin
  NombreArchivo := FController.ExportarGrafico;
  
  if NombreArchivo <> '' then
    ShowMessage('Gráfico exportado exitosamente: ' + ExtractFileName(NombreArchivo))
  else
    ShowMessage('Error al exportar el gráfico');
end;

procedure TMainForm.btnLimpiarClick(Sender: TObject);
begin
  if MessageDlg('Confirmación', '¿Desea limpiar todos los gráficos?', 
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FController.LimpiarGraficos;
  end;
end;

procedure TMainForm.CheckListBox1ClickCheck(Sender: TObject);
var
  i: Integer;
  ListaVisible: String;
begin
  ListaVisible := '';
  
  for i := 0 to CheckListBox1.Count - 1 do
  begin
    FController.MostrarEstacion(i + 1, CheckListBox1.Checked[i]);
    
    if CheckListBox1.Checked[i] then
    begin
      if ListaVisible <> '' then
        ListaVisible := ListaVisible + ', ';
      ListaVisible := ListaVisible + IntToStr(i + 1);
    end;
  end;
  
  if ListaVisible <> '' then
    OnControllerLog('Estaciones visibles: ' + ListaVisible)
  else
    OnControllerLog('Ninguna estación visible');
end;

procedure TMainForm.OnControllerLog(const AMensaje: String);
begin
  Memo1.Lines.Add(AMensaje);
  
  // Limitar tamaño del log
  while Memo1.Lines.Count > 100 do
    Memo1.Lines.Delete(0);
  
  // Auto-scroll al final
  Memo1.SelStart := Length(Memo1.Text);
  Memo1.SelLength := 0;
  
  // Actualizar estadísticas
  ActualizarEstadisticas;
end;

procedure TMainForm.ActualizarEstadoUI;
var
  Activo: Boolean;
begin
  Activo := FController.ServidorActivo;
  
  btnIniciar.Enabled := not Activo;
  btnDetener.Enabled := Activo;
  btnExportar.Enabled := True;
  btnLimpiar.Enabled := True;
  
  if Activo then
  begin
    lblEstado.Caption := 'ACTIVO';
    lblEstado.Font.Color := clGreen;
    StatusBar1.SimpleText := 'Servidor activo - Esperando datos en puerto 8080';
  end
  else
  begin
    lblEstado.Caption := 'DETENIDO';
    lblEstado.Font.Color := clRed;
    StatusBar1.SimpleText := 'Servidor detenido';
  end;
end;

procedure TMainForm.ActualizarEstadisticas;
var
  Total: Integer;
begin
  Total := FController.ObtenerTotalRegistros;
  lblTotalRegistros.Caption := Format('Total de registros: %d', [Total]);
end;

end.
