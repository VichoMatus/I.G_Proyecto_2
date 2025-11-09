unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls,
  UImagenController, UHTTPImagenService, UGridService;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnIniciar: TButton;
    btnDetener: TButton;
    btnLimpiar: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblEstado: TLabel;
    lblTotalImagenes: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    
    procedure btnDetenerClick(Sender: TObject);
    procedure btnIniciarClick(Sender: TObject);
    procedure btnLimpiarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TImagenController;
    FHTTPService: THTTPImagenService;
    FGridService: TGridService;
    
    procedure InicializarComponentes;
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
  FGridService.Free;
  FHTTPService.Free;
end;

procedure TMainForm.InicializarComponentes;
begin
  // Crear componentes de arquitectura limpia
  FHTTPService := THTTPImagenService.Create(8080);
  FGridService := TGridService.Create(Panel2);
  
  // Inicializar servicios
  FGridService.Inicializar;
  
  // Crear controlador
  FController := TImagenController.Create(FHTTPService, FGridService);
  FController.OnLog := @OnControllerLog;
  
  OnControllerLog('Puerto HTTP: 8080');
  OnControllerLog('Endpoint: POST /imagen');
  OnControllerLog('Sistema listo para usar');
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

procedure TMainForm.btnLimpiarClick(Sender: TObject);
begin
  if MessageDlg('Confirmación', '¿Desea limpiar todas las imágenes del grid?', 
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FController.LimpiarGrid;
    ActualizarEstadisticas;
  end;
end;

procedure TMainForm.OnControllerLog(const AMensaje: String);
begin
  StatusBar1.SimpleText := AMensaje;
  
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
  btnLimpiar.Enabled := True;
  
  if Activo then
  begin
    lblEstado.Caption := 'ACTIVO';
    lblEstado.Font.Color := clGreen;
    StatusBar1.SimpleText := 'Servidor activo - Esperando imágenes en puerto 8080';
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
  Total := FController.ObtenerTotalImagenes;
  lblTotalImagenes.Caption := Format('Total de imágenes: %d', [Total]);
end;

end.
