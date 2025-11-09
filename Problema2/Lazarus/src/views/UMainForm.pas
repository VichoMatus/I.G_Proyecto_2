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
    btnSalir: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TImagenController;
    FHTTPService: THTTPImagenService;
    FGridService: TGridService;
    
    procedure InicializarComponentes;
    procedure OnControllerLog(const AMensaje: String);
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
  Caption := 'IMAGE HTTP SERVER';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FController) then
  begin
    FController.DetenerServidor;
    FController.Free;
  end;
  if Assigned(FGridService) then
    FGridService.Free;
  if Assigned(FHTTPService) then
    FHTTPService.Free;
end;

procedure TMainForm.btnSalirClick(Sender: TObject);
begin
  if Assigned(FController) then
    FController.DetenerServidor;
  Application.Terminate;
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
  
  // Iniciar servidor automáticamente
  FController.IniciarServidor;
end;

procedure TMainForm.OnControllerLog(const AMensaje: String);
begin
  // Log silencioso - solo para debugging si es necesario
end;

end.
