program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ExtCtrls, StdCtrls, Graphics, Controls, Classes, SysUtils;

type
  TFormServidor = class(TForm)
  private
    PanelGrid: TPanel;
    Celdas: array[0..24] of TPanel;
    procedure CrearInterfaz;
    procedure BtnSalirClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormServidor: TFormServidor;

constructor TFormServidor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  Caption := 'Servidor de Imagenes - Grid 5x5';
  Width := 700;
  Height := 750;
  Position := poScreenCenter;
  Color := clBtnFace;
  
  CrearInterfaz;
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
    
    Celdas[i] := TPanel.Create(Self);
    Celdas[i].Parent := PanelGrid;
    Celdas[i].Left := margen + (col * (tamCelda + margen));
    Celdas[i].Top := margen + (fila * (tamCelda + margen));
    Celdas[i].Width := tamCelda;
    Celdas[i].Height := tamCelda;
    Celdas[i].BevelOuter := bvRaised;
    Celdas[i].BevelWidth := 2;
    Celdas[i].Color := clSilver;
    Celdas[i].Caption := IntToStr(i + 1);
    Celdas[i].Font.Size := 20;
    Celdas[i].Font.Style := [fsBold];
  end;
end;

procedure TFormServidor.BtnSalirClick(Sender: TObject);
begin
  Close;
end;

begin
  Application.Initialize;
  Application.CreateForm(TFormServidor, FormServidor);
  Application.Run;
end.