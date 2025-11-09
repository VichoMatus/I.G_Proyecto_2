program ServidorImagenes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms,
  UMainForm, UImagenController, UHTTPImagenService, UGridService, UImagenModel;

{$R *.res}

begin
  Randomize;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.