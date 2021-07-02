program JTSIXReadout;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JTSIXReadoutMain,
  SerialUSBSelection, AboutForm;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Title:='JT SIX Readout';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSerialUSBSelectionF, SerialUSBSelectionF);
  Application.CreateForm(TAboutFormF, AboutFormF);
  Application.Run;
end.

