unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  LCLIntf;

type

  { TAboutFormF }

  TAboutFormF = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    DescriptionText: TStaticText;
    VersionNumber: TLabel;
    UsageText: TStaticText;
    SourceCodeText: TLabel;
    GitHubLink: TLabel;
    procedure GitHubLinkClick(Sender: TObject);
  private

  public

  end;

var
  AboutFormF: TAboutFormF;

implementation

{$R *.lfm}

{ TAboutFormF }

procedure TAboutFormF.GitHubLinkClick(Sender: TObject);
begin
 OpenURL('https://github.com/JobstTechnologies/JT-SIX-Readout');
end;

end.

