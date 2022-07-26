unit SerialUSBSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TSerialUSBSelectionF }

  TSerialUSBSelectionF = class(TForm)
    ConnectBB: TBitBtn;
    CancelBB: TBitBtn;
    SerialUSBPortCB: TComboBox;
    SerialUSBPortL: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SerialUSBSelectionF: TSerialUSBSelectionF;

implementation

{$R *.lfm}

{ TSerialUSBSelectionF }

procedure TSerialUSBSelectionF.FormCreate(Sender: TObject);
begin
 ActiveControl:= SerialUSBPortCB;
end;


end.

