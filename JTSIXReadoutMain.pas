unit JTSIXReadoutMain;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  StdCtrls, ExtCtrls, Buttons, LCLType, Registry, LazFileUtils,
  SynaSer, Crt, UITypes, Types, Streamex,
  // the custom forms
  SerialUSBSelection, AboutForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    DefinitionGB: TGroupBox;
    LoadedDefFileLE: TLabeledEdit;
    LoadDefBB: TBitBtn;
    OpenDialog: TOpenDialog;
    SIXTypeRG: TRadioGroup;
    SIXConnectionGB: TGroupBox;
    StopButtonBB: TBitBtn;
    ConnComPortSensLE: TLabeledEdit;
    StopTimeLE: TLabeledEdit;
    StartButtonBB: TBitBtn;
    ReadTimer: TTimer;
    IndicatorSensorP: TPanel;
    Label66: TLabel;
    LoadedFileSensM: TMemo;
    StartTimeLE: TLabeledEdit;
    StatusGB: TGroupBox;
    SaveDialog: TSaveDialog;
    AboutMI: TMenuItem;
    MainMenu: TMainMenu;
    procedure AboutMIClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure LoadDefBBClick(Sender: TObject);
    procedure ReadTimerTimerFinished(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonBBClick(Sender: TObject);
    procedure StopButtonBBClick(Sender: TObject);
  private

  public
    function DialogWithPos(const Message: string; DialogType: TMsgDlgType;
              Buttons: TMsgDlgButtons; AX, AY: Integer): TModalResult;
    function OpenHandling(InName: string; FileExt: string): string;
    function SaveHandling(InName: string; FileExt: string): string;
    procedure CloseLazSerialConn(MousePointer: TPoint);
    function ParseDefFile(InFile: string): Boolean;

  end;

var
  MainForm : TMainForm;
  Version : string = '1.03';
  serSensor: TBlockSerial;
  timeCounter : double = 0.0; // counter of the overall SIX signal time in min
  signalCounter : integer = 0; // counter of the overall SIX readouts
  HaveSerialSensor : Boolean = False;
  SensorFileStream : TFileStream;
  HaveSensorFileStream : Boolean = False;
  InNameDef : string = ''; // name of loaded sensor definition file
  DropfileNameDef : string = ''; // name of dropped sensor definition file
  InNameSensor: string = ''; // name of sensor file
  DelayReadCounter : integer = 0; // to check if the readout will stop
  HeaderStrings : array [1..6] of string; // string for the output file header
  isBlank : array [1..6] of Boolean; // if channel is a blank
  Gains : array [1..6] of double; // the channel gains
  GainsRaw : array [1..6] of double; // the default gains
  Subtracts : array [1..6] of integer; // the channel subtracts
  TemperGains : array [1..8] of double; // the temperature gains
  NumChannels : integer = 6; // number of channels
  ErrorCount : integer = 0; // counts how many times we did not reeive a stop bit
  wasNoStopByte : Boolean = false; // to catch the case no stop byte was sent

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
 MainForm.Caption:= 'JT SIX Readout ' + Version;
 DefaultFormatSettings.DecimalSeparator:= '.'; // we use English numbers

 // explicitly set there because the IDE always
 // stores initial values with trailing LineEnding
 LoadedFileSensM.Text:= 'None';

 ActiveControl:= StartButtonBB;
end;

procedure TMainForm.FormClose(Sender: TObject);
begin
 // stop SIX reader timer
 ReadTimer.Enabled:= False;
 if HaveSerialSensor and (serSensor.LastError <> 9997) then
 // we cannot close socket or free when the connection timed out
 begin
  serSensor.CloseSocket;
  serSensor.free;
  HaveSerialSensor:= False;
 end;
 if HaveSensorFileStream then
  SensorFileStream.Free;
end;

function TMainForm.DialogWithPos(const Message: string; DialogType: TMsgDlgType;
           Buttons: TMsgDlgButtons; AX, AY: Integer): TModalResult;
// creates a dialog that will appear with its upper left edge
// at the current mouse position
var
  MessageForm: TForm;
begin
 MessageForm:= CreateMessageDialog(Message, DialogType, Buttons);
 try
   MessageForm.FormStyle:= fsStayOnTop;
   MessageForm.Position:= poDefaultSizeOnly;
   MessageForm.Left:= AX;
   MessageForm.Top:= AY;
   Result:= MessageForm.ShowModal;
 finally
   MessageForm.Free
 end;
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
 // set version number
 AboutFormF.VersionNumber.Caption:= Version;
 // open the dialog
 AboutFormF.ShowModal;
end;

procedure TMainForm.LoadDefBBClick(Sender: TObject);
var
 ParseSuccess : Boolean = false;
 MousePointer : TPoint;
 DummyString : string = '';
begin
 MousePointer:= Mouse.CursorPos; // store mouse position
 if DropfileNameDef = '' then // no file was dropped into the main window
 begin
  OpenDialog.InitialDir:= '';
  DummyString:= OpenHandling('', '.def');
  if DummyString = '' then
  begin
   // user aborted the loading
   IndicatorSensorP.Color:= clDefault;
   IndicatorSensorP.Caption:= '';
   LoadedDefFileLE.Text:= 'None';
   LoadedDefFileLE.Color:= clDefault;
   exit;
  end;
 end;

 if DropfileNameDef <> '' then
  InNameDef:= DropfileNameDef
 else
  InNameDef:= DummyString;
 SaveDialog.FileName:= ''; // will be re-set in SaveHandling()

  // parse the file
  ParseSuccess:= ParseDefFile(InNameDef);
  if not ParseSuccess then
  begin
   MessageDlgPos('Invalid definition file',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   IndicatorSensorP.Color:= clRed;
   IndicatorSensorP.Caption:= 'No definition file loaded';
   LoadedDefFileLE.Color:= clDefault;
   LoadedDefFileLE.Text:= 'None';
   exit;
  end;

  // display file name without suffix
  DummyString:= ExtractFileName(InNameDef);
  SetLength(DummyString, Length(DummyString) - 4);
  LoadedDefFileLE.Text:= DummyString;
  LoadedDefFileLE.Color:= clActiveCaption;

  // setup UI to start
  IndicatorSensorP.Color:= clDefault;
  IndicatorSensorP.Caption:= '';
end;

procedure TMainForm.ReadTimerTimerFinished(Sender: TObject);
type intArray = array[1..4] of byte;
     PintArray = ^intArray;
var
 OutLine : string;
 temperature, lastInterval : double;
 i, k, StopPos: integer;
 MousePointer : TPoint;
 dataArray : array[0..24] of byte;
 HiLowArray : array[0..1] of byte;
 Chan : array [1..6] of Int16;
 ChanDbl : array [0..8] of double; // start from zero purposely for non-existing subtracts
 checksum : integer;
 tempInt16 : Int16;
 PintegerArray: PintArray;
 wasRead : Boolean = false;
 SingleByte : byte;
begin
 // tell the OS the application is alive
 Application.ProcessMessages;

 // initialize
 MousePointer:= Mouse.CursorPos;
 lastInterval:= 0.0;
 for i:= 0 to 8 do
  ChanDbl[i]:= 0.0;

 // first check if we still have a filestream
 if not HaveSensorFileStream then
 begin
  ReadTimer.Enabled:= false;
  MessageDlgPos('The connection to the data file was lost!' + LineEnding
    + 'To restart you must again click on the Start button',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  ConnComPortSensLE.Color:= clRed;
  IndicatorSensorP.Caption:= 'Connection lost';
  IndicatorSensorP.Color:= clRed;
  // disable all buttons
  StartButtonBB.Enabled:= false;
  StopButtonBB.Enabled:= false;
  CloseLazSerialConn(MousePointer);
  HaveSerialSensor:= False;
  exit;
 end;

 try
  // in case we need to re-sync we must readout as many bytes until we get a
  // stop byte again
  if wasNoStopByte then
  begin
   SingleByte:= $1;
   i:= 0;
   // look for a stop byte in the next 100 bytes
   while (SingleByte <> $16) and (i < 101) do
   begin
    SingleByte:= serSensor.RecvByte(100);
    inc(i);
   end;
   wasNoStopByte:= false;
   if i > 100 then // no stop byte within 100 bytes, so there is a severe problem
   begin
    ReadTimer.Enabled:= False;
    MessageDlgPos('The received last 100 bytes do not contain a stop bit.'
    + LineEnding + 'Try to reconnect to the SIX.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ConnComPortSensLE.Color:= clRed;
    IndicatorSensorP.Caption:= 'SIX error';
    IndicatorSensorP.Color:= clRed;
    StartButtonBB.Enabled:= true;
    StopButtonBB.Enabled:= false;
    CloseLazSerialConn(MousePointer);
    HaveSerialSensor:= False;
    exit;
   end;
  end;

  // check if there are 25 bytes available to be read
  // if not wait another 100 ms until the timer finished the next time
  while serSensor.WaitingDataEx < 25 do
  begin
   delay(100);
   lastInterval:= lastInterval + 0.00166; // 100 ms of the delay in min
   inc(DelayReadCounter);
   if DelayReadCounter > 52 then
   // we reached 3 times the 1.7 s SIX output cycle, so there is something wrong
   // this will for example occur if USB cable was removed
   begin
    // often the SIX only stops telling it has not enough data
    // to try to read data
    try
     k:= serSensor.RecvBufferEx(@dataArray[0], 25, 100);
     wasRead:= true;
    finally
     if k <> 25 then
     begin
      ReadTimer.Enabled:= false;
      MessageDlgPos('Error: ' + ConnComPortSensLE.Text
       + ' did not deliver data within 5.1 s.' + LineEnding
       + ' Check the USB cable for a loose contact.',
       mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
      ConnComPortSensLE.Color:= clRed;
      IndicatorSensorP.Caption:= 'SIX error';
      IndicatorSensorP.Color:= clRed;
      StartButtonBB.Enabled:= true;
      StopButtonBB.Enabled:= false;
      CloseLazSerialConn(MousePointer);
      HaveSerialSensor:= False;
      exit;
     end;
     DelayReadCounter:= 0;
    end;
   end;
   if wasRead then
    break;
  end;
 finally
  if serSensor.LastError <> 0 then // can occur if USB cable was removed
  begin
   // Skip further error messages when there was already an error and thus
   // the timer is already stopped. This happens when the user pulled the
   // USB cable out while the SIX was running.
   if ReadTimer.Enabled = false then
    exit;
   ReadTimer.Enabled:= False;
   MessageDlgPos(ConnComPortSensLE.Text
    + ' error on connecting to SIX: ' + serSensor.LastErrorDesc + LineEnding
    + 'Check the USB cable for a loose contact.', mtError, [mbOK], 0,
    MousePointer.X, MousePointer.Y);
   ConnComPortSensLE.Color:= clRed;
   IndicatorSensorP.Caption:= 'Check USB cable';
   IndicatorSensorP.Color:= clRed;
   StartButtonBB.Enabled:= true;
   StopButtonBB.Enabled:= false;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // read the data
 if not wasRead then
  k:= serSensor.RecvBufferEx(@dataArray[0], 25, 100);

 // in case the read failed or not 25 bytes received
 if (serSensor.LastError <> 0) or (k <> 25) then
 begin
  inc(ErrorCount);
  // we wait then another timer run
  // if we get 3 times the same error, something is wrong and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + ReadTimer.Interval / 60000;
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   ReadTimer.Enabled:= False;
   if serSensor.LastError <> 0 then
    MessageDlgPos(ConnComPortSensLE.Text + ' error on reading signal data: '
     + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y)
   else
    MessageDlgPos('Error: Could not read 25 bytes. Got only ' + IntToStr(k) + ' bytes.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   ConnComPortSensLE.Color:= clRed;
   IndicatorSensorP.Caption:= 'SIX error';
   IndicatorSensorP.Color:= clRed;
   // disable all buttons
   StartButtonBB.Enabled:= true;
   StopButtonBB.Enabled:= false;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // now search the byte array for the stop bit
 StopPos:= -1;
 // since a value byte can also have the value $16, we search backwards
 // and check that the byte 20 positions earlier has the value $4 (begin of
 // a data block)
 for i:= 24 downto 20 do
 begin
  if dataArray[i] = $16 then
  begin
   if dataArray[i - 20] = $4 then
   begin
    StopPos:= i;
    break;
   end;
  end;
 end;
 if StopPos = -1 then
 begin
  inc(ErrorCount);
  // the read data block sometimes misses the stop bit
  // usually the stop bit appears again within the next readout, but not at the
  // expected position
  // to re-sync then with the SIX, wait one interval and read out byte by
  // byte until a stop byte appears
  wasNoStopByte:= true;
  // however, if we cannot re-sync after 3 attempts, something is wrong with the
  // SIX and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + ReadTimer.Interval / 60000;
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   ReadTimer.Enabled:= False;
   MessageDlgPos('The received last 300 bytes do not contain a stop bit.'
    + LineEnding + 'Try to reconnect to the SIX.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   ConnComPortSensLE.Color:= clRed;
   IndicatorSensorP.Caption:= 'SIX error';
   IndicatorSensorP.Color:= clRed;
   StartButtonBB.Enabled:= true;
   StopButtonBB.Enabled:= false;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // reset counter since we got no error
 ErrorCount:= 0;

 // we have all relevant data before the stop bit
 // first calculate the checksum
 checksum:= dataArray[StopPos - 2];
 for i:= StopPos - 3 downto StopPos - 20 do
  checksum:= checksum + dataArray[i];
 PintegerArray:= PintArray(@checksum);
 if PintegerArray^[1] <> dataArray[StopPos - 1] then
 begin
  // the data are corrupted so wait for another timer run
  lastInterval:= lastInterval + ReadTimer.Interval / 60000;
  timeCounter:= timeCounter + lastInterval;
  exit;
 end;
 // transform the dataArray so that zero array position gets the first value byte
 for i := 0 to 18 do
  dataArray[i]:= dataArray[StopPos - 19 + i];

 // create now a string with the line we will write to the file
 // first the time and counter
 inc(signalCounter);
 OutLine:= IntToStr(signalCounter) + #9;
 // take the time passed until the timer was triggered
 lastInterval:= lastInterval + ReadTimer.Interval / 60000; // in min
 if signalCounter > 1 then
  timeCounter:= timeCounter + lastInterval // in min
 else
  timeCounter:= 0.0; // assures the first data point has time zero
 OutLine:= OutLine + FloatToStrF(timeCounter, ffFixed, 3, 3) + #9;

 // now the channels
 // first convert each 2 bytes to a signed 16 bit integer
 for i:= 1 to NumChannels do
 begin
  HiLowArray[0]:= dataArray[2*i-1];
  HiLowArray[1]:= dataArray[2*i-2];
  Chan[i]:= Int16(HiLowArray);
 end;

 // now the temperature
 HiLowArray[0]:= dataArray[13];
 HiLowArray[1]:= dataArray[12];
 tempInt16:= Int16(HiLowArray);
 // the temperature value must be divided by 16 to get the value in deg celsius
 temperature:= tempInt16 / 16;

 if LoadedDefFileLE.Text <> 'None' then
 // convert to mM
 begin
  for i:= 1 to NumChannels do
   ChanDbl[i]:= Chan[i] * Gains[i] / 100
    / exp(TemperGains[i] / 100 * (temperature - TemperGains[8]));
  // subtract blank values
  for i:= 1 to NumChannels do
   ChanDbl[i]:= ChanDbl[i] - ChanDbl[Subtracts[i]];
  // output all non-blank channels
  for i:= 1 to NumChannels do
   begin
    if not isBlank[i] then
     OutLine:= OutLine + FormatFloat('0.0000', ChanDbl[i]) + #9;
   end;
   OutLine:= OutLine + FormatFloat('0.00', temperature) + #9;
 end;

 // store also the raw current values
 for i:= 1 to NumChannels do
  ChanDbl[i]:= Chan[i] * GainsRaw[i] / 100;
 for i:= 1 to NumChannels do
  OutLine:= OutLine + FormatFloat('0.0000', ChanDbl[i]) + #9;
 if LoadedDefFileLE.Text = 'None' then
  OutLine:= OutLine + FormatFloat('0.00', temperature);

 OutLine:= OutLine + LineEnding;

 // write the line to the file
 SensorFileStream.Write(OutLine[1], Length(OutLine));

end;

procedure TMainForm.StopButtonBBClick(Sender: TObject);
var
 MousePointer : TPoint;
begin
 ReadTimer.Enabled:= False;
 StopTimeLE.Text:= FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
 IndicatorSensorP.Caption:= 'Readout stopped';
 SIXTypeRG.Enabled:= true;
 StartButtonBB.Enabled:= true;
 LoadDefBB.Enabled:= true;

 // close com connection and file stream
 MousePointer:= Mouse.CursorPos; // store mouse position for possible error message
 CloseLazSerialConn(MousePointer);
 HaveSerialSensor:= False
end;

procedure TMainForm.StartButtonBBClick(Sender: TObject);
// opens the connection settings dialog and opens a connections according
// to the dialog input
var
 Reg : TRegistry;
 i, k : integer;
 MousePointer : TPoint;
 HeaderLine : string;
 dataArray : array[0..24] of byte;
begin
 // initialize
 MousePointer:= Mouse.CursorPos;
 DelayReadCounter:= 0;
 timeCounter:= 0.0;
 signalCounter:= 0;

 // determine all possible COM ports
 Reg:= TRegistry.Create;
 try
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
  begin
   with SerialUSBSelectionF do
   begin
    SerialUSBPortCB.Items.Clear;
    SerialUSBPortCB.Sorted:= false;
    Reg.GetValueNames(SerialUSBPortCB.Items);
    for i:= 0 to SerialUSBPortCB.Items.Count - 1 do
     SerialUSBPortCB.Items[i]:= Reg.ReadString(SerialUSBPortCB.Items[i]);
    SerialUSBPortCB.Sorted:= true;
   end;
  end;
 finally
  Reg.Free;
 end;
 // if there is only one COM port, preselect it
 with SerialUSBSelectionF do
 begin
  if SerialUSBPortCB.Items.Count = 1 then
   SerialUSBPortCB.ItemIndex:= 0
  else
   SerialUSBPortCB.ItemIndex:= -1;
 end;
 // open connection dialog
 SerialUSBSelectionF.ShowModal;
 if (COMPort = 'Ignore') then // user pressed Disconnect
 begin
  ConnComPortSensLE.Text:= 'Not connected';
  ConnComPortSensLE.Color:= clHighlight;
  IndicatorSensorP.Caption:= '';
  IndicatorSensorP.Color:= clDefault;
  if HaveSerialSensor then
  begin
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   IndicatorSensorP.Caption:= 'SIX stopped';
   IndicatorSensorP.Color:= clHighlight;
  end;
  // SIX type can now be set again
  SIXTypeRG.Enabled:= true;
  LoadDefBB.Enabled:= true;
  exit;
 end;
 if (COMPort = '') then // user forgot to set a COM port
 begin
  MessageDlgPos('Error: No COM port selected.',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  IndicatorSensorP.Caption:= 'Connection failiure';
  IndicatorSensorP.Color:= clRed;
  if HaveSerialSensor then
  begin
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   IndicatorSensorP.Caption:= 'SIX stopped';
   IndicatorSensorP.Color:= clHighlight;
  end;
  exit;
 end;

 try
  if HaveSerialSensor then
  begin
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
  end;
  ConnComPortSensLE.Text:= 'Not connected';
  ConnComPortSensLE.Color:= clHighlight;
  // open the connection
  try
   serSensor:= TBlockSerial.Create;
   serSensor.DeadlockTimeout:= 10000; // set timeout to 10 s
   serSensor.Connect(COMPort);
   // the connection settings must be after the opening
   serSensor.config(9600, 8, 'N', SB1, False, False);
  except
   exit;
  end;
  HaveSerialSensor:= True;
 finally
  if serSensor.LastError <> 0 then // output the error
  begin
   MessageDlgPos(ConnComPortSensLE.Text + ' error: ' + serSensor.LastErrorDesc,
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   IndicatorSensorP.Caption:= 'Connection failiure';
   IndicatorSensorP.Color:= clRed;
   ConnComPortSensLE.Color:= clRed;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // output connected port
 ConnComPortSensLE.Text:= COMPort;
 ConnComPortSensLE.Color:= clDefault;
 IndicatorSensorP.Caption:= 'Connection successful';
 IndicatorSensorP.Color:= clDefault;

 // read out some data as test
 // first wait until we get bytes to read
 k:= 0;
 while serSensor.WaitingDataEx < 25 do
 begin
  delay(100);
  inc(k);
  if k > 19 then // we reached 2.0 seconds, so there is something wrong
  begin
   MessageDlgPos('Error: ' + ConnComPortSensLE.Text + ' did not deliver data within 2 s.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   ConnComPortSensLE.Color:= clRed;
   IndicatorSensorP.Caption:= 'Wrong device';
   IndicatorSensorP.Color:= clRed;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // read now 25 bytes
 k:= serSensor.RecvBufferEx(@dataArray[0], 25, 50);

 // in case the read failed or not 25 bytes received
 if (serSensor.LastError <> 0) or (k <> 25) then
 begin
  MessageDlgPos(COMPort + ' error on reading 25 bytes: '
   + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  ConnComPortSensLE.Color:= clRed;
  IndicatorSensorP.Caption:= 'Wrong device';
  IndicatorSensorP.Color:= clRed;
  CloseLazSerialConn(MousePointer);
  HaveSerialSensor:= False;
  exit;
 end;

 // now open the file dialog to select the file to save the SIX data
 InNameSensor:= '';
 InNameSensor:= SaveHandling(InNameSensor, '.csv'); // opens file dialog
 if InNameSensor <> '' then
 begin
  try
   if FileExists(InNameSensor) = true then
   begin
    SensorFileStream:= TFileStream.Create(InNameSensor, fmOpenWrite or fmShareDenyNone);
    // the new command might be shorter, therefore delete its content
    SensorFileStream.Size:= 0;
   end
   else
    SensorFileStream:= TFileStream.Create(InNameSensor, fmCreate or fmShareDenyNone);
  except
   SensorFileStream.Free;
   LoadedFileSensM.Color:= clRed;
   LoadedFileSensM.Hint:= 'Sensor file could not be created or written';
   exit;
  end;
 end //end if OutName <> ''
 else
 begin
  MessageDlgPos('Error: A filename must be set to store the sensor data!',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   ConnComPortSensLE.Color:= clRed;
   IndicatorSensorP.Caption:= 'No file to save';
   IndicatorSensorP.Color:= clRed;
   CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
 end;

 StartTimeLE.Text:= FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
 StopTimeLE.Text:= '';
 // write header lines
 HeaderLine:= 'Created: ' + StartTimeLE.Text + LineEnding;
 if LoadedDefFileLE.Text = 'None' then
 begin
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  for i:= 1 to NumChannels do
   HeaderLine:= HeaderLine + 'Ch' + IntToStr(i) + ' [nA]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + LineEnding;
 end
 else
 begin
  HeaderLine:= HeaderLine + 'Used definition file: "' + LoadedDefFileLE.Text +
   '.def"' + LineEnding;
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  // the blank channels have the unit nA
  for i:= 1 to 6 do
  begin
   if (Pos('Blank', HeaderStrings[i]) <> 0)
    or (Pos('blank', HeaderStrings[i]) <> 0) then
    isBlank[i]:= true
   else
    isBlank[i]:= false;
  end;
  // output all non-blank channels
  for i:= 1 to NumChannels do
   if not isBlank[i] then
    HeaderLine:= HeaderLine + HeaderStrings[i] + ' [mM]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
  // also store the raw values
  for i:= 1 to NumChannels do
   HeaderLine:= HeaderLine + HeaderStrings[i] + ' [nA]' + #9;
  HeaderLine:= HeaderLine + LineEnding;
 end;
 try
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 except
  SensorFileStream.Free;
  LoadedFileSensM.Color:= clRed;
  LoadedFileSensM.Hint:= 'Writing to sensor file failed';
  exit;
 end;

 // we have now a valid file stream
 HaveSensorFileStream:= true;

 // final UI settings
 LoadedFileSensM.Text:= ExtractFileNameOnly(InNameSensor);
 LoadedFileSensM.Color:= clActiveCaption;
 LoadedFileSensM.Hint:= LoadedFileSensM.Text;
 StopButtonBB.Enabled:= true;
 // the user must not change these things while it is running:
 SIXTypeRG.Enabled:= false;
 StartButtonBB.Enabled:= false;
 LoadDefBB.Enabled:= false;

 // get the default gain for the raw values
 for i:= 1 to NumChannels do
 begin
  if SIXTypeRG.ItemIndex = 1 then
   GainsRaw[i]:= 0.1526
  else
   GainsRaw[i]:= 0.0763;
 end;

 // we can now set the timer interval,
 // which is fixed to the 1.7 s cycle of the SIX
 ReadTimer.Interval:= 1710; // in ms, we add 1 ms to avoid timer issues
 ReadTimer.Enabled:= true;
 DelayReadCounter:= 0; // for the case there was a previous run

end;

procedure TMainForm.CloseLazSerialConn(MousePointer: TPoint);
begin
 // stop timer
 ReadTimer.Enabled:= false;
 // close open file stream
 if HaveSensorFileStream then
 begin
  SensorFileStream.Free;
  HaveSensorFileStream:= false;
 end;
 if serSensor.LastError = 9997 then
 begin
  // we cannot close socket or free when the connection timed out
  MessageDlgPos('Error: ' + COMPort + ' cannot be closed.',
  mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  ConnComPortSensLE.Text:= 'Not acessible';
  ConnComPortSensLE.Color:= clRed;
  exit;
 end;
 if HaveSerialSensor then
 begin
  // close connection
  serSensor.CloseSocket;
  serSensor.free;
  HaveSerialSensor:= False;
 end;

 ConnComPortSensLE.Text:= 'Not connected';
 ConnComPortSensLE.Color:= clHighlight;
 StopButtonBB.Enabled:= false;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  DropfileNameDef:= FileNames[0];
  LoadDefBBClick(Sender);
  DropfileNameDef:= '';
end;

function TMainForm.OpenHandling(InName: string; FileExt: string): string;
// handles the open dialog
var
 OutNameTemp : string;
begin
 result:= '';
 if FileExt = '.def' then
 begin
  OpenDialog.Filter:= 'Definition file (*.def)|*.def';
  OpenDialog.Title:= 'Open sensor definition file';
 end;
 // propose a file name
 if (InName <> '') and (OpenDialog.FileName = '') then
  OpenDialog.FileName:= ExtractFileName(InName);
 if OpenDialog.FileName <> '' then
  OpenDialog.FileName:= ExtractFileName(OpenDialog.FileName);
 if OpenDialog.Execute = true then
 begin
  OutNameTemp:= OpenDialog.FileName;
  // add file extension if it is missing
  if (ExtractFileExt(OutNameTemp) <> FileExt) then
   Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);
  if not FileExists(OutNameTemp) = true then
  begin
   MessageDlg('The file does not exist!', mtError, [mbOK], 0);
   result:= '';
   exit;
  end;
  result:= OutNameTemp;
  // store last used name
  OpenDialog.FileName:= ExtractFileName(OutNameTemp);
 end
 else // was not executed for some reason
  result:= '';
end;

function TMainForm.SaveHandling(InName: string; FileExt: string): string;
// handles the save dialog
var YesNo : integer;
    OutNameTemp : string;
begin
 result:= '';
 if FileExt = '.PDAction' then
 begin
  SaveDialog.Filter:= 'Pump Driver Actions|*.PDAction';
  SaveDialog.Title:= 'Save file as';
 end
 else if FileExt = '.csv' then
 begin
  SaveDialog.Filter:= 'Table (*.csv)|*.csv';
  SaveDialog.Title:= 'Save data as';
 end
 else if FileExt = '.svg' then
 begin
  SaveDialog.Filter:= 'Vector graphics (*.svg)|*.svg';
  SaveDialog.Title:= 'Save screenshot as';
 end;
 // propose a file name
 if (InName <> '') and (SaveDialog.FileName = '') then
  SaveDialog.FileName:= ExtractFileName(InName);
 if SaveDialog.FileName <> '' then
  SaveDialog.FileName:= ExtractFileName(SaveDialog.FileName);
 if SaveDialog.Execute = true then
 begin
  OutNameTemp:= SaveDialog.FileName;
  // add file extension if it is missing
  if (ExtractFileExt(OutNameTemp) <> FileExt) then
   Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);
  if FileExists(OutNameTemp) = true then
  begin
   with CreateMessageDialog // MessageDlg with mbNo as default
       ('Do you want to overwrite the existing file' + LineEnding
             + ExtractFileName(OutNameTemp) + ' ?',
             mtWarning, [mbYes]+[mbNo]) do
   try
    ActiveControl := FindComponent('NO') as TWinControl;
    YesNo := ShowModal;
   finally
    Free;
   end;
   if YesNo = mrNo then // if No
   begin
    SaveHandling(InName, FileExt);
    exit;
   end
   else // if Yes
   begin
    result:= OutNameTemp;
    // store last used name
    SaveDialog.FileName:= ExtractFileName(OutNameTemp);
    exit;
   end;
  end; // end if FileExists

  result:= OutNameTemp;
  // store last used name
  SaveDialog.FileName:= ExtractFileName(OutNameTemp);
 end; // end if SaveDialog.Execute = true

end;

function TMainForm.ParseDefFile(InFile: string): Boolean;
// parses the input sensor definition file
var
 OpenFileStream : TFileStream;
 LineReader : TStreamReader;
 ReadLine : string;
 i, gainFactor : integer;
 StringArray : TStringArray;
 ppp : PChar;
begin
 result:= false;

 // check the SIX type
 if SIXTypeRG.ItemIndex = 1 then
  gainFactor:= 1
 else
  gainFactor:= 2;

 // open file stream
 try
  OpenFileStream:= TFileStream.Create(InFile, fmOpenRead);
  LineReader:= TStreamReader.Create(OpenFileStream);

  // read first line
  LineReader.ReadLine(ReadLine);
  // read until first comma
  StringArray:= ReadLine.Split(',');
  for i := 0 to 5 do
  begin
   if not TryStrToFloat(StringArray[i], Gains[i+1]) then
   begin
    Result:= false;
    exit;
   end;
   Gains[i+1]:= Gains[i+1] / gainFactor;
  end;

  // next interesting line is line 4
  LineReader.ReadLine(ReadLine); // line 2
  LineReader.ReadLine(ReadLine); // line 3
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(',');
  NumChannels:= 0;
  for i := 0 to 5 do
  begin
   ppp:= PChar(StringArray[i]);
   // take this later as names for file header
   HeaderStrings[i+1]:= AnsiExtractQuotedStr(ppp, '"');
   if HeaderStrings[i+1] <> '' then // we have a channel
    inc(NumChannels);
  end;

  // read line 5
  LineReader.ReadLine(ReadLine);
  // read until first comma
  StringArray:= ReadLine.Split(',');
  for i := 0 to 5 do
   if not TryStrToInt(StringArray[i], Subtracts[i+1]) then
   begin
    Result:= false;
    exit;
   end;

  // next interesting line is line 7
  LineReader.ReadLine(ReadLine); // line 6
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(',');
  for i := 0 to 7 do
   if not TryStrToFloat(StringArray[i], TemperGains[i+1]) then
   begin
    Result:= false;
    exit;
   end;

 finally
  LineReader.Free;
  OpenFileStream.Free;
 end;

 result:= true;
end;

end.

