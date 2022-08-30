unit RealtorWorldSystemInformation;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, RealtorWorldBaseFrame, dxCustomTileControl, ExtCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxClasses, cxGridLevel, cxGrid,
  cxMaskEdit, cxCurrencyEdit, cxSpinEdit, cxContainer, cxGroupBox, cxImage,
  dxGDIPlusClasses, cxLabel, cxProgressBar, dxCore;

type
  TfrmSystemInformation = class(TfrmBase)
    cxGrid1: TcxGrid;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1TableView1: TcxGridTableView;
    cxGrid1TableView1Column1: TcxGridColumn;
    cxGrid1TableView1Column2: TcxGridColumn;
    cxGrid1TableView1Column3: TcxGridColumn;
    tmRefesh: TTimer;
    cxGroupBox1: TcxGroupBox;
    Image1: TcxImage;
    lbComputerName: TcxLabel;
    lbMemory: TcxLabel;
    pbMemoryUsage: TcxProgressBar;
    lbWindowsInfo: TcxLabel;
    lbProcessorInfo: TcxLabel;
    procedure tmRefeshTimer(Sender: TObject);
  private
    function GetMachineName: string;
    function GetProcessMemoryUsage(AProcessID: THandle): Cardinal;
    procedure QueryMemoryUsage;
    procedure QuerySystemInfo;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  TlHelp32, PsAPI, Registry;

{$R *.dfm}

{ TfrmSystemInformation }

constructor TfrmSystemInformation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lbComputerName.Caption := GetMachineName;
  QuerySystemInfo;
  QueryMemoryUsage;
  tmRefeshTimer(nil);
end;

function TfrmSystemInformation.GetMachineName: string;
var
  ABuffer: array[0..MAX_PATH + 1] of Char;
  ABufferSize: Cardinal;
begin
  ABufferSize := Length(ABuffer);
  GetComputerName(ABuffer, ABufferSize);
  Result := ABuffer;
end;

function TfrmSystemInformation.GetProcessMemoryUsage(AProcessID: THandle): Cardinal;
var
  ACounters: TProcessMemoryCounters;
  AProcessHandle: THandle;
begin
  Result := 0;

  AProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, True, AProcessID);
  if AProcessHandle <> 0 then
  try
    ZeroMemory(@ACounters, SizeOf(ACounters));
    ACounters.cb := SizeOf(ACounters);
    if GetProcessMemoryInfo(AProcessHandle, @ACounters, SizeOf(ACounters)) then
      Result := ACounters.WorkingSetSize;
  finally
    CloseHandle(AProcessHandle);
  end;
end;

procedure TfrmSystemInformation.tmRefeshTimer(Sender: TObject);
var
  AProcessInfo: TProcessEntry32;
  ARecordIndex: Integer;
  ASnapshot: THandle;
begin
  ASnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    ZeroMemory(@AProcessInfo, SizeOf(AProcessInfo));
    AProcessInfo.dwSize := SizeOf(AProcessInfo);
    cxGrid1TableView1.DataController.BeginFullUpdate;
    try
      cxGrid1TableView1.DataController.RecordCount := 0;
      if Process32First(ASnapshot, AProcessInfo) then
      repeat
        ARecordIndex := cxGrid1TableView1.DataController.RecordCount;
        cxGrid1TableView1.DataController.RecordCount := cxGrid1TableView1.DataController.RecordCount + 1;
        cxGrid1TableView1.DataController.Values[ARecordIndex, cxGrid1TableView1Column1.Index] :=
          AProcessInfo.th32ProcessID;
        cxGrid1TableView1.DataController.Values[ARecordIndex, cxGrid1TableView1Column2.Index] :=
          dxStringToAnsiString(AProcessInfo.szExeFile);
        cxGrid1TableView1.DataController.Values[ARecordIndex, cxGrid1TableView1Column3.Index] :=
          GetProcessMemoryUsage(AProcessInfo.th32ProcessID) div 1024;

      until not Process32Next(ASnapshot, AProcessInfo);
    finally
      cxGrid1TableView1.DataController.EndFullUpdate;
    end;
    QueryMemoryUsage;
  finally
    CloseHandle(ASnapshot);
  end;
end;

procedure TfrmSystemInformation.QueryMemoryUsage;
const
  GigaByte = 1024 * 1024 * 1024;
var
{$IFDEF DELPHI12}
  AMemStatus: TMemoryStatusEx;
{$ELSE}
  AMemStatus: TMemoryStatus;
{$ENDIF}
begin
  ZeroMemory(@AMemStatus, SizeOf(AMemStatus));
  AMemStatus.dwLength := SizeOf(AMemStatus);
{$IFDEF DELPHI12}
  if GlobalMemoryStatusEx(AMemStatus) then
  begin
    pbMemoryUsage.Position := 100 * (AMemStatus.ullTotalPhys - AMemStatus.ullAvailPhys) / AMemStatus.ullTotalPhys;
    pbMemoryUsage.Properties.Text := Format('%0.2f GB Free', [AMemStatus.ullAvailPhys / GigaByte]);
  end;
{$ELSE}
  GlobalMemoryStatus(AMemStatus);
  pbMemoryUsage.Position := 100 * (AMemStatus.dwTotalPhys - AMemStatus.dwAvailPhys) / AMemStatus.dwTotalPhys;
  pbMemoryUsage.Properties.Text := Format('%0.2f GB Free', [AMemStatus.dwAvailPhys / GigaByte]);
{$ENDIF}
end;

procedure TfrmSystemInformation.QuerySystemInfo;
var
  ARegistry: TRegistry;
begin
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    if ARegistry.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    try
      lbWindowsInfo.Caption := ARegistry.ReadString('ProductName') + ' ' + ARegistry.ReadString('CSDVersion');
    finally
      ARegistry.CloseKey;
    end;
    if ARegistry.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    try
      lbProcessorInfo.Caption := ARegistry.ReadString('ProcessorNameString');
    finally
      ARegistry.CloseKey;
    end;
  finally
    ARegistry.Free;
  end;
end;

initialization
  RegisterFrame(IDSystemInformation, TfrmSystemInformation);
end.
