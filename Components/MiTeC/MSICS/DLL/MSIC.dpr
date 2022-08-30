{*******************************************************}
{         System Information Component Suite            }
{                   MSIC DLL                            }
{                version 12.0.0                         }
{                                                       }
{        Copyright (c) 1997-2016 Michal Mutl            }
{                                                       }
{*******************************************************}

library MSIC;

uses
  Windows,
  SysUtils,
  Classes,
  MSI_Defs,
  MSI_SystemInfo,
  MSI_XML_Reports,
  MSIC_Intf in 'MSIC_Intf.pas';

{$R *.res}

procedure GenerateXMLReport(ATopics: DWORD; Filename: PChar); stdcall;
var
  sl: TStringList;
  t: TScanObjects;
  MSIC: TMiTeC_SystemInfo;
  s: string;
begin
  t:=[];
  s:=string(FileName);
  if SO_CPU and ATopics = SO_CPU then
    t:=t+[soCPU];
  if SO_Machine and ATopics = SO_Machine then
    t:=t+[soMachine];
  if SO_Devices and ATopics = SO_Devices then
    t:=t+[soDevices];
  if SO_Display and ATopics = SO_Display then
    t:=t+[soDisplay];
  if SO_Monitor and ATopics = SO_Monitor then
    t:=t+[soMonitor];
  if SO_Network and ATopics = SO_Network then
    t:=t+[soNetwork];
  if SO_Media and ATopics = SO_Media then
    t:=t+[soMedia];
  if SO_Memory and ATopics = SO_Memory then
    t:=t+[soMemory];
  if SO_STORAGE and ATopics = SO_STORAGE then
    t:=t+[soStorage];
  if SO_USB and ATopics = SO_USB then
    t:=t+[soUSB];
  if SO_Engines and ATopics = SO_Engines then
    t:=t+[soEngines];
  if SO_APM and ATopics = SO_APM then
    t:=t+[soAPM];
  if SO_Disk and ATopics = SO_Disk then
    t:=t+[soDisk];
  if SO_OS and ATopics = SO_OS then
    t:=t+[soOS];
  if SO_Printers and ATopics = SO_Printers then
    t:=t+[soPrinters];
  if SO_Software and ATopics = SO_Software then
    t:=t+[soSoftware];
  if SO_Startup and ATopics = SO_Startup then
    t:=t+[soStartup];
  if SO_Processes and ATopics = SO_Processes then
    t:=t+[soProcesses];
  MSIC:=TMiTeC_SystemInfo.Create(nil);
  with MSIC do begin
    sl:=TStringList.Create;
    try
      RefreshData(t);
      SystemInfo_XML_Report(MSIC,sl);
      {$IFDEF UNICODE}
      sl.SaveToFile(s,TEncoding.UTF8);
      {$ELSE}
      sl.Text:=UTF8Encode(sl.Text);
      sl.SaveToFile(s);
      {$ENDIF}
    finally
      sl.Free;
      Free;
    end;
  end;
end;

procedure SaveToStorage(ATopics: DWORD; Filename: PChar); stdcall;
var
  t: TScanObjects;
  s: string;
begin
  t:=[];
  s:=string(FileName);
  if SO_CPU and ATopics = SO_CPU then
    t:=t+[soCPU];
  if SO_Machine and ATopics = SO_Machine then
    t:=t+[soMachine];
  if SO_Devices and ATopics = SO_Devices then
    t:=t+[soDevices];
  if SO_Display and ATopics = SO_Display then
    t:=t+[soDisplay];
  if SO_Monitor and ATopics = SO_Monitor then
    t:=t+[soMonitor];
  if SO_Network and ATopics = SO_Network then
    t:=t+[soNetwork];
  if SO_Media and ATopics = SO_Media then
    t:=t+[soMedia];
  if SO_Memory and ATopics = SO_Memory then
    t:=t+[soMemory];
  if SO_STORAGE and ATopics = SO_STORAGE then
    t:=t+[soStorage];
  if SO_USB and ATopics = SO_USB then
    t:=t+[soUSB];
  if SO_Engines and ATopics = SO_Engines then
    t:=t+[soEngines];
  if SO_APM and ATopics = SO_APM then
    t:=t+[soAPM];
  if SO_Disk and ATopics = SO_Disk then
    t:=t+[soDisk];
  if SO_OS and ATopics = SO_OS then
    t:=t+[soOS];
  if SO_Printers and ATopics = SO_Printers then
    t:=t+[soPrinters];
  if SO_Software and ATopics = SO_Software then
    t:=t+[soSoftware];
  if SO_Startup and ATopics = SO_Startup then
    t:=t+[soStartup];
  if SO_Processes and ATopics = SO_Processes then
    t:=t+[soProcesses];
  with TMiTeC_SystemInfo.Create(nil) do begin
    try
      RefreshData(t);
      SaveToStorage(s);
    finally
      Free;
    end;
  end;
end;

exports
  GenerateXMLReport,
  SaveToStorage;
end.
