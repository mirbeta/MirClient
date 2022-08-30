{*******************************************************}
{	     MiTeC System Information Component Suite       }
{              MSIC DLL Interface                       }
{                version 12.7.0                         }
{                                                       }
{        Copyright (c) 1997-2018 Michal Mutl            }
{                                                       }
{*******************************************************}

unit MSIC_Intf;

interface

uses Windows;

const
  SO_CPU      = $0001;
  SO_Machine  = $0002;
  SO_Devices  = $0004;
  SO_Display  = $0008;
  SO_Network  = $0010;
  SO_Media    = $0020;
  SO_Memory   = $0040;
  SO_Storage  = $0080;
  SO_USB      = $0100;
  SO_Engines  = $0200;
  SO_APM      = $0400;
  SO_Disk     = $0800;
  SO_OS       = $1000;
  SO_Printers = $2000;
  SO_Software = $4000;
  SO_Startup  = $8000;
  SO_Processes= $10000;
  SO_Monitor  = $20000;


  SO_All = SO_CPU or SO_Machine or SO_Devices or SO_Display or SO_Network or SO_Media or
          SO_Memory or SO_Engines or SO_STORAGE or SO_USB or SO_APM or SO_Disk or SO_OS or
          SO_Printers or SO_Software or SO_Startup or SO_Processes or SO_Monitor;


type
  TGenerateXMLReport = procedure(Topics: DWORD; Filename: PChar); stdcall;
  TSaveToStorage = procedure(Topics: DWORD; Filename: PChar); stdcall;

var
  MSIC_DLL: THandle = 0;
  GenerateXMLReport: TGenerateXMLReport = nil;
  SaveToStorage: TSaveToStorage = nil;

function InitMSICDLL(const AFilename: string): THandle;

implementation

function InitMSICDLL(const AFilename: string): THandle;
begin
  Result:=GetModuleHandle(PChar(AFileName));
  if Result=0 then
    Result:=LoadLibrary(PChar(AFileName));
  if Result<>0 then begin
    @GenerateXMLReport:=GetProcAddress(Result,'GenerateXMLReport');
    @SaveToStorage:=GetProcAddress(Result,'SaveToStorage');
  end;
end;

initialization
finalization
  if MSIC_DLL<>0 then
    FreeLibrary(MSIC_DLL);
end.
