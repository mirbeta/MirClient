{*******************************************************}
{       MiTeC System Information Component Suite        }
{        Design Editors and Registration Routines       }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_DsgnIntf;

interface

uses
  MSI_Defs,
  MSI_Common,
  MSI_AD,
  MSI_APM,
  MSI_BT,
  MSI_CPU,
  MSI_DeviceMonitor,
  MSI_Devices,
  MSI_Disk,
  MSI_DiskMonitor,
  MSI_Display,
  MSI_DMA,
  MSI_DriveContent,
  MSI_Engines,
  MSI_EventLog,
  MSI_FW,
  MSI_Machine,
  MSI_Media,
  MSI_Memory,
  MSI_Monitor,
  MSI_MSProduct,
  MSI_Network,
  MSI_NetCreds,
  MSI_OS,
  MSI_Printers,
  MSI_Processes,
  MSI_Security,
  MSI_SMBIOS,
  MSI_Software,
  MSI_Startup,
  MSI_Storage,
  MSI_SystemInfo,
  MSI_USB,
  MSI_USBHistory,
  MSI_WIFI,
  MSI_WLANC,

  {$IFDEF RAD9PLUS}
  WinAPI.Windows, System.SysUtils, System.Classes, VCL.Graphics,
  ToolsAPI, DesignIntf, DesignEditors
  {$ELSE}
  Windows, SysUtils, Classes
  {$IFDEF BDS3PLUS}
  ,Graphics, ToolsAPI
  {$ENDIF}
  {$IFDEF FPC}
  ,LazarusPackageIntf
  {$ELSE}
  ,DesignIntf, DesignEditors
  {$ENDIF}
  {$ENDIF}
  ;


{$IFNDEF FPC}
type
  {$IFDEF RAD18PLUS}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}

  TMSI_RefreshPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TMSI_ComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
{$ENDIF}

procedure Register;

{$IFNDEF FPC}
{$R 'MSICS.dcr'}
{$ENDIF}

implementation

uses {$IFDEF RAD9PLUS}
     VCL.Forms, VCL.Controls;
     {$ELSE}
     Forms, Controls;
     {$ENDIF}

{$IFNDEF FPC}
const
  MSI_Software_CategoryName = 'Software Information';
  MSI_Hardware_CategoryName = 'Hardware Information';
  MSI_Forensic_CategoryName = 'Forensic Information';
  MSI_Extra_CategoryName = 'Extra';

{$IFDEF BDS3PLUS}
{$R MSICS_LOGO.RES}
procedure RegisterWithSplashScreen;
var
  bmp: TBitmap;
begin
  Bmp:=TBitmap.Create;
  Bmp.LoadFromResourceName(hInstance,'LOGO');
  try
    SplashScreenServices.AddPluginBitmap(cCompName, Bmp.Handle,False,Format('version %s',[cVersion]));
  except on E : Exception do
  end;
  Bmp.Free;
end;
{$ENDIF}
{$ENDIF}

procedure Register;
begin
  {$IFDEF BDS3PLUS}
  RegisterWithSplashScreen;
  {$ENDIF}
  RegisterComponents('MiTeC SIC Suite',[
                              TMiTeC_AD,
                              TMiTeC_APM,
                              TMiTeC_BT,
                              TMiTeC_CPU,
                              TMiTeC_DeviceMonitor,
                              TMiTeC_Devices,
                              TMiTeC_Disk,
                              TMiTeC_DiskMonitor,
                              TMiTeC_Display,
                              TMiTeC_DMA,
                              TMiTeC_DriveContent,
                              TMiTeC_Engines,
                              TMiTeC_EventLog,
                              TMiTeC_Machine,
                              TMiTeC_Media,
                              TMiTeC_Memory,
                              TMiTeC_Monitor,
                              TMiTeC_MSProduct,
                              TMiTeC_Network,
                              TMiTeC_NetCreds,
                              TMiTeC_OperatingSystem,
                              TMiTeC_Printers,
                              TMiTeC_ProcessList,
                              TMiTeC_Security,
                              TMiTeC_SMBIOS,
                              TMiTeC_Software,
                              TMiTeC_Startup,
                              TMiTeC_Storage,
                              TMiTeC_SystemInfo,
                              TMiTeC_USB,
                              TMiTeC_USBHistory,
                              TMiTeC_WIFI,
                              TMiTeC_WLANC,
                              TMiTeC_Firewall
                              ]);

  {$IFNDEF FPC}
  //RegisterComponentEditor(TMiTeC_SystemInfo,TMSI_ComponentEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_SystemInfo,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_CPU,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Machine,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Devices,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Display,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Network,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Media,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Memory,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_APM,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Engines,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Disk,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_OperatingSystem,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_DMA,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_SMBIOS,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Printers,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Software,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Startup,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Storage,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_USB,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_ProcessList,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Monitor,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_DriveContent,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_MSProduct,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_BT,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_EventLog,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_AD,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Security,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_WIFI,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_NetCreds,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_USBHistory,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_WLANC,'_Refresh',TMSI_RefreshPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TMiTeC_Firewall,'_Refresh',TMSI_RefreshPropertyEditor);

  RegisterPropertiesInCategory(MSI_Extra_CategoryName,TMiTeC_SystemInfo,
      ['_About',
       '_Refresh']);
  RegisterPropertiesInCategory(MSI_Software_CategoryName,TMiTeC_SystemInfo,
      ['Engines',
       'OS',
       'ProcessList',
       'Software',
       'Startup',
       'EventLog',
       'Security',
       'Firewall']);
  RegisterPropertiesInCategory(MSI_Hardware_CategoryName,TMiTeC_SystemInfo,
      ['APM',
       'CPU',
       'Devices',
       'Disk',
       'Display',
       'Machine',
       'Media',
       'Memory',
       'DMA',
       'SMBIOS',
       'Monitor',
       'Network',
       'Printers',
       'Storage',
       'USB',
       'Bluetooth',
       'ActiveDirectory',
       'WIFI'
    ]);
  RegisterPropertiesInCategory(MSI_Forensic_CategoryName,TMiTeC_SystemInfo,
      ['NetworkCredentials',
       'USBHistory',
       'WiFiKnownNetworks']);
  {$ENDIF}
end;

{$IFNDEF FPC}
{ TMSI_ComponentEditor }

procedure TMSI_ComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    1: Edit;
  end;
end;

function TMSI_ComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result:='Refresh';
  else
    Result:=inherited GetVerb(Index-1);
  end;
end;

function TMSI_ComponentEditor.GetVerbCount: Integer;
begin
  Result:=inherited GetVerbCount+2;
end;

{ TMSI_RefreshPropertyEditor }

procedure TMSI_RefreshPropertyEditor.Edit;
begin
  Screen.Cursor:=crHourGlass;
  try
    TMiTeC_Component(Self.GetComponent(0)).RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

function TMSI_RefreshPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;
{$ENDIF}

end.

