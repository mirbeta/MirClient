{*******************************************************}
{       MiTeC System Information Component Suite        }
{                                                       }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MSI_SystemInfo;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_CPU, MSI_Machine, MSI_Devices, MSI_Display, MSI_Network, MSI_Media,
     MSI_Memory, MSI_Engines, MSI_APM, MSI_Disk, MSI_OS, MSI_SMBIOS,
     MSI_Printers, MSI_Software, MSI_Startup, MSI_Storage, MSI_BT, MSI_EventLog, MSI_AD,
     MSI_USB, MSI_Processes, MSI_Monitor, MiTeC_WinIOCTL, MiTeC_USB, MSI_MSProduct, MSI_Security,
     MSI_Defs, MSI_WIFI, MSI_NetCreds, MSI_USBHistory, MSI_WLANC, MSI_FW;

type
  TMiTeC_SystemInfo = class(TMiTeC_Component)
  private
    FCPU: TMiTeC_CPU;
    FMemory: TMiTeC_Memory;
    FOS :TMiTeC_OperatingSystem;
    FDisk :TMiTeC_Disk;
    FMachine: TMiTeC_Machine;
    FNetwork: TMiTeC_Network;
    FDisplay: TMiTeC_Display;
    FEngines: TMiTeC_Engines;
    FDevices: TMiTeC_Devices;
    FAPM :TMiTeC_APM;
    FMedia: TMiTeC_Media;
    FPrinters: TMiTeC_Printers;
    FSoftware: TMiTeC_Software;
    FStartup: TMiTeC_Startup;
    FStorage: TMiTeC_Storage;
    FUSB: TMiTeC_USB;
    FProcessList: TMiTeC_ProcessList;
    FMonitor: TMiTeC_Monitor;
    FMSProduct: TMiTeC_MSProduct;
    FRefreshed: Boolean;
    FFilename: string;
    FBT: TMiTeC_BT;
    FEL: TMiTeC_EventLog;
    FAD: TMiTeC_AD;
    FCS: TCodeStreamProcedure;
    FSO: TScanObjects;
    FSC: TMiTeC_Security;
    FWIFI: TMiTeC_WIFI;
    FNetCreds: TMiTeC_NetCreds;
    FUSBHistory: TMiTeC_USBHistory;
    FWLANC: TMiTeC_WLANC;
    FFW: TMiTeC_Firewall;
    FDebug: string;
    procedure SetFilename(const Value: string);
  protected
    procedure SetHeaderReader(const Value: THeaderReader); override;
    procedure SetHeaderWriter(const Value: THeaderWriter); override;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure ExportOverview(sl: TStrings);

    procedure SetStorageName(const AValue: string);
    procedure SetLiveData(AValue: Boolean);

    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property StreamCodeProc: TCodeStreamProcedure read FCS;
    property Debug: string read FDebug;
  published
    property FileName: string read FFilename write SetFilename;
    property CPU :TMiTeC_CPU read FCPU stored False;
    property Memory :TMiTeC_Memory read FMemory stored False;
    property OS :TMiTeC_OperatingSystem read FOS stored False;
    property Disk :TMiTeC_Disk read FDisk stored False;
    property Machine :TMiTeC_Machine read FMachine stored False;
    property Network :TMiTeC_Network read FNetwork stored False;
    property NetworkCredentials: TMiTeC_NetCreds read FNetCreds stored False;
    property Display :TMiTeC_Display read FDisplay stored False;
    property Monitor :TMiTeC_Monitor read FMonitor stored False;
    property Media :TMiTeC_Media read FMedia stored False;
    property Devices :TMiTeC_Devices read FDevices stored False;
    property Storage :TMiTeC_Storage read FStorage stored False;
    property USB :TMiTeC_USB read FUSB stored False;
    property USBHistory :TMiTeC_USBHistory read FUSBHistory stored False;
    property Engines :TMiTeC_Engines read FEngines stored False;
    property APM :TMiTeC_APM read FAPM stored False;
    property Printers :TMiTeC_Printers read FPrinters stored False;
    property Software :TMiTeC_Software read FSoftware stored False;
    property Startup: TMiTeC_Startup read FStartup stored False;
    property ProcessList: TMiTeC_ProcessList read FProcessList stored false;
    property MSProduct: TMiTeC_MSProduct read FMSProduct stored false;
    property Bluetooth: TMiTeC_BT read FBT stored False;
    property EventLog: TMiTeC_EventLog read FEL stored False;
    property ActiveDirectory: TMiTeC_AD read FAD stored False;
    property Security: TMiTeC_Security read FSC stored False;
    property WIFI: TMiTeC_WIFI read FWIFI stored False;
    property WiFiKnownNetworks: TMiTeC_WLANC read FWLANC write FWLANC stored false;
    property Firewall: TMiTeC_Firewall read FFW write FFW stored False;
  end;

procedure SystemInfo_XML_Report(SI: TMiTeC_SystemInfo; var sl: TStringList); overload;

implementation

uses MiTeC_Routines, MiTeC_Datetime, MSI_XML_Reports;

procedure SystemInfo_XML_Report(SI: TMiTeC_SystemInfo; var sl: TStringList);
begin
  with SI do begin
  ReportHeader(sl);
  Machine_XML_Report(Machine,False,sl);
  CPU_XML_Report(CPU,False,sl);
  OperatingSystem_XML_Report(OS,False,sl);
  Memory_XML_Report(Memory,False,sl);
  Display_XML_Report(Display,False,sl);
  Monitor_XML_Report(Monitor,False,sl);
  APM_XML_Report(APM,False,sl);
  Media_XML_Report(Media,False,sl);
  Network_XML_Report(Network,False,sl);
  NetCreds_XML_Report(NetworkCredentials,False,sl);
  WIFI_XML_Report(WIFI,False,sl);
  WLANC_XML_Report(WiFiKnownNetworks,False,sl);
  Devices_XML_Report(Devices,False,sl);
  Storage_XML_Report(Storage,False,sl);
  Disk_XML_Report(Disk,False,sl);
  USB_XML_Report(USB,False,sl);
  USBHistory_XML_Report(USBHistory,False,sl);
  Bluetooth_XML_Report(Bluetooth,False,sl);
  Printers_XML_Report(Printers,False,sl);
  Engines_XML_Report(Engines,False,sl);
  ProcessList_XML_Report(ProcessList,False,sl);
  Software_XML_Report(Software,False,sl);
  Startup_XML_Report(Startup,False,sl);
  Security_XML_Report(Security,False,sl);
  ActiveDirectory_XML_Report(ActiveDirectory,False,sl);
  EventLog_XML_Report(EventLog,False,sl);

  ReportFooter(sl);
  end;
end;

{ TMiTeC_SystemInfo }

procedure TMiTeC_SystemInfo.Clear;
begin
  Machine.Clear;
  Memory.Clear;
  OS.Clear;
  CPU.Clear;
  Display.Clear;
  Monitor.Clear;
  APM.Clear;
  Media.Clear;
  Network.Clear;
  NetworkCredentials.Clear;
  Devices.Clear;
  Storage.Clear;
  USB.Clear;
  USBHistory.Clear;
  Engines.Clear;
  Disk.Clear;
  Printers.Clear;
  Software.Clear;
  Startup.Clear;
  ProcessList.Clear;
  MSProduct.Clear;
  Bluetooth.Clear;
  EventLog.Clear;
  ActiveDirectory.Clear;
  Security.Clear;
  WIFI.Clear;
  WiFiKnownNetworks.Clear;
  Firewall.Clear;
end;

constructor TMiTeC_SystemInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshed:=False;
  FCPU:=TMiTeC_CPU.Create(Self);
  FCPU.Name:='CPU';
  FMemory:=TMiTeC_Memory.Create(Self);
  FMemory.Name:='Memory';
  FDevices:=TMiTeC_Devices.Create(Self);
  FDevices.Name:='Devices';
  FOS:=TMiTeC_OperatingSystem.Create(Self);
  FOS.Name:='OperatingSystem';
  FDisk:=TMiTeC_Disk.Create(Self);
  FDisk.Name:='Disk';
  FMachine:=TMiTeC_Machine.Create(Self);
  FMachine.Name:='Machine';
  //FMachine.MiTeC_Devices:=FDevices;
  FStorage:=TMiTeC_Storage.Create(Self);
  FStorage.Name:='Storage';
  FUSB:=TMiTeC_USB.Create(Self);
  FUSB.Name:='USB';
  FUSBHistory:=TMiTeC_USBHistory.Create(Self);
  FUSBHistory.Name:='USBHistory';
  FNetwork:=TMiTeC_Network.Create(Self);
  FNetwork.Name:='Network';
  //FNetwork.MiTeC_Devices:=FDevices;
  FNetCreds:=TMiTeC_NetCreds.Create(Self);
  FNetCreds.Name:='NetworkCredentials';
  FDisplay:=TMiTeC_Display.Create(Self);
  FDisplay.Name:='Display';
  //FDisplay.MiTeC_Devices:=FDevices;
  FMonitor:=TMiTeC_Monitor.Create(Self);
  FMonitor.Name:='Monitor';
  //FMonitor.MiTeC_Devices:=FDevices;
  FMedia:=TMiTeC_Media.Create(Self);
  FMedia.Name:='Media';
  //FMedia.MiTeC_Devices:=FDevices;
  FEngines:=TMiTeC_Engines.Create(Self);
  FEngines.Name:='Engines';
  FAPM:=TMiTeC_APM.Create(Self);
  FAPM.Name:='APM';
  FPrinters:=TMiTeC_Printers.Create(Self);
  FPrinters.Name:='Printers';
  FSoftware:=TMiTeC_Software.Create(Self);
  FSoftware.Name:='Software';
  FStartup:=TMiTeC_Startup.Create(Self);
  FStartup.Name:='Startup';
  FProcessList:=TMiTeC_ProcessList.Create(Self);
  FProcessList.Name:='ProcessList';
  FMSProduct:=TMiTeC_MSProduct.Create(Self);
  FMSProduct.Name:='MSProduct';
  FBT:=TMiTeC_BT.Create(Self);
  FBT.Name:='Bluetooth';
  FEL:=TMiTeC_EventLog.Create(Self);
  FEL.Name:='EventLog';
  FAD:=TMiTeC_AD.Create(Self);
  FAD.Name:='ActiveDirectory';
  FSC:=TMiTeC_Security.Create(Self);
  FSC.Name:='Security';
  FWIFI:=TMiTeC_WIFI.Create(Self);
  FWIFI.Name:='WIFI';
  FWLANC:=TMiTeC_WLANC.Create(Self);
  FWLANC.Name:='WiFiKnownNetworks';
  FFW:=TMiTeC_Firewall.Create(Self);
  FFW.Name:='Firewall';
end;

destructor TMiTeC_SystemInfo.Destroy;
begin
  FCPU.Free;
  FMemory.Free;
  FOS.Free;
  FDisk.Free;
  FDevices.Free;
  FMachine.Free;
  FNetwork.Free;
  FNetCreds.Free;
  FDisplay.Free;
  FMonitor.Free;
  FMedia.Free;
  FStorage.Free;
  FEngines.Free;
  FAPM.Free;
  FPrinters.Free;
  FSoftware.Free;
  FStartup.Free;
  FProcessList.Free;
  FUSB.Free;
  FUSBHistory.Free;
  FMSProduct.Free;
  FBT.Free;
  FEL.Free;
  FAD.Free;
  FSC.Free;
  FWIFI.Free;
  FWLANC.Free;
  FFW.Free;
  inherited;
end;

procedure TMiTeC_SystemInfo.ExportOverview(sl: TStrings);
var
  i,j,k,c: Integer;
  ad,d,s: string;
  b: Boolean;
begin
  with sl do begin
    try
      Add('Machine: '+Machine.MachineName);
        Add('System : '+Machine.Computer);
        Add('Model  : '+Trim(Format('%s %s',[Trim(Machine.SMBIOS.SystemManufacturer),
                                                                   Trim(Machine.SMBIOS.SystemModel)])));
        Add('UpTime : '+FormatSeconds(Machine.SystemUpTime));

        Add('');

        if Trim(CPU.CPUName)='' then
          s:=Trim(CPU.MarketingName)
        else
          s:=Format('%s %s',[cVendorNames[CPU.Vendor].Prefix,CPU.CPUName]);
        Add('CPU  : '+Format('%d x %s - %d MHz',[CPU.CPUPhysicalCount,s,CPU.Frequency]));
        with CPU.Cache do
          Add('Cache: '+Format('%d KB L1 + %d KB L2 + %d KB L3',[Level1.Code.Size+Level1.Data.Size,Level2.Size,Level3.Size]));
        Add(Format('Core per package: %d',[CPU.CorePerPackage]));
        Add(Format('Logical per core: %d',[CPU.LogicalPerCore]));
        Add('');

        Add('Mainboard: '+Trim(Format('%s %s',[Trim(Machine.SMBIOS.MainBoardManufacturer),
                                                                   Trim(Machine.SMBIOS.MainBoardModel)])));
        Add('Memory   : '+Format('%d MB',[Memory.PhysicalTotal shr 20]));

        Add('');

        Add('Operating system: '+Format('%s %s [%d.%d.%d]',[OS.OSName,OS.OSEdition,
                                      OS.MajorVersion,
                                      OS.MinorVersion,
                                      OS.BuildNumber]));
        Add('Service pack    : '+Format('%d.%d',[OS.ServicePackMajorVersion,OS.ServicePackMinorVersion]));
        Add('Logged user     : '+Machine.User);

        Add('');

        c:=0;
        for i:=0 to Display.AdapterCount-1 do begin
          Add('Display adapter: '+Display.Adapter[i].Name);
          Add('Memory         : '+Format('%d MB',[Display.Adapter[i].Memory shr 20]));
        end;
        if c>0 then
          Add('');

        c:=0;
        for i:=0 to Monitor.Count-1 do begin
          Add('Monitor: '+Format('%s %s',[Monitor.Monitors[i].EDID.Name,Monitor.Monitors[i].EDID.ProductNumber]));
          Add('Dim    : '+Format('(%dx%d)cm',[Monitor.Monitors[i].EDID.Width,Monitor.Monitors[i].EDID.Height]));
          Inc(c);
        end;

        if c>0 then
          Add('');

        ad:=Disk.AvailableDisks;
        d:='';
        with Storage do
          for i:=0 to PhysicalCount-1 do begin
            case Physical[i].DeviceType of
              FILE_DEVICE_DISK: if Physical[i].Removable then
                                  s:='Removable: '
                                else
                                  s:='Fixed    : ';
              FILE_DEVICE_CD_ROM: s:='CD-ROM   : ';
              FILE_DEVICE_DVD:    s:='DVD      : ';
              FILE_DEVICE_TAPE:   s:='Tape     : ';
            end;
            Add(s+Trim(Format('%s - %d MB (%s)',[Physical[i].Model,Physical[i].Size shr 20,GetStorageBusTypeStr(Physical[i].BusType)])));
            for j:=0 to LogicalCount-1 do
              with Logical[j] do
                if PhysicalIndex=i then begin
                   d:=d+Copy(Drive,1,1);
                   if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then
                     Add(Format(' %s: ',[Drive])+Format('(%s %s - %d MB)',[
                                   GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ),
                                   GetPartitionSystem(Layout[LayoutIndex].Typ),
                                   Layout[LayoutIndex].Length.QuadPart shr 20]))
                else
                  Add(Format(' %s: ',[Drive])+'');
                end;
          end;
        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if MediaType=dtRemote then begin
              if not b then begin
                Add('Network drives');
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);
              Add(Format(' %s (%s)',[Drive,UNCPath]));
            end;
          end;
        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if (Pos(Copy(ad,i,1),d)=0) and (MediaType=dtRemovable) then begin
              if not b then begin
                Add('Removable drives');
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);
              if Disk.MediaPresent then
                Add(Format(' %s (%s - %d MB)',[Drive,FileSystem,Capacity shr 20]))
              else
                Add(' '+Drive);
            end;
          end;
        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if (Pos(Copy(ad,i,1),d)=0) then begin
              if not b then begin
                Add('Other drives');
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);
              if Disk.MediaPresent then
                Add(Format(' %s (%s - %d MB)',[Drive,FileSystem,Capacity shr 20]))
              else
                Add(' '+Drive);
            end;
          end;

        Add('');

        c:=0;
        with USB do
          for i:=0 to USBNodeCount-1 do
            if USBNodes[i].USBDevice.ConnectionStatus=1 then
              if not(USBNodes[i].USBClass in [usbHostController,usbHub,usbExternalHub]) then begin
                Add(Format('USB port %d  : %s',[USBNodes[i].USBDevice.Port,USBNodes[i].USBDevice.Product]));
                Add('Class       : '+ClassNames[integer(USBNodes[i].USBClass)]);
                Add('Manufacturer: '+Trim(USBNodes[i].USBDevice.Manufacturer));
                Add('Version     : '+Format('%d.%d',[USBNodes[i].USBDevice.MajorVersion,USBNodes[i].USBDevice.MinorVersion]));
                Inc(c);
              end;

        if c>0 then
          Add('');

        c:=-1;
        k:=maxint;
        j:=-1;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Media') and
             (Devices.Devices[i].PCINumber>-1) then
            if Devices.Devices[i].FunctionNumber<k then begin
              k:=Devices.Devices[i].FunctionNumber;
              j:=Devices.Devices[i].DeviceNumber;
              c:=i;
            end;
        if c>-1 then begin
          Add('Audio adapter: '+Devices.Devices[c].Name);
          c:=1;
          for i:=0 to Devices.DeviceCount-1 do
            if SameText(Devices.Devices[i].ClassName,'Media') and
               (Devices.Devices[i].DeviceNumber=j) and
               (Devices.Devices[i].FunctionNumber>k) then
              Add('Game adapter : '+Devices.Devices[i].Name);
        end;
        if (c=-1) and (Media.WAVEOut.Count>0) then begin
          Add('Audio adapter: '+Media.WAVEOut[0]);
          c:=1;
        end;

        if c>0 then
          Add('');

        c:=0;
        with Network.TCPIP do
          for i:=0 to AdapterCount-1 do
            if (Adapter[i].Address<>'') then begin
              s:=Adapter[i].Name;
              k:=Pos(' - ',s);
              if k>0 then
                s:=Copy(s,1,k);
              Add('Network adapter: '+s);
              Add('MAC address    : '+Adapter[i].Address);
              Add('IP address     : '+Adapter[i].IPAddress.CommaText);
              Inc(c);
            end;

        if c>0 then
          Add('');

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Modem') then begin
            Add('Modem: '+Devices.Devices[i].Name);
            Inc(c);
          end;

        if c>0 then
          Add('');

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
        if SameText(Devices.Devices[i].ClassName,'Keyboard') then begin
          Add('Keyboard: '+Devices.Devices[i].Name);
          Inc(c);
          Break;
        end;

        if c>0 then
          Add('');

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Mouse') then begin
            Add('Mouse: '+Devices.Devices[i].Name);
            Inc(c);
            Break;
          end;

        if c>0 then
          Add('');

        Add('Default printer: '+Printers.DefaultPrinter);
        for i:=0 to Printers.PrinterCount-1 do
          if not SameText(Printers.DefaultPrinter,Printers.PrinterName[i]) then
            Add('Other printer  : '+Printers.PrinterName[i]);
    except
      on e: exception do begin
        Add(e.message);
      end;
    end;
  end;
end;

function TMiTeC_SystemInfo.LoadFromStorage;
begin
  Clear;
  {$B+}
  FCS:=ACodeStream;
  Result:=Machine.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  SetStorageInfo(Machine.StorageInfo);
  Result:=Result or OS.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or CPU.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Memory.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Display.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Monitor.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or APM.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Media.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Network.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or NetworkCredentials.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Devices.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Storage.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or USB.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or USBHistory.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Engines.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Disk.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Printers.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Software.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Startup.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or ProcessList.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or MSProduct.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Bluetooth.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or EventLog.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or ActiveDirectory.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Security.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or WIFI.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or WiFiKnownNetworks.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Result:=Result or Firewall.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B-}
end;

procedure TMiTeC_SystemInfo.RefreshData;
begin
  inherited;
  FSO:=AScanObjects;
  if soUSB in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='USB';
    {$ENDIF}
    USB.RefreshData;
  end;
  if soUSBHistory in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='USBHistory';
    {$ENDIF}
    USBHistory.RefreshData;
  end;
  if soDevices in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Devices';
    {$ENDIF}
    Devices.RefreshData;
  end;
  if soStorage in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Storage';
    {$ENDIF}
    Storage.RefreshData;
  end;
  if soEngines in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Engines';
    {$ENDIF}
    Engines.RefreshData;
  end;
  if soCPU in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='CPU';
    {$ENDIF}
    CPU.RefreshData;
  end;
  if soMemory in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Memory';
    {$ENDIF}
    Memory.RefreshData;
  end;
  if soOS in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='OS';
    {$ENDIF}
    OS.RefreshData;
  end;
  if soMachine in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Machine';
    {$ENDIF}
    Machine.RefreshData;
  end;
  if soSoftware in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Software';
    {$ENDIF}
    Software.RefreshData;
  end;
  if soNetwork in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Network';
    {$ENDIF}
    Network.RefreshData;
  end;
  if soNetCreds in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='NetworkCredentials';
    NetworkCredentials.RefreshData;
    {$ELSE}
    try NetworkCredentials.RefreshData; except end;
    {$ENDIF}
  end;
  if soDisplay in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Display';
    {$ENDIF}
    Display.RefreshData;
  end;
  if soMonitor in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Monitor';
    {$ENDIF}
    Monitor.RefreshData;
  end;
  if soMedia in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Media';
    {$ENDIF}
    Media.RefreshData;
  end;
  if soAPM in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='APM';
    {$ENDIF}
    APM.RefreshData;
  end;
  if soPrinters in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Printers';
    {$ENDIF}
    Printers.RefreshData;
  end;
  if soStartup in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Startup';
    {$ENDIF}
    Startup.RefreshData;
  end;
  if soDisk in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Disk';
    {$ENDIF}
    Disk.RefreshData;
    Disk.Drive:=ExtractFileDrive(GetWinDir);
  end;
  if soProcesses in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='ProcessList';
    {$ENDIF}
    ProcessList.RefreshData;
  end;
  if soMSProduct in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='MSProduct';
    {$ENDIF}
    MSProduct.RefreshData;
  end;
  if soBT in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Bluetooth';
    {$ENDIF}
    Bluetooth.RefreshData;
  end;
  if soEventlog in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='EventLog';
    {$ENDIF}
    EventLog.RefreshData;
  end;
  if soAD in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='ActiveDirectory';
    {$ENDIF}
    ActiveDirectory.RefreshData;
  end;
  if soSecurity in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Security';
    {$ENDIF}
    Security.RefreshData;
  end;
  if soWIFI in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='WIFI';
    {$ENDIF}
    WIFI.RefreshData;
  end;
  if soWLANC in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='WLANC';
    {$ENDIF}
    WiFiKnownNetworks.RefreshData;
  end;
  if soFirewall in AScanObjects then begin
    {$IFDEF DEBUG}
    FDebug:='Firewall';
    {$ENDIF}
    Firewall.RefreshData;
  end;
  {$IFDEF DEBUG}
  FDebug:='';
  {$ENDIF}
  FRefreshed:=True;
end;

procedure TMiTeC_SystemInfo.SaveToStorage;
var
  i: Integer;
begin
  Memory.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Machine.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  OS.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  CPU.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Display.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Monitor.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  APM.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Media.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Network.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  NetworkCredentials.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Devices.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Storage.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  USB.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  USBHistory.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Engines.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Disk.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Printers.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Software.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Startup.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  ProcessList.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  MSProduct.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Bluetooth.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Security.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  WIFI.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  WiFiKnownNetworks.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Firewall.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if soEventlog in FSO then
    for i:=0 to EventLog.ContainerCount-1 do begin
      EventLog.SourceName:=EventLog.Containers[i].Name;
      EventLog.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
    end;
  if soAD in FSO then
    ActiveDirectory.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
end;


procedure TMiTeC_SystemInfo.SetFilename(const Value: string);
begin
  FFilename:=Value;
end;

procedure TMiTeC_SystemInfo.SetHeaderReader(const Value: THeaderReader);
begin
  inherited;
  FCPU.OnReadHeader:=Value;
  FMemory.OnReadHeader:=Value;
  FOS.OnReadHeader:=Value;
  FDisk.OnReadHeader:=Value;
  FMachine.OnReadHeader:=Value;
  FNetwork.OnReadHeader:=Value;
  FNetCreds.OnReadHeader:=Value;
  FDisplay.OnReadHeader:=Value;
  FMonitor.OnReadHeader:=Value;
  FMedia.OnReadHeader:=Value;
  FDevices.OnReadHeader:=Value;
  FStorage.OnReadHeader:=Value;
  FEngines.OnReadHeader:=Value;
  FAPM.OnReadHeader:=Value;
  FPrinters.OnReadHeader:=Value;
  FSoftware.OnReadHeader:=Value;
  FStartup.OnReadHeader:=Value;
  FProcessList.OnReadHeader:=Value;
  FUSB.OnReadHeader:=Value;
  FUSBHistory.OnReadHeader:=Value;
  FMSProduct.OnReadHeader:=Value;
  FBT.OnReadHeader:=Value;
  FEL.OnReadHeader:=Value;
  FAD.OnReadHeader:=Value;
  FSC.OnReadHeader:=Value;
  FWIFI.OnReadHeader:=Value;
  FWLANC.OnReadHeader:=Value;
  FFW.OnReadHeader:=Value;
end;

procedure TMiTeC_SystemInfo.SetHeaderWriter(const Value: THeaderWriter);
begin
  inherited;
  FCPU.OnWriteHeader:=Value;
  FMemory.OnWriteHeader:=Value;
  FOS.OnWriteHeader:=Value;
  FDisk.OnWriteHeader:=Value;
  FMachine.OnWriteHeader:=Value;
  FNetwork.OnWriteHeader:=Value;
  FNetCreds.OnWriteHeader:=Value;
  FDisplay.OnWriteHeader:=Value;
  FMonitor.OnWriteHeader:=Value;
  FMedia.OnWriteHeader:=Value;
  FDevices.OnWriteHeader:=Value;
  FStorage.OnWriteHeader:=Value;
  FEngines.OnWriteHeader:=Value;
  FAPM.OnWriteHeader:=Value;
  FPrinters.OnWriteHeader:=Value;
  FSoftware.OnWriteHeader:=Value;
  FStartup.OnWriteHeader:=Value;
  FProcessList.OnWriteHeader:=Value;
  FUSB.OnWriteHeader:=Value;
  FUSBHistory.OnWriteHeader:=Value;
  FMSProduct.OnWriteHeader:=Value;
  FBT.OnWriteHeader:=Value;
  FEL.OnWriteHeader:=Value;
  FAD.OnWriteHeader:=Value;
  FSC.OnWriteHeader:=Value;
  FWIFI.OnWriteHeader:=Value;
  FWLANC.OnWriteHeader:=Value;
  FFW.OnWriteHeader:=Value;
end;

procedure TMiTeC_SystemInfo.SetLiveData(AValue: Boolean);
begin
  inherited SetLiveData(AValue);
end;

procedure TMiTeC_SystemInfo.SetStorageName(const AValue: string);
begin
  inherited SetStorageName(AValue);
end;

end.














