{*******************************************************}
{       MiTeC System Information Component Suite        }
{                   XML Reports                         }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_XML_Reports;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants,
     {$ELSE}
     Windows, SysUtils, Classes,Variants,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs, MiTeC_StrUtils, MiTeC_Routines, MiTeC_Datetime,
     MSI_CPU, MSI_Machine, MSI_Devices, MSI_Display, MSI_Network, MSI_Media,
     MSI_Memory, MSI_Engines, MSI_APM, MSI_Disk, MSI_OS, MSI_SMBIOS,
     MSI_Printers, MSI_Software, MSI_Startup, MSI_Storage, MiTeC_WinIOCTL,
     MSI_USB, MSI_Processes, MSI_Monitor, MiTeC_USB, MSI_SystemInfo, MiTeC_AdvApi,
     MSI_BT, MSI_EventLog, MSI_AD, MSI_Security, MSI_WIFI, MSI_NetCreds, MSI_USBHistory,
     MSI_WLANC;

const
  XSLName = 'msixmlreport.xsl';

procedure ReportHeader(sl: TStrings; AMachine: string = ''; AUser: string = '');
procedure ReportFooter(sl: TStrings);
function CheckXMLValue(AValue: string): string;
procedure SaveXSLTemplate(AFilename: string);

procedure CPU_XML_Report(CPU: TMiTeC_CPU; Standalone: Boolean; sl: TStrings);
procedure APM_XML_Report(APM: TMiTeC_APM; Standalone: Boolean; sl: TStrings);
procedure SMBIOS_XML_Report(SMBIOS: TMiTeC_SMBIOS; Standalone: Boolean; sl: TStrings);
procedure BIOS_XML_Report(BIOS: TMiTeC_BIOS; Standalone: Boolean; sl: TStrings);
procedure Machine_XML_Report(Machine: TMiTeC_Machine; Standalone: Boolean; sl: TStrings);
procedure TimeZone_XML_Report(TimeZone: TMiTeC_TimeZone; Standalone: Boolean; sl: TStrings);
procedure Internet_XML_Report(Internet: TMiTeC_Internet; Standalone: Boolean; sl: TStrings);
procedure LocaleInfo_XML_Report(LocaleInfo: TMiTeC_LocaleInfo; Standalone: Boolean; sl: TStrings);
procedure OperatingSystem_XML_Report(OS: TMiTeC_OperatingSystem; Standalone: Boolean; sl: TStrings);
procedure Storage_XML_Report(Storage: TMiTeC_Storage; Standalone: Boolean; sl: TStrings);
procedure Memory_XML_Report(Memory: TMiTeC_Memory; Standalone: Boolean; sl: TStrings);
procedure Display_XML_Report(Display: TMiTeC_Display; Standalone: Boolean; sl: TStrings);
procedure Monitor_XML_Report(Monitor: TMiTeC_Monitor; Standalone: Boolean; sl: TStrings);
procedure Devices_XML_Report(Devices: TMiTeC_Devices; Standalone: Boolean; sl: TStrings);
procedure USB_XML_Report(USB: TMiTeC_USB; Standalone: Boolean; sl: TStrings);
procedure USBHistory_XML_Report(USBHistory: TMiTeC_USBHistory; Standalone: Boolean; sl: TStrings);
procedure Network_XML_Report(Network: TMiTeC_Network; Standalone: Boolean; sl: TStrings);
procedure NetCreds_XML_Report(NetCreds: TMiTeC_NetCreds; Standalone: Boolean; sl: TStrings);
procedure Media_XML_Report(Media: TMiTeC_Media; Standalone: Boolean; sl: TStrings);
procedure Printers_XML_Report(Printers: TMiTeC_Printers; Standalone: Boolean; sl: TStrings);
procedure Engines_XML_Report(Engines: TMiTeC_Engines; Standalone: Boolean; sl: TStrings);
procedure ProcessList_XML_Report(ProcessList: TMiTeC_ProcessList; Standalone: Boolean; sl: TStrings);
procedure Software_XML_Report(Software: TMiTeC_Software; Standalone: Boolean; sl: TStrings);
procedure Startup_XML_Report(Startup: TMiTeC_Startup; Standalone: Boolean; sl: TStrings);
procedure Disk_XML_Report(Disk: TMiTeC_Disk; Standalone: Boolean; sl: TStrings);
procedure Bluetooth_XML_Report(Bluetooth: TMiTeC_BT; Standalone: Boolean; sl: TStrings);
procedure EventLog_XML_Report(EL: TMiTeC_EventLog; Standalone: Boolean; sl: TStrings);
procedure ActiveDirectory_XML_Report(AD: TMiTeC_AD; Standalone: Boolean; sl: TStrings);
procedure Security_XML_Report(SC: TMiTeC_Security; Standalone: Boolean; sl: TStrings);
procedure WIFI_XML_Report(WIFI: TMiTeC_WIFI; Standalone: Boolean; sl: TStrings);
procedure WLANC_XML_Report(WLANC: TMiTeC_WLANC; Standalone: Boolean; sl: TStrings);

procedure SystemInfo_XML_Report(AComponent: TMiTeC_SystemInfo; sl: TStrings);

{$R *.res}

implementation

uses
  MiTeC_EventLogNT, MiTeC_SysUtils, MiTeC_Storage;

procedure ReportHeader;
begin
  sl.Add('<?xml version="1.0" encoding="utf-8" standalone="yes"?>');
  sl.Add(Format('<?xml-stylesheet type="text/xsl" href="%s"?>',[XSLName]));
  sl.Add(Format('<SystemInfoReport Version="%s"><machine>%s</machine><user>%s</user>',[cVersion,AMachine,AUser]));
end;

procedure ReportFooter;
begin
  sl.Add('</SystemInfoReport>');
end;

procedure SaveXSLTemplate(AFilename: string);
var
  rs: TResourceStream;
  rcd: TStringList;
begin
  rs:=TResourceStream.Create(hinstance,'XSL',RT_RCDATA);
  rcd:=TStringList.Create;
  try
    rcd.LoadFromStream(rs);
    rcd.SaveToFile(AFilename);
  finally
    rs.Free;
    rcd.Free;
  end;
end;

function CheckXMLValue;
var
  i: Integer;
  c: Char;
begin
  Result:=Trim(AValue);
  i:=1;
  while i<=Length(Result) do begin
    c:=Result[i];
      if {$IFDEF UNICODE}CharInSet(c,[#0..#31]){$ELSE}(c in [#0..#31]){$ENDIF} then begin
        Delete(Result,i,1);
        Insert('_',Result,i);
      end else if (c='&') then begin
        Delete(Result,i,1);
        Insert('&amp;',Result,i);
      end else if (c='"') then begin
        Delete(Result,i,1);
        Insert('&quot;',Result,i);
      end else if (c='''') then begin
        Delete(Result,i,1);
        Insert('&apos;',Result,i);
      end else if (c='>') then begin
        Delete(Result,i,1);
        Insert('&gt;',Result,i);
      end else if (c='<') then begin
        Delete(Result,i,1);
        Insert('&lt;',Result,i);
      end else if (c='™') then begin
        Delete(Result,i,1);
        Insert('&#x2122;',Result,i);
      end else if (c='®') then begin
        Delete(Result,i,1);
        Insert('&#x00AE;',Result,i);
      end else if (c='©') then begin
        Delete(Result,i,1);
        Insert('&#xA9;',Result,i);
      end;
    Inc(i);
  end;
  Result:=string({$IFDEF UNICODE}UTF8Encode{$ELSE}AnsiToUTF8{$ENDIF}(Result));
end;

procedure OpenCategory(ATitle: string; sl: TStrings);
begin
  with sl do
    Add(Format('<category title="%s">',[CheckXMLValue(ATitle)]));
end;

procedure CloseCategory(sl: TStrings);
begin
  with sl do
    Add('</category>');
end;

procedure OpenSection(ATitle: string; sl: TStrings);
begin
  with sl do
    Add(Format('  <section title="%s">',[CheckXMLValue(ATitle)]));
end;

procedure CloseSection(sl: TStrings);
begin
  with sl do
    Add('  </section>');
end;

procedure OpenRecordset(ATitle: string; const Fields: array of string; sl: TStrings);
var
  i: Cardinal;
begin
  with sl do begin
    Add(Format('    <recordset title="%s">',[CheckXMLValue(Atitle)]));
    for i:=0 to High(Fields) do
      Add(Format('      <fieldname>%s</fieldname>',[CheckXMLValue(Fields[i])]));
  end;
end;

procedure AddDataRow(const Vals: array of Variant; sl: TStrings);
var
  i: Cardinal;
begin
  with sl do begin
    Add('      <datarow>');
    for i:=0 to High(Vals) do
      Add(Format('        <fieldvalue>%s</fieldvalue>',[CheckXMLValue(VarToStr(Vals[i]))]));
    Add('      </datarow>');
  end;
end;

procedure AddHLDataRow(const Vals: array of Variant; sl: TStrings);
var
  i: Cardinal;
begin
  with sl do begin
    Add('      <datarow hl="yes">');
    for i:=0 to High(Vals) do
      Add(Format('        <fieldvalue>%s</fieldvalue>',[CheckXMLValue(VarToStr(Vals[i]))]));
    Add('      </datarow>');
  end;
end;

procedure CloseRecordset(sl: TStrings);
begin
  with sl do
    Add('    </recordset>');
end;

procedure StringsToRecordset(Source: TStrings; ATitle,AFieldname: string; sl: TStrings);
var
  i: integer;
begin
  OpenRecordset(ATitle,['Property','Value'],sl);
  with sl do begin
    for i:=0 to Source.Count-1 do
      AddDatarow([Format('%s_%d',[AFieldname,i+1]),Source[i]],sl);
  end;
  CloseRecordset(sl);
end;

procedure CPU_XML_Report;
var
  i,j: Integer;
  rh: Boolean;
begin
  with sl, CPU do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
    OpenCategory('CPU',sl);
    for i:=0 to CPUCount-1 do begin
      CPUIndex:=i;
      if LiveData then
        RefreshData
      else begin
        rh:=True;
        LoadFromStorage(StorageFileName,rh,StreamCodeProc);
      end;
      OpenSection(Format('Processor #%d',[i+1]),sl);
        OpenRecordset('General',['Property','Value','Units'],sl);
          case Architecture of
            PROCESSOR_ARCHITECTURE_AMD64: AddDatarow(['Architecture','x64 (AMD or Intel)',''],sl);
            PROCESSOR_ARCHITECTURE_IA32_ON_WIN64: AddDatarow(['Architecture','WOW64',''],sl);
            PROCESSOR_ARCHITECTURE_IA64: AddDatarow(['Architecture','Intel Itanium Processor Family (IPF)',''],sl);
            PROCESSOR_ARCHITECTURE_INTEL: AddDatarow(['Architecture','x86',''],sl);
            else AddDatarow(['Architecture',Architecture,''],sl);
          end;
          AddDatarow(['Vendor',cVendorNames[Vendor].Name,''],sl);
          AddDatarow(['CPU Name',CPUName,''],sl);
          AddDatarow(['Generic Name',GenericName,''],sl);
          AddDatarow(['Marketing Name',MarketingName,''],sl);
          AddDatarow(['Code Name',CodeName,''],sl);
          AddDatarow(['Revision',Revision,''],sl);
          AddDatarow(['Technology',Technology,''],sl);
          AddDatarow(['Brand',Brand,''],sl);
          AddDatarow(['Type',CPUType,''],sl);
          AddDatarow(['FamilyEx',FamilyEx,''],sl);
          AddDatarow(['ModelEx',ModelEx,''],sl);
          AddDatarow(['SteppingEx',SteppingEx,''],sl);
          AddDatarow(['Frequency',Frequency,'MHz'],sl);
          AddDatarow(['Serial Number',SerialNumber,''],sl);
          AddDatarow(['APICID',APICID,''],sl);
          AddDatarow(['Logical Per Package',LogicalPerPackage,''],sl);
          AddDatarow(['Core Per Package',CorePerPackage,''],sl);
          AddDatarow(['Logical Per Core',LogicalPerCore,''],sl);
          AddDatarow(['Physical ID',PhysicalID,''],sl);
          AddDatarow(['Logical ID',LogicalID,''],sl);
        CloseRecordset(sl);

        with Cache.Level1.Code do
          if Size>0 then begin
            OpenRecordset('Cache Level 1 Code',['Property','Value','Units'],sl);
              AddDatarow(['SharedWays',SharedWays,''],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              if Associativity=caNone then
                AddDatarow(['Ways',Ways,''],sl)
              else
                AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);
              AddDatarow(['LineSize',LineSize,'entries'],sl);
            CloseRecordset(sl);
          end;
        with Cache.Level1.Data do
          if Size>0 then begin
            OpenRecordset('Cache Level 1 Data',['Property','Value','Units'],sl);
              AddDatarow(['SharedWays',SharedWays,''],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              if Associativity=caNone then
                AddDatarow(['Ways',Ways,''],sl)
              else
                AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);
              AddDatarow(['LineSize',LineSize,'entries'],sl);
            CloseRecordset(sl);
          end;
        with Cache.Level2 do
          if Size>0 then begin
            OpenRecordset('Cache Level 2',['Property','Value','Units'],sl);
              AddDatarow(['SharedWays',SharedWays,''],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              if Associativity=caNone then
                AddDatarow(['Ways',Ways,''],sl)
              else
                AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);
              AddDatarow(['LineSize',LineSize,'entries'],sl);
            CloseRecordset(sl);
          end;
        with Cache.Level3 do
          if Size>0 then begin
            OpenRecordset('Cache Level 3',['Property','Value','Units'],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);AddDatarow(['SharedWays',SharedWays,''],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              if Associativity=caNone then
                AddDatarow(['Ways',Ways,''],sl)
              else
                AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);
              AddDatarow(['LineSize',LineSize,'entries'],sl);
            CloseRecordset(sl);
          end;
        with Cache.Trace do
          if Size>0 then begin
            OpenRecordset('Cache Trace',['Property','Value','Units'],sl);
              AddDatarow(['Size',Size,'KB'],sl);
              AddDatarow(['Associativity',cAssociativityDescription[Associativity],''],sl);
              AddDatarow(['LineSize',LineSize,'entries'],sl);
            CloseRecordset(sl);
          end;

        with Features.Standard1 do
          if Count>0 then begin
            OpenRecordset('Standard-1 Features',['Property','Value'],sl);
            for j:=0 to Count-1 do
              AddDatarow([Format('%s - %s',[Features[j].Mnemonic,Features[j].Name]),Integer(Features[j].Available)],sl);
            CloseRecordset(sl);
          end;
        with Features.Standard2 do
          if Count>0 then begin
            OpenRecordset('Standard-2 Features',['Property','Value'],sl);
            for j:=0 to Count-1 do
              AddDatarow([Format('%s - %s',[Features[j].Mnemonic,Features[j].Name]),Integer(Features[j].Available)],sl);
            CloseRecordset(sl);
          end;
        with Features.Extended1 do
          if Count>0 then begin
            OpenRecordset('Extended-1 Features',['Property','Value'],sl);
            for j:=0 to Count-1 do
              AddDatarow([Format('%s - %s',[Features[j].Mnemonic,Features[j].Name]),Integer(Features[j].Available)],sl);
            CloseRecordset(sl);
          end;
        with Features.Extended2 do
          if Count>0 then begin
            OpenRecordset('Extended-2 Features',['Property','Value'],sl);
            for j:=0 to Count-1 do
              AddDatarow([Format('%s - %s',[Features[j].Mnemonic,Features[j].Name]),Integer(Features[j].Available)],sl);
            CloseRecordset(sl);
          end;
        with Features.PowerManagement do
          if Count>0 then begin
            OpenRecordset('Power Management Features',['Property','Value'],sl);
            for j:=0 to Count-1 do
              AddDatarow([Format('%s - %s',[Features[j].Mnemonic,Features[j].Name]),Integer(Features[j].Available)],sl);
            CloseRecordset(sl);
          end;
      CloseSection(sl);
    end;
    CloseCategory(sl);
    if Standalone then ReportFooter(sl);
  end;
end;

procedure APM_XML_Report;
begin
  with sl, APM do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Advanced Power Management',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['AC Power Status',GetACPSStr(ACPowerStatus)],sl);
          AddDatarow(['Battery Charge Status',GetBSStr(BatteryChargeStatus)],sl);
    if BatteryLifePercent<=100 then begin
          AddDatarow(['Battery Full Time',FormatSeconds(BatteryLifeFullTime)],sl);
          AddDatarow(['Battery Life Time',FormatSeconds(BatteryLifeTime)],sl);
    end;
        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);
    if Standalone then ReportFooter(sl);
  end;
end;

procedure SMBIOS_XML_Report;
var
  i: Integer;
begin
  with SMBIOS, sl do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
    OpenCategory('System Management BIOS',sl);
      OpenSection('BIOS',sl);
        OpenRecordset('',['Property','Value','Units'],sl);
          AddDatarow(['Vendor',BIOSVendor,''],sl);
          AddDatarow(['Version',BIOSVersion,''],sl);
          AddDatarow(['Date',BIOSDate,''],sl);
          AddDatarow(['Size',BIOSSize,'KB'],sl);
          AddDatarow(['System Version',Format('%d.%d',[BIOSMajorVersion,BIOSMinorVersion]),''],sl);
          AddDatarow(['Embedded Controller Firmware Version',Format('%d.%d',[BIOS_ECF_MajorVersion,BIOS_ECF_MinorVersion]),''],sl);
          AddDatarow(['Characteristics',GetBIOSCharStr(BIOSCharacteristics),''],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('System',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Model',SystemModel],sl);
          AddDatarow(['Manufacturer',SystemManufacturer],sl);
          AddDatarow(['Version',SystemVersion],sl);
          AddDatarow(['Serial',SystemSerial],sl);
          AddDatarow(['UUID',SystemUUID],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Mainboard',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Model',MainBoardModel],sl);
          AddDatarow(['Manufacturer',MainBoardManufacturer],sl);
          AddDatarow(['Version',MainBoardVersion],sl);
          AddDatarow(['Serial',MainBoardSerial],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Chassis',sl);
        OpenRecordset('',['Property','Value'],sl);
          try AddDatarow(['Model',ChassisTypes[ChassisModel]],sl); except end;
          AddDatarow(['Manufacturer',ChassisManufacturer],sl);
          AddDatarow(['Version',ChassisVersion],sl);
          AddDatarow(['Serial',ChassisSerial],sl);
          AddDatarow(['AssetTag',ChassisAssetTag],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Memory Controller',sl);
        OpenRecordset('',['Property','Value','Units'],sl);
          try AddDatarow(['Supported Interleave',InterleaveSupports[MemCtrlSupportedInterleave],''],sl); except end;
          try AddDatarow(['Active Interleave',InterleaveSupports[MemCtrlCurrentInterleave],''],sl); except end;
          AddDatarow(['Max Module Size',MemCtrlMaxSize,'MB'],sl);
          AddDatarow(['Supported Speeds',GetMemorySpeedStr(MemCtrlSupportedSpeeds),''],sl);
          AddDatarow(['Supported Types',GetMemoryTypeStr(MemCtrlSupportedTypes),''],sl);
          AddDatarow(['Supported Voltages',GetMemoryVoltageStr(MemCtrlSupportedVoltages),''],sl);
          AddDatarow(['Slot Count',MemCtrlSlotCount,''],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      if ProcessorCount>0 then begin
        OpenSection('CPU',sl);
        for i:=0 to ProcessorCount-1 do begin
          OpenRecordset(Format('Processor #%d',[i+1]),['Property','Value','Units'],sl);
            AddDatarow(['Socket',Processor[i].Socket,''],sl);
            try AddDatarow(['Upgrade interface',Upgrades[Processor[i].Upgrade],''],sl); except end;
            AddDatarow(['Manufacturer',Processor[i].Manufacturer,''],sl);
            AddDatarow(['Version',Processor[i].Version,''],sl);
            AddDatarow(['Voltage',Processor[i].Voltage,'V'],sl);
            AddDatarow(['Frequency',Processor[i].Frequency,'MHz'],sl);
            AddDatarow(['External Frequency',Processor[i].ExternalClock,'MHz'],sl);
            AddDatarow(['Serial Number',Processor[i].SerialNumber,''],sl);
            AddDatarow(['Asset Tag',Processor[i].AssetTag,''],sl);
            AddDatarow(['Part Number',Processor[i].PartNumber,''],sl);
          CloseRecordset(sl);
        end;
        CloseSection(sl);
      end;
      if CacheCount>0 then begin
        OpenSection('Cache',sl);
        for i:=0 to CacheCount-1 do begin
          OpenRecordset(Cache[i].Designation,['Property','Value','Units'],sl);
            AddDatarow(['Socket',Cache[i].Designation],sl);
            AddDatarow(['Installed Size',Cache[i].InstalledSize,'KB'],sl);
            AddDatarow(['Max Size',Cache[i].MaxSize,'KB'],sl);
            AddDatarow(['Speed',Cache[i].Speed,'ns'],sl);
            try AddDatarow(['type',CacheTypes[Cache[i].Typ],''],sl); except end;
            try AddDatarow(['Associativity',CacheAssociativities[Cache[i].Associativity],''],sl); except end;
            try AddDatarow(['SRAM type',SRAMTypes[Cache[i].SRAMType],''],sl); except end;
          CloseRecordset(sl);
        end;
        CloseSection(sl);
      end;
      if MemoryModuleCount>0 then begin
        OpenSection('Memory Modules',sl);
        for i:=0 to MemoryModuleCount-1 do begin
          OpenRecordset(Format('Module %d',[i+1]),['Property','Value','Units'],sl);
            AddDatarow(['Bank',MemoryModule[i].Socket,''],sl);
            AddDatarow(['Type',GetMemoryTypeStr(MemoryModule[i].Types),''],sl);
            AddDatarow(['Size',MemoryModule[i].Size,'MB'],sl);
            AddDatarow(['Speed',MemoryModule[i].Speed,'ns'],sl);
          CloseRecordset(sl);
        end;
        CloseSection(sl);
      end;
      if MemoryDeviceCount>0 then begin
        OpenSection('Memory Devices',sl);
        for i:=0 to MemoryDeviceCount-1 do begin
          OpenRecordset(Format('Device %d',[i+1]),['Property','Value','Units'],sl);
            AddDatarow(['Device Locator',MemoryDevice[i].DeviceLocator,''],sl);
            AddDatarow(['Bank Locator',MemoryDevice[i].BankLocator,''],sl);
            AddDatarow(['Manufacturer',MemoryDevice[i].Manufacturer,''],sl);
            AddDatarow(['Serial Number',MemoryDevice[i].SerialNumber,''],sl);
            AddDatarow(['Asset Tag',MemoryDevice[i].AssetTag,''],sl);
            AddDatarow(['Part Number',MemoryDevice[i].PartNumber,''],sl);
            try AddDatarow(['Device',MemoryDeviceTypes[MemoryDevice[i].Device],''],sl); except end;
            AddDatarow(['Type Details',GetMemoryTypeDetailsStr(MemoryDevice[i].TypeDetails),''],sl);
            try AddDatarow(['Form Factor',MemoryFormFactors[MemoryDevice[i].FormFactor],''],sl); except end;
            AddDatarow(['Size',MemoryDevice[i].Size,'MB'],sl);
            AddDatarow(['Speed',MemoryDevice[i].Speed,'MHz'],sl);
            AddDatarow(['Total Width',MemoryDevice[i].TotalWidth,'b'],sl);
            AddDatarow(['Data Width',MemoryDevice[i].DataWidth,'b'],sl);
          CloseRecordset(sl);
        end;
        CloseSection(sl);
      end;
      if PortCount>0 then begin
        OpenSection('Port Slots',sl);
        for i:=0 to PortCount-1 do begin
          OpenRecordset(Port[i].InternalDesignator,['Property','Value'],sl);
            try AddDatarow(['Type',PortTypes[Port[i].Typ]],sl); except end;
            AddDatarow(['Internal Designator',Port[i].InternalDesignator],sl);
            try AddDatarow(['Internal Connector',ConnectorTypes[Port[i].InternalConnector]],sl); except end;
            AddDatarow(['External Designator',Port[i].ExternalDesignator],sl);
            try AddDatarow(['External Connector',ConnectorTypes[Port[i].ExternalConnector]],sl); except end;
          CloseRecordset(sl);
        end;
        CloseSection(sl);
      end;
    if SystemSlotCount>0 then begin
      OpenSection('System Slots',sl);
      for i:=0 to SystemSlotCount-1 do begin
        OpenRecordset(SystemSlot[i].Designation,['Property','Value'],sl);
          AddDatarow(['Designation',SystemSlot[i].Designation],sl);
          try AddDatarow(['Type',SlotTypes[SystemSlot[i].Typ]],sl); except end;
          try AddDatarow(['Data Bus',DataBusTypes[SystemSlot[i].DataBus]],sl); except end;
          try AddDatarow(['Current Usage',SlotUsages[SystemSlot[i].Usage]],sl); except end;
          try AddDatarow(['Slot Length',SlotLengths[SystemSlot[i].Length]],sl); except end;
        CloseRecordset(sl);
      end;
      CloseSection(sl);
    end;
    if OnBoardDeviceCount>0 then begin
      OpenSection('On-Board Devices',sl);
      for i:=0 to OnBoardDeviceCount-1 do begin
        OpenRecordset(OnBoardDevice[i].DeviceName,['Property','Value'],sl);
          try AddDatarow(['Type',OnBoardDeviceTypes[OnBoardDevice[i].Typ]],sl); except end;
          AddDatarow(['Status',Integer(OnBoardDevice[i].Status)],sl);
        CloseRecordset(sl);
      end;
      CloseSection(sl);
    end;
    CloseCategory(sl);
    if Standalone then ReportFooter(sl);
  end;
end;

procedure BIOS_XML_Report;
begin
  with sl, BIOS do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('BIOS',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Name',Name],sl);
          AddDatarow(['Copyright',Copyright],sl);
          AddDatarow(['Date',Date],sl);
          AddDatarow(['Extended Info',ExtendedInfo],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);
    
    if Standalone then ReportFooter(sl);
  end;
end;

procedure Machine_XML_Report;
begin
  with sl, Machine do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
    OpenCategory('Machine',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Computer',Computer],sl);
          AddDatarow(['Name',MachineName],sl);
          AddDatarow(['JoinedTo',JoinedTo],sl);
          AddDatarow(['User',User],sl);
          AddDatarow(['Session',GetSessionStr(Session)],sl);
          if (stTerminal in Session) or (stCitrix in Session) then
            AddDatarow(['Session Protocol',cSessionProto[SessionProtocol]],sl);
          AddDatarow(['Last Boot',DateTimeToStrDef(LastBoot)],sl);
          AddDatarow(['System UpTime',FormatSeconds(SystemUpTime)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);

    BIOS_XML_Report(BIOS,False,sl);
    SMBIOS_XML_Report(SMBIOS,False,sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure TimeZone_XML_Report;
begin
  with sl, TimeZone do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Time Zone',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value','Units'],sl);
          AddDatarow(['Name',DisplayName,''],sl);
          AddDatarow(['Standard Name',DateTimeToStrDef(StandardStart),''],sl);
          AddDatarow(['Standard Bias',StandardBias,'min'],sl);
          AddDatarow(['Daylight Name',DateTimeToStrDef(DaylightStart),''],sl);
          AddDatarow(['Daylight Bias',DaylightBias,'min'],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Internet_XML_Report;
var
  i: Integer;
begin
  with sl, Internet do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Internet',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Default Browser',DefaultBrowser],sl);
          AddDatarow(['Default Mail Client',DefaultMailClient],sl);
          AddDatarow(['Connection',GetConnTypeStr(ConnectionType)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      if ProxyServer.Count>0 then begin
        OpenSection('Proxy Server',sl);
          OpenRecordset('',['Property','Value'],sl);
          for i:=0 to ProxyServer.Count-1 do
            AddDatarow([ProxyServer.Names[i],ProxyServer.Values[ProxyServer.Names[i]]],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure LocaleInfo_XML_Report;
var
  s: string;
begin
  with sl, LocaleInfo do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Locale Information',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Abbreviate Country Code',AbbreviateCountryCode],sl);
          AddDatarow(['Abbreviate Language Code',AbbreviateLanguageName],sl);
          AddDatarow(['Country Code',CountryCode],sl);
          AddDatarow(['Currency Decimal Digits',CurrencyDecimalDigits],sl);
          AddDatarow(['Full Country Code',FullCountryCode],sl);
          AddDatarow(['Full English Language Name',FullLanguageEnglishName],sl);
          AddDatarow(['Full Localize Language Name',FullLocalizeLanguage],sl);
          AddDatarow(['International Monetary Symbol',InternationalMonetarySymbol],sl);
          AddDatarow(['Local Monetary Symbol',LocalMonetarySymbol],sl);
          case PositiveCurrencyMode of
            Prefix_No_Separation: s:='Prefix_No_Separation';
            Suffix_No_Separation: s:='Suffix_No_Separation';
            Prefix_One_Char_Separation: s:='Prefix_One_Char_Separation';
            Suffix_One_Char_Separation: s:='Suffix_One_Char_Separation';
          end;
          AddDatarow(['Positive Currency Mode',s],sl);
          AddDatarow(['Negative Currency Mode',NegativeCurrencyMode],sl);
          AddDatarow(['Currency Decimal Separator',CurrencyDecimalSeparator],sl);
          AddDatarow(['Currency Thousand Separator',CurrencyThousandSeparator],sl);
          AddDatarow(['Decimal Separator',DecimalSeparator],sl);
          AddDatarow(['Number Of Decimal Digits',NumberOfDecimalDigits],sl);
          AddDatarow(['List Separator',ListSeparator],sl);
          AddDatarow(['Date Separator',DateSeparator],sl);
          case LongDateOrder of
            MDY: s:='M-D-Y';
            DMY: s:='D-M-Y';
            YMD: s:='Y-M-D';
          end;
          AddDatarow(['Long Date Order',s],sl);
          case ShortDateOrder of
            MDY: s:='M-D-Y';
            DMY: s:='D-M-Y';
            YMD: s:='Y-M-D';
          end;
          AddDatarow(['Short Date Order',s],sl);
          AddDatarow(['Short Date Format',ShortDateFormat],sl);
          AddDatarow(['Long Date Format',LongDateFormat],sl);
          AddDatarow(['Time Format',TimeFormat],sl);
          case TimeFormatSpecifier of
            H12: s:='12H';
            H24: s:='24H';
          end;
          AddDatarow(['Time Format Specifier',s],sl);
          case YearFormat of
            TwoDigit: s:='Two Digit';
            FourDigit: s:='Four Digit';
          end;
          AddDatarow(['Year Format',s],sl);
          case MeasurementSystem of
            Metric: s:='Metric';
            US: s:='US';
          end;
          AddDatarow(['Measurement System',s],sl);
       CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);
    
    if Standalone then ReportFooter(sl);
  end;
end;

procedure OperatingSystem_XML_Report;
var
  i: Integer;
begin
  with sl, OS do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Operating System',sl);
      OpenSection('General',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Name',OSName],sl);
          AddDatarow(['Edition',OSEdition],sl);
          AddDatarow(['Major Version',MajorVersion],sl);
          AddDatarow(['Minor Version',MinorVersion],sl);
          AddDatarow(['Build',BuildNumber],sl);
          AddDatarow(['CSD Version',CSD],sl);
          AddDatarow(['ServicePackMajorVersion',ServicePackMajorVersion],sl);
          AddDatarow(['ServicePackMinorVersion',ServicePackMinorVersion],sl);
          AddDatarow(['Product ID',ProductID],sl);
          AddDatarow(['Product Key',ProductKey],sl);
          AddDatarow(['Install Date',DateTimeToStrDef(InstallDate)],sl);
          AddDatarow(['Registered User',RegisteredUser],sl);
          AddDatarow(['Registered Organisation',RegisteredOrg],sl);
          AddDatarow(['DVD Region',DVDRegion],sl);
          AddDatarow(['NumberOfLicensedUsers',NumberOfLicensedUsers],sl);
          AddDatarow(['NumberOfUsers',NumberOfUsers],sl);
          AddDatarow(['GenuineWindows',GenuineWindows],sl);
          AddDatarow(['UserAccountControl',UserAccountControl],sl);
          AddDatarow(['Virtualization',Virtualization],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Installed suites',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['MS Small Business Server',integer(suSmallBusiness in InstalledSuites)],sl);
          AddDatarow(['MS Small Business Server With Restrictive Client License In Force',integer(suSmallBusinessRestricted in InstalledSuites)],sl);
          AddDatarow(['Windows 2000 Advanced Server',integer(suEnterprise in InstalledSuites)],sl);
          AddDatarow(['Windows 2000 Datacenter Server',integer(suDataCenter in InstalledSuites)],sl);
          AddDatarow(['MS BackOffice Components',integer(suBackOffice in InstalledSuites)],sl);
          AddDatarow(['Communications',integer(suCommunications in InstalledSuites)],sl);
          AddDatarow(['Terminal Services',integer(suTerminal in InstalledSuites)],sl);
          AddDatarow(['Embedded NT',integer(suEmbeddedNT in InstalledSuites)],sl);
          AddDatarow(['Service Pack',Format('%d.%d',[ServicePackMajorVersion,ServicePackMinorVersion])],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('HotFixes',sl);
        OpenRecordset('',['Property','Value'],sl);
        for i:=0 to UpdateCount-1 do
          AddDatarow([Updates[i].ID,Updates[i].Description],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Environment',sl);
        OpenRecordset('',['Property','Value'],sl);
        for i:=0 to Environment.Count-1 do
          AddDatarow([Environment.Names[i],Environment.Values[Environment.Names[i]]],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Folders',sl);
        OpenRecordset('',['Property','Value'],sl);
        for i:=0 to Folders.Count-1 do
          AddDatarow([Folders.Names[i],Folders.Values[Folders.Names[i]]],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);

    TimeZone_XML_Report(TimeZone,False,sl);
    Internet_XML_Report(Internet,False,sl);
    LocaleInfo_XML_Report(LocaleInfo,False,sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Storage_XML_Report;
var
  i,j: Integer;
  s: string;
begin
  with sl, Storage do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Storage devices',sl);

    for i:=0 to PhysicalCount-1 do begin
      with Physical[i] do begin
        s:=Format('%s: %s %s',[GetDeviceTypeStr(DeviceType),Model,Revision]);
        OpenSection(s,sl);
          OpenRecordset('General',['Property','Value','Units'],sl);
            AddDatarow(['Serial Number',SerialNumber,''],sl);
            AddDatarow(['Model',Model,''],sl);
            AddDatarow(['Revision',Revision,''],sl);
            AddDatarow(['SCSI Adapter',HaId,''],sl);
            AddDatarow(['Bus',PathId,''],sl);
            AddDatarow(['Target Device',Target,''],sl);
            AddDatarow(['LUN',Lun,''],sl);
            AddDatarow(['Temperature',Temperature,'°C'],sl);
            AddDatarow(['Cylinders',Geometry.Cylinders.QuadPart,''],sl);
            AddDatarow(['Heads',Geometry.TracksPerCylinder,''],sl);
            AddDatarow(['Sectors Per Track',Geometry.SectorsPerTrack,''],sl);
            AddDatarow(['Bytes Per Sector',Geometry.BytesPerSector,''],sl);
            AddDatarow(['ECC Code',ECCCode,''],sl);
            AddDatarow(['Buffer Size',CtlBufSize,'B'],sl);
            AddDatarow(['Capacity',Size,'B'],sl);
            if IdentifyDeviceData.MajorRevision>0 then
              AddDatarow(['ATA Major Version',GetATAMajorVersion(IdentifyDeviceData.MajorRevision),''],sl);
            if IdentifyDeviceData.MinorRevision>0 then
              AddDatarow(['ATA Minor Version',GetATAMinorVersion(IdentifyDeviceData.MinorRevision),''],sl);
            if IdentifyDeviceData.ReservedWord220[2]>0 then
              AddDatarow(['ATA Transport Version',GetATATransportVersion(IdentifyDeviceData.ReservedWord220[2]),''],sl);
            AddDatarow(['SMART Support',Integer(SMARTSupport),''],sl);
            AddDatarow(['SMART Active',Integer(SMARTActive),''],sl);
          CloseRecordSet(sl);
      end;
          OpenRecordset('Logical units',['Property','Value','Units'],sl);
      for j:=0 to LogicalCount-1 do
        with Logical[j] do
          if PhysicalIndex=i then begin
            AddHLDatarow(['Drive',Format('%s:',[Drive]),''],sl);
            if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then begin
              AddDatarow(['Partition Number',Layout[LayoutIndex].Number,''],sl);
              AddDatarow(['Starting Offset',Layout[LayoutIndex].StartingOffset.QuadPart,''],sl);
              AddDatarow(['Partition Length',Layout[LayoutIndex].Length.QuadPart,'B'],sl);
              AddDatarow(['Hidden Sectors',Layout[LayoutIndex].HiddenSectors,''],sl);
              AddDatarow(['Partition Type',GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ),''],sl);
              AddDatarow(['Partition File System',GetPartitionSystem(Layout[LayoutIndex].Typ),''],sl);
              AddDatarow(['Boot Indicator',Integer(Layout[LayoutIndex].BootIndicator),''],sl);
              AddDatarow(['Recognized Partition',Integer(Layout[LayoutIndex].Recognized),''],sl);
              AddDatarow(['Rewrite Partition',Integer(Layout[LayoutIndex].Rewrite),''],sl);
            end;
          end;
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Memory_XML_Report;
begin
  with sl, Memory do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Memory',sl);
      OpenSection('',sl);
        OpenRecordset('',['Property','Value','Units'],sl);
          AddDatarow(['Physical Total',PhysicalTotal,'B'],sl);
          AddDatarow(['Physical Free',PhysicalFree,'B'],sl);
          AddDatarow(['PageFile Total',PageFileTotal,'B'],sl);
          AddDatarow(['PageFile Free',PageFileFree,'B'],sl);
          AddDatarow(['Virtual Total',VirtualTotal,'B'],sl);
          AddDatarow(['Virtual Free',VirtualFree,'B'],sl);
          AddDatarow(['Allocation Granularity',AllocGranularity,'B'],sl);
          AddDatarow(['Min App Address',MinAppAddress,''],sl);
          AddDatarow(['Max App Address',MaxAppAddress,''],sl);
          AddDatarow(['Page Size',PageSize,'B'],sl);

        CloseRecordset(sl);
      CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Display_XML_Report;
var
  i: Integer;
begin
  with sl, Display do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Display',sl);
      for i:=0 to AdapterCount-1 do begin
        OpenSection(Adapter[i].Name,sl);
          OpenRecordset('',['Property','Value','Units'],sl);
            AddDatarow(['Name',Adapter[i].Name,''],sl);
            AddDatarow(['DAC',Adapter[i].DAC,''],sl);
            AddDatarow(['Chipset',Adapter[i].Chipset,''],sl);
            AddDatarow(['BIOS',Adapter[i].BIOS,''],sl);
            AddDatarow(['Memory',Adapter[i].Memory,'B'],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
      OpenSection('General',sl);
        OpenRecordset('',['Property','Value','Units'],sl);
          AddDatarow(['BIOS Version',BIOSVersion,''],sl);
          AddDatarow(['BIOS Date',BIOSDate,''],sl);
          AddDatarow(['BIOS String',BIOSString,''],sl);
          AddDatarow(['Technology',Technology,''],sl);
          AddDatarow(['Device Driver Version',DeviceDriverVersion,''],sl);
          AddDatarow(['Horizontal Resolution',HorzRes,'px'],sl);
          AddDatarow(['Vertical Resolution',VertRes,'px'],sl);
          AddDatarow(['Horizontal Size',HorzSize,'mm'],sl);
          AddDatarow(['Vertical Size',VertSize,'mm'],sl);
          AddDatarow(['Color Depth',ColorDepth,'b'],sl);
          AddDatarow(['Pixel Width',PixelWidth,''],sl);
          AddDatarow(['Pixel Height',PixelHeight,''],sl);
          AddDatarow(['Pixel Diagonal',PixelDiagonal,''],sl);
          AddDatarow(['Font Resolution',FontResolution,'dpi'],sl);
          AddDatarow(['Vertical Refresh Rate',VerticalRefreshRate,'Hz'],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Video Modes',sl);
        StringsToRecordset(Modes,'','Mode',sl);
      CloseSection(sl);

      OpenSection('Curve Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Circles',integer(ccCircles in CurveCaps)],sl);
          AddDatarow(['PieWedges',integer(ccPieWedges in CurveCaps)],sl);
          AddDatarow(['Chords',integer(ccChords in CurveCaps)],sl);
          AddDatarow(['Ellipses',integer(ccEllipses in CurveCaps)],sl);
          AddDatarow(['WideBorders',integer(ccWideBorders in CurveCaps)],sl);
          AddDatarow(['StyledBorders',integer(ccStyledBorders in CurveCaps)],sl);
          AddDatarow(['WideAndStyledBorders',integer(ccWideStyledBorders in CurveCaps)],sl);
          AddDatarow(['Interiors',integer(ccInteriors in CurveCaps)],sl);
          AddDatarow(['RoundedRectangles',integer(ccRoundedRects in CurveCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Line Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Polylines',integer(lcPolylines in LineCaps)],sl);
          AddDatarow(['Markers',integer(lcMarkers in LineCaps)],sl);
          AddDatarow(['MultipleMarkers',integer(lcMultipleMarkers in LineCaps)],sl);
          AddDatarow(['WideLines',integer(lcWideLines in LineCaps)],sl);
          AddDatarow(['StyledLines',integer(lcStyledLines in LineCaps)],sl);
          AddDatarow(['WideAndStyledLines',integer(lcWideStyledLines in LineCaps)],sl);
          AddDatarow(['Interiors',integer(lcInteriors in LineCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Polygon Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['AlternateFillPolygons',integer(pcAltFillPolygons in PolygonCaps)],sl);
          AddDatarow(['Rectangles',integer(pcRectangles in PolygonCaps)],sl);
          AddDatarow(['WindingFillPolygons',integer(pcWindingFillPolygons in PolygonCaps)],sl);
          AddDatarow(['SingleScanlines',integer(pcSingleScanlines in PolygonCaps)],sl);
          AddDatarow(['WideBorders',integer(pcWideBorders in PolygonCaps)],sl);
          AddDatarow(['StyledBorders',integer(pcStyledBorders in PolygonCaps)],sl);
          AddDatarow(['WideAndStyledBorders',integer(pcWideStyledBorders in PolygonCaps)],sl);
          AddDatarow(['Interiors',integer(pcInteriors in PolygonCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Raster Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['RequiresBanding',integer(rcRequiresBanding in RasterCaps)],sl);
          AddDatarow(['BitmapsTransferSupport',integer(rcTransferBitmaps in RasterCaps)],sl);
          AddDatarow(['LargeBitmapSupport',integer(rcBitmaps64K in RasterCaps)],sl);
          AddDatarow(['SetGetDIBitsSupport',integer(rcSetGetDIBits in RasterCaps)],sl);
          AddDatarow(['SetDIBitsToDeviceSupport',integer(rcSetDIBitsToDevice in RasterCaps)],sl);
          AddDatarow(['FloodfillsSupport',integer(rcFloodfills in RasterCaps)],sl);
          AddDatarow(['Win20FeaturesSupport',integer(rcWindows2xFeatures in RasterCaps)],sl);
          AddDatarow(['PaletteBased',integer(rcPaletteBased in RasterCaps)],sl);
          AddDatarow(['Scaling',integer(rcScaling in RasterCaps)],sl);
          AddDatarow(['StretchBltSupport',integer(rcStretchBlt in RasterCaps)],sl);
          AddDatarow(['StretchDIBitsSupport',integer(rcStretchDIBits in RasterCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Text Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['CapableOfCharacterOutputPrecision',integer(tcCharOutPrec in TextCaps)],sl);
          AddDatarow(['CapableOfStrokeOutputPrecision',integer(tcStrokeOutPrec in TextCaps)],sl);
          AddDatarow(['CapableOfStrokeClipPrecision',integer(tcStrokeClipPrec in TextCaps)],sl);
          AddDatarow(['Suports90DegreeCharacterRotation',integer(tcCharRotation90 in TextCaps)],sl);
          AddDatarow(['SupportsCharacterRotationToAnyAngle',integer(tcCharRotationAny in TextCaps)],sl);
          AddDatarow(['XYScaleIndependent',integer(tcScaleIndependent in TextCaps)],sl);
          AddDatarow(['SupportsDoubledCharacterScaling',integer(tcDoubledCharScaling in TextCaps)],sl);
          AddDatarow(['SupportsIntegerMultiplesOnlyWhenScaling',integer(tcIntMultiScaling in TextCaps)],sl);
          AddDatarow(['SupportsAnyMultiplesForExactCharacterScaling',integer(tcAnyMultiExactScaling in TextCaps)],sl);
          AddDatarow(['SupportsDoubleWeightCharacters',integer(tcDoubleWeightChars in TextCaps)],sl);
          AddDatarow(['SupportsItalics',integer(tcItalics in TextCaps)],sl);
          AddDatarow(['SupportsUnderlines',integer(tcUnderlines in TextCaps)],sl);
          AddDatarow(['SupportsStrikeouts',integer(tcStrikeouts in TextCaps)],sl);
          AddDatarow(['SupportsRasterFonts',integer(tcRasterFonts in TextCaps)],sl);
          AddDatarow(['SupportsVectorFonts',integer(tcVectorFonts in TextCaps)],sl);
          AddDatarow(['CannotScrollUsingBlts',integer(tcNoScrollUsingBlts in TextCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Shade Blend Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Source Constant Alpha handling',integer(sbcConstAlpha in ShadeBlendCaps)],sl);
          AddDatarow(['GradientFill rectangles',integer(sbcGradRect in ShadeBlendCaps)],sl);
          AddDatarow(['GradientFill triangles',integer(sbcGradTri in ShadeBlendCaps)],sl);
          AddDatarow(['Per-pixel alpha handling',integer(sbcPixelAlpha in ShadeBlendCaps)],sl);
          AddDatarow(['Premultiplied alpha handling',integer(sbcPremultAlpha in ShadeBlendCaps)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
      OpenSection('Color Management Capabilities',sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['CMYK color space ICC color profile',integer(cmcCMYKColor in ColorMgmtCaps)],sl);
          AddDatarow(['ICM performation',integer(cmcDeviceICM in ColorMgmtCaps)],sl);
          AddDatarow(['Gamma Ramp support',integer(cmcGammaRamp in ColorMgmtCaps)],sl);
        CloseRecordset(sl);
     CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Monitor_XML_Report;
var
  i: Integer;
begin
  with sl, Monitor do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Monitors',sl);
    if Monitor.Count>0 then begin
      for i:=0 to Monitor.Count-1 do begin
        OpenSection(Monitors[i].DeviceDescription,sl);
          OpenRecordset('',['Property','Value','Units'],sl);
            AddDatarow(['Name',Monitors[i].EDID.Name,''],sl);
            AddDatarow(['ProductNumber',Monitors[i].EDID.ProductNumber,''],sl);
            AddDatarow(['Model',Monitors[i].Model,''],sl);
            AddDatarow(['Manufacturer',Monitors[i].Manufacturer,''],sl);
            AddDatarow(['EDID',Monitors[i].EDID.Version,''],sl);
            AddDatarow(['Width',Monitors[i].EDID.Width,'cm'],sl);
            AddDatarow(['Height',Monitors[i].EDID.Height,'cm'],sl);
            AddDatarow(['Gamma',Monitors[i].EDID.Gamma,''],sl);
            AddDatarow(['Serial Number',Monitors[i].EDID.SerialNumber,''],sl);
            AddDatarow(['Year',Monitors[i].EDID.Year,''],sl);
            AddDatarow(['Week',Monitors[i].EDID.Week,''],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Bluetooth_XML_Report;
var
  i: Integer;
begin
  with sl, Bluetooth do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Bluetooth',sl);
    if DeviceCount>0 then begin
      for i:=0 to DeviceCount-1 do begin
        OpenSection(Devices[i].Name,sl);
          OpenRecordset('',['Property','Value'],sl);
            AddDatarow(['Last seen',DateTimeToStrDef(Devices[i].LastSeen)],sl);
            AddDatarow(['Last used',DateTimeToStrDef(Devices[i].LastUsed)],sl);
            AddDatarow(['Authenticated',Devices[i].Authenticated],sl);
            AddDatarow(['Remembered',Devices[i].Remembered],sl);
            AddDatarow(['Connected',Devices[i].Connected],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Devices_XML_Report;
var
  j,c: integer;
  s,lc: string;
begin
  with sl, Devices do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
    lc:='';
    c:=0;
    OpenCategory('Devices',sl);
    for j:=0 to DeviceCount-1 do begin
      if not SameText(Devices[j].ClassName,lc) then begin
        lc:=Devices[j].ClassName;
        c:=0;
        if Devices[j].ClassDesc<>'' then
          s:=Devices[j].ClassDesc
        else
          s:=Devices[j].ClassName;
        OpenSection(s,sl);
      end;
      Inc(c);
      OpenRecordset(Devices[j].Name,['Property','Value'],sl);
      AddDatarow(['Name',Devices[j].Name],sl);
      AddDatarow(['Manufacturer',Devices[j].Manufacturer],sl);
      AddDatarow(['Location',Devices[j].Location],sl);
      AddDatarow(['HardwareID',Devices[j].HardwareID],sl);
      AddDatarow(['Last Init',DateTimeToStrDef(Devices[j].LastArrivalDate)],sl);
      CloseRecordset(sl);
    end;
    if c>0 then
      CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure USB_XML_Report;
var
  i,j: Integer;
  s: string;
begin
  with sl, USB do begin

    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('USB',sl);
    for i:=0 to USBNodeCount-1 do
      if USBNodes[i].USBDevice.ConnectionStatus=1 then
        if not(USBNodes[i].USBClass in [usbHostController,usbHub,usbExternalHub]) then begin
          OpenSection(USBNodes[i].USBDevice.Product,sl);
            OpenRecordset('General',['Property','Value','Units'],sl);
              AddDatarow(['Port',USBNodes[i].USBDevice.Port,''],sl);
              AddDatarow(['Address',USBNodes[i].USBDevice.DeviceAddress,''],sl);
              if USBNodes[i].USBDevice.USBClassname<>'' then
                s:=USBNodes[i].USBDevice.USBClassname
              else
                s:=ClassNames[Integer(USBNodes[i].USBClass)];
              AddDatarow(['Manufacturer',USBNodes[i].USBDevice.Manufacturer,''],sl);
              AddDatarow(['Class',s,''],sl);
              AddDatarow(['Serial Number',USBNodes[i].USBDevice.Serial,''],sl);
              AddDatarow(['Version',Format('%d.%d',[USBNodes[i].USBDevice.MajorVersion,USBNodes[i].USBDevice.MinorVersion]),''],sl);
              AddDatarow(['Power Consumption',USBNodes[i].USBDevice.MaxPower,'mA'],sl);
            CloseRecordset(sl);

            if Length(USBNodes[i].USBDevice.Registry)>0 then begin
              OpenRecordset('Featured Devices',['Property','Value'],sl);
              for j:=0 to High(USBNodes[i].USBDevice.Registry) do begin
                AddHLDatarow(['Device',USBNodes[i].USBDevice.Registry[j].DeviceClass],sl);
                AddDatarow(['Name',USBNodes[i].USBDevice.Registry[j].Name],sl);
                if USBNodes[i].USBDevice.Registry[j].Drive<>'' then
                  AddDatarow(['Drive',USBNodes[i].USBDevice.Registry[j].Drive+':'],sl);
                AddDatarow(['Last Init',DateTimeToStrDef(USBNodes[i].USBDevice.Registry[j].Timestamp)],sl);
              end;
              CloseRecordset(sl);
            end;

          CloseSection(sl);
        end;

    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure USBHistory_XML_Report(USBHistory: TMiTeC_USBHistory; Standalone: Boolean; sl: TStrings);
var
  i: Integer;
begin
  with sl, USBHistory do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('USB History',sl);
    if RecordCount>0 then begin
      OpenRecordset('',['Device','SerialNumber','Last Seen','Class'],sl);
      for i:=0 to RecordCount-1 do
        AddDatarow([Records[i].Name,Records[i].SerialNumber,DateTimeToStrDef(Records[i].Timestamp),Records[i].DeviceClass],sl);
      CloseRecordset(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Network_XML_Report;
var
  i: Integer;
begin
  with sl, Network do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

      OpenCategory('Network',sl);
        OpenSection('Adapters',sl);
        for i:=0 to TCPIP.AdapterCount-1 do begin
          OpenRecordset(TCPIP.Adapter[i].Name,['Property','Value','Units'],sl);
            AddDatarow(['MAC Address',TCPIP.Adapter[i].Address,''],sl);
            AddDatarow(['IP Address',TCPIP.Adapter[i].IPAddress.CommaText,''],sl);
            try AddDatarow(['Type',AdapterTypes[TCPIP.Adapter[i].Typ],''],sl); except end;
            AddDatarow(['Max Speed',TCPIP.Adapter[i].MaxSpeed,'Kbps'],sl);
            AddDatarow(['MTU Size',TCPIP.Adapter[i].MTU,'B'],sl);
          CloseRecordset(sl);
        end;
      CloseSection(sl);
      OpenSection('Protocols',sl);
        StringsToRecordset(Protocols,'','Protocol',sl);
      CloseSection(sl);
      OpenSection('Services',sl);
        StringsToRecordset(Services,'','Service',sl);
      CloseSection(sl);
      OpenSection('Clients',sl);
        StringsToRecordset(Clients,'','Client',sl);
      CloseSection(sl);

    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure NetCreds_XML_Report(NetCreds: TMiTeC_NetCreds; Standalone: Boolean; sl: TStrings);
var
  i: Integer;
  s: string;
begin
  with sl, NetCreds do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('NetworkCredentials',sl);
    for i:=0 to RecordCount-1 do begin
      case Records[i].Typ of
        1: s:='Generic';
        2: s:='Domain Password';
        3: s:='Domain Certificate';
        4: s:='Domain Visible Password';
        5: s:='Generic Certificate';
        6: s:='Domain Extended';
        7: s:='Maximum';
        1007: s:='Maximum Extended';
        else s:=IntToStr(Records[i].Typ);
      end;
      OpenSection(s,sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Timestamp',DatetimetoStr(Records[i].Timestamp)],sl);
          AddDatarow(['Target',Records[i].Target],sl);
          AddDatarow(['Username',Records[i].UserName],sl);
          AddDatarow(['Password',Records[i].Password],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Media_XML_Report;
begin
  with sl, Media do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
      OpenCategory('Media',sl);
        OpenSection('Devices',sl);
          StringsToRecordset(Devices,'','Device',sl);
        CloseSection(sl);
        if WaveIn.Count+WaveOut.Count>0 then begin
          OpenSection('WAVE',sl);
          if WAVEIn.Count>0 then
            StringsToRecordset(WaveIn,'','WaveIn',sl);
          if WAVEOut.Count>0 then
            StringsToRecordset(WaveOut,'','WaveOut',sl);
          CloseSection(sl);
        end;
        if MIDIIn.Count+MIDIOut.Count>0 then begin
          OpenSection('MIDI',sl);
            if MIDIIn.Count>0 then
              StringsToRecordset(MIDIIn,'','MIDIIn',sl);
            if MIDIOut.Count>0 then
              StringsToRecordset(MIDIOut,'','MIDIOut',sl);
          CloseSection(sl);
        end;
        if AUX.Count>0 then begin
          OpenSection('AUX',sl);
            StringsToRecordset(AUX,'','AUX',sl);
          CloseSection(sl);
        end;
        if Mixer.Count>0 then begin
          OpenSection('Mixer',sl);
            StringsToRecordset(Mixer,'','Mixer',sl);
          CloseSection(sl);
        end;
      CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Printers_XML_Report;
var
  i: integer;
begin
  with sl, Printers do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Printers',sl);
    for i:=0 to PrinterCount-1 do begin
      OpenSection(Printers.PrinterName[i],sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Share',Share[i]],sl);
          AddDatarow(['Port',Port[i]],sl);
          AddDatarow(['Driver',Driver[i]],sl);
          AddDatarow(['Comment',Comment[i]],sl);
          AddDatarow(['Location',Location[i]],sl);
          AddDatarow(['DriverPath',DriverPath[i]],sl);
          AddDatarow(['DriverVersion',DriverVersion[i]],sl);
          AddDatarow(['Monitor',Monitor[i]],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Engines_XML_Report;
begin
  with sl, Engines do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);
    OpenCategory('Engines',sl);
    OpenSection('',sl);
    OpenRecordset('',['Name','Version'],sl);
    if ODBC<>'' then
      AddDatarow(['ODBC',ODBC],sl);
    if BDE<>'' then
      AddDatarow(['BDE',BDE],sl);
    if DAO<>'' then
      AddDatarow(['DAO',DAO],sl);
    if ADO<>'' then
      AddDatarow(['ADO',ADO],sl);
    if OpenGL<>'' then
      AddDatarow(['OpenGL',OpenGL],sl);
    if ASPI32.ASPI<>'' then
      AddDatarow(['Adaptec ASPI',ASPI32.ASPI],sl);
    if IE<>'' then
      AddDatarow(['Microsoft Internet Explorer',IE],sl);
    if NET<>'' then
      AddDatarow(['Microsoft .NET Framework',NET],sl);
    if MSI<>'' then
      AddDatarow(['Microsoft Windows Installer',MSI],sl);
    if QT<>'' then
      AddDatarow(['Apple QuickTime',QT],sl);
    if DirectX.Version<>'' then
      AddDatarow(['Microsoft.DirectX',DirectX.Version],sl);
    CloseRecordset(sl);
    CloseSection(sl);
    CloseCategory(sl);
    if Standalone then ReportFooter(sl);
  end;
end;

procedure ProcessList_XML_Report;
var
  i: Integer;
begin
  with sl, ProcessList do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Process List',sl);
    for i:=0 to ProcessCount-1 do begin
      OpenSection(Format('%s',[Processes[i].Name]),sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['PID',Processes[i].PID],sl);
          AddDatarow(['Parent PID',Processes[i].ParentPID],sl);
          AddDatarow(['Image Name',Processes[i].ImageName],sl);
          AddDatarow(['Priority',Processes[i].Priority],sl);
          AddDatarow(['Thread Count',Processes[i].ThreadCount],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if ServiceCount>0 then begin
      OpenCategory('Service List',sl);
      for i:=0 to ServiceCount-1 do begin
        OpenSection(Format('%s',[Services[i].DisplayName]),sl);
          OpenRecordset('',['Property','Value'],sl);
            AddDatarow(['Image Name',Services[i].ImageName],sl);
            AddDatarow(['Type',cSvcType[Services[i].Typ]],sl);
            AddDatarow(['Status',cSvcStatus[Services[i].Status]],sl);
            AddDatarow(['Startup',cSvcStartup[Services[i].StartUp]],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
      CloseCategory(sl);
    end;

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Software_XML_Report;
var
  i: integer;
begin
  with sl, Software do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Software',sl);
    for i:=0 to Software.Count-1 do begin
      OpenSection(InstallEntry[i].Name,sl);
        OpenRecordset('',['Property','Value'],sl);
          AddDatarow(['Version',InstallEntry[i].Version],sl);
          AddDatarow(['Company',InstallEntry[i].Company],sl);
          AddDatarow(['Source',InstallEntry[i].InstallSource],sl);
          AddDatarow(['Uninstall',InstallEntry[i].Uninstall],sl);
          AddDatarow(['Date',InstallEntry[i].InstallDate],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Startup_XML_Report;
var
  i: integer;
begin
  with sl, Startup do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Startup',sl);
    OpenSection('',sl);
    OpenRecordset('',['Name','Location','Command Line'],sl);

    for i:=0 to Count-1 do
      with Records[i] do
        AddDatarow([Name,Path,CmdLine],sl);

    CloseRecordset(sl);
    CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Security_XML_Report;
var
  i: integer;
begin
  with sl, SC do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Security',sl);

    OpenSection('Antivirus',sl);
    OpenRecordset('',['Product','EXE'],sl);

    for i:=0 to Antivirus.Count-1 do
      AddDatarow([ListName(Antivirus,i),ListValueFromIndex(Antivirus,i)],sl);

    CloseRecordset(sl);
    CloseSection(sl);

    OpenSection('AntiSpyware',sl);
    OpenRecordset('',['Product','EXE'],sl);

    for i:=0 to Antispyware.Count-1 do
      AddDatarow([ListName(Antispyware,i),ListValueFromIndex(Antispyware,i)],sl);

    CloseRecordset(sl);
    CloseSection(sl);

    OpenSection('Firewall',sl);
    OpenRecordset('',['Product','EXE'],sl);

    for i:=0 to Firewall.Count-1 do
      AddDatarow([ListName(Firewall,i),ListValueFromIndex(Firewall,i)],sl);

    CloseRecordset(sl);
    CloseSection(sl);
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure Disk_XML_Report;
var
  i: Integer;
begin
  with sl, Disk do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Disk',sl);
    for i:=1 to Length(Disk.AvailableDisks) do begin
      Disk.Drive:=Copy(Disk.AvailableDisks,i,1)+':';
      OpenSection(Format('Drive %s',[Disk.Drive]),sl);
        OpenRecordset('General',['Property','Value','Units'],sl);
          AddDatarow(['Type',Disk.GetMediaTypeStr(Disk.MediaType),''],sl);
          AddDatarow(['File system',Disk.FileSystem,''],sl);
          AddDatarow(['UNC',Disk.UNCPath,''],sl);
          AddDatarow(['Volume label',Disk.VolumeLabel,''],sl);
          AddDatarow(['Serial number',Disk.SerialNumber,''],sl);
          AddDatarow(['Capacity',Disk.Capacity,'B'],sl);
          AddDatarow(['Free Space',Disk.FreeSpace,'B'],sl);
          AddDatarow(['Total clusters',Disk.TotalClusters,''],sl);
          AddDatarow(['Sectors per cluster',Disk.SectorsPerCluster,''],sl);
          AddDatarow(['Bytes per sector',Disk.BytesPerSector,''],sl);
        CloseRecordset(sl);

        OpenRecordset('Flags',['Property','Value'],sl);
          AddDatarow(['CaseIsPreserved',integer(fsCaseIsPreserved in FileFlags)],sl);
          AddDatarow(['CaseSensitive',Integer(fsCaseSensitive in FileFlags)],sl);
          AddDatarow(['UnicodeStoredOnDisk',Integer(fsUnicodeStoredOnDisk in FileFlags)],sl);
          AddDatarow(['PersistentAcls',Integer(fsPersistentAcls in FileFlags)],sl);
          AddDatarow(['FileCompression',Integer(fsFileCompression in FileFlags)],sl);
          AddDatarow(['VolumeIsCompressed',Integer(fsVolumeIsCompressed in FileFlags)],sl);
          AddDatarow(['LongFileNames',Integer(fsLongFileNames in FileFlags)],sl);
          AddDatarow(['EncryptedFileSystemSupport',Integer(fsEncryptedFileSystemSupport in FileFlags)],sl);
          AddDatarow(['ObjectIDsSupport',integer(fsObjectIDsSupport in FileFlags)],sl);
          AddDatarow(['ReparsePointsSupport',integer(fsReparsePointsSupport in FileFlags)],sl);
          AddDatarow(['SparseFilesSupport',Integer(fsSparseFilesSupport in FileFlags)],sl);
          AddDatarow(['DiskQuotasSupport',Integer(fsDiskQuotasSupport in FileFlags)],sl);
        CloseRecordset(sl);
      CloseSection(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure EventLog_XML_Report;
var
  i,j: Integer;
  rh: Boolean;
begin
  with sl, EL do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Event Log',sl);

    for j:=0 to ContainerCount-1 do begin
      SourceName:=Containers[j].Name;
      if LiveData then
        RefreshData
      else begin
        rh:=True;
        LoadFromStorage(StorageFileName,rh,StreamCodeProc);
      end;

        OpenSection(SourceName,sl);
        OpenRecordset('',['Event','Timestamp','Source','Category','Event ID','User','Machine','Description'],sl);
        for i:=0 to RecordCount-1 do
          AddDatarow([EventTypes[Records[i].EventType],
                      DateTimeToStrDef(Records[i].DateTime),
                      Records[i].Source,
                      Records[i].Category,
                      Records[i].EventID,
                      Records[i].Username,
                      Records[i].Computer,
                      Records[i].Description],sl);
          CloseRecordset(sl);
        CloseSection(sl);
    end;

    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure ActiveDirectory_XML_Report;
var
  i: Integer;
begin
  with sl, AD do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Active Directory',sl);

    OpenSection('AD Database',sl);
      OpenRecordset('',['Name'],sl);
      AddDatarow([GetName(Domain)],sl);
      CloseRecordset(sl);
    CloseSection(sl);  

      OpenSection('Users',sl);
        OpenRecordset('',['Full name','Email','Last Logon','Logon Count','Created'],sl);
        for i:=0 to UserCount-1 do
          AddDatarow([GetName(Users[i]),
                      GetProp(Users[i],'mail').Value,
                      DateTimeToStrDef(UTCToDatetime(GetProp(Users[i],'lastLogon').Value)),
                      GetProp(Users[i],'logonCount').Value,
                      DateTimeToStrDef(GetProp(Users[i],'whenCreated').Value)
                      ],sl);
          CloseRecordset(sl);
      CloseSection(sl);

      OpenSection('Groups',sl);
        OpenRecordset('',['Name','Description','Created'],sl);
        for i:=0 to GroupCount-1 do
          AddDatarow([GetName(Groups[i]),
                      GetProp(Groups[i],'Description').Value,
                      DateTimeToStrDef(GetProp(Groups[i],'whenCreated').Value)
                      ],sl);
          CloseRecordset(sl);
      CloseSection(sl);

      OpenSection('Computers',sl);
        OpenRecordset('',['Name','OS','Service Pack','Version','Last Logon','Logon Count','Created'],sl);
        for i:=0 to ComputerCount-1 do
          AddDatarow([GetName(Computers[i]),
                      GetProp(Computers[i],'OperatingSystem').Value,
                      GetProp(Computers[i],'operatingSystemServicePack').Value,
                      GetProp(Computers[i],'operatingSystemVersion').Value,
                      DateTimeToStrDef(UTCToDatetime(GetProp(Computers[i],'lastLogon').Value)),
                      GetProp(Computers[i],'logonCount').Value,
                      DateTimeToStrDef(GetProp(Computers[i],'whenCreated').Value)
                      ],sl);
          CloseRecordset(sl);

    CloseSection(sl);

    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure WIFI_XML_Report;
var
  i: Integer;
begin
  with sl, WIFI do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Wi-Fi Available Networks',sl);
    if NetworkCount>0 then begin
      for i:=0 to NetworkCount-1 do begin
        OpenSection(Networks[i].SSID,sl);
          OpenRecordset('',['Property','Value','Units'],sl);
            AddDatarow(['SSID',Networks[i].SSID,''],sl);
            AddDatarow(['Profile',Networks[i].Profile,''],sl);
            AddDatarow(['SignalQuality',Networks[i].SignalQuality,'%'],sl);
            AddDatarow(['Authentication',AuthToStr(Networks[i].AuthAlgorithm),''],sl);
            AddDatarow(['Security',CipherToStr(Networks[i].CipherAlgorithm),''],sl);
            AddDatarow(['PHY Type',PHYToStr(Networks[i].PHYType),''],sl);
            AddDatarow(['BSS Type',BSSToStr(Networks[i].BSSType),''],sl);
            AddDatarow(['MAC Address',Networks[i].MACAddress,''],sl);
            AddDatarow(['RSSI',Networks[i].RSSI,'dBm'],sl);
            AddDatarow(['MaxSpeed',Networks[i].MaxSpeed,'Mbps'],sl);
            AddDatarow(['ChannelFreq',Networks[i].ChannelFreq/1000000,'GHz'],sl);
            AddDatarow(['Connected',BooleanEn[Networks[i].Connected],''],sl);
          CloseRecordset(sl);
        CloseSection(sl);
      end;
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure WLANC_XML_Report(WLANC: TMiTeC_WLANC; Standalone: Boolean; sl: TStrings);
var
  i: Integer;
begin
  with sl, WLANC do begin
    if Standalone then ReportHeader(sl,StorageInfo.Machine,StorageInfo.LoggedUser);

    OpenCategory('Wi-Fi Known Networks',sl);
    if RecordCount>0 then begin
      OpenRecordset('',['SSID','Key','Authentication','Encryption','Connection'],sl);
      for i:=0 to RecordCount-1 do
        AddDatarow([Records[i].SSID,Records[i].Key,Records[i].Encryption,Records[i].Connection],sl);
      CloseRecordset(sl);
    end;
    CloseCategory(sl);

    if Standalone then ReportFooter(sl);
  end;
end;

procedure SystemInfo_XML_Report;
begin
  with AComponent do begin
  ReportHeader(sl,CPU.StorageInfo.Machine,CPU.StorageInfo.LoggedUser);
  if Assigned(Machine) then
    Machine_XML_Report(Machine,False,sl);
  if Assigned(OS) then
    OperatingSystem_XML_Report(OS,False,sl);
  if Assigned(CPU) then
    CPU_XML_Report(CPU,False,sl);
  if Assigned(Memory) then
    Memory_XML_Report(Memory,False,sl);
  if Assigned(Display) then
    Display_XML_Report(Display,False,sl);
  if Assigned(Monitor) then
    Monitor_XML_Report(Monitor,False,sl);
  if Assigned(APM) then
    APM_XML_Report(APM,False,sl);
  if Assigned(Media) then
    Media_XML_Report(Media,False,sl);
  if Assigned(Network) then
    Network_XML_Report(Network,False,sl);
  if Assigned(WIFI) then
    WIFI_XML_Report(WIFI,False,sl);
  if Assigned(ActiveDirectory) then
    ActiveDirectory_XML_Report(ActiveDirectory,False,sl);
  if Assigned(Devices) then
    Devices_XML_Report(Devices,False,sl);
  if Assigned(Storage) then
    Storage_XML_Report(Storage,False,sl);
  if Assigned(Disk) then
    Disk_XML_Report(Disk,False,sl);
  if Assigned(USB) then
    USB_XML_Report(USB,False,sl);
  if Assigned(Bluetooth) then
    Bluetooth_XML_Report(Bluetooth,False,sl);
  if Assigned(Printers) then
    Printers_XML_Report(Printers,False,sl);
  if Assigned(Engines) then
    Engines_XML_Report(Engines,False,sl);
  if Assigned(ProcessList) then
    ProcessList_XML_Report(ProcessList,False,sl);
  if Assigned(Software) then
    Software_XML_Report(Software,False,sl);
  if Assigned(Startup) then
    Startup_XML_Report(Startup,False,sl);
  if Assigned(Security) then
    Security_XML_Report(Security,False,sl);
  if Assigned(EventLog) then
    EventLog_XML_Report(EventLog,False,sl);
  if Assigned(ActiveDirectory) then
    ActiveDirectory_XML_Report(ActiveDirectory,False,sl);

  ReportFooter(sl);
  end;
end;

end.
