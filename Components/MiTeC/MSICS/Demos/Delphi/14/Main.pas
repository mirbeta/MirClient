unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus,  MSI_Common, MSI_Defs;

type
  Twnd_cputest_Main = class(TForm)
    Box: TMemo;
    Panel2: TPanel;
    Button1: TButton;
    Panel3: TPanel;
    Button2: TButton;
    cbxSMBIOS: TCheckBox;
    cbxWMI: TCheckBox;
    cbxSave: TCheckBox;
    cbxXML: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    procedure DetectCPU;
  end;

var
  wnd_cputest_Main: Twnd_cputest_Main;

implementation

uses Registry, MSI_CPU, MSI_SMBIOS, MiTeC_WbemScripting_TLB, MiTeC_WMI, MSI_XML_Reports,
  MiTeC_Routines, MiTeC_Windows;

{$R *.DFM}

function RoundFrequency(const Frequency: Integer): Integer;
const
  NF: array [0..9] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100, 133);
var
  Freq, RF: Integer;
  i: Byte;
  Hi, Lo: Byte;
begin
  RF:=0;
  Freq:=Frequency mod 100;
  for i:=0 to High(NF) do begin
    if Freq<NF[i] then begin
      Hi:=i;
      Lo:=i-1;
      if (NF[Hi]-Freq)>(Freq-NF[Lo]) then
        RF:=NF[Lo]-Freq
      else
        RF:=NF[Hi]-Freq;
      Break;
    end;
  end;
  Result:=Frequency+RF;
end;


procedure Twnd_cputest_Main.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure Twnd_cputest_Main.DetectCPU;
const
  CPUIDCommands:  array[0..19] of DWORD = (CPUID_STD_MaximumLevel,
                                           CPUID_STD_Signature,
                                           CPUID_STD_CacheTlbs,
                                           CPUID_STD_SerialNumber,
                                           CPUID_STD_CacheParams,
                                           CPUID_STD_MonitorMWAIT,
                                           CPUID_STD_ThermalPower,
                                           CPUID_STD_DCA,
                                           CPUID_STD_ArcPerfMon,
                                           CPUID_STD_Topology,
                                           CPUID_STD_XSAVE,

                                           CPUID_EXT_MaximumLevel,
                                           CPUID_EXT_Signature,
                                           CPUID_EXT_MarketingName1,
                                           CPUID_EXT_MarketingName2,
                                           CPUID_EXT_MarketingName3,
                                           CPUID_EXT_Level1Cache,
                                           CPUID_EXT_Level2Cache,
                                           CPUID_EXT_PowerManagement,
                                           CPUID_EXT_AA64Information);
var
   i,j,m: Integer;
   s,f,fn: string;
   wmiServices: ISWbemServices;
   wmi: TInstances;
   CPUID: TCPUIDResult;
   CPU: TMiTeC_CPU;
   sl: TStringList;
   slpi: array of TSystemLogicalProcessorInformation;
   n: Cardinal;
   h: Boolean;
begin
  with Box, Lines do begin
    Add('[CPU Test]');
    Add(Format('Version = %s',[cVersion]));
    {$IFDEF WIN32}
    Add('Target platform: 32-bit');
    {$ENDIF}
    {$IFDEF WIN64}
    Add('Target platform: 64-bit');
    {$ENDIF}
    Add('OS: '+FormOSName);
    Add('Machine: '+MachineName);
    Add('');
    CPU:=TMiTeC_CPU.Create(Self);
    with CPU do
      try
        Refreshdata;
        Add('[General]');
        Add(Format('Number of Physical Processors = %d',[CPUPhysicalCount]));
        Add(Format('Number of Logical Processors = %d',[CPUCount]));
        Add(Format('Total Number of Cores = %d',[CoreCount]));
        Add(Format('Total Number of Threads = %d',[ThreadCount]));

        Add(Format('Number of Cores per Package = %d',[CorePerPackage]));
        Add(Format('Number of Threads per Core = %d',[LogicalPerCore]));
        Add(Format('Number of Threads per Package = %d',[LogicalPerPackage]));

        Add(Format('Max number of Threads per Core = %d',[MaxLogicalPerCore]));
        Add(Format('Max number of Cores per Package = %d',[MaxCorePerPackage]));
        Add(Format('Max number of Threads per Package = %d',[MaxLogicalPerPackage]));
        Add('');
        for i:=0 to CPUPhysicalCount-1 do begin
          CPUIndex:=i;
          Refreshdata;
          Add(Format('[Processor #%d]',[i+1]));
          case Architecture of
            PROCESSOR_ARCHITECTURE_AMD64: Add('Architecture = x64 (AMD or Intel)');
            PROCESSOR_ARCHITECTURE_IA32_ON_WIN64: Add('Architecture = WOW64');
            PROCESSOR_ARCHITECTURE_IA64: Add('Architecture = Intel Itanium Processor Family (IPF)');
            PROCESSOR_ARCHITECTURE_INTEL: Add('Architecture = x86');
            else Add(Format('Architecture = %d',[Architecture]));
          end;
          Add(Format('Type = %d',[SystemInfo.dwProcessorType]));
          Add(Format('Level = %d',[SystemInfo.wProcessorLevel]));
          Add(Format('Revision = %d (%4.4x)',[SystemInfo.wProcessorRevision,SystemInfo.wProcessorRevision]));
          Add(Format('APIC ID = %d',[APICID]));
          Add(Format('LogicalPerPackage = %d',[LogicalPerPackage]));
          Add(Format('CorePerPackage = %d',[CorePerPackage]));
          Add(Format('LogicalPerCore = %d',[LogicalPerCore]));
          Add(Format('Physical ID = %d',[PhysicalID]));
          Add(Format('Logical ID = %d',[LogicalID]));
          Add(Format('Vendor = %s',[cVendorNames[Vendor].Name]));
          Add(Format('CPUName = %s',[CPUName]));
          Add(Format('GenericName = %s',[GenericName]));
          Add(Format('MarketingName = %s',[MarketingName]));
          Add(Format('CodeName = %s',[CodeName]));
          Add(Format('Revision = %s',[Revision]));
          Add(Format('Technology = %s',[Technology]));
          Add(Format('Brand = %d',[Brand]));
          Add(Format('Type = %d',[CPUType]));
          Add(Format('FamilyEx = %d',[FamilyEx]));
          Add(Format('ModelEx = %d',[ModelEx]));
          Add(Format('SteppingEx = %d',[SteppingEx]));
          Add(Format('Frequency = %d MHz',[Frequency]));
          Add(Format('Serial Number = %s',[SerialNumber]));
          Add('');
          f:=GenericName;
          Add(Format('[CPUID #%d]',[i+1]));
          CPUID:=ExecuteCPUID(i,CPUID_STD_MaximumLevel);
          m:=CPUID.EAX;
          for j:=0 to m do begin
            CPUID:=ExecuteCPUID(-1,j);
            with CPUID do
              Add(Format('%8.8x = EAX:%8.8x  EBX:%8.8x  ECX:%8.8x  EDX:%8.8x',[CPUIDCommand,EAX,EBX,ECX,EDX]));
            if j=11 then begin
              CPUID:=ExecuteCPUID(-1,j,1,1);
              with CPUID do
                Add(Format('%8.8x = EAX:%8.8x  EBX:%8.8x  ECX:%8.8x  EDX:%8.8x',[CPUIDCommand,EAX,EBX,ECX,EDX]));
              CPUID:=ExecuteCPUID(-1,j,1,2);
              with CPUID do
                Add(Format('%8.8x = EAX:%8.8x  EBX:%8.8x  ECX:%8.8x  EDX:%8.8x',[CPUIDCommand,EAX,EBX,ECX,EDX]));
            end;  
          end;

          Add('');

          CPUID:=ExecuteCPUID(i,CPUID_EXT_MaximumLevel);
          m:=CPUID.EAX-CPUID_EXT_MaximumLevel;
          for j:=0 to m do begin
            CPUID:=ExecuteCPUID(i,j+CPUID_EXT_MaximumLevel);
            with CPUID do
              Add(Format('%8.8x = EAX:%8.8x  EBX:%8.8x  ECX:%8.8x  EDX:%8.8x',[CPUIDCommand,EAX,EBX,ECX,EDX]));
          end;

          Add('');
          Add(Format('[Cache #%d]',[i+1]));
          with Cache.Level1.Code do
            if Typ>0 then
              Add(Format('%d x %s',[SharedWays,Descriptor]));
          with Cache.Level1.Data do
            if Typ>0 then
              Add(Format('%d x %s',[SharedWays,Descriptor]));
          with Cache.Level1.Unified do
            if Typ>0 then
              Add(Format('%d x %s',[SharedWays,Descriptor]));
          with Cache.Level2 do begin
            if Typ>0 then
              Add(Format('%d x %s',[SharedWays,Descriptor]));
          end;
          with Cache.Level3 do begin
            if Typ>0 then
              Add(Format('%d x %s',[SharedWays,Descriptor]));
          end;
          with Cache.Trace do begin
            if Typ>0 then
              Add(Descriptor);
          end;

          Add('');
          s:='';
          if Vendor=cvIntel then begin
            with Cache.Level1.Code do
              for j:=1 to 16 do
                s:=s+Format('%2.2x ',[Descriptors[j]]);
            Add(Format('Level1 Descriptors = %s',[s]));
          end;
          s:='';
          with Cache.Level2 do begin
            if Vendor=cvIntel then begin
              for j:=1 to 16 do
                s:=s+Format('%2.2x ',[Descriptors[j]]);
              Add(Format('Level2 Descriptors = %s',[s]));
            end;
          end;
          s:='';
          with Cache.Level3 do begin
            if Vendor=cvIntel then begin
              for j:=1 to 16 do
                s:=s+Format('%2.2x ',[Descriptors[j]]);
              Add(Format('Level3 Descriptors = %s',[s]));
            end;
          end;
          s:='';
          with Cache.Trace do begin
            if Vendor=cvIntel then begin
              for j:=1 to 16 do
                s:=s+Format('%2.2x ',[Descriptors[j]]);
              Add(Format('Trace Descriptors = %s',[s]));
            end;
          end;
          Add('');
          Add(Format('[Standard Features-1 #%d]',[i+1]));
          for j:=0 to Features.Standard1.Count-1 do
            with Features.Standard1.Features[j] do
              Add(Format('%s (%s) = %d',[Mnemonic,Name,Integer(Available)]));
          Add('');
          Add(Format('[Standard Features-2 #%d]',[i+1]));
          for j:=0 to Features.Standard2.Count-1 do
            with Features.Standard2.Features[j] do
              Add(Format('%s (%s) = %d',[Mnemonic,Name,Integer(Available)]));
          Add('');
          Add(Format('[Extended Features-1 #%d]',[i+1]));
          for j:=0 to Features.Extended1.Count-1 do
            with Features.Extended1.Features[j] do
              Add(Format('%s (%s) = %d',[Mnemonic,Name,Integer(Available)]));
          Add('');
          Add(Format('[Extended Features-2 #%d]',[i+1]));
          for j:=0 to Features.Extended2.Count-1 do
            with Features.Extended2.Features[j] do
              Add(Format('%s (%s) = %d',[Mnemonic,Name,Integer(Available)]));
          Add('');
          Add(Format('[Power Management Features #%d]',[i+1]));
          for j:=0 to Features.PowerManagement.Count-1 do
            with Features.PowerManagement.Features[j] do
              Add(Format('%s (%s) = %d',[Mnemonic,Name,Integer(Available)]));
          Add('');
        end;

        Add('[Logical Processor Information]');
        n:=0;
        SetLength(slpi,1);
        if not GetLogicalProcessorInformation(@slpi[0],n) then begin
          if GetLastError=ERROR_INSUFFICIENT_BUFFER then begin
            SetLength(slpi,n div SizeOf(TSystemLogicalProcessorInformation)+1);
            if GetLogicalProcessorInformation(@slpi[0],n) then begin
              for i:=0 to High(slpi)-1 do
                case slpi[i].Relationship of
                  RelationProcessorCore: Add(Format('[%d] RelationProcessorCore: %d bits set',[i,CountSetBits(slpi[i].ProcessorMask)]));
                  RelationProcessorPackage: Add(Format('[%d] RelationProcessorPackage',[i]));
                  RelationNumaNode: Add(Format('[%d] RelationNumaNode',[i]));
                  RelationCache: with slpi[i].Cache do Add(Format('[%d] RelationCache: Type=%d, Level=%d, Associativity=%d, LineSize=%d, Size=%d',[i,Integer(_Type),Level,Associativity,LineSize,Size]));
                  RelationGroup: Add(Format('[%d] RelationGroup',[i]));
                  else Add(Format('[%d] (out of range): %d',[i,Integer(slpi[i].Relationship)]));
                end;
            end else
              Add('Not available');
          end else
            Add('Not available');
        end else
          Add('Not available');

        fn:=f+'.sis';
        h:=True;
        if cbxSave.Checked then
          SaveToStorage(ExtractFilePath(Application.EXEName)+fn,h);

        fn:=f+'.xml';
        if cbxXML.Checked then begin
          sl:=TStringList.Create;
          try
            CPU_XML_Report(CPU,True,sl);
            sl.SaveToFile(ExtractFilePath(Application.EXEName)+fn);
            SaveXSLTemplate(ExtractFilePath(Application.EXEName)+XSLName);
          finally
            sl.Free;
          end;
        end;
        fn:=f+'.sis';

      finally
        Free;
      end;

      if cbxSMBIOS.Checked then begin
        Add('');
        Add('[SMBIOS]');
        with TMiTeC_SMBIOS.Create(Self) do
          try
            Refreshdata;
            for i:=0 to ProcessorCount-1 do begin
              Add(Format('Vendor[%d] = %s',[i,Processor[i].Manufacturer]));
              Add(Format('NameString[%d] = %s',[i,Processor[i].Version]));
              Add(Format('Socket[%d] = %s',[i,Processor[i].Socket+'('+Upgrades[Processor[i].Upgrade]+')']));
              Add(Format('Voltage[%d] = %1.1f V',[i,Processor[i].Voltage]));
              Add(Format('Frequency[%d] = %d MHz',[i,Processor[i].Frequency]));
              Add(Format('ExternalClock[%d] = %d MHz',[i,Processor[i].ExternalClock]));
            end;
            for i:=0 to CacheCount-1 do
              Add(Format('%s = %d of %d KB - %d ns (%s,%s,%s)',[Cache[i].Designation,
                                                                     Cache[i].InstalledSize,
                                                                     Cache[i].MaxSize,
                                                                     Cache[i].Speed,
                                                                     CacheTypes[Cache[i].Typ],
                                                                     CacheAssociativities[Cache[i].Associativity],
                                                                     SRAMTypes[Cache[i].SRAMType]]));


          h:=True;
          if cbxSave.Checked then
            SaveToStorage(ExtractFilePath(Application.EXEName)+fn,h);
        finally
          Free;
        end;
      end;

      if cbxWMI.Checked then begin
        Add('');
        Add('[WMI]');
        try
          if not WMIConnect('','','',Rootnamespace,wmiServices) then
            Exit;
          WMICommand(wmiServices,'Win32_Processor',wmi);

          for i:=0 to High(wmi) do
            for j:=0 to High(wmi[i]) do
              Add(Format('%s[%d] = %s',[wmi[i][j].Name,i,wmi[i][j].Value]));

          if cbxSave.Checked then
            WmiSaveToStorage('Win32_Processor',ExtractFilePath(Application.EXEName)+fn,wmi);
        finally
          WMIDisconnect(wmiServices);
          Finalize(wmi);
        end;
      end;

      Add('');

      Add('Test passed.');
      fn:=Format('%s.txt',[f]);

    try
      SaveToFile(ExtractFilePath(Application.EXEName)+fn);
    except
    end;
  end;
  Box.SelStart:=0;
  Box.SelLength:=0;
end;


procedure Twnd_cputest_Main.Button2Click(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  Box.Lines.Clear;
  DetectCPU;
  Screen.Cursor:=crDefault;
end;


end.
