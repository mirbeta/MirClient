uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_SIF,
     MiTeC_Routines,
     MiTeC_AccountsNT,
     MiTeC_Datetime,
     MiTeC_StrUtils,
     MiTeC_WinIOCTL,
     MiTeC_USB,
     MSI_Defs,
     MSI_Storage,
     MSI_XML_Reports,
     MSI_Common,
     MSI_SystemInfo,
     MSI_CPU,
     MSI_Devices,
     MSI_Network,
     MSI_Processes,
     MSI_Startup,
     Codecs;

{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP or IMAGE_FILE_NET_RUN_FROM_SWAP}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE}
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

var
  hConsole: THandle;
  ScreenBuffer: TConsoleScreenBufferInfo;
  SavePath: string;

const
  FOREGROUND_SILVER = FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_GREEN;
  FOREGROUND_WHITE = FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  FOREGROUND_YELLOW = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  FOREGROUND_OLIVE = FOREGROUND_RED or FOREGROUND_GREEN;
  FOREGROUND_CYAN = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  FOREGROUND_TEAL = FOREGROUND_BLUE or FOREGROUND_GREEN;

function Switch(Aparam: string): Boolean;
begin
  Result:=FindCmdLineSwitch(AParam,['\','/','-'],True);
end;

procedure WriteLine(AText1: string; AAttr1: Cardinal; AText2: string; AAttr2: Cardinal);
begin
  SetConsoleTextAttribute(hConsole,AAttr1);
  Write(AText1);
  SetConsoleTextAttribute(hConsole,AAttr2);
  WriteLn(AText2);
  SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
end;

procedure Separator;
begin
  SetConsoleTextAttribute(hConsole, FOREGROUND_INTENSITY);
  Writeln(stringofchar('-',79));
  SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
end;

procedure LoadFromSIF(AComponent: TMiTeC_Component);
var
  sir: TStorageInfoRecord;
  rh: boolean;
begin
  rh:=True;
  AComponent.LoadStorageInfo(AComponent.StorageFileName,@sir);
  case sir.SIFFormat of
    0: AComponent.LoadFromStorage(AComponent.StorageFileName,rh);
    1: AComponent.LoadFromStorage(AComponent.StorageFileName,rh,DecompressStream);
  end;
end;

procedure SystemOverview;
var
  i,j,k,c: integer;
  s,d,ad: string;
  b: Boolean;
begin
  with TMiTeC_SystemInfo.Create(nil) do begin
    try
      try
        SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
        Write('SYSTEM OVERVIEW...');
        RefreshData([soMachine,soCPU,soOS,soDisplay,soMonitor,soDisk,soMemory,soUSB,soNetwork,soPrinters,soDevices,soStorage,soMedia]);
        Write('OK');
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;
        WriteLine('Machine: ',FOREGROUND_SILVER,Machine.MachineName,FOREGROUND_YELLOW);
        WriteLine('System : ',FOREGROUND_SILVER,Machine.Computer,FOREGROUND_OLIVE);
        WriteLine('Model  : ',FOREGROUND_SILVER,Trim(Format('%s %s',[Trim(Machine.SMBIOS.SystemManufacturer),
                                                                   Trim(Machine.SMBIOS.SystemModel)])),FOREGROUND_OLIVE);
        WriteLine('UpTime : ',FOREGROUND_SILVER,formatseconds(Machine.SystemUpTime),FOREGROUND_OLIVE);

        Separator;

        if Trim(CPU.CPUName)='' then
          s:=Trim(CPU.MarketingName)
        else
          s:=Format('%s %s',[cVendorNames[CPU.Vendor].Prefix,CPU.CPUName]);
        WriteLine('CPU  : ',FOREGROUND_SILVER,Format('%d x %s - %d MHz',[CPU.CPUPhysicalCount,s,CPU.Frequency]),FOREGROUND_YELLOW);
        with CPU.Cache do
          WriteLine('Cache: ',FOREGROUND_SILVER,Format('%d KB L1 + %d KB L2 + %d KB L3',[Level1.Code.Size*Level1.Code.SharedWays+Level1.Data.Size*Level1.Data.SharedWays,Level2.Size*Level2.SharedWays,Level3.Size*Level3.SharedWays]),FOREGROUND_OLIVE);
        WriteLine('Number of cores: ',FOREGROUND_SILVER,Format('%d',[CPU.CorePerPackage*CPU.CPUPhysicalCount]),FOREGROUND_OLIVE);
        WriteLine('Number of threads: ',FOREGROUND_SILVER,Format('%d',[CPU.LogicalPerPackage*CPU.CPUPhysicalCount]),FOREGROUND_OLIVE);

        Separator;

        WriteLine('Mainboard: ',FOREGROUND_SILVER,Trim(Format('%s %s',[Trim(Machine.SMBIOS.MainBoardManufacturer),
                                                                   Trim(Machine.SMBIOS.MainBoardModel)])),FOREGROUND_YELLOW);
        WriteLine('Memory   : ',FOREGROUND_SILVER,Format('%d MB',[Memory.PhysicalTotal shr 20]),FOREGROUND_OLIVE);

        Separator;

        WriteLine('Operating system: ',FOREGROUND_SILVER,Format('%s %s [%d.%d.%d]',[OS.OSName,
                                      OS.OSEdition,
                                      OS.MajorVersion,
                                      OS.MinorVersion,
                                      OS.BuildNumber]),FOREGROUND_YELLOW);
        WriteLine('Service pack    : ',FOREGROUND_SILVER,ServicePack,FOREGROUND_OLIVE);
        WriteLine('Logged user     : ',FOREGROUND_SILVER,Machine.User,FOREGROUND_OLIVE);

        Separator;

        c:=0;
        for i:=0 to Display.AdapterCount-1 do begin
          WriteLine('Display adapter: ',FOREGROUND_SILVER,Display.Adapter[i].Name,FOREGROUND_YELLOW);
          WriteLine('Memory         : ',FOREGROUND_SILVER,Format('%d MB',[Display.Adapter[i].Memory shr 20]),FOREGROUND_OLIVE);
          Inc(c);
        end;

        if c>0 then
          Separator;

        c:=0;
        for i:=0 to Monitor.Count-1 do begin
          WriteLine('Monitor: ',FOREGROUND_SILVER,Format('%s %s',[Monitor.Monitors[i].Manufacturer,Monitor.Monitors[i].Model]),FOREGROUND_YELLOW);
          WriteLine('Dim    : ',FOREGROUND_SILVER,Format('(%dx%d)cm',[Monitor.Monitors[i].EDID.Width,Monitor.Monitors[i].EDID.Height]),FOREGROUND_OLIVE);
          Inc(c);
        end;

        if c>0 then
          Separator;

        Disk.RefreshData;
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
            WriteLine(s,FOREGROUND_SILVER,Trim(Format('%s - %d MB (%s)',[Physical[i].Model,Physical[i].Size shr 20,GetStorageBusTypeStr(Physical[i].BusType)])),FOREGROUND_YELLOW);
            for j:=0 to LogicalCount-1 do
              with Logical[j] do
                if PhysicalIndex=i then begin
                   d:=d+Copy(Drive,1,1);
                   if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) then
                     WriteLine(Format(' %s: ',[Drive]),FOREGROUND_SILVER,Format('(%s %s - %d MB)',[
                                   GetPartitionType(Layout[0].Number,Layout[0].Typ),
                                   GetPartitionSystem(Layout[0].Typ),
                                   Layout[0].Length.QuadPart shr 20]),FOREGROUND_OLIVE)
                else
                  WriteLine(Format(' %s: ',[Drive]),FOREGROUND_SILVER,'',FOREGROUND_OLIVE);
                end;
          end;

        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if MediaType=dtRemote then begin
              if not b then begin
                WriteLine('Network drives',FOREGROUND_SILVER,'',FOREGROUND_YELLOW);
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);

              WriteLine(Format(' %s ',[Drive]),FOREGROUND_SILVER,Format('(%s)',[UNCPath]),FOREGROUND_OLIVE);
            end;
          end;
        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if (Pos(Copy(ad,i,1),d)=0) and (MediaType=dtRemovable) then begin
              if not b then begin
                WriteLine('Removable drives',FOREGROUND_SILVER,'',FOREGROUND_YELLOW);
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);
              if Disk.MediaPresent then
                WriteLine(Format(' %s ',[Drive]),FOREGROUND_SILVER,Format('(%s - %d MB)',[Disk.FileSystem,Disk.Capacity shr 20]),FOREGROUND_OLIVE)
              else
                WriteLine(Format(' %s ',[Drive]),FOREGROUND_SILVER,'',FOREGROUND_OLIVE)
            end;
          end;
        b:=False;
        with Disk do
          for i:=1 to Length(ad) do begin
            Drive:=Format('%s:\',[Copy(ad,i,1)]);
            if (Pos(Copy(ad,i,1),d)=0) then begin
              if not b then begin
                WriteLine('Other drives',FOREGROUND_SILVER,'',FOREGROUND_YELLOW);
                b:=True;
              end;
              d:=d+Copy(Drive,1,1);
              if Disk.MediaPresent then
                WriteLine(Format(' %s ',[Drive]),FOREGROUND_SILVER,Format('(%s - %d MB)',[Disk.FileSystem,Disk.Capacity shr 20]),FOREGROUND_OLIVE)
              else
                WriteLine(Format(' %s ',[Drive]),FOREGROUND_SILVER,'',FOREGROUND_OLIVE)
            end;
          end;

        Separator;

        c:=0;
        with USB do
          for i:=0 to USBNodeCount-1 do
            if USBNodes[i].USBDevice.ConnectionStatus=1 then
              if not(USBNodes[i].USBClass in [usbHostController,usbHub,usbExternalHub]) then begin
                WriteLine(Format('USB port %d  : ',[USBNodes[i].USBDevice.Port]),FOREGROUND_SILVER,Trim(USBNodes[i].USBDevice.Product),FOREGROUND_YELLOW);
                WriteLine('Class       : ',FOREGROUND_SILVER,ClassNames[integer(USBNodes[i].USBClass)],FOREGROUND_OLIVE);
                WriteLine('Manufacturer: ',FOREGROUND_SILVER,USBNodes[i].USBDevice.Manufacturer,FOREGROUND_OLIVE);
                WriteLine('Version     : ',FOREGROUND_SILVER,Format('%d.%d',[USBNodes[i].USBDevice.MajorVersion,USBNodes[i].USBDevice.MinorVersion]),FOREGROUND_OLIVE);
                Inc(c);
              end;

        if c>0 then
          Separator;

        c:=-1;
        k:=maxint;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Media') and
             (Devices.Devices[i].PCINumber>-1) then
            if Devices.Devices[i].FunctionNumber<k then begin
              k:=Devices.Devices[i].FunctionNumber;
              j:=Devices.Devices[i].DeviceNumber;
              c:=i;
            end;
        if c>-1 then begin
          WriteLine('Audio adapter: ',FOREGROUND_SILVER,Devices.Devices[c].Name,FOREGROUND_YELLOW);
          c:=1;
          for i:=0 to Devices.DeviceCount-1 do
            if SameText(Devices.Devices[i].ClassName,'Media') and
               (Devices.Devices[i].DeviceNumber=j) and
               (Devices.Devices[i].FunctionNumber>k) then
              WriteLine('Game adapter : ',FOREGROUND_SILVER,Devices.Devices[i].Name,FOREGROUND_OLIVE);
        end;

        if (c=-1) and (Media.WAVEOut.Count>0) then begin
          WriteLine('Audio adapter: ',FOREGROUND_SILVER,Media.WaveOut[0],FOREGROUND_YELLOW);
          c:=1;
        end;

        if c>0 then
          Separator;

        c:=0;
        with Network.TCPIP do
          for i:=0 to AdapterCount-1 do
            if (Adapter[i].Address<>'') then begin
              s:=Adapter[i].Name;
              k:=Pos(' - ',s);
              if k>0 then
                s:=Copy(s,1,k);
              WriteLine('Network adapter: ',FOREGROUND_SILVER,s,FOREGROUND_YELLOW);
              WriteLine('MAC address    : ',FOREGROUND_SILVER,Adapter[i].Address,FOREGROUND_OLIVE);
              WriteLine('IP address     : ',FOREGROUND_SILVER,Adapter[i].IPAddress.CommaText,FOREGROUND_OLIVE);
              Inc(c);
            end;

        if c>0 then
          Separator;

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Modem') then begin
            if Trim(Devices.Devices[i].FriendlyName)='' then
              s:=Devices.Devices[i].Description
            else
              s:=Devices.Devices[i].FriendlyName;
            WriteLine('Modem: ',FOREGROUND_SILVER,s,FOREGROUND_YELLOW);
            Inc(c);
          end;

        if c>0 then
          Separator;

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
        if SameText(Devices.Devices[i].ClassName,'Keyboard') then begin
          if Trim(Devices.Devices[i].FriendlyName)='' then
            s:=Devices.Devices[i].Description
          else
            s:=Devices.Devices[i].FriendlyName;
          WriteLine('Keyboard: ',FOREGROUND_SILVER,s,FOREGROUND_YELLOW);
          Inc(c);
          Break;
        end;

        if c>0 then
          Separator;

        c:=0;
        for i:=0 to Devices.DeviceCount-1 do
          if SameText(Devices.Devices[i].ClassName,'Mouse') then begin
            if Trim(Devices.Devices[i].FriendlyName)='' then
              s:=Devices.Devices[i].Description
            else
              s:=Devices.Devices[i].FriendlyName;
            WriteLine('Mouse: ',FOREGROUND_SILVER,s,FOREGROUND_YELLOW);
            Inc(c);
            Break;
          end;

        if c>0 then
          Separator;

        WriteLine('Default printer: ',FOREGROUND_SILVER,Printers.DefaultPrinter,FOREGROUND_YELLOW);
        for i:=0 to Printers.PrinterCount-1 do
          if not SameText(Printers.DefaultPrinter,Printers.PrinterName[i]) then
            WriteLine('Other printer  : ',FOREGROUND_SILVER,Printers.PrinterName[i],FOREGROUND_OLIVE);

        Separator;
      except
        on e: exception do begin
          SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
          writeln(e.message);
          SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure ListTCPIP;
var
  i: Integer;
begin
  try
    try
      SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
      Write('TCP/IP CONFIGURATION...');
      with TMiTeC_TCPIP.Create(nil) do begin
        RefreshData;
        Write('OK');
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;
        Writeline('Host name  : ',FOREGROUND_SILVER,Hostname,FOREGROUND_YELLOW);
        Writeline('Domain name: ',FOREGROUND_SILVER,Domainname,FOREGROUND_OLIVE);
        Writeline('Node type  : ',FOREGROUND_SILVER,NodeTypes[NodeType],FOREGROUND_OLIVE);
        Writeline('WINS proxy : ',FOREGROUND_SILVER,BoolToStr(Boolean(EnableProxy),True),FOREGROUND_OLIVE);
        Writeline('IP routing : ',FOREGROUND_SILVER,BoolToStr(Boolean(EnableRouting),True),FOREGROUND_OLIVE);
        Writeline('DNS enabled: ',FOREGROUND_SILVER,BoolToStr(Boolean(EnableDNS),True),FOREGROUND_OLIVE);
        Writeline('DHCP scope name   : ',FOREGROUND_SILVER,DHCPScopeName,FOREGROUND_OLIVE);
        Writeline('Primary DNS Suffix: ',FOREGROUND_SILVER,PrimaryDNSSuffix,FOREGROUND_OLIVE);
        Writeline('DNS Servers       : ',FOREGROUND_SILVER,StringReplace(DNSServers.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
        separator;
        for i:=0 to AdapterCount-1 do begin
          Writeline('Adapter    : ',FOREGROUND_SILVER,Adapter[i].Name,FOREGROUND_YELLOW);
          Writeline('MAC address: ',FOREGROUND_SILVER,Adapter[i].Address,FOREGROUND_OLIVE);
          Writeline('Type       : ',FOREGROUND_SILVER,AdapterTypes[Adapter[i].Typ],FOREGROUND_OLIVE);
          Writeline('DHCP       : ',FOREGROUND_SILVER,BoolToStr(Boolean(Adapter[i].EnableDHCP),True),FOREGROUND_OLIVE);
          Writeline('WINS       : ',FOREGROUND_SILVER,BoolToStr(Boolean(Adapter[i].HaveWINS),True),FOREGROUND_OLIVE);
          Writeline('IP address : ',FOREGROUND_SILVER,StringReplace(Adapter[i].IPAddress.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          Writeline('Subnet mask: ',FOREGROUND_SILVER,StringReplace(Adapter[i].IPAddressMask.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          Writeline('Gateway    : ',FOREGROUND_SILVER,StringReplace(Adapter[i].Gateway_IPAddress.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          Writeline('DHCP IP    : ',FOREGROUND_SILVER,StringReplace(Adapter[i].DHCP_IPAddress.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          Writeline('Primary WINS server  : ',FOREGROUND_SILVER,StringReplace(Adapter[i].PrimaryWINS_IPAddress.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          Writeline('Secondary WINS server: ',FOREGROUND_SILVER,StringReplace(Adapter[i].SecondaryWINS_IPAddress.CommaText,'"','',[rfReplaceAll,rfIgnoreCase]),FOREGROUND_OLIVE);
          separator;
        end;
      end;
    except
      on e: exception do begin
        SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
        writeln(e.message);
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
      end;
    end;
  finally
  end;
end;

procedure ListProc;
var
  i: Integer;
begin
  try
    try
      SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
      Write('PROCESSES...');
      with TMiTeC_ProcessList.Create(nil) do begin
        DetectionRange:=[dtProcesses];
        RefreshData;
        Write(Format('%d items',[ProcessCount]));
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;
        SetConsoleTextAttribute(hConsole, FOREGROUND_YELLOW);
        Writeln(Format('%-45s %-4s %-9s %9s %8s',['Image name','PID','CPU time','Mem usage','Priority']));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        Separator;
        for i:=0 to ProcessCount-1 do
          with Processes[i] do
            Writeln(Format('%-45.45s %4d %9s %8dK %8d',[Name,
                                                  PID,
                                                  FormatTicks(CPUTimes.UserTime.QuadPart+CPUTimes.KernelTime.QuadPart),
                                                  VMCounters.WorkingSetSize div 1024,
                                                  Priority]));
        Free;
      end;
    except
      on e: exception do begin
        SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
        writeln(e.message);
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
      end;
    end;
  finally
    Separator;
  end;
end;

procedure ListAutoruns;
var
  i: Integer;
begin
  try
    try
      SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
      Write('AUTORUNS...');
      with TMiTeC_Startup.Create(nil) do begin
        RefreshData;
        Write(Format('%d items',[Count]));
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;

        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        for i:=0 to Count-1 do
          with Records[i] do
            Writeln(CmdLine);
        Free;
      end;
    except
      on e: exception do begin
        SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
        writeln(e.message);
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
      end;
    end;
  finally
    Separator;
  end;
end;

procedure ListDevs;
var
  i: Integer;
  s: string;
begin
  try
    try
      SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
      Write('DEVICES...');
      with TMiTeC_Devices.Create(nil) do begin
        RefreshData;
        Write(Format('%d items',[DeviceCount]));
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;
        SetConsoleTextAttribute(hConsole, FOREGROUND_YELLOW);
        Writeln(Format('%-14.14s %-63.63s',['Class name','Device Name']));
        Separator;
        for i:=0 to DeviceCount-1 do
          with Devices[i] do begin
            if Trim(FriendlyName)='' then
              s:=Description
            else
              s:=FriendlyName;
            Writeln(Format('%-14.14s %-63.63s',[ClassName,s]));
          end;
        Free;
      end;
    except
      on e: exception do begin
        SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
        writeln(e.message);
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
      end;
    end;
  finally
    Separator;
  end;
end;

procedure ListAccs;
var
  i: Integer;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    try
      SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE or BACKGROUND_INTENSITY);
      Write('LOCAL ACCOUNTS...');
      with TAccounts.Create do begin
        Machine:=MachineName;
        RefreshUsers;
        RefreshLocalGroups;
        Write(Format('%d+%d=%d items',[UserCount,LocalGroupCount,UserCount+LocalGroupCount]));
        GetConsoleScreenBufferInfo(hConsole,ScreenBuffer);
        Write(StringOfChar(#32,ScreenBuffer.dwSize.X-ScreenBuffer.dwCursorPosition.X-1));
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        writeln;
        for i:=0 to UserCount-1 do
          with Users[i]^ do begin
            WriteLine     ('Name      : ',FOREGROUND_SILVER,Name,FOREGROUND_YELLOW);
            Writeln(Format('Comment   : %s',[Comment]));
            Writeln(Format('Domain    : %s',[Domain]));
            Writeln(Format('SID type  : %s',[GetNameUseStr(SidType)]));
            Writeln(Format('SID       : %s',[SID]));
            Writeln(Format('Last logon: %s',[DateTimeToStr(LastLogon)]));
            Writeln(Format('Last logoff: %s',[DateTimeToStr(LastLogoff)]));
            Separator;
          end;
        for i:=0 to LocalGroupCount-1 do
          with LocalGroups[i]^ do begin
            GetLocalGroupUsers(Name,sl);
            WriteLine     ('Name    : ',FOREGROUND_SILVER,Name,FOREGROUND_GREEN or FOREGROUND_INTENSITY);
            Writeln(Format('Comment : %s',[Comment]));
            Writeln(Format('Domain  : %s',[Domain]));
            Writeln(Format('SID type: %s',[GetNameUseStr(SidType)]));
            Writeln(Format('SID     : %s',[SID]));
            Writeln(Format('Members : %s',[sl.CommaText]));
            Separator;
          end;
        Free;
      end;
    except
      on e: exception do begin
        SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
        writeln(e.message);
        SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure SaveXML;
var
  sl: TStringList;
  s: string;
  MSIC: TMiTeC_SystemInfo;
  so: TScanObjects;
begin
  so:=soAll;
  if Switch('nl') then
    so:=so-[soEventLog];
  if Switch('na') then
    so:=so-[soAD];
  MSIC:=TMiTeC_SystemInfo.Create(nil);
  try
  with MSIC do begin
    sl:=TStringList.Create;
    try
      try
        if not Switch('sm') then
          Write('GENERATING XML REPORT...');
        RefreshData(so);
        SystemInfo_XML_Report(MSIC,sl);
        s:=SavePath+MachineName+'.xml';
        {$IFDEF UNICODE}
        sl.SaveToFile(s,TEncoding.UTF8);
        {$ELSE}
        sl.SaveToFile(s);
        {$ENDIF}
        SaveXSLTemplate(SavePath+XSLName);
        if not Switch('sm') then begin
          Writeln('OK');
          Writeln;
          Writeln('XML report saved to '+s);
        end;
      except
        on e: exception do begin
          SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
          if not Switch('sm') then
            writeln(e.message);
          SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        end;
      end;
    finally
      sl.Free;
    end;
  end;
  finally
    MSIC.Free;
  end;
  Writeln;
end;

procedure SaveData;
var
  wh: Boolean;
  s: string;
  MSIC: TMiTeC_SystemInfo;
  so: TScanObjects;
begin
  so:=soAll;
  if Switch('nl') then
    so:=so-[soEventLog];
  if Switch('na') then
    so:=so-[soAD];
  MSIC:=TMiTeC_SystemInfo.Create(nil);
  try
  with MSIC do begin
    try
      try
        if not Switch('sm') then
          Write('SAVING DATA...');
        ProcessList.DetectionRange:=[dtProcesses, dtServices];
        RefreshData(so);
        s:=SavePath+MachineName+cSIFExt;
        SaveToStorage(s,wh,1,'',CompressStream);
        if not Switch('sm') then begin
          Writeln('OK');
          Writeln;
          Writeln('Data saved to '+s);
        end;
      except
        on e: exception do begin
          SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
          if not Switch('sm') then
            writeln(e.message);
          SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        end;
      end;
    finally
    end;
  end;
  finally
    MSIC.Free;
  end;
  Writeln;
end;

procedure ConvertSIF2XML(AFilename: string);
var
  sl: TStringList;
  s: string;
  MSIC: TMiTeC_SystemInfo;
begin
  MSIC:=TMiTeC_SystemInfo.Create(nil);
  try
  with MSIC do begin
    sl:=TStringList.Create;
    try
      try
        Write('CONVERTING FILE...');
        LoadFromSIF(MSIC);
        SystemInfo_XML_Report(MSIC,sl);
        s:=ChangeFileExt(AFilename,'.xml');
        {$IFDEF UNICODE}
        sl.SaveToFile(s,TEncoding.UTF8);
        {$ELSE}
        sl.SaveToFile(s);
        {$ENDIF}
        Writeln('OK');
        Writeln;
        Writeln('Report saved to '+s);
      except
        on e: exception do begin
          SetConsoleTextAttribute(hConsole, FOREGROUND_RED or FOREGROUND_INTENSITY);
          writeln(e.message);
          SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
        end;
      end;
    finally
      sl.Free;
    end;
  end;
  finally
    MSIC.Free;
  end;
  Writeln;
end;

procedure Help;
begin
  Writeln;
  Writeln('Usage: GetSys [parameters]');
  Writeln;
  Writeln('Parameters: /? ......... ..this screen');
  Writeln('            /so .......... system overview');
  Writeln('            /lp .......... list running processes');
  Writeln('            /la .......... list local accounts (NT only)');
  Writeln('            /ld .......... list installed devices');
  Writeln('            /lt .......... list TCP/IP configuration');
  Writeln('            /ar .......... list autoruns');
  Writeln('            /xml.......... save data to XML file');
  Writeln('            /sif.......... save data to storage file');
  Writeln('            /nl .......... do not save EventLog');
  Writeln('            /na .......... do not save ActiveDirectory');
  Writeln('            /s2x:file .... load storage file and save it as XML');
  Writeln('            /p:path....... set path for file saving');
  Writeln('            /w  .......... wait for key press before exit');
  Writeln('            /sm .......... silent mode for /sif or /xml switch');
  Writeln;
  Writeln('Example: getsys /sif /nl /sm /p:c:\buffer ... saves SIF file without EventLog to c:\buffer with no message');
end;

var
  i,p: Integer;
  s: string;
begin
  SavePath:=ExtractFilePath(ParamStr(0));
  hConsole:=GetStdHandle(STD_OUTPUT_HANDLE);
  SetConsoleTextAttribute(hConsole, FOREGROUND_CYAN);
  if not Switch('sm') then begin
    Writeln(Format('%s %s',[EXEVersionInfo.Description{$IFDEF WIN64}+' (64-bit)'{$ENDIF},EXEVersionInfo.FileVersion]));
    Writeln(EXEVersionInfo.Copyright);
  end;
  SetConsoleTextAttribute(hConsole, FOREGROUND_SILVER);
  if not Switch('?') then begin
    if not Switch('sm') then begin
      Writeln('Type /? for options');
      writeln;
    end;
    for i:=1 to ParamCount do begin
      s:=LowerCase(ParamStr(i));
      p:=Pos('-p:',s);
      if p=0 then
        p:=Pos('/p:',s);
      if p>0 then
        SavePath:=IncludeTrailingPathDelimiter(Copy(ParamStr(i),p+3,255));
    end;
    for i:=1 to ParamCount do begin
      s:=LowerCase(ParamStr(i));
      p:=Pos('-s2x:',s);
      if p=0 then
        p:=Pos('/s2x:',s);
      if p>0 then
        ConvertSIF2XML(Copy(ParamStr(i),p+5,255));
    end;
    if (ParamCount=0) or Switch('so') or ((ParamCount=1) and Switch('w')) then
      SystemOverview;
    if Switch('lp') then
      ListProc;
    if Switch('ar') then
      ListAutoruns;
    if Switch('la') then
      ListAccs;
    if Switch('ld') then
      ListDevs;
    if Switch('lt') then
      ListTCPIP;
    if Switch('xml') then
      SaveXML;
    if Switch('sif') then
      SaveData;
    if Switch('w') then begin
      if not Switch('sm') then begin
        Writeln('');
        WriteLn('Press [Enter] to exit...');
      end;
      ReadLn;
    end;
  end else
    Help;
end.
