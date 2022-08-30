{*******************************************************}
{       MiTeC System Information Component Suite        }
{               APM Detection Part                      }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


{ \ \  }
unit MSI_APM;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj,
     {$ENDIF}
     MiTeC_SS, MSI_Common, MSI_Defs, MiTeC_PowrProf, MiTeC_WinIOCtl;

const
  StorageFolderName = 'APM';
  strm_Proc = 'Proc_%d';
  strm_Bat = 'Bat_%d';
  Processor_StorageFolderName = 'ProcessorList';
  Battery_StorageFolderName = 'BatteryList';

  BATTERY_UNKNOWN_VOLTAGE = $FFFFFFFF;
  BATTERY_UNKNOWN_RATE = $80000000;
  BATTERY_UNKNOWN_CAPACITY = $FFFFFFFF;
  BATTERY_UNKNOWN_TIME = $80000000;

type
  TPowerStatus = (psUnknown, psOffline, psOnline);

  TBatteryStatusFlag = (bsUnknown, bsHigh, bsLow, bsCritical, bsCharging, bsNoBattery, bsDischarging);

  TBatteryState = set of TBatteryStatusFlag;

  TBatteryRecord = record
    Tag: Byte;
    SymbolicLink,
    Description,
    Devicename,
    SerialNumber,
    UniqueID,
    Chemistry,
    Manufacturer: string;
    Temperature: Cardinal;
    DesignedCapacity,
    Capacity,
    CurrentCapacity,
    Voltage,
    PowerState,
    EstimatedTime: Cardinal;
    Rate: Longint;
  end;

  TBatteries = array of TBatteryRecord;

  TMiTeC_APM = class(TMiTeC_Component)
  private
    FBats: TBatteries;
    FBatteryLifePercent: Byte;
    FBatteryLifeFullTime: Cardinal;
    FBatteryLifeTime: Cardinal;
    FACPowerStatus: TPowerStatus;
    FBatteryChargeStatus: TBatteryState;
    FPS :TSystemPowerStatus;
    FProcCount,FCPUCount,FCoreCount,FThreadCount: Byte;
    FPPI: array of TProcessorPowerInformation;
    function GetBatteryFlag(ABS: Byte): TBatteryState;
    function GetPPI(CPUIndex: Byte): TProcessorPowerInformation;
    function GetBat(ABatIndex: Byte): TBatteryRecord;
    function GetBatCount: Byte;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property ProcessorPowerStatus[CPUIndex: Byte]: TProcessorPowerInformation read GetPPI;
    property Battery[BatIndex: Byte]: TBatteryRecord read GetBat;
  published
    property ACPowerStatus: TPowerStatus read FACPowerStatus stored false;
    property BatteryChargeStatus: TBatteryState read FBatteryChargeStatus stored false;
    property BatteryLifePercent: Byte read FBatteryLifePercent stored false;
    property BatteryLifeTime: Cardinal read FBatteryLifeTime stored false;
    property BatteryLifeFullTime: Cardinal read FBatteryLifeFullTime stored false;

    property ProcessorCount: Byte read FProcCount stored False;
    property BatteryCount: Byte read GetBatCount stored False;
  end;

function BatteryStateAsInt(A: TBatteryState): Cardinal;
function IntAsBatteryState(A: Cardinal): TBatteryState;
function GetACPSStr(ACPS: TPowerStatus): string;
function GetBSStr(BS: TBatteryState): string;

implementation

uses MiTeC_Routines, MiTeC_Datetime, MiTeC_CfgMgrSetupAPI;

{ TMiTeC_APM }

function BatteryStateAsInt;
var
  i: TBatteryStatusFlag;
begin
  Result:=0;
  for i:=Low(TBatteryStatusFlag) to High(TBatteryStatusFlag) do
    if i in A then
      Result:=Result or (1 shl Integer(i));
end;

function IntAsBatteryState;
var
  i: TBatteryStatusFlag;
begin
  Result:=[];
  for i:=Low(TBatteryStatusFlag) to High(TBatteryStatusFlag) do
    if (A and (1 shl Integer(i)))<>0 then
      Result:=Result+[i];
end;

function GetACPSStr(ACPS: TPowerStatus): string;
begin
  case ACPS of
    psUnknown: Result:='Unknown';
    psOnline: Result:='Online';
    psOffline: Result:='Offline';
  end;
end;

function GetBSStr(BS: TBatteryState): string;
begin
  Result:='';
  if bsDischarging in BS then
    Result:=Result+'Discharging, ';
  if bsHigh in BS then
    Result:=Result+'High, ';
  if bsLow in BS then
    Result:=Result+'Low, ';
  if bsCritical in BS then
    Result:=Result+'Critical, ';
  if bsCharging in BS then
    Result:=Result+'Charging, ';
  if bsNoBattery in BS then
    Result:=Result+'No battery, ';
  if bsUnknown in BS then
    Result:=Result+'Unknown, ';
  SetLength(Result,Length(Result)-2);
end;

procedure TMiTeC_APM.Clear;
begin
  FProcCount:=0;
  FACPowerStatus:=psUnknown;
  FBatteryLifePercent:=0;
  FBatteryLifeTime:=0;
  FBatteryLifeFullTime:=0;
  Finalize(FBats);
  Finalize(FPPI);
end;

constructor TMiTeC_APM.Create(AOwner: TComponent);
begin
  inherited;
  GetCPUTopology(FCPUCount,FCoreCount,FThreadCount);
end;

destructor TMiTeC_APM.Destroy;
begin
  Finalize(FPPI);
  Finalize(FBats);
  inherited;
end;

function TMiTeC_APM.GetBat(ABatIndex: Byte): TBatteryRecord;
begin
  Result:=FBats[ABatIndex];
end;

function TMiTeC_APM.GetBatCount: Byte;
begin
  Result:=Length(FBats);
end;

function TMiTeC_APM.GetBatteryFlag(ABS: Byte): TBatteryState;
begin
  FBatteryChargeStatus:=[];
  if ABS=0 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsDischarging];
  if (ABS and 1)=1 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsHigh];
  if (ABS and 2)=2 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsLow];
  if (ABS and 4)=4 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsCritical];
  if (ABS and 8)=8 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsCharging];
  if (ABS and 128)=128 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsNoBattery];
  if (ABS and 255)=255 then
    FBatteryChargeStatus:=FBatteryChargeStatus+[bsUnknown];
  Result:=FBatteryChargeStatus;
end;

function TMiTeC_APM.GetPPI(CPUIndex: Byte): TProcessorPowerInformation;
begin
  Result:=FPPI[CPUIndex];
end;

function TMiTeC_APM.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  Sub, Sub1: TStructuredStorage;
  i: integer;

function ReadProcessorFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Proc,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FPPI,Length(Self.FPPI)+1);
        with Self.FPPI[High(Self.FPPI)] do begin
          Number:=ReadIntProperty(sl,'Number');
          MaxMHz:=ReadIntProperty(sl,'MaxMHz');
          CurrentMHz:=ReadIntProperty(sl,'CurrentMHz');
          MHzLimit:=ReadIntProperty(sl,'MHzLimit');
          MaxIdleState:=ReadIntProperty(sl,'MaxIdleState');
          CurrentIdleState:=ReadIntProperty(sl,'CurrentIdleState');
        end;
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadBatteryFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Bat,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(Self.FBats,Length(Self.FBats)+1);
        with Self.FBats[High(Self.FBats)] do begin
          Tag:=ReadIntProperty(sl,'Tag');
          Devicename:=ReadStrProperty(sl,'DeviceName');
          Manufacturer:=ReadStrProperty(sl,'Manufacturer');
          Chemistry:=ReadStrProperty(sl,'Chemistry');
          SerialNumber:=ReadStrProperty(sl,'SerialNumber');
          UniqueID:=ReadStrProperty(sl,'UniqueID');
          DesignedCapacity:=ReadIntProperty(sl,'DesignedCapacity');
          Capacity:=ReadIntProperty(sl,'Capacity');
          CurrentCapacity:=ReadIntProperty(sl,'CurrentCapacity');
          Rate:=ReadIntProperty(sl,'Rate');
          Voltage:=ReadIntProperty(sl,'Voltage');
          PowerState:=ReadIntProperty(sl,'PowerState');
          EstimatedTime:=ReadIntProperty(sl,'EstimatedTime');
        end;
        Result:=True;
        SetDataAvail(True);
     finally
       sl.Free;
     end;
   finally
     strm.Free;
   end;
end;

begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);

  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FACPowerStatus:=TPowerStatus(ReadIntProperty(sl,'ACPowerStatus'));
            GetbatteryFlag(FPS.BatteryFlag);
            Self.FBatteryLifePercent:=ReadIntProperty(sl,'BatteryLifePercent');
            Self.FBatteryLifeFullTime:=ReadIntProperty(sl,'BatteryLifeFullTime');
            Self.FBatteryLifeTime:=ReadIntProperty(sl,'BatteryLifeTime');
            Self.FPS.BatteryFlag:=ReadIntProperty(sl,'BatteryFlag');
            GetBatteryFlag(FPS.BatteryFlag);
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      try
        Sub1:=Sub.OpenSubStorage(Processor_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        FCPUCount:=0;
        while ReadProcessorFromStream(FProcCount) do
          Inc(FProcCount);
        Result:=Result or (FProcCount>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(Battery_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadBatteryFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_APM.RefreshData;
var
  guid: TGUID;
  OK: Boolean;
  ppi: Pointer;
  i,j,r,c: Integer;
  hdev: HDEVINFO;
  hbat: THandle;
  w,t,n: cardinal;
  bqi: TBatteryQueryInformation;
  bi: TBatteryInformation;
  bs: TBatteryStatus;
  bws: TBatteryWaitStatus;
  br: TBatteryRecord;
  p: PWideChar;
  pi: TProcessorPowerInformation;
  did: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  s: string;
  le: integer;
begin
  inherited;
  Clear;
  guid:=GUID_DEVCLASS_BATTERY;
  hdev:=SetupDiGetClassDevs(@guid,nil,0,DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then begin
    try
      for i:=0 to 99 do begin
        FillChar(did,SizeOf(did),0);
        did.cbSize:=SizeOf(did);
        if (SetupDiEnumDeviceInterfaces(hdev,nil,guid,i,did)) then begin
          n:=0;
          SetupDiGetDeviceInterfaceDetail(hdev,@did,nil,0,n,nil);
          le:=GetLastError;
          if (le=ERROR_INSUFFICIENT_BUFFER) then begin
            n:=n;
            pdidd:=AllocMem(n);
            try
              pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData){$IFDEF FPC}+1{$ENDIF};
              if (SetupDiGetDeviceInterfaceDetail(hdev,@did,pdidd,n,n,nil)) then begin
                s:=PChar(@(pdidd.DevicePath));
                if (Trim(s)<>'') and (Trim(s)<>'\') then begin
                  SetLength(FBats,Length(FBats)+1);
                  FBats[High(FBats)].SymbolicLink:=s;
                  FBats[High(FBats)].Tag:=255;
                end;
              end;
            finally
              FreeMem(pdidd);
            end;
          end;
        end else begin
          le:=GetLastError;
          if le=ERROR_NO_MORE_ITEMS then
            Break;
        end;
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
  end;

  if Length(FBats)>0 then begin
    for i:=0 to High(FBats) do begin
      hbat:=GetDeviceHandle(FBats[i].SymbolicLink);
      if hbat=Cardinal(-1) then
        Continue;
      try
        w:=1;
        if not DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_TAG,@w,sizeof(Cardinal),@t,sizeof(Cardinal),n,nil) then
          Continue;

        FBats[i].Tag:=t;
        FillChar(bqi,sizeof(bqi),0);
        bqi.BatteryTag:=t;

        FillChar(bs,sizeof(bs),0);
        FillChar(bws,sizeof(bws),0);
        bws.BatteryTag:=bqi.BatteryTag;
        bws.Timeout:=100;
        if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_STATUS,@bws,SizeOf(bws),@bs,SizeOf(bs),n,nil) then begin
          FBats[i].Voltage:=bs.Voltage;
          FBats[i].CurrentCapacity:=bs.Capacity;
          FBats[i].Rate:=bs.Rate;
          FBats[i].PowerState:=bs.PowerState;
        end;

        p:=AllocMem(MAX_PATH+1);
        try
          bqi.InformationLevel:=BatteryManufactureName;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),p,255,n,nil) then
            FBats[i].Manufacturer:=WideCharToString(p);

          ZeroMemory(p,MAX_PATH+1);
          bqi.InformationLevel:=BatteryDeviceName;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),p,255,n,nil) then
            FBats[i].Devicename:=WideCharToString(p);

          ZeroMemory(p,MAX_PATH+1);
          bqi.InformationLevel:=BatterySerialNumber;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),p,255,n,nil) then
            FBats[i].SerialNumber:=WideCharToString(p);

          ZeroMemory(p,MAX_PATH+1);
          bqi.InformationLevel:=BatteryUniqueID;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),p,255,n,nil) then
            FBats[i].UniqueID:=WideCharToString(p);

          bqi.InformationLevel:=BatteryTemperature;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),@t,255,n,nil) then
            FBats[i].Temperature:=Round(t/10+273.15);

          bqi.InformationLevel:=BatteryEstimatedTime;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),@t,255,n,nil) then
            FBats[i].EstimatedTime:=t;

          bqi.InformationLevel:=BatteryInformation;
          if DeviceIoControl(hbat,IOCTL_BATTERY_QUERY_INFORMATION,@bqi,sizeof(BATTERY_QUERY_INFORMATION),@bi,sizeof(bi),n,nil) then begin
            FBats[i].Chemistry:=Copy(string(PAnsiChar(@bi.Chemistry)),1,4);
            FBats[i].DesignedCapacity:=bi.DesignedCapacity;
            FBats[i].Capacity:=bi.FullChargedCapacity;
          end;
        finally
          FreeMem(p);
        end;
      finally
        CloseHandle(hbat);
      end;
    end;

    for i:=0 to High(FBats) do
      for j:=High(FBats) downto i+1 do
        if FBats[i].Tag>FBats[j].Tag then begin
          br:=FBats[i];
          FBats[i]:=FBats[j];
          FBats[j]:=br;
        end;

  end;

  if Assigned(CallNtPowerInformation) then begin
    c:=SystemInfo.dwNumberOfProcessors;
    n:=0;
    ppi:=nil;
    ppi:=AllocMem(SizeOf(TProcessorPowerInformation)*c);
    try
      r:=CallNtPowerInformation(ProcessorInformation,nil,0,ppi,c*SizeOf(TProcessorPowerInformation));
      while (r<>0) do begin
        Inc(c);
        ReallocMem(ppi,c*SizeOf(TProcessorPowerInformation));
        r:=CallNtPowerInformation(ProcessorInformation,nil,0,ppi,c*SizeOf(TProcessorPowerInformation));
      end;
      if (c>1) and Odd(c) then
        Dec(c);
      i:=0;
      if FCoreCount>0 then begin
        FProcCount:=FCoreCount;
        n:=c div FCoreCount;
        if n=0 then
          n:=1;
      end else begin
        FProcCount:=c;
        n:=1;
      end;
      SetLength(FPPI,FProcCount);
      j:=0;
      while i<c do begin
        pi:=PProcessorPowerInformation(PProcessorPowerInformation(PAnsiChar(ppi)+i*SizeOf(TProcessorPowerInformation)))^;
        if pi.MaxMhz=0 then
          Break;
        FPPI[j]:=pi;
        Inc(j);
        inc(i,n);
      end;
    finally
      FreeMem(ppi);
    end;
  end;

  {CallNtPowerInformation(SystemBatteryState,nil,0,@FSBS,SizeOf(FSBS));

  CallNtPowerInformation(SystemPowerCapabilities,nil,0,@FSPC,SizeOf(FSPC));

  CallNtPowerInformation(SIM_PowrProf.LastWakeTime,nil,0,@FLWT,SizeOf(FLWT));
  CallNtPowerInformation(SIM_PowrProf.LastSleepTime,nil,0,@FLST,SizeOf(FLST));

  FLWT:=Round(FLWT/1000);
  FLST:=Round(FLST/1000);}

  ok:=GetSystemPowerStatus(FPS);
  if OK then begin
    case FPS.ACLineStatus of
      0 : FACPowerStatus:=psOffLine;
      1 : FACPowerStatus:=psOnLine;
      else FACPowerStatus:=psUnknown;
    end;
    FBatteryLifePercent:=FPS.BatteryLifePercent;
    FBatteryLifeTime:=FPS.BatteryLifeTime;
    FBatteryLifeFullTime:=FPS.BatteryFullLifeTime;
    GetBatteryFlag(FPS.BatteryFlag);
  end;
  SetDataAvail(True);
end;

procedure TMiTeC_APM.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: integer;

procedure WriteProcessorToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteIntProperty(sl,'Number',Self.ProcessorPowerStatus[AIndex].Number);
    WriteIntProperty(sl,'MaxMHz',Self.ProcessorPowerStatus[AIndex].MaxMhz);
    WriteIntProperty(sl,'CurrentMHz',Self.ProcessorPowerStatus[AIndex].CurrentMhz);
    WriteIntProperty(sl,'MHzLimit',Self.ProcessorPowerStatus[AIndex].MhzLimit);
    WriteIntProperty(sl,'MaxIdleState',Self.ProcessorPowerStatus[AIndex].MaxIdleState);
    WriteIntProperty(sl,'CurrentIdleState',Self.ProcessorPowerStatus[AIndex].CurrentIdleState);
    strm:=Sub.OpenStream(Format(strm_Proc,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteBatteryToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteIntProperty(sl,'Tag',Self.Battery[AIndex].Tag);
    WriteStrProperty(sl,'DeviceName',Self.Battery[AIndex].Devicename);
    WriteStrProperty(sl,'Manufacturer',Self.Battery[AIndex].Manufacturer);
    WriteStrProperty(sl,'Chemistry',Self.Battery[AIndex].Chemistry);
    WriteStrProperty(sl,'SerialNumber',Self.Battery[AIndex].SerialNumber);
    WriteStrProperty(sl,'UniqueID',Self.Battery[AIndex].UniqueID);
    WriteIntProperty(sl,'DesignedCapacity',Self.Battery[AIndex].DesignedCapacity);
    WriteIntProperty(sl,'Capacity',Self.Battery[AIndex].Capacity);
    WriteIntProperty(sl,'CurrentCapacity',Self.Battery[AIndex].CurrentCapacity);
    WriteIntProperty(sl,'Rate',Self.Battery[AIndex].Rate);
    WriteIntProperty(sl,'Voltage',Self.Battery[AIndex].Voltage);
    WriteIntProperty(sl,'PowerState',Self.Battery[AIndex].PowerState);
    WriteIntProperty(sl,'EstimatedTime',Self.Battery[AIndex].EstimatedTime);
    strm:=Sub.OpenStream(Format(strm_Bat,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);

  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'ACPowerStatus',Integer(Self.ACPowerStatus));
        WriteIntProperty(sl,'BatteryFlag',FPS.BatteryFlag);
        WriteIntProperty(sl,'BatteryLifePercent',Self.BatteryLifePercent);
        WriteIntProperty(sl,'BatteryLifeTime',Self.BatteryLifeTime);
        WriteIntProperty(sl,'BatteryLifeFullTime',Self.BatteryLifeFullTime);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
      for i:=0 to Self.ProcessorCount-1 do
        WriteProcessorToStream(i);
      for i:=0 to Self.BatteryCount-1 do
        WriteBatteryToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

initialization
  InitPPAAPI;
end.
