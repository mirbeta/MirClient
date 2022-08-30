{*******************************************************}
{       MiTeC System Information Component Suite        }
{               Monitor Detection Part                  }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Monitor;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, WinApi.MultiMon,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MultiMon,
     {$ENDIF}
     MiTeC_SS, MSI_Common, MSI_Defs, MiTeC_EDID, MiTeC_Windows;

const
  StorageFoldername = 'Monitor';

type
  PMonitorRecord = ^TMonitorRecord;
  TMonitorRecord = record
    Handle: HMONITOR;
    DeviceID: string;
    DeviceDescription,
    Model: string;
    Manufacturer: string;
    RegistryKey: string;
    DriverKey: string;
    Bounds: TRect;
    WorkArea: TRect;
    IdentityNum: integer;
    MonitorNum: integer;
    DPI: integer;
    Primary: boolean;
    EDID: TEDIDRecord;
  end;

  PMonitorList = ^TMonitorList;
  TMonitorList = array of TMonitorRecord;

  TMiTeC_Monitor = class(TMiTeC_Component)
  private
    FMR: TMonitorList;
    function GetCount: integer;
    function GetMonitor(Index: integer): TMonitorRecord;
    procedure SetMonitor(Index: integer; const Value: TMonitorRecord);
    function DriverKeyCompare(AField: Integer; AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
    function MonitorNumCompare(AField: Integer; AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
    procedure Swap(AIndex1, AIndex2: integer);
    function GetPrimaryMonitorIndex: integer;
  protected
    procedure LoadEdidFields;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property Monitors[Index: integer]: TMonitorRecord read GetMonitor write SetMonitor; default;
    property Count: integer read GetCount;
    property PrimaryMonitorIndex: integer read GetPrimaryMonitorIndex;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Math,
     {$ELSE}
     Registry, Math,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Routines, MiTeC_CfgMgrSetupAPI, MiTeC_RegUtils, MiTeC_MultiMon;

function GetMonitorInfoCB(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall;
var
  mr: TMonitorRecord;
  ml: PMonitorList;
  mi: TMonitorInfoEx;
begin
  ml:=PMonitorList(pointer(l));
  FillChar(mi,sizeof(mi),0);
  mi.cbSize:=SizeOf(TMonitorInfoEx);
  GetMonitorInfo(hm,@mi);
  ResetMemory(mr,sizeof(mr));
  mr.Handle:=hm;
  mr.Bounds:=mi.rcMonitor;
  mr.WorkArea:=mi.rcWork;
  mr.Primary:=mi.dwFlags=MONITORINFOF_PRIMARY;
  mr.DeviceID:=mi.szDevice;
  mr.DPI:=GetPixelsPerInch(hm);
  mr.MonitorNum:=Length(ml^);
  SetLength(ml^,Length(ml^)+1);
  ml^[High(ml^)]:=mr;
  Result:=True;
end;

{ TMiTeC_Monitor }

procedure TMiTeC_Monitor.Clear;
begin
  Finalize(FMR);
end;

function TMiTeC_Monitor.DriverKeyCompare(AField, AIndex1, AIndex2: Integer;
  ADescending: Boolean): integer;
begin
  Result:=CompareText(FMR[AIndex1].DriverKey,FMR[AIndex2].DriverKey);
  if ADescending then
    Result:=-Result;
end;

destructor TMiTeC_Monitor.Destroy;
begin
  Clear;
  inherited;
end;

function TMiTeC_Monitor.GetCount: integer;
begin
  Result:=Length(FMR);
end;

procedure TMiTeC_Monitor.RefreshData;
const
  rkDeviceParams = 'Device Parameters';
  rvEDID = 'EDID';
var
  i,j,idx: Integer;
  s,v: string;
  guid: TGUID;
  did: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  dinfo: TSPDevInfoData;
  hdev: HDEVINFO;
  le,n,c: Cardinal;
  pm: PPhysicalMonitor;
  dd: TDisplayDeviceEx;
begin
  inherited;

  Clear;

  EnumDisplayMonitors(0,nil,GetMonitorInfoCB,integer(@FMR));
  for i:=0 to High(FMR) do begin
    if Assigned(GetNumberOfPhysicalMonitorsFromHMONITOR) then begin
      if GetNumberOfPhysicalMonitorsFromHMONITOR(FMR[i].Handle,n) then begin
        pm:=Allocmem(n*sizeof(TPhysicalMonitor));
        try
          if GetPhysicalMonitorsFromHMONITOR(FMR[i].Handle,n,pm) then begin
            {$IFDEF UNICODE}
            s:=string(pm.szPhysicalMonitorDescription);
            {$ELSE}
            s:=WideCharToString(pm.szPhysicalMonitorDescription);
            {$ENDIF}
            FMR[i].DeviceDescription:=s;
            DestroyPhysicalMonitors(n,pm);
          end;
        finally
          Freemem(pm);
        end;
      end;
    end;
  end;

  c:=0;
  dd.cb:=sizeof(dd);
  while EnumDisplayDevicesEx(nil,c,dd,EDD_GET_DEVICE_INTERFACE_NAME) do begin
    v:=dd.DeviceName;
    n:=0;
    while EnumDisplayDevicesEx(PChar(v),n,dd,EDD_GET_DEVICE_INTERFACE_NAME) do begin
      idx:=-1;
      for j:=0 to High(FMR) do
        if SameText(FMR[j].DeviceID,v) then begin
          idx:=j;
          Break;
        end;
      if idx>-1 then begin
        s:=dd.DeviceID;
        FMR[idx].DeviceID:=v;
        FMR[idx].RegistryKey:='\SYSTEM\CurrentControlSet\Enum\'+FastStringReplace(Copy(s,5,Pos('{',s)-5),'#','\');
        FMR[idx].DeviceDescription:=dd.DeviceString;
      end;
      inc(n);
    end;
    inc(c);
  end;

  guid:=GUID_DEVINTERFACE_MONITOR;
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
              pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
              dinfo.cbSize:=sizeof(TSPDevInfoData);
              if (SetupDiGetDeviceInterfaceDetail(hdev,@did,pdidd,n,n,@dinfo)) then begin
                s:=PChar(@(pdidd.DevicePath));
                if (Trim(s)<>'') and (Trim(s)<>'\') then begin
                  s:='\SYSTEM\CurrentControlSet\Enum\'+FastStringReplace(Copy(s,5,Pos('{',s)-5),'#','\');
                  idx:=-1;
                  for j:=0 to High(FMR) do
                    if SameText(FMR[j].RegistryKey,s) then begin
                      idx:=j;
                      Break;
                    end;
                  if idx>-1 then begin
                    FMR[idx].DeviceDescription:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
                    FMR[idx].Model:=ExtractFilename(GetString(hdev,dinfo,SPDRP_HARDWAREID));
                    FMR[idx].Manufacturer:=GetString(hdev,dinfo,SPDRP_MFG);
                    FMR[idx].DriverKey:=GetString(hdev,dinfo,SPDRP_DRIVER);
                    s:=GetString(hdev,dinfo,SPDRP_PHYSICAL_DEVICE_OBJECT_NAME);
                    //FMR[i].DeviceID:=s;
                    s:=ExtractFilename(s);
                    FMR[idx].IdentityNum:=StrToIntDef('$'+s,FMR[idx].MonitorNum);
                  end;
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

  LoadEdidFields;

  DynamicArrayQuickSort(0,0,High(FMR),MonitorNumCompare,Swap);
  for i:=0 to High(FMR) do
    FMR[i].IdentityNum:=i;

  DynamicArrayQuickSort(0,0,High(FMR),DriverKeyCompare,Swap);

  SetDataAvail(True);
end;

function TMiTeC_Monitor.GetMonitor(Index: integer): TMonitorRecord;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(Result),0);
  try
    Result:=FMR[Index];
  except
  end;
end;

function TMiTeC_Monitor.GetPrimaryMonitorIndex: integer;
var
  i: integer;
begin
  Result:=-1;
  for i:=0 to High(FMR) do
    if FMR[i].Primary then begin
      Result:=i;
      Break;
    end;
end;

procedure TMiTeC_Monitor.LoadEdidFields;
const
  rkDeviceParams = 'Device Parameters';
  rvEDID = 'EDID';
  rvDriver = 'Driver';
var
  edid: PAnsiChar;
  i, n: Integer;
begin
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      for i:=0 to High(FMR) do begin
        if OpenKey(FMR[i].RegistryKey,False) then begin
          if ValueExists(rvDriver) and (FMR[i].DriverKey='') then
            FMR[i].DriverKey:=ReadString(rvDriver);
          CloseKey;
        end;
        if OpenKey(FMR[i].RegistryKey+'\'+rkDeviceParams,False) then begin
          if ValueExists(rvEDID) then
            if GetDataType(rvEDID)=rdBinary then begin
              n:=GetDataSize(rvEDID);
              edid:=AllocMem(n+1);
              try
                ReadBinaryData(rvEDID,edid^,n);
                DecodeEDID(edid,FMR[i].EDID);
              finally
                FreeMem(edid);
              end;
            end;
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
end;

function TMiTeC_Monitor.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
   try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            SetLength(FMR,Length(FMR)+1);
            with FMR[High(FMR)] do begin
              Model:=ReadStrProperty(sl,'Model');
              Manufacturer:=ReadStrProperty(sl,'Manufacturer');
              DeviceDescription:=ReadStrProperty(sl,'DeviceDescription');
              DeviceID:=ReadStrProperty(sl,'DeviceID');
              RegistryKey:=ReadStrProperty(sl,'RegistryKey');
              Bounds.Left:=ReadIntProperty(sl,'Bounds.Left');
              Bounds.Top:=ReadIntProperty(sl,'Bounds.Top');
              Bounds.Right:=ReadIntProperty(sl,'Bounds.Right');
              Bounds.Bottom:=ReadIntProperty(sl,'Bounds.Bottom');
              WorkArea.Left:=ReadIntProperty(sl,'WorkArea.Left');
              WorkArea.Top:=ReadIntProperty(sl,'WorkArea.Top');
              WorkArea.Right:=ReadIntProperty(sl,'WorkArea.Right');
              WorkArea.Bottom:=ReadIntProperty(sl,'WorkArea.Bottom');
              DPI:=ReadIntProperty(sl,'DPI');
              Primary:=ReadIntProperty(sl,'Primary')=1;
              MonitorNum:=ReadIntProperty(sl,'MonitorNum');
              with EDID do begin
                Name:=ReadStrProperty(sl,'Name');
                ProductNumber:=ReadStrProperty(sl,'ProductNumber');
                Version:=ReadStrProperty(sl,'EDID_Version');
                Width:=ReadIntProperty(sl,'Width');
                Height:=ReadIntProperty(sl,'Height');
                SerialNumber:=ReadIntProperty(sl,'SerialNumber');
                Week:=ReadIntProperty(sl,'Week');
                Year:=ReadIntProperty(sl,'Year');
                ManufacturerCode:=ReadIntProperty(sl,'ManufacturerCode');
                ProductCode:=ReadIntProperty(sl,'ProductCode');
                Gamma:=ReadDblProperty(sl,'Gamma');
              end;
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

var
  i: Integer;
begin
  Finalize(Self.FMR);
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Exit;
    end;
    try
      i:=0;
      while ReadFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
      SetDataAvail(Result);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

function TMiTeC_Monitor.MonitorNumCompare(AField, AIndex1, AIndex2: Integer;
  ADescending: Boolean): integer;
begin
  Result:=CompareValue(FMR[AIndex1].MonitorNum,FMR[AIndex2].MonitorNum);
  if ADescending then
    Result:=-Result;
end;

procedure TMiTeC_Monitor.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Model',Self.Monitors[AIndex].Model);
      WriteStrProperty(sl,'Manufacturer',Self.Monitors[AIndex].Manufacturer);
      WriteStrProperty(sl,'DeviceDescription',Self.Monitors[AIndex].DeviceDescription);
      WriteStrProperty(sl,'DeviceID',Self.Monitors[AIndex].DeviceID);
      WriteStrProperty(sl,'RegistryKey',Self.Monitors[AIndex].RegistryKey);
      WriteIntProperty(sl,'Bounds.Left',Self.Monitors[AIndex].Bounds.Left);
      WriteIntProperty(sl,'Bounds.Top',Self.Monitors[AIndex].Bounds.Top);
      WriteIntProperty(sl,'Bounds.Right',Self.Monitors[AIndex].Bounds.Right);
      WriteIntProperty(sl,'Bounds.Bottom',Self.Monitors[AIndex].Bounds.Bottom);
      WriteIntProperty(sl,'WorkArea.Left',Self.Monitors[AIndex].WorkArea.Left);
      WriteIntProperty(sl,'WorkArea.Top',Self.Monitors[AIndex].WorkArea.Top);
      WriteIntProperty(sl,'WorkArea.Right',Self.Monitors[AIndex].WorkArea.Right);
      WriteIntProperty(sl,'WorkArea.Bottom',Self.Monitors[AIndex].WorkArea.Bottom);
      WriteIntProperty(sl,'DPI',Self.Monitors[AIndex].DPI);
      WriteIntProperty(sl,'MonitorNum',Self.Monitors[AIndex].MonitorNum);
      WriteIntProperty(sl,'Primary',integer(Self.Monitors[AIndex].Primary));
      with Self.Monitors[AIndex].EDID do begin
        WriteStrProperty(sl,'ProductNumber',ProductNumber);
        WriteStrProperty(sl,'Name',Name);
        WriteStrProperty(sl,'EDID_Version',Version);
        WriteIntProperty(sl,'Width',Width);
        WriteIntProperty(sl,'Height',Height);
        WriteIntProperty(sl,'SerialNumber',SerialNumber);
        WriteIntProperty(sl,'Week',Week);
        WriteIntProperty(sl,'Year',Year);
        WriteIntProperty(sl,'ManufacturerCode',ManufacturerCode);
        WriteIntProperty(sl,'ProductCode',ProductCode);
        WriteDblProperty(sl,'Gamma',Gamma);
      end;
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
end;

var
  i: Integer;
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
      for i:=0 to Self.Count-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Monitor.SetMonitor(Index: integer; const Value: TMonitorRecord);
begin
  if Index>High(FMR) then begin
    SetLength(FMR,Length(FMR)+1);
    Index:=Cardinal(High(FMR));
  end;
  FMR[Index]:=Value;
end;

procedure TMiTeC_Monitor.Swap(AIndex1, AIndex2: integer);
var
  r: TMonitorRecord;
begin
  r:=FMR[AIndex1];
  FMR[AIndex1]:=FMR[AIndex2];
  FMR[AIndex2]:=r;
end;

end.
