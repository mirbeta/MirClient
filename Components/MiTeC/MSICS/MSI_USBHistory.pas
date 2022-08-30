{*******************************************************}
{       MiTeC System Information Component Suite        }
{            USB History Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_USBHistory;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs;

const
  StorageFolderName = 'USBHistory';

type
  TUSBRecord = record
    Name: string;
    SerialNumber,
    DeviceClass: string;
    Timestamp: TDateTime;

    _PIP,
    _SN,
    _SRV,
    _HID: string;
  end;

  TUSBHistory = array of TUSBRecord;

  TMiTeC_USBHistory = class(TMiTeC_Component)
  private
    function GetRecord(AIndex: Cardinal): TUSBRecord;
    function GetRecordCount: Cardinal;
  protected
    FData: TUSBHistory;
    function DeviceExists(ARecord: TUSBRecord): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): Boolean; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;

    property Records[AIndex: Cardinal]: TUSBRecord read GetRecord;
  published
    property RecordCount: Cardinal read GetRecordCount;
  end;

function ParseDeviceName(S: string): string;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$else}
     Registry,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Datetime, MiTeC_Routines, MiTeC_RegUtils;

{ TMiTeC_USBHistory }

procedure TMiTeC_USBHistory.Clear;
begin
  Finalize(FData);
end;

constructor TMiTeC_USBHistory.Create;
begin
  inherited;
end;

destructor TMiTeC_USBHistory.Destroy;
begin
  Finalize(FData);
  inherited;
end;

function TMiTeC_USBHistory.DeviceExists(ARecord: TUSBRecord): Boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to High(FData) do
    if SameText(Arecord.Name,FData[i].Name) and (Int(ARecord.Timestamp)=Int(FData[i].Timestamp)) then begin
      Result:=True;
      Break;
    end;
end;

function TMiTeC_USBHistory.GetRecord(AIndex: Cardinal): TUSBRecord;
begin
  try
    Result:=FData[AIndex];
  except
    Finalize(Result);
  end;
end;

function TMiTeC_USBHistory.GetRecordCount: Cardinal;
begin
  Result:=Length(FData);
end;

function TMiTeC_USBHistory.LoadFromStorage;

procedure ParseRecord(ASource: string; var ARecord: TUSBRecord);
var
  p: Integer;
begin
  Finalize(Arecord);
  ZeroMemory(@ARecord,SizeOf(ARecord));
  p:=Pos(';',ASource);
  if p=0 then
    Exit;
  p:=Pos(';',ASource);
  ARecord.Name:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.DeviceClass:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  ARecord.Timestamp:=StrToFloatDef(ASource,0);
  p:=Pos(';',ASource);
  if p>0 then
    ARecord.SerialNumber:=Copy(ASource,1,p-1);
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  i: Integer;
  sl: TStringList;
  r: TUSBRecord;
  ds: char;
begin
  Sub:=nil;
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  Finalize(FData);
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  sl:=TStringList.Create;
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then begin
      strm:=Sub.OpenStream(strm_Data,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        for i:=0 to sl.Count-1 do begin
          ParseRecord(sl[i],r);
          SetLength(FData,Length(FData)+1);
          FData[High(FData)]:=r;
        end;
        SetDataAvail(Length(Fdata)>0);
      finally
        strm.Free;
      end;
    end;
  finally
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
    sl.Free;
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

function ParseDeviceName(S: string): string;
var
  i: Cardinal;
  v,p,r: string;
begin
  Result:='';
  v:='';
  p:='';
  r:='';
  i:=Pos('&VEN_',Uppercase(S));
  if i=0 then
    Exit;
  Delete(S,1,i+4);
  i:=Pos('&PROD_',Uppercase(S));
  if i>0 then begin
    v:=Copy(S,1,i-1);
    Delete(S,1,i+5);
    i:=Pos('&REV_',Uppercase(S));
    if i>0 then begin
      p:=Copy(S,1,i-1);
      Delete(S,1,i+4);
      i:=Pos('\',S);
      if i>0 then
        Delete(S,i,Length(S));
      r:=S;
    end;
  end;
  Result:=Trim(StringReplace(Format('%s %s %s',[v,p,r]),'_',' ',[rfReplaceAll,rfIgnoreCase]));
end;

function DecodeSerial(const AValue: string): string;
var
  i,b: Integer;
  s: AnsiString;
begin
  Result:='';
  s:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(AValue);

  i:=1;
  while i<Length(s) do begin
    if TryStrtoInt('$'+Copy(string(s),i,2),b) and (b in [32..127]) then begin
      Result:=Result+Chr(b);
      inc(i,2);
    end else begin
      Result:=string(s);
      break;
    end;
  end;
end;

procedure TMiTeC_USBHistory.RefreshData(AScanObjects: TScanObjects = soAll);
const
  rkNT = 'SYSTEM\CurrentControlSet\Enum';
  rk9x = 'Enum';
    rvPIP = 'ParentIdPrefix';
    rvSrv = 'Service';
    rvD = 'Driver';
    rvFN = 'FriendlyName';
    rvC = 'Class';
    rvDD = 'DeviceDesc';
    rvHID = 'HardwareID';
    rvSN = 'SymbolicName';
    rkontrol = 'Control';
  rkMDNT = 'SYSTEM\MountedDevices';
var
  sl,kl,dl,vl: TStringList;
  i,j,k,l: Integer;
  rk,dn,rksrv,dc: string;
  b: Boolean;
  rki: TRegKeyInfo;
  ts: TDateTime;
  r: TUSBRecord;
begin
  Finalize(FData);
  with OpenRegistryReadOnly do begin
    sl:=TStringList.Create;
    kl:=TStringList.Create;
    dl:=TStringList.Create;
    vl:=TStringList.Create;
    try
      Rootkey:=HKEY_LOCAL_MACHINE;

      if Win32Platform=VER_PLATFORM_WIN32_NT then
        rk:=rkNT
      else
        rk:=rk9x;
      if OpenKey(rk+'\USB',False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if OpenKey(Format('%s\USB\%s',[rk,sl[i]]),False) then begin
            GetKeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do begin
              if OpenKey(Format('%s\USB\%s\%s',[rk,sl[i],kl[j]]),False) then begin
                Finalize(r);
                GetKeyInfo(rki);
                {$IFDEF FPC}
                ts:=rki.FileTime;
                {$ELSE}
                try ts:=FileTimeTodateTime(rki.FileTime,ConvertTimeToLocal); except ts:=0 end;
                {$ENDIF}
                if ValueExists(rvD) then begin
                  r._sn:=Uppercase(ReadString(rvD));
                  if Win32Platform=VER_PLATFORM_WIN32_NT then begin
                    if ValueExists(rvPIP) then
                      r._pip:=ReadString(rvPIP)
                    else
                      r._pip:=kl[j];
                    if Pos('&',r._PIP)=0 then
                      r.SerialNumber:=DecodeSerial(r._PIP);
                    if ValueExists(rvSrv) then
                      r._srv:=ReadString(rvSrv);
                  end else begin
                    r._pip:=Format('%s&%s',[sl[i],kl[j]]);
                  end;
                  if ValueExists(rvDD) then begin
                    r.Name:=Trim(ReadString(rvDD));
                    if Pos(';',r.Name)>0 then
                      r.Name:=Copy(r.Name,Pos(';',r.Name)+1,1024);
                  end;
                end;
                CloseKey;
                if OpenKey(Format('%s\USB\%s\%s\Device Parameters',[rk,sl[i],kl[j]]),False) then begin
                  if ValueExists(rvSN) then
                    r._hid:=Trim(ReadString(rvSN));
                  CloseKey;
                end;
                if OpenKey(Format('%s\USB\%s\%s\Control',[rk,sl[i],kl[j]]),False) then begin
                  GetKeyInfo(rki);
                  {$IFDEF FPC}
                  ts:=rki.FileTime;
                  {$ELSE}
                  try ts:=FileTimeTodateTime(rki.FileTime,ConvertTimeToLocal); except ts:=0 end;
                  {$ENDIF}
                  CloseKey;
                end;
                r.TimeStamp:=ts;
                if (r._srv='') or (PosText('USB',r._SRV)>0) then
                  r._srv:='USBSTOR';
                if SameText(r._srv,'USBSTOR') then begin
                  r.Name:='';
                  rksrv:=Format('%s\%s',[rk,r._SRV]);
                  if OpenKey(rksrv,False) then begin
                    GetKeyNames(dl);
                    CloseKey;
                    for k:=0 to dl.Count-1 do begin
                      if OpenKey(Format('%s\%s',[rksrv,dl[k]]),False) then begin
                        GetkeyNames(vl);
                        CloseKey;
                        for l:=0 to vl.Count-1 do begin
                          if Win32Platform=VER_PLATFORM_WIN32_NT then
                            b:=PosText(r._PIP,vl[l])=1
                          else
                            b:=PosText(r._pip,vl[l])>0;
                          if b then begin
                            if OpenKey(Format('%s\%s\%s',[rksrv,dl[k],vl[l]]),False) then begin
                              dn:=ParseDeviceName(dl[k]);
                              if dn<>'' then
                                r.Name:=dn;
                              if r.Name='' then
                                if ValueExists(rvFN) then
                                  r.Name:=Trim(ReadString(rvFN));
                              if ValueExists(rvDD) then begin
                                dc:=Trim(ReadString(rvDD));
                                if Pos(';',dc)>0 then
                                 dc:=Copy(dc,Pos(';',dc)+1,1024);
                              end;
                              r.DeviceClass:=dc;
                              if r.Name='' then
                                r.Name:=dc;
                              if not DeviceExists(r) then begin
                                SetLength(FData,Length(FData)+1);
                                FData[High(FData)]:=r;
                              end;
                              CloseKey;
                              //ok:=True;
                              //Break;
                            end;
                          end;
                        end;
                      end;
                      {if ok then
                        Break;}
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      vl.Free;
      sl.Free;
      kl.Free;
      dl.Free;
      Free;
    end;
  end;
  SetDataAvail(Length(Fdata)>0);
end;

procedure TMiTeC_USBHistory.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
  ds: char;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
  try
    Sub.DeleteElement(strm_Data);
    strm:=Sub.OpenStream(strm_Data,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
      try
        for i:=0 to High(FData) do
          sl.Add(Format('%s;%s;%1.10f;%s',[FData[i].Name,
                                    FData[i].DeviceClass,
                                   FData[i].Timestamp,
                                   FData[i].SerialNumber
                                  ]));
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
        sl.Free;
      end;
    finally
      strm.Free;
    end;
  finally
    Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

end.
