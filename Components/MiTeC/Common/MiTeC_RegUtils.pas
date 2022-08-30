{*******************************************************}
{               MiTeC Common Routines                   }
{                 Registry routines                     }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_RegUtils;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Win.Registry, System.Classes, System.SysUtils, System.Variants,
     {$ELSE}
     Windows, Classes, SysUtils, Variants, Registry,
     {$ENDIF}
     MiTeC_Windows;

function OpenRegistryReadOnly(ARoot: HKEY = HKEY_LOCAL_MACHINE): TRegistry;
function ReadRegInfo(ARoot :hkey; AKey, AValue :string) :string;
function ReadRegistryString(Root: HKEY; Key,Value: string): string;
function ReadRegistryValueAsString(Root: HKEY; const Key,Value: string; BinaryAsHex: Boolean = True): string;
function ReadCardinal(AReg: TRegistry; const Name: string): Cardinal;
procedure ModifyRegValue(ARootKey: HKEY; AKey, AName: string; AValue: Variant; AType: TRegDataType);
procedure GetRegistryKeyNames(Root: HKEY; Key: string; var KeyList: TStringList);
procedure WriteRegistryString(Root: HKEY; Key,ValueName,Value: string);
function DeleteRegistryValue(Root: HKEY; Key,Value: string): Boolean;

function GetClassDevices(AStartKey,AClassName,AValueName :string; var AResult :TStrings) :string;
procedure GetResourceListLocation(AKey: string; var RLKey, RLValue: string);
procedure GetLocation(ASource: string; var PCI, Device, Func: Integer);
procedure CreateMountedDevicesTable(ATable: TStrings; AOnlyDosDevices: Boolean = True; AFormat: Boolean = False);
function GetDeviceFriendlyName(ADev: string): string;
function GetMountPointUniqueID(AVolumeName: string): string;

implementation

uses {$IFDEF RAD9PLUS}
     System.RTLConsts
     {$ELSE}
     RTLConsts
     {$ENDIF}
     ;

function OpenRegistryReadOnly(ARoot: HKEY = HKEY_LOCAL_MACHINE): TRegistry;
var
  KeyAccess: Cardinal;
begin
  KeyAccess:=KEY_READ;
  if IsWow64 then
    KeyAccess:=KeyAccess or KEY_WOW64_64KEY;
  Result:=TRegistry.Create(KeyAccess);
  Result.RootKey:=ARoot;
end;

function ReadRegInfo(ARoot :hkey; AKey, AValue :string) :string;
begin
  with OpenRegistryReadOnly do begin
    result:='';
    rootkey:=aroot;
    if keyexists(akey) then begin
      OpenKey(akey,False);
      if ValueExists(avalue) then begin
        case getdatatype(avalue) of
          rdstring: result:=ReadString(avalue);
          rdinteger: result:=inttostr(readinteger(avalue));
        end;
      end;
      closekey;
    end;
    free;
  end;
end;


function ReadRegistryString(Root: HKEY; Key,Value: string): string;
begin
 Result:='';
 with OpenRegistryReadOnly do
   try
     RootKey:=Root;
      if OpenKey(Key,False) then begin
       Result:=ReadString(Value);
       CloseKey;
     end;
   finally
     Free;
   end;
end;

function ReadRegistryValueAsString(Root: HKEY; const Key,Value: string; BinaryAsHex: Boolean = True): string;
var
  Data: PByte;
  i: Integer;
  DataSize, DataType: Integer;
  p: array[0..4096] of char;
begin
 Result:='';
 with OpenRegistryReadOnly do
   try
     RootKey:=Root;
      if OpenKey(Key,False) and ValueExists(Value) then begin
       RegQueryValueEx(CurrentKey, PChar(Value), nil, PDWORD(@DataType), nil, PDWORD(@DataSize));
       case DataType of
         REG_SZ: Result:=ReadString(Value);
         REG_EXPAND_SZ: begin
           Result:=ReadString(Value);
           DataSize:=ExpandEnvironmentStrings(PChar(Result),PChar(@p),DataSize);
           if DataSize>0 then
             Result:=string(PChar(@p));
         end;
         REG_DWORD,REG_DWORD_BIG_ENDIAN: Result:=IntToStr(ReadInteger(Value));
         REG_MULTI_SZ: begin
           Result:='';
           if DataSize>-1 then begin
             Data:=Allocmem(DataSize);
             try
               RegQueryValueEx(CurrentKey,PChar(Value),nil,nil,PBYTE(Data),@DataSize);
               Result:=string(PChar(Data));
             finally
               Freemem(Data);
             end;
           end;
         end;
         else{REG_RESOURCE_LIST,
         REG_FULL_RESOURCE_DESCRIPTOR,
         REG_RESOURCE_REQUIREMENTS_LIST:} begin
           Result:='';
           if DataSize>-1 then begin
             Data:=Allocmem(DataSize);
             try
               ReadBinaryData(Value,Data^,DataSize);
               if BinaryAsHex then begin
                 for i:=0 to DataSize-1 do
                   Result:=Result+Format('%2.2x ',[Byte(PAnsiChar(Data)[i])]);
                 SetLength(Result,Length(Result)-1);
               end else
                 for i:=0 to DataSize-1 do
                   if Byte(PAnsiChar(Data)[i]) in [0..31] then
                     Result:=Result+'.'
                   else
                     Result:=Result+string(PAnsiChar(Data)[i]);
             finally
               Freemem(Data);
             end;
           end;
         end;
       end;
       CloseKey;
     end;
   finally
     Free;
   end;
end;

function ReadCardinal(AReg: TRegistry; const Name: string): Cardinal;
var
  RegData: TRegDataType;
  DataType: Integer;
  BufSize: Integer;
begin
  with AReg do begin
  DataType:=REG_NONE;
  BufSize:=SizeOf(Result);
  if RegQueryValueEx(CurrentKey,PChar(Name),nil,@DataType,PByte(@Result),@BufSize)<>ERROR_SUCCESS then
    raise ERegistryException.CreateResFmt(@SRegGetDataFailed,[Name]);
  if DataType=REG_SZ then
    RegData:=rdString
  else if DataType=REG_EXPAND_SZ then
    RegData:=rdExpandString
  else if DataType=REG_DWORD then
    RegData:=rdInteger
  else if DataType=REG_BINARY then
    RegData:=rdBinary
  else
    RegData:=rdUnknown;
  if RegData<>rdInteger then
    raise ERegistryException.CreateResFmt(@SInvalidRegType,[Name]);
  end;
end;

procedure ModifyRegValue(ARootKey: HKEY; AKey, AName: string; AValue: Variant; AType: TRegDataType);
begin
  with TRegistry.Create do
    try
      RootKey:=ARootKey;
      if OpenKey(AKey,True) then begin
        case AType of
          rdString, rdExpandString: WriteString(AName,AValue);
          rdInteger: WriteInteger(AName,AValue);
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;


procedure GetRegistryKeyNames(Root: HKEY; Key: string; var KeyList: TStringList);
begin
 with OpenRegistryReadOnly do
   try
     RootKey:=Root;
     if OpenKey(Key,False) then begin
       GetKeyNames(KeyList);
       CloseKey;
     end;
   finally
     Free;
   end;
end;

procedure WriteRegistryString(Root: HKEY; Key,ValueName,Value: string);
begin
  with TRegistry.Create do
    try
      RootKey:=Root;
      if OpenKey(Key,True) then begin
        WriteString(ValueName,Value);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function DeleteRegistryValue(Root: HKEY; Key,Value: string): Boolean;
begin
  Result:=False;
  with TRegistry.Create(KEY_WRITE OR KEY_WOW64_64KEY) do
    try
      RootKey:=Root;
      if OpenKey(Key,False) then begin
        Result:=DeleteValue(Value);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetClassDevices(AStartKey,AClassName,AValueName :string; var AResult :TStrings) :string;
var
  i,j :integer;
  sl :TStringList;
  s,v,rclass :string;
const
  rvGUID = 'ClassGUID';
  rvClass = 'Class';
  rvLink = 'Link';
begin
  Result:='';
  AResult.Clear;
  with OpenRegistryReadOnly do
    try
       RootKey:=HKEY_LOCAL_MACHINE;
       if OpenKey(AStartKey,False) then begin
         sl:=TStringList.Create;
         GetKeyNames(sl);
         CloseKey;
         for i:=0 to sl.Count-1 do
           if OpenKey(AStartKey+'\'+sl[i],False) then begin
             if ValueExists(rvClass) then begin
               rclass:=UpperCase(ReadString(rvClass));
               if rclass=UpperCase(AClassName) then begin
                 s:=sl[i];
                 Result:=s;
                 GetKeyNames(sl);
                 CloseKey;
                 for j:=0 to sl.count-1 do
                   if OpenKey(AStartKey+'\'+s+'\'+sl[j],False) then begin
                     if ValueExists(AValueName) then begin
                       v:=ReadString(AValueName);
                       if AResult.IndexOf(v)=-1 then
                         AResult.Add(v);
                     end;
                     CloseKey;
                   end;
                   Break;
               end;
             end;
             CloseKey;
           end;
         sl.free;
       end;
    finally
      Free;
    end;
end;

procedure GetResourceListLocation(AKey: string; var RLKey, RLValue: string);
var
  i,j :integer;
  sl,vl :TStringList;
  DataSize, DataType: Integer;
begin
  RLKey:='';
  RLValue:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(AKey,False) then begin
        sl:=TStringList.Create;
        vl:=TStringList.Create;
        try
          GetKeyNames(sl);
          sl.Sort;
          for i:=0 to sl.Count-1 do begin
            if OpenKey(AKey+'\'+sl[i],False) then begin
              GetValueNames(vl);
              for j:=0 to vl.Count-1 do begin
                RegQueryValueEx(CurrentKey,PChar(vl[j]),nil,PDWORD(@DataType),nil,PDWORD(@DataSize));
                if DataType=REG_RESOURCE_LIST then begin
                  RLKey:=AKey+'\'+sl[i];
                  RLValue:=vl[j];
                  Break;
                end;
              end;
              CloseKey;
              if Trim(RLKey)<>'' then
                Break;
            end;
          end;
        finally
          sl.Free;
          vl.Free;
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
end;

procedure GetLocation(ASource: string; var PCI, Device, Func: Integer);
var
  p: Integer;
begin
  PCI:=-1;
  Device:=-1;
  Func:=-1;
  if ASource='' then
    exit;
  p:=Pos('(',ASource);
  if p>0 then begin
    Delete(ASource,1,p);
    p:=Pos(')',ASource);
    Delete(ASource,p,255);
    ASource:=' '+StringReplace(ASource,',',', ',[rfReplaceAll,rfIgnorecase]);
  end;
  p:=Pos(',',ASource);
  if p>0 then begin
    PCI:=StrToIntDef(Trim(Copy(ASource,p-2,2)),0);
    Delete(ASource,1,p);
    p:=Pos(',',ASource);
    Device:=StrToIntDef(Trim(Copy(ASource,p-2,2)),0);
    Delete(ASource,1,p);
    Func:=StrToIntDef(Trim(Copy(ASource,Length(ASource)-1,2)),0);
  end;
end;

procedure CreateMountedDevicesTable(ATable: TStrings; AOnlyDosDevices: Boolean = True; AFormat: Boolean = False);
const
  rkMDNT = 'SYSTEM\MountedDevices';
var
  i,j,p,v: Integer;
  sl: TStringList;
  buf: array[0..2048] of ansichar;
  s: string;
begin
  ATable.Clear;
  sl:=TStringList.Create;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkMDNT,False) then begin
        GetValueNames(sl);
        for i:=0 to sl.Count-1 do begin
          p:=Pos('\DosDevices\',sl[i]);
          if (p=1) or not AOnlyDosDevices then begin
            try
              ZeroMemory(@buf,SizeOf(buf));
              j:=GetDataSize(sl[i]);
              j:=ReadBinaryData(sl[i],buf,j);
            except
              j:=0;
            end;
            if j>0 then begin
              if j>32 then
                s:=WideCharToString(@buf)
              else
                s:=string(buf);
              if Pos('???',s)=1 then
                s:=string(buf);
              if AFormat then begin
                v:=Pos('\#??#',s);
                if v>0 then
                  Delete(s,1,v+4);
                v:=Pos('_??_',s);
                if v>0 then
                  Delete(s,1,v+3);
                s:=StringReplace(s,'#','\',[rfReplaceAll,rfIgnoreCase]);
              end;
              if p=1 then
                ATable.Add(Format('%s=%s',[Copy(sl[i],13,1),s]))
              else
                ATable.Add(Format('%s=%s',[sl[i],s]));
            end;
          end;
        end;
        CloseKey;
      end;
    finally
      Free;
      sl.Free;
    end;
end;

function GetDeviceFriendlyName(ADev: string): string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(Format('SYSTEM\CurrentControlSet\Enum\%s',[ADev])) then begin
        if ValueExists('FriendlyName') then
          Result:=ReadString('FriendlyName');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetMountPointUniqueID(AVolumeName: string): string;
var
  p: array[0..255] of Byte;
begin
  Result:='';
  AVolumeName:='\??\'+Copy(AVolumename,Pos('Volume',AVolumename),255);
  Delete(AVolumeName,Length(Avolumename),1);
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SYSTEM\MountedDevices',False) then begin
        if ValueExists(AVolumename) then
          try
            ReadBinaryData(AVolumename,p,SizeOf(p));
            Result:=WideCharToString(PWideChar(@p));
          except
          end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;


end.
