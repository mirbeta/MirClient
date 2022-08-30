{*******************************************************}
{       MiTeC System Information Component Suite        }
{             WI-FI Known Networks                      }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_WLANC;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs;

const
  StorageFolderName = 'WLANC';

type
  TWLANCRecord = record
    SSID,
    Key,
    Authentication,
    Encryption,
    Connection,
    AdapterName,
    GUID,
    IPAddress: string;
    Timestamp: TDateTime;
    _keyMaterial: string;
  end;

  TWLANC = array of TWLANCRecord;

  TMiTeC_WLANC = class(TMiTeC_Component)
  private
    FData: TWLANC;
    FFiles: TStrings;
    function GetCount: integer;
    function GetRecord(Index: integer): TWLANCRecord;
    procedure AnalyzeFile(AFilename: string);
    procedure RefreshDataXP;
    procedure RefreshDataVista;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): Boolean; override;
    procedure ScanFiles(AUser: string = '');
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    function FindNetwork(const ASSID,AGUID: string): Integer;

    property Records[Index: integer]: TWLANCRecord read GetRecord;
  published
    property RecordCount: integer read GetCount;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     XML.XMLDOM, XML.XMLIntf, XML.XMLDoc, XML.Win.msxmldom, System.Win.Registry,
     {$ELSE}
     {$IFDEF FPC}
     DOM, XmlRead, XmlWrite,
     {$ELSE}
     xmldom, XMLIntf, msxmldom, XMLDoc,
     {$ENDIF}
      Registry,
     {$ENDIF}
     MiTeC_WinCrypt, MiTeC_Datetime, MiTeC_Routines, MiTeC_SysUtils, MiTeC_RegUtils;

{ TMiTeC_WLANC }

procedure TMiTeC_WLANC.AnalyzeFile(AFilename: string);
var
  XMLDoc: TXMLDocument;
  k,n: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
  r: TWLANCRecord;
  fi: TFileInfo;
begin
  GetFileInfo(Afilename,fi);
  r.Timestamp:=fi.Modified;
  r.GUID:=ExtractFilename(ExcludeTrailingPathDelimiter(ExtractFilePath(AFilename)));
  {$IFDEF FPC}
  ReadXMLFile(XMLDoc,AFilename);
    n:=XMLDoc.DocumentElement.FindNode('SSIDConfig');
    if n<>nil then begin
      n:=n.FindNode('SSID');
      if n<>nil then
        r.SSID:=n.FindNode('name').TextContent;
    end;
    n:=XMLDoc.DocumentElement.FindNode('connectionType');
    if n<>nil then
      r.Connection:=n.TextContent;
    n:=XMLDoc.DocumentElement.FindNode('MSM');
    if n<>nil then begin
      k:=n.FindNode('security');
      if (k<>nil) then begin
        n:=k.FindNode('authEncryption');
        if n<>nil then begin
          r.Authentication:=n.FindNode('authentication').TextContent;
          r.Encryption:=n.FindNode('encryption').TextContent;
        end;
        n:=k.FindNode('sharedKey');
        if n<>nil then
          r._keyMaterial:=n.FindNode('keyMaterial').TextContent;
      end;
    end;
    if r.SSID<>'' then begin
      SetLength(FData,Length(FData)+1);
      FData[High(FData)]:=r;
    end;
  {$ELSE}
  XMLDoc:=TXMLDocument.Create(Self);
  try
    XMLDoc.FileName:=AFilename;
    XMLDoc.Active:=True;
    n:=XMLDoc.DocumentElement.ChildNodes.FindNode('SSIDConfig');
    if n<>nil then begin
      n:=n.ChildNodes.FindNode('SSID');
      if n<>nil then
        r.SSID:=n.ChildNodes['name'].Text;
    end;
    n:=XMLDoc.DocumentElement.ChildNodes.FindNode('connectionType');
    if n<>nil then
      r.Connection:=n.Text;
    n:=XMLDoc.DocumentElement.ChildNodes.FindNode('MSM');
    if n<>nil then begin
      k:=n.ChildNodes.FindNode('security');
      if (k<>nil) then begin
        n:=k.ChildNodes.FindNode('authEncryption');
        if n<>nil then begin
          r.Authentication:=n.ChildNodes['authentication'].Text;
          r.Encryption:=n.ChildNodes['encryption'].Text;
        end;
        n:=k.ChildNodes.FindNode('sharedKey');
        if n<>nil then
          r._keyMaterial:=n.ChildNodes['keyMaterial'].Text;
      end;
    end;
    if r.SSID<>'' then begin
      SetLength(FData,Length(FData)+1);
      FData[High(FData)]:=r;
    end;
    XMLDoc.Active:=False;
  finally
    XMLDoc.Free;
  end;
  {$ENDIF}
end;

procedure TMiTeC_WLANC.Clear;
begin
  FFiles.Clear;
  Finalize(FData);
end;

constructor TMiTeC_WLANC.Create(AOwner: TComponent);
begin
  inherited;
  FFiles:=TStringList.Create;
end;

destructor TMiTeC_WLANC.Destroy;
begin
  FFiles.Free;
  Finalize(FData);
  inherited;
end;

function TMiTeC_WLANC.FindNetwork(const ASSID, AGUID: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FData) do
    if SameText(FData[i].SSID,ASSID) and SameText(FData[i].GUID,AGUID) then begin
      Result:=i;
      Break;
    end;
end;

function TMiTeC_WLANC.GetCount: integer;
begin
  Result:=Length(FData);
end;

function TMiTeC_WLANC.GetRecord(Index: integer): TWLANCRecord;
begin
  Result:=FData[Index];
end;

function TMiTeC_WLANC.LoadFromStorage;

procedure ParseRecord(ASource: string; var ARecord: TWLANCRecord);
var
  p: Integer;
begin
  Finalize(Arecord);
  ZeroMemory(@ARecord,SizeOf(ARecord));
  p:=Pos(';',ASource);
  if p=0 then
    Exit;
  ARecord.SSID:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Key:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Authentication:=Copy(ASource,1,p-1);
  p:=Pos(';',ASource);
  ARecord.Encryption:=Copy(ASource,1,p-1);
  p:=Pos(';',ASource);
  ARecord.Connection:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.AdapterName:=Copy(ASource,1,p-1);
  p:=Pos(';',ASource);
  ARecord.GUID:=Copy(ASource,1,p-1);
  p:=Pos(';',ASource);
  ARecord.IPAddress:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  ARecord.Timestamp:=StrToIntdef(Copy(ASource,1,p-1),0);
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  i: Integer;
  sl: TStringList;
  r: TWLANCRecord;
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
        SetDataAvail(True);
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

procedure TMiTeC_WLANC.RefreshDataXP;
const
  rkWZC = '\SOFTWARE\Microsoft\WZCSVC\Parameters\Interfaces\';
  rkTCPIP = '\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\';
  rkNC = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion\NetworkCards\';
var
  i,j: Integer;
  nl,sl: TStringList;
  ip,v: string;
  rdi: TRegDataInfo;
  buf: TByteArray;
  c: array[0..255] of ansichar;
  b: Byte;
  rki: TRegKeyInfo;
begin
  Finalize(FData);
  sl:=TStringList.Create;
  nl:=TStringList.Create;
  with TRegistry.Create do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkNC,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkNC+sl[i],False) then begin
            nl.Add(ReadString('ServiceName')+'='+ReadString('Description'));
            CloseKey;
          end;
      end;
      if OpenKeyReadOnly(rkWZC) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          ip:='0.0.0.0';
          if OpenKeyReadOnly(rkTCPIP+sl[i]) then begin
            if ValueExists('IPAddress') then begin
              GetDataInfo('IPAddress',rdi);
              SetLength(ip,rdi.DataSize);
              ReadBinaryData('IPAddress',ip[1],rdi.DataSize);
              ip:=Trim(ip);
            end;
            if SameText(ip,'0.0.0.0') and ValueExists('DhcpIPAddress') then
              ip:=ReadString('DhcpIPAddress');
            CloseKey;
          end;
          if OpenKeyReadOnly(rkWZC+sl[i]) then begin
            GetKeyInfo(rki);
            for j:=0 to 65535 do begin
              v:=Format('Static#%4.4x',[j]);
              if ValueExists(v) then begin
                GetDataInfo(v,rdi);
                if rdi.RegData=rdBinary then begin
                  ZeroMemory(@buf,SizeOf(buf));
                  ReadBinaryData(v,buf,rdi.DataSize);
                  Zeromemory(@c,SizeOf(c));
                  Move(buf[$14],c,buf[$10]);
                  if Trim(string(c))<>'' then begin
                    SetLength(FData,Length(FData)+1);
                    with FData[High(FData)] do begin
                      SSID:=Trim(string(c));
                      IPAddress:=ip;
                      GUID:=sl[i];
                      AdapterName:=nl.Values[GUID];
                      {$IFDEF FPC}
                      TimeStamp:=rki.FileTime;
                      {$ELSE}
                      TimeStamp:=FileTimeTodateTime(rki.FileTime);
                      {$ENDIF}
                      Move(buf[$34],b,1);
                      case b of
                        0: Encryption:='WEP';
                        1: Encryption:='Disabled';
                        4: Encryption:='TKIP';
                        6: Encryption:='AES';
                      end;
                      Move(buf[$94],b,1);
                      case b of
                        0: Encryption:='Open';
                        1: Encryption:='Shared';
                        3,5: Encryption:='WPA';
                        4: Encryption:='WPA-PSK';
                        6: Encryption:='WPA2';
                        7: Encryption:='WPA2-PSK';
                      end;
                    end;
                  end;
                end;
              end;
            end;
            CloseKey;
          end;
        end;
      end;
    finally
      Free;
      sl.Free;
      nl.Free;
    end;
end;

procedure TMiTeC_WLANC.RefreshData(AScanObjects: TScanObjects = soAll);
begin
  if (Win32MajorVersion>5) then
    RefreshDataVista
  else
    RefreshDataXP;
end;

procedure TMiTeC_WLANC.RefreshDataVista;
const
  rkTCPIP = '\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\';
  rkNC = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion\NetworkCards\';
var
  DataIn, DataOut: DATA_BLOB;
  pid,i: Integer;
  ph,th: THandle;
  ok: Boolean;
  sid: Cardinal;
  sl,kl: TStringList;
  reg: TRegistry;
  rdi: TRegDataInfo;
begin
  Finalize(FData);
  ph:=0;
  th:=0;
  ok:=False;
  sid:=DWORD(-1);
  pid:=GetProcessID('winlogon.exe',sid);
  if pid>-1 then begin
    if SetDebugPriv then begin
      ph:=OpenProcess(MAXIMUM_ALLOWED,False,pid);
      if OpenProcessToken(ph,MAXIMUM_ALLOWED,th) then
        ok:=ImpersonateLoggedOnUser(th);
    end;
  end;

  sl:=TStringList.Create;
  kl:=TStringList.Create;
  reg:=OpenRegistryReadOnly;
  try

    ScanFiles;

    for i:=0 to FFiles.Count-1 do
      AnalyzeFile(FFiles[i]);

    with reg do begin
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkNC,False) then begin
        GetKeyNames(kl);
        CloseKey;
        for i:=0 to kl.Count-1 do
          if OpenKey(rkNC+kl[i],False) then begin
            sl.Add(ReadString('ServiceName')+'='+ReadString('Description'));
            CloseKey;
          end;
      end;
    end;

    for i:=0 to High(FData) do begin
      FData[i].IPAddress:='';
      with reg do
        if OpenKey(rkTCPIP+FData[i].GUID,False) then begin
          if ValueExists('IPAddress') then begin
            GetDataInfo('IPAddress',rdi);
            if rdi.RegData=rdBinary then begin
              SetLength(FData[i].IPAddress,rdi.DataSize);
              ReadBinaryData('IPAddress',FData[i].IPAddress[1],rdi.DataSize);
              FData[i].IPAddress:=Trim(FData[i].IPAddress);
            end else if rdi.RegData=rdString then begin
              FData[i].IPAddress:=Trim(ReadString('IPAddress'));
            end;
          end;
          if (SameText(FData[i].IPAddress,'0.0.0.0') or (FData[i].IPAddress='')) and ValueExists('DhcpIPAddress') then
            FData[i].IPAddress:=ReadString('DhcpIPAddress');
          CloseKey;
        end;
      FData[i].AdapterName:=sl.Values[Fdata[i].GUID];
      if CryptStringToBinary(PChar(FData[i]._keyMaterial),Length(FData[i]._keyMaterial),CRYPT_STRING_HEX,nil,DataIn.cbData,nil,nil) then begin
        DataIn.pbData:=Allocmem(DataIn.cbData);
        if CryptStringToBinary(PChar(FData[i]._keyMaterial),0,CRYPT_STRING_HEX,DataIn.pbData,DataIn.cbData,nil,nil) then begin
          if CryptUnprotectData(@DataIn,nil,nil,nil,nil,0,@DataOut) then
            FData[i].Key:=string(PAnsiChar(DataOut.pbData))
          else if CryptUnprotectData(@DataIn,nil,nil,nil,nil,CRYPTPROTECT_UI_FORBIDDEN,@DataOut) then
            FData[i].Key:=string(PAnsiChar(DataOut.pbData))
          else
            FData[i].Key:=FData[i]._keyMaterial;
        end;
        Freemem(DataIn.pbData);
      end;
    end;

  finally
    sl.Free;
    kl.Free;
    reg.Free;

    if ok then
      RevertToSelf;
    if ph>0 then
      CloseHandle(ph);
    if th>0 then
      CloseHandle(th);
  end;
end;

procedure TMiTeC_WLANC.SaveToStorage;
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
          sl.Add(Format('%s;%s;%s;%s;%s;%s;%s;%s;%1.10f;',[
                                   FData[i].SSID,
                                   FData[i].Key,
                                   FData[i].Authentication,
                                   FData[i].Encryption,
                                   FData[i].Connection,
                                   FData[i].AdapterName,
                                   FData[i].GUID,
                                   FData[i].IPAddress,
                                   FData[i].Timestamp
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

procedure TMiTeC_WLANC.ScanFiles;
var
  s: string;
begin
  if not LiveData then
    Exit;

  if AUser='' then
    AUser:=WindowsUser;

  FFiles.Clear;
  s:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_COMMON_APPDATA))+'Microsoft\Wlansvc\Profiles\Interfaces\';
  BuildFileList(s,'*.xml',faAnyFile,FFiles,True);
end;

end.
