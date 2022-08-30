{*******************************************************}
{       MiTeC System Information Component Suite        }
{             Security Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Security;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_Routines, MiTeC_WMI;

const
  StorageFolderName = 'Security';

type
  TMiTeC_Security = class(TMiTeC_Component)
  private
    FFW: TStrings;
    FAV: TStrings;
    FAS: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property AntiVirus: TStrings read FAV stored False;
    property AntiSpyware: TStrings read FAS stored False;
    property Firewall: TStrings read FFW stored False;
  end;

implementation

uses MiTeC_StrUtils, {$IFDEF FPC}MiTeC_FPC_WbemScripting_TLB{$ELSE}MiTeC_WbemScripting_TLB{$ENDIF};

{ TMiTeC_Security }

procedure TMiTeC_Security.Clear;
begin
  FAS.Clear;
  FAV.Clear;
  FFW.Clear;
end;

constructor TMiTeC_Security.Create(AOwner: TComponent);
begin
  inherited;
  FAS:=TStringList.Create;
  FAV:=TStringList.Create;
  FFW:=TStringList.Create;
end;

destructor TMiTeC_Security.Destroy;
begin
  FAS.Free;
  FAV.Free;
  FFW.Free;
  inherited;
end;

function TMiTeC_Security.LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure): boolean;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Sub:=nil;
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

    if Assigned(Sub) then begin
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FAV.CommaText:=ReadStrProperty(sl,'AntiVirus');
            Self.FAS.CommaText:=ReadStrProperty(sl,'AntiSpyware');
            Self.FFW.CommaText:=ReadStrProperty(sl,'Firewall');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

procedure TMiTeC_Security.RefreshData(AScanObjects: TScanObjects);
var
  wmiServices: ISWbemServices;
  wmi: TInstances;

procedure Add(AList: TStrings; AWMI: TInstances);
var
  i,idx: Integer;
  s,fn: string;
begin
  for i:=0 to High(AWMI) do begin
    s:=GetInstancePropertyValue(AWMI,'displayName',i);
    fn:=ExpandEnvVars(GetInstancePropertyValue(AWMI,'pathToSignedProductExe',i));
    if not SameText(fn,'NULL') then begin
      idx:=ListNameExists(AList,s);
      if idx=-1 then
        AList.Add(Format('%s=%s',[s,fn]))
      else
        AList[idx]:=Format('%s=%s',[s,fn]);
    end;
  end;
end;

begin
  inherited;
  Clear;

  if WMIConnect('','','',SCNameSpace,wmiServices) then begin
    try
      if WMICommand(wmiServices,'AntiVirusProduct',wmi)>0 then
        Add(FAV,wmi);
    except
    end;
    Finalize(wmi);
    try
      if WMICommand(wmiServices,'AntiSpywareProduct',wmi)>0 then
        Add(FAS,wmi);
    except
    end;
    Finalize(wmi);
    try
      if WMICommand(wmiServices,'FirewallProduct',wmi)>0 then
        Add(FFW,wmi);
    except
    end;
    Finalize(wmi);
    WMIDisconnect(wmiServices);
  end;
  if WMIConnect('','','',SC2NameSpace,wmiServices) then begin
    try
      if WMICommand(wmiServices,'AntiVirusProduct',wmi)>0 then
        Add(FAV,wmi);
    except
    end;
    Finalize(wmi);
    try
      if WMICommand(wmiServices,'AntiSpywareProduct',wmi)>0 then
        Add(FAS,wmi);
    except
    end;
    Finalize(wmi);
    try
      if WMICommand(wmiServices,'FirewallProduct',wmi)>0 then
        Add(FFW,wmi);
    except
    end;
    Finalize(wmi);
    WMIDisconnect(wmiServices);
  end;
  SetDataAvail(True);
end;

procedure TMiTeC_Security.SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
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
        WriteStrProperty(sl,'AntiVirus',Self.AntiVirus.CommaText);
        WriteStrProperty(sl,'AntiSpyware',Self.AntiSpyware.CommaText);
        WriteStrProperty(sl,'Firewall',Self.Firewall.CommaText);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.
