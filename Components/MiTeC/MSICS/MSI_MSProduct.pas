{*******************************************************}
{       MiTeC System Information Component Suite        }
{            Microsoft Product Key Detection            }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_MSProduct;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs;

const
  StorageFolderName = 'MSProduct';

type
  TMSProduct = record
    Name: string;
    ProductID: string;
    ProductKey: string;
    RegistryPath: string;
  end;

  TMSProducts = array of TMSProduct;

  TMiTeC_MSProduct = class(TMiTeC_Component)
  private
    FData: TMSProducts;
    function GetCount: Cardinal;
    function GetProduct(AIndex: Cardinal): TMSProduct;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property ProductCount: Cardinal read GetCount;
    property Products[AIndex: Cardinal]: TMSProduct read GetProduct;
  published

  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}
     Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils;

{function GetProductName(AGUID: string): string;
const
  rk = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\';
  rv = 'DisplayName';
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rk+AGUID,False) then begin
        if ValueExists(rv) then
          Result:=ReadString(rv);
        CloseKey;
      end;
    finally
      Free;
    end;
end;}

{ TMiTeC_MSProduct }

procedure TMiTeC_MSProduct.Clear;
begin
  Finalize(FData);
end;

constructor TMiTeC_MSProduct.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TMiTeC_MSProduct.Destroy;
begin
  Finalize(FData);
  inherited;
end;

function TMiTeC_MSProduct.GetCount: Cardinal;
begin
  Result:=Length(FData);
end;

function TMiTeC_MSProduct.GetProduct(AIndex: Cardinal): TMSProduct;
begin
  try
    Result:=FData[AIndex];
  except
  end;
end;

function TMiTeC_MSProduct.LoadFromStorage;
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
        SetLength(FData,Length(FData)+1);
        with FData[High(FData)] do begin
          Name:=ReadStrProperty(sl,'Name');
          ProductID:=ReadStrProperty(sl,'ProductID');
          ProductKey:=ReadStrProperty(sl,'ProductKey');
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
  Finalize(Self.FData);
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
    if Assigned(Sub) then begin
      try
        i:=0;
        while ReadFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_MSProduct.RefreshData;
const
  rkMS = '\SOFTWARE\Microsoft\';
  rkMSWow = '\SOFTWARE\Wow6432Node\Microsoft\';
  rvDPID = 'DigitalProductID';
  rvPID = 'ProductID';
  rvPN = 'ProductName';
  rvCTE = 'ConvertToEdition';
  rvPC = 'ProductCode';
var
  reg: TRegistry;

function FindDigitalPID(AKey: string): Boolean;
var
  i: Integer;
  kl,vl: TStringList;
  s: string;
  r: TMSProduct;
  f: Boolean;
begin
  Finalize(r);
  ZeroMemory(@r,SizeOf(r));
  kl:=TStringList.Create;
  vl:=TStringList.Create;
  try
    Result:=False;
    with reg do begin
      if OpenKey(AKey,False) then begin
        GetKeyNames(kl);
        GetValueNames(vl);
        if vl.IndexOf(rvPID)>-1 then begin
          s:=StringReplace(AKey,'Registration\','',[rfIgnoreCase]);
          s:=StringReplace(s,rkMS,'',[rfIgnoreCase]);
          s:=StringReplace(s,rkMSWow,'',[rfIgnoreCase]);
          s:=ExtractFilePath(s);
          s:=Trim(StringReplace(s,'\',' ',[rfIgnoreCase,rfReplaceAll]));

          r.RegistryPath:=AKey;
          if ValueExists(rvPN) then
            r.Name:=ReadString(rvPN)
          else if ValueExists(rvCTE) then
            r.Name:=ReadString(rvCTE)
          else
            r.Name:=s;
          try
            if ValueExists(rvDPID) then
              r.ProductKey:=ReadDPID(reg,rvDPID);
          except
          end;
          if ValueExists(rvPID) then
            try
              r.ProductID:=ReadString(rvPID);
            except
            end;
          if r.ProductID<>'' then begin
            f:=False;
            s:=StringReplace(r.RegistryPath,rkMS,'',[rfIgnoreCase]);
            s:=StringReplace(s,rkMSWow,'',[rfIgnoreCase]);
            for i:=0 to High(FData) do
              if PosText(s,FData[i].RegistryPath)>0 then begin
                f:=True;
                Break;
              end;
            if not f then begin
              Setlength(FData,Length(FData)+1);
              FData[High(FData)]:=r;
            end;
          end;
          Result:=True;
        end;
        CloseKey;
        for i:=0 to kl.Count-1 do
          if not SameText(kl[i],'Windows NT') and not SameText(kl[i],'Windows') then
            Result:=FindDigitalPID(IncludeTrailingPathDelimiter(AKey)+kl[i]);
      end;
    end;
  finally
    vl.Free;
    kl.Free;
  end;
end;

function FindProductName(AKey: string): string;
var
  i: Integer;
  kl,vl: TStringList;
  pc,s: string;
begin
  Result:='';
  AKey:=ExcludeTrailingPathDelimiter(AKey);
  pc:=ExtractFilename(AKey);
  AKey:=ExtractFilePath(AKey);
  kl:=TStringList.Create;
  vl:=TStringList.Create;
  try
    with reg do begin
      if OpenKey(AKey,False) then begin
        GetKeyNames(kl);
        CloseKey;
        for i:=0 to kl.Count-1 do
          if OpenKey(AKey+kl[i],False) then begin
            GetValueNames(vl);
            if vl.IndexOf(rvPC)>-1 then begin
              s:=ReadString(rvPC);
              if SameText(pc,s) then
                if vl.IndexOf(rvPN)>-1 then begin
                  Result:=ReadString(rvPN);
                  Break;
                end;
            end;
            CloseKey;
          end;
      end;
    end;
  finally
    vl.Free;
    kl.Free;
  end;
end;

var
  {x,x64,}i: Integer;
  s: string;
begin
  inherited;
  Clear;

  if OS>=osXP then
    reg:=TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    reg:=TRegistry.Create;

  {if IsWow64 then
    x64:=1
  else
    x64:=0;
  for x:=0 to X64 do begin
    if x=0 then
      Reg:=TRegistry.Create(KEY_READ)
    else
      Reg:=TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);}
    with reg do
      try
        RootKey:=HKEY_LOCAL_MACHINE;
        FindDigitalPID(rkMS);
        FindDigitalPID(rkMSWow);

        for i:=0 to High(FData) do
          if FData[i].Name<>'' then begin
            s:=FindProductName(FData[i].RegistryPath);
            if s<>'' then
              FData[i].Name:=s;
          end;
      finally
        Free;
      end;
  //end;
  SetDataAvail(Length(FData)>0);
end;

procedure TMiTeC_MSProduct.SaveToStorage;
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
      WriteStrProperty(sl,'Name',Self.Products[AIndex].Name);
      WriteStrProperty(sl,'ProductID',Self.Products[AIndex].ProductID);
      WriteStrProperty(sl,'ProductKey',Self.Products[AIndex].ProductKey);
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
      for i:=0 to Self.ProductCount-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;


end.
