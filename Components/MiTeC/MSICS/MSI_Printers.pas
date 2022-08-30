{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Printer Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Printers;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.WinSpool,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, WinSpool, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs;

const
  StorageFolderName = 'Printers';

type
  TPrinterInfo = record
    PrinterName,
    ShareName,
    PortName,
    DriverName,
    Comment,
    Location,
    DriverPath,
    DriverVersion,
    Monitor: string;
  end;

  TPrinters = array of TPrinterInfo;

  TMiTeC_Printers = class(TMiTeC_Component)
  private
    FPrinterInfo: TPrinters;
    FPrinterCount: Cardinal;
    FDefaultPrinter: string;
    function GetComment(Index: Cardinal): string;
    function GetDriver(Index: Cardinal): string;
    function GetLocation(Index: Cardinal): string;
    function GetPort(Index: Cardinal): string;
    function GetPrinterName(Index: Cardinal): string;
    function GetShare(Index: Cardinal): string;
    function GetDefaultPrinter: string;
    function GetDriverPath(Index: Cardinal): string;
    function GetMonitor(Index: Cardinal): string;
    function GetDriverVersion(Index: Cardinal): string;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property PrinterName[Index: Cardinal]: string read GetPrinterName;
    property Driver[Index: Cardinal]: string read GetDriver;
    property Port[Index: Cardinal]: string read GetPort;
    property Location[Index: Cardinal]: string read GetLocation;
    property Comment[Index: Cardinal]: string read GetComment;
    property Share[Index: Cardinal]: string read GetShare;
    property DriverPath[Index: Cardinal]: string read GetDriverPath;
    property DriverVersion[Index: Cardinal]: string read GetDriverVersion;
    property Monitor[Index: Cardinal]: string read GetMonitor;
  published
    property PrinterCount: Cardinal read FPrinterCount;
    property DefaultPrinter: string read FDefaultPrinter;
  end;

implementation

uses MiTeC_Routines;

{ TMiTeC_Printers }

function FetchStr(var Str: PChar): PChar;
var
  P: PChar;
begin
  Result:=Str;
  if Str=nil then
    Exit;
  P:=Str;
  while P^=' ' do
    Inc(P);
  Result:=P;
  while (P^<>#0) and (P^<>',') do
    Inc(P);
  if P^= ',' then begin
    P^:=#0;
    Inc(P);
  end;
  Str:=P;
end;

procedure TMiTeC_Printers.Clear;
begin
  FPrinterCount:=0;
  Finalize(FPrinterInfo);
end;

destructor TMiTeC_Printers.Destroy;
begin
  Finalize(FPrinterInfo);
  inherited;
end;

function TMiTeC_Printers.GetComment(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].Comment);
end;

function TMiTeC_Printers.GetDefaultPrinter: string;
var
  ByteCnt,StructCnt: Cardinal;
  DefaultPrinter: array[0..1023] of Char;
  Cur,Device: PChar;
  PrinterInfo: ^PRINTER_INFO_2;
begin
  Result:='';
  ByteCnt:=0;
  StructCnt:=0;
  if not EnumPrinters(PRINTER_ENUM_DEFAULT,nil,2,nil,0,{$IFDEF FPC}@{$ENDIF}ByteCnt,{$IFDEF FPC}@{$ENDIF}StructCnt) and (GetLastError<>ERROR_INSUFFICIENT_BUFFER) then
    if GetLastError=ERROR_INVALID_NAME then
      Exit;
  PrinterInfo:=AllocMem(ByteCnt);
  try
    EnumPrinters(PRINTER_ENUM_DEFAULT, nil,2,{$IFDEF FPC}PByte{$ENDIF}(PrinterInfo),ByteCnt,{$IFDEF FPC}@{$ENDIF}ByteCnt,{$IFDEF FPC}@{$ENDIF}StructCnt);
    if StructCnt>0 then
      Device:=PrinterInfo.pPrinterName
    else begin
      GetProfileString('windows','device','',DefaultPrinter,SizeOf(DefaultPrinter)-1);
      Cur:=DefaultPrinter;
      Device:=FetchStr(Cur);
    end;
  finally
    FreeMem(PrinterInfo);
  end;
  Result:=string(Device);
end;

function TMiTeC_Printers.GetDriver(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].DriverName);
end;

function TMiTeC_Printers.GetDriverPath(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].DriverPath);
end;

function TMiTeC_Printers.GetDriverVersion(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].DriverVersion);
end;

function TMiTeC_Printers.GetLocation(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].Location);
end;

function TMiTeC_Printers.GetMonitor(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].Monitor);
end;

function TMiTeC_Printers.GetPort(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].PortName);
end;

function TMiTeC_Printers.GetPrinterName(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].PrinterName);
end;

function TMiTeC_Printers.GetShare(Index: Cardinal): string;
begin
  Result:=string(FPrinterInfo[index].ShareName);
end;

function TMiTeC_Printers.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AIndex: Cardinal): boolean;
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
            SetLength(Self.FPrinterInfo,Length(Self.FPrinterInfo)+1);
            with Self.FPrinterInfo[High(Self.FPrinterInfo)] do begin
              PrinterName:=ReadStrProperty(sl,'PrinterName');
              DriverName:=ReadStrProperty(sl,'Driver');
              Comment:=ReadStrProperty(sl,'Comment');
              Location:=ReadStrProperty(sl,'Location');
              PortName:=ReadStrProperty(sl,'Port');
              ShareName:=ReadStrProperty(sl,'Share');
              DriverPath:=ReadStrProperty(sl,'DriverPath');
              DriverVersion:=ReadStrProperty(sl,'DriverVersion');
              Monitor:=ReadStrProperty(sl,'Monitor');
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
  strm: TStorageStream;
  sl: TStringList;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
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
            Self.FDefaultPrinter:=ReadStrProperty(sl,'DefaultPrinter');
            Self.FPrinterCount:=ReadIntProperty(sl,'PrinterCount');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      i:=0;
      while ReadFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
    finally
      if Sub<>nil then
       Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Printers.RefreshData;
var
  Count,Flags,NumInfo,i,n: Cardinal;
  FPrinterBuf :array of PRINTER_INFO_2;
  ph: THandle;
  di: PDriverInfo3;
begin
  inherited;
  Clear;
  Flags:=(PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL);
  try
    EnumPrinters(Flags,nil,2,nil,0,{$IFDEF FPC}@{$ENDIF}Count,{$IFDEF FPC}@{$ENDIF}NumInfo);
    if Count=0 then
      Exit;
    try
      SetLength(FPrinterBuf,Count div SizeOf(FPrinterBuf[0])+1);
      if not EnumPrinters(Flags,nil,2,PByte(FPrinterBuf),Length(FPrinterBuf)*SizeOf(FPrinterBuf[0]),{$IFDEF FPC}@{$ENDIF}Count,{$IFDEF FPC}@{$ENDIF}NumInfo) then
        Exit;
      FPrinterCount:=NumInfo;
      SetLength(FPrinterInfo,NumInfo);
      di:=AllocMem(SizeOf(TDriverInfo3));
      try
        for i:=0 to Length(FPrinterInfo)-1 do
          with FPrinterInfo[i] do begin
            PrinterName:=string(FPrinterBuf[i].pPrinterName);
            ShareName:=string(FPrinterBuf[i].pShareName);
            PortName:=string(FPrinterBuf[i].pPortName);
            DriverName:=string(FPrinterBuf[i].pDriverName);
            Location:=string(FPrinterBuf[i].pLocation);
            Comment:=string(FPrinterBuf[i].pComment);

            if OpenPrinter(FPrinterBuf[i].pPrinterName,{$IFDEF FPC}@{$ENDIF}ph,nil) then begin
              GetPrinterDriver(ph,nil,3,nil,0,{$IFDEF FPC}@{$ENDIF}n);
              ReallocMem(di,n);
              if GetPrinterDriver(ph,nil,3,{$IFDEF FPC}PByte{$ENDIF}(di),n,{$IFDEF FPC}@{$ENDIF}n){$IFDEF FPC}>0{$ENDIF} then begin
                DriverPath:=string(di^.pDriverPath);
                DriverVersion:=GetFileVersion(DriverPath);
                Monitor:=string(di^.pMonitorName);
              end; {else
                DriverPath:=SysErrorMessage(GetLastError);}
              ClosePrinter(ph);
            end;
          end;
      finally
        ReallocMem(di,0);
      end;
    except
    end;
  except
  end;
  FDefaultPrinter:=GetDefaultPrinter;
  SetDataAvail(True);
end;

procedure TMiTeC_Printers.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'PrinterName',Self.PrinterName[AIndex]);
      WriteStrProperty(sl,'Driver',Self.Driver[AIndex]);
      WriteStrProperty(sl,'Port',Self.Port[AIndex]);
      WriteStrProperty(sl,'Location',Self.Location[AIndex]);
      WriteStrProperty(sl,'Comment',Self.Comment[AIndex]);
      WriteStrProperty(sl,'Share',Self.Share[AIndex]);
      WriteStrProperty(sl,'DriverPath',Self.DriverPath[AIndex]);
      WriteStrProperty(sl,'DriverVersion',Self.DriverVersion[AIndex]);
      WriteStrProperty(sl,'Monitor',Self.Monitor[AIndex]);
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
        WriteStrProperty(sl,'DefaultPrinter',Self.DefaultPrinter);
        WriteIntProperty(sl,'PrinterCount',Self.PrinterCount);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
      for i:=0 to Self.PrinterCount-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;


end.
