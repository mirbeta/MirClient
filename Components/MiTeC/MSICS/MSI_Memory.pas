{*******************************************************}
{       MiTeC System Information Component Suite        }       
{                Memory Detection Part                  }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Memory;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs;

const
  StorageFoldername = 'Memory';

type
  TMiTeC_Memory = class(TMiTeC_Component)
  private
    FMaxAppAddress: int64;
    FVirtualTotal: int64;
    FPageFileFree: Int64;
    FVirtualFree: int64;
    FPhysicalFree: int64;
    FAllocGranularity: integer;
    FMinAppAddress: int64;
    FMemoryLoad: integer;
    FPhysicalTotal: int64;
    FPageFileTotal: int64;
    FPageSize: integer;
  protected
  public
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property PhysicalTotal: int64 read FPhysicalTotal stored false;
    property PhysicalFree: int64 read FPhysicalFree stored false;
    property VirtualTotal: int64 read FVirtualTotal stored false;
    property VirtualFree: int64 read FVirtualFree stored false;
    property PageFileTotal: int64 read FPageFileTotal stored false;
    property PageFileFree: int64 read FPageFileFree stored false;
    property MemoryLoad: integer read FMemoryLoad stored false;
    property AllocGranularity: integer read FAllocGranularity stored false;
    property MaxAppAddress: int64 read FMaxAppAddress stored false;
    property MinAppAddress: int64 read FMinAppAddress stored false;
    property PageSize: integer read FPageSize stored false;
  end;

implementation

uses MiTeC_Windows;

{ TMiTeC_Memory }

procedure TMiTeC_Memory.RefreshData;
var
  SI :TSystemInfo;
  MS :TMemoryStatus;
  MSEX :TMemoryStatusEx;
begin
  inherited;

  if Assigned(GlobalMemoryStatusEx_) then begin
    FillChar(MSEX,SizeOf(MSEX),0);
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    FMemoryLoad:=MSEX.dwMemoryLoad;
    FPhysicalTotal:=MSEX.ullTotalPhys;
    FPhysicalFree:=MSEX.ullAvailPhys;
    FVirtualTotal:=MSEX.ullTotalVirtual;
    FVirtualFree:=MSEX.ullAvailVirtual;
    FPageFileTotal:=MSEX.ullTotalPageFile;
    FPageFileFree:=MSEX.ullAvailPageFile;
  end else begin
    FillChar(MS,SizeOf(MS),0);
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    FMemoryLoad:=MS.dwMemoryLoad;
    FPhysicalTotal:=MS.dwTotalPhys;
    FPhysicalFree:=MS.dwAvailPhys;
    FVirtualTotal:=MS.dwTotalVirtual;
    FVirtualFree:=MS.dwAvailVirtual;
    FPageFileTotal:=MS.dwTotalPageFile;
    FPageFileFree:=MS.dwAvailPageFile;
  end;
  FillChar(SI,SizeOf(SI),0);
  GetNativeSystemInfo(SI);
  FAllocGranularity:=SI.dwAllocationGranularity;
  FMaxAppAddress:=int64(SI.lpMaximumApplicationAddress);
  FMinAppAddress:=int64(SI.lpMinimumApplicationAddress);
  FPageSize:=Cardinal(SI.dwPageSize);
  SetDataAvail(True);
end;

procedure TMiTeC_Memory.SaveToStorage;
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
        WriteIntProperty(sl,'PhysicalTotal',Self.PhysicalTotal);
        WriteIntProperty(sl,'PhysicalFree',Self.PhysicalFree);
        WriteIntProperty(sl,'PageFileTotal',Self.PageFileTotal);
        WriteIntProperty(sl,'PageFileFree',Self.PageFileFree);
        WriteIntProperty(sl,'VirtualTotal',Self.VirtualTotal);
        WriteIntProperty(sl,'VirtualFree',Self.VirtualFree);
        WriteIntProperty(sl,'MemoryLoad',Self.MemoryLoad);
        WriteIntProperty(sl,'AllocGranularity',Self.AllocGranularity);
        WriteIntProperty(sl,'MinAppAddress',Self.MinAppAddress);
        WriteIntProperty(sl,'MaxAppAddress',Self.MaxAppAddress);
        WriteIntProperty(sl,'PageSize',Self.PageSize);

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

procedure TMiTeC_Memory.Clear;
begin

end;

function TMiTeC_Memory.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
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
            Self.FMaxAppAddress:=ReadIntProperty(sl,'MaxAppAddress');
            Self.FVirtualTotal:=ReadIntProperty(sl,'VirtualTotal');
            Self.FPageFileFree:=ReadIntProperty(sl,'PageFileFree');
            Self.FVirtualFree:=ReadIntProperty(sl,'VirtualFree');
            Self.FPhysicalFree:=ReadIntProperty(sl,'PhysicalFree');
            Self.FAllocGranularity:=ReadIntProperty(sl,'AllocGranularity');
            Self.FMinAppAddress:=ReadIntProperty(sl,'MinAppAddress');
            Self.FMemoryLoad:=ReadIntProperty(sl,'MemoryLoad');
            Self.FPhysicalTotal:=ReadIntProperty(sl,'PhysicalTotal');
            Self.FPageFileTotal:=ReadIntProperty(sl,'PageFileTotal');
            Self.FPageSize:=ReadIntProperty(sl,'PageSize');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.
