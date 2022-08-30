{*******************************************************}
{                      MiTeC                            }
{              Mapped files and streams                 }
{                                                       }
{         Copyright (c) 2006-2018 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Mappings;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes
     {$ELSE}
     Windows, SysUtils, Classes
     {$ENDIF}
     ;

type
  TMappedFile = class
  private
    FFilename: string;
    FMapping: THandle;
    FContent: PByte;
    FSize: Int64;
    procedure MapFile(const AFileName: string);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property Content: PByte read FContent;
    property Size: Int64 read FSize;
    property Filename: string read FFilename;
  end;

implementation

uses MiTeC_Windows;

function GetFileSize(const AFilename: string): int64;
var
  SRec: TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, SRec) <> 0 then
    Result:=-1
  else begin
    Int64Rec(Result).Lo:=SRec.FindData.nFileSizeLow;
    Int64Rec(Result).Hi:=SRec.FindData.nFileSizeHigh;
    FindClose(SRec);
  end;
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: string);
begin
  inherited Create;
  FFilename:='';
  FContent:=nil;
  FSize:=0;
  MapFile(AFileName);
end;

destructor TMappedFile.Destroy;
begin
  if Assigned(FContent) then
    UnmapViewOfFile(FContent);
  CloseHandle(FMapping);
  inherited;
end;

procedure TMappedFile.MapFile(const AFileName: string);
var
  FileHandle: THandle;
  p: Pointer;
begin
  if IsWow64 and Assigned(Wow64DisableWow64FsRedirection) then
    Wow64DisableWow64FsRedirection(p);
  try
    FileHandle:=FileOpen(AFileName,fmOpenRead or fmShareDenyWrite);
    if (FileHandle<>0) and (FileHandle<>INVALID_HANDLE_VALUE) then begin
      try
        //GetFileSizeEx(FileHandle,FSize);
        FSize:=GetFileSize(AFilename);
        FMapping:=CreateFileMapping(FileHandle,nil,PAGE_READONLY,0,0,nil);
      finally
        FileClose(FileHandle);
      end;
      if FMapping<>0 then begin
        FContent:=MapViewOfFile(FMapping, FILE_MAP_READ, 0, 0, 0);
        FFilename:=AFilename;
      end;
    end;
  finally
    if IsWow64 and Assigned(Wow64RevertWow64FsRedirection) then
      Wow64RevertWow64FsRedirection(p);
  end;
end;

end.
