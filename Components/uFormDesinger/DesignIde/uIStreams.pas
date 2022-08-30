{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uIStreams;

interface

uses Windows, ActiveX, SysUtils, Classes, AxCtrls, uToolsAPI;

type

  { IStreamModifyTime - Allows setting the file time stamp of an IStream }

  IStreamModifyTime = IOTAStreamModifyTime;

  { Allows direct access to the memory stream's memory buffer for more efficiency }

  IMemoryStream = interface
    ['{CD001314-EF15-47A9-949F-B30AA85ABF15}']
    function GetMemoryStream: TMemoryStream;
    function GetMemory: Pointer;
  end;

  { TIStreamAdapter }

  TIStreamAdapter = class(TStreamAdapter, IStreamModifyTime)
  protected
    FModifyTime: Longint;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
    function Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; override;
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; override;
    function GetModifyTime: Longint; virtual; stdcall;
    procedure SetModifyTime(Value: Longint); virtual; stdcall;
  end;

  { TIMemoryStream }

  TIMemoryStream = class(TIStreamAdapter, IMemoryStream)
  private
    function GetMemoryStream: TMemoryStream;
    function GetMemory: Pointer;
  public
    constructor Create(Stream: TMemoryStream; Ownership: TStreamOwnership = soReference);
    property MemoryStream: TMemoryStream read GetMemoryStream;
  end;

  { TIFileStream }

  TIFileStream = class(TStreamAdapter, IStreamModifyTime)
  private
    FFileName: string;
    function GetFileStream: TFileStream;
  public
    constructor Create(const FileName: string; Mode: Word);
    function Commit(grfCommitFlags: DWORD): HResult; override;
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; override;
    function GetModifyTime: Longint; stdcall;
    procedure SetModifyTime(Time: Longint); stdcall;
    property FileStream: TFileStream read GetFileStream;
  end;

  { TVirtualStream }

  TVirtualStream = class(TOleStream)
  private
    FStreamModifyTime: IStreamModifyTime;
  public
    constructor Create(AStream: IStream);
    function GetModifyTime: Longint;
    procedure SetModifyTime(Time: Longint);
  end;

  TExceptionHandler = procedure;

const
  ExceptionHandler: TExceptionHandler = nil;

implementation

{$IFDEF LINUX}
uses Libc;
{$ENDIF}

{ TIStreamAdapter }

constructor TIStreamAdapter.Create(Stream: TStream;
  Ownership: TStreamOwnership);
begin
  inherited Create(Stream, Ownership);
  FModifyTime := DateTimeToFileDate(Now);
end;

function TIStreamAdapter.Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
begin
  Result := inherited Write(pv, cb, pcbWritten);
  FModifyTime := DateTimeToFileDate(Now);
end;

function TIStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
var
  DosFileTime: Longint;
  LocalFileTime: TFileTime;
begin
  Result := inherited Stat(statstg, grfStatFlag);
  if Result <> 0 then Exit;
  DosFileTime := GetModifyTime;
  DosDateTimeToFileTime(LongRec(DosFileTime).Hi, LongRec(DosFileTime).Lo, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, statstg.mtime);
end;

function TIStreamAdapter.GetModifyTime: Longint;
begin
  Result := FModifyTime;
end;

procedure TIStreamAdapter.SetModifyTime(Value: Longint);
begin
  FModifyTime := Value;
end;

{ TIMemoryStream }

constructor TIMemoryStream.Create(Stream: TMemoryStream;
  Ownership: TStreamOwnership);
begin
  if Stream = nil then
  begin
    Ownership := soOwned;
    Stream := TMemoryStream.Create;
  end;
  inherited Create(Stream, Ownership);
end;

function TIMemoryStream.GetMemory: Pointer;
begin
  Result := TMemoryStream(Stream).Memory;
end;

function TIMemoryStream.GetMemoryStream: TMemoryStream;
begin
  Result := TMemoryStream(Stream);
end;

{ TIFileStream }

constructor TIFileStream.Create(const FileName: string; Mode: Word);
begin
{$IFDEF LINUX}
  if Mode = fmCreate then
    unlink(PChar(FileName));
{$ENDIF}
  FFileName := FileName;
  inherited Create(TFileStream.Create(FileName, Mode), soOwned);
end;

function TIFileStream.GetFileStream: TFileStream;
begin
  Result := TFileStream(Stream);
end;

function TIFileStream.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
var
  FileNameW: PWideChar;
  NumChars: Integer;
begin
  if (@statstg <> nil) then
    statstg.pwcsName := nil;
  Result := inherited Stat(statstg, grfStatFlag);
  if Result <> 0 then Exit;
  if @statstg <> nil then
  begin
    if (grfStatFlag = STATFLAG_DEFAULT) and (FFileName <> '') then
    begin
{$IFNDEF UNICODE}
      NumChars := MultibyteToWideChar(CP_ACP, 0, PChar(FFileName), Length(FFileName), nil, 0);
      FileNameW := CoTaskMemAlloc((NumChars + 1) * SizeOf(WideChar));
      MultibyteToWideChar(CP_ACP, 0, PChar(FFileName), Length(FFileName), FileNameW, NumChars);
      statstg.pwcsName := FileNameW;
{$ELSE}
      NumChars := Length(FFileName);
      FileNameW := CoTaskMemAlloc((NumChars + 1) * SizeOf(WideChar));
      statstg.pwcsName := StrPLCopy(FileNameW, FFileName, NumChars);
{$ENDIF}
    end;
    GetFileTime(FileStream.Handle, @statstg.ctime, @statstg.atime, @statstg.mtime);
  end;
end;

function TIFileStream.GetModifyTime: Longint;
var
  StatStg: TStatStg;
  LocalTime: TFileTime;
begin
  if Stat(StatStg, STATFLAG_NONAME) = S_OK then
  begin
    FileTimeToLocalFileTime(StatStg.mtime, LocalTime);
    if not FileTimeToDosDateTime(LocalTime, LongRec(Result).Hi, LongRec(Result).Lo) then
      Result := -1;
  end else
    Result := FileGetDate(FileStream.Handle);
end;

procedure TIFileStream.SetModifyTime(Time: Longint);
begin
{$IFDEF MSWINDOWS}
  FileSetDate(FileStream.Handle, Time);
{$ELSE}
{$ENDIF}
end;

function TIFileStream.Commit(grfCommitFlags: DWORD): HResult;
begin
  FlushFileBuffers(FileStream.Handle);
  Result := inherited Commit(grfCommitFlags);
end;

{ TVirtualStream }

constructor TVirtualStream.Create(AStream: IStream);
begin
  inherited Create(AStream);
  if AStream.QueryInterface(IStreamModifyTime, FStreamModifyTime) <> 0 then
    FStreamModifyTime := nil;
end;

function TVirtualStream.GetModifyTime: Longint;
begin
  if FStreamModifyTime <> nil then
    Result := FStreamModifyTime.GetModifyTime
  else
    Result := 0;
end;

procedure TVirtualStream.SetModifyTime(Time: Longint);
begin
  if FStreamModifyTime <> nil then
    FStreamModifyTime.SetModifyTime(Time);
end;

end.

