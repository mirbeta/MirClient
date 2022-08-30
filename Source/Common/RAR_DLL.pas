//  written by Philippe Wechsler 2008 - 2009
//                                                                                         
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  please see license.txt and documentation.pdf!
//
//  changes in 2.0
//   - major changes in code, 75% of the code is rewriten
//   - cleaner code
//   - AutoNextVolumeOnList
//   - ForceTestToEnd
//   - TOnRARFileProcessed
//   - TOnRARVolumeChanged
//   - support for wildchars (extract files)
//   - better unicode support and big files
//   - demo updated
//   - fixed some minor bugs
//
//  changes in 1.2 stable
//   - support for delphi 2009
//   - support for unicode filenames (see TRARFileItem.FileNameW)
//   - dll name + path is custom
//   - fixed a memory leak (thanks to Claes Enskär)
//   - some small improvements in the demo
//
//  changes in 1.1 stable
//   - fixed problem with mySelf pointer - you can use now multiple TRAR instances
//   - "SFX" in archive informations
//   - code better commented
//   - bugfixing in reading multivolumes
//
//  known bugs:
//   - Unicode support is only for d2009, but with older versions you can open, extract and
//     test archives.
//   - no support for Unicode archive names because unrar.dll does not support Unicode
//     filenames for multivolume parts! (workaround in 2.1)
//   - to extract only custom files you have to set the overall progress (ArchiveBytesTotal)
//     yourself. This is the sum of the uncompressed size of the selected files.
//     this issue will be fixed in 2.1

unit RAR_DLL;

interface

uses
  Windows, SysUtils;

const
  RAR_METHOD_STORE = 48;
  RAR_METHOD_FASTEST = 49;
  RAR_METHOD_FAST = 50;
  RAR_METHOD_NORMAL = 51;
  RAR_METHOD_GOOD = 52;
  RAR_METHOD_BEST = 53;

  RAR_SUCCESS = 0;
  ERAR_COMMENTS_EXISTS = 1;
  ERAR_NO_COMMENTS = 0;
  ERAR_END_ARCHIVE = 10;
  ERAR_NO_MEMORY = 11;
  ERAR_BAD_DATA = 12;
  ERAR_BAD_ARCHIVE = 13;
  ERAR_UNKNOWN_FORMAT = 14;
  ERAR_EOPEN = 15;
  ERAR_ECREATE = 16;
  ERAR_ECLOSE = 17;
  ERAR_EREAD = 18;
  ERAR_EWRITE = 19;
  ERAR_SMALL_BUF = 20;
  ERAR_UNKNOWN = 21;
  ERAR_DLL_LOAD_ERROR = 99;

  RAR_OM_LIST = 0;
  RAR_OM_EXTRACT = 1;
  RAR_OM_LIST_INCSPLIT = 2;
  RAR_SKIP = 0;
  RAR_TEST = 1;
  RAR_EXTRACT = 2;
  RAR_VOL_ASK = 0;
  RAR_VOL_NOTIFY = 1;
  RAR_DLL_VERSION = 3;

  UCM_CHANGEVOLUME = 0;
  UCM_PROCESSDATA = 1;
  UCM_NEEDPASSWORD = 2;

  MAX_RAR_COMMENTSIZE = 65536;
  MIN_RAR_VERSION = 4;

type
  TRARProcessDataProc = function(Addr: PByte; Size: integer): integer; stdcall;
  TRARChangeVolProc = function(ArcName: PAnsiChar; Mode: integer): integer; stdcall;
  TRARUnRarCallBack = function(msg: Cardinal; UserData, P1, P2: longint): integer; stdcall;

  TRARArchiveData = record
    ArcName: PAnsiChar;
    OpenMode: cardinal;
    OpenResult: cardinal;
    CmtBuf: PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize: cardinal;
    CmtState: cardinal;
  end;
  PRARArchiveData = ^TRARArchiveData;

  //for UniCode FileNames and 64-Bit Sizes
  TRARArchiveDataEx = record
    ArcName: PAnsiChar;
    ArcNameW: PWideChar;
    OpenMode: cardinal;
    OpenResult: cardinal;
    CmtBuf: PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize: cardinal;
    CmtState: cardinal;
    Flags: cardinal;
    Reserved: array[1..32] of cardinal;
  end;
  PRARArchiveDataEx = ^TRARArchiveDataEx;

  TRARHeaderData = record
    ArcName: array[0..259] of AnsiChar;
    FileName: array[0..259] of AnsiChar;
    Flags: cardinal;
    PackSize: cardinal;
    UnpSize: cardinal;
    HostOS: cardinal;
    FileCRC: cardinal;
    FileTime: cardinal;
    UnpVer: cardinal;
    Method: cardinal;
    FileAttr: cardinal;
    CmtBuf: PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize: cardinal;
    CmtState: cardinal;
  end;
  PRARHeaderData = ^TRARHeaderData;

  //for UniCode FileNames and 64-Bit Sizes
  TRARHeaderDataEx = record
    ArcName: array[0..1023] of AnsiChar;
    ArcNameW: array[0..1023] of WideChar;
    FileName: array[0..1023] of AnsiChar;
    FileNameW: array[0..1023] of WideChar;
    Flags: cardinal;
    PackSize: cardinal;
    PackSizeHigh: cardinal;
    UnpSize: cardinal;
    UnpSizeHigh: cardinal;
    HostOS: cardinal;
    FileCRC: cardinal;
    FileTime: cardinal;
    UnpVer: cardinal;
    Method: cardinal;
    FileAttr: cardinal;
    CmtBuf: PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize: cardinal;
    CmtState: cardinal;
    Reserved: array[1..1024] of cardinal;
  end;
  PRARHeaderDataEx = ^TRARHeaderDataEx;

  TRAROpenArchive = function(ArchiveData: PRARArchiveData): THandle; stdcall;
  TRAROpenArchiveEx = function(ArchiveData: PRARArchiveDataEx): THandle; stdcall;
  TRARCloseArchive = function(hArcData: THandle): integer; stdcall;
  TRARReadHeader = function(hArcData: THandle; HeaderData: PRARHeaderData): Integer; stdcall;
  TRARReadHeaderEx = function(hArcData: THandle; HeaderData: PRARHeaderDataEx): Integer; stdcall;
  TRARProcessFile = function(hArcData: THandle; Operation: Integer; DestPath, DestName: PAnsiChar): Integer; stdcall;
  TRARProcessFileW = function(hArcData: THandle; Operation: Integer; DestPath, DestName: PWideChar): Integer; stdcall;
  TRARSetCallback = procedure(hArcData: THandle; Callback: TRARUnRarCallback; UserData: longint); stdcall;
  TRARSetChangeVolProc = procedure(hArcData: THandle; ChangeVolProc: TRARChangeVolProc); stdcall;
  TRARSetProcessDataProc = procedure(hArcData: THandle; ProcessDataProc: TRARProcessDataProc); stdcall;
  TRARSetPassword = procedure(hArcData: THandle; Password: PAnsiChar); stdcall;
  TRARGetDllVersion = function: Integer; stdcall;

  function GetFileModifyDate(const FileName: String): TDateTime;
  function GetFileSize(const FileName: String): Int64;
  function isSFX(const FileName: String): boolean;
  function CardToInt64(const high,low: Cardinal): int64;

implementation

function GetFileModifyDate(const FileName: String): TDateTime;
var
  h: THandle;
  Struct: TOFSTRUCT;
  lastwrite: Integer;
begin
  Result := 0;
  h := OpenFile(PAnsiChar(FileName), Struct, OF_SHARE_DENY_NONE);
  try
    if h <> HFILE_ERROR then
    begin
      lastwrite := FileGetDate(h);
      Result := FileDateToDateTime(lastwrite);
    end;
  finally
    CloseHandle(h);
  end;
end;

function GetFileSize(const FileName: String): Int64;
var
  lFindData: TWin32FindData;
  lHandle : Cardinal;
begin
  lHandle := FindFirstFile(PChar(FileName), lFindData);
  if (lHandle <> INVALID_HANDLE_VALUE) then 
  begin 
    Result := lFindData.nFileSizeLow;
    PCardinal(Cardinal(@Result) + SizeOf(Cardinal))^ := lFindData.nFileSizeHigh;
    windows.FindClose(lHandle); 
  end 
  else 
    Result := 0;
end;

function isSFX(const FileName: String): boolean;
var
  BinaryType: DWord;
begin
  if GetBinaryTypeA(PAnsiChar(FileName), BinaryType) then begin
    if (BinaryType = SCS_32BIT_BINARY) or
      (BinaryType = SCS_DOS_BINARY) or
      (BinaryType = SCS_WOW_BINARY) or
      (BinaryType = SCS_PIF_BINARY) or
      (BinaryType = SCS_POSIX_BINARY) or
      (BinaryType = SCS_OS216_BINARY)
      then
      Result := True
    else
      Result := False;
  end else
    Result := False;
end;

function CardToInt64(const high,low: Cardinal):int64;
begin
  Result := int64(high) shl 32 or low;
end;

end.
