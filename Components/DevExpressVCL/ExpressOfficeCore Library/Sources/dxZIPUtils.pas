{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxZIPUtils;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, ZLIB, RTLConsts, dxCore, dxCoreClasses, cxClasses, AnsiStrings, Generics.Defaults, Generics.Collections;

const
  dxUnixPathDelim = AnsiChar('/');

type
  TdxZIPCustomStream = class;

  EdxZIPUnsupportedCompressionMethod = class(EdxException);

  EdxPackedStreamReader = class(Exception);
  EdxPackedStreamWriter = class(Exception);

  { TdxZIPFileEntry }

  TdxZIPFileEntry = packed record
  private
    function GetDataPosition: Integer;
    function GetSizeForStream: Integer;
  public
    Sign: Integer;
    VersionToExtract: Word;
    Flag: Word;
    Method: Word;
    DateTime: Cardinal;
    CRC32: Cardinal;
    PackedSize: Integer;
    OriginalSize: Integer;
    NameLength: Word;
    ExtraLength: Word;
    // Extra
    IsDirectory: Boolean;
    Name: AnsiString;
    RelativeOffset: Integer;

    constructor Create(const AName: AnsiString; ADateTime: Cardinal; ACRC32, APackedSize, AOriginalSize: Integer);
    class function CheckName(const AName: AnsiString): AnsiString; static;
    procedure LoadFromStream(AStream: TStream; const AOffset: Integer);
    procedure WriteToStream(AStream: TStream);
  end;

  { TdxZIPDataDescriptor }

  TdxZIPDataDescriptor = packed record
    CRC32: Integer;
    PackedSize: Integer;
    OriginalSize: Integer;
  end;

  { TdxZIPCentralDirEntry }

  TdxZIPCentralDirEntry = packed record
    Sign: Integer;
    Version: Word;
    VersionToExtract: Word;
    Flag: Word;
    Method: Word;
    DateTime: Cardinal;
    CRC32: Integer;
    PackedSize: Integer;
    OriginalSize: Integer;
    NameLength: Word;
    ExtraLength: Word;
    CommentLength: Word;
    DiskStart: Word;
    IntAttributes: Word;
    ExtAttributes: Integer;
    RelativeOffset: Integer;
  end;

  { TdxZIPEndOfDir }

  TdxZIPEndOfDir = packed record
    Sign: Integer;
    DiskNumber: Word;
    NumberOfDiskStart: Word;
    DirStart: Word;
    DirEntryCount: Word;
    DirSize: Integer;
    DirOffset: Integer;
    CommentLength: Word;
  end;

  { TdxCompressedStream }

  TdxCompressedStream = class(TStream)
  strict private
    FCompressionStream: TZCompressionStream;
    FCRC32: LongWord;
    FUncompressedSize: Integer;
  protected
    FBuffer: TMemoryStream;
  public
    constructor Create(ACompressionLevel: TCompressionLevel = clDefault; ABufferCapacity: Cardinal = 0);
    destructor Destroy; override;
    procedure Flush;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    property CRC32: LongWord read FCRC32;
    property UncompressedSize: Integer read FUncompressedSize;
  end;

  { TdxZIPCustomStream }

  TdxZIPCustomStream = class
  strict private
    FAutoFreeStream: Boolean;
    FFiles: TList<TdxZIPFileEntry>;
    FStream: TStream;
  protected
    property Files: TList<TdxZIPFileEntry> read FFiles;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream; AAutoFreeStream: Boolean = False); overload; virtual;
    constructor Create(const AFileName: string); overload; virtual;
    destructor Destroy; override;
  end;

  { TdxZIPStreamReader }

  TdxZIPStreamReaderUnpackMethod = procedure (APackedSize: Integer; ATargetStream: TStream) of object;

  TdxZIPStreamReader = class(TdxZIPCustomStream)
  strict private
    FUnpackMethods: TDictionary<Word, TdxZIPStreamReaderUnpackMethod>;

    function Find(AFileName: AnsiString; var AEntry: TdxZIPFileEntry): Boolean;
    procedure ReadCentralDirectory;
  protected
    procedure UnpackDeflate(APackedSize: Integer; ATargetStream: TStream);
    procedure UnpackNone(APackedSize: Integer; ATargetStream: TStream);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function Exists(const AFileName: AnsiString): Boolean;
    procedure Extract(const AEntry: TdxZIPFileEntry; ATargetStream: TStream); overload;
    function Extract(const AFileName: AnsiString; ATargetStream: TStream): Boolean; overload;
    //
    property Files;
  end;

  { TdxZIPStreamWriter }

  TdxZIPStreamWriter = class(TdxZIPCustomStream)
  strict private
    procedure AddCompressedFile(const AName: AnsiString; AData: PByte; ACompressedSize, AUncompressedSize, ACRC32: Integer);
    procedure AddFileHeader(const AName: AnsiString; ACRC32, APackedSize, ASize: Integer);
    procedure WriteCentralDirectory;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
    procedure AddDirectory(const AName: AnsiString);
    procedure AddFile(const AFileName: AnsiString; AData: PByte; ADataSize: Integer); overload;
    procedure AddFile(const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean = False); overload;
  end;

  { TdxZIPPathHelper }

  TdxZIPPathHelper = class
  public
    class function AbsoluteFileName(const ACurrentPath, AFileName: AnsiString): AnsiString; static;
    class function DecodePath(const APath: AnsiString): AnsiString; static;
    class function EncodePath(const APath: AnsiString): AnsiString; static;
    class function ExcludeRootPathDelimiter(const APath: AnsiString): AnsiString; static;
    class function ExcludeTrailingPathDelimiter(const APath: AnsiString): AnsiString; static;
    class function ExpandFileName(const AFileName: AnsiString): AnsiString; static;
    class function ExtractFilePath(const AFileName: AnsiString): AnsiString; static;
    class function IsAbsoluteFileName(const AFileName: AnsiString): Boolean; static;
    class function RelativePath(const ACurrentPath, AFileName: AnsiString): AnsiString; static;
  end;

  { TdxInflateHelper }

  TdxInflateHelper = class
    class procedure Decompress(APackedStream: TStream; APackedSize: Integer; ATargetStream: TStream); static;
  end;

implementation

uses
  dxHash, dxHashUtils, Math;

const
  ZLibHeader: Word = $9C78;
  ZLibHeaderSize = 2;
  ZLibFooterSize = 4;

  FileHeader    = $04034B50;
  DirFileHeader = $02014B50;
  EndOfDir      = $06054B50;

type
  TMemoryStreamAccess = class(TMemoryStream);

procedure CheckRead(AValue: Boolean; const AMessage: string = '');
begin
  if not AValue then
    raise EdxPackedStreamReader.Create(AMessage);
end;

{ TdxZIPFileEntry }

constructor TdxZIPFileEntry.Create(const AName: AnsiString;
  ADateTime: Cardinal; ACRC32, APackedSize, AOriginalSize: Integer);
begin
  Sign := FileHeader;
  VersionToExtract := 20;
  Flag := 0;
  Method := 8;
  DateTime := ADateTime;
  CRC32 := ACRC32;
  PackedSize := APackedSize;
  OriginalSize := AOriginalSize;
  ExtraLength := 0;
  IsDirectory := False;
  Name := CheckName(AName);
  NameLength := Length(Name);
end;

class function TdxZIPFileEntry.CheckName(const AName: AnsiString): AnsiString;
begin
  Result := TdxZIPPathHelper.ExcludeRootPathDelimiter(TdxZIPPathHelper.EncodePath(AName));
end;

procedure TdxZIPFileEntry.LoadFromStream(AStream: TStream; const AOffset: Integer);
begin
  AStream.Position := AOffset;
  AStream.ReadBuffer(Self, GetSizeForStream);
  SetLength(Name, NameLength);
  AStream.ReadBuffer(Name[1], NameLength);
  RelativeOffset := AOffset;
  Name := CheckName(Name);
end;

procedure TdxZIPFileEntry.WriteToStream(AStream: TStream);
begin
  AStream.WriteBuffer(Self, GetSizeForStream);
  AStream.WriteBuffer(Name[1], NameLength);
end;

function TdxZIPFileEntry.GetDataPosition: Integer;
begin
  Result := RelativeOffset + GetSizeForStream + NameLength + ExtraLength;
end;

function TdxZIPFileEntry.GetSizeForStream: Integer;
begin
  Result := 30;
end;

{ TdxCompressedStream }

constructor TdxCompressedStream.Create(ACompressionLevel: TCompressionLevel = clDefault; ABufferCapacity: Cardinal = 0);
begin
  inherited Create;
  FCRC32 := $FFFFFFFF;
  FBuffer := TMemoryStream.Create;
  if ABufferCapacity > 0 then
    TMemoryStreamAccess(FBuffer).Capacity := ABufferCapacity;
  FCompressionStream := TZCompressionStream.Create(ACompressionLevel, FBuffer);
end;

destructor TdxCompressedStream.Destroy;
begin
  FreeAndNil(FCompressionStream);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TdxCompressedStream.Flush;
begin
  FreeAndNil(FCompressionStream);
  FCRC32 := not FCRC32;
end;

function TdxCompressedStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ENotSupportedException.Create(ClassName);
end;

function TdxCompressedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise ENotSupportedException.Create(ClassName);
end;

function TdxCompressedStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  Inc(FUncompressedSize, ACount);
  FCRC32 := dxCRC32(PByte(@ABuffer), ACount, FCRC32);
  Result := FCompressionStream.Write(ABuffer, ACount);
end;

{ TdxZIPCustomStream }

constructor TdxZIPCustomStream.Create(const AFileName: string);
begin
  Create(TFileStream.Create(AFileName, fmOpenReadWrite), True);
end;

constructor TdxZIPCustomStream.Create(AStream: TStream; AAutoFreeStream: Boolean = False);
begin
  inherited Create;
  FStream := AStream;
  FAutoFreeStream := AAutoFreeStream;
  FFiles := TList<TdxZIPFileEntry>.Create;
end;

destructor TdxZIPCustomStream.Destroy;
begin
  if FAutoFreeStream then
    FreeAndNil(FStream);
  FreeAndNil(FFiles);
  inherited Destroy;
end;

{ TdxZIPStreamReader }

destructor TdxZIPStreamReader.Destroy;
begin
  FreeAndNil(FUnpackMethods);
  inherited Destroy;
end;

procedure TdxZIPStreamReader.AfterConstruction;
begin
  inherited AfterConstruction;
  FUnpackMethods := TDictionary<Word, TdxZIPStreamReaderUnpackMethod>.Create;
  FUnpackMethods.Add(0, UnpackNone);
  FUnpackMethods.Add(8, UnpackDeflate);
  ReadCentralDirectory;
end;

function TdxZIPStreamReader.Exists(const AFileName: AnsiString): Boolean;
var
  AEntry: TdxZIPFileEntry;
begin
  Result := Find(AFileName, AEntry)
end;

procedure TdxZIPStreamReader.Extract(const AEntry: TdxZIPFileEntry; ATargetStream: TStream);
var
  ATargetPosition: Int64;
  AUnpackMethod: TdxZIPStreamReaderUnpackMethod;
begin
  if not FUnpackMethods.TryGetValue(AEntry.Method, AUnpackMethod) then
    raise EdxZIPUnsupportedCompressionMethod.Create('');

  ATargetPosition := ATargetStream.Position;
  Stream.Position := AEntry.GetDataPosition;
  AUnpackMethod(AEntry.PackedSize, ATargetStream);

  if AEntry.CRC32 <> 0 then
  begin
    CheckRead((AEntry.CRC32 = dxCRC32(ATargetStream, ATargetPosition, ATargetStream.Position - ATargetPosition)),
      dxAnsiStringToString(AEntry.Name) + ' is corrupted.');
  end;
end;

function TdxZIPStreamReader.Extract(const AFileName: AnsiString; ATargetStream: TStream): Boolean;
var
  AEntry: TdxZIPFileEntry;
begin
  Result := Find(AFileName, AEntry);
  if Result then
    Extract(AEntry, ATargetStream)
end;

function TdxZIPStreamReader.Find(AFileName: AnsiString; var AEntry: TdxZIPFileEntry): Boolean;
var
  I: Integer;
begin
  Result := False;
  AFileName := TdxZIPPathHelper.ExcludeRootPathDelimiter(TdxZIPPathHelper.ExpandFileName(AFileName));
  for I := 0 to Files.Count - 1 do
    if SameText(Files[I].Name, AFileName) then
    begin
      AEntry := Files[I];
      Exit(True);
    end;
end;

procedure TdxZIPStreamReader.ReadCentralDirectory;
var
  ADir: TdxZIPCentralDirEntry;
  ADirs: TList<TdxZIPCentralDirEntry>;
  AEndOfDir: TdxZIPEndOfDir;
  AFileEntry: TdxZIPFileEntry;
  I: Integer;
begin
  Stream.Position := 0;
  Stream.ReadBuffer(ADir, SizeOf(ADir));
  if ADir.Sign = FileHeader then
  begin
    ADirs := TList<TdxZIPCentralDirEntry>.Create;
    try
      Stream.Position := Stream.Size;
      repeat
        Stream.Seek(-SizeOf(TdxZIPEndOfDir), soCurrent);
        Stream.ReadBuffer(AEndOfDir, SizeOf(AEndOfDir));
        Stream.Seek(-1, soCurrent);
      until (AEndOfDir.Sign = EndOfDir) or (Stream.Position < SizeOf(AEndOfDir));
      CheckRead(AEndOfDir.Sign = EndOfDir);

      Stream.Position := AEndOfDir.DirOffset;
      ADirs.Capacity := AEndOfDir.DirEntryCount;
      for I := 0 to AEndOfDir.DirEntryCount - 1 do
      begin
        Stream.ReadBuffer(ADir, SizeOf(ADir));
        CheckRead(ADir.Sign = DirFileHeader);
        ADirs.Add(ADir);
        Stream.Seek(ADir.NameLength + ADir.ExtraLength + ADir.CommentLength, soCurrent);
      end;

      Files.Capacity := ADirs.Count;
      for I := 0 to ADirs.Count - 1 do
      begin
        ADir := ADirs[I];
        AFileEntry.LoadFromStream(Stream, ADir.RelativeOffset);
        AFileEntry.IsDirectory := ADir.ExtAttributes and faDirectory = faDirectory;
        if AFileEntry.OriginalSize = 0 then
          AFileEntry.OriginalSize := ADir.OriginalSize;
        if AFileEntry.PackedSize = 0 then
          AFileEntry.PackedSize := ADir.PackedSize;
        CheckRead(FileHeader = AFileEntry.Sign);
        CheckRead(ADir.OriginalSize = AFileEntry.OriginalSize);
        CheckRead(ADir.PackedSize = AFileEntry.PackedSize);
        Files.Add(AFileEntry);
      end;
    finally
      ADirs.Free;
    end;
  end;
end;

procedure TdxZIPStreamReader.UnpackDeflate(APackedSize: Integer; ATargetStream: TStream);
begin
  TdxInflateHelper.Decompress(Stream, APackedSize, ATargetStream);
end;

procedure TdxZIPStreamReader.UnpackNone(APackedSize: Integer; ATargetStream: TStream);
begin
  if APackedSize > 0 then
    ATargetStream.CopyFrom(Stream, APackedSize);
end;

{ TdxZIPStreamWriter }

constructor TdxZIPStreamWriter.Create(const AFileName: string);
begin
  Create(TFileStream.Create(AFileName, fmCreate), True);
end;

destructor TdxZIPStreamWriter.Destroy;
begin
  WriteCentralDirectory;
  inherited Destroy;
end;

procedure TdxZIPStreamWriter.AddDirectory(const AName: AnsiString);
var
  AEntry: TdxZIPFileEntry;
begin
  AEntry := TdxZIPFileEntry.Create(AName, DateTimeToFileDate(Now), 0, 0, 0);
  AEntry.IsDirectory := True;
  AEntry.RelativeOffset := Stream.Position;
  AEntry.WriteToStream(Stream);
  Files.Add(AEntry);
end;

procedure TdxZIPStreamWriter.AddFile(const AFileName: AnsiString; AData: PByte; ADataSize: Integer);
var
  ABuffer: Pointer;
  APackedSize: Integer;
begin
  if ADataSize > 0 then
  begin
    ZCompress(AData, ADataSize, ABuffer, APackedSize);
    try
      AddCompressedFile(AFileName, ABuffer, APackedSize, ADataSize, dxCRC32(AData, ADataSize));
    finally
      FreeMem(ABuffer);
    end;
  end
  else
    AddFileHeader(AFileName, 0, 0, 0);
end;

procedure TdxZIPStreamWriter.AddFile(const AFileName: AnsiString; AStream: TStream; AFreeStream: Boolean = False);
var
  ABuffer: Pointer;
  ABufferSize: Integer;
begin
  if AStream is TdxCompressedStream then
  begin
    dxTestCheck(AFreeStream, EInvalidOperation.ClassName);

    TdxCompressedStream(AStream).Flush;
    AddCompressedFile(AFileName,
      TdxCompressedStream(AStream).FBuffer.Memory,
      TdxCompressedStream(AStream).FBuffer.Size,
      TdxCompressedStream(AStream).UncompressedSize,
      TdxCompressedStream(AStream).CRC32);
  end
  else

  if AStream is TMemoryStream then
    AddFile(AFileName, TMemoryStream(AStream).Memory, TMemoryStream(AStream).Size)
  else
  begin
    ABufferSize := AStream.Size;
    ABuffer := AllocMem(ABufferSize);
    try
      AStream.Position := 0;
      AStream.ReadBuffer(ABuffer^, ABufferSize);
      AddFile(AFileName, ABuffer, ABufferSize);
    finally
      FreeMem(ABuffer);
    end;
  end;

  if AFreeStream then
    AStream.Free;
end;

procedure TdxZIPStreamWriter.AddCompressedFile(const AName: AnsiString;
  AData: PByte; ACompressedSize, AUncompressedSize, ACRC32: Integer);
begin
  if ACompressedSize > 0 then
  begin
    Inc(AData, ZLibHeaderSize);
    Dec(ACompressedSize, ZLibFooterSize);
    Dec(ACompressedSize, ZLibHeaderSize);
    AddFileHeader(AName, ACRC32, ACompressedSize, AUncompressedSize);
    Stream.WriteBuffer(AData^, ACompressedSize);
  end
  else
    AddFileHeader(AName, 0, 0, 0);
end;

procedure TdxZIPStreamWriter.AddFileHeader(const AName: AnsiString; ACRC32, APackedSize, ASize: Integer);
var
  AEntry: TdxZIPFileEntry;
begin
  AEntry := TdxZIPFileEntry.Create(AName, DateTimeToFileDate(Now), ACRC32, APackedSize, ASize);
  AEntry.RelativeOffset := Stream.Position;
  AEntry.WriteToStream(Stream);
  Files.Add(AEntry);
end;

procedure TdxZIPStreamWriter.WriteCentralDirectory;
var
  ADirData: TdxZIPCentralDirEntry;
  AEndOfDir: TdxZIPEndOfDir;
  AItem: TdxZIPFileEntry;
  I: Integer;
begin
  FillChar(AEndOfDir, SizeOf(AEndOfDir), 0);
  FillChar(ADirData, SizeOf(ADirData), 0);
  AEndOfDir.Sign := EndOfDir;
  AEndOfDir.DirOffset := Stream.Size;
  ADirData.Sign := DirFileHeader;
  ADirData.Version := 20;
  ADirData.VersionToExtract := 20;
  ADirData.Method := 8;

  for I := 0 to Files.Count - 1 do
  begin
    AItem := Files[I];
    ADirData.CRC32 := AItem.CRC32;
    ADirData.PackedSize := AItem.PackedSize;
    ADirData.OriginalSize := AItem.OriginalSize;
    ADirData.ExtAttributes := IfThen(AItem.IsDirectory, faDirectory);
    ADirData.DateTime := AItem.DateTime;
    ADirData.NameLength := Length(AItem.Name);
    ADirData.RelativeOffset := AItem.RelativeOffset;
    Stream.WriteBuffer(ADirData, SizeOf(ADirData));
    Stream.WriteBuffer(AItem.Name[1], ADirData.NameLength);
    Inc(AEndOfDir.DirEntryCount);
  end;
  AEndOfDir.DirStart := AEndOfDir.DirEntryCount;
  AEndOfDir.DirSize := Stream.Position - AEndOfDir.DirOffset;
  Stream.Write(AEndOfDir, SizeOf(AEndOfDir));
end;

{ TdxZIPPathHelper }

class function TdxZIPPathHelper.AbsoluteFileName(const ACurrentPath, AFileName: AnsiString): AnsiString;
begin
  if IsAbsoluteFileName(AFileName) then
    Result := Copy(AFileName, 2, MaxInt)
  else
    if AFileName <> '' then
      Result := ACurrentPath + AFileName
    else
      Result := AFileName;
end;

class function TdxZIPPathHelper.DecodePath(const APath: AnsiString): AnsiString;
begin
  Result := dxReplacePathDelimiter(APath, dxUnixPathDelim, PathDelim);
end;

class function TdxZIPPathHelper.EncodePath(const APath: AnsiString): AnsiString;
begin
  Result := dxReplacePathDelimiter(APath, PathDelim, dxUnixPathDelim);
end;

class function TdxZIPPathHelper.ExcludeRootPathDelimiter(const APath: AnsiString): AnsiString;
begin
  Result := APath;
  if (Result <> '') and (Result[1] = dxUnixPathDelim) then
    Delete(Result, 1, 1);
end;

class function TdxZIPPathHelper.ExcludeTrailingPathDelimiter(const APath: AnsiString): AnsiString;
var
  AIndex: Integer;
begin
  Result := APath;
  AIndex := Length(Result);
  if (AIndex > 0) and (Result[AIndex] = dxUnixPathDelim) then
    SetLength(Result, AIndex - 1);
end;

class function TdxZIPPathHelper.ExtractFilePath(const AFileName: AnsiString): AnsiString;
var
  AIndex: Integer;
begin
  AIndex := LastDelimiter(AnsiString(dxUnixPathDelim + DriveDelim), AFileName);
  Result := Copy(AFileName, 1, AIndex);
end;

class function TdxZIPPathHelper.ExpandFileName(const AFileName: AnsiString): AnsiString;

  function FindNearestFromLeftPathDelimiter(const S: AnsiString; const AStartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AStartIndex - 1 downto 1 do
    begin
      if S[I] = dxUnixPathDelim then
        Exit(I);
    end;
  end;

const
  sdxParentPathMacro = AnsiString(dxUnixPathDelim + '..' + dxUnixPathDelim);
var
  ADelimPos: Integer;
  AParentPathMacroPos: Integer;
begin
  Result := AFileName;
  repeat
    AParentPathMacroPos := Pos(sdxParentPathMacro, Result);
    if AParentPathMacroPos > 1 then
    begin
      ADelimPos := FindNearestFromLeftPathDelimiter(Result, AParentPathMacroPos);
      Delete(Result, ADelimPos + 1, AParentPathMacroPos - ADelimPos + Length(sdxParentPathMacro) - 1);
    end;
  until AParentPathMacroPos <= 1;
end;

class function TdxZIPPathHelper.IsAbsoluteFileName(const AFileName: AnsiString): Boolean;
begin
  Result := (AFileName <> '') and (AFileName[1] = dxUnixPathDelim);
end;

class function TdxZIPPathHelper.RelativePath(const ACurrentPath, AFileName: AnsiString): AnsiString;
begin
  Result := EncodePath(ExtractRelativePath(DecodePath(ACurrentPath), DecodePath(AFileName)));
end;

{ TdxInflateHelper }

class procedure TdxInflateHelper.Decompress(APackedStream: TStream; APackedSize: Integer; ATargetStream: TStream);

  procedure FlushBuffer(var AZStream: TZStreamRec; AOutBuffer: Pointer; AOutBufferSize: Integer);
  var
    AErrorCode: Integer;
  begin
    repeat
      AZStream.next_out := Pointer(AOutBuffer);
      AZStream.avail_out := AOutBufferSize;
      AErrorCode := inflate(AZStream, Z_FINISH);
      if AErrorCode = Z_BUF_ERROR then
        AErrorCode := Z_STREAM_END;
      CheckRead(AErrorCode >= Z_OK);
      ATargetStream.Write(AOutBuffer^, AOutBufferSize - Integer(AZStream.avail_out));
    until (AErrorCode = Z_STREAM_END) and (AZStream.avail_out > 0);
  end;

  procedure ProcessBuffer(var AZStream: TZStreamRec;
    AInBuffer, AOutBuffer: Pointer; AInBufferSize, AOutBufferSize: Integer);
  var
    AErrorCode: Integer;
  begin
    AZStream.avail_in := AInBufferSize;
    AZStream.next_in := AInBuffer;
    repeat
      AZStream.next_out := AOutBuffer;
      AZStream.avail_out := AOutBufferSize;
      AErrorCode := inflate(AZStream, Z_NO_FLUSH);
      CheckRead((AErrorCode >= Z_OK) or (AErrorCode = Z_BUF_ERROR));
      ATargetStream.Write(AOutBuffer^, AOutBufferSize - Integer(AZStream.avail_out));
    until (AZStream.avail_in = 0) and (AZStream.avail_out > 0);
  end;

const
  BufferSize = 32768;
var
  AInBuffer: Pointer;
  AInBufferSize: Integer;
  AOutBuffer: Pointer;
  AZStream: TZStreamRec;
begin
  if APackedSize <= 0 then
    Exit;

  AInBuffer := AllocMem(BufferSize);
  AOutBuffer := AllocMem(BufferSize);
  try
    FillChar(AZStream, SizeOf(TZStreamRec), 0);

    CheckRead(InflateInit_(AZStream, ZLIB_VERSION, SizeOf(AZStream)) >= 0);
    try
      ProcessBuffer(AZStream, @ZLibHeader, AOutBuffer, ZLibHeaderSize, BufferSize);
      repeat
        AInBufferSize := Min(APackedSize, BufferSize);
        if AInBufferSize > 0 then
        begin
          APackedStream.ReadBuffer(AInBuffer^, AInBufferSize);
          Dec(APackedSize, AInBufferSize);
          ProcessBuffer(AZStream, AInBuffer, AOutBuffer, AInBufferSize, BufferSize);
        end;
      until AInBufferSize = 0;
      FlushBuffer(AZStream, AOutBuffer, BufferSize);
    finally
      CheckRead(inflateEnd(AZStream) >= 0);
    end;
  finally
    FreeMem(AOutBuffer);
    FreeMem(AInBuffer);
  end;
end;

end.
