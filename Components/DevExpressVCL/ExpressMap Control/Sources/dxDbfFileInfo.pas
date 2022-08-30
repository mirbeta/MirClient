{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxDbfFileInfo;

interface

uses
  Classes, SysUtils, RTLConsts, StrUtils, Windows, Math, Generics.Defaults, Generics.Collections,
  dxCore;

{$I cxVer.inc}

type
  TdxDbfFileField = class
  private
    FFieldName: string;
    FFieldType: Byte;
    FFieldLengthInBytes: Byte;
    FNumberOfDecimalPlaces: Byte;
    FFieldFlags: Byte;
    FAutoIncrementStepValue: Byte;
  public
    procedure LoadFromStream(AStream: TStream; AEncoding: TEncoding);

    property FieldName: string read FFieldName;
    property FieldType: Byte read FFieldType;
    property FieldLengthInBytes: Byte read FFieldLengthInBytes;
    property NumberOfDecimalPlaces: Byte read FNumberOfDecimalPlaces;
    property FieldFlags: Byte read FFieldFlags;
    property AutoIncrementStepValue: Byte read FAutoIncrementStepValue;
  end;

  TdxDbfFileHeader = class
  private
    FFields: TObjectList<TdxDbfFileField>;
    FRecordsOffset: Integer;
    FRecordLength: Integer;
    FRecordCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Fields: TObjectList<TdxDbfFileField> read FFields;
    property RecordsOffset: Integer read FRecordsOffset write FRecordsOffset;
    property RecordLength: Integer read FRecordLength write FRecordLength;
    property RecordCount: Integer read FRecordCount write FRecordCount;
  end;

  TdxDbfFileRecordValues = class(TDictionary<TdxDbfFileField, string>);

  TdxDbfFileRecord = class
  private
    FFields: TdxDbfFileRecordValues;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddField(const AField: TdxDbfFileField; const AValue: string);
    property Fields: TdxDbfFileRecordValues read FFields;
  end;

  TdxDbfFileInfo = class
  private
    FEncoding: TEncoding;
    FHeader: TdxDbfFileHeader;
    FRecords: TObjectList<TdxDbfFileRecord>;
    procedure CheckEncoding(ALanguageDriver: Byte);
    function CheckTerminatedByte(AStream: TStream; AValue: Byte): Boolean;
    function CorrectFieldValueByType(AData: string; AType: Byte): string;
    function IsFileTypeSupported(AFileType: Byte): Boolean;
    procedure LoadHeader(AStream: TStream);
    procedure LoadRecord(ARecord: TdxDbfFileRecord; AStream: TStream);
    procedure LoadRecords(AStream: TStream);
  protected
    function GetEncoding: TEncoding;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    property Records: TObjectList<TdxDbfFileRecord> read FRecords;
  end;

implementation

const
  // Dbf file types
  dbftFoxBase = 2;
  dbftWithoutDBT = 3;
  dbftVisualFoxPro = $30;
  dbftFlagship = $43;
  dbftWithDBT = $83;
  dbftDBase4 = $8B;
  dbftFoxProWithMemo = $F5;
  dbftFoxPro = $FB;
  SupportedDbfFileTypesCount = 8;
  SupportedDbfFileTypes: array [0..SupportedDbfFileTypesCount - 1] of Byte = (dbftFoxBase, dbftWithoutDBT,
    dbftVisualFoxPro, dbftFlagship, dbftWithDBT, dbftDBase4, dbftFoxProWithMemo,
    dbftFoxPro);

{ TdxDbfFileInfo }

procedure TdxDbfFileInfo.CheckEncoding(ALanguageDriver: Byte);

const
  cp437 = 437;
  cp850 = 850;
  windows_1252 = 1252;
  macintosh = 1000;
  cp852 = 852;
  cp865 = 865;
  cp866 = 866;
  cp861 = 861;
  ibm737 = 737;
  cp857 = 857;
  x_mac_cyrillic = 10007;
  x_mac_ce = 10029;
  x_mac_greek = 10006;
  windows_1250 = 1250;
  windows_1251 = 1251;
  windows_1254 = 1254;
  windows_1253 = 1253;

  LanguageDriverCount = 19;
  LanguageDriver: array [0..LanguageDriverCount - 1] of Byte = (1, 2, 3, 4, 100, 101, 102, 103, 104,
   105, 106, 107, 150, 151, 152, 200, 201, 202, 203);
  CodePages: array [0..LanguageDriverCount - 1] of Integer = (cp437, cp850, windows_1252, macintosh, cp852, cp865, cp866, cp861, cp852,
   cp437, ibm737, cp857, x_mac_cyrillic, x_mac_ce, x_mac_greek, windows_1250, windows_1251, windows_1254, windows_1253);

var
  I: Integer;
begin
  FreeAndNil(FEncoding);
  for I := 0 to LanguageDriverCount - 1 do
    if LanguageDriver[I] = ALanguageDriver then
    begin
      FEncoding := TEncoding.GetEncoding(CodePages[I]);
      Break;
    end;
end;

function TdxDbfFileInfo.CheckTerminatedByte(AStream: TStream;
  AValue: Byte): Boolean;
var
  AByte: Byte;
begin
  AStream.ReadBuffer(AByte, SizeOf(Byte));
  Result := AByte = AValue;
  if not Result then
    AStream.Seek(-SizeOf(Byte), soCurrent);
end;

function TdxDbfFileInfo.CorrectFieldValueByType(AData: string;
  AType: Byte): string;
begin
  case Char(AType) of
    'L':
      if SameText(AData, 'T') or SameText(AData, 'Y') then
        Result := DefaultTrueBoolStr
      else
        Result := DefaultFalseBoolStr;
  else
    Result := Trim(AData);
  end;
end;

constructor TdxDbfFileInfo.Create;
begin
  inherited Create;
  FRecords := TObjectList<TdxDbfFileRecord>.Create;
  FHeader := TdxDbfFileHeader.Create;
end;

destructor TdxDbfFileInfo.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FRecords);
  FreeAndNil(FEncoding);
  inherited Destroy;
end;

function TdxDbfFileInfo.GetEncoding: TEncoding;
begin
  if FEncoding <> nil then
    Result := FEncoding
  else
    Result := TEncoding.Default;
end;

function TdxDbfFileInfo.IsFileTypeSupported(AFileType: Byte): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to SupportedDbfFileTypesCount - 1 do
    if AFileType = SupportedDbfFileTypes[I] then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxDbfFileInfo.LoadFromStream(AStream: TStream);
begin
  LoadHeader(AStream);
  LoadRecords(AStream);
end;

procedure TdxDbfFileInfo.LoadHeader(AStream: TStream);
var
  AFileType: Byte;
  ARecordCount: Cardinal;
  ARecordOffset, ARecordLength: Word;
  ALanguageDriver: Byte;
  AField: TdxDbfFileField;
begin
  AStream.ReadBuffer(AFileType, SizeOf(Byte));
  if not IsFileTypeSupported(AFileType) then
    raise EdxException.Create('Unsupported Dbf File Format');
  try
    AStream.Seek(3, soCurrent);
    AStream.ReadBuffer(ARecordCount, SizeOf(ARecordCount));
    AStream.ReadBuffer(ARecordOffset, SizeOf(ARecordOffset));
    AStream.ReadBuffer(ARecordLength, SizeOf(ARecordLength));
    FHeader.RecordCount := ARecordCount;
    FHeader.RecordsOffset := ARecordOffset;
    FHeader.RecordLength := ARecordLength;
    AStream.Seek(SizeOf(UInt16) + SizeOf(Byte) + SizeOf(Byte) + SizeOf(UInt32) + SizeOf(Byte) * 8 + SizeOf(Byte), soCurrent);
    AStream.ReadBuffer(ALanguageDriver, SizeOf(ALanguageDriver));
    CheckEncoding(ALanguageDriver);
    AStream.Seek(SizeOf(UInt16), soCurrent);
    while not CheckTerminatedByte(AStream, 13) do
    begin
      AField := TdxDbfFileField.Create;
      try
        AField.LoadFromStream(AStream, GetEncoding);
      except
        AField.Free;
        raise;
      end;
      FHeader.Fields.Add(AField);
    end;
  except
    raise EdxException.Create('Incorrect Dbf File Format');
  end;
end;

procedure TdxDbfFileInfo.LoadRecord(ARecord: TdxDbfFileRecord;
  AStream: TStream);
var
  ABytes: TBytes;
  AField: TdxDbfFileField;
  AData: string;
  AOffset: Integer;
  ACorrectFieldValueByType: string;
begin
  SetLength(ABytes, FHeader.RecordLength);
  AStream.ReadBuffer(ABytes[0], FHeader.RecordLength);
  AOffset := 1;
  for AField in FHeader.Fields do
  begin
    AData := GetEncoding.GetString(ABytes, AOffset, AField.FieldLengthInBytes);
    ACorrectFieldValueByType := CorrectFieldValueByType(AData, AField.FieldType);
    ARecord.AddField(AField, ACorrectFieldValueByType);
    Inc(AOffset, AField.FieldLengthInBytes);
  end;
end;

procedure TdxDbfFileInfo.LoadRecords(AStream: TStream);
var
  I: Integer;
  ARecord: TdxDbfFileRecord;
begin
  for I := 0 to FHeader.FRecordCount - 1 do
  begin
//    if CheckTerminatedByte(AStream, 26) then
//      Break;
    ARecord := TdxDbfFileRecord.Create;
    LoadRecord(ARecord, AStream);
    FRecords.Add(ARecord);
  end;
end;

{ TdxDbfFileField }

procedure TdxDbfFileField.LoadFromStream(AStream: TStream; AEncoding: TEncoding);
var
  ABytes: TBytes;
begin
  SetLength(ABytes, 11);
  AStream.ReadBuffer(ABytes[0], 11);
  FFieldName := PChar(AEncoding.GetString(ABytes));
  AStream.ReadBuffer(FFieldType, 1);
  AStream.Seek(SizeOf(UInt32), soCurrent);
  AStream.ReadBuffer(FFieldLengthInBytes, 1);
  AStream.ReadBuffer(FNumberOfDecimalPlaces, 1);
  AStream.ReadBuffer(FFieldFlags, 1);
  AStream.Seek(SizeOf(UInt32), soCurrent);
  AStream.ReadBuffer(FAutoIncrementStepValue, 1);
  AStream.Seek(8, soCurrent);
end;

{ TdxDbfFileHeader }

constructor TdxDbfFileHeader.Create;
begin
  inherited Create;
  FFields := TObjectList<TdxDbfFileField>.Create;
end;

destructor TdxDbfFileHeader.Destroy;
begin
  FreeAndNil(FFields);
  inherited Destroy;
end;

{ TdxDbfFileRecord }

procedure TdxDbfFileRecord.AddField(const AField: TdxDbfFileField; const AValue: string);
begin
  FFields.AddOrSetValue(AField, AValue);
end;

constructor TdxDbfFileRecord.Create;
begin
  inherited Create;
  FFields := TdxDbfFileRecordValues.Create;
end;

destructor TdxDbfFileRecord.Destroy;
begin
  FreeAndNil(FFields);
  inherited Destroy;
end;

end.
