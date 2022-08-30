{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Import.Doc.DocComment;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocStringTable,
  dxRichEdit.Import.Doc.DocObjectCollection,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Import.Doc.PositionConverter;

type
  TdxComment = class(TObject);

  { TdxDocCommentsAuthorTable }

  TdxDocCommentsAuthorTable = class(TdxDocStringTableBase)
  strict private
    FData: TdxStringList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer); override;
    function CalcIsExtended(AReader: TBinaryReader): Boolean; override;
    procedure ReadString(AReader: TBinaryReader); override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    procedure WriteString(AWriter: TBinaryWriter; AIndex: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsAuthorTable; static;
    procedure Write(AWriter: TBinaryWriter); override;

    property Data: TdxStringList read FData;
  end;

  { TdxDocCommentsNameTable }

  TdxDocCommentsNameTable = class(TdxDocStringTableBase)
  public const
    TagOld                       = Integer(-1);
    ExtraDataSize                = SmallInt($000a);
    ClassAnnotationBookmark      = SmallInt($0100);
    MaxNumberAnnotationBookmarks = SmallInt($3ffb);
  strict private
    FExtraData: TdxStringList;
  protected
    function CalcRecordsCount(AReader: TBinaryReader): Integer; override;
    function CalcExtraDataSize(AReader: TBinaryReader): Integer; override;
    procedure ReadCore(AReader: TBinaryReader); override;
    procedure ReadExtraData(AReader: TBinaryReader); override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    procedure WriteCount(AWriter: TBinaryWriter); override;
    procedure WriteExtraDataSize(AWriter: TBinaryWriter); override;
    procedure WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsNameTable; static;
    procedure Write(AWriter: TBinaryWriter); override;

    property ExtraData: TdxStringList read FExtraData;
  end;

  { TdxDocCommentsFirstTable }

  TdxDocCommentsFirstTable = class
  strict private
    FCharacterPositions: TdxIntegerList;
    FIndexCharacterPositions: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
    procedure SortPosition;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsFirstTable; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure Finish(ALastCharacterPosition: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
    property IndexCharacterPositions: TdxIntegerList read FIndexCharacterPositions;
  end;

  { TdxDocCommentsLimTable }

  TdxDocCommentsLimTable = class
  strict private
    FCharacterPositions: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsLimTable; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure Finish(ALastCharacterPosition: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
  end;

  { TdxDocCommentsInfoTable }

  TdxDocCommentsInfoTable = class
  public const
    ReferenceLimSize       = Integer(4);
    StructureATRDPre10Size = Integer(30);
    InitialsSize           = Integer(20);
    PrefixNewCommentName   = 'dxcomment';
  strict private
    FReferenceLimTable: TdxIntegerList;
    FInitialsTable: TdxStringList;
    FAuthorIndexTable: TdxIntegerList;
    FReferenceNameTable: TdxStringList;
    FDateTimeDTTMTable: TdxIntegerList;
    FParentTable: TdxIntegerList;
    FOffsetParentTable: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
    procedure ReadInitials(AReader: TBinaryReader);
    procedure ReadAuthorNumbers(AReader: TBinaryReader);
    procedure ReadReferenceName(AReader: TBinaryReader; AIndex: Integer);
    function GetNewCommentName(AIndex: Integer): string;
    procedure ReadDateTime(AReader: TBinaryReader);
    procedure ReadIdParent(AReader: TBinaryReader);
    procedure ReadOffsetParent(AReader: TBinaryReader);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsInfoTable; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure WriteInitials(AWriter: TBinaryWriter; AIndex: Integer);
    procedure WriteAuthorNumbers(AWriter: TBinaryWriter; AIndex: Integer);
    procedure WriteReferenceName(AWriter: TBinaryWriter; AIndex: Integer);
    procedure WriteDateTime(AWriter: TBinaryWriter; AIndex: Integer);
    procedure WriteIdParent(AWriter: TBinaryWriter; AIndex: Integer);
    procedure WriteOffsetParent(AWriter: TBinaryWriter; AIndex: Integer);
    procedure Finish(ALastCharacterPosition: Integer);

    property ReferenceLimTable: TdxIntegerList read FReferenceLimTable;
    property InitialsTable: TdxStringList read FInitialsTable;
    property AuthorIndexTable: TdxIntegerList read FAuthorIndexTable;
    property ReferenceNameTable: TdxStringList read FReferenceNameTable;
    property DateTimeDTTMTable: TdxIntegerList read FDateTimeDTTMTable;
    property ParentTable: TdxIntegerList read FParentTable;
    property OffsetParentTable: TdxIntegerList read FOffsetParentTable;
  end;

  { TdxDocCommentsIterator }

  TdxDocCommentsIterator = class
  public const
    PrefixNewCommentName = 'dxcomment';
  strict private
    FKeepCommentsForRemovedRanges: Boolean;
    FCommentsContent: TdxDocObjectCollectionDictionary;
    FCommentsReferences: TdxIntegerList;
    FCommentsAuthorTable: TdxDocCommentsAuthorTable;
    FCommentsNameTable: TdxDocCommentsNameTable;
    FCommentsFirstTable: TdxDocCommentsFirstTable;
    FCommentsLimTable: TdxDocCommentsLimTable;
    FCommentsInfoTable: TdxDocCommentsInfoTable;
    FConverter: TdxPositionConverter;
    FCommentsIndex: TDictionary<Integer, TdxComment>;
  protected
    procedure Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
    procedure UpdateTablesForZeroLenghtComments;
    procedure InitConverter;

    property Converter: TdxPositionConverter read FConverter;
    property CommentsIndex: TDictionary<Integer, TdxComment> read FCommentsIndex;
  public
    constructor Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepCommentsForRemovedRanges: Boolean);
    destructor Destroy; override;
    procedure AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
    function FindParentComment(AIndex: Integer): TdxComment;
    function GetIndexCharacterPosition(ACharacterPosition: Integer): Integer;
    function GetCommentsReferences: TdxIntegerList;
    function GetCommentsContent(ACharacterPosition: Integer): TdxDocObjectCollection;
    procedure BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
    procedure EndEmbeddedContent;

    property CommentsContent: TdxDocObjectCollectionDictionary read FCommentsContent;
    property CommentsReferences: TdxIntegerList read FCommentsReferences;
  end;

implementation

uses
  Math, dxStringHelper, dxHash, dxEncoding;

{ TdxDocCommentsAuthorTable }

constructor TdxDocCommentsAuthorTable.Create;
begin
  FData := TdxStringList.Create;
end;

destructor TdxDocCommentsAuthorTable.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

class function TdxDocCommentsAuthorTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsAuthorTable;
begin
  Result := TdxDocCommentsAuthorTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocCommentsAuthorTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
begin
  if ASize = 0 then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  IsExtended := CalcIsExtended(AReader);
  Encoding := GetEncoding;
  while AReader.BaseStream.Position < AOffset + ASize do
    ReadString(AReader);
end;

{$HINTS OFF}
function TdxDocCommentsAuthorTable.CalcIsExtended(AReader: TBinaryReader): Boolean;
var
  ATypecode: Word;
begin
  ATypecode := AReader.ReadUInt16;
  Assert(ATypecode <> ExtendedTypeCode);
  AReader.BaseStream.Seek(-SizeOf(ATypecode), TSeekOrigin.soCurrent);
  Result := False;
end;
{$HINTS ON}

procedure TdxDocCommentsAuthorTable.ReadString(AReader: TBinaryReader);
var
  ALength: Integer;
  ABuffer: TBytes;
  AResult: string;
begin
  ALength := AReader.ReadByte;
  AReader.ReadByte;
  ABuffer := AReader.ReadBytes(ALength * 2);
  AResult := Encoding.GetString(ABuffer, 0, Length(ABuffer));
  Data.Add(TdxStringHelper.RemoveSpecialSymbols(AResult));
end;

procedure TdxDocCommentsAuthorTable.Write(AWriter: TBinaryWriter);
begin
  Encoding := GetEncoding;
  Count := Data.Count;
  WriteCore(AWriter);
end;

procedure TdxDocCommentsAuthorTable.WriteCore(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    WriteString(AWriter, I);
end;

procedure TdxDocCommentsAuthorTable.WriteString(AWriter: TBinaryWriter; AIndex: Integer);
var
  AStringData, AValidateStringData: string;
begin
  AStringData := Data[AIndex];
  AValidateStringData := Copy(AStringData, Min(56, Length(AStringData)));
  AWriter.Write(SmallInt(Length(AValidateStringData)));
  AWriter.Write(Encoding.GetBytes(AValidateStringData));
end;

{ TdxDocCommentsNameTable }

constructor TdxDocCommentsNameTable.Create;
begin
  FExtraData := TdxStringList.Create;
end;

destructor TdxDocCommentsNameTable.Destroy;
begin
  FExtraData.Free;
  inherited Destroy;
end;

class function TdxDocCommentsNameTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsNameTable;
begin
  Result := TdxDocCommentsNameTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

function TdxDocCommentsNameTable.CalcRecordsCount(AReader: TBinaryReader): Integer;
begin
  Result := Min(MaxNumberAnnotationBookmarks, AReader.ReadSmallInt);
end;

function TdxDocCommentsNameTable.CalcExtraDataSize(AReader: TBinaryReader): Integer;
begin
  Result := AReader.ReadSmallInt;
  Assert(Result = ExtraDataSize);
end;

procedure TdxDocCommentsNameTable.ReadCore(AReader: TBinaryReader);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ReadExtraData(AReader);
end;

procedure TdxDocCommentsNameTable.ReadExtraData(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(4, TSeekOrigin.soCurrent);
  ExtraData.Add(IntToStr(AReader.ReadInt32));
  AReader.BaseStream.Seek(4, TSeekOrigin.soCurrent);
end;

procedure TdxDocCommentsNameTable.Write(AWriter: TBinaryWriter);
begin
  Count := Min(MaxNumberAnnotationBookmarks, SmallInt(ExtraData.Count));
  if Count = 0 then
    Exit;
  inherited Write(AWriter);
end;

procedure TdxDocCommentsNameTable.WriteCore(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AWriter.Write(SmallInt(0));
    WriteExtraData(AWriter, I);
  end;
end;

procedure TdxDocCommentsNameTable.WriteCount(AWriter: TBinaryWriter);
begin
  AWriter.Write(SmallInt(Min(MaxNumberAnnotationBookmarks, ExtraData.Count)));
end;

procedure TdxDocCommentsNameTable.WriteExtraDataSize(AWriter: TBinaryWriter);
begin
  AWriter.Write(ExtraDataSize);
end;

procedure TdxDocCommentsNameTable.WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer);
var
  AName: Integer;
  AData: string;
begin
  AWriter.Write(ClassAnnotationBookmark);
  AData := ExtraData[AIndex];
  if TryStrToInt(AData, AName) then
    AWriter.Write(AName)
  else
    AWriter.Write(dxElfHash(AData));
  AWriter.Write(TagOld);
end;

{ TdxDocCommentsFirstTable }

constructor TdxDocCommentsFirstTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FIndexCharacterPositions := TdxIntegerList.Create;
end;

destructor TdxDocCommentsFirstTable.Destroy;
begin
  FCharacterPositions.Free;
  FIndexCharacterPositions.Free;
  inherited Destroy;
end;

class function TdxDocCommentsFirstTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsFirstTable;
begin
  Result := TdxDocCommentsFirstTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocCommentsFirstTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACommentsPositionSize, ACount, ACountOriginalStartPositions, I: Integer;
begin
  if ASize = 0 then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACommentsPositionSize := TdxDocConstants.CharacterPositionSize;
  ACount := (ASize - ACommentsPositionSize) div ACommentsPositionSize;
  ACountOriginalStartPositions := ACount div 2;
  for I := 0 to ACountOriginalStartPositions - 1 do
    CharacterPositions.Add(AReader.ReadInt32);
  AReader.ReadInt32;
  for I := ACountOriginalStartPositions to ACount - 1 do
    IndexCharacterPositions.Add(AReader.ReadInt32);
  SortPosition;
end;

procedure TdxDocCommentsFirstTable.SortPosition;
var
  ACount, I, J, AIndex, APosition: Integer;
begin
  ACount := IndexCharacterPositions.Count;
  for I := 0 to ACount - 1 - 1 do
    for J := 0 to ACount - 1 - 1 do
      if IndexCharacterPositions[J] > IndexCharacterPositions[J + 1] then
      begin
        AIndex := IndexCharacterPositions[J];
        IndexCharacterPositions[J] := IndexCharacterPositions[J + 1];
        IndexCharacterPositions[J + 1] := AIndex;
        APosition := CharacterPositions[J];
        CharacterPositions[J] := CharacterPositions[J + 1];
        CharacterPositions[J + 1] := APosition;
      end;
end;

procedure TdxDocCommentsFirstTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := CharacterPositions.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(CharacterPositions[I]);
  for I := 0 to ACount - 1 - 1 do
    AWriter.Write(IndexCharacterPositions[I]);
end;

procedure TdxDocCommentsFirstTable.Finish(ALastCharacterPosition: Integer);
begin
  CharacterPositions.Add(ALastCharacterPosition);
end;

{ TdxDocCommentsLimTable }

constructor TdxDocCommentsLimTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
end;

destructor TdxDocCommentsLimTable.Destroy;
begin
  FCharacterPositions.Free;
  inherited Destroy;
end;

class function TdxDocCommentsLimTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsLimTable;
begin
  Result := TdxDocCommentsLimTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocCommentsLimTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACommentsPositionSize, ACount, I: Integer;
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACommentsPositionSize := TdxDocConstants.CharacterPositionSize;
  ACount := (ASize - ACommentsPositionSize) div ACommentsPositionSize;
  for I := 0 to ACount - 1 do
    CharacterPositions.Add(AReader.ReadInt32);
end;

procedure TdxDocCommentsLimTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := CharacterPositions.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(CharacterPositions[I]);
end;

procedure TdxDocCommentsLimTable.Finish(ALastCharacterPosition: Integer);
begin
  CharacterPositions.Add(ALastCharacterPosition);
end;

{ TdxDocCommentsInfoTable }

constructor TdxDocCommentsInfoTable.Create;
begin
  FReferenceLimTable := TdxIntegerList.Create;
  FInitialsTable := TdxStringList.Create;
  FAuthorIndexTable := TdxIntegerList.Create;
  FReferenceNameTable := TdxStringList.Create;
  FDateTimeDTTMTable := TdxIntegerList.Create;
  FParentTable := TdxIntegerList.Create;
  FOffsetParentTable := TdxIntegerList.Create;
end;

destructor TdxDocCommentsInfoTable.Destroy;
begin
  FReferenceLimTable.Free;
  FInitialsTable.Free;
  FAuthorIndexTable.Free;
  FReferenceNameTable.Free;
  FDateTimeDTTMTable.Free;
  FParentTable.Free;
  FOffsetParentTable.Free;
  inherited Destroy;
end;

class function TdxDocCommentsInfoTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocCommentsInfoTable;
begin
  Result := TdxDocCommentsInfoTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocCommentsInfoTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACount, I: Integer;
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := (ASize - ReferenceLimSize) div (ReferenceLimSize + StructureATRDPre10Size);
  for I := 0 to ACount - 1 do
    ReferenceLimTable.Add(AReader.ReadInt32);
  AReader.ReadInt32;
  for I := 0 to ACount - 1 do
  begin
    ReadInitials(AReader);
    ReadAuthorNumbers(AReader);
    AReader.ReadInt32;
    ReadReferenceName(AReader, I);
  end;
  for I := 0 to ACount - 1 do
  begin
    ReadDateTime(AReader);
    AReader.ReadSmallInt;
    ReadIdParent(AReader);
    ReadOffsetParent(AReader);
    AReader.ReadInt32;
  end;
end;

procedure TdxDocCommentsInfoTable.ReadInitials(AReader: TBinaryReader);
var
  ALength: Integer;
  ABuffer: TBytes;
  AResult: string;
begin
  ALength := AReader.ReadByte;
  AReader.ReadByte;
  ABuffer := AReader.ReadBytes(InitialsSize - 2);
  AResult := Copy(TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer)), 1, ALength);
  InitialsTable.Add(TdxStringHelper.RemoveSpecialSymbols(AResult));
end;

procedure TdxDocCommentsInfoTable.ReadAuthorNumbers(AReader: TBinaryReader);
begin
  AuthorIndexTable.Add(AReader.ReadSmallInt);
end;

procedure TdxDocCommentsInfoTable.ReadReferenceName(AReader: TBinaryReader; AIndex: Integer);
var
  AReferenceName: Integer;
begin
  AReferenceName := AReader.ReadInt32;
  if AReferenceName = -1 then
    ReferenceNameTable.Add(GetNewCommentName(AIndex))
  else
    ReferenceNameTable.Add(IntToStr(AReferenceName));
end;

function TdxDocCommentsInfoTable.GetNewCommentName(AIndex: Integer): string;
begin
  Result := PrefixNewCommentName + IntToStr(AIndex);
end;

procedure TdxDocCommentsInfoTable.ReadDateTime(AReader: TBinaryReader);
begin
  DateTimeDTTMTable.Add(AReader.ReadInt32);
end;

procedure TdxDocCommentsInfoTable.ReadIdParent(AReader: TBinaryReader);
begin
  ParentTable.Add(AReader.ReadInt32);
end;

procedure TdxDocCommentsInfoTable.ReadOffsetParent(AReader: TBinaryReader);
begin
  OffsetParentTable.Add(AReader.ReadInt32);
end;

procedure TdxDocCommentsInfoTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := ReferenceLimTable.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(ReferenceLimTable[I]);
  for I := 0 to ACount - 1 - 1 do
  begin
    WriteInitials(AWriter, I);
    WriteAuthorNumbers(AWriter, I);
    AWriter.Write(Integer(0));
    WriteReferenceName(AWriter, I);
  end;
  for I := 0 to ACount - 1 - 1 do
  begin
    WriteDateTime(AWriter, I);
    AWriter.Write(SmallInt(0));
    WriteIdParent(AWriter, I);
    WriteOffsetParent(AWriter, I);
    AWriter.Write(Integer(0));
  end;
end;

procedure TdxDocCommentsInfoTable.WriteInitials(AWriter: TBinaryWriter; AIndex: Integer);
var
  AStringInitials, AValidateStringInitials: string;
  AByteInitials, ABuffer: TBytes;
  I: Integer;
begin
  AStringInitials := InitialsTable[AIndex];
  AValidateStringInitials := TdxStringHelper.Substring(AStringInitials, 0, Min(InitialsSize - 2, Length(AStringInitials)));
  AByteInitials := TdxEncoding.Unicode.GetBytes(AValidateStringInitials);
  SetLength(ABuffer, InitialsSize);
  ABuffer[0] := Byte(Length(AValidateStringInitials));
  for I := 0 to Length(AByteInitials) - 1 do
    ABuffer[2 + I] := AByteInitials[I];
  AWriter.Write(ABuffer);
end;

procedure TdxDocCommentsInfoTable.WriteAuthorNumbers(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(SmallInt(AuthorIndexTable[AIndex]));
end;

procedure TdxDocCommentsInfoTable.WriteReferenceName(AWriter: TBinaryWriter; AIndex: Integer);
var
  AName: Integer;
  AReference: string;
begin
  AReference := ReferenceNameTable[AIndex];
  AName := StrToIntDef(AReference, -1);
  if AName = -1 then
    AName := dxElfHash(AReference);
  AWriter.Write(AName);
end;

procedure TdxDocCommentsInfoTable.WriteDateTime(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(DateTimeDTTMTable[AIndex]);
end;

procedure TdxDocCommentsInfoTable.WriteIdParent(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(ParentTable[AIndex]);
end;

procedure TdxDocCommentsInfoTable.WriteOffsetParent(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(OffsetParentTable[AIndex]);
end;

procedure TdxDocCommentsInfoTable.Finish(ALastCharacterPosition: Integer);
begin
  ReferenceLimTable.Add(ALastCharacterPosition);
end;

{ TdxDocCommentsIterator }

constructor TdxDocCommentsIterator.Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepCommentsForRemovedRanges: Boolean);
begin
  FCommentsContent := TdxDocObjectCollectionDictionary.Create([doOwnsValues]);
  FConverter := TdxPositionConverter.Create;
  Read(AFib, AReader);
  FCommentsReferences := GetCommentsReferences;
  FCommentsIndex := TDictionary<Integer, TdxComment>.Create;
  FKeepCommentsForRemovedRanges := AKeepCommentsForRemovedRanges;
end;

destructor TdxDocCommentsIterator.Destroy;
begin
  FCommentsContent.Free;
  FConverter.Free;
  FCommentsIndex.Free;

  FCommentsAuthorTable.Free;
  FCommentsNameTable.Free;
  FCommentsFirstTable.Free;
  FCommentsLimTable.Free;
  FCommentsInfoTable.Free;
  inherited Destroy;
end;

procedure TdxDocCommentsIterator.Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
begin
  FCommentsAuthorTable := TdxDocCommentsAuthorTable.FromStream(AReader, AFib.CommentsAuthorTableOffset, AFib.CommentsAuthorTableSize);
  FCommentsNameTable := TdxDocCommentsNameTable.FromStream(AReader, AFib.CommentsNameTableOffset, AFib.CommentsNameTableSize);
  FCommentsFirstTable := TdxDocCommentsFirstTable.FromStream(AReader, AFib.CommentsFirstTableOffset, AFib.CommentsFirstTableSize);
  FCommentsLimTable := TdxDocCommentsLimTable.FromStream(AReader, AFib.CommentsLimTableOffset, AFib.CommentsLimTableSize);
  FCommentsInfoTable := TdxDocCommentsInfoTable.FromStream(AReader, AFib.CommentsReferenceOffset, AFib.CommentsReferenceSize);
  UpdateTablesForZeroLenghtComments;
  InitConverter;
end;

procedure TdxDocCommentsIterator.UpdateTablesForZeroLenghtComments;
var
  AReferenceNameTable: TdxStringList;
  ACount, I, APosition: Integer;
begin
  AReferenceNameTable := FCommentsInfoTable.ReferenceNameTable;
  ACount := AReferenceNameTable.Count;
  for I := 0 to ACount - 1 do
    if TdxStringHelper.Contains(AReferenceNameTable[I], PrefixNewCommentName) then
    begin
      APosition := FCommentsInfoTable.ReferenceLimTable[I];
      FCommentsFirstTable.CharacterPositions.Insert(I, APosition);
      FCommentsLimTable.CharacterPositions.Insert(I, APosition);
    end;
end;

procedure TdxDocCommentsIterator.InitConverter;
begin
  Converter.BeginInit;
  Converter.AppendPositions(FCommentsFirstTable.CharacterPositions);
  Converter.AppendPositions(FCommentsLimTable.CharacterPositions);
  Converter.EndInit;
end;

procedure TdxDocCommentsIterator.AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
begin
  Converter.AdvanceNext(ALogPosition, AOriginalPosition, ALength);
end;


function TdxDocCommentsIterator.FindParentComment(AIndex: Integer): TdxComment;
var
  AParentId, AOffsetParent, AParentIndex: Integer;
  AParentComment: TdxComment;
begin
  AParentId := FCommentsInfoTable.ParentTable[AIndex];
  AOffsetParent := FCommentsInfoTable.OffsetParentTable[AIndex];
  AParentIndex := AIndex + AOffsetParent;
  if (AParentId > 0) and CommentsIndex.TryGetValue(AParentIndex, AParentComment) then
    Exit(AParentComment);
  Result := nil;
end;

function TdxDocCommentsIterator.GetIndexCharacterPosition(ACharacterPosition: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to CommentsReferences.Count - 1 do
    if CommentsReferences[I] = ACharacterPosition then
      Exit(I);
  Result := -1;
end;

function TdxDocCommentsIterator.GetCommentsReferences: TdxIntegerList;
var
  ACount: Integer;
begin
  Result := FCommentsInfoTable.ReferenceLimTable;
  ACount := Result.Count;
  if ACount > 0 then
  begin
    Assert(FCommentsFirstTable.CharacterPositions.Count = ACount);
    Assert(FCommentsLimTable.CharacterPositions.Count = ACount);
    Assert(FCommentsInfoTable.ReferenceNameTable.Count = ACount);
    Assert(FCommentsInfoTable.AuthorIndexTable.Count = ACount);
    Assert(FCommentsInfoTable.InitialsTable.Count = ACount);
    Assert(FCommentsInfoTable.DateTimeDTTMTable.Count = ACount);
    Assert(FCommentsInfoTable.ParentTable.Count = ACount);
  end;
end;

function TdxDocCommentsIterator.GetCommentsContent(ACharacterPosition: Integer): TdxDocObjectCollection;
begin
  if not CommentsContent.TryGetValue(ACharacterPosition, Result) then
  begin
    Result := TdxDocObjectCollection.Create;
    CommentsContent.Add(ACharacterPosition, Result);
  end;
end;

procedure TdxDocCommentsIterator.BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
begin
  FConverter.BeginEmbeddedContent(AStart, AEnd);
end;

procedure TdxDocCommentsIterator.EndEmbeddedContent;
begin
  FConverter.EndEmbeddedContent;
end;

end.
