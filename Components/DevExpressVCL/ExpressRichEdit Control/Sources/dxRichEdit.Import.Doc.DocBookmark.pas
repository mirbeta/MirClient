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

unit dxRichEdit.Import.Doc.DocBookmark;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.Doc.DocStringTable,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Import.Doc.PositionConverter;

type
  { TdxDocBookmark }

  TdxDocBookmark = class
  strict private
    FOriginalStartPosition: Integer;
    FOriginalEndPosition: Integer;
    FName: string;
  public
    constructor Create(AOriginalStartPosition: Integer; AOriginalEndPosition: Integer; const AName: string);

    property OriginalStartPosition: Integer read FOriginalStartPosition;
    property OriginalEndPosition: Integer read FOriginalEndPosition;
    property Name: string read FName;
  end;

  { TdxDocBookmarkFirstDescriptor }

  TdxDocBookmarkFirstDescriptor = class
  strict private
    FBookmarkLimitDescriptorIndex: Integer;
    FFirstColumnIndex: SmallInt;
    FLimitColumnIndex: SmallInt;
    FColumn: Boolean;
  protected
    procedure Read(AReader: TBinaryReader; ABookmarkIndexSize: Integer);
  public
    constructor Create(ABookmarkLimitDescriptorIndex: SmallInt = 0);
    class function FromStream(AReader: TBinaryReader; ABookmarkIndexSize: Integer): TdxDocBookmarkFirstDescriptor; static;
    procedure Write(AWriter: TBinaryWriter; ABookmarkIndexSize: Integer);

    property BookmarkLimitDescriptorIndex: Integer read FBookmarkLimitDescriptorIndex;
    property FirstColumnIndex: SmallInt read FFirstColumnIndex;
    property LimitColumnIndex: SmallInt read FLimitColumnIndex;
    property Column: Boolean read FColumn;
  end;

  { TdxDocBookmarkFirstTable }

  TdxDocBookmarkFirstTable = class
  public const
    PositionSize           = Integer(4);
    BookmarkDescriptorSize = Integer(2);
    BookmarkFirstSize      = Integer(4);
  strict private
    FCharacterPositions: TdxIntegerList;
    FBookmarkFirstDescriptors: TdxObjectList<TdxDocBookmarkFirstDescriptor>;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; ABookmarkIndexSize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; ABookmarkIndexSize: Integer): TdxDocBookmarkFirstTable; static;
    procedure Write(AWriter: TBinaryWriter; ABookmarkIndexSize: Integer);
    procedure AddEntry(ACharacterPosition: Integer; AFirstDescriptor: TdxDocBookmarkFirstDescriptor);
    procedure Finish(ALastCharacterPosition: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
    property BookmarkFirstDescriptors: TdxObjectList<TdxDocBookmarkFirstDescriptor> read FBookmarkFirstDescriptors;
  end;

  { TdxDocBookmarkLimTable }

  TdxDocBookmarkLimTable = class
  public const
    PositionSize = Integer(4);
  strict private
    FCharacterPositions: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocBookmarkLimTable; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure Finish(ALastCharacterPosition: Integer);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
  end;

  { TdxDocBookmarkIteratorBase }

  TdxDocBookmarkIteratorBase = class abstract
  strict private
    FFirstTable: TdxDocBookmarkFirstTable;
    FLimTable: TdxDocBookmarkLimTable;
    FCurrentIndex: SmallInt;
    FConverter: TdxPositionConverter;
  protected
    procedure InitConverter; virtual;
    procedure Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader); virtual; abstract;

    property PositionConverter: TdxPositionConverter read FConverter;
    property LimTable: TdxDocBookmarkLimTable read FLimTable write FLimTable;
    property CurrentIndex: SmallInt read FCurrentIndex write FCurrentIndex;
  public
    constructor Create; overload;
    constructor Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader); overload; virtual;
    destructor Destroy; override;
    procedure AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
    procedure Finish(ALastCharacterPosition: Integer); virtual;
    procedure Write(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter); virtual; abstract;
    procedure BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
    procedure EndEmbeddedContent;

    property FirstTable: TdxDocBookmarkFirstTable read FFirstTable write FFirstTable;
  end;

  { TdxDocBookmarkIterator }

  TdxDocBookmarks = class(TdxObjectList<TdxDocBookmark>);

  TdxDocBookmarkIterator = class(TdxDocBookmarkIteratorBase)
  public const
    BookmarkIndexSize = Integer(2);
  strict private
    FBookmarks: TdxDocBookmarks;
    FKeepBookmarksForRemovedRanges: Boolean;
    FBookmarkNames: TdxDocStringTable;
    FBookmarkDescriptions: TDictionary<string, TdxIntegersPair>;
  protected
    procedure Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader); override;
  public
    constructor Create; overload;
    constructor Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepBookmarksForRemovedRanges: Boolean); reintroduce; overload;
    destructor Destroy; override;
    procedure InsertBookmarks(APieceTable: TdxPieceTable);
    procedure RemoveProcessedBookmarks(AProcessedBookmarks: TdxDocBookmarks);
    procedure Write(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter); override;
    procedure CollectBookmarks;
    procedure InitBookmarks;
    procedure AddBookmarkStart(const AName: string; AStartPosition: Integer);
    procedure AddBookmarkEnd(const AName: string; AEndPosition: Integer);
    procedure Finish(ALastCharacterPosition: Integer); override;
  end;

implementation

uses
  Types, Contnrs;

type
  { TDocBookmarkComparer }

  TDocBookmarkComparer = class(TComparer<TdxDocBookmark>)
  public
    function Compare(const Left, Right: TdxDocBookmark): Integer; override;
  end;

{ TDocBookmarkComparer }

function TDocBookmarkComparer.Compare(const Left, Right: TdxDocBookmark): Integer;
begin
  Result := Left.OriginalStartPosition - Right.OriginalStartPosition;
end;

{ TdxDocBookmark }

constructor TdxDocBookmark.Create(AOriginalStartPosition: Integer; AOriginalEndPosition: Integer; const AName: string);
begin
  FOriginalStartPosition := AOriginalStartPosition;
  FOriginalEndPosition := AOriginalEndPosition;
  FName := AName;
end;

{ TdxDocBookmarkFirstDescriptor }

constructor TdxDocBookmarkFirstDescriptor.Create(ABookmarkLimitDescriptorIndex: SmallInt = 0);
begin
  inherited Create;
  FBookmarkLimitDescriptorIndex := ABookmarkLimitDescriptorIndex;
end;

class function TdxDocBookmarkFirstDescriptor.FromStream(AReader: TBinaryReader; ABookmarkIndexSize: Integer): TdxDocBookmarkFirstDescriptor;
begin
  Result := TdxDocBookmarkFirstDescriptor.Create;
  Result.Read(AReader, ABookmarkIndexSize);
end;

procedure TdxDocBookmarkFirstDescriptor.Read(AReader: TBinaryReader; ABookmarkIndexSize: Integer);
var
  ABitField: Byte;
begin
  if ABookmarkIndexSize = 2 then
    FBookmarkLimitDescriptorIndex := AReader.ReadSmallInt
  else
    FBookmarkLimitDescriptorIndex := AReader.ReadInt32;
  ABitField := AReader.ReadByte;
  FFirstColumnIndex := ABitField and $7f;
  ABitField := AReader.ReadByte;
  FLimitColumnIndex := ABitField and $7f;
  FColumn := (ABitField and $80) = 1;
end;

procedure TdxDocBookmarkFirstDescriptor.Write(AWriter: TBinaryWriter; ABookmarkIndexSize: Integer);
var
  ABitField: Byte;
begin
  if ABookmarkIndexSize = 2 then
    AWriter.Write(SmallInt(FBookmarkLimitDescriptorIndex))
  else
    AWriter.Write(FBookmarkLimitDescriptorIndex);
  ABitField := FFirstColumnIndex and $7f;
  AWriter.Write(ABitField);
  ABitField := FLimitColumnIndex and $7f;
  if FColumn then
    ABitField := ABitField and $80;
  AWriter.Write(ABitField);
end;

{ TdxDocBookmarkFirstTable }

constructor TdxDocBookmarkFirstTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FBookmarkFirstDescriptors := TdxObjectList<TdxDocBookmarkFirstDescriptor>.Create;
end;

destructor TdxDocBookmarkFirstTable.Destroy;
begin
  FCharacterPositions.Free;
  FBookmarkFirstDescriptors.Free;
  inherited Destroy;
end;

class function TdxDocBookmarkFirstTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; ABookmarkIndexSize: Integer): TdxDocBookmarkFirstTable;
begin
  Result := TdxDocBookmarkFirstTable.Create;
  Result.Read(AReader, AOffset, ASize, ABookmarkIndexSize);
end;

procedure TdxDocBookmarkFirstTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; ABookmarkIndexSize: Integer);
var
  ABookmarkSize, ACount, I: Integer;
begin
  if (ASize <= 0) or (AOffset < 0) or (AReader.BaseStream.Size - AOffset < ASize) then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ABookmarkSize := ABookmarkIndexSize + BookmarkDescriptorSize;
  ACount := (ASize - PositionSize) div (PositionSize + ABookmarkSize);
  for I := 0 to ACount + 1 - 1 do
    FCharacterPositions.Add(AReader.ReadInt32);
  for I := 0 to ACount - 1 do
    FBookmarkFirstDescriptors.Add(TdxDocBookmarkFirstDescriptor.FromStream(AReader, ABookmarkIndexSize));
end;

procedure TdxDocBookmarkFirstTable.Write(AWriter: TBinaryWriter; ABookmarkIndexSize: Integer);
var
  ACount, I: Integer;
begin
  ACount := FCharacterPositions.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(FCharacterPositions[I]);
  for I := 0 to ACount - 1 - 1 do
    FBookmarkFirstDescriptors[I].Write(AWriter, ABookmarkIndexSize);
end;

procedure TdxDocBookmarkFirstTable.AddEntry(ACharacterPosition: Integer; AFirstDescriptor: TdxDocBookmarkFirstDescriptor);
begin
  FCharacterPositions.Add(ACharacterPosition);
  FBookmarkFirstDescriptors.Add(AFirstDescriptor);
end;

procedure TdxDocBookmarkFirstTable.Finish(ALastCharacterPosition: Integer);
begin
  FCharacterPositions.Add(ALastCharacterPosition);
end;

{ TdxDocBookmarkLimTable }

constructor TdxDocBookmarkLimTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
end;

destructor TdxDocBookmarkLimTable.Destroy;
begin
  FCharacterPositions.Free;
  inherited Destroy;
end;

class function TdxDocBookmarkLimTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocBookmarkLimTable;
begin
  Result := TdxDocBookmarkLimTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocBookmarkLimTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACount, I: Integer;
begin
  if (ASize <= 0) or (AOffset < 0) or (AReader.BaseStream.Size - AOffset < ASize) then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := ASize div PositionSize;
  for I := 0 to ACount - 1 do
    FCharacterPositions.Add(AReader.ReadInt32);
end;

procedure TdxDocBookmarkLimTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := FCharacterPositions.Count;
  if ACount <= 1 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(FCharacterPositions[I]);
end;

procedure TdxDocBookmarkLimTable.Finish(ALastCharacterPosition: Integer);
begin
  FCharacterPositions.Add(ALastCharacterPosition);
end;

{ TdxDocBookmarkIteratorBase }

constructor TdxDocBookmarkIteratorBase.Create;
begin
  FFirstTable := TdxDocBookmarkFirstTable.Create;
  FLimTable := TdxDocBookmarkLimTable.Create;
end;

constructor TdxDocBookmarkIteratorBase.Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
begin
  Read(AFib, AReader);
end;

destructor TdxDocBookmarkIteratorBase.Destroy;
begin
  FFirstTable.Free;
  FLimTable.Free;
  FConverter.Free;
  inherited Destroy;
end;

procedure TdxDocBookmarkIteratorBase.InitConverter;
begin
  FConverter := TdxPositionConverter.Create;
  FConverter.BeginInit;
  FConverter.AppendPositions(FFirstTable.CharacterPositions);
  FConverter.AppendPositions(FLimTable.CharacterPositions);
  FConverter.EndInit;
end;

procedure TdxDocBookmarkIteratorBase.AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
begin
  FConverter.AdvanceNext(ALogPosition, AOriginalPosition, ALength);
end;

procedure TdxDocBookmarkIteratorBase.Finish(ALastCharacterPosition: Integer);
begin
  FFirstTable.Finish(ALastCharacterPosition);
  FLimTable.Finish(ALastCharacterPosition);
end;

procedure TdxDocBookmarkIteratorBase.BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
begin
  FConverter.BeginEmbeddedContent(AStart, AEnd);
end;

procedure TdxDocBookmarkIteratorBase.EndEmbeddedContent;
begin
  FConverter.EndEmbeddedContent;
end;

{ TdxDocBookmarkIterator }

constructor TdxDocBookmarkIterator.Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepBookmarksForRemovedRanges: Boolean);
begin
  inherited Create(AFib, AReader);
  FKeepBookmarksForRemovedRanges := AKeepBookmarksForRemovedRanges;
end;

constructor TdxDocBookmarkIterator.Create;
begin
  inherited Create;
  FBookmarkDescriptions := TDictionary<string, TdxIntegersPair>.Create;
  FBookmarkNames := TdxDocStringTable.Create;
  FBookmarks := TdxDocBookmarks.Create;
end;

destructor TdxDocBookmarkIterator.Destroy;
begin
  FBookmarks.Free;
  FBookmarkNames.Free;
  FBookmarkDescriptions.Free;
  inherited Destroy;
end;

procedure TdxDocBookmarkIterator.InsertBookmarks(APieceTable: TdxPieceTable);
var
  AProcessedBookmarks: TdxDocBookmarks;
  AStartPosition, AEndPosition: TdxDocumentLogPosition;
  ASkipDeletedPosition, AStartPositionObtainable, AEndPositionObtainable: Boolean;
  ABookmark: TdxDocBookmark;
  I: Integer;
begin
  AProcessedBookmarks := TdxDocBookmarks.Create(False);
  try
    ASkipDeletedPosition := not FKeepBookmarksForRemovedRanges;
    for I := 0 to FBookmarks.Count - 1 do
    begin
      ABookmark := FBookmarks[I];
      if not PositionConverter.ContainsPosition(ABookmark.OriginalStartPosition) or not PositionConverter.ContainsPosition(ABookmark.OriginalEndPosition) then
        Continue;
      AStartPositionObtainable := PositionConverter.TryConvert(ABookmark.OriginalStartPosition, ASkipDeletedPosition, AStartPosition);
      AEndPositionObtainable := PositionConverter.TryConvert(ABookmark.OriginalEndPosition, ASkipDeletedPosition, AEndPosition);
      if AStartPositionObtainable and AEndPositionObtainable then
      begin
        APieceTable.CreateBookmarkCore(AStartPosition, AEndPosition - AStartPosition, ABookmark.Name);
        AProcessedBookmarks.Add(ABookmark);
        Continue;
      end;
      if AStartPositionObtainable then
        PositionConverter.TryConvert(ABookmark.OriginalEndPosition, False, AEndPosition);
      if AEndPositionObtainable then
        PositionConverter.TryConvert(ABookmark.OriginalStartPosition, False, AStartPosition);
      if AEndPosition > AStartPosition then
      begin
        APieceTable.CreateBookmarkCore(AStartPosition, AEndPosition - AStartPosition, ABookmark.Name);
        AProcessedBookmarks.Add(ABookmark);
      end;
    end;
    RemoveProcessedBookmarks(AProcessedBookmarks);
  finally
    AProcessedBookmarks.Free;
  end;
end;

procedure TdxDocBookmarkIterator.RemoveProcessedBookmarks(AProcessedBookmarks: TdxDocBookmarks);
var
  AProcessed: TdxDocBookmark;
  I: Integer;
begin
  for I := 0 to AProcessedBookmarks.Count - 1 do
  begin
    AProcessed := AProcessedBookmarks[I];
    FBookmarks.Remove(AProcessed);
  end;
end;

procedure TdxDocBookmarkIterator.Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
begin
  FBookmarkNames := TdxDocStringTable.FromStream(AReader, AFib.BookmarkNamesTableOffset, AFib.BookmarkNamesTableSize);
  FirstTable := TdxDocBookmarkFirstTable.FromStream(AReader, AFib.BookmarkStartInfoOffset, AFib.BookmarkStartInfoSize, BookmarkIndexSize);
  LimTable := TdxDocBookmarkLimTable.FromStream(AReader, AFib.BookmarkEndInfoOffset, AFib.BookmarkEndInfoSize);
  InitBookmarks;
  InitConverter;
end;

procedure TdxDocBookmarkIterator.Write(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  AFib.BookmarkNamesTableOffset := AWriter.BaseStream.Position;
  FBookmarkNames.Write(AWriter);
  AFib.BookmarkNamesTableSize := AWriter.BaseStream.Position - AFib.BookmarkNamesTableOffset;
  AFib.BookmarkStartInfoOffset := AWriter.BaseStream.Position;
  FirstTable.Write(AWriter, BookmarkIndexSize);
  AFib.BookmarkStartInfoSize := AWriter.BaseStream.Position - AFib.BookmarkStartInfoOffset;
  AFib.BookmarkEndInfoOffset := AWriter.BaseStream.Position;
  LimTable.Write(AWriter);
  AFib.BookmarkEndInfoSize := AWriter.BaseStream.Position - AFib.BookmarkEndInfoOffset;
end;

procedure TdxDocBookmarkIterator.CollectBookmarks;
var
  AEndPositions: TdxIntegerList;
  AEndPositionIndicies: TdxIntegersDictionary;
  AItem: TPair<string, TdxIntegersPair>;
  AEnd, ACount, I, AEndPositionIndex, AOffset: Integer;
  ABookmark: TdxDocBookmark;
  AComparer: IComparer<TdxDocBookmark>;
begin
  AEndPositions := TdxIntegerList.Create;
  try
    AEndPositionIndicies := TdxIntegersDictionary.Create;
    try
      for AItem in FBookmarkDescriptions do
      begin
        AEnd := AItem.Value.Value;
        if AEnd <> -1 then
        begin
          FBookmarks.Add(TdxDocBookmark.Create(AItem.Value.Key, AEnd, AItem.Key));
          AEndPositions.Add(AEnd);
        end;
      end;
      AComparer := TDocBookmarkComparer.Create;
      FBookmarks.Sort(AComparer);
      AEndPositions.Sort;
      ACount := FBookmarks.Count;
      for I := 0 to ACount - 1 do
      begin
        ABookmark := FBookmarks[I];
        FBookmarkNames.Data.Add(ABookmark.Name);
        AEndPositionIndex := AEndPositions.IndexOf(ABookmark.OriginalEndPosition);
        if not AEndPositionIndicies.TryGetValue(AEndPositionIndex, AOffset) then
        begin
          AOffset := 0;
          AEndPositionIndicies.Add(AEndPositionIndex, AOffset);
        end
        else
        begin
          Inc(AOffset);
          AEndPositionIndicies[AEndPositionIndex] := AOffset;
        end;
        FirstTable.AddEntry(ABookmark.OriginalStartPosition, TdxDocBookmarkFirstDescriptor.Create(AEndPositionIndex + AOffset));
        CurrentIndex := CurrentIndex + 1;
      end;
    finally
      AEndPositionIndicies.Free;
    end;
    LimTable.CharacterPositions.AddRange(AEndPositions);
  finally
    AEndPositions.Free;
  end;
end;

procedure TdxDocBookmarkIterator.InitBookmarks;
var
  ACount, I, ABookmarkStartPosition, AEndPositionIndex, ABookmarkEndPosition: Integer;
  ABookmarkName: string;
begin
  FBookmarks := TdxDocBookmarks.Create;
  ACount := FBookmarkNames.Data.Count;

  for I := 0 to ACount - 1 do
  begin
    if FirstTable.BookmarkFirstDescriptors[I].Column then
      Continue;

    ABookmarkName := FBookmarkNames.Data[I];
    ABookmarkStartPosition := FirstTable.CharacterPositions[I];
    AEndPositionIndex := FirstTable.BookmarkFirstDescriptors[I].BookmarkLimitDescriptorIndex;
    if AEndPositionIndex >= ACount then
      Continue;
    ABookmarkEndPosition := LimTable.CharacterPositions[AEndPositionIndex];
    FBookmarks.Add(TdxDocBookmark.Create(ABookmarkStartPosition, ABookmarkEndPosition, ABookmarkName));
  end;
end;

procedure TdxDocBookmarkIterator.AddBookmarkStart(const AName: string; AStartPosition: Integer);
begin
  if not FBookmarkDescriptions.ContainsKey(AName) then
    FBookmarkDescriptions.Add(AName, TdxIntegersPair.Create(AStartPosition, -1));
end;

procedure TdxDocBookmarkIterator.AddBookmarkEnd(const AName: string; AEndPosition: Integer);
var
  AInterval: TdxIntegersPair;
begin
  if not FBookmarkDescriptions.TryGetValue(AName, AInterval) then
    Exit;
  AInterval.Value := AEndPosition;
  FBookmarkDescriptions[AName] := AInterval;
end;

procedure TdxDocBookmarkIterator.Finish(ALastCharacterPosition: Integer);
begin
  CollectBookmarks;
  inherited Finish(ALastCharacterPosition);
end;

end.
