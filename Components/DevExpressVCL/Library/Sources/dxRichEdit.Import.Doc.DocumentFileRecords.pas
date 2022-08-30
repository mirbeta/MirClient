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
unit dxRichEdit.Import.Doc.DocumentFileRecords;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxGenerics, dxRichEdit.Utils.Types;

type

  { TdxListStylesRecordItem }

  TdxListStylesRecordItem = record
  strict private
    FListIndex: Integer;
    FStyleIndex: Integer;
    FStyleDefinition: Boolean;
  public
    constructor Create(AListIndex: Integer; AStyleIndex: Integer; AStyleDefinition: Boolean);

    property ListIndex: Integer read FListIndex;
    property StyleIndex: Integer read FStyleIndex;
    property StyleDefinition: Boolean read FStyleDefinition;
  end;

  { TdxDocumentFileRecords }

  TdxDocumentFileRecords = class
  public type
    TRecordType = (
      FrameSetRoot,
      Frame,
      FrameChildMarker,
      FrameName,
      FrameFilePath,
      FrameBorderAttributes,
      ListStyles
    );
  strict private
    FListStyles: TList<TdxListStylesRecordItem>;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocumentFileRecords; static;
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
    function ReadDocumentFileRecord(AReader: TBinaryReader; AOffset: Integer): Integer;
    procedure ReadListStylesRecord(AReader: TBinaryReader);
    procedure Write(AWriter: TBinaryWriter);
    function ExportListStyles: TBytes;

    property ListStyles: TList<TdxListStylesRecordItem> read FListStyles;
  end;

implementation

uses
  Math, Contnrs;

{ TdxListStylesRecordItem }

constructor TdxListStylesRecordItem.Create(AListIndex: Integer; AStyleIndex: Integer; AStyleDefinition: Boolean);
begin
  FListIndex := AListIndex;
  FStyleIndex := AStyleIndex;
  FStyleDefinition := AStyleDefinition;
end;

{ TdxDocumentFileRecords }

constructor TdxDocumentFileRecords.Create;
begin
  FListStyles := TList<TdxListStylesRecordItem>.Create;
end;

destructor TdxDocumentFileRecords.Destroy;
begin
  FListStyles.Free;
  inherited Destroy;
end;

class function TdxDocumentFileRecords.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocumentFileRecords;
begin
  Result := TdxDocumentFileRecords.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocumentFileRecords.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ARecordSize: Integer;
begin
  if (ASize = 0) or (AReader.BaseStream.Size < Abs(AOffset)) then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  while ASize > 0 do
  begin
    ARecordSize := ReadDocumentFileRecord(AReader, AOffset);
    Dec(ASize, ARecordSize);
    Inc(AOffset, ARecordSize);
  end;
end;

function TdxDocumentFileRecords.ReadDocumentFileRecord(AReader: TBinaryReader; AOffset: Integer): Integer;
var
  AType: TRecordType;
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  Result := AReader.ReadInt32;
  AType := TRecordType(AReader.ReadInt32);
  if AType = TRecordType.ListStyles then
    ReadListStylesRecord(AReader);
end;

procedure TdxDocumentFileRecords.ReadListStylesRecord(AReader: TBinaryReader);
var
  ACount, I, AListIndex, ATmp, AStyleIndex: Integer;
  AStyleDefinition: Boolean;
begin
  ACount := AReader.ReadInt32;
  for I := 0 to ACount - 1 do
  begin
    AListIndex := AReader.ReadSmallInt;
    ATmp := AReader.ReadSmallInt;
    AStyleIndex := (ATmp and $0FFF);
    AStyleDefinition := (ATmp and $1000) <> 0;
    FListStyles.Add(TdxListStylesRecordItem.Create(AListIndex, AStyleIndex, AStyleDefinition));
  end;
end;

procedure TdxDocumentFileRecords.Write(AWriter: TBinaryWriter);
var
  AListStyles: TBytes;
begin
  AListStyles := ExportListStyles;
  AWriter.Write(Integer(Length(AListStyles) + 4));
  AWriter.Write(AListStyles, 0, Length(AListStyles));
end;

function TdxDocumentFileRecords.ExportListStyles: TBytes;
var
  ACount, I: Integer;
  AStream: TdxMemoryStream;
  AWriter: TBinaryWriter;
  AItem: TdxListStylesRecordItem;
  ASecondByte: SmallInt;
begin
  ACount := FListStyles.Count;
  AStream := TdxMemoryStream.Create;
  try
    AWriter := TBinaryWriter.Create(AStream);
    try
      AWriter.Write(Integer(Ord(TRecordType.ListStyles)));
      AWriter.Write(ACount);
      for I := 0 to ACount - 1 do
      begin
        AItem := FListStyles[I];
        AWriter.Write(SmallInt(AItem.ListIndex));
        ASecondByte := SmallInt(AItem.StyleIndex and $0FFF);
        if AItem.StyleDefinition then
          ASecondByte := ASecondByte or $1000;
        AWriter.Write(ASecondByte);
      end;
    finally
      AWriter.Free;
    end;
    Result := AStream.ToArray;
  finally
    AStream.Free;
  end;
end;

end.
