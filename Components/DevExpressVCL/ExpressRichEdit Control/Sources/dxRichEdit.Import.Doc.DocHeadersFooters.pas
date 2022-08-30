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
unit dxRichEdit.Import.Doc.DocHeadersFooters;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxGenerics,

  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocObjectCollection;

type

  { TdxDocHeadersFootersPositions }

  TdxDocHeadersFootersPositions = class
  public const
    PositionSize = 4;
  strict private
    FCurrentStoryIndex: Integer;
    FCharacterPositions: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset, ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset, ASize: Integer): TdxDocHeadersFootersPositions; static;
    procedure AdvanceNext;
    function IsEmpty: Boolean;
    function IsLastHeaderFooter: Boolean;
    function GetNextStoryPosition: Integer;
    procedure Write(AWriter: TBinaryWriter);

    property CharacterPositions: TdxIntegerList read FCharacterPositions;
  end;

  { TdxDocHeadersFooters }

  TdxDocHeadersFooters = class
  public const
    FootnoteCollectionsCount = Integer(6);
    ItemsPerSection          = Integer(6);
    EvenPageHeaderPosition   = Integer(0);
    OddPageHeaderPosition    = Integer(1);
    EvenPageFooterPosition   = Integer(2);
    OddPageFooterPosition    = Integer(3);
    FirstPageHeaderPosition  = Integer(4);
    FirstPageFooterPosition  = Integer(5);
  strict private
    FHeadersFooters: TdxObjectList<TdxDocObjectCollection>;
    function GetActiveCollection: TdxDocObjectCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEvenPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetEvenPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetOddPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetOddPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetFirstPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetFirstPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
    function GetCollectionByIndex(ACollectionIndex: Integer): TdxDocObjectCollection;

    property HeadersFooters: TdxObjectList<TdxDocObjectCollection> read FHeadersFooters;
    property ActiveCollection: TdxDocObjectCollection read GetActiveCollection;
  end;


implementation

{ TdxDocHeadersFootersPositions }

constructor TdxDocHeadersFootersPositions.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
end;

destructor TdxDocHeadersFootersPositions.Destroy;
begin
  FCharacterPositions.Free;
  inherited Destroy;
end;

class function TdxDocHeadersFootersPositions.FromStream(AReader: TBinaryReader; AOffset, ASize: Integer): TdxDocHeadersFootersPositions;
begin
  Result := TdxDocHeadersFootersPositions.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocHeadersFootersPositions.AdvanceNext;
begin
  Inc(FCurrentStoryIndex);
end;

function TdxDocHeadersFootersPositions.IsEmpty: Boolean;
begin
  Result := FCharacterPositions.Count = 0;
end;

function TdxDocHeadersFootersPositions.IsLastHeaderFooter: Boolean;
begin
  Result := FCharacterPositions.Count - 2 = FCurrentStoryIndex;
end;

function TdxDocHeadersFootersPositions.GetNextStoryPosition: Integer;
begin
  Result := CharacterPositions[FCurrentStoryIndex + 1];
end;

procedure TdxDocHeadersFootersPositions.Read(AReader: TBinaryReader; AOffset, ASize: Integer);
var
  APositionIndex, ACount: Integer;
begin
  Assert(AReader <> nil, 'reader');
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := ASize div PositionSize;
  for APositionIndex := 0 to ACount - 1 do
    FCharacterPositions.Add(AReader.ReadInt32);
end;

procedure TdxDocHeadersFootersPositions.Write(AWriter: TBinaryWriter);
var
  APositionIndex: Integer;
begin
  for APositionIndex := 0 to FCharacterPositions.Count - 1 do
    AWriter.Write(FCharacterPositions[APositionIndex]);
end;

{ TdxDocHeadersFooters }

constructor TdxDocHeadersFooters.Create;
begin
  FHeadersFooters := TdxObjectList<TdxDocObjectCollection>.Create;
end;

destructor TdxDocHeadersFooters.Destroy;
begin
  FHeadersFooters.Free;
  inherited Destroy;
end;

function TdxDocHeadersFooters.GetActiveCollection: TdxDocObjectCollection;
begin
  Result := FHeadersFooters[FHeadersFooters.Count - 1];
end;

function TdxDocHeadersFooters.GetEvenPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + EvenPageHeaderPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetEvenPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + EvenPageFooterPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetOddPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + OddPageHeaderPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetOddPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + OddPageFooterPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetFirstPageHeaderObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + FirstPageHeaderPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetFirstPageFooterObjects(ASectionIndex: Integer): TdxDocObjectCollection;
var
  ACollectionIndex: Integer;
begin
  ACollectionIndex := FootnoteCollectionsCount + (ItemsPerSection * ASectionIndex) + FirstPageFooterPosition;
  Result := GetCollectionByIndex(ACollectionIndex);
end;

function TdxDocHeadersFooters.GetCollectionByIndex(ACollectionIndex: Integer): TdxDocObjectCollection;
begin
  if FHeadersFooters.Count > ACollectionIndex then
    Result := FHeadersFooters[ACollectionIndex]
  else
    Result := nil;
end;

end.
