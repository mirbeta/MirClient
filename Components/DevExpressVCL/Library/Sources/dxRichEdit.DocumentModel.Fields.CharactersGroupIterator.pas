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

unit dxRichEdit.DocumentModel.Fields.CharactersGroupIterator;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,

  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTableIterators;

type
  { TdxCharactersGroupIterator }

  TdxCharactersGroupIterator = class
  strict private
    class var
      GroupSymbols: TdxCharList;
  strict private
    FCachedRunIndex: TdxRunIndex;
    FCachedRangeText: string;
    FPieceTable: TdxSimplePieceTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateGroupSymbolsTable: TdxCharList; static;
    function GetDocumentModel: TdxCustomDocumentModel;
  protected
    procedure MoveNextCore(var APos: TdxDocumentModelPosition); virtual;
    function SkipCharacters(var APos: TdxDocumentModelPosition;
      AIterator: TdxPieceTableIterator; const APredicate: TdxPredicate<Char>): Boolean;
    function IsEndOfDocument(const APos: TdxDocumentModelPosition): Boolean;
    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    function IsGroupCharacter(const ACh: Char): Boolean;
    function IsWhiteSpace(const ACh: Char): Boolean;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxSimplePieceTable);
    function MoveNext(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
  end;

implementation

uses
  Character;

type
  TdxCharactersDocumentModelIteratorAccess = class(TdxCharactersDocumentModelIterator);
  TdxPieceTableIteratorAccess = class(TdxPieceTableIterator);

{ TdxCharactersGroupIterator }

class constructor TdxCharactersGroupIterator.Initialize;
begin
  GroupSymbols := CreateGroupSymbolsTable;
end;

class destructor TdxCharactersGroupIterator.Finalize;
begin
  FreeAndNil(GroupSymbols);
end;

constructor TdxCharactersGroupIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FCachedRunIndex := -1;
  FCachedRangeText := '';
  FPieceTable := APieceTable;
end;

class function TdxCharactersGroupIterator.CreateGroupSymbolsTable: TdxCharList;
begin
  Result := TdxCharList.Create;
  Result.Add('+');
  Result.Add('-');
  Result.Add(',');
  Result.Add('''');
end;

function TdxCharactersGroupIterator.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxCharactersGroupIterator.MoveNext(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if IsEndOfDocument(Result) then
    Exit(Result);
  MoveNextCore(Result);
end;

procedure TdxCharactersGroupIterator.MoveNextCore(var APos: TdxDocumentModelPosition);
var
  AIterator: TdxPieceTableIterator;
begin
  AIterator := TdxCharactersDocumentModelIterator.Create(PieceTable);
  try
    if not SkipCharacters(APos, AIterator, IsGroupCharacter) then
    begin
      if not SkipCharacters(APos, AIterator, IsWhiteSpace) then
        TdxCharactersDocumentModelIteratorAccess(AIterator).MoveForwardCore(APos);
    end;
  finally
    AIterator.Free;
  end;
end;

function TdxCharactersGroupIterator.SkipCharacters(var APos: TdxDocumentModelPosition;
  AIterator: TdxPieceTableIterator; const APredicate: TdxPredicate<Char>): Boolean;
begin
  if not APredicate(GetCharacter(APos)) then
    Exit(False);
  while not IsEndOfDocument(APos) and APredicate(GetCharacter(APos)) do
    TdxPieceTableIteratorAccess(AIterator).MoveForwardCore(APos);
  Result := True;
end;

function TdxCharactersGroupIterator.IsEndOfDocument(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.LogPosition = PieceTable.DocumentEndLogPosition;
end;

function TdxCharactersGroupIterator.GetCharacter(const APos: TdxDocumentModelPosition): Char;
begin
  if FCachedRunIndex <> APos.RunIndex then
  begin
    FCachedRunIndex := APos.RunIndex;
    FCachedRangeText := PieceTable.GetRunNonEmptyText(FCachedRunIndex);
  end;
  Result := FCachedRangeText[APos.RunOffset + 1];
end;

function TdxCharactersGroupIterator.IsGroupCharacter(const ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(ACh){$ENDIF} or GroupSymbols.Contains(ACh);
end;

function TdxCharactersGroupIterator.IsWhiteSpace(const ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF};
end;

end.
