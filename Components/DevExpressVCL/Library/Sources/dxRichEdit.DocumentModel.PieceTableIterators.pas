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

unit dxRichEdit.DocumentModel.PieceTableIterators;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Platform.Font;

type
  { TdxPieceTableIterator }

  TdxPieceTableIterator = class abstract
  strict private
    FPieceTable: TdxSimplePieceTable;
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); virtual; abstract;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); virtual; abstract;
    procedure UpdateModelPositionByLogPosition(var APos: TdxDocumentModelPosition);
    property PieceTable: TdxSimplePieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); virtual;

    function MoveBack(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function MoveForward(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;

    function IsEndOfDocument(const APos: TdxDocumentModelPosition): Boolean; virtual;
    function IsNewElement(const ACurrentPosition: TdxDocumentModelPosition): Boolean; virtual;
    function IsStartOfDocument(const APos: TdxDocumentModelPosition): Boolean; virtual;
  end;

  { TdxCharactersDocumentModelIterator }

  TdxCharactersDocumentModelIterator = class(TdxPieceTableIterator)
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;

    procedure SkipForward(var APos: TdxDocumentModelPosition); virtual;
    procedure SkipBackward(var APos: TdxDocumentModelPosition); virtual;
  end;

  { TdxVisibleCharactersStopAtFieldsDocumentModelIterator }

  TdxVisibleCharactersStopAtFieldsDocumentModelIterator = class(TdxCharactersDocumentModelIterator)
  strict private
    FFilter: IdxVisibleTextFilter;
  protected
    function IsFieldDelimiter(const APos: TdxDocumentModelPosition): Boolean; virtual;
    procedure SkipForward(var APos: TdxDocumentModelPosition); override;
    procedure SkipBackward(var APos: TdxDocumentModelPosition); override;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
  end;

  { TdxWordsDocumentModelIteratorBase }

  TdxWordsDocumentModelIteratorBase = class abstract(TdxPieceTableIterator)
  strict private
    FCachedRunIndex: TdxRunIndex;
    FCachedRunText: string;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;

    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    procedure SkipForward(AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); virtual;
    procedure SkipBackward(AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); virtual;
  end;

  { TdxWordsDocumentModelIterator }

  TdxWordsDocumentModelIterator = class(TdxWordsDocumentModelIteratorBase)
  strict private
    class var FNonWordSymbols: TdxCharList;
    class var FSpaces: TdxCharList;
    class var FSpecialSymbols: TdxCharList;
    class var FWordsDelimiter: TdxCharList;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateSpacesList: TdxCharList;
    class function CreateSpecialSymbolsList: TdxCharList;
    class function CreateWordDelimiters: TdxCharList;
    class function CreateNonWordSymbols: TdxCharList;
    class procedure PopulateSpacesList(AList: TdxCharList);
    class procedure PopulateSpecialSymbolsList(AList: TdxCharList);
    class procedure PopulateWordDelimiters(AList: TdxCharList);
  protected
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;

    class property NonWordSymbols: TdxCharList read FNonWordSymbols;
    class property Spaces: TdxCharList read FSpaces;
    class property SpecialSymbols: TdxCharList read FSpecialSymbols;
    class property WordsDelimiter: TdxCharList read FWordsDelimiter;
  public
    function IsWordsDelimiter(const AItem: Char): Boolean;
    function IsNotNonWordsSymbols(const AItem: Char): Boolean;
    function IsSpace(const AItem: Char): Boolean;

    function IsInsideWord(const ACurrentPosition: TdxDocumentModelPosition): Boolean;
    function IsAtWord(const ACurrentPosition: TdxDocumentModelPosition): Boolean;
  end;

  { TdxVisibleWordsIterator }

  TdxVisibleWordsIterator = class(TdxWordsDocumentModelIterator)
  strict private
    FFilter: IdxVisibleTextFilter;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;

    procedure SkipForward(AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); override;
    procedure SkipBackward(AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
      const APredicate: TdxPredicate<Char>); override;
  end;

  { TdxParagraphsDocumentModelIterator }

  TdxParagraphsDocumentModelIterator = class(TdxPieceTableIterator)
  protected
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
  end;


  { TdxSpellCheckerWordIterator }

  TdxSpellCheckerWordIterator = class(TdxPieceTableIterator)
  strict private
    class var
      WordsSeparators: TdxCharList;
      InWordSymbols: TdxCharList;
  strict private
    FCachedRunIndex: TdxRunIndex;
    FCachedRunText: string;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateSeparatorsTable: TdxCharList; static;
    class function CreateInWordSymbolsTable: TdxCharList; static;
    procedure SkipBackwardInvisibleRuns(var APos: TdxDocumentModelPosition);
    procedure SkipForwardInvisibleRuns(var APos: TdxDocumentModelPosition);
    function GetVisibleTextFilter: IdxVisibleTextFilter;
  protected
    procedure MoveForwardCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveBackCore(var APos: TdxDocumentModelPosition); override;
    procedure MoveToWordStartCore(var APos: TdxDocumentModelPosition); overload;

    property VisibleTextFilter: IdxVisibleTextFilter read GetVisibleTextFilter;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    function MoveToWordEnd(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
    function MoveToWordStart(const APos: TdxDocumentModelPosition; ASkipNotLetterOrDigitChar: Boolean = True): TdxDocumentModelPosition; overload; virtual;
    procedure MoveToNextWordEnd(var APos: TdxDocumentModelPosition); virtual;
    procedure MoveToPrevWordStart(var APos: TdxDocumentModelPosition); virtual;
    // for internal use
    function GetCharacter(const APos: TdxDocumentModelPosition): Char;
    function IsNotWordsSeparators(const ACh: Char): Boolean; virtual;
    function IsWordsSeparators(const ACh: Char): Boolean; virtual;
    function IsNotLetterOrDigit(const ACh: Char): Boolean; virtual;
    function IsLetterOrDigit(const ACh: Char): Boolean; virtual;
    procedure MoveToPrevChar(var APos: TdxDocumentModelPosition);
    procedure MoveToPrevRun(var APos: TdxDocumentModelPosition; ARunEndPos: TdxDocumentLogPosition);
    procedure MoveToNextChar(var APos: TdxDocumentModelPosition);
    function MoveToNextRun(var APos: TdxDocumentModelPosition; ARunStartPos: TdxDocumentLogPosition): Boolean;
    procedure MoveToWordEndCore(var APos: TdxDocumentModelPosition; AWordWithDots: Boolean = False); overload;
    procedure SkipBackward(var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
    procedure SkipForward(var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);

    property PieceTable;
    property CachedRunIndex: TdxRunIndex read FCachedRunIndex;
  end;

implementation

uses
  Math, Character, dxCore,
  dxRichEdit.DocumentModel.FieldRange,
  dxCharacters;

{ TdxPieceTableIterator }

constructor TdxPieceTableIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
end;

function TdxPieceTableIterator.IsEndOfDocument(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.LogPosition >= PieceTable.DocumentEndLogPosition;
end;

function TdxPieceTableIterator.IsNewElement(const ACurrentPosition: TdxDocumentModelPosition): Boolean;
var
  APos: TdxDocumentModelPosition;
begin
  APos := ACurrentPosition;
  APos := MoveBack(APos);
  APos := MoveForward(APos);
  Result := APos = ACurrentPosition;
end;

function TdxPieceTableIterator.IsStartOfDocument(
  const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := APos.LogPosition = PieceTable.DocumentStartLogPosition;
end;

function TdxPieceTableIterator.MoveBack(
  const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if IsStartOfDocument(Result) then
    Exit;
  MoveBackCore(Result);
end;

function TdxPieceTableIterator.MoveForward(
  const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  if IsEndOfDocument(Result) then
    Exit;
  MoveForwardCore(Result);
end;

procedure TdxPieceTableIterator.UpdateModelPositionByLogPosition(
  var APos: TdxDocumentModelPosition);
var
  AParagraph: TdxParagraphBase;
  ALogPosition: TdxDocumentLogPosition;
  ARunStart: TdxDocumentLogPosition;
begin
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  ALogPosition := APos.LogPosition;
  if (ALogPosition >= APos.RunStartLogPosition) and (ALogPosition < APos.RunEndLogPosition) then
    Exit;
  while (ALogPosition > AParagraph.EndLogPosition) or (ALogPosition < AParagraph.LogPosition) do
  begin
    if ALogPosition < AParagraph.LogPosition then
      APos.ParagraphIndex := APos.ParagraphIndex - 1
    else
      APos.ParagraphIndex := APos.ParagraphIndex + 1;
    AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  end;
  APos.RunIndex := AParagraph.FirstRunIndex;
  ARunStart := AParagraph.LogPosition;
  while (ALogPosition >= ARunStart + PieceTable.Runs[APos.RunIndex].Length) do
  begin
    ARunStart := ARunStart + PieceTable.Runs[APos.RunIndex].Length;
    APos.RunIndex := APos.RunIndex + 1;
  end;
  APos.RunStartLogPosition := ARunStart;
end;

{ TdxWordsDocumentModelIteratorBase }

constructor TdxWordsDocumentModelIteratorBase.Create(
  APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FCachedRunIndex := -1;
end;

function TdxWordsDocumentModelIteratorBase.GetCharacter(const APos: TdxDocumentModelPosition): Char;
var
  AIndex: Integer;
begin
  if APos.RunIndex <> FCachedRunIndex then
  begin
    FCachedRunIndex := APos.RunIndex;
    FCachedRunText := PieceTable.GetRunNonEmptyText(FCachedRunIndex);
  end;
  AIndex := Min(APos.LogPosition, APos.PieceTable.DocumentEndLogPosition) - APos.RunStartLogPosition;
  if AIndex = Length(FCachedRunText) then
  begin
    if AIndex > 0 then
      Dec(AIndex);
  end;
  Result := FCachedRunText[AIndex + 1]
end;

procedure TdxWordsDocumentModelIteratorBase.SkipBackward(
  AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
  const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and APredicate(GetCharacter(APos)) do
    AIterator.MoveBackCore(APos);
end;

procedure TdxWordsDocumentModelIteratorBase.SkipForward(
  AIterator: TdxPieceTableIterator; var APos: TdxDocumentModelPosition;
  const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and APredicate(GetCharacter(APos)) do
    AIterator.MoveForwardCore(APos);
end;

{ TdxWordsDocumentModelIterator }

class function TdxWordsDocumentModelIterator.CreateNonWordSymbols: TdxCharList;
begin
  Result := TdxCharList.Create;
  PopulateSpacesList(Result);
  PopulateSpecialSymbolsList(Result);
  PopulateWordDelimiters(Result);
end;

class function TdxWordsDocumentModelIterator.CreateSpacesList: TdxCharList;
begin
  Result := TdxCharList.Create;
  PopulateSpacesList(Result);
end;

class function TdxWordsDocumentModelIterator.CreateSpecialSymbolsList: TdxCharList;
begin
  Result := TdxCharList.Create;
  PopulateSpecialSymbolsList(Result);
end;

class function TdxWordsDocumentModelIterator.CreateWordDelimiters: TdxCharList;
begin
  Result := TdxCharList.Create;
  PopulateWordDelimiters(Result);
end;

class destructor TdxWordsDocumentModelIterator.Finalize;
begin
  FreeAndNil(FNonWordSymbols);
  FreeAndNil(FSpaces);
  FreeAndNil(FSpecialSymbols);
  FreeAndNil(FWordsDelimiter);
end;

class constructor TdxWordsDocumentModelIterator.Initialize;
begin
  FNonWordSymbols := CreateNonWordSymbols;
  FSpaces := CreateSpacesList;
  FSpecialSymbols := CreateSpecialSymbolsList;
  FWordsDelimiter := CreateWordDelimiters;
end;

function TdxWordsDocumentModelIterator.IsInsideWord(
  const ACurrentPosition: TdxDocumentModelPosition): Boolean;
begin
  Assert(ACurrentPosition.IsValid);
  Result := not NonWordSymbols.Contains(GetCharacter(ACurrentPosition)) and
    not IsNewElement(ACurrentPosition);
end;

function TdxWordsDocumentModelIterator.IsAtWord(const ACurrentPosition: TdxDocumentModelPosition): Boolean;
begin
  Assert(ACurrentPosition.IsValid);
  Result := not NonWordSymbols.Contains(GetCharacter(ACurrentPosition));
end;

function TdxWordsDocumentModelIterator.IsNotNonWordsSymbols(
  const AItem: Char): Boolean;
begin
  Result := not NonWordSymbols.Contains(AItem);
end;

function TdxWordsDocumentModelIterator.IsSpace(const AItem: Char): Boolean;
begin
  Result := Spaces.Contains(AItem);
end;

function TdxWordsDocumentModelIterator.IsWordsDelimiter(
  const AItem: Char): Boolean;
begin
  Result := WordsDelimiter.Contains(AItem);
end;

procedure TdxWordsDocumentModelIterator.MoveBackCore(
  var APos: TdxDocumentModelPosition);
var
  AIterator: TdxCharactersDocumentModelIterator;
begin
  AIterator := TdxCharactersDocumentModelIterator.Create(PieceTable);
  try
    AIterator.MoveBackCore(APos);
    if SpecialSymbols.Contains(GetCharacter(APos)) then
      Exit;
    SkipBackward(AIterator, APos, IsSpace);
    if SpecialSymbols.Contains(GetCharacter(APos)) then
    begin
      if not IsStartOfDocument(APos) then
        AIterator.MoveForwardCore(APos);
      Exit;
    end;
    if WordsDelimiter.Contains(GetCharacter(APos)) then
    begin
      SkipBackward(AIterator, APos, IsWordsDelimiter);
      if not IsStartOfDocument(APos) or not WordsDelimiter.Contains(GetCharacter(APos)) then
        AIterator.MoveForwardCore(APos);
    end
    else
    begin
      SkipBackward(AIterator, APos, IsNotNonWordsSymbols);
      if not IsStartOfDocument(APos) or NonWordSymbols.Contains(GetCharacter(APos)) then
        AIterator.MoveForwardCore(APos);
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxWordsDocumentModelIterator.MoveForwardCore(
  var APos: TdxDocumentModelPosition);
var
  AIterator: TdxCharactersDocumentModelIterator;
begin
  AIterator := TdxCharactersDocumentModelIterator.Create(PieceTable);
  try
    if SpecialSymbols.Contains(GetCharacter(APos)) then
    begin
      AIterator.MoveForwardCore(APos);
      Exit;
    end;
    if WordsDelimiter.Contains(GetCharacter(APos)) then
      SkipForward(AIterator, APos, IsWordsDelimiter)
    else
      SkipForward(AIterator, APos, IsNotNonWordsSymbols);
    SkipForward(AIterator, APos, IsSpace);
  finally
    AIterator.Free;
  end;
end;

class procedure TdxWordsDocumentModelIterator.PopulateSpacesList(
  AList: TdxCharList);
begin
  AList.Add(TdxCharacters.Space);
  AList.Add(TdxCharacters.NonBreakingSpace);
  AList.Add(TdxCharacters.EnSpace);
  AList.Add(TdxCharacters.EmSpace);
  AList.Add(TdxCharacters.QmSpace);
end;

class procedure TdxWordsDocumentModelIterator.PopulateSpecialSymbolsList(
  AList: TdxCharList);
begin
  AList.Add(TdxCharacters.PageBreak);
  AList.Add(TdxCharacters.LineBreak);
  AList.Add(TdxCharacters.ColumnBreak);
  AList.Add(TdxCharacters.SectionMark);
  AList.Add(TdxCharacters.ParagraphMark);
  AList.Add(TdxCharacters.TabMark);
end;

class procedure TdxWordsDocumentModelIterator.PopulateWordDelimiters(
  AList: TdxCharList);
begin
  AList.Add(TdxCharacters.Dot);
  AList.Add(',');
  AList.Add('!');
  AList.Add('@');
  AList.Add('#');
  AList.Add('$');
  AList.Add('%');
  AList.Add('^');
  AList.Add('&');
  AList.Add('*');
  AList.Add('(');
  AList.Add(')');
  AList.Add(TdxCharacters.Dash);
  AList.Add(TdxCharacters.EmDash);
  AList.Add(TdxCharacters.EnDash);
  AList.Add(TdxCharacters.Hyphen);
  AList.Add(TdxCharacters.Underscore);
  AList.Add('=');
  AList.Add('+');
  AList.Add('[');
  AList.Add(']');
  AList.Add('{');
  AList.Add('}');
  AList.Add('\');
  AList.Add('|');
  AList.Add(';');
  AList.Add(':');
  AList.Add('''');
  AList.Add('"');
  AList.Add('<');
  AList.Add('>');
  AList.Add('/');
  AList.Add('?');
  AList.Add('`');
  AList.Add('~');
  AList.Add(TdxCharacters.TrademarkSymbol);
  AList.Add(TdxCharacters.CopyrightSymbol);
  AList.Add(TdxCharacters.RegisteredTrademarkSymbol);
  AList.Add(TdxCharacters.Ellipsis);
  AList.Add(TdxCharacters.LeftDoubleQuote);
  AList.Add(TdxCharacters.LeftSingleQuote);
  AList.Add(TdxCharacters.RightDoubleQuote);
  AList.Add(TdxCharacters.OpeningDoubleQuotationMark);
  AList.Add(TdxCharacters.OpeningSingleQuotationMark);
  AList.Add(TdxCharacters.ClosingDoubleQuotationMark);
  AList.Add(TdxCharacters.ClosingSingleQuotationMark);
end;

{ TdxVisibleWordsIterator }

constructor TdxVisibleWordsIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FFilter := APieceTable.VisibleTextFilter;
end;

procedure TdxVisibleWordsIterator.SkipForward(AIterator: TdxPieceTableIterator;
  var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and
      (APredicate(GetCharacter(APos)) or not FFilter.IsRunVisible(APos.RunIndex)) do
    AIterator.MoveForwardCore(APos);
end;

procedure TdxVisibleWordsIterator.SkipBackward(AIterator: TdxPieceTableIterator;
  var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and
      (APredicate(GetCharacter(APos)) or not FFilter.IsRunVisible(APos.RunIndex)) do
    AIterator.MoveBackCore(APos);
end;

{ TdxCharactersDocumentModelIterator }

procedure TdxCharactersDocumentModelIterator.MoveBackCore(
  var APos: TdxDocumentModelPosition);
begin
  SkipBackward(APos);
  UpdateModelPositionByLogPosition(APos);
end;

procedure TdxCharactersDocumentModelIterator.MoveForwardCore(
  var APos: TdxDocumentModelPosition);
begin
  SkipForward(APos);
  UpdateModelPositionByLogPosition(APos);
end;

procedure TdxCharactersDocumentModelIterator.SkipBackward(
  var APos: TdxDocumentModelPosition);
begin
  APos.LogPosition := APos.LogPosition - 1;
end;

procedure TdxCharactersDocumentModelIterator.SkipForward(
  var APos: TdxDocumentModelPosition);
begin
  APos.LogPosition := APos.LogPosition + 1;
end;

{ TdxVisibleCharactersStopAtFieldsDocumentModelIterator }

constructor TdxVisibleCharactersStopAtFieldsDocumentModelIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FFilter := APieceTable.VisibleTextFilter;
end;

function TdxVisibleCharactersStopAtFieldsDocumentModelIterator.IsFieldDelimiter(const APos: TdxDocumentModelPosition): Boolean;
var
  ARun: TdxRunBase;
begin
  ARun := APos.PieceTable.Runs[APos.RunIndex];
  Result := (ARun is TdxFieldCodeRunBase) or (ARun is TdxFieldResultEndRun);
end;

procedure TdxVisibleCharactersStopAtFieldsDocumentModelIterator.SkipForward(var APos: TdxDocumentModelPosition);
begin
  if IsFieldDelimiter(APos) then
    Exit;

  repeat
    inherited SkipForward(APos);
    if IsFieldDelimiter(APos) then
    begin
      inherited SkipBackward(APos);
      Exit;
    end;
  until IsEndOfDocument(APos) or FFilter.IsRunVisible(APos.RunIndex);
end;

procedure TdxVisibleCharactersStopAtFieldsDocumentModelIterator.SkipBackward(var APos: TdxDocumentModelPosition);
begin
  if IsFieldDelimiter(APos) then
    Exit;

  repeat
    inherited SkipBackward(APos);
    if IsFieldDelimiter(APos) then
    begin
      inherited SkipForward(APos);
      Exit;
    end;
  until IsStartOfDocument(APos) or FFilter.IsRunVisible(APos.RunIndex);
end;

{ TdxParagraphsDocumentModelIterator }

procedure TdxParagraphsDocumentModelIterator.MoveBackCore(
  var APos: TdxDocumentModelPosition);
var
  AParagraph: TdxParagraphBase;
begin
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  if (APos.LogPosition = AParagraph.LogPosition) and (APos.ParagraphIndex > 0) then
    APos.ParagraphIndex := APos.ParagraphIndex - 1;
  AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
  APos.LogPosition := AParagraph.LogPosition;
  APos.RunStartLogPosition := APos.LogPosition;
  APos.RunIndex := AParagraph.FirstRunIndex;
end;

procedure TdxParagraphsDocumentModelIterator.MoveForwardCore(
  var APos: TdxDocumentModelPosition);
var
  ALastParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxParagraphBase;
  ALastParagraph: TdxParagraphBase;
begin
  ALastParagraphIndex := PieceTable.Paragraphs.Last.Index;
  if APos.ParagraphIndex > ALastParagraphIndex then
    Exit;
  APos.ParagraphIndex := APos.ParagraphIndex + 1;
  if APos.ParagraphIndex <= ALastParagraphIndex then
  begin
    AParagraph := PieceTable.Paragraphs[APos.ParagraphIndex];
    APos.LogPosition := AParagraph.LogPosition;
    APos.RunIndex := AParagraph.FirstRunIndex;
  end
  else
  begin
    ALastParagraph := PieceTable.Paragraphs.Last;
    APos.LogPosition := ALastParagraph.EndLogPosition + 1;
    APos.RunIndex := ALastParagraph.LastRunIndex + 1;
  end;
  APos.RunStartLogPosition := APos.LogPosition;
end;

{ TdxSpellCheckerWordIterator }

class constructor TdxSpellCheckerWordIterator.Initialize;
begin
  WordsSeparators := CreateSeparatorsTable;
  InWordSymbols := CreateInWordSymbolsTable;
end;

class destructor TdxSpellCheckerWordIterator.Finalize;
begin
  FreeAndNil(WordsSeparators);
  FreeAndNil(InWordSymbols);
end;

constructor TdxSpellCheckerWordIterator.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FCachedRunIndex := -1;
end;

class function TdxSpellCheckerWordIterator.CreateSeparatorsTable: TdxCharList;
begin
  Result := TdxCharList.Create;
  Result.Add(TdxCharacters.Space);
  Result.Add(TdxCharacters.TabMark);
  Result.Add(#10);
  Result.Add(TdxCharacters.ParagraphMark);
  Result.Add('(');
  Result.Add(')');
  Result.Add('[');
  Result.Add(']');
  Result.Add('{');
  Result.Add('}');
  Result.Add('<');
  Result.Add('>');
  Result.Add('/');
  Result.Add('\');

  Result.Add(TdxCharacters.ObjectMark);
  Result.Add(TdxCharacters.NonBreakingSpace);
  Result.Add(TdxCharacters.EnSpace);
  Result.Add(TdxCharacters.EmSpace);
  Result.Add(TdxCharacters.QmSpace);
  Result.Add(TdxCharacters.LineBreak);
  Result.Add(TdxCharacters.ColumnBreak);
  Result.Add(TdxCharacters.SectionMark);
  Result.Add(TdxCharacters.PageBreak);
end;

class function TdxSpellCheckerWordIterator.CreateInWordSymbolsTable: TdxCharList;
begin
  Result := TdxCharList.Create;
  Result.Add('''');
  Result.Add(',');
  Result.Add(#8216);
  Result.Add('`');
  Result.Add('_');
end;

procedure TdxSpellCheckerWordIterator.SkipBackwardInvisibleRuns(var APos: TdxDocumentModelPosition);
begin
  if not VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    MoveToPrevChar(APos);
end;

procedure TdxSpellCheckerWordIterator.SkipForwardInvisibleRuns(var APos: TdxDocumentModelPosition);
begin
  if not VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    MoveToNextChar(APos);
end;

function TdxSpellCheckerWordIterator.GetVisibleTextFilter: IdxVisibleTextFilter;
begin
  Result := PieceTable.VisibleTextFilter;
end;

procedure TdxSpellCheckerWordIterator.SkipForward(var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsEndOfDocument(APos) and APredicate(GetCharacter(APos)) do
    MoveToNextChar(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToNextChar(var APos: TdxDocumentModelPosition);
var
  ARuns: TdxTextRunCollection;
  AEndRunIndex: TdxRunIndex;
begin
  if IsEndOfDocument(APos) then
    Exit;
  APos.LogPosition := APos.LogPosition + 1;
  if APos.LogPosition <= APos.RunEndLogPosition then
    Exit;

  ARuns := PieceTable.Runs;
  AEndRunIndex := ARuns.Count - 1;
  while MoveToNextRun(APos, APos.LogPosition) and
      (APos.RunIndex < AEndRunIndex) and not VisibleTextFilter.IsRunVisible(APos.RunIndex) do
    APos.LogPosition := APos.LogPosition + ARuns[APos.RunIndex].Length;
end;

function TdxSpellCheckerWordIterator.MoveToNextRun(var APos: TdxDocumentModelPosition; ARunStartPos: TdxDocumentLogPosition): Boolean;
begin
  Result := False;
  if IsEndOfDocument(APos) then
    Exit;
  Result := True;
  APos.RunIndex := APos.RunIndex + 1;
  APos.ParagraphIndex := PieceTable.Runs[APos.RunIndex].Paragraph.Index;
  APos.RunStartLogPosition := ARunStartPos;
end;

procedure TdxSpellCheckerWordIterator.SkipBackward(var APos: TdxDocumentModelPosition; const APredicate: TdxPredicate<Char>);
begin
  while not IsStartOfDocument(APos) and APredicate(GetCharacter(APos)) do
    MoveToPrevChar(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevChar(var APos: TdxDocumentModelPosition);
var
  ARuns: TdxTextRunCollection;
  AFirstRunIndex: TdxRunIndex;
begin
  APos.LogPosition := APos.LogPosition - 1;
  if APos.LogPosition >= APos.RunStartLogPosition then
    Exit;

  ARuns := PieceTable.Runs;
  MoveToPrevRun(APos, APos.LogPosition);
  AFirstRunIndex := 0;
  while (APos.RunIndex > AFirstRunIndex) and not VisibleTextFilter.IsRunVisible(APos.RunIndex) do
  begin
    APos.LogPosition := APos.RunEndLogPosition - ARuns[APos.RunIndex].Length;
    MoveToPrevRun(APos, APos.LogPosition);
  end;
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevRun(var APos: TdxDocumentModelPosition; ARunEndPos: TdxDocumentLogPosition);
var
  ARun: TdxTextRunBase;
begin
  APos.RunIndex := APos.RunIndex - 1;
  ARun := PieceTable.Runs[APos.RunIndex];
  APos.ParagraphIndex := ARun.Paragraph.Index;
  APos.RunStartLogPosition := ARunEndPos - ARun.Length + 1;
end;

procedure TdxSpellCheckerWordIterator.MoveForwardCore(var APos: TdxDocumentModelPosition);
var
  APosClone: TdxDocumentModelPosition;
begin
  APosClone := APos;
  SkipForwardInvisibleRuns(APosClone);
  if not IsLetterOrDigit(GetCharacter(APosClone)) then
    SkipForward(APosClone, IsNotLetterOrDigit);
  if IsEndOfDocument(APosClone) then
    Exit;
  APos.CopyFrom(APosClone);
  MoveToWordEndCore(APos);
end;

function TdxSpellCheckerWordIterator.GetCharacter(const APos: TdxDocumentModelPosition): Char;
begin
  if APos.RunIndex <> FCachedRunIndex then
  begin
    FCachedRunIndex := APos.RunIndex;
    FCachedRunText := PieceTable.GetRunNonEmptyText(FCachedRunIndex);
  end;
  Result := FCachedRunText[APos.RunOffset + 1];
end;

procedure TdxSpellCheckerWordIterator.MoveBackCore(var APos: TdxDocumentModelPosition);
begin
  SkipBackwardInvisibleRuns(APos);
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipBackward(APos, IsNotLetterOrDigit);
  SkipBackward(APos, IsNotWordsSeparators);
  SkipBackward(APos, IsNotLetterOrDigit);
  if IsStartOfDocument(APos) then
    Exit;
  MoveToWordStartCore(APos);
  MoveToWordEndCore(APos);
end;

function TdxSpellCheckerWordIterator.MoveToWordEnd(const APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
begin
  Result := APos;
  SkipForwardInvisibleRuns(Result);
  if IsEndOfDocument(Result) then
    Exit(Result);
  MoveToWordEndCore(Result);
end;

procedure TdxSpellCheckerWordIterator.MoveToWordEndCore(var APos: TdxDocumentModelPosition;
  AWordWithDots: Boolean = False);
var
  ANewPos: TdxDocumentModelPosition;
  AIsLastCharDot: Boolean;
begin
  SkipForward(APos, IsLetterOrDigit);
  if IsWordsSeparators(GetCharacter(APos)) or IsEndOfDocument(APos) then
    Exit;

  ANewPos := APos;
  AIsLastCharDot := GetCharacter(ANewPos) = '.';
  MoveToNextChar(ANewPos);
  if not IsLetterOrDigit(GetCharacter(ANewPos)) then
  begin
    if (AWordWithDots and AIsLastCharDot) and not IsEndOfDocument(ANewPos) then
      APos.CopyFrom(ANewPos);
    Exit;
  end;
  APos.CopyFrom(ANewPos);
  MoveToWordEndCore(APos, AWordWithDots or AIsLastCharDot);
end;

function TdxSpellCheckerWordIterator.MoveToWordStart(const APos: TdxDocumentModelPosition; ASkipNotLetterOrDigitChar: Boolean = True): TdxDocumentModelPosition;
begin
  Result := APos;
  SkipBackwardInvisibleRuns(Result);
  if not IsLetterOrDigit(GetCharacter(Result)) then
  begin
    if not ASkipNotLetterOrDigitChar then
      Exit(Result);
    SkipBackward(Result, IsNotLetterOrDigit);
  end;
  MoveToWordStartCore(Result);
end;

procedure TdxSpellCheckerWordIterator.MoveToWordStartCore(var APos: TdxDocumentModelPosition);
var
  ANewPos: TdxDocumentModelPosition;
begin
  SkipBackward(APos, IsLetterOrDigit);
  if IsWordsSeparators(GetCharacter(APos)) then
  begin
    MoveToNextChar(APos);
    Exit;
  end;
  if IsStartOfDocument(APos) then
    Exit;
  ANewPos := APos;
  MoveToPrevChar(ANewPos);
  if not IsLetterOrDigit(GetCharacter(ANewPos)) then
  begin
    MoveToNextChar(APos);
    Exit;
  end;
  APos.CopyFrom(ANewPos);
  MoveToWordStartCore(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToNextWordEnd(var APos: TdxDocumentModelPosition);
begin
  SkipForwardInvisibleRuns(APos);
  if IsEndOfDocument(APos) then
    Exit;
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipForward(APos, IsNotLetterOrDigit);
  SkipForward(APos, IsNotWordsSeparators);
  SkipForward(APos, IsNotLetterOrDigit);
  MoveToWordEndCore(APos);
end;

procedure TdxSpellCheckerWordIterator.MoveToPrevWordStart(var APos: TdxDocumentModelPosition);
begin
  SkipBackwardInvisibleRuns(APos);
  if IsStartOfDocument(APos) then
    Exit;
  if not IsLetterOrDigit(GetCharacter(APos)) then
    SkipBackward(APos, IsNotLetterOrDigit);
  SkipBackward(APos, IsNotWordsSeparators);
  SkipBackward(APos, IsNotLetterOrDigit);
  MoveToWordStartCore(APos);
end;

function TdxSpellCheckerWordIterator.IsNotWordsSeparators(const ACh: Char): Boolean;
begin
  Result := not IsWordsSeparators(ACh);
end;

function TdxSpellCheckerWordIterator.IsWordsSeparators(const ACh: Char): Boolean;
begin
  Result := WordsSeparators.Contains(ACh);
end;

function TdxSpellCheckerWordIterator.IsNotLetterOrDigit(const ACh: Char): Boolean;
begin
  Result := not IsLetterOrDigit(ACh);
end;

function TdxSpellCheckerWordIterator.IsLetterOrDigit(const ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(ACh){$ENDIF};
end;

end.
