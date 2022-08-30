{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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
unit cxRegExpr;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, dxCore, cxClasses, cxEdit, cxEditConsts;

type
  { TcxRegExprError }

  TcxRegExprError = class
  private
    FChar: Integer;
    FLine: Integer;
    FMessage: string;
    function GetFullMessage: string;
  public
    constructor Create(ALine: Integer; AChar: Integer; AMessage: string);
    function Clone: TcxRegExprError;
    property Char: Integer read FChar;
    property FullMessage: string read GetFullMessage;
    property Line: Integer read FLine;
    property Message: string read FMessage;
  end;

  { TcxRegExprErrors }

  TcxRegExprErrors = class
  private
    FErrors: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TcxRegExprError;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AError: TcxRegExprError);
    procedure Clear;
    function Clone: TcxRegExprErrors;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxRegExprError read GetItems; default;
  end;

  { EcxRegExprError }

  EcxRegExprError = class(EcxEditError)
  private
    FErrors: TcxRegExprErrors;
  public
    constructor Create(AErrors: TcxRegExprErrors);
    property Errors: TcxRegExprErrors read FErrors;
  end;

  { TcxRegExprLexemCode }

  TcxRegExprLexemCode =
  (
    relcSymbol,
    relcSpecial,
    relcInteger,
    relcAll,
    relcId,
    relcNotId,
    relcDigit,
    relcNotDigit,
    relcSpace,
    relcNotSpace,
    relcReference,
    relcDateSeparator,
    relcTimeSeparator
  );

  { TcxLexem }

  TcxLexem = record
    Char: Integer;
    Code: TcxRegExprLexemCode;
    Line: Integer;
    Value: string;
  end;
  PcxLexem = ^TcxLexem;

  { TcxLexems }

  TcxLexems = class
  private
    FLexems: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TcxLexem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ALexem: TcxLexem);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxLexem read GetItems; default;
  end;

  { TcxRegExprItem }

  TcxRegExprItem = class
  public
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; virtual; abstract;
    function Clone: TcxRegExprItem; virtual; abstract;
  end;

  { TcxRegExprSymbol }

  TcxRegExprSymbol = class(TcxRegExprItem)
  private
    FValue: Char;
  public
    constructor Create(AValue: Char);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprTimeSeparator }

  TcxRegExprTimeSeparator = class(TcxRegExprItem)
  public
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
    function Value: Char;
  end;

  { TcxRegExprDateSeparator }

  TcxRegExprDateSeparator = class(TcxRegExprItem)
  public
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
    function Value: Char;
  end;

  { TcxRegExprSubrange }

  TcxRegExprSubrange = class(TcxRegExprItem)
  private
    FStartValue: Char;
    FFinishValue: Char;
  public
    constructor Create(AStartValue: Char; AFinishValue: Char);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprEnumeration }

  TcxRegExprEnumeration = class(TcxRegExprItem)
  private
    FInverse: Boolean;
  public
    constructor Create(AInverse: Boolean = False);
  end;

  { TcxRegExprUserEnumeration }

  TcxRegExprUserEnumeration = class(TcxRegExprEnumeration)
  private
    FItems: TList;
    function Item(AIndex: Integer): TcxRegExprItem;
  public
    constructor Create(AInverse: Boolean = False);
    destructor Destroy; override;
    procedure Add(AItem: TcxRegExprItem);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprDigit }

  TcxRegExprDigit = class(TcxRegExprEnumeration)
  public
    constructor Create(AInverse: Boolean = False);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprIdLetter }

  TcxRegExprIdLetter = class(TcxRegExprEnumeration)
  public
    constructor Create(AInverse: Boolean = False);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprSpace }

  TcxRegExprSpace = class(TcxRegExprEnumeration)
  public
    constructor Create(AInverse: Boolean = False);
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  { TcxRegExprAll }

  TcxRegExprAll = class(TcxRegExprItem)
  public
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean; override;
    function Clone: TcxRegExprItem; override;
  end;

  TcxRegExprStates = class;

  { TcxRegExprState }

  TcxRegExprState = class
  protected
    FStates: TcxRegExprStates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AState: TcxRegExprState); overload;
    procedure Add(AStates: TcxRegExprStates); overload;
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates; virtual;
    function Clone: TcxRegExprState; virtual;
    function GetAllNextStates: TcxRegExprStates;
    function GetSelf: TcxRegExprStates; virtual;
    function Next(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
    property States: TcxRegExprStates read FStates;
  end;

  { TcxRegExprSimpleState }

  TcxRegExprSimpleState = class(TcxRegExprState)
  private
    FIsFinal: Boolean;
    FValue: TcxRegExprItem;
  public
    constructor Create(AValue: TcxRegExprItem);
    destructor Destroy; override;
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates; override;
    function Clone: TcxRegExprState; override;
    function GetSelf: TcxRegExprStates; override;
    procedure SetFinal;
    //
    property IsFinal: Boolean read FIsFinal;
    property Value: TcxRegExprItem read FValue;
  end;

  { TcxRegExprBlockState }

  TcxRegExprBlockState = class(TcxRegExprState)
  public
    function Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates; override;
    function Clone: TcxRegExprState; override;
    function GetSelf: TcxRegExprStates; override;
  end;

  { TcxRegExprStates }

  TcxRegExprStates = class
  private
    FStates: TList;
    function GetCount: Integer;
    function GetState(AIndex: Integer): TcxRegExprState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AState: TcxRegExprState); overload;
    procedure Add(AStates: TcxRegExprStates); overload;
    procedure Clear;
    function Equ(var ASymbol: Char): Boolean;
    function GetAllNextStates: TcxRegExprStates;
    function IsFinal: Boolean;
    function Next(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
    property Count: Integer read GetCount;
    property State[AIndex: Integer]: TcxRegExprState read GetState; default;
  end;

  TcxRegExprParserAlts = class;
  TcxRegExpr = class;

  { TcxRegExprAutomat }

  TcxRegExprAutomat = class
  protected
    FOwner: TcxRegExpr;
    FCurrentStates: TcxRegExprStates;
    FExpr: TcxRegExprParserAlts;
    FHistory: TList;
    FStartState: TcxRegExprSimpleState;
    function GetAllNextStates: TcxRegExprStates;
    function Pop: TcxRegExprStates;
    procedure Push(AStates: TcxRegExprStates);
  public
    constructor Create(AExpr: TcxRegExprParserAlts; AOwner: TcxRegExpr);
    destructor Destroy; override;
    function IsFinal: Boolean;
    function IsStart: Boolean;
    function Next(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
    function Prev: Boolean;
    function Print: string;
    procedure Reset;
    procedure ReUpdate;
    procedure Update;
  end;

  { TcxRegExprQuantifier }

  TcxRegExprQuantifier = class
  public
    function CanMissing: Boolean; virtual; abstract;
    function CanRepeat: Boolean; virtual; abstract;
    function Clone: TcxRegExprQuantifier; virtual; abstract;
    function Print: string; virtual; abstract;
  end;

  { TcxRegExprSimpleQuantifier }

  TcxRegExprSimpleQuantifier = class(TcxRegExprQuantifier) // missing quantifier
  public
    function CanMissing: Boolean; override;
    function CanRepeat: Boolean; override;
    function Clone: TcxRegExprQuantifier; override;
    function Print: string; override;
  end;

  { TcxRegExprQuestionQuantifier }

  TcxRegExprQuestionQuantifier = class(TcxRegExprQuantifier) // ?
  public
    function CanMissing: Boolean; override;
    function CanRepeat: Boolean; override;
    function Clone: TcxRegExprQuantifier; override;
    function Print: string; override;
  end;

  { TcxRegExprStarQuantifier }

  TcxRegExprStarQuantifier = class(TcxRegExprQuantifier) // *
  public
    function CanMissing: Boolean; override;
    function CanRepeat: Boolean; override;
    function Clone: TcxRegExprQuantifier; override;
    function Print: string; override;
  end;

  { TcxRegExprPlusQuantifier }

  TcxRegExprPlusQuantifier = class(TcxRegExprQuantifier) // +
  public
    function CanMissing: Boolean; override;
    function CanRepeat: Boolean; override;
    function Clone: TcxRegExprQuantifier; override;
    function Print: string; override;
  end;

  { TcxRegExprParserItem }

  TcxRegExprParserItem = class
  private
    FQuantifier: TcxRegExprQuantifier;
  public
    constructor Create(AQuantifier: TcxRegExprQuantifier = nil);
    destructor Destroy; override;
    function CanEmpty: Boolean; virtual; abstract;
    function CanMissing: Boolean;
    function CanRepeat: Boolean;
    function Clone: TcxRegExprParserItem; virtual; abstract;
    function NotQuantifier: Boolean;
    function Print: string; virtual; abstract;
    procedure SetFinal; virtual; abstract;
    procedure SetQuantifier(AQuantifier: TcxRegExprQuantifier);
  end;

  { TcxRegExprParserSimpleItem }

  TcxRegExprParserSimpleItem = class(TcxRegExprParserItem)
  private
    FState: TcxRegExprState;
  public
    constructor Create(AState: TcxRegExprState; AQuantifier: TcxRegExprQuantifier = nil);
    destructor Destroy; override;
    function CanEmpty: Boolean; override;
    function Clone: TcxRegExprParserItem; override;
    function Print: string; override;
    procedure SetFinal; override;
    property State: TcxRegExprState read FState;
  end;

  TcxRegExprParserAlt = class;

  { TcxRegExprParserBlockItem }

  TcxRegExprParserBlockItem = class(TcxRegExprParserItem)
  private
    FAlts: TcxRegExprParserAlts;
    FFinishState: TcxRegExprState;
    FStartState: TcxRegExprState;
  public
    constructor Create(AQuantifier: TcxRegExprQuantifier = nil);
    destructor Destroy; override;
    function CanEmpty: Boolean; override;
    procedure CreateConnections;
    procedure AddAlt(AAlt: TcxRegExprParserAlt);
    procedure AddAlts(AAlts: TcxRegExprParserAlts);
    function Clone: TcxRegExprParserItem; override;
    function Print: string; override;
    procedure SetFinal; override;
    property Alts: TcxRegExprParserAlts read FAlts;
    property FinishState: TcxRegExprState read FFinishState;
    property StartState: TcxRegExprState read FStartState;
  end;

  { TcxRegExprParserAlt }

  TcxRegExprParserAlt = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetFirstItem: TcxRegExprParserItem;
    function GetItem(AIndex: Integer): TcxRegExprParserItem;
    function GetLastItem: TcxRegExprParserItem;
    procedure SetLastItem(AItem: TcxRegExprParserItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AItem: TcxRegExprParserItem);
    function CanEmpty: Boolean;
    function CanMissing: Boolean;
    function Clone: TcxRegExprParserAlt;
    procedure CreateConnections;
    procedure CreateFinalStates;
    function GetStartConnections: TcxRegExprStates;
    function Print: string;
    procedure SetFinishConnection(AFinishState: TcxRegExprState);
    property Count: Integer read GetCount;
    property FirstItem: TcxRegExprParserItem read GetFirstItem;
    property Item[AIndex: Integer]: TcxRegExprParserItem read GetItem; default;
    property LastItem: TcxRegExprParserItem read GetLastItem write SetLastItem;
  end;

  { TcxRegExprParserAlts }

  TcxRegExprParserAlts = class
  private
    FAlts: TList;
    function GetAlt(AIndex: Integer): TcxRegExprParserAlt;
    function GetCount: Integer;
    function GetLastAlt: TcxRegExprParserAlt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AAlt: TcxRegExprParserAlt);
    procedure AddAlt;
    function CanEmpty: Boolean;
    procedure CreateConnections;
    procedure CreateFinalStates;
    function Clone: TcxRegExprParserAlts;
    function GetStartConnections: TcxRegExprStates;
    function Print: string;
    procedure SetFinishConnections(AFinishState: TcxRegExprState);
    function StartStateIsFinal: Boolean;
    function ThereIsEmptyAlt: Boolean;
    property Alt[AIndex: Integer]: TcxRegExprParserAlt read GetAlt; default;
    property Count: Integer read GetCount;
    property LastAlt: TcxRegExprParserAlt read GetLastAlt;
  end;

  TcxSymbolDeleteEvent = procedure of object;
  TcxSymbolUpdateEvent = procedure(ASymbol: Char) of object;

  { TcxRegExpr }

  TcxRegExpr = class
  private
    FAutomat: TcxRegExprAutomat;
    FBlocks: TList;
    FChar: Integer;
    FCaseInsensitive: Boolean;
    FCompiled: Boolean;
    FErrors: TcxRegExprErrors;
    FFirstExpr: Boolean;
    FIndex: Integer;
    FLexemIndex: Integer;
    FLexems: TcxLexems;
    FLine: Integer;
    FOnSymbolDelete: TcxSymbolDeleteEvent;
    FOnSymbolUpdate: TcxSymbolUpdateEvent;
    FStream: TStringStream;
    FUpdateOn: Boolean;
    procedure Clear;
    function Decimal(AToken: Char): Boolean;
    function EmptyStream: Boolean;
    function CreateLexem(ALine: Integer; AChar: Integer; ACode: TcxRegExprLexemCode; AValue: string): TcxLexem;
    function GetLexem(var ALexem: TcxLexem): Boolean;
    function GetToken(out AToken: Char): Boolean;
    function GetStream: TStream;
    function Hexadecimal(AToken: Char): Boolean;
    function LookToken(out AToken: Char; APtr: Integer): Boolean;
    function ParseAlt(AAlt: TcxRegExprParserAlt; Global: Boolean = True): Boolean;
    function ParseBlock: TcxRegExprParserBlockItem;
    function ParseEnumeration: TcxRegExprParserSimpleItem;
    procedure ParseExpr;
    procedure ParseQuantifier(var A: Integer; var B: Integer);
    procedure ScanASCII(ALine: Integer; AChar: Integer);
    procedure ScanClass;
    procedure ScanExpr;
    procedure ScanEscape(ALine: Integer; AChar: Integer);
    function ScanInteger(ALine: Integer; AChar: Integer; var AToken: Char): Boolean;
    procedure ScanQuantifier;
    procedure ScanString;
    procedure SetUpdateOn(AUpdateOn: Boolean);
    function Space(AToken: Char): Boolean;
    procedure SymbolDelete;
    procedure SymbolUpdate(ASymbol: Char);
    procedure TestCompiledStatus;
  protected
    property Automat: TcxRegExprAutomat read FAutomat;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(AStream: TStream); overload;
    procedure Compile(const S: string); overload;
    function IsCompiled: Boolean;
    function IsFinal: Boolean;
    function IsStart: Boolean;
    function Next(var AToken: Char): Boolean;
    function NextEx(const AString: string): string;
    function Prev: Boolean;
    function Print: string;
    procedure Reset;

    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive;
    property Stream: TStream read GetStream;
    property UpdateOn: Boolean read FUpdateOn write SetUpdateOn;
    property OnSymbolDelete: TcxSymbolDeleteEvent read FOnSymbolDelete write FOnSymbolDelete;
    property OnSymbolUpdate: TcxSymbolUpdateEvent read FOnSymbolUpdate write FOnSymbolUpdate;
  end;

  function IsTextFullValid(const AText, AMask: string): Boolean;
  function IsTextValid(const AText, AMask: string): Boolean;

implementation

uses
  cxFormats, cxEditUtils;

{ TcxRegExprError }

constructor TcxRegExprError.Create(ALine, AChar: Integer; AMessage: string);
begin
  inherited Create;
  FLine := ALine;
  FChar := AChar;
  FMessage := AMessage;
end;

function TcxRegExprError.Clone: TcxRegExprError;
begin
  Result := TcxRegExprError.Create(FLine, FChar, FMessage);
end;

function TcxRegExprError.GetFullMessage: string;
begin
  Result := '';
  if FLine > 0 then
  begin
    Result := Result + cxGetResourceString(@scxRegExprLine) + IntToStr(FLine);
    if FChar > 0 then
      Result := Result + ', ' + cxGetResourceString(@scxRegExprChar) + IntToStr(FChar);
    Result := Result + ': ';
  end;
  Result := Result + FMessage;
end;

{ TcxRegExprErrors }

constructor TcxRegExprErrors.Create;
begin
  inherited Create;
  FErrors := TList.Create;
end;

destructor TcxRegExprErrors.Destroy;
begin
  Clear;
  FErrors.Free;
  inherited Destroy;
end;

procedure TcxRegExprErrors.Add(AError: TcxRegExprError);
begin
  FErrors.Add(AError);
end;

procedure TcxRegExprErrors.Clear;
var
  I: Integer;
begin
  for I := 0 to FErrors.Count - 1 do
    TcxRegExprError(FErrors[I]).Free;
  FErrors.Clear;
end;

function TcxRegExprErrors.Clone: TcxRegExprErrors;
var
  I: Integer;
begin
  Result := TcxRegExprErrors.Create;

  for I := 0 to Count - 1 do
    Result.Add(Items[I].Clone);
end;

function TcxRegExprErrors.GetCount: Integer;
begin
  Result := FErrors.Count;
end;

function TcxRegExprErrors.GetItems(Index: Integer): TcxRegExprError;
begin
  Result := TcxRegExprError(FErrors[Index]);
end;

{ EcxRegExprError }

constructor EcxRegExprError.Create(AErrors: TcxRegExprErrors);
begin
  FErrors := AErrors;
end;

{ TcxLexems }

constructor TcxLexems.Create;
begin
  inherited Create;
  FLexems := TList.Create;
end;

destructor TcxLexems.Destroy;
begin
  Clear;
  FLexems.Free;
  inherited Destroy;
end;

procedure TcxLexems.Add(ALexem: TcxLexem);
var
  LexemP: PcxLexem;
begin
  New(LexemP);
  LexemP^ := ALexem;
  FLexems.Add(LexemP);
end;

procedure TcxLexems.Clear;
var
  I: Integer;
begin
  for I := 0 to FLexems.Count - 1 do
    Dispose(PcxLexem(FLexems[I]));
  FLexems.Clear;
end;

function TcxLexems.GetCount: Integer;
begin
  Result := FLexems.Count;
end;

function TcxLexems.GetItems(Index: Integer): TcxLexem;
begin
  Result := PcxLexem(FLexems[Index])^;
end;

{ TcxRegExprSymbol }

constructor TcxRegExprSymbol.Create(AValue: Char);
begin
  inherited Create;
  FValue := AValue;
end;


function TcxRegExprSymbol.Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  Result := AToken = FValue;

  if not Result and ACaseInsensitive then
  begin
    Result := AnsiUpperCase(AToken) = AnsiUpperCase(FValue);
    if Result and ACanChangeCase then
      AToken := FValue;
  end;
end;

function TcxRegExprSymbol.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprSymbol.Create(FValue);
end;

{ TcxRegExprTimeSeparator }

function TcxRegExprTimeSeparator.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  Result := AToken = Value;
end;

function TcxRegExprTimeSeparator.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprTimeSeparator.Create;
end;

function TcxRegExprTimeSeparator.Value: Char;
begin
  Result := dxFormatSettings.TimeSeparator;
end;

{ TcxRegExprDateSeparator }

function TcxRegExprDateSeparator.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  Result := AToken = Value;
end;

function TcxRegExprDateSeparator.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprDateSeparator.Create;
end;

function TcxRegExprDateSeparator.Value: Char;
begin
  Result := dxFormatSettings.DateSeparator;
end;

{ TcxRegExprSubrange }

constructor TcxRegExprSubrange.Create(AStartValue, AFinishValue: Char);
begin
  inherited Create;
  FStartValue := AStartValue;
  FFinishValue := AFinishValue;
end;

function TcxRegExprSubrange.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  Result := (AToken >= FStartValue) and (AToken <= FFinishValue);
end;

function TcxRegExprSubrange.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprSubrange.Create(FStartValue, FFinishValue);
end;

{ TcxRegExprEnumeration }

constructor TcxRegExprEnumeration.Create(AInverse: Boolean = False);
begin
  inherited Create;
  FInverse := AInverse;
end;

{ TcxRegExprUserEnumeration }

constructor TcxRegExprUserEnumeration.Create(AInverse: Boolean);
begin
  inherited Create(AInverse);

  FItems := TList.Create;
end;

destructor TcxRegExprUserEnumeration.Destroy;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Item(I).Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TcxRegExprUserEnumeration.Add(AItem: TcxRegExprItem);
begin
  FItems.Add(AItem);
end;

function TcxRegExprUserEnumeration.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if Item(I).Check(AToken, False, ACanChangeCase) then
    begin
      Result := not FInverse;
      Exit;
    end;

  if ACaseInsensitive then
    for I := 0 to FItems.Count - 1 do
      if Item(I).Check(AToken, True, ACanChangeCase) then
      begin
        Result := not FInverse;
        Exit;
      end;

  Result := FInverse;
end;

function TcxRegExprUserEnumeration.Item(AIndex: Integer): TcxRegExprItem;
begin
  Result := TcxRegExprItem(FItems[AIndex]);
end;

function TcxRegExprUserEnumeration.Clone: TcxRegExprItem;
var
  I: Integer;
begin
  Result := TcxRegExprUserEnumeration.Create(FInverse);

  for I := 0 to FItems.Count - 1 do
    TcxRegExprUserEnumeration(Result).Add(Item(I).Clone);
end;

{ TcxRegExprDigit }

constructor TcxRegExprDigit.Create(AInverse: Boolean);
begin
  inherited Create(AInverse);
end;

function TcxRegExprDigit.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  if (AToken >= '0') and (AToken <= '9') then
    Result := not FInverse
  else
    Result := FInverse;
end;

function TcxRegExprDigit.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprDigit.Create(FInverse);
end;

{ TcxRegExprIdLetter }

constructor TcxRegExprIdLetter.Create(AInverse: Boolean);
begin
  inherited Create(AInverse);
end;

function TcxRegExprIdLetter.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  if ((AToken >= 'a') and (AToken <= 'z')) or (AToken = '_') or
      ((AToken >= 'A') and (AToken <= 'Z'))  or
      ((AToken >= '0') and (AToken <= '9')) then
    Result := not FInverse
  else
    Result := FInverse;
end;

function TcxRegExprIdLetter.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprIdLetter.Create(FInverse);
end;

{ TcxRegExprSpace }

constructor TcxRegExprSpace.Create(AInverse: Boolean);
begin
  inherited Create(AInverse);
end;

function TcxRegExprSpace.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  if (AToken = ' ') or (AToken = #0) or (AToken = #9) or
      (AToken = #10) or (AToken = #12) or (AToken = #13) then
    Result := not FInverse
  else
    Result := FInverse;
end;

function TcxRegExprSpace.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprSpace.Create(FInverse);
end;

{ TcxRegExprAll }

function TcxRegExprAll.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
begin
  Result := True;
end;

function TcxRegExprAll.Clone: TcxRegExprItem;
begin
  Result := TcxRegExprAll.Create;
end;

{ TcxRegExprState }

constructor TcxRegExprState.Create;
begin
  inherited Create;
  FStates := TcxRegExprStates.Create;
end;

destructor TcxRegExprState.Destroy;
begin
  FStates.Free;
  inherited Destroy;
end;

procedure TcxRegExprState.Add(AState: TcxRegExprState);
begin
  States.Add(AState);
end;

procedure TcxRegExprState.Add(AStates: TcxRegExprStates);
begin
  States.Add(AStates);
end;

function TcxRegExprState.Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
begin
  Result := TcxRegExprStates.Create;
end;

function TcxRegExprState.Clone: TcxRegExprState;
begin
  Result := TcxRegExprState.Create;
end;

function TcxRegExprState.GetAllNextStates: TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to States.Count - 1 do
    Result.Add(States[I].GetSelf);
end;

function TcxRegExprState.GetSelf: TcxRegExprStates;
begin
  Result := TcxRegExprStates.Create;

  Result.Add(Self);
end;

function TcxRegExprState.Next(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to FStates.Count - 1 do
    Result.Add(FStates[I].Check(AToken, ACaseInsensitive, ACanChangeCase and (Result.Count = 0)));
end;

{ TcxRegExprSimpleState }

constructor TcxRegExprSimpleState.Create(AValue: TcxRegExprItem);
begin
  inherited Create;

  FValue := AValue;
  FIsFinal := False;
end;

destructor TcxRegExprSimpleState.Destroy;
begin
  if FValue <> nil then
    FValue.Free;

  inherited Destroy;
end;

function TcxRegExprSimpleState.Check(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
begin
  Result := TcxRegExprStates.Create;

  if FValue.Check(AToken, ACaseInsensitive, ACanChangeCase) then
    Result.Add(Self);
end;

function TcxRegExprSimpleState.Clone: TcxRegExprState;
begin
  Result := TcxRegExprSimpleState.Create(FValue.Clone);
end;

function TcxRegExprSimpleState.GetSelf: TcxRegExprStates;
begin
  Result := TcxRegExprStates.Create;

  Result.Add(Self);
end;

procedure TcxRegExprSimpleState.SetFinal;
begin
  FIsFinal := True;
end;

{ TcxRegExprBlockState }

function TcxRegExprBlockState.Check(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
begin
  Result := Next(AToken, False, ACanChangeCase);
  if ACaseInsensitive then
    Result.Add(Next(AToken, True, ACanChangeCase));
end;

function TcxRegExprBlockState.Clone: TcxRegExprState;
begin
  Result := TcxRegExprBlockState.Create;
end;

function TcxRegExprBlockState.GetSelf: TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to States.Count - 1 do
    Result.Add(States[I].GetSelf);
end;

{ TcxRegExprStates }

constructor TcxRegExprStates.Create;
begin
  inherited Create;
  FStates := TList.Create;
end;

destructor TcxRegExprStates.Destroy;
begin
  FStates.Free;
  inherited Destroy;
end;

procedure TcxRegExprStates.Add(AState: TcxRegExprState);
begin
  if FStates.IndexOf(AState) < 0 then
    FStates.Add(AState);
end;

procedure TcxRegExprStates.Add(AStates: TcxRegExprStates);
var
  I: Integer;
begin
  for I := 0 to AStates.Count - 1 do
    Add(AStates.State[I]);
  AStates.Free;
end;

procedure TcxRegExprStates.Clear;
begin
  FStates.Clear;
end;

function TcxRegExprStates.Equ(var ASymbol: Char): Boolean;
var
  I: Integer;
  Flag: Boolean;
begin
  if Count = 0 then
  begin
    Result := False;
    Exit;
  end;

  Flag := False;

  for I := 0 to Count - 1 do
  begin
    if State[I] is TcxRegExprSimpleState then
    begin
      with TcxRegExprSimpleState(State[I]) do
      begin
        if FValue is TcxRegExprSymbol then
        begin
          if not Flag then
          begin
            ASymbol := TcxRegExprSymbol(FValue).FValue;
            Flag := True;
          end
          else
          begin
            if ASymbol <> TcxRegExprSymbol(FValue).FValue then
            begin
              Result := False;
              Exit;
            end;
          end;
        end
        else if FValue is TcxRegExprTimeSeparator then
        begin
          if not Flag then
          begin
            ASymbol := TcxRegExprTimeSeparator(FValue).Value;
            Flag := True;
          end
          else
          begin
            if ASymbol <> TcxRegExprTimeSeparator(FValue).Value then
            begin
              Result := False;
              Exit;
            end;
          end;
        end
        else if FValue is TcxRegExprDateSeparator then
        begin
          if not Flag then
          begin
            ASymbol := TcxRegExprDateSeparator(FValue).Value;
            Flag := True;
          end
          else
          begin
            if ASymbol <> TcxRegExprDateSeparator(FValue).Value then
            begin
              Result := False;
              Exit;
            end;
          end;
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

function TcxRegExprStates.GetAllNextStates: TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to Count - 1 do
    Result.Add(State[I].GetAllNextStates);
end;

function TcxRegExprStates.IsFinal: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if TcxRegExprSimpleState(State[I]).IsFinal then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TcxRegExprStates.Next(var AToken: Char;
  ACaseInsensitive, ACanChangeCase: Boolean): TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to Count - 1 do
    Result.Add(State[I].Next(AToken, False, ACanChangeCase and (Result.Count = 0)));

  if ACaseInsensitive then
  begin
    for I := 0 to Count - 1 do
      Result.Add(State[I].Next(AToken, True, ACanChangeCase and (Result.Count = 0)));
  end;
end;

function TcxRegExprStates.GetCount: Integer;
begin
  Result := FStates.Count;
end;

function TcxRegExprStates.GetState(AIndex: Integer): TcxRegExprState;
begin
  Result := TcxRegExprState(FStates[AIndex]);
end;

{ TcxRegExprAutomat }

constructor TcxRegExprAutomat.Create(AExpr: TcxRegExprParserAlts; AOwner: TcxRegExpr);
begin
  inherited Create;
  FHistory := TList.Create;
  FExpr := AExpr;
  FStartState := TcxRegExprSimpleState.Create(nil);
  FStartState.Add(FExpr.GetStartConnections);
  if FExpr.StartStateIsFinal then
    FStartState.SetFinal;
  FCurrentStates := TcxRegExprStates.Create;
  FCurrentStates.Add(FStartState);
  FOwner := AOwner;
end;

destructor TcxRegExprAutomat.Destroy;
var
  I: Integer;
begin
  for I := 0 to FHistory.Count - 1 do
    TcxRegExprStates(FHistory[I]).Free;
  FHistory.Free;
  FCurrentStates.Free;
  FExpr.Free;
  FStartState.Free;
  inherited Destroy;
end;

function TcxRegExprAutomat.GetAllNextStates: TcxRegExprStates;
begin
  Result := FCurrentStates.GetAllNextStates
end;

function TcxRegExprAutomat.IsFinal: Boolean;
begin
  Result := FCurrentStates.IsFinal;
end;

function TcxRegExprAutomat.IsStart: Boolean;
begin
  Result := FCurrentStates[0] = FStartState;
end;

function TcxRegExprAutomat.Next(var AToken: Char; ACaseInsensitive, ACanChangeCase: Boolean): Boolean;
var
  NextStates: TcxRegExprStates;
begin
  NextStates := FCurrentStates.Next(AToken, ACaseInsensitive, ACanChangeCase);
  if NextStates.Count > 0 then
  begin
    Push(FCurrentStates);
    FCurrentStates := NextStates;
    Result := True;
  end
  else
  begin
    NextStates.Free;
    Result := False;
  end;
end;

function TcxRegExprAutomat.Prev: Boolean;
var
  LastStates: TcxRegExprStates;
begin
  LastStates := Pop;
  if LastStates = nil then
    Result := False
  else
  begin
    FCurrentStates.Free;
    FCurrentStates := LastStates;
    Result := True;
  end;
end;

function TcxRegExprAutomat.Print: string;
begin
  Result := FExpr.Print;
end;

procedure TcxRegExprAutomat.Reset;
var
  I: Integer;
begin
  for I := 0 to FHistory.Count - 1 do
    TcxRegExprStates(FHistory[I]).Free;
  FHistory.Clear;
  FCurrentStates.Free;
  FCurrentStates := TcxRegExprStates.Create;
  FCurrentStates.Add(FStartState);
end;

procedure TcxRegExprAutomat.ReUpdate;
var
  ASymbol: Char;
  PrevStates: TcxRegExprStates;
  AllNextStates: TcxRegExprStates;
begin
  while FCurrentStates.Equ(ASymbol) do
  begin
    PrevStates := Pop;
    if PrevStates = nil then
    begin
      Push(PrevStates);
      Exit;
    end;

    AllNextStates := PrevStates.GetAllNextStates;
    if not AllNextStates.Equ(ASymbol) or PrevStates.IsFinal then
    begin
      Push(PrevStates);
      AllNextStates.Free;
      Exit;
    end
    else
      AllNextStates.Free;

    FOwner.SymbolDelete;

    FCurrentStates.Free;
    FCurrentStates := PrevStates;
  end;
end;

procedure TcxRegExprAutomat.Update;
var
  NextStates: TcxRegExprStates;
  ASymbol: Char;
begin
  if FCurrentStates.IsFinal then
    Exit;

  NextStates := GetAllNextStates;

  while NextStates.Equ(ASymbol) do
  begin
    FOwner.SymbolUpdate(ASymbol);

    Push(FCurrentStates);
    FCurrentStates := NextStates;

    if NextStates.IsFinal then
      Exit;

    NextStates := GetAllNextStates;
  end;

  NextStates.Free;
end;

function TcxRegExprAutomat.Pop: TcxRegExprStates;
begin
  if FHistory.Count > 0 then
  begin
    Result := TcxRegExprStates(FHistory.Last);
    FHistory.Delete(FHistory.Count - 1);
  end
  else
    Result := nil;
end;

procedure TcxRegExprAutomat.Push(AStates: TcxRegExprStates);
begin
  FHistory.Add(AStates);
end;

{ TcxRegExprSimpleQuantifier }

function TcxRegExprSimpleQuantifier.CanMissing: Boolean;
begin
  Result := False;
end;

function TcxRegExprSimpleQuantifier.CanRepeat: Boolean;
begin
  Result := False;
end;

function TcxRegExprSimpleQuantifier.Clone: TcxRegExprQuantifier;
begin
  Result := TcxRegExprSimpleQuantifier.Create;
end;

function TcxRegExprSimpleQuantifier.Print: string;
begin
  Result := '';
end;

{ TcxRegExprQuestionQuantifier }

function TcxRegExprQuestionQuantifier.CanMissing: Boolean;
begin
  Result := True;
end;

function TcxRegExprQuestionQuantifier.CanRepeat: Boolean;
begin
  Result := False;
end;

function TcxRegExprQuestionQuantifier.Clone: TcxRegExprQuantifier;
begin
  Result := TcxRegExprQuestionQuantifier.Create;
end;

function TcxRegExprQuestionQuantifier.Print: string;
begin
  Result := '?';
end;

{ TcxRegExprStarQuantifier }

function TcxRegExprStarQuantifier.CanMissing: Boolean;
begin
  Result := True;
end;

function TcxRegExprStarQuantifier.CanRepeat: Boolean;
begin
  Result := True;
end;

function TcxRegExprStarQuantifier.Clone: TcxRegExprQuantifier;
begin
  Result := TcxRegExprStarQuantifier.Create;
end;

function TcxRegExprStarQuantifier.Print: string;
begin
  Result := '*';
end;

{ TcxRegExprPlusQuantifier }

function TcxRegExprPlusQuantifier.CanMissing: Boolean;
begin
  Result := False;
end;

function TcxRegExprPlusQuantifier.CanRepeat: Boolean;
begin
  Result := True;
end;

function TcxRegExprPlusQuantifier.Clone: TcxRegExprQuantifier;
begin
  Result := TcxRegExprPlusQuantifier.Create;
end;

function TcxRegExprPlusQuantifier.Print: string;
begin
  Result := '+';
end;

{ TcxRegExprParserItem }

constructor TcxRegExprParserItem.Create(AQuantifier: TcxRegExprQuantifier = nil);
begin
  inherited Create;
  if AQuantifier = nil then
    FQuantifier := TcxRegExprSimpleQuantifier.Create
  else
    FQuantifier := AQuantifier;
end;

destructor TcxRegExprParserItem.Destroy;
begin
  FQuantifier.Free;
  inherited Destroy;
end;

function TcxRegExprParserItem.CanMissing: Boolean;
begin
  Result := FQuantifier.CanMissing;
end;

function TcxRegExprParserItem.CanRepeat: Boolean;
begin
  Result := FQuantifier.CanRepeat;
end;

function TcxRegExprParserItem.NotQuantifier: Boolean;
begin
  Result := FQuantifier is TcxRegExprSimpleQuantifier;
end;

procedure TcxRegExprParserItem.SetQuantifier(
  AQuantifier: TcxRegExprQuantifier);
begin
  if AQuantifier <> nil then
  begin
    FQuantifier.Free;
    FQuantifier := AQuantifier;
  end;
end;

{ TcxRegExprParserSimpleItem }

constructor TcxRegExprParserSimpleItem.Create(AState: TcxRegExprState;
    AQuantifier: TcxRegExprQuantifier);
begin
  inherited Create(AQuantifier);
  FState := AState;
end;

destructor TcxRegExprParserSimpleItem.Destroy;
begin
  if FState <> nil then
    FState.Free;
  inherited Destroy;
end;

function TcxRegExprParserSimpleItem.CanEmpty: Boolean;
begin
  Result := FQuantifier.CanMissing;
end;

function TcxRegExprParserSimpleItem.Clone: TcxRegExprParserItem;
begin
  Result := TcxRegExprParserSimpleItem.Create(FState.Clone, FQuantifier.Clone);
end;

function TcxRegExprParserSimpleItem.Print: string;
begin
  Result := 'item --> ' + FQuantifier.Print + #13#10;
end;

procedure TcxRegExprParserSimpleItem.SetFinal;
begin
  TcxRegExprSimpleState(State).SetFinal;
end;

{ TcxRegExprParserBlockItem }

constructor TcxRegExprParserBlockItem.Create(AQuantifier: TcxRegExprQuantifier = nil);
begin
  inherited Create(AQuantifier);

  FStartState := TcxRegExprBlockState.Create;
  FFinishState := TcxRegExprBlockState.Create;
  FAlts := TcxRegExprParserAlts.Create;
end;

destructor TcxRegExprParserBlockItem.Destroy;
begin
  FStartState.Free;
  FFinishState.Free;
  FAlts.Free;

  inherited Destroy;
end;

function TcxRegExprParserBlockItem.CanEmpty: Boolean;
begin
  if FQuantifier.CanMissing then
    Result := True
  else
    Result := Alts.CanEmpty;
end;

procedure TcxRegExprParserBlockItem.CreateConnections;
begin
  Alts.CreateConnections;
end;

procedure TcxRegExprParserBlockItem.AddAlt(AAlt: TcxRegExprParserAlt);
begin
  FAlts.Add(AAlt);
end;

procedure TcxRegExprParserBlockItem.AddAlts(AAlts: TcxRegExprParserAlts);
var
  I: Integer;
begin
  for I := 0 to AAlts.Count - 1 do
    FAlts.Add(AAlts[I]);
  AAlts.Free;
end;

function TcxRegExprParserBlockItem.Clone: TcxRegExprParserItem;
begin
  Result := TcxRegExprParserBlockItem.Create(FQuantifier.Clone);
  with TcxRegExprParserBlockItem(Result) do
  begin
    FAlts.Free;
    FAlts := Self.Alts.Clone;
  end;
end;

function TcxRegExprParserBlockItem.Print: string;
begin
  Result := '<Start_Block>'#13#10;
  Result := Result + Alts.Print;
  Result := Result + '<Finish_Block> --> ' + FQuantifier.Print + #13#10;
end;

procedure TcxRegExprParserBlockItem.SetFinal;
begin
  Alts.CreateFinalStates;
end;

{ TcxRegExprParserAlt }

constructor TcxRegExprParserAlt.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxRegExprParserAlt.Destroy;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TcxRegExprParserItem(FItems[I]).Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TcxRegExprParserAlt.Add(AItem: TcxRegExprParserItem);
begin
  FItems.Add(AItem);
end;

function TcxRegExprParserAlt.CanEmpty: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if not Item[I].CanEmpty then
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TcxRegExprParserAlt.CanMissing: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if not Item[I].CanMissing then
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TcxRegExprParserAlt.Clone: TcxRegExprParserAlt;
var
  I: Integer;
begin
  Result := TcxRegExprParserAlt.Create;

  for I := 0 to Count - 1 do
    Result.Add(Item[I].Clone);
end;

procedure TcxRegExprParserAlt.CreateConnections;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Item[I] is TcxRegExprParserSimpleItem then
    begin
      with TcxRegExprParserSimpleItem(Item[I]) do
      begin
        for J := I + 1 to Count - 1 do
        begin
          if Item[J] is TcxRegExprParserSimpleItem then
            State.Add(TcxRegExprParserSimpleItem(Item[J]).State)
          else if Item[J] is TcxRegExprParserBlockItem then
            State.Add(TcxRegExprParserBlockItem(Item[J]).StartState);

          if not Item[J].CanMissing then
            Break;
        end;

        if Item[I].CanRepeat then
          State.Add(State);
      end;
    end
    else if Item[I] is TcxRegExprParserBlockItem then
    begin
      with TcxRegExprParserBlockItem(Item[I]) do
      begin
        for J := I + 1 to Count - 1 do
        begin
          if Item[J] is TcxRegExprParserSimpleItem then
            FinishState.Add(TcxRegExprParserSimpleItem(Item[J]).State)
          else if Item[J] is TcxRegExprParserBlockItem then
            FinishState.Add(TcxRegExprParserBlockItem(Item[J]).StartState);

          if not Item[J].CanMissing then
            Break;
        end;

        if Item[I].CanRepeat then
          FinishState.Add(StartState);

        StartState.Add(Alts.GetStartConnections);
        if Alts.ThereIsEmptyAlt then
          StartState.Add(FinishState);

        Alts.CreateConnections;
        Alts.SetFinishConnections(FinishState);
      end;
    end;
  end;
end;

procedure TcxRegExprParserAlt.CreateFinalStates;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Item[I].SetFinal;

    if not Item[I].CanMissing then
      Break;
  end;
end;

function TcxRegExprParserAlt.GetStartConnections: TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to Count - 1 do
  begin
    if Item[I] is TcxRegExprParserSimpleItem then
      Result.Add(TcxRegExprParserSimpleItem(Item[I]).State)
    else if Item[I] is TcxRegExprParserBlockItem then
      Result.Add(TcxRegExprParserBlockItem(Item[I]).StartState);

    if not Item[I].CanMissing then
      Break;
  end;
end;

function TcxRegExprParserAlt.Print: string;
var
  I: Integer;
begin
  Result := '<Start_Alt>'#13#10;
  for I := 0 to Count - 1 do
    Result := Result + Item[I].Print;
  Result := result + '<Finish_Alt>'#13#10;
end;

procedure TcxRegExprParserAlt.SetFinishConnection(
  AFinishState: TcxRegExprState);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Item[I] is TcxRegExprParserSimpleItem then
      TcxRegExprParserSimpleItem(Item[I]).State.Add(AFinishState)
    else if Item[I] is TcxRegExprParserBlockItem then
      TcxRegExprParserBlockItem(Item[I]).FinishState.Add(AFinishState);

    if not Item[I].CanMissing then
      Break;
  end;
end;

function TcxRegExprParserAlt.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxRegExprParserAlt.GetFirstItem: TcxRegExprParserItem;
begin
  Result := TcxRegExprParserItem(FItems[0]);
end;

function TcxRegExprParserAlt.GetItem(
  AIndex: Integer): TcxRegExprParserItem;
begin
  Result := TcxRegExprParserItem(FItems[AIndex]);
end;

function TcxRegExprParserAlt.GetLastItem: TcxRegExprParserItem;
begin
  Result := TcxRegExprParserItem(FItems.Last);
end;

procedure TcxRegExprParserAlt.SetLastItem(AItem: TcxRegExprParserItem);
begin
  TcxRegExprParserItem(FItems[FItems.Count - 1]).Free;
  FItems.Delete(FItems.Count - 1);
  FItems.Add(AItem);
end;

{ TcxRegExprParserAlts }

constructor TcxRegExprParserAlts.Create;
begin
  inherited Create;
  FAlts := TList.Create;
end;

destructor TcxRegExprParserAlts.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Alt[I].Free;
  FAlts.Free;
  inherited Destroy;
end;

procedure TcxRegExprParserAlts.Add(AAlt: TcxRegExprParserAlt);
begin
  FAlts.Add(AAlt);
end;

procedure TcxRegExprParserAlts.AddAlt;
begin
  FAlts.Add(TcxRegExprParserAlt.Create)
end;

function TcxRegExprParserAlts.CanEmpty: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Alt[I].CanEmpty then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

procedure TcxRegExprParserAlts.CreateConnections;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Alt[I].CreateConnections;
end;

procedure TcxRegExprParserAlts.CreateFinalStates;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Alt[I].CreateFinalStates;
end;

function TcxRegExprParserAlts.Clone: TcxRegExprParserAlts;
var
  I: Integer;
begin
  Result := TcxRegExprParserAlts.Create;

  for I := 0 to Count - 1 do
    Result.Add(Alt[I].Clone);
end;

function TcxRegExprParserAlts.GetStartConnections: TcxRegExprStates;
var
  I: Integer;
begin
  Result := TcxRegExprStates.Create;

  for I := 0 to Count - 1 do
    Result.Add(Alt[I].GetStartConnections);
end;

function TcxRegExprParserAlts.Print: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Alt[I].Print;
end;

procedure TcxRegExprParserAlts.SetFinishConnections(
  AFinishState: TcxRegExprState);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Alt[I].SetFinishConnection(AFinishState);
end;

function TcxRegExprParserAlts.StartStateIsFinal: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Alt[I].CanMissing then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TcxRegExprParserAlts.ThereIsEmptyAlt: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Alt[I].CanMissing then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TcxRegExprParserAlts.GetAlt(AIndex: Integer): TcxRegExprParserAlt;
begin
  Result := TcxRegExprParserAlt(FAlts[AIndex]);
end;

function TcxRegExprParserAlts.GetCount: Integer;
begin
  Result := FAlts.Count;
end;

function TcxRegExprParserAlts.GetLastAlt: TcxRegExprParserAlt;
begin
  Result := TcxRegExprParserAlt(FAlts.Last);
end;

{ TcxRegExpr }

constructor TcxRegExpr.Create;
begin
  inherited Create;
  FStream := TStringStream.Create('', TEncoding.UTF8);
  FErrors := TcxRegExprErrors.Create;
  FLexems := TcxLexems.Create;
  FBlocks := TList.Create;
  FAutomat := nil;
  FIndex := 0;
  FLexemIndex := 0;
  FLine := 1;
  FChar := 0;
  FFirstExpr := True;
  FCompiled := False;
  FUpdateOn := False;
  FCaseInsensitive := False;
end;

destructor TcxRegExpr.Destroy;
begin
  Clear;
  FStream.Free;
  FLexems.Free;
  FBlocks.Free;
  FErrors.Free;
  FAutomat.Free;
  inherited Destroy;
end;

procedure TcxRegExpr.Compile(AStream: TStream);
begin
  if FFirstExpr then
    FFirstExpr := False
  else
    Clear;

  try
    FStream.CopyFrom(AStream, 0);
  except
    FErrors.Add(TcxRegExprError.Create(0, 0, cxGetResourceString(@scxRegExprNotAssignedSourceStream)));
    raise EcxRegExprError.Create(FErrors);
  end;

  if EmptyStream then
  begin
    FErrors.Add(TcxRegExprError.Create(0, 0, cxGetResourceString(@scxRegExprEmptySourceStream)));
    raise EcxRegExprError.Create(FErrors);
  end;

  ScanExpr;
  if FErrors.Count > 0 then
    raise EcxRegExprError.Create(FErrors);

  ParseExpr;
  if FErrors.Count > 0 then
    raise EcxRegExprError.Create(FErrors);

  FCompiled := True;

  if UpdateOn then
    FAutomat.Update;
end;

procedure TcxRegExpr.Compile(const S: string);
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create(S, TEncoding.UTF8);
  try
    Compile(AStream);
  finally
    AStream.Free;
  end;
end;

function TcxRegExpr.IsCompiled: Boolean;
begin
  Result := FCompiled;
end;

function TcxRegExpr.IsFinal: Boolean;
begin
  TestCompiledStatus;

  Result := FAutomat.IsFinal;
end;

function TcxRegExpr.IsStart: Boolean;
begin
  TestCompiledStatus;

  Result := FAutomat.IsStart;
end;

function TcxRegExpr.Next(var AToken: Char): Boolean;
begin
  TestCompiledStatus;

  Result := FAutomat.Next(AToken, FCaseInsensitive, True);

  if not FAutomat.IsFinal and Result and UpdateOn then
    FAutomat.Update;
end;

function TcxRegExpr.NextEx(const AString: string): string;
var
  C: Char;
  I: Integer;
begin
  TestCompiledStatus;

  Result := '';
  for I := 1 to Length(AString) do
  begin
    C := AString[I];
    if FAutomat.Next(C, FCaseInsensitive, True) then
      Result := Result + AString[I];
  end;
end;

function TcxRegExpr.Prev: Boolean;
begin
  TestCompiledStatus;

  if UpdateOn then
    FAutomat.ReUpdate;

  Result := FAutomat.Prev;
end;

function TcxRegExpr.Print: string;
begin
  Result := FAutomat.Print;
end;

procedure TcxRegExpr.Reset;
begin
  TestCompiledStatus;

  FAutomat.Reset;
end;

procedure TcxRegExpr.Clear;
begin
  FStream.Size := 0;
  FErrors.Clear;
  FLexems.Clear;
  FBlocks.Clear;

  if FAutomat <> nil then
    FAutomat.Free;

  FAutomat := nil;
  FIndex := 0;
  FLexemIndex := 0;
  FChar := 0;
  FLine := 1;
  FCompiled := False;
end;

function TcxRegExpr.Decimal(AToken: Char): Boolean;
begin
  Result := cxIsDigitChar(AToken);
end;

function TcxRegExpr.EmptyStream: Boolean;
var
  AToken: Char;
  I: Integer;
begin
  if FStream.Size = 0 then
    Result := True
  else
  begin
    I := 0;
    while LookToken(AToken, I) do
    begin
      if not Space(AToken) then
      begin
        Result := False;
        Exit;
      end;

      Inc(I);
    end;

    Result := True;
  end;
end;

function TcxRegExpr.CreateLexem(ALine: Integer; AChar: Integer; ACode: TcxRegExprLexemCode;
    AValue: string): TcxLexem;
begin
  Result.Line := ALine;
  Result.Char := AChar;
  Result.Code := ACode;
  Result.Value := AValue;
end;

function TcxRegExpr.GetLexem(var ALexem: TcxLexem): Boolean;
begin
  if (FLexemIndex >= 0) and (FLexemIndex < FLexems.Count) then
  begin
    ALexem := FLexems[FLexemIndex];
    Inc(FLexemIndex);

    Result := True;
  end
  else
    Result := False;
end;

function TcxRegExpr.GetToken(out AToken: Char): Boolean;
begin
  Result := LookToken(AToken, 0);
  if Result then
  begin
    Inc(FIndex);

    if AToken = #13 then
      Inc(FLine);

    if AToken = #10 then
      FChar := 0
    else
      Inc(FChar);
  end;
end;

function TcxRegExpr.GetStream: TStream;
begin
  if FCompiled then
    Result := FStream
  else
    Result := nil;
end;

function TcxRegExpr.Hexadecimal(AToken: Char): Boolean;
begin
  Result := (AToken >= '0') and (AToken <= '9') or
    (AToken >= 'A') and (AToken <= 'F') or
    (AToken >= 'a') and (AToken <= 'f');
end;

function TcxRegExpr.LookToken(out AToken: Char; APtr: Integer): Boolean;
var
  AStartPos: Integer;
  ADataString: string;
begin
  AStartPos := FIndex + APtr;
  ADataString := FStream.DataString;
  Result := (AStartPos < Length(ADataString)) and (AStartPos >= 0);
  if Result then
    AToken := ADataString[AStartPos + 1];
end;

function TcxRegExpr.ParseAlt(AAlt: TcxRegExprParserAlt; Global: Boolean): Boolean;
var
  ALexem: TcxLexem;
  ACurrentItem: TcxRegExprParserItem;

  procedure AddItem(AItem: TcxRegExprParserItem);
  begin
    ACurrentItem := AItem;
    AAlt.Add(AItem);
  end;

  procedure SetQuantifier(AQuantifier: TcxRegExprQuantifier);
  var
    ABlock: TcxRegExprParserBlockItem;
  begin
    if ACurrentItem.NotQuantifier then
      ACurrentItem.SetQuantifier(AQuantifier)
    else
    begin
      ABlock := TcxRegExprParserBlockItem.Create(AQuantifier);
      ABlock.Alts.AddAlt;
      ABlock.Alts.LastAlt.Add(ACurrentItem);
      ACurrentItem := ABlock;
      AAlt.FItems[AAlt.FItems.Count - 1] := ABlock;
    end;
  end;

  function CreateParameterQuantifierBlock(AIndex, ACount: Integer): TcxRegExprParserItem;
  begin
    if AIndex < (ACount - 1) then
    begin
      Result := TcxRegExprParserBlockItem.Create(TcxRegExprQuestionQuantifier.Create);
      with TcxRegExprParserBlockItem(Result).Alts do
      begin
        AddAlt;
        LastAlt.Add(ACurrentItem.Clone);
        LastAlt.Add(CreateParameterQuantifierBlock(AIndex + 1, ACount));
      end;
    end
    else
    begin
      Result := ACurrentItem.Clone;
      Result.SetQuantifier(TcxRegExprQuestionQuantifier.Create);
    end;
  end;

  procedure SetParameterQuantifier(A, B: Integer);
  var
    ABlock: TcxRegExprParserBlockItem;
    AItem: TcxRegExprParserItem;
    I: Integer;
  begin
    if ACurrentItem.CanMissing then
    begin
      FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
          cxGetResourceString(@scxRegExprCantUseParameterQuantifier)));
      Exit;
    end;

    ABlock := TcxRegExprParserBlockItem.Create(TcxRegExprSimpleQuantifier.Create);
    ABlock.Alts.AddAlt;
    for I := 0 to A - 1 do
      ABlock.Alts.LastAlt.Add(ACurrentItem.Clone);
    if B = -1 then
    begin
      AItem := ACurrentItem.Clone;
      AItem.SetQuantifier(TcxRegExprStarQuantifier.Create);
      ABlock.Alts.LastAlt.Add(AItem);
    end
    else if B > A then
      ABlock.Alts.LastAlt.Add(CreateParameterQuantifierBlock(A, B));
    ACurrentItem := ABlock;
    AAlt.LastItem := ABlock;
  end;

  procedure SetQuestionQuantifier;
  begin
    SetQuantifier(TcxRegExprQuestionQuantifier.Create);
  end;

  procedure SetPlusQuantifier;
  begin
    if ACurrentItem.CanEmpty then
      FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
          cxGetResourceString(@scxRegExprCantUsePlusQuantifier)))
    else
      SetQuantifier(TcxRegExprPlusQuantifier.Create);
  end;

  procedure SetStarQuantifier;
  begin
    if ACurrentItem.CanEmpty then
      FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
          cxGetResourceString(@scxRegExprCantUseStarQuantifier)))
    else
      SetQuantifier(TcxRegExprStarQuantifier.Create);
  end;

var
  RefNumber: Integer;
  A, B: Integer;
begin
  ACurrentItem := nil;

  if GetLexem(ALexem) then
  begin
    if (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = '|') then
    begin
      FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
        cxGetResourceString(@scxRegExprCantCreateEmptyAlt)));
      Result := True;
      Exit;
    end;

    if not Global then
    begin
      if (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = ')') then
      begin
        FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
          cxGetResourceString(@scxRegExprCantCreateEmptyBlock)));
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    FErrors.Add(TcxRegExprError.Create(0, 0, cxGetResourceString(@scxRegExprCantCreateEmptyAlt)));
    Result := False;
    Exit;
  end;

  repeat
    case TcxRegExprLexemCode(ALexem.Code) of
      relcSymbol:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprSymbol.Create(
                ALexem.Value[1]))));
      relcSpecial:
      begin
        case ALexem.Value[1] of
          '|':
          begin
            Result := True;
            Exit;
          end;
          '(':
            AddItem(ParseBlock);
          ')':
          begin
            if Global then
              FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalSymbol), [')'])))
            else
            begin
              Result := False;
              Exit;
            end;
          end;
          '[':
            AddItem(ParseEnumeration);
          ']':
            FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalSymbol), [']'])));
          '{':
          begin
            if ACurrentItem = nil then
              FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  cxGetResourceString(@scxRegExprIncorrectParameterQuantifier)))
            else
            begin
              ParseQuantifier(A, B);
              SetParameterQuantifier(A, B);
              if A = 0 then
                SetQuestionQuantifier;
            end;
          end;
          '}':
            FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalSymbol), ['}'])));
          '-':
            FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalSymbol), ['-'])));
          '?':
          begin
            if ACurrentItem = nil then
              FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalQuantifier), ['?'])))
            else
              SetQuestionQuantifier;
          end;
          '+':
          begin
            if ACurrentItem = nil then
              FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalQuantifier), ['+'])))
            else
              SetPlusQuantifier;
          end;
          '*':
          begin
            if ACurrentItem = nil then
              FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
                  Format(cxGetResourceString(@scxRegExprIllegalQuantifier), ['*'])))
            else
              SetStarQuantifier;
          end;
        end;
      end;
      relcInteger:
        FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
            cxGetResourceString(@scxRegExprIllegalIntegerValue)));
      relcTimeSeparator:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprTimeSeparator.Create)));
      relcDateSeparator:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprDateSeparator.Create)));
      relcAll:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprAll.Create)));
      relcId:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprIdLetter.Create)));
      relcNotId:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprIdLetter.Create(True))));
      relcDigit:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprDigit.Create)));
      relcNotDigit:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprDigit.Create(True))));
      relcSpace:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprSpace.Create)));
      relcNotSpace:
        AddItem(
          TcxRegExprParserSimpleItem.Create(
            TcxRegExprSimpleState.Create(
              TcxRegExprSpace.Create(True))));
      relcReference:
      begin
        RefNumber := StrToInt(ALexem.Value) - 1;
        if RefNumber >= FBlocks.Count then
          FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
              cxGetResourceString(@scxRegExprTooBigReferenceNumber)))
        else
          AddItem(TcxRegExprParserItem(FBlocks[RefNumber]).Clone);
      end;
    end;
  until not GetLexem(ALexem);

  Result := False;
end;

function TcxRegExpr.ParseBlock: TcxRegExprParserBlockItem;
begin
  Result := TcxRegExprParserBlockItem.Create;
  FBlocks.Add(Result);

  repeat
    Result.Alts.AddAlt;
  until not ParseAlt(Result.Alts.LastAlt, False);
end;

function TcxRegExpr.ParseEnumeration: TcxRegExprParserSimpleItem;
var
  ALexem: TcxLexem;
  ALexem1: TcxLexem;
  Enumeration: TcxRegExprUserEnumeration;
begin
  GetLexem(ALexem);

  if (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = '^') then
  begin
    Enumeration := TcxRegExprUserEnumeration.Create(True);
    GetLexem(ALexem);
  end
  else
    Enumeration := TcxRegExprUserEnumeration.Create;

  Result := TcxRegExprParserSimpleItem.Create(TcxRegExprSimpleState.Create(Enumeration));

  if (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = ']') then
  begin
    FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
        cxGetResourceString(@scxRegExprCantCreateEmptyEnum)));
    Exit;
  end;

  repeat
    GetLexem(ALexem1);

    if (TcxRegExprLexemCode(ALexem1.Code) = relcSpecial) and (ALexem1.Value[1] = '-') then
    begin
      GetLexem(ALexem1);
      if ALexem.Value[1] < ALexem1.Value[1] then
        Enumeration.Add(TcxRegExprSubrange.Create(ALexem.Value[1], ALexem1.Value[1]))
      else
        FErrors.Add(TcxRegExprError.Create(ALexem1.Line, ALexem1.Char,
          cxGetResourceString(@scxRegExprSubrangeOrder)));

      GetLexem(ALexem);
      while (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = '-') do
      begin
        FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
          Format(cxGetResourceString(@scxRegExprIllegalSymbol), ['-'])));
        GetLexem(ALexem);
      end;
    end
    else
    begin
      case TcxRegExprLexemCode(ALexem.Code) of
        relcTimeSeparator:
          Enumeration.Add(TcxRegExprTimeSeparator.Create);
        relcDateSeparator:
          Enumeration.Add(TcxRegExprDateSeparator.Create);
        relcAll:
          Enumeration.Add(TcxRegExprAll.Create);
        relcId:
          Enumeration.Add(TcxRegExprIdLetter.Create);
        relcNotId:
          Enumeration.Add(TcxRegExprIdLetter.Create(True));
        relcDigit:
          Enumeration.Add(TcxRegExprDigit.Create);
        relcNotDigit:
          Enumeration.Add(TcxRegExprDigit.Create(True));
        relcSpace:
          Enumeration.Add(TcxRegExprSpace.Create);
        relcNotSpace:
          Enumeration.Add(TcxRegExprSpace.Create(True));
        else
          Enumeration.Add(TcxRegExprSymbol.Create(ALexem.Value[1]));
      end;

      ALexem := ALexem1;
    end;

  until (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value[1] = ']');
end;

procedure TcxRegExpr.ParseExpr;
var
  Expr: TcxRegExprParserAlts;
begin
  Expr := TcxRegExprParserAlts.Create;

  repeat
    Expr.AddAlt;
  until not ParseAlt(Expr.LastAlt);

  if FErrors.Count > 0 then
    Expr.Free
  else
  begin
    Expr.CreateConnections;
    Expr.CreateFinalStates;
    FAutomat := TcxRegExprAutomat.Create(Expr, Self);
  end;
end;

procedure TcxRegExpr.ParseQuantifier(var A: Integer; var B: Integer);
var
  ALexem: TcxLexem;
begin
  GetLexem(ALexem);
  if TcxRegExprLexemCode(ALexem.Code) = relcInteger then
  begin
    A := StrToInt(ALexem.Value);
    GetLexem(ALexem);
    if TcxRegExprLexemCode(ALexem.Code) = relcSpecial then
    begin
      if ALexem.Value = ',' then
      begin
        GetLexem(ALexem);
        if TcxRegExprLexemCode(ALexem.Code) = relcInteger then
        begin
          B := StrToInt(ALexem.Value);
          if B >= A then
          begin
            GetLexem(ALexem);
            if (TcxRegExprLexemCode(ALexem.Code) = relcSpecial) and (ALexem.Value = '}') then
              Exit;
          end;
        end
        else if TcxRegExprLexemCode(ALexem.Code) = relcSpecial then
        begin
          if ALexem.Value = '}' then
          begin
            B := -1;
            Exit;
          end;
        end;
      end
      else if ALexem.Value = '}' then
      begin
        B := A;
        Exit;
      end;
    end;
  end;
  FErrors.Add(TcxRegExprError.Create(ALexem.Line, ALexem.Char,
    cxGetResourceString(@scxRegExprIncorrectParameterQuantifier)));
end;

procedure TcxRegExpr.ScanASCII(ALine: Integer; AChar: Integer);
var
  AToken: Char;
  ALexem: TcxLexem;
begin
  if GetToken(AToken) then
  begin
    if Hexadecimal(AToken)then
        ALexem.Value := AToken
    else
    begin
      FErrors.Add(TcxRegExprError.Create(FLine, FChar,
        Format(cxGetResourceString(@scxRegExprHexNumberExpected), [AToken])));
      Exit;
    end;

    if GetToken(AToken) then
    begin
      if Hexadecimal(AToken)then
        ALexem.Value := ALexem.Value + AToken
      else
      begin
        FErrors.Add(TcxRegExprError.Create(FLine, FChar,
          Format(cxGetResourceString(@scxRegExprHexNumberExpected), [AToken])));
        Exit;
      end;

      ALexem.Line := ALine;
      ALexem.Char := AChar;
      ALexem.Code := relcSymbol;
      ALexem.Value := Char(StrToInt('$' + ALexem.Value));
      FLexems.Add(ALexem);
    end
    else
      FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
        cxGetResourceString(@scxRegExprHexNumberExpected0)));
  end
  else
    FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
      cxGetResourceString(@scxRegExprHexNumberExpected0)));
end;

procedure TcxRegExpr.ScanClass;
var
  AToken: Char;
  Flag: Boolean;
  _Line: Integer;
  _Char: Integer;
begin
  while GetToken(AToken) do
  begin
    if Space(AToken) then
      Continue;
    case AToken of
      '''':
      begin
        ScanString;
        Break;
      end;
      '^':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        Break;
      end;
      ':':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcTimeSeparator, AToken));
        Break;
      end;
      '/':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcDateSeparator, AToken));
        Break;
      end;
      '\':
      begin
        ScanEscape(FLine, FChar);
        Break;
      end;
      ']':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        Exit;
      end;
      else
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSymbol, AToken));
        Break;
      end;
    end;
  end;

  while GetToken(AToken) do
  begin
    if Space(AToken) then
      Continue;
    case AToken of
      '''':
        ScanString;
      ']':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        Exit;
      end;
      '-':
      begin
        Flag := False;
        _Line := FLine;
        _Char := FChar;
        while LookToken(AToken, 0) do
        begin
          if Space(AToken) then
          begin
            GetToken(AToken);
            Continue;
          end
          else
          begin
            if AToken = ']' then
              FLexems.Add(CreateLexem(_Line, _Char, relcSymbol, '-'))
            else
              FLexems.Add(CreateLexem(_Line, _Char, relcSpecial, '-'));

            Flag := True;
            Break;
          end;
        end;

        if not Flag then
        begin
          FLexems.Add(CreateLexem(_Line, _Char, relcSpecial, '-'));
          FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
            Format(cxGetResourceString(@scxRegExprMissing), [']'])));
          Exit;
        end;
      end;
      ':':
        FLexems.Add(CreateLexem(FLine, FChar, relcTimeSeparator, AToken));
      '/':
        FLexems.Add(CreateLexem(FLine, FChar, relcDateSeparator, AToken));
      '\':
        ScanEscape(FLine, FChar);
      else
        FLexems.Add(CreateLexem(FLine, FChar, relcSymbol, AToken));
    end;
  end;

  FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
    Format(cxGetResourceString(@scxRegExprMissing), [']'])));
end;

procedure TcxRegExpr.ScanExpr;
var
  AToken: Char;
  AOpenSkobCounter: Integer;
begin
 AOpenSkobCounter := 0;

  while GetToken(AToken) do
  begin
    if Space(AToken) then
      Continue;

    case AToken of
      '''':
        ScanString;
      '[':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        ScanClass;
      end;
      '{':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        ScanQuantifier;
      end;
      '.':
        FLexems.Add(CreateLexem(FLine, FChar, relcAll, AToken));
      ':':
        FLexems.Add(CreateLexem(FLine, FChar, relcTimeSeparator, AToken));
      '/':
        FLexems.Add(CreateLexem(FLine, FChar, relcDateSeparator, AToken));
      '\':
        ScanEscape(FLine, FChar);
      '(':
      begin
        Inc(AOpenSkobCounter);
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
      end;
      ')':
      begin
        Dec(AOpenSkobCounter);
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
      end;
      '+', '*', '?', '|':
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
      else
        FLexems.Add(CreateLexem(FLine, FChar, relcSymbol, AToken));
    end;
  end;

  if AOpenSkobCounter > 0 then
    FErrors.Add(TcxRegExprError.Create(0, 0,
      Format(cxGetResourceString(@scxRegExprMissing), [')'])))
  else if AOpenSkobCounter < 0 then
    FErrors.Add(TcxRegExprError.Create(0, 0,
      Format(cxGetResourceString(@scxRegExprUnnecessary), [')'])));
end;

procedure TcxRegExpr.ScanEscape(ALine: Integer; AChar: Integer);
var
  AToken: Char;
begin
  while GetToken(AToken) do
  begin
    if Decimal(AToken) and (AToken <> '0') then
    begin
      FLexems.Add(CreateLexem(ALine, AChar, relcReference, AToken));
      Exit;
    end
    else
    begin
      if Space(AToken) then
      begin
        FErrors.Add(TcxRegExprError.Create(FLine, FChar,
          cxGetResourceString(@scxRegExprIncorrectSpace)));
        Exit;
      end;

      case AToken of
        'x':
        begin
          ScanASCII(ALine, AChar);
          Exit;
        end;
        'w':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcId, AToken));
          Exit;
        end;
        'W':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcNotId, AToken));
          Exit;
        end;
        'd':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcDigit, AToken));
          Exit;
        end;
        'D':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcNotDigit, AToken));
          Exit;
        end;
        's':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSpace, AToken));
          Exit;
        end;
        'S':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcNotSpace, AToken));
          Exit;
        end;
        't':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #9));
          Exit;
        end;
        'n':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #10));
          Exit;
        end;
        'r':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #13));
          Exit;
        end;
        'f':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #12));
          Exit;
        end;
        'a':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #7));
          Exit;
        end;
        'e':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, #27));
          Exit;
        end;
        'p':
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol,
            dxFormatSettings.DecimalSeparator));
          Exit;
        end
        else
        begin
          FLexems.Add(CreateLexem(ALine, AChar, relcSymbol, AToken));
          Exit;
        end;
      end;
    end;
  end;
end;

function TcxRegExpr.ScanInteger(ALine, AChar: Integer; var AToken: Char): Boolean;
var
  AValue: string;
begin
  AValue := AToken;
  while GetToken(AToken) do
  begin
    if Decimal(AToken) then
      AValue := AValue + AToken
    else
    begin
      FLexems.Add(CreateLexem(ALine, AChar, relcInteger, AValue));
      Result := True;
      Exit;
    end;
  end;

  FLexems.Add(CreateLexem(ALine, AChar, relcInteger, AValue));
  Result := False;
end;

procedure TcxRegExpr.ScanQuantifier;
var
  AToken: Char;
  ALexem: TcxLexem;
begin
  ALexem.Value := '';

  while GetToken(AToken) do
  begin
    if Space(AToken) then
      Continue
    else if Decimal(AToken) then
    begin
      if not ScanInteger(FLine, FChar, AToken) then
        Break;
      if Space(AToken) then
        Continue;
    end;

    case AToken of
      '''':
        FErrors.Add(TcxRegExprError.Create(FLine, FChar,
          Format(cxGetResourceString(@scxRegExprIllegalSymbol), [' '' '])));
      ',':
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
      '}':
      begin
        FLexems.Add(CreateLexem(FLine, FChar, relcSpecial, AToken));
        Exit;
      end;
      else
        FErrors.Add(TcxRegExprError.Create(FLine, FChar,
          Format(cxGetResourceString(@scxRegExprIllegalSymbol), [AToken])));
    end;
  end;

  FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
    Format(cxGetResourceString(@scxRegExprMissing), ['}'])));
end;

procedure TcxRegExpr.ScanString;
var
  AToken: Char;
begin
  while GetToken(AToken) do
  begin
    if AToken = '''' then
    begin
      if LookToken(AToken, 0) then
      begin
        if AToken = '''' then
        begin
          FLexems.Add(CreateLexem(FLine, FChar, relcSymbol, ''''));
          GetToken(AToken);
          Continue;
        end
        else
          Exit;
      end
      else
        Exit;
    end
    else
      FLexems.Add(CreateLexem(FLine, FChar, relcSymbol, AToken));
  end;

  FErrors.Add(TcxRegExprError.Create(FLine, FChar + 1,
    Format(cxGetResourceString(@scxRegExprMissing), [' '' '])));
end;

procedure TcxRegExpr.SetUpdateOn(AUpdateOn: Boolean);
begin
  FUpdateOn := AUpdateOn;
  if FCompiled then
    if FUpdateOn then
      FAutomat.Update;
end;

function TcxRegExpr.Space(AToken: Char): Boolean;
begin
  Result := IsSpaceChar(AToken);
end;

procedure TcxRegExpr.SymbolDelete;
begin
  if Assigned(FOnSymbolDelete) then
    FOnSymbolDelete;
end;

procedure TcxRegExpr.SymbolUpdate(ASymbol: Char);
begin
  if Assigned(FOnSymbolUpdate) then
    FOnSymbolUpdate(ASymbol);
end;

procedure TcxRegExpr.TestCompiledStatus;
begin
  if not FCompiled then
    raise EcxEditError.Create(cxGetResourceString(@scxRegExprNotCompiled));
end;

function InternalIsTextValid(const AText, AMask: string; AIsFull: Boolean): Boolean;
var
  ARegExpr: TcxRegExpr;
  C: Char;
  I: Integer;
begin
{$IFNDEF DELPHI102TOKYO}
  Result := False;
{$ENDIF}

  ARegExpr := TcxRegExpr.Create;
  try
    try
      ARegExpr.Compile(AMask);

      Result := True;
      for I := 1 to Length(AText) do
      begin
        C := AText[I];
        if not ARegExpr.Next(C) then
        begin
          Result := False;
          Break;
        end;
      end;

      if AIsFull and Result and not ARegExpr.IsFinal then
        Result := False;
    except
      on E: EcxRegExprError do
        raise EcxRegExprError.Create(E.Errors.Clone);
    end;
  finally
    ARegExpr.Free;
  end;
end;

function IsTextFullValid(const AText, AMask: string): Boolean;
begin
  Result := InternalIsTextValid(AText, AMask, True);
end;

function IsTextValid(const AText, AMask: string): Boolean;
begin
  Result := InternalIsTextValid(AText, AMask, False);
end;

end.
