{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetCoreFormulasParser;

{$I cxVer.Inc}

interface

uses
  Types, Windows, SysUtils, Classes, Generics.Collections, Generics.Defaults,
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;


type
  TdxSpreadSheetParserTokenController = function(var APosition: Integer; ALength: Integer; out AName: TdxSpreadSheetFormulaToken): Boolean of object;

  { TdxSpreadSheetCustomFormulaParser }

  TdxSpreadSheetCustomFormulaParser = class
  strict private
    FFormatSettings: TdxSpreadSheetFormatSettings;
    FFormula: TdxSpreadSheetCustomFormula;
    FTokenControllers: TList<TdxSpreadSheetParserTokenController>;

    function GetR1C1Reference: Boolean;
    function InternalCheckOperation(const APosition: Integer): Boolean; inline;
    function IsSubStringBetweenMarkChars(ASubStringPos: Integer; const S: string): Boolean;
  protected
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FFormulaSourceText: string;
    FFormulaText: string;
    FOffset: Integer;

    procedure AddToken(var AList: TdxSpreadSheetFormulaToken; AToken: TdxSpreadSheetFormulaToken);
    procedure AddTokenController(AController: TdxSpreadSheetParserTokenController);
    procedure AddTokenFromStack(var AList, AStack: TdxSpreadSheetFormulaToken);

    function CleanLineBreaks(const S: string): string; virtual;
    function CleanSpaces(const S: string): string; virtual;

    function IsABCChar(APosition: Integer): Boolean; inline;
    function IsArray(var APosition: Integer; ALength: Integer; out AArray: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsArraySeparator(const APosition: Integer): Boolean; inline;
    function IsBoolean(var APosition: Integer; ALength: Integer; out ANumber: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsBreakChar(APosition: Integer): Boolean; inline;
    function IsDefinedName(var APosition: Integer; ALength: Integer; out AName: TdxSpreadSheetFormulaToken): Boolean; virtual;
    function IsError(var APosition: Integer; ALength: Integer; out AError: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsErrorReference(var APosition: Integer; AFinishPos: Integer): Boolean; inline;
    function IsFunction(var APosition: Integer; ALength: Integer; out AFunction: TdxSpreadSheetFormulaToken): Boolean;
    function IsFunctionInfo(const APosition, ALength: Integer; var ALeftParenthesisPos: Integer; out AInfo: TdxSpreadSheetFunctionInfo): Boolean;
    function IsListSeparator(const APosition: Integer): Boolean; inline;
    function IsNumber(var APosition: Integer; ALength: Integer; out ANumber: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsOperation(var APosition: Integer; ALength: Integer; out AOperation: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsReference(var APosition: Integer; ALength: Integer; out AReference: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsSeparator(var APosition: Integer): Boolean; inline;
    function IsString(var APosition: Integer; ALength: Integer; out AString: TdxSpreadSheetFormulaToken): Boolean; inline;
    function IsStringMark(APosition: Integer; ACheckAdditionalMark: Boolean = False): Boolean;
    function IsSubExpression(var APosition: Integer; ALength: Integer; out AExpression: TdxSpreadSheetFormulaToken): Boolean;
    function IsUnknown(var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean;

    function CheckAbsoluteReference(var AStartPos: Integer; AFinishPos: Integer; var AbsoluteReference: Boolean): Boolean; inline;
    function CheckAreaSeparator(var AStartPos: Integer; AFinishPos: Integer): Boolean; inline;
    function CheckColumnReference(var AStartPos: Integer; ALength: Integer; var AColumn: Integer): Boolean; inline;
    function CheckError: Boolean; virtual;
    function CheckExternalLink(var APosition: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean; virtual;
    function CheckExtraChars: Boolean; virtual;
    function CheckFullColumnRowReference(var AStartPos: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink;
      var ARow, ACol, ARow2, ACol2: Integer; var AAbsRow, AAbsCol, AAbsRow2, AAbsCol2: Boolean): Boolean; inline;
    function CheckRCReference(var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean; inline;
    function CheckRCReferencePart(var APosition: Integer; AFinishPos: Integer; const APrefix: string; var AIndex: Integer; var AAbsIndex: Boolean): Boolean; inline;
    function CheckReference(var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean; inline;
    function CheckReferenceToView(var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer;
      var AbsRow, AbsColumn: Boolean; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean; virtual;
    function CheckRowReference(var AStartPos, AFinishPos: Integer; var ARow: Integer): Boolean; inline;
    function CheckSignChar(const APosition: Integer): Boolean; inline;
    function CheckStandardReference(var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean; inline;
    function CheckText(const APosition: Integer; const ACandidate: string): Boolean; inline;
    function CheckTokenEnd(const APosition, AFinishPos: Integer): Boolean; inline;
    function CheckUnaryOperation(AValue: TdxSpreadSheetFormulaToken; AOperation: TdxSpreadSheetFormulaOperationToken): Boolean; inline;
    function CheckViewName(var APosition: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean; inline;

    function DoFullParse(AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken; virtual;
    function DoParse(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken; virtual;

    function GetError(var AStartPos, AFinishPos: Integer; var ACode: TdxSpreadSheetFormulaErrorCode): Boolean;
    function GetIntReference(var AStartPos, AFinishPos: Integer; var AValue: Integer; const AIsSignForbidden: Boolean = False): Boolean; inline;
    function GetIntValue(var AStartPos, AFinishPos: Integer; var AValue: Integer; const AIsSignForbidden: Boolean = False): Boolean; inline;
    function GetNextToken(var APosition: Integer; const ALength: Integer): TdxSpreadSheetFormulaToken;
    function GetParameters(AParent: TdxSpreadSheetFormulaToken; AStartPos, AFinishPos: Integer): Boolean;
    function GetStringLength(const ACheckedMarkChar: string; APosition, ALength: Integer): Integer; inline;
    function GetSubExpressionLength(const AClosingParenthesis: string; const APosition, ALength: Integer): Integer; inline;
    function GetViewByName(APosition, ALength: Integer; out AView: TObject): Boolean; virtual;

    procedure Initialize(const AText: string; AAnchorRow, AAnchorColumn: Integer);
    function MakeReference(ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink; ARow, ACol, ARow2, ACol2: Integer;
      AAbsRow, AAbsCol, AAbsRow2, AAbsCol2: Boolean; AIsArea: Boolean): TdxSpreadSheetFormulaToken; inline;
    function PrepareTokensFromString(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken; virtual;
    procedure SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode = ecNone); virtual;
    procedure ValidateReference(AIndex: Integer; var AReference: Integer; AIsAbsolute: Boolean);
    procedure ValidateR1C1Reference(AIndex: Integer; var AReference: Integer; AIsAbsolute: Boolean);

    procedure RegisterTokenControllers; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function ParseFormula(const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean; virtual;

    property AnchorColumn: Integer read FAnchorColumn;
    property AnchorRow: Integer read FAnchorRow;
    property FormatSettings: TdxSpreadSheetFormatSettings read FFormatSettings;
    property Formula: TdxSpreadSheetCustomFormula read FFormula;
    property R1C1Reference: Boolean read GetR1C1Reference;
  end;

function dxSpreadSheetFormulaExcludeEqualSymbol(const S: string): string;
function dxSpreadSheetFormulaIncludeEqualSymbol(const S: string): string;
function dxTryStringToReferenceArea(const S: string; out AArea: TRect): Boolean; overload;
function dxTryStringToReferenceArea(const S: string; R1C1ReferenceStyle: Boolean; out AArea: TRect): Boolean; overload;
implementation

uses
  dxSpreadSheetCoreStrs, Math, dxCore, StrUtils, cxGeometry;

type
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);

function dxTryStringToReferenceArea(const S: string; R1C1ReferenceStyle: Boolean; out AArea: TRect): Boolean;
var
  ARect: TRect;
  AFormula: TdxSpreadSheetCustomFormula;
  AParser: TdxSpreadSheetCustomFormulaParser;
begin
  if R1C1ReferenceStyle then
  begin
    AParser := TdxSpreadSheetCustomFormulaParser.Create;
    try
      AParser.FormatSettings.R1C1Reference := True;
      AFormula := TdxSpreadSheetCustomFormula.Create;
      try
        if AParser.ParseFormula(dxSpreadSheetFormulaIncludeEqualSymbol(S), AFormula) then
        begin
          ARect := cxInvalidRect;
          AFormula.EnumReferences(
            procedure (const R: TRect; View: TObject)
            begin
              ARect := R;
            end);
          AArea := ARect;
        end;
      finally
        AFormula.Free;
      end;
    finally
      AParser.Free;
    end;
  end
  else
    AArea := dxStringToReferenceArea(StringReplace(S, '$', '', [rfReplaceAll]));

  Result := dxSpreadSheetIsValidArea(AArea) and
    InRange(AArea.Top, 0, dxSpreadSheetMaxRowIndex) and
    InRange(AArea.Left, 0, dxSpreadSheetMaxColumnIndex);
end;

function dxTryStringToReferenceArea(const S: string; out AArea: TRect): Boolean;
begin
  Result := dxTryStringToReferenceArea(S, False, AArea) or dxTryStringToReferenceArea(S, True, AArea);
end;

function dxSpreadSheetFormulaExcludeEqualSymbol(const S: string): string;
begin
  if (Length(S) > 0) and (S[1] = dxDefaultOperations[opEQ]) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

function dxSpreadSheetFormulaIncludeEqualSymbol(const S: string): string;
begin
  if (Length(S) > 0) and (S[1] <> dxDefaultOperations[opEQ]) then
    Result := dxDefaultOperations[opEQ] + S
  else
    Result := S;
end;

{ TdxSpreadSheetCustomFormulaParser }

constructor TdxSpreadSheetCustomFormulaParser.Create;
begin
  FFormatSettings := TdxSpreadSheetFormatSettings.Create;
  FTokenControllers := TList<TdxSpreadSheetParserTokenController>.Create;
end;

destructor TdxSpreadSheetCustomFormulaParser.Destroy;
begin
  FreeAndNil(FTokenControllers);
  FreeAndNil(FFormatSettings);
  inherited;
end;

procedure TdxSpreadSheetCustomFormulaParser.AfterConstruction;
begin
  inherited;
  RegisterTokenControllers;
end;

function TdxSpreadSheetCustomFormulaParser.ParseFormula(
  const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean;
begin
  FFormula := AFormula;
  Result := (Length(AFormulaText) >= 1) and (AFormulaText[1] = FormatSettings.Operations[opEQ]);
  if Result then
  begin
    Initialize(Copy(AFormulaText, 2, MaxInt), Formula.AnchorRow, Formula.AnchorColumn);
    FFormula.SourceText := FFormulaSourceText;
    if CheckExtraChars then
    begin
      TFormulaAccess(FFormula).DestroyTokens;
      TFormulaAccess(FFormula).FTokens := DoFullParse(1, Length(FFormulaText));
    end;
  end;
end;

procedure TdxSpreadSheetCustomFormulaParser.AddToken(var AList: TdxSpreadSheetFormulaToken; AToken: TdxSpreadSheetFormulaToken);
begin
  if AList = nil then
    AList := AToken
  else
    TdxSpreadSheetFormulaToken.Add(AList.LastSibling, AToken);
end;

procedure TdxSpreadSheetCustomFormulaParser.AddTokenController(AController: TdxSpreadSheetParserTokenController);
begin
  FTokenControllers.Add(AController);
end;

procedure TdxSpreadSheetCustomFormulaParser.AddTokenFromStack(var AList, AStack: TdxSpreadSheetFormulaToken);
var
  AItem: TdxSpreadSheetFormulaToken;
begin
  AItem := AStack.LastSibling;
  AStack := AItem.Prev;
  AItem.ExtractFromList;
  AddToken(AList, AItem);
end;

function TdxSpreadSheetCustomFormulaParser.CleanLineBreaks(const S: string): string;
const
  ALineBreak = #13#10;
var
  P: Integer;
begin
  Result := S;
  P := PosEx(ALineBreak, Result, 1);
  while P > 0 do
  begin
    if IsSubStringBetweenMarkChars(P, Result) then
      Inc(P, Length(ALineBreak))
    else
      Delete(Result, P, Length(ALineBreak));
    P := PosEx(ALineBreak, Result, P);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CleanSpaces(const S: string): string;

  function HasSpecialSymbolAtLeft(ASpacePos: Integer; const S: string): Boolean;
  begin
    Result := (S[ASpacePos-1] = dxAreaSeparator) or (S[ASpacePos-1] = dxLeftParenthesis) or
     (S[ASpacePos-1] = dxLeftArrayParenthesis) or (S[ASpacePos-1] = FormatSettings.ListSeparator) or
     (S[ASpacePos-1] = dxRefSeparator);
  end;

  function HasSpecialSymbolAtRight(AFirstSpacePos: Integer; const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := AFirstSpacePos + 1 to Length(S) do
      if S[I] <> ' ' then
      begin
        Result := (S[I] = dxAreaSeparator) or (S[I] = dxRightParenthesis) or (S[I] = dxRightArrayParenthesis) ;
        Break;
      end;
  end;

var
  I: Integer;
begin
  I := 2;
  Result := S;
  while I <= Length(Result) do
    if (Result[I] = ' ') and (HasSpecialSymbolAtLeft(I, Result) or HasSpecialSymbolAtRight(I, Result)) and
       not IsSubStringBetweenMarkChars(I, Result)
    then
      Delete(Result, I, 1)
    else
      Inc(I);
end;

function TdxSpreadSheetCustomFormulaParser.IsABCChar(APosition: Integer): Boolean;
begin
  Result :=
    (Integer(FFormulaText[APosition]) >= Integer(WideChar('A'))) and
    (Integer(FFormulaText[APosition]) <= Integer(WideChar('Z')));
end;

function TdxSpreadSheetCustomFormulaParser.IsArray(var APosition: Integer;
  ALength: Integer; out AArray: TdxSpreadSheetFormulaToken): Boolean;
var
  L: Integer;
begin
  Result := CheckText(APosition, dxLeftArrayParenthesis);
  if Result then
  begin
    L := GetSubExpressionLength(dxRightArrayParenthesis, APosition, ALength) + APosition;
    AArray := TdxSpreadSheetFormulaArrayToken.Create();
    GetParameters(AArray, APosition + 1, L);
    APosition := L + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsArraySeparator(const APosition: Integer): Boolean;
begin
  Result := CheckText(APosition, FormatSettings.ArraySeparator);
end;

function TdxSpreadSheetCustomFormulaParser.IsBoolean(
  var APosition: Integer; ALength: Integer; out ANumber: TdxSpreadSheetFormulaToken): Boolean;
begin
  if CheckText(APosition, dxBoolToString[False]) then
    ANumber := TdxSpreadSheetFormulaBooleanValueToken.Create(False)
  else
  begin
    if CheckText(APosition, dxBoolToString[True]) then
      ANumber := TdxSpreadSheetFormulaBooleanValueToken.Create(True)
  end;
  Result := ANumber <> nil;
  if Result then
    Inc(APosition, Length(dxBoolToString[TdxSpreadSheetFormulaBooleanValueToken(ANumber).Value]))
end;

function TdxSpreadSheetCustomFormulaParser.IsBreakChar(APosition: Integer): Boolean;
var
  I: Integer;
begin
  Result := APosition <= Length(FFormulaText);
  if Result then
    for I := 1 to Length(FormatSettings.BreakChars) do
    begin
      Result := FFormulaText[APosition] = FormatSettings.BreakChars[I];
      if Result then
        Break;
    end;
end;

function TdxSpreadSheetCustomFormulaParser.IsDefinedName(
  var APosition: Integer; ALength: Integer; out AName: TdxSpreadSheetFormulaToken): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomFormulaParser.IsError(
  var APosition: Integer; ALength: Integer; out AError: TdxSpreadSheetFormulaToken): Boolean;
var
  ASavePos: Integer;
  ALastPos: Integer;
  ACode: TdxSpreadSheetFormulaErrorCode;
begin
  ASavePos := APosition;
  ALastPos := APosition + ALength;
  Result := GetError(APosition, ALastPos, ACode) and
    ((APosition >= ALastPos) or (not CheckText(APosition, dxAreaSeparator) and IsBreakChar(APosition)));
  if Result then
    AError := TdxSpreadSheetFormulaErrorValueToken.Create(ACode)
  else
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.IsErrorReference(var APosition: Integer; AFinishPos: Integer): Boolean;
var
  ASavePos: Integer;
  ACode: TdxSpreadSheetFormulaErrorCode;
begin
  ASavePos := APosition;
  Result := GetError(APosition, AFinishPos, ACode) and (ACode = ecRefErr);
  if not Result then
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.IsFunction(
  var APosition: Integer; ALength: Integer; out AFunction: TdxSpreadSheetFormulaToken): Boolean;
var
  ALeftParenthesisPos, J: Integer;
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  Result := IsFunctionInfo(APosition, ALength, ALeftParenthesisPos, AInfo);
  if Result then
  begin
    AFunction := TdxSpreadSheetFormulaFunctionToken.Create(AInfo);
    J := GetSubExpressionLength(dxRightParenthesis, ALeftParenthesisPos - 1,
      ALength - (ALeftParenthesisPos - 1 - APosition)) + ALeftParenthesisPos - 1;
    GetParameters(AFunction, ALeftParenthesisPos, J);
    TdxSpreadSheetFormulaFunctionToken(AFunction).InitializeParamInfo;
    APosition := J + 2;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsFunctionInfo(
  const APosition, ALength: Integer; var ALeftParenthesisPos: Integer; out AInfo: TdxSpreadSheetFunctionInfo): Boolean;
begin
  Result := False;
  ALeftParenthesisPos := APosition + 1;
  while not Result and (ALeftParenthesisPos < APosition + ALength) do
  begin
    Result := CheckText(ALeftParenthesisPos, dxLeftParenthesis);
    Inc(ALeftParenthesisPos);
  end;
  if not Result then Exit;
  AInfo := dxSpreadSheetFunctionsRepository.GetInfoByName(@FFormulaText[APosition], ALeftParenthesisPos - APosition - 1);
  Result := AInfo <> nil;
end;

function TdxSpreadSheetCustomFormulaParser.IsListSeparator(const APosition: Integer): Boolean;
begin
  Result := CheckText(APosition, FormatSettings.ListSeparator);
end;

function TdxSpreadSheetCustomFormulaParser.IsNumber(var APosition: Integer; ALength: Integer; out ANumber: TdxSpreadSheetFormulaToken): Boolean;
var
  AFloatValue: Double;
  AHasSeparator, AHasExponent, AHasExponentSign: Boolean;
  AIntegerValue: Integer;
  ANumberCharCount: Integer;
  APartAsString: string;
  I: Integer;
begin
  ANumber := nil;
  I := APosition;
  if not IsBoolean(APosition, ALength, ANumber) then
  begin
    ANumberCharCount := 0;
    AHasExponent := False;
    AHasSeparator := False;
    AHasExponentSign := False;
    while I <= APosition + ALength do
    begin
      if dxWideIsNumeric(FFormulaText[I]) then
        Inc(ANumberCharCount)
      else if not AHasSeparator and CheckText(I, FormatSettings.DecimalSeparator) then
        AHasSeparator := True
      else if not AHasExponent and CheckText(I, dxExponentChar) and (ANumberCharCount > 0) then
        AHasExponent := True
      else if (AHasExponent and not AHasExponentSign) and CheckSignChar(I) then
        AHasExponentSign := True
      else
      begin
        if (I < APosition + ALength) and not (
          CheckText(I, dxRightParenthesis) or
          CheckText(I, dxRightArrayParenthesis) or
          CheckText(I, FormatSettings.ListSeparator) or
          CheckText(I, FormatSettings.ArraySeparator) or
          InternalCheckOperation(I))
        then
          ANumberCharCount := 0;
        Break;
      end;
      Inc(I);
    end;
    AHasSeparator := AHasSeparator or AHasExponent;
    if (ANumberCharCount > 0) and not CheckText(I, dxAreaSeparator) then
    begin
      APartAsString := Copy(FFormulaText, APosition, I - APosition);
      if not AHasSeparator and TryStrToInt(APartAsString, AIntegerValue) then
        ANumber := TdxSpreadSheetFormulaIntegerValueToken.Create(AIntegerValue)
      else
        if dxTryStrToFloat(APartAsString, AFloatValue, FormatSettings.Data) then
          ANumber := TdxSpreadSheetFormulaFloatValueToken.Create(AFloatValue);
    end;
  end
  else
    I := APosition;
  Result := ANumber <> nil;
  if Result then
    APosition := I;
end;

function TdxSpreadSheetCustomFormulaParser.IsOperation(
  var APosition: Integer; ALength: Integer; out AOperation: TdxSpreadSheetFormulaToken): Boolean;
var
  I, J: TdxSpreadSheetFormulaOperation;
begin
  Result := False;
  AOperation := nil;
  J := Low(FormatSettings.Operations);
  for I := Low(FormatSettings.Operations) to High(FormatSettings.Operations) do
    if CheckText(APosition, FormatSettings.Operations[I]) and
      (not Result or (Length(FormatSettings.Operations[J]) < Length(FormatSettings.Operations[I]))) then
    begin
      J := I;
      Result := True;
    end;
  if Result then
  begin
    if J = opIsect then
      AOperation := TdxSpreadSheetFormulaAttributeToken.Create
    else
      AOperation := TdxSpreadSheetFormulaOperationToken.Create(J);
    Inc(APosition, Length(FormatSettings.Operations[J]));
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsReference(
  var APosition: Integer; ALength: Integer; out AReference: TdxSpreadSheetFormulaToken): Boolean;
var
  AAbsRow, AAbsRow2, AAbsCol, AAbsCol2: Boolean;
  AInfo: TdxSpreadSheetFunctionInfo;
  AIsArea: Boolean;
  ALeftParenthesisPos: Integer;
  ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink;
  ASavePos, AFinishPos, ARow, ARow2, ACol, ACol2: Integer;
begin
  // todo: need check maximum XFD column number
  AIsArea := True;
  ASavePos := APosition;
  AFinishPos := APosition + ALength - 1;
  ALink := nil;
  ALink2 := nil;
  Result := CheckReferenceToView(APosition, AFinishPos, ARow, ACol, AAbsRow, AAbsCol, ALink);

  if not Result then
    Result := CheckFullColumnRowReference(APosition, AFinishPos, ALink2, ARow, ACol, ARow2, ACol2, AAbsRow, AAbsCol, AAbsRow2, AAbsCol2)
  else
    if CheckAreaSeparator(APosition, AFinishPos) then
    begin
      CheckViewName(APosition, AFinishPos, ALink2);
      if not CheckReference(APosition, AFinishPos, ARow2, ACol2, AAbsRow2, AAbsCol2) then
      begin
        Result := IsFunctionInfo(APosition, ALength, ALeftParenthesisPos, AInfo);
        Dec(APosition);
        AIsArea := False;
      end;
    end
    else
      AIsArea := False;

  if Result then
    AReference := MakeReference(ALink, ALink2, ARow, ACol, ARow2, ACol2, AAbsRow, AAbsCol, AAbsRow2, AAbsCol2, AIsArea)
  else
    if (ALink <> nil) or (ALink2 <> nil) then
    begin
      FreeAndNil(ALink);
      FreeAndNil(ALink2);
      SetErrorIndex(APosition);
    end;

  if not Result then
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.IsSeparator(var APosition: Integer): Boolean;
begin
  Result := FFormulaText[APosition] = FormatSettings.ListSeparator;
end;

function TdxSpreadSheetCustomFormulaParser.IsString(
  var APosition: Integer; ALength: Integer; out AString: TdxSpreadSheetFormulaToken): Boolean;
var
  L: Integer;
begin
  Result := IsStringMark(APosition);
  if Result then
  begin
    L := GetStringLength(dxStringMarkChar, APosition, ALength);
    if ((L + 2) < ALength) and CheckText(APosition + L + 2, dxRefSeparator) then
      Exit(False);

    Result := not CheckText(APosition + L + 2, dxRefSeparator);
    if Result then
    begin
      AString := TdxSpreadSheetFormulaStringValueToken.Create(
        dxSpreadSheetUnescapeString(Copy(FFormulaSourceText, APosition + 1, L)));
      Inc(APosition, L + 2);
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsStringMark(APosition: Integer; ACheckAdditionalMark: Boolean): Boolean;
begin
  Result := CheckText(APosition, dxStringMarkChar) or ACheckAdditionalMark and CheckText(APosition, dxStringMarkChar2);
end;

function TdxSpreadSheetCustomFormulaParser.IsSubExpression(var APosition: Integer;
  ALength: Integer; out AExpression: TdxSpreadSheetFormulaToken): Boolean;
var
  L: Integer;
begin
  Result := CheckText(APosition, dxLeftParenthesis);
  if Result then
  begin
    L := GetSubExpressionLength(dxRightParenthesis, APosition, ALength);
    AExpression := TdxSpreadSheetFormulaParenthesesToken.Create();
    if L > 0 then
      TdxSpreadSheetFormulaToken.AddChild(AExpression, DoFullParse(APosition + 1, APosition + L));
    Inc(APosition, L + 2);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.IsUnknown(
  var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean;
var
  AFinishPos: Integer;
  AIndex1: Integer;
  AIndex2: Integer;
  ALink: TdxSpreadSheet3DReferenceCustomLink;
  AName: string;
begin
  ALink := nil;
  AFinishPos := APosition + ALength - 1;

  CheckExternalLink(APosition, AFinishPos, ALink);
  if CheckText(APosition, dxRefSeparator) then
    Inc(APosition);

  AIndex1 := APosition;
  while ((FFormulaText[AIndex1] = ' ') or not IsBreakChar(AIndex1)) and (AIndex1 <= APosition + ALength) do
    Inc(AIndex1);
  Result := AIndex1 > APosition;
  if not Result then
  begin
    FreeAndNil(ALink);
    Exit;
  end;

  AName := Copy(FFormulaSourceText, APosition, AIndex1 - APosition);
  if CheckText(AIndex1, dxLeftParenthesis) then
  begin
    AToken := TdxSpreadSheetFormulaUnknownFunctionToken.Create(AName);
    AIndex2 := GetSubExpressionLength(dxRightParenthesis, AIndex1, ALength - (AIndex1 - APosition)) + AIndex1;
    GetParameters(AToken, AIndex1 + 1, AIndex2);
    APosition := AIndex2 + 2;
  end
  else
  begin
    AToken := TdxSpreadSheetFormulaUnknownNameToken.Create(AName);
    APosition := AIndex1;
  end;
  if ALink <> nil then
    TdxSpreadSheetFormulaUnknownNameToken(AToken).Link := ALink;
end;

function TdxSpreadSheetCustomFormulaParser.CheckAbsoluteReference(
  var AStartPos: Integer; AFinishPos: Integer; var AbsoluteReference: Boolean): Boolean;
begin
  Result := AStartPos <= AFinishPos;
  if Result then
  begin
    AbsoluteReference := CheckText(AStartPos, dxAbsoluteReferenceChar);
    if AbsoluteReference then
      Inc(AStartPos);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CheckAreaSeparator(var AStartPos: Integer; AFinishPos: Integer): Boolean;
begin
  Result := (AStartPos < AFinishPos) and CheckText(AStartPos, dxAreaSeparator);
  if Result then
    Inc(AStartPos);
end;

function TdxSpreadSheetCustomFormulaParser.CheckColumnReference(var AStartPos: Integer; ALength: Integer; var AColumn: Integer): Boolean;
var
  I: Integer;
  AName: string;
begin
  Result := ALength > 0;
  if Result then
  begin
    I := AStartPos;
    repeat
      if IsABCChar(I) then
        Inc(I)
      else
      begin
        Dec(I);
        Break;
      end;
    until I > (AStartPos + ALength);
    if I > (AStartPos + ALength) then
      Dec(I);
    I := I - AStartPos + 1;
    Result := (I > 0) and (I <= 3);
    if Result then
    begin
      AName := Copy(FFormulaText, AStartPos, I);
      AColumn := TdxSpreadSheetColumnHelper.IndexByName(AName, False);
      Inc(AStartPos, I);
    end;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CheckFullColumnRowReference(
  var AStartPos: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink;
  var ARow, ACol, ARow2, ACol2: Integer; var AAbsRow, AAbsCol, AAbsRow2, AAbsCol2: Boolean): Boolean;
var
  ASavePos: Integer;
begin
  ASavePos := AStartPos;
  ARow := 0;
  ACol := 0;
  ARow2 := MaxInt;
  ACol2 := MaxInt;
  if R1C1Reference then
  begin
    Result := CheckRCReferencePart(AStartPos, AFinishPos, dxRCRowReferenceChar, ARow, AAbsRow) and
      CheckTokenEnd(AStartPos - 1, AFinishPos);
    if Result then
    begin
      if CheckAreaSeparator(AStartPos, AFinishPos) then
        Result := CheckRCReferencePart(AStartPos, AFinishPos, dxRCRowReferenceChar, ARow2, AAbsRow2)
      else
      begin
        ARow2 := ARow;
        AAbsRow2 :=  AAbsRow;
      end;
    end
    else
    begin
      Result := CheckRCReferencePart(AStartPos, AFinishPos, dxRCColumnReferenceChar, ACol, AAbsCol) and
        CheckTokenEnd(AStartPos - 1, AFinishPos);
      if CheckAreaSeparator(AStartPos, AFinishPos) then
        Result := CheckRCReferencePart(AStartPos, AFinishPos, dxRCColumnReferenceChar, ACol2, AAbsCol2)
      else
      begin
        ACol2 := ACol;
        AAbsCol2 :=  AAbsCol;
      end;
    end;
  end
  else
  begin
    Result := CheckAbsoluteReference(AStartPos, AFinishPos, AAbsCol) and
      CheckColumnReference(AStartPos, AFinishPos, ACol);
    if Result then
    begin
      Result := CheckAreaSeparator(AStartPos, AFinishPos);
      if Result then
        CheckViewName(AStartPos, AFinishPos, ALink);
      Result := Result and CheckAbsoluteReference(AStartPos, AFinishPos, AAbsCol2) and
        CheckColumnReference(AStartPos, AFinishPos, ACol2);
    end
    else
    begin
      AStartPos := ASavePos;
      Result := CheckAbsoluteReference(AStartPos, AFinishPos, AAbsRow) and
        CheckRowReference(AStartPos, AFinishPos, ARow);
      if Result then
      begin
        Result := CheckAreaSeparator(AStartPos, AFinishPos);
        if Result then
          CheckViewName(AStartPos, AFinishPos, ALink);
        Result := Result and CheckAbsoluteReference(AStartPos, AFinishPos, AAbsRow2) and
          CheckRowReference(AStartPos, AFinishPos, ARow2);
      end;
    end;
  end;
  if not Result then
    AStartPos := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.CheckError: Boolean;
begin
  Result := Formula.ErrorIndex = 0;
end;

function TdxSpreadSheetCustomFormulaParser.CheckExternalLink(
  var APosition: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomFormulaParser.CheckExtraChars: Boolean;
var
  AIndex: Integer;
begin
  AIndex := 1;
  while (AIndex <= Length(FFormulaText)) and CheckError do
  begin
    if IsStringMark(AIndex) then
      Inc(AIndex, GetStringLength(dxStringMarkChar, AIndex, Length(FFormulaText)) + 1)
    else if CheckText(AIndex, dxStringMarkChar2) then
      Inc(AIndex, GetStringLength(dxStringMarkChar2, AIndex, Length(FFormulaText)) + 1)
    else if FFormulaText[AIndex] = dxLeftParenthesis then
      Inc(AIndex, GetSubExpressionLength(dxRightParenthesis, AIndex, Length(FFormulaText)) + 1)
    else if FFormulaText[AIndex] = dxLeftArrayParenthesis then
      Inc(AIndex, GetSubExpressionLength(dxRightArrayParenthesis, AIndex, Length(FFormulaText)) + 1);

    Inc(AIndex);
  end;
  Result := (AIndex - Length(FFormulaText) = 1) and CheckError;
  if not Result and CheckError then
    SetErrorIndex(Length(FFormulaText));
end;

function TdxSpreadSheetCustomFormulaParser.CheckReference(var APosition: Integer;
  AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean;
var
  ASavePos: Integer;
begin
  ASavePos := APosition;
  AColumn := MinInt;
  ARow := MinInt;
  Result := IsErrorReference(APosition, AFinishPos);
  if Result then
    Exit;
  if R1C1Reference then
    Result := CheckRCReference(APosition, AFinishPos, ARow, AColumn, AbsRow, AbsColumn)
  else
    Result := CheckStandardReference(APosition, AFinishPos, ARow, AColumn, AbsRow, AbsColumn);

  if not Result then
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.CheckReferenceToView(
  var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer;
  var AbsRow, AbsColumn: Boolean; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
begin
  CheckViewName(APosition, AFinishPos, ALink);
  Result := CheckReference(APosition, AFinishPos, ARow, AColumn, AbsRow, AbsColumn);
end;

function TdxSpreadSheetCustomFormulaParser.CheckRowReference(var AStartPos, AFinishPos: Integer; var ARow: Integer): Boolean;
var
  ASavePos: Integer;
begin
  ASavePos := AStartPos;
  Result := GetIntReference(AStartPos, AFinishPos, ARow) and ((AStartPos > AFinishPos) or CheckTokenEnd(AStartPos - 1, AFinishPos));
  if not Result then
    AStartPos := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.CheckRCReference(var APosition: Integer;
  AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean;
begin
  Result :=
    CheckRCReferencePart(APosition, AFinishPos, dxRCRowReferenceChar, ARow, AbsRow) and
    CheckRCReferencePart(APosition, AFinishPos, dxRCColumnReferenceChar, AColumn, AbsColumn)
end;

function TdxSpreadSheetCustomFormulaParser.CheckRCReferencePart(var APosition: Integer;
  AFinishPos: Integer; const APrefix: string; var AIndex: Integer; var AAbsIndex: Boolean): Boolean;
var
  ASavePos: Integer;
begin
  ASavePos := APosition;
  Result := CheckText(APosition, APrefix);
  Inc(APosition);
  AAbsIndex := Result and not CheckText(APosition, dxReferenceLeftParenthesis);
  if not AAbsIndex then
    Inc(APosition);
  if Result then
  begin
    if AAbsIndex then
    begin
      AAbsIndex := GetIntReference(APosition, AFinishPos, AIndex, True);
      if not AAbsIndex then
        AIndex := 0;

      if (((APrefix = dxRCRowReferenceChar) and not CheckText(APosition, dxRCColumnReferenceChar)) or
         ((APrefix = dxRCColumnReferenceChar) and not CheckTokenEnd(APosition - 1, AFinishPos))) then
        Result := False;
    end
    else
      Result := GetIntReference(APosition, AFinishPos, AIndex) and CheckText(APosition, dxReferenceRightParenthesis)
  end;
  if Result and not AAbsIndex and CheckText(APosition, dxReferenceRightParenthesis) then
    Inc(APosition);
  if not Result then
    APosition := ASavePos
  else
    if AAbsIndex then
      Dec(AIndex);
end;

function TdxSpreadSheetCustomFormulaParser.CheckSignChar(const APosition: Integer): Boolean;
begin
  Result := CheckText(APosition, FormatSettings.Operations[opAdd]) or CheckText(APosition, FormatSettings.Operations[opSub]);
end;

function TdxSpreadSheetCustomFormulaParser.CheckStandardReference(
  var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer; var AbsRow, AbsColumn: Boolean): Boolean;
var
  ASavePos: Integer;
begin
  ASavePos := APosition;
  Result :=
    CheckAbsoluteReference(APosition, AFinishPos, AbsColumn) and CheckColumnReference(APosition, AFinishPos, AColumn) and
    CheckAbsoluteReference(APosition, AFinishPos, AbsRow) and CheckRowReference(APosition, AFinishPos, ARow);
  if not Result then
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.CheckText(const APosition: Integer; const ACandidate: string): Boolean;
begin
  Result := (Length(ACandidate) > 0) and (APosition + Length(ACandidate) - 1 <= Length(FFormulaText)) and
    CompareMem(@FFormulaText[APosition], @ACandidate[1], Length(ACandidate) * SizeOf(Char));
end;

function TdxSpreadSheetCustomFormulaParser.CheckTokenEnd(const APosition, AFinishPos: Integer): Boolean;
var
  Ch: WideChar;
begin
  Result := APosition >= AFinishPos;
  if not Result then
  begin
    Ch := WideChar(FFormulaText[APosition + 1]);
    Result := (Ch = dxRightParenthesis) or (Ch = dxRightArrayParenthesis) or
      CharInSet(Ch, ['+', '-', '*', '/', '^', '&', '<', '=', '>', ' ', ',', ':', ';', '%']);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CheckUnaryOperation(
  AValue: TdxSpreadSheetFormulaToken; AOperation: TdxSpreadSheetFormulaOperationToken): Boolean;
var
  ASign: TValueSign;
begin
  if AOperation.Operation = opUplus then
    ASign := 1
  else if AOperation.Operation = opUminus then
    ASign := -1
  else
    ASign := 0;

  Result := ASign <> 0;
  if Result then
  begin
    if (AValue is TdxSpreadSheetFormulaIntegerValueToken) and (Sign(TdxSpreadSheetFormulaIntegerValueToken(AValue).Value) <> ASign) then
      TdxSpreadSheetFormulaIntegerValueToken(AValue).Value := TdxSpreadSheetFormulaIntegerValueToken(AValue).Value * ASign
    else
      if (AValue is TdxSpreadSheetFormulaFloatValueToken) and (Sign(TdxSpreadSheetFormulaFloatValueToken(AValue).Value) <> ASign) then
        TdxSpreadSheetFormulaFloatValueToken(AValue).Value := TdxSpreadSheetFormulaFloatValueToken(AValue).Value * ASign
      else
        if (AValue is TdxSpreadSheetFormulaCurrencyValueToken) and (Sign(TdxSpreadSheetFormulaCurrencyValueToken(AValue).Value) <> ASign) then
          TdxSpreadSheetFormulaCurrencyValueToken(AValue).Value := TdxSpreadSheetFormulaCurrencyValueToken(AValue).Value * ASign
        else
          if (AValue is TdxSpreadSheetFormulaDateTimeValueToken) and (Sign(TdxSpreadSheetFormulaDateTimeValueToken(AValue).Value) <> ASign) then
            TdxSpreadSheetFormulaDateTimeValueToken(AValue).Value := TdxSpreadSheetFormulaDateTimeValueToken(AValue).Value * ASign
          else
            Result := False;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.CheckViewName(
  var APosition: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
var
  AHasRefSeparator: Boolean;
  AHasStringMark: Boolean;
  ASavePos: Integer;
  AView: TObject;
  L: Integer;
begin
  ALink := nil;
  ASavePos := APosition;
  AHasStringMark := IsStringMark(APosition, True);
  if AHasStringMark then
    Inc(APosition);

  Result := IsErrorReference(APosition, AFinishPos) and not IsBreakChar(APosition);
  if Result then
  begin
    ALink := TdxSpreadSheet3DReferenceLink.Create(TdxSpreadSheetInvalidObject.Instance);
    Exit;
  end;

  CheckExternalLink(APosition, AFinishPos, ALink);

  L := 0;
  AHasRefSeparator := CheckText(APosition + L, dxRefSeparator);
  while not AHasRefSeparator and (APosition + L + 1 < AFinishPos) do
  begin
    if APosition + L < AFinishPos then
    begin
      Inc(L);
      AHasRefSeparator := CheckText(APosition + L, dxRefSeparator);
    end;
  end;

  if ALink <> nil then
  begin
    Result := L > 0;
    if Result then
      TdxSpreadSheet3DReferenceCustomExternalLink(ALink).Name := Copy(FFormulaSourceText, APosition, L)
    else
      FreeAndNil(ALink);
  end
  else
  begin
    if AHasRefSeparator then
      Result := GetViewByName(APosition, L - Byte(AHasStringMark), AView);
    if Result then
      ALink := TdxSpreadSheet3DReferenceLink.Create(AView);
  end;

  if Result then
  begin
    Inc(APosition, L);
    if CheckText(APosition, dxRefSeparator) then
      Inc(APosition);
  end
  else
    APosition := ASavePos;
end;

function TdxSpreadSheetCustomFormulaParser.DoFullParse(AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken;
var
  ASavePos: Integer;
begin
  ASavePos := AStartPos;
  Result := DoParse(AStartPos, AFinishPos);
  if not CheckError or ((AStartPos < AFinishPos) and not IsListSeparator(AStartPos)) then
    SetErrorIndex(AStartPos)
  else
    if AStartPos < AFinishPos then
    begin
      AStartPos := ASavePos;
      TdxSpreadSheetFormulaToken.DestroyTokens(Result);
      Result := TdxSpreadSheetListToken.Create;
      Result.Owner := Formula;
      GetParameters(Result, AStartPos, AFinishPos);
    end;
end;

function TdxSpreadSheetCustomFormulaParser.DoParse(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken;

  function IsDoubleMinus(AStack, AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    Result := (AStack is TdxSpreadSheetFormulaOperationToken) and (AToken is TdxSpreadSheetFormulaOperationToken) and
      (TdxSpreadSheetFormulaOperationToken(AStack).Operation = opUminus) and
      (TdxSpreadSheetFormulaOperationToken(AToken).Operation = TdxSpreadSheetFormulaOperationToken(AStack).Operation);
  end;

var
  AToken, ANextToken, AStack: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  AStack := nil;
  AToken := PrepareTokensFromString(AStartPos, AFinishPos);
  if AToken = nil then
    Exit;
  while AToken <> nil do
  begin
    ANextToken := AToken.Next;
    AToken.ExtractFromList;
    if AToken.Priority = -1 then
      AddToken(Result, AToken)
    else
    begin
      AStack := AStack.LastSibling;
      while (AStack <> nil) and (AStack.Priority >= AToken.Priority) and not IsDoubleMinus(AStack, AToken) do
        AddTokenFromStack(Result, AStack);
      AddToken(AStack,  AToken);
    end;
    AToken := ANextToken;
  end;
  while AStack <> nil do
    AddTokenFromStack(Result, AStack);
end;

function TdxSpreadSheetCustomFormulaParser.GetError(
  var AStartPos, AFinishPos: Integer; var ACode: TdxSpreadSheetFormulaErrorCode): Boolean;

  function CheckCode(const AText: string; AValue: TdxSpreadSheetFormulaErrorCode): Boolean;
  begin
    Result := dxSpreadSheetCompareText(AText, @FFormulaText[AStartPos], Length(AText)) = 0;
    if Result then
    begin
      Inc(AStartPos, Length(AText));
      ACode := AValue;
    end;
  end;

begin
  Result := CheckText(AStartPos, dxErrorPrefix) and (
    CheckCode(serNullError, ecNull) or
    CheckCode(serDivZeroError, ecDivByZero) or
    CheckCode(serValueError, ecValue) or
    CheckCode(serRefError, ecRefErr) or
    CheckCode(serNameError, ecName) or
    CheckCode(serNumError, ecNUM) or
    CheckCode(serNAError, ecNA));
end;

function TdxSpreadSheetCustomFormulaParser.GetIntReference(var AStartPos, AFinishPos: Integer;
  var AValue: Integer; const AIsSignForbidden: Boolean = False): Boolean;
begin
  Result := GetIntValue(AStartPos, AFinishPos, AValue, AIsSignForbidden);
  if Result and not R1C1Reference then
    if AValue > 0 then
      Dec(AValue)
    else
      Result := False;
end;

function TdxSpreadSheetCustomFormulaParser.GetIntValue(
  var AStartPos, AFinishPos: Integer; var AValue: Integer; const AIsSignForbidden: Boolean = False): Boolean;
var
  APos, C: Integer;
  AHasSign: Boolean;
begin
  Result := AStartPos <= AFinishPos;
  if not Result then Exit;
  AValue := 0;
  AHasSign := CheckText(AStartPos, FormatSettings.Operations[opAdd]) or CheckText(AStartPos, FormatSettings.Operations[opSub]);
  Result := not(AHasSign and AIsSignForbidden);
  if Result then
  begin
    APos := AStartPos + Byte(AHasSign);
    repeat
      C := Integer(FFormulaText[APos]);
      if (C >= Integer(WideChar('0'))) and (C <= Integer(WideChar('9'))) then
        Inc(APos)
      else
      begin
        Dec(APos);
        Break;
      end;
    until APos > AFinishPos;
    if APos > AFinishPos then
      Dec(APos);
    Result := (APos - Byte(AHasSign) >= AStartPos ) and
      TryStrToInt(Copy(FFormulaText, AStartPos, APos - AStartPos + 1), AValue);
    if Result then
      AStartPos := APos + 1;
  end;
end;

function TdxSpreadSheetCustomFormulaParser.GetNextToken(var APosition: Integer; const ALength: Integer): TdxSpreadSheetFormulaToken;
var
  I: Integer;
begin
  Result := nil;
  if FFormulaText[APosition] = '' then
  begin
    Inc(APosition);
    Exit;
  end;
  for I := 0 to FTokenControllers.Count - 1 do
  begin
    if FTokenControllers[I](APosition, ALength, Result) then
      Break;
  end;
  if Result = nil then
    SetErrorIndex(APosition, ecName);
  Result.Owner := Formula;
end;

function TdxSpreadSheetCustomFormulaParser.GetParameters(AParent: TdxSpreadSheetFormulaToken; AStartPos, AFinishPos: Integer): Boolean;

  procedure AddParameter(ATokens: TdxSpreadSheetFormulaToken);
  var
    AParameter: TdxSpreadSheetFormulaToken;
  begin
    if CheckError then
    begin
      AParameter := TdxSpreadSheetFormulaToken.Create;
      TdxSpreadSheetFormulaToken.AddChild(AParameter, ATokens);
      TdxSpreadSheetFormulaToken.AddChild(AParent, AParameter);
    end
    else
      TdxSpreadSheetFormulaToken.DestroyTokens(ATokens);
  end;

begin
  while CheckError and (AStartPos <= AFinishPos) do
  begin
    AddParameter(DoParse(AStartPos, AFinishPos));
    if IsListSeparator(AStartPos) then
    begin
      while AStartPos <= AFinishPos do
      begin
        Inc(AStartPos);
        if (AStartPos > AFinishPos) or IsListSeparator(AStartPos) then
          AddParameter(TdxSpreadSheetFormulaNullToken.Create)
        else
          Break;
      end;
    end
    else
      if IsArraySeparator(AStartPos) then
      begin
        TdxSpreadSheetFormulaToken.AddChild(AParent, TdxSpreadSheetFormulaArrayRowSeparator.Create());
        Inc(AStartPos)
      end
      else
        if AStartPos < AFinishPos then
          SetErrorIndex(AStartPos)
        else
          Break;
  end;
  Result := CheckError;
end;

function TdxSpreadSheetCustomFormulaParser.GetStringLength(const ACheckedMarkChar: string; APosition, ALength: Integer): Integer;
var
  ADoubleCheckedMarkChar: string;
  L: Integer;
begin
  Result := 0;
  if CheckText(APosition + Result, ACheckedMarkChar) then
    Inc(APosition)
  else
    Exit;
  ADoubleCheckedMarkChar := ACheckedMarkChar + ACheckedMarkChar;
  L := Length(ADoubleCheckedMarkChar);
  while Result <= ALength do
  begin
    if CheckText(APosition + Result, ADoubleCheckedMarkChar) then
      Inc(Result, L);
    if CheckText(APosition + Result, ACheckedMarkChar) then
    begin
      if (Result < ALength) and CheckText(APosition + Result + 1, ACheckedMarkChar) then
        Inc(Result)
      else
        Break;
    end
    else
      Inc(Result)
  end;
  if Result > ALength then
    SetErrorIndex(APosition - 1);
end;

function TdxSpreadSheetCustomFormulaParser.GetSubExpressionLength(
  const AClosingParenthesis: string; const APosition, ALength: Integer): Integer;
var
  I, C: Integer;
begin
  C := 1;
  Result := -1;
  I := APosition + 1;
  while (I <= APosition + ALength) and CheckError do
  begin
    if CheckText(I, dxStringMarkChar) then
      Inc(I, GetStringLength(dxStringMarkChar, I, ALength - (I - APosition)) + 1)
    else

    if CheckText(I, dxStringMarkChar2) then
      Inc(I, GetStringLength(dxStringMarkChar2, I, ALength - (I - APosition)) + 1)
    else

    if CheckText(I, dxLeftParenthesis) then
      Inc(I, GetSubExpressionLength(dxRightParenthesis, I, ALength - (I - APosition)) + 1)
    else

    if CheckText(I, dxLeftArrayParenthesis) then
      Inc(I, GetSubExpressionLength(dxRightArrayParenthesis, I, ALength - (I - APosition)) + 1)
    else

    if CheckText(I, AClosingParenthesis) then
    begin
      Dec(C);
      if C = 0 then
      begin
        Result := I - APosition - 1;
        Break;
      end;
    end;
    Inc(I);
  end;

  if Result = -1 then
  begin
    Result := 0;
    if CheckError then
      SetErrorIndex(APosition);
  end;
end;

function TdxSpreadSheetCustomFormulaParser.GetViewByName(APosition, ALength: Integer; out AView: TObject): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomFormulaParser.Initialize(const AText: string; AAnchorRow, AAnchorColumn: Integer);
begin
  FAnchorRow := AAnchorRow;
  FAnchorColumn := AAnchorColumn;
  FFormulaSourceText := CleanLineBreaks(CleanSpaces(Trim(AText)));
  FFormulaText := WideUpperCase(FFormulaSourceText);
end;

function TdxSpreadSheetCustomFormulaParser.MakeReference(
  ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink; ARow, ACol, ARow2, ACol2: Integer;
  AAbsRow, AAbsCol, AAbsRow2, AAbsCol2: Boolean; AIsArea: Boolean): TdxSpreadSheetFormulaToken;
begin
  if R1C1Reference then
  begin
    ValidateR1C1Reference(AnchorRow, ARow, AAbsRow);
    ValidateR1C1Reference(AnchorColumn, ACol, AAbsCol);
    if AIsArea then
    begin
      ValidateR1C1Reference(AnchorRow, ARow2, AAbsRow2);
      ValidateR1C1Reference(AnchorColumn, ACol2, AAbsCol2);
    end;
  end
  else
  begin
    ValidateReference(AnchorRow, ARow, AAbsRow);
    ValidateReference(AnchorColumn, ACol, AAbsCol);
    if AIsArea then
    begin
      ValidateReference(AnchorRow, ARow2, AAbsRow2);
      ValidateReference(AnchorColumn, ACol2, AAbsCol2);
    end;
  end;

  if AIsArea then
  begin
    if (ALink <> nil) or (ALink2 <> nil) then
      Result := TdxSpreadSheetFormula3DAreaReference.Create(ALink, ALink2, ARow, ACol, ARow2, ACol2, AAbsRow, AAbsCol, AAbsRow2, AAbsCol2)
    else
      Result := TdxSpreadSheetFormulaAreaReference.Create(ARow, ACol, ARow2, ACol2, AAbsRow, AAbsCol, AAbsRow2, AAbsCol2)
  end
  else
  begin
    if ALink <> nil then
      Result := TdxSpreadSheetFormula3DReference.Create(ALink, ARow, ACol, AAbsRow, AAbsCol)
    else
      Result := TdxSpreadSheetFormulaReference.Create(ARow, ACol, AAbsRow, AAbsCol);
  end
end;

function TdxSpreadSheetCustomFormulaParser.PrepareTokensFromString(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken;
var
  AToken, APrevToken, ANextToken: TdxSpreadSheetFormulaToken;

  procedure RemoveToken;
  begin
    ANextToken := AToken.Next;
    if AToken = Result then
      Result := ANextToken;
    AToken.ExtractFromList;
    AToken.Free;
  end;

begin
  Result := nil;
  APrevToken := nil;
  // convert to tokens
  if IsListSeparator(AStartPos) or IsArraySeparator(AStartPos) then
    Result := TdxSpreadSheetFormulaNullToken.Create
  else
    while (AStartPos <= AFinishPos) and not (IsListSeparator(AStartPos) or IsArraySeparator(AStartPos)) do
    begin
      AToken := GetNextToken(AStartPos, AFinishPos - AStartPos + 1);
      if AToken = nil then
        Break;
      if Result = nil then
        Result := AToken
      else
        AddToken(APrevToken, AToken);
      APrevToken := AToken;
    end;

  AToken := Result;
  while AToken <> nil do
  begin
    AToken.CheckNeighbors;
    if AToken is TdxSpreadSheetFormulaAttributeToken then
    begin
      APrevToken := AToken.Prev;
      RemoveToken;
      if (APrevToken is TdxSpreadSheetFormulaReference) and (ANextToken is TdxSpreadSheetFormulaReference) then
      begin
         AToken := TdxSpreadSheetFormulaOperationToken.Create(opIsect);
         TdxSpreadSheetFormulaToken.Add(APrevToken, AToken);
         AToken.Owner := Formula;
      end;
      AToken := ANextToken;
    end
    else
      if AToken is TdxSpreadSheetFormulaOperationToken then
      begin
        if (TdxSpreadSheetFormulaOperationToken(AToken).Operation = opPercent) and (AToken.Prev = nil) then
        begin
          SetErrorIndex(AStartPos);
          AToken := nil;
        end
        else
          if CheckUnaryOperation(AToken.Next, TdxSpreadSheetFormulaOperationToken(AToken)) then
          begin
            RemoveToken;
            AToken := ANextToken;
          end
          else
            AToken := AToken.Next;
      end
      else
        AToken := AToken.Next;
  end;

  if Formula.ErrorIndex <> 0 then
    TdxSpreadSheetFormulaToken.DestroyTokens(Result);
end;

procedure TdxSpreadSheetCustomFormulaParser.SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode = ecNone);
begin
  TFormulaAccess(Formula).SetError(ACode, AErrorIndex);
  TdxSpreadSheetFormulaToken.DestroyTokens(TFormulaAccess(Formula).FTokens);
end;

procedure TdxSpreadSheetCustomFormulaParser.ValidateReference(AIndex: Integer; var AReference: Integer; AIsAbsolute: Boolean);
begin
  if (AReference = MaxInt) or (AReference = MinInt) then
    Exit;
  if not AIsAbsolute then
    Integer(AReference) := AReference - AIndex
end;

procedure TdxSpreadSheetCustomFormulaParser.ValidateR1C1Reference(AIndex: Integer; var AReference: Integer; AIsAbsolute: Boolean);
begin
  if (AReference = MaxInt) or (AReference = MinInt) then
    Exit;
{  if not AIsAbsolute then
    AReference := AReference + AIndex
  else
    Dec(AReference);}
end;

procedure TdxSpreadSheetCustomFormulaParser.RegisterTokenControllers;
begin
  AddTokenController(IsString);
  AddTokenController(IsSubExpression);
  AddTokenController(IsArray);
  AddTokenController(IsError);
  AddTokenController(IsOperation);
  AddTokenController(IsFunction);
  AddTokenController(IsNumber);
  AddTokenController(IsDefinedName);
  AddTokenController(IsReference);
  AddTokenController(IsUnknown);
end;

function TdxSpreadSheetCustomFormulaParser.GetR1C1Reference: Boolean;
begin
  Result := FormatSettings.R1C1Reference;
end;

function TdxSpreadSheetCustomFormulaParser.InternalCheckOperation(const APosition: Integer): Boolean;
var
  I: TdxSpreadSheetFormulaOperation;
begin
  Result := False;
  for I := Low(FormatSettings.Operations) to High(FormatSettings.Operations) do
    if CheckText(APosition, FormatSettings.Operations[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxSpreadSheetCustomFormulaParser.IsSubStringBetweenMarkChars(ASubStringPos: Integer; const S: string): Boolean;
var
  ACount, I: Integer;
begin
  ACount := 0;
  for I := 1 to ASubStringPos - 1 do
  begin
    if SameStr(S[I], dxStringMarkChar) then
      Inc(ACount);
  end;
  Result := Odd(ACount);
end;

end.
