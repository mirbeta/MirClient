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

unit dxSpreadSheetFormulas;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Variants, Classes, Types, StrUtils, dxCore, dxCoreClasses, cxClasses, cxFormats,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetUtils, dxSpreadSheetStrs, dxSpreadSheetClasses,
  cxVariants, Generics.Defaults, Generics.Collections, cxGeometry, dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens, dxSpreadSheetFunctions, dxSpreadSheetCoreFormulasParser;

{ Aliases }

type
  TdxSpreadSheetFunctionParamKind = dxSpreadSheetCoreFormulas.TdxSpreadSheetFunctionParamKind;
  TdxSpreadSheetFunctionParamKindInfo = dxSpreadSheetCoreFormulas.TdxSpreadSheetFunctionParamKindInfo;
  TdxSpreadSheetFunctionResultKind = dxSpreadSheetCoreFormulas.TdxSpreadSheetFunctionResultKind;

const
  frkArray = dxSpreadSheetCoreFormulas.frkArray;
  frkNonArrayValue = dxSpreadSheetCoreFormulas.frkNonArrayValue;
  frkValue = dxSpreadSheetCoreFormulas.frkValue;

  fpkArray = dxSpreadSheetCoreFormulas.fpkArray;
  fpkNonRequiredArray = dxSpreadSheetCoreFormulas.fpkNonRequiredArray;
  fpkNonRequiredUnlimited = dxSpreadSheetCoreFormulas.fpkNonRequiredUnlimited;
  fpkNonRequiredValue = dxSpreadSheetCoreFormulas.fpkNonRequiredValue;
  fpkUnlimited = dxSpreadSheetCoreFormulas.fpkUnlimited;
  fpkValue = dxSpreadSheetCoreFormulas.fpkValue;

type
  { TdxSpreadSheet3DExternalReferenceLink }

  TdxSpreadSheet3DExternalReferenceLink = class(TdxSpreadSheet3DReferenceCustomExternalLink)
  strict private
    function IsFileProtocol(const ATarget: string): Boolean;
  public
    function ToString: string; override;
  end;

  { TdxSpreadSheetDefinedNameToken }

  TdxSpreadSheetDefinedNameToken = class(TdxSpreadSheetCustomDefinedNameToken)
  strict private
    FDefinedNames: TdxSpreadSheetDefinedNames;
    FLink: TdxSpreadSheet3DReferenceLink;

    function GetDefinedName: TdxSpreadSheetDefinedName;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(const AValue: string; ADefinedNames: TdxSpreadSheetDefinedNames = nil; ALink: TdxSpreadSheet3DReferenceLink = nil); reintroduce;
    destructor Destroy; override;
    procedure EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False); override;
    function ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;  override;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function UpdateValue(ADefinedName: TdxSpreadSheetDefinedName; const ANewName: string): Boolean;
    //
    property DefinedName: TdxSpreadSheetDefinedName read GetDefinedName;
    property DefinedNames: TdxSpreadSheetDefinedNames read FDefinedNames;
    property Link: TdxSpreadSheet3DReferenceLink read FLink;
  end;

  { TdxSpreadSheetUnknownNameToken }

  TdxSpreadSheetUnknownNameToken = class(TdxSpreadSheetDefinedNameToken)
  public
    function ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;  override;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
  end;

  { TdxSpreadSheetFormulaParser }

  TdxSpreadSheetFormulaParser = class(TdxSpreadSheetCustomFormulaParser)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;

    function GetDefinedNames: TdxSpreadSheetDefinedNames; inline;
    function GetSheet: TdxSpreadSheetTableView;
  protected
    function CheckExternalLink(var APosition: Integer; AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean; override;
    function CheckReferenceToView(var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer;
      var AbsRow, AbsColumn: Boolean; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean; override;
    function IsDefinedName(var APosition: Integer; ALength: Integer; out AName: TdxSpreadSheetFormulaToken): Boolean; override;

    function GetStringPart(var APosition: Integer; ALength: Integer): string; inline;
    function GetSubString(const APosition, ALength: Integer): string; inline;
    function GetViewByName(APosition, ALength: Integer; out AView: TObject): Boolean; override;

    property DefinedNames: TdxSpreadSheetDefinedNames read GetDefinedNames;
    property Sheet: TdxSpreadSheetTableView read GetSheet;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); reintroduce; virtual;
    function ParseFormula(const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean; overload; override;
    function ParseFormula(const AFormulaText: string; ACell: TdxSpreadSheetCell): Boolean; reintroduce; overload; virtual;

    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

  { TdxSpreadSheetFormulaReferences }

  TdxSpreadSheetFormulaReferences = class(TdxSpreadSheetFormula)
  protected
    function GetExpressionAsText(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText; override;
  end;

  { TdxSpreadSheetFormulaReferencesParser }

  TdxSpreadSheetFormulaReferencesParser = class(TdxSpreadSheetFormulaParser)
  strict private
    function IsDelimiter(const C: Char): Boolean; inline;
  protected
    function CheckExtraChars: Boolean; override;
    function CleanLineBreaks(const S: string): string; override;
    function CleanSpaces(const S: string): string; override;
    function IsText(var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean;
    function PrepareTokensFromString(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken; override;
    procedure SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode = ecNone); override;
    procedure RegisterTokenControllers; override;
  end;

  { TdxSpreadSheetFormulaReferencesHelper }

  TdxSpreadSheetFormulaReferencesHelper = class
  public
    class function GetReferences(ACell: TdxSpreadSheetCell; const AText: string;
      AReferences: TdxRectList; out AFormattedText: TdxSpreadSheetFormulaFormattedText): Boolean;
  end;

implementation

uses
  Math, dxSpreadSheetCoreStrs;

type
  TFormulaAccess = class(TdxSpreadSheetFormula);
  TNameAccess = class(TdxSpreadSheetDefinedName);
  TResultAccess = class(TdxSpreadSheetFormulaResult);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetExternalLinksAccess = class(TdxSpreadSheetExternalLinks);

{ TdxSpreadSheetUnknownNameToken }

function TdxSpreadSheetUnknownNameToken.ExtractColumn(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  Result := TdxSpreadSheetSimpleVector.Create;
  AErrorCode := ecName;
end;

function TdxSpreadSheetUnknownNameToken.ExtractRow(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  Result := TdxSpreadSheetSimpleVector.Create;
  AErrorCode := ecName;
end;

procedure TdxSpreadSheetUnknownNameToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := 0;
  AErrorCode := ecName;
end;

{ TdxSpreadSheet3DExternalReferenceLink }

function TdxSpreadSheet3DExternalReferenceLink.ToString: string;
var
  AExternalLink: TdxSpreadSheetExternalLink;
begin
  AExternalLink := TdxSpreadSheetExternalLink(Data);
  if AExternalLink = nil then
    Result := ''
  else
    if TdxSpreadSheetInvalidObject.IsInvalid(AExternalLink) then
      Result := serRefError
    else
      if Owner.FormatSettings.ExpandExternalLinks then
      begin
        if IsFileProtocol(AExternalLink.ActualTarget) then
          Result := AExternalLink.ActualTarget
        else
          Result := dxReferenceLeftParenthesis + AExternalLink.ActualTarget + dxReferenceRightParenthesis
      end
      else
        Result := dxReferenceLeftParenthesis + IntToStr(AExternalLink.Index + 1) + dxReferenceRightParenthesis;

  if Length(Name) > 0 then
  begin
    if (AExternalLink <> nil) and not TdxSpreadSheetInvalidObject.IsInvalid(AExternalLink) then
      Result := Result + Name + dxRefSeparator
    else
      Result := Name
  end;
end;

function TdxSpreadSheet3DExternalReferenceLink.IsFileProtocol(const ATarget: string): Boolean;
begin
  Result := (Length(ATarget) > 5) and (dxSpreadSheetLowerCase(Copy(ATarget, 1, 5)) = 'file:');
end;

{ TdxSpreadSheetDefinedNameToken }

constructor TdxSpreadSheetDefinedNameToken.Create(const AValue: string;
  ADefinedNames: TdxSpreadSheetDefinedNames = nil; ALink: TdxSpreadSheet3DReferenceLink = nil);
begin
  inherited Create(AValue);
  FDefinedNames := ADefinedNames;
  SetLink(FLink, ALink);
end;

destructor TdxSpreadSheetDefinedNameToken.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TdxSpreadSheetDefinedNameToken.EnumReferences(
  AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False);
var
  ALink: TNameAccess;
begin
  if AProcessDefinedNames then
  begin
    ALink := TNameAccess(DefinedName);
    if (ALink <> nil) and (ALink.Formula <> nil) then
      ALink.Formula.EnumReferences(AProc, True);
  end;
end;

function TdxSpreadSheetDefinedNameToken.ExtractColumn(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    Result := TTokenAccess(TResultAccess(AResult).FirstItem).ExtractColumn(AIndex, AErrorCode);
  finally
    AResult.Free;
  end;
end;

function TdxSpreadSheetDefinedNameToken.ExtractRow(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    Result := TTokenAccess(TResultAccess(AResult).FirstItem).ExtractRow(AIndex, AErrorCode);
  finally
    AResult.Free;
  end;
end;

procedure TdxSpreadSheetDefinedNameToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    AErrorCode := AResult.ErrorCode;
    if AResult.Validate then
      AValue := AResult.Value;
  finally
    AResult.Free;
  end;
end;

function TdxSpreadSheetDefinedNameToken.UpdateValue(ADefinedName: TdxSpreadSheetDefinedName; const ANewName: string): Boolean;
begin
  Result := DefinedName = ADefinedName;
  if Result then
    Value := ANewName;
end;

procedure TdxSpreadSheetDefinedNameToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
var
  ALink: TNameAccess;
begin
  ALink := TNameAccess(DefinedName);
  if ALink <> nil then
  begin
    if (ALink.Formula <> nil) and (ALink.Formula.ErrorCode = ecNone) then
      AResult.AddResultValue(ALink.Formula.ResultValue)
    else
      AResult.SetError(ecName);
  end
  else
    AResult.SetError(ecName);
end;

procedure TdxSpreadSheetDefinedNameToken.CalculateDimension(
  var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    Calculate(AResult);
    if AResult.Validate then
      TTokenAccess(TResultAccess(AResult).Items[0]).CalculateDimension(ADimension, AErrorCode)
    else
      AErrorCode := AResult.ErrorCode;
  finally
    AResult.Free;
  end;
end;

procedure TdxSpreadSheetDefinedNameToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  if Link <> nil then
    AttachString(AAsText, Link.ToString + Value)
  else
    AttachString(AAsText, Value);
end;

function TdxSpreadSheetDefinedNameToken.GetDefinedName: TdxSpreadSheetDefinedName;
begin
  if DefinedNames = nil then
    Exit(nil);
  if Link <> nil then
  begin
    if Link.Data <> nil then
      Result := DefinedNames.GetItemByName(Value, Link.Data as TdxSpreadSheetCustomView)
    else
      Result := nil;
  end
  else
  begin
    Result := DefinedNames.GetItemByName(Value, Owner.View as TdxSpreadSheetCustomView);
    if Result = nil then
      Result := DefinedNames.GetItemByName(Value, nil);
  end;
end;

{ TdxSpreadSheetFormulaParser }

constructor TdxSpreadSheetFormulaParser.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create;
  FSpreadSheet := ASpreadSheet;
  FormatSettings.Assign(ASpreadSheet.FormulaController.FormatSettings);
end;

function TdxSpreadSheetFormulaParser.ParseFormula(const AFormulaText: string; ACell: TdxSpreadSheetCell): Boolean;
var
  AFormula: TdxSpreadSheetFormula;
begin
  AFormula := TdxSpreadSheetFormula.Create(ACell);
  try
    Result := ParseFormula(AFormulaText, AFormula);
    if Result then
      ACell.AsFormula := AFormula
    else
      FreeAndNil(AFormula);
  except
    SetErrorIndex(Length(AFormula.SourceText));
    FreeAndNil(AFormula);
    Result := False;
  end;
end;

function TdxSpreadSheetFormulaParser.ParseFormula(const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean;
begin
  Result := inherited ParseFormula(AFormulaText, AFormula);
end;

function TdxSpreadSheetFormulaParser.IsDefinedName(var APosition: Integer; ALength: Integer; out AName: TdxSpreadSheetFormulaToken): Boolean;
var
  ACandidate: string;
  ACandidatePos: Integer;
  ALink: TdxSpreadSheet3DReferenceCustomLink;
  ASavedPos: Integer;
begin
  ASavedPos := APosition;
  if not CheckViewName(APosition, APosition + ALength, ALink) then
    ALink := nil;

  ACandidatePos := APosition;
  ACandidate := GetStringPart(APosition, ALength - (APosition - ASavedPos));
  if (ACandidate <> '') and (APosition <= ASavedPos + ALength - 1) and CheckText(APosition, dxAreaSeparator) then
  begin
    Inc(APosition);
    ACandidate := ACandidate + dxAreaSeparator + GetStringPart(APosition, ALength - (APosition - ASavedPos));
  end;

  if ALink <> nil then
    Result := DefinedNames.Contains(ACandidate, TdxSpreadSheetTableView(ALink.Data))
  else
    Result := DefinedNames.Contains(ACandidate, Sheet) or DefinedNames.Contains(ACandidate, nil);

  if Result then
  begin
    AName := TdxSpreadSheetDefinedNameToken.Create(
      Copy(FFormulaSourceText, ACandidatePos, Length(ACandidate)),
      DefinedNames, ALink as TdxSpreadSheet3DReferenceLink)
  end
  else
  begin
    APosition := ASavedPos;
    FreeAndNil(ALink);
  end;
end;

function TdxSpreadSheetFormulaParser.GetStringPart(var APosition: Integer; ALength: Integer): string;
var
  ALen: Integer;
begin
  ALen := 0;
  while (ALen < ALength) and not IsBreakChar(APosition + ALen) do
    Inc(ALen);
  Result := GetSubString(APosition, ALen);
  Inc(APosition, ALen);
end;

function TdxSpreadSheetFormulaParser.GetSubString(const APosition, ALength: Integer): string;
begin
  SetLength(Result, ALength);
  if ALength > 0 then
    Move(FFormulaText[APosition], Result[1], ALength * SizeOf(Char));
end;

function TdxSpreadSheetFormulaParser.GetViewByName(APosition, ALength: Integer; out AView: TObject): Boolean;
var
  ASheetIndex: Integer;
begin
  for ASheetIndex := 0 to SpreadSheet.SheetCount - 1 do
  begin
    if dxSpreadSheetCompareText(SpreadSheet.Sheets[ASheetIndex].Caption, @FFormulaText[APosition], ALength) = 0 then
    begin
      AView := SpreadSheet.Sheets[ASheetIndex];
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxSpreadSheetFormulaParser.CheckExternalLink(var APosition: Integer;
  AFinishPos: Integer; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
var
  AIndex1: Integer;
  ALinkID: Integer;
begin
  Result := False;
  AIndex1 := APosition;
  if CheckText(APosition, dxReferenceLeftParenthesis) then
  begin
    Inc(APosition);
    if GetIntValue(APosition, AFinishPos, ALinkID) and CheckText(APosition, dxReferenceRightParenthesis) then
    begin
      FreeAndNil(ALink);
      ALink := TdxSpreadSheet3DExternalReferenceLink.Create(SpreadSheet.ExternalLinks[ALinkID - 1]);
      Inc(APosition);
      Result := True;
    end
    else
      APosition := AIndex1;
  end;
end;

function TdxSpreadSheetFormulaParser.CheckReferenceToView(
  var APosition: Integer; AFinishPos: Integer; var ARow, AColumn: Integer;
  var AbsRow, AbsColumn: Boolean; var ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
var
  ASheet: TdxSpreadSheetCustomView;
begin
  Result := inherited CheckReferenceToView(APosition, AFinishPos, ARow, AColumn, AbsRow, AbsColumn, ALink);
  if Result and (ALink = nil) and (Formula <> nil) and not TFormulaAccess(Formula).IsLinkedToCell then
  begin
    ASheet := Sheet;
    if ASheet = nil then
      ASheet := SpreadSheet.ActiveSheetAsTable;
    ALink := TdxSpreadSheet3DReferenceLink.Create(ASheet);
  end;
end;

function TdxSpreadSheetFormulaParser.GetDefinedNames: TdxSpreadSheetDefinedNames;
begin
  Result := SpreadSheet.DefinedNames;
end;

function TdxSpreadSheetFormulaParser.GetSheet: TdxSpreadSheetTableView;
begin
  if Formula <> nil then
    Result := Formula.View as TdxSpreadSheetTableView
  else
    Result := nil;
end;

{ TdxSpreadSheetFormulaReferences }

function TdxSpreadSheetFormulaReferences.GetExpressionAsText(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaFormattedText.Create;
  if AExpression <> nil then
  begin
    AToken := TdxSpreadSheetFormulaToken.Create;
    try
      while AExpression <> nil do
      begin
        TTokenAccess(AExpression).ToString(AToken);
        AExpression := AExpression.Next;
      end;

      Result.Add(FormatSettings.Operations[opEQ]);
      while AToken.FirstChild <> nil do
        Result.Add(AToken.FirstChild.ExtractFromList as TdxSpreadSheetFormulaFormattedText);
    finally
      AToken.Free;
    end;
  end;
end;

{ TdxSpreadSheetFormulaReferencesParser }

function TdxSpreadSheetFormulaReferencesParser.CheckExtraChars: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetFormulaReferencesParser.IsText(
  var APosition: Integer; ALength: Integer; out AToken: TdxSpreadSheetFormulaToken): Boolean;
var
  AIndex: Integer;
begin
  if IsDelimiter(FFormulaText[APosition]) then
  begin
    AToken := TdxSpreadSheetFormulaTextValueToken.Create(FFormulaText[APosition]);
    Inc(APosition);
    Exit(True);
  end;

  AIndex := APosition;
  while (ALength > 0) and not IsDelimiter(FFormulaText[AIndex]) do
  begin
    Dec(ALength);
    Inc(AIndex);
  end;
  AToken := TdxSpreadSheetFormulaTextValueToken.Create(Copy(FFormulaText, APosition, AIndex - APosition));
  APosition := AIndex;
  Result := True;
end;

function TdxSpreadSheetFormulaReferencesParser.PrepareTokensFromString(var AStartPos, AFinishPos: Integer): TdxSpreadSheetFormulaToken;
var
  APrevToken: TdxSpreadSheetFormulaToken;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  APrevToken := nil;

  while AStartPos <= AFinishPos do
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

  if Formula.ErrorIndex <> 0 then
    TdxSpreadSheetFormulaToken.DestroyTokens(Result);
end;

procedure TdxSpreadSheetFormulaReferencesParser.RegisterTokenControllers;
begin
  AddTokenController(IsString);
  AddTokenController(IsArray);
  AddTokenController(IsError);
  AddTokenController(IsNumber);
  AddTokenController(IsDefinedName);
  AddTokenController(IsReference);
  AddTokenController(IsText);
end;

procedure TdxSpreadSheetFormulaReferencesParser.SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode);
begin
  // do nothing
end;

function TdxSpreadSheetFormulaReferencesParser.CleanLineBreaks(const S: string): string;
begin
  Result := S;
end;

function TdxSpreadSheetFormulaReferencesParser.CleanSpaces(const S: string): string;
begin
  Result := S;
end;

function TdxSpreadSheetFormulaReferencesParser.IsDelimiter(const C: Char): Boolean;
const
  Delimiters = #0#9#10#11#13#32'.,<>=!*^&%#@?:;"()[]{}+|-/\'#$201C#$201D;
begin
  Result := Pos(C, Delimiters) > 0;
end;

{ TdxSpreadSheetFormulaReferencesHelper }

class function TdxSpreadSheetFormulaReferencesHelper.GetReferences(ACell: TdxSpreadSheetCell;
  const AText: string; AReferences: TdxRectList; out AFormattedText: TdxSpreadSheetFormulaFormattedText): Boolean;
var
  AFormula: TdxSpreadSheetFormula;
  AParser: TdxSpreadSheetFormulaReferencesParser;
begin
  AReferences.Clear;
  AFormula := TdxSpreadSheetFormulaReferences.Create(ACell);
  try
    AParser := TdxSpreadSheetFormulaReferencesParser.Create(ACell.SpreadSheet);
    try
      Result := AParser.ParseFormula(AText, AFormula);
      if Result then
      begin
        AFormula.EnumReferences(
          procedure (const AArea: TRect; ASheet: TObject)
          begin
            if (ASheet = nil) or (ASheet = AFormula.View) then
              AReferences.Add(AArea)
            else
              AReferences.Add(cxInvalidRect);
          end);
        AFormattedText := TFormulaAccess(AFormula).GetExpressionAsText(AFormula.Tokens);
      end;
    finally
      AParser.Free;
    end;
  finally
    AFormula.Free;
  end;
end;

end.
