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

unit dxSpreadSheetFormatODSFormulas;

{$I cxVer.Inc}

interface

uses
  Windows, Classes, dxCore, dxSpreadSheetTypes, dxSpreadSheetFormulas, dxSpreadSheetCore, dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens;

type

  { TdxSpreadSheetODSFormula }

  TdxSpreadSheetODSFormula = class
  public
    class function Convert(const ASpreadSheet: TdxCustomSpreadSheet; const S: string): string;
  end;

  { TdxSpreadSheetODSFormulaHelper }

  TdxSpreadSheetODSFormulaHelper = class(TdxSpreadSheetFormula)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); reintroduce;
  end;

  { TdxSpreadSheetODSFormulaParser }

  TdxSpreadSheetODSFormulaParser = class(TdxSpreadSheetFormulaParser)
  strict private
    function ParseReference(var APosition: Integer; out AColumnIndex, ARowIndex: Integer; out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;
  protected
    function CheckError: Boolean; override;
    function CheckTag(const AFormulaText, ATag: string): Boolean;
    function IsReference(var APosition: Integer; ALength: Integer; out AReference: TdxSpreadSheetFormulaToken): Boolean;
    procedure SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode = ecNone); override;
    //
    procedure RegisterTokenControllers; override;
  public const
    Tag = 'of:=';
    Tag2 = 'oooc:=';
  public
    function ExtractParams(const AFunctionParams: string): TStringList;
    function ParseFormula(const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean; override;
  end;

implementation

uses
  SysUtils, dxSpreadSheetStrs, dxSpreadSheetUtils, dxSpreadSheetFormatUtils, dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreStrs;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);

{ TdxSpreadSheetODSFormula }

class function TdxSpreadSheetODSFormula.Convert(const ASpreadSheet: TdxCustomSpreadSheet; const S: string): string;
var
  AFormula: TdxSpreadSheetFormula;
  AParser: TdxSpreadSheetODSFormulaParser;
begin
  AParser := TdxSpreadSheetODSFormulaParser.Create(ASpreadSheet);
  try
    AFormula := TdxSpreadSheetODSFormulaHelper.Create(ASpreadSheet);
    try
      if AParser.ParseFormula(S, AFormula) then
        Result := AFormula.AsText
      else
        Result := '';
    finally
      AFormula.Free;
    end;
  finally
    AParser.Free;
  end;
end;

{ TdxSpreadSheetODSFormulaHelper }

constructor TdxSpreadSheetODSFormulaHelper.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create(nil);
  FSpreadSheet := ASpreadSheet;
end;

function TdxSpreadSheetODSFormulaHelper.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := TdxCustomSpreadSheetAccess(FSpreadSheet).FormulaController;
end;

{ TdxSpreadSheetODSFormulaParser }

function TdxSpreadSheetODSFormulaParser.ExtractParams(const AFunctionParams: string): TStringList;
var
  AFinishPos: Integer;
  ALength: Integer;
  AStartPos: Integer;
begin
  Result := TStringList.Create;

  FFormulaText := AFunctionParams;
  AStartPos := 0;
  AFinishPos := Length(AFunctionParams) - 1;
  repeat
    ALength := GetSubExpressionLength(',', AStartPos, AFinishPos - AStartPos + 1);
    if ALength = 0 then
      ALength := AFinishPos - AStartPos + 1;
    if ALength > 0 then
    begin
      Result.Add(Copy(AFunctionParams, AStartPos + 1, ALength));
      Inc(AStartPos, ALength + 1);
    end;
  until ALength <= 0;
end;

function TdxSpreadSheetODSFormulaParser.ParseFormula(
  const AFormulaText: string; AFormula: TdxSpreadSheetCustomFormula): Boolean;
var
  ATag: string;
begin
  if CheckTag(AFormulaText, Tag) then
    ATag := Tag
  else if CheckTag(AFormulaText, Tag2)  then
    ATag := Tag2
  else
    Exit(False);

  Result := inherited ParseFormula(Copy(AFormulaText, Length(ATag), MaxInt), AFormula);
end;

function TdxSpreadSheetODSFormulaParser.CheckError: Boolean;
begin
  Result := (Formula = nil) or (Formula.ErrorIndex = 0);
end;

function TdxSpreadSheetODSFormulaParser.CheckTag(const AFormulaText, ATag: string): Boolean;
begin
  Result := (Length(AFormulaText) > Length(ATag)) and CompareMem(@AFormulaText[1], @ATag[1], Length(ATag) * SizeOf(Char));
end;

function TdxSpreadSheetODSFormulaParser.IsReference(
  var APosition: Integer; ALength: Integer; out AReference: TdxSpreadSheetFormulaToken): Boolean;
var
  AColumn1Index, AColumn2Index: Integer;
  ALink1, ALink2: TdxSpreadSheet3DReferenceCustomLink;
  ARow1Index, ARow2Index: Integer;
  ASavedPosition: Integer;
begin
  Result := False;
  if CheckText(APosition, '[') then
  begin
    ASavedPosition := APosition;
    try
      ARow2Index := -1;
      AColumn2Index := -1;

      Inc(APosition);
      Result := ParseReference(APosition, AColumn1Index, ARow1Index, ALink1);
      if not Result then
      begin
        SetErrorIndex(APosition);
        Exit;
      end;

      if CheckText(APosition, ':') then
      begin
        Inc(APosition);
        ParseReference(APosition, AColumn2Index, ARow2Index, ALink2);
      end
      else
        ALink2 := nil;

      AReference := MakeReference(ALink1, ALink2, ARow1Index, AColumn1Index,
        ARow2Index, AColumn2Index, True, True, True, True, (ARow2Index >= 0) and (AColumn2Index >= 0));
      if CheckText(APosition, ']') then
        Inc(APosition);
    finally
      if not Result then
        APosition := ASavedPosition;
    end;
  end;
end;

procedure TdxSpreadSheetODSFormulaParser.SetErrorIndex(AErrorIndex: Integer; const ACode: TdxSpreadSheetFormulaErrorCode = ecNone);
begin
  if Formula <> nil then
    inherited SetErrorIndex(AErrorIndex, ACode);
end;

procedure TdxSpreadSheetODSFormulaParser.RegisterTokenControllers;
begin
  inherited RegisterTokenControllers;
  AddTokenController(IsReference);
end;

function TdxSpreadSheetODSFormulaParser.ParseReference(var APosition: Integer;
  out AColumnIndex, ARowIndex: Integer; out ALink: TdxSpreadSheet3DReferenceCustomLink): Boolean;

  procedure GetToken(out S, L: Integer);
  begin
    if IsStringMark(APosition, True) then
    begin
      Inc(APosition);
      S := APosition;
      while not IsStringMark(APosition, True) do
        Inc(APosition);
      L := APosition - S;
      Inc(APosition);
    end
    else
    begin
      S := APosition;
      while (APosition <= Length(FFormulaText)) and not CharInSet(FFormulaText[APosition], ['.', '#', ']', '$', ':']) do
        Inc(APosition);
      L := APosition - S;
    end;
  end;

var
  ASheet: TObject;
  L, S: Integer;
begin
  ARowIndex := -1;
  AColumnIndex := -1;

  GetToken(S, L);
  if CheckText(APosition, '#') then
  begin
    ALink := TdxSpreadSheet3DExternalReferenceLink.Create(SpreadSheet.ExternalLinks.Add(Copy(FFormulaSourceText, S, L)));
    Inc(APosition);
    if not CheckText(APosition, '$') then
    begin
      SetErrorIndex(APosition, ecName);
      FreeAndNil(ALink);
      Exit(False);
    end;
    Inc(APosition);
    GetToken(S, L);
    TdxSpreadSheet3DExternalReferenceLink(ALink).Name := Copy(FFormulaSourceText, S, L);
  end
  else
  begin
    if CheckText(APosition, '$') then
    begin
      Inc(APosition);
      GetToken(S, L);
    end;
    if L > 0 then
    begin
      if GetViewByName(S, L, ASheet) then
        ALink := TdxSpreadSheet3DReferenceLink.Create(ASheet)
      else
        ALink := TdxSpreadSheet3DReferenceLink.Create(TdxSpreadSheetInvalidObject.Instance)
    end
    else
      ALink := nil;
  end;

  if not CheckText(APosition, '.') then
  begin
    SetErrorIndex(APosition, ecName);
    FreeAndNil(ALink);
    Exit(False);
  end;

  Inc(APosition);
  if CheckText(APosition, '$') then
    Inc(APosition);
  GetToken(S, L);
  if CheckText(APosition, '$') then
  begin
    AColumnIndex := TdxSpreadSheetColumnHelper.IndexByName(Copy(FFormulaSourceText, S, L));
    Inc(APosition);
    GetToken(S, L);
    ARowIndex := StrToIntDef(Copy(FFormulaSourceText, S, L), 0) - 1;
  end
  else
    dxStringToReference(Copy(FFormulaSourceText, S, L), AColumnIndex, ARowIndex);

  Result := True;
end;

end.
