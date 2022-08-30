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

unit dxRichEdit.Import.Html.CSSParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Utils.DXUnit;

type

  { TdxLengthValueParser }

  TdxLengthValueParser = record
  public
    type
      TUnit = (
        None,
        EM,
        EX,
        PT,
        PC,
        PX,
        &IN,
        CM,
        MM);
  strict private
    FPointsValue: Single;
    FIsRelativeUnit: Boolean;
    FValueInfo: TdxValueInfo;
    FRelativeUnit: string;
    FLengthValue: string;
    FDpi: Single;
    FSuccess: Boolean;
    function GetValue: Single;
    function GetUnit: string;
    function GetIsDigit: Boolean;

    procedure ParseLengthValue;
    function Parse: Boolean;
    function GetLengthValueInPoints(const AValueInfo: TdxValueInfo): Single; overload;
    function GetLengthValueInPoints(const AValueInfo: TdxValueInfo; const V: TdxUnitConversionParameters): Single; overload;
  public
    constructor Create(const ALengthValue: string; ADpi: Single); overload;
    constructor Create(const ALengthValue: string); overload;
    class function GetUnitType(const AValue: string): TUnit; static;

    property PointsValue: Single read FPointsValue;
    property Value: Single read GetValue;
    property &Unit: string read GetUnit;
    property RelativeUnit: string read FRelativeUnit;
    property IsRelativeUnit: Boolean read FIsRelativeUnit;
    property IsDigit: Boolean read GetIsDigit;
    property Success: Boolean read FSuccess;
  end;

  { TdxCssParser }

  TdxCssParser = class(TdxCustomCssParser)
  strict private
    class var
      FAcssKeywordTable: TdxCssKeywordTranslatorTable;
      FHtmlColors: TdxStringColorDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class function CreateCssKeywordTable: TdxCssKeywordTranslatorTable; static;
    class function CreateHtmlColors: TdxStringColorDictionary; static;
    class function SetLineIndentType(ACssProperties: TdxCssProperties; AIndent: Single): Single; static;
    class procedure CssTextTransformKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssUnicodeBidiKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssVerticalAlignKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssVisibilityKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssVisitedKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssWhiteSpaceKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssWordSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
  protected
    class procedure CssBackgroundKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBackgroundAttachmentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBackgroundColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBackgroundImageKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBackgroundPositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBackgroundRepeatKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBeforeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderKeywordCore(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>;
      ABorder: TdxHtmlBorderProperty); static;
    class procedure CssBorderBottomColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderBottomStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderBottomWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderCollapseKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderLeftColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderLeftStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderLeftWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderRightColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderRightStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderRightWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderTopColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderTopStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBorderTopWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssCaptionSideKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssClearKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssClipKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function GetColor(const AValue: string): TdxAlphaColor; static;
    class procedure CssContentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssCounterIncrementKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssCounterResetKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssCursorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssDirectionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssDisplayKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssEmptyCellsKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssFirstChildKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssFloatKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssFocusKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssFontKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function TrySetFontProperty(ACssProperties: TdxCssProperties; AProperty: TdxFontProperties;
      const AValue: string): Boolean; static;
    class function ShouldSplitFontSizeLineHeight(const AFontSizeProperties: string): Boolean; static;
    class function GetPropertiesList(const AProperty: string): TArray<string>; static;
    class procedure CssFontFamilyKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetFontFamily(ACssProperties: TdxCssProperties; const AValue: string): Boolean; static;
    class procedure CssFontSizeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetFontSize(ACssProperties: TdxCssProperties; const AValue: string): Boolean; static;
    class procedure CssFontStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetFontStyle(ACssProperties: TdxCssProperties; const AValue: string): Boolean; static;
    class procedure CssFontVariantKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetFontVariant(ACssProperties: TdxCssProperties; const AValue: string): Boolean; static;
    class procedure CssFontWeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetFontWeight(ACssProperties: TdxCssProperties; const AValue: string): Boolean; static;
    class procedure CssHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssHoverKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssLetterSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssLineHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssLinkKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssListStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssListStyleImageKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssListStylePositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssListStyleTypeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMarginKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMarginKeywordCore(ACssProperties: TdxCssProperties; const ABottom: string; const ATop: string;
      const ALeft: string; const ARight: string); static;
    class function CreateStringList(const AItem: string): TArray<string>; static;
    class procedure CssMarginTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMarginBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMarginLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMarginRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMaxHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMaxWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMinHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMinWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssBeforeAutoSpacing(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssAfterAutoSpacing(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssMsoPagination(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOpacityKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOutlineKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOutlineColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOutlineStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOutlineWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssOverflowKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPaddingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPaddingKeywordCore(ACssProperties: TdxCssProperties; const ABottom: string; const ATop: string;
      const ALeft: string; const ARight: string); static;
    class procedure CssPaddingBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPaddingLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPaddingRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPaddingTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPageBreakAfterKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPageBreakBeforeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPageBreakInsideKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssPositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssQuotesKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssTableLayoutKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssTextAlignKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssTextDecorationKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class procedure CssTextIndentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>); static;
    class function SetMarginProperties(const APropertiesValue: TArray<string>; var AMarginValue: Single;
      AUnitConverter: TdxDocumentModelUnitConverter): string; static;

    function GetCssKeywordTable: TdxCssKeywordTranslatorTable; override;
  end;

implementation

uses
  Math, TypInfo, StrUtils,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Platform.Win.FontCache,
  dxStringHelper,
  dxRichEdit.Utils.Types,
  dxMeasurementUnits,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Import.Html,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.TableTags;

{ TdxLengthValueParser }

constructor TdxLengthValueParser.Create(const ALengthValue: string);
begin
  Create(ALengthValue, TdxCustomDocumentModel.Dpi);
end;

constructor TdxLengthValueParser.Create(const ALengthValue: string; ADpi: Single);
begin
  FIsRelativeUnit := False;
  FPointsValue := 0.0;
  FDpi := ADpi;
  FRelativeUnit := '';
  FLengthValue := ALengthValue;
  FValueInfo := TdxValueInfo.Empty;
  ParseLengthValue;
end;

function TdxLengthValueParser.GetValue: Single;
begin
  Result := FValueInfo.Value;
end;

function TdxLengthValueParser.GetUnit: string;
begin
  Result := FValueInfo.&Unit;
end;

function TdxLengthValueParser.GetIsDigit: Boolean;
begin
  Result := FValueInfo.IsValidNumber;
end;

procedure TdxLengthValueParser.ParseLengthValue;
begin
  FSuccess := Parse;
end;

class function TdxLengthValueParser.GetUnitType(const AValue: string): TUnit;
var
  AUnit: string;
  I: Integer;
begin
  AUnit := UpperCase(Trim(AValue));
  I := GetEnumValue(TypeInfo(TdxLengthValueParser.TUnit), AUnit);
  if I < 0 then
    Result := TdxLengthValueParser.TUnit.None
  else
    Result := TdxLengthValueParser.TUnit(I);
end;

function TdxLengthValueParser.Parse: Boolean;
begin
  if FLengthValue = '' then
    Exit(False);

  FValueInfo := TdxStringValueParser.TryParse(FLengthValue);
  if FValueInfo = TdxValueInfo.Empty then
    Exit(False);

  if IsDigit then
  begin
    FPointsValue := GetLengthValueInPoints(FValueInfo);
    if (FRelativeUnit = 'em') or (FRelativeUnit = 'rem') or (FRelativeUnit = 'ex') or (FRelativeUnit = '%') then
      FIsRelativeUnit := True;
  end;
  Result := True;
end;

function TdxLengthValueParser.GetLengthValueInPoints(const AValueInfo: TdxValueInfo): Single;
begin
  Result := GetLengthValueInPoints(AValueInfo, TdxUnitConversionParameters.Empty);
end;

function TdxLengthValueParser.GetLengthValueInPoints(const AValueInfo: TdxValueInfo;
  const V: TdxUnitConversionParameters): Single;
var
  AUnit: string;
begin
  Result := AValueInfo.Value;
  AUnit := UpperCase(AValueInfo.&Unit);
  if TdxStringHelper.StartsWithChar(AUnit, '%', False) then
  begin
    FRelativeUnit := '%';
    Exit;
  end;
  if Length(AUnit) >= 2 then
  begin
    if TdxStringHelper.StartsWith(AUnit, 'REM') then
    begin
      FRelativeUnit := 'rem';
      Result := Result * 100;
    end
    else
      case GetUnitType(TdxStringHelper.Substring(AUnit, 0, 2)) of
        TUnit.EM:
          begin
            FRelativeUnit := 'em';
            Result := Result * 100;
          end;
        TUnit.EX:
          begin
            FRelativeUnit := 'ex';
            Result := Result * 100 / 2;
          end;
        TUnit.PC:
          Result := Result * 12;
        TUnit.PX:
          Result := PixelsToPointsF(Result, FDpi);
        TUnit.&IN:
          Result := Result * 72;
        TUnit.CM:
          Result := MillimetersToPointsF(Result * 10);
        TUnit.MM:
          Result := MillimetersToPointsF(Result);
      end;
  end;
end;

{ TdxCssParser }

class constructor TdxCssParser.Initialize;
begin
  FAcssKeywordTable := CreateCssKeywordTable;
  FHtmlColors := CreateHtmlColors;
end;

class destructor TdxCssParser.Finalize;
begin
  FAcssKeywordTable.Free;
  FHtmlColors.Free;
end;

class function TdxCssParser.CreateCssKeywordTable: TdxCssKeywordTranslatorTable;
begin
  Result := TdxCssKeywordTranslatorTable.Create;
  Result.Add(ConvertKeyToUpper('font-size'), CssFontSizeKeyword);
  Result.Add(ConvertKeyToUpper('background'), CssBackgroundKeyword);
  Result.Add(ConvertKeyToUpper('background-attachment'), CssBackgroundAttachmentKeyword);
  Result.Add(ConvertKeyToUpper('background-color'), CssBackgroundColorKeyword);
  Result.Add(ConvertKeyToUpper('background-image'), CssBackgroundImageKeyword);
  Result.Add(ConvertKeyToUpper('background-position'), CssBackgroundPositionKeyword);
  Result.Add(ConvertKeyToUpper('background-repeat'), CssBackgroundRepeatKeyword);
  Result.Add(ConvertKeyToUpper('before'), CssBeforeKeyword);
  Result.Add(ConvertKeyToUpper('border'), CssBorderKeyword);
  Result.Add(ConvertKeyToUpper('border-bottom'), CssBorderBottomKeyword);
  Result.Add(ConvertKeyToUpper('border-bottom-color'), CssBorderBottomColorKeyword);
  Result.Add(ConvertKeyToUpper('border-bottom-style'), CssBorderBottomStyleKeyword);
  Result.Add(ConvertKeyToUpper('border-bottom-width'), CssBorderBottomWidthKeyword);
  Result.Add(ConvertKeyToUpper('border-collapse'), CssBorderCollapseKeyword);
  Result.Add(ConvertKeyToUpper('border-color'), CssBorderColorKeyword);
  Result.Add(ConvertKeyToUpper('border-left'), CssBorderLeftKeyword);
  Result.Add(ConvertKeyToUpper('border-left-color'), CssBorderLeftColorKeyword);
  Result.Add(ConvertKeyToUpper('border-left-style'), CssBorderLeftStyleKeyword);
  Result.Add(ConvertKeyToUpper('border-left-width'), CssBorderLeftWidthKeyword);
  Result.Add(ConvertKeyToUpper('border-right'), CssBorderRightKeyword);
  Result.Add(ConvertKeyToUpper('border-right-color'), CssBorderRightColorKeyword);
  Result.Add(ConvertKeyToUpper('border-right-style'), CssBorderRightStyleKeyword);
  Result.Add(ConvertKeyToUpper('border-right-width'), CssBorderRightWidthKeyword);
  Result.Add(ConvertKeyToUpper('border-spacing'), CssBorderSpacingKeyword);
  Result.Add(ConvertKeyToUpper('border-style'), CssBorderStyleKeyword);
  Result.Add(ConvertKeyToUpper('border-top'), CssBorderTopKeyword);
  Result.Add(ConvertKeyToUpper('border-top-color'), CssBorderTopColorKeyword);
  Result.Add(ConvertKeyToUpper('border-top-style'), CssBorderTopStyleKeyword);
  Result.Add(ConvertKeyToUpper('border-top-width'), CssBorderTopWidthKeyword);
  Result.Add(ConvertKeyToUpper('border-width'), CssBorderWidthKeyword);
  Result.Add(ConvertKeyToUpper('bottom'), CssBottomKeyword);
  Result.Add(ConvertKeyToUpper('caption-side'), CssCaptionSideKeyword);
  Result.Add(ConvertKeyToUpper('clear'), CssClearKeyword);
  Result.Add(ConvertKeyToUpper('clip'), CssClipKeyword);
  Result.Add(ConvertKeyToUpper('color'), CssColorKeyword);
  Result.Add(ConvertKeyToUpper('content'), CssContentKeyword);
  Result.Add(ConvertKeyToUpper('counter-increment'), CssCounterIncrementKeyword);
  Result.Add(ConvertKeyToUpper('counter-reset'), CssCounterResetKeyword);
  Result.Add(ConvertKeyToUpper('cursor'), CssCursorKeyword);
  Result.Add(ConvertKeyToUpper('direction'), CssDirectionKeyword);
  Result.Add(ConvertKeyToUpper('display'), CssDisplayKeyword);
  Result.Add(ConvertKeyToUpper('empty-cells'), CssEmptyCellsKeyword);
  Result.Add(ConvertKeyToUpper('first-child'), CssFirstChildKeyword);
  Result.Add(ConvertKeyToUpper('float'), CssFloatKeyword);
  Result.Add(ConvertKeyToUpper('focus'), CssFocusKeyword);
  Result.Add(ConvertKeyToUpper('font'), CssFontKeyword);
  Result.Add(ConvertKeyToUpper('font-family'), CssFontFamilyKeyword);
  Result.Add(ConvertKeyToUpper('font-style'), CssFontStyleKeyword);
  Result.Add(ConvertKeyToUpper('font-variant'), CssFontVariantKeyword);
  Result.Add(ConvertKeyToUpper('font-weight'), CssFontWeightKeyword);
  Result.Add(ConvertKeyToUpper('height'), CssHeightKeyword);
  Result.Add(ConvertKeyToUpper('hover'), CssHoverKeyword);
  Result.Add(ConvertKeyToUpper('left'), CssLeftKeyword);
  Result.Add(ConvertKeyToUpper('letter-spacing'), CssLetterSpacingKeyword);
  Result.Add(ConvertKeyToUpper('line-height'), CssLineHeightKeyword);
  Result.Add(ConvertKeyToUpper('link'), CssLinkKeyword);
  Result.Add(ConvertKeyToUpper('list-style'), CssListStyleKeyword);
  Result.Add(ConvertKeyToUpper('list-style-image'), CssListStyleImageKeyword);
  Result.Add(ConvertKeyToUpper('list-style-position'), CssListStylePositionKeyword);
  Result.Add(ConvertKeyToUpper('list-style-type'), CssListStyleTypeKeyword);
  Result.Add(ConvertKeyToUpper('margin'), CssMarginKeyword);
  Result.Add(ConvertKeyToUpper('margin-bottom'), CssMarginBottomKeyword);
  Result.Add(ConvertKeyToUpper('margin-left'), CssMarginLeftKeyword);
  Result.Add(ConvertKeyToUpper('margin-right'), CssMarginRightKeyword);
  Result.Add(ConvertKeyToUpper('margin-top'), CssMarginTopKeyword);
  Result.Add(ConvertKeyToUpper('max-height'), CssMaxHeightKeyword);
  Result.Add(ConvertKeyToUpper('max-width'), CssMaxWidthKeyword);
  Result.Add(ConvertKeyToUpper('min-height'), CssMinHeightKeyword);
  Result.Add(ConvertKeyToUpper('min-width'), CssMinWidthKeyword);
  Result.Add(ConvertKeyToUpper('mso-margin-top-alt'), CssBeforeAutoSpacing);
  Result.Add(ConvertKeyToUpper('mso-margin-bottom-alt'), CssAfterAutoSpacing);
  Result.Add(ConvertKeyToUpper('mso-pagination'), CssMsoPagination);
  Result.Add(ConvertKeyToUpper('opacity'), CssOpacityKeyword);
  Result.Add(ConvertKeyToUpper('outline'), CssOutlineKeyword);
  Result.Add(ConvertKeyToUpper('outline-color'), CssOutlineColorKeyword);
  Result.Add(ConvertKeyToUpper('outline-style'), CssOutlineStyleKeyword);
  Result.Add(ConvertKeyToUpper('outline-width'), CssOutlineWidthKeyword);
  Result.Add(ConvertKeyToUpper('overflow'), CssOverflowKeyword);
  Result.Add(ConvertKeyToUpper('padding'), CssPaddingKeyword);
  Result.Add(ConvertKeyToUpper('padding-bottom'), CssPaddingBottomKeyword);
  Result.Add(ConvertKeyToUpper('padding-left'), CssPaddingLeftKeyword);
  Result.Add(ConvertKeyToUpper('padding-right'), CssPaddingRightKeyword);
  Result.Add(ConvertKeyToUpper('padding-top'), CssPaddingTopKeyword);
  Result.Add(ConvertKeyToUpper('page-break-after'), CssPageBreakAfterKeyword);
  Result.Add(ConvertKeyToUpper('page-break-before'), CssPageBreakBeforeKeyword);
  Result.Add(ConvertKeyToUpper('page-break-inside'), CssPageBreakInsideKeyword);

  Result.Add(ConvertKeyToUpper('position'), CssPositionKeyword);
  Result.Add(ConvertKeyToUpper('quotes'), CssQuotesKeyword);
  Result.Add(ConvertKeyToUpper('right'), CssRightKeyword);
  Result.Add(ConvertKeyToUpper('table-layout'), CssTableLayoutKeyword);
  Result.Add(ConvertKeyToUpper('text-align'), CssTextAlignKeyword);
  Result.Add(ConvertKeyToUpper('text-decoration'), CssTextDecorationKeyword);
  Result.Add(ConvertKeyToUpper('text-indent'), CssTextIndentKeyword);
  Result.Add(ConvertKeyToUpper('text-transform'), CssTextTransformKeyword);
  Result.Add(ConvertKeyToUpper('top'), CssTopKeyword);
  Result.Add(ConvertKeyToUpper('unicode-bidi'), CssUnicodeBidiKeyword);
  Result.Add(ConvertKeyToUpper('vertical-align'), CssVerticalAlignKeyword);
  Result.Add(ConvertKeyToUpper('visibility'), CssVisibilityKeyword);
  Result.Add(ConvertKeyToUpper('visited'), CssVisitedKeyword);
  Result.Add(ConvertKeyToUpper('white-space'), CssWhiteSpaceKeyword);
  Result.Add(ConvertKeyToUpper('width'), CssWidthKeyword);
  Result.Add(ConvertKeyToUpper('word-spacing'), CssWordSpacingKeyword);
end;

class function TdxCssParser.CreateHtmlColors: TdxStringColorDictionary;
begin
  Result := TdxStringColorDictionary.Create(148, TdxIStringComparer.Ordinal);
  Result.Add('AliceBlue',            TdxAlphaColors.AliceBlue);
  Result.Add('AntiqueWhite',         TdxAlphaColors.AntiqueWhite);
  Result.Add('Aqua',                 TdxAlphaColors.Aqua);
  Result.Add('Aquamarine',           TdxAlphaColors.Aquamarine);
  Result.Add('Azure',                TdxAlphaColors.Azure);
  Result.Add('Beige',                TdxAlphaColors.Beige);
  Result.Add('Bisque',               TdxAlphaColors.Bisque);
  Result.Add('Black',                TdxAlphaColors.Black);
  Result.Add('BlanchedAlmond',       TdxAlphaColors.BlanchedAlmond);
  Result.Add('Blue',                 TdxAlphaColors.Blue);
  Result.Add('BlueViolet',           TdxAlphaColors.BlueViolet);
  Result.Add('Brown',                TdxAlphaColors.Brown);
  Result.Add('BurlyWood',            TdxAlphaColors.BurlyWood);
  Result.Add('CadetBlue',            TdxAlphaColors.CadetBlue);
  Result.Add('Chartreuse',           TdxAlphaColors.Chartreuse);
  Result.Add('Chocolate',            TdxAlphaColors.Chocolate);
  Result.Add('Coral',                TdxAlphaColors.Coral);
  Result.Add('CornflowerBlue',       TdxAlphaColors.CornflowerBlue);
  Result.Add('Cornsilk',             TdxAlphaColors.Cornsilk);
  Result.Add('Crimson',              TdxAlphaColors.Crimson);
  Result.Add('Cyan',                 TdxAlphaColors.Cyan);
  Result.Add('DarkBlue',             TdxAlphaColors.DarkBlue);
  Result.Add('DarkCyan',             TdxAlphaColors.DarkCyan);
  Result.Add('DarkGoldenRod',        TdxAlphaColors.DarkGoldenRod);
  Result.Add('DarkGray',             TdxAlphaColors.DarkGray);
  Result.Add('DarkGrey',             TdxAlphaColors.FromArgb(169, 169, 169));
  Result.Add('DarkGreen',            TdxAlphaColors.DarkGreen);
  Result.Add('DarkKhaki',            TdxAlphaColors.DarkKhaki);
  Result.Add('DarkMagenta',          TdxAlphaColors.DarkMagenta);
  Result.Add('DarkOliveGreen',       TdxAlphaColors.DarkOliveGreen);
  Result.Add('DarkOrange',           TdxAlphaColors.DarkOrange);
  Result.Add('DarkOrchid',           TdxAlphaColors.DarkOrchid);
  Result.Add('DarkRed',              TdxAlphaColors.DarkRed);
  Result.Add('DarkSalmon',           TdxAlphaColors.DarkSalmon);
  Result.Add('DarkSeaGreen',         TdxAlphaColors.DarkSeaGreen);
  Result.Add('DarkSlateBlue',        TdxAlphaColors.DarkSlateBlue);
  Result.Add('DarkSlateGray',        TdxAlphaColors.DarkSlateGray);
  Result.Add('DarkSlateGrey',        TdxAlphaColors.FromArgb(47, 79, 79));
  Result.Add('DarkTurquoise',        TdxAlphaColors.DarkTurquoise);
  Result.Add('DarkViolet',           TdxAlphaColors.DarkViolet);
  Result.Add('DeepPink',             TdxAlphaColors.DeepPink);
  Result.Add('DeepSkyBlue',          TdxAlphaColors.DeepSkyBlue);
  Result.Add('DimGray',              TdxAlphaColors.DimGray);
  Result.Add('DimGrey',              TdxAlphaColors.FromArgb(105, 105, 105));
  Result.Add('DodgerBlue',           TdxAlphaColors.DodgerBlue);
  Result.Add('Firebrick',            TdxAlphaColors.Firebrick);
  Result.Add('FloralWhite',          TdxAlphaColors.FloralWhite);
  Result.Add('ForestGreen',          TdxAlphaColors.ForestGreen);
  Result.Add('Fuchsia',              TdxAlphaColors.Fuchsia);
  Result.Add('Gainsboro',            TdxAlphaColors.Gainsboro);
  Result.Add('GhostWhite',           TdxAlphaColors.GhostWhite);
  Result.Add('Gold',                 TdxAlphaColors.Gold);
  Result.Add('GoldenRod',            TdxAlphaColors.GoldenRod);
  Result.Add('Gray',                 TdxAlphaColors.Gray);
  Result.Add('Grey',                 TdxAlphaColors.FromArgb(128, 128, 128));
  Result.Add('Green',                TdxAlphaColors.Green);
  Result.Add('GreenYellow',          TdxAlphaColors.GreenYellow);
  Result.Add('HoneyDew',             TdxAlphaColors.HoneyDew);
  Result.Add('HotPink',              TdxAlphaColors.HotPink);
  Result.Add('IndianRed',            TdxAlphaColors.IndianRed);
  Result.Add('Indigo',               TdxAlphaColors.Indigo);
  Result.Add('Ivory',                TdxAlphaColors.Ivory);
  Result.Add('Khaki',                TdxAlphaColors.Khaki);
  Result.Add('Lavender',             TdxAlphaColors.Lavender);
  Result.Add('LavenderBlush',        TdxAlphaColors.LavenderBlush);
  Result.Add('LawnGreen',            TdxAlphaColors.LawnGreen);
  Result.Add('LemonChiffon',         TdxAlphaColors.LemonChiffon);
  Result.Add('LightBlue',            TdxAlphaColors.LightBlue);
  Result.Add('LightCoral',           TdxAlphaColors.LightCoral);
  Result.Add('LightCyan',            TdxAlphaColors.LightCyan);
  Result.Add('LightGoldenRodYellow', TdxAlphaColors.LightGoldenRodYellow);
  Result.Add('LightGray',            TdxAlphaColors.LightGray);
  Result.Add('LightGrey',            TdxAlphaColors.FromArgb(211, 211, 211));
  Result.Add('LightGreen',           TdxAlphaColors.LightGreen);
  Result.Add('LightPink',            TdxAlphaColors.LightPink);
  Result.Add('LightSalmon',          TdxAlphaColors.LightSalmon);
  Result.Add('LightSeaGreen',        TdxAlphaColors.LightSeaGreen);
  Result.Add('LightSkyBlue',         TdxAlphaColors.LightSkyBlue);
  Result.Add('LightSlateGray',       TdxAlphaColors.LightSlateGray);
  Result.Add('LightSlateGrey',       TdxAlphaColors.FromArgb(119, 136, 153));
  Result.Add('LightSteelBlue',       TdxAlphaColors.LightSteelBlue);
  Result.Add('LightYellow',          TdxAlphaColors.LightYellow);
  Result.Add('Lime',                 TdxAlphaColors.Lime);
  Result.Add('LimeGreen',            TdxAlphaColors.LimeGreen);
  Result.Add('Linen',                TdxAlphaColors.Linen);
  Result.Add('Magenta',              TdxAlphaColors.Magenta);
  Result.Add('Maroon',               TdxAlphaColors.Maroon);
  Result.Add('MediumAquaMarine',     TdxAlphaColors.MediumAquaMarine);
  Result.Add('MediumBlue',           TdxAlphaColors.MediumBlue);
  Result.Add('MediumOrchid',         TdxAlphaColors.MediumOrchid);
  Result.Add('MediumPurple',         TdxAlphaColors.MediumPurple);
  Result.Add('MediumSeaGreen',       TdxAlphaColors.MediumSeaGreen);
  Result.Add('MediumSlateBlue',      TdxAlphaColors.MediumSlateBlue);
  Result.Add('MediumSpringGreen',    TdxAlphaColors.MediumSpringGreen);
  Result.Add('MediumTurquoise',      TdxAlphaColors.MediumTurquoise);
  Result.Add('MediumVioletRed',      TdxAlphaColors.MediumVioletRed);
  Result.Add('MidnightBlue',         TdxAlphaColors.MidnightBlue);
  Result.Add('MintCream',            TdxAlphaColors.MintCream);
  Result.Add('MistyRose',            TdxAlphaColors.MistyRose);
  Result.Add('Moccasin',             TdxAlphaColors.Moccasin);
  Result.Add('NavajoWhite',          TdxAlphaColors.NavajoWhite);
  Result.Add('Navy',                 TdxAlphaColors.Navy);
  Result.Add('OldLace',              TdxAlphaColors.OldLace);
  Result.Add('Olive',                TdxAlphaColors.Olive);
  Result.Add('OliveDrab',            TdxAlphaColors.OliveDrab);
  Result.Add('Orange',               TdxAlphaColors.Orange);
  Result.Add('OrangeRed',            TdxAlphaColors.OrangeRed);
  Result.Add('Orchid',               TdxAlphaColors.Orchid);
  Result.Add('PaleGoldenRod',        TdxAlphaColors.PaleGoldenRod);
  Result.Add('PaleGreen',            TdxAlphaColors.PaleGreen);
  Result.Add('PaleTurquoise',        TdxAlphaColors.PaleTurquoise);
  Result.Add('PaleVioletRed',        TdxAlphaColors.PaleVioletRed);
  Result.Add('PapayaWhip',           TdxAlphaColors.PapayaWhip);
  Result.Add('PeachPuff',            TdxAlphaColors.PeachPuff);
  Result.Add('Peru',                 TdxAlphaColors.Peru);
  Result.Add('Pink',                 TdxAlphaColors.Pink);
  Result.Add('Plum',                 TdxAlphaColors.Plum);
  Result.Add('PowderBlue',           TdxAlphaColors.PowderBlue);
  Result.Add('Purple',               TdxAlphaColors.Purple);
  Result.Add('RebeccaPurple',        TdxAlphaColors.FromArgb(102, 51, 153));
  Result.Add('Red',                  TdxAlphaColors.Red);
  Result.Add('RosyBrown',            TdxAlphaColors.RosyBrown);
  Result.Add('RoyalBlue',            TdxAlphaColors.RoyalBlue);
  Result.Add('SaddleBrown',          TdxAlphaColors.SaddleBrown);
  Result.Add('Salmon',               TdxAlphaColors.Salmon);
  Result.Add('SandyBrown',           TdxAlphaColors.SandyBrown);
  Result.Add('SeaGreen',             TdxAlphaColors.SeaGreen);
  Result.Add('SeaShell',             TdxAlphaColors.SeaShell);
  Result.Add('Sienna',               TdxAlphaColors.Sienna);
  Result.Add('Silver',               TdxAlphaColors.Silver);
  Result.Add('SkyBlue',              TdxAlphaColors.SkyBlue);
  Result.Add('SlateBlue',            TdxAlphaColors.SlateBlue);
  Result.Add('SlateGray',            TdxAlphaColors.SlateGray);
  Result.Add('SlateGrey',            TdxAlphaColors.SlateGray);
  Result.Add('Snow',                 TdxAlphaColors.Snow);
  Result.Add('SpringGreen',          TdxAlphaColors.SpringGreen);
  Result.Add('SteelBlue',            TdxAlphaColors.SteelBlue);
  Result.Add('Tan',                  TdxAlphaColors.Tan);
  Result.Add('Teal',                 TdxAlphaColors.Teal);
  Result.Add('Thistle',              TdxAlphaColors.Thistle);
  Result.Add('Tomato',               TdxAlphaColors.Tomato);
  Result.Add('Turquoise',            TdxAlphaColors.Turquoise);
  Result.Add('Violet',               TdxAlphaColors.Violet);
  Result.Add('Wheat',                TdxAlphaColors.Wheat);
  Result.Add('White',                TdxAlphaColors.White);
  Result.Add('WhiteSmoke',           TdxAlphaColors.WhiteSmoke);
  Result.Add('Yellow',               TdxAlphaColors.Yellow);
  Result.Add('YellowGreen',          TdxAlphaColors.YellowGreen);
end;

class procedure TdxCssParser.CssBackgroundKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AProperties: TArray<string>;
  ACount, I: Integer;
  AColor: TdxAlphaColor;
begin
  AProperties := TdxStringHelper.Split(APropertiesValue[0], [' '], [RemoveEmptyEntries]);
  ACount := Length(AProperties);
  for I := 0 to ACount - 1 do
  begin
    AColor := GetColor(AProperties[I]);
    if TdxAlphaColors.IsEmpty(AColor) then
      Continue;
    ACssProperties.CssCharacterProperties.BackColor := AColor;
    ACssProperties.CssParagraphProperties.BackColor := AColor;
    ACssProperties.CellProperties.BackgroundColor := AColor;
    ACssProperties.TableProperties.BackgroundColor := AColor;
  end;
end;

class procedure TdxCssParser.CssBackgroundAttachmentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBackgroundColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AColor: TdxAlphaColor;
begin
  AColor := GetColor(APropertiesValue[0]);

  ACssProperties.CssCharacterProperties.BackColor := AColor;
  ACssProperties.CellProperties.BackgroundColor := AColor;
  ACssProperties.TableProperties.BackgroundColor := AColor;
end;

class procedure TdxCssParser.CssBackgroundImageKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBackgroundPositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBackgroundRepeatKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBeforeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBorderKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ATopBorder: TdxHtmlBorderProperty;
begin
  ATopBorder := ACssProperties.BordersProperties.TopBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ATopBorder);

  ACssProperties.BordersProperties.LeftBorder.CopyFrom(ATopBorder);
  ACssProperties.BordersProperties.RightBorder.CopyFrom(ATopBorder);
  ACssProperties.BordersProperties.BottomBorder.CopyFrom(ATopBorder);
end;

class procedure TdxCssParser.CssBorderBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABottomBorder: TdxHtmlBorderProperty;
begin
  ABottomBorder := ACssProperties.BordersProperties.BottomBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABottomBorder);
end;

class procedure TdxCssParser.CssBorderKeywordCore(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>; ABorder: TdxHtmlBorderProperty);
var
  ANormalizedSpaces, APropertyValueWithoutRgbColorRepresentation, AColorRgbValue, AValue: string;
  AStartIndex, AEndIndex2, AColorRgbValueLength, I: Integer;
  ABordersContent: TArray<string>;
begin
  if Length(APropertiesValue) = 0 then
    Exit;
  ANormalizedSpaces := TdxStringHelper.Replace(APropertiesValue[0], '  ', ' ');
  APropertyValueWithoutRgbColorRepresentation := ANormalizedSpaces;
  if TdxStringHelper.Contains(ANormalizedSpaces, 'rgb') then
  begin
    AStartIndex := TdxStringHelper.IndexOf(ANormalizedSpaces, 'rgb');
    AEndIndex2 := TdxStringHelper.IndexOf(ANormalizedSpaces, ')', AStartIndex);
    AColorRgbValueLength := AEndIndex2 - AStartIndex + 1;
    AColorRgbValue := TdxStringHelper.Substring(ANormalizedSpaces, AStartIndex, AColorRgbValueLength);
    TdxTdTag.ImportBorderColor(ACssProperties.UnitConverter, AColorRgbValue, ABorder);
    APropertyValueWithoutRgbColorRepresentation := TdxStringHelper.Remove(ANormalizedSpaces, AStartIndex, AColorRgbValueLength);
    if APropertyValueWithoutRgbColorRepresentation = '' then
      Exit;
  end;
  ABordersContent := TdxStringHelper.Split(APropertyValueWithoutRgbColorRepresentation, [' ']);
  for I := 0 to Length(ABordersContent) - 1 do
  begin
    AValue := ABordersContent[I];

    TdxTdTag.ImportBorderWidth(ACssProperties.UnitConverter, AValue, ABorder);
    TdxTagBase.ImportBorderLineStyle(ACssProperties.UnitConverter, AValue, ABorder);
    TdxTdTag.ImportBorderColor(ACssProperties.UnitConverter, AValue, ABorder);
  end;
end;

class procedure TdxCssParser.CssBorderBottomColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.BottomBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderBottomStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.BottomBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderBottomWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABottomBorder: TdxHtmlBorderProperty;
begin
  ABottomBorder := ACssProperties.BordersProperties.BottomBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABottomBorder);
end;

class procedure TdxCssParser.CssBorderCollapseKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: string;
begin
  if Length(APropertiesValue) > 0 then
    AValue := LowerCase(APropertiesValue[0])
  else
    AValue := '';
  if AValue = 'collapse' then
    ACssProperties.TableProperties.BorderCollapse := TdxBorderCollapse.Collapse
  else
    if AValue = 'separate' then
      ACssProperties.TableProperties.BorderCollapse := TdxBorderCollapse.Separate;
end;

class procedure TdxCssParser.CssBorderLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.LeftBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderLeftColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.LeftBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderLeftStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.LeftBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderLeftWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.LeftBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.RightBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderRightColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.RightBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderRightStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.RightBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderRightWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.RightBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBorderStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;
  TdxTagBase.ImportBordersLineStyles(ACssProperties.UnitConverter, APropertiesValue[0], ACssProperties.BordersProperties);
end;

class procedure TdxCssParser.CssBorderColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;
  TdxTagBase.ImportBordersColors(ACssProperties.UnitConverter, APropertiesValue[0], ACssProperties.BordersProperties);
end;

class procedure TdxCssParser.CssBorderWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;
  TdxTagBase.ImportBordersWidths(ACssProperties.UnitConverter, APropertiesValue[0], ACssProperties.BordersProperties);
end;

class procedure TdxCssParser.CssBorderTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.TopBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderTopColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.TopBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderTopStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.TopBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBorderTopWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ABorder: TdxHtmlBorderProperty;
begin
  ABorder := ACssProperties.BordersProperties.TopBorder;
  CssBorderKeywordCore(ACssProperties, APropertiesValue, ABorder);
end;

class procedure TdxCssParser.CssBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssCaptionSideKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssClearKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssClipKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin

  ACssProperties.CssCharacterProperties.ForeColor := GetColor(APropertiesValue[0]);
end;

class function TdxCssParser.GetColor(const AValue: string): TdxAlphaColor;
begin
  if FHtmlColors.TryGetValue(AValue, Result) then
    Exit;
  Result := TdxMarkupLanguageColorParser.ParseColor(AValue);
end;

class procedure TdxCssParser.CssContentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssCounterIncrementKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssCounterResetKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssCursorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssDirectionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssDisplayKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: string;
begin
  AValue := APropertiesValue[0];
  if (AValue <> '') and CompareNoCase(AValue, 'none') then
    ACssProperties.CssCharacterProperties.Hidden := True;
end;

class procedure TdxCssParser.CssEmptyCellsKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssFirstChildKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssFloatKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AValue: string;
  ATargetAlign: TdxTableRowAlignment;
begin
  if Length(APropertiesValue) = 0 then
    Exit;
  AValue := APropertiesValue[0];
  ATargetAlign := TdxTableRowAlignment.Left;
  if (AValue <> '') and TdxTagBase.ImportAlignment(AValue, ATargetAlign) then
    ACssProperties.TableProperties.TableAlignment := ATargetAlign;

  if AValue = 'left' then
    ACssProperties.ImageProperties.CssFloat := TdxHtmlCssFloat.Left
  else
    if AValue = 'right' then
      ACssProperties.ImageProperties.CssFloat := TdxHtmlCssFloat.Right
    else
      if AValue = 'none' then
        ACssProperties.ImageProperties.CssFloat := TdxHtmlCssFloat.None;
end;

class procedure TdxCssParser.CssFocusKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssFontKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  APropertyValue, AToken: string;
  ATokenStartIndex, ASeparatorIndex: Integer;
  ANextFontProperty, AProperty: TdxFontProperties;
begin
  APropertyValue := APropertiesValue[0];
  ATokenStartIndex := 0;
  ANextFontProperty := TdxFontProperties.Style;
  while (ATokenStartIndex < Length(APropertyValue)) and (ANextFontProperty <= TdxFontProperties.Family) do
  begin
    if ANextFontProperty < TdxFontProperties.Family then
    begin
      ASeparatorIndex := TdxStringHelper.IndexOf(APropertyValue, ' ', ATokenStartIndex);
      if ASeparatorIndex < 0 then
        ASeparatorIndex := Length(APropertyValue);
    end
    else
      ASeparatorIndex := Length(APropertyValue);
    AToken := TdxStringHelper.Substring(APropertyValue, ATokenStartIndex, ASeparatorIndex - ATokenStartIndex);
    if AToken <> '' then
    begin
      for AProperty := ANextFontProperty to TdxFontProperties.Family do
      begin
        if TrySetFontProperty(ACssProperties, AProperty, AToken) then
        begin
          ANextFontProperty := Succ(AProperty);
          Break;
        end
        else
          if (AProperty = TdxFontProperties.Size) or (AProperty = TdxFontProperties.Family) then
            Exit;
      end;
    end;
    ATokenStartIndex := ASeparatorIndex + 1;
  end;
end;

class function TdxCssParser.TrySetFontProperty(ACssProperties: TdxCssProperties; AProperty: TdxFontProperties; const AValue: string): Boolean;
var
  ASizeParts: TArray<string>;
  ALineHeight: string;
begin
  case AProperty of
    TdxFontProperties.Style:
      Result := SetFontStyle(ACssProperties, AValue);
    TdxFontProperties.Variant:
      Result := SetFontVariant(ACssProperties, AValue);
    TdxFontProperties.Weight:
      Result := SetFontWeight(ACssProperties, AValue);
    TdxFontProperties.Size:
      begin
        ASizeParts := TdxStringHelper.Split(AValue, ['/'], [RemoveEmptyEntries]);
        if Length(ASizeParts) > 1 then
        begin
          ALineHeight := ASizeParts[1];
          CssLineHeightKeyword(ACssProperties, GetPropertiesList(ALineHeight));
        end;
        Result := SetFontSize(ACssProperties, AValue);
      end;
    TdxFontProperties.Family:
      begin
        SetFontFamily(ACssProperties, AValue);
        Result := True;
      end
    else
      Result := False;
  end;
end;

class function TdxCssParser.ShouldSplitFontSizeLineHeight(const AFontSizeProperties: string): Boolean;
var
  AFontsizeLineHeight: TArray<string>;
begin
  if AFontSizeProperties = '' then
    Exit(False);
  AFontsizeLineHeight := TdxStringHelper.Split(AFontSizeProperties, ['/']);
  Result := Length(AFontsizeLineHeight) = 2;
end;

class function TdxCssParser.GetPropertiesList(const AProperty: string): TArray<string>;
begin
  Result := TArray<string>.Create(AProperty);
end;

class procedure TdxCssParser.CssFontFamilyKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AFontFamily: string;
begin
  for AFontFamily in APropertiesValue do
    if SetFontFamily(ACssProperties, AFontFamily) then
      Exit
end;

class function TdxCssParser.SetFontFamily(ACssProperties: TdxCssProperties; const AValue: string): Boolean;
var
  AFontName, AName: string;
begin
  AFontName := TdxStringHelper.Trim(AValue, [' ', #$27, '"']);
  AName := LowerCase(AFontName);
  if AName = 'serif' then
    AFontName := 'Times New Roman'
  else
    if AName = 'sans-serif' then
      AFontName := 'Arial'
    else
      if AName = 'cursive' then
        AFontName := 'Comic Sans MS'
      else
        if AName = 'fantasy' then
          AFontName := 'Algerian'
        else
          if AName = 'monospace' then
            AFontName := 'Courier New';
  Result := TdxGdiFontCache.SystemTrueTypeFonts.ContainsKey(UpperCase(AFontName));
  ACssProperties.CssCharacterProperties.FontName := AFontName;
end;

class procedure TdxCssParser.CssFontSizeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if APropertiesValue[0] = '' then
    Exit;

  SetFontSize(ACssProperties, APropertiesValue[0]);
end;

class function TdxCssParser.SetFontSize(ACssProperties: TdxCssProperties; const AValue: string): Boolean;
var
  ACharacterFormatting: TdxCharacterFormattingBase;
  AHtmlFontSize: TdxHtmlFontSize;
  APar: TdxLengthValueParser;
  AFontSize: string;
begin
  ACharacterFormatting := ACssProperties.CssCharacterProperties;
  AHtmlFontSize := TdxHtmlFontSize.Create;
  AFontSize := LowerCase(AValue);
  if AFontSize = 'xx-small' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(1)
  else
  if AFontSize = 'x-small' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(2)
  else
  if AFontSize = 'small' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(3)
  else
  if AFontSize = 'medium' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(4)
  else
  if AFontSize = 'large' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(5)
  else
  if AFontSize = 'x-large' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(6)
  else
  if AFontSize = 'xx-large' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetDoubleFontSize(7)
  else
  if AFontSize = 'larger' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetLargerDoubleFontSize
  else
  if AFontSize = 'smaller' then
    ACharacterFormatting.DoubleFontSize := AHtmlFontSize.GetSmallerDoubleFontSize
  else
  if not ((AFontSize = 'inherit') or (AFontSize = 'initial')) then
    begin
      APar := TdxLengthValueParser.Create(AValue, ACssProperties.UnitConverter.ScreenDpi);
      if (not APar.Success) or (not APar.IsDigit) then
        Exit(False);
      if APar.PointsValue > 0 then
      begin
        ACssProperties.RelativeProperties.UnitRelativeFontSize := APar.RelativeUnit;
        ACharacterFormatting.DoubleFontSize := Max(1, Round(APar.PointsValue * 2));
      end;
    end;
  Result := True;
end;

class procedure TdxCssParser.CssFontStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  SetFontStyle(ACssProperties, APropertiesValue[0]);
end;

class function TdxCssParser.SetFontStyle(ACssProperties: TdxCssProperties; const AValue: string): Boolean;
var
  AFontStyle: string;
begin
  AFontStyle := LowerCase(AValue);
  if (AFontStyle = 'inherit') or (AFontStyle = 'initial') or (AFontStyle = 'normal') then
    ACssProperties.CssCharacterProperties.FontItalic := False
  else
    if (AFontStyle = 'italic') or (AFontStyle = 'oblique') then
      ACssProperties.CssCharacterProperties.FontItalic := True
    else
      Exit(False);
  Result := True;
end;

class procedure TdxCssParser.CssFontVariantKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  SetFontVariant(ACssProperties, APropertiesValue[0]);
end;

class function TdxCssParser.SetFontVariant(ACssProperties: TdxCssProperties; const AValue: string): Boolean;
var
  AFontVariant: string;
begin
  AFontVariant := LowerCase(AValue);
  Result := (AFontVariant = 'normal') or (AFontVariant = 'small-caps') or (AFontVariant = 'initial') or
    (AFontVariant = 'inherit');
end;

class procedure TdxCssParser.CssFontWeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  SetFontWeight(ACssProperties, APropertiesValue[0]);
end;

class function TdxCssParser.SetFontWeight(ACssProperties: TdxCssProperties; const AValue: string): Boolean;
var
  AFontWeight: Integer;
  AFontWeightName: string;
begin
  if TdxNumber.TryParse(AValue, AFontWeight) then
  begin
    case AFontWeight of
      100,
      200,
      300,
      400,
      500:
        ACssProperties.CssCharacterProperties.FontBold := False;
      600,
      700,
      800,
      900:
        ACssProperties.CssCharacterProperties.FontBold := True;
    end;
  end
  else
  begin
    AFontWeightName := LowerCase(AValue);
    if (AFontWeightName = 'initial') or (AFontWeightName = 'inherit') or (AFontWeightName = 'lighter')
      or (AFontWeightName = 'normal') then
      ACssProperties.CssCharacterProperties.FontBold := False
    else
      if (AFontWeightName = 'bold') or (AFontWeightName = 'bolder') then
        ACssProperties.CssCharacterProperties.FontBold := True
      else
        Exit(False);
  end;
  Result := True;
end;

class procedure TdxCssParser.CssHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  APar: TdxLengthValueParser;
  AValue: Integer;
  ANewHeight: TdxHeightUnitInfo;
  ANewWidth: TdxWidthUnitInfo;
begin
  if APropertiesValue[0] = '' then
    Exit;
  APar := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if (not APar.IsDigit) or (APar.PointsValue < 0) then
    Exit;

  if not APar.IsRelativeUnit then
  begin
    AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(APar.PointsValue));
    ANewHeight := TdxHeightUnitInfo.Create(AValue, TdxHeightUnitType.Minimum);
    try
      ACssProperties.RowProperties.Height := ANewHeight;
    finally
      ANewHeight.Free;
    end;

    ANewWidth := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AValue);
    try
      ACssProperties.ImageProperties.Height := ANewWidth;
    finally
      ANewWidth.Free;
    end;
  end
  else
  begin
    ACssProperties.RelativeProperties.RelativeHeight := APar.Value;
    ACssProperties.RelativeProperties.UnitRelativeHeight := APar.&Unit;
  end;
end;

class procedure TdxCssParser.CssHoverKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssLetterSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssLineHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  APar: TdxLengthValueParser;
  AMultiple: Single;
begin
  if APropertiesValue[0] = '' then
    Exit;
  APar := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if (not APar.IsDigit) or (APar.PointsValue <= 0) then
    Exit;

  if APar.&Unit = '' then
  begin
    ACssProperties.CssParagraphProperties.SetMultipleLineSpacing(APar.PointsValue);
  end
  else
    if APar.&Unit = '%' then
    begin
      AMultiple := APar.PointsValue / 100;
      ACssProperties.CssParagraphProperties.SetMultipleLineSpacing(AMultiple);
    end
    else
    begin
      ACssProperties.CssParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.AtLeast;
      if APar.IsRelativeUnit then
      begin
        ACssProperties.RelativeProperties.UnitRelativeLineSpacing := APar.RelativeUnit;
        ACssProperties.CssParagraphProperties.LineSpacing := APar.PointsValue;
      end
      else
        ACssProperties.CssParagraphProperties.LineSpacing := ACssProperties.UnitConverter.PointsToModelUnitsF(APar.PointsValue);
    end;
end;

class procedure TdxCssParser.CssLinkKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssListStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssListStyleImageKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssListStylePositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssListStyleTypeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AStyleType: string;
begin
  AStyleType := UpperCase(APropertiesValue[0]);
  if AStyleType = 'DISC' then
    begin
      ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Bullet;
      ACssProperties.ListLevelProperties.BulletFontName := 'Symbol';
    end
  else
  if AStyleType = 'CIRCLE' then
    begin
      ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Bullet;
      ACssProperties.ListLevelProperties.BulletFontName := 'Courier New';
    end
  else
  if AStyleType = 'SQUARE' then
    begin
      ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Bullet;
      ACssProperties.ListLevelProperties.BulletFontName := 'Wingdings';
    end
  else
  if AStyleType = 'DECIMAL' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Decimal
  else
  if AStyleType = 'DECIMAL-LEADING-ZERO' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.DecimalZero
  else
  if AStyleType = 'UPPER-LATIN' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.UpperLetter
  else
  if AStyleType = 'LOWER-LATIN' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.LowerLetter
  else
  if AStyleType = 'LOWER-ALPHA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.LowerLetter
  else
  if AStyleType = 'UPPER-ALPHA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.UpperLetter
  else
  if AStyleType = 'UPPER-ROMAN' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.UpperRoman
  else
  if AStyleType = 'LOWER-ROMAN' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.LowerRoman
  else
  if AStyleType = 'HEBREW' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Hebrew1
  else
  if AStyleType = 'HIRAGANA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.AIUEOHiragana
  else
  if AStyleType = 'KATAKANA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.AIUEOFullWidthHiragana
  else
  if AStyleType = 'HIRAGANA-IROHA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.Iroha
  else
  if AStyleType = 'KATAKANA-IROHA' then
    ACssProperties.ListLevelProperties.Format := TdxNumberingFormat.IrohaFullWidth;
end;

class procedure TdxCssParser.CssMarginKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AProperties: TArray<string>;
begin
  AProperties := TdxStringHelper.Split(APropertiesValue[0], [' '], [RemoveEmptyEntries]);
  case Length(AProperties) of
    1:
      CssMarginKeywordCore(ACssProperties, AProperties[0], AProperties[0], AProperties[0], AProperties[0]);
    2:
      CssMarginKeywordCore(ACssProperties, AProperties[0], AProperties[0], AProperties[1], AProperties[1]);
    3:
      CssMarginKeywordCore(ACssProperties, AProperties[2], AProperties[0], AProperties[1], AProperties[1]);
    4:
      CssMarginKeywordCore(ACssProperties, AProperties[2], AProperties[0], AProperties[3], AProperties[1]);
  end;
end;

class procedure TdxCssParser.CssMarginKeywordCore(ACssProperties: TdxCssProperties; const ABottom: string;
  const ATop: string; const ALeft: string; const ARight: string);
begin
  CssMarginBottomKeyword(ACssProperties, CreateStringList(ABottom));
  CssMarginTopKeyword(ACssProperties, CreateStringList(ATop));
  CssMarginLeftKeyword(ACssProperties, CreateStringList(ALeft));
  CssMarginRightKeyword(ACssProperties, CreateStringList(ARight));
end;

class function TdxCssParser.CreateStringList(const AItem: string): TArray<string>;
begin
  Result := TArray<string>.Create(AItem);
end;

class procedure TdxCssParser.CssMarginTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ASpacingBefore: Single;
  AResult: Integer;
begin
  ASpacingBefore := ACssProperties.CssParagraphProperties.SpacingBefore;
  ACssProperties.RelativeProperties.UnitRelativeSpacingBefore := SetMarginProperties(APropertiesValue, ASpacingBefore,
    ACssProperties.UnitConverter);
  AResult := Round(ASpacingBefore);
  if AResult > 0 then
    ACssProperties.CssParagraphProperties.SpacingBefore := AResult
  else
    ACssProperties.CssParagraphProperties.SpacingBefore := 0;
end;

class procedure TdxCssParser.CssMarginBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ASpacingAfter: Single;
  AResult: Integer;
begin
  ASpacingAfter := ACssProperties.CssParagraphProperties.SpacingAfter;
  ACssProperties.RelativeProperties.UnitRelativeSpacingAfter := SetMarginProperties(APropertiesValue, ASpacingAfter,
    ACssProperties.UnitConverter);
  AResult := Round(ASpacingAfter);
  if AResult > 0 then
    ACssProperties.CssParagraphProperties.SpacingAfter := AResult
  else
    ACssProperties.CssParagraphProperties.SpacingAfter := 0;
end;

class procedure TdxCssParser.CssMarginLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ALeftIndent: Single;
  AIndent: TdxWidthUnitInfo;
begin
  ALeftIndent := ACssProperties.CssParagraphProperties.LeftIndent;
  ACssProperties.RelativeProperties.UnitRelativeLeftIndent := SetMarginProperties(APropertiesValue, ALeftIndent,
    ACssProperties.UnitConverter);
  ACssProperties.CssParagraphProperties.LeftIndent := Round(ALeftIndent);

  AIndent := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, Round(ALeftIndent));
  try
    ACssProperties.TableProperties.Indent := AIndent;
  finally
    AIndent.Free;
  end;
end;

class procedure TdxCssParser.CssMarginRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ARightIndent: Single;
begin
  ARightIndent := ACssProperties.CssParagraphProperties.RightIndent;
  ACssProperties.RelativeProperties.UnitRelativeRightIndent := SetMarginProperties(APropertiesValue, ARightIndent,
    ACssProperties.UnitConverter);
  ACssProperties.CssParagraphProperties.RightIndent := Round(ARightIndent);
end;

class procedure TdxCssParser.CssMaxHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssMaxWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssMinHeightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssMinWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssBeforeAutoSpacing(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if (Length(APropertiesValue) > 0) and CompareNoCase(APropertiesValue[0], 'auto') then
    ACssProperties.CssParagraphProperties.BeforeAutoSpacing := True;
end;

class procedure TdxCssParser.CssAfterAutoSpacing(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if (Length(APropertiesValue) > 0) and CompareNoCase(APropertiesValue[0], 'auto') then
    ACssProperties.CssParagraphProperties.AfterAutoSpacing := True;
end;

class procedure TdxCssParser.CssMsoPagination(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ACount, I: Integer;
begin
  ACount := Length(APropertiesValue);
  for I := 0 to ACount - 1 do
  begin
    if CompareNoCase(APropertiesValue[I], 'lines-together') then
      ACssProperties.CssParagraphProperties.KeepLinesTogether := True;
  end;
end;

class procedure TdxCssParser.CssOpacityKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssOutlineKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssOutlineColorKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssOutlineStyleKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssOutlineWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssOverflowKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssPaddingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AProperties: TArray<string>;
begin
  AProperties := TdxStringHelper.Split(APropertiesValue[0], [' '], [RemoveEmptyEntries]);
  case Length(AProperties) of
    1:
      CssPaddingKeywordCore(ACssProperties, AProperties[0], AProperties[0], AProperties[0], AProperties[0]);
    2:
      CssPaddingKeywordCore(ACssProperties, AProperties[0], AProperties[0], AProperties[1], AProperties[1]);
    3:
      CssPaddingKeywordCore(ACssProperties, AProperties[0], AProperties[2], AProperties[1], AProperties[1]);
    4:
      CssPaddingKeywordCore(ACssProperties, AProperties[2], AProperties[0], AProperties[3], AProperties[1]);
  end;
end;

class procedure TdxCssParser.CssPaddingKeywordCore(ACssProperties: TdxCssProperties; const ABottom: string;
  const ATop: string; const ALeft: string; const ARight: string);
begin
  CssPaddingBottomKeyword(ACssProperties, CreateStringList(ABottom));
  CssPaddingTopKeyword(ACssProperties, CreateStringList(ATop));
  CssPaddingLeftKeyword(ACssProperties, CreateStringList(ALeft));
  CssPaddingRightKeyword(ACssProperties, CreateStringList(ARight));
end;

class procedure TdxCssParser.CssPaddingBottomKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AParser: TdxLengthValueParser;
  AValue: Integer;
begin
  if APropertiesValue[0] = '' then
    Exit;

  AParser := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if AParser.IsDigit and (AParser.PointsValue >= 0) then
  begin
    if not AParser.IsRelativeUnit then
    begin
      AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AParser.PointsValue));
      ACssProperties.CellProperties.CellMargins.Bottom.Value := AValue;
      ACssProperties.CellProperties.CellMargins.Bottom.&Type := TdxWidthUnitType.ModelUnits;
    end;
  end;
end;

class procedure TdxCssParser.CssPaddingLeftKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AParser: TdxLengthValueParser;
  AValue: Integer;
begin
  if APropertiesValue[0] = '' then
    Exit;

  AParser := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if AParser.IsDigit and (AParser.PointsValue >= 0) then
  begin
    if not AParser.IsRelativeUnit then
    begin
      AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AParser.PointsValue));
      ACssProperties.CellProperties.CellMargins.Left.Value := AValue;
      ACssProperties.CellProperties.CellMargins.Left.&Type := TdxWidthUnitType.ModelUnits;
    end;
  end;
end;

class procedure TdxCssParser.CssPaddingRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AParser: TdxLengthValueParser;
  AValue: Integer;
begin
  if APropertiesValue[0] = '' then
    Exit;
  AParser := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if AParser.IsDigit and (AParser.PointsValue >= 0) then
  begin
    if not AParser.IsRelativeUnit then
    begin
      AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AParser.PointsValue));
      ACssProperties.CellProperties.CellMargins.Right.Value := AValue;
      ACssProperties.CellProperties.CellMargins.Right.&Type := TdxWidthUnitType.ModelUnits;
    end;
  end;
end;

class procedure TdxCssParser.CssPaddingTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AParser: TdxLengthValueParser;
  AValue: Integer;
begin
  if APropertiesValue[0] = '' then
    Exit;

  AParser := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if AParser.IsDigit and (AParser.PointsValue >= 0) then
  begin
    if not AParser.IsRelativeUnit then
    begin
      AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AParser.PointsValue));
      ACssProperties.CellProperties.CellMargins.Top.Value := AValue;
      ACssProperties.CellProperties.CellMargins.Top.&Type := TdxWidthUnitType.ModelUnits;
    end;
  end;
end;

class procedure TdxCssParser.CssPageBreakAfterKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;

  if CompareNoCase(APropertiesValue[0], 'avoid') then
    ACssProperties.CssParagraphProperties.KeepWithNext := True;
end;

class procedure TdxCssParser.CssPageBreakBeforeKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;

  if CompareNoCase(APropertiesValue[0], 'always') then
    ACssProperties.CssParagraphProperties.PageBreakBefore := True;
end;

class procedure TdxCssParser.CssPageBreakInsideKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if Length(APropertiesValue) = 0 then
    Exit;

  if CompareNoCase(APropertiesValue[0], 'avoid') then
    ACssProperties.CssParagraphProperties.KeepLinesTogether := True;
end;

class procedure TdxCssParser.CssPositionKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssQuotesKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssRightKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssTableLayoutKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssTextAlignKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ATextAlign: string;
begin
  ATextAlign := UpperCase(APropertiesValue[0]);
  if ATextAlign = 'LEFT' then
    ACssProperties.CssParagraphProperties.Alignment := TdxParagraphAlignment.Left
  else
  if ATextAlign = 'RIGHT' then
    ACssProperties.CssParagraphProperties.Alignment := TdxParagraphAlignment.Right
  else
  if ATextAlign = 'CENTER' then
    ACssProperties.CssParagraphProperties.Alignment := TdxParagraphAlignment.Center
  else
  if ATextAlign = 'JUSTIFY' then
    ACssProperties.CssParagraphProperties.Alignment := TdxParagraphAlignment.Justify;
end;

class procedure TdxCssParser.CssTextDecorationKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AProperties: TArray<string>;
  ACount, I: Integer;
  AVAlue: string;
begin
  AProperties := TdxStringHelper.Split(APropertiesValue[0], [' '], [RemoveEmptyEntries]);
  ACount := Length(AProperties);
  for I := 0 to ACount - 1 do
  begin
    AValue := UpperCase(AProperties[I]);
    if AValue = 'LINE-THROUGH' then
      ACssProperties.CssCharacterProperties.FontStrikeoutType := TdxStrikeoutType.Single
    else
    if AValue = 'UNDERLINE' then
      ACssProperties.CssCharacterProperties.FontUnderlineType := TdxUnderlineType.Single
    else
    if AValue = 'NONE' then
      begin
        ACssProperties.CssCharacterProperties.FontUnderlineType := TdxUnderlineType.None;
        ACssProperties.CssCharacterProperties.FontStrikeoutType := TdxStrikeoutType.None;
      end;
  end;
end;

class procedure TdxCssParser.CssTextIndentKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  APar: TdxLengthValueParser;
  AIndent: Single;
begin
  if APropertiesValue[0] = '' then
    Exit;
  APar := TdxLengthValueParser.Create(APropertiesValue[0], ACssProperties.UnitConverter.ScreenDpi);
  if not APar.IsDigit then
    Exit;
  AIndent := SetLineIndentType(ACssProperties, APar.PointsValue);
  if APar.IsRelativeUnit then
  begin
    ACssProperties.RelativeProperties.UnitRelativeFirstLineIndent := APar.RelativeUnit;
    ACssProperties.CssParagraphProperties.FirstLineIndent := Round(AIndent);
  end
  else
    ACssProperties.CssParagraphProperties.FirstLineIndent := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AIndent));
end;

class function TdxCssParser.SetMarginProperties(const APropertiesValue: TArray<string>; var AMarginValue: Single;
  AUnitConverter: TdxDocumentModelUnitConverter): string;
var
  AParser: TdxLengthValueParser;
begin
  if APropertiesValue[0] = '' then
    Exit('');
  AParser := TdxLengthValueParser.Create(APropertiesValue[0], AUnitConverter.ScreenDpi);
  if not AParser.IsDigit then
    Exit('');
  if AParser.IsRelativeUnit then
  begin
    AMarginValue := AParser.PointsValue;
    Result := AParser.RelativeUnit;
  end
  else
  begin
    AMarginValue := AUnitConverter.PointsToModelUnitsF(AParser.PointsValue);
    Result := '';
  end;
end;

class function TdxCssParser.SetLineIndentType(ACssProperties: TdxCssProperties; AIndent: Single): Single;
begin
  if AIndent < 0 then
  begin
    ACssProperties.CssParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;

    AIndent := -AIndent;
  end
  else
    ACssProperties.CssParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented;
  Result := AIndent;
end;

class procedure TdxCssParser.CssTextTransformKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
  if (Length(APropertiesValue) = 1) and (CompareText(APropertiesValue[0], 'uppercase') = 0) then
    ACssProperties.CssCharacterProperties.AllCaps := True;
end;

class procedure TdxCssParser.CssTopKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssUnicodeBidiKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssVerticalAlignKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AAlignment: TdxVerticalAlignment;
begin

  if Length(APropertiesValue) <> 1 then
    Exit;
  AAlignment := TdxTagBase.ReadVerticalAlignment(APropertiesValue[0]);
  ACssProperties.CellProperties.VerticalAlignment := AAlignment;
  ACssProperties.RowProperties.VerticalAlignment := AAlignment;
end;

class procedure TdxCssParser.CssVisibilityKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssVisitedKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

class procedure TdxCssParser.CssWhiteSpaceKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  AReadedValue: string;
begin
  if Length(APropertiesValue) <> 1 then
    Exit;
  AReadedValue := APropertiesValue[0];
  if AReadedValue = 'nowrap' then
    ACssProperties.CellProperties.NoWrap := True
  else
    if AReadedValue = 'normal' then
      ACssProperties.CellProperties.NoWrap := False;
end;

class procedure TdxCssParser.CssWidthKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
var
  ASeparatorPos, AValue: Integer;
  AParser: TdxLengthValueParser;
  ANewWidth: TdxWidthUnitInfo;
  APropertyValue: string;
begin
  APropertyValue := APropertiesValue[0];
  if APropertyValue = '' then
    Exit;

  ASeparatorPos := TdxStringHelper.LastIndexOf(APropertyValue, '!');
  if ASeparatorPos > 1 then
    APropertyValue := TdxStringHelper.Substring(APropertyValue, 1, ASeparatorPos - 1);
  AParser := TdxLengthValueParser.Create(APropertyValue, ACssProperties.UnitConverter.ScreenDpi);
  if AParser.IsDigit and (AParser.PointsValue > 0) then
  begin
    if not AParser.IsRelativeUnit then
    begin
      AValue := Round(ACssProperties.UnitConverter.PointsToModelUnitsF(AParser.PointsValue));

      ACssProperties.CellProperties.PreferredWidth.Value := AValue;
      ACssProperties.CellProperties.PreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
      ACssProperties.TableProperties.Width := ACssProperties.CellProperties.PreferredWidth.Info;

      ANewWidth := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, AValue);
      try
        ACssProperties.ImageProperties.Width := ANewWidth;
      finally
        ANewWidth.Free;
      end;
    end
    else
    begin
      ACssProperties.RelativeProperties.RelativeWidth := AParser.Value;
      ACssProperties.RelativeProperties.UnitRelativeWidth := AParser.&Unit;
    end;
  end;
end;

class procedure TdxCssParser.CssWordSpacingKeyword(ACssProperties: TdxCssProperties; const APropertiesValue: TArray<string>);
begin
end;

function TdxCssParser.GetCssKeywordTable: TdxCssKeywordTranslatorTable;
begin
  Result := FAcssKeywordTable;
end;

end.
