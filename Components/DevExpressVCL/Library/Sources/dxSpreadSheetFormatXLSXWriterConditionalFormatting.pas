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

unit dxSpreadSheetFormatXLSXWriterConditionalFormatting;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Types, Windows, SysUtils, Classes, Graphics, dxCore, dxCoreClasses, cxClasses, dxCustomTree,
  dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxSpreadSheetGraphics, Generics.Defaults, Generics.Collections, dxGDIPlusClasses, dxCoreGraphics, cxGeometry,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetFormatXLSXWriter,
  dxSpreadSheetConditionalFormatting, dxXMLWriter;

type

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilderClass = class of TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder;
  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder = class(TdxSpreadSheetCustomPackedWriterBuilder)
  strict private
    FNamePrefix: string;
    FRule: TdxSpreadSheetCustomConditionalFormattingRule;
    FWriter: TdxXmlWriter;

    function GetConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList;
    function GetOwner: TdxSpreadSheetXLSXWriter;
  protected
    procedure AddColorNode(const ANodeName: string; AColor: TColor);
    function GetRuleStyleID(out ID: Integer): Boolean; virtual;
    function GetRuleType: TdxXMLString; virtual; abstract;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AWriter: TdxXmlWriter;
      ARule: TdxSpreadSheetCustomConditionalFormattingRule; const ANamePrefix: string); virtual;
    procedure Execute; override; final;
    procedure WriteAttributes; virtual;
    procedure WriteChildren; virtual;
    //
    property ConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList read GetConditionalFormattingStyles;
    property NamePrefix: string read FNamePrefix;
    property Owner: TdxSpreadSheetXLSXWriter read GetOwner;
    property Rule: TdxSpreadSheetCustomConditionalFormattingRule read FRule;
    property Writer: TdxXmlWriter read FWriter;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder = class(TdxSpreadSheetCustomPackedWriterBuilder)
  strict private
    FConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    FWriter: TdxXmlWriter;

    function GetRule(Index: Integer): TdxSpreadSheetCustomConditionalFormattingRule;
    function GetRuleCount: Integer;
  protected
    FRuleBuilders: TDictionary<TClass, TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilderClass>;

    function EncodeReferences(AAreas: TdxSpreadSheetAreaList): string;
    function IsCustomFormattedRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
    function IsValidRule(ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
    procedure RegisterRulesBuilders; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AWriter: TdxXmlWriter;
      AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property RuleCount: Integer read GetRuleCount;
    property Rules[Index: Integer]: TdxSpreadSheetCustomConditionalFormattingRule read GetRule;
    property Writer: TdxXmlWriter read FWriter;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder)
  protected
    procedure RegisterRulesBuilders; override;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder)
  protected
    function GetRuleStyleID(out ID: Integer): Boolean; override;
  end;

  { TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder }

  TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
  protected
    function GetRuleType: TdxXMLString; override;
  public
    procedure WriteAttributes; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleExpression;
  protected
    function GetRuleType: TdxXMLString; override;
  public
    procedure WriteChildren; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleExpression read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleCellIs;
  protected
    function GetRuleType: TdxXMLString; override;
  public
    procedure WriteAttributes; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleCellIs read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleCustomColorScale;
    function GetStop(Index: Integer): TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
    function GetStopCount: Integer;
  protected
    function GetRuleType: TdxXMLString; override;
    procedure WriteStop(AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop);
  public
    procedure WriteChildren; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleCustomColorScale read GetRule;
    property StopCount: Integer read GetStopCount;
    property Stops[Index: Integer]: TdxSpreadSheetConditionalFormattingRuleColorScaleStop read GetStop;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDuplicateValuesRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDuplicateValuesRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder)
  protected
    function GetRuleType: TdxXMLString; override;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleDataBar;
  protected
    function GetRuleType: TdxXMLString; override;
    procedure WriteStop(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
  public
    procedure WriteChildren; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleDataBar read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder)
  strict private
    function EncodeValue(const AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop): string;
    function GetPresetName: string;
    function GetRule: TdxSpreadSheetConditionalFormattingRuleIconSet;
  protected
    function GetRuleType: TdxXMLString; override;
    procedure WriteStop(AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
    procedure WriteStopCustomIcon(AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
  public
    procedure WriteChildren; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleIconSet read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder }

  TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
  protected
    function GetRuleType: TdxXMLString; override;
  public
    procedure WriteAttributes; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues read GetRule;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingUniqueValuesRuleBuilder }

  TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingUniqueValuesRuleBuilder = class(
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder)
  protected
    function GetRuleType: TdxXMLString; override;
  end;

implementation

uses
  AnsiStrings, Math, TypInfo, StrUtils, dxColorPicker, cxGraphics, dxHashUtils, dxTypeHelpers,
  dxSpreadSheetFormulas, dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatXLSX, dxSpreadSheetFormatUtils,
  dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.Create(
  AOwner: TdxSpreadSheetCustomFiler; AWriter: TdxXmlWriter;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule; const ANamePrefix: string);
begin
  inherited Create(AOwner);
  FNamePrefix := ANamePrefix;
  FWriter := AWriter;
  FRule := ARule;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.Execute;
begin
  Writer.WriteStartElement(FNamePrefix, sdxXLSXNodeConditionalFormattingRule, '');
  try
    if NamePrefix <> '' then
      Writer.WriteAttributeString('id', dxGenerateGUID);
    WriteAttributes;
    WriteChildren;
  finally
    Writer.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.WriteAttributes;
var
  APropInfo: PPropInfo;
  ID: Integer;
begin
  Writer.WriteAttributeString(sdxXLSXAttrTypeLC, GetRuleType);
  if GetRuleStyleID(ID) then
    Writer.WriteAttributeInteger(sdxXLSXAttrDfxID, ID);
  Writer.WriteAttributeInteger(sdxXLSXAttrPriority, Rule.Index + 1);

  APropInfo := GetPropInfo(Rule, 'StopIfTrue');
  if (APropInfo <> nil) and (GetOrdProp(Rule, APropInfo) <> 0) then
    Writer.WriteAttributeBoolean(sdxXLSXAttrStopIfTrue, True);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.WriteChildren;
begin
  // do nothing
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.AddColorNode(const ANodeName: string; AColor: TColor);
begin
  if cxColorIsValid(AColor) then
  begin
    Writer.WriteStartElement(FNamePrefix, ANodeName, '');
    Writer.WriteAttributeString(sdxXLSXAttrRGB, TdxColorHelper.AlphaColorToHexCode(dxColorToAlphaColor(AColor), False));
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.GetRuleStyleID(out ID: Integer): Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.GetConditionalFormattingStyles: TdxSpreadSheetXLSXWriterResourceList;
begin
  Result := Owner.ConditionalFormattingStyles;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilder.GetOwner: TdxSpreadSheetXLSXWriter;
begin
  Result := TdxSpreadSheetXLSXWriter(inherited Owner);
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.Create(
  AOwner: TdxSpreadSheetCustomFiler; AWriter: TdxXmlWriter; AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FWriter := AWriter;
  FConditionalFormatting := AConditionalFormatting;
  FRuleBuilders := TDictionary<TClass, TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilderClass>.Create;
  RegisterRulesBuilders;
end;

destructor TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.Destroy;
begin
  FreeAndNil(FRuleBuilders);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.Execute;
var
  AClass: TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilderClass;
  ANodeStarted: Boolean;
  APrevAreas: TdxSpreadSheetAreaList;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  APrevAreas := nil;
  ANodeStarted := False;
  for I := 0 to RuleCount - 1 do
  begin
    ARule := Rules[I];
    if IsValidRule(ARule) and not IsCustomFormattedRule(ARule) and FRuleBuilders.TryGetValue(ARule.ClassType, AClass) then
    begin
      if not ARule.Areas.Equals(APrevAreas) then
      begin
        if ANodeStarted then
          Writer.WriteEndElement;

        Writer.WriteStartElement(sdxXLSXNodeConditionalFormatting);
        Writer.WriteAttributeString(sdxXLSXAttrSqRef, EncodeReferences(ARule.Areas));
        APrevAreas := ARule.Areas;
        ANodeStarted := True;
      end;
      ExecuteSubTask(AClass.Create(Owner, Writer, ARule, ''));
    end;
  end;
  if ANodeStarted then
    Writer.WriteEndElement;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.EncodeReferences(AAreas: TdxSpreadSheetAreaList): string;
begin
  Result := StringReplace(AAreas.ToString, AAreas.ValueSeparator, ' ', [rfReplaceAll]);
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.IsCustomFormattedRule(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
begin
  Result := (ARule is TdxSpreadSheetConditionalFormattingRuleDataBar) or
    (ARule is TdxSpreadSheetConditionalFormattingRuleIconSet) and
    (TdxSpreadSheetConditionalFormattingRuleIconSet(ARule).PresetName = '');
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.IsValidRule(
  ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
begin
  Result := ARule.Areas.Count > 0;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.RegisterRulesBuilders;
begin
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleCellIs,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleExpression,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleDuplicateValues,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDuplicateValuesRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleUniqueValues,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingUniqueValuesRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage,
    TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleTopBottomValues,
    TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleThreeColorScale,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleTwoColorScale,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleIconSet,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder);
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.GetRule(
  Index: Integer): TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := FConditionalFormatting.Rules[Index];
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingBuilder.GetRuleCount: Integer;
begin
  Result := FConditionalFormatting.RuleCount;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder.RegisterRulesBuilders;
begin
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleIconSet,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder);
  FRuleBuilders.AddOrSetValue(TdxSpreadSheetConditionalFormattingRuleDataBar,
    TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExBuilder.Execute;
var
  AClass: TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCustomRuleBuilderClass;
  ANodeStarted: Boolean;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  ANodeStarted := False;
  for I := 0 to RuleCount - 1 do
  begin
    ARule := Rules[I];
    if IsValidRule(ARule) and IsCustomFormattedRule(ARule) and FRuleBuilders.TryGetValue(ARule.ClassType, AClass) then
    begin
      if not ANodeStarted then
      begin
        Writer.WriteStartElement(sdxXLSXNodeExtList);
        Writer.WriteStartElement(sdxXLSXNodeExt);
        Writer.WriteAttributeString('xmlns', 'x14', '', 'http://schemas.microsoft.com/office/spreadsheetml/2009/9/main');
        Writer.WriteAttributeString('uri', '{78C0D931-6437-407d-A8EE-F0AAD7539E65}');
        Writer.WriteStartElement(sdxXLSXNamespaceX14, sdxXLSXNodeConditionalFormattings, '');
        ANodeStarted := True;
      end;

      Writer.WriteStartElement(sdxXLSXNamespaceX14, sdxXLSXNodeConditionalFormatting, '');
      try
        Writer.WriteAttributeString('xmlns', 'xm', '', 'http://schemas.microsoft.com/office/excel/2006/main');
        ExecuteSubTask(AClass.Create(Owner, Writer, ARule, sdxXLSXNamespaceX14));
        Writer.WriteElementString('xm', 'sqref', '', EncodeReferences(ARule.Areas));
      finally
        Writer.WriteEndElement;
      end;
    end;
  end;

  if ANodeStarted then
  begin
    Writer.WriteEndElement;
    Writer.WriteEndElement;
    Writer.WriteEndElement;
  end;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder }

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingStyleBasedRuleBuilder.GetRuleStyleID(out ID: Integer): Boolean;
begin
  ID := ConditionalFormattingStyles.Add(TdxSpreadSheetConditionalFormattingRuleStyleBased(Rule).Style.Handle);
  Result := True;
end;

{ TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder }

procedure TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder.WriteAttributes;
begin
  inherited;
  if Rule.ComparisonOperator in [abacoAboveOrEqualAverage, abacoBelowOrEqualAverage] then
    Writer.WriteAttributeInteger(sdxXLSXAttrEqualAverage, 1);
  if Rule.ComparisonOperator in [abacoBelowAverage, abacoBelowOrEqualAverage, abacoBelowAverageOnStandardDeviation] then
    Writer.WriteAttributeInteger(sdxXLSXAttrAboveAverage, 0);
  if Rule.ComparisonOperator in [abacoAboveAverageOnStandardDeviation, abacoBelowAverageOnStandardDeviation] then
    Writer.WriteAttributeInteger(sdxXLSXAttrStdDev, Rule.StandardDeviationLevel);
end;

function TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(inherited Rule);
end;

function TdxSpreadSheetXLSXWriterConditionalFormattingAboveAverageRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueAboveAverage;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder.WriteChildren;
begin
  inherited;
  Writer.WriteElementString(sdxXLSXNodeFormula, dxSpreadSheetFormulaExcludeEqualSymbol(Rule.Expression));
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueExpression;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingExpressionRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleExpression;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleExpression(inherited Rule);
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder.WriteAttributes;
begin
  inherited;
  Writer.WriteAttributeString(sdxXLSXAttrOperator, dxXLSXCfCellIsRuleOperator[Rule.ComparisonOperator]);
  if Rule.ComparisonOperator in [cicoBetween, cicoNotBetween] then
    Writer.WriteElementString(sdxXLSXNodeFormula, dxSpreadSheetFormulaExcludeEqualSymbol(Rule.Expression2));
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueCellIs;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingCellIsRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleCellIs;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCellIs(inherited Rule);
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.WriteChildren;
var
  I: Integer;
begin
  inherited;

  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeColorScale, '');
  try
    for I := 0 to StopCount - 1 do
      WriteStop(Stops[I]);
    for I := 0 to StopCount - 1 do
      AddColorNode(sdxXLSXNodeColor, Stops[I].Color);
  finally
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueColorScale;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.WriteStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop);
var
  AValueType: AnsiString;
begin
  if AStop.ValueType <> cssvtLimitValue then
    AValueType := dxXLSXScaleStopValueTypeMap[AStop.ValueType]
  else
    if AStop <> Stops[StopCount - 1] then
      AValueType := sdxXLSXValueTypeMin
    else
      AValueType := sdxXLSXValueTypeMax;

  Writer.WriteStartElement(sdxXLSXNodeCFVO);
  Writer.WriteAttributeString(sdxXLSXAttrTypeLC, AValueType);
  if AStop.ValueType <> cssvtLimitValue then
    Writer.WriteAttributeString(sdxXLSXAttrVal, AStop.Value);
  Writer.WriteEndElement;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleCustomColorScale;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomColorScale(inherited Rule);
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.GetStop(
  Index: Integer): TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess(Rule).Stops[Index];
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingColorScaleBuilder.GetStopCount: Integer;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess(Rule).StopCount;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDuplicateValuesRuleBuilder }

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDuplicateValuesRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueDuplicateValues;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder.WriteChildren;
begin
  inherited;

  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeDataBar, '');
  try
    Writer.WriteAttributeInteger('minLength', 0);
    Writer.WriteAttributeInteger('maxLength', 100);
    Writer.WriteAttributeBoolean(sdxXLSXAttrShowValue, Rule.ShowValue);
    Writer.WriteAttributeBoolean(sdxXLSXAttrBorder, cxColorIsValid(Rule.Style.PositiveBarBorderColor));
    Writer.WriteAttributeBoolean(sdxXLSXAttrGradient, Rule.Style.FillMode = dbfmGradient);
    Writer.WriteAttributeString(sdxXLSXAttrDirection, dxXLSXDataBarDirectionMap[Rule.Style.Direction]);
    Writer.WriteAttributeBoolean(sdxXLSXAttrNegativeBarColorSameAsPositive, Rule.Style.NegativeBarColor = clDefault);
    Writer.WriteAttributeBoolean(sdxXLSXAttrNegativeBarBorderColorSameAsPositive, Rule.Style.NegativeBarBorderColor = clDefault);
    Writer.WriteAttributeString(sdxXLSXAttrAxisPosition, dxXLSXAxisPosition[Rule.Style.AxisPosition]);

    WriteStop(Rule.MinValue);
    WriteStop(Rule.MaxValue);

    AddColorNode(sdxXLSXNodeFillColor, Rule.Style.PositiveBarColor);
    AddColorNode(sdxXLSXNodeBorderColor, Rule.Style.PositiveBarBorderColor);
    AddColorNode(sdxXLSXNodeNegativeFillColor, Rule.Style.NegativeBarColor);
    AddColorNode(sdxXLSXNodeNegativeBorderColor, Rule.Style.NegativeBarBorderColor);
    if Rule.Style.AxisPosition <> dbapNone then
      AddColorNode(sdxXLSXNodeAxisColor, Rule.Style.AxisColor);
  finally
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueTypeDataBar;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder.WriteStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);

  function GetValueType: AnsiString;
  begin
    if AStop.ValueType <> cssvtLimitValue then
      Result := dxXLSXScaleStopValueTypeMap[AStop.ValueType]
    else
      if AStop = Rule.MinValue then
        Result := sdxXLSXValueTypeMin
      else
        Result := sdxXLSXValueTypeMax;
  end;

begin
  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeCFVO, '');
  try
    Writer.WriteAttributeString(sdxXLSXAttrTypeLC, GetValueType);
    if AStop.ValueType <> cssvtLimitValue then
    begin
      if NamePrefix <> '' then
        Writer.WriteElementString(sdxXLSXNodeXMFunc, AStop.Value)
      else
        Writer.WriteAttributeString(sdxXLSXAttrVal, AStop.Value);
    end;
  finally
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingDataBarRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleDataBar;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleDataBar(inherited Rule);
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.WriteChildren;
var
  I: Integer;
begin
  inherited;

  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeIconSet, '');
  try
    Writer.WriteAttributeString(sdxXLSXAttrIconSet, GetPresetName);
    Writer.WriteAttributeBoolean(sdxXLSXAttrShowValue, Rule.ShowValue);

    if Rule.PresetName = '' then
      Writer.WriteAttributeBoolean(sdxXLSXAttrCustom, True)
    else
      Writer.WriteAttributeBoolean(sdxXLSXAttrReverse, Rule.Order = isioReversed);

    for I := 0 to Rule.StopCount - 1 do
      WriteStop(Rule.Stops[I]);

    if Rule.PresetName = '' then
    begin
      for I := 0 to Rule.StopCount - 1 do
        WriteStopCustomIcon(Rule.Stops[I]);
    end;
  finally
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueTypeIconSet;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.WriteStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
begin
  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeCFVO, '');
  try
    Writer.WriteAttributeString(sdxXLSXAttrTypeLC, dxXLSXScaleStopValueTypeMap[AStop.ValueType]);
    if NamePrefix <> '' then
      Writer.WriteElementString(sdxXLSXNodeXMFunc, EncodeValue(AStop))
    else
      Writer.WriteAttributeString(sdxXLSXAttrVal, EncodeValue(AStop));
  finally
    Writer.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.WriteStopCustomIcon(
  AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
var
  AIndexInPreset: Integer;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
begin
  if not ConditionalFormattingIconSet.Presets.FindByIconIndex(AStop.IconIndex, APreset, AIndexInPreset) then
    raise EdxSpreadSheetFormatError.CreateFmt(cxGetResourceString(@sdxErrorInternal), [ClassName]);

  Writer.WriteStartElement(NamePrefix, sdxXLSXNodeCFIcon, '');
  try
    Writer.WriteAttributeString(sdxXLSXAttrIconSet, APreset.Name);
    Writer.WriteAttributeInteger(sdxXLSXAttrIconId, AIndexInPreset);
  finally
    Writer.WriteEndElement;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.EncodeValue(
  const AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop): string;
begin
  if VarIsNumeric(AStop.Value) then
    Result := dxCore.dxFloatToStr(AStop.Value)
  else
  begin
    Result := AStop.Value;
    if (AStop.ValueType = cssvtFormula) and dxSpreadSheetIsFormula(Result) then
      Delete(Result, 1, 1);
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.GetPresetName: string;
var
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
begin
  Result := Rule.PresetName;
  if Result = '' then
  begin
    if ConditionalFormattingIconSet.Presets.FindByCount(Rule.StopCount, APreset) then
      Result := APreset.Name
    else
      raise EdxSpreadSheetFormatError.CreateFmt(cxGetResourceString(@sdxErrorInternal), [ClassName]);
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingIconSetRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleIconSet;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleIconSet(inherited Rule);
end;

{ TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder }

procedure TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder.WriteAttributes;
begin
  inherited;
  Writer.WriteAttributeInteger(sdxXLSXAttrRank, Rule.Value);
  if Rule.Direction = tbvdBottom then
    Writer.WriteAttributeBoolean(sdxXLSXAttrBottom, True);
  if Rule.ValueType = tbvvtPercent then
    Writer.WriteAttributeBoolean(sdxXLSXAttrPercent, True);
end;

function TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder.GetRule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleTopBottomValues(inherited Rule);
end;

function TdxSpreadSheetXLSXWriterConditionalFormattingTopBottomRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueTop10;
end;

{ TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingUniqueValuesRuleBuilder }

function TdxSpreadSheetXLSXWriterWorksheetConditionalFormattingUniqueValuesRuleBuilder.GetRuleType: TdxXMLString;
begin
  Result := sdxXLSXValueUniqueValues;
end;

end.
