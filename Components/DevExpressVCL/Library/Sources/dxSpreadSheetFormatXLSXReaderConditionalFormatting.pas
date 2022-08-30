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

unit dxSpreadSheetFormatXLSXReaderConditionalFormatting;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections, Variants,
  dxCore, dxCoreClasses, cxClasses, dxCustomTree, dxXMLDoc, dxZIPUtils, dxGDIPlusClasses, dxCoreGraphics,
  cxGeometry, dxHashUtils,
  // SpreadSheet;
  dxSpreadSheetClasses,
  dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetContainers,
  dxSpreadSheetCore,
  dxSpreadSheetFormatXLSXReader,
  dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetPrinting,
  dxSpreadSheetStrs,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type
  TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParserClass = class of TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser;
  TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser = class;

  { TdxSpreadSheetXLSXReaderConditionalFormattingParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  strict private
    FConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    FRuleMap: TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule>;
    FRulePriorities: TList;
  protected
    procedure ProcessConditionalFormatting(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessConditionalFormattingEx(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessExtListItem(ANode: TdxXMLNode; AUserData: Pointer); virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXReaderWorksheetParser);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read FConditionalFormatting;
    property RuleMap: TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule> read FRuleMap;
    property RulePriorities: TList read FRulePriorities;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  strict private
    FAreas: TdxSpreadSheetAreaList;
    FConditionalFormattingParser: TdxSpreadSheetXLSXReaderConditionalFormattingParser;

    procedure ParseAreas(const S: string);
  protected
    FRuleParsers: TDictionary<string, TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParserClass>;

    procedure ProcessRule(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    function ReadReferences: string; virtual;
    procedure RegisterRuleParsers; virtual;
  public
    constructor Create(ANode: TdxXMLNode; AConditionalFormattingParser: TdxSpreadSheetXLSXReaderConditionalFormattingParser);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property Areas: TdxSpreadSheetAreaList read FAreas;
    property ConditionalFormattingParser: TdxSpreadSheetXLSXReaderConditionalFormattingParser read FConditionalFormattingParser;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser = class(TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser)
  protected
    function ReadReferences: string; override;
    procedure RegisterRuleParsers; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  strict private
    FAreaParser: TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser;

    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    function GetRuleMap: TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule>;
    function GetRulePriorities: TList;
  protected
    FRule: TdxSpreadSheetCustomConditionalFormattingRule;

    procedure CreateRule(AAreas: TdxSpreadSheetAreaList); virtual; abstract;
  public
    constructor Create(ANode: TdxXMLNode; AAreaParser: TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser);
    procedure Execute; override;
    //
    property AreaParser: TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser read FAreaParser;
    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read GetConditionalFormatting;
    property RuleMap: TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule> read GetRuleMap;
    property RulePriorities: TList read GetRulePriorities;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser)
  protected
    FRuleClass: TdxSpreadSheetConditionalFormattingRuleStyleBasedClass;

    procedure CreateRule(AAreas: TdxSpreadSheetAreaList); override;
    procedure ReadConditions(ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased); virtual;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser)
  protected
    procedure ReadConditions(ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased); override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser)
  protected
    procedure ReadConditions(ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased); override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser)
  protected
    procedure ReadConditions(ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased); override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser)
  protected
    procedure ParseStop(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; ANode: TdxXMLNode); overload;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser)
  strict private
    FStopsInfo: TList<TPair<TdxXMLNode, TdxXMLNode>>;
  protected
    procedure CreateRule(AAreas: TdxSpreadSheetAreaList); override;
    procedure PopulateStopsInfo;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingDuplicateValuesRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingDuplicateValuesRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser)
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser)
  protected
    procedure ReadConditions(ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased); override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingUniqueValuesRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingUniqueValuesRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser)
  public
    procedure AfterConstruction; override;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleIconSet;
  protected
    procedure CreateRule(AAreas: TdxSpreadSheetAreaList); override;
    procedure ParseIconSet(ANode: TdxXMLNode);
    procedure ParseIconSetCustomIcons(ANode: TdxXMLNode);
    procedure ParseIconSetStop(ANode: TdxXMLNode; AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
  public
    procedure Execute; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleIconSet read GetRule;
  end;

  { TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser }

  TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser = class(
    TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser)
  strict private
    function GetRule: TdxSpreadSheetConditionalFormattingRuleDataBar;
  protected
    procedure CreateRule(AAreas: TdxSpreadSheetAreaList); override;
    procedure ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
  public
    procedure Execute; override;
    //
    property Rule: TdxSpreadSheetConditionalFormattingRuleDataBar read GetRule;
  end;

implementation

uses
  AnsiStrings, Math, dxColorPicker, cxGraphics, dxSpreadSheetGraphics, dxSpreadSheetFormulas,
  dxSpreadSheetFormatXLSX, dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatUtils,
  dxSpreadSheetConditionalFormattingIconSet, dxTypeHelpers, dxSpreadSheetCoreStyles, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);

{ TdxSpreadSheetXLSXReaderConditionalFormattingParser }

constructor TdxSpreadSheetXLSXReaderConditionalFormattingParser.Create(AOwner: TdxSpreadSheetXLSXReaderWorksheetParser);
begin
  inherited Create(AOwner.Document.Root.First, AOwner.Owner);
  FConditionalFormatting := AOwner.View.ConditionalFormatting;
  FRuleMap := TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule>.Create;
  FRulePriorities := TList.Create;
end;

destructor TdxSpreadSheetXLSXReaderConditionalFormattingParser.Destroy;
begin
  FreeAndNil(FRulePriorities);
  FreeAndNil(FRuleMap);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingParser.Execute;
var
  ASubNode: TdxXMLNode;
  I: Integer;
begin
  Node.ForEach(ProcessConditionalFormatting);
  if Node.FindChild(sdxXLSXNodeExtList, ASubNode) then
    ASubNode.ForEach(ProcessExtListItem);

  RulePriorities.Pack;
  for I := 0 to RulePriorities.Count - 1 do
    TdxSpreadSheetCustomConditionalFormattingRule(RulePriorities.List[I]).Index := I;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingParser.ProcessConditionalFormatting(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxXLSXNodeConditionalFormatting then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.Create(ANode, Self));
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingParser.ProcessConditionalFormattingEx(ANode: TdxXMLNode; AUserData: Pointer);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser.Create(ANode, Self));
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingParser.ProcessExtListItem(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxXLSXNodeExt then
  begin
    if ANode.FindChild(sdxXLSXNamespaceX14 + ':' + sdxXLSXNodeConditionalFormattings, ANode) then
      ANode.ForEach(ProcessConditionalFormattingEx);
  end;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser }

constructor TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.Create(
  ANode: TdxXMLNode; AConditionalFormattingParser: TdxSpreadSheetXLSXReaderConditionalFormattingParser);
begin
  inherited Create(ANode, AConditionalFormattingParser.Owner);
  FAreas := TdxSpreadSheetAreaList.Create;
  FConditionalFormattingParser := AConditionalFormattingParser;
  FRuleParsers := TDictionary<string, TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParserClass>.Create;
  RegisterRuleParsers;
end;

destructor TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.Destroy;
begin
  FreeAndNil(FRuleParsers);
  FreeAndNil(FAreas);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.Execute;
begin
  ParseAreas(ReadReferences);
  Node.ForEach(ProcessRule);
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.ProcessRule(ANode: TdxXMLNode; AUserData: Pointer);
var
  AParserClass: TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParserClass;
begin
  if ANode.NameWithoutNameScope = sdxXLSXNodeConditionalFormattingRule then
  begin
    if FRuleParsers.TryGetValue(ANode.Attributes.GetValueAsString(sdxXLSXAttrTypeLC), AParserClass) then
      ExecuteSubTask(AParserClass.Create(ANode, Self));
  end;
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.ReadReferences: string;
begin
  Result := Node.Attributes.GetValueAsString(sdxXLSXAttrSqRef);
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.RegisterRuleParsers;
begin
  FRuleParsers.Add(sdxXLSXValueAboveAverage, TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser);
  FRuleParsers.Add(sdxXLSXValueCellIs, TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser);
  FRuleParsers.Add(sdxXLSXValueColorScale, TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser);
  FRuleParsers.Add(sdxXLSXValueExpression, TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add(sdxXLSXValueTop10, TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser);
  FRuleParsers.Add(sdxXLSXValueTypeDataBar, TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser);
  FRuleParsers.Add(sdxXLSXValueTypeIconSet, TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser);

  FRuleParsers.Add('beginsWith', TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add('containsText', TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add('duplicateValues', TdxSpreadSheetXLSXReaderConditionalFormattingDuplicateValuesRuleParser);
  FRuleParsers.Add('endsWith', TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add('notContainsBlanks', TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add('notContainsErrors', TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser);
  FRuleParsers.Add('uniqueValues', TdxSpreadSheetXLSXReaderConditionalFormattingUniqueValuesRuleParser);
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser.ParseAreas(const S: string);
var
  AArea: TRect;
  AList: TStringList;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    AList.Text := StringReplace(S, ' ', dxCRLF, [rfReplaceAll]);

    FAreas.Clear;
    FAreas.Capacity := AList.Count;
    for I := 0 to AList.Count - 1 do
    begin
      AArea := dxStringToReferenceArea(StringReplace(AList[I], '$', '', [rfReplaceAll]));
      if dxSpreadSheetIsValidArea(AArea) then
        FAreas.Add(AArea);
    end;
  finally
    AList.Free;
  end;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser }

function TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser.ReadReferences: string;
var
  ANode: TdxXMLNode;
begin
  if Node.FindChild(sdxXLSXNodeXMSqRef, ANode) then
    Result := ANode.TextAsString
  else
    Result := '';
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAreaExParser.RegisterRuleParsers;
begin
  FRuleParsers.Add(sdxXLSXValueTypeDataBar, TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser);
  FRuleParsers.Add(sdxXLSXValueTypeIconSet, TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser);
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser }

constructor TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser.Create(
  ANode: TdxXMLNode; AAreaParser: TdxSpreadSheetXLSXReaderConditionalFormattingAreaParser);
begin
  inherited Create(ANode, AAreaParser.Owner);
  FAreaParser := AAreaParser;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser.Execute;
var
  APriority: Integer;
  ASubNode: TdxXMLNode;
begin
  CreateRule(AreaParser.Areas);
  if FRule <> nil then
  begin
    APriority := Node.Attributes.GetValueAsInteger(sdxXLSXAttrPriority) - 1;
    if APriority >= 0 then
    begin
      RulePriorities.Count := Max(RulePriorities.Count, APriority + 1);
      RulePriorities.Items[APriority] := FRule;
    end;
    if Node.FindChild([sdxXLSXNodeExtList, sdxXLSXNodeExt, sdxXLSXNodeX14ID], ASubNode) then
      RuleMap.AddOrSetValue(ASubNode.TextAsString, FRule);
  end;
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := AreaParser.ConditionalFormattingParser.ConditionalFormatting;
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser.GetRuleMap: TDictionary<string, TdxSpreadSheetCustomConditionalFormattingRule>;
begin
  Result := AreaParser.ConditionalFormattingParser.RuleMap;
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingCustomRuleParser.GetRulePriorities: TList;
begin
  Result := AreaParser.ConditionalFormattingParser.RulePriorities;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser.CreateRule(AAreas: TdxSpreadSheetAreaList);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased;
  AStyleIndex: Integer;
begin
  ARule := FRuleClass.Create(ConditionalFormatting);
  try
    ARule.StopIfTrue := Node.Attributes.GetValueAsBoolean(sdxXLSXAttrStopIfTrue);
    if Node.Attributes.Exists(sdxXLSXAttrDfxID) then
    begin
      AStyleIndex := Node.Attributes.GetValueAsInteger(sdxXLSXAttrDfxID);
      if CheckListIndex(AStyleIndex, Owner.ConditionalFormattingStyles, sdxErrorInvalidStyleIndex, ssmtError) then
        ARule.Style.Handle := TdxSpreadSheetCellStyleHandle(Owner.ConditionalFormattingStyles[AStyleIndex]);
    end;
    ARule.Areas := AAreas;
    ReadConditions(ARule);
  except
    FreeAndNil(ARule);
    raise;
  end;
  FRule := ARule;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingStyleBasedRuleParser.ReadConditions(
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);
begin
  // do nothing
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingAboveAverageRuleParser.ReadConditions(
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);

  function DecodeComparisonOperator(AIsAbove, AIsEquals: Boolean): TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator;
  const
    Map: array[Boolean, Boolean] of TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator = (
      (abacoBelowAverage, abacoAboveAverage), (abacoBelowOrEqualAverage, abacoAboveOrEqualAverage)
    );
  begin
    if Node.Attributes.Exists(sdxXLSXAttrStdDev) then
    begin
      if AIsAbove then
        Result := abacoAboveAverageOnStandardDeviation
      else
        Result := abacoBelowAverageOnStandardDeviation
    end
    else
      Result := Map[AIsEquals, AIsAbove];
  end;

var
  AAverageRule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
begin
  AAverageRule := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(ARule);
  AAverageRule.ComparisonOperator := DecodeComparisonOperator(
    Node.Attributes.GetValueAsBoolean(sdxXLSXAttrAboveAverage, True),
    Node.Attributes.GetValueAsBoolean(sdxXLSXAttrEqualAverage, False));
  AAverageRule.StandardDeviationLevel := Node.Attributes.GetValueAsInteger(sdxXLSXAttrStdDev, 1);
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleExpression;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingExpressionRuleParser.ReadConditions(
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);
begin
  inherited ReadConditions(ARule);
  if Node.HasChildren then
    TdxSpreadSheetConditionalFormattingRuleExpression(ARule).Expression := Node.First.TextAsString;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleCellIs;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingCellIsRuleParser.ReadConditions(
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);
begin
  inherited ReadConditions(ARule);
  TdxSpreadSheetConditionalFormattingRuleCellIs(ARule).ComparisonOperator :=
    TdxSpreadSheetXLSXHelper.StringToCfCellIsRuleOperator(Node.Attributes.GetValue(sdxXLSXAttrOperator));
  if Node.Count > 1 then
    TdxSpreadSheetConditionalFormattingRuleCellIs(ARule).Expression2 := Node[1].TextAsString;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingCustomScaleRuleParser.ParseStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; ANode: TdxXMLNode);

  function DecodeValue(const S: string): Variant;
  begin
    case AStop.ValueType of
      cssvtValue:
        Result := dxStrToFloat(S);
      cssvtFormula:
        Result := S;
      cssvtPercent, cssvtPercentile:
        Result := StrToIntDef(S, 0);
    else
      Result := Null;
    end;
  end;

var
  ASubNode: TdxXMLNode;
begin
  AStop.ValueType := TdxSpreadSheetXLSXHelper.StringToColorScaleStopValueType(ANode.Attributes.GetValue(sdxXLSXAttrTypeLC));
  if ANode.FindChild(sdxXLSXNodeXMFunc, ASubNode) then
    AStop.Value := DecodeValue(ASubNode.TextAsString)
  else
    AStop.Value := DecodeValue(ANode.Attributes.GetValueAsString(sdxXLSXAttrVal));
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FStopsInfo := TList<TPair<TdxXMLNode, TdxXMLNode>>.Create;
end;

destructor TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser.Destroy;
begin
  FreeAndNil(FStopsInfo);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser.Execute;
var
  APair: TPair<TdxXMLNode, TdxXMLNode>;
  ARule: TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess;
  AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
  I: Integer;
begin
  PopulateStopsInfo;
  inherited Execute;

  if FRule <> nil then
  begin
    ARule := TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess(FRule);
    ARule.BeginUpdate;
    try
      for I := 0 to FStopsInfo.Count - 1 do
      begin
        AStop := ARule.Stops[I];
        APair := FStopsInfo[I];
        ParseStop(AStop, APair.Key);
        if APair.Value <> nil then
          AStop.Color := DecodeColor(APair.Value);
      end;
    finally
      ARule.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser.CreateRule(AAreas: TdxSpreadSheetAreaList);
begin
  case FStopsInfo.Count of
    2: FRule := TdxSpreadSheetConditionalFormattingRuleTwoColorScale.Create(ConditionalFormatting);
    3: FRule := TdxSpreadSheetConditionalFormattingRuleThreeColorScale.Create(ConditionalFormatting);
  end;
  if FRule <> nil then
    FRule.Areas := AAreas;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingColorScaleRuleParser.PopulateStopsInfo;
var
  AIndex: Integer;
  APair: TPair<TdxXMLNode, TdxXMLNode>;
  AStopInfoIndex: Integer;
  ASubNode: TdxXMLNode;
begin
  if Node.FindChild(sdxXLSXNodeColorScale, ASubNode) then
  begin
    AIndex := 0;
    while (AIndex < ASubNode.Count) and (ASubNode[AIndex].Name = sdxXLSXNodeCFVO) do
    begin
      FStopsInfo.Add(TPair<TdxXMLNode, TdxXMLNode>.Create(ASubNode[AIndex], nil));
      Inc(AIndex);
    end;

    while (AIndex < ASubNode.Count) and (ASubNode[AIndex].Name = sdxXLSXNodeColor) do
    begin
      AStopInfoIndex := AIndex - FStopsInfo.Count;
      if AStopInfoIndex >= FStopsInfo.Count then
        Break;
      APair := FStopsInfo.Items[AStopInfoIndex];
      APair.Value := ASubNode[AIndex];
      FStopsInfo.Items[AStopInfoIndex] := APair;
      Inc(AIndex);
    end;
  end;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingDuplicateValuesRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingDuplicateValuesRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleDuplicateValues;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingTopBottomRuleParser.ReadConditions(
  ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);
var
  ATopBottomRule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
begin
  inherited ReadConditions(ARule);

  ATopBottomRule := TdxSpreadSheetConditionalFormattingRuleTopBottomValues(ARule);
  ATopBottomRule.Value := Node.Attributes.GetValueAsInteger(sdxXLSXAttrRank, 10);

  if Node.Attributes.GetValueAsBoolean(sdxXLSXAttrBottom) then
    ATopBottomRule.Direction := tbvdBottom
  else
    ATopBottomRule.Direction := tbvdTop;

  if Node.Attributes.GetValueAsBoolean(sdxXLSXAttrPercent) then
    ATopBottomRule.ValueType := tbvvtPercent
  else
    ATopBottomRule.ValueType := tbvvtRank;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingUniqueValuesRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingUniqueValuesRuleParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FRuleClass := TdxSpreadSheetConditionalFormattingRuleUniqueValues;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.CreateRule(AAreas: TdxSpreadSheetAreaList);
begin
  FRule := TdxSpreadSheetConditionalFormattingRuleIconSet.Create(ConditionalFormatting);
  FRule.Areas := AAreas;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.Execute;
begin
  inherited Execute;
  if Node.Count > 0 then
    ParseIconSet(Node.First);
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.GetRule: TdxSpreadSheetConditionalFormattingRuleIconSet;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleIconSet(FRule);
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.ParseIconSet(ANode: TdxXMLNode);
const
  OrderMap: array[Boolean] of TdxSpreadSheetConditionalFormattingRuleIconSetOrder = (isioNormal, isioReversed);
var
  I: Integer;
begin
  Rule.BeginUpdate;
  try
    Rule.PresetName := dxXMLStringToString(ANode.Attributes.GetValue(sdxXLSXAttrIconSet, '3TrafficLights1'));
    Rule.ShowValue := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrShowValue, True);
    Rule.Order := OrderMap[ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrReverse)];
    for I := 0 to Rule.StopCount - 1 do
      ParseIconSetStop(ANode[I], Rule.Stops[I]);
    if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCustom) then
      ParseIconSetCustomIcons(ANode);
  finally
    Rule.EndUpdate;
  end;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.ParseIconSetCustomIcons(ANode: TdxXMLNode);
var
  AIndex: Integer;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  AStopIndex: Integer;
  ASubNode: TdxXMLNode;
begin
  AIndex := 0;
  while (AIndex < ANode.Count) and (ANode[AIndex].NameWithoutNameScope <> sdxXLSXNodeCFIcon) do
    Inc(AIndex);

  AStopIndex := 0;
  while (AIndex < ANode.Count) and (AStopIndex < Rule.StopCount) do
  begin
    ASubNode := ANode[AIndex];
    if ASubNode.NameWithoutNameScope <> sdxXLSXNodeCFIcon then
      Break;
    if ConditionalFormattingIconSet.Presets.FindByName(ASubNode.Attributes.GetValueAsString(sdxXLSXAttrIconSet), APreset) then
      Rule.Stops[AStopIndex].IconIndex := APreset.IconIndexes[ASubNode.Attributes.GetValueAsInteger(sdxXLSXAttrIconId)];
    Inc(AStopIndex);
    Inc(AIndex);
  end;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingIconSetRuleParser.ParseIconSetStop(
  ANode: TdxXMLNode; AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
begin
  if ANode.NameWithoutNameScope = sdxXLSXNodeCFVO then
  begin
    ParseStop(AStop, ANode);
    if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrGTE, True) then
      AStop.ComparisonOperator := isscoGreaterThanOrEqual
    else
      AStop.ComparisonOperator := isscoGreaterThan;
  end;
end;

{ TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser }

procedure TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser.CreateRule(AAreas: TdxSpreadSheetAreaList);
begin
  if not RuleMap.TryGetValue(Node.Attributes.GetValueAsString(sdxXLSXAttrIdLC), FRule) then
  begin
    FRule := TdxSpreadSheetConditionalFormattingRuleDataBar.Create(ConditionalFormatting);
    FRule.Areas := AAreas;
  end;
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser.Execute;
var
  ASubNode: TdxXMLNode;
begin
  inherited Execute;
  if Node.Count > 0 then
  begin
    ASubNode := Node.First;
    ASubNode.ForEach(ProcessSubNode);

    Rule.ShowValue := ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrShowValue, True);

    if ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrNegativeBarColorSameAsPositive) then
      Rule.Style.NegativeBarColor := clDefault;

    if ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrNegativeBarBorderColorSameAsPositive, True) then
      Rule.Style.NegativeBarBorderColor := clDefault;

    if not ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrBorder) then
    begin
      Rule.Style.NegativeBarBorderColor := clDefault;
      Rule.Style.PositiveBarBorderColor := clNone;
    end;

    if ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrGradient, True) then
      Rule.Style.FillMode := dbfmGradient
    else
      Rule.Style.FillMode := dbfmSolid;

    Rule.Style.AxisPosition := TdxSpreadSheetXLSXHelper.StringToAxisPosition(ASubNode.Attributes.GetValue(sdxXLSXAttrAxisPosition));
    Rule.Style.Direction := TdxSpreadSheetXLSXHelper.StringToDataBarDirection(ASubNode.Attributes.GetValue(sdxXLSXAttrDirection));
  end;
end;

function TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser.GetRule: TdxSpreadSheetConditionalFormattingRuleDataBar;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleDataBar(FRule);
end;

procedure TdxSpreadSheetXLSXReaderConditionalFormattingDataBarParser.ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.NameWithoutNameScope = sdxXLSXNodeCFVO then
  begin
    if ANode.Index = 0 then
      ParseStop(Rule.MinValue, ANode)
    else
      ParseStop(Rule.MaxValue, ANode);
  end
  else
  if ANode.NameWithoutNameScope = sdxXLSXNodeBorderColor then
    Rule.Style.PositiveBarBorderColor := DecodeColor(ANode)
  else

  if ANode.NameWithoutNameScope = sdxXLSXNodeNegativeBorderColor then
    Rule.Style.NegativeBarBorderColor := DecodeColor(ANode)
  else

  if ANode.NameWithoutNameScope = sdxXLSXNodeNegativeFillColor then
    Rule.Style.NegativeBarColor := DecodeColor(ANode)
  else

  if ANode.NameWithoutNameScope = sdxXLSXNodeAxisColor then
    Rule.Style.AxisColor := DecodeColor(ANode)
  else

  if (ANode.NameWithoutNameScope = sdxXLSXNodeColor) or
     (ANode.NameWithoutNameScope = sdxXLSXNodeFillColor)
  then
    Rule.Style.PositiveBarColor := DecodeColor(ANode)
end;

end.
