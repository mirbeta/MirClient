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

unit dxSpreadSheetConditionalFormatting;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections, Forms,
  dxCore, cxGraphics, cxClasses, cxControls, dxCoreClasses, cxVariants, cxStorage,
  cxLookAndFeels, cxGeometry,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetClasses,
  dxSpreadSheetGraphics,
  dxSpreadSheetStyles,
  dxSpreadSheetTypes;

type
  TdxSpreadSheetCustomConditionalFormatting = class;
  TdxSpreadSheetCustomConditionalFormattingRule = class;
  TdxSpreadSheetCustomConditionalFormattingRuleClass = class of TdxSpreadSheetCustomConditionalFormattingRule;
  TdxSpreadSheetConditionalFormattingAreaInfo = class;

  { IdxSpreadSheetConditionalFormatting }

  IdxSpreadSheetConditionalFormatting = interface
  ['{5E4679AF-EE22-4657-A0C0-F774E2515263}']
    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
  end;

  { IdxSpreadSheetConditionalFormattingOwner }

  IdxSpreadSheetConditionalFormattingOwner = interface(IdxSpreadSheetViewData)
  ['{E81C6304-414A-4A81-B254-4A1AFE7562BB}']
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetFormulaController: TdxSpreadSheetCustomFormulaController;

    procedure BeginUpdate;
    procedure EndUpdate;

    function GetSelectionArea: TRect;
    function IsRightToLeft: Boolean;

    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property FormulaController: TdxSpreadSheetCustomFormulaController read GetFormulaController;
  end;

  { TdxSpreadSheetCustomConditionalFormatting }

  TdxSpreadSheetCustomConditionalFormatting = class abstract
  strict private
    function GetCellStyles: TdxSpreadSheetCellStyles;
    function GetRule(Index: Integer): TdxSpreadSheetCustomConditionalFormattingRule; inline;
    function GetRuleCount: Integer;
    procedure RulesChangeHandler(Sender: TObject;
      const AItem: TdxSpreadSheetCustomConditionalFormattingRule; AAction: TCollectionNotification);
  protected
    FAreaInfoCache: TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>;
    FOwner: IdxSpreadSheetConditionalFormattingOwner;
    FRules: TObjectList<TdxSpreadSheetCustomConditionalFormattingRule>;

    function CreateAreaInfo(AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetConditionalFormattingAreaInfo; virtual;
    function GetAreaInfo(AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetConditionalFormattingAreaInfo;

    function CanValidateExpressionRuleResultValue: Boolean; virtual;
    function IsStyleBorderSupported: Boolean; virtual;
    function IsValueFormattingSupported: Boolean; virtual;
    function GetFormulaEditMask: string; virtual;

    procedure DoChanged; virtual;
    procedure DoRuleAdded(ARule: TdxSpreadSheetCustomConditionalFormattingRule); virtual;
    procedure DoRuleChanging(ARule: TdxSpreadSheetCustomConditionalFormattingRule); virtual;
    procedure DoRuleIndexChanged(AOldIndex, ANewIndex: Integer); virtual;
    procedure DoRuleDeleted(ARule: TdxSpreadSheetCustomConditionalFormattingRule); virtual;
  public
    constructor Create(const AOwner: IdxSpreadSheetConditionalFormattingOwner); virtual;
    destructor Destroy; override;
    procedure Add(ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;
    procedure Add(const AArea: TRect; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule); overload;
    procedure Assign(ASource: TdxSpreadSheetCustomConditionalFormatting); virtual;
    procedure Remove(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
    function CalculateStyle(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; ACell: IdxSpreadSheetCellData): Boolean;
    procedure Clear;
    procedure FlushCache; virtual;
    //
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    procedure BeginEditing; virtual;
    procedure EndEditing(ACancel: Boolean); virtual;

    // Utilities
    function ReferencesToString(const AAreas: TdxSpreadSheetAreaList): string; virtual;

    property CellStyles: TdxSpreadSheetCellStyles read GetCellStyles;
    property RuleCount: Integer read GetRuleCount;
    property Rules[Index: Integer]: TdxSpreadSheetCustomConditionalFormattingRule read GetRule; default;
    property Owner: IdxSpreadSheetConditionalFormattingOwner read FOwner;
  end;

  { TdxSpreadSheetCustomConditionalFormattingRule }

  TdxSpreadSheetCustomConditionalFormattingRule = class(TcxInterfacedPersistent,
    IcxStoredObject)
  strict private
    FAreas: TdxSpreadSheetAreaList;
    FLockCount: Integer;
    FOwner: TdxSpreadSheetCustomConditionalFormatting;
    FStopIfTrue: Boolean;

    procedure AreasChangeHandler(Sender: TObject);
    function GetArea: TRect;
    function GetIndex: Integer;
    procedure SetArea(const AValue: TRect);
    procedure SetAreas(AAreas: TdxSpreadSheetAreaList);
    procedure SetIndex(AValue: Integer);
    procedure SetStopIfTrue(const AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); virtual;
    procedure DoLoadFromStream(AReader: TcxReader); virtual;
    procedure DoSaveToStream(AWriter: TcxWriter); virtual;

    procedure Apply(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil); virtual; abstract;
    function CanApply(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; virtual; abstract;

    procedure DrawPreview(ACanvas: TcxCanvas; R: TRect); virtual; abstract;
    procedure FlushCache; virtual;
    function GetDetails: string; virtual; abstract;
    function ApplyToTheRowSupported: Boolean; virtual;

  {$REGION 'IcxStoredObject'}
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
  {$ENDREGION}

    property StopIfTrue: Boolean read FStopIfTrue write SetStopIfTrue;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); reintroduce; overload; virtual;
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting; const AArea: TRect); reintroduce; overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    function Clone: TdxSpreadSheetCustomConditionalFormattingRule; overload;
    function Clone(const AArea: TRect): TdxSpreadSheetCustomConditionalFormattingRule; overload;
    function Clone(const AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetCustomConditionalFormattingRule; overload;
    //
    procedure LoadFromStream(AReader: TcxReader);
    procedure SaveToStream(AWriter: TcxWriter);
    //
    property Area: TRect read GetArea write SetArea; // obsolete
    property Areas: TdxSpreadSheetAreaList read FAreas write SetAreas;
    property Index: Integer read GetIndex write SetIndex;
    //
    property Owner: TdxSpreadSheetCustomConditionalFormatting read FOwner;
  end;

  { TdxSpreadSheetConditionalFormattingAreaInfo }

  TdxSpreadSheetConditionalFormattingDataCacheAssignedValue = (dcavMinMax, dcavAverage, dcavNumericValueCount, dcavStandardDeviation);
  TdxSpreadSheetConditionalFormattingDataCacheAssignedValues = set of TdxSpreadSheetConditionalFormattingDataCacheAssignedValue;

  TdxSpreadSheetConditionalFormattingAreaInfo = class
  strict private
    FAreas: TdxSpreadSheetAreaList;
    FAssignedValues: TdxSpreadSheetConditionalFormattingDataCacheAssignedValues;
    FAverageValue: Double;
    FBottomValues: TList<Double>;
    FBottomValuesRank: Integer;
    FDuplicates: TdxVariantList;
    FMaxValue: Double;
    FMinValue: Double;
    FNumericValueCount: Integer;
    FOwner: IdxSpreadSheetConditionalFormattingOwner;
    FPercentiles: TDictionary<Integer, Double>;
    FStandardDeviation: Double;
    FTopValues: TList<Double>;
    FTopValuesRank: Integer;

    procedure CalculateAverageValue;
    function CalculateBoundValueForPercentile(APercentile: Integer; out AValue: Double): Boolean;
    procedure CalculateMinMaxValues;
    procedure CalculateNumericValueCount;
    procedure CalculateStandardDeviation;
    procedure CalculateTopBottomValues;
    function CheckTopBottomValues(var AList: TList<Double>; var AListRank: Integer; ATargetRank: Integer): Boolean;
    procedure EnumCells(const AProc: TdxSpreadSheetViewForEachCellProc);
    function GetSortedNumericValuesInArea(out AValues: TList<Double>): Boolean;
    procedure PopulateDuplicateValues;

    function GetAverageValue: Double;
    function GetMaxValue: Double;
    function GetMinValue: Double;
    function GetNumericValueCount: Integer;
    function GetStandardDeviation: Double;
  public
    constructor Create(const AOwner: IdxSpreadSheetConditionalFormattingOwner; AAreas: TdxSpreadSheetAreaList);
    destructor Destroy; override;
    function GetLimitValueForPercent(APercent: Integer): Double;
    function GetLimitValueForPercentile(APercentile: Integer): Double;
    function IsBottomValue(const AValue: Double; ARank: Integer): Boolean;
    function IsTopValue(const AValue: Double; ARank: Integer): Boolean;
    function IsUniqueValue(const AValue: Variant): Boolean;

    property AverageValue: Double read GetAverageValue;
    property MaxValue: Double read GetMaxValue;
    property MinValue: Double read GetMinValue;
    property NumericValueCount: Integer read GetNumericValueCount;
    property StandardDeviation: Double read GetStandardDeviation;
    //
    property Areas: TdxSpreadSheetAreaList read FAreas;
  end;

  { TdxSpreadSheetConditionalFormattingCustomRule }

  TdxSpreadSheetConditionalFormattingCustomRuleClass = class of TdxSpreadSheetConditionalFormattingCustomRule;
  TdxSpreadSheetConditionalFormattingCustomRule = class(TdxSpreadSheetCustomConditionalFormattingRule)
  strict private
    function GetAreaInfo: TdxSpreadSheetConditionalFormattingAreaInfo;
  protected
    property AreaInfo: TdxSpreadSheetConditionalFormattingAreaInfo read GetAreaInfo;
  end;

  { TdxSpreadSheetConditionalFormattingStyleViewInfo }

  TdxSpreadSheetConditionalFormattingStyleViewInfo = class
  strict private
    FBackgroundColor: TColor;
    FDataBarAxisBounds: TRect;
    FDataBarBounds: TRect;
    FIconBounds: TRect;
    FScaleFactor: TdxScaleFactor;
    FStyle: TdxSpreadSheetCellDisplayStyle;
    FTextRect: TRect;

    function GetDataBar: TdxSpreadSheetCellDataBar; inline;
  protected
    procedure CalculateDataBarBounds(const R: TRect); virtual;
    procedure CalculateIconBounds(var R: TRect); virtual;
  public
    constructor Create(AStyle: TdxSpreadSheetCellDisplayStyle; AScaleFactor: TdxScaleFactor); virtual;
    procedure Calculate(const ABounds: TRect); overload;
    procedure Calculate(const ABounds, AContentBounds: TRect); overload; virtual;
    procedure Draw(ACanvas: TcxCanvas); overload;
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); overload; virtual;
    procedure Offset(const AOffset: TPoint); virtual;

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property DataBar: TdxSpreadSheetCellDataBar read GetDataBar;
    property DataBarAxisBounds: TRect read FDataBarAxisBounds;
    property DataBarBounds: TRect read FDataBarBounds;
    property IconBounds: TRect read FIconBounds;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property Style: TdxSpreadSheetCellDisplayStyle read FStyle;
    property TextRect: TRect read FTextRect;
  end;

implementation

uses
  SysUtils, RTLConsts, Math, Variants,
  dxTypeHelpers, cxDrawTextUtils, dxGDIPlusClasses, dxGDIPlusAPI,
  dxSpreadSheetUtils,
  dxSpreadSheetNumberFormat,
  dxSpreadSheetConditionalFormattingIconSet,
  dxSpreadSheetCoreStrs;

{ TdxSpreadSheetCustomConditionalFormatting }

constructor TdxSpreadSheetCustomConditionalFormatting.Create(const AOwner: IdxSpreadSheetConditionalFormattingOwner);
begin
  inherited Create;
  FOwner := AOwner;
  FRules := TObjectList<TdxSpreadSheetCustomConditionalFormattingRule>.Create;
  FRules.OnNotify := RulesChangeHandler;
  FAreaInfoCache := TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>.Create([doOwnsValues]);
end;

destructor TdxSpreadSheetCustomConditionalFormatting.Destroy;
begin
  FreeAndNil(FAreaInfoCache);
  FreeAndNil(FRules);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.Add(
  ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
begin
  TdxSpreadSheetCustomConditionalFormattingRule(ARule) := ARuleClass.Create(Self);
end;

procedure TdxSpreadSheetCustomConditionalFormatting.Add(
  const AArea: TRect; ARuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass; out ARule);
begin
  TdxSpreadSheetCustomConditionalFormattingRule(ARule) := ARuleClass.Create(Self, AArea);
end;

procedure TdxSpreadSheetCustomConditionalFormatting.Assign(ASource: TdxSpreadSheetCustomConditionalFormatting);
var
  ASourceRule: TdxSpreadSheetCustomConditionalFormattingRule;
  ATargetRule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  BeginEditing;
  try
    Clear;
    for I := 0 to ASource.RuleCount - 1 do
    begin
      ASourceRule := ASource.Rules[I];
      Add(TdxSpreadSheetConditionalFormattingCustomRuleClass(ASourceRule.ClassType), ATargetRule);
      ATargetRule.Assign(ASourceRule);
    end;
  finally
    EndEditing(False);
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.Remove(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  ARule.Free;
end;

function TdxSpreadSheetCustomConditionalFormatting.CalculateStyle(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; ACell: IdxSpreadSheetCellData): Boolean;
var
  AList: TList;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  Result := False;
  if RuleCount > 0 then
  begin
    AStyle.BeginUpdate;
    try
      AList := TList.Create;
      try
        AList.Capacity := RuleCount;
        for I := 0 to RuleCount - 1 do
        begin
          ARule := Rules[I];
          if ARule.CanApply(ARow, AColumn, ACell) then
          begin
            Result := True;
            AList.Add(ARule);
            if ARule.StopIfTrue then
              Break;
          end;
        end;

        for I := AList.Count - 1 downto 0 do
          TdxSpreadSheetCustomConditionalFormattingRule(AList[I]).Apply(AStyle, ARow, AColumn, ACell);
      finally
        AList.Free;
      end;
    finally
      AStyle.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.Clear;
var
  I: Integer;
begin
  BeginEditing;
  try
    for I := FRules.Count - 1 downto 0 do
      FRules[I].Free;
    DoChanged;
  finally
    EndEditing(False);
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.BeginUpdate;
begin
  Owner.BeginUpdate;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.EndUpdate;
begin
  Owner.EndUpdate;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.BeginEditing;
begin
  BeginUpdate;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.EndEditing(ACancel: Boolean);
begin
  EndUpdate;
end;

function TdxSpreadSheetCustomConditionalFormatting.ReferencesToString(const AAreas: TdxSpreadSheetAreaList): string;
begin
  Result := AAreas.ToString;
end;

function TdxSpreadSheetCustomConditionalFormatting.CreateAreaInfo(
  AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetConditionalFormattingAreaInfo;
begin
  Result := TdxSpreadSheetConditionalFormattingAreaInfo.Create(Owner, AAreas);
end;

function TdxSpreadSheetCustomConditionalFormatting.GetAreaInfo(
  AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetConditionalFormattingAreaInfo;
begin
  if not FAreaInfoCache.TryGetValue(AAreas.ToString, Result) then
  begin
    Result := CreateAreaInfo(AAreas);
    FAreaInfoCache.Add(AAreas.ToString, Result);
  end;
end;

function TdxSpreadSheetCustomConditionalFormatting.CanValidateExpressionRuleResultValue: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetCustomConditionalFormatting.IsStyleBorderSupported: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetCustomConditionalFormatting.IsValueFormattingSupported: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetCustomConditionalFormatting.GetFormulaEditMask: string;
begin
  Result := '';
end;

procedure TdxSpreadSheetCustomConditionalFormatting.DoRuleAdded(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  FRules.Add(ARule);
end;

procedure TdxSpreadSheetCustomConditionalFormatting.DoRuleChanging(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
// do nothing
end;

procedure TdxSpreadSheetCustomConditionalFormatting.DoRuleIndexChanged(AOldIndex, ANewIndex: Integer);
begin
// do nothing
end;

procedure TdxSpreadSheetCustomConditionalFormatting.DoRuleDeleted(ARule: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  if FRules <> nil then
    FRules.Extract(ARule);
end;

procedure TdxSpreadSheetCustomConditionalFormatting.DoChanged;
begin
// do nothing
end;

procedure TdxSpreadSheetCustomConditionalFormatting.FlushCache;
var
  I: Integer;
begin
  FAreaInfoCache.Clear;
  for I := 0 to RuleCount - 1 do
    Rules[I].FlushCache;
end;

function TdxSpreadSheetCustomConditionalFormatting.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := Owner.CellStyles;
end;

function TdxSpreadSheetCustomConditionalFormatting.GetRule(
  Index: Integer): TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := FRules[Index];
end;

function TdxSpreadSheetCustomConditionalFormatting.GetRuleCount: Integer;
begin
  Result := FRules.Count;
end;

procedure TdxSpreadSheetCustomConditionalFormatting.RulesChangeHandler(Sender: TObject;
  const AItem: TdxSpreadSheetCustomConditionalFormattingRule; AAction: TCollectionNotification);
begin
  DoChanged;
end;

{ TdxSpreadSheetCustomConditionalFormattingRule }

constructor TdxSpreadSheetCustomConditionalFormattingRule.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FAreas := TdxSpreadSheetAreaList.Create;
end;

constructor TdxSpreadSheetCustomConditionalFormattingRule.Create(
  AOwner: TdxSpreadSheetCustomConditionalFormatting; const AArea: TRect);
begin
  Create(AOwner);
  if not cxRectIsNull(AArea) then
    FAreas.Add(AArea);
end;

destructor TdxSpreadSheetCustomConditionalFormattingRule.Destroy;
begin
  FreeAndNil(FAreas);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.AfterConstruction;
begin
  inherited AfterConstruction;
  FAreas.OnChange := AreasChangeHandler;
  FOwner.DoRuleAdded(Self);
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.Assign(Source: TPersistent);
begin
  if (Source is TdxSpreadSheetCustomConditionalFormattingRule) and (Source <> Self) then
  begin
    BeginUpdate;
    try
      DoAssign(TdxSpreadSheetCustomConditionalFormattingRule(Source));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.BeforeDestruction;
begin
  FOwner.DoRuleDeleted(Self);
  inherited BeforeDestruction;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.BeginUpdate;
begin
  Owner.BeginUpdate;
  Inc(FLockCount);
  if FLockCount = 1 then
    Owner.DoRuleChanging(Self);
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.EndUpdate;
begin
  Dec(FLockCount);
  Owner.EndUpdate;
end;

function TdxSpreadSheetCustomConditionalFormattingRule.Clone: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := Clone(Areas);
end;

function TdxSpreadSheetCustomConditionalFormattingRule.Clone(const AArea: TRect): TdxSpreadSheetCustomConditionalFormattingRule;
var
  AList: TdxSpreadSheetAreaList;
begin
  AList := TdxSpreadSheetAreaList.Create;
  try
    AList.Add(AArea);
    Result := Clone(AList);
  finally
    AList.Free;
  end;
end;

function TdxSpreadSheetCustomConditionalFormattingRule.Clone(
  const AAreas: TdxSpreadSheetAreaList): TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := TdxSpreadSheetCustomConditionalFormattingRuleClass(ClassType).Create(Owner);
  Result.BeginUpdate;
  try
    Result.Assign(Self);
    Result.Areas := AAreas;
    Result.Index := Index + 1;
  finally
    Result.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.LoadFromStream(AReader: TcxReader);
begin
  BeginUpdate;
  try
    DoLoadFromStream(AReader);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SaveToStream(AWriter: TcxWriter);
begin
  DoSaveToStream(AWriter);
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.Changed;
begin
  Owner.DoChanged;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.Changing;
begin
  Owner.DoRuleChanging(Self);
end;

function TdxSpreadSheetCustomConditionalFormattingRule.CanApply(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  try
    Result := Areas.Contains(ARow, AColumn) and CanApplyCore(ARow, AColumn, ACell);
  except
    Result := False;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  Areas := Source.Areas;
  StopIfTrue := Source.StopIfTrue;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.DoLoadFromStream(AReader: TcxReader);
var
  ACount: Integer;
  I: Integer;
begin
  if AReader.Version > 10 then
  begin
    Areas.Clear;
    ACount := AReader.ReadInteger;
    for I := 0 to ACount - 1 do
      Areas.Add(AReader.ReadRect);
  end
  else
    Area := AReader.ReadRect;

  StopIfTrue := AReader.ReadBoolean;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.DoSaveToStream(AWriter: TcxWriter);
var
  I: Integer;
begin
  if AWriter.Version > 10 then
  begin
    AWriter.WriteInteger(Areas.Count);
    for I := 0 to Areas.Count - 1 do
      AWriter.WriteRect(Areas[I]);
  end
  else
    AWriter.WriteRect(Area);

  AWriter.WriteBoolean(StopIfTrue);
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.FlushCache;
begin
  // do nothing
end;

function TdxSpreadSheetCustomConditionalFormattingRule.ApplyToTheRowSupported: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetCustomConditionalFormattingRule.GetObjectName: string;
begin
  Result := Format('Item%d', [Index]);
end;

function TdxSpreadSheetCustomConditionalFormattingRule.GetProperties(AProperties: TStrings): Boolean;
begin
  // #AI: do not change the order
  Result := True;
  // for backward compatibility
  AProperties.Add('AreaLeft');
  AProperties.Add('AreaTop');
  AProperties.Add('AreaRight');
  AProperties.Add('AreaBottom');

  AProperties.Add('Areas');
  AProperties.Add('StopIfTrue');
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Areas' then
    AValue := Areas.ToString
  else
    if AName = 'StopIfTrue' then
      AValue := StopIfTrue
    else
      AValue := Null;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'AreaLeft' then
    Area := cxRect(AValue, Area.Top, Area.Right, Area.Bottom)
  else if AName = 'AreaTop' then
    Area := cxRect(Area.Left, AValue, Area.Right, Area.Bottom)
  else if AName = 'AreaRight' then
    Area := cxRect(Area.Left, Area.Top, AValue, Area.Bottom)
  else if AName = 'AreaBottom' then
    Area := cxRect(Area.Left, Area.Top, Area.Right, AValue)
  else if AName = 'Areas' then
    Areas.AssignFromString(AValue)
  else if AName = 'StopIfTrue' then
    StopIfTrue := AValue;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.AreasChangeHandler(Sender: TObject);
begin
  Changed;
end;

function TdxSpreadSheetCustomConditionalFormattingRule.GetArea: TRect;
begin
  Result := Areas.BoundingRect;
end;

function TdxSpreadSheetCustomConditionalFormattingRule.GetIndex: Integer;
begin
  Result := Owner.FRules.IndexOf(Self);
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SetArea(const AValue: TRect);
begin
  BeginUpdate;
  try
    Areas.Clear;
    Areas.Add(AValue);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SetAreas(AAreas: TdxSpreadSheetAreaList);
begin
  BeginUpdate;
  try
    Areas.Assign(AAreas);
  finally
    EndUpdate;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SetIndex(AValue: Integer);
var
  AIndex: Integer;
begin
  AIndex := Index;
  if AIndex <> AValue then
  begin
    Owner.FRules.Move(AIndex, AValue);
    Owner.DoRuleIndexChanged(AIndex, AValue);
    Changed;
  end;
end;

procedure TdxSpreadSheetCustomConditionalFormattingRule.SetStopIfTrue(const AValue: Boolean);
begin
  if StopIfTrue <> AValue then
  begin
    BeginUpdate;
    try
      FStopIfTrue := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingAreaInfo }

constructor TdxSpreadSheetConditionalFormattingAreaInfo.Create(
  const AOwner: IdxSpreadSheetConditionalFormattingOwner; AAreas: TdxSpreadSheetAreaList);
begin
  inherited Create;
  FAreas := AAreas.Clone;
  FOwner := AOwner;
end;

destructor TdxSpreadSheetConditionalFormattingAreaInfo.Destroy;
begin
  FreeAndNil(FPercentiles);
  FreeAndNil(FDuplicates);
  FreeAndNil(FBottomValues);
  FreeAndNil(FTopValues);
  FreeAndNil(FAreas);
  inherited Destroy;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetLimitValueForPercent(APercent: Integer): Double;
begin
  Result := MinValue + (MaxValue - MinValue) * APercent / 100;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetLimitValueForPercentile(APercentile: Integer): Double;
begin
  Result := 0;
  if (FPercentiles = nil) or not FPercentiles.TryGetValue(APercentile, Result) then
  begin
    if FPercentiles = nil then
      FPercentiles := TDictionary<Integer, Double>.Create;
    if CalculateBoundValueForPercentile(APercentile, Result) then
      FPercentiles.Add(APercentile, Result);
  end;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.IsBottomValue(const AValue: Double; ARank: Integer): Boolean;
begin
  Result := CheckTopBottomValues(FBottomValues, FBottomValuesRank, ARank) and (AValue <= FBottomValues[ARank - 1]);
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.IsTopValue(const AValue: Double; ARank: Integer): Boolean;
begin
  Result := CheckTopBottomValues(FTopValues, FTopValuesRank, ARank) and (AValue >= FTopValues[ARank - 1]);
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.IsUniqueValue(const AValue: Variant): Boolean;
begin
  if FDuplicates = nil then
  begin
    FDuplicates := TdxVariantList.Create;
    PopulateDuplicateValues;
  end;
  Result := FDuplicates.IndexOf(AValue) < 0;
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.CalculateAverageValue;
var
  AScaleFactor: Double;
begin
  FAverageValue := 0;
  if NumericValueCount > 0 then
  begin
    AScaleFactor := 1 / NumericValueCount;
    EnumCells(
      procedure (const ACell: IdxSpreadSheetCellData)
      begin
        if ACell.IsNumericValue then
          FAverageValue := FAverageValue + ACell.AsFloat * AScaleFactor;
      end);
  end;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.CalculateBoundValueForPercentile(
  APercentile: Integer; out AValue: Double): Boolean;
var
  AIndex: Integer;
  AValues: TList<Double>;
begin
  Result := False;
  if InRange(APercentile, 0, 100) then
    if GetSortedNumericValuesInArea(AValues) then
    try
      AIndex := (APercentile * AValues.Count) div 100;
      Result := InRange(AIndex, 0, AValues.Count - 1);
      if Result then
        AValue := AValues[AIndex];
    finally
      AValues.Free;
    end;
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.CalculateMinMaxValues;
begin
  FMaxValue := MinDouble;
  FMinValue := MaxDouble;

  EnumCells(
    procedure (const ACell: IdxSpreadSheetCellData)
    var
      AValue: Double;
    begin
      if ACell.IsNumericValue then
      begin
        AValue := ACell.AsFloat;
        FMaxValue := Max(FMaxValue, AValue);
        FMinValue := Min(FMinValue, AValue);
      end;
    end);

  if FMaxValue < FMinValue then
  begin
    FMaxValue := 0;
    FMinValue := 0;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.CalculateNumericValueCount;
begin
  FNumericValueCount := 0;
  EnumCells(
    procedure (const ACell: IdxSpreadSheetCellData)
    begin
      if ACell.IsNumericValue then
        Inc(FNumericValueCount);
    end);
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.CalculateStandardDeviation;
var
  AAverageValue: Double;
  AScaleFactor: Double;
begin
  FStandardDeviation := 0;
  if NumericValueCount > 1 then
  begin
    AAverageValue := AverageValue;
    AScaleFactor := 1 / (NumericValueCount - 1);
    EnumCells(
      procedure (const ACell: IdxSpreadSheetCellData)
      begin
        if ACell.IsNumericValue then
          FStandardDeviation := FStandardDeviation + AScaleFactor * Sqr(ACell.AsFloat - AAverageValue);
      end);
    FStandardDeviation := Sqrt(FStandardDeviation);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.CalculateTopBottomValues;
var
  AValues: TList<Double>;
  I: Integer;
begin
  if GetSortedNumericValuesInArea(AValues) then
  try
    if (FTopValues <> nil) and (FTopValuesRank <> FTopValues.Count) then
    begin
      FTopValues.Clear;
      FTopValues.Capacity := FTopValuesRank;
      for I := AValues.Count - 1 downto Max(0, AValues.Count - FTopValuesRank - 2) do
        FTopValues.Add(AValues.Items[I]);
      while (FTopValues.Count > 0) and (FTopValues.Count < FTopValuesRank) do
        FTopValues.Add(FTopValues.Last);
    end;

    if (FBottomValues <> nil) and (FBottomValuesRank <> FBottomValues.Count) then
    begin
      FBottomValues.Clear;
      FBottomValues.Capacity := FBottomValuesRank;
      for I := 0 to Min(FBottomValuesRank, AValues.Count) - 1 do
        FBottomValues.Add(AValues.Items[I]);
      while (FBottomValues.Count > 0) and (FBottomValues.Count < FBottomValuesRank) do
        FBottomValues.Add(FBottomValues.Last);
    end;
  finally
    AValues.Free;
  end;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.CheckTopBottomValues(
  var AList: TList<Double>; var AListRank: Integer; ATargetRank: Integer): Boolean;
begin
  if (ATargetRank > AListRank) and (ATargetRank > 0) then
  begin
    if AList = nil then
      AList := TList<Double>.Create;
    AListRank := ATargetRank;
    CalculateTopBottomValues;
  end;
  Result := (AList <> nil) and (ATargetRank > 0) and (ATargetRank <= AList.Count);
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.EnumCells(const AProc: TdxSpreadSheetViewForEachCellProc);
var
  I: Integer;
begin
  for I := 0 to Areas.Count - 1 do
    FOwner.ForEachCell(Areas.Items[I], AProc);
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetSortedNumericValuesInArea(out AValues: TList<Double>): Boolean;
var
  AList: TList<Double>;
begin
  Result := NumericValueCount > 0;
  if Result then
  begin
    AList := TList<Double>.Create;
    AList.Capacity := NumericValueCount;
    EnumCells(
      procedure (const ACell: IdxSpreadSheetCellData)
      begin
        if ACell.IsNumericValue then
          AList.Add(ACell.AsFloat);
      end);
    AList.Sort;
    AValues := AList;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingAreaInfo.PopulateDuplicateValues;
var
  ATempList: TdxVariantList;
begin
  FDuplicates.Clear;
  ATempList := TdxVariantList.Create;
  try
    ATempList.Capacity := 4096;
    EnumCells(
      procedure (const ACell: IdxSpreadSheetCellData)
      var
        AValue: Variant;
      begin
        AValue := ACell.AsVariant;
        if not VarIsNull(AValue) then
        begin
          if ATempList.IndexOf(AValue) < 0 then
            ATempList.Add(AValue)
          else
            if FDuplicates.IndexOf(AValue) < 0 then
              FDuplicates.Add(AValue);
        end;
      end);
  finally
    ATempList.Free;
  end;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetAverageValue: Double;
begin
  if not (dcavAverage in FAssignedValues) then
  begin
    CalculateAverageValue;
    Include(FAssignedValues, dcavAverage);
  end;
  Result := FAverageValue;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetMaxValue: Double;
begin
  if not (dcavMinMax in FAssignedValues) then
  begin
    CalculateMinMaxValues;
    Include(FAssignedValues, dcavMinMax);
  end;
  Result := FMaxValue;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetMinValue: Double;
begin
  if not (dcavMinMax in FAssignedValues) then
  begin
    CalculateMinMaxValues;
    Include(FAssignedValues, dcavMinMax);
  end;
  Result := FMinValue;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetNumericValueCount: Integer;
begin
  if not (dcavNumericValueCount in FAssignedValues) then
  begin
    CalculateNumericValueCount;
    Include(FAssignedValues, dcavNumericValueCount);
  end;
  Result := FNumericValueCount;
end;

function TdxSpreadSheetConditionalFormattingAreaInfo.GetStandardDeviation: Double;
begin
  if not (dcavStandardDeviation in FAssignedValues) then
  begin
    CalculateStandardDeviation;
    Include(FAssignedValues, dcavStandardDeviation);
  end;
  Result := FStandardDeviation;
end;

{ TdxSpreadSheetConditionalFormattingCustomRule }

function TdxSpreadSheetConditionalFormattingCustomRule.GetAreaInfo: TdxSpreadSheetConditionalFormattingAreaInfo;
begin
  Result := Owner.GetAreaInfo(Areas);
end;

{ TdxSpreadSheetConditionalFormattingStyleViewInfo }

constructor TdxSpreadSheetConditionalFormattingStyleViewInfo.Create(
  AStyle: TdxSpreadSheetCellDisplayStyle; AScaleFactor: TdxScaleFactor);
begin
  inherited Create;
  FStyle := AStyle;
  FScaleFactor := AScaleFactor;
  BackgroundColor := clDefault;
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.Calculate(const ABounds: TRect);
begin
  Calculate(ABounds, ABounds);
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.Calculate(const ABounds, AContentBounds: TRect);
begin
  FTextRect := AContentBounds;
  CalculateIconBounds(FTextRect);
  CalculateDataBarBounds(ABounds);
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.CalculateDataBarBounds(const R: TRect);

  function GetPointPosition(APosition: Single): Integer;
  begin
    Result := R.Left + Round(APosition * cxRectWidth(R));
  end;

begin
  FDataBarBounds := cxNullRect;
  FDataBarAxisBounds := cxNullRect;
  if not DataBar.IsEmpty then
  begin
    FDataBarAxisBounds := R;
    FDataBarAxisBounds.Left := GetPointPosition(DataBar.AxisPosition);
    FDataBarAxisBounds.Right := FDataBarAxisBounds.Left + 1;

    FDataBarBounds := cxRectInflate(R, -cxTextSpace);
    if DataBar.Position >= DataBar.AxisPosition then
    begin
      FDataBarBounds.Left := DataBarAxisBounds.Right;
      FDataBarBounds.Right := GetPointPosition(DataBar.Position);
    end
    else
    begin
      FDataBarBounds.Left := GetPointPosition(DataBar.Position);
      FDataBarBounds.Right := DataBarAxisBounds.Left;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.CalculateIconBounds(var R: TRect);
var
  AImageSetSize: TSize;
begin
  if Style.IconIndex >= 0 then
  begin
    AImageSetSize := dxGetImageSize(ConditionalFormattingIconSet.Icons, FScaleFactor);
    FIconBounds := cxRectSetWidth(R, AImageSetSize.cx);
    FIconBounds := cxRectCenterVertically(FIconBounds, AImageSetSize.cy);
    R.Left := FIconBounds.Right + cxTextSpace;
  end
  else
    FIconBounds := cxRectSetWidth(R, 0);
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Draw(ACanvas, dsFirst);
  Draw(ACanvas, dsSecond);
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  if AStage = dsFirst then
  begin
    if DataBar.AxisColor <> clNone then
      dxSpreadSheetDrawBackground(ACanvas, DataBarAxisBounds,
        cxGetActualColor(BackgroundColor, ACanvas.Brush.Color), DataBar.AxisColor, sscfsGray75);

    if DataBar.Color1 <> clNone then
    begin
      dxGpFillRectByGradient(ACanvas.Handle, DataBarBounds, DataBar.Color1, DataBar.Color2, LinearGradientModeHorizontal);
      if DataBar.Border <> clNone then
        ACanvas.FrameRect(DataBarBounds, DataBar.Border);
    end;
  end;
  if AStage = dsSecond then
    cxDrawImage(ACanvas, IconBounds, nil, ConditionalFormattingIconSet.Icons, Style.IconIndex, True, nil, ScaleFactor);
end;

procedure TdxSpreadSheetConditionalFormattingStyleViewInfo.Offset(const AOffset: TPoint);
begin
  FDataBarAxisBounds := cxRectOffset(FDataBarAxisBounds, AOffset);
  FDataBarBounds := cxRectOffset(FDataBarBounds, AOffset);
  FIconBounds := cxRectOffset(FIconBounds, AOffset);
  FTextRect := cxRectOffset(FTextRect, AOffset);
end;

function TdxSpreadSheetConditionalFormattingStyleViewInfo.GetDataBar: TdxSpreadSheetCellDataBar;
begin
  Result := Style.DataBar;
end;

end.
