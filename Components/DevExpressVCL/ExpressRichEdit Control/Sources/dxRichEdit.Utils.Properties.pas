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

unit dxRichEdit.Utils.Properties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.MailMerge;

type

  { TdxTableCellOperation }

  TdxTableCellOperation = (
    ShiftToTheHorizontally,
    ShiftToTheVertically,
    RowOperation,
    ColumnOperation
  );

  { TdxMergeDestination }

  TdxMergeDestination = (
    NewTab,
    &File
  );

 { TdxSymbolProperties }

  TdxSymbolProperties = record
  public
    UnicodeChar: Char;
    FontName: string;
    class function Create: TdxSymbolProperties; overload; static;
    class function Create(AUnicodeChar: Char; const AFontName: string): TdxSymbolProperties; overload; static;
    class operator Equal(const A, B: TdxSymbolProperties): Boolean;
    procedure SetFontName(const Value: string);
  end;

  { TdxCreateTableParameters }

  TdxCreateTableParameters = record
  strict private
    FRowCount: Integer;
    FColumnCount: Integer;
  private
    procedure SetColumnCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
  public
    constructor Create(ARowCount: Integer; AColumnCount: Integer);

    property RowCount: Integer read FRowCount write SetRowCount;
    property ColumnCount: Integer read FColumnCount write SetColumnCount;
  end;

  { TdxSplitTableCellsParameters  }

  TdxSplitTableCellsParameters = record
  private
    FIsSelectedCellsSquare: Boolean;
    FRowsCount: Integer;
    FColumnsCount: Integer;
    FMergeCellsBeforeSplit: Boolean;
    FRowCountAfterMerge: Integer;
    procedure SetColumnsCount(const Value: Integer);
    procedure SetIsSelectedCellsSquare(const Value: Boolean);
    procedure SetMergeCellsBeforeSplit(const Value: Boolean);
    procedure SetRowCountAfterMerge(const Value: Integer);
    procedure SetRowsCount(const Value: Integer);
  public
    constructor Create(AColumnsCount, ARowsCount: Integer; AMergeCellsBeforeSplit: Boolean; ARowCountAfterMerge: Integer);
    property ColumnsCount : Integer read FColumnsCount write SetColumnsCount;
    property RowsCount : Integer read FRowsCount write SetRowsCount;
    property MergeCellsBeforeSplit: Boolean read FMergeCellsBeforeSplit write SetMergeCellsBeforeSplit;
    property IsSelectedCellsSquare: Boolean read FIsSelectedCellsSquare write SetIsSelectedCellsSquare;
    property RowCountAfterMerge: Integer read FRowCountAfterMerge write SetRowCountAfterMerge;
  end;

  { TdxTableCellsParameters }

  TdxTableCellsParameters = record
  strict private
    FCellOperation: TdxTableCellOperation;
  public
    constructor Create(ACellOperation: TdxTableCellOperation);
    property CellOperation: TdxTableCellOperation read FCellOperation write FCellOperation;
  end;

  { TdxColumnInfoUI }

  TdxColumnInfoUI = class
  strict private
    FNumber: Integer;
    FWidth: TdxNullableInteger;
    FSpacing: TdxNullableInteger;
  public
    constructor Create(ANumber: Integer);

    property Number: Integer read FNumber;
    property Width: TdxNullableInteger read FWidth write FWidth;
    property Spacing: TdxNullableInteger read FSpacing write FSpacing;
  end;

  { TdxColumnsInfoUI }

  TdxColumnsInfoUI = class
  strict private const
    MinColumnWidth = 720;
    MinSpacingWidth = 0;
  strict private
    FApplyType: TdxSectionPropertiesApplyType;
    FAvailableApplyType: TdxSectionPropertiesApplyTypes;
    FPageWidth: Integer;
    FColumnCount: TdxNullableInteger;
    FEqualColumnWidth: TdxNullableBoolean;
    FColumns: TdxObjectList<TdxColumnInfoUI>;
    function GetMaxColumnCount: Integer;
  protected
    procedure CalculateEqualColumnsOnChangeCount; virtual;
    procedure CalculateNotEqualColumnsOnChangeCount(APreviousCount: Integer); virtual;
    procedure CorrectColumns; virtual;
    procedure DisableTheLastSpacing; virtual;
    procedure CalculateUniformColumnsCore(AColumnWidth: Integer; AColumnSpacing: Integer; ARestWidth: Integer; ARestSpacing: Integer); virtual;
    function CalculateAvailableSpace: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function HasColumnsNull: Boolean;
    function HasColumnsInfoUINull: Boolean;
    procedure ChangeColumnCount(ACount: Integer);
    procedure RecalculateColumnsByWidthAfterIndex(AIndex: Integer);
    procedure RecalculateColumnsBySpacingAfterIndex(AIndex: Integer);
    procedure CalculateUniformColumnsByColumnWidth; overload;
    procedure CalculateUniformColumnsByColumnWidth(AColumnWidth: Integer); overload;
    procedure CalculateUniformColumnsByColumnSpacing; overload;
    procedure CalculateUniformColumnsByColumnSpacing(AColumnSpacing: Integer); overload;
    procedure ChangeColumnsNotEqualByWidthAfterIndex(AIndex: Integer);
    procedure ChangeColumnsNotEqualBySpacingAfterIndex(AIndex: Integer);
    function Clone: TdxColumnsInfoUI;
    procedure CopyFrom(AInfo: TdxColumnsInfoUI);

    property AvailableApplyType: TdxSectionPropertiesApplyTypes read FAvailableApplyType write FAvailableApplyType;
    property ApplyType: TdxSectionPropertiesApplyType read FApplyType write FApplyType;
    property PageWidth: Integer read FPageWidth write FPageWidth;
    property ColumnCount: TdxNullableInteger read FColumnCount write FColumnCount;
    property EqualColumnWidth: TdxNullableBoolean read FEqualColumnWidth write FEqualColumnWidth;
    property MaxColumnCount: Integer read GetMaxColumnCount;
    property Columns: TdxObjectList<TdxColumnInfoUI> read FColumns;
  end;

  { TdxColumnsDistributionCalculator }

  TdxColumnsDistributionCalculator = class abstract
  strict private
    FColumns: TdxObjectList<TdxColumnInfoUI>;
  protected
    function GetMinValue: Integer; virtual; abstract;
    function CalculateTotal(AFrom, ATo: Integer): Integer; virtual;
    function HasEnoughSpaceForDistribution(AFrom, ATo, ASpace: Integer): Boolean; virtual;
    function SetMinValues(AFrom, ATo, ASpace: Integer): Integer; virtual;
    procedure CorrectValue(AIndex: Integer); virtual;
    function DistributeRemainder(AFrom: Integer; ATo: Integer; ARemainder: Integer): Integer; virtual;
    function DistributeSpaceCore(AFrom: Integer; ATo: Integer; ASpace: Integer): Integer; virtual;
    function DistributeSpace(AFrom: Integer; ATo: Integer; ASpace: Integer): Integer; virtual;
    procedure SetAllValues(AValue: Integer; ARest: Integer); virtual;
    function GetValue(AColumn: TdxColumnInfoUI): Integer; virtual; abstract;
    procedure SetValue(AColumn: TdxColumnInfoUI; AValue: Integer); virtual; abstract;
  public
    constructor Create(AColumns: TdxObjectList<TdxColumnInfoUI>);

    property Columns: TdxObjectList<TdxColumnInfoUI> read FColumns;
    property MinValue: Integer read GetMinValue;
  end;

  { TdxColumnsDistributionWidthPriorityCalculator }

  TdxColumnsDistributionWidthPriorityCalculator = class(TdxColumnsDistributionCalculator)
  protected
    function GetMinValue: Integer; override;
    function GetValue(AColumn: TdxColumnInfoUI): Integer; override;
    procedure SetValue(AColumn: TdxColumnInfoUI; AValue: Integer); override;
  end;

  { TdxColumnsDistributionSpacingPriorityCalculator }

  TdxColumnsDistributionSpacingPriorityCalculator = class(TdxColumnsDistributionCalculator)
  protected
    function GetMinValue: Integer; override;
    function GetValue(AColumn: TdxColumnInfoUI): Integer; override;
    procedure SetValue(AColumn: TdxColumnInfoUI; AValue: Integer); override;
  end;

  { TdxColumnsInfoPreset }

  TdxColumnsInfoPreset = class abstract
  public const
    Spacing = 1800;
  public
    function MatchTo(AColumnsInfo: TdxColumnsInfoUI): Boolean; virtual; abstract;
    procedure ApplyTo(AColumnsInfo: TdxColumnsInfoUI); virtual; abstract;
  end;

  { TdxUniformColumnsInfoPreset }

  TdxUniformColumnsInfoPreset = class abstract(TdxColumnsInfoPreset)
  protected
    function GetColumnCount: Integer; virtual; abstract;
  public
    function MatchTo(AColumnsInfo: TdxColumnsInfoUI): Boolean; override;
    procedure ApplyTo(AColumnsInfo: TdxColumnsInfoUI); override;

    property ColumnCount: Integer read GetColumnCount;
  end;

  { TdxSingleColumnsInfoPreset }

  TdxSingleColumnsInfoPreset = class(TdxUniformColumnsInfoPreset)
  protected
    function GetColumnCount: Integer; override;
  end;

  { TdxTwoUniformColumnsInfoPreset }

  TdxTwoUniformColumnsInfoPreset = class(TdxUniformColumnsInfoPreset)
  protected
    function GetColumnCount: Integer; override;
  end;

  { TdxThreeUniformColumnsInfoPreset }

  TdxThreeUniformColumnsInfoPreset = class(TdxUniformColumnsInfoPreset)
  protected
    function GetColumnCount: Integer; override;
  end;

  { TdxTwoNonUniformColumnsInfoPreset }

  TdxTwoNonUniformColumnsInfoPreset = class abstract(TdxColumnsInfoPreset)
  protected
    function GetFirstColumnRelativeWidth: Single; virtual; abstract;

    property FirstColumnRelativeWidth: Single read GetFirstColumnRelativeWidth;
  public
    function MatchTo(AColumnsInfo: TdxColumnsInfoUI): Boolean; override;
    procedure ApplyTo(AColumnsInfo: TdxColumnsInfoUI); override;
  end;

  { TdxLeftNarrowColumnsInfoPreset }

  TdxLeftNarrowColumnsInfoPreset = class(TdxTwoNonUniformColumnsInfoPreset)
  protected
    function GetFirstColumnRelativeWidth: Single; override;
  end;

  { TdxRightNarrowColumnsInfoPreset }

  TdxRightNarrowColumnsInfoPreset = class(TdxTwoNonUniformColumnsInfoPreset)
  protected
    function GetFirstColumnRelativeWidth: Single; override;
  end;

  { TdxFloatingInlineObjectParameters }

  TdxFloatingInlineObjectParameters = record
  strict private
    FFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun;
    FInlinePictureRun: TdxInlinePictureRun;
  public
    constructor Create(AFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun); overload;
    constructor Create(AInlinePictureRun: TdxInlinePictureRun); overload;

    property FloatingObjectAnchorRun: TdxFloatingObjectAnchorRun read FFloatingObjectAnchorRun write FFloatingObjectAnchorRun;
    property InlinePictureRun: TdxInlinePictureRun read FInlinePictureRun;
  end;

  { TdxPageSetupInfo }

  TdxPageSetupInfo = class(TdxCloneable)
  strict private
    FApplyType: TdxSectionPropertiesApplyType;
    FAvailableApplyTypes: TdxSectionPropertiesApplyTypes;
    FLeftMargin: TdxNullableInteger;
    FRightMargin: TdxNullableInteger;
    FTopMargin: TdxNullableInteger;
    FBottomMargin: TdxNullableInteger;
    FPaperWidth: TdxNullableInteger;
    FPaperHeight: TdxNullableInteger;
    FPaperKind: TdxNullableValue<TdxPaperKind>;
    FLandscape: TdxNullableBoolean;
    FSectionStartType: TdxNullableValue<TdxSectionStartType>;
    FDifferentFirstPage: TdxNullableBoolean;
    FDifferentOddAndEvenPages: TdxNullableBoolean;
  public
    constructor Create; override;
    function Clone: TdxPageSetupInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property AvailableApplyType: TdxSectionPropertiesApplyTypes read FAvailableApplyTypes write FAvailableApplyTypes;
    property ApplyType: TdxSectionPropertiesApplyType read FApplyType write FApplyType;
    property LeftMargin: TdxNullableInteger read FLeftMargin write FLeftMargin;
    property RightMargin: TdxNullableInteger read FRightMargin write FRightMargin;
    property TopMargin: TdxNullableInteger read FTopMargin write FTopMargin;
    property BottomMargin: TdxNullableInteger read FBottomMargin write FBottomMargin;
    property PaperWidth: TdxNullableInteger read FPaperWidth write FPaperWidth;
    property PaperHeight: TdxNullableInteger read FPaperHeight write FPaperHeight;
    property PaperKind: TdxNullableValue<TdxPaperKind> read FPaperKind write FPaperKind;
    property Landscape: TdxNullableBoolean read FLandscape write FLandscape;
    property SectionStartType: TdxNullableValue<TdxSectionStartType> read FSectionStartType write FSectionStartType;
    property DifferentFirstPage: TdxNullableBoolean read FDifferentFirstPage write FDifferentFirstPage;
    property DifferentOddAndEvenPages: TdxNullableBoolean read FDifferentOddAndEvenPages write FDifferentOddAndEvenPages;
  end;

  { TdxMergeRecordsParameters }

  TdxMergeRecordsParameters = record
  private
    FMergeDestination: TdxMergeDestination;
    FMergeRecords: TdxMergeRecords;
  public
    constructor Create(AMergeRecords: TdxMergeRecords; AMergeDestination: TdxMergeDestination);
    property MergeRecords: TdxMergeRecords read FMergeRecords write FMergeRecords;
    property MergeDestination: TdxMergeDestination read FMergeDestination write FMergeDestination;
  end;

implementation

uses
  Math, Contnrs,
  dxMeasurementUnits;

{ TdxSymbolProperties }

class function TdxSymbolProperties.Create: TdxSymbolProperties;
begin
  Result.UnicodeChar := ' ';
  Result.FontName := '';
end;

class function TdxSymbolProperties.Create(AUnicodeChar: Char; const AFontName: string): TdxSymbolProperties;
begin
  Result.UnicodeChar := AUnicodeChar;
  Result.FontName := AFontName;
end;

class operator TdxSymbolProperties.Equal(const A, B: TdxSymbolProperties): Boolean;
begin
  Result := (A.UnicodeChar = B.UnicodeChar) and (A.FontName = B.FontName);
end;

procedure TdxSymbolProperties.SetFontName(const Value: string);
begin
  FontName := Value;
end;

{ TdxCreateTableParameters }

constructor TdxCreateTableParameters.Create(ARowCount: Integer; AColumnCount: Integer);
begin
  FRowCount := ARowCount;
  FColumnCount := AColumnCount;
end;

procedure TdxCreateTableParameters.SetColumnCount(const Value: Integer);
begin
  FColumnCount := Value;
end;

procedure TdxCreateTableParameters.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
end;

  { TdxSplitTableCellsParameters }

constructor TdxSplitTableCellsParameters.Create(AColumnsCount, ARowsCount: Integer; AMergeCellsBeforeSplit: Boolean;
  ARowCountAfterMerge: Integer);
begin
  FIsSelectedCellsSquare := False;
  FRowsCount := ARowsCount;
  FColumnsCount := AColumnsCount;
  FMergeCellsBeforeSplit := AMergeCellsBeforeSplit;
  FRowCountAfterMerge := ARowCountAfterMerge;
end;

procedure TdxSplitTableCellsParameters.SetColumnsCount(const Value: Integer);
begin
  FColumnsCount := Value;
end;

procedure TdxSplitTableCellsParameters.SetIsSelectedCellsSquare(const Value: Boolean);
begin
  FIsSelectedCellsSquare := Value;
end;

procedure TdxSplitTableCellsParameters.SetMergeCellsBeforeSplit(const Value: Boolean);
begin
  FMergeCellsBeforeSplit := Value;
end;

procedure TdxSplitTableCellsParameters.SetRowCountAfterMerge(const Value: Integer);
begin
  FRowCountAfterMerge := Value;
end;

procedure TdxSplitTableCellsParameters.SetRowsCount(const Value: Integer);
begin
  FRowsCount := Value;
end;

{ TdxTableCellsParameters }

constructor TdxTableCellsParameters.Create(ACellOperation: TdxTableCellOperation);
begin
  FCellOperation := ACellOperation;
end;

{ TdxColumnInfoUI }

constructor TdxColumnInfoUI.Create(ANumber: Integer);
begin
  inherited Create;
  FNumber := ANumber;
end;

{ TdxColumnsInfoUI }

constructor TdxColumnsInfoUI.Create;
begin
  inherited Create;
  FColumns := TdxObjectList<TdxColumnInfoUI>.Create;
  AvailableApplyType := [TdxSectionPropertiesApplyType.CurrentSection, TdxSectionPropertiesApplyType.SelectedSections, TdxSectionPropertiesApplyType.WholeDocument];
end;

destructor TdxColumnsInfoUI.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

function TdxColumnsInfoUI.GetMaxColumnCount: Integer;
begin
  Result := PageWidth div (MinColumnWidth + MinSpacingWidth);
end;

function TdxColumnsInfoUI.HasColumnsNull: Boolean;
var
  I, AColumnCount: Integer;
begin
  if ColumnCount > Columns.Count then
    Exit(True);
  if ColumnCount.IsNull then
    AColumnCount := 0
  else
    AColumnCount := ColumnCount.Value;
  for I := 0 to AColumnCount - 1 do
  begin
    if Columns[I].Width.IsNull then
      Exit(True);
    if Columns[I].Spacing.IsNull then
      Exit(True);
  end;
  Result := False;
end;

function TdxColumnsInfoUI.HasColumnsInfoUINull: Boolean;
begin
  if ColumnCount.IsNull then
    Exit(True);
  if EqualColumnWidth.IsNull then
    Exit(True);
  Result := HasColumnsNull;
end;

procedure TdxColumnsInfoUI.ChangeColumnCount(ACount: Integer);
var
  APreviousCount, I: Integer;
  AHasColumnInfoUINull: Boolean;
begin
  if ACount <= 0 then
    Exit;

  ACount := Min(ACount, MaxColumnCount);

  APreviousCount := Columns.Count;
  AHasColumnInfoUINull := HasColumnsInfoUINull;

  for I := Columns.Count to ACount - 1 do
    Columns.Add(TdxColumnInfoUI.Create(I + 1));

  for I := Columns.Count - 1 downto ACount do
    Columns.Delete(I);

  ColumnCount := ACount;

  if AHasColumnInfoUINull then
  begin
    CalculateEqualColumnsOnChangeCount;
    Exit;
  end;

  if not FEqualColumnWidth.Value and (APreviousCount > 0) then
    CalculateNotEqualColumnsOnChangeCount(APreviousCount)
  else
    CalculateEqualColumnsOnChangeCount;
end;

procedure TdxColumnsInfoUI.CalculateEqualColumnsOnChangeCount;
var
  ASpacingValue: Integer;
begin
  if Columns.Count <= 0 then
    Exit;
  if not Columns[0].Spacing.IsNull then
    ASpacingValue := Columns[0].Spacing.Value
  else
    ASpacingValue := MinColumnWidth;

  CalculateUniformColumnsByColumnSpacing(ASpacingValue);
end;

procedure TdxColumnsInfoUI.CalculateNotEqualColumnsOnChangeCount(APreviousCount: Integer);
var
  ACalculateCount, I: Integer;
begin
  if Columns.Count <= 0 then
    Exit;

  if Columns.Count = 1 then
    Columns[0].Width := FPageWidth;

  ACalculateCount := Min(APreviousCount, Columns.Count);

  for I := 0 to ACalculateCount - 1 do
    Columns[I].Width := Max(MinColumnWidth, MulDiv(Columns[I].Width.Value, APreviousCount, Columns.Count));

  for I := 0 to ACalculateCount - 1 - 1 do
    Columns[I].Spacing := Max(MinSpacingWidth, MulDiv(Columns[I].Spacing.Value, (APreviousCount - 1), Columns.Count - 1));

  if ACalculateCount > 0 then
    for I := ACalculateCount to Columns.Count - 1 do
      Columns[I].Width := Columns[ACalculateCount - 1].Width.Value;

  if ACalculateCount > 1 then
    for I := ACalculateCount - 1 to Columns.Count - 1 - 1 do
      Columns[I].Spacing := Columns[ACalculateCount - 2].Spacing.Value;

  DisableTheLastSpacing;
  CorrectColumns;
end;

procedure TdxColumnsInfoUI.CorrectColumns;
var
  ADifference, ASumWidth, ASumSpacing, ADifferenceWidth, ADifferenceSpacing: Integer;
  ACalculatorWidth, ACalculatorSpacing: TdxColumnsDistributionCalculator;
  APartWidth: Double;
begin
  if FColumnCount.IsNull or (FColumnCount.Value <= 0) then
    Exit;

  ADifference := -CalculateAvailableSpace;
  ACalculatorWidth := TdxColumnsDistributionWidthPriorityCalculator.Create(Columns);
  ACalculatorSpacing := TdxColumnsDistributionSpacingPriorityCalculator.Create(Columns);
  try
    ASumWidth := ACalculatorWidth.CalculateTotal(0, Columns.Count - 1);
    ASumSpacing := ACalculatorSpacing.CalculateTotal(0, Columns.Count - 1);
    APartWidth := ASumWidth / (ASumWidth + ASumSpacing);
    ADifferenceWidth := Trunc(ADifference * APartWidth);
    ADifferenceSpacing := ADifference - ADifferenceWidth;
    ACalculatorWidth.DistributeSpace(0, Columns.Count - 1, ADifferenceWidth);
    ACalculatorSpacing.DistributeSpace(0, Columns.Count - 2, ADifferenceSpacing);
  finally
    ACalculatorWidth.Free;
    ACalculatorSpacing.Free;
  end;
end;

procedure TdxColumnsInfoUI.DisableTheLastSpacing;
begin
  Columns.Last.Spacing := 0;
end;

procedure TdxColumnsInfoUI.RecalculateColumnsByWidthAfterIndex(AIndex: Integer);
begin
  if HasColumnsInfoUINull then
    Exit;
  if EqualColumnWidth.Value then
    CalculateUniformColumnsByColumnWidth
  else
    ChangeColumnsNotEqualByWidthAfterIndex(AIndex);
end;

procedure TdxColumnsInfoUI.RecalculateColumnsBySpacingAfterIndex(AIndex: Integer);
begin
  if HasColumnsInfoUINull then
    Exit;
  if EqualColumnWidth.Value then
    CalculateUniformColumnsByColumnSpacing
  else
    ChangeColumnsNotEqualBySpacingAfterIndex(AIndex);
end;

procedure TdxColumnsInfoUI.CalculateUniformColumnsCore(AColumnWidth: Integer; AColumnSpacing: Integer; ARestWidth: Integer; ARestSpacing: Integer);
var
  ACalculatorWidth, ACalculatorSpacing: TdxColumnsDistributionCalculator;
begin
  ACalculatorWidth := TdxColumnsDistributionWidthPriorityCalculator.Create(Columns);
  ACalculatorSpacing := TdxColumnsDistributionSpacingPriorityCalculator.Create(Columns);
  try
    ACalculatorWidth.SetAllValues(AColumnWidth, ARestWidth);
    ACalculatorSpacing.SetAllValues(AColumnSpacing, ARestSpacing);

    DisableTheLastSpacing;
  finally
    ACalculatorWidth.Free;
    ACalculatorSpacing.Free;
  end;
end;

procedure TdxColumnsInfoUI.CalculateUniformColumnsByColumnWidth;
var
  AColumnWidth: Integer;
begin
  if not Columns[0].Width.IsNull then
    AColumnWidth := Columns[0].Width.Value
  else
    AColumnWidth := MinColumnWidth;
  CalculateUniformColumnsByColumnWidth(AColumnWidth);
end;

procedure TdxColumnsInfoUI.CalculateUniformColumnsByColumnWidth(AColumnWidth: Integer);
var
  ADividend, ADivider, ARestSpacing, AColumnSpacing: Integer;
begin
  if FColumnCount.IsNull or (FColumnCount.Value <= 0) then
    Exit;

  if FColumnCount.Value <= 1 then
    AColumnWidth := FPageWidth;

  if AColumnWidth * FColumnCount.Value > FPageWidth then
    AColumnWidth := FPageWidth div FColumnCount.Value;

  AColumnWidth := Max(AColumnWidth, MinColumnWidth);

  ADividend := PageWidth - AColumnWidth * FColumnCount.Value;
  ADivider := Max(1, FColumnCount.Value - 1);
  ARestSpacing := ADividend mod ADivider;
  AColumnSpacing := ADividend div ADivider;

  CalculateUniformColumnsCore(AColumnWidth, AColumnSpacing, 0, ARestSpacing);
end;

procedure TdxColumnsInfoUI.CalculateUniformColumnsByColumnSpacing;
var
  AColumnSpacing: Integer;
begin
  if HasColumnsInfoUINull then
    Exit;
  if not Columns[0].Spacing.IsNull then
    AColumnSpacing := Columns[0].Spacing.Value
  else
    AColumnSpacing := MinSpacingWidth;
  CalculateUniformColumnsByColumnSpacing(AColumnSpacing);
end;

procedure TdxColumnsInfoUI.CalculateUniformColumnsByColumnSpacing(AColumnSpacing: Integer);
var
  ADividend, ARestWidth, AColumnWidth: Integer;
begin
  if FColumnCount.IsNull or (FColumnCount.Value <= 0) then
    Exit;

  AColumnSpacing := Max(AColumnSpacing, MinSpacingWidth);

  Assert(Columns.Count = FColumnCount.Value);

  if AColumnSpacing * (FColumnCount.Value - 1) > FPageWidth - MinColumnWidth * FColumnCount.Value then
    AColumnSpacing := (FPageWidth - MinColumnWidth * FColumnCount.Value) div (FColumnCount.Value - 1);

  if FColumnCount.Value <= 1 then
    AColumnSpacing := 0;

  ADividend := PageWidth - AColumnSpacing * (FColumnCount.Value - 1);
  ARestWidth := ADividend mod FColumnCount.Value;
  AColumnWidth := ADividend div FColumnCount.Value;

  CalculateUniformColumnsCore(AColumnWidth, AColumnSpacing, ARestWidth, 0);
end;

function TdxColumnsInfoUI.CalculateAvailableSpace: Integer;
var
  AUsedSpace, I: Integer;
  AColumn: TdxColumnInfoUI;
begin
  AUsedSpace := 0;
  for I := 0 to FColumnCount.Value - 1 do
  begin
    AColumn := Columns[I];
    if not AColumn.Width.IsNull then
      Inc(AUsedSpace, AColumn.Width.Value);
    if not AColumn.Spacing.IsNull then
      Inc(AUsedSpace, AColumn.Spacing.Value);
  end;
  Result := FPageWidth - AUsedSpace;
end;

procedure TdxColumnsInfoUI.ChangeColumnsNotEqualByWidthAfterIndex(AIndex: Integer);
var
  ACalculatorWidth, ACalculatorSpacing: TdxColumnsDistributionCalculator;
  ADifference: Integer;
begin
  if FColumnCount.IsNull or (FColumnCount.Value <= 0) or (AIndex >= FColumnCount.Value) then
    Exit;

  ACalculatorWidth := TdxColumnsDistributionWidthPriorityCalculator.Create(Columns);
  ACalculatorSpacing := TdxColumnsDistributionSpacingPriorityCalculator.Create(Columns);
  try
    ACalculatorWidth.CorrectValue(AIndex);
    ADifference := -CalculateAvailableSpace;
    ADifference := ACalculatorWidth.DistributeSpace(AIndex + 1, FColumnCount.Value - 1, ADifference);
    ADifference := ACalculatorWidth.DistributeSpace(0, AIndex - 1, ADifference);
    ADifference := ACalculatorSpacing.DistributeSpace(0, FColumnCount.Value - 2, ADifference);
    Columns[AIndex].Width := Columns[AIndex].Width.Value - ADifference;
    DisableTheLastSpacing;
  finally
    ACalculatorWidth.Free;
    ACalculatorSpacing.Free;
  end;
end;

procedure TdxColumnsInfoUI.ChangeColumnsNotEqualBySpacingAfterIndex(AIndex: Integer);
var
  ACalculatorWidth, ACalculatorSpacing: TdxColumnsDistributionCalculator;
  ADifference: Integer;
begin
  if FColumnCount.IsNull or (FColumnCount.Value <= 0) or (AIndex >= FColumnCount.Value) then
    Exit;

  ACalculatorWidth := TdxColumnsDistributionWidthPriorityCalculator.Create(Columns);
  ACalculatorSpacing := TdxColumnsDistributionSpacingPriorityCalculator.Create(Columns);
  try
    ACalculatorSpacing.CorrectValue(AIndex);
    ADifference := -CalculateAvailableSpace;
    ADifference := ACalculatorWidth.DistributeSpace(AIndex + 1, FColumnCount.Value - 1, ADifference);
    ADifference := ACalculatorWidth.DistributeSpace(0, AIndex, ADifference);
    ADifference := ACalculatorSpacing.DistributeSpace(0, AIndex - 1, ADifference);
    ADifference := ACalculatorSpacing.DistributeSpace(AIndex + 1, FColumnCount.Value - 2, ADifference);
    Columns[AIndex].Spacing := Columns[AIndex].Spacing.Value - ADifference;
    DisableTheLastSpacing;
  finally
    ACalculatorWidth.Free;
    ACalculatorSpacing.Free;
  end;
end;

function TdxColumnsInfoUI.Clone: TdxColumnsInfoUI;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxColumnsInfoUI.Create;
  Result.CopyFrom(Self);
end;

procedure TdxColumnsInfoUI.CopyFrom(AInfo: TdxColumnsInfoUI);
var
  I: Integer;
begin
  ApplyType := AInfo.ApplyType;
  AvailableApplyType := AInfo.AvailableApplyType;

  PageWidth := AInfo.PageWidth;
  EqualColumnWidth := AInfo.EqualColumnWidth;
  ChangeColumnCount(AInfo.Columns.Count);
  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].Width := AInfo.Columns[I].Width;
    Columns[I].Spacing := AInfo.Columns[I].Spacing;
  end;
end;

{ TdxColumnsDistributionCalculator }

constructor TdxColumnsDistributionCalculator.Create(AColumns: TdxObjectList<TdxColumnInfoUI>);
begin
  inherited Create;
  FColumns := AColumns;
end;

function TdxColumnsDistributionCalculator.CalculateTotal(AFrom, ATo: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AFrom to ATo do
    Inc(Result, GetValue(Columns[I]));
end;

function TdxColumnsDistributionCalculator.HasEnoughSpaceForDistribution(AFrom, ATo, ASpace: Integer): Boolean;
var
  ATotal: Integer;
begin
  ATotal := CalculateTotal(AFrom, ATo);
  Result := ASpace < ATotal - MinValue * (ATo - AFrom + 1);
end;

function TdxColumnsDistributionCalculator.SetMinValues(AFrom, ATo, ASpace: Integer): Integer;
var
  I: Integer;
begin
  for I := AFrom to ATo do
  begin
    Dec(ASpace, GetValue(Columns[I]) - MinValue);
    SetValue(Columns[I], MinValue);
  end;
  Result := ASpace;
end;

procedure TdxColumnsDistributionCalculator.CorrectValue(AIndex: Integer);
begin
  if AIndex >= Columns.Count then
    Exit;

  if GetValue(Columns[AIndex]) < MinValue then
    SetValue(Columns[AIndex], MinValue);
end;

function TdxColumnsDistributionCalculator.DistributeRemainder(AFrom: Integer; ATo: Integer; ARemainder: Integer): Integer;
var
  ACorrection, I, ANewValue: Integer;
begin
  ACorrection := IfThen(ARemainder > 0, 1, -1);

  while ARemainder <> 0 do
  begin
    I := AFrom;
    while (I <= ATo) and (ARemainder <> 0) do
    begin
      ANewValue := GetValue(Columns[I]) - ACorrection;
      if ANewValue > MinValue then
      begin
        SetValue(Columns[I], ANewValue);
        Dec(ARemainder, ACorrection);
      end;
      Inc(I);
    end;
  end;

  Result := 0;
end;

function TdxColumnsDistributionCalculator.DistributeSpaceCore(AFrom: Integer; ATo: Integer; ASpace: Integer): Integer;
var
  ARemainder, ADifference, I, ANewValue: Integer;
begin
  ARemainder := ASpace mod (ATo - AFrom + 1);
  ADifference := ASpace div (ATo - AFrom + 1);

  for I := AFrom to ATo do
  begin
    ANewValue := GetValue(Columns[I]) - ADifference;
    if ANewValue >= MinValue then
      SetValue(Columns[I], ANewValue)
    else
    begin
      SetValue(Columns[I], MinValue);
      Inc(ARemainder, (MinValue - ANewValue));
    end;
  end;
  DistributeRemainder(AFrom, ATo, ARemainder);

  Result := 0;
end;

function TdxColumnsDistributionCalculator.DistributeSpace(AFrom: Integer; ATo: Integer; ASpace: Integer): Integer;
begin
  if AFrom > ATo then
    Result := ASpace
  else
    if HasEnoughSpaceForDistribution(AFrom, ATo, ASpace) then
      Result := DistributeSpaceCore(AFrom, ATo, ASpace)
    else
      Result := SetMinValues(AFrom, ATo, ASpace);
end;

procedure TdxColumnsDistributionCalculator.SetAllValues(AValue: Integer; ARest: Integer);
var
  ACount, I: Integer;
begin
  ACount := Columns.Count;
  for I := 0 to ACount - 1 do
    SetValue(Columns[I], AValue);
  DistributeSpace(0, ACount - 1, -ARest);
end;

{ TdxColumnsDistributionWidthPriorityCalculator }

function TdxColumnsDistributionWidthPriorityCalculator.GetMinValue: Integer;
begin
  Result := 720;
end;

function TdxColumnsDistributionWidthPriorityCalculator.GetValue(AColumn: TdxColumnInfoUI): Integer;
begin
  if not AColumn.Width.IsNull then
    Result := AColumn.Width.Value
  else
    Result := 0;
end;

procedure TdxColumnsDistributionWidthPriorityCalculator.SetValue(AColumn: TdxColumnInfoUI; AValue: Integer);
begin
  AColumn.Width := AValue;
end;

{ TdxColumnsDistributionSpacingPriorityCalculator }

function TdxColumnsDistributionSpacingPriorityCalculator.GetMinValue: Integer;
begin
  Result := 0;
end;

function TdxColumnsDistributionSpacingPriorityCalculator.GetValue(AColumn: TdxColumnInfoUI): Integer;
begin
  if not AColumn.Spacing.IsNull then
    Result := AColumn.Spacing.Value
  else
    Result := 0;
end;

procedure TdxColumnsDistributionSpacingPriorityCalculator.SetValue(AColumn: TdxColumnInfoUI; AValue: Integer);
begin
  AColumn.Spacing := AValue;
end;

{ TdxUniformColumnsInfoPreset }

function TdxUniformColumnsInfoPreset.MatchTo(AColumnsInfo: TdxColumnsInfoUI): Boolean;
begin
  if AColumnsInfo.EqualColumnWidth.IsNull then
    Result := False
  else
    if not AColumnsInfo.EqualColumnWidth.Value then
      Result := False
    else
      if AColumnsInfo.ColumnCount.IsNull then
        Result := False
      else
        Result := AColumnsInfo.ColumnCount.Value = ColumnCount;
end;

procedure TdxUniformColumnsInfoPreset.ApplyTo(AColumnsInfo: TdxColumnsInfoUI);
begin
  AColumnsInfo.EqualColumnWidth := True;
  if AColumnsInfo.Columns.Count > 0 then
    AColumnsInfo.Columns[0].Spacing := Spacing;
  AColumnsInfo.ChangeColumnCount(ColumnCount);
end;

{ TdxSingleColumnsInfoPreset }

function TdxSingleColumnsInfoPreset.GetColumnCount: Integer;
begin
  Result := 1;
end;

{ TdxTwoUniformColumnsInfoPreset }

function TdxTwoUniformColumnsInfoPreset.GetColumnCount: Integer;
begin
  Result := 2;
end;

{ TdxThreeUniformColumnsInfoPreset }

function TdxThreeUniformColumnsInfoPreset.GetColumnCount: Integer;
begin
  Result := 3;
end;

{ TdxTwoNonUniformColumnsInfoPreset }

function TdxTwoNonUniformColumnsInfoPreset.MatchTo(AColumnsInfo: TdxColumnsInfoUI): Boolean;
var
  ATotalWidth: Integer;
begin
  if AColumnsInfo.EqualColumnWidth.IsNull or AColumnsInfo.EqualColumnWidth.Value then
    Exit(False);
  if (AColumnsInfo.ColumnCount <> 2) or (AColumnsInfo.Columns.Count <> 2) then
    Exit(False);

  if AColumnsInfo.Columns[0].Width.IsNull then
    Exit(False);
  if AColumnsInfo.Columns[0].Spacing.IsNull then
    Exit(False);
  if AColumnsInfo.Columns[1].Width.IsNull then
    Exit(False);
  if AColumnsInfo.Columns[1].Spacing.IsNull then
    Exit(False);

  ATotalWidth := AColumnsInfo.PageWidth - Spacing;
  if AColumnsInfo.Columns[0].Width <> Round(ATotalWidth * FirstColumnRelativeWidth) then
    Exit(False);
  if AColumnsInfo.Columns[0].Spacing <> Spacing then
    Exit(False);
  if AColumnsInfo.Columns[1].Width <> Round(ATotalWidth - AColumnsInfo.Columns[0].Width.Value) then
    Exit(False);

  Result := AColumnsInfo.Columns[1].Spacing = 0;
end;

procedure TdxTwoNonUniformColumnsInfoPreset.ApplyTo(AColumnsInfo: TdxColumnsInfoUI);
var
  ATotalWidth: Integer;
begin
  AColumnsInfo.EqualColumnWidth := False;
  AColumnsInfo.ChangeColumnCount(2);

  ATotalWidth := AColumnsInfo.PageWidth - Spacing;
  AColumnsInfo.Columns[0].Width := Round(ATotalWidth * FirstColumnRelativeWidth);
  AColumnsInfo.Columns[0].Spacing := Spacing;
  AColumnsInfo.Columns[1].Width := Round(ATotalWidth - AColumnsInfo.Columns[0].Width.Value);
  AColumnsInfo.Columns[1].Spacing := 0;
end;

{ TdxLeftNarrowColumnsInfoPreset }

function TdxLeftNarrowColumnsInfoPreset.GetFirstColumnRelativeWidth: Single;
begin
  Result := 0.292;
end;

{ TdxRightNarrowColumnsInfoPreset }

function TdxRightNarrowColumnsInfoPreset.GetFirstColumnRelativeWidth: Single;
begin
  Result := 0.708;
end;

{ TdxFloatingInlineObjectParameters }

constructor TdxFloatingInlineObjectParameters.Create(AInlinePictureRun: TdxInlinePictureRun);
begin
  FInlinePictureRun := AInlinePictureRun;
  FFloatingObjectAnchorRun := nil;
end;

constructor TdxFloatingInlineObjectParameters.Create(AFloatingObjectAnchorRun: TdxFloatingObjectAnchorRun);
begin
  FFloatingObjectAnchorRun := AFloatingObjectAnchorRun;
  FInlinePictureRun := nil;
end;

{ TdxPageSetupInfo }

constructor TdxPageSetupInfo.Create;
begin
  inherited Create;
  FAvailableApplyTypes := [TdxSectionPropertiesApplyType.CurrentSection,
    TdxSectionPropertiesApplyType.SelectedSections, TdxSectionPropertiesApplyType.WholeDocument];
end;

function TdxPageSetupInfo.Clone: TdxPageSetupInfo;
begin
  Result := TdxPageSetupInfo(inherited Clone);
end;

procedure TdxPageSetupInfo.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxPageSetupInfo absolute Source;
begin
  ApplyType := AValue.ApplyType;
  AvailableApplyType := AValue.AvailableApplyType;

  LeftMargin := AValue.LeftMargin;
  RightMargin := AValue.RightMargin;
  TopMargin := AValue.TopMargin;
  BottomMargin := AValue.BottomMargin;

  PaperWidth := AValue.PaperWidth;
  PaperHeight := AValue.PaperHeight;
  PaperKind := AValue.PaperKind;
  Landscape := AValue.Landscape;

  SectionStartType := AValue.SectionStartType;
  DifferentFirstPage := AValue.DifferentFirstPage;
  DifferentOddAndEvenPages := AValue.DifferentOddAndEvenPages;
end;

{ TdxMergeRecordsParameters }

constructor TdxMergeRecordsParameters.Create(AMergeRecords: TdxMergeRecords; AMergeDestination: TdxMergeDestination);
begin
  FMergeRecords := AMergeRecords;
  FMergeDestination := AMergeDestination;
end;

end.
