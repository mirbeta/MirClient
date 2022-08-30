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

unit dxSpreadSheetAutoFilling;

{$I cxVer.Inc}

interface

uses
  Types, Windows, SysUtils, Generics.Defaults, Generics.Collections, cxClasses, dxHashUtils,
  dxSpreadSheetCore, dxSpreadSheetCoreHelpers, dxSpreadSheetCoreHistory;

type

  { TdxSpreadSheetAutoFillingCellInfo }

  TdxSpreadSheetAutoFillingDataType = (afdtText, afdtTextWithNumbers, afdtDateTime, afdtNumeric, afdtFormula);

  TdxSpreadSheetAutoFillingCellInfo = record
    DataType: TdxSpreadSheetAutoFillingDataType;
    PatternPrefixPart: string;
    PatternSuffixPart: string;

    constructor Create(ADataType: TdxSpreadSheetAutoFillingDataType); overload;
    constructor Create(const S: string; AStart, AFinish: Integer); overload;
    function Compare(const AInfo: TdxSpreadSheetAutoFillingCellInfo): Boolean;
    procedure Reset;
  end;

  { TdxSpreadSheetHistoryAutoFillAction }

  TdxSpreadSheetHistoryAutoFillAction = class(TdxSpreadSheetHistoryAction)
  protected
    class function GetDescription: string; override;
  end;

  { TdxSpreadSheetAutoFillingPattern }

  TdxSpreadSheetAutoFillingPattern = class
  public
    procedure CalculateValue(ACell: TdxSpreadSheetCell); virtual; abstract;
    function IsLoner: Boolean; virtual;
  end;

  { TdxSpreadSheetAutoFillingPatternNumericSequence }

  TdxSpreadSheetAutoFillingPatternNumericSequence = class(TdxSpreadSheetAutoFillingPattern)
  protected
    FIndex: Integer;
    FReversed: Boolean;
    FSumSquareX: Double;
    FSumX: Double;
    FSumXY: Double;
    FSumY: Double;

    procedure AccumulateValue(const AValue: Double); inline;
    function ExtractValue(ACell: TdxSpreadSheetCell): Double; virtual;
    function GenerateValue: Double;
  public
    constructor Create(AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection);
    procedure CalculateValue(ACell: TdxSpreadSheetCell); override;
    function IsLoner: Boolean; override;
  end;

  { TdxSpreadSheetAutoFillingPatternNumericInTextSequence }

  TdxSpreadSheetAutoFillingPatternNumericInTextSequence = class(TdxSpreadSheetAutoFillingPatternNumericSequence)
  protected
    FIsQuarterTemplate: Boolean;
    FPatternPrefixPart: string;
    FPatternPrefixPartLength: Integer;
    FPatternSuffixPart: string;
    FPatternSuffixPartLength: Integer;

    function CheckIsQuarterTemplate: Boolean;
    function ExtractValue(ACell: TdxSpreadSheetCell): Double; override;
  public
    constructor Create(AView: TdxSpreadSheetTableView; const ASourceArea: TRect;
      ADirection: TcxDirection; const AInfo: TdxSpreadSheetAutoFillingCellInfo);
    procedure CalculateValue(ACell: TdxSpreadSheetCell); override;
    function IsLoner: Boolean; override;
  end;

  { TdxSpreadSheetAutoFillingPatternDateTimeSequence }

  TdxSpreadSheetAutoFillingPatternDateTimeSequence = class(TdxSpreadSheetAutoFillingPattern)
  protected type

    TDateTimePart = (dtpDate, dtpTime);
    TDateTimeParts = set of TDateTimePart;

    TDistance = record
      Days: Integer;
      MilliSeconds: Int64;
      Months: Integer;

      procedure Combine(const ASource: TDistance);
      function IsAssigned: Boolean;
    end;

  protected
    FDistance: TDistance;
    FIndex: Integer;
    FInitialDate: TDateTime;

    procedure CalculateDistance(AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection); virtual;
    function GetDateTime(ACell: TdxSpreadSheetCell): TDateTime; virtual;
    function GetDateTimeParts(ACell: TdxSpreadSheetCell): TDateTimeParts;
    function GetDistance(const AValue1, AValue2: TDateTime): TDistance; virtual;
  public
    constructor Create(AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection);
    procedure CalculateValue(ACell: TdxSpreadSheetCell); override;
  end;

  { TdxSpreadSheetAutoFillingRange }

  TdxSpreadSheetAutoFillingRange = class
  protected
    FArea: TRect;
    FPattern: TdxSpreadSheetAutoFillingPattern;
  public
    constructor Create(const AArea: TRect; APattern: TdxSpreadSheetAutoFillingPattern); virtual;
    destructor Destroy; override;
    procedure ApplyTo(ACell: TdxSpreadSheetCell; const APatternPosition: TPoint); virtual;
    //
    property Area: TRect read FArea;
    property Pattern: TdxSpreadSheetAutoFillingPattern read FPattern;
  end;

  { TdxSpreadSheetAutoFillingRanges }

  TdxSpreadSheetAutoFillingRanges = class
  strict private
    FDirection: TcxDirection;
    FList: TObjectList<TdxSpreadSheetAutoFillingRange>;
    FSourceArea: TRect;
    FSourceAreaIsSingleCellPattern: Boolean;
    FView: TdxSpreadSheetTableView;
  protected
    procedure Add(const AArea: TRect); overload;
    procedure Add(const AColumn, ARow: Integer); overload;
    procedure Add(const AFirstCellInRange, ALastCellInRange: TPoint); overload;
    procedure ValidatePatterns(ARangeStartIndex, ARangeFinishInteger: Integer; AIsMultipleSourceArea: Boolean);

    function CreatePattern(const AArea: TRect): TdxSpreadSheetAutoFillingPattern; virtual;
    function CreateRange(const AArea: TRect; APattern: TdxSpreadSheetAutoFillingPattern): TdxSpreadSheetAutoFillingRange; virtual;

    function FindRange(const P: TPoint; out ARange: TdxSpreadSheetAutoFillingRange): Boolean;
    function GetCellInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetAutoFillingCellInfo; virtual;
    function GetCellPlacementInPattern(ACell: TdxSpreadSheetCell): TPoint; virtual;
    function IsSingleCell(ARange: TdxSpreadSheetAutoFillingRange): Boolean;

    procedure SplitArea; virtual;
    procedure SplitAreaProcessCell(ARow, AColumn: Integer;
      var AFirstCellInRange, ALastCellInRange: TPoint;
      var APrevCellInfo: TdxSpreadSheetAutoFillingCellInfo); virtual;
  public
    constructor Create(AView: TdxSpreadSheetTableView);
    destructor Destroy; override;
    procedure ApplyTo(ACell: TdxSpreadSheetCell);
    procedure Initialize(const ASourceArea: TRect; ADirection: TcxDirection); virtual;
    //
    property Direction: TcxDirection read FDirection;
    property SourceArea: TRect read FSourceArea;
    property SourceAreaIsSingleCellPattern: Boolean read FSourceAreaIsSingleCellPattern;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetTableViewAutoFillingHelper }

  TdxSpreadSheetTableViewAutoFillingHelper = class(TdxSpreadSheetCustomTableViewHelper)
  protected
    // History
    procedure StartAction; override;
    procedure EndAction; override;
    // Filling
    function CalculateFillingArea(const ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer): TRect;
    function CreateRangeList: TdxSpreadSheetAutoFillingRanges; virtual;
    procedure FillCore(const ATargetArea: TRect; ADirection: TcxDirection; ARanges: TdxSpreadSheetAutoFillingRanges);
  public
    procedure Fill(ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer);
  end;

  { TdxSpreadSheetTableViewFillCellsHelper }

  TdxSpreadSheetTableViewFillCellsHelper = class(TdxSpreadSheetCustomTableViewHelper)
  strict private
    procedure PrepareAreasForFilling(const ASourceArea: TRect; AToDown: Boolean; var AMasterArea, ADestArea: TRect);
  public
    procedure FillCells(AToDown: Boolean); overload;
    procedure FillCells(const AArea: TRect; AToDown: Boolean); overload;
  end;

implementation

uses
  DateUtils, Math, cxDateUtils, dxCore, dxCoreClasses, cxGeometry, dxTypeHelpers,
  dxSpreadSheetClipboard, dxSpreadSheetClipboardFormats, dxSpreadSheetStrs, dxSpreadSheetTypes, dxSpreadSheetUtils,
  dxSpreadSheetNumberFormat, dxSpreadSheetStyles, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

function GetDigitsPlaceholder(const S: string; out AStart, AFinish: Integer): Boolean;

  function Check(AStartIndex, AFinishIndex: Integer; out AStart, AFinish: Integer): Boolean;
  var
    ADelta: Integer;
  begin
    ADelta := Sign(AFinishIndex - AStartIndex);
    while (AStartIndex <> AFinishIndex) and dxWideIsSpace(S[AStartIndex]) do
      Inc(AStartIndex, ADelta);
    Result := dxWideIsNumeric(S[AStartIndex]);
    if Result then
    begin
      AStart := AStartIndex;
      while (AStartIndex <> AFinishIndex) and dxWideIsNumeric(S[AStartIndex]) do
        Inc(AStartIndex, ADelta);
      AFinish := AStartIndex;
      if (AStartIndex <> AFinishIndex) or not dxWideIsNumeric(S[AStartIndex]) then
        Dec(AFinish, ADelta);
      if AFinish < AStart then
        ExchangeLongWords(AStart, AFinish);
    end;
  end;

begin
  Result := (S <> '') and (
    Check(1, Length(S), AStart, AFinish) and ((AFinish = Length(S)) or dxWideIsSpace(S[AFinish + 1])) or
    Check(Length(S), 1, AStart, AFinish));
end;

{ TdxSpreadSheetAutoFillingCellInfo }

constructor TdxSpreadSheetAutoFillingCellInfo.Create(ADataType: TdxSpreadSheetAutoFillingDataType);
begin
  DataType := ADataType;
end;

constructor TdxSpreadSheetAutoFillingCellInfo.Create(const S: string; AStart, AFinish: Integer);
begin
  Create(afdtTextWithNumbers);
  PatternPrefixPart := Copy(S, 1, AStart - 1);
  PatternSuffixPart := Copy(S, AFinish + 1, MaxInt);
end;

function TdxSpreadSheetAutoFillingCellInfo.Compare(const AInfo: TdxSpreadSheetAutoFillingCellInfo): Boolean;
begin
  Result := DataType = AInfo.DataType;
  if Result and (DataType = afdtTextWithNumbers) then
    Result := (PatternPrefixPart = AInfo.PatternPrefixPart) and (PatternSuffixPart = AInfo.PatternSuffixPart);
end;

procedure TdxSpreadSheetAutoFillingCellInfo.Reset;
begin
  DataType := afdtText;
  PatternPrefixPart := '';
  PatternSuffixPart := '';
end;

{ TdxSpreadSheetHistoryAutoFillAction }

class function TdxSpreadSheetHistoryAutoFillAction.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxActionAutoFill);
end;

{ TdxSpreadSheetAutoFillingPattern }

function TdxSpreadSheetAutoFillingPattern.IsLoner: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetAutoFillingPatternNumericSequence }

constructor TdxSpreadSheetAutoFillingPatternNumericSequence.Create(
  AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection);
begin
  inherited Create;
  FReversed := ADirection in [dirLeft, dirUp];

  AView.ForEachCell(ASourceArea,
    procedure (ACell: TdxSpreadSheetCell)
    begin
      AccumulateValue(ExtractValue(ACell));
    end,
    ADirection in [dirRight, dirDown]);
end;

procedure TdxSpreadSheetAutoFillingPatternNumericSequence.CalculateValue(ACell: TdxSpreadSheetCell);
begin
  ACell.AsFloat := GenerateValue;
end;

procedure TdxSpreadSheetAutoFillingPatternNumericSequence.AccumulateValue(const AValue: Double);
begin
  FSumX := FSumX + FIndex;
  FSumY := FSumY + AValue;
  FSumXY := FSumXY + FIndex * AValue;
  FSumSquareX := FSumSquareX + Power(FIndex, 2);
  Inc(FIndex);
end;

function TdxSpreadSheetAutoFillingPatternNumericSequence.ExtractValue(ACell: TdxSpreadSheetCell): Double;
begin
  Result := ACell.AsFloat;
end;

function TdxSpreadSheetAutoFillingPatternNumericSequence.GenerateValue: Double;
var
  AFactorA: Double;
begin
  AFactorA := FIndex * FSumSquareX - Power(FSumX, 2);
  if IsZero(AFactorA) then
    AFactorA := ValueIncr[not FReversed]
  else
    AFactorA := (FIndex * FSumXY - FSumX * FSumY) / AFactorA;

  Result := AFactorA * FIndex + (FSumY - AFactorA * FSumX) / FIndex;
  AccumulateValue(Result);
end;

function TdxSpreadSheetAutoFillingPatternNumericSequence.IsLoner: Boolean;
begin
  Result := False;
end;

{ TdxSpreadSheetAutoFillingPatternNumericInTextSequence }

constructor TdxSpreadSheetAutoFillingPatternNumericInTextSequence.Create(AView: TdxSpreadSheetTableView;
  const ASourceArea: TRect; ADirection: TcxDirection; const AInfo: TdxSpreadSheetAutoFillingCellInfo);
begin
  FPatternPrefixPart := AInfo.PatternPrefixPart;
  FPatternPrefixPartLength := Length(FPatternPrefixPart);
  FPatternSuffixPart := AInfo.PatternSuffixPart;
  FPatternSuffixPartLength := Length(FPatternSuffixPart);
  FIsQuarterTemplate := CheckIsQuarterTemplate;
  inherited Create(AView, ASourceArea, ADirection);
end;

procedure TdxSpreadSheetAutoFillingPatternNumericInTextSequence.CalculateValue(ACell: TdxSpreadSheetCell);
var
  AValue: Double;
  AValueAsInt: Integer;
begin
  AValue := Abs(GenerateValue);
  AValueAsInt := Trunc(AValue);
  if IsZero(AValue - AValueAsInt) then
  begin
    if FIsQuarterTemplate and (AValueAsInt > 0) then
      AValueAsInt := (AValueAsInt - 1) mod 4 + 1;
    ACell.AsString := Format('%s%d%s', [FPatternPrefixPart, AValueAsInt, FPatternSuffixPart]);
  end;
end;

function TdxSpreadSheetAutoFillingPatternNumericInTextSequence.CheckIsQuarterTemplate: Boolean;

  function Check(const S: string): Boolean;
  begin
    Result := (S <> '') and (
      SameText(cxGetResourceString(@sdxQuarter), S) or
      SameText(cxGetResourceString(@sdxQuarterAbbreviation1), S) or
      SameText(cxGetResourceString(@sdxQuarterAbbreviation2), S));
  end;

begin
  Result := Check(Trim(FPatternPrefixPart)) or Check(Trim(FPatternSuffixPart));
end;

function TdxSpreadSheetAutoFillingPatternNumericInTextSequence.ExtractValue(ACell: TdxSpreadSheetCell): Double;
var
  AValue: string;
begin
  AValue := ACell.AsString;
  AValue := Copy(AValue, FPatternPrefixPartLength + 1, Length(AValue) - FPatternSuffixPartLength - FPatternPrefixPartLength);
  Result := StrToIntDef(AValue, 0);
  FIsQuarterTemplate := FIsQuarterTemplate and InRange(Result, 1, 4);
end;

function TdxSpreadSheetAutoFillingPatternNumericInTextSequence.IsLoner: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetAutoFillingPatternDateTimeSequence }

constructor TdxSpreadSheetAutoFillingPatternDateTimeSequence.Create(
  AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection);
begin
  inherited Create;
  CalculateDistance(AView, ASourceArea, ADirection);
end;

procedure TdxSpreadSheetAutoFillingPatternDateTimeSequence.CalculateValue(ACell: TdxSpreadSheetCell);
var
  AValue: Integer;
  Y, M, D: Word;
begin
  if FDistance.IsAssigned then
  begin
    DecodeDate(FInitialDate, Y, M, D);

    if FDistance.Months <> 0 then
    begin
      AValue := Integer(M) + FIndex * FDistance.Months;
      while AValue > MonthsPerYear do
      begin
        Dec(AValue, MonthsPerYear);
        Inc(Y);
      end;
      M := AValue;
      D := Min(D, DaysPerMonth(Y, M));
    end
    else
      if FDistance.Days <> 0 then
      begin
        AValue := Integer(D) + FIndex * FDistance.Days;
        while AValue > DaysPerMonth(Y, M) do
        begin
          Dec(AValue, DaysPerMonth(Y, M));
          Inc(M);
          if M > MonthsPerYear then
          begin
            Dec(M, MonthsPerYear);
            Inc(Y);
          end;
        end;
        D := AValue;
      end;

    ACell.AsDateTime := EncodeDate(Y, M, D) + TimeOf(FInitialDate) + FIndex * FDistance.MilliSeconds / MSecsPerDay;
    Inc(FIndex);
  end;
end;

procedure TdxSpreadSheetAutoFillingPatternDateTimeSequence.CalculateDistance(
  AView: TdxSpreadSheetTableView; const ASourceArea: TRect; ADirection: TcxDirection);
type
  TDistanceState = (dsNone, dsIrregular, dsAssigned);
var
  ADistanceState: TDistanceState;
  APrevCell: TdxSpreadSheetCell;
begin
  APrevCell := nil;
  ADistanceState := dsNone;

  AView.ForEachCell(ASourceArea,
    procedure (ACell: TdxSpreadSheetCell)
    var
      ADistance: TDistance;
    begin
      if APrevCell = nil then
        FInitialDate := ACell.AsDateTime;
      if (APrevCell <> nil) and (ADistanceState <> dsIrregular) then
      begin
        ADistance := GetDistance(GetDateTime(APrevCell), GetDateTime(ACell));
        if ADistanceState = dsAssigned then
          FDistance.Combine(ADistance)
        else
          FDistance := ADistance;

        if FDistance.IsAssigned then
          ADistanceState := dsAssigned
        else
          ADistanceState := dsIrregular;
      end;
      APrevCell := ACell;
      Inc(FIndex);
    end,
    ADirection in [dirRight, dirDown]);

  if ADistanceState = dsNone then
  begin
    if dtpDate in GetDateTimeParts(AView.Cells[ASourceArea.Top, ASourceArea.Left]) then
      FDistance.Days := 1
    else
      FDistance.MilliSeconds := MSecsPerSec * SecsPerHour;
  end;
end;

function TdxSpreadSheetAutoFillingPatternDateTimeSequence.GetDateTime(ACell: TdxSpreadSheetCell): TDateTime;
begin
  Result := ACell.AsDateTime;
  if not (dtpTime in GetDateTimeParts(ACell)) then
    Result := DateOf(Result);
end;

function TdxSpreadSheetAutoFillingPatternDateTimeSequence.GetDateTimeParts(ACell: TdxSpreadSheetCell): TDateTimeParts;

  function GetFormattedValue(const AValue: TDateTime): string;
  var
    AResult: TdxSpreadSheetNumberFormatResult;
  begin
    ACell.Style.DataFormat.Format(AValue, cdtDateTime, AResult);
    Result := AResult.Text;
  end;

begin
  Result := [];
  if GetFormattedValue(EncodeDate(2000, 1, 1)) <> GetFormattedValue(EncodeDate(2001, 2, 2)) then
    Include(Result, dtpDate);
  if GetFormattedValue(EncodeTime(1, 1, 1, 1)) <> GetFormattedValue(EncodeTime(2, 2, 2, 2)) then
    Include(Result, dtpTime);
end;

function TdxSpreadSheetAutoFillingPatternDateTimeSequence.GetDistance(const AValue1, AValue2: TDateTime): TDistance;
var
  AYear1, AMonth1, ADay1: Word;
  AYear2, AMonth2, ADay2: Word;
begin
  Result.MilliSeconds := Sign(AValue2 - AValue1) * MilliSecondsBetween(TimeOf(AValue2), TimeOf(AValue1));
  Result.Days := Trunc(AValue2) - Trunc(AValue1);

  DecodeDate(AValue1, AYear1, AMonth1, ADay1);
  DecodeDate(AValue2, AYear2, AMonth2, ADay2);
  if (ADay1 = ADay2) or (ADay1 = DaysInAMonth(AYear1, AMonth1)) and (ADay2 = DaysInAMonth(AYear2, AMonth2)) then
    Result.Months := (AYear2 - AYear1) * 12 + (AMonth2 - AMonth1)
  else
    Result.Months := 0;
end;

{ TdxSpreadSheetAutoFillingPatternDateTimeSequence.TDistance }

procedure TdxSpreadSheetAutoFillingPatternDateTimeSequence.TDistance.Combine(const ASource: TDistance);
begin
  if ASource.Days <> Days then
    Days := 0;
  if ASource.Months <> Months then
    Months := 0;
  if ASource.MilliSeconds <> MilliSeconds then
    MilliSeconds := 0;
end;

function TdxSpreadSheetAutoFillingPatternDateTimeSequence.TDistance.IsAssigned: Boolean;
begin
  Result := (Days > 0) or (MilliSeconds > 0);
end;

{ TdxSpreadSheetAutoFillingRange }

constructor TdxSpreadSheetAutoFillingRange.Create(const AArea: TRect; APattern: TdxSpreadSheetAutoFillingPattern);
begin
  inherited Create;
  FArea := AArea;
  FPattern := APattern;
end;

destructor TdxSpreadSheetAutoFillingRange.Destroy;
begin
  FreeAndNil(FPattern);
  inherited Destroy;
end;

procedure TdxSpreadSheetAutoFillingRange.ApplyTo(ACell: TdxSpreadSheetCell; const APatternPosition: TPoint);
var
  ASourceCell: TdxSpreadSheetCell;
  ASourceCellArea: TRect;
begin
  ASourceCell := ACell.View.Cells[APatternPosition.Y, APatternPosition.X];
  if ASourceCell <> nil then
    ACell.Assign(ASourceCell)
  else
  begin
    ACell.Clear;
    ACell.Style.Merge(ACell.Column.Style, ACell.Row.Style);
  end;

  ASourceCellArea := ACell.View.MergedCells.ExpandArea(APatternPosition.X, APatternPosition.Y);
  if not dxSpreadSheetIsSingleCellArea(ASourceCellArea) and cxPointIsEqual(ASourceCellArea.TopLeft, APatternPosition) then
    ACell.View.MergedCells.Add(cxRectSetOrigin(ASourceCellArea, Point(ACell.ColumnIndex, ACell.RowIndex)));

  if Pattern <> nil then
    Pattern.CalculateValue(ACell);
end;

{ TdxSpreadSheetAutoFillingRanges }

constructor TdxSpreadSheetAutoFillingRanges.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create;
  FView := AView;
  FList := TObjectList<TdxSpreadSheetAutoFillingRange>.Create;
end;

destructor TdxSpreadSheetAutoFillingRanges.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxSpreadSheetAutoFillingRanges.ApplyTo(ACell: TdxSpreadSheetCell);
var
  ARange: TdxSpreadSheetAutoFillingRange;
  APosition: TPoint;
begin
  APosition := GetCellPlacementInPattern(ACell);
  if FindRange(APosition, ARange) then
    ARange.ApplyTo(ACell, APosition);
end;

procedure TdxSpreadSheetAutoFillingRanges.Initialize(const ASourceArea: TRect; ADirection: TcxDirection);
begin
  FDirection := ADirection;
  FSourceArea := ASourceArea;
  SplitArea;
end;

function TdxSpreadSheetAutoFillingRanges.CreatePattern(const AArea: TRect): TdxSpreadSheetAutoFillingPattern;
var
  AInfo: TdxSpreadSheetAutoFillingCellInfo;
begin
  Result := nil;
  AInfo := GetCellInfo(View.Cells[AArea.Top, AArea.Left]);
  case AInfo.DataType of
    afdtDateTime:
      Result := TdxSpreadSheetAutoFillingPatternDateTimeSequence.Create(View, AArea, Direction);
    afdtTextWithNumbers:
      Result := TdxSpreadSheetAutoFillingPatternNumericInTextSequence.Create(View, AArea, Direction, AInfo);
    afdtNumeric:
      Result := TdxSpreadSheetAutoFillingPatternNumericSequence.Create(View, AArea, Direction);
  end;
end;

function TdxSpreadSheetAutoFillingRanges.CreateRange(
  const AArea: TRect; APattern: TdxSpreadSheetAutoFillingPattern): TdxSpreadSheetAutoFillingRange;
begin
  Result := TdxSpreadSheetAutoFillingRange.Create(AArea, APattern);
end;

function TdxSpreadSheetAutoFillingRanges.FindRange(
  const P: TPoint; out ARange: TdxSpreadSheetAutoFillingRange): Boolean;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if dxSpreadSheetContains(FList[I].Area, P.Y, P.X) then
    begin
      ARange := FList[I];
      Exit(True);
    end;

  Result := False;
end;

procedure TdxSpreadSheetAutoFillingRanges.Add(const AArea: TRect);
begin
  if dxSpreadSheetIsValidArea(AArea) then
    FList.Add(CreateRange(AArea, CreatePattern(AArea)));
end;

procedure TdxSpreadSheetAutoFillingRanges.Add(const AColumn, ARow: Integer);
begin
  Add(cxRect(AColumn, ARow, AColumn, ARow));
end;

procedure TdxSpreadSheetAutoFillingRanges.Add(const AFirstCellInRange, ALastCellInRange: TPoint);
begin
  Add(cxRect(AFirstCellInRange, ALastCellInRange));
end;

procedure TdxSpreadSheetAutoFillingRanges.ValidatePatterns(
  ARangeStartIndex, ARangeFinishInteger: Integer; AIsMultipleSourceArea: Boolean);

  function HasSimilarPatternsInArea(APattern: TdxSpreadSheetAutoFillingPattern): Boolean;
  var
    ARange: TdxSpreadSheetAutoFillingRange;
    I: Integer;
  begin
    for I := ARangeStartIndex to ARangeFinishInteger do
    begin
      ARange := FList[I];
      if (ARange.Pattern <> nil) and (ARange.Pattern <> APattern) and APattern.InheritsFrom(ARange.Pattern.ClassType) then
        Exit(True);
    end;
    Result := False;
  end;

  function ShouldDeletePatternOfRange(ARange: TdxSpreadSheetAutoFillingRange): Boolean;
  begin
    Result := (ARange.Pattern <> nil) and not ARange.Pattern.IsLoner and IsSingleCell(ARange);
  end;

var
  AList: TList<TdxSpreadSheetAutoFillingRange>;
  ARange: TdxSpreadSheetAutoFillingRange;
  I: Integer;
begin
  if AIsMultipleSourceArea then
  begin
    AList := TList<TdxSpreadSheetAutoFillingRange>.Create;
    try
      for I := ARangeStartIndex to ARangeFinishInteger do
      begin
        ARange := FList[I];
        if ShouldDeletePatternOfRange(ARange) and not HasSimilarPatternsInArea(ARange.Pattern) then
          AList.Add(ARange);
      end;
      for I := 0 to AList.Count - 1 do
        FreeAndNil(AList[I].FPattern);
    finally
      AList.Free;
    end;
  end
  else
    if ARangeStartIndex = ARangeFinishInteger then
    begin
      ARange := FList[ARangeStartIndex];
      if ShouldDeletePatternOfRange(ARange) then
        FreeAndNil(ARange.FPattern);
    end;
end;

function TdxSpreadSheetAutoFillingRanges.GetCellInfo(ACell: TdxSpreadSheetCell): TdxSpreadSheetAutoFillingCellInfo;
const
  NumericDataTypes = [cdtCurrency, cdtFloat, cdtInteger];
var
  AStart, AFinish: Integer;
begin
  if ACell = nil then
    Exit(TdxSpreadSheetAutoFillingCellInfo.Create(afdtText));

  if ACell.IsFormula then
    Result := TdxSpreadSheetAutoFillingCellInfo.Create(afdtFormula)
  else
    if (ACell.DataType = cdtDateTime) or (ACell.DataType in NumericDataTypes) and ACell.Style.DataFormat.IsDateTime then
      Result := TdxSpreadSheetAutoFillingCellInfo.Create(afdtDateTime)
    else
      if ACell.DataType in NumericDataTypes then
        Result := TdxSpreadSheetAutoFillingCellInfo.Create(afdtNumeric)
      else
        if (ACell.DataType = cdtString) and GetDigitsPlaceholder(ACell.AsString, AStart, AFinish) then
          Result := TdxSpreadSheetAutoFillingCellInfo.Create(ACell.AsString, AStart, AFinish)
        else
          Result := TdxSpreadSheetAutoFillingCellInfo.Create(afdtText);
end;

function TdxSpreadSheetAutoFillingRanges.GetCellPlacementInPattern(ACell: TdxSpreadSheetCell): TPoint;
begin
  Result.Y := ACell.RowIndex;
  while Result.Y < FSourceArea.Top do
    Inc(Result.Y, dxSpreadSheetAreaHeight(FSourceArea));
  while Result.Y > FSourceArea.Bottom do
    Dec(Result.Y, dxSpreadSheetAreaHeight(FSourceArea));

  Result.X := ACell.ColumnIndex;
  while Result.X < FSourceArea.Left do
    Inc(Result.X, dxSpreadSheetAreaWidth(FSourceArea));
  while Result.X > FSourceArea.Right do
    Dec(Result.X, dxSpreadSheetAreaWidth(FSourceArea));
end;

function TdxSpreadSheetAutoFillingRanges.IsSingleCell(ARange: TdxSpreadSheetAutoFillingRange): Boolean;
var
  R: TRect;
begin
  Result := dxSpreadSheetIsSingleCellArea(ARange.Area);
  if not Result then
  begin
    R := View.MergedCells.ExpandArea(ARange.Area.Left, ARange.Area.Top);
    if Direction in [dirLeft, dirRight] then
      Result := Result or (R.Left = ARange.Area.Left) and (R.Right = ARange.Area.Right)
    else
      Result := Result or (R.Top = ARange.Area.Top) and (R.Bottom = ARange.Area.Bottom);
  end;
end;

procedure TdxSpreadSheetAutoFillingRanges.SplitArea;
var
  AFirstCellInRange: TPoint;
  ALastCellInRange: TPoint;
  APrevCellInfo: TdxSpreadSheetAutoFillingCellInfo;
  ARow, AColumn: Integer;
  AStartIndex: Integer;
begin
  APrevCellInfo.Reset;
  ALastCellInRange := cxInvalidPoint;
  AFirstCellInRange := cxInvalidPoint;

  if Direction in [dirLeft, dirRight] then
  begin
    for ARow := SourceArea.Top to SourceArea.Bottom do
    begin
      AStartIndex := FList.Count;
      AColumn := SourceArea.Left;
      while AColumn <= SourceArea.Right do
      begin
        SplitAreaProcessCell(ARow, AColumn, AFirstCellInRange, ALastCellInRange, APrevCellInfo);
        AColumn := View.MergedCells.ExpandArea(AColumn, ARow).Right + 1;
      end;
      Add(AFirstCellInRange, ALastCellInRange);
      ValidatePatterns(AStartIndex, FList.Count - 1, dxSpreadSheetAreaHeight(SourceArea) > 1);
      AFirstCellInRange := cxInvalidPoint;
    end;
  end
  else
    for AColumn := SourceArea.Left to SourceArea.Right do
    begin
      AStartIndex := FList.Count;
      ARow := SourceArea.Top;
      while ARow <= SourceArea.Bottom do
      begin
        SplitAreaProcessCell(ARow, AColumn, AFirstCellInRange, ALastCellInRange, APrevCellInfo);
        ARow := View.MergedCells.ExpandArea(AColumn, ARow).Bottom + 1;
      end;
      Add(AFirstCellInRange, ALastCellInRange);
      ValidatePatterns(AStartIndex, FList.Count - 1, dxSpreadSheetAreaWidth(SourceArea) > 1);
      AFirstCellInRange := cxInvalidPoint;
    end;
end;

procedure TdxSpreadSheetAutoFillingRanges.SplitAreaProcessCell(ARow, AColumn: Integer;
  var AFirstCellInRange, ALastCellInRange: TPoint; var APrevCellInfo: TdxSpreadSheetAutoFillingCellInfo);

  function IsValid(const P: TPoint): Boolean;
  begin
    Result := not cxPointIsEqual(P, cxInvalidPoint);
  end;

  function MustSplit(const AInfo: TdxSpreadSheetAutoFillingCellInfo): Boolean;
  begin
    Result := (AInfo.DataType in [afdtText, afdtFormula]) or IsValid(AFirstCellInRange) and not APrevCellInfo.Compare(AInfo);
  end;

var
  ACellInfo: TdxSpreadSheetAutoFillingCellInfo;
begin
  ACellInfo := GetCellInfo(View.Cells[ARow, AColumn]);
  if MustSplit(ACellInfo) then
  begin
    Add(AFirstCellInRange, ALastCellInRange);
    Add(AColumn, ARow);
    AFirstCellInRange := cxInvalidPoint;
  end
  else
  begin
    ALastCellInRange := cxPoint(AColumn, ARow);
    if not IsValid(AFirstCellInRange) then
      AFirstCellInRange := ALastCellInRange;
  end;
  APrevCellInfo := ACellInfo;
end;

{ TdxSpreadSheetTableViewAutoFillingHelper }

procedure TdxSpreadSheetTableViewAutoFillingHelper.Fill(ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer);
var
  ARanges: TdxSpreadSheetAutoFillingRanges;
  ATargetArea: TRect;
begin
  ASourceArea.Right := Min(ASourceArea.Right, View.Dimensions.Right);
  ASourceArea.Bottom := Min(ASourceArea.Bottom, View.Dimensions.Bottom);
  if (ADirection <> dirNone) and (ACount > 0) and dxSpreadSheetIsValidArea(ASourceArea) then
  begin
    ATargetArea := CalculateFillingArea(ASourceArea, ADirection, ACount);
    CheckProtection(ATargetArea);

    ARanges := CreateRangeList;
    try
      ARanges.Initialize(ASourceArea, ADirection);
      FillCore(ATargetArea, ADirection, ARanges);
    finally
      ARanges.Free;
    end;

    TdxSpreadSheetTableViewAccess(View).Pack;
  end;
end;

procedure TdxSpreadSheetTableViewAutoFillingHelper.StartAction;
begin
  History.BeginAction(TdxSpreadSheetHistoryAutoFillAction);
end;

procedure TdxSpreadSheetTableViewAutoFillingHelper.EndAction;
begin
  History.EndAction;
end;

function TdxSpreadSheetTableViewAutoFillingHelper.CalculateFillingArea(
  const ASourceArea: TRect; ADirection: TcxDirection; ACount: Integer): TRect;
begin
  Result := ASourceArea;
  case ADirection of
    dirLeft:
      Result := cxRectSetRight(Result, ASourceArea.Left - 1, ACount - 1);
    dirRight:
      Result := cxRectSetLeft(Result, ASourceArea.Right + 1, ACount - 1);
    dirDown:
      Result := cxRectSetTop(Result, ASourceArea.Bottom + 1, ACount - 1);
    dirUp:
      Result := cxRectSetBottom(Result, ASourceArea.Top - 1, ACount - 1);
  end;
end;

function TdxSpreadSheetTableViewAutoFillingHelper.CreateRangeList: TdxSpreadSheetAutoFillingRanges;
begin
  Result := TdxSpreadSheetAutoFillingRanges.Create(View);
end;

procedure TdxSpreadSheetTableViewAutoFillingHelper.FillCore(
  const ATargetArea: TRect; ADirection: TcxDirection; ARanges: TdxSpreadSheetAutoFillingRanges);
var
  APosition, I: Integer;
begin
  if ADirection in [dirLeft, dirRight] then
  begin
    APosition := IfThen(ADirection = dirLeft, ATargetArea.Right, ATargetArea.Left);
    while InRange(APosition, ATargetArea.Left, ATargetArea.Right) do
    begin
      for I := ATargetArea.Top to ATargetArea.Bottom do
        ARanges.ApplyTo(View.CreateCell(I, APosition));
      Inc(APosition, ValueIncr[ADirection = dirRight]);
    end;
  end
  else
  begin
    APosition := IfThen(ADirection = dirUp, ATargetArea.Bottom, ATargetArea.Top);
    while InRange(APosition, ATargetArea.Top, ATargetArea.Bottom) do
    begin
      for I := ATargetArea.Left to ATargetArea.Right do
        ARanges.ApplyTo(View.CreateCell(APosition, I));
      Inc(APosition, ValueIncr[ADirection = dirDown]);
    end;
  end;
end;

{ TdxSpreadSheetTableViewFillCellsHelper }

procedure TdxSpreadSheetTableViewFillCellsHelper.FillCells(AToDown: Boolean);
var
  ASelection: TdxSpreadSheetTableViewPasteSelection;
  I: Integer;
begin
  CheckProtection;
  View.BeginUpdate;
  try
    ASelection := TdxSpreadSheetTableViewPasteSelection.Create;
    try
      ASelection.Store(View, cxSize(1, 1));
      try
        View.Selection.Clear;
        for I := 0 to ASelection.Count - 1 do
          FillCells(ASelection[I], AToDown);
      finally
        ASelection.Restore(View);
      end;
    finally
      ASelection.Free;
    end;
  finally
    View.EndUpdate;
  end;
end;

procedure TdxSpreadSheetTableViewFillCellsHelper.FillCells(const AArea: TRect; AToDown: Boolean);
var
  AData: IdxSpreadSheetClipboardData;
  AFocusedCell, AScrollPos: TPoint;
  AMasterArea, ADestArea: TRect;
  ASelection: TdxRectList;
  AStartIndex, AEndIndex: Integer;
  I: Integer;
begin
  PrepareAreasForFilling(AArea, AToDown, AMasterArea, ADestArea);
  if dxSpreadSheetIsValidArea(AMasterArea) and dxSpreadSheetIsValidArea(ADestArea) then
  begin
    CheckProtection(ADestArea);
    ASelection := TdxRectList.Create;
    dxSpreadSheetStoreSelectionAndScrollPosition(View, ASelection, AFocusedCell, AScrollPos);
    try
      AData := TdxSpreadSheetBinaryClipboardFormat.Build(AMasterArea, nil, ccmCopy, View);

      History.BeginAction(TdxSpreadSheetHistoryFillCellsAction);
      try
        View.MergedCells.DeleteItemsInArea(ADestArea);
        TdxSpreadSheetTableViewAccess(View).ClearCells(ADestArea, dxSpreadSheetTableViewClearCellsOptionsAll);

        if AToDown then
        begin
          AStartIndex := ADestArea.Top;
          AEndIndex := ADestArea.Bottom;
        end
        else
        begin
          AStartIndex := ADestArea.Left;
          AEndIndex := ADestArea.Right;
        end;

        for I := AStartIndex to AEndIndex do
        begin
          if AToDown then
            View.Selection.SetFocused(I, ADestArea.Left, [])
          else
            View.Selection.SetFocused(ADestArea.Top, I, []);

          AData.Paste(View, View.Selection.Area.TopLeft, dxSpreadSheetDefaultPasteOptions);
        end;
      finally
        History.EndAction;
      end;
    finally
      dxSpreadSheetRestoreSelectionAndScrollPosition(View, ASelection, AFocusedCell, AScrollPos);
      FreeAndNil(ASelection);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewFillCellsHelper.PrepareAreasForFilling(
  const ASourceArea: TRect; AToDown: Boolean; var AMasterArea, ADestArea: TRect);
begin
  ADestArea := ASourceArea;
  AMasterArea := ADestArea;
  if AToDown then
  begin
    AMasterArea.Bottom := AMasterArea.Top;
    if cxRectIsEqual(AMasterArea, ADestArea) then
      AMasterArea := cxRectOffsetVert(AMasterArea, -1)
    else
      Inc(ADestArea.Top);
  end
  else
  begin
    AMasterArea.Right := AMasterArea.Left;
    if cxRectIsEqual(AMasterArea, ADestArea) then
      AMasterArea := cxRectOffsetHorz(AMasterArea, -1)
    else
      Inc(ADestArea.Left);
  end;
end;

end.
