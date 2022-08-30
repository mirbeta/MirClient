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

unit dxSpreadSheetTypes;

{$I cxVer.Inc}

interface

uses
  Windows, Math, Classes, Types, SysUtils, Graphics, Variants, Generics.Collections, Generics.Defaults,
  cxClasses, dxCore, dxCoreClasses;

type
  PFloat = PDouble;
  PObject = ^TObject;

  TdxSpreadSheetMessageType = (ssmtError, ssmtWarning, ssmtHint);
  TdxSpreadSheetMessageTypes = set of TdxSpreadSheetMessageType;

  TdxSpreadSheetCellDataType = (cdtBlank, cdtBoolean, cdtError, cdtCurrency, cdtFloat, cdtDateTime, cdtInteger, cdtString, cdtFormula);

  TdxSpreadSheetClipboardCopyMode = (ccmNone, ccmCopy, ccmCut);
  TdxSpreadSheetClipboardPasteOption = (cpoValues, cpoFormulas, cpoComments, cpoNumberFormatting, cpoStyles, cpoColumnWidths, cpoSkipBlanks);
  TdxSpreadSheetClipboardPasteOptions = set of TdxSpreadSheetClipboardPasteOption;

  TdxSpreadSheetDateTimeSystem = (dts1900, dts1904, dtsDefault);

  TdxSpreadSheetCellState = (csLocked, csHidden, csShrinkToFit, csWordWrap);
  TdxSpreadSheetCellStates = set of TdxSpreadSheetCellState;

  TdxSpreadSheetFontScript = (fsNone, fsSuperscript, fsSubscript);

  TdxSpreadSheetCellsModification = (cmShiftCellsHorizontally, cmShiftCellsVertically, cmShiftColumns, cmShiftRows);
  TdxSpreadSheetCellsModificationMode = (cmmClear, cmmDelete, cmmReplace);

  TdxSpreadSheetEnterKeyNavigation = (eknDefault, eknSkipLockedCells);

  TdxSpreadSheetSizingMarker = (smLeft, smTopLeft, smTop, smTopRight, smRight, smBottomRight, smBottom, smBottomLeft);
  TdxSpreadSheetSizingMarkers = set of TdxSpreadSheetSizingMarker;

  // formulas
  TdxSpreadSheetFormulaErrorCode = (ecNone, ecNull, ecDivByZero, ecValue, ecRefErr, ecName, ecNUM, ecNA);
  TdxSpreadSheetFormulaUpdateReferencesMode = (urmInsert, urmDelete, urmMove);
  TdxSpreadSheetFormulaOperation = (opAdd, opSub, opMul, opDiv, opPower, opConcat, opLT, opLE,
    opEQ, opGE, opGT, opNE, opIsect, opUnion, opRange, opUplus, opUminus, opPercent, opParen);
  TdxSpreadSheetOperationStrings = array[TdxSpreadSheetFormulaOperation] of string;

type
  TdxSpreadSheetVector = class;

  { IdxSpreadSheetCellData }

  IdxSpreadSheetCellData = interface
  ['{B775B17A-375A-47E0-A5BA-7F9A82CC9FB9}']
    function GetAsError: TdxSpreadSheetFormulaErrorCode;
    function GetAsFloat: Double;
    function GetAsFormula: TObject{TdxSpreadSheetCustomFormula};
    function GetAsVariant: Variant;
    function GetDataType: TdxSpreadSheetCellDataType;
    function GetIsEmpty: Boolean;
    //
    function IsNumericValue: Boolean;
    //
    property AsError: TdxSpreadSheetFormulaErrorCode read GetAsError;
    property AsFloat: Double read GetAsFloat;
    property AsFormula: TObject read GetAsFormula;
    property AsVariant: Variant read GetAsVariant;
    property DataType: TdxSpreadSheetCellDataType read GetDataType;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { IdxSpreadSheetViewCaption }

  IdxSpreadSheetViewCaption = interface
  ['{0F1806B1-2C58-4520-8D5E-3AD4BD2EAF94}']
    function GetCaption: string;
    function IsCaptionTextDelimited: Boolean;
  end;

  { IdxSpreadSheetViewData }

  TdxSpreadSheetViewForEachCellProc = reference to procedure (const ACell: IdxSpreadSheetCellData);

  IdxSpreadSheetViewData = interface
  ['{08C7DC62-1EEA-4FD3-9922-CFBD8911A5FB}']
    procedure ForEachCell(const AArea: TRect; AProc: TdxSpreadSheetViewForEachCellProc; AGoForward: Boolean = True);
    function GetCellData(const ARow, AColumn: Integer): IdxSpreadSheetCellData;
    function GetMaxColumnIndex: Integer;
    function GetMaxRowIndex: Integer;
    function GetNextColumnWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function GetNextRowWithNonEmptyCell(const ARow, AColumn: Integer; const AGoForward: Boolean = True): Integer;
    function IsRowVisible(const ARow: Integer): Boolean;
    procedure SetCellData(const ARow, AColumn: Integer; const AValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode);
  end;

  { TdxSpreadSheetCellData }

  TdxSpreadSheetCellData = class
  public
    Column: Integer;
    Row: Integer;
    Value: Variant;
    constructor Create(ARow, AColumn: Integer; const AValue: Variant);
  end;

 { TdxSpreadSheetCellReference }

  PdxSpreadSheetCellReference = ^TdxSpreadSheetCellReference;
  TdxSpreadSheetCellReference = record
    ColumnIndex: Integer;
    RowIndex: Integer;
    View: TObject;
  end;

  { TdxSpreadSheetVectorValue }

  TdxSpreadSheetVectorValue = record
  public
    ErrorCode: TdxSpreadSheetFormulaErrorCode;
    Value: Variant;

    constructor Create(const AValue: Variant; AErrorCode: TdxSpreadSheetFormulaErrorCode);
    function IsError: Boolean; inline;
  end;

  { TdxSpreadSheetVector }

  TdxSpreadSheetVector = class abstract
  protected
    FAnchorItemsIndex: Integer;
    FIsAllItems: Boolean;

    function GetItem(Index: Integer): TdxSpreadSheetVectorValue; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
    function IsEqualValues(V1, V2: Variant; ANeedMaskSearch: Boolean): Boolean;
  public
    function GetFirstZeroValueIndex: Integer; virtual;
    function GetItemIndex(const AValue: Variant; ANeedMaskSearch: Boolean): Integer; virtual;
    function GetNextItemIndex(AIndex: Integer; AForward: Boolean): Integer; virtual;

    property AnchorItemsIndex: Integer read FAnchorItemsIndex;
    property IsAllItems: Boolean read FIsAllItems;
    property Items[Index: Integer]: TdxSpreadSheetVectorValue read GetItem; default;
    property Length: Integer read GetLength;
  end;

  { TdxSpreadSheetSimpleVector }

  TdxSpreadSheetSimpleVector = class(TdxSpreadSheetVector)
  strict private
    FData: TList<TdxSpreadSheetVectorValue>;
  protected
    function GetItem(Index: Integer): TdxSpreadSheetVectorValue; override;
    function GetLength: Integer; override;
  public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy; override;
    procedure Add(const AValue: Variant; AErrorCode: TdxSpreadSheetFormulaErrorCode);
  end;

  { TdxSpreadSheetFormulaTokenDimension }

  TdxSpreadSheetFormulaTokenDimension = record
    ColumnCount: Integer;
    RowCount: Integer;

    class operator Equal(const ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension): Boolean;
    class operator NotEqual(const ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension): Boolean;
    function Count: Integer;
    procedure SetDimension(const ARowCount, AColumnCount: Integer);
  end;

 { TdxSpreadSheetReference }

  TdxSpreadSheetReference = record
    Value: Int64;
    function GetFlags: Integer; inline;
    function GetIsAbsolute: Boolean; inline;
    function GetIsAllItems: Boolean; inline;
    function GetIsError: Boolean; inline;
    function GetOffset: Integer; inline;
    procedure SetFlags(AValue: Integer); inline;
    procedure SetIsAbsolute(AValue: Boolean); inline;
    procedure SetIsAllItems(AValue: Boolean); inline;
    procedure SetIsError(AValue: Boolean); inline;
    procedure SetOffset(AValue: Integer); inline;
  public
    constructor Create(AIndex, AOrigin, AMaxIndex: Integer);
    function ActualValue(const AOrigin: Integer): Integer; inline;
    procedure SetActualValue(const AOrigin, AValue: Integer);
    class operator Equal(const V1, V2: TdxSpreadSheetReference): Boolean;
    class operator NotEqual(const V1, V2: TdxSpreadSheetReference): Boolean;
    procedure Move(ADelta: Integer); inline;
    procedure Reset;
    function UpdateAreaReference(const AOrigin, AIndex1, AIndex2, ANewIndex1: Integer;
      var APoint2: TdxSpreadSheetReference; AMoveReference, AMoveOrigin: Boolean): Boolean; inline;
    function UpdateReference(const AOrigin, AIndex1, AIndex2, ANewIndex1: Integer;
      AMoveReference, AMoveOrigin: Boolean): Boolean; inline;

    property Flags: Integer read GetFlags write SetFlags;
    property IsAbsolute: Boolean read GetIsAbsolute write SetIsAbsolute;
    property IsAllItems: Boolean read GetIsAllItems write SetIsAllItems;
    property IsError: Boolean read GetIsError write SetIsError;
    property Offset: Integer read GetOffset write SetOffset;
  end;

  { TdxSpreadSheetReferencePath }

  TdxSpreadSheetReferencePath = class
  strict private
    FColumn: Integer;
    FNext: TdxSpreadSheetReferencePath;
    FRow: Integer;
    FSheet: TObject;
  public
    constructor Create(ARow, AColumn: Integer; ASheet: TObject);
    destructor Destroy; override;
    procedure Add(ARow, AColumn: Integer; ASheet: TObject);
    procedure Remove(ARow, AColumn: Integer; ASheet: TObject);

    property Column: Integer read FColumn;
    property Next: TdxSpreadSheetReferencePath read FNext;
    property Row: Integer read FRow;
    property Sheet: TObject read FSheet;
  end;

  { TdxSpreadSheetAreaList }

  TdxSpreadSheetAreaList = class(TdxRectList)
  public const
    ValueSeparator = ';';
  strict private
    FOnChange: TNotifyEvent;
  public
    procedure Assign(AList: TdxRectList);
    procedure AssignFromString(const S: string);
    function BoundingRect: TRect;
    function Clone: TdxSpreadSheetAreaList;
    function Contains(ARow, AColumn: Integer): Boolean; overload;
    function Equals(Obj: TObject): Boolean; override;
    function Intersects(const AArea: TRect): Boolean;
    function ToString: string; override;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const
  dxSpreadSheetDefaultPasteOptions = [cpoValues..cpoStyles];
  dxSpreadSheetDefaultPassword = 'VelvetSweatshop';
  dxSpreadSheetFeatureFunctionPrefix = '_xlfn.';

  // Zoom
  dxSpreadSheetDefaultZoomFactor = 100;
  dxSpreadSheetMinimumZoomFactor = 45;
  dxSpreadSheetMaximumZoomFactor = 400;

  // Columns
  dxSpreadSheetDefaultColumnWidth = 85;
  dxSpreadSheetMaxColumnCount = 1 shl 14;
  dxSpreadSheetMaxColumnIndex = dxSpreadSheetMaxColumnCount - 1;

  // Rows
  dxSpreadSheetMaxRowCount = 1 shl 20;
  dxSpreadSheetMaxRowIndex = dxSpreadSheetMaxRowCount - 1;
  dxSpreadSheetDefaultRowHeight = 20;

  // Outlines
  dxSpreadSheetMaxOutlineLevel = 8;

  dxSpreadSheetMaxCaptionLength = 31;
  dxSpreadSheetMaxSeparatorWidth  = 10;
  dxSpreadSheetResizeDelta = 5;
  dxSpreadSheetScrollAreaWidth = 15;

  dxExcelStandardColors: array[0..55] of TColor = (
    $000000, $FFFFFF, $0000FF, $00FF00, $FF0000, $00FFFF, $FF00FF, $FFFF00,
    $000080, $008000, $800000, $008080, $800080, $808000, $C0C0C0, $808080,
    $FF9999, $663399, $CCFFFF, $FFFFCC, $660066, $8080FF, $CC6600, $FFCCCC,
    $800000, $FF00FF, $00FFFF, $FFFF00, $800080, $000080, $808000, $FF0000,
    $FFCC00, $FFFFCC, $CCFFCC, $99FFFF, $FFCC99, $CC99FF, $FF99CC, $99CCFF,
    $FF6633, $CCCC33, $00CC99, $00CCFF, $0099FF, $0066FF, $996666, $969696,
    $663300, $669933, $003300, $003333, $003399, $663399, $993333, $333333
  );


function dxSpreadSheetGetStandardColorIndex(const AColor: TColor): Integer;

implementation

uses
  dxSpreadSheetUtils, dxSpreadSheetFunctionsText, cxVariants, cxGeometry, StrUtils, dxStringHelper;

type
  TInt64 = record
    Hi, Low: Integer;
  end;

const
  dxRefAbsolute = 1;
  dxRefAllItems = 2;
  dxRefError    = 4;

function dxSpreadSheetGetStandardColorIndex(const AColor: TColor): Integer;
var
  I: Integer;
begin
  for I := Low(dxExcelStandardColors) to High(dxExcelStandardColors) do
  begin
    if dxExcelStandardColors[I] = AColor then
      Exit(I);
  end;
  Result := -1;
end;

{  TdxSpreadSheetCellData }

constructor TdxSpreadSheetCellData.Create(ARow, AColumn: Integer; const AValue: Variant);
begin
  Column := AColumn;
  Row := ARow;
  Value := AValue;
end;

{ TdxSpreadSheetVectorValue }

constructor TdxSpreadSheetVectorValue.Create(const AValue: Variant; AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  Value := AValue;
  ErrorCode := AErrorCode;
end;

function TdxSpreadSheetVectorValue.IsError: Boolean;
begin
  Result := ErrorCode <> ecNone;
end;

{ TdxSpreadSheetVector }

function TdxSpreadSheetVector.GetItemIndex(const AValue: Variant; ANeedMaskSearch: Boolean): Integer;
var
  AIndex: Integer;
  ALength: Integer;
begin
  AIndex := 0;
  ALength := Length;
  while AIndex < ALength do
  begin
    if IsEqualValues(AValue, Items[AIndex].Value, ANeedMaskSearch) then
      Exit(AIndex);

    AIndex := GetNextItemIndex(AIndex, True);
    if AIndex < 0 then Break;
  end;
  Result := -1;
end;

function TdxSpreadSheetVector.GetFirstZeroValueIndex: Integer;
var
  AIndex: Integer;
  ALength: Integer;
  AValue: Variant;
begin
  AIndex := 0;
  ALength := Length;
  while AIndex < ALength do
  begin
    AValue := Items[AIndex].Value;
    if dxIsNumberOrDateTime(AValue) and (AValue = 0) then
      Exit(AIndex);

    AIndex := GetNextItemIndex(AIndex, True);
    if AIndex < 0 then Break;
  end;
  Result := -1;
end;

function TdxSpreadSheetVector.GetNextItemIndex(AIndex: Integer; AForward: Boolean): Integer;
begin
  Result := IfThen(AForward, AIndex + 1, AIndex - 1);
end;

function TdxSpreadSheetVector.IsEqualValues(V1, V2: Variant; ANeedMaskSearch: Boolean): Boolean;
begin
  if VarIsStr(V1) then
    V1 := dxSpreadSheetUpperCase(V1);
  if VarIsStr(V2) then
    V2 := dxSpreadSheetUpperCase(V2);
  if ANeedMaskSearch then
    Result := dxSearchValueInText(V1, V2, 1) = 1
  else
    Result := dxSpreadSheetVarCompare(V1, V2) = 0;
end;

{ TdxSpreadSheetSimpleVector }

constructor TdxSpreadSheetSimpleVector.Create(ACapacity: Integer);
begin
  inherited Create;
  FData := TList<TdxSpreadSheetVectorValue>.Create;
  FData.Capacity := ACapacity;
end;

destructor TdxSpreadSheetSimpleVector.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TdxSpreadSheetSimpleVector.Add(const AValue: Variant; AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  FData.Add(TdxSpreadSheetVectorValue.Create(AValue, AErrorCode));
end;

function TdxSpreadSheetSimpleVector.GetItem(Index: Integer): TdxSpreadSheetVectorValue;
begin
  Result := FData[Index];
end;

function TdxSpreadSheetSimpleVector.GetLength: Integer;
begin
  Result := FData.Count;
end;

{ TdxSpreadSheetFormulaTokenDimension }

class operator TdxSpreadSheetFormulaTokenDimension.Equal(const ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension): Boolean;
begin
  Result := (ADimension1.ColumnCount = ADimension2.ColumnCount) and (ADimension1.RowCount = ADimension2.RowCount);
end;

class operator TdxSpreadSheetFormulaTokenDimension.NotEqual(const ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension): Boolean;
begin
  Result := not (ADimension1 = ADimension2);
end;

function TdxSpreadSheetFormulaTokenDimension.Count: Integer;
begin
  Result := RowCount * ColumnCount;
end;

procedure TdxSpreadSheetFormulaTokenDimension.SetDimension(const ARowCount, AColumnCount: Integer);
begin
  RowCount := ARowCount;
  ColumnCount := AColumnCount;
end;

{ TdxSpreadSheetReference }

constructor TdxSpreadSheetReference.Create(AIndex, AOrigin, AMaxIndex: Integer);
begin
  Reset;
  IsAbsolute := AOrigin < 0;
  if IsAbsolute then
    Offset := AIndex
  else
    Offset := AIndex - AOrigin;

  if Offset >= AMaxIndex then
    IsAllItems := True;
end;

function TdxSpreadSheetReference.ActualValue(const AOrigin: Integer): Integer;
begin
  if IsError then
    Result := -1
  else
    if IsAllItems then
      Result := MaxInt
    else
      if IsAbsolute then
        Result := Offset
      else
        Result := Offset + AOrigin;
end;

procedure TdxSpreadSheetReference.SetActualValue(const AOrigin, AValue: Integer);
begin
  if IsAbsolute then
    Offset := AValue
  else
    Offset := AValue - AOrigin;

  IsError := IsError or (ActualValue(AOrigin) < 0);
end;

class operator TdxSpreadSheetReference.Equal(const V1, V2: TdxSpreadSheetReference): Boolean;
begin
  Result := V1.Value = V2.Value;
end;

class operator TdxSpreadSheetReference.NotEqual(const V1, V2: TdxSpreadSheetReference): Boolean;
begin
  Result := not (V1 = V2);
end;

procedure TdxSpreadSheetReference.Move(ADelta: Integer);
begin
  if IsAbsolute or IsError or IsAllItems then
    Exit;
  Offset := Offset + ADelta;
end;

procedure TdxSpreadSheetReference.Reset;
begin
  Value := 0;
end;

function TdxSpreadSheetReference.UpdateAreaReference(
  const AOrigin, AIndex1, AIndex2, ANewIndex1: Integer;
  var APoint2: TdxSpreadSheetReference; AMoveReference, AMoveOrigin: Boolean): Boolean;
var
  ADestOrigin, ARef1, ARef2, ADelta, AOffset1, AOffset2: Integer;
begin
  Result := False;
  if IsError or IsAllItems or APoint2.IsError or APoint2.IsAllItems then
    Exit;

  if ANewIndex1 >= 0 then
    ADelta := ANewIndex1 - AIndex1
  else
    ADelta := AIndex2 - AIndex1 + 1;

  ADestOrigin := AOrigin;
  if (AIndex1 <= AOrigin) and AMoveOrigin then
    ADestOrigin := ADestOrigin - ADelta * (Ord(ANewIndex1 < 0) * 2 - 1);

  AOffset1 := Offset;
  AOffset2 := APoint2.Offset;

  ARef1 := ActualValue(AOrigin);
  ARef2 := APoint2.ActualValue(AOrigin);

  if AMoveReference then
  begin
    if ANewIndex1 >= 0 then
    begin
      if ARef1 >= AIndex1 then
        Inc(ARef1, ADelta);
      if ARef2 >= AIndex1 then
        Inc(ARef2, ADelta);
    end
    else
    begin
      IsError := (AIndex1 <= ARef1) and (AIndex2 >= ARef2);

      if ARef1 > AIndex2 then
        Dec(ARef1, ADelta)
      else
        if ARef1 >= AIndex1 then
          ARef1 := AIndex1;

      if ARef2 >= AIndex2 then
        Dec(ARef2, ADelta)
      else
        if AIndex1 < ARef2 then
          ARef2 := AIndex1;
    end;
  end;

  SetActualValue(ADestOrigin, ARef1);
  APoint2.SetActualValue(ADestOrigin, ARef2);

  IsError := IsError or APoint2.IsError or (APoint2.ActualValue(AOrigin) < ActualValue(AOrigin));
  APoint2.IsError := IsError;
  Result := IsError or (AOffset1 <> Offset) or (AOffset2 <> APoint2.Offset);
end;

function TdxSpreadSheetReference.UpdateReference(
  const AOrigin, AIndex1, AIndex2, ANewIndex1: Integer; AMoveReference, AMoveOrigin: Boolean): Boolean;
var
  ADestOrigin, ARef, ADelta, AOffset: Integer;
begin
  Result := False;
  if IsError or IsAllItems or (AIndex1 = ANewIndex1) then
    Exit;

  if ANewIndex1 < 0 then
    ADelta := AIndex2 - AIndex1 + 1
  else
    ADelta := ANewIndex1 - AIndex1;

  ADestOrigin := AOrigin;
  if (AIndex1 <= AOrigin) and AMoveOrigin then
    ADestOrigin := ADestOrigin - ADelta * (Ord(ANewIndex1 < 0) * 2 - 1);

  ARef := ActualValue(AOrigin);
  AOffset := Offset;
  if AMoveReference then
  begin
    if ANewIndex1 < 0 then
    begin
      IsError := InRange(ARef, AIndex1, AIndex2);
      if ARef >= AIndex2 then
        Dec(ARef, ADelta)
      else
        if ARef >= AIndex1 then
          ARef := AIndex1;
    end
    else
    begin
      if ARef >= AIndex1 then
        Inc(ARef, ADelta);
    end;
  end;
  SetActualValue(ADestOrigin, ARef);
  Result := IsError or (AOffset <> Offset);
end;

function TdxSpreadSheetReference.GetFlags: Integer;
begin
  Result := TInt64(Value).Hi;
end;

function TdxSpreadSheetReference.GetIsAbsolute: Boolean;
begin
  Result := Flags and dxRefAbsolute = dxRefAbsolute;
end;

function TdxSpreadSheetReference.GetIsAllItems: Boolean;
begin
  Result := Flags and dxRefAllItems = dxRefAllItems;
end;

function TdxSpreadSheetReference.GetIsError: Boolean;
begin
  Result := Flags and dxRefError = dxRefError;
end;

function TdxSpreadSheetReference.GetOffset: Integer;
begin
  Result := TInt64(Value).Low;
end;

procedure TdxSpreadSheetReference.SetFlags(AValue: Integer);
begin
  TInt64(Value).Hi := AValue;
end;

procedure TdxSpreadSheetReference.SetIsAbsolute(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefAbsolute
  else
    Flags := Flags and not dxRefAbsolute;
end;

procedure TdxSpreadSheetReference.SetIsAllItems(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefAllItems
  else
    Flags := Flags and not dxRefAllItems;
end;

procedure TdxSpreadSheetReference.SetIsError(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefError
  else
    Flags := Flags and not dxRefError;
end;

procedure TdxSpreadSheetReference.SetOffset(AValue: Integer);
begin
  TInt64(Value).Low := AValue;
  IsAllItems := AValue = MaxInt;
end;

{ TdxSpreadSheetReferencePath }

constructor TdxSpreadSheetReferencePath.Create(ARow, AColumn: Integer; ASheet: TObject);
begin
  FColumn := AColumn;
  FRow := ARow;
  FSheet := ASheet;
end;

destructor TdxSpreadSheetReferencePath.Destroy;
begin
  FreeAndNil(FNext);
  inherited Destroy;
end;

procedure TdxSpreadSheetReferencePath.Add(ARow, AColumn: Integer; ASheet: TObject);
var
  ALast, AItem: TdxSpreadSheetReferencePath;
begin
  AItem := TdxSpreadSheetReferencePath.Create(ARow, AColumn, ASheet);
  ALast := Self;
  while ALast.Next <> nil do
    ALast := ALast.Next;
  ALast.FNext := AItem;
end;

procedure TdxSpreadSheetReferencePath.Remove(ARow, AColumn: Integer; ASheet: TObject);
var
  ABeforeLast, ALast: TdxSpreadSheetReferencePath;
begin
  ALast := Self;
  ABeforeLast := nil;
  while ALast.Next <> nil do
  begin
    ABeforeLast := ALast;
    ALast := ALast.Next;
  end;
  if (ALast.Row = ARow) and (ALast.Column = AColumn) and (ALast.Sheet = ASheet) then
  begin
    if ABeforeLast <> nil then
      ABeforeLast.FNext := nil;
    ALast.Free;
  end;
end;

{ TdxSpreadSheetAreaList }

procedure TdxSpreadSheetAreaList.Assign(AList: TdxRectList);
begin
  Clear;
  AddRange(AList);
end;

procedure TdxSpreadSheetAreaList.AssignFromString(const S: string);
var
  AArea: TRect;
  AParts: TStringDynArray;
  I: Integer;
begin
  AParts := SplitString(S, ValueSeparator);

  Clear;
  Capacity := Length(AParts);
  for I := 0 to Length(AParts) - 1 do
  begin
    AArea := dxStringToReferenceArea(AParts[I]);
    if dxSpreadSheetIsValidArea(AArea) then
      Add(AArea);
  end;
end;

function TdxSpreadSheetAreaList.BoundingRect: TRect;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Result := Items[0];
    for I := 1 to Count - 1 do
      Result := cxRectUnion(Result, Items[I]);
  end
  else
    Result := cxNullRect;
end;

function TdxSpreadSheetAreaList.Clone: TdxSpreadSheetAreaList;
begin
  Result := TdxSpreadSheetAreaList.Create;
  Result.Assign(Self);
end;

function TdxSpreadSheetAreaList.Contains(ARow, AColumn: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if dxSpreadSheetContains(Items[I], ARow, AColumn) then
      Exit(True);
  end;
  Result := False;
end;

function TdxSpreadSheetAreaList.Equals(Obj: TObject): Boolean;
var
  I: Integer;
begin
  Result := (Obj is TdxSpreadSheetAreaList) and (TdxSpreadSheetAreaList(Obj).Count = Count);
  if Result then
  begin
    for I := 0 to TdxSpreadSheetAreaList(Obj).Count - 1 do
      Result := Result and cxRectIsEqual(Items[I], TdxSpreadSheetAreaList(Obj).Items[I]);
  end;
end;

function TdxSpreadSheetAreaList.Intersects(const AArea: TRect): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if dxSpreadSheetIntersects(AArea, Items[I]) then
      Exit(True);
  end;
  Result := False;
end;

function TdxSpreadSheetAreaList.ToString: string;
var
  I: Integer;
  S: TStringBuilder;
begin
  S := TdxStringBuilderManager.Get(64);
  try
    for I := 0 to Count - 1 do
    begin
      if S.Length > 0 then
        S.Append(ValueSeparator);
      dxReferenceToString(S, Items[I], False, 0, 0);
    end;
    Result := S.ToString;
  finally
    TdxStringBuilderManager.Release(S);
  end;
end;

end.
