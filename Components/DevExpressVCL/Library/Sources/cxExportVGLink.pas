{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxExportVGLink;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, DB, cxVGrid, dxCore, cxClasses;

procedure cxExportVGToHTML(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure cxExportVGToXML(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure cxExportVGToExcel(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; ARecordPerBand: Integer = 8; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure cxExportVGToXLSX(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; ARecordPerBand: Integer = 8; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure cxExportVGToCSV(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  const ASeparator: Char = ','; ARecordPerBand: Integer = 8; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure cxExportVGToText(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure cxExportVGToText(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = ''; ARecordPerBand: Integer = 8;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;

procedure cxExportVGToFile(AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExportType: Integer;
  AExpand, AUseNativeFormat: Boolean; const ASeparators: array of string; ARecordPerBand: Integer;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);
implementation

uses
  Windows, Types, Graphics, cxGeometry, Variants,
  cxEdit, cxCalendar, cxCurrencyEdit, cxSpinEdit, cxCalc, cxTimeEdit, Classes, cxGraphics, cxStyles,
  cxInplaceContainer, cxDataUtils, cxExport, cxVGridViewInfo, cxVGridConsts, cxVGridUtils, cxTextEdit, cxDBVGrid;

type
  TcxControllerAccess = class(TcxvgController);
  TcxCustomEditorRowAccess = class(TcxCustomEditorRow);
  TcxCustomEditorRowPropertiesAccess = class(TcxCustomEditorRowProperties);
  TcxCustomMultiEditorRowAccess = class(TcxCustomMultiEditorRow);
  TcxCustomRowAccess = class(TcxCustomRow);
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);
  TcxPropertiesAccess = class(TcxCustomEditProperties);
  TcxRowHeaderAccess = class(TcxCustomRowHeaderInfo);
  TcxScrollerAccess = class(TcxvgScroller);
  TcxStylesAccess = class(TcxVerticalGridStyles);
  TcxVerticalGridAccess = class(TcxCustomVerticalGrid);
  TcxViewInfoAccess = class(TcxvgCustomViewInfo);

  TcxVerticalGridExportHelper = class;
  TcxColumnsMap = class;

  TcxRowIndentData = record
    StyleIndex: Integer;
    IsCategory: Boolean;
    Column: Integer;
    Width: Integer;
  end;
  PcxRowIndentData = ^TcxRowIndentData;

  { TcxRowIndentsInfo }

  TcxRowIndentsInfo = class
  private
    FGridLineColor: TColor;
    FGridLines: TcxvgGridLines;
    FHeaderColumnsMap: TcxColumnsMap;
    FHeaderInfo: TcxRowHeaderAccess;
    FIsCategory: Boolean;
    FList: TList;
    FPaintStyle: TcxvgPaintStyle;
    FProvider: IcxExportProvider;
    FRow: TcxCustomRow;
    function GetCount: Integer;
    function GetIndent(Index: Integer): TcxRowIndentData;
  protected
    procedure AddFirstIndent;
    procedure AddParentIndents;
    procedure Calculate(AHeaderColumnsMap: TcxColumnsMap; AProvider: IcxExportProvider);
    property Row: TcxCustomRow read FRow;
    property GridLineColor: TColor read FGridLineColor;
    property GridLines: TcxvgGridLines read FGridLines;
    property HeaderColumnsMap: TcxColumnsMap read FHeaderColumnsMap;
    property HeaderInfo: TcxRowHeaderAccess read FHeaderInfo;
    property IsCategory: Boolean read FIsCategory;
    property PaintStyle: TcxvgPaintStyle read FPaintStyle;
    property Provider: IcxExportProvider read FProvider;
  public
    constructor Create(ARow: TcxCustomRow);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Indents[Index: Integer]: TcxRowIndentData read GetIndent;
  end;

  { TcxColumnsMap }

  TElementInfo = record
    Pos: Integer;
    ColumnStart: Integer;
    ColumnEnd: Integer;
    Width: Integer;
    IsLevel: Boolean;
    case Boolean of
      False: (
        Row: TcxCustomRow;
        CellIndex: Integer
      );
      True: (
        Level: Integer
      );
  end;
  PElementInfo = ^TElementInfo;

  TcxColumnsMap = class
  private
    FElements: TList;
    FColumnWidths: array of Integer;
  protected
    NeedWidth: Integer;
    MaxColumnIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRowCell(APos: Integer; ARow: TcxCustomRow; ACellIndex, AWidth: Integer);
    procedure AddLevel(ALevel, APos, AWidth: Integer);
    procedure Build;
    procedure CheckNeedWidth(APos: Integer);
    function FindColumnForPos(APos: Integer): Integer;
    procedure GetColumnInfoFromRowCell(ARow: TcxCustomRow; ACellIndex: Integer;  var AStart, AEnd: Integer);
    procedure GetColumnInfoFromLevel(ALevel: Integer; var AStart, AEnd: Integer);
    function GetColumnWidth(AIndex: Integer): Integer;
  end;

  { TcxRowsIndents }

  TcxRowsIndents = class(TList)
  private
    function GetIndent(Index: Integer): TcxRowIndentsInfo;
  public
    procedure Clear; override;
    property Indents[Index: Integer]: TcxRowIndentsInfo read GetIndent; default;
  end;

  { TcxRowCellsInfo }

  TRowCaptionCellInfo = record
    Caption: string;
    Column: Integer;
    Width: Integer;
    StyleIndex: Integer;
  end;
  PRowCaptionCellInfo = ^TRowCaptionCellInfo;

  TcxRowCellsInfo = class(TList)
  private
    function GetItem(Index: Integer): TRowCaptionCellInfo;
  public
    function AddCaption(AColumn: Integer; AWidth: Integer; AStyleIndex: Integer; const ACaption: string): Integer; overload;
    procedure Clear; override;
    property Items[Index: Integer]: TRowCaptionCellInfo read GetItem; default;
  end;

  { TcxRowsCaptions }

  TcxRowsCaptions = class(TList)
  private
    function GetCaption(Index: Integer): TcxRowCellsInfo;
  public
    function AddCaption: TcxRowCellsInfo;
    procedure Clear; override;
    property Captions[Index: Integer]: TcxRowCellsInfo read GetCaption;
  end;

  { TcxVerticalGridMapInfo }

  TcxVerticalGridMapsInfo = class
  private
    FBandCount: Integer;
    FFirstValuesColumn: Integer;
    FGridLineColor: TColor;
    FGridLines: TcxvgGridLines;
    FHeaderColumnsMap: TcxColumnsMap;
    FIsEmpty: Boolean;
    FLastBandRecords: Integer;
    FLevelIndents: array of Integer;
    FMaxHeaderWidth: Integer;
    FMaxLevel: Integer;
    FMaxValueWidth: Integer;
    FMinHeaderWidth: Integer;
    FMinValueWidth: Integer;
    FOwner: TcxVerticalGridExportHelper;
    FPaintStyle: TcxvgPaintStyle;
    FRecordCount: Integer;
    FRecordsPerBand: Integer;
    FRowCount: Integer;
    FRows: TList;
    FRowsCaptions: TcxRowsCaptions;
    FRowsIndents: TcxRowsIndents;
    FSize: TSize;
    FValueColumnsMap: TcxColumnsMap;
    FVerticalGrid: TcxVerticalGridAccess;
    FViewInfo: TcxViewInfoAccess;
    function GetLevelIndent(Index: Integer): Integer;
    function GetProvider: IcxExportProvider;
    function GetRow(Index: Integer): TcxCustomRow;
  protected
    TotalWidth: Integer;
    Position: Integer;
    procedure AddMultiEditorRowCells(ARow: TcxCustomRow);
    procedure AddMapRightSide(AMap: TcxColumnsMap; ARight, AMinCellWidth: Integer; ACalcIndent: Boolean);
    procedure AlignCategories;
    procedure CalculateHeader;
    procedure CalculateRowsCaptions;
    procedure CalculateRowsIndents;
    procedure CalculateSize;
    procedure CalculateValuesMap;
    procedure DoCalculate; virtual;
    procedure DoWrite; virtual;
    function GetDisplayText(ARecordIndex: Integer; ARow: TcxCustomEditorRowProperties): string;
    procedure GetDisplayValue(ARecordIndex: Integer; ARow: TcxCustomEditorRowProperties;
      out AValue: Variant; out AValueDisplayFormat: string; out AValueDisplayFormatType: TcxValueDisplayFormatType);
    function GetMultiEditorRowProperties(ARow: TcxCustomRow): TcxMultiEditorRowProperties;
    function IsIncludeRow(ARow: TcxCustomRow): Boolean; virtual;
    function IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
    procedure SetCellStyle(ACol, ARow, W, AStyleIndex: Integer); overload;
    procedure SetCellStyle(ACol, ARow, W: Integer; const AStyle: TcxCacheCellStyle); overload;
    procedure WriteColumnWidths;
    procedure WriteHeaders(ARowIndex: Integer); virtual;
    procedure WriteRecord(ACol, ARowIndex, ARecordIndex: Integer); virtual;
    procedure WriteRowCaptions(ARowIndex: Integer; ACaptions: TcxRowCellsInfo);
    procedure WriteRowHeader(ARowIndex: Integer; AIndents: TcxRowIndentsInfo; ACaptions: TcxRowCellsInfo);
    procedure WriteValue(ACol, ARow, ARecordIndex: Integer; AProperties: TcxCustomEditorRowProperties);
    procedure WriteValues;

    property FirstValuesColumn: Integer read FFirstValuesColumn;
    property GridLineColor: TColor read FGridLineColor;
    property GridLines: TcxvgGridLines read FGridLines;
    property HeaderColumnsMap: TcxColumnsMap read FHeaderColumnsMap;
    property IsEmpty: Boolean read FIsEmpty;
    property LastBandRecords: Integer read FLastBandRecords;
    property LevelIndents[Index: Integer]: Integer read GetLevelIndent;
    property MaxHeaderWidth: Integer read FMaxHeaderWidth;
    property MaxValueWidth: Integer read FMaxValueWidth;
    property MinHeaderWidth: Integer read FMinHeaderWidth;
    property MinValueWidth: Integer read FMinHeaderWidth;
    property Owner: TcxVerticalGridExportHelper read FOwner;
    property PaintStyle: TcxvgPaintStyle read FPaintStyle;
    property Provider: IcxExportProvider read GetProvider;
    property RecordCount: Integer read FRecordCount;
    property RecordsPerBand: Integer read FRecordsPerBand;
    property ValueColumnsMap: TcxColumnsMap read FValueColumnsMap;
    property ViewInfo: TcxViewInfoAccess read FViewInfo;
  public
    constructor Create(AOwner: TcxVerticalGridExportHelper); virtual;
    destructor Destroy; override;
    property MaxLevel: Integer read FMaxLevel;
    property RowCount: Integer read FRowCount;
    property Rows[Index: Integer]: TcxCustomRow read GetRow;
    property RowsCaptions: TcxRowsCaptions read FRowsCaptions;
    property RowsIndents: TcxRowsIndents read FRowsIndents;
    property Size: TSize read FSize;
    property VerticalGrid: TcxVerticalGridAccess read FVerticalGrid;
  end;

  TcxVerticalGridMapsInfoClass = class of TcxVerticalGridMapsInfo;

  { TcxVerticalGridExportHelper }

  TcxVerticalGridExportHelper = class
  private
    FExpand: Boolean;
    FHandler: TObject;
    FIsNativeFormat: Boolean;
    FProgressHelper: TcxCustomProgressCalculationHelper;
    FProvider: IcxExportProvider;
    FRecordsPerBand: Integer;
    FVerticalGrid: TcxVerticalGridAccess;

    procedure ProgressHandler(Sender: TObject; Percents: Integer);
  protected
    MapsInfo: TcxVerticalGridMapsInfo;

    procedure DoCalculateTableMap; virtual;
    procedure DoWriteCells; virtual;
    function GetMapsInfoClass: TcxVerticalGridMapsInfoClass; virtual;
  public
    constructor Create(AVerticalGrid: TcxCustomVerticalGrid; AExportType: Integer;
      const AFileName: string; AHandler: TObject); virtual;
    destructor Destroy; override;
    //
    property Expand: Boolean read FExpand write FExpand;
    property IsNativeFormat: Boolean read FIsNativeFormat;
    property ProgressHelper: TcxCustomProgressCalculationHelper read FProgressHelper;
    property Provider: IcxExportProvider read FProvider;
    property RecordsPerBand: Integer read FRecordsPerBand write FRecordsPerBand;
    property VerticalGrid: TcxVerticalGridAccess read FVerticalGrid;
  end;

const
  cxInvalidIndex       = -1;
  cxIndentFontName     = 'Tahoma';
  cxCellBorders: array[Boolean] of TcxBorders = ([], cxBordersAll);
  cxIndentStyle: TcxCacheCellStyle = (
    AlignText: catCenter;
    BrushStyle: cbsSolid;
    FontCharset: 0;
    FontColor: 0;
    FontSize: 12;
    FontStyle: []
  );

// todo: need move to cxExport for Delphi 4
  AlignToCxAlign: array[TAlignment] of TcxAlignText = (catLeft, catRight, catCenter);
  cxUsedBorder: TcxCellBorders = (IsDefault: False; Width: 1);
  cxEmptyBorder: TcxCellBorders = (IsDefault: True; Width: 0);

function cxSetBorder(var ABorder: TcxCellBorders; NeedSet: Boolean; Color: Integer): Boolean;
begin
  Result := NeedSet;
  if NeedSet then
  begin
    ABorder := cxUsedBorder;
    ABorder.Color := Color;
  end
  else
    ABorder := cxEmptyBorder;
end;

procedure cxCheckBorders(var AStyle: TcxCacheCellStyle;
  const ABorders: TcxBorders; AColor: TColor; AGridLines: TcxvgGridLines);
begin
  cxSetBorder(AStyle.Borders[0],
    (bLeft in ABorders) and (AGridLines in [vglVertical, vglBoth]), AColor);
  cxSetBorder(AStyle.Borders[1],
    (bTop in ABorders) and (AGridLines in [vglHorizontal, vglBoth]), AColor);
  cxSetBorder(AStyle.Borders[2],
    (bRight in ABorders) and (AGridLines in [vglVertical, vglBoth]), AColor);
  cxSetBorder(AStyle.Borders[3],
    (bBottom in ABorders) and (AGridLines in [vglHorizontal, vglBoth]), AColor);
end;

procedure cxViewParamsToCacheStyle(AViewParams: TcxViewParams; var ACacheStyle: TcxCacheCellStyle);
begin
  ACacheStyle := DefaultCellStyle;
  ACacheStyle.FontName := AViewParams.Font.Name;
  ACacheStyle.FontStyle := AViewParams.Font.Style;
  ACacheStyle.FontColor := ColorToRgb(AViewParams.TextColor);
  ACacheStyle.FontSize := AViewParams.Font.Size;
  ACacheStyle.FontCharset := AViewParams.Font.Charset;
  ACacheStyle.BrushStyle := cbsSolid;
  ACacheStyle.BrushBkColor := ColorToRgb(AViewParams.Color);
  ACacheStyle.BrushFgColor := ACacheStyle.BrushBkColor;
end;

function GetHeaderIndentViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  with TcxRowHeaderAccess(ARow.ViewInfo.HeaderInfo) do
  begin
    CalcViewParams(False);
    Result := IndentViewParams;
  end;
end;

function GetHeaderViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  with TcxRowHeaderAccess(ARow.ViewInfo.HeaderInfo) do
  begin
    CalcViewParams(False);
    Result := ViewParams;
  end;
end;

{ TcxRowIndentsInfo }

constructor TcxRowIndentsInfo.Create(ARow: TcxCustomRow);
begin
  FRow := ARow;
  FList := TList.Create;
end;

destructor TcxRowIndentsInfo.Destroy;
var
  I: Integer;
begin
  FProvider := nil;
  for I := 0 to FList.Count - 1 do
    Dispose(PcxRowIndentData(FList[I]));
  FList.Free;
  inherited Destroy;
end;

procedure TcxRowIndentsInfo.AddFirstIndent;
var
  AEnd: Integer;
  Info: PcxRowIndentData;
  AStyle: TcxCacheCellStyle;
  ABorders: TcxBorders;
begin
  New(Info);
  FIsCategory := FRow.IsCategory;
  Info.IsCategory := IsCategory;
  HeaderColumnsMap.GetColumnInfoFromLevel(FRow.Level, Info.Column, AEnd);
  Info.Width := AEnd - Info.Column;
  cxViewParamsToCacheStyle(GetHeaderIndentViewParams(FRow), AStyle);
  AStyle.AlignText := catCenter;
  ABorders := [bTop, bBottom];
  if FRow.IsRootLevel then Include(ABorders, bLeft);
  if (PaintStyle = psDotNet) and IsCategory then
  begin
    Include(ABorders, bLeft);
    if FRow.HasVisibleChildren then
      Exclude(ABorders, bBottom);
  end;
  cxCheckBorders(AStyle, ABorders, GridLineColor, GridLines);
  Info.StyleIndex := Provider.RegisterStyle(AStyle);
  FList.Add(Info);
end;

procedure TcxRowIndentsInfo.AddParentIndents;
var
  AEnd: Integer;
  Info: PcxRowIndentData;
  ABorders: TcxBorders;
  ANeedBottom, ALastRow: Boolean;
  AParent: TcxCustomRow;
  AStyle: TcxCacheCellStyle;
begin
  if FRow.IsRootLevel then Exit;
  AParent := Row;
  ALastRow := Row.VerticalGrid.LastVisibleRow = Row;
  ANeedBottom := True;
  repeat
    AParent := AParent.Parent;
    New(Info);
    Info.IsCategory := AParent.IsCategory;
    HeaderColumnsMap.GetColumnInfoFromLevel(AParent.Level, Info.Column, AEnd);
    Info.Width := AEnd - Info.Column;
    cxViewParamsToCacheStyle(GetHeaderIndentViewParams(AParent), AStyle);
    if PaintStyle = psDelphi then
    begin
      ABorders := [bTop, bBottom];
      if AParent.IsRootLevel then Include(ABorders, bLeft);
    end
    else
    begin
      if Info.IsCategory then
      begin
        ANeedBottom := False;
        ABorders := [bLeft, bRight];
        if AParent.GetLastVisibleChild = FRow then
          Include(ABorders, bBottom);
      end
      else
        if (not IsCategory and ANeedBottom) or ALastRow then
          ABorders := [bBottom]
        else
          ABorders := [];
    end;
    cxCheckBorders(AStyle, ABorders, GridLineColor, GridLines);
    Info.StyleIndex := Provider.RegisterStyle(AStyle);
    FList.Add(Info);
  until AParent.IsRootLevel;
end;

procedure TcxRowIndentsInfo.Calculate(AHeaderColumnsMap: TcxColumnsMap;
  AProvider: IcxExportProvider);
begin
  FHeaderColumnsMap := AHeaderColumnsMap;
  FHeaderInfo := TcxRowHeaderAccess(FRow.ViewInfo.HeaderInfo);
  with FRow.VerticalGrid.OptionsView do
  begin
    FGridLines := GridLines;
    FPaintStyle := PaintStyle;
    FGridLineColor := GridLineColor;
  end;
  FProvider := AProvider;
  AddFirstIndent;
  AddParentIndents;
end;

function TcxRowIndentsInfo.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxRowIndentsInfo.GetIndent(Index: Integer): TcxRowIndentData;
begin
  Result := PcxRowIndentData(FList[Index])^;
end;

{ TcxRowsIndents }

procedure TcxRowsIndents.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited Clear;
end;

function TcxRowsIndents.GetIndent(Index: Integer): TcxRowIndentsInfo;
begin
  Result := TcxRowIndentsInfo(List[Index]);
end;

{ TcxRowCellsInfo }

function TcxRowCellsInfo.AddCaption(AColumn, AWidth,
  AStyleIndex: Integer; const ACaption: string): Integer;
var
  C: PRowCaptionCellInfo;
begin
  New(C);
  with C^ do
  begin
    Caption := ACaption;
    Column := AColumn;
    StyleIndex := AStyleIndex;
    Width := AWidth;
  end;
  Result := Add(C);
end;

procedure TcxRowCellsInfo.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PRowCaptionCellInfo(inherited Items[I]));
  inherited Clear;
end;

function TcxRowCellsInfo.GetItem(Index: Integer): TRowCaptionCellInfo;
begin
  Result := TRowCaptionCellInfo(inherited Items[Index]^);
end;

{ TcxRowsCaptions }

function TcxRowsCaptions.AddCaption: TcxRowCellsInfo;
begin
  Result := TcxRowCellsInfo.Create;
  Add(Result);
end;

procedure TcxRowsCaptions.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited Clear;
end;

function TcxRowsCaptions.GetCaption(Index: Integer): TcxRowCellsInfo;
begin
  Result := List[Index];
end;

{ TcxColumnsMap }

constructor TcxColumnsMap.Create;
begin
  FElements := TList.Create;
  NeedWidth := 0;
end;

destructor TcxColumnsMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    Dispose(PElementInfo(FElements.List[I]));
  FElements.Free;
  FColumnWidths := nil;
  inherited Destroy;
end;

procedure TcxColumnsMap.AddRowCell(APos: Integer; ARow: TcxCustomRow; ACellIndex, AWidth: Integer);
var
  E: PElementInfo;
begin
  New(E);
  E.Pos := APos;
  E.Width := AWidth;
  E.IsLevel := False;
  E.Row := ARow;
  E.CellIndex := ACellIndex;
  FElements.Add(E);
  if AWidth > 0 then
  begin
    New(E);
    E.Pos := APos + AWidth;
    E.Width := 0;
    E.IsLevel := False;
    E.Row := ARow;
    E.CellIndex := ACellIndex;
    FElements.Add(E);
  end;
end;

procedure TcxColumnsMap.AddLevel(ALevel, APos, AWidth: Integer);
var
  E: PElementInfo;
begin
  New(E);
  E.Pos := APos;
  E.Width := AWidth;
  E.IsLevel := True;
  E.Level := ALevel;
  FElements.Add(E);
  if AWidth > 0 then
  begin
    New(E);
    E.Pos := APos + AWidth;
    E.Width := 0;
    E.IsLevel := True;
    E.Level := ALevel;
    FElements.Add(E);
  end;
end;

function ComparePos(Item1, Item2: Pointer): Integer;
begin
  Result := PElementInfo(Item1).Pos - PElementInfo(Item2).Pos;
end;

procedure TcxColumnsMap.Build;
var
  I, AColumn, APos: Integer;
begin
  FElements.Sort(ComparePos);
  AColumn := 0;
  APos := 0;
  for I := 0 to FElements.Count - 1 do
    with PElementInfo(FElements.List[I])^ do
    begin
      if Pos > APos then
      begin
        SetLength(FColumnWidths, Succ(AColumn));
        FColumnWidths[AColumn] := Pos - APos;
        APos := Pos;
        Inc(AColumn);
      end;
      ColumnStart := AColumn;
    end;
  for I := 0 to FElements.Count - 1 do
    with PElementInfo(FElements.List[I])^ do
      if Width > 0 then ColumnEnd := FindColumnForPos(Pos + Width);
  MaxColumnIndex := AColumn - 1;
end;

procedure TcxColumnsMap.CheckNeedWidth(APos: Integer);
begin
  NeedWidth := Max(NeedWidth, APos);
end;

function TcxColumnsMap.FindColumnForPos(APos: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FElements.Count - 1 do
    with PElementInfo(FElements.List[I])^ do
      if APos = Pos then
      begin
        Result := ColumnStart;
        break;
      end;
end;

procedure TcxColumnsMap.GetColumnInfoFromRowCell(ARow: TcxCustomRow; ACellIndex: Integer; var AStart, AEnd: Integer);
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    with PElementInfo(FElements.List[I])^ do
      if not IsLevel and (Row = ARow) and (CellIndex = ACellIndex) then
      begin
        AStart := ColumnStart;
        AEnd := ColumnEnd;
        break;
      end;
end;

procedure TcxColumnsMap.GetColumnInfoFromLevel(ALevel: Integer; var AStart, AEnd: Integer);
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    with PElementInfo(FElements.List[I])^ do
      if IsLevel and (Level = ALevel) then
      begin
        AStart := ColumnStart;
        AEnd := ColumnEnd;
        break;
      end;
end;

function TcxColumnsMap.GetColumnWidth(AIndex: Integer): Integer;
begin
  Result := FColumnWidths[AIndex];
end;

{ TcxVerticalGridMapsInfo }

constructor TcxVerticalGridMapsInfo.Create(
  AOwner: TcxVerticalGridExportHelper);
begin
  FOwner := AOwner;
  FVerticalGrid := FOwner.VerticalGrid;
  FViewInfo := TcxViewInfoAccess(FVerticalGrid.ViewInfo);
  FRows := TList.Create;
  FRowsCaptions := TcxRowsCaptions.Create;
  FRowsIndents := TcxRowsIndents.Create;
  FHeaderColumnsMap := TcxColumnsMap.Create;
  FValueColumnsMap := TcxColumnsMap.Create;
  with FVerticalGrid.OptionsView do
  begin
    FGridLineColor := GridLineColor;
    FGridLines := GridLines;
    FPaintStyle := PaintStyle;
  end;
end;

destructor TcxVerticalGridMapsInfo.Destroy;
begin
  FLevelIndents := nil;
  FRows.Free;
  FHeaderColumnsMap.Free;
  FValueColumnsMap.Free;
  FRowsCaptions.Free;
  FRowsIndents.Free;
  inherited Destroy;
end;

procedure TcxVerticalGridMapsInfo.AddMultiEditorRowCells(ARow: TcxCustomRow);
var
  I, APosHeaderCell, AWidthHeaderCell, APosValueCell, AWidthValueCell: Integer;
begin
  with GetMultiEditorRowProperties(ARow) do
  begin
    if Editors.Count < 2 then Exit;
    APosHeaderCell := Succ(ARow.Level) * ViewInfo.RowIndentWidth;
    APosValueCell := 0;
    for I := 0 to Editors.Count - 1 do
    begin
      AWidthHeaderCell := Max(Editors[I].Width, FMinHeaderWidth);
      AWidthValueCell := Max(Editors[I].Width, FMinValueWidth);
      HeaderColumnsMap.CheckNeedWidth(APosHeaderCell + AWidthHeaderCell);
      ValueColumnsMap.CheckNeedWidth(APosValueCell + AWidthValueCell);
      if I = Editors.Count - 1 then break;
      HeaderColumnsMap.AddRowCell(APosHeaderCell, ARow, I, AWidthHeaderCell);
      Inc(APosHeaderCell, AWidthHeaderCell);
      ValueColumnsMap.AddRowCell(APosValueCell, ARow, I, AWidthValueCell);
      Inc(APosValueCell, AWidthValueCell);
    end;
  end;
end;

procedure TcxVerticalGridMapsInfo.AddMapRightSide(AMap: TcxColumnsMap;
  ARight, AMinCellWidth: Integer; ACalcIndent: Boolean);
var
  I, J, AWidth, APos: Integer;
  ARow: TcxCustomRow;
begin
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    APos := cxSetValue(ACalcIndent, (ARow.Level + 1) * ViewInfo.RowIndentWidth, 0);
    if ARow is TcxCustomEditorRow then
      AMap.AddRowCell(APos, ARow, 0, ARight - APos)
    else
      if ARow is TcxCustomMultiEditorRow then
      with GetMultiEditorRowProperties(ARow) do
      begin
        if Editors.Count = 0 then continue;
        for J := 0 to Editors.Count - 1 do
        begin
          AWidth := Max(Editors[J].Width, AMinCellWidth);
          if J = Editors.Count - 1 then
            AMap.AddRowCell(APos, ARow, J, ARight - APos);
          Inc(APos, AWidth);
        end;
      end;
  end;
end;

procedure TcxVerticalGridMapsInfo.AlignCategories;
var
  I: Integer;
  AProperties: TcxCaptionRowProperties;
  ARow: TcxCustomRow;
  AStart, AEnd: Integer;
  AStyle: TcxCacheCellStyle;
  AViewParams: TcxViewParams;
begin
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    if ARow.IsCategory then
    begin
      AProperties := TcxCategoryRow(ARow).Properties;
      HeaderColumnsMap.GetColumnInfoFromLevel(ARow.Level, AStart, AEnd);
      AViewParams := TcxCategoryRow(ARow).Styles.GetHeaderParams(ARow);
      cxViewParamsToCacheStyle(AViewParams, AStyle);
      AStyle.AlignText := AlignToCxAlign[AProperties.HeaderAlignmentHorz];
      cxCheckBorders(AStyle, [bTop, bBottom, bRight], GridLineColor, GridLines);
      RowsCaptions.Captions[I].AddCaption(AEnd, Size.cx - AEnd, Provider.RegisterStyle(AStyle), AProperties.Caption);
    end;
  end;
end;

procedure TcxVerticalGridMapsInfo.CalculateHeader;
var
  I, AWidth: Integer;
  ARow: TcxCustomRow;
begin
  FMinHeaderWidth := VerticalGrid.OptionsView.RowHeaderMinWidth;
  FMaxLevel := 0;
  for I := 0 to VerticalGrid.Rows.Count - 1 do
  begin
    ARow := VerticalGrid.Rows[I];
    if IsIncludeRow(ARow) then
    begin
      FRows.Add(ARow);
      if ARow is TcxCustomMultiEditorRow then
        AddMultiEditorRowCells(ARow);
      FMaxLevel := Max(FMaxLevel, ARow.Level);
    end;
  end;
  FRowCount := FRows.Count;
  AWidth := ViewInfo.RowIndentWidth;
  for I := 0 to FMaxLevel do
    HeaderColumnsMap.AddLevel(I, I * AWidth, AWidth);
  FMaxHeaderWidth := Max(HeaderColumnsMap.NeedWidth,
    VerticalGrid.OptionsView.RowHeaderWidth);
  HeaderColumnsMap.AddLevel(-1, 0, MaxHeaderWidth);
  AddMapRightSide(HeaderColumnsMap, MaxHeaderWidth, MinHeaderWidth, True);
  HeaderColumnsMap.Build;
  CalculateRowsIndents;
  CalculateRowsCaptions;
end;

procedure TcxVerticalGridMapsInfo.CalculateRowsCaptions;
var
  I, J: Integer;
  AViewParams: TcxViewParams;
  ARow: TcxCustomRow;
  ACaptions: TcxRowCellsInfo;

  procedure AddCaption(AProperties: TcxCaptionRowProperties; ACellIndex: Integer);
  var
    AStart, AEnd: Integer;
    AStyle: TcxCacheCellStyle;
  begin
    HeaderColumnsMap.GetColumnInfoFromRowCell(ARow, ACellIndex, AStart, AEnd);
    cxViewParamsToCacheStyle(AViewParams, AStyle);
    AStyle.AlignText := AlignToCxAlign[AProperties.HeaderAlignmentHorz];
    cxCheckBorders(AStyle, [bTop, bBottom, bRight], GridLineColor, GridLines);
    ACaptions.AddCaption(AStart, AEnd - AStart, Provider.RegisterStyle(AStyle), AProperties.Caption);
  end;

begin
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    ACaptions := RowsCaptions.AddCaption;
    AViewParams := GetHeaderViewParams(ARow);
    if ARow is TcxCustomEditorRow then
      AddCaption(TcxCustomEditorRowAccess(ARow).Properties, 0)
    else
      if ARow is TcxCustomMultiEditorRow then
        with GetMultiEditorRowProperties(ARow) do
          for J := 0 to Editors.Count - 1 do
            AddCaption(Editors[J], J);
  end;
end;

procedure TcxVerticalGridMapsInfo.CalculateRowsIndents;
var
  I: Integer;
  AIndents: TcxRowIndentsInfo;
begin
  for I := 0 to RowCount - 1 do
  begin
    AIndents := TcxRowIndentsInfo.Create(Rows[I]);
    AIndents.Calculate(HeaderColumnsMap, Provider);
    RowsIndents.Add(AIndents);
  end;
end;

procedure TcxVerticalGridMapsInfo.CalculateSize;
begin
  CalculateHeader;
  CalculateValuesMap;
  FRecordCount := VerticalGrid.RecordCount;
  FIsEmpty := FRecordCount = 0;
  if FIsEmpty then
    Inc(FRecordCount);
  FRecordsPerBand := Max(Owner.RecordsPerBand, 1);
  FBandCount := FRecordCount div FRecordsPerBand;
  FLastBandRecords := FRecordCount - FBandCount * FRecordsPerBand;
  if FLastBandRecords > 0 then
    Inc(FBandCount)
  else
    FLastBandRecords := FRecordsPerBand;

  FSize := cxSize(FirstValuesColumn + (ValueColumnsMap.MaxColumnIndex + 1) *
    cxSetValue(FBandCount = 1, FLastBandRecords, RecordsPerBand), (RowCount + 1) * FBandCount - 1);
end;

procedure TcxVerticalGridMapsInfo.CalculateValuesMap;
begin
  FFirstValuesColumn := HeaderColumnsMap.MaxColumnIndex + 1;
  FMinValueWidth := VerticalGrid.OptionsView.ValueMinWidth;
  FMaxValueWidth := Max(ValueColumnsMap.NeedWidth, VerticalGrid.OptionsView.ValueWidth);
  ValueColumnsMap.AddLevel(-1, 0, MaxValueWidth);
  AddMapRightSide(ValueColumnsMap, MaxValueWidth, MinValueWidth, False);
  ValueColumnsMap.Build;
end;

procedure TcxVerticalGridMapsInfo.DoCalculate;
begin
  CalculateSize;
  AlignCategories;
end;

procedure TcxVerticalGridMapsInfo.DoWrite;
begin
  Provider.SetRange(Size.cx, Size.cy, False);
  WriteColumnWidths;
  WriteValues;
end;

function TcxVerticalGridMapsInfo.GetDisplayText(ARecordIndex: Integer; ARow: TcxCustomEditorRowProperties): string;
begin
  with ARow.DisplayEditProperties[ARecordIndex] do
  begin
    if GetEditValueSource(False) = evsValue then
      Result := GetDisplayText(ARow.Values[ARecordIndex], True)
    else
      Result := ARow.DisplayTexts[ARecordIndex];
  end;
end;

procedure TcxVerticalGridMapsInfo.GetDisplayValue(ARecordIndex: Integer; ARow: TcxCustomEditorRowProperties;
  out AValue: Variant; out AValueDisplayFormat: string; out AValueDisplayFormatType: TcxValueDisplayFormatType);

  function GetField: TField;
  begin
    if ARow is TcxDBEditorRowProperties then
      Result := TcxDBEditorRowProperties(ARow).DataBinding.Field
    else
      Result := nil;
  end;

var
  AProperties: TcxCustomEditProperties;
begin
  AValueDisplayFormat := '';
  AValueDisplayFormatType := vdftAuto;
  AProperties := ARow.DisplayEditProperties[ARecordIndex];
  if IsNativeFormatProperties(AProperties) then
  begin
    AValue := ARow.Values[ARecordIndex];
    AValueDisplayFormat := cxGetDisplayFormat(AProperties, GetField);
    AValueDisplayFormatType := cxGetDisplayFormatType(AProperties, GetField);
  end
  else
    AValue := AProperties.GetDisplayText(ARow.Values[ARecordIndex], True);
end;

function TcxVerticalGridMapsInfo.GetMultiEditorRowProperties(ARow: TcxCustomRow): TcxMultiEditorRowProperties;
begin
  Result := TcxMultiEditorRowProperties(TcxCustomMultiEditorRowAccess(ARow).FProperties);
end;

function TcxVerticalGridMapsInfo.IsIncludeRow(ARow: TcxCustomRow): Boolean;
begin
  Result := VerticalGrid.IsRowVisible(ARow);
end;

function TcxVerticalGridMapsInfo.IsNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := (AProperties is TcxDateEditProperties) or (AProperties is TcxCurrencyEditProperties) or
    (AProperties is TcxSpinEditProperties) or (AProperties is TcxCalcEditProperties) or
    (AProperties is TcxTimeEditProperties);
  Result := Result or (IsRTFSupported(AProperties) and Provider.SupportRTF);
end;

procedure TcxVerticalGridMapsInfo.SetCellStyle(ACol, ARow, W, AStyleIndex: Integer);
begin
  with Provider do
    SetCellStyle(cxRectBounds(ACol, ARow, W, 1), AStyleIndex);
end;

procedure TcxVerticalGridMapsInfo.SetCellStyle(ACol, ARow, W: Integer; const AStyle: TcxCacheCellStyle);
begin
  with Provider do
    SetCellStyle(cxRectBounds(ACol, ARow, W, 1), RegisterStyle(AStyle));
end;

procedure TcxVerticalGridMapsInfo.WriteColumnWidths;
var
  I, J, AWidth, ACount: Integer;
begin
  for I := 0 to HeaderColumnsMap.MaxColumnIndex do
  begin
    AWidth := HeaderColumnsMap.GetColumnWidth(I);
    Provider.SetColumnWidth(I, AWidth);
  end;
  ACount := cxSetValue(RecordCount > FRecordsPerBand, FRecordsPerBand, FLastBandRecords);
  for J := 0 to ACount - 1 do
    for I := 0 to ValueColumnsMap.MaxColumnIndex do
    begin
      AWidth := ValueColumnsMap.GetColumnWidth(I);
      Provider.SetColumnWidth(FirstValuesColumn + J * (ValueColumnsMap.MaxColumnIndex + 1) + I, AWidth);
    end;
end;

procedure TcxVerticalGridMapsInfo.WriteHeaders(ARowIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    WriteRowHeader(ARowIndex + I, RowsIndents[I], RowsCaptions[I]);
end;

procedure TcxVerticalGridMapsInfo.WriteRecord(ACol, ARowIndex, ARecordIndex: Integer);
var
  I, J: Integer;
  ARow: TcxCustomRow;

  procedure WriteCell(ARowIndex: Integer; AProperties: TcxCustomEditorRowProperties; ACellIndex: Integer);
  var
    AStart, AEnd: Integer;
    AViewParams: TcxViewParams;
    AStyle: TcxCacheCellStyle;
  begin
    ValueColumnsMap.GetColumnInfoFromRowCell(ARow, ACellIndex, AStart, AEnd);
    AViewParams := TcxEditorRowStyles(TcxCustomRowAccess(ARow).Styles).GetContentParams(AProperties, False, ARecordIndex);
    cxViewParamsToCacheStyle(AViewParams, AStyle);
    AStyle.AlignText := AlignToCxAlign[TcxPropertiesAccess(AProperties.DisplayEditProperties[ARecordIndex]).Alignment.Horz];
    cxCheckBorders(AStyle, cxBordersAll, GridLineColor, GridLines);
    SetCellStyle(ACol + AStart, ARowIndex, AEnd - AStart, AStyle);
    WriteValue(ACol + AStart, ARowIndex, ARecordIndex, AProperties);
  end;

begin
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    if ARow is TcxCustomEditorRow then
      WriteCell(ARowIndex + I, TcxCustomEditorRowAccess(ARow).Properties, 0)
    else
      if ARow is TcxCustomMultiEditorRow then
      with GetMultiEditorRowProperties(ARow) do
        for J := 0 to Editors.Count - 1 do
          WriteCell(ARowIndex + I, Editors[J], J)
  end;
end;

procedure TcxVerticalGridMapsInfo.WriteRowCaptions(ARowIndex: Integer; ACaptions: TcxRowCellsInfo);
var
  I: Integer;
begin
  with Provider do
    for I := 0 to ACaptions.Count - 1 do
      with ACaptions[I] do
      begin
        SetCellStyle(cxRectBounds(Column, ARowIndex, Width, 1), StyleIndex);
        SetCellValueAsString(Column, ARowIndex, Caption);
      end;
end;

procedure TcxVerticalGridMapsInfo.WriteRowHeader(ARowIndex: Integer; AIndents: TcxRowIndentsInfo; ACaptions: TcxRowCellsInfo);
const
  PlusMinus: array[Boolean] of Char = ('+', '-');
var
  I: Integer;
begin
  for I := 0 to AIndents.Count - 1 do
    with AIndents.Indents[I] do
    begin
      if (I = 0) and AIndents.Row.HasVisibleChildren then
        Provider.SetCellValueAsString(Column, ARowIndex, PlusMinus[AIndents.Row.Expanded]);
      SetCellStyle(Column, ARowIndex, Width, StyleIndex);
    end;
  WriteRowCaptions(ARowIndex, ACaptions);
end;

procedure TcxVerticalGridMapsInfo.WriteValue(ACol, ARow, ARecordIndex: Integer; AProperties: TcxCustomEditorRowProperties);
var
  AValue: Variant;
  AValueDisplayFormat: string;
  AValueDisplayFormatType: TcxValueDisplayFormatType;
begin
  if IsEmpty then
    Provider.SetCellValueAsString(ACol, ARow, '')
  else
    if Owner.IsNativeFormat then
    begin
      GetDisplayValue(ARecordIndex, AProperties, AValue, AValueDisplayFormat, AValueDisplayFormatType);
      Provider.SetCellValue(ACol, ARow, AValue, AValueDisplayFormat, AValueDisplayFormatType);
    end
    else
      Provider.SetCellValueAsString(ACol, ARow, GetDisplayText(ARecordIndex, AProperties));
end;

procedure TcxVerticalGridMapsInfo.WriteValues;
var
  I, J, ARow, ACol, ARecord: Integer;
begin
  ARow := 0;
  ARecord := 0;
  Owner.ProgressHelper.BeginStage(FBandCount * FRecordsPerBand);
  try
    for I := 0 to FBandCount - 1 do
    begin
      ACol := HeaderColumnsMap.MaxColumnIndex + 1;
      WriteHeaders(ARow);
      for J := 0 to FRecordsPerBand - 1 do
      begin
        WriteRecord(ACol + J * (ValueColumnsMap.MaxColumnIndex + 1), ARow, ARecord);
        Inc(ARecord);
        if ARecord = RecordCount then
          Exit;
        Owner.ProgressHelper.NextTask;
      end;
      Inc(ARow, RowCount + 1);
      Provider.SetRowHeight(ARow - 1, 8);
    end;
  finally
    Owner.ProgressHelper.EndStage;
  end;
end;

function TcxVerticalGridMapsInfo.GetLevelIndent(Index: Integer): Integer;
begin
  Result := FLevelIndents[Index];
end;

function TcxVerticalGridMapsInfo.GetProvider: IcxExportProvider;
begin
  Result := FOwner.Provider;
end;

function TcxVerticalGridMapsInfo.GetRow(Index: Integer): TcxCustomRow;
begin
  Result := TcxCustomRow(FRows.List[Index]);
end;

{ TcxVerticalGridExportHelper }

constructor TcxVerticalGridExportHelper.Create(AVerticalGrid: TcxCustomVerticalGrid;
  AExportType: Integer; const AFileName: string; AHandler: TObject);

  function DefaultStyle: TcxCacheCellStyle;
  var
    I: Integer;
  begin
    cxViewParamsToCacheStyle(VerticalGrid.Styles.GetBackgroundParams, Result);
    for I := 0 to 3 do
    begin
      Result.Borders[I].IsDefault := True;
      Result.Borders[I].Width := 0;
    end;
  end;

begin
  FHandler := AHandler;
  FVerticalGrid := TcxVerticalGridAccess(AVerticalGrid);
  FProgressHelper := TcxProgressCalculationHelper.Create(2, AVerticalGrid, ProgressHandler);
  TcxExport.Provider(AExportType, AFileName).GetInterface(IcxExportProvider, FProvider);
  FProvider.SetDefaultStyle(DefaultStyle);
  MapsInfo := GetMapsInfoClass.Create(Self);
end;

destructor TcxVerticalGridExportHelper.Destroy;
begin
  try
    FreeAndNil(MapsInfo);
    FreeAndNil(FProgressHelper);
    FProvider := nil;
  finally
    inherited Destroy;
  end;
end;

procedure TcxVerticalGridExportHelper.DoCalculateTableMap;
begin
  MapsInfo.DoCalculate;
end;

procedure TcxVerticalGridExportHelper.DoWriteCells;
begin
  MapsInfo.DoWrite;
  Provider.Commit(ProgressHelper, FHandler);
end;

function TcxVerticalGridExportHelper.GetMapsInfoClass: TcxVerticalGridMapsInfoClass;
begin
  Result := TcxVerticalGridMapsInfo;
end;

procedure TcxVerticalGridExportHelper.ProgressHandler(Sender: TObject; Percents: Integer);
var
  AIntf: IcxExportProgress;
begin
  if Supports(FHandler, IcxExportProgress, AIntf) then
    AIntf.OnProgress(Sender, Percents);
end;

// external procedures definition

function cxSupports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

procedure cxExportVGToFile(AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExportType: Integer;
  AExpand, AUseNativeFormat: Boolean; const ASeparators: array of string; ARecordPerBand: Integer;
  const AFileExt: string; AHandler: TObject; AEncoding: TEncoding);
var
  AEncodingIntf: IcxExportProviderEncoding;
  AIntf: IcxExportWithSeparators;
  I: Integer;
begin
  if AFileExt <> '' then
    AFileName := ChangeFileExt(AFileName, '.' + AFileExt);
  if not AVerticalGrid.Visible then
    cxVerticalGridError(cxSvgExportNotVisibleControl);
  with TcxVerticalGridExportHelper.Create(AVerticalGrid, AExportType, AFileName, AHandler) do
  try
    FExpand := AExpand;
    if AExpand then
      AVerticalGrid.FullExpand;
    AVerticalGrid.FocusedRow := nil;
    FRecordsPerBand := Max(ARecordPerBand, 1);
    FIsNativeFormat :=  AUseNativeFormat;
    if cxSupports(Provider, IcxExportProviderEncoding, AEncodingIntf) then
      AEncodingIntf.SetEncoding(AEncoding);
    if cxSupports(Provider, IcxExportWithSeparators, AIntf) and (Length(ASeparators) > 0) then
    begin
      for I := Low(ASeparators) to High(ASeparators) do
        AIntf.AddSeparator(ASeparators[I]);
    end;
    DoCalculateTableMap;
    DoWriteCells;
  finally
    Free;
  end;
end;

procedure cxExportVGToHTML(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'html'; AHandler: TObject = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToHtml, AExpand, False, [], ARecordPerBand, AFileExt, AHandler);
end;

procedure cxExportVGToXML(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'xml'; AHandler: TObject = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToXML, AExpand, False, [], ARecordPerBand, AFileExt, AHandler);
end;

procedure cxExportVGToExcel(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  AUseNativeFormat: Boolean = True; ARecordPerBand: Integer = 8; const AFileExt: string = 'xls';
  AHandler: TObject = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToExcel,
    AExpand, AUseNativeFormat, [], ARecordPerBand, AFileExt, AHandler);
end;

procedure cxExportVGToXLSX(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid;
  AExpand: Boolean = True; AUseNativeFormat: Boolean = True; ARecordPerBand: Integer = 8;
  const AFileExt: string = 'xlsx'; AHandler: TObject = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToXlsx,
    AExpand, AUseNativeFormat, [], ARecordPerBand, AFileExt, AHandler);
end;

procedure cxExportVGToCSV(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid;
  AExpand: Boolean = True; const ASeparator: Char = ','; ARecordPerBand: Integer = 8;
  const AFileExt: string = 'csv'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToCSV, AExpand,
    False, [ASeparator], ARecordPerBand, AFileExt, AHandler, AEncoding);
end;

procedure cxExportVGToText(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean;
  ARecordPerBand: Integer = 8; const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportVGToText(AFileName, AVerticalGrid, AExpand, '', '', '', ARecordPerBand, AFileExt, AHandler, AEncoding);
end;

procedure cxExportVGToText(const AFileName: string; AVerticalGrid: TcxCustomVerticalGrid; AExpand: Boolean = True;
  const ASeparator: string = ''; const ABeginString: string = ''; const AEndString: string = ''; ARecordPerBand: Integer = 8;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportVGToFile(AFileName, AVerticalGrid, cxExportToText, AExpand, False,
    [ASeparator, ABeginString, AEndString], ARecordPerBand, AFileExt, AHandler, AEncoding);
end;

initialization
  cxIndentStyle.FontName := cxIndentFontName;
end.

