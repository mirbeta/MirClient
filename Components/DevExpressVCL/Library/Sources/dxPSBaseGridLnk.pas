{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSBaseGridLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Classes, Windows, Graphics, Controls, StdCtrls, ImgList, dxCore, dxPSCore, dxPSGlbl,
  dxPrnPg, dxPSRes, dxBase, dxPSFillPatterns, cxDrawTextUtils, cxDropDownEdit,
  dxPSReportRenderCanvas, cxGeometry, cxImageComboBox;

type

  TAbstractdxGridReportLink = class;
  TdxPSCustomGridCellDataMapClass = class of TdxPSCustomGridCellDataMap;
  TdxPSCustomGridCellDataMap = class;

  { DataMaps }

  TdxPSGridCellDataMaps = class(TdxCustomClassMaps)
  private
    function GetMapClass(ADataItem: TAbstractdxReportCellData): TdxPSCustomGridCellDataMapClass;
  public
    class function Instance: TdxPSGridCellDataMaps; reintroduce; overload;
    procedure InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
      AReportLink: TAbstractdxGridReportLink);
    property MapClasses[ADataItem: TAbstractdxReportCellData]: TdxPSCustomGridCellDataMapClass read GetMapClass; default;
  end;

  TdxPSCustomGridCellDataMap = class(TdxCustomClassMapItem)
  protected
    class procedure InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
      AReportLink: TAbstractdxGridReportLink); virtual;
    class function DataClass: TdxReportCellDataClass; virtual;
  public
    class function PairClass: TClass; override;

    class procedure Register;
    class procedure Unregister;
  end;

  TdxPSTextGridCellDataMap = class(TdxPSCustomGridCellDataMap)
  protected
    class procedure InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
      AReportLink: TAbstractdxGridReportLink); override;
    class function DataClass: TdxReportCellDataClass; override;
  end;

  TdxPSImageGridCellDataMap = class(TdxPSTextGridCellDataMap)
  protected
    class procedure InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
      AReportLink: TAbstractdxGridReportLink); override;
    class function DataClass: TdxReportCellDataClass; override;
  end;

  { ReportLink }

  TdxGridDrawMode = (gdmStrict, gdmOddEven, gdmChess, gdmBorrowSource);
  TdxGridDrawModes = set of TdxGridDrawMode;

  TdxCustomGridReportLinkEvent = procedure(Sender: TAbstractdxGridReportLink) of object;

  TdxCustomGridReportLinkInitializeItemEvent = procedure (Sender: TAbstractdxGridReportLink;
    ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData) of object;

  TAbstractdxGridReportLink = class(TBasedxReportLink)
  private
    FAutoWidth: Boolean;
    FDefaultFixedFont: TFont;
    FDelimitersHardHorz: TList;
    FDelimitersHardVert: TList;
    FDrawMode: TdxGridDrawMode;
    FEffects3D: Boolean;
    FEndEllipsis: Boolean;
    FEvenColor: TColor;
    FEvenFont: TFont;
    FFixedColor: TColor;
    FFixedFont: TFont;
    FFixedTransparent: Boolean;
    FGridLineColor: TColor;
    FIncludeFixed: Boolean;
    FMultiline: Boolean;
    FOnlySelected: Boolean;
    FReportRows: TList;
    FRowAutoHeight: Boolean;
    FRowWidth: Integer;
    FScreenCanvas: TdxPSReportRenderCustomCanvas;
    FSoft3D: Boolean;
    FSupportedCustomDraw: Boolean;
    FUseCustomPageBreaks: Boolean;

    FOnGetCustomPageBreaks: TdxCustomGridReportLinkEvent;
    FOnInitializeItem: TdxCustomGridReportLinkInitializeItemEvent;

    function GetActualFirstCol: Integer;
    function GetActualFirstRow: Integer;
    function GetActualLastCol: Integer;
    function GetActualLastRow: Integer;
    function GetAvailableSiteHeight: Integer;
    function GetAvailableSiteWidth: Integer;
    function GetColumnWidth(Index: Integer): Integer;
    function GetOddColor: TColor;
    function GetOddFont: TFont;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowByRow(Row: Integer): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetRowHeight(ARow: Integer): Integer;
    function GetRowWidth: Integer;
    procedure SetAutoWidth(Value: Boolean);
    procedure SetColumnWidth(Index: Integer; Value: Integer);
    procedure SetEffects3D(Value: Boolean);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetEvenColor(Value: TColor);
    procedure SetEvenFont(Value: TFont);
    procedure SetFixedColor(Value: TColor);
    procedure SetFixedFont(Value: TFont);
    procedure SetFixedTransparent(Value: Boolean);
    procedure SetGridLineColor(Value: TColor);
    procedure SetIncludeFixed(Value: Boolean);
    procedure SetMultiline(Value: Boolean);
    procedure SetOddColor(Value: TColor);
    procedure SetOddFont(Value: TFont);
    procedure SetOnlySelected(Value: Boolean);
    procedure SetRowAutoHeight(Value: Boolean);
    procedure SetRowHeight(Index: Integer; Value: Integer);
    procedure SetSoft3D(Value: Boolean);
    procedure SetSupportedCustomDraw(Value: Boolean);
    procedure SetUseCustomPageBreaks(Value: Boolean);
  protected
    FColumnWidths: array of Integer;
    FCurrentCol: Integer;
    FCurrentRow: Integer;
    FEvenFontIndex: Integer;
    FFixedFontIndex: Integer;
    FRowHeights: array of Integer;

    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConstructReportContent(AReportCells: TdxReportCells); virtual;
    procedure ConvertCoords; override;
    function GetBreakPagesByHardDelimiters: Boolean; override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    function GetUseHardHorzDelimiters: Boolean; override;
    function GetUseHardVertDelimiters: Boolean; override;
    procedure InternalRestoreDefaults; override;
    function IsSupportedCustomDraw(AItem: TAbstractdxReportCellData): Boolean; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;
    procedure MakeHardDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    procedure DoGetCustomPageBreaks; dynamic;
    procedure DoInitializeItem(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); dynamic;

    procedure AssignData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); virtual;
    function CreateDataItem(AParent: TdxReportCell; ACol, ARow: Integer; const ABounds: TRect): TAbstractdxReportCellData; virtual;
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; virtual;

    procedure CalculateColumnAutoWidths; virtual;
    procedure CalculateColumnWidths; virtual;
    procedure CalculateRowHeights(AReportCells: TdxReportCells); virtual;
    function CalculateRowWidth: Integer; virtual;
    function CanCalculateColumnAutoWidths: Boolean; virtual;
    function CanCalculateRowAutoHeight(ARow: Integer): Boolean; virtual;

    function GetRowClass(ARow: Integer): TdxReportCellClass; virtual;
    function GetRowParent(AReportCells: TdxReportCells; ARow: Integer): TdxReportCell; virtual;
    procedure InitializeRow(AReportRow: TdxReportCell; ARow: Integer); virtual;

    function GetColCount: Integer; virtual;
    function GetFixedColCount: Integer; virtual;
    function GetFixedRowCount: Integer; virtual;
    function GetRowCount: Integer; virtual;

    function GetActualColCount: Integer; virtual;
    function GetActualColIndex(Col: Integer): Integer; virtual;
    function GetActualRowCount: Integer; virtual;
    function GetCellColor(ACol, ARow: Integer): TColor; virtual;
    procedure GetCellColRow(AItem: TdxReportVisualItem; var ACol, ARow: Integer);
    function GetCellContentBkColor(ACol, ARow: Integer): TColor; virtual;
    function GetCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass; virtual;
    function GetCellEdge3DSoft(AItem: TAbstractdxReportCellData; ACol, ARow: Integer): Boolean; virtual;
    function GetCellEdge3DStyle(AItem: TAbstractdxReportCellData; ACol, ARow: Integer): TdxCellEdgeStyle; virtual;
    function GetCellEdgeMode(AItem: TAbstractdxReportCellData; ACol, ARow: Integer): TdxCellEdgeMode; virtual;
    function GetCellFont(ACol, ARow: Integer): TFont; virtual;
    function GetCellFontIndex(ACol, ARow: Integer): Integer; virtual;
    function GetCellHasImage(ACol, ARow: Integer): Boolean; virtual;
    function GetCellImage(ACol, ARow: Integer): TGraphic; virtual;
    function GetCellImageIndex(ACol, ARow: Integer): Integer; virtual;
    function GetCellImageLayout(ACol, ARow: Integer): TdxImageLayout; virtual;
    function GetCellImageList(ACol, ARow: Integer): TCustomImageList; virtual;
    function GetCellImageMakeSpaceForEmpty(ACol, ARow: Integer): Boolean; virtual;
    function GetCellImageTransparent(ACol, ARow: Integer): Boolean; virtual;
    function GetCellMultiline(ACol, ARow: Integer): Boolean; virtual;
    function GetCellSides(ACol, ARow: Integer): TdxCellSides; virtual;
    function GetCellText(ACol, ARow: Integer): string; virtual; abstract;
    function GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; virtual;
    function GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; virtual;
    function GetCellTransparent(ACol, ARow: Integer): Boolean; virtual;
    function GetColSortOrder(ACol: Integer): TdxCellSortOrder; virtual;
    function GetEndEllipsis: Boolean; virtual;
    function GetInternalRowHeight(Index: Integer): Integer;
    function GetMinRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont): Integer; virtual;
    function GetMultiline: Boolean; virtual;

    procedure GetSelectedRange(ABeginCol, AEndCol, ABeginRow, AEndRow: PInteger); overload; virtual;
    procedure GetSelectedRange(out ABeginCol, AEndCol, ABeginRow, AEndRow: Integer); overload; virtual;
    function GetSelectedColCount: Integer; virtual;
    function GetSelectedRowCount: Integer; virtual;
    function GetSelectionRect: TRect; virtual;

    function GetSourceCellColor(ACol, ARow: Integer): TColor; virtual;
    function GetSourceCellContentBkColor(ACol, ARow: Integer): TColor; virtual;
    function GetSourceCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass; virtual;
    function GetSourceCellEdge3DSoft(ACol, ARow: Integer): Boolean; virtual;
    function GetSourceCellEdge3DStyle(ACol, ARow: Integer): TdxCellEdgeStyle; virtual;
    function GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode; virtual;
    function GetSourceCellFont(ACol, ARow: Integer): TFont; virtual;
    function GetSourceCellFontIndex(ACol, ARow: Integer): Integer; virtual;
    function GetSourceCellMultiline(ACol, ARow: Integer): Boolean; virtual;
    function GetSourceCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; virtual;
    function GetSourceCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; virtual;
    function GetSourceCellTransparent(ACol, ARow: Integer): Boolean; virtual;
    function GetSourceColWidth(ACol: Integer): Integer; virtual;
    function GetSourceRowHeight(ARow: Integer): Integer; virtual;

    function HasColumnHeaderImage(ACol: Integer): Boolean; virtual;
    function HasSelection: Boolean; virtual;
    function HasSelectionInCol(ACol: Integer): Boolean; virtual;
    function HasSelectionInRow(ARow: Integer): Boolean; virtual;
    function IsDrawBorder: Boolean; virtual;
    function IsDrawFixedHorzLines: Boolean; virtual;
    function IsDrawFixedVertLines: Boolean; virtual;
    function IsDrawHorzLines: Boolean; virtual;
    function IsDrawVertLines: Boolean; virtual;
    function IsEmptyRow(ARow: Integer): Boolean; virtual;
    function IsFixedCell(ACol, ARow: Integer): Boolean; virtual;
    function IsFixedCol(ACol: Integer): Boolean; virtual;
    function IsFixedRow(ARow: Integer): Boolean; virtual;
    function IsFooterRow(ARow: Integer): Boolean; virtual;
    function IsHeaderRow(ARow: Integer): Boolean; virtual;
    function IsProcessedCell(ACol, ARow: Integer): Boolean; virtual;
    function IsProcessedCol(ACol: Integer): Boolean; virtual;
    function IsProcessedRow(ARow: Integer): Boolean; virtual;
    function IsSelectedCell(ACol, ARow: Integer): Boolean; virtual;
    function IsSelectedRow(ARow: Integer): Boolean; virtual;

    procedure NextCol; virtual;
    procedure NextRow; virtual;
    procedure SetDrawMode(Value: TdxGridDrawMode); virtual;
    procedure SetupBoundsRect(ACell: TdxReportCell);

    procedure AddReportRow(ARow: TdxReportCell);
    procedure AddHorizontalHardDelimiter(ADelimiter: Integer);
    procedure AddVerticalHardDelimiter(ADelimiter: Integer); overload;
    procedure AddVerticalHardDelimiter(ADelimiter: TdxReportCell); overload;

    procedure AfterConstruct(AReportCells: TdxReportCells); virtual;
    procedure BeforeConstruct(AReportCells: TdxReportCells); virtual;
    procedure PrepareConstruct(AReportCells: TdxReportCells); virtual;
    procedure UnprepareConstruct(AReportCells: TdxReportCells); virtual;

    procedure InitializeDefaultFixedFont(AFont: TFont); virtual;
    function IsFixedFontStored: Boolean;

    property ActualColIndexes[Col: Integer]: Integer read GetActualColIndex;
    property ActualColCount: Integer read GetActualColCount;
    property ActualFirstCol: Integer read GetActualFirstCol;
    property ActualFirstRow: Integer read GetActualFirstRow;
    property ActualLastCol: Integer read GetActualLastCol;
    property ActualLastRow: Integer read GetActualLastRow;
    property ActualRowCount: Integer read GetActualRowCount;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property AvailableSiteHeight: Integer read GetAvailableSiteHeight;
    property AvailableSiteWidth: Integer read GetAvailableSiteWidth;
    property ColCount: Integer read GetColCount;
    property ColumnWidths[Index: Integer]: Integer read GetColumnWidth write SetColumnWidth;
    property DelimitersHardHorz: TList read FDelimitersHardHorz;
    property DelimitersHardVert: TList read FDelimitersHardVert;
    property DrawMode: TdxGridDrawMode read FDrawMode write SetDrawMode default gdmStrict;
    property Effects3D: Boolean read FEffects3D write SetEffects3D default False;
    property EndEllipsis: Boolean read GetEndEllipsis write SetEndEllipsis default False;
    property EvenColor: TColor read FEvenColor write SetEvenColor default clWhite;
    property EvenFont: TFont read FEvenFont write SetEvenFont stored IsFontStored;
    property FixedColCount: Integer read GetFixedColCount;
    property FixedColor: TColor read FFixedColor write SetFixedColor default clBtnFace; {dxDefaultFixedColor}
    property FixedFont: TFont read FFixedFont write SetFixedFont stored IsFixedFontStored;
    property FixedRowCount: Integer read GetFixedRowCount;
    property FixedTransparent: Boolean read FFixedTransparent write SetFixedTransparent default False;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clBlack;
    property IncludeFixed: Boolean read FIncludeFixed write SetIncludeFixed default True;
    property InternalRowHeights[Index: Integer]: Integer read GetInternalRowHeight;
    property Multiline: Boolean read FMultiline write SetMultiline default False;
    property OddColor: TColor read GetOddColor write SetOddColor stored IsFontStored default clWhite;
    property OddFont: TFont read GetOddFont write SetOddFont stored IsFontStored;
    property OnlySelected: Boolean read FOnlySelected write SetOnlySelected default False;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property ReportRowsByRow[ARow: Integer]: TdxReportCell read GetReportRowByRow;
    property RowAutoHeight: Boolean read FRowAutoHeight write SetRowAutoHeight default False;
    property RowCount: Integer read GetRowCount;
    property RowHeights[Index: Integer]: Integer read GetRowHeight write SetRowHeight;
    property RowWidth: Integer read GetRowWidth;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read FScreenCanvas;
    property SelectedColCount: Integer read GetSelectedColCount;
    property SelectedRowCount: Integer read GetSelectedRowCount;
    property SelectionRect: TRect read GetSelectionRect;
    property Soft3D: Boolean read FSoft3D write SetSoft3D default True;
    property SupportedCustomDraw: Boolean read FSupportedCustomDraw write SetSupportedCustomDraw default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Vertical Page Breaks
    procedure AddPageBreak(ARow: Integer); overload; virtual;
    procedure AddPageBreak(ARows: PIntegerArray; ACount: Integer); overload;
    procedure AddPageBreak(ARows: PIntegerArray; AStartIndex, ACount: Integer); overload;
    procedure AddPageBreak(ARows: TList); overload; // List of Integers -> Row Indexes
    procedure AddPageBreak(ARows: TList; AStartIndex, ACount: Integer); overload;
    procedure AddPageBreak(const ARows: array of Integer); overload;
    procedure AddPageBreak(const ARows: array of Integer; AStartIndex, ACount: Integer); overload;
    procedure AddPageBreak(const ARows: TBoundArray); overload;
    procedure AddPageBreak(const ARows: TBoundArray; AStartIndex, ACount: Integer); overload;
    // Horizontal Page Breaks
    procedure AddHorizontalPageBreak(AColumn: Integer); overload; virtual;
    procedure AddHorizontalPageBreak(AColumns: PIntegerArray; ACount: Integer); overload;
    procedure AddHorizontalPageBreak(AColumns: PIntegerArray; AStartIndex, ACount: Integer); overload;
    procedure AddHorizontalPageBreak(AColumns: TList); overload; // List of Integers -> Column Indexes
    procedure AddHorizontalPageBreak(AColumns: TList; AStartIndex, ACount: Integer); overload;
    procedure AddHorizontalPageBreak(const AColumns: array of Integer); overload;
    procedure AddHorizontalPageBreak(const AColumns: array of Integer; AStartIndex, ACount: Integer); overload;
    procedure AddHorizontalPageBreak(const AColumns: TBoundArray); overload;
    procedure AddHorizontalPageBreak(const AColumns: TBoundArray; AStartIndex, ACount: Integer); overload;

    function DefaultFixedFont: TFont; virtual;

    property Color;
    property Font;
    property ScaleFonts;
    property Transparent;
    property UseCustomPageBreaks: Boolean read FUseCustomPageBreaks write SetUseCustomPageBreaks default False;

    property OnGetCustomPageBreaks: TdxCustomGridReportLinkEvent read FOnGetCustomPageBreaks write FOnGetCustomPageBreaks;
    property OnInitializeItem: TdxCustomGridReportLinkInitializeItemEvent read FOnInitializeItem write FOnInitializeItem;
  end;

function dxPSGetSelectedDrawMode(AComboBox: TcxImageComboBox): TdxGridDrawMode;
procedure dxPSInitalizeDrawModeCombo(AComboBox: TcxImageComboBox; const AModes: TdxGridDrawModes);
procedure dxPSSyncDrawModeComboItemIndex(AComboBox: TcxImageComboBox; AMode: TdxGridDrawMode);

procedure dxPSDrawGridPreview(ACanvas: TCanvas; R: TRect; AReportLink: TAbstractdxGridReportLink;
  AShowFixedColumn: Boolean = True; AShowFixedRow: Boolean = True; AScaleFactor: TdxScaleFactor = nil);

implementation

uses
  Forms, SysUtils, Math,
  cxClasses, dxPSUtl, dxPSImgs, cxGraphics, dxDPIAwareUtils;

const
  DrawModeNamesMap: array[TdxGridDrawMode] of Pointer =
    (@sdxDrawModeStrict, @sdxDrawModeOddEven, @sdxDrawModeChess, @sdxDrawModeBorrow);

procedure dxPSInitalizeDrawModeCombo(AComboBox: TcxImageComboBox; const AModes: TdxGridDrawModes);

  procedure PopulateItems(AItems: TcxImageComboBoxItems);
  var
    AItem: TcxImageComboBoxItem;
    AMode: TdxGridDrawMode;
  begin
    AItems.BeginUpdate;
    try
      AItems.Clear;
      for AMode := Low(TdxGridDrawMode) to High(TdxGridDrawMode) do
        if AMode in AModes then
        begin
          AItem := AItems.Add;
          AItem.ImageIndex := Ord(AMode);
          AItem.Description := cxGetResourceString(DrawModeNamesMap[AMode]);
          AItem.Value := Integer(AMode);
          AItem.Tag := TcxTag(AMode);
        end;
    finally
      AItems.EndUpdate;
    end;
  end;

begin
  AComboBox.Properties.BeginUpdate;
  try
    AComboBox.Properties.Images := dxPSDrawModeImages;
    PopulateItems(AComboBox.Properties.Items);
  finally
    AComboBox.Properties.EndUpdate;
  end;
end;

function dxPSGetSelectedDrawMode(AComboBox: TcxImageComboBox): TdxGridDrawMode;
begin
  if AComboBox.ItemIndex < 0 then
    Result := gdmStrict
  else
    Result := TdxGridDrawMode(AComboBox.Properties.Items[AComboBox.ItemIndex].Tag);
end;

procedure dxPSSyncDrawModeComboItemIndex(AComboBox: TcxImageComboBox; AMode: TdxGridDrawMode);
var
  I: Integer;
begin
  for I := 0 to AComboBox.Properties.Items.Count - 1 do
    if AComboBox.Properties.Items[I].Tag = TcxTag(AMode) then
    begin
      AComboBox.ItemIndex := I;
      Break;
    end;
end;

function GetDesignerString(ACol, ARow: Integer): string;
const
  sdxGridStrings: array[0..4, 0..6] of string =
    (('7', '12', '27', '11', '11', '16',  '84'),
    ( '8',  '6', '17', '12', '11', '16',  '70'),
    ('23', '32', '21', '15', '10', '26', '127'),
    ('22', '12', '12', '32', '32', '12', '122'),
    ('60', '62', '77', '70', '64', '70', '403'));
begin
  if (ARow = 0) or (ACol = 0) then
  begin
    if ARow = 0 then
      if ACol = 0 then
        Result := ''
      else
        case ACol of
          1: Result := cxGetResourceString(@sdxEast);
          2: Result := cxGetResourceString(@sdxWest);
          3: Result := cxGetResourceString(@sdxSouth);
          4: Result := cxGetResourceString(@sdxNorth);
        else
          Result := cxGetResourceString(@sdxTotal);
        end
    else
      case ARow of
        1: Result := cxGetResourceString(@sdxJanuaryShort);
        2: Result := cxGetResourceString(@sdxFebruaryShort);
        3: Result := cxGetResourceString(@sdxMarchShort);
        4: Result := cxGetResourceString(@sdxAprilShort);
        5: Result := cxGetResourceString(@sdxMayShort);
        6: Result := cxGetResourceString(@sdxJuneShort);
      else
        Result := cxGetResourceString(@sdxTotal);
      end;
  end
  else
    Result := sdxGridStrings[ACol - 1,  ARow - 1];
end;

procedure dxPSDrawGridPreview(ACanvas: TCanvas; R: TRect; AReportLink: TAbstractdxGridReportLink;
  AShowFixedColumn: Boolean = True; AShowFixedRow: Boolean = True; AScaleFactor: TdxScaleFactor = nil);
const
  PreviewDefaultColCount = 6;
  PreviewDefaultRowCount = 8;

  function ColCount: Integer;
  begin
    Result := PreviewDefaultColCount - Ord(not AShowFixedColumn);
  end;

  function RowCount: Integer;
  begin
    Result := PreviewDefaultRowCount - Ord(not AShowFixedRow);
  end;

  function FixedColumnCount: Integer;
  begin
    Result := 1 - Ord(not AShowFixedColumn);
  end;

  function FixedRowCount: Integer;
  begin
    Result := 1 - Ord(not AShowFixedRow);
  end;

  function IsFixedCol(ACol: Integer): Boolean;
  begin
    Result := AShowFixedColumn and (ACol = 0);
  end;

  function IsFixedRow(ARow: Integer): Boolean;
  begin
    Result := AShowFixedRow and (ARow = 0);
  end;

  function IsFixedCell(ACol, ARow: Integer): Boolean;
  begin
    Result := IsFixedCol(ACol) or IsFixedRow(ARow);
  end;

  procedure FrameRect(DC: HDC; var R: TRect; ABrush: HBRUSH; ASides: TdxCellSides);
  var
    R2: TRect;
  begin
    if csLeft in ASides then
    begin
      R2 := Rect(R.Left - 1, R.Top - 1, R.Left, R.Bottom);
      FillRect(DC, R2, ABrush);
    end;

    if csTop in ASides then
    begin
      R2 := Rect(R.Left - 1, R.Top - 1, R.Right, R.Top);
      FillRect(DC, R2, ABrush);
    end;

    if csRight in ASides then
    begin
      R2 := Rect(R.Right - 1, R.Top - 1, R.Right, R.Bottom);
      FillRect(DC, R2, ABrush);
    end;

    if csBottom in ASides then
    begin
      R2 := Rect(R.Left - 1, R.Bottom - 1, R.Right, R.Bottom);
      FillRect(DC, R2, ABrush);
    end;

    if csRight in ASides then
      Dec(R.Right);
    if csBottom in ASides then
      Dec(R.Bottom);
  end;

  function GetCellSides(ACol, ARow: Integer): TdxCellSides;
  var
    ABeginCol, AEndCol, ABeginRow, AEndRow: Integer;
  begin
    Result := csAll;
    if not AReportLink.IsDrawBorder then
    begin
      if ACol = 0 then
        Exclude(Result, csLeft);
      if ACol = ColCount - 1 then
        Exclude(Result, csRight);
      if ARow = 0 then
        Exclude(Result, csTop);
      if ARow = RowCount - 1 then
        Exclude(Result, csBottom);
    end;

    if IsFixedCell(ACol, ARow) then
    begin
      if not AReportLink.IsDrawFixedHorzLines then
      begin
        if ARow > 0 then
          Exclude(Result, csTop);
        if IsFixedCol(ACol) then
        begin
          if ARow < RowCount - 1 then
            Exclude(Result, csBottom)
        end
        else
          if ARow < FixedRowCount - Byte(AReportLink.IsDrawHorzLines) then
            Exclude(Result, csBottom);
      end;
      if not AReportLink.IsDrawFixedVertLines then
      begin
        if ACol > 0 then
          Exclude(Result, csLeft);
        if IsFixedRow(ARow) then
        begin
          if ACol < ColCount - 1 then
            Exclude(Result, csRight)
        end
        else
          if ACol < FixedColumnCount - Byte(AReportLink.IsDrawVertLines) then
            Exclude(Result, csRight);
      end;
    end
    else
    begin
      if not AReportLink.IsDrawHorzLines then
      begin
        ABeginRow := 0;
        AEndRow := RowCount - 1;
        if ARow < AEndRow then
          if ARow > ABeginRow then
            Result := Result - [csTop, csBottom]
          else
            Exclude(Result, csBottom)
        else
          if ARow > ABeginRow then
            Exclude(Result, csTop);
      end;
      if not AReportLink.IsDrawVertLines then
      begin
        ABeginCol := 0;
        AEndCol := ColCount - 1;
        if ACol < AEndCol then
          if ACol > ABeginCol then
            Result := Result - [csLeft, csRight]
          else
            Exclude(Result, csRight)
        else
          if ACol > ABeginCol then
            Exclude(Result, csLeft)
      end;
    end;
  end;

  function GetCellFont(I, J: Integer): TFont;
  begin
    if IsFixedCell(I, J) then
      Result := AReportLink.FixedFont
    else
      case AReportLink.DrawMode of
        gdmStrict:
           Result := AReportLink.Font;
        gdmOddEven:
          if Odd(J) then
            Result := AReportLink.OddFont
          else
            Result := AReportLink.EvenFont;
      else
        if not Odd((I - FixedColumnCount) + (J - FixedRowCount)) then
          Result := AReportLink.OddFont
        else
          Result := AReportLink.EvenFont;
      end;
  end;

  function GetCellColor(I, J: Integer): TColor;
  begin
    if IsFixedCell(I, J) then
      Result := AReportLink.FixedColor
    else
      case AReportLink.DrawMode of
        gdmStrict:
          Result := AReportLink.Color;
        gdmOddEven:
          if Odd(J) then
            Result := AReportLink.OddColor
          else
            Result := AReportLink.EvenColor;
      else
        if not Odd((I - FixedColumnCount) + (J - FixedRowCount)) then
          Result := AReportLink.OddColor
        else
          Result := AReportLink.EvenColor;
      end;
    Result := ColorToRGB(Result);
  end;

const
  RightBottomInnerBorderColorMap: array[Boolean] of TColor = (clBtnShadow, clBtnFace);
var
  ABorderBrush: HBRUSH;
  ABrush: HBRUSH;
  AColor: TColor;
  AFixedCell: Boolean;
  AFont: TFont;
  AIsTransparentCell: Boolean;
  AOffsetX: Integer;
  AOffsetY: Integer;
  APrevFont: HFONT;
  APrevFontColor: COLORREF;
  APreviewFont: TFont;
  ASides: TdxCellSides;
  I, J, W, H: Integer;
  R2: TRect;
  S: string;
begin
  if AScaleFactor = nil then
    AScaleFactor := AReportLink.ScaleFactor;
  APrevFontColor := GetTextColor(ACanvas.Handle);
  APrevFont := GetCurrentObject(ACanvas.Handle, OBJ_FONT);
  APreviewFont := TFont.Create;
  try
    SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);

    R := cxRectInflate(R, -AScaleFactor.Apply(5));
    W := (R.Right - R.Left) div ColCount;
    H := (R.Bottom - R.Top) div RowCount;
    AOffsetX := R.Left + (R.Right - R.Left - ColCount * W) div 2;
    AOffsetY := R.Top + (R.Bottom - R.Top - RowCount * H) div 2;

    ABorderBrush := CreateSolidBrush(ColorToRGB(AReportLink.GridLineColor));

    for I := 0 to ColCount - 1 do
      for J := 0 to RowCount - 1 do
      begin
        R2 := Bounds(AOffsetX + I * W, AOffsetY + J * H, W, H);
        ASides := GetCellSides(I, J);
        AFont := GetCellFont(I, J);
        AColor := GetCellColor(I, J);
        AFixedCell := IsFixedCell(I, J);

        FrameRect(ACanvas.Handle, R2, ABorderBrush, ASides);
        if AFixedCell and AReportLink.Effects3D then
        begin
          if (I = 0) and not AReportLink.IsDrawBorder then
            Exclude(ASides, csTop);
          if (J = 0) and not AReportLink.IsDrawBorder then
            Exclude(ASides, csLeft);
          cxPaintCanvas.BeginPaint(ACanvas.Handle);
          cxPaintCanvas.DrawComplexFrame(R2, clBtnHighlight, RightBottomInnerBorderColorMap[AReportLink.Soft3D], TcxBorders(ASides));
          cxPaintCanvas.EndPaint;
          R2 := cxRectInflate(R2, -AScaleFactor.Apply(1));
        end;

        AIsTransparentCell := AFixedCell and AReportLink.FixedTransparent or not AFixedCell and AReportLink.Transparent;

        if not AIsTransparentCell then
        begin
          ABrush := CreateSolidBrush(AColor);
          FillRect(ACanvas.Handle, R2, ABrush);
          DeleteObject(ABrush);
        end;

        APreviewFont.Assign(AFont);
        APreviewFont.PixelsPerInch := dxDefaultDPI;
        APreviewFont.Height := AScaleFactor.Apply(APreviewFont.Height, AReportLink.ScaleFactor);
        SetTextColor(ACanvas.Handle, ColorToRGB(APreviewFont.Color));
        SelectObject(ACanvas.Handle, APreviewFont.Handle);
        S := GetDesignerString(I + Ord(not AShowFixedColumn), J + Ord(not AShowFixedRow));
        ExtTextOut(ACanvas.Handle, R2.Left + AScaleFactor.Apply(2), R2.Top + AScaleFactor.Apply(2), ETO_CLIPPED, @R2, PChar(S), Length(S), nil);
      end;

    SetBkMode(ACanvas.Handle, Windows.OPAQUE);
    DeleteObject(ABorderBrush);
  finally
    APreviewFont.Free;
  end;
  SelectObject(ACanvas.Handle, APrevFont);
  SetTextColor(ACanvas.Handle, APrevFontColor);
end;

{ TdxPSGridCellDataMaps }

function dxPSGridCellDataMaps: TdxPSGridCellDataMaps;
begin
  Result := TdxPSGridCellDataMaps.Instance;
end;

class function TdxPSGridCellDataMaps.Instance: TdxPSGridCellDataMaps;
begin
  Result := inherited Instance as TdxPSGridCellDataMaps;
end;

procedure TdxPSGridCellDataMaps.InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
  AReportLink: TAbstractdxGridReportLink);
begin
  MapClasses[ADataItem].InitializeCellData(ACol, ARow, ADataItem, AReportLink);
end;

function TdxPSGridCellDataMaps.GetMapClass(ADataItem: TAbstractdxReportCellData): TdxPSCustomGridCellDataMapClass;
begin
  Result := TdxPSCustomGridCellDataMapClass(PairClasses[ADataItem.ClassType]);
end;

{ TdxPSCustomGridCellDataMap }

class function TdxPSCustomGridCellDataMap.PairClass: TClass;
begin
  Result := DataClass;
end;

class procedure TdxPSCustomGridCellDataMap.Register;
begin
  dxPSGridCellDataMaps.Register(Self);
end;

class procedure TdxPSCustomGridCellDataMap.Unregister;
begin
  dxPSGridCellDataMaps.Unregister(Self);
end;

class function TdxPSCustomGridCellDataMap.DataClass: TdxReportCellDataClass;
begin
  Result := TAbstractdxReportCellData;
end;

class procedure TdxPSCustomGridCellDataMap.InitializeCellData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink);
begin
  ADataItem.Color := AReportLink.GetCellColor(ACol, ARow);
  ADataItem.ContentBkColor := AReportLink.GetCellContentBkColor(ACol, ARow);
  ADataItem.ContentPattern := AReportLink.GetCellContentPattern(ACol, ARow);
  ADataItem.Transparent := AReportLink.GetCellTransparent(ACol, ARow);

  ADataItem.CellSides := AReportLink.GetCellSides(ACol, ARow);
  ADataItem.Data := ACol + ARow * AReportLink.ColCount;
  ADataItem.EdgeMode := AReportLink.GetCellEdgeMode(ADataItem, ACol, ARow);
  ADataItem.Edge3DStyle := AReportLink.GetCellEdge3DStyle(ADataItem, ACol, ARow);
  ADataItem.Edge3DSoft := AReportLink.GetCellEdge3DSoft(ADataItem, ACol, ARow);
  ADataItem.FontIndex := AReportLink.GetCellFontIndex(ACol, ARow);
end;

{ TdxPSTextGridCellDataMap }

class function TdxPSTextGridCellDataMap.DataClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellString;
end;

class procedure TdxPSTextGridCellDataMap.InitializeCellData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink);
begin
  inherited;
  with TdxReportCellString(ADataItem) do
  begin
    EndEllipsis := AReportLink.GetEndEllipsis;
    Multiline := AReportLink.GetCellMultiline(ACol, ARow);
    SortOrder := AReportLink.GetColSortOrder(ACol);
    Text := AReportLink.GetCellText(ACol, ARow);
    TextAlignX := AReportLink.GetCellTextAlignX(ACol, ARow);
    TextAlignY := AReportLink.GetCellTextAlignY(ACol, ARow);
  end;
end;

{ TdxPSImageGridCellDataMap }

class function TdxPSImageGridCellDataMap.DataClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellImage;
end;

class procedure TdxPSImageGridCellDataMap.InitializeCellData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink);
begin
  inherited;
  with TdxReportCellImage(ADataItem) do
  begin
    if AReportLink.GetCellHasImage(ACol, ARow) then
    begin
      Image := AReportLink.GetCellImage(ACol, ARow);
      ImageTransparent := AReportLink.GetCellImageTransparent(ACol, ARow);
      MakeSpaceForEmptyImage := AReportLink.GetCellImageMakeSpaceForEmpty(ACol, ARow);
    end;
    ImageIndex := AReportLink.GetCellImageIndex(ACol, ARow);
    ImageLayout := AReportLink.GetCellImageLayout(ACol, ARow);
    ImageList := AReportLink.GetCellImageList(ACol, ARow);
  end;
end;

{ TAbstractdxGridReportLink }

constructor TAbstractdxGridReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelimitersHardVert := TList.Create;
  FDelimitersHardHorz := TList.Create;
  FFixedFont := TFont.Create;
  FEvenFont := TFont.Create;
  InternalRestoreDefaults;
  FFixedFont.OnChange := FontChanged;
  FEvenFont.OnChange := FontChanged;
  LinkModified(False);
  FCurrentCol := -1;
  FCurrentRow := -1;
  FReportRows := TList.Create;
end;

destructor TAbstractdxGridReportLink.Destroy;
begin
  FreeAndNil(FDefaultFixedFont);
  FreeAndNil(FReportRows);
  FreeAndNil(FEvenFont);
  FreeAndNil(FFixedFont);
  FreeAndNil(FDelimitersHardVert);
  FreeAndNil(FDelimitersHardHorz);
  inherited Destroy;
end;

procedure TAbstractdxGridReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAbstractdxGridReportLink then
    with TAbstractdxGridReportLink(Source) do
    begin
      Self.DrawMode := DrawMode;
      Self.Effects3D := Effects3D;
      Self.EndEllipsis := EndEllipsis;
      Self.EvenColor := EvenColor;
      Self.EvenFont := EvenFont;
      Self.FixedColor := FixedColor;
      Self.FixedFont := FixedFont;
      Self.FixedTransparent := FixedTransparent;
      Self.GridLineColor := GridLineColor;
      Self.IncludeFixed := IncludeFixed;
      Self.OnlySelected := OnlySelected;
      Self.Multiline := Multiline;
      Self.RowAutoHeight := RowAutoHeight;
      Self.Soft3D := Soft3D;
      Self.SupportedCustomDraw := SupportedCustomDraw;
      Self.UseCustomPageBreaks := UseCustomPageBreaks;
    end;
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(AColumn: Integer);
var
  ARow: TdxReportCell;
begin
  if (ReportRowCount > 0) and (AColumn > 0) then
  begin
    ARow := ReportRows[0];
    if AColumn < ARow.DataItemCount then
      AddHorizontalHardDelimiter(ARow.DataItems[AColumn].AbsoluteOrigin.X);
  end;
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  AColumns: PIntegerArray; ACount: Integer);
begin
  AddHorizontalPageBreak(AColumns, 0, ACount);
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  AColumns: PIntegerArray; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddHorizontalPageBreak(AColumns^[I]);
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(AColumns: TList);
begin
  AddHorizontalPageBreak(AColumns, 0, AColumns.Count);
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  AColumns: TList; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddHorizontalPageBreak(Integer(AColumns.Items[I]));
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  const AColumns: array of Integer);
begin
  AddHorizontalPageBreak(AColumns, 0, Length(AColumns));
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  const AColumns: array of Integer; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddHorizontalPageBreak(AColumns[I]);
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(const AColumns: TBoundArray);
var
  I: Integer;
begin
  for I := 0 to Length(AColumns) - 1 do
    AddHorizontalPageBreak(AColumns[I]);
end;

procedure TAbstractdxGridReportLink.AddHorizontalPageBreak(
  const AColumns: TBoundArray; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddHorizontalPageBreak(AColumns[I]);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(ARow: Integer);
var
  ReportRow: TdxReportCell;
begin
  ReportRow := ReportRowsByRow[ARow];
  if (ReportRow <> nil) and not ReportRow.IsFirstItem then
    AddVerticalHardDelimiter(ReportRow);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(const ARows: array of Integer);
begin
  AddPageBreak(ARows, 0, High(ARows) + 1);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(const ARows: array of Integer;
  AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddPageBreak(ARows[I]);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(ARows: PIntegerArray; ACount: Integer);
begin
  AddPageBreak(ARows, 0, ACount);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(ARows: PIntegerArray; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddPageBreak(ARows^[I]);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(const ARows: TBoundArray);
var
  I: Integer;
begin
  for I := 0 to Length(ARows) - 1 do
    AddPageBreak(ARows[I]);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(const ARows: TBoundArray; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddPageBreak(ARows[I]);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(ARows: TList);
begin
  AddPageBreak(ARows, 0, ARows.Count);
end;

procedure TAbstractdxGridReportLink.AddPageBreak(ARows: TList; AStartIndex, ACount: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AStartIndex + ACount - 1 do
    AddPageBreak(Integer(ARows[I]));
end;

function TAbstractdxGridReportLink.DefaultFixedFont: TFont;
begin
  if FDefaultFixedFont = nil then
  begin
    FDefaultFixedFont := TFont.Create;
    InitializeDefaultFixedFont(FDefaultFixedFont);
  end;
  Result := FDefaultFixedFont;
end;

procedure TAbstractdxGridReportLink.ChangeScale(M: Integer; D: Integer);
begin
  inherited;
  EvenFont.Height := MulDiv(EvenFont.Height, M, D);
  FixedFont.Height := MulDiv(FixedFont.Height, M, D);
  if FDefaultFixedFont <> nil then
    FDefaultFixedFont.Height := MulDiv(FDefaultFixedFont.Height, M, D);
end;

procedure TAbstractdxGridReportLink.ConstructReport(AReportCells: TdxReportCells);
begin
  if not DataProviderPresent then
    Exit;

  inherited ConstructReport(AReportCells);
  BeforeConstruct(AReportCells);
  try
    PrepareConstruct(AReportCells);
    try
      AReportCells.BorderColor := GridLineColor;
      AReportCells.Cells.FontIndex := 0;
      AReportCells.Cells.Color := Color;
      if FootersOnEveryPage then
      begin
        AReportCells.FooterCells.FontIndex := FFixedFontIndex;
        AReportCells.FooterCells.Color := FixedColor;
      end;
      if HeadersOnEveryPage then
      begin
        AReportCells.HeaderCells.FontIndex := FFixedFontIndex;
        AReportCells.HeaderCells.Color := FixedColor;
      end;
      ConstructReportContent(AReportCells);
    finally
      UnprepareConstruct(AReportCells);
    end;

    if not AbortBuilding then
    begin
      SetupBoundsRect(AReportCells.Cells);
      if FootersOnEveryPage then
        SetupBoundsRect(AReportCells.FooterCells);
      if HeadersOnEveryPage then
        SetupBoundsRect(AReportCells.HeaderCells);
    end;
  finally
    AfterConstruct(AReportCells);
  end;
end;

procedure TAbstractdxGridReportLink.ConstructReportContent(AReportCells: TdxReportCells);
var
  ACol: Integer;
  AEntireRowCount: Integer;
  AItem: TdxReportItem;
  AReportRow: TdxReportCell;
  ARow: Integer;
  R, R2: TRect;
begin
  FCurrentRow := 0;
  AEntireRowCount := ActualRowCount;
  R := Rect(0, 0, RowWidth, 0);
  for ARow := 0 to RowCount - 1 do
  begin
    if IsProcessedRow(ARow) then
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Top + GetRowHeight(ARow);

      AReportRow := GetRowClass(ARow).Create(GetRowParent(AReportCells, ARow));
      AddReportRow(AReportRow);
      InitializeRow(AReportRow, ARow);
      AReportRow.BoundsRect := R;

      AItem := AReportRow.getPrevSibling;
      if AItem <> nil then
        AReportRow.Top := TdxReportVisualItem(AItem).BoundsRect.Bottom
      else
        AReportRow.Top := 0;

      FCurrentCol := 0;
      R2 := Rect(0, 0, 0, R.Bottom - R.Top);
      for ACol := 0 to ColCount - 1 do
      begin
        if IsProcessedCol(ACol) then
        begin
          R2.Left := R2.Right;
          R2.Right := R2.Left + ColumnWidths[ACol];
          if IsProcessedCell(ACol, ARow) then
            CreateDataItem(AReportRow, ActualColIndexes[ACol], ARow, R2);
        end;
        NextCol;
      end;
      AReportCells.DoProgress(MulDiv(ARow, 100, AEntireRowCount));
      if AbortBuilding then
        Break;
    end;
    NextRow;
  end;
end;

procedure TAbstractdxGridReportLink.ConvertCoords;

  procedure ConvertDelimiters(ADelimiters: TList);
  var
    I, Value: Integer;
  begin
    for I := 0 to ADelimiters.Count - 1 do
    begin
      Value := Integer(ADelimiters[I]);
      Value := MulDiv(Value, PixelsNumerator, PixelsDenominator);
      ADelimiters[I] := TObject(Value);
    end;
  end;

begin
  inherited ConvertCoords;
  ConvertDelimiters(DelimitersHardVert);
  ConvertDelimiters(DelimitersHardHorz);
end;

function TAbstractdxGridReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := UseCustomPageBreaks;
end;

function TAbstractdxGridReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := AutoWidth and (AUpdateCodes * uaMarginsVert <> []);
end;

function TAbstractdxGridReportLink.GetUseHardHorzDelimiters: Boolean;
begin
  Result := UseCustomPageBreaks;
end;

function TAbstractdxGridReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := UseCustomPageBreaks;
end;

procedure TAbstractdxGridReportLink.InternalRestoreDefaults;
begin
  inherited;
  AutoWidth := False;
  DrawMode := gdmStrict;
  Effects3D := False;
  EndEllipsis := False;
  EvenColor := dxDefaultColor;
  FixedColor := dxDefaultFixedColor;
  EvenFont := Font;
  FixedFont := Font;
  FixedFont.Style := [fsBold];
  FixedTransparent := False;
  GridLineColor := dxDefaultGridLineColor;
  IncludeFixed := True;
  Multiline := False;
  OnlySelected := False;
  RowAutoHeight := False;
  Soft3D := True;
  SupportedCustomDraw := False;
end;

function TAbstractdxGridReportLink.IsSupportedCustomDraw(AItem: TAbstractdxReportCellData): Boolean;
begin
  Result := SupportedCustomDraw;
end;

procedure TAbstractdxGridReportLink.MakeDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
var
  I: Integer;
  ReportRow: TdxReportCell;
begin
  inherited MakeDelimiters(AReportCells, AHorzDelimiters, AVertDelimiters);
  if ReportRowCount <> 0 then
    with AReportCells do
    begin
      if UseHorzDelimiters then
      begin
        ReportRow := ReportRows[0];
        for I := 1 to ReportRow.DataItemCount - 1 do
          AHorzDelimiters.Add(TObject(ReportRow.DataItems[I].AbsoluteOrigin.X));
      end;

      if UseVertDelimiters then
      begin
        for I := 1 to ReportRowCount - 1 do
          AVertDelimiters.Add(TObject(ReportRows[I].AbsoluteOrigin.Y));
      end;
    end;
end;

procedure TAbstractdxGridReportLink.MakeHardDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited;
  if UseCustomPageBreaks then
  try
    DoGetCustomPageBreaks;
  except
    Application.HandleException(Self);
  end;
  dxCopyList(DelimitersHardHorz, AHorzDelimiters);
  dxCopyList(DelimitersHardVert, AVertDelimiters);
end;

procedure TAbstractdxGridReportLink.DoGetCustomPageBreaks;
begin
  if Assigned(FOnGetCustomPageBreaks) then FOnGetCustomPageBreaks(Self);
end;

procedure TAbstractdxGridReportLink.DoInitializeItem(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeItem) then FOnInitializeItem(Self, ACol, ARow, ADataItem);
end;

procedure TAbstractdxGridReportLink.AssignData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData);
begin
  dxPSGridCellDataMaps[ADataItem].InitializeCellData(ACol, ARow, ADataItem, Self);
end;

procedure TAbstractdxGridReportLink.CalculateColumnAutoWidths;
var
  AutoWidthObject: TcxAutoWidthObject;
  I, Index: Integer;
 begin
  AutoWidthObject := TcxAutoWidthObject.Create(ActualColCount);
  try
    for I := 0 to ColCount - 1 do
      if IsProcessedCol(I) then
      begin
        with AutoWidthObject.AddItem do
        begin
          Width := ColumnWidths[I];
          MinWidth := 1;
          Fixed := False;
        end;
      end;
    AutoWidthObject.AvailableWidth := AvailableSiteWidth;
    AutoWidthObject.Calculate;

    Index := 0;
    for I := 0 to ColCount - 1 do
      if IsProcessedCol(I) then
      begin
        ColumnWidths[I]:= AutoWidthObject[Index].AutoWidth;
        Inc(Index);
      end;
  finally
    AutoWidthObject.Free;
  end;
end;

procedure TAbstractdxGridReportLink.CalculateColumnWidths;
var
  I: Integer;
begin
  SetLength(FColumnWidths, ColCount);
  for I := 0 to ColCount - 1 do
    ColumnWidths[I] := GetSourceColWidth(ActualColIndexes[I]);
  if AutoWidth and CanCalculateColumnAutoWidths then
    CalculateColumnAutoWidths;
end;

procedure TAbstractdxGridReportLink.CalculateRowHeights(AReportCells: TdxReportCells);

  function CalculateCellAutoHeight(ACol, ARow: Integer): Integer;
  var
    AActualColIndex: Integer;
    R: TRect;
  begin
    AActualColIndex := ActualColIndexes[ACol];
    R := Rect(0, 0, ColumnWidths[ACol], 5);
    Result := Renderer.CalcTextRect(ScreenCanvas, GetCellText(AActualColIndex, ARow), R,
      GetCellMultiline(AActualColIndex, ARow), GetCellFont(AActualColIndex, ARow));
  end;

  function CalculateRowAutoHeight(ARow: Integer): Integer;
  var
    AColumn: Integer;
  begin
    Result := 0;
    for AColumn := 0 to ColCount - 1 do
    begin
      if IsProcessedCol(AColumn) then
        Result := Max(Result, CalculateCellAutoHeight(AColumn, ARow));
    end;
  end;

var
  AMinFixedRowHeight, AMinRowHeight: Integer;
  ARow: Integer;
  ARowHeight: Integer;
begin
  ScreenCanvas.SaveState;
  try
    AMinRowHeight := GetMinRowHeight(ScreenCanvas, Font);
    AMinFixedRowHeight := GetMinRowHeight(ScreenCanvas, FixedFont);

    SetLength(FRowHeights, RowCount);
    for ARow := 0 to RowCount - 1 do
    begin
      ARowHeight := 0;
      if IsProcessedRow(ARow) then
      begin
        ARowHeight := GetSourceRowHeight(ARow);
        if CanCalculateRowAutoHeight(ARow) and (ARowHeight <> 0) then
        begin
          ARowHeight := CalculateRowAutoHeight(ARow);
          if (ARowHeight = 0) and not IsEmptyRow(ARow) then
            ARowHeight := GetSourceRowHeight(ARow)
        end;
        if ARowHeight <> 0 then
        begin
          if IsFixedRow(ARow) or (FixedColCount > 0) then
            ARowHeight := Max(ARowHeight, AMinFixedRowHeight)
          else
            ARowHeight := Max(ARowHeight, AMinRowHeight);
        end;
      end;
      RowHeights[ARow] := ARowHeight;
    end;
  finally
    ScreenCanvas.RestoreState;
  end;
end;

function TAbstractdxGridReportLink.CalculateRowWidth: Integer;
var
  Col: Integer;
begin
  Result := 0;
  for Col := 0 to ColCount - 1 do
    if IsProcessedCol(Col) then
      Inc(Result, ColumnWidths[Col]);
end;

function TAbstractdxGridReportLink.CanCalculateColumnAutoWidths: Boolean;
begin
  Result := not IsAggregated or SysUtils.Supports(Controller, IdxReportLinkController);
end;

function TAbstractdxGridReportLink.CanCalculateRowAutoHeight(ARow: Integer): Boolean;
begin
  Result := RowAutoHeight;
end;

function TAbstractdxGridReportLink.CreateDataItem(AParent: TdxReportCell;
  ACol, ARow: Integer; const ABounds: TRect): TAbstractdxReportCellData;
var
  DataClass: TdxReportCellDataClass;
begin
  DataClass := GetDataItemClass(ACol, ARow);
  if DataClass <> nil then
  begin
    Result := DataClass.Create(AParent);
    Result.BoundsRect := ABounds;
    AssignData(ACol, ARow, Result);
    DoInitializeItem(ACol, ARow, Result);
  end
  else
    Result := nil;
end;

function TAbstractdxGridReportLink.GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass;
begin
  Result := TdxReportCellString;
end;

function TAbstractdxGridReportLink.GetRowClass(ARow: Integer): TdxReportCellClass;
begin
  Result := TdxReportCell;
end;

function TAbstractdxGridReportLink.GetRowParent(AReportCells: TdxReportCells; ARow: Integer): TdxReportCell;
begin
  if HeadersOnEveryPage and IsHeaderRow(ARow) then
    Result := AReportCells.HeaderCells
  else
    if FootersOnEveryPage and IsFooterRow(ARow) then
      Result := AReportCells.FooterCells
    else
      Result := AReportCells.Cells;
end;

procedure TAbstractdxGridReportLink.InitializeRow(AReportRow: TdxReportCell;
  ARow: Integer);
begin
  AReportRow.Data := ARow;
  AReportRow.Transparent := True;
  AReportRow.CellSides := [];
end;

function TAbstractdxGridReportLink.GetColCount: Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.GetFixedColCount: Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.GetFixedRowCount: Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.GetRowCount: Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.GetActualColCount: Integer;
begin
  if HasSelection then
    Result := SelectedColCount
  else
    Result := ColCount;
end;

function TAbstractdxGridReportLink.GetActualColIndex(Col: Integer): Integer;
begin
  Result := Col;
end;

function TAbstractdxGridReportLink.GetActualRowCount: Integer;
begin
  if HasSelection then
    Result := SelectedRowCount
  else
    Result := RowCount;
end;

function TAbstractdxGridReportLink.GetCellEdge3DSoft(AItem: TAbstractdxReportCellData;
  ACol, ARow: Integer): Boolean;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellEdge3DSoft(ACol, ARow)
  else
    Result := Soft3D;
end;

function TAbstractdxGridReportLink.GetCellEdge3DStyle(AItem: TAbstractdxReportCellData;
  ACol, ARow: Integer): TdxCellEdgeStyle;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellEdge3DStyle(ACol, ARow)
  else
    Result := cesRaised;
end;

function TAbstractdxGridReportLink.GetCellEdgeMode(AItem: TAbstractdxReportCellData;
  ACol, ARow: Integer): TdxCellEdgeMode;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellEdgeMode(ACol, ARow)
  else
    if IsFixedCell(ACol, ARow) and Effects3D then
      Result := cem3DEffects
    else
      Result := cemPattern;
end;

function TAbstractdxGridReportLink.GetEndEllipsis: Boolean;
begin
  Result := FEndEllipsis;
end;

function TAbstractdxGridReportLink.GetInternalRowHeight(Index: Integer): Integer;
begin
  Result := FRowHeights[Index];
end;

function TAbstractdxGridReportLink.GetMinRowHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont): Integer;
begin
  Result := Renderer.CalcTextPatternHeight(ACanvas, AFont);
end;

function TAbstractdxGridReportLink.GetMultiline: Boolean;
begin
  Result := FMultiline;
end;

procedure TAbstractdxGridReportLink.GetSelectedRange(ABeginCol, AEndCol,
  ABeginRow, AEndRow: PInteger);
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
  if ABeginCol <> nil then
    ABeginCol^ := BeginCol;

  if AEndCol <> nil then
    AEndCol^ := EndCol;

  if ABeginRow <> nil then
    ABeginRow^ := BeginRow;

  if AEndRow <> nil then
    AEndRow^ := EndRow;
end;

procedure TAbstractdxGridReportLink.GetSelectedRange(out ABeginCol, AEndCol, ABeginRow, AEndRow: Integer);
begin
  with SelectionRect do
  begin
    ABeginCol := Left;
    AEndCol := Right;
    ABeginRow := Top;
    AEndRow := Bottom;
  end;
end;

function TAbstractdxGridReportLink.GetSelectedColCount: Integer;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
  Result := EndCol - BeginCol + 1;
  if IncludeFixed then Inc(Result, FixedColCount);
end;

function TAbstractdxGridReportLink.GetSelectedRowCount: Integer;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
  Result := EndRow - BeginRow + 1;
  if IncludeFixed then Inc(Result, FixedRowCount);
end;

function TAbstractdxGridReportLink.GetSelectionRect: TRect;
begin
  Result := cxNullRect;
end;

function TAbstractdxGridReportLink.GetSourceCellContentBkColor(ACol, ARow: Integer): TColor;
begin
  Result := GetSourceCellColor(ACol, ARow);
end;

function TAbstractdxGridReportLink.GetSourceCellColor(ACol, ARow: Integer): TColor;
begin
  if IsFixedCell(ACol, ARow) then
    Result := FixedColor
  else
    Result := Color;
end;

function TAbstractdxGridReportLink.GetSourceCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass;
begin
  Result := TdxPSSolidFillPattern;
end;

function TAbstractdxGridReportLink.GetSourceCellEdge3DSoft(ACol, ARow: Integer): Boolean;
begin
  Result := False;
end;

function TAbstractdxGridReportLink.GetSourceCellEdge3DStyle(ACol, ARow: Integer): TdxCellEdgeStyle;
begin
  if IsFixedCell(ACol, ARow) then
    Result := cesRaised
  else
    Result := cesSunken;
end;

function TAbstractdxGridReportLink.GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode;
begin
  Result := cemPattern;
end;

function TAbstractdxGridReportLink.GetSourceCellFont(ACol, ARow: Integer): TFont;
begin
  if IsFixedCell(ACol, ARow) then
    Result := FixedFont
  else
    Result := Font;
end;

function TAbstractdxGridReportLink.GetSourceCellFontIndex(ACol, ARow: Integer): Integer;
begin
  if IsFixedCell(ACol, ARow) then
    Result := FFixedFontIndex
  else
    Result := FFontIndex;
end;

function TAbstractdxGridReportLink.GetSourceCellMultiline(ACol, ARow: Integer): Boolean;
begin
  Result := False;
end;

function TAbstractdxGridReportLink.GetSourceCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
begin
  Result := taLeft;
end;

function TAbstractdxGridReportLink.GetSourceCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  Result := taCenterY;
end;

function TAbstractdxGridReportLink.GetSourceCellTransparent(ACol, ARow: Integer): Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.GetSourceColWidth(ACol: Integer): Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.GetSourceRowHeight(ARow: Integer): Integer;
begin
  Result := 0;
end;

function TAbstractdxGridReportLink.HasColumnHeaderImage(ACol: Integer): Boolean;
begin
  Result := False;
end;

function TAbstractdxGridReportLink.HasSelection: Boolean;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  if OnlySelected then
  begin
    GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
    Result := (EndCol >= BeginCol) and (EndRow >= BeginRow);
  end
  else
    Result := False;
end;

function TAbstractdxGridReportLink.HasSelectionInCol(ACol: Integer): Boolean;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
  Result := ((ACol >= BeginCol) and (ACol <= EndCol)) or (IncludeFixed and IsFixedCol(ACol));
end;

function TAbstractdxGridReportLink.HasSelectionInRow(ARow: Integer): Boolean;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);
  Result := ((ARow >= BeginRow) and (ARow <= EndRow)) or (IncludeFixed and IsFixedRow(ARow));
end;

function TAbstractdxGridReportLink.IsDrawBorder: Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsDrawHorzLines: Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsDrawVertLines: Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsEmptyRow(ARow: Integer): Boolean;
var
  AColumn: Integer;
begin
  Result := True;
  for AColumn := 0 to ColCount - 1 do
    if IsProcessedCol(AColumn) then
    begin
      Result := GetCellText(AColumn, ARow) = '';
      if not Result then
        Break;
    end;
end;

function TAbstractdxGridReportLink.IsDrawFixedHorzLines: Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsDrawFixedVertLines: Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsFixedCell(ACol, ARow: Integer): Boolean;
begin
  Result := IsFixedCol(ACol) or IsFixedRow(ARow);
end;

function TAbstractdxGridReportLink.IsFixedCol(ACol: Integer): Boolean;
begin
  Result := ACol < FixedColCount;
end;

function TAbstractdxGridReportLink.IsFixedRow(ARow: Integer): Boolean;
begin
  Result := ARow < FixedRowCount;
end;

function TAbstractdxGridReportLink.IsFooterRow(ARow: Integer): Boolean;
begin
  Result := False;
end;

function TAbstractdxGridReportLink.IsHeaderRow(ARow: Integer): Boolean;
begin
  Result := IsFixedRow(ARow);
end;

function TAbstractdxGridReportLink.IsProcessedCell(ACol, ARow: Integer): Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.IsProcessedCol(ACol: Integer): Boolean;
begin
  Result := not HasSelection or HasSelectionInCol(ACol);
end;

function TAbstractdxGridReportLink.IsProcessedRow(ARow: Integer): Boolean;
begin
  Result := not HasSelection or HasSelectionInRow(ARow);
end;

function TAbstractdxGridReportLink.IsSelectedCell(ACol, ARow: Integer): Boolean;
var
  BeginCol, EndCol, BeginRow, EndRow: Integer;
begin
  GetSelectedRange(BeginCol, EndCol, BeginRow, EndRow);

  Result := ((ACol >= BeginCol) and (ACol <= EndCol) and (ARow >= BeginRow) and (ARow <= EndRow));
  if not Result and IncludeFixed then
    Result := ((ACol >= BeginCol) and (ACol <= EndCol) and IsFixedCol(ACol)) or
              ((ARow >= BeginRow) and (ARow <= EndRow) and IsFixedRow(ARow));
end;

function TAbstractdxGridReportLink.IsSelectedRow(ARow: Integer): Boolean;
begin
  Result := False;
end;

procedure TAbstractdxGridReportLink.NextCol;
begin
  Inc(FCurrentCol);
end;

procedure TAbstractdxGridReportLink.NextRow;
begin
  Inc(FCurrentRow);
end;

procedure TAbstractdxGridReportLink.SetDrawMode(Value: TdxGridDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetupBoundsRect(ACell: TdxReportCell);
begin
  if ACell.CellCount > 0 then
    ACell.BoundsRect := Rect(0, 0, ACell.LastCell.BoundsRect.Right, ACell.LastCell.BoundsRect.Bottom);
end;

procedure TAbstractdxGridReportLink.AddReportRow(ARow: TdxReportCell);
begin
  FReportRows.Add(ARow);
end;

procedure TAbstractdxGridReportLink.AddVerticalHardDelimiter(ADelimiter: TdxReportCell);
begin
  AddVerticalHardDelimiter(ADelimiter.AbsoluteRect.Top);
end;

procedure TAbstractdxGridReportLink.AddHorizontalHardDelimiter(ADelimiter: Integer);
begin
  FDelimitersHardHorz.Add(Pointer(ADelimiter));
end;

procedure TAbstractdxGridReportLink.AddVerticalHardDelimiter(ADelimiter: Integer);
begin
  FDelimitersHardVert.Add(Pointer(ADelimiter));
end;

procedure TAbstractdxGridReportLink.AfterConstruct(AReportCells: TdxReportCells);
begin
  FreeAndNil(FScreenCanvas);
end;

procedure TAbstractdxGridReportLink.BeforeConstruct(AReportCells: TdxReportCells);
begin
  FScreenCanvas := TdxPSReportRenderScreenCanvas.Create;
end;

procedure TAbstractdxGridReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  FRowWidth := -1;
  DelimitersHardHorz.Clear;
  DelimitersHardVert.Clear;
  SetLength(FColumnWidths, 0);
  CalculateColumnWidths;
  FReportRows.Clear;
  CalculateRowHeights(AReportCells);
  FFixedFontIndex := AddFontToPool(FixedFont);
  FEvenFontIndex := AddFontToPool(EvenFont);
end;

procedure TAbstractdxGridReportLink.UnprepareConstruct(AReportCells: TdxReportCells);
begin
end;

procedure TAbstractdxGridReportLink.InitializeDefaultFixedFont(AFont: TFont);
begin
  AFont.Assign(Font);
  AFont.Style := AFont.Style + [fsBold];
end;

function TAbstractdxGridReportLink.IsFixedFontStored: Boolean;
begin
  Result := not dxPSUtl.dxAreFontsEqual(FixedFont, DefaultFixedFont);
end;

procedure TAbstractdxGridReportLink.GetCellColRow(AItem: TdxReportVisualItem;
  var ACol, ARow: Integer);
begin
  ARow := AItem.Data div ColCount;
  ACol := AItem.Data - ARow * ColCount;
end;

function TAbstractdxGridReportLink.GetCellColor(ACol, ARow: Integer): TColor;
begin
  case DrawMode of
    gdmStrict:
      if IsFixedCell(ACol, ARow) then
        Result := FixedColor
      else
        Result := Color;

    gdmOddEven:
      if IsFixedCell(ACol, ARow) then
        Result := FixedColor
      else
        if Odd(ARow) then
          Result := Color
        else
          Result := EvenColor;

    gdmChess:
      if IsFixedCell(ACol, ARow) then
        Result := FixedColor
      else
        if not Odd((ACol - FixedColCount) + (ARow - FixedRowCount)) then
          Result := Color
        else
          Result := EvenColor;
    else { gdmBorrowSource }
      Result := GetSourceCellColor(ACol, ARow);
  end;
end;

function TAbstractdxGridReportLink.GetCellContentBkColor(ACol, ARow: Integer): TColor;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellContentBkColor(ACol, ARow)
  else
    Result := GetCellColor(ACol, ARow);
end;

function TAbstractdxGridReportLink.GetCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellContentPattern(ACol, ARow)
  else
    Result := TdxPSSolidFillPattern;
end;

function TAbstractdxGridReportLink.GetCellTransparent(ACol, ARow: Integer): Boolean;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellTransparent(ACol, ARow)
  else
    if IsFixedCell(ACol, ARow) then
      Result := FixedTransparent
    else
      Result := Self.Transparent;
end;

function TAbstractdxGridReportLink.GetCellFont(ACol, ARow: Integer): TFont;
begin
  case DrawMode of
    gdmStrict:
      if IsFixedCell(ACol, ARow) then
        Result := FixedFont
      else
        Result := Font;
    gdmOddEven:
      if IsFixedCell(ACol, ARow) then
        Result := FixedFont
      else
        if Odd(ARow) then
          Result := Font
        else
          Result := EvenFont;
    gdmChess:
      if IsFixedCell(ACol, ARow) then
        Result := FixedFont
      else
        if not Odd((ACol - FixedColCount) + (ARow - FixedRowCount)) then
          Result := Font
        else
          Result := EvenFont;
    else {gdmBorrowSource}
      Result := GetSourceCellFont(ACol, ARow);
  end;
end;

function TAbstractdxGridReportLink.GetCellFontIndex(ACol, ARow: Integer): Integer;
begin
  case DrawMode of
    gdmStrict:
      if IsFixedCell(ACol, ARow) then
        Result := FFixedFontIndex
      else
        Result := FFontIndex;
    gdmOddEven:
      if IsFixedCell(ACol, ARow) then
        Result := FFixedFontIndex
      else
        if Odd(ARow) then
          Result := FFontIndex
        else
          Result := FEvenFontIndex;
    gdmChess:
      if IsFixedCell(ACol, ARow) then
        Result := FFixedFontIndex
      else
        if not Odd((ACol - FixedColCount) + (ARow - FixedRowCount)) then
          Result := FFontIndex
        else
          Result := FEvenFontIndex;
    else {gdmBorrowSource}
      Result := GetSourceCellFontIndex(ACol, ARow);
  end;
end;

function TAbstractdxGridReportLink.GetCellHasImage(ACol, ARow: Integer): Boolean;
begin
  Result := False;
end;

function TAbstractdxGridReportLink.GetCellImage(ACol, ARow: Integer): TGraphic;
begin
  Result := nil;
end;

function TAbstractdxGridReportLink.GetCellImageIndex(ACol, ARow: Integer): Integer;
begin
  Result := -1;
end;

function TAbstractdxGridReportLink.GetCellImageLayout(ACol, ARow: Integer): TdxImageLayout;
begin
  Result := ilImageCenterLeft;
end;

function TAbstractdxGridReportLink.GetCellImageList(ACol, ARow: Integer): TCustomImageList;
begin
  Result := nil;
end;

function TAbstractdxGridReportLink.GetCellImageMakeSpaceForEmpty(ACol, ARow: Integer): Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.GetCellImageTransparent(ACol, ARow: Integer): Boolean;
begin
  Result := True;
end;

function TAbstractdxGridReportLink.GetCellMultiline(ACol, ARow: Integer): Boolean;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellMultiline(ACol, ARow)
  else
    Result := Multiline or RowAutoHeight;
end;

function TAbstractdxGridReportLink.GetCellSides(ACol, ARow: Integer): TdxCellSides;
var
  ABeginCol, AEndCol, ABeginRow, AEndRow: Integer;
begin
  Result := csAll;
  if not IsDrawBorder then
  begin
    if ACol = ActualFirstCol then Exclude(Result, csLeft);
    if ACol = ActualLastCol then Exclude(Result, csRight);
    if ARow = ActualFirstRow then Exclude(Result, csTop);
    if ARow = ActualLastRow then Exclude(Result, csBottom);
  end;
  if IsFixedCell(ACol, ARow) then
  begin
    if not IsDrawFixedHorzLines then
    begin
      if ARow > 0 then Exclude(Result, csTop);
      if IsFixedCol(ACol) then
      begin
        if ARow < ActualLastRow then
          Exclude(Result, csBottom)
      end
      else
        if ARow < FixedRowCount - Byte(IsDrawHorzLines) then
          Exclude(Result, csBottom);
    end;
    if not IsDrawFixedVertLines then
    begin
      if ACol > 0 then Exclude(Result, csLeft);
      if IsFixedRow(ARow) then
      begin
        if ACol < ActualLastCol then
          Exclude(Result, csRight)
      end
      else
        if ACol < FixedColCount - Byte(IsDrawVertLines) then
          Exclude(Result, csRight);
    end;
  end
  else
  begin
    GetSelectedRange(ABeginCol, AEndCol, ABeginRow, AEndRow);
    if not IsDrawHorzLines then
    begin
      if not HasSelection then
      begin
        ABeginRow := FixedRowCount - 1;
        if ABeginRow = -1 then ABeginRow := 0;
        AEndRow := RowCount - 1;
      end;

      if ARow < AEndRow then
        if (ARow > ABeginRow) or (HasSelection and IncludeFixed and (FixedRowCount > 0)) then
          Result := Result - [csTop, csBottom]
        else
          Exclude(Result, csBottom)
      else
        if (ARow > ABeginRow) or (HasSelection and IncludeFixed and (FixedRowCount > 0)) then
          Exclude(Result, csTop);
    end;
    if not IsDrawVertLines then
    begin
      if not HasSelection then
      begin
        ABeginCol := FixedColCount - 1;
        if ABeginCol = -1 then ABeginCol := 0;
        AEndCol := ColCount - 1;
      end;

      if ACol < AEndCol then
        if (ACol > ABeginCol) or (HasSelection and IncludeFixed and (FixedColCount > 0)) then
          Result := Result - [csLeft, csRight]
        else
          Exclude(Result, csRight)
      else
        if (ACol > ABeginCol) or (HasSelection and IncludeFixed and (FixedColCount > 0)) then
          Exclude(Result, csLeft)
    end;
  end;
end;

function TAbstractdxGridReportLink.GetCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellTextAlignX(ACol, ARow)
  else
    if IsFixedCell(ACol, ARow) then
      Result := taCenterX
    else
      Result := taLeft;
end;

function TAbstractdxGridReportLink.GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  if DrawMode = gdmBorrowSource then
    Result := GetSourceCellTextAlignY(ACol, ARow)
  else
    if IsFixedCell(ACol, ARow) then
      Result := taBottom //taCenterY
    else
      Result := taCenterY;
end;

function TAbstractdxGridReportLink.GetColSortOrder(ACol: Integer): TdxCellSortOrder;
begin
  Result := csoNone;
end;

function TAbstractdxGridReportLink.GetActualFirstCol: Integer;
var
  EndCol, BeginRow, EndRow: Integer;
begin
  if not HasSelection or IncludeFixed then
    Result := 0
  else
    GetSelectedRange(Result, EndCol, BeginRow, EndRow);
end;

function TAbstractdxGridReportLink.GetActualFirstRow: Integer;
var
  BeginCol, EndCol, EndRow: Integer;
begin
  if not HasSelection or IncludeFixed then
    Result := 0
  else
    GetSelectedRange(BeginCol, EndCol, Result, EndRow);
end;

function TAbstractdxGridReportLink.GetActualLastCol: Integer;
var
  BeginCol, BeginRow, EndRow: Integer;
begin
  if not HasSelection then
    Result := ColCount - 1
  else
    GetSelectedRange(BeginCol, Result, BeginRow, EndRow);
end;

function TAbstractdxGridReportLink.GetActualLastRow: Integer;
var
  BeginCol, EndCol, BeginRow: Integer;
begin
  if not HasSelection then
    Result := RowCount - 1
  else
    GetSelectedRange(BeginCol, EndCol, BeginRow, Result);
end;

function TAbstractdxGridReportLink.GetAvailableSiteHeight: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if IsAggregated and SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
    with ControllerIntf.GetControlSiteBounds(TControl(Component)) do
      Result := Bottom - Top - 1
  else
    with RealPrinterPage.PaintRectPixels do
      Result := Bottom - Top - 1;
end;

function TAbstractdxGridReportLink.GetAvailableSiteWidth: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if IsAggregated and SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
    with ControllerIntf.GetControlSiteBounds(TControl(Component)) do
      Result := Right - Left - 1
  else
    with RealPrinterPage.PaintRectPixels do
      Result := Right - Left - 1;
end;

function TAbstractdxGridReportLink.GetColumnWidth(Index: Integer): Integer;
begin
  Result := FColumnWidths[Index];
end;

function TAbstractdxGridReportLink.GetOddColor: TColor;
begin
  Result := Color;
end;

function TAbstractdxGridReportLink.GetOddFont: TFont;
begin
  Result := inherited Font;
end;

function TAbstractdxGridReportLink.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FReportRows[Index]);
end;

function TAbstractdxGridReportLink.GetReportRowByRow(Row: Integer): TdxReportCell;
var
  I: Integer;
begin
  for I := 0 to ReportRowCount - 1 do
  begin
    Result := ReportRows[I];
    if Result.Data = Row then Exit;
  end;
  Result := nil;
end;

function TAbstractdxGridReportLink.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TAbstractdxGridReportLink.GetRowHeight(ARow: Integer): Integer;
begin
  Result := FRowHeights[ARow];
end;

function TAbstractdxGridReportLink.GetRowWidth: Integer;
begin
  if FRowWidth = -1 then
    FRowWidth := CalculateRowWidth;
  Result := FRowWidth;
end;

procedure TAbstractdxGridReportLink.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetColumnWidth(Index: Integer; Value: Integer);
begin
  FColumnWidths[Index] := Value;
end;

procedure TAbstractdxGridReportLink.SetEffects3D(Value: Boolean);
begin
  if FEffects3D <> Value then
  begin
    FEffects3D := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetEndEllipsis(Value: Boolean);
begin
  if FEndEllipsis <> Value then
  begin
    FEndEllipsis := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetEvenColor(Value: TColor);
begin
  if FEvenColor <> Value then
  begin
    FEvenColor := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetEvenFont(Value: TFont);
begin
  FEvenFont.Assign(Value)
end;

procedure TAbstractdxGridReportLink.SetFixedColor(Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetFixedFont(Value: TFont);
begin
  FFixedFont.Assign(Value)
end;

procedure TAbstractdxGridReportLink.SetFixedTransparent(Value: Boolean);
begin
  if FFixedTransparent <> Value then
  begin
    FFixedTransparent := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetGridLineColor(Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetIncludeFixed(Value: Boolean);
begin
  if FIncludeFixed <> Value then
  begin
    FIncludeFixed := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetMultiline(Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetOddColor(Value: TColor);
begin
  inherited Color := Value;
end;

procedure TAbstractdxGridReportLink.SetOddFont(Value: TFont);
begin
  inherited Font := Value;
end;

procedure TAbstractdxGridReportLink.SetOnlySelected(Value: Boolean);
begin
  if FOnlySelected <> Value then
  begin
    FOnlySelected := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetRowAutoHeight(Value: Boolean);
begin
  if FRowAutoHeight <> Value then
  begin
    FRowAutoHeight := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetRowHeight(Index: Integer; Value: Integer);
begin
  FRowHeights[Index] := Value;
end;

procedure TAbstractdxGridReportLink.SetSoft3D(Value: Boolean);
begin
  if Soft3D <> Value then
  begin
    FSoft3D := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetSupportedCustomDraw(Value: Boolean);
begin
  if FSupportedCustomDraw <> Value then
  begin
    FSupportedCustomDraw := Value;
    LinkModified(True);
  end;
end;

procedure TAbstractdxGridReportLink.SetUseCustomPageBreaks(Value: Boolean);
begin
  if FUseCustomPageBreaks <> Value then
  begin
    FUseCustomPageBreaks := Value;
    LinkModified(True);
  end;
end;

procedure RegisterAssistants;
begin
  TdxPSCustomGridCellDataMap.Register;
  TdxPSTextGridCellDataMap.Register;
  TdxPSImageGridCellDataMap.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSImageGridCellDataMap.Unregister;
  TdxPSTextGridCellDataMap.Unregister;
  TdxPSCustomGridCellDataMap.Unregister;
  TdxPSGridCellDataMaps.ReleaseInstance;
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;

end.
