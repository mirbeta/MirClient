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

unit dxPSdxSpreadSheetLnkCore;

interface

{$I cxVer.inc}

uses
  Types,
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs, Generics.Collections, Generics.Defaults,
  dxSpreadSheetCore, dxHashUtils, cxEdit, dxSpreadSheet, dxSpreadSheetTypes, dxSpreadSheetGraphics, dxPSCore, dxPSForm,
  dxPSBaseGridLnk, dxPSExcelEdgePatterns, dxPSExcelFillPatterns,  dxPSEdgePatterns, dxPSFillPatterns, cxDrawTextUtils,
  cxPC, dxPSGlbl, cxControls, cxContainer, cxLabel, cxCheckBox, Menus, cxLookAndFeelPainters, cxTextEdit, cxButtons,
  cxGraphics, cxDropDownEdit, dxCore, cxMaskEdit, cxColorComboBox, dxPSReportRenderCanvas, cxLookAndFeels, dxPScxCommon,
  dxSpreadSheetUtils, cxClasses, dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters, dxSpreadSheetStyles, dxGDIPlusClasses,
  dxSpreadSheetConditionalFormattingRules;

type
  PdxPSCellBorder = ^TdxPSCellBorder;
  TdxPSCellBorder = record
    Color: TColor;
    Pattern: TdxPSEdgePatternClass;

    constructor Create(APattern: TdxPSEdgePatternClass; AColor: TColor = clBlack);
  end;

  PdxPSCellBorders = ^TdxPSCellBorders;
  TdxPSCellBorders = array[TdxCellSide] of TdxPSCellBorder;

  TdxPSCellBorderCorner = (cbcTopLeft, cbcTopRight, cbcBottomRight, cbcBottomLeft);
  TdxPSCellBorderCorners = set of TdxPSCellBorderCorner;

  TdxPSCellBorderEnd = (cbsTopLeft, cbsBottomRight);
  TdxPSCellBorderEnds = set of TdxPSCellBorderEnd;

  TdxPSCellBorderSub = 0..3;

  TdxPSCellCorner = (ccTopLeft, ccTopRight, ccBottomRight, ccBottomLeft);

  TdxSpreadSheetAbstractReportLink = class;
  TdxPSCellPatternsBorderPainter = class;
  TdxPSGridCellsAdapter = class;
  TdxReportCellSSString = class;

  { IdxSpreadSheetAdvancedViewParams }

  IdxSpreadSheetAdvancedViewParams = interface
  ['{2F80F5AF-55D1-4DD3-9B3F-47E74598BABC}']
    function GetDataBar: TdxSpreadSheetCellDataBar;
    function GetFillStyle(out AColor1, AColor2: TColor): TdxSpreadSheetCellFillStyle;
    function GetIconSetIndex: Integer;
    function GetShowCellValue: Boolean;
  end;

  { TdxPSCellPatternsBorderPainter }

  TdxPSCellPatternsBorderPainter = class(TdxPSCellBorderPainter)
  protected
    FGridAdapter: TdxPSGridCellsAdapter;

    procedure ClipItemBounds(ACanvas: TdxPSReportRenderCustomCanvas; AOuterRect: TRect);
    procedure DrawBorder(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; ASide: TdxCellSide);
  public
    function Item: TdxReportCellSSString; reintroduce; overload;
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); override;
    //
    property GridAdapter: TdxPSGridCellsAdapter read FGridAdapter;
  end;

  { TdxPSGridCellsAdapter }

  TdxPSGridCellsAdapter = class
  strict private
    function GetCell(Col, Row: Integer): TdxReportCellSSString;
    function GetColCount: Integer;
    function GetColOffset(Index: Integer): Integer;
    function GetColWidth(Index: Integer): Integer;
    function GetRow(Index: Integer): TdxReportCell;
    function GetRowCount: Integer;
    function GetRowHeight(Index: Integer): Integer;
    function GetRowIndex(Index: Integer): Integer;
    function GetRowOffset(Index: Integer): Integer;
  protected
    FReportCells: TdxReportCells;
  public
    constructor Create(AReportCells: TdxReportCells);
    function GetNeighborCell(AItem: TdxReportCellSSString; ASide: TdxCellSide): TdxReportCellSSString;

    property Cells[Col, Row: Integer]: TdxReportCellSSString read GetCell; default;
    property ColCount: Integer read GetColCount;
    property ColOffsets[Index: Integer]: Integer read GetColOffset;
    property ColWidths[Index: Integer]: Integer read GetColWidth;
    property RowCount: Integer read GetRowCount;
    property RowHeights[Index: Integer]: Integer read GetRowHeight;
    property RowIndexes[Index: Integer]: Integer read GetRowIndex;
    property RowOffsets[Index: Integer]: Integer read GetRowOffset;
    property Rows[Index: Integer]: TdxReportCell read GetRow;
  end;

  { TdxPSSpreadSheetCellDisplayStyleCache }

  TdxPSSpreadSheetCellDisplayStyleCache = class(TdxSpreadSheetTableViewCellDisplayStyle)
  public
    Column: Integer;
    Row: Integer;

    constructor Create(AOwner: TdxSpreadSheetTableView); reintroduce;
  end;

  { TdxPSSSStringDataMap }

  TdxPSSSStringDataMap = class(TdxPSTextDataMap)
  protected
    class procedure InitializeItem(AItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: Variant; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties;
      const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean = False): TdxReportCellDataClass; override;
  end;

  { TdxPSSSStringGridCellDataMap }

  TdxPSSSStringGridCellDataMap = class(TdxPSTextGridCellDataMap)
  protected
    class procedure InitializeCellData(ACol, ARow: Integer;
      ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink); override;
    class function DataClass: TdxReportCellDataClass; override;
  end;

  { TdxSpreadSheetAdvancedViewParams }

  TdxSpreadSheetAdvancedViewParams = class(TInterfacedObject, IdxSpreadSheetAdvancedViewParams)
  protected
    FBackgroundColor1: TColor;
    FBackgroundColor2: TColor;
    FBackgroundFillStyle: TdxSpreadSheetCellFillStyle;
    FDataBar: TdxSpreadSheetCellDataBar;
    FIconSetIndex: Integer;
    FShowCellValue: Boolean;

    // IdxSpreadSheetAdvancedViewParams
    function GetDataBar: TdxSpreadSheetCellDataBar;
    function GetFillStyle(out AColor1, AColor2: TColor): TdxSpreadSheetCellFillStyle;
    function GetIconSetIndex: Integer;
    function GetShowCellValue: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateFrom(AStyle: TdxSpreadSheetCellDisplayStyle): IdxSpreadSheetAdvancedViewParams;
  end;

  { TdxReportCellConnectionLine }

  TdxReportCellConnectionLine = class(TdxReportCell)
  strict private const
    ArrowAngle = 45;
  strict private
    FArrowPoints: array [0..2] of TPoint;
    FArrowSize: Integer;
    FLinePoints: array[0..1] of TPoint;

    function GetLinePoint(Index: Integer): TPoint;
    procedure SetLinePoint(Index: Integer; const Value: TPoint);
  protected
    procedure AdjustBounds;
    procedure CalculateArrow;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawBackground(ACanvas: TdxPSReportRenderCustomCanvas); override;
    //
    property LinePoint1: TPoint index 0 read GetLinePoint write SetLinePoint;
    property LinePoint2: TPoint index 1 read GetLinePoint write SetLinePoint;
  end;

  { TdxReportCellBaseSSString }

  TdxReportCellBaseSSString = class(TdxReportCellString)
  strict private
    FContentBkColor: TColor;
    FContentPattern: TdxPSFillPatternClass;
    FDataBar: TdxSpreadSheetCellDataBar;
    FIconIndex: Integer;
    FIconSize: Integer;
    FTextAlignHorzIndent: Integer;
    FTextExtentLeft: Integer;
    FTextExtentRight: Integer;

    procedure SetDataBar(AValue: TdxSpreadSheetCellDataBar);
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    procedure DrawDataBar(ACanvas: TdxPSReportRenderCustomCanvas);
    procedure DrawIcon(ACanvas: TdxPSReportRenderCustomCanvas);

    function GetContentBkColor: TColor; override;
    function GetContentPattern: TdxPSFillPatternClass; override;
    procedure SetContentBkColor(Value: TColor); override;
    procedure SetContentPattern(Value: TdxPSFillPatternClass); override;

    procedure GetDataBarBounds(ACanvas: TdxPSReportRenderCustomCanvas; out AAxisBounds, ABarBounds: TRect);
    function GetIconBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
    function GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;

    procedure ReadData(AReader: TdxPSDataReader); override; final;
    procedure ReadDataCore(AReader: TdxPSDataReader); virtual;
    procedure WriteData(AWriter: TdxPSDataWriter); override; final;
    procedure WriteDataCore(AWriter: TdxPSDataWriter); virtual;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;

    property DataBar: TdxSpreadSheetCellDataBar read FDataBar write SetDataBar;
    property IconIndex: Integer read FIconIndex write FIconIndex;
    property IconSize: Integer read FIconSize write FIconSize;
    property TextAlignHorzIndent: Integer read FTextAlignHorzIndent write FTextAlignHorzIndent;
    property TextExtentLeft: Integer read FTextExtentLeft write FTextExtentLeft;
    property TextExtentRight: Integer read FTextExtentRight write FTextExtentRight;
  end;

  { TdxReportCellSSString }

  TdxReportCellSSString = class(TdxReportCellBaseSSString, IUnknown, IdxPSCellParams)
  strict private
    FBorders: TdxPSCellBorders;
    FBorderSlants: DWORD;
    FBorderSubs: DWORD;
    FRealCol: Integer;
    FRealRow: Integer;

    function GetBorder(ASide: TdxCellSide): TdxPSCellBorder;
    function GetBordersBkColor: TColor;
    function GetBorderSlant(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner): Integer;
    function GetBorderSlantOffset(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner): TDWORDBits;
    function GetBorderSub(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): TdxPSCellBorderSub;
    function GetBorderSubMask(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): DWORD;
    function GetBorderSubOffset(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): TDWORDBits;
    function GetClipContent: Boolean;
    function GetCol: Integer;
    function GetFill: Boolean;
    function GetIsFixed: Boolean;
    function GetIsMerged: Boolean;
    function GetIsNearMostLeft: Boolean;
    function GetIsNearMostTop: Boolean;
    function GetIsNearMostTopOrLeft: Boolean;
    function GetIsRTF: Boolean;
    function GetIsVirtual: Boolean;
    function GetRow: Integer;
    procedure SetBorder(ASide: TdxCellSide; Value: TdxPSCellBorder);
    procedure SetBorderSlant(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner; Value: Integer);
    procedure SetBorderSub(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd; Value: TdxPSCellBorderSub);
    procedure SetClipConent(Value: Boolean);
    procedure SetFill(Value: Boolean);
    procedure SetIsFixed(Value: Boolean);
    procedure SetIsMerged(Value: Boolean);
    procedure SetIsNearMostLeft(Value: Boolean);
    procedure SetIsNearMostTop(Value: Boolean);
    procedure SetIsRTF(Value: Boolean);
    procedure SetIsVirtual(Value: Boolean);

    // IdxPSCellParams
    function GetAutoHeight: Boolean;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetDisplayGraphicsAsText: Boolean;
    function GetDisplayTrackBarsAsText: Boolean;
    function GetEndEllipsis: Boolean;
    function GetFlatCheckMarks: Boolean;
    function GetGraphicsText: string;
    function GetMultiline: Boolean;
    function GetTransparentGraphics: Boolean;
  protected
    procedure CalcBorderSubs(AnAdapter: TdxPSGridCellsAdapter);
    procedure CalcDoubleBorderSlants(AnAdapter: TdxPSGridCellsAdapter);

    function GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;
    function GetBorderBrush(ASide: TdxCellSide): TBrush;
    function GetBorderEdgeBounds(ASide: TdxCellSide; const AOuterRect: TRect): TRect; override;
    function GetBorderEdgeClass(ASide: TdxCellSide): TdxPSCellBorderClass; override;
    function GetBorderPainterClass: TdxPSCellBorderPainterClass; override;
    function GetEffectiveBounds(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect; override;
    procedure InitBorderPainter(ABordersPainter: TdxPSCellBorderPainter); override;

    procedure ReadBorders(AReader: TdxPSDataReader);
    procedure ReadDataCore(AReader: TdxPSDataReader); override;
    procedure WriteBorders(AWriter: TdxPSDataWriter);
    procedure WriteDataCore(AWriter: TdxPSDataWriter); override;

    function GetAbsoluteInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
    function GetDoubleBorderRgn(ASide: TdxCellSide; const R: TRect): HRGN;
    function IsDoubleLineBorderPattern(ABorder: TdxPSCellBorder): Boolean; overload;
    function IsDoubleLineBorderPattern(ASide: TdxCellSide): Boolean; overload;
    function NullBorder: TdxPSCellBorder;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBorders(AColor: TColor; APattern: TdxPSEdgePatternClass);

    property Borders[ASide: TdxCellSide]: TdxPSCellBorder read GetBorder write SetBorder;
    property BordersBkColor: TColor read GetBordersBkColor;
    property BorderSlants[ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner]: Integer read GetBorderSlant write SetBorderSlant;
    property BorderSubs[ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd]: TdxPSCellBorderSub read GetBorderSub write SetBorderSub;
    property ClipContent: Boolean read GetClipContent write SetClipConent;
    property Col: Integer read GetCol;
    property Fill: Boolean read GetFill write SetFill;
    property IsFixed: Boolean read GetIsFixed write SetIsFixed;
    property IsMerged: Boolean read GetIsMerged write SetIsMerged;
    property IsNearMostLeft: Boolean read GetIsNearMostLeft write SetIsNearMostLeft;
    property IsNearMostTop: Boolean read GetIsNearMostTop write SetIsNearMostTop;
    property IsNearMostTopOrLeft: Boolean read GetIsNearMostTopOrLeft;
    property IsRTF: Boolean read GetIsRTF write SetIsRTF;
    property IsVirtual: Boolean read GetIsVirtual write SetIsVirtual;
    property RealCol: Integer read FRealCol write FRealCol;
    property RealRow: Integer read FRealRow write FRealRow;
    property Row: Integer read GetRow;
  end;

  { TdxSpreadSheetAbstractReportLink }

  TdxSpreadSheetAbstractReportLink = class(TAbstractdxGridReportLink)
  strict private
    FCachedDisplayCellStyle: TdxPSSpreadSheetCellDisplayStyleCache;
    FCellObjects: TList;
    FColCount: Integer;
    FExtraColumnCount: Integer;
    FFontIndexes: TDictionary<TObject, Integer>;
    FHostContainers: TdxReportCell;
    FHostMergedCells: TdxReportCell;
    FProcessingMerges: Boolean;
    FRowCount: Integer;
    FSourceHeaderFontIndex: Integer;
    FTempFont: TFont;

    function GetEditProperties: TcxCustomEditProperties;
    function GetItemViewParams(AItem: TdxReportCellSSString): TdxReportItemViewParams;
    function GetMeaningColCount: Integer;
    function GetMeaningContainersArea: TRect;
    function GetMeaningRowCount: Integer;
    function GetMergedCells: TdxSpreadSheetMergedCellList;
    function GetRealColor(AColor, ADefaultColor: TColor): TColor; inline;
    function GetSheet: TdxSpreadSheetTableView;
    function GetSpreadSheet: TdxCustomSpreadSheet;
  protected
    FAppendingExtraColumns: Boolean;
    FGridAdapter: TdxPSGridCellsAdapter;

    procedure DeleteCellObjects;

    function CannotActivateReportErrorString: string; override;
    procedure AfterConstruct(AReportCells: TdxReportCells); override;
    procedure ConstructReportContent(AReportCells: TdxReportCells); override;
    procedure PrepareConstruct(AReportCells: TdxReportCells); override;

    function GetColumnOffset(ACol: Integer): Integer;
    function GetRowOffset(ARow: Integer): Integer;
    function GetCellText(ACol, ARow: Integer): string; override;
    function GetCellTextCore(ACell: TdxSpreadSheetCell): string; virtual;
    function GetCellSides(ACol, ARow: Integer): TdxCellSides; override;
    function GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; override;
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; override;
    function GetDefaultCellBorderColor: TColor; virtual;
    function GetFlatIndex(ACol, ARow: Integer): Integer;
    function GetMinRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont): Integer; override;
    function GetSourceCellColor(ACol, ARow: Integer): TColor; override;
    function GetSourceCellContentBkColor(ACol, ARow: Integer): TColor; override;
    function GetSourceCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass; override;
    function GetSourceCellEdge3DSoft(ACol, ARow: Integer): Boolean; override;
    function GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode; override;
    function GetSourceCellFont(ACol, ARow: Integer): TFont; override;
    function GetSourceCellFontIndex(ACol, ARow: Integer): Integer; override;
    function GetSourceCellMultiline(ACol, ARow: Integer): Boolean; override;
    function GetSourceCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX; override;
    function GetSourceCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY; override;
    function GetSourceCellTransparent(ACol, ARow: Integer): Boolean; override;
    function GetSourceColWidth(ACol: Integer): Integer; override;
    function GetSourceRowHeight(ARow: Integer): Integer; override;
    function GetSSCellObject(ARow, ACol: Integer): TdxSpreadSheetCell;
    function GetSSCellStyle(ARow, ACol: Integer): TdxSpreadSheetCellStyle;
    function GetSSDefaultStyle: TdxSpreadSheetCellStyle; inline;

    function GetColCount: Integer; override;
    function GetFixedColCount: Integer; override;
    function GetFixedRowCount: Integer; override;
    function GetRowCount: Integer; override;

    function IsCellContainsRTFText(ACol, ARow: Integer): Boolean;
    function IsDrawBorder: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    function IsDrawVertLines: Boolean; override;
    function IsEmptyCell(const ACell: TdxSpreadSheetCell): Boolean;
    function IsEmptyRow(ARow: Integer): Boolean; override;
    function IsMergedBorder(ACol, ARow: Integer; ASide: TdxCellSide): Boolean;
    function IsMergedCell(ACol, ARow: Integer): Boolean; virtual;
    function IsNearMostLeftCell(ACol, ARow: Integer): Boolean; virtual;
    function IsNearMostTopCell(ACol, ARow: Integer): Boolean; virtual;
    function IsProcessedCol(ACol: Integer): Boolean; override;
    function IsProcessedRow(ARow: Integer): Boolean; override;
    function IsScaleGridLines: Boolean; override;
    function IsShowGridLines: Boolean; virtual; abstract;
    function IsShowRowAndColumnHeadings: Boolean; virtual; abstract;
    function IsSuppressFontColors: Boolean; virtual;
    function IsSuppressSourceFormats: Boolean; virtual;
    function NeedTwoPassRendering: Boolean; override;
    function UseGrayScaleForGraphics: Boolean; virtual;

    procedure AddMergedCells(AReportCells: TdxReportCells);
    procedure AppendAdditionalColumns(AAdapter: TdxPSGridCellsAdapter);
    procedure CalcTextExtents(AAdapter: TdxPSGridCellsAdapter);
    procedure FixupRowWidths(AAdapter: TdxPSGridCellsAdapter);
    procedure DeleteUnneededCellSides(AAdapter: TdxPSGridCellsAdapter);
    function HasMergedCells: Boolean;
    procedure PostProcessItems(AAdapter: TdxPSGridCellsAdapter);

    // Containers
    procedure AddContainers(AReportCells: TdxReportCells); virtual;
    function CalculateContainerBounds(AViewInfo: TdxSpreadSheetContainerViewInfo): TRect; virtual;
    function CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean; virtual;
    function CreateContainerImage(AViewInfo: TdxSpreadSheetContainerViewInfo): TdxSmartImage; virtual;
    function CreateContainersHost(AReportCells: TdxReportCells): TdxReportCell; virtual;
    procedure InitializeContainerCell(ACell: TdxReportCell; AViewInfo: TdxSpreadSheetContainerViewInfo); virtual;
    function GetContainerBoundsOffset: TPoint;
    function TranslateContainerBounds(AView: TdxSpreadSheetCustomView; var ABounds: TRect): Boolean; virtual;

    function GetCellBoundsInReport(ARow, AColumn: Integer; AConsiderMergedCells: Boolean; out ARect: TRect): Boolean;
    function ReportAreaToSpreadSheetArea(const R: TRect): TRect;
    function SpreadSheetAreaToReportArea(const R: TRect): TRect;

    function GetBorderStylePatternClass(AStyle: TdxSpreadSheetCellBorderStyle): TdxPSEdgePatternClass; virtual;
    function OnlyEdgeIsAssigned(const ACell: TdxSpreadSheetCell; ASide: TcxBorder): Boolean;
    function OnlyLeftEdgeIsAssigned(ACell: TdxSpreadSheetCell): Boolean;
    function OnlyTopEdgeIsAssigned(ACell: TdxSpreadSheetCell): Boolean;
    procedure SetupCellBorders(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); virtual;
    //
    property ColCount: Integer read GetColCount;
    property EditProperties: TcxCustomEditProperties read GetEditProperties;
    property ExtraColumnCount: Integer read FExtraColumnCount;
    property HostContainers: TdxReportCell read FHostContainers;
    property HostMergedCells: TdxReportCell read FHostMergedCells;
    property MeaningColCount: Integer read GetMeaningColCount;
    property MeaningRowCount: Integer read GetMeaningRowCount;
    property MergedCells: TdxSpreadSheetMergedCellList read GetMergedCells;
    property RowCount: Integer read GetRowCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Sheet: TdxSpreadSheetTableView read GetSheet;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

var
  dxPSFillPatternClassMap: array[TdxSpreadSheetCellFillStyle] of TdxPSFillPatternClass = (
    TdxPSSolidFillPattern, TdxPSGray75FillPattern, TdxPSGray50FillPattern,
    TdxPSGray25FillPattern, TdxPSGray125FillPattern, TdxPSGray625FillPattern,
    TdxPSHorizontalStripeFillPattern,  TdxPSVerticalStripeFillPattern,
    TdxPSReverseDiagonalStripeFillPattern, TdxPSDiagonalStripeFillPattern,
    TdxPSDiagonalCrossHatchFillPattern, TdxPSThickCrossHatchFillPattern,
    TdxPSThinHorizontalStripeFillPattern, TdxPSThinVerticalStripeFillPattern,
    TdxPSThinReverseDiagonalStripeFillPattern, TdxPSThinDiagonalStripeFillPattern,
    TdxPSThinDiagonalCrossHatchFillPattern, TdxPSThinHorizontalCrossHatchFillPattern
  );

  dxCellEdgeSideOrientation: array[TdxCellSide] of TdxPSCellEdgePatternOrientation = (
    cepoVertical, cepoHorizontal, cepoVertical, cepoHorizontal
  );

  dxPSEdgePatternClassMap: array[TdxSpreadSheetCellBorderStyle] of TdxPSEdgePatternClass = (
    TdxPSSolidEdgePattern, TdxPSHairEdgePattern, TdxPSDottedEdgePattern, TdxPSDashDotDotEdgePattern,
    TdxPSDashDotEdgePattern, TdxPSDashedEdgePattern, TdxPSSolidEdgePattern, TdxPSMediumDashDotDotEdgePattern,
    TdxPSSlantedDashDotEdgePattern, TdxPSMediumDashDotEdgePattern, TdxPSMediumDashedEdgePattern, TdxPSMediumSolidEdgePattern,
    TdxPSThickSolidEdgePattern, TdxPSDoubleLineSolidEdgePattern, TdxPSSolidEdgePattern
  );

  dxPSTextAlignXMap: array[TdxSpreadSheetDataAlignHorz] of TcxTextAlignX = (
    taLeft, taLeft, taCenterX, taRight, taLeft, taDistributeX, taDistributeX
  );

  dxPSTextAlignYMap: array[TdxSpreadSheetDataAlignVert] of TcxTextAlignY = (
    taTop, taCenterY, taBottom, taDistributeY, taDistributeY
  );
implementation

uses
  SysUtils, dxSpreadSheetConditionalFormattingIconSet, cxGeometry, Math, dxSpreadSheetCoreStyles, RTLConsts, dxCoreClasses,
  dxSpreadSheetContainers, dxPSRes, dxPSUtl, dxSmartImage, dxDPIAwareUtils;

const
  { Since we don't use following Format Bits in TdxReportCellSSString we are allowed to safely override them }
  dxFormatClipContent  = dxPSGlbl.dxFormatMakeSpaceForEmptyImage;
  dxFormatFill         = dxPSGlbl.dxFormatCheckEnabled;
  dxFormatFixed        = dxPSGlbl.dxFormatCheckBold;
  dxFormatMerged       = dxPSGlbl.dxFormatImageTransparent;
  dxFormatNearMostLeft = dxPSGlbl.dxFormatCheckFlat;
  dxFormatNearMostTop  = dxPSGlbl.dxFormatCheckChecked;
  dxFormatVirtual      = dxPSGlbl.dxFormatImageTransparent;
  dxFormatRTF          = dxPSGlbl.dxFormatCheckPosCenter;

  SubEndMask = $00000003;
  SubBitsPerEnd = 2;
  SubBitsPerSide = SubBitsPerEnd * (Integer(High(TdxPSCellBorderEnd)) + 1);
  SlantBitsPerSide = 4;
  SlantMask = $00000001;

  { from SS }
  SystemColorStart = 55;

type
  TdxSpreadSheetTableItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetCellDataBarAccess = class(TdxSpreadSheetCellDataBar);
  TdxSpreadSheetCellFontAccess = class(TdxSpreadSheetCellFont);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetCustomViewAccess = class(TdxSpreadSheetCustomView);
  TdxSpreadSheetEditingControllerAccess = class(TdxSpreadSheetTableViewEditingController);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetShapeAccess = class(TdxSpreadSheetShape);

function AreStylesEqual(AStyle1, AStyle2: TdxSpreadSheetCellStyle; AExclusion: TcxBorders = []): Boolean;

  function AreBordersEqual(AStyle1, AStyle2: TdxSpreadSheetCellStyle; AExclusion: TcxBorders = []): Boolean;
  var
    ACompareResult: Boolean;
    ASide: TcxBorder;
  begin
    for ASide := Low(TcxBorder) to High(TcxBorder) do
    begin
      ACompareResult :=
        (AStyle1.Handle.Borders.BorderColor[ASide] = AStyle2.Handle.Borders.BorderColor[ASide]) and
        (AStyle1.Handle.Borders.BorderStyle[ASide] = AStyle2.Handle.Borders.BorderStyle[ASide]);

      if ASide in AExclusion then
        Result := not ACompareResult
      else
        Result := ACompareResult;

      if not Result then
        Break;
    end;
  end;

begin
  if AExclusion = [] then
    Exit(AStyle1.Handle = AStyle2.Handle);

  Result :=
    (AStyle1.Brush.BackgroundColor = AStyle2.Brush.BackgroundColor) and
    (AStyle1.Brush.ForegroundColor = AStyle2.Brush.ForegroundColor) and
    (AStyle1.Brush.Style = AStyle2.Brush.Style) and AreBordersEqual(AStyle1, AStyle2, AExclusion);
end;

function IsCellDataEmpty(const ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := (ACell = nil) or ACell.IsEmpty or (ACell.AsString = '');
end;

{ TdxPSCellBorder }

constructor TdxPSCellBorder.Create(APattern: TdxPSEdgePatternClass; AColor: TColor);
begin
  Color := AColor;
  Pattern := APattern;
end;

{ TdxPSCellPatternsBorderPainter }

function TdxPSCellPatternsBorderPainter.Item: TdxReportCellSSString;
begin
  Result := inherited Item as TdxReportCellSSString;
end;

procedure TdxPSCellPatternsBorderPainter.ClipItemBounds(ACanvas: TdxPSReportRenderCustomCanvas; AOuterRect: TRect);
begin
  with Item do
  begin
    if IsNearMostTop then
      Inc(AOuterRect.Top, LineThickness * BorderEdgeSalients[csTop, bstOuter]);
    if IsNearMostLeft then
      Inc(AOuterRect.Left, LineThickness * BorderEdgeSalients[csLeft, bstOuter]);
  end;
  ACanvas.IntersectClipRgn(AOuterRect)
end;

procedure TdxPSCellPatternsBorderPainter.DrawBorder(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect; ASide: TdxCellSide);
var
  ABorder: TdxPSCellBorder;
  ARegion: TcxRegionHandle;
begin
  if Item.IsDoubleLineBorderPattern(ASide) then
    ARegion := Item.GetDoubleBorderRgn(ASide, R)
  else
    ARegion := CreateRectRgnIndirect(R);

  ABorder := Item.Borders[ASide];
  ACanvas.FillEdge(ARegion, Item.BordersBkColor, ABorder.Color, ASide in [csLeft, csRight], ABorder.Pattern);
  DeleteObject(ARegion);
end;

procedure TdxPSCellPatternsBorderPainter.DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
var
  ABorder: TdxPSCellBorder;
  ABorderRect: TRect;
  ABrushOrg: TPoint;
  ASide: TdxCellSide;
  ATempRect: TRect;
begin
  ACanvas.SaveClipRgn;
  try
    if Item.ClipContent then
      ClipItemBounds(ACanvas, R);

    for ASide := Low(TdxCellSide) to High(TdxCellSide) do
      if ASide in Item.CellSides then
      begin
        ABorderRect := Item.GetBorderEdgeBounds(ASide, R);
        if ACanvas.IsRectVisible(ABorderRect) then
        begin
          ABrushOrg := ACanvas.BrushOrg;
          try
            ABorder := Item.Borders[ASide];
            if (ABorder.Pattern <> nil) and ABorder.Pattern.RequiredBrushOrigin then
            begin
              ATempRect := ABorderRect;
              ACanvas.LogicalToDeviceCoordinates(ATempRect);
              ACanvas.BrushOrg := Point(ATempRect.Left mod 2, ATempRect.Top mod 2);
            end;
            ACanvas.FixupRect(ABorderRect);
            DrawBorder(ACanvas, ABorderRect, ASide);
          finally
            ACanvas.BrushOrg := ABrushOrg;
          end;
        end;
      end;
  finally
    ACanvas.RestoreClipRgn;
  end;
end;

{ TdxPSGridCellsAdapter }

constructor TdxPSGridCellsAdapter.Create(AReportCells: TdxReportCells);
begin
  inherited Create;
  FReportCells := AReportCells;
end;

function TdxPSGridCellsAdapter.GetNeighborCell(AItem: TdxReportCellSSString; ASide: TdxCellSide): TdxReportCellSSString;
begin
  Result := nil;
  case ASide of
    csLeft:
      if AItem.Col > 0 then
        Result := Cells[AItem.Col - 1, AItem.Row];
    csTop:
      if AItem.Row > 0 then
        Result := Cells[AItem.Col, AItem.Row - 1];
    csRight:
      if AItem.Col + 1 < ColCount then
        Result := Cells[AItem.Col + 1, AItem.Row];
    csBottom:
      if AItem.Row + 1 < RowCount then
        Result := Cells[AItem.Col, AItem.Row + 1];
  end;
end;

function TdxPSGridCellsAdapter.GetCell(Col, Row: Integer): TdxReportCellSSString;
var
  AItem: TAbstractdxReportCellData;
begin
  AItem := FReportCells.Cells[Row].DataItems[Col];
  if AItem is TdxReportCellSSString then
    Result := TdxReportCellSSString(AItem)
  else
    Result := nil;
end;

function TdxPSGridCellsAdapter.GetColCount: Integer;
begin
  if FReportCells.Cells.CellCount > 0 then
    Result := FReportCells.Cells[0].DataItemCount
  else
    Result := 0;
end;

function TdxPSGridCellsAdapter.GetColOffset(Index: Integer): Integer;
begin
  if Index < ColCount then
    Result := Cells[Index, 0].Left
  else
    Result := Cells[ColCount - 1, 0].Left + Cells[ColCount - 1, 0].Width;
end;

function TdxPSGridCellsAdapter.GetColWidth(Index: Integer): Integer;
begin
  Result := Cells[Index, 0].Width;
end;

function TdxPSGridCellsAdapter.GetRow(Index: Integer): TdxReportCell;
begin
  Result := FReportCells.Cells[Index];
end;

function TdxPSGridCellsAdapter.GetRowCount: Integer;
begin
  Result := FReportCells.Cells.CellCount;
end;

function TdxPSGridCellsAdapter.GetRowHeight(Index: Integer): Integer;
begin
  Result := Cells[0, Index].Height;
end;

function TdxPSGridCellsAdapter.GetRowIndex(Index: Integer): Integer;
begin
  Result := Rows[Index].Data;
end;

function TdxPSGridCellsAdapter.GetRowOffset(Index: Integer): Integer;
begin
  if Index < RowCount then
    Result := Cells[0, Index].Top
  else
    Result := Cells[0, RowCount - 1].Top + Cells[0, RowCount - 1].Height;
end;

{ TdxPSSpreadSheetCellDisplayStyleCache }

constructor TdxPSSpreadSheetCellDisplayStyleCache.Create(AOwner: TdxSpreadSheetTableView);
begin
  inherited Create(AOwner);
  Column := -1;
  Row := -1;
end;

{ TdxPSSSStringDataMap }

class procedure TdxPSSSStringDataMap.InitializeItem(AItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: Variant; const ACellParams: IdxPSCellParams;
  var AViewParams: TdxReportItemViewParams; AIsPreview: Boolean; ARecordIndex: Integer; AOwner: TObject);
var
  AAdvancedViewParams: IdxSpreadSheetAdvancedViewParams;
  AColor1, AColor2: TColor;
  AStyle: TdxSpreadSheetCellFillStyle;
begin
  inherited InitializeItem(AItem, AProperties, AValue, ACellParams, AViewParams, AIsPreview, ARecordIndex, AOwner);

  if AItem is TdxReportCellBaseSSString then
  begin
    if Supports(AViewParams.AdvancedViewParams, IdxSpreadSheetAdvancedViewParams, AAdvancedViewParams) then
    begin
      TdxReportCellBaseSSString(AItem).DataBar.Assign(AAdvancedViewParams.GetDataBar);
      TdxReportCellBaseSSString(AItem).IconIndex := AAdvancedViewParams.GetIconSetIndex;
      TdxReportCellBaseSSString(AItem).IconSize := ConditionalFormattingIconSet.Icons.Width;

      AStyle := AAdvancedViewParams.GetFillStyle(AColor1, AColor2);
      TdxReportCellBaseSSString(AItem).Color := AColor1;
      TdxReportCellBaseSSString(AItem).ContentBkColor := AColor2;
      TdxReportCellBaseSSString(AItem).ContentPattern := dxPSFillPatternClassMap[AStyle];

      if not AAdvancedViewParams.GetShowCellValue then
        TdxReportCellBaseSSString(AItem).Text := '';
    end;
  end;
end;

class function TdxPSSSStringDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean): TdxReportCellDataClass;
begin
  if Supports(AViewParams.AdvancedViewParams, IdxSpreadSheetAdvancedViewParams) then
    Result := TdxReportCellBaseSSString
  else
    Result := inherited ItemClass(AProperties, AViewParams, AIsPreview);
end;

{ TdxPSSSStringGridCellDataMap }

class procedure TdxPSSSStringGridCellDataMap.InitializeCellData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink);
var
  ACell: TdxSpreadSheetCell;
  ACellStyle: TdxSpreadSheetCellStyle;
begin
  inherited InitializeCellData(ACol, ARow, ADataItem, AReportLink);

  with AReportLink as TdxSpreadSheetAbstractReportLink do
  begin
    if not IsFixedCell(ACol, ARow) then
    begin
      ACell := GetSSCellObject(ARow, ACol);
      if ACell <> nil then
      begin
        TdxReportCellSSString(ADataItem).Fill := ACell.Style.AlignHorz = ssahFill;
        TdxReportCellSSString(ADataItem).TextAlignHorzIndent := ACell.Style.AlignHorzIndent;
      end;

      ACellStyle := GetSSCellStyle(ARow, ACol);
      if ACellStyle is TdxSpreadSheetTableViewCellDisplayStyle then
      begin
        TdxReportCellSSString(ADataItem).DataBar := TdxSpreadSheetTableViewCellDisplayStyle(ACellStyle).DataBar;
        TdxReportCellSSString(ADataItem).IconIndex := TdxSpreadSheetTableViewCellDisplayStyle(ACellStyle).IconIndex;
        TdxReportCellSSString(ADataItem).IconSize := ConditionalFormattingIconSet.Icons.Width;
      end;
    end;

    TdxReportCellSSString(ADataItem).IsFixed := IsFixedCell(ACol, ARow);
    TdxReportCellSSString(ADataItem).IsMerged := IsMergedCell(ACol, ARow);
    TdxReportCellSSString(ADataItem).IsNearMostLeft := IsNearMostLeftCell(ACol, ARow);
    TdxReportCellSSString(ADataItem).IsNearMostTop := IsNearMostTopCell(ACol, ARow);
    TdxReportCellSSString(ADataItem).ClipContent := IsShowRowAndColumnHeadings and TdxReportCellSSString(ADataItem).IsNearMostTopOrLeft;
    TdxReportCellSSString(ADataItem).IsRTF := IsCellContainsRTFText(ACol, ARow);
    TdxReportCellSSString(ADataItem).RealCol := ACol;
    TdxReportCellSSString(ADataItem).RealRow := ARow;

    SetupCellBorders(ACol, ARow, ADataItem);
  end;
end;

class function TdxPSSSStringGridCellDataMap.DataClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellSSString;
end;

{ TdxSpreadSheetAdvancedViewParams }

constructor TdxSpreadSheetAdvancedViewParams.Create;
begin
  FDataBar := TdxSpreadSheetCellDataBar.Create;
end;

destructor TdxSpreadSheetAdvancedViewParams.Destroy;
begin
  FreeAndNil(FDataBar);
  inherited;
end;

class function TdxSpreadSheetAdvancedViewParams.CreateFrom(
  AStyle: TdxSpreadSheetCellDisplayStyle): IdxSpreadSheetAdvancedViewParams;
var
  AParams: TdxSpreadSheetAdvancedViewParams;
begin
  if (AStyle.IconIndex >= 0) or not AStyle.DataBar.IsEmpty or (AStyle.Brush.Style <> sscfsSolid) then
  begin
    AParams := TdxSpreadSheetAdvancedViewParams.Create;
    AParams.FBackgroundColor1 := AStyle.Brush.BackgroundColor;
    AParams.FBackgroundColor2 := AStyle.Brush.ForegroundColor;
    AParams.FBackgroundFillStyle := AStyle.Brush.Style;
    AParams.FDataBar.Assign(AStyle.DataBar);
    AParams.FIconSetIndex := AStyle.IconIndex;
    AParams.FShowCellValue := AStyle.ShowCellValue;
    Result := AParams;
  end
  else
    Result := nil;
end;

function TdxSpreadSheetAdvancedViewParams.GetDataBar: TdxSpreadSheetCellDataBar;
begin
  Result := FDataBar;
end;

function TdxSpreadSheetAdvancedViewParams.GetFillStyle(out AColor1, AColor2: TColor): TdxSpreadSheetCellFillStyle;
begin
  AColor1 := FBackgroundColor1;
  AColor2 := FBackgroundColor2;
  Result := FBackgroundFillStyle;
end;

function TdxSpreadSheetAdvancedViewParams.GetIconSetIndex: Integer;
begin
  Result := FIconSetIndex;
end;

function TdxSpreadSheetAdvancedViewParams.GetShowCellValue: Boolean;
begin
  Result := FShowCellValue;
end;

{ TdxReportCellConnectionLine }

constructor TdxReportCellConnectionLine.Create(AParent: TdxReportCell);
begin
  inherited;
  FArrowSize := 6;
  CellSides := [];
  Color := clWindowText;
  Transparent := True;
end;

procedure TdxReportCellConnectionLine.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxReportCellConnectionLine then
  begin
    LinePoint1 := TdxReportCellConnectionLine(Source).LinePoint1;
    LinePoint2 := TdxReportCellConnectionLine(Source).LinePoint2;
  end;
end;

procedure TdxReportCellConnectionLine.DrawBackground(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  inherited;
  ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, BoundsRect.TopLeft);
  ACanvas.Polyline(FLinePoints, Color, Renderer.LineThickness);
  ACanvas.Polygon(FArrowPoints, Color, Color, Renderer.LineThickness);
  ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, BoundsRect.TopLeft, False);
end;

procedure TdxReportCellConnectionLine.AdjustBounds;
begin
  CalculateArrow;
  BoundsRect := cxRect(
    Min(LinePoint1.X, LinePoint2.X),
    Min(LinePoint1.Y, LinePoint2.Y),
    Max(LinePoint1.X, LinePoint2.X),
    Max(LinePoint1.Y, LinePoint2.Y));
end;

procedure TdxReportCellConnectionLine.CalculateArrow;

  function CalculateArrowPoint(const AAngle: Double): TPoint;
  begin
    Result.X := Round(LinePoint1.X + FArrowSize * Sin(AAngle));
    Result.Y := Round(LinePoint1.Y + FArrowSize * Cos(AAngle));
  end;

var
  AAngle: Double;
begin
  AAngle := Pi / 2 - ArcTan2(LinePoint2.Y - LinePoint1.Y, LinePoint2.X - LinePoint1.X);
  FArrowPoints[0] := CalculateArrowPoint(AAngle - ArrowAngle * Pi / 180);
  FArrowPoints[1] := LinePoint1;
  FArrowPoints[2] := CalculateArrowPoint(AAngle + ArrowAngle * Pi / 180);
end;

procedure TdxReportCellConnectionLine.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited;
  FArrowSize := MulDiv(FArrowSize, APixelsNumerator, APixelsDenominator);
  FLinePoints[0] := cxPointScale(FLinePoints[0], APixelsNumerator, APixelsDenominator);
  FLinePoints[1] := cxPointScale(FLinePoints[1], APixelsNumerator, APixelsDenominator);
  CalculateArrow;
end;

procedure TdxReportCellConnectionLine.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  FLinePoints[0] := AReader.ReadPoint;
  FLinePoints[1] := AReader.ReadPoint;
end;

procedure TdxReportCellConnectionLine.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WritePoint(LinePoint1);
  AWriter.WritePoint(LinePoint2);
end;

function TdxReportCellConnectionLine.GetLinePoint(Index: Integer): TPoint;
begin
  Result := FLinePoints[Index];
end;

procedure TdxReportCellConnectionLine.SetLinePoint(Index: Integer; const Value: TPoint);
begin
  FLinePoints[Index] := Value;
  AdjustBounds;
end;

{ TdxReportCellBaseSSString }

constructor TdxReportCellBaseSSString.Create(AParent: TdxReportCell);
begin
  inherited;
  FIconIndex := -1;
  FDataBar := TdxSpreadSheetCellDataBar.Create;
end;

destructor TdxReportCellBaseSSString.Destroy;
begin
  FreeAndNil(FDataBar);
  inherited;
end;

procedure TdxReportCellBaseSSString.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TdxReportCellBaseSSString then
  begin
    FContentBkColor := TdxReportCellBaseSSString(Source).FContentBkColor;
    FContentPattern := TdxReportCellBaseSSString(Source).FContentPattern;
    FTextExtentLeft := TdxReportCellBaseSSString(Source).FTextExtentLeft;
    FTextExtentRight := TdxReportCellBaseSSString(Source).FTextExtentRight;
    FTextAlignHorzIndent := TdxReportCellBaseSSString(Source).TextAlignHorzIndent;
    FIconIndex := TdxReportCellBaseSSString(Source).IconIndex;
    FIconSize := TdxReportCellBaseSSString(Source).IconSize;
    FDataBar.Assign(TdxReportCellBaseSSString(Source).DataBar);
  end;
end;

procedure TdxReportCellBaseSSString.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  if rsFirstPass in AStage then
  begin
    if IsBackgroundDrawn then
      DrawBackground(ACanvas);
    if not DataBar.IsEmpty then
      DrawDataBar(ACanvas);
    if IconIndex >= 0 then
      DrawIcon(ACanvas);
  end;

  if rsSecondPass in AStage then
  begin
    if IsTextDrawn then
      DrawText(ACanvas);
    if IsBordersDrawn then
      DrawBorders(ACanvas);
  end;
end;

procedure TdxReportCellBaseSSString.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  TextAlignHorzIndent := MulDiv(TextAlignHorzIndent, APixelsNumerator, APixelsDenominator);
  TextExtentLeft := MulDiv(TextExtentLeft, APixelsNumerator, APixelsDenominator);
  TextExtentRight := MulDiv(TextExtentRight, APixelsNumerator, APixelsDenominator);
  IconSize := MulDiv(IconSize, APixelsNumerator, APixelsDenominator);
end;

procedure TdxReportCellBaseSSString.DrawDataBar(ACanvas: TdxPSReportRenderCustomCanvas);

  function GetActualBarColor: TColor;
  begin
    if (DataBar.Position >= DataBar.AxisPosition) or (DataBar.Color2 = clNone) then
      Result := DataBar.Color1
    else
      Result := DataBar.Color2;
  end;

var
  AAxisBounds: TRect;
  ABarBounds: TRect;
begin
  GetDataBarBounds(ACanvas, AAxisBounds, ABarBounds);

  if not DataBar.IsEmpty then
  begin
    ACanvas.FillRect(ABarBounds, GetActualBarColor);
    if DataBar.Border <> clNone then
      ACanvas.DrawFrame(ABarBounds, DataBar.Border, DataBar.Border);
  end;

  if DataBar.AxisColor <> clNone then
    ACanvas.FillRect(AAxisBounds, DataBar.AxisColor);
end;

procedure TdxReportCellBaseSSString.DrawIcon(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  Renderer.DrawGraphicEx(ACanvas, GetIconBounds(ACanvas), BoundsRect,
    ConditionalFormattingIconSet.Icons, IconIndex, nil, True, True, Color, Color, TdxPSSolidFillPattern);
end;

function TdxReportCellBaseSSString.GetContentBkColor: TColor;
begin
  Result := FContentBkColor;
end;

function TdxReportCellBaseSSString.GetContentPattern: TdxPSFillPatternClass;
begin
  Result := FContentPattern;
end;

procedure TdxReportCellBaseSSString.SetContentBkColor(Value: TColor);
begin
  FContentBkColor := Value;
end;

procedure TdxReportCellBaseSSString.SetContentPattern(Value: TdxPSFillPatternClass);
begin
  FContentPattern := Value;
end;

procedure TdxReportCellBaseSSString.GetDataBarBounds(ACanvas: TdxPSReportRenderCustomCanvas; out AAxisBounds, ABarBounds: TRect);

  function GetPointPosition(const AContentBounds: TRect; APosition: Single): Integer;
  begin
    Result := AContentBounds.Left + Round(APosition * cxRectWidth(AContentBounds));
  end;

  function GetAxisBounds(const AContentBounds: TRect): TRect;
  begin
    Result := AContentBounds;
    Result.Left := GetPointPosition(AContentBounds, DataBar.AxisPosition);
    Result.Right := Result.Left + LineThickness;
  end;

  function GetBarBounds(const AAxisBounds, AContentBounds: TRect): TRect;
  begin
    Result := cxRectInflate(AContentBounds, -cxTextSpace * LineThickness);
    if DataBar.Position >= DataBar.AxisPosition then
    begin
      Result.Left := AAxisBounds.Right;
      Result.Right := GetPointPosition(AContentBounds, DataBar.Position);
    end
    else
    begin
      Result.Left := GetPointPosition(AContentBounds, DataBar.Position);
      Result.Right := AAxisBounds.Left;
    end;
  end;

begin
  AAxisBounds := GetAxisBounds(GetInnerBounds(ACanvas));
  ABarBounds := GetBarBounds(AAxisBounds, GetInnerBounds(ACanvas));
end;

function TdxReportCellBaseSSString.GetIconBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
  if IconIndex >= 0 then
  begin
    Result := cxRectSetWidth(Result, IconSize);
    Result := cxRectCenterVertically(Result, IconSize);
  end
  else
    Result := cxRectSetWidth(Result, 0);
end;

function TdxReportCellBaseSSString.GetTextBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := inherited GetTextBounds(ACanvas);
  Result.Left := GetIconBounds(ACanvas).Right;
  if TextAlignX in [taLeft, taDistributeX] then
    Inc(Result.Left, TextAlignHorzIndent);
  if TextAlignX in [taRight, taDistributeX] then
    Dec(Result.Right, TextAlignHorzIndent);
  if TextExtentLeft <> 0 then
    Result.Left := TextExtentLeft;
  if TextExtentRight <> 0 then
    Result.Right := TextExtentRight;
end;

procedure TdxReportCellBaseSSString.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  ReadDataCore(AReader);
end;

procedure TdxReportCellBaseSSString.ReadDataCore(AReader: TdxPSDataReader);
begin
  TextExtentLeft := AReader.ReadInteger;
  TextExtentRight := AReader.ReadInteger;
  if dxPSCheckVersion(AReader.PSVersion, 4, 20140104) then
    TextAlignHorzIndent := AReader.ReadInteger;
  if dxPSCheckVersion(AReader.PSVersion, 4, 20150200) then
  begin
    IconSize := AReader.ReadInteger;
    IconIndex := AReader.ReadInteger;
    TdxSpreadSheetCellDataBarAccess(DataBar).ReadData(AReader);
  end;
end;

procedure TdxReportCellBaseSSString.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  WriteDataCore(AWriter);
end;

procedure TdxReportCellBaseSSString.WriteDataCore(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteInteger(TextExtentLeft);
  AWriter.WriteInteger(TextExtentRight);
  if dxPSCheckVersion(AWriter.PSVersion, 4, 20140104) then
    AWriter.WriteInteger(TextAlignHorzIndent);
  if dxPSCheckVersion(AWriter.PSVersion, 4, 20150200) then
  begin
    AWriter.WriteInteger(IconSize);
    AWriter.WriteInteger(IconIndex);
    TdxSpreadSheetCellDataBarAccess(DataBar).WriteData(AWriter);
  end;
end;

procedure TdxReportCellBaseSSString.SetDataBar(AValue: TdxSpreadSheetCellDataBar);
begin
  FDataBar.Assign(AValue);
end;

{ TdxReportCellSSString }

constructor TdxReportCellSSString.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  PreventLeftTextExceed := False;
  TextAlignY := taBottom;
end;

procedure TdxReportCellSSString.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TdxReportCellSSString then
  begin
    FBorders := TdxReportCellSSString(Source).FBorders;
    FBorderSlants  := TdxReportCellSSString(Source).FBorderSlants;
    FBorderSubs := TdxReportCellSSString(Source).FBorderSubs;
    FRealCol := TdxReportCellSSString(Source).FRealCol;
    FRealRow := TdxReportCellSSString(Source).FRealRow;
  end;
end;

procedure TdxReportCellSSString.SetBorders(AColor: TColor; APattern: TdxPSEdgePatternClass);
var
  ABorder: TdxPSCellBorder;
  ASide: TdxCellSide;
begin
  for ASide := csLeft to csBottom do
  begin
    ABorder := Borders[ASide];
    ABorder.Color := AColor;
    ABorder.Pattern := APattern;
    Borders[ASide] := ABorder;
  end;
end;

procedure TdxReportCellSSString.CalcBorderSubs(AnAdapter: TdxPSGridCellsAdapter);

  function GetNeighborCell(ASide: TdxCellSide): TdxReportCellSSString;
  begin
    Result := AnAdapter.GetNeighborCell(Self, ASide);
  end;

  function GetNeighborCellBorder(ASide, ABorderSide: TdxCellSide): TdxPSCellBorder;
  var
    Neighbor: TdxReportCellSSString;
  begin
    Neighbor := GetNeighborCell(ASide);
    if Neighbor <> nil then
      Result := Neighbor.Borders[ABorderSide]
    else
      Result := NullBorder;
    if Result.Pattern = nil then
      Result := Borders[ASide];
  end;

  function GetNeighborCellBorderSalient(ASide, ABorderSide: TdxCellSide;
    ASalient: TdxPSCellBorderSalientType): Integer;
  var
    Neighbor: TdxReportCellSSString;
  begin
    Neighbor := GetNeighborCell(ASide);
    if Neighbor <> nil then
      Result := Neighbor.BorderEdgeSalients[ABorderSide, ASalient]
    else
      Result := 0;
  end;

  procedure GetBordersAtCorner(ACorner: TdxPSCellCorner; out ABorders: TdxPSCellBorders);
  begin
    case ACorner of
      ccTopLeft:
        begin
          ABorders[csLeft] := GetNeighborCellBorder(csLeft, csTop);
          ABorders[csTop] := GetNeighborCellBorder(csTop, csLeft);
          ABorders[csRight] := Borders[csTop];
          ABorders[csBottom] := Borders[csLeft];
        end;
      ccTopRight:
        begin
          ABorders[csLeft] := Borders[csTop];
          ABorders[csTop] := GetNeighborCellBorder(csTop, csRight);
          ABorders[csRight] := GetNeighborCellBorder(csRight, csTop);
          ABorders[csBottom] := Borders[csRight];
        end;
      ccBottomRight:
        begin
          ABorders[csLeft] := Borders[csBottom];
          ABorders[csTop] := Borders[csRight];
          ABorders[csRight] := GetNeighborCellBorder(csRight, csBottom);
          ABorders[csBottom] :=  GetNeighborCellBorder(csBottom, csRight);
        end;
      ccBottomLeft:
        begin
          ABorders[csLeft] := GetNeighborCellBorder(csLeft, csBottom);
          ABorders[csTop] := Borders[csLeft];
          ABorders[csRight] := Borders[csBottom];
          ABorders[csBottom] := GetNeighborCellBorder(csBottom, csLeft);
        end;
    end;
  end;

  function DontNeedCornerSubsCalculation(ACorner: TdxPSCellCorner; ABorders: TdxPSCellBorders): Boolean;
  var
    BorderCount, DblCount: Integer;
    Color: TColor;
    Side: TdxCellSide;
    Border: TdxPSCellBorder;
  begin
    BorderCount := 0;
    DblCount := 0;
    Color := clBlack;

    Result := False;
    for Side := csLeft to csBottom do
    begin
      Border := ABorders[Side];
      if Border.Pattern = nil then Continue;

      Inc(BorderCount);
      if IsDoubleLineBorderPattern(Border) then
        Inc(DblCount);

      if (DblCount <> 0) and (DblCount <> BorderCount) then
        Exit;

      if BorderCount = 1 then
        Color := Border.Color
      else
        if Color <> Border.Color then
          Exit;
    end;
    Result := True;
  end;

  function GetFavoriteSides(ACorner: TdxPSCellCorner; const ABorders: TdxPSCellBorders): TdxCellSides;
  const
    CornerSideMaps: array[TdxPSCellCorner, TdxCellSide] of TdxCellSides =
      (([],         [],        [csTop],    [csLeft]),
       ([csTop],    [],        [],         [csRight]),
       ([csBottom], [csRight], [],         []),
       ([],         [csLeft],  [csBottom], []));
  var
    StartSide, FavoriteSide, Side: TdxCellSide;
    DblCount: Integer;
    DblCandidates: TdxCellSides;
    Border, FavoriteBorder: TdxPSCellBorder;
  begin
    StartSide := csLeft;
    while (StartSide < csBottom) and (ABorders[StartSide].Pattern = nil) do
      Inc(StartSide);
    FavoriteSide := StartSide;
    FavoriteBorder := ABorders[FavoriteSide];

    DblCount := 0;
    DblCandidates := [];
    for Side := StartSide to csBottom do
    begin
       Border := ABorders[Side];
       if Border.Pattern = nil then Continue;

       if IsDoubleLineBorderPattern(Border) then
       begin
         Inc(DblCount);
         DblCandidates := DblCandidates + CornerSideMaps[ACorner, Side];
       end;

       if Side > StartSide then
         if not IsDoubleLineBorderPattern(Border) and
           (IsDoubleLineBorderPattern(FavoriteBorder) or
           (Border.Pattern.Thickness > FavoriteBorder.Pattern.Thickness) or
           ((Border.Pattern.Thickness = FavoriteBorder.Pattern.Thickness) and
           (Border.Color < FavoriteBorder.Color))) then
         begin
           FavoriteBorder := Border;
           FavoriteSide := Side;
         end;
    end;

    if DblCount > 1 then
      Result := DblCandidates
    else
      Result := CornerSideMaps[ACorner, FavoriteSide];
  end;

  function CalcBorderSub(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): Integer;
  const
    ConcurSides: array[TdxCellSide, TdxPSCellBorderEnd] of TdxCellSide =
      ((csTop, csBottom),
       (csLeft, csRight),
       (csTop, csBottom),
       (csLeft, csRight));
  var
    ConcurSide: TdxCellSide;
  begin
    ConcurSide := ConcurSides[ASide, AEnd];
    Result := BorderEdgeSalients[ConcurSide, bstOuter] +
      Max(BorderEdgeSalients[ConcurSide, bstInner], GetNeighborCellBorderSalient(ASide, ConcurSide, bstInner));
  end;

const
  BorderEnds: array[TdxPSCellCorner, TdxCellSide] of TdxPSCellBorderEnd =
   ((cbsTopLeft, cbsTopLeft, cbsTopLeft, cbsTopLeft),
    (cbsTopLeft, cbsBottomRight, cbsTopLeft, cbsTopLeft),
    (cbsBottomRight, cbsBottomRight, cbsBottomRight, cbsBottomRight),
    (cbsBottomRight, cbsTopLeft, cbsTopLeft, cbsTopLeft));
  CornerSides: array[TdxPSCellCorner] of TdxCellSides =
    ([csLeft, csTop],
     [csTop, csRight],
     [csRight, csBottom],
     [csBottom, csLeft]);
var
  Corner: TdxPSCellCorner;
  Borders: TdxPSCellBorders;
  UnfavorableSides: TdxCellSides;
  Side: TdxCellSide;
  BorderEnd: TdxPSCellBorderEnd;
begin
  for Corner := ccTopLeft to ccBottomLeft do
  begin
    GetBordersAtCorner(Corner, Borders);
    if DontNeedCornerSubsCalculation(Corner, Borders) then
      Continue;

    UnfavorableSides := CornerSides[Corner] - GetFavoriteSides(Corner, Borders);
    for Side := csLeft to csBottom do
      if Side in UnfavorableSides then
      begin
        BorderEnd := BorderEnds[Corner, Side];
        BorderSubs[Side, BorderEnd] := CalcBorderSub(Side, BorderEnd);
      end;
  end;
end;

procedure TdxReportCellSSString.CalcDoubleBorderSlants(AnAdapter: TdxPSGridCellsAdapter);
var
  Neighbor: TdxReportCellSSString;
begin
  if IsDoubleLineBorderPattern(csLeft) then
  begin
    Neighbor := AnAdapter.GetNeighborCell(Self, csLeft);
    BorderSlants[csLeft, cbcTopLeft] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csTop));
    BorderSlants[csLeft, cbcTopRight] := Ord(IsDoubleLineBorderPattern(csTop));
    BorderSlants[csLeft, cbcBottomRight] := Ord(IsDoubleLineBorderPattern(csBottom));
    BorderSlants[csLeft, cbcBottomLeft] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csBottom));
  end;

  if IsDoubleLineBorderPattern(csTop) then
  begin
    Neighbor := AnAdapter.GetNeighborCell(Self, csTop);
    BorderSlants[csTop, cbcTopLeft] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csLeft));
    BorderSlants[csTop, cbcTopRight] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csRight));
    BorderSlants[csTop, cbcBottomRight] := Ord(IsDoubleLineBorderPattern(csRight));
    BorderSlants[csTop, cbcBottomLeft] := Ord(IsDoubleLineBorderPattern(csLeft));
  end;

  if IsDoubleLineBorderPattern(csRight) then
  begin
    Neighbor := AnAdapter.GetNeighborCell(Self, csRight);
    BorderSlants[csRight, cbcTopLeft] := Ord(IsDoubleLineBorderPattern(csTop));
    BorderSlants[csRight, cbcTopRight] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csTop));
    BorderSlants[csRight, cbcBottomRight] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csBottom));
    BorderSlants[csRight, cbcBottomLeft] := Ord(IsDoubleLineBorderPattern(csBottom));
  end;

  if IsDoubleLineBorderPattern(csBottom) then
  begin
    Neighbor := AnAdapter.GetNeighborCell(Self, csBottom);
    BorderSlants[csBottom, cbcTopLeft] := Ord(IsDoubleLineBorderPattern(csLeft));
    BorderSlants[csBottom, cbcTopRight] := Ord(IsDoubleLineBorderPattern(csRight));
    BorderSlants[csBottom, cbcBottomRight] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csRight));
    BorderSlants[csBottom, cbcBottomLeft] := Ord((Neighbor <> nil) and Neighbor.IsDoubleLineBorderPattern(csLeft));
  end;
end;

function TdxReportCellSSString.GetBackgroundBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
var
  LineThickness: Integer;
begin
  Result := inherited GetBackgroundBounds(ACanvas);
  if not IsFixed then
  begin
    LineThickness := Self.LineThickness;
    if BorderEdgeSalients[csLeft, bstOuter] > 0 then
      Dec(Result.Left, LineThickness);
    if BorderEdgeSalients[csTop, bstOuter] > 0 then
      Dec(Result.Top, LineThickness);
    if BorderEdgeSalients[csRight, bstOuter] > 0 then
      Inc(Result.Right, LineThickness);
    if BorderEdgeSalients[csBottom, bstOuter] > 0 then
      Inc(Result.Bottom, LineThickness);
    ACanvas.FixupRect(Result);
  end;
end;

function TdxReportCellSSString.GetBorderEdgeBounds(ASide: TdxCellSide; const AOuterRect: TRect): TRect;
begin
  Result := inherited GetBorderEdgeBounds(ASide, AOuterRect);
  with Result do
    if ASide in csLeftRight then
    begin
      Inc(Top, LineThickness * BorderSubs[ASide, cbsTopLeft]);
      Dec(Bottom, LineThickness * BorderSubs[ASide, cbsBottomRight]);
    end
    else
    begin
      Inc(Left, LineThickness * BorderSubs[ASide, cbsTopLeft]);
      Dec(Right, LineThickness * BorderSubs[ASide, cbsBottomRight]);
    end;
end;

function TdxReportCellSSString.GetBorderBrush(ASide: TdxCellSide): TBrush;
var
  AItem: TdxPSEdgePatternItem;
begin
  AItem := dxPSEdgePatternFactory.Items[TdxPSEdgePatternClass(BorderEdgeClasses[ASide]), IsPrinting];
  Result := AItem.Brushes[dxCellEdgeSideOrientation[ASide]];
end;

function TdxReportCellSSString.GetBorderEdgeClass(ASide: TdxCellSide): TdxPSCellBorderClass;
begin
  Result := Borders[ASide].Pattern;
  if Result = nil then
    Result := TdxPSSolidEdgePattern;
end;

function TdxReportCellSSString.GetEffectiveBounds(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages): TRect;
begin
  if rsFirstPass in AStage then
    Result := inherited GetEffectiveBounds(ACanvas, AStage)
  else
    UnionRect(Result, GetOuterBounds(ACanvas), GetTextBounds(ACanvas));
end;

function TdxReportCellSSString.GetBorderPainterClass: TdxPSCellBorderPainterClass;
begin
  if IsFixed then
    Result := inherited GetBorderPainterClass
  else
    Result := TdxPSCellPatternsBorderPainter;
end;

procedure TdxReportCellSSString.InitBorderPainter(ABordersPainter: TdxPSCellBorderPainter);
begin
  inherited InitBorderPainter(ABordersPainter);
  if not IsFixed then
    TdxPSCellPatternsBorderPainter(ABordersPainter).FGridAdapter :=
      TdxSpreadSheetAbstractReportLink(ReportCells.ReportLink).FGridAdapter;
end;

procedure TdxReportCellSSString.ReadBorders(AReader: TdxPSDataReader);
var
  ABorder: TdxPSCellBorder;
  ASide: TdxCellSide;
begin
  for ASide := Low(TdxCellSide) to High(TdxCellSide) do
  begin
    ABorder.Color := AReader.ReadInteger;
    ABorder.Pattern := TdxPSEdgePatternClass(AReader.ReadCellBorderClass);
    Borders[ASide] := ABorder;
  end;
end;

procedure TdxReportCellSSString.ReadDataCore(AReader: TdxPSDataReader);
begin
  ReadBorders(AReader);
  FBorderSlants := AReader.ReadInteger;
  FBorderSubs := AReader.ReadInteger;
  RealCol := AReader.ReadInteger;
  RealRow := AReader.ReadInteger;
  inherited;
end;

procedure TdxReportCellSSString.WriteBorders(AWriter: TdxPSDataWriter);
var
  ASide: TdxCellSide;
begin
  for ASide := Low(TdxCellSide) to High(TdxCellSide) do
  begin
    AWriter.WriteInteger(Borders[ASide].Color);
    AWriter.WriteClassName(Borders[ASide].Pattern);
  end;
end;

procedure TdxReportCellSSString.WriteDataCore(AWriter: TdxPSDataWriter);
begin
  WriteBorders(AWriter);
  AWriter.WriteInteger(FBorderSlants);
  AWriter.WriteInteger(FBorderSubs);
  AWriter.WriteInteger(RealCol);
  AWriter.WriteInteger(RealRow);
  inherited;
end;

function TdxReportCellSSString.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxReportCellSSString._AddRef: Integer;
begin
  Result := -1;
end;

function TdxReportCellSSString._Release: Integer;
begin
  Result := -1;
end;

function TdxReportCellSSString.GetAbsoluteInnerBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := GetInnerBounds(ACanvas);
  if Parent <> nil then
    with Parent.AbsoluteOrigin do
      OffsetRect(Result, X, Y);
end;

function TdxReportCellSSString.GetDoubleBorderRgn(ASide: TdxCellSide; const R: TRect): HRGN;
const
  VertexCount = 4;
type
  PdxPSCellEdgeCoords = ^TdxPSCellEdgeCoords;
  TdxPSCellEdgeCoords = record
    case Byte of
      0: (TopLeft, TopRight, BottomRight, BottomLeft: TPoint);
      1: (Points: array[0..VertexCount - 1] of TPoint);
  end;
var
  EdgeCoords: TdxPSCellEdgeCoords;
  Thickness: Integer;
begin
  with EdgeCoords do
  begin
    TopLeft := R.TopLeft;
    TopRight := Point(R.Right, R.Top);
    BottomRight := R.BottomRight;
    BottomLeft := Point(R.Left, R.Bottom);
  end;

  Thickness := LineThickness * (TdxPSDoubleLineSolidEdgePattern.Thickness - 1);

  with EdgeCoords do
    case ASide of
      csLeft:
        begin
          Inc(TopLeft.Y, Thickness * BorderSlants[ASide, cbcTopLeft]);
          Inc(TopRight.Y, Thickness * BorderSlants[ASide, cbcTopRight]);
          Dec(BottomRight.Y, Thickness * BorderSlants[ASide, cbcBottomRight]);
          Dec(BottomLeft.Y, Thickness * BorderSlants[ASide, cbcBottomLeft]);
        end;
      csTop:
        begin
          Inc(TopLeft.X, Thickness * BorderSlants[ASide, cbcTopLeft]);
          Dec(TopRight.X, Thickness * BorderSlants[ASide, cbcTopRight]);
          Dec(BottomRight.X, Thickness * BorderSlants[ASide, cbcBottomRight]);
          Inc(BottomLeft.X, Thickness * BorderSlants[ASide, cbcBottomLeft]);
        end;
      csRight:
        begin
          Inc(TopLeft.Y, Thickness * BorderSlants[ASide, cbcTopLeft]);
          Inc(TopRight.Y, Thickness * BorderSlants[ASide, cbcTopRight]);
          Dec(BottomRight.Y, Thickness * BorderSlants[ASide, cbcBottomRight]);
          Dec(BottomLeft.Y, Thickness * BorderSlants[ASide, cbcBottomLeft]);
        end;
      csBottom:
        begin
          Inc(TopLeft.X, Thickness * BorderSlants[ASide, cbcTopLeft]);
          Dec(TopRight.X, Thickness * BorderSlants[ASide, cbcTopRight]);
          Dec(BottomRight.X, Thickness * BorderSlants[ASide, cbcBottomRight]);
          Inc(BottomLeft.X, Thickness * BorderSlants[ASide, cbcBottomLeft]);
        end;
    end;

  Result := CreatePolygonRgn(EdgeCoords.Points, SizeOf(TdxPSCellEdgeCoords) div SizeOf(TPoint), Windows.WINDING);
end;

function TdxReportCellSSString.IsDoubleLineBorderPattern(ABorder: TdxPSCellBorder): Boolean;
begin
  with ABorder do
    Result := (Pattern <> nil) and Pattern.InheritsFrom(TdxPSDoubleLineSolidEdgePattern);
end;

function TdxReportCellSSString.IsDoubleLineBorderPattern(ASide: TdxCellSide): Boolean;
begin
  Result := (ASide in CellSides) and IsDoubleLineBorderPattern(Self.Borders[ASide]);
end;

function TdxReportCellSSString.NullBorder: TdxPSCellBorder;
begin
  FillChar(Result, SizeOf(TdxPSCellBorder), 0);
end;

function TdxReportCellSSString.GetBorder(ASide: TdxCellSide): TdxPSCellBorder;
begin
  Result := FBorders[ASide];
end;

function TdxReportCellSSString.GetBordersBkColor: TColor;
begin
  if Transparent then
    Result := ColorToRGB(clWindow)
  else
    Result := Color;
end;

function TdxReportCellSSString.GetBorderSlant(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner): Integer;
begin
  Result := (FBorderSlants shr GetBorderSlantOffset(ASide, ACorner)) and SlantMask;
end;

function TdxReportCellSSString.GetBorderSlantOffset(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner): TDWORDBits;
begin
  Result := Integer(ASide) * SlantBitsPerSide + Integer(ACorner);
end;

function TdxReportCellSSString.GetBorderSub(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): TdxPSCellBorderSub;
begin
  Result := (FBorderSubs shr GetBorderSubOffset(ASide, AEnd)) and SubEndMask;
end;

function TdxReportCellSSString.GetBorderSubMask(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): DWORD;
begin
  Result := 0 or (SubEndMask shl GetBorderSubOffset(ASide, AEnd));
end;

function TdxReportCellSSString.GetBorderSubOffset(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd): TDWORDBits;
begin
  Result := Integer(ASide) * SubBitsPerSide + SubBitsPerEnd * Integer(AEnd);
end;

function TdxReportCellSSString.GetClipContent: Boolean;
begin
  Result := (Format and dxFormatClipContent) = dxFormatClipContent;
end;

function TdxReportCellSSString.GetCol: Integer;
begin
  Result := Index;
end;

function TdxReportCellSSString.GetFill: Boolean;
begin
  Result := (Format and dxFormatFill) = dxFormatFill;
end;

function TdxReportCellSSString.GetIsFixed: Boolean;
begin
  Result := (Format and dxFormatFixed) = dxFormatFixed;
end;

function TdxReportCellSSString.GetIsMerged: Boolean;
begin
  Result := (Format and dxFormatMerged) = dxFormatMerged;
end;

function TdxReportCellSSString.GetIsNearMostLeft: Boolean;
begin
  Result := (Format and dxFormatNearMostLeft) = dxFormatNearMostLeft;
end;

function TdxReportCellSSString.GetIsNearMostTop: Boolean;
begin
  Result := (Format and dxFormatNearMostTop) = dxFormatNearMostTop;
end;

function TdxReportCellSSString.GetIsNearMostTopOrLeft: Boolean;
begin
  Result := GetIsNearMostLeft or GetIsNearMostTop;
end;

function TdxReportCellSSString.GetIsRTF: Boolean;
begin
  Result := GetFormatBit(dxFormatRTF);
end;

function TdxReportCellSSString.GetIsVirtual: Boolean;
begin
  Result := GetFormatBit(dxFormatVirtual);
end;

function TdxReportCellSSString.GetRow: Integer;
begin
  Result := Parent.Index;
end;

procedure TdxReportCellSSString.SetBorder(ASide: TdxCellSide; Value: TdxPSCellBorder);
begin
  FBorders[ASide] := Value;
end;

procedure TdxReportCellSSString.SetBorderSlant(ASide: TdxCellSide; ACorner: TdxPSCellBorderCorner; Value: Integer);
var
  Mask: DWORD;
begin
  Mask := 1 shl GetBorderSlantOffset(ASide, ACorner);
  FBorderSlants := FBorderSlants and not Mask;
  if Value <> 0 then
    FBorderSlants := FBorderSlants or Mask;
end;

procedure TdxReportCellSSString.SetBorderSub(ASide: TdxCellSide; AEnd: TdxPSCellBorderEnd;
  Value: TdxPSCellBorderSub);
begin
  FBorderSubs := FBorderSubs and not GetBorderSubMask(ASide, AEnd) or (Value shl GetBorderSubOffset(ASide, AEnd));
end;

procedure TdxReportCellSSString.SetClipConent(Value: Boolean);
const
  dxClipContent: array[Boolean] of DWORD = (0, dxFormatClipContent);
begin
  Format := Format and not dxFormatClipContent or dxClipContent[Value];
end;

procedure TdxReportCellSSString.SetFill(Value: Boolean);
const
  dxFill: array[Boolean] of DWORD = (0, dxFormatFill);
begin
  Format := Format and not dxFormatFill or dxFill[Value];
end;

procedure TdxReportCellSSString.SetIsFixed(Value: Boolean);
const
  dxFixed: array[Boolean] of DWORD = (0, dxFormatFixed);
begin
  Format := Format and not dxFormatFixed or dxFixed[Value];
end;

procedure TdxReportCellSSString.SetIsMerged(Value: Boolean);
const
  dxMerged: array[Boolean] of DWORD = (0, dxFormatMerged);
begin
  Format := Format and not dxFormatMerged or dxMerged[Value];
end;

procedure TdxReportCellSSString.SetIsNearMostLeft(Value: Boolean);
const
  dxIsNearMostLeft: array[Boolean] of DWORD = (0, dxFormatNearMostLeft);
begin
  Format := Format and not dxFormatNearMostLeft or dxIsNearMostLeft[Value];
end;

procedure TdxReportCellSSString.SetIsNearMostTop(Value: Boolean);
const
  dxIsNearMostTop: array[Boolean] of DWORD = (0, dxFormatNearMostTop);
begin
  Format := Format and not dxFormatNearMostTop or dxIsNearMostTop[Value];
end;

procedure TdxReportCellSSString.SetIsRTF(Value: Boolean);
begin
  SetFormatBit(dxFormatRTF, Value);
end;

procedure TdxReportCellSSString.SetIsVirtual(Value: Boolean);
begin
  SetFormatBit(dxFormatVirtual, Value);
end;

function TdxReportCellSSString.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TdxReportCellSSString.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.Renderer.Canvas;
end;

function TdxReportCellSSString.GetDisplayGraphicsAsText: Boolean;
begin
  Result := False;
end;

function TdxReportCellSSString.GetDisplayTrackBarsAsText: Boolean;
begin
  Result := False;
end;

function TdxReportCellSSString.GetEndEllipsis: Boolean;
begin
  Result := EndEllipsis;
end;

function TdxReportCellSSString.GetFlatCheckMarks: Boolean;
begin
  Result := False;
end;

function TdxReportCellSSString.GetGraphicsText: string;
begin
  Result := '';
end;

function TdxReportCellSSString.GetMultiline: Boolean;
begin
  Result := Multiline;
end;

function TdxReportCellSSString.GetTransparentGraphics: Boolean;
begin
  Result := Transparent;
end;

{ TdxSpreadSheetAbstractReportLink }

constructor TdxSpreadSheetAbstractReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FColCount := -1;
  FRowCount := -1;
  FGridAdapter := TdxPSGridCellsAdapter.Create(nil);
  FCellObjects := TList.Create;
  FTempFont := TFont.Create;
end;

destructor TdxSpreadSheetAbstractReportLink.Destroy;
begin
  FreeAndNil(FGridAdapter);
  DeleteCellObjects;
  FreeAndNil(FCellObjects);
  FreeAndNil(FTempFont);
  inherited;
end;

procedure TdxSpreadSheetAbstractReportLink.DeleteCellObjects;
begin
  FCellObjects.Clear;
  FreeAndNil(FCachedDisplayCellStyle);
end;

function TdxSpreadSheetAbstractReportLink.CannotActivateReportErrorString: string;
begin
  if inherited DataProviderPresent then
    Result := cxGetResourceString(@sdxDataToPrintDoesNotExist)
  else
    Result := inherited CannotActivateReportErrorString;
end;

procedure TdxSpreadSheetAbstractReportLink.AfterConstruct(AReportCells: TdxReportCells);
begin
  FColCount := -1;
  FRowCount := -1;
  FHostContainers := nil;
  FHostMergedCells := nil;
  FGridAdapter.FReportCells := nil;
  DeleteCellObjects;
  FreeAndNil(FFontIndexes);
  inherited AfterConstruct(AReportCells);
end;

procedure TdxSpreadSheetAbstractReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  FExtraColumnCount := 0;
  FCellObjects.Count := ColCount * RowCount - 1;
  FGridAdapter.FReportCells := AReportCells;
  FSourceHeaderFontIndex := AddFontToPool(SpreadSheet.Styles.GetHeaderStyle(Sheet).Font);
  FCachedDisplayCellStyle := TdxPSSpreadSheetCellDisplayStyleCache.Create(Sheet);
  FFontIndexes := TDictionary<TObject, Integer>.Create;
  inherited PrepareConstruct(AReportCells);
end;

procedure TdxSpreadSheetAbstractReportLink.ConstructReportContent(AReportCells: TdxReportCells);
begin
  inherited ConstructReportContent(AReportCells);
  if not AbortBuilding then
  begin
    if HasMergedCells then
      AddMergedCells(AReportCells);
    CalcTextExtents(FGridAdapter);
    if not HasSelection then
      AppendAdditionalColumns(FGridAdapter);
    DeleteUnneededCellSides(FGridAdapter);
    FixupRowWidths(FGridAdapter);
    PostProcessItems(FGridAdapter);
    AddContainers(AReportCells);
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetCellText(ACol, ARow: Integer): string;
var
  ACell: TdxSpreadSheetCell;
  ACellStyle: TdxSpreadSheetCellStyle;
begin
  Result := '';
  if IsFixedCell(ACol, ARow) then
  begin
    if (ACol = 0) and (ARow <> 0) then
      Result := IntToStr(ARow);
    if (ARow = 0) and (ACol <> 0) then
      Result := TdxSpreadSheetColumnHelper.NameByIndex(ACol - 1, SpreadSheet.OptionsView.R1C1Reference);
  end
  else
    if not FAppendingExtraColumns and (FProcessingMerges or not IsMergedCell(ACol, ARow)) then
    begin
      ACell := GetSSCellObject(ARow, ACol);
      if ACell <> nil then
      begin
        ACellStyle := GetSSCellStyle(ARow, ACol);
        if not (ACellStyle is TdxSpreadSheetTableViewCellDisplayStyle) or TdxSpreadSheetTableViewCellDisplayStyle(ACellStyle).ShowCellValue then
          if not TdxSpreadSheetCellAccess(ACell).IsValueFormattedString or not dxSpreadSheetTextService.GetAsRTF(ACell, Result) then
            Result := GetCellTextCore(ACell);
      end;
    end
end;

function TdxSpreadSheetAbstractReportLink.GetCellTextCore(ACell: TdxSpreadSheetCell): string;
begin
  Result := ACell.DisplayText;
end;

function TdxSpreadSheetAbstractReportLink.GetColumnOffset(ACol: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ACol - 1 do
    Inc(Result, ColumnWidths[I]);
end;

function TdxSpreadSheetAbstractReportLink.GetRowOffset(ARow: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ARow - 1 do
    Inc(Result, RowHeights[I]);
end;

function TdxSpreadSheetAbstractReportLink.GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass;
begin
  Result := TdxReportCellSSString;
end;

function TdxSpreadSheetAbstractReportLink.GetDefaultCellBorderColor: TColor;
begin
  Result := GridLineColor;
end;

function TdxSpreadSheetAbstractReportLink.GetCellSides(ACol, ARow: Integer): TdxCellSides;

  function AreNeigborhoodsTransparent(ASide: TdxCellSide): Boolean;
  begin
    case ASide of
      csLeft:
        Result := IsNearMostLeftCell(ACol, ARow) or (GetCellTransparent(ACol - 1, ARow) and GetCellTransparent(ACol, ARow));
      csTop:
        Result := IsNearMostTopCell(ACol, ARow) or (GetCellTransparent(ACol, ARow - 1) and GetCellTransparent(ACol, ARow));
      csRight:
        Result := (ACol = GetColCount - 1) or (GetCellTransparent(ACol + 1, ARow) and GetCellTransparent(ACol, ARow));
    else // csBottom
      Result := (ARow = GetRowCount - 1) or (GetCellTransparent(ACol, ARow + 1) and GetCellTransparent(ACol, ARow));
    end;
  end;

var
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
  ASide: TdxCellSide;
  AStyle: TdxSpreadSheetCellStyle;
begin
  if IsFixedCell(ACol, ARow) or IsSuppressSourceFormats then
    Result := inherited GetCellSides(ACol, ARow)
  else
  begin
    Result := csAll;
    AStyle := GetSSCellStyle(ARow, ACol);

    for ASide := Low(TdxCellSide) to High(TdxCellSide) do
    begin
      if AStyle <> nil then
        ABorderStyle := AStyle.Borders[TcxBorder(ASide)].Style
      else
        ABorderStyle := sscbsDefault;

      if (ABorderStyle in [sscbsNone, sscbsDefault]) and not (IsShowGridLines and AreNeigborhoodsTransparent(ASide)) or
        (IsMergedCell(ACol, ARow) and not IsMergedBorder(ACol, ARow, ASide))
      then
        Exclude(Result, ASide);
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  if (DrawMode <> gdmBorrowSource) and not IsFixedCell(ACol, ARow) then
    Result := taBottom
  else
    Result := inherited GetCellTextAlignY(ACol, ARow);
end;

function TdxSpreadSheetAbstractReportLink.GetMinRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont): Integer;
begin
  Result := 1;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellColor(ACol, ARow: Integer): TColor;
begin
  if IsFixedCell(ACol, ARow) then
    Result := ColorToRGB(SpreadSheet.Styles.GetHeaderStyle(Sheet).Color)
  else
    if GetSourceCellContentPattern(ACol, ARow).InheritsFrom(TdxPSSolidFillPattern) then
      Result := GetRealColor(GetSSCellStyle(ARow, ACol).Brush.BackgroundColor, clWindow)
    else
      Result := GetRealColor(GetSSCellStyle(ARow, ACol).Brush.ForegroundColor, clWindow);
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellContentBkColor(ACol, ARow: Integer): TColor;
begin
  if IsFixedCell(ACol, ARow) then
    Result := ColorToRGB(SpreadSheet.Styles.GetHeaderStyle(Sheet).Color)
  else
    Result := GetRealColor(GetSSCellStyle(ARow, ACol).Brush.BackgroundColor, clWindow);
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellContentPattern(ACol, ARow: Integer): TdxPSFillPatternClass;
begin
  if IsFixedCell(ACol, ARow) then
    Result := TdxPSSolidFillPattern
  else
    Result := dxPSFillPatternClassMap[GetSSCellStyle(ARow, ACol).Brush.Style];
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellEdge3DSoft(ACol, ARow: Integer): Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellEdgeMode(ACol, ARow: Integer): TdxCellEdgeMode;
begin
  Result := cemPattern;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellFont(ACol, ARow: Integer): TFont;
begin
  if IsFixedCell(ACol, ARow) then
    Result := inherited GetSourceCellFont(ACol, ARow)
  else
    Result := TdxSpreadSheetCellFontAccess(GetSSCellStyle(ARow, ACol).Font).Handle.GraphicObject;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellFontIndex(ACol, ARow: Integer): Integer;
var
  AFont: TFont;
begin
  if IsFixedCell(ACol, ARow) then
    Result := FSourceHeaderFontIndex
  else
  begin
    AFont := GetSourceCellFont(ACol, ARow);
    if not FFontIndexes.TryGetValue(AFont, Result) then
    begin
      FTempFont.PixelsPerInch := PixelsPerInch;
      FTempFont.Assign(AFont);
      if not dxIsTrueTypeFont(FTempFont) then
        FTempFont.Name := Font.Name;
      if IsSuppressFontColors then
        FTempFont.Color := clWindowText;
      if FTempFont.Color = clDefault then
        FTempFont.Color := Font.Color;
      Result := AddFontToPool(FTempFont);
      FFontIndexes.Add(AFont, Result);
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellMultiline(ACol, ARow: Integer): Boolean;
begin
  Result := not IsFixedCell(ACol, ARow) and GetSSCellStyle(ARow, ACol).WordWrap;
end;

function TdxSpreadSheetAbstractReportLink.GetFlatIndex(ACol, ARow: Integer): Integer;
begin
  Result := ARow * (ColCount - 1) + ACol;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellTextAlignX(ACol, ARow: Integer): TcxTextAlignX;
var
  AAlign: TdxSpreadSheetDataAlignHorz;
  ACell: TdxSpreadSheetCell;
begin
  if IsFixedCell(ACol, ARow) then
    Result := taCenterX
  else
  begin
    AAlign := GetSSCellStyle(ARow, ACol).AlignHorz;
    if AAlign = ssahGeneral then
    begin
      ACell := GetSSCellObject(ARow, ACol);
      if (ACell <> nil) and ACell.IsNumericValue then
        AAlign := ssahRight;
    end;
    Result := dxPSTextAlignXMap[AAlign];
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellTextAlignY(ACol, ARow: Integer): TcxTextAlignY;
begin
  if IsFixedCell(ACol, ARow) then
    Result := taBottom
  else
    Result := dxPSTextAlignYMap[GetSSCellStyle(ARow, ACol).AlignVert];
end;

function TdxSpreadSheetAbstractReportLink.GetSourceCellTransparent(ACol, ARow: Integer): Boolean;
begin
  if IsFixedCell(ACol, ARow) then
    Result := FixedTransparent
  else
    Result := inherited GetSourceCellTransparent(ACol, ARow) and
      (GetSSCellStyle(ARow, ACol).Brush.Style = sscfsSolid) and
      (GetSourceCellColor(ACol, ARow) = ColorToRGB(clWindow));
end;

function TdxSpreadSheetAbstractReportLink.GetSourceColWidth(ACol: Integer): Integer;
begin
  if IsFixedCol(ACol) then
    Result := 50
  else
    Result := TdxSpreadSheetTableItemsAccess(Sheet.Columns).GetItemSize(ACol - 1);

  Result := ScaleFactor.Apply(Result);
end;

function TdxSpreadSheetAbstractReportLink.GetSourceRowHeight(ARow: Integer): Integer;
begin
  if IsFixedRow(ARow) then
    Result := 22
  else
    Result := TdxSpreadSheetTableItemsAccess(Sheet.Rows).GetItemSize(ARow - 1);

  Result := ScaleFactor.Apply(Result);
end;

function TdxSpreadSheetAbstractReportLink.GetSSCellObject(ARow, ACol: Integer): TdxSpreadSheetCell;
var
  Index: Integer;
begin
  Index := GetFlatIndex(ACol - 1, ARow - 1);
  if Index < 0 then
    Exit(nil);
  if Index > FCellObjects.Count - 1 then // ExtraColumns
    Result := nil
  else
    Result := FCellObjects.List[Index];

  if Result = nil then
  begin
    Result := Sheet.Cells[ARow - 1, ACol - 1];
    if Index > FCellObjects.Count - 1 then
      FCellObjects.Count := Index + 1;
    FCellObjects.List[Index] := Result;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetSSCellStyle(ARow, ACol: Integer): TdxSpreadSheetCellStyle;
var
  ACell: TdxSpreadSheetCell;
begin
  Result := GetSSDefaultStyle;
  if not FAppendingExtraColumns then
  begin
    Dec(ACol);
    Dec(ARow);
    if (Sheet.ConditionalFormatting.RuleCount > 0) and not IsSuppressSourceFormats then
    begin
      if (FCachedDisplayCellStyle.Row <> ARow) or (FCachedDisplayCellStyle.Column <> ACol) then
      begin
        ACell := GetSSCellObject(ARow + 1, ACol + 1);
        FCachedDisplayCellStyle.Reset;
        FCachedDisplayCellStyle.Assign(TdxSpreadSheetTableViewAccess(Sheet).GetCellStyle(ARow, ACol, ACell));
        FCachedDisplayCellStyle.Column := ACol;
        FCachedDisplayCellStyle.Row := ARow;
        Sheet.ConditionalFormatting.CalculateStyle(FCachedDisplayCellStyle, ARow, ACol, ACell);
      end;
      Result := FCachedDisplayCellStyle;
    end
    else
    begin
      ACell := GetSSCellObject(ARow + 1, ACol + 1);
      if ACell <> nil then
        Result := ACell.Style;
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetSSDefaultStyle: TdxSpreadSheetCellStyle;
begin
  Result := SpreadSheet.DefaultCellStyle;
end;

function TdxSpreadSheetAbstractReportLink.GetColCount: Integer;
var
  ABeginCol, ABeginRow, AEndRow: Integer;
begin
  if FColCount <= 0 then
  begin
    if HasSelection then
    begin
      GetSelectedRange(ABeginCol, FColCount, ABeginRow, AEndRow);
      Inc(FColCount);
    end
    else
      FColCount := MeaningColCount + 1;
  end;
  Result := FColCount;
end;

function TdxSpreadSheetAbstractReportLink.GetFixedColCount: Integer;
begin
  Result := 1;
end;

function TdxSpreadSheetAbstractReportLink.GetFixedRowCount: Integer;
begin
  Result := 1;
end;

function TdxSpreadSheetAbstractReportLink.GetRowCount: Integer;
var
  BeginCol, EndCol, BeginRow: Integer;
begin
  if FRowCount <= 0 then
    if HasSelection then
    begin
      GetSelectedRange(BeginCol, EndCol, BeginRow, FRowCount);
      Inc(FRowCount);
    end
    else
      FRowCount := MeaningRowCount + 1;

  Result := FRowCount;
end;

function TdxSpreadSheetAbstractReportLink.IsCellContainsRTFText(ACol, ARow: Integer): Boolean;
var
  ARTF: string;
  ACell: TdxSpreadSheetCellAccess;
begin
  Result := (ARow > 0) and (ACol > 0) and dxSpreadSheetTextService.IsRTFSupported;
  if Result then
  begin
    ACell := TdxSpreadSheetCellAccess(GetSSCellObject(ARow, ACol));
    Result := (ACell <> nil) and ACell.IsValueFormattedString and dxSpreadSheetTextService.GetAsRTF(ACell, ARTF);
  end;
end;

function TdxSpreadSheetAbstractReportLink.IsDrawBorder: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetAbstractReportLink.IsDrawHorzLines: Boolean;
begin
  Result := IsShowGridLines
end;

function TdxSpreadSheetAbstractReportLink.IsDrawVertLines: Boolean;
begin
  Result := IsShowGridLines;
end;

function TdxSpreadSheetAbstractReportLink.IsEmptyCell(const ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := IsCellDataEmpty(ACell) and ((ACell = nil) or AreStylesEqual(SpreadSheet.DefaultCellStyle, ACell.Style));
end;

function TdxSpreadSheetAbstractReportLink.IsEmptyRow(ARow: Integer): Boolean;
var
  APrevProcessingMerges: Boolean;
begin
  APrevProcessingMerges := FProcessingMerges;
  FProcessingMerges := True;
  try
    Result := inherited IsEmptyRow(ARow);
  finally
    FProcessingMerges := APrevProcessingMerges;
  end;
end;

procedure TdxSpreadSheetAbstractReportLink.AddMergedCells(AReportCells: TdxReportCells);

  function IntersectRect(R1, R2: TRect): Boolean;
  begin
    Inc(R1.Right);
    Inc(R1.Bottom);
    Inc(R2.Right);
    Inc(R2.Bottom);
    Result := Windows.IntersectRect(R1, R1, R2);
  end;

var
  SelRect, R: TRect;
  LeftOffset, TopOffset, I, ACol, ARow: Integer;
  Cell: TdxReportCell;
  DataClass: TdxReportCellDataClass;
  DataItem: TAbstractdxReportCellData;
  AReportCell: TdxReportCellSSString;
  ARTFItem: TAbstractdxReportCellData;
  AViewParams: TdxReportItemViewParams;
begin
  FProcessingMerges := True;
  try
    SelRect := Rect(0, 0, 0, 0);
    LeftOffset := 0;
    TopOffset := 0;

    if HasSelection then
    begin
      SelRect := SelectionRect;
      LeftOffset := GetColumnOffset(SelRect.Left);
      TopOffset := GetRowOffset(SelRect.Top);
      if IsShowRowAndColumnHeadings then
      begin
        Dec(LeftOffset, ColumnWidths[0]);
        if not HeadersOnEveryPage then
          Dec(TopOffset, RowHeights[0]);
      end;
    end
    else
      if not IsShowRowAndColumnHeadings then
      begin
        Inc(LeftOffset, ColumnWidths[0]);
        Inc(TopOffset, RowHeights[0]);
      end
      else
        if HeadersOnEveryPage then
          Inc(TopOffset, RowHeights[0]);

    FHostMergedCells := AReportCells.AddOverlay;

    for I := 0 to MergedCells.Count - 1 do
    begin
      R := SpreadSheetAreaToReportArea(MergedCells[I].Area);

      if HasSelection and not IntersectRect(R, SelRect) then
        Continue;

      ACol := R.Left;
      ARow := R.Top;

      R.Left := GetColumnOffset(R.Left);
      R.Top := GetRowOffset(R.Top);
      R.Right := GetColumnOffset(R.Right + 1);
      R.Bottom := GetRowOffset(R.Bottom + 1);

      OffsetRect(R, -LeftOffset, -TopOffset);

      Cell := TdxReportCell.Create(FHostMergedCells);
      Cell.BoundsRect := R;
      Cell.Transparent := True;
      Cell.CellSides := [];

      DataClass := GetDataItemClass(ACol);
      if DataClass <> nil then
      begin
        DataItem := DataClass.Create(Cell);
        OffsetRect(R, -R.Left, -R.Top);
        DataItem.BoundsRect := R;
        AssignData(ACol, ARow, DataItem);
        if TdxReportCellSSString(DataItem).IsRTF then
        begin
          DataClass := dxPSDataMaps.ItemClass(EditProperties);
          if (DataClass <> TdxReportCellString) and (DataClass <> nil) then
          begin
            AReportCell := TdxReportCellSSString(DataItem);
            AViewParams := GetItemViewParams(AReportCell);
            ARTFItem := DataClass.Create(DataItem.Parent);
            ARTFItem.Assign(DataItem);
            dxPSDataMaps.InitializeItem(ARTFItem, EditProperties, AReportCell.Text, AReportCell, AViewParams);
            AReportCell.Text := '';
            AReportCell.ExcludeFromClipRgn := False;
            AReportCell.Transparent := True;
          end;
        end;
        DataItem.CellSides := [];
      end;
    end;
  finally
    FProcessingMerges := False;
  end;
end;

procedure TdxSpreadSheetAbstractReportLink.AppendAdditionalColumns(AAdapter: TdxPSGridCellsAdapter);

  function DoCalcExtraColumnCount(AAdapter: TdxPSGridCellsAdapter; ADefaultColumnWidth: Integer): Integer;
  var
    Col, Row, Index, CurrentColumnOffset: Integer;
    Item: TdxReportCellSSString;
  begin
    Result := AAdapter.ColCount;

    for Row := 0 to AAdapter.RowCount - 1 do
      for Col := 0 to AAdapter.ColCount - 1 do
      begin
        Item := AAdapter[Col, Row];
        if Item.TextExtentRight <> 0 then
        begin
          Index := Col;
          CurrentColumnOffset := AAdapter.ColOffsets[Index];
          while Item.TextExtentRight > CurrentColumnOffset do
          begin
            Inc(Index);
            if Index <= AAdapter.ColCount then
              CurrentColumnOffset := AAdapter.ColOffsets[Index]
            else
              Inc(CurrentColumnOffset, ADefaultColumnWidth);
          end;
          if Index > Result then
            Result := Index;
        end;
      end;
    Dec(Result, AAdapter.ColCount);
  end;

  function FindNearestVisibleColIndex(AStartIndex: Integer): Integer;
  var
    AColumn: TdxSpreadSheetTableColumn;
  begin
    Result := AStartIndex;
    repeat
      AColumn := Sheet.Columns[Result - 1];
      if (AColumn = nil) or AColumn.Visible then Break;
      Inc(Result);
    until False;
  end;

  procedure DoAppendExtraColumns(AAdapter: TdxPSGridCellsAdapter;  AExtraColumnCount, ADefaultColumnWidth: Integer);
  var
    L, Row, Col, StartRealCol, RealCol: Integer;
    Item: TdxReportCellSSString;
    Cell: TdxReportCell;
    R: TRect;
  begin
    L := AAdapter.ColOffsets[AAdapter.ColCount];
    StartRealCol := AAdapter.Cells[AAdapter.ColCount - 1, 0].RealCol;

    for Row := 0 to AAdapter.RowCount - 1 do
    begin
      Item := nil;
      Cell := AAdapter.Rows[Row];
      R := Bounds(L, 0, 0, Cell.Height);
      RealCol := StartRealCol;
      for Col := 0 to AExtraColumnCount - 1 do
      begin
        R.Left := R.Right;
        R.Right := R.Left + ADefaultColumnWidth;

        Item := TdxReportCellSSString.Create(Cell);
        Item.BoundsRect := R;
        Item.IsVirtual := True;

        RealCol := FindNearestVisibleColIndex(RealCol + 1);
        AssignData(RealCol, AAdapter.RowIndexes[Row], Item);
        if Item.IsFixed then
          Item.Text := TdxSpreadSheetColumnHelper.NameByIndex(RealCol - 1, SpreadSheet.OptionsView.R1C1Reference);
      end;
      Cell.Width := Item.BoundsRect.Right;
    end;
  end;

var
  ADefaultColumnWidth: Integer;
begin
  FAppendingExtraColumns := True;
  try
    ADefaultColumnWidth := ScaleFactor.Apply(Sheet.Columns.DefaultSize);
    FExtraColumnCount := DoCalcExtraColumnCount(AAdapter, ADefaultColumnWidth);
    if ExtraColumnCount <> 0 then
      DoAppendExtraColumns(AAdapter, ExtraColumnCount, ADefaultColumnWidth);
  finally
    FAppendingExtraColumns := False;
  end;
end;

procedure TdxSpreadSheetAbstractReportLink.CalcTextExtents(AAdapter: TdxPSGridCellsAdapter);

  function GetTextWidth(ACanvas: TdxPSReportRenderCustomCanvas; AFont: TFont; const S: string): Integer;
  begin
    ACanvas.SaveState;
    try
      ACanvas.Font := AFont;
      Result := ACanvas.TextSize(S).cx;
    finally
      ACanvas.RestoreState;
    end;
  end;

  function NeedCalcItemTextExtents(AItem: TdxReportCellSSString): Boolean;
  begin
    Result := not (AItem.IsMerged or AItem.IsFixed or AItem.Multiline or AItem.Fill or AItem.IsRTF) and
      (AItem.Text <> '') and (AItem.TextAlignX in [taLeft, taCenterX, taRight]);
  end;

  procedure DoCalcItemTextExtents(AItem: TdxReportCellSSString; Col, Row: Integer);

    function CalcTextRightExtent(ATextWidth, ACol, ARow: Integer): Integer;
    var
      ACurrentColumnOffset: Integer;
      AEndOfTextOffset: Integer;
      AItem: TdxReportCellSSString;
      AOriginalColumnOffset: Integer;
      I: Integer;
    begin
      Result := 0;
      AOriginalColumnOffset := AAdapter.ColOffsets[ACol];
      AEndOfTextOffset := AOriginalColumnOffset + ATextWidth;
      ACurrentColumnOffset := AAdapter.ColOffsets[ACol + 1];

      for I := ACol + 1 to AAdapter.ColCount do
      begin
        ACurrentColumnOffset := AAdapter.ColOffsets[I];
        if ACurrentColumnOffset > AEndOfTextOffset then
          Break;

        if I < AAdapter.ColCount then
          AItem := AAdapter[I, ARow]
        else
          AItem := nil;

        if (AItem <> nil) and (AItem.IsMerged or (AItem.Text <> '')) then
        begin
          Result := ACurrentColumnOffset;
          Break;
        end
      end;

      if Result = 0 then
      begin
        Result := AEndOfTextOffset;
        if (Result > ACurrentColumnOffset) and HasSelection then
          Result := ACurrentColumnOffset + dxTextSpace;
        if Result < AAdapter.ColOffsets[ACol + 1] then
          Result := 0;
      end;
    end;

    function CalcTextLeftExtent(ATextWidth, ACol, ARow: Integer): Integer;
    var
      TextLeftEdge, I, CurrentColumnOffset: Integer;
      Item: TdxReportCellSSString;
    begin
      Result := 0;
      TextLeftEdge := Max(0, AAdapter.ColOffsets[ACol + 1] - ATextWidth);

      for I := ACol downto 0 do
      begin
        if I > 0 then
          Item := AAdapter[I - 1, ARow]
        else
          Item := nil;

        CurrentColumnOffset := AAdapter.ColOffsets[I];
        if CurrentColumnOffset < TextLeftEdge then
          Break;
        if (Item = nil) or Item.IsFixed or Item.IsMerged or (Item.Text <> '') then
        begin
          Result := CurrentColumnOffset;
          Break;
        end
      end;

      if Result = 0 then
      begin
        Result := TextLeftEdge;
        if Result > AAdapter.ColOffsets[ACol] then
          Result := 0;
      end;
    end;

  var
    ATextWidth: Integer;
  begin
    ATextWidth := GetTextWidth(ScreenCanvas, AItem.Font, AItem.Text);
    case AItem.TextAlignX of
      taLeft:
        AItem.TextExtentRight := CalcTextRightExtent(AItem.TextAlignHorzIndent + ATextWidth + 3 * dxTextSpace, Col, Row);
      taRight:
        AItem.TextExtentLeft := CalcTextLeftExtent(AItem.TextAlignHorzIndent + ATextWidth + 3 * dxTextSpace, Col, Row);
      taCenterX:
        begin
          Dec(ATextWidth, (ATextWidth - AItem.Width) div 2);
          AItem.TextExtentRight := CalcTextRightExtent(ATextWidth + dxTextSpace, Col, Row);
          AItem.TextExtentLeft := CalcTextLeftExtent(ATextWidth + dxTextSpace, Col, Row);
        end;
    end;
  end;

var
  ACol: Integer;
  AItem: TdxReportCellSSString;
  ARow: Integer;
begin
  ScreenCanvas.SaveState;
  try
    for ACol := 0 to AAdapter.ColCount - 1 do
      for ARow := 0 to AAdapter.RowCount - 1 do
      begin
        AItem := AAdapter[ACol, ARow];
        if NeedCalcItemTextExtents(AItem) then
          DoCalcItemTextExtents(AItem, ACol, ARow);
      end;
  finally
    ScreenCanvas.RestoreState;
  end;
end;

procedure TdxSpreadSheetAbstractReportLink.FixupRowWidths(AAdapter: TdxPSGridCellsAdapter);
var
  AMaxWidth: Integer;
  I: Integer;
begin
  AMaxWidth := 0;
  for I := 0 to AAdapter.ColCount - 1 do
    Inc(AMaxWidth, AAdapter.ColWidths[I]);
  for I := 0 to AAdapter.RowCount - 1 do
    AAdapter.Rows[I].Width := AMaxWidth;
end;

procedure TdxSpreadSheetAbstractReportLink.DeleteUnneededCellSides(AAdapter: TdxPSGridCellsAdapter);

  procedure DoDeleteUnneededCellSidesFromRightSide(AItem: TdxReportCellSSString; ACol: Integer);
  var
    TextRightExtent, Col, CurrentColumnOffset: Integer;
  begin
    TextRightExtent := AItem.TextExtentRight;
    for Col := ACol + 1 to AAdapter.ColCount - 1 do
    begin
      CurrentColumnOffset := AAdapter.ColOffsets[Col];
      if CurrentColumnOffset < TextRightExtent then
      begin
        AItem.CellSides := AItem.CellSides - [csRight];
        AItem := TdxReportCellSSString(AItem.getNextSibling);
        if AItem = nil then
          Break;
        AItem.CellSides := AItem.CellSides - [csLeft];
      end;
    end;
  end;

  procedure DoDeleteUnneededCellSidesFromLeftSide(AItem: TdxReportCellSSString; ACol: Integer);
  var
    TextLeftExtent, Col, CurrentColumnOffset: Integer;
  begin
    TextLeftExtent := AItem.TextExtentLeft;
    for Col := ACol downto 0 do
    begin
      CurrentColumnOffset := AAdapter.ColOffsets[Col];
      if CurrentColumnOffset > TextLeftExtent then
      begin
        AItem.CellSides := AItem.CellSides - [csLeft];
        AItem := TdxReportCellSSString(AItem.getPrevSibling);
        if (AItem = nil) or AItem.IsFixed then
          Break;
        AItem.CellSides := AItem.CellSides - [csRight];
      end;
    end;
  end;

var
  Col, Row: Integer;
  Item: TdxReportCellSSString;
begin
  for Row := 0 to AAdapter.RowCount - 1 do
    for Col := 0 to AAdapter.ColCount - 1 do
    begin
      Item := AAdapter[Col, Row];
      if Item.TextExtentRight <> 0 then
        DoDeleteUnneededCellSidesFromRightSide(Item, Col);
      if Item.TextExtentLeft <> 0 then
        DoDeleteUnneededCellSidesFromLeftSide(Item, Col);
    end;
end;

function TdxSpreadSheetAbstractReportLink.HasMergedCells: Boolean;
begin
  Result := MergedCells.Count <> 0;
end;

procedure TdxSpreadSheetAbstractReportLink.PostProcessItems(AAdapter: TdxPSGridCellsAdapter);

  procedure CalculateRTFBounds(AReportCell: TdxReportCellSSString; ACol, ARow: Integer; var R: TRect);
  var
    I: Integer;
    ALeft, ARight: Integer;
    ACell: TdxSpreadSheetCell;
  begin
    R := AReportCell.BoundsRect;
    ACell := GetSSCellObject(AReportCell.RealRow, AReportCell.RealCol);
    if ACell = nil then
      Exit;

    ALeft := 0;
    dxSpreadSheetTextService.CalculateSize(ACell, cxScreenCanvas, R, ACell.IsMerged, @ARight, nil);
    Dec(ARight, cxRectWidth(R));
    if ARight <= 0 then
      Exit;

    if ACell.Style.AlignHorz = ssahRight then
    begin
      ALeft := ARight;
      ARight := 0;
    end
    else
      if ACell.Style.AlignHorz = ssahCenter then
      begin
        ALeft := ARight div 2 + 1;
        ARight := ALeft;
      end;

    for I := ACol + 1 to AAdapter.ColCount - 1 do
    begin
      if (AAdapter.Cells[I, ARow].Text <> '') or (ARight <= 0) then
        Break;
      R.Right := AAdapter.Cells[I, ARow].Right;
      Dec(ARight, AAdapter.Cells[I, ARow].Width);
    end;

    for I := ACol - 1 downto 1 do
    begin
      if (AAdapter.Cells[I, ARow].Text <> '') or (ALeft <= 0) then
        Break;
      R.Left := AAdapter.Cells[I, ARow].Left;
      Dec(ALeft, AAdapter.Cells[I, ARow].Width);
    end;
  end;

var
  R: TRect;
  ARTFCells: TList;
  ARTFCellsPos: TList;
  ACell: TdxReportCellSSString;
  ACol, ARow: Integer;
  AItem: TAbstractdxReportCellData;
  AItemClass: TdxReportCellDataClass;
  AViewParams: TdxReportItemViewParams;
begin
  ARTFCells := TList.Create;
  ARTFCellsPos := TList.Create;
  try
    for ACol := 0 to AAdapter.ColCount - 1 do
      for ARow := 0 to AAdapter.RowCount - 1 do
      begin
        ACell := AAdapter.Cells[ACol, ARow];
        if not ACell.IsFixed and not ACell.IsVirtual then
        begin
          ACell.CalcBorderSubs(AAdapter);
          ACell.CalcDoubleBorderSlants(AAdapter);
        end;
        if ACell.IsRTF then
        begin
          ARTFCells.Add(ACell);
          CalculateRTFBounds(ACell, ACol, ARow, R);
          ARTFCellsPos.Add(Pointer(R.Left));
          ARTFCellsPos.Add(Pointer(R.Right));
        end;
      end;
     AItemClass := dxPSDataMaps.ItemClass(EditProperties);
     if AItemClass <> TdxReportCellString then
     begin
       for ACol := 0 to ARTFCells.Count - 1 do
       begin
         ACell := TdxReportCellSSString(ARTFCells[ACol]);
         AItem := AItemClass.Create(ACell.Parent);
         AItem.Assign(ACell);
         AItem.Left := Integer(ARTFCellsPos[ACol * 2]);
         AItem.Right := Integer(ARTFCellsPos[ACol * 2 + 1]);
         AViewParams := GetItemViewParams(ACell);
         dxPSDataMaps.InitializeItem(AItem, EditProperties, ACell.Text, ACell, AViewParams);
         ACell.Text := '';
         ACell.ExcludeFromClipRgn := False;
         ACell.Transparent := True;
       end;
     end;
  finally
    ARTFCellsPos.Free;
    ARTFCells.Free;
  end;
end;

procedure TdxSpreadSheetAbstractReportLink.AddContainers(AReportCells: TdxReportCells);
var
  AContainer: TdxSpreadSheetContainerAccess;
  AContainerBounds: TRect;
  AContainerCell: TdxReportCell;
  AContainerViewInfo: TdxSpreadSheetContainerViewInfo;
  AFocusedContainer: TdxSpreadSheetContainer;
  AView: TdxSpreadSheetCustomViewAccess;
  I: Integer;
begin
  AView := TdxSpreadSheetCustomViewAccess(SpreadSheet.ActiveSheet);
  if AView.Containers.Count > 0 then
  begin
    AFocusedContainer := AView.Controller.FocusedContainer;
    try
      AView.Controller.FocusedContainer := nil;
      for I := 0 to AView.Containers.Count - 1 do
      begin
        if CanPrintContainer(AView.Containers[I]) then
        begin
          AContainer := TdxSpreadSheetContainerAccess(AView.Containers[I]);
          AContainerViewInfo := AContainer.CreateViewInfo;
          try
            AContainerBounds := CalculateContainerBounds(AContainerViewInfo);
            if TranslateContainerBounds(AView, AContainerBounds) then
            begin
              if FHostContainers = nil then
                FHostContainers := CreateContainersHost(AReportCells);
              AContainerCell := TdxReportCell.Create(FHostContainers);
              AContainerCell.BoundsRect := AContainerBounds;
              AContainerCell.CellSides := [];
              AContainerCell.ClipChildren := True;
              InitializeContainerCell(AContainerCell, AContainerViewInfo);
            end;
          finally
            AContainerViewInfo.Free;
          end;
        end;
        if AbortBuilding then
          Break;
      end;
    finally
      AView.Controller.FocusedContainer := AFocusedContainer;
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.CalculateContainerBounds(AViewInfo: TdxSpreadSheetContainerViewInfo): TRect;
var
  AContainer: TdxSpreadSheetContainerAccess;
  AOrigin: TPoint;
begin
  AContainer := TdxSpreadSheetContainerAccess(AViewInfo.Owner);
  AOrigin := TdxSpreadSheetCustomViewAccess(AContainer.Parent).GetContentOrigin;
  AViewInfo.SetBounds(cxRectOffset(AContainer.Calculator.CalculateBounds, AOrigin, False));
  AViewInfo.Calculate;
  Result := cxRectOffset(AViewInfo.RealBounds, AOrigin);
end;

function TdxSpreadSheetAbstractReportLink.CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result := AContainer.Visible;
end;

function TdxSpreadSheetAbstractReportLink.CreateContainerImage(AViewInfo: TdxSpreadSheetContainerViewInfo): TdxSmartImage;
var
  AImageCanvas: TdxGPCanvas;
  APictureContainer: TdxSpreadSheetPictureContainer;
begin
  if AViewInfo.Owner is TdxSpreadSheetPictureContainer then
  begin
    APictureContainer := TdxSpreadSheetPictureContainer(AViewInfo.Owner);
    if cxRectIsNull(APictureContainer.Picture.CropMargins) and TdxSpreadSheetShapeAccess(APictureContainer.Shape).IsEmpty then
    begin
      Result := TdxSmartImage.Create;
      Result.Assign(APictureContainer.Picture.Image);
      Exit;
    end;
  end;

  Result := TdxSmartImage.CreateSize(AViewInfo.RealDrawingBounds);
  AImageCanvas := Result.CreateCanvas;
  try
    AImageCanvas.TranslateWorldTransform(-AViewInfo.RealDrawingBounds.Left, -AViewInfo.RealDrawingBounds.Top);
    AViewInfo.InternalDraw(AImageCanvas);
  finally
    AImageCanvas.Free;
  end;
end;

function TdxSpreadSheetAbstractReportLink.CreateContainersHost(AReportCells: TdxReportCells): TdxReportCell;
const
  HeadersBorderSize = 1;
var
  ARect: TRect;
begin
  SetupBoundsRect(AReportCells.Cells);

  ARect := AReportCells.Cells.BoundsRect;
  ARect.Top := ARect.Top + HeadersBorderSize;
  ARect.Left := ARect.Left + HeadersBorderSize;
  if IsShowRowAndColumnHeadings then
  begin
    Inc(ARect.Left, ColumnWidths[0]);
    if not HeadersOnEveryPage then
      Inc(ARect.Top, RowHeights[0]);
  end;

  Result := TdxReportCell.Create(AReportCells.AddOverlay);
  Result.BoundsRect := ARect;
  Result.ClipChildren := True;
  Result.CellSides := [];
end;

procedure TdxSpreadSheetAbstractReportLink.InitializeContainerCell(ACell: TdxReportCell; AViewInfo: TdxSpreadSheetContainerViewInfo);
var
  AImage: TdxSmartImage;
  AGraphic: TdxReportCellGraphic;
begin
  AImage := CreateContainerImage(AViewInfo);
  try
    if UseGrayScaleForGraphics then
      cxTransformImage(AImage, btmGrayScale);

    AGraphic := TdxReportCellGraphic(ACell.AddDataItem(TdxReportCellGraphic));
    AGraphic.BoundsRect := cxRectSetNullOrigin(ACell.BoundsRect);
    AGraphic.ImageBuffering := cibNone;
    AGraphic.ImageTransparent := True;
    AGraphic.CellSides := [];
    AGraphic.Stretch := True;
    AGraphic.Image := AImage;
  finally
    AImage.Free;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetContainerBoundsOffset: TPoint;
begin
  Result := cxNullPoint;
  if IsShowRowAndColumnHeadings then
  begin
    Result.X := -ColumnWidths[0];
    if not HeadersOnEveryPage then
      Result.Y := -RowHeights[0];
  end;
end;

function TdxSpreadSheetAbstractReportLink.TranslateContainerBounds(AView: TdxSpreadSheetCustomView; var ABounds: TRect): Boolean;
type
  TAnchorInfo = record
    Column: Integer;
    Row: Integer;
    ScaledOffsetX: Single;
    ScaledOffsetY: Single;
  end;

  function CalculateAnchor(const P: TPoint): TAnchorInfo;
  var
    AIntf: IdxSpreadSheetTableView;
    ARect: TRect;
  begin
    Result.Row := -1;
    Result.Column := -1;
    if SysUtils.Supports(AView, IdxSpreadSheetTableView, AIntf) then
    begin
      if AIntf.GetCellAtAbsolutePoint(P, Result.Row, Result.Column) then
      begin
        ARect := AIntf.GetAbsoluteCellBounds(Result.Row, Result.Column, False);
        Result.ScaledOffsetX := (P.X - ARect.Left) / Max(1, cxRectWidth(ARect));
        Result.ScaledOffsetY := (P.Y - ARect.Top) / Max(1, cxRectHeight(ARect));
      end;
    end;
  end;

  function GetAnchorPositionInReport(const AAnchor: TAnchorInfo; out P: TPoint): Boolean;
  var
    ARect: TRect;
  begin
    Result := (AAnchor.Row >= 0) and (AAnchor.Column >= 0) and GetCellBoundsInReport(AAnchor.Row, AAnchor.Column, False, ARect);
    if Result then
    begin
      P.X := ARect.Left + Trunc(AAnchor.ScaledOffsetX * cxRectWidth(ARect));
      P.Y := ARect.Top + Trunc(AAnchor.ScaledOffsetY * cxRectHeight(ARect));
    end;
  end;

begin
  Result :=
    GetAnchorPositionInReport(CalculateAnchor(ABounds.TopLeft), ABounds.TopLeft) and
    GetAnchorPositionInReport(CalculateAnchor(ABounds.BottomRight), ABounds.BottomRight);
  if Result then
    ABounds := cxRectOffset(ABounds, GetContainerBoundsOffset);
end;

function TdxSpreadSheetAbstractReportLink.GetCellBoundsInReport(
  ARow, AColumn: Integer; AConsiderMergedCells: Boolean; out ARect: TRect): Boolean;
var
  ACell: TdxReportCellSSString;
  AMergedCell: TdxSpreadSheetMergedCell;
  I, J: Integer;
  R: TRect;
begin
  if AConsiderMergedCells then
  begin
    AMergedCell := MergedCells.FindCell(ARow, AColumn);
    if AMergedCell <> nil then
      if GetCellBoundsInReport(AMergedCell.Area.Top, AMergedCell.Area.Left, False, R) and
         GetCellBoundsInReport(AMergedCell.Area.Bottom, AMergedCell.Area.Right, False, ARect)
      then
        begin
          ARect := cxRectUnion(ARect, R);
          Exit(True);
        end;
  end;

  Inc(ARow);
  Inc(AColumn);
  for I := 0 to FGridAdapter.RowCount - 1 do
    for J := 0 to FGridAdapter.ColCount - 1 do
    begin
      ACell := FGridAdapter.Cells[J, I];
      if (ACell <> nil) and (ACell.RealCol = AColumn) and (ACell.RealRow = ARow) then
      begin
        ARect := ACell.AbsoluteRect;
        Exit(True);
      end;
    end;

  Result := False;
end;

function TdxSpreadSheetAbstractReportLink.ReportAreaToSpreadSheetArea(const R: TRect): TRect;
begin
  Result := cxRectOffset(R, -1, -1);
end;

function TdxSpreadSheetAbstractReportLink.SpreadSheetAreaToReportArea(const R: TRect): TRect;
begin
  Result := cxRectOffset(dxSpreadSheetGetRealArea(R), 1, 1);
end;

function TdxSpreadSheetAbstractReportLink.GetBorderStylePatternClass(AStyle: TdxSpreadSheetCellBorderStyle): TdxPSEdgePatternClass;
begin
  Result :=  dxPSEdgePatternClassMap[AStyle];
end;

function TdxSpreadSheetAbstractReportLink.OnlyEdgeIsAssigned(const ACell: TdxSpreadSheetCell; ASide: TcxBorder): Boolean;
begin
  Result := IsCellDataEmpty(ACell) and ((ACell = nil) or AreStylesEqual(SpreadSheet.DefaultCellStyle, ACell.Style, [ASide]));
end;

function TdxSpreadSheetAbstractReportLink.OnlyLeftEdgeIsAssigned(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := OnlyEdgeIsAssigned(ACell, bLeft);
end;

function TdxSpreadSheetAbstractReportLink.OnlyTopEdgeIsAssigned(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := OnlyEdgeIsAssigned(ACell, bTop);
end;

function TdxSpreadSheetAbstractReportLink.IsMergedBorder(ACol, ARow: Integer; ASide: TdxCellSide): Boolean;
var
  AArea: TRect;
begin
  Result := False;
  if not IsFixedCell(ACol, ARow) then
  begin
    Dec(ACol);
    Dec(ARow);
    AArea := MergedCells.ExpandArea(ACol, ARow);
    if not dxSpreadSheetIsSingleCellArea(AArea) then
      case ASide of
        csLeft:
          Result := ACol = AArea.Left;
        csTop:
          Result := ARow = AArea.Top;
        csRight:
          Result := ACol = AArea.Right;
        csBottom:
          Result := ARow = AArea.Bottom;
      end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.IsMergedCell(ACol, ARow: Integer): Boolean;
begin
  Result := not (IsFixedCell(ACol, ARow) or dxSpreadSheetIsSingleCellArea(MergedCells.ExpandArea(ACol - 1, ARow - 1)));
end;

function TdxSpreadSheetAbstractReportLink.IsNearMostLeftCell(ACol, ARow: Integer): Boolean;
begin
  if IsShowRowAndColumnHeadings and HasSelection then
    Dec(ACol, SelectionRect.Left - 1);
  Result := ACol = 1;
end;

function TdxSpreadSheetAbstractReportLink.IsScaleGridLines: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetAbstractReportLink.IsNearMostTopCell(ACol, ARow: Integer): Boolean;
begin
  if IsShowRowAndColumnHeadings and HasSelection then
    Dec(ARow, SelectionRect.Top - 1);
  Result := ARow = 1;
end;

function TdxSpreadSheetAbstractReportLink.IsProcessedCol(ACol: Integer): Boolean;
var
  AItem: TdxSpreadSheetTableItem;
begin
  Result := inherited IsProcessedCol(ACol);
  if Result then
    if IsFixedCol(ACol) then
      Result := IsShowRowAndColumnHeadings
    else
    begin
      AItem := Sheet.Columns.Items[ACol - 1];
      Result := (AItem = nil) or AItem.Visible;
    end;
end;

function TdxSpreadSheetAbstractReportLink.IsProcessedRow(ARow: Integer): Boolean;
var
  AItem: TdxSpreadSheetTableItem;
begin
  Result := inherited IsProcessedRow(ARow);
  if Result then
    if IsFixedRow(ARow) then
      Result := IsShowRowAndColumnHeadings
    else
    begin
      AItem := Sheet.Rows.Items[ARow - 1];
      Result := (AItem = nil) or AItem.Visible;
    end;
end;

function TdxSpreadSheetAbstractReportLink.IsSuppressFontColors: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetAbstractReportLink.IsSuppressSourceFormats: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetAbstractReportLink.NeedTwoPassRendering: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetAbstractReportLink.UseGrayScaleForGraphics: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetAbstractReportLink.SetupCellBorders(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData);
const
  OppositeSide: array[TdxCellSide] of TdxCellSide = (csRight, csBottom, csLeft, csTop);
  HorzOffset: array[TdxCellSide] of Integer = (-1, 0, 1, 0);
  VertOffset: array[TdxCellSide] of Integer = (0, -1, 0, 1);
var
  ABorderColor: TColor;
  ADefaultAutoColor: TColor;
  ADefaultColor: TColor;
  ANeighborStyle: TdxSpreadSheetCellStyle;
  ASide: TdxCellSide;
  AStyle: TdxSpreadSheetCellStyle;
  AStyleBorder: TdxSpreadSheetCellBorder;
begin
  if IsFixedCell(ACol, ARow) then
    Exit;

  ADefaultColor := ColorToRGB(GetDefaultCellBorderColor);
  if IsSuppressSourceFormats then
  begin
    if IsMergedCell(ACol, ARow) then
    begin
      for ASide := Low(TdxCellSide) to High(TdxCellSide) do
        TdxReportCellSSString(ADataItem).Borders[ASide] := TdxPSCellBorder.Create(nil);
    end
    else
      for ASide := Low(TdxCellSide) to High(TdxCellSide) do
        TdxReportCellSSString(ADataItem).Borders[ASide] := TdxPSCellBorder.Create(TdxPSSolidEdgePattern, ADefaultColor);
  end
  else
  begin
    AStyle := GetSSCellStyle(ARow, ACol);
    ADefaultAutoColor := ColorToRGB(clWindowText);
    for ASide := Low(TdxCellSide) to High(TdxCellSide) do
    begin
      AStyleBorder := AStyle.Borders[TcxBorder(ASide)];
      if AStyleBorder.Style = sscbsDefault then
      begin
        ANeighborStyle := GetSSCellStyle(ARow + VertOffset[ASide], ACol + HorzOffset[ASide]);
        if ANeighborStyle <> nil then
          AStyleBorder := ANeighborStyle.Borders[TcxBorder(OppositeSide[ASide])];
      end;

      ABorderColor := AStyleBorder.Color;
      if dxSpreadSheetIsColorDefault(ABorderColor) then
      begin
        if AStyleBorder.Style = sscbsDefault then
          ABorderColor := ADefaultColor
        else
          ABorderColor := ADefaultAutoColor;
      end;

      TdxReportCellSSString(ADataItem).Borders[ASide] :=
        TdxPSCellBorder.Create(GetBorderStylePatternClass(AStyleBorder.Style), ABorderColor);
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetMergedCells: TdxSpreadSheetMergedCellList;
begin
  Result := Sheet.MergedCells;
end;

function TdxSpreadSheetAbstractReportLink.GetRealColor(AColor, ADefaultColor: TColor): TColor;
begin
  Result := ColorToRGB(cxGetActualColor(AColor, ADefaultColor));
end;

function TdxSpreadSheetAbstractReportLink.GetEditProperties: TcxCustomEditProperties;
begin
  Result := TdxSpreadSheetEditingControllerAccess(Sheet.Controller.EditingController).DefaultEditProperties;
end;

function TdxSpreadSheetAbstractReportLink.GetItemViewParams(AItem: TdxReportCellSSString): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.NativeParams.Font := AItem.Font;
  Result.NativeParams.Color := AItem.Color;
  Result.NativeParams.TextColor := clDefault;
end;

function TdxSpreadSheetAbstractReportLink.GetMeaningColCount: Integer;
var
  ACandidateColumn: Integer;
  ACell: TdxSpreadSheetCell;
  AColumn: Integer;
  ARow: Integer;
begin
  Result := -1;
  ACandidateColumn := 0;
  for ARow := 0 to Sheet.Dimensions.Bottom do
  begin
    for AColumn := Sheet.Dimensions.Right downto 0 do
    begin
      ACell := Sheet.Cells[ARow, AColumn];
      if not IsEmptyCell(ACell) then
      begin
        ACandidateColumn := AColumn;
        if (ACandidateColumn > 0) and OnlyLeftEdgeIsAssigned(ACell) then
          Dec(ACandidateColumn);
        Break;
      end;
    end;
    Result := Max(Result, ACandidateColumn);
  end;

  for AColumn := 0 to MergedCells.Count - 1 do
    Result := Max(Result, MergedCells[AColumn].Area.Right);
  Result := Max(Result, GetMeaningContainersArea.Right);

  if Result > -1 then
    Inc(Result);
end;

function TdxSpreadSheetAbstractReportLink.GetMeaningContainersArea: TRect;
var
  AColumnIndex: Integer;
  ARect: TRect;
  ARowIndex: Integer;
  ASheet: TdxSpreadSheetCustomViewAccess;
  ATableView: IdxSpreadSheetTableView;
  AViewInfo: TdxSpreadSheetContainerViewInfo;
  I: Integer;
begin
  Result := cxRect(MaxInt, MaxInt, 0, 0);
  if SysUtils.Supports(Sheet, IdxSpreadSheetTableView, ATableView) then
  begin
    ASheet := TdxSpreadSheetCustomViewAccess(Sheet);
    for I := 0 to ASheet.ViewInfo.Containers.Count - 1 do
    begin
      AViewInfo := TdxSpreadSheetContainerViewInfo(ASheet.ViewInfo.Containers[I]);
      if CanPrintContainer(AViewInfo.Owner) then
      begin
        ARect := AViewInfo.RealBounds;
        if ATableView.GetCellAtAbsolutePoint(ARect.TopLeft, ARowIndex, AColumnIndex) then
        begin
          Result.Top := Min(Result.Top, ARowIndex);
          Result.Left := Min(Result.Left, AColumnIndex);
        end;
        if ATableView.GetCellAtAbsolutePoint(ARect.BottomRight, ARowIndex, AColumnIndex) then
        begin
          Result.Bottom := Max(Result.Bottom, ARowIndex);
          Result.Right := Max(Result.Right, AColumnIndex);
        end;
      end;
    end;
  end;
end;

function TdxSpreadSheetAbstractReportLink.GetMeaningRowCount: Integer;
var
  ACell: TdxSpreadSheetCell;
  ACandidateRow: Integer;
  AColumn: Integer;
  AIsBreaked: Boolean;
  ARow: Integer;
  I: Integer;
begin
  ACandidateRow := -1;
  for ARow := Sheet.Dimensions.Bottom downto 0 do
  begin
    AIsBreaked := False;
    for AColumn := 0 to Sheet.Dimensions.Right do
    begin
      ACell := Sheet.Cells[ARow, AColumn];
      if not IsEmptyCell(ACell) then
      begin
        ACandidateRow := ARow;
        if (ACandidateRow = 0) or not OnlyTopEdgeIsAssigned(ACell) then
        begin
          AIsBreaked := True;
          Break;
        end;
      end;
    end;

    if ACandidateRow <> -1 then
    begin
      if not AIsBreaked then
        Dec(ACandidateRow);
      Break;
    end;
  end;

  Result := ACandidateRow;
  for I := 0 to MergedCells.Count - 1 do
    Result := Max(Result, MergedCells[I].Area.Bottom);
  Result := Max(Result, GetMeaningContainersArea.Bottom);

  if Result > -1 then
    Inc(Result);
end;

function TdxSpreadSheetAbstractReportLink.GetSheet: TdxSpreadSheetTableView;
begin
  if SpreadSheet <> nil then
    Result := SpreadSheet.ActiveSheetAsTable
  else
    Result := nil;
end;

function TdxSpreadSheetAbstractReportLink.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := TdxCustomSpreadSheet(Component);
end;

initialization
  TdxPSSSStringDataMap.Register;
  TdxPSSSStringGridCellDataMap.Register;
  TdxReportCellConnectionLine.Register;
  TdxReportCellBaseSSString.Register;
  TdxReportCellSSString.Register;

finalization
  TdxPSSSStringDataMap.Unregister;
  TdxPSSSStringGridCellDataMap.Unregister;
  TdxReportCellConnectionLine.Unregister;
  TdxReportCellBaseSSString.Unregister;
  TdxReportCellSSString.Unregister;
end.
