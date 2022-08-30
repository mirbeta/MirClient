{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxGridExportLink;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Variants, FMTBcd, SqlTimSt, Windows, Classes, SysUtils, Graphics, DB, dxCore, cxGeometry, cxControls,
  cxClasses, cxGraphics, cxStyles, cxLookAndFeels, cxLookAndFeelPainters, cxCustomData, cxDataStorage, cxDataUtils,
  cxDBData, dxLayoutContainer, cxVariants, cxExport, cxEdit, cxImage, cxCalendar, cxCurrencyEdit, cxSpinEdit, cxCalc,
  cxTimeEdit, cxMaskEdit, cxGrid, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView,
  cxGridCardView, cxGridLevel, cxGridRows, cxGridStrs, cxGridDBDataDefinitions, cxGridCommon, cxGridChartView, dxCoreClasses,
  cxGridLayoutView, cxGridWinExplorerView, Contnrs, Generics.Defaults, Generics.Collections, cxContainer, dxSparkline,
  dxSpreadSheetConditionalFormatting, cxDataControllerConditionalFormatting, dxSpreadSheetTypes;

const
  cxGridFooterCellIndent: Integer = 0;

type
  TcxExportCard = class;
  TcxGridCardViewExport = class;

  EcxGridExport = class(EdxException);

  { TcxGridExportProviderCache }

  TcxGridExportProviderCache = class
  strict private
    FColumns: TcxExportScale;
    FProvider: IcxExportProvider;
    FRows: TcxExportScale;

  public
    constructor Create(AProvider: IcxExportProvider);
    destructor Destroy; override;
    procedure Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
    function IsEmpty: Boolean;
    procedure PrepareToExport(ADataOnly: Boolean = False);
    //
    property Columns: TcxExportScale read FColumns;
    property Provider: IcxExportProvider read FProvider;
    property Rows: TcxExportScale read FRows;
  end;

  { IcxGridExportProviderAdapter }

  IcxGridExportProviderAdapter = interface
  ['{60404A30-D5D2-4A17-A086-C40FC64100B4}']
    function GetBoundingRect: TRect;
    function GetDimensions: TRect;
    function GetFormatCode(ACol, ARow: Integer): string;
    function GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
    function IsEmpty: Boolean;
    function IsGraphicsSupported: Boolean;
    function IsRTFSupported: Boolean;
    function RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
    function SetCellGraphic(const AGlobalArea: TRect; AStyleIndex: Integer; AGraphic: TGraphic; AFitMode: TcxImageFitMode = ifmStretch): TObject;
    procedure SetCellGraphicAsSharedHandle(const AGlobalArea: TRect; AStyleIndex: Integer; AHandle: TObject; AFitMode: TcxImageFitMode = ifmStretch);
    procedure SetCellStyle(const AArea: TRect; const AStyleIndex: Integer); overload;
    procedure SetCellStyle(const AGlobalCol, AGlobalRow, AStyleIndex: Integer); overload;
    procedure SetCellValue(const AGlobalCol, AGlobalRow: Integer; const AValue: Variant;
      const AValueDisplayFormat: string; AValueDisplayFormatType: TcxValueDisplayFormatType);
    procedure SetCellValueAsFormula(const ACol, ARow: Integer; const AValue: string; ADisplayText: string = '';
      AFormatCode: string = ''; AListSeparator: Char = ',');
    procedure SetDefaultStyle(const AStyle: TcxCacheCellStyle);

    procedure AddBounds(const ABounds: TRect);
    procedure AddOutlineGroup(AStart, AFinish: Integer);
    procedure AddRangeHorz(X1, X2: Integer);
    procedure AddRangeVert(Y1, Y2: Integer);
    procedure ApplyBestFit;
    procedure FreezePanes(ACol, ARow: Integer);
    procedure RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
    procedure RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
    procedure RealRangeToLogicalRangeHorz(X1, X2: Integer; out ALogicalX1, ALogicalX2: Integer);
    procedure RealRangeToLogicalRangeVert(Y1, Y2: Integer; out ALogicalY1, ALogicalY2: Integer);
  end;

  { TcxGridExportProviderAdapter }

  TcxGridExportProviderAdapter = class(TInterfacedObject, IcxGridExportProviderAdapter)
  strict private
    FBoundingRect: TRect;
    FClientOrigin: TPoint;
    FParentAdapter: IcxGridExportProviderAdapter;
    FProviderCache: TcxGridExportProviderCache;
  public
    constructor Create(ACache: TcxGridExportProviderCache);
    constructor CreateParented(AParentAdapter: IcxGridExportProviderAdapter; const AClientOrigin: TPoint);

    // IcxGridExportProviderAdapter
    function GetBoundingRect: TRect;
    function GetDimensions: TRect;
    function GetFormatCode(ACol, ARow: Integer): string;
    function GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
    function IsEmpty: Boolean;
    function IsGraphicsSupported: Boolean;
    function IsRTFSupported: Boolean;
    function RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
    function SetCellGraphic(const AGlobalArea: TRect; AStyleIndex: Integer; AGraphic: TGraphic; AFitMode: TcxImageFitMode = ifmStretch): TObject;
    procedure SetCellGraphicAsSharedHandle(const AGlobalArea: TRect; AStyleIndex: Integer; AHandle: TObject; AFitMode: TcxImageFitMode = ifmStretch);
    procedure SetCellStyle(const AGlobalCol, AGlobalRow, AStyleIndex: Integer); overload;
    procedure SetCellStyle(const AArea: TRect; const AStyleIndex: Integer); overload;
    procedure SetCellValue(const AGlobalCol, AGlobalRow: Integer; const AValue: Variant;
      const AValueDisplayFormat: string; AValueDisplayFormatType: TcxValueDisplayFormatType);
    procedure SetCellValueAsFormula(const AGlobalCol, AGlobalRow: Integer; const AValue: string; ADisplayText: string = '';
      AFormatCode: string = ''; AListSeparator: Char = ',');
    procedure SetDefaultStyle(const AStyle: TcxCacheCellStyle);

    procedure AddBounds(const ABounds: TRect);
    procedure AddOutlineGroup(AStart, AFinish: Integer);
    procedure AddRangeHorz(X1, X2: Integer);
    procedure AddRangeVert(Y1, Y2: Integer);
    procedure ApplyBestFit;
    procedure FreezePanes(ACol, ARow: Integer);
    procedure RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
    procedure RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
    procedure RealRangeToLogicalRangeHorz(X1, X2: Integer; out ALogicalX1, ALogicalX2: Integer);
    procedure RealRangeToLogicalRangeVert(Y1, Y2: Integer; out ALogicalY1, ALogicalY2: Integer);
  end;

  { TcxGridCustomExport }

  TcxGridCustomExportClass = class of TcxGridCustomExport;
  TcxGridCustomExport = class
  private
    FGridView: TcxCustomGridView;
  strict private
    FAdapter: IcxGridExportProviderAdapter;
    FDefaultRowHeight: Integer;
    FDefaultStyle: TcxCacheCellStyle;
    FDefaultStyleIndex: Integer;
    FExpand: Boolean;
    FFileName: string;
    FGrid: TcxCustomGrid;
    FHandler: TObject;
    FProgressHelper: TcxProgressCalculationHelper;
    FProviderCache: TcxGridExportProviderCache;
    FProviderCacheOwner: Boolean;
    FRecordsList: TList;
    FSaveAll: Boolean;
    FSaveGridModeFlag: Boolean;
    FUseNativeFormat: Boolean;
    FViewInfo: TcxCustomGridViewInfo;

    function GetDataController: TcxCustomDataController;
    function GetExpandButtonSize: Integer;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetRecord(ARecordIndex: Integer): TcxCustomGridRecord;
    function GetRecordCount: Integer;
    function GetViewInfo: TcxCustomGridViewInfo;
    procedure ProgressHandler(Sender: TObject; Percents: Integer);
  protected
    procedure BeforeCommit; virtual;
    function CalculateViewViewInfo(AGridView: TcxCustomGridView; ABounds: TRect): TcxCustomGridViewInfo; virtual;
    function CheckNativeValue(AProperties: TcxCustomEditProperties; AItem: TcxCustomGridTableItem; const AValue: Variant): Variant;

    procedure CreateExportCells; virtual;
    procedure DoCreateExportCells; virtual;
    procedure DoExportCells; virtual;
    procedure ExportCells; virtual;
    procedure Finalize; virtual;
    procedure Initialize; virtual;

    function CanUseNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
    procedure ExpandRecords(AFullExpand: Boolean); virtual;
    function ExportImagesAsGraphic: Boolean; virtual;
    procedure ExtractRowsForExport;
    procedure FillArea(const ABounds: TRect; AStyleIndex: Integer; ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
    procedure FillRealArea(const ABounds: TRect; AStyleIndex: Integer; ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
    function GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault): TcxCacheCellStyle;
    function GetDataOnly: Boolean; virtual;
    function GetField(AItem: TcxCustomGridTableItem): TField;
    function GetViewItemValue(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem): Variant;
    function GetViewItemValueEx(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AValueDisplayFormat: string; out AValueDisplayFormatType: TcxValueDisplayFormatType): Variant; virtual;
    function HasSelectedRecords: Boolean; virtual;
    function IsCurrencyItem(AItem: TcxCustomGridTableItem): Boolean;
    function IsCurrencyProperties(AProperties: TcxCustomEditProperties): Boolean;
    function IsEmpty: Boolean;
    function IsNativeFormatProperties(AProperties: TcxCustomEditProperties; AItem: TcxCustomGridTableItem): Boolean; virtual;
    function NeedExpandRecord(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function NeedProcessRecord(ARecord: TcxCustomGridRecord): Boolean; virtual;
    procedure RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
    procedure RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
    function RegisterContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams): Integer;
    function RegisterSolidStyle(AStyleIndex: Integer; AData: TObject = nil): Integer;
    function RegisterSolidStyleEx(AColor: TColor): Integer;
    procedure RegisterStyles; virtual;
    function RegisterViewParams(const AViewParams: TcxViewParams; const AAlignment: TAlignment = taLeftJustify): Integer;

    procedure SetRealCellStyle(const ARealBounds, ASearchArea: TRect; AStyleIndex: Integer);
    procedure SetRealCellStyleAndFormula(const ARealBounds, ASearchArea: TRect; AStyleIndex: Integer;
      const AFormula: string; ADisplayText: string = ''; AFormatCode: string = ''; AListSeparator: Char = ',');
    procedure SetRealCellStyleAndValue(const ARealBounds, ASearchArea: TRect;
      AStyleIndex: Integer; const AValue: Variant; const AValueDisplayFormat: string = '');
    procedure SetRealCellStyleAndValueEx(const ARealBounds, ASearchArea: TRect;
      AStyleIndex: Integer; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem);
    function TrySetCellValueAsGraphic(const ABounds, AArea: TRect; AStyleIndex: Integer;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem): Boolean;

    function TextHeight(AFont: TFont): Integer;
    function TextHeightEx(const AViewParams: TcxViewParams): Integer;
    function TextWidth(AFont: TFont; const AText: string): Integer;
    function TextWidthEx(const AViewParams: TcxViewParams; const AText: string): Integer;

    procedure ViewParamsToExportStyle(const AViewParams: TcxViewParams; var AExportStyle: TcxCacheCellStyle;
      const AAlignment: TAlignment = taLeftJustify; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault);

    property DataController: TcxCustomDataController read GetDataController;
    property DataOnly: Boolean read GetDataOnly;
    property DefaultRowHeight: Integer read FDefaultRowHeight write FDefaultRowHeight;
    property DefaultStyle: TcxCacheCellStyle read FDefaultStyle write FDefaultStyle;
    property DefaultStyleIndex: Integer read FDefaultStyleIndex write FDefaultStyleIndex;
    property ProgressHelper: TcxProgressCalculationHelper read FProgressHelper;
    property ProviderCache: TcxGridExportProviderCache read FProviderCache;
    property RecordsList: TList read FRecordsList;
  public
    constructor Create(const AFileName: string; AExportType: Integer; AGridView: TcxCustomGridView;
      AGrid: TcxCustomGrid; AViewInfo: TcxCustomGridViewInfo; AHandler: TObject = nil); overload; virtual;
    constructor Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
      AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil); overload; virtual;
    constructor CreateFrom(AMasterExport: TcxGridCustomExport; AGridView: TcxCustomGridView;
      AViewInfo: TcxCustomGridViewInfo; const AClientOrigin: TPoint); virtual;
    destructor Destroy; override;
    procedure AddSeparators(const ASeparators: array of string);
    procedure Assign(ASource: TcxGridCustomExport); virtual;
    procedure SetEncoding(AEncoding: TEncoding);
    procedure SetName(const AName: string);
    procedure DoExport; virtual;

    property Adapter: IcxGridExportProviderAdapter read FAdapter;
    property Expand: Boolean read FExpand write FExpand;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
    property FileName: string read FFileName;
    property Grid: TcxCustomGrid read FGrid;
    property GridView: TcxCustomGridView read FGridView;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property RecordCount: Integer read GetRecordCount;
    property Records[ARecordIndex: Integer]: TcxCustomGridRecord read GetRecord;
    property SaveAll: Boolean read FSaveAll write FSaveAll;
    property UseNativeFormat: Boolean read FUseNativeFormat write FUseNativeFormat;
    property ViewInfo: TcxCustomGridViewInfo read GetViewInfo;
  end;

  { TcxExportCustomItem }

  TcxExportCustomItemClass = class of TcxExportCustomItem;
  TcxExportCustomItem = class
  strict private
    FBounds: TRect;
  public
    function GetBoundsRelativeTo(ATop, ALeft: Integer): TRect;

    property Bounds: TRect read FBounds write FBounds;
  end;

  { TcxExportTextItem }

  TcxExportTextItemClass = class of TcxExportTextItem;
  TcxExportTextItem = class(TcxExportCustomItem)
  strict private
    FDisplayText: string;
  public
    property DisplayText: string read FDisplayText write FDisplayText;
  end;

  { TcxExportVisualItem }

  TcxExportVisualItemClass = class of TcxExportVisualItem;
  TcxExportVisualItem = class(TcxExportTextItem)
  private
    FData: TObject;
    FData2: TObject;
    FDetailExport: TcxGridCustomExport;
    FDetailViewViewInfo: TcxCustomGridViewInfo;
    FHidden: Boolean;
    FIsBackground: Boolean;
    FItemBorderColor: TColor;
    FItemBorders: TcxBorders;
    FSlave: Boolean;
    FStyle: Integer;
    FValue: Variant;
  public
    destructor Destroy; override;

    procedure DestroyDetailExport;
    procedure DestroyDetailViewViewInfo;
    function IsColumn: Boolean;

    property Data: TObject read FData write FData;
    property Data2: TObject read FData2 write FData2;
    property DetailExport: TcxGridCustomExport read FDetailExport write FDetailExport;
    property DetailViewViewInfo: TcxCustomGridViewInfo read FDetailViewViewInfo write FDetailViewViewInfo;
    property Hidden: Boolean read FHidden write FHidden;
    property IsBackground: Boolean read FIsBackground write FIsBackground;
    property ItemBorderColor: TColor read FItemBorderColor write FItemBorderColor;
    property ItemBorders: TcxBorders read FItemBorders write FItemBorders;
    property Slave: Boolean read FSlave write FSlave;
    property Style: Integer read FStyle write FStyle;
    property Value: Variant read FValue write FValue;
  end;

  { TcxGridCustomTableViewExport }

  TcxGridCustomTableViewExport = class(TcxGridCustomExport)
  private
    FVisualItemsList: TcxObjectList;

    function GetGridView: TcxCustomGridTableView;
    function GetVisualItem(AIndex: Integer): TcxExportVisualItem;
    function GetVisualItemCount: Integer;
  protected
    function AddVisualDataItem(const AItemBounds: TRect; AStyle: Integer; ARecord: TcxCustomGridRecord;
      AGridItem: TcxCustomGridTableItem): TcxExportVisualItem;
    function AddVisualItem(AItemClass: TcxExportVisualItemClass; const ABounds: TRect): TcxExportVisualItem; virtual;
    function AddVisualItemEx(const AItemBounds: TRect; const ADisplayText: string; const AViewParams: TcxViewParams;
      AAlignment: TAlignment; ABorders: TcxBorders; ABorderColor: TColor = clDefault; AIsBackground: Boolean = False;
      AWordWrap: Boolean = True): TcxExportVisualItem; overload;
    function AddVisualItemEx(const AItemBounds: TRect; const ADisplayText: string; AStyle: Integer;
      AIsBackground: Boolean = False): TcxExportVisualItem; overload;
    function CanExportVisualItem(AItem: TcxExportVisualItem): Boolean; virtual;
    procedure DoExportCells; override;
    procedure ExportBackgroundVisualItem(AItem: TcxExportVisualItem); virtual;
    procedure ExportVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect); virtual;
    procedure ExportDataVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect); virtual;
    function GetVisualItemsListCapacity: Integer; virtual;
    function HasSelectedRecords: Boolean; override;
    function IsDataVisualItem(AItem: TcxExportVisualItem): Boolean; virtual;

    procedure Initialize; override;
    procedure Finalize; override;
  public
    property GridView: TcxCustomGridTableView read GetGridView;
    property VisualItemCount: Integer read GetVisualItemCount;
    property VisualItems[Index: Integer]: TcxExportVisualItem read GetVisualItem;
    property VisualItemsList: TcxObjectList read FVisualItemsList;
  end;

  { TcxExportGroupSummaryItem }

  TcxExportGroupSummaryItem = class
  public
    Alignment: TAlignment;
    Bounds: TRect;
    Column: TcxCustomGridTableItem;
    Index: Integer;
    SummaryItem: TcxGridTableSummaryItem;
    Text: string;
    Value: Variant;
    ViewParams: TcxViewParams;
    procedure InitWidth;
  end;

  { TcxGridTableViewExport }

  TcxGridTableViewExport = class(TcxGridCustomTableViewExport)
  strict private
    FGroupSummaryItemsList: TcxObjectList;
    FFooterHeight: Integer;
    FRecordHeight: Integer;
    FRecordWidth: Integer;
    FPatternsList: TcxObjectList;

    function GetFooterCellBorderColor: TColor;
    function GetGridLineColor: TColor;
    function GetGridLines: TcxBorders;
    function GetGridView: TcxGridTableView;
    function GetGroupSummaryCount: Integer;
    function GetGroupSummaryItem(AIndex: Integer): TcxExportGroupSummaryItem;
    function GetHasPreview(ARow: TcxCustomGridRecord): Boolean;
    function GetIndicatorWidth: Integer;
    function GetLeftPos: Integer;
    function GetOptionsView: TcxGridTableOptionsView;
    function GetPattern(AIndex: Integer): TcxExportVisualItem;
    function GetPatternCount: Integer;
    function GetPreviewPlace: TcxGridPreviewPlace;
    function GetStyles: TcxGridTableViewStyles;
    function GetViewInfo: TcxGridTableViewInfo;
    procedure SetLeftPos(AValue: Integer);
  protected
    FRecordRowCount: Integer;

    procedure AddDataRow(var ATop, ALeft: Integer; ARow: TcxGridDataRow); virtual;
    procedure AddFooterCell(ABounds: TRect; AColumn: TcxGridColumn;
      AFooterItem: TcxDataSummaryItem; AValue: Variant; const AParams: TcxViewParams); virtual;
    procedure AddFooterCells(var ATop, ALeft: Integer); virtual;
    procedure AddFooterCellsByColumn(ABounds: TRect; AColumn: TcxGridColumn; APatternIndex: Integer; out ACount: Integer); virtual;
    procedure AddGroupFooterCells(var ATop, ALeft: Integer; ARow: TcxCustomGridRow); virtual;
    procedure AddGroupFooterCellsByColumn(ABounds: TRect; ARow: TcxCustomGridRow;
      AColumn: TcxGridColumn; APatternIndex: Integer; out ACount: Integer); virtual;
    procedure AddGroupRow(var ATop, ALeft: Integer; ARow: TcxGridGroupRow); virtual;
    function AddIndents(ATop, ARowHeight: Integer; ARow: TcxCustomGridRecord; AHasButton: Boolean; ALevel: Integer = -1): Integer; virtual;
    procedure AddMasterDataRow(var ATop, ALeft: Integer; ARow: TcxGridMasterDataRow); virtual;
    procedure AddRowFooter(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord; ALevel: Integer); virtual;
    procedure AddRowFooters(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord); virtual;
    procedure AddRowPreview(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord); virtual;
    function AddPattern(const ABounds: TRect; AData: TObject; AOffset: Integer = 0): TcxExportVisualItem;
    function AddVisualItem(AItemClass: TcxExportVisualItemClass; const ABounds: TRect): TcxExportVisualItem; override;
    function CalculateFooterCellCount(AItems: TcxDataSummaryItems): Integer; virtual;
    function CanShowMultiSummaries(AIsRowFooter: Boolean = False): Boolean;
    procedure CreateContent(var ATop, ALeft: Integer); virtual;
    procedure CreateFooter(var ATop, ALeft: Integer); virtual;
    procedure CreateHeader(var ATop, ALeft: Integer); virtual;
    procedure DoCreateExportCells; override;
    function CanExportVisualItem(AItem: TcxExportVisualItem): Boolean; override;
    procedure CreateRecordFromPattern(var ATop, ALeft: Integer; ARecord: TcxCustomGridRecord);
    function DoMergeCell(AMasterItem, ASlaveItem: TcxExportVisualItem): Boolean;
    procedure ExportDataVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect); override;
    procedure ExportDetailVisualItem(AItem: TcxExportVisualItem); virtual;
    procedure ExportVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect); override;
    procedure Finalize; override;
    function GetColumnOffset(AColumn: TcxGridColumn): Integer; virtual;
    function GetExpandButtonParams(ABorders: TcxBorders): TcxCacheCellStyle;
    function GetFooterCellCount: Integer; virtual;
    function GetFooterItemBounds(AIndex, ALineIndex: Integer; const AOrigin: TRect; AIsRowFooter: Boolean = False): TRect;
    function GetFooterRowCount: Integer; virtual;
    function GetGroupFooterCellCount(ARow: TcxCustomGridRow): Integer; virtual;
    function GetGroupFooterRowCount(ARow: TcxCustomGridRow): Integer; virtual;
    function GetGroupRowColumnIntersection(const ARowBounds: TRect; AColumn: TcxCustomGridTableItem): TRect;
    function GetIsSummaryUnderColumns(ARow: TcxGridGroupRow): Boolean; virtual;
    function GetPatternParams(ARecord: TcxCustomGridRecord; AItem: TcxExportVisualItem): TcxViewParams; virtual;
    function GetPreviewHeight(ARow: TcxCustomGridRecord): Integer;
    function GetVisualItemsListCapacity: Integer; override;
    function HasDetailViewSelectedRecords(AView: TcxCustomGridView): Boolean;
    function HasMasterRowSelectedChildren(AMasterRow: TcxGridMasterDataRow): Boolean;
    function HasMasterViewSelectedChildren(AView: TcxGridTableView): Boolean;
    procedure Initialize; override;
    function IsColumnContainsFooterItem(AItem: TcxDataSummaryItem; AColumn: TcxGridColumn): Boolean;
    function IsDataVisualItem(AItem: TcxExportVisualItem): Boolean; override;
    procedure MergeCells; virtual;
    function NeedProcessRecord(ARecord: TcxCustomGridRecord): Boolean; override;
    procedure ProcessGroupSummaryItem(ARow: TcxGridGroupRow; AValues: PVariant;
      AIndex: Integer; const ABounds: TRect; const ARowViewParams: TcxViewParams);
    procedure ProcessGroupSummaryItems(ARow: TcxGridGroupRow; ABounds: TRect);
    procedure ProduceHeadersContainer(var ATop, ALeft: Integer; AViewInfo: TcxGridColumnContainerViewInfo);
    procedure SetPatternsBounds(ATop, ABottom: Integer);
    procedure SetPatternsHeight(AHeight: Integer);
    procedure ScalePatterns(AHeight, ARowHeight: Integer);

    property GroupSummaryItemCount: Integer read GetGroupSummaryCount;
    property GroupSummaryItems[Index: Integer]: TcxExportGroupSummaryItem read GetGroupSummaryItem;
    property GroupSummaryItemsList: TcxObjectList read FGroupSummaryItemsList;
    property LeftPos: Integer read GetLeftPos write SetLeftPos;
    property PatternCount: Integer read GetPatternCount;
    property Patterns[Index: Integer]: TcxExportVisualItem read GetPattern;
    property PatternsList: TcxObjectList read FPatternsList;
  public
    property FooterHeight: Integer read FFooterHeight write FFooterHeight;
    property FooterCellBorderColor: TColor read GetFooterCellBorderColor;
    property GridLineColor: TColor read GetGridLineColor;
    property GridLines: TcxBorders read GetGridLines;
    property GridView: TcxGridTableView read GetGridView;
    property HasPreview[ARow: TcxCustomGridRecord]: Boolean read GetHasPreview;
    property IndicatorWidth: Integer read GetIndicatorWidth;
    property IsSummaryUnderColumns[ARow: TcxGridGroupRow]: Boolean read GetIsSummaryUnderColumns;
    property OptionsView: TcxGridTableOptionsView read GetOptionsView;
    property PreviewPlace: TcxGridPreviewPlace read GetPreviewPlace;
    property RecordHeight: Integer read FRecordHeight write FRecordHeight;
    property RecordRowCount: Integer read FRecordRowCount;
    property RecordWidth: Integer read FRecordWidth write FRecordWidth;
    property Styles: TcxGridTableViewStyles read GetStyles;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  end;

  { TcxGridBandedTableViewExport }

  TcxGridBandedTableViewExport = class(TcxGridTableViewExport)
  strict private
    function GetGridView: TcxGridBandedTableView;
    function GetOptionsView: TcxGridBandedTableOptionsView;
    function GetViewInfo: TcxGridBandedTableViewInfo;
  protected
    procedure CreateBandHeaders(var ATop: Integer; AForRootBands: Boolean); virtual;
    procedure CreateHeader(var ATop, ALeft: Integer); override;
    function GetColumnOffset(AColumn: TcxGridColumn): Integer; override;
    function GetContentOffset: TPoint; virtual;
    function GetIsSummaryUnderColumns(ARow: TcxGridGroupRow): Boolean; override;
    function GetParentBandOffset(ABand: TcxGridBand): Integer;
    function GetPatternByBand(ABand: TcxGridBand): TcxExportVisualItem;
    function GetPatternParams(ARecord: TcxCustomGridRecord; AItem: TcxExportVisualItem): TcxViewParams; override;
    function ProduceColumnsContainer(AContainer: TcxGridColumnContainerViewInfo; ATop, ALeft: Integer): Integer;
  public
    property ContentOffset: TPoint read GetContentOffset;
    property GridView: TcxGridBandedTableView read GetGridView;
    property OptionsView: TcxGridBandedTableOptionsView read GetOptionsView;
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
  end;

  { TcxExportCardRow }

  TcxExportCardRow = class
  strict private
    FOwner: TcxExportCard;

    function GetCaptionBounds: TRect;
    function GetCaptionStyle: TcxViewParams;
    function GetCard: TcxGridCard;
    function GetCategoryIndent: Integer;
    function GetDataAlignment: TAlignment;
    function GetDataBounds: TRect;
    function GetDataStyle: TcxViewParams;
    function GetDataValue: Variant;
    function GetHasIndent: Boolean;
    function GetHasSeparator: Boolean;
    function GetHeight: Integer;
    function GetIndentBounds: TRect;
    function GetIndentStyle: TcxViewParams;
    function GetSeparatorBounds: TRect;
    function GetSeparatorWidth: Integer;
    function GetShowCaption: Boolean;
    function GetShowData: Boolean;
    function GetVisibleCaption: string;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    FHasSeparator: Boolean;

    procedure AddToScales(AAdapter: IcxGridExportProviderAdapter);
  public
    Bounds: TRect;
    CaptionStyleIndex: Integer;
    CaptionWidth: Integer;
    DataStyleIndex: Integer;
    IndentStyleIndex: Integer;
    Row: TcxGridCardViewRow;
    constructor Create(AOwner: TcxExportCard);

    property CaptionBounds: TRect read GetCaptionBounds;
    property CaptionStyle: TcxViewParams read GetCaptionStyle;
    property Card: TcxGridCard read GetCard;
    property CategoryIndent: Integer read GetCategoryIndent;
    property DataAlignment: TAlignment read GetDataAlignment;
    property DataBounds: TRect read GetDataBounds;
    property DataStyle: TcxViewParams read GetDataStyle;
    property DataValue: Variant read GetDataValue;
    property HasIndent: Boolean read GetHasIndent;
    property HasSeparator: Boolean read GetHasSeparator;
    property Height: Integer read GetHeight write SetHeight;
    property IndentBounds: TRect read GetIndentBounds;
    property IndentStyle: TcxViewParams read GetIndentStyle;
    property Owner: TcxExportCard read FOwner;
    property SeparatorBounds: TRect read GetSeparatorBounds;
    property SeparatorWidth: Integer read GetSeparatorWidth;
    property ShowCaption: Boolean read GetShowCaption;
    property ShowData: Boolean read GetShowData;
    property VisibleCaption: string read GetVisibleCaption;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TcxExportCard }

  TcxExportCard = class
  strict private
    FCard: TcxGridCard;
    FLayersList: TcxObjectList;
    FOwner: TcxGridCardViewExport;

    function GetBorderWidth: Integer;
    function GetHasSeparators: Boolean;
    function GetLayer(AIndex: Integer): TList;
    function GetLayerCount: Integer;
    function GetLayerSeparator(AIndex: Integer): TRect;
    function GetRow(ALayerIndex, ARowIndex: Integer): TcxExportCardRow;
    function GetRowCount(ALayerIndex: Integer): Integer;
    function GetSeparatorWidth: Integer;
    procedure SetBounds(const ABounds: TRect);
  protected
    FBounds: TRect;

    function AddLayer: TList;
    function AddRow(ALayerIndex: Integer; ARow: TcxGridCardViewRow): TcxExportCardRow;
    procedure AddLayerSeparators;
    procedure AdjustLayersWidthToWidth;
    function AdjustRowsHeightInLayer(ALayer, ATop: Integer): Integer;
    procedure AdjustRowsWidthToWidth(ALayer: Integer);
    procedure CheckCategorySeparators(AHorizontalLayout: Boolean);
  public
    constructor Create(AOwner: TcxGridCardViewExport; ACard: TcxGridCard);
    destructor Destroy; override;
    procedure AddToScales(AAdapter: IcxGridExportProviderAdapter);
    procedure CalculateLayersCaptionWidth(AWidths: TcxExportIntList; AFistRowInLayerOnly: Boolean);
    function GetRowCaptionWidth(ALayerIndex, ARowIndex: Integer; AMaxWidth: Integer = 0): Integer;
    procedure SetLayersCaptionWidth(AWidths: TcxExportIntList; AFistRowInLayerOnly: Boolean);
    procedure SetRowCaptionWidth(ALayerIndex, ARowIndex, AWidth: Integer);

    property BorderWidth: Integer read GetBorderWidth;
    property Bounds: TRect read FBounds write SetBounds;
    property Card: TcxGridCard read FCard;
    property HasSeparators: Boolean read GetHasSeparators;
    property LayerCount: Integer read GetLayerCount;
    property Layers[AIndex: Integer]: TList read GetLayer;
    property LayerSeparators[Index: Integer]: TRect read GetLayerSeparator;
    property LayersList: TcxObjectList read FLayersList;
    property Owner: TcxGridCardViewExport read FOwner;
    property RowCount[ALayerIndex: Integer]: Integer read GetRowCount;
    property Rows[ALayerIndex, ARowIndex: Integer]: TcxExportCardRow read GetRow;
    property SeparatorWidth: Integer read GetSeparatorWidth;
  end;

  { TcxExportCardLayoutBuilder }

  TcxExportCardLayoutBuilder = class
  strict private
    FExportCard: TcxExportCard;
    FOwner: TcxGridCardViewExport;
    FRowsList: TList;
  protected
    function GetLayerIndex(ARow: TcxGridCardViewRow): Integer; virtual;
    procedure SplitRowsToLayers; virtual;
  public
    constructor Create(AOwner: TcxGridCardViewExport);
    destructor Destroy; override;
    procedure BuildLayout(ACard: TcxGridCard; AExportCard: TcxExportCard);

    property ExportCard: TcxExportCard read FExportCard;
    property Owner: TcxGridCardViewExport read FOwner;
    property RowsList: TList read FRowsList;
  end;

  { TcxGridCardViewExport }

  TcxGridCardViewExport = class(TcxGridCustomExport)
  strict private
    FCardBorderStyle: Integer;
    FCardSeparators: TcxExportScale;
    FCardSeparatorStyleIndex: Integer;

    FCategorySeparatorStyleIndex: Integer;
    FColumnCardCount: Integer;
    FKeepRowsSameHeight: Boolean;
    FLayerSeparatorStyleIndex: Integer;
    FLayoutBuilder: TcxExportCardLayoutBuilder;
    FRowCardCount: Integer;
    FExportCardsList: TcxObjectList;

    function GetCard(AIndex: Integer): TcxGridCard;
    function GetCardBorderWidth: Integer;
    function GetCardCount: Integer;
    function GetCardHeight: Integer;
    function GetCardIndent: Integer;
    function GetCardSeparator(AIndex: Integer): TRect;
    function GetCardSeparatorCount: Integer;
    function GetCardWidth: Integer;
    function GetCategoryIndent: Integer;
    function GetCategorySeparatorWidth: Integer;
    function GetExportCard(AIndex: Integer): TcxExportCard;
    function GetGridView: TcxGridCardView;
    function GetInterCardHorzSpace: Integer;
    function GetInterCardVertSpace: Integer;
    function GetIsHorizontalRows: Boolean;
    function GetIsSimpleLayout: Boolean;
    function GetLayerSeparatorWidth: Integer;
    function GetLayoutDirection: TcxGridCardViewLayoutDirection;
    function GetOptionsView: TcxGridCardViewOptionsView;
  protected
    procedure AddCardSeparator(APosition: Integer);
    function AddExportCard(AColumnPosition, ARowPosition: Integer; ACard: TcxGridCard): TcxExportCard;
    procedure AdjustRowCaptionWidth;
    procedure CalculateVisibleInfo;
    function CreateCardLayoutBuilder: TcxExportCardLayoutBuilder;
    procedure DoCreateExportCells; override;
    procedure DoExportCells; override;
    procedure ExportCardRow(ACard: TcxExportCard; ARow: TcxExportCardRow; const ACardLogicalBounds: TRect);
    procedure Finalize; override;
    function HasSelectedRecords: Boolean; override;
    procedure RegisterStyles; override;
    procedure SetRowSameHeight; virtual;

    property ExportCardsList: TcxObjectList read FExportCardsList;
  public
    property CardBorderStyle: Integer read FCardBorderStyle;
    property CardBorderWidth: Integer read GetCardBorderWidth;
    property CardCount: Integer read GetCardCount;
    property CardHeight: Integer read GetCardHeight;
    property CardIndent: Integer read GetCardIndent;
    property Cards[Index: Integer]: TcxGridCard read GetCard;
    property CardSeparatorCount: Integer read GetCardSeparatorCount;
    property CardSeparators[Index: Integer]: TRect read GetCardSeparator;
    property CardSeparatorStyleIndex: Integer read FCardSeparatorStyleIndex;
    property CardWidth: Integer read GetCardWidth;
    property CategoryIndent: Integer read GetCategoryIndent;
    property CategorySeparatorStyleIndex: Integer read FCategorySeparatorStyleIndex;
    property CategorySeparatorWidth: Integer read GetCategorySeparatorWidth;
    property ColumnCardCount: Integer read FColumnCardCount write FColumnCardCount;
    property ExportCards[Index: Integer]: TcxExportCard read GetExportCard;
    property GridView: TcxGridCardView read GetGridView;
    property InterCardHorzSpace: Integer read GetInterCardHorzSpace;
    property InterCardVertSpace: Integer read GetInterCardVertSpace;
    property IsHorizontalRows: Boolean read GetIsHorizontalRows;
    property IsSimpleLayout: Boolean read GetIsSimpleLayout;
    property KeepRowsSameHeight: Boolean read FKeepRowsSameHeight;
    property LayerSeparatorStyleIndex: Integer read FLayerSeparatorStyleIndex;
    property LayerSeparatorWidth: Integer read GetLayerSeparatorWidth;
    property LayoutBuilder: TcxExportCardLayoutBuilder read FLayoutBuilder;
    property LayoutDirection: TcxGridCardViewLayoutDirection read GetLayoutDirection;
    property OptionsView: TcxGridCardViewOptionsView read GetOptionsView;
    property RowCardCount: Integer read FRowCardCount write FRowCardCount;
  end;

  { TcxGridLayoutViewExport }

  TcxGridLayoutViewExport = class(TcxGridCustomTableViewExport)
  strict private
    FBackground: TcxViewParams;
    FCard: TcxGridLayoutViewRecord;
    FCardBounds: TRect;
    FCardViewInfo: TcxGridLayoutViewRecordViewInfo;
    FSavedItemStates: TObjectList;

    function GetCardCount: Integer;
    function GetCard(AIndex: Integer): TcxGridLayoutViewRecord;
    function GetContainer: TcxGridLayoutContainer;
    function GetGridView: TcxGridLayoutView;
    function GetRecordHeight: Integer;
    function GetRecordWidth: Integer;
    procedure UpdateGridViewSize;
  protected
    procedure AddRectangle(const ABounds: TRect; const AViewParams: TcxViewParams;
      ABorders: TcxBorders = []; ABorderColor: TColor = clDefault);
    procedure DoCreateExportCells; override;
    procedure Finalize; override;
    procedure Initialize; override;

    procedure ExportCardCaption(AInfo: TcxGridLayoutViewRecordCaptionViewInfo);
    procedure ExportEditItem(const ABounds: TRect; AInfo: TcxGridLayoutViewItemViewInfo);
    procedure ExportLayoutItem(AInfo: TdxCustomLayoutItemViewInfo);
    //
    procedure GetItemBordersInfo(const AItemInfo: TdxCustomLayoutItemViewInfo; AItem: TcxExportVisualItem);
    function GetParams(AItemInfo: TdxCustomLayoutItemViewInfo): TcxViewParams;
    function GetRelativeBounds(const AItemBounds: TRect): TRect;
    function HasCaption(AInfo: TdxCustomLayoutItemViewInfo): Boolean;

    property Background: TcxViewParams read FBackground;
    property Card: TcxGridLayoutViewRecord read FCard;
    property CardBounds: TRect read FCardBounds;
    property CardViewInfo: TcxGridLayoutViewRecordViewInfo read FCardViewInfo;
  public
    property GridView: TcxGridLayoutView read GetGridView;
    property CardCount: Integer read GetCardCount;
    property Cards[Index: Integer]: TcxGridLayoutViewRecord read GetCard;
    property Container: TcxGridLayoutContainer read GetContainer;
    property RecordHeight: Integer read GetRecordHeight;
    property RecordWidth: Integer read GetRecordWidth;
  end;

  { TcxGridWinExplorerViewExport }

  TcxGridWinExplorerViewExport = class(TcxGridCustomTableViewExport)
  private
    function GetGridView: TcxGridWinExplorerView;
    function GetRecord(ARecordIndex: Integer): TcxGridWinExplorerViewCustomRecord;
  protected
    procedure AddExportDataCell(AViewInfo: TcxGridWinExplorerViewCustomCellViewInfo);
    procedure AddExportDataRecord(AViewInfo: TcxGridWinExplorerViewRecordViewInfo);
    procedure AddExportGroupRecord(AViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo);
    procedure AddExportRecord(ARecord: TcxGridWinExplorerViewCustomRecord; ARecordViewInfo: TcxCustomGridRecordViewInfo);
    procedure DoCreateExportCells; override;
  public
    property GridView: TcxGridWinExplorerView read GetGridView;
    property Records[ARecordIndex: Integer]: TcxGridWinExplorerViewCustomRecord read GetRecord;
  end;

  { TcxGridChartViewExport }

  TcxGridChartViewExportEnumCellsProc = reference to procedure (AColumn, ARow: Integer; const AValue: Variant);

  TcxGridChartViewExport = class(TcxGridCustomExport)
  strict private
    FCellStyleIndex: Integer;
    FColumnsBounds: array of TRect;
    FHeaderStyleIndex: Integer;
    FRowHeight: Integer;

    function GetCellBounds(C, R: Integer): TRect;
    function GetColumnCount: Integer;
    function GetGridView: TcxGridChartView;
    function GetRowCount: Integer;
  protected
    procedure CreateExportCells; override;
    procedure CreateExportCellsAsData; virtual;
    procedure CreateExportCellsAsGraphic; virtual;
    procedure EnumCells(AProc: TcxGridChartViewExportEnumCellsProc);
    procedure ExportCells; override;
    procedure ExportCellsAsData; virtual;
    procedure ExportCellsAsGraphic; virtual;
    procedure RegisterStyles; override;
    //
    property CellBounds[C, R: Integer]: TRect read GetCellBounds;
    property ColumnCount: Integer read GetColumnCount;
    property RowCount: Integer read GetRowCount;
  public
    property GridView: TcxGridChartView read GetGridView;
  end;

  //Data Export

  TcxGridViewDataCustomExport = class;

  { TcxExportDataItem }

  TcxExportDataItemClass = class of TcxExportDataItem;
  TcxExportDataItem = class(TcxExportTextItem)
  strict private
    FData: TObject;
  public
    property Data: TObject read FData write FData;
  end;

  { TcxGridDataConditionalFormattingRules }

  TcxGridDataConditionalFormattingRules = class
  strict private
    FData: TDictionary<TdxSpreadSheetCustomConditionalFormattingRule, TdxSpreadSheetAreaList>;

    procedure OptimizeAreas(AAreas: TdxSpreadSheetAreaList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CellAdded(ASourceRow, ASourceColumn: Integer; const ATargetArea: TRect);
    procedure Commit(AProvider: IcxExportProvider);
    procedure Initialize(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
  end;

  { TcxGridViewDataAbstractExport }

  TcxGridViewDataAbstractExport = class(TcxGridCustomExport)
  strict private
    FConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    FConditionalFormattingRules: TcxGridDataConditionalFormattingRules;
    FOffset: TPoint;
  protected
    function GetDataOnly: Boolean; override;

    procedure BeforeCommit; override;
    procedure Initialize; override;

    property ConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider read FConditionalFormattingProvider;
    property ConditionalFormattingRules: TcxGridDataConditionalFormattingRules read FConditionalFormattingRules;
    property Offset: TPoint read FOffset write FOffset;
  public
    constructor Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
      AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil); override;
    destructor Destroy; override;
  end;

  { TcxGridViewDataCustomExport }

  TcxGridViewDataCustomExport = class(TcxGridViewDataAbstractExport)
  strict private
    FDataStyleIndex: Integer;
    FVisualItemsList: TcxObjectList;
  protected
    function CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect; virtual;
    function CreateExportCell(const ABounds: TRect; AClass: TcxExportCustomItemClass): TcxExportCustomItem; overload;
    function CreateExportCell(const ABounds: TRect; const ADisplayText: string; AClass: TcxExportTextItemClass): TcxExportTextItem; overload;
    procedure DoExportCells; override;
    procedure ExportCell(ACell: TcxExportCustomItem; ARect: TRect); virtual;
    function GetItemBounds(ALeftPos, ATopPos: Integer): TRect; virtual;
    function GetStyleIndex(ACell: TcxExportCustomItem): Integer; virtual;
    function RegisterDataStyle: Integer; virtual;
    procedure RegisterStyles; override;

    procedure Initialize; override;
    procedure Finalize; override;

    property DataStyleIndex: Integer read FDataStyleIndex;
  end;

  { TcxGridDataExportRange }

  TcxGridDataExportRangeClass = class of TcxGridDataExportRange;
  TcxGridDataExportRange = class
  strict private
    FFinish: Integer;
    FStart: Integer;
  protected
    function IsActive: Boolean;
    procedure Offset(ADelta: Integer);

    property Finish: Integer read FFinish write FFinish;
    property Start: Integer read FStart write FStart;
  public
    constructor Create(AStart: Integer); virtual;
    procedure Assign(Source: TcxGridDataExportRange); virtual;
  end;

  { TcxGridDataExportRanges }

  TcxGridDataExportRanges = class(TdxFastObjectList)
  private
    function GetItem(AIndex: Integer): TcxGridDataExportRange; inline;
    procedure SetItem(AIndex: Integer; ARange: TcxGridDataExportRange); inline;
  protected
    procedure AddFrom(ARanges: TcxGridDataExportRanges; AIndex: Integer; AOffset: Integer);
    function CreateRange(AStart: Integer): TcxGridDataExportRange;
    procedure FinishRange(AFinish: Integer); overload;
    procedure FinishRange(ARange: TcxGridDataExportRange; AFinish: Integer); overload;
    function GetRangeClass: TcxGridDataExportRangeClass; virtual;
    function HasActiveRange: Boolean;
    procedure StartRange(AStart: Integer); overload;
    procedure StartRange(AIndex: Integer; AStart: Integer); overload;
  public
    function First: TcxGridDataExportRange; inline;
    function Last: TcxGridDataExportRange; inline;

    property Items[Index: Integer]: TcxGridDataExportRange read GetItem write SetItem; default;
  end;

  { TcxGridDataExportFooterSubTotalFormulaRanges }

  TcxGridDataExportFooterSubTotalFormulaRanges = class(TcxGridDataExportRanges)
  private
    FParent: TcxCustomGridRecord;
  protected
    function IsParent(AParentRow: TcxCustomGridRecord): Boolean;

    property Parent: TcxCustomGridRecord read FParent;
  public
    constructor Create(AParentRow: TcxCustomGridRecord); virtual;
  end;

  { TcxGridDataExportFooterSubTotalFormulaBuilder }

  TcxGridDataExportFooterSubTotalFormulaBuilder = class(TdxFastObjectList)
  private
    FListSeparator: Char;

    function GetItem(AIndex: Integer): TcxGridDataExportFooterSubTotalFormulaRanges; inline;
    procedure SetItem(AIndex: Integer; AObject: TcxGridDataExportFooterSubTotalFormulaRanges); inline;
  protected
    function AddRanges(AParentRow: TcxCustomGridRecord): TcxGridDataExportFooterSubTotalFormulaRanges;
    function Build(AParentRow: TcxCustomGridRecord; AColumnIndex: Integer; AKind: TcxSummaryKind): string;
    function FindRanges(AParentRow: TcxCustomGridRecord): TcxGridDataExportFooterSubTotalFormulaRanges;
    procedure FinishRange(AFinish: Integer);
    function GetSubTotalDescription(AKind: TcxSummaryKind): string;
    function HasActiveRange: Boolean;
    function IsParentForRanges(AParent: TcxCustomGridRecord; ARanges: TcxGridDataExportFooterSubTotalFormulaRanges): Boolean;
    procedure StartRange(AParentRow: TcxCustomGridRecord; AStart: Integer);

    property ListSeparator: Char read FListSeparator;
  public
    constructor Create(AOwnsObjects: Boolean = True; ACapacity: Integer = 0);

    function First: TcxGridDataExportFooterSubTotalFormulaRanges; inline;
    function Last: TcxGridDataExportFooterSubTotalFormulaRanges; inline;

    property Items[Index: Integer]: TcxGridDataExportFooterSubTotalFormulaRanges read GetItem write SetItem; default;
  end;

  { TcxGridDataExportOutlineGroupRange }

  TcxGridDataExportOutlineGroupRange = class(TcxGridDataExportRange)
  strict private
    FLevel: Integer;
  protected
    property Level: Integer read FLevel write FLevel;
  public
    constructor Create(AStart: Integer); override;
    procedure Assign(Source: TcxGridDataExportRange); override;
  end;

  { TcxGridDataExportOutlineGroupRanges }

  TcxGridDataExportOutlineGroupRanges = class(TcxGridDataExportRanges)
  strict private
    function GetItem(AIndex: Integer): TcxGridDataExportOutlineGroupRange; inline;
    procedure SetItem(AIndex: Integer; AObject: TcxGridDataExportOutlineGroupRange); inline;
  protected
    procedure FinishLevelRange(AFinish: Integer; ALevel: Integer);
    function GetRangeClass: TcxGridDataExportRangeClass; override;
    procedure StartLevelRange(AStart: Integer; ALevel: Integer); overload;
    procedure StartLevelRange(AIndex: Integer; AStart: Integer; ALevel: Integer); overload;
  public
    function First: TcxGridDataExportOutlineGroupRange; inline;
    function Last: TcxGridDataExportOutlineGroupRange; inline;

    property Items[Index: Integer]: TcxGridDataExportOutlineGroupRange read GetItem write SetItem; default;
  end;

  { TcxExportGridViewDataItem }

  TcxExportGridViewDataItemClass = class of TcxExportGridViewDataItem;
  TcxExportGridViewDataItem = class(TcxExportDataItem)
  strict private
    FRecord: TcxCustomGridRecord;
  public
    property GridRecord: TcxCustomGridRecord read FRecord write FRecord;
  end;

  { TcxExportGridViewFormulaDataItem }

  TcxExportGridViewFormulaDataItem = class(TcxExportDataItem)
  strict private
    FFormula: string;
  public
    property Formula: string read FFormula write FFormula;
  end;

  { TcxExportGridViewDetailDataItem }

  TcxExportGridViewDetailDataItem = class(TcxExportGridViewDataItem)
  strict private
    function GetDetailExport: TcxGridViewDataCustomExport;
    procedure SetDetailExport(AValue: TcxGridViewDataCustomExport);
  public
    destructor Destroy; override;
    procedure DestroyDetailExport;
    procedure UpdateBoundByDetailBounds(ALeftOffset, ATopOffset: Integer); virtual;

    property DetailExport: TcxGridViewDataCustomExport read GetDetailExport write SetDetailExport;
  end;

  { TcxGridViewDataExport }

  TcxGridViewDataExport = class(TcxGridViewDataCustomExport)
  strict private
    FContentStyleIndex: Integer;
    FOutlineGroupRanges: TcxGridDataExportOutlineGroupRanges;

    function GetGridView: TcxCustomGridTableView;
  protected
    procedure AddOutlineGroups; virtual;
    function CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect; override;
    procedure CreateExportCell(const ABounds: TRect; const ADisplayText: string;
      ARecord: TcxCustomGridRecord; AData: TObject = nil); reintroduce; overload;
    function CreateExportCell(const ABounds: TRect; const ADisplayText: string; ARecord: TcxCustomGridRecord;
      AData: TObject; AClass: TcxExportGridViewDataItemClass): TcxExportGridViewDataItem; reintroduce; overload; virtual;
    procedure CreateExportContent(var ALeft, ATop: Integer); virtual;
    procedure CreateExportDataRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure CreateExportGroupRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure CreateExportRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure CreateExportRecords(var ALeft, ATop: Integer); virtual;
    procedure CreateExportHeader(var ALeft, ATop: Integer); virtual;
    procedure CreateIndents(AStartLeft, AFinishLeft, AStartTop, AFinishTop: Integer); virtual;
    procedure DoCreateExportCells; override;
    procedure ExportCell(ACell: TcxExportCustomItem; ARect: TRect); override;
    function GetGroupDisplayText(ARecord: TcxCustomGridRecord): string; virtual;
    function GetStyleIndex(ACell: TcxExportCustomItem): Integer; override;
    function HasHeader: Boolean; virtual;
    function HasSelectedRecords: Boolean; override;
    function RegisterContentDataStyle: Integer; virtual;
    procedure RegisterStyles; override;

    property ContentStyleIndex: Integer read FContentStyleIndex;
    property OutlineGroupRanges: TcxGridDataExportOutlineGroupRanges read FOutlineGroupRanges;
  public
    constructor Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
      AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil); override;
    destructor Destroy; override;

    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  { TcxGridTableViewDataExport }

  TcxGridTableViewDataExport = class(TcxGridViewDataExport)
  private
    FFooterSubTotalFormulaBuilder: TcxGridDataExportFooterSubTotalFormulaBuilder;

    function GetGridView: TcxGridTableView;
  protected
    procedure AddDetailOutlineGroupRanges(ADetailExport: TcxGridViewDataExport; ATop: Integer); virtual;
    procedure AddOutlineGroups; override;
    function CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect; override;
    procedure CreateExportContent(var ALeft, ATop: Integer); override;
    procedure CreateExportDataRecord(var ALeft: Integer; var ATop: Integer; ARecord: TcxCustomGridRecord); override;
    procedure CreateExportFooterByColumn(ALeft, ATop: Integer; AColumn: TcxGridColumn); virtual;
    procedure CreateExportFooters(var ALeft, ATop: Integer); virtual;
    procedure CreateExportFormulaCell(const ABounds: TRect; const AFormula, ADisplayText: string; AData: TObject); virtual;
    procedure CreateExportGroupFooterByColumn(ALeft, ATop: Integer; ARow: TcxCustomGridRecord; AColumn: TcxGridColumn); virtual;
    procedure CreateExportGroupRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord); override;
    procedure CreateExportMasterDataRow(var ALeft, ATop: Integer; ARecord: TcxGridMasterDataRow); virtual;
    procedure CreateExportRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord); override;
    procedure CreateExportRowFooter(var ALeft, ATop: Integer; ADataLevel, ALevel: Integer; ARow: TcxCustomGridRecord); virtual;
    procedure CreateExportRowFooters(var ALeft, ATop: Integer; ARow: TcxCustomGridRecord); virtual;
    procedure ExportCell(ACell: TcxExportCustomItem; ARect: TRect); override;
    procedure ExportDetailCell(ACell: TcxExportGridViewDetailDataItem); virtual;
    procedure ExportFooterCell(ACell: TcxExportDataItem; ARect: TRect); virtual;
    function GetFooterFormatCode(ACell: TcxExportDataItem): string; virtual;
    function GetFooterLineCount: Integer; virtual;
    function GetGroupDisplayText(ARecord: TcxCustomGridRecord): string; override;
    function GetGroupFooterLineCount(ARow: TcxCustomGridRecord): Integer; virtual;
    function HasFooter: Boolean; virtual;
    function HasHeader: Boolean; override;

    property FooterSubTotalFormulaBuilder: TcxGridDataExportFooterSubTotalFormulaBuilder read FFooterSubTotalFormulaBuilder;
  public
    constructor Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
      AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil); override;
    destructor Destroy; override;

    property GridView: TcxGridTableView read GetGridView;
  end;

  { TcxGridWinExplorerViewDataExport }

  TcxGridWinExplorerViewDataExport = class(TcxGridViewDataExport)
  protected
    function GetGroupDisplayText(ARecord: TcxCustomGridRecord): string; override;
  end;

  { TcxGridChartViewDataExport }

  TcxGridChartViewDataExport = class(TcxGridViewDataCustomExport)
  strict private
    function GetGridView: TcxGridChartView;
  protected
    function CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect; override;
    procedure CreateExportCategories(ASeries: TcxGridChartSeries; AHasLeftIndent, AHasTopIndent: Boolean); virtual;
    procedure CreateExportCategoryDescriptions; virtual;
    procedure CreateExportSeries; virtual;
    procedure CreateExportSeriesDescriptions; virtual;
    procedure DoCreateExportCells; override;
    function HasCategory: Boolean; virtual;
    function HasSeries: Boolean; virtual;
    procedure RegisterStyles; override;
  public
    property GridView: TcxGridChartView read GetGridView;
  end;

const
  ExportImagesAsGraphic: Boolean = True;

procedure ExportGridToHTML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure ExportGridToXML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure ExportGridToCSV(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure ExportGridToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure ExportGridToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean;
  ASaveAll: Boolean; const ASeparator, ABeginString, AEndString: string; const AFileExt: string = 'txt';
  AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;

procedure ExportGridToExcel(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure ExportGridToXLSX(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure ExportGridToFile(AFileName: string; AExportType: Integer; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparator, ABeginString, AEndString: string;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure ExportGridDataToHTML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure ExportGridDataToXML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure ExportGridDataToCSV(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure ExportGridDataToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure ExportGridDataToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean;
  ASaveAll: Boolean; const ASeparator, ABeginString, AEndString: string; const AFileExt: string = 'txt';
  AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;

procedure ExportGridDataToExcel(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure ExportGridDataToXLSX(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure ExportGridDataToFile(AFileName: string; AExportType: Integer; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparator, ABeginString, AEndString: string;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);

implementation

uses
  Math, RTLConsts, Dialogs, cxGridCustomLayoutView, cxGridViewLayoutContainer, cxImageComboBox,
  cxTextEdit, cxLookupEdit, dxSpreadSheetUtils, dxDPIAwareUtils;

type
  TConditionalFormattingProviderAccess = class(TcxDataControllerConditionalFormattingProvider);
  TDataRowAccess = class(TcxGridDataRow);

const
  AlignmentToExportAlignment: array[TAlignment] of TcxAlignText = (catLeft, catRight, catCenter);

  GridLinesToBorders: array[TcxGridLines] of TcxBorders = (cxBordersAll, [], [bRight, bLeft], [bTop, bBottom]);

  BorderWidths: array[Boolean] of Integer = (0, 1);

  ButtonTexts: array[Boolean] of string = ('+', '-');

  CardRowDefaultWidth = 20;
  cxDefaultRowHeight  = 19;

  cxVisualItemsListCapacity = 1000000;
  cxVisualItemsListCapacityForMasterDetails = cxVisualItemsListCapacity;

type
  TcxCustomGridRecordAccess = class(TcxCustomGridRecord);
  TcxCustomGridRecordViewInfoAccess = class(TcxCustomGridRecordViewInfo);
  TcxGridChartViewAccess = class(TcxGridChartView);
  TcxGridLayoutViewAccess = class(TcxGridLayoutView);
  TcxImagePropertiesAccess = class(TcxImageProperties);
  TcxTableItemAccess = class(TcxCustomGridTableItem);
  TdxLayoutGroupViewInfoAccess = class(TdxLayoutGroupViewInfo);
  TdxLayoutItemCaptionViewInfoAccess = class(TdxCustomLayoutItemCaptionViewInfo);
  TdxLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);

function cxCompareGroupSummaryItems(AItem1, AItem2: TcxExportGroupSummaryItem): Integer;
begin
  if AItem1.Column =  AItem2.Column then
    Result := AItem1.Index - AItem2.Index
  else
    if AItem1.Column = nil then
      Result := -1
    else
      if AItem2.Column = nil then
        Result := 1
      else
        Result := AItem1.Column.VisibleIndex - AItem2.Column.VisibleIndex;
end;

function GetExportClassByGridView(AGridView: TcxCustomGridView): TcxGridCustomExportClass;
begin
  if AGridView is TcxGridBandedTableView then
    Result := TcxGridBandedTableViewExport
  else if AGridView is TcxGridTableView then
    Result := TcxGridTableViewExport
  else if AGridView is TcxGridCardView then
    Result := TcxGridCardViewExport
  else if AGridView is TcxGridChartView then
    Result := TcxGridChartViewExport
  else if AGridView is TcxGridLayoutView then
    Result := TcxGridLayoutViewExport
  else if AGridView is TcxGridWinExplorerView then
    Result := TcxGridWinExplorerViewExport
  else
    raise EcxGridExport.Create(cxGetResourceString(@scxNotExistGridView));
end;

function GetExportDataClassByGridView(AGridView: TcxCustomGridView): TcxGridCustomExportClass;
begin
  if AGridView is TcxGridTableView then
    Result := TcxGridTableViewDataExport
  else if AGridView is TcxGridCustomLayoutView then
    Result := TcxGridViewDataExport
  else if AGridView is TcxGridWinExplorerView then
    Result := TcxGridWinExplorerViewDataExport
  else if AGridView is TcxGridChartView then
    Result := TcxGridChartViewDataExport
  else
    raise EcxGridExport.Create(cxGetResourceString(@scxNotExistGridView));
end;

procedure CalculateCardRowWidths(var AWidths: array of Integer; AAvailableWidth: Integer);
var
  AAutoWidths: TcxAutoWidthObject;
  AAllFixed: Boolean;
  I: Integer;
begin
  AAutoWidths := TcxAutoWidthObject.Create(Length(AWidths));
  try
    AAllFixed := True;
    for I := 0 to High(AWidths) do
      with AAutoWidths.AddItem do
      begin
        Width := AWidths[I];
        Fixed := Width <> 0;
        AAllFixed := AAllFixed and Fixed;
        if Width = 0 then Width := CardRowDefaultWidth;
      end;
    if AAllFixed or (AAutoWidths.Width > AAvailableWidth) then
      for I := 0 to AAutoWidths.Count - 1 do
        AAutoWidths[I].Fixed := False;
    AAutoWidths.AvailableWidth := AAvailableWidth;
    AAutoWidths.Calculate;
    for I := 0 to High(AWidths) do
      AWidths[I] := AAutoWidths[I].AutoWidth;
  finally
    AAutoWidths.Free;
  end;
end;

procedure DoExportGridToFile(AFileName: string; AExportType: Integer; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparator, ABeginString, AEndString: string;
  const AFileExt: string; AExportDataOnly: Boolean; AHandler: TObject; AEncoding: TEncoding);

  function CreateExport(AView: TcxCustomGridView): TcxGridCustomExport;
  var
    AExportClass: TcxGridCustomExportClass;
  begin
    if AExportDataOnly then
      AExportClass := GetExportDataClassByGridView(AView)
    else
      AExportClass := GetExportClassByGridView(AView);

    Result := AExportClass.Create(AFileName, AExportType, AView, AGrid, nil, AHandler);
    Result.SaveAll := ASaveAll;
    Result.Expand := AExpand;
    Result.UseNativeFormat := AUseNativeFormat;
    Result.AddSeparators([ASeparator, ABeginString, AEndString]);
    Result.SetName(ChangeFileExt(ExtractFileName(AFileName), ''));
    Result.SetEncoding(AEncoding);
  end;

  procedure BeginExport(out APrevEnabled: Boolean);
  begin
    APrevEnabled := AGrid.Enabled;
    AGrid.BeginExport;
    AGrid.BeginUpdate;
    AGrid.Enabled := False;
  end;

  procedure EndExport(AWasEnabled: Boolean);
  begin
    AGrid.Enabled := AWasEnabled;
    AGrid.EndUpdate;
    AGrid.EndExport;
  end;

var
  APrevEnabled: Boolean;
begin
  if AGrid <> nil then
  begin
    if AFileExt <> '' then
      AFileName := ChangeFileExt(AFileName, '.' + AFileExt);

    ShowHourglassCursor;
    try
      BeginExport(APrevEnabled);
      try
        with CreateExport(AGrid.ActiveLevel.GridView) do
        try
          DoExport;
        finally
          Free;
        end;
      finally
        EndExport(APrevEnabled);
      end;
    finally
      HideHourglassCursor;
    end;
  end;
end;

procedure ExportGridToHTML(const AFileName: string; AGrid: TcxGrid;
  AExpand, ASaveAll: Boolean; const AFileExt: string; AHandler: TObject);
begin
  ExportGridToFile(AFileName, cxExportToHtml, AGrid, AExpand, ASaveAll, False, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridToXML(const AFileName: string; AGrid: TcxGrid;
  AExpand, ASaveAll: Boolean; const AFileExt: string; AHandler: TObject);
begin
  ExportGridToFile(AFileName, cxExportToXml, AGrid, AExpand, ASaveAll, False, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridToCSV(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  ExportGridToFile(AFileName, cxExportToCSV, AGrid, AExpand, ASaveAll, False, ASeparator, '', '', AFileExt, AHandler, AEncoding);
end;

procedure ExportGridToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  ExportGridToText(AFileName, AGrid, AExpand, ASaveAll, '', '', '', AFileExt, AHandler, AEncoding);
end;

procedure ExportGridToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean;
  ASaveAll: Boolean; const ASeparator, ABeginString, AEndString: string; const AFileExt: string = 'txt';
  AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  ExportGridToFile(AFileName, cxExportToText, AGrid, AExpand, ASaveAll,
    False, ASeparator, ABeginString, AEndString, AFileExt, AHandler, AEncoding);
end;

procedure ExportGridToExcel(const AFileName: string; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const AFileExt: string; AHandler: TObject);
begin
  ExportGridToFile(AFileName, cxExportToExcel, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridToXLSX(const AFileName: string; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const AFileExt: string; AHandler: TObject);
begin
  ExportGridToFile(AFileName, cxExportToXLSX, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridDataToHTML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'html'; AHandler: TObject = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToHtml, AGrid, AExpand, ASaveAll, True, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridToFile(AFileName: string; AExportType: Integer; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparator, ABeginString, AEndString: string;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  DoExportGridToFile(AFileName, AExportType, AGrid, AExpand, ASaveAll, AUseNativeFormat,
    ASeparator, ABeginString, AEndString, AFileExt, False, AHandler, AEncoding);
end;

procedure ExportGridDataToXML(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const AFileExt: string = 'xml'; AHandler: TObject = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToXml, AGrid, AExpand, ASaveAll, True, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridDataToCSV(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True;
  ASaveAll: Boolean = True; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToCSV, AGrid, AExpand, ASaveAll, True, ASeparator, '', '', AFileExt, AHandler, AEncoding);
end;

procedure ExportGridDataToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  ExportGridDataToText(AFileName, AGrid, AExpand, ASaveAll, '', '', '', AFileExt, AHandler, AEncoding);
end;

procedure ExportGridDataToText(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean;
  ASaveAll: Boolean; const ASeparator, ABeginString, AEndString: string; const AFileExt: string = 'txt';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToText, AGrid, AExpand, ASaveAll, True, ASeparator,
    ABeginString, AEndString, AFileExt, AHandler, AEncoding);
end;

procedure ExportGridDataToExcel(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xls'; AHandler: TObject = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToExcel, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridDataToXLSX(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean = True; ASaveAll: Boolean = True;
  AUseNativeFormat: Boolean = True; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);
begin
  ExportGridDataToFile(AFileName, cxExportToXLSX, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', '', '', AFileExt, AHandler);
end;

procedure ExportGridDataToFile(AFileName: string; AExportType: Integer; AGrid: TcxGrid;
  AExpand, ASaveAll, AUseNativeFormat: Boolean; const ASeparator, ABeginString, AEndString: string;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  DoExportGridToFile(AFileName, AExportType, AGrid, AExpand, ASaveAll, AUseNativeFormat,
    ASeparator, ABeginString, AEndString, AFileExt, True, AHandler, AEncoding);
end;

type
  { TcxGridExportHelper }

  TcxGridExportHelper = class
  public
    class function CalculateFooterLineCount(AItems: TcxDataSummaryItems): Integer;
    class function HasGroupFooter(AGridView: TcxGridTableView; ARow: TcxCustomGridRecord; var ALevel: Integer): Boolean;
    class function IsSameFooterItems(AItem1, AItem2: TcxDataSummaryItem): Boolean;
  end;

{ TcxGridExportProviderCache }

constructor TcxGridExportProviderCache.Create(AProvider: IcxExportProvider);
begin
  inherited Create;
  FProvider := AProvider;

  FColumns := TcxExportScale.Create;
  FColumns.Capacity := 100000;
  FColumns.Add(0);

  FRows := TcxExportScale.Create;
  FRows.Capacity := 4000000;
  FRows.Add(0);
end;

destructor TcxGridExportProviderCache.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TcxGridExportProviderCache.Commit(AProgressHelper: TcxCustomProgressCalculationHelper; AHandler: TObject);
begin
  Rows.Clear;
  Columns.Clear;
  Provider.Commit(AProgressHelper, AHandler);
end;

function TcxGridExportProviderCache.IsEmpty: Boolean;
begin
  Result := (Columns.VisibleCount = 0) or (Rows.VisibleCount = 0);
end;

procedure TcxGridExportProviderCache.PrepareToExport(ADataOnly: Boolean = False);
var
  I: Integer;
begin
  Columns.Arrange;
  Rows.Arrange;

  if not (IsEmpty or ADataOnly) then
  begin
    Provider.SetRange(Columns.VisibleCount, Rows.VisibleCount, False);
    for I := 0 to Columns.VisibleCount - 1 do
      Provider.SetColumnWidth(I, Columns.Delta[I]);
    for I := 0 to Rows.VisibleCount - 1 do
      Provider.SetRowHeight(I, Rows.Delta[I]);
  end;
end;

{ TcxGridExportProviderAdapter }

constructor TcxGridExportProviderAdapter.Create(ACache: TcxGridExportProviderCache);
begin
  inherited Create;
  FBoundingRect := cxNullRect;
  FProviderCache := ACache;
end;

constructor TcxGridExportProviderAdapter.CreateParented(AParentAdapter: IcxGridExportProviderAdapter; const AClientOrigin: TPoint);
begin
  inherited Create;
  FParentAdapter := AParentAdapter;
  FClientOrigin := AClientOrigin;
  FBoundingRect := cxNullRect;
end;

function TcxGridExportProviderAdapter.GetBoundingRect: TRect;
begin
  Result := FBoundingRect;
end;

function TcxGridExportProviderAdapter.GetDimensions: TRect;
begin
  RealBoundsToLogicalBounds(FBoundingRect, Result);
end;

function TcxGridExportProviderAdapter.GetFormatCode(ACol, ARow: Integer): string;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.GetFormatCode(ACol, ARow)
  else
    Result := FProviderCache.Provider.GetFormatCode(ACol, ARow);
end;

function TcxGridExportProviderAdapter.GetStyle(AStyleIndex: Integer): TcxCacheCellStyle;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.GetStyle(AStyleIndex)
  else
    Result := FProviderCache.Provider.GetStyle(AStyleIndex);
end;

function TcxGridExportProviderAdapter.IsEmpty: Boolean;
begin
  Result := cxRectIsEmpty(GetBoundingRect);
end;

function TcxGridExportProviderAdapter.IsGraphicsSupported: Boolean;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.IsGraphicsSupported
  else
    Result := FProviderCache.Provider.SupportGraphic;
end;

function TcxGridExportProviderAdapter.IsRTFSupported: Boolean;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.IsRTFSupported
  else
    Result := FProviderCache.Provider.SupportRTF;
end;

function TcxGridExportProviderAdapter.RegisterStyle(const AStyle: TcxCacheCellStyle): Integer;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.RegisterStyle(AStyle)
  else
    Result := FProviderCache.Provider.RegisterStyle(AStyle);
end;

function TcxGridExportProviderAdapter.SetCellGraphic(const AGlobalArea: TRect; AStyleIndex: Integer;
  AGraphic: TGraphic; AFitMode: TcxImageFitMode): TObject;
begin
  if FParentAdapter <> nil then
    Result := FParentAdapter.SetCellGraphic(AGlobalArea, AStyleIndex, AGraphic, AFitMode)
  else
    Result := FProviderCache.Provider.SetCellGraphic(AGlobalArea, AStyleIndex, AGraphic, AFitMode);
end;

procedure TcxGridExportProviderAdapter.SetCellGraphicAsSharedHandle(const AGlobalArea: TRect; AStyleIndex: Integer;
  AHandle: TObject; AFitMode: TcxImageFitMode);
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetCellGraphicAsSharedHandle(AGlobalArea, AStyleIndex, AHandle, AFitMode)
  else
    FProviderCache.Provider.SetCellGraphicAsSharedHandle(AGlobalArea, AStyleIndex, AHandle, AFitMode);
end;

procedure TcxGridExportProviderAdapter.SetCellStyle(const AGlobalCol, AGlobalRow, AStyleIndex: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetCellStyle(AGlobalCol, AGlobalRow, AStyleIndex)
  else
    FProviderCache.Provider.SetCellStyle(AGlobalCol, AGlobalRow, AStyleIndex);
end;

procedure TcxGridExportProviderAdapter.SetCellStyle(const AArea: TRect; const AStyleIndex: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetCellStyle(AArea, AStyleIndex)
  else
    FProviderCache.Provider.SetCellStyle(AArea, AStyleIndex);
end;

procedure TcxGridExportProviderAdapter.SetCellValue(const AGlobalCol, AGlobalRow: Integer;
  const AValue: Variant; const AValueDisplayFormat: string; AValueDisplayFormatType: TcxValueDisplayFormatType);
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetCellValue(AGlobalCol, AGlobalRow, AValue, AValueDisplayFormat, AValueDisplayFormatType)
  else
    FProviderCache.Provider.SetCellValue(AGlobalCol, AGlobalRow, AValue, AValueDisplayFormat, AValueDisplayFormatType);
end;

procedure TcxGridExportProviderAdapter.SetCellValueAsFormula(const AGlobalCol, AGlobalRow: Integer; const AValue: string;
  ADisplayText: string = ''; AFormatCode: string = ''; AListSeparator: Char = ',');
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetCellValueAsFormula(AGlobalCol, AGlobalRow, AValue, ADisplayText, AFormatCode, AListSeparator)
  else
    FProviderCache.Provider.SetCellValueAsFormula(AGlobalCol, AGlobalRow, AValue, ADisplayText, AFormatCode, AListSeparator);
end;

procedure TcxGridExportProviderAdapter.SetDefaultStyle(const AStyle: TcxCacheCellStyle);
begin
  if FParentAdapter <> nil then
    FParentAdapter.SetDefaultStyle(AStyle)
  else
    FProviderCache.Provider.SetDefaultStyle(AStyle);
end;

procedure TcxGridExportProviderAdapter.AddBounds(const ABounds: TRect);
begin
  AddRangeHorz(ABounds.Left, ABounds.Right);
  AddRangeVert(ABounds.Top, ABounds.Bottom);
end;

procedure TcxGridExportProviderAdapter.AddOutlineGroup(AStart, AFinish: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.AddOutlineGroup(AStart, AFinish)
  else
    FProviderCache.Provider.AddOutlineGroup(AStart, AFinish);
end;

procedure TcxGridExportProviderAdapter.AddRangeHorz(X1, X2: Integer);
begin
  FBoundingRect.Left := Min(FBoundingRect.Left, Min(X1, X2));
  FBoundingRect.Right := Max(FBoundingRect.Right, Max(X1, X2));

  if FParentAdapter <> nil then
    FParentAdapter.AddRangeHorz(X1 + FClientOrigin.X, X2 + FClientOrigin.X)
  else
    FProviderCache.Columns.AddPairs(X1, X2);
end;

procedure TcxGridExportProviderAdapter.AddRangeVert(Y1, Y2: Integer);
begin
  FBoundingRect.Bottom := Max(FBoundingRect.Bottom, Max(Y1, Y2));
  FBoundingRect.Top := Min(FBoundingRect.Top, Min(Y1, Y2));

  if FParentAdapter <> nil then
    FParentAdapter.AddRangeVert(Y1 + FClientOrigin.Y, Y2 + FClientOrigin.Y)
  else
    FProviderCache.Rows.AddPairs(Y1, Y2);
end;

procedure TcxGridExportProviderAdapter.ApplyBestFit;
begin
  if FParentAdapter <> nil then
    FParentAdapter.ApplyBestFit
  else
    FProviderCache.Provider.ApplyBestFit;
end;

procedure TcxGridExportProviderAdapter.FreezePanes(ACol, ARow: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.FreezePanes(ACol, ARow)
  else
    FProviderCache.Provider.FreezePanes(ACol, ARow);
end;

procedure TcxGridExportProviderAdapter.RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
begin
  RealRangeToLogicalRangeHorz(ABounds.Left, ABounds.Right, ALogicalBounds.Left, ALogicalBounds.Right);
  RealRangeToLogicalRangeVert(ABounds.Top, ABounds.Bottom, ALogicalBounds.Top, ALogicalBounds.Bottom);
end;

procedure TcxGridExportProviderAdapter.RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
begin
  if FParentAdapter <> nil then
    FParentAdapter.RealBoundsToLogicalBoundsEx(cxRectOffset(ABounds, FClientOrigin), ASearchArea, ALogicalBounds)
  else
  begin
    FProviderCache.Columns.GetPositionEx(ABounds.Left, ABounds.Right,
      ASearchArea.Left, ASearchArea.Right, ALogicalBounds.Left, ALogicalBounds.Right);
    FProviderCache.Rows.GetPositionEx(ABounds.Top, ABounds.Bottom,
      ASearchArea.Top, ASearchArea.Bottom, ALogicalBounds.Top, ALogicalBounds.Bottom);
  end;
end;

procedure TcxGridExportProviderAdapter.RealRangeToLogicalRangeHorz(X1, X2: Integer; out ALogicalX1, ALogicalX2: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.RealRangeToLogicalRangeHorz(X1 + FClientOrigin.X, X2 + FClientOrigin.X, ALogicalX1, ALogicalX2)
  else
    FProviderCache.Columns.GetPosition(X1, X2, ALogicalX1, ALogicalX2);
end;

procedure TcxGridExportProviderAdapter.RealRangeToLogicalRangeVert(Y1, Y2: Integer; out ALogicalY1, ALogicalY2: Integer);
begin
  if FParentAdapter <> nil then
    FParentAdapter.RealRangeToLogicalRangeVert(Y1 + FClientOrigin.Y, Y2 + FClientOrigin.Y, ALogicalY1, ALogicalY2)
  else
    FProviderCache.Rows.GetPosition(Y1, Y2, ALogicalY1, ALogicalY2);
end;

{ TcxGridCustomExport }

constructor TcxGridCustomExport.Create(const AFileName: string; AExportType: Integer; AGridView: TcxCustomGridView;
  AGrid: TcxCustomGrid; AViewInfo: TcxCustomGridViewInfo; AHandler: TObject = nil);
var
  AExportProvider: IcxExportProvider;
begin
  AExportProvider := TcxExport.Provider(AExportType, AFileName) as IcxExportProvider;
  FProviderCache := TcxGridExportProviderCache.Create(AExportProvider);
  FProviderCacheOwner := True;
  Create(AFileName, AGridView, AGrid, AViewInfo, TcxGridExportProviderAdapter.Create(FProviderCache), AHandler);
end;

constructor TcxGridCustomExport.Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
  AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil);
begin
  inherited Create;
  FAdapter := AAdapter;
  FGrid := AGrid;
  FGridView := AGridView;
  FDefaultRowHeight := cxDefaultRowHeight;
  FFileName := AFileName;
  FViewInfo := AViewInfo;
  FHandler := AHandler;

  FRecordsList := TList.Create;
  FProgressHelper := TcxProgressCalculationHelper.Create(3, AGrid, ProgressHandler);
end;

constructor TcxGridCustomExport.CreateFrom(AMasterExport: TcxGridCustomExport;
  AGridView: TcxCustomGridView; AViewInfo: TcxCustomGridViewInfo; const AClientOrigin: TPoint);
begin
  Create(AMasterExport.FileName, AGridView, AMasterExport.Grid, AViewInfo,
    TcxGridExportProviderAdapter.CreateParented(AMasterExport.Adapter, AClientOrigin));
  Assign(AMasterExport);
end;

destructor TcxGridCustomExport.Destroy;
begin
  FAdapter := nil;
  FreeAndNil(FProgressHelper);
  if FProviderCacheOwner then
    FreeAndNil(FProviderCache);
  FreeAndNil(FRecordsList);
  inherited Destroy;
end;

procedure TcxGridCustomExport.AddSeparators(const ASeparators: array of string);
var
  AExportWithSeparators: IcxExportWithSeparators;
  I: Integer;
begin
  if ProviderCache <> nil then
    if Supports(ProviderCache.Provider, IcxExportWithSeparators, AExportWithSeparators) then
    begin
      for I := 0 to High(ASeparators) do
        AExportWithSeparators.AddSeparator(ASeparators[I]);
    end;
end;

procedure TcxGridCustomExport.Assign(ASource: TcxGridCustomExport);
begin
  if ASource = nil then
    Exit;
  FProviderCache := ASource.ProviderCache;
  FExpand := ASource.Expand;
  FSaveAll := ASource.FSaveAll;
  FUseNativeFormat := ASource.FUseNativeFormat;
end;

procedure TcxGridCustomExport.SetEncoding(AEncoding: TEncoding);
var
  AProvider: IcxExportProviderEncoding;
begin
  if ProviderCache <> nil then
  begin
    if Supports(ProviderCache.Provider, IcxExportProviderEncoding, AProvider) then
      AProvider.SetEncoding(AEncoding);
  end;
end;

procedure TcxGridCustomExport.SetName(const AName: string);
var
  AIntf: IcxNameExportProvider;
begin
  if ProviderCache <> nil then
  begin
    if Supports(ProviderCache.Provider, IcxNameExportProvider, AIntf) then
      AIntf.SetName(AName);
  end;
end;

procedure TcxGridCustomExport.DoExport;
begin
  Initialize;
  try
    CreateExportCells;
    ExportCells;
  finally
    Finalize;
  end;
  BeforeCommit;
  ProviderCache.Commit(ProgressHelper, FHandler);
end;

procedure TcxGridCustomExport.BeforeCommit;
begin
end;

function TcxGridCustomExport.CalculateViewViewInfo(AGridView: TcxCustomGridView; ABounds: TRect): TcxCustomGridViewInfo;
begin
  Result := nil;
  if AGridView is TcxCustomGridTableView then
  begin
    Result := AGridView.CreateViewInfo;
    with Result as TcxCustomGridTableViewInfo do
    begin
      FirstRecordIndex := 0;
      PixelScrollRecordOffset := 0;
      MainCalculate(ABounds);
    end;
  end;
end;

function TcxGridCustomExport.CheckNativeValue(AProperties: TcxCustomEditProperties;
  AItem: TcxCustomGridTableItem; const AValue: Variant): Variant;
begin
  if VarIsSoftEmpty(AValue) then
    Exit(Null);

  try
    if IsCurrencyItem(AItem) and IsCurrencyProperties(AProperties) or (AProperties is TcxCurrencyEditProperties) then
      VarCast(Result, AValue, varCurrency)
    else

    if AProperties is TcxDateEditProperties then
      VarCast(Result, AValue, varDate)
    else

    if (VarType(AValue) = varCurrency) and not IsCurrencyItem(AItem) and not (AProperties is TcxCurrencyEditProperties) then
      VarCast(Result, AValue, varDouble)
    else
      Result := AValue;
  except
    on EVariantError do
      Result := AValue
    else
      raise;
  end;
end;

procedure TcxGridCustomExport.CreateExportCells;
begin
  ExtractRowsForExport;
  RegisterStyles;
  DoCreateExportCells;
end;

procedure TcxGridCustomExport.DoCreateExportCells;
begin
  // do nothing
end;

procedure TcxGridCustomExport.DoExportCells;
begin
  FillArea(Adapter.GetDimensions, DefaultStyleIndex);
end;

procedure TcxGridCustomExport.ExportCells;
begin
  if ProviderCache <> nil then
    ProviderCache.PrepareToExport(DataOnly);
  if not IsEmpty then
    DoExportCells;
end;

procedure TcxGridCustomExport.Finalize;
begin
  if FSaveGridModeFlag and (DataController is TcxDBDataController) then
  begin
    TcxDBDataController(DataController).DataModeController.GridMode := FSaveGridModeFlag;
    DataController.RestoreDataSetPos;
  end;
end;

procedure TcxGridCustomExport.Initialize;
begin
  FSaveGridModeFlag := DataController.IsGridMode;
  if FSaveGridModeFlag and (DataController is TcxDBDataController) then
  begin
    DataController.SaveDataSetPos;
    TcxDBDataController(DataController).DataModeController.GridMode := False;
  end;
end;

function TcxGridCustomExport.CanUseNativeFormatProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := not ((AProperties is TcxCustomLookupEditProperties) or (AProperties is TdxSparklineProperties) or
    (AProperties is TcxCustomImageComboBoxProperties));
end;

procedure TcxGridCustomExport.ExpandRecords(AFullExpand: Boolean);
var
  I: Integer;
  ARecord: TcxCustomGridRecord;
  AViewData: TcxCustomGridTableViewData;
begin
  if not GridView.InheritsFrom(TcxCustomGridTableView) then
    Exit;

  AViewData := (GridView as TcxCustomGridTableView).ViewData;
  if AFullExpand then
    AViewData.Expand(True)
  else
  begin
    I := 0;
    while I < AViewData.RecordCount - 1 do
    begin
      ARecord := AViewData.Records[I];
      if ARecord.Selected and ARecord.Expandable and not ARecord.Expanded then
        ARecord.Expanded := True
      else
        Inc(I);
    end;
  end;
end;

function TcxGridCustomExport.ExportImagesAsGraphic: Boolean;
begin
  Result := cxGridExportLink.ExportImagesAsGraphic;
end;

procedure TcxGridCustomExport.ExtractRowsForExport;

  procedure ExpandRecords(AViewData: TcxCustomGridTableViewData);
  var
    AIndex: Integer;
    ARecord: TcxCustomGridRecord;
    AView: TcxCustomGridView;
  begin
    AIndex := 0;
    while AIndex < AViewData.RecordCount do
    begin
      ARecord := AViewData.Records[AIndex];
      if NeedExpandRecord(ARecord) then
      begin
        if ARecord is TcxGridMasterDataRow then
        begin
          AView := TcxGridMasterDataRow(ARecord).ActiveDetailGridView;
          if (AView <> nil) and (AView.ViewData is TcxCustomGridTableViewData) then
            ExpandRecords(TcxCustomGridTableViewData(AView.ViewData));
        end;
        ARecord.Expanded := True;
      end;
      Inc(AIndex);
    end;
  end;

var
  AIndex: Integer;
  ARecord: TcxCustomGridRecord;
  AViewData: TcxCustomGridTableViewData;
begin
  if not (GridView.ViewData is TcxCustomGridTableViewData) then
    Exit;

  AViewData := TcxCustomGridTableViewData(GridView.ViewData);
  SaveAll := SaveAll or DataOnly or not HasSelectedRecords;
  Expand := Expand or DataOnly or (not SaveAll and GridView.DataController.MultiSelectionSyncGroupWithChildren);

  Grid.BeginUpdate;
  try
    if Expand then
      ExpandRecords(AViewData);

    AIndex := 0;
    while AIndex < AViewData.RecordCount do
    begin
      ARecord := AViewData.Records[AIndex];
      if NeedProcessRecord(ARecord) then
        RecordsList.Add(Pointer(AIndex));
      Inc(AIndex);
    end;

    for AIndex := 0 to RecordCount - 1 do
      RecordsList[AIndex] := AViewData.Records[Integer(RecordsList[AIndex])];
  finally
    Grid.EndUpdate;
  end;
end;

procedure TcxGridCustomExport.FillArea(const ABounds: TRect; AStyleIndex: Integer;
  ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
var
  AStyle: TcxCacheCellStyle;
  I, J, AActualStyleIndex: Integer;

  procedure SetBorderStyle(ASide: TcxBorder);
  begin
    AStyle.Borders[Integer(ASide)].IsDefault := False;
    AStyle.Borders[Integer(ASide)].Width := 1;
    AStyle.Borders[Integer(ASide)].Color := ColorToRgb(ABorderColor);
  end;

begin
  for I := ABounds.Top to ABounds.Bottom - 1 do
    for J := ABounds.Left to ABounds.Right - 1 do
    begin
      AActualStyleIndex := AStyleIndex;
      if (ABorderColor <> clDefault) and (ABorders <> []) then
      begin
        AStyle := Adapter.GetStyle(AStyleIndex);
        if J = ABounds.Left then
          SetBorderStyle(bLeft);
        if I = ABounds.Top then
          SetBorderStyle(bTop);
        if J = ABounds.Right - 1 then
          SetBorderStyle(bRight);
        if I = ABounds.Bottom - 1 then
          SetBorderStyle(bBottom);
        AActualStyleIndex := Adapter.RegisterStyle(AStyle);
      end;
      Adapter.SetCellStyle(J, I, AActualStyleIndex);
    end;
end;

procedure TcxGridCustomExport.FillRealArea(const ABounds: TRect; AStyleIndex: Integer;
  ABorderColor: TColor = clDefault; ABorders: TcxBorders = cxBordersAll);
var
  ALogicalBounds: TRect;
begin
  RealBoundsToLogicalBounds(ABounds, ALogicalBounds);
  FillArea(ALogicalBounds, AStyleIndex, ABorderColor, ABorders);
end;

function TcxGridCustomExport.GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
  out AParams: TcxViewParams; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault): TcxCacheCellStyle;
begin
  AItem.Styles.GetContentParams(ARecord, AParams);
  ViewParamsToExportStyle(AParams, Result, AItem.GetProperties.Alignment.Horz, ABorders, ABorderColor);
end;

function TcxGridCustomExport.GetDataOnly: Boolean;
begin
  Result := False;
end;

function TcxGridCustomExport.GetViewItemValue(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem): Variant;
var
  AValueDisplayFormat: string;
  AValueDisplayFormatType: TcxValueDisplayFormatType;
begin
  Result := GetViewItemValueEx(ARecord, AItem, AValueDisplayFormat, AValueDisplayFormatType);
end;

function TcxGridCustomExport.GetViewItemValueEx(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
  out AValueDisplayFormat: string; out AValueDisplayFormatType: TcxValueDisplayFormatType): Variant;
var
  AProperties: TcxCustomEditProperties;
  AString: string;
begin
  AValueDisplayFormat := '';
  AValueDisplayFormatType := vdftAuto;
  AProperties := AItem.GetProperties(ARecord);
  if IsNativeFormatProperties(AProperties, AItem) then
  begin
    Result := CheckNativeValue(AProperties, AItem, ARecord.Values[AItem.Index]);
    AValueDisplayFormat := cxGetDisplayFormat(AProperties, GetField(AItem));
    AValueDisplayFormatType := cxGetDisplayFormatType(AProperties, GetField(AItem));
  end
  else
  begin
    if AProperties.GetEditValueSource(False) = evsValue then
      AString := AProperties.GetDisplayText(ARecord.Values[AItem.Index], True)
    else
      AString := ARecord.DisplayTexts[AItem.Index];
    TcxCustomGridTableItemAccess.DoGetDisplayText(AItem, ARecord, AString);
    Result := AString;
  end;
end;

function TcxGridCustomExport.HasSelectedRecords: Boolean;
begin
  Result := False;
end;

function TcxGridCustomExport.IsCurrencyItem(AItem: TcxCustomGridTableItem): Boolean;
var
  AField: TField;
begin
  Result := AItem.DataBinding.ValueTypeClass = TcxCurrencyValueType;
  if GridView.DataController is TcxGridDBDataController then
  begin
    AField := TcxGridDBDataController(GridView.DataController).GetItemField(AItem.Index);
    if AField is TFloatField then
      Result := TFloatField(AField).Currency
    else

    if AField is TBCDField then
      Result := TBCDField(AField).Currency
    else

    if AField is TAggregateField then
      Result := TAggregateField(AField).Currency
    else

    if AField is TFMTBCDField then
      Result := TFMTBCDField(AField).Currency
  end;
end;

function TcxGridCustomExport.IsCurrencyProperties(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := ((AProperties is TcxMaskEditProperties) or (AProperties is TcxCalcEditProperties) or
    (AProperties is TcxCurrencyEditProperties)) and UseNativeFormat;
end;

function TcxGridCustomExport.IsEmpty: Boolean;
begin
  Result := Adapter.IsEmpty;
end;

function TcxGridCustomExport.IsNativeFormatProperties(AProperties: TcxCustomEditProperties; AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := DataOnly and CanUseNativeFormatProperties(AProperties) or (AProperties is TcxDateEditProperties) or (AProperties is TcxSpinEditProperties)
     or (AProperties is TcxTimeEditProperties) or IsCurrencyProperties(AProperties);
  Result := (Result and UseNativeFormat) or (IsRTFSupported(AProperties) and Adapter.IsRTFSupported);
end;

function TcxGridCustomExport.NeedExpandRecord(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := Expand and not ARecord.Expanded and (SaveAll or not ARecord.IsData and
    ARecord.GridView.DataController.GroupContainsSelectedRows(ARecord.Index));
end;

function TcxGridCustomExport.NeedProcessRecord(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := SaveAll or ARecord.Selected;
end;

procedure TcxGridCustomExport.RealBoundsToLogicalBounds(const ABounds: TRect; out ALogicalBounds: TRect);
begin
  Adapter.RealBoundsToLogicalBounds(ABounds, ALogicalBounds);
end;

procedure TcxGridCustomExport.RealBoundsToLogicalBoundsEx(const ABounds, ASearchArea: TRect; out ALogicalBounds: TRect);
begin
  Adapter.RealBoundsToLogicalBoundsEx(ABounds, ASearchArea, ALogicalBounds);
end;

function TcxGridCustomExport.RegisterContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams): Integer;
begin
  Result := Adapter.RegisterStyle(GetContentParams(ARecord, AItem, AParams));
end;

function TcxGridCustomExport.RegisterSolidStyle(AStyleIndex: Integer; AData: TObject = nil): Integer;
var
  AParams: TcxViewParams;
begin
  AParams.Color := clNone;
  if GridView is TcxCustomGridTableView then
    TcxCustomGridTableView(GridView).Styles.GetViewParams(AStyleIndex, AData, nil, AParams);
  Result := RegisterSolidStyleEx(AParams.Color);
end;

procedure TcxGridCustomExport.RegisterStyles;
var
  AViewParams: TcxViewParams;
begin
  if GridView is TcxCustomGridTableView then
    with TcxCustomGridTableView(GridView).Styles do
    begin
      GetViewParams(vsBackground, nil, Background, AViewParams);
      ViewParamsToExportStyle(AViewParams, FDefaultStyle);
    end;
  Adapter.SetDefaultStyle(DefaultStyle);
  DefaultStyleIndex := Adapter.RegisterStyle(DefaultStyle);
end;

function TcxGridCustomExport.RegisterSolidStyleEx(AColor: TColor): Integer;
var
  AStyle: TcxCacheCellStyle;
begin
  AStyle := DefaultStyle;
  AStyle.BrushBkColor := ColorToRgb(AColor);
  Result := Adapter.RegisterStyle(AStyle);
end;

function TcxGridCustomExport.RegisterViewParams(const AViewParams: TcxViewParams; const AAlignment: TAlignment = taLeftJustify): Integer;
var
  AStyle: TcxCacheCellStyle;
begin
  ViewParamsToExportStyle(AViewParams, AStyle, AAlignment);
  Result := Adapter.RegisterStyle(AStyle)
end;

procedure TcxGridCustomExport.SetRealCellStyle(const ARealBounds, ASearchArea: TRect; AStyleIndex: Integer);
begin
  SetRealCellStyleAndValue(ARealBounds, ASearchArea, AStyleIndex, Null);
end;

procedure TcxGridCustomExport.SetRealCellStyleAndFormula(const ARealBounds, ASearchArea: TRect; AStyleIndex: Integer;
  const AFormula: string; ADisplayText: string = ''; AFormatCode: string = ''; AListSeparator: Char = ',');
var
  R: TRect;
begin
  RealBoundsToLogicalBoundsEx(ARealBounds, ASearchArea, R);
  Adapter.SetCellValueAsFormula(R.Left, R.Top, AFormula, ADisplayText, AFormatCode, AListSeparator);
  Adapter.SetCellStyle(R, AStyleIndex);
end;

procedure TcxGridCustomExport.SetRealCellStyleAndValue(const ARealBounds, ASearchArea: TRect;
  AStyleIndex: Integer; const AValue: Variant; const AValueDisplayFormat: string = '');
var
  R: TRect;
begin
  RealBoundsToLogicalBoundsEx(ARealBounds, ASearchArea, R);
  if not VarIsNull(AValue) then
    Adapter.SetCellValue(R.Left, R.Top, AValue, AValueDisplayFormat, vdftAuto);
  Adapter.SetCellStyle(R, AStyleIndex);
end;

procedure TcxGridCustomExport.SetRealCellStyleAndValueEx(const ARealBounds, ASearchArea: TRect;
  AStyleIndex: Integer; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem);
var
  AValue: Variant;
  AValueDisplayFormat: string;
  AValueDisplayFormatType: TcxValueDisplayFormatType;
  R: TRect;
begin
  RealBoundsToLogicalBoundsEx(ARealBounds, ASearchArea, R);
  if AItem <> nil then
  begin
    if not (Adapter.IsGraphicsSupported and TrySetCellValueAsGraphic(ARealBounds, R, AStyleIndex, ARecord, AItem)) then
    begin
      AValue := GetViewItemValueEx(ARecord, AItem, AValueDisplayFormat, AValueDisplayFormatType);
      Adapter.SetCellValue(R.Left, R.Top, AValue, AValueDisplayFormat, AValueDisplayFormatType);
    end;
  end;
  Adapter.SetCellStyle(R, AStyleIndex);
end;

function TcxGridCustomExport.TrySetCellValueAsGraphic(const ABounds, AArea: TRect;
  AStyleIndex: Integer; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem): Boolean;

  procedure ProcessCellAsImage(AProperties: TcxImagePropertiesAccess);
  var
    AClientRect, R: TRect;
    APicture: TPicture;
    AFitMode: TcxImageFitMode;
  begin
    APicture := TPicture.Create;
    try
      LoadPicture(APicture, AProperties.GetGraphicClass(AItem, ARecord.Index), ARecord.Values[AItem.Index]);
      if (APicture.Graphic <> nil) and not cxRectIsEmpty(ABounds) then
      begin
        R := cxRectBounds(0, 0, APicture.Width, APicture.Height);
        if DataOnly then
        begin
          AFitMode := ifmStretch;
          AClientRect := R;
        end
        else
        begin
          AFitMode := AProperties.FitMode;
          AClientRect := cxRectSetNullOrigin(ABounds);
          if AProperties.GetRealStretch(cxSize(APicture.Width, APicture.Height), cxRectSize(AClientRect)) then
          begin
            if AProperties.Proportional then
              R := cxRectProportionalStretch(AClientRect, APicture.Width, APicture.Height)
            else
              R := AClientRect;
          end;
          if AProperties.Center then
            R := cxRectCenter(AClientRect, cxRectWidth(R), cxRectHeight(R));
        end;
        Adapter.SetCellGraphic(AArea, DefaultStyleIndex, GraphicToPNGImage(AClientRect, R, APicture.Graphic), AFitMode);
      end;
    finally
      APicture.Free;
    end;
  end;

  function ProcessSparkLinesAsImage(AProperties: TdxSparklineProperties): Boolean;
  var
    R: TRect;
    AGraphic: TcxBitmap32;
    AViewData: TcxCustomEditViewData;
    AViewInfo: TdxSparkLineViewInfo;
  begin
    Result := not DataOnly;
    if Result then
    begin
      R := cxRectSetNullOrigin(ABounds);
      AGraphic := TcxBitmap32.CreateSize(R);
      try
        AViewData := TcxTableItemAccess(AItem).CreateEditViewData(AProperties);
        try
          AViewInfo := AProperties.GetViewInfoClass.Create as TdxSparkLineViewInfo;
          try
            AViewData.EditValueToDrawValue(ARecord.Values[AItem.Index], AViewInfo);
            AViewData.Calculate(AGraphic.cxCanvas, R, cxNullPoint, cxmbNone, [], AViewInfo, False);
            AViewInfo.Paint(AGraphic.cxCanvas);
          finally
            AViewInfo.Free;
          end;
        finally
          AViewData.Free;
        end;
        Adapter.SetCellGraphic(AArea, DefaultStyleIndex, GraphicToPNGImage(R, R, AGraphic));
      finally
        AGraphic.Free;
      end;
    end;
  end;

  function ProcessCellValueAsImageComboBox(AProperties: TcxImageComboBoxProperties): Boolean;
  var
    ABitmap: TcxBitmap;
    AImageIndex: TcxImageIndex;
    ARect: TRect;
    AText: string;
    AHandle: TObject;
    AFitMode: TcxImageFitMode;
  begin
    Result := False;
    if (AProperties.Images <> nil) and not AProperties.ShowDescriptions then
    begin
      Result := True;
      AProperties.GetImageComboBoxDisplayValue(ARecord.Values[AItem.Index], AText, AImageIndex);
      if AImageIndex >= 0 then
      begin
        if DataOnly then
          AFitMode := ifmStretch
        else
          AFitMode := ifmNormal;
        if ProviderCache.Provider.TryGetSharedImageHandle(AProperties.Images, AImageIndex, AHandle) then
           Adapter.SetCellGraphicAsSharedHandle(AArea, DefaultStyleIndex, AHandle, AFitMode)
        else
        begin
          ABitmap := TcxBitmap.CreateSize(AProperties.Images.Width, AProperties.Images.Height, pf24bit);
          try
            ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, Adapter.GetStyle(AStyleIndex).BrushBkColor);
            AProperties.Images.Draw(ABitmap.Canvas, 0, 0, AImageIndex);
            if DataOnly then
              ARect := cxRectBounds(0, 0, ABitmap.Width, ABitmap.Height)
            else
              ARect := cxRectSetNullOrigin(ABounds);
            AHandle := Adapter.SetCellGraphic(AArea, DefaultStyleIndex,
              GraphicToPNGImage(ARect, cxRectCenter(ARect, ABitmap.Width, ABitmap.Height), ABitmap), AFitMode);
            ProviderCache.Provider.AddSharedImageHandle(AProperties.Images, AImageIndex, AHandle);
          finally
            ABitmap.Free;
          end;
        end;
      end;
    end;
  end;

begin
  Result := ExportImagesAsGraphic;
  if Result then
  begin
    if AItem.GetProperties is TcxCustomImageProperties then
      ProcessCellAsImage(TcxImagePropertiesAccess(AItem.GetProperties))
    else
      if AItem.GetProperties is TcxCustomImageComboBoxProperties then
        Result := ProcessCellValueAsImageComboBox(TcxImageComboBoxProperties(AItem.GetProperties))
      else
        if AItem.GetProperties is TdxSparklineProperties then
          Result := ProcessSparkLinesAsImage(TdxSparklineProperties(AItem.GetProperties))
        else
          Result := False;
  end;
end;

function TcxGridCustomExport.TextHeight(AFont: TFont): Integer;
begin
  Result := cxTextHeight(AFont) + cxTextOffset * 2;
end;

function TcxGridCustomExport.TextHeightEx(const AViewParams: TcxViewParams): Integer;
begin
  Result := TextHeight(AViewParams.Font);
end;

function TcxGridCustomExport.TextWidth(AFont: TFont; const AText: string): Integer;
begin
  Result := cxTextWidth(AFont, AText) + cxTextOffset * 2;
end;

function TcxGridCustomExport.TextWidthEx(const AViewParams: TcxViewParams; const AText: string): Integer;
begin
  Result := TextWidth(AViewParams.Font, AText);
end;

procedure TcxGridCustomExport.ViewParamsToExportStyle(const AViewParams: TcxViewParams;
  var AExportStyle: TcxCacheCellStyle; const AAlignment: TAlignment = taLeftJustify;
  ABorders: TcxBorders = []; ABorderColor: TColor = clDefault);
var
  I: Integer;
  ABkColor: TColor;
begin
  AExportStyle := DefaultCellStyle;
  if DataOnly then
    ABkColor := clWindow
  else
    ABkColor := AViewParams.Color;
  AExportStyle.BrushBkColor := ColorToRGB(ABkColor);
  AExportStyle.FontColor := ColorToRGB(AViewParams.TextColor);
  AExportStyle.FontName := AViewParams.Font.Name;
  AExportStyle.FontStyle := AViewParams.Font.Style;
  AExportStyle.FontSize := AViewParams.Font.Size;
  AExportStyle.FontCharset := Integer(AViewParams.Font.Charset);
  AExportStyle.AlignText := AlignmentToExportAlignment[AAlignment];
  for I := 0 to 3 do
  begin
    AExportStyle.Borders[I].IsDefault := (ABorderColor = clDefault) or not (TcxBorder(I) in ABorders);
    AExportStyle.Borders[I].Width := BorderWidths[not AExportStyle.Borders[I].IsDefault];
    if not AExportStyle.Borders[I].IsDefault then
      AExportStyle.Borders[I].Color := ColorToRgb(ABorderColor);
  end;
end;

function TcxGridCustomExport.GetDataController: TcxCustomDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridCustomExport.GetExpandButtonSize: Integer;
begin
  Result := ViewInfo.LookAndFeelPainter.ScaledExpandButtonSize(dxGetScaleFactor(Grid));
end;

function TcxGridCustomExport.GetField(AItem: TcxCustomGridTableItem): TField;
begin
  if AItem.DataBinding is TcxGridItemDBDataBinding then
    Result := TcxGridItemDBDataBinding(AItem.DataBinding).Field
  else
    Result := nil;
end;

function TcxGridCustomExport.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GridView.LookAndFeelPainter;
end;

function TcxGridCustomExport.GetRecord(ARecordIndex: Integer): TcxCustomGridRecord;
begin
  Result := TcxCustomGridRecord(RecordsList[ARecordIndex]);
end;

function TcxGridCustomExport.GetRecordCount: Integer;
begin
  Result := RecordsList.Count;
end;

function TcxGridCustomExport.GetViewInfo: TcxCustomGridViewInfo;
begin
  if FViewInfo = nil then
    FViewInfo := GridView.ViewInfo;
  Result := FViewInfo;
end;

procedure TcxGridCustomExport.ProgressHandler(Sender: TObject; Percents: Integer);
var
  AIntf: IcxExportProgress;
begin
  if Supports(FHandler, IcxExportProgress, AIntf) then
    AIntf.OnProgress(Sender, Percents);
end;

{ TcxExportCustomItem }

function TcxExportCustomItem.GetBoundsRelativeTo(ATop, ALeft: Integer): TRect;
begin
  Result := Bounds;
  OffsetRect(Result, 0, ATop);
  Result.Left := Max(Result.Left, ALeft);
  Result.Right := Max(Result.Right, ALeft);
end;

{ TcxExportVisualItem }

destructor TcxExportVisualItem.Destroy;
begin
  DestroyDetailViewViewInfo;
  DestroyDetailExport;
  inherited Destroy;
end;

procedure TcxExportVisualItem.DestroyDetailExport;
begin
  FreeAndNil(FDetailExport);
end;

procedure TcxExportVisualItem.DestroyDetailViewViewInfo;
begin
  FreeAndNil(FDetailViewViewInfo);
end;

function TcxExportVisualItem.IsColumn: Boolean;
begin
  Result := Data is TcxGridColumn;
end;

{ TcxGridCustomTableViewExport }

function TcxGridCustomTableViewExport.AddVisualDataItem(const AItemBounds: TRect; AStyle: Integer; ARecord: TcxCustomGridRecord;
  AGridItem: TcxCustomGridTableItem): TcxExportVisualItem;
begin
  Result := AddVisualItem(TcxExportVisualItem, AItemBounds);
  Result.Style := AStyle;
  Result.Data := ARecord;
  Result.Data2 := AGridItem;
end;

function TcxGridCustomTableViewExport.AddVisualItem(AItemClass: TcxExportVisualItemClass;
  const ABounds: TRect): TcxExportVisualItem;
begin
  Result := AItemClass.Create;
  Result.Style := -1;
  Result.Bounds := ABounds;
  Result.ItemBorderColor := clBtnShadow;
  Adapter.AddBounds(ABounds);
  if VisualItemsList.Capacity - VisualItemsList.Count < 2 then
    VisualItemsList.Capacity := VisualItemsList.Count * 2;
  VisualItemsList.Add(Result);
end;

function TcxGridCustomTableViewExport.AddVisualItemEx(const AItemBounds: TRect; const ADisplayText: string;
  const AViewParams: TcxViewParams; AAlignment: TAlignment; ABorders: TcxBorders; ABorderColor: TColor = clDefault;
  AIsBackground: Boolean = False; AWordWrap: Boolean = True): TcxExportVisualItem;
var
  ASide: TcxBorder;
  AStyle: TcxCacheCellStyle;
begin
  ViewParamsToExportStyle(AViewParams, AStyle, AAlignment);
  if ABorderColor <> clDefault then
  begin
    ABorderColor := ColorToRgb(ABorderColor);
    for ASide := bLeft to bBottom do
      if ASide in ABorders then
      begin
        AStyle.Borders[Integer(ASide)].IsDefault := False;
        AStyle.Borders[Integer(ASide)].Color := ABorderColor;
        AStyle.Borders[Integer(ASide)].Width := 1;
      end;
  end;
  AStyle.SingleLine := not AWordWrap;
  Result := AddVisualItemEx(AItemBounds, ADisplayText, Adapter.RegisterStyle(AStyle), AIsBackground);
end;

function TcxGridCustomTableViewExport.AddVisualItemEx(const AItemBounds: TRect; const ADisplayText: string;
  AStyle: Integer; AIsBackground: Boolean = False): TcxExportVisualItem;
begin
  Result := AddVisualItem(TcxExportVisualItem, AItemBounds);
  Result.Bounds := AItemBounds;
  Result.IsBackground := AIsBackground;
  Result.DisplayText := ADisplayText;
  Result.Style := AStyle;
end;

function TcxGridCustomTableViewExport.CanExportVisualItem(AItem: TcxExportVisualItem): Boolean;
begin
  Result := not (AItem.Slave or AItem.Hidden or cxRectIsEmpty(AItem.Bounds));
end;

procedure TcxGridCustomTableViewExport.DoExportCells;
var
  AItem: TcxExportVisualItem;
  I: Integer;
  ASearchArea: TRect;
begin
  inherited DoExportCells;
  ASearchArea := Adapter.GetDimensions;
  ProgressHelper.BeginStage(VisualItemCount);
  try
    for I := 0 to VisualItemCount - 1 do
    begin
      AItem := VisualItems[I];
      if CanExportVisualItem(AItem) then
        ExportVisualItem(AItem, ASearchArea);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridCustomTableViewExport.ExportBackgroundVisualItem(AItem: TcxExportVisualItem);
begin
  FillRealArea(AItem.Bounds, AItem.Style, AItem.ItemBorderColor, AItem.ItemBorders);
end;

procedure TcxGridCustomTableViewExport.ExportVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect);
begin
  if AItem.Hidden then
    Exit;
  if AItem.IsBackground then
    ExportBackgroundVisualItem(AItem)
  else
    if IsDataVisualItem(AItem) then
      ExportDataVisualItem(AItem, ASearchArea)
    else
      SetRealCellStyleAndValue(AItem.Bounds, ASearchArea, AItem.Style, AItem.DisplayText);
end;

procedure TcxGridCustomTableViewExport.ExportDataVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect);
var
  ARecord: TcxCustomGridRecord;
  AGridItem: TcxCustomGridTableItem;
begin
  AGridItem := TcxCustomGridTableItem(AItem.Data2);
  ARecord := AItem.Data as TcxCustomGridRecord;
  SetRealCellStyleAndValueEx(AItem.Bounds, ASearchArea, AItem.Style, ARecord, AGridItem);
end;

function TcxGridCustomTableViewExport.GetVisualItemsListCapacity: Integer;
begin
  Result := cxVisualItemsListCapacity;
end;

function TcxGridCustomTableViewExport.HasSelectedRecords: Boolean;
begin
  Result := GridView.Controller.SelectedRecordCount > 0;
end;

function TcxGridCustomTableViewExport.IsDataVisualItem(AItem: TcxExportVisualItem): Boolean;
begin
  Result := AItem.Data2 is TcxCustomGridTableItem;
end;

procedure TcxGridCustomTableViewExport.Initialize;
begin
  inherited Initialize;
  FVisualItemsList := TcxObjectList.Create;
  FVisualItemsList.Capacity := GetVisualItemsListCapacity;
end;

procedure TcxGridCustomTableViewExport.Finalize;
begin
  FreeAndNil(FVisualItemsList);
  inherited Finalize;
end;

function TcxGridCustomTableViewExport.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridCustomTableViewExport.GetVisualItem(AIndex: Integer): TcxExportVisualItem;
begin
  Result := VisualItemsList[AIndex] as TcxExportVisualItem;
end;

function TcxGridCustomTableViewExport.GetVisualItemCount: Integer;
begin
  Result := VisualItemsList.Count;
end;

{ TcxExportGroupSummaryItem }

procedure TcxExportGroupSummaryItem.InitWidth;
begin
  Bounds.Right := Min(Bounds.Right, Bounds.Left + cxTextWidth(ViewParams.Font, Text) + cxTextOffset * 3);
end;

{ TcxGridTableViewExport }

procedure TcxGridTableViewExport.AddDataRow(var ATop, ALeft: Integer; ARow: TcxGridDataRow);
var
  AHeight: Integer;
  ARecordViewInfoClass: TcxCustomGridRecordViewInfoClass;
  AViewInfo: TcxGridDataRowViewInfo;
begin
  AHeight := RecordHeight;
  if ARow is TcxGridMasterDataRow then
    ARecordViewInfoClass := TcxGridDataRowViewInfo
  else
    ARecordViewInfoClass := TDataRowAccess(ARow).GetViewInfoClass;

  AViewInfo := ARecordViewInfoClass.Create(ViewInfo.RecordsViewInfo, ARow) as TcxGridDataRowViewInfo;
  try
    TcxCustomGridRecordViewInfoAccess(AViewInfo).CacheItem.IsHeightAssigned := False;
    try
      AViewInfo.Recalculate;
      RecordHeight := AViewInfo.DataHeight;
      if AViewInfo.HasPreview then
        Dec(FRecordHeight, AViewInfo.PreviewViewInfo.Height);

      if ARow.FixedState = rfsNotFixed then
        ALeft := AddIndents(ATop, RecordHeight, ARow, ARow.Expandable);
      ScalePatterns(RecordHeight, DefaultRowHeight);
      if (PreviewPlace = ppTop) and AViewInfo.HasPreview and (AViewInfo.PreviewViewInfo.Height > 0) then
        AddRowPreview(ATop, ALeft, ARow);

      CreateRecordFromPattern(ATop, ALeft, ARow);
      Inc(ATop, RecordHeight);
      if (PreviewPlace = ppBottom) and AViewInfo.HasPreview and (AViewInfo.PreviewViewInfo.Height > 0) then
        AddRowPreview(ATop, ALeft, ARow);
    finally
      TcxCustomGridRecordViewInfoAccess(AViewInfo).CacheItem.IsHeightAssigned := False;
    end;
  finally
    AViewInfo.Free;
    ScalePatterns(AHeight, RecordHeight div RecordRowCount);
  end;
end;

procedure TcxGridTableViewExport.AddFooterCell(ABounds: TRect; AColumn: TcxGridColumn; AFooterItem: TcxDataSummaryItem;
  AValue: Variant; const AParams: TcxViewParams);
var
  ADisplayText: string;
  AFooterCell: TcxExportVisualItem;
begin
  ADisplayText := AFooterItem.FormatValue(AValue, True);
  AFooterCell := AddVisualItemEx(ABounds, ADisplayText, AParams, AColumn.FooterAlignmentHorz,
    cxBordersAll, FooterCellBorderColor);
  if UseNativeFormat and (VarIsNumeric(AValue) or VarIsFMTBcd(AValue)) then
  begin
    AFooterCell.Data2 := AFooterItem;
    AFooterCell.Value := AValue;
    if UseNativeFormat and (AColumn <> nil) and (AFooterItem.Format = '') then
      AFooterCell.Value := CheckNativeValue(AColumn.GetProperties, AColumn, AValue);
  end;
end;

procedure TcxGridTableViewExport.AddFooterCells(var ATop, ALeft: Integer);
var
  AParams: TcxViewParams;
  APatternIndex, ACount, ACellCount: Integer;
  R: TRect;
begin
  Styles.GetFooterParams(nil, nil, -1, nil, AParams);
  FooterHeight := cxTextHeight(AParams.Font) + 2 * cxTextOffset;
  R := Rect(ALeft, ATop, RecordWidth, ATop);
  Inc(R.Bottom, GetFooterRowCount * (FooterHeight + cxGridFooterCellIndent));
  ATop := R.Bottom;
  AddVisualItemEx(R, '', AParams, taLeftJustify, [], clDefault, True);
  ACellCount := GetFooterCellCount;
  for APatternIndex := 0 to PatternCount - 1 do
  begin
    if ACellCount = 0 then
      Break;
    if Patterns[APatternIndex].IsColumn then
    begin
      AddFooterCellsByColumn(R, TcxGridColumn(Patterns[APatternIndex].Data), APatternIndex, ACount);
      Dec(ACellCount, ACount);
    end;
  end;
end;

procedure TcxGridTableViewExport.AddFooterCellsByColumn(ABounds: TRect; AColumn: TcxGridColumn; APatternIndex: Integer;
  out ACount: Integer);
var
  I: Integer;
  AItem: TcxDataSummaryItem;
  AParams: TcxViewParams;
begin
  ACount := 0;
  for I := 0 to DataController.Summary.FooterSummaryItems.Count - 1 do
  begin
    AItem := DataController.Summary.FooterSummaryItems[I];
    if not IsColumnContainsFooterItem(AItem, AColumn) then
      Continue;
    GridView.Styles.GetFooterCellParams(nil, AColumn, -1, AItem, AParams);
    AddFooterCell(GetFooterItemBounds(APatternIndex, ACount, ABounds), AColumn, AItem,
      DataController.Summary.FooterSummaryValues[I], AParams);
    Inc(ACount);
    if not CanShowMultiSummaries then
      Break;
  end;
end;

procedure TcxGridTableViewExport.AddGroupFooterCells(var ATop, ALeft: Integer; ARow: TcxCustomGridRow);
var
  AParams: TcxViewParams;
  APatternIndex, ACellCount, ABottom, ACount: Integer;
  R: TRect;
begin
  Styles.GetFooterParams(ARow, nil, ARow.Level, nil, AParams);
  FooterHeight := cxTextHeight(AParams.Font) + 2 * cxTextOffset;
  ABottom := ATop + GetGroupFooterRowCount(ARow) * (FooterHeight + cxGridFooterCellIndent);
  ALeft := AddIndents(ATop, ABottom - ATop, ARow, False, ARow.Level - 1);
  R := Rect(ALeft, ATop, RecordWidth, ABottom);
  ATop := R.Bottom;
  AddVisualItemEx(R, '', AParams, taLeftJustify, [], clDefault, True);
  ACellCount := GetGroupFooterCellCount(ARow);
  for APatternIndex := 0 to PatternCount - 1 do
  begin
    if ACellCount = 0 then
      Break;
    if Patterns[APatternIndex].IsColumn then
    begin
      AddGroupFooterCellsByColumn(R, ARow, TcxGridColumn(Patterns[APatternIndex].Data), APatternIndex, ACount);
      Dec(ACellCount, ACount);
    end;
  end;
end;

procedure TcxGridTableViewExport.AddGroupFooterCellsByColumn(ABounds: TRect; ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
  APatternIndex: Integer; out ACount: Integer);
var
  ASummaryItemIndex, ALevelGroupedItemIndex: Integer;
  AItem: TcxDataSummaryItem;
  AItems: TcxDataSummaryItems;
  AValues: PVariant;
  AParams: TcxViewParams;
begin
  ACount := 0;
  for ALevelGroupedItemIndex := 0 to DataController.Groups.GetLevelGroupedItemCount(ARow.Level) - 1 do
  begin
    if not DataController.Summary.GetGroupSummaryInfo(ARow.Index, AItems, AValues, ALevelGroupedItemIndex) then
      Continue;
    for ASummaryItemIndex := 0 to AItems.Count - 1 do
    begin
      AItem := AItems[ASummaryItemIndex];
      if not IsColumnContainsFooterItem(AItem, AColumn) then
        Continue;
      GridView.Styles.GetFooterCellParams(ARow, AColumn, ARow.Level, AItem, AParams);
      AddFooterCell(GetFooterItemBounds(APatternIndex, ACount, ABounds, True), AColumn, AItem,
        AValues^[ASummaryItemIndex], AParams);
      Inc(ACount);
      if not CanShowMultiSummaries(True) then
        Break;
    end;
  end;
end;

procedure TcxGridTableViewExport.AddGroupRow(var ATop, ALeft: Integer; ARow: TcxGridGroupRow);
var
  R: TRect;
  S: string;
  AValues: PVariant;
  I, AHeight: Integer;
  AViewParams: TcxViewParams;
  AItems: TcxDataSummaryItems;
  AItem: TcxExportGroupSummaryItem;
  AExportItem: TcxExportVisualItem;
begin
  AHeight := ViewInfo.RecordsViewInfo.GroupRowHeight;
  R := Rect(0, ATop, RecordWidth, ATop + AHeight);
  R.Left := AddIndents(ATop, AHeight, ARow, True);
  Styles.GetGroupParams(ARow, ARow.Level, AViewParams);
  AddVisualItemEx(R, '', AViewParams, taLeftJustify, [], clDefault, True);
  if IsSummaryUnderColumns[ARow] and ARow.GetGroupSummaryInfo(AItems, AValues) then
  begin
    GroupSummaryItemsList.Clear;
    GroupSummaryItemsList.Capacity := 1024;
    for I := 0 to AItems.Count - 1 do
      ProcessGroupSummaryItem(ARow, AValues, I, R, AViewParams);
    GroupSummaryItemsList.Sort(@cxCompareGroupSummaryItems);
    ProcessGroupSummaryItems(ARow, R);
    for I := 0 to GroupSummaryItemCount - 1 do
    begin
      AItem := GroupSummaryItems[I];
      AExportItem := AddVisualItemEx(AItem.Bounds, AItem.Text, AItem.ViewParams,
        AItem.Alignment, GridLines - [bLeft, bRight], GridLineColor, False);
      if UseNativeFormat and not VarIsNull(AItem.Value) then
      begin
        AExportItem.Data2 := AItem.SummaryItem;
        AExportItem.Value := AItem.Value;
      end;
    end;
  end
  else
  begin
    S := ARow.DisplayText;
    for I := 0 to ARow.GroupedColumnCount - 1 do
      TcxCustomGridTableItemAccess.DoGetDisplayText(ARow.GroupedColumns[I], ARow, S);
    AddVisualItemEx(R, S, AViewParams, taLeftJustify, GridLines, GridLineColor, False);
  end;
  Inc(ATop, AHeight);
end;

function TcxGridTableViewExport.AddIndents(ATop, ARowHeight: Integer;
  ARow: TcxCustomGridRecord; AHasButton: Boolean; ALevel: Integer = -1): Integer;
var
  R: TRect;
  I: Integer;
  AText: string;
  AStyle: TcxCacheCellStyle;
  AGridLines: TcxBorders;
  AViewParams: TcxViewParams;
begin
  if ALevel = -1 then
    ALevel := ARow.Level - 1 + Byte(AHasButton);
  while (ALevel >= 0) and (ALevel < ARow.Level) do
    ARow := TcxGridDataRow(ARow).ParentRecord;
  R := Rect(ALevel * ViewInfo.LevelIndent, ATop, (ALevel + 1) * ViewInfo.LevelIndent, ATop + ARowHeight);
  AGridLines := GridLines;
  if not ARow.IsLast or AHasButton and ARow.Expanded then
    AGridLines := GridLines - [bBottom];
  for I := ALevel downto 0 do
  begin
    AText := '';
    if AHasButton and (I = ALevel) then
    begin
      AText := ButtonTexts[ARow.Expanded];
      AStyle := GetExpandButtonParams(AGridLines);
    end
    else
    begin
      if ARow is TcxGridGroupRow then
        Styles.GetGroupParams(ARow, I, AViewParams)
      else
        Styles.GetContentParams(ARow, nil, AViewParams);

      ViewParamsToExportStyle(AViewParams, AStyle, taLeftJustify, AGridLines - [bTop], GridLineColor);
    end;
    ARow := ARow.ParentRecord;
    AddVisualItemEx(R, AText, Adapter.RegisterStyle(AStyle), False);
    OffsetRect(R, -ViewInfo.LevelIndent, 0);
  end;
  Result := (ALevel + 1) * ViewInfo.LevelIndent;
end;

procedure TcxGridTableViewExport.AddMasterDataRow(var ATop, ALeft: Integer; ARow: TcxGridMasterDataRow);
var
  ABounds: TRect;
  ADetailHeight: Integer;
  AVisualItem: TcxExportVisualItem;
begin
  AddDataRow(ATop, ALeft, ARow);
  if ARow.Expanded then
  begin
    ABounds := Rect((ARow.Level + 1) * ViewInfo.LevelIndent, ATop, RecordWidth, ATop + DefaultRowHeight);
    AVisualItem := AddVisualDataItem(ABounds, DefaultStyleIndex, ARow, nil);
    AVisualItem.DetailViewViewInfo := CalculateViewViewInfo(ARow.ActiveDetailGridView, ViewInfo.Bounds);
    AVisualItem.DetailExport := GetExportClassByGridView(ARow.ActiveDetailGridView).CreateFrom(
      Self, ARow.ActiveDetailGridView, AVisualItem.DetailViewViewInfo, ABounds.TopLeft);
    AVisualItem.DetailExport.Initialize;
    AVisualItem.DetailExport.CreateExportCells;

    ADetailHeight := cxRectHeight(AVisualItem.DetailExport.Adapter.GetBoundingRect);
    AddIndents(ATop, ADetailHeight, ARow, False, ARow.Level);
    ATop := ABounds.Top + ADetailHeight;
    ALeft := ABounds.Left;
  end;
end;

procedure TcxGridTableViewExport.AddRowFooter(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord; ALevel: Integer);
begin
  ALeft := 0;
  while ARow.Level > ALevel do
    ARow := ARow.ParentRecord;
  AddGroupFooterCells(ATop, ALeft, TcxCustomGridRow(ARow));
end;

procedure TcxGridTableViewExport.AddRowFooters(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord);
var
  ALevel, ARealLevel: Integer;
begin
  for ALevel := 0 to ARow.Level do
  begin
    ARealLevel := ALevel;
    if TcxGridExportHelper.HasGroupFooter(GridView, ARow, ARealLevel) then
      AddRowFooter(ATop, ALeft, ARow, ARealLevel);
  end;
end;

procedure TcxGridTableViewExport.AddRowPreview(var ATop, ALeft: Integer; ARow: TcxCustomGridRecord);
var
  AColumn: TcxGridColumn;
  AParams: TcxViewParams;
  R: TRect;
begin
  AColumn := GridView.Preview.Column;
  AColumn.Styles.GetContentParams(ARow, AParams);
  R := Rect(ALeft, ATop, RecordWidth, ATop + GetPreviewHeight(ARow));
  AddVisualItemEx(R, VarToStr(GetViewItemValue(ARow, AColumn)), AParams, AColumn.GetProperties.Alignment.Horz, GridLines, GridLineColor);
  ATop := R.Bottom;
end;

function TcxGridTableViewExport.AddPattern(const ABounds: TRect;
  AData: TObject; AOffset: Integer = 0): TcxExportVisualItem;
begin
  Result := TcxExportVisualItem.Create;
  Result.Bounds := ABounds;
  Result.Data := AData;
  Result.Data2 := TObject(AOffset);
  PatternsList.Add(Result);
end;

function TcxGridTableViewExport.AddVisualItem(AItemClass: TcxExportVisualItemClass; const ABounds: TRect): TcxExportVisualItem;
begin
  Result := inherited AddVisualItem(AItemClass, ABounds);
  Result.ItemBorders := GridLines;
end;

function TcxGridTableViewExport.CalculateFooterCellCount(AItems: TcxDataSummaryItems): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AItems = nil then Exit;
  for I := 0 to AItems.Count - 1 do
    if AItems[I].Position = spFooter then
      Inc(Result);
end;

function TcxGridTableViewExport.CanShowMultiSummaries(AIsRowFooter: Boolean = False): Boolean;
begin
  if AIsRowFooter then
    Result := OptionsView.CanShowGroupFooterMultiSummaries
  else
    Result := OptionsView.CanShowFooterMultiSummaries;
end;

procedure TcxGridTableViewExport.CreateContent(var ATop, ALeft: Integer);
var
  ARow: TcxCustomGridRecord;
  I: Integer;
begin
  ProgressHelper.BeginStage(RecordCount);
  try
    for I := 0 to RecordCount - 1 do
    begin
      ALeft := 0;
      ARow := Records[I];
      if ARow is TcxGridGroupRow then
        AddGroupRow(ATop, ALeft, TcxGridGroupRow(ARow))
      else

      if ARow is TcxGridMasterDataRow then
        AddMasterDataRow(ATop, ALeft, TcxGridMasterDataRow(ARow))
      else

      if ARow is TcxGridDataRow then
        AddDataRow(ATop, ALeft, TcxGridDataRow(ARow));

      AddRowFooters(ATop, ALeft, ARow);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridTableViewExport.CreateFooter(var ATop, ALeft: Integer);
begin
  if OptionsView.Footer then
    AddFooterCells(ATop, ALeft);
end;

procedure TcxGridTableViewExport.CreateHeader(var ATop, ALeft: Integer);
begin
  FRecordWidth := ViewInfo.DataWidth;
  ProduceHeadersContainer(ATop, ALeft, ViewInfo.HeaderViewInfo);
end;

procedure TcxGridTableViewExport.DoCreateExportCells;
var
  ATop, ALeft: Integer;
begin
  ATop := 0;
  ALeft := 0;
  CreateHeader(ATop, ALeft);
  CreateContent(ATop, ALeft);
  ALeft := 0;
  CreateFooter(ATop, ALeft);
  MergeCells;
end;

function TcxGridTableViewExport.CanExportVisualItem(AItem: TcxExportVisualItem): Boolean;
begin
  Result := (AItem.DetailExport <> nil) or inherited CanExportVisualItem(AItem);
end;

procedure TcxGridTableViewExport.CreateRecordFromPattern(var ATop, ALeft: Integer; ARecord: TcxCustomGridRecord);
var
  R: TRect;
  I: Integer;
  APattern: TcxExportVisualItem;
  AViewParams: TcxViewParams;
  AStyle: TcxCacheCellStyle;
begin
  for I := 0 to PatternCount - 1 do
  begin
    APattern := Patterns[I];
    R := APattern.GetBoundsRelativeTo(ATop, ALeft);
    if APattern.IsColumn then
    begin
      AStyle := GetContentParams(ARecord, TcxGridColumn(APattern.Data), AViewParams, GridLines, GridLineColor);
      AddVisualDataItem(R, Adapter.RegisterStyle(AStyle), ARecord, TcxGridColumn(APattern.Data));
    end
    else
      AddVisualItemEx(R, '', GetPatternParams(ARecord, APattern), taLeftJustify, [], clDefault, True);
  end;
end;

function TcxGridTableViewExport.DoMergeCell(AMasterItem, ASlaveItem: TcxExportVisualItem): Boolean;
var
  AColumn: TcxGridColumn;
  AMasterValue, ASlaveValue: Variant;
  AMasterProperties, ASlaveProperties: TcxCustomEditProperties;
  ABounds: TRect;
begin
  Result := AMasterItem <> ASlaveItem;
  with AMasterItem.Bounds do
  begin
    Result := Result and not TcxGridDataRow(AMasterItem.Data).Expanded and
      (Bottom = ASlaveItem.Bounds.Top) and (Left = ASlaveItem.Bounds.Left) and
      (Right = ASlaveItem.Bounds.Right);
  end;
  if Result then
  begin
    AColumn := TcxGridColumn(AMasterItem.Data2);
    AMasterValue := TcxGridDataRow(AMasterItem.Data).Values[AColumn.Index];
    AMasterProperties := AColumn.GetProperties(TcxGridDataRow(AMasterItem.Data));
    ASlaveValue := TcxGridDataRow(ASlaveItem.Data).Values[AColumn.Index];
    ASlaveProperties := AColumn.GetProperties(TcxGridDataRow(ASlaveItem.Data));
    Result := AColumn.DoCompareValuesForCellMerging(TcxGridDataRow(AMasterItem.Data), AMasterProperties,
      AMasterValue, TcxGridDataRow(ASlaveItem.Data), ASlaveProperties,  ASlaveValue);
  end;
  if Result then
  begin
    ABounds := AMasterItem.Bounds;
    ABounds.Bottom := ASlaveItem.Bounds.Bottom;
    AMasterItem.Bounds := ABounds;
    ASlaveItem.Hidden := True;
    ASlaveItem.Slave := True;
  end;
end;

procedure TcxGridTableViewExport.ExportDataVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect);
var
  ASummaryItem: TcxDataSummaryItem;
begin
  if AItem.Data2 is TcxDataSummaryItem then
  begin
    ASummaryItem := TcxDataSummaryItem(AItem.Data2);
    if Assigned(ASummaryItem.OnGetText) then
      SetRealCellStyleAndValue(AItem.Bounds, ASearchArea, AItem.Style, AItem.DisplayText)
    else
      SetRealCellStyleAndValue(AItem.Bounds, ASearchArea, AItem.Style, AItem.Value, ASummaryItem.Format);
  end
  else
    inherited ExportDataVisualItem(AItem, ASearchArea);
end;

procedure TcxGridTableViewExport.ExportDetailVisualItem(AItem: TcxExportVisualItem);
begin
  try
    AItem.DetailExport.ExportCells;
  finally
    AItem.DetailExport.Finalize;
    AItem.DestroyDetailExport;
    AItem.DestroyDetailViewViewInfo;
  end;
end;

procedure TcxGridTableViewExport.ExportVisualItem(AItem: TcxExportVisualItem; const ASearchArea: TRect);
begin
  if AItem.DetailExport <> nil then
    ExportDetailVisualItem(AItem)
  else
    inherited ExportVisualItem(AItem, ASearchArea);
end;

procedure TcxGridTableViewExport.Finalize;
begin
  FreeAndNil(FGroupSummaryItemsList);
  FreeAndNil(FPatternsList);
  inherited Finalize;
end;

function TcxGridTableViewExport.GetColumnOffset(AColumn: TcxGridColumn): Integer;
begin
  Result := LeftPos - IndicatorWidth;
end;

function TcxGridTableViewExport.GetExpandButtonParams(ABorders: TcxBorders): TcxCacheCellStyle;
var
  ABorder: TcxBorder;
begin
  Result := DefaultCellStyle;
  for ABorder := bLeft to bBottom do
    if ABorder in ABorders then
    begin
      Result.Borders[Integer(ABorder)].IsDefault := False;
      Result.Borders[Integer(ABorder)].Width := 1;
      Result.Borders[Integer(ABorder)].Color := ColorToRgb(GridLineColor);
    end
    else
    begin
      Result.Borders[Integer(ABorder)].IsDefault := True;
      Result.Borders[Integer(ABorder)].Width := 0;
    end;
  Result.BrushBkColor := ColorToRgb(clBtnFace);
end;

function TcxGridTableViewExport.GetFooterCellCount: Integer;
begin
  Result := CalculateFooterCellCount(DataController.Summary.FooterSummaryItems);
end;

function TcxGridTableViewExport.GetFooterItemBounds(AIndex, ALineIndex: Integer; const AOrigin: TRect;
  AIsRowFooter: Boolean = False): TRect;
var
  H, LCount: Integer;
begin
  with Patterns[AIndex] do
  begin
    H := FooterHeight + cxGridFooterCellIndent;
    Result.Left := Max(Bounds.Left, AOrigin.Left);
    Result.Right := Min(Bounds.Right, AOrigin.Right);
    Result.Top := AOrigin.Top + (Bounds.Top div DefaultRowHeight + ALineIndex) * H;
    LCount := 1;
    if not CanShowMultiSummaries(AIsRowFooter) then
      LCount := Max(1, (Bounds.Bottom - Bounds.Top) div DefaultRowHeight);
    Result.Bottom := Result.Top + LCount * H;
    InflateRect(Result, -cxGridFooterCellIndent, -cxGridFooterCellIndent);
  end;
end;

function TcxGridTableViewExport.GetFooterRowCount: Integer;
begin
  if CanShowMultiSummaries then
    Result := TcxGridExportHelper.CalculateFooterLineCount(DataController.Summary.FooterSummaryItems)
  else
    Result := RecordRowCount;
end;

function TcxGridTableViewExport.GetGroupFooterCellCount(ARow: TcxCustomGridRow): Integer;
var
  I: Integer;
  AItems: TcxDataSummaryItems;
  AValues: PVariant;
begin
  Result := 0;
  for I := 0 to DataController.Groups.GetLevelGroupedItemCount(ARow.Level) - 1 do
    if DataController.Summary.GetGroupSummaryInfo(ARow.Index, AItems, AValues, I) then
      Inc(Result, CalculateFooterCellCount(AItems));
end;

function TcxGridTableViewExport.GetGroupFooterRowCount(ARow: TcxCustomGridRow): Integer;

  function CalculateRowCount(ARow: TcxCustomGridRow): Integer;
  var
    I: Integer;
    AItems: TcxDataSummaryItems;
    AValues: PVariant;
  begin
    Result := 0;
    for I := 0 to DataController.Groups.GetLevelGroupedItemCount(ARow.Level) - 1 do
      if DataController.Summary.GetGroupSummaryInfo(ARow.Index, AItems, AValues, I) then
        Inc(Result, TcxGridExportHelper.CalculateFooterLineCount(AItems));
  end;

begin
  if not CanShowMultiSummaries(True) then
    Result := RecordRowCount
  else
    Result := CalculateRowCount(ARow);
end;

function TcxGridTableViewExport.GetGroupRowColumnIntersection(const ARowBounds: TRect; AColumn: TcxCustomGridTableItem): TRect;
var
  I: Integer;
begin
  Result := ARowBounds;
  if AColumn = nil then Exit;
  for I := 0 to PatternCount - 1 do
  begin
    if Patterns[I].Data = AColumn then
    begin
      Result.Left := Max(Result.Left, Patterns[I].Bounds.Left);
      Result.Right := Min(Result.Right, Patterns[I].Bounds.Right);
      Break;
    end;
  end;
end;

function TcxGridTableViewExport.GetIsSummaryUnderColumns(ARow: TcxGridGroupRow): Boolean;
begin
  Result := (OptionsView.GroupSummaryLayout <> gslStandard) and (ARow.GroupedColumnCount = 1);
end;

function TcxGridTableViewExport.GetPatternParams(
  ARecord: TcxCustomGridRecord; AItem: TcxExportVisualItem): TcxViewParams;
begin
  if AItem.IsColumn then
    TcxGridColumn(AItem.Data).Styles.GetContentParams(ARecord, Result)
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TcxGridTableViewExport.GetPreviewHeight(ARow: TcxCustomGridRecord): Integer;
var
  R: TRect;
  AMaxLineCount: Integer;
  AColumn: TcxGridColumn;
  AParams: TcxViewParams;
begin
  if HasPreview[ARow] then
  begin
    AMaxLineCount := GridView.Preview.MaxLineCount;
    Result := AMaxLineCount * DefaultRowHeight;
    if GridView.Preview.AutoHeight then
    begin
      AColumn := GridView.Preview.Column;
      AColumn.Styles.GetContentParams(ARow, AParams);
      R := Rect(0, 0, RecordWidth, Result);
      with TcxScreenCanvas.Create do
      try
        TextExtent(VarToStr(GetViewItemValue(ARow, AColumn)), R, cxWordBreak and not cxSingleLine);
      finally
        Free;
      end;
      if AMaxLineCount = 0 then
        Result := cxRectHeight(R) + cxTextOffset * 3
      else
        Result := Min(Result, cxRectHeight(R) + cxTextOffset * 3);
    end;
  end
  else
    Result := 0;
end;

function TcxGridTableViewExport.GetVisualItemsListCapacity: Integer;
begin
  if GridView.IsDetail then
    Result := inherited GetVisualItemsListCapacity
  else
    Result := cxVisualItemsListCapacityForMasterDetails;
end;

function TcxGridTableViewExport.HasDetailViewSelectedRecords(AView: TcxCustomGridView): Boolean;
begin
  Result := (AView is TcxCustomGridTableView) and ((TcxCustomGridTableView(AView).Controller.SelectedRecordCount > 0) or
    AView.IsMaster and HasMasterViewSelectedChildren(TcxGridTableView(AView)));
end;

function TcxGridTableViewExport.HasMasterRowSelectedChildren(AMasterRow: TcxGridMasterDataRow): Boolean;
begin
  Result := AMasterRow.Expanded and (AMasterRow.ActiveDetailGridView <> nil) and
    HasDetailViewSelectedRecords(AMasterRow.ActiveDetailGridView);
end;

function TcxGridTableViewExport.HasMasterViewSelectedChildren(AView: TcxGridTableView): Boolean;
var
  I: Integer;
  ARow: TcxCustomGridRow;
begin
  Result := False;
  for I := 0 to AView.ViewData.RecordCount - 1 do
  begin
    ARow := AView.ViewData.Rows[I];
    if ARow.IsData and HasMasterRowSelectedChildren(TcxGridMasterDataRow(ARow)) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TcxGridTableViewExport.Initialize;
begin
  inherited Initialize;
  FRecordRowCount := 1;
  FPatternsList := TcxObjectList.Create;
  FGroupSummaryItemsList := TcxObjectList.Create;
  FGroupSummaryItemsList.Capacity := 1024;
end;

function TcxGridTableViewExport.IsColumnContainsFooterItem(AItem: TcxDataSummaryItem; AColumn: TcxGridColumn): Boolean;
begin
  Result := (AItem.ItemLink = AColumn) and (AItem.Position = spFooter);
end;

function TcxGridTableViewExport.IsDataVisualItem(AItem: TcxExportVisualItem): Boolean;
begin
  Result := inherited IsDataVisualItem(AItem) or (AItem.Data2 is TcxDataSummaryItem);
end;

procedure TcxGridTableViewExport.MergeCells;
var
  I, J, AMasterRowIndex, ASlaveRowIndex: Integer;
  AColumn: TcxGridColumn;
  AMergedRowsList: TList;
begin
  AMergedRowsList := TList.Create;
  try
    for I := 0 to PatternCount - 1 do
    begin
      if not Patterns[I].IsColumn then Continue;
      AColumn := TcxGridColumn(Patterns[I].Data);
      if not AColumn.Options.CellMerging then Continue;
      AMergedRowsList.Clear;
      for J := 0 to VisualItemCount - 1 do
      begin
        if VisualItems[J].Data2 = AColumn then
          AMergedRowsList.Add(VisualItems[J]);
      end;
      AMasterRowIndex := 0;
      while AMasterRowIndex < AMergedRowsList.Count do
      begin
        ASlaveRowIndex := AMasterRowIndex + 1;
        while ASlaveRowIndex < AMergedRowsList.Count do
        begin
          if DoMergeCell(TcxExportVisualItem(AMergedRowsList[AMasterRowIndex]),
            TcxExportVisualItem(AMergedRowsList[ASlaveRowIndex])) then
            Inc(ASlaveRowIndex)
          else
            Break;
        end;
        AMasterRowIndex := ASlaveRowIndex;
      end;
    end;
  finally
    AMergedRowsList.Free;
  end;
end;

function TcxGridTableViewExport.NeedProcessRecord(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := inherited NeedProcessRecord(ARecord) or ARecord.GridView.IsMaster and ARecord.IsData and
    HasMasterRowSelectedChildren(TcxGridMasterDataRow(ARecord));
end;

procedure TcxGridTableViewExport.ProcessGroupSummaryItem(ARow: TcxGridGroupRow;
  AValues: PVariant; AIndex: Integer; const ABounds: TRect; const ARowViewParams: TcxViewParams);
var
  ADisplayText: string;
  ADisplayValue: Variant;
  AItem: TcxExportGroupSummaryItem;
  ASummaryItem: TcxGridTableSummaryItem;
begin
  ASummaryItem := TcxGridTableSummaryItem(ARow.GroupSummaryItems[AIndex]);
  if (ASummaryItem.Column <> nil) and not ASummaryItem.Column.ActuallyVisible then
    Exit;

  ADisplayText := ASummaryItem.FormatValue(AValues^[AIndex], False);
  ADisplayValue := AValues^[AIndex];
  if (ASummaryItem.Position <> spGroup) or (ADisplayText = '') then
    Exit;

  AItem := TcxExportGroupSummaryItem.Create;
  AItem.Column := ASummaryItem.Column;
  if (AItem.Column <> nil) and (AItem.Column.VisibleIndex < 0) then
    AItem.Column := nil;
  if (AItem.Column <> nil) then
    AItem.Alignment := ASummaryItem.Column.GroupSummaryAlignment;

  if AItem.Column <> nil then
  begin
    Styles.GetGroupSummaryCellContentParams(ARow, ASummaryItem, AItem.ViewParams);
    AItem.ViewParams.Color := ARowViewParams.Color;
    AItem.ViewParams.Bitmap := ARowViewParams.Bitmap;
  end
  else
    AItem.ViewParams := ARowViewParams;

  AItem.Bounds := GetGroupRowColumnIntersection(ABounds, AItem.Column);
  AItem.Text := ADisplayText;
  AItem.SummaryItem := ASummaryItem;
  AItem.Value := ADisplayValue;
  AItem.Index := AIndex;
  GroupSummaryItemsList.Add(AItem);
end;

procedure TcxGridTableViewExport.ProcessGroupSummaryItems(ARow: TcxGridGroupRow; ABounds: TRect);
var
  I: Integer;
  AItem: TcxExportGroupSummaryItem;
  AItems: TcxDataGroupSummaryItems;
begin
  I := 0;
  AItems := ARow.GroupSummaryItems;
  while I < GroupSummaryItemCount do
  begin
    AItem := GroupSummaryItems[I];
    if (I < (GroupSummaryItemCount - 1)) and
      (AItem.Column = GroupSummaryItems[I + 1].Column) then
    begin
      AItem.Text := AItem.Text + AItems.Separator + ' ' + GroupSummaryItems[I + 1].Text;
      AItem.Value := Null;
      GroupSummaryItemsList[I + 1].Free;
      GroupSummaryItemsList.Delete(I + 1);
      Continue;
    end
    else
      Inc(I);
  end;
  if (GroupSummaryItemCount > 0) and (GroupSummaryItems[0].Column = nil) then
  begin
    GroupSummaryItems[0].Text := ARow.DisplayCaption +
      AItems.BeginText + GroupSummaryItems[0].Text + AItems.EndText;
    GroupSummaryItems[0].Value := Null;
  end
  else
  begin
    AItem := TcxExportGroupSummaryItem.Create;
    AItem.Text := ARow.DisplayCaption;
    if GroupSummaryItemCount = 0 then
    begin
      for I := 0 to ARow.GroupedColumnCount - 1 do
        TcxCustomGridTableItemAccess.DoGetDisplayText(ARow.GroupedColumns[I], ARow, AItem.Text);
      if AItem.Text <> ARow.DisplayCaption then
        AItem.Value := Null;
    end;
    Styles.GetGroupParams(ARow, ARow.Level, AItem.ViewParams);
    AItem.Bounds := ABounds;
    GroupSummaryItemsList.Insert(0, AItem);
  end;
  GroupSummaryItemsList.Sort(@cxCompareGroupSummaryItems);
  I := 0;
  while I <= GroupSummaryItemCount - 2 do
  begin
    if (I = 0) and (GroupSummaryItems[I].Column = nil) then
      GroupSummaryItems[I].InitWidth;
    with GroupSummaryItems[I] do
    begin
      if (I = 0) and (Column = nil) and (Bounds.Left >= GroupSummaryItems[I + 1].Bounds.Left) then
        GroupSummaryItems[I + 1].Bounds.Left := Bounds.Right
      else
        Bounds.Right := Min(Bounds.Right, GroupSummaryItems[I + 1].Bounds.Left);
      if Bounds.Left >= Bounds.Right then
        GroupSummaryItemsList.Delete(I)
      else
        Inc(I);
    end;
  end;
end;

procedure TcxGridTableViewExport.ProduceHeadersContainer(var ATop, ALeft: Integer; AViewInfo: TcxGridColumnContainerViewInfo);
var
  AText: string;
  R, AVisibleRect: TRect;
  I, APrevTop, AOffsetY: Integer;
  AItem: TcxGridColumnHeaderViewInfo;
  AVisualItem: TcxExportVisualItem;
begin
  AOffsetY := ATop - AViewInfo.Bounds.Top;
  APrevTop := cxMaxRectSize;
  for I := 0 to AViewInfo.Count - 1 do
  begin
    AItem := AViewInfo.Items[I];
    AItem.Recalculate;
    R := cxRectOffset(AItem.RealBounds, GetColumnOffset(AItem.Column), AOffsetY);
    APrevTop := Min(APrevTop, R.Top);
    AVisibleRect := R;
    if not AViewInfo.Visible or not OptionsView.Header then
      AVisibleRect := cxNullRect;
    AText := AItem.Text;
    if Trim(AText) = '' then
      AText := AItem.Column.AlternateCaption;
    AVisualItem := AddVisualItemEx(AVisibleRect, AText, AItem.Params, AItem.AlignmentHorz, cxBordersAll, clBtnShadow);
    AVisualItem.Bounds := R;
    AVisualItem.Hidden := not OptionsView.Header;
    ATop := Max(ATop, R.Bottom);
    AVisualItem.Data := AItem.Column;
    AddPattern(R, AVisualItem.Data);
  end;
  SetPatternsBounds(APrevTop, ATop - APrevTop);
  if not OptionsView.Header then
    ATop := 0;
end;

procedure TcxGridTableViewExport.ScalePatterns(AHeight, ARowHeight: Integer);
var
  I: Integer;
  ABounds: TRect;
begin
  for I := 0 to PatternsList.Count - 1 do
    with Patterns[I] do
    begin
      ABounds := Bounds;
      ABounds.Top := (Bounds.Top div ARowHeight) * (AHeight div RecordRowCount);
      ABounds.Bottom := (Bounds.Bottom div ARowHeight) * (AHeight div RecordRowCount);
      Bounds := ABounds;
    end;
  RecordHeight := AHeight;
end;

procedure TcxGridTableViewExport.SetPatternsBounds(ATop, ABottom: Integer);
var
  I: Integer;
  ABounds: TRect;
  APattern: TcxExportVisualItem;
begin
  for I := 0 to PatternsList.Count - 1 do
  begin
    APattern := Patterns[I];
    ABounds := APattern.Bounds;
    OffsetRect(ABounds, 0, -ATop);
    ABounds.Top := Max(0, ABounds.Top);
    ABounds.Bottom := Min(ABottom, ABounds.Bottom);
    APattern.Bounds := ABounds;
  end;
  RecordHeight := ABottom;
  if RecordHeight = 0 then
    RecordHeight := ViewInfo.RecordsViewInfo.RowHeight;
  DefaultRowHeight := RecordHeight;
end;

procedure TcxGridTableViewExport.SetPatternsHeight(AHeight: Integer);
var
  I: Integer;
  ABounds: TRect;
  APattern: TcxExportVisualItem;
begin
  for I := 0 to PatternsList.Count - 1 do
  begin
    APattern := Patterns[I];
    ABounds := APattern.Bounds;
    ABounds.Top := 0;
    ABounds.Bottom := AHeight;
    APattern.Bounds := ABounds;
  end;
  RecordHeight := AHeight;
end;

function TcxGridTableViewExport.GetFooterCellBorderColor: TColor;
begin
  Result := Grid.LookAndFeelPainter.FooterSeparatorColor;
end;

function TcxGridTableViewExport.GetGridLineColor: TColor;
begin
  Result := OptionsView.GridLineColor;
  if Result = clDefault then
    Result := clBtnShadow;
end;

function TcxGridTableViewExport.GetGridLines: TcxBorders;
begin
  Result := GridLinesToBorders[OptionsView.GridLines];
end;

function TcxGridTableViewExport.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewExport.GetGroupSummaryCount: Integer;
begin
  Result := FGroupSummaryItemsList.Count;
end;

function TcxGridTableViewExport.GetGroupSummaryItem(
  AIndex: Integer): TcxExportGroupSummaryItem;
begin
  Result := TcxExportGroupSummaryItem(FGroupSummaryItemsList[AIndex]);
end;

function TcxGridTableViewExport.GetHasPreview(
  ARow: TcxCustomGridRecord): Boolean;
begin
  with GridView.Preview do
    Result := Visible and (Column <> nil);
end;

function TcxGridTableViewExport.GetIndicatorWidth: Integer;
begin
  if ViewInfo.IndicatorViewInfo.Visible then
    Result := ViewInfo.IndicatorViewInfo.Width
  else
    Result := 0;
end;

function TcxGridTableViewExport.GetLeftPos: Integer;
begin
  Result := GridView.Controller.LeftPos;
end;

function TcxGridTableViewExport.GetOptionsView: TcxGridTableOptionsView;
begin
  Result := GridView.OptionsView;
end;

function TcxGridTableViewExport.GetPattern(AIndex: Integer): TcxExportVisualItem;
begin
  Result := PatternsList[AIndex] as TcxExportVisualItem;
end;

function TcxGridTableViewExport.GetPatternCount: Integer;
begin
  Result := PatternsList.Count;
end;

function TcxGridTableViewExport.GetPreviewPlace: TcxGridPreviewPlace;
begin
  Result := GridView.Preview.Place;
end;

function TcxGridTableViewExport.GetStyles: TcxGridTableViewStyles;
begin
  Result := GridView.Styles;
end;

function TcxGridTableViewExport.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := inherited ViewInfo as TcxGridTableViewInfo;
end;

procedure TcxGridTableViewExport.SetLeftPos(AValue: Integer);
begin
  GridView.Controller.LeftPos := AValue;
  ViewInfo.Recalculate;
end;

{ TcxGridBandedTableViewExport }

procedure TcxGridBandedTableViewExport.CreateBandHeaders(
  var ATop: Integer; AForRootBands: Boolean);

  function IsFirstInGroup(ABand: TcxGridBand): Boolean;
  begin
    if ABand.IsRoot then
      Result := (ABand.VisibleRootIndex = 0) or
        (ABand.Bands.VisibleRootItems[ABand.VisibleRootIndex - 1].FixedKind <> ABand.FixedKind)
    else
      Result := (ABand = ABand.ParentBand.VisibleChildBands[0]) and IsFirstInGroup(ABand.ParentBand);
  end;

var
  R: TRect;
  ABandViewInfo: TcxGridBandViewInfo;
  ABandsViewInfo: TcxGridBandsViewInfo;
  ABandHeader: TcxGridBandHeaderViewInfo;
  AVisualItem: TcxExportVisualItem;
  AHeight, I, AOffsetX, AOffsetY, AColsOffset: Integer;
begin
  AHeight := 0;
  AOffsetX := 0;
  AOffsetY := ATop - ViewInfo.HeaderViewInfo.Bounds.Top;
  ABandsViewInfo := ViewInfo.HeaderViewInfo.BandsViewInfo;
  for I := 0 to ABandsViewInfo.Count - 1 do
  begin
    ABandViewInfo := ABandsViewInfo.Items[I];
    if (AForRootBands and (ABandViewInfo.Band.ParentBand <> nil)) or
      (not AForRootBands and (ABandViewInfo.Band.ParentBand = nil)) then Continue;
    if not AForRootBands then
      AOffsetX := ABandViewInfo.Bounds.Left + GetParentBandOffset(ABandViewInfo.Band.ParentBand);
    with cxRectSize(ABandViewInfo.Bounds) do
      R := cxRectBounds(AOffsetX, ABandViewInfo.Bounds.Top + AOffsetY, cx, cy);
    if not cxRectIsEmpty(R) then
    begin
      AddVisualItemEx(R, ABandViewInfo.Text, ABandViewInfo.Params,
        ABandViewInfo.AlignmentHorz, [], clDefault, True);
    end;
    AColsOffset := AOffsetX - ABandViewInfo.Bounds.Left;
    AddPattern(R, ABandViewInfo.Band, AColsOffset);
    ABandHeader := ABandViewInfo.HeaderViewInfo;
    R := cxRectOffset(cxRectSetLeft(ABandHeader.Bounds, AOffsetX), 0, AOffsetY);
    if (ABandHeader.Band.FixedKind = fkRight) and IsFirstInGroup(ABandHeader.Band) then
      OffsetRect(R, OptionsView.FixedBandSeparatorWidth, 0);
    AVisualItem := AddVisualItemEx(R, ABandHeader.Text, ABandHeader.Params,
      ABandHeader.AlignmentHorz, cxBordersAll, clBtnShadow);
    AVisualItem.Data := ABandHeader.Band;
    AVisualItem.Hidden := not OptionsView.BandHeaders;
    if OptionsView.BandHeaders then
      AHeight := Max(AHeight, R.Bottom);
    if AForRootBands then
      Inc(AOffsetX, cxRectWidth(ABandViewInfo.Bounds));
  end;
  ATop := AHeight;
end;

procedure TcxGridBandedTableViewExport.CreateHeader(var ATop, ALeft: Integer);
var
  ABandsViewInfo: TcxGridBandsViewInfo;
  APrevTop, AHeight: Integer;
begin
  AHeight := 0;
  APrevTop := ATop;
  ABandsViewInfo := ViewInfo.HeaderViewInfo.BandsViewInfo;
  CreateBandHeaders(AHeight, True);
  ATop := Max(ATop, AHeight);
  AHeight := APrevTop;
  CreateBandHeaders(AHeight, False);
  ATop := Max(ATop, AHeight);
  inherited CreateHeader(APrevTop, ALeft);
  ATop := Max(ATop, APrevTop);
  FRecordRowCount := ABandsViewInfo.LineCount;
  if RecordRowCount = 0 then
    DefaultRowHeight := RecordHeight
  else
    DefaultRowHeight := RecordHeight div RecordRowCount;
end;

function TcxGridBandedTableViewExport.GetColumnOffset(AColumn: TcxGridColumn): Integer;
begin
  Result := Integer(GetPatternByBand(TcxGridBandedColumn(AColumn).Position.Band).Data2);
end;

function TcxGridBandedTableViewExport.GetContentOffset: TPoint;
begin
  Result := Point(0, 0);
  if ViewInfo.GroupByBoxViewInfo.Visible then
    Result.Y := -ViewInfo.GroupByBoxViewInfo.Bounds.Bottom;
end;

function TcxGridBandedTableViewExport.GetIsSummaryUnderColumns(ARow: TcxGridGroupRow): Boolean;
begin
  Result := inherited GetIsSummaryUnderColumns(ARow) and (GridView.Bands.Layout = blNonFixed) and
    (GridView.Bands.VisibleRowCount = 1);
end;

function TcxGridBandedTableViewExport.GetParentBandOffset(ABand: TcxGridBand): Integer;
begin
  while ABand.ParentBand <> nil do
    ABand := ABand.ParentBand;
  Result := Integer(GetPatternByBand(ABand).Data2);
end;

function TcxGridBandedTableViewExport.GetPatternByBand(
  ABand: TcxGridBand): TcxExportVisualItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PatternCount - 1 do
    with Patterns[I] do
      if not IsColumn and (Data = ABand) then
      begin
        Result := Patterns[I];
        Break;
      end;
end;

function TcxGridBandedTableViewExport.GetPatternParams(
  ARecord: TcxCustomGridRecord; AItem: TcxExportVisualItem): TcxViewParams;
var
  ACellPos: TcxGridDataCellPos;
begin
  if AItem.IsColumn then
    Result := inherited GetPatternParams(ARecord, AItem)
  else
  begin
    ACellPos := TcxGridDataCellPos.Create(ARecord, nil);
    try
      TcxGridBand(AItem.Data).Styles.GetViewParams(bsContent, ACellPos, nil, Result);
    finally
      ACellPos.Free;
    end;
  end;
end;

function TcxGridBandedTableViewExport.ProduceColumnsContainer(
  AContainer: TcxGridColumnContainerViewInfo; ATop, ALeft: Integer): Integer;
var
  R: TRect;
  I: Integer;
  AItem: TcxGridColumnHeaderViewInfo;
  AVisualItem: TcxExportVisualItem;
begin
  Result := 0;
  if not AContainer.Visible then Exit;
  Dec(ATop, AContainer.Bounds.Top);
  Dec(ALeft, AContainer.Bounds.Left);
  R := AContainer.Bounds;
  AddVisualItemEx(cxRectOffset(AContainer.Bounds, ALeft, ATop),
    AContainer.Text, AContainer.Params, AContainer.AlignmentHorz, [], clDefault, True);
  Result := 0;
  for I := 0 to AContainer.Count - 1 do
  begin
    AItem := AContainer.Items[I];
    if not AItem.Visible then
    R := cxRectOffset(AItem.RealBounds, ALeft, ATop);
    AVisualItem := AddVisualItemEx(R, AItem.Text,
      AItem.Params, AItem.AlignmentHorz, cxBordersAll, clBtnShadow);
    Result := Max(Result, R.Bottom);
    AVisualItem.Data := AItem.Column;
  end;
end;

function TcxGridBandedTableViewExport.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedTableViewExport.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

function TcxGridBandedTableViewExport.GetOptionsView: TcxGridBandedTableOptionsView;
begin
  Result := TcxGridBandedTableOptionsView(inherited OptionsView);
end;

{ TcxExportCardRow }

constructor TcxExportCardRow.Create(AOwner: TcxExportCard);
begin
  FOwner := AOwner;
end;

procedure TcxExportCardRow.AddToScales(AAdapter: IcxGridExportProviderAdapter);
begin
  AAdapter.AddRangeHorz(Bounds.Left, Bounds.Right);
  if HasIndent then
    AAdapter.AddRangeHorz(Bounds.Left + CategoryIndent, Bounds.Left + CategoryIndent);
  if HasSeparator then
    AAdapter.AddRangeVert(Bounds.Top + SeparatorWidth, Bounds.Top + SeparatorWidth);
  if ShowCaption then
    AAdapter.AddRangeHorz(Bounds.Left + CaptionWidth, Bounds.Left + CaptionWidth);
  AAdapter.AddRangeVert(Bounds.Top, Bounds.Bottom);
end;

function TcxExportCardRow.GetCaptionBounds: TRect;
begin
  Result := Bounds;
  if ShowData then
    Result.Right := Result.Left + CaptionWidth;
  if HasIndent then
    Inc(Result.Left, CategoryIndent);
  if HasSeparator then
    Inc(Result.Top, SeparatorWidth);

  Result.Top := Min(Result.Top, Result.Bottom);
  Result.Right := Min(Result.Right, Bounds.Right);
end;

function TcxExportCardRow.GetCaptionStyle: TcxViewParams;
begin
  case Row.Kind of
    rkCaption:
      Row.Styles.GetCaptionRowParams(Card, Result);
    rkCategory:
      Row.Styles.GetCategoryRowParams(Card,Result);
    else
      Row.Styles.GetCaptionParams(Card, Result);
  end;
end;

function TcxExportCardRow.GetCard: TcxGridCard;
begin
  Result := Owner.Card;
end;

function TcxExportCardRow.GetCategoryIndent: Integer;
begin
  if Row.HasExpandButton then
    Result := Owner.Owner.ExpandButtonSize + cxTextOffset * 2
  else
    Result := Owner.Owner.CategoryIndent;
end;

function TcxExportCardRow.GetDataAlignment: TAlignment;
begin
  Result := Row.GetProperties.Alignment.Horz;
end;

function TcxExportCardRow.GetDataBounds: TRect;
begin
  Result := Bounds;
  if ShowCaption then
    Inc(Result.Left, CaptionWidth)
  else
    if HasIndent then
      Inc(Result.Left, CategoryIndent);
  if HasSeparator then
    Inc(Result.Top, SeparatorWidth);

  Result.Left := Min(Result.Left, Result.Right);
  Result.Top := Min(Result.Top, Result.Bottom);
end;

function TcxExportCardRow.GetDataStyle: TcxViewParams;
begin
  case Row.Kind of
    rkCaption:
      Row.Styles.GetCaptionRowParams(Card, Result);
    rkCategory:
      Row.Styles.GetCategoryRowParams(Card, Result);
    else
      Row.Styles.GetContentParams(Card, Result);
  end;
end;

function TcxExportCardRow.GetDataValue: Variant;
begin
  Result := Owner.Owner.GetViewItemValue(Card, Row);
end;

function TcxExportCardRow.GetHasIndent: Boolean;
begin
  Result := Row.HasExpandButton or (Row.CategoryRow <> nil);
  if Result and Owner.Owner.IsHorizontalRows then
    Result := Row.Position.VisibleIndexInLayer = 0;
end;

function TcxExportCardRow.GetHasSeparator: Boolean;
begin
  Result := FHasSeparator;
end;

function TcxExportCardRow.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

function TcxExportCardRow.GetIndentBounds: TRect;
begin
  Result := Bounds;
  Result.Right := Result.Left + CategoryIndent;
  if HasSeparator then
    Inc(Result.Top, SeparatorWidth);
end;

function TcxExportCardRow.GetIndentStyle: TcxViewParams;
begin
  if HasIndent then
  begin
    if Row.HasExpandButton or (Row.CategoryRow = nil) then
      Result := CaptionStyle
    else
      Row.CategoryRow.Styles.GetCategoryRowParams(Card, Result);
  end;
end;

function TcxExportCardRow.GetSeparatorBounds: TRect;
begin
  if Owner.Owner.IsHorizontalRows then
  begin
    Result := cxRectSetTop(Owner.Bounds, Bounds.Top, SeparatorWidth);
    InflateRect(Result, -Owner.BorderWidth, 0);
  end
  else
    Result := cxRectSetTop(Bounds, Bounds.Top, SeparatorWidth);
end;

function TcxExportCardRow.GetSeparatorWidth: Integer;
begin
  Result := Owner.Owner.OptionsView.CategorySeparatorWidth;
end;

function TcxExportCardRow.GetShowCaption: Boolean;
begin
  Result := Row.Options.ShowCaption;
end;

function TcxExportCardRow.GetShowData: Boolean;
begin
  Result := Row.Options.ShowData;
end;

function TcxExportCardRow.GetVisibleCaption: string;
begin
  Result := Row.VisibleCaption;
end;

function TcxExportCardRow.GetWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

procedure TcxExportCardRow.SetHeight(AValue: Integer);
begin
  Bounds.Bottom := Bounds.Top + AValue;
end;

procedure TcxExportCardRow.SetWidth(AValue: Integer);
begin
  Bounds.Right := Bounds.Left + AValue;
end;

{ TcxExportCard }

constructor TcxExportCard.Create(AOwner: TcxGridCardViewExport; ACard: TcxGridCard);
begin
  FCard := ACard;
  FLayersList := TcxObjectList.Create();
  FOwner := AOwner;
end;

destructor TcxExportCard.Destroy;
begin
  FreeAndNil(FLayersList);
  inherited Destroy;
end;

procedure TcxExportCard.AddToScales(AAdapter: IcxGridExportProviderAdapter);
var
  ALayerIndex, ARowIndex: Integer;
begin
  for ALayerIndex := 0 to LayerCount - 1 do
  begin
    for ARowIndex := 0 to RowCount[ALayerIndex] - 1 do
      Rows[ALayerIndex, ARowIndex].AddToScales(AAdapter);
  end;
  if BorderWidth > 0 then
  begin
    AAdapter.AddRangeHorz(Bounds.Left, Bounds.Right);
    AAdapter.AddRangeVert(Bounds.Top, Bounds.Bottom);
  end;
  if HasSeparators then
  begin
    for ALayerIndex := 0 to LayerCount - 2 do
      with LayerSeparators[ALayerIndex] do
      begin
        AAdapter.AddRangeHorz(Left, Right);
        AAdapter.AddRangeVert(Top, Bottom);
      end;
  end;
end;

procedure TcxExportCard.CalculateLayersCaptionWidth(AWidths: TcxExportIntList; AFistRowInLayerOnly: Boolean);
var
  I, J: Integer;
begin
  I := LayerCount;
  if AFistRowInLayerOnly then
    I := 1;
  if AWidths.Count < I then
    AWidths.Count := I;
  for I := 0 to LayerCount - 1 do
  begin
    if AFistRowInLayerOnly then
       AWidths[0] := GetRowCaptionWidth(I, 0, AWidths[0])
    else
      for J := 0 to RowCount[I] - 1 do
        AWidths[I] := GetRowCaptionWidth(I, J, AWidths[I]);
  end;
end;

function TcxExportCard.GetRowCaptionWidth(ALayerIndex, ARowIndex: Integer; AMaxWidth: Integer = 0): Integer;
var
  ARow: TcxExportCardRow;
begin
  ARow := Rows[ALayerIndex, ARowIndex];
  if ARow.HasIndent then
    Result := Owner.OptionsView.CategoryIndent
  else
    Result := 0;
  if ARow.ShowCaption and ARow.ShowData then
    Inc(Result, Owner.TextWidthEx(ARow.CaptionStyle, ARow.VisibleCaption) + cxTextOffset * 2);
  Result := Max(Result, AMaxWidth);
end;

procedure TcxExportCard.SetLayersCaptionWidth(AWidths: TcxExportIntList; AFistRowInLayerOnly: Boolean);
var
  ALayerIndex, ARowIndex, AWidth: Integer;
begin
  for ALayerIndex := 0 to LayerCount - 1 do
    for ARowIndex := 0 to RowCount[ALayerIndex] - 1 do
    begin
      if Owner.IsHorizontalRows then
      begin
        if (ARowIndex = 0) or not AFistRowInLayerOnly then
          AWidth := AWidths[ARowIndex]
        else
          AWidth := GetRowCaptionWidth(ALayerIndex, ARowIndex);
        SetRowCaptionWidth(ALayerIndex, ARowIndex, AWidth);
      end
      else
        SetRowCaptionWidth(ALayerIndex, ARowIndex, AWidths[ALayerIndex]);
    end;
end;

procedure TcxExportCard.SetRowCaptionWidth(ALayerIndex, ARowIndex, AWidth: Integer);
begin
  Rows[ALayerIndex, ARowIndex].CaptionWidth := AWidth;
end;

function TcxExportCard.AddLayer: TList;
begin
  Result := TcxObjectList.Create;
  LayersList.Add(Result);
end;

function TcxExportCard.AddRow(ALayerIndex: Integer; ARow: TcxGridCardViewRow): TcxExportCardRow;
var
  ACardViewInfo: TcxGridCardViewInfo;
  ACellViewInfo: TcxGridTableDataCellViewInfo;
begin
  Result := TcxExportCardRow.Create(Self);
  Result.Row := ARow;
  if ARow.Options.ShowCaption then
    Result.CaptionStyleIndex := Owner.RegisterViewParams(Result.CaptionStyle, ARow.CaptionAlignmentHorz);
  Result.DataStyleIndex := Owner.RegisterViewParams(Result.DataStyle, Result.DataAlignment);
  if Result.HasIndent then
    Result.IndentStyleIndex := Owner.RegisterViewParams(Result.IndentStyle);
  Result.Bounds := cxRectBounds(Bounds.Left, 0, ARow.Position.Width, ARow.Position.LineCount * Owner.TextHeightEx(Result.DataStyle));

  ACardViewInfo := TcxGridCardViewInfo.Create(Card.GridView.ViewInfo.RecordsViewInfo, Card);
  try
    ACardViewInfo.Recalculate;
    ACellViewInfo := ACardViewInfo.GetCellViewInfoByItem(ARow);
    if ACellViewInfo <> nil then
      Result.Bounds := cxRectSetHeight(Result.Bounds, ACellViewInfo.ContentHeight);
  finally
    ACardViewInfo.Free;
  end;
  Layers[ALayerIndex].Add(Result);
end;

procedure TcxExportCard.AddLayerSeparators;
var
  ALayerIndex, ARowIndex, AOffset: Integer;
begin
  if HasSeparators and Owner.IsHorizontalRows then
  begin
    AOffset := 0;
    for ALayerIndex := 1 to LayerCount - 1 do
    begin
      Inc(AOffset, SeparatorWidth);
      for ARowIndex := 0 to RowCount[ALayerIndex] - 1 do
        OffsetRect(Rows[ALayerIndex, ARowIndex].Bounds, 0, AOffset);
    end;
    Inc(FBounds.Bottom, AOffset);
  end;
end;

procedure TcxExportCard.AdjustLayersWidthToWidth;
var
  ALayer, ARowIndex, ALeft: Integer;
  AWidths: array of Integer;
begin
  SetLength(AWidths, LayerCount);
  for ALayer := 0 to LayerCount - 1 do
    for ARowIndex := 0 to RowCount[ALayer] - 1 do
      AWidths[ALayer] := Max(AWidths[ALayer], Rows[ALayer, ARowIndex].Width);
  CalculateCardRowWidths(AWidths, (Bounds.Right - Bounds.Left) - BorderWidth * 2);
  ALeft := Bounds.Left + BorderWidth;
  for ALayer := 0 to LayerCount - 1 do
  begin
    if HasSeparators and (ALayer > 0) then
    begin
      Inc(ALeft, SeparatorWidth);
      Dec(AWidths[ALayer], SeparatorWidth);
    end;
    for ARowIndex := 0 to RowCount[ALayer] - 1 do
      with Rows[ALayer, ARowIndex] do
        Bounds := cxRectSetLeft(Bounds, ALeft, AWidths[ALayer]);
    Inc(ALeft, AWidths[ALayer]);
  end;
end;

function TcxExportCard.AdjustRowsHeightInLayer(ALayer, ATop: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount[ALayer] - 1 do
    Result := Max(Rows[ALayer, I].Height, Result);
  for I := 0 to RowCount[ALayer] - 1 do
    with Rows[ALayer, I].Bounds do
    begin
      Top := ATop;
      Bottom := Top + Result;
    end;
end;

procedure TcxExportCard.AdjustRowsWidthToWidth(ALayer: Integer);
var
  I, ALeft, AWidth: Integer;
  AWidths: array of Integer;
begin
  AWidth := Bounds.Right - Bounds.Left - BorderWidth * 2;
  if RowCount[ALayer] = 1 then
  begin
    Rows[ALayer, 0].Bounds.Left := Bounds.Left + BorderWidth;
    Rows[ALayer, 0].Width := AWidth
  end
  else
  begin
    SetLength(AWidths, RowCount[ALayer]);
    for I := 0 to RowCount[ALayer] - 1 do
      AWidths[I] := Rows[ALayer, I].Width;
    CalculateCardRowWidths(AWidths, AWidth);
    ALeft := Bounds.Left + BorderWidth;
    for I := 0 to RowCount[ALayer] - 1 do
      with Rows[ALayer, I] do
      begin
        Bounds.Left := ALeft;
        Inc(ALeft, AWidths[I]);
        Bounds.Right := ALeft;
      end;
  end;
end;

procedure TcxExportCard.CheckCategorySeparators(AHorizontalLayout: Boolean);
var
  ARow: TcxExportCardRow;
  ALayerIndex, ARowIndex, ACountRowsInLayer: Integer;
begin
  if Owner.OptionsView.CategorySeparatorWidth = 0 then Exit;
  for ALayerIndex := 1 to LayerCount - 1 do
  begin
    ACountRowsInLayer := RowCount[ALayerIndex];
    for ARowIndex := 0 to ACountRowsInLayer - 1 do
    begin
      ARow := Rows[ALayerIndex, ARowIndex];
      ARow.FHasSeparator := ARow.Row.Kind = rkCategory;
      if ARow.FHasSeparator then
      begin
        if Owner.IsHorizontalRows then
          ARow.FHasSeparator := ACountRowsInLayer = 1
        else
          ARow.FHasSeparator := ARowIndex > 0
      end;
      if ARow.HasSeparator then
        Inc(ARow.Bounds.Bottom, SeparatorWidth);
    end;
  end;
end;

function TcxExportCard.GetBorderWidth: Integer;
begin
  Result := Owner.OptionsView.CardBorderWidth;
end;

function TcxExportCard.GetHasSeparators: Boolean;
begin
  Result := not Owner.IsSimpleLayout and (LayerCount > 1) and (SeparatorWidth > 0);
end;

function TcxExportCard.GetLayer(AIndex: Integer): TList;
begin
  Result := TList(LayersList[AIndex])
end;

function TcxExportCard.GetLayerCount: Integer;
begin
  Result := LayersList.Count;
end;

function TcxExportCard.GetLayerSeparator(AIndex: Integer): TRect;
begin
  if Owner.IsHorizontalRows then
  begin
    Result := cxRectSetTop(Bounds, Rows[AIndex, 0].Bounds.Bottom, SeparatorWidth);
    InflateRect(Result, -BorderWidth, 0);
  end
  else
  begin
    Result := cxRectSetLeft(Bounds, Rows[AIndex, 0].Bounds.Right, SeparatorWidth);
    InflateRect(Result, 0, -BorderWidth);
  end;
end;

function TcxExportCard.GetRow(ALayerIndex, ARowIndex: Integer): TcxExportCardRow;
begin
  Result := TcxExportCardRow(Layers[ALayerIndex][ARowIndex]);
end;

function TcxExportCard.GetRowCount(ALayerIndex: Integer): Integer;
begin
  Result := Layers[ALayerIndex].Count;
end;

function TcxExportCard.GetSeparatorWidth: Integer;
begin
  Result := Owner.OptionsView.LayerSeparatorWidth;
end;

procedure TcxExportCard.SetBounds(const ABounds: TRect);
var
  AOffset: TPoint;
  ALayerIndex, ARowIndex: Integer;
begin
  FBounds := ABounds;
  AOffset := Point(ABounds.Left - FBounds.Left, ABounds.Top - FBounds.Top);
  for ALayerIndex := 0 to LayerCount - 1 do
    for ARowIndex := 0 to RowCount[ALayerIndex] - 1 do
      OffsetRect(Rows[ALayerIndex, ARowIndex].Bounds, AOffset.X, AOffset.Y);
end;

{ TcxExportCardLayoutBuilder }

constructor TcxExportCardLayoutBuilder.Create(AOwner: TcxGridCardViewExport);
begin
  FRowsList := TList.Create;
  FOwner := AOwner;
end;

destructor TcxExportCardLayoutBuilder.Destroy;
begin
  FreeAndNil(FRowsList);
  inherited Destroy;
end;

procedure TcxExportCardLayoutBuilder.BuildLayout(ACard: TcxGridCard; AExportCard: TcxExportCard);
var
  ALayerIndex, ARowIndex, ATop: Integer;
begin
  FExportCard := AExportCard;
  ACard.GetVisibleRows(RowsList);
  ATop := AExportCard.Bounds.Top + AExportCard.BorderWidth;
  SplitRowsToLayers;
  if Owner.IsHorizontalRows then
  begin
    for ALayerIndex := 0 to ExportCard.LayerCount - 1 do
    begin
      ExportCard.AdjustRowsWidthToWidth(ALayerIndex);
      Inc(ATop, ExportCard.AdjustRowsHeightInLayer(ALayerIndex, ATop));
      AExportCard.FBounds.Bottom := ATop + AExportCard.BorderWidth;
    end;
    ExportCard.AddLayerSeparators;
  end
  else
  begin
    ExportCard.AdjustLayersWidthToWidth();
    for ALayerIndex := 0 to ExportCard.LayerCount - 1 do
    begin
      ATop := ExportCard.Bounds.Top + ExportCard.BorderWidth;
      for ARowIndex := 0 to ExportCard.RowCount[ALayerIndex] - 1 do
        with ExportCard.Rows[ALayerIndex, ARowIndex] do
        begin
          Bounds := cxRectSetTop(Bounds, ATop);
          ATop := Bounds.Bottom;
        end;
      ExportCard.FBounds.Bottom := Max(ExportCard.FBounds.Bottom, ATop + ExportCard.BorderWidth);
    end;
  end;
end;

function TcxExportCardLayoutBuilder.GetLayerIndex(ARow: TcxGridCardViewRow): Integer;
begin
  if Owner.IsHorizontalRows then
    Result := ARow.Position.RowIndex
  else
    Result := ARow.Position.ColIndex;
end;

procedure TcxExportCardLayoutBuilder.SplitRowsToLayers;
var
  ARow: TcxGridCardViewRow;
  I, ACurrentLayerIndex: Integer;
begin
  ACurrentLayerIndex := -1;
  for I := 0 to RowsList.Count - 1 do
  begin
    ARow := TcxGridCardViewRow(RowsList[I]);
    if GetLayerIndex(ARow) <> ACurrentLayerIndex then
    begin
      ACurrentLayerIndex := GetLayerIndex(ARow);
      ExportCard.AddLayer;
    end;
    ExportCard.AddRow(ExportCard.LayerCount - 1, ARow);
  end;
  ExportCard.CheckCategorySeparators(Owner.IsHorizontalRows);
end;

{ TcxGridCardViewExport }

procedure TcxGridCardViewExport.AddCardSeparator(APosition: Integer);
begin
  if LayoutDirection = ldHorizontal then
    Adapter.AddRangeHorz(APosition, APosition + OptionsView.SeparatorWidth)
  else
    Adapter.AddRangeVert(APosition, APosition + OptionsView.SeparatorWidth);

  FCardSeparators.AddPairs(APosition, APosition + OptionsView.SeparatorWidth);
end;

function TcxGridCardViewExport.AddExportCard(AColumnPosition, ARowPosition: Integer; ACard: TcxGridCard): TcxExportCard;
begin
  Result := TcxExportCard.Create(Self, ACard);
  Result.Bounds := cxRectBounds(AColumnPosition, ARowPosition, CardWidth, 0);
  LayoutBuilder.BuildLayout(ACard, Result);
  FExportCardsList.Add(Result);
end;

procedure TcxGridCardViewExport.AdjustRowCaptionWidth;
var
  I: Integer;
  AWidths: TcxExportIntList;
  AAutoWidth: Boolean;
begin
  AWidths := TcxExportIntList.Create;
  try
    AAutoWidth := GridView.OptionsView.CaptionWidth = 0;
    if AAutoWidth then
    begin
      for I := 0 to CardCount - 1 do
        ExportCards[I].CalculateLayersCaptionWidth(AWidths, IsHorizontalRows);
    end
    else
    begin
      for I := 0 to CardCount - 1 do
        AWidths.Count := Max(AWidths.Count, ExportCards[I].LayerCount);
      for I := 0 to AWidths.Count - 1 do
        AWidths[I] := GridView.OptionsView.CaptionWidth;
    end;
    for I := 0 to CardCount - 1 do
      ExportCards[I].SetLayersCaptionWidth(AWidths, AAutoWidth);
  finally
    AWidths.Free;
  end;
end;

procedure TcxGridCardViewExport.CalculateVisibleInfo;
begin
  FCardSeparators := TcxExportScale.Create;
  FExportCardsList := TcxObjectList.Create;
  FLayoutBuilder := CreateCardLayoutBuilder;
  RowCardCount := 1;
  ColumnCardCount := 1;
  with GridView.ViewInfo do
  try
    if LayoutDirection = ldVertical then
      ColumnCardCount := Min(CardCount, Max(1, (Bounds.Right - Bounds.Left) div CardWidth))
    else
      RowCardCount := Min(CardCount, Max(1, (Bounds.Bottom - Bounds.Top) div CardHeight));
  except
    on EDivByZero do;
  end;
  if LayoutDirection = ldHorizontal then
    ColumnCardCount := Ceil(CardCount / RowCardCount)
  else
    RowCardCount := Ceil(CardCount / ColumnCardCount)
end;

function TcxGridCardViewExport.CreateCardLayoutBuilder: TcxExportCardLayoutBuilder;
begin
  Result := TcxExportCardLayoutBuilder.Create(Self);
end;

procedure TcxGridCardViewExport.DoCreateExportCells;
var
  AIndex, AColumnPosition, ARowPosition, ARowHeight, ACount: Integer;
begin
  CalculateVisibleInfo;
  ACount := CardCount;
  AColumnPosition := CardIndent;
  ARowPosition := CardIndent;
  ARowHeight := 0;
  ProgressHelper.BeginStage(2 * ACount);
  try
    for AIndex := 0 to ACount - 1 do
    begin
      ARowHeight := Max(ARowHeight, cxRectHeight(AddExportCard(AColumnPosition, ARowPosition, Cards[AIndex]).Bounds));
      if LayoutDirection = ldHorizontal then
      begin
        Inc(ARowPosition, ARowHeight + InterCardVertSpace);
        if (AIndex + 1) mod RowCardCount = 0 then
        begin
          AddCardSeparator(AColumnPosition + CardWidth + CardIndent);
          Inc(AColumnPosition, CardWidth + InterCardHorzSpace);
          ARowPosition := CardIndent;
        end;
      end
      else
      begin
        Inc(AColumnPosition, CardWidth + InterCardHorzSpace);
        if (AIndex + 1) mod ColumnCardCount = 0 then
        begin
          AddCardSeparator(AColumnPosition + ARowHeight + CardIndent);
          Inc(ARowPosition, ARowHeight + InterCardVertSpace);
          AColumnPosition := CardIndent;
        end;
      end;
      ProgressHelper.NextTask;
    end;
    AdjustRowCaptionWidth;
    if KeepRowsSameHeight then
      SetRowSameHeight;
    for AIndex := 0 to ACount - 1 do
    begin
      ExportCards[AIndex].AddToScales(Adapter);
      ProgressHelper.NextTask;
    end;
    Adapter.AddBounds(cxRectBounds(cxPointOffset(Adapter.GetBoundingRect.BottomRight, CardIndent, CardIndent), 0, 0));
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridCardViewExport.ExportCardRow(ACard: TcxExportCard; ARow: TcxExportCardRow; const ACardLogicalBounds: TRect);
var
  ARowLogicalBounds: TRect;
begin
  RealBoundsToLogicalBoundsEx(ARow.Bounds, ACardLogicalBounds, ARowLogicalBounds);
  if ARow.HasIndent then
    SetRealCellStyle(ARow.IndentBounds, ARowLogicalBounds, ARow.IndentStyleIndex);
  if ARow.HasSeparator then
    SetRealCellStyle(ARow.SeparatorBounds, ARowLogicalBounds, CategorySeparatorStyleIndex);
  if ARow.ShowCaption then
    SetRealCellStyleAndValue(ARow.CaptionBounds, ARowLogicalBounds, ARow.CaptionStyleIndex, ARow.VisibleCaption);
  if ARow.Row.Options.ShowData then
    SetRealCellStyleAndValueEx(ARow.DataBounds, ARowLogicalBounds, ARow.DataStyleIndex, ACard.Card, ARow.Row);
end;

procedure TcxGridCardViewExport.DoExportCells;
var
  ACard: TcxExportCard;
  ALogicalBounds: TRect;
  AParams: TcxViewParams;
  I, ALayerIndex, ARowIndex: Integer;
begin
  inherited DoExportCells;
  ProgressHelper.BeginStage(CardCount + CardSeparatorCount);
  try
    for I := 0 to CardCount - 1 do
    begin
      ACard := ExportCards[I];
      RealBoundsToLogicalBounds(ACard.Bounds, ALogicalBounds);
      FillArea(ALogicalBounds, CardBorderStyle);
      RealBoundsToLogicalBounds(cxRectInflate(ACard.Bounds, -CardBorderWidth, -CardBorderWidth), ALogicalBounds);
      GridView.Styles.GetContentParams(ACard.Card, nil, AParams);
      FillArea(ALogicalBounds, RegisterViewParams(AParams));
      for ALayerIndex := 0 to ACard.LayerCount - 1 do
      begin
        for ARowIndex := 0 to ACard.RowCount[ALayerIndex] - 1 do
          ExportCardRow(ACard, ACard.Rows[ALayerIndex, ARowIndex], ALogicalBounds);
        if ACard.HasSeparators and (ALayerIndex < (ACard.LayerCount - 1)) then
          SetRealCellStyle(ACard.LayerSeparators[ALayerIndex], ALogicalBounds, LayerSeparatorStyleIndex);
      end;
      ProgressHelper.NextTask;
    end;

    for I := 0 to CardSeparatorCount - 1 do
    begin
      FillArea(CardSeparators[I], CardSeparatorStyleIndex);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridCardViewExport.Finalize;
begin
  FreeAndNil(FCardSeparators);
  FreeAndNil(FLayoutBuilder);
  FreeAndNil(FExportCardsList);
  inherited Finalize;
end;

function TcxGridCardViewExport.HasSelectedRecords: Boolean;
begin
  Result := GridView.Controller.SelectedRecordCount > 0;
end;

procedure TcxGridCardViewExport.RegisterStyles;
begin
  inherited RegisterStyles;
  FCardSeparatorStyleIndex := RegisterSolidStyleEx(OptionsView.SeparatorColor);
  FCategorySeparatorStyleIndex := RegisterSolidStyle(vsCategorySeparator);
  FLayerSeparatorStyleIndex := RegisterSolidStyle(vsLayerSeparator);
  FCardBorderStyle := RegisterSolidStyle(vsCardBorder);
end;

procedure TcxGridCardViewExport.SetRowSameHeight;
begin
end;

function TcxGridCardViewExport.GetCard(AIndex: Integer): TcxGridCard;
begin
  Result := Records[AIndex] as TcxGridCard;
end;

function TcxGridCardViewExport.GetCardBorderWidth: Integer;
begin
  Result := OptionsView.CardBorderWidth;
end;

function TcxGridCardViewExport.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

function TcxGridCardViewExport.GetCardHeight: Integer;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.RowHeight;
end;

function TcxGridCardViewExport.GetCardIndent: Integer;
begin
  Result := OptionsView.CardIndent;
end;

function TcxGridCardViewExport.GetCardSeparatorCount: Integer;
begin
  Result := FCardSeparators.Count div 2;
end;

function TcxGridCardViewExport.GetCardSeparator(AIndex: Integer): TRect;
var
  AIndex1, AIndex2: Integer;
begin
  Result := Adapter.GetDimensions;
  if LayoutDirection = ldHorizontal then
  begin
    Adapter.RealRangeToLogicalRangeHorz(FCardSeparators[AIndex * 2], FCardSeparators[AIndex * 2 + 1], AIndex1, AIndex2);
    Result.Right := AIndex2;
    Result.Left := AIndex1;
  end
  else
  begin
    Adapter.RealRangeToLogicalRangeVert(FCardSeparators[AIndex * 2], FCardSeparators[AIndex * 2 + 1], AIndex1, AIndex2);
    Result.Bottom := AIndex2;
    Result.Top := AIndex1;
  end;
end;

function TcxGridCardViewExport.GetCardWidth: Integer;
var
  R: TRect;
begin
  Result := OptionsView.CardWidth;
  if OptionsView.CardAutoWidth then
  begin
    R := GridView.ViewInfo.Bounds;
    Result := Max(1, (R.Right - R.Left) div (Result + InterCardHorzSpace));
    Result := (R.Right - R.Left) div Result;
  end;
end;

function TcxGridCardViewExport.GetCategoryIndent: Integer;
begin
  Result := OptionsView.CategoryIndent;
end;

function TcxGridCardViewExport.GetCategorySeparatorWidth: Integer;
begin
  Result := GridView.OptionsView.CategorySeparatorWidth;
end;

function TcxGridCardViewExport.GetExportCard(AIndex: Integer): TcxExportCard;
begin
  Result := ExportCardsList[AIndex] as TcxExportCard;
end;

function TcxGridCardViewExport.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewExport.GetInterCardHorzSpace: Integer;
begin
  Result := CardIndent * 2;
  if LayoutDirection = ldHorizontal then
    Inc(Result, OptionsView.SeparatorWidth);
end;

function TcxGridCardViewExport.GetInterCardVertSpace: Integer;
begin
  Result := CardIndent * 2;
  if LayoutDirection = ldVertical then
    Inc(Result, OptionsView.SeparatorWidth);
end;

function TcxGridCardViewExport.GetIsHorizontalRows: Boolean;
begin
  Result := GridView.RowLayout = rlHorizontal;
end;

function TcxGridCardViewExport.GetIsSimpleLayout: Boolean;
begin
  Result := GridView.RowLayoutController.IsSimpleLayout or (LayerSeparatorWidth = 0);
end;

function TcxGridCardViewExport.GetLayerSeparatorWidth: Integer;
begin
  Result := GridView.OptionsView.LayerSeparatorWidth;
end;

function TcxGridCardViewExport.GetLayoutDirection: TcxGridCardViewLayoutDirection;
begin
  Result := GridView.LayoutDirection;
end;

function TcxGridCardViewExport.GetOptionsView: TcxGridCardViewOptionsView;
begin
  Result := GridView.OptionsView;
end;

{ TcxGridLayoutViewExport }

procedure TcxGridLayoutViewExport.AddRectangle(const ABounds: TRect;
  const AViewParams: TcxViewParams; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault);
var
  AItem: TcxExportVisualItem;
begin
  AItem := AddVisualItemEx(ABounds, '', AViewParams, taLeftJustify, [], clDefault, True);
  AItem.ItemBorders := ABorders;
  AItem.ItemBorderColor := ABorderColor;
end;

procedure TcxGridLayoutViewExport.DoCreateExportCells;
var
  AColumnCardCount, ACard, ACardsRowHeight: Integer;
begin
  try
    if GridView.OptionsView.ViewMode in [lvvmSingleRecord, lvvmSingleRow, lvvmSingleColumn] then
      AColumnCardCount := 1
    else
      with GridView.ViewInfo.RecordsViewInfo do
        AColumnCardCount := Max(cxRectWidth(Bounds) div RecordWidth, 1);
  except
    on EDivByZero do
      AColumnCardCount := 1;
  end;
  FCardBounds := cxNullRect;
  ACardsRowHeight := 0;
  ProgressHelper.BeginStage(CardCount);
  try
    for ACard := 0 to CardCount - 1 do
    begin
      FCard := Cards[ACard];
      FCardViewInfo := TcxGridLayoutViewRecordViewInfo.Create(GridView.ViewInfo.RecordsViewInfo, FCard);
      try
        FCardViewInfo.Calculate(-10000, FCardBounds.Top,  RecordWidth, RecordHeight);
        FCardBounds := cxRectSetSize(FCardBounds, FCardViewInfo.Width, FCardViewInfo.Height);
        GridView.Styles.GetContentParams(Card, nil, FBackground);
        ExportLayoutItem(FCardViewInfo.LayoutViewInfo.ItemsViewInfo);
        ACardsRowHeight := Max(ACardsRowHeight, FCardViewInfo.Height);
        ExportCardCaption(FCardViewInfo.CaptionViewInfo);
        FCardBounds.Left := FCardBounds.Right + GridView.OptionsView.RecordIndent;
        if (ACard + 1) mod AColumnCardCount = 0 then
        begin
          OffsetRect(FCardBounds, 0, ACardsRowHeight + GridView.OptionsView.RecordIndent);
          FCardBounds.Left := 0;
          ACardsRowHeight := 0;
        end;
        FCard := nil;
      finally
        FreeAndNil(FCardViewInfo);
      end;
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridLayoutViewExport.Finalize;
begin
  inherited Finalize;
  dxLayoutRestoreItemStates(FSavedItemStates, Container);
  FreeAndNil(FSavedItemStates);
  UpdateGridViewSize;
end;

procedure TcxGridLayoutViewExport.Initialize;
begin
  FSavedItemStates := TObjectList.Create;
  dxLayoutStoreItemStates(FSavedItemStates, Container);
  dxLayoutSetItemStates(Container, True, False, False, False);
  UpdateGridViewSize;
  inherited Initialize;
end;

procedure TcxGridLayoutViewExport.ExportCardCaption(AInfo: TcxGridLayoutViewRecordCaptionViewInfo);
var
  P: TcxViewParams;
begin
  if AInfo.Visible then
  begin
    P := AInfo.Params;
    P.Color := Painter.DefaultGroupColor;
    AddVisualItemEx(GetRelativeBounds(AInfo.Bounds), AInfo.Text, P, AInfo.AlignmentHorz, cxBordersAll, Painter.DefaultGridLineColor);
  end;
end;

procedure TcxGridLayoutViewExport.ExportEditItem(const ABounds: TRect; AInfo: TcxGridLayoutViewItemViewInfo);
var
  AViewParams: TcxViewParams;
begin
  AddVisualDataItem(ABounds, Adapter.RegisterStyle(GetContentParams(Card, AInfo.Item, AViewParams)), Card, AInfo.Item);
end;

procedure TcxGridLayoutViewExport.ExportLayoutItem(AInfo: TdxCustomLayoutItemViewInfo);
var
  I: Integer;
  R: TRect;
  AParams: TcxViewParams;
  AItem: TcxExportVisualItem;
begin
  R := GetRelativeBounds(AInfo.Bounds);
  AParams := GetParams(AInfo);
  AItem := AddVisualItemEx(R, '',  AParams, taLeftJustify, [], clDefault, True);
  if (AInfo is TdxLayoutGroupViewInfo) and HasCaption(AInfo) then
  begin
    Inc(R.Top, cxRectHeight(AInfo.CaptionViewInfo.TextAreaBounds) div 2);
    AItem := AddVisualItemEx(R, '',  AParams, taLeftJustify, [], clDefault, True);
    GetItemBordersInfo(AInfo, AItem);
  end;
  GetItemBordersInfo(AInfo, AItem);

  if HasCaption(AInfo) then
  begin
    AParams.Color := AInfo.CaptionViewInfo.Color;
    AParams.TextColor := AInfo.CaptionViewInfo.TextColor;
    AParams.Font := AInfo.CaptionViewInfo.Font;
    R := GetRelativeBounds(AInfo.CaptionViewInfo.Bounds);
    AddVisualItemEx(R, AInfo.CaptionViewInfo.Text, AParams, AInfo.CaptionViewInfo.AlignHorz, [], clDefault, False, False);
  end;

  if AInfo is TcxGridLayoutItemViewInfo then
    ExportEditItem(GetRelativeBounds(
      TcxGridLayoutItemViewInfo(AInfo).ControlViewInfo.Bounds),
      TcxGridLayoutItemViewInfo(AInfo).GridItemViewInfo);

  if AInfo is TdxLayoutGroupViewInfo then
  begin
    for  I := 0 to TdxLayoutGroupViewInfo(AInfo).ItemViewInfoCount - 1 do
      ExportLayoutItem(TdxLayoutGroupViewInfo(AInfo).ItemViewInfos[I]);
  end;
end;

procedure TcxGridLayoutViewExport.GetItemBordersInfo(const AItemInfo: TdxCustomLayoutItemViewInfo; AItem: TcxExportVisualItem);
begin
  if not TdxLayoutItemViewInfoAccess(AItemInfo).HasBorder and (AItemInfo.ParentViewInfo <> nil) then
  begin
    AItem.ItemBorders := [];
    AItem.ItemBorderColor := clDefault;
  end
  else
  begin
    AItem.ItemBorders := cxBordersAll;
    AItem.ItemBorderColor := clBtnShadow;
  end;
end;

function TcxGridLayoutViewExport.GetParams(AItemInfo: TdxCustomLayoutItemViewInfo): TcxViewParams;
begin
  Result := Background;
  Result.Color := AItemInfo.Color;
end;

function TcxGridLayoutViewExport.GetRelativeBounds(const AItemBounds: TRect): TRect;
begin
  Result := cxRectOffset(cxRectOffset(AItemBounds, CardViewInfo.Bounds.TopLeft, False), CardBounds.TopLeft);
end;

function TcxGridLayoutViewExport.HasCaption(AInfo: TdxCustomLayoutItemViewInfo): Boolean;
begin
  Result := not (TdxLayoutItemViewInfoAccess(AInfo).Item.CaptionOptions is TdxLayoutNonLabeledItemCaptionOptions)
end;

function TcxGridLayoutViewExport.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

function TcxGridLayoutViewExport.GetCard(AIndex: Integer): TcxGridLayoutViewRecord;
begin
  Result := TcxGridLayoutViewRecord(inherited Records[AIndex]);
end;

function TcxGridLayoutViewExport.GetContainer: TcxGridLayoutContainer;
begin
  Result := GridView.Container;
end;

function TcxGridLayoutViewExport.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewExport.GetRecordHeight: Integer;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.RecordHeight;
end;

function TcxGridLayoutViewExport.GetRecordWidth: Integer;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.RecordWidth;
end;

procedure TcxGridLayoutViewExport.UpdateGridViewSize;
begin
  TcxGridLayoutViewAccess(GridView).IsDefaultViewInfoCalculated := False;
  GridView.Changed(vcSize);
end;

{ TcxGridWinExplorerViewExport }

function TcxGridWinExplorerViewExport.GetGridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TcxGridWinExplorerViewExport.GetRecord(ARecordIndex: Integer): TcxGridWinExplorerViewCustomRecord;
begin
  Result := TcxGridWinExplorerViewCustomRecord(inherited Records[ARecordIndex]);
end;

procedure TcxGridWinExplorerViewExport.AddExportDataCell(AViewInfo: TcxGridWinExplorerViewCustomCellViewInfo);
var
  AParams: TcxViewParams;
  AStyle: TcxCacheCellStyle;
  AStyleIndex: Integer;
begin
  AStyle := GetContentParams(AViewInfo.GridRecord, AViewInfo.Item, AParams);
  AStyleIndex := Adapter.RegisterStyle(AStyle);
  AddVisualDataItem(AViewInfo.Bounds, AStyleIndex, AViewInfo.GridRecord, AViewInfo.Item);
end;

procedure TcxGridWinExplorerViewExport.AddExportDataRecord(AViewInfo: TcxGridWinExplorerViewRecordViewInfo);
begin
  if AViewInfo.HasCheckBox then
    AddExportDataCell(AViewInfo.CheckBoxViewInfo);
  if AViewInfo.HasImage then
    AddExportDataCell(AViewInfo.ImageViewInfo);
  if AViewInfo.HasText then
    AddExportDataCell(AViewInfo.TextViewInfo);
  if AViewInfo.HasDescription then
    AddExportDataCell(AViewInfo.DescriptionViewInfo);
end;

procedure TcxGridWinExplorerViewExport.AddExportGroupRecord(AViewInfo: TcxGridWinExplorerViewGroupRecordViewInfo);
var
  AViewParams: TcxViewParams;
begin
  GridView.Styles.GetGroupParams(AViewInfo.GridRecord, nil, AViewParams);
  AddVisualItemEx(AViewInfo.Bounds, AViewInfo.Text, AViewParams, taLeftJustify, cxBordersAll);
end;

procedure TcxGridWinExplorerViewExport.AddExportRecord(ARecord: TcxGridWinExplorerViewCustomRecord;
  ARecordViewInfo: TcxCustomGridRecordViewInfo);
begin
  if ARecord.IsData then
    AddExportDataRecord(TcxGridWinExplorerViewRecordViewInfo(ARecordViewInfo))
  else
    AddExportGroupRecord(TcxGridWinExplorerViewGroupRecordViewInfo(ARecordViewInfo));
end;

procedure TcxGridWinExplorerViewExport.DoCreateExportCells;
const
  IndentBetweenItems = 10;
var
  I, AContentWidth, APrevHeight: Integer;
  AStartPos, APos: TPoint;
  ARecord: TcxGridWinExplorerViewCustomRecord;
  ARecordsViewInfo: TcxGridWinExplorerViewRecordsViewInfo;
  ARecordAccess: TcxCustomGridRecordAccess;
  ARecordViewInfo: TcxCustomGridRecordViewInfo;
begin
  APrevHeight := 0;
  AContentWidth := GridView.ViewInfo.ClientWidth;
  ARecordsViewInfo := GridView.ViewInfo.RecordsViewInfo;
  AStartPos := cxNullPoint;
  APos := AStartPos;
  ProgressHelper.BeginStage(RecordCount);
  try
    for I := 0 to RecordCount - 1 do
    begin
      ARecord := Records[I];
      ARecordAccess := TcxCustomGridRecordAccess(ARecord);
      ARecordViewInfo := ARecordAccess.GetViewInfoClass.Create(ARecordsViewInfo, ARecord);
      try
        if (I > 0) and ((APos.X + ARecordViewInfo.Width > AStartPos.X + AContentWidth) or
          (ARecord.IsData <> Records[I - 1].IsData)) then
        begin
          APos.X := AStartPos.X;
          Inc(APos.Y, APrevHeight + IndentBetweenItems);
        end;
        ARecordViewInfo.MainCalculate(APos.X, APos.Y);
        AddExportRecord(ARecord, ARecordViewInfo);
        Inc(APos.X, ARecordViewInfo.Width + IndentBetweenItems);
        APrevHeight := ARecordViewInfo.Height;
      finally
        FreeAndNil(ARecordViewInfo);
      end;
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

{ TcxGridChartViewExport }

procedure TcxGridChartViewExport.CreateExportCells;
begin
  ProgressHelper.BeginStage(1);
  try
    if Adapter.IsGraphicsSupported then
      CreateExportCellsAsGraphic
    else
      CreateExportCellsAsData;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridChartViewExport.CreateExportCellsAsData;
var
  AColumnWidths: array of Integer;
  ARect: TRect;
  C, R: Integer;
begin
  RegisterStyles;

  cxScreenCanvas.Font.Name := DefaultCellStyle.FontName;
  cxScreenCanvas.Font.Size := DefaultCellStyle.FontSize;
  cxScreenCanvas.Font.Style := TFontStyles(DefaultCellStyle.FontStyle);
  try
    FRowHeight := cxTextHeight(cxScreenCanvas.Handle);
    SetLength(FColumnsBounds, ColumnCount);
    SetLength(AColumnWidths, ColumnCount);
    EnumCells(
      procedure (AColumn, ARow: Integer; const AValue: Variant)
      begin
        AColumnWidths[AColumn] := Max(AColumnWidths[AColumn], cxScreenCanvas.TextWidth(AValue));
      end);
  finally
    cxScreenCanvas.Dormant;
  end;

  ARect := cxRect(0, 0, 0, FRowHeight);
  for C := 0 to ColumnCount - 1 do
  begin
    ARect := cxRectSetWidth(ARect, AColumnWidths[C]);
    FColumnsBounds[C] := ARect;
    ARect.Left := ARect.Right;
  end;

  for C := 0 to ColumnCount - 1 do
  begin
    for R := 0 to RowCount - 1 do
      Adapter.AddBounds(CellBounds[C, R]);
  end;
end;

procedure TcxGridChartViewExport.CreateExportCellsAsGraphic;
var
  AWidth, AHeight: Integer;
begin
  AWidth := 0;
  AHeight := 0;
  TcxGridChartViewAccess(GridView).CalculateImageHeight(AHeight);
  TcxGridChartViewAccess(GridView).CalculateImageWidth(AWidth);
  Adapter.AddBounds(cxRect(0, 0, AWidth, AHeight));
end;

procedure TcxGridChartViewExport.EnumCells(AProc: TcxGridChartViewExportEnumCellsProc);
var
  ASeries: TcxGridChartSeries;
  C, R: Integer;
begin
  for C := 0 to ColumnCount - 1 do
  begin
    if C = 0 then
    begin
      AProc(C, 0, 'Category');
      for R := 0 to GridView.ViewData.VisibleCategoryCount - 1 do
        AProc(C, R + 1, GridView.ViewData.VisibleCategories[R]);
    end
    else
    begin
      ASeries := GridView.VisibleSeries[C - 1];
      AProc(C, 0, ASeries.GetDisplayText);
      for R := 0 to ASeries.VisibleValueCount - 1 do
        AProc(C, R + 1, ASeries.VisibleValues[R]);
    end;
  end;
end;

procedure TcxGridChartViewExport.ExportCells;
begin
  ProgressHelper.BeginStage(1);
  try
    if ProviderCache <> nil then
      ProviderCache.PrepareToExport;
    if Adapter.IsGraphicsSupported then
      ExportCellsAsGraphic
    else
      ExportCellsAsData;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridChartViewExport.ExportCellsAsData;
var
  ASearchRect: TRect;
begin
  ASearchRect := Adapter.GetDimensions;
  EnumCells(
    procedure (AColumn, ARow: Integer; const AValue: Variant)
    begin
      SetRealCellStyleAndValue(CellBounds[AColumn, ARow], ASearchRect,
        IfThen(ARow = 0, FHeaderStyleIndex, FCellStyleIndex), AValue);
    end);
end;

procedure TcxGridChartViewExport.ExportCellsAsGraphic;
var
  AGraphic: TGraphic;
  AGraphicClass: TGraphicClass;
  ARect: TRect;
begin
  RegisterStyles;
  AGraphicClass := cxExportGraphicClass;
  if not SupportGraphic(AGraphicClass) then
    AGraphicClass := TBitmap;

  AGraphic := GridView.CreateImage(AGraphicClass);
  if AGraphic is TBitmap then
    TBitmap(AGraphic).PixelFormat := pf24bit;
  Adapter.RealBoundsToLogicalBounds(Adapter.GetBoundingRect, ARect);
  Adapter.SetCellGraphic(ARect, DefaultStyleIndex, AGraphic);
end;

procedure TcxGridChartViewExport.RegisterStyles;
var
  AHeaderStyle, AStyle: TcxCacheCellStyle;
  I: Integer;
begin
  Adapter.SetDefaultStyle(DefaultCellStyle);

  AStyle := DefaultCellStyle;
  for I := 0 to 3 do
    with AStyle.Borders[I] do
    begin
      IsDefault := False;
      Color := clBlack;
      Width := 1;
    end;

  AHeaderStyle := AStyle;
  AHeaderStyle.BrushStyle := cbsSolid;
  AHeaderStyle.BrushBkColor := ColorToRGB(clBtnFace);

  FCellStyleIndex := Adapter.RegisterStyle(AStyle);
  FHeaderStyleIndex := Adapter.RegisterStyle(AHeaderStyle);
end;

function TcxGridChartViewExport.GetCellBounds(C, R: Integer): TRect;
begin
  Result := cxRectOffset(FColumnsBounds[C], 0, R * FRowHeight);
end;

function TcxGridChartViewExport.GetColumnCount: Integer;
begin
  Result := Max(GridView.VisibleSeriesCount + 1, 1);
end;

function TcxGridChartViewExport.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(FGridView);
end;

function TcxGridChartViewExport.GetRowCount: Integer;
begin
  Result := GridView.ViewData.VisibleCategoryCount + 1;
end;

{ TcxGridViewDataAbstractExport }

constructor TcxGridViewDataAbstractExport.Create(const AFileName: string; AGridView: TcxCustomGridView;
  AGrid: TcxCustomGrid; AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject);
begin
  inherited;
  FConditionalFormattingRules := TcxGridDataConditionalFormattingRules.Create;
end;

destructor TcxGridViewDataAbstractExport.Destroy;
begin
  FreeAndNil(FConditionalFormattingRules);
  inherited;
end;

function TcxGridViewDataAbstractExport.GetDataOnly: Boolean;
begin
  Result := True;
end;

procedure TcxGridViewDataAbstractExport.BeforeCommit;
var
  AFormatSettings: TcxDataControllerConditionalFormattingFormatSettings;
  APrevNativeReferences: Boolean;
begin
  inherited BeforeCommit;
  if (ProviderCache <> nil) and (ConditionalFormattingProvider <> nil) then
  begin
    AFormatSettings := TConditionalFormattingProviderAccess(ConditionalFormattingProvider).FormatSettings;
    APrevNativeReferences := AFormatSettings.NativeReferences;
    try
      AFormatSettings.NativeReferences := True;
      ConditionalFormattingRules.Commit(ProviderCache.Provider);
    finally
      AFormatSettings.NativeReferences := APrevNativeReferences;
    end;
  end;
end;

procedure TcxGridViewDataAbstractExport.Initialize;
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  inherited;
  if Supports(GridView, IcxDataControllerConditionalFormattingProviderOwner, AIntf) then
  begin
    FConditionalFormattingProvider := AIntf.GetConditionalFormattingProvider;
    if FConditionalFormattingProvider <> nil then
      FConditionalFormattingRules.Initialize(FConditionalFormattingProvider.ConditionalFormatting);
  end;
end;

{ TcxGridViewDataCustomExport }

function TcxGridViewDataCustomExport.CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect;
begin
  Result := cxEmptyRect;
end;

function TcxGridViewDataCustomExport.CreateExportCell(const ABounds: TRect; AClass: TcxExportCustomItemClass): TcxExportCustomItem;
begin
  Result := AClass.Create;
  Result.Bounds := ABounds;
  Adapter.AddBounds(ABounds);
  FVisualItemsList.Add(Result);
end;

function TcxGridViewDataCustomExport.CreateExportCell(const ABounds: TRect;
  const ADisplayText: string; AClass: TcxExportTextItemClass): TcxExportTextItem;
begin
  Result := TcxExportDataItem(CreateExportCell(ABounds, AClass));
  Result.DisplayText := ADisplayText;
end;

procedure TcxGridViewDataCustomExport.DoExportCells;
var
  I: Integer;
  ARect: TRect;
begin
  ARect := Adapter.GetDimensions;
  ProgressHelper.BeginStage(FVisualItemsList.Count);
  try
    for I := 0 to FVisualItemsList.Count - 1 do
    begin
      ExportCell(TcxExportCustomItem(FVisualItemsList.List[I]), ARect);
      FreeAndNil(FVisualItemsList.List[I]);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
  Adapter.ApplyBestFit;
end;

procedure TcxGridViewDataCustomExport.ExportCell(ACell: TcxExportCustomItem; ARect: TRect);
var
  ADisplayText: string;
begin
  if ACell is TcxExportTextItem then
    ADisplayText := TcxExportTextItem(ACell).DisplayText
  else
    ADisplayText := '';

  SetRealCellStyleAndValue(ACell.Bounds, ARect, GetStyleIndex(ACell), ADisplayText);
end;

function TcxGridViewDataCustomExport.GetItemBounds(ALeftPos, ATopPos: Integer): TRect;
begin
  Result := Rect(ALeftPos, ATopPos, ALeftPos + 1, ATopPos + 1);
end;

function TcxGridViewDataCustomExport.GetStyleIndex(ACell: TcxExportCustomItem): Integer;
begin
  Result := DataStyleIndex;
end;

function TcxGridViewDataCustomExport.RegisterDataStyle: Integer;
var
  AStyle: TcxCacheCellStyle;
begin
  AStyle := DefaultStyle;
  AStyle.FontColor := clNone;
  AStyle.FontSize := 10;
  Result := Adapter.RegisterStyle(AStyle);
end;

procedure TcxGridViewDataCustomExport.RegisterStyles;
begin
  inherited RegisterStyles;
  FDataStyleIndex := RegisterDataStyle;
end;

procedure TcxGridViewDataCustomExport.Initialize;
begin
  inherited Initialize;
  FVisualItemsList := TcxObjectList.Create;
  FVisualItemsList.Capacity := cxVisualItemsListCapacity;
end;

procedure TcxGridViewDataCustomExport.Finalize;
begin
  inherited Finalize;
  FreeAndNil(FVisualItemsList);
end;

{ TcxGridDataExportRange }

constructor TcxGridDataExportRange.Create(AStart: Integer);
begin
  inherited Create;
  FStart := AStart;
  FFinish := -1;
end;

procedure TcxGridDataExportRange.Assign(Source: TcxGridDataExportRange);
begin
  if Source <> nil then
  begin
    Start := Source.Start;
    Finish := Source.Finish;
  end;
end;

function TcxGridDataExportRange.IsActive: Boolean;
begin
  Result := Finish = -1;
end;

procedure TcxGridDataExportRange.Offset(ADelta: Integer);
begin
  Start := Start + ADelta;
  Finish := Finish + ADelta;
end;

{ TcxGridDataExportRanges }

function TcxGridDataExportRanges.First: TcxGridDataExportRange;
begin
  Result := TcxGridDataExportRange(inherited First);
end;

function TcxGridDataExportRanges.Last: TcxGridDataExportRange;
begin
  Result := TcxGridDataExportRange(inherited Last);
end;

procedure TcxGridDataExportRanges.AddFrom(ARanges: TcxGridDataExportRanges; AIndex: Integer; AOffset: Integer);
var
  I: Integer;
  ARange: TcxGridDataExportRange;
begin
  for I := 0 to ARanges.Count - 1 do
  begin
    StartRange(AIndex, -1);
    ARange := Items[AIndex];
    ARange.Assign(ARanges[I]);
    ARange.Offset(AOffset);
  end;
end;

function TcxGridDataExportRanges.CreateRange(AStart: Integer): TcxGridDataExportRange;
begin
  Result := GetRangeClass.Create(AStart);
end;

procedure TcxGridDataExportRanges.FinishRange(AFinish: Integer);
begin
  FinishRange(Last, AFinish);
end;

procedure TcxGridDataExportRanges.FinishRange(ARange: TcxGridDataExportRange; AFinish: Integer);
begin
  ARange.Finish := AFinish;
end;

function TcxGridDataExportRanges.GetRangeClass: TcxGridDataExportRangeClass;
begin
  Result := TcxGridDataExportRange;
end;

function TcxGridDataExportRanges.HasActiveRange: Boolean;
begin
  Result := (Count <> 0) and Last.IsActive;
end;

procedure TcxGridDataExportRanges.StartRange(AStart: Integer);
begin
  Add(CreateRange(AStart));
end;

procedure TcxGridDataExportRanges.StartRange(AIndex: Integer; AStart: Integer);
begin
  Insert(AIndex, CreateRange(AStart));
end;

function TcxGridDataExportRanges.GetItem(AIndex: Integer): TcxGridDataExportRange;
begin
  Result := TcxGridDataExportRange(inherited Items[AIndex]);
end;

procedure TcxGridDataExportRanges.SetItem(AIndex: Integer; ARange: TcxGridDataExportRange);
begin
  inherited Items[AIndex] := ARange;
end;

{ TcxGridDataExportFooterSubTotalFormulaRanges }

constructor TcxGridDataExportFooterSubTotalFormulaRanges.Create(AParentRow: TcxCustomGridRecord);
begin
  inherited Create;
  FParent := AParentRow;
end;

function TcxGridDataExportFooterSubTotalFormulaRanges.IsParent(AParentRow: TcxCustomGridRecord): Boolean;
var
  ACheckParent: TcxCustomGridRecord;
begin
  Result := AParentRow = nil;
  ACheckParent := Parent;
  while not Result and (ACheckParent <> nil) do
  begin
    Result := ACheckParent = AParentRow;
    ACheckParent := ACheckParent.ParentRecord;
  end;
end;

{ TcxGridDataExportFooterSubTotalFormulaBuilder }

constructor TcxGridDataExportFooterSubTotalFormulaBuilder.Create(AOwnsObjects: Boolean; ACapacity: Integer);
begin
  inherited Create(AOwnsObjects, ACapacity);
  FListSeparator := ',';
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.First: TcxGridDataExportFooterSubTotalFormulaRanges;
begin
  Result := TcxGridDataExportFooterSubTotalFormulaRanges(inherited First);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.Last: TcxGridDataExportFooterSubTotalFormulaRanges;
begin
  Result := TcxGridDataExportFooterSubTotalFormulaRanges(inherited Last);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.AddRanges(AParentRow: TcxCustomGridRecord): TcxGridDataExportFooterSubTotalFormulaRanges;
begin
  Result := TcxGridDataExportFooterSubTotalFormulaRanges.Create(AParentRow);
  Add(Result);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.Build(AParentRow: TcxCustomGridRecord; AColumnIndex: Integer; AKind: TcxSummaryKind): string;
const
  ASubTotalKinds: array[TcxSummaryKind] of Char = ('0', '9', '5', '4', '3', '1');
var
  I, J: Integer;
  AArea: TRect;
begin
  Result := '';
  if AKind = skNone then
    Exit;
  for I := 0 to Count - 1 do
    if IsParentForRanges(AParentRow, Items[I]) then
    begin
      for J := 0 to Items[I].Count - 1 do
      begin
        AArea := Rect(AColumnIndex, Items[I].Items[J].Start, AColumnIndex, Items[I].Items[J].Finish);
        Result := Result + ListSeparator + dxReferenceToString(AArea.Top, AArea.Left) + ':' +
          dxReferenceToString(AArea.Bottom, AArea.Right);
      end;
    end;
  Result := Format('=SUBTOTAL(%s%s)', [ASubTotalKinds[AKind], Result]);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.FindRanges(AParentRow: TcxCustomGridRecord): TcxGridDataExportFooterSubTotalFormulaRanges;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Parent = AParentRow then
      Exit;
  end;
  Result := nil;
end;

procedure TcxGridDataExportFooterSubTotalFormulaBuilder.FinishRange(AFinish: Integer);
begin
  Last.FinishRange(AFinish);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.GetSubTotalDescription(AKind: TcxSummaryKind): string;
const
  ASubTotalDescriptions: array[TcxSummaryKind] of string = ('', '"SUM="', '"MIN="', '"MAX="', '', '"AVG="');
begin
  Result := ASubTotalDescriptions[AKind];
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.HasActiveRange: Boolean;
begin
  Result := (Count <> 0) and Last.HasActiveRange;
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.IsParentForRanges(AParent: TcxCustomGridRecord;
  ARanges: TcxGridDataExportFooterSubTotalFormulaRanges): Boolean;
begin
  Result := ARanges.IsParent(AParent);
end;

procedure TcxGridDataExportFooterSubTotalFormulaBuilder.StartRange(AParentRow: TcxCustomGridRecord; AStart: Integer);
var
  AItem: TcxGridDataExportFooterSubTotalFormulaRanges;
begin
  AItem := FindRanges(AParentRow);
  if AItem = nil then
    AddRanges(AParentRow);
  Last.StartRange(AStart);
end;

function TcxGridDataExportFooterSubTotalFormulaBuilder.GetItem(AIndex: Integer): TcxGridDataExportFooterSubTotalFormulaRanges;
begin
  Result := TcxGridDataExportFooterSubTotalFormulaRanges(inherited Items[AIndex]);
end;

procedure TcxGridDataExportFooterSubTotalFormulaBuilder.SetItem(AIndex: Integer; AObject: TcxGridDataExportFooterSubTotalFormulaRanges);
begin
  inherited Items[AIndex] := AObject;
end;

{ TcxGridDataExportOutlineGroupRange }

constructor TcxGridDataExportOutlineGroupRange.Create(AStart: Integer);
begin
  inherited Create(AStart);
  FLevel := -1;
end;

procedure TcxGridDataExportOutlineGroupRange.Assign(Source: TcxGridDataExportRange);
begin
  inherited Assign(Source);
  if Source is TcxGridDataExportOutlineGroupRange then
    Level := TcxGridDataExportOutlineGroupRange(Source).Level;
end;

{ TcxGridDataExportOutlineGroupRanges }

function TcxGridDataExportOutlineGroupRanges.First: TcxGridDataExportOutlineGroupRange;
begin
  Result := TcxGridDataExportOutlineGroupRange(inherited First);
end;

function TcxGridDataExportOutlineGroupRanges.Last: TcxGridDataExportOutlineGroupRange;
begin
  Result := TcxGridDataExportOutlineGroupRange(inherited Last);
end;

procedure TcxGridDataExportOutlineGroupRanges.FinishLevelRange(AFinish: Integer; ALevel: Integer);
var
  I: Integer;
  ARange: TcxGridDataExportOutlineGroupRange;
begin
  for I := Count - 1 downto 0 do
  begin
    ARange := Items[I];
    if ARange.IsActive and (ARange.Level >= ALevel) then
      FinishRange(ARange, AFinish);
    if ARange.Level <= ALevel then
      Break;
  end;
end;

function TcxGridDataExportOutlineGroupRanges.GetRangeClass: TcxGridDataExportRangeClass;
begin
  Result := TcxGridDataExportOutlineGroupRange;
end;

procedure TcxGridDataExportOutlineGroupRanges.StartLevelRange(AIndex: Integer; AStart: Integer; ALevel: Integer);
begin
  StartRange(AIndex, AStart);
  Items[AIndex].Level := ALevel;
end;

procedure TcxGridDataExportOutlineGroupRanges.StartLevelRange(AStart: Integer; ALevel: Integer);
begin
  StartRange(AStart);
  Last.Level := ALevel;
end;

function TcxGridDataExportOutlineGroupRanges.GetItem(AIndex: Integer): TcxGridDataExportOutlineGroupRange;
begin
  Result := TcxGridDataExportOutlineGroupRange(inherited Items[AIndex]);
end;

procedure TcxGridDataExportOutlineGroupRanges.SetItem(AIndex: Integer; AObject: TcxGridDataExportOutlineGroupRange);
begin
  inherited Items[AIndex] := AObject;
end;

{ TcxGridDataConditionalFormattingRules }

constructor TcxGridDataConditionalFormattingRules.Create;
begin
  inherited Create;
  FData := TObjectDictionary<TdxSpreadSheetCustomConditionalFormattingRule, TdxSpreadSheetAreaList>.Create([doOwnsValues]);
end;

destructor TcxGridDataConditionalFormattingRules.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TcxGridDataConditionalFormattingRules.CellAdded(ASourceRow, ASourceColumn: Integer; const ATargetArea: TRect);
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  for ARule in FData.Keys do
  begin
    if ARule.Areas.Contains(ASourceRow, ASourceColumn) then
      FData.Items[ARule].Add(cxRectBounds(ATargetArea.Left, ATargetArea.Top, 0, 0));
  end;
end;

procedure TcxGridDataConditionalFormattingRules.Commit(AProvider: IcxExportProvider);
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ARuleAreas: TdxSpreadSheetAreaList;
begin
  for ARule in FData.Keys do
  begin
    ARuleAreas := FData.Items[ARule];
    OptimizeAreas(ARuleAreas);
    if ARuleAreas.Count > 0 then
      AProvider.AddConditionalFormattingRule(ARule, ARuleAreas);
  end;
end;

procedure TcxGridDataConditionalFormattingRules.Initialize(
  AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
var
  I: Integer;
begin
  for I := 0 to AConditionalFormatting.RuleCount - 1 do
    FData.Add(AConditionalFormatting.Rules[I], TdxSpreadSheetAreaList.Create);
end;

procedure TcxGridDataConditionalFormattingRules.OptimizeAreas(AAreas: TdxSpreadSheetAreaList);

  function TryCombineAreas(const ASourceArea: TRect; var ATargetArea: TRect): Boolean;
  begin
    Result :=
      ((ASourceArea.Top = ATargetArea.Top) and (ASourceArea.Bottom = ATargetArea.Bottom) and
      ((ASourceArea.Left = ATargetArea.Right + 1) or (ASourceArea.Right + 1 = ATargetArea.Left))) or
      ((ASourceArea.Left = ATargetArea.Left) and (ASourceArea.Right = ATargetArea.Right) and
      ((ASourceArea.Top = ATargetArea.Bottom + 1) or (ASourceArea.Bottom + 1 = ATargetArea.Top)));
    if Result then
      ATargetArea := dxSpreadSheetCellsUnion(ATargetArea, ASourceArea);
  end;

var
  I, J: Integer;
  R: TRect;
begin
  for I := AAreas.Count - 1 downto 0 do
  begin
    R := AAreas[I];
    if not dxSpreadSheetIsValidArea(R) then
      AAreas.Delete(I)
    else
      for J := I - 1 downto 0 do
        if TryCombineAreas(AAreas[J], R) then
        begin
          AAreas[J] := R;
          AAreas.Delete(I);
          Break;
        end;
  end;
end;

{ TcxExportGridViewDetailDataItem }

destructor TcxExportGridViewDetailDataItem.Destroy;
begin
  DestroyDetailExport;
  inherited Destroy;
end;

procedure TcxExportGridViewDetailDataItem.DestroyDetailExport;
begin
  Data.Free;
  Data := nil;
end;

function TcxExportGridViewDetailDataItem.GetDetailExport: TcxGridViewDataCustomExport;
begin
  Result := TcxGridViewDataCustomExport(Data);
end;

procedure TcxExportGridViewDetailDataItem.SetDetailExport(AValue: TcxGridViewDataCustomExport);
begin
  Data := AValue;
end;

procedure TcxExportGridViewDetailDataItem.UpdateBoundByDetailBounds(ALeftOffset, ATopOffset: Integer);
begin
  Bounds := DetailExport.CalculateBounds(ALeftOffset, ATopOffset);
end;

{ TcxGridViewDataExport }

constructor TcxGridViewDataExport.Create(const AFileName: string; AGridView: TcxCustomGridView;
  AGrid: TcxCustomGrid; AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject = nil);
begin
  inherited Create(AFileName, AGridView, AGrid, AViewInfo, AAdapter, AHandler);
  FOutlineGroupRanges := TcxGridDataExportOutlineGroupRanges.Create;
end;

destructor TcxGridViewDataExport.Destroy;
begin
  FreeAndNil(FOutlineGroupRanges);
  inherited Destroy;
end;

procedure TcxGridViewDataExport.AddOutlineGroups;
var
  ARange: TcxGridDataExportOutlineGroupRange;
  I: Integer;
begin
  for I := 0 to OutlineGroupRanges.Count - 1 do
  begin
    ARange := OutlineGroupRanges[I];
    Adapter.AddOutlineGroup(ARange.Start, ARange.Finish);
  end;
end;

function TcxGridViewDataExport.CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect;
begin
  Result := cxRectOffset(inherited CalculateBounds(ALeftOffset, ATopOffset), ALeftOffset, ATopOffset);
  if HasHeader then
    Inc(Result.Bottom);
  Inc(Result.Right, GridView.VisibleItemCount);
  Inc(Result.Bottom, GridView.ViewData.RecordCount);
end;

procedure TcxGridViewDataExport.CreateExportCell(const ABounds: TRect;
  const ADisplayText: string; ARecord: TcxCustomGridRecord; AData: TObject = nil);
begin
  CreateExportCell(ABounds, ADisplayText, ARecord, AData, TcxExportGridViewDataItem);
end;

function TcxGridViewDataExport.CreateExportCell(const ABounds: TRect; const ADisplayText: string;
  ARecord: TcxCustomGridRecord; AData: TObject; AClass: TcxExportGridViewDataItemClass): TcxExportGridViewDataItem;
begin
  Result := TcxExportGridViewDataItem(inherited CreateExportCell(ABounds, ADisplayText, AClass));
  Result.GridRecord := ARecord;
  Result.Data := AData;
end;

procedure TcxGridViewDataExport.CreateExportContent(var ALeft, ATop: Integer);
begin
  if HasHeader then
  begin
    if not GridView.IsDetail then
      Adapter.FreezePanes(-1, 0);
    CreateExportHeader(ALeft, ATop);
  end;
  CreateExportRecords(ALeft, ATop);
end;

procedure TcxGridViewDataExport.CreateExportDataRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord);
var
  AItem: TcxCustomGridTableItem;
  I: Integer;
begin
  CreateIndents(0, ALeft - 1, ATop, ATop);
  for I := 0 to GridView.VisibleItemCount - 1 do
  begin
    AItem := GridView.VisibleItems[I];
    CreateExportCell(GetItemBounds(ALeft + I, ATop), VarToStr(ARecord.Values[AItem.Index]), ARecord, AItem);
  end;
  Inc(ATop);
end;

procedure TcxGridViewDataExport.CreateExportGroupRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord);
begin
  if OutlineGroupRanges.HasActiveRange then
    OutlineGroupRanges.FinishLevelRange(ATop - 1, ARecord.Level);
  if ARecord.Expanded then
    OutlineGroupRanges.StartLevelRange(ATop + 1, ARecord.Level);
  CreateExportCell(GetItemBounds(ARecord.Level, ATop), GetGroupDisplayText(ARecord), ARecord);
  Inc(ATop);
end;

procedure TcxGridViewDataExport.CreateExportRecord(var ALeft, ATop: Integer;
  ARecord: TcxCustomGridRecord);
begin
  if ARecord.IsData then
    CreateExportDataRecord(ALeft, ATop, ARecord)
  else
    CreateExportGroupRecord(ALeft, ATop, ARecord)
end;

procedure TcxGridViewDataExport.CreateExportRecords(var ALeft, ATop: Integer);
var
  I: Integer;
begin
  ProgressHelper.BeginStage(RecordCount);
  try
    for I := 0 to RecordCount - 1 do
    begin
      CreateExportRecord(ALeft, ATop, Records[I]);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
  if OutlineGroupRanges.HasActiveRange then
    OutlineGroupRanges.FinishLevelRange(ATop, 0);
  AddOutlineGroups;
end;

procedure TcxGridViewDataExport.CreateExportHeader(var ALeft, ATop: Integer);
var
  I: Integer;
  AText: string;
begin
  CreateIndents(0, ALeft - 1, ATop, ATop);
  for I := 0 to GridView.VisibleItemCount - 1 do
  begin
    AText := GridView.VisibleItems[I].Caption;
    if Trim(AText) = '' then
      AText := GridView.VisibleItems[I].AlternateCaption;
    CreateExportCell(GetItemBounds(ALeft + I, ATop), AText, nil, GridView.VisibleItems[I]);
  end;
  Inc(ATop);
end;

procedure TcxGridViewDataExport.CreateIndents(AStartLeft, AFinishLeft, AStartTop, AFinishTop: Integer);
var
  I, J: Integer;
begin
  for I := AStartLeft to AFinishLeft do
    for J := AStartTop to AFinishTop do
      CreateExportCell(GetItemBounds(I, J), TcxExportCustomItem);
end;

procedure TcxGridViewDataExport.DoCreateExportCells;
var
  ALeft, ATop: Integer;
begin
  ATop := 0;
  ALeft := DataController.Groups.LevelCount;
  CreateExportContent(ALeft, ATop);
end;

procedure TcxGridViewDataExport.ExportCell(ACell: TcxExportCustomItem; ARect: TRect);
var
  ATableItem: TcxCustomGridTableItem;
begin
  if (ACell is TcxExportGridViewDataItem) and (
    TcxExportGridViewDataItem(ACell).GridRecord <> nil) and
    TcxExportGridViewDataItem(ACell).GridRecord.IsData then
  begin
    ATableItem := TcxCustomGridTableItem(TcxExportGridViewDataItem(ACell).Data);
    ConditionalFormattingRules.CellAdded(TcxExportGridViewDataItem(ACell).GridRecord.RecordIndex, ATableItem.Index, ACell.Bounds);
    SetRealCellStyleAndValueEx(ACell.Bounds, ARect, GetStyleIndex(ACell), TcxExportGridViewDataItem(ACell).GridRecord, ATableItem);
  end
  else
    inherited ExportCell(ACell, ARect);
end;

function TcxGridViewDataExport.GetGroupDisplayText(ARecord: TcxCustomGridRecord): string;
begin
  Result := ARecord.DisplayTexts[-1];
end;

function TcxGridViewDataExport.GetStyleIndex(ACell: TcxExportCustomItem): Integer;

  function IsContentCell(ACell: TcxExportCustomItem): Boolean;
  begin
    if ACell is TcxExportGridViewDataItem then
    begin
      Result :=
        (TcxExportGridViewDataItem(ACell).GridRecord = nil) and (TcxExportGridViewDataItem(ACell).Data <> nil) or
        (TcxExportGridViewDataItem(ACell).GridRecord <> nil) and not TcxExportGridViewDataItem(ACell).GridRecord.IsData;
    end
    else
      if ACell is TcxExportDataItem then
        Result := TcxExportDataItem(ACell).Data <> nil
      else
        Result := False;
  end;

begin
  if IsContentCell(ACell) then
    Result := ContentStyleIndex
  else
    Result := inherited GetStyleIndex(ACell);
end;

function TcxGridViewDataExport.HasHeader: Boolean;
begin
  Result := True;
end;

function TcxGridViewDataExport.HasSelectedRecords: Boolean;
begin
  Result := GridView.Controller.SelectedRecordCount > 0;
end;

function TcxGridViewDataExport.RegisterContentDataStyle: Integer;
var
  AStyle: TcxCacheCellStyle;
begin
  AStyle := Adapter.GetStyle(DataStyleIndex);
  AStyle.FontStyle := AStyle.FontStyle + [fsBold];
  Result := Adapter.RegisterStyle(AStyle);
end;

procedure TcxGridViewDataExport.RegisterStyles;
begin
  inherited RegisterStyles;
  FContentStyleIndex := RegisterContentDataStyle;
end;

function TcxGridViewDataExport.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

{ TcxGridTableViewDataExport }

procedure TcxGridTableViewDataExport.AddDetailOutlineGroupRanges(ADetailExport: TcxGridViewDataExport; ATop: Integer);
begin
  OutlineGroupRanges.AddFrom(ADetailExport.OutlineGroupRanges, 0, ATop);
end;

procedure TcxGridTableViewDataExport.AddOutlineGroups;
begin
  if not GridView.IsDetail then
    inherited AddOutlineGroups;
end;

function TcxGridTableViewDataExport.CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect;
var
  I, J: Integer;
  ARealLevel: Integer;
  ARecord: TcxCustomGridRecord;
  AItems: TcxDataSummaryItems;
  AValues: PVariant;
begin
  Result := inherited CalculateBounds(ALeftOffset, ATopOffset);
  for I := 0 to GridView.ViewData.RecordCount - 1 do
  begin
    ARecord := GridView.ViewData.Records[I];
    for J := 0 to ARecord.Level do
    begin
      ARealLevel := J;
      if TcxGridExportHelper.HasGroupFooter(GridView, ARecord, ARealLevel) then
        if DataController.Summary.GetGroupSummaryInfo(ARecord.Index, AItems, AValues) then
          Inc(Result.Bottom, TcxGridExportHelper.CalculateFooterLineCount(AItems));
    end;
  end;
  if HasFooter then
    Inc(Result.Bottom, TcxGridExportHelper.CalculateFooterLineCount(DataController.Summary.FooterSummaryItems));
end;

constructor TcxGridTableViewDataExport.Create(const AFileName: string; AGridView: TcxCustomGridView; AGrid: TcxCustomGrid;
  AViewInfo: TcxCustomGridViewInfo; AAdapter: IcxGridExportProviderAdapter; AHandler: TObject);
begin
  inherited Create(AFileName, AGridView, AGrid, AViewInfo, AAdapter, AHandler);
  FFooterSubTotalFormulaBuilder := TcxGridDataExportFooterSubTotalFormulaBuilder.Create(True, 1024);
end;

procedure TcxGridTableViewDataExport.CreateExportContent(var ALeft, ATop: Integer);
begin
  inherited CreateExportContent(ALeft, ATop);
  if HasFooter then
    CreateExportFooters(ALeft, ATop);
end;

procedure TcxGridTableViewDataExport.CreateExportDataRecord(var ALeft: Integer; var ATop: Integer; ARecord: TcxCustomGridRecord);
var
  AParent: TcxCustomGridRecord;
begin
  if not FooterSubTotalFormulaBuilder.HasActiveRange then
  begin
    AParent := ARecord.ParentRecord;
    if (AParent <> nil) and AParent.IsData then
      AParent := nil;
    FooterSubTotalFormulaBuilder.StartRange(AParent, Offset.Y + ATop);
  end;
  if GridView.IsMaster then
    CreateExportMasterDataRow(ALeft, ATop, ARecord as TcxGridMasterDataRow)
  else
    inherited CreateExportDataRecord(ALeft, ATop, ARecord);
  if FooterSubTotalFormulaBuilder.HasActiveRange and (ARecord.IsLast or not GridView.ViewData.Records[ARecord.Index + 1].IsData) then
    FooterSubTotalFormulaBuilder.FinishRange(Offset.Y + ATop - 1);
end;

procedure TcxGridTableViewDataExport.CreateExportFooterByColumn(ALeft, ATop: Integer; AColumn: TcxGridColumn);
var
  I: Integer;
  AFormula, ADisplayText: string;
  AItem: TcxDataSummaryItem;
begin
  for I := 0 to DataController.Summary.FooterSummaryItems.Count - 1 do
  begin
    AItem := DataController.Summary.FooterSummaryItems[I];
    if (AItem.Position <> spFooter) or (AItem.ItemLink <> AColumn) then
      Continue;
    AFormula := FooterSubTotalFormulaBuilder.Build(nil, Offset.X + ALeft + AColumn.VisibleIndex, AItem.Kind);
    ADisplayText := AItem.FormatValue(DataController.Summary.FooterSummaryValues[I], True);
    CreateExportFormulaCell(GetItemBounds(ALeft + AColumn.VisibleIndex, ATop), AFormula, ADisplayText, AItem);
    Inc(ATop);
  end;
end;

procedure TcxGridTableViewDataExport.CreateExportFooters(var ALeft, ATop: Integer);
var
  I, ALineCount: Integer;
begin
  for I := 0 to GridView.VisibleColumnCount - 1 do
    CreateExportFooterByColumn(ALeft, ATop, GridView.VisibleColumns[I]);
  ALineCount := GetFooterLineCount;
  CreateIndents(0, ALeft - 1, ATop, ATop + ALineCount - 1);
  Inc(ATop, ALineCount);
end;

procedure TcxGridTableViewDataExport.CreateExportFormulaCell(
  const ABounds: TRect; const AFormula, ADisplayText: string; AData: TObject);
var
  AItem: TcxExportGridViewFormulaDataItem;
begin
  AItem := TcxExportGridViewFormulaDataItem(CreateExportCell(ABounds, ADisplayText, TcxExportGridViewFormulaDataItem));
  AItem.Formula := AFormula;
  AItem.Data := AData;
end;

procedure TcxGridTableViewDataExport.CreateExportGroupFooterByColumn(ALeft, ATop: Integer; ARow: TcxCustomGridRecord;
  AColumn: TcxGridColumn);
var
  ALevelGroupedItemIndex, ASummaryItemIndex: Integer;
  AFormula, ADisplayText: string;
  AValues: PVariant;
  AItem: TcxDataSummaryItem;
  AItems: TcxDataSummaryItems;
begin
  for ALevelGroupedItemIndex := 0 to DataController.Groups.GetLevelGroupedItemCount(ARow.Level) - 1 do
  begin
    if not DataController.Summary.GetGroupSummaryInfo(ARow.Index, AItems, AValues, ALevelGroupedItemIndex) then
      Continue;
    for ASummaryItemIndex := 0 to AItems.Count - 1 do
    begin
      AItem := AItems[ASummaryItemIndex];
      if (AItem.Position <> spFooter) or (AItem.ItemLink <> AColumn) then
        Continue;
      AFormula := FooterSubTotalFormulaBuilder.Build(ARow, Offset.Y + ALeft + AColumn.VisibleIndex, AItem.Kind);
      ADisplayText := AItem.FormatValue(AValues^[ASummaryItemIndex], True);
      CreateExportFormulaCell(GetItemBounds(ALeft + AColumn.VisibleIndex, ATop), AFormula, ADisplayText, AItem);
      Inc(ATop);
    end;
  end;
end;

procedure TcxGridTableViewDataExport.CreateExportGroupRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord);
begin
  if FooterSubTotalFormulaBuilder.HasActiveRange then
    FooterSubTotalFormulaBuilder.FinishRange(Offset.Y + ATop - 1);
  inherited CreateExportGroupRecord(ALeft, ATop, ARecord);
end;

procedure TcxGridTableViewDataExport.CreateExportMasterDataRow(var ALeft, ATop: Integer; ARecord: TcxGridMasterDataRow);
var
  ADetailView: TcxCustomGridView;
  AItem: TcxExportGridViewDetailDataItem;
  ALeftIndent: Integer;
  ARange: TcxGridDataExportOutlineGroupRange;
begin
  inherited CreateExportDataRecord(ALeft, ATop, ARecord);
  if ARecord.Expanded then
  begin
    if FooterSubTotalFormulaBuilder.HasActiveRange then
      FooterSubTotalFormulaBuilder.FinishRange(Offset.Y + ATop - 1);
    OutlineGroupRanges.StartLevelRange(0, ATop, ARecord.Level);
    ARange := OutlineGroupRanges.First;
    try
      ALeftIndent := ALeft + 1;
      ADetailView := ARecord.ActiveDetailGridView;
      AItem := TcxExportGridViewDetailDataItem(CreateExportCell(cxEmptyRect, '', ARecord,
        GetExportDataClassByGridView(ADetailView).CreateFrom(Self, ADetailView, ADetailView.ViewInfo, Point(ALeftIndent, ATop)),
        TcxExportGridViewDetailDataItem));
      AItem.UpdateBoundByDetailBounds(ALeftIndent, ATop);
      AItem.DetailExport.Offset := Point(ALeftIndent, ATop);
      AItem.DetailExport.Initialize;
      AItem.DetailExport.CreateExportCells;
      if AItem.DetailExport is TcxGridViewDataExport then
        AddDetailOutlineGroupRanges(TcxGridViewDataExport(AItem.DetailExport), ATop);
      Inc(ATop, cxRectHeight(AItem.Bounds));
    finally
      OutlineGroupRanges.FinishRange(ARange, ATop - 1);
    end;
  end;
end;

procedure TcxGridTableViewDataExport.CreateExportRecord(var ALeft, ATop: Integer; ARecord: TcxCustomGridRecord);
begin
  inherited CreateExportRecord(ALeft, ATop, ARecord);
  CreateExportRowFooters(ALeft, ATop, ARecord);
end;

procedure TcxGridTableViewDataExport.CreateExportRowFooter(var ALeft, ATop: Integer; ADataLevel, ALevel: Integer; ARow: TcxCustomGridRecord);
var
  I, ALineCount: Integer;
begin
  while ARow.Level > ADataLevel do
    ARow := ARow.ParentRecord;
  for I := 0 to GridView.VisibleColumnCount - 1 do
    CreateExportGroupFooterByColumn(ALeft, ATop, ARow, GridView.VisibleColumns[I]);
  ALineCount := GetGroupFooterLineCount(ARow);
  CreateIndents(0, ALeft - 1, ATop, ATop + ALineCount - 1);
  Inc(ATop, ALineCount);
end;

procedure TcxGridTableViewDataExport.CreateExportRowFooters(var ALeft, ATop: Integer; ARow: TcxCustomGridRecord);
var
  ALevel, ARealLevel: Integer;
begin
  for ALevel := 0 to ARow.Level do
  begin
    ARealLevel := ALevel;
    if TcxGridExportHelper.HasGroupFooter(GridView, ARow, ARealLevel) then
      CreateExportRowFooter(ALeft, ATop, ARealLevel, ARow.Level - ALevel, ARow);
  end;
end;

destructor TcxGridTableViewDataExport.Destroy;
begin
  FreeAndNil(FFooterSubTotalFormulaBuilder);
  inherited Destroy;
end;

procedure TcxGridTableViewDataExport.ExportCell(ACell: TcxExportCustomItem; ARect: TRect);
begin
  if ACell is TcxExportGridViewDetailDataItem then
    ExportDetailCell(TcxExportGridViewDetailDataItem(ACell))
  else
    if (ACell is TcxExportDataItem) and (TcxExportDataItem(ACell).Data is TcxDataSummaryItem) then
      ExportFooterCell(TcxExportDataItem(ACell), ARect)
    else
      inherited ExportCell(ACell, ARect);
end;

procedure TcxGridTableViewDataExport.ExportDetailCell(ACell: TcxExportGridViewDetailDataItem);
begin
  try
    ACell.DetailExport.ExportCells;
  finally
    ACell.DetailExport.Finalize;
  end
end;

procedure TcxGridTableViewDataExport.ExportFooterCell(ACell: TcxExportDataItem; ARect: TRect);
var
  AFormula: string;
begin
  if ACell is TcxExportGridViewFormulaDataItem then
    AFormula := TcxExportGridViewFormulaDataItem(ACell).Formula
  else
    AFormula := '';

  SetRealCellStyleAndFormula(ACell.Bounds, ARect, GetStyleIndex(ACell), AFormula,
    ACell.DisplayText, GetFooterFormatCode(ACell), FooterSubTotalFormulaBuilder.ListSeparator);
end;

function TcxGridTableViewDataExport.GetFooterFormatCode(ACell: TcxExportDataItem): string;
var
  ACol, ARow: Integer;
  ASummaryItem: TcxDataSummaryItem;
begin
  ASummaryItem := TcxDataSummaryItem(ACell.Data);
  ARow := FooterSubTotalFormulaBuilder.First.First.Start;
  ACol := Offset.X + ACell.Bounds.Left;
  if not (ASummaryItem.Kind in [skNone, skCount]) then
    Result := FooterSubTotalFormulaBuilder.GetSubTotalDescription(ASummaryItem.Kind) + Adapter.GetFormatCode(ACol, ARow)
  else
    Result := '';
end;

function TcxGridTableViewDataExport.GetFooterLineCount: Integer;
begin
  Result := TcxGridExportHelper.CalculateFooterLineCount(DataController.Summary.FooterSummaryItems);
end;

function TcxGridTableViewDataExport.GetGroupDisplayText(ARecord: TcxCustomGridRecord): string;
begin
  Result := TcxGridGroupRow(ARecord).DisplayText;
end;

function TcxGridTableViewDataExport.GetGroupFooterLineCount(ARow: TcxCustomGridRecord): Integer;
var
  I: Integer;
  AItems: TcxDataSummaryItems;
  AValues: PVariant;
begin
  Result := 0;
  for I := 0 to DataController.Groups.GetLevelGroupedItemCount(ARow.Level) - 1 do
    if DataController.Summary.GetGroupSummaryInfo(ARow.Index, AItems, AValues, I) then
      Inc(Result, TcxGridExportHelper.CalculateFooterLineCount(AItems));
end;

function TcxGridTableViewDataExport.HasFooter: Boolean;
begin
  Result := GridView.OptionsView.Footer and (DataController.Summary.FooterSummaryItems.Count > 0);
end;

function TcxGridTableViewDataExport.HasHeader: Boolean;
begin
  Result := GridView.OptionsView.Header;
end;

function TcxGridTableViewDataExport.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

{ TcxGridWinExplorerViewDataExport }

function TcxGridWinExplorerViewDataExport.GetGroupDisplayText(ARecord: TcxCustomGridRecord): string;
begin
  Result := TcxGridWinExplorerViewGroupRecord(ARecord).DisplayCaption;
end;

{ TcxGridChartViewDataExport }

function TcxGridChartViewDataExport.CalculateBounds(ALeftOffset, ATopOffset: Integer): TRect;
begin
  Result := inherited CalculateBounds(ALeftOffset, ATopOffset);
  Result := cxRectOffset(Result, Point(ALeftOffset, ATopOffset));
  if HasCategory then
    Inc(Result.Right, GridView.ViewData.VisibleCategoryCount + 1);
  if HasSeries then
    Inc(Result.Bottom, GridView.VisibleSeriesCount + 1);
end;

procedure TcxGridChartViewDataExport.CreateExportCategories(
  ASeries: TcxGridChartSeries; AHasLeftIndent, AHasTopIndent: Boolean);
var
  I: Integer;
  ALeft, ATop: Integer;
  ARect: TRect;
  ADisplayText: string;
begin
  for I := 0 to GridView.ViewData.VisibleCategoryCount - 1 do
  begin
    ALeft := ASeries.VisibleIndex;
    if AHasLeftIndent then
      Inc(ALeft);
    ATop := I;
    if AHasTopIndent then
      Inc(ATop);
    ARect := GetItemBounds(ALeft, ATop);
    ADisplayText := ASeries.VisibleValues[I];
    CreateExportCell(ARect, ADisplayText, TcxExportTextItem);
  end;
end;

procedure TcxGridChartViewDataExport.CreateExportCategoryDescriptions;
var
  I, ATop: Integer;
  ADisplayText: string;
  ARect: TRect;
begin
  for I := 0 to GridView.ViewData.VisibleCategoryCount - 1 do
  begin
    ATop := I;
    if HasSeries then
      Inc(ATop);
    ADisplayText := GridView.ViewData.VisibleCategories[I];
    ARect := GetItemBounds(0, ATop);
    CreateExportCell(ARect, ADisplayText, TcxExportTextItem);
  end;
end;

procedure TcxGridChartViewDataExport.CreateExportSeries;
var
  I: Integer;
begin
  ProgressHelper.BeginStage(1);
  try
    for I := 0 to GridView.VisibleSeriesCount - 1 do
      CreateExportCategories(GridView.VisibleSeries[I], HasCategory, HasSeries);
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TcxGridChartViewDataExport.CreateExportSeriesDescriptions;
var
  I, ALeft: Integer;
begin
  for I := 0 to GridView.VisibleSeriesCount - 1 do
  begin
    ALeft := I;
    if HasCategory then
      Inc(ALeft);
    CreateExportCell(GetItemBounds(ALeft, 0), GridView.VisibleSeries[I].GetDisplayText, TcxExportTextItem);
  end;
end;

procedure TcxGridChartViewDataExport.DoCreateExportCells;
begin
  CreateExportSeriesDescriptions;
  CreateExportCategoryDescriptions;
  CreateExportSeries;
end;

function TcxGridChartViewDataExport.HasCategory: Boolean;
begin
  Result := GridView.ViewData.VisibleCategoryCount > 0;
end;

function TcxGridChartViewDataExport.HasSeries: Boolean;
begin
  Result := GridView.VisibleSeriesCount > 0;
end;

procedure TcxGridChartViewDataExport.RegisterStyles;
begin
  DefaultStyle := DefaultCellStyle;
  inherited RegisterStyles;
end;

function TcxGridChartViewDataExport.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

{ TcxGridExportHelper }

class function TcxGridExportHelper.CalculateFooterLineCount(AItems: TcxDataSummaryItems): Integer;
var
  I, J, ACount: Integer;
begin
  Result := 1;
  if AItems = nil
    then Exit;
  for I := 0 to AItems.Count - 1 do
  begin
    ACount := 0;
    for J := 0 to AItems.Count - 1 do
      if IsSameFooterItems(AItems[I], AItems[J]) then
        Inc(ACount);
    Result := Max(Result, ACount);
  end;
end;

class function TcxGridExportHelper.HasGroupFooter(AGridView: TcxGridTableView; ARow: TcxCustomGridRecord; var ALevel: Integer): Boolean;
var
  AGroupFootersMode: TcxGridGroupFootersMode;
begin
  AGroupFootersMode := AGridView.OptionsView.GroupFooters;
  if AGroupFootersMode = gfInvisible then
    Result := False
  else
  begin
    if AGroupFootersMode = gfAlwaysVisible then
      Dec(ALevel);
    Result := (0 <= ALevel) and (ALevel < ARow.Level) and TcxCustomGridRecordAccess(ARow).IsParentRecordLast[ALevel] and
      AGridView.Controller.CanShowGroupFooter(ARow.Level - 1 - ALevel);
  end;
  if ARow is TcxGridGroupRow then
    Result := Result or (AGroupFootersMode = gfAlwaysVisible) and (ALevel = -1) and
      not ARow.Expanded and AGridView.Controller.CanShowGroupFooter(ARow.Level);
  if Result then
    ALevel := ARow.Level - ALevel - 1;
end;

class function TcxGridExportHelper.IsSameFooterItems(AItem1, AItem2: TcxDataSummaryItem): Boolean;
begin
  Result := (AItem1.ItemLink = AItem2.ItemLink) and (AItem1.Position = spFooter) and (AItem2.Position = spFooter);
end;

end.
