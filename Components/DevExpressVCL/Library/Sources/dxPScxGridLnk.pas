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
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxGridLnk;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls, ExtCtrls, ImgList, Dialogs,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters,
  ExtDlgs, Menus, cxGraphics, cxClasses, cxControls, cxCustomData, cxDataStorage, cxData, cxDBData, cxGridDBDataDefinitions,
  cxGrid, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridCardView, cxGridDBCardView,
  cxGridBandedTableView, cxGridDBBandedTableView, cxGridChartView, cxGridDBChartView, cxGridWinExplorerView, cxEdit,
  cxGridDBWinExplorerView, cxGridLevel, cxStyles, cxLookAndFeels, cxGridCustomLayoutView, cxLookAndFeelPainters,
  cxFilter, cxTextEdit, cxCheckBox, cxImage, dxBase, dxPSSngltn, dxExtCtrls, dxPSCore, dxPrnPg, dxPScxCommon,
  dxCore, cxContainer, cxLabel, cxPC, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxColorComboBox, cxButtons,
  dxPSReportRenderCanvas, cxGeometry, cxNavigator, dxCoreClasses, dxServerModeData, cxGridServerModeTableView,
  cxGridServerModeBandedTableView, cxTrackBar, cxImageList, dxSpreadSheetConditionalFormatting,
  cxDataControllerConditionalFormatting, dxSpreadSheetStyles;

const
  vspsGridFirst = 0;
  vspsGridBandHeader = vspsGridFirst + 0;
  vspsGridCaption = vspsGridFirst + 1;
  vspsGridCardCaptionRow = vspsGridFirst + 2;
  vspsGridCardRowCaption = vspsGridFirst + 3;
  vspsGridContent = vspsGridFirst + 4;
  vspsGridContentEven = vspsGridFirst + 5;
  vspsGridContentOdd = vspsGridFirst + 6;
  vspsGridFilterBar = vspsGridFirst + 7;
  vspsGridFooter = vspsGridFirst + 8;
  vspsGridGroup = vspsGridFirst + 9;
  vspsGridHeader = vspsGridFirst + 10;
  vspsGridPreview = vspsGridFirst + 11;
  vspsGridSelection = vspsGridFirst + 12;
  vspsGridLast = vspsGridFirst + 12;

  vsCardShadow = vsCardViewLast + 1;
  vsCardViewLast = vsCardShadow;

  bbTabs = -1;

  dxDefaultInterRecordsSpaceHorz = 4;
  dxDefaultInterRecordsSpaceVert = 4;
  dxDefaultCardsShadowDepth = 4;
  dxDefaultCardsShadowColor = clBlack;

  dxGridAttributeIDBase = 0;
  dxGridUndefinedID = dxGridAttributeIDBase + 0;
  dxGridBandID = dxGridAttributeIDBase + 1;
  dxGridCardRowCaptionID = dxGridAttributeIDBase + 2;
  dxGridCardRowDataID = dxGridAttributeIDBase + 3;
  dxGridFilterBarID = dxGridAttributeIDBase + 4;
  dxGridFooterID = dxGridAttributeIDBase + 5;
  dxGridGroupFooterID = dxGridAttributeIDBase + 6;
  dxGridHeaderID = dxGridAttributeIDBase + 7;
  dxGridLevelCaptionID = dxGridAttributeIDBase + 8;
  dxGridRecordID = dxGridAttributeIDBase + 9;

type
  TdxGridAttributeHostInfo = class;
  TdxGridAttributeHostInfoServices = class;
  TdxGridReportLink = class;
  TdxGridReportLinkOptionsExpanding = class;
  TdxfmGridReportLinkDesignWindow = class;

  TdxCustomGridViewAdapter = class;
  TdxCustomGridViewFormatter = class;
  TdxCustomGridViewBuilder = class;

  TdxCustomGridTableViewAdapter = class;
  TdxCustomGridTableViewBuilder = class;

  TdxGridTableViewAdapter = class;
  TdxGridTableViewFormatter = class;
  TdxGridTableViewBuilder = class;

  TdxGridBandedTableViewAdapter = class;
  TdxGridBandedTableViewFormatter = class;
  TdxGridBandedTableViewBuilder = class;

  TdxGridCustomLayoutViewBuilder = class;
  TdxGridCardViewBuilder = class;

  TdxGridWinExplorerViewAdapter = class;
  TdxGridWinExplorerViewBuilder = class;

  TdxGridChartViewBuilder = class;

  TdxGridTableViewColumnPlace = class;
  TdxGridTableViewColumnPlaceController  = class;
  TdxCustomGridBandedTableViewItemPlace = class;
  TdxGridBandedTableViewItemPlace = class;
  TdxGridBandedTableViewItemPlaceController = class;

  TdxGridViewRowProducerClass = class of TdxGridViewRowProducer;
  TdxGridViewRowProducer = class;
  TdxGridViewCaptionProducerClass = class of TdxGridViewCaptionProducer;
  TdxGridViewCaptionProducer = class;
  TdxGridViewFilterBarProducerClass = class of TdxGridViewFilterBarProducer;
  TdxGridViewFilterBarProducer = class;
  TdxGridViewDetailsSeparatorProducerClass = class of TdxGridViewDetailsSeparatorProducer;
  TdxGridViewDetailsSeparatorProducer = class;
  TdxGridViewTerminatorProducerClass = class of TdxGridViewTerminatorProducer;
  TdxGridViewTerminatorProducer = class;
  TdxGridTableViewHeadersProducerClass = class of TdxGridTableViewHeadersProducer;
  TdxGridTableViewHeadersProducer = class;
  TdxGridTableViewFootersProducerClass = class of TdxGridTableViewFootersProducer;
  TdxGridTableViewFootersProducer = class;
  TdxGridTableViewCustomDataRowProducerClass = class of TdxGridTableViewCustomDataRowProducer;
  TdxGridTableViewCustomDataRowProducer = class;
  TdxGridTableViewRowSeparatorProducerClass = class of TdxGridTableViewRowSeparatorProducer;
  TdxGridTableViewRowSeparatorProducer = class;
  TdxGridTableViewGroupFooterProducerClass = class of TdxGridTableViewGroupFooterProducer;
  TdxGridTableViewGroupFooterProducer = class;
  TdxGridTableViewGroupRowSeparatorProducerClass = class of TdxGridTableViewGroupRowSeparatorProducer;
  TdxGridTableViewGroupRowSeparatorProducer = class;
  TdxGridTableViewBandsProducerClass = class of TdxGridTableViewBandsProducer;
  TdxGridTableViewBandsProducer = class;
  TdxGridCardViewCardsRowProducerClass = class of TdxGridCardViewCardsRowProducer;
  TdxGridCardViewCardsRowProducer = class;
  TdxGridCustomLayoutViewRecordProducerClass = class of TdxGridCustomLayoutViewRecordProducer;
  TdxGridCustomLayoutViewRecordProducer = class;
  TdxGridWinExplorerViewRecordProducer = class;
  TdxGridChartViewChartProducerClass = class of TdxGridChartViewChartProducer;
  TdxGridChartViewChartProducer = class;

  { View Attributes }

  TdxGridAttributeClass = class of TdxGridAttribute;
  TdxGridAttributeClasses = array of TdxGridAttributeClass;
  TdxGridAttribute = class end;

  TdxGridBandHeader = class(TdxGridAttribute);
  TdxGridCardRowCaption = class(TdxGridAttribute);
  TdxGridCardRowData = class(TdxGridAttribute);
  TdxGridDetails = class(TdxGridAttribute);
  TdxGridExpandButton = class(TdxGridAttribute);
  TdxGridFilterBar = class(TdxGridAttribute);
  TdxGridFooter = class(TdxGridAttribute);
  TdxGridGroupFooter = class(TdxGridAttribute);
  TdxGridGroupRow = class(TdxGridAttribute);
  TdxGridHeader = class(TdxGridAttribute);
  TdxGridLevelCaption = class(TdxGridAttribute);
  TdxGridPreview = class(TdxGridAttribute);

  TdxGridAttributeID = type Integer;


  TdxGridCellCustomDrawInfo = record
    GridAttributeID: TdxGridAttributeID;
    GridView: TcxCustomGridView;
    GridRecord: TcxCustomGridRecord;
    GridColumn: TcxGridColumn;
    GroupLevel: Integer;
    GridBand: TcxGridBand;
    GridCard: TcxGridCard;
    GridCardRow: TcxGridCardViewRow;
    (*case Byte of
      0: (GridRecord: TcxCustomGridRecord;
          GridColumn: TcxGridColumn;
          GroupLevel: Integer;
          GridBand: TcxGridBand);
      1: (GridCard: TcxGridCard;
          GridCardRow: TcxGridCardViewRow); *)
  end;


  TdxAttributeColorKind = (ackContentColor, ackTextColor);

  TdxVerticalDetailsSeparatorKind = (vdskLeading, vdskTrailing);

  { View Helpers }

  TdxCustomGridViewHelperClass = class of TdxCustomGridViewHelper;

  TdxCustomGridViewHelper = class(TdxCustomClassMapItem)
  private
    FView: TcxCustomGridView;
  protected
    class procedure AddAttribute(var AnAtributes: TdxGridAttributeClasses; AnAttribute: TdxGridAttributeClass);
    class function FilterPosition(AView: TcxCustomGridView): TcxGridFilterPosition; virtual;

    class function IsFilterBarAtBottom(AView: TcxCustomGridView): Boolean; virtual;
    class function IsFilterBarAtTop(AView: TcxCustomGridView): Boolean; virtual;
    class function IsOffice11StyleGrouping(AView: TcxCustomGridView): Boolean; virtual;
    class function ViewClass: TcxCustomGridViewClass; virtual;
  public
    constructor Create(AView: TcxCustomGridView); virtual;

    class function PairClass: TClass; override;
    class procedure Register;
    class procedure Unregister;

    class function ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID; virtual;
    class procedure ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
      out ACustomDrawInfo: TdxGridCellCustomDrawInfo); virtual;
    class function ExtractGridView(AnItem: TdxReportVisualItem): TcxCustomGridView; virtual;
    class function HasData(AView: TcxCustomGridView): Boolean; virtual;
    class function IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean; virtual;
    class function SupportedAttributes: TdxGridAttributeClasses; virtual;

    property View: TcxCustomGridView read FView write FView;
  end;

  TdxNullGridViewHelper = class(TdxCustomGridViewHelper)
  protected
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean; override;
  end;

  TdxCustomGridTableViewHelper = class(TdxCustomGridViewHelper)
  protected
    class function ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord; virtual;
    class function FilterPosition(AView: TcxCustomGridView): TcxGridFilterPosition; override;
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID; override;
    class function HasData(AView: TcxCustomGridView): Boolean; override;
    class function SupportedAttributes: TdxGridAttributeClasses; override;
  end;

  TdxGridCardViewHelper = class(TdxCustomGridTableViewHelper)
  protected
    class function ExtractCard(AnItem: TdxReportVisualItem): TcxGridCard; virtual;
    class function ExtractCardRow(AnItem: TdxReportVisualItem): TcxGridCardViewRow; virtual;
    class function ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord; override;
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID; override;
    class procedure ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
      out ACustomDrawInfo: TdxGridCellCustomDrawInfo); override;
  end;

  TdxGridTableViewHelper = class(TdxCustomGridTableViewHelper)
  protected
    class function ExtractColumn(AnItem: TdxReportVisualItem): TcxGridColumn; virtual;
    class function ExtractGroupLevel(AnItem: TdxReportVisualItem): Integer; virtual;
    class function ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord; override;
    class function IsOffice11StyleGrouping(AView: TcxCustomGridView): Boolean; override;
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID; override;
    class procedure ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
      out ACustomDrawInfo: TdxGridCellCustomDrawInfo); override;
    class function SupportedAttributes: TdxGridAttributeClasses; override;
  end;

  TdxGridBandedTableViewHelper = class(TdxGridTableViewHelper)
  protected
    class function ExtractBand(AnItem: TdxReportVisualItem): TcxGridBand; virtual;
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID; override;
    class procedure ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
      out ACustomDrawInfo: TdxGridCellCustomDrawInfo); override;
    class function SupportedAttributes: TdxGridAttributeClasses; override;
  end;

  TdxGridChartViewHelper = class(TdxCustomGridViewHelper)
  protected
    class function ViewClass: TcxCustomGridViewClass; override;
  public
    class function HasData(AView: TcxCustomGridView): Boolean; override;
  end;

  { Record Helpers }

  TdxCustomGridRecordHelperClass = class of TdxCustomGridRecordHelper;

  TdxCustomGridRecordHelper = class(TdxCustomClassMapItem)
  private
    FAdapter: TdxCustomGridTableViewAdapter;
    FGridRecord: TcxCustomGridRecord;
  protected
    function Adapter: TdxCustomGridTableViewAdapter; overload; virtual;
    class function ProducerClass: TdxGridViewRowProducerClass; virtual;
    class function RecordClass: TcxCustomGridRecordClass; virtual;

    function GetCanCellMergingAsMaster: Boolean; virtual;
    function GetCanCellMergingAsSlave: Boolean; virtual;
    function GetHasDetails: Boolean; virtual;
    function GetHasExpandButton: Boolean; virtual;
    function GetHasSelectedChildren: Boolean; virtual;
    function GetIsCellMergingSeparator: Boolean; virtual;
    function GetParent: TcxCustomGridRecord; virtual;
  public
    constructor Create(AAdapter: TdxCustomGridTableViewAdapter); virtual;

    class function PairClass: TClass; override;
    class procedure Register;
    class procedure Unregister;

    property CanCellMergingAsMaster: Boolean read GetCanCellMergingAsMaster;
    property CanCellMergingAsSlave: Boolean read GetCanCellMergingAsSlave;
    property GridRecord: TcxCustomGridRecord read FGridRecord write FGridRecord;
    property HasDetails: Boolean read GetHasDetails;
    property HasExpandButton: Boolean read GetHasExpandButton;
    property HasSelectedChildren: Boolean read GetHasSelectedChildren;
    property IsCellMergingSeparator: Boolean read GetIsCellMergingSeparator;
    property Parent: TcxCustomGridRecord read GetParent;
  end;

  TdxCustomGridRowHelper = class(TdxCustomGridRecordHelper)
  protected
    function Adapter: TdxGridTableViewAdapter; reintroduce; overload;
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;
  end;

  TdxGridDataRowHelper = class(TdxCustomGridRowHelper)
  private
    function GetRow: TcxGridDataRow;
  protected
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;
  public
    property Row: TcxGridDataRow read GetRow;
  end;

  TdxGridGroupRowHelper = class(TdxCustomGridRowHelper)
  private
    function GetGroupRow: TcxGridGroupRow;
  protected
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;

    function GetCanCellMergingAsMaster: Boolean; override;
    function GetCanCellMergingAsSlave: Boolean; override;
    function GetHasExpandButton: Boolean; override;
    function GetHasSelectedChildren: Boolean; override;
  public
    property GroupRow: TcxGridGroupRow read GetGroupRow;
  end;

  TdxGridMasterDataRowHelper = class(TdxCustomGridRowHelper)
  private
    function GetDetailView: TcxCustomGridView;
    function GetMasterRow: TcxGridMasterDataRow;
  protected
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;

    function GetCanCellMergingAsMaster: Boolean; override;
    function GetHasDetails: Boolean; override;
    function GetHasExpandButton: Boolean; override;
    function GetHasSelectedChildren: Boolean; override;
    function GetIsCellMergingSeparator: Boolean; override;
  public
    property DetailView: TcxCustomGridView read GetDetailView;
    property MasterRow: TcxGridMasterDataRow read GetMasterRow;
  end;

  TdxGridWinExplorerViewRecordHelper = class(TdxCustomGridRecordHelper);

  TdxGridWinExplorerViewDataRecordHelper = class(TdxGridWinExplorerViewRecordHelper)
  protected
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;
  end;

  TdxGridWinExplorerViewGroupRecordHelper = class(TdxGridWinExplorerViewRecordHelper)
  protected
    class function ProducerClass: TdxGridViewRowProducerClass; override;
    class function RecordClass: TcxCustomGridRecordClass; override;
  end;

  { caches }

  TdxRecordHelpersCache = class(TdxCustomCache)
  private
    FAdapter: TdxCustomGridTableViewAdapter;
    function GetHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRecordHelper;
    function GetItem(Index: Integer): TdxCustomGridRecordHelper;
  protected
    function IndexOf(ARecord: TcxCustomGridRecord): Integer;
    property Items[Index: Integer]: TdxCustomGridRecordHelper read GetItem;
  public
    constructor Create(AAdapter: TdxCustomGridTableViewAdapter);
    property Adapter: TdxCustomGridTableViewAdapter read FAdapter;
    property Helpers[ARecord: TcxCustomGridRecord]: TdxCustomGridRecordHelper read GetHelper; default;
  end;

  TdxProducerCache = class(TdxCustomCache)
  private
    FBuilder: TdxCustomGridViewBuilder;
    function GetItem(Index: Integer): TdxGridViewRowProducer;
    function GetProducer(ProducerClass: TdxGridViewRowProducerClass): TdxGridViewRowProducer;
  protected
    function IndexOf(AProducerClass: TdxGridViewRowProducerClass): Integer;
    property Items[Index: Integer]: TdxGridViewRowProducer read GetItem;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder);
    property Builder: TdxCustomGridViewBuilder read FBuilder;
    property Producers[ProducerClass: TdxGridViewRowProducerClass]: TdxGridViewRowProducer read GetProducer; default;
  end;

  { CustomView Adapter, Formatter and Builder }

  TdxGridViewAdapterClass = class of TdxCustomGridViewAdapter;

  TdxCustomGridViewAdapter = class
  private
    FGridView: TcxCustomGridView;
    FMasterAdapter: TdxCustomGridViewAdapter;
    FPrevAllRecordsAreLoaded: Boolean;
    function GetAbsoluteLevel: Integer;
    function GetCanUseLookAndFeelColors: Boolean;
    function GetCaptionText: string;
    function GetDetailsSeparatorColor: TColor;
    function GetDetailsSeparatorThickness: Integer;
    function GetExpandButtonColor: TColor;
    function GetExpandButtonSize: Integer;
    function GetFilter: TcxDataFilterCriteria;
    function GetFilterActive: Boolean;
    function GetFilterEmpty: Boolean;
    function GetFilterText: string;
    function GetGrid: TcxCustomGrid;
    function GetGridLevel: TcxGridLevel;
    function GetGridWidth: Integer;
    function GetHasDetailsSeparator: Boolean;
    function GetLookAndFeelKind: TcxLookAndFeelKind;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetIsDetail: Boolean;
    function GetIsMaster: Boolean;
    function GetMasterGridLevel: TcxGridLevel;
    function GetScaleFactor: TdxScaleFactor;
    function GetSummary: TcxDataSummary;
    function GetThemedCaptionColor: TColor;
    function GetThemedCaptionTextColor: TColor;
    function GetThemedFilterBarColor: TColor;
    function GetThemedFilterBarTextColor: TColor;
    function GetViewWidth: Integer;
  protected
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;

    function BackgroundBitmaps: TcxCustomGridBackgroundBitmaps; overload; virtual;
    function GetBackgroundBitmap(Index: Integer): TGraphic;
    function HasBackgroundBitmap(Index: Integer): Boolean;
    function IsBackgroundBitmapSupported(Index: Integer): Boolean; virtual;

    function Controller: TcxCustomGridController; overload; virtual;
    function DataController: TcxCustomDataController; overload; virtual;

    function CreateCloneAdapter(AClone: TcxCustomGridView): TdxCustomGridViewAdapter;
    procedure ExpandAllRowsInClones(AMasterRows, AGroupRows: Boolean); virtual;
    procedure ExpandAllRowsInGridView(AGridView: TcxCustomGridView; AMasterRows, AGroupRows: Boolean);
    procedure ExpandAllRowsInLevel(ALevel: TcxGridLevel; AMasterRows, AGroupRows: Boolean; ARecursive: Boolean);
    procedure ExpandAllRowsInItself(AMasterRows, AGroupRows: Boolean); virtual;

    function GetAreAllMasterRowsCollapsed: Boolean; virtual;
    function GetAutoWidth: Boolean; virtual;
    function GetCanUseOnEveryPageMode: Boolean; virtual;
    function GetFilterPosition: TcxGridFilterPosition; virtual;
    function GetHasSelectedRecords: Boolean; virtual;
    function GetIndentCount: Integer; virtual;
    function GetIndentWidth: Integer; virtual;
    function GetIsOffice11StyleGrouping: Boolean; virtual;
    function GetMasterGridRecord: TcxCustomGridRecord; virtual;
    function GetRecordCount: Integer; virtual;
    function GetViewWidthExtra: Integer; virtual;
    { Styles }
    function GetCaptionViewParams: TcxViewParams; virtual;
    function GetFilterBarViewParams: TcxViewParams; virtual;
    function GetRootCaptionParams: TcxViewParams; virtual;

    {DB}
    function DBDataModeController: TcxDBDataModeController; virtual;
    function IsBoundView: Boolean; virtual;
    procedure LoadAllRecords; virtual;
    procedure UnloadAllRecords; virtual;

    property Filter: TcxDataFilterCriteria read GetFilter;
    property Grid: TcxCustomGrid read GetGrid;
    property LookAndFeelKind: TcxLookAndFeelKind read GetLookAndFeelKind;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Summary: TcxDataSummary read GetSummary;
  public
    constructor Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView); virtual;

    function GridView: TcxCustomGridView; overload; virtual;
    class function GridViewClass: TcxCustomGridViewClass; virtual;
    function Styles: TcxCustomGridStyles; overload; virtual;

    procedure ExpandAllRows(AnOptionsExpanding: TdxGridReportLinkOptionsExpanding;
      ARecursive: Boolean); virtual;

    property AbsoluteLevel: Integer read GetAbsoluteLevel;
    property AreAllMasterRowsCollapsed: Boolean read GetAreAllMasterRowsCollapsed;
    property AutoWidth: Boolean read GetAutoWidth;
    property CanUseOnEveryPageMode: Boolean read GetCanUseOnEveryPageMode;
    property CanUseLookAndFeelColors: Boolean read GetCanUseLookAndFeelColors;
    property CaptionText: string read GetCaptionText;
    property DetailsSeparatorColor: TColor read GetDetailsSeparatorColor;
    property DetailsSeparatorThickness: Integer read GetDetailsSeparatorThickness;
    property ExpandButtonColor: TColor read GetExpandButtonColor;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
    property GridLevel: TcxGridLevel read GetGridLevel;
    property GridWidth: Integer read GetGridWidth;
    property FilterActive: Boolean read GetFilterActive;
    property FilterEmpty: Boolean read GetFilterEmpty;
    property FilterPosition: TcxGridFilterPosition read GetFilterPosition;
    property FilterText: string read GetFilterText;
    property HasDetailsSeparator: Boolean read GetHasDetailsSeparator;
    property HasSelectedRecords: Boolean read GetHasSelectedRecords;
    property IndentCount: Integer read GetIndentCount;
    property IndentWidth: Integer read GetIndentWidth;
    property IsDetail: Boolean read GetIsDetail;
    property IsMaster: Boolean read GetIsMaster;
    property IsOffice11StyleGrouping: Boolean read GetIsOffice11StyleGrouping;
    property MasterAdapter: TdxCustomGridViewAdapter read FMasterAdapter;
    property MasterGridLevel: TcxGridLevel read GetMasterGridLevel;
    property MasterGridRecord: TcxCustomGridRecord read GetMasterGridRecord;
    property RecordCount: Integer read GetRecordCount;
    property ThemedCaptionColor: TColor read GetThemedCaptionColor;
    property ThemedCaptionTextColor: TColor read GetThemedCaptionTextColor;
    property ThemedFilterBarColor: TColor read GetThemedFilterBarColor;
    property ThemedFilterBarTextColor: TColor read GetThemedFilterBarTextColor;
    property ViewWidth: Integer read GetViewWidth;
    property ViewWidthExtra: Integer read GetViewWidthExtra;
  end;

  TdxGridViewFormatterClass = class of TdxCustomGridViewFormatter;

  TdxCustomGridViewFormatter = class(TObject, IUnknown)
  private
    FBuilder: TdxCustomGridViewBuilder;
    FCaptionLineHeight: Integer;
    FExpandButtons: TList;
    FFilterBarLineHeight: Integer;
    FFont: TFont;
    FLookAndFeelItems: TList;
    FTransparentColor: TColor;
    function GetAbsoluteLevel: Integer;
    function GetAutoWidth: Boolean;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetEdgeMode: TdxCellEdgeMode;
    function GetFilterBarPosition: TcxGridFilterPosition;
    function GetGridWidth: Integer;
    function GetExpandButton(Index: Integer): TdxReportCellExpandButton;
    function GetExpandButtonCount: Integer;
    function GetIndentWidth: Integer;
    function GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
    function GetLookAndFeelItemCount: Integer;
    function GetPaginateByTopLevelGroups: Boolean;
    function GetRenderer: TdxPSReportRenderer;
    function GetReportLink: TdxGridReportLink;
    function GetScaleFactor: TdxScaleFactor;
    function GetShowCaption: Boolean;
    function GetShowFilterBar: Boolean;
    function GetUseLookAndFeelColors: Boolean;
    function GetViewAvailableWidth: Integer;
    function GetViewWidthExtra: Integer;

    procedure FormatLookAndFeelItems;
    procedure FormatExpandButtons;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddDelimiters; virtual;
    procedure AddHorizontalDelimiters; virtual;
    procedure AddVerticalDelimiters; virtual;

    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;

    procedure Calculate; virtual;
    procedure CalculateLineHeights; virtual;

    function GetDetailsSeparatorColor: TColor; virtual;
    function GetDetailsSeparatorThickness: Integer; virtual;
    function GetHasDetailsSeparator: Boolean; virtual;

    function GetSiteHeight: Integer; virtual;
    function GetSiteWidth: Integer; virtual;
    function GetViewWidth: Integer; virtual;
    function GetViewWidthExtraAfter: Integer; virtual;  // MD relation considering
    function GetViewWidthExtraBefore: Integer; virtual; // MD relation considering

    function BackgroundBitmaps: TcxCustomGridBackgroundBitmaps; overload; virtual;
    function GetBackgroundBitmap(Index: Integer): TGraphic; virtual;
    function GetBackgroundBitmapIndex(Index: Integer): Integer;
    function HasBackgroundBitmap(Index: Integer): Boolean; virtual;
    function MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer; virtual;

    function CalculateFontHeight(const AParams: TdxReportItemViewParams): Integer;
    procedure CalculateHeight(const AParams: TdxReportItemViewParams; var AHeight: Integer);
    function GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
    function IsColorTransparent(AColor: TColor): Boolean;
    procedure RegisterExpandButton(AExpandButton: TdxReportCellExpandButton);
    procedure RegisterLookAndFeelItem(AnItem: TdxReportVisualItem; AEdgeStyle: TdxCellEdgeStyle);
    procedure SetViewParams(AnItem: TdxReportVisualItem; AGridLines: TcxGridLines; const AParams: TcxViewParams); overload;
    procedure SetViewParams(AnItem: TdxReportVisualItem; const AParams: TdxReportItemViewParams); overload;

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property EdgeMode: TdxCellEdgeMode read GetEdgeMode;
    property ExpandButtonCount: Integer read GetExpandButtonCount;
    property ExpandButtons[Index: Integer]: TdxReportCellExpandButton read GetExpandButton;
    property GridWidth: Integer read GetGridWidth;
    property LookAndFeelItemCount: Integer read GetLookAndFeelItemCount;
    property LookAndFeelItems[Index: Integer]: TdxReportVisualItem read GetLookAndFeelItem;
    property PaginateByTopLevelGroups: Boolean read GetPaginateByTopLevelGroups;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property ReportLink: TdxGridReportLink read GetReportLink;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SiteHeight: Integer read GetSiteHeight;
    property SiteWidth: Integer read GetSiteWidth;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); virtual;
    destructor Destroy; override;

    function Adapter: TdxCustomGridViewAdapter; overload; virtual;
    function Builder: TdxCustomGridViewBuilder; overload; virtual;

    { Host }
    procedure DoInitializeHost(AHost: TdxReportCell); virtual;
    function GetHostClass: TdxReportCellClass; virtual;
    function GetRowHostClass: TdxReportCellClass; virtual;

    { Caption }
    procedure DoInitializeCaption(ACaption: TdxReportCellText); virtual;
    procedure DoInitializeCaptionRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeCaption(ACaption: TdxReportCellText); virtual;
    function GetCaptionClass: TdxReportCellTextClass; virtual;
    function GetCaptionViewParams: TdxReportItemViewParams; virtual;

    { Filter Box }
    procedure DoInitializeFilterBar(AFilterBar: TdxReportCellText); virtual;
    procedure DoInitializeFilterBarRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeFilterBar(AFilterBar: TdxReportCellText); virtual;
    function GetFilterBarClass: TdxReportCellTextClass; virtual;
    function GetFilterBarViewParams: TdxReportItemViewParams; virtual;

    { Details Separator }
    procedure DoInitializeDetailsSeparatorRow(ARow: TdxReportCell); virtual;
    procedure DoInitializeHorzDetailsSeparator(ASeparator: TdxReportCellBox); virtual;
    procedure DoInitializeVertBottomDetailsSeparator(ASeparator: TdxReportCellBox;
      ASeparatorKind: TdxVerticalDetailsSeparatorKind); virtual;
    procedure DoInitializeVertDetailsSeparator(ASeparator: TdxReportCellBox;
      ASeparatorKind: TdxVerticalDetailsSeparatorKind); virtual;
    procedure DoInitializeVertTopDetailsSeparator(ASeparator: TdxReportCellBox;
      ASeparatorKind: TdxVerticalDetailsSeparatorKind); virtual;
    function GetDetailsSeparatorClass: TdxReportCellBoxClass; virtual;

    { Master Indent }
    procedure DoInitializeExpandButton(AnExpandButton: TdxReportCellExpandButton;
      ARecord: TcxCustomGridRecord; AnIsMasterIndent: Boolean);
    procedure DoInitializeMasterIndent(AnIndent: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer); virtual;
    function GetMasterIndentClass: TdxReportCellExpandButtonClass; virtual;

    { View Terminator }
    procedure DoInitializeViewTerminator(ATerminator: TdxReportCellBox); virtual;

    property AbsoluteLevel: Integer read GetAbsoluteLevel;
    property AutoWidth: Boolean read GetAutoWidth;
    property CaptionLineHeight: Integer read FCaptionLineHeight write FCaptionLineHeight;
    property DetailsSeparatorColor: TColor read GetDetailsSeparatorColor;
    property DetailsSeparatorThickness: Integer read GetDetailsSeparatorThickness;
    property FilterBarLineHeight: Integer read FFilterBarLineHeight write FFilterBarLineHeight;
    property FilterBarPosition: TcxGridFilterPosition read GetFilterBarPosition;
    property HasDetailsSeparator: Boolean read GetHasDetailsSeparator;
    property IndentWidth: Integer read GetIndentWidth;
    property ShowCaption: Boolean read GetShowCaption;
    property ShowFilterBar: Boolean read GetShowFilterBar;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
    property UseLookAndFeelColors: Boolean read GetUseLookAndFeelColors;
    property ViewAvailableWidth: Integer read GetViewAvailableWidth;
    property ViewWidth: Integer read GetViewWidth;
    property ViewWidthExtra: Integer read GetViewWidthExtra;
    property ViewWidthExtraAfter: Integer read GetViewWidthExtraAfter;   // MD relation considering
    property ViewWidthExtraBefore: Integer read GetViewWidthExtraBefore; // MD relation considering
  end;

  TdxGridViewBuilderClass = class of TdxCustomGridViewBuilder;

  TdxCustomGridViewBuilder = class(TdxCustomClassMapItem)
  private
    FAdapter: TdxCustomGridViewAdapter;
    FDetailsBuilder: TdxCustomGridViewBuilder;
    FFormatter: TdxCustomGridViewFormatter;
    FGridView: TcxCustomGridView;
    FMasterBuilder: TdxCustomGridViewBuilder;
    FProducerCache: TdxProducerCache;
    FReportLink: TdxGridReportLink;
    FReportRows: TList;
    function GetAbsoluteBuilder(AIndex: Integer): TdxCustomGridViewBuilder;
    function GetAutoWidth: Boolean;
    function GetGridLevel: TcxGridLevel;
    function GetGrid: TcxCustomGrid;
    function GetHost: TdxReportCell;
    function GetHostInfoServices: TdxGridAttributeHostInfoServices;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetRootBuilder: TdxCustomGridViewBuilder;
  protected
    procedure AddReportRow(ARow: TdxReportCell);

    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;
    procedure DoBuild; virtual;
    procedure DoBuildViewBody; virtual;
    procedure DoBuildViewFooter; virtual;
    procedure DoBuildViewHeader; virtual;

    function GetViewAvailableWidth: Integer; virtual;
    { Caption }
    procedure CreateCaption; virtual;
    function GetCaptionProducer: TdxGridViewCaptionProducer; virtual;
    function GetCaptionProducerClass: TdxGridViewCaptionProducerClass; virtual;
    { Details Separator }
    procedure CreateBottomDetailsSeparator; virtual;
    procedure CreateTopDetailsSeparator; virtual;
    function GetDetailsBottomSeparatorProducer: TdxGridViewDetailsSeparatorProducer; virtual;
    function GetDetailsBottomSeparatorProducerClass: TdxGridViewDetailsSeparatorProducerClass; virtual;
    function GetDetailsTopSeparatorProducer: TdxGridViewDetailsSeparatorProducer; virtual;
    function GetDetailsTopSeparatorProducerClass: TdxGridViewDetailsSeparatorProducerClass; virtual;
    { Filter Box }
    procedure CreateFilterBar; virtual;
    function GetFilterBarProducer: TdxGridViewFilterBarProducer; virtual;
    function GetFilterBarProducerClass: TdxGridViewFilterBarProducerClass; virtual;
    { View Terminator }
    procedure CreateViewTerminator; virtual;
    function GetViewTerminatorProducer: TdxGridViewTerminatorProducer; virtual;
    function GetViewTerminatorProducerClass: TdxGridViewTerminatorProducerClass; virtual;

    function GridView: TcxCustomGridView; overload; virtual;
    class function GridViewClass: TcxCustomGridViewClass; virtual;

    property GridLevel: TcxGridLevel read GetGridLevel;
    property HostInfoServices: TdxGridAttributeHostInfoServices read GetHostInfoServices;
    property ProducerCache: TdxProducerCache read FProducerCache;
    property ReportLink: TdxGridReportLink read FReportLink;
  public
    constructor Create(AReportLink: TdxGridReportLink; AMasterBuilder: TdxCustomGridViewBuilder;
      AGridView: TcxCustomGridView); virtual;
    destructor Destroy; override;

    function Adapter: TdxCustomGridViewAdapter; overload; virtual;
    class function AdapterClass: TdxGridViewAdapterClass; virtual;
    class function CreateAdapter(AMasterAdapter: TdxCustomGridViewAdapter;
      AGridView: TcxCustomGridView): TdxCustomGridViewAdapter; virtual;

    function Formatter: TdxCustomGridViewFormatter; overload; virtual;
    class function FormatterClass: TdxGridViewFormatterClass; virtual;

    procedure Build;
    function IsAborted: Boolean;
    procedure Progress(const APercentDone: Double);

    class function PairClass: TClass; override;
    class procedure Register;
    class procedure Unregister;

    property AbsoluteBuilders[Index: Integer]: TdxCustomGridViewBuilder read GetAbsoluteBuilder;
    property AutoWidth: Boolean read GetAutoWidth;
    property DetailsBuilder: TdxCustomGridViewBuilder read FDetailsBuilder;
    property Grid: TcxCustomGrid read GetGrid;
    property Host: TdxReportCell read GetHost;
    property MasterBuilder: TdxCustomGridViewBuilder read FMasterBuilder;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property RootBuilder: TdxCustomGridViewBuilder read GetRootBuilder;
    property ViewAvailableWidth: Integer read GetViewAvailableWidth;
  end;

  { CustomTableView Adapter and Formatter }

  TdxGridTableAdapterForEachRecordProc = procedure(ARecord: TcxCustomGridRecord;
    AData: Integer; var AContinue: Boolean) of object;

  TdxCustomGridTableViewAdapter = class(TdxCustomGridViewAdapter)
  strict private
    FConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    FDetailsLineCount: Integer;
    FRecordHelpersCache: TdxRecordHelpersCache;

    function GetCellEndEllipsis: Boolean;
    function GetDetailsLineCount: Integer;
    function GetFiltering: TcxCustomGridTableFiltering;
    function GetIsGridMode: Boolean;
    function GetRecord(Index: Integer): TcxCustomGridRecord;
    function GetRecordHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRecordHelper;
    function GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
    function GetSelectedRecordCount: Integer;
  protected
    function CalculateDetailsLineCount: Integer; virtual;

    function GetContentViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxCustomGridTableItem; AIsDataCell: Boolean = False): TcxViewParams; virtual;
    function GetPreviewViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxCustomGridTableItem): TcxViewParams; virtual;
    function GetSelectionViewParams: TcxViewParams; virtual;
    function HasSelectionStyle: Boolean;
    function TryGetAdvancedStyle(const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;

    function Controller: TcxCustomGridTableController; reintroduce; overload;

    procedure DoExpandMasterRow(ARecord: TcxCustomGridRecord; AData: Integer; var AContinue: Boolean);
    procedure ExpandAllGroupRows; virtual;
    procedure ExpandAllMasterRows(AnExpandGroups: Boolean); virtual;
    procedure ExpandAllRowsInItself(AMasterRows, AGroupRows: Boolean); override;

    function GetAreAllMasterRowsCollapsed: Boolean; override;
    function GetCanUseOnEveryPageMode: Boolean; override;
    function GetCellAutoHeight: Boolean; virtual;
    function GetCellMultiline: Boolean; virtual;
    function GetFilterPosition: TcxGridFilterPosition; override;
    function GetGridLineColor: TColor; virtual;
    function GetHasSelectedRecords: Boolean; override;
    function GetMasterGridRecord: TcxCustomGridRecord; override;
    function GetRecordCount: Integer; override;
    procedure IsMasterRowCollapsed(ARecord: TcxCustomGridRecord; AData: Integer; var AContinue: Boolean);

    class function GetProperties(AnItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord): TcxCustomEditProperties; virtual;
    class function GetPropertiesClass(AnItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord): TcxCustomEditPropertiesClass; virtual;
    class function GetRepositoryItem(AnItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord): TcxEditRepositoryItem; virtual;

    property RecordHelpersCache: TdxRecordHelpersCache read FRecordHelpersCache;
  public
    constructor Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    function ForEachRecord(AProc: TdxGridTableAdapterForEachRecordProc; AData: Integer; AProcessSelection: Boolean): Boolean;

    function GridView: TcxCustomGridTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    function Styles: TcxCustomGridTableViewStyles; reintroduce; overload;

    property ConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider read FConditionalFormattingProvider;
    property CellAutoHeight: Boolean read GetCellAutoHeight;
    property CellEndEllipsis: Boolean read GetCellEndEllipsis;
    property CellMultiline: Boolean read GetCellMultiline;
    property DetailsLineCount: Integer read GetDetailsLineCount write FDetailsLineCount;
    property Filtering: TcxCustomGridTableFiltering read GetFiltering;
    property GridLineColor: TColor read GetGridLineColor;
    property IsGridMode: Boolean read GetIsGridMode;
    property RecordHelpers[Row: TcxCustomGridRecord]: TdxCustomGridRecordHelper read GetRecordHelper;
    property Records[Index: Integer]: TcxCustomGridRecord read GetRecord;
    property SelectedRecordCount: Integer read GetSelectedRecordCount;
    property SelectedRecords[Index: Integer]: TcxCustomGridRecord read GetSelectedRecord;
  end;

  TdxCustomGridTableViewFormatter = class(TdxCustomGridViewFormatter,
    IdxPSCellParams,
    IdxPSCellParams2)
  strict private
    FRecordIndexes: TList;
    FSelectedRecordList: TList;

    function GetGridLineColor: TColor;
    function GetRecord(Index: Integer): TcxCustomGridRecord;
    function GetRecordCount: Integer;
    function GetRecordIndex(ARecord: TcxCustomGridRecord): Integer;
    function GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
    function GetSelectedRecordCount: Integer;
  protected
    { IdxPSCellParams }
    function IdxPSCellParams.GetAutoHeight = IdxPSCellParams_GetAutoHeight;
    function IdxPSCellParams.GetCanvas = IdxPSCellParams_GetCanvas;
    function IdxPSCellParams.GetDisplayGraphicsAsText = IdxPSCellParams_GetDisplayGraphicsAsText;
    function IdxPSCellParams.GetDisplayTrackBarsAsText = IdxPSCellParams_GetDisplayTrackBarsAsText;
    function IdxPSCellParams.GetEndEllipsis = IdxPSCellParams_GetEndEllipsis;
    function IdxPSCellParams.GetFlatCheckMarks = IdxPSCellParams_GetFlatCheckMarks;
    function IdxPSCellParams.GetGraphicsText = IdxPSCellParams_GetGraphicsText;
    function IdxPSCellParams.GetMultiline = IdxPSCellParams_GetMultiline;
    function IdxPSCellParams.GetTransparentGraphics = IdxPSCellParams_GetTransparentGraphics;
    function IdxPSCellParams_GetAutoHeight: Boolean; virtual;
    function IdxPSCellParams_GetCanvas: TdxPSReportRenderCustomCanvas; virtual;
    function IdxPSCellParams_GetDisplayGraphicsAsText: Boolean; virtual;
    function IdxPSCellParams_GetDisplayTrackBarsAsText: Boolean; virtual;
    function IdxPSCellParams_GetEndEllipsis: Boolean; virtual;
    function IdxPSCellParams_GetFlatCheckMarks: Boolean; virtual;
    function IdxPSCellParams_GetGraphicsText: string; virtual;
    function IdxPSCellParams_GetMultiline: Boolean; virtual;
    function IdxPSCellParams_GetTransparentGraphics: Boolean; virtual;
    { IdxPSCellParams2 }
    function IdxPSCellParams2.GetPreviewMarginLeft = IdxPSCellParams2_GetPreviewMarginLeft;
    function IdxPSCellParams2.GetPreviewMarginRight = IdxPSCellParams2_GetPreviewMarginRight;
    function IdxPSCellParams2.GetPreviewMaxHeight = IdxPSCellParams2_GetPreviewMaxHeight;
    function IdxPSCellParams2.GetPreviewMaxLineCount = IdxPSCellParams2_GetPreviewMaxLineCount;
    function IdxPSCellParams2.GetRichEditGraphicClass = IdxPSCellParams2_GetRichEditGraphicClass;
    function IdxPSCellParams2.GetRichEditTransparent = IdxPSCellParams2_GetRichEditTransparent;
    function IdxPSCellParams2_GetPreviewMarginLeft: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMarginRight: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMaxHeight: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMaxLineCount: Integer; virtual;
    function IdxPSCellParams2_GetRichEditGraphicClass: TGraphicClass; virtual;
    function IdxPSCellParams2_GetRichEditTransparent: Boolean; virtual;

    procedure AfterBuilding; override;
    procedure BeforeBuilding; override;

    procedure BuildSelectedRecordList; virtual;
    function CanProcessSelectionStyle(ARecord: TcxCustomGridRecord): Boolean;
    function IsSelectedRecord(ARecord: TcxCustomGridRecord): Boolean; virtual;

    function GetContentBackgroundBitmapStyleIndex(ATableItem: TcxCustomGridTableItem): Integer; virtual;
    function MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer; override;

    property SelectedRecordList: TList read FSelectedRecordList;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); override;
    destructor Destroy; override;

    function Adapter: TdxCustomGridTableViewAdapter; reintroduce; overload;
    function Builder: TdxCustomGridTableViewBuilder; reintroduce; overload;

    function IndexOfRecord(ARecord: TcxCustomGridRecord): Integer;

    { Items }
    procedure CheckDisplayValuePost(AProperties: TcxCustomEditProperties;
      ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AnItem: TAbstractdxReportCellData);
    procedure DoInitializeItem(AItem: TAbstractdxReportCellData; ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False); virtual;
    function GetDataItemClass(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False): TdxReportCellDataClass; virtual;
    function GetItemClass(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False): TdxReportCellDataClass; virtual;
    function GetItemProperties(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord): TcxCustomEditProperties; virtual;
    function GetItemValue(AProperties: TcxCustomEditProperties; ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord): TcxEditValue; virtual;
    function GetItemViewParams(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams; virtual;
    function GetSelectionViewParams: TdxReportItemViewParams;

    property GridLineColor: TColor read GetGridLineColor;
    property RecordCount: Integer read GetRecordCount;
    property RecordIndexes[ARecord: TcxCustomGridRecord]: Integer read GetRecordIndex;
    property Records[Index: Integer]: TcxCustomGridRecord read GetRecord;
    property SelectedRecordCount: Integer read GetSelectedRecordCount;
    property SelectedRecords[Index: Integer]: TcxCustomGridRecord read GetSelectedRecord;
  end;

  TdxCustomGridTableViewBuilder = class(TdxCustomGridViewBuilder)
  protected
    procedure DoBuildViewFooter; override;
    procedure DoBuildViewHeader; override;

    procedure DoGetCellHeight(ARecord: TcxCustomGridRecord; ATableItem: TcxCustomGridTableItem;
      var AHeight: Integer);

    function GridView: TcxCustomGridTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxCustomGridTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxCustomGridTableViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  { GridTableView Adapter, Formatter and Builder }

  TdxGridTableViewAdapter = class(TdxCustomGridTableViewAdapter)
  private
    function GetColumn(Index: Integer): TcxGridColumn;
    function GetColumnCount: Integer;
    function GetFooterItemText(Index: Integer): string;
    function GetFooterMultiline: Boolean;
    function GetFooterMultiSummaries: Boolean;
    function GetGridLines: TcxGridLines;
    function GetGroupFooterMultiSummaries: Boolean;
    function GetGroupedColumn(Index: Integer): TcxGridColumn;
    function GetGroupedColumnCount: Integer;
    function GetGroupedLevelCount: Integer;
    function GetGroupFootersMode: TcxGridGroupFootersMode;
    function GetGroupFootersMultiline: Boolean;
    function GetGroupRowSeparatorColor: TColor;
    function GetGroupRowSeparatorThickness: Integer;
    function GetHasFooterItem(Index: Integer): Boolean;
    function GetHasPreview: Boolean;
    function GetHasRowSeparators: Boolean;
    function GetHeaderEndEllipsis: Boolean;
    function GetInternalColumnMinWidth(Column: TcxGridColumn): Integer;
    function GetInternalColumnWidth(Column: TcxGridColumn): Integer;
    function GetPreviewColumn: TcxGridColumn;
    function GetPreviewLeftIndent: Integer;
    function GetPreviewPlace: TcxGridPreviewPlace;
    function GetPreviewRightIndent: Integer;
    function GetRecordHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRowHelper;
    function GetRow(Index: Integer): TcxCustomGridRow;
    function GetRowCount: Integer;
    function GetRowSeparatorColor: TColor;
    function GetRowSeparatorThickness: Integer;
    function GetShowExpandButtonsForEmptyDetails: Boolean;
    function GetShowHorzGridLines: Boolean;
    function GetShowVertGridLines: Boolean;
    function GetThemedFooterItemColor: TColor;
    function GetThemedFooterItemTextColor: TColor;
    function GetThemedHeaderItemColor: TColor;
    function GetThemedHeaderItemTextColor: TColor;
  protected
    function Controller: TcxGridTableController; reintroduce; overload;
    function DataController: TcxGridDataController; reintroduce; overload;

    function GetAutoWidth: Boolean; override;
    function GetCellAutoHeight: Boolean; override;
    function GetCellMultiline: Boolean; override;
    function GetGridLineColor: TColor; override;
    function GetHeaderAutoHeight: Boolean; virtual;
    function GetHeaderMultiline(Column: TcxGridColumn): Boolean; virtual;
    function GetIndentCount: Integer; override;
    function GetIndentWidth: Integer; override;
    function GetIndicatorWidth: Integer; virtual;
    function GetIsOffice11StyleGrouping: Boolean; override;
    function GetViewWidthExtra: Integer; override;

    function GetDataRowHeight: Integer; virtual;
    function GetDataRowHeightAssigned: Boolean; virtual;
    function GetFooterHeight: Integer; virtual;
    function GetFooterHeightAssigned: Boolean; virtual;
    function GetGroupFooterHeight: Integer; virtual;
    function GetGroupFooterHeightAssigned: Boolean; virtual;
    function GetGroupRowHeight: Integer; virtual;
    function GetGroupRowHeightAssigned: Boolean; virtual;
    function GetHeaderHeight: Integer; virtual;
    function GetHeaderHeightAssigned: Boolean; virtual;
    function GetRowLineCount: Integer; virtual;

    { Styles }
    function GetFilterBarViewParams: TcxViewParams; override;
    function GetFooterViewParams(ARecord: TcxCustomGridRecord; AGroupLevel: Integer;
      AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TcxViewParams; virtual;
    function GetGroupRowViewParams(ARecord: TcxCustomGridRecord;
      AGroupLevel: Integer): TcxViewParams; virtual;
    function GetHeaderViewParams(AColumn: TcxGridColumn): TcxViewParams; virtual;
    function GetPreviewViewParams(ARecord: TcxCustomGridRecord;
      AnItem: TcxCustomGridTableItem): TcxViewParams; override;

    property InternalColumnMinWidths[Column: TcxGridColumn]: Integer read GetInternalColumnMinWidth;
    property InternalColumnWidths[Column: TcxGridColumn]: Integer read GetInternalColumnWidth;
  public
    constructor Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView); override;

    function GridView: TcxGridTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    function Styles: TcxGridTableViewStyles; reintroduce; overload;

    function HasDetails(AGridRow: TcxCustomGridRow): Boolean;

    function IsSummaryHasSameLink(AItem1, AItem2: TcxDataSummaryItem): Boolean;
    function CheckSummaryItemIndex(AItems: TcxDataSummaryItems; AIndex: Integer; AIsGroup: Boolean): Integer;
    function GetFooterItemCount(AItems: TcxDataSummaryItems; ACanMultiLine: Boolean; AIsGroup: Boolean): Integer;
    function GetFooterItemInfo(AItems: TcxDataSummaryItems; AIndex: Integer; var ARowIndex: Integer): Integer;
    function GetFooterLineCount(AItems: TcxDataSummaryItems; ACanMultiLine: Boolean): Integer;

    function GetGroupFooterCount(AGridRow: TcxCustomGridRow): Integer;
    function GetGroupFooterItemText(ARowIndex, ALevel, AIndex: Integer): string;
    function HasGroupFooter(AGridRow: TcxCustomGridRow; ALevel: Integer): Boolean;
    function HasGroupFooterItem(ALevel, AIndex: Integer): Boolean;
    function HasGroupFooters(AGridRow: TcxCustomGridRow): Boolean;

    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxGridColumn read GetColumn;
    property DataRowHeight: Integer read GetDataRowHeight;
    property DataRowHeightAssigned: Boolean read GetDataRowHeightAssigned;
    property FooterHeight: Integer read GetFooterHeight;
    property FooterHeightAssigned: Boolean read GetFooterHeightAssigned;
    property FooterItemTexts[Index: Integer]: string read GetFooterItemText;
    property FooterMultiSummaries: Boolean read GetFooterMultiSummaries;
    property FooterMultiline: Boolean read GetFooterMultiline;
    property GridLines: TcxGridLines read GetGridLines;
    property GroupedColumnCount: Integer read GetGroupedColumnCount;
    property GroupedColumns[Index: Integer]: TcxGridColumn read GetGroupedColumn;
    property GroupedLevelCount: Integer read GetGroupedLevelCount;
    property GroupFooterHeight: Integer read GetGroupFooterHeight;
    property GroupFooterHeightAssigned: Boolean read GetGroupFooterHeightAssigned;
    property GroupFooterMultiSummaries: Boolean read GetGroupFooterMultiSummaries;
    property GroupFootersMode: TcxGridGroupFootersMode read GetGroupFootersMode;
    property GroupFootersMultiline: Boolean read GetGroupFootersMultiline;
    property GroupRowHeight: Integer read GetGroupRowHeight;
    property GroupRowHeightAssigned: Boolean read GetGroupRowHeightAssigned;
    property GroupRowSeparatorColor: TColor read GetGroupRowSeparatorColor;
    property GroupRowSeparatorThickness: Integer read GetGroupRowSeparatorThickness;
    property HasFooterItem[Index: Integer]: Boolean read GetHasFooterItem;
    property HasPreview: Boolean read GetHasPreview;
    property HasRowSeparators: Boolean read GetHasRowSeparators;
    property HeaderAutoHeight: Boolean read GetHeaderAutoHeight;
    property HeaderEndEllipsis: Boolean read GetHeaderEndEllipsis;
    property HeaderHeight: Integer read GetHeaderHeight;
    property HeaderHeightAssigned: Boolean read GetHeaderHeightAssigned;
    property HeaderMultilines[Column: TcxGridColumn]: Boolean read GetHeaderMultiline;
    property IndicatorWidth: Integer read GetIndicatorWidth;
    property PreviewColumn: TcxGridColumn read GetPreviewColumn;
    property PreviewLeftIndent: Integer read GetPreviewLeftIndent;
    property PreviewPlace: TcxGridPreviewPlace read GetPreviewPlace;
    property PreviewRightIndent: Integer read GetPreviewRightIndent;
    property RecordHelpers[Row: TcxCustomGridRecord]: TdxCustomGridRowHelper read GetRecordHelper;
    property RowCount: Integer read GetRowCount;
    property RowLineCount: Integer read GetRowLineCount;
    property Rows[Index: Integer]: TcxCustomGridRow read GetRow;
    property RowSeparatorColor: TColor read GetRowSeparatorColor;
    property RowSeparatorThickness: Integer read GetRowSeparatorThickness;
    property ShowExpandButtonsForEmptyDetails: Boolean read GetShowExpandButtonsForEmptyDetails;
    property ShowHorzGridLines: Boolean read GetShowHorzGridLines;
    property ShowVertGridLines: Boolean read GetShowVertGridLines;
    property ThemedFooterItemColor: TColor read GetThemedFooterItemColor;
    property ThemedFooterItemTextColor: TColor read GetThemedFooterItemTextColor;
    property ThemedHeaderItemColor: TColor read GetThemedHeaderItemColor;
    property ThemedHeaderItemTextColor: TColor read GetThemedHeaderItemTextColor;
  end;

  TdxCustomGridTableViewItemPlaceControllerClass = class of TdxCustomGridTableViewItemPlaceController;

  TdxCustomGridTableViewItemPlaceController = class
  private
    FFormatter: TdxGridTableViewFormatter;
    FHeaderLineCount: Integer;
    FWidth: Integer;
    function GetHeaderLineCount: Integer;
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
  protected
    function CalculateWidth: Integer; virtual;
    function CalculateHeaderLineCount: Integer; virtual;
    function GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace; virtual;
    procedure WidthChanged; virtual;
  public
    constructor Create(AFormatter: TdxGridTableViewFormatter); virtual;

    procedure Calculate; virtual;
    procedure Refresh; virtual;

    property Formatter: TdxGridTableViewFormatter read FFormatter;
    property HeaderLineCount: Integer read GetHeaderLineCount;
    property ItemsByColumn[Column: TcxGridColumn]: TdxGridTableViewColumnPlace read GetItemByColumn; default;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TdxGridTableViewColumnPlaceClass = class of TdxGridTableViewColumnPlace;

  TdxGridTableViewColumnPlace = class
  private
    FColumn: TcxGridColumn;
    FController: TdxGridTableViewColumnPlaceController;
    FLeftBound: Integer;
    FWidth: Integer;
    function GetAdapter: TdxGridTableViewAdapter;
    function GetCellBounds(RowIndex: Integer): TRect;
    function GetCellHeight(RowIndex: Integer): Integer;
    function GetFooterCellBounds: TRect;
    function GetFooterLineHeight: Integer;
    function GetFormatter: TdxGridTableViewFormatter;
    function GetGroupFooterCellBounds(IndentCount: Integer): TRect;
    function GetGroupFooterLineHeight: Integer;
    function GetHeaderCellBounds: TRect;
    function GetHeaderLineHeight: Integer;
    function GetIndex: Integer;
    function GetIsFixed: Boolean;
    function GetLeftBound: Integer;
    function GetMinWidth: Integer;
    function GetOriginalWidth: Integer;
    function GetWidth: Integer;
  protected
    function CalculateLeftBound: Integer; virtual;
    function GetLineCount: Integer; virtual;
    function GetRowIndex: Integer; virtual;
    procedure InitAutoWidthItem(AnItem: TcxAutoWidthItem); virtual;

    property Adapter: TdxGridTableViewAdapter read GetAdapter;
    property Formatter: TdxGridTableViewFormatter read GetFormatter;
    property IsFixed: Boolean read GetIsFixed;
    property LineCount: Integer read GetLineCount;
    property MinWidth: Integer read GetMinWidth;
    property OriginalWidth: Integer read GetOriginalWidth;
    property RowIndex: Integer read GetRowIndex;
  public
    constructor Create(AController: TdxGridTableViewColumnPlaceController; AColumn: TcxGridColumn); virtual;
    procedure Calculate(ALeftBound: Integer); virtual;

    property CellBounds[RowIndex: Integer]: TRect read GetCellBounds;
    property CellHeights[RowIndex: Integer]: Integer read GetCellHeight;
    property Column: TcxGridColumn read FColumn;
    property Controller: TdxGridTableViewColumnPlaceController read FController;
    property FooterCellBounds: TRect read GetFooterCellBounds;
    property FooterLineHeight: Integer read GetFooterLineHeight;
    property GroupFooterCellBounds[IndentCount: Integer]: TRect read GetGroupFooterCellBounds;
    property GroupFooterLineHeight: Integer read GetGroupFooterLineHeight;
    property HeaderCellBounds: TRect read GetHeaderCellBounds;
    property HeaderLineHeight: Integer read GetHeaderLineHeight;
    property Index: Integer read GetIndex;
    property LeftBound: Integer read GetLeftBound write FLeftBound;
    property Width: Integer read GetWidth write FWidth;
  end;

  TdxGridTableViewColumnPlaceController = class(TdxCustomGridTableViewItemPlaceController)
  private
    FColumnIndexes: TList;
    FItems: TList;
    function GetAdapter: TdxGridTableViewAdapter;
    function GetColumnIndex(Column: TcxGridColumn): Integer;
    function GetItem(Index: Integer): TdxGridTableViewColumnPlace;
    function GetItemCount: Integer;
    procedure FreeAndNilItems;
  protected
    function CalculateWidth: Integer; override;
    function GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace; override;

    procedure AddItems; virtual;
    procedure ClearItems;
    function CreateItem(AColumn: TcxGridColumn): TdxGridTableViewColumnPlace; virtual;
    function GetItemClass(AColumn: TcxGridColumn): TdxGridTableViewColumnPlaceClass; virtual;

    function CalculateItemLeftBound(AnItem: TdxGridTableViewColumnPlace): Integer; virtual;
    procedure CalculateItemsWidth; virtual;

    function GetItemsAutoWidth: Boolean; virtual;
    function GetItemsAvailableWidth: Integer; virtual;

    property Adapter: TdxGridTableViewAdapter read GetAdapter;
    property ItemsAutoWidth: Boolean read GetItemsAutoWidth;
    property ItemsAvailableWidth: Integer read GetItemsAvailableWidth;
  public
    constructor Create(AFormatter: TdxGridTableViewFormatter); override;
    destructor Destroy; override;

    procedure Calculate; override;
    procedure Refresh; override;

    function IndexOf(AnItem: TdxGridTableViewColumnPlace): Integer; overload;
    function IndexOf(AColumn: TcxGridColumn): Integer; overload;

    property ColumnIndexes[Column: TcxGridColumn]: Integer read GetColumnIndex;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxGridTableViewColumnPlace read GetItem; default;
  end;

  TdxGridTableViewReportDataAdapter = class
  private
    FBuilder: TdxCustomGridViewBuilder;
    function GetReportItem(Row: TcxCustomGridRow; Column: TcxGridColumn): TdxReportVisualItem;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowByGridRow(Row: TcxCustomGridRow): TdxReportCell;
    function GetReportRowCount: Integer;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); virtual;

    property Builder: TdxCustomGridViewBuilder read FBuilder;
    property ReportItems[Row: TcxCustomGridRow; Column: TcxGridColumn]: TdxReportVisualItem read GetReportItem;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow; default;
    property ReportRowsByGridRow[Row: TcxCustomGridRow]: TdxReportCell read GetReportRowByGridRow;
  end;

  TdxGridTableViewFormatter = class(TdxCustomGridTableViewFormatter)
  private
    FColumns: TList;
    FDetailsLineHeight: Integer;
    FFooterLineHeight: Integer;
    FGroupFooterLineHeight: Integer;
    FGroupRowLineHeight: Integer;
    FHeaderLineHeight: Integer;
    FItemPlaceController: TdxCustomGridTableViewItemPlaceController;
    FPreviewLineHeight: Integer;
    FReportDataAdapter: TdxGridTableViewReportDataAdapter;
    FRowHeights: TList;
    function GetColumn(Index: Integer): TcxGridColumn;
    function GetColumnCount: Integer;
    function GetGroupRowSeparatorColor: TColor;
    function GetGroupRowSeparatorThickness: Integer;
    function GetHasPreview: Boolean;
    function GetHeaderLineCount: Integer;
    function GetPreviewAutoHeight: Boolean;
    function GetPreviewColumn: TcxGridColumn;
    function GetPreviewMaxLineCount: Integer;
    function GetRow(Index: Integer): TcxCustomGridRow;
    function GetRowCount: Integer;
    function GetRowHeight(Index: Integer): Integer;
    function GetRowHeightByRow(Row: TcxCustomGridRow): Integer;
    function GetRowSeparatorColor: TColor;
    function GetRowSeparatorThickness: Integer;
    function GetShowGroupFooters: Boolean;
    function GetShowFooters: Boolean;
    function GetShowHeaders: Boolean;
    function GetShowPreview: Boolean;
    procedure SetRowHeight(Index: Integer; Value: Integer);
    procedure SetRowHeightByRow(Row: TcxCustomGridRow; Value: Integer);
  protected
    { IdxPSCellParams2 }
    function IdxPSCellParams2_GetPreviewMarginLeft: Integer; override;
    function IdxPSCellParams2_GetPreviewMarginRight: Integer; override;
    function IdxPSCellParams2_GetPreviewMaxHeight: Integer; override;
    function IdxPSCellParams2_GetPreviewMaxLineCount: Integer; override;

    procedure AddHorizontalDelimiters; override;
    procedure AfterBuilding; override;
    procedure BeforeBuilding; override;
    procedure Calculate; override;
    procedure CalculateDataRowHeights; virtual;
    procedure CalculateLineHeights; override;

    function GetAlignSummaryWithColumns: Boolean; virtual;
    function GetItemPlaceControllerClass: TdxCustomGridTableViewItemPlaceControllerClass; virtual;
    function MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer; override;

    function CanCellMerging: Boolean; virtual;
    function CanColumnCellMerging(AColumn: TcxGridColumn): Boolean; virtual;
    procedure PerformCellMerging; virtual;
    procedure PerformColumnCellMerging(AColumn: TcxGridColumn); virtual;

    // actual columns used inside building (depends on ActualVisibility of columns and selection status)
    procedure BuildColumnList; virtual;
    procedure BuildItemLists; virtual;
    function IsColumnActuallyVisible(AColumn: TcxGridColumn): Boolean; virtual;

    function GetViewWidth: Integer; override;

    property ItemPlaceController: TdxCustomGridTableViewItemPlaceController read FItemPlaceController;
    property ReportDataAdapter: TdxGridTableViewReportDataAdapter read FReportDataAdapter;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); override;
    destructor Destroy; override;

    function Adapter: TdxGridTableViewAdapter; reintroduce; overload;
    function Builder: TdxGridTableViewBuilder; reintroduce; overload;

    { Footers }
    procedure DoInitializeFooterItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer; AItem: TcxDataSummaryItem); virtual;
    procedure DoInitializeFooterRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeFooterItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    function GetFooterItemBounds(AnIndex: Integer): TRect; virtual;
    function GetFooterItemClass(AnIndex: Integer): TdxReportCellTextClass; virtual;
    function GetFooterItemViewParams(AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TdxReportItemViewParams; virtual;

    { Group Footers }
    procedure DoInitializeGroupFooterHost(AnItem: TdxReportVisualItem; ARecord: TcxCustomGridRecord; AGroupLevel: Integer); virtual;
    procedure DoInitializeGroupFooterItem(AnItem: TAbstractdxReportCellData; ARecord: TcxCustomGridRecord;
      AGroupLevel: Integer; const AValue: Variant; AItem: TcxDataSummaryItem); virtual;
    procedure DoInitializeGroupFooterRow(ARow: TdxReportCell;
      ARecord: TcxCustomGridRecord; AGroupLevel: Integer); virtual;
    procedure DoReportLinkInitializeGroupFooterItem(AnItem: TAbstractdxReportCellData;
      AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord; AGroupLevel: Integer); virtual;
    function GetGroupFooterItemClass(AnIndex: Integer): TdxReportCellTextClass; virtual;
    function GetGroupFooterItemBounds(AnIndex, AGroupLevel: Integer): TRect; virtual;
    function GetGroupFooterItemViewParams(ARecord: TcxCustomGridRecord; AGroupLevel: Integer;
      AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TdxReportItemViewParams; virtual;

    { Group Row }
    procedure DoInitializeGroupRowItem(AnItem: TAbstractdxReportCellData; ARow: TcxGridGroupRow;
      AIndex: Integer; const AText: string); virtual;
    procedure DoInitializeGroupRow(ARow: TdxReportCell; ARecord: TcxCustomGridRecord); virtual;
    function GetGroupRowClass: TdxReportCellTextClass; virtual;
    function GetGroupRowViewParams(ARecord: TcxCustomGridRecord; ALevel: Integer): TdxReportItemViewParams; virtual;

    { Group Row Separator - Office11 Grouping Style }
    procedure DoInitializeGroupRowSeparator(ASeparator: TAbstractdxReportCellData); virtual;
    procedure DoInitializeGroupSeparatorRow(ARow: TdxReportCell); virtual;
    function GetGroupRowSeparatorClass: TdxReportCellDataClass; virtual;

    { Headers }
    procedure DoInitializeHeaderItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    procedure DoInitializeHeaderRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeHeaderItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    function GetHeaderItemBounds(AnIndex: Integer): TRect; virtual;
    function GetHeaderItemClass(AnIndex: Integer): TdxReportCellTextClass; virtual;
    function GetHeaderItemViewParams(AColumn: TcxGridColumn): TdxReportItemViewParams; virtual;

    { Indents and Expand Buttons }
    function CreateIndent(AParent: TdxReportCell): TdxReportCellExpandButton; virtual;
    procedure DoInitializeDataRowIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure DoInitializeGroupFooterIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure DoInitializeGroupRowIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure DoInitializeMasterDataRowIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord); virtual;
    procedure DoInitializeRowSeparatorIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord); virtual;
    function GetIndentClass: TdxReportCellExpandButtonClass; virtual;
    function GetIndentViewParams(ARecord: TcxCustomGridRecord;
      AnIndex, AnIndentCount: Integer; ANonRecordIndent: Boolean): TdxReportItemViewParams; virtual;
    function HasExpandButton(ARecord: TcxCustomGridRecord): Boolean; virtual;

    { Items }
    procedure DoInitializeDataRow(ARow: TdxReportCell; ARecord: TcxCustomGridRecord); virtual;
    procedure DoReportLinkInitializeItem(AnItem: TAbstractdxReportCellData;
      AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord); virtual;
    function GetItemBounds(AGridRow: TcxCustomGridRow;
      AGridRowIndex, AColumnIndex: Integer): TRect; virtual;
    function GetItemViewParams(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams; override;

    { Master Indent }
    procedure DoInitializeMasterIndent(AnIndent: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer); override;

    { Preview }
    procedure DoInitializePreview(APreview: TAbstractdxReportCellData; AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord); virtual;
    function GetPreviewClass(AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord): TdxReportCellDataClass; virtual;
    function GetPreviewViewParams(AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord): TdxReportItemViewParams; virtual;

    { Row Separator }
    procedure DoInitializeSeparatorRow(ARow: TdxReportCell); virtual;
    procedure DoInitializeRowSeparator(ASeparator: TAbstractdxReportCellData); virtual;
    function GetRowSeparatorClass: TdxReportCellDataClass; virtual;

    function GetReportItem(ARow: TcxCustomGridRow; AColumn: TcxGridColumn): TdxReportVisualItem;

    property AlignSummaryWithColumns: Boolean read GetAlignSummaryWithColumns;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxGridColumn read GetColumn;
    property DetailsLineHeight: Integer read FDetailsLineHeight write FDetailsLineHeight;
    property FooterLineHeight: Integer read FFooterLineHeight write FFooterLineHeight;
    property GroupFooterLineHeight: Integer read FGroupFooterLineHeight write FGroupFooterLineHeight;
    property GroupRowLineHeight: Integer read FGroupRowLineHeight write FGroupRowLineHeight;
    property GroupRowSeparatorColor: TColor read GetGroupRowSeparatorColor;
    property GroupRowSeparatorThickness: Integer read GetGroupRowSeparatorThickness;
    property HasPreview: Boolean read GetHasPreview;
    property HeaderLineCount: Integer read GetHeaderLineCount;
    property HeaderLineHeight: Integer read FHeaderLineHeight write FHeaderLineHeight;
    property PreviewAutoHeight: Boolean read GetPreviewAutoHeight;
    property PreviewColumn: TcxGridColumn read GetPreviewColumn;
    property PreviewLineHeight: Integer read FPreviewLineHeight write FPreviewLineHeight;
    property PreviewMaxLineCount: Integer read GetPreviewMaxLineCount;
    property RowCount: Integer read GetRowCount;
    property RowHeights[Index: Integer]: Integer read GetRowHeight write SetRowHeight;
    property RowHeightsByRow[Row: TcxCustomGridRow]: Integer read GetRowHeightByRow write SetRowHeightByRow;
    property Rows[Index: Integer]: TcxCustomGridRow read GetRow;
    property RowSeparatorColor: TColor read GetRowSeparatorColor;
    property RowSeparatorThickness: Integer read GetRowSeparatorThickness;
    property ShowGroupFooters: Boolean read GetShowGroupFooters;
    property ShowFooters: Boolean read GetShowFooters;
    property ShowHeaders: Boolean read GetShowHeaders;
    property ShowPreview: Boolean read GetShowPreview;
  end;

  TdxGridTableViewBuilder = class(TdxCustomGridTableViewBuilder)
  private
    function GetLastReportRow: TdxReportCell;
    function GetProcessedView: TcxCustomGridView;
    procedure SetProcessedView(Value: TcxCustomGridView);
  protected
    procedure AddDelimitersHardVert(AReportRow: TdxReportCell);

    procedure DoBuildViewBody; override;
    procedure DoBuildViewFooter; override;

    { Details View }
    procedure CreateDetails(AMasterRow: TcxGridMasterDataRow); virtual;
    procedure CreateDetailView(AGridView: TcxCustomGridView); virtual;
    function GetAreDetailsBuilt: Boolean; virtual;

    { Footer }
    procedure CreateFooter; virtual;
    function GetFootersProducer: TdxGridTableViewFootersProducer; virtual;
    function GetFootersProducerClass: TdxGridTableViewFootersProducerClass; virtual;

    { Group Footer }
    procedure CreateGroupFooters(AGridRow: TcxCustomGridRow); virtual;
    function GetGroupFooterProducer: TdxGridTableViewGroupFooterProducer; virtual;
    function GetGroupFooterProducerClass: TdxGridTableViewGroupFooterProducerClass; virtual;

    { Header }
    procedure CreateHeader; virtual;
    function GetHeadersProducer: TdxGridTableViewHeadersProducer; virtual;
    function GetHeadersProducerClass: TdxGridTableViewHeadersProducerClass; virtual;

    { Rows }
    procedure CreateRow(AGridRow: TcxCustomGridRow; ARowIndex: Integer); virtual;
    procedure CreateRows; virtual;
    function GetRowProducer(AGridRow: TcxCustomGridRow): TdxGridTableViewCustomDataRowProducer; virtual;
    function GetRowProducerClass(AGridRow: TcxCustomGridRow): TdxGridTableViewCustomDataRowProducerClass; virtual;

    { Row Separators }
    procedure CreateRowSeparator(AGridRow: TcxCustomGridRow; AnIsLast: Boolean = False); virtual;
    function GetRowSeparatorProducer: TdxGridTableViewRowSeparatorProducer; virtual;
    function GetRowSeparatorProducerClass: TdxGridTableViewRowSeparatorProducerClass; virtual;
    function GetShowRowSeparators: Boolean; virtual;

    { GroupRow Separators }
    procedure CreateGroupRowSeparator(AGridRow: TcxGridGroupRow); virtual;
    function GetGroupRowSeparatorProducer: TdxGridTableViewGroupRowSeparatorProducer; virtual;
    function GetGroupRowSeparatorProducerClass: TdxGridTableViewGroupRowSeparatorProducerClass; virtual;
    function GetShowGroupRowSeparators: Boolean; virtual;

    function GridView: TcxGridTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;

    property LastReportRow: TdxReportCell read GetLastReportRow;
    property ProcessedView: TcxCustomGridView read GetProcessedView write SetProcessedView;
  public
    function Adapter: TdxGridTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridTableViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;

    property AreDetailsBuilt: Boolean read GetAreDetailsBuilt;
    property ShowGroupRowSeparators: Boolean read GetShowGroupRowSeparators;
    property ShowRowSeparators: Boolean read GetShowRowSeparators;
  end;

  { DBTableView Adapter, Formatter and Builder }

  TdxGridDBTableViewAdapter = class(TdxGridTableViewAdapter)
  protected
    function DataController: TcxGridDBDataController; reintroduce; overload;
    function DBDataModeController: TcxDBDataModeController; override;
  public
    function GridView: TcxGridDBTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TdxGridDBTableViewBuilder = class(TdxGridTableViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxGridDBTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

  { ServerModeTableView Adapter, Formatter and Builder }

  TcxGridServerModeTableViewAdapter = class(TdxGridTableViewAdapter)
  protected
    function DataController: TdxServerModeDataController; reintroduce; overload;
  public
    function GridView: TcxGridServerModeTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TcxGridServerModeTableViewBuilder = class(TdxGridTableViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TcxGridServerModeTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;
  { BandedTableView Adapter, Formatter and Builder }

  TdxGridBandedTableViewAdapter = class(TdxGridTableViewAdapter)
  private
    FFooterLineCount: Integer;
    function GetBand(Index: Integer): TcxGridBand;
    function GetBandCount: Integer;
    function GetBandEndEllipsis: Boolean;
    function GetBandHeaderLineCount: Integer;
    function GetBandHeaderMultiline: Boolean;
    function GetBottomBand(Index: Integer): TcxGridBand;
    function GetBottomBandCount: Integer;
    function GetColumn(Index: Integer): TcxGridBandedColumn;
    function GetFooterLineCount: Integer;
    function GetRootBand(Index: Integer): TcxGridBand;
    function GetRootBandCount: Integer;
    function GetThemedBandHeaderItemColor: TColor;
    function GetThemedBandHeaderItemTextColor: TColor;
  protected
    function CalculateDetailsLineCount: Integer; override;
    function CalculateFooterLineCount: Integer; virtual;

    function GetBandHeaderHeight: Integer; virtual;
    function GetBandHeaderHeightAssigned: Boolean; virtual;
    function GetBandHeaderViewParams(ABand: TcxGridBand): TcxViewParams; virtual;
    function GetDataRowHeightAssigned: Boolean; override;
    function GetFooterHeightAssigned: Boolean; override;
    function GetHeaderHeightAssigned: Boolean; override;
    function GetHeaderMultiline(Column: TcxGridColumn): Boolean; override;
    function GetRowLineCount: Integer; override;
  public
    constructor Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView); override;

    function GridView: TcxGridBandedTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    function Styles: TcxGridBandedTableViewStyles; reintroduce; overload;

    property BandCount: Integer read GetBandCount;
    property BandEndEllipsis: Boolean read GetBandEndEllipsis;
    property BandHeaderHeight: Integer read GetBandHeaderHeight;
    property BandHeaderHeightAssigned: Boolean read GetBandHeaderHeightAssigned;
    property BandHeaderLineCount: Integer read GetBandHeaderLineCount;
    property BandHeaderMultiline: Boolean read GetBandHeaderMultiline;
    property Bands[Index: Integer]: TcxGridBand read GetBand;
    property BottomBandCount: Integer read GetBottomBandCount;
    property BottomBands[Index: Integer]: TcxGridBand read GetBottomBand;
    property Columns[Index: Integer]: TcxGridBandedColumn read GetColumn;
    property FooterLineCount: Integer read GetFooterLineCount write FFooterLineCount;
    property RootBandCount: Integer read GetRootBandCount;
    property RootBands[Index: Integer]: TcxGridBand read GetRootBand;
    property ThemedBandHeaderItemColor: TColor read GetThemedBandHeaderItemColor;
    property ThemedBandHeaderItemTextColor: TColor read GetThemedBandHeaderItemTextColor;
  end;

  TdxGridBandedTableViewColumnPlace = class(TdxGridTableViewColumnPlace)
  private
    function GetColumn: TcxGridBandedColumn;
    function GetController: TdxCustomGridBandedTableViewItemPlace;
    function GetFormatter: TdxGridBandedTableViewFormatter;
  protected
    function GetLineCount: Integer; override;
    function GetRowIndex: Integer; override;
  public
    property Column: TcxGridBandedColumn read GetColumn;
    property Controller: TdxCustomGridBandedTableViewItemPlace read GetController;
    property Formatter: TdxGridBandedTableViewFormatter read GetFormatter;
  end;

  TdxCustomGridBandedTableViewItemPlaceClass = class of TdxCustomGridBandedTableViewItemPlace;

  TdxCustomGridBandedTableViewItemPlace = class(TdxGridTableViewColumnPlaceController)
  private
    FBand: TcxGridBand;
    FController: TdxGridBandedTableViewItemPlaceController;
    FHeight: Integer;
    FLeftBound: Integer;
    FMinWidth: Integer;
    FParent: TdxGridBandedTableViewItemPlace;
    FTopBound: Integer;
    function GetAdapter: TdxGridBandedTableViewAdapter;
    function GetBounds: TRect;
    function GetFormatter: TdxGridBandedTableViewFormatter;
    function GetHeight: Integer;
    function GetIndex: Integer;
    function GetIsFixed: Boolean;
    function GetLeftBound: Integer;
    function GetLevelIndex: Integer;
    function GetMinWidth: Integer;
    function GetTopBound: Integer;
    function GetViewParams: TdxReportItemViewParams;
  protected
    function CalculateHeaderLineCount: Integer; override;
    function CalculateWidth: Integer; override;
    function GetItemClass(AColumn: TcxGridColumn): TdxGridTableViewColumnPlaceClass; override;

    procedure AssignWidth;
    function CalculateHeight: Integer; virtual;
    function CalculateLeftBound: Integer; virtual;
    function CalculateLevelHeight: Integer; virtual;
    function CalculateLineHeight: Integer; virtual;
    function CalculateMinWidth: Integer; virtual;
    function CalculateTopBound: Integer; virtual;
    procedure InitAutoWidthItem(AnItem: TcxAutoWidthItem); virtual;

    function GetRowCount: Integer; virtual;
    function InternalCalculateMinWidth: Integer; virtual;
    function InternalCalculateWidth: Integer; virtual;

    property Adapter: TdxGridBandedTableViewAdapter read GetAdapter;
    property Band: TcxGridBand read FBand;
    property IsFixed: Boolean read GetIsFixed;
    property LevelIndex: Integer read GetLevelIndex;
    property MinWidth: Integer read GetMinWidth;
    property RowCount: Integer read GetRowCount;
    property ViewParams: TdxReportItemViewParams read GetViewParams;
  public
    constructor Create(AController: TdxGridBandedTableViewItemPlaceController;
      AParent: TdxGridBandedTableViewItemPlace; ABand: TcxGridBand); reintroduce; virtual;

    property Bounds: TRect read GetBounds;
    property Controller: TdxGridBandedTableViewItemPlaceController read FController;
    property Formatter: TdxGridBandedTableViewFormatter read GetFormatter;
    property Height: Integer read GetHeight write FHeight;
    property Index: Integer read GetIndex;
    property LeftBound: Integer read GetLeftBound write FLeftBound;
    property Parent: TdxGridBandedTableViewItemPlace read FParent;
    property TopBound: Integer read GetTopBound write FTopBound;
  end;

  TdxGridBandedTableViewItemPlace = class(TdxCustomGridBandedTableViewItemPlace)
  private
    FChildItems: TList;
    function GetChildItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
    function GetChildItemCount: Integer;
  protected
    function GetRowCount: Integer; override;
    function InternalCalculateMinWidth: Integer; override;
    function InternalCalculateWidth: Integer; override;
    procedure WidthChanged; override;

    procedure CalculateChildItemWidths;
    procedure RefreshChildItems;
  public
    constructor Create(AnOwner: TdxGridBandedTableViewItemPlaceController;
      AParent: TdxGridBandedTableViewItemPlace; ABand: TcxGridBand); override;
    destructor Destroy; override;

    function IndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
    procedure Refresh; override;

    property ChildItemCount: Integer read GetChildItemCount;
    property ChildItems[Index: Integer]: TdxCustomGridBandedTableViewItemPlace read GetChildItem;
  end;

  TdxGridBandedTableViewBottomItemPlace = class(TdxCustomGridBandedTableViewItemPlace)
  protected
    procedure AddItems; override;
    function CalculateItemLeftBound(AnItem: TdxGridTableViewColumnPlace): Integer; override;
    procedure CalculateItemsWidth; override;
    function GetItemsAutoWidth: Boolean; override;
    function GetItemsAvailableWidth: Integer; override;

    function GetRowCount: Integer; override;
    function InternalCalculateMinWidth: Integer; override;
    function InternalCalculateWidth: Integer; override;
    procedure WidthChanged; override;
  end;

  TdxGridBandedTableViewItemPlaceController = class(TdxCustomGridTableViewItemPlaceController)
  private
    FBottomItems: TList;
    FHeight: Integer;
    FItems: TList;
    FLevelCount: Integer;
    FLevelHeights: TList;
    FRootItems: TList;
    function GetAdapter: TdxGridBandedTableViewAdapter;
    function GetAutoWidth: Boolean;
    function GetAvailableWidth: Integer;
    function GetBottomItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
    function GetBottomItemCount: Integer;
    function GetFormatter: TdxGridBandedTableViewFormatter;
    function GetHeight: Integer;
    function GetItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
    function GetItemByBand(Band: TcxGridBand): TdxCustomGridBandedTableViewItemPlace;
    function GetItemCount: Integer;
    function GetLevelCount: Integer;
    function GetLevelHeight(Index: Integer): Integer;
    function GetRootItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
    function GetRootItemCount: Integer;
    procedure SetLevelHeight(Index: Integer; Value: Integer);
    procedure FreeAndNilItems;
  protected
    function CalculateHeaderLineCount: Integer; override;
    function CalculateWidth: Integer; override;
    function GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace; override;

    procedure AddItems; virtual;
    procedure ClearItems;
    function CreateItem(ABand: TcxGridBand): TdxCustomGridBandedTableViewItemPlace; virtual;
    function GetItemClass(ABand: TcxGridBand): TdxCustomGridBandedTableViewItemPlaceClass; virtual;
    procedure RefreshBottomItems;
    procedure RefreshItems;
    procedure RefreshRootItems;

    function CalculateHeight: Integer; virtual;
    function CalculateItemHeight(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer; virtual;
    function CalculateItemLeftBound(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer; virtual;
    function CalculateItemTopBound(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer; virtual;
    function CalculateLevelCount: Integer; virtual;

    procedure CalculateItemWidths;
    procedure CalculateLevelHeights;

    property AutoWidth: Boolean read GetAutoWidth;
    property Adapter: TdxGridBandedTableViewAdapter read GetAdapter;
    property AvailableWidth: Integer read GetAvailableWidth;
  public
    constructor Create(AFormatter: TdxGridTableViewFormatter); override;
    destructor Destroy; override;

    procedure Calculate; override;
    procedure Refresh; override;

    function IndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer; overload;
    function IndexOf(ABand: TcxGridBand): Integer; overload;
    function RootIndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer; overload;
    function RootIndexOf(ABand: TcxGridBand): Integer; overload;

    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItems[Index: Integer]: TdxCustomGridBandedTableViewItemPlace read GetBottomItem;
    property Formatter: TdxGridBandedTableViewFormatter read GetFormatter;
    property Height: Integer read GetHeight;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxCustomGridBandedTableViewItemPlace read GetItem; default;
    property ItemsByBand[Band: TcxGridBand]: TdxCustomGridBandedTableViewItemPlace read GetItemByBand;
    property LevelCount: Integer read GetLevelCount;
    property LevelHeights[Index: Integer]: Integer read GetLevelHeight write SetLevelHeight;
    property RootItemCount: Integer read GetRootItemCount;
    property RootItems[Index: Integer]: TdxCustomGridBandedTableViewItemPlace read GetRootItem;
  end;

  TdxGridBandedTableViewFormatter = class(TdxGridTableViewFormatter)
  private
    FBands: TList;
    FHeadersSingleLine: Boolean;
    function GetBand(Index: Integer): TcxGridBand;
    function GetBandCount: Integer;
    function GetColumn(Index: Integer): TcxGridBandedColumn;
    function GetItemPlaceController: TdxGridBandedTableViewItemPlaceController;
    function GetShowBandHeaders: Boolean;
  protected
    procedure AddHorizontalDelimiters; override;

    function CanColumnCellMerging(AColumn: TcxGridColumn): Boolean; override;
    function IsColumnActuallyVisible(AColumn: TcxGridColumn): Boolean; override;
    function GetAlignSummaryWithColumns: Boolean; override;
    function GetItemPlaceControllerClass: TdxCustomGridTableViewItemPlaceControllerClass; override;

    procedure BuildBandList; virtual;
    procedure BuildItemLists; override;
    function IsBandActuallyVisible(ABand: TcxGridBand): Boolean; virtual;

    function MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer; override;

    property HeadersSingleLine: Boolean read FHeadersSingleLine;
    property ItemPlaceController: TdxGridBandedTableViewItemPlaceController read GetItemPlaceController;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); override;
    destructor Destroy; override;

    function Adapter: TdxGridBandedTableViewAdapter; reintroduce; overload;
    function Builder: TdxGridBandedTableViewBuilder; reintroduce; overload;

    { Bands }
    procedure DoInitializeBandItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    procedure DoInitializeBandRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeBandItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    function GetBandItemBounds(AnIndex: Integer): TRect; virtual;
    function GetBandItemClass(AnIndex: Integer): TdxReportCellTextClass; virtual;
    function GetBandItemViewParams(ABand: TcxGridBand): TdxReportItemViewParams; virtual;

    property BandCount: Integer read GetBandCount;
    property Bands[Index: Integer]: TcxGridBand read GetBand;
    property Columns[Index: Integer]: TcxGridBandedColumn read GetColumn;
    property ShowBandHeaders: Boolean read GetShowBandHeaders;
  end;

  TdxGridBandedTableViewBuilder = class(TdxGridTableViewBuilder)
  protected
    procedure DoBuildViewBody; override;

    procedure CreateBands; virtual;
    function GetBandsProducer: TdxGridTableViewBandsProducer; virtual;
    function GetBandsProducerClass: TdxGridTableViewBandsProducerClass; virtual;

    function GridView: TcxGridBandedTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxGridBandedTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridBandedTableViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  { DBBandedTableView Adapter, Formatter and Builder }

  TdxGridDBBandedTableViewAdapter = class(TdxGridBandedTableViewAdapter)
  protected
    function DataController: TcxGridDBDataController; reintroduce; overload;

    function DBDataModeController: TcxDBDataModeController; override;
  public
    function GridView: TcxGridDBBandedTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TdxGridDBBandedTableViewBuilder = class(TdxGridBandedTableViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxGridDBBandedTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

  { ServerModeBandedTableView Adapter, Formatter and Builder }

  TdxGridServerModeBandedTableViewAdapter = class(TdxGridBandedTableViewAdapter)
  protected
    function DataController: TdxServerModeDataController; reintroduce; overload;
  public
    function GridView: TcxGridServerModeBandedTableView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TdxGridServerModeBandedTableViewBuilder = class(TdxGridBandedTableViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxGridServerModeBandedTableViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

  { WinExplorerView Adapter, Formatter, Builder and Records}

  TdxReportWinExplorerViewRecord = class(TdxReportCell)
  private
    function GetGridRecord: TcxGridWinExplorerViewCustomRecord;
  protected
    procedure CalculateSize(AViewInfo: TcxCustomGridRecordViewInfo); virtual;

    property GridRecord: TcxGridWinExplorerViewCustomRecord read GetGridRecord;
  end;

  TdxReportWinExplorerViewRecordClass = class of TdxReportWinExplorerViewRecord;

  TdxReportWinExplorerViewDataRecord = class(TdxReportWinExplorerViewRecord)
  private
    function GetGridRecord: TcxGridWinExplorerViewDataRecord;
  protected
    property GridRecord: TcxGridWinExplorerViewDataRecord read GetGridRecord;
  end;

  TdxReportWinExplorerViewGroupRecord = class(TdxReportWinExplorerViewRecord)
  private
    function GetGridRecord: TcxGridWinExplorerViewGroupRecord;
  protected
    property GridRecord: TcxGridWinExplorerViewGroupRecord read GetGridRecord;
  end;

  TdxGridWinExplorerViewFormatter = class(TdxCustomGridTableViewFormatter)
  private
    function GetRecord(Index: Integer): TcxGridWinExplorerViewCustomRecord;
  protected
    function CreateDataCell(AParent: TdxReportWinExplorerViewRecord;
      AItem: TcxGridWinExplorerViewItem): TAbstractdxReportCellData; virtual;
    procedure DoInitializeRecord(ARecord: TdxReportWinExplorerViewRecord); virtual;
    function GetCaptionItemViewParams(ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      AnIsPreview: Boolean = False): TdxReportItemViewParams; virtual;
    function GetFirstRecordOffset: Integer; virtual;
    function GetInterRecordsSpaceHorz: Integer; virtual;
    function GetInterRecordsSpaceVert: Integer; virtual;
    function GetRecordsAreaWidth: Integer; virtual;
    function GetRecordViewParams(ARecord: TdxReportWinExplorerViewRecord): TdxReportItemViewParams; virtual;
    function GetViewWidth: Integer; override;
  public
    function Adapter: TdxGridWinExplorerViewAdapter; reintroduce; overload;
    function Builder: TdxGridWinExplorerViewBuilder; reintroduce; overload;
    function GetItemViewParams(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False;
      AIsDataCell: Boolean = False): TdxReportItemViewParams; override;
    procedure DoInitializeItem(AnItem: TAbstractdxReportCellData;
      ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      AnIsPreview: Boolean = False); override;

    property Records[Index: Integer]: TcxGridWinExplorerViewCustomRecord read GetRecord;
  end;

  TdxGridWinExplorerViewAdapter = class(TdxCustomGridTableViewAdapter)
  protected
    function GetCaptionItemViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxGridWinExplorerViewItem): TcxViewParams; virtual;
    function GetDescriptionItemViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxGridWinExplorerViewItem): TcxViewParams; virtual;
    function GetGroupViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxCustomGridTableItem): TcxViewParams; virtual;
    function IsGridItemCaption(AGridItem: TcxGridWinExplorerViewItem): Boolean; virtual;
    function IsGridItemDescription(AGridItem: TcxGridWinExplorerViewItem): Boolean; virtual;
    function GetContentViewParams(ARecord: TcxCustomGridRecord; ATableItem: TcxCustomGridTableItem;
      AIsDataCell: Boolean = False): TcxViewParams; override;
  public
    function GridView: TcxGridWinExplorerView; reintroduce; overload;
    function Styles: TcxGridWinExplorerViewStyles; reintroduce; overload;
  end;

  TdxGridWinExplorerViewBuilder = class(TdxCustomGridTableViewBuilder)
  private
    FMaxRecordWidth: Integer;
    FRecords: TList;

    function GetRecord(Index: Integer): TdxReportWinExplorerViewRecord;
  protected
    procedure CalculateMaxRecordWidth; virtual;
    procedure CreateRecord(AGridRecord: TcxGridWinExplorerViewCustomRecord); virtual;
    procedure CreateRecords; virtual;
    procedure DoBuildViewBody; override;
    procedure DoResizeRecords; virtual;
    function GetRecordProducer(AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxGridWinExplorerViewRecordProducer; virtual;
    function GetRecordProducerClass(AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxGridViewRowProducerClass; virtual;
    function GridView: TcxGridWinExplorerView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    procedure InjectRecord(ARecord: TdxReportWinExplorerViewRecord; AParent: TdxReportCell; AIndex: Integer); virtual;
    procedure PlaceRecords; virtual;
    procedure ResizeGroups; virtual;
    procedure ResizeRecords; virtual;

    property Records[Index: Integer]: TdxReportWinExplorerViewRecord read GetRecord;
    property MaxRecordWidth: Integer read FMaxRecordWidth;
  public
    constructor Create(AReportLink: TdxGridReportLink;
      AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    function Adapter: TdxGridWinExplorerViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridWinExplorerViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  { DB WinExplorerView Adapter and Builder}

  TdxGridDBWinExplorerViewAdapter = class(TdxGridWinExplorerViewAdapter)
  protected
    function DataController: TcxGridDBDataController; reintroduce; overload;
    function DBDataModeController: TcxDBDataModeController; override;
  end;

  TdxGridDBWinExplorerViewBuilder = class(TdxGridWinExplorerViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

  { CardView Adapter, Formatter and Builder }

  TdxReportCard = class;
  TdxReportCardHorz = class;
  TdxReportCardVert = class;
  TdxReportCardRow = class;

  TdxReportCardRowClass = class of TdxReportCardRow;
  TdxReportCardLayerClass = class of TdxReportCardLayer;

  TdxReportCardLayer = class(TdxReportCell)
  private
    FCategorySeparator: TdxReportCellBox;
    FLayerSeparator: TdxReportCellBox;
    FNeedCategorySeparator: Boolean;
    FNeedLayerSeparator: Boolean;
    FRows: TList;
    function GetBeginsLayerRow: TcxGridCardViewRow;
    function GetCard: TdxReportCardHorz;
    function GetHeight: Integer;
    function GetInternalHeight: Integer;
    function GetInternalWidth: Integer;
    function GetRow(AIndex: Integer): TdxReportCardRow;
    function GetRowCount: Integer;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetInternalHeight(AValue: Integer);
    procedure SetInternalWidth(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    procedure AdjustRowsHeight; virtual;
    procedure AdjustRowHeight(ACanvas: TdxPSReportRenderCustomCanvas;
      AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder); virtual;
    function CanHaveIndent(ARow: TdxReportCardRow): Boolean; virtual;
    procedure CheckNeedCategorySeparator(ANextLayer: TdxReportCardLayer); virtual;
    procedure CreateItems(ABuilder: TdxGridCardViewBuilder);
    procedure CreateCategorySeparator;
    procedure CreateLayerSeparator; virtual;
    procedure InitAutoWidthItem(AnItem: TcxAutoWidthItem; var AAllFixed: Boolean);
    procedure PlaceSeparatorAndExcludeHeightFromHeight(var AValue: Integer); virtual;
    procedure RecalculateHeight; virtual;
    procedure ResizeRowsHorz; virtual;
    function SetPosition(ALeft, AWidth: Integer): Integer;
    procedure SetRowCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas;
      ACaptionAutoWidths: Boolean; AValue: Integer); virtual;
    procedure SetRowHeight(ARowIndex, AValue: Integer); virtual;
    procedure SizeChanged; virtual;

    property InternalHeight: Integer read GetInternalHeight write SetInternalHeight;
    property InternalWidth: Integer read GetInternalWidth write SetInternalWidth;

    property NeedCategorySeparator: Boolean read FNeedCategorySeparator write FNeedCategorySeparator;
    property NeedLayerSeparator: Boolean read FNeedLayerSeparator write FNeedLayerSeparator;
  public
    constructor CreateEx(AParent: TdxReportCell; ABeginsLayerRow: TcxGridCardViewRow); virtual;
    destructor Destroy; override;
    procedure AdjustCellSides; virtual;
    function AddRow(AGridCardRow: TcxGridCardViewRow): TdxReportCardRow;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    function MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
    function MeasureLayerHeight(ACanvas: TdxPSReportRenderCustomCanvas; ACalculateLineHeight: Boolean;
      ABuilder: TdxGridCardViewBuilder): Integer; virtual;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property BeginsLayerRow: TcxGridCardViewRow read GetBeginsLayerRow;
    property Card: TdxReportCardHorz read GetCard;
    property CategorySeparator: TdxReportCellBox read FCategorySeparator;
    property Height: Integer read GetHeight write SetHeight;
    property LayerSeparator: TdxReportCellBox read FLayerSeparator;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TdxReportCardRow read GetRow; default;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TdxReportCardVerticalLayer = class(TdxReportCardLayer)
  protected
    procedure AdjustRowsHeight; override;
    function CanHaveIndent(ARow: TdxReportCardRow): Boolean; override;
    procedure CreateLayerSeparator; override;
    procedure CheckNeedCategorySeparator(ANextLayer: TdxReportCardLayer); override;
    procedure PlaceSeparatorAndExcludeHeightFromHeight(var AValue: Integer); override;
    procedure RecalculateHeight; override;
    procedure SetRowCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas;
      ACaptionAutoWidths: Boolean; AValue: Integer); override;
    procedure SizeChanged; override;
  public
    procedure AdjustCellSides; override;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
  end;

  TdxReportCardRow = class(TdxReportCell)
  private
    FNeedSeparator: Boolean;
    FRowCaption: TdxReportCellText;
    FRowData: TAbstractdxReportCellData;
    FRowIndent: TdxReportCellExpandButton;
    FRowSeparator: TdxReportCellBox;
    function GetCard: TdxReportCard;
    function GetGridCardRow: TcxGridCardViewRow;
    function GetHasCaption: Boolean;
    function GetHasData: Boolean;
    function GetHasIndent: Boolean;
    function GetHasRowSeparator: Boolean;
    function GetHeight: Integer;
    function GetLayer: TdxReportCardLayer;
    function GetProperties: TcxCustomEditProperties;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetRowCaptionWidth(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure AdjustCellSides(AIsFirstItem, AIsLastItem, AIsLeftItem, AIsRightItem: Boolean); virtual;
    procedure CreateItems(ABuilder: TdxGridCardViewBuilder); virtual;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
    function MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;

    property Properties: TcxCustomEditProperties read GetProperties;
    property NeedSeparator: Boolean read FNeedSeparator write FNeedSeparator;
  public
    constructor CreateEx(AParent: TdxReportCell; AGridCardRow: TcxGridCardViewRow); virtual;

    function MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas; ACalculateLineHeight: Boolean;
      ABuilder: TdxGridCardViewBuilder): Integer; reintroduce; overload;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property Card: TdxReportCard read GetCard;
    property GridCardRow: TcxGridCardViewRow read GetGridCardRow;
    property HasCaption: Boolean read GetHasCaption;
    property HasData: Boolean read GetHasData;
    property HasIndent: Boolean read GetHasIndent;
    property HasRowSeparator: Boolean read GetHasRowSeparator;
    property Height: Integer read GetHeight write SetHeight;
    property Layer: TdxReportCardLayer read GetLayer;
    property RowCaption: TdxReportCellText read FRowCaption;
    property RowCaptionWidth: Integer write SetRowCaptionWidth;
    property RowData: TAbstractdxReportCellData read FRowData;
    property RowIndent: TdxReportCellExpandButton read FRowIndent;
    property RowSeparator: TdxReportCellBox read FRowSeparator;

    property Width: Integer read GetWidth write SetWidth;

  end;

  TdxCardBorderPainter = class(TdxPSCellBorderPainter)
  protected
    procedure DrawShadow(ACanvas: TdxPSReportRenderCustomCanvas); virtual;
  public
    function Card: TdxReportCard; reintroduce; overload;
    procedure DrawBorders(ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect); override;
  end;

  TdxGridCardShadowDepth = 0..15;
  TdxGridCardShadowPosition = (cspTopLeft, cspTopRight, cspBottomRight, cspBottomLeft);

  TdxReportCustomLayoutRecord = class(TdxReportCell)
  protected
    procedure AdjustRecordWidth; virtual;
  public
    constructor CreateEx(AParent: TdxReportCell; AGridRecord: TcxGridCustomLayoutRecord); virtual;
  end;
  TdxReportCustomLayoutRecordClass = class of TdxReportCustomLayoutRecord;

  TdxReportCard = class(TdxReportCustomLayoutRecord)
  private
    FCaptionWidth: Integer;
    FLayerCaptionWidth: Integer;
    FShadowColor: TColor;
    FShadowDepth: Integer;
    FShadowPosition: TdxGridCardShadowPosition;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetHasShadow: Boolean;
    function GetHeight: Integer;
    function GetLayerCount: Integer;
    function GetLayer(Index: Integer): TdxReportCardLayer;
    function GetLayerHeight(Index: Integer): Integer;
    function GetLayersOriginLeft: Integer;
    function GetLayersOriginTop: Integer;
    function GetReportLink: TdxGridReportLink;
    function GetShadowRectHorz: TRect;
    function GetShadowRectVert: TRect;
    function GetGridCard: TcxGridCard;
    function GetWidth: Integer;
    procedure SetContentHeight(AValue: Integer);
    procedure SetContentWidth(AValue: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetShadowDepth2(AValue: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure AddVerticalDelimiters; virtual;
    procedure AdjustRecordWidth; override;
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    function GetBorderPainterClass: TdxPSCellBorderPainterClass; override;
    procedure ReadProperties(AReader: TdxPSDataReader); override;
    procedure WriteProperties(AWriter: TdxPSDataWriter); override;

    procedure AdjustHeight; virtual; abstract;
    procedure AdjustWidth(ACanvas: TdxPSReportRenderCustomCanvas); virtual; abstract;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual; abstract;
    procedure SetCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas; ALayer, AValue, AMaxValue: Integer; ACaptionAutoWidths: Boolean); virtual;
    // layers
    procedure AdjustLayers; virtual;
    procedure AdjustLayersCellSides; virtual; abstract;
    procedure AdjustLayersPos; virtual; abstract;
    procedure CreateLayers(ABuilder: TdxGridCardViewBuilder); virtual;
    function GetLayerClass: TdxReportCardLayerClass; virtual;
    function GetLayerIndexByRow(ARow: TcxGridCardViewRow): Integer; virtual;
    function GetLayerWidth(Index: Integer): Integer; virtual;
    function HasLayerSeparator(ALayerIndex: Integer): Boolean; virtual;
    procedure RecalculateLayersHeight;
    procedure SetLayerHeight(Index, Value: Integer); virtual;
    procedure SetLayerWidth(Index, Value: Integer); virtual;
    // virtual card rows
    procedure AdjustRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder); virtual; abstract;
    function GetRowCount: Integer; virtual;
    function GetRowHeight(AIndex: Integer): Integer; virtual;
    procedure SetRowHeight(AIndex, AValue: Integer); virtual;
    procedure SizeChanged; virtual;

    property ReportLink: TdxGridReportLink read GetReportLink;
  public
    constructor Create(AParent: TdxReportCell); override;
    function GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect; override;

    property CaptionWidth: Integer read FCaptionWidth write FCaptionWidth;
    property ContentHeight: Integer read GetContentHeight write SetContentHeight;
    property ContentWidth: Integer read GetContentWidth write SetContentWidth;
    property GridCard: TcxGridCard read GetGridCard;
    property HasShadow: Boolean read GetHasShadow;
    property Height: Integer read GetHeight write SetHeight;
    property LayerCaptionWidth: Integer read FLayerCaptionWidth write FLayerCaptionWidth;
    property LayerCount: Integer read GetLayerCount;
    property LayerHeights[Index: Integer]: Integer read GetLayerHeight write SetLayerHeight;
    property Layers[Index: Integer]: TdxReportCardLayer read GetLayer;
    property LayersOriginLeft: Integer read GetLayersOriginLeft;
    property LayersOriginTop: Integer read GetLayersOriginTop;
    property LayerWidths[Index: Integer]: Integer read GetLayerWidth write SetLayerWidth;
    property RowCount: Integer read GetRowCount;
    property RowHeight[AIndex: Integer]: Integer read GetRowHeight write SetRowHeight;
    property ShadowColor: TColor read FShadowColor write FShadowColor default clBlack;
    property ShadowDepth: Integer read FShadowDepth write SetShadowDepth2 default dxDefaultCardsShadowDepth;
    property ShadowPosition: TdxGridCardShadowPosition read FShadowPosition write FShadowPosition default cspBottomRight;
    property ShadowRectHorz: TRect read GetShadowRectHorz;
    property ShadowRectVert: TRect read GetShadowRectVert;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TdxReportCardHorz = class(TdxReportCard)
  protected
    procedure AdjustHeight; override;
    procedure AdjustLayersCellSides; override;
    procedure AdjustLayersPos; override;
    procedure AdjustRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder); override;
    procedure AdjustWidth(ACanvas: TdxPSReportRenderCustomCanvas); override;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; virtual;
    procedure SizeChanged; override;
  end;

  TdxReportCardVert = class(TdxReportCard)
  protected
    procedure AdjustHeight; override;
    procedure AdjustLayers; override;
    procedure AdjustLayersCellSides; override;
    procedure AdjustLayersPos; override;
    procedure AdjustRowHeight(ACanvas: TdxPSReportRenderCustomCanvas;
      AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder); override;
    procedure AdjustWidth(ACanvas: TdxPSReportRenderCustomCanvas); override;
    function GetLayerClass: TdxReportCardLayerClass; override;
    function GetLayerByAbsoluteRowIndex(var AIndex: Integer): TdxReportCardLayer;
    function GetLayerIndexByRow(ARow: TcxGridCardViewRow): Integer; override;
    function GetRowCount: Integer; override;
    function GetRowHeight(AIndex: Integer): Integer; override;
    function HasLayerSeparator(ALayerIndex: Integer): Boolean; override;
    function MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    procedure SetCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas; ALayer, AValue, AMaxValue: Integer; ACaptionAutoWidth: Boolean); override;
    procedure SetLayerWidth(Index, Value: Integer); override;
    procedure SetLayerHeight(AIndex, AValue: Integer); override;
    procedure SetLayersSameHeight;
    procedure SetRowHeight(AIndex, AValue: Integer); override;
    procedure SizeChanged; override;
  end;

  TdxReportCardClass = class of TdxReportCard;

  TdxGridCustomLayoutViewAdapter = class(TdxCustomGridTableViewAdapter)
  public
    procedure ExpandAllRows(AnOptionsExpanding: TdxGridReportLinkOptionsExpanding; ARecursive: Boolean); override;
  end;

  TdxGridCardViewAdapter = class(TdxGridCustomLayoutViewAdapter)
  private
    function GetCaptionSeparator: string;
    function GetCard(Index: Integer): TcxGridCard;
    function GetCardAutoWidth: Boolean;
    function GetCardCaptionWidth: Integer;
    function GetCardCount: Integer;
    function GetCardRow(Index: Integer): TcxGridCardViewRow;
    function GetCardRowCount: Integer;
    function GetCardSeparatorColor: Integer;
    function GetCardSeparatorThickness: Integer;
    function GetCardWidth: Integer;
    function GetHasCardsSeparator: Boolean;
    function GetHasIndent(Index: Integer): Boolean;
    function GetRowAutoHeight: Boolean;
    function GetRowCaptionAutoHeight: Boolean;
    function GetRowCaptionEndEllipsis: Boolean;
    function GetShowRowCaption(Index: Integer): Boolean;
    function GetShowRowData(Index: Integer): Boolean;
  protected
    function GetAreAllMasterRowsCollapsed: Boolean; override;

    function GetContentViewParams(ARecord: TcxCustomGridRecord;
      ATableItem: TcxCustomGridTableItem; AIsDataCell: Boolean = False): TcxViewParams; override;
    function GetCaptionRowViewParams(ARecord: TcxCustomGridRecord;
      ACardRow: TcxGridCardViewRow): TcxViewParams; virtual;
    function GetCardViewParams(ARecord: TcxCustomGridRecord): TcxViewParams; virtual;
    function GetRowCaptionViewParams(ARecord: TcxCustomGridRecord;
      ACardRow: TcxGridCardViewRow): TcxViewParams; virtual;
  public
    function GridView: TcxGridCardView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    function Styles: TcxGridCardViewStyles; reintroduce; overload;

    procedure GetVisibleCardRows(ACard: TcxGridCard; AList: TList);
    function IsFirstRow(ACardRow: TcxGridCardViewRow): Boolean;
    function IsLastRow(ACardRow: TcxGridCardViewRow): Boolean;

    property CaptionSeparator: string read GetCaptionSeparator;
    property CardAutoWidth: Boolean read GetCardAutoWidth;
    property CardCaptionWidth: Integer read GetCardCaptionWidth;
    property CardCount: Integer read GetCardCount;
    property CardRowCount: Integer read GetCardRowCount;
    property CardRows[Index: Integer]: TcxGridCardViewRow read GetCardRow;
    property Cards[Index: Integer]: TcxGridCard read GetCard;
    property CardSeparatorColor: Integer read GetCardSeparatorColor;
    property CardSeparatorThickness: Integer read GetCardSeparatorThickness;
    property CardWidth: Integer read GetCardWidth;
    property HasCardsSeparator: Boolean read GetHasCardsSeparator;
    property HasIndent[Index: Integer]: Boolean read GetHasIndent;
    property RowCaptionAutoHeight: Boolean read GetRowCaptionAutoHeight;
    property RowCaptionEndEllipsis: Boolean read GetRowCaptionEndEllipsis;
    property RowAutoHeight: Boolean read GetRowAutoHeight;
    property ShowRowCaption[Index: Integer]: Boolean read GetShowRowCaption;
    property ShowRowData[Index: Integer]: Boolean read GetShowRowData;
  end;

  TdxGridCustomLayoutViewFormatter = class(TdxCustomGridTableViewFormatter)
  private
    function GetRecord(Index: Integer): TcxGridCustomLayoutRecord;
  protected
    procedure AddHorizontalDelimiters; override;
    function GetViewWidth: Integer; override;

    function GetCardAutoWidth: Boolean; virtual;
    function GetFirstRecordOffset: Integer; virtual;
    function GetInterRecordsSpaceHorz: Integer; virtual;
    function GetInterRecordsSpaceVert: Integer; virtual;
    function GetRecordsAreaWidth: Integer; virtual;
  public
    function Adapter: TdxGridCustomLayoutViewAdapter; reintroduce; overload;
    function Builder: TdxGridCustomLayoutViewBuilder; reintroduce; overload;

    function GetItemViewParams(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams; override;

    function GetRecordClass(AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass; virtual;
    procedure DoInitializeRecord(ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord); virtual;
    procedure DoInitializeRecordRow(ARecord: TdxReportCell); virtual;

    property CardAutoWidth: Boolean read GetCardAutoWidth;
    property Records[Index: Integer]: TcxGridCustomLayoutRecord read GetRecord;
  end;

  TdxGridCardViewFormatter = class(TdxGridCustomLayoutViewFormatter)
  private
    function GetAutoWidth: Boolean;
    function GetCaptionAutoWidth: Boolean;
    function GetCard(Index: Integer): TcxGridCard;
    function GetCardCount: Integer;
    function GetKeepSameHeight: Boolean;
    function GetKeepSameWidth: Boolean;
    function GetRowAutoHeight: Boolean;
  protected
    function GetCardAutoWidth: Boolean; override;
    function GetFirstRecordOffset: Integer; override;
    function GetInterRecordsSpaceHorz: Integer; override;
    function GetInterRecordsSpaceVert: Integer; override;

    function GetContentBackgroundBitmapStyleIndex(ATableItem: TcxCustomGridTableItem): Integer; override;
    function IsHorizontalLayout(AGridCard: TcxGridCard): Boolean;
    function MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer; override;

    function NeedDelimitByRows: Boolean; virtual;
  public
    function Adapter: TdxGridCardViewAdapter; reintroduce; overload;
    function Builder: TdxGridCardViewBuilder; reintroduce; overload;

    { Cards }
    procedure DoInitializeRecord(ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord); override;
    function GetRecordClass(AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass; override;
    function GetCardViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams; virtual;

    { Cards Indent }
    procedure DoInitializeCardRowIndent(AnItem: TdxReportCellExpandButton;
      ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard); virtual;

    { Cards Caption }
    procedure DoInitializeCardRowCaption(AnItem: TdxReportCellText;
      ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard); virtual;
    procedure DoReportLinkInitializeCardRowCaption(AnItem: TdxReportCellText;
      ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard); virtual;
    function GetCardRowCaptionClass(ACardRow: TcxGridCardViewRow): TdxReportCellTextClass; virtual;
    function GetCardRowCaptionViewParams(ARecord: TcxCustomGridRecord;
      ACardRow: TcxGridCardViewRow): TdxReportItemViewParams; virtual;

    { Cards Data }
    procedure DoReportLinkInitializeCardRowData(AnItem: TAbstractdxReportCellData;
      ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard); virtual;
    function GetItemViewParams(ATableItem: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams; override;

    property AutoWidth: Boolean read GetAutoWidth;
    property CaptionAutoWidth: Boolean read GetCaptionAutoWidth;
    property CardCount: Integer read GetCardCount;
    property Cards[Index: Integer]: TcxGridCard read GetCard;
    property KeepSameHeight: Boolean read GetKeepSameHeight;
    property KeepSameWidth: Boolean read GetKeepSameWidth;
    property RowAutoHeight: Boolean read GetRowAutoHeight;
  end;

  TdxGridCustomLayoutViewBuilder = class(TdxCustomGridTableViewBuilder)
  private
    FMaxRecordWidth: Integer;
    FRecords: TList;
    function GetRecord(Index: Integer): TdxReportCustomLayoutRecord;
    function GetRecordCount: Integer;
  protected
    procedure DoBuildViewBody; override;

    procedure CreateRecords; virtual;
    procedure PlaceRecords; virtual;
    procedure DoResizeRecords; virtual;
    procedure ResizeRecords;
    procedure SetSameRecordsWidth(AWidth: Integer);

    procedure CalculateMaxRecordWidth; virtual;

    function GetRecordProducer: TdxGridCustomLayoutViewRecordProducer;
    function GetRecordProducerClass: TdxGridCustomLayoutViewRecordProducerClass; virtual;

    function CreateRecord(AParent: TdxReportCell; ARecord: TcxGridCustomLayoutRecord): TdxReportCell; virtual;

    property RecordCount: Integer read GetRecordCount;
    property Records[Index: Integer]: TdxReportCustomLayoutRecord read GetRecord;
  public
    constructor Create(AReportLink: TdxGridReportLink;
      AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridCustomLayoutViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;

    property MaxRecordWidth: Integer read FMaxRecordWidth;
  end;

  TdxGridCardViewBuilder = class(TdxGridCustomLayoutViewBuilder)
  private
    function GetCard(Index: Integer): TdxReportCard;
    function GetCardCount: Integer;

    procedure AdjustCardRowsCellSides;
  protected
    procedure DoBuildViewBody; override;

    function GetRecordProducer: TdxGridCardViewCardsRowProducer; reintroduce; overload;
    function GetRecordProducerClass: TdxGridCustomLayoutViewRecordProducerClass; override;
    procedure DoResizeRecords; override;

    function GridView: TcxGridCardView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;

    property CardCount: Integer read GetCardCount;
    property Cards[Index: Integer]: TdxReportCard read GetCard;
  public
    function Adapter: TdxGridCardViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridCardViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  { DBCardView Adapter, Formatter and Builder }

  TdxGridDBCardViewAdapter = class(TdxGridCardViewAdapter)
  protected
    function DataController: TcxGridDBDataController; reintroduce; overload;
    function DBDataModeController: TcxDBDataModeController; override;
  public
    function GridView: TcxGridDBCardView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TdxGridDBCardViewBuilder = class(TdxGridCardViewBuilder)
  public
    function Adapter: TdxGridDBCardViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

  { ChartView Adapter, Formatter and Builder }

  TdxGridChartViewAdapter = class(TdxCustomGridViewAdapter)
  protected
    function CreateGraphic(AGraphicClass: TGraphicClass; AWidth: Integer): TGraphic;
    function GetCanUseOnEveryPageMode: Boolean; override;
  public
    function GridView: TcxGridChartView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  end;

  TdxGridChartViewFormatter = class(TdxCustomGridViewFormatter)
  strict private
    function GetGraphicClass: TGraphicClass;
    function GetGraphicTransparent: Boolean;
  protected
    function CreateChartImage: TGraphic; virtual;
    procedure DoInitializeChartItem(AItem: TdxReportCellDpiAwareGraphic); virtual;
    procedure DoReportLinkInitializeChartItem(AItem: TdxReportCellGraphic); virtual;
    function GetChartItemClass: TdxReportCellGraphicClass;
    function GetViewWidth: Integer; override;
  public
    function Adapter: TdxGridChartViewAdapter; reintroduce; overload;
    function Builder: TdxGridChartViewBuilder; reintroduce; overload;

    property GraphicClass: TGraphicClass read GetGraphicClass;
    property GraphicTransparent: Boolean read GetGraphicTransparent;
  end;

  TdxGridChartViewBuilder = class(TdxCustomGridViewBuilder)
  protected
    function GetChartProducer: TdxGridChartViewChartProducer; virtual;
    function GetChartProducerClass: TdxGridChartViewChartProducerClass; virtual;
    procedure CreateChart; virtual;
    procedure DoBuildViewBody; override;

    function GridView: TcxGridChartView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    function Adapter: TdxGridChartViewAdapter; reintroduce; overload;
    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridChartViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  { Producers }

  TdxGridViewRowProducer = class
  private
    FBuilder: TdxCustomGridViewBuilder;
    FHost: TdxReportCell;
    FMasterIndents: TList;
    FRow: TdxReportCell;
    FRowHeight: Integer;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetHostClass: TdxReportCellClass;
    function GetIndentWidth: Integer;
    function GetMasterBuilder(Index: Integer): TdxCustomGridViewBuilder;
    function GetMasterIndent(Index: Integer): TdxReportCellBox;
    function GetMasterIndentCount: Integer;
    function GetRowHostClass: TdxReportCellClass;
    function GetRowOriginX: Integer;
    function GetRowWidth: Integer;
  protected
    procedure AddLeadingMasterIndentsAndSeparators; virtual;
    procedure AddMasterIndents(AMasterFormatter: TdxCustomGridViewFormatter;
      AMasterAdapter: TdxCustomGridViewAdapter; ALevel: Integer; var ALeftPos: Integer);
    procedure AddMasterSeparator(AMasterFormatter: TdxCustomGridViewFormatter;
      AMasterAdapter: TdxCustomGridViewAdapter; var ALeftPos: Integer;
      ASeparatorKind: TdxVerticalDetailsSeparatorKind);
    procedure AddTrailingMasterSeparators; virtual;
    function CalculateItemHeight(ACanvas: TdxPSReportRenderCustomCanvas;
      AnItem: TdxReportVisualItem): Integer; virtual;
    procedure CalculateRowAutoHeight; virtual;
    procedure CalculateRowHeight; virtual;
    function CreateDetailsSeparator(AParent: TdxReportCell): TdxReportCellBox; virtual;
    function CreateMasterIndent(AParent: TdxReportCell): TdxReportCellExpandButton; virtual;
    procedure CreateRow; virtual;
    procedure CreateRowHost(const AHostInfo: TdxGridAttributeHostInfo); virtual;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean; virtual;
    function GetDetailsSeparatorClass: TdxReportCellBoxClass; virtual;
    function GetMasterIndentClass: TdxReportCellExpandButtonClass; virtual;
    procedure FixupMasterIndentsHeight; virtual;
    procedure FixupRowDataItemHeight(AnItem: TdxReportVisualItem); virtual;
    procedure FixupRowDataHeight; virtual;
    procedure FixupRowHeight; virtual;
    procedure FixupRowOwnHeight; virtual;
    procedure InitializeDetailsSeparator(AFormatter: TdxCustomGridViewFormatter;
      ASeparator: TdxReportCellBox; ASeparatorKind: TdxVerticalDetailsSeparatorKind); virtual;
    procedure InitializeMasterIndent(AFormatter: TdxCustomGridViewFormatter;
      AIndent: TdxReportCellExpandButton; AIndex, ALevel: Integer); virtual;
    procedure InitializeHost; virtual;
    procedure InitializeRow; virtual;

    function GetAutoHeight: Boolean; virtual;
    function GetItemsSite: TdxReportCell; virtual;
    function GetLineCount: Integer; virtual;
    function GetLineHeight: Integer; virtual;
    function GetWidth: Integer; virtual;

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property HostClass: TdxReportCellClass read GetHostClass;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); virtual;
    destructor Destroy; override;

    function Adapter: TdxCustomGridViewAdapter; overload; virtual;
    function Builder: TdxCustomGridViewBuilder; overload; virtual;
    function Formatter: TdxCustomGridViewFormatter; overload; virtual;

    function Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell; virtual;

    property AutoHeight: Boolean read GetAutoHeight;
    property Host: TdxReportCell read FHost;
    property IndentWidth: Integer read GetIndentWidth;
    property ItemsSite: TdxReportCell read GetItemsSite;
    property LineCount: Integer read GetLineCount;
    property LineHeight: Integer read GetLineHeight;
    property MasterBuilders[Index: Integer]: TdxCustomGridViewBuilder read GetMasterBuilder;
    property MasterIndentCount: Integer read GetMasterIndentCount;
    property MasterIndents[Index: Integer]: TdxReportCellBox read GetMasterIndent;
    property Row: TdxReportCell read FRow;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowOriginX: Integer read GetRowOriginX;
    property RowWidth: Integer read GetRowWidth;
    property Width: Integer read GetWidth;
  end;

  TdxGridViewCustomRowProducer = class(TdxGridViewRowProducer)
  private
    FItem: TAbstractdxReportCellData;
  protected
    procedure AddItem; virtual;
    function CreateItem(AParent: TdxReportCell): TAbstractdxReportCellData; virtual;
    procedure FixupRowDataHeight; override;
    procedure InitializeItem; virtual;

    function GetAutoHeight: Boolean; override;
    function GetItemClass: TdxReportCellDataClass; virtual;

    property Item: TAbstractdxReportCellData read FItem;
  public
    function Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell; override;
  end;

  TdxGridViewCaptionProducer = class(TdxGridViewCustomRowProducer)
  private
    function GetItem: TdxReportCellText;
  protected
    procedure InitializeItem; override;
    procedure InitializeRow; override;

    function GetItemClass: TdxReportCellDataClass; override;
    function GetLineHeight: Integer; override;
  public
    property Item: TdxReportCellText read GetItem;
  end;

  TdxGridViewFilterBarProducer = class(TdxGridViewCustomRowProducer)
  private
    function GetItem: TdxReportCellText;
  protected
    procedure InitializeItem; override;
    procedure InitializeRow; override;

    function GetItemClass: TdxReportCellDataClass; override;
    function GetLineHeight: Integer; override;
  public
    property Item: TdxReportCellText read GetItem;
  end;

  TdxGridViewDetailsSeparatorProducer = class(TdxGridViewCustomRowProducer)
  private
    function GetItem: TdxReportCellBox;
  protected
    procedure InitializeItem; override;
    procedure InitializeRow; override;

    function GetItemClass: TdxReportCellDataClass; override;
    function GetLineHeight: Integer; override;
  public
    property Item: TdxReportCellBox read GetItem;
  end;

  TdxGridViewDetailsTopSeparatorProducer = class(TdxGridViewDetailsSeparatorProducer)
  protected
    procedure InitializeDetailsSeparator(AFormatter: TdxCustomGridViewFormatter;
      ASeparator: TdxReportCellBox; ASeparatorKind: TdxVerticalDetailsSeparatorKind); override;
  end;

  TdxGridViewDetailsBottomSeparatorProducer = class(TdxGridViewDetailsSeparatorProducer)
  protected
    procedure InitializeDetailsSeparator(AFormatter: TdxCustomGridViewFormatter;
      ASeparator: TdxReportCellBox; ASeparatorKind: TdxVerticalDetailsSeparatorKind); override;
  end;

  TdxGridViewTerminatorProducer = class(TdxGridViewDetailsSeparatorProducer)
  protected
    procedure InitializeItem; override;
    function GetLineHeight: Integer; override;
  end;

  TdxGridTableViewRowProducer = class(TdxGridViewRowProducer)
  public
    function Adapter: TdxGridTableViewAdapter; reintroduce; overload;
    function Builder: TdxGridTableViewBuilder; reintroduce; overload;
    function Formatter: TdxGridTableViewFormatter; reintroduce; overload;
  end;

  TdxGridTableViewRowSubItemsProducer = class(TdxGridTableViewRowProducer)
  private
    FIndents: TList;
    FSubItems: TList;
    function GetColumn(Index: Integer): TcxGridColumn;
    function GetIndent(Index: Integer): TdxReportCellExpandButton;
    function GetSubItem(Index: Integer): TdxReportVisualItem;
  protected
    procedure AddIndents(AParent: TdxReportCell); virtual;
    procedure AddSubItems(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;
    function CreateSubItem(AnIndex: Integer; AParent: TdxReportCell): TAbstractdxReportCellData;
    function IsItemIndent(AnItem: TdxReportVisualItem): Boolean;

    procedure FixupIndentsHeight; virtual;
    procedure FixupRowDataHeight; override;
    procedure FixupRowHeight; override;

    procedure InitializeIndentList; virtual;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); virtual;
    procedure InitializeSubItemList; virtual;

    function GetHasSubItem(Index: Integer): Boolean; virtual;
    function GetIndentCount: Integer; virtual;
    function GetSubItemBound(Index: Integer): TRect; virtual;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; virtual; abstract;
    function GetSubItemCount: Integer; virtual;
  public
    constructor Create(ABuilder: TdxCustomGridViewBuilder); override;
    destructor Destroy; override;

    function Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell; override;

    property Columns[Index: Integer]: TcxGridColumn read GetColumn;
    property HasSubItem[Index: Integer]: Boolean read GetHasSubItem;
    property IndentCount: Integer read GetIndentCount;
    property Indents[Index: Integer]: TdxReportCellExpandButton read GetIndent;
    property SubItemBounds[Index: Integer]: TRect read GetSubItemBound;
    property SubItemClasses[Index: Integer]: TdxReportCellDataClass read GetSubItemClass;
    property SubItemCount: Integer read GetSubItemCount;
    property SubItems[Index: Integer]: TdxReportVisualItem read GetSubItem; default;
  end;

  TdxGridTableViewHeadersProducer = class(TdxGridTableViewRowSubItemsProducer)
  protected
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
  end;

  TdxGridTableViewFootersProducer = class(TdxGridTableViewRowSubItemsProducer)
  private
    function GetSummaryItems: TcxDataSummaryItems; virtual;
  protected
    procedure CalculateRowAutoHeight; override;
    procedure CalculateRowHeight; override;
    procedure FixupRowDataItemHeight(AnItem: TdxReportVisualItem); override;
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetHasSubItem(Index: Integer): Boolean; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
    function GetSubItemCount: Integer; override;

    property SummaryItems: TcxDataSummaryItems read GetSummaryItems;
  end;

  TdxGridTableViewCustomDataRowProducer = class(TdxGridTableViewRowSubItemsProducer)
  private
    FGridRow: TcxCustomGridRow;
    FGridRowIndex: Integer;
    function GetIndentArea: Integer;
    function GetIndentBounds(Index: Integer): TRect;
  protected
    procedure AddIndents(AParent: TdxReportCell); override;
    function CreateIndent(AnIndex: Integer; AParent: TdxReportCell): TdxReportCellExpandButton; virtual;
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); virtual;

    function GetIndentCount: Integer; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
  public
    function GridRow: TcxCustomGridRow; overload; virtual;
    function Produce(AHostInfo: TdxGridAttributeHostInfo;
      AGridRow: TcxCustomGridRow; AGridRowIndex: Integer): TdxReportCell; reintroduce; virtual;

    property GridRowIndex: Integer read FGridRowIndex;
    property IndentArea: Integer read GetIndentArea;
    property IndentBounds[Index: Integer]: TRect read GetIndentBounds;
  end;

  TdxCustomGridTableViewRowSeparatorProducer = class(TdxGridTableViewCustomDataRowProducer)
  protected
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetSubItemCount: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
  public
    function Produce(AHostInfo: TdxGridAttributeHostInfo;
      AGridRow: TcxCustomGridRow): TdxReportCell; reintroduce; virtual;
  end;

  TdxGridTableViewRowSeparatorProducer = class(TdxCustomGridTableViewRowSeparatorProducer)
  private
    FIsLast: Boolean;
  protected
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetIndentCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
  public
    function Produce(AHostInfo: TdxGridAttributeHostInfo; AGridRow: TcxCustomGridRow;
      AnIsLast: Boolean): TdxReportCell; reintroduce; virtual;
    property IsLast: Boolean read FIsLast;
  end;

  TdxGridTableViewGroupRowSeparatorProducer = class(TdxCustomGridTableViewRowSeparatorProducer)
  protected
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;
    function GetLineHeight: Integer; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
  end;

  TdxGridTableViewDataRowProducer = class(TdxGridTableViewCustomDataRowProducer)
  private
    FPreviewItem: TdxReportVisualItem;//TAbstractdxReportCellData;
    function GetPreviewColumn: TcxGridColumn;
    function GetPreviewPlace: TcxGridPreviewPlace;
  protected
    procedure AddPreview(AParent: TdxReportCell); virtual;
    procedure AddSubItems(AParent: TdxReportCell); override;
    procedure CalculateRowAutoHeight; override;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean; override;
    function CreatePreview(AParent: TdxReportCell): TdxReportVisualItem; virtual;
    function GetPreviewClass: TdxReportCellDataClass; virtual;
    procedure FixupRowDataHeight; override;
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); override;
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetCellAutoHeight: Boolean; virtual;
    function GetHasPreview: Boolean; virtual;
    function GetIndentCount: Integer; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetPreviewHeight: Integer; virtual;
    function GetPreviewLineCount: Integer; virtual;
    function GetPreviewLineHeight: Integer; virtual;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
  public
    function GridRow: TcxGridDataRow; reintroduce; overload;

    property CellAutoHeight: Boolean read GetCellAutoHeight;
    property HasPreview: Boolean read GetHasPreview;
    property PreviewColumn: TcxGridColumn read GetPreviewColumn;
    property PreviewHeight: Integer read GetPreviewHeight;
    property PreviewItem: TdxReportVisualItem read FPreviewItem;//TAbstractdxReportCellData read FPreviewItem;
    property PreviewLineCount: Integer read GetPreviewLineCount;
    property PreviewLineHeight: Integer read GetPreviewLineHeight;
    property PreviewPlace: TcxGridPreviewPlace read GetPreviewPlace;
  end;

  TcxGridSummaryItemInfo = class
  public
    Bounds: TRect;
    Column: TcxGridColumn;
    DisplayText: string;
    SummaryIndex: Integer;
    ViewParams: TcxViewParams;
  end;

  TdxGridTableViewGroupRowProducer = class(TdxGridTableViewCustomDataRowProducer)
  private
    function GetSummaryItemInfo(Index: Integer): TcxGridSummaryItemInfo;
  protected
    SummaryItemInfoList: TcxObjectList;
    RowViewParams: TcxViewParams;
    function AddSummaryGroupInfo(AItem: TcxDataSummaryItem; const ADisplayText: string): Boolean;
    procedure AddSubItems(AParent: TdxReportCell); override;
    procedure DeleteInfo(AIndex: Integer);
    procedure InitializeRow; override;
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;
    procedure InitializeSubItemList; override;
    function GetAutoHeight: Boolean; override;
    function GetIndentCount: Integer; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
    function GetSubItemCount: Integer; override;
    procedure PrepareSummaryItemsInfo;
    procedure PrepareSummaryItemsBounds;
  public
    function GridRow: TcxGridGroupRow; reintroduce; overload;
    function Produce(AHostInfo: TdxGridAttributeHostInfo;
      AGridRow: TcxCustomGridRow; AGridRowIndex: Integer): TdxReportCell; override;

    property SummaryItemInfos[Index: Integer]: TcxGridSummaryItemInfo read GetSummaryItemInfo;
  end;

  TdxGridTableViewMasterRowProducer = class(TdxGridTableViewDataRowProducer)
  protected
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetIndentCount: Integer; override;
  public
    function GridRow: TcxGridMasterDataRow; reintroduce; overload;
  end;

  TdxGridTableViewGroupFooterSubItemInfo = class
  private
    FSummaryItem: TcxDataSummaryItem;
    FSummaryValue: Variant;
  public
    constructor Create(ASummaryItem: TcxDataSummaryItem; ASummaryValue: Variant); virtual;

    property SummaryItem: TcxDataSummaryItem read FSummaryItem;
    property SummaryValue: Variant read FSummaryValue;
  end;

  TdxGridTableViewGroupFooterProducer = class(TdxGridTableViewCustomDataRowProducer)
  private
    FGroupLevel: Integer;
    FIndex: Integer;
    FInternalLineCount: Integer;
    FItemsSite: TdxReportCell;
    FSubItemInfos: TdxFastObjectList;

    function GetGroupLevel: Integer;
    function GetHostBounds: TRect;
    function GetSubItemInfo(AIndex: Integer): TdxGridTableViewGroupFooterSubItemInfo;
  protected
    procedure AddSubItems(AParent: TdxReportCell); override;
    procedure CalculateRowAutoHeight; override;
    procedure CalculateSubItemPosition(AIndex: Integer; out AColIndex, ARowIndex: Integer);
    function CanCreateSubItem(ASummaryItem: TcxDataSummaryItem): Boolean; virtual;
    function CreateSubItemInfo(ASummaryItem: TcxDataSummaryItem;
      ASummaryValue: Variant): TdxGridTableViewGroupFooterSubItemInfo; virtual;
    procedure FixupRowDataItemHeight(AnItem: TdxReportVisualItem); override;
    procedure InitializeIndent(AIndent: TdxReportCellExpandButton; AIndex: Integer); override;
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;
    function GetAutoHeight: Boolean; override;
    function GetCorrectRowIndex(AGridRow: TcxCustomGridRow; AGroupLevel: Integer): Integer;
    function GetHasSubItem(Index: Integer): Boolean; override;
    function GetIndentCount: Integer; override;
    function GetItemsSite: TdxReportCell; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
    function GetSubItemCount: Integer; override;
    procedure PopulateSubItemInfos(AGroupIndex: Integer); virtual;
    procedure UpdateLineCount; virtual;

    property SubItemInfoList: TdxFastObjectList read FSubItemInfos;
    property SubItemInfos[AIndex: Integer]: TdxGridTableViewGroupFooterSubItemInfo read GetSubItemInfo;
  public
    function Produce(AHostInfo: TdxGridAttributeHostInfo; AGridRow: TcxCustomGridRow;
      AGroupLevel, AnIndex: Integer): TdxReportCell; reintroduce; virtual;

    property GroupLevel: Integer read GetGroupLevel;
    property HostBounds: TRect read GetHostBounds;
    property Index: Integer read FIndex;
    property InternalLineCount: Integer read FInternalLineCount;
  end;

  TdxGridTableViewBandsProducer = class(TdxGridTableViewRowSubItemsProducer)
  protected
    procedure CalculateRowHeight; override;
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
    function GetSubItemCount: Integer; override;
  public
    function Adapter: TdxGridBandedTableViewAdapter; reintroduce; overload;
    function Builder: TdxGridBandedTableViewBuilder; reintroduce; overload;
    function Formatter: TdxGridBandedTableViewFormatter; reintroduce; overload;
  end;

  TdxGridWinExplorerViewRecordProducer = class(TdxGridViewRowProducer)
  protected
    procedure CalculateRecordSize(ARecord: TdxReportWinExplorerViewRecord;
      ARecordViewInfo: TcxCustomGridRecordViewInfo); virtual;
    procedure CalculateRowHeight; override;
    function CreateRecord(AParent: TdxReportCell;
      AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxReportWinExplorerViewRecord; virtual;
    procedure CreateRecordSubItems(ARecord: TdxReportWinExplorerViewRecord;
      ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo); virtual;
    function GetRecordClass: TdxReportWinExplorerViewRecordClass; virtual;
    procedure InitializeRecord(ARecord: TdxReportWinExplorerViewRecord;
      AGridRecord: TcxGridWinExplorerViewCustomRecord); virtual;
    procedure InitializeRow; override;
  public
    function Adapter: TdxGridWinExplorerViewAdapter; reintroduce; overload;
    function Builder: TdxGridWinExplorerViewBuilder; reintroduce; overload;
    function Formatter: TdxGridWinExplorerViewFormatter; reintroduce; overload;
    function Produce(AHostInfo: TdxGridAttributeHostInfo; ARowHeight: Integer): TdxReportCell; reintroduce; virtual;
  end;

  TdxGridWinExplorerViewDataRecordProducer = class(TdxGridWinExplorerViewRecordProducer)
  protected
    procedure CreateDataCell(AParent: TdxReportWinExplorerViewRecord;
      ACellViewInfo: TcxGridWinExplorerViewCustomCellViewInfo); virtual;
    procedure CreateRecordSubItems(ARecord: TdxReportWinExplorerViewRecord;
      ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo); override;
    function GetRecordClass: TdxReportWinExplorerViewRecordClass; override;
  end;

  TdxGridWinExplorerViewGroupRecordProducer = class(TdxGridWinExplorerViewRecordProducer)
  protected
    procedure CreateRecordSubItems(ARecord: TdxReportWinExplorerViewRecord;
      ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo); override;
    procedure CreateTextItem(AParent: TdxReportWinExplorerViewRecord;
      ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo); virtual;
    function GetRecordClass: TdxReportWinExplorerViewRecordClass; override;
  end;

  TdxGridCustomLayoutViewRecordProducer = class(TdxGridViewRowProducer)
  protected
    procedure CalculateRowHeight; override;
    function GetAutoHeight: Boolean; override;
    procedure InitializeRow; override;
  public
    function Builder: TdxGridCustomLayoutViewBuilder; reintroduce; overload;
    function Formatter: TdxGridCustomLayoutViewFormatter; reintroduce; overload;

    function CreateRecord(AParent: TdxReportCell; AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecord; virtual;
    function Produce(AHostInfo: TdxGridAttributeHostInfo; ARowHeight: Integer): TdxReportCell; reintroduce; virtual;
    procedure InjectRecord(ARecord: TdxReportCustomLayoutRecord; AIndex: Integer); virtual;
  end;

  TdxGridCardViewCardsRowProducer = class(TdxGridCustomLayoutViewRecordProducer)
  public
    function Adapter: TdxGridCardViewAdapter; reintroduce; overload;
    function Builder: TdxGridCardViewBuilder; reintroduce; overload;
    function Formatter: TdxGridCardViewFormatter; reintroduce; overload;
  end;

  TdxGridChartViewChartProducer = class(TdxGridViewCustomRowProducer)
  strict private
    FChartImage: TGraphic;

    function GetChartImage: TGraphic;
    function GetItem: TdxReportCellDpiAwareGraphic;
  protected
    procedure CalculateRowHeight; override;
    function GetItemClass: TdxReportCellDataClass; override;
    procedure InitializeItem; override;

    property ChartImage: TGraphic read GetChartImage;
    property Item: TdxReportCellDpiAwareGraphic read GetItem;
  public
    function Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell; override;
    function Formatter: TdxGridChartViewFormatter; reintroduce; overload;
  end;

  { Options }

  TdxGridReportLinkOptionsClass = class of TdxGridReportLinkOptions;

  TdxGridReportLinkOptions = class(TdxCustomReportLinkOptions)
  private
    function GetReportLink: TdxGridReportLink;
  public
    property ReportLink: TdxGridReportLink read GetReportLink;
  end;

  TdxGridReportLinkOptionsCards = class;

  TdxGridReportLinkCardsShadowClass = class of TdxGridReportLinkCardsShadow;

  TdxGridReportLinkCardsShadow = class(TPersistent)
  private
    FColor: TColor;
    FDepth: TdxGridCardShadowDepth;
    FOptionsCards: TdxGridReportLinkOptionsCards;
    FPosition: TdxGridCardShadowPosition;
    function GetActualColor: TColor;
    function GetVisible: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetDepth(Value: TdxGridCardShadowDepth);
    procedure SetPosition(Value: TdxGridCardShadowPosition);
  protected
    procedure Changed; dynamic;
  public
    constructor Create(AnOptionsCards: TdxGridReportLinkOptionsCards); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;

    property ActualColor: TColor read GetActualColor;
    property OptionsCards: TdxGridReportLinkOptionsCards read FOptionsCards;
    property Visible: Boolean read GetVisible;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Depth: TdxGridCardShadowDepth read FDepth write SetDepth default dxDefaultCardsShadowDepth;
    property Position: TdxGridCardShadowPosition read FPosition write SetPosition default cspBottomRight;
  end;

  TdxGridReportLinkOptionsCustomLayoutView = class(TdxGridReportLinkOptions)
  private
    FAutoWidth: Boolean;
    FInterRecordsSpaceHorz: Integer;
    FInterRecordsSpaceVert: Integer;
    procedure SetAutoWidth(Value: Boolean);
    procedure SetInterRecordsSpaceHorz(Value: Integer);
    procedure SetInterRecordsSpaceVert(Value: Integer);
  protected
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property InterRecordsSpaceHorz: Integer read FInterRecordsSpaceHorz write SetInterRecordsSpaceHorz default 4;
    property InterRecordsSpaceVert: Integer read FInterRecordsSpaceVert write SetInterRecordsSpaceVert default 4;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  end;

  TdxGridReportLinkOptionsCardsClass = class of TdxGridReportLinkOptionsCards;

  TdxGridReportLinkOptionsCards = class(TdxGridReportLinkOptionsCustomLayoutView)
  private
    FBorders: Boolean;
    FKeepSameHeight: Boolean;
    FKeepSameWidth: Boolean;
    FRowBordersHorz: Boolean;
    FRowBordersVert: Boolean;
    FShadow: TdxGridReportLinkCardsShadow;
    function GetInterCardsSpaceHorz: Integer;
    function GetInterCardsSpaceVert: Integer;
    function GetShadow: TdxGridReportLinkCardsShadow;
    procedure SetBorders(Value: Boolean);
    procedure SetInterCardsSpaceHorz(Value: Integer);
    procedure SetInterCardsSpaceVert(Value: Integer);
    procedure SetKeepSameHeight(Value: Boolean);
    procedure SetKeepSameWidth(Value: Boolean);
    procedure SetRowBordersHorz(Value: Boolean);
    procedure SetRowBordersVert(Value: Boolean);
    procedure SetShadow(Value: TdxGridReportLinkCardsShadow);
  protected
    function DesignerTabIndex: Integer; override;
    function GetShadowClass: TdxGridReportLinkCardsShadowClass; virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property AutoWidth default False;
    property Borders: Boolean read FBorders write SetBorders default True;
    property InterCardsSpaceHorz: Integer read GetInterCardsSpaceHorz write SetInterCardsSpaceHorz default 4;
    property InterCardsSpaceVert: Integer read GetInterCardsSpaceVert write SetInterCardsSpaceVert default 4;
    property KeepSameHeight: Boolean read FKeepSameHeight write SetKeepSameHeight default True;
    property KeepSameWidth: Boolean read FKeepSameWidth write SetKeepSameWidth default True;
    property RowBordersHorz: Boolean read FRowBordersHorz write SetRowBordersHorz default False;
    property RowBordersVert: Boolean read FRowBordersVert write SetRowBordersVert default False;
    property Shadow: TdxGridReportLinkCardsShadow read GetShadow write SetShadow;
  end;

  TdxGridReportLinkOptionsChartsClass = class of TdxGridReportLinkOptionsCharts;

  TdxGridReportLinkOptionsCharts = class(TdxGridReportLinkOptions)
  private
    FGraphicClass: TGraphicClass;
    FIsGraphicClassAssigned: Boolean;
    FTransparent: Boolean;
    function GetGraphicClass: TGraphicClass;
    function GetGraphicClassName: string;
    function IsGraphicClassNameStored: Boolean;
    procedure SetGraphicClass(Value: TGraphicClass);
    procedure SetGraphicClassName(const Value: string);
    procedure SetTransparent(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    function DefaultGraphicClass: TGraphicClass; virtual; //TBitmap
    function SupportsGraphicClass(AGraphicClass: TGraphicClass): Boolean; virtual; //excluding TIcon

    property GraphicClass: TGraphicClass read GetGraphicClass write SetGraphicClass;
  published
    property GraphicClassName: string read GetGraphicClassName write SetGraphicClassName stored IsGraphicClassNameStored;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TdxGridReportLinkOptionsDetailsClass = class of TdxGridReportLinkOptionsDetails;

  TdxGridReportLinkOptionsDetails = class(TdxGridReportLinkOptions)
  private
    FOnlyFocusedView: Boolean;
    FStartFromFocusedView: Boolean;
    procedure SetOnlyFocusedView(Value: Boolean);
    procedure SetStartFromFocusedView(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property OnlyFocusedView: Boolean read FOnlyFocusedView write SetOnlyFocusedView default False;
    property StartFromFocusedView: Boolean read FStartFromFocusedView write SetStartFromFocusedView default False;
  end;

  TdxGridReportLinkOptionsExpanding = class(TdxCustomReportLinkOptionsExpanding)
  private
    FExpandCards: Boolean;
    FExpandGroupRows: Boolean;
    FExpandMasterRows: Boolean;
    function GetHasAny: Boolean;
    function GetReportLink: TdxGridReportLink;
    procedure SetExpandCards(Value: Boolean);
    procedure SetExpandGroupRows(Value: Boolean);
    procedure SetExpandMasterRows(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    property HasAny: Boolean read GetHasAny;
    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property ExpandCards: Boolean read FExpandCards write SetExpandCards default False;
    property ExpandGroupRows: Boolean read FExpandGroupRows write SetExpandGroupRows default False;
    property ExpandMasterRows: Boolean read FExpandMasterRows write SetExpandMasterRows default False;
  end;

  TdxGridReportLinkOptionsFormatting = class(TdxCustomReportLinkOptionsFormatting)
  private
    FConsumeSelectionStyle: Boolean;
    function GetReportLink: TdxGridReportLink;
    procedure SetConsumeSelectionStyle(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property ConsumeSelectionStyle: Boolean read FConsumeSelectionStyle write SetConsumeSelectionStyle default False;
    property UseLookAndFeelColors;
  end;

  TdxGridReportLinkOptionsLevelsClass = class of TdxGridReportLinkOptionsLevels;

  TdxGridReportLinkOptionsLevels = class(TdxGridReportLinkOptions)
  private
    FRiseActiveLevelOntoTop: Boolean;
    FSkipEmptyViews: Boolean;
    FUnwrap: Boolean;
    FUnwrapTopLevel: Boolean;
    procedure SetRiseActiveLevelOntoTop(Value: Boolean);
    procedure SetSkipEmptyViews(Value: Boolean);
    procedure SetUnwrap(Value: Boolean);
    procedure SetUnwrapTopLevel(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property RiseActiveLevelOntoTop: Boolean read FRiseActiveLevelOntoTop write SetRiseActiveLevelOntoTop default True;
    property SkipEmptyViews: Boolean read FSkipEmptyViews write SetSkipEmptyViews default True;
    property Unwrap: Boolean read FUnwrap write SetUnwrap default False;
    property UnwrapTopLevel: Boolean read FUnwrapTopLevel write SetUnwrapTopLevel default True;
  end;

  TdxGridReportLinkOptionsOnEveryPage = class(TdxCustomTableControlReportLinkOptionsOnEveryPage)
  private
    FCaption: Boolean;
    FFilterBar: Boolean;
    function GetReportLink: TdxGridReportLink;
    procedure SetCaption(Value: Boolean);
    procedure SetFilterBar(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    function HasInstalledAttribute(AnAttribute: TdxGridAttributeClass): Boolean; virtual;
    procedure SetAll; virtual;
    procedure UnsetAll; virtual;

    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property BandHeaders;
    property Caption: Boolean read FCaption write SetCaption default True;
    property FilterBar: Boolean read FFilterBar write SetFilterBar default True;
    property Footers;
    property Headers;
  end;

  TdxGridReportLinkOptionsPagination = class(TdxCustomTableControlReportLinkOptionsPagination)
  private
    FOneGroupPerPage: Boolean;
    FTopLevelGroup: Boolean;
    function GetReportLink: TdxGridReportLink;
    procedure SetOneGroupPerPage(Value: Boolean);
    procedure SetTopLevelGroup(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property Band;
    property Column;
    property Custom;
    property OneGroupPerPage: Boolean read FOneGroupPerPage write SetOneGroupPerPage default False;
    property Row;
    property TopLevelGroup: Boolean read FTopLevelGroup write SetTopLevelGroup default False;
  end;

  TdxGridReportLinkOptionsPreview = class(TdxCustomTableControlReportLinkOptionsPreview)
  private
    function GetReportLink: TdxGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TdxGridReportLink read GetReportLink;
  end;

  TdxGridReportLinkOptionsRefinements = class(TdxCustomReportLinkOptionsRefinements)
  private
    function GetReportLink: TdxGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TdxGridReportLink read GetReportLink;
  end;

  TdxGridReportLinkOptionsSelection = class(TdxCustomTableControlReportLinkOptionsSelection)
  private
    function GetReportLink: TdxGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TdxGridReportLink read GetReportLink;
  end;

  TdxGridReportLinkOptionsSize = class(TdxCustomReportLinkOptionsSize)
  private
    function GetReportLink: TdxGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property AutoWidth;
  end;

  TdxGridReportLinkOptionsView = class(TdxCustomTableControlReportLinkOptionsView)
  private
    FCaption: Boolean;
    FFilterBar: Boolean;
    FGroupFooters: Boolean;
    function GetReportLink: TdxGridReportLink;
    procedure SetCaption(Value: Boolean);
    procedure SetFilterBar(Value: Boolean);
    procedure SetGroupFooters(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    function HasInstalledAttribute(AnAttribute: TdxGridAttributeClass): Boolean; virtual;
    procedure SetAll;
    procedure UnsetAll;

    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property BandHeaders;
    property Caption: Boolean read FCaption write SetCaption default True;
    property ExpandButtons;
    property FilterBar: Boolean read FFilterBar write SetFilterBar default True;
    property Footers;
    property GroupFooters: Boolean read FGroupFooters write SetGroupFooters default True;
    property Headers;
  end;

  TdxGridAttributeHostInfo = class
  private
    FParent: TdxReportCell;
  public
    Origin: TPoint;
    procedure Initialize(AParent: TdxReportCell);
    property Parent: TdxReportCell read FParent;
  end;

  TdxGridAttributeHostInfoServicesClass = class of TdxGridAttributeHostInfoServices;

  TdxGridAttributeHostInfoServices = class
  private
    FPageDetailsHostInfo: TdxGridAttributeHostInfo;
    FPageFootersHostInfo: TdxGridAttributeHostInfo;
    FPageHeadersHostInfo: TdxGridAttributeHostInfo;
    FReportLink: TdxGridReportLink;
    function GetActiveView: TcxCustomGridView;
    function GetActiveViewHelper: TdxCustomGridViewHelperClass;
    function GetArePageFootersAssigned: Boolean;
    function GetArePageHeadersAssigned: Boolean;
    function GetCanUseBandHeadersOnEveyPage: Boolean;
    function GetCanUseCaptionOnEveryPage: Boolean;
    function GetCanUseFilterBarOnEveryPage: Boolean;
    function GetCanUseFootersOnEveryPage: Boolean;
    function GetCanUseHeadersOnEveryPage: Boolean;
    function GetIsInconsistentBandHeadersState: Boolean;
    function GetIsInconsistentFilterBarState: Boolean;
    function GetIsInconsistentFootersState: Boolean;
    function GetIsInconsistentHeadersState: Boolean;
    function GetLevelSeparatorBottomHostInfo: TdxGridAttributeHostInfo;
    function GetLevelSeparatorTopHostInfo: TdxGridAttributeHostInfo;
    function GetOptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage;
    function GetOptionsView: TdxGridReportLinkOptionsView;
    function GetPageDetails: TdxReportCell;
    function GetPageFooters: TdxReportCell;
    function GetPageHeaders: TdxReportCell;
  protected
    procedure CreateHostInfos;
    procedure DestroyHostInfos;

    function GetBandHeadersHostInfo: TdxGridAttributeHostInfo; virtual;
    function GetCaptionHostInfo: TdxGridAttributeHostInfo; virtual;
    function GetFilterBarHostInfo: TdxGridAttributeHostInfo; virtual;
    function GetFootersHostInfo: TdxGridAttributeHostInfo; virtual;
    function GetHeadersHostInfo: TdxGridAttributeHostInfo; virtual;
    function GetInconsistentStateText: string; virtual;
    function GetIsInconsistentState: Boolean; virtual;
    function HasCells: Boolean;
    function IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean;

    //function HasAttributeOnEveryPage: Boolean;

    property ActiveView: TcxCustomGridView read GetActiveView;
    property ActiveViewHelper: TdxCustomGridViewHelperClass read GetActiveViewHelper;
    property OptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage read GetOptionsOnEveryPage;
    property OptionsView: TdxGridReportLinkOptionsView read GetOptionsView;
    property PageDetails: TdxReportCell read GetPageDetails;
    property PageFooters: TdxReportCell read GetPageFooters;
    property PageHeaders: TdxReportCell read GetPageHeaders;
  public
    constructor Create(AReportLink: TdxGridReportLink); virtual;
    destructor Destroy; override;
    procedure Initialize;

    property ArePageFootersAssigned: Boolean read GetArePageFootersAssigned;
    property ArePageHeadersAssigned: Boolean read GetArePageHeadersAssigned;
    property BandHeadersHostInfo: TdxGridAttributeHostInfo read GetBandHeadersHostInfo;
    property CanUseBandHeadersOnEveyPage: Boolean read GetCanUseBandHeadersOnEveyPage;
    property CanUseCaptionOnEveryPage: Boolean read GetCanUseCaptionOnEveryPage;
    property CanUseFilterBarOnEveryPage: Boolean read GetCanUseFilterBarOnEveryPage;
    property CanUseFootersOnEveryPage: Boolean read GetCanUseFootersOnEveryPage;
    property CanUseHeadersOnEveryPage: Boolean read GetCanUseHeadersOnEveryPage;
    property CaptionHostInfo: TdxGridAttributeHostInfo read GetCaptionHostInfo;
    property FilterBarHostInfo: TdxGridAttributeHostInfo read GetFilterBarHostInfo;
    property FootersHostInfo: TdxGridAttributeHostInfo read GetFootersHostInfo;
    property HeadersHostInfo: TdxGridAttributeHostInfo read GetHeadersHostInfo;
    property InconsistentStateText: string read GetInconsistentStateText;
    property IsInconsistentBandHeadersState: Boolean read GetIsInconsistentBandHeadersState;
    property IsInconsistentFilterBarState: Boolean read GetIsInconsistentFilterBarState;
    property IsInconsistentFootersState: Boolean read GetIsInconsistentFootersState;
    property IsInconsistentHeadersState: Boolean read GetIsInconsistentHeadersState;
    property IsInconsistentState: Boolean read GetIsInconsistentState;
    property LevelSeparatorBottomHostInfo: TdxGridAttributeHostInfo read GetLevelSeparatorBottomHostInfo;
    property LevelSeparatorTopHostInfo: TdxGridAttributeHostInfo read GetLevelSeparatorTopHostInfo;
    property PageDetailsHostInfo: TdxGridAttributeHostInfo read FPageDetailsHostInfo;
    property PageFootersHostInfo: TdxGridAttributeHostInfo read FPageFootersHostInfo;
    property PageHeadersHostInfo: TdxGridAttributeHostInfo read FPageHeadersHostInfo;
    property ReportLink: TdxGridReportLink read FReportLink;
  end;

  TdxGridReportLinkStylesClass = class of TdxGridReportLinkStyles;

  TdxGridReportLinkStyles = class(TdxCustomReportLinkStyles)
  private
    function GetReportLink: TdxGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;

    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    class function GetStyleCaption(AnIndex: Integer): string; override;
    function GetStyleIndexByCaption(const Caption: string): Integer; override;

    function IsCardViewStyle(AStyle: TcxStyle): Boolean;
  public
    procedure Assign(Source: TPersistent); override;

    procedure GetBandHeaderParams(ABand: TcxGridBand; out AParams: TcxViewParams); virtual;
    procedure GetCaptionParams(ATabLevel: TcxGridLevel; out AParams: TcxViewParams); virtual;
    procedure GetCardCaptionRowParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetCardRowCaptionParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetFilterBarParams(out AParams: TcxViewParams); virtual;
    procedure GetFooterParams(ARecord: TcxCustomGridRecord; AGroupLevel: Integer;
      AItem: TcxGridColumn; out AParams: TcxViewParams); virtual;
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AGroupLevel: Integer;
      out AParams: TcxViewParams); virtual;
    procedure GetHeaderParams(AItem: TcxGridColumn; out AParams: TcxViewParams); virtual;
    procedure GetPreviewParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetSelectionParams(out AParams: TcxViewParams); virtual;

    property ReportLink: TdxGridReportLink read GetReportLink;
  published
    property BandHeader: TcxStyle Index vspsGridBandHeader read GetValue write SetValue;
    property Caption: TcxStyle Index vspsGridCaption read GetValue write SetValue;
    property CardCaptionRow: TcxStyle Index vspsGridCardCaptionRow read GetValue write SetValue;
    property CardRowCaption: TcxStyle Index vspsGridCardRowCaption read GetValue write SetValue;
    property Content: TcxStyle Index vspsGridContent read GetValue write SetValue;
    property ContentEven: TcxStyle Index vspsGridContentEven read GetValue write SetValue;
    property ContentOdd: TcxStyle Index vspsGridContentOdd read GetValue write SetValue;
    property FilterBar: TcxStyle Index vspsGridFilterBar read GetValue write SetValue;
    property Footer: TcxStyle Index vspsGridFooter read GetValue write SetValue;
    property Group: TcxStyle Index vspsGridGroup read GetValue write SetValue;
    property Header: TcxStyle Index vspsGridHeader read GetValue write SetValue;
    property Preview: TcxStyle Index vspsGridPreview read GetValue write SetValue;
    property Selection: TcxStyle Index vspsGridSelection read GetValue write SetValue;
    property StyleSheet;
  end;

  TdxGridReportLinkStyleSheet = class(TdxCustomReportLinkStyleSheet)
  private
    function GetStylesValue: TdxGridReportLinkStyles;
    procedure SetStylesValue(Value: TdxGridReportLinkStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TdxGridReportLinkStyles read GetStylesValue write SetStylesValue;
  end;

  TdxGridReportLinkProcessParams = record
    HasCardsInDetail: Boolean;
    HasMasterDetails: Boolean;
    HasMasterDetailsInTopView: Boolean;
    HasOnlyCards: Boolean;
    HasOnlyCharts: Boolean;
    HasUnwrapableData: Boolean;
    CanUseOnEveryPageMode: Boolean;
  end;

  TdxGridRecordArray = array of TcxCustomGridRecord;

  TdxGridViewProc = procedure(AGridView: TcxCustomGridView) of object;

  TdxGridGetCustomPageBreaksEvent = procedure(Sender: TdxGridReportLink) of object;

  TdxGridReportLinkGetCellHeightEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
    ATableItem: TcxCustomGridTableItem; var AHeight: Integer) of object;

  { CustomDraw Events }

  TdxGridReportLinkCustomDrawBandCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxGridBandedTableView; ABand: TcxGridBand;
    AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
    AColumn: TcxGridColumn; AnItem: TAbstractdxReportCellData; var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawCardRowCaptionCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
    AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawCardRowDataCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
    AnItem: TAbstractdxReportCellData; var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawFilterBarEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxCustomGridTableView; AnItem: TdxReportCellString;
    var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawFooterCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxGridTableView; ARecord: TcxCustomGridRecord;
    AColumn: TcxGridColumn; ALevel: Integer; AnItem: TdxReportCellString;
    var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawHeaderCellEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxGridTableView; AColumn: TcxGridColumn;
    AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TdxGridReportLinkCustomDrawLevelCaptionEvent = procedure(Sender: TdxGridReportLink;
    ACanvas: TCanvas; AView: TcxCustomGridView; AnItem: TdxReportCellString;
    var ADone: Boolean) of object;

  { Initializtion Events }

  TdxGridReportLinkInitializeBandCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridBandedTableView; ABand: TcxGridBand; AnItem: TdxReportCellString) of object;

  TdxGridReportLinkInitializeCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
    AColumn: TcxGridColumn; AnItem: TAbstractdxReportCellData) of object;

  TdxGridReportLinkInitializeCardRowCaptionCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
    AnItem: TdxReportCellString) of object;

  TdxGridReportLinkInitializeCardRowDataCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
    AnItem: TAbstractdxReportCellData) of object;

  TdxGridReportLinkInitializeChartCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridChartView; AnItem: TdxReportCellGraphic) of object;

  TdxGridReportLinkInitializeFilterBarEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxCustomGridTableView; AnItem: TdxReportCellString) of object;

  TdxGridReportLinkInitializeFooterCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridTableView; ARecord: TcxCustomGridRecord;
    AColumn: TcxGridColumn; ALevel: Integer; AnItem: TdxReportCellString) of object;

  TdxGridReportLinkInitializeHeaderCellEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxGridTableView; AColumn: TcxGridColumn; AnItem: TdxReportCellString) of object;

  TdxGridReportLinkInitializeLevelCaptionEvent = procedure(Sender: TdxGridReportLink;
    AView: TcxCustomGridView; AnItem: TdxReportCellString) of object;

  { ReportLink }

  TdxGridReportLink = class(TdxCustomTableControlReportLink)
  strict private
    FDelimitersHardHorz: TList;
    FDelimitersHardVert: TList;
    FHostInfoServices: TdxGridAttributeHostInfoServices;
    FOptionsCards: TdxGridReportLinkOptionsCards;
    FOptionsCharts: TdxGridReportLinkOptionsCharts;
    FOptionsDetails: TdxGridReportLinkOptionsDetails;
    FOptionsLevels: TdxGridReportLinkOptionsLevels;
    FReportRows: TList;

    FOnCustomDrawBandCell: TdxGridReportLinkCustomDrawBandCellEvent;
    FOnCustomDrawCardRowCaptionCell: TdxGridReportLinkCustomDrawCardRowCaptionCellEvent;
    FOnCustomDrawCardRowDataCell: TdxGridReportLinkCustomDrawCardRowDataCellEvent;
    FOnCustomDrawCell: TdxGridReportLinkCustomDrawCellEvent;
    FOnCustomDrawFilterBar: TdxGridReportLinkCustomDrawFilterBarEvent;
    FOnCustomDrawFooterCell: TdxGridReportLinkCustomDrawFooterCellEvent;
    FOnCustomDrawHeaderCell: TdxGridReportLinkCustomDrawHeaderCellEvent;
    FOnCustomDrawLevelCaption: TdxGridReportLinkCustomDrawLevelCaptionEvent;
    FOnGetCellHeight: TdxGridReportLinkGetCellHeightEvent;
    FOnGetCustomPageBreaks: TdxGridGetCustomPageBreaksEvent;
    FOnInitializeBandCell: TdxGridReportLinkInitializeBandCellEvent;
    FOnInitializeCardRowCaptionCell: TdxGridReportLinkInitializeCardRowCaptionCellEvent;
    FOnInitializeCardRowDataCell: TdxGridReportLinkInitializeCardRowDataCellEvent;
    FOnInitializeCell: TdxGridReportLinkInitializeCellEvent;
    FOnInitializeChartCell: TdxGridReportLinkInitializeChartCellEvent;
    FOnInitializeFilterBar: TdxGridReportLinkInitializeFilterBarEvent;
    FOnInitializeFooterCell: TdxGridReportLinkInitializeFooterCellEvent;
    FOnInitializeHeaderCell: TdxGridReportLinkInitializeHeaderCellEvent;
    FOnInitializeLevelCaption: TdxGridReportLinkInitializeLevelCaptionEvent;

    function GetActiveStyles: TdxGridReportLinkStyles;
    function GetActiveView: TcxCustomGridView;
    function GetActiveViewHelper: TdxCustomGridViewHelperClass;
    function GetActiveViewMasterRow: TcxGridMasterDataRow;
    function GetActiveViewParentLevel: TcxGridLevel;
    function GetDesignWindow: TdxfmGridReportLinkDesignWindow;
    function GetGrid: TcxGrid;
    function GetOptionsExpanding: TdxGridReportLinkOptionsExpanding;
    function GetOptionsFormatting: TdxGridReportLinkOptionsFormatting;
    function GetOptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage;
    function GetOptionsPagination: TdxGridReportLinkOptionsPagination;
    function GetOptionsPreview: TdxGridReportLinkOptionsPreview;
    function GetOptionsRefinements: TdxGridReportLinkOptionsRefinements;
    function GetOptionsSelection: TdxGridReportLinkOptionsSelection;
    function GetOptionsSize: TdxGridReportLinkOptionsSize;
    function GetOptionsView: TdxGridReportLinkOptionsView;
    function GetReportDataCellByGridColumn(AColumn: TcxGridColumn): TAbstractdxReportCellData;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowByGridRecord(GridRecord: TcxCustomGridRecord): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetStyles: TdxGridReportLinkStyles;
    procedure SetOnCustomDrawBandCell(Value: TdxGridReportLinkCustomDrawBandCellEvent);
    procedure SetOnCustomDrawCell(Value: TdxGridReportLinkCustomDrawCellEvent);
    procedure SetOnCustomDrawCardRowCaptionCell(Value: TdxGridReportLinkCustomDrawCardRowCaptionCellEvent);
    procedure SetOnCustomDrawCardRowDataCell(Value: TdxGridReportLinkCustomDrawCardRowDataCellEvent);
    procedure SetOnCustomDrawFilterBar(Value: TdxGridReportLinkCustomDrawFilterBarEvent);
    procedure SetOnCustomDrawFooterCell(Value: TdxGridReportLinkCustomDrawFooterCellEvent);
    procedure SetOnCustomDrawHeaderCell(Value: TdxGridReportLinkCustomDrawHeaderCellEvent);
    procedure SetOnCustomDrawLevelCaption(Value: TdxGridReportLinkCustomDrawLevelCaptionEvent);
    procedure SetOptionsCards(Value: TdxGridReportLinkOptionsCards);
    procedure SetOptionsCharts(Value: TdxGridReportLinkOptionsCharts);
    procedure SetOptionsDetails(Value: TdxGridReportLinkOptionsDetails);
    procedure SetOptionsExpanding(Value: TdxGridReportLinkOptionsExpanding);
    procedure SetOptionsFormatting(Value: TdxGridReportLinkOptionsFormatting);
    procedure SetOptionsLevels(Value: TdxGridReportLinkOptionsLevels);
    procedure SetOptionsOnEveryPage(Value: TdxGridReportLinkOptionsOnEveryPage);
    procedure SetOptionsPagination(Value: TdxGridReportLinkOptionsPagination);
    procedure SetOptionsPreview(Value: TdxGridReportLinkOptionsPreview);
    procedure SetOptionsRefinements(Value: TdxGridReportLinkOptionsRefinements);
    procedure SetOptionsSelection(Value: TdxGridReportLinkOptionsSelection);
    procedure SetOptionsSize(Value: TdxGridReportLinkOptionsSize);
    procedure SetOptionsView(Value: TdxGridReportLinkOptionsView);
    procedure SetStyles(Value: TdxGridReportLinkStyles);
  protected
    FActualScaleFactor: Integer;
    FProcessedView: TcxCustomGridView;
    FProcessParams: TdxGridReportLinkProcessParams;

    procedure BeforeDesignReport; override;
    function CalculateActualScaleFactor: Integer; override;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConvertCoords; override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    function GetBreakPagesByHardDelimiters: Boolean; override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    function GetUseHardHorzDelimiters: Boolean; override;
    function GetUseHardVertDelimiters: Boolean; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;
    function IsDrawFootersOnEveryPage: Boolean; override;
    function IsDrawHeadersOnEveryPage: Boolean; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeHardDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;
    function NeedExpandGroups(AGridView: TcxCustomGridView; ARecursive: Boolean = True): Boolean;

    function GetAreNativeStylesAvailable: Boolean; override;
    function GetStylesClass: TdxCustomReportLinkStylesClass; override;
    function GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass; override;
    function GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet; override;
    procedure PrepareConstruct; override;

    procedure DoCustomDrawBandCell(ACanvas: TCanvas; AView: TcxGridBandedTableView;
      ABand: TcxGridBand; AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawCardRowCaptionCell(ACanvas: TCanvas; AView: TcxGridCardView;
      ACard: TcxGridCard; ARow: TcxGridCardViewRow; AnItem: TdxReportCellString;
      var ADone: Boolean); dynamic;
    procedure DoCustomDrawCardRowDataCell(ACanvas: TCanvas; AView: TcxGridCardView;
      ACard: TcxGridCard; ARow: TcxGridCardViewRow; AnItem: TAbstractdxReportCellData;
      var ADone: Boolean); dynamic;
    procedure DoCustomDrawCell(ACanvas: TCanvas; AView: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AColumn: TcxGridColumn;
      AnItem: TAbstractdxReportCellData; var ADone: Boolean); dynamic;
    procedure DoCustomDrawFilterBar(ACanvas: TCanvas; AView: TcxCustomGridTableView;
      AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawFooterCell(ACanvas: TCanvas; AView: TcxGridTableView;
      ARecord: TcxCustomGridRecord; AColumn: TcxGridColumn; ALevel: Integer;
      AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawHeaderCell(ACanvas: TCanvas; AView: TcxGridTableView;
      AColumn: TcxGridColumn; AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawLevelCaption(ACanvas: TCanvas; AView: TcxCustomGridView;
      AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoGetCellHeight(AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      ATableItem: TcxCustomGridTableItem; var AHeight: Integer); dynamic;
    procedure DoGetCustomPageBreaks; dynamic;
    procedure DoInitializeBandCell(AView: TcxGridBandedTableView; ABand: TcxGridBand;
      AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeCardRowCaptionCell(AView: TcxGridCardView; ACard: TcxGridCard;
      ARow: TcxGridCardViewRow; AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeCardRowDataCell(AView: TcxGridCardView; ACard: TcxGridCard;
      ARow: TcxGridCardViewRow; AnItem: TAbstractdxReportCellData); dynamic;
    procedure DoInitializeCell(AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AColumn: TcxGridColumn; AnItem: TAbstractdxReportCellData); dynamic;
    procedure DoInitializeChartCell(AView: TcxGridChartView; AnItem: TdxReportCellGraphic); dynamic;
    procedure DoInitializeFilterBar(AView: TcxCustomGridTableView; AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeFooterCell(AView: TcxGridTableView; ARecord: TcxCustomGridRecord;
      AColumn: TcxGridColumn; ALevel: Integer; AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeHeaderCell(AView: TcxGridTableView; AColumn: TcxGridColumn;
      AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeLevelCaption(AView: TcxCustomGridView; AnItem: TdxReportCellString); dynamic;

    procedure CreateOptions; override;
    procedure DestroyOptions; override;
    function GetOptionsCardsClass: TdxGridReportLinkOptionsCardsClass; virtual;
    function GetOptionsChartsClass: TdxGridReportLinkOptionsChartsClass; virtual;
    function GetOptionsDetailsClass: TdxGridReportLinkOptionsDetailsClass; virtual;
    function GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass; override;
    function GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass; override;
    function GetOptionsLevelsClass: TdxGridReportLinkOptionsLevelsClass; virtual;
    function GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass; override;
    function GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass; override;
    function GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass; override;
    function GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass; override;
    function GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass; override;
    function GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass; override;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; override;

    procedure AddReportRow(AReportRow: TdxReportCell);
    procedure AddHorizontalHardDelimiter(ADelimiter: Integer);
    procedure AddVerticalHardDelimiter(ADelimiter: Integer); overload;
    procedure AddVerticalHardDelimiter(ADelimiter: TdxReportCell); overload;
    procedure BuildTopLevelView(AGridView: TcxCustomGridView);
    procedure BuildTopLevelViews;
    procedure BuildView(AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView);
    function CreateViewAdapter(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView): TdxCustomGridViewAdapter;
    function CreateViewBuilder(AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView): TdxCustomGridViewBuilder;

    procedure DoExpandRows;
    procedure DoExpandViewRows(AGridView: TcxCustomGridView);
    procedure ForEachView(AMasterRow: TcxGridMasterDataRow; AProc: TdxGridViewProc);

    function GetItemCustomDrawInfo(AnItem: TdxReportVisualItem; out ADrawInfo: TdxGridCellCustomDrawInfo): TdxGridAttributeID; virtual;
    function IsCardViewStyle(AStyle: TcxStyle): Boolean; virtual;
    function IsCustomDrawn(AnAttributeID: TdxGridAttributeID): Boolean; virtual;
    function IsOffice11StyleGrouping: Boolean;

    procedure CalculateProcessParams;
    function CanAttributeBeUsedOnEveryPage(AnAttribute: TdxGridAttributeClass): Boolean;
    function IsAttributeUsedOnEveryPage(AnAttribute: TdxGridAttributeClass): Boolean;

    property ActiveStyles: TdxGridReportLinkStyles read GetActiveStyles;
    property ActiveView: TcxCustomGridView read GetActiveView;
    property ActiveViewHelper: TdxCustomGridViewHelperClass read GetActiveViewHelper;
    property ActiveViewMasterRow: TcxGridMasterDataRow read GetActiveViewMasterRow;
    property ActiveViewParentLevel: TcxGridLevel read GetActiveViewParentLevel;
    property DelimitersHardHorz: TList read FDelimitersHardHorz;
    property DelimitersHardVert: TList read FDelimitersHardVert;
    property HostInfoServices: TdxGridAttributeHostInfoServices read FHostInfoServices;
    property ProcessParams: TdxGridReportLinkProcessParams read FProcessParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure AddPageBreak(AGridRecord: TcxCustomGridRecord); overload;
    procedure AddPageBreak(const AGridRecords: array of TcxCustomGridRecord); overload;
    procedure AddPageBreak(const AGridRecords: TdxGridRecordArray); overload;
    procedure AddPageBreak(AGridRecords: TList); overload; // list of TcxCustomGridRecord

    procedure AddHorizontalPageBreak(AGridColumn: TcxGridColumn); overload;
    procedure AddHorizontalPageBreak(AGridColumns: TList); overload; // list of TcxGridColumn
    procedure AddHorizontalPageBreak(const AGridColumns: array of TcxGridColumn); overload;

    property Grid: TcxGrid read GetGrid;
    property DesignWindow: TdxfmGridReportLinkDesignWindow read GetDesignWindow;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property ReportRowsByGridRecord[GridRow: TcxCustomGridRecord]: TdxReportCell read GetReportRowByGridRecord;
    property ReportRowCount: Integer read GetReportRowCount;
  published
    property Color;
    property Font;
    property OptionsCards: TdxGridReportLinkOptionsCards read FOptionsCards write SetOptionsCards;
    property OptionsCharts: TdxGridReportLinkOptionsCharts read FOptionsCharts write SetOptionsCharts;
    property OptionsDetails: TdxGridReportLinkOptionsDetails read FOptionsDetails write SetOptionsDetails;
    property OptionsExpanding: TdxGridReportLinkOptionsExpanding read GetOptionsExpanding write SetOptionsExpanding;
    property OptionsFormatting: TdxGridReportLinkOptionsFormatting read GetOptionsFormatting write SetOptionsFormatting;
    property OptionsLevels: TdxGridReportLinkOptionsLevels read FOptionsLevels write SetOptionsLevels;
    property OptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage read GetOptionsOnEveryPage write SetOptionsOnEveryPage;
    property OptionsPagination: TdxGridReportLinkOptionsPagination read GetOptionsPagination write SetOptionsPagination;
    property OptionsPreview: TdxGridReportLinkOptionsPreview read GetOptionsPreview write SetOptionsPreview;
    property OptionsRefinements: TdxGridReportLinkOptionsRefinements read GetOptionsRefinements write SetOptionsRefinements;
    property OptionsSelection: TdxGridReportLinkOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsSize: TdxGridReportLinkOptionsSize read GetOptionsSize write SetOptionsSize;
    property OptionsView: TdxGridReportLinkOptionsView read GetOptionsView write SetOptionsView;
    property ScaleFonts;
    property StyleRepository;
    property Styles: TdxGridReportLinkStyles read GetStyles write SetStyles;
    property SupportedCustomDraw;

    property OnCustomDrawBandCell: TdxGridReportLinkCustomDrawBandCellEvent read FOnCustomDrawBandCell
      write SetOnCustomDrawBandCell;
    property OnCustomDrawCell: TdxGridReportLinkCustomDrawCellEvent read FOnCustomDrawCell
      write SetOnCustomDrawCell;
    property OnCustomDrawCardRowCaptionCell: TdxGridReportLinkCustomDrawCardRowCaptionCellEvent read FOnCustomDrawCardRowCaptionCell
      write SetOnCustomDrawCardRowCaptionCell;
    property OnCustomDrawCardRowDataCell: TdxGridReportLinkCustomDrawCardRowDataCellEvent read FOnCustomDrawCardRowDataCell
      write SetOnCustomDrawCardRowDataCell;
    property OnCustomDrawFilterBar: TdxGridReportLinkCustomDrawFilterBarEvent read FOnCustomDrawFilterBar
      write SetOnCustomDrawFilterBar;
    property OnCustomDrawFooterCell: TdxGridReportLinkCustomDrawFooterCellEvent read FOnCustomDrawFooterCell
      write SetOnCustomDrawFooterCell;
    property OnCustomDrawHeaderCell: TdxGridReportLinkCustomDrawHeaderCellEvent read FOnCustomDrawHeaderCell
      write SetOnCustomDrawHeaderCell;
    property OnCustomDrawLevelCaption: TdxGridReportLinkCustomDrawLevelCaptionEvent read FOnCustomDrawLevelCaption
      write SetOnCustomDrawLevelCaption;
    property OnGetCellHeight: TdxGridReportLinkGetCellHeightEvent read FOnGetCellHeight write FOnGetCellHeight;
    property OnGetCustomPageBreaks: TdxGridGetCustomPageBreaksEvent read FOnGetCustomPageBreaks write FOnGetCustomPageBreaks;
    property OnInitializeBandCell: TdxGridReportLinkInitializeBandCellEvent read FOnInitializeBandCell
      write FOnInitializeBandCell;
    property OnInitializeCardRowCaptionCell: TdxGridReportLinkInitializeCardRowCaptionCellEvent read FOnInitializeCardRowCaptionCell
      write FOnInitializeCardRowCaptionCell;
    property OnInitializeCardRowDataCell: TdxGridReportLinkInitializeCardRowDataCellEvent read FOnInitializeCardRowDataCell
      write FOnInitializeCardRowDataCell;
    property OnInitializeCell: TdxGridReportLinkInitializeCellEvent read FOnInitializeCell
      write FOnInitializeCell;
    property OnInitializeChartCell: TdxGridReportLinkInitializeChartCellEvent read FOnInitializeChartCell
      write FOnInitializeChartCell;
    property OnInitializeFilterBar: TdxGridReportLinkInitializeFilterBarEvent read FOnInitializeFilterBar
      write FOnInitializeFilterBar;
    property OnInitializeFooterCell: TdxGridReportLinkInitializeFooterCellEvent read FOnInitializeFooterCell
      write FOnInitializeFooterCell;
    property OnInitializeHeaderCell: TdxGridReportLinkInitializeHeaderCellEvent read FOnInitializeHeaderCell
      write FOnInitializeHeaderCell;
    property OnInitializeLevelCaption: TdxGridReportLinkInitializeLevelCaptionEvent read FOnInitializeLevelCaption
      write FOnInitializeLevelCaption;
  end;

  TcxGridCardView2OptionsView = class(TcxGridCardViewOptionsView)
  private
    FCardBorders: Boolean;
    FGridLineColor: TColor;
    FGridLines: TcxGridLines;
    FShadowDepth: Integer;
    function GetShadowVisible: Boolean;
    procedure SetCardBorders(Value: Boolean);
    procedure SetGridLineColor(Value: TColor);
    procedure SetGridLines(Value: TcxGridLines);
    procedure SetShadowDepth(Value: Integer);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property ShadowVisible: Boolean read GetShadowVisible;
  published
    property CardBorders: Boolean read FCardBorders write SetCardBorders default False;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLines: TcxGridLines read FGridLines write SetGridLines default glNone;
    property ShadowDepth: Integer read FShadowDepth write SetShadowDepth;
  end;

  TcxGridCardView2Styles = class(TcxGridCardViewStyles)
  private
    FOnGetCardShadowStyle: TcxGridGetRecordStyleEvent;
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetCardShadowParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
  published
    property CardShadow: TcxStyle index vsCardShadow read GetValue write SetValue;
    property OnGetCardShadowStyle: TcxGridGetRecordStyleEvent read FOnGetCardShadowStyle
      write FOnGetCardShadowStyle;
  end;

  TcxGridCardView2 = class(TcxGridCardView)
  protected
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
  end;

  TdxfmGridReportLinkDesignWindow = class(TdxfmCustomcxControlReportLinkDesignWindow)
    btnStyleBackgroundBitmap: TcxButton;
    btnStyleBackgroundBitmapClear: TcxButton;
    btnStyleColor: TcxButton;
    btnStyleFont: TcxButton;
    btnStyleRestoreDefaults: TcxButton;
    btnStyleSheetCopy: TcxButton;
    btnStyleSheetDelete: TcxButton;
    btnStyleSheetNew: TcxButton;
    btnStyleSheetRename: TcxButton;
    btnStylesSaveAs: TcxButton;
    bvlStylesHost: TdxLayoutItem;
    bvlWarningHost: TdxLayoutItem;
    cbxLookAndFeel: TcxComboBox;
    cbxStyleSheets: TcxComboBox;
    ccbxCardsShadowColor: TcxColorComboBox;
    chbxBandsOnEveryPage: TcxCheckBox;
    chbxCaptionsOnEveryPage: TcxCheckBox;
    chbxCardsAutoWidth: TcxCheckBox;
    chbxCardsBorder: TcxCheckBox;
    chbxCardsHorzLines: TcxCheckBox;
    chbxCardsKeepSameHeight: TcxCheckBox;
    chbxCardsKeepSameWidth: TcxCheckBox;
    chbxCardsVertLines: TcxCheckBox;
    chbxChartsTransparent: TcxCheckBox;
    chbxConsumeSelectionStyle: TcxCheckBox;
    chbxDisplayGraphicsAsText: TcxCheckBox;
    chbxDisplayTrackBarsAsText: TcxCheckBox;
    chbxExpandCards: TcxCheckBox;
    chbxExpandGroupRows: TcxCheckBox;
    chbxExpandMasterRows: TcxCheckBox;
    chbxFilterBarOnEveryPage: TcxCheckBox;
    chbxFlatCheckMarks: TcxCheckBox;
    chbxFootersOnEveryPage: TcxCheckBox;
    chbxGridAutoWidth: TcxCheckBox;
    chbxHeadersOnEveryPage: TcxCheckBox;
    chbxLevelsRiseActiveLevelOntoTop: TcxCheckBox;
    chbxLevelsSkipEmptyViews: TcxCheckBox;
    chbxLevelsUnwrap: TcxCheckBox;
    chbxLevelsUnwrapTopLevel: TcxCheckBox;
    chbxOnlyActiveView: TcxCheckBox;
    chbxPaginateByTopLevelGroups: TcxCheckBox;
    chbxPaginateOneGroupPerPage: TcxCheckBox;
    chbxPreviewAutoHeight: TcxCheckBox;
    chbxPreviewVisible: TcxCheckBox;
    chbxProcessExactSelection: TcxCheckBox;
    chbxProcessSelection: TcxCheckBox;
    chbxShowBands: TcxCheckBox;
    chbxShowCaptions: TcxCheckBox;
    chbxShowExpandButtons: TcxCheckBox;
    chbxShowFilterBar: TcxCheckBox;
    chbxShowFooters: TcxCheckBox;
    chbxShowGroupFooters: TcxCheckBox;
    chbxShowHeaders: TcxCheckBox;
    chbxStartFromActiveView: TcxCheckBox;
    chbxSuppressBackgroundBitmaps: TcxCheckBox;
    chbxTransparentGraphics: TcxCheckBox;
    chbxUseNativeStyles: TcxCheckBox;
    colCarModel: TcxGridBandedColumn;
    colIsSUVModel: TcxGridBandedColumn;
    colSpeedCount: TcxGridBandedColumn;
    colVendorCountry: TcxGridBandedColumn;
    colVendorLogo: TcxGridBandedColumn;
    colVendorName: TcxGridBandedColumn;
    cxStyleRepository1: TcxStyleRepository;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup16: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup17: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup18: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup19: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup20: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup21: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup22: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup23: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup24: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup25: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup26: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup27: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup28: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup29: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup30: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup31: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup32: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup33: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup34: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup35: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup36: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    lilblDetails: TdxLayoutItem;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem32: TdxLayoutItem;
    dxLayoutItem33: TdxLayoutItem;
    dxLayoutItem34: TdxLayoutItem;
    dxLayoutItem35: TdxLayoutItem;
    dxLayoutItem36: TdxLayoutItem;
    dxLayoutItem37: TdxLayoutItem;
    dxLayoutItem38: TdxLayoutItem;
    dxLayoutItem39: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem40: TdxLayoutItem;
    dxLayoutItem41: TdxLayoutItem;
    dxLayoutItem42: TdxLayoutItem;
    dxLayoutItem43: TdxLayoutItem;
    dxLayoutItem44: TdxLayoutItem;
    dxLayoutItem45: TdxLayoutItem;
    dxLayoutItem46: TdxLayoutItem;
    dxLayoutItem47: TdxLayoutItem;
    dxLayoutItem48: TdxLayoutItem;
    dxLayoutItem49: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem50: TdxLayoutItem;
    dxLayoutItem51: TdxLayoutItem;
    dxLayoutItem52: TdxLayoutItem;
    dxLayoutItem53: TdxLayoutItem;
    dxLayoutItem54: TdxLayoutItem;
    dxLayoutItem55: TdxLayoutItem;
    dxLayoutItem56: TdxLayoutItem;
    dxLayoutItem57: TdxLayoutItem;
    dxLayoutItem58: TdxLayoutItem;
    dxLayoutItem59: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem60: TdxLayoutItem;
    dxLayoutItem61: TdxLayoutItem;
    dxLayoutItem62: TdxLayoutItem;
    dxLayoutItem63: TdxLayoutItem;
    dxLayoutItem64: TdxLayoutItem;
    dxLayoutItem65: TdxLayoutItem;
    dxLayoutItem66: TdxLayoutItem;
    dxLayoutItem67: TdxLayoutItem;
    dxLayoutItem68: TdxLayoutItem;
    dxLayoutItem69: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem70: TdxLayoutItem;
    dxLayoutItem71: TdxLayoutItem;
    dxLayoutItem72: TdxLayoutItem;
    dxLayoutItem73: TdxLayoutItem;
    dxLayoutItem74: TdxLayoutItem;
    dxLayoutItem75: TdxLayoutItem;
    dxLayoutItem76: TdxLayoutItem;
    dxLayoutItem77: TdxLayoutItem;
    dxLayoutItem78: TdxLayoutItem;
    dxLayoutItem79: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem80: TdxLayoutItem;
    dxLayoutItem81: TdxLayoutItem;
    dxLayoutItem82: TdxLayoutItem;
    dxLayoutItem83: TdxLayoutItem;
    dxLayoutItem84: TdxLayoutItem;
    dxLayoutItem85: TdxLayoutItem;
    dxLayoutItem86: TdxLayoutItem;
    dxLayoutItem87: TdxLayoutItem;
    dxLayoutItem88: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    ilStylesPopup: TcxImageList;
    imgCardFraming: TcxImage;
    imgCardShadow: TcxImage;
    imgCardSizes: TcxImage;
    imgCardSpacing: TcxImage;
    imgCharts: TcxImage;
    imgDetails: TcxImage;
    imgExpanding: TcxImage;
    imgGridSize: TcxImage;
    imgLevels: TcxImage;
    imgLookAndFeel: TcxImage;
    imgOnEveryPage: TcxImage;
    imgPagination: TcxImage;
    imgPreview: TcxImage;
    imgRefinements: TcxImage;
    imgSelection: TcxImage;
    imgShow: TcxImage;
    lblCardFraming: TcxLabel;
    lblCardShadow: TcxLabel;
    lblCardShadowColor: TdxLayoutItem;
    lblCardShadowDepth: TdxLayoutItem;
    lblCardSizes: TcxLabel;
    lblCardSpaceHorz: TdxLayoutItem;
    lblCardSpaceVert: TdxLayoutItem;
    lblCardSpacing: TcxLabel;
    lblChartsOptions: TcxLabel;
    lblDetails: TcxLabel;
    lblExpanding: TcxLabel;
    lblGridSize: TcxLabel;
    lblLevels: TcxLabel;
    lblLookAndFeel: TcxLabel;
    lblOnEveryPage: TcxLabel;
    lblPagination: TcxLabel;
    lblPreviewMaxLineCount: TdxLayoutItem;
    lblPreviewOptions: TcxLabel;
    lblPreviewWindow: TdxLayoutItem;
    lblRefinements: TcxLabel;
    lblSelection: TcxLabel;
    lblShow: TcxLabel;
    lblStyleSheets: TcxLabel;
    lblUseNativeStyles: TcxLabel;
    miLine2: TMenuItem;
    miLine3: TMenuItem;
    miLine4: TMenuItem;
    milLine: TMenuItem;
    miStyleBackgroundBitmap: TMenuItem;
    miStyleBackgroundBitmapClear: TMenuItem;
    miStyleColor: TMenuItem;
    miStyleFont: TMenuItem;
    miStyleRestoreDefaults: TMenuItem;
    miStylesSaveAs: TMenuItem;
    miStylesSelectAll: TMenuItem;
    pcMain: TdxLayoutGroup;
    pmStyles: TPopupMenu;
    pnlPreview: TPanel;
    PreviewBandedView: TcxGridBandedTableView;
    PreviewGrid: TcxGrid;
    PreviewGridLevel: TcxGridLevel;
    seCardsShadowDepth: TcxSpinEdit;
    seCardsSpaceHorz: TcxSpinEdit;
    seCardsSpaceVert: TcxSpinEdit;
    sePreviewMaxLineCount: TcxSpinEdit;
    styleCardBorder: TcxStyle;
    styleCardShadow: TcxStyle;
    tshBehaviors: TdxLayoutGroup;
    tshCards: TdxLayoutGroup;
    tshCharts: TdxLayoutGroup;
    tshFormatting: TdxLayoutGroup;
    tshPreview: TdxLayoutGroup;
    tshStyles: TdxLayoutGroup;
    tshView: TdxLayoutGroup;
    lgDetails: TdxLayoutGroup;
    lgLevels: TdxLayoutGroup;
    lgPagination: TdxLayoutGroup;

    procedure OptionsViewClick(Sender: TObject);
    procedure OptionsOnEveryPageClick(Sender: TObject);
    procedure OptionsSelectionClick(Sender: TObject);
    procedure OptionsExpandingClick(Sender: TObject);
    procedure OptionsRefinementClick(Sender: TObject);
    procedure LookAndFeelClick(Sender: TObject);
    procedure PreviewVisibleClick(Sender: TObject);
    procedure PreviewAutoHeightClick(Sender: TObject);
    procedure OptionsCardsClick(Sender: TObject);
    procedure IsSUVModelCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure VendorLogoCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure PreviewBandedViewCustomDrawColumnHeader(
      Sender: TcxGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
    procedure PreviewBandedViewCustomDrawBandHeader(
      Sender: TcxGridBandedTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridBandHeaderViewInfo; var ADone: Boolean);
    procedure PreviewBandedViewCustomDrawFooterCell(
      Sender: TcxGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
    procedure OptionsDetailsClick(Sender: TObject);
    procedure OptionsSizeClick(Sender: TObject);
    procedure OptionsLevelsClick(Sender: TObject);
    procedure OptionsFormatingClick(Sender: TObject);
    procedure StyleColorClick(Sender: TObject);
    procedure StyleFontClick(Sender: TObject);
    procedure StyleBackgroundBitmapClick(Sender: TObject);
    procedure lbxStylesClick(Sender: TObject);
    procedure pmStylesPopup(Sender: TObject);
    procedure cbxStyleSheetsClick(Sender: TObject);
    procedure StyleSheetNewClick(Sender: TObject);
    procedure StyleSheetCopyClick(Sender: TObject);
    procedure StyleSheetDeleteClick(Sender: TObject);
    procedure StylesSaveAsClick(Sender: TObject);
    procedure cbxStyleSheetsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StyleSheetRenameClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StyleBackgroundBitmapClearClick(Sender: TObject);
    procedure StyleRestoreDefaultsClick(Sender: TObject);
    procedure miStylesSelectAllClick(Sender: TObject);
    procedure OptionsPaginationClick(Sender: TObject);
    procedure lblUseNativeStylesClick(Sender: TObject);
    procedure chbxChartsTransparentClick(Sender: TObject);
    procedure cbxStyleSheetsPropertiesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas;
      AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure CardShadowDepthChanged(Sender: TObject);
    procedure CardShadowColorChanged(Sender: TObject);
    procedure CardSpaceHorzChanged(Sender: TObject);
    procedure CardSpaceVertChanged(Sender: TObject);
    procedure PreviewMaxLineCountChanged(Sender: TObject);
    procedure colSpeedCountCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
  private
    lbxStyles: TdxStylesListBox;
    PreviewCardView: TcxGridCardView2;
    PreviewCharView: TcxGridChartView;
    rowCarModel: TcxGridCardViewRow;
    rowIsSuvModel: TcxGridCardViewRow;
    rowSpeedCount: TcxGridCardViewRow;
    rowVendorCountry: TcxGridCardViewRow;
    rowVendorLogo: TcxGridCardViewRow;
    rowVendorName: TcxGridCardViewRow;
    wpIncorrectOnEveryPageState: TdxPSWarningPane;
    FGridLinkPreviewStyles: TdxGridReportLinkStyles;

    function GetActiveStyle: TcxStyle;
    function GetHasSelectedStyles: Boolean;
    function GetHasSelectedStylesWithAssignedBitmap: Boolean;
    function GetReportLink: TdxGridReportLink;

    function CanSelectAllStyles: Boolean;
    procedure ChangeActiveView(AView: TcxCustomGridView);
    procedure CreateCardView;
    procedure CreateChartView;
    procedure CreateControls;
    procedure CustomDrawBorders(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo);
    procedure CustomDrawFooter(ACanvas: TcxCanvas; R: TRect; AViewInfo: TcxCustomGridCellViewInfo);
    procedure CustomDrawHeader(ACanvas: TcxCanvas; R: TRect; AViewInfo: TcxCustomGridCellViewInfo);
    procedure CustomDrawTextRect(ACanvas: TcxCanvas; R: TRect; AViewInfo: TcxCustomGridCellViewInfo;
      ABackgroundBitmap: TGraphic);
    procedure InitializePreviewGrid;
    procedure InitializePreviewGridStyles;
    procedure LoadDataIntoPreviewGridChartView(AView: TcxGridChartView);
    procedure LoadDataIntoPreviewGridView(AView: TcxCustomGridTableView);
    procedure RecreateStylesListBox;
    procedure RestoreSelectedStyles(AList: TList);
    procedure SaveSelectedStyles(AList: TList);
    procedure SetActivePage;
    procedure UpdatePreviewGridStyles(const ACaption: string; AStyle: TcxStyle);
    procedure UpdateWarningPane;

    procedure SetOptionsCardsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsChartsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsDetailsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsExpandingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsFormattingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsLevelsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsOnEveryPageByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsPaginationByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsRefinementsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsSelectionByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsSizeByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsViewByIndex(Index: Integer; Value: Boolean);
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadGroupsIcons; override;
    procedure LoadStrings; override;
    procedure UpdateControlsState; override;
    procedure UpdatePreview; override;

    function GetDesignerTabIndex: Integer; override;
    procedure SetDesignerTabIndex(Value: Integer); override;

    procedure DoActiveStyleSheetChanged; override;
    procedure DoFormActivated(AnActive: Boolean); override;
    procedure DoRefreshStylesList; override;
    procedure DoStyleChanged(const ACaption: string; AStyle: TcxStyle); override;
    procedure DoStylesChanged(AStrings: TStrings; ARecreate: Boolean); override;

    procedure GetSelectedStyleNames(AStrings: TStrings); override;
    procedure GetStyleNames(out AStrings: TStrings); override;
    procedure GetStyleSheetNames(out AStrings: TStrings); override;

    property ActiveStyle: TcxStyle read GetActiveStyle;
    property HasSelectedStyles: Boolean read GetHasSelectedStyles;
    property HasSelectedStylesWithAssignedBitmap: Boolean read GetHasSelectedStylesWithAssignedBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ReportLink: TdxGridReportLink read GetReportLink;
  end;

implementation

{$R *.DFM}

uses
  RTLConsts, Registry, Forms, Variants, cxVariants, cxGridStrs,
  cxDataUtils, dxThemeManager, dxOffice11, dxPSGlbl,
  cxDrawTextUtils, dxPSUtl, dxBkgnd, dxPSPopupMan, dxPSRes, dxPSImgs, Math, dxDPIAwareUtils, dxPSdxSpreadSheetLnk,
  dxPSdxSpreadSheetLnkCore;

const
  SortOrderMap: array[TcxGridSortOrder] of TdxCellSortOrder = (csoNone, csoUp, csoDown);

  ExpandButtonInteriorColor = clBtnFace;
  DefaultFilterBarColor = clBtnShadow;
  DefaultFilterBarTextColor = clWindow;
  DefaultIndentWidth = 20;
  DefaultDataRowLineHeight = 19;
  DefaultGroupFooterLineHeight = 20;
  DefaultGroupRowLineHeight = 20;
  DefaultFooterLineHeight = 20;
  FilterBarTextOffset = 3;
  FirstCardOffset = 2;
  FooterItemInflateHorz = 2;
  FooterItemInflateVert = 3;
  TabsAreaOffset = 2;
  WinExplorerViewFirstRecordOffset = 2;
  WinExplorerViewRecordRecordsSpaceHorz = 4;
  WinExplorerViewRecordRecordsSpaceVert = 4;

var
  FGridLinkStyleSheetPrototype: TdxGridReportLinkStyleSheet;

type
  TdxPSGridDataCellPos = TcxGridDataCellPos;
  TcxDataControllerConditionalFormattingProviderAccess = class(TcxDataControllerConditionalFormattingProvider);

  TdxGridViewBuildersFactory = class(TdxCustomClassMaps)
  private
    function GetBuilderClass(GridView: TcxCustomGridView): TdxGridViewBuilderClass;
  public
    class function Instance: TdxGridViewBuildersFactory; reintroduce; overload;
    function CreateViewAdapter(AMasterAdapter: TdxCustomGridViewAdapter;
      AGridView: TcxCustomGridView): TdxCustomGridViewAdapter;
    function CreateViewBuilder(AReportLink: TdxGridReportLink; AMasterBuilder: TdxCustomGridViewBuilder;
      AGridView: TcxCustomGridView): TdxCustomGridViewBuilder;
    property BuilderClasses[GridView: TcxCustomGridView]: TdxGridViewBuilderClass read GetBuilderClass; default;
  end;

  TdxViewHelpersFactory = class(TdxCustomClassMaps)
  private
    function GetHelperClass(GridView: TcxCustomGridView): TdxCustomGridViewHelperClass;
  public
    class function Instance: TdxViewHelpersFactory; reintroduce; overload;
    function HelperClassByItem(AnItem: TdxReportVisualItem): TdxCustomGridViewHelperClass;
    property HelperClasses[GridView: TcxCustomGridView]: TdxCustomGridViewHelperClass read GetHelperClass; default;
  end;

  TdxRecordHelpersFactory = class(TdxCustomClassMaps)
  private
    function GetHelperClass(GridRecord: TcxCustomGridRecord): TdxCustomGridRecordHelperClass;
  public
    class function Instance: TdxRecordHelpersFactory; reintroduce; overload;
    property HelperClasses[GridRecord: TcxCustomGridRecord]: TdxCustomGridRecordHelperClass read GetHelperClass; default;
  end;


  TcxCustomGridCellPainterAccess = class(TcxCustomGridCellPainter);
  TcxCustomGridCellViewInfoAccess = class(TcxCustomGridCellViewInfo);
  TcxCustomGridRecordAccess = class(TcxCustomGridRecord);
  TcxCustomGridTableControllerAccess = class(TcxCustomGridTableController);
  TcxCustomGridTableItemAccess = class(TcxCustomGridTableItem);
  TcxCustomGridTableFilteringAccess = class(TcxCustomGridTableFiltering);
  TcxCustomGridTableOptionsViewAccess = class(TcxCustomGridTableOptionsView);
  TcxCustomGridViewAccess = class(TcxCustomGridView);
  TcxGridColumnAccess = class(TcxGridColumn);
  TcxGridMasterDataRowAccess = class(TcxGridMasterDataRow);
  TcxGridTableViewInfoAccess = class(TcxGridTableViewInfo);


{ Helpers }

function CarVendorName(Index: Integer): string;
begin
  case Index of
    0: Result := cxGetResourceString(@sdxCarManufacturerName1);
    1: Result := cxGetResourceString(@sdxCarManufacturerName2);
    2: Result := cxGetResourceString(@sdxCarManufacturerName3);
    else
      Result := cxGetResourceString(@sdxCarManufacturerName4);
  end;
end;

function CarVendorCountry(Index: Integer): string;
begin
  case Index of
    0: Result := cxGetResourceString(@sdxCarManufacturerCountry1);
    1: Result := cxGetResourceString(@sdxCarManufacturerCountry2);
    2: Result := cxGetResourceString(@sdxCarManufacturerCountry3);
    else
      Result := cxGetResourceString(@sdxCarManufacturerCountry4);
  end;
end;

function CarModel(Index: Integer): string;
begin
  case Index of
    0: Result := cxGetResourceString(@sdxCarModel1);
    1: Result := cxGetResourceString(@sdxCarModel2);
    2: Result := cxGetResourceString(@sdxCarModel3);
    else
      Result := cxGetResourceString(@sdxCarModel4);
  end;
end;

{ CustomGridCellPainter Helpers }

procedure CustomGridCellPainter_DrawBorders(AInstance: TcxCustomGridCellPainter);
begin
  TcxCustomGridCellPainterAccess(AInstance).DrawBorders;
end;

{ CustomGridCellViewInfo Helpers }

function CustomGridCellViewInfo_GetPainterClass(AInstance: TcxCustomGridCellViewInfo): TcxCustomGridCellPainterClass;
begin
  Result := TcxCustomGridCellViewInfoAccess(AInstance).GetPainterClass;
end;

{ CustomGridRecord Helpers }

function CustomGridRecord_GetIsParent(AInstance: TcxCustomGridRecord): Boolean;
begin
  Result := TcxCustomGridRecordAccess(AInstance).IsParent;
end;

{ CustomGridTableController Helpers }

function CustomGridTableController_GetMultiSelect(AInstance: TcxCustomGridTableController): Boolean;
begin
  Result := TcxCustomGridTableControllerAccess(AInstance).MultiSelect;
end;

{ CustomGridTableFiltering Helpers }

function CustomGridTableFiltering_GetPosition(AInstance: TcxCustomGridTableFiltering): TcxGridFilterPosition;
begin
  Result := TcxCustomGridTableFilteringAccess(AInstance).Position;
end;

{ CustomGridTableOptionsView Helpers }

function CustomGridTableOptionsView_GetCellAutoHeight(AInstance: TcxCustomGridTableOptionsView): Boolean;
begin
  Result := TcxCustomGridTableOptionsViewAccess(AInstance).CellAutoHeight;
end;

{ CustomGridView Helpers }

function CustomGridView_GetBackgroundBitmaps(AInstance: TcxCustomGridView): TcxCustomGridBackgroundBitmaps;
begin
  Result := TcxCustomGridViewAccess(AInstance).BackgroundBitmaps;
end;

function CustomGridView_GetDataController(AInstance: TcxCustomGridView): TcxCustomDataController;
begin
  Result := TcxCustomGridViewAccess(AInstance).DataController;
end;

function CustomGridView_GetStyles(AInstance: TcxCustomGridView): TcxCustomGridStyles;
begin
  Result := TcxCustomGridViewAccess(AInstance).Styles;
end;

{ GridColumn Helpers }

function GridColumn_GetFixed(AInstance: TcxGridColumn): Boolean;
begin
  Result := TcxGridColumnAccess(AInstance).Fixed;
end;

function GridColumn_DoCompareValuesForCellMerging(AInstance: TcxGridColumn;
  ARow1: TcxGridDataRow; AProperties1: TcxCustomEditProperties; const AValue1: TcxEditValue;
  ARow2: TcxGridDataRow; AProperties2: TcxCustomEditProperties; const AValue2: TcxEditValue): Boolean;
begin
  Result := TcxGridColumnAccess(AInstance).DoCompareValuesForCellMerging(ARow1, AProperties1, AValue1, ARow2, AProperties2, AValue2);
end;

{ GridMasterDataRow Helpers }

procedure GridMasterDataRow_DoExpand(AInstance: TcxGridMasterDataRow; ARecurse: Boolean);
begin
  TcxGridMasterDataRowAccess(AInstance).DoExpand(ARecurse);
end;

{ GridTableViewInfo Helpers }

procedure GridTableViewInfo_CalculateExpandButtonParams(AInstance: TcxGridTableViewInfo);
begin
  TcxGridTableViewInfoAccess(AInstance).CalculateExpandButtonParams;
end;

{ Utilities }

function GridLinkStyleSheetPrototype: TdxGridReportLinkStyleSheet;

  function CreateStyle(AColor, AFontColor: TColor): TcxStyle;
  begin
    Result := TcxStyle.Create(FGridLinkStyleSheetPrototype);
    with Result do
    begin
      Color := AColor;
      Font.Name := dxPSCore.dxPSDefaultFontName;
      Font.Color := AFontColor;
    end;
  end;

begin
  if FGridLinkStyleSheetPrototype = nil then
  begin
    FGridLinkStyleSheetPrototype := TdxGridReportLinkStyleSheet.Create(nil);
    with FGridLinkStyleSheetPrototype.Styles as TdxGridReportLinkStyles do
    begin
      BandHeader := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Caption := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      CardCaptionRow := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      CardRowCaption := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Content := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      ContentEven := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      ContentOdd := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      FilterBar := CreateStyle(clBtnShadow, dxPSCore.dxDefaultContentColor);
      Footer := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Group := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Header := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Preview := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      Selection := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
    end;
  end;
  Result := FGridLinkStyleSheetPrototype;
end;

function CreatePSDataCellPos: TdxPSGridDataCellPos;
begin
  Result := TdxPSGridDataCellPos.Create;
end;

procedure FreePSDataCellPos(out ADataCellPos: TdxPSGridDataCellPos);
begin
  FreeAndNil(ADataCellPos);
end;

procedure _LoadAllRecords(ADBDataModeController: TcxDBDataModeController;
  out APrevAllRecordsAreLoaded: Boolean);
begin
  APrevAllRecordsAreLoaded := not ADBDataModeController.GridMode;
  if not APrevAllRecordsAreLoaded then
    ADBDataModeController.GridMode := False;
end;

procedure _UnloadAllRecords(ADBDataModeController: TcxDBDataModeController;
  const APrevAllRecordsAreLoaded: Boolean);
begin
  if not APrevAllRecordsAreLoaded then
    ADBDataModeController.GridMode := True;
end;

{ TdxGridViewBuildersFactory }

function dxGridViewBuildersFactory: TdxGridViewBuildersFactory;
begin
  Result := TdxGridViewBuildersFactory.Instance;
end;

class function TdxGridViewBuildersFactory.Instance: TdxGridViewBuildersFactory;
begin
  Result := inherited Instance as TdxGridViewBuildersFactory;
end;

function TdxGridViewBuildersFactory.CreateViewAdapter(AMasterAdapter: TdxCustomGridViewAdapter;
  AGridView: TcxCustomGridView): TdxCustomGridViewAdapter;
begin
  Result := BuilderClasses[AGridView].CreateAdapter(AMasterAdapter, AGridView);
end;

function TdxGridViewBuildersFactory.CreateViewBuilder(AReportLink: TdxGridReportLink;
  AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView): TdxCustomGridViewBuilder;
begin
  Result := BuilderClasses[AGridView].Create(AReportLink, AMasterBuilder, AGridView);
end;

function TdxGridViewBuildersFactory.GetBuilderClass(GridView: TcxCustomGridView): TdxGridViewBuilderClass;
begin
  Result := TdxGridViewBuilderClass(PairClasses[GridView.ClassType]);
end;

{ TdxViewHelpersFactory }

function dxViewHelpersFactory: TdxViewHelpersFactory;
begin
  Result := TdxViewHelpersFactory.Instance;
end;

class function TdxViewHelpersFactory.Instance: TdxViewHelpersFactory;
begin
  Result := inherited Instance as TdxViewHelpersFactory;
end;

function TdxViewHelpersFactory.HelperClassByItem(AnItem: TdxReportVisualItem): TdxCustomGridViewHelperClass;
var
  AView: TcxCustomGridView;
begin
  AView := TcxCustomGridView(AnItem.TopLevelParent.Data);
  if AView = nil then
    AView := (AnItem.Creator as TdxGridReportLink).Grid.ActiveView;
  Result := HelperClasses[AView];
end;

function TdxViewHelpersFactory.GetHelperClass(GridView: TcxCustomGridView): TdxCustomGridViewHelperClass;
begin
  if GridView = nil then
    Result := nil
  else
    Result := TdxCustomGridViewHelperClass(PairClasses[GridView.ClassType]);
end;

{ TdxRecordHelpersFactory }

function dxRecordHelpersFactory: TdxRecordHelpersFactory;
begin
  Result := TdxRecordHelpersFactory.Instance;
end;

class function TdxRecordHelpersFactory.Instance: TdxRecordHelpersFactory;
begin
  Result := inherited Instance as TdxRecordHelpersFactory;
end;

function TdxRecordHelpersFactory.GetHelperClass(GridRecord: TcxCustomGridRecord): TdxCustomGridRecordHelperClass;
begin
  Result := TdxCustomGridRecordHelperClass(PairClasses[GridRecord.ClassType]);
end;

{ CLR Accesssors }



{ TdxCustomGridViewHelper }

constructor TdxCustomGridViewHelper.Create(AView: TcxCustomGridView);
begin
  inherited Create;
  FView := AView;
end;

class function TdxCustomGridViewHelper.PairClass: TClass;
begin
  Result := ViewClass;
end;

class procedure TdxCustomGridViewHelper.Register;
begin
  dxViewHelpersFactory.Register(Self);
end;

class procedure TdxCustomGridViewHelper.Unregister;
begin
  dxViewHelpersFactory.Unregister(Self);
end;

class function TdxCustomGridViewHelper.ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID;
begin
  Result := dxGridUndefinedID;
  if TClass(AnItem.Data) = TdxGridLevelCaption then
    Result := dxGridLevelCaptionID;
end;

class procedure TdxCustomGridViewHelper.ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ACustomDrawInfo: TdxGridCellCustomDrawInfo);
begin
  FillChar(ACustomDrawInfo, SizeOf(ACustomDrawInfo), 0);
  with ACustomDrawInfo do
  begin
    GridAttributeID := ExtractCustomDrawID(AnItem);
    GridView := ExtractGridView(AnItem);
  end;
end;

class function TdxCustomGridViewHelper.ExtractGridView(AnItem: TdxReportVisualItem): TcxCustomGridView;
begin
  Result := TcxCustomGridView(AnItem.TopLevelParent.Data);
end;

class function TdxCustomGridViewHelper.HasData(AView: TcxCustomGridView): Boolean;
begin
  Result := True;
end;

class function TdxCustomGridViewHelper.IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean;
var
  Attributes: TdxGridAttributeClasses;
  I: Integer;
begin
  Attributes := SupportedAttributes;

  Result := True;
  for I := Low(Attributes) to High(Attributes) do
    if Attributes[I] = AnAttribute then Exit;
  Result := False;
end;

class function TdxCustomGridViewHelper.SupportedAttributes: TdxGridAttributeClasses;
begin
  SetLength(Result, 0);
  AddAttribute(Result, TdxGridLevelCaption);
end;

class procedure TdxCustomGridViewHelper.AddAttribute(var AnAtributes: TdxGridAttributeClasses;
  AnAttribute: TdxGridAttributeClass);
begin
  SetLength(AnAtributes, Length(AnAtributes) + 1);
  AnAtributes[Length(AnAtributes) - 1] := AnAttribute;
end;

class function TdxCustomGridViewHelper.FilterPosition(AView: TcxCustomGridView): TcxGridFilterPosition;
begin
  Result := fpBottom;
end;

class function TdxCustomGridViewHelper.IsFilterBarAtBottom(AView: TcxCustomGridView): Boolean;
begin
  Result := FilterPosition(AView) = fpBottom;
end;

class function TdxCustomGridViewHelper.IsFilterBarAtTop(AView: TcxCustomGridView): Boolean;
begin
  Result := FilterPosition(AView) = fpTop;
end;

class function TdxCustomGridViewHelper.IsOffice11StyleGrouping(AView: TcxCustomGridView): Boolean;
begin
  Result := False;
end;

class function TdxCustomGridViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridView;
end;

{ TdxNullGridViewHelper }

class function TdxNullGridViewHelper.IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  Result := True;
end;

class function TdxNullGridViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := nil;
end;

{  TdxCustomGridTableViewHelper }

class function TdxCustomGridTableViewHelper.ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID;
begin
  Result := inherited ExtractCustomDrawID(AnItem);
  if Result = dxGridUndefinedID then
    if TClass(AnItem.Data) = TdxGridFilterBar then
      Result := dxGridFilterBarID;
end;

class function TdxCustomGridTableViewHelper.HasData(AView: TcxCustomGridView): Boolean;
begin
  Result := (AView <> nil) and (TcxCustomGridTableView(AView).ViewData.RecordCount <> 0);
end;

class function TdxCustomGridTableViewHelper.SupportedAttributes: TdxGridAttributeClasses;
begin
  Result := inherited SupportedAttributes;
  AddAttribute(Result, TdxGridFilterBar);
end;

class function TdxCustomGridTableViewHelper.ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord;
begin
  Result := nil;
end;

class function TdxCustomGridTableViewHelper.FilterPosition(AView: TcxCustomGridView): TcxGridFilterPosition;
begin
  Result := CustomGridTableFiltering_GetPosition(TcxCustomGridTableView(AView).Filtering);
end;

class function TdxCustomGridTableViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridTableView;
end;

{ TdxGridCardViewHelper }

class function TdxGridCardViewHelper.ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID;
begin
  Result := inherited ExtractCustomDrawID(AnItem);
  if Result = dxGridUndefinedID then
    if (AnItem.Parent <> nil) and (AnItem.Parent is TdxReportCardRow) then
      if TClass(AnItem.Data) = TdxGridCardRowCaption then
        Result := dxGridCardRowCaptionID
      else
        Result := dxGridCardRowDataID;
end;

class procedure TdxGridCardViewHelper.ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ACustomDrawInfo: TdxGridCellCustomDrawInfo);
begin
  with ACustomDrawInfo do
  try
    inherited;
    if GridAttributeID in [dxGridCardRowCaptionID, dxGridCardRowDataID] then
    begin
      GridCard := ExtractCard(AnItem);
      GridCardRow := ExtractCardRow(AnItem);
    end;
  except
    FillChar(ACustomDrawInfo, SizeOf(ACustomDrawInfo), 0);
    GridAttributeID := dxGridUndefinedID;
  end;
end;

class function TdxGridCardViewHelper.ExtractCard(AnItem: TdxReportVisualItem): TcxGridCard;
begin
  Result := ExtractRecord(AnItem) as TcxGridCard;
end;

class function TdxGridCardViewHelper.ExtractCardRow(AnItem: TdxReportVisualItem): TcxGridCardViewRow;
begin
  Result := TdxReportCardRow(AnItem.Parent).GridCardRow;
end;

class function TdxGridCardViewHelper.ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord;
begin
  Result := TdxReportCardRow(AnItem.Parent).Card.GridCard;
end;

class function TdxGridCardViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

{ TdxGridTableViewHelper }

class function TdxGridTableViewHelper.ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID;
begin
  Result := inherited ExtractCustomDrawID(AnItem);
  if Result = dxGridUndefinedID then
    if AnItem.Parent <> nil then
      if TClass(AnItem.Parent.Data) = TdxGridHeader then
        Result := dxGridHeaderID
      else
        if TClass(AnItem.Parent.Data) = TdxGridFooter then
          Result := dxGridFooterID
        else
          if IsDelphiObject(AnItem.Data) and (TObject(AnItem.Data) is TcxCustomGridTableItem) then
            if (AnItem.Parent.Parent <> nil) and (AnItem.Parent.Parent.Data >= 0) and
              (AnItem.Parent.Parent.Data < High(Byte)) then
              Result := dxGridGroupFooterID
            else
              Result := dxGridRecordID;
end;

class procedure TdxGridTableViewHelper.ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ACustomDrawInfo: TdxGridCellCustomDrawInfo);
begin
  with ACustomDrawInfo do
  try
    inherited;

    if GridAttributeID in [dxGridFooterID, dxGridGroupFooterID, dxGridHeaderID, dxGridRecordID] then
    begin
      GridColumn := ExtractColumn(AnItem);
      if GridAttributeID in [dxGridRecordID, dxGridGroupFooterID] then
      begin
        GridRecord := ExtractRecord(AnItem);
        if GridAttributeID = dxGridGroupFooterID then
          GroupLevel := ExtractGroupLevel(AnItem);
      end
      else
        GroupLevel := -1;
    end;
  except
    FillChar(ACustomDrawInfo, SizeOf(ACustomDrawInfo), 0);
    GridAttributeID := dxGridUndefinedID;
  end;
end;

class function TdxGridTableViewHelper.SupportedAttributes: TdxGridAttributeClasses;
begin
  Result := inherited SupportedAttributes;
  AddAttribute(Result, TdxGridDetails);
  AddAttribute(Result, TdxGridExpandButton);
  AddAttribute(Result, TdxGridFooter);
  AddAttribute(Result, TdxGridGroupFooter);
  AddAttribute(Result, TdxGridGroupRow);
  AddAttribute(Result, TdxGridHeader);
  AddAttribute(Result, TdxGridPreview);
end;

class function TdxGridTableViewHelper.ExtractColumn(AnItem: TdxReportVisualItem): TcxGridColumn;
begin
  Result := TcxGridColumn(AnItem.Data);
end;

class function TdxGridTableViewHelper.ExtractGroupLevel(AnItem: TdxReportVisualItem): Integer;
begin
  Result := Integer(AnItem.Parent.Parent.Data);
end;

class function TdxGridTableViewHelper.ExtractRecord(AnItem: TdxReportVisualItem): TcxCustomGridRecord;
begin
  Result := TcxCustomGridRecord(AnItem.Parent.Data);
end;

class function TdxGridTableViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridTableView;
end;

class function TdxGridTableViewHelper.IsOffice11StyleGrouping(AView: TcxCustomGridView): Boolean;
begin
  Result := TcxGridTableView(AView).OptionsView.GroupRowStyle = grsOffice11;
end;

{ TdxGridBandedTableViewHelper }

class function TdxGridBandedTableViewHelper.ExtractCustomDrawID(AnItem: TdxReportVisualItem): TdxGridAttributeID;
begin
  Result := inherited ExtractCustomDrawID(AnItem);
  if Result = dxGridUndefinedID then
    if IsDelphiObject(AnItem.Data) and (TObject(AnItem.Data) is TcxGridBand) then
      Result := dxGridBandID;
end;

class procedure TdxGridBandedTableViewHelper.ExtractCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ACustomDrawInfo: TdxGridCellCustomDrawInfo);
begin
  with ACustomDrawInfo do
  try
    inherited;
    if GridAttributeID = dxGridBandID then
      GridBand := ExtractBand(AnItem);
  except
    FillChar(ACustomDrawInfo, SizeOf(ACustomDrawInfo), 0);
    GridAttributeID := dxGridUndefinedID;
  end;
end;

class function TdxGridBandedTableViewHelper.SupportedAttributes: TdxGridAttributeClasses;
begin
  Result := inherited SupportedAttributes;
  AddAttribute(Result, TdxGridBandHeader);
end;

class function TdxGridBandedTableViewHelper.ExtractBand(AnItem: TdxReportVisualItem): TcxGridBand;
begin
  Result := TcxGridBand(AnItem.Data);
end;

class function TdxGridBandedTableViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

class function TdxGridChartViewHelper.ViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridChartView;
end;

class function TdxGridChartViewHelper.HasData(AView: TcxCustomGridView): Boolean;
begin
  Result := TcxGridChartView(AView).VisibleSeriesCount <> 0;
end;

{ TdxCustomGridRecordHelper }

constructor TdxCustomGridRecordHelper.Create(AAdapter: TdxCustomGridTableViewAdapter);
begin
  inherited Create;
  FAdapter := AAdapter;
end;

class function TdxCustomGridRecordHelper.PairClass: TClass;
begin
  Result := RecordClass;
end;

class procedure TdxCustomGridRecordHelper.Register;
begin
  dxRecordHelpersFactory.Register(Self);
end;

class procedure TdxCustomGridRecordHelper.Unregister;
begin
  dxRecordHelpersFactory.Unregister(Self);
end;

function TdxCustomGridRecordHelper.Adapter: TdxCustomGridTableViewAdapter;
begin
  Result := FAdapter;
end;

class function TdxCustomGridRecordHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridViewRowProducer;
end;

class function TdxCustomGridRecordHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxCustomGridRecord;
end;


function TdxCustomGridRecordHelper.GetCanCellMergingAsMaster: Boolean;
begin
  Result := True;
end;

function TdxCustomGridRecordHelper.GetCanCellMergingAsSlave: Boolean;
begin
  Result := True;
end;


function TdxCustomGridRecordHelper.GetHasDetails: Boolean;
begin
  Result := False;
end;


function TdxCustomGridRecordHelper.GetHasExpandButton: Boolean;
begin
  Result := False;
end;


function TdxCustomGridRecordHelper.GetHasSelectedChildren: Boolean;
begin
  Result := False;
end;


function TdxCustomGridRecordHelper.GetIsCellMergingSeparator: Boolean;
begin
  Result := False;
end;


function TdxCustomGridRecordHelper.GetParent: TcxCustomGridRecord;
var
  I: Integer;
begin
  for I := GridRecord.Index - 1 downto 0 do
  begin
    Result := Adapter.Records[I];
    if CustomGridRecord_GetIsParent(Result) and (Result.Level < GridRecord.Level) then
      Exit;
  end;
  Result := nil;
end;

{ TdxCustomGridRowHelper }

function TdxCustomGridRowHelper.Adapter: TdxGridTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridTableViewAdapter;
end;

class function TdxCustomGridRowHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridTableViewCustomDataRowProducer;
end;

class function TdxCustomGridRowHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxCustomGridRow;
end;

{ TdxGridDataRowHelper }

class function TdxGridDataRowHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridTableViewDataRowProducer;
end;

class function TdxGridDataRowHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridDataRow;
end;

function TdxGridDataRowHelper.GetRow: TcxGridDataRow;
begin
  Result := inherited GridRecord as TcxGridDataRow;
end;

{ TdxGridGroupRowHelper }

class function TdxGridGroupRowHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridTableViewGroupRowProducer;
end;

class function TdxGridGroupRowHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridGroupRow;
end;


function TdxGridGroupRowHelper.GetCanCellMergingAsMaster: Boolean;
begin
  Result := False;
end;

function TdxGridGroupRowHelper.GetCanCellMergingAsSlave: Boolean;
begin
  Result := False;
end;

function TdxGridGroupRowHelper.GetHasExpandButton: Boolean;
begin
  Result := not (dcoGroupsAlwaysExpanded in Adapter.DataController.Options);
end;


function TdxGridGroupRowHelper.GetHasSelectedChildren: Boolean;
var
  GroupLevel, I: Integer;
  Row: TcxCustomGridRow;
begin
  GroupLevel := GroupRow.Level;
  for I := GroupRow.Index + 1 to Adapter.RowCount - 1 do
  begin
    Row := Adapter.Rows[I];
    if Row.Level <= GroupLevel then Break;
    Result := Row.Selected or Adapter.RecordHelpers[Row].HasSelectedChildren; //!!!
    if Result then Exit;
  end;
  Result := False;
end;

function TdxGridGroupRowHelper.GetGroupRow: TcxGridGroupRow;
begin
  Result := inherited GridRecord as TcxGridGroupRow;
end;

{ TdxGridMasterDataRowHelper }

class function TdxGridMasterDataRowHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridTableViewMasterRowProducer;
end;

class function TdxGridMasterDataRowHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridMasterDataRow;
end;


function TdxGridMasterDataRowHelper.GetCanCellMergingAsMaster: Boolean;
begin
  Result := not MasterRow.Expanded;
end;


function TdxGridMasterDataRowHelper.GetHasDetails: Boolean;
begin
  Result := MasterRow.Expanded;// and MasterRow.ActiveDetailGridViewExists //!!!
end;


function TdxGridMasterDataRowHelper.GetHasExpandButton: Boolean;
begin
  Result := True;
end;


function TdxGridMasterDataRowHelper.GetHasSelectedChildren: Boolean;
begin
  Result := MasterRow.Expanded;
  if Result then
    with dxGridViewBuildersFactory.CreateViewAdapter(Adapter, DetailView) do
    try
      Result := HasSelectedRecords;
    finally
      Free;
    end;
end;


function TdxGridMasterDataRowHelper.GetIsCellMergingSeparator: Boolean;
begin
  Result := MasterRow.Expanded;
end;


function TdxGridMasterDataRowHelper.GetDetailView: TcxCustomGridView;
begin
  if HasDetails then
    Result := MasterRow.ActiveDetailGridView
  else
    Result := nil;
end;

function TdxGridMasterDataRowHelper.GetMasterRow: TcxGridMasterDataRow;
begin
  Result := inherited GridRecord as TcxGridMasterDataRow;
end;

{ TdxGridWinExplorerViewDataRecordHelper }

class function TdxGridWinExplorerViewDataRecordHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridWinExplorerViewDataRecordProducer;
end;

class function TdxGridWinExplorerViewDataRecordHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridWinExplorerViewDataRecord;
end;

{ TdxGridWinExplorerViewGroupRecordHelper }

class function TdxGridWinExplorerViewGroupRecordHelper.ProducerClass: TdxGridViewRowProducerClass;
begin
  Result := TdxGridWinExplorerViewGroupRecordProducer;
end;

class function TdxGridWinExplorerViewGroupRecordHelper.RecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridWinExplorerViewGroupRecord;
end;

{ TdxRecordHelpersCache }

constructor TdxRecordHelpersCache.Create(AAdapter: TdxCustomGridTableViewAdapter);
begin
  inherited Create;
  FAdapter := AAdapter;
end;

function TdxRecordHelpersCache.IndexOf(ARecord: TcxCustomGridRecord): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].RecordClass = ARecord.ClassType then
      Exit;

  Result := Add(dxRecordHelpersFactory.HelperClasses[ARecord].Create(Adapter));
end;

function TdxRecordHelpersCache.GetHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRecordHelper;
begin
  Result := Items[IndexOf(ARecord)];
  Result.GridRecord := ARecord;
end;

function TdxRecordHelpersCache.GetItem(Index: Integer): TdxCustomGridRecordHelper;
begin
  Result := TdxCustomGridRecordHelper(inherited Items[Index]);
end;

{ TdxProducerCache }

constructor TdxProducerCache.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TdxProducerCache.IndexOf(AProducerClass: TdxGridViewRowProducerClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].ClassType = AProducerClass then Exit;

  Result := Add(AProducerClass.Create(Builder));
end;

function TdxProducerCache.GetProducer(ProducerClass: TdxGridViewRowProducerClass): TdxGridViewRowProducer;
begin
  Result := Items[IndexOf(ProducerClass)];
end;

function TdxProducerCache.GetItem(Index: Integer): TdxGridViewRowProducer;
begin
  Result := inherited Items[Index] as TdxGridViewRowProducer;
end;

{ TdxCustomGridViewAdapter }

constructor TdxCustomGridViewAdapter.Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView);
begin
  inherited Create;
  FMasterAdapter := AMasterAdapter;
  FGridView := AGridView;
  FGridView.SizeChanged(True, True); //!?
end;

function TdxCustomGridViewAdapter.GridView: TcxCustomGridView;
begin
  Result := FGridView;
end;

class function TdxCustomGridViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridView;
end;

function TdxCustomGridViewAdapter.Styles: TcxCustomGridStyles;
begin
  Result := CustomGridView_GetStyles(GridView);
end;

procedure TdxCustomGridViewAdapter.ExpandAllRows(AnOptionsExpanding: TdxGridReportLinkOptionsExpanding;
  ARecursive: Boolean);
begin
  Grid.BeginUpdate;
  try
    ExpandAllRowsInLevel(GridLevel, AnOptionsExpanding.ExpandMasterRows,
      AnOptionsExpanding.ExpandGroupRows, ARecursive);
  finally
    Grid.EndUpdate;
  end;
end;

procedure TdxCustomGridViewAdapter.AfterBuilding;
begin
  UnloadAllRecords;
end;

procedure TdxCustomGridViewAdapter.BeforeBuilding;
begin
  LoadAllRecords;
end;

function TdxCustomGridViewAdapter.BackgroundBitmaps: TcxCustomGridBackgroundBitmaps;
begin
  Result := CustomGridView_GetBackgroundBitmaps(GridView);
end;

function TdxCustomGridViewAdapter.GetBackgroundBitmap(Index: Integer): TGraphic;
begin
  if IsBackgroundBitmapSupported(Index) then
    Result := BackgroundBitmaps.GetBitmap(Index)
  else
    Result := nil;
end;

function TdxCustomGridViewAdapter.HasBackgroundBitmap(Index: Integer): Boolean;
begin
  Result := (BackgroundBitmaps.GetBitmap(Index) <> nil) and not BackgroundBitmaps.GetBitmap(Index).Empty;
end;

function TdxCustomGridViewAdapter.IsBackgroundBitmapSupported(Index: Integer): Boolean;
begin
  Result := Index <> bbTabs;
end;

function TdxCustomGridViewAdapter.Controller: TcxCustomGridController;
begin
  Result := GridView.Controller;
end;

function TdxCustomGridViewAdapter.DataController: TcxCustomDataController;
begin
  Result := CustomGridView_GetDataController(GridView);
end;

function TdxCustomGridViewAdapter.CreateCloneAdapter(AClone: TcxCustomGridView): TdxCustomGridViewAdapter;
begin
  Result := dxGridViewBuildersFactory.CreateViewAdapter(MasterAdapter, AClone);
end;

procedure TdxCustomGridViewAdapter.ExpandAllRowsInGridView(AGridView: TcxCustomGridView;
  AMasterRows, AGroupRows: Boolean);
begin
  if AGridView = nil then Exit;
  with dxGridViewBuildersFactory.CreateViewAdapter(nil, AGridView) do
  try
    ExpandAllRowsInItself(AMasterRows, AGroupRows);
    ExpandAllRowsInClones(AMasterRows, AGroupRows);
  finally
    Free;
  end;
end;

procedure TdxCustomGridViewAdapter.ExpandAllRowsInClones(AMasterRows, AGroupRows: Boolean);
var
  I: Integer;
  Clone: TcxCustomGridView;
begin
  for I := 0 to GridView.CloneCount - 1 do
  begin
    Clone := GridView.Clones[I];
    with CreateCloneAdapter(Clone) do
    try
      ExpandAllRowsInGridView(Clone, AMasterRows, AGroupRows);
    finally
      Free;
    end;
  end;
end;

procedure TdxCustomGridViewAdapter.ExpandAllRowsInLevel(ALevel: TcxGridLevel;
  AMasterRows, AGroupRows: Boolean; ARecursive: Boolean);
var
  I: Integer;
begin
  ExpandAllRowsInGridView(ALevel.GridView, AMasterRows, AGroupRows);
  if ARecursive then
    for I := 0 to ALevel.VisibleCount - 1 do
      ExpandAllRowsInLevel(ALevel.VisibleItems[I], AMasterRows, AGroupRows, ARecursive);
end;

procedure TdxCustomGridViewAdapter.ExpandAllRowsInItself(AMasterRows, AGroupRows: Boolean);
begin
end;

function TdxCustomGridViewAdapter.GetAreAllMasterRowsCollapsed: Boolean;
begin
  Result := True;
end;

function TdxCustomGridViewAdapter.GetAutoWidth: Boolean;
begin
  Result := False;
end;

function TdxCustomGridViewAdapter.GetCanUseOnEveryPageMode: Boolean;
begin
  Result := False;
end;

function TdxCustomGridViewAdapter.GetFilterPosition: TcxGridFilterPosition;
begin
  Result := fpBottom;
end;

function TdxCustomGridViewAdapter.GetHasSelectedRecords: Boolean;
begin
  Result := False;
end;

function TdxCustomGridViewAdapter.GetIndentCount: Integer;
begin
  Result := Ord(IsMaster);
end;

function TdxCustomGridViewAdapter.GetIndentWidth: Integer;
begin
  Result := ScaleFactor.Apply(DefaultIndentWidth);
end;

function TdxCustomGridViewAdapter.GetIsOffice11StyleGrouping: Boolean;
begin
  Result := False;
end;

function TdxCustomGridViewAdapter.GetMasterGridRecord: TcxCustomGridRecord;
begin
  Result := nil;
end;

function TdxCustomGridViewAdapter.GetRecordCount: Integer;
begin
  Result := 0;
end;

function TdxCustomGridViewAdapter.GetViewWidthExtra: Integer;
begin
  Result := Ord(IsMaster) * IndentWidth;
end;

function TdxCustomGridViewAdapter.GetCaptionViewParams: TcxViewParams;
var
  Level: TcxGridLevel;
begin
  if MasterAdapter = nil then
    if GridView.MasterGridView <> nil then
    begin
      Level := GridView.MasterGridView.Level as TcxGridLevel;
      Level.Styles.GetTabParams( MasterGridRecord,  GridLevel, Result);
    end
    else
      Result := GetRootCaptionParams
  else
    MasterAdapter.GridLevel.Styles.GetTabParams( MasterGridRecord,  GridLevel, Result);
end;

function TdxCustomGridViewAdapter.GetFilterBarViewParams: TcxViewParams;
begin
  Result.Color := clBtnShadow;
  Result.Font := nil;
  Result.TextColor := clWindow;
end;

function TdxCustomGridViewAdapter.GetRootCaptionParams: TcxViewParams;
begin
  Grid.RootLevelStyles.GetTabParams( nil,  GridLevel, Result);
end;

function TdxCustomGridViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := nil;
end;

function TdxCustomGridViewAdapter.IsBoundView: Boolean;
begin
  Result := DBDataModeController <> nil;
end;

procedure TdxCustomGridViewAdapter.LoadAllRecords;
begin
  if IsBoundView then
    _LoadAllRecords(DBDataModeController, FPrevAllRecordsAreLoaded);
end;

procedure TdxCustomGridViewAdapter.UnloadAllRecords;
begin
  if IsBoundView then
    _UnloadAllRecords(DBDataModeController, FPrevAllRecordsAreLoaded);
end;

function TdxCustomGridViewAdapter.GetAbsoluteLevel: Integer;
begin
  Result := GridLevel.Level;
end;

function TdxCustomGridViewAdapter.GetCanUseLookAndFeelColors: Boolean;
begin
  Result := Grid.LookAndFeel.NativeStyle and dxThemeManager.AreVisualStylesAvailable;
end;

function TdxCustomGridViewAdapter.GetCaptionText: string;
begin
  Result := GridLevel.DisplayCaption;
end;

function TdxCustomGridViewAdapter.GetDetailsSeparatorColor: TColor;
begin
  Result := MasterGridLevel.Options.DetailFrameColor;
end;

function TdxCustomGridViewAdapter.GetDetailsSeparatorThickness: Integer;
begin
  Result := MasterGridLevel.Options.DetailFrameWidth;
end;

function TdxCustomGridViewAdapter.GetExpandButtonColor: TColor;
begin
  Result := ExpandButtonInteriorColor;
end;

function TdxCustomGridViewAdapter.GetExpandButtonSize: Integer;
begin
  Result := GridView.ViewInfo.LookAndFeelPainter.ScaledExpandButtonSize(TcxCustomGridViewAccess(GridView).ScaleFactor);
end;

function TdxCustomGridViewAdapter.GetFilter: TcxDataFilterCriteria;
begin
  Result := DataController.Filter;
end;

function TdxCustomGridViewAdapter.GetFilterActive: Boolean;
begin
  Result := Filter.Active;
end;

function TdxCustomGridViewAdapter.GetFilterEmpty: Boolean;
begin
  Result := Filter.FilterText = '';
end;

function TdxCustomGridViewAdapter.GetFilterText: string;
begin
  Result := Filter.FilterCaption;
  if Result = '' then
    Result := cxGetResourceString(@scxGridFilterIsEmpty);
end;

function TdxCustomGridViewAdapter.GetGrid: TcxCustomGrid;
begin
  Result := GridView.Control as TcxCustomGrid;
end;

function TdxCustomGridViewAdapter.GetGridLevel: TcxGridLevel;
begin
  Result := GridView.Level as TcxGridLevel;
end;

function TdxCustomGridViewAdapter.GetGridWidth: Integer;
begin
  with Grid.ClientRect do
    Result := Right - Left;
end;

function TdxCustomGridViewAdapter.GetHasDetailsSeparator: Boolean;
begin
  Result := DetailsSeparatorThickness <> 0;
end;

function TdxCustomGridViewAdapter.GetLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := Grid.LookAndFeel.Kind;
end;

function TdxCustomGridViewAdapter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Grid.LookAndFeelPainter;
end;

function TdxCustomGridViewAdapter.GetIsDetail: Boolean;
begin
  Result := GridView.IsDetail;
end;

function TdxCustomGridViewAdapter.GetIsMaster: Boolean;
begin
  Result := GridView.IsMaster;
end;

function TdxCustomGridViewAdapter.GetMasterGridLevel: TcxGridLevel;
begin
  Result := GridLevel.Parent;
end;

function TdxCustomGridViewAdapter.GetScaleFactor: TdxScaleFactor;
begin
  Result := TcxCustomGridViewAccess(GridView).ScaleFactor;
end;

function TdxCustomGridViewAdapter.GetSummary: TcxDataSummary;
begin
  Result := DataController.Summary;
end;

function TdxCustomGridViewAdapter.GetThemedCaptionColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultTabColor;
end;

function TdxCustomGridViewAdapter.GetThemedCaptionTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultTabTextColor;
end;

function TdxCustomGridViewAdapter.GetThemedFilterBarColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFilterBoxColor;
end;

function TdxCustomGridViewAdapter.GetThemedFilterBarTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFilterBoxTextColor;
end;

function TdxCustomGridViewAdapter.GetViewWidth: Integer;
begin
  Result := GridView.Control.Width;
end;

{ TdxCustomGridViewFormatter }

constructor TdxCustomGridViewFormatter.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;

  FLookAndFeelItems := TList.Create;
  FExpandButtons := TList.Create;
  FFont := TFont.Create;

  FTransparentColor := dxPSCore.dxDefaultContentColor;
end;

destructor TdxCustomGridViewFormatter.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FExpandButtons);
  FreeAndNil(FLookAndFeelItems);
  inherited;
end;

function TdxCustomGridViewFormatter.Builder: TdxCustomGridViewBuilder;
begin
  Result := FBuilder;
end;

function TdxCustomGridViewFormatter.Adapter: TdxCustomGridViewAdapter;
begin
  Result := Builder.Adapter;
end;

procedure TdxCustomGridViewFormatter.DoInitializeHost(AHost: TdxReportCell);
begin
  AHost.CellSides := [];
  AHost.Data := TdxNativeInt(Adapter.GridView);
  AHost.Transparent := True;
end;

function TdxCustomGridViewFormatter.GetHostClass: TdxReportCellClass;
begin
  Result := TdxReportCell; {Host}
end;

function TdxCustomGridViewFormatter.GetRowHostClass: TdxReportCellClass;
begin
  Result := TdxReportCell;
end;

procedure TdxCustomGridViewFormatter.DoInitializeCaption(ACaption: TdxReportCellText);
begin
  SetViewParams(ACaption, GetCaptionViewParams);
  if HasBackgroundBitmap(bbTabs) then
    ACaption.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbTabs);
  RegisterLookAndFeelItem(ACaption, cesRaised);
  ACaption.Data := TdxNativeInt(TdxGridLevelCaption);
  ACaption.Text := Adapter.CaptionText;
  ACaption.HidePrefix := True;
end;

procedure TdxCustomGridViewFormatter.DoInitializeCaptionRow(ARow: TdxReportCell);
begin
end;

procedure TdxCustomGridViewFormatter.DoReportLinkInitializeCaption(ACaption: TdxReportCellText);
begin
  ReportLink.DoInitializeLevelCaption(Adapter.GridView, TdxReportCellString(ACaption));
end;

function TdxCustomGridViewFormatter.GetCaptionClass: TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxCustomGridViewFormatter.GetCaptionViewParams: TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetCaptionParams(Adapter.GridLevel, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetCaptionViewParams;

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
  begin
    Result.NativeParams.Color := Adapter.ThemedCaptionColor;
    Result.NativeParams.TextColor := Adapter.ThemedCaptionTextColor;
  end;
  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TdxCustomGridViewFormatter.DoInitializeFilterBar(AFilterBar: TdxReportCellText);
begin
  SetViewParams(AFilterBar, GetFilterBarViewParams);
  if HasBackgroundBitmap(bbFilterBox) then
    AFilterBar.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbFilterBox);

  with AFilterBar do
  begin
    Data := TdxNativeInt(TdxGridFilterBar);
    EndEllipsis := True;
    Text := Adapter.FilterText;
  end;

  if not Adapter.FilterEmpty then
    with TdxReportCellCheck(AFilterBar) do
    begin
      Checked := Adapter.FilterActive;
      CheckPos := ccpLeft;
    end;
end;

procedure TdxCustomGridViewFormatter.DoInitializeFilterBarRow(ARow: TdxReportCell);
begin
end;

procedure TdxCustomGridViewFormatter.DoReportLinkInitializeFilterBar(AFilterBar: TdxReportCellText);
begin
  ReportLink.DoInitializeFilterBar(TcxCustomGridTableView(Adapter.GridView), TdxReportCellString(AFilterBar));
end;

function TdxCustomGridViewFormatter.GetFilterBarClass: TdxReportCellTextClass;
const
  CheckClasses: array[Boolean] of TdxReportCellTextClass = (TdxReportCellCheck, TdxReportCellString);
begin
  Result := CheckClasses[Adapter.FilterEmpty];
end;

function TdxCustomGridViewFormatter.GetFilterBarViewParams: TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetFilterBarParams(Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetFilterBarViewParams;
  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
  begin
    Result.NativeParams.Color := Adapter.ThemedCaptionColor;
    Result.NativeParams.TextColor := Adapter.ThemedCaptionTextColor;
  end;
  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TdxCustomGridViewFormatter.DoInitializeDetailsSeparatorRow(ARow: TdxReportCell);
begin
end;

procedure TdxCustomGridViewFormatter.DoInitializeHorzDetailsSeparator(ASeparator: TdxReportCellBox);
begin
  with ASeparator do
  begin
    CellSides := csTopBottom;
    Color := DetailsSeparatorColor;
    Transparent := False;
  end;
end;

procedure TdxCustomGridViewFormatter.DoInitializeVertBottomDetailsSeparator(ASeparator: TdxReportCellBox;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  DoInitializeVertDetailsSeparator(ASeparator, ASeparatorKind);
  case ASeparatorKind of
    vdskLeading:
      ASeparator.CellSides := [csLeft, csBottom];
    vdskTrailing:
      ASeparator.CellSides := [csRight, csBottom];
  end;
end;

procedure TdxCustomGridViewFormatter.DoInitializeVertDetailsSeparator(ASeparator: TdxReportCellBox;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  with ASeparator do
  begin
    CellSides := [csLeft, csRight];
    Color := DetailsSeparatorColor;
    Transparent := False;
  end;
end;

procedure TdxCustomGridViewFormatter.DoInitializeVertTopDetailsSeparator(ASeparator: TdxReportCellBox;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  DoInitializeVertDetailsSeparator(ASeparator, ASeparatorKind);
  case ASeparatorKind of
    vdskLeading:
      ASeparator.CellSides := [csLeft, csTop];
    vdskTrailing:
      ASeparator.CellSides := [csRight, csTop];
  end;
end;

function TdxCustomGridViewFormatter.GetDetailsSeparatorClass: TdxReportCellBoxClass;
begin
  Result := TdxReportCellBox;
end;

procedure TdxCustomGridViewFormatter.DoInitializeExpandButton(
  AnExpandButton: TdxReportCellExpandButton; ARecord: TcxCustomGridRecord;
  AnIsMasterIndent: Boolean);
begin
  AnExpandButton.ButtonAlignHorz := bahCenter;
  if Adapter.IsOffice11StyleGrouping and not AnIsMasterIndent then {.2}
    AnExpandButton.ButtonAlignVert := bavBottom
  else
    AnExpandButton.ButtonAlignVert := bavCenter;

  AnExpandButton.ButtonExpanded := ARecord.Expanded;
  AnExpandButton.ButtonSize := Adapter.ExpandButtonSize;
  if (ReportLink.OptionsFormatting.LookAndFeelKind <> lfStandard) and not Odd(AnExpandButton.ButtonSize) then
    AnExpandButton.ButtonSize := AnExpandButton.ButtonSize - 1;
  AnExpandButton.ButtonInteriorColor := Adapter.ExpandButtonColor;
  AnExpandButton.ButtonTransparent := IsColorTransparent(AnExpandButton.ButtonInteriorColor);
  RegisterExpandButton(AnExpandButton);
end;

procedure TdxCustomGridViewFormatter.DoInitializeMasterIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer);
begin
end;

function TdxCustomGridViewFormatter.GetMasterIndentClass: TdxReportCellExpandButtonClass;
begin
  Result := TdxReportCellExpandButton;
end;

procedure TdxCustomGridViewFormatter.DoInitializeViewTerminator(ATerminator: TdxReportCellBox);
begin
  ATerminator.CellSides := [csBottom];
  ATerminator.Transparent := True;
end;

function TdxCustomGridViewFormatter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TdxCustomGridViewFormatter._AddRef: Integer;
begin
  Result := 1;
end;

function TdxCustomGridViewFormatter._Release: Integer;
begin
  Result := 1;
end;

procedure TdxCustomGridViewFormatter.AddDelimiters;
begin
  AddHorizontalDelimiters;
  AddVerticalDelimiters;
end;

procedure TdxCustomGridViewFormatter.AddHorizontalDelimiters;
begin
end;

procedure TdxCustomGridViewFormatter.AddVerticalDelimiters;
var
  I: Integer;
begin
  if ReportLink.OptionsPagination.Row then
    for I := 0 to Builder.ReportRowCount - 1 do
      ReportLink.AddVerticalDelimiter(Builder.ReportRows[I]);
end;

procedure TdxCustomGridViewFormatter.AfterBuilding;
begin
  Adapter.AfterBuilding;
  FormatLookAndFeelItems;
  FormatExpandButtons;
  AddDelimiters;
end;

procedure TdxCustomGridViewFormatter.BeforeBuilding;
begin
  Adapter.BeforeBuilding;
  Calculate;
end;

procedure TdxCustomGridViewFormatter.Calculate;
begin
  CalculateLineHeights;
end;

procedure TdxCustomGridViewFormatter.CalculateLineHeights;
begin
  CaptionLineHeight := 2 * ScaleFactor.Apply(TabsAreaOffset) + CalculateFontHeight(GetCaptionViewParams);
  FilterBarLineHeight := 2 * ScaleFactor.Apply(FilterBarTextOffset) + CalculateFontHeight(GetFilterBarViewParams);
end;

function TdxCustomGridViewFormatter.GetDetailsSeparatorColor: TColor;
begin
  Result := Adapter.DetailsSeparatorColor;
  {with ReportLink.OptionsFormatting do
    if UseNativeStyles then
    begin
      Result := FixedColor;
    end;}
end;

function TdxCustomGridViewFormatter.GetDetailsSeparatorThickness: Integer;
begin
  Result := Adapter.DetailsSeparatorThickness;
end;

function TdxCustomGridViewFormatter.GetHasDetailsSeparator: Boolean;
begin
  Result := Adapter.HasDetailsSeparator;
end;

function TdxCustomGridViewFormatter.GetSiteHeight: Integer;
var
  AIntf: IdxReportLinkController;
begin
  if ReportLink.IsAggregated and Supports(ReportLink.Controller, IdxReportLinkController, AIntf) then
    Result := cxRectHeight(AIntf.GetControlSiteBounds(ReportLink.Grid)) - 1
  else
    Result := cxRectHeight(ReportLink.RealPrinterPage.PaintRectPixels) - 1;
end;

function TdxCustomGridViewFormatter.GetSiteWidth: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if ReportLink.IsAggregated and Supports(TObject(ReportLink.Controller), IdxReportLinkController, ControllerIntf) then
    Result := cxRectWidth(ControllerIntf.GetControlSiteBounds(ReportLink.Grid)) - 1
  else
    Result := cxRectWidth(ReportLink.RealPrinterPage.PaintRectPixels) - 1;

  Result := MulDiv(Result, 100, ReportLink.GetRealScaleFactor);
end;

function TdxCustomGridViewFormatter.GetViewWidth: Integer;
begin
  Result := 0;
end;

function TdxCustomGridViewFormatter.GetViewWidthExtraAfter: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to AbsoluteLevel - 1 do
    with Builder.AbsoluteBuilders[I].Adapter do
      if HasDetailsSeparator then Inc(Result, DetailsSeparatorThickness);

  if Adapter.HasDetailsSeparator then
    Inc(Result, Adapter.DetailsSeparatorThickness);
end;

function TdxCustomGridViewFormatter.GetViewWidthExtraBefore: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AbsoluteLevel - 1 do
    with Builder.AbsoluteBuilders[I].Adapter do
    begin
      Inc(Result, ViewWidthExtra);
      if HasDetailsSeparator then Inc(Result, DetailsSeparatorThickness);
    end;

  if Adapter.HasDetailsSeparator then
    Inc(Result, Adapter.DetailsSeparatorThickness);
end;

function TdxCustomGridViewFormatter.BackgroundBitmaps: TcxCustomGridBackgroundBitmaps;
begin
  Result := Adapter.BackgroundBitmaps;
end;

function TdxCustomGridViewFormatter.GetBackgroundBitmap(Index: Integer): TGraphic;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    Result := ReportLink.Styles.GetBitmap(MapStyleBackgroundBitmapIndex(Index))
  else
    Result := Adapter.GetBackgroundBitmap(Index);
end;

function TdxCustomGridViewFormatter.GetBackgroundBitmapIndex(Index: Integer): Integer;
begin
  Result := ReportLink.AddBackgroundBitmapToPool(GetBackgroundBitmap(Index));
end;

function TdxCustomGridViewFormatter.HasBackgroundBitmap(Index: Integer): Boolean;
begin
  Result := not ReportLink.OptionsFormatting.SuppressBackgroundBitmaps and
    (GetBackgroundBitmap(Index) <> nil) and not GetBackgroundBitmap(Index).Empty;
end;

function TdxCustomGridViewFormatter.MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AGridBackgroundBitmapIndex of
    bbTabs:
      Result := vspsGridCaption;
    bbContent:
      Result := vspsGridContent;
    bbFilterBox:
      Result := vspsGridFilterBar;
  else
    Result := 0;
  end;
end;

function TdxCustomGridViewFormatter.CalculateFontHeight(const AParams: TdxReportItemViewParams): Integer;
begin
  Result := Renderer.CalcTextPatternHeight(Canvas, AParams.NativeParams.Font);
end;

procedure TdxCustomGridViewFormatter.CalculateHeight(
  const AParams: TdxReportItemViewParams; var AHeight: Integer);
begin
  AHeight := Max(AHeight, CalculateFontHeight(AParams));
end;

function TdxCustomGridViewFormatter.GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
begin
  if AParams.NativeParams.Font <> nil then
  begin
    FFont.Assign(AParams.NativeParams.Font);
    if not dxPSUtl.dxIsTrueTypeFont(FFont) then
      FFont.Name := ReportLink.Font.Name;
    FFont.Color := ColorToRGB(AParams.NativeParams.TextColor);
    FFont.Style := FFont.Style + AParams.FontStyle;
    Result := ReportLink.AddFontToPool(FFont);
  end
  else
    Result := 0;
end;

function TdxCustomGridViewFormatter.IsColorTransparent(AColor: TColor): Boolean;
begin
  Result := ColorToRGB(AColor) = ColorToRGB(TransparentColor);
end;

procedure TdxCustomGridViewFormatter.RegisterExpandButton(AExpandButton: TdxReportCellExpandButton);
begin
  FExpandButtons.Add(AExpandButton);
end;

procedure TdxCustomGridViewFormatter.RegisterLookAndFeelItem(AnItem: TdxReportVisualItem;
  AEdgeStyle: TdxCellEdgeStyle);
begin
  AnItem.EdgeMode := Self.EdgeMode;
  AnItem.Edge3DStyle := AEdgeStyle;
  FLookAndFeelItems.Add(AnItem);
end;

procedure TdxCustomGridViewFormatter.SetViewParams(AnItem: TdxReportVisualItem;
  AGridLines: TcxGridLines; const AParams: TcxViewParams);
var
  AReportItemParams: TdxReportItemViewParams;
begin
  AReportItemParams.NativeParams := AParams;
  AReportItemParams.CellSides := CellSidesMap[AGridLines] - [csLeft] + [csRight];
  AReportItemParams.FontStyle := [];
  AReportItemParams.Transparent := IsColorTransparent(AReportItemParams.NativeParams.Color);

  SetViewParams(AnItem, AReportItemParams);
end;

procedure TdxCustomGridViewFormatter.SetViewParams(AnItem: TdxReportVisualItem;
  const AParams: TdxReportItemViewParams);
begin
  AnItem.CellSides := AParams.CellSides;
  AnItem.Color := ColorToRGB(AParams.NativeParams.Color);
  AnItem.FontIndex := GetStyleFontIndex(AParams);
  AnItem.Transparent := AParams.Transparent;
end;

function TdxCustomGridViewFormatter.GetAbsoluteLevel: Integer;
begin
  Result := Adapter.AbsoluteLevel;
  if ReportLink.OptionsDetails.StartFromFocusedView then
    Dec(Result, (ReportLink.ActiveView.Level as TcxGridLevel).Level);
end;

function TdxCustomGridViewFormatter.GetAutoWidth: Boolean;
begin
  Result := ReportLink.OptionsSize.AutoWidth or Adapter.AutoWidth;
end;

function TdxCustomGridViewFormatter.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

function TdxCustomGridViewFormatter.GetEdgeMode: TdxCellEdgeMode;
const
  EdgeModeMap: array[Boolean] of TdxCellEdgeMode = (cemPattern, cem3DEffects);
begin
  Result := EdgeModeMap[ReportLink.OptionsFormatting.LookAndFeelKind <> lfUltraFlat];
end;

function TdxCustomGridViewFormatter.GetFilterBarPosition: TcxGridFilterPosition;
begin
  Result := Adapter.FilterPosition;
end;

function TdxCustomGridViewFormatter.GetGridWidth: Integer;
begin
  Result := Adapter.GridWidth;
end;

function TdxCustomGridViewFormatter.GetExpandButton(Index: Integer): TdxReportCellExpandButton;
begin
  Result := TdxReportCellExpandButton(FExpandButtons[Index]);
end;

function TdxCustomGridViewFormatter.GetExpandButtonCount: Integer;
begin
  Result := FExpandButtons.Count;
end;

function TdxCustomGridViewFormatter.GetIndentWidth: Integer;
begin
  Result := Adapter.IndentWidth;
end;

function TdxCustomGridViewFormatter.GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
begin
  Result := TdxReportVisualItem(FLookAndFeelItems[Index]);
end;

function TdxCustomGridViewFormatter.GetLookAndFeelItemCount: Integer;
begin
  Result := FLookAndFeelItems.Count;
end;

function TdxCustomGridViewFormatter.GetPaginateByTopLevelGroups: Boolean;
begin
  Result := ReportLink.OptionsPagination.TopLevelGroup;
end;

function TdxCustomGridViewFormatter.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportLink.Renderer;
end;

function TdxCustomGridViewFormatter.GetReportLink: TdxGridReportLink;
begin
  Result := Builder.ReportLink;
end;

function TdxCustomGridViewFormatter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Adapter.ScaleFactor;
end;

function TdxCustomGridViewFormatter.GetShowCaption: Boolean;
begin
  Result := ReportLink.OptionsView.Caption;
end;

function TdxCustomGridViewFormatter.GetShowFilterBar: Boolean;
begin
  Result := ReportLink.OptionsView.FilterBar;
end;

function TdxCustomGridViewFormatter.GetUseLookAndFeelColors: Boolean;
begin
  Result := ReportLink.OptionsFormatting.UseLookAndFeelColors;
end;

function TdxCustomGridViewFormatter.GetViewAvailableWidth: Integer;
begin
  if ReportLink.OptionsSize.AutoWidth then
    Result := SiteWidth
  else
    Result := Adapter.ViewWidth;
  Dec(Result, ViewWidthExtraBefore + ViewWidthExtraAfter);
end;

function TdxCustomGridViewFormatter.GetViewWidthExtra: Integer;
begin
  Result := Adapter.ViewWidthExtra;
end;

procedure TdxCustomGridViewFormatter.FormatLookAndFeelItems;
const
  Borders3D: array[TdxCellEdgeStyle, Boolean] of TdxPSCellBorderClass =
   ((TdxPSCellRaisedBorder, TdxPSCellRaisedSoftBorder),
    (TdxPSCellSunkenBorder, TdxPSCellSunkenSoftBorder));
var
  AItem: TdxReportVisualItem;
  I: Integer;
begin
  for I := 0 to LookAndFeelItemCount - 1 do
  begin
    AItem := LookAndFeelItems[I];
    if ReportLink.Effects3D then
      AItem.BorderClass := Borders3D[AItem.Edge3DStyle, ReportLink.Soft3D]
    else
      AItem.BorderClass := TdxPSCellUltraFlatBorder;
  end;
end;

procedure TdxCustomGridViewFormatter.FormatExpandButtons;
var
  I: Integer;
begin
  for I := 0 to ExpandButtonCount - 1 do
  begin
    ExpandButtons[I].ButtonBorder3D := ReportLink.Effects3D;
    ExpandButtons[I].ButtonBorder3DSoft := ReportLink.Soft3D;
  end;
end;

{ TdxCustomGridViewBuilder }

constructor TdxCustomGridViewBuilder.Create(AReportLink: TdxGridReportLink;
  AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView);

  function GetMasterAdapter: TdxCustomGridViewAdapter;
  begin
    if AMasterBuilder = nil then
      Result := nil
    else
      Result := AMasterBuilder.Adapter;
  end;

begin
  inherited Create;
  FReportLink := AReportLink;
  FMasterBuilder := AMasterBuilder;
  FGridView := AGridView;

  FAdapter := CreateAdapter(GetMasterAdapter, GridView);
  FFormatter := FormatterClass.Create(Self);
  FProducerCache := TdxProducerCache.Create(Self);
  FReportRows := TList.Create;

  if MasterBuilder <> nil then
    MasterBuilder.FDetailsBuilder := Self;
end;

destructor TdxCustomGridViewBuilder.Destroy;
begin
  if MasterBuilder <> nil then
    MasterBuilder.FDetailsBuilder := nil;
  FreeAndNil(FReportRows);
  FreeAndNil(FProducerCache);
  FreeAndNil(FFormatter);
  FreeAndNil(FAdapter);
  inherited;
end;

function TdxCustomGridViewBuilder.Adapter: TdxCustomGridViewAdapter;
begin
  Result := FAdapter;
end;

class function TdxCustomGridViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxCustomGridViewAdapter;
end;

class function TdxCustomGridViewBuilder.CreateAdapter(AMasterAdapter: TdxCustomGridViewAdapter;
  AGridView: TcxCustomGridView): TdxCustomGridViewAdapter;
begin
  Result := AdapterClass.Create(AMasterAdapter, AGridView);
end;

function TdxCustomGridViewBuilder.Formatter: TdxCustomGridViewFormatter;
begin
  Result := FFormatter;
end;

class function TdxCustomGridViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxCustomGridViewFormatter;
end;

procedure TdxCustomGridViewBuilder.Build;
begin
  BeforeBuilding;
  try
    DoBuild;
  finally
    AfterBuilding;
  end;
end;

function TdxCustomGridViewBuilder.IsAborted: Boolean;
begin
  Result := ReportLink.AbortBuilding;
end;

procedure TdxCustomGridViewBuilder.Progress(const APercentDone: Double);
begin
  ReportLink.DoProgress(APercentDone);
end;

class function TdxCustomGridViewBuilder.PairClass: TClass;
begin
  Result := GridViewClass;
end;

class procedure TdxCustomGridViewBuilder.Register;
begin
  dxGridViewBuildersFactory.Register(Self);
end;

class procedure TdxCustomGridViewBuilder.Unregister;
begin
  dxGridViewBuildersFactory.Unregister(Self);
end;

procedure TdxCustomGridViewBuilder.AddReportRow(ARow: TdxReportCell);
begin
  if ARow.Parent = ReportLink.ReportCells.Cells then
    FReportRows.Add(ARow);
end;

procedure TdxCustomGridViewBuilder.AfterBuilding;
begin
  if not IsAborted then Formatter.AfterBuilding;
end;

procedure TdxCustomGridViewBuilder.BeforeBuilding;
begin
  Formatter.BeforeBuilding;
end;

procedure TdxCustomGridViewBuilder.DoBuild;
begin
  DoBuildViewHeader;
  DoBuildViewBody;
  if not IsAborted then
    DoBuildViewFooter;
end;

procedure TdxCustomGridViewBuilder.DoBuildViewBody;
begin
end;

procedure TdxCustomGridViewBuilder.DoBuildViewFooter;
begin
end;

procedure TdxCustomGridViewBuilder.DoBuildViewHeader;
begin
end;

function TdxCustomGridViewBuilder.GetViewAvailableWidth: Integer;
begin
  Result := Formatter.ViewAvailableWidth;
end;

procedure TdxCustomGridViewBuilder.CreateCaption;
begin
  AddReportRow(GetCaptionProducer.Produce(HostInfoServices.CaptionHostInfo));
end;

function TdxCustomGridViewBuilder.GetCaptionProducer: TdxGridViewCaptionProducer;
begin
  Result := ProducerCache[GetCaptionProducerClass] as TdxGridViewCaptionProducer;
end;

function TdxCustomGridViewBuilder.GetCaptionProducerClass: TdxGridViewCaptionProducerClass;
begin
  Result := TdxGridViewCaptionProducer;
end;

procedure TdxCustomGridViewBuilder.CreateBottomDetailsSeparator;
begin
  AddReportRow(GetDetailsBottomSeparatorProducer.Produce(
    HostInfoServices.LevelSeparatorBottomHostInfo));
end;

procedure TdxCustomGridViewBuilder.CreateTopDetailsSeparator;
begin
  AddReportRow(GetDetailsTopSeparatorProducer.Produce(HostInfoServices.LevelSeparatorTopHostInfo));
end;

function TdxCustomGridViewBuilder.GetDetailsBottomSeparatorProducer: TdxGridViewDetailsSeparatorProducer;
begin
 Result := ProducerCache[GetDetailsBottomSeparatorProducerClass] as TdxGridViewDetailsSeparatorProducer;
end;

function TdxCustomGridViewBuilder.GetDetailsBottomSeparatorProducerClass: TdxGridViewDetailsSeparatorProducerClass;
begin
  Result := TdxGridViewDetailsBottomSeparatorProducer;
end;

function TdxCustomGridViewBuilder.GetDetailsTopSeparatorProducer: TdxGridViewDetailsSeparatorProducer;
begin
 Result := ProducerCache[GetDetailsTopSeparatorProducerClass] as TdxGridViewDetailsSeparatorProducer;
end;

function TdxCustomGridViewBuilder.GetDetailsTopSeparatorProducerClass: TdxGridViewDetailsSeparatorProducerClass;
begin
  Result := TdxGridViewDetailsTopSeparatorProducer;
end;

procedure TdxCustomGridViewBuilder.CreateFilterBar;
begin
  AddReportRow(GetFilterBarProducer.Produce(HostInfoServices.FilterBarHostInfo));
end;

function TdxCustomGridViewBuilder.GetFilterBarProducer: TdxGridViewFilterBarProducer;
begin
  Result := ProducerCache[GetFilterBarProducerClass] as TdxGridViewFilterBarProducer;
end;

function TdxCustomGridViewBuilder.GetFilterBarProducerClass: TdxGridViewFilterBarProducerClass;
begin
  Result := TdxGridViewFilterBarProducer;
end;

procedure TdxCustomGridViewBuilder.CreateViewTerminator;
begin
  AddReportRow(GetViewTerminatorProducer.Produce(HostInfoServices.PageDetailsHostInfo));
end;

function TdxCustomGridViewBuilder.GetViewTerminatorProducer: TdxGridViewTerminatorProducer;
begin
  Result := ProducerCache[GetViewTerminatorProducerClass] as TdxGridViewTerminatorProducer;
end;

function TdxCustomGridViewBuilder.GetViewTerminatorProducerClass: TdxGridViewTerminatorProducerClass;
begin
  Result := TdxGridViewTerminatorProducer;
end;

function TdxCustomGridViewBuilder.GridView: TcxCustomGridView;
begin
  Result := FGridView;
end;

class function TdxCustomGridViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridView;
end;

function TdxCustomGridViewBuilder.GetAbsoluteBuilder(AIndex: Integer): TdxCustomGridViewBuilder;
begin
  Result := RootBuilder;
  while AIndex > 0 do
  begin
    Result := Result.DetailsBuilder;
    Dec(AIndex);
  end;
end;

function TdxCustomGridViewBuilder.GetAutoWidth: Boolean;
begin
  Result := Formatter.AutoWidth;
end;

function TdxCustomGridViewBuilder.GetGridLevel: TcxGridLevel;
begin
  Result := GridView.Level as TcxGridLevel;
end;

function TdxCustomGridViewBuilder.GetGrid: TcxCustomGrid;
begin
  Result := ReportLink.Grid;
end;

function TdxCustomGridViewBuilder.GetHost: TdxReportCell;
begin
  Result := ReportLink.ReportCells.Cells;
end;

function TdxCustomGridViewBuilder.GetHostInfoServices: TdxGridAttributeHostInfoServices;
begin
  Result := ReportLink.HostInfoServices;
end;

function TdxCustomGridViewBuilder.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FReportRows[Index]);
end;

function TdxCustomGridViewBuilder.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TdxCustomGridViewBuilder.GetRootBuilder: TdxCustomGridViewBuilder;
begin
  Result := Self;
  while Result.MasterBuilder <> nil do
    Result := Result.MasterBuilder;
end;

{ TdxCustomGridTableViewAdapter }

constructor TdxCustomGridTableViewAdapter.Create(AMasterAdapter: TdxCustomGridViewAdapter; AGridView: TcxCustomGridView);
var
  AConditionalFormatting: IcxDataControllerConditionalFormattingProviderOwner;
begin
  inherited;
  FDetailsLineCount := -1;
  FRecordHelpersCache := TdxRecordHelpersCache.Create(Self);

  if Supports(GridView, IcxDataControllerConditionalFormattingProviderOwner, AConditionalFormatting) then
    FConditionalFormattingProvider := AConditionalFormatting.GetConditionalFormattingProvider;
end;

destructor TdxCustomGridTableViewAdapter.Destroy;
begin
  FreeAndNil(FRecordHelpersCache);
  inherited;
end;

function TdxCustomGridTableViewAdapter.ForEachRecord(
  AProc: TdxGridTableAdapterForEachRecordProc; AData: Integer; AProcessSelection: Boolean): Boolean;
var
  I: Integer;
  Continue: Boolean;
  Record_: TcxCustomGridRecord;
begin
  Result := True;
  if @AProc = nil then Exit;

  Continue := True;
  for I := 0 to RecordCount - 1 do
  begin
    Record_ := Records[I];
    if not AProcessSelection or Record_.Selected then
      AProc(Record_, AData, Continue);
    if not Continue then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TdxCustomGridTableViewAdapter.GridView: TcxCustomGridTableView;
begin
  Result := inherited GridView as TcxCustomGridTableView;
end;

class function TdxCustomGridTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridTableView;
end;

function TdxCustomGridTableViewAdapter.Styles: TcxCustomGridTableViewStyles;
begin
  Result := inherited Styles as TcxCustomGridTableViewStyles;
end;

function TdxCustomGridTableViewAdapter.CalculateDetailsLineCount: Integer;
begin
  Result := 1;
end;

function TdxCustomGridTableViewAdapter.GetContentViewParams(ARecord: TcxCustomGridRecord;
  ATableItem: TcxCustomGridTableItem; AIsDataCell: Boolean = False): TcxViewParams;
begin
  if (ATableItem <> nil) and (ATableItem.Styles <> nil) then
    ATableItem.Styles.GetContentParams(ARecord, Result)
  else
    Styles.GetContentParams(ARecord, ATableItem, Result);
  if Result.Color = clWindow then // 3.2
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TdxCustomGridTableViewAdapter.GetPreviewViewParams(ARecord: TcxCustomGridRecord;
  ATableItem: TcxCustomGridTableItem): TcxViewParams;
begin
  Result := GetContentViewParams(ARecord, ATableItem);
end;

function TdxCustomGridTableViewAdapter.GetSelectionViewParams: TcxViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Styles.Selection do
  begin
    Result.Bitmap := Bitmap;
    Result.Color := Color;
    Result.Font := Font;
    Result.TextColor := TextColor;
  end;
end;

function TdxCustomGridTableViewAdapter.HasSelectionStyle: Boolean;
begin
  Result := Styles.Selection <> nil;
end;

function TdxCustomGridTableViewAdapter.TryGetAdvancedStyle(
  const ACell: TPoint; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
begin
  Result := (ConditionalFormattingProvider <> nil) and
    TcxDataControllerConditionalFormattingProviderAccess(ConditionalFormattingProvider).TryGetStyle(ACell, AStyle);
end;

function TdxCustomGridTableViewAdapter.Controller: TcxCustomGridTableController;
begin
  Result := inherited Controller as TcxCustomGridTableController;
end;

procedure TdxCustomGridTableViewAdapter.DoExpandMasterRow(
  ARecord: TcxCustomGridRecord; AData: Integer; var AContinue: Boolean);
begin
  if (ARecord is TcxGridMasterDataRow) and ARecord.Expandable then
    GridMasterDataRow_DoExpand(TcxGridMasterDataRow(ARecord), True);
  {begin
    ARecord.Expanded := True;
    with TcxGridMasterDataRow(ARecord) do
      if ActiveDetailGridViewExists then
        ExpandAllRowsInGridView(ActiveDetailGridView, True, Boolean(AData));
  end;}
end;

procedure TdxCustomGridTableViewAdapter.ExpandAllGroupRows;
begin
  DataController.Groups.FullExpand;
end;

procedure TdxCustomGridTableViewAdapter.ExpandAllMasterRows(AnExpandGroups: Boolean);
begin
  ForEachRecord(DoExpandMasterRow, Integer(AnExpandGroups), False);
end;

procedure TdxCustomGridTableViewAdapter.ExpandAllRowsInItself(AMasterRows, AGroupRows: Boolean);
begin
  if AGroupRows or GridView.DataController.MultiSelectionSyncGroupWithChildren then
    ExpandAllGroupRows;
  if AMasterRows then
    ExpandAllMasterRows(AGroupRows);
end;

function TdxCustomGridTableViewAdapter.GetAreAllMasterRowsCollapsed: Boolean;
begin
  Result := ForEachRecord(IsMasterRowCollapsed, 0, False);
end;

function TdxCustomGridTableViewAdapter.GetCanUseOnEveryPageMode: Boolean;
begin
  Result := RecordCount <> 0;
end;

function TdxCustomGridTableViewAdapter.GetHasSelectedRecords: Boolean;
var
  I: Integer;
begin
  Result := SelectedRecordCount <> 0;
  if not Result then
    for I := 0 to RecordCount - 1 do
    begin
      Result := RecordHelpers[Records[I]].HasSelectedChildren;
      if Result then Break;
    end;
end;

function TdxCustomGridTableViewAdapter.GetMasterGridRecord: TcxCustomGridRecord;
begin
  Result := GridView.MasterGridRecord;
end;

function TdxCustomGridTableViewAdapter.GetRecordCount: Integer;
begin
  Result := GridView.ViewData.RecordCount;
end;

class function TdxCustomGridTableViewAdapter.GetProperties(AnItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord): TcxCustomEditProperties;
begin
  Result := AnItem.GetProperties(ARecord);
  if Result = nil then
    Result := GetRepositoryItem(AnItem, ARecord).Properties;
end;

class function TdxCustomGridTableViewAdapter.GetPropertiesClass(AnItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord): TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditPropertiesClass(GetProperties(AnItem, ARecord).ClassType);
end;

class function TdxCustomGridTableViewAdapter.GetRepositoryItem(AnItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord): TcxEditRepositoryItem;
begin
  Result := AnItem.GetRepositoryItem;
end;

procedure TdxCustomGridTableViewAdapter.IsMasterRowCollapsed(ARecord: TcxCustomGridRecord;
  AData: Integer; var AContinue: Boolean);
begin
  if ARecord is TcxGridMasterDataRow then
    AContinue := not ARecord.Expanded;
end;

function TdxCustomGridTableViewAdapter.GetCellAutoHeight: Boolean;
begin
  Result := CustomGridTableOptionsView_GetCellAutoHeight(GridView.OptionsView);
end;

function TdxCustomGridTableViewAdapter.GetCellMultiline: Boolean;
begin
  Result := CustomGridTableOptionsView_GetCellAutoHeight(GridView.OptionsView);
end;

function TdxCustomGridTableViewAdapter.GetFilterPosition: TcxGridFilterPosition;
begin
  Result := CustomGridTableFiltering_GetPosition(Filtering);
end;

function TdxCustomGridTableViewAdapter.GetGridLineColor: TColor;
begin
  Result := GridView.LookAndFeelPainter.DefaultGridLineColor;
end;

function TdxCustomGridTableViewAdapter.GetCellEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.CellEndEllipsis;
end;
function TdxCustomGridTableViewAdapter.GetDetailsLineCount: Integer;
begin
  if FDetailsLineCount = -1 then
    FDetailsLineCount := CalculateDetailsLineCount;
  Result := FDetailsLineCount;
end;

function TdxCustomGridTableViewAdapter.GetFiltering: TcxCustomGridTableFiltering;
begin
  Result := GridView.Filtering;
end;

function TdxCustomGridTableViewAdapter.GetIsGridMode: Boolean;
begin
  Result := DataController.IsGridMode;
end;

function TdxCustomGridTableViewAdapter.GetRecord(Index: Integer): TcxCustomGridRecord;
begin
  Result := GridView.ViewData.Records[Index];
end;

function TdxCustomGridTableViewAdapter.GetRecordHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRecordHelper;
begin
  Result := RecordHelpersCache.Helpers[ARecord];
end;

function TdxCustomGridTableViewAdapter.GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
begin
  Result := Controller.SelectedRecords[Index]
end;

function TdxCustomGridTableViewAdapter.GetSelectedRecordCount: Integer;
begin
  if CustomGridTableController_GetMultiSelect(Controller) then
    Result := Controller.SelectedRecordCount
  else
    Result := 0;
end;

{ TdxCustomGridTableViewFormatter }

constructor TdxCustomGridTableViewFormatter.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited;
  FRecordIndexes := TList.Create;
  FSelectedRecordList := TList.Create;
  //if ReportLink.OptionsSelection.ProcessSelection then BuildSelectedRecordList; {moved to BeforeBuilding in 3.03}
end;

destructor TdxCustomGridTableViewFormatter.Destroy;
begin
  FreeAndNil(FRecordIndexes);
  FreeAndNil(FSelectedRecordList);
  inherited;
end;

function TdxCustomGridTableViewFormatter.Adapter: TdxCustomGridTableViewAdapter;
begin
  Result := inherited Adapter as TdxCustomGridTableViewAdapter;
end;

function TdxCustomGridTableViewFormatter.Builder: TdxCustomGridTableViewBuilder;
begin
  Result := inherited Builder as TdxCustomGridTableViewBuilder;
end;

function TdxCustomGridTableViewFormatter.IndexOfRecord(ARecord: TcxCustomGridRecord): Integer;
begin
  for Result := 0 to RecordCount - 1 do
    if Records[Result] = ARecord then Exit;
  Result := -1;
end;

procedure TdxCustomGridTableViewFormatter.CheckDisplayValuePost(
  AProperties: TcxCustomEditProperties; ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnItem: TAbstractdxReportCellData);
var
  S: string;
begin
  if (ATableItem <> nil) and (ARecord <> nil) and (AnItem is TdxReportCellText) and
    (AProperties.GetEditValueSource(False) = evsValue) then
  begin
    S := TdxReportCellText(AnItem).Text;
    TcxCustomGridTableItemAccess(ATableItem).DoGetDisplayText(ARecord, S);
    TdxReportCellText(AnItem).Text := S;
  end;
end;

procedure TdxCustomGridTableViewFormatter.DoInitializeItem(AItem: TAbstractdxReportCellData;
  ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False);
var
  ACellValue: TcxEditValue;
  AContentBackgroundBitmapStyleIndex: Integer;
  AProperties: TcxCustomEditProperties;
  AViewParams: TdxReportItemViewParams;
begin
  AProperties := GetItemProperties(ATableItem, ARecord);
  AViewParams := GetItemViewParams(ATableItem, ARecord, AIsPreview, True);
  ACellValue := GetItemValue(AProperties, ATableItem, ARecord);

  SetViewParams(AItem, AViewParams);
  dxPSDataMaps.InitializeItem(AItem, AProperties, ACellValue, Self, AViewParams, AIsPreview, ARecord.RecordIndex, ATableItem);
  CheckDisplayValuePost(AProperties, ATableItem, ARecord, AItem);
  dxPSDataMaps.GetImageLists(AProperties, ReportLink.AppendImageList);

  if AIsPreview then
    AContentBackgroundBitmapStyleIndex := bbPreview
  else
    AContentBackgroundBitmapStyleIndex := GetContentBackgroundBitmapStyleIndex(ATableItem);

  if HasBackgroundBitmap(AContentBackgroundBitmapStyleIndex) then
    AItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(AContentBackgroundBitmapStyleIndex);

  AItem.Data := TdxNativeInt(ATableItem);
end;

function TdxCustomGridTableViewFormatter.GetDataItemClass(
  ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := dxPSDataMaps.ItemClass(GetItemProperties(ATableItem, ARecord),
    GetItemViewParams(ATableItem, ARecord, AIsPreview, True), AIsPreview);
end;

function TdxCustomGridTableViewFormatter.GetItemClass(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := dxPSDataMaps.ItemClass(GetItemProperties(ATableItem, ARecord), AIsPreview);
end;

function TdxCustomGridTableViewFormatter.GetItemProperties(
  ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord): TcxCustomEditProperties;
begin
  Result := Adapter.GetProperties(ATableItem, ARecord);
end;

function TdxCustomGridTableViewFormatter.GetItemValue(AProperties: TcxCustomEditProperties;
  ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord): TcxEditValue;
var
  S: string;
begin
  if AProperties.GetEditValueSource(False) = evsValue then
    Result := ARecord.Values[ATableItem.Index]
  else
  begin
    S := ARecord.DisplayTexts[ATableItem.Index];
    TcxCustomGridTableItemAccess(ATableItem).DoGetDisplayText(ARecord, S);
    Result := S;
  end;
end;

function TdxCustomGridTableViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    if CanProcessSelectionStyle(ARecord) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      if AnIsPreview then
        ReportLink.Styles.GetPreviewParams(ARecord, ATableItem, Result.NativeParams)
      else
        ReportLink.Styles.GetContentParams(ARecord, ATableItem, Result.NativeParams)
  end
  else
    if CanProcessSelectionStyle(ARecord) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      if AnIsPreview then
        Result.NativeParams := Adapter.GetPreviewViewParams(ARecord, ATableItem)
      else
        Result.NativeParams := Adapter.GetContentViewParams(ARecord, ATableItem, AIsDataCell);

  if AIsDataCell then
  begin
    if Adapter.TryGetAdvancedStyle(Point(ATableItem.Index, ARecord.RecordIndex), AStyle) and (AStyle <> nil) then
      Result.AdvancedViewParams := TdxSpreadSheetAdvancedViewParams.CreateFrom(AStyle);
  end;
end;

function TdxCustomGridTableViewFormatter.GetSelectionViewParams: TdxReportItemViewParams;
begin
  ReportLink.Styles.GetSelectionParams(Result.NativeParams);
end;

{ IdxPSCellParams }

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetAutoHeight: Boolean;
begin
  Result := Adapter.CellAutoHeight;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Canvas;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetDisplayGraphicsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayGraphicsAsText;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetDisplayTrackBarsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayTrackBarsAsText;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetEndEllipsis: Boolean;
begin
  Result := Adapter.CellEndEllipsis;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetFlatCheckMarks: Boolean;
begin
  Result := ReportLink.OptionsRefinements.FlatCheckMarks;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetGraphicsText: string;
begin
  Result := ReportLink.OptionsRefinements.GraphicsText;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetMultiline: Boolean;
begin
  Result := Adapter.CellMultiline;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams_GetTransparentGraphics: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentGraphics;
end;

{ IdxPSCellParams2 }
function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetPreviewMarginLeft: Integer;
begin
  Result := dxTextSpace;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetPreviewMarginRight: Integer;
begin
  Result := dxTextSpace;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetPreviewMaxHeight: Integer;
begin
  Result := -1;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetPreviewMaxLineCount: Integer;
begin
  Result := -1;
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetRichEditGraphicClass: TGraphicClass;
const
  GraphicClasses: array[Boolean] of TGraphicClass = (TMetafile, TBitmap);
begin
  Result := GraphicClasses[IdxPSCellParams2_GetRichEditTransparent];
end;

function TdxCustomGridTableViewFormatter.IdxPSCellParams2_GetRichEditTransparent: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentRichEdits;
end;

procedure TdxCustomGridTableViewFormatter.AfterBuilding;
begin
  inherited;
  ReportLink.ReportCells.BorderColor := GridLineColor;
end;

procedure TdxCustomGridTableViewFormatter.BeforeBuilding;
begin
  inherited;
  if ReportLink.OptionsSelection.ProcessSelection then BuildSelectedRecordList;
end;

procedure TdxCustomGridTableViewFormatter.BuildSelectedRecordList;
var
  I: Integer;
  ARecord: TcxCustomGridRecord;
  IsRecordIncluded: Boolean;
  Helper: TdxCustomGridRecordHelper;
begin
  SelectedRecordList.Clear;

  for I := 0 to Adapter.RecordCount - 1 do
  begin
    ARecord := Adapter.Records[I];
    IsRecordIncluded := ARecord.Selected;
    if not IsRecordIncluded then
    begin
      Helper := Adapter.RecordHelpers[ARecord];
      IsRecordIncluded := Helper.HasSelectedChildren or
        (not ReportLink.OptionsSelection.ProcessExactSelection and (SelectedRecordList.IndexOf(Helper.Parent) <> -1));
    end;

    if IsRecordIncluded then
      SelectedRecordList.Add(ARecord);
  end;
end;

function TdxCustomGridTableViewFormatter.CanProcessSelectionStyle(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := (ARecord <> nil) and ReportLink.OptionsFormatting.ConsumeSelectionStyle and IsSelectedRecord(ARecord);
  if Result and not ReportLink.OptionsFormatting.UseNativeStyles then
    Result := Adapter.HasSelectionStyle;
end;

function TdxCustomGridTableViewFormatter.IsSelectedRecord(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := (SelectedRecordCount <> 0) or ARecord.Selected;
end;

function TdxCustomGridTableViewFormatter.GetContentBackgroundBitmapStyleIndex(ATableItem: TcxCustomGridTableItem): Integer;
begin
  Result := bbContent;
end;

function TdxCustomGridTableViewFormatter.MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AGridBackgroundBitmapIndex of
    bbContent:
      Result := vspsGridContent;
  else
    Result := 0;
  end;

  if Result = 0 then
    Result := inherited MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex);
end;

function TdxCustomGridTableViewFormatter.GetGridLineColor: TColor;
begin
  Result := ReportLink.OptionsFormatting.GridLineColor;
  if Result = clDefault then
    Result := Adapter.GridLineColor;
end;

function TdxCustomGridTableViewFormatter.GetRecord(Index: Integer): TcxCustomGridRecord;
begin
  if SelectedRecordCount <> 0 then
    Result := SelectedRecords[Index]
  else
    Result := Adapter.Records[Index];
end;

function TdxCustomGridTableViewFormatter.GetRecordCount: Integer;
begin
  Result := SelectedRecordCount;
  if Result = 0 then
    Result := Adapter.RecordCount;
end;

function TdxCustomGridTableViewFormatter.GetRecordIndex(ARecord: TcxCustomGridRecord): Integer;
var
  RecordIndex, IndexCount, I: Integer;
begin
  RecordIndex := ARecord.Index;
  IndexCount := FRecordIndexes.Count;
  if RecordIndex > IndexCount - 1 then
  begin
    FRecordIndexes.Count := RecordIndex + 1;
    for I := IndexCount to FRecordIndexes.Count - 1 do
      FRecordIndexes[I] := TObject(-1);
  end;
  Result := Integer(FRecordIndexes[RecordIndex]);
  if Result = -1 then
  begin
    FRecordIndexes[RecordIndex] := TObject(IndexOfRecord(ARecord));
    Result := Integer(FRecordIndexes[RecordIndex]);
  end;
end;

function TdxCustomGridTableViewFormatter.GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
begin
  Result := TcxCustomGridRecord(FSelectedRecordList[Index]);
end;

function TdxCustomGridTableViewFormatter.GetSelectedRecordCount: Integer;
begin
  Result := FSelectedRecordList.Count;
end;

{ TdxCustomGridTableViewBuilder }

function TdxCustomGridTableViewBuilder.Adapter: TdxCustomGridTableViewAdapter;
begin
  Result := inherited Adapter as TdxCustomGridTableViewAdapter;
end;

class function TdxCustomGridTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxCustomGridTableViewAdapter;
end;

function TdxCustomGridTableViewBuilder.Formatter: TdxCustomGridTableViewFormatter;
begin
  Result := inherited Formatter as TdxCustomGridTableViewFormatter;
end;

class function TdxCustomGridTableViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxCustomGridTableViewFormatter;
end;

procedure TdxCustomGridTableViewBuilder.DoBuildViewFooter;
begin
  if Formatter.ShowFilterBar and (Formatter.FilterBarPosition = fpBottom)  then
    CreateFilterBar;
  if Formatter.HasDetailsSeparator then
    CreateBottomDetailsSeparator;
  inherited DoBuildViewFooter;
end;

procedure TdxCustomGridTableViewBuilder.DoBuildViewHeader;
begin
  inherited DoBuildViewHeader;
  if Formatter.HasDetailsSeparator then
    CreateTopDetailsSeparator;
  if Formatter.ShowCaption then
    CreateCaption;
  if Formatter.ShowFilterBar and (Formatter.FilterBarPosition = fpTop) then
    CreateFilterBar;
end;

procedure TdxCustomGridTableViewBuilder.DoGetCellHeight(
  ARecord: TcxCustomGridRecord; ATableItem: TcxCustomGridTableItem; var AHeight: Integer);
begin
  ReportLink.DoGetCellHeight(GridView, ARecord, ATableItem, AHeight);
end;

function TdxCustomGridTableViewBuilder.GridView: TcxCustomGridTableView;
begin
  Result := inherited GridView as TcxCustomGridTableView;
end;

class function TdxCustomGridTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridTableView;
end;

{ TdxGridTableViewAdapter }

constructor TdxGridTableViewAdapter.Create(AMasterAdapter: TdxCustomGridViewAdapter;
  AGridView: TcxCustomGridView);
begin
  inherited;
  GridTableViewInfo_CalculateExpandButtonParams(GridView.ViewInfo); // because LevelIndent
end;

function TdxGridTableViewAdapter.GridView: TcxGridTableView;
begin
  Result := inherited GridView as TcxGridTableView;
end;

class function TdxGridTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridView;
end;

function TdxGridTableViewAdapter.Styles: TcxGridTableViewStyles;
begin
  Result := inherited Styles as TcxGridTableViewStyles;
end;

function TdxGridTableViewAdapter.HasDetails(AGridRow: TcxCustomGridRow): Boolean;
begin
  Result := RecordHelpers[AGridRow].HasDetails;
end;

function TdxGridTableViewAdapter.IsSummaryHasSameLink(
  AItem1, AItem2: TcxDataSummaryItem): Boolean;
begin
  Result := (AItem1.ItemLink = AItem2.ItemLink) and
    (AItem1.Position = spFooter) and (AItem2.Position = spFooter);
end;

function TdxGridTableViewAdapter.GetFooterItemCount(
  AItems: TcxDataSummaryItems; ACanMultiLine: Boolean; AIsGroup: Boolean): Integer;
var
  I, J, ACount: Integer;
const
  APos: array[Boolean] of TcxSummaryPosition = (spFooter, spGroup);
begin
  Result := 0;
  if AItems = nil then Exit;
  for I := 0 to ColumnCount - 1 do
  begin
    ACount := 0;
    for J := 0 to AItems.Count - 1 do
      if (AItems[J].Position = APos[AIsGroup]) and (AItems[J].ItemLink = Columns[I]) then
      begin
        Inc(ACount);
        if not ACanMultiline then Break;
      end;
    Inc(Result, ACount);
    if (Result = AItems.Count) then Break;
  end;
end;

function TdxGridTableViewAdapter.CheckSummaryItemIndex(
  AItems: TcxDataSummaryItems; AIndex: Integer; AIsGroup: Boolean): Integer;
var
  I: Integer;
  AColumn: TcxGridColumn;
const
  APos: array[Boolean] of TcxSummaryPosition = (spFooter, spGroup);
begin
  Result := AIndex;
  I := 0;
  while I <= Result do
  begin
    AColumn := AItems[I].ItemLink as TcxGridColumn;
    if (AColumn = nil) or (AColumn.VisibleIndex < 0) or (AItems[I].Position <> APos[AIsGroup]) then
      Inc(Result);
    Inc(I);
  end;
end;

function TdxGridTableViewAdapter.GetFooterItemInfo(
  AItems: TcxDataSummaryItems; AIndex: Integer; var ARowIndex: Integer): Integer;
var
  I: Integer;
  AColumn: TcxGridColumn;
begin
  ARowIndex := 0;
  AIndex := CheckSummaryItemIndex(AItems, AIndex, False);
  AColumn := AItems[AIndex].ItemLink as TcxGridColumn;
  for I := 0 to AIndex - 1 do
    if (AItems[I].ItemLink = AColumn) and (AItems[I].Position = spFooter) then
      Inc(ARowIndex);
  Result := AColumn.VisibleIndex;
end;

function TdxGridTableViewAdapter.GetFooterLineCount(AItems: TcxDataSummaryItems; ACanMultiLine: Boolean): Integer;
var
  ACount: Integer;
  I, J: Integer;
begin
  Result := RowLineCount;
  if (AItems <> nil) and ACanMultiLine then
    for I := 0 to AItems.Count - 1 do
    begin
      ACount := 0;
      for J := 0 to AItems.Count - 1 do
        if IsSummaryHasSameLink(AItems[I], AItems[J]) then
          Inc(ACount);
      Result := Max(Result, ACount);
    end;
end;

function TdxGridTableViewAdapter.GetGroupFooterCount(AGridRow: TcxCustomGridRow): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AGridRow.Level do
    if HasGroupFooter(AGridRow, I) then
      Inc(Result);
{  if Result > 0 then
     Result := 1;}
end;

function TdxGridTableViewAdapter.GetGroupFooterItemText(ARowIndex, ALevel, AIndex: Integer): string;
var
  SummaryIndex: Integer;
begin
  SummaryIndex := Summary.GroupFooterIndexOfItemLink[ALevel, Columns[AIndex]];
  if SummaryIndex <> -1 then
    Result := Summary.GroupFooterSummaryTexts[ARowIndex, ALevel, SummaryIndex]
  else
    Result := '';
end;

function TdxGridTableViewAdapter.HasGroupFooter(AGridRow: TcxCustomGridRow; ALevel: Integer): Boolean;
begin
  if GroupFootersMode = gfInvisible then
    Result := False
  else
  begin
    if GroupFootersMode = gfAlwaysVisible then
      Dec(ALevel);
    Result := (0 <= ALevel) and (ALevel < AGridRow.Level) and
      TcxCustomGridRecordAccess(AGridRow).IsParentRecordLast[ALevel] and
      TcxGridColumnAccess(GridView.GroupedColumns[AGridRow.Level - 1 - ALevel]).Options.GroupFooters;
  end;
  if not Result and (AGridRow is TcxGridGroupRow) then
  begin
    Result := Result or
      (GroupFootersMode = gfAlwaysVisible) and (ALevel = -1) and not AGridRow.Expanded and
      TcxGridColumnAccess(TcxGridGroupRow(AGridRow).GroupedColumn).Options.GroupFooters;
  end;
end;

function TdxGridTableViewAdapter.HasGroupFooterItem(ALevel, AIndex: Integer): Boolean;
begin
  Result := Summary.GroupFooterIndexOfItemLink[ALevel, Columns[AIndex]] <> -1;
end;

function TdxGridTableViewAdapter.HasGroupFooters(AGridRow: TcxCustomGridRow): Boolean;
begin
  Result := GetGroupFooterCount(AGridRow) <> 0;
end;

function TdxGridTableViewAdapter.Controller: TcxGridTableController;
begin
  Result := inherited Controller as TcxGridTableController;
end;

function TdxGridTableViewAdapter.DataController: TcxGridDataController;
begin
  Result := GridView.DataController;
end;

function TdxGridTableViewAdapter.GetAutoWidth: Boolean;
begin
  Result := GridView.OptionsView.ColumnAutoWidth;
end;

function TdxGridTableViewAdapter.GetCellAutoHeight: Boolean;
begin
  Result := inherited GetCellAutoHeight and (DetailsLineCount = 1) and not DataRowHeightAssigned;
end;

function TdxGridTableViewAdapter.GetCellMultiline: Boolean;
begin
  Result := inherited GetCellAutoHeight;// {3.1} and not DataRowHeightAssigned;// and (DetailsLineCount <> 1);
end;

function TdxGridTableViewAdapter.GetHeaderAutoHeight: Boolean;
begin
  Result := GridView.OptionsView.HeaderAutoHeight and not HeaderHeightAssigned;
end;

function TdxGridTableViewAdapter.GetHeaderMultiline(Column: TcxGridColumn): Boolean;
begin
  Result := HeaderAutoHeight or HeaderHeightAssigned;
end;

function TdxGridTableViewAdapter.GetIndentCount: Integer;
begin
  Result := inherited GetIndentCount + GroupedLevelCount;
  if (Result <> 0) and (GroupedLevelCount <> 0) and IsOffice11StyleGrouping then
    Dec(Result);
end;

function TdxGridTableViewAdapter.GetIndentWidth: Integer;
begin
  Result := GridView.ViewInfo.LevelIndent;
end;

function TdxGridTableViewAdapter.GetIndicatorWidth: Integer;
begin
  if GridView.OptionsView.Indicator then
    Result := GridView.OptionsView.IndicatorWidth
  else
    Result := 0;
end;

function TdxGridTableViewAdapter.GetIsOffice11StyleGrouping: Boolean;
begin
  Result := GridView.OptionsView.GroupRowStyle = grsOffice11;
end;

function TdxGridTableViewAdapter.GetViewWidthExtra: Integer;
var
  IndentCount: Integer;
begin
  Result := inherited GetViewWidthExtra;
  IndentCount := GroupedLevelCount;
  if (IndentCount <> 0) and IsOffice11StyleGrouping then
    Dec(IndentCount);
  Inc(Result, IndentCount * IndentWidth);
end;

function TdxGridTableViewAdapter.GetDataRowHeight: Integer;
begin
  Result := GridView.OptionsView.DataRowHeight;
end;

function TdxGridTableViewAdapter.GetDataRowHeightAssigned: Boolean;
begin
  Result := DataRowHeight <> 0;
end;

function TdxGridTableViewAdapter.GetFooterHeight: Integer;
begin
  Result := ScaleFactor.Apply(DefaultFooterLineHeight);//GridView.OptionsView.HeaderHeight; {PS 3.03}
end;

function TdxGridTableViewAdapter.GetFooterHeightAssigned: Boolean;
begin
  Result := False;
end;

function TdxGridTableViewAdapter.GetGroupFooterHeight: Integer;
begin
  Result := ScaleFactor.Apply(DefaultGroupFooterLineHeight);
end;

function TdxGridTableViewAdapter.GetGroupFooterHeightAssigned: Boolean;
begin
  Result := False;
end;

function TdxGridTableViewAdapter.GetGroupRowHeight: Integer;
begin
  Result := GridView.OptionsView.GroupRowHeight;
end;

function TdxGridTableViewAdapter.GetGroupRowHeightAssigned: Boolean;
begin
  Result := GroupRowHeight <> 0;
end;

function TdxGridTableViewAdapter.GetHeaderHeight: Integer;
begin
  Result := GridView.OptionsView.HeaderHeight;
end;

function TdxGridTableViewAdapter.GetHeaderHeightAssigned: Boolean;
begin
  Result := HeaderHeight <> 0;
end;

function TdxGridTableViewAdapter.GetRowLineCount: Integer;
begin
  Result := 1;
end;

function TdxGridTableViewAdapter.GetFilterBarViewParams: TcxViewParams;
begin
  Styles.GetViewParams(vsFilterBox, nil, nil, Result);
end;

function TdxGridTableViewAdapter.GetFooterViewParams(ARecord: TcxCustomGridRecord;
  AGroupLevel: Integer; AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TcxViewParams;
begin
  if (AColumn <> nil) and (AColumn.Styles <> nil) then
    Styles.GetFooterCellParams(TcxCustomGridRow(ARecord), AColumn, AGroupLevel, AItem, Result)
  else
    Styles.GetFooterParams(TcxCustomGridRow(ARecord), AColumn, AGroupLevel, AItem, Result);
end;

function TdxGridTableViewAdapter.GetGroupRowViewParams(ARecord: TcxCustomGridRecord;
  AGroupLevel: Integer): TcxViewParams;
begin
  if (AGroupLevel >= 0) and (AGroupLevel < Styles.GridView.GroupedItemCount) then
    Styles.GetGroupParams(ARecord, AGroupLevel, Result)
  else
    Styles.GetViewParams(vsGroup, ARecord, nil, Result);
end;

function TdxGridTableViewAdapter.GetHeaderViewParams(AColumn: TcxGridColumn): TcxViewParams;
begin
  if (AColumn <> nil) and (AColumn.Styles <> nil) then
    AColumn.Styles.GetHeaderParams(Result)
  else
    Styles.GetHeaderParams(AColumn, Result);
  if AColumn = nil then
    Result.Color := LookAndFeelPainter.DefaultHeaderBackgroundColor;
end;

function TdxGridTableViewAdapter.GetPreviewViewParams(ARecord: TcxCustomGridRecord;
  AnItem: TcxCustomGridTableItem): TcxViewParams;
begin
  Styles.GetPreviewParams(ARecord, TcxGridColumn(AnItem), Result);
  // 3.2
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TdxGridTableViewAdapter.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := GridView.VisibleColumns[Index];
end;

function TdxGridTableViewAdapter.GetColumnCount: Integer;
begin
  Result := GridView.VisibleColumnCount;
end;

function TdxGridTableViewAdapter.GetFooterItemText(Index: Integer): string;
var
  SummaryIndex: Integer;
begin
  SummaryIndex := Summary.FooterSummaryItems.IndexOfItemLink(Columns[Index]);
  if SummaryIndex <> -1  then
    Result := Summary.FooterSummaryTexts[SummaryIndex]
  else
    Result := '';
end;

function TdxGridTableViewAdapter.GetFooterMultiline: Boolean;
begin
  Result := GridView.OptionsView.FooterAutoHeight;
end;

function TdxGridTableViewAdapter.GetFooterMultiSummaries: Boolean;
begin
  Result := GridView.OptionsView.CanShowFooterMultiSummaries;
end;

function TdxGridTableViewAdapter.GetGridLineColor: TColor;
begin
  Result := GridView.OptionsView.GridLineColor;
  if Result = clDefault then
    Result := GridView.LookAndFeelPainter.DefaultGridLineColor;
end;

function TdxGridTableViewAdapter.GetGridLines: TcxGridLines;
begin
  Result := GridView.OptionsView.GridLines;
end;

function TdxGridTableViewAdapter.GetGroupedColumn(Index: Integer): TcxGridColumn;
begin
  Result := GridView.GroupedColumns[Index];
end;

function TdxGridTableViewAdapter.GetGroupedColumnCount: Integer;
begin
  Result := GridView.GroupedColumnCount;
end;

function TdxGridTableViewAdapter.GetGroupedLevelCount: Integer;
begin
  Result := GridView.DataController.Groups.LevelCount;
end;

function TdxGridTableViewAdapter.GetGroupFootersMode: TcxGridGroupFootersMode;
begin
  Result := GridView.OptionsView.GroupFooters;
end;

function TdxGridTableViewAdapter.GetGroupFooterMultiSummaries: Boolean;
begin
  Result := GridView.OptionsView.CanShowGroupFooterMultiSummaries;
end;

function TdxGridTableViewAdapter.GetGroupFootersMultiline: Boolean;
begin
  Result := GridView.OptionsView.FooterAutoHeight;
end;

function TdxGridTableViewAdapter.GetGroupRowSeparatorColor: TColor;
begin
  Result := dxOffice11.dxOffice11GroupRowSeparatorColor;
end;

function TdxGridTableViewAdapter.GetGroupRowSeparatorThickness: Integer;
begin
  Result := cxGridTableView.cxGridOffice11GroupRowSeparatorWidth;
end;

function TdxGridTableViewAdapter.GetHasFooterItem(Index: Integer): Boolean;
begin
  Result := Summary.FooterSummaryItems.IndexOfItemLink(Columns[Index]) <> -1;
end;

function TdxGridTableViewAdapter.GetHasPreview: Boolean;
begin
  Result := PreviewColumn <> nil;
end;

function TdxGridTableViewAdapter.GetHasRowSeparators: Boolean;
begin
  Result := RowSeparatorThickness > 0;
end;

function TdxGridTableViewAdapter.GetHeaderEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.HeaderEndEllipsis;
end;

function TdxGridTableViewAdapter.GetInternalColumnMinWidth(Column: TcxGridColumn): Integer;
begin
  Result := Column.MinWidth;
  if Column.IsMostLeft then
    Inc(Result, IndentCount * IndentWidth);
end;

function TdxGridTableViewAdapter.GetInternalColumnWidth(Column: TcxGridColumn): Integer;
begin
  Result := Column.Width;
  if Column.IsMostLeft then
    Inc(Result, IndentCount * IndentWidth);
end;

function TdxGridTableViewAdapter.GetPreviewColumn: TcxGridColumn;
begin
  Result := GridView.Preview.Column;
end;

function TdxGridTableViewAdapter.GetPreviewLeftIndent: Integer;
begin
  Result := GridView.Preview.LeftIndent;
end;

function TdxGridTableViewAdapter.GetPreviewPlace: TcxGridPreviewPlace;
begin
  Result := GridView.Preview.Place;
end;

function TdxGridTableViewAdapter.GetPreviewRightIndent: Integer;
begin
  Result := GridView.Preview.RightIndent;
end;

function TdxGridTableViewAdapter.GetRecordHelper(ARecord: TcxCustomGridRecord): TdxCustomGridRowHelper;
begin
  Result := TdxCustomGridRowHelper(inherited RecordHelpers[ARecord]);
end;

function TdxGridTableViewAdapter.GetRow(Index: Integer): TcxCustomGridRow;
begin
  Result := TcxCustomGridRow(Records[Index]);
end;

function TdxGridTableViewAdapter.GetRowCount: Integer;
begin
  Result := RecordCount;
end;

function TdxGridTableViewAdapter.GetRowSeparatorColor: TColor;
begin
  Result := GridView.OptionsView.RowSeparatorColor;
end;

function TdxGridTableViewAdapter.GetRowSeparatorThickness: Integer;
begin
  Result := GridView.OptionsView.RowSeparatorWidth;
end;

function TdxGridTableViewAdapter.GetShowExpandButtonsForEmptyDetails: Boolean;
begin
  Result := GridView.OptionsView.ExpandButtonsForEmptyDetails;
end;

function TdxGridTableViewAdapter.GetShowHorzGridLines: Boolean;
begin
  Result := GridLines in [glBoth, glHorizontal];
end;

function TdxGridTableViewAdapter.GetShowVertGridLines: Boolean;
begin
  Result := GridLines in [glBoth, glVertical];
end;

function TdxGridTableViewAdapter.GetThemedFooterItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFooterColor;
end;

function TdxGridTableViewAdapter.GetThemedFooterItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFooterTextColor;
end;

function TdxGridTableViewAdapter.GetThemedHeaderItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

function TdxGridTableViewAdapter.GetThemedHeaderItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderTextColor;
end;

{ TdxCustomGridTableViewItemPlaceController }

constructor TdxCustomGridTableViewItemPlaceController.Create(AFormatter: TdxGridTableViewFormatter);
begin
  inherited Create;
  FFormatter := AFormatter;
  FHeaderLineCount := -1;
  FWidth := -1;
end;

procedure TdxCustomGridTableViewItemPlaceController.Calculate;
begin
end;

procedure TdxCustomGridTableViewItemPlaceController.Refresh;
begin
end;

function TdxCustomGridTableViewItemPlaceController.CalculateHeaderLineCount: Integer;
begin
  Result := 1;
end;

function TdxCustomGridTableViewItemPlaceController.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TdxCustomGridTableViewItemPlaceController.GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace;
begin
  Result := nil;
end;

procedure TdxCustomGridTableViewItemPlaceController.WidthChanged;
begin
end;

function TdxCustomGridTableViewItemPlaceController.GetHeaderLineCount: Integer;
begin
  if FHeaderLineCount = -1 then
    FHeaderLineCount := CalculateHeaderLineCount;
  Result := FHeaderLineCount;
end;

function TdxCustomGridTableViewItemPlaceController.GetWidth: Integer;
begin
  if FWidth = -1 then
    FWidth := CalculateWidth;
  Result := FWidth;
end;

procedure TdxCustomGridTableViewItemPlaceController.SetWidth(Value: Integer);
begin
  FWidth := Value;
  WidthChanged;
end;

{ TdxGridTableViewColumnPlace }

constructor TdxGridTableViewColumnPlace.Create(AController: TdxGridTableViewColumnPlaceController;
  AColumn: TcxGridColumn);
begin
  inherited Create;
  FController := AController;
  FColumn := AColumn;

  FLeftBound := -1;
  FWidth := -1;
end;

procedure TdxGridTableViewColumnPlace.Calculate(ALeftBound: Integer);
begin
  FLeftBound := ALeftBound;
end;

function TdxGridTableViewColumnPlace.CalculateLeftBound: Integer;
begin
  Result := Controller.CalculateItemLeftBound(Self);
end;

function TdxGridTableViewColumnPlace.GetLineCount: Integer;
begin
  Result := 1;
end;

function TdxGridTableViewColumnPlace.GetRowIndex: Integer;
begin
  Result := 0;
end;

procedure TdxGridTableViewColumnPlace.InitAutoWidthItem(AnItem: TcxAutoWidthItem);
begin
  AnItem.Fixed := IsFixed;
  AnItem.MinWidth := MinWidth;
  AnItem.Width := Width;
end;

function TdxGridTableViewColumnPlace.GetAdapter: TdxGridTableViewAdapter;
begin
  Result := Formatter.Adapter;
end;

function TdxGridTableViewColumnPlace.GetCellBounds(RowIndex: Integer): TRect;
var
  CellHeight: Integer;
begin
  CellHeight := CellHeights[RowIndex];
  Result.Left := LeftBound;
  Result.Top := Self.RowIndex * CellHeight;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + LineCount * CellHeight;

  if Column.IsMostLeft then
    Inc(Result.Left, Formatter.ViewWidthExtra);
end;

function TdxGridTableViewColumnPlace.GetCellHeight(RowIndex: Integer): Integer;
begin
  Result := Formatter.RowHeights[RowIndex];
end;

function TdxGridTableViewColumnPlace.GetFooterCellBounds: TRect;
begin
  Result.Left := LeftBound;
  Result.Top := RowIndex * FooterLineHeight;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + LineCount * FooterLineHeight;

  InflateRect(Result, -Adapter.ScaleFactor.Apply(FooterItemInflateHorz), -Adapter.ScaleFactor.Apply(FooterItemInflateVert));
  if Column.IsMostLeft then
    Inc(Result.Left, 2);
end;

function TdxGridTableViewColumnPlace.GetFooterLineHeight: Integer;
begin
  Result := Formatter.FooterLineHeight;
end;

function TdxGridTableViewColumnPlace.GetFormatter: TdxGridTableViewFormatter;
begin
  Result := Controller.Formatter;
end;

function TdxGridTableViewColumnPlace.GetGroupFooterCellBounds(IndentCount: Integer): TRect;
var
  V: Integer;
begin
  Result.Left := LeftBound;
  Result.Top := RowIndex * GroupFooterLineHeight;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + LineCount * GroupFooterLineHeight;

  V := IndentCount * Formatter.IndentWidth;
  OffsetRect(Result, -V, 0);
  if Column.IsMostLeft then
    Inc(Result.Left, V);

  InflateRect(Result, -Adapter.ScaleFactor.Apply(FooterItemInflateHorz), -Adapter.ScaleFactor.Apply(FooterItemInflateVert));
  if Column.IsMostLeft then
    Inc(Result.Left, 2);
end;

function TdxGridTableViewColumnPlace.GetGroupFooterLineHeight: Integer;
begin
  Result := Formatter.GroupFooterLineHeight;
end;

function TdxGridTableViewColumnPlace.GetHeaderCellBounds: TRect;
begin
  Result.Left := LeftBound;
  Result.Top := RowIndex * HeaderLineHeight;
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + LineCount * HeaderLineHeight;
end;

function TdxGridTableViewColumnPlace.GetHeaderLineHeight: Integer;
begin
  Result := Formatter.HeaderLineHeight;
end;

function TdxGridTableViewColumnPlace.GetIndex: Integer;
begin
  Result := Controller.IndexOf(Self);
end;

function TdxGridTableViewColumnPlace.GetIsFixed: Boolean;
begin
  Result := GridColumn_GetFixed(Column);
end;

function TdxGridTableViewColumnPlace.GetLeftBound: Integer;
begin
  if FLeftBound = -1 then
    FLeftBound := CalculateLeftBound;
  Result := FLeftBound;
end;

function TdxGridTableViewColumnPlace.GetMinWidth: Integer;
begin
  Result := Adapter.InternalColumnMinWidths[Column];
end;

function TdxGridTableViewColumnPlace.GetOriginalWidth: Integer;
begin
  Result := Adapter.InternalColumnWidths[Column];
end;

function TdxGridTableViewColumnPlace.GetWidth: Integer;
begin
  Result := FWidth;
  if Result = -1 then
    Result := OriginalWidth;
end;

{ TdxGridTableViewColumnPlaceController }

constructor TdxGridTableViewColumnPlaceController.Create(AFormatter: TdxGridTableViewFormatter);
begin
  inherited;
  FColumnIndexes := TList.Create;
  FItems := TList.Create;
  FWidth := -1;
end;

destructor TdxGridTableViewColumnPlaceController.Destroy;
begin
  FreeAndNilItems;
  FreeAndNil(FColumnIndexes);
  inherited;
end;

procedure TdxGridTableViewColumnPlaceController.Calculate;
begin
  if ItemsAutoWidth then CalculateItemsWidth;
end;

procedure TdxGridTableViewColumnPlaceController.Refresh;
begin
  FColumnIndexes.Clear;
  ClearItems;
  AddItems;
end;

function TdxGridTableViewColumnPlaceController.IndexOf(AnItem: TdxGridTableViewColumnPlace): Integer;
begin
  Result := FItems.IndexOf(AnItem);
end;

function TdxGridTableViewColumnPlaceController.IndexOf(AColumn: TcxGridColumn): Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Column = AColumn then
      Exit;
  Result := -1;
end;

function TdxGridTableViewColumnPlaceController.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ItemCount - 1 do
    Inc(Result, Items[I].Width);
end;

function TdxGridTableViewColumnPlaceController.GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace;
var
  Index: Integer;
begin
  Index := ColumnIndexes[Column];
  //Index := IndexOf(Column);
  if Index <> -1 then // might be -1 when we are in loading stage (Building report while loading form)
    Result := Items[Index]
  else
    Result := nil;
end;

procedure TdxGridTableViewColumnPlaceController.AddItems;
var
  I: Integer;
begin
  for I := 0 to Formatter.ColumnCount - 1 do
    CreateItem(Formatter.Columns[I]);
end;

procedure TdxGridTableViewColumnPlaceController.ClearItems;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxGridTableViewColumnPlaceController.CreateItem(AColumn: TcxGridColumn): TdxGridTableViewColumnPlace;
begin
  Result := GetItemClass(AColumn).Create(Self, AColumn);
  FItems.Add(Result);
end;

function TdxGridTableViewColumnPlaceController.GetItemClass(AColumn: TcxGridColumn): TdxGridTableViewColumnPlaceClass;
begin
  Result := TdxGridTableViewColumnPlace;
end;

function TdxGridTableViewColumnPlaceController.CalculateItemLeftBound(AnItem: TdxGridTableViewColumnPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AnItem.Index - 1 do
    Inc(Result, Items[I].Width);
end;

procedure TdxGridTableViewColumnPlaceController.CalculateItemsWidth;
var
  AutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AutoWidthObject := TcxAutoWidthObject.Create(ItemCount);
  try
    for I := 0 to ItemCount - 1 do
      Items[I].InitAutoWidthItem(AutoWidthObject.AddItem);
    AutoWidthObject.AvailableWidth := ItemsAvailableWidth;
    AutoWidthObject.Calculate;

    for I := 0 to ItemCount - 1 do
      Items[I].Width := AutoWidthObject[I].AutoWidth;
  finally
    AutoWidthObject.Free;
  end;
end;

function TdxGridTableViewColumnPlaceController.GetItemsAutoWidth: Boolean;
begin
  Result := Formatter.AutoWidth;
end;

function TdxGridTableViewColumnPlaceController.GetItemsAvailableWidth: Integer;
begin
  Result := Formatter.ViewAvailableWidth;
end;

function TdxGridTableViewColumnPlaceController.GetAdapter: TdxGridTableViewAdapter;
begin
  Result := Formatter.Adapter;
end;

function TdxGridTableViewColumnPlaceController.GetColumnIndex(Column: TcxGridColumn): Integer;
var
  ColumnIndex, IndexCount, I: Integer;
begin
  ColumnIndex := Column.Index;
  IndexCount := FColumnIndexes.Count;
  if ColumnIndex > IndexCount - 1 then
  begin
    FColumnIndexes.Count := ColumnIndex + 1;
    for I := IndexCount to FColumnIndexes.Count - 1 do
      FColumnIndexes[I] := TObject(-1);
  end;
  Result := Integer(FColumnIndexes[ColumnIndex]);
  if Result = -1 then
  begin
    FColumnIndexes[ColumnIndex] := TObject(IndexOf(Column));
    Result := Integer(FColumnIndexes[ColumnIndex]);
  end;
end;

function TdxGridTableViewColumnPlaceController.GetItem(Index: Integer): TdxGridTableViewColumnPlace;
begin
  Result := TdxGridTableViewColumnPlace(FItems[Index]);
end;

function TdxGridTableViewColumnPlaceController.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxGridTableViewColumnPlaceController.FreeAndNilItems;
begin
  ClearItems;
  FreeAndNil(FItems);
end;

{ TdxGridTableViewReportDataAdapter }

constructor TdxGridTableViewReportDataAdapter.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TdxGridTableViewReportDataAdapter.GetReportItem(Row: TcxCustomGridRow;
  Column: TcxGridColumn): TdxReportVisualItem;
var
  ReportRow: TdxReportCell;
  I: Integer;
begin
  ReportRow := ReportRowsByGridRow[Row];
  if ReportRow <> nil then
    for I := 0 to ReportRow.DataItemCount - 1 do
    begin
      Result := ReportRow.DataItems[I];
      if Result.Data = TdxNativeInt(Column) then
        Exit;
    end;
  Result := nil;
end;

function TdxGridTableViewReportDataAdapter.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := Builder.ReportRows[Index];
end;

function TdxGridTableViewReportDataAdapter.GetReportRowByGridRow(Row: TcxCustomGridRow): TdxReportCell;
var
  I: Integer;
begin
  for I := 0 to ReportRowCount - 1 do
    if ReportRows[I].CellCount <> 0 then
    begin
      Result := ReportRows[I].Cells[0];
      if Result.Data = TdxNativeInt(Row) then
        Exit;
    end;
  Result := nil;
end;

function TdxGridTableViewReportDataAdapter.GetReportRowCount: Integer;
begin
  Result := Builder.ReportRowCount;
end;

{ TdxGridTableViewFormatter }

constructor TdxGridTableViewFormatter.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited;
  FColumns := TList.Create;
  FItemPlaceController := GetItemPlaceControllerClass.Create(Self);
  FReportDataAdapter := TdxGridTableViewReportDataAdapter.Create(ABuilder);
  FRowHeights := TList.Create;
end;

destructor TdxGridTableViewFormatter.Destroy;
begin
  FreeAndNil(FRowHeights);
  FreeAndNil(FItemPlaceController);
  FreeAndNil(FReportDataAdapter);
  FreeAndNil(FColumns);
  inherited;
end;

function TdxGridTableViewFormatter.Adapter: TdxGridTableViewAdapter;
begin
  Result := Builder.Adapter;
end;

function TdxGridTableViewFormatter.Builder: TdxGridTableViewBuilder;
begin
  Result := inherited Builder as TdxGridTableViewBuilder;
end;

procedure TdxGridTableViewFormatter.DoInitializeFooterItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer; AItem: TcxDataSummaryItem);
var
  Column: TcxGridColumn;
begin
  Column := TcxGridColumn(AItem.ItemLink);
  SetViewParams(AnItem, GetFooterItemViewParams(Column, AItem));
  if HasBackgroundBitmap(bbFooter) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbFooter);

  RegisterLookAndFeelItem(AnItem, cesSunken);

  with AnItem as TdxReportCellString do
  begin
    Data := TdxNativeInt(Column);
    Text := AItem.FormatValue(Adapter.Summary.FooterSummaryValues[AnIndex], True);
    Multiline := Adapter.FooterMultiline;
    TextAlignX := TextAlignXMap[Column.FooterAlignmentHorz];
    if Multiline then
      TextAlignY := taTop
    else
      TextAlignY := taCenterY;
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeFooterRow(ARow: TdxReportCell);
begin
  SetViewParams(ARow, GetFooterItemViewParams(nil, nil));
  if HasBackgroundBitmap(bbFooter) then
    ARow.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbFooter);

  RegisterLookAndFeelItem(ARow, cesRaised);
  ARow.Data := TdxNativeInt(TdxGridFooter);
end;

procedure TdxGridTableViewFormatter.DoReportLinkInitializeFooterItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
  ReportLink.DoInitializeFooterCell(Adapter.GridView, nil, Columns[AnIndex], 0, TdxReportCellString(AnItem));
end;

function TdxGridTableViewFormatter.GetFooterItemBounds(AnIndex: Integer): TRect;
var
  Item: TdxGridTableViewColumnPlace;
begin
  Item := ItemPlaceController[Columns[AnIndex]];
  if Item <> nil then
    Result := Item.FooterCellBounds
  else
    Result := cxNullRect;
end;

function TdxGridTableViewFormatter.GetFooterItemClass(AnIndex: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxGridTableViewFormatter.GetFooterItemViewParams(
  AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetFooterParams(nil, -1, AColumn, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetFooterViewParams(nil, -1, AColumn, AItem);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if AColumn <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedFooterItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedFooterItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupFooterHost(AnItem: TdxReportVisualItem;
  ARecord: TcxCustomGridRecord; AGroupLevel: Integer);
begin
  SetViewParams(AnItem, GetGroupFooterItemViewParams(nil, AGroupLevel, nil, nil));
  if HasBackgroundBitmap(bbFooter) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbFooter);

  RegisterLookAndFeelItem(AnItem, cesRaised);
  AnItem.Data := TdxNativeInt(ARecord);
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupFooterItem(AnItem: TAbstractdxReportCellData;
  ARecord: TcxCustomGridRecord; AGroupLevel: Integer; const AValue: Variant; AItem: TcxDataSummaryItem);
var
  AColumn: TcxGridColumn;
begin
  AColumn := TcxGridColumn(AItem.ItemLink);
  SetViewParams(AnItem, GetGroupFooterItemViewParams(
    ARecord, AGroupLevel, AColumn, AItem));
  if HasBackgroundBitmap(bbFooter) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbFooter);
  RegisterLookAndFeelItem(AnItem, cesSunken);

  with TdxReportCellString(AnItem) do
  begin
    Data := TdxNativeInt(AColumn);

    Multiline := Adapter.GetGroupFootersMultiline;
    Text := AItem.FormatValue(AValue, True);
    TextAlignX := TextAlignXMap[AColumn.FooterAlignmentHorz];
    if Multiline then
      TextAlignY := taTop
    else
      TextAlignY := taCenterY;
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupFooterRow(ARow: TdxReportCell;
  ARecord: TcxCustomGridRecord; AGroupLevel: Integer);
begin
  ARow.CellSides := csLeftRight;
  ARow.Data := AGroupLevel; //v3.0 ARecord.Level
  ARow.Transparent := True;
end;

procedure TdxGridTableViewFormatter.DoReportLinkInitializeGroupFooterItem(AnItem: TAbstractdxReportCellData;
  AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord; AGroupLevel: Integer);
begin
  ReportLink.DoInitializeFooterCell(Adapter.GridView, ARecord, AColumn,
    AGroupLevel, TdxReportCellString(AnItem));
end;

function TdxGridTableViewFormatter.GetGroupFooterItemBounds(AnIndex, AGroupLevel: Integer): TRect;
var
  Item: TdxGridTableViewColumnPlace;
begin
  Item := ItemPlaceController[Columns[AnIndex]];
  if Item <> nil then
    Result := Item.GroupFooterCellBounds[AGroupLevel]
  else
    Result := cxNullRect;
end;

function TdxGridTableViewFormatter.GetGroupFooterItemClass(AnIndex: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxGridTableViewFormatter.GetGroupFooterItemViewParams(ARecord: TcxCustomGridRecord;
  AGroupLevel: Integer; AColumn: TcxGridColumn; AItem: TcxDataSummaryItem): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetFooterParams(ARecord, AGroupLevel, AColumn, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetFooterViewParams(ARecord, AGroupLevel, AColumn, AItem);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if AColumn <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedFooterItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedFooterItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupRowItem(AnItem: TAbstractdxReportCellData;
  ARow: TcxGridGroupRow; AIndex: Integer; const AText: string);
begin
  if not ReportLink.OptionsFormatting.UseNativeStyles and IsDelphiObject(AnItem.Data)
    and (TObject(AnItem.Data) is TcxGridSummaryItemInfo) then
    SetViewParams(AnItem, Adapter.GridLines, TcxGridSummaryItemInfo(AnItem.Data).ViewParams)
  else
    SetViewParams(AnItem, GetGroupRowViewParams(ARow, ARow.Level));
  if HasBackgroundBitmap(bbGroup) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);
  with AnItem as TdxReportCellString do
  begin
    Data := TdxNativeInt(Adapter.GroupedColumns[ARow.Level]);
    Text := AText;
    if AIndex = 0 then
      TextAlignX := taLeft;
    if Adapter.IsOffice11StyleGrouping then TextAlignY := taBottom; {.2}
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupRow(ARow: TdxReportCell;
  ARecord: TcxCustomGridRecord);
begin
  ReportLink.AddReportRow(ARow);

  ARow.CellSides := [];
  ARow.Data := TdxNativeInt(ARecord);
  ARow.Transparent := True;
end;

function TdxGridTableViewFormatter.GetGroupRowClass: TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxGridTableViewFormatter.GetGroupRowViewParams(ARecord: TcxCustomGridRecord;
  ALevel: Integer): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ARecord) then
      Result := GetSelectionViewParams
    else
      ReportLink.Styles.GetGroupParams(ARecord, ALevel, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ARecord) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetGroupRowViewParams(ARecord, ALevel);

  Result.CellSides := CellSidesMap[Adapter.GridLines] - [csLeft] + [csRight];
  if ARecord.Index = 0 then
    Result.CellSides := Result.CellSides + [csTop];
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupRowSeparator(ASeparator: TAbstractdxReportCellData);
begin
  ASeparator.CellSides := csAll;
  ASeparator.Color := GroupRowSeparatorColor;
  ASeparator.Transparent := IsColorTransparent(ASeparator.Color); //ReportLink.Transparent;
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupSeparatorRow(ARow: TdxReportCell);
begin
  ARow.CellSides := csLeftRight;
  ARow.Transparent := True;
end;

function TdxGridTableViewFormatter.GetGroupRowSeparatorClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellBox;
end;

procedure TdxGridTableViewFormatter.DoInitializeHeaderItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
var
  Column: TcxGridColumn;
begin
  Column := Columns[AnIndex];
  SetViewParams(AnItem, GetHeaderItemViewParams(Column));
  if HasBackgroundBitmap(bbHeader) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbHeader);
  RegisterLookAndFeelItem(AnItem, cesRaised);

  with AnItem as TdxReportCellImage do
  begin
    Data := TdxNativeInt(Column);
    EndEllipsis := Adapter.HeaderEndEllipsis;
    if (Column.HeaderGlyph <> nil) and not Column.HeaderGlyph.Empty then
    begin
      Image := Column.HeaderGlyph;
      ImageLayout := HeaderImageLayoutMap[Column.HeaderGlyphAlignmentHorz, Column.HeaderGlyphAlignmentVert];
      IsTextDrawnForCenteredImage := True;
      IsTextShiftedForHorizontallyCenteredImage :=
        not (ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]) and (Text <> '');
    end;
    Multiline := Adapter.HeaderMultilines[Column];
    SortOrder := SortOrderMap[Column.SortOrder];
    if Column.Options.ShowCaption then
      Text := Column.Caption
    else
      Text := '';
    TextAlignX := TextAlignXMap[Column.HeaderAlignmentHorz];
    if Multiline then
      TextAlignY := TextAlignYMap[Column.HeaderAlignmentVert]
    else
      TextAlignY := taCenterY;
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeHeaderRow(ARow: TdxReportCell);
begin
  SetViewParams(ARow, GetHeaderItemViewParams(nil));
  ARow.Data := TdxNativeInt(TdxGridHeader);
end;

procedure TdxGridTableViewFormatter.DoReportLinkInitializeHeaderItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
  ReportLink.DoInitializeHeaderCell(Adapter.GridView, Columns[AnIndex], TdxReportCellString(AnItem));
end;

function TdxGridTableViewFormatter.GetHeaderItemBounds(AnIndex: Integer): TRect;
var
  Item: TdxGridTableViewColumnPlace;
begin
  Item := ItemPlaceController[Columns[AnIndex]];
  if Item <> nil then
    Result := Item.HeaderCellBounds
  else
    Result := cxNullRect;
end;

function TdxGridTableViewFormatter.GetHeaderItemClass(AnIndex: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellImage;
end;

function TdxGridTableViewFormatter.GetHeaderItemViewParams(AColumn: TcxGridColumn): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetHeaderParams(AColumn, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetHeaderViewParams(AColumn);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if AColumn <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedHeaderItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedHeaderItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := False;
end;

function TdxGridTableViewFormatter.CreateIndent(AParent: TdxReportCell): TdxReportCellExpandButton;
begin
  Result := GetIndentClass.Create(AParent);
end;

procedure TdxGridTableViewFormatter.DoInitializeDataRowIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord);
begin
  SetViewParams(AnIndent, GetIndentViewParams(ARecord, AnIndex, AnIndentCount, False));
  if HasBackgroundBitmap(bbGroup) then
    AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupFooterIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord);
begin
  SetViewParams(AnIndent, GetIndentViewParams(ARecord, AnIndex, AnIndentCount, True));
  if HasBackgroundBitmap(bbGroup) then
    AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);
//  if (AIndex = AIndentCount - 1) and Adapter.ShowHorzGridLines then
//    AIndent.CellSides := AIndent.CellSides + [csBottom];
end;

procedure TdxGridTableViewFormatter.DoInitializeGroupRowIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord);
begin
  SetViewParams(AnIndent, GetIndentViewParams(ARecord, AnIndex, AnIndentCount, False));
  if HasBackgroundBitmap(bbGroup) then
    AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);

  if AnIndex = AnIndentCount - 1 then
  begin
    AnIndent.CellSides := AnIndent.CellSides - [csRight];
    if Adapter.ShowHorzGridLines then
      AnIndent.CellSides := AnIndent.CellSides + [csTop];
    //if Adapter.IsOffice11StyleGrouping then
    //  AnIndent.CellSides := AnIndent.CellSides + [csBottom];

    AnIndent.ShowButton := HasExpandButton(ARecord);
    if AnIndent.ShowButton then
      DoInitializeExpandButton(AnIndent, ARecord, False);
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeMasterDataRowIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord);
begin
  { DoInitializeMasterIndent -> ARecord = nil }
  SetViewParams(AnIndent, GetIndentViewParams(ARecord, AnIndex, AnIndentCount, False));
  if AnIndex = AnIndentCount - 1 then
    if HasBackgroundBitmap(bbContent) then
      AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbContent)
    else
  else
    if HasBackgroundBitmap(bbGroup) then
      AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);

  if (ARecord <> nil) and (AnIndex = AnIndentCount - 1) then
  begin
    AnIndent.CellSides := AnIndent.CellSides - [csRight];
    if Adapter.ShowHorzGridLines then
      AnIndent.CellSides := AnIndent.CellSides + [csTop];
    if not ARecord.Expanded then
      AnIndent.CellSides := AnIndent.CellSides + [csBottom];

    AnIndent.ShowButton := ReportLink.OptionsView.ExpandButtons and ARecord.Expandable;
    if AnIndent.ShowButton then
      DoInitializeExpandButton(AnIndent, ARecord, True);
  end;
end;

procedure TdxGridTableViewFormatter.DoInitializeRowSeparatorIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ARecord: TcxCustomGridRecord);
begin
  SetViewParams(AnIndent, GetIndentViewParams(ARecord, AnIndex, AnIndentCount, True));
  if HasBackgroundBitmap(bbGroup) then
    AnIndent.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbGroup);
end;

function TdxGridTableViewFormatter.GetIndentClass: TdxReportCellExpandButtonClass;
begin
  if Adapter.IsOffice11StyleGrouping then
    Result := TdxReportCellExpandButtonEx
  else
    Result := TdxReportCellExpandButton;
end;

function TdxGridTableViewFormatter.GetIndentViewParams(ARecord: TcxCustomGridRecord;
  AnIndex, AnIndentCount: Integer; ANonRecordIndent: Boolean): TdxReportItemViewParams;

  function IsMasterDataRowIndent: Boolean;
  begin
    with Adapter do
      Result := AnIndex >= GroupedLevelCount - Ord(IsOffice11StyleGrouping and not (ARecord is TcxGridGroupRow));
  end;

  function GetRecord: TcxCustomGridRecord;
  begin
    if not ANonRecordIndent and (ARecord is TcxGridGroupRow) and (AnIndex = AnIndentCount - 1) then
      Result := ARecord
    else
      Result := nil;
  end;

const
  VertGridLinesMap: array[Boolean] of TdxCellSides = ([], csLeftRight);
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if IsMasterDataRowIndent then
      ReportLink.Styles.GetContentParams(Adapter.MasterGridRecord, nil, Result.NativeParams)
    else
      ReportLink.Styles.GetGroupParams(GetRecord, AnIndex, Result.NativeParams)
  else
    if IsMasterDataRowIndent then
      Result.NativeParams := Adapter.GetContentViewParams(Adapter.MasterGridRecord, nil)
    else
      Result.NativeParams := Adapter.GetGroupRowViewParams(GetRecord, AnIndex);

  Result.CellSides := VertGridLinesMap[Adapter.ShowHorzGridLines]; //PS3.1  (it was Adapter.ShowVertGridLines)
  if AnIndex = 0 then
    Result.CellSides := Result.CellSides + [csLeft];
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
end;

function TdxGridTableViewFormatter.HasExpandButton(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := ReportLink.OptionsView.ExpandButtons{$IFDEF VER500} and Adapter.RecordHelpers[ARecord].HasExpandButton{$ENDIF};
end;

procedure TdxGridTableViewFormatter.DoInitializeDataRow(ARow: TdxReportCell;
  ARecord: TcxCustomGridRecord);
begin
  ReportLink.AddReportRow(ARow);

  ARow.CellSides := csLeftRight;
  ARow.Data := TdxNativeInt(ARecord);
  ARow.Transparent := True;
end;

procedure TdxGridTableViewFormatter.DoReportLinkInitializeItem(AnItem: TAbstractdxReportCellData;
  AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord);
begin
  ReportLink.DoInitializeCell(Adapter.GridView, ARecord, AColumn, TdxReportCellString(AnItem));
end;

function TdxGridTableViewFormatter.GetItemBounds(AGridRow: TcxCustomGridRow;
  AGridRowIndex, AColumnIndex: Integer): TRect;
var
  Item: TdxGridTableViewColumnPlace;
begin
  Item := ItemPlaceController[Columns[AColumnIndex]];
  if Item <> nil then
    Result := Item.CellBounds[AGridRowIndex]
  else
    Result := cxNullRect;
end;

function TdxGridTableViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
var
  ACellSides: TdxCellSides;
begin
  Result := inherited GetItemViewParams(ATableItem, ARecord, AnIsPreview, AIsDataCell);
  if AnIsPreview then
  begin
    Result.CellSides := csLeftRight;
    if Adapter.GridLines in [glBoth, glHorizontal] then
      Result.CellSides := Result.CellSides + csTopBottom;
  end
  else
  begin
    ACellSides := CellSidesMap[Adapter.GridLines];
    if TcxGridColumn(ATableItem).IsTop and (ARecord <> nil) and (ARecord.Index = 0) then
      Include(ACellSides, csTop);
    if TcxGridColumn(ATableItem).IsMostRight then
      Include(ACellSides, csRight);
    if TcxGridColumn(ATableItem).IsMostLeft and (ARecord <> nil) and ARecord.Expandable then
      Exclude(ACellSides, csLeft); {3.1}
    Result.CellSides := ACellSides;
  end;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

procedure TdxGridTableViewFormatter.DoInitializeMasterIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer);
begin
  DoInitializeMasterDataRowIndent(AnIndent, AnIndex, AnIndentCount, nil); //Adapter.GroupedColumnCount + 1, nil); //PS3.1 changed
end;

procedure TdxGridTableViewFormatter.DoInitializePreview(APreview: TAbstractdxReportCellData;
  AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord);
begin
  DoInitializeItem(APreview, AColumn, ARecord, True);
end;

function TdxGridTableViewFormatter.GetPreviewClass(AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord): TdxReportCellDataClass;
begin
  Result := GetDataItemClass(AColumn, ARecord,  True);
end;

function TdxGridTableViewFormatter.GetPreviewViewParams(
  AColumn: TcxGridColumn; ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
begin
  Result := GetItemViewParams(AColumn, ARecord, True)
end;

procedure TdxGridTableViewFormatter.DoInitializeSeparatorRow(ARow: TdxReportCell);
begin
  ARow.CellSides := csLeftRight;
  ARow.Transparent := True;
end;

procedure TdxGridTableViewFormatter.DoInitializeRowSeparator(ASeparator: TAbstractdxReportCellData);
begin
  ASeparator.CellSides := csAll;
  ASeparator.Color := RowSeparatorColor;
  ASeparator.Transparent := IsColorTransparent(ASeparator.Color); //ReportLink.Transparent;
end;

function TdxGridTableViewFormatter.GetRowSeparatorClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellBox;
end;

function TdxGridTableViewFormatter.GetReportItem(ARow: TcxCustomGridRow;
  AColumn: TcxGridColumn): TdxReportVisualItem;
begin
  Result := ReportDataAdapter.ReportItems[ARow, AColumn];
end;

{ IdxPSCellParams2 }
function TdxGridTableViewFormatter.IdxPSCellParams2_GetPreviewMarginLeft: Integer;
begin
  Result := Adapter.PreviewLeftIndent;
end;

function TdxGridTableViewFormatter.IdxPSCellParams2_GetPreviewMarginRight: Integer;
begin
  Result := Adapter.PreviewRightIndent;
end;

function TdxGridTableViewFormatter.IdxPSCellParams2_GetPreviewMaxHeight: Integer;
begin
  if PreviewAutoHeight then
    Result := -1
  else
    Result := PreviewLineHeight * PreviewMaxLineCount;
end;

function TdxGridTableViewFormatter.IdxPSCellParams2_GetPreviewMaxLineCount: Integer;
begin
  Result := PreviewMaxLineCount;
end;

procedure TdxGridTableViewFormatter.AddHorizontalDelimiters;
var
  Origin, I: Integer;
  Item: TdxGridTableViewColumnPlace;
  R: TRect;
begin
  inherited;
  if ReportLink.OptionsPagination.Column then
  begin
    Origin := ViewWidthExtraBefore;
    for I := 0 to ColumnCount - 1 do
    begin
      Item := ItemPlaceController[Columns[I]];
      if Item <> nil then
      begin
        R := Item.HeaderCellBounds;
        ReportLink.AddHorizontalDelimiter(Origin + R.Left);
        ReportLink.AddHorizontalDelimiter(Origin + R.Right);
      end;
    end;
  end;
end;

procedure TdxGridTableViewFormatter.AfterBuilding;
begin
  if CanCellMerging then
    PerformCellMerging;
  inherited;
end;

procedure TdxGridTableViewFormatter.BeforeBuilding;
begin
  BuildItemLists;
  ItemPlaceController.Refresh;
  inherited;
end;

procedure TdxGridTableViewFormatter.Calculate;
begin
  inherited;
  ItemPlaceController.Calculate;
end;

procedure TdxGridTableViewFormatter.CalculateDataRowHeights;
var
  I, RowHeight, J, V: Integer;
  Row: TcxCustomGridRow;
  Column: TcxGridColumn;
begin
  FRowHeights.Count := RowCount;
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    if Row is TcxGridGroupRow then
      RowHeight := GroupRowLineHeight
    else
    begin
      RowHeight := DetailsLineHeight;
      if HeaderLineCount = 1 then
        for J := 0 to ColumnCount - 1 do
        begin
          Column := Columns[J];
          if not Column.IsPreview then
          begin
            V := DetailsLineHeight;
            Builder.DoGetCellHeight(Row, Column, V);
            RowHeight := Max(RowHeight, V);
          end;
        end
    end;
    RowHeights[I] := RowHeight;
  end;
end;

procedure TdxGridTableViewFormatter.CalculateLineHeights;
var
  I: Integer;
  Column: TcxGridColumn;
begin
  inherited;
  FGroupFooterLineHeight := ScaleFactor.Apply(DefaultGroupFooterLineHeight);
  if Adapter.GroupRowHeightAssigned then
    FGroupRowLineHeight := Adapter.GroupRowHeight
  else
  begin
    FGroupRowLineHeight := ScaleFactor.Apply(DefaultGroupRowLineHeight);
    if Adapter.IsOffice11StyleGrouping then {.2}
      FGroupRowLineHeight := 2 * FGroupRowLineHeight;
  end;

  FFooterLineHeight := Adapter.FooterHeight;
  FGroupFooterLineHeight := Adapter.GroupFooterHeight;
  FHeaderLineHeight := Adapter.HeaderHeight;

  if Adapter.DataRowHeightAssigned then
    FDetailsLineHeight := Adapter.DataRowHeight
  else
    FDetailsLineHeight := ScaleFactor.Apply(DefaultDataRowLineHeight);

  FPreviewLineHeight := ScaleFactor.Apply(DefaultDataRowLineHeight);

  for I := 0 to ColumnCount - 1 do
  begin
    Column := Columns[I];
    if not Adapter.HeaderHeightAssigned then
      CalculateHeight(GetHeaderItemViewParams(Column), FHeaderLineHeight);
    {if not Adapter.FooterHeightAssigned then
    begin
      CalculateHeight(GetGroupFooterItemViewParams(nil, -1, Column), FGroupFooterLineHeight);
      CalculateHeight(GetFooterItemViewParams(Column), FFooterLineHeight);
    end;}
    if not Adapter.FooterHeightAssigned then
      CalculateHeight(GetFooterItemViewParams(Column, nil), FFooterLineHeight);
    if not Adapter.GroupFooterHeightAssigned then
      CalculateHeight(GetGroupFooterItemViewParams(nil, -1, Column, nil), FGroupFooterLineHeight);
    if not Adapter.DataRowHeightAssigned and not Column.IsPreview then
      CalculateHeight(GetItemViewParams(Column, nil, False), FDetailsLineHeight);
  end;

  if not Adapter.GroupRowHeightAssigned then
    for I := 0 to RowCount - 1 do
      CalculateHeight(GetGroupRowViewParams(Rows[I], -1), FGroupRowLineHeight);

  if HasPreview then
    CalculateHeight(GetItemViewParams({PreviewColumn}nil, nil, True), FPreviewLineHeight);

  Inc(FFooterLineHeight, 2 * Adapter.ScaleFactor.Apply(FooterItemInflateVert) - Adapter.ScaleFactor.Apply(1));
  Inc(FGroupFooterLineHeight, 2 * Adapter.ScaleFactor.Apply(FooterItemInflateVert) - Adapter.ScaleFactor.Apply(1));

  CalculateDataRowHeights;
end;

function TdxGridTableViewFormatter.GetAlignSummaryWithColumns: Boolean;
begin
  Result := Adapter.GridView.OptionsView.GetGroupSummaryLayout <> gslStandard;
end;

function TdxGridTableViewFormatter.GetItemPlaceControllerClass: TdxCustomGridTableViewItemPlaceControllerClass;
begin
  Result := TdxGridTableViewColumnPlaceController;
end;

function TdxGridTableViewFormatter.MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AGridBackgroundBitmapIndex of
    bbFooter:
      Result := vspsGridFooter;
    bbGroup:
      Result := vspsGridGroup;
    bbHeader:
      Result := vspsGridHeader;
    bbPreview:
      Result := vspsGridPreview;
  else
    Result := 0;
  end;

  if Result = 0 then
    Result := inherited MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex);
end;


function TdxGridTableViewFormatter.CanCellMerging: Boolean;
begin
  Result := not HasPreview and (RowSeparatorThickness = 0);
end;

function TdxGridTableViewFormatter.CanColumnCellMerging(AColumn: TcxGridColumn): Boolean;
begin
  Result := AColumn.Options.CellMerging;
end;

procedure TdxGridTableViewFormatter.PerformCellMerging;
var
  I: Integer;
  Column: TcxGridColumn;
begin
  for I := 0 to ColumnCount - 1 do
  begin
    Column := Columns[I];
    if CanColumnCellMerging(Column) then // because Column.CanCellMerging also performs GridView.CellMerging validation
      PerformColumnCellMerging(Column);
  end;
end;

procedure TdxGridTableViewFormatter.PerformColumnCellMerging(AColumn: TcxGridColumn);

  function CanRowBeMergedAsMaster(ARow: TcxCustomGridRow): Boolean;
  begin
    Result := Adapter.RecordHelpers[ARow].CanCellMergingAsMaster;
  end;

  function CanRowBeMergedAsSlave(ARow: TcxCustomGridRow): Boolean;
  begin
    Result := Adapter.RecordHelpers[ARow].CanCellMergingAsSlave;
  end;

  function IsRowMergingSeparator(ARow: TcxCustomGridRow): Boolean;
  begin
    Result := Adapter.RecordHelpers[ARow].IsCellMergingSeparator;
  end;

  procedure RetrieveCellPropertiesAndValue(ARow: TcxCustomGridRow;
    out AProperties: TcxCustomEditProperties; out AValue: TcxEditValue);
  begin
    AProperties := GetItemProperties(AColumn, ARow);
    AValue := GetItemValue(AProperties, AColumn, ARow);
  end;

  function CompareCellsForMerging(ARow1: TcxGridDataRow; AProperties1: TcxCustomEditProperties;
    const AValue1: TcxEditValue; ARow2: TcxGridDataRow; AProperties2: TcxCustomEditProperties;
    const AValue2: TcxEditValue): Boolean;
  begin
    Result := GridColumn_DoCompareValuesForCellMerging(AColumn, ARow1, AProperties1, AValue1, ARow2, AProperties2, AValue2);
  end;

  procedure DoMergeCells(AStartRowIndex, AStopRowIndex: Integer);
  var
    AMasterReportItem: TdxReportVisualItem;
    AMasterRowIndex: Integer;
    ASlaveReportItem: TdxReportVisualItem;
    I: Integer;
  begin
    case GetItemProperties(AColumn, Rows[AStartRowIndex]).Alignment.Vert of
      taTopJustify:
        AMasterRowIndex := AStartRowIndex;
      taBottomJustify:
        AMasterRowIndex := AStopRowIndex;
    else
      AMasterRowIndex := (AStartRowIndex + AStopRowIndex) div 2;
    end;

    AMasterReportItem := GetReportItem(Rows[AMasterRowIndex], AColumn);
    for I := AStartRowIndex to AStopRowIndex do
    begin
      ASlaveReportItem := GetReportItem(Rows[I], AColumn);
      ASlaveReportItem.BackgroundBitmapIndex := AMasterReportItem.BackgroundBitmapIndex;
      if I <> AStopRowIndex then
        ASlaveReportItem.CellSides := ASlaveReportItem.CellSides - [csBottom];
      if I <> AStartRowIndex then
        ASlaveReportItem.CellSides := ASlaveReportItem.CellSides - [csTop];
      if ASlaveReportItem <> AMasterReportItem then
      begin
        ASlaveReportItem.Color := AMasterReportItem.Color;
        ASlaveReportItem.ContentBkColor := AMasterReportItem.ContentBkColor;
        ASlaveReportItem.ContentPattern := AMasterReportItem.ContentPattern;
        if ASlaveReportItem is TdxReportCellString then
          TdxReportCellString(ASlaveReportItem).Text := '';
        if ASlaveReportItem is TCustomdxReportCellImageContainer then
        begin
          TCustomdxReportCellImageContainer(ASlaveReportItem).Image := nil;
          TCustomdxReportCellImageContainer(ASlaveReportItem).ImageIndex := -1;
        end;
      end;
    end;
  end;

var
  AAreCellsMerged: Boolean;
  AHasCellsMerging: Boolean;
  AIsMergingSeparator: Boolean;
  AProperties1: TcxCustomEditProperties;
  AProperties2: TcxCustomEditProperties;
  ARow1: TcxCustomGridRow;
  ARow2: TcxCustomGridRow;
  AValue1: TcxEditValue;
  AValue2: TcxEditValue;
  I, J: Integer;
begin
  I := 0;
  while I < RowCount - 1 do
  begin
    AHasCellsMerging := False;
    ARow1 := Rows[I];
    if CanRowBeMergedAsMaster(ARow1) then
    begin
      RetrieveCellPropertiesAndValue(ARow1, AProperties1, AValue1);
      //AreCellsMerged := False;
      AIsMergingSeparator := False;
      J := I;
      repeat
        Inc(J);
        ARow2 := Rows[J];
        if CanRowBeMergedAsSlave(ARow2) then
        begin
          RetrieveCellPropertiesAndValue(ARow2, AProperties2, AValue2);
          AAreCellsMerged := CompareCellsForMerging(TcxGridDataRow(ARow1),
            AProperties1, AValue1, TcxGridDataRow(ARow2), AProperties2, AValue2);
          if not AHasCellsMerging then
            AHasCellsMerging := AAreCellsMerged;
          AIsMergingSeparator := IsRowMergingSeparator(ARow2);
        end
        else
          AAreCellsMerged := False;
      until (J = RowCount - 1) or not AAreCellsMerged or AIsMergingSeparator;

      if AHasCellsMerging then
      begin
        if not AAreCellsMerged then Dec(J);
        DoMergeCells(I, J);
        Inc(J);
      end;
      I := J;
    end
    else
      Inc(I);
  end;
end;


procedure TdxGridTableViewFormatter.BuildColumnList;
var
  I: Integer;
  Column: TcxGridColumn;
begin
  FColumns.Clear;
  for I := 0 to Adapter.ColumnCount - 1 do
  begin
    Column := Adapter.Columns[I];
    if IsColumnActuallyVisible(Column) then FColumns.Add(Column);
  end;
end;

procedure TdxGridTableViewFormatter.BuildItemLists;
begin
  BuildColumnList;
end;

function TdxGridTableViewFormatter.IsColumnActuallyVisible(AColumn: TcxGridColumn): Boolean;
begin
  Result := AColumn.ActuallyVisible;
end;

function TdxGridTableViewFormatter.GetViewWidth: Integer;
begin
  Result := ItemPlaceController.Width;
end;

function TdxGridTableViewFormatter.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(FColumns[Index]);
end;

function TdxGridTableViewFormatter.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TdxGridTableViewFormatter.GetGroupRowSeparatorColor: TColor;
begin
  Result := Adapter.GroupRowSeparatorColor;
end;

function TdxGridTableViewFormatter.GetGroupRowSeparatorThickness: Integer;
begin
  Result := Adapter.GroupRowSeparatorThickness;
end;

function TdxGridTableViewFormatter.GetHasPreview: Boolean;
begin
  Result := Adapter.HasPreview and ShowPreview and
    (PreviewAutoHeight or (PreviewMaxLineCount > 0));
end;

function TdxGridTableViewFormatter.GetHeaderLineCount: Integer;
begin
  Result := ItemPlaceController.HeaderLineCount;
end;

function TdxGridTableViewFormatter.GetPreviewAutoHeight: Boolean;
begin
  Result := ReportLink.OptionsPreview.AutoHeight;
end;

function TdxGridTableViewFormatter.GetPreviewColumn: TcxGridColumn;
begin
  Result := Adapter.PreviewColumn;
end;

function TdxGridTableViewFormatter.GetPreviewMaxLineCount: Integer;
begin
  Result := ReportLink.OptionsPreview.MaxLineCount;
end;

function TdxGridTableViewFormatter.GetRow(Index: Integer): TcxCustomGridRow;
begin
  Result := inherited Records[Index] as TcxCustomGridRow;
end;

function TdxGridTableViewFormatter.GetRowCount: Integer;
begin
  Result := RecordCount;
end;

function TdxGridTableViewFormatter.GetRowHeight(Index: Integer): Integer;
begin
  Result := Integer(FRowHeights[Index])
end;

function TdxGridTableViewFormatter.GetRowHeightByRow(Row: TcxCustomGridRow): Integer;
begin
  Result := {20;//}RowHeights[RecordIndexes[Row]];
end;

function TdxGridTableViewFormatter.GetRowSeparatorColor: TColor;
begin
  Result := Adapter.RowSeparatorColor;
  {with ReportLink.OptionsFormatting do
    if UseNativeStyles then
    begin
      Result := FixedColor;
    end;}
end;

function TdxGridTableViewFormatter.GetRowSeparatorThickness: Integer;
begin
  Result := Adapter.RowSeparatorThickness;
end;

function TdxGridTableViewFormatter.GetShowGroupFooters: Boolean;
begin
  Result := ReportLink.OptionsView.GroupFooters;
end;

function TdxGridTableViewFormatter.GetShowFooters: Boolean;
begin
  Result := ReportLink.OptionsView.Footers;
end;

function TdxGridTableViewFormatter.GetShowHeaders: Boolean;
begin
  Result := ReportLink.OptionsView.Headers;
end;

function TdxGridTableViewFormatter.GetShowPreview: Boolean;
begin
  Result := ReportLink.OptionsPreview.Visible;
end;

procedure TdxGridTableViewFormatter.SetRowHeight(Index: Integer; Value: Integer);
begin
  FRowHeights[Index] := TObject(Value);
end;

procedure TdxGridTableViewFormatter.SetRowHeightByRow(Row: TcxCustomGridRow; Value: Integer);
begin
  RowHeights[IndexOfRecord(Row)] := Value;
end;

{ TdxGridTableViewBuilder }

function TdxGridTableViewBuilder.Adapter: TdxGridTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridTableViewAdapter;
end;

class function TdxGridTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridTableViewAdapter;
end;

function TdxGridTableViewBuilder.Formatter: TdxGridTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridTableViewFormatter;
end;

class function TdxGridTableViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridTableViewFormatter;
end;

procedure TdxGridTableViewBuilder.AddDelimitersHardVert(AReportRow: TdxReportCell);
begin
  ReportLink.AddVerticalHardDelimiter(AReportRow);
end;

procedure TdxGridTableViewBuilder.DoBuildViewBody;
begin
  if Formatter.ShowHeaders then
    CreateHeader;
  CreateRows;
  if not IsAborted and Formatter.ShowFooters then
    CreateFooter;
end;

procedure TdxGridTableViewBuilder.DoBuildViewFooter;
begin
  inherited DoBuildViewFooter;
  if ReportLink.OptionsLevels.Unwrap and not Formatter.HasDetailsSeparator then
    CreateViewTerminator;
end;

procedure TdxGridTableViewBuilder.CreateDetails(AMasterRow: TcxGridMasterDataRow);
var
  ACurrentView: TcxCustomGridView;
begin
  AMasterRow.GridView.BeginUpdate;
  try
    if AMasterRow.ActiveDetailGridView <> nil then
      if ReportLink.OptionsLevels.Unwrap then
      begin
        ProcessedView := nil;
        try
          if ReportLink.OptionsLevels.RiseActiveLevelOntoTop then
          begin
            ACurrentView := AMasterRow.ActiveDetailGridView;
            CreateDetailView(ACurrentView);
            ProcessedView := ACurrentView;
          end;
          ReportLink.ForEachView(AMasterRow, CreateDetailView);
        finally
          ProcessedView := nil;
        end;
      end
      else
        CreateDetailView(AMasterRow.ActiveDetailGridView);
  finally
    AMasterRow.GridView.EndUpdate; //Q276990 need for TcxGridDBDataController.DataModeController.DetailInSQLMode = true
  end;
end;

procedure TdxGridTableViewBuilder.CreateDetailView(AGridView: TcxCustomGridView);
begin
  if AGridView <> ProcessedView then
    ReportLink.BuildView(Self, AGridView);
end;

function TdxGridTableViewBuilder.GetAreDetailsBuilt: Boolean;
begin
  with ReportLink.OptionsDetails do
    Result := not StartFromFocusedView or not OnlyFocusedView;
end;

procedure TdxGridTableViewBuilder.CreateFooter;
begin
  AddReportRow(GetFootersProducer.Produce(HostInfoServices.FootersHostInfo));
end;

function TdxGridTableViewBuilder.GetFootersProducer: TdxGridTableViewFootersProducer;
begin
  Result := ProducerCache[GetFootersProducerClass] as TdxGridTableViewFootersProducer;
end;

function TdxGridTableViewBuilder.GetFootersProducerClass: TdxGridTableViewFootersProducerClass;
begin
  Result := TdxGridTableViewFootersProducer;
end;

procedure TdxGridTableViewBuilder.CreateGroupFooters(AGridRow: TcxCustomGridRow);
var
  Producer: TdxGridTableViewGroupFooterProducer;
  HostInfo: TdxGridAttributeHostInfo;
  Index, I: Integer;
begin
  Producer := GetGroupFooterProducer;
  HostInfo := HostInfoServices.PageDetailsHostInfo;
  Index := 0;
  for I := 0 to AGridRow.Level do
    if Adapter.HasGroupFooter(AGridRow, I) then
    begin
      AddReportRow(Producer.Produce(HostInfo, AGridRow, AGridRow.Level - I, Index));
      Inc(Index);
    end;
end;

function TdxGridTableViewBuilder.GetGroupFooterProducer: TdxGridTableViewGroupFooterProducer;
begin
  Result := ProducerCache[GetGroupFooterProducerClass] as TdxGridTableViewGroupFooterProducer;
end;

function TdxGridTableViewBuilder.GetGroupFooterProducerClass: TdxGridTableViewGroupFooterProducerClass;
begin
  Result := TdxGridTableViewGroupFooterProducer;
end;

procedure TdxGridTableViewBuilder.CreateHeader;
begin
  AddReportRow(GetHeadersProducer.Produce(HostInfoServices.HeadersHostInfo));
end;

function TdxGridTableViewBuilder.GetHeadersProducer: TdxGridTableViewHeadersProducer;
begin
  Result := ProducerCache[GetHeadersProducerClass] as TdxGridTableViewHeadersProducer;
end;

function TdxGridTableViewBuilder.GetHeadersProducerClass: TdxGridTableViewHeadersProducerClass;
begin
  Result := TdxGridTableViewHeadersProducer;
end;

procedure TdxGridTableViewBuilder.CreateRow(AGridRow: TcxCustomGridRow; ARowIndex: Integer);
begin
  AddReportRow(GetRowProducer(AGridRow).Produce(
    HostInfoServices.PageDetailsHostInfo, AGridRow, ARowIndex));
end;

function TdxGridTableViewBuilder.GetRowProducer(AGridRow: TcxCustomGridRow): TdxGridTableViewCustomDataRowProducer;
begin
  Result := ProducerCache[GetRowProducerClass(AGridRow)] as TdxGridTableViewCustomDataRowProducer;
end;

procedure TdxGridTableViewBuilder.CreateRows;
var
  PrevGridRow, GridRow: TcxCustomGridRow;
  I: Integer;
  HasGroupDelimiter: Boolean;
begin
  PrevGridRow := nil;
  for I := 0 to Formatter.RowCount - 1 do
  begin
    GridRow := Formatter.Rows[I];

    if (ShowRowSeparators and (I > 0)) and (not (PrevGridRow is TcxGridGroupRow) or not ShowGroupRowSeparators) then
      CreateRowSeparator(GridRow, False);

    HasGroupDelimiter := Formatter.PaginateByTopLevelGroups and (I > 0) and
      (GridRow.Level = 0) and (GridRow is TcxGridGroupRow);

    CreateRow(GridRow, I);

    // Hard Delimiters
    if HasGroupDelimiter then
      AddDelimitersHardVert(LastReportRow);

    if AreDetailsBuilt and Adapter.HasDetails(GridRow) then
      CreateDetails(GridRow as TcxGridMasterDataRow);

    if ShowGroupRowSeparators and (GridRow is TcxGridGroupRow) then
      CreateGroupRowSeparator(TcxGridGroupRow(GridRow));

    if Formatter.ShowGroupFooters and Adapter.HasGroupFooters(GridRow) then
      CreateGroupFooters(GridRow);

    // Last Row Separator
    if (ShowRowSeparators and (I = Formatter.RowCount - 1)) and (not (GridRow is TcxGridGroupRow) or not ShowGroupRowSeparators) then
      CreateRowSeparator(GridRow, True);

    // Progress
    if MasterBuilder = nil then
    begin
      Progress(100 * (I + 1) / Formatter.RowCount);
      if IsAborted then Break;
    end;

    PrevGridRow := GridRow;
  end;
end;

function TdxGridTableViewBuilder.GetRowProducerClass(AGridRow: TcxCustomGridRow): TdxGridTableViewCustomDataRowProducerClass;
var
  AProducerClass: TdxGridViewRowProducerClass;
begin
  AProducerClass := Adapter.RecordHelpers[AGridRow].ProducerClass;
  if (AProducerClass = nil) or not AProducerClass.InheritsFrom(TdxGridTableViewCustomDataRowProducer) then
    raise EdxException.Create('TdxGridTableViewBuilder.GetRowProducerClass');
  Result := TdxGridTableViewCustomDataRowProducerClass(AProducerClass);
end;

procedure TdxGridTableViewBuilder.CreateRowSeparator(AGridRow: TcxCustomGridRow;
  AnIsLast: Boolean = False);
begin
  AddReportRow(GetRowSeparatorProducer.Produce(
    HostInfoServices.PageDetailsHostInfo, AGridRow, AnIsLast));
end;

function TdxGridTableViewBuilder.GetRowSeparatorProducer: TdxGridTableViewRowSeparatorProducer;
begin
  Result := ProducerCache[GetRowSeparatorProducerClass] as TdxGridTableViewRowSeparatorProducer;
end;

function TdxGridTableViewBuilder.GetRowSeparatorProducerClass: TdxGridTableViewRowSeparatorProducerClass;
begin
  Result := TdxGridTableViewRowSeparatorProducer;
end;

function TdxGridTableViewBuilder.GetShowRowSeparators: Boolean;
begin
  Result := Adapter.HasRowSeparators;
end;

procedure TdxGridTableViewBuilder.CreateGroupRowSeparator(AGridRow: TcxGridGroupRow);
begin
  AddReportRow(GetGroupRowSeparatorProducer.Produce(
    HostInfoServices.PageDetailsHostInfo, AGridRow));
end;

function TdxGridTableViewBuilder.GetGroupRowSeparatorProducer: TdxGridTableViewGroupRowSeparatorProducer;
begin
  Result := ProducerCache[GetGroupRowSeparatorProducerClass] as TdxGridTableViewGroupRowSeparatorProducer;
end;

function TdxGridTableViewBuilder.GetGroupRowSeparatorProducerClass: TdxGridTableViewGroupRowSeparatorProducerClass;
begin
  Result := TdxGridTableViewGroupRowSeparatorProducer;
end;

function TdxGridTableViewBuilder.GetShowGroupRowSeparators: Boolean;
begin
  Result := Adapter.IsOffice11StyleGrouping;
end;

function TdxGridTableViewBuilder.GridView: TcxGridTableView;
begin
  Result := inherited GridView as TcxGridTableView;
end;

class function TdxGridTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridTableView;
end;

function TdxGridTableViewBuilder.GetLastReportRow: TdxReportCell;
begin
  Result := ReportRows[ReportRowCount - 1];
end;

function TdxGridTableViewBuilder.GetProcessedView: TcxCustomGridView;
begin
  Result := ReportLink.FProcessedView;
end;

procedure TdxGridTableViewBuilder.SetProcessedView(Value: TcxCustomGridView);
begin
  ReportLink.FProcessedView := Value;
end;

{ TdxGridDBTableViewAdapter }

function TdxGridDBTableViewAdapter.GridView: TcxGridDBTableView;
begin
  Result := inherited GridView as TcxGridDBTableView;
end;

class function TdxGridDBTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBTableView;
end;

function TdxGridDBTableViewAdapter.DataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(inherited DataController);
end;

function TdxGridDBTableViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := DataController.DataModeController;
end;

{ TdxGridDBTableViewBuilder }

function TdxGridDBTableViewBuilder.Adapter: TdxGridDBTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridDBTableViewAdapter;
end;

class function TdxGridDBTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridDBTableViewAdapter;
end;

class function TdxGridDBTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBTableView;
end;

{ TcxGridServerModeTableViewAdapter }

function TcxGridServerModeTableViewAdapter.GridView: TcxGridServerModeTableView;
begin
  Result := inherited GridView as TcxGridServerModeTableView;
end;

class function TcxGridServerModeTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeTableView;
end;

function TcxGridServerModeTableViewAdapter.DataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TcxGridServerModeTableViewBuilder }

function TcxGridServerModeTableViewBuilder.Adapter: TcxGridServerModeTableViewAdapter;
begin
  Result := inherited Adapter as TcxGridServerModeTableViewAdapter;
end;

class function TcxGridServerModeTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TcxGridServerModeTableViewAdapter;
end;

class function TcxGridServerModeTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeTableView;
end;

{ TdxGridBandedTableViewAdapter }

constructor TdxGridBandedTableViewAdapter.Create(AMasterAdapter: TdxCustomGridViewAdapter;
  AGridView: TcxCustomGridView);
begin
  inherited;
  FFooterLineCount := -1;
end;

function TdxGridBandedTableViewAdapter.GridView: TcxGridBandedTableView;
begin
  Result := inherited GridView as TcxGridBandedTableView;
end;

class function TdxGridBandedTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

function TdxGridBandedTableViewAdapter.Styles: TcxGridBandedTableViewStyles;
begin
  Result := inherited Styles as TcxGridBandedTableViewStyles;
end;

function TdxGridBandedTableViewAdapter.CalculateDetailsLineCount: Integer;
var
  ABandRows: TcxGridBandRows;
  AValue: Integer;
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to BottomBandCount - 1 do
  begin
    ABandRows := BottomBands[I].Rows;
    AValue := 0;
    for J := 0 to ABandRows.VisibleCount - 1 do
      Inc(AValue, ABandRows[J].LineCount);
    Result := Max(Result, AValue);
  end;
end;

function TdxGridBandedTableViewAdapter.CalculateFooterLineCount: Integer;
begin
  Result := CalculateDetailsLineCount;
end;

function TdxGridBandedTableViewAdapter.GetBandHeaderHeight: Integer;
begin
  Result := GridView.OptionsView.BandHeaderHeight;
end;

function TdxGridBandedTableViewAdapter.GetBandHeaderHeightAssigned: Boolean;
begin
  Result := BandHeaderHeight <> 0;
end;

function TdxGridBandedTableViewAdapter.GetBandHeaderViewParams(ABand: TcxGridBand): TcxViewParams;
begin
  if (ABand <> nil) and (ABand.Styles <> nil) then
    ABand.Styles.GetHeaderParams(Result)
  else
    Styles.GetBandHeaderParams(ABand, Result);
  if ABand = nil then
    Result.Color := LookAndFeelPainter.DefaultHeaderBackgroundColor
end;

function TdxGridBandedTableViewAdapter.GetDataRowHeightAssigned: Boolean;
begin
  Result := (DetailsLineCount = 1) and inherited GetDataRowHeightAssigned;
end;

function TdxGridBandedTableViewAdapter.GetFooterHeightAssigned: Boolean;
begin
  Result := (FooterLineCount = 1) and inherited GetFooterHeightAssigned;
end;

function TdxGridBandedTableViewAdapter.GetHeaderHeightAssigned: Boolean;
begin
  Result := (DetailsLineCount = 1) and inherited GetHeaderHeightAssigned;
end;

function TdxGridBandedTableViewAdapter.GetHeaderMultiline(Column: TcxGridColumn): Boolean;
begin
  Result := ((DetailsLineCount = 1) and inherited GetHeaderMultiline(Column)) or
    (TcxGridBandedColumn(Column).Position.LineCount > 1); {PS 3.01}
end;

function TdxGridBandedTableViewAdapter.GetRowLineCount: Integer;
begin
  Result := CalculateFooterLineCount;
end;

function TdxGridBandedTableViewAdapter.GetBand(Index: Integer): TcxGridBand;
begin
  Result := GridView.Bands.VisibleItems[Index];
end;

function TdxGridBandedTableViewAdapter.GetBandCount: Integer;
begin
  Result := GridView.Bands.VisibleCount;
end;

function TdxGridBandedTableViewAdapter.GetBandEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.BandHeaderEndEllipsis;
end;

function TdxGridBandedTableViewAdapter.GetBandHeaderLineCount: Integer;
begin
  Result := GridView.OptionsView.BandHeaderLineCount;
end;

function TdxGridBandedTableViewAdapter.GetBandHeaderMultiline: Boolean;
begin
  Result := BandHeaderHeightAssigned;// or (BandHeaderLineCount > 1); {!!! v3.03}
end;

function TdxGridBandedTableViewAdapter.GetBottomBand(Index: Integer): TcxGridBand;
begin
  Result := GridView.Bands.VisibleBottomItems[Index];
end;

function TdxGridBandedTableViewAdapter.GetBottomBandCount: Integer;
begin
  Result := GridView.Bands.VisibleBottomItemCount;
end;

function TdxGridBandedTableViewAdapter.GetColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := GridView.VisibleColumns[Index];
end;

function TdxGridBandedTableViewAdapter.GetFooterLineCount: Integer;
begin
  if FFooterLineCount = -1 then
    FFooterLineCount := CalculateFooterLineCount;
  Result := FFooterLineCount;
end;

function TdxGridBandedTableViewAdapter.GetRootBand(Index: Integer): TcxGridBand;
begin
  Result := GridView.Bands.VisibleRootItems[Index];
end;

function TdxGridBandedTableViewAdapter.GetRootBandCount: Integer;
begin
  Result := GridView.Bands.VisibleRootItemCount;
end;

function TdxGridBandedTableViewAdapter.GetThemedBandHeaderItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

function TdxGridBandedTableViewAdapter.GetThemedBandHeaderItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderTextColor;
end;

{ TdxGridBandedTableViewColumnPlace }

function TdxGridBandedTableViewColumnPlace.GetLineCount: Integer;
begin
  Result := Column.Position.LineCount;
end;

function TdxGridBandedTableViewColumnPlace.GetRowIndex: Integer;
begin
  Result := Column.Position.Row.LineOffset;
end;

function TdxGridBandedTableViewColumnPlace.GetColumn: TcxGridBandedColumn;
begin
  Result := inherited Column as TcxGridBandedColumn;
end;

function TdxGridBandedTableViewColumnPlace.GetController: TdxCustomGridBandedTableViewItemPlace;
begin
  Result := inherited Controller as TdxCustomGridBandedTableViewItemPlace;
end;

function TdxGridBandedTableViewColumnPlace.GetFormatter: TdxGridBandedTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridBandedTableViewFormatter;
end;

{ TdxCustomGridBandedTableViewItemPlace }

constructor TdxCustomGridBandedTableViewItemPlace.Create(AController: TdxGridBandedTableViewItemPlaceController;
  AParent: TdxGridBandedTableViewItemPlace; ABand: TcxGridBand);
begin
  inherited Create(AController.Formatter);
  FController := AController;
  FParent := AParent;
  FBand := ABand;

  FHeight := -1;
  FLeftBound := -1;
  FMinWidth := -1;
  FTopBound := -1;
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateHeaderLineCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  with Band.Rows do
    for I := 0 to VisibleCount - 1 do
      Inc(Result, VisibleItems[I].LineCount);
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateWidth: Integer;

  function FixedSize: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Band.ColumnCount - 1 do
      Result := Result or not Band.Columns[I].Options.HorzSizing;
  end;

begin
  if FixedSize then
    Result := Max(Band.Width, InternalCalculateWidth)
  else
  begin
    Result := Band.Width;
    if Result = 0 then
      Result := InternalCalculateWidth
  end;
  if Result = 0 then
     Result := cxGridBandedTableView.cxGridDefaultEmptyBandWidth;
  if Result < InternalCalculateMinWidth then
    Result := InternalCalculateMinWidth;
end;

function TdxCustomGridBandedTableViewItemPlace.GetItemClass(AColumn: TcxGridColumn): TdxGridTableViewColumnPlaceClass;
begin
  Result := TdxGridBandedTableViewColumnPlace;
end;

procedure TdxCustomGridBandedTableViewItemPlace.AssignWidth;
begin
  Width := Width;
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateHeight: Integer;
begin
  Result := Controller.CalculateItemHeight(Self);
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateLevelHeight: Integer;
begin
  if Adapter.BandHeaderHeightAssigned then
    Result := Adapter.BandHeaderHeight
  else
    Result := CalculateLineHeight * Adapter.BandHeaderLineCount;
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateLineHeight: Integer;
begin
  Result := 0;
  Formatter.CalculateHeight(ViewParams, Result);
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateMinWidth: Integer;
begin
  Result := InternalCalculateMinWidth;
  if Result < Band.MinWidth then Result := Band.MinWidth;
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateLeftBound: Integer;
begin
  Result := Controller.CalculateItemLeftBound(Self);
end;

function TdxCustomGridBandedTableViewItemPlace.CalculateTopBound: Integer;
begin
  Result := Controller.CalculateItemTopBound(Self);
end;

function TdxCustomGridBandedTableViewItemPlace.GetRowCount: Integer;
begin
  Result := 0;
end;

function TdxCustomGridBandedTableViewItemPlace.InternalCalculateMinWidth: Integer;
begin
  Result := 0;
end;

function TdxCustomGridBandedTableViewItemPlace.InternalCalculateWidth: Integer;
begin
  Result := 0;
end;

procedure TdxCustomGridBandedTableViewItemPlace.InitAutoWidthItem(AnItem: TcxAutoWidthItem);
begin
  AnItem.Fixed := IsFixed;
  AnItem.MinWidth := MinWidth;
  AnItem.Width := Width;
end;

function TdxCustomGridBandedTableViewItemPlace.GetAdapter: TdxGridBandedTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridBandedTableViewAdapter;
end;

function TdxCustomGridBandedTableViewItemPlace.GetBounds: TRect;
begin
  Result := Types.Bounds(LeftBound, TopBound, Width, Height);
end;

function TdxCustomGridBandedTableViewItemPlace.GetFormatter: TdxGridBandedTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridBandedTableViewFormatter;
end;

function TdxCustomGridBandedTableViewItemPlace.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TdxCustomGridBandedTableViewItemPlace.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.IndexOf(Self)
  else
    Result := Controller.RootIndexOf(Self);
end;

function TdxCustomGridBandedTableViewItemPlace.GetIsFixed: Boolean;
begin
  Result := not Band.Options.Sizing;
end;

function TdxCustomGridBandedTableViewItemPlace.GetLeftBound: Integer;
begin
  if FLeftBound = -1 then
    FLeftBound := CalculateLeftBound;
  Result := FLeftBound;
end;

function TdxCustomGridBandedTableViewItemPlace.GetLevelIndex: Integer;
begin
  Result := Band.BandLevelIndex;
end;

function TdxCustomGridBandedTableViewItemPlace.GetMinWidth: Integer;
begin
  if FMinWidth = -1 then
    FMinWidth := CalculateMinWidth;
  Result := FMinWidth;
end;

function TdxCustomGridBandedTableViewItemPlace.GetTopBound: Integer;
begin
  if FTopBound = -1 then
    FTopBound := CalculateTopBound;
  Result := FTopBound;
end;

function TdxCustomGridBandedTableViewItemPlace.GetViewParams: TdxReportItemViewParams;
begin
  Result := Formatter.GetBandItemViewParams(Band);
end;

{ TdxGridBandedTableViewItemPlace }

constructor TdxGridBandedTableViewItemPlace.Create(AnOwner: TdxGridBandedTableViewItemPlaceController;
  AParent: TdxGridBandedTableViewItemPlace; ABand: TcxGridBand);
begin
  inherited;
  FChildItems := TList.Create;
end;

destructor TdxGridBandedTableViewItemPlace.Destroy;
begin
  FreeAndNil(FChildItems);
  inherited;
end;

function TdxGridBandedTableViewItemPlace.IndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
begin
  Result := FChildItems.IndexOf(AnItem);
end;

procedure TdxGridBandedTableViewItemPlace.Refresh;
begin
  inherited;
  RefreshChildItems;
end;

function TdxGridBandedTableViewItemPlace.GetRowCount: Integer;
begin
  Result := 1;
end;

function TdxGridBandedTableViewItemPlace.InternalCalculateMinWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ChildItemCount - 1 do
    Inc(Result, ChildItems[I].MinWidth);
end;

function TdxGridBandedTableViewItemPlace.InternalCalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ChildItemCount - 1 do
    Inc(Result, ChildItems[I].Width);
end;

procedure TdxGridBandedTableViewItemPlace.WidthChanged;
begin
  inherited;
  CalculateChildItemWidths;
end;

procedure TdxGridBandedTableViewItemPlace.CalculateChildItemWidths;
var
  AutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AutoWidthObject := TcxAutoWidthObject.Create(ChildItemCount);
  try
    for I := 0 to ChildItemCount - 1 do
      ChildItems[I].InitAutoWidthItem(AutoWidthObject.AddItem);
    AutoWidthObject.AvailableWidth := Width;
    AutoWidthObject.Calculate;

    for I := 0 to ChildItemCount - 1 do
      ChildItems[I].Width := AutoWidthObject[I].AutoWidth;
  finally
    AutoWidthObject.Free;
  end;
end;

procedure TdxGridBandedTableViewItemPlace.RefreshChildItems;
var
  I: Integer;
begin
  FChildItems.Count := Band.VisibleChildBandCount;
  for I := 0 to FChildItems.Count - 1 do
  begin
    FChildItems[I] := Controller.ItemsByBand[Band.VisibleChildBands[I]];
    ChildItems[I].FParent := Self;
  end;
end;

function TdxGridBandedTableViewItemPlace.GetChildItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
begin
  Result := TdxCustomGridBandedTableViewItemPlace(FChildItems[Index]);
end;

function TdxGridBandedTableViewItemPlace.GetChildItemCount: Integer;
begin
  Result := FChildItems.Count;
end;

{ TdxGridBandedTableViewBottomItemPlace }

procedure TdxGridBandedTableViewBottomItemPlace.AddItems;
var
  I, J: Integer;
  Row: TcxGridBandRow;
begin
  for I := 0 to Band.Rows.VisibleCount - 1 do
  begin
    Row := Band.Rows.VisibleItems[I];
    for J := 0 to Row.VisibleCount - 1 do
      CreateItem(Row.VisibleItems[J]);
  end;
end;

function TdxGridBandedTableViewBottomItemPlace.CalculateItemLeftBound(AnItem: TdxGridTableViewColumnPlace): Integer;
var
  Row: TcxGridBandRow;
  I: Integer;
  Item: TdxGridTableViewColumnPlace;
begin
  Result := LeftBound;
  Row := TdxGridBandedTableViewColumnPlace(AnItem).Column.Position.Row;
  for I := 0 to TdxGridBandedTableViewColumnPlace(AnItem).Column.Position.VisibleColIndex - 1 do
  begin
    Item := ItemsByColumn[Row.VisibleItems[I]];
    Inc(Result, Item.Width);
  end;
end;

procedure TdxGridBandedTableViewBottomItemPlace.CalculateItemsWidth;
var
  I, J: Integer;
  Row: TcxGridBandRow;
  AutoWidthObject: TcxAutoWidthObject;
  Column: TcxGridColumn;
begin
  for I := 0 to Band.Rows.VisibleCount - 1 do
  begin
    Row := Band.Rows.VisibleItems[I];
    AutoWidthObject := TcxAutoWidthObject.Create(Row.VisibleCount);
    try
      for J := 0 to Row.VisibleCount - 1 do
        ItemsByColumn[Row.VisibleItems[J]].InitAutoWidthItem(AutoWidthObject.AddItem);
      AutoWidthObject.AvailableWidth := ItemsAvailableWidth;
      AutoWidthObject.Calculate;

      for J := 0 to Row.VisibleCount - 1 do
      begin
        Column := Row.VisibleItems[J];
        ItemsByColumn[Column].Width := AutoWidthObject[J].AutoWidth;
      end;
    finally
      AutoWidthObject.Free;
    end;
  end;
end;

function TdxGridBandedTableViewBottomItemPlace.GetItemsAutoWidth: Boolean;
begin
  Result := True;
end;

function TdxGridBandedTableViewBottomItemPlace.GetItemsAvailableWidth: Integer;
begin
  Result := Width;
end;

function TdxGridBandedTableViewBottomItemPlace.GetRowCount: Integer;
begin
  Result := Controller.LevelCount - LevelIndex;
end;

function TdxGridBandedTableViewBottomItemPlace.InternalCalculateMinWidth: Integer;
var
  I, V, J: Integer;
  Row: TcxGridBandRow;
  Column: TcxGridColumn;
begin
  Result := 0;
  for I := 0 to Band.Rows.VisibleCount - 1 do
  begin
    V := 0;
    Row := Band.Rows.VisibleItems[I];
    for J := 0 to Row.VisibleCount - 1 do
    begin
      Column := Row.VisibleItems[J];
      Inc(V, Adapter.InternalColumnMinWidths[Column]);
    end;
    //if Row.IsFirst then
    //  Inc(V, Formatter.IndentWidth * Formatter.IndentCount);
    if Result < V then Result := V;
  end
end;

function TdxGridBandedTableViewBottomItemPlace.InternalCalculateWidth: Integer;
var
  I, V, J: Integer;
  Row: TcxGridBandRow;
  Column: TcxGridColumn;
begin
  Result := 0;
  for I := 0 to Band.Rows.VisibleCount - 1 do
  begin
    V := 0;
    Row := Band.Rows.VisibleItems[I];
    for J := 0 to Row.VisibleCount - 1 do
    begin
      Column := Row.VisibleItems[J];
      Inc(V, Adapter.InternalColumnWidths[Column]);
    end;
    if Result < V then Result := V;
  end;
end;

procedure TdxGridBandedTableViewBottomItemPlace.WidthChanged;
begin
  inherited;
  CalculateItemsWidth;
end;

{ TdxGridBandedTableViewItemPlaceController }

constructor TdxGridBandedTableViewItemPlaceController.Create(AFormatter: TdxGridTableViewFormatter);
begin
  inherited;
  FBottomItems := TList.Create;
  FItems := TList.Create;
  FRootItems := TList.Create;
  FLevelHeights := TList.Create;

  FHeight := -1;
  FLevelCount := -1;
end;

destructor TdxGridBandedTableViewItemPlaceController.Destroy;
begin
  FreeAndNil(FLevelHeights);
  FreeAndNil(FRootItems);
  FreeAndNilItems;
  FreeAndNil(FBottomItems);
  inherited;
end;

procedure TdxGridBandedTableViewItemPlaceController.Calculate;
begin
  CalculateLevelHeights;
  CalculateItemWidths;
end;

procedure TdxGridBandedTableViewItemPlaceController.Refresh;
begin
  ClearItems;
  AddItems;
  RefreshRootItems;
  RefreshBottomItems;
  RefreshItems;
end;

function TdxGridBandedTableViewItemPlaceController.IndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
begin
  Result := FItems.IndexOf(AnItem);
end;

function TdxGridBandedTableViewItemPlaceController.IndexOf(ABand: TcxGridBand): Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Band = ABand then
      Exit;
  Result := -1;
end;

function TdxGridBandedTableViewItemPlaceController.RootIndexOf(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
begin
  Result := FRootItems.IndexOf(AnItem);
end;

function TdxGridBandedTableViewItemPlaceController.RootIndexOf(ABand: TcxGridBand): Integer;
begin
  for Result := 0 to RootItemCount - 1 do
    if RootItems[Result].Band = ABand then
      Exit;
  Result := -1;
end;

function TdxGridBandedTableViewItemPlaceController.CalculateHeaderLineCount: Integer;
var
  I, V: Integer;
begin
  Result := -1;
  for I := 0 to BottomItemCount - 1 do
  begin
    V := BottomItems[I].HeaderLineCount;
    if Result < V then Result := V;
  end;
end;

function TdxGridBandedTableViewItemPlaceController.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RootItemCount - 1 do
    Inc(Result, RootItems[I].Width);
end;

function TdxGridBandedTableViewItemPlaceController.GetItemByColumn(Column: TcxGridColumn): TdxGridTableViewColumnPlace;
var
  BandPlace: TdxCustomGridBandedTableViewItemPlace;
begin
  BandPlace := ItemsByBand[TcxGridBandedColumn(Column).Position.Band];
  if BandPlace <> nil then
    Result := BandPlace.ItemsByColumn[Column]
  else
    Result := nil;
end;

procedure TdxGridBandedTableViewItemPlaceController.AddItems;
var
  I: Integer;
begin
  FItems.Count := Formatter.BandCount;
  for I := 0 to ItemCount - 1 do
    FItems[I] := CreateItem(Formatter.Bands[I]);
end;

function TdxGridBandedTableViewItemPlaceController.CalculateHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LevelCount - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TdxGridBandedTableViewItemPlaceController.CalculateItemHeight(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AnItem.LevelIndex to AnItem.LevelIndex + AnItem.RowCount - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TdxGridBandedTableViewItemPlaceController.CalculateItemLeftBound(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
var
  I: Integer;
begin
  if AnItem.Parent <> nil then
  begin
    Result := AnItem.Parent.LeftBound;
    for I := 0 to AnItem.Index - 1 do
      Inc(Result, AnItem.Parent.ChildItems[I].Width);
  end
  else
  begin
    Result := 0;
    for I := 0 to AnItem.Index - 1 do
      Inc(Result, RootItems[I].Width);
  end;
end;

function TdxGridBandedTableViewItemPlaceController.CalculateItemTopBound(AnItem: TdxCustomGridBandedTableViewItemPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AnItem.LevelIndex - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TdxGridBandedTableViewItemPlaceController.CalculateLevelCount: Integer;
var
  I, V: Integer;
begin
  Result := 0;
  if ItemCount <> 0 then
  begin
    for I := 0 to ItemCount - 1 do
    begin
      V := Items[I].LevelIndex;
      if Result < V then Result := V;
    end;
    Inc(Result);
  end;
end;

procedure TdxGridBandedTableViewItemPlaceController.CalculateItemWidths;
var
  I: Integer;
  AutoWidthObject: TcxAutoWidthObject;
begin
  for I := 0 to RootItemCount - 1 do
    RootItems[I].AssignWidth;

  if AutoWidth then
  begin
    AutoWidthObject := TcxAutoWidthObject.Create(RootItemCount);
    try
      for I := 0 to RootItemCount - 1 do
        RootItems[I].InitAutoWidthItem(AutoWidthObject.AddItem);
      AutoWidthObject.AvailableWidth := AvailableWidth;
      AutoWidthObject.Calculate;

      for I := 0 to RootItemCount - 1 do
        RootItems[I].Width := AutoWidthObject[I].AutoWidth;
    finally
      AutoWidthObject.Free;
    end;
  end;
end;

procedure TdxGridBandedTableViewItemPlaceController.CalculateLevelHeights;
var
  I, V: Integer;
  Item: TdxCustomGridBandedTableViewItemPlace;
begin
  FLevelHeights.Count := LevelCount;
  for I := 0 to ItemCount - 1 do
  begin
    Item := Items[I];
    V := Item.CalculateLevelHeight;
    if LevelHeights[Item.LevelIndex] < V then
      LevelHeights[Item.LevelIndex] := V;
  end;
end;

procedure TdxGridBandedTableViewItemPlaceController.ClearItems;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxGridBandedTableViewItemPlaceController.CreateItem(ABand: TcxGridBand): TdxCustomGridBandedTableViewItemPlace;
begin
  Result := GetItemClass(ABand).Create(Self, nil, ABand);
end;

function TdxGridBandedTableViewItemPlaceController.GetItemClass(ABand: TcxGridBand): TdxCustomGridBandedTableViewItemPlaceClass;
begin
  if ABand.IsBottom then
    Result := TdxGridBandedTableViewBottomItemPlace
  else
    Result := TdxGridBandedTableViewItemPlace;
end;

procedure TdxGridBandedTableViewItemPlaceController.RefreshBottomItems;
var
  I: Integer;
begin
  FBottomItems.Count := Adapter.BottomBandCount;
  for I := 0 to BottomItemCount - 1 do
    FBottomItems[I] := ItemsByBand[Adapter.BottomBands[I]];
end;

procedure TdxGridBandedTableViewItemPlaceController.RefreshItems;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Refresh;
end;

procedure TdxGridBandedTableViewItemPlaceController.RefreshRootItems;
var
  I: Integer;
begin
  FRootItems.Count := Adapter.RootBandCount;
  for I := 0 to RootItemCount - 1 do
    FRootItems[I] := ItemsByBand[Adapter.RootBands[I]];
end;

function TdxGridBandedTableViewItemPlaceController.GetAdapter: TdxGridBandedTableViewAdapter;
begin
  Result := Formatter.Adapter;
end;

function TdxGridBandedTableViewItemPlaceController.GetAutoWidth: Boolean;
begin
  Result := Formatter.AutoWidth;
end;

function TdxGridBandedTableViewItemPlaceController.GetAvailableWidth: Integer;
begin
  Result := Formatter.ViewAvailableWidth;
end;

function TdxGridBandedTableViewItemPlaceController.GetBottomItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
begin
  Result := TdxCustomGridBandedTableViewItemPlace(FBottomItems[Index]);
end;

function TdxGridBandedTableViewItemPlaceController.GetBottomItemCount: Integer;
begin
  Result := FBottomItems.Count;
end;

function TdxGridBandedTableViewItemPlaceController.GetFormatter: TdxGridBandedTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridBandedTableViewFormatter;
end;

function TdxGridBandedTableViewItemPlaceController.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TdxGridBandedTableViewItemPlaceController.GetItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
begin
  Result := TdxCustomGridBandedTableViewItemPlace(FItems[Index]);
end;

function TdxGridBandedTableViewItemPlaceController.GetItemByBand(Band: TcxGridBand): TdxCustomGridBandedTableViewItemPlace;
var
  Index: Integer;
begin
  Index := IndexOf(Band);
  if Index <> -1 then
    Result := Items[IndexOf(Band)]
  else
    Result := nil;
end;

function TdxGridBandedTableViewItemPlaceController.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxGridBandedTableViewItemPlaceController.GetLevelCount: Integer;
begin
  if FLevelCount = -1 then
    FLevelCount := CalculateLevelCount;
  Result := FLevelCount;
end;

function TdxGridBandedTableViewItemPlaceController.GetLevelHeight(Index: Integer): Integer;
begin
  Result := Integer(FLevelHeights[Index]);
end;

function TdxGridBandedTableViewItemPlaceController.GetRootItem(Index: Integer): TdxCustomGridBandedTableViewItemPlace;
begin
  Result := TdxCustomGridBandedTableViewItemPlace(FRootItems[Index]);
end;

function TdxGridBandedTableViewItemPlaceController.GetRootItemCount: Integer;
begin
  Result := FRootItems.Count;
end;

procedure TdxGridBandedTableViewItemPlaceController.SetLevelHeight(Index: Integer; Value: Integer);
begin
  FLevelHeights[Index] := TObject(Value);
end;

procedure TdxGridBandedTableViewItemPlaceController.FreeAndNilItems;
begin
  ClearItems;
  FreeAndNil(FItems);
end;

{ TdxGridBandedTableViewFormatter }

constructor TdxGridBandedTableViewFormatter.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited;
  FBands := TList.Create;
end;

destructor TdxGridBandedTableViewFormatter.Destroy;
begin
  FreeAndNil(FBands);
  inherited;
end;

function TdxGridBandedTableViewFormatter.Adapter: TdxGridBandedTableViewAdapter;
begin
  Result := Builder.Adapter;
end;

function TdxGridBandedTableViewFormatter.Builder: TdxGridBandedTableViewBuilder;
begin
  Result := inherited Builder as TdxGridBandedTableViewBuilder;
end;

procedure TdxGridBandedTableViewFormatter.DoInitializeBandItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
var
  Band: TcxGridBand;
begin
  Band := Bands[AnIndex];
  SetViewParams(AnItem, GetBandItemViewParams(Band));
  if HasBackgroundBitmap(bbBandHeader) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbBandHeader);

  RegisterLookAndFeelItem(AnItem, cesRaised);

  with AnItem as TdxReportCellString do
  begin
    Data := TdxNativeInt(Band);
    EndEllipsis := Adapter.BandEndEllipsis;
    Multiline := Adapter.BandHeaderMultiline;
    Text := Band.Caption;
    TextAlignX := TextAlignXMap[Band.HeaderAlignmentHorz];
    TextAlignY := TextAlignYMap[Band.HeaderAlignmentVert];
    //Transparent := False; {!!! v3.03}
  end;
end;

procedure TdxGridBandedTableViewFormatter.DoInitializeBandRow(ARow: TdxReportCell);
begin
  SetViewParams(ARow, GetBandItemViewParams(nil));
  ARow.Transparent := True; //???
end;

procedure TdxGridBandedTableViewFormatter.DoReportLinkInitializeBandItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
  ReportLink.DoInitializeBandCell(Adapter.GridView, Bands[AnIndex], TdxReportCellString(AnItem));
end;

function TdxGridBandedTableViewFormatter.GetBandItemBounds(AnIndex: Integer): TRect;
var
  Item: TdxCustomGridBandedTableViewItemPlace;
begin
  Item := ItemPlaceController.ItemsByBand[Bands[AnIndex]];
  if Item <> nil then
    Result := Item.Bounds
  else
    Result := cxNullRect;
end;

function TdxGridBandedTableViewFormatter.GetBandItemClass(AnIndex: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxGridBandedTableViewFormatter.GetBandItemViewParams(ABand: TcxGridBand): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetBandHeaderParams(ABand, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetBandHeaderViewParams(ABand);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if ABand <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedBandHeaderItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedBandHeaderItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := False;
end;

procedure TdxGridBandedTableViewFormatter.AddHorizontalDelimiters;
var
  Origin, I: Integer;
  R: TRect;
  Item: TdxCustomGridBandedTableViewItemPlace;
begin
  if ReportLink.OptionsPagination.Band then
  begin
    Origin := ViewWidthExtraBefore;
    for I := 0 to BandCount - 1 do
    begin
      Item := ItemPlaceController.ItemsByBand[Bands[I]];
      if Item <> nil then
      begin
        R := Item.Bounds;
        ReportLink.AddHorizontalDelimiter(Origin + R.Left);
        ReportLink.AddHorizontalDelimiter(Origin + R.Right);
      end;
    end;
  end;
  inherited;
end;

function TdxGridBandedTableViewFormatter.CanColumnCellMerging(AColumn: TcxGridColumn): Boolean;
begin
  Result := inherited CanColumnCellMerging(AColumn) and
    (TcxGridBandedColumn(AColumn).Position.LineCount = ItemPlaceController.HeaderLineCount);
end;

function TdxGridBandedTableViewFormatter.IsColumnActuallyVisible(AColumn: TcxGridColumn): Boolean;
begin
  Result := inherited IsColumnActuallyVisible(AColumn) and
    TcxGridBandedColumn(AColumn).Position.Band.ActuallyVisible;
end;

function TdxGridBandedTableViewFormatter.GetAlignSummaryWithColumns: Boolean;
begin
  Result := inherited GetAlignSummaryWithColumns and HeadersSingleLine;
end;

function TdxGridBandedTableViewFormatter.GetItemPlaceControllerClass: TdxCustomGridTableViewItemPlaceControllerClass;
begin
  Result := TdxGridBandedTableViewItemPlaceController;
end;

procedure TdxGridBandedTableViewFormatter.BuildBandList;
var
  I: Integer;
  Band: TcxGridBand;
begin
  FBands.Clear;
  for I := 0 to Adapter.BandCount - 1 do
  begin
    Band := Adapter.Bands[I];
    if IsBandActuallyVisible(Band) then FBands.Add(Band);
  end;
end;

procedure TdxGridBandedTableViewFormatter.BuildItemLists;
var
  I: Integer;
begin
  BuildBandList;
  inherited;
  FHeadersSingleLine := True;
  for I := 0 to ColumnCount - 1 do
  begin
    FHeadersSingleLine := FHeadersSingleLine and
      (Columns[I].Position.RowIndex = 0);
  end;
end;

function TdxGridBandedTableViewFormatter.IsBandActuallyVisible(ABand: TcxGridBand): Boolean;
begin
  Result := ABand.ActuallyVisible;
end;

function TdxGridBandedTableViewFormatter.MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AGridBackgroundBitmapIndex of
    bbBandHeader:
      Result := vspsGridBandHeader;
  else
    Result := 0;
  end;

  if Result = 0 then
    Result := inherited MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex);
end;

function TdxGridBandedTableViewFormatter.GetBand(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FBands[Index]);
end;

function TdxGridBandedTableViewFormatter.GetBandCount: Integer;
begin
  Result := FBands.Count;
end;

function TdxGridBandedTableViewFormatter.GetColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := inherited Columns[Index] as TcxGridBandedColumn;
end;

function TdxGridBandedTableViewFormatter.GetItemPlaceController: TdxGridBandedTableViewItemPlaceController;
begin
  Result := inherited ItemPlaceController as TdxGridBandedTableViewItemPlaceController;
end;

function TdxGridBandedTableViewFormatter.GetShowBandHeaders: Boolean;
begin
  Result := ReportLink.OptionsView.BandHeaders;
end;

{ TdxGridBandedTableViewBuilder }

function TdxGridBandedTableViewBuilder.Adapter: TdxGridBandedTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridBandedTableViewAdapter;
end;

class function TdxGridBandedTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridBandedTableViewAdapter;
end;

function TdxGridBandedTableViewBuilder.Formatter: TdxGridBandedTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridBandedTableViewFormatter;
end;

class function TdxGridBandedTableViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridBandedTableViewFormatter;
end;

procedure TdxGridBandedTableViewBuilder.DoBuildViewBody;
begin
  if Formatter.ShowBandHeaders then CreateBands;
  if Formatter.ShowHeaders then CreateHeader;
  CreateRows;
  if Formatter.ShowFooters then CreateFooter;
end;

procedure TdxGridBandedTableViewBuilder.CreateBands;
begin
  AddReportRow(GetBandsProducer.Produce(HostInfoServices.BandHeadersHostInfo));
end;

function TdxGridBandedTableViewBuilder.GetBandsProducer: TdxGridTableViewBandsProducer;
begin
  Result := ProducerCache[GetBandsProducerClass] as TdxGridTableViewBandsProducer;
end;

function TdxGridBandedTableViewBuilder.GetBandsProducerClass: TdxGridTableViewBandsProducerClass;
begin
  Result := TdxGridTableViewBandsProducer;
end;

function TdxGridBandedTableViewBuilder.GridView: TcxGridBandedTableView;
begin
  Result := inherited GridView as TcxGridBandedTableView;
end;

class function TdxGridBandedTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

{ TdxGridDBBandedTableViewAdapter }

function TdxGridDBBandedTableViewAdapter.GridView: TcxGridDBBandedTableView;
begin
  Result := inherited GridView as TcxGridDBBandedTableView;
end;

class function TdxGridDBBandedTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBBandedTableView;
end;

function TdxGridDBBandedTableViewAdapter.DataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(inherited DataController);
end;

function TdxGridDBBandedTableViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := DataController.DataModeController;
end;

{ TdxGridDBBandedTableViewBuilder }

function TdxGridDBBandedTableViewBuilder.Adapter: TdxGridDBBandedTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridDBBandedTableViewAdapter;
end;

class function TdxGridDBBandedTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridDBBandedTableViewAdapter;
end;

class function TdxGridDBBandedTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBBandedTableView;
end;

{ TdxGridServerModeBandedTableViewAdapter }

function TdxGridServerModeBandedTableViewAdapter.GridView: TcxGridServerModeBandedTableView;
begin
  Result := inherited GridView as TcxGridServerModeBandedTableView;
end;

class function TdxGridServerModeBandedTableViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeBandedTableView;
end;

function TdxGridServerModeBandedTableViewAdapter.DataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TdxGridServerModeBandedTableViewBuilder }

function TdxGridServerModeBandedTableViewBuilder.Adapter: TdxGridServerModeBandedTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridServerModeBandedTableViewAdapter;
end;

class function TdxGridServerModeBandedTableViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridServerModeBandedTableViewAdapter;
end;

class function TdxGridServerModeBandedTableViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridServerModeBandedTableView;
end;

{ TdxReportWinExplorerViewRecord }

procedure TdxReportWinExplorerViewRecord.CalculateSize(AViewInfo: TcxCustomGridRecordViewInfo);
begin
  Height := AViewInfo.Height;
  Width := AViewInfo.Width;
end;

function TdxReportWinExplorerViewRecord.GetGridRecord: TcxGridWinExplorerViewCustomRecord;
begin
  Result := TcxGridWinExplorerViewCustomRecord(Data);
end;

{ TdxReportWinExplorerViewDataRecord }

function TdxReportWinExplorerViewDataRecord.GetGridRecord: TcxGridWinExplorerViewDataRecord;
begin
  Result := TcxGridWinExplorerViewDataRecord(inherited GridRecord);
end;

{ TdxReportWinExplorerViewGroupRecord }

function TdxReportWinExplorerViewGroupRecord.GetGridRecord: TcxGridWinExplorerViewGroupRecord;
begin
  Result := TcxGridWinExplorerViewGroupRecord(inherited GridRecord);
end;

{ TdxGridWinExplorerViewFormatter }

function TdxGridWinExplorerViewFormatter.Adapter: TdxGridWinExplorerViewAdapter;
begin
  Result := TdxGridWinExplorerViewAdapter(inherited Adapter);
end;

function TdxGridWinExplorerViewFormatter.Builder: TdxGridWinExplorerViewBuilder;
begin
  Result := TdxGridWinExplorerViewBuilder(inherited Builder);
end;

function TdxGridWinExplorerViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
begin
  Result := inherited GetItemViewParams(ATableItem, ARecord, AnIsPreview, AIsDataCell);
  Result.CellSides := [];
  Result.FontStyle := [];
  Result.Transparent := AIsDataCell;
end;

procedure TdxGridWinExplorerViewFormatter.DoInitializeItem(AnItem: TAbstractdxReportCellData;
  ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False);
var
  AStringItem: TdxReportCellString;
  AItem: TcxGridWinExplorerViewItem;
begin
  inherited DoInitializeItem(AnItem, ATableItem, ARecord, AnIsPreview);
  AItem := TcxGridWinExplorerViewItem(ATableItem);
  if (AnItem is TdxReportCellString) and (AItem <> nil) and
    (Adapter.IsGridItemCaption(AItem) or Adapter.IsGridItemDescription(AItem)) then
  begin
    AStringItem := TdxReportCellString(AnItem);
    if Adapter.GridView.ActiveDisplayMode = dmContent then
      AStringItem.TextAlignX := taLeft
    else
      AStringItem.TextAlignX := taCenterX;
    AStringItem.TextAlignY := taTop;
  end;
end;

function TdxGridWinExplorerViewFormatter.CreateDataCell(
  AParent: TdxReportWinExplorerViewRecord; AItem: TcxGridWinExplorerViewItem): TAbstractdxReportCellData;
begin
  Result := GetDataItemClass(AItem, AParent.GridRecord).Create(AParent);
  DoInitializeItem(Result, AItem, AParent.GridRecord);
end;

procedure TdxGridWinExplorerViewFormatter.DoInitializeRecord(ARecord: TdxReportWinExplorerViewRecord);
begin
  SetViewParams(ARecord, GetRecordViewParams(ARecord));
end;

function TdxGridWinExplorerViewFormatter.GetCaptionItemViewParams(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False): TdxReportItemViewParams;
begin
  if not (ReportLink.OptionsFormatting.UseNativeStyles or
    CanProcessSelectionStyle(ARecord) or AnIsPreview) then
  begin
    FillChar(Result, 0, SizeOf(Result));
    Result.NativeParams := Adapter.GetCaptionItemViewParams(ARecord, TcxGridWinExplorerViewItem(ATableItem));
    Result.CellSides := [];
    Result.FontStyle := [];
  end
  else
    GetItemViewParams(ATableItem, ARecord, AnIsPreview, True);
end;

function TdxGridWinExplorerViewFormatter.GetFirstRecordOffset: Integer;
begin
  Result := ScaleFactor.Apply(WinExplorerViewFirstRecordOffset);
end;

function TdxGridWinExplorerViewFormatter.GetInterRecordsSpaceHorz: Integer;
begin
  Result := ScaleFactor.Apply(WinExplorerViewRecordRecordsSpaceHorz);
end;

function TdxGridWinExplorerViewFormatter.GetInterRecordsSpaceVert: Integer;
begin
  Result := ScaleFactor.Apply(WinExplorerViewRecordRecordsSpaceVert);
end;

function TdxGridWinExplorerViewFormatter.GetRecordsAreaWidth: Integer;
begin
  Result := ViewWidth - GetFirstRecordOffset;
end;

function TdxGridWinExplorerViewFormatter.GetRecordViewParams(ARecord: TdxReportWinExplorerViewRecord): TdxReportItemViewParams;
begin
  Result := GetItemViewParams(nil, ARecord.GridRecord);
end;

function TdxGridWinExplorerViewFormatter.GetViewWidth: Integer;
begin
  Result := Max(SiteWidth, Builder.MaxRecordWidth + GetFirstRecordOffset);
end;

function TdxGridWinExplorerViewFormatter.GetRecord(Index: Integer): TcxGridWinExplorerViewCustomRecord;
begin
  Result := TcxGridWinExplorerViewCustomRecord(inherited Records[Index]);
end;

{ TdxGridWinExplorerViewAdapter }

function TdxGridWinExplorerViewAdapter.GridView: TcxGridWinExplorerView;
begin
  Result := TcxGridWinExplorerView(inherited GridView);
end;

function TdxGridWinExplorerViewAdapter.Styles: TcxGridWinExplorerViewStyles;
begin
  Result := inherited Styles as TcxGridWinExplorerViewStyles;
end;

function TdxGridWinExplorerViewAdapter.GetCaptionItemViewParams(ARecord: TcxCustomGridRecord;
  ATableItem: TcxGridWinExplorerViewItem): TcxViewParams;
begin
  if (ATableItem <> nil) and (ATableItem.Styles <> nil) then
    ATableItem.Styles.GetTextItemParams(ARecord, Result)
  else
    Styles.GetTextItemCellParams(ARecord, ATableItem, Result);
  if Result.Color = clWindow then // 3.2
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TdxGridWinExplorerViewAdapter.GetDescriptionItemViewParams(
  ARecord: TcxCustomGridRecord; ATableItem: TcxGridWinExplorerViewItem): TcxViewParams;
begin
  if (ATableItem <> nil) and (ATableItem.Styles <> nil) then
    ATableItem.Styles.GetDescriptionItemParams(ARecord, Result)
  else
    Styles.GetDescriptionItemCellParams(ARecord, ATableItem, Result);
  if Result.Color = clWindow then // 3.2
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TdxGridWinExplorerViewAdapter.GetGroupViewParams(
  ARecord: TcxCustomGridRecord; ATableItem: TcxCustomGridTableItem): TcxViewParams;
begin
  Styles.GetGroupParams(ARecord, ATableItem, Result);
end;

function TdxGridWinExplorerViewAdapter.IsGridItemCaption(AGridItem: TcxGridWinExplorerViewItem): Boolean;
begin
  Result := GridView.ItemSet.TextItem = AGridItem;
end;

function TdxGridWinExplorerViewAdapter.IsGridItemDescription(AGridItem: TcxGridWinExplorerViewItem): Boolean;
begin
  Result := GridView.ItemSet.DescriptionItem = AGridItem;
end;

function TdxGridWinExplorerViewAdapter.GetContentViewParams(ARecord: TcxCustomGridRecord;
  ATableItem: TcxCustomGridTableItem; AIsDataCell: Boolean = False): TcxViewParams;
var
  AItem: TcxGridWinExplorerViewItem;
begin
  AItem := TcxGridWinExplorerViewItem(ATableItem);
  if (AItem <> nil) and AIsDataCell then
    if IsGridItemCaption(AItem) then
      Result := GetCaptionItemViewParams(ARecord, AItem)
    else
      if IsGridItemDescription(AItem) then
        Result := GetDescriptionItemViewParams(ARecord, AItem)
      else
        Result := inherited GetContentViewParams(ARecord, ATableItem, AIsDataCell)
  else
    if not ARecord.IsData then
      Result := GetGroupViewParams(ARecord, ATableItem)
    else
      Result := inherited GetContentViewParams(ARecord, ATableItem, AIsDataCell);
end;

{ TdxGridWinExplorerViewBuilder }

constructor TdxGridWinExplorerViewBuilder.Create(AReportLink: TdxGridReportLink;
  AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView);
begin
  inherited Create(AReportLink, AMasterBuilder, AGridView);
  FRecords := TList.Create;
end;

destructor TdxGridWinExplorerViewBuilder.Destroy;
begin
  FreeAndNil(FRecords);
  inherited Destroy;
end;

function TdxGridWinExplorerViewBuilder.Adapter: TdxGridWinExplorerViewAdapter;
begin
  Result := inherited Adapter as TdxGridWinExplorerViewAdapter;
end;

class function TdxGridWinExplorerViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridWinExplorerViewAdapter;
end;

function TdxGridWinExplorerViewBuilder.Formatter: TdxGridWinExplorerViewFormatter;
begin
  Result := inherited Formatter as TdxGridWinExplorerViewFormatter;
end;

class function TdxGridWinExplorerViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridWinExplorerViewFormatter;
end;

procedure TdxGridWinExplorerViewBuilder.CalculateMaxRecordWidth;
var
  I: Integer;
  ARecord: TdxReportWinExplorerViewRecord;
begin
  FMaxRecordWidth := 0;
  for I := 0 to FRecords.Count - 1 do
  begin
    ARecord := Records[I];
    if ARecord.GridRecord.IsData then
      FMaxRecordWidth := Max(FMaxRecordWidth, ARecord.Width);
  end;
end;

procedure TdxGridWinExplorerViewBuilder.CreateRecord(AGridRecord: TcxGridWinExplorerViewCustomRecord);
var
  ARecord: TdxReportWinExplorerViewRecord;
  AProducer: TdxGridWinExplorerViewRecordProducer;
begin
  AProducer := GetRecordProducer(AGridRecord);
  ARecord := AProducer.CreateRecord(Host, AGridRecord);
  FRecords.Add(ARecord);
end;

procedure TdxGridWinExplorerViewBuilder.CreateRecords;
var
  I: Integer;
begin
  for I := 0 to Formatter.RecordCount - 1 do
    CreateRecord(Formatter.Records[I]);
end;

procedure TdxGridWinExplorerViewBuilder.DoBuildViewBody;
begin
  inherited DoBuildViewBody;
  CreateRecords;
  if not IsAborted then
  begin
    ResizeRecords;
    PlaceRecords;
  end;
end;

procedure TdxGridWinExplorerViewBuilder.DoResizeRecords;
begin
  CalculateMaxRecordWidth;
  if GridView.GroupedItemCount > 0 then
    ResizeGroups;
end;

function TdxGridWinExplorerViewBuilder.GetRecordProducer(
  AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxGridWinExplorerViewRecordProducer;
begin
  Result := ProducerCache[GetRecordProducerClass(AGridRecord)] as TdxGridWinExplorerViewRecordProducer;
end;

function TdxGridWinExplorerViewBuilder.GetRecordProducerClass(AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxGridViewRowProducerClass;
begin
  Result := Adapter.RecordHelpers[AGridRecord].ProducerClass;
end;

function TdxGridWinExplorerViewBuilder.GridView: TcxGridWinExplorerView;
begin
  Result := inherited GridView as TcxGridWinExplorerView;
end;

class function TdxGridWinExplorerViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridWinExplorerView;
end;

procedure TdxGridWinExplorerViewBuilder.InjectRecord(ARecord: TdxReportWinExplorerViewRecord;
  AParent: TdxReportCell; AIndex: Integer);

  function CalculateLeft: Integer;
  var
    I: Integer;
    AStartIndex: Integer;
  begin
    Result := Formatter.GetInterRecordsSpaceHorz div 2;
    AStartIndex := FRecords.IndexOf(ARecord) - AIndex;
    for I := 0 to AIndex - 1 do
    begin
      Inc(Result, Formatter.GetInterRecordsSpaceHorz);
      Inc(Result, Records[AStartIndex + I].Width);
    end;
  end;

begin
  ARecord.Parent := AParent;
  if ARecord.GridRecord.IsData then
    ARecord.Left := CalculateLeft
  else
    ARecord.Left := 0;
  ARecord.Top := Formatter.GetInterRecordsSpaceVert div 2;
end;

procedure TdxGridWinExplorerViewBuilder.PlaceRecords;

  procedure MakeRowIndexes(AIndexes: TList);
  var
    I, APlaceWidth, AEntryNumber: Integer;
  begin
    I := 0;
    if FRecords.Count > 0 then
      AIndexes.Add(TObject(0));

    while I < FRecords.Count do
    begin
      APlaceWidth := Formatter.GetRecordsAreaWidth;
      AEntryNumber := 0;
      repeat
        Inc(AEntryNumber);
        if AEntryNumber > 1 then
          Dec(APlaceWidth, Formatter.GetInterRecordsSpaceHorz);
        Dec(APlaceWidth, Records[I].Width);
        if (APlaceWidth >= 0) or (AEntryNumber = 1) then
          Inc(I);
      until (APlaceWidth <= 0) or (I >= FRecords.Count);
      AIndexes.Add(TObject(I));
    end;
  end;

  procedure MakeRows(AIndexes: TList);

    function GetMaxRecordHeight(AStartIndex, AEndIndex: Integer): Integer;
    var
      I: Integer;
    begin
      Result := Records[AStartIndex].Height;
      for I := AStartIndex + 1 to AEndIndex do
        Result := Max(Result, Records[I].Height);
    end;

  var
    AProducer: TdxGridWinExplorerViewRecordProducer;
    ARowIndex, AStartIndex, AEndIndex, I, ARowHeight: Integer;
    AReportRow: TdxReportCell;
  begin
    for ARowIndex := 0 to AIndexes.Count - 2 do
    begin
      AStartIndex := Integer(AIndexes[ARowIndex]);
      AEndIndex := Integer(AIndexes[ARowIndex + 1]) - 1;

      AProducer := GetRecordProducer(Records[AStartIndex].GridRecord);

      ARowHeight := GetMaxRecordHeight(AStartIndex, AEndIndex) + Formatter.GetInterRecordsSpaceVert;
      AReportRow := AProducer.Produce(HostInfoServices.PageDetailsHostInfo, ARowHeight);
      AddReportRow(AReportRow);

      for I := AStartIndex to AEndIndex do
        InjectRecord(Records[I], AProducer.Row, I - AStartIndex);
    end;
  end;

var
  AIndexes: TList;
begin
  AIndexes := TList.Create;
  try
    MakeRowIndexes(AIndexes);
    MakeRows(AIndexes);
  finally
    AIndexes.Free;
  end;
end;

procedure TdxGridWinExplorerViewBuilder.ResizeGroups;
var
  I: Integer;
  ARecord: TdxReportWinExplorerViewRecord;
begin
  for I := 0 to FRecords.Count - 1 do
  begin
    ARecord := Records[I];
    if not ARecord.GridRecord.IsData then
      ARecord.Width := Formatter.ViewWidth;
  end;
end;

procedure TdxGridWinExplorerViewBuilder.ResizeRecords;
begin
  FMaxRecordWidth := 0;
  if FRecords.Count > 0 then
    DoResizeRecords;
end;

function TdxGridWinExplorerViewBuilder.GetRecord(Index: Integer): TdxReportWinExplorerViewRecord;
begin
  Result := TdxReportWinExplorerViewRecord(FRecords[Index]);
end;

{ TdxGridDBWinExplorerViewAdapter }

function TdxGridDBWinExplorerViewAdapter.DataController: TcxGridDBDataController;
begin
  Result := inherited DataController as TcxGridDBDataController;
end;

function TdxGridDBWinExplorerViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := DataController.DataModeController;
end;

{ TdxGridDBWinExplorerViewBuilder }

class function TdxGridDBWinExplorerViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridDBWinExplorerViewAdapter;
end;

class function TdxGridDBWinExplorerViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBWinExplorerView;
end;

{ TdxGridCustomLayoutViewAdapter }

procedure TdxGridCustomLayoutViewAdapter.ExpandAllRows(
  AnOptionsExpanding: TdxGridReportLinkOptionsExpanding; ARecursive: Boolean);
begin
  if AnOptionsExpanding.ExpandCards then GridView.ViewData.Expand(True);
end;

{ TdxGridCardViewAdapter }

function TdxGridCardViewAdapter.GridView: TcxGridCardView;
begin
  Result := inherited GridView as TcxGridCardView;
end;

class function TdxGridCardViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

function TdxGridCardViewAdapter.Styles: TcxGridCardViewStyles;
begin
  Result := inherited Styles as TcxGridCardViewStyles;
end;

procedure TdxGridCardViewAdapter.GetVisibleCardRows(ACard: TcxGridCard; AList: TList);
begin
  ACard.GetVisibleRows(AList);
end;

function TdxGridCardViewAdapter.IsFirstRow(ACardRow: TcxGridCardViewRow): Boolean;
begin
  Result := ACardRow.VisibleIndex = 0;
end;

function TdxGridCardViewAdapter.IsLastRow(ACardRow: TcxGridCardViewRow): Boolean;
begin
  Result := ACardRow.VisibleIndex = CardRowCount - 1;
end;

function TdxGridCardViewAdapter.GetAreAllMasterRowsCollapsed: Boolean;
begin
  Result := True;
end;

function TdxGridCardViewAdapter.GetCaptionRowViewParams(ARecord: TcxCustomGridRecord;
  ACardRow: TcxGridCardViewRow): TcxViewParams;
begin
  if (ACardRow <> nil) and (ACardRow.Styles.CaptionRow <> nil) then
    ACardRow.Styles.GetCaptionRowParams(ARecord, Result)
  else
    Styles.GetCaptionRowParams(ARecord, ACardRow, Result);
end;

function TdxGridCardViewAdapter.GetCardViewParams(ARecord: TcxCustomGridRecord): TcxViewParams;
begin
  Styles.GetContentParams(ARecord, nil, Result);
end;

function TdxGridCardViewAdapter.GetContentViewParams(ARecord: TcxCustomGridRecord;
  ATableItem: TcxCustomGridTableItem; AIsDataCell: Boolean = False): TcxViewParams;
begin
  if (TcxGridCardViewRow(ATableItem).Kind = rkData) or AIsDataCell then
    Result := inherited GetContentViewParams(ARecord, ATableItem)
  else
    Result := GetCaptionRowViewParams(ARecord, TcxGridCardViewRow(ATableItem));
end;

function TdxGridCardViewAdapter.GetRowCaptionViewParams(ARecord: TcxCustomGridRecord;
  ACardRow: TcxGridCardViewRow): TcxViewParams;
begin
  if (ACardRow <> nil) and (ACardRow.Kind = rkData) then
    if ACardRow.Styles.Caption <> nil then
      ACardRow.Styles.GetCaptionParams(ARecord, Result)
    else
      Styles.GetRowCaptionParams(ARecord, ACardRow, Result)
  else
    Result := GetCaptionRowViewParams(ARecord, ACardRow);
end;

function TdxGridCardViewAdapter.GetCaptionSeparator: string;
begin
  Result := GridView.OptionsView.CaptionSeparator;
end;

function TdxGridCardViewAdapter.GetCard(Index: Integer): TcxGridCard;
begin
  Result := Records[Index] as TcxGridCard;
end;

function TdxGridCardViewAdapter.GetCardAutoWidth: Boolean;
begin
  Result := GridView.OptionsView.CardAutoWidth;
end;

function TdxGridCardViewAdapter.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

function TdxGridCardViewAdapter.GetCardCaptionWidth: Integer;
begin
  Result := GridView.OptionsView.CaptionWidth;
end;

function TdxGridCardViewAdapter.GetCardRow(Index: Integer): TcxGridCardViewRow;
begin
  Result := GridView.VisibleRows[Index];
end;

function TdxGridCardViewAdapter.GetCardRowCount: Integer;
begin
  Result := GridView.VisibleRowCount;
end;

function TdxGridCardViewAdapter.GetCardSeparatorColor: Integer;
begin
  Result := GridView.OptionsView.SeparatorColor;
end;

function TdxGridCardViewAdapter.GetCardSeparatorThickness: Integer;
begin
  Result := GridView.OptionsView.SeparatorWidth;
end;

function TdxGridCardViewAdapter.GetCardWidth: Integer;
begin
  Result := GridView.OptionsView.CardWidth;
end;

function TdxGridCardViewAdapter.GetHasCardsSeparator: Boolean;
begin
  Result := CardSeparatorThickness <> 0;
end;

function TdxGridCardViewAdapter.GetHasIndent(Index: Integer): Boolean;
begin
  with CardRows[Index] do
    Result := HasExpandButton or (CategoryRow <> nil);
end;

function TdxGridCardViewAdapter.GetRowAutoHeight: Boolean;
begin
  with GridView.OptionsView do
    Result := CellAutoHeight or RowCaptionAutoHeight;
end;

function TdxGridCardViewAdapter.GetRowCaptionAutoHeight: Boolean;
begin
  Result := GridView.OptionsView.RowCaptionAutoHeight;
end;

function TdxGridCardViewAdapter.GetRowCaptionEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.RowCaptionEndEllipsis;
end;

function TdxGridCardViewAdapter.GetShowRowCaption(Index: Integer): Boolean;
begin
  Result := CardRows[Index].Options.ShowCaption;
end;

function TdxGridCardViewAdapter.GetShowRowData(Index: Integer): Boolean;
begin
  Result := CardRows[Index].Options.ShowData;
end;

{ TdxGridCustomLayoutViewFormatter }

function TdxGridCustomLayoutViewFormatter.Adapter: TdxGridCustomLayoutViewAdapter;
begin
  Result := inherited Adapter as TdxGridCustomLayoutViewAdapter;
end;

function TdxGridCustomLayoutViewFormatter.Builder: TdxGridCustomLayoutViewBuilder;
begin
  Result := inherited Builder as TdxGridCustomLayoutViewBuilder;
end;

function TdxGridCustomLayoutViewFormatter.GetRecordClass(
  AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass;
begin
  Result := TdxReportCustomLayoutRecord;
end;

procedure TdxGridCustomLayoutViewFormatter.DoInitializeRecord(
  ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord);
begin
  ARecord.Data := TdxNativeInt(AGridRecord);
end;

procedure TdxGridCustomLayoutViewFormatter.DoInitializeRecordRow(ARecord: TdxReportCell);
begin
  if HasBackgroundBitmap(bbBackground) then
    ARecord.BackgroundBitmapIndex := GetBackgroundBitmapIndex(bbBackground);
  ARecord.CellSides := [];
end;

procedure TdxGridCustomLayoutViewFormatter.AddHorizontalDelimiters;
var
  I: Integer;
begin
  for I := 0 to Builder.RecordCount - 1 do
    ReportLink.AddHorizontalDelimiter(Builder.Records[I].AbsoluteRect.Right);
end;

function TdxGridCustomLayoutViewFormatter.GetViewWidth: Integer;
var
  AMasterBuilder: TdxCustomGridViewBuilder;
begin
  AMasterBuilder := Builder.MasterBuilder;
  if AMasterBuilder <> nil then
  begin
    Result := AMasterBuilder.Formatter.ViewWidth - AMasterBuilder.Adapter.ViewWidthExtra;
    if HasDetailsSeparator then
      Dec(Result, 2 * DetailsSeparatorThickness);
  end
  else
  begin
    Result := SiteWidth;
    if HasDetailsSeparator then
      Dec(Result, 2 * DetailsSeparatorThickness);
    Result := Max(Result, Builder.MaxRecordWidth + GetFirstRecordOffset);
  end;
end;

function TdxGridCustomLayoutViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if AIsDataCell then
      ReportLink.Styles.GetContentParams(ARecord, ATableItem, Result.NativeParams)
    else
      if CanProcessSelectionStyle(ARecord) then
        Result := GetSelectionViewParams
      else
        ReportLink.Styles.GetCardCaptionRowParams(ARecord, ATableItem, Result.NativeParams)
  else
    if AIsDataCell then
      Result := inherited GetItemViewParams(ATableItem, ARecord, AnIsPreview, AIsDataCell)
    else
      if CanProcessSelectionStyle(ARecord) then
        Result.NativeParams := Adapter.GetSelectionViewParams
      else
        Result.NativeParams := Adapter.GetCaptionViewParams;

  Result.CellSides := [];
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
end;

function TdxGridCustomLayoutViewFormatter.GetCardAutoWidth: Boolean;
begin
  Result := ReportLink.OptionsSize.AutoWidth;
end;

function TdxGridCustomLayoutViewFormatter.GetFirstRecordOffset: Integer;
begin
  Result := 0;
end;

function TdxGridCustomLayoutViewFormatter.GetInterRecordsSpaceHorz: Integer;
begin
  Result := 0;
end;

function TdxGridCustomLayoutViewFormatter.GetInterRecordsSpaceVert: Integer;
begin
  Result := 0;
end;

function TdxGridCustomLayoutViewFormatter.GetRecordsAreaWidth: Integer;
begin
  Result := ViewWidth - GetFirstRecordOffset;
end;

function TdxGridCustomLayoutViewFormatter.GetRecord(Index: Integer): TcxGridCustomLayoutRecord;
begin
  Result := inherited Records[Index] as TcxGridCustomLayoutRecord;
end;

{ TdxGridCardViewFormatter }

function TdxGridCardViewFormatter.Adapter: TdxGridCardViewAdapter;
begin
  Result := Builder.Adapter;
end;

function TdxGridCardViewFormatter.Builder: TdxGridCardViewBuilder;
begin
  Result := inherited Builder as TdxGridCardViewBuilder;
end;

procedure TdxGridCardViewFormatter.DoInitializeRecord(
  ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord);
var
  ACardRecord: TdxReportCard;
begin
  inherited;
  ACardRecord := ARecord as TdxReportCard;
  ACardRecord.ShadowColor := ReportLink.OptionsCards.Shadow.ActualColor;
  ACardRecord.ShadowDepth := ReportLink.OptionsCards.Shadow.Depth;
  ACardRecord.ShadowPosition := ReportLink.OptionsCards.Shadow.Position;
  SetViewParams(ACardRecord, GetCardViewParams(AGridRecord));
  ACardRecord.Width := Adapter.CardWidth;
  ACardRecord.CreateLayers(Builder);
end;

function TdxGridCardViewFormatter.GetRecordClass(AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass;
begin
  if IsHorizontalLayout(AGridRecord as TcxGridCard) then
    Result := TdxReportCardHorz
  else
    Result := TdxReportCardVert;
end;

function TdxGridCardViewFormatter.GetCardViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
begin
  Result.NativeParams := Adapter.GetCardViewParams(ARecord);
  Result.CellSides := csAll;//[]; // because of Card Shadow
  Result.FontStyle := [];
  Result.Transparent := True;
end;

procedure TdxGridCardViewFormatter.DoInitializeCardRowIndent(
  AnItem: TdxReportCellExpandButton; ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard);
const
  BackgroundBitmapIndexes: array[TcxGridCardViewRowKind] of Integer =
    (bbRowCaption, bbCaptionRow, bbCaptionRow);
begin
  if ACardRow.CategoryRow <> nil then
    ACardRow := ACardRow.CategoryRow;
  SetViewParams(AnItem, GetCardRowCaptionViewParams(AGridCard, ACardRow));
  if HasBackgroundBitmap(BackgroundBitmapIndexes[ACardRow.Kind]) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(BackgroundBitmapIndexes[ACardRow.Kind]);
end;

procedure TdxGridCardViewFormatter.DoInitializeCardRowCaption(
  AnItem: TdxReportCellText; ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard);
const
  BackgroundBitmapIndexes: array[TcxGridCardViewRowKind] of Integer = (bbRowCaption, bbCaptionRow, bbCaptionRow);
var
  ACaption: TdxReportCellString;
begin
  SetViewParams(AnItem, GetCardRowCaptionViewParams(AGridCard, ACardRow));
  if HasBackgroundBitmap(BackgroundBitmapIndexes[ACardRow.Kind]) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(BackgroundBitmapIndexes[ACardRow.Kind]);

  ACaption := AnItem as TdxReportCellString;
  ACaption.EndEllipsis := Adapter.RowCaptionEndEllipsis;
  ACaption.Multiline := not ACaption.EndEllipsis and not CaptionAutoWidth and Adapter.RowCaptionAutoHeight;
  ACaption.TextAlignX := TextAlignXMap[ACardRow.CaptionAlignmentHorz];
  if ACaption.Multiline then
    ACaption.TextAlignY := TextAlignYMap[ACardRow.CaptionAlignmentVert]
  else
    ACaption.TextAlignY := taTop;

  if Adapter.CaptionSeparator <> #0 then
    ACaption.Text := ACardRow.Caption + Adapter.CaptionSeparator
  else
    ACaption.Text := ACardRow.Caption;
end;

procedure TdxGridCardViewFormatter.DoReportLinkInitializeCardRowCaption(AnItem: TdxReportCellText;
  ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard);
begin
  ReportLink.DoInitializeCardRowCaptionCell(Adapter.GridView, AGridCard, ACardRow, TdxReportCellString(AnItem));
end;

function TdxGridCardViewFormatter.GetCardRowCaptionClass(ACardRow: TcxGridCardViewRow): TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TdxGridCardViewFormatter.GetCardRowCaptionViewParams(ARecord: TcxCustomGridRecord;
  ACardRow: TcxGridCardViewRow): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if ACardRow.Kind = rkData then
      ReportLink.Styles.GetCardRowCaptionParams(ARecord, ACardRow, Result.NativeParams)
    else
      if CanProcessSelectionStyle(ARecord) then
        Result := GetSelectionViewParams
      else
        ReportLink.Styles.GetCardCaptionRowParams(ARecord, ACardRow, Result.NativeParams)
  else
    if (ACardRow.Kind = rkCaption) and CanProcessSelectionStyle(ARecord) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetRowCaptionViewParams(ARecord, ACardRow);

  // v.3.1 - because of fact that each Card in Grid v.5 might has different number of Rows,
  // CellSides initialization was moved inside ReportCard
  Result.CellSides := [];
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent
end;

procedure TdxGridCardViewFormatter.DoReportLinkInitializeCardRowData(AnItem: TAbstractdxReportCellData;
  ACardRow: TcxGridCardViewRow; AGridCard: TcxGridCard);
begin
  ReportLink.DoInitializeCardRowDataCell(Adapter.GridView, AGridCard, ACardRow, TdxReportCellString(AnItem));
end;

function TdxGridCardViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem;
  ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
var
  CardRow: TcxGridCardViewRow;
begin
  CardRow := TcxGridCardViewRow(ATableItem);
  AIsDataCell := AIsDataCell and (CardRow.Kind <> rkCaption);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if (CardRow.Kind = rkData) or AIsDataCell then
      ReportLink.Styles.GetContentParams(ARecord, CardRow, Result.NativeParams)
    else
      if CanProcessSelectionStyle(ARecord) then
        Result := GetSelectionViewParams
      else
        ReportLink.Styles.GetCardCaptionRowParams(ARecord, CardRow, Result.NativeParams)
  else
    if (CardRow.Kind = rkData) or AIsDataCell then
      Result := inherited GetItemViewParams(ATableItem, ARecord, AnIsPreview, AIsDataCell)
    else
      if CanProcessSelectionStyle(ARecord) then
        Result.NativeParams := Adapter.GetSelectionViewParams
      else
        Result.NativeParams := Adapter.GetCaptionRowViewParams(ARecord, CardRow);

  // v.3.1 - because fact that each Card in Grid v.5 might has different number of Rows,
  // CellSides initialization was moved inside ReportCard
  Result.CellSides := [];
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent
end;

function TdxGridCardViewFormatter.GetFirstRecordOffset: Integer;
begin
  Result := ScaleFactor.Apply(FirstCardOffset);
end;

function TdxGridCardViewFormatter.GetInterRecordsSpaceHorz: Integer;
begin
  Result := ReportLink.OptionsCards.InterCardsSpaceHorz;
end;

function TdxGridCardViewFormatter.GetInterRecordsSpaceVert: Integer;
begin
  Result := ReportLink.OptionsCards.InterCardsSpaceVert;
end;

function TdxGridCardViewFormatter.IsHorizontalLayout(AGridCard: TcxGridCard): Boolean;
begin
  Result := AGridCard.GridView.RowLayout = rlHorizontal;
end;

function TdxGridCardViewFormatter.MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AGridBackgroundBitmapIndex of
    bbCaptionRow:
      Result := vspsGridCardCaptionRow;
    bbRowCaption:
      Result := vspsGridCardRowCaption;
  else
    Result := 0;
  end;
  if Result = 0 then
    Result := inherited MapStyleBackgroundBitmapIndex(AGridBackgroundBitmapIndex);
end;

function TdxGridCardViewFormatter.GetContentBackgroundBitmapStyleIndex(ATableItem: TcxCustomGridTableItem): Integer;
const
  BackgroundBitmapIndexes: array[TcxGridCardViewRowKind] of Integer =
    (bbContent, bbCaptionRow, bbCaptionRow);
begin
  Result := BackgroundBitmapIndexes[TcxGridCardViewRow(ATableItem).Kind];
end;

function TdxGridCardViewFormatter.NeedDelimitByRows: Boolean;
{var
  I: Integer;}
begin
  Result := False;
  {if MasterBuilder = nil then
    for I := 0 to Builder.CardCount - 1 do
      if Builder.Cards[I].Height > SiteHeight then
      begin
        Result := True;
        Exit;
      end;}
end;

function TdxGridCardViewFormatter.GetAutoWidth: Boolean;
begin
  Result := ReportLink.OptionsCards.AutoWidth;
end;

function TdxGridCardViewFormatter.GetCaptionAutoWidth: Boolean;
begin
  Result := Adapter.CardCaptionWidth = 0;
end;

function TdxGridCardViewFormatter.GetCardAutoWidth: Boolean;
begin
  Result := Adapter.CardAutoWidth or inherited GetCardAutoWidth;
end;

function TdxGridCardViewFormatter.GetCard(Index: Integer): TcxGridCard;
begin
  Result := TcxGridCard(Records[Index]);
end;

function TdxGridCardViewFormatter.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

function TdxGridCardViewFormatter.GetKeepSameHeight: Boolean;
begin
  Result := ReportLink.OptionsCards.KeepSameHeight;
end;

function TdxGridCardViewFormatter.GetKeepSameWidth: Boolean;
begin
  Result := ReportLink.OptionsCards.KeepSameWidth;
end;

function TdxGridCardViewFormatter.GetRowAutoHeight: Boolean;
begin
  Result := Adapter.RowAutoHeight;
end;

{  TdxReportCardLayer }

constructor TdxReportCardLayer.CreateEx(
  AParent: TdxReportCell; ABeginsLayerRow: TcxGridCardViewRow);
begin
  inherited Create(AParent);
  FRows := TList.Create;
  CellSides := [];
  Data := TdxNativeInt(ABeginsLayerRow);
  Width := Card.LayerWidths[Index];
//  Height := Card.ContentHeight;
  AddRow(ABeginsLayerRow);
end;

destructor TdxReportCardLayer.Destroy;
begin
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TdxReportCardLayer.AdjustCellSides;
var
  I: Integer;
  ReportLink: TdxGridReportLink;
begin
  ReportLink := Card.ReportLink;
  for I := 0 to RowCount - 1 do
  begin
    Rows[I].AdjustCellSides(IsFirstItem, IsLastItem and
      not (NeedCategorySeparator or NeedLayerSeparator), I = 0, I = RowCount - 1);
    if Rows[I].Left + Rows[I].Width > Width then
      Rows[I].Width := Width - Rows[I].Left;
  end;
  if ReportLink.OptionsCards.Borders then
  begin
    if LayerSeparator <> nil then
    begin
      LayerSeparator.CellSides := [csLeft, csRight];
      if IsLastItem and not NeedCategorySeparator then
        LayerSeparator.CellSides := LayerSeparator.CellSides + [csBottom];
    end;
    if CategorySeparator <> nil then
    begin
      CategorySeparator.CellSides := [csLeft, csRight];
      if IsLastItem then
        CategorySeparator.CellSides := CategorySeparator.CellSides + [csBottom];
    end;
  end;
end;

function TdxReportCardLayer.AddRow(
  AGridCardRow: TcxGridCardViewRow): TdxReportCardRow;
begin
  Result := TdxReportCardRow.CreateEx(Self, AGridCardRow);
  FRows.Add(Result);
end;

function TdxReportCardLayer.MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Min(0, RowCount - 1) do
    Result := Max(Result, Rows[I].MeasureCaptionWidth(ACanvas));
end;

function TdxReportCardLayer.MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].MeasureDataWidth(ACanvas));
end;

function TdxReportCardLayer.MeasureLayerHeight(ACanvas: TdxPSReportRenderCustomCanvas; ACalculateLineHeight: Boolean;
  ABuilder: TdxGridCardViewBuilder): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].MeasureHeight(ACanvas, ACalculateLineHeight, ABuilder));
  if LayerSeparator <> nil then
    Inc(Result, LayerSeparator.Height);
  if CategorySeparator <> nil then
    Inc(Result, CategorySeparator.Height);
end;

function TdxReportCardLayer.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Inc(Result, Rows[I].MeasureWidth(ACanvas));
end;

procedure TdxReportCardLayer.AdjustRowsHeight;
var
  I, H: Integer;
begin
  H := Height;
  if LayerSeparator <> nil then
    Dec(H, LayerSeparator.Height);
  if CategorySeparator <> nil then
    Dec(H, CategorySeparator.Height);
  for I := 0 to RowCount - 1 do
    Rows[I].Height := H;
end;

procedure TdxReportCardLayer.AdjustRowHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder);
var
  I, ARowHeight, ATotalHeight: Integer;
begin
  ATotalHeight := 0;
  for I := 0 to RowCount - 1 do
  begin
    ARowHeight := Rows[I].MeasureHeight(ACanvas, not AAutoHeight, ABuilder);
    SetRowHeight(I, ARowHeight);
    Inc(ATotalHeight, ARowHeight);
  end;
  InternalHeight := ATotalHeight;
end;

function TdxReportCardLayer.CanHaveIndent(ARow: TdxReportCardRow): Boolean;
begin
  Result := ARow.GridCardRow.Position.VisibleColIndex = 0;
end;

procedure TdxReportCardLayer.CheckNeedCategorySeparator(ANextLayer: TdxReportCardLayer);
var
  I: Integer;
begin
  FNeedCategorySeparator := ANextLayer.BeginsLayerRow.HasExpandButton;
  if FNeedCategorySeparator then
  begin
    FNeedCategorySeparator := False;
    for I := 0 to RowCount - 1 do
      with Rows[I].GridCardRow do
        FNeedCategorySeparator := FNeedCategorySeparator or (HasExpandButton or (CategoryRow <> nil));
  end;
end;

procedure TdxReportCardLayer.CreateItems(ABuilder: TdxGridCardViewBuilder);
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].CreateItems(ABuilder);
  if NeedCategorySeparator then
    CreateCategorySeparator;
  if NeedLayerSeparator then
    CreateLayerSeparator;
end;

procedure TdxReportCardLayer.CreateCategorySeparator;
var
  AParams: TcxViewParams;
  ACardView: TcxGridCardView;
begin
  FCategorySeparator := TdxReportCellBox.Create(Self);
  ACardView := TcxGridCardView(BeginsLayerRow.GridView);
  ACardView.Styles.GetViewParams(vsCategorySeparator, nil, nil, AParams);
  FCategorySeparator.BoundsRect := Rect(0, 0, Width,
    ACardView.OptionsView.CategorySeparatorWidth);
  FCategorySeparator.Color := AParams.Color;
  FCategorySeparator.CellSides := [];
  FCategorySeparator.Transparent := False;
end;

procedure TdxReportCardLayer.CreateLayerSeparator;
var
  AParams: TcxViewParams;
  ACardView: TcxGridCardView;
begin
  FLayerSeparator := TdxReportCellBox(AddDataItem(TdxReportCellBox));
  ACardView := TcxGridCardView(BeginsLayerRow.GridView);
  FLayerSeparator.BoundsRect := Rect(0, 0, Width,
     ACardView.OptionsView.LayerSeparatorWidth);
  ACardView.Styles.GetViewParams(vsLayerSeparator, nil, nil, AParams);
  FLayerSeparator.Transparent := False;
  FLayerSeparator.CellSides := [];
  FLayerSeparator.Color := AParams.Color;
end;

procedure TdxReportCardLayer.InitAutoWidthItem(
  AnItem: TcxAutoWidthItem; var AAllFixed: Boolean);
var
  I, AWidth: Integer;
const
  LayerDefaultWidth = 20;
begin
  AWidth := 0;
  for I := 0 to RowCount - 1 do
    AWidth := Max(AWidth, Rows[I].GridCardRow.Position.Width);
  AnItem.Width := AWidth;
  AnItem.Fixed := AnItem.Width <> 0;
  AAllFixed := AAllFixed and AnItem.Fixed;
  if AnItem.Width = 0 then
    AnItem.Width := LayerDefaultWidth;
end;

procedure TdxReportCardLayer.PlaceSeparatorAndExcludeHeightFromHeight(
  var AValue: Integer);
begin
  if LayerSeparator <> nil then
  begin
    Dec(AValue, LayerSeparator.Height);
    LayerSeparator.Top := AValue;
  end;
end;

procedure TdxReportCardLayer.RecalculateHeight;
var
  AHeight, I: Integer;
begin
  AHeight := 0;
  for I := 0 to RowCount - 1 do
    AHeight := Max(AHeight, Rows[I].Height);
  if LayerSeparator <> nil then
    Inc(AHeight, LayerSeparator.Height);
  if CategorySeparator <> nil then
    Inc(AHeight, CategorySeparator.Height);
  Height := AHeight;
end;

procedure TdxReportCardLayer.ResizeRowsHorz;
var
  ALeft, I: Integer;
  AAllFixed: Boolean;
  AAutoWidths: TcxAutoWidthObject;
  AItem: TcxAutoWidthItem;
const
  CardRowDefaultWidth = 20;
begin
  if RowCount = 0 then Exit;
  if RowCount  = 1 then
    Rows[0].Width := Width
  else
  begin
    AAutoWidths := TcxAutoWidthObject.Create(RowCount);
    try
      AAllFixed := True;
      for I := 0 to RowCount - 1 do
      begin
        AItem := AAutoWidths.AddItem;
        AItem.Width := Rows[I].GridCardRow.Position.Width;
        AItem.Fixed := AItem.Width <> 0;
        AAllFixed := AAllFixed and AItem.Fixed;
        if AItem.Width = 0 then
          AItem.Width := CardRowDefaultWidth;
      end;
      if AAllFixed or (AAutoWidths.Width > Width) then
        for I := 0 to AAutoWidths.Count - 1 do
          AAutoWidths[I].Fixed := False;
      AAutoWidths.AvailableWidth := Width;
      AAutoWidths.Calculate;
      ALeft := 0;
      for I := 0 to RowCount - 1 do
      begin
        Rows[I].Left := ALeft;
        Rows[I].Width := AAutoWidths[I].AutoWidth;
        Inc(ALeft, Rows[I].Width);
      end;
    finally
      AAutoWidths.Free;
    end;
  end;
end;

function TdxReportCardLayer.SetPosition(ALeft, AWidth: Integer): Integer;
begin
  Left := ALeft;
  Width := AWidth;
  Result := Left + Width;
end;

procedure TdxReportCardLayer.SetRowCaptionWidth(
  ACanvas: TdxPSReportRenderCustomCanvas; ACaptionAutoWidths: Boolean;
  AValue: Integer);
var
  I: Integer;
begin
  if ACaptionAutoWidths then
  begin
    for I := 0 to Min(0, RowCount - 1) do
      Rows[I].RowCaptionWidth := AValue;
    for I := 1 to RowCount - 1 do
      Rows[I].RowCaptionWidth := Rows[I].MeasureCaptionWidth(ACanvas);
  end
  else
    for I := 0 to RowCount - 1 do
      Rows[I].RowCaptionWidth := AValue;
end;

procedure TdxReportCardLayer.SetRowHeight(ARowIndex, AValue: Integer);
var
  Delta, I: Integer;
begin
  if AValue < 0 then AValue := 0;

  Delta := AValue - Rows[ARowIndex].Height;
  Rows[ARowIndex].Height := AValue;

  for I := ARowIndex + 1 to RowCount - 1 do
    Rows[I].Top := Rows[I].Top + Delta;
end;

procedure TdxReportCardLayer.SizeChanged;
begin
  if LayerSeparator <> nil then
    LayerSeparator.Width := Width;
  if CategorySeparator <> nil then
    CategorySeparator.Width := Width;
  ResizeRowsHorz;
end;

function TdxReportCardLayer.GetBeginsLayerRow: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(Data);
end;

function TdxReportCardLayer.GetCard: TdxReportCardHorz;
begin
  Result := TdxReportCardHorz(Parent);
end;

function TdxReportCardLayer.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TdxReportCardLayer.GetInternalHeight: Integer;
begin
  Result := inherited Height;
end;

function TdxReportCardLayer.GetInternalWidth: Integer;
begin
  Result := inherited Width;
end;

function TdxReportCardLayer.GetRow(AIndex: Integer): TdxReportCardRow;
begin
  Result := TdxReportCardRow(FRows[AIndex]);
end;

function TdxReportCardLayer.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TdxReportCardLayer.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TdxReportCardLayer.SetHeight(AValue: Integer);
begin
  inherited Height := AValue;
  if AValue = 0 then Exit;
  if CategorySeparator <> nil then
  begin
    FCategorySeparator.Top := AValue - FCategorySeparator.Height;
    Dec(AValue, FCategorySeparator.Height);
  end;
  PlaceSeparatorAndExcludeHeightFromHeight(AValue);
  AdjustRowsHeight;
end;

procedure TdxReportCardLayer.SetInternalHeight(AValue: Integer);
begin
  inherited Height := AValue;
end;

procedure TdxReportCardLayer.SetInternalWidth(AValue: Integer);
begin
  inherited Width := AValue;
end;

procedure TdxReportCardLayer.SetWidth(AValue: Integer);
begin
  inherited Width := AValue;
  SizeChanged;
end;

{  TdxReportCardVerticalLayer }

procedure TdxReportCardVerticalLayer.AdjustCellSides;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].AdjustCellSides(I = 0, False, Index = 0, Index = Card.LayerCount - 1);
  if (LayerSeparator <> nil) and Card.ReportLink.OptionsCards.RowBordersVert then
    LayerSeparator.CellSides := LayerSeparator.CellSides + [csLeft, csRight];
end;

function TdxReportCardVerticalLayer.MeasureCaptionWidth(
  ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].MeasureCaptionWidth(ACanvas));
end;

function TdxReportCardVerticalLayer.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].MeasureWidth(ACanvas));
  if LayerSeparator <> nil then
    Inc(Result, LayerSeparator.Width);
end;

procedure TdxReportCardVerticalLayer.AdjustRowsHeight;
begin
end;

function TdxReportCardVerticalLayer.CanHaveIndent(ARow: TdxReportCardRow): Boolean;
begin
  Result := True;
end;

procedure TdxReportCardVerticalLayer.CreateLayerSeparator;
begin
  inherited CreateLayerSeparator;
  LayerSeparator.Width := LayerSeparator.Height;
end;

procedure TdxReportCardVerticalLayer.CheckNeedCategorySeparator(
  ANextLayer: TdxReportCardLayer);
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    if Rows[I].GridCardRow.HasExpandButton and (I > 0) then
      Rows[I - 1].NeedSeparator := True;
  end;
end;

procedure TdxReportCardVerticalLayer.PlaceSeparatorAndExcludeHeightFromHeight(
  var AValue: Integer);
begin
end;

procedure TdxReportCardVerticalLayer.RecalculateHeight;
var
  I, AHeight: Integer;
begin
  AHeight := 0;
  for I := 0 to RowCount - 1  do
    Inc(AHeight, Rows[I].Height);
  Height := AHeight;
end;

procedure TdxReportCardVerticalLayer.SetRowCaptionWidth(
  ACanvas: TdxPSReportRenderCustomCanvas; ACaptionAutoWidths: Boolean; AValue: Integer);
var
  I: Integer;
begin
  if ACaptionAutoWidths then
    for I := 0 to RowCount - 1 do
      Rows[I].RowCaptionWidth := AValue
  else
    for I := 0 to RowCount - 1 do
      Rows[I].RowCaptionWidth := AValue;
end;

procedure TdxReportCardVerticalLayer.SizeChanged;
var
  I, AWidth: Integer;
begin
  AWidth := Width;
  if LayerSeparator <> nil then
  begin
    Dec(AWidth, LayerSeparator.Width);
    LayerSeparator.Height := Card.ContentHeight;
  end;
  for I := 0 to RowCount - 1 do
  begin
    Rows[I].Width := AWidth;
    if LayerSeparator <> nil then
      Rows[I].Left := LayerSeparator.Width;
  end;
end;

{ TdxReportCardRow }

constructor TdxReportCardRow.CreateEx(AParent: TdxReportCell; AGridCardRow: TcxGridCardViewRow);
begin
  inherited Create(AParent);
  CellSides := [];
  Data := TdxNativeInt(AGridCardRow);
  Width := Card.ContentWidth;
end;

function TdxReportCardRow.MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas; ACalculateLineHeight: Boolean;
  ABuilder: TdxGridCardViewBuilder): Integer;

  function MeasureCaptionHeight: Integer;
  begin
    Result := ABuilder.Adapter.ScaleFactor.Apply(DefaultDataRowLineHeight);
    if HasCaption then
    begin
      if ACalculateLineHeight then
        Result := RowCaption.MeasureFontHeight(ACanvas)
      else
        Result := RowCaption.MeasureContentHeight(ACanvas);
    end;
    Result := Max(Result, ABuilder.Adapter.ScaleFactor.Apply(DefaultDataRowLineHeight));
  end;

  function MeasureDataHeight: Integer;
  var
    MinHeight: Integer;
  begin
    Result := ABuilder.Adapter.ScaleFactor.Apply(DefaultDataRowLineHeight) * GridCardRow.Position.LineCount;
    if HasData then
    begin
      MinHeight := RowData.MeasureFontHeight(ACanvas);
      if not ACalculateLineHeight then
      begin
        Result := RowData.MeasureContentHeight(ACanvas);
        if Result = 0 then
          Result := MinHeight * GridCardRow.Position.LineCount;
      end
      else
        Result := MinHeight * GridCardRow.Position.LineCount;

      ABuilder.DoGetCellHeight(Card.GridCard, GridCardRow, Result);
      Result := Max(Result, MinHeight);
    end;
  end;

begin
  Result := Max(MeasureCaptionHeight, MeasureDataHeight);
  if RowSeparator <> nil then
    Inc(Result, RowSeparator.Height);
end;

function TdxReportCardRow.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GridCardRow.Position.Width;
  if Result = 0 then
  begin
    Result := MeasureCaptionWidth(ACanvas) + MeasureDataWidth(ACanvas);
    if HasIndent then
      Inc(Result, RowIndent.Width);
  end;
end;

function TdxReportCardRow.MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  if HasCaption then
  begin
    Result := RowCaption.MeasureContentWidth(ACanvas){ + 2};
    if HasIndent then
      Inc(Result, RowIndent.Width);
  end
  else
    Result := 0;
end;

function TdxReportCardRow.MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  if HasData and dxPSDataMaps.DoesItemParticipateInAutoWidthCalculation(Properties) then
    Result := RowData.MeasureContentWidth(ACanvas) + 2
  else
    Result := 0;
end;

procedure TdxReportCardRow.AdjustCellSides(
  AIsFirstItem, AIsLastItem, AIsLeftItem, AIsRightItem: Boolean);
var
  ReportLink: TdxGridReportLink;
  Sides: TdxCellSides;
begin
  ReportLink := Card.ReportLink;
  if ReportLink.OptionsCards.Borders then
  begin
    Sides := [];
    if AIsFirstItem then Include(Sides, csTop);
    if AIsLastItem then Include(Sides, csBottom);
    if HasIndent then
      RowIndent.CellSides := Sides;
    if HasIndent and AIsLeftItem then
      RowIndent.CellSides := Sides + [csLeft];
    if HasCaption then
    begin
      RowCaption.CellSides := RowCaption.CellSides + Sides;
      if not HasIndent and AIsLeftItem then
        RowCaption.CellSides := RowCaption.CellSides + [csLeft];
    end;
    if HasData then
    begin
      RowData.CellSides := RowData.CellSides + Sides;
      if AIsRightItem then
        RowData.CellSides := RowData.CellSides + [csRight];
    end;
    if not HasCaption and AIsLeftItem then
      RowData.CellSides := RowData.CellSides + [csLeft]
    else
      if not HasData and AIsRightItem then
        RowCaption.CellSides := RowCaption.CellSides + [csRight];
  end;

  if ReportLink.OptionsCards.RowBordersHorz then
  begin
    Sides := [];
    if not AIsFirstItem then Include(Sides, csTop);
    if not AIsLastItem then Include(Sides, csBottom);
    if HasCaption then
      RowCaption.CellSides := RowCaption.CellSides + Sides;
    if HasData then
      RowData.CellSides := RowData.CellSides + Sides;
    if HasIndent then
      RowIndent.CellSides := RowIndent.CellSides + Sides;
  end;

  if ReportLink.OptionsCards.RowBordersVert then
  begin
    if HasCaption then
    begin
      RowCaption.CellSides := RowCaption.CellSides + [csRight];
      if not HasIndent then
         RowCaption.CellSides := RowCaption.CellSides + [csLeft];
    end;
    if HasData and (HasCaption or not HasIndent) then
      RowData.CellSides := RowData.CellSides + [csLeft];
    if HasIndent then
      RowIndent.CellSides := RowIndent.CellSides + [csLeft];
  end;
end;

procedure TdxReportCardRow.CreateItems(ABuilder: TdxGridCardViewBuilder);
const
  InitialCaptionWidth = 100;

  function HasRowCaption: Boolean;
  begin
    Result := GridCardRow.Options.ShowCaption;
  end;

  function HasRowData: Boolean;
  begin
    Result := GridCardRow.Options.ShowData;
  end;

  function HasIndent: Boolean;
  begin
    Result := GridCardRow.HasExpandButton or
      ((GridCardRow.CategoryRow <> nil) and Layer.CanHaveIndent(Self));
  end;

  function ExpandButtonSize: Integer;
  begin
    Result := ABuilder.Adapter.ExpandButtonSize + cxTextOffset * 3;
  end;

  procedure CreateRowCaption;
  begin
    FRowCaption := ABuilder.Formatter.GetCardRowCaptionClass(GridCardRow).Create(Self);
    if HasIndent then
    begin
      FRowIndent := TdxReportCellExpandButton.Create(Self);
      FRowIndent.Width := ExpandButtonSize;
      FRowIndent.ShowButton := GridCardRow.HasExpandButton;
      FRowIndent.CellSides := [];
      if FRowIndent.ShowButton then
      begin
        ABuilder.Formatter.DoInitializeExpandButton(FRowIndent, Card.GridCard, True);
        FRowIndent.ButtonExpanded := GridCardRow.Expanded;
      end;
      ABuilder.Formatter.DoInitializeCardRowIndent(FRowIndent, GridCardRow, Card.GridCard);
    end;
    if FRowIndent <> nil then
      RowCaption.Left := FRowIndent.Width;
    if HasRowData then
      RowCaption.Width := InitialCaptionWidth
    else
      RowCaption.Width := Parent.Width - RowCaption.Left;
    RowCaption.Height := Height;
    RowCaption.Data := TdxNativeInt(TdxGridCardRowCaption);
    ABuilder.Formatter.DoInitializeCardRowCaption(RowCaption, GridCardRow, Card.GridCard);
    ABuilder.Formatter.DoReportLinkInitializeCardRowCaption(RowCaption, GridCardRow, Card.GridCard);
  end;

  procedure CreateRowData;
  begin
    FRowData := ABuilder.Formatter.GetDataItemClass(GridCardRow, Card.GridCard).Create(Self);
    if HasRowCaption then
      RowData.Left := InitialCaptionWidth
    else
      RowData.Left := 0;
    RowData.Width := Parent.Width - Left;
    RowData.Height := ABuilder.Adapter.ScaleFactor.Apply(DefaultDataRowLineHeight);
    ABuilder.Formatter.DoInitializeItem(RowData, GridCardRow, Card.GridCard, False);
    ABuilder.Formatter.DoReportLinkInitializeCardRowData(RowData, GridCardRow, Card.GridCard);
  end;

  procedure CreateRowSeparator;
  var
    AParams: TcxViewParams;
    AGridView: TcxGridCardView;
  begin
    AGridView := TcxGridCardView(GridCardRow.GridView);
    FRowSeparator := TdxReportCellBox(AddDataItem(TdxReportCellBox));
    FRowSeparator.BoundsRect := Rect(0, 0, Width, AGridView.OptionsView.CategorySeparatorWidth);
    AGridView.Styles.GetViewParams(vsCategorySeparator, nil, nil, AParams);
    FRowSeparator.Color := AParams.Color;
    FRowSeparator.Transparent := False;
    FRowSeparator.CellSides := [];
  end;

begin
  if HasRowCaption then
    CreateRowCaption;
  if HasRowData then
    CreateRowData;
  if NeedSeparator then
    CreateRowSeparator;
  Width := Width;
  Height := Height;
end;

function TdxReportCardRow.GetCard: TdxReportCard;
begin
  Result := TdxReportCard(Parent.Parent);
end;

function TdxReportCardRow.GetGridCardRow: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(Data);
end;

function TdxReportCardRow.GetHasCaption: Boolean;
begin
  Result := FRowCaption <> nil;
end;

function TdxReportCardRow.GetHasData: Boolean;
begin
  Result := FRowData <> nil;
end;

function TdxReportCardRow.GetHasIndent: Boolean;
begin
  Result := FRowIndent <> nil;
end;

function TdxReportCardRow.GetHasRowSeparator: Boolean;
begin
  Result := FRowSeparator <> nil;
end;

function TdxReportCardRow.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TdxReportCardRow.GetLayer: TdxReportCardLayer;
begin
  Result := Parent as TdxReportCardLayer;
end;

function TdxReportCardRow.GetProperties: TcxCustomEditProperties;
begin
  Result := GridCardRow.GetProperties;
end;

function TdxReportCardRow.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TdxReportCardRow.SetHeight(AValue: Integer);
begin
  inherited Height := AValue;
  if HasRowSeparator then
  begin
    Dec(AValue, RowSeparator.Height);
    RowSeparator.Top := AValue;
  end;
  if HasCaption then
    RowCaption.Height := AValue;
  if HasData then
    RowData.Height := AValue;
  if HasIndent then
    RowIndent.Height := AValue;
end;

procedure TdxReportCardRow.SetRowCaptionWidth(Value: Integer);
begin
  if Value > Width then
    Value := Width - 1;
  if not HasData then
    Value := Width;
  if HasIndent then
    Dec(Value, RowIndent.Width);
  if HasCaption then
  begin
    RowCaption.Width := Value;
    if HasData then
      RowData.Left := RowCaption.Left + RowCaption.Width;
  end;
  Width := Width;
end;

procedure TdxReportCardRow.SetWidth(Value: Integer);
begin
  inherited Width := Value;
  if HasRowSeparator then
    RowSeparator.Width := Value;
  if HasCaption and (RowCaption.Width > Width) then
    RowCaption.Width := Width - RowCaption.Left;
  if HasData then
    RowData.Width := Width - RowData.Left
  else
    if HasCaption then
      RowCaption.Width := Width;
end;

{ TdxCardBorderPainter }

function TdxCardBorderPainter.Card: TdxReportCard;
begin
  Result := inherited Item as TdxReportCard;
end;

procedure TdxCardBorderPainter.DrawBorders(
  ACanvas: TdxPSReportRenderCustomCanvas; const R: TRect);
begin
  if Card.HasShadow then
    DrawShadow(ACanvas);
end;

procedure TdxCardBorderPainter.DrawShadow(ACanvas: TdxPSReportRenderCustomCanvas);
begin
  Renderer.FillRect(ACanvas, Card.ShadowRectHorz, Card.ShadowColor);
  Renderer.FillRect(ACanvas, Card.ShadowRectVert, Card.ShadowColor);
end;

{ TdxReportCustomLayoutRecord }

constructor TdxReportCustomLayoutRecord.CreateEx(
  AParent: TdxReportCell; AGridRecord: TcxGridCustomLayoutRecord);
begin
  Create(AParent);
  Data := TdxNativeInt(AGridRecord);
end;

procedure TdxReportCustomLayoutRecord.AdjustRecordWidth;
begin
end;

{ TdxReportCard }

constructor TdxReportCard.Create(AParent: TdxReportCell);
begin
  inherited;
  ShadowColor := clBlack;
  ShadowDepth := dxDefaultCardsShadowDepth;
  ShadowPosition := cspBottomRight;
end;

function TdxReportCard.GetBorderOuterBounds(ACanvas: TdxPSReportRenderCustomCanvas): TRect;
begin
  Result := inherited GetBorderBounds(ACanvas);
  with Result do
  begin
    case ShadowPosition of
      cspTopLeft:
        begin
          Inc(Left, ShadowDepth);
          Inc(Top, ShadowDepth);
        end;
      cspTopRight:
        begin
          Dec(Right, ShadowDepth);
          Inc(Top, ShadowDepth);
        end;
      cspBottomRight:
        begin
          Dec(Right, ShadowDepth);
          Dec(Bottom, ShadowDepth);
        end;
      cspBottomLeft:
        begin
          Inc(Left, ShadowDepth);
          Dec(Bottom, ShadowDepth);
        end;
    end;
  end;
end;

procedure TdxReportCard.AddVerticalDelimiters;
var
  I: Integer;
begin
  for I := 0 to CellCount - 1 do
    ReportLink.AddVerticalDelimiter(Cells[I]);
end;

procedure TdxReportCard.AdjustRecordWidth;
begin
  inherited;
  AdjustLayers;
end;

procedure TdxReportCard.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited;
  FShadowDepth := MulDiv(FShadowDepth, APixelsNumerator, APixelsDenominator);
end;

function TdxReportCard.GetBorderPainterClass: TdxPSCellBorderPainterClass;
begin
  Result := TdxCardBorderPainter;
end;

procedure TdxReportCard.ReadProperties(AReader: TdxPSDataReader);
begin
  inherited ReadProperties(AReader);
  with AReader do
  begin
    FCaptionWidth := ReadInteger;
    FShadowColor := ReadInteger;
    FShadowDepth := ReadInteger;
    FShadowPosition := TdxGridCardShadowPosition(ReadInteger);
  end;
end;

procedure TdxReportCard.WriteProperties(AWriter: TdxPSDataWriter);
begin
  inherited WriteProperties(AWriter);
  with AWriter do
  begin
    WriteInteger(FCaptionWidth);
    WriteInteger(ShadowColor);
    WriteInteger(ShadowDepth);
    WriteInteger(Integer(ShadowPosition));
  end;
end;

procedure TdxReportCard.SetCaptionWidth(
  ACanvas: TdxPSReportRenderCustomCanvas; ALayer, AValue, AMaxValue: Integer; ACaptionAutoWidths: Boolean);
var
  I: Integer;
begin
  AValue := Max(AValue, 0);
  FLayerCaptionWidth := AValue;
  for I := 0 to LayerCount - 1 do
    Layers[I].SetRowCaptionWidth(ACanvas, ACaptionAutoWidths, AMaxValue);
end;

procedure TdxReportCard.AdjustLayers;
begin
end;

procedure TdxReportCard.CreateLayers(ABuilder: TdxGridCardViewBuilder);
var
  List: TList;
  ACurIndex, APrevIndex, I: Integer;
  ARow: TcxGridCardViewRow;
  ALayer: TdxReportCardLayer;
  ANeedLayerSeparator: Boolean;
begin
  List := TList.Create;
  try
    ABuilder.Adapter.GetVisibleCardRows(GridCard, List);
    ALayer := nil;
    AllocateSpaceForCells(List.Count);
    APrevIndex := -1;
    ANeedLayerSeparator := False;
    for I := 0 to List.Count - 1 do
    begin
      ARow := TcxGridCardViewRow(List[I]);
      ACurIndex := GetLayerIndexByRow(ARow);
      if ACurIndex <> APrevIndex then
      begin
        ALayer := GetLayerClass.CreateEx(Self, ARow);
        APrevIndex := ACurIndex;
      end
      else
      begin
        ALayer.AddRow(ARow);
        ANeedLayerSeparator := True;
      end;
    end;
    ANeedLayerSeparator := ANeedLayerSeparator and
      (GridCard.GridView.OptionsView.LayerSeparatorWidth > 0);
    for I := 0 to LayerCount - 1 do
    begin
      if I < (LayerCount - 1) then
        Layers[I].CheckNeedCategorySeparator(Layers[I + 1]);
      Layers[I].NeedLayerSeparator := ANeedLayerSeparator and HasLayerSeparator(I);
      Layers[I].CreateItems(ABuilder);
      Layers[I].Width := Layers[I].Width;
    end;
    AdjustLayers;
  finally
    List.Free;
  end;
end;

function TdxReportCard.GetLayerClass: TdxReportCardLayerClass;
begin
  Result := TdxReportCardLayer;
end;

function TdxReportCard.GetLayerIndexByRow(ARow: TcxGridCardViewRow): Integer;
begin
  Result := ARow.Position.RowIndex;
end;

function TdxReportCard.GetLayerWidth(Index: Integer): Integer;
begin
  Result := ContentWidth;
end;

function TdxReportCard.HasLayerSeparator(ALayerIndex: Integer): Boolean;
begin
  Result := ALayerIndex < (LayerCount - 1);
end;

procedure TdxReportCard.RecalculateLayersHeight;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].RecalculateHeight;
end;

procedure TdxReportCard.SetLayerHeight(Index, Value: Integer);
var
  Delta, I: Integer;
begin
  if Value < 0 then Value := 0;

  Delta := Value - Layers[Index].Height;
  Layers[Index].Height := Value;

  for I := Index + 1 to LayerCount - 1 do
    Layers[I].Top := Layers[I].Top + Delta;
end;

procedure TdxReportCard.SetLayerWidth(Index, Value: Integer);
begin
  Layers[Index].Width := Value;
end;

function TdxReportCard.GetRowCount: Integer;
begin
  Result := LayerCount;
end;

function TdxReportCard.GetRowHeight(AIndex: Integer): Integer;
begin
  Result := LayerHeights[AIndex];
end;

procedure TdxReportCard.SetRowHeight(AIndex, AValue: Integer);
begin
  LayerHeights[AIndex] := AValue;
end;

procedure TdxReportCard.SizeChanged;
begin
end;

function TdxReportCard.GetGridCard: TcxGridCard;
begin
  Result := TcxGridCard(Data);
end;

function TdxReportCard.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TdxReportCard.GetContentHeight: Integer;
begin
  Result := Height - ShadowDepth;
end;

function TdxReportCard.GetContentWidth: Integer;
begin
  Result := Width - ShadowDepth;
end;

function TdxReportCard.GetHasShadow: Boolean;
begin
  Result := FShadowDepth > 0;
end;

function TdxReportCard.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TdxReportCard.GetLayerCount: Integer;
begin
  Result := CellCount;
end;

function TdxReportCard.GetLayer(Index: Integer): TdxReportCardLayer;
begin
  Result := Cells[Index] as TdxReportCardLayer;
end;

function TdxReportCard.GetLayerHeight(Index: Integer): Integer;
begin
  Result := Layers[Index].Height;
end;

function TdxReportCard.GetLayersOriginLeft: Integer;
begin
  if ShadowPosition in [cspTopLeft, cspBottomLeft] then
    Result := ShadowDepth
  else
    Result := 0;
end;

function TdxReportCard.GetLayersOriginTop: Integer;
begin
  if ShadowPosition in [cspTopLeft, cspTopRight] then
    Result := ShadowDepth
  else
    Result := 0;
end;

function TdxReportCard.GetReportLink: TdxGridReportLink;
begin
  Result := TdxGridReportLink(ReportCells.ReportLink);
end;

function TdxReportCard.GetShadowRectHorz: TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  with Result do
    case ShadowPosition of
      cspTopLeft:
        begin
          Right := Right - ShadowDepth;
          Bottom := Top + ShadowDepth;
        end;
      cspTopRight:
        begin
          Left := Left + ShadowDepth;
          Bottom := Top + ShadowDepth;
        end;
      cspBottomRight:
        begin
          Left := Left + ShadowDepth;
          Top := Bottom - ShadowDepth;
        end;
      cspBottomLeft:
        begin
          Right := Right - ShadowDepth;
          Top := Bottom - ShadowDepth;
        end;
    end;
end;

function TdxReportCard.GetShadowRectVert: TRect;
begin
  Result := BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  with Result do
    case ShadowPosition of
      cspTopLeft:
        begin
          Right := Left + ShadowDepth;
          Top := Top + ShadowDepth;
          Bottom := Bottom - ShadowDepth;
        end;
      cspTopRight:
        begin
          Left := Right - ShadowDepth;
          Top := Top + ShadowDepth;
          Bottom := Bottom - ShadowDepth;
        end;
      cspBottomRight:
        begin
          Left := Right - ShadowDepth;
          Top := Top + ShadowDepth;
          Bottom := Bottom - ShadowDepth;
        end;
      cspBottomLeft:
        begin
          Right := Left + ShadowDepth;
          Top := Top + ShadowDepth;
          Bottom := Bottom - ShadowDepth;
        end;
    end;
end;

procedure TdxReportCard.SetContentHeight(AValue: Integer);
begin
  Height := AValue + ShadowDepth;
end;

procedure TdxReportCard.SetContentWidth(AValue: Integer);
begin
  Width := AValue + ShadowDepth;
end;

procedure TdxReportCard.SetHeight(Value: Integer);
begin
  inherited Height := Value + ShadowDepth;
  SizeChanged;
end;

procedure TdxReportCard.SetShadowDepth2(AValue: Integer);
var
  SaveWidth, SaveHeight: Integer;
begin
  if FShadowDepth <> AValue then
  begin
    SaveHeight := ContentHeight;
    SaveWidth := ContentWidth;
    FShadowDepth := AValue;
    Height := SaveHeight;
    Width := SaveWidth;
  end;
end;

procedure TdxReportCard.SetWidth(Value: Integer);
begin
  inherited Width := Value + ShadowDepth;
  SizeChanged;
end;

{ TdxReportCardHorz }

procedure TdxReportCardHorz.AdjustHeight;
var
  TotalHeight, I: Integer;
begin
  TotalHeight := 0;
  for I := 0 to LayerCount - 1 do
    Inc(TotalHeight, LayerHeights[I]);
  Height := TotalHeight;
end;

procedure TdxReportCardHorz.AdjustLayersCellSides;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].AdjustCellSides;
end;

procedure TdxReportCardHorz.AdjustLayersPos;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    with Layers[I] do
    begin
      Left := LayersOriginLeft;
      Top := Top + LayersOriginTop;
    end;
end;

procedure TdxReportCardHorz.AdjustRowHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; AAutoHeight: Boolean;
  ABuilder: TdxGridCardViewBuilder);
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    LayerHeights[I] := Layers[I].MeasureLayerHeight(ACanvas, not AAutoHeight, ABuilder);
end;

procedure TdxReportCardHorz.AdjustWidth(ACanvas: TdxPSReportRenderCustomCanvas);
var
  AMaxLayerWidth, I: Integer;
begin
  AMaxLayerWidth := 0;
  for I := 0 to LayerCount - 1 do
    AMaxLayerWidth := Max(AMaxLayerWidth, Layers[I].MeasureWidth(ACanvas));
  Width := AMaxLayerWidth;
end;

function TdxReportCardHorz.MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LayerCount - 1 do
    Result := Max(Result, Layers[I].MeasureCaptionWidth(ACanvas));
end;

function TdxReportCardHorz.MeasureDataWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LayerCount - 1 do
    Result := Max(Result, Layers[I].MeasureDataWidth(ACanvas));
end;

procedure TdxReportCardHorz.SizeChanged;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].Width := ContentWidth;
end;

{ TdxReportCardVert }

procedure TdxReportCardVert.AdjustHeight;
var
  TotalHeight, I: Integer;
begin
  TotalHeight := 0;
  for I := 0 to LayerCount - 1 do
    TotalHeight := Max(TotalHeight, LayerHeights[I]);
  Height := TotalHeight;
end;

procedure TdxReportCardVert.AdjustLayers;
var
  I, ALeft: Integer;
  AAllFixed: Boolean;
  AAutoWidths: TcxAutoWidthObject;
begin
  if LayerCount = 0 then Exit;
  AAutoWidths := TcxAutoWidthObject.Create(LayerCount);
  try
    AAllFixed := True;
    for I := 0 to LayerCount - 1 do
      Layers[I].InitAutoWidthItem(AAutoWidths.AddItem, AAllFixed);
    if AAllFixed or (AAutoWidths.Width > Width) then
      for I := 0 to AAutoWidths.Count - 1 do
        AAutoWidths[I].Fixed := False;
    AAutoWidths.AvailableWidth := ContentWidth;
    AAutoWidths.Calculate;
    ALeft := 0;
    for I := 0 to LayerCount - 1 do
    begin
      Layers[I].NeedLayerSeparator := I > 0;
      ALeft := Layers[I].SetPosition(ALeft, AAutoWidths[I].AutoWidth);
    end;
  finally
    AAutoWidths.Free;
  end;
end;

procedure TdxReportCardVert.AdjustLayersCellSides;
var
  I: Integer;
  ASides: TdxCellSides;
begin
  for I := 0 to LayerCount - 1 do
  begin
    ASides := [csTop, csBottom];
    if I = 0 then
      ASides := ASides + [csLeft];
    if I = LayerCount - 1 then
      ASides := ASides + [csRight];
    Layers[I].CellSides := ASides;
    Layers[I].AdjustCellSides;
  end;
end;

procedure TdxReportCardVert.AdjustLayersPos;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    with Layers[I] do Top := Top + LayersOriginTop;
end;

procedure TdxReportCardVert.AdjustRowHeight(ACanvas: TdxPSReportRenderCustomCanvas;
  AAutoHeight: Boolean; ABuilder: TdxGridCardViewBuilder);
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].AdjustRowHeight(ACanvas, AAutoHeight, ABuilder);
end;

procedure TdxReportCardVert.AdjustWidth(ACanvas: TdxPSReportRenderCustomCanvas);
var
  AMaxWidth, I: Integer;
begin
  AMaxWidth := 0;
  for I := 0 to LayerCount - 1 do
    Inc(AMaxWidth, Layers[I].MeasureWidth(ACanvas));
  Width := AMaxWidth;
end;

function TdxReportCardVert.GetLayerClass: TdxReportCardLayerClass;
begin
  Result := TdxReportCardVerticalLayer;
end;

function TdxReportCardVert.GetLayerByAbsoluteRowIndex(var AIndex: Integer): TdxReportCardLayer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to LayerCount - 1 do
  begin
    Result := Layers[I];
    if AIndex < Result.RowCount then
      Break
    else
      Dec(AIndex, Result.RowCount);
  end;
end;

function TdxReportCardVert.GetLayerIndexByRow(
  ARow: TcxGridCardViewRow): Integer;
begin
  Result := ARow.Position.ColIndex;
end;

function TdxReportCardVert.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LayerCount - 1 do
    Inc(Result, Layers[I].RowCount);
end;

function TdxReportCardVert.GetRowHeight(AIndex: Integer): Integer;
var
  ALayer: TdxReportCardLayer;
begin
  ALayer := GetLayerByAbsoluteRowIndex(AIndex);
  Result := ALayer.Rows[AIndex].Height;
end;

function TdxReportCardVert.HasLayerSeparator(ALayerIndex: Integer): Boolean;
begin
  Result := ALayerIndex > 0;
end;

function TdxReportCardVert.MeasureCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LayerCount - 1 do
    Result := Max(Result, Layers[I].MeasureCaptionWidth(ACanvas));
end;

procedure TdxReportCardVert.SetCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas;
  ALayer, AValue, AMaxValue: Integer; ACaptionAutoWidth: Boolean);
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[ALayer].SetRowCaptionWidth(ACanvas, ACaptionAutoWidth, AValue);
end;

procedure TdxReportCardVert.SetLayerWidth(Index, Value: Integer);
var
  Delta, I: Integer;
begin
  if Value < 0 then Value := 0;

  Delta := Value - Layers[Index].Height;
  Layers[Index].Width := Value;

  for I := Index + 1 to LayerCount - 1 do
    Layers[I].Left := Layers[I].Left + Delta;
end;

procedure TdxReportCardVert.SetLayerHeight(AIndex, AValue: Integer);
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].Height := AValue;
end;

procedure TdxReportCardVert.SetLayersSameHeight;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
  begin
    Layers[I].Height := ContentHeight;
    Layers[I].ClipChildren := True;
  end;
end;

procedure TdxReportCardVert.SetRowHeight(AIndex, AValue: Integer);
var
  ALayer: TdxReportCardLayer;
begin
  ALayer := GetLayerByAbsoluteRowIndex(AIndex);
  ALayer.SetRowHeight(AIndex, AValue);
end;

procedure TdxReportCardVert.SizeChanged;
begin
  AdjustLayers;
  SetLayersSameHeight;
end;

{ TdxGridCustomLayoutViewBuilder }

constructor TdxGridCustomLayoutViewBuilder.Create(AReportLink: TdxGridReportLink; AMasterBuilder: TdxCustomGridViewBuilder;
  AGridView: TcxCustomGridView);
begin
  inherited;
  FRecords := TList.Create;
end;

destructor TdxGridCustomLayoutViewBuilder.Destroy;
begin
  FreeAndNil(FRecords);
  inherited;
end;

class function TdxGridCustomLayoutViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridCustomLayoutViewAdapter;
end;

function TdxGridCustomLayoutViewBuilder.Formatter: TdxGridCustomLayoutViewFormatter;
begin
  Result := inherited Formatter as TdxGridCustomLayoutViewFormatter;
end;

class function TdxGridCustomLayoutViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridCustomLayoutViewFormatter;
end;

procedure TdxGridCustomLayoutViewBuilder.DoBuildViewBody;
begin
  CreateRecords;
  if not IsAborted then
  begin
    ResizeRecords;
    PlaceRecords;
  end;
end;

procedure TdxGridCustomLayoutViewBuilder.CreateRecords;
var
  I: Integer;
  AProducer: TdxGridCustomLayoutViewRecordProducer;
begin
  AProducer := GetRecordProducer;
  FRecords.Clear;
  for I := 0 to Formatter.RecordCount - 1 do
  begin
    FRecords.Add(AProducer.CreateRecord(Host, Formatter.Records[I]));
    if MasterBuilder = nil then
    begin
      Progress(100 * (I + 1) / Formatter.RecordCount);
      if IsAborted then Break;
    end;
  end;
end;

procedure TdxGridCustomLayoutViewBuilder.PlaceRecords;

  procedure MakeRowIndexes(AIndexes: TList);
  var
    I, APlaceWidth, AEntryNumber: Integer;
  begin
    I := 0;
    if RecordCount > 0 then
      AIndexes.Add(TObject(0));

    while I < RecordCount do
    begin
      APlaceWidth := Formatter.GetRecordsAreaWidth;
      AEntryNumber := 0;
      repeat
        Inc(AEntryNumber);
        if AEntryNumber > 1 then
          Dec(APlaceWidth, Formatter.GetInterRecordsSpaceHorz);
        Dec(APlaceWidth, Records[I].Width);
        if (APlaceWidth >= 0) or (AEntryNumber = 1) then
          Inc(I);
      until (APlaceWidth <= 0) or (I >= RecordCount);
      AIndexes.Add(TObject(I));
    end;
  end;

  procedure MakeRows(AIndexes: TList; AProducer: TdxGridCustomLayoutViewRecordProducer);

    function GetMaxRecordHeight(AStartIndex, AEndIndex: Integer): Integer;
    var
      I: Integer;
    begin
      Result := Records[AStartIndex].Height;
      for I := AStartIndex + 1 to AEndIndex do
        Result := Max(Result, Records[I].Height);
    end;

  var
    ARowIndex, AStartIndex, AEndIndex, I, ARowHeight: Integer;
    AReportRow: TdxReportCell;
  begin
    for ARowIndex := 0 to AIndexes.Count - 2 do
    begin
      AStartIndex := Integer(AIndexes[ARowIndex]);
      AEndIndex := Integer(AIndexes[ARowIndex + 1]) - 1;

      ARowHeight := GetMaxRecordHeight(AStartIndex, AEndIndex) + Formatter.GetInterRecordsSpaceVert;
      AReportRow := AProducer.Produce(HostInfoServices.PageDetailsHostInfo, ARowHeight);
      AddReportRow(AReportRow);

      for I := AStartIndex to AEndIndex do
        AProducer.InjectRecord(Records[I], I - AStartIndex);
    end;
  end;

var
  AIndexes: TList;
begin
  AIndexes := TList.Create;
  try
    MakeRowIndexes(AIndexes);
    MakeRows(AIndexes, GetRecordProducer);
  finally
    AIndexes.Free;
  end;
end;

procedure TdxGridCustomLayoutViewBuilder.DoResizeRecords;
var
  ARecordColCount: Integer;
begin
  CalculateMaxRecordWidth;
  if Formatter.CardAutoWidth then
  begin
    ARecordColCount := Max((Formatter.ViewAvailableWidth - 4) div FMaxRecordWidth, 1);
    FMaxRecordWidth := (Formatter.ViewAvailableWidth - (4 * ARecordColCount)) div ARecordColCount;
    SetSameRecordsWidth(FMaxRecordWidth - 4);
  end;
end;

procedure TdxGridCustomLayoutViewBuilder.ResizeRecords;
begin
  FMaxRecordWidth := 0;
  if RecordCount > 0 then
    DoResizeRecords;
end;

procedure TdxGridCustomLayoutViewBuilder.SetSameRecordsWidth(AWidth: Integer);
var
  I: Integer;
begin
  for I := 0 to RecordCount - 1 do
    with Records[I] do
    begin
      Width := AWidth;
      AdjustRecordWidth;
    end;
end;

procedure TdxGridCustomLayoutViewBuilder.CalculateMaxRecordWidth;
var
  I: Integer;
begin
  FMaxRecordWidth := 0;
  for I := 0 to RecordCount - 1 do
    FMaxRecordWidth := Max(FMaxRecordWidth, Records[I].Width);
end;

function TdxGridCustomLayoutViewBuilder.GetRecordProducer: TdxGridCustomLayoutViewRecordProducer;
begin
  Result := ProducerCache[GetRecordProducerClass] as TdxGridCustomLayoutViewRecordProducer;
end;

function TdxGridCustomLayoutViewBuilder.GetRecordProducerClass: TdxGridCustomLayoutViewRecordProducerClass;
begin
  Result := TdxGridCustomLayoutViewRecordProducer;
end;

function TdxGridCustomLayoutViewBuilder.CreateRecord(AParent: TdxReportCell; ARecord: TcxGridCustomLayoutRecord): TdxReportCell;
begin
  Result := nil;
end;

function TdxGridCustomLayoutViewBuilder.GetRecord(Index: Integer): TdxReportCustomLayoutRecord;
begin
  Result := TdxReportCustomLayoutRecord(FRecords[Index]);
end;

function TdxGridCustomLayoutViewBuilder.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

{ TdxGridCardViewBuilder }

function TdxGridCardViewBuilder.Adapter: TdxGridCardViewAdapter;
begin
  Result := inherited Adapter as TdxGridCardViewAdapter;
end;

class function TdxGridCardViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridCardViewAdapter;
end;

function TdxGridCardViewBuilder.Formatter: TdxGridCardViewFormatter;
begin
  Result := inherited Formatter as TdxGridCardViewFormatter;
end;

class function TdxGridCardViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridCardViewFormatter;
end;

procedure TdxGridCardViewBuilder.DoBuildViewBody;
begin
  inherited;
  if not IsAborted then
    AdjustCardRowsCellSides;
end;

procedure TdxGridCardViewBuilder.AdjustCardRowsCellSides;
var
  I: Integer;
begin
  for I := 0 to CardCount - 1 do
    Cards[I].AdjustLayersCellSides;
end;

procedure TdxGridCardViewBuilder.DoResizeRecords;

  procedure SetCardsRowCaptionWidth(ACanvas: TdxPSReportRenderCustomCanvas);
  var
    ACard: TdxReportCard;
    ALayerCaptionWidths: array of Integer;
    ALayerCount: Integer;
    ARowCaptionWidth, I, J: Integer;
  begin
    if CardCount = 0 then
      Exit;

    ALayerCount := 0;
    for I := 0 to CardCount - 1 do
      ALayerCount := Max(ALayerCount, Cards[I].LayerCount);
    SetLength(ALayerCaptionWidths, ALayerCount);

    if Formatter.CaptionAutoWidth then
    begin
      for I := 0 to CardCount - 1 do
      begin
        ACard := Cards[I];
        for J := 0 to ACard.LayerCount - 1 do
          ALayerCaptionWidths[J] := Max(ALayerCaptionWidths[J], ACard.Layers[J].MeasureCaptionWidth(ACanvas));
      end;
    end
    else
      for I := 0 to Length(ALayerCaptionWidths) - 1 do
        ALayerCaptionWidths[I] := Adapter.CardCaptionWidth;

    ARowCaptionWidth := 0;
    for I := 0 to Length(ALayerCaptionWidths) - 1 do
      ARowCaptionWidth := Max(ARowCaptionWidth, ALayerCaptionWidths[I]);
    if (ARowCaptionWidth > Adapter.CardWidth) and not Formatter.AutoWidth then
      ARowCaptionWidth := Adapter.CardWidth;

    for I := 0 to CardCount - 1 do
      for J := 0 to Length(ALayerCaptionWidths) - 1 do
        Cards[I].SetCaptionWidth(ACanvas, J, Min(ALayerCaptionWidths[J], ARowCaptionWidth), ARowCaptionWidth, Formatter.CaptionAutoWidth);
  end;

  procedure SetCardsAutoWidth(ACanvas: TdxPSReportRenderCustomCanvas);
  var
    I: Integer;
  begin
    for I := 0 to CardCount - 1 do
      Cards[I].AdjustWidth(ACanvas);
  end;

  procedure SetCardsRowHeight(ACanvas: TdxPSReportRenderCustomCanvas; AAutoHeight: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to CardCount - 1 do
      Cards[I].AdjustRowHeight(ACanvas, AAutoHeight, Self);
  end;

  procedure SetCardsSameHeights;
  var
    CardRowHeights: array of Integer;
    I, J: Integer;
  begin
    SetLength(CardRowHeights, Adapter.CardRowCount);

    for I := 0 to CardCount - 1 do
      with Cards[I] do
      begin
        for J := 0 to RowCount - 1 do
          CardRowHeights[J] := Max(CardRowHeights[J], RowHeight[J]);
      end;

    for I := 0 to CardCount - 1 do
      with Cards[I] do
      begin
        for J := 0 to RowCount - 1 do
          RowHeight[J] := CardRowHeights[J];
        RecalculateLayersHeight;
      end;
  end;

  procedure AdjustCards;
  var
    I: Integer;
  begin
    for I := 0 to CardCount - 1 do
      with Cards[I] do
      begin
        AdjustHeight;
        AdjustLayersPos;
      end;
  end;

begin
  SetCardsRowCaptionWidth(Formatter.Canvas);
  if Formatter.AutoWidth then
  begin
    SetCardsAutoWidth(Formatter.Canvas);
    CalculateMaxRecordWidth;
    if Formatter.KeepSameWidth then
      SetSameRecordsWidth(FMaxRecordWidth - 4);
    SetCardsRowCaptionWidth(Formatter.Canvas);
  end;
  inherited;
  SetCardsRowHeight(Formatter.Canvas, Formatter.RowAutoHeight);
  if Formatter.KeepSameHeight then
    SetCardsSameHeights;

  AdjustCards;
end;

function TdxGridCardViewBuilder.GetRecordProducer: TdxGridCardViewCardsRowProducer;
begin
  Result := inherited GetRecordProducer as TdxGridCardViewCardsRowProducer;
end;

function TdxGridCardViewBuilder.GetRecordProducerClass: TdxGridCustomLayoutViewRecordProducerClass;
begin
  Result := TdxGridCardViewCardsRowProducer;
end;

function TdxGridCardViewBuilder.GridView: TcxGridCardView;
begin
  Result := inherited GridView as TcxGridCardView;
end;

class function TdxGridCardViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

function TdxGridCardViewBuilder.GetCard(Index: Integer): TdxReportCard;
begin
  Result := TdxReportCard(inherited Records[Index]);
end;

function TdxGridCardViewBuilder.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

{ TdxGridDBCardViewAdapter }

function TdxGridDBCardViewAdapter.GridView: TcxGridDBCardView;
begin
  Result := inherited GridView as TcxGridDBCardView;
end;

class function TdxGridDBCardViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridCardView;
end;

function TdxGridDBCardViewAdapter.DataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(inherited DataController);
end;

function TdxGridDBCardViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := DataController.DataModeController;
end;

{ TdxGridDBCardViewBuilder }

function TdxGridDBCardViewBuilder.Adapter: TdxGridDBCardViewAdapter;
begin
  Result := inherited Adapter as TdxGridDBCardViewAdapter;
end;

class function TdxGridDBCardViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridDBCardViewAdapter;
end;

{ TdxGridChartViewAdapter }

function TdxGridChartViewAdapter.GridView: TcxGridChartView;
begin
  Result := inherited GridView as TcxGridChartView;
end;

class function TdxGridChartViewAdapter.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridChartView;
end;

function TdxGridChartViewAdapter.CreateGraphic(AGraphicClass: TGraphicClass; AWidth: Integer): TGraphic;
var
  AGraphic: TGraphic;
begin
  if not (AGraphicClass.InheritsFrom(TBitmap) or AGraphicClass.InheritsFrom(TMetaFile)) then
  begin
    Result := GridView.CreateImage(TBitmap, Max(AWidth, 0), 0);
    AGraphic := AGraphicClass.Create;
    try
      AGraphic.Assign(Result);
    finally
      Result.Free;
      Result := AGraphic;
    end;
  end
  else
    Result := GridView.CreateImage(AGraphicClass, Max(AWidth, 0), 0);
end;

function TdxGridChartViewAdapter.GetCanUseOnEveryPageMode: Boolean;
begin
  Result := True;
end;

{ TdxGridChartViewFormatter }

function TdxGridChartViewFormatter.Adapter: TdxGridChartViewAdapter;
begin
  Result := inherited Adapter as TdxGridChartViewAdapter;
end;

function TdxGridChartViewFormatter.Builder: TdxGridChartViewBuilder;
begin
  Result := inherited Builder as TdxGridChartViewBuilder;
end;

function TdxGridChartViewFormatter.CreateChartImage: TGraphic;

  function GetWidth: Integer;
  begin
    if ReportLink.OptionsSize.AutoWidth then
      Result := ViewAvailableWidth
    else
      Result := -1;
  end;

begin
  Result := Adapter.CreateGraphic(GraphicClass, GetWidth);
end;

procedure TdxGridChartViewFormatter.DoInitializeChartItem(AItem: TdxReportCellDpiAwareGraphic);
begin
  AItem.ImageBuffering := cibNone;
  AItem.Image.Transparent := GraphicTransparent;
  AItem.ImageSourceDPI := dxGetScaleFactor(Adapter.GridView).TargetDPI;
  AItem.ImageTransparent := GraphicTransparent;
  AItem.Center := True;
end;

procedure TdxGridChartViewFormatter.DoReportLinkInitializeChartItem(AItem: TdxReportCellGraphic);
begin
  ReportLink.DoInitializeChartCell(Adapter.GridView, AItem);
end;

function TdxGridChartViewFormatter.GetChartItemClass: TdxReportCellGraphicClass;
begin
  Result := TdxReportCellDpiAwareGraphic;
end;

function TdxGridChartViewFormatter.GetViewWidth: Integer;
begin
  Result := ViewAvailableWidth;
end;

function TdxGridChartViewFormatter.GetGraphicClass: TGraphicClass;
begin
  Result := ReportLink.OptionsCharts.GraphicClass;
end;

function TdxGridChartViewFormatter.GetGraphicTransparent: Boolean;
begin
  Result := ReportLink.OptionsCharts.Transparent;
end;

{ TdxGridChartViewBuilder }

function TdxGridChartViewBuilder.Adapter: TdxGridChartViewAdapter;
begin
  Result := inherited Adapter as TdxGridChartViewAdapter;
end;

class function TdxGridChartViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridChartViewAdapter;
end;

function TdxGridChartViewBuilder.Formatter: TdxGridChartViewFormatter;
begin
  Result := inherited Formatter as TdxGridChartViewFormatter;
end;

class function TdxGridChartViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridChartViewFormatter;
end;

procedure TdxGridChartViewBuilder.DoBuildViewBody;
begin
  inherited;
  CreateChart;
end;

procedure TdxGridChartViewBuilder.CreateChart;
var
  ReportRow: TdxReportCell;
begin
  ReportRow := GetChartProducer.Produce(HostInfoServices.PageDetailsHostInfo);
  AddReportRow(ReportRow);
end;

function TdxGridChartViewBuilder.GetChartProducer: TdxGridChartViewChartProducer;
begin
  Result := ProducerCache[GetChartProducerClass] as TdxGridChartViewChartProducer;
end;

function TdxGridChartViewBuilder.GetChartProducerClass: TdxGridChartViewChartProducerClass;
begin
  Result := TdxGridChartViewChartProducer;
end;

function TdxGridChartViewBuilder.GridView: TcxGridChartView;
begin
  Result := inherited GridView as TcxGridChartView;
end;

class function TdxGridChartViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridChartView;
end;

{ TdxGridViewRowProducer }

constructor TdxGridViewRowProducer.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
  FMasterIndents := TList.Create;
end;

destructor TdxGridViewRowProducer.Destroy;
begin
  FMasterIndents.Free;
  inherited;
end;

function TdxGridViewRowProducer.Adapter: TdxCustomGridViewAdapter;
begin
  Result := Builder.Adapter;
end;

function TdxGridViewRowProducer.Builder: TdxCustomGridViewBuilder;
begin
  Result := FBuilder;
end;

function TdxGridViewRowProducer.Formatter: TdxCustomGridViewFormatter;
begin
  Result := Builder.Formatter;
end;

function TdxGridViewRowProducer.Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell;
begin
  FMasterIndents.Clear;

  CalculateRowHeight;

  CreateRowHost(AHostInfo);
  CreateRow;
  if AutoHeight then
  begin
    CalculateRowAutoHeight;
    FixupRowHeight;
  end;

  Inc(AHostInfo.Origin.Y, RowHeight);
  Result := Host;
end;

procedure TdxGridViewRowProducer.AddLeadingMasterIndentsAndSeparators;
var
  LeftPos, I: Integer;
begin
  LeftPos := 0;
  for I := 0 to Formatter.AbsoluteLevel - 1 do
    with MasterBuilders[I] do
    begin
      AddMasterSeparator(Formatter, Adapter, LeftPos, vdskLeading);
      AddMasterIndents(Formatter, Adapter, I, LeftPos);
    end;
  AddMasterSeparator(Formatter, Adapter, LeftPos, vdskLeading);
end;

procedure TdxGridViewRowProducer.AddMasterIndents(AMasterFormatter: TdxCustomGridViewFormatter;
  AMasterAdapter: TdxCustomGridViewAdapter; ALevel: Integer; var ALeftPos: Integer);
var
  I: Integer;
  Indent: TdxReportCellExpandButton;
begin
  for I := 0 to AMasterAdapter.IndentCount - 1 do
  begin
    Indent := CreateMasterIndent(Host);
    Indent.BoundsRect := Bounds(ALeftPos, 0, AMasterAdapter.IndentWidth, RowHeight);
    InitializeMasterIndent(AMasterFormatter, Indent, I, ALevel);
    Inc(ALeftPos, Indent.Width);
  end;
end;

procedure TdxGridViewRowProducer.AddMasterSeparator(AMasterFormatter: TdxCustomGridViewFormatter;
  AMasterAdapter: TdxCustomGridViewAdapter; var ALeftPos: Integer;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
var
  Separator: TdxReportCellBox;
begin
  if AMasterAdapter.HasDetailsSeparator then
  begin
    Separator := CreateDetailsSeparator(Host);
    Separator.BoundsRect := Bounds(ALeftPos, 0, AMasterFormatter.DetailsSeparatorThickness, RowHeight);
    InitializeDetailsSeparator(AMasterFormatter, Separator, ASeparatorKind);
    Inc(ALeftPos, Separator.Width);
  end;
end;

procedure TdxGridViewRowProducer.AddTrailingMasterSeparators;
var
  LeftPos, I: Integer;
begin
  LeftPos := Width - Formatter.ViewWidthExtraAfter;
  AddMasterSeparator(Formatter, Adapter, LeftPos, vdskTrailing);
  for I := Formatter.AbsoluteLevel - 1 downto 0 do
    with Builder.AbsoluteBuilders[I] do
      AddMasterSeparator(Formatter, Adapter, LeftPos, vdskTrailing);
end;

function TdxGridViewRowProducer.CalculateItemHeight(
  ACanvas: TdxPSReportRenderCustomCanvas; AnItem: TdxReportVisualItem): Integer;
begin
  Result := AnItem.MeasureContentHeight(Canvas);
end;

procedure TdxGridViewRowProducer.CalculateRowAutoHeight;
var
  ItemsSite: TdxReportCell;
  MaxRowHeight, I: Integer;
  Item: TdxReportVisualItem;
begin
  ItemsSite := Self.ItemsSite;
  MaxRowHeight := 0;
  for I := 0 to ItemsSite.DataItemCount - 1 do
  begin
    Item := ItemsSite.DataItems[I];
    if DoesItemParticipateInRowAutoHeightCalculation(Item) then
      MaxRowHeight := Max(CalculateItemHeight(Canvas, Item), MaxRowHeight);
  end;
  {3.1}
  for I := 0 to ItemsSite.CellCount - 1 do
  begin
    Item := ItemsSite.Cells[I];
    if DoesItemParticipateInRowAutoHeightCalculation(Item) then
      MaxRowHeight := Max(CalculateItemHeight(Canvas, Item), MaxRowHeight);
  end;
  RowHeight := Max(MaxRowHeight, RowHeight);
end;

procedure TdxGridViewRowProducer.CalculateRowHeight;
begin
  FRowHeight := LineCount * LineHeight;
end;

function TdxGridViewRowProducer.CreateDetailsSeparator(AParent: TdxReportCell): TdxReportCellBox;
begin
  Result := GetDetailsSeparatorClass.Create(AParent);
  FMasterIndents.Add(Result);
end;

function TdxGridViewRowProducer.CreateMasterIndent(AParent: TdxReportCell): TdxReportCellExpandButton;
begin
  Result := GetMasterIndentClass.Create(AParent);
  FMasterIndents.Add(Result);
end;

procedure TdxGridViewRowProducer.CreateRow;
begin
  FRow := GetRowHostClass.Create(Host);
  FRow.BoundsRect := Bounds(RowOriginX, 0, RowWidth, RowHeight);
  InitializeRow;
  AddTrailingMasterSeparators;
end;

procedure TdxGridViewRowProducer.CreateRowHost(const AHostInfo: TdxGridAttributeHostInfo);
begin
  FHost := HostClass.Create(AHostInfo.Parent);
  FHost.BoundsRect := Bounds(AHostInfo.Origin.X, AHostInfo.Origin.Y, Width, RowHeight);
  InitializeHost;
  AddLeadingMasterIndentsAndSeparators;
end;

function TdxGridViewRowProducer.DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean;
begin
  Result := True;
end;

function TdxGridViewRowProducer.GetDetailsSeparatorClass: TdxReportCellBoxClass;
begin
  Result := Formatter.GetDetailsSeparatorClass;
end;

function TdxGridViewRowProducer.GetMasterIndentClass: TdxReportCellExpandButtonClass;
begin
  Result := Formatter.GetMasterIndentClass;
end;

procedure TdxGridViewRowProducer.FixupMasterIndentsHeight;
var
  I: Integer;
begin
  for I := 0 to MasterIndentCount - 1 do
    with MasterIndents[I] do
      Height := Parent.Height;
end;

procedure TdxGridViewRowProducer.FixupRowDataItemHeight(AnItem: TdxReportVisualItem);
begin
  with AnItem do
    Height := Parent.Height;
end;

procedure TdxGridViewRowProducer.FixupRowDataHeight;
begin
end;

procedure TdxGridViewRowProducer.FixupRowHeight;
begin
  FixupRowOwnHeight;
  FixupMasterIndentsHeight;
  FixupRowDataHeight;
end;

procedure TdxGridViewRowProducer.FixupRowOwnHeight;
begin
  Host.Height := RowHeight;
  Row.Height := RowHeight;
  ItemsSite.Height := RowHeight;
end;

procedure TdxGridViewRowProducer.InitializeDetailsSeparator(AFormatter: TdxCustomGridViewFormatter;
  ASeparator: TdxReportCellBox; ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  AFormatter.DoInitializeVertDetailsSeparator(ASeparator, ASeparatorKind);
end;

procedure TdxGridViewRowProducer.InitializeMasterIndent(AFormatter: TdxCustomGridViewFormatter;
  AIndent: TdxReportCellExpandButton; AIndex, ALevel: Integer);
begin
  AFormatter.DoInitializeMasterIndent(AIndent, AIndex, MasterBuilders[ALevel].Adapter.IndentCount);
end;

procedure TdxGridViewRowProducer.InitializeHost;
begin
  Formatter.DoInitializeHost(Host);
end;

procedure TdxGridViewRowProducer.InitializeRow;
begin
end;

function TdxGridViewRowProducer.GetAutoHeight: Boolean;
begin
  Result := LineCount = 1;
end;

function TdxGridViewRowProducer.GetItemsSite: TdxReportCell;
begin
  Result := Row;
end;

function TdxGridViewRowProducer.GetLineCount: Integer;
begin
  Result := 1;
end;

function TdxGridViewRowProducer.GetLineHeight: Integer;
begin
  Result := 20;
end;

function TdxGridViewRowProducer.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Formatter.Canvas;
end;

function TdxGridViewRowProducer.GetHostClass: TdxReportCellClass;
begin
  Result := Formatter.GetHostClass;
end;

function TdxGridViewRowProducer.GetIndentWidth: Integer;
begin
  Result := Adapter.IndentWidth;
end;

function TdxGridViewRowProducer.GetMasterBuilder(Index: Integer): TdxCustomGridViewBuilder;
begin
  Result := Builder.AbsoluteBuilders[Index];
end;

function TdxGridViewRowProducer.GetMasterIndent(Index: Integer): TdxReportCellBox;
begin
  Result := TdxReportCellBox(FMasterIndents[Index]);
end;

function TdxGridViewRowProducer.GetMasterIndentCount: Integer;
begin
  Result := FMasterIndents.Count;
end;

function TdxGridViewRowProducer.GetRowHostClass: TdxReportCellClass;
begin
  Result := Formatter.GetRowHostClass;
end;

function TdxGridViewRowProducer.GetWidth: Integer;
begin
  with Formatter do
    Result := ViewWidthExtraBefore + ViewWidth + ViewWidthExtraAfter;
end;

function TdxGridViewRowProducer.GetRowOriginX: Integer;
begin
  Result := Formatter.ViewWidthExtraBefore;
end;

function TdxGridViewRowProducer.GetRowWidth: Integer;
begin
  Result := Formatter.ViewWidth;
end;

{ TdxGridViewCustomRowProducer }

function TdxGridViewCustomRowProducer.Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell;
begin
  Result := inherited Produce(AHostInfo);
  AddItem;
end;

procedure TdxGridViewCustomRowProducer.AddItem;
begin
  FItem := CreateItem(Row);
  FItem.BoundsRect := Rect(0, 0, RowWidth, RowHeight);
  InitializeItem;
end;

function TdxGridViewCustomRowProducer.CreateItem(AParent: TdxReportCell): TAbstractdxReportCellData;
begin
  Result := GetItemClass.Create(AParent);
end;

procedure TdxGridViewCustomRowProducer.FixupRowDataHeight;
begin
  inherited;
  FixupRowDataItemHeight(Item);
end;

procedure TdxGridViewCustomRowProducer.InitializeItem;
begin
end;

function TdxGridViewCustomRowProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TdxGridViewCustomRowProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := TAbstractdxReportCellData;
end;

{ TdxGridViewCaptionProducer }

procedure TdxGridViewCaptionProducer.InitializeItem;
begin
  inherited;
  Formatter.DoInitializeCaption(Item);
  Formatter.DoReportLinkInitializeCaption(Item);
end;

procedure TdxGridViewCaptionProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeCaptionRow(Row);
end;

function TdxGridViewCaptionProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetCaptionClass;
end;

function TdxGridViewCaptionProducer.GetLineHeight: Integer;
begin
  Result := Formatter.CaptionLineHeight;
end;

function TdxGridViewCaptionProducer.GetItem: TdxReportCellText;
begin
  Result := TdxReportCellText(inherited Item);
end;

{ TdxGridViewFilterBarProducer }

procedure TdxGridViewFilterBarProducer.InitializeItem;
begin
  inherited;
  Formatter.DoInitializeFilterBar(Item);
  Formatter.DoReportLinkInitializeFilterBar(Item);
end;

procedure TdxGridViewFilterBarProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeFilterBarRow(Row);
end;

function TdxGridViewFilterBarProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetFilterBarClass;
end;

function TdxGridViewFilterBarProducer.GetLineHeight: Integer;
begin
  Result := Formatter.FilterBarLineHeight;
end;

function TdxGridViewFilterBarProducer.GetItem: TdxReportCellText;
begin
  Result := TdxReportCellText(inherited Item);
end;

{ TdxGridViewDetailsSeparatorProducer }

procedure TdxGridViewDetailsSeparatorProducer.InitializeItem;
begin
  inherited;
  Formatter.DoInitializeHorzDetailsSeparator(Item);
end;

procedure TdxGridViewDetailsSeparatorProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeDetailsSeparatorRow(Row);
end;

function TdxGridViewDetailsSeparatorProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetDetailsSeparatorClass;
end;

function TdxGridViewDetailsSeparatorProducer.GetLineHeight: Integer;
begin
  Result := Formatter.DetailsSeparatorThickness;
end;

function TdxGridViewDetailsSeparatorProducer.GetItem: TdxReportCellBox;
begin
  Result := TdxReportCellBox(inherited Item);
end;

{ TdxGridViewDetailsTopSeparatorProducer }

procedure TdxGridViewDetailsTopSeparatorProducer.InitializeDetailsSeparator(
  AFormatter: TdxCustomGridViewFormatter; ASeparator: TdxReportCellBox;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  if AFormatter = Formatter then
    AFormatter.DoInitializeVertTopDetailsSeparator(ASeparator, ASeparatorKind)
  else
    inherited;
end;

{ TdxGridViewDetailsBottomSeparatorProducer }

procedure TdxGridViewDetailsBottomSeparatorProducer.InitializeDetailsSeparator(
  AFormatter: TdxCustomGridViewFormatter; ASeparator: TdxReportCellBox;
  ASeparatorKind: TdxVerticalDetailsSeparatorKind);
begin
  if AFormatter = Formatter then
    AFormatter.DoInitializeVertBottomDetailsSeparator(ASeparator, ASeparatorKind)
  else
    inherited;
end;

{ TdxGridViewTerminatorProducer }

procedure TdxGridViewTerminatorProducer.InitializeItem;
begin
  inherited InitializeItem;
  Formatter.DoInitializeViewTerminator(Item);
end;

function TdxGridViewTerminatorProducer.GetLineHeight: Integer;
begin
  Result := 1;
end;

{ TdxGridTableViewRowProducer }

function TdxGridTableViewRowProducer.Adapter: TdxGridTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridTableViewAdapter;
end;

function TdxGridTableViewRowProducer.Builder: TdxGridTableViewBuilder;
begin
  Result := inherited Builder as TdxGridTableViewBuilder;
end;

function TdxGridTableViewRowProducer.Formatter: TdxGridTableViewFormatter;
begin
  Result := Builder.Formatter;
end;

{ TdxGridTableViewRowSubItemsProducer }

constructor TdxGridTableViewRowSubItemsProducer.Create(ABuilder: TdxCustomGridViewBuilder);
begin
  inherited;
  FIndents := TList.Create;
  FSubItems := TList.Create;
end;

destructor TdxGridTableViewRowSubItemsProducer.Destroy;
begin
  FreeAndNil(FSubItems);
  FreeAndNil(FIndents);
  inherited;
end;

function TdxGridTableViewRowSubItemsProducer.Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell;
begin
  InitializeIndentList;
  InitializeSubItemList;
  Result := inherited Produce(AHostInfo);
end;

procedure TdxGridTableViewRowSubItemsProducer.AddIndents(AParent: TdxReportCell);
begin
end;

procedure TdxGridTableViewRowSubItemsProducer.AddSubItems(AParent: TdxReportCell);
var
  I: Integer;
  Item: TAbstractdxReportCellData;
begin
  for I := 0 to GetSubItemCount - 1 do
    if HasSubItem[I] then
    begin
      Item := CreateSubItem(I, AParent);
      Item.BoundsRect := SubItemBounds[I]; // needed before item initialization
      InitializeSubItem(Item, I);
    end;
end;

procedure TdxGridTableViewRowSubItemsProducer.CreateRow;
begin
  inherited;
  AddIndents(Row);
  AddSubItems(Row);
end;

function TdxGridTableViewRowSubItemsProducer.CreateSubItem(AnIndex: Integer;
  AParent: TdxReportCell): TAbstractdxReportCellData;
begin
  Result := SubItemClasses[AnIndex].Create(AParent);
  FSubItems[AnIndex] := Result;
end;

function TdxGridTableViewRowSubItemsProducer.IsItemIndent(AnItem: TdxReportVisualItem): Boolean;
begin
  Result := FIndents.IndexOf(AnItem) <> -1;
end;

procedure TdxGridTableViewRowSubItemsProducer.FixupIndentsHeight;
var
  I: Integer;
begin
  for I := 0 to IndentCount - 1 do
    with Indents[I] do
      Height := Parent.Height;
end;

procedure TdxGridTableViewRowSubItemsProducer.FixupRowDataHeight;
var
  I: Integer;
  SubItem: TdxReportVisualItem;
begin
  for I := 0 to SubItemCount - 1 do {.SubItemCount}
  begin
    SubItem := SubItems[I];
    if SubItem <> nil then
      FixupRowDataItemHeight(SubItem);
  end;
end;

procedure TdxGridTableViewRowSubItemsProducer.FixupRowHeight;
begin
  inherited;
  FixupIndentsHeight;
end;

procedure TdxGridTableViewRowSubItemsProducer.InitializeIndentList;
begin
  FIndents.Clear;
  FIndents.Count := GetIndentCount;
end;

procedure TdxGridTableViewRowSubItemsProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
end;

procedure TdxGridTableViewRowSubItemsProducer.InitializeSubItemList;
begin
  FSubItems.Clear;
  FSubItems.Count := GetSubItemCount;
end;

function TdxGridTableViewRowSubItemsProducer.GetHasSubItem(Index: Integer): Boolean;
begin
  Result := True;
end;

function TdxGridTableViewRowSubItemsProducer.GetIndentCount: Integer;
begin
  Result := 0;
end;

function TdxGridTableViewRowSubItemsProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Rect(0, 0, -1, -1);
end;

function TdxGridTableViewRowSubItemsProducer.GetSubItemCount: Integer;
begin
  Result := Formatter.ColumnCount;
end;

function TdxGridTableViewRowSubItemsProducer.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := Formatter.Columns[Index];
end;

function TdxGridTableViewRowSubItemsProducer.GetIndent(Index: Integer): TdxReportCellExpandButton;
begin
  Result := TdxReportCellExpandButton(FIndents[Index]);
end;

function TdxGridTableViewRowSubItemsProducer.GetSubItem(Index: Integer): TdxReportVisualItem;
begin
  Result := TdxReportVisualItem(FSubItems[Index]);
end;

{ TdxGridTableViewHeadersProducer }

procedure TdxGridTableViewHeadersProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeHeaderRow(Row);
end;

procedure TdxGridTableViewHeadersProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeHeaderItem(ASubItem, AIndex);
  Formatter.DoReportLinkInitializeHeaderItem(ASubItem, AIndex);
end;

function TdxGridTableViewHeadersProducer.GetAutoHeight: Boolean;
begin
  Result := inherited GetAutoHeight and Adapter.HeaderAutoHeight;
end;

function TdxGridTableViewHeadersProducer.GetLineCount: Integer;
begin
  Result := Adapter.DetailsLineCount;
end;

function TdxGridTableViewHeadersProducer.GetLineHeight: Integer;
begin
  Result := Formatter.HeaderLineHeight;
end;

function TdxGridTableViewHeadersProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Formatter.GetHeaderItemBounds(Index);
end;

function TdxGridTableViewHeadersProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetHeaderItemClass(Index);
end;

{ TdxGridTableViewFootersProducer }

procedure TdxGridTableViewFootersProducer.CalculateRowAutoHeight;
begin
  inherited;
  Inc(FRowHeight, 2 * Adapter.ScaleFactor.Apply(FooterItemInflateVert));
end;

procedure TdxGridTableViewFootersProducer.CalculateRowHeight;
begin
  if AutoHeight then
    inherited
  else
    RowHeight := LineCount * LineHeight;
end;

procedure TdxGridTableViewFootersProducer.FixupRowDataItemHeight(AnItem: TdxReportVisualItem);
begin
  with AnItem do
    Height := Parent.Height - 2 * Adapter.ScaleFactor.Apply(FooterItemInflateVert);
end;

procedure TdxGridTableViewFootersProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeFooterRow(Row);
end;

procedure TdxGridTableViewFootersProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
var
  ARowIndex, ARealIndex, AColIndex: Integer;
begin
  inherited;
  AColIndex := Adapter.GetFooterItemInfo(SummaryItems, AIndex, ARowIndex);
  ARealIndex := Adapter.CheckSummaryItemIndex(SummaryItems, AIndex, False);
  Formatter.DoInitializeFooterItem(ASubItem, ARealIndex, SummaryItems[ARealIndex]);
  Formatter.DoReportLinkInitializeFooterItem(ASubItem, AColIndex);
end;

function TdxGridTableViewFootersProducer.GetAutoHeight: Boolean;
var
  I: Integer;
begin
  Result := inherited GetAutoHeight and
    Adapter.FooterMultiline and Adapter.FooterMultiSummaries;
  if Result then
  begin
    for I := 0 to SubItemCount - 1 do
      if HasSubItem[I] then Exit;
    Result := False;
  end;
end;

function TdxGridTableViewFootersProducer.GetHasSubItem(Index: Integer): Boolean;
var
  ARowIndex: Integer;
begin
  Adapter.GetFooterItemInfo(SummaryItems, Index, ARowIndex);
  Result := Adapter.FooterMultiSummaries or (ARowIndex = 0);
end;

function TdxGridTableViewFootersProducer.GetLineCount: Integer;
begin
  Result := Adapter.GetFooterLineCount(SummaryItems, Adapter.FooterMultiSummaries);
end;

function TdxGridTableViewFootersProducer.GetLineHeight: Integer;
begin
  Result := Formatter.FooterLineHeight;
end;

function TdxGridTableViewFootersProducer.GetSubItemBound(Index: Integer): TRect;
var
  AColIndex, ARowIndex: Integer;
begin
  AColIndex := Adapter.GetFooterItemInfo(SummaryItems, Index, ARowIndex);
  Result := Formatter.GetFooterItemBounds(AColIndex);
  if Adapter.FooterMultiSummaries then
  begin
    Result.Top := LineHeight * ARowIndex;
    Result.Bottom := Result.Top + LineHeight;
    InflateRect(Result, 0, -Adapter.ScaleFactor.Apply(FooterItemInflateVert));
  end;
end;

function TdxGridTableViewFootersProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetFooterItemClass(Index);
end;

function TdxGridTableViewFootersProducer.GetSubItemCount: Integer;
begin
  Result := Adapter.GetFooterItemCount(SummaryItems, True, False);
end;

function TdxGridTableViewFootersProducer.GetSummaryItems: TcxDataSummaryItems;
begin
  Result := Adapter.Summary.FooterSummaryItems;
end;

{ TdxGridTableViewCustomDataRowProducer }

function TdxGridTableViewCustomDataRowProducer.Produce(AHostInfo: TdxGridAttributeHostInfo;
  AGridRow: TcxCustomGridRow; AGridRowIndex: Integer): TdxReportCell;
begin
  FGridRow := AGridRow;
  FGridRowIndex := AGridRowIndex;
  Result := inherited Produce(AHostInfo);
  FGridRowIndex := -1;
  FGridRow := nil;
end;

function TdxGridTableViewCustomDataRowProducer.GridRow: TcxCustomGridRow;
begin
  Result := FGridRow;
end;

procedure TdxGridTableViewCustomDataRowProducer.AddIndents(AParent: TdxReportCell);
var
  I: Integer;
  Indent: TdxReportCellExpandButton;
begin
  inherited;
  for I := 0 to IndentCount - 1 do
  begin
    Indent := CreateIndent(I, AParent);
    Indent.BoundsRect := IndentBounds[I];
    InitializeIndent(Indent, I);
  end;
end;

function TdxGridTableViewCustomDataRowProducer.CreateIndent(AnIndex: Integer;
  AParent: TdxReportCell): TdxReportCellExpandButton;
begin
  Result := Formatter.CreateIndent(AParent);
  FIndents[AnIndex] := Result;
end;

procedure TdxGridTableViewCustomDataRowProducer.InitializeIndent(AIndent: TdxReportCellExpandButton;
  AIndex: Integer);
begin
end;

function TdxGridTableViewCustomDataRowProducer.GetIndentCount: Integer;
begin
  Result := GridRow.Level;
end;

function TdxGridTableViewCustomDataRowProducer.GetLineCount: Integer;
begin
  Result := 1;
end;

function TdxGridTableViewCustomDataRowProducer.GetLineHeight: Integer;
begin
  Result := Adapter.ScaleFactor.Apply(DefaultDataRowLineHeight);
end;

function TdxGridTableViewCustomDataRowProducer.GetIndentArea: Integer;
begin
  Result := IndentCount * IndentWidth;
end;

function TdxGridTableViewCustomDataRowProducer.GetIndentBounds(Index: Integer): TRect;
begin
  Result := Bounds(IndentWidth * Index, 0, IndentWidth, RowHeight);
end;

{ TdxCustomGridTableViewRowSeparatorProducer }

function TdxCustomGridTableViewRowSeparatorProducer.Produce(AHostInfo: TdxGridAttributeHostInfo;
  AGridRow: TcxCustomGridRow): TdxReportCell;
begin
  Result := inherited Produce(AHostInfo, AGridRow, -1);
end;

procedure TdxCustomGridTableViewRowSeparatorProducer.InitializeIndent(AIndent: TdxReportCellExpandButton;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeRowSeparatorIndent(AIndent, AIndex, IndentCount, GridRow);
end;

function TdxCustomGridTableViewRowSeparatorProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TdxCustomGridTableViewRowSeparatorProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Rect(IndentArea, 0, RowWidth, RowHeight);
end;

function TdxCustomGridTableViewRowSeparatorProducer.GetSubItemCount: Integer;
begin
  Result := 1;
end;

{ TdxGridTableViewRowSeparatorProducer }

function TdxGridTableViewRowSeparatorProducer.Produce(AHostInfo: TdxGridAttributeHostInfo;
  AGridRow: TcxCustomGridRow; AnIsLast: Boolean): TdxReportCell;
begin
  FIsLast := AnIsLast;
  Result := inherited Produce(AHostInfo, AGridRow);
end;

procedure TdxGridTableViewRowSeparatorProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeSeparatorRow(Row);
end;

procedure TdxGridTableViewRowSeparatorProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  Formatter.DoInitializeRowSeparator(ASubItem);
end;

function TdxGridTableViewRowSeparatorProducer.GetIndentCount: Integer;
begin
  if not IsLast then
  begin
    Result := inherited GetIndentCount;
    if (Result <> 0) and Adapter.IsOffice11StyleGrouping and not (GridRow is TcxGridGroupRow) then {.2}
      Dec(Result);
  end
  else
    Result := 0;
end;

function TdxGridTableViewRowSeparatorProducer.GetLineHeight: Integer;
begin
  Result := Formatter.RowSeparatorThickness;
end;

function TdxGridTableViewRowSeparatorProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetRowSeparatorClass;
end;

{ TdxGridTableViewGroupRowSeparatorProducer }

procedure TdxGridTableViewGroupRowSeparatorProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeGroupSeparatorRow(Row);
end;

procedure TdxGridTableViewGroupRowSeparatorProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeGroupRowSeparator(ASubItem);
end;

function TdxGridTableViewGroupRowSeparatorProducer.GetLineHeight: Integer;
begin
  Result := Formatter.GroupRowSeparatorThickness;
end;

function TdxGridTableViewGroupRowSeparatorProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetGroupRowSeparatorClass;
end;

{ TdxGridTableViewDataRowProducer }

function TdxGridTableViewDataRowProducer.GridRow: TcxGridDataRow;
begin
  Result := inherited GridRow as TcxGridDataRow;
end;

procedure TdxGridTableViewDataRowProducer.AddPreview(AParent: TdxReportCell);
begin
  FPreviewItem := CreatePreview(Row);

  PreviewItem.Left := IndentArea;
  // We have to set Width before because we use that value for PreviewHeight calculation
  PreviewItem.Width := Width - IndentArea;
  Formatter.DoInitializePreview(TAbstractdxReportCellData(PreviewItem), PreviewColumn, GridRow);
  PreviewItem.Height := PreviewHeight;
  PreviewItem.AdjustContent(Formatter.Canvas); {3.1}
end;

procedure TdxGridTableViewDataRowProducer.AddSubItems(AParent: TdxReportCell);
begin
  inherited;
  if HasPreview then AddPreview(AParent);
end;

procedure TdxGridTableViewDataRowProducer.CalculateRowAutoHeight;
begin
  if CellAutoHeight then inherited;
  if PreviewItem <> nil then
    RowHeight := RowHeight + PreviewItem.Height;
end;

function TdxGridTableViewDataRowProducer.CreatePreview(AParent: TdxReportCell): TdxReportVisualItem;
begin
  Result := GetPreviewClass.Create(AParent);
  FPreviewItem := Result;
end;

function TdxGridTableViewDataRowProducer.DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean;
var
  TableItem: TcxCustomGridTableItem;
  Properties: TcxCustomEditProperties;
begin
  Result := AnItem <> FPreviewItem;
  // 3.2
  if Result and not IsItemIndent(AnItem) then
  begin
    TableItem := TcxCustomGridTableItem(AnItem.Data);
    Properties := Adapter.GetProperties(TableItem, GridRow);
    Result := dxPScxCommon.dxPSDataMaps.DoesItemParticipateInAutoHeightCalculation(Properties);
  end;
end;

function TdxGridTableViewDataRowProducer.GetPreviewClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetPreviewClass(PreviewColumn, GridRow);
end;

procedure TdxGridTableViewDataRowProducer.FixupRowDataHeight;
var
  NewHeight, I: Integer;
  Item: TdxReportVisualItem;
begin
  NewHeight := RowHeight;
  if PreviewItem <> nil then
    Dec(NewHeight, PreviewItem.Height);

  for I := 0 to Row.DataItemCount - 1 do
  begin
    Item := Row.DataItems[I];
    if Item = PreviewItem then
      if PreviewPlace = cxGridTableView.ppTop then
        Item.Top := 0
      else
        Item.Top := NewHeight
    else
      if IsItemIndent(Item) then
      begin
        Item.Top := 0;
        Item.Height := RowHeight;
      end
      else
      begin
        if CellAutoHeight then
          Item.Height := NewHeight;
        if HasPreview and (PreviewPlace = cxGridTableView.ppTop) then
          Item.Top := Item.Top + PreviewItem.Height;
      end;
  end;
  {3.1}
  for I := 0 to Row.CellCount - 1 do
  begin
    Item := Row.Cells[I];
    if Item = PreviewItem then
      if PreviewPlace = cxGridTableView.ppTop then
        Item.Top := 0
      else
        Item.Top := NewHeight
    else
    begin
      if CellAutoHeight then
        Item.Height := NewHeight;
      if HasPreview and (PreviewPlace = cxGridTableView.ppTop) then
        Item.Top := Item.Top + PreviewItem.Height;
    end;
  end;
end;

procedure TdxGridTableViewDataRowProducer.InitializeIndent(AIndent: TdxReportCellExpandButton;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeDataRowIndent(AIndent, AIndex, IndentCount, GridRow);
end;

procedure TdxGridTableViewDataRowProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeDataRow(Row, GridRow);
end;

procedure TdxGridTableViewDataRowProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeItem(ASubItem, Columns[AIndex], GridRow, False);
  Formatter.DoReportLinkInitializeItem(ASubItem, Columns[AIndex], GridRow);
end;

function TdxGridTableViewDataRowProducer.GetAutoHeight: Boolean;
begin
  Result := CellAutoHeight or HasPreview;
end;

function TdxGridTableViewDataRowProducer.GetCellAutoHeight: Boolean;
begin
  Result := inherited GetAutoHeight and Adapter.CellAutoHeight;
end;

function TdxGridTableViewDataRowProducer.GetHasPreview: Boolean;
begin
  Result := Formatter.HasPreview;
end;

function TdxGridTableViewDataRowProducer.GetIndentCount: Integer;
begin
  Result := inherited GetIndentCount;
  if (Result <> 0) and Adapter.IsOffice11StyleGrouping then {.2}
    Dec(Result);
end;

function TdxGridTableViewDataRowProducer.GetLineCount: Integer;
begin
  Result := Adapter.DetailsLineCount;
end;

function TdxGridTableViewDataRowProducer.GetLineHeight: Integer;
begin
  if Formatter.HeaderLineCount = 1 then
    Result := Formatter.RowHeights[GridRowIndex]
  else
    Result := Formatter.DetailsLineHeight;
end;

function TdxGridTableViewDataRowProducer.GetPreviewHeight: Integer;
begin
  Result := PreviewLineCount * PreviewLineHeight;
end;

function TdxGridTableViewDataRowProducer.GetPreviewLineCount: Integer;
begin
  if FPreviewItem <> nil then
    if not Formatter.PreviewAutoHeight then
    begin
      Result := PreviewItem.CalculateLineCount(Formatter.Canvas);
      if Result <> Formatter.PreviewMaxLineCount then
        Result := Formatter.PreviewMaxLineCount;
    end
    else
      Result := 1
  else
    Result := 0;
end;

function TdxGridTableViewDataRowProducer.GetPreviewLineHeight: Integer;
var
  LineHeight: Integer;
begin
  if FPreviewItem <> nil then
    if Formatter.PreviewAutoHeight then
    begin
      Result := PreviewItem.MeasureContentHeight(Formatter.Canvas);
      if Formatter.PreviewMaxLineCount <> 0 then
      begin
        LineHeight := PreviewItem.MeasureFontHeight(Formatter.Canvas);
        Result := Min(Result, LineHeight * Formatter.PreviewMaxLineCount);
      end;
    end
    else
      Result := PreviewItem.MeasureFontHeight(Formatter.Canvas)
  else
    Result := 0;
end;

function TdxGridTableViewDataRowProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Formatter.GetItemBounds(GridRow, GridRowIndex, Index);
end;

function TdxGridTableViewDataRowProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetDataItemClass(Columns[Index], GridRow);
end;

function TdxGridTableViewDataRowProducer.GetPreviewColumn: TcxGridColumn;
begin
  Result := Adapter.PreviewColumn;
end;

function TdxGridTableViewDataRowProducer.GetPreviewPlace: TcxGridPreviewPlace;
begin
  Result := Adapter.PreviewPlace;
end;

{ TdxGridTableViewGroupRowProducer}

function TdxGridTableViewGroupRowProducer.GridRow: TcxGridGroupRow;
begin
  Result := inherited GridRow as TcxGridGroupRow;
end;

function TdxGridTableViewGroupRowProducer.Produce(AHostInfo: TdxGridAttributeHostInfo;
  AGridRow: TcxCustomGridRow; AGridRowIndex: Integer): TdxReportCell;
begin
  SummaryItemInfoList := TcxObjectList.Create;
  try
    Result := inherited Produce(AHostInfo, AGridRow, AGridRowIndex);
  finally
    FreeAndNil(SummaryItemInfoList);
  end;
end;

procedure TdxGridTableViewGroupRowProducer.AddSubItems(AParent: TdxReportCell);
var
  I: Integer;
  Item: TAbstractdxReportCellData;
begin
  for I := GetSubItemCount - 1 downto 0 do
    if HasSubItem[I] then
    begin
      Item := CreateSubItem(I, AParent);
      Item.BoundsRect := SubItemBounds[I]; // needed before item initialization
      Item.Data := TdxNativeInt(SummaryItemInfos[I]);
      InitializeSubItem(Item, I);
    end;
end;

function TdxGridTableViewGroupRowProducer.AddSummaryGroupInfo(
  AItem: TcxDataSummaryItem; const ADisplayText: string): Boolean;
var
  AInfo: TcxGridSummaryItemInfo;
begin
  Result := (AItem = nil) or (ADisplayText <> '');
  if Result then
  begin
    AInfo := TcxGridSummaryItemInfo.Create;
    if AItem <> nil then
      AInfo.Column := TcxGridColumn(AItem.ItemLink);
    if not Formatter.AlignSummaryWithColumns or ((AInfo.Column <> nil) and (AInfo.Column.VisibleIndex < 0)) then
      AInfo.Column := nil;
    if AInfo.Column <> nil then
    begin
      Formatter.Adapter.Styles.GetGroupSummaryCellContentParams(
        GridRow, AItem, AInfo.ViewParams);
      AInfo.ViewParams.Color := RowViewParams.Color;
      AInfo.ViewParams.Bitmap := RowViewParams.Bitmap;
    end
    else
      AInfo.ViewParams := RowViewParams;
    AInfo.DisplayText := ADisplayText;
    if AItem <> nil then
      AInfo.SummaryIndex := AItem.Index;
    AInfo.Bounds := Rect(IndentArea, 0, RowWidth, LineHeight);
    SummaryItemInfoList.Add(AInfo);
  end;
end;

procedure TdxGridTableViewGroupRowProducer.DeleteInfo(AIndex: Integer);
begin
  SummaryItemInfoList.Items[AIndex].Free;
  SummaryItemInfoList.Delete(AIndex);
end;

procedure TdxGridTableViewGroupRowProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeGroupRow(Row, GridRow);
end;

procedure TdxGridTableViewGroupRowProducer.InitializeIndent(
  AIndent: TdxReportCellExpandButton; AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeGroupRowIndent(AIndent, AIndex, IndentCount, GridRow);
end;

procedure TdxGridTableViewGroupRowProducer.InitializeSubItem(
  ASubItem: TAbstractdxReportCellData; AIndex: Integer);
const
  Alignment2TextAlign: array[TAlignment] of TcxTextAlignX =
    (taLeft, taRight, taCenterX);
begin
  inherited;
  Formatter.DoInitializeGroupRowItem(ASubItem, GridRow,
    AIndex, SummaryItemInfos[AIndex].DisplayText);
  if AIndex <> 0 then
  begin
    ASubItem.CellSides := [];
    TdxReportCellString(ASubItem).TextAlignX :=
      Alignment2TextAlign[SummaryItemInfos[AIndex].Column.GroupSummaryAlignment];
    TdxReportCellString(ASubItem).EndEllipsis := True;
  end;
end;

procedure TdxGridTableViewGroupRowProducer.InitializeSubItemList;
begin
  SummaryItemInfoList.Clear;
  PrepareSummaryItemsInfo;
  PrepareSummaryItemsBounds;
  inherited InitializeSubItemList;
end;

function TdxGridTableViewGroupRowProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TdxGridTableViewGroupRowProducer.GetIndentCount: Integer;
begin
  Result := inherited GetIndentCount + 1;
end;

function TdxGridTableViewGroupRowProducer.GetLineCount: Integer;
begin
  Result := 1;
end;

function TdxGridTableViewGroupRowProducer.GetLineHeight: Integer;
begin
  Result := Formatter.RowHeights[GridRowIndex];//}Formatter.GroupRowLineHeight; {3.1}
end;

function TdxGridTableViewGroupRowProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := SummaryItemInfos[Index].Bounds;
end;

function TdxGridTableViewGroupRowProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetGroupRowClass;
end;

function CompareSummaryItemsInfo(AInfo1, AInfo2: TcxGridSummaryItemInfo): Integer;
begin
  if AInfo1.Column = AInfo2.Column then
    Result := AInfo1.SummaryIndex - AInfo2.SummaryIndex
  else
    if AInfo1.Column = nil then
      Result := -1
    else
      if AInfo2.Column = nil then
        Result := 1
      else
        Result := AInfo1.Column.VisibleIndex - AInfo2.Column.VisibleIndex;
end;

function TdxGridTableViewGroupRowProducer.GetSubItemCount: Integer;
begin
  Result := SummaryItemInfoList.Count;
end;

procedure TdxGridTableViewGroupRowProducer.PrepareSummaryItemsInfo;
var
  I: Integer;
  S: string;
  AValues: PVariant;
  AItems: TcxDataSummaryItems;
begin
  Formatter.Adapter.Styles.GetGroupParams(GridRow, GridRow.Level, RowViewParams);
  if GridRow.GroupedColumnCount = 1 then
  begin
    if not GridRow.GetGroupSummaryInfo(AItems, AValues) then Exit;
    for I := 0 to AItems.Count - 1 do
      if AItems[I].Position = spGroup then
        AddSummaryGroupInfo(AItems[I], AItems[I].FormatValue(AValues^[I], False));
    SummaryItemInfoList.Sort(@CompareSummaryItemsInfo);
    with GridRow.GroupSummaryItems do
    begin
      if Formatter.AlignSummaryWithColumns then
        for I := SubItemCount - 1 downto 0 do
          with SummaryItemInfos[I] do
            if Column = nil then DeleteInfo(I);
      I := 0;
      while I < SubItemCount do
      begin
        with SummaryItemInfos[I] do
        begin
          if (I < (SubItemCount - 1)) and (Column = SummaryItemInfos[I + 1].Column) then
          begin
            DisplayText := DisplayText + Separator + ' ' + SummaryItemInfos[I + 1].DisplayText;
            DeleteInfo(I + 1);
            Continue;
          end
          else
            Inc(I);
        end;
      end;
      if (SubItemCount > 0) and (SummaryItemInfos[0].Column = nil) then
      begin
        with GridRow.GroupSummaryItems do
         SummaryItemInfos[0].DisplayText := GridRow.DisplayCaption + BeginText +
           SummaryItemInfos[0].DisplayText + EndText;
        cxGridCustomTableView.TcxCustomGridTableItemAccess.DoGetDisplayText(
          GridRow.GroupedColumn, GridRow, SummaryItemInfos[0].DisplayText);
      end
      else
      begin
        S := GridRow.DisplayCaption;
        if SubItemCount = 0 then
        begin
          S := GridRow.DisplayText;
          cxGridCustomTableView.TcxCustomGridTableItemAccess.DoGetDisplayText(
            GridRow.GroupedColumn, GridRow, S);
        end;
        if AddSummaryGroupInfo(nil, S) then
          SummaryItemInfoList.Exchange(0, SubItemCount - 1);
      end;
    end;
  end
  else
  begin
    S := GridRow.DisplayText;
    for I := 0 to GridRow.GroupedColumnCount - 1 do
      cxGridCustomTableView.TcxCustomGridTableItemAccess.DoGetDisplayText(
        GridRow.GroupedColumn, GridRow, S);
    AddSummaryGroupInfo(nil, S);
  end;
end;

procedure TdxGridTableViewGroupRowProducer.PrepareSummaryItemsBounds;
var
  I, AMinLeft: Integer;
  AItem: TcxGridSummaryItemInfo;
begin
  if SubItemCount = 0 then Exit;
  AItem := SummaryItemInfos[0];
  AMinLeft := AItem.Bounds.Left +
    cxTextWidth(AItem.ViewParams.Font, AItem.DisplayText) + cxTextOffset * 2;
  I := 1;
  while I < SubItemCount do
  begin
    AItem := SummaryItemInfos[I];
    with Formatter.GetHeaderItemBounds(SummaryItemInfos[I].Column.VisibleIndex) do
    begin
      AItem.Bounds.Left := Left;
      AItem.Bounds.Right := Right;
    end;
    if AItem.Bounds.Left < AMinLeft then
      AItem.Bounds.Left := AMinLeft;
    if AItem.Bounds.Left >= AItem.Bounds.Right then
      DeleteInfo(I)
    else
      Inc(I);
  end;
end;

function TdxGridTableViewGroupRowProducer.GetSummaryItemInfo(
  Index: Integer): TcxGridSummaryItemInfo;
begin
  Result := SummaryItemInfoList[Index] as TcxGridSummaryItemInfo;
end;

{ TdxGridTableViewMasterRowProducer }

function TdxGridTableViewMasterRowProducer.GridRow: TcxGridMasterDataRow;
begin
  Result := inherited GridRow as TcxGridMasterDataRow;
end;

procedure TdxGridTableViewMasterRowProducer.InitializeIndent(AIndent: TdxReportCellExpandButton;
  AIndex: Integer);
begin
  //inherited;
  Formatter.DoInitializeMasterDataRowIndent(AIndent, AIndex, IndentCount, GridRow);
end;

procedure TdxGridTableViewMasterRowProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  inherited;
  if AIndex = 0 then
    ASubItem.CellSides := ASubItem.CellSides - [csLeft];
end;

function TdxGridTableViewMasterRowProducer.GetIndentCount: Integer;
begin
  Result := inherited GetIndentCount + 1;
end;

{ TdxGridTableGroupFooterInfo }

constructor TdxGridTableViewGroupFooterSubItemInfo.Create(ASummaryItem: TcxDataSummaryItem; ASummaryValue: Variant);
begin
  inherited Create;
  FSummaryItem := ASummaryItem;
  FSummaryValue := ASummaryValue;
end;

{ TdxGridTableViewGroupFooterProducer }

function TdxGridTableViewGroupFooterProducer.Produce(AHostInfo: TdxGridAttributeHostInfo;
  AGridRow: TcxCustomGridRow; AGroupLevel, AnIndex: Integer): TdxReportCell;
var
  AGroupIndex: Integer;
begin
  FIndex := AnIndex;
  FGroupLevel := AGroupLevel;
  AGroupIndex := GetCorrectRowIndex(AGridRow, AGroupLevel);
  FSubItemInfos := TdxFastObjectList.Create;
  try
    PopulateSubItemInfos(AGroupIndex);
    UpdateLineCount;
    Result := inherited Produce(AHostInfo, AGridRow, -1);
  finally
    FSubItemInfos.Free;
  end;
end;

procedure TdxGridTableViewGroupFooterProducer.AddSubItems(AParent: TdxReportCell);

  function SubstituteParent(AParent: TdxReportCell): TdxReportCell;
  begin
    Result := TdxReportCell.Create(AParent);
    Result.BoundsRect := HostBounds;
    Formatter.DoInitializeGroupFooterHost(Result, GridRow, GroupLevel);
  end;

begin
  FItemsSite := SubstituteParent(AParent);
  inherited AddSubItems(ItemsSite);
end;

procedure TdxGridTableViewGroupFooterProducer.CalculateSubItemPosition(AIndex: Integer; out AColIndex, ARowIndex: Integer);
var
  AInfoIndex: Integer;
  AInfo: TdxGridTableViewGroupFooterSubItemInfo;
  ASummaryItem: TcxDataSummaryItem;
  AColumn: TcxGridColumn;
begin
  AInfo := SubItemInfos[AIndex];
  AColumn := AInfo.SummaryItem.ItemLink as TcxGridColumn;
  AColIndex := AColumn.VisibleIndex;
  ARowIndex := -1;
  for AInfoIndex := 0 to SubItemInfoList.Count - 1 do
  begin
    ASummaryItem := SubItemInfos[AInfoIndex].SummaryItem;
    if ASummaryItem.ItemLink = AColumn then
      Inc(ARowIndex);
    if AInfo.SummaryItem = ASummaryItem then
      Exit;
  end;
end;

function TdxGridTableViewGroupFooterProducer.CanCreateSubItem(ASummaryItem: TcxDataSummaryItem): Boolean;
var
  AColumn: TcxGridColumn;
begin
  AColumn := TcxGridColumn(ASummaryItem.ItemLink);
  Result := (ASummaryItem.Position = spFooter) and (AColumn <> nil) and (AColumn.VisibleIndex > -1);
end;

function TdxGridTableViewGroupFooterProducer.CreateSubItemInfo(ASummaryItem: TcxDataSummaryItem;
  ASummaryValue: Variant): TdxGridTableViewGroupFooterSubItemInfo;
begin
  Result := TdxGridTableViewGroupFooterSubItemInfo.Create(ASummaryItem, ASummaryValue);
end;

procedure TdxGridTableViewGroupFooterProducer.CalculateRowAutoHeight;
begin
  inherited;
  Inc(FRowHeight, Adapter.ScaleFactor.Apply(FooterItemInflateVert));
end;

procedure TdxGridTableViewGroupFooterProducer.FixupRowDataItemHeight(AnItem: TdxReportVisualItem);
begin
  with AnItem do
    Height := Parent.Height - 2 * Adapter.ScaleFactor.Apply(FooterItemInflateVert);
end;

procedure TdxGridTableViewGroupFooterProducer.InitializeIndent(AIndent: TdxReportCellExpandButton;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeGroupFooterIndent(AIndent, AIndex, IndentCount, GridRow);
end;

procedure TdxGridTableViewGroupFooterProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeGroupFooterRow(Row, GridRow, GroupLevel);
end;

procedure TdxGridTableViewGroupFooterProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData; AIndex: Integer);
var
  ASummaryItem: TcxDataSummaryItem;
begin
  inherited InitializeSubItem(ASubItem, AIndex);
  ASummaryItem := SubItemInfos[AIndex].SummaryItem;
  Formatter.DoInitializeGroupFooterItem(ASubItem, GridRow, GroupLevel, SubItemInfos[AIndex].SummaryValue, ASummaryItem);
  Formatter.DoReportLinkInitializeGroupFooterItem(ASubItem, TcxGridColumn(ASummaryItem.ItemLink), GridRow, GroupLevel);
end;

function TdxGridTableViewGroupFooterProducer.GetAutoHeight: Boolean;
begin
  Result := inherited GetAutoHeight and Adapter.GroupFootersMultiline and
    ((ItemsSite.DataItemCount <> 0) or (ItemsSite.CellCount <> 0));
end;

function TdxGridTableViewGroupFooterProducer.GetCorrectRowIndex(
  AGridRow: TcxCustomGridRow; AGroupLevel: Integer): Integer;
begin
  if Adapter.GroupFootersMode = gfVisibleWhenExpanded then
    Dec(AGroupLevel);
  while (AGridRow.Level > AGroupLevel) and (AGridRow.Level > 0) do
    AGridRow := TcxCustomGridRow(AGridRow.ParentRecord);
  Result := AGridRow.Index;
end;

function TdxGridTableViewGroupFooterProducer.GetHasSubItem(Index: Integer): Boolean;
begin
  Result := True;
end;

function TdxGridTableViewGroupFooterProducer.GetIndentCount: Integer;
begin
  Result := GroupLevel;
  if Adapter.GroupFootersMode = gfVisibleWhenExpanded then
  begin
    Inc(Result);
    if Adapter.IsOffice11StyleGrouping and (Index = 0) and not (GridRow is TcxGridGroupRow) then
      Dec(Result);
  end
end;

function TdxGridTableViewGroupFooterProducer.GetItemsSite: TdxReportCell;
begin
  Result := FItemsSite;
end;

function TdxGridTableViewGroupFooterProducer.GetLineCount: Integer;
begin
  Result := InternalLineCount;
end;

function TdxGridTableViewGroupFooterProducer.GetLineHeight: Integer;
begin
  Result := Formatter.GroupFooterLineHeight;
end;

function TdxGridTableViewGroupFooterProducer.GetSubItemBound(Index: Integer): TRect;
var
  AColIndex, ARowIndex: Integer;
begin
  CalculateSubItemPosition(Index, AColIndex, ARowIndex);
  Result := Formatter.GetGroupFooterItemBounds(AColIndex, IndentCount);
  if Adapter.GroupFooterMultiSummaries then
  begin
    Result.Top := LineHeight * ARowIndex;
    Result.Bottom := Result.Top + LineHeight;
    InflateRect(Result, 0, -Adapter.ScaleFactor.Apply(FooterItemInflateVert));
  end;
end;

function TdxGridTableViewGroupFooterProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetGroupFooterItemClass(Index);
end;

function TdxGridTableViewGroupFooterProducer.GetSubItemCount: Integer;
begin
  Result := SubItemInfoList.Count;
end;

procedure TdxGridTableViewGroupFooterProducer.PopulateSubItemInfos(AGroupIndex: Integer);
var
  AGroupedColumnIndex, ASummaryItemIndex: Integer;
  AGroupRow: TcxGridGroupRow;
  ASummaryItems: TcxDataSummaryItems;
  ASummaryItem: TcxDataSummaryItem;
  ASummaryValues: PVariant;
begin
  AGroupRow := TcxGridGroupRow(Adapter.Rows[AGroupIndex]);
  for AGroupedColumnIndex := 0 to AGroupRow.GroupedColumnCount - 1 do
  begin
    Adapter.Summary.GetGroupSummaryInfo(AGroupIndex, ASummaryItems, ASummaryValues, AGroupedColumnIndex);
    for ASummaryItemIndex := 0 to ASummaryItems.Count - 1 do
    begin
      ASummaryItem := ASummaryItems[ASummaryItemIndex];
      if CanCreateSubItem(ASummaryItem) then
        SubItemInfoList.Add(CreateSubItemInfo(ASummaryItem, ASummaryValues^[ASummaryItemIndex]));
    end;
  end;
end;

procedure TdxGridTableViewGroupFooterProducer.UpdateLineCount;
var
  AColumnIndex, ALineCount: Integer;
  AColumn: TcxGridColumn;
  ASummaryItem: TcxDataSummaryItem;
  AInfoIndex: Integer;
begin
  FInternalLineCount := Adapter.RowLineCount;
  if not Adapter.GroupFooterMultiSummaries then
    Exit;
  for AColumnIndex := 0 to Adapter.ColumnCount - 1 do
  begin
    ALineCount := 0;
    AColumn := Adapter.Columns[AColumnIndex];
    for AInfoIndex := 0 to SubItemInfoList.Count - 1 do
    begin
      ASummaryItem := SubItemInfos[AInfoIndex].SummaryItem;
      if ASummaryItem.ItemLink = AColumn then
        Inc(ALineCount);
    end;
    FInternalLineCount := Max(FInternalLineCount, ALineCount);
  end;
end;

function TdxGridTableViewGroupFooterProducer.GetGroupLevel: Integer;
begin
  Result := FGroupLevel;
  if Adapter.GroupFootersMode = gfVisibleWhenExpanded then
    Dec(Result);
end;

function TdxGridTableViewGroupFooterProducer.GetHostBounds: TRect;
begin
  Result := Rect(IndentArea, 0, Row.Width, Row.Height);
end;

function TdxGridTableViewGroupFooterProducer.GetSubItemInfo(AIndex: Integer): TdxGridTableViewGroupFooterSubItemInfo;
begin
  Result := TdxGridTableViewGroupFooterSubItemInfo(SubItemInfoList[AIndex]);
end;

{ TdxGridTableViewBandsProducer }

function TdxGridTableViewBandsProducer.Adapter: TdxGridBandedTableViewAdapter;
begin
  Result := inherited Adapter as TdxGridBandedTableViewAdapter;
end;

function TdxGridTableViewBandsProducer.Builder: TdxGridBandedTableViewBuilder;
begin
  Result := inherited Builder as TdxGridBandedTableViewBuilder;
end;

function TdxGridTableViewBandsProducer.Formatter: TdxGridBandedTableViewFormatter;
begin
  Result := inherited Formatter as TdxGridBandedTableViewFormatter;
end;

function TdxGridTableViewBandsProducer.GetSubItemCount: Integer;
begin
  Result := Formatter.BandCount;
end;

procedure TdxGridTableViewBandsProducer.CalculateRowHeight;
begin
  FRowHeight := Formatter.ItemPlaceController.Height;
end;

procedure TdxGridTableViewBandsProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeBandRow(Row);
end;

procedure TdxGridTableViewBandsProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeBandItem(ASubItem, AIndex);
  Formatter.DoReportLinkInitializeBandItem(ASubItem, AIndex);
end;

function TdxGridTableViewBandsProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TdxGridTableViewBandsProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Formatter.GetBandItemBounds(Index);
end;

function TdxGridTableViewBandsProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetBandItemClass(Index);
end;

{ TdxGridWinExplorerViewRecordProducer }

function TdxGridWinExplorerViewRecordProducer.Adapter: TdxGridWinExplorerViewAdapter;
begin
  Result := inherited Adapter as TdxGridWinExplorerViewAdapter;
end;

function TdxGridWinExplorerViewRecordProducer.Builder: TdxGridWinExplorerViewBuilder;
begin
  Result := inherited Builder as TdxGridWinExplorerViewBuilder;
end;

function TdxGridWinExplorerViewRecordProducer.Formatter: TdxGridWinExplorerViewFormatter;
begin
  Result := Builder.Formatter;
end;

function TdxGridWinExplorerViewRecordProducer.Produce(AHostInfo: TdxGridAttributeHostInfo; ARowHeight: Integer): TdxReportCell;
begin
  FRowHeight := ARowHeight;
  Result := inherited Produce(AHostInfo);
end;

procedure TdxGridWinExplorerViewRecordProducer.CalculateRecordSize(
  ARecord: TdxReportWinExplorerViewRecord; ARecordViewInfo: TcxCustomGridRecordViewInfo);
begin
  ARecord.CalculateSize(ARecordViewInfo);
end;

procedure TdxGridWinExplorerViewRecordProducer.CalculateRowHeight;
begin
//do nothing
end;

function TdxGridWinExplorerViewRecordProducer.CreateRecord(
  AParent: TdxReportCell; AGridRecord: TcxGridWinExplorerViewCustomRecord): TdxReportWinExplorerViewRecord;
begin
  Result := GetRecordClass.Create(AParent);
  InitializeRecord(Result, AGridRecord);
end;

procedure TdxGridWinExplorerViewRecordProducer.CreateRecordSubItems(
  ARecord: TdxReportWinExplorerViewRecord; ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo);
begin
//do nothing
end;

function TdxGridWinExplorerViewRecordProducer.GetRecordClass: TdxReportWinExplorerViewRecordClass;
begin
  Result := TdxReportWinExplorerViewRecord;
end;

procedure TdxGridWinExplorerViewRecordProducer.InitializeRecord(
  ARecord: TdxReportWinExplorerViewRecord; AGridRecord: TcxGridWinExplorerViewCustomRecord);
var
  ARecordViewInfo: TcxCustomGridRecordViewInfo;
  ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  AGridRecordAccess: TcxCustomGridRecordAccess;
begin
  ARecord.Data := TdxNativeInt(AGridRecord);
  AGridRecordAccess := TcxCustomGridRecordAccess(AGridRecord);
  ARecordsViewInfo := Builder.GridView.ViewInfo.RecordsViewInfo;
  ARecordViewInfo := AGridRecordAccess.GetViewInfoClass.Create(ARecordsViewInfo, AGridRecord);
  try
    ARecordViewInfo.MainCalculate(cxNullPoint.X, cxNullPoint.Y);
    CalculateRecordSize(ARecord, ARecordViewInfo);
    Formatter.DoInitializeRecord(ARecord);
    CreateRecordSubItems(ARecord, TcxGridWinExplorerViewCustomRecordViewInfo(ARecordViewInfo));
  finally
    ARecordViewInfo.Free;
  end;
end;

procedure TdxGridWinExplorerViewRecordProducer.InitializeRow;
begin
  inherited InitializeRow;
  Row.CellSides := [];
end;

{ TdxGridWinExplorerViewDataRecordProducer }

procedure TdxGridWinExplorerViewDataRecordProducer.CreateDataCell(
  AParent: TdxReportWinExplorerViewRecord; ACellViewInfo: TcxGridWinExplorerViewCustomCellViewInfo);
var
  AReportItem: TAbstractdxReportCellData;
  ARecordBounds: TRect;
begin
  AReportItem := Formatter.CreateDataCell(AParent, ACellViewInfo.Item);
  ARecordBounds := ACellViewInfo.RecordViewInfo.Bounds;
  AReportItem.BoundsRect := cxRectOffset(ACellViewInfo.Bounds, -ARecordBounds.Left, -ARecordBounds.Top);
end;

procedure TdxGridWinExplorerViewDataRecordProducer.CreateRecordSubItems(ARecord: TdxReportWinExplorerViewRecord;
  ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo);
var
  ADataRecordViewInfo: TcxGridWinExplorerViewRecordViewInfo;
begin
  ADataRecordViewInfo := TcxGridWinExplorerViewRecordViewInfo(ARecordViewInfo);
  if ADataRecordViewInfo.HasCheckBox then
    CreateDataCell(ARecord, ADataRecordViewInfo.CheckBoxViewInfo);
  if ADataRecordViewInfo.HasImage then
    CreateDataCell(ARecord, ADataRecordViewInfo.ImageViewInfo);
  if ADataRecordViewInfo.HasText then
    CreateDataCell(ARecord, ADataRecordViewInfo.TextViewInfo);
  if ADataRecordViewInfo.HasDescription then
    CreateDataCell(ARecord, ADataRecordViewInfo.DescriptionViewInfo);
end;

function TdxGridWinExplorerViewDataRecordProducer.GetRecordClass: TdxReportWinExplorerViewRecordClass;
begin
  Result := TdxReportWinExplorerViewDataRecord;
end;

{ TdxGridWinExplorerViewGroupRecordProducer }

procedure TdxGridWinExplorerViewGroupRecordProducer.CreateRecordSubItems(
  ARecord: TdxReportWinExplorerViewRecord; ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo);
begin
  CreateTextItem(ARecord, ARecordViewInfo);
end;

procedure TdxGridWinExplorerViewGroupRecordProducer.CreateTextItem(
  AParent: TdxReportWinExplorerViewRecord; ARecordViewInfo: TcxGridWinExplorerViewCustomRecordViewInfo);
var
  AReportItem: TdxReportCellString;
  ABounds: TRect;
begin
  AReportItem := TdxReportCellString.Create(AParent);
  ABounds := ARecordViewInfo.Bounds;
  AReportItem.BoundsRect := cxRectOffset(ARecordViewInfo.Bounds, -ABounds.Left, -ABounds.Top);
  AReportItem.CellSides := [];
  AReportItem.Text := ARecordViewInfo.Text;
  AReportItem.Font := AParent.Font;
end;

function TdxGridWinExplorerViewGroupRecordProducer.GetRecordClass: TdxReportWinExplorerViewRecordClass;
begin
  Result := TdxReportWinExplorerViewGroupRecord;
end;

{ TdxGridCustomLayoutViewRecordProducer }

function TdxGridCustomLayoutViewRecordProducer.Builder: TdxGridCustomLayoutViewBuilder;
begin
  Result := inherited Builder as TdxGridCustomLayoutViewBuilder;
end;

function TdxGridCustomLayoutViewRecordProducer.Formatter: TdxGridCustomLayoutViewFormatter;
begin
  Result := inherited Formatter as TdxGridCustomLayoutViewFormatter;
end;

function TdxGridCustomLayoutViewRecordProducer.CreateRecord(AParent: TdxReportCell; AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecord;
begin
  Result := Formatter.GetRecordClass(AGridRecord).CreateEx(AParent, AGridRecord);
  Formatter.DoInitializeRecord(Result, AGridRecord);
end;

function TdxGridCustomLayoutViewRecordProducer.Produce(AHostInfo: TdxGridAttributeHostInfo; ARowHeight: Integer): TdxReportCell;
begin
  FRowHeight := ARowHeight;
  Result := inherited Produce(AHostInfo);
end;

procedure TdxGridCustomLayoutViewRecordProducer.InjectRecord(ARecord: TdxReportCustomLayoutRecord; AIndex: Integer);

  function CalculateLeft: Integer;
  var
    I: Integer;
    AStartIndex: Integer;
  begin
    Result := Formatter.GetInterRecordsSpaceHorz div 2;
    AStartIndex := Builder.FRecords.IndexOf(ARecord) - AIndex;
    for I := 0 to AIndex - 1 do
    begin
      Inc(Result, Formatter.GetInterRecordsSpaceHorz);
      Inc(Result, Builder.Records[AStartIndex + I].Width);
    end;
  end;

begin
  ARecord.Parent := Row;
  ARecord.Left := CalculateLeft;
  ARecord.Top := Formatter.GetInterRecordsSpaceVert div 2;
end;

procedure TdxGridCustomLayoutViewRecordProducer.CalculateRowHeight;
begin
// do nothing
end;

function TdxGridCustomLayoutViewRecordProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

procedure TdxGridCustomLayoutViewRecordProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeRecordRow(Row);
end;

{ TdxGridCardViewCardsRowProducer }

function TdxGridCardViewCardsRowProducer.Adapter: TdxGridCardViewAdapter;
begin
  Result := inherited Adapter as TdxGridCardViewAdapter;
end;

function TdxGridCardViewCardsRowProducer.Builder: TdxGridCardViewBuilder;
begin
  Result := inherited Builder as TdxGridCardViewBuilder;
end;

function TdxGridCardViewCardsRowProducer.Formatter: TdxGridCardViewFormatter;
begin
  Result := inherited Formatter as TdxGridCardViewFormatter;
end;

{ TdxGridViewChartRowProducer }

function TdxGridChartViewChartProducer.Produce(AHostInfo: TdxGridAttributeHostInfo): TdxReportCell;
begin
  try
    Result := inherited Produce(AHostInfo);
  finally
    FreeAndNil(FChartImage);
  end;
end;

function TdxGridChartViewChartProducer.Formatter: TdxGridChartViewFormatter;
begin
  Result := inherited Formatter as TdxGridChartViewFormatter;
end;

procedure TdxGridChartViewChartProducer.CalculateRowHeight;
begin
  if ChartImage <> nil then
    RowHeight := ChartImage.Height
  else
    inherited;
end;

procedure TdxGridChartViewChartProducer.InitializeItem;
begin
  inherited;
  Item.Image := ChartImage;

  Formatter.DoInitializeChartItem(Item);
  Formatter.DoReportLinkInitializeChartItem(Item);
end;

function TdxGridChartViewChartProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetChartItemClass;
end;

function TdxGridChartViewChartProducer.GetChartImage: TGraphic;
begin
  if FChartImage = nil then
    FChartImage := Formatter.CreateChartImage;
  Result := FChartImage;
end;

function TdxGridChartViewChartProducer.GetItem: TdxReportCellDpiAwareGraphic;
begin
  Result := inherited Item as TdxReportCellDpiAwareGraphic;
end;

{ TdxGridReportLinkOptions }

function TdxGridReportLinkOptions.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkOptionsCardsShadow }

constructor TdxGridReportLinkCardsShadow.Create(AnOptionsCards: TdxGridReportLinkOptionsCards);
begin
  inherited Create;
  RestoreDefaults;
  FOptionsCards := AnOptionsCards;
end;

procedure TdxGridReportLinkCardsShadow.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkCardsShadow then
    with TdxGridReportLinkCardsShadow(Source) do
    begin
      Self.Color := Color;
      Self.Depth := Depth;
      Self.Position := Position;
    end
  else
    inherited;
end;

procedure TdxGridReportLinkCardsShadow.RestoreDefaults;
begin
  inherited;
  Color := clDefault;
  Depth := dxDefaultCardsShadowDepth;
  Position := cspBottomRight;
end;

procedure TdxGridReportLinkCardsShadow.Changed;
begin
  if OptionsCards <> nil then OptionsCards.Changed;
end;

function TdxGridReportLinkCardsShadow.GetActualColor: TColor;
begin
  Result := Color;
  if Result = clDefault then
    Result := dxDefaultCardsShadowColor;
end;

function TdxGridReportLinkCardsShadow.GetVisible: Boolean;
begin
  Result := Depth <> 0;
end;

procedure TdxGridReportLinkCardsShadow.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Visible then Changed;
  end;
end;

procedure TdxGridReportLinkCardsShadow.SetDepth(Value: TdxGridCardShadowDepth);
begin
  if FDepth <> Value then
  begin
    FDepth := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkCardsShadow.SetPosition(Value: TdxGridCardShadowPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    if Visible then Changed;
  end;
end;

{ TdxGridReportLinkOptionsCustomLayoutView }

procedure TdxGridReportLinkOptionsCustomLayoutView.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsCustomLayoutView then
    with TdxGridReportLinkOptionsCustomLayoutView(Source) do
    begin
      Self.AutoWidth := AutoWidth;
      Self.InterRecordsSpaceHorz := InterRecordsSpaceHorz;
      Self.InterRecordsSpaceVert := InterRecordsSpaceVert;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsCustomLayoutView.RestoreDefaults;
begin
  inherited;
  AutoWidth := False;
  InterRecordsSpaceHorz := dxDefaultInterRecordsSpaceHorz;
  InterRecordsSpaceVert := dxDefaultInterRecordsSpaceVert;
end;

procedure TdxGridReportLinkOptionsCustomLayoutView.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCustomLayoutView.SetInterRecordsSpaceHorz(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FInterRecordsSpaceHorz <> Value then
  begin
    FInterRecordsSpaceHorz := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCustomLayoutView.SetInterRecordsSpaceVert(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FInterRecordsSpaceVert <> Value then
  begin
    FInterRecordsSpaceVert := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsCards }

destructor TdxGridReportLinkOptionsCards.Destroy;
begin
  FreeAndNil(FShadow);
  inherited;
end;

procedure TdxGridReportLinkOptionsCards.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsCards then
    with TdxGridReportLinkOptionsCards(Source) do
    begin
      Self.AutoWidth := AutoWidth;
      Self.Borders := Borders;
      Self.InterCardsSpaceHorz := InterCardsSpaceHorz;
      Self.InterCardsSpaceVert := InterCardsSpaceVert;
      Self.KeepSameHeight := KeepSameHeight;
      Self.KeepSameWidth := KeepSameWidth;
      Self.RowBordersHorz := RowBordersHorz;
      Self.RowBordersVert := RowBordersVert;
      Self.Shadow := Shadow;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsCards.RestoreDefaults;
begin
  inherited;
  AutoWidth := False;
  Borders := True;
  KeepSameHeight := True;
  KeepSameWidth := True;
  RowBordersHorz := False;
  RowBordersVert := False;
  Shadow.RestoreDefaults;
end;

function TdxGridReportLinkOptionsCards.DesignerTabIndex: Integer;
begin
  Result := 5;
end;

function TdxGridReportLinkOptionsCards.GetShadowClass: TdxGridReportLinkCardsShadowClass;
begin
  Result := TdxGridReportLinkCardsShadow;
end;

function TdxGridReportLinkOptionsCards.GetShadow: TdxGridReportLinkCardsShadow;
begin
  if FShadow = nil then
    FShadow := GetShadowClass.Create(Self);
  Result := FShadow;
end;

function TdxGridReportLinkOptionsCards.GetInterCardsSpaceHorz: Integer;
begin
  Result := InterRecordsSpaceHorz;
end;

function TdxGridReportLinkOptionsCards.GetInterCardsSpaceVert: Integer;
begin
  Result := InterRecordsSpaceVert;
end;

procedure TdxGridReportLinkOptionsCards.SetBorders(Value: Boolean);
begin
  if FBorders <> Value then
  begin
    FBorders := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCards.SetInterCardsSpaceHorz(Value: Integer);
begin
  InterRecordsSpaceHorz := Value;
end;

procedure TdxGridReportLinkOptionsCards.SetInterCardsSpaceVert(Value: Integer);
begin
  InterRecordsSpaceVert := Value;
end;

procedure TdxGridReportLinkOptionsCards.SetKeepSameHeight(Value: Boolean);
begin
  if FKeepSameHeight <> Value then
  begin
    FKeepSameHeight := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCards.SetKeepSameWidth(Value: Boolean);
begin
  if FKeepSameWidth <> Value then
  begin
    FKeepSameWidth := Value;
    if AutoWidth then Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCards.SetRowBordersHorz(Value: Boolean);
begin
  if FRowBordersHorz <> Value then
  begin
    FRowBordersHorz := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCards.SetRowBordersVert(Value: Boolean);
begin
  if FRowBordersVert <> Value then
  begin
    FRowBordersVert := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsCards.SetShadow(Value: TdxGridReportLinkCardsShadow);
begin
  Shadow.Assign(Value);
end;

{ TdxGridReportLinkOptionsPagination }

procedure TdxGridReportLinkOptionsPagination.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsPagination then
    with TdxGridReportLinkOptionsPagination(Source) do
    begin
      Self.OneGroupPerPage := OneGroupPerPage;
      Self.TopLevelGroup := TopLevelGroup;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsPagination.RestoreDefaults;
begin
  inherited;
  OneGroupPerPage := False;
  TopLevelGroup := False;
end;

function TdxGridReportLinkOptionsPagination.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TdxGridReportLinkOptionsPagination.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxGridReportLinkOptionsPagination.SetOneGroupPerPage(Value: Boolean);
begin
  if FOneGroupPerPage <> Value then
  begin
    FOneGroupPerPage := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsPagination.SetTopLevelGroup(Value: Boolean);
begin
  if FTopLevelGroup <> Value then
  begin
    FTopLevelGroup := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsCharts }

procedure TdxGridReportLinkOptionsCharts.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsCharts then
    with TdxGridReportLinkOptionsCharts(Source) do
    begin
      Self.GraphicClass := GraphicClass;
      Self.Transparent := Transparent;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsCharts.RestoreDefaults;
begin
  inherited;
  FIsGraphicClassAssigned := False;
  Transparent := False;
end;

function TdxGridReportLinkOptionsCharts.DefaultGraphicClass: TGraphicClass;
begin
  Result := TBitmap;
end;

function TdxGridReportLinkOptionsCharts.SupportsGraphicClass(AGraphicClass: TGraphicClass): Boolean;
begin
  Result := (AGraphicClass <> nil) and not AGraphicClass.InheritsFrom(TIcon);
end;

function TdxGridReportLinkOptionsCharts.DesignerTabIndex: Integer;
begin
  Result := 6;
end;

function TdxGridReportLinkOptionsCharts.GetGraphicClass: TGraphicClass;
begin
  if FIsGraphicClassAssigned then
    Result := FGraphicClass
  else
    Result := DefaultGraphicClass;
end;

function TdxGridReportLinkOptionsCharts.GetGraphicClassName: string;
begin
  Result := GraphicClass.ClassName;
end;

function TdxGridReportLinkOptionsCharts.IsGraphicClassNameStored: Boolean;
begin
  Result := FIsGraphicClassAssigned;
end;

procedure TdxGridReportLinkOptionsCharts.SetGraphicClass(Value: TGraphicClass);
begin
  if Value = nil then
    Value := DefaultGraphicClass;
  if (GraphicClass <> Value) and SupportsGraphicClass(Value) then
  begin
    FGraphicClass := Value;
    FIsGraphicClassAssigned := Value <> DefaultGraphicClass;
  end;
end;

procedure TdxGridReportLinkOptionsCharts.SetGraphicClassName(const Value: string);
var
  AClass: TClass;
begin
  AClass := Classes.GetClass(Value);
  if (AClass <> nil) and AClass.InheritsFrom(TGraphic) then
    GraphicClass := TGraphicClass(AClass)
  else
    GraphicClass := nil;
end;

procedure TdxGridReportLinkOptionsCharts.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsDetails }

procedure TdxGridReportLinkOptionsDetails.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsDetails then
    with TdxGridReportLinkOptionsDetails(Source) do
    begin
      Self.OnlyFocusedView := OnlyFocusedView;
      Self.StartFromFocusedView := StartFromFocusedView;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsDetails.RestoreDefaults;
begin
  inherited;
  OnlyFocusedView := False;
  StartFromFocusedView := False;
end;

function TdxGridReportLinkOptionsDetails.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

procedure TdxGridReportLinkOptionsDetails.SetOnlyFocusedView(Value: Boolean);
begin
  if FOnlyFocusedView <> Value then
  begin
    FOnlyFocusedView := Value;
    if StartFromFocusedView then Changed;
  end;
end;

procedure TdxGridReportLinkOptionsDetails.SetStartFromFocusedView(Value: Boolean);
begin
  if FStartFromFocusedView <> Value then
  begin
    FStartFromFocusedView := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsExpanding }

procedure TdxGridReportLinkOptionsExpanding.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsExpanding then
    with TdxGridReportLinkOptionsExpanding(Source) do
    begin
      Self.ExpandCards := ExpandCards;
      Self.ExpandGroupRows := ExpandGroupRows;
      Self.ExpandMasterRows := ExpandMasterRows;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsExpanding.RestoreDefaults;
begin
  inherited;
  ExpandCards := False;
  ExpandGroupRows := False;
  ExpandMasterRows := False;
end;

function TdxGridReportLinkOptionsExpanding.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TdxGridReportLinkOptionsExpanding.GetHasAny: Boolean;
begin
  Result := ExpandGroupRows or ExpandMasterRows  or ExpandCards ;
end;

function TdxGridReportLinkOptionsExpanding.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxGridReportLinkOptionsExpanding.SetExpandCards(Value: Boolean);
begin
  if FExpandCards <> Value then
  begin
    FExpandCards := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsExpanding.SetExpandGroupRows(Value: Boolean);
begin
  if FExpandGroupRows <> Value then
  begin
    FExpandGroupRows := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsExpanding.SetExpandMasterRows(Value: Boolean);
begin
  if FExpandMasterRows <> Value then
  begin
    FExpandMasterRows := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsFormatting }

procedure TdxGridReportLinkOptionsFormatting.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsFormatting then
    with TdxGridReportLinkOptionsFormatting(Source) do
    begin
      Self.ConsumeSelectionStyle := ConsumeSelectionStyle;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsFormatting.RestoreDefaults;
begin
  inherited;
  ConsumeSelectionStyle := False;
end;

function TdxGridReportLinkOptionsFormatting.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TdxGridReportLinkOptionsFormatting.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxGridReportLinkOptionsFormatting.SetConsumeSelectionStyle(Value: Boolean);
begin
  if FConsumeSelectionStyle <> Value then
  begin
    FConsumeSelectionStyle := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsLevels }

{ TdxGridReportLinkOptionsLevels }

procedure TdxGridReportLinkOptionsLevels.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsLevels then
    with TdxGridReportLinkOptionsLevels(Source) do
    begin
      Self.RiseActiveLevelOntoTop := RiseActiveLevelOntoTop;
      Self.SkipEmptyViews := SkipEmptyViews;
      Self.Unwrap := Unwrap;
      Self.UnwrapTopLevel := UnwrapTopLevel;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsLevels.RestoreDefaults;
begin
  inherited;
  RiseActiveLevelOntoTop := True;
  SkipEmptyViews := True;
  Unwrap := False;
  UnwrapTopLevel := True;
end;

function TdxGridReportLinkOptionsLevels.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

procedure TdxGridReportLinkOptionsLevels.SetRiseActiveLevelOntoTop(Value: Boolean);
begin
  if FRiseActiveLevelOntoTop <> Value then
  begin
    FRiseActiveLevelOntoTop := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsLevels.SetSkipEmptyViews(Value: Boolean);
begin
  if FSkipEmptyViews <> Value then
  begin
    FSkipEmptyViews := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsLevels.SetUnwrap(Value: Boolean);
begin
  if FUnwrap <> Value then
  begin
    FUnwrap := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsLevels.SetUnwrapTopLevel(Value: Boolean);
begin
  if FUnwrapTopLevel <> Value then
  begin
    FUnwrapTopLevel := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsOnEveryPage }

procedure TdxGridReportLinkOptionsOnEveryPage.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsOnEveryPage then
    with TdxGridReportLinkOptionsOnEveryPage(Source) do
    begin
      Self.Caption := Caption;
      Self.FilterBar := FilterBar;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsOnEveryPage.RestoreDefaults;
begin
  inherited;
  Caption := True;
  FilterBar := True;
end;

function TdxGridReportLinkOptionsOnEveryPage.HasInstalledAttribute(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  if AnAttribute.InheritsFrom(TdxGridLevelCaption) then
    Result := Caption
  else
    if AnAttribute.InheritsFrom(TdxGridBandHeader) then
      Result := BandHeaders
    else
      if AnAttribute.InheritsFrom(TdxGridHeader) then
        Result := Headers
      else
        if AnAttribute.InheritsFrom(TdxGridFooter) then
          Result := Footers
        else
          if AnAttribute.InheritsFrom(TdxGridFilterBar) then
            Result := FilterBar
          else
            Result := False;
end;

procedure TdxGridReportLinkOptionsOnEveryPage.SetAll;
begin
  BandHeaders := True;
  Caption := True;
  FilterBar := True;
  Footers := True;
  Headers := True;
end;

procedure TdxGridReportLinkOptionsOnEveryPage.UnsetAll;
begin
  BandHeaders := False;
  Caption := False;
  FilterBar := False;
  Footers := False;
  Headers := False;
end;

function TdxGridReportLinkOptionsOnEveryPage.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TdxGridReportLinkOptionsOnEveryPage.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxGridReportLinkOptionsOnEveryPage.SetCaption(Value: Boolean);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsOnEveryPage.SetFilterBar(Value: Boolean);
begin
  if FFilterBar <> Value then
  begin
    FFilterBar := Value;
    Changed;
  end;
end;

{ TdxGridReportLinkOptionsPreview }

function TdxGridReportLinkOptionsPreview.DesignerTabIndex: Integer;
begin
  Result := 4;
end;

function TdxGridReportLinkOptionsPreview.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkOptionsRefinements }

function TdxGridReportLinkOptionsRefinements.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TdxGridReportLinkOptionsRefinements.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkOptionsSelection }

function TdxGridReportLinkOptionsSelection.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TdxGridReportLinkOptionsSelection.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkOptionsSize }

function TdxGridReportLinkOptionsSize.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TdxGridReportLinkOptionsSize.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkOptionsView }

procedure TdxGridReportLinkOptionsView.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLinkOptionsView then
    with TdxGridReportLinkOptionsView(Source) do
    begin
      Self.Caption := Caption;
      Self.FilterBar := FilterBar;
      Self.GroupFooters := GroupFooters;
    end;
  inherited;
end;

procedure TdxGridReportLinkOptionsView.RestoreDefaults;
begin
  inherited;
  Caption := True;
  FilterBar := True;
  GroupFooters := True;
end;

function TdxGridReportLinkOptionsView.HasInstalledAttribute(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  if AnAttribute.InheritsFrom(TdxGridLevelCaption) then
    Result := Caption
  else
    if AnAttribute.InheritsFrom(TdxGridBandHeader) then
      Result := BandHeaders
    else
      if AnAttribute.InheritsFrom(TdxGridHeader) then
        Result := Headers
      else
        if AnAttribute.InheritsFrom(TdxGridFooter) then
          Result := Footers
        else
          if AnAttribute.InheritsFrom(TdxGridFilterBar) then
            Result := FilterBar
          else
            Result := False;
end;

procedure TdxGridReportLinkOptionsView.SetAll;
begin
  BandHeaders := True;
  Caption := True;
  FilterBar := True;
  Footers := True;
  GroupFooters := True;
  Headers := True;
  ExpandButtons := True;
end;

procedure TdxGridReportLinkOptionsView.UnsetAll;
begin
  BandHeaders := False;
  Caption := False;
  FilterBar := False;
  Footers := False;
  GroupFooters := False;
  Headers := False;
  ExpandButtons := False;
end;

function TdxGridReportLinkOptionsView.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TdxGridReportLinkOptionsView.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxGridReportLinkOptionsView.SetCaption(Value: Boolean);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsView.SetFilterBar(Value: Boolean);
begin
  if FFilterBar <> Value then
  begin
    FFilterBar := Value;
    Changed;
  end;
end;

procedure TdxGridReportLinkOptionsView.SetGroupFooters(Value: Boolean);
begin
  if FGroupFooters <> Value then
  begin
    FGroupFooters := Value;
    Changed;
  end;
end;

{ TdxGridAttributeHostInfo }

procedure TdxGridAttributeHostInfo.Initialize(AParent: TdxReportCell);
begin
  Origin := cxNullPoint;
  FParent := AParent;
end;

{ TdxGridAttributeHostInfoServices }

constructor TdxGridAttributeHostInfoServices.Create(AReportLink: TdxGridReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  CreateHostInfos;
end;

destructor TdxGridAttributeHostInfoServices.Destroy;
begin
  DestroyHostInfos;
  inherited;
end;

procedure TdxGridAttributeHostInfoServices.Initialize;
begin
  PageDetailsHostInfo.Initialize(PageDetails);
  PageFootersHostInfo.Initialize(PageFooters);
  PageHeadersHostInfo.Initialize(PageHeaders);
end;

procedure TdxGridAttributeHostInfoServices.CreateHostInfos;
begin
  FPageDetailsHostInfo := TdxGridAttributeHostInfo.Create;
  FPageFootersHostInfo := TdxGridAttributeHostInfo.Create;
  FPageHeadersHostInfo := TdxGridAttributeHostInfo.Create;
end;

procedure TdxGridAttributeHostInfoServices.DestroyHostInfos;
begin
  FreeAndNil(FPageHeadersHostInfo);
  FreeAndNil(FPageFootersHostInfo);
  FreeAndNil(FPageDetailsHostInfo);
end;

function TdxGridAttributeHostInfoServices.GetBandHeadersHostInfo: TdxGridAttributeHostInfo;
begin
  if CanUseBandHeadersOnEveyPage then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetCaptionHostInfo: TdxGridAttributeHostInfo;
begin
  if CanUseCaptionOnEveryPage then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetFilterBarHostInfo: TdxGridAttributeHostInfo;
begin
  if CanUseFilterBarOnEveryPage then
    if ActiveViewHelper.IsFilterBarAtTop(ActiveView) then
      Result := PageHeadersHostInfo
    else
      Result := PageFootersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetFootersHostInfo: TdxGridAttributeHostInfo;
begin
  if CanUseFootersOnEveryPage then
    Result := PageFootersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetHeadersHostInfo: TdxGridAttributeHostInfo;
begin
  if CanUseHeadersOnEveryPage then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetInconsistentStateText: string;
begin
  if ReportLink.IsAggregated then
    Result := cxGetResourceString(@sdxCannotUseOnEveryPageModeInAggregatedState)
  else
    if ReportLink.ProcessParams.HasMasterDetails then
      Result := cxGetResourceString(@sdxCannotUseOnEveryPageMode)
    else
      if IsInconsistentFilterBarState then
        Result := cxGetResourceString(@sdxIncorrectFilterBarState)
      else
        if IsInconsistentBandHeadersState then
          if ActiveViewHelper.IsFilterBarAtTop(ActiveView) then
            Result := cxGetResourceString(@sdxIncorrectBandHeadersState2)
          else
            Result := cxGetResourceString(@sdxIncorrectBandHeadersState)
        else
          if IsInconsistentHeadersState then
            if ActiveViewHelper.IsFilterBarAtTop(ActiveView) then
              Result := cxGetResourceString(@sdxIncorrectHeadersState2)
            else
              Result := cxGetResourceString(@sdxIncorrectHeadersState)
          else
            if IsInconsistentFootersState then
              Result := cxGetResourceString(@sdxIncorrectFootersState)
            else
              Result := '';
end;

function TdxGridAttributeHostInfoServices.GetIsInconsistentState: Boolean;
begin
  Result := ReportLink.IsAggregated or ReportLink.ProcessParams.HasMasterDetails or
    IsInconsistentFilterBarState or IsInconsistentBandHeadersState or
    IsInconsistentHeadersState or IsInconsistentFootersState;
end;

function TdxGridAttributeHostInfoServices.HasCells: Boolean;
begin
  Result := ReportLink.ReportCells <> nil;
end;

function TdxGridAttributeHostInfoServices.IsAttributeSupported(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  Result := ActiveViewHelper.IsAttributeSupported(AnAttribute)
end;

function TdxGridAttributeHostInfoServices.GetActiveView: TcxCustomGridView;
begin
  Result := ReportLink.ActiveView;
end;

function TdxGridAttributeHostInfoServices.GetActiveViewHelper: TdxCustomGridViewHelperClass;
begin
  Result := ReportLink.ActiveViewHelper;
//  if ActiveView = nil then
//    Result := TdxNullGridViewHelper
//  else
//    Result := dxViewHelpersFactory[ActiveView];
end;

function TdxGridAttributeHostInfoServices.GetArePageFootersAssigned: Boolean;
begin
  with ReportLink.ReportCells do
    Result := (FootersHostInfo.Parent = FooterCells) or (FilterBarHostInfo.Parent = FooterCells);
end;

function TdxGridAttributeHostInfoServices.GetArePageHeadersAssigned: Boolean;
begin
  with ReportLink.ReportCells do
    Result := (CaptionHostInfo.Parent = HeaderCells) or (FilterBarHostInfo.Parent = HeaderCells) or
      (BandHeadersHostInfo.Parent = HeaderCells) or (HeadersHostInfo.Parent = HeaderCells);
end;

function TdxGridAttributeHostInfoServices.GetCanUseBandHeadersOnEveyPage: Boolean;
begin
  Result := ReportLink.CanAttributeBeUsedOnEveryPage(TdxGridBandHeader) and
    ReportLink.IsAttributeUsedOnEveryPage(TdxGridLevelCaption) and
    (ActiveViewHelper.IsFilterBarAtBottom(ActiveView) or ReportLink.IsAttributeUsedOnEveryPage(TdxGridFilterBar));
end;

function TdxGridAttributeHostInfoServices.GetCanUseCaptionOnEveryPage: Boolean;
begin
  Result := ReportLink.CanAttributeBeUsedOnEveryPage(TdxGridLevelCaption);
end;

function TdxGridAttributeHostInfoServices.GetCanUseFilterBarOnEveryPage: Boolean;
begin
  Result := ReportLink.CanAttributeBeUsedOnEveryPage(TdxGridFilterBar) and
    ReportLink.IsAttributeUsedOnEveryPage(TdxGridLevelCaption);
end;

function TdxGridAttributeHostInfoServices.GetCanUseFootersOnEveryPage: Boolean;
begin
  Result := ReportLink.CanAttributeBeUsedOnEveryPage(TdxGridFooter) and
    (ActiveViewHelper.IsFilterBarAtTop(ActiveView) or ReportLink.IsAttributeUsedOnEveryPage(TdxGridFilterBar));
end;

function TdxGridAttributeHostInfoServices.GetCanUseHeadersOnEveryPage: Boolean;
begin
  Result := ReportLink.CanAttributeBeUsedOnEveryPage(TdxGridHeader) and
    ReportLink.IsAttributeUsedOnEveryPage(TdxGridLevelCaption) and
    (ActiveViewHelper.IsFilterBarAtBottom(ActiveView) or ReportLink.IsAttributeUsedOnEveryPage(TdxGridFilterBar)) and
    ReportLink.IsAttributeUsedOnEveryPage(TdxGridBandHeader);
end;

function TdxGridAttributeHostInfoServices.GetIsInconsistentBandHeadersState: Boolean;
begin
  Result := IsAttributeSupported(TdxGridBandHeader) and OptionsOnEveryPage.BandHeaders and OptionsView.BandHeaders and
    ((IsAttributeSupported(TdxGridLevelCaption) and OptionsView.Caption and not OptionsOnEveryPage.Caption) or
     (IsAttributeSupported(TdxGridFilterBar) and ActiveViewHelper.IsFilterBarAtTop(ActiveView) and
      OptionsView.FilterBar and not OptionsOnEveryPage.FilterBar));
end;

function TdxGridAttributeHostInfoServices.GetIsInconsistentFilterBarState: Boolean;
begin
  Result := ActiveViewHelper.IsFilterBarAtTop(ActiveView) and
    (IsAttributeSupported(TdxGridFilterBar) and OptionsView.FilterBar and OptionsOnEveryPage.FilterBar and
    IsAttributeSupported(TdxGridLevelCaption) and OptionsView.Caption and not OptionsOnEveryPage.Caption);
end;

function TdxGridAttributeHostInfoServices.GetIsInconsistentFootersState: Boolean;
begin
  Result := IsAttributeSupported(TdxGridFooter) and OptionsView.Footers and OptionsOnEveryPage.Footers and
    IsAttributeSupported(TdxGridFilterBar) and ActiveViewHelper.IsFilterBarAtBottom(ActiveView) and
    OptionsView.FilterBar and not OptionsOnEveryPage.FilterBar;
end;

function TdxGridAttributeHostInfoServices.GetIsInconsistentHeadersState: Boolean;
begin
  Result := IsAttributeSupported(TdxGridHeader) and OptionsView.Headers and OptionsOnEveryPage.Headers and
    ((IsAttributeSupported(TdxGridLevelCaption) and OptionsView.Caption and not OptionsOnEveryPage.Caption) or
     (IsAttributeSupported(TdxGridFilterBar) and ActiveViewHelper.IsFilterBarAtTop(ActiveView) and
      OptionsView.FilterBar and not OptionsOnEveryPage.FilterBar) or
     (IsAttributeSupported(TdxGridBandHeader) and OptionsView.BandHeaders and not OptionsOnEveryPage.BandHeaders));
end;

function TdxGridAttributeHostInfoServices.GetLevelSeparatorBottomHostInfo: TdxGridAttributeHostInfo;
begin
  if ArePageFootersAssigned then
    Result := PageFootersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetLevelSeparatorTopHostInfo: TdxGridAttributeHostInfo;
begin
  if ArePageHeadersAssigned then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TdxGridAttributeHostInfoServices.GetOptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage;
begin
  Result := ReportLink.OptionsOnEveryPage;
end;

function TdxGridAttributeHostInfoServices.GetOptionsView: TdxGridReportLinkOptionsView;
begin
  Result := ReportLink.OptionsView;
end;

function TdxGridAttributeHostInfoServices.GetPageDetails: TdxReportCell;
begin
  if HasCells then
    Result := ReportLink.ReportCells.Cells
  else
    Result := nil;
end;

function TdxGridAttributeHostInfoServices.GetPageFooters: TdxReportCell;
begin
  if HasCells and not ReportLink.ProcessParams.HasMasterDetails then
    Result := ReportLink.ReportCells.FooterCells
  else
    Result := PageDetails;
end;

function TdxGridAttributeHostInfoServices.GetPageHeaders: TdxReportCell;
begin
  if HasCells and not ReportLink.ProcessParams.HasMasterDetails then
    Result := ReportLink.ReportCells.HeaderCells
  else
    Result := PageDetails;
end;

{ TdxGridReportLinkStyles }

procedure TdxGridReportLinkStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxGridReportLinkStyles then
    with TdxGridReportLinkStyles(Source) do
    begin
      Self.BandHeader := BandHeader;
      Self.Caption := Caption;
      Self.CardCaptionRow := CardCaptionRow;
      Self.CardRowCaption := CardRowCaption;
      Self.Content := Content;
      Self.ContentEven := ContentEven;
      Self.ContentOdd := ContentOdd;
      Self.FilterBar := FilterBar;
      Self.Footer := Footer;
      Self.Group := Group;
      Self.Header := Header;
      Self.Preview := Preview;
      Self.Selection := Selection;
    end; // TODO: assign to TcxGridXXXStyles
end;

procedure TdxGridReportLinkStyles.GetBandHeaderParams(ABand: TcxGridBand;
  out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridBandHeader, ABand, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetCaptionParams(ATabLevel: TcxGridLevel; out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridCaption, ATabLevel, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetCardCaptionRowParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  DataCellPos: TdxPSGridDataCellPos;
begin
  DataCellPos := CreatePSDataCellPos;
  try
    with DataCellPos do
    begin
      GridRecord := ARecord;
      Item := AItem;
    end;
    GetViewParams(vspsGridCardCaptionRow, DataCellPos, nil, AParams);
  finally
    FreePSDataCellPos(DataCellPos);
  end;
end;

procedure TdxGridReportLinkStyles.GetCardRowCaptionParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  DataCellPos: TdxPSGridDataCellPos;
begin
  DataCellPos := CreatePSDataCellPos;
  try
    with DataCellPos do
    begin
      GridRecord := ARecord;
      Item := AItem;
    end;
    GetViewParams(vspsGridCardRowCaption, DataCellPos, nil, AParams);
  finally
    FreePSDataCellPos(DataCellPos);
  end;
end;

procedure TdxGridReportLinkStyles.GetContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
const
  StyleIndexes: array[Boolean] of Integer = (vspsGridContentEven, vspsGridContentOdd);
var
  DataCellPos: TdxPSGridDataCellPos;
begin
  if (ARecord <> nil) and (GetValue(StyleIndexes[Odd(ARecord.Index)]) <> nil) then
  begin
    DataCellPos := CreatePSDataCellPos;
    try
      with DataCellPos do
      begin
        GridRecord := ARecord;
        Item := AItem;
      end;
      GetViewParams(StyleIndexes[Odd(ARecord.Index)], DataCellPos, nil, AParams);
    finally
      FreePSDataCellPos(DataCellPos);
    end;
  end
  else
    GetViewParams(vspsGridContent, ARecord, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetFilterBarParams(out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridFilterBar, nil, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetFooterParams(ARecord: TcxCustomGridRecord;
  AGroupLevel: Integer; AItem: TcxGridColumn; out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridFooter, ARecord, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetGroupParams(ARecord: TcxCustomGridRecord;
  AGroupLevel: Integer; out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridGroup, ARecord, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetHeaderParams(AItem: TcxGridColumn;
  out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridHeader, nil, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetPreviewParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridPreview, ARecord, nil, AParams);
end;

procedure TdxGridReportLinkStyles.GetSelectionParams(out AParams: TcxViewParams);
begin
  GetViewParams(vspsGridSelection, nil, nil, AParams);
end;

function TdxGridReportLinkStyles.DesignerTabIndex: Integer;
begin
  Result := 3;
end;

procedure TdxGridReportLinkStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
const
  FixedParts = [vspsGridBandHeader, vspsGridCaption, vspsGridFooter, vspsGridHeader,
    vspsGridSelection];
begin
  inherited;
  if ReportLink <> nil then
    with AParams do
    begin
      if Index in FixedParts then
        Color := dxPSCore.dxDefaultFixedColor
      else
        if Index  = vspsGridGroup then
          if ReportLink.IsOffice11StyleGrouping and (AData = nil) then
            Color := dxPSCore.dxDefaultFixedColor
          else
            Color := dxPSCore.dxDefaultContentColor
        else
          if Index = vspsGridFilterBar then
            Color := DefaultFilterBarColor
          else
            Color := dxPSCore.dxDefaultContentColor;

      Font := ReportLink.Font;

      if Index = vspsGridFilterBar then
        TextColor := DefaultFilterBarTextColor
      else
        TextColor := Font.Color;
    end;
end;

class function TdxGridReportLinkStyles.GetStyleCaption(AnIndex: Integer): string;
begin
  case AnIndex of
    vspsGridBandHeader:
      Result := cxGetResourceString(@sdxBandHeaderStyle);
    vspsGridCaption:
      Result := cxGetResourceString(@sdxCaptionStyle);
    vspsGridCardCaptionRow:
      Result := cxGetResourceString(@sdxCardCaptionRowStyle);
    vspsGridCardRowCaption:
      Result := cxGetResourceString(@sdxCardRowCaptionStyle);
    vspsGridContent:
      Result := cxGetResourceString(@sdxContentStyle);
    vspsGridContentEven:
      Result := cxGetResourceString(@sdxContentEvenStyle);
    vspsGridContentOdd:
      Result := cxGetResourceString(@sdxContentOddStyle);
    vspsGridFilterBar:
      Result := cxGetResourceString(@sdxFilterBarStyle);
    vspsGridFooter:
      Result := cxGetResourceString(@sdxFooterStyle);
    vspsGridGroup:
      Result := cxGetResourceString(@sdxGroupStyle);
    vspsGridHeader:
      Result := cxGetResourceString(@sdxHeaderStyle);
    vspsGridPreview:
      Result := cxGetResourceString(@sdxPreviewStyle);
  else
    Result := cxGetResourceString(@sdxSelectionStyle);
  end;
end;

function TdxGridReportLinkStyles.GetStyleIndexByCaption(const Caption: string): Integer;
begin
  for Result := vspsGridFirst to vspsGridLast do
    if dxPSUtl.dxSameText(Caption, GetStyleCaption(Result)) then
      Exit;
  Result := -1;
end;

function TdxGridReportLinkStyles.IsCardViewStyle(AStyle: TcxStyle): Boolean;
begin
  Result := (AStyle <> nil) and ((AStyle = CardRowCaption) or (AStyle = CardCaptionRow));
end;

function TdxGridReportLinkStyles.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

{ TdxGridReportLinkStyleSheet }

class function TdxGridReportLinkStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TdxGridReportLinkStyles;
end;

function TdxGridReportLinkStyleSheet.GetStylesValue: TdxGridReportLinkStyles;
begin
  if GetStyles is TdxGridReportLinkStyles then
    Result := TdxGridReportLinkStyles(GetStyles)
  else
    Result := nil;
end;

procedure TdxGridReportLinkStyleSheet.SetStylesValue(Value: TdxGridReportLinkStyles);
begin
  SetStyles(Value);
end;

{ TdxGridReportLink }

constructor TdxGridReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelimitersHardVert := TList.Create;
  FDelimitersHardHorz := TList.Create;
  FHostInfoServices := TdxGridAttributeHostInfoServices.Create(Self);
  FReportRows := TList.Create;
  InternalRestoreDefaults;
  LinkModified(False);
end;

destructor TdxGridReportLink.Destroy;
begin
  FreeAndNil(FReportRows);
  FreeAndNil(FHostInfoServices);
  FreeAndNil(FDelimitersHardVert);
  FreeAndNil(FDelimitersHardHorz);
  inherited Destroy;
end;

procedure TdxGridReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxGridReportLink then
    with TdxGridReportLink(Source) do
    begin
      Self.OptionsCards := OptionsCards;
      Self.OptionsCharts := OptionsCharts;
      Self.OptionsDetails := OptionsDetails;
      Self.OptionsLevels := OptionsLevels;
    end;
  inherited;
end;

procedure TdxGridReportLink.AddHorizontalPageBreak(AGridColumn: TcxGridColumn);
var
  ACellData: TAbstractdxReportCellData;
begin
  if not AGridColumn.IsFirst then
  begin
    ACellData := GetReportDataCellByGridColumn(AGridColumn);
    if Assigned(ACellData) then
      AddHorizontalHardDelimiter(ACellData.AbsoluteOrigin.X);
  end;
end;

procedure TdxGridReportLink.AddHorizontalPageBreak(AGridColumns: TList);
var
  I: Integer;
begin
  for I := 0 to AGridColumns.Count - 1 do
    AddHorizontalPageBreak(TcxGridColumn(AGridColumns[I]));
end;

procedure TdxGridReportLink.AddHorizontalPageBreak(const AGridColumns: array of TcxGridColumn);
var
  I: Integer;
begin
  for I := 0 to Length(AGridColumns) - 1 do
    AddHorizontalPageBreak(AGridColumns[I]);
end;

procedure TdxGridReportLink.AddPageBreak(AGridRecord: TcxCustomGridRecord);
var
  ReportRow: TdxReportCell;
begin
  ReportRow := ReportRowsByGridRecord[AGridRecord];
  if ReportRow <> nil then
    AddVerticalHardDelimiter(ReportRow);
end;

procedure TdxGridReportLink.AddPageBreak(const AGridRecords: array of TcxCustomGridRecord);
var
  I: Integer;
begin
  for I := Low(AGridRecords) to High(AGridRecords) do
    AddPageBreak(AGridRecords[I]);
end;

procedure TdxGridReportLink.AddPageBreak(const AGridRecords: TdxGridRecordArray);
var
  I: Integer;
begin
  for I := 0 to Length(AGridRecords) - 1 do
    AddPageBreak(AGridRecords[I]);
end;

procedure TdxGridReportLink.AddPageBreak(AGridRecords: TList);
var
  I: Integer;
  P: Pointer;
begin
  for I := 0 to AGridRecords.Count - 1 do
  begin
    P := AGridRecords[I];
    if TObject(P) is TcxCustomGridRecord then
      AddPageBreak(TcxCustomGridRecord(P));
  end;
end;

procedure TdxGridReportLink.BeforeDesignReport;
begin
  CalculateProcessParams;
  inherited BeforeDesignReport;
end;

function TdxGridReportLink.CalculateActualScaleFactor: Integer;
begin
  if OptionsSize.AutoWidth then
  begin
    if FActualScaleFactor > 0 then
      Result := FActualScaleFactor
    else
      Result := 100;
  end
  else
    Result := inherited CalculateActualScaleFactor;
end;

procedure TdxGridReportLink.ConstructReport(AReportCells: TdxReportCells);

  function CalculateActualScaleFactor: Integer;
  begin
    Result := RenderInfo.CalculateActualScaleFactor(0, RealPrinterPage.FitToPagesVertically, PixelsPerInch);
  end;

  procedure CalculateReportPartSizes(ACell: TdxReportCell);
  var
    AWidth, I: Integer;
  begin
    if ACell.CellCount > 0 then
    begin
      AWidth := 0;
      for I := 0 to ACell.CellCount - 1 do
        AWidth := Max(AWidth, ACell.Cells[I].Width);
      ACell.BoundsRect := Rect(0, 0, AWidth, ACell.LastCell.BoundsRect.Bottom);
    end;
  end;

  procedure CalculateSizes;
  begin
    CalculateReportPartSizes(AReportCells.Cells);
    if AReportCells.AreFooterCellsAllocated then
      CalculateReportPartSizes(AReportCells.FooterCells);
    if AReportCells.AreHeaderCellsAllocated then
      CalculateReportPartSizes(AReportCells.HeaderCells);
  end;

  procedure ConstructReportCore;
  begin
    PrepareConstruct;
    try
      BuildTopLevelViews;
      if not AbortBuilding then
        CalculateSizes;
    finally
      UnprepareConstruct;
    end;
  end;

  function IsSecondPassNeeded: Boolean;
  begin
    Result := OptionsSize.AutoWidth and (RealPrinterPage.ScaleMode = smFit) and (RealPrinterPage.FitToPagesVertically > 0);
  end;

begin
  if Grid = nil then
    Exit;

  inherited;

  if ActiveView <> nil then
  begin
    FActualScaleFactor := -1;
    ConstructReportCore;
    if IsSecondPassNeeded then
    begin
      FActualScaleFactor := Min(100, CalculateActualScaleFactor);
      if FActualScaleFactor < 100 then
        ConstructReportCore;
    end;
  end;
end;

procedure TdxGridReportLink.ConvertCoords;
begin
  inherited ConvertCoords;
  ConvertDelimiters(DelimitersHardHorz);
  ConvertDelimiters(DelimitersHardVert);
end;

procedure TdxGridReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  DrawInfo: TdxGridCellCustomDrawInfo;
begin
  GetItemCustomDrawInfo(AItem, DrawInfo);
  with DrawInfo do
    case GridAttributeID of
      dxGridBandID:
        DoCustomDrawBandCell(ACanvas, TcxGridBandedTableView(GridView), GridBand,
          TdxReportCellString(AItem), ADone);
      dxGridCardRowCaptionID:
        DoCustomDrawCardRowCaptionCell(ACanvas, TcxGridCardView(GridView), GridCard,
          GridCardRow, TdxReportCellString(AItem), ADone);
      dxGridCardRowDataID:
        DoCustomDrawCardRowDataCell(ACanvas, TcxGridCardView(GridView), GridCard,
          GridCardRow, AItem, ADone);
      dxGridFilterBarID:
        DoCustomDrawFilterBar(ACanvas, TcxCustomGridTableView(GridView),
          TdxReportCellString(AItem), ADone);
      dxGridFooterID,
      dxGridGroupFooterID:
        DoCustomDrawFooterCell(ACanvas, TcxGridTableView(GridView), GridRecord,
          GridColumn, GroupLevel, TdxReportCellString(AItem), ADone);
      dxGridHeaderID:
        DoCustomDrawHeaderCell(ACanvas, TcxGridTableView(GridView), GridColumn,
          TdxReportCellString(AItem), ADone);
      dxGridRecordID:
        DoCustomDrawCell(ACanvas, TcxCustomGridTableView(GridView), GridRecord,
          GridColumn, AItem, ADone);
      dxGridLevelCaptionID:
        DoCustomDrawLevelCaption(ACanvas, GridView, TdxReportCellString(AItem), ADone);
    end;
end;

function TdxGridReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  with OptionsPagination do
    Result := OneGroupPerPage or Custom;
end;

function TdxGridReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := (AUpdateCodes * uaMarginsVert  <> []) and (ProcessParams.HasOnlyCards or ProcessParams.HasCardsInDetail or
    OptionsSize.AutoWidth);
end;

function TdxGridReportLink.GetUseHardHorzDelimiters: Boolean;
begin
  Result := OptionsPagination.Custom;
end;

function TdxGridReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := OptionsPagination.TopLevelGroup or OptionsPagination.Custom;
end;

procedure TdxGridReportLink.InternalRestoreDefaults;
begin
  inherited InternalRestoreDefaults;
  OptionsCards.RestoreDefaults;
  OptionsCharts.RestoreDefaults;
  OptionsDetails.RestoreDefaults;
  OptionsLevels.RestoreDefaults;
end;

procedure TdxGridReportLink.InternalRestoreFromOriginal;
var
  View: TcxCustomGridView;
begin
  inherited;
  Color := clWhite;

  if Grid <> nil then
  begin
    OptionsFormatting.LookAndFeelKind := Grid.LookAndFeel.Kind;
    OptionsView.Caption := Grid.RootLevelOptions.DetailTabsPosition <> dtpNone;

    View := ActiveView;
    if View <> nil then
    begin
      if View is TcxGridTableView then
      begin
        OptionsPreview.AutoHeight := TcxGridTableView(View).Preview.AutoHeight;
        OptionsPreview.MaxLineCount := TcxGridTableView(View).Preview.MaxLineCount;
        OptionsPreview.Visible := TcxGridTableView(View).Preview.Visible;

        if View is TcxGridBandedTableView then
          OptionsView.BandHeaders := TcxGridBandedTableView(View).OptionsView.BandHeaders;
        OptionsView.FilterBar := (TcxGridTableView(View).Filtering.Visible = fvAlways) or
          ((TcxGridTableView(View).Filtering.Visible = fvNonEmpty) and
           (TcxGridTableView(View).DataController.Filter.FilterText <> ''));
        OptionsView.Footers := TcxGridTableView(View).OptionsView.Footer;
        OptionsView.GroupFooters := TcxGridTableView(View).OptionsView.GroupFooters <> gfInvisible;
        OptionsView.Headers := TcxGridTableView(View).OptionsView.Header;

        OptionsSize.AutoWidth := TcxGridTableView(View).OptionsView.ColumnAutoWidth;
      end;

      if View is TcxGridCardView then
        OptionsView.FilterBar := False;
    end;
  end;
end;

function TdxGridReportLink.IsDrawFootersOnEveryPage: Boolean;
begin
  Result := HostInfoServices.ArePageFootersAssigned;
end;

function TdxGridReportLink.IsDrawHeadersOnEveryPage: Boolean;
begin
  Result := HostInfoServices.ArePageHeadersAssigned;
end;

function TdxGridReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
var
  DrawInfo: TdxGridCellCustomDrawInfo;
begin
  Result := inherited IsSupportedCustomDraw(Item) and
    (Item <> nil) and IsCustomDrawn(GetItemCustomDrawInfo(Item, DrawInfo));
end;

procedure TdxGridReportLink.MakeHardDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited MakeHardDelimiters(AReportCells, AHorzDelimiters, AVertDelimiters);
  if OptionsPagination.Custom then
  try
    DoGetCustomPageBreaks;
  except
    Application.HandleException(Self);
  end;
  dxCopyList(DelimitersHardHorz, AHorzDelimiters);
  dxCopyList(DelimitersHardVert, AVertDelimiters);
end;

function TdxGridReportLink.NeedExpandGroups(AGridView: TcxCustomGridView; ARecursive: Boolean = True): Boolean;
var
  I: Integer;
  ALevel: TcxGridLevel;
begin
  Result := AGridView.DataController.MultiSelectionSyncGroupWithChildren;
  if Result or not ARecursive then
    Exit;
  ALevel := TcxGridLevel(AGridView.Level);
  for I := 0 to ALevel.VisibleCount - 1 do
  begin
    AGridView := ALevel.VisibleItems[I].GridView;
    Result := (AGridView <> nil) and NeedExpandGroups(AGridView);
    if Result then
      Exit;
  end;
end;

function TdxGridReportLink.GetAreNativeStylesAvailable: Boolean;
begin
  Result := OptionsFormatting.UseNativeStyles;
end;

function TdxGridReportLink.GetStylesClass: TdxCustomReportLinkStylesClass;
begin
  Result := TdxGridReportLinkStyles;
end;

function TdxGridReportLink.GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass;
begin
  Result := TdxGridReportLinkStyleSheet;
end;

function TdxGridReportLink.GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet;
begin
  Result := GridLinkStyleSheetPrototype;
end;

procedure TdxGridReportLink.PrepareConstruct;
begin
  inherited PrepareConstruct;
  DelimitersHardHorz.Clear;
  DelimitersHardVert.Clear;
  FReportRows.Clear;

  ReportCells.LookAndFeel := nil;//ReportCells.CreateGroupLookAndFeel(TdxPSReportGroupNullLookAndFeel);

  if OptionsExpanding.HasAny or NeedExpandGroups(ActiveView, not OptionsDetails.OnlyFocusedView) then
    DoExpandRows;

  CalculateProcessParams;
  HostInfoServices.Initialize;
end;

procedure TdxGridReportLink.DoCustomDrawBandCell(ACanvas: TCanvas;
  AView: TcxGridBandedTableView; ABand: TcxGridBand; AnItem: TdxReportCellString;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawBandCell) then
    FOnCustomDrawBandCell(Self, ACanvas, AView, ABand, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawCardRowCaptionCell(ACanvas: TCanvas;
  AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
  AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawCardRowCaptionCell) then
    FOnCustomDrawCardRowCaptionCell(Self, ACanvas, AView, ACard, ARow, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawCardRowDataCell(ACanvas: TCanvas;
  AView: TcxGridCardView; ACard: TcxGridCard; ARow: TcxGridCardViewRow;
  AnItem: TAbstractdxReportCellData; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawCardRowDataCell) then
    FOnCustomDrawCardRowDataCell(Self, ACanvas, AView, ACard, ARow, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawCell(ACanvas: TCanvas;
  AView: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AColumn: TcxGridColumn; AnItem: TAbstractdxReportCellData; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawCell) then
    FOnCustomDrawCell(Self, ACanvas, AView, ARecord, AColumn, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawFilterBar(ACanvas: TCanvas;
  AView: TcxCustomGridTableView; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawFilterBar) then
    FOnCustomDrawFilterBar(Self, ACanvas, AView, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawFooterCell(ACanvas: TCanvas;
  AView: TcxGridTableView; ARecord: TcxCustomGridRecord; AColumn: TcxGridColumn;
  ALevel: Integer; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawFooterCell) then
    FOnCustomDrawFooterCell(Self, ACanvas, AView, ARecord, AColumn, ALevel, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawHeaderCell(ACanvas: TCanvas;
  AView: TcxGridTableView; AColumn: TcxGridColumn; AnItem: TdxReportCellString;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawHeaderCell) then
    FOnCustomDrawHeaderCell(Self, ACanvas, AView, AColumn, AnItem, ADone);
end;

procedure TdxGridReportLink.DoCustomDrawLevelCaption(ACanvas: TCanvas;
  AView: TcxCustomGridView; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawLevelCaption) then
    FOnCustomDrawLevelCaption(Self, ACanvas, AView, AnItem, ADone);
end;

procedure TdxGridReportLink.DoGetCellHeight(AView: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; ATableItem: TcxCustomGridTableItem;
  var AHeight: Integer);
begin
  if Assigned(FOnGetCellHeight) then
    FOnGetCellHeight(Self, AView, ARecord, ATableItem, AHeight);
end;

procedure TdxGridReportLink.DoGetCustomPageBreaks;
begin
  if Assigned(FOnGetCustomPageBreaks) then FOnGetCustomPageBreaks(Self);
end;

procedure TdxGridReportLink.DoInitializeBandCell(AView: TcxGridBandedTableView;
  ABand: TcxGridBand; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeBandCell) then
    FOnInitializeBandCell(Self, AView, ABand, AnItem);
end;

procedure TdxGridReportLink.DoInitializeCardRowCaptionCell(AView: TcxGridCardView;
  ACard: TcxGridCard; ARow: TcxGridCardViewRow; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeCardRowCaptionCell) then
    FOnInitializeCardRowCaptionCell(Self, AView, ACard, ARow, AnItem);
end;

procedure TdxGridReportLink.DoInitializeCardRowDataCell(AView: TcxGridCardView;
  ACard: TcxGridCard; ARow: TcxGridCardViewRow; AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeCardRowDataCell) then
    FOnInitializeCardRowDataCell(Self, AView, ACard, ARow, AnItem);
end;

procedure TdxGridReportLink.DoInitializeCell(AView: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; AColumn: TcxGridColumn; AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeCell) then
    FOnInitializeCell(Self, AView, ARecord, AColumn, AnItem);
end;

procedure TdxGridReportLink.DoInitializeChartCell(AView: TcxGridChartView;
  AnItem: TdxReportCellGraphic);
begin
  if Assigned(FOnInitializeChartCell) then
    FOnInitializeChartCell(Self, AView, AnItem);
end;

procedure TdxGridReportLink.DoInitializeFilterBar(AView: TcxCustomGridTableView;
  AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeFilterBar) then
    FOnInitializeFilterBar(Self, AView, AnItem);
end;

procedure TdxGridReportLink.DoInitializeFooterCell(AView: TcxGridTableView;
  ARecord: TcxCustomGridRecord; AColumn: TcxGridColumn; ALevel: Integer;
  AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeFooterCell) then
    FOnInitializeFooterCell(Self, AView, ARecord, AColumn, ALevel, AnItem);
end;

procedure TdxGridReportLink.DoInitializeHeaderCell(AView: TcxGridTableView;
  AColumn: TcxGridColumn; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeHeaderCell) then
    FOnInitializeHeaderCell(Self, AView, AColumn, AnItem);
end;

procedure TdxGridReportLink.DoInitializeLevelCaption(AView: TcxCustomGridView;
  AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeLevelCaption) then
    FOnInitializeLevelCaption(Self, AView, AnItem);
end;

procedure TdxGridReportLink.CreateOptions;
begin
  inherited;
  FOptionsCards := GetOptionsCardsClass.Create(Self);
  FOptionsCharts := GetOptionsChartsClass.Create(Self);
  FOptionsDetails := GetOptionsDetailsClass.Create(Self);
  FOptionsLevels := GetOptionsLevelsClass.Create(Self);
end;

procedure TdxGridReportLink.DestroyOptions;
begin
  FreeAndNil(FOptionsLevels);
  FreeAndNil(FOptionsDetails);
  FreeAndNil(FOptionsCharts);
  FreeAndNil(FOptionsCards);
  inherited;
end;

function TdxGridReportLink.GetOptionsCardsClass: TdxGridReportLinkOptionsCardsClass;
begin
  Result := TdxGridReportLinkOptionsCards;
end;

function TdxGridReportLink.GetOptionsChartsClass: TdxGridReportLinkOptionsChartsClass;
begin
  Result := TdxGridReportLinkOptionsCharts;
end;

function TdxGridReportLink.GetOptionsDetailsClass: TdxGridReportLinkOptionsDetailsClass;
begin
  Result := TdxGridReportLinkOptionsDetails;
end;

function TdxGridReportLink.GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass;
begin
  Result := TdxGridReportLinkOptionsExpanding;
end;

function TdxGridReportLink.GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass;
begin
  Result := TdxGridReportLinkOptionsFormatting;
end;

function TdxGridReportLink.GetOptionsLevelsClass: TdxGridReportLinkOptionsLevelsClass;
begin
  Result := TdxGridReportLinkOptionsLevels;
end;

function TdxGridReportLink.GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass;
begin
  Result := TdxGridReportLinkOptionsOnEveryPage;
end;

function TdxGridReportLink.GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass;
begin
  Result := TdxGridReportLinkOptionsPagination;
end;

function TdxGridReportLink.GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass;
begin
  Result := TdxGridReportLinkOptionsPreview;
end;

function TdxGridReportLink.GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass;
begin
  Result := TdxGridReportLinkOptionsRefinements;
end;

function TdxGridReportLink.GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass;
begin
  Result := TdxGridReportLinkOptionsSelection;
end;

function TdxGridReportLink.GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass;
begin
  Result := TdxGridReportLinkOptionsSize;
end;

function TdxGridReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TdxGridReportLinkOptionsView;
end;

procedure TdxGridReportLink.AddReportRow(AReportRow: TdxReportCell);
begin
  FReportRows.Add(AReportRow);
end;

procedure TdxGridReportLink.AddHorizontalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardHorz.Add(Pointer(ADelimiter));
end;

procedure TdxGridReportLink.AddVerticalHardDelimiter(ADelimiter: TdxReportCell);
begin
  AddVerticalHardDelimiter(ADelimiter.AbsoluteRect.Top);
end;

procedure TdxGridReportLink.AddVerticalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardVert.Add(TObject(ADelimiter));
end;

procedure TdxGridReportLink.BuildTopLevelView(AGridView: TcxCustomGridView);
begin
  if (AGridView <> FProcessedView) and (AGridView <> nil) then
    BuildView(nil, AGridView);
end;

procedure TdxGridReportLink.BuildTopLevelViews;
var
  MasterRow: TcxGridMasterDataRow;
  CurrentView: TcxCustomGridView;
begin
  if OptionsLevels.Unwrap and OptionsLevels.UnwrapTopLevel then
  begin
    FProcessedView := nil;
    try
      MasterRow := ActiveViewMasterRow;
      if OptionsLevels.RiseActiveLevelOntoTop then
      begin
        if MasterRow <> nil then
          CurrentView := MasterRow.ActiveDetailGridView
        else
          CurrentView := ActiveView;
        BuildTopLevelView(CurrentView);
        FProcessedView := CurrentView;
      end;
      if not AbortBuilding then
        ForEachView(MasterRow, BuildTopLevelView);
    finally
      FProcessedView := nil;
    end;
  end
  else
    BuildTopLevelView(ActiveView);
end;

procedure TdxGridReportLink.BuildView(AMasterBuilder: TdxCustomGridViewBuilder;
  AGridView: TcxCustomGridView);
begin
  if OptionsLevels.Unwrap and OptionsLevels.SkipEmptyViews and not dxViewHelpersFactory[AGridView].HasData(AGridView) then
    Exit;
  with CreateViewBuilder(AMasterBuilder, AGridView) do
  try
    Build;
  finally
    Free;
  end;
end;

function TdxGridReportLink.CreateViewAdapter(AMasterAdapter: TdxCustomGridViewAdapter;
  AGridView: TcxCustomGridView): TdxCustomGridViewAdapter;
begin
  Result := dxGridViewBuildersFactory.CreateViewAdapter(AMasterAdapter, AGridView);
end;

function TdxGridReportLink.CreateViewBuilder(AMasterBuilder: TdxCustomGridViewBuilder;
  AGridView: TcxCustomGridView): TdxCustomGridViewBuilder;
begin
  Result := dxGridViewBuildersFactory.CreateViewBuilder(Self, AMasterBuilder, AGridView);
end;

procedure TdxGridReportLink.DoExpandRows;
begin
  if OptionsLevels.Unwrap and OptionsLevels.UnwrapTopLevel then
    ForEachView(ActiveViewMasterRow, DoExpandViewRows)
  else
    DoExpandViewRows(ActiveView);
end;

procedure TdxGridReportLink.DoExpandViewRows(AGridView: TcxCustomGridView);
begin
  if AGridView <> nil then
    with CreateViewAdapter(nil, AGridView) do
    try
      ExpandAllRows(OptionsExpanding, not OptionsDetails.OnlyFocusedView);
    finally
      Free;
    end;
end;

procedure TdxGridReportLink.ForEachView(AMasterRow: TcxGridMasterDataRow; AProc: TdxGridViewProc);
var
  I: Integer;
  GridView: TcxCustomGridView;
  Level: TcxGridLevel;
begin
  if not Assigned(AProc) then Exit;

  if AMasterRow <> nil then
    for I := 0 to AMasterRow.DetailGridViewCount -1 do
    begin
      GridView := AMasterRow.DetailGridViews[I];
      if GridView <> nil then
      begin
        Level := GridView.Level as TcxGridLevel;
        if Level.Visible then AProc(GridView);
      end;
      if AbortBuilding then Break;
    end
  else
    with Grid.Levels do
      for I := 0 to VisibleCount - 1 do
      begin
        AProc(VisibleItems[I].GridView);
        if AbortBuilding then Break;
      end;
end;

function TdxGridReportLink.GetItemCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ADrawInfo: TdxGridCellCustomDrawInfo): TdxGridAttributeID;
begin
  dxViewHelpersFactory.HelperClassByItem(AnItem).ExtractCustomDrawInfo(AnItem, ADrawInfo);
  Result := ADrawInfo.GridAttributeID;
end;

function TdxGridReportLink.IsCardViewStyle(AStyle: TcxStyle): Boolean;
begin
  Result := Styles.IsCardViewStyle(AStyle);
end;

function TdxGridReportLink.IsCustomDrawn(AnAttributeID: TdxGridAttributeID): Boolean;
begin
  Result := False;
  case AnAttributeID of
    dxGridBandID:
      Result := Assigned(FOnCustomDrawBandCell);
    dxGridCardRowCaptionID:
      Result := Assigned(FOnCustomDrawCardRowCaptionCell);
    dxGridCardRowDataID:
      Result := Assigned(FOnCustomDrawCardRowDataCell);
    dxGridFilterBarID:
      Result := Assigned(FOnCustomDrawFilterBar);
    dxGridFooterID,
    dxGridGroupFooterID:
      Result := Assigned(FOnCustomDrawFooterCell);
    dxGridHeaderID:
      Result := Assigned(FOnCustomDrawHeaderCell);
    dxGridLevelCaptionID:
      Result := Assigned(FOnCustomDrawLevelCaption);
    dxGridRecordID:
      Result := Assigned(FOnCustomDrawCell);
  end;
end;

function TdxGridReportLink.IsOffice11StyleGrouping: Boolean;
begin
  Result := ActiveViewHelper.IsOffice11StyleGrouping(ActiveView);
end;

procedure TdxGridReportLink.CalculateProcessParams;

  function CalculateHasUnwrapableData(ALevel: TComponent; ARecursive: Boolean): Boolean; overload;
  var
    ViewCount, I: Integer;
    GridView: TcxCustomGridView;
  begin
    ViewCount := 0;
    for I := 0 to TcxGridLevel(ALevel).VisibleCount - 1 do
    begin
      GridView := TcxGridLevel(ALevel).VisibleItems[I].GridView;
      if GridView <> nil then
        Inc(ViewCount);
      if ViewCount > 1 then
      begin
        Result := True;
        Exit;
      end;

      if ARecursive and (GridView <> nil) then
      begin
        Result := CalculateHasUnwrapableData(GridView.Level, ARecursive);
        if Result then
          Exit;
      end;
    end;
    Result := False;
  end;

  function CalculateHasUnwrapableData: Boolean; overload;
  begin
    if OptionsDetails.StartFromFocusedView and (Grid.FocusedView <> nil) then
      Result := CalculateHasUnwrapableData(TcxGridLevel(Grid.FocusedView.Level).Parent, not OptionsDetails.OnlyFocusedView)
    else
      Result := CalculateHasUnwrapableData(Grid.Levels, True);
  end;

var
  I: Integer;
  Adapter: TdxCustomGridViewAdapter;
  View: TcxCustomGridView;
begin
  View := ActiveView;

  with FProcessParams do
    if View = nil then
    begin
      HasMasterDetails := False;
      HasMasterDetailsInTopView := False;
      HasOnlyCards := False;
      HasOnlyCharts := False;
      HasUnwrapableData := False;
      CanUseOnEveryPageMode := not IsAggregated;
    end
    else
    begin
      Adapter := CreateViewAdapter(nil, View);
      try
        HasUnwrapableData := CalculateHasUnwrapableData;//Grid.ActiveLevel.GridView);
        HasMasterDetailsInTopView := View.IsDetail or View.IsMaster;
        HasMasterDetails := (HasUnwrapableData and OptionsLevels.Unwrap) or
          (View.IsMaster and not Adapter.AreAllMasterRowsCollapsed and
          not (OptionsDetails.StartFromFocusedView and OptionsDetails.OnlyFocusedView));
        HasCardsInDetail := False;
        for I := 0 to Grid.ViewCount - 1 do
          if Grid.Views[I].IsDetail and (Grid.Views[I] is TcxGridCustomLayoutView) then
            HasCardsInDetail := True;
        HasOnlyCards := View is TcxGridCustomLayoutView;
        HasOnlyCharts := View is TcxGridChartView;
        CanUseOnEveryPageMode := not IsAggregated and not HasMasterDetails and
          Adapter.CanUseOnEveryPageMode; {Adapter.RecordCount <> 0}
      finally
        Adapter.Free;
      end;
    end;
end;

function TdxGridReportLink.CanAttributeBeUsedOnEveryPage(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  Result := ProcessParams.CanUseOnEveryPageMode and OptionsOnEveryPage.HasInstalledAttribute(AnAttribute);
end;

function TdxGridReportLink.IsAttributeUsedOnEveryPage(AnAttribute: TdxGridAttributeClass): Boolean;
begin
  Result := CanAttributeBeUsedOnEveryPage(AnAttribute) or not OptionsView.HasInstalledAttribute(AnAttribute);
end;

function TdxGridReportLink.GetActiveStyles: TdxGridReportLinkStyles;
begin
  Result := inherited ActiveStyles as TdxGridReportLinkStyles;
end;

function TdxGridReportLink.GetActiveView: TcxCustomGridView;
var
  Level: TcxGridLevel;
begin
  Result := nil;
  if Grid <> nil then
    Result := Grid.FocusedView;
  if (Result <> nil) and not OptionsDetails.StartFromFocusedView then
  begin
    Level := Result.Level as TcxGridLevel;
    while not Level.IsTop do
      Level := Level.Parent;
    Result := Level.GridView;
  end;
end;

function TdxGridReportLink.GetActiveViewHelper: TdxCustomGridViewHelperClass;
begin
  if ActiveView = nil then
    Result := TdxNullGridViewHelper
  else
    Result := dxViewHelpersFactory[ActiveView];
end;

function TdxGridReportLink.GetActiveViewMasterRow: TcxGridMasterDataRow;
begin
  Result := nil;
  with ActiveView do
    if MasterGridRecordIndex <> -1 then
      Result := (MasterGridView as TcxGridTableView).ViewData.Rows[MasterGridRecordIndex] as TcxGridMasterDataRow;
end;

function TdxGridReportLink.GetActiveViewParentLevel: TcxGridLevel;
var
  GridView: TcxCustomGridView;
begin
  Result := nil;
  GridView := ActiveView;
  if GridView <> nil then
    Result := TcxGridLevel(GridView.Level).Parent;
end;

function TdxGridReportLink.GetDesignWindow: TdxfmGridReportLinkDesignWindow;
begin
  Result := inherited DesignWindow as TdxfmGridReportLinkDesignWindow;
end;

function TdxGridReportLink.GetGrid: TcxGrid;
begin
  Result := TcxGrid(Component);
end;

function TdxGridReportLink.GetOptionsExpanding: TdxGridReportLinkOptionsExpanding;
begin
  Result := inherited OptionsExpanding as TdxGridReportLinkOptionsExpanding;
end;

function TdxGridReportLink.GetOptionsFormatting: TdxGridReportLinkOptionsFormatting;
begin
  Result := inherited OptionsFormatting as TdxGridReportLinkOptionsFormatting;
end;

function TdxGridReportLink.GetOptionsOnEveryPage: TdxGridReportLinkOptionsOnEveryPage;
begin
  Result := inherited OptionsOnEveryPage as TdxGridReportLinkOptionsOnEveryPage;
end;

function TdxGridReportLink.GetOptionsPagination: TdxGridReportLinkOptionsPagination;
begin
  Result := inherited OptionsPagination as TdxGridReportLinkOptionsPagination;
end;

function TdxGridReportLink.GetOptionsPreview: TdxGridReportLinkOptionsPreview;
begin
  Result := inherited OptionsPreview as TdxGridReportLinkOptionsPreview;
end;

function TdxGridReportLink.GetOptionsRefinements: TdxGridReportLinkOptionsRefinements;
begin
  Result := inherited OptionsRefinements as TdxGridReportLinkOptionsRefinements;
end;

function TdxGridReportLink.GetOptionsSelection: TdxGridReportLinkOptionsSelection;
begin
  Result := inherited OptionsSelection as TdxGridReportLinkOptionsSelection;
end;

function TdxGridReportLink.GetOptionsSize: TdxGridReportLinkOptionsSize;
begin
  Result := inherited OptionsSize as TdxGridReportLinkOptionsSize;
end;

function TdxGridReportLink.GetOptionsView: TdxGridReportLinkOptionsView;
begin
  Result := inherited OptionsView as TdxGridReportLinkOptionsView;
end;

function TdxGridReportLink.GetReportDataCellByGridColumn(AColumn: TcxGridColumn): TAbstractdxReportCellData;
begin
  if (ReportRowCount = 0) or not ReportRows[0].FindDataItemByData(TdxNativeInt(AColumn), Result) then
    Result := nil;
end;

function TdxGridReportLink.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FReportRows[Index]);
end;

function TdxGridReportLink.GetReportRowByGridRecord(GridRecord: TcxCustomGridRecord): TdxReportCell;
var
  I: Integer;
begin
  if GridRecord <> nil then
    for I := 0 to ReportRowCount - 1 do
    begin
      Result := ReportRows[I];
      if Result.Data = TdxNativeInt(GridRecord) then
        Exit;
    end;
  Result := nil;
end;

function TdxGridReportLink.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TdxGridReportLink.GetStyles: TdxGridReportLinkStyles;
begin
  Result := inherited Styles as TdxGridReportLinkStyles;
end;

procedure TdxGridReportLink.SetOnCustomDrawBandCell(Value: TdxGridReportLinkCustomDrawBandCellEvent);
begin
  if @FOnCustomDrawBandCell <> @Value then
  begin
    FOnCustomDrawBandCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawCell(Value: TdxGridReportLinkCustomDrawCellEvent);
begin
  if @FOnCustomDrawCell <> @Value then
  begin
    FOnCustomDrawCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawCardRowCaptionCell(Value: TdxGridReportLinkCustomDrawCardRowCaptionCellEvent);
begin
  if @FOnCustomDrawCardRowCaptionCell <> @Value then
  begin
    FOnCustomDrawCardRowCaptionCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawCardRowDataCell(Value: TdxGridReportLinkCustomDrawCardRowDataCellEvent);
begin
  if @FOnCustomDrawCardRowDataCell <> @Value then
  begin
    FOnCustomDrawCardRowDataCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawFilterBar(Value: TdxGridReportLinkCustomDrawFilterBarEvent);
begin
  if @FOnCustomDrawFilterBar <> @Value then
  begin
    FOnCustomDrawFilterBar := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawFooterCell(Value: TdxGridReportLinkCustomDrawFooterCellEvent);
begin
  if @FOnCustomDrawFooterCell <> @Value then
  begin
    FOnCustomDrawFooterCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawHeaderCell(Value: TdxGridReportLinkCustomDrawHeaderCellEvent);
begin
  if @FOnCustomDrawHeaderCell <> @Value then
  begin
    FOnCustomDrawHeaderCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOnCustomDrawLevelCaption(Value: TdxGridReportLinkCustomDrawLevelCaptionEvent);
begin
  if @FOnCustomDrawLevelCaption <> @Value then
  begin
    FOnCustomDrawLevelCaption := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxGridReportLink.SetOptionsCards(Value: TdxGridReportLinkOptionsCards);
begin
  OptionsCards.Assign(Value);
end;

procedure TdxGridReportLink.SetOptionsCharts(Value: TdxGridReportLinkOptionsCharts);
begin
  OptionsCharts.Assign(Value);
end;

procedure TdxGridReportLink.SetOptionsDetails(Value: TdxGridReportLinkOptionsDetails);
begin
  OptionsDetails.Assign(Value);
end;

procedure TdxGridReportLink.SetOptionsExpanding(Value: TdxGridReportLinkOptionsExpanding);
begin
  OptionsExpanding.Assign(Value);
end;

procedure TdxGridReportLink.SetOptionsFormatting(Value: TdxGridReportLinkOptionsFormatting);
begin
  inherited OptionsFormatting := Value;
end;

procedure TdxGridReportLink.SetOptionsLevels(Value: TdxGridReportLinkOptionsLevels);
begin
  OptionsLevels.Assign(Value);
end;

procedure TdxGridReportLink.SetOptionsOnEveryPage(Value: TdxGridReportLinkOptionsOnEveryPage);
begin
  inherited OptionsOnEveryPage := Value;
end;

procedure TdxGridReportLink.SetOptionsPagination(Value: TdxGridReportLinkOptionsPagination);
begin
  inherited OptionsPagination := Value;
end;

procedure TdxGridReportLink.SetOptionsPreview(Value: TdxGridReportLinkOptionsPreview);
begin
  inherited OptionsPreview := Value;
end;

procedure TdxGridReportLink.SetOptionsRefinements(Value: TdxGridReportLinkOptionsRefinements);
begin
  inherited OptionsRefinements := Value;
end;

procedure TdxGridReportLink.SetOptionsSelection(Value: TdxGridReportLinkOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TdxGridReportLink.SetOptionsSize(Value: TdxGridReportLinkOptionsSize);
begin
  inherited OptionsSize := Value;
end;

procedure TdxGridReportLink.SetOptionsView(Value: TdxGridReportLinkOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TdxGridReportLink.SetStyles(Value: TdxGridReportLinkStyles);
begin
  inherited Styles := Value;
end;

{ TcxGridCardView2OptionsView }

constructor TcxGridCardView2OptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCardBorders := False;
  FGridLineColor := clDefault;
  FGridLines := glNone;
  FShadowDepth := 4;
end;

procedure TcxGridCardView2OptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardView2OptionsView then
    with TcxGridCardView2OptionsView(Source) do
    begin
      Self.CardBorders := CardBorders;
      Self.GridLineColor := GridLineColor;
      Self.GridLines := GridLines;
      Self.ShadowDepth := ShadowDepth;
    end;
  inherited;
end;

function TcxGridCardView2OptionsView.GetShadowVisible: Boolean;
begin
  Result := ShadowDepth <> 0;
end;

procedure TcxGridCardView2OptionsView.SetCardBorders(Value: Boolean);
begin
  if FCardBorders <> Value then
  begin
    FCardBorders := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardView2OptionsView.SetGridLineColor(Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridCardView2OptionsView.SetGridLines(Value: TcxGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardView2OptionsView.SetShadowDepth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    Changed(vcSize);
  end;
end;

{ TcxGridCardView2Styles }

procedure TcxGridCardView2Styles.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardView2Styles then
    with TcxGridCardView2Styles(Source) do
    begin
      Self.CardShadow := CardShadow;
    end;
  inherited;
end;

procedure TcxGridCardView2Styles.GetCardShadowParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetCardShadowStyle) then
    FOnGetCardShadowStyle(GridView, ARecord, AStyle);
  GetViewParams(vsCardShadow, ARecord, AStyle, AParams);
end;

procedure TcxGridCardView2Styles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  inherited;
  if Index = vsCardShadow then AParams.Color := clWindowText;
end;

type
  TcxGridCardViewInfo2 = class;
  TcxGridCardViewViewInfo2 = class;

  TcxGridCardRowCaptionViewInfo2 = class(TcxGridCardRowCaptionViewInfo)
  private
    function GetCardViewInfo: TcxGridCardViewInfo2;
  protected
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
  public
    property CardViewInfo: TcxGridCardViewInfo2 read GetCardViewInfo;
  end;

  TcxGridCardRowDataViewInfo2 = class(TcxGridCardRowDataViewInfo)
  private
    function GetCardViewInfo: TcxGridCardViewInfo2;
  protected
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
  public
    property CardViewInfo: TcxGridCardViewInfo2 read GetCardViewInfo;
  end;

  TcxGridCardDataRowViewInfo2 = class(TcxGridCardDataRowViewInfo)
  protected
    class function GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass; override;
    class function GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass; override;
  end;

  TcxGridCardCaptionRowViewInfo2 = class(TcxGridCardRowViewInfo)
  protected
    class function GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass; override;
    class function GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass; override;
  end;

  TcxGridCardPainter2 = class(TcxGridCardPainter)
  private
    function GetViewInfo: TcxGridCardViewInfo2;
  protected
    procedure DrawBottomShadowAndRestSpace; virtual;
    procedure DrawCardBorder; override;
    procedure DrawRightShadowAndRestSpace; virtual;
    procedure DrawShadows; virtual;
    procedure Paint; override;
    property ViewInfo: TcxGridCardViewInfo2 read GetViewInfo;
  end;

  TcxGridCardViewInfo2 = class(TcxGridCardViewInfo)
  private
    function GetGridView: TcxGridCardView2;
  protected
    function CalculateHeight: Integer; override;
    function GetContentWidth: Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    class function GetRowViewInfoClass(ARow: TcxGridCardViewRow): TcxGridCardRowViewInfoClass; override;

    function GetBottomShadowRect: TRect; virtual;
    function GetBottomShadowRestRect: TRect; virtual;
    function GetCardRowCaptionBorders(ARowCaption: TcxGridCardRowCaptionViewInfo2): TcxBorders; virtual;
    function GetCardRowDataBorders(ARowData: TcxGridCardRowDataViewInfo2): TcxBorders; virtual;
    function GetGridLineColor: TColor; virtual;
    function GetRightShadowRect: TRect; virtual;
    function GetRightShadowRestRect: TRect; virtual;
    function GetShadowColor: TColor; virtual;
    function GetShadowDepth: Integer; virtual;
    function GetShadowRestColor: TColor; virtual;

    property GridView: TcxGridCardView2 read GetGridView;
  public
    property BottomShadowRect: TRect read GetBottomShadowRect;
    property BottomShadowRestRect: TRect read GetBottomShadowRestRect;
    property GridLineColor: TColor read GetGridLineColor;
    property RightShadowRect: TRect read GetRightShadowRect;
    property RightShadowRestRect: TRect read GetRightShadowRestRect;
    property ShadowColor: TColor read GetShadowColor;
    property ShadowDepth: Integer read GetShadowDepth;
    property ShadowRestColor: TColor read GetShadowRestColor;
  end;

  TcxGridCard2 = class(TcxGridCard)
  protected
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
  end;

  TcxGridCardViewViewData2 = class(TcxGridCardViewViewData)
  protected
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
  end;

  TcxGridCardsViewInfo2 = class(TcxGridCardsViewInfo)
  private
    function GetGridView: TcxGridCardView2;
  protected
    function CalculateRecordHeight: Integer; override;
    function GetShadowDepth: Integer; virtual;

    property GridView: TcxGridCardView2 read GetGridView;
  public
    property ShadowDepth: Integer read GetShadowDepth;
  end;

  TcxGridCardViewViewInfo2 = class(TcxGridCardViewViewInfo)
  protected
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
  end;

{ TcxGridCardRowCaptionViewInfo2 }

function TcxGridCardRowCaptionViewInfo2.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := CardViewInfo.GridLineColor;
end;

function TcxGridCardRowCaptionViewInfo2.GetBorders: TcxBorders;
begin
  Result := CardViewInfo.GetCardRowCaptionBorders(Self);
end;

function TcxGridCardRowCaptionViewInfo2.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridCardRowCaptionViewInfo2.GetCardViewInfo: TcxGridCardViewInfo2;
begin
  Result := inherited CardViewInfo as TcxGridCardViewInfo2
end;

{ TcxGridCardRowDataViewInfo2 }

function TcxGridCardRowDataViewInfo2.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := CardViewInfo.GridLineColor;
end;

function TcxGridCardRowDataViewInfo2.GetBorders: TcxBorders;
begin
  Result := CardViewInfo.GetCardRowDataBorders(Self);
end;

function TcxGridCardRowDataViewInfo2.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridCardRowDataViewInfo2.GetCardViewInfo: TcxGridCardViewInfo2;
begin
  Result := inherited CardViewInfo as TcxGridCardViewInfo2
end;

{ TcxGridCardDataRowViewInfo2 }

class function TcxGridCardDataRowViewInfo2.GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass;
begin
  Result := TcxGridCardRowCaptionViewInfo2;
end;

class function TcxGridCardDataRowViewInfo2.GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass;
begin
  Result := TcxGridCardRowDataViewInfo2;
end;

{ TcxGridCardCaptionRowViewInfo2 }

class function TcxGridCardCaptionRowViewInfo2.GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass;
begin
  Result := TcxGridCardRowCaptionViewInfo2;
end;

class function TcxGridCardCaptionRowViewInfo2.GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass;
begin
  Result := TcxGridCardRowDataViewInfo2;
end;

{ TcxGridCardPainter2 }

procedure TcxGridCardPainter2.DrawBottomShadowAndRestSpace;
var
  R: TRect;
begin
  R := ViewInfo.BottomShadowRect;
  if Canvas.RectVisible(R) then
  begin
    Canvas.Brush.Color := ViewInfo.ShadowColor;
    Canvas.FillRect(R);
  end;

  R := ViewInfo.BottomShadowRestRect;
  if Canvas.RectVisible(R) then
  begin
    Canvas.Brush.Color := ViewInfo.ShadowRestColor;
    Canvas.FillRect(R);
  end;
end;

procedure TcxGridCardPainter2.DrawCardBorder;
begin
  with ViewInfo, Self.Canvas do
    FrameRect(Bounds, clWindow, CardBorderWidth);
end;

procedure TcxGridCardPainter2.DrawRightShadowAndRestSpace;
var
  R: TRect;
begin
  R := ViewInfo.RightShadowRect;
  if Canvas.RectVisible(R) then
  begin
    Canvas.Brush.Color := ViewInfo.ShadowColor;
    Canvas.FillRect(R);
  end;

  R := ViewInfo.RightShadowRestRect;
  if Canvas.RectVisible(R) then
  begin
    Canvas.Brush.Color := ViewInfo.ShadowRestColor;
    Canvas.FillRect(R);
  end;
end;

procedure TcxGridCardPainter2.DrawShadows;
var
  Color: TColor;
begin
  Color := Canvas.Brush.Color;
  DrawBottomShadowAndRestSpace;
  DrawRightShadowAndRestSpace;
  Canvas.Brush.Color := Color;
end;

procedure TcxGridCardPainter2.Paint;
begin
  inherited;
  DrawShadows;
end;

function TcxGridCardPainter2.GetViewInfo: TcxGridCardViewInfo2;
begin
  Result := TcxGridCardViewInfo2(inherited ViewInfo);
end;

{ TcxGridCardViewInfo2 }

function TcxGridCardViewInfo2.CalculateHeight: Integer;
begin
  Result := inherited CalculateHeight;
  if RecordsViewInfo.AutoDataRecordHeight or (RecordsViewInfo.RecordHeight = -1) then
    Inc(Result, ShadowDepth);
end;

function TcxGridCardViewInfo2.GetContentWidth: Integer;
begin
  Result := inherited GetContentWidth - ShadowDepth;
end;

function TcxGridCardViewInfo2.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardPainter2;
end;

class function TcxGridCardViewInfo2.GetRowViewInfoClass(ARow: TcxGridCardViewRow): TcxGridCardRowViewInfoClass;
const
  RowViewInfoClasses: array[TcxGridCardViewRowKind] of TcxGridCardRowViewInfoClass =
    (TcxGridCardDataRowViewInfo2, TcxGridCardCaptionRowViewInfo2, TcxGridCardCaptionRowViewInfo2);
begin
  Result := RowViewInfoClasses[ARow.Kind];
end;

function TcxGridCardViewInfo2.GetBottomShadowRect: TRect;
begin
  Result := ContentBounds;
  Inc(Result.Left, ShadowDepth);
  Result.Top := Result.Bottom - ShadowDepth;
end;

function TcxGridCardViewInfo2.GetBottomShadowRestRect: TRect;
begin
  Result := ContentBounds;
  Result.Right := Result.Left + ShadowDepth;
  Result.Top := Result.Bottom - ShadowDepth;
end;

function TcxGridCardViewInfo2.GetCardRowCaptionBorders(ARowCaption: TcxGridCardRowCaptionViewInfo2): TcxBorders;
begin
  Result := [bLeft, bTop];
  if ARowCaption.Row.VisibleIndex = GridView.RowCount - 1 then
    Include(Result, bBottom);

  with TcxGridCardView2OptionsView(GridView.OptionsView) do
  begin
    if not CardBorders then
    begin
      Exclude(Result, bLeft);
      if ARowCaption.Row.VisibleIndex = 0 then
        Exclude(Result, bTop);
      if ARowCaption.Row.VisibleIndex = Self.GridView.RowCount - 1 then
        Exclude(Result, bBottom);
    end;
    if not (GridLines in [glBoth, glHorizontal]) and (ARowCaption.Row.VisibleIndex > 0) then
      Exclude(Result, bTop);
  end;
end;

function TcxGridCardViewInfo2.GetCardRowDataBorders(ARowData: TcxGridCardRowDataViewInfo2): TcxBorders;
begin
  Result := [bLeft, bTop, bRight];
  if ARowData.Row.VisibleIndex = GridView.RowCount - 1 then
    Include(Result, bBottom);
  with TcxGridCardView2OptionsView(GridView.OptionsView) do
  begin
    if not CardBorders then
    begin
      Exclude(Result, bRight);
      if ARowData.Row.VisibleIndex = 0 then
        Exclude(Result, bTop);
      if ARowData.Row.VisibleIndex = Self.GridView.RowCount - 1 then
        Exclude(Result, bBottom);
    end;
    if not (GridLines in [glBoth, glHorizontal]) and (ARowData.Row.VisibleIndex > 0) then
      Exclude(Result, bTop);
    if not (GridLines in [glBoth, glVertical]) then
      Exclude(Result, bLeft);
  end;
end;

function TcxGridCardViewInfo2.GetGridLineColor: TColor;
begin
  Result := TcxGridCardView2OptionsView(GridView.OptionsView).GridLineColor;
  if Result = clDefault then
    Result := GridView.LookAndFeelPainter.DefaultGridLineColor;
end;

function TcxGridCardViewInfo2.GetRightShadowRect: TRect;
begin
  Result := ContentBounds;
  Result.Left := Result.Right - ShadowDepth;
  Inc(Result.Top, ShadowDepth);
end;

function TcxGridCardViewInfo2.GetRightShadowRestRect: TRect;
begin
  Result := ContentBounds;
  Result.Left := Result.Right - ShadowDepth;
  Result.Bottom := Result.Top + ShadowDepth;
end;

function TcxGridCardViewInfo2.GetShadowColor: TColor;
var
  Params: TcxViewParams;
begin
  TcxGridCardView2Styles(GridView.Styles).GetCardShadowParams(GridRecord, Params);
  Result := Params.Color;
  if Result = clDefault then
    Result := clWindowText;
end;

function TcxGridCardViewInfo2.GetShadowDepth: Integer;
begin
  Result := TcxGridCardView2OptionsView(GridView.OptionsView).ShadowDepth;
end;

function TcxGridCardViewInfo2.GetShadowRestColor: TColor;
var
  Params: TcxViewParams;
begin
  TcxGridCardView2Styles(GridView.Styles).GetViewParams(vsBackground, nil, nil, Params);
  Result := Params.Color;
end;

function TcxGridCardViewInfo2.GetGridView: TcxGridCardView2;
begin
  Result := TcxGridCardView2(inherited GridView);
end;

{ TcxGridCard2 }

function TcxGridCard2.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridCardViewInfo2;
end;

{ TcxGridCardViewViewData2 }

function TcxGridCardViewViewData2.GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridCard2;
end;

{ TcxGridCardsViewInfo2 }

function TcxGridCardsViewInfo2.CalculateRecordHeight: Integer;
begin
  Result := inherited CalculateRecordHeight + ShadowDepth;
end;

function TcxGridCardsViewInfo2.GetShadowDepth: Integer;
begin
  Result := TcxGridCardView2OptionsView(GridView.OptionsView).ShadowDepth;
end;

function TcxGridCardsViewInfo2.GetGridView: TcxGridCardView2;
begin
  Result := TcxGridCardView2(inherited GridView);
end;

{ TcxGridCardViewViewInfo2 }

function TcxGridCardViewViewInfo2.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridCardsViewInfo2;
end;

{ TcxGridCardView2 }

function TcxGridCardView2.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridCardView2OptionsView;
end;

function TcxGridCardView2.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridCardView2Styles;
end;

function TcxGridCardView2.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridCardViewViewData2;
end;

function TcxGridCardView2.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridCardViewViewInfo2;
end;

{ TdxfmGridReportLinkDesignWindow }

constructor TdxfmGridReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhccxGridReportLinkDesigner;
  inherited;
  FGridLinkPreviewStyles := TdxGridReportLinkStyles.Create(nil);
  CreateControls;
  SetActivePage;
end;

destructor TdxfmGridReportLinkDesignWindow.Destroy;
var
  I: Integer;
begin
  dxPSPopupMan.dxPSPopupMenuController.UnregisterControl(lbxStyles);
  for I := 0 to FGridLinkPreviewStyles.Count - 1 do
    FGridLinkPreviewStyles.Values[FGridLinkPreviewStyles.Items[0].Index].Free;
  FreeAndNil(FGridLinkPreviewStyles);
  inherited Destroy;
end;

procedure TdxfmGridReportLinkDesignWindow.DoInitialize;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited;
  RefreshStylesList;

  CreateCardView;
  CreateChartView;
  if ReportLink.ProcessParams.HasOnlyCards then
    pcMain.ItemIndex := tshCards.Index;
  if ReportLink.ProcessParams.HasOnlyCharts then
    pcMain.ItemIndex := tshCharts.Index;

  InitializePreviewGrid;
  InitializePreviewGridStyles;
  LoadDataIntoPreviewGridView(PreviewGrid.Levels[0].GridView as TcxCustomGridTableView);

  with ReportLink.OptionsView do
  begin
    chbxShowCaptions.Checked := Caption;
    chbxShowBands.Checked := BandHeaders;
    chbxShowExpandButtons.Checked := ExpandButtons;
    chbxShowGroupFooters.Checked := GroupFooters;
    chbxShowFilterBar.Checked := FilterBar;
    chbxShowFooters.Checked := Footers;
    chbxShowHeaders.Checked := Headers;
  end;

  with ReportLink.OptionsOnEveryPage do
  begin
    chbxCaptionsOnEveryPage.Checked := Caption;
    chbxBandsOnEveryPage.Checked := BandHeaders;
    chbxFilterBarOnEveryPage.Checked := FilterBar;
    chbxFootersOnEveryPage.Checked := Footers;
    chbxHeadersOnEveryPage.Checked := Headers;
  end;

  with cbxLookAndFeel.Properties do
  begin
    Items.Clear;
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelFlat), TObject(lfFlat));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelStandard), TObject(lfStandard));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelUltraFlat), TObject(lfUltraFlat));

    cbxLookAndFeel.ItemIndex :=
      Items.IndexOfObject(TObject(ReportLink.OptionsFormatting.LookAndFeelKind));
  end;

  with ReportLink.OptionsSelection do
  begin
    chbxProcessSelection.Checked := ProcessSelection;
    chbxProcessExactSelection.Checked := ProcessExactSelection;
  end;

  with ReportLink.OptionsExpanding do
  begin
    chbxExpandGroupRows.Checked := ExpandGroupRows;
    chbxExpandMasterRows.Checked := ExpandMasterRows;
    chbxExpandCards.Checked := ExpandCards;
  end;

  with ReportLink.OptionsRefinements do
  begin
    chbxTransparentGraphics.Checked := TransparentGraphics;
    chbxDisplayGraphicsAsText.Checked := DisplayGraphicsAsText;
    chbxDisplayTrackBarsAsText.Checked := DisplayTrackBarsAsText;
    chbxFlatCheckMarks.Checked := FlatCheckMarks;
    {.3}
    //chbxTransparentRichEdits.Checked := TransparentRichEdits;
  end;

  with ReportLink.OptionsSize do
  begin
    chbxGridAutoWidth.Checked := AutoWidth;
  end;

  with ReportLink.OptionsDetails do
  begin
    chbxStartFromActiveView.Checked := StartFromFocusedView;
    chbxOnlyActiveView.Checked := OnlyFocusedView;
  end;

  with ReportLink.OptionsLevels do
  begin
    chbxLevelsUnwrap.Checked := Unwrap;
    chbxLevelsUnwrapTopLevel.Checked := UnwrapTopLevel;
    chbxLevelsRiseActiveLevelOntoTop.Checked := RiseActiveLevelOntoTop;
    chbxLevelsSkipEmptyViews.Checked := SkipEmptyViews;
  end;

  with ReportLink.OptionsFormatting do
  begin
    chbxSuppressBackgroundBitmaps.Checked := SuppressBackgroundBitmaps;
    chbxConsumeSelectionStyle.Checked := ConsumeSelectionStyle;
    chbxUseNativeStyles.Checked := UseNativeStyles;
  end;

  with ReportLink.OptionsPagination do
  begin
    chbxPaginateByTopLevelGroups.Checked := TopLevelGroup;
    chbxPaginateOneGroupPerPage.Checked := OneGroupPerPage;
  end;

  with ReportLink.OptionsPreview do
  begin
    chbxPreviewVisible.Checked := Visible;
    chbxPreviewAutoHeight.Checked := AutoHeight;
    sePreviewMaxLineCount.Value := MaxLineCount;
  end;

  with ReportLink.OptionsCards do
  begin
    chbxCardsAutoWidth.Checked := AutoWidth;
    chbxCardsKeepSameWidth.Checked := KeepSameWidth;
    chbxCardsKeepSameHeight.Checked := KeepSameHeight;
    chbxCardsBorder.Checked := Borders;
    chbxCardsHorzLines.Checked := RowBordersHorz;
    chbxCardsVertLines.Checked := RowBordersVert;
    seCardsSpaceHorz.Value := InterCardsSpaceHorz;
    seCardsSpaceVert.Value := InterCardsSpaceVert;
    with Shadow do
    begin
      seCardsShadowDepth.Value := Depth;
      ccbxCardsShadowColor.ColorValue := Color;
    end;
  end;

  with ReportLink.OptionsCharts do
  begin
    chbxChartsTransparent.Checked := Transparent;
  end;
end;

function TdxfmGridReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxfmGridReportLinkDesignWindow.LoadStrings;
begin
  inherited;

  lblPreviewWindow.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  tshView.Caption := cxGetResourceString(@sdxViewTab);

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowCaptions.Caption := cxGetResourceString(@sdxLevelCaption);
  chbxShowBands.Caption := cxGetResourceString(@sdxBands);
  chbxShowHeaders.Caption := cxGetResourceString(@sdxHeaders);
  chbxShowFooters.Caption := cxGetResourceString(@sdxFooters);
  chbxShowGroupFooters.Caption := cxGetResourceString(@sdxGroupFooters);
  chbxShowExpandButtons.Caption := cxGetResourceString(@sdxExpandButtons);
  chbxShowFilterBar.Caption := cxGetResourceString(@sdxFilterBar);

  lblOnEveryPage.Caption := cxGetResourceString(@sdxOnEveryPage);
  chbxCaptionsOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxLevelCaption));
  chbxBandsOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxBands));
  chbxHeadersOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxHeaders));
  chbxFootersOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxFooters));
  chbxFilterBarOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxFilterBar));

  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviorsTab);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxProcessSelection.Caption := cxGetResourceString(@sdxProcessSelection);
  chbxProcessExactSelection.Caption := cxGetResourceString(@sdxProcessExactSelection);

  lblExpanding.Caption := cxGetResourceString(@sdxExpanding);
  chbxExpandGroupRows.Caption := cxGetResourceString(@sdxGroups);
  chbxExpandMasterRows.Caption := cxGetResourceString(@sdxDetails);
  chbxExpandCards.Caption := cxGetResourceString(@sdxCardsRows);

  lblGridSize.Caption := cxGetResourceString(@sdxSize);
  chbxGridAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);

  lblDetails.Caption := DropAmpersand(cxGetResourceString(@sdxDetails));
  chbxStartFromActiveView.Caption := cxGetResourceString(@sdxStartFromActiveDetails);
  chbxOnlyActiveView.Caption := cxGetResourceString(@sdxOnlyActiveDetails);

  lblLevels.Caption := cxGetResourceString(@sdxLevels);
  chbxLevelsUnwrap.Caption := cxGetResourceString(@sdxUnwrap);
  chbxLevelsUnwrapTopLevel.Caption := cxGetResourceString(@sdxUnwrapTopLevel);
  chbxLevelsRiseActiveLevelOntoTop.Caption := cxGetResourceString(@sdxRiseActiveToTop);
  chbxLevelsSkipEmptyViews.Caption := cxGetResourceString(@sdxSkipEmptyViews);

  tshFormatting.Caption := cxGetResourceString(@sdxFormatting);
  lblLookAndFeel.Caption := cxGetResourceString(@sdxLookAndFeel);

  lblRefinements.Caption := cxGetResourceString(@sdxRefinements);
  chbxTransparentGraphics.Caption := cxGetResourceString(@sdxTransparentGraphics);
  chbxDisplayGraphicsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayGraphicsAsText));
  chbxDisplayTrackBarsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayTrackBarsAsText));
  chbxFlatCheckMarks.Caption := cxGetResourceString(@sdxFlatCheckMarks);
  //chbxTransparentRichEdits.Caption := cxGetResourceString(@sdxTransparentRichEdits); {.3}

  chbxSuppressBackgroundBitmaps.Caption := cxGetResourceString(@sdxSuppressBackgroundBitmaps);
  chbxConsumeSelectionStyle.Caption := cxGetResourceString(@sdxConsumeSelectionStyle);

  lblPagination.Caption := cxGetResourceString(@sdxPagination);
  chbxPaginateByTopLevelGroups.Caption := cxGetResourceString(@sdxByTopLevelGroups);
  chbxPaginateOneGroupPerPage.Caption := cxGetResourceString(@sdxOneGroupPerPage);

  tshStyles.Caption := cxGetResourceString(@sdxStyles);
  lblUseNativeStyles.Caption := cxGetResourceString(@sdxUseNativeStyles);
  btnStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  btnStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  btnStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  btnStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  btnStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);

  miStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  miStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  miStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  miStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  miStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  miStylesSelectAll.Caption := cxGetResourceString(@sdxSelectAll);
  miStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);

  lblStyleSheets.Caption := cxGetResourceString(@sdxStyleSheets);
  btnStyleSheetNew.Caption := cxGetResourceString(@sdxBtnNew);
  btnStyleSheetCopy.Caption := cxGetResourceString(@sdxBtnCopy);
  btnStyleSheetDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  btnStyleSheetRename.Caption := cxGetResourceString(@sdxBtnRename);

  tshPreview.Caption := cxGetResourceString(@sdxPreviewTab);
  lblPreviewOptions.Caption := cxGetResourceString(@sdxOptions);
  chbxPreviewVisible.Caption := cxGetResourceString(@sdxVisible);
  chbxPreviewAutoHeight.Caption := cxGetResourceString(@sdxPreviewAutoHeight);
  lblPreviewMaxLineCount.Caption := cxGetResourceString(@sdxPreviewMaxLineCount);

  tshCards.Caption := cxGetResourceString(@sdxCardsTab);
  lblCardSizes.Caption := cxGetResourceString(@sdxSizes);
  chbxCardsAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxCardsKeepSameWidth.Caption := cxGetResourceString(@sdxKeepSameWidth);
  chbxCardsKeepSameHeight.Caption := cxGetResourceString(@sdxKeepSameHeight);

  lblCardSpacing.Caption := cxGetResourceString(@sdxSpacing);
  lblCardSpaceHorz.Caption := cxGetResourceString(@sdxHorizontal);
  lblCardSpaceVert.Caption := cxGetResourceString(@sdxVertical);

  lblCardFraming.Caption := cxGetResourceString(@sdxFraming);
  chbxCardsBorder.Caption := cxGetResourceString(@sdxBorderLines);
  chbxCardsHorzLines.Caption := cxGetResourceString(@sdxHorzLines);
  chbxCardsVertLines.Caption := cxGetResourceString(@sdxVertLines);

  lblCardShadow.Caption := cxGetResourceString(@sdxShadow);
  lblCardShadowColor.Caption := cxGetResourceString(@sdxColor);
  lblCardShadowDepth.Caption := cxGetResourceString(@sdxDepth);

  tshCharts.Caption := cxGetResourceString(@sdxCharts);
  lblChartsOptions.Caption := cxGetResourceString(@sdxOptions);

  chbxChartsTransparent.Caption := cxGetResourceString(@sdxTransparent);
end;

procedure TdxfmGridReportLinkDesignWindow.UpdateControlsState;
var
  View: TcxCustomGridView;
begin
  inherited;

  View := ReportLink.ActiveView;
  if View <> nil then
    with ReportLink.ProcessParams, dxViewHelpersFactory[View] do
    begin
      chbxShowBands.Enabled := IsAttributeSupported(TdxGridBandHeader);
      chbxShowExpandButtons.Enabled := IsAttributeSupported(TdxGridExpandButton);
      chbxShowFooters.Enabled := IsAttributeSupported(TdxGridFooter);
      chbxShowGroupFooters.Enabled := IsAttributeSupported(TdxGridGroupFooter);
      chbxShowHeaders.Enabled := IsAttributeSupported(TdxGridHeader);

      chbxCaptionsOnEveryPage.Enabled := IsAttributeSupported(TdxGridLevelCaption) and not ReportLink.IsAggregated;
      chbxBandsOnEveryPage.Enabled := IsAttributeSupported(TdxGridBandHeader) and not ReportLink.IsAggregated;
      chbxHeadersOnEveryPage.Enabled := IsAttributeSupported(TdxGridHeader) and not ReportLink.IsAggregated;
      chbxFilterBarOnEveryPage.Enabled := IsAttributeSupported(TdxGridFilterBar) and not ReportLink.IsAggregated;
      chbxFootersOnEveryPage.Enabled := IsAttributeSupported(TdxGridFooter) and not ReportLink.IsAggregated;

      chbxExpandGroupRows.Enabled := IsAttributeSupported(TdxGridGroupRow);
      chbxExpandMasterRows.Enabled := IsAttributeSupported(TdxGridDetails);

      lgDetails.Visible := IsDesigning or HasMasterDetailsInTopView or HasUnwrapableData;
      chbxOnlyActiveView.Enabled := chbxStartFromActiveView.Checked;

      lgLevels.Visible := IsDesigning or HasUnwrapableData;

      chbxLevelsUnwrapTopLevel.Enabled := chbxLevelsUnwrap.Checked;
      chbxLevelsRiseActiveLevelOntoTop.Enabled := chbxLevelsUnwrap.Checked;
      chbxLevelsSkipEmptyViews.Enabled := chbxLevelsUnwrap.Checked;

      chbxPreviewVisible.Enabled := IsAttributeSupported(TdxGridPreview);
      chbxPreviewAutoHeight.Enabled := IsAttributeSupported(TdxGridPreview);
      sePreviewMaxLineCount.Enabled := IsAttributeSupported(TdxGridPreview);

      lgPagination.Visible := IsDesigning or IsAttributeSupported(TdxGridGroupRow);
    end;

  chbxProcessExactSelection.Enabled := chbxProcessSelection.Checked;

  chbxPaginateOneGroupPerPage.Enabled := chbxPaginateByTopLevelGroups.Checked;

  chbxCardsKeepSameWidth.Enabled := chbxCardsAutoWidth.Checked;

  chbxChartsTransparent.Checked := ReportLink.OptionsCharts.Transparent;

  lbxStyles.Enabled := AreNativeStylesAvailable;
  btnStyleColor.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleFont.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleBackgroundBitmap.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleBackgroundBitmapClear.Enabled := AreNativeStylesAvailable and HasSelectedStylesWithAssignedBitmap;
  btnStyleRestoreDefaults.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStylesSaveAs.Enabled := CanSaveStyles;

  lblStyleSheets.Enabled := AreNativeStylesAvailable;
  cbxStyleSheets.Enabled := AreNativeStylesAvailable;
  btnStyleSheetNew.Enabled := CanCreateStyleSheet;
  btnStyleSheetCopy.Enabled := CanCopyStyleSheet;
  btnStyleSheetDelete.Enabled := CanDeleteStyleSheet;
  btnStyleSheetRename.Enabled := CanRenameStyleSheet;

  UpdateWarningPane;
end;

procedure TdxfmGridReportLinkDesignWindow.UpdatePreview;
const
  DetailTabsPositionMap: array[Boolean] of TcxGridDetailTabsPosition = (dtpNone, dtpTop);
  GroupFootersMap: array[Boolean] of TcxGridGroupFootersMode = (gfInvisible, gfAlwaysVisible);
  FilteringMap: array[Boolean] of TcxGridFilterVisible =
    (fvNonEmpty, fvAlways);
  TransparentsMap: array[Boolean] of TcxImageTransparency = (gtOpaque, gtTransparent);

  function GetFilterBarPosition: TcxGridFilterPosition;
  var
    View: TcxCustomGridView;
  begin
    View := ReportLink.ActiveView;
    if View is TcxCustomGridTableView then
      Result := CustomGridTableFiltering_GetPosition(TcxCustomGridTableView(View).Filtering)
    else
      Result := fpBottom;
  end;

  procedure UpdateBandedView;
  begin
    TcxImageProperties(colVendorLogo.Properties).GraphicTransparency :=
      TransparentsMap[ReportLink.OptionsRefinements.TransparentGraphics];
    with PreviewBandedView do
    begin
      Filtering.Position := GetFilterBarPosition;
      Filtering.Visible := FilteringMap[ReportLink.OptionsView.FilterBar];
      OptionsView.BandHeaders := ReportLink.OptionsView.BandHeaders;
      OptionsView.Footer := ReportLink.OptionsView.Footers;
      OptionsView.GroupFooters := GroupFootersMap[ReportLink.OptionsView.GroupFooters];
      OptionsView.Header := ReportLink.OptionsView.Headers;
      Preview.Visible := ReportLink.OptionsPreview.Visible;
      ViewChanged;
    end;
  end;

  procedure UpdateCardView;
  begin
    TcxImageProperties(rowVendorLogo.Properties).GraphicTransparency :=
      TransparentsMap[ReportLink.OptionsRefinements.TransparentGraphics];
    PreviewCardView.Filtering.Position := GetFilterBarPosition;
    PreviewCardView.Filtering.Visible := FilteringMap[ReportLink.OptionsView.FilterBar];
    PreviewCardView.ViewChanged;
  end;

var
  I: Integer;
begin
  PreviewGrid.Enabled := False;

  dxSetupPreviewControlLookAndFeel(PreviewGrid.LookAndFeel, ReportLink.OptionsFormatting.LookAndFeelKind, ReportLink.Grid);
  dxAssignFont(PreviewGrid.Font, ReportLink.Font, ScaleFactor, ReportLink.ScaleFactor);

  PreviewGrid.RootLevelOptions.DetailTabsPosition := DetailTabsPositionMap[ReportLink.OptionsView.Caption];

  for I := 0 to FGridLinkPreviewStyles.Count - 1 do
    FGridLinkPreviewStyles.Values[FGridLinkPreviewStyles.Items[I].Index].Font.Height :=
      ScaleFactor.Apply(ReportLink.ActiveStyles.Values[ReportLink.ActiveStyles.Items[I].Index].Font.Height, ReportLink.ScaleFactor);

  UpdateBandedView;
  UpdateCardView;
end;

function TdxfmGridReportLinkDesignWindow.GetDesignerTabIndex: Integer;
begin
  Result := pcMain.ItemIndex;
end;

procedure TdxfmGridReportLinkDesignWindow.SetDesignerTabIndex(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > pcMain.Count - 1 then
    Value := pcMain.Count - 1;
  pcMain.ItemIndex := Value;
end;

procedure TdxfmGridReportLinkDesignWindow.DoActiveStyleSheetChanged;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited;
  cbxStyleSheets.ItemIndex := cbxStyleSheets.Properties.Items.IndexOfObject(ActiveStyleSheet);
  if not LockControlsUpdate then
  begin
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.DoFormActivated(AnActive: Boolean);
begin
  inherited;
  if not AnActive then lbxStyles.HideToolTips;
end;

procedure TdxfmGridReportLinkDesignWindow.DoRefreshStylesList;
var
  Styles: TdxGridReportLinkStyles;
  List: TList;
begin
  Styles := ReportLink.ActiveStyles;

  with lbxStyles.Items do
  begin
    BeginUpdate;
    try
      List := TList.Create;
      try
        SaveSelectedStyles(List);
        try
          Clear;
          AddObject(cxGetResourceString(@sdxBandHeaderStyle), Styles.BandHeader);
          AddObject(cxGetResourceString(@sdxCaptionStyle), Styles.Caption);
          AddObject(cxGetResourceString(@sdxCardCaptionRowStyle), Styles.CardCaptionRow);
          AddObject(cxGetResourceString(@sdxCardRowCaptionStyle), Styles.CardRowCaption);
          AddObject(cxGetResourceString(@sdxContentStyle), Styles.Content);
          AddObject(cxGetResourceString(@sdxContentEvenStyle), Styles.ContentEven);
          AddObject(cxGetResourceString(@sdxContentOddStyle), Styles.ContentOdd);
          AddObject(cxGetResourceString(@sdxFilterBarStyle), Styles.FilterBar);
          AddObject(cxGetResourceString(@sdxFooterStyle), Styles.Footer);
          AddObject(cxGetResourceString(@sdxGroupStyle), Styles.Group);
          AddObject(cxGetResourceString(@sdxHeaderStyle), Styles.Header);
          AddObject(cxGetResourceString(@sdxPreviewStyle), Styles.Preview);
          AddObject(cxGetResourceString(@sdxSelectionStyle), Styles.Selection);
        finally
          RestoreSelectedStyles(List);
        end;
      finally
        List.Free;
      end;
    finally
      EndUpdate;
    end;
  end;
  InitializePreviewGridStyles;
end;

procedure TdxfmGridReportLinkDesignWindow.DoStyleChanged(const ACaption: string; AStyle: TcxStyle);
var
  AIndex: Integer;
begin
  inherited;
  Modified := True;
  AIndex := FGridLinkPreviewStyles.GetStyleIndexByCaption(ACaption);
  if (AStyle <> nil) and (FGridLinkPreviewStyles.Values[AIndex] = nil) then
    FGridLinkPreviewStyles.Values[AIndex] := TcxStyle.Create(nil)
  else
    if AStyle = nil then
      FGridLinkPreviewStyles.Values[AIndex].Free;

  if FGridLinkPreviewStyles.Values[AIndex] <> nil then
    FGridLinkPreviewStyles.Values[AIndex].Assign(AStyle);

  UpdatePreviewGridStyles(ACaption, FGridLinkPreviewStyles.Values[AIndex]);
end;

procedure TdxfmGridReportLinkDesignWindow.DoStylesChanged(AStrings: TStrings; ARecreate: Boolean);
begin
  if ARecreate then
    RecreateStylesListBox
  else
    lbxStyles.Invalidate;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.GetSelectedStyleNames(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.Clear;
  with lbxStyles do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
        AStrings.AddObject(Items[I], Items.Objects[I]);
end;

procedure TdxfmGridReportLinkDesignWindow.GetStyleNames(out AStrings: TStrings);
begin
  AStrings := lbxStyles.Items;
end;

procedure TdxfmGridReportLinkDesignWindow.GetStyleSheetNames(out AStrings: TStrings);
begin
  AStrings := cbxStyleSheets.Properties.Items;
end;

function TdxfmGridReportLinkDesignWindow.GetActiveStyle: TcxStyle;
begin
  Result := lbxStyles.SelectedStyle;
end;

function TdxfmGridReportLinkDesignWindow.GetHasSelectedStyles: Boolean;
begin
  Result := lbxStyles.SelCount <> 0;
end;

function TdxfmGridReportLinkDesignWindow.GetHasSelectedStylesWithAssignedBitmap: Boolean;
var
  Strings: TStrings;
  I: Integer;
  cxStyle: TcxStyle;
begin
  Result := True;
  Strings := TStringList.Create;
  try
    GetSelectedStyleNames(Strings);
    for I := 0 to Strings.Count - 1 do
    begin
      cxStyle := TcxStyle(Strings.Objects[I]);
      if (cxStyle <> nil) and (cxStyle.Bitmap <> nil) and not cxStyle.Bitmap.Empty then
        Exit;
    end;
  finally
    Strings.Free;
  end;
  Result := False;
end;

function TdxfmGridReportLinkDesignWindow.GetReportLink: TdxGridReportLink;
begin
  Result := inherited ReportLink as TdxGridReportLink;
end;

procedure TdxfmGridReportLinkDesignWindow.CardShadowColorChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsCards.Shadow.Color := TcxColorComboBox(Sender).ColorValue;
  TcxGridCardView2Styles(PreviewCardView.Styles).CardShadow.Color :=
    ReportLink.OptionsCards.Shadow.ActualColor;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.CardShadowDepthChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsCards.Shadow.Depth := TcxSpinEdit(Sender).Value;
  TcxGridCardView2OptionsView(PreviewCardView.OptionsView).ShadowDepth :=
    Min(4, TcxSpinEdit(Sender).Value);
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.CardSpaceHorzChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsCards.InterCardsSpaceHorz := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.CardSpaceVertChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsCards.InterCardsSpaceVert := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewMaxLineCountChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.MaxLineCount := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

function TdxfmGridReportLinkDesignWindow.CanSelectAllStyles: Boolean;
var
  I: Integer;
begin
  Result := AreNativeStylesAvailable;
  if Result then
  begin
    for I := 0 to lbxStyles.Items.Count - 1 do
      if not lbxStyles.Selected[I] then Exit;
    Result := False;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.ChangeActiveView(AView: TcxCustomGridView);
var
  ALevel: TcxGridLevel;
begin
  ALevel := PreviewGrid.Levels[0];
  if ALevel.GridView <> AView then
  begin
    ALevel.GridView := AView;
    if AView is TcxCustomGridTableView then
      LoadDataIntoPreviewGridView(TcxCustomGridTableView(AView));
    if AView is TcxGridChartView then
      LoadDataIntoPreviewGridChartView(TcxGridChartView(AView));
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.CreateCardView;

  function CreateRow(APropertiesClass: TcxCustomEditPropertiesClass = nil;
    AOnCustomDraw: TcxGridTableDataCellCustomDrawEvent = nil): TcxGridCardViewRow;
  begin
    Result := PreviewCardView.CreateRow;
    Result.PropertiesClass := APropertiesClass;
    Result.OnCustomDrawCell := AOnCustomDraw;
  end;

begin
  PreviewCardView := PreviewGrid.CreateView(TcxGridCardView2) as TcxGridCardView2;
  with PreviewCardView do
  begin
    LayoutDirection := ldVertical;
    Filtering.CustomizeDialog := False;
    TcxGridCardView2Styles(Styles).CardShadow := styleCardShadow;
    with TcxGridCardView2OptionsView(OptionsView) do
    begin
      CardWidth := 135;
      CardBorderWidth := 1;
      CellAutoHeight := True;
      GridLineColor := ReportLink.OptionsFormatting.GridLineColor;
      SeparatorWidth := 0;
      ScrollBars := ssNone;
      ShadowDepth := ReportLink.OptionsCards.Shadow.Depth;
     end;
     with OptionsCustomize do
     begin
       CardExpanding := False;
       RowFiltering := False;
     end;
  end;

  rowVendorName := CreateRow;
  rowVendorLogo := CreateRow(TcxImageProperties, VendorLogoCustomDrawCell);
  rowVendorCountry := CreateRow;
  rowCarModel := CreateRow;
  rowIsSuvModel := CreateRow(TcxCheckBoxProperties, IsSUVModelCustomDrawCell);
  rowSpeedCount := CreateRow(TcxTrackBarProperties, colSpeedCountCustomDrawCell);
end;

procedure TdxfmGridReportLinkDesignWindow.CreateChartView;
begin
  PreviewCharView := PreviewGrid.CreateView(TcxGridChartView) as TcxGridChartView;
  PreviewCharView.Legend.Position := cppNone;
end;

procedure TdxfmGridReportLinkDesignWindow.CreateControls;

  procedure CreateWarningPane;
  begin
    wpIncorrectOnEveryPageState := TdxPSWarningPane.Create(Self);
    bvlWarningHost.Control := wpIncorrectOnEveryPageState;
    wpIncorrectOnEveryPageState.Font.Name := 'Tahoma';
    wpIncorrectOnEveryPageState.Font.Size := 8;
  end;

  procedure CreateStylesListBox;
  begin
    lbxStyles := TdxStylesListBox.Create(Self);
    bvlStylesHost.Control := lbxStyles;
    lbxStyles.PopupMenu := pmStyles;
    lbxStyles.TabOrder := chbxUseNativeStyles.TabOrder + 1;
    lbxStyles.OnClick := lbxStylesClick;

    dxPSPopupMan.dxPSPopupMenuController.RegisterControl(lbxStyles);
  end;

begin
  seCardsShadowDepth.Properties.MinValue := Low(TdxGridCardShadowDepth);
  seCardsShadowDepth.Properties.MaxValue := High(TdxGridCardShadowDepth);

  CreateStylesListBox;
  CreateWarningPane;
end;

procedure TdxfmGridReportLinkDesignWindow.CustomDrawBorders(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo);
var
  PainterClass: TcxCustomGridCellPainterClass;
  Painter: TcxCustomGridCellPainter;
begin
  PainterClass := CustomGridCellViewInfo_GetPainterClass(AViewInfo);
  Painter := PainterClass.Create(ACanvas, AViewInfo);
  try
    CustomGridCellPainter_DrawBorders(Painter);
  finally
    Painter.Free;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.CustomDrawFooter(ACanvas: TcxCanvas;
  R: TRect; AViewInfo: TcxCustomGridCellViewInfo);
begin
  case ReportLink.OptionsFormatting.LookAndFeelKind of
    lfStandard,
    lfFlat:
      ACanvas.DrawComplexFrame(R, AViewInfo.Params.TextColor, clBtnHighlight);
    lfUltraFlat:
      ACanvas.FrameRect(R, AViewInfo.Params.TextColor);
  end;

  InflateRect(R, -1, -1);
  CustomDrawTextRect(ACanvas, R, AViewInfo, AViewInfo.BackgroundBitmap);
end;

procedure TdxfmGridReportLinkDesignWindow.CustomDrawHeader(ACanvas: TcxCanvas;
  R: TRect; AViewInfo: TcxCustomGridCellViewInfo);
const
  BottomRightColors: array[Boolean] of TColor = (clBtnFace, clBtnShadow);
begin
  ACanvas.FrameRect(R, clWindowText);

  if ReportLink.OptionsFormatting.LookAndFeelKind <> lfUltraFlat then
  begin
    InflateRect(R, -1, -1);
    ACanvas.DrawComplexFrame(R, clBtnHighlight, BottomRightColors[ReportLink.OptionsFormatting.LookAndFeelKind = lfStandard]);
  end;

  InflateRect(R, -1, -1);
  CustomDrawTextRect(ACanvas, R, AViewInfo, AViewInfo.BackgroundBitmap);
end;

procedure TdxfmGridReportLinkDesignWindow.CustomDrawTextRect(ACanvas: TcxCanvas;
  R: TRect; AViewInfo: TcxCustomGridCellViewInfo; ABackgroundBitmap: TGraphic);
const
  AlignmentHorzMap: array[TAlignment] of Integer = (cxAlignLeft, cxAlignRight, cxAlignCenter);
  AlignmentVertMap: array[TcxAlignmentVert] of Integer = (cxAlignTop, cxAlignBottom, cxAlignVCenter);
begin
  with AViewInfo do
  begin
    if ABackgroundBitmap = nil then
    begin
      ACanvas.Brush.Color := Params.Color;
      ACanvas.FillRect(R);
    end
    else
      cxBkgndDrawPicture(ABackgroundBitmap, ACanvas.Canvas, R, ppmTile, 1, 1, -R.Left, -R.Top);

    InflateRect(R, -2, -1);
    if ReportLink.OptionsFormatting.LookAndFeelKind = lfUltraFlat then
      InflateRect(R, -1, -1);
    if Params.Font <> nil then
      ACanvas.Font := Params.Font;
    ACanvas.Font.Color := Params.TextColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.DrawText(Text, R, AlignmentHorzMap[AlignmentHorz] or AlignmentVertMap[AlignmentVert] or cxSingleLine);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.InitializePreviewGrid;

  procedure SetupDataBindings(AView: TcxCustomGridTableView);

    function GetColumnCaption(Index: Integer): string;
    begin
      case Index of
        0: Result := cxGetResourceString(@sdxManufacturerNameColumnCaption);
        1: Result := cxGetResourceString(@sdxManufacturerLogoColumnCaption);
        2: Result := cxGetResourceString(@sdxManufacturerCountryColumnCaption);
        3: Result := cxGetResourceString(@sdxCarModelColumnCaption);
        4: Result := cxGetResourceString(@sdxCarIsSUVColumnCaption);
      else
        Result := cxGetResourceString(@sdxCarSpeedCountColumnCaption);
      end;
    end;

  const
    ColumnTypes: array[0..5] of TcxValueTypeClass =
      (TcxStringValueType, TcxStringValueType, TcxStringValueType,
       TcxStringValueType, TcxBooleanValueType, TcxStringValueType);
  var
    I: Integer;
  begin
    for I := 0 to AView.ItemCount - 1 do
      with AView.Items[I] do
      begin
        DataBinding.ValueTypeClass := ColumnTypes[I];
        Caption := GetColumnCaption(I);
      end;
  end;

begin
  if pcMain.ItemIndex = tshCards.Index then
    PreviewGrid.Levels[0].GridView := PreviewCardView
  else
    PreviewGrid.Levels[0].GridView := PreviewBandedView;

  SetupDataBindings(PreviewBandedView);
  PreviewGridLevel.Caption := cxGetResourceString(@sdxCarLevelCaption);

  PreviewBandedView.Bands[0].Caption := cxGetResourceString(@sdxManufacturerBandCaption);
  PreviewBandedView.Bands[1].Caption := cxGetResourceString(@sdxModelBandCaption);
  PreviewBandedView.DataController.Summary.FooterSummaryItems[0].Format := cxGetResourceString(@sdxSummaryFormat);

  SetupDataBindings(PreviewCardView);
//  PreviewCardView.Rows[3].Visible := False;
end;

procedure TdxfmGridReportLinkDesignWindow.InitializePreviewGridStyles;

  procedure ResetEvents(AStyles: TcxGridBandedTableViewStyles); overload;
  begin
    with AStyles do
    begin
      OnGetBandHeaderStyle := nil;
      OnGetContentStyle := nil;
      OnGetFooterStyle := nil;
      OnGetGroupStyle := nil;
      OnGetHeaderStyle := nil;
      OnGetPreviewStyle := nil;
    end;
  end;

  procedure ResetEvents(AStyles: TcxGridCardViewStyles); overload;
  begin
    with AStyles do
    begin
      OnGetCaptionRowStyle := nil;
      OnGetCardBorderStyle := nil;
      OnGetContentStyle := nil;
      OnGetRowCaptionStyle := nil;
    end;
  end;

  procedure SynhronizePreviewGridStyles;
  var
    I: Integer;
    AStyle: TcxStyle;
    AInternalStyle: TcxStyle;
    AIndex: Integer;
  begin
    dxPScxCommon.dxPSResetStyles(FGridLinkPreviewStyles);
    for I := 0 to ReportLink.ActiveStyles.Count - 1 do
    begin
      AIndex := ReportLink.ActiveStyles.Items[I].Index;
      AStyle := ReportLink.ActiveStyles.Values[AIndex];
      if AStyle <> nil then
      begin
        AInternalStyle := FGridLinkPreviewStyles.Values[AIndex];
        if (AStyle <> nil) and (AInternalStyle = nil) then
        begin
          FGridLinkPreviewStyles.Values[AIndex] := TcxStyle.Create(nil);
          AInternalStyle := FGridLinkPreviewStyles.Values[AIndex];
        end;
        AInternalStyle.Assign(AStyle);
      end;
    end;
  end;

begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    SynhronizePreviewGridStyles;

    PreviewGrid.RootLevelStyles.Tab := FGridLinkPreviewStyles.Caption;
    PreviewGrid.RootLevelStyles.TabsBackground := FGridLinkPreviewStyles.Caption;

    if PreviewBandedView <> nil then
    begin
      dxPScxCommon.dxPSResetStyles(PreviewBandedView.Styles);
      with PreviewBandedView.Styles do
      begin
        BandHeader := FGridLinkPreviewStyles.BandHeader;
        Content := FGridLinkPreviewStyles.Content;
        ContentEven := FGridLinkPreviewStyles.ContentEven;
        ContentOdd := FGridLinkPreviewStyles.ContentOdd;
        FilterBox := FGridLinkPreviewStyles.FilterBar;
        Footer := FGridLinkPreviewStyles.Footer;
        Group := FGridLinkPreviewStyles.Group;
        Header := FGridLinkPreviewStyles.Header;
        Preview := FGridLinkPreviewStyles.Preview;
      end;
    end;

    if PreviewCardView <> nil then
    begin
      dxPScxCommon.dxPSResetStyles(PreviewCardView.Styles);
      with PreviewCardView.Styles do
      begin
        CaptionRow := FGridLinkPreviewStyles.CardCaptionRow;
        CardBorder := styleCardBorder;
        Content := FGridLinkPreviewStyles.Content;
        ContentEven := FGridLinkPreviewStyles.ContentEven;
        ContentOdd := FGridLinkPreviewStyles.ContentOdd;
        RowCaption := FGridLinkPreviewStyles.CardRowCaption;
      end;
    end;

  end
  else
  begin
    if ReportLink.Grid <> nil then
    begin
      PreviewGrid.RootLevelStyles.Tab := ReportLink.Grid.RootLevelStyles.Tab;
      PreviewGrid.RootLevelStyles.TabsBackground := ReportLink.Grid.RootLevelStyles.TabsBackground;
    end
    else
      dxPScxCommon.dxPSResetStyles(PreviewGrid.RootLevelStyles);

    if PreviewBandedView <> nil then
      if ReportLink.ActiveView is TcxGridBandedTableView then
        PreviewBandedView.Styles := TcxGridBandedTableView(ReportLink.ActiveView).Styles
      else
        dxPScxCommon.dxPSResetStyles(PreviewBandedView.Styles);

    if PreviewCardView <> nil then
      if ReportLink.ActiveView is TcxGridCardView then
        PreviewCardView.Styles := TcxGridCardView(ReportLink.ActiveView).Styles
      else
        dxPScxCommon.dxPSResetStyles(PreviewCardView.Styles);
  end;

  PreviewGrid.RootLevelStyles.OnGetTabStyle := nil;
  if PreviewBandedView <> nil then
  begin
    ResetEvents(PreviewBandedView.Styles);
    PreviewBandedView.SizeChanged;
  end;
  if PreviewCardView <> nil then
  begin
    ResetEvents(PreviewCardView.Styles);
    PreviewCardView.SizeChanged;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.LoadDataIntoPreviewGridChartView(
  AView: TcxGridChartView);
const
  RecordCount = 4;
  CarSalesCount: array[0..RecordCount - 1] of Integer = (10, 6, 3, 5);
var
  AViewData: TcxGridChartViewData;
  I: Integer;
begin
  AView.BeginUpdate;
  try
    AView.ClearSeries;
    AView.ClearDataGroups;
    AView.CreateSeries;
    AViewData := AView.ViewData;

    AViewData.CategoryCount := RecordCount;
    for I := 0 to RecordCount - 1 do
      AViewData.Categories[I] := CarVendorName(I);
    for I := 0 to RecordCount - 1 do
      AViewData.Values[0, I] := CarSalesCount[I];
  finally
    AView.EndUpdate;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.LoadDataIntoPreviewGridView(AView: TcxCustomGridTableView);

  procedure AddRecord(ARecordIndex: Integer; const AValues: array of AnsiString);
  var
    I: Integer;
  begin
    with AView.DataController do
      for I := 0 to ItemCount - 1 do
        Values[ARecordIndex, I] := AValues[I];
  end;

const
  RecordCount = 4;
  //CarSalesCount: array[0..RecordCount - 1] of Integer = (10, 6, 3, 5);
  IsCarSUV: array[0..RecordCount - 1] of string = ('True', 'True', 'False', 'True');
  SpeedCount: array[0..RecordCount - 1] of string = ('5', '6', '5', '5');
var
  I: Integer;
begin
  AView.DataController.RecordCount := RecordCount;
  for I := 0 to RecordCount - 1 do
    AddRecord(I, [dxStringToAnsiString(CarVendorName(I)), dxPScxCommon.dxPSPreviewCarLogosAsString(I),
      dxStringToAnsiString(CarVendorCountry(I)), dxStringToAnsiString(CarModel(I)), dxStringToAnsiString(IsCarSUV[I]), dxStringToAnsiString(SpeedCount[I])]);//, CarSalesCount[I]]);
end;

procedure TdxfmGridReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgShow, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(imgOnEveryPage, IDB_DXPSGROUPICON_ONEVERYPAGE);
  dxLoadIconFromResourceEx(imgExpanding, IDB_DXPSGROUPICON_EXPANDING);
  dxLoadIconFromResourceEx(imgGridSize, IDB_DXPSGROUPICON_SIZE);
  dxLoadIconFromResourceEx(imgSelection, IDB_DXPSGROUPICON_SELECTION);
  dxLoadIconFromResourceEx(imgDetails, IDB_DXPSGROUPICON_GRIDDETAILS);
  dxLoadIconFromResourceEx(imgLevels, IDB_DXPSGROUPICON_GRIDLEVELS);
  dxLoadIconFromResourceEx(imgLookAndFeel, IDB_DXPSGROUPICON_LOOKANDFEEL);
  dxLoadIconFromResourceEx(imgRefinements, IDB_DXPSGROUPICON_REFINEMENTS);
  dxLoadIconFromResourceEx(imgPagination, IDB_DXPSGROUPICON_PAGINATION);
  dxLoadIconFromResourceEx(imgCardSizes, IDB_DXPSGROUPICON_SIZE);
  dxLoadIconFromResourceEx(imgCardSpacing, IDB_DXPSGROUPICON_SPACING);
  dxLoadIconFromResourceEx(imgPreview, IDB_DXPSGROUPICON_PREVIEW);
  dxLoadIconFromResourceEx(imgCardFraming, IDB_DXPSGROUPICON_FRAMING);
  dxLoadIconFromResourceEx(imgCardShadow, IDB_DXPSGROUPICON_SHADOW);
  dxLoadIconFromResourceEx(imgCharts, IDB_DXPSGROUPICON_GRIDCHARTS);
  dxLoadImageListFromResources(ilStylesPopup, IDIL_DXPSSTYLESMENU);
end;

procedure TdxfmGridReportLinkDesignWindow.RecreateStylesListBox;
var
  List: TList;
begin
  List := TList.Create;
  try
    SaveSelectedStyles(List);
    cxRecreateControlWnd(lbxStyles);
    RestoreSelectedStyles(List);
  finally
    List.Free;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.RestoreSelectedStyles(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    lbxStyles.Selected[Integer(AList[I])] := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SaveSelectedStyles(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to lbxStyles.Items.Count - 1 do
    if lbxStyles.Selected[I] then AList.Add(TObject(I));
end;

procedure TdxfmGridReportLinkDesignWindow.SetActivePage;
begin
  pcMain.ItemIndex := DesignerTabIndex;
  PageControl1Change(nil);
end;

procedure TdxfmGridReportLinkDesignWindow.UpdatePreviewGridStyles(const ACaption: string;
  AStyle: TcxStyle);
begin
  with PreviewGrid.RootLevelStyles do
    if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxCaptionStyle)) then
    begin
      Tab := AStyle;
      TabsBackground := AStyle;
    end;

  with PreviewBandedView.Styles do
  begin
    if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxBandHeaderStyle)) then
      BandHeader := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentStyle)) then
      Content := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentEvenStyle)) then
      ContentEven := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentOddStyle)) then
      ContentOdd := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxFooterStyle)) then
      Footer := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxGroupStyle)) then
      Group := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxHeaderStyle)) then
      Header := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxPreviewStyle)) then
      Preview := AStyle;
  end;

  with PreviewCardView.Styles do
  begin
    if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxCardCaptionRowStyle)) then
      CaptionRow := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxCardRowCaptionStyle)) then
      RowCaption := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentStyle)) then
      Content := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentEvenStyle)) then
      ContentEven := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentOddStyle)) then
      ContentOdd := AStyle
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.UpdateWarningPane;
begin
  with ReportLink.HostInfoServices do
    wpIncorrectOnEveryPageState.SetStateAndHint(IsInconsistentState, InconsistentStateText);
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsCardsByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsCards do
    case Index of
      0: AutoWidth := Value;
      1: KeepSameWidth := Value;
      2: KeepSameHeight := Value;
      3: Borders := Value;
      4: RowBordersHorz := Value;
      5: RowBordersVert := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsChartsByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsCharts do
    case Index of
      0: Transparent := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsDetailsByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsDetails do
    case Index of
      0: StartFromFocusedView := Value;
      1: OnlyFocusedView := Value;
    end;
  ReportLink.CalculateProcessParams;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsExpandingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsExpanding do
    case Index of
      0: ExpandGroupRows := Value;
      1: ExpandMasterRows := Value;
      2: ExpandCards := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsFormattingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsFormatting do
    case Index of
      0:
        begin
          UseNativeStyles := Value;
          InitializePreviewGridStyles;
          RecreateStylesListBox;
        end;
      1: SuppressBackgroundBitmaps := Value;
      2: ConsumeSelectionStyle := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsLevelsByIndex(Index: Integer; Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsLevels do
    case Index of
      0: Unwrap := Value;
      1: UnwrapTopLevel := Value;
      2: RiseActiveLevelOntoTop := Value;
      3: SkipEmptyViews := Value;
    end;
  ReportLink.CalculateProcessParams;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsOnEveryPageByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsOnEveryPage do
    case Index of
      0: Caption := Value;
      1: BandHeaders := Value;
      2: Headers := Value;
      3: Footers := Value;
      4: FilterBar := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsPaginationByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsPagination do
    case Index of
      0: TopLevelGroup := Value;
      1: OneGroupPerPage := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsRefinementsByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsRefinements do
    case Index of
      0: TransparentGraphics := Value;
      1: DisplayGraphicsAsText := Value;
      2: FlatCheckMarks := Value;
      3: TransparentRichEdits := Value;
      4: DisplayTrackBarsAsText := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsSelectionByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsSelection do
    case Index of
      0: ProcessSelection := Value;
      1: ProcessExactSelection := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsSizeByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsSize do
    case Index of
      0: AutoWidth := Value;
    end;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.SetOptionsViewByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsView do
    case Index of
      0: Caption := Value;
      1: BandHeaders := Value;
      2: Headers := Value;
      3: Footers := Value;
      4: GroupFooters := Value;
      5: ExpandButtons := Value;
      6: FilterBar := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsViewClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsViewByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsOnEveryPageClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsOnEveryPageByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsSelectionClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSelectionByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsExpandingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsExpandingByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsRefinementClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsRefinementsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsFormatingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsFormattingByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.lblUseNativeStylesClick(Sender: TObject);
begin
  if chbxUseNativeStyles.CanFocus then ActiveControl := chbxUseNativeStyles;
  chbxUseNativeStyles.Checked := not chbxUseNativeStyles.Checked;
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsPaginationClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsPaginationByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsSizeClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSizeByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsDetailsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsDetailsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsLevelsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsLevelsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.LookAndFeelClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.OptionsFormatting.LookAndFeelKind := TcxLookAndFeelKind(TcxComboBox(Sender).ItemObject);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.PageControl1Change(Sender: TObject);
begin
  if ReportLink = nil then Exit;

  if not ReportLink.ProcessParams.HasOnlyCards then
  begin
    if pcMain.ItemIndex = tshCards.Index then
      ChangeActiveView(PreviewCardView)
    else

    if pcMain.ItemIndex = tshCharts.Index then
      ChangeActiveView(PreviewCharView)
    else

    if pcMain.ItemIndex <> tshStyles.Index then
      ChangeActiveView(PreviewBandedView)
    else

    if ReportLink.IsCardViewStyle(ActiveStyle) then
      ChangeActiveView(PreviewCardView)
    else
      ChangeActiveView(PreviewBandedView);
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewVisibleClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.Visible := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.AutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmGridReportLinkDesignWindow.OptionsCardsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsCardsByIndex(TTagToInt(Tag), Checked);

  with TcxGridCardView2OptionsView(PreviewCardView.OptionsView), ReportLink.OptionsCards do
  begin
    CardBorders := Borders;
    GridLines := dxPScxCommon.dxPSMakecxGridLines(RowBordersHorz, RowBordersVert);
  end;
  //UpdatePreview;
end;

procedure TdxfmGridReportLinkDesignWindow.chbxChartsTransparentClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsChartsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmGridReportLinkDesignWindow.colSpeedCountCustomDrawCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);

  procedure DrawTrackBar(ACanvas: TcxCanvas);
  var
    ATrackBarViewInfo: TcxCustomTrackBarViewInfo;
    AViewData: TcxCustomTrackBarViewData;
    ADisplayValue: TcxEditValue;
    AProperties: TcxCustomTrackBarProperties;
  begin
    AProperties := TcxCustomTrackBarProperties(AViewInfo.Properties);
    ATrackBarViewInfo := TcxCustomTrackBarViewInfo(AProperties.GetViewInfoClass.Create);
    try
      AViewData := TcxCustomTrackBarViewData(AProperties.CreateViewData(DefaultEditStyleController.Style, True));
      try
        AProperties.PrepareDisplayValue(AViewInfo.Value, ADisplayValue, False);
        ATrackBarViewInfo.Position := ADisplayValue;
        AViewData.Calculate(ACanvas, cxRectInflate(AViewInfo.Bounds, -2, -2), Point(0, 0), cxmbNone, [], ATrackBarViewInfo, False);
        ATrackBarViewInfo.Paint(ACanvas);
      finally
        AViewData.Free;
      end;
    finally
      ATrackBarViewInfo.Free;
    end;
  end;

begin
  if ((Sender is TcxGridTableView) or (AViewInfo is TcxGridCardRowDataViewInfo)) then
  begin
    if ReportLink.OptionsRefinements.DisplayTrackBarsAsText then
    begin
      AViewInfo.AlignmentVert := vaCenter;
      AViewInfo.AlignmentHorz := taLeftJustify;
      AViewInfo.Text := IntToStr(AViewInfo.Value);
      CustomDrawTextRect(ACanvas, AViewInfo.Bounds, AViewInfo, AViewInfo.RecordViewInfo.BackgroundBitmap);
    end
    else
      DrawTrackBar(ACanvas);

    CustomDrawBorders(ACanvas, AViewInfo);
    ADone := True;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.pmStylesPopup(Sender: TObject);
begin
  lbxStyles.HideToolTips;
  miStyleColor.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleFont.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleBackgroundBitmap.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleBackgroundBitmapClear.Enabled := AreNativeStylesAvailable and HasSelectedStylesWithAssignedBitmap;
  miStyleRestoreDefaults.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStylesSelectAll.Enabled := CanSelectAllStyles;
  miStylesSaveAs.Enabled := CanSaveStyles;
end;

procedure TdxfmGridReportLinkDesignWindow.lbxStylesClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  if not ReportLink.ProcessParams.HasOnlyCards then
  begin
    if ReportLink.IsCardViewStyle(ActiveStyle) then
      ChangeActiveView(PreviewCardView)
    else
      ChangeActiveView(PreviewBandedView);
  end;
  UpdateControlsState;
end;

procedure TdxfmGridReportLinkDesignWindow.IsSUVModelCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);

  procedure DrawBackground(const R: TRect);

    function GetParams: TcxViewParams;
    begin
      if AViewInfo is TcxGridCardRowDataViewInfo then
        PreviewCardView.Styles.GetContentParams(AViewInfo.GridRecord, AViewInfo.Item, Result)
      else
        PreviewBandedView.Styles.GetContentParams(AViewInfo.GridRecord, AViewInfo.Item, Result);
    end;

  var
    BackgroundBitmap: TBitmap;
    PrevColor: TColor;
  begin
    BackgroundBitmap := cxGetAsBitmap(CustomGridView_GetBackgroundBitmaps(AViewInfo.GridRecord.GridView).GetBitmap(bbContent));
    try
      if BackgroundBitmap = nil then
      begin
        PrevColor := ACanvas.Brush.Color;
        ACanvas.Brush.Color := GetParams.Color;
        ACanvas.FillRect(R);
        ACanvas.Brush.Color := PrevColor;
      end
      else
        cxBkgndDrawPicture(BackgroundBitmap, ACanvas.Canvas, R, ppmTile, 1, 1, -R.Left, -R.Top);
    finally
      BackgroundBitmap.Free;
    end;
  end;

  procedure DrawCheck(R: TRect);
  const
    CheckStateMap: array[Boolean] of TcxCheckBoxState = (cbsChecked, cbsUnchecked);
    StylesMap: array[Boolean] of TcxLookAndFeelStyle = (lfsStandard, lfsUltraFlat);
  var
    X, Y: Integer;
    CheckState: Variant;
  begin
    X := R.Left + (R.Right - R.Left - ScaleFactor.Apply(dxPSGlbl.CheckWidth)) div 2;
    Y := R.Top + (R.Bottom - R.Top - ScaleFactor.Apply(dxPSGlbl.CheckHeight)) div 2;
    R := Bounds(X, Y, ScaleFactor.Apply(dxPSGlbl.CheckWidth), ScaleFactor.Apply(dxPSGlbl.CheckHeight));

    CheckState := Variant(CheckStateMap[AViewInfo.RecordViewInfo.GridRecord.RecordIndex = 2]);
    cxLookAndFeelPaintersManager.GetPainter(StylesMap[ReportLink.OptionsRefinements.FlatCheckMarks]).
      DrawScaledCheckButton(ACanvas, R, cxbsDefault, CheckState = Variant(cbsChecked), ScaleFactor);
  end;

var
  R: TRect;
begin
  if (Sender is TcxGridTableView) or (AViewInfo is TcxGridCardRowDataViewInfo) then
  begin
    R := AViewInfo.Bounds;
    DrawBackground(R);
    DrawCheck(R);

    CustomDrawBorders(ACanvas, AViewInfo);
    ADone := True;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.VendorLogoCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  PrevText: string;
begin
  if ReportLink.OptionsRefinements.DisplayGraphicsAsText and
    ((Sender is TcxGridTableView) or (AViewInfo is TcxGridCardRowDataViewInfo)) then
  begin
    PrevText := AViewInfo.Text;
    AViewInfo.Text := ReportLink.OptionsRefinements.GraphicsText;
    AViewInfo.AlignmentVert := vaTop;
    //AViewInfo.Transparent := True;
    CustomDrawTextRect(ACanvas, AViewInfo.Bounds, AViewInfo, AViewInfo.RecordViewInfo.BackgroundBitmap);
    AViewInfo.Text := PrevText;
    CustomDrawBorders(ACanvas, AViewInfo);
    ADone := True;
  end;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewBandedViewCustomDrawColumnHeader(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
var
  R: TRect;
begin
  R := AViewInfo.Bounds;
  Dec(R.Left);
  if not ((PreviewGrid.LookAndFeel.Kind = lfUltraFlat) and
    not TcxGridBandedTableOptionsView(Sender.OptionsView).BandHeaders) then
    Dec(R.Top);
  CustomDrawHeader(ACanvas, R, AViewInfo);

  ADone := True;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewBandedViewCustomDrawBandHeader(
  Sender: TcxGridBandedTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridBandHeaderViewInfo; var ADone: Boolean);
var
  R: TRect;
begin
  R := AViewInfo.Bounds;
  Dec(R.Left);
  CustomDrawHeader(ACanvas, R, AViewInfo);

  ADone := True;
end;

procedure TdxfmGridReportLinkDesignWindow.PreviewBandedViewCustomDrawFooterCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  ADone := Sender.LookAndFeel.SkinPainter = nil;
  if ADone then
    CustomDrawFooter(ACanvas, AViewInfo.Bounds, AViewInfo);
end;

procedure TdxfmGridReportLinkDesignWindow.StyleColorClick(Sender: TObject);
begin
  PerformStylesChangeColor;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleFontClick(Sender: TObject);
begin
  PerformStylesChangeFont;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleBackgroundBitmapClick(Sender: TObject);
begin
  PerformStylesChangeBitmap;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleBackgroundBitmapClearClick(Sender: TObject);
begin
  PerformStylesClearBitmap;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleRestoreDefaultsClick(Sender: TObject);
begin
  PerformStylesRestoreDefaults;
end;

procedure TdxfmGridReportLinkDesignWindow.miStylesSelectAllClick(Sender: TObject);
begin
  lbxStyles.SelectAll;
  UpdateControlsState;
end;

procedure TdxfmGridReportLinkDesignWindow.cbxStyleSheetsClick(Sender: TObject);
begin
  ActiveStyleSheet := TcxCustomStyleSheet(TcxComboBox(Sender).ItemObject);
end;

procedure TdxfmGridReportLinkDesignWindow.StyleSheetNewClick(Sender: TObject);
begin
  PerformStyleSheetNew;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleSheetCopyClick(Sender: TObject);
begin
  PerformStyleSheetCopy;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleSheetDeleteClick(Sender: TObject);
begin
  PerformStyleSheetDelete;
end;

procedure TdxfmGridReportLinkDesignWindow.StyleSheetRenameClick(Sender: TObject);
begin
  PerformStyleSheetRename;
end;

procedure TdxfmGridReportLinkDesignWindow.StylesSaveAsClick(Sender: TObject);
begin
  PerformStylesSaveAsStyleSheet;
end;

procedure TdxfmGridReportLinkDesignWindow.cbxStyleSheetsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  PerformStyleSheetKeyDown(Sender, Key, Shift);
end;

procedure TdxfmGridReportLinkDesignWindow.cbxStyleSheetsPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
begin
  PerformStyleSheetDrawItem(ACanvas.Canvas, AIndex, ARect, AState, AControl.Enabled);
end;

procedure RegisterAssistants;
begin
  TdxCustomGridViewBuilder.Register;
  TdxCustomGridTableViewBuilder.Register;
  TdxGridTableViewBuilder.Register;
  TdxGridDBTableViewBuilder.Register;
  TcxGridServerModeTableViewBuilder.Register;
  TdxGridBandedTableViewBuilder.Register;
  TdxGridDBBandedTableViewBuilder.Register;
  TdxGridServerModeBandedTableViewBuilder.Register;
  TdxGridCardViewBuilder.Register;
  TdxGridDBCardViewBuilder.Register;
	TdxGridChartViewBuilder.Register;
  TdxGridWinExplorerViewBuilder.Register;
  TdxGridDBWinExplorerViewBuilder.Register;

  TdxCustomGridViewHelper.Register;
  TdxCustomGridTableViewHelper.Register;
  TdxGridCardViewHelper.Register;
  TdxGridTableViewHelper.Register;
  TdxGridBandedTableViewHelper.Register;

  TdxCustomGridRecordHelper.Register;
  TdxCustomGridRowHelper.Register;
  TdxGridDataRowHelper.Register;
  TdxGridGroupRowHelper.Register;
  TdxGridMasterDataRowHelper.Register;

  TdxGridWinExplorerViewRecordHelper.Register;
  TdxGridWinExplorerViewDataRecordHelper.Register;
  TdxGridWinExplorerViewGroupRecordHelper.Register;

  TdxGridReportLinkStyleSheet.Register;
end;

procedure RegisterItems;
begin
  TdxReportCardHorz.Register;
  TdxReportCardVert.Register;

  TdxReportCardLayer.Register;
  TdxReportCardVerticalLayer.Register;

  TdxReportCardRow.Register;
end;

procedure UnregisterAssistants;
begin
  TdxGridReportLinkStyleSheet.Unregister;

  TdxGridViewBuildersFactory.ReleaseInstance;
  TdxViewHelpersFactory.ReleaseInstance;
  TdxRecordHelpersFactory.ReleaseInstance;
end;

procedure UnregisterItems;
begin
  TdxReportCardHorz.UnRegister;
  TdxReportCardVert.UnRegister;

  TdxReportCardLayer.UnRegister;
  TdxReportCardVerticalLayer.UnRegister;

  TdxReportCardRow.UnRegister;
end;
initialization
  RegisterAssistants;
  RegisterItems;

  dxPSRegisterReportLink(TdxGridReportLink, TcxGrid, TdxfmGridReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxGridReportLink, TcxGrid, TdxfmGridReportLinkDesignWindow);

  UnregisterItems;
  UnregisterAssistants;

  FreeAndNil(FGridLinkStyleSheetPrototype);

end.
