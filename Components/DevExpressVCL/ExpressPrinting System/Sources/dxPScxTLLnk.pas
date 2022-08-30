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

unit dxPScxTLLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Messages, Classes, Graphics, Controls, StdCtrls, ComCtrls,
  ExtCtrls, ImgList, Menus, Dialogs, ExtDlgs, Contnrs, Types, dxCore, cxClasses,
  cxControls, cxGraphics, cxGeometry, cxStyles, cxLookAndFeels, cxLookAndFeelPainters,
  cxEdit, cxTextEdit, cxInplaceContainer, cxTL, cxDBTL, cxTLData, cxCustomData,
  dxExtCtrls, dxBase, dxPSSngltn, dxPrnPg, dxPSCore, dxPScxCommon, cxContainer,
  cxLabel, cxCheckBox, cxMaskEdit, cxDropDownEdit, cxButtons, cxPC, cxSpinEdit,
  cxColorComboBox, dxPSReportRenderCanvas, cxTrackBar, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters,
  cxImageList, cxImage, cxDataControllerConditionalFormatting, dxPSdxSpreadSheetLnk, dxSpreadSheetStyles;

const
  vspsTreeListFirst = 0;
  vspsTreeListBandHeader = vspsTreeListFirst + 0;
  vspsTreeListContent = vspsTreeListFirst + 1;
  vspsTreeListContentEven = vspsTreeListFirst + 2;
  vspsTreeListContentOdd = vspsTreeListFirst + 3;
  vspsTreeListFooter = vspsTreeListFirst + 4;
  vspsTreeListFooterRow = vspsTreeListFirst + 5;
  vspsTreeListHeader = vspsTreeListFirst + 6;
  vspsTreeListIndent = vspsTreeListFirst + 7;
  vspsTreeListPreview = vspsTreeListFirst + 8;
  vspsTreeListSelection = vspsTreeListFirst + 9;
  vspsTreeListBandBackground = vspsTreeListFirst + 10;
  vspsTreeListLast = vspsTreeListBandBackground;

  cxTreeListAttributeIDBase = 0;
  cxTreeListUndefinedID = cxTreeListAttributeIDBase + 0;
  cxTreeListBandID = cxTreeListAttributeIDBase + 1;
  cxTreeListFooterID = cxTreeListAttributeIDBase + 2;
  cxTreeListHeaderID = cxTreeListAttributeIDBase + 3;
  cxTreeListIndentID = cxTreeListAttributeIDBase + 4;
  cxTreeListNodeID = cxTreeListAttributeIDBase + 5;

type
  TcxTreeListNodeClass = class of TcxTreeListNode;

  TcxTreeListCustomReportLink = class;
  TdxfmTreeListReportLinkDesignWindow = class;

  TcxTreeListAttributeHostInfo = class;
  TcxTreeListAdapterClass = class of TcxTreeListAdapter;
  TcxTreeListAdapter = class;
  TcxTreeListReportLinkFormatterClass = class of TcxTreeListReportLinkFormatter;
  TcxTreeListReportLinkFormatter = class;
  TcxTreeListReportLinkBuilderClass = class of TcxTreeListReportLinkBuilder;
  TcxTreeListReportLinkBuilder = class;

  TcxTreeListAttributeClass = class of TcxTreeListAttribute;
  TcxTreeListAttribute = class end;
  TcxTreeListBandAttribute = class(TcxTreeListAttribute);
  TcxTreeListNodeAttribute = class(TcxTreeListAttribute);
  TcxTreeListFooterAttribute = class(TcxTreeListAttribute);
  TcxTreeListHeaderAttribute = class(TcxTreeListAttribute);
  TcxTreeListIndentsRowAttribute = class(TcxTreeListAttribute);

  TcxTreeListColumnPlace = class;
  TcxTreeListCustomBandPlace = class;
  TcxTreeListBandPlace = class;
  TcxTreeListItemPlaceController = class;

  TcxTreeListAttributeID = type Integer;

  TcxTreeListCellCustomDrawInfo = record
    AttributeID: TcxTreeListAttributeID;
    Band: TcxTreeListBand;
    Column: TcxTreeListColumn;
    Node: TcxTreeListNode;
    Index: Integer;
  end;

  { producers }

  TcxTreeListReportLinkRowProducerClass = class of TcxTreeListReportLinkRowProducer;

  TcxTreeListReportLinkRowProducer = class
  private
    FBuilder: TcxTreeListReportLinkBuilder;
    FHost: TdxReportCell;
    FRow: TdxReportCell;
    FRowHeight: Integer;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetIndentWidth: Integer;
    function GetRowWidth: Integer;
  protected
    procedure AfterCalculateRowHeight; virtual;
    procedure CalculateRowAutoHeight; virtual;
    procedure CalculateRowHeight; virtual;
    procedure CreateRow; virtual;
    procedure CreateRowHost(const AHostInfo: TcxTreeListAttributeHostInfo); virtual;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean; virtual;
    procedure FixupRowDataHeight; virtual;
    procedure FixupRowHeight; virtual;
    procedure FixupRowItselfHeight; virtual;
    procedure InitializeRow; virtual;

    function GetAutoHeight: Boolean; virtual;
    function GetLineCount: Integer; virtual;
    function GetLineHeight: Integer; virtual;

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder); virtual;

    function Adapter: TcxTreeListAdapter; overload; virtual;
    function Builder: TcxTreeListReportLinkBuilder; overload; virtual;
    function Formatter: TcxTreeListReportLinkFormatter; overload; virtual;

    function Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell; virtual;

    property AutoHeight: Boolean read GetAutoHeight;
    property Host: TdxReportCell read FHost;
    property IndentWidth: Integer read GetIndentWidth;
    property LineCount: Integer read GetLineCount;
    property LineHeight: Integer read GetLineHeight;
    property Row: TdxReportCell read FRow;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowWidth: Integer read GetRowWidth;
  end;

  TcxTreeListReportLinkNodeSeparatorProducerClass = class of TcxTreeListReportLinkNodeSeparatorProducer;

  TcxTreeListReportLinkNodeSeparatorProducer = class(TcxTreeListReportLinkRowProducer)
  private
    FItem: TAbstractdxReportCellData;
  protected
    procedure InitializeItem; virtual;
    procedure InitializeRow; override;

    function GetAutoHeight: Boolean; override;
    function GetItemClass: TdxReportCellDataClass; virtual;
    function GetLineHeight: Integer; override;
  public
    function Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell; override;
  end;

  TcxTreeListReportLinkRowSubItemsProducerClass = class of TcxTreeListReportLinkRowSubItemsProducer;

  TcxTreeListReportLinkRowSubItemsProducer = class(TcxTreeListReportLinkRowProducer)
  private
    function GetColumn(Index: Integer): TcxTreeListColumn;
  protected
    procedure CreateDataItems(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;

    function GetHasSubItem(Index: Integer): Boolean; virtual;
    function GetSubItemBound(Index: Integer): TRect; virtual;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; virtual; abstract;
    function GetSubItemCount: Integer; virtual;
  public
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumn;
    property HasSubItem[Index: Integer]: Boolean read GetHasSubItem;
    property SubItemBounds[Index: Integer]: TRect read GetSubItemBound;
    property SubItemClasses[Index: Integer]: TdxReportCellDataClass read GetSubItemClass;
    property SubItemCount: Integer read GetSubItemCount;
  end;

  TcxTreeListReportLinkBandsProducerClass = class of TcxTreeListReportLinkBandsProducer;

  TcxTreeListReportLinkBandsProducer = class(TcxTreeListReportLinkRowSubItemsProducer)
  protected
    procedure CalculateRowHeight; override;
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AnIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
    function GetSubItemCount: Integer; override;
  end;

  TcxTreeListReportLinkFootersProducerClass = class of TcxTreeListReportLinkFootersProducer;

  TcxTreeListReportLinkBandedRowProducer = class(TcxTreeListReportLinkRowProducer)
  private
    FIndents: TList;
    function GetIndent(Index: Integer): TdxReportCellExpandButton;
  protected
    procedure AfterCalculateRowHeight; override;
    procedure CreateBandedDataItems(AParent: TdxReportCell); virtual;
    procedure CreateBandedRows(AParent: TdxReportCell); virtual;
    procedure CreateIndents(AParent: TdxReportCell); virtual;
    procedure CreateIndentsRow(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;

    function GetBandedDataItemBounds(AItem: TAbstractdxReportCellData): TRect; virtual;
    function GetBandedDataItemClass(AColumn: TcxTreeListColumn): TdxReportCellDataClass; virtual;
    function GetBandedRowIndent: Integer; virtual;

    function GetIndentArea: Integer; virtual;
    function GetIndentBound(AIndex: Integer): TRect; virtual;
    function GetIndentCount: Integer; virtual;

    procedure InitializeBandedDataItem(AItem: TAbstractdxReportCellData); virtual;
    procedure InitializeBandedRow(ARow: TdxReportCell); virtual;
    procedure InitializeIndent(AnIndent: TdxReportCellExpandButton; AnIndex: Integer); virtual;

    property IndentArea: Integer read GetIndentArea;
    property IndentBounds[Index: Integer]: TRect read GetIndentBound;
    property IndentCount: Integer read GetIndentCount;
    property Indents[Index: Integer]: TdxReportCellExpandButton read GetIndent;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder); override;
    destructor Destroy; override;
    function Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell; override;
  end;

  TcxTreeListReportLinkFootersProducer = class(TcxTreeListReportLinkBandedRowProducer)
  private
    FAttachedNode: TcxTreeListNode;
    FDataNode: TcxTreeListNode;
  protected
    procedure CreateBandedDataItems(AParent: TdxReportCell); override;
    function GetAutoHeight: Boolean; override;
    function GetBandedRowIndent: Integer; override;
    function GetIndentCount: Integer; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    procedure InitializeBandedRow(ARow: TdxReportCell); override;
    procedure InitializeIndent(AnIndent: TdxReportCellExpandButton; AnIndex: Integer); override;
    procedure InitializeRow; override;
  public
    function Produce(AHostInfo: TcxTreeListAttributeHostInfo; AAttachedNode,
      ADataNode: TcxTreeListNode): TdxReportCell; reintroduce; virtual;
  end;

  TcxTreeListReportLinkHeadersProducerClass = class of TcxTreeListReportLinkHeadersProducer;

  TcxTreeListReportLinkHeadersProducer = class(TcxTreeListReportLinkRowSubItemsProducer)
  protected
    procedure InitializeRow; override;
    procedure InitializeSubItem(ASubItem: TAbstractdxReportCellData; AnIndex: Integer); override;

    function GetAutoHeight: Boolean; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetSubItemBound(Index: Integer): TRect; override;
    function GetSubItemClass(Index: Integer): TdxReportCellDataClass; override;
  end;

  TcxTreeListReportLinkNodeProducerClass = class of TcxTreeListReportLinkNodeProducer;

  TcxTreeListReportLinkNodeProducer = class(TcxTreeListReportLinkBandedRowProducer)
  private
    FCheck: TdxCustomReportCellCheck;
    FImage: TdxReportCellGraphic;
    FNode: TcxTreeListNode;
    FPreviewItem: TAbstractdxReportCellData;
    FStateImage: TdxReportCellGraphic;
    function GetCheckIndentIndex: Integer;
    function GetImageIndentIndex: Integer;
    function GetPreviewColumn: TcxTreeListColumn;
    function GetPreviewPlace: TcxTreeListPreviewPlace;
    function GetStateImageIndentIndex: Integer;
  protected
    procedure CalculateRowAutoHeight; override;
    procedure CalculateRowHeight; override;
    procedure CreateChecks(AParent: TdxReportCell); virtual;
    procedure CreateIcons(AParent: TdxReportCell); virtual;
    procedure CreateImages(AParent: TdxReportCell); virtual;
    procedure CreateIndents(AParent: TdxReportCell); override;
    procedure CreatePreview(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;
    procedure CreateStateImages(AParent: TdxReportCell); virtual;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TdxReportVisualItem): Boolean; override;
    procedure FixupRowDataHeight; override;
    procedure InitializeBandedDataItem(AItem: TAbstractdxReportCellData); override;
    procedure InitializeBandedRow(ARow: TdxReportCell); override;
    procedure InitializeCheck; virtual;
    procedure InitializeImage(AnIndex: Integer); virtual;
    procedure InitializeIndent(AnIndent: TdxReportCellExpandButton; AnIndex: Integer); override;
    procedure InitializeRow; override;
    procedure InitializeStateImage(AnIndex: Integer); virtual;
    function IsItemIndent(AnItem: TdxReportVisualItem): Boolean;

    function GetAutoHeight: Boolean; override;
    function GetBandedDataItemBounds(AItem: TAbstractdxReportCellData): TRect; override;
    function GetBandedDataItemClass(AColumn: TcxTreeListColumn): TdxReportCellDataClass; override;
    function GetBandedRowIndent: Integer; override;
    function GetCellAutoHeight: Boolean; virtual;
    function GetCheckRect: TRect; virtual;
    function GetHasPreview: Boolean; virtual;
    function GetImageRect: TRect; virtual;
    function GetIndentCount: Integer; override;
    function GetLineCount: Integer; override;
    function GetLineHeight: Integer; override;
    function GetPreviewHeight: Integer; virtual;
    function GetPreviewIndent: Integer; virtual;
    function GetPreviewLineCount: Integer; virtual;
    function GetPreviewLineHeight: Integer; virtual;
    function GetStateImageRect: TRect; virtual;
  public
    function Produce(AHostInfo: TcxTreeListAttributeHostInfo;
      ANode: TcxTreeListNode): TdxReportCell; reintroduce; virtual;

    property CellAutoHeight: Boolean read GetCellAutoHeight;
    property CheckIndentIndex: Integer read GetCheckIndentIndex;
    property CheckRect: TRect read GetCheckRect;
    property HasPreview: Boolean read GetHasPreview;
    property Image: TdxReportCellGraphic read FImage;
    property ImageIndentIndex: Integer read GetImageIndentIndex;
    property ImageRect: TRect read GetImageRect;
    property PreviewColumn: TcxTreeListColumn read GetPreviewColumn;
    property PreviewHeight: Integer read GetPreviewHeight;
    property PreviewIndent: Integer read GetPreviewIndent;
    property PreviewItem: TAbstractdxReportCellData read FPreviewItem;
    property PreviewLineCount: Integer read GetPreviewLineCount;
    property PreviewLineHeight: Integer read GetPreviewLineHeight;
    property PreviewPlace: TcxTreeListPreviewPlace read GetPreviewPlace;
    property StateImage: TdxReportCellGraphic read FStateImage;
    property StateImageIndentIndex: Integer read GetStateImageIndentIndex;
    property StateImageRect: TRect read GetStateImageRect;
  end;

  TcxTreeListReportLinkCategorizedNodeProducer = class(TcxTreeListReportLinkNodeProducer)
  protected
    procedure CreateBandedRows(AParent: TdxReportCell); override;
  end;

  { helpers }

  TcxTreeListNodeHelperClass = class of TcxTreeListNodeHelper;

  TcxTreeListNodeHelper = class(TdxCustomClassMapItem)
  private
    FAdapter: TcxTreeListAdapter;
    FNode: TcxTreeListNode;
  protected
    function Adapter: TcxTreeListAdapter; overload; virtual;
    class function NodeClass: TcxTreeListNodeClass; virtual;

    function GetHasSelectedChildren: Boolean; virtual;
    function GetHasSelectedParents: Boolean; virtual;
  public
    constructor Create(AnAdapter: TcxTreeListAdapter); virtual;

    class function PairClass: TClass; override;
    class function ProducerClass: TcxTreeListReportLinkNodeProducerClass; virtual;
    class procedure Register;
    class procedure Unregister;

    property HasSelectedChildren: Boolean read GetHasSelectedChildren;
    property HasSelectedParents: Boolean read GetHasSelectedParents;
    property Node: TcxTreeListNode read FNode write FNode;
  end;

  TcxTreeListNodeHelperCache = class(TdxCustomCache)
  private
    FAdapter: TcxTreeListAdapter;
    function GetHelper(Node: TcxTreeListNode): TcxTreeListNodeHelper;
    function GetItem(Index: Integer): TcxTreeListNodeHelper;
  protected
    function IndexOf(Node: TcxTreeListNode): Integer;
    property Items[Index: Integer]: TcxTreeListNodeHelper read GetItem;
  public
    constructor Create(AnAdapter: TcxTreeListAdapter);

    property Adapter: TcxTreeListAdapter read FAdapter;
    property Helpers[Node: TcxTreeListNode]: TcxTreeListNodeHelper read GetHelper; default;
  end;

  TcxTreeListReportLinkProducerCache = class(TdxCustomCache)
  private
    FBuilder: TcxTreeListReportLinkBuilder;
    function GetItem(Index: Integer): TcxTreeListReportLinkRowProducer;
    function GetProducer(ProducerClass: TcxTreeListReportLinkRowProducerClass): TcxTreeListReportLinkRowProducer;
  protected
    function IndexOf(AProducerClass: TcxTreeListReportLinkRowProducerClass): Integer;
    property Items[Index: Integer]: TcxTreeListReportLinkRowProducer read GetItem;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder);
    property Builder: TcxTreeListReportLinkBuilder read FBuilder;
    property Producers[ProducerClass: TcxTreeListReportLinkRowProducerClass]: TcxTreeListReportLinkRowProducer read GetProducer; default;
  end;

  TcxTreeListReportLinkBuilder = class
  private
    FAdapter: TcxTreeListAdapter;
    FFormatter: TcxTreeListReportLinkFormatter;
    FProducerCache: TcxTreeListReportLinkProducerCache;
    FReportLink: TcxTreeListCustomReportLink;
    FReportRows: TList;
    function GetHost: TdxReportCell;
    function GetReportCells: TdxReportCells;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure AddReportRow(ARow: TdxReportCell);
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;
    { bands }
    procedure CreateBands; virtual;
    function GetBandsProducer: TcxTreeListReportLinkBandsProducer;
    function GetBandsProducerClass: TcxTreeListReportLinkBandsProducerClass; virtual;
    { footers }
    procedure CreateFooters(AHostInfo: TcxTreeListAttributeHostInfo;
      AAttachedNode, ADataNode: TcxTreeListNode); virtual;
    function GetFootersProducer: TcxTreeListReportLinkFootersProducer;
    function GetFootersProducerClass: TcxTreeListReportLinkFootersProducerClass; virtual;
    { headers }
    procedure CreateHeaders; virtual;
    function GetHeadersProducer: TcxTreeListReportLinkHeadersProducer;
    function GetHeadersProducerClass: TcxTreeListReportLinkHeadersProducerClass; virtual;
    { nodes }
    procedure CreateNode(ANode: TcxTreeListNode); virtual;
    procedure CreateNodes; virtual;
    function GetNodeProducer(ANode: TcxTreeListNode): TcxTreeListReportLinkNodeProducer;
    function GetNodeProducerClass(ANode: TcxTreeListNode): TcxTreeListReportLinkNodeProducerClass; virtual;
    { separators }
    procedure CreateNodeSeparator(ANode: TcxTreeListNode); virtual;
    function GetNodeSeparatorProducer: TcxTreeListReportLinkNodeSeparatorProducer;
    function GetNodeSeparatorProducerClass: TcxTreeListReportLinkNodeSeparatorProducerClass; virtual;

    procedure DoBuild; virtual;
    function IsAborted: Boolean;

    property ProducerCache: TcxTreeListReportLinkProducerCache read FProducerCache;
    property ReportLink: TcxTreeListCustomReportLink read FReportLink;
  public
    constructor Create(AReportLink: TcxTreeListCustomReportLink); virtual;
    destructor Destroy; override;

    procedure Build; virtual;
    procedure Progress(const APercentDone: Double);

    function Adapter: TcxTreeListAdapter; overload; virtual;
    class function AdapterClass: TcxTreeListAdapterClass; virtual;
    function Formatter: TcxTreeListReportLinkFormatter; overload; virtual;
    class function FormatterClass: TcxTreeListReportLinkFormatterClass; virtual;

    property Host: TdxReportCell read GetHost;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  TcxTreeListReportLinkBuilderHandler = class
  private
    FBuilder: TcxTreeListReportLinkBuilder;
    function GetReportLink: TcxTreeListCustomReportLink;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder); virtual;
    function Builder: TcxTreeListReportLinkBuilder; overload; virtual;

    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  end;

  TcxTreeListAdapter = class(TcxTreeListReportLinkBuilderHandler)
  private
    FConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    FDetailsLineCount: Integer;
    FFooterLineCount: Integer;
    FGroupFooterLineCount: Integer;
    FHelperCache: TcxTreeListNodeHelperCache;
    FMultiRows: Boolean;

    function GetAutoWidth: Boolean;
    function GetBand(Index: Integer): TcxTreeListBand;
    function GetBandCount: Integer;
    function GetBottomBand(Index: Integer): TcxTreeListBand;
    function GetBottomBandCount: Integer;
    function GetCanUseLookAndFeelColors: Boolean;
    function GetCategorizedColumn: TcxTreeListColumn;
    function GetCellAutoHeight: Boolean;
    function GetCellEndEllipsis: Boolean;
    function GetCellMultiline: Boolean;
    function GetColumn(Index: Integer): TcxTreeListColumn;
    function GetColumnCount: Integer;
    function GetDefaultRowHeight: Integer;
    function GetFooterLineCount(ANode: TcxTreeListNode): Integer;
    function GetGridLines: TcxTreeListGridLines;
    function GetGridLinesColor: TColor;
    function GetHasPreview: Boolean;
    function GetHeaderAutoHeight: Boolean;
    function GetHelper(Node: TcxTreeListNode): TcxTreeListNodeHelper;
    function GetImages: TCustomImageList;
    function GetIndentWidth: Integer;
    function GetIsDefaultRowHeightAssigned: Boolean;
    function GetIsNodeColorUsedForIndents: Boolean;
    function GetLookAndFeelKind: TcxLookAndFeelKind;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetMultiSelection: Boolean;
    function GetNode(Index: Integer): TcxTreeListNode;
    function GetNodeCount: Integer;
    function GetOptionsView: TcxTreeListOptionsView;
    function GetPaintStyle: TcxTreeListPaintStyle;
    function GetPreviewColumn: TcxTreeListColumn;
    function GetPreviewLeftIndent: Integer;
    function GetPreviewPlace: TcxTreeListPreviewPlace;
    function GetPreviewRightIndent: Integer;
    function GetRootBand(Index: Integer): TcxTreeListBand;
    function GetRootBandCount: Integer;
    function GetShowRoot: Boolean;
    function GetShowHorzGridLines: Boolean;
    function GetShowTreeLines: Boolean;
    function GetShowVertGridLines: Boolean;
    function GetStateImages: TCustomImageList;
    function GetStyles: TcxTreeListStyles;
    function GetThemedBandHeaderItemColor: TColor;
    function GetThemedBandHeaderItemTextColor: TColor;
    function GetThemedFooterItemColor: TColor;
    function GetThemedFooterItemTextColor: TColor;
    function GetThemedHeaderItemColor: TColor;
    function GetThemedHeaderItemTextColor: TColor;
    function GetThemedHeaderRowColor: TColor;
    function GetTreeLinesColor: TColor;
    function GetTreeLinesStyle: TcxTreeListTreeLineStyle;
    function GetTreeList: TcxCustomTreeList;
    function GetUseStylesForIndents: Boolean;
  protected
    function CalculateDetailsLineCount: Integer; virtual;
    procedure Calculate;
    { properties }
    class function GetProperties(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TcxCustomEditProperties;
    class function GetPropertiesClass(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TcxCustomEditPropertiesClass;
    class function GetRepositoryItem(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TcxEditRepositoryItem;
    { styles }
    function GetBackgroundViewParams: TcxViewParams; virtual;
    function GetBandBackgroundViewParams: TcxViewParams; virtual;
    function GetBandHeaderViewParams(ABand: TcxTreeListBand): TcxViewParams; virtual;
    function GetColumnFooterViewParams(AColumn: TcxTreeListColumn): TcxViewParams; virtual;
    function GetColumnHeaderViewParams(AColumn: TcxTreeListColumn): TcxViewParams; virtual;
    function GetContentViewParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams; virtual;
    function GetIndentViewParams(ANode: TcxTreeListNode; AnIndent: Integer): TcxViewParams; virtual;
    function GetPreviewViewParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams; virtual;
    function GetFooterRowViewParams: TcxViewParams; virtual;
    function GetSelectionViewParams: TcxViewParams; virtual;
    function HasSelectionStyle: Boolean;
    function TryGetAdvancedStyle(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean; virtual;
    { Backgrounds }
    function GetBackgroundBitmap(Index: Integer): TBitmap;
    function HasBackgroundBitmap(Index: Integer): Boolean;

    procedure DoGetLevelImages(ALevel: Integer; var AImages, AStateImages: TCustomImageList);
    function GetCheckWidth(ANode: TcxTreeListNode): Integer;
    function HasCheck(ANode: TcxTreeListNode): Boolean;
    function IsGroupNode(ANode: TcxTreeListNode): Boolean;
    function HasNodeAttachedFooter(ANode, ASummaryNode: TcxTreeListNode): Boolean;
    function HasNodeSummary(ANode: TcxTreeListNode): Boolean;

    property ConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider read FConditionalFormattingProvider;
    property Helpers[Node: TcxTreeListNode]: TcxTreeListNodeHelper read GetHelper;
    property LookAndFeelKind: TcxLookAndFeelKind read GetLookAndFeelKind;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property OptionsView: TcxTreeListOptionsView read GetOptionsView;
    property Styles: TcxTreeListStyles read GetStyles;
    property TreeList: TcxCustomTreeList read GetTreeList;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder); override;
    destructor Destroy; override;

    property AutoWidth: Boolean read GetAutoWidth;
    property BandCount: Integer read GetBandCount;
    property Bands[Index: Integer]: TcxTreeListBand read GetBand;
    property BottomBandCount: Integer read GetBottomBandCount;
    property BottomBands[Index: Integer]: TcxTreeListBand read GetBottomBand;
    property CanUseLookAndFeelColors: Boolean read GetCanUseLookAndFeelColors;
    property CategorizedColumn: TcxTreeListColumn read GetCategorizedColumn;
    property CellAutoHeight: Boolean read GetCellAutoHeight;
    property CellEndEllipsis: Boolean read GetCellEndEllipsis;
    property CellMultiline: Boolean read GetCellMultiline;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumn;
    property DefaultRowHeight: Integer read GetDefaultRowHeight;
    property DetailsLineCount: Integer read FDetailsLineCount;
    property FooterLineCount[ANode: TcxTreeListNode]: Integer read GetFooterLineCount;
    property GridLines: TcxTreeListGridLines read GetGridLines;
    property GridLinesColor: TColor read GetGridLinesColor;
    property HasPreview: Boolean read GetHasPreview;
    property HeaderAutoHeight: Boolean read GetHeaderAutoHeight;
    property Images: TCustomImageList read GetImages;
    property IndentWidth: Integer read GetIndentWidth;
    property IsDefaultRowHeightAssigned: Boolean read GetIsDefaultRowHeightAssigned;
    property IsNodeColorUsedForIndents: Boolean read GetIsNodeColorUsedForIndents;
    property MultiSelection: Boolean read GetMultiSelection;
    property PaintStyle: TcxTreeListPaintStyle read GetPaintStyle;
    property PreviewColumn: TcxTreeListColumn read GetPreviewColumn;
    property PreviewLeftIndent: Integer read GetPreviewLeftIndent;
    property PreviewPlace: TcxTreeListPreviewPlace read GetPreviewPlace;
    property PreviewRightIndent: Integer read GetPreviewRightIndent;
    property NodeCount: Integer read GetNodeCount;
    property Nodes[Index: Integer]: TcxTreeListNode read GetNode;
    property RootBandCount: Integer read GetRootBandCount;
    property RootBands[Index: Integer]: TcxTreeListBand read GetRootBand;
    property ShowRoot: Boolean read GetShowRoot;
    property ShowHorzGridLines: Boolean read GetShowHorzGridLines;
    property ShowTreeLines: Boolean read GetShowTreeLines;
    property ShowVertGridLines: Boolean read GetShowVertGridLines;
    property StateImages: TCustomImageList read GetStateImages;
    property ThemedBandHeaderItemColor: TColor read GetThemedBandHeaderItemColor;
    property ThemedBandHeaderItemTextColor: TColor read GetThemedBandHeaderItemTextColor;
    property ThemedFooterItemColor: TColor read GetThemedFooterItemColor;
    property ThemedFooterItemTextColor: TColor read GetThemedFooterItemTextColor;
    property ThemedHeaderItemColor: TColor read GetThemedHeaderItemColor;
    property ThemedHeaderItemTextColor: TColor read GetThemedHeaderItemTextColor;
    property ThemedHeaderRowColor: TColor read GetThemedHeaderRowColor;
    property TreeLinesColor: TColor read GetTreeLinesColor;
    property TreeLinesStyle: TcxTreeListTreeLineStyle read GetTreeLinesStyle;
    property UseStylesForIndents: Boolean read GetUseStylesForIndents;
  end;

  TcxTreeListCustomItemPlaceController = class
  private
    FFormatter: TcxTreeListReportLinkFormatter;
    FHeaderLineCount: Integer;
    FWidth: Integer;
    function GetAdapter: TcxTreeListAdapter;
    function GetHeaderLineCount: Integer;
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
  protected
    function CalculateWidth: Integer; virtual;
    function CalculateHeaderLineCount: Integer; virtual;
    function GetItemByColumn(Column: TcxTreeListColumn): TcxTreeListColumnPlace; virtual;
    procedure WidthChanged; virtual;
  public
    constructor Create(AFormatter: TcxTreeListReportLinkFormatter); virtual;
    procedure Calculate; virtual;
    procedure Refresh; virtual;

    property Adapter: TcxTreeListAdapter read GetAdapter;
    property Formatter: TcxTreeListReportLinkFormatter read FFormatter;
    property HeaderLineCount: Integer read GetHeaderLineCount;
    property ItemsByColumn[Column: TcxTreeListColumn]: TcxTreeListColumnPlace read GetItemByColumn; default;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TcxTreeListColumnPlace = class
  private
    FColumn: TcxTreeListColumn;
    FController: TcxTreeListCustomBandPlace;
    FLeftBound: Integer;
    FWidth: Integer;
    function GetAdapter: TcxTreeListAdapter;
    function GetCellBounds(ANode: TcxTreeListNode): TRect;
    function GetCellHeight(ANode: TcxTreeListNode): Integer;
    function GetFooterCellBounds(AIndex: Integer): TRect;
    function GetFooterLineHeight: Integer;
    function GetFormatter: TcxTreeListReportLinkFormatter;
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

    property Adapter: TcxTreeListAdapter read GetAdapter;
    property Formatter: TcxTreeListReportLinkFormatter read GetFormatter;
    property IsFixed: Boolean read GetIsFixed;
    property LineCount: Integer read GetLineCount;
    property MinWidth: Integer read GetMinWidth;
    property OriginalWidth: Integer read GetOriginalWidth;
    property RowIndex: Integer read GetRowIndex;
  public
    constructor Create(AController: TcxTreeListCustomBandPlace; AColumn: TcxTreeListColumn); virtual;
    procedure Calculate(ALeftBound: Integer); virtual;

    property CellBounds[ANode: TcxTreeListNode]: TRect read GetCellBounds;
    property CellHeights[ANode: TcxTreeListNode]: Integer read GetCellHeight;
    property Column: TcxTreeListColumn read FColumn;
    property Controller: TcxTreeListCustomBandPlace read FController;
    property FooterCellBounds[AIndex: Integer]: TRect read GetFooterCellBounds;
    property FooterLineHeight: Integer read GetFooterLineHeight;
    property HeaderCellBounds: TRect read GetHeaderCellBounds;
    property HeaderLineHeight: Integer read GetHeaderLineHeight;
    property Index: Integer read GetIndex;
    property LeftBound: Integer read GetLeftBound write FLeftBound;
    property Width: Integer read GetWidth write FWidth;
  end;

  TcxTreeListCustomBandPlace = class(TcxTreeListCustomItemPlaceController)
  private
    FBand: TcxTreeListBand;
    FColumnIndexes: TList;
    FController: TcxTreeListItemPlaceController;
    FHeight: Integer;
    FItems: TObjectList;
    FLeftBound: Integer;
    FMinWidth: Integer;
    FParent: TcxTreeListBandPlace;
    FTopBound: Integer;
    function GetColumnIndex(AColumn: TcxTreeListColumn): Integer;
    function GetBounds: TRect;
    function GetExpandable: Boolean;
    function GetHeight: Integer;
    function GetIndex: Integer;
    function GetIsFixed: Boolean;
    function GetItem(AIndex: Integer): TcxTreeListColumnPlace;
    function GetItemCount: Integer;
    function GetLeftBound: Integer;
    function GetLevelIndex: Integer;
    function GetMinWidth: Integer;
    function GetTopBound: Integer;
    function GetViewParams: TdxReportItemViewParams;
  protected
    function CalculateHeaderLineCount: Integer; override;
    function CalculateWidth: Integer; override;
    function GetItemByColumn(Column: TcxTreeListColumn): TcxTreeListColumnPlace; override;

    procedure AddItems; virtual;
    procedure AssignWidth;
    function CalculateHeight: Integer; virtual;
    function CalculateItemLeftBound(AnItem: TcxTreeListColumnPlace): Integer; virtual;
    procedure CalculateItemsWidth; virtual;
    function CalculateLeftBound: Integer; virtual;
    function CalculateLevelHeight: Integer; virtual;
    function CalculateLineHeight: Integer; virtual;
    function CalculateMinWidth: Integer; virtual;
    function CalculateTopBound: Integer; virtual;
    function CreateItem(AColumn: TcxTreeListColumn): TcxTreeListColumnPlace; virtual;
    procedure InitAutoWidthItem(AnItem: TcxAutoWidthItem); virtual;

    function GetRowCount: Integer; virtual;
    function GetItemsAutoWidth: Boolean; virtual;
    function GetItemsAvailableWidth: Integer; virtual;
    function InternalCalculateMinWidth: Integer; virtual;
    function InternalCalculateWidth: Integer; virtual;

    property Band: TcxTreeListBand read FBand;
    property IsFixed: Boolean read GetIsFixed;
    property ItemsAutoWidth: Boolean read GetItemsAutoWidth;
    property ItemsAvailableWidth: Integer read GetItemsAvailableWidth;
    property LevelIndex: Integer read GetLevelIndex;
    property MinWidth: Integer read GetMinWidth;
    property RowCount: Integer read GetRowCount;
    property ViewParams: TdxReportItemViewParams read GetViewParams;
  public
    constructor Create(AController: TcxTreeListItemPlaceController;
      AParent: TcxTreeListBandPlace; ABand: TcxTreeListBand); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate; override;
    function IndexOf(AnItem: TcxTreeListColumnPlace): Integer; overload;
    function IndexOf(AColumn: TcxTreeListColumn): Integer; overload;
    procedure Refresh; override;

    property Bounds: TRect read GetBounds;
    property ColumnIndexes[Column: TcxTreeListColumn]: Integer read GetColumnIndex;
    property Controller: TcxTreeListItemPlaceController read FController;
    property Expandable: Boolean read GetExpandable;
    property Height: Integer read GetHeight write FHeight;
    property Index: Integer read GetIndex;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TcxTreeListColumnPlace read GetItem; default;
    property LeftBound: Integer read GetLeftBound write FLeftBound;
    property Parent: TcxTreeListBandPlace read FParent;
    property TopBound: Integer read GetTopBound write FTopBound;
  end;

  TcxTreeListCustomBandPlaceClass = class of TcxTreeListCustomBandPlace;

  TcxTreeListBandPlace = class(TcxTreeListCustomBandPlace)
  private
    FChildItems: TList;
    function GetChildItem(Index: Integer): TcxTreeListCustomBandPlace;
    function GetChildItemCount: Integer;
  protected
    function GetRowCount: Integer; override;
    function InternalCalculateMinWidth: Integer; override;
    function InternalCalculateWidth: Integer; override;
    procedure WidthChanged; override;

    procedure CalculateChildItemWidths;
    procedure RefreshChildItems;
  public
    constructor Create(AnOwner: TcxTreeListItemPlaceController;
      AParent: TcxTreeListBandPlace; ABand: TcxTreeListBand); override;
    destructor Destroy; override;

    function IndexOf(AnItem: TcxTreeListCustomBandPlace): Integer;
    procedure Refresh; override;

    property ChildItemCount: Integer read GetChildItemCount;
    property ChildItems[Index: Integer]: TcxTreeListCustomBandPlace read GetChildItem;
  end;

  TcxTreeListBottomBandPlace = class(TcxTreeListCustomBandPlace)
  protected
    procedure AddItems; override;
    function CalculateItemLeftBound(AnItem: TcxTreeListColumnPlace): Integer; override;
    procedure CalculateItemsWidth; override;
    function GetItemsAutoWidth: Boolean; override;
    function GetItemsAvailableWidth: Integer; override;

    function GetRowCount: Integer; override;
    function InternalCalculateMinWidth: Integer; override;
    function InternalCalculateWidth: Integer; override;
    procedure WidthChanged; override;
  end;

  TcxTreeListItemPlaceController = class(TcxTreeListCustomItemPlaceController)
  private
    FBottomItems: TList;
    FHeight: Integer;
    FItems: TObjectList;
    FLevelCount: Integer;
    FLevelHeights: TList;
    FRootItems: TList;
    function GetAutoWidth: Boolean;
    function GetAvailableWidth: Integer;
    function GetBottomItem(AIndex: Integer): TcxTreeListCustomBandPlace;
    function GetBottomItemCount: Integer;
    function GetHeight: Integer;
    function GetItem(AIndex: Integer): TcxTreeListCustomBandPlace;
    function GetItemByBand(ABand: TcxTreeListBand): TcxTreeListCustomBandPlace;
    function GetItemCount: Integer;
    function GetLevelCount: Integer;
    function GetLevelHeight(AIndex: Integer): Integer;
    function GetRootItem(AIndex: Integer): TcxTreeListCustomBandPlace;
    function GetRootItemCount: Integer;
    procedure SetLevelHeight(AIndex: Integer; AValue: Integer);
  protected
    function CalculateHeaderLineCount: Integer; override;
    function CalculateWidth: Integer; override;
    function GetItemByColumn(AColumn: TcxTreeListColumn): TcxTreeListColumnPlace; override;

    procedure AddItems; virtual;
    function CreateItem(ABand: TcxTreeListBand): TcxTreeListCustomBandPlace; virtual;
    function GetItemClass(ABand: TcxTreeListBand): TcxTreeListCustomBandPlaceClass; virtual;
    procedure RefreshBottomItems;
    procedure RefreshItems;
    procedure RefreshRootItems;

    function CalculateHeight: Integer; virtual;
    function CalculateItemHeight(AnItem: TcxTreeListCustomBandPlace): Integer; virtual;
    function CalculateItemLeftBound(AnItem: TcxTreeListCustomBandPlace): Integer; virtual;
    function CalculateItemTopBound(AnItem: TcxTreeListCustomBandPlace): Integer; virtual;
    function CalculateLevelCount: Integer; virtual;

    procedure CalculateItemWidths;
    procedure CalculateLevelHeights;

    property AutoWidth: Boolean read GetAutoWidth;
    property AvailableWidth: Integer read GetAvailableWidth;
  public
    constructor Create(AFormatter: TcxTreeListReportLinkFormatter); override;
    destructor Destroy; override;

    procedure Calculate; override;
    procedure Refresh; override;

    function BottomIndexOf(AnItem: TcxTreeListCustomBandPlace): Integer;
    function IndexOf(AnItem: TcxTreeListCustomBandPlace): Integer; overload;
    function IndexOf(ABand: TcxTreeListBand): Integer; overload;
    function RootIndexOf(AnItem: TcxTreeListCustomBandPlace): Integer; overload;
    function RootIndexOf(ABand: TcxTreeListBand): Integer; overload;

    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItems[Index: Integer]: TcxTreeListCustomBandPlace read GetBottomItem;
    property Height: Integer read GetHeight;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TcxTreeListCustomBandPlace read GetItem; default;
    property ItemsByBand[Band: TcxTreeListBand]: TcxTreeListCustomBandPlace read GetItemByBand;
    property LevelCount: Integer read GetLevelCount;
    property LevelHeights[Index: Integer]: Integer read GetLevelHeight write SetLevelHeight;
    property RootItemCount: Integer read GetRootItemCount;
    property RootItems[Index: Integer]: TcxTreeListCustomBandPlace read GetRootItem;
  end;

  TcxTreeListReportLinkSummaryItems = array of array of TcxTreeListSummaryItem;

  TcxTreeListReportLinkLevelInfo = class
  private
    FImages: TCustomImageList;
    FStateImages: TCustomImageList;
  end;

  TcxTreeListReportLinkFormatter = class(TcxTreeListReportLinkBuilderHandler,
    IUnknown, IdxPSCellParams, IdxPSCellParams2)
  private
    FBands: TList;
    FBandLineHeight: Integer;
    FColumns: TList;
    FDetailsLineHeight: Integer;
    FGroupFooterSummaryItems: TcxTreeListReportLinkSummaryItems;
    FItemPlaceController: TcxTreeListItemPlaceController;
    FFont: TFont;
    FFooterLineHeight: Integer;
    FHeaderLineHeight: Integer;
    FLevelInfos: TObjectList;
    FLookAndFeelItems: TList;
    FExpansionLevel: Integer;
    FMaxNodeImageHeight: Integer;
    FNodes: TList;
    FPreviewLineHeight: Integer;
    FFooterSummaryItems: TcxTreeListReportLinkSummaryItems;
    FTransparentColor: TColor;
    function GetAutoWidth: Boolean;
    function GetAvailableWidth: Integer;
    function GetBandCount: Integer;
    function GetBands(AIndex: Integer): TcxTreeListBand;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetColumnCount: Integer;
    function GetColumns(AIndex: Integer): TcxTreeListColumn;
    function GetExpandButtonColor: TColor;
    function GetExpandButtonSize: Integer;
    function GetGridLinesColor: TColor;
    function GetHasNodeSeparator: Boolean;
    function GetHasPreview: Boolean;
    function GetIndentCount(Node: TcxTreeListNode): Integer;
    function GetImages(ALevel: Integer): TCustomImageList;
    function GetImagesWidth(ANode: TcxTreeListNode): Integer;
    function GetIsNodeColorUsedForIndents: Boolean;
    function GetLevelInfos(ALevel: Integer): TcxTreeListReportLinkLevelInfo;
    function GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
    function GetLookAndFeelItemCount: Integer;
    function GetNode(Index: Integer): TcxTreeListNode;
    function GetNodeCount: Integer;
    function GetNodeSeparatorColor: TColor;
    function GetNodeSeparatorThickness: Integer;
    function GetPreviewAutoHeight: Boolean;
    function GetPreviewColumn: TcxTreeListColumn;
    function GetPreviewMaxLineCount: Integer;
    function GetRenderer: TdxPSReportRenderer;
    function GetShowBandHeaders: Boolean;
    function GetShowFooters: Boolean;
    function GetShowHeaders: Boolean;
    function GetShowPreview: Boolean;
    function GetStateImages(ALevel: Integer): TCustomImageList;
    function GetUseLookAndFeelColors: Boolean;
    function GetViewWidth: Integer;
    function GetTreeLinesColor: TColor;
    function GetTreeLinesStyle: TcxTreeListTreeLineStyle;
    function GetStateImagesWidth(ANode: TcxTreeListNode): Integer;
    function HasImages(ANode: TcxTreeListNode): Boolean;
    function HasStateImages(ANode: TcxTreeListNode): Boolean;

    procedure FormatLookAndFeelItems;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
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

    procedure AddDelimiters; virtual;
    procedure AddHorizontalDelimiters; virtual;
    procedure AddVerticalDelimiters; virtual;
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;
    procedure CalculateLineHeights; virtual;
    function CanProcessSelectionStyle(ANode: TcxTreeListNode): Boolean;
    procedure CreateLevelInfos;
    procedure CreateNodeList; virtual;
    function IsSelectedNode(ANode: TcxTreeListNode): Boolean; virtual;

    function GetBackgroundBitmap(Index: Integer): TBitmap;
    function GetBackgroundBitmapIndex(Index: Integer): Integer;
    function HasBackgroundBitmap(Index: Integer): Boolean;
    function MapStyleBackgroundBitmapIndex(ATreeListBackgroundBitmapIndex: Integer): Integer;

    procedure CalculateHeight(const AParams: TdxReportItemViewParams; var AHeight: Integer);
    function CalculatePatternHeight(const AParams: TdxReportItemViewParams): Integer;
    procedure CreateBands;
    procedure CreateColumns;
    procedure CreateItems;
    procedure CreateSummaryItems;
    function GetFooterCellCount(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Integer;
    function GetNodeIndent(ANode: TcxTreeListNode): Integer;
    function GetNodeParent(ANode: TcxTreeListNode; ALevel: Integer): TcxTreeListNode;
    function GetNextNode(ANode: TcxTreeListNode): TcxTreeListNode;
    function GetPrevNode(ANode: TcxTreeListNode): TcxTreeListNode;
    function GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
    function GetSummaryItems(ANode: TcxTreeListNode): TcxTreeListReportLinkSummaryItems;
    function HasFooterCell(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Boolean;
    function HasParent(ANode: TcxTreeListNode): Boolean;
    function IndexOfNode(ANode: TcxTreeListNode): Integer;
    function IsColorTransparent(AColor: TColor): Boolean;
    function IsFirstNode(ANode: TcxTreeListNode): Boolean;
    function IsLastNode(ANode: TcxTreeListNode): Boolean;
    function IsNodeExpanded(ANode: TcxTreeListNode): Boolean;
    function IsNodeFirstChild(ANode: TcxTreeListNode): Boolean;
    function IsNodeLastChild(ANode: TcxTreeListNode): Boolean;
    function IsOddNode(ANode: TcxTreeListNode): Boolean;
    function IsNodeSelected(ANode: TcxTreeListNode): Boolean;
    procedure RegisterLookAndFeelItem(AnItem: TdxReportVisualItem; AEdgeStyle: TdxCellEdgeStyle);
    procedure SetViewParams(AnItem: TdxReportVisualItem; const AParams: TdxReportItemViewParams);

    function MakeIndentIndex(AnIndex: Integer): DWORD;

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property ItemPlaceController: TcxTreeListItemPlaceController read FItemPlaceController;
    property LevelInfos[ALevel: Integer]: TcxTreeListReportLinkLevelInfo read GetLevelInfos;
    property LookAndFeelItemCount: Integer read GetLookAndFeelItemCount;
    property LookAndFeelItems[Index: Integer]: TdxReportVisualItem read GetLookAndFeelItem;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property ViewWidth: Integer read GetViewWidth;
  public
    constructor Create(ABuilder: TcxTreeListReportLinkBuilder); override;
    destructor Destroy; override;
    function Adapter: TcxTreeListAdapter; overload; virtual;

    procedure DoInitializeHost(AHost: TdxReportCell); virtual;
    { Bands }
    procedure DoInitializeBandItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    procedure DoInitializeBandRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeBandItem(AnItem: TAbstractdxReportCellData; AIndex: Integer); virtual;
    function GetBandBackgroundViewParams: TdxReportItemViewParams; virtual;
    function GetBandItemClass(Index: Integer): TdxReportCellTextClass; virtual;
    function GetBandItemBounds(Index: Integer): TRect;
    function GetBandItemViewParams(ABand: TcxTreeListBand): TdxReportItemViewParams; virtual;
    { Checks }
    procedure DoInitializeNodeCheck(AnItem: TdxCustomReportCellCheck;
      ANode: TcxTreeListNode; AnIndex: Integer); virtual;
    function GetNodeCheckCellSides(ANode: TcxTreeListNode; AnIndex: Integer): TdxCellSides; virtual;
    function GetNodeCheckClass(ANode: TcxTreeListNode): TdxCustomReportCellCheckClass; virtual;
    function GetNodeCheckViewParams(ANode: TcxTreeListNode; AnIndex: Integer): TdxReportItemViewParams; virtual;
    { Footers }
    procedure DoInitializeFooterCell(AnItem: TAbstractdxReportCellData;
      AColumn: TcxTreeListColumn; ACellIndex: Integer; ANode: TcxTreeListNode); virtual;
    procedure DoInitializeFooterIndent(AnItem: TdxReportCellExpandButton;
      AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode);
    procedure DoInitializeFooterBandedRow(ARow: TdxReportCell; AIndentCount: Integer); virtual;
    procedure DoInitializeFooterRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeFooterCell(AnItem: TAbstractdxReportCellData;
      AColumnIndex, ACellIndex: Integer); virtual;
    function GetFooterCellBounds(AColumn: TcxTreeListColumn; ACellIndex, AIndent: Integer): TRect; virtual;
    function GetFooterCellClass: TdxReportCellTextClass; virtual;
    function GetFooterCellViewParams(AColumn: TcxTreeListColumn): TdxReportItemViewParams; virtual;
    function GetNodeFooterIndentTreeLineMode(AnIndex, AnIndentCount: Integer;
      ANode: TcxTreeListNode): TdxPSTreeLineMode;
    function GetFooterRowViewParams: TdxReportItemViewParams; virtual;
    { Headers }
    procedure DoInitializeHeaderItem(AnItem: TAbstractdxReportCellData; AnIndex: Integer); virtual;
    procedure DoInitializeHeaderRow(ARow: TdxReportCell); virtual;
    procedure DoReportLinkInitializeHeaderItem(AnItem: TAbstractdxReportCellData; AIndex: Integer); virtual;
    function GetHeaderItemBounds(AnIndex: Integer): TRect; virtual;
    function GetHeaderItemClass(AnIndex: Integer): TdxReportCellTextClass; virtual;
    function GetHeaderItemViewParams(AColumn: TcxTreeListColumn): TdxReportItemViewParams; virtual;
    { Images }
    procedure DoInitializeNodeImage(AnItem: TdxReportCellGraphic; ANode: TcxTreeListNode; AnIndex: Integer); virtual;
    procedure DoReportLinkInitializeNodeImage(AnItem: TAbstractdxReportCellData; ANode: TcxTreeListNode; AnIndex: Integer); virtual;
    function GetNodeImageCellSides(ANode: TcxTreeListNode; AnIndex: Integer): TdxCellSides;
    function GetNodeImageClass: TdxReportCellGraphicClass; virtual;
    function GetNodeImageViewParams(ANode: TcxTreeListNode; AnIndex: Integer): TdxReportItemViewParams; virtual;
    { Indents and Expand Buttons }
    procedure DoInitializeNodeIndent(AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode); virtual;
    procedure DoInitializeExpandButton(AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode); virtual;
    procedure DoReportLinkInitializeNodeIndent(AnItem: TAbstractdxReportCellData; AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode); virtual;
    function GetNodeIndentCellSides(ANode: TcxTreeListNode; AnIndex, AnIndentCount: Integer): TdxCellSides; virtual;
    function GetNodeIndentClass: TdxReportCellExpandButtonClass; virtual;
    function GetNodeIndentTreeLineMode(AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode): TdxPSTreeLineMode;
    function GetNodeIndentViewParams(ANode: TcxTreeListNode; AnIndex, AnIndentCount: Integer): TdxReportItemViewParams; virtual;
    { Nodes }
    procedure DoInitializeNodeBandedRow(ARow: TdxReportCell); virtual;
    procedure DoInitializeNodeRow(ARow: TdxReportCell; ANode: TcxTreeListNode); virtual;
    procedure DoInitializeNodeItem(AnItem: TAbstractdxReportCellData;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; AnIsPreview: Boolean = False); virtual;
    procedure DoReportLinkInitializeNodeItem(AnItem: TAbstractdxReportCellData;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode); virtual;
    function GetCellValue(AProperties: TcxCustomEditProperties;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TcxEditValue; virtual;
    function GetNodeBackgroundBitmapStyleIndex(ANode: TcxTreeListNode): Integer;
    function GetNodeItemBounds(ANode: TcxTreeListNode; AIndex: Integer): TRect; virtual;
    function GetNodeItemCellSides(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxCellSides;
    function GetNodeItemClass(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode;
      AnIsPreview: Boolean = False): TdxReportCellDataClass; virtual;
    function GetNodeItemViewParams(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxReportItemViewParams; virtual;
    { Preview }
    procedure DoInitializePreview(AnItem: TAbstractdxReportCellData; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode); virtual;
    function GetPreviewCellSides(ANode: TcxTreeListNode): TdxCellSides; virtual;
    function GetPreviewClass(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxReportCellDataClass; virtual;
    function GetPreviewViewParams(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxReportItemViewParams; virtual;
    { Separators }
    procedure DoInitializeNodeSeparator(AnItem: TAbstractdxReportCellData); virtual;
    procedure DoInitializeNodeSeparatorRow(ARow: TdxReportCell); virtual;
    function GetNodeSeparatorClass: TdxReportCellBoxClass; virtual;
    { State images }
    procedure DoInitializeNodeStateImage(AnItem: TdxReportCellGraphic; ANode: TcxTreeListNode; AnIndex: Integer); virtual;
    procedure DoReportLinkInitializeNodeStateImage(AnItem: TAbstractdxReportCellData; ANode: TcxTreeListNode; AnIndex: Integer); virtual;
    function GetNodeStateImageCellSides(ANode: TcxTreeListNode; AnIndex: Integer): TdxCellSides; virtual;
    function GetNodeStateImageClass: TdxReportCellGraphicClass; virtual;
    function GetNodeStateImageViewParams(ANode: TcxTreeListNode; AnIndex: Integer): TdxReportItemViewParams; virtual;

    property AutoWidth: Boolean read GetAutoWidth;
    property AvailableWidth: Integer read GetAvailableWidth;
    property BandLineHeight: Integer read FBandLineHeight write FBandLineHeight;
    property BandCount: Integer read GetBandCount;
    property Bands[Index: Integer]: TcxTreeListBand read GetBands;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumns;
    property DetailsLineHeight: Integer read FDetailsLineHeight write FDetailsLineHeight;
    property ExpandButtonColor: TColor read GetExpandButtonColor;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
    property FooterLineHeight: Integer read FFooterLineHeight write FFooterLineHeight;
    property GridLinesColor: TColor read GetGridLinesColor;
    property HasNodeSeparator: Boolean read GetHasNodeSeparator;
    property HasPreview: Boolean read GetHasPreview;
    property HeaderLineHeight: Integer read FHeaderLineHeight write FHeaderLineHeight;
    property Images[ALevel: Integer]: TCustomImageList read GetImages;
    property IndentCounts[Node: TcxTreeListNode]: Integer read GetIndentCount;
    property IsNodeColorUsedForIndents: Boolean read GetIsNodeColorUsedForIndents;
    property NodeCount: Integer read GetNodeCount;
    property Nodes[Index: Integer]: TcxTreeListNode read GetNode;
    property NodeSeparatorColor: TColor read GetNodeSeparatorColor;
    property NodeSeparatorThickness: Integer read GetNodeSeparatorThickness;
    property PreviewAutoHeight: Boolean read GetPreviewAutoHeight;
    property PreviewColumn: TcxTreeListColumn read GetPreviewColumn;
    property PreviewLineHeight: Integer read FPreviewLineHeight write FPreviewLineHeight;
    property PreviewMaxLineCount: Integer read GetPreviewMaxLineCount;
    property ShowBandHeaders: Boolean read GetShowBandHeaders;
    property ShowFooters: Boolean read GetShowFooters;
    property ShowHeaders: Boolean read GetShowHeaders;
    property ShowPreview: Boolean read GetShowPreview;
    property StateImages[ALevel: Integer]: TCustomImageList read GetStateImages;
    property UseLookAndFeelColors: Boolean read GetUseLookAndFeelColors;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor default clWindow;
    property TreeLinesColor: TColor read GetTreeLinesColor;
    property TreeLinesStyle: TcxTreeListTreeLineStyle read GetTreeLinesStyle;
  end;

  { Options }

  TcxTreeListReportLinkOptionsExpanding = class(TdxCustomReportLinkOptionsExpanding)
  private
    FAutoExpandNodes: Boolean;
    FExplicitlyExpandNodes: Boolean;
    function GetReportLink: TcxTreeListCustomReportLink;
    procedure SetAutoExpandNodes(Value: Boolean);
    procedure SetExplicitlyExpandNodes(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property AutoExpandNodes: Boolean read FAutoExpandNodes write SetAutoExpandNodes default False;
    property ExplicitlyExpandNodes: Boolean read FExplicitlyExpandNodes write SetExplicitlyExpandNodes default False;
  end;

  TcxTreeListReportLinkOptionsFormatting = class(TdxCustomReportLinkOptionsFormatting)
  private
    FConsumeSelectionStyle: Boolean;
    FNodeSeparatorColor: TColor;
    FNodeSeparatorThickness: Integer;
    function GetActualNodeSeparatorColor: TColor;
    function GetReportLink: TcxTreeListCustomReportLink;
    procedure SetConsumeSelectionStyle(Value: Boolean);
    procedure SetNodeSeparatorColor(Value: TColor);
    procedure SetNodeSeparatorThickness(Value: Integer);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    property ActualNodeSeparatorColor: TColor read GetActualNodeSeparatorColor;
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property ConsumeSelectionStyle: Boolean read FConsumeSelectionStyle write SetConsumeSelectionStyle default False;
    property NodeSeparatorColor: TColor read FNodeSeparatorColor write SetNodeSeparatorColor default clDefault;
    property NodeSeparatorThickness: Integer read FNodeSeparatorThickness write SetNodeSeparatorThickness default 0;
    property UseLookAndFeelColors;
  end;

  TcxTreeListReportLinkOptionsOnEveryPage = class(TdxCustomTableControlReportLinkOptionsOnEveryPage)
  private
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property BandHeaders;
    property Footers;
    property Headers;
  end;

  TcxTreeListReportLinkOptionsPagination = class(TdxCustomTableControlReportLinkOptionsPagination)
  private
    function GetNode: Boolean;
    function GetReportLink: TcxTreeListCustomReportLink;
    procedure SetNode(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property Band;
    property Column;
    property Custom;
    property Node: Boolean read GetNode write SetNode default True;
  end;

  TcxTreeListReportLinkOptionsPreview = class(TdxCustomTableControlReportLinkOptionsPreview)
  private
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  end;

  TcxTreeListReportLinkOptionsRefinements = class(TdxCustomReportLinkOptionsRefinements)
  private
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  end;

  TcxTreeListReportLinkOptionsSelection = class(TdxCustomTableControlReportLinkOptionsSelection)
  private
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  end;

  TcxTreeListReportLinkOptionsSize = class(TdxCustomReportLinkOptionsSize)
  private
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property AutoWidth;
  end;

  TcxTreeListReportLinkOptionsView = class(TdxCustomTableControlReportLinkOptionsView)
  private
    FBorders: Boolean;
    FTreeLines: Boolean;
    function GetReportLink: TcxTreeListCustomReportLink;
    procedure SetBorders(Value: Boolean);
    procedure SetTreeLines(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property BandHeaders;
    property Borders: Boolean read FBorders write SetBorders default True;
    property ExpandButtons;
    property Footers;
    property Headers;
    property TreeLines: Boolean read FTreeLines write SetTreeLines default True;
  end;

  { TcxTreeListReportLinkStyles }

  TcxTreeListReportLinkStylesClass = class of TcxTreeListReportLinkStyles;

  TcxTreeListReportLinkStyles = class(TdxCustomReportLinkStyles)
  private
    FOnGetNodeIndentStyle: TcxTreeListGetNodeIndentStyleEvent;
    function GetReportLink: TcxTreeListCustomReportLink;
  protected
    function DesignerTabIndex: Integer; override;

    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    class function GetStyleCaption(AnIndex: Integer): string; override;
    function GetStyleIndexByCaption(const Caption: string): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure GetBandHeaderParams(ABand: TcxTreeListBand; out AParams: TcxViewParams); virtual;
    procedure GetColumnFooterParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      out AParams: TcxViewParams); virtual;
    procedure GetColumnHeaderParams(AColumn: TcxTreeListColumn; out AParams: TcxViewParams); virtual;
    procedure GetContentParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      out AParams: TcxViewParams); virtual;
    procedure GetFooterRowParams(out AParams: TcxViewParams); virtual;
    procedure GetImagesParams(ANode: TcxTreeListNode; out AParams: TcxViewParams); virtual;
    procedure GetIndentParams(ANode: TcxTreeListNode; AnIndent: Integer; out AParams: TcxViewParams); virtual;
    procedure GetPreviewParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      out AParams: TcxViewParams); virtual;
    procedure GetSelectionParams(out AParams: TcxViewParams); virtual;

    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  published
    property BandBackground: TcxStyle Index vspsTreeListBandBackground read GetValue write SetValue;
    property BandHeader: TcxStyle Index vspsTreeListBandHeader read GetValue write SetValue;
    property Content: TcxStyle Index vspsTreeListContent read GetValue write SetValue;
    property ContentEven: TcxStyle Index vspsTreeListContentEven read GetValue write SetValue;
    property ContentOdd: TcxStyle Index vspsTreeListContentOdd read GetValue write SetValue;
    property Footer: TcxStyle Index vspsTreeListFooter read GetValue write SetValue;
    property FooterRow: TcxStyle Index vspsTreeListFooterRow read GetValue write SetValue;
    property Header: TcxStyle Index vspsTreeListHeader read GetValue write SetValue;
    property Preview: TcxStyle Index vspsTreeListPreview read GetValue write SetValue;
    property Selection: TcxStyle Index vspsTreeListSelection read GetValue write SetValue;
    property StyleSheet;
    property OnGetNodeIndentStyle: TcxTreeListGetNodeIndentStyleEvent read FOnGetNodeIndentStyle write FOnGetNodeIndentStyle;
  end;

  TcxTreeListReportLinkStyleSheet = class(TdxCustomReportLinkStyleSheet)
  private
    function GetStylesValue: TcxTreeListReportLinkStyles;
    procedure SetStylesValue(Value: TcxTreeListReportLinkStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxTreeListReportLinkStyles read GetStylesValue write SetStylesValue;
  end;

  { Host Services }

  TcxTreeListAttributeHostInfo = class
  private
    FParent: TdxReportCell;
  public
    Origin: TPoint;
    procedure Initialize(AParent: TdxReportCell);
    property Parent: TdxReportCell read FParent;
  end;

  TcxTreeListAttributeHostInfoServicesClass = class of TcxTreeListAttributeHostInfoServices;

  TcxTreeListAttributeHostInfoServices = class
  private
    FPageDetailsHostInfo: TcxTreeListAttributeHostInfo;
    FPageFootersHostInfo: TcxTreeListAttributeHostInfo;
    FPageHeadersHostInfo: TcxTreeListAttributeHostInfo;
    FReportLink: TcxTreeListCustomReportLink;
    function GetArePageFootersAssigned: Boolean;
    function GetArePageHeadersAssigned: Boolean;
    function GetCanUseBandHeadersOnEveyPage: Boolean;
    function GetCanUseFootersOnEveryPage: Boolean;
    function GetCanUseHeadersOnEveryPage: Boolean;
    function GetIsInconsistentHeadersState: Boolean;
    function GetOptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage;
    function GetOptionsView: TcxTreeListReportLinkOptionsView;
    function GetPageDetails: TdxReportCell;
    function GetPageFooters: TdxReportCell;
    function GetPageHeaders: TdxReportCell;
  protected
    procedure CreateHostInfos;
    procedure DestroyHostInfos;

    function GetBandHeadersHostInfo: TcxTreeListAttributeHostInfo; virtual;
    function GetFootersHostInfo: TcxTreeListAttributeHostInfo; virtual;
    function GetHeadersHostInfo: TcxTreeListAttributeHostInfo; virtual;
    function GetInconsistentStateText: string; virtual;
    function GetIsInconsistentState: Boolean; virtual;
    function HasCells: Boolean;

    property OptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage read GetOptionsOnEveryPage;
    property OptionsView: TcxTreeListReportLinkOptionsView read GetOptionsView;
    property PageDetails: TdxReportCell read GetPageDetails;
    property PageFooters: TdxReportCell read GetPageFooters;
    property PageHeaders: TdxReportCell read GetPageHeaders;
  public
    constructor Create(AReportLink: TcxTreeListCustomReportLink); virtual;
    destructor Destroy; override;
    procedure InitializeHostInfos;

    property ArePageFootersAssigned: Boolean read GetArePageFootersAssigned;
    property ArePageHeadersAssigned: Boolean read GetArePageHeadersAssigned;
    property BandHeadersHostInfo: TcxTreeListAttributeHostInfo read GetBandHeadersHostInfo;
    property CanUseBandHeadersOnEveyPage: Boolean read GetCanUseBandHeadersOnEveyPage;
    property CanUseFootersOnEveryPage: Boolean read GetCanUseFootersOnEveryPage;
    property CanUseHeadersOnEveryPage: Boolean read GetCanUseHeadersOnEveryPage;
    property FootersHostInfo: TcxTreeListAttributeHostInfo read GetFootersHostInfo;
    property HeadersHostInfo: TcxTreeListAttributeHostInfo read GetHeadersHostInfo;
    property InconsistentStateText: string read GetInconsistentStateText;
    property IsInconsistentHeadersState: Boolean read GetIsInconsistentHeadersState;
    property IsInconsistentState: Boolean read GetIsInconsistentState;
    property PageDetailsHostInfo: TcxTreeListAttributeHostInfo read FPageDetailsHostInfo;
    property PageFootersHostInfo: TcxTreeListAttributeHostInfo read FPageFootersHostInfo;
    property PageHeadersHostInfo: TcxTreeListAttributeHostInfo read FPageHeadersHostInfo;
    property ReportLink: TcxTreeListCustomReportLink read FReportLink;
  end;

  { TcxTreeListCustomReportLink }

  TcxTreeListNodeArray = array of TcxTreeListNode;
  TcxTreeListColumnArray = array of TcxTreeListColumn;

  TcxTreeListReportLinkGetCustomPageBreaksEvent = procedure(Sender: TcxTreeListCustomReportLink) of object;

  TcxTreeListReportLinkCustomDrawBandCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ACanvas: TCanvas; ABand: TcxTreeListBand; AnItem: TdxReportCellImage;
    var ADone: Boolean) of object;

  TcxTreeListReportLinkCustomDrawCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ACanvas: TCanvas; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
    AnItem: TAbstractdxReportCellData; var ADone: Boolean) of object;

  TcxTreeListReportLinkCustomDrawFooterCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ACanvas: TCanvas; AColumn: TcxTreeListColumn; AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TcxTreeListReportLinkCustomDrawHeaderCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ACanvas: TCanvas; AColumn: TcxTreeListColumn; AnItem: TdxReportCellImage; var ADone: Boolean) of object;

  TcxTreeListReportLinkCustomDrawIndentCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ACanvas: TCanvas; ANode: TcxTreeListNode; AnIndex: Integer; AnItem: TAbstractdxReportCellData;
    var ADone: Boolean) of object;

  TcxTreeListReportLinkInitializeBandCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ABand: TcxTreeListBand; AnItem: TdxReportCellImage) of object;

  TcxTreeListReportLinkInitializeCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; AnItem: TAbstractdxReportCellData) of object;

  TcxTreeListReportLinkInitializeFooterCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ASummaryCellIndex: Integer; AColumn: TcxTreeListColumn; AnItem: TdxReportCellString) of object;

  TcxTreeListReportLinkInitializeHeaderCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    AColumn: TcxTreeListColumn; AnItem: TdxReportCellImage) of object;

  TcxTreeListReportLinkInitializeIndentCellEvent = procedure(Sender: TcxTreeListCustomReportLink;
    ANode: TcxTreeListNode; AnIndex: Integer; AnItem: TAbstractdxReportCellData) of object;

  TcxTreeListCustomReportLink = class(TdxCustomTableControlReportLink)
  private
    FBuilder: TcxTreeListReportLinkBuilder;
    FDelimitersHardHorz: TList;
    FDelimitersHardVert: TList;
    FHostInfoServices: TcxTreeListAttributeHostInfoServices;
    FReportRows: TList;
    FOnCustomDrawBandCell: TcxTreeListReportLinkCustomDrawBandCellEvent;
    FOnCustomDrawCell: TcxTreeListReportLinkCustomDrawCellEvent;
    FOnCustomDrawFooterCell: TcxTreeListReportLinkCustomDrawFooterCellEvent;
    FOnCustomDrawHeaderCell: TcxTreeListReportLinkCustomDrawHeaderCellEvent;
    FOnCustomDrawIndentCell: TcxTreeListReportLinkCustomDrawIndentCellEvent;
    FOnGetCustomPageBreaks: TcxTreeListReportLinkGetCustomPageBreaksEvent;
    FOnInitializeBandCell: TcxTreeListReportLinkInitializeBandCellEvent;
    FOnInitializeCell: TcxTreeListReportLinkInitializeCellEvent;
    FOnInitializeFooterCell: TcxTreeListReportLinkInitializeFooterCellEvent;
    FOnInitializeHeaderCell: TcxTreeListReportLinkInitializeHeaderCellEvent;
    FOnInitializeIndentCell: TcxTreeListReportLinkInitializeIndentCellEvent;
    function GetActiveStyles: TcxTreeListReportLinkStyles;
    function GetDesignWindow: TdxfmTreeListReportLinkDesignWindow;
    function GetOptionsExpanding: TcxTreeListReportLinkOptionsExpanding;
    function GetOptionsFormatting: TcxTreeListReportLinkOptionsFormatting;
    function GetOptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage;
    function GetOptionsPagination: TcxTreeListReportLinkOptionsPagination;
    function GetOptionsPreview: TcxTreeListReportLinkOptionsPreview;
    function GetOptionsRefinements: TcxTreeListReportLinkOptionsRefinements;
    function GetOptionsSelection: TcxTreeListReportLinkOptionsSelection;
    function GetOptionsSize: TcxTreeListReportLinkOptionsSize;
    function GetOptionsView: TcxTreeListReportLinkOptionsView;
    function GetReportCellDataByColumn(AColumn: TcxTreeListColumn): TAbstractdxReportCellData;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowByNode(Node: TcxTreeListNode): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetStyles: TcxTreeListReportLinkStyles;
    function GetTreeList: TcxCustomTreeList;
    procedure SetOnCustomDrawBandCell(Value: TcxTreeListReportLinkCustomDrawBandCellEvent);
    procedure SetOnCustomDrawCell(Value: TcxTreeListReportLinkCustomDrawCellEvent);
    procedure SetOnCustomDrawFooterCell(Value: TcxTreeListReportLinkCustomDrawFooterCellEvent);
    procedure SetOnCustomDrawHeaderCell(Value: TcxTreeListReportLinkCustomDrawHeaderCellEvent);
    procedure SetOnCustomDrawIndentCell(Value: TcxTreeListReportLinkCustomDrawIndentCellEvent);
    procedure SetOptionsExpanding(Value: TcxTreeListReportLinkOptionsExpanding);
    procedure SetOptionsFormatting(Value: TcxTreeListReportLinkOptionsFormatting);
    procedure SetOptionsOnEveryPage(Value: TcxTreeListReportLinkOptionsOnEveryPage);
    procedure SetOptionsPagination(Value: TcxTreeListReportLinkOptionsPagination);
    procedure SetOptionsPreview(Value: TcxTreeListReportLinkOptionsPreview);
    procedure SetOptionsRefinements(Value: TcxTreeListReportLinkOptionsRefinements);
    procedure SetOptionsSelection(Value: TcxTreeListReportLinkOptionsSelection);
    procedure SetOptionsSize(Value: TcxTreeListReportLinkOptionsSize);
    procedure SetOptionsView(Value: TcxTreeListReportLinkOptionsView);
    procedure SetStyles(Value: TcxTreeListReportLinkStyles);
  protected
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConvertCoords; override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    function GetBreakPagesByHardDelimiters: Boolean; override;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    function GetUseHardVertDelimiters: Boolean; override;
    procedure InternalRestoreFromOriginal; override;
    function IsDrawFootersOnEveryPage: Boolean; override;
    function IsDrawHeadersOnEveryPage: Boolean; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeHardDelimiters(AReportCells: TdxReportCells;
      AHorzDelimiters, AVertDelimiters: TList); override;

    function GetAreNativeStylesAvailable: Boolean; override;
    function GetStylesClass: TdxCustomReportLinkStylesClass; override;
    function GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass; override;
    function GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet; override;
    procedure PrepareConstruct; override;

    procedure DoCustomDrawBandCell(ACanvas: TCanvas; ABand: TcxTreeListBand;
      AnItem: TdxReportCellImage; var ADone: Boolean); dynamic;
    procedure DoCustomDrawCell(ACanvas: TCanvas; ANode: TcxTreeListNode;
      AColumn: TcxTreeListColumn; AnItem: TAbstractdxReportCellData;
      var ADone: Boolean); dynamic;
    procedure DoCustomDrawFooterCell(ACanvas: TCanvas; AColumn: TcxTreeListColumn;
      AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawHeaderCell(ACanvas: TCanvas; AColumn: TcxTreeListColumn;
      AnItem: TdxReportCellImage; var ADone: Boolean); dynamic;
    procedure DoCustomDrawIndentCell(ACanvas: TCanvas; ANode: TcxTreeListNode;
      AnIndex: Integer; AnItem: TAbstractdxReportCellData; var ADone: Boolean); dynamic;
    procedure DoGetCustomPageBreaks; dynamic;
    procedure DoInitializeBandCell(ABand: TcxTreeListBand; AnItem: TdxReportCellImage); dynamic;
    procedure DoInitializeCell(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; AnItem: TAbstractdxReportCellData); dynamic;
    procedure DoInitializeFooterCell( AColumn: TcxTreeListColumn; ASummaryCellIndex: Integer; AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeHeaderCell(AColumn: TcxTreeListColumn; AnItem: TdxReportCellImage); dynamic;
    procedure DoInitializeIndentCell(ANode: TcxTreeListNode; AnIndex: Integer; AnItem: TAbstractdxReportCellData); dynamic;

    function GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass; override;
    function GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass; override;
    function GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass; override;
    function GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass; override;
    function GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass; override;
    function GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass; override;
    function GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass; override;
    function GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass; override;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; override;

    procedure AddReportRow(AReportRow: TdxReportCell);
    procedure AddHorizontalHardDelimiter(ADelimiter: Integer);
    procedure AddVerticalHardDelimiter(ADelimiter: TdxReportCell); overload;
    procedure AddVerticalHardDelimiter(ADelimiter: Integer); overload;

    function CreateBuilder: TcxTreeListReportLinkBuilder; virtual;
    class function GetBuilderClass: TcxTreeListReportLinkBuilderClass; virtual;

    function ExtractIndentIndex(AData: Integer): Integer;
    function GetItemCustomDrawInfo(AnItem: TdxReportVisualItem;
       out ADrawInfo: TcxTreeListCellCustomDrawInfo): TcxTreeListAttributeID; virtual;
    function IsCustomDrawn(AnAttributeID: TcxTreeListAttributeID): Boolean; virtual;
    function MakeIndentIndex(AnIndex: Integer): DWORD;

    property ActiveStyles: TcxTreeListReportLinkStyles read GetActiveStyles;
    property Builder: TcxTreeListReportLinkBuilder read FBuilder;
    property DelimitersHardHorz: TList read FDelimitersHardHorz;
    property DelimitersHardVert: TList read FDelimitersHardVert;
    property HostInfoServices: TcxTreeListAttributeHostInfoServices read FHostInfoServices;
    property TreeList: TcxCustomTreeList read GetTreeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddHorizontalPageBreak(AColumn: TcxTreeListColumn); overload;
    procedure AddHorizontalPageBreak(const AColumns: array of TcxTreeListColumn); overload;
    procedure AddHorizontalPageBreak(const AColumns: TcxTreeListColumnArray); overload;
    procedure AddHorizontalPageBreak(AColumns: TList); overload; // List of TcxTreeListColumn

    procedure AddPageBreak(ANode: TcxTreeListNode); overload;
    procedure AddPageBreak(const ANodes: array of TcxTreeListNode); overload;
    procedure AddPageBreak(const ANodes: TcxTreeListNodeArray); overload;
    procedure AddPageBreak(ANodes: TList); overload; // List of TcxTreeListNode

    property DesignWindow: TdxfmTreeListReportLinkDesignWindow read GetDesignWindow;
    property ReportCellDataByColumn[AColumn: TcxTreeListColumn]: TAbstractdxReportCellData read GetReportCellDataByColumn;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property ReportRowsByNode[Node: TcxTreeListNode]: TdxReportCell read GetReportRowByNode;
  published
    property Color;
    property Font;
    property OptionsExpanding: TcxTreeListReportLinkOptionsExpanding read GetOptionsExpanding write SetOptionsExpanding;
    property OptionsFormatting: TcxTreeListReportLinkOptionsFormatting read GetOptionsFormatting write SetOptionsFormatting;
    property OptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage read GetOptionsOnEveryPage write SetOptionsOnEveryPage;
    property OptionsPagination: TcxTreeListReportLinkOptionsPagination read GetOptionsPagination write SetOptionsPagination;
    property OptionsPreview: TcxTreeListReportLinkOptionsPreview read GetOptionsPreview write SetOptionsPreview;
    property OptionsRefinements: TcxTreeListReportLinkOptionsRefinements read GetOptionsRefinements write SetOptionsRefinements;
    property OptionsSelection: TcxTreeListReportLinkOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsSize: TcxTreeListReportLinkOptionsSize read GetOptionsSize write SetOptionsSize;
    property OptionsView: TcxTreeListReportLinkOptionsView read GetOptionsView write SetOptionsView;
    property ScaleFonts;
    property StyleRepository;
    property Styles: TcxTreeListReportLinkStyles read GetStyles write SetStyles;
    property SupportedCustomDraw;

    property OnCustomDrawBandCell: TcxTreeListReportLinkCustomDrawBandCellEvent read FOnCustomDrawBandCell
      write SetOnCustomDrawBandCell;
    property OnCustomDrawCell: TcxTreeListReportLinkCustomDrawCellEvent read FOnCustomDrawCell
      write SetOnCustomDrawCell;
    property OnCustomDrawFooterCell: TcxTreeListReportLinkCustomDrawFooterCellEvent read FOnCustomDrawFooterCell
      write SetOnCustomDrawFooterCell;
    property OnCustomDrawHeaderCell: TcxTreeListReportLinkCustomDrawHeaderCellEvent read FOnCustomDrawHeaderCell
      write SetOnCustomDrawHeaderCell;
    property OnCustomDrawIndentCell: TcxTreeListReportLinkCustomDrawIndentCellEvent read FOnCustomDrawIndentCell
      write SetOnCustomDrawIndentCell;
    property OnGetCustomPageBreaks: TcxTreeListReportLinkGetCustomPageBreaksEvent read FOnGetCustomPageBreaks write FOnGetCustomPageBreaks;
    property OnInitializeBandCell: TcxTreeListReportLinkInitializeBandCellEvent read FOnInitializeBandCell
      write FOnInitializeBandCell;
    property OnInitializeCell: TcxTreeListReportLinkInitializeCellEvent read FOnInitializeCell
      write FOnInitializeCell;
    property OnInitializeFooterCell: TcxTreeListReportLinkInitializeFooterCellEvent read FOnInitializeFooterCell
      write FOnInitializeFooterCell;
    property OnInitializeHeaderCell: TcxTreeListReportLinkInitializeHeaderCellEvent read FOnInitializeHeaderCell
      write FOnInitializeHeaderCell;
    property OnInitializeIndentCell: TcxTreeListReportLinkInitializeIndentCellEvent read FOnInitializeIndentCell
      Write FOnInitializeIndentCell;
  end;

  TcxTreeListReportLink = class(TcxTreeListCustomReportLink)
  private
    function GetTreeList: TcxTreeList;
  public
    property TreeList: TcxTreeList read GetTreeList;
  end;

  TcxDBTreeListReportLink = class(TcxTreeListCustomReportLink)
  private
    function GetDBTreeList: TcxDBTreeList;
  public
    property DBTreeList: TcxDBTreeList read GetDBTreeList;
  end;

  TcxVirtualTreeListReportLink = class(TcxTreeListCustomReportLink)
  private
    function GetVirtualTreeList: TcxVirtualTreeList;
  public
    property VirtualTreeList: TcxVirtualTreeList read GetVirtualTreeList;
  end;

  TdxfmTreeListReportLinkDesignWindow = class(TdxfmCustomcxControlReportLinkDesignWindow)
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
    ccbxSeparatorColor: TcxColorComboBox;
    chbxAutoWidth: TcxCheckBox;
    chbxBandsOnEveryPage: TcxCheckBox;
    chbxConsumeSelectionStyle: TcxCheckBox;
    chbxDisplayGraphicsAsText: TcxCheckBox;
    chbxDisplayTrackBarsAsText: TcxCheckBox;
    chbxExpandNodes: TcxCheckBox;
    chbxExplicitlyExpandNodes: TcxCheckBox;
    chbxFlatCheckMarks: TcxCheckBox;
    chbxFootersOnEveryPage: TcxCheckBox;
    chbxHeadersOnEveryPage: TcxCheckBox;
    chbxPreviewAutoHeight: TcxCheckBox;
    chbxPreviewVisible: TcxCheckBox;
    chbxProcessExactSelection: TcxCheckBox;
    chbxProcessSelection: TcxCheckBox;
    chbxShowBands: TcxCheckBox;
    chbxShowBorders: TcxCheckBox;
    chbxShowExpandButtons: TcxCheckBox;
    chbxShowFooters: TcxCheckBox;
    chbxShowHeaders: TcxCheckBox;
    chbxShowTreeLines: TcxCheckBox;
    chbxSuppressBackgroundBitmaps: TcxCheckBox;
    chbxTransparentGraphics: TcxCheckBox;
    chbxUseNativeStyles: TcxCheckBox;
    colCarIsSUV: TcxTreeListColumn;
    colCarModel: TcxTreeListColumn;
    colManufacturerCountry: TcxTreeListColumn;
    colManufacturerLogo: TcxTreeListColumn;
    colManufacturerName: TcxTreeListColumn;
    colSpeedCount: TcxTreeListColumn;
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
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
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
    dxLayoutItem27: TdxLayoutItem;
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
    dxLayoutItem49: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem50: TdxLayoutItem;
    dxLayoutItem51: TdxLayoutItem;
    dxLayoutItem52: TdxLayoutItem;
    dxLayoutItem53: TdxLayoutItem;
    dxLayoutItem54: TdxLayoutItem;
    dxLayoutItem55: TdxLayoutItem;
    dxLayoutItem56: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    ilStylesPopup: TcxImageList;
    imgExpanding: TcxImage;
    imgGridSize: TcxImage;
    imgLookAndFeel: TcxImage;
    imgOnEveryPage: TcxImage;
    imgPreview: TcxImage;
    imgRefinements: TcxImage;
    imgSelection: TcxImage;
    imgSeparators: TcxImage;
    imgShow: TcxImage;
    lblExpanding: TcxLabel;
    lblLookAndFeel: TcxLabel;
    lblOnEveryPage: TcxLabel;
    lblPreviewMaxLineCount: TdxLayoutItem;
    lblPreviewOptions: TcxLabel;
    lblPreviewWindow: TdxLayoutItem;
    lblRefinements: TcxLabel;
    lblSelection: TcxLabel;
    lblSeparators: TcxLabel;
    lblSeparatorsColor: TdxLayoutItem;
    lblSeparatorsThickness: TdxLayoutItem;
    lblShow: TcxLabel;
    lblSize: TcxLabel;
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
    PreviewTreeList: TcxTreeList;
    sePreviewMaxLineCount: TcxSpinEdit;
    seSeparatorThickness: TcxSpinEdit;
    styleBandHeaders: TcxStyle;
    styleCardShadow: TcxStyle;
    stylePreview: TcxStyle;
    styleStandard: TcxStyle;
    tshBehaviors: TdxLayoutGroup;
    tshFormatting: TdxLayoutGroup;
    tshPreview: TdxLayoutGroup;
    tshStyles: TdxLayoutGroup;
    tshView: TdxLayoutGroup;

    procedure btnStyleSheetCopyClick(Sender: TObject);
    procedure btnStyleSheetDeleteClick(Sender: TObject);
    procedure btnStyleSheetNewClick(Sender: TObject);
    procedure btnStyleSheetRenameClick(Sender: TObject);
    procedure cbxStyleSheetsClick(Sender: TObject);
    procedure cbxStyleSheetsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxStyleSheetsPropertiesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure lblUseNativeStylesClick(Sender: TObject);
    procedure lbxStylesClick(Sender: TObject);
    procedure LookAndFeelChange(Sender: TObject);
    procedure miStylesSelectAllClick(Sender: TObject);
    procedure OptionsExpandingClick(Sender: TObject);
    procedure OptionsFormattingClick(Sender: TObject);
    procedure OptionsOnEveryPageClick(Sender: TObject);
    procedure OptionsRefinementsClick(Sender: TObject);
    procedure OptionsSelectionClick(Sender: TObject);
    procedure OptionsSizeClick(Sender: TObject);
    procedure OptionsViewClick(Sender: TObject);
    procedure pmStylesPopup(Sender: TObject);
    procedure PreviewAutoHeightClick(Sender: TObject);
    procedure PreviewMaxLineCountChanged(Sender: TObject);
    procedure PreviewTreeListCustomDrawBandHeaderCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListHeaderCellViewInfo; var ADone: Boolean);
    procedure PreviewTreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure PreviewTreeListCustomDrawFooterCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListFooterCellViewInfo; var ADone: Boolean);
    procedure PreviewTreeListCustomDrawHeaderCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListHeaderCellViewInfo; var ADone: Boolean);
    procedure PreviewVisibleClick(Sender: TObject);
    procedure SeparatorColorChanged(Sender: TObject);
    procedure SeparatorThicknessChanged(Sender: TObject);
    procedure StyleBackgroundBitmapClearClick(Sender: TObject);
    procedure StyleBackgroundBitmapClick(Sender: TObject);
    procedure StyleColorClick(Sender: TObject);
    procedure StyleFontClick(Sender: TObject);
    procedure StyleRestoreDefaultsClick(Sender: TObject);
    procedure StylesSaveAsClick(Sender: TObject);
  private
    lbxStyles: TdxStylesListBox;
    wpIncorrectOnEveryPageState: TdxPSWarningPane;

    function GetActiveStyle: TcxStyle;
    function GetHasSelectedStyles: Boolean;
    function GetHasSelectedStylesWithAssignedBitmap: Boolean;
    function GetReportLink: TcxTreeListCustomReportLink;

    function CanSelectAllStyles: Boolean;
    procedure CreateControls;
    procedure CustomDrawBorders(ACanvas: TcxCanvas; const R: TRect);
    procedure CustomDrawCheckMark(ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo);
    procedure CustomDrawColCarSpeedCount(ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo);
    procedure CustomDrawFooter(ACanvas: TcxCanvas; R: TRect; AViewInfo: TcxTreeListFooterCellViewInfo);
    procedure CustomDrawHeader(ACanvas: TcxCanvas; R: TRect; AViewInfo: TcxTreeListHeaderCellViewInfo);
    procedure CustomDrawTextRect(ACanvas: TcxCanvas; R: TRect; const AText: string;
      AnAlignmentHorz: TAlignment; AnAlignmentVert: TcxAlignmentVert; AParams: TcxViewParams);
    function ExtractAlignmentHorz(AViewInfo: TcxTreeListEditCellViewInfo): TAlignment;
    function ExtractAlignmentVert(AViewInfo: TcxTreeListEditCellViewInfo): TcxAlignmentVert;
    procedure InitializePreviewTreeList;
    procedure InitializePreviewTreeListStyles;
    procedure LoadDataIntoPreviewTreeList;
    procedure RecreateStylesListBox;
    procedure RestoreSelectedStyles(AList: TList);
    procedure SaveSelectedStyles(AList: TList);
    procedure SetActivePage;
    procedure UpdatePreviewTreeListStyles(const ACaption: string; AStyle: TcxStyle);
    procedure WarningPaneUpdate;

    procedure SetOptionsExpandingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsFormattingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsOnEveryPageByIndex(Index: Integer; Value: Boolean);
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
    property ReportLink: TcxTreeListCustomReportLink read GetReportLink;
  end;

const
  dxPSDefaultTreeListNodeSeparatorColor: TColor = clBtnShadow;

implementation

{$R *.dfm}

uses
  Variants, Forms, Registry, dxThemeManager, cxDataStorage, cxDataUtils, dxPSGlbl, dxPSRes, dxPSImgs,
  cxDrawTextUtils, dxPSPopupMan, dxPSUtl, dxBkgnd, Math, dxDPIAwareUtils, dxPSdxSpreadSheetLnkCore;

const
  CellSidesMap: array[TcxTreeListGridLines] of TdxCellSides = ([], csTopBottom, csLeftRight, csAll);
  SortOrderMap: array[TcxDataSortOrder] of TdxCellSortOrder = (csoNone, csoUp, csoDown);
  TreeLinesStyleMap: array[TcxTreeListTreeLineStyle] of TdxPSTreeLineStyle = (tlsDot, tlsDot, tlsSolid);

  DefaultDataRowLineHeight = 19;
  DefaultExpandButtonSize = 9;
  DefaultIndentWidth = 15;
  ExpandButtonInteriorColors: array[Boolean] of TColor = (clWindow, clBtnShadow);
  FooterItemInflateHorz = 2;
  FooterItemInflateVert = 3;

  NodeIndentMask   = $00000FF0;
  NodeIndentOffset = $00000004;

type
  TcxDataControllerConditionalFormattingProviderAccess = class(TcxDataControllerConditionalFormattingProvider);

  TcxCustomTreeListAccess = class(TcxCustomTreeList);
  TcxTreeListBandAccess = class(TcxTreeListBand);
  TcxTreeListColumnAccess = class(TcxTreeListColumn);
  TcxTreeListNodeAccess = class(TcxTreeListNode);
  TcxTreeListViewInfoAccess = class(TcxTreeListViewInfo);

  TcxTreeListNodeHelperFactory = class(TdxCustomClassMaps)
  private
    function GetHelperClass(Node: TcxTreeListNode): TcxTreeListNodeHelperClass;
  public
    class function Instance: TcxTreeListNodeHelperFactory; reintroduce; overload;
    property HelperClasses[Node: TcxTreeListNode]: TcxTreeListNodeHelperClass read GetHelperClass; default;
  end;

var
  FDefaultdxPScxTreeListLinkStyleSheet: TcxTreeListReportLinkStyleSheet;

{ Helpers }

{ CustomTreeList Helpers }

procedure CustomTreeList_DoGetLevelImages(AInstance: TcxCustomTreeList;
  ALevel: Integer; var AImages, AStateImages: TCustomImageList);
begin
  if Assigned(TcxCustomTreeListAccess(AInstance).OnGetLevelImages) then
    TcxCustomTreeListAccess(AInstance).OnGetLevelImages(AInstance, ALevel, AImages,
    AStateImages);
end;

function CustomTreeList_DoIsGroupNode(AInstance: TcxCustomTreeList; ANode: TcxTreeListNode): Boolean;
begin
  Result := TcxCustomTreeListAccess(AInstance).DoIsGroupNode(ANode);
end;

function CustomTreeList_GetImages(AInstance: TcxCustomTreeList): TCustomImageList;
begin
  Result := TcxCustomTreeListAccess(AInstance).Images;
end;

function CustomTreeList_GetLookAndFeel(AInstance: TcxCustomTreeList): TcxLookAndFeel;
begin
  Result := TcxCustomTreeListAccess(AInstance).LookAndFeel;
end;

function CustomTreeList_GetLookAndFeelPainter(AInstance: TcxCustomTreeList): TcxCustomLookAndFeelPainter;
begin
  Result := TcxCustomTreeListAccess(AInstance).LookAndFeelPainter;
end;

function CustomTreeList_GetOptionsSelection(AInstance: TcxCustomTreeList): TcxTreeListOptionsSelection;
begin
  Result := TcxCustomTreeListAccess(AInstance).OptionsSelection;
end;

function CustomTreeList_GetOptionsView(AInstance: TcxCustomTreeList): TcxTreeListOptionsView;
begin
  Result := TcxCustomTreeListAccess(AInstance).OptionsView;
end;

function CustomTreeList_GetPreview(AInstance: TcxCustomTreeList): TcxTreeListPreview;
begin
  Result := TcxCustomTreeListAccess(AInstance).Preview;
end;

function CustomTreeList_GetStateImages(AInstance: TcxCustomTreeList): TCustomImageList;
begin
  Result := TcxCustomTreeListAccess(AInstance).StateImages;
end;

function CustomTreeList_GetStyles(AInstance: TcxCustomTreeList): TcxTreeListStyles;
begin
  Result := TcxCustomTreeListAccess(AInstance).Styles;
end;

function CustomTreeList_GetVisibleBand(AInstance: TcxCustomTreeList; Index: Integer): TcxTreeListBand;
begin
  Result := TcxCustomTreeListAccess(AInstance).Bands.VisibleItems[Index];
end;

function CustomTreeList_GetVisibleBandCount(AInstance: TcxCustomTreeList): Integer;
begin
  Result := TcxCustomTreeListAccess(AInstance).Bands.VisibleItemCount;
end;

function CustomTreeList_GetVisibleColumn(AInstance: TcxCustomTreeList; Index: Integer): TcxTreeListColumn;
begin
  Result := TcxCustomTreeListAccess(AInstance).VisibleColumns[Index];
end;

function CustomTreeList_GetVisibleColumnCount(AInstance: TcxCustomTreeList): Integer;
begin
  Result := TcxCustomTreeListAccess(AInstance).VisibleColumnCount;
end;

{ TreeListColumn Helpers }

function TreeListColumn_DoGetEditProperties(AInstance: TcxTreeListColumn;
  ANode: TcxTreeListNode): TcxCustomEditProperties;
begin
  Result := TcxTreeListColumnAccess(AInstance).DoGetEditProperties(ANode);
end;

{ Utilities }

function DefaultdxPScxTreeListLinkStyleSheet: TcxTreeListReportLinkStyleSheet;

  function CreateStyle(AColor: TColor; AFontColor: TColor): TcxStyle;
  begin
    Result := TcxStyle.Create(DefaultdxPScxTreeListLinkStyleSheet);
    with Result do
    begin
      Color := AColor;
      Font.Name := dxPSCore.dxPSDefaultFontName;
      Font.Color := AFontColor;
    end;
  end;

begin
  if FDefaultdxPScxTreeListLinkStyleSheet = nil then
  begin
    FDefaultdxPScxTreeListLinkStyleSheet := TcxTreeListReportLinkStyleSheet.Create(nil);
    with FDefaultdxPScxTreeListLinkStyleSheet.Styles as TcxTreeListReportLinkStyles do
    begin
      BandHeader := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Content := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      ContentEven := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      ContentOdd := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      Footer := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      FooterRow := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Header := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Preview := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      Selection := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
    end;
  end;
  Result := FDefaultdxPScxTreeListLinkStyleSheet;
end;

{ TcxTreeListNodeHelperFactory }

function cxTreeListNodeHelperFactory: TcxTreeListNodeHelperFactory;
begin
  Result := TcxTreeListNodeHelperFactory.Instance;
end;

class function TcxTreeListNodeHelperFactory.Instance: TcxTreeListNodeHelperFactory;
begin
  Result := inherited Instance as TcxTreeListNodeHelperFactory;
end;

function TcxTreeListNodeHelperFactory.GetHelperClass(Node: TcxTreeListNode): TcxTreeListNodeHelperClass;
begin
  Result := TcxTreeListNodeHelperClass(PairClasses[Node.ClassType]);
end;

{ CLR Accessors }


{ TcxTreeListReportLinkRowProducer }

constructor TcxTreeListReportLinkRowProducer.Create(ABuilder: TcxTreeListReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxTreeListReportLinkRowProducer.Adapter: TcxTreeListAdapter;
begin
  Result := Builder.Adapter;
end;

function TcxTreeListReportLinkRowProducer.Builder: TcxTreeListReportLinkBuilder;
begin
  Result := FBuilder;
end;

function TcxTreeListReportLinkRowProducer.Formatter: TcxTreeListReportLinkFormatter;
begin
  Result := Builder.Formatter;
end;

function TcxTreeListReportLinkRowProducer.Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell;
begin
  CalculateRowHeight;

  CreateRowHost(AHostInfo);
  CreateRow;

  if AutoHeight then
  begin
    CalculateRowAutoHeight;
    FixupRowHeight;
  end;
  AfterCalculateRowHeight;

  Inc(AHostInfo.Origin.Y, RowHeight);
  Result := Host;
end;

procedure TcxTreeListReportLinkRowProducer.AfterCalculateRowHeight;
begin
end;

procedure TcxTreeListReportLinkRowProducer.CalculateRowAutoHeight;
var
  MaxRowHeight, I: Integer;
  Item: TdxReportVisualItem;
begin
  MaxRowHeight := 0;
  for I := 0 to Row.DataItemCount - 1 do
  begin
    Item := Row.DataItems[I];
    if DoesItemParticipateInRowAutoHeightCalculation(Item) then
      MaxRowHeight := Max(MaxRowHeight, Item.MeasureContentHeight(Canvas));
  end;
  // v3.1
  for I := 0 to Row.CellCount - 1 do
  begin
    Item := Row.Cells[I];
    if DoesItemParticipateInRowAutoHeightCalculation(Item) then
      MaxRowHeight := Max(MaxRowHeight, Item.MeasureContentHeight(Canvas));
  end;
  RowHeight := Max(RowHeight, MaxRowHeight);
end;

procedure TcxTreeListReportLinkRowProducer.CalculateRowHeight;
begin
  FRowHeight := LineCount * LineHeight;
end;

procedure TcxTreeListReportLinkRowProducer.CreateRow;
begin
  FRow := TdxReportCell.Create(Host);
  FRow.BoundsRect := Bounds(0, 0, RowWidth, RowHeight);
  InitializeRow;
end;

procedure TcxTreeListReportLinkRowProducer.CreateRowHost(const AHostInfo: TcxTreeListAttributeHostInfo);
begin
  FHost := TdxReportCell.Create(AHostInfo.Parent);
  FHost.BoundsRect := Bounds(AHostInfo.Origin.X, AHostInfo.Origin.Y, RowWidth, RowHeight);
  Formatter.DoInitializeHost(Host);
end;

function TcxTreeListReportLinkRowProducer.DoesItemParticipateInRowAutoHeightCalculation(
  AnItem: TdxReportVisualItem): Boolean;
begin
  Result := True;
end;

procedure TcxTreeListReportLinkRowProducer.FixupRowDataHeight;
var
  I: Integer;
begin
  for I := 0 to Row.DataItemCount - 1 do
    Row.DataItems[I].Height := RowHeight;
  // v3.1
  for I := 0 to Row.CellCount - 1 do
    Row.Cells[I].Height := RowHeight;
end;

procedure TcxTreeListReportLinkRowProducer.FixupRowHeight;
begin
  FixupRowItselfHeight;
  FixupRowDataHeight;
end;

procedure TcxTreeListReportLinkRowProducer.FixupRowItselfHeight;
begin
  Host.Height := RowHeight;
  Row.Height := RowHeight;
end;

procedure TcxTreeListReportLinkRowProducer.InitializeRow;
begin
end;

function TcxTreeListReportLinkRowProducer.GetAutoHeight: Boolean;
begin
  Result := LineCount = 1;
end;

function TcxTreeListReportLinkRowProducer.GetLineCount: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkRowProducer.GetLineHeight: Integer;
begin
  Result := 20;
end;

function TcxTreeListReportLinkRowProducer.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Formatter.Canvas;
end;

function TcxTreeListReportLinkRowProducer.GetIndentWidth: Integer;
begin
  Result := Adapter.IndentWidth;
end;

function TcxTreeListReportLinkRowProducer.GetRowWidth: Integer;
begin
  Result := Formatter.ViewWidth;
end;

{ TcxTreeListReportLinkNodeSeparatorProducer }

function TcxTreeListReportLinkNodeSeparatorProducer.Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell;
begin
  Result := inherited Produce(AHostInfo);
  FItem := GetItemClass.Create(Row);
  FItem.BoundsRect := Rect(0, 0, RowWidth, RowHeight);
  InitializeItem;
end;

procedure TcxTreeListReportLinkNodeSeparatorProducer.InitializeItem;
begin
  Formatter.DoInitializeNodeSeparator(FItem);
end;

procedure TcxTreeListReportLinkNodeSeparatorProducer.InitializeRow;
begin
  Formatter.DoInitializeNodeSeparatorRow(Row);
end;

function TcxTreeListReportLinkNodeSeparatorProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxTreeListReportLinkNodeSeparatorProducer.GetItemClass: TdxReportCellDataClass;
begin
  Result := Formatter.GetNodeSeparatorClass;
end;

function TcxTreeListReportLinkNodeSeparatorProducer.GetLineHeight: Integer;
begin
  Result := Formatter.NodeSeparatorThickness;
end;

{ TcxTreeListReportLinkRowSubItemsProducer }

procedure TcxTreeListReportLinkRowSubItemsProducer.CreateDataItems(AParent: TdxReportCell);
var
  I: Integer;
  AItem: TAbstractdxReportCellData;
begin
  for I := 0 to SubItemCount - 1 do
    if HasSubItem[I] then
    begin
      AItem := SubItemClasses[I].Create(AParent);
      AItem.BoundsRect := SubItemBounds[I];
      InitializeSubItem(AItem, I);
    end;
end;

procedure TcxTreeListReportLinkRowSubItemsProducer.CreateRow;
begin
  inherited CreateRow;
  CreateDataItems(Row);
end;

procedure TcxTreeListReportLinkRowSubItemsProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
end;

function TcxTreeListReportLinkRowSubItemsProducer.GetHasSubItem(Index: Integer): Boolean;
begin
  Result := True;
end;

function TcxTreeListReportLinkRowSubItemsProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Rect(0, 0, -1, -1);
end;

function TcxTreeListReportLinkRowSubItemsProducer.GetSubItemCount: Integer;
begin
  Result := Formatter.ColumnCount;
end;

function TcxTreeListReportLinkRowSubItemsProducer.GetColumn(Index: Integer): TcxTreeListColumn;
begin
  Result := Formatter.Columns[Index];
end;

{ TcxTreeListReportLinkBandsProducer }

function TcxTreeListReportLinkBandsProducer.GetSubItemCount: Integer;
begin
  Result := Formatter.BandCount;
end;

procedure TcxTreeListReportLinkBandsProducer.CalculateRowHeight;
begin
  FRowHeight := Formatter.ItemPlaceController.Height;
end;

procedure TcxTreeListReportLinkBandsProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeBandRow(Row);
end;

procedure TcxTreeListReportLinkBandsProducer.InitializeSubItem(ASubItem: TAbstractdxReportCellData;
  AnIndex: Integer);
begin
  inherited;
  Formatter.DoInitializeBandItem(ASubItem, AnIndex);
  Formatter.DoReportLinkInitializeBandItem(ASubItem, AnIndex);
end;

function TcxTreeListReportLinkBandsProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxTreeListReportLinkBandsProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Formatter.GetBandItemBounds(Index);
end;

function TcxTreeListReportLinkBandsProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetBandItemClass(Index);
end;

{ TcxTreeListReportLinkBandedProducer }

constructor TcxTreeListReportLinkBandedRowProducer.Create(ABuilder: TcxTreeListReportLinkBuilder);
begin
  inherited Create(ABuilder);
  FIndents := TList.Create;
end;

destructor TcxTreeListReportLinkBandedRowProducer.Destroy;
begin
  FIndents.Free;
  inherited Destroy;
end;

function TcxTreeListReportLinkBandedRowProducer.Produce(AHostInfo: TcxTreeListAttributeHostInfo): TdxReportCell;
begin
  FIndents.Clear;
  Result := inherited Produce(AHostInfo);
end;

procedure TcxTreeListReportLinkBandedRowProducer.AfterCalculateRowHeight;
begin
  CreateIndentsRow(Row);
end;

procedure TcxTreeListReportLinkBandedRowProducer.CreateBandedDataItems(AParent: TdxReportCell);
var
  I: Integer;
  ABandPlace: TcxTreeListCustomBandPlace;
  AColumnPlace: TcxTreeListColumnPlace;
  AItem: TAbstractdxReportCellData;
begin
  ABandPlace := Formatter.ItemPlaceController.GetItemByBand(TcxTreeListBand(AParent.Data));
  for I := 0 to ABandPlace.ItemCount - 1 do
  begin
    AColumnPlace := ABandPlace.Items[I];
    AItem := GetBandedDataItemClass(AColumnPlace.Column).Create(AParent);
    AItem.Data := TdxNativeInt(AColumnPlace.Column);
    AItem.BoundsRect := GetBandedDataItemBounds(AItem);
    InitializeBandedDataItem(AItem);
  end;
end;

procedure TcxTreeListReportLinkBandedRowProducer.CreateBandedRows(AParent: TdxReportCell);
var
  I: Integer;
  ABandPlace: TcxTreeListCustomBandPlace;
  ARow: TdxReportCell;
  ABandRowBounds: TRect;
begin
  for I := 0 to Formatter.ItemPlaceController.BottomItemCount - 1 do
  begin
    ABandPlace := Formatter.ItemPlaceController.BottomItems[I];
    ARow := TdxReportCell.Create(AParent);
    ARow.Data := TdxNativeInt(ABandPlace.Band);
    ABandRowBounds := Bounds(ABandPlace.LeftBound, 0, ABandPlace.Width, RowHeight);
    if ABandPlace.Band.ActuallyExpandable then
      Inc(ABandRowBounds.Left, GetBandedRowIndent);
    ARow.BoundsRect := ABandRowBounds;
    InitializeBandedRow(ARow);
    CreateBandedDataItems(ARow);
  end;
end;

procedure TcxTreeListReportLinkBandedRowProducer.CreateIndents(AParent: TdxReportCell);
var
  I: Integer;
  AIndentCell: TdxReportCellExpandButton;
begin
  for I := 0 to IndentCount - 1 do
  begin
    AIndentCell := Formatter.GetNodeIndentClass.Create(AParent);
    AIndentCell.BoundsRect := IndentBounds[I];
    InitializeIndent(AIndentCell, I);
    FIndents.Add(AIndentCell);
  end;
end;

procedure TcxTreeListReportLinkBandedRowProducer.CreateIndentsRow(AParent: TdxReportCell);
var
  AIndentsRow: TdxReportCell;
  ABandPlace: TcxTreeListCustomBandPlace;
begin
  if Adapter.TreeList.Bands.ExpandableBand = nil then
    Exit;
  ABandPlace := Formatter.ItemPlaceController.ItemsByBand[Adapter.TreeList.Bands.ExpandableBand];
  AIndentsRow := TdxReportCell.Create(AParent);
  AIndentsRow.CellSides := [];
  AIndentsRow.Data := TdxNativeInt(TcxTreeListIndentsRowAttribute);
  AIndentsRow.BoundsRect := Bounds(ABandPlace.LeftBound, 0, GetBandedRowIndent, RowHeight);
  CreateIndents(AIndentsRow);
end;

procedure TcxTreeListReportLinkBandedRowProducer.CreateRow;
begin
  inherited CreateRow;
  CreateBandedRows(Row);
end;

function TcxTreeListReportLinkBandedRowProducer.GetBandedDataItemBounds(
  AItem: TAbstractdxReportCellData): TRect;
begin
  Result := cxNullRect;
end;

function TcxTreeListReportLinkBandedRowProducer.GetBandedDataItemClass(
  AColumn: TcxTreeListColumn): TdxReportCellDataClass;
begin
  Result := nil;
end;

function TcxTreeListReportLinkBandedRowProducer.GetBandedRowIndent: Integer;
begin
  Result := 0;
end;

function TcxTreeListReportLinkBandedRowProducer.GetIndentArea: Integer;
begin
  Result := IndentCount * IndentWidth;
end;

function TcxTreeListReportLinkBandedRowProducer.GetIndentBound(AIndex: Integer): TRect;
begin
  Result := Bounds(IndentWidth * AIndex, 0, IndentWidth, RowHeight);
end;

function TcxTreeListReportLinkBandedRowProducer.GetIndentCount: Integer;
begin
  Result := 0;
end;

procedure TcxTreeListReportLinkBandedRowProducer.InitializeBandedDataItem(
  AItem: TAbstractdxReportCellData);
begin
end;

procedure TcxTreeListReportLinkBandedRowProducer.InitializeBandedRow(
  ARow: TdxReportCell);
begin
end;

procedure TcxTreeListReportLinkBandedRowProducer.InitializeIndent(AnIndent: TdxReportCellExpandButton; AnIndex: Integer);
begin
end;

function TcxTreeListReportLinkBandedRowProducer.GetIndent(Index: Integer): TdxReportCellExpandButton;
begin
  Result := FIndents[Index];
end;

{ TcxTreeListReportLinkFootersProducer }

function TcxTreeListReportLinkFootersProducer.Produce(AHostInfo: TcxTreeListAttributeHostInfo;
  AAttachedNode, ADataNode: TcxTreeListNode): TdxReportCell;
begin
  FAttachedNode := AAttachedNode;
  FDataNode := ADataNode;
  Result := inherited Produce(AHostInfo);
  FDataNode := nil;
  FAttachedNode := nil;
end;

procedure TcxTreeListReportLinkFootersProducer.CreateBandedDataItems(AParent: TdxReportCell);
var
  I, ACellIndex: Integer;
  AItem: TAbstractdxReportCellData;
  AColumnPlace: TcxTreeListColumnPlace;
  ABandPlace: TcxTreeListCustomBandPlace;
begin
  ABandPlace := Formatter.ItemPlaceController.GetItemByBand(TcxTreeListBand(AParent.Data));
  for I := 0 to ABandPlace.ItemCount - 1 do
  begin
    AColumnPlace := ABandPlace.Items[I];
    for ACellIndex := 0 to Formatter.GetFooterCellCount(AColumnPlace.Column, FDataNode) - 1 do
    begin
      AItem := TdxReportCellString.Create(AParent);
      AItem.BoundsRect := cxRectOffset(Formatter.GetFooterCellBounds(AColumnPlace.Column,
        ACellIndex, IndentArea), -AParent.Left, 0);
      Formatter.DoInitializeFooterCell(AItem, AColumnPlace.Column, ACellIndex, FDataNode);
    end;
  end;
end;

function TcxTreeListReportLinkFootersProducer.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxTreeListReportLinkFootersProducer.GetBandedRowIndent: Integer;
begin
  Result := IndentArea;
end;

function TcxTreeListReportLinkFootersProducer.GetIndentCount: Integer;
begin
  if FAttachedNode <> nil then
  begin
    Result := Formatter.IndentCounts[FDataNode];
    if Adapter.OptionsView.GroupFooters = tlgfVisibleWhenExpanded then
      Inc(Result);
  end
  else
    Result := 0;
end;

function TcxTreeListReportLinkFootersProducer.GetLineCount: Integer;
begin
  Result := Adapter.FooterLineCount[FDataNode];
end;

function TcxTreeListReportLinkFootersProducer.GetLineHeight: Integer;
begin
  Result := Formatter.FooterLineHeight;
end;

procedure TcxTreeListReportLinkFootersProducer.InitializeBandedRow(ARow: TdxReportCell);
begin
  inherited InitializeBandedRow(ARow);
  Formatter.DoInitializeFooterBandedRow(ARow, IndentCount);
end;

procedure TcxTreeListReportLinkFootersProducer.InitializeIndent(
  AnIndent: TdxReportCellExpandButton; AnIndex: Integer);
begin
  Formatter.DoInitializeFooterIndent(AnIndent, AnIndex, IndentCount, FAttachedNode);
  Formatter.DoReportLinkInitializeNodeIndent(AnIndent, AnIndex, IndentCount, FAttachedNode);  // TODO FooterIndentinitialize
end;

procedure TcxTreeListReportLinkFootersProducer.InitializeRow;
begin
  inherited InitializeRow;
  Formatter.DoInitializeFooterRow(Row);
end;

{ TcxTreeListReportLinkHeadersProducer }

procedure TcxTreeListReportLinkHeadersProducer.InitializeRow;
begin
  inherited InitializeRow;
  Formatter.DoInitializeHeaderRow(Row);
end;

procedure TcxTreeListReportLinkHeadersProducer.InitializeSubItem(
  ASubItem: TAbstractdxReportCellData; AnIndex: Integer);
begin
  inherited InitializeSubItem(ASubItem, AnIndex);
  Formatter.DoInitializeHeaderItem(ASubItem, AnIndex);
  Formatter.DoReportLinkInitializeHeaderItem(ASubItem, AnIndex);
end;

function TcxTreeListReportLinkHeadersProducer.GetAutoHeight: Boolean;
begin
  Result := inherited GetAutoHeight and Adapter.HeaderAutoHeight;
end;

function TcxTreeListReportLinkHeadersProducer.GetLineCount: Integer;
begin
  Result := Adapter.DetailsLineCount;
end;

function TcxTreeListReportLinkHeadersProducer.GetLineHeight: Integer;
begin
  Result := Formatter.HeaderLineHeight;
end;

function TcxTreeListReportLinkHeadersProducer.GetSubItemBound(Index: Integer): TRect;
begin
  Result := Formatter.GetHeaderItemBounds(Index);
end;

function TcxTreeListReportLinkHeadersProducer.GetSubItemClass(Index: Integer): TdxReportCellDataClass;
begin
  Result := Formatter.GetHeaderItemClass(Index);
end;

{ TcxTreeListReportLinkNodeProducer }

function TcxTreeListReportLinkNodeProducer.Produce(AHostInfo: TcxTreeListAttributeHostInfo;
  ANode: TcxTreeListNode): TdxReportCell;
begin
  FNode := ANode;
  Result := inherited Produce(AHostInfo);
  FNode := nil;
end;

procedure TcxTreeListReportLinkNodeProducer.CalculateRowAutoHeight;
var
  AMaxRowHeight, ACurrentItemHeight: Integer;
  I, J: Integer;
  ACell: TdxReportCell;
  AItem: TdxReportVisualItem;
begin
  if CellAutoHeight then
  begin
    AMaxRowHeight := 0;
    for I := 0 to Row.CellCount - 1 do
    begin
      ACell := Row.Cells[I];
      for J := 0 to ACell.DataItemCount - 1 do
      begin
        AItem := ACell.DataItems[J];
        if DoesItemParticipateInRowAutoHeightCalculation(AItem) then
        begin
          ACurrentItemHeight := AItem.MeasureContentHeight(Canvas);
          if AMaxRowHeight < ACurrentItemHeight then
           AMaxRowHeight := ACurrentItemHeight;
        end;
      end;
    end;
    if RowHeight < AMaxRowHeight then
      RowHeight := AMaxRowHeight;
  end;
  if HasPreview then
    RowHeight := RowHeight + PreviewItem.Height;
end;

procedure TcxTreeListReportLinkNodeProducer.CalculateRowHeight;
var
  AViewInfo: TcxTreeListViewInfo;
  AViewData, APrevViewData: TcxTreeListNodeViewData;
begin
  if (FNode = nil) or (FNode.Height = 0) then
    inherited CalculateRowHeight
  else
    FRowHeight := FNode.Height;
  TcxCustomTreeListAccess(Adapter.TreeList).DoGetNodeHeight(FNode, FRowHeight);
  if FNode <> nil then
  begin
    APrevViewData := TcxTreeListNodeAccess(FNode).ViewData;
    AViewInfo := TcxCustomTreeListAccess(Adapter.TreeList).ViewInfo;
    AViewData := TcxTreeListNodeViewData.Create(AViewInfo, FNode, Adapter.ColumnCount);
    try
      TcxTreeListViewInfoAccess(AViewInfo).InitializeRows(FRowHeight, AViewData);
    finally
      AViewData.Free;
    end;
    TcxTreeListNodeAccess(FNode).ViewData := APrevViewData;
  end;
end;

procedure TcxTreeListReportLinkNodeProducer.CreateChecks(AParent: TdxReportCell);
begin
  FCheck := Formatter.GetNodeCheckClass(FNode).Create(AParent);
  FCheck.BoundsRect := CheckRect;
  InitializeCheck;
end;

procedure TcxTreeListReportLinkNodeProducer.CreateIcons(AParent: TdxReportCell);
begin
  if Adapter.HasCheck(FNode) then CreateChecks(AParent);
  if Formatter.HasStateImages(FNode) then CreateStateImages(AParent);
  if Formatter.HasImages(FNode) then CreateImages(AParent);
end;

procedure TcxTreeListReportLinkNodeProducer.CreateImages(AParent: TdxReportCell);
begin
  FImage := Formatter.GetNodeImageClass.Create(AParent);
  FImage.BoundsRect := ImageRect;
  InitializeImage(ImageIndentIndex);
end;

procedure TcxTreeListReportLinkNodeProducer.CreateIndents(AParent: TdxReportCell);
begin
  inherited CreateIndents(AParent);
  CreateIcons(AParent);
end;

procedure TcxTreeListReportLinkNodeProducer.CreatePreview(AParent: TdxReportCell);
begin
  FPreviewItem := Formatter.GetPreviewClass(PreviewColumn, FNode).Create(AParent);
  PreviewItem.Left := PreviewIndent;
  // We have to set Width before because we use this value for PreviewHeight calculation
  PreviewItem.Width := RowWidth - PreviewItem.Left;
  Formatter.DoInitializePreview(FPreviewItem, PreviewColumn, FNode); // v3.1 because cxRichEdit
  PreviewItem.Height := PreviewHeight;
  PreviewItem.AdjustContent(Formatter.Canvas); // v3.1
end;

procedure TcxTreeListReportLinkNodeProducer.CreateRow;
begin
  inherited CreateRow;
  if HasPreview then
    CreatePreview(Row);
end;

procedure TcxTreeListReportLinkNodeProducer.CreateStateImages(AParent: TdxReportCell);
begin
  FStateImage := Formatter.GetNodeStateImageClass.Create(AParent);
  FStateImage.BoundsRect := StateImageRect;
  InitializeStateImage(StateImageIndentIndex);
end;

function TcxTreeListReportLinkNodeProducer.DoesItemParticipateInRowAutoHeightCalculation(
  AnItem: TdxReportVisualItem): Boolean;
var
  AColumn: TcxTreeListColumn;
  AProperties: TcxCustomEditProperties;
begin
  Result := AnItem <> FPreviewItem;
  if Result and not IsItemIndent(AnItem) and IsDelphiObject(AnItem.Data) then
  begin
    AColumn := TcxTreeListColumn(AnItem.Data);
    AProperties := Adapter.GetProperties(AColumn, FNode);
    Result := dxPScxCommon.dxPSDataMaps.DoesItemParticipateInAutoHeightCalculation(AProperties);
  end;
end;

procedure TcxTreeListReportLinkNodeProducer.FixupRowDataHeight;
var
  ANewHeight, I, J: Integer;
  AItem: TdxReportVisualItem;
  ACell: TdxReportCell;
begin
  ANewHeight := RowHeight;
  if PreviewItem <> nil then
  begin
    Dec(ANewHeight, PreviewItem.Height);
    if PreviewPlace = tlppTop then
      PreviewItem.Top := 0
    else
      PreviewItem.Top := ANewHeight;
  end;

  for I := 0 to Row.DataItemCount - 1 do
  begin
    if Row.DataItems[I] <> PreviewItem then
    begin
      Row.DataItems[I].Height := ANewHeight;
      if HasPreview and (PreviewPlace = tlppTop) then
        Row.DataItems[I].Top := Row.DataItems[I].Top + PreviewItem.Height;
    end;
  end;

  for I := 0 to Row.CellCount - 1 do
  begin
    ACell := Row.Cells[I];
    if HasPreview and (PreviewPlace = tlppTop) then
      ACell.Top := ACell.Top + PreviewItem.Height;
    if CellAutoHeight then
    begin
      ACell.Height := ANewHeight;
      for J := 0 to ACell.DataItemCount - 1 do
      begin
        AItem := ACell.DataItems[J];
        AItem.Height := ANewHeight;
      end;
    end;
  end;
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeBandedDataItem(
  AItem: TAbstractdxReportCellData);
begin
  inherited InitializeBandedDataItem(AItem);
  Formatter.DoInitializeNodeItem(AItem, TcxTreeListColumn(AItem.Data), FNode, False);
  Formatter.DoReportLinkInitializeNodeItem(AItem, TcxTreeListColumn(AItem.Data), FNode);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeBandedRow(ARow: TdxReportCell);
begin
  inherited InitializeBandedRow(ARow);
  Formatter.DoInitializeNodeBandedRow(ARow);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeCheck;
begin
  Formatter.DoInitializeNodeCheck(FCheck, FNode, CheckIndentIndex);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeImage(AnIndex: Integer);
begin
  Formatter.DoInitializeNodeImage(Image, FNode, AnIndex);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeIndent(AnIndent: TdxReportCellExpandButton;
  AnIndex: Integer);
begin
  Formatter.DoInitializeNodeIndent(AnIndent, AnIndex, IndentCount, FNode);
  Formatter.DoReportLinkInitializeNodeIndent(AnIndent, AnIndex, IndentCount, FNode);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeRow;
begin
  inherited;
  Formatter.DoInitializeNodeRow(Row, FNode);
end;

procedure TcxTreeListReportLinkNodeProducer.InitializeStateImage(AnIndex: Integer);
begin
  Formatter.DoInitializeNodeStateImage(StateImage, FNode, AnIndex);
  Formatter.DoReportLinkInitializeNodeStateImage(StateImage, FNode, AnIndex);
end;

function TcxTreeListReportLinkNodeProducer.IsItemIndent(AnItem: TdxReportVisualItem): Boolean;
begin
  Result := FIndents.IndexOf(AnItem) <> -1;
end;

function TcxTreeListReportLinkNodeProducer.GetAutoHeight: Boolean;
begin
  Result := CellAutoHeight or HasPreview;
end;

function TcxTreeListReportLinkNodeProducer.GetBandedDataItemBounds(
  AItem: TAbstractdxReportCellData): TRect;
var
  AColumnPlace: TcxTreeListColumnPlace;
begin
  AColumnPlace := Formatter.ItemPlaceController.GetItemByColumn(TcxTreeListColumn(AItem.Data));
  Result := cxRectOffset(AColumnPlace.CellBounds[FNode], -AItem.Parent.Left, 0);
end;

function TcxTreeListReportLinkNodeProducer.GetBandedDataItemClass(
  AColumn: TcxTreeListColumn): TdxReportCellDataClass;
begin
  Result := Formatter.GetNodeItemClass(AColumn, FNode, False);
end;

function TcxTreeListReportLinkNodeProducer.GetBandedRowIndent: Integer;
begin
  Result := PreviewIndent;
end;

function TcxTreeListReportLinkNodeProducer.GetCellAutoHeight: Boolean;
begin
  Result := inherited GetAutoHeight and Adapter.CellAutoHeight;
end;

function TcxTreeListReportLinkNodeProducer.GetCheckRect: TRect;
begin
  Result := Bounds(IndentWidth * IndentCount, 0, Adapter.GetCheckWidth(FNode), RowHeight);
end;

function TcxTreeListReportLinkNodeProducer.GetHasPreview: Boolean;
begin
  Result := Formatter.HasPreview;
end;

function TcxTreeListReportLinkNodeProducer.GetImageRect: TRect;
begin
  Result := Bounds(IndentWidth * IndentCount, 0, Formatter.GetImagesWidth(FNode), RowHeight);
  OffsetRect(Result, Formatter.GetStateImagesWidth(FNode) + Adapter.GetCheckWidth(FNode), 0);
end;

function TcxTreeListReportLinkNodeProducer.GetIndentCount: Integer;
begin
  Result := Formatter.IndentCounts[FNode];
end;

function TcxTreeListReportLinkNodeProducer.GetLineCount: Integer;
begin
  Result := Adapter.DetailsLineCount;
end;

function TcxTreeListReportLinkNodeProducer.GetLineHeight: Integer;
begin
  Result := Formatter.DetailsLineHeight;
end;

function TcxTreeListReportLinkNodeProducer.GetPreviewHeight: Integer;
begin
  Result := PreviewLineCount * PreviewLineHeight;
  if not (Formatter.PreviewAutoHeight and (Formatter.PreviewMaxLineCount = 0)) then
    Inc(Result, 2 * dxTextSpace);
end;

function TcxTreeListReportLinkNodeProducer.GetPreviewIndent: Integer;
begin
  Result := Formatter.GetNodeIndent(FNode);
end;

function TcxTreeListReportLinkNodeProducer.GetPreviewLineCount: Integer;
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

function TcxTreeListReportLinkNodeProducer.GetPreviewLineHeight: Integer;
var
  LineHeight: Integer;
begin
  if FPreviewItem <> nil then
    if Formatter.PreviewAutoHeight then
    begin
      Result := PreviewItem.MeasureContentHeight(Formatter.Canvas);
      if Formatter.PreviewMaxLineCount <> 0 then
      begin
        LineHeight := PreviewItem.MeasureFontHeight(Formatter.Canvas) - 2 * dxTextSpace;
        Result := Min(LineHeight * Formatter.PreviewMaxLineCount, Result);
      end;
    end
    else
      Result := PreviewItem.MeasureFontHeight(Formatter.Canvas) - 2 * dxTextSpace
  else
    Result := 0;
end;

function TcxTreeListReportLinkNodeProducer.GetStateImageRect: TRect;
begin
  Result := Bounds(IndentWidth * IndentCount + Adapter.GetCheckWidth(FNode), 0,
    Formatter.GetStateImagesWidth(FNode), RowHeight);
end;

function TcxTreeListReportLinkNodeProducer.GetCheckIndentIndex: Integer;
begin
  Result := IndentCount;
end;

function TcxTreeListReportLinkNodeProducer.GetImageIndentIndex: Integer;
begin
  Result := StateImageIndentIndex;
  if Formatter.HasStateImages(FNode) then
    Inc(Result);
end;

function TcxTreeListReportLinkNodeProducer.GetPreviewColumn: TcxTreeListColumn;
begin
  Result := Adapter.PreviewColumn;
end;

function TcxTreeListReportLinkNodeProducer.GetPreviewPlace: TcxTreeListPreviewPlace;
begin
  Result := Adapter.PreviewPlace;
end;

function TcxTreeListReportLinkNodeProducer.GetStateImageIndentIndex: Integer;
begin
  Result := CheckIndentIndex;
  if Adapter.HasCheck(FNode) then
    Inc(Result);
end;

{ TcxTreeListReportLinkCategorizedNodeProducer }

procedure TcxTreeListReportLinkCategorizedNodeProducer.CreateBandedRows(AParent: TdxReportCell);
var
  AItem: TAbstractdxReportCellData;
begin
  AItem := Formatter.GetNodeItemClass(Adapter.CategorizedColumn, FNode, False).Create(Row);
  AItem.BoundsRect := Rect(PreviewIndent, 0, RowWidth, RowHeight);
  Formatter.DoInitializeNodeItem(AItem, Adapter.CategorizedColumn, FNode, False);
  Formatter.DoReportLinkInitializeNodeItem(AItem, Adapter.CategorizedColumn, FNode);
end;

{ TcxTreeListNodeHelper }

constructor TcxTreeListNodeHelper.Create(AnAdapter: TcxTreeListAdapter);
begin
  inherited Create;
  FAdapter := AnAdapter;
end;

class function TcxTreeListNodeHelper.PairClass: TClass;
begin
  Result := NodeClass;
end;

class function TcxTreeListNodeHelper.ProducerClass: TcxTreeListReportLinkNodeProducerClass;
begin
  Result := TcxTreeListReportLinkNodeProducer;
end;

class procedure TcxTreeListNodeHelper.Register;
begin
  cxTreeListNodeHelperFactory.Register(Self);
end;

class procedure TcxTreeListNodeHelper.Unregister;
begin
  cxTreeListNodeHelperFactory.Unregister(Self);
end;

function TcxTreeListNodeHelper.Adapter: TcxTreeListAdapter;
begin
  Result := FAdapter;
end;

function TcxTreeListNodeHelper.GetHasSelectedChildren: Boolean;
var
  SaveNode: TcxTreeListNode;
  I: Integer;
begin
  Result := True;
  SaveNode := Node;
  try
    with Node do
      for I := 0 to Count - 1 do
      begin
        Node := Items[I];
        if Node.Selected or Adapter.Helpers[Node].HasSelectedChildren then Exit;
      end;
  finally
    Node := SaveNode;
  end;
  Result := False;
end;

function TcxTreeListNodeHelper.GetHasSelectedParents: Boolean;
var
  Parent: TcxTreeListNode;
begin
  Parent := Node;
  while (Parent <> nil) and not Parent.Selected do
    Parent := Parent.Parent;
  Result := Parent <> nil;
end;

class function TcxTreeListNodeHelper.NodeClass: TcxTreeListNodeClass;
begin
  Result := TcxTreeListNode;
end;

{ TcxTreeListNodeHelperCache }

constructor TcxTreeListNodeHelperCache.Create(AnAdapter: TcxTreeListAdapter);
begin
  inherited Create;
  FAdapter := AnAdapter;
end;

function TcxTreeListNodeHelperCache.IndexOf(Node: TcxTreeListNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].NodeClass = Node.ClassType then Exit;

  Result := Add(cxTreeListNodeHelperFactory.HelperClasses[Node].Create(Adapter));
end;

function TcxTreeListNodeHelperCache.GetHelper(Node: TcxTreeListNode): TcxTreeListNodeHelper;
begin
  Result := Items[IndexOf(Node)];
  Result.Node := Node;
end;

function TcxTreeListNodeHelperCache.GetItem(Index: Integer): TcxTreeListNodeHelper;
begin
  Result := TcxTreeListNodeHelper(inherited Items[Index]);
end;

{ TcxTreeListReportLinkProducerCache }

constructor TcxTreeListReportLinkProducerCache.Create(ABuilder: TcxTreeListReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxTreeListReportLinkProducerCache.IndexOf(AProducerClass: TcxTreeListReportLinkRowProducerClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].ClassType = AProducerClass then Exit;

  Result := Add(AProducerClass.Create(Builder));
end;

function TcxTreeListReportLinkProducerCache.GetProducer(ProducerClass: TcxTreeListReportLinkRowProducerClass): TcxTreeListReportLinkRowProducer;
begin
  Result := Items[IndexOf(ProducerClass)];
end;

function TcxTreeListReportLinkProducerCache.GetItem(Index: Integer): TcxTreeListReportLinkRowProducer;
begin
  Result := TcxTreeListReportLinkRowProducer(inherited Items[Index]);
end;

{ TcxTreeListReportLinkBuilder }

constructor TcxTreeListReportLinkBuilder.Create(AReportLink: TcxTreeListCustomReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  FAdapter := AdapterClass.Create(Self);
  FFormatter := FormatterClass.Create(Self);
  FProducerCache := TcxTreeListReportLinkProducerCache.Create(Self);
  FReportRows := TList.Create;
end;

destructor TcxTreeListReportLinkBuilder.Destroy;
begin
  FreeAndNil(FReportRows);
  FreeAndNil(FProducerCache);
  FreeAndNil(FFormatter);
  FreeAndNil(FAdapter);
  inherited;
end;

procedure TcxTreeListReportLinkBuilder.Build;
begin
  BeforeBuilding;
  try
    DoBuild;
  finally
    AfterBuilding;
  end;
end;

procedure TcxTreeListReportLinkBuilder.Progress(const APercentDone: Double);
begin
  ReportLink.DoProgress(APercentDone);
end;

function TcxTreeListReportLinkBuilder.Adapter: TcxTreeListAdapter;
begin
  Result := FAdapter;
end;

class function TcxTreeListReportLinkBuilder.AdapterClass: TcxTreeListAdapterClass;
begin
  Result := TcxTreeListAdapter;
end;

function TcxTreeListReportLinkBuilder.Formatter: TcxTreeListReportLinkFormatter;
begin
  Result := FFormatter;
end;

class function TcxTreeListReportLinkBuilder.FormatterClass: TcxTreeListReportLinkFormatterClass;
begin
  Result := TcxTreeListReportLinkFormatter;
end;

procedure TcxTreeListReportLinkBuilder.AddReportRow(ARow: TdxReportCell);
begin
  if ARow.Parent = ReportLink.ReportCells.Cells then
    FReportRows.Add(ARow);
end;

procedure TcxTreeListReportLinkBuilder.AfterBuilding;
begin
  Formatter.AfterBuilding;
end;

procedure TcxTreeListReportLinkBuilder.BeforeBuilding;
begin
  Formatter.BeforeBuilding;
end;

procedure TcxTreeListReportLinkBuilder.CreateBands;
begin
  AddReportRow(GetBandsProducer.Produce(ReportLink.HostInfoServices.BandHeadersHostInfo));
end;

function TcxTreeListReportLinkBuilder.GetBandsProducer: TcxTreeListReportLinkBandsProducer;
begin
  Result := ProducerCache[GetBandsProducerClass] as TcxTreeListReportLinkBandsProducer;
end;

function TcxTreeListReportLinkBuilder.GetBandsProducerClass: TcxTreeListReportLinkBandsProducerClass;
begin
  Result := TcxTreeListReportLinkBandsProducer;
end;

procedure TcxTreeListReportLinkBuilder.CreateFooters(AHostInfo: TcxTreeListAttributeHostInfo;
   AAttachedNode, ADataNode: TcxTreeListNode);
begin
  AddReportRow(GetFootersProducer.Produce(AHostInfo, AAttachedNode, ADataNode));
end;

function TcxTreeListReportLinkBuilder.GetFootersProducer: TcxTreeListReportLinkFootersProducer;
begin
  Result := ProducerCache[GetFootersProducerClass] as TcxTreeListReportLinkFootersProducer;
end;

function TcxTreeListReportLinkBuilder.GetFootersProducerClass: TcxTreeListReportLinkFootersProducerClass;
begin
  Result := TcxTreeListReportLinkFootersProducer;
end;

procedure TcxTreeListReportLinkBuilder.CreateHeaders;
begin
  AddReportRow(GetHeadersProducer.Produce(ReportLink.HostInfoServices.HeadersHostInfo));
end;

function TcxTreeListReportLinkBuilder.GetHeadersProducer: TcxTreeListReportLinkHeadersProducer;
begin
  Result := ProducerCache[GetHeadersProducerClass] as TcxTreeListReportLinkHeadersProducer;
end;

function TcxTreeListReportLinkBuilder.GetHeadersProducerClass: TcxTreeListReportLinkHeadersProducerClass;
begin
  Result := TcxTreeListReportLinkHeadersProducer;
end;

procedure TcxTreeListReportLinkBuilder.CreateNode(ANode: TcxTreeListNode);
begin
  AddReportRow(GetNodeProducer(ANode).Produce(ReportLink.HostInfoServices.PageDetailsHostInfo, ANode));
end;

procedure TcxTreeListReportLinkBuilder.CreateNodes;
var
  AHasNodeSeparator, AHasGroupFooters: Boolean;
  I: Integer;
  ANode: TcxTreeListNode;
  ASummaryNodes: TObjectStack;
begin
  AHasNodeSeparator := Formatter.HasNodeSeparator;
  AHasGroupFooters := Adapter.OptionsView.GroupFooters <> tlgfInvisible;
  ASummaryNodes := TObjectStack.Create;
  try
    for I := 0 to Formatter.NodeCount - 1 do
    begin
      ANode := Formatter.Nodes[I];
      CreateNode(ANode);
      if AHasGroupFooters then
      begin
        if Adapter.HasNodeSummary(ANode) then
          ASummaryNodes.Push(ANode);
        while (ASummaryNodes.Count > 0) and
          Adapter.HasNodeAttachedFooter(ANode, TcxTreeListNode(ASummaryNodes.Peek)) do
          CreateFooters(ReportLink.HostInfoServices.PageDetailsHostInfo, ANode,
             TcxTreeListNode(ASummaryNodes.Pop));
      end;
      if AHasNodeSeparator and (I < Formatter.NodeCount - 1) then
        CreateNodeSeparator(ANode);

      Progress(100 * (I + 1) / Formatter.NodeCount);
      if IsAborted then Break;
    end;
  finally
    ASummaryNodes.Free;
  end;
end;

function TcxTreeListReportLinkBuilder.GetNodeProducer(ANode: TcxTreeListNode): TcxTreeListReportLinkNodeProducer;
begin
  Result := ProducerCache[GetNodeProducerClass(ANode)] as TcxTreeListReportLinkNodeProducer;
end;

function TcxTreeListReportLinkBuilder.GetNodeProducerClass(ANode: TcxTreeListNode): TcxTreeListReportLinkNodeProducerClass;
begin
  if Adapter.IsGroupNode(ANode) then
    Result := TcxTreeListReportLinkCategorizedNodeProducer
  else
    Result := Adapter.Helpers[ANode].ProducerClass;
end;

procedure TcxTreeListReportLinkBuilder.CreateNodeSeparator(ANode: TcxTreeListNode);
begin
  AddReportRow(GetNodeSeparatorProducer.Produce(ReportLink.HostInfoServices.PageDetailsHostInfo));
end;

function TcxTreeListReportLinkBuilder.GetNodeSeparatorProducer: TcxTreeListReportLinkNodeSeparatorProducer;
begin
  Result := ProducerCache[GetNodeSeparatorProducerClass] as TcxTreeListReportLinkNodeSeparatorProducer;
end;

function TcxTreeListReportLinkBuilder.GetNodeSeparatorProducerClass: TcxTreeListReportLinkNodeSeparatorProducerClass;
begin
  Result := TcxTreeListReportLinkNodeSeparatorProducer;
end;

procedure TcxTreeListReportLinkBuilder.DoBuild;
begin
  if Formatter.ShowBandHeaders then CreateBands;
  if Formatter.ShowHeaders then CreateHeaders;
  CreateNodes;

  if not IsAborted and Formatter.ShowFooters then
    CreateFooters(ReportLink.HostInfoServices.FootersHostInfo, nil, TreeList.Root);
end;

function TcxTreeListReportLinkBuilder.IsAborted: Boolean;
begin
  Result := ReportLink.AbortBuilding;
end;

function TcxTreeListReportLinkBuilder.GetHost: TdxReportCell;
begin
  Result := ReportLink.ReportCells.Cells;
end;

function TcxTreeListReportLinkBuilder.GetReportCells: TdxReportCells;
begin
  Result := ReportLink.ReportCells;
end;

function TcxTreeListReportLinkBuilder.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FReportRows[Index]);
end;

function TcxTreeListReportLinkBuilder.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TcxTreeListReportLinkBuilder.GetTreeList: TcxCustomTreeList;
begin
  Result := ReportLink.TreeList;
end;

{ TcxTreeListReportLinkBuilderHandler }

constructor TcxTreeListReportLinkBuilderHandler.Create(ABuilder: TcxTreeListReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxTreeListReportLinkBuilderHandler.Builder: TcxTreeListReportLinkBuilder;
begin
  Result := FBuilder;
end;

function TcxTreeListReportLinkBuilderHandler.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := Builder.ReportLink;
end;

{ TcxTreeListAdapter }

constructor TcxTreeListAdapter.Create(ABuilder: TcxTreeListReportLinkBuilder);
var
  AConditionalFormatting: IcxDataControllerConditionalFormattingProviderOwner;
begin
  inherited;
  FHelperCache := TcxTreeListNodeHelperCache.Create(Self);
  FDetailsLineCount := -1;
  FFooterLineCount := -1;
  FMultiRows := False;

  if Supports(TreeList, IcxDataControllerConditionalFormattingProviderOwner, AConditionalFormatting) then
    FConditionalFormattingProvider := AConditionalFormatting.GetConditionalFormattingProvider;
end;

destructor TcxTreeListAdapter.Destroy;
begin
  FreeAndNil(FHelperCache);
  inherited;
end;

function TcxTreeListAdapter.CalculateDetailsLineCount: Integer;
var
  ABandRows: TcxTreeListBandRows;
  I, J, V: Integer;
begin
  Result := 0;
  for I := 0 to BottomBandCount - 1 do
  begin
    ABandRows := BottomBands[I].BandRows;
    FMultiRows := FMultiRows or (ABandRows.VisibleItemCount > 1);
    V := 0;
    for J := 0 to ABandRows.VisibleItemCount - 1 do
      Inc(V, ABandRows[J].LineCount);
    Result := Max(Result, V);
  end;
end;

procedure TcxTreeListAdapter.Calculate;
begin
  FDetailsLineCount := CalculateDetailsLineCount;
  if not FMultiRows then
  begin
    FFooterLineCount := Max(1, TreeList.Summary.FooterSummaryRowCount);
    FGroupFooterLineCount := Max(1, TreeList.Summary.GroupFooterSummaryRowCount);
  end
  else
  begin
    FFooterLineCount := Max(1, TreeList.Bands.ColumnsLineCount);
    FGroupFooterLineCount := FFooterLineCount;
  end;
end;

class function TcxTreeListAdapter.GetProperties(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode): TcxCustomEditProperties;
begin
  Result := TreeListColumn_DoGetEditProperties(AColumn, ANode);
  if Result = nil then
    Result := GetRepositoryItem(AColumn, ANode).Properties;
end;

class function TcxTreeListAdapter.GetPropertiesClass(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode): TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditPropertiesClass(GetProperties(AColumn, ANode).ClassType);
end;

class function TcxTreeListAdapter.GetRepositoryItem(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode): TcxEditRepositoryItem;
begin
  Result := AColumn.RepositoryItem;
end;

function TcxTreeListAdapter.GetBackgroundViewParams: TcxViewParams;
begin
  Result := Styles.GetBackgroundParams;
end;

function TcxTreeListAdapter.GetBandBackgroundViewParams: TcxViewParams;
begin
  Result := Styles.GetBandBackgroundParams(nil);
end;

function TcxTreeListAdapter.GetBandHeaderViewParams(ABand: TcxTreeListBand): TcxViewParams;
begin
  Result := Styles.GetBandHeaderParams(ABand);
  if ABand = nil then
    Result.Color := LookAndFeelPainter.DefaultHeaderBackgroundColor
end;

function TcxTreeListAdapter.GetContentViewParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
begin
  Result := Styles.GetContentParams(ANode, AColumn);
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxTreeListAdapter.GetColumnFooterViewParams(AColumn: TcxTreeListColumn): TcxViewParams;
begin
  Result := Styles.GetColumnFooterParams(AColumn, nil);
  if Result.Color = clDefault then
    Result.Color := LookAndFeelPainter.DefaultHeaderColor;
end;

function TcxTreeListAdapter.GetColumnHeaderViewParams(AColumn: TcxTreeListColumn): TcxViewParams;
begin
  if AColumn = nil then
    Result := Styles.GetBandBackgroundParams(Bands[0])
  else
    Result := Styles.GetColumnHeaderParams(AColumn);
end;

function TcxTreeListAdapter.GetIndentViewParams(ANode: TcxTreeListNode; AnIndent: Integer): TcxViewParams;
begin
  Result := Styles.GetIndentParams(ANode, AnIndent);
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxTreeListAdapter.GetPreviewViewParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
begin
  Result := Styles.GetPreviewParams(ANode);
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxTreeListAdapter.GetFooterRowViewParams: TcxViewParams;
begin
  Result := Styles.GetFooterParams;
end;

function TcxTreeListAdapter.GetSelectionViewParams: TcxViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Styles.Selection do
  begin
    Result.Bitmap := nil;
    Result.Color := Color;
    Result.Font := Font;
    Result.TextColor := TextColor;
  end;
end;

function TcxTreeListAdapter.HasSelectionStyle: Boolean;
begin
  Result := Styles.Selection <> nil;
end;

function TcxTreeListAdapter.TryGetAdvancedStyle(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
begin
  Result := (ConditionalFormattingProvider <> nil) and
    TcxDataControllerConditionalFormattingProviderAccess(ConditionalFormattingProvider).TryGetStyle(
    cxPoint(AColumn.ItemIndex, ANode.AbsoluteIndex), AStyle);
end;

function TcxTreeListAdapter.GetBackgroundBitmap(Index: Integer): TBitmap;
begin
  Result := Styles.GetBitmap(Index);
end;

function TcxTreeListAdapter.HasBackgroundBitmap(Index: Integer): Boolean;
begin
  Result := GetBackgroundBitmap(Index) <> nil;
end;

procedure TcxTreeListAdapter.DoGetLevelImages(ALevel: Integer; var AImages, AStateImages: TCustomImageList);
begin
  AImages := Images;
  AStateImages := StateImages;
  CustomTreeList_DoGetLevelImages(TreeList, ALevel, AImages, AStateImages);
end;

function TcxTreeListAdapter.GetCheckWidth(ANode: TcxTreeListNode): Integer;
begin
  if HasCheck(ANode) then
    Result := DefaultIndentWidth
  else
    Result := 0;
end;

function TcxTreeListAdapter.HasCheck(ANode: TcxTreeListNode): Boolean;
begin
  Result := TreeList.OptionsView.CheckGroups and (ANode.Parent <> nil) and (ANode.Parent.CheckGroupType <> ncgNone);
end;

function TcxTreeListAdapter.IsGroupNode(ANode: TcxTreeListNode): Boolean;
begin
  Result := CustomTreeList_DoIsGroupNode(TreeList, ANode);
end;

function TcxTreeListAdapter.HasNodeAttachedFooter(ANode, ASummaryNode: TcxTreeListNode): Boolean;
begin
  Result := not ANode.Expanded and ((ANode = ASummaryNode) or
    (ANode = ASummaryNode.GetLastChildVisible) or
    ANode.HasAsParent(ASummaryNode.GetLastChildVisible));
end;

function TcxTreeListAdapter.HasNodeSummary(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.HasChildren and ((OptionsView.GroupFooters = tlgfAlwaysVisible) or
    (OptionsView.GroupFooters = tlgfVisibleWhenExpanded) and ANode.Expanded);
end;

function TcxTreeListAdapter.GetAutoWidth: Boolean;
begin
  Result := OptionsView.ColumnAutoWidth;
end;

function TcxTreeListAdapter.GetBand(Index: Integer): TcxTreeListBand;
begin
  Result := CustomTreeList_GetVisibleBand(TreeList, Index);
end;

function TcxTreeListAdapter.GetBandCount: Integer;
begin
  Result := CustomTreeList_GetVisibleBandCount(TreeList);
end;

function TcxTreeListAdapter.GetBottomBand(Index: Integer): TcxTreeListBand;
begin
  Result := TreeList.Bands.BottomItems[Index];
end;

function TcxTreeListAdapter.GetBottomBandCount: Integer;
begin
  Result := TreeList.Bands.BottomItemCount;
end;

function TcxTreeListAdapter.GetCanUseLookAndFeelColors: Boolean;
begin
  Result := CustomTreeList_GetLookAndFeel(TreeList).NativeStyle and dxThemeManager.AreVisualStylesAvailable;
end;

function TcxTreeListAdapter.GetCategorizedColumn: TcxTreeListColumn;
begin
  Result := OptionsView.GetCategorizedColumn;
end;

function TcxTreeListAdapter.GetColumnCount: Integer;
begin
  Result := CustomTreeList_GetVisibleColumnCount(TreeList);
end;

function TcxTreeListAdapter.GetCellAutoHeight: Boolean;
begin
  Result := OptionsView.CellAutoHeight and (DetailsLineCount = 1);
end;

function TcxTreeListAdapter.GetCellEndEllipsis: Boolean;
begin
  Result := OptionsView.CellEndEllipsis;
end;

function TcxTreeListAdapter.GetCellMultiline: Boolean;
begin
  Result := OptionsView.CellAutoHeight and (DetailsLineCount = 1);
end;

function TcxTreeListAdapter.GetColumn(Index: Integer): TcxTreeListColumn;
begin
  Result := CustomTreeList_GetVisibleColumn(TreeList, Index);
end;

function TcxTreeListAdapter.GetDefaultRowHeight: Integer;
begin
  Result := TreeList.DefaultRowHeight;
end;

function TcxTreeListAdapter.GetFooterLineCount(ANode: TcxTreeListNode): Integer;
begin
  if ANode = TreeList.Root then
    Result := FFooterLineCount
  else
    Result := FGroupFooterLineCount;
end;

function TcxTreeListAdapter.GetGridLines: TcxTreeListGridLines;
begin
  Result := OptionsView.GridLines;
end;

function TcxTreeListAdapter.GetGridLinesColor: TColor;
begin
  Result := OptionsView.GridLineColor;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultGridLineColor;
end;

function TcxTreeListAdapter.GetHasPreview: Boolean;
begin
  Result := TreeList.Preview.Active;
end;

function TcxTreeListAdapter.GetHeaderAutoHeight: Boolean;
begin
  Result := (DetailsLineCount = 1) and OptionsView.HeaderAutoHeight;
end;

function TcxTreeListAdapter.GetHelper(Node: TcxTreeListNode): TcxTreeListNodeHelper;
begin
  Result := FHelperCache.Helpers[Node];
end;

function TcxTreeListAdapter.GetIndentWidth: Integer;
begin
  Result := DefaultIndentWidth;
end;

function TcxTreeListAdapter.GetIsDefaultRowHeightAssigned: Boolean;
begin
  Result := DefaultRowHeight <> 0;
end;

function TcxTreeListAdapter.GetImages: TCustomImageList;
begin
  Result := CustomTreeList_GetImages(TreeList);
end;

function TcxTreeListAdapter.GetIsNodeColorUsedForIndents: Boolean;
begin
  Result := OptionsView.UseNodeColorForIndent;
end;

function TcxTreeListAdapter.GetLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := CustomTreeList_GetLookAndFeel(TreeList).Kind;
end;

function TcxTreeListAdapter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := CustomTreeList_GetLookAndFeelPainter(TreeList);
end;

function TcxTreeListAdapter.GetMultiSelection: Boolean;
begin
  Result := CustomTreeList_GetOptionsSelection(TreeList).MultiSelect;
end;

function TcxTreeListAdapter.GetNode(Index: Integer): TcxTreeListNode;
begin
  Result := TreeList.Root[Index];
end;

function TcxTreeListAdapter.GetNodeCount: Integer;
begin
  Result := TreeList.Root.Count;
end;

function TcxTreeListAdapter.GetOptionsView: TcxTreeListOptionsView;
begin
  Result := CustomTreeList_GetOptionsView(TreeList);
end;

function TcxTreeListAdapter.GetPaintStyle: TcxTreeListPaintStyle;
begin
  Result := OptionsView.PaintStyle;
end;

function TcxTreeListAdapter.GetPreviewColumn: TcxTreeListColumn;
begin
  Result := CustomTreeList_GetPreview(TreeList).Column;
end;

function TcxTreeListAdapter.GetPreviewLeftIndent: Integer;
begin
  Result := CustomTreeList_GetPreview(TreeList).LeftIndent;
end;

function TcxTreeListAdapter.GetPreviewPlace: TcxTreeListPreviewPlace;
begin
  Result := CustomTreeList_GetPreview(TreeList).Place;
end;

function TcxTreeListAdapter.GetPreviewRightIndent: Integer;
begin
  Result := CustomTreeList_GetPreview(TreeList).RightIndent;
end;

function TcxTreeListAdapter.GetRootBand(Index: Integer): TcxTreeListBand;
begin
  Result := TreeList.Bands.VisibleRootItems[Index];
end;

function TcxTreeListAdapter.GetRootBandCount: Integer;
begin
  Result := TreeList.Bands.visibleRootItemCount;
end;

function TcxTreeListAdapter.GetShowRoot: Boolean;
begin
  Result := OptionsView.ShowRoot;
end;

function TcxTreeListAdapter.GetShowHorzGridLines: Boolean;
begin
  Result := GridLines in [tlglHorz, tlglBoth];
end;

function TcxTreeListAdapter.GetShowTreeLines: Boolean;
begin
  Result := OptionsView.TreeLineStyle <> tllsNone;
end;

function TcxTreeListAdapter.GetShowVertGridLines: Boolean;
begin
  Result := GridLines in [tlglVert, tlglBoth];
end;

function TcxTreeListAdapter.GetStateImages: TCustomImageList;
begin
  Result := CustomTreeList_GetStateImages(TreeList);
end;

function TcxTreeListAdapter.GetStyles: TcxTreeListStyles;
begin
  Result := CustomTreeList_GetStyles(TreeList);
end;

function TcxTreeListAdapter.GetThemedBandHeaderItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

function TcxTreeListAdapter.GetThemedBandHeaderItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderTextColor;
end;

function TcxTreeListAdapter.GetThemedFooterItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFooterColor;
end;

function TcxTreeListAdapter.GetThemedFooterItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultFooterTextColor;
end;

function TcxTreeListAdapter.GetThemedHeaderItemColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

function TcxTreeListAdapter.GetThemedHeaderItemTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderTextColor;
end;

function TcxTreeListAdapter.GetThemedHeaderRowColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderBackgroundColor;
end;

function TcxTreeListAdapter.GetTreeLinesColor: TColor;
begin
  Result := OptionsView.TreeLineColor;
end;

function TcxTreeListAdapter.GetTreeLinesStyle: TcxTreeListTreeLineStyle;
begin
  Result := OptionsView.TreeLineStyle;
end;

function TcxTreeListAdapter.GetTreeList: TcxCustomTreeList;
begin
  Result := Builder.TreeList;
end;

function TcxTreeListAdapter.GetUseStylesForIndents: Boolean;
begin
  Result := OptionsView.UseNodeColorForIndent;
end;

{ TcxTreeListCustomItemPlaceController }

constructor TcxTreeListCustomItemPlaceController.Create(
  AFormatter: TcxTreeListReportLinkFormatter);
begin
  inherited Create;
  FFormatter := AFormatter;
  FHeaderLineCount := -1;
  FWidth := -1;
end;

procedure TcxTreeListCustomItemPlaceController.Calculate;
begin
end;

procedure TcxTreeListCustomItemPlaceController.Refresh;
begin
end;

function TcxTreeListCustomItemPlaceController.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomItemPlaceController.CalculateHeaderLineCount: Integer;
begin
  Result := 1;
end;

function TcxTreeListCustomItemPlaceController.GetItemByColumn(
  Column: TcxTreeListColumn): TcxTreeListColumnPlace;
begin
  Result := nil;
end;

procedure TcxTreeListCustomItemPlaceController.WidthChanged;
begin
end;

function TcxTreeListCustomItemPlaceController.GetAdapter: TcxTreeListAdapter;
begin
  Result := Formatter.Adapter;
end;

function TcxTreeListCustomItemPlaceController.GetHeaderLineCount: Integer;
begin
  if FHeaderLineCount = -1 then
    FHeaderLineCount := CalculateHeaderLineCount;
  Result := FHeaderLineCount;
end;

function TcxTreeListCustomItemPlaceController.GetWidth: Integer;
begin
  if FWidth = -1 then
    FWidth := CalculateWidth;
  Result := FWidth;
end;

procedure TcxTreeListCustomItemPlaceController.SetWidth(Value: Integer);
begin
  FWidth := Value;
  WidthChanged;
end;

{ TcxTreeListColumnPlace }

constructor TcxTreeListColumnPlace.Create(
  AController: TcxTreeListCustomBandPlace; AColumn: TcxTreeListColumn);
begin
  inherited Create;
  FController := AController;
  FColumn := AColumn;

  FLeftBound := -1;
  FWidth := -1;
end;

procedure TcxTreeListColumnPlace.Calculate(ALeftBound: Integer);
begin
  FLeftBound := ALeftBound;
end;

function TcxTreeListColumnPlace.CalculateLeftBound: Integer;
begin
  Result := Controller.CalculateItemLeftBound(Self);
end;

function TcxTreeListColumnPlace.GetLineCount: Integer;
begin
  Result := Column.Position.LineCount;
end;

function TcxTreeListColumnPlace.GetRowIndex: Integer;
begin
  Result := Column.Position.Row.LineOffset;
end;

procedure TcxTreeListColumnPlace.InitAutoWidthItem(
  AnItem: TcxAutoWidthItem);
begin
  AnItem.Fixed := IsFixed;
  AnItem.MinWidth := MinWidth;
  AnItem.Width := Width;
end;

function TcxTreeListColumnPlace.GetAdapter: TcxTreeListAdapter;
begin
  Result := Formatter.Adapter;
end;

function TcxTreeListColumnPlace.GetCellBounds(ANode: TcxTreeListNode): TRect;
var
  ACellHeight: Integer;
  AColumnAccess: TcxTreeListColumnAccess;
begin
  ACellHeight := CellHeights[ANode];
  Result := cxRectBounds(LeftBound, RowIndex * ACellHeight, Width, ACellHeight);
  AColumnAccess := TcxTreeListColumnAccess(Column);
  if AColumnAccess.HasIndent then
    Inc(Result.Left, Formatter.GetNodeIndent(ANode));
end;

function TcxTreeListColumnPlace.GetCellHeight(ANode: TcxTreeListNode): Integer;
var
  AViewInfo: TcxTreeListViewInfo;
begin
  AViewInfo := TcxCustomTreeListAccess(Adapter.TreeList).ViewInfo;
  with TcxTreeListViewInfoAccess(AViewInfo) do
  begin
    Result := RowOffset[Column.Position.Row.LineOffset + Column.Position.LineCount] -
      RowOffset[Column.Position.Row.LineOffset];
  end;
end;

function TcxTreeListColumnPlace.GetFooterCellBounds(AIndex: Integer): TRect;
begin
  Result := Bounds(LeftBound, (RowIndex + AIndex * LineCount) * FooterLineHeight,
    Width, LineCount * FooterLineHeight);
  InflateRect(Result, -FooterItemInflateHorz, -FooterItemInflateVert);
end;

function TcxTreeListColumnPlace.GetFooterLineHeight: Integer;
begin
  Result := Formatter.FooterLineHeight;
end;

function TcxTreeListColumnPlace.GetFormatter: TcxTreeListReportLinkFormatter;
begin
  Result := Controller.Formatter;
end;

function TcxTreeListColumnPlace.GetHeaderCellBounds: TRect;
begin
  Result := cxRectBounds(LeftBound, RowIndex * HeaderLineHeight, Width,
    LineCount * HeaderLineHeight);
end;

function TcxTreeListColumnPlace.GetHeaderLineHeight: Integer;
begin
  Result := Formatter.HeaderLineHeight;
end;

function TcxTreeListColumnPlace.GetIndex: Integer;
begin
  Result := Controller.IndexOf(Self);
end;

function TcxTreeListColumnPlace.GetIsFixed: Boolean;
var
  AColumnAccess: TcxTreeListColumnAccess;
begin
  AColumnAccess := TcxTreeListColumnAccess(Column);
  Result := AColumnAccess.IsFixed;
end;

function TcxTreeListColumnPlace.GetLeftBound: Integer;
begin
  if FLeftBound = -1 then
    FLeftBound := CalculateLeftBound;
  Result := FLeftBound;
end;

function TcxTreeListColumnPlace.GetMinWidth: Integer;
begin
  Result := Column.MinWidth + Column.IndentWidth;
end;

function TcxTreeListColumnPlace.GetOriginalWidth: Integer;
begin
  Result := Column.DisplayWidth;
end;

function TcxTreeListColumnPlace.GetWidth: Integer;
begin
  Result := FWidth;
  if Result = -1 then
    Result := OriginalWidth;
end;

{ TcxTreeListCustomBandPlace }

constructor TcxTreeListCustomBandPlace.Create(
  AController: TcxTreeListItemPlaceController;
  AParent: TcxTreeListBandPlace; ABand: TcxTreeListBand);
begin
  inherited Create(AController.Formatter);
  FColumnIndexes := TList.Create;
  FItems := TObjectList.Create;
  FController := AController;
  FParent := AParent;
  FBand := ABand;
  FHeight := -1;
  FLeftBound := -1;
  FMinWidth := -1;
  FTopBound := -1;
end;

destructor TcxTreeListCustomBandPlace.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FColumnIndexes);
  inherited Destroy;
end;

procedure TcxTreeListCustomBandPlace.Calculate;
begin
  if ItemsAutoWidth then CalculateItemsWidth;
end;

function TcxTreeListCustomBandPlace.IndexOf(
  AnItem: TcxTreeListColumnPlace): Integer;
begin
  Result := FItems.IndexOf(AnItem);
end;

function TcxTreeListCustomBandPlace.IndexOf(
  AColumn: TcxTreeListColumn): Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Column = AColumn then
      Exit;
  Result := -1;
end;

procedure TcxTreeListCustomBandPlace.Refresh;
begin
  FColumnIndexes.Clear;
  FItems.Clear;
  AddItems;
end;

function TcxTreeListCustomBandPlace.CalculateHeaderLineCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  with Band.BandRows do
    for I := 0 to VisibleItemCount - 1 do
      Inc(Result, VisibleItems[I].LineCount);
end;

function TcxTreeListCustomBandPlace.CalculateWidth: Integer;

  function FixedSize: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Band.ColumnCount - 1 do
      Result := Result or not Band.Columns[I].Options.Sizing;
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
     Result := cxTreeListDefWidth;
  if Result < InternalCalculateMinWidth then
    Result := InternalCalculateMinWidth;
end;

function TcxTreeListCustomBandPlace.GetItemByColumn(
  Column: TcxTreeListColumn): TcxTreeListColumnPlace;
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

procedure TcxTreeListCustomBandPlace.AddItems;
var
  I: Integer;
begin
  for I := 0 to Formatter.ColumnCount - 1 do
    CreateItem(Formatter.Columns[I]);
end;

procedure TcxTreeListCustomBandPlace.AssignWidth;
begin
  Width := Width;
end;

function TcxTreeListCustomBandPlace.CalculateHeight: Integer;
begin
  Result := Controller.CalculateItemHeight(Self);
end;

function TcxTreeListCustomBandPlace.CalculateItemLeftBound(
  AnItem: TcxTreeListColumnPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AnItem.Index - 1 do
    Inc(Result, Items[I].Width);
end;

procedure TcxTreeListCustomBandPlace.CalculateItemsWidth;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(ItemCount);
  try
    for I := 0 to ItemCount - 1 do
      Items[I].InitAutoWidthItem(AAutoWidthObject.AddItem);
    AAutoWidthObject.AvailableWidth := ItemsAvailableWidth;
    AAutoWidthObject.Calculate;

    for I := 0 to ItemCount - 1 do
      Items[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

function TcxTreeListCustomBandPlace.CalculateLeftBound: Integer;
begin
  Result := Controller.CalculateItemLeftBound(Self);
end;

function TcxTreeListCustomBandPlace.CalculateLevelHeight: Integer;
begin
  if Adapter.OptionsView.BandLineHeight > 0 then
    Result := Adapter.OptionsView.BandLineHeight
  else
    Result := CalculateLineHeight;// * BandLineCount;
end;

function TcxTreeListCustomBandPlace.CalculateLineHeight: Integer;
begin
  Result := 0;
  Formatter.CalculateHeight(ViewParams, Result);
end;

function TcxTreeListCustomBandPlace.CalculateMinWidth: Integer;
begin
  Result := Max(InternalCalculateMinWidth, Band.MinWidth);
end;

function TcxTreeListCustomBandPlace.CalculateTopBound: Integer;
begin
  Result := Controller.CalculateItemTopBound(Self);
end;

function TcxTreeListCustomBandPlace.CreateItem(
  AColumn: TcxTreeListColumn): TcxTreeListColumnPlace;
begin
  Result := TcxTreeListColumnPlace.Create(Self, AColumn);
  FItems.Add(Result);
end;

procedure TcxTreeListCustomBandPlace.InitAutoWidthItem(
  AnItem: TcxAutoWidthItem);
begin
  AnItem.Fixed := IsFixed;
  AnItem.MinWidth := MinWidth;
  AnItem.Width := Width;
end;

function TcxTreeListCustomBandPlace.GetRowCount: Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomBandPlace.GetItemsAutoWidth: Boolean;
begin
  Result := Formatter.AutoWidth;
end;

function TcxTreeListCustomBandPlace.GetItemsAvailableWidth: Integer;
begin
  Result := Formatter.AvailableWidth;
end;

function TcxTreeListCustomBandPlace.InternalCalculateMinWidth: Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomBandPlace.InternalCalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomBandPlace.GetColumnIndex(
  AColumn: TcxTreeListColumn): Integer;
var
  AColumnIndex, AIndexCount, I: Integer;
begin
  AColumnIndex := AColumn.ItemIndex;
  AIndexCount := FColumnIndexes.Count;
  if AColumnIndex > AIndexCount - 1 then
  begin
    FColumnIndexes.Count := AColumnIndex + 1;
    for I := AIndexCount to FColumnIndexes.Count - 1 do
      FColumnIndexes[I] := TObject(-1);
  end;
  Result := Integer(FColumnIndexes[AColumnIndex]);
  if Result = -1 then
  begin
    FColumnIndexes[AColumnIndex] := TObject(IndexOf(AColumn));
    Result := Integer(FColumnIndexes[AColumnIndex]);
  end;
end;

function TcxTreeListCustomBandPlace.GetBounds: TRect;
begin
  Result := Classes.Bounds(LeftBound, TopBound, Width, Height);
end;

function TcxTreeListCustomBandPlace.GetExpandable: Boolean;
begin
  Result := Band.ActuallyExpandable;
end;

function TcxTreeListCustomBandPlace.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TcxTreeListCustomBandPlace.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.IndexOf(Self)
  else
    Result := Controller.RootIndexOf(Self);
end;

function TcxTreeListCustomBandPlace.GetIsFixed: Boolean;
begin
  Result := not Band.Options.Sizing;
end;

function TcxTreeListCustomBandPlace.GetItem(AIndex: Integer): TcxTreeListColumnPlace;
begin
  Result := TcxTreeListColumnPlace(FItems[AIndex]);
end;

function TcxTreeListCustomBandPlace.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxTreeListCustomBandPlace.GetLeftBound: Integer;
begin
  if FLeftBound = -1 then
    FLeftBound := CalculateLeftBound;
  Result := FLeftBound;
end;

function TcxTreeListCustomBandPlace.GetLevelIndex: Integer;
begin
  Result := Band.Level;
end;

function TcxTreeListCustomBandPlace.GetMinWidth: Integer;
begin
  if FMinWidth = -1 then
    FMinWidth := CalculateMinWidth;
  Result := FMinWidth;
end;

function TcxTreeListCustomBandPlace.GetTopBound: Integer;
begin
  if FTopBound = -1 then
    FTopBound := CalculateTopBound;
  Result := FTopBound;
end;

function TcxTreeListCustomBandPlace.GetViewParams: TdxReportItemViewParams;
begin
  Result := Formatter.GetBandItemViewParams(Band);
end;

{ TcxTreeListBandPlace }

constructor TcxTreeListBandPlace.Create(AnOwner: TcxTreeListItemPlaceController;
  AParent: TcxTreeListBandPlace; ABand: TcxTreeListBand);
begin
  inherited;
  FChildItems := TList.Create;
end;

destructor TcxTreeListBandPlace.Destroy;
begin
  FreeAndNil(FChildItems);
  inherited;
end;

function TcxTreeListBandPlace.IndexOf(
  AnItem: TcxTreeListCustomBandPlace): Integer;
begin
  Result := FChildItems.IndexOf(AnItem);
end;

procedure TcxTreeListBandPlace.Refresh;
begin
  inherited Refresh;
  RefreshChildItems;
end;

function TcxTreeListBandPlace.GetRowCount: Integer;
begin
  Result := 1;
end;

function TcxTreeListBandPlace.InternalCalculateMinWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ChildItemCount - 1 do
    Inc(Result, ChildItems[I].MinWidth);
end;

function TcxTreeListBandPlace.InternalCalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ChildItemCount - 1 do
    Inc(Result, ChildItems[I].Width);
end;

procedure TcxTreeListBandPlace.WidthChanged;
begin
  inherited WidthChanged;
  CalculateChildItemWidths;
end;

procedure TcxTreeListBandPlace.CalculateChildItemWidths;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(ChildItemCount);
  try
    for I := 0 to ChildItemCount - 1 do
      ChildItems[I].InitAutoWidthItem(AAutoWidthObject.AddItem);
    AAutoWidthObject.AvailableWidth := Width;
    AAutoWidthObject.Calculate;

    for I := 0 to ChildItemCount - 1 do
      ChildItems[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

procedure TcxTreeListBandPlace.RefreshChildItems;
var
  I: Integer;
  ABandAccess: TcxTreeListBandAccess;
begin
  ABandAccess := TcxTreeListBandAccess(Band);
  FChildItems.Count := ABandAccess.ChildVisibleBands.Count;
  for I := 0 to FChildItems.Count - 1 do
  begin
    FChildItems[I] := Controller.ItemsByBand[ABandAccess.ChildVisibleBands[I]];
    ChildItems[I].FParent := Self;
  end;
end;

function TcxTreeListBandPlace.GetChildItem(
  Index: Integer): TcxTreeListCustomBandPlace;
begin
  Result := TcxTreeListCustomBandPlace(FChildItems[Index]);
end;

function TcxTreeListBandPlace.GetChildItemCount: Integer;
begin
  Result := FChildItems.Count;
end;

{ TcxTreeListBottomBandPlace }

procedure TcxTreeListBottomBandPlace.AddItems;
var
  I, J: Integer;
  ARow: TcxTreeListBandRow;
begin
  for I := 0 to Band.BandRows.VisibleItemCount - 1 do
  begin
    ARow := Band.BandRows.VisibleItems[I];
    for J := 0 to ARow.VisibleItemCount - 1 do
      CreateItem(ARow.VisibleItems[J]);
  end;
end;

function TcxTreeListBottomBandPlace.CalculateItemLeftBound(
  AnItem: TcxTreeListColumnPlace): Integer;
var
  ARow: TcxTreeListBandRow;
  I: Integer;
  ACurrentItem: TcxTreeListColumnPlace;
begin
  Result := LeftBound;
  ARow := AnItem.Column.Position.Row;
  for I := 0 to AnItem.Column.Position.VisibleColIndex - 1 do
  begin
    ACurrentItem := ItemsByColumn[ARow.VisibleItems[I]];
    Inc(Result, ACurrentItem.Width);
  end;
end;

procedure TcxTreeListBottomBandPlace.CalculateItemsWidth;
var
  I, J: Integer;
  ARow: TcxTreeListBandRow;
  AAutoWidthObject: TcxAutoWidthObject;
  AColumn: TcxTreeListColumn;
begin
  for I := 0 to Band.BandRows.VisibleItemCount - 1 do
  begin
    ARow := Band.BandRows.VisibleItems[I];
    AAutoWidthObject := TcxAutoWidthObject.Create(ARow.VisibleItemCount);
    try
      for J := 0 to ARow.VisibleItemCount - 1 do
        ItemsByColumn[ARow.VisibleItems[J]].InitAutoWidthItem(AAutoWidthObject.AddItem);
      AAutoWidthObject.AvailableWidth := ItemsAvailableWidth;
      AAutoWidthObject.Calculate;

      for J := 0 to ARow.VisibleItemCount - 1 do
      begin
        AColumn := ARow.VisibleItems[J];
        ItemsByColumn[AColumn].Width := AAutoWidthObject[J].AutoWidth;
      end;
    finally
      AAutoWidthObject.Free;
    end;
  end;
end;

function TcxTreeListBottomBandPlace.GetItemsAutoWidth: Boolean;
begin
  Result := True;
end;

function TcxTreeListBottomBandPlace.GetItemsAvailableWidth: Integer;
begin
  Result := Width;
end;

function TcxTreeListBottomBandPlace.GetRowCount: Integer;
begin
  Result := Controller.LevelCount - LevelIndex;
end;

function TcxTreeListBottomBandPlace.InternalCalculateMinWidth: Integer;
var
  I, V, J: Integer;
  ARow: TcxTreeListBandRow;
  AColumn: TcxTreeListColumn;
begin
  Result := 0;
  for I := 0 to Band.BandRows.VisibleItemCount - 1 do
  begin
    V := 0;
    ARow := Band.BandRows.VisibleItems[I];
    for J := 0 to ARow.VisibleItemCount - 1 do
    begin
      AColumn := ARow.VisibleItems[J];
      Inc(V, AColumn.MinWidth);
    end;
    if Result < V then Result := V;
  end;
end;

function TcxTreeListBottomBandPlace.InternalCalculateWidth: Integer;
var
  I, V, J: Integer;
  ARow: TcxTreeListBandRow;
  AColumn: TcxTreeListColumn;
begin
  Result := 0;
  for I := 0 to Band.BandRows.VisibleItemCount - 1 do
  begin
    V := 0;
    ARow := Band.BandRows.VisibleItems[I];
    for J := 0 to ARow.VisibleItemCount - 1 do
    begin
      AColumn := ARow.VisibleItems[J];
      Inc(V, AColumn.Width);
    end;
    if Result < V then Result := V;
  end;
end;

procedure TcxTreeListBottomBandPlace.WidthChanged;
begin
  inherited;
  CalculateItemsWidth;
end;

{ TcxTreeListItemPlaceController }

constructor TcxTreeListItemPlaceController.Create(
  AFormatter: TcxTreeListReportLinkFormatter);
begin
  inherited Create(AFormatter);
  FBottomItems := TList.Create;
  FItems := TObjectList.Create;
  FRootItems := TList.Create;
  FLevelHeights := TList.Create;

  FHeight := -1;
  FLevelCount := -1;
end;

destructor TcxTreeListItemPlaceController.Destroy;
begin
  FreeAndNil(FLevelHeights);
  FreeAndNil(FRootItems);
  FreeAndNil(FItems);
  FreeAndNil(FBottomItems);
  inherited;
end;

procedure TcxTreeListItemPlaceController.Calculate;
begin
  CalculateLevelHeights;
  CalculateItemWidths;
end;

procedure TcxTreeListItemPlaceController.Refresh;
begin
  FItems.Clear;
  AddItems;
  RefreshRootItems;
  RefreshBottomItems;
  RefreshItems;
end;

function TcxTreeListItemPlaceController.BottomIndexOf(AnItem: TcxTreeListCustomBandPlace): Integer;
begin
  Result := FBottomItems.IndexOf(AnItem);
end;

function TcxTreeListItemPlaceController.IndexOf(
  AnItem: TcxTreeListCustomBandPlace): Integer;
begin
  Result := FItems.IndexOf(AnItem);
end;

function TcxTreeListItemPlaceController.IndexOf(
  ABand: TcxTreeListBand): Integer;
begin
  for Result := 0 to ItemCount - 1 do
    if Items[Result].Band = ABand then
      Exit;
  Result := -1;
end;

function TcxTreeListItemPlaceController.RootIndexOf(
  ABand: TcxTreeListBand): Integer;
begin
  for Result := 0 to RootItemCount - 1 do
    if RootItems[Result].Band = ABand then
      Exit;
  Result := -1;
end;

function TcxTreeListItemPlaceController.RootIndexOf(
  AnItem: TcxTreeListCustomBandPlace): Integer;
begin
  Result := FRootItems.IndexOf(AnItem);
end;

function TcxTreeListItemPlaceController.CalculateHeaderLineCount: Integer;
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

function TcxTreeListItemPlaceController.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RootItemCount - 1 do
    Inc(Result, RootItems[I].Width);
end;

function TcxTreeListItemPlaceController.GetItemByColumn(
  AColumn: TcxTreeListColumn): TcxTreeListColumnPlace;
var
  ABandPlace: TcxTreeListCustomBandPlace;
begin
  ABandPlace := ItemsByBand[TcxTreeListColumn(AColumn).Position.Band];
  if ABandPlace <> nil then
    Result := ABandPlace.ItemsByColumn[AColumn]
  else
    Result := nil;
end;

procedure TcxTreeListItemPlaceController.AddItems;
var
  I: Integer;
begin
  FItems.Count := Formatter.BandCount;
  for I := 0 to ItemCount - 1 do
    FItems[I] := CreateItem(Formatter.Bands[I]);
end;

function TcxTreeListItemPlaceController.CreateItem(
  ABand: TcxTreeListBand): TcxTreeListCustomBandPlace;
begin
  Result := GetItemClass(ABand).Create(Self, nil, ABand);
end;

function TcxTreeListItemPlaceController.GetItemClass(
  ABand: TcxTreeListBand): TcxTreeListCustomBandPlaceClass;
begin
  if ABand.IsBottom then
    Result := TcxTreeListBottomBandPlace
  else
    Result := TcxTreeListBandPlace;
end;

procedure TcxTreeListItemPlaceController.RefreshBottomItems;
var
  I: Integer;
begin
  FBottomItems.Count := Adapter.BottomBandCount;
  for I := 0 to BottomItemCount - 1 do
    FBottomItems[I] := ItemsByBand[Adapter.BottomBands[I]];
end;

procedure TcxTreeListItemPlaceController.RefreshItems;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Refresh;
end;

procedure TcxTreeListItemPlaceController.RefreshRootItems;
var
  I: Integer;
begin
  FRootItems.Count := Adapter.RootBandCount;
  for I := 0 to RootItemCount - 1 do
    FRootItems[I] := ItemsByBand[Adapter.RootBands[I]];
end;

function TcxTreeListItemPlaceController.CalculateHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LevelCount - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TcxTreeListItemPlaceController.CalculateItemHeight(
  AnItem: TcxTreeListCustomBandPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AnItem.LevelIndex to AnItem.LevelIndex + AnItem.RowCount - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TcxTreeListItemPlaceController.CalculateItemLeftBound(
  AnItem: TcxTreeListCustomBandPlace): Integer;
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

function TcxTreeListItemPlaceController.CalculateItemTopBound(
  AnItem: TcxTreeListCustomBandPlace): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AnItem.LevelIndex - 1 do
    Inc(Result, LevelHeights[I]);
end;

function TcxTreeListItemPlaceController.CalculateLevelCount: Integer;
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

procedure TcxTreeListItemPlaceController.CalculateItemWidths;
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

procedure TcxTreeListItemPlaceController.CalculateLevelHeights;
var
  I, V: Integer;
  AItem: TcxTreeListCustomBandPlace;
begin
  FLevelHeights.Count := LevelCount;
  for I := 0 to ItemCount - 1 do
  begin
    AItem := Items[I];
    V := AItem.CalculateLevelHeight;
    if LevelHeights[AItem.LevelIndex] < V then
      LevelHeights[AItem.LevelIndex] := V;
  end;
end;

function TcxTreeListItemPlaceController.GetAutoWidth: Boolean;
begin
  Result := Formatter.AutoWidth or Adapter.AutoWidth;
end;

function TcxTreeListItemPlaceController.GetAvailableWidth: Integer;
begin
  Result := Formatter.AvailableWidth;
end;

function TcxTreeListItemPlaceController.GetBottomItem(
  AIndex: Integer): TcxTreeListCustomBandPlace;
begin
  Result := TcxTreeListCustomBandPlace(FBottomItems[AIndex]);
end;

function TcxTreeListItemPlaceController.GetBottomItemCount: Integer;
begin
  Result := FBottomItems.Count;
end;

function TcxTreeListItemPlaceController.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TcxTreeListItemPlaceController.GetItem(
  AIndex: Integer): TcxTreeListCustomBandPlace;
begin
  Result := TcxTreeListCustomBandPlace(FItems[AIndex]);
end;

function TcxTreeListItemPlaceController.GetItemByBand(
  ABand: TcxTreeListBand): TcxTreeListCustomBandPlace;
var
  AIndex: Integer;
begin
  AIndex := IndexOf(ABand);
  if AIndex <> -1 then
    Result := Items[IndexOf(ABand)]
  else
    Result := nil;
end;

function TcxTreeListItemPlaceController.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxTreeListItemPlaceController.GetLevelCount: Integer;
begin
  if FLevelCount = -1 then
    FLevelCount := CalculateLevelCount;
  Result := FLevelCount;
end;

function TcxTreeListItemPlaceController.GetLevelHeight(AIndex: Integer): Integer;
begin
  Result := Integer(FLevelHeights[AIndex]);
end;

function TcxTreeListItemPlaceController.GetRootItem(
  AIndex: Integer): TcxTreeListCustomBandPlace;
begin
  Result := TcxTreeListCustomBandPlace(FRootItems[AIndex]);
end;

function TcxTreeListItemPlaceController.GetRootItemCount: Integer;
begin
  Result := FRootItems.Count;
end;

procedure TcxTreeListItemPlaceController.SetLevelHeight(AIndex,
  AValue: Integer);
begin
  FLevelHeights[AIndex] := TObject(AValue);
end;

{ TcxTreeListReportLinkFormatter }

constructor TcxTreeListReportLinkFormatter.Create(ABuilder: TcxTreeListReportLinkBuilder);
begin
  inherited;
  FFont := TFont.Create;
  FLookAndFeelItems := TList.Create;
  FLevelInfos := TObjectList.Create;
  FNodes := TList.Create;
  FColumns := TList.Create;
  FBands := TList.Create;
  CreateNodeList;
  CreateLevelInfos;
  FTransparentColor := dxPSCore.dxDefaultContentColor;
  FItemPlaceController := TcxTreeListItemPlaceController.Create(Self);
end;

destructor TcxTreeListReportLinkFormatter.Destroy;
begin
  FreeAndNil(FItemPlaceController);
  FreeAndNil(FBands);
  FreeAndNil(FColumns);
  FreeAndNil(FNodes);
  FreeAndNil(FLevelInfos);
  FreeAndNil(FLookAndFeelItems);
  FreeAndNil(FFont);
  inherited;
end;

function TcxTreeListReportLinkFormatter.Adapter: TcxTreeListAdapter;
begin
  Result := Builder.Adapter;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeHost(AHost: TdxReportCell);
begin
  AHost.CellSides := [];
  AHost.Transparent := True;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeBandItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
var
  Band: TcxTreeListBand;
begin
  Band := Bands[AnIndex];
  SetViewParams(AnItem, GetBandItemViewParams(Band));
  RegisterLookAndFeelItem(AnItem, cesRaised);
  if HasBackgroundBitmap(tlsv_BandHeader) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_BandHeader);

  with AnItem as TdxReportCellImage do
  begin
    Data := TdxNativeInt(Band);
    EndEllipsis := Band.Caption.ShowEndEllipsis;
    if not Band.Caption.Glyph.Empty then
    begin
      Image := Band.Caption.Glyph;
      ImageLayout := HeaderImageLayoutMap[Band.Caption.GlyphAlignHorz, Band.Caption.GlyphAlignVert];
      ImageTransparent := True;
      IsTextDrawnForCenteredImage := True;
      IsTextShiftedForHorizontallyCenteredImage := not (ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]);
    end;
    Multiline := Band.Caption.Multiline;
    Text := Band.Caption.Text;
    TextAlignX := TextAlignXMap[Band.Caption.AlignHorz];
    TextAlignY := TextAlignYMap[Band.Caption.AlignVert];
  end;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeBandRow(ARow: TdxReportCell);
begin
  //SetViewParams(ARow, GetBandBackgroundViewParams);
  ARow.Transparent := True; //???
  ARow.Data := TdxNativeInt(TcxTreeListBandAttribute);
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeBandItem(
  AnItem: TAbstractdxReportCellData; AIndex: Integer);
begin
  ReportLink.DoInitializeBandCell(Bands[AIndex], TdxReportCellImage(AnItem));
end;

function TcxTreeListReportLinkFormatter.GetBandBackgroundViewParams: TdxReportItemViewParams;
begin
  Result.NativeParams := Adapter.GetBandBackgroundViewParams;
  Result.CellSides := [];
  Result.FontStyle := [];
  Result.Transparent := True;
end;

function TcxTreeListReportLinkFormatter.GetBandItemBounds(Index: Integer): TRect;
var
  Item: TcxTreeListCustomBandPlace;
begin
  Item := ItemPlaceController.ItemsByBand[Bands[Index]];
  if Item <> nil then
    Result := Item.Bounds
  else
    Result := cxNullRect;
end;

function TcxTreeListReportLinkFormatter.GetBandItemClass(Index: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellImage;
end;

function TcxTreeListReportLinkFormatter.GetBandItemViewParams(ABand: TcxTreeListBand): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
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
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeCheck(AnItem: TdxCustomReportCellCheck;
  ANode: TcxTreeListNode; AnIndex: Integer);
begin
  SetViewParams(AnItem, GetNodeCheckViewParams(ANode, AnIndex));
  if HasBackgroundBitmap(tlsv_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_Content);
  AnItem.Checked := ANode.CheckState <> cbsUnchecked;
  AnItem.Enabled := ANode.CheckState <> cbsGrayed;
  AnItem.Data := MakeIndentIndex(AnIndex);
  TdxReportCellCheck(AnItem).FlatBorder := ReportLink.OptionsRefinements.FlatCheckMarks;
end;

function TcxTreeListReportLinkFormatter.GetNodeCheckCellSides(ANode: TcxTreeListNode;
  AnIndex: Integer): TdxCellSides;
begin
  Result := [];
  if ReportLink.OptionsView.Borders and not HasStateImages(ANode) and
    not HasImages(ANode) then
      Result := [csRight];
end;

function TcxTreeListReportLinkFormatter.GetNodeCheckClass(ANode: TcxTreeListNode): TdxCustomReportCellCheckClass;
begin
  case ANode.Parent.CheckGroupType of
    ncgCheckGroup:
      Result := TdxReportCellCheck
  else {ncgRadioGroup}
    Result := TdxReportCellRadio;
  end;
end;

function TcxTreeListReportLinkFormatter.GetNodeCheckViewParams(ANode: TcxTreeListNode; AnIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetImagesParams(ANode, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetIndentViewParams(ANode, AnIndex);

  Result.CellSides := GetNodeCheckCellSides(ANode, AnIndex);
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeFooterCell(AnItem: TAbstractdxReportCellData;
  AColumn: TcxTreeListColumn; ACellIndex: Integer; ANode: TcxTreeListNode);
var
  ASummaryItem: TcxTreeListSummaryItem;
  I, AColumnIndex: Integer;
begin
  AColumnIndex := FColumns.IndexOf(AColumn);
  SetViewParams(AnItem, GetFooterCellViewParams(AColumn));
  RegisterLookAndFeelItem(AnItem, cesSunken);
  if HasBackgroundBitmap(tlsv_ColumnFooter) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_ColumnFooter);

  ASummaryItem := GetSummaryItems(ANode)[AColumnIndex, ACellIndex];

  with AnItem as TdxReportCellString do
  begin
    Data := TdxNativeInt(AColumn);
    Multiline := AColumn.Position.LineCount > 1;
    TextAlignX := TextAlignXMap[ASummaryItem.AlignHorz];
    TextAlignY := TextAlignYMap[ASummaryItem.AlignVert];
    if (GetFooterCellCount(AColumn, ANode) <> 1) or
      (Length(GetSummaryItems(ANode)[AColumnIndex]) = 1) then
      Text := ANode.FooterSummaryTexts[ASummaryItem.AbsoluteIndex]
    else
      for I := 0 to High(GetSummaryItems(ANode)[AColumnIndex]) do
      begin
        if Text <> '' then
          Text := Text + ';';
        Text := Text + ANode.FooterSummaryTexts[GetSummaryItems(ANode)[AColumnIndex, I].AbsoluteIndex];
      end;
    end;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeFooterIndent(AnItem: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode);
begin
  SetViewParams(AnItem, GetNodeIndentViewParams(ANode, AnIndex, AnIndentCount));
  if HasBackgroundBitmap(tlsv_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_Content);
  AnItem.Data := MakeIndentIndex(AnIndex);
  AnItem.ShowButton := False;
  if AnIndex = AnIndentCount - 1 then
    AnItem.CellSides := [csRight]
  else
    AnItem.CellSides := [];
  if ReportLink.OptionsView.TreeLines then
    AnItem.TreeLineMode := GetNodeFooterIndentTreeLineMode(AnIndex, AnIndentCount, ANode);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeFooterBandedRow(
  ARow: TdxReportCell; AIndentCount: Integer);
var
  ABandPlace: TcxTreeListCustomBandPlace;
  AIndex, ALastIndex: Integer;
begin
  SetViewParams(ARow, GetFooterRowViewParams);
  RegisterLookAndFeelItem(ARow, cesRaised);
  ABandPlace := ItemPlaceController.GetItemByBand(TcxTreeListBand(ARow.Data));
  ARow.CellSides := csTopBottom;
  AIndex := ABandPlace.Controller.BottomIndexOf(ABandPlace);
  ALastIndex := ABandPlace.Controller.BottomItemCount - 1;
  if (AIndex = 0) or ABandPlace.Expandable and (AIndentCount > 0) then
    ARow.CellSides := ARow.CellSides + [csLeft];
  if (AIndex = ALastIndex) or
    ABandPlace.Controller.BottomItems[AIndex + 1].Expandable and (AIndentCount > 0) then
    ARow.CellSides := ARow.CellSides + [csRight];
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeFooterRow(ARow: TdxReportCell);
begin
  ARow.Transparent := True;
  ARow.Data := TdxNativeInt(TcxTreeListFooterAttribute);
  if ReportLink.OptionsView.Borders then
    ARow.CellSides := csLeftRight
  else
    ARow.CellSides := [];
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeFooterCell(AnItem: TAbstractdxReportCellData;
  AColumnIndex, ACellIndex: Integer);
begin
  ReportLink.DoInitializeFooterCell(Columns[AColumnIndex], ACellIndex,
    TdxReportCellString(AnItem));
end;

function TcxTreeListReportLinkFormatter.GetFooterCellBounds(AColumn: TcxTreeListColumn;
  ACellIndex, AIndent: Integer): TRect;
var
  AItem: TcxTreeListColumnPlace;
  AColumnAccess: TcxTreeListColumnAccess;
begin
  AItem := ItemPlaceController.ItemsByColumn[AColumn];
  if AItem <> nil then
  begin
    Result := AItem.FooterCellBounds[ACellIndex];
    AColumnAccess := TcxTreeListColumnAccess(AColumn);
    if AColumnAccess.HasIndent then
      Inc(Result.Left, AIndent);
  end
  else
    Result := cxNullRect;
end;

function TcxTreeListReportLinkFormatter.GetFooterCellClass: TdxReportCellTextClass;
begin
  Result := TdxReportCellString;
end;

function TcxTreeListReportLinkFormatter.GetFooterCellViewParams(AColumn: TcxTreeListColumn): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetColumnFooterParams(nil, AColumn, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetColumnFooterViewParams(AColumn);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if AColumn <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedFooterItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedFooterItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
end;

function TcxTreeListReportLinkFormatter.GetNodeFooterIndentTreeLineMode(AnIndex, AnIndentCount: Integer;
  ANode: TcxTreeListNode): TdxPSTreeLineMode;

  function GetParentWithCertainLevel(ALevel: Integer): TcxTreeListNode;
  begin
    Result := ANode;
    while Result.Level > ALevel do
      Result := Result.Parent;
  end;

begin
  if not Adapter.ShowRoot then
    Inc(AnIndex);
  if IsNodeLastChild(GetParentWithCertainLevel(AnIndex)) then
    Result := tlmNone
  else
    Result := tlmVertical
end;

function TcxTreeListReportLinkFormatter.GetFooterRowViewParams: TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetFooterRowParams(Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetFooterRowViewParams;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeHeaderItem(AnItem: TAbstractdxReportCellData;
  AnIndex: Integer);
var
  AColumn: TcxTreeListColumn;
begin
  AColumn := Columns[AnIndex];
  SetViewParams(AnItem, GetHeaderItemViewParams(AColumn));
  RegisterLookAndFeelItem(AnItem, cesRaised);
  if HasBackgroundBitmap(tlsv_ColumnHeader) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_ColumnHeader);

  with AnItem as TdxReportCellImage do
  begin
    Data := TdxNativeInt(AColumn);
    EndEllipsis := AColumn.Caption.ShowEndEllipsis;
    if not AColumn.Caption.Glyph.Empty then
    begin
      Image := AColumn.Caption.Glyph;
      ImageLayout := HeaderImageLayoutMap[AColumn.Caption.GlyphAlignHorz, AColumn.Caption.GlyphAlignVert];
      IsTextDrawnForCenteredImage := True;
      IsTextShiftedForHorizontallyCenteredImage := not (ImageLayout in [ilImageTopCenter, ilImageCenterCenter, ilImageBottomCenter]);
    end;
    Multiline := AColumn.Caption.MultiLine;
    SortOrder := SortOrderMap[AColumn.SortOrder];
    Text := AColumn.Caption.Text;
    TextAlignX := TextAlignXMap[AColumn.Caption.AlignHorz];
    TextAlignY := TextAlignYMap[AColumn.Caption.AlignVert];
  end;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeHeaderRow(ARow: TdxReportCell);
begin
  SetViewParams(ARow, GetHeaderItemViewParams(nil));
  ARow.Data := TdxNativeInt(TcxTreeListHeaderAttribute);
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeHeaderItem(AnItem: TAbstractdxReportCellData;
  AIndex: Integer);
begin
  ReportLink.DoInitializeHeaderCell(Columns[AIndex], TdxReportCellImage(AnItem));
end;

function TcxTreeListReportLinkFormatter.GetHeaderItemBounds(AnIndex: Integer): TRect;
var
  AItem: TcxTreeListColumnPlace;
begin
  AItem := ItemPlaceController.ItemsByColumn[Columns[AnIndex]];
  if AItem <> nil then
    Result := AItem.HeaderCellBounds
  else
    Result := cxNullRect;
end;

function TcxTreeListReportLinkFormatter.GetHeaderItemClass(AnIndex: Integer): TdxReportCellTextClass;
begin
  Result := TdxReportCellImage;
end;

function TcxTreeListReportLinkFormatter.GetHeaderItemViewParams(AColumn: TcxTreeListColumn): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetColumnHeaderParams(AColumn, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetColumnHeaderViewParams(AColumn);

  if UseLookAndFeelColors and Adapter.CanUseLookAndFeelColors then
    if AColumn <> nil then
    begin
      Result.NativeParams.Color := Adapter.ThemedHeaderItemColor;
      Result.NativeParams.TextColor := Adapter.ThemedHeaderItemTextColor;
    end;

  Result.CellSides := csAll;
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeImage(AnItem: TdxReportCellGraphic;
  ANode: TcxTreeListNode; AnIndex: Integer);
begin
  SetViewParams(AnItem, GetNodeImageViewParams(ANode, AnIndex));
  if HasBackgroundBitmap(tlsv_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_Content);

  AnItem.Data := MakeIndentIndex(AnIndex);
  AnItem.DrawMode := gdmCenter;
  AnItem.ImageIndex := ANode.ImageIndex;
  AnItem.OverlayImageIndex := ANode.OverlayIndex;
  AnItem.ImageList := Images[ANode.Level];
  AnItem.ImageTransparent := True;
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeNodeImage(AnItem: TAbstractdxReportCellData;
  ANode: TcxTreeListNode; AnIndex: Integer);
begin
  ReportLink.DoInitializeIndentCell(ANode, AnIndex, AnItem);
end;

function TcxTreeListReportLinkFormatter.GetNodeImageCellSides(ANode: TcxTreeListNode;
  AnIndex: Integer): TdxCellSides;
begin
  Result := [];
  if ReportLink.OptionsView.Borders then
      Result := [csRight];
end;

function TcxTreeListReportLinkFormatter.GetNodeImageClass: TdxReportCellGraphicClass;
begin
  Result := TdxReportCellGraphic;
end;

function TcxTreeListReportLinkFormatter.GetNodeImageViewParams(ANode: TcxTreeListNode; AnIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetImagesParams(ANode, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetIndentViewParams(ANode, AnIndex);

  Result.CellSides := GetNodeImageCellSides(ANode, AnIndex);
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeIndent(AnItem: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode);
begin
  SetViewParams(AnItem, GetNodeIndentViewParams(ANode, AnIndex, AnIndentCount));
  if HasBackgroundBitmap(tlsv_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_Content);

  AnItem.Data := MakeIndentIndex(AnIndex);
  AnItem.ShowButton := (AnIndex = AnIndentCount - 1) and ANode.HasChildren and ReportLink.OptionsView.ExpandButtons;
  if AnItem.ShowButton then
    DoInitializeExpandButton(AnItem, AnIndex, AnIndentCount, ANode);
  if ReportLink.OptionsView.TreeLines then
    AnItem.TreeLineMode := GetNodeIndentTreeLineMode(AnIndex, AnIndentCount, ANode);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeExpandButton(AnItem: TdxReportCellExpandButton;
  AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode);
begin
  with AnItem do
  begin
    ButtonExpanded := IsNodeExpanded(ANode);
    ButtonSize := ExpandButtonSize;
    ButtonInteriorColor := ExpandButtonColor;
    ButtonTransparent := IsColorTransparent(ButtonInteriorColor);
  end;
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeNodeIndent(AnItem: TAbstractdxReportCellData;
  AnIndex, AnIndentCount: Integer; ANode: TcxTreeListNode);
begin
  ReportLink.DoInitializeIndentCell(ANode, AnIndex, AnItem);
end;

function TcxTreeListReportLinkFormatter.GetNodeIndentCellSides(ANode: TcxTreeListNode;
  AnIndex, AnIndentCount: Integer): TdxCellSides;
begin
  Result := [];
  if ReportLink.OptionsView.Borders and (AnIndex = AnIndentCount - 1) and
    not HasStateImages(ANode) and not HasImages(ANode) and not
      Adapter.HasCheck(ANode) then
      Result := [csRight];
end;

function TcxTreeListReportLinkFormatter.GetNodeIndentClass: TdxReportCellExpandButtonClass;
begin
  Result := TdxReportCellExpandButton;
end;

function TcxTreeListReportLinkFormatter.GetNodeIndentTreeLineMode(AnIndex,
  AnIndentCount: Integer; ANode: TcxTreeListNode): TdxPSTreeLineMode;
var
  Level: Integer;
  Parent: TcxTreeListNode;
begin
  Level := AnIndentCount - AnIndex - 1;
  Parent := GetNodeParent(ANode, Level);

  if Parent = ANode then
    if IsNodeLastChild(ANode) then
      Result := tlmBottomRightCorner
    else
      if not HasParent(ANode) and IsNodeFirstChild(ANode) then
        Result := tlmTopRightCorner
      else
        Result := tlmCross
  else
    if IsNodeLastChild(Parent) then
      Result := tlmNone
    else
      Result := tlmVertical;
end;

function TcxTreeListReportLinkFormatter.GetNodeIndentViewParams(
  ANode: TcxTreeListNode; AnIndex, AnIndentCount: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetIndentParams(ANode, AnIndex, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetIndentViewParams(ANode, AnIndex);

//  if not CanProcessSelectionStyle(ANode) and not IsNodeColorUsedForIndents then
//    Result.Color := ReportLink.Color;

  Result.CellSides := GetNodeIndentCellSides(ANode, AnIndex, AnIndentCount);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeBandedRow(ARow: TdxReportCell);
const
  CellSidesMap: array[Boolean] of TdxCellSides = ([], csAll);
begin
  ARow.CellSides := CellSidesMap[ReportLink.OptionsView.Borders];
  ARow.Transparent := True;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeRow(ARow: TdxReportCell;
  ANode: TcxTreeListNode);
const
  CellSidesMap: array[Boolean] of TdxCellSides = ([], csLeftRight);
begin
  ARow.CellSides := CellSidesMap[ReportLink.OptionsView.Borders];
  ARow.Data := TdxNativeInt(ANode);
  ARow.Transparent := True;

  ReportLink.AddReportRow(ARow);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeItem(AnItem: TAbstractdxReportCellData;
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; AnIsPreview: Boolean = False);

  function GetViewParams: TdxReportItemViewParams;
  begin
    if AnIsPreview then
      Result := GetPreviewViewParams(AColumn, ANode)
    else
      Result := GetNodeItemViewParams(AColumn, ANode);
  end;

var
  Properties: TcxCustomEditProperties;
  ViewParams: TdxReportItemViewParams;
  CellValue: TcxEditValue;
begin
  Properties := Adapter.GetProperties(AColumn, ANode);
  ViewParams := GetViewParams;
  CellValue := GetCellValue(Properties, AColumn, ANode);
  SetViewParams(AnItem, ViewParams);
  dxPScxCommon.dxPSDataMaps.InitializeItem(AnItem, Properties, CellValue, Self, ViewParams, AnIsPreview{, ANode.RecordIndex});
  dxPScxCommon.dxPSDataMaps.GetImageLists(Properties, ReportLink.AppendImageList);

  if (ViewParams.NativeParams.Bitmap <> nil) and not ViewParams.NativeParams.Bitmap.Empty then
    AnItem.BackgroundBitmapIndex := ReportLink.AddBackgroundBitmapToPool(ViewParams.NativeParams.Bitmap);

  AnItem.Data := TdxNativeInt(AColumn);
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeNodeItem(AnItem: TAbstractdxReportCellData;
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode);
begin
  ReportLink.DoInitializeCell(ANode, AColumn, AnItem);
end;

function TcxTreeListReportLinkFormatter.GetCellValue(AProperties: TcxCustomEditProperties;
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TcxEditValue;
begin
  if AProperties.GetEditValueSource(False) = evsValue then
    Result := ANode.Values[AColumn.ItemIndex]
  else
    Result := ANode.Texts[AColumn.ItemIndex];
end;

function TcxTreeListReportLinkFormatter.GetNodeBackgroundBitmapStyleIndex(ANode: TcxTreeListNode): Integer;

  function HasBackgroundBitmap(AStyle: TcxStyle): Boolean;
  begin
    Result := (AStyle <> nil) and (svBitmap in AStyle.AssignedValues);
  end;

begin
  Result := tlsv_Content;
  if IsOddNode(ANode) and HasBackgroundBitmap(ReportLink.Styles.StylesByCaption[cxGetResourceString(@sdxContentOddStyle)]) then
    Result := tlsv_ContentOdd
  else
    if not IsOddNode(ANode) and HasBackgroundBitmap(ReportLink.Styles.StylesByCaption[cxGetResourceString(@sdxContentEvenStyle)]) then
      Result := tlsv_ContentEven
end;

function TcxTreeListReportLinkFormatter.GetNodeItemBounds(ANode: TcxTreeListNode; AIndex: Integer): TRect;
var
  AItem: TcxTreeListColumnPlace;
begin
  AItem := ItemPlaceController.ItemsByColumn[Columns[AIndex]];
  if AItem <> nil then
    Result := AItem.CellBounds[ANode]
  else
    Result := cxNullRect;
end;

function TcxTreeListReportLinkFormatter.GetNodeItemCellSides(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode): TdxCellSides;
begin
  if ReportLink.OptionsView.Borders then
    Result := csAll
  else
    Result := [];
end;

function TcxTreeListReportLinkFormatter.GetNodeItemClass(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := dxPSDataMaps.ItemClass(Adapter.GetProperties(AColumn, ANode), GetNodeItemViewParams(AColumn, ANode), AnIsPreview);
end;

function TcxTreeListReportLinkFormatter.GetNodeItemViewParams(
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxReportItemViewParams;
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetContentParams(ANode, AColumn, Result.NativeParams)
  end
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetContentViewParams(ANode, AColumn);

  Result.CellSides := GetNodeItemCellSides(AColumn, ANode);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;

  if Adapter.TryGetAdvancedStyle(AColumn, ANode, AStyle) and (AStyle <> nil) then
    Result.AdvancedViewParams := TdxSpreadSheetAdvancedViewParams.CreateFrom(AStyle);
end;

procedure TcxTreeListReportLinkFormatter.DoInitializePreview(
  AnItem: TAbstractdxReportCellData; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode);
begin
  DoInitializeNodeItem(AnItem, AColumn, ANode, True);
end;

function TcxTreeListReportLinkFormatter.GetPreviewCellSides(ANode: TcxTreeListNode): TdxCellSides;
const
  CellSidesMap: array[Boolean] of TdxCellSides = ([], csAll);
begin
  Result := CellSidesMap[ReportLink.OptionsView.Borders];
end;

function TcxTreeListReportLinkFormatter.GetPreviewClass(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode): TdxReportCellDataClass;
begin
  Result := GetNodeItemClass(AColumn, ANode, True);
end;

function TcxTreeListReportLinkFormatter.GetPreviewViewParams(
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetPreviewParams(ANode, AColumn, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetPreviewViewParams(ANode, AColumn);

  Result.CellSides := GetPreviewCellSides(ANode);
  Result.FontStyle := [];
  Result.Transparent := {True;//}IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeSeparator(AnItem: TAbstractdxReportCellData);
begin
  with TdxReportCellBox(AnItem) do
  begin
    CellSides := csAll;
    Color := NodeSeparatorColor;
    Transparent := False;
  end;
end;

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeSeparatorRow(ARow: TdxReportCell);
begin
end;

function TcxTreeListReportLinkFormatter.GetNodeSeparatorClass: TdxReportCellBoxClass;
begin
  Result := TdxReportCellBox;
end;

{ state images }

procedure TcxTreeListReportLinkFormatter.DoInitializeNodeStateImage(AnItem: TdxReportCellGraphic;
  ANode: TcxTreeListNode; AnIndex: Integer);
begin
  SetViewParams(AnItem, GetNodeStateImageViewParams(ANode, AnIndex));
  if HasBackgroundBitmap(tlsv_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(tlsv_Content);

  AnItem.Data := MakeIndentIndex(AnIndex);
  AnItem.DrawMode := gdmCenter;
  AnItem.ImageIndex := ANode.StateIndex;
  AnItem.OverlayImageIndex := ANode.OverlayStateIndex;
  AnItem.ImageList := StateImages[ANode.Level];
  AnItem.ImageTransparent := True;
end;

procedure TcxTreeListReportLinkFormatter.DoReportLinkInitializeNodeStateImage(AnItem: TAbstractdxReportCellData;
  ANode: TcxTreeListNode; AnIndex: Integer);
begin
  ReportLink.DoInitializeIndentCell(ANode, AnIndex, AnItem);
end;

function TcxTreeListReportLinkFormatter.GetNodeStateImageCellSides(ANode: TcxTreeListNode;
  AnIndex: Integer): TdxCellSides;
begin
  Result := [];
  if ReportLink.OptionsView.Borders and not HasImages(ANode) then
      Result := [csRight];
end;

function TcxTreeListReportLinkFormatter.GetNodeStateImageClass: TdxReportCellGraphicClass;
begin
  Result := TdxReportCellGraphic;
end;

function TcxTreeListReportLinkFormatter.GetNodeStateImageViewParams(ANode: TcxTreeListNode;
  AnIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    if CanProcessSelectionStyle(ANode) then
      ReportLink.Styles.GetSelectionParams(Result.NativeParams)
    else
      ReportLink.Styles.GetImagesParams(ANode, Result.NativeParams)
  else
    if CanProcessSelectionStyle(ANode) then
      Result.NativeParams := Adapter.GetSelectionViewParams
    else
      Result.NativeParams := Adapter.GetIndentViewParams(ANode, AnIndex);

  Result.CellSides := GetNodeStateImageCellSides(ANode, AnIndex);
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent;
end;

function TcxTreeListReportLinkFormatter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxTreeListReportLinkFormatter._AddRef: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkFormatter._Release: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetAutoHeight: Boolean;
begin
  Result := Adapter.CellAutoHeight;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Canvas;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetDisplayGraphicsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayGraphicsAsText;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetDisplayTrackBarsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayTrackBarsAsText;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetEndEllipsis: Boolean;
begin
  Result := Adapter.CellEndEllipsis;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetFlatCheckMarks: Boolean;
begin
  Result := ReportLink.OptionsRefinements.FlatCheckMarks;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetGraphicsText: string;
begin
  Result := ReportLink.OptionsRefinements.GraphicsText;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetMultiline: Boolean;
begin
  Result := Adapter.CellMultiline;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams_GetTransparentGraphics: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentGraphics;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetPreviewMarginLeft: Integer;
begin
  Result := Adapter.PreviewLeftIndent;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetPreviewMarginRight: Integer;
begin
  Result := Adapter.PreviewRightIndent;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetPreviewMaxHeight: Integer;
begin
  if PreviewAutoHeight then
    Result := -1
  else
    Result := PreviewLineHeight * PreviewMaxLineCount;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetPreviewMaxLineCount: Integer;
begin
  Result := PreviewMaxLineCount;
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetRichEditGraphicClass: TGraphicClass;
const
  GraphicClasses: array[Boolean] of TGraphicClass = (TMetafile, TBitmap);
begin
  Result := GraphicClasses[IdxPSCellParams2_GetRichEditTransparent];
end;

function TcxTreeListReportLinkFormatter.IdxPSCellParams2_GetRichEditTransparent: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentRichEdits;
end;

procedure TcxTreeListReportLinkFormatter.AddDelimiters;
begin
  with ReportLink.OptionsPagination do
  begin
    if Band or Column then AddHorizontalDelimiters;
    if Node then AddVerticalDelimiters;
  end;
end;

procedure TcxTreeListReportLinkFormatter.AddHorizontalDelimiters;
var
  I: Integer;
  R: TRect;
  AColumnPlace: TcxTreeListColumnPlace;
  ABandPlace: TcxTreeListCustomBandPlace;
begin
  if ReportLink.OptionsPagination.Band then
  begin
    for I := 0 to BandCount - 1 do
    begin
      ABandPlace := ItemPlaceController.ItemsByBand[Bands[I]];
      if ABandPlace <> nil then
      begin
        R := ABandPlace.Bounds;
        ReportLink.AddHorizontalDelimiter(R.Left);
        ReportLink.AddHorizontalDelimiter(R.Right);
      end;
    end;
  end
  else
    for I := 0 to ColumnCount - 1 do
    begin
      AColumnPlace := ItemPlaceController.ItemsByColumn[Columns[I]];
      if AColumnPlace <> nil then
      begin
        R := AColumnPlace.HeaderCellBounds;
        ReportLink.AddHorizontalDelimiter(R.Left);
        ReportLink.AddHorizontalDelimiter(R.Right);
      end;
    end;
end;

procedure TcxTreeListReportLinkFormatter.AddVerticalDelimiters;
var
  I: Integer;
begin
  for I := 0 to Builder.ReportRowCount - 1 do
    ReportLink.AddVerticalDelimiter(Builder.ReportRows[I]);
end;

procedure TcxTreeListReportLinkFormatter.AfterBuilding;
begin
  if not Builder.IsAborted then
  begin
    FormatLookAndFeelItems;
    AddDelimiters;
    Builder.ReportCells.BorderColor := GridLinesColor;
    Builder.ReportCells.TreeLineColor := Adapter.TreeLinesColor;
    Builder.ReportCells.TreeLineStyle := TreeLinesStyleMap[Adapter.TreeLinesStyle];
  end;
end;

procedure TcxTreeListReportLinkFormatter.BeforeBuilding;
begin
  CreateItems;
  Adapter.Calculate;
  CalculateLineHeights;
  ItemPlaceController.Refresh;
  ItemPlaceController.Calculate;
end;

procedure TcxTreeListReportLinkFormatter.CalculateLineHeights;
var
  I: Integer;
  AColumn: TcxTreeListColumn;
begin
  FBandLineHeight := 0;
  if Adapter.IsDefaultRowHeightAssigned then
    FDetailsLineHeight := Adapter.DefaultRowHeight
  else
    FDetailsLineHeight := Max(FMaxNodeImageHeight, DefaultDataRowLineHeight);
  FFooterLineHeight := 0;
  FHeaderLineHeight := 0;

  for I := 0 to ColumnCount - 1 do
  begin
    AColumn := Columns[I];
    CalculateHeight(GetFooterCellViewParams(AColumn), FFooterLineHeight);
    CalculateHeight(GetHeaderItemViewParams(AColumn), FHeaderLineHeight);
    if not Adapter.IsDefaultRowHeightAssigned and not AColumn.IsPreview then
      CalculateHeight(GetNodeItemViewParams(AColumn, Nodes[0]), FDetailsLineHeight);
  end;
  Inc(FFooterLineHeight, 2 * FooterItemInflateVert);

  FPreviewLineHeight := Adapter.DefaultRowHeight;
  if PreviewColumn <> nil then
    CalculateHeight(GetPreviewViewParams(PreviewColumn, Nodes[0]), FPreviewLineHeight);

  for I := 0 to BandCount - 1 do
    CalculateHeight(GetBandItemViewParams(Bands[I]), FBandLineHeight);
end;

function TcxTreeListReportLinkFormatter.CanProcessSelectionStyle(ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode <> nil) and ReportLink.OptionsFormatting.ConsumeSelectionStyle and IsSelectedNode(ANode);
  if Result and not ReportLink.OptionsFormatting.UseNativeStyles then
    Result := Adapter.HasSelectionStyle;
end;

procedure TcxTreeListReportLinkFormatter.CreateLevelInfos;
var
  I: Integer;
  ALevelInfo: TcxTreeListReportLinkLevelInfo;
begin
  FMaxNodeImageHeight := 0;
  FLevelInfos.Clear;
  FLevelInfos.Capacity := FExpansionLevel + 1;
  for I := 0 to FExpansionLevel do
  begin
    ALevelInfo := TcxTreeListReportLinkLevelInfo.Create;
    FLevelInfos.Add(ALevelInfo);
    Adapter.DoGetLevelImages(I, ALevelInfo.FImages, ALevelInfo.FStateImages);
    if ALevelInfo.FImages <> nil then
      FMaxNodeImageHeight := Max(FMaxNodeImageHeight, ALevelInfo.FImages.Height);
    if ALevelInfo.FStateImages <> nil then
      FMaxNodeImageHeight := Max(FMaxNodeImageHeight, ALevelInfo.FStateImages.Height);
  end;
end;

procedure TcxTreeListReportLinkFormatter.CreateNodeList;

  function IsNodeProcessed(ANode: TcxTreeListNode): Boolean;
  var
    Helper: TcxTreeListNodeHelper;
  begin
    Helper := Adapter.Helpers[ANode];
    Result := not Adapter.MultiSelection or not ReportLink.OptionsSelection.ProcessSelection or
      (ANode.Selected or Helper.HasSelectedChildren or
      (not ReportLink.OptionsSelection.ProcessExactSelection and Helper.HasSelectedParents));
  end;

  procedure ProcessNode(ANode: TcxTreeListNode);
  var
    I: Integer;
    Node: TcxTreeListNode;
  begin
    if not ANode.Visible or TcxTreeListNodeAccess(ANode).HiddenByFilter then
      Exit;
    FNodes.Add(ANode);
    FExpansionLevel := Max(FExpansionLevel, ANode.Level);
    if (ANode.Count > 0) and (ReportLink.OptionsExpanding.ExplicitlyExpandNodes or
      (ANode.Expanded or (ReportLink.OptionsExpanding.AutoExpandNodes and ANode.CanExpand))) then
      for I := 0 to ANode.Count - 1 do
      begin
        Node := ANode[I];
        if IsNodeProcessed(Node) then ProcessNode(Node);
      end;
  end;

var
  I: Integer;
  ANode: TcxTreeListNode;
begin
  FNodes.Clear;
  FExpansionLevel := -1;
  for I := 0 to Adapter.NodeCount - 1 do
  begin
    ANode := Adapter.Nodes[I];
    if IsNodeProcessed(ANode) then ProcessNode(ANode);
  end;
end;

function TcxTreeListReportLinkFormatter.IsSelectedNode(ANode: TcxTreeListNode): Boolean;
begin
  Result := ReportLink.OptionsSelection.ProcessSelection or ANode.Selected;
end;

function TcxTreeListReportLinkFormatter.GetBackgroundBitmap(Index: Integer): TBitmap;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    Result := ReportLink.Styles.GetBitmap(MapStyleBackgroundBitmapIndex(Index))
  else
    Result := Adapter.GetBackgroundBitmap(Index);
end;

function TcxTreeListReportLinkFormatter.GetBackgroundBitmapIndex(Index: Integer): Integer;
begin
  Result := ReportLink.AddBackgroundBitmapToPool(GetBackgroundBitmap(Index));
end;

function TcxTreeListReportLinkFormatter.HasBackgroundBitmap(Index: Integer): Boolean;
var
  Bitmap: TBitmap;
begin
  if not ReportLink.OptionsFormatting.SuppressBackgroundBitmaps then
  begin
    Bitmap := GetBackgroundBitmap(Index);
    Result := (Bitmap <> nil) and not Bitmap.Empty;
  end
  else
    Result := False;
end;

function TcxTreeListReportLinkFormatter.MapStyleBackgroundBitmapIndex(ATreeListBackgroundBitmapIndex: Integer): Integer;
begin
  case ATreeListBackgroundBitmapIndex of
    tlsv_BandBackground:
      Result := vspsTreeListBandBackground;
    tlsv_BandHeader:
      Result := vspsTreeListBandHeader;
    tlsv_Content:
      Result := vspsTreeListContent;
    tlsv_ContentOdd:
      Result := vspsTreeListContentOdd;
    tlsv_ContentEven:
      Result := vspsTreeListContentEven;
    tlsv_ColumnFooter:
      Result := vspsTreeListFooter;
    tlsv_ColumnHeader:
      Result := vspsTreeListHeader;
    tlsv_Footer:
      Result := vspsTreeListFooterRow;
    tlsv_Preview:
      Result := vspsTreeListPreview;
  else
    Result := 0;
  end;
end;

procedure TcxTreeListReportLinkFormatter.CalculateHeight(const AParams: TdxReportItemViewParams;
  var AHeight: Integer);
begin
  AHeight := Max(AHeight, CalculatePatternHeight(AParams));
end;

function TcxTreeListReportLinkFormatter.CalculatePatternHeight(const AParams: TdxReportItemViewParams): Integer;
begin
  Result := Renderer.CalcTextPatternHeight(Canvas, AParams.NativeParams.Font);
end;

procedure TcxTreeListReportLinkFormatter.CreateBands;
var
  I: Integer;
  Band: TcxTreeListBand;
begin
  FBands.Clear;
  for I := 0 to Adapter.BandCount - 1 do
  begin
    Band := Adapter.Bands[I];
    if Band.ActuallyVisible then FBands.Add(Band);
  end;
end;

procedure TcxTreeListReportLinkFormatter.CreateColumns;
var
  I: Integer;
  AColumn: TcxTreeListColumn;
begin
  FColumns.Clear;
  for I := 0 to Adapter.ColumnCount - 1 do
  begin
    AColumn := Adapter.Columns[I];
    if AColumn.ActuallyVisible then FColumns.Add(AColumn);
  end;
end;

procedure TcxTreeListReportLinkFormatter.CreateItems;
begin
  CreateColumns;
  CreateBands;
  CreateSummaryItems;
end;

procedure TcxTreeListReportLinkFormatter.CreateSummaryItems;

  procedure InternalCreateSummaryItems(AColumnIndex: Integer;
    AVisibleSummaryItems: TcxTreeListReportLinkSummaryItems; ASummaryItems: TcxTreeListSummaryItems);
  var
    ASummaryIndex, AVisibleIndex: Integer;
  begin
    AVisibleIndex := 0;
    SetLength(AVisibleSummaryItems[AColumnIndex], ASummaryItems.VisibleCount);
    for ASummaryIndex := 0 to ASummaryItems.Count - 1 do
      if ASummaryItems[ASummaryIndex].Visible then
        begin
          AVisibleSummaryItems[AColumnIndex, AVisibleIndex] := ASummaryItems[ASummaryIndex];
          Inc(AVisibleIndex);
        end;
  end;

var
  I: Integer;
begin
  SetLength(FGroupFooterSummaryItems, ColumnCount);
  SetLength(FFooterSummaryItems, ColumnCount);
  for I := 0 to ColumnCount - 1 do
  begin
    InternalCreateSummaryItems(I, FFooterSummaryItems,
      Columns[I].Summary.FooterSummaryItems);
    InternalCreateSummaryItems(I, FGroupFooterSummaryItems,
      Columns[I].Summary.GroupFooterSummaryItems);
  end;
end;

function TcxTreeListReportLinkFormatter.GetFooterCellCount(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Integer;
begin
  if HasFooterCell(AColumn, ANode) then
    Result := IfThen(Adapter.FMultiRows, 1, Length(GetSummaryItems(ANode)[FColumns.IndexOf(AColumn)]))
  else
    Result := 0;
end;

function TcxTreeListReportLinkFormatter.GetNodeIndent(ANode: TcxTreeListNode): Integer;
begin
  Result := Adapter.IndentWidth * IndentCounts[ANode] + GetImagesWidth(ANode) +
    GetStateImagesWidth(ANode) + Adapter.GetCheckWidth(ANode);
end;

function TcxTreeListReportLinkFormatter.GetNodeParent(ANode: TcxTreeListNode;
  ALevel: Integer): TcxTreeListNode;
begin
  Result := ANode;
  while ALevel > 0 do
  begin
    Result := Result.Parent;
    Dec(ALevel);
  end;
end;

function TcxTreeListReportLinkFormatter.GetNextNode(ANode: TcxTreeListNode): TcxTreeListNode;
var
  Index: Integer;
begin
  Index := IndexOfNode(ANode);
  if Index < NodeCount - 1 then
    Result := Nodes[Index + 1]
  else
    Result := nil;
end;

function TcxTreeListReportLinkFormatter.GetPrevNode(ANode: TcxTreeListNode): TcxTreeListNode;
var
  Index: Integer;
begin
  Index := IndexOfNode(ANode);
  if Index > 0 then
    Result := Nodes[Index - 1]
  else
    Result := nil;
end;

function TcxTreeListReportLinkFormatter.GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
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

function TcxTreeListReportLinkFormatter.GetSummaryItems(ANode: TcxTreeListNode): TcxTreeListReportLinkSummaryItems;
begin
  if ANode = Adapter.TreeList.Root then
    Result := FFooterSummaryItems
  else
    Result := FGroupFooterSummaryItems;
end;

function TcxTreeListReportLinkFormatter.HasFooterCell(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Boolean;
begin
  if ANode = Adapter.TreeList.Root then
    Result := AColumn.Options.Footer
  else
    Result := AColumn.Options.GroupFooter;
  Result := Result and (Length(GetSummaryItems(ANode)[FColumns.IndexOf(AColumn)]) > 0);
end;

function TcxTreeListReportLinkFormatter.HasImages(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := Images[ANode.Level] <> nil;
  if Result then
    Result := (ANode.ImageIndex > -1) and (ANode.ImageIndex < Images[ANode.Level].Count);
end;

function TcxTreeListReportLinkFormatter.HasParent(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.Level > 0;
end;

function TcxTreeListReportLinkFormatter.HasStateImages(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := StateImages[ANode.Level] <> nil;
  if Result then
    Result := (ANode.StateIndex > -1) and (ANode.StateIndex < StateImages[ANode.Level].Count);
end;

function TcxTreeListReportLinkFormatter.IndexOfNode(ANode: TcxTreeListNode): Integer;
begin
  Result := FNodes.IndexOf(ANode);
end;

function TcxTreeListReportLinkFormatter.IsColorTransparent(AColor: TColor): Boolean;
begin
  Result := ColorToRGB(AColor) = ColorToRGB(TransparentColor);
end;

function TcxTreeListReportLinkFormatter.IsFirstNode(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode = FNodes.First;
end;

function TcxTreeListReportLinkFormatter.IsLastNode(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode = FNodes.Last;
end;

function TcxTreeListReportLinkFormatter.IsNodeExpanded(ANode: TcxTreeListNode): Boolean;
var
  NextNode: TcxTreeListNode;
begin
  Result := ANode.Expanded;
  if not Result then
  begin
    NextNode := GetNextNode(ANode);
    Result := (NextNode <> nil) and (NextNode.Level > ANode.Level);
  end;
end;

function TcxTreeListReportLinkFormatter.IsNodeFirstChild(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.GetPrevSiblingVisible = nil;
end;

function TcxTreeListReportLinkFormatter.IsNodeLastChild(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.GetNextSiblingVisible = nil;
end;

function TcxTreeListReportLinkFormatter.IsOddNode(ANode: TcxTreeListNode): Boolean;
begin
  Result := Odd(FNodes.IndexOf(ANode));
end;

function TcxTreeListReportLinkFormatter.IsNodeSelected(ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.Selected;
end;

procedure TcxTreeListReportLinkFormatter.RegisterLookAndFeelItem(AnItem: TdxReportVisualItem;
  AEdgeStyle: TdxCellEdgeStyle);
begin
  AnItem.EdgeMode := cem3DEffects;
  AnItem.Edge3DStyle := AEdgeStyle;
  FLookAndFeelItems.Add(AnItem);
end;

procedure TcxTreeListReportLinkFormatter.SetViewParams(AnItem: TdxReportVisualItem;
  const AParams: TdxReportItemViewParams);
begin
  with AnItem do
  begin
    CellSides := AParams.CellSides;
    Color := ColorToRGB(AParams.NativeParams.Color);
    FontIndex := GetStyleFontIndex(AParams);
    Transparent := AParams.Transparent;
  end;
end;

function TcxTreeListReportLinkFormatter.MakeIndentIndex(AnIndex: Integer): DWORD;
begin
  Result := ReportLink.MakeIndentIndex(AnIndex);
end;

function TcxTreeListReportLinkFormatter.GetAutoWidth: Boolean;
begin
  Result := ReportLink.OptionsSize.AutoWidth;
end;

function TcxTreeListReportLinkFormatter.GetAvailableWidth: Integer;
var
  AControllerIntf: IdxReportLinkController;
  R: TRect;
begin
  if AutoWidth then
    if ReportLink.IsAggregated and
      Supports(TObject(ReportLink.Controller), IdxReportLinkController, AControllerIntf) then
      R := AControllerIntf.GetControlSiteBounds(ReportLink.TreeList)
    else
      R := ReportLink.RealPrinterPage.PaintRectPixels
  else
    R := Adapter.TreeList.ClientBounds;

  Result := R.Right - R.Left - 1;
end;

function TcxTreeListReportLinkFormatter.GetBandCount: Integer;
begin
  Result := FBands.Count;
end;

function TcxTreeListReportLinkFormatter.GetBands(AIndex: Integer): TcxTreeListBand;
begin
  Result := FBands[AIndex];
end;

function TcxTreeListReportLinkFormatter.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TcxTreeListReportLinkFormatter.GetColumns(AIndex: Integer): TcxTreeListColumn;
begin
  Result := FColumns[AIndex];
end;

function TcxTreeListReportLinkFormatter.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

function TcxTreeListReportLinkFormatter.GetExpandButtonColor: TColor;
begin
  Result := ExpandButtonInteriorColors[Adapter.TreeLinesStyle = tllsSolid];
end;

function TcxTreeListReportLinkFormatter.GetExpandButtonSize: Integer;
begin
  Result := DefaultExpandButtonSize;
end;

function TcxTreeListReportLinkFormatter.GetGridLinesColor: TColor;
begin
  Result := ReportLink.OptionsFormatting.GridLineColor;
  if Result = clDefault then
    Result := Adapter.GridLinesColor;
end;

function TcxTreeListReportLinkFormatter.GetHasNodeSeparator: Boolean;
begin
  Result := NodeSeparatorThickness > 0;
end;

function TcxTreeListReportLinkFormatter.GetHasPreview: Boolean;
begin
  Result := Adapter.HasPreview and ShowPreview and (PreviewAutoHeight or (PreviewMaxLineCount > 0));
end;

function TcxTreeListReportLinkFormatter.GetIndentCount(Node: TcxTreeListNode): Integer;
begin
  Result := Node.Level;
  if Adapter.ShowRoot then Inc(Result);
end;

function TcxTreeListReportLinkFormatter.GetImages(ALevel: Integer): TCustomImageList;
begin
  Result := LevelInfos[ALevel].FImages;
end;

function TcxTreeListReportLinkFormatter.GetImagesWidth(
  ANode: TcxTreeListNode): Integer;
begin
  if HasImages(ANode) then
    Result := 1 + Images[ANode.Level].Width + 1
  else
    Result := 0;
end;

function TcxTreeListReportLinkFormatter.GetIsNodeColorUsedForIndents: Boolean;
begin
  Result := Adapter.IsNodeColorUsedForIndents;
end;

function TcxTreeListReportLinkFormatter.GetLevelInfos(ALevel: Integer): TcxTreeListReportLinkLevelInfo;
begin
  Result := TcxTreeListReportLinkLevelInfo(FLevelInfos[ALevel]);
end;

function TcxTreeListReportLinkFormatter.GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
begin
  Result := TdxReportVisualItem(FLookAndFeelItems[Index]);
end;

function TcxTreeListReportLinkFormatter.GetLookAndFeelItemCount: Integer;
begin
  Result := FLookAndFeelItems.Count;
end;

function TcxTreeListReportLinkFormatter.GetNode(Index: Integer): TcxTreeListNode;
begin
  Result := TcxTreeListNode(FNodes[Index]);
end;

function TcxTreeListReportLinkFormatter.GetNodeCount: Integer;
begin
  Result := FNodes.Count;
end;

function TcxTreeListReportLinkFormatter.GetNodeSeparatorColor: TColor;
begin
  Result := ReportLink.OptionsFormatting.ActualNodeSeparatorColor;
end;

function TcxTreeListReportLinkFormatter.GetNodeSeparatorThickness: Integer;
begin
  Result := ReportLink.OptionsFormatting.NodeSeparatorThickness;
end;

function TcxTreeListReportLinkFormatter.GetPreviewAutoHeight: Boolean;
begin
  Result := ReportLink.OptionsPreview.AutoHeight;
end;

function TcxTreeListReportLinkFormatter.GetPreviewColumn: TcxTreeListColumn;
begin
  Result := Adapter.PreviewColumn;
end;

function TcxTreeListReportLinkFormatter.GetPreviewMaxLineCount: Integer;
begin
  Result := ReportLink.OptionsPreview.MaxLineCount;
end;

function TcxTreeListReportLinkFormatter.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportLink.Renderer;
end;

function TcxTreeListReportLinkFormatter.GetShowBandHeaders: Boolean;
begin
  Result := ReportLink.OptionsView.BandHeaders;
end;

function TcxTreeListReportLinkFormatter.GetShowFooters: Boolean;
begin
 Result := ReportLink.OptionsView.Footers;
end;

function TcxTreeListReportLinkFormatter.GetShowHeaders: Boolean;
begin
  Result := ReportLink.OptionsView.Headers;
end;

function TcxTreeListReportLinkFormatter.GetShowPreview: Boolean;
begin
  Result := ReportLink.OptionsPreview.Visible;
end;

function TcxTreeListReportLinkFormatter.GetStateImages(ALevel: Integer): TCustomImageList;
begin
  Result := LevelInfos[ALevel].FStateImages;
end;

function TcxTreeListReportLinkFormatter.GetStateImagesWidth(
  ANode: TcxTreeListNode): Integer;
begin
  if HasStateImages(ANode) then
    Result := 1 + StateImages[ANode.Level].Width + 1
  else
    Result := 0;
end;

function TcxTreeListReportLinkFormatter.GetUseLookAndFeelColors: Boolean;
begin
  Result := ReportLink.OptionsFormatting.UseLookAndFeelColors;
end;

function TcxTreeListReportLinkFormatter.GetViewWidth: Integer;
begin
  Result := ItemPlaceController.Width;
end;

function TcxTreeListReportLinkFormatter.GetTreeLinesColor: TColor;
begin
  Result := Adapter.TreeLinesColor;
end;

function TcxTreeListReportLinkFormatter.GetTreeLinesStyle: TcxTreeListTreeLineStyle;
begin
  Result := Adapter.TreeLinesStyle;
end;

procedure TcxTreeListReportLinkFormatter.FormatLookAndFeelItems;
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

{ TcxTreeListReportLinkOptionsExpanding }

procedure TcxTreeListReportLinkOptionsExpanding.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListReportLinkOptionsExpanding then
    with TcxTreeListReportLinkOptionsExpanding(Source) do
    begin
      Self.AutoExpandNodes := AutoExpandNodes;
      Self.ExplicitlyExpandNodes := ExplicitlyExpandNodes;
    end;
  inherited;
end;

procedure TcxTreeListReportLinkOptionsExpanding.RestoreDefaults;
begin
  inherited;
  AutoExpandNodes := False;
  ExplicitlyExpandNodes := False;
end;

function TcxTreeListReportLinkOptionsExpanding.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkOptionsExpanding.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

procedure TcxTreeListReportLinkOptionsExpanding.SetAutoExpandNodes(Value: Boolean);
begin
  if FAutoExpandNodes <> Value then
  begin
    FAutoExpandNodes := Value;
    Changed;
  end;
end;

procedure TcxTreeListReportLinkOptionsExpanding.SetExplicitlyExpandNodes(Value: Boolean);
begin
  if FExplicitlyExpandNodes <> Value then
  begin
    FExplicitlyExpandNodes := Value;
    Changed;
  end;
end;

{ TcxTreeListReportLinkOptionsFormatting }

procedure TcxTreeListReportLinkOptionsFormatting.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListReportLinkOptionsFormatting then
    with TcxTreeListReportLinkOptionsFormatting(Source) do
    begin
      Self.ConsumeSelectionStyle := ConsumeSelectionStyle;
      Self.NodeSeparatorColor := NodeSeparatorColor;
      Self.NodeSeparatorThickness := NodeSeparatorThickness;
    end;
  inherited;
end;

procedure TcxTreeListReportLinkOptionsFormatting.RestoreDefaults;
begin
  inherited;
  ConsumeSelectionStyle := False;
  NodeSeparatorColor := clDefault;
  NodeSeparatorThickness := 0;
end;

function TcxTreeListReportLinkOptionsFormatting.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TcxTreeListReportLinkOptionsFormatting.GetActualNodeSeparatorColor: TColor;
begin
  Result := NodeSeparatorColor;
  if Result = clDefault then
    Result := dxPSDefaultTreeListNodeSeparatorColor;
end;

function TcxTreeListReportLinkOptionsFormatting.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

procedure TcxTreeListReportLinkOptionsFormatting.SetConsumeSelectionStyle(Value: Boolean);
begin
  if FConsumeSelectionStyle <> Value then
  begin
    FConsumeSelectionStyle := Value;
    Changed;
  end;
end;

procedure TcxTreeListReportLinkOptionsFormatting.SetNodeSeparatorColor(Value: TColor);
begin
  if FNodeSeparatorColor <> Value then
  begin
    FNodeSeparatorColor := Value;
    if NodeSeparatorThickness <> 0 then Changed;
  end;
end;

procedure TcxTreeListReportLinkOptionsFormatting.SetNodeSeparatorThickness(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FNodeSeparatorThickness <> Value then
  begin
    FNodeSeparatorThickness := Value;
    Changed;
  end;
end;

{ TcxTreeListReportLinkOptionsOnEveryPage }

function TcxTreeListReportLinkOptionsOnEveryPage.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TcxTreeListReportLinkOptionsOnEveryPage.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkOptionsPagination }

function TcxTreeListReportLinkOptionsPagination.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TcxTreeListReportLinkOptionsPagination.GetNode: Boolean;
begin
  Result := inherited Row;
end;

function TcxTreeListReportLinkOptionsPagination.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

procedure TcxTreeListReportLinkOptionsPagination.SetNode(Value: Boolean);
begin
  inherited Row := Value;
end;

{ TcxTreeListReportLinkOptionsPreview }

function TcxTreeListReportLinkOptionsPreview.DesignerTabIndex: Integer;
begin
  Result := 4;
end;

function TcxTreeListReportLinkOptionsPreview.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkOptionsRefinements }

function TcxTreeListReportLinkOptionsRefinements.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TcxTreeListReportLinkOptionsRefinements.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkOptionsSelection }

function TcxTreeListReportLinkOptionsSelection.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkOptionsSelection.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkOptionsSize }

function TcxTreeListReportLinkOptionsSize.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TcxTreeListReportLinkOptionsSize.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkOptionsView }

procedure TcxTreeListReportLinkOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListReportLinkOptionsView then
    with TcxTreeListReportLinkOptionsView(Source) do
    begin
      Self.Borders := Borders;
      Self.TreeLines := TreeLines;
    end;
  inherited;
end;

procedure TcxTreeListReportLinkOptionsView.RestoreDefaults;
begin
  inherited;
  Borders := True;
  TreeLines := True;
end;

function TcxTreeListReportLinkOptionsView.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TcxTreeListReportLinkOptionsView.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

procedure TcxTreeListReportLinkOptionsView.SetBorders(Value: Boolean);
begin
  if FBorders <> Value then
  begin
    FBorders := Value;
    Changed;
  end;
end;

procedure TcxTreeListReportLinkOptionsView.SetTreeLines(Value: Boolean);
begin
  if FTreeLines <> Value then
  begin
    FTreeLines := Value;
    Changed;
  end;
end;

{ TcxTreeListReportLinkStyles }

procedure TcxTreeListReportLinkStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxTreeListReportLinkStyles then
    with TcxTreeListReportLinkStyles(Source) do
    begin
      Self.BandHeader := BandHeader;
      Self.Content := Content;
      Self.ContentEven := ContentEven;
      Self.ContentOdd := ContentOdd;
      Self.Footer := Footer;
      Self.FooterRow := FooterRow;
      Self.Header := Header;
      Self.Preview := Preview;
    end;
end;

procedure TcxTreeListReportLinkStyles.GetBandHeaderParams(ABand: TcxTreeListBand;
  out AParams: TcxViewParams);
begin
  GetViewParams(vspsTreeListBandHeader, ABand, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetColumnFooterParams(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; out AParams: TcxViewParams);
begin
  GetViewParams(vspsTreeListFooter, ANode, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetColumnHeaderParams(AColumn: TcxTreeListColumn;
  out AParams: TcxViewParams);
begin
  if AColumn <> nil then
    GetViewParams(vspsTreeListHeader, nil, nil, AParams)
  else
    GetViewParams(vspsTreeListBandBackground, nil, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetContentParams(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; out AParams: TcxViewParams);

  function IsOddNode(ANode: TcxTreeListNode): Boolean;
  var
    Builder: TcxTreeListReportLinkBuilder;
  begin
    Builder := ReportLink.Builder;
    if Builder <> nil then
      Result := Builder.Formatter.IsOddNode(ANode)
    else
      Result := Odd(ANode.VisibleIndex);
  end;

const
  StyleIndexes: array[Boolean] of Integer = (vspsTreeListContentEven, vspsTreeListContentOdd);
begin
  if (ANode <> nil) and (GetValue(StyleIndexes[IsOddNode(ANode)]) <> nil) then
    GetViewParams(StyleIndexes[IsOddNode(ANode)], ANode, nil, AParams)
  else
    GetViewParams(vspsTreeListContent, ANode, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetFooterRowParams(out AParams: TcxViewParams);
begin
  GetViewParams(vspsTreeListFooterRow, nil, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetImagesParams(ANode: TcxTreeListNode;
  out AParams: TcxViewParams);
begin
  GetContentParams(ANode, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetIndentParams(ANode: TcxTreeListNode;
  AnIndent: Integer; out AParams: TcxViewParams);

  function IsNodeColorUsedForIndents: Boolean;
  var
    Builder: TcxTreeListReportLinkBuilder;
  begin
    Builder := ReportLink.Builder;
    Result := (Builder <> nil) and Builder.Formatter.IsNodeColorUsedForIndents;
  end;

begin
  if IsNodeColorUsedForIndents then
    GetContentParams(ANode, nil, AParams)
  else
    GetViewParams(vspsTreeListIndent, ANode, nil{AnIndent}, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetPreviewParams(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; out AParams: TcxViewParams);
begin
  GetViewParams(vspsTreeListPreview, ANode, nil, AParams);
end;

procedure TcxTreeListReportLinkStyles.GetSelectionParams(out AParams: TcxViewParams);
begin
  GetViewParams(vspsTreeListSelection, nil, nil, AParams);
end;

function TcxTreeListReportLinkStyles.DesignerTabIndex: Integer;
begin
  Result := 3;
end;

procedure TcxTreeListReportLinkStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
const
  FixedParts = [vspsTreeListBandHeader, vspsTreeListFooter, vspsTreeListFooterRow,
    vspsTreeListHeader, vspsTreeListSelection];
begin
  inherited;
  if ReportLink <> nil then
    with AParams do
    begin
      if Index in FixedParts then
        Color := dxPSCore.dxDefaultFixedColor
      else
        Color := dxPSCore.dxDefaultContentColor; //TreeListReportLink.Color;  // ???

      Font := ReportLink.Font;
      TextColor := Font.Color;
    end;
end;

class function TcxTreeListReportLinkStyles.GetStyleCaption(AnIndex: Integer): string;
begin
  case AnIndex of
    vspsTreeListBandBackground:
      Result := cxGetResourceString(@sdxBandBackgroundStyle);
    vspsTreeListBandHeader:
      Result := cxGetResourceString(@sdxBandHeaderStyle);
    vspsTreeListContent:
      Result := cxGetResourceString(@sdxContentStyle);
    vspsTreeListContentEven:
      Result := cxGetResourceString(@sdxContentEvenStyle);
    vspsTreeListContentOdd:
      Result := cxGetResourceString(@sdxContentOddStyle);
    vspsTreeListFooter:
      Result := cxGetResourceString(@sdxFooterStyle);
    vspsTreeListFooterRow:
      Result := cxGetResourceString(@sdxFooterRowStyle);
    vspsTreeListHeader:
      Result := cxGetResourceString(@sdxHeaderStyle);
    vspsTreeListIndent:
      Result := cxGetResourceString(@sdxIndentStyle);
    vspsTreeListPreview:
      Result := cxGetResourceString(@sdxPreviewStyle);
  else
    Result := cxGetResourceString(@sdxSelectionStyle);
  end;
end;

function TcxTreeListReportLinkStyles.GetStyleIndexByCaption(const Caption: string): Integer;
begin
  for Result := vspsTreeListFirst to vspsTreeListLast do
    if dxPSUtl.dxSameText(Caption, GetStyleCaption(Result)) then
      Exit;
  Result := -1;
end;

function TcxTreeListReportLinkStyles.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

{ TcxTreeListReportLinkStyleSheet }

class function TcxTreeListReportLinkStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxTreeListReportLinkStyles;
end;

function TcxTreeListReportLinkStyleSheet.GetStylesValue: TcxTreeListReportLinkStyles;
begin
  if GetStyles is TcxTreeListReportLinkStyles then
    Result := TcxTreeListReportLinkStyles(GetStyles)
  else
    Result := nil;
end;

procedure TcxTreeListReportLinkStyleSheet.SetStylesValue(Value: TcxTreeListReportLinkStyles);
begin
  SetStyles(Value);
end;

{ TcxTreeListAttributeHostInfo }

procedure TcxTreeListAttributeHostInfo.Initialize(AParent: TdxReportCell);
begin
  Origin := cxNullPoint;
  FParent := AParent;
end;

{ TcxTreeListAttributeHostInfoServices }

constructor TcxTreeListAttributeHostInfoServices.Create(AReportLink: TcxTreeListCustomReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  CreateHostInfos;
end;

destructor TcxTreeListAttributeHostInfoServices.Destroy;
begin
  DestroyHostInfos;
  inherited;
end;

procedure TcxTreeListAttributeHostInfoServices.InitializeHostInfos;
begin
  PageDetailsHostInfo.Initialize(PageDetails);
  PageFootersHostInfo.Initialize(PageFooters);
  PageHeadersHostInfo.Initialize(PageHeaders);
end;

procedure TcxTreeListAttributeHostInfoServices.CreateHostInfos;
begin
  FPageDetailsHostInfo := TcxTreeListAttributeHostInfo.Create;
  FPageFootersHostInfo := TcxTreeListAttributeHostInfo.Create;
  FPageHeadersHostInfo := TcxTreeListAttributeHostInfo.Create;
end;

procedure TcxTreeListAttributeHostInfoServices.DestroyHostInfos;
begin
  FreeAndNil(FPageHeadersHostInfo);
  FreeAndNil(FPageFootersHostInfo);
  FreeAndNil(FPageDetailsHostInfo);
end;

function TcxTreeListAttributeHostInfoServices.GetBandHeadersHostInfo: TcxTreeListAttributeHostInfo;
begin
  if CanUseBandHeadersOnEveyPage then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TcxTreeListAttributeHostInfoServices.GetFootersHostInfo: TcxTreeListAttributeHostInfo;
begin
  if CanUseFootersOnEveryPage then
    Result := PageFootersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TcxTreeListAttributeHostInfoServices.GetHeadersHostInfo: TcxTreeListAttributeHostInfo;
begin
  if CanUseHeadersOnEveryPage then
    Result := PageHeadersHostInfo
  else
    Result := PageDetailsHostInfo;
end;

function TcxTreeListAttributeHostInfoServices.GetInconsistentStateText: string;
begin
  if ReportLink.IsAggregated then
    Result := cxGetResourceString(@sdxCannotUseOnEveryPageModeInAggregatedState)
  else
    if IsInconsistentHeadersState then
      Result := cxGetResourceString(@sdxTLIncorrectHeadersState)
    else
      Result := '';
end;

function TcxTreeListAttributeHostInfoServices.GetIsInconsistentState: Boolean;
begin
  Result := ReportLink.IsAggregated or IsInconsistentHeadersState;
end;

function TcxTreeListAttributeHostInfoServices.HasCells: Boolean;
begin
  Result := ReportLink.ReportCells <> nil;
end;

function TcxTreeListAttributeHostInfoServices.GetArePageFootersAssigned: Boolean;
begin
  Result := FootersHostInfo.Parent = ReportLink.ReportCells.FooterCells;
end;

function TcxTreeListAttributeHostInfoServices.GetArePageHeadersAssigned: Boolean;
begin
  with ReportLink.ReportCells do
    Result := (BandHeadersHostInfo.Parent = HeaderCells) or (HeadersHostInfo.Parent = HeaderCells);
end;

function TcxTreeListAttributeHostInfoServices.GetCanUseBandHeadersOnEveyPage: Boolean;
begin
  Result := not ReportLink.IsAggregated and OptionsOnEveryPage.BandHeaders;
end;

function TcxTreeListAttributeHostInfoServices.GetCanUseFootersOnEveryPage: Boolean;
begin
  Result := not ReportLink.IsAggregated and OptionsOnEveryPage.Footers;
end;

function TcxTreeListAttributeHostInfoServices.GetCanUseHeadersOnEveryPage: Boolean;
begin
  Result := not ReportLink.IsAggregated and OptionsOnEveryPage.Headers and
    ((BandHeadersHostInfo = PageHeadersHostInfo) or not OptionsView.BandHeaders);
end;

function TcxTreeListAttributeHostInfoServices.GetIsInconsistentHeadersState: Boolean;
begin
  Result := OptionsOnEveryPage.Headers and OptionsView.Headers and OptionsView.BandHeaders and not OptionsOnEveryPage.BandHeaders;
end;

function TcxTreeListAttributeHostInfoServices.GetOptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage;
begin
  Result := ReportLink.OptionsOnEveryPage;
end;

function TcxTreeListAttributeHostInfoServices.GetOptionsView: TcxTreeListReportLinkOptionsView;
begin
  Result := ReportLink.OptionsView;
end;

function TcxTreeListAttributeHostInfoServices.GetPageDetails: TdxReportCell;
begin
  if HasCells then
    Result := ReportLink.ReportCells.Cells
  else
    Result := nil;
end;

function TcxTreeListAttributeHostInfoServices.GetPageFooters: TdxReportCell;
begin
  if HasCells then
    Result := ReportLink.ReportCells.FooterCells
  else
    Result := PageDetails;
end;

function TcxTreeListAttributeHostInfoServices.GetPageHeaders: TdxReportCell;
begin
  if HasCells then
    Result := ReportLink.ReportCells.HeaderCells
  else
    Result := PageDetails;
end;

{ TcxTreeListCustomReportLink }

constructor TcxTreeListCustomReportLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelimitersHardVert := TList.Create;
  FDelimitersHardHorz := TList.Create;
  FHostInfoServices := TcxTreeListAttributeHostInfoServices.Create(Self);
  FReportRows := TList.Create;
  InternalRestoreDefaults;
  LinkModified(False);
end;

destructor TcxTreeListCustomReportLink.Destroy;
begin
  FreeAndNil(FReportRows);
  FreeAndNil(FHostInfoServices);
  FreeAndNil(FDelimitersHardVert);
  FreeAndNil(FDelimitersHardHorz);
  inherited Destroy;
end;

procedure TcxTreeListCustomReportLink.AddHorizontalPageBreak(AColumn: TcxTreeListColumn);
var
  ACellData: TAbstractdxReportCellData;
begin
  ACellData := ReportCellDataByColumn[AColumn];
  if Assigned(ACellData) then
    AddHorizontalHardDelimiter(ACellData.AbsoluteOrigin.X);
end;

procedure TcxTreeListCustomReportLink.AddHorizontalPageBreak(const AColumns: array of TcxTreeListColumn);
var
  I: Integer;
begin
  for I := 0 to Length(AColumns) - 1 do
    AddHorizontalPageBreak(AColumns[I]);
end;

procedure TcxTreeListCustomReportLink.AddHorizontalPageBreak(const AColumns: TcxTreeListColumnArray);
var
  I: Integer;
begin
  for I := 0 to Length(AColumns) - 1 do
    AddHorizontalPageBreak(AColumns[I]);
end;

procedure TcxTreeListCustomReportLink.AddHorizontalPageBreak(AColumns: TList);
var
  I: Integer;
begin
  for I := 0 to AColumns.Count - 1 do
    AddHorizontalPageBreak(TcxTreeListColumn(AColumns[I]));
end;

procedure TcxTreeListCustomReportLink.AddPageBreak(ANode: TcxTreeListNode);
var
  ReportRow: TdxReportCell;
begin
  ReportRow := ReportRowsByNode[ANode];
  if ReportRow <> nil then
    AddVerticalHardDelimiter(ReportRow);
end;

procedure TcxTreeListCustomReportLink.AddPageBreak(const ANodes: array of TcxTreeListNode);
var
  I: Integer;
begin
  for I := Low(ANodes) to High(ANodes) do
    AddPageBreak(ANodes[I]);
end;

procedure TcxTreeListCustomReportLink.AddPageBreak(const ANodes: TcxTreeListNodeArray);
var
  I: Integer;
begin
  for I := 0 to Length(ANodes) - 1 do
    AddPageBreak(ANodes[I]);
end;

procedure TcxTreeListCustomReportLink.AddPageBreak(ANodes: TList);
var
  I: Integer;
  P:  Pointer ;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    P := ANodes[I];
    if TObject(P) is TcxTreeListNode then
      AddPageBreak(TcxTreeListNode(P));
  end;
end;

procedure TcxTreeListCustomReportLink.ConstructReport(AReportCells: TdxReportCells);

  function HasData: Boolean;
  begin
    Result := (CustomTreeList_GetVisibleColumnCount(TreeList) <> 0) and (TreeList.AbsoluteVisibleCount <> 0);
  end;

  procedure Build;
  begin
    FBuilder := CreateBuilder;
    try
      FBuilder.Build;
    finally
      FreeAndNil(FBuilder);
    end;
  end;

  procedure CalculateSizes;

    procedure CalculateReportPartSizes(ACell: TdxReportCell);
    var
      Width, I: Integer;
    begin
      if ACell.CellCount > 0 then
      begin
        Width := 0;
        for I := 0 to ACell.CellCount - 1 do
          Width := Max(Width, ACell.Cells[I].Width);
        ACell.BoundsRect := Rect(0, 0, Width, ACell[ACell.CellCount - 1].BoundsRect.Bottom);
      end;
    end;

  begin
    with AReportCells do
    begin
      CalculateReportPartSizes(Cells);
      if AreFooterCellsAllocated then
        CalculateReportPartSizes(FooterCells);
      if AreHeaderCellsAllocated then
        CalculateReportPartSizes(HeaderCells);
    end;
  end;

begin
  if TreeList = nil then Exit;
  inherited ConstructReport(AReportCells);
  if not HasData then Exit;

  PrepareConstruct;
  try
    Build;
    if not AbortBuilding then
      CalculateSizes;
  finally
    UnprepareConstruct;
  end;
end;

procedure TcxTreeListCustomReportLink.ConvertCoords;
begin
  inherited ConvertCoords;
  ConvertDelimiters(DelimitersHardVert);
  ConvertDelimiters(DelimitersHardHorz);
end;

procedure TcxTreeListCustomReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  DrawInfo: TcxTreeListCellCustomDrawInfo;
begin
  GetItemCustomDrawInfo(AItem, DrawInfo);
  with DrawInfo do
    case AttributeID of
      cxTreeListBandID:
        DoCustomDrawBandCell(ACanvas, Band, TdxReportCellImage(AItem), ADone);
      cxTreeListFooterID:
        DoCustomDrawFooterCell(ACanvas, Column, TdxReportCellString(AItem), ADone);
      cxTreeListHeaderID:
        DoCustomDrawHeaderCell(ACanvas, Column, TdxReportCellImage(AItem), ADone);
      cxTreeListIndentID:
        DoCustomDrawIndentCell(ACanvas, Node, Index, AItem, ADone);
      cxTreeListNodeID:
        DoCustomDrawCell(ACanvas, Node, Column, AItem, ADone);
    end;
end;

function TcxTreeListCustomReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := OptionsPagination.Custom;
end;

procedure TcxTreeListCustomReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(CustomTreeList_GetImages(TreeList));
  AProc(CustomTreeList_GetStateImages(TreeList));
end;

function TcxTreeListCustomReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := (AUpdateCodes * uaMarginsVert  <> []) and OptionsSize.AutoWidth;
end;

function TcxTreeListCustomReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := OptionsPagination.Custom;
end;

procedure TcxTreeListCustomReportLink.InternalRestoreFromOriginal;
var
  TreeListOptionsView: TcxTreeListOptionsView;
  TreeListPreview: TcxTreeListPreview;
begin
  inherited;
  if TreeList <> nil then
  begin
    OptionsFormatting.LookAndFeelKind := CustomTreeList_GetLookAndFeel(TreeList).Kind;

    TreeListPreview := CustomTreeList_GetPreview(TreeList);
    with OptionsPreview do
    begin
      AutoHeight := TreeListPreview.AutoHeight;
      MaxLineCount := TreeListPreview.MaxLineCount;
      Visible := TreeListPreview.Visible;
    end;

    TreeListOptionsView :=  CustomTreeList_GetOptionsView(TreeList);
    with OptionsView do
    begin
      BandHeaders := TreeListOptionsView.Bands;
      ExpandButtons := TreeListOptionsView.Buttons;
      Footers := TreeListOptionsView.Footer;
      Headers := TreeListOptionsView.Headers;
      TreeLines := TreeListOptionsView.TreeLineStyle <> tllsNone;
    end;
    OptionsSize.AutoWidth := TreeListOptionsView.ColumnAutoWidth;
  end;
end;

function TcxTreeListCustomReportLink.IsDrawFootersOnEveryPage: Boolean;
begin
  Result := HostInfoServices.ArePageFootersAssigned;
end;

function TcxTreeListCustomReportLink.IsDrawHeadersOnEveryPage: Boolean;
begin
  Result := HostInfoServices.ArePageHeadersAssigned;
end;

function TcxTreeListCustomReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
var
  DrawInfo: TcxTreeListCellCustomDrawInfo;
begin
  Result := inherited IsSupportedCustomDraw(Item) and
    (Item <> nil) and IsCustomDrawn(GetItemCustomDrawInfo(Item, DrawInfo));
end;

procedure TcxTreeListCustomReportLink.MakeHardDelimiters(
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

function TcxTreeListCustomReportLink.GetAreNativeStylesAvailable: Boolean;
begin
  Result := OptionsFormatting.UseNativeStyles;
end;

function TcxTreeListCustomReportLink.GetStylesClass: TdxCustomReportLinkStylesClass;
begin
  Result := TcxTreeListReportLinkStyles;
end;

function TcxTreeListCustomReportLink.GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass;
begin
  Result := TcxTreeListReportLinkStyleSheet;
end;

function TcxTreeListCustomReportLink.GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet;
begin
  Result := DefaultdxPScxTreeListLinkStyleSheet;
end;

procedure TcxTreeListCustomReportLink.PrepareConstruct;
begin
  inherited PrepareConstruct;
  DelimitersHardHorz.Clear;
  DelimitersHardVert.Clear;
  FReportRows.Clear;
  ReportCells.LookAndFeel := nil;//CreateGroupLookAndFeel(TdxPSReportGroupNullLookAndFeel);

  HostInfoServices.InitializeHostInfos;
  if OptionsExpanding.ExplicitlyExpandNodes then
    TreeList.FullExpand;
end;

procedure TcxTreeListCustomReportLink.DoCustomDrawBandCell(ACanvas: TCanvas;
  ABand: TcxTreeListBand; AnItem: TdxReportCellImage; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawBandCell) then
    FOnCustomDrawBandCell(Self, ACanvas, ABand, AnItem, ADone);
end;

procedure TcxTreeListCustomReportLink.DoCustomDrawCell(ACanvas: TCanvas;
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
  AnItem: TAbstractdxReportCellData; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawCell) then
    FOnCustomDrawCell(Self, ACanvas, ANode, AColumn, AnItem, ADone);
end;

procedure TcxTreeListCustomReportLink.DoCustomDrawFooterCell(ACanvas: TCanvas;
  AColumn: TcxTreeListColumn; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawFooterCell) then
    FOnCustomDrawFooterCell(Self, ACanvas, AColumn, AnItem, ADone);
end;

procedure TcxTreeListCustomReportLink.DoCustomDrawHeaderCell(ACanvas: TCanvas;
  AColumn: TcxTreeListColumn; AnItem: TdxReportCellImage; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawHeaderCell) then
    FOnCustomDrawHeaderCell(Self, ACanvas, AColumn, AnItem, ADone);
end;

procedure TcxTreeListCustomReportLink.DoCustomDrawIndentCell(ACanvas: TCanvas;
  ANode: TcxTreeListNode; AnIndex: Integer; AnItem: TAbstractdxReportCellData;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawIndentCell) then
    FOnCustomDrawIndentCell(Self, ACanvas, ANode, AnIndex, AnItem, ADone);
end;

procedure TcxTreeListCustomReportLink.DoGetCustomPageBreaks;
begin
  if Assigned(FOnGetCustomPageBreaks) then FOnGetCustomPageBreaks(Self);
end;

procedure TcxTreeListCustomReportLink.DoInitializeBandCell(ABand: TcxTreeListBand;
  AnItem: TdxReportCellImage);
begin
  if Assigned(FOnInitializeBandCell) then
    FOnInitializeBandCell(Self, ABand, AnItem);
end;

procedure TcxTreeListCustomReportLink.DoInitializeCell(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeCell) then
    FOnInitializeCell(Self, ANode, AColumn, AnItem);
end;

procedure TcxTreeListCustomReportLink.DoInitializeFooterCell(AColumn: TcxTreeListColumn;
  ASummaryCellIndex: Integer; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeFooterCell) then
    FOnInitializeFooterCell(Self, ASummaryCellIndex, AColumn, AnItem);
end;

procedure TcxTreeListCustomReportLink.DoInitializeHeaderCell(AColumn: TcxTreeListColumn;
  AnItem: TdxReportCellImage);
begin
  if Assigned(FOnInitializeHeaderCell) then
    FOnInitializeHeaderCell(Self, AColumn, AnItem);
end;

procedure TcxTreeListCustomReportLink.DoInitializeIndentCell(ANode: TcxTreeListNode;
  AnIndex: Integer; AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeIndentCell) then
    FOnInitializeIndentCell(Self, ANode, AnIndex, AnItem);
end;

function TcxTreeListCustomReportLink.GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass;
begin
  Result := TcxTreeListReportLinkOptionsExpanding;
end;

function TcxTreeListCustomReportLink.GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass;
begin
  Result := TcxTreeListReportLinkOptionsFormatting;
end;

function TcxTreeListCustomReportLink.GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass;
begin
  Result := TcxTreeListReportLinkOptionsOnEveryPage;
end;

function TcxTreeListCustomReportLink.GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass;
begin
  Result := TcxTreeListReportLinkOptionsPagination;
end;

function TcxTreeListCustomReportLink.GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass;
begin
  Result := TcxTreeListReportLinkOptionsPreview;
end;

function TcxTreeListCustomReportLink.GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass;
begin
  Result := TcxTreeListReportLinkOptionsRefinements;
end;

function TcxTreeListCustomReportLink.GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass;
begin
  Result := TcxTreeListReportLinkOptionsSelection;
end;

function TcxTreeListCustomReportLink.GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass;
begin
  Result := TcxTreeListReportLinkOptionsSize;
end;

function TcxTreeListCustomReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TcxTreeListReportLinkOptionsView;
end;

procedure TcxTreeListCustomReportLink.AddReportRow(AReportRow: TdxReportCell);
begin
  FReportRows.Add(AReportRow);
end;

procedure TcxTreeListCustomReportLink.AddHorizontalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardHorz.Add(Pointer(ADelimiter));
end;

procedure TcxTreeListCustomReportLink.AddVerticalHardDelimiter(ADelimiter: TdxReportCell);
begin
  AddVerticalHardDelimiter(ADelimiter.AbsoluteRect.Top);
end;

procedure TcxTreeListCustomReportLink.AddVerticalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardVert.Add(TObject(ADelimiter));
end;

function TcxTreeListCustomReportLink.CreateBuilder: TcxTreeListReportLinkBuilder;
begin
  Result := GetBuilderClass.Create(Self);
end;

class function TcxTreeListCustomReportLink.GetBuilderClass: TcxTreeListReportLinkBuilderClass;
begin
  Result := TcxTreeListReportLinkBuilder;
end;

function TcxTreeListCustomReportLink.ExtractIndentIndex(AData: Integer): Integer;
begin
  Result := (AData and NodeIndentMask) shr NodeIndentOffset;
end;

function TcxTreeListCustomReportLink.GetItemCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ADrawInfo: TcxTreeListCellCustomDrawInfo): TcxTreeListAttributeID;

  function IsAttributedRow(AData: DWORD; AAttributeClass: TcxTreeListAttributeClass): Boolean;
  begin
    try
      Result := TClass(AData) = AAttributeClass;
    except
      Result := False;
    end;
  end;

  function IsBandRow(AData: DWORD): Boolean;
  begin
    Result := IsAttributedRow(AData, TcxTreeListBandAttribute);
  end;

  function IsHeaderRow(AData: DWORD): Boolean;
  begin
    Result := IsAttributedRow(AData, TcxTreeListHeaderAttribute);
  end;

  function IsFooterRow(AData: DWORD): Boolean;
  begin
    Result := IsAttributedRow(AData, TcxTreeListFooterAttribute);
  end;

  function IsIndentRow(AData: DWORD): Boolean;
  begin
    Result := IsAttributedRow(AData, TcxTreeListIndentsRowAttribute);
  end;

  function CheckBand(AData: DWORD): Boolean;
  begin
    try
      Result := IsDelphiObject(AData) and (TObject(AData) is TcxTreeListBand);
    except
      Result := False;
    end;
    if Result then
      ADrawInfo.Band := TcxTreeListBand(AData);
  end;

  function CheckColumn(AData: DWORD): Boolean;
  begin
    try
      Result := IsDelphiObject(AData) and (TObject(AData) is TcxTreeListColumn);
    except
      Result := False;
    end;
    if Result then
      ADrawInfo.Column := TcxTreeListColumn(AData);
  end;

  function CheckNode(AData: DWORD): Boolean;
  begin
    try
      Result := IsDelphiObject(AData) and (TObject(AData) is TcxTreeListNode);
    except
      Result := False;
    end;
    if Result then
      ADrawInfo.Node := TcxTreeListNode(AData);
  end;

  function IsBandCell(AData: DWORD): Boolean;
  begin
    Result := CheckBand(AData);
    if Result then
      ADrawInfo.AttributeID := cxTreeListBandID;
  end;

  function IsFooterCell(AData: DWORD): Boolean;
  begin
    Result := IsFooterRow(AData);
    if Result then
      ADrawInfo.AttributeID := cxTreeListFooterID;
  end;

  function IsHeaderCell(AData: Integer): Boolean;
  begin
    Result := IsHeaderRow(AData);
    if Result then
      ADrawInfo.AttributeID := cxTreeListHeaderID;
  end;

  function IsNode(AData: DWORD): Boolean;
  begin
    Result := CheckNode(AData);
    if Result then
      ADrawInfo.AttributeID := cxTreeListNodeID;
  end;

  function IsCustomizableCell(AData: DWORD): Boolean;
  begin
    Result := not IsBandRow(AData) and not IsHeaderRow(AData) and
      not IsFooterRow(AData) and not IsIndentRow(AData);
  end;

  function IsIndentCell(AData: DWORD): Boolean;
  begin
    Result := (AnItem.Parent <> nil) and IsIndentRow(AnItem.Parent.Data);
    if Result then
    begin
      ADrawInfo.Index := ExtractIndentIndex(AData);
      ADrawInfo.AttributeID := cxTreeListIndentID;
      if AnItem.Parent.Parent <> nil then
      begin
        AData := AnItem.Parent.Parent.Data;
        if not IsFooterRow(AData) then
          CheckNode(AData);
      end;
    end;
  end;

var
  AData: DWORD;
begin
  FillChar(ADrawInfo, SizeOf(ADrawInfo), 0);
  try
    try
      AData := AnItem.Data;
      if IsCustomizableCell(AData) and not IsIndentCell(AData) then
        if CheckColumn(AData) then
        begin
          if AnItem.Parent <> nil then
          begin
            AData := AnItem.Parent.Data;
            if not IsHeaderCell(AData) then
              if CheckBand(AData) then
              begin
                if AnItem.Parent.Parent <> nil then
                begin
                  AData := AnItem.Parent.Parent.Data;
                  if not IsFooterCell(AData) then
                    IsNode(AData);
                end;
              end
              else
                IsNode(AData);
          end;
        end
        else
          IsBandCell(AData);
    except
      FillChar(ADrawInfo, SizeOf(ADrawInfo), 0);
    end;
  finally
     Result := ADrawInfo.AttributeID;
  end;
end;

function TcxTreeListCustomReportLink.IsCustomDrawn(AnAttributeID: TcxTreeListAttributeID): Boolean;
begin
  Result := False;
  case AnAttributeID of
    cxTreeListBandID:
      Result := Assigned(FOnCustomDrawBandCell);
    cxTreeListFooterID:
      Result := Assigned(FOnCustomDrawFooterCell);
    cxTreeListHeaderID:
      Result := Assigned(FOnCustomDrawHeaderCell);
    cxTreeListIndentID:
      Result := Assigned(FOnCustomDrawIndentCell);
    cxTreeListNodeID:
      Result := Assigned(FOnCustomDrawCell);
  end;
end;

function TcxTreeListCustomReportLink.MakeIndentIndex(AnIndex: Integer): DWORD;
begin
  Result := AnIndex shl NodeIndentOffset;
end;

function TcxTreeListCustomReportLink.GetActiveStyles: TcxTreeListReportLinkStyles;
begin
  Result := inherited ActiveStyles as TcxTreeListReportLinkStyles;
end;

function TcxTreeListCustomReportLink.GetDesignWindow: TdxfmTreeListReportLinkDesignWindow;
begin
  Result := inherited DesignWindow as TdxfmTreeListReportLinkDesignWindow;
end;

function TcxTreeListCustomReportLink.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := FReportRows[Index];
end;

function TcxTreeListCustomReportLink.GetReportRowByNode(Node: TcxTreeListNode): TdxReportCell;
var
  I: Integer;
begin
  if (Node <> nil) and not Node.IsFirstVisible then
    for I := 0 to ReportRowCount - 1 do
    begin
      Result := ReportRows[I];
      if Result.Data = TdxNativeInt(Node) then Exit;
    end;
  Result := nil;
end;

function TcxTreeListCustomReportLink.GetReportCellDataByColumn(
  AColumn: TcxTreeListColumn): TAbstractdxReportCellData;

  function FindColumn(AReportCell: TdxReportCell; var AData: TAbstractdxReportCellData): Boolean;
  var
    I: Integer;
  begin
    Result := AReportCell.FindDataItemByData(TdxNativeInt(AColumn), AData);
    if not Result then
    begin
      for I := 0 to AReportCell.CellCount - 1 do
      begin
        Result := FindColumn(AReportCell.Cells[I], AData);
        if Result then
          Break;
      end;
    end;
  end;

begin
  if (ReportRowCount = 0) or not FindColumn(ReportRows[0], Result) then
    Result := nil;
end;

function TcxTreeListCustomReportLink.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TcxTreeListCustomReportLink.GetOptionsExpanding: TcxTreeListReportLinkOptionsExpanding;
begin
  Result := inherited OptionsExpanding as TcxTreeListReportLinkOptionsExpanding;
end;

function TcxTreeListCustomReportLink.GetOptionsFormatting: TcxTreeListReportLinkOptionsFormatting;
begin
  Result := inherited OptionsFormatting as TcxTreeListReportLinkOptionsFormatting;
end;

function TcxTreeListCustomReportLink.GetOptionsOnEveryPage: TcxTreeListReportLinkOptionsOnEveryPage;
begin
  Result := inherited OptionsOnEveryPage as TcxTreeListReportLinkOptionsOnEveryPage;
end;

function TcxTreeListCustomReportLink.GetOptionsPagination: TcxTreeListReportLinkOptionsPagination;
begin
  Result := inherited OptionsPagination as TcxTreeListReportLinkOptionsPagination;
end;

function TcxTreeListCustomReportLink.GetOptionsPreview: TcxTreeListReportLinkOptionsPreview;
begin
  Result := inherited OptionsPreview as TcxTreeListReportLinkOptionsPreview;
end;

function TcxTreeListCustomReportLink.GetOptionsRefinements: TcxTreeListReportLinkOptionsRefinements;
begin
  Result := inherited OptionsRefinements as TcxTreeListReportLinkOptionsRefinements;
end;

function TcxTreeListCustomReportLink.GetOptionsSelection: TcxTreeListReportLinkOptionsSelection;
begin
  Result := inherited OptionsSelection as TcxTreeListReportLinkOptionsSelection;
end;

function TcxTreeListCustomReportLink.GetOptionsSize: TcxTreeListReportLinkOptionsSize;
begin
  Result := inherited OptionsSize as TcxTreeListReportLinkOptionsSize;
end;

function TcxTreeListCustomReportLink.GetOptionsView: TcxTreeListReportLinkOptionsView;
begin
  Result := inherited OptionsView as TcxTreeListReportLinkOptionsView;
end;

function TcxTreeListCustomReportLink.GetStyles: TcxTreeListReportLinkStyles;
begin
  Result := inherited Styles as TcxTreeListReportLinkStyles;
end;

function TcxTreeListCustomReportLink.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Component);
end;

procedure TcxTreeListCustomReportLink.SetOnCustomDrawBandCell(Value: TcxTreeListReportLinkCustomDrawBandCellEvent);
begin
  if @FOnCustomDrawBandCell <> @Value then
  begin
    FOnCustomDrawBandCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxTreeListCustomReportLink.SetOnCustomDrawCell(Value: TcxTreeListReportLinkCustomDrawCellEvent);
begin
  if @FOnCustomDrawCell <> @Value then
  begin
    FOnCustomDrawCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxTreeListCustomReportLink.SetOnCustomDrawFooterCell(Value: TcxTreeListReportLinkCustomDrawFooterCellEvent);
begin
  if @FOnCustomDrawFooterCell <> @Value then
  begin
    FOnCustomDrawFooterCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxTreeListCustomReportLink.SetOnCustomDrawHeaderCell(Value: TcxTreeListReportLinkCustomDrawHeaderCellEvent);
begin
  if @FOnCustomDrawHeaderCell <> @Value then
  begin
    FOnCustomDrawHeaderCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxTreeListCustomReportLink.SetOnCustomDrawIndentCell(Value: TcxTreeListReportLinkCustomDrawIndentCellEvent);
begin
  if @FOnCustomDrawIndentCell <> @Value then
  begin
    FOnCustomDrawIndentCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxTreeListCustomReportLink.SetOptionsExpanding(Value: TcxTreeListReportLinkOptionsExpanding);
begin
  inherited OptionsExpanding := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsFormatting(Value: TcxTreeListReportLinkOptionsFormatting);
begin
  inherited OptionsFormatting := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsOnEveryPage(Value: TcxTreeListReportLinkOptionsOnEveryPage);
begin
  inherited OptionsOnEveryPage := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsPagination(Value: TcxTreeListReportLinkOptionsPagination);
begin
  inherited OptionsPagination := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsPreview(Value: TcxTreeListReportLinkOptionsPreview);
begin
  inherited OptionsPreview := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsRefinements(Value: TcxTreeListReportLinkOptionsRefinements);
begin
  inherited OptionsRefinements := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsSelection(Value: TcxTreeListReportLinkOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsSize(Value: TcxTreeListReportLinkOptionsSize);
begin
  inherited OptionsSize := Value;
end;

procedure TcxTreeListCustomReportLink.SetOptionsView(Value: TcxTreeListReportLinkOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxTreeListCustomReportLink.SetStyles(Value: TcxTreeListReportLinkStyles);
begin
  inherited Styles := Value;
end;

{ TcxTreeListReportLink }

function TcxTreeListReportLink.GetTreeList: TcxTreeList;
begin
  Result := TcxTreeList(Component);
end;

{ TcxDBTreeListReportLink }

function TcxDBTreeListReportLink.GetDBTreeList: TcxDBTreeList;
begin
  Result := TcxDBTreeList(Component);
end;

{ TcxVirtualTreeListReportLink }

function TcxVirtualTreeListReportLink.GetVirtualTreeList: TcxVirtualTreeList;
begin
  Result := TcxVirtualTreeList(Component);
end;

{ TdxfmTreeListReportLinkDesignWindow }

constructor TdxfmTreeListReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxPSGlbl .dxhccxTreeListReportLinkDesigner;
  inherited;
  CreateControls;
  SetActivePage;
end;

destructor TdxfmTreeListReportLinkDesignWindow.Destroy;
begin
  dxPSPopupMan.dxPSPopupMenuController.UnregisterControl(lbxStyles);
  inherited;
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoInitialize;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited DoInitialize;
  RefreshStylesList;

  InitializePreviewTreeList;
  InitializePreviewTreeListStyles;
  LoadDataIntoPreviewTreeList;

  with ReportLink.OptionsView do
  begin
    chbxShowBands.Checked := BandHeaders;
    chbxShowBorders.Checked := Borders;
    chbxShowExpandButtons.Checked := ExpandButtons;
    chbxShowFooters.Checked := Footers;
    chbxShowHeaders.Checked := Headers;
    chbxShowTreeLines.Checked := TreeLines;
  end;

  with ReportLink.OptionsOnEveryPage do
  begin
    chbxBandsOnEveryPage.Checked := BandHeaders;
    chbxFootersOnEveryPage.Checked := Footers;
    chbxHeadersOnEveryPage.Checked := Headers;
  end;

  with cbxLookAndFeel.Properties do
  begin
    Items.Clear;
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelFlat), TObject(lfFlat));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelStandard), TObject(lfStandard));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelUltraFlat), TObject(lfUltraFlat));

    cbxLookAndFeel.ItemIndex := Items.IndexOfObject(TObject(ReportLink.OptionsFormatting.LookAndFeelKind));
  end;

  with ReportLink.OptionsFormatting do
  begin
    ccbxSeparatorColor.ColorValue := ActualNodeSeparatorColor;
    seSeparatorThickness.Value := NodeSeparatorThickness;
    chbxConsumeSelectionStyle.Checked := ConsumeSelectionStyle;
    chbxSuppressBackgroundBitmaps.Checked := SuppressBackgroundBitmaps;
    chbxUseNativeStyles.Checked := UseNativeStyles;
  end;

  with ReportLink.OptionsSelection do
  begin
    chbxProcessSelection.Checked := ProcessSelection;
    chbxProcessExactSelection.Checked := ProcessExactSelection;
  end;

  with ReportLink.OptionsExpanding do
  begin
    chbxExpandNodes.Checked := AutoExpandNodes;
    chbxExplicitlyExpandNodes.Checked := ExplicitlyExpandNodes;
  end;

  with ReportLink.OptionsSize do
  begin
    chbxAutoWidth.Checked := AutoWidth;
  end;

  with ReportLink.OptionsRefinements do
  begin
    chbxTransparentGraphics.Checked := TransparentGraphics;
    chbxDisplayGraphicsAsText.Checked := DisplayGraphicsAsText;
    chbxFlatCheckMarks.Checked := FlatCheckMarks;
    chbxDisplayTrackBarsAsText.Checked := DisplayTrackBarsAsText;
   (*
    chbxTransparentRichEdits.Checked := TransparentRichEdits;
   *)
  end;

  with ReportLink.OptionsPreview do
  begin
    chbxPreviewVisible.Checked := Visible;
    chbxPreviewAutoHeight.Checked := AutoHeight;
    sePreviewMaxLineCount.Value := MaxLineCount;
  end;
end;

function TdxfmTreeListReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.LoadStrings;
begin
  inherited LoadStrings;

  lblPreviewWindow.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));
  tshView.Caption := cxGetResourceString(@sdxViewTab);

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBands.Caption := cxGetResourceString(@sdxBands);
  chbxShowHeaders.Caption := cxGetResourceString(@sdxHeaders);
  chbxShowFooters.Caption := cxGetResourceString(@sdxFooters);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorders);
  chbxShowExpandButtons.Caption := cxGetResourceString(@sdxExpandButtons);
  chbxShowTreeLines.Caption := cxGetResourceString(@sdxTreeLines);

  lblOnEveryPage.Caption := cxGetResourceString(@sdxOnEveryPage);
  chbxBandsOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxBands));
  chbxHeadersOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxHeaders));
  chbxFootersOnEveryPage.Caption := DropAmpersand(cxGetResourceString(@sdxFooters));

  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviorsTab);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxProcessSelection.Caption := cxGetResourceString(@sdxProcessSelection);
  chbxProcessExactSelection.Caption := cxGetResourceString(@sdxProcessExactSelection);

  lblExpanding.Caption := cxGetResourceString(@sdxExpanding);
  chbxExpandNodes.Caption := cxGetResourceString(@sdxNodes);
  chbxExplicitlyExpandNodes.Caption := cxGetResourceString(@sdxExplicitlyExpandNodes);

  lblSize.Caption := cxGetResourceString(@sdxSize);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);

  tshFormatting.Caption := cxGetResourceString(@sdxFormatting);
  lblLookAndFeel.Caption := cxGetResourceString(@sdxLookAndFeel);

  lblRefinements.Caption := cxGetResourceString(@sdxRefinements);
  chbxTransparentGraphics.Caption := cxGetResourceString(@sdxTransparentGraphics);
  chbxDisplayGraphicsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayGraphicsAsText));
  chbxDisplayTrackBarsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayTrackBarsAsText));
  chbxFlatCheckMarks.Caption := cxGetResourceString(@sdxFlatCheckMarks);
  chbxSuppressBackgroundBitmaps.Caption := cxGetResourceString(@sdxSuppressBackgroundBitmaps);
  chbxConsumeSelectionStyle.Caption := cxGetResourceString(@sdxConsumeSelectionStyle);

  lblSeparators.Caption := cxGetResourceString(@sdxSeparators);
  lblSeparatorsColor.Caption := cxGetResourceString(@sdxColor);
  lblSeparatorsThickness.Caption := cxGetResourceString(@sdxThickness);

  tshStyles.Caption := cxGetResourceString(@sdxStyles);
  lblUseNativeStyles.Caption := cxGetResourceString(@sdxUseNativeStyles);
  btnStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  btnStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  btnStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  btnStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  btnStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);

  btnStyleSheetNew.Caption := cxGetResourceString(@sdxBtnNew);
  btnStyleSheetCopy.Caption := cxGetResourceString(@sdxBtnCopy);
  btnStyleSheetDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  btnStyleSheetRename.Caption := cxGetResourceString(@sdxBtnRename);

  miStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  miStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  miStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  miStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  miStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  miStylesSelectAll.Caption := cxGetResourceString(@sdxSelectAll);
  miStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);

  tshPreview.Caption := cxGetResourceString(@sdxPreviewTab);
  lblPreviewOptions.Caption := cxGetResourceString(@sdxOptions);
  chbxPreviewVisible.Caption := cxGetResourceString(@sdxVisible);
  chbxPreviewAutoHeight.Caption := cxGetResourceString(@sdxPreviewAutoHeight);
  lblPreviewMaxLineCount.Caption := cxGetResourceString(@sdxPreviewMaxLineCount);
  lblStyleSheets.Caption := cxGetResourceString(@sdxStyleSheets);
end;

procedure TdxfmTreeListReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  chbxProcessExactSelection.Enabled := chbxProcessSelection.Checked;
  chbxExplicitlyExpandNodes.Enabled := chbxExpandNodes.Checked;

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

  WarningPaneUpdate;
end;

procedure TdxfmTreeListReportLinkDesignWindow.UpdatePreview;
const
  TransparentsMap: array[Boolean] of TcxImageTransparency = (gtOpaque, gtTransparent);
  TreeLineStylesMap: array[Boolean] of TcxTreeListTreeLineStyle = (tllsNone, tllsDot);
begin
  PreviewTreeList.Enabled := False;
  dxSetupPreviewControlLookAndFeel(PreviewTreeList.LookAndFeel, ReportLink.OptionsFormatting.LookAndFeelKind, ReportLink.TreeList);
  dxAssignFont(PreviewTreeList.Font, ReportLink.Font, ScaleFactor, ReportLink.ScaleFactor);

  chbxBandsOnEveryPage.Enabled := not ReportLink.IsAggregated;
  chbxHeadersOnEveryPage.Enabled := not ReportLink.IsAggregated;
  chbxFootersOnEveryPage.Enabled := not ReportLink.IsAggregated;

  TcxImageProperties(colManufacturerLogo.Properties).GraphicTransparency :=
    TransparentsMap[ReportLink.OptionsRefinements.TransparentGraphics];

  PreviewTreeList.OptionsView.Bands := ReportLink.OptionsView.BandHeaders;
  PreviewTreeList.OptionsView.Footer := ReportLink.OptionsView.Footers;
  PreviewTreeList.OptionsView.Headers := ReportLink.OptionsView.Headers;
  PreviewTreeList.OptionsView.Buttons := ReportLink.OptionsView.ExpandButtons;
  PreviewTreeList.OptionsView.TreeLineStyle := TreeLineStylesMap[ReportLink.OptionsView.TreeLines];
  PreviewTreeList.OptionsView.ShowRoot := (ReportLink.TreeList <> nil) and
    CustomTreeList_GetOptionsView(ReportLink.TreeList).ShowRoot;
  PreviewTreeList.Preview.Visible := ReportLink.OptionsPreview.Visible;

  if ReportLink.TreeList <> nil then
    PreviewTreeList.OptionsView.UseNodeColorForIndent :=
      CustomTreeList_GetOptionsView(ReportLink.TreeList).UseNodeColorForIndent;

  PreviewTreeList.Invalidate;
end;

function TdxfmTreeListReportLinkDesignWindow.GetDesignerTabIndex: Integer;
begin
  Result := pcMain.Index;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetDesignerTabIndex(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > pcMain.Count - 1 then
    Value := pcMain.Count - 1;
  pcMain.ItemIndex := Value;
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoActiveStyleSheetChanged;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited DoActiveStyleSheetChanged;
  cbxStyleSheets.ItemIndex := cbxStyleSheets.Properties.Items.IndexOfObject(ActiveStyleSheet);

  if not LockControlsUpdate then
  begin
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoFormActivated(AnActive: Boolean);
begin
  inherited;
  if not AnActive then lbxStyles.HideToolTips;
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoRefreshStylesList;
var
  Styles: TcxTreeListReportLinkStyles;
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
          AddObject(cxGetResourceString(@sdxBandBackgroundStyle), Styles.BandBackground);
          AddObject(cxGetResourceString(@sdxBandHeaderStyle), Styles.BandHeader);
          AddObject(cxGetResourceString(@sdxContentStyle), Styles.Content);
          AddObject(cxGetResourceString(@sdxContentEvenStyle), Styles.ContentEven);
          AddObject(cxGetResourceString(@sdxContentOddStyle), Styles.ContentOdd);
          AddObject(cxGetResourceString(@sdxFooterStyle), Styles.Footer);
          AddObject(cxGetResourceString(@sdxFooterRowStyle), Styles.FooterRow);
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
  InitializePreviewTreeListStyles;
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoStyleChanged(const ACaption: string;
  AStyle: TcxStyle);
begin
  inherited;
  UpdatePreviewTreeListStyles(ACaption, AStyle);
end;

procedure TdxfmTreeListReportLinkDesignWindow.DoStylesChanged(AStrings: TStrings;
  ARecreate: Boolean);
begin
  if ARecreate then
    RecreateStylesListBox
  else
    lbxStyles.Invalidate;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.GetSelectedStyleNames(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.Clear;
  with lbxStyles do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
        AStrings.AddObject(Items[I], Items.Objects[I]);
end;

procedure TdxfmTreeListReportLinkDesignWindow.GetStyleNames(out AStrings: TStrings);
begin
  AStrings := lbxStyles.Items;
end;

procedure TdxfmTreeListReportLinkDesignWindow.GetStyleSheetNames(out AStrings: TStrings);
begin
  AStrings := cbxStyleSheets.Properties.Items;
end;

function TdxfmTreeListReportLinkDesignWindow.GetActiveStyle: TcxStyle;
begin
  with lbxStyles do
    if ItemIndex <> -1 then
      Result := TcxStyle(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TdxfmTreeListReportLinkDesignWindow.GetHasSelectedStyles: Boolean;
begin
  Result := lbxStyles.SelCount <> 0;
end;

function TdxfmTreeListReportLinkDesignWindow.GetHasSelectedStylesWithAssignedBitmap: Boolean;
var
  Styles: TStrings;
  I: Integer;
  Style: TcxStyle;
begin
  Result := True;
  Styles := TStringList.Create;
  try
    GetSelectedStyleNames(Styles);
    for I := 0 to Styles.Count - 1 do
    begin
      Style := TcxStyle(Styles.Objects[I]);
      if (Style <> nil) and (Style.Bitmap <> nil) and not Style.Bitmap.Empty then
        Exit;
    end;
  finally
    Styles.Free;
  end;
  Result := False;
end;

function TdxfmTreeListReportLinkDesignWindow.GetReportLink: TcxTreeListCustomReportLink;
begin
  Result := inherited ReportLink as TcxTreeListCustomReportLink;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewMaxLineCountChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.MaxLineCount := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SeparatorColorChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsFormatting.NodeSeparatorColor := TcxColorComboBox(Sender).ColorValue;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SeparatorThicknessChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsFormatting.NodeSeparatorThickness := TcxSpinEdit(Sender).Value;
  Modified := True;
end;

function TdxfmTreeListReportLinkDesignWindow.CanSelectAllStyles: Boolean;
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

procedure TdxfmTreeListReportLinkDesignWindow.CreateControls;

  procedure CreateWarningPane;
  begin
    wpIncorrectOnEveryPageState := TdxPSWarningPane.Create(Self);
    bvlWarningHost.Control := wpIncorrectOnEveryPageState;
  end;

  procedure CreateStylesListBox;
  begin
    lbxStyles := TdxStylesListBox.Create(Self);
    lbxStyles.PopupMenu := pmStyles;
    lbxStyles.TabOrder := chbxUseNativeStyles.TabOrder + 1;
    lbxStyles.OnClick := lbxStylesClick;
    bvlStylesHost.Control := lbxStyles;
    dxPSPopupMan.dxPSPopupMenuController.RegisterControl(lbxStyles);
  end;

begin
  CreateWarningPane;
  CreateStylesListBox;
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawBorders(ACanvas: TcxCanvas;
  const R: TRect);
var
  GridLinesColor: TColor;
begin
  with ReportLink do
  begin
    GridLinesColor := PreviewTreeList.OptionsView.GridLineColor;
    {if TreeList <> nil then
      GridLinesColor := CustomTreeList_GetOptionsView(TreeList).GridLineColor
    else
      GridLinesColor := PreviewTreeList.OptionsView.GridLineColor;//clWindowText;}
    if GridLinesColor = clDefault then
      GridLinesColor := CustomTreeList_GetLookAndFeelPainter(PreviewTreeList).DefaultGridLineColor;
  end;
  ACanvas.DrawComplexFrame(R, GridLinesColor, GridLinesColor, [bLeft, bBottom, bRight]);
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawCheckMark(
  ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo);
const
  StylesMap: array[Boolean] of TcxLookAndFeelStyle = (lfsStandard, lfsUltraFlat);
var
  CheckValue: Variant;
  Params: TcxViewParams;
  PrevColor: TColor;
  R: TRect;
  X, Y: Integer;
begin
  PrevColor := ACanvas.Brush.Color;
  R := AViewInfo.BoundsRect;
  Params := PreviewTreeList.Styles.GetContentParams(AViewInfo.Node, AViewInfo.Column);

  if Params.Bitmap = nil then
  begin
    ACanvas.Brush.Color := Params.Color;
    ACanvas.FillRect(R);
  end
  else
    cxBkgndDrawPicture(Params.Bitmap, ACanvas.Canvas, R, ppmTile, 1, 1, -R.Left, -R.Top);

  X := R.Left + (R.Right - R.Left - CheckWidth) div 2;
  Y := R.Top + (R.Bottom - R.Top - CheckWidth) div 2;
  R := Bounds(X, Y, CheckWidth, CheckWidth);
  CheckValue := cbsChecked;
  if AViewInfo.Node.Index = 0 then
    CheckValue := cbsUnchecked;
  with cxLookAndFeelPaintersManager.GetPainter(StylesMap[ReportLink.OptionsRefinements.FlatCheckMarks]) do
    DrawScaledCheckButton(ACanvas, R, cxbsDefault, CheckValue = cbsChecked, ScaleFactor);
  ACanvas.Brush.Color := PrevColor;
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawColCarSpeedCount(ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo);

  procedure DrawTrackBar(ACanvas: TcxCanvas);
  var
    ATrackBarViewInfo: TcxCustomTrackBarViewInfo;
    AViewData: TcxCustomTrackBarViewData;
    AProperties: TcxCustomTrackBarProperties;
  begin
    AProperties := TcxCustomTrackBarProperties(AViewInfo.Column.Properties);
    ATrackBarViewInfo := TcxCustomTrackBarViewInfo(AProperties.GetViewInfoClass.Create);
    try
      AViewData := TcxCustomTrackBarViewData(AProperties.CreateViewData(DefaultEditStyleController.Style, True));
      try
        ATrackBarViewInfo.Position := StrToInt(AViewInfo.DisplayValue);
        AViewData.Style.Color := AViewInfo.TreeList.Color;
        AViewData.Calculate(ACanvas, cxRectInflate(AViewInfo.BoundsRect, -2, -2), Point(0, 0), cxmbNone, [], ATrackBarViewInfo, False);
        ATrackBarViewInfo.Paint(ACanvas);
      finally
        AViewData.Free;
      end;
    finally
      ATrackBarViewInfo.Free;
    end;
  end;

begin
  if ReportLink.OptionsRefinements.DisplayTrackBarsAsText then
    CustomDrawTextRect(ACanvas, cxRectInflate(AViewInfo.BoundsRect, -2, -2), AViewInfo.DisplayValue, taLeftJustify, vaCenter, AViewInfo.ViewParams)
  else
    DrawTrackBar(ACanvas);
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawFooter(ACanvas: TcxCanvas;
  R: TRect; AViewInfo: TcxTreeListFooterCellViewInfo);
begin
  if AViewInfo.Hidden then Exit;
  case ReportLink.OptionsFormatting.LookAndFeelKind of
    lfStandard,
    lfFlat:
      ACanvas.DrawComplexFrame(R, AViewInfo.ViewParams.TextColor, clBtnHighlight);
    lfUltraFlat:
      ACanvas.FrameRect(R, AViewInfo.ViewParams.TextColor);
  end;

  InflateRect(R, -1, -1);
  with AViewInfo do
    CustomDrawTextRect(ACanvas, R, Text, AlignHorz, AlignVert, ViewParams);
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawHeader(ACanvas: TcxCanvas;
  R: TRect; AViewInfo: TcxTreeListHeaderCellViewInfo);
const
  BottomRightColors: array[Boolean] of TColor = (clBtnFace, clBtnShadow);
begin
  ACanvas.FrameRect(R, clWindowText);//AViewInfo.ViewParams.TextColor);
  if ReportLink.OptionsFormatting.LookAndFeelKind <> lfUltraFlat then
  begin
    InflateRect(R, -1, -1);
    ACanvas.DrawComplexFrame(R, clBtnHighlight,
      BottomRightColors[ReportLink.OptionsFormatting.LookAndFeelKind = lfStandard]);
  end;

  InflateRect(R, -1, -1);
  with AViewInfo do
    CustomDrawTextRect(ACanvas, R, Text, AlignHorz, AlignVert, ViewParams);
end;

procedure TdxfmTreeListReportLinkDesignWindow.CustomDrawTextRect(ACanvas: TcxCanvas;
  R: TRect; const AText: string; AnAlignmentHorz: TAlignment; AnAlignmentVert: TcxAlignmentVert;
  AParams: TcxViewParams);
const
  AlignmentHorzMap: array[TAlignment] of Integer = (cxAlignLeft, cxAlignRight, cxAlignCenter);
  AlignmentVertMap: array[TcxAlignmentVert] of Integer = (cxAlignTop, cxAlignBottom, cxAlignVCenter);
begin
  if AParams.Bitmap = nil then
    ACanvas.FillRect(R, AParams.Color)
  else
    cxBkgndDrawPicture(AParams.Bitmap, ACanvas.Canvas, R, ppmTile, 1, 1, - R.Left - 0, - R.Top - 0);

  InflateRect(R, -2, -1);
  if AParams.Font <> nil then
    ACanvas.Font := AParams.Font;
  ACanvas.Font.Color := AParams.TextColor;
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawText(AText, R,
    AlignmentHorzMap[AnAlignmentHorz] or AlignmentVertMap[AnAlignmentVert] or cxSingleLine);
  ACanvas.Brush.Style := bsSolid;
end;

function TdxfmTreeListReportLinkDesignWindow.ExtractAlignmentHorz(
  AViewInfo: TcxTreeListEditCellViewInfo): TAlignment;
begin
  with AViewInfo.Column do
    if Properties <> nil then
      Result := cxEditProperties_GetAlignment(Properties).Horz
    else
      Result := taLeftJustify;
end;

function TdxfmTreeListReportLinkDesignWindow.ExtractAlignmentVert(
  AViewInfo: TcxTreeListEditCellViewInfo): TcxAlignmentVert;
const
  EditAlignmentVertMap: array[TcxEditVertAlignment] of TcxAlignmentVert =
    (vaTop, vaBottom, vaCenter);
begin
  with AViewInfo.Column do
    if Properties <> nil then
      Result := EditAlignmentVertMap[cxEditProperties_GetAlignment(Properties).Vert]
    else
      Result := vaCenter;
end;

procedure TdxfmTreeListReportLinkDesignWindow.InitializePreviewTreeList;

  procedure SetupDataBindings;

    function GetColumnCaption(AnIndex: Integer): string;
    begin
      case AnIndex of
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
    ColumnProperties: array[0..5] of string =
      ('TcxTextEditProperties', 'TcxImageProperties', 'TcxTextEditProperties',
       'TcxTextEditProperties', 'TcxCheckBoxProperties', 'TcxTrackBarProperties');
  var
    I: Integer;
  begin
    for I := 0 to PreviewTreeList.ColumnCount - 1 do
      with PreviewTreeList.Columns[I] do
      begin
        PropertiesClassName := ColumnProperties[I];
        Caption.Text := GetColumnCaption(I);
      end;
  end;

var
  ASummaryItems: TcxTreeListSummaryItems;
begin
  SetupDataBindings;
  //TcxImageProperties(colManufacturerLogo.Properties).Stretch := True;
  PreviewTreeList.Bands[0].Caption.Text := cxGetResourceString(@sdxManufacturerBandCaption);
  PreviewTreeList.Bands[1].Caption.Text := cxGetResourceString(@sdxModelBandCaption);
  PreviewTreeList.OptionsView.Footer := True;
  ASummaryItems :=PreviewTreeList.Columns[0].Summary.FooterSummaryItems;
  if ASummaryItems.Count = 0 then
    ASummaryItems.Add;
  ASummaryItems[0].Kind := skCount;
  ASummaryItems[0].Format := cxGetResourceString(@sdxSummaryFormat);
end;

procedure TdxfmTreeListReportLinkDesignWindow.InitializePreviewTreeListStyles;

  procedure ResetEvents(AStyles: TcxTreeListStyles);
  begin
    with AStyles do
    begin
      OnGetBandBackgroundStyle := nil;
      OnGetBandContentStyle := nil;
      OnGetBandHeaderStyle := nil;
      OnGetBandFooterStyle := nil;
      OnGetColumnFooterStyle := nil;
      OnGetColumnHeaderStyle := nil;
      OnGetContentStyle := nil;
      OnGetNodeIndentStyle := nil;
      OnGetPreviewStyle := nil;
    end;
  end;

var
  Styles: TcxTreeListReportLinkStyles;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    Styles := ReportLink.ActiveStyles;
    dxPSResetStyles(PreviewTreeList.Styles);
    with PreviewTreeList.Styles do
    begin
      BandBackground := Styles.BandBackground;
      BandHeader := Styles.BandHeader;
      Content := Styles.Content;
      ContentEven := Styles.ContentEven;
      ContentOdd := Styles.ContentOdd;
      ColumnFooter := Styles.Footer;
      ColumnHeader := Styles.Header;
      Footer := Styles.FooterRow;
      Preview := Styles.Preview;
    end;
  end
  else
    if ReportLink.TreeList <> nil then
      PreviewTreeList.Styles := CustomTreeList_GetStyles(ReportLink.TreeList)
    else
      dxPScxCommon.dxPSResetStyles(PreviewTreeList.Styles);

  ResetEvents(PreviewTreeList.Styles);
end;

procedure TdxfmTreeListReportLinkDesignWindow.LoadDataIntoPreviewTreeList;

  function GetManufacturerName(AnIndex: Integer): string;
  begin
    case AnIndex of
      0: Result := cxGetResourceString(@sdxCarManufacturerName5);
      1: Result := cxGetResourceString(@sdxCarManufacturerName1);
      2: Result := cxGetResourceString(@sdxCarManufacturerName2);
    else
      Result := cxGetResourceString(@sdxCarManufacturerName4);
    end;
  end;

  function GetManufacturerCountry(AnIndex: Integer): string;
  begin
    case AnIndex of
      0: Result := cxGetResourceString(@sdxCarManufacturerCountry5);
      1: Result := cxGetResourceString(@sdxCarManufacturerCountry1);
      2: Result := cxGetResourceString(@sdxCarManufacturerCountry2);
    else
      Result := cxGetResourceString(@sdxCarManufacturerCountry4);
    end;
  end;

  function GetCarModel(AnIndex: Integer): string;
  begin
    case AnIndex of
      0: Result := cxGetResourceString(@sdxCarModel5);
      1: Result := cxGetResourceString(@sdxCarModel1);
      2: Result := cxGetResourceString(@sdxCarModel2);
    else
      Result := cxGetResourceString(@sdxCarModel4);
    end;
  end;

const
  RecordCount = 4;
  CarLogosIndexes: array[0..RecordCount - 1] of Integer = (4, 0, 1, 3);
  AreCarsSUV: array[0..RecordCount - 1] of string = ('False', 'True', 'True', 'True');
  AreCarSpeedCount: array[0..RecordCount - 1] of string = ('6', '5', '5', '6');
var
  I: Integer;
begin
  PreviewTreeList.Clear;
  for I := 0 to RecordCount - 1 do
    with PreviewTreeList.Add do
    begin
      Values[0] := GetManufacturerName(I);
      Values[1] := dxPScxCommon.dxPSPreviewCarLogosAsString(CarLogosIndexes[I]);
      Values[2] := GetManufacturerCountry(I);
      Values[3] := GetCarModel(I);
      Values[4] := AreCarsSUV[I];
      Values[5] := AreCarSpeedCount[I];
    end;
end;

procedure TdxfmTreeListReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgShow, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(imgOnEveryPage, IDB_DXPSGROUPICON_ONEVERYPAGE);
  dxLoadIconFromResourceEx(imgExpanding, IDB_DXPSGROUPICON_EXPANDING);
  dxLoadIconFromResourceEx(imgGridSize, IDB_DXPSGROUPICON_SIZE);
  dxLoadIconFromResourceEx(imgSeparators, IDB_DXPSGROUPICON_PAGINATION);
  dxLoadIconFromResourceEx(imgSelection, IDB_DXPSGROUPICON_SELECTION);
  dxLoadIconFromResourceEx(imgLookAndFeel, IDB_DXPSGROUPICON_LOOKANDFEEL);
  dxLoadIconFromResourceEx(imgRefinements, IDB_DXPSGROUPICON_REFINEMENTS);
  dxLoadIconFromResourceEx(imgPreview, IDB_DXPSGROUPICON_PREVIEW);
  dxLoadImageListFromResources(ilStylesPopup, IDIL_DXPSSTYLESMENU);
end;

procedure TdxfmTreeListReportLinkDesignWindow.RecreateStylesListBox;
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

procedure TdxfmTreeListReportLinkDesignWindow.RestoreSelectedStyles(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    lbxStyles.Selected[Integer(AList[I])] := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SaveSelectedStyles(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to lbxStyles.Items.Count - 1 do
    if lbxStyles.Selected[I] then AList.Add(TObject(I));
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetActivePage;
begin
  pcMain.ItemIndex := DesignerTabIndex;
end;

procedure TdxfmTreeListReportLinkDesignWindow.UpdatePreviewTreeListStyles(const ACaption: string;
  AStyle: TcxStyle);
begin
  with PreviewTreeList.Styles do
  begin
    if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxBandBackgroundStyle)) then
      BandBackground := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxBandHeaderStyle)) then
      BandHeader := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentStyle)) then
      Content := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentEvenStyle)) then
      ContentEven := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentOddStyle)) then
      ContentOdd := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxFooterStyle)) then
      ColumnFooter := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxHeaderStyle)) then
      ColumnHeader := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxFooterRowStyle)) then
      Footer := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxPreviewStyle)) then
      Preview := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxSelectionStyle)) then
      Selection := AStyle
  end;
end;

procedure TdxfmTreeListReportLinkDesignWindow.WarningPaneUpdate;
begin
  with ReportLink.HostInfoServices do
    wpIncorrectOnEveryPageState.SetStateAndHint(IsInconsistentState, InconsistentStateText);
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsExpandingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsExpanding do
    case Index of
      0: AutoExpandNodes := Value;
      1: ExplicitlyExpandNodes := Value;
    end;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsFormattingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsFormatting do
    case Index of
      0:
         begin
           UseNativeStyles := Value;
           InitializePreviewTreeListStyles;
         end;
      1: SuppressBackgroundBitmaps := Value;
      2: ConsumeSelectionStyle := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsOnEveryPageByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsOnEveryPage do
    case Index of
      0: BandHeaders := Value;
      1: Headers := Value;
      2: Footers := Value;
    end;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsRefinementsByIndex(Index: Integer;
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

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsSelectionByIndex(Index: Integer;
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

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsSizeByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsSize do
    case Index of
      0: AutoWidth := Value;
    end;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.SetOptionsViewByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsView do
    case Index of
      0: BandHeaders := Value;
      1: Headers := Value;
      2: Footers := Value;
      3: ExpandButtons := Value;
      4: TreeLines := Value;
      5: Borders := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.LookAndFeelChange(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  with TcxComboBox(Sender) do
    ReportLink.OptionsFormatting.LookAndFeelKind :=
      TcxLookAndFeelKind(Properties.Items.Objects[ItemIndex]);
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsRefinementsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsRefinementsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewVisibleClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.Visible := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OptionsPreview.AutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsSelectionClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSelectionByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsExpandingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsExpandingByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsSizeClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSizeByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsViewClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsViewByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsOnEveryPageClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsOnEveryPageByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.OptionsFormattingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsFormattingByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmTreeListReportLinkDesignWindow.lblUseNativeStylesClick(
  Sender: TObject);
begin
  if chbxUseNativeStyles.CanFocus then ActiveControl := chbxUseNativeStyles;
  chbxUseNativeStyles.Checked := not chbxUseNativeStyles.Checked;
end;

procedure TdxfmTreeListReportLinkDesignWindow.pmStylesPopup(Sender: TObject);
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

procedure TdxfmTreeListReportLinkDesignWindow.StyleColorClick(Sender: TObject);
begin
  PerformStylesChangeColor;
end;

procedure TdxfmTreeListReportLinkDesignWindow.StyleFontClick(Sender: TObject);
begin
  PerformStylesChangeFont;
end;

procedure TdxfmTreeListReportLinkDesignWindow.StyleBackgroundBitmapClick(
  Sender: TObject);
begin
  PerformStylesChangeBitmap;
end;

procedure TdxfmTreeListReportLinkDesignWindow.StyleBackgroundBitmapClearClick(
  Sender: TObject);
begin
  PerformStylesClearBitmap;
end;

procedure TdxfmTreeListReportLinkDesignWindow.StyleRestoreDefaultsClick(
  Sender: TObject);
begin
  PerformStylesRestoreDefaults;
end;

procedure TdxfmTreeListReportLinkDesignWindow.cbxStyleSheetsClick(Sender: TObject);
begin
  ActiveStyleSheet := TcxCustomStyleSheet(TcxComboBox(Sender).ItemObject);
end;

procedure TdxfmTreeListReportLinkDesignWindow.btnStyleSheetNewClick(Sender: TObject);
begin
  PerformStyleSheetNew;
end;

procedure TdxfmTreeListReportLinkDesignWindow.btnStyleSheetCopyClick(Sender: TObject);
begin
  PerformStyleSheetCopy;
end;

procedure TdxfmTreeListReportLinkDesignWindow.btnStyleSheetDeleteClick(Sender: TObject);
begin
  PerformStyleSheetDelete;
end;

procedure TdxfmTreeListReportLinkDesignWindow.btnStyleSheetRenameClick(Sender: TObject);
begin
  PerformStyleSheetRename;
end;

procedure TdxfmTreeListReportLinkDesignWindow.miStylesSelectAllClick(Sender: TObject);
begin
  lbxStyles.SelectAll;
  UpdateControlsState;
end;

procedure TdxfmTreeListReportLinkDesignWindow.StylesSaveAsClick(Sender: TObject);
begin
  PerformStylesSaveAsStyleSheet;
end;

procedure TdxfmTreeListReportLinkDesignWindow.cbxStyleSheetsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  PerformStyleSheetKeyDown(Sender, Key, Shift);
end;

procedure TdxfmTreeListReportLinkDesignWindow.lbxStylesClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  UpdateControlsState;
end;

procedure RegisterAssistants;
begin
  TcxTreeListNodeHelper.Register;
  TcxTreeListReportLinkStyleSheet.Register;
end;

procedure UnregisterAssistants;
begin
  TcxTreeListReportLinkStyleSheet.Unregister;
  TcxTreeListNodeHelperFactory.ReleaseInstance;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewTreeListCustomDrawBandHeaderCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListHeaderCellViewInfo; var ADone: Boolean);
var
  R: TRect;
begin
  R := AViewInfo.BoundsRect;
  Dec(R.Top);
  Dec(R.Left);
  CustomDrawHeader(ACanvas, R, AViewInfo);

  ADone := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewTreeListCustomDrawDataCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if AViewInfo.Column = colManufacturerLogo then
    if ReportLink.OptionsRefinements.DisplayGraphicsAsText then
    begin
      CustomDrawTextRect(ACanvas, AViewInfo.BoundsRect, ReportLink.OptionsRefinements.GraphicsText,
        ExtractAlignmentHorz(AViewInfo), ExtractAlignmentVert(AViewInfo), AViewInfo.ViewParams);
      CustomDrawBorders(ACanvas, AViewInfo.BoundsRect);
      ADone := True;
    end;

  if AViewInfo.Column = colCarIsSUV then
  begin
    CustomDrawCheckMark(ACanvas, AViewInfo);
    CustomDrawBorders(ACanvas, AViewInfo.BoundsRect);
    ADone := True;
  end;

  if AViewInfo.Column = colSpeedCount then
  begin
    CustomDrawColCarSpeedCount(ACanvas, AViewInfo);
    CustomDrawBorders(ACanvas, AViewInfo.BoundsRect);
    ADone := True;
  end;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewTreeListCustomDrawFooterCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListFooterCellViewInfo; var ADone: Boolean);
begin
  CustomDrawFooter(ACanvas, AViewInfo.BoundsRect, TcxTreeListFooterCellViewInfo(AViewInfo));
  ADone := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.PreviewTreeListCustomDrawHeaderCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListHeaderCellViewInfo; var ADone: Boolean);
var
  R: TRect;
begin
  R := AViewInfo.BoundsRect;
  Dec(R.Top);
  Dec(R.Left);
  CustomDrawHeader(ACanvas, R, AViewInfo);

  ADone := True;
end;

procedure TdxfmTreeListReportLinkDesignWindow.cbxStyleSheetsPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
begin
  PerformStyleSheetDrawItem(ACanvas.Canvas, AIndex, ARect, AState, AControl.Enabled);
end;

initialization
  RegisterAssistants;

  dxPSRegisterReportLink(TcxTreeListReportLink, TcxTreeList, TdxfmTreeListReportLinkDesignWindow);
  dxPSRegisterReportLink(TcxDBTreeListReportLink, TcxDBTreeList, TdxfmTreeListReportLinkDesignWindow);
  dxPSRegisterReportLink(TcxVirtualTreeListReportLink, TcxVirtualTreeList, TdxfmTreeListReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TcxVirtualTreeListReportLink, TcxVirtualTreeList, TdxfmTreeListReportLinkDesignWindow);
  dxPSUnregisterReportLink(TcxDBTreeListReportLink, TcxDBTreeList, TdxfmTreeListReportLinkDesignWindow);
  dxPSUnregisterReportLink(TcxTreeListReportLink, TcxTreeList, TdxfmTreeListReportLinkDesignWindow);

  UnregisterAssistants;

  FreeAndNil(FDefaultdxPScxTreeListLinkStyleSheet);

end.

