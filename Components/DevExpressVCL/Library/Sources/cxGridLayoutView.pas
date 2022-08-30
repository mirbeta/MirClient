{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridLayoutView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Classes, Graphics, Controls, Contnrs, ImgList, StdCtrls,
  dxCore, cxClasses, cxGraphics, cxControls, cxStyles, cxLookAndFeelPainters,
  cxGridCommon, cxGrid, dxCoreClasses, dxGDIPlusClasses, cxGeometry, dxFilterPopupWindow,
  cxGridCustomView, cxGridCustomTableView, dxLayoutLookAndFeels, cxDataStorage,
  cxCustomData, cxEdit, dxLayoutContainer, cxGridCustomLayoutView, dxLayoutSelection,
  dxLayoutCommon, Forms, cxNavigator, cxGridViewLayoutContainer, cxLookAndFeels, cxStorage;

const
  cxGridLayoutViewDefaultMinValueWidth = 80;
  cxGridLayoutViewCustomizationFormDefaultWidth = 800;
  cxGridLayoutViewCustomizationFormDefaultHeight = 600;
  cxGridLayoutViewCarouselModeDefaultRecordCount = 15;
  cxGridLayoutViewCarouselModeDefaultAnimatedDelay = 500;
  cxGridLayoutViewCarouselModeDefaultBackgroundRecordEndScale = 20;
  cxGridLayoutViewCarouselModeDefaultBackgroundRecordStartScale = 60;

  htLayoutViewBase = htCustomGridTableBase + 70;
  htLayoutViewItemIndent = htLayoutViewBase + 1;
  htLayoutViewItemExpandButton = htLayoutViewBase + 2;
  htLayoutViewItem = htLayoutViewBase + 3;
  htLayoutViewItemFilterButton = htLayoutViewBase + 4;
  htLayoutViewRecordCaption = htLayoutViewBase + 5;
  htLayoutViewRecordScrollButtonUp = htLayoutViewBase + 6;
  htLayoutViewRecordScrollButtonDown = htLayoutViewBase + 7;
  htLayoutViewRecordSeparator = htLayoutViewBase + 8;

  vsLayoutViewFirst = vsCustomTableLast + 1;
  vsGroup = vsLayoutViewFirst;
  vsItem = vsLayoutViewFirst + 1;
  vsRecordBorder = vsLayoutViewFirst + 2;
  vsRecordCaption = vsLayoutViewFirst + 3;
  vsItemHottrack = vsLayoutViewFirst + 4;

  cxGridLayoutViewDefaultRecordBorderWidth = 0;
  cxGridLayoutViewRecordExpandButtonOffset = 5;

type
  TcxGridLayoutItem = class;
  TcxGridBaseLayoutItem = class;
  TcxGridLayoutView = class;
  TcxGridLayoutViewItem = class;
  TcxGridLayoutViewController = class;
  TcxGridLayoutViewControllerHelper = class;
  TcxGridLayoutViewRecord = class;

  { View Infos }

  TcxGridLayoutContainerCloneViewInfo = class;
  TcxGridLayoutItemViewInfo = class;
  TcxGridBaseLayoutItemViewInfo = class;
  TcxGridLayoutItemEditViewInfo = class;
  TcxGridLayoutViewViewInfo = class;
  TcxGridLayoutViewRecordViewInfo = class;
  TcxGridLayoutViewRecordsViewInfo = class;

  TcxGridLayoutViewItemFilterButtonViewInfo = class;
  TcxGridLayoutViewItemViewInfo = class;
  TcxGridLayoutViewRecordCaptionViewInfo = class;

  TcxGridLayoutItemEditPainter = class;
  TcxGridLayoutItemEditPainterClass = class of TcxGridLayoutItemEditPainter;
  TcxGridLayoutViewRecordExpandButtonViewInfo = class;

  TcxGridLayoutContainer = class;
  TcxGridLayoutViewOptionsView = class;

  ///////////////////////////////// Layout classes ////////////////////////////////////////

  { TcxGridLayoutItemEditPainter}

  TcxGridLayoutItemEditPainter = class(TcxGridCustomLayoutItemEditPainter)
  protected
    procedure DrawEdit(ACanvas: TcxCanvas); override;
  end;

  { TcxGridLayoutItemPainter }

  TcxGridLayoutItemPainter = class(TcxGridCustomLayoutItemPainter)
  protected
    function GetControlPainterClass: TdxLayoutControlItemControlPainterClass; override;
  end;

  { TcxGridBaseLayoutItemEditViewInfo }

  TcxGridBaseLayoutItemEditViewInfo = class(TcxGridCustomLayoutItemEditViewInfo)
  private
    function GetItem: TcxGridLayoutItem;inline;
    function GetGridView: TcxGridLayoutView;inline;
  protected
    function GetDefaultValueHeight: Integer; override;
    function GetRecord: TcxGridLayoutViewRecord; virtual;
    function GetMinValueWidth: Integer; override;

    property GridView: TcxGridLayoutView read GetGridView;
    property Item: TcxGridLayoutItem read GetItem;
  end;

  { TcxGridLayoutItemEditViewInfo }

  TcxGridLayoutItemEditViewInfo = class(TcxGridBaseLayoutItemEditViewInfo)
  private
    function GetDataViewInfo: TcxGridLayoutViewItemViewInfo;inline;
  protected
    function GetRecord: TcxGridLayoutViewRecord; override;

    property DataViewInfo: TcxGridLayoutViewItemViewInfo read GetDataViewInfo;
  end;

  { TcxGridBaseLayoutItemCaptionViewInfo }

  TcxGridBaseLayoutItemCaptionViewInfo = class(TcxGridCustomLayoutItemCaptionViewInfo)
  private
    function GetItem: TcxGridBaseLayoutItem;inline;
    function GetItemViewInfo: TcxGridBaseLayoutItemViewInfo;inline;
  protected
    function CalculatePadding: TRect; override;

    property Item: TcxGridBaseLayoutItem read GetItem;
    property ItemViewInfo: TcxGridBaseLayoutItemViewInfo read GetItemViewInfo;
  end;

  { TcxGridLayoutItemCaptionViewInfo }

  TcxGridLayoutItemCaptionViewInfo = class(TcxGridBaseLayoutItemCaptionViewInfo)
  private
    function GetItem: TcxGridLayoutItem;inline;
    function GetItemViewInfo: TcxGridLayoutItemViewInfo;inline;
  protected
    property Item: TcxGridLayoutItem read GetItem;
    property ItemViewInfo: TcxGridLayoutItemViewInfo read GetItemViewInfo;
  end;

  { TcxGridBaseLayoutItemViewInfo }

  TcxGridBaseLayoutItemViewInfo = class(TcxGridCustomLayoutItemViewInfo)
  private
    function GetGridItemViewInfo: TcxGridLayoutViewItemViewInfo;inline;
    function GetItem: TcxGridBaseLayoutItem;inline;
  protected
    function GetActuallyVisible: Boolean; override;
    function GetCaptionViewInfoClass: TdxCustomLayoutItemCaptionViewInfoClass; override;
    function GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass; override;

    property Item: TcxGridBaseLayoutItem read GetItem;
  public
    property GridItemViewInfo: TcxGridLayoutViewItemViewInfo read GetGridItemViewInfo;
  end;

  { TcxGridLayoutItemViewInfo }

  TcxGridLayoutItemViewInfo = class(TcxGridBaseLayoutItemViewInfo)
  private
    function GetControlViewInfo: TcxGridLayoutItemEditViewInfo;inline;
    function GetItem: TcxGridLayoutItem;inline;
    function GetContainerViewInfo: TcxGridLayoutContainerViewInfo;inline;
  protected
    function GetCaptionViewInfoClass: TdxCustomLayoutItemCaptionViewInfoClass; override;
    function GetCurrentGridItemViewInfo: TcxGridTableDataCellViewInfo; override;
    function GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass; override;
    function GetPainterClass: TdxCustomLayoutItemPainterClass; override;

    property Item: TcxGridLayoutItem read GetItem;
  public
    property ContainerViewInfo: TcxGridLayoutContainerViewInfo read GetContainerViewInfo;
    property ControlViewInfo: TcxGridLayoutItemEditViewInfo read GetControlViewInfo;
  end;

  { TcxGridLayoutItemCaptionOptions }

  TcxGridLayoutItemCaptionOptions = class(TcxGridCustomLayoutItemCaptionOptions)
  private
    function GetItem: TcxGridLayoutItem;inline;
  public
    property Item: TcxGridLayoutItem read GetItem;
  published
    property AlignHorz;
    property AlignVert;
    property Glyph;
    property ImageIndex;
    property Layout default clLeft;
    property Visible;
    property VisibleElements;
    property Width;
  end;

  { TcxGridBaseLayoutItem }

  TcxGridBaseLayoutItem = class(TcxGridCustomLayoutItem)
  private
    function GetGridViewItem: TcxGridLayoutViewItem;
    function GetViewInfo: TcxGridBaseLayoutItemViewInfo; inline;
    procedure SetGridViewItem(Value: TcxGridLayoutViewItem); inline;
  protected
    function GetViewInfoClass: TdxCustomLayoutItemViewInfoClass; override;
    function IsVisibleForCustomization: Boolean; override;

    function CaptionToDisplayCaption(AValue: string): string; override;
    function DisplayCaptionToCaption(AValue: string): string; override;
  public
    procedure BeforeDestruction; override;

    property GridViewItem: TcxGridLayoutViewItem read GetGridViewItem write SetGridViewItem;
    property ViewInfo: TcxGridBaseLayoutItemViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutItem }

  TcxGridLayoutItem = class(TcxGridBaseLayoutItem)
  private
    function GetContainer: TcxGridLayoutContainer; inline;
    function GetCaptionOptions: TcxGridLayoutItemCaptionOptions; inline;
    procedure SetContainer(Value: TcxGridLayoutContainer); inline;
    procedure SetCaptionOptions(Value: TcxGridLayoutItemCaptionOptions); inline;
    function GetViewInfo: TcxGridLayoutItemViewInfo; inline;
  protected
    class function GetCaptionOptionsClass: TdxCustomLayoutItemCaptionOptionsClass; override;
    function GetViewInfoClass: TdxCustomLayoutItemViewInfoClass; override;
  public
    property Container: TcxGridLayoutContainer read GetContainer write SetContainer;
    property ViewInfo: TcxGridLayoutItemViewInfo read GetViewInfo;
  published
    property CaptionOptions: TcxGridLayoutItemCaptionOptions read GetCaptionOptions write SetCaptionOptions;
  end;

  { TcxGridLayoutLookAndFeel }

  TcxGridLayoutLookAndFeel = class(TcxGridCustomLayoutLookAndFeel)
  private
    function GetGridView: TcxGridLayoutView;
  public
    constructor Create(AGridView: TcxCustomGridTableView); override;

    property GridView: TcxGridLayoutView read GetGridView;
  end;

  { TcxGridLayoutContainerCloneViewInfo }

  TcxGridLayoutContainerCloneViewInfo = class(TcxGridRecordLayoutContainerViewInfo)
  private
    function GetRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
  protected
    function CanAssignBounds(ASource: TdxLayoutContainerViewInfo): Boolean; override;
    function CanShowGroupScrollBars: Boolean; override;
    procedure DestroyViewInfos; override;
    function FindGridItemViewInfo(AViewInfo: TcxGridCustomLayoutItemViewInfo): TcxGridTableDataCellViewInfo; override;
    function GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel; override;
    function IsActuallyVisible: Boolean; override;
  public
    function GetItemViewInfo(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemViewInfo; override;

    property RecordViewInfo: TcxGridLayoutViewRecordViewInfo read GetRecordViewInfo;
  end;

  { TcxGridLayoutContainerFocusController }

  TcxGridLayoutContainerFocusController = class(TdxLayoutContainerFocusController)
  protected
    function IsFocused(AItem: TdxCustomLayoutItem): Boolean; override;
  end;

  { TcxGridLayoutContainer }

  TcxGridLayoutContainer = class(TcxGridCustomLayoutContainer)
  private
    function GetGridLayoutView: TcxGridLayoutView;
    function GetViewInfo: TcxGridLayoutContainerViewInfo;
  protected
    procedure AfterRestoring; override;
    function CanRootScrollingHorizontal: Boolean; virtual;
    function CanRootScrollingVertical: Boolean; virtual;
    procedure DoCalculateRoot(ANeedPack: Boolean); override;
    procedure DoGroupScroll(Sender: TObject); override;
    procedure DoInitialize; override;
    procedure DoUpdateRootOptions; override;
    function GetFocusControllerClass: TdxLayoutContainerFocusControllerClass; override;
    function GetViewInfoClass: TdxLayoutContainerViewInfoClass; override;
    function IsCustomization: Boolean; override;

    function GetClientBounds: TRect; override;
    function GetClientRect: TRect; override;
    function GetDefaultFont: TFont; override;
    function NeedStretchRecordHeight: Boolean; override;
    function NeedStretchRecordWidth: Boolean; override;
    property GridView: TcxGridLayoutView read GetGridLayoutView;
    property ViewInfo: TcxGridLayoutContainerViewInfo read GetViewInfo;
  public
    property MenuItems;
  end;

  TcxGridLayoutContainerClass = class of TcxGridLayoutContainer;

  ///////////////////////////////// Grid classes ////////////////////////////////////////

  TcxGridLayoutViewRecordExpandButtonAlignment = (rebaLeft, rebaRight);

  { hit tests }

  TcxGridLayoutViewItemFilterButtonHitTest = class(TcxGridRecordCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridLayoutViewItemHitTest = class(TcxGridRecordCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridLayoutViewRecordCaptionHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

{
  TcxGridLayoutViewRecordScrollButtonDownHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridLayoutViewRecordScrollButtonUpHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;
}

  { Painters }

  TcxGridLayoutViewItemFilterButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridLayoutViewItemFilterButtonViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxGridLayoutViewItemFilterButtonViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewItemPainter }

  TcxGridLayoutViewItemPainter = class(TcxGridViewLayoutItemDataCellPainter)
  private
    function GetViewInfo: TcxGridLayoutViewItemViewInfo;
  protected
    procedure DrawContent; override;

    property ViewInfo: TcxGridLayoutViewItemViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewRecordExpandButtonPainter }

  TcxGridLayoutViewRecordExpandButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridLayoutViewRecordExpandButtonViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridLayoutViewRecordExpandButtonViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewRecordCaptionPainter }

  TcxGridLayoutViewRecordCaptionPainter = class(TcxCustomGridCellPainter)
  strict private
    function GetViewInfo: TcxGridLayoutViewRecordCaptionViewInfo;
  protected
    procedure DrawBackground; override;
    function GetState: TcxButtonState; virtual;

    property ViewInfo: TcxGridLayoutViewRecordCaptionViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewRecordPainter }

  TcxGridLayoutViewRecordPainter = class(TcxCustomGridRecordPainter)
  private
    FClipRegion: TcxRegion;
    function GetViewInfo: TcxGridLayoutViewRecordViewInfo;
  protected
    procedure AfterPaint; override;
    procedure BeforePaint; override;
    function CanDrawBackground: Boolean; virtual;
    function CanDrawExpandButton: Boolean; virtual;
    procedure DrawBackground; override;
    procedure DrawLayoutGroups; virtual;
    procedure DrawRecordBorder; virtual;
    procedure DrawRecordCaption; virtual;
    procedure DrawExpandButton; override;
    function DrawExpandButtonBeforePaint: Boolean; override;
    procedure DrawItems; virtual;
    function GetState: TcxButtonState; virtual;
    function IsRecordExpanded: Boolean;
    procedure Paint; override;

    property ViewInfo: TcxGridLayoutViewRecordViewInfo read GetViewInfo;
  end;

  TcxGridLayoutViewPainter = class(TcxGridCustomLayoutViewPainter);

  { TcxGridLayoutViewViewData }

  TcxGridLayoutViewViewData = class(TcxGridCustomLayoutViewViewData)
  protected
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
  end;

  { View Infos }

  { TcxGridLayoutViewItemFilterButtonViewInfo }

  TcxGridLayoutViewItemFilterButtonViewInfo = class(TcxGridCustomLayoutViewItemFilterButtonViewInfo)
  private
    FItemViewInfo: TcxGridLayoutViewItemViewInfo;
    function GetGridView: TcxGridLayoutView; inline;
  protected
    function GetItem: TcxCustomGridTableItem; override;

    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function GetDropDownWindowOwnerBounds: TRect; override;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; override;

    property ItemViewInfo: TcxGridLayoutViewItemViewInfo read FItemViewInfo;
  public
    constructor Create(ALayoutViewItemViewInfo: TcxGridLayoutViewItemViewInfo); reintroduce; virtual;

    property GridView: TcxGridLayoutView read GetGridView;
  end;

  TcxGridLayoutViewItemFilterButtonViewInfoClass = class of TcxGridLayoutViewItemFilterButtonViewInfo;

  { TcxGridLayoutViewItemViewInfo }

  TcxGridLayoutViewItemViewInfo = class(TcxGridViewLayoutItemDataCellViewInfo)
  private
    FFilterButtonViewInfo: TcxGridLayoutViewItemFilterButtonViewInfo;
    FItem: TcxGridLayoutViewItem;
    FLayoutItemViewInfo: TcxGridLayoutItemViewInfo;
    FRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
    procedure CreateViewInfos;
    procedure DestroyViewInfos;
    function GetGridView: TcxGridLayoutView;
    function GetGridRecord: TcxGridLayoutViewRecord;
    function GetLayoutItemViewInfo: TcxGridLayoutItemViewInfo;
    function GetRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
  protected
    function AllowRightToLeftConversionOnRecalculate: Boolean; override;
    procedure GetCaptionParams(var AParams: TcxViewParams); override;
    function GetDesignSelectionBounds: TRect; override;
    function GetEditBounds: TRect; override;
    function GetEditMinSize(AFont: TFont): TSize; virtual;
    function GetEditViewDataBounds: TRect; override;
    function GetExtraCaptionPadding: Integer; virtual;
    function GetFilterButtonBounds: TRect; virtual;
    function GetFilterButtonViewInfoClass: TcxGridLayoutViewItemFilterButtonViewInfoClass; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetLayoutItemViewInfoInstance: TcxGridCustomLayoutItemViewInfo; override;
    function GetIsDesignSelected: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function HasFilterButton: Boolean; virtual;
    function HasFocusRect: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function InvalidateOnStateChange: Boolean; override;
    procedure StateChanged(APrevState: TcxGridCellState); override;
    function CanFocus: Boolean; override;
    function GetRealEditViewDataBounds: TRect; override;

    property FilterButtonBounds: TRect read GetFilterButtonBounds;
    property LayoutItemViewInfo: TcxGridLayoutItemViewInfo read GetLayoutItemViewInfo;
  public
    constructor Create(ARecordViewInfo: TcxGridLayoutViewRecordViewInfo; AItem: TcxGridLayoutViewItem);  reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function HasPoint(const P: TPoint): Boolean; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean; override;

    property GridRecord: TcxGridLayoutViewRecord read GetGridRecord;
    property GridView: TcxGridLayoutView read GetGridView;
    property Item: TcxGridLayoutViewItem read FItem;
    property FilterButtonViewInfo: TcxGridLayoutViewItemFilterButtonViewInfo read FFilterButtonViewInfo;
    property RecordViewInfo: TcxGridLayoutViewRecordViewInfo read GetRecordViewInfo;
  end;

  TcxGridLayoutViewItemViewInfoClass = class of TcxGridLayoutViewItemViewInfo;

  { TcxGridLayoutViewRecordExpandButtonViewInfo }

  TcxGridLayoutViewRecordExpandButtonViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    procedure Click; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(ARecordViewInfo: TcxGridLayoutViewRecordViewInfo); reintroduce; virtual;
    function HasPoint(const P: TPoint): Boolean; override;
    property RecordViewInfo: TcxGridLayoutViewRecordViewInfo read FRecordViewInfo;
  end;

  TcxGridLayoutViewRecordExpandButtonViewInfoClass = class of TcxGridLayoutViewRecordExpandButtonViewInfo;

  { TcxGridLayoutViewRecordCaptionViewInfo }

  TcxGridLayoutViewRecordCaptionViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
    function GetGridRecord: TcxGridLayoutViewRecord;
    function GetGridView: TcxGridLayoutView;
    function GetRotatedTextBounds: TRect;
    function GetTextBoundsValue: TRect;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function HasCustomDraw: Boolean; override;
    procedure PrepareCanvas(ACanvas: TcxCanvas); override;
    //text
    function GetDrawTextRotationAngle: TcxRotationAngle; override;
    function GetRotatedText: Boolean;
    function GetTextAreaBounds: TRect; override;
    function GetTextAttributes(AForPainting: Boolean): Integer; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function GetVisibleForHitTest: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(ARecordViewInfo: TcxGridLayoutViewRecordViewInfo); reintroduce; virtual;

    property GridView: TcxGridLayoutView read GetGridView;
    property GridRecord: TcxGridLayoutViewRecord read GetGridRecord;
    property RecordViewInfo: TcxGridLayoutViewRecordViewInfo read FRecordViewInfo;
    property RotatedText: Boolean read GetRotatedText;
    property TextBounds: TRect read GetTextBoundsValue;
  end;

  TcxGridLayoutViewRecordCaptionViewInfoClass = class of TcxGridLayoutViewRecordCaptionViewInfo;

  { Cache }

  TcxGridLayoutViewRecordViewInfoCacheItem = class(TcxGridCustomLayoutViewInfoCacheItem)
  private
    FIsRecordCaptionHeightAssigned: Boolean;
    FRecordCaptionHeight: Integer;
    function GetGridRecord: TcxGridLayoutViewRecord;
    procedure SetRecordCaptionHeight(Value: Integer);
  protected
    procedure UnassignHeightValues;

    property GridRecord: TcxGridLayoutViewRecord read GetGridRecord;
  public
    procedure UnassignValues(AKeepMaster: Boolean); override;
    property RecordCaptionHeight: Integer read FRecordCaptionHeight write SetRecordCaptionHeight;
    property IsRecordCaptionHeightAssigned: Boolean read FIsRecordCaptionHeightAssigned write FIsRecordCaptionHeightAssigned;
  end;

  { TcxGridLayoutViewRecordViewInfo }

  TcxGridLayoutViewRecordViewInfo = class(TcxGridCustomLayoutRecordViewInfo)
  private
    FCalculatedCaptionHeight: Integer;
    FCaptionViewInfo: TcxGridLayoutViewRecordCaptionViewInfo;
    FExpandButtonViewInfo: TcxGridLayoutViewRecordExpandButtonViewInfo;
    FIsPixelScrollSizeCalculating: Boolean;
    FItemViewInfos: TcxObjectList;
    FLayoutCalculated: Boolean;
    FLayoutViewInfo: TcxGridLayoutContainerCloneViewInfo;
    FLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
    function GetCacheItem: TcxGridLayoutViewRecordViewInfoCacheItem;
    function GetExpandButtonSizeValue: Integer;
    function GetGridView: TcxGridLayoutView;
    function GetGridViewInfo: TcxGridLayoutViewViewInfo;
    function GetItemViewInfoCount: Integer;
    function GetItemViewInfos(Index: Integer): TcxGridLayoutViewItemViewInfo;
    function GetGridRecord: TcxGridLayoutViewRecord;
    function GetRecordBorderWidth: Integer;
    function GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
    procedure SetLayoutCalculated(AValue: Boolean);
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CanShowGroupScrollBars: Boolean; virtual;
    function HasFocusRect: Boolean; override;
    function GetHeight: Integer; override;
    function GetPixelScrollSize: Integer; override;
    function GetWidth: Integer; override;
    function GetTransparent: Boolean; override;
    function GetVisible: Boolean; override;

    procedure CalculateExpandButtonBounds(var ABounds: TRect); override;
    function CanGenerateExpandButtonHitTest: Boolean; override;
    function GetExpandButtonAlignment: TcxGridLayoutViewRecordExpandButtonAlignment; virtual;
    function GetExpandButtonAreaBounds: TRect; override;
    function GetExpandButtonAreaWidth: Integer; virtual;

    function CalculateRecordCaptionHeight: Integer; virtual;
    function CanGetChildrenHitTest: Boolean; virtual;
    function GetEditViewDataBounds(AItem: TcxGridLayoutViewItemViewInfo): TRect; virtual;

    function GetRecordCaptionBounds: TRect; virtual;
    function GetRecordCaptionHeight: Integer; virtual;
    function GetRecordCaptionPosition: TcxGroupBoxCaptionPosition;
    function GetRecordCaptionTailSize: Integer; virtual;
    function GetRecordCaptionText: string; virtual;
    function HasRecordCaption: Boolean; virtual;
    function HasItemFocusRect(AItem: TcxGridLayoutViewItemViewInfo): Boolean; virtual;
    function IsExpanded: Boolean; virtual;
    function IsGridViewInfoRightToLeftConverted: Boolean; virtual;
    function IsRecordCaptionRotated: Boolean; virtual;

    function GetLayoutAreaBounds: TRect; virtual;
    function GetLayoutAreaBorders: TcxBorders;

    procedure CalculateItemViewInfos; virtual;
    procedure CreateItemViewInfos; virtual;
    procedure DestroyItemViewInfos; virtual;

    procedure PopulateTabOrderList(AList: TList); virtual;

    //internal classes
    function GetExpandButtonViewInfoClass: TcxGridLayoutViewRecordExpandButtonViewInfoClass; virtual;
    function GetItemViewInfoClass: TcxGridLayoutViewItemViewInfoClass; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRecordCaptionViewInfoClass: TcxGridLayoutViewRecordCaptionViewInfoClass; virtual;

    procedure CalculateLayout; virtual;
    procedure CheckCloneData;
    procedure CheckFocusedItem;
    procedure ClearItemLinks;
    procedure CloneDataChangedHandler(Sender: TObject);
    function IsDefaultCloneData: Boolean;
    function LoadCloneData: Boolean;
    procedure RecalculateCloneData;
    procedure ResetCalculated; virtual;
    procedure ResetCloneData;
    procedure UpdateContainerStyles;
    procedure SaveCloneData;

    procedure CalculateLayoutViewInfo;
    procedure CreateLayoutViewInfo;

    property CacheItem: TcxGridLayoutViewRecordViewInfoCacheItem read GetCacheItem;
    property ExpandButtonAlignment: TcxGridLayoutViewRecordExpandButtonAlignment read GetExpandButtonAlignment;
    property ExpandButtonAreaWidth: Integer read GetExpandButtonAreaWidth;
    property ExpandButtonSize: Integer read GetExpandButtonSizeValue;
    property LayoutAreaBounds: TRect read GetLayoutAreaBounds;
    property LayoutAreaBorders: TcxBorders read GetLayoutAreaBorders;
    property LayoutCalculated: Boolean read FLayoutCalculated write SetLayoutCalculated;
    property RecordCaptionBounds: TRect read GetRecordCaptionBounds;
    property RecordCaptionPosition: TcxGroupBoxCaptionPosition read GetRecordCaptionPosition;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;

    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure MainCalculate(ALeftBound, ATopBound: Integer); override;

    function Click(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect; override;
    function GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure GetRecordBorderViewParams(var AParams: TcxViewParams); virtual;

    property CaptionViewInfo: TcxGridLayoutViewRecordCaptionViewInfo read FCaptionViewInfo;
    property ExpandButtonViewInfo: TcxGridLayoutViewRecordExpandButtonViewInfo read FExpandButtonViewInfo;
    property GridRecord: TcxGridLayoutViewRecord read GetGridRecord;
    property GridView: TcxGridLayoutView read GetGridView;
    property GridViewInfo: TcxGridLayoutViewViewInfo read GetGridViewInfo;
    property ItemViewInfoCount: Integer read GetItemViewInfoCount;
    property ItemViewInfos[Index: Integer]: TcxGridLayoutViewItemViewInfo read GetItemViewInfos;
    property LayoutViewInfo: TcxGridLayoutContainerCloneViewInfo read FLayoutViewInfo;
    property LayoutLookAndFeel: TcxGridLayoutLookAndFeel read FLayoutLookAndFeel;
    property RecordBorderWidth: Integer read GetRecordBorderWidth;
    property RecordsViewInfo: TcxGridLayoutViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  // calculators

  { TcxGridLayoutViewInfoHorizontalCalculator }

  TcxGridLayoutViewInfoHorizontalCalculator = class(TcxGridCustomLayoutViewInfoHorizontalCalculator)
  private
    function GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    procedure DoCalculate(const AContentBounds: TRect); override;
    function GetMaxColumnCount: Integer; override;
    function GetMaxRowCount: Integer; override;

    function CanCenterHorizontally: Boolean; virtual;
    function CanCenterVertically: Boolean; virtual;
    procedure CenterHorizontally(const AContentBounds: TRect);
    procedure CenterVertically(const AContentBounds: TRect);
  public
    property RecordsViewInfo: TcxGridLayoutViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridLayoutViewInfoVerticalCalculator }

  TcxGridLayoutViewInfoVerticalCalculator = class(TcxGridCustomLayoutViewInfoVerticalCalculator)
  private
    function GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
    function GetRowCount: Integer;
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    procedure DoCalculate(const AContentBounds: TRect); override;
    function GetMaxColumnCount: Integer; override;
    function GetMaxRowCount: Integer; override;

    function CanCenterHorizontally: Boolean; virtual;
    function CanCenterVertically: Boolean; virtual;
    procedure CenterHorizontally(const AContentBounds: TRect);
    procedure CenterVertically(const AContentBounds: TRect);
  public
    property RecordsViewInfo: TcxGridLayoutViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridLayoutViewInfoSingleRecordCalculator }

  TcxGridLayoutViewInfoSingleRecordCalculator = class(TcxGridLayoutViewInfoHorizontalCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    function CanCenterVertically: Boolean; override;
    function GetMaxColumnCount: Integer; override;
    function GetMaxRowCount: Integer; override;
  end;

  { TcxGridLayoutViewInfoSingleColumnCalculator }

  TcxGridLayoutViewInfoSingleColumnCalculator = class(TcxGridLayoutViewInfoVerticalCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    function CanCenterHorizontally: Boolean; override;
    function CanCenterVertically: Boolean; override;
    function GetMaxColumnCount: Integer; override;
  end;

  { TcxGridLayoutViewInfoMultiColumnCalculator }

  TcxGridLayoutViewInfoMultiColumnCalculator = class(TcxGridLayoutViewInfoHorizontalCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    function CanCenterHorizontally: Boolean; override;
  end;

  { TcxGridLayoutViewInfoSingleRowCalculator }

  TcxGridLayoutViewInfoSingleRowCalculator = class(TcxGridLayoutViewInfoHorizontalCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    function CanCenterHorizontally: Boolean; override;
    function CanCenterVertically: Boolean; override;
    function GetMaxRowCount: Integer; override;
  end;

  { TcxGridLayoutViewInfoMultiRowCalculator }

  TcxGridLayoutViewInfoMultiRowCalculator = class(TcxGridLayoutViewInfoVerticalCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    function CanCenterVertically: Boolean; override;
  end;

  { TcxGridLayoutViewRecordsViewInfo }

  TcxGridLayoutViewRecordsViewInfo = class(TcxGridCustomLayoutRecordsViewInfo)
  private
    function GetController: TcxGridLayoutViewController;inline;
    function GetControllerHelper: TcxGridLayoutViewControllerHelper;
    function GetGridView: TcxGridLayoutView;inline;
    function GetGridViewInfo: TcxGridLayoutViewViewInfo;inline;
    function GetItem(Index: Integer): TcxGridLayoutViewRecordViewInfo;inline;
    function GetViewData: TcxGridLayoutViewViewData;inline;
  protected
    function CalculateRecordWidth: Integer; override;
    procedure CalculateVisibleRecordsBound(out AFirstRecordIndex, ALastRecordIndex: Integer); virtual;
    function GetAutoDataRecordHeight: Boolean; override;

    function GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass; override;
    function GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass; override;
    function GetScrollableAreaBoundsForEdit: TRect; virtual;
    function IsCarouselMode: Boolean; virtual;

    function CanCenterRecords: Boolean;

    property Controller: TcxGridLayoutViewController read GetController;
    property ControllerHelper: TcxGridLayoutViewControllerHelper read GetControllerHelper;
    property GridView: TcxGridLayoutView read GetGridView;
    property GridViewInfo: TcxGridLayoutViewViewInfo read GetGridViewInfo;
    property Items[Index: Integer]: TcxGridLayoutViewRecordViewInfo read GetItem; default;
    property ViewData: TcxGridLayoutViewViewData read GetViewData;
  end;

  // separators

  { TcxGridLayoutViewSeparatorsViewInfo }

  TcxGridLayoutViewSeparatorsViewInfo = class(TcxGridCustomLayoutViewSeparatorsViewInfo)
  private
    function GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
  protected
    function CalculateMaxBottom(ARow: Integer): Integer;
    function CalculateMaxRight(AColumn: Integer): Integer;
    function CalculateMinLeft(AColumn: Integer): Integer;
    function CalculateMinTop(ARow: Integer): Integer;
  public
    property RecordsViewInfo: TcxGridLayoutViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridLayoutViewSingleRecordSeparatorsViewInfo }

  TcxGridLayoutViewSingleRecordSeparatorsViewInfo = class(TcxGridLayoutViewSeparatorsViewInfo);

  { TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo }

  TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo = class(TcxGridLayoutViewSeparatorsViewInfo)
  public
    procedure DoCalculate; override;
  end;

  { TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo }

  TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo = class(TcxGridLayoutViewSeparatorsViewInfo)
  public
    procedure DoCalculate; override;
  end;

  { TcxGridLayoutViewMultiColumnRecordSeparatorsViewInfo }

  TcxGridLayoutViewMultiColumnRecordSeparatorsViewInfo = class(TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo)
  public
    procedure DoCalculate; override;
  end;

  { TcxGridLayoutViewMultiRowRecordSeparatorsViewInfo }

  TcxGridLayoutViewMultiRowRecordSeparatorsViewInfo = class(TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo)
  public
    procedure DoCalculate; override;
  end;

  { TcxGridLayoutViewSite }

  TcxGridLayoutViewSite = class(TcxGridSite)
  private
    function GetGridView: TcxGridLayoutView; inline;
  protected
    procedure UpdateSize; override;
  public
    property GridView: TcxGridLayoutView read GetGridView;
  end;

  { TcxGridLayoutViewViewInfo }

  TcxGridLayoutViewViewInfo = class(TcxGridCustomLayoutViewViewInfo)
  private
    FIsHeightCalculating: Boolean;
    function GetController: TcxGridLayoutViewController;
    function GetGridView: TcxGridLayoutView;
    function GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
  protected
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean); override;
    function GetAllowBoundsChangedNotification: Boolean; override;
    function GetFirstRecordIndex: Integer; override;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
    function GetScrollableAreaBoundsForEdit: TRect; override;
    function GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass; override;
    function GetSiteClass: TcxGridSiteClass; override;
    function IsDoubleBufferedNeeded: Boolean; override;

    function GetRecordCaptionHeight: Integer;
    function IsCarouselMode: Boolean;

    property Controller: TcxGridLayoutViewController read GetController;
    property IsHeightCalculating: Boolean read FIsHeightCalculating;
  public
    property GridView: TcxGridLayoutView read GetGridView;
    property RecordsViewInfo: TcxGridLayoutViewRecordsViewInfo read GetRecordsViewInfo;
  end;

  { TcxGridLayoutViewItemOptions }

  TcxGridLayoutViewItemOptions = class(TcxCustomGridTableItemOptions)
  published
    property FilteringAddValueItems;
    property FilteringExcelPopupApplyChanges;
    property FilteringExcelPopupDateTimeValuesPageType;
    property FilteringExcelPopupDefaultPage;
    property FilteringExcelPopupNumericValuesPageType;
    property FilteringFilteredItemsList;
    property FilteringMRUItemsList;
    property FilteringPopup;
    property FilteringPopupIncrementalFiltering;
    property FilteringPopupIncrementalFilteringOptions;
    property FilteringPopupMode;
    property FilteringPopupMultiSelect;
    property ShowEditButtons;
  end;

  { TcxGridLayoutViewItemStyles }

  TcxGridLayoutViewItemStyles = class(TcxCustomGridTableItemStyles);

  { TcxGridLayoutViewItem }

  TcxGridLayoutViewItem = class(TcxCustomGridTableItem)
  private
    FLayoutItem: TcxGridLayoutItem;
    function GetGridView: TcxGridLayoutView;
    function GetOptions: TcxGridLayoutViewItemOptions;
    function GetStyles: TcxGridLayoutViewItemStyles;
    function GetFocusedLayoutViewItemViewInfo: TcxGridLayoutViewItemViewInfo;
    procedure SetLayoutItem(const Value: TcxGridLayoutItem);
    procedure SetOptions(Value: TcxGridLayoutViewItemOptions);
    procedure SetStyles(Value: TcxGridLayoutViewItemStyles);
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CanAutoHeight: Boolean; override;
    function CanFocus(ARecord: TcxCustomGridRecord): Boolean; override;
    procedure CaptionChanged; override;
    procedure ChangeGroupIndex(Value: Integer); override;
    function GetActuallyVisible: Boolean; override;
    function GetEditPropertiesClass: TcxCustomEditPropertiesClass;
    function GetEditMinSize(AFont: TFont): TSize;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; override;
    function GetVisible: Boolean; override;
    function GetVisibleCaption: string; override;
    procedure DoSetVisible(Value: Boolean); override;
    function IsFocusedCellViewInfoPartVisible: Boolean; override;

    function CaptionToDisplayCaption(const ACaption: string): string; virtual;
    function DisplayCaptionToCaption(const ADisplayCaption: string): string; virtual;

    procedure SetGridView(Value: TcxCustomGridTableView); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridLayoutView read GetGridView;
    property FocusedCellViewInfo: TcxGridLayoutViewItemViewInfo read GetFocusedLayoutViewItemViewInfo;
  published
    property LayoutItem: TcxGridLayoutItem read FLayoutItem write SetLayoutItem;
    property Options: TcxGridLayoutViewItemOptions read GetOptions write SetOptions;
    property SortIndex;
    property SortOrder;
    property Styles: TcxGridLayoutViewItemStyles read GetStyles write SetStyles;
  end;

  { TcxGridLayoutViewRecord }

  TcxGridLayoutViewRecord = class(TcxGridCustomLayoutRecord)
  private
    FCloneDataStream: TMemoryStream;
    function GetGridView: TcxGridLayoutView;
    function GetViewInfo: TcxGridLayoutViewRecordViewInfo;
  protected
    function GetExpanded: Boolean; override;
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;

    property CloneDataStream: TMemoryStream read FCloneDataStream;
  public
    constructor Create(AViewData: TcxCustomGridTableViewData; AIndex: Integer; const ARecordInfo: TcxRowInfo); override;
    destructor Destroy; override;

    property GridView: TcxGridLayoutView read GetGridView;
    property ViewInfo: TcxGridLayoutViewRecordViewInfo read GetViewInfo;
  end;

  { TcxGridLayoutViewControllerHelper }

  TcxGridLayoutViewControllerHelper = class(TcxGridCustomLayoutViewControllerHelper)
  private
    function GetController: TcxGridLayoutViewController;
    function GetLayoutContainerViewInfo(AHitTest: TcxGridRecordHitTest): TcxGridLayoutContainerCloneViewInfo;
    function GetViewInfo: TcxGridLayoutViewViewInfo;
  protected
    function CanScrollBarVisible(AKind: TScrollBarKind): Boolean; override;
    procedure DoInitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean); override;
    function GetContentPageSize: Integer; virtual; abstract;
    function GetContentScrollSize: Integer; virtual;
    function NeedContentScrollBar: Boolean;

    property Controller: TcxGridLayoutViewController read GetController;
    property ViewInfo: TcxGridLayoutViewViewInfo read GetViewInfo;
  public
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollDelta: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TcxGridLayoutViewControllerHorizontalHelper }

  TcxGridLayoutViewControllerHorizontalHelper = class(TcxGridLayoutViewControllerHelper)
  protected
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; override;
    function GetContentPageSize: Integer; override;
    function GetContentScrollSize: Integer; override;
  end;

  { TcxGridCustomLayoutViewControllerVerticalHelper }

  TcxGridLayoutViewControllerVerticalHelper = class(TcxGridLayoutViewControllerHelper)
  protected
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; override;
    function GetContentPageSize: Integer; override;
    function GetContentScrollSize: Integer; override;
  end;

  { TcxGridLayoutViewController }

  TcxGridLayoutViewController = class(TcxGridCustomLayoutViewController)
  private
    FContentScrollBarPosition: Integer;
    FLayoutCustomizationFormHelper: TcxGridLayoutCustomizationFormHelper;
    function GetFocusedItem: TcxGridLayoutViewItem;
    function GetFocusedRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
    function GetGridView: TcxGridLayoutView;
    function GetHelper: TcxGridLayoutViewControllerHelper;
    function GetViewInfo: TcxGridLayoutViewViewInfo;
    procedure SetContentScrollBarPosition(Value: Integer);
    procedure SetFocusedItem(Value: TcxGridLayoutViewItem);
  protected
    // Customization
    function CanCustomize: Boolean; override;
    procedure CheckCustomizationFormBounds(var R: TRect); override;
    function CreateCustomizationForm: TForm; override;
    procedure CustomizationChanged; override;
    procedure DoCreateCustomizationForm; override;
    function GetCustomizationFormBounds: TRect; override;
    function GetCustomizationFormDefaultWidth: Integer; override;
    function GetCustomizationFormDefaultHeight: Integer; override;

    //scrolling
    procedure ScrollContentScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer); override;

    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass; override;

    procedure CheckFocusItem(AItemViewInfo: TcxGridLayoutViewItemViewInfo);
    function GetNextIndex(AFocusedIndex: Integer; AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): Integer;

    property ContentScrollBarPosition: Integer read FContentScrollBarPosition write SetContentScrollBarPosition;
    property FocusedRecordViewInfo: TcxGridLayoutViewRecordViewInfo read GetFocusedRecordViewInfo;
    property Helper: TcxGridLayoutViewControllerHelper read GetHelper;
    property LayoutCustomizationFormHelper: TcxGridLayoutCustomizationFormHelper read FLayoutCustomizationFormHelper;
    property ViewInfo: TcxGridLayoutViewViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    function FindNextItem(AFocusedItemIndex: Integer; AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean;
      out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer; override;
    function FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean; override;
    function IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean; override;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); override;

    property FocusedItem: TcxGridLayoutViewItem read GetFocusedItem write SetFocusedItem;
    property GridView: TcxGridLayoutView read GetGridView;
  end;

  { TcxGridLayoutViewDateTimeHandling }

  TcxGridLayoutViewDateTimeHandling = class(TcxCustomGridTableDateTimeHandling);

  { TcxGridLayoutViewFiltering }

  TcxGridLayoutViewFiltering = class(TcxCustomGridTableFiltering)
  published
    property ItemAddValueItems;
    property ItemExcelPopup;
    property ItemFilteredItemsList;
    property ItemMRUItemsList;
    property ItemMRUItemsListCount;
    property ItemPopup;
    property ItemPopupMode;
    property MRUItemsList default True;
    property MRUItemsListCount default cxGridFilterDefaultMRUItemsListCount;
  end;

  { TcxGridLayoutViewOptionsData }

  TcxGridLayoutViewOptionsData = class(TcxCustomGridTableOptionsData);

  { TcxGridLayoutViewOptionsSelection }

  TcxGridLayoutViewOptionsSelection = class(TcxCustomGridTableOptionsSelection)
  private
    FRecordBorderSelection: Boolean;
    procedure SetRecordBorderSelection(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property HideFocusRect;
    property HideFocusRectOnExit;
    property HideSelection;
    property InvertSelect;
    property RecordBorderSelection: Boolean read FRecordBorderSelection write SetRecordBorderSelection default True;
    property UnselectFocusedRecordOnExit;
  end;

  { TcxGridLayoutViewOptionsBehavior }

  TcxGridLayoutViewOptionsBehavior = class(TcxCustomGridTableOptionsBehavior)
  private
    FExpandRecordOnDblClick: Boolean;
    FItemHotTrack: Boolean;
    procedure SetExpandRecordOnDblClick(const Value: Boolean);
    procedure SetItemHotTrack(const Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ExpandRecordOnDblClick: Boolean read FExpandRecordOnDblClick write SetExpandRecordOnDblClick default True;
    property ImmediateEditor;
    property ItemHotTrack: Boolean read FItemHotTrack write SetItemHotTrack default True;
    property RecordScrollMode;
  end;

  { TcxGridLayoutViewOptionsCustomize }

  TcxGridLayoutViewOptionsCustomize = class(TcxGridCustomLayoutViewOptionsCustomize)
  private
    function GetGridView: TcxGridLayoutView;
    function GetGroupExpanding: Boolean;
    function GetRecordExpanding: Boolean;
    procedure SetGroupExpanding(const Value: Boolean);
    procedure SetRecordExpanding(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridLayoutView read GetGridView;
  published
    property GroupExpanding: Boolean read GetGroupExpanding write SetGroupExpanding default False;
    property ItemFiltering;
    property RecordExpanding: Boolean read GetRecordExpanding write SetRecordExpanding default False;
  end;

  { TcxGridLayoutViewCarouselMode }

  TcxGridLayoutViewCarouselMode = class(TcxCustomGridOptions)
  strict private
    FAnimatedDelay: Cardinal;
    FAutoPitchAngle: Boolean;
    FBackgroundRecordAlphaLevel: Integer;
    FBackgroundRecordEndScale: Cardinal;
    FBackgroundRecordStartScale: Cardinal;
    FInterpolationMode: TdxGPInterpolationMode;
    FPitchAngle: Double;
    FRadius: Cardinal;
    FRecordCount: Integer;
    FRollAngle: Double;

    function GetDeltaAngle: Double;
    function GetGridView: TcxGridLayoutView;
    function IsPitchAngleStored: Boolean;
    function IsRollAngleStored: Boolean;
    procedure SetAutoPitchAngle(Value: Boolean);
    procedure SetBackgroundRecordAlphaLevel(Value: Integer);
    procedure SetBackgroundRecordEndScale(Value: Cardinal);
    procedure SetBackgroundRecordStartScale(const Value: Cardinal);
    procedure SetInterpolationMode(Value: TdxGPInterpolationMode);
    procedure SetPitchAngle(Value: Double);
    procedure SetRadius(Value: Cardinal);
    procedure SetRecordCount(Value: Integer);
    procedure SetRollAngle(Value: Double);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOptionsView: TcxGridLayoutViewOptionsView); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;

    property DeltaAngle: Double read GetDeltaAngle;
    property GridView: TcxGridLayoutView read GetGridView;
  published
    property AnimationInterval: Cardinal read FAnimatedDelay write FAnimatedDelay default cxGridLayoutViewCarouselModeDefaultAnimatedDelay;
    property AutoPitchAngle: Boolean read FAutoPitchAngle write SetAutoPitchAngle default False;
    property BackgroundRecordAlphaLevel: Integer read FBackgroundRecordAlphaLevel write SetBackgroundRecordAlphaLevel default 0;
    property BackgroundRecordEndScale: Cardinal read FBackgroundRecordEndScale write SetBackgroundRecordEndScale default cxGridLayoutViewCarouselModeDefaultBackgroundRecordEndScale;
    property BackgroundRecordStartScale: Cardinal read FBackgroundRecordStartScale write SetBackgroundRecordStartScale default cxGridLayoutViewCarouselModeDefaultBackgroundRecordStartScale;
    property InterpolationMode: TdxGPInterpolationMode read FInterpolationMode write SetInterpolationMode default imDefault;
    property PitchAngle: Double read FPitchAngle write SetPitchAngle stored IsPitchAngleStored;
    property Radius: Cardinal read FRadius write SetRadius default 0;
    property RecordCount: Integer read FRecordCount write SetRecordCount default cxGridLayoutViewCarouselModeDefaultRecordCount;
    property RollAngle: Double read FRollAngle write SetRollAngle stored IsRollAngleStored;
  end;

  TcxGridLayoutViewCarouselModeClass = class of TcxGridLayoutViewCarouselMode;

  { TcxGridLayoutViewRecordCaption }

  TcxGridLayoutViewRecordCaption = class(TcxCustomGridOptions)
  strict private
    FDisplayMask: string;
    FExpandButtonAlignment: TcxGridLayoutViewRecordExpandButtonAlignment;
    FFormatString: string;
    FHeight: Integer;
    FIsNeedRecordCount: Boolean;
    FIsNeedRecordIndex: Boolean;
    FTextAlignmentHorz: TAlignment;
    FTextAlignmentVert: TcxAlignmentVert;
    FVisible: Boolean;

    function IsDisplayMaskStored: Boolean;
    procedure SetExpandButtonAlignment(Value: TcxGridLayoutViewRecordExpandButtonAlignment);
    procedure SetHeight(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetTextAlignmentHorz(Value: TAlignment);
    procedure SetTextAlignmentVert(Value: TcxAlignmentVert);
    procedure SetDisplayMask(const Value: string);
  protected
    procedure BuildFormatString;
    procedure ChangeScale(M: Integer; D: Integer); override;

    property IsNeedRecordIndex: Boolean read FIsNeedRecordIndex;
    property IsNeedRecordCount: Boolean read FIsNeedRecordCount;
  public
    constructor Create(AOptionsView: TcxGridLayoutViewOptionsView); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    function GetFormattedText(AGridRecord: TcxGridLayoutViewRecord): string; virtual;
  published
    property DisplayMask: string read FDisplayMask write SetDisplayMask stored IsDisplayMaskStored;
    property ExpandButtonAlignment: TcxGridLayoutViewRecordExpandButtonAlignment
      read FExpandButtonAlignment write SetExpandButtonAlignment default rebaRight;
    property Height: Integer read FHeight write SetHeight default 0;
    property TextAlignmentHorz: TAlignment read FTextAlignmentHorz write SetTextAlignmentHorz default taLeftJustify;
    property TextAlignmentVert: TcxAlignmentVert read FTextAlignmentVert write SetTextAlignmentVert default vaCenter;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TcxGridLayoutViewRecordHeaderClass = class of TcxGridLayoutViewRecordCaption;

  { TcxGridLayoutViewOptionsView }

  TcxGridLayoutViewViewMode = (lvvmSingleRecord, lvvmSingleRow, lvvmMultiRow,
    lvvmSingleColumn, lvvmMultiColumn, lvvmCarousel);

  TcxGridLayoutViewSingleRecordStretch = (srsNone, srsHorizontal, srsVertical, srsClient);

  TcxGridLayoutViewOptionsView = class(TcxGridCustomLayoutViewOptionsView)
  private
    FCarouselMode: TcxGridLayoutViewCarouselMode;
    FCenterRecords: Boolean;
    FMaxColumnCount: Integer;
    FMaxRowCount: Integer;
    FMinValueWidth: Integer;
    FRecordBorderWidth: Integer;
    FRecordCaption: TcxGridLayoutViewRecordCaption;
    FRecordSize: TcxSize;
    FShowOnlyEntireRecords: Boolean;
    FSingleRecordStretch: TcxGridLayoutViewSingleRecordStretch;
    FViewMode: TcxGridLayoutViewViewMode;

    function GetGridView: TcxGridLayoutView;
    function GetItemPadding: TcxLayoutViewLookAndFeelItemPadding;
    function GetRecordIndent: Integer;
    procedure SetCarouselMode(Value: TcxGridLayoutViewCarouselMode);
    procedure SetCenterRecords(Value: Boolean);
    procedure SetItemPadding(const Value: TcxLayoutViewLookAndFeelItemPadding);
    procedure SetMaxColumnCount(Value: Integer);
    procedure SetMaxRowCount(Value: Integer);
    procedure SetMinValueWidth(Value: Integer);
    procedure SetRecordBorderWidth(Value: Integer);
    procedure SetRecordCaption(Value: TcxGridLayoutViewRecordCaption);
    procedure SetRecordIndent(Value: Integer);
    procedure SetRecordSize(AValue: TcxSize);
    procedure SetShowOnlyEntireRecords(Value: Boolean);
    procedure SetSingleRecordStretch(Value: TcxGridLayoutViewSingleRecordStretch);
    procedure SetViewMode(Value: TcxGridLayoutViewViewMode);

    procedure CreateRecordSize;
    procedure DestroyRecordSize;
    procedure RecordSizeChangeHandler(Sender: TObject);
  protected
    function CanRecordScrollingHorizontal: Boolean; virtual;
    function CanRecordScrollingVertical: Boolean; virtual;
    procedure ChangeScale(M, D: Integer); override;
    function GetCarouselModeClass: TcxGridLayoutViewCarouselModeClass; virtual;
    function GetRecordCaptionClass: TcxGridLayoutViewRecordHeaderClass; virtual;
    function NeedStretchRecordHeight: Boolean;
    function NeedStretchRecordWidth: Boolean;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridLayoutView read GetGridView;
  published
    property CarouselMode: TcxGridLayoutViewCarouselMode read FCarouselMode write SetCarouselMode;
    property CenterRecords: Boolean read FCenterRecords write SetCenterRecords default True;
    property ItemPadding: TcxLayoutViewLookAndFeelItemPadding read GetItemPadding write SetItemPadding;
    property MaxColumnCount: Integer read FMaxColumnCount write SetMaxColumnCount default 0;
    property MaxRowCount: Integer read FMaxRowCount write SetMaxRowCount default 0;
    property MinValueWidth: Integer read FMinValueWidth write SetMinValueWidth default cxGridLayoutViewDefaultMinValueWidth;
    property RecordBorderWidth: Integer read FRecordBorderWidth write SetRecordBorderWidth default cxGridLayoutViewDefaultRecordBorderWidth;
    property RecordCaption: TcxGridLayoutViewRecordCaption read FRecordCaption write SetRecordCaption;
    property RecordIndent: Integer read GetRecordIndent write SetRecordIndent default cxGridCustomLayoutViewRecordDefaultIndent;
    property RecordSize: TcxSize read FRecordSize write SetRecordSize;
    property SeparatorColor;
    property SeparatorWidth;
    property ShowItemFilterButtons;
    property ShowOnlyEntireRecords: Boolean read FShowOnlyEntireRecords write SetShowOnlyEntireRecords default True;
    property SingleRecordStretch: TcxGridLayoutViewSingleRecordStretch read FSingleRecordStretch write SetSingleRecordStretch default srsNone;
    property ViewMode: TcxGridLayoutViewViewMode read FViewMode write SetViewMode default lvvmSingleRecord;
  end;

  { TcxGridLayoutViewStyles }

  TcxGridLayoutViewStyles = class(TcxCustomGridTableViewStyles)
  private
    FIsRecordCaptionParams: Boolean;
    FOnGetGroupStyle: TcxGridGetCellStyleEvent;
    FOnGetItemStyle: TcxGridGetCellStyleEvent;
    FOnGetRecordBorderStyle: TcxGridGetRecordStyleEvent;
    FOnGetRecordCaptionStyle: TcxGridGetRecordStyleEvent;
    function GetGridViewValue: TcxGridLayoutView;
    procedure SetOnGetItemStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetRecordBorderStyle(Value: TcxGridGetRecordStyleEvent);
    procedure SetOnGetRecordCaptionStyle(Value: TcxGridGetRecordStyleEvent);
  protected
    function GetBackgroundBitmapIndex(Index: Integer): Integer; virtual;
    function GetDefaultBitmap(Index: Integer): TBitmap; virtual;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetItemHottrackParams(AItem: TcxCustomGridTableItem; out AParams: TcxViewParams); virtual;
    procedure GetRecordBorderParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetRecordBorderVisualParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    property GridView: TcxGridLayoutView read GetGridViewValue;
    property IsRecordCaptionParams: Boolean read FIsRecordCaptionParams;
  published
    property Group: TcxStyle index vsGroup read GetValue write SetValue;
    property Inactive;
    property Item: TcxStyle index vsItem read GetValue write SetValue;
    property ItemHottrack: TcxStyle index vsItemHottrack read GetValue write SetValue;
    property RecordBorder: TcxStyle index vsRecordBorder read GetValue write SetValue;
    property RecordCaption: TcxStyle index vsRecordCaption read GetValue write SetValue;
    property Selection;
    property StyleSheet;
    property OnGetGroupStyle: TcxGridGetCellStyleEvent read FOnGetGroupStyle write FOnGetGroupStyle;
    property OnGetItemStyle: TcxGridGetCellStyleEvent read FOnGetItemStyle write SetOnGetItemStyle;
    property OnGetRecordBorderStyle: TcxGridGetRecordStyleEvent read FOnGetRecordBorderStyle write SetOnGetRecordBorderStyle;
    property OnGetRecordCaptionStyle: TcxGridGetRecordStyleEvent read FOnGetRecordCaptionStyle write SetOnGetRecordCaptionStyle;
  end;

  { TcxGridLayoutViewStyleSheet }

  TcxGridLayoutViewStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxGridLayoutViewStyles;
    procedure SetStylesValue(Value: TcxGridLayoutViewStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxGridLayoutViewStyles read GetStylesValue write SetStylesValue;
  end;

  TcxGridLayoutViewCustomDrawRecordHeaderEvent = procedure(Sender: TcxGridLayoutView; ACanvas: TcxCanvas;
    AViewInfo: TcxGridLayoutViewRecordCaptionViewInfo; var ADone: Boolean) of object;

  { IcxGridLayoutViewStylesHelper }

  IcxGridLayoutViewStylesHelper = interface
  ['{79CDADCA-DDC3-439F-9881-3406B13399EA}']
    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
  end;

  { TcxGridLayoutViewStylesAdapter }

  TcxGridLayoutViewStylesAdapter = class(TPersistent)
  private
    FGridView: TcxGridLayoutView;
    FHelper: IcxGridLayoutViewStylesHelper;
    function GetStyles: TcxGridLayoutViewStyles;
  public
    constructor Create(AGridView: TcxGridLayoutView);

    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);

    property Styles: TcxGridLayoutViewStyles read GetStyles;
    property GridView: TcxGridLayoutView read FGridView;
    property Helper: IcxGridLayoutViewStylesHelper read FHelper write FHelper;
  end;

  { TcxGridLayoutView }

  TcxGridLayoutView = class(TcxGridCustomLayoutView,
    IdxLayoutContainerOwner)
  strict private
    FContainer: TcxGridLayoutContainer;
    FDefaultCloneData: TMemoryStream;
    FDefaultLayoutViewInfo: TcxGridLayoutContainerCloneViewInfo;
    FIsDefaultViewInfoCalculated: Boolean;
    FLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
    FStylesAdapter: TcxGridLayoutViewStylesAdapter;

    FOnCustomDrawRecordCaption: TcxGridLayoutViewCustomDrawRecordHeaderEvent;

    function GetControl: TcxCustomGrid; inline;
    function GetController: TcxGridLayoutViewController; inline;
    function GetDataController: TcxGridDataController; inline;
    function GetDateTimeHandling: TcxGridLayoutViewDateTimeHandling; inline;
    function GetFiltering: TcxGridLayoutViewFiltering; inline;
    function GetItem(Index: Integer): TcxGridLayoutViewItem; inline;
    function GetOptionsBehavior: TcxGridLayoutViewOptionsBehavior; inline;
    function GetOptionsCustomize: TcxGridLayoutViewOptionsCustomize; inline;
    function GetOptionsData: TcxGridLayoutViewOptionsData; inline;
    function GetOptionsSelection: TcxGridLayoutViewOptionsSelection; inline;
    function GetOptionsView: TcxGridLayoutViewOptionsView; inline;
    function GetRoot: TdxLayoutGroup;
    function GetStyles: TcxGridLayoutViewStyles; inline;
    function GetViewInfo: TcxGridLayoutViewViewInfo; inline;
    function GetVisibleItem(Index: Integer): TcxGridLayoutViewItem; inline;
    procedure LayoutChangedHandler(Sender: TObject);
    procedure SetDataController(Value: TcxGridDataController);
    procedure SetDateTimeHandling(Value: TcxGridLayoutViewDateTimeHandling); inline;
    procedure SetFiltering(Value: TcxGridLayoutViewFiltering); inline;
    procedure SetIsDefaultViewInfoCalculated(AValue: Boolean);
    procedure SetItem(Index: Integer; Value: TcxGridLayoutViewItem); inline;
    procedure SetOptionsBehavior(Value: TcxGridLayoutViewOptionsBehavior); inline;
    procedure SetOptionsCustomize(Value: TcxGridLayoutViewOptionsCustomize); inline;
    procedure SetOptionsData(Value: TcxGridLayoutViewOptionsData); inline;
    procedure SetOptionsSelection(Value: TcxGridLayoutViewOptionsSelection); inline;
    procedure SetOptionsView(Value: TcxGridLayoutViewOptionsView); inline;
    procedure SetStyles(Value: TcxGridLayoutViewStyles); inline;
    procedure SetOnCustomDrawRecordCaption(Value: TcxGridLayoutViewCustomDrawRecordHeaderEvent);
  protected
    procedure AfterRestoring; override;
    procedure BeforeRestoring; override;
    function HandleSetStoredPropertyValueError(Sender: TcxStorage; const AName: string; const AValue: Variant): Boolean; override;

    procedure BiDiModeChanged; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure DoStylesChanged; override;
    procedure GetStoredChildren(AChildren: TStringList); override;
    procedure LookAndFeelChanged; override;
    procedure Init; override;
    function IsRecordPixelScrolling: Boolean; override;
    function ShowGridViewEditor: Boolean; override;

    procedure AssignDefaultBounds(AViewInfo: TcxGridLayoutContainerCloneViewInfo);
    procedure RecalculateCloneData;
    procedure ResetCalculatedLayoutViewInfo;
    procedure ResetCloneData;
    procedure UpdateContainerStyles(ARecord: TcxCustomGridRecord; var ALayoutLookAndFeel: TcxGridLayoutLookAndFeel);

    //interfaces
    // IdxLayoutContainerOwner
    function IdxLayoutContainerOwner.GetContainer = GetLayoutContainer;
    function GetLayoutContainer: TdxLayoutContainer;
    //IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    function GetLayoutCustomizationFormButtonCaption: string; override;

    function GetCanvas: TcxCanvas;
    function GetClientBounds: TRect;
    function GetClientRect: TRect;
    function GetContainer: TdxLayoutContainer;
    function GetDefaultFont: TFont;

    function GetHighlightRoot: Boolean;

    procedure CreateLayoutContainer;
    procedure DestroyLayoutContainer;
    procedure RecreateLayoutContainer;

    function CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
      ACellViewInfo: TcxGridTableCellViewInfo): Boolean; override;
    //function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; override;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass; override;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    procedure SetControl(Value: TcxControl); override;
    procedure SetName(const NewName: TComponentName); override;

    procedure CopyLayoutStructure(AContainer: TdxLayoutContainer);
    function CreateLayoutLookAndFeel: TcxGridLayoutLookAndFeel; virtual;
    function GetLayoutContainerClass: TcxGridLayoutContainerClass; virtual;
    function GetLayoutGroupClass: TdxLayoutGroupClass; virtual;
    procedure UpdateSizeDefaultViewInfo;

    procedure DoCustomDrawRecordHeader(ACanvas: TcxCanvas; AViewInfo: TcxGridLayoutViewRecordCaptionViewInfo; var ADone: Boolean); virtual;
    function HasCustomDrawRecordHeader: Boolean;
    function DrawRecordBorderSelected(ARecord: TcxCustomGridRecord): Boolean; virtual;

    property DefaultCloneData: TMemoryStream read FDefaultCloneData;
    property DefaultLayoutViewInfo: TcxGridLayoutContainerCloneViewInfo read FDefaultLayoutViewInfo;
    property IsDefaultViewInfoCalculated: Boolean read FIsDefaultViewInfoCalculated write SetIsDefaultViewInfoCalculated;
    property Root: TdxLayoutGroup read GetRoot;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function CreateItem: TcxGridLayoutViewItem;
    function SizeChanged(AUpdateSelfOnly: Boolean = False; AKeepMaster: Boolean = False): Boolean; override;

    property Container: TcxGridLayoutContainer read FContainer;
    property Control: TcxCustomGrid read GetControl;
    property Controller: TcxGridLayoutViewController read GetController;
    property LayoutLookAndFeel: TcxGridLayoutLookAndFeel read FLayoutLookAndFeel;
    property StylesAdapter: TcxGridLayoutViewStylesAdapter read FStylesAdapter;
    property Items[Index: Integer]: TcxGridLayoutViewItem read GetItem write SetItem;
    property ViewInfo: TcxGridLayoutViewViewInfo read GetViewInfo;
    property VisibleItems[Index: Integer]: TcxGridLayoutViewItem read GetVisibleItem;
  published
    property DataController: TcxGridDataController read GetDataController write SetDataController;
    property DateTimeHandling: TcxGridLayoutViewDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property Filtering: TcxGridLayoutViewFiltering read GetFiltering write SetFiltering;
    property Images;
    property OptionsBehavior: TcxGridLayoutViewOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxGridLayoutViewOptionsCustomize read GetOptionsCustomize write SetOptionsCustomize;
    property OptionsData: TcxGridLayoutViewOptionsData read GetOptionsData write SetOptionsData;
    property OptionsSelection: TcxGridLayoutViewOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxGridLayoutViewOptionsView read GetOptionsView write SetOptionsView;
    property Styles: TcxGridLayoutViewStyles read GetStyles write SetStyles;

    property OnCustomization;
    property OnCustomDrawRecordCaption: TcxGridLayoutViewCustomDrawRecordHeaderEvent read
      FOnCustomDrawRecordCaption write SetOnCustomDrawRecordCaption;
  end;

implementation

uses
  Math, SysUtils, Types, StrUtils, dxGDIPlusApi, cxContainer, cxGridStrs,
  cxGridLayoutViewCarouselMode, cxGridLayoutViewCustomizationForm, Dialogs, dxDPIAwareUtils;

type
  TcxControlAccess = class(TcxControl);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxLayoutGroupViewInfoAccess = class(TdxLayoutGroupViewInfo);
  TdxLayoutLookAndFeelGroupOptionsAccess = class(TdxLayoutLookAndFeelGroupOptions);
  TcxLayoutViewLookAndFeelItemPaddingAccess = class(TcxLayoutViewLookAndFeelItemPadding);

const
  cxGridLayoutViewCarouselModeDefaultPitchAngle = 90.0;
  cxGridLayoutViewCarouselModeDefaultRollAngle = 180.0;

///////////////////////////////// Layout classes ////////////////////////////////////////

{ TcxGridLayoutItemEditPainter }

procedure TcxGridLayoutItemEditPainter.DrawEdit(ACanvas: TcxCanvas);
begin
end;

{ TcxGridLayoutItemPainter }

function TcxGridLayoutItemPainter.GetControlPainterClass: TdxLayoutControlItemControlPainterClass;
begin
  Result := TcxGridLayoutItemEditPainter;
end;

{ TcxGridBaseLayoutItemEditViewInfo }

function TcxGridBaseLayoutItemEditViewInfo.GetDefaultValueHeight: Integer;
var
  AParams: TcxViewParams;
begin
  GridView.StylesAdapter.GetContentParams(GetRecord, Item.GridViewItem, AParams);
  Result := Item.GridViewItem.CalculateDefaultCellHeight(cxScreenCanvas, AParams.Font);
end;

function TcxGridBaseLayoutItemEditViewInfo.GetRecord: TcxGridLayoutViewRecord;
begin
  Result := nil;
end;

function TcxGridBaseLayoutItemEditViewInfo.GetMinValueWidth: Integer;
begin
  Result := GridView.OptionsView.MinValueWidth;
end;

function TcxGridBaseLayoutItemEditViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridBaseLayoutItemEditViewInfo.GetItem: TcxGridLayoutItem;
begin
  Result := TcxGridLayoutItem(inherited Item);
end;

{ TcxGridLayoutItemEditViewInfo }

function TcxGridLayoutItemEditViewInfo.GetRecord: TcxGridLayoutViewRecord;
begin
  if DataViewInfo <> nil then
    Result := DataViewInfo.RecordViewInfo.GridRecord
  else
    Result := inherited GetRecord;
end;

function TcxGridLayoutItemEditViewInfo.GetDataViewInfo: TcxGridLayoutViewItemViewInfo;
begin
  Result := TcxGridLayoutViewItemViewInfo(inherited DataViewInfo);
end;

{ TcxGridBaseLayoutItemCaptionViewInfo }

function TcxGridBaseLayoutItemCaptionViewInfo.CalculatePadding: TRect;
begin
  Result := inherited CalculatePadding;
  if ItemViewInfo.GridItemViewInfo <> nil then
    Inc(Result.Right, ItemViewInfo.GridItemViewInfo.GetExtraCaptionPadding);
end;

function TcxGridBaseLayoutItemCaptionViewInfo.GetItem: TcxGridBaseLayoutItem;
begin
  Result := TcxGridBaseLayoutItem(inherited Item);
end;

function TcxGridBaseLayoutItemCaptionViewInfo.GetItemViewInfo: TcxGridBaseLayoutItemViewInfo;
begin
  Result := TcxGridBaseLayoutItemViewInfo(inherited ItemViewInfo);
end;

{ TcxGridLayoutItemCaptionViewInfo }

function TcxGridLayoutItemCaptionViewInfo.GetItem: TcxGridLayoutItem;
begin
  Result := TcxGridLayoutItem(inherited Item);
end;

function TcxGridLayoutItemCaptionViewInfo.GetItemViewInfo: TcxGridLayoutItemViewInfo;
begin
  Result := TcxGridLayoutItemViewInfo(inherited ItemViewInfo);
end;

{ TcxGridBaseLayoutItemViewInfo }

function TcxGridBaseLayoutItemViewInfo.GetActuallyVisible: Boolean;
begin
  Result := Assigned(Item) and
    Assigned(Item.GridViewItem) and
    inherited GetActuallyVisible;
end;

function TcxGridBaseLayoutItemViewInfo.GetCaptionViewInfoClass: TdxCustomLayoutItemCaptionViewInfoClass;
begin
  Result := TcxGridBaseLayoutItemCaptionViewInfo;
end;

function TcxGridBaseLayoutItemViewInfo.GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass;
begin
  Result := TcxGridBaseLayoutItemEditViewInfo;
end;

function TcxGridBaseLayoutItemViewInfo.GetGridItemViewInfo: TcxGridLayoutViewItemViewInfo;
begin
  Result := TcxGridLayoutViewItemViewInfo(inherited GridItemViewInfo);
end;

function TcxGridBaseLayoutItemViewInfo.GetItem: TcxGridBaseLayoutItem;
begin
  Result := TcxGridBaseLayoutItem(inherited Item);
end;

{ TcxGridLayoutItemViewInfo }

function TcxGridLayoutItemViewInfo.GetCaptionViewInfoClass: TdxCustomLayoutItemCaptionViewInfoClass;
begin
  Result := TcxGridLayoutItemCaptionViewInfo;
end;

function TcxGridLayoutItemViewInfo.GetCurrentGridItemViewInfo: TcxGridTableDataCellViewInfo;
begin
  Result := ContainerViewInfo.GetGridItemViewInfo(Self);
end;

function TcxGridLayoutItemViewInfo.GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass;
begin
  Result := TcxGridLayoutItemEditViewInfo;
end;

function TcxGridLayoutItemViewInfo.GetPainterClass: TdxCustomLayoutItemPainterClass;
begin
  Result := TcxGridLayoutItemPainter;
end;

function TcxGridLayoutItemViewInfo.GetContainerViewInfo: TcxGridLayoutContainerViewInfo;
begin
  Result := TcxGridLayoutContainerViewInfo(inherited ContainerViewInfo);
end;

function TcxGridLayoutItemViewInfo.GetControlViewInfo: TcxGridLayoutItemEditViewInfo;
begin
  Result := TcxGridLayoutItemEditViewInfo(inherited ControlViewInfo);
end;

function TcxGridLayoutItemViewInfo.GetItem: TcxGridLayoutItem;
begin
  Result := TcxGridLayoutItem(inherited Item);
end;

{ TcxGridLayoutItemCaptionOptions }

function TcxGridLayoutItemCaptionOptions.GetItem: TcxGridLayoutItem;
begin
  Result := TcxGridLayoutItem(inherited Item);
end;

{ TcxGridBaseLayoutItem }

procedure TcxGridBaseLayoutItem.BeforeDestruction;
begin
  if not Container.IsDestroying and (GridViewItem <> nil) and GridViewItem.GridView.IsPattern and
    not GridViewItem.GridView.IsAssigningItems and
    ([csDesigning, csDestroying] * GridViewItem.ComponentState = [csDesigning])
  then
    raise Exception.Create('Can''t delete internal component');

  inherited BeforeDestruction;
end;

function TcxGridBaseLayoutItem.IsVisibleForCustomization: Boolean;
begin
  Result := inherited IsVisibleForCustomization and GridViewItem.VisibleForCustomization
end;

function TcxGridBaseLayoutItem.CaptionToDisplayCaption(AValue: string): string;
begin
  Result := GridViewItem.CaptionToDisplayCaption(AValue);
end;

function TcxGridBaseLayoutItem.DisplayCaptionToCaption(AValue: string): string;
begin
  Result := GridViewItem.DisplayCaptionToCaption(AValue);
end;

function TcxGridBaseLayoutItem.GetViewInfoClass: TdxCustomLayoutItemViewInfoClass;
begin
  Result := TcxGridBaseLayoutItemViewInfo;
end;

function TcxGridBaseLayoutItem.GetGridViewItem: TcxGridLayoutViewItem;
begin
  Result := TcxGridLayoutViewItem(inherited GridViewItem);
end;

function TcxGridBaseLayoutItem.GetViewInfo: TcxGridBaseLayoutItemViewInfo;
begin
  Result := TcxGridLayoutItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridBaseLayoutItem.SetGridViewItem(Value: TcxGridLayoutViewItem);
begin
  inherited GridViewItem := Value;
end;

{ TcxGridLayoutItem }

function TcxGridLayoutItem.GetCaptionOptions: TcxGridLayoutItemCaptionOptions;
begin
  Result := TcxGridLayoutItemCaptionOptions(inherited CaptionOptions);
end;

class function TcxGridLayoutItem.GetCaptionOptionsClass: TdxCustomLayoutItemCaptionOptionsClass;
begin
  Result := TcxGridLayoutItemCaptionOptions;
end;

function TcxGridLayoutItem.GetContainer: TcxGridLayoutContainer;
begin
  Result := (inherited Container) as TcxGridLayoutContainer;
end;

function TcxGridLayoutItem.GetViewInfoClass: TdxCustomLayoutItemViewInfoClass;
begin
  Result := TcxGridLayoutItemViewInfo;
end;

procedure TcxGridLayoutItem.SetCaptionOptions(Value: TcxGridLayoutItemCaptionOptions);
begin
  inherited CaptionOptions := Value;
end;

function TcxGridLayoutItem.GetViewInfo: TcxGridLayoutItemViewInfo;
begin
  Result := TcxGridLayoutItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridLayoutItem.SetContainer(Value: TcxGridLayoutContainer);
begin
  inherited Container := Value;
end;

{ TcxGridLayoutLookAndFeel }

constructor TcxGridLayoutLookAndFeel.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView);
  Offsets.ControlOffsetHorz := 2;
  Offsets.ControlOffsetVert := 2;
  Offsets.ItemOffset := 0;
  Offsets.RootItemsAreaOffsetVert := 4;
  Offsets.RootItemsAreaOffsetHorz := 4;

  GroupOptions.Padding.Left := 0;
  GroupOptions.Padding.Top := 0;
  GroupOptions.Padding.Right := 0;
  GroupOptions.Padding.Bottom := 0;
end;

function TcxGridLayoutLookAndFeel.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

{ TcxGridLayoutContainerCloneViewInfo }

function TcxGridLayoutContainerCloneViewInfo.GetItemViewInfo(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemViewInfo;
begin
  Result := ItemsViewInfo.FindItemViewInfo(AItem);
end;

function TcxGridLayoutContainerCloneViewInfo.CanAssignBounds(ASource: TdxLayoutContainerViewInfo): Boolean;
begin
  Result := Container = ASource.Container;
end;

function TcxGridLayoutContainerCloneViewInfo.CanShowGroupScrollBars: Boolean;
begin
  Result := HasRecordViewInfo and RecordViewInfo.CanShowGroupScrollBars;;
end;

procedure TcxGridLayoutContainerCloneViewInfo.DestroyViewInfos;
begin
  if HasRecordViewInfo then
    RecordViewInfo.ClearItemLinks;
  inherited DestroyViewInfos;
end;

function TcxGridLayoutContainerCloneViewInfo.FindGridItemViewInfo(
  AViewInfo: TcxGridCustomLayoutItemViewInfo): TcxGridTableDataCellViewInfo;
var
  I: Integer;
  ALayoutItem: TdxLayoutControlItem;
begin
  Result := nil;
  ALayoutItem := GetLayoutItem(AViewInfo);
  for I := 0 to RecordViewInfo.ItemViewInfoCount - 1 do
    if ALayoutItem = RecordViewInfo.ItemViewInfos[I].Item.LayoutItem then
    begin
      Result := RecordViewInfo.ItemViewInfos[I];
      Break;
    end;
end;

function TcxGridLayoutContainerCloneViewInfo.GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel;
begin
  if HasRecordViewInfo then
    Result := RecordViewInfo.LayoutLookAndFeel
  else
    Result := inherited GetLayoutLookAndFeel;
end;

function TcxGridLayoutContainerCloneViewInfo.IsActuallyVisible: Boolean;
begin
  Result := inherited IsActuallyVisible and HasRecordViewInfo and RecordViewInfo.Visible;
end;

function TcxGridLayoutContainerCloneViewInfo.GetRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited RecordViewInfo);
end;

{ TcxGridLayoutContainerFocusController }

function TcxGridLayoutContainerFocusController.IsFocused(AItem: TdxCustomLayoutItem): Boolean;
begin
  Result := False;
end;

{ TcxGridLayoutContainer }

procedure TcxGridLayoutContainer.AfterRestoring;
var
  I: Integer;
  AItem: TdxCustomLayoutItem;
  AGridViewItem: TcxGridLayoutViewItem;
  AGridLayoutItem: TcxGridLayoutItem;
begin
  BeginUpdate;
  try
    for I := AbsoluteItemCount - 1 downto 0 do
    begin
      AItem := AbsoluteItems[I];
      if AItem is TcxGridLayoutItem then
      begin
        AGridLayoutItem := TcxGridLayoutItem(AItem);
        AGridViewItem := TcxGridLayoutViewItem(GridView.FindItemByObjectName(AGridLayoutItem.LoadedGridViewItemName));
        if AGridViewItem <> nil then
          AGridViewItem.LayoutItem := AGridLayoutItem
        else
          if AGridLayoutItem.GridViewItem = nil then
            AItem.Free;
      end;
    end;
    inherited AfterRestoring;
  finally
    EndUpdate;
  end;
end;

function TcxGridLayoutContainer.CanRootScrollingHorizontal: Boolean;
begin
  Result := GridView.OptionsView.CanRecordScrollingHorizontal;
end;

function TcxGridLayoutContainer.CanRootScrollingVertical: Boolean;
begin
  Result := GridView.OptionsView.CanRecordScrollingVertical;
end;

procedure TcxGridLayoutContainer.DoCalculateRoot(ANeedPack: Boolean);
begin
  inherited DoCalculateRoot(ANeedPack);
  GridView.IsDefaultViewInfoCalculated := False;
  GridView.ResetCalculatedLayoutViewInfo;
  if ANeedPack then
    GridView.RecalculateCloneData;
  GridView.Changed(vcSize);
end;

procedure TcxGridLayoutContainer.DoGroupScroll(Sender: TObject);
begin
  inherited DoGroupScroll(Sender);
  GridView.ValidateEditVisibility;
end;

procedure TcxGridLayoutContainer.DoInitialize;
begin
  inherited DoInitialize;
  MenuItems := MenuItems - [cfmiExpandButton];
end;

procedure TcxGridLayoutContainer.DoUpdateRootOptions;
const
  ScrollMode: array[Boolean] of TdxLayoutGroupScrollMode = (smNone, smAuto);
begin
  inherited DoUpdateRootOptions;
  Root.Height := GridView.OptionsView.RecordSize.Height;
  Root.Width := GridView.OptionsView.RecordSize.Width;
  Root.ScrollOptions.Horizontal := ScrollMode[CanRootScrollingHorizontal];
  Root.ScrollOptions.Vertical := ScrollMode[CanRootScrollingVertical];
end;

function TcxGridLayoutContainer.GetFocusControllerClass: TdxLayoutContainerFocusControllerClass;
begin
  Result := TcxGridLayoutContainerFocusController;
end;

function TcxGridLayoutContainer.GetViewInfo: TcxGridLayoutContainerViewInfo;
begin
  Result := TcxGridLayoutContainerViewInfo(inherited ViewInfo);
end;

function TcxGridLayoutContainer.GetViewInfoClass: TdxLayoutContainerViewInfoClass;
begin
  Result := TcxGridLayoutContainerViewInfo;
end;

function TcxGridLayoutContainer.GetGridLayoutView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutContainer.IsCustomization: Boolean;
begin
  Result := Customization;
end;

function TcxGridLayoutContainer.GetClientBounds: TRect;
begin
  Result := GridView.GetClientBounds;
end;

function TcxGridLayoutContainer.GetClientRect: TRect;
begin
  Result := GridView.GetClientRect;
end;

function TcxGridLayoutContainer.GetDefaultFont: TFont;
begin
  Result := GridView.GetDefaultFont;
end;

function TcxGridLayoutContainer.NeedStretchRecordHeight: Boolean;
begin
  Result := GridView.OptionsView.NeedStretchRecordHeight;
end;

function TcxGridLayoutContainer.NeedStretchRecordWidth: Boolean;
begin
  Result := GridView.OptionsView.NeedStretchRecordWidth;
end;

///////////////////////////////// Grid classes ////////////////////////////////////////

{ TcxGridLayoutViewItemFilterButtonHitTest }

class function TcxGridLayoutViewItemFilterButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htLayoutViewItemFilterButton;
end;

class function TcxGridLayoutViewItemFilterButtonHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridLayoutViewItemCaptionHitTest }

class function TcxGridLayoutViewItemHitTest.GetHitTestCode: Integer;
begin
  Result := htLayoutViewItem;
end;

{ TcxGridLayoutViewRecordCaptionHitTest }

class function TcxGridLayoutViewRecordCaptionHitTest.GetHitTestCode: Integer;
begin
  Result := htLayoutViewRecordCaption;
end;

{ TcxGridLayoutViewItemFilterButtonPainter }

procedure TcxGridLayoutViewItemFilterButtonPainter.Paint;
begin
  with ViewInfo do
    GridViewInfo.LookAndFeelPainter.DrawScaledFilterDropDownButton(
      Self.Canvas, Bounds, ButtonState, Active, ScaleFactor);
end;

function TcxGridLayoutViewItemFilterButtonPainter.GetViewInfo: TcxGridLayoutViewItemFilterButtonViewInfo;
begin
  Result := TcxGridLayoutViewItemFilterButtonViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewItemPainter }

procedure TcxGridLayoutViewItemPainter.DrawContent;
begin
  inherited DrawContent;
  if ViewInfo.HasFilterButton then
    ViewInfo.FilterButtonViewInfo.Paint(Canvas);
end;

function TcxGridLayoutViewItemPainter.GetViewInfo: TcxGridLayoutViewItemViewInfo;
begin
  Result := TcxGridLayoutViewItemViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewRecordExpandButtonPainter }

function TcxGridLayoutViewRecordExpandButtonPainter.GetViewInfo: TcxGridLayoutViewRecordExpandButtonViewInfo;
begin
  Result := TcxGridLayoutViewRecordExpandButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridLayoutViewRecordExpandButtonPainter.Paint;
begin
  with ViewInfo do
    LookAndFeelPainter.LayoutViewDrawScaledRecordExpandButton(
      Self.Canvas, Bounds, ButtonState, RecordViewInfo.IsExpanded, ScaleFactor);
end;

{ TcxGridLayoutViewRecordCaptionPainter }

procedure TcxGridLayoutViewRecordCaptionPainter.DrawBackground;
begin
  ViewInfo.LookAndFeelPainter.LayoutViewDrawRecordCaption(Canvas, ViewInfo.Bounds, ViewInfo.TextBounds,
    ViewInfo.RecordViewInfo.RecordCaptionPosition, GetState, ViewInfo.Params.Color, ViewInfo.Params.Bitmap);
end;

function TcxGridLayoutViewRecordCaptionPainter.GetState: TcxButtonState;
begin
  with ViewInfo.RecordViewInfo do
    if not Selected then
      Result := cxbsNormal
    else
      if GridView.IsControlFocused then
        Result := cxbsHot
      else
        Result := cxbsDisabled;
end;

function TcxGridLayoutViewRecordCaptionPainter.GetViewInfo: TcxGridLayoutViewRecordCaptionViewInfo;
begin
  Result := TcxGridLayoutViewRecordCaptionViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewRecordPainter }

procedure TcxGridLayoutViewRecordPainter.AfterPaint;
begin
  inherited AfterPaint;
  Canvas.SetClipRegion(FClipRegion, roSet);
end;

procedure TcxGridLayoutViewRecordPainter.BeforePaint;
begin
  FClipRegion := Canvas.GetClipRegion;
  Canvas.IntersectClipRect(ViewInfo.Bounds);
  inherited BeforePaint;
end;

function TcxGridLayoutViewRecordPainter.CanDrawBackground: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewRecordPainter.CanDrawExpandButton: Boolean;
begin
  Result := True;
end;

procedure TcxGridLayoutViewRecordPainter.DrawBackground;
begin
  if CanDrawBackground then
    with ViewInfo do
      LookAndFeelPainter.LayoutViewDrawRecordContent(Self.Canvas, LayoutAreaBounds,
        RecordCaptionPosition, GetState, LayoutAreaBorders);
end;

procedure TcxGridLayoutViewRecordPainter.DrawRecordBorder;
var
  ABounds: TRect;
  AParams: TcxViewParams;
begin
  ABounds := ViewInfo.Bounds;
  ViewInfo.GetRecordBorderViewParams(AParams);
  if AParams.Bitmap = nil then
    Canvas.FrameRect(ABounds, AParams.Color, ViewInfo.RecordBorderWidth)
  else
    with ViewInfo.ContentBounds do
    begin
      Canvas.FillRect(Rect(ABounds.Left, ABounds.Top, ABounds.Right, Top), AParams.Bitmap);
      Canvas.FillRect(Rect(ABounds.Left, Top, Left, Bottom), AParams.Bitmap);
      Canvas.FillRect(Rect(Right, Top, ABounds.Right, Bottom), AParams.Bitmap);
      Canvas.FillRect(Rect(ABounds.Left, Bottom, ABounds.Right, ABounds.Bottom), AParams.Bitmap);
    end;
end;

procedure TcxGridLayoutViewRecordPainter.DrawRecordCaption;
begin
  if ViewInfo.CaptionViewInfo.Visible then
    ViewInfo.CaptionViewInfo.Paint(Canvas);
end;

procedure TcxGridLayoutViewRecordPainter.Paint;
begin
  DrawRecordCaption;
  DrawRecordBorder;
  DrawItems;
  inherited Paint;
end;

procedure TcxGridLayoutViewRecordPainter.DrawLayoutGroups;
var
  ARootViewInfo: TdxLayoutGroupViewInfo;
begin
  ARootViewInfo := ViewInfo.LayoutViewInfo.ItemsViewInfo;
  with TdxCustomLayoutItemViewInfoAccess(ARootViewInfo).GetPainterClass.Create(ARootViewInfo) do
    try
      Paint(Canvas);
    finally
      Free;
    end;
end;

procedure TcxGridLayoutViewRecordPainter.DrawExpandButton;
begin
  if CanDrawExpandButton then
    ViewInfo.ExpandButtonViewInfo.Paint(Canvas);
end;

function TcxGridLayoutViewRecordPainter.DrawExpandButtonBeforePaint: Boolean;
begin
  Result := False;
end;

procedure TcxGridLayoutViewRecordPainter.DrawItems;
var
  I: Integer;
begin
  if IsRecordExpanded then
  begin
    DrawLayoutGroups;
    for I := 0 to ViewInfo.ItemViewInfoCount - 1 do
      ViewInfo.ItemViewInfos[I].Paint(Canvas);
  end;
end;

function TcxGridLayoutViewRecordPainter.IsRecordExpanded: Boolean;
begin
  Result := ViewInfo.IsExpanded;
end;

function TcxGridLayoutViewRecordPainter.GetState: TcxButtonState;
begin
  if ViewInfo.Selected then
    if ViewInfo.GridView.IsControlFocused then
      Result := cxbsHot
    else
      Result := cxbsDisabled
  else
    Result := cxbsNormal;
end;

function TcxGridLayoutViewRecordPainter.GetViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewViewData }

function TcxGridLayoutViewViewData.GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridLayoutViewRecord;
end;

{ TcxGridLayoutViewItemFilterButtonViewInfo }

constructor TcxGridLayoutViewItemFilterButtonViewInfo.Create(
  ALayoutViewItemViewInfo: TcxGridLayoutViewItemViewInfo);
begin
  inherited Create(ALayoutViewItemViewInfo.GridViewInfo);
  FItemViewInfo := ALayoutViewItemViewInfo;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := inherited GetDropDownWindowOwnerBounds;
  if dxGetFilterPopupActualMode(GetFilterPopupMode) <> fpmExcel then
  begin
    Result.Left := ItemViewInfo.LayoutItemViewInfo.CaptionViewInfo.Bounds.Left;
    Result.Right := ItemViewInfo.LayoutItemViewInfo.CaptionViewInfo.Bounds.Right;
  end;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := ItemViewInfo.Item.GetFilterPopupMode;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  if GridView.IsDesigning then
    Result := nil
  else
    Result := TcxGridLayoutViewItemFilterButtonHitTest;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetItem: TcxCustomGridTableItem;
begin
  Result := ItemViewInfo.Item;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewItemFilterButtonPainter;
end;

function TcxGridLayoutViewItemFilterButtonViewInfo.GetVisible: Boolean;
begin
  Result := ItemViewInfo.Visible and (ItemViewInfo.LayoutItemViewInfo <> nil) and
    ItemViewInfo.LayoutItemViewInfo.HasCaption and
    (OccupiesSpace or (State <> gcsNone) or (ItemViewInfo.State <> gcsNone) or
    (ItemViewInfo.Focused and GridView.Controller.IsEditing));
end;

procedure TcxGridLayoutViewItemFilterButtonViewInfo.InitHitTest(
  AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  with TcxGridLayoutViewItemFilterButtonHitTest(AHitTest) do
  begin
    GridRecord := ItemViewInfo.GridRecord;
    Item := GetItem;
  end;
end;

{ TcxGridLayoutViewItemViewInfo }

constructor TcxGridLayoutViewItemViewInfo.Create(
  ARecordViewInfo: TcxGridLayoutViewRecordViewInfo;
  AItem: TcxGridLayoutViewItem);
begin
  inherited Create(ARecordViewInfo, AItem);
  FItem := AItem;
  FRecordViewInfo := ARecordViewInfo;
  CreateViewInfos;
end;

destructor TcxGridLayoutViewItemViewInfo.Destroy;
begin
  DestroyViewInfos;
  inherited Destroy;
end;

function TcxGridLayoutViewItemViewInfo.AllowRightToLeftConversionOnRecalculate: Boolean;
begin
  Result := False;
end;

procedure TcxGridLayoutViewItemViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  if (LayoutItemViewInfo <> nil) and HasFilterButton then
    FilterButtonViewInfo.Calculate(FilterButtonBounds);
end;

function TcxGridLayoutViewItemViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result <> nil) and HasFilterButton then
  begin
    AHitTest := FilterButtonViewInfo.GetHitTest(P);
    if AHitTest <> nil then
      Result := AHitTest;
  end;
end;

function TcxGridLayoutViewItemViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(VisibleBounds, P);
end;

function TcxGridLayoutViewItemViewInfo.MouseDown(
  AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  ASelfLink: TcxObjectLink;
begin
  ASelfLink := cxAddObjectLink(Self);
  try
    Result := inherited MouseDown(AHitTest, AButton, AShift);
    if (ASelfLink.Ref <> nil) and (AButton = mbLeft) then
      if GridView.IsDesigning then
      begin
        GridView.Controller.DesignController.SelectObject(Item, not (ssShift in AShift));
        Result := True;
      end;
  finally
    cxRemoveObjectLink(ASelfLink);
  end;
end;

function TcxGridLayoutViewItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewItemPainter;
end;

function TcxGridLayoutViewItemViewInfo.HasFilterButton: Boolean;
begin
  Result := FilterButtonViewInfo <> nil;
end;

function TcxGridLayoutViewItemViewInfo.HasFocusRect: Boolean;
begin
  Result := RecordViewInfo.HasItemFocusRect(Self);
end;

procedure TcxGridLayoutViewItemViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited InitHitTest(AHitTest);
  if AHitTest is TcxGridRecordCellHitTest then
  begin
    TcxGridRecordCellHitTest(AHitTest).Item := Item;
    TcxGridRecordCellHitTest(AHitTest).GridRecord := GridRecord;
  end;
end;

function TcxGridLayoutViewItemViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := GridView.OptionsBehavior.ItemHotTrack;
end;

procedure TcxGridLayoutViewItemViewInfo.StateChanged(
  APrevState: TcxGridCellState);
begin
  inherited StateChanged(APrevState);
  if HasFilterButton and FilterButtonViewInfo.Visible then
    FilterButtonViewInfo.Invalidate;
end;

function TcxGridLayoutViewItemViewInfo.CanFocus: Boolean;
begin
  Result := inherited CanFocus and Item.CanFocus(GridRecord);
end;

function TcxGridLayoutViewItemViewInfo.GetEditMinSize(AFont: TFont): TSize;
begin
  Result := Item.GetEditMinSize(AFont);
end;

function TcxGridLayoutViewItemViewInfo.GetExtraCaptionPadding: Integer;
begin
  if HasFilterButton and FilterButtonViewInfo.OccupiesSpace then
    Result := FilterButtonViewInfo.Width
  else
    Result := 0;
end;

procedure TcxGridLayoutViewItemViewInfo.GetCaptionParams(var AParams: TcxViewParams);
begin
  if InvalidateOnStateChange and (State = gcsSelected) {and not isPress} then
    GridView.Styles.GetItemHottrackParams(Item, AParams)
  else
    GridView.StylesAdapter.GetItemParams(GridRecord, Item, AParams);
end;

function TcxGridLayoutViewItemViewInfo.GetEditViewDataBounds: TRect;
begin
  Result := RecordViewInfo.GetEditViewDataBounds(Self);
end;

function TcxGridLayoutViewItemViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := LayoutItemViewInfo.Bounds;
end;

function TcxGridLayoutViewItemViewInfo.GetEditBounds: TRect;
begin
  Result := inherited GetEditBounds;
  Result.Right := Min(Result.Right, VisibleBounds.Right);
  Result.Bottom := Min(Result.Bottom, VisibleBounds.Bottom);
end;

function TcxGridLayoutViewItemViewInfo.GetFilterButtonBounds: TRect;
var
  AOffset: Integer;
  ACaptionBounds: TRect;
begin
  ACaptionBounds := LayoutItemViewInfo.CaptionViewInfo.Bounds;
  Result := ACaptionBounds;
  Inc(Result.Top);
  AOffset := FilterButtonViewInfo.Width + ScaleFactor.Apply(cxGridCellTextOffset) - LayoutItemViewInfo.ElementOffsetHorz;
  Result.Left := Max(Result.Left, Result.Right - AOffset);
  Result.Right := Result.Left + FilterButtonViewInfo.Width;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ACaptionBounds);
end;

function TcxGridLayoutViewItemViewInfo.GetFilterButtonViewInfoClass: TcxGridLayoutViewItemFilterButtonViewInfoClass;
begin
  Result := TcxGridLayoutViewItemFilterButtonViewInfo;
end;

function TcxGridLayoutViewItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridLayoutViewItemHitTest;
end;

function TcxGridLayoutViewItemViewInfo.GetHotTrack: Boolean;
begin
  Result := not GridView.IsDesigning and (HasFilterButton and FilterButtonViewInfo.NeedsContainerHotTrack) or
    inherited GetHotTrack;
end;

function TcxGridLayoutViewItemViewInfo.GetLayoutItemViewInfoInstance: TcxGridCustomLayoutItemViewInfo;
begin
  if FLayoutItemViewInfo = nil then
    FLayoutItemViewInfo := TcxGridLayoutItemViewInfo(RecordViewInfo.LayoutViewInfo.GetItemViewInfo(Item.LayoutItem));
  Result := FLayoutItemViewInfo;
end;

function TcxGridLayoutViewItemViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    (GridView.Controller.DesignController.IsObjectSelected(Item) or
    GridView.Controller.DesignController.IsObjectSelected(Item.LayoutItem.GetObjectForSelect));
end;

function TcxGridLayoutViewItemViewInfo.GetLayoutItemViewInfo: TcxGridLayoutItemViewInfo;
begin
  Result := TcxGridLayoutItemViewInfo(inherited LayoutItemViewInfo);
end;

function TcxGridLayoutViewItemViewInfo.GetRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited RecordViewInfo);
end;

procedure TcxGridLayoutViewItemViewInfo.CreateViewInfos;
begin
  if Item.CanFilter(True) then
    FFilterButtonViewInfo := GetFilterButtonViewInfoClass.Create(Self);
end;

procedure TcxGridLayoutViewItemViewInfo.DestroyViewInfos;
begin
  FreeAndNil(FFilterButtonViewInfo);
end;

function TcxGridLayoutViewItemViewInfo.GetGridRecord: TcxGridLayoutViewRecord;
begin
  Result := FRecordViewInfo.GridRecord;
end;

function TcxGridLayoutViewItemViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewItemViewInfo.GetRealEditViewDataBounds: TRect;
begin
  Result := GetEditViewDataBounds;
end;

{ TcxGridLayoutViewRecordExpandButtonViewInfo }

constructor TcxGridLayoutViewRecordExpandButtonViewInfo.Create(
  ARecordViewInfo: TcxGridLayoutViewRecordViewInfo);
begin
  inherited Create(ARecordViewInfo.GridViewInfo);
  FRecordViewInfo := ARecordViewInfo;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

procedure TcxGridLayoutViewRecordExpandButtonViewInfo.Click;
begin
  inherited;
  FRecordViewInfo.GridRecord.ToggleExpanded;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridExpandButtonHitTest;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewRecordExpandButtonPainter;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.GetVisible: Boolean;
begin
  with RecordViewInfo do
    Result := GridRecord.Expandable and CaptionViewInfo.Visible;
end;

function TcxGridLayoutViewRecordExpandButtonViewInfo.HasPoint(
  const P: TPoint): Boolean;
begin
  Result := inherited HasPoint(P) and
    LookAndFeelPainter.IsPointOverGroupExpandButton(Bounds, P);
end;

procedure TcxGridLayoutViewRecordExpandButtonViewInfo.InitHitTest(
  AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridExpandButtonHitTest(AHitTest).GridRecord := FRecordViewInfo.GridRecord;
end;

{ TcxGridLayoutViewRecordCaptionViewInfo }

constructor TcxGridLayoutViewRecordCaptionViewInfo.Create(
  ARecordViewInfo: TcxGridLayoutViewRecordViewInfo);
begin
  inherited Create(ARecordViewInfo.GridViewInfo);
  FRecordViewInfo := ARecordViewInfo;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.CustomDraw(
  ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
    GridView.DoCustomDrawRecordHeader(ACanvas, Self, Result);
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := GridView.OptionsView.RecordCaption.TextAlignmentHorz;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := GridView.OptionsView.RecordCaption.TextAlignmentVert;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetGridRecord: TcxGridLayoutViewRecord;
begin
  Result := RecordViewInfo.GridRecord;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridLayoutViewRecordCaptionHitTest;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewRecordCaptionPainter;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetText: string;
begin
  Result := RecordViewInfo.GetRecordCaptionText;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetDrawTextRotationAngle: TcxRotationAngle;
begin
  if RotatedText then
    Result := raPlus90
  else
    Result := ra0;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetRotatedText: Boolean;
begin
  Result := RecordViewInfo.IsRecordCaptionRotated;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  if RecordViewInfo.ExpandButtonViewInfo.Visible then
  begin
    if RotatedText then
    begin
      InflateRect(Result, -cxGridLayoutViewRecordExpandButtonOffset, 0);
      Inc(Result.Top, RecordViewInfo.ExpandButtonAreaWidth);
      Dec(Result.Bottom, cxGridLayoutViewRecordExpandButtonOffset);
    end
    else
    begin
      if RecordViewInfo.ExpandButtonAlignment = rebaRight then
      begin
        Dec(Result.Right, RecordViewInfo.ExpandButtonAreaWidth);
        Inc(Result.Left, cxGridLayoutViewRecordExpandButtonOffset);
      end
      else
      begin
        Inc(Result.Left, RecordViewInfo.ExpandButtonAreaWidth);
        Dec(Result.Right, cxGridLayoutViewRecordExpandButtonOffset);
      end;
    end;
  end
  else
    InflateRect(Result, -cxGridLayoutViewRecordExpandButtonOffset, 0);
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetTextAttributes(
  AForPainting: Boolean): Integer;
begin
  Result := inherited GetTextAttributes(AForPainting) or cxDontClip;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetRotatedTextBounds: TRect;
var
  ATextHeight: Integer;
  ATextWidth: Integer;
begin
  Result := TextAreaBounds;
  ATextWidth := TextWidth;
  ATextHeight := TextHeight;
  with Result do
  begin
    case AlignmentHorz of
      taLeftJustify:
        Top := Bottom - ATextWidth;
      taRightJustify:
        Bottom := Top + ATextWidth;
      taCenter:
        begin
          Top := (Top + Bottom - ATextWidth) div 2;
          Bottom := Top + ATextWidth;
        end;
    end;
    case AlignmentVert of
      vaTop:
        Right := Left + ATextHeight;
      vaBottom:
        Left := Right - ATextHeight;
      vaCenter:
        begin
          Left := (Left + Right - ATextHeight) div 2;
          Right := Left + ATextHeight;
        end;
    end;
  end;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetTextBoundsValue: TRect;
begin
  if RotatedText then
    Result := GetRotatedTextBounds
  else
    Result := inherited TextBounds;
end;

procedure TcxGridLayoutViewRecordCaptionViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.StylesAdapter.GetRecordCaptionParams(GridRecord, AParams);
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetVisible: Boolean;
begin
  Result := RecordViewInfo.HasRecordCaption and inherited GetVisible;
end;

function TcxGridLayoutViewRecordCaptionViewInfo.GetVisibleForHitTest: Boolean;
begin
  Result := inherited GetVisibleForHitTest and GridRecord.Expandable;
end;

procedure TcxGridLayoutViewRecordCaptionViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  RecordViewInfo.InitHitTest(AHitTest);
end;

function TcxGridLayoutViewRecordCaptionViewInfo.HasCustomDraw: Boolean;
begin
  Result := GridView.HasCustomDrawRecordHeader;
end;

procedure TcxGridLayoutViewRecordCaptionViewInfo.PrepareCanvas(ACanvas: TcxCanvas);
begin
  inherited;
  if LookAndFeelPainter.LayoutViewRecordCaptionTextBold then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
end;

{ TcxGridLayoutViewRecordViewInfoCacheItem }

function TcxGridLayoutViewRecordViewInfoCacheItem.GetGridRecord: TcxGridLayoutViewRecord;
begin
  Result := TcxGridLayoutViewRecord(inherited GridRecord);
end;

procedure TcxGridLayoutViewRecordViewInfoCacheItem.SetRecordCaptionHeight(Value: Integer);
begin
  FRecordCaptionHeight := Value;
  FIsRecordCaptionHeightAssigned := True;
end;

procedure TcxGridLayoutViewRecordViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  inherited UnassignValues(AKeepMaster);
  UnassignHeightValues;
end;

procedure TcxGridLayoutViewRecordViewInfoCacheItem.UnassignHeightValues;
begin
  IsHeightAssigned := False;
  FIsRecordCaptionHeightAssigned := False;
end;

{ TcxGridLayoutViewRecordViewInfo }

constructor TcxGridLayoutViewRecordViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo; ARecord: TcxCustomGridRecord);
begin
  inherited Create(ARecordsViewInfo, ARecord);
  FLayoutLookAndFeel := GridView.CreateLayoutLookAndFeel;
  FLayoutLookAndFeel.Assign(GridView.LayoutLookAndFeel);
  CreateItemViewInfos;
  FCaptionViewInfo := GetRecordCaptionViewInfoClass.Create(Self);
  FExpandButtonViewInfo := GetExpandButtonViewInfoClass.Create(Self);
end;

destructor TcxGridLayoutViewRecordViewInfo.Destroy;
begin
  FreeAndNil(FCaptionViewInfo);
  FreeAndNil(FExpandButtonViewInfo);
  DestroyItemViewInfos;
  FreeAndNil(FLayoutViewInfo);
  FreeAndNil(FLayoutLookAndFeel);
  inherited Destroy;
end;

procedure TcxGridLayoutViewRecordViewInfo.BeforeRecalculation;
var
  I: Integer;
begin
  inherited BeforeRecalculation;
  for I := 0 to ItemViewInfoCount - 1 do
    ItemViewInfos[I].BeforeRecalculation;
  FExpandButtonViewInfo.BeforeRecalculation;
  FCaptionViewInfo.BeforeRecalculation;
end;

procedure TcxGridLayoutViewRecordViewInfo.Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  CalculateLayout;
  inherited;
  if CaptionViewInfo.Visible then
    CaptionViewInfo.Calculate(RecordCaptionBounds);
  if IsGridViewInfoRightToLeftConverted then
    LayoutViewInfo.Offset := Point(GridViewInfo.Bounds.Right - LayoutAreaBounds.Right, LayoutAreaBounds.Top)
  else
    LayoutViewInfo.Offset := Point(LayoutAreaBounds.Left - GridViewInfo.Bounds.Left, LayoutAreaBounds.Top);
  CalculateItemViewInfos;
end;

procedure TcxGridLayoutViewRecordViewInfo.MainCalculate(ALeftBound, ATopBound: Integer);
begin
  inherited MainCalculate(ALeftBound, ATopBound);
  if ExpandButtonViewInfo.Visible then
    ExpandButtonViewInfo.Calculate(ExpandButtonBounds);
  if IsGridViewInfoRightToLeftConverted then
    RightToLeftConversion(Bounds);
end;

function TcxGridLayoutViewRecordViewInfo.GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect;
begin
  if AItem.VisibleIndex = -1 then
    Result := inherited GetBoundsForItem(AItem)
  else
    Result := ItemViewInfos[AItem.VisibleIndex].ContentBounds;
end;

function TcxGridLayoutViewRecordViewInfo.GetCacheItem: TcxGridLayoutViewRecordViewInfoCacheItem;
begin
  Result := TcxGridLayoutViewRecordViewInfoCacheItem(inherited CacheItem);
end;

function TcxGridLayoutViewRecordViewInfo.GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo;
begin
  if AItem.VisibleIndex = -1 then
    Result := nil
  else
    Result := ItemViewInfos[AItem.VisibleIndex];
end;

function TcxGridLayoutViewRecordViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result = nil) or not CanGetChildrenHitTest then
    Exit;
  AHitTest := ExpandButtonViewInfo.GetHitTest(P);
  if AHitTest <> nil then
  begin
    Result := AHitTest;
    Exit;
  end;
  AHitTest := CaptionViewInfo.GetHitTest(P);
  if AHitTest <> nil then
  begin
    Result := AHitTest;
    Exit;
  end;
  for I := 0 to ItemViewInfoCount - 1 do
  begin
    AHitTest := ItemViewInfos[I].GetHitTest(P);
    if AHitTest <> nil then
    begin
      Result := AHitTest;
      Exit;
    end;
  end;
end;

procedure TcxGridLayoutViewRecordViewInfo.GetRecordBorderViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetRecordBorderVisualParams(GridRecord, AParams);
end;

function TcxGridLayoutViewRecordViewInfo.CalculateRecordCaptionHeight: Integer;
begin
  Result := GridViewInfo.GetRecordCaptionHeight;
end;

function TcxGridLayoutViewRecordViewInfo.CanGetChildrenHitTest: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewRecordViewInfo.GetEditViewDataBounds(AItem: TcxGridLayoutViewItemViewInfo): TRect;
begin
  Result := AItem.LayoutItemViewInfo.ControlViewInfo.Bounds;
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionBounds: TRect;
begin
  if not HasRecordCaption then
  begin
    Result := cxEmptyRect;
    Exit;
  end;
  Result := ContentBounds;
  with GridView.OptionsView do
    InflateRect(Result, -RecordBorderWidth, -RecordBorderWidth);
  if IsRecordCaptionRotated then
    Dec(Result.Right, GetRecordCaptionTailSize)
  else
    Result.Bottom := Result.Top + GetRecordCaptionHeight;
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionHeight: Integer;
begin
  if CacheItem.IsRecordCaptionHeightAssigned then
    Result := CacheItem.RecordCaptionHeight
  else
  begin
    Result := CalculateRecordCaptionHeight;
    CacheItem.RecordCaptionHeight := Result;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionTailSize: Integer;
begin
  Result := LookAndFeelPainter.LayoutViewRecordCaptionTailSize(GetRecordCaptionPosition);
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionText: string;
begin
  Result := GridView.OptionsView.RecordCaption.GetFormattedText(GridRecord);
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionPosition: TcxGroupBoxCaptionPosition;
begin
  if not HasRecordCaption then
    Result := cxgpCenter
  else
    if IsRecordCaptionRotated then
      if IsRightToLeftConverted then
        Result := cxgpRight
      else
        Result := cxgpLeft
    else
      Result := cxgpTop;
end;

function TcxGridLayoutViewRecordViewInfo.HasRecordCaption: Boolean;
begin
  Result := GridView.OptionsView.RecordCaption.Visible;
end;

function TcxGridLayoutViewRecordViewInfo.HasItemFocusRect(AItem: TcxGridLayoutViewItemViewInfo): Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewRecordViewInfo.IsExpanded: Boolean;
begin
  Result := Expanded;
end;

function TcxGridLayoutViewRecordViewInfo.IsGridViewInfoRightToLeftConverted: Boolean;
begin
  Result := GridViewInfo.IsRightToLeftConverted;
end;

function TcxGridLayoutViewRecordViewInfo.IsRecordCaptionRotated: Boolean;
begin
  Result := HasRecordCaption and not IsExpanded and
    (GridView.OptionsView.ViewMode in [lvvmSingleRow, lvvmMultiRow,
    lvvmCarousel]);
end;

function TcxGridLayoutViewRecordViewInfo.GetLayoutAreaBorders: TcxBorders;
begin
  Result := [bLeft, bRight, bBottom];
  if not HasRecordCaption then
    Include(Result, bTop);
end;

function TcxGridLayoutViewRecordViewInfo.GetLayoutAreaBounds: TRect;
begin
  Result := ContentBounds;
  with GridView.OptionsView do
    InflateRect(Result, -RecordBorderWidth, -RecordBorderWidth);
  if IsRecordCaptionRotated then
    if GridViewInfo.IsRightToLeftConverted then
      Dec(Result.Right, GetRecordCaptionHeight)
    else
      Inc(Result.Left, GetRecordCaptionHeight)
  else
    Inc(Result.Top, GetRecordCaptionHeight);
end;

procedure TcxGridLayoutViewRecordViewInfo.CalculateItemViewInfos;
var
  I: Integer;
begin
  for I := 0 to ItemViewInfoCount - 1 do
    ItemViewInfos[I].Calculate(Bounds);
  CheckFocusedItem;
end;

procedure TcxGridLayoutViewRecordViewInfo.CreateItemViewInfos;
var
  I: Integer;
  AItem: TcxGridLayoutViewItem;
  AViewInfo: TcxGridLayoutViewItemViewInfo;
begin
  FItemViewInfos := TcxObjectList.Create;
  if IsExpanded or not GridView.IsDefaultViewInfoCalculated then
  begin
    for I := 0 to GridView.VisibleItemCount - 1 do
    begin
      AItem := GridView.VisibleItems[I];
      AViewInfo := GetItemViewInfoClass.Create(Self, AItem);
      FItemViewInfos.Add(AViewInfo)
    end;
  end;
  CreateLayoutViewInfo;
end;

procedure TcxGridLayoutViewRecordViewInfo.DestroyItemViewInfos;
begin
  FreeAndNil(FLayoutViewInfo);
  FreeAndNil(FItemViewInfos);
end;

procedure TcxGridLayoutViewRecordViewInfo.PopulateTabOrderList(AList: TList);
begin
  if LayoutCalculated then
    LayoutViewInfo.PopulateTabOrderList(AList);
end;

procedure TcxGridLayoutViewRecordViewInfo.CalculateLayout;
begin
  if not LayoutCalculated then
  begin
    UpdateContainerStyles;
    CacheItem.UnassignHeightValues;
    LayoutViewInfo.PrepareData(True);
    CalculateLayoutViewInfo;
    LayoutCalculated := True;
  end;
end;

procedure TcxGridLayoutViewRecordViewInfo.CheckCloneData;
begin
  if GridRecord.CloneDataStream.Size <> GridView.DefaultCloneData.Size then
  begin
    ResetCloneData;
    SaveCloneData;
  end;
end;

procedure TcxGridLayoutViewRecordViewInfo.CheckFocusedItem;
var
  I: Integer;
begin
  if GridViewInfo.CalculateDown and (GridView.Controller.FocusedRecordViewInfo = Self) and Visible then
    for I := 0 to ItemViewInfoCount - 1 do
      if ItemViewInfos[I].Focused then
      begin
        GridView.Controller.CheckFocusItem(ItemViewInfos[I]);
        Break;
      end;
end;

procedure TcxGridLayoutViewRecordViewInfo.ClearItemLinks;
var
  I: Integer;
begin
  for I := 0 to ItemViewInfoCount - 1 do
    ItemViewInfos[I].FLayoutItemViewInfo := nil;
end;

procedure TcxGridLayoutViewRecordViewInfo.CloneDataChangedHandler(Sender: TObject);
var
  ANeedRefresh: Boolean;
  R: TRect;
begin
  SaveCloneData;
  R := LayoutViewInfo.ContentBounds;
  CalculateLayoutViewInfo;
  ANeedRefresh := not EqualRect(R, LayoutViewInfo.ContentBounds);
  if ANeedRefresh then
    GridView.Reposition
  else
  begin
    CalculateItemViewInfos;
    Invalidate;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.IsDefaultCloneData: Boolean;
begin
  Result := (GridRecord.CloneDataStream.Size = 0) or
    ((GridView.DefaultCloneData.Size = GridRecord.CloneDataStream.Size) and
    CompareMem(GridView.DefaultCloneData.Memory, GridRecord.CloneDataStream.Memory, GridRecord.CloneDataStream.Size));
end;

function TcxGridLayoutViewRecordViewInfo.LoadCloneData: Boolean;
begin
  Result := GridRecord.CloneDataStream.Size > 0;
  if Result then
  begin
    GridRecord.CloneDataStream.Position := 0;
    FLayoutViewInfo.LoadCloneData(GridRecord.CloneDataStream);
    FLayoutViewInfo.DoCalculate(False);
  end;
end;

procedure TcxGridLayoutViewRecordViewInfo.RecalculateCloneData;
begin
  ResetCalculated;
  LayoutViewInfo.PrepareData(True);
  LayoutViewInfo.RecalculateCloneDataList;
  CalculateLayout;
  SaveCloneData;
end;

procedure TcxGridLayoutViewRecordViewInfo.ResetCalculated;
begin
  FCalculatedCaptionHeight := -1;
  LayoutCalculated := False;
end;

procedure TcxGridLayoutViewRecordViewInfo.ResetCloneData;
begin
  GridRecord.CloneDataStream.Clear;
end;

procedure TcxGridLayoutViewRecordViewInfo.UpdateContainerStyles;
begin
  GridView.UpdateContainerStyles(GridRecord, FLayoutLookAndFeel);
end;

procedure TcxGridLayoutViewRecordViewInfo.SaveCloneData;
begin
  ResetCloneData;
  GridRecord.CloneDataStream.Size := LayoutViewInfo.GetCloneDataSize;
  LayoutViewInfo.SaveCloneData(GridRecord.CloneDataStream);
end;

procedure TcxGridLayoutViewRecordViewInfo.CalculateLayoutViewInfo;
var
  AOffset: TPoint;
begin
  CheckCloneData;
  if not IsDefaultCloneData then
  begin
    if not LoadCloneData then
    begin
      LayoutViewInfo.Calculate(True);
      SaveCloneData;
    end;
  end
  else
  begin
    AOffset := LayoutViewInfo.Offset;
    GridView.AssignDefaultBounds(LayoutViewInfo);
    LayoutViewInfo.Offset := AOffset;
  end;
end;

procedure TcxGridLayoutViewRecordViewInfo.CreateLayoutViewInfo;
begin
  FLayoutViewInfo := TcxGridLayoutContainerCloneViewInfo.Create(GridView.Container, Self);
  FLayoutViewInfo.OnCloneDataChanged := CloneDataChangedHandler;
end;

function TcxGridLayoutViewRecordViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridLayoutViewRecordPainter;
end;

function TcxGridLayoutViewRecordViewInfo.GetPixelScrollSize: Integer;
begin
  FIsPixelScrollSizeCalculating := True;
  try
    Result := inherited GetPixelScrollSize;
  finally
    FIsPixelScrollSizeCalculating := False;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.CalculateHeight: Integer;
begin
  if not FIsPixelScrollSizeCalculating or not IsDefaultCloneData then
    CalculateLayout;
  if IsExpanded or IsRecordCaptionRotated then
  begin
    if FIsPixelScrollSizeCalculating and IsDefaultCloneData then
      Result := GridView.DefaultLayoutViewInfo.ContentHeight
    else
      Result := LayoutViewInfo.ContentHeight;
  end
  else
    Result := GetRecordCaptionTailSize;
  Inc(Result, GetRecordCaptionHeight);
  Inc(Result, 2 * RecordBorderWidth);
end;

function TcxGridLayoutViewRecordViewInfo.CalculateWidth: Integer;
begin
  if not FIsPixelScrollSizeCalculating or not IsDefaultCloneData then
    CalculateLayout;
  if not IsExpanded and IsRecordCaptionRotated then
    Result := GetRecordCaptionHeight
  else
    if FIsPixelScrollSizeCalculating and IsDefaultCloneData then
      Result := GridView.DefaultLayoutViewInfo.ContentWidth
    else
      Result := LayoutViewInfo.ContentWidth;
  Inc(Result, 2 * RecordBorderWidth);
  Result := Max(Result, GridView.OptionsView.MinValueWidth);
end;

function TcxGridLayoutViewRecordViewInfo.CanShowGroupScrollBars: Boolean;
begin
  Result := Expanded;
end;

function TcxGridLayoutViewRecordViewInfo.GetHeight: Integer;
begin
  if CacheItem.IsHeightAssigned then
    Result := CacheItem.Height
  else
  begin
    Result := CalculateHeight;
    CacheItem.Height := Result;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.GetExpandButtonViewInfoClass: TcxGridLayoutViewRecordExpandButtonViewInfoClass;
begin
  Result := TcxGridLayoutViewRecordExpandButtonViewInfo;
end;

function TcxGridLayoutViewRecordViewInfo.GetItemViewInfoClass: TcxGridLayoutViewItemViewInfoClass;
begin
  Result := TcxGridLayoutViewItemViewInfo;
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordCaptionViewInfoClass: TcxGridLayoutViewRecordCaptionViewInfoClass;
begin
  Result := TcxGridLayoutViewRecordCaptionViewInfo;
end;

function TcxGridLayoutViewRecordViewInfo.GetWidth: Integer;
begin
  if IsRecordCaptionRotated then
    Result := GetRecordCaptionHeight + 2 * RecordBorderWidth + GetRecordCaptionTailSize
  else
    Result := CalculateWidth;
end;

function TcxGridLayoutViewRecordViewInfo.GetTransparent: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewRecordViewInfo.HasFocusRect: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewRecordViewInfo.GetVisible: Boolean;
begin
  if GetGridView.OptionsView.ShowOnlyEntireRecords then
    Result := Index < RecordsViewInfo.VisibleCount
  else
    Result := inherited GetVisible;
end;

procedure TcxGridLayoutViewRecordViewInfo.CalculateExpandButtonBounds(var ABounds: TRect);
var
  ASize: Integer;
begin
  ABounds := ExpandButtonAreaBounds;
  if IsRectEmpty(ABounds) then Exit;
  ASize := ExpandButtonSize;
  with ABounds do
  begin
    Left := (Left + Right - ASize) div 2;
    Right := Left + ASize;
    Top := (Top + Bottom - ASize) div 2;
    Bottom := Top + ASize;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.CanGenerateExpandButtonHitTest: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewRecordViewInfo.Click(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  if (AButton = mbLeft) and (ssDouble in AShift) and
    (AHitTest.HitTestCode = htLayoutViewRecordCaption) and GridView.OptionsBehavior.ExpandRecordOnDblClick then
  begin
    GridRecord.ToggleExpanded;
    Result := True;
  end
  else
    Result := inherited Click(AHitTest, AButton, AShift);
end;

procedure TcxGridLayoutViewRecordViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if CaptionViewInfo.Visible then
    CaptionViewInfo.RightToLeftConversion(ABounds);
  if ExpandButtonViewInfo.Visible then
    ExpandButtonViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridLayoutViewRecordViewInfo.GetExpandButtonAlignment: TcxGridLayoutViewRecordExpandButtonAlignment;
begin
  Result := GridView.OptionsView.RecordCaption.ExpandButtonAlignment;
end;

function TcxGridLayoutViewRecordViewInfo.GetExpandButtonAreaBounds: TRect;
begin
  Result := RecordCaptionBounds;
  Result.Bottom := Result.Top + ExpandButtonAreaWidth;
  case ExpandButtonAlignment of
    rebaLeft:
      Result.Right := Result.Left + ExpandButtonAreaWidth;
    rebaRight:
      Result.Left := Result.Right - ExpandButtonAreaWidth;
  end;
end;

function TcxGridLayoutViewRecordViewInfo.GetExpandButtonAreaWidth: Integer;
begin
  Result := cxGridLayoutViewRecordExpandButtonOffset * 2 + ExpandButtonSize;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxGridLayoutViewRecordViewInfo.GetGridRecord: TcxGridLayoutViewRecord;
begin
  Result := TcxGridLayoutViewRecord(inherited GridRecord);
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordBorderWidth: Integer;
begin
  Result := GridView.OptionsView.RecordBorderWidth;
end;

function TcxGridLayoutViewRecordViewInfo.GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  Result := TcxGridLayoutViewRecordsViewInfo(inherited RecordsViewInfo);
end;

procedure TcxGridLayoutViewRecordViewInfo.SetLayoutCalculated(AValue: Boolean);
begin
  FLayoutCalculated := AValue;
  if LayoutViewInfo <> nil then
    LayoutViewInfo.FIsValid := FLayoutCalculated;
end;

function TcxGridLayoutViewRecordViewInfo.GetExpandButtonSizeValue: Integer;
begin
  Result := GetExpandButtonSize(LookAndFeelPainter, ScaleFactor);
end;

function TcxGridLayoutViewRecordViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewRecordViewInfo.GetGridViewInfo: TcxGridLayoutViewViewInfo;
begin
  Result := TcxGridLayoutViewViewInfo(inherited GridViewInfo);
end;

function TcxGridLayoutViewRecordViewInfo.GetItemViewInfoCount: Integer;
begin
  Result := FItemViewInfos.Count;
end;

function TcxGridLayoutViewRecordViewInfo.GetItemViewInfos(Index: Integer): TcxGridLayoutViewItemViewInfo;
begin
  Result := TcxGridLayoutViewItemViewInfo(FItemViewInfos[Index]);
end;

{ TcxGridLayoutViewInfoHorizontalCalculator }

procedure TcxGridLayoutViewInfoHorizontalCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited;
  if RecordsViewInfo.GridView.OptionsView.CenterRecords then
    Dec(ABounds.Right, RecordsViewInfo.RecordIndent);
end;

procedure TcxGridLayoutViewInfoHorizontalCalculator.DoCalculate(const AContentBounds: TRect);
begin
  inherited DoCalculate(AContentBounds);
  if CanCenterHorizontally then
    CenterHorizontally(AContentBounds);
  if CanCenterVertically then
    CenterVertically(AContentBounds);
end;

function TcxGridLayoutViewInfoHorizontalCalculator.GetMaxColumnCount: Integer;
begin
  Result := RecordsViewInfo.GridView.OptionsView.MaxColumnCount;
end;

function TcxGridLayoutViewInfoHorizontalCalculator.GetMaxRowCount: Integer;
begin
  Result := RecordsViewInfo.GridView.OptionsView.MaxRowCount;
end;

function TcxGridLayoutViewInfoHorizontalCalculator.CanCenterHorizontally: Boolean;
begin
  Result := RecordsViewInfo.CanCenterRecords;
end;

function TcxGridLayoutViewInfoHorizontalCalculator.CanCenterVertically: Boolean;
begin
  Result := RecordsViewInfo.CanCenterRecords;
end;

procedure TcxGridLayoutViewInfoHorizontalCalculator.CenterHorizontally(const AContentBounds: TRect);
var
  I, J: Integer;
  AColumnCount: Integer;
  AWidth: Integer;
  AOffset: Integer;
begin
  AColumnCount := RecordsViewInfo.Bands.VisibleCount;
  AWidth := 0;
  for I := 0 to AColumnCount - 1 do
  begin
    if I > 0 then
      Inc(AWidth, RecordSpaceHorz);
    Inc(AWidth, RecordsViewInfo.GetMaxRecordWidth(I));
  end;
  AOffset := (cxRectWidth(AContentBounds) - AWidth) div (2 * AColumnCount);
  for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
    for J := 0 to RecordsViewInfo.Bands[I].VisibleCount - 1 do
      with TcxGridLayoutViewRecordViewInfo(RecordsViewInfo.Bands[I][J]) do
      begin
        CalculationPosition := cxPointOffset(CalculationPosition, AOffset * (1 + 2 * Col), 0);
        CalculationPosition := cxPointOffset(CalculationPosition, (Self.RecordsViewInfo.GetMaxRecordWidth(Col) - Width) div 2, 0);
      end;
end;

procedure TcxGridLayoutViewInfoHorizontalCalculator.CenterVertically(const AContentBounds: TRect);
var
  I, J: Integer;
  AColumnCount: Integer;
  ARowCount: Integer;
  AHeight: Integer;
  AOffset: Integer;
begin
  AColumnCount := RecordsViewInfo.Bands.VisibleCount;
  for I := 0 to AColumnCount - 1 do
  begin
    AHeight := 0;
    ARowCount := RecordsViewInfo.Bands[I].VisibleCount;
    for J := 0 to ARowCount - 1 do
    begin
      if J > 0 then
        Inc(AHeight, GetRecordSpaceVert);
      Inc(AHeight, RecordsViewInfo.Bands[I][J].Height);
    end;
    AOffset := (cxRectHeight(AContentBounds) - AHeight) div (2 * ARowCount);
    for J := 0 to ARowCount - 1 do
      with TcxGridLayoutViewRecordViewInfo(RecordsViewInfo.Bands[I][J]) do
        CalculationPosition := cxPointOffset(CalculationPosition, 0, AOffset * (1 + 2 * Row));
  end;
end;

function TcxGridLayoutViewInfoHorizontalCalculator.GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  Result := TcxGridLayoutViewRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridLayoutViewInfoVerticalCalculator }

procedure TcxGridLayoutViewInfoVerticalCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited;
  if RecordsViewInfo.GridView.OptionsView.CenterRecords then
    Dec(ABounds.Bottom, RecordsViewInfo.RecordIndent);
end;

procedure TcxGridLayoutViewInfoVerticalCalculator.DoCalculate(const AContentBounds: TRect);
begin
  inherited DoCalculate(AContentBounds);
  if CanCenterHorizontally then
    CenterHorizontally(AContentBounds);
  if CanCenterVertically then
    CenterVertically(AContentBounds);
end;

function TcxGridLayoutViewInfoVerticalCalculator.GetMaxColumnCount: Integer;
begin
  Result := RecordsViewInfo.GridView.OptionsView.MaxColumnCount;
end;

function TcxGridLayoutViewInfoVerticalCalculator.GetMaxRowCount: Integer;
begin
  Result := RecordsViewInfo.GridView.OptionsView.MaxRowCount;
end;

function TcxGridLayoutViewInfoVerticalCalculator.CanCenterHorizontally: Boolean;
begin
  Result := RecordsViewInfo.CanCenterRecords;
end;

function TcxGridLayoutViewInfoVerticalCalculator.CanCenterVertically: Boolean;
begin
  Result := RecordsViewInfo.CanCenterRecords;
end;

procedure TcxGridLayoutViewInfoVerticalCalculator.CenterHorizontally(const AContentBounds: TRect);
var
  I, AColumn, ARow: Integer;
  AColumnCount: Integer;
  ACount: Integer;
  ARowCount: Integer;
  AWidth: Integer;
  AOffset: Integer;
begin
  ARowCount := GetRowCount;
  AColumnCount := RecordsViewInfo.Bands.VisibleCount;
  for ARow := 0 to ARowCount - 1 do
  begin
    AWidth := 0;
    ACount := 0;
    for AColumn := 0 to AColumnCount - 1 do
      if RecordsViewInfo.Bands.IsItemVisible(AColumn, ARow) then
      begin
        if AWidth > 0 then
          Inc(AWidth, GetRecordSpaceHorz);
        Inc(AWidth, RecordsViewInfo.Bands[AColumn][ARow].Width);
        Inc(ACount);
      end;
    if ACount = 0 then
      Continue;
    AOffset := (cxRectWidth(AContentBounds) - AWidth) div (2 * ACount);
    for I := 0 to AColumnCount - 1 do
      if RecordsViewInfo.Bands.IsItemVisible(I, ARow) then
        with TcxGridLayoutViewRecordViewInfo(RecordsViewInfo.Bands[I][ARow]) do
          CalculationPosition := cxPointOffset(CalculationPosition, AOffset * (1 + 2 * Col), 0);
  end;
end;

procedure TcxGridLayoutViewInfoVerticalCalculator.CenterVertically(const AContentBounds: TRect);
var
  AColumn, ARow: Integer;
  ARowCount: Integer;
  AHeight: Integer;
  AOffset: Integer;
begin
  ARowCount := GetRowCount;
  AHeight := 0;
  for ARow := 0 to ARowCount - 1 do
  begin
    if ARow > 0 then
      Inc(AHeight, RecordSpaceVert);
    Inc(AHeight, RecordsViewInfo.GetMaxRecordHeight(ARow));
  end;
  AOffset := (cxRectHeight(AContentBounds) - AHeight) div (2 * ARowCount);
  for AColumn := 0 to RecordsViewInfo.Bands.Count - 1 do
    for ARow := 0 to RecordsViewInfo.Bands[AColumn].Count - 1 do
      if RecordsViewInfo.Bands.IsItemVisible(AColumn, ARow) then
        with TcxGridLayoutViewRecordViewInfo(RecordsViewInfo.Bands[AColumn][ARow]) do
        begin
          CalculationPosition := cxPointOffset(CalculationPosition, 0, AOffset * (1 + 2 * Row));
          CalculationPosition := cxPointOffset(CalculationPosition, 0, (Self.RecordsViewInfo.GetMaxRecordHeight(Row) - Height) div 2);
        end;
end;

function TcxGridLayoutViewInfoVerticalCalculator.GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  Result := TcxGridLayoutViewRecordsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridLayoutViewInfoVerticalCalculator.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
    Result := Max(Result, RecordsViewInfo.Bands[I].VisibleCount);
end;

{ TcxGridLayoutViewInfoSingleRecordCalculator }

procedure TcxGridLayoutViewInfoSingleRecordCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited AdjustContentBounds(ABounds);
  Dec(ABounds.Top, RecordsViewInfo.Controller.ContentScrollBarPosition);
end;

function TcxGridLayoutViewInfoSingleRecordCalculator.CanCenterVertically: Boolean;
begin
  Result := inherited CanCenterVertically and not RecordsViewInfo.ControllerHelper.NeedContentScrollBar;
end;

function TcxGridLayoutViewInfoSingleRecordCalculator.GetMaxColumnCount: Integer;
begin
  Result := 1;
end;

function TcxGridLayoutViewInfoSingleRecordCalculator.GetMaxRowCount: Integer;
begin
  Result := 1;
end;

{ TcxGridLayoutViewInfoSingleColumnCalculator }

procedure TcxGridLayoutViewInfoSingleColumnCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited AdjustContentBounds(ABounds);
  Dec(ABounds.Left, RecordsViewInfo.Controller.ContentScrollBarPosition);
end;

function TcxGridLayoutViewInfoSingleColumnCalculator.CanCenterHorizontally: Boolean;
begin
  Result := inherited CanCenterHorizontally and not RecordsViewInfo.ControllerHelper.NeedContentScrollBar;
end;

function TcxGridLayoutViewInfoSingleColumnCalculator.CanCenterVertically: Boolean;
begin
  Result := inherited CanCenterVertically and RecordsViewInfo.GridView.OptionsView.ShowOnlyEntireRecords;
end;

function TcxGridLayoutViewInfoSingleColumnCalculator.GetMaxColumnCount: Integer;
begin
  Result := 1;
end;

{ TcxGridLayoutViewInfoMultiColumnCalculator }


function TcxGridLayoutViewInfoMultiColumnCalculator.CanCenterHorizontally: Boolean;
begin
  Result := inherited CanCenterHorizontally and RecordsViewInfo.GridView.OptionsView.ShowOnlyEntireRecords and
    not RecordsViewInfo.ControllerHelper.NeedContentScrollBar;
end;

procedure TcxGridLayoutViewInfoMultiColumnCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited AdjustContentBounds(ABounds);
  Dec(ABounds.Left, RecordsViewInfo.Controller.ContentScrollBarPosition);
end;

{ TcxGridLayoutViewInfoSingleRowCalculator }

procedure TcxGridLayoutViewInfoSingleRowCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited AdjustContentBounds(ABounds);
  Dec(ABounds.Top, RecordsViewInfo.Controller.ContentScrollBarPosition);
end;

function TcxGridLayoutViewInfoSingleRowCalculator.CanCenterHorizontally: Boolean;
begin
  Result := inherited CanCenterHorizontally and
    RecordsViewInfo.GridView.OptionsView.ShowOnlyEntireRecords;
end;

function TcxGridLayoutViewInfoSingleRowCalculator.CanCenterVertically: Boolean;
begin
  Result := inherited CanCenterVertically and
    not RecordsViewInfo.ControllerHelper.NeedContentScrollBar;
end;

function TcxGridLayoutViewInfoSingleRowCalculator.GetMaxRowCount: Integer;
begin
  Result := 1;
end;

{ TcxGridLayoutViewInfoMultiRowCalculator }

procedure TcxGridLayoutViewInfoMultiRowCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited AdjustContentBounds(ABounds);
  Dec(ABounds.Top, RecordsViewInfo.Controller.ContentScrollBarPosition);
end;

function TcxGridLayoutViewInfoMultiRowCalculator.CanCenterVertically: Boolean;
begin
  Result := inherited CanCenterVertically and RecordsViewInfo.GridView.OptionsView.ShowOnlyEntireRecords and
    not RecordsViewInfo.ControllerHelper.NeedContentScrollBar;
end;

{ TcxGridLayoutViewSeparatorsViewInfo }

function TcxGridLayoutViewSeparatorsViewInfo.CalculateMaxBottom(ARow: Integer): Integer;
var
  I: Integer;
begin
  Result := RecordsViewInfo.Bounds.Top;
  for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
    if RecordsViewInfo.Bands.IsItemVisible(I, ARow) then
      Result := Max(Result, RecordsViewInfo.Bands[I][ARow].Bounds.Bottom);
end;

function TcxGridLayoutViewSeparatorsViewInfo.CalculateMaxRight(AColumn: Integer): Integer;
var
  I: Integer;
  AIsLastVisibleColumn: Boolean;
begin
  Result := RecordsViewInfo.Bounds.Left;
  AIsLastVisibleColumn := AColumn = RecordsViewInfo.Bands.VisibleCount - 1;
  if AIsLastVisibleColumn then
    for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
      Result := Max(Result, RecordsViewInfo.Bands[I].LastVisible.Bounds.Right)
  else
    for I := 0 to RecordsViewInfo.Bands[AColumn].VisibleCount - 1 do
      Result := Max(Result, RecordsViewInfo.Bands[AColumn][I].Bounds.Right);
end;

function TcxGridLayoutViewSeparatorsViewInfo.CalculateMinLeft(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := RecordsViewInfo.Bounds.Right;
  for I := 0 to RecordsViewInfo.Bands[AColumn].VisibleCount - 1 do
    Result := Min(Result, RecordsViewInfo.Bands[AColumn][I].Bounds.Left);
end;

function TcxGridLayoutViewSeparatorsViewInfo.CalculateMinTop(ARow: Integer): Integer;
var
  I: Integer;
begin
  Result := RecordsViewInfo.Bounds.Bottom;
  for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
    if RecordsViewInfo.Bands.IsItemVisible(I, ARow) then
      Result := Min(Result, RecordsViewInfo.Bands[I][ARow].Bounds.Top)
end;

function TcxGridLayoutViewSeparatorsViewInfo.GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  Result := TcxGridLayoutViewRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo }

procedure TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo.DoCalculate;

  procedure CalculateLeftRight(var ALeft, ARight: Integer);
  var
    ALeftClientOffset, AOffset: Integer;
  begin
    ALeftClientOffset := RecordsViewInfo.GridViewInfo.Bounds.Left;
    ALeft := CalculateMinLeft(0);
    ARight := CalculateMaxRight(RecordsViewInfo.Bands.VisibleCount - 1);
    AOffset := (ALeft - ALeftClientOffset) div 2;
    ALeft := ALeftClientOffset + AOffset;
    ARight := ARight + AOffset;
  end;

var
  I: Integer;
  R: TRect;
begin
  inherited;
  CalculateLeftRight(R.Left, R.Right);
  for I := 1 to RecordsViewInfo.Bands[0].VisibleCount - 1 do
  begin
    R.Top := CalculateMaxBottom(I - 1);
    R.Bottom := CalculateMinTop(I);
    R.Top := (R.Top + R.Bottom - Width) div 2;
    R.Bottom := R.Top + Width;
    Add(R);
  end;
end;

{ TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo }

procedure TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo.DoCalculate;

  procedure CalculateTopBottom(var ATop, ABottom: Integer);
  begin
    ATop := CalculateMinTop(0);
    ABottom := CalculateMaxBottom(RecordsViewInfo.Bands[0].VisibleCount - 1);
    ATop := ATop div 2;
    ABottom := ABottom + ATop;
  end;

var
  I: Integer;
  R: TRect;
begin
  inherited;

  CalculateTopBottom(R.Top, R.Bottom);
  for I := 1 to RecordsViewInfo.Bands.VisibleCount - 1 do
  begin
    R.Left := CalculateMaxRight(I - 1);
    R.Right := CalculateMinLeft(I);
    R.Left := (R.Left + R.Right - Width) div 2;
    R.Right := R.Left + Width;
    Add(R);
  end;
end;

{ TcxGridLayoutViewMultiColumnRecordSeparatorsViewInfo }

procedure TcxGridLayoutViewMultiColumnRecordSeparatorsViewInfo.DoCalculate;

  procedure CalculateLeftRight(var ALeft, ARight: Integer; AColumn: Integer);
  var
    AIsLastVisibleColumn: Boolean;
    AIsFirstVisibleColumn: Boolean;
  begin
    AIsFirstVisibleColumn := AColumn = 0;
    AIsLastVisibleColumn := AColumn = RecordsViewInfo.Bands.VisibleCount - 1;
    ALeft := CalculateMinLeft(AColumn);
    if AIsFirstVisibleColumn then
      ALeft := ALeft div 2
    else
      ALeft := (ALeft + CalculateMaxRight(AColumn - 1)) div 2;
    ARight := CalculateMaxRight(AColumn);
    if AIsLastVisibleColumn then
      ARight := (ARight + RecordsViewInfo.Bounds.Right) div 2
    else
      ARight := (ARight + CalculateMinLeft(AColumn + 1)) div 2;
  end;

var
  I, J: Integer;
  R: TRect;
begin
  inherited;
  for I := 0 to RecordsViewInfo.Bands.VisibleCount - 1 do
  begin
    CalculateLeftRight(R.Left, R.Right, I);
    for J := 1 to RecordsViewInfo.Bands[I].VisibleCount - 1 do
    begin
      R.Top := RecordsViewInfo.Bands[I][J - 1].Bounds.Bottom;
      R.Bottom := RecordsViewInfo.Bands[I][J].Bounds.Top;
      R.Top := (R.Top + R.Bottom - Width) div 2;
      R.Bottom := R.Top + Width;
      Add(R);
    end;
  end;
end;

{ TcxGridLayoutViewMultiRowRecordSeparatorsViewInfo }

procedure TcxGridLayoutViewMultiRowRecordSeparatorsViewInfo.DoCalculate;

  procedure CalculateTopBottom(var ATop, ABottom: Integer; ARow: Integer);
  var
    AIsLastVisibleRow: Boolean;
    AIsFirstVisibleRow: Boolean;
  begin
    AIsFirstVisibleRow := ARow = 0;
    AIsLastVisibleRow := ARow = RecordsViewInfo.Bands[0].VisibleCount - 1;
    ATop := CalculateMinTop(ARow);
    if AIsFirstVisibleRow then
      ATop := ATop div 2
    else
      ATop := (ATop + CalculateMaxBottom(ARow - 1)) div 2;
    ABottom := CalculateMaxBottom(ARow);
    if AIsLastVisibleRow then
      ABottom := (ABottom + RecordsViewInfo.Bounds.Bottom) div 2
    else
      ABottom := (ABottom + CalculateMinTop(ARow + 1)) div 2;
  end;

var
  I, J: Integer;
  R: TRect;
begin
  inherited;
  for I := 1 to RecordsViewInfo.Bands.Count - 1 do
    for J := 0 to RecordsViewInfo.Bands[I].Count - 1 do
      if RecordsViewInfo.Bands.IsItemVisible(I, J) then
      begin
        CalculateTopBottom(R.Top, R.Bottom, J);
        R.Left := RecordsViewInfo.Bands[I - 1][J].Bounds.Right;
        R.Right := RecordsViewInfo.Bands[I][J].Bounds.Left;
        R.Left := (R.Left + R.Right - Width) div 2;
        R.Right := R.Left + Width;
        Add(R);
      end;
end;

{ TcxGridLayoutViewRecordsViewInfo }

function TcxGridLayoutViewRecordsViewInfo.CalculateRecordWidth: Integer;
begin
  if GridView.IsDefaultViewInfoCalculated then
    Result := cxRectWidth(GridView.DefaultLayoutViewInfo.ItemsViewInfo.Bounds)
  else
    Result := 0;
  Inc(Result, 2 * GridView.OptionsView.RecordBorderWidth);
end;

procedure TcxGridLayoutViewRecordsViewInfo.CalculateVisibleRecordsBound(out AFirstRecordIndex, ALastRecordIndex: Integer);
begin
  AFirstRecordIndex := 0;
  ALastRecordIndex := 0;
end;

function TcxGridLayoutViewRecordsViewInfo.GetAutoDataRecordHeight: Boolean;
begin
  Result := AutoDataCellHeight or GridView.OptionsCustomize.RecordExpanding;
end;

function TcxGridLayoutViewRecordsViewInfo.GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass;
begin
  case GridView.OptionsView.ViewMode of
    lvvmSingleColumn: Result := TcxGridLayoutViewInfoSingleColumnCalculator;
    lvvmMultiColumn: Result := TcxGridLayoutViewInfoMultiColumnCalculator;
    lvvmSingleRow: Result := TcxGridLayoutViewInfoSingleRowCalculator;
    lvvmMultiRow: Result := TcxGridLayoutViewInfoMultiRowCalculator;
  else
    Result := TcxGridLayoutViewInfoSingleRecordCalculator;
  end;
end;

function TcxGridLayoutViewRecordsViewInfo.GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass;
begin
  Result := TcxGridLayoutViewRecordViewInfo;
end;

function TcxGridLayoutViewRecordsViewInfo.GetScrollableAreaBoundsForEdit: TRect;
begin
  Result := cxNullRect;
end;

function TcxGridLayoutViewRecordsViewInfo.IsCarouselMode: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewRecordsViewInfo.CanCenterRecords: Boolean;
begin
  Result := GridViewInfo.CalculateDown and (VisibleCount > 0) and
    GridView.OptionsView.CenterRecords and not GridViewInfo.IsHeightCalculating;
end;

function TcxGridLayoutViewRecordsViewInfo.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutViewRecordsViewInfo.GetControllerHelper: TcxGridLayoutViewControllerHelper;
begin
  Result := TcxGridLayoutViewControllerHelper(Controller.Helper);
end;

function TcxGridLayoutViewRecordsViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewRecordsViewInfo.GetGridViewInfo: TcxGridLayoutViewViewInfo;
begin
  Result := TcxGridLayoutViewViewInfo(inherited GridViewInfo);
end;

function TcxGridLayoutViewRecordsViewInfo.GetItem(Index: Integer): TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited Items[Index]);
end;

function TcxGridLayoutViewRecordsViewInfo.GetViewData: TcxGridLayoutViewViewData;
begin
  Result := TcxGridLayoutViewViewData(inherited ViewData);
end;

{ TcxGridLayoutViewSite }

function TcxGridLayoutViewSite.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

procedure TcxGridLayoutViewSite.UpdateSize;
begin
  inherited UpdateSize;
  GridView.UpdateSizeDefaultViewInfo;
end;

{ TcxGridLayoutViewViewInfo }

procedure TcxGridLayoutViewViewInfo.CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean);
begin
  FIsHeightCalculating := True;
  try
    inherited;
  finally
    FIsHeightCalculating := False;
  end;
end;

function TcxGridLayoutViewViewInfo.GetAllowBoundsChangedNotification: Boolean;
begin
  Result := (GridView.Root.AlignHorz = ahClient) or (GridView.Root.AlignVert = avClient) or
     inherited GetAllowBoundsChangedNotification;
end;

function TcxGridLayoutViewViewInfo.GetFirstRecordIndex: Integer;
var
  ALast: Integer;
begin
  if IsCarouselMode then
    RecordsViewInfo.CalculateVisibleRecordsBound(Result, ALast)
  else
    Result := inherited GetFirstRecordIndex;
end;

function TcxGridLayoutViewViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  if IsCarouselMode then
    Result := TcxGridLayoutViewCarouselModeRecordsViewInfo
  else
    Result := TcxGridLayoutViewRecordsViewInfo;
end;

function TcxGridLayoutViewViewInfo.GetScrollableAreaBoundsForEdit: TRect;
begin
  if IsCarouselMode then
    Result := RecordsViewInfo.GetScrollableAreaBoundsForEdit
  else
    Result := inherited GetScrollableAreaBoundsForEdit;
end;

function TcxGridLayoutViewViewInfo.GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass;
const
  SeparatorsViewInfoClasses: array[TcxGridLayoutViewViewMode] of TcxGridCustomLayoutViewSeparatorsViewInfoClass =
    (TcxGridLayoutViewSingleRecordSeparatorsViewInfo,
     TcxGridLayoutViewSingleRowRecordSeparatorsViewInfo,
     TcxGridLayoutViewMultiRowRecordSeparatorsViewInfo,
     TcxGridLayoutViewSingleColumnRecordSeparatorsViewInfo,
     TcxGridLayoutViewMultiColumnRecordSeparatorsViewInfo,
     TcxGridLayoutViewSeparatorsViewInfo); // todo:
begin
  Result := SeparatorsViewInfoClasses[GridView.OptionsView.ViewMode];
end;

function TcxGridLayoutViewViewInfo.GetSiteClass: TcxGridSiteClass;
begin
  Result := TcxGridLayoutViewSite;
end;

function TcxGridLayoutViewViewInfo.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TcxGridLayoutViewViewInfo.GetRecordCaptionHeight: Integer;
var
  AHeight: Integer;
  AParams: TcxViewParams;
begin
  if not GridView.OptionsView.RecordCaption.Visible then
    Exit(0);

  AHeight := cxGridLayoutViewRecordExpandButtonOffset * 2 + LookAndFeelPainter.ScaledGroupExpandButtonSize(ScaleFactor);
  dxAdjustToTouchableSize(AHeight, ScaleFactor);
  GridView.StylesAdapter.GetRecordCaptionParams(nil, AParams);
  AHeight := Max(AHeight, cxTextHeight(AParams.Font));

  Result := Max(AHeight, GridView.OptionsView.RecordCaption.Height);
end;

function TcxGridLayoutViewViewInfo.IsCarouselMode: Boolean;
begin
  Result := GridView.OptionsView.ViewMode = lvvmCarousel;
end;

function TcxGridLayoutViewViewInfo.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutViewViewInfo.GetGridView: TcxGridLayoutView;
begin
  Result := inherited GridView as TcxGridLayoutView;
end;

function TcxGridLayoutViewViewInfo.GetRecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  Result := TcxGridLayoutViewRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridLayoutViewItem }

destructor TcxGridLayoutViewItem.Destroy;
begin
  LayoutItem := nil;
  inherited Destroy;
end;

procedure TcxGridLayoutViewItem.Assign(Source: TPersistent);

  function FindItem: TcxGridLayoutItem;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to GridView.Container.AbsoluteItemCount - 1 do
      if (GridView.Container.AbsoluteItems[I] is TcxGridLayoutItem) and
        (TcxGridLayoutItem(GridView.Container.AbsoluteItems[I]).GridViewItem = Source) then
      begin
        Result := TcxGridLayoutItem(GridView.Container.AbsoluteItems[I]);
        Break;
      end;
  end;

var
  AItem: TcxGridLayoutItem;
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewItem then
  begin
    AItem := FindItem;
    if AItem <> nil then
      Self.LayoutItem := AItem;
  end;
end;

function TcxGridLayoutViewItem.CanAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewItem.CanFocus(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := inherited CanFocus(ARecord) and not GridView.IsAssigningItems and
    (LayoutItem <> nil) and LayoutItem.Enabled;
end;

procedure TcxGridLayoutViewItem.CaptionChanged;
begin
  if LayoutItem <> nil then
    LayoutItem.Caption := Caption;
  inherited CaptionChanged;
end;

procedure TcxGridLayoutViewItem.ChangeGroupIndex(Value: Integer);
begin
// do nothing
end;

function TcxGridLayoutViewItem.GetActuallyVisible: Boolean;
begin
  Result := inherited GetActuallyVisible;
end;

function TcxGridLayoutViewItem.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  Result := inherited GetStoredProperties(AProperties);
  AProperties.Add('Caption');
end;

procedure TcxGridLayoutViewItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Caption' then
    AValue := Caption
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TcxGridLayoutViewItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Caption' then
    Caption := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TcxGridLayoutViewItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLayoutItem) then
    FLayoutItem := nil;
end;

procedure TcxGridLayoutViewItem.SetGridView(Value: TcxCustomGridTableView);
var
  AParent: TdxLayoutGroup;
begin
  if GridView <> nil then
    LayoutItem := nil;
  inherited;
  if (GridView <> nil) and not GridView.IsLoading then
  begin
    if Visible then
      AParent := GridView.Container.Root
    else
      AParent := nil;
    LayoutItem := GridView.Container.CreateItem(TcxGridLayoutItem, AParent) as TcxGridLayoutItem;
  end;
end;

function TcxGridLayoutViewItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := PropertiesClass;
end;

function TcxGridLayoutViewItem.GetEditMinSize(AFont: TFont): TSize;
const
  cxGridLayoutViewEditMinWidth = 60;
var
  AEditStyle: TcxEditStyle;
  AEditSizeProperties: TcxEditSizeProperties;
  ACanvas: TcxCanvas;
begin
  AEditStyle := GetCellStyle;
  try
    AEditStyle.Font := AFont;
    AEditSizeProperties.Height := -1;
    AEditSizeProperties.MaxLineCount := 0;
    AEditSizeProperties.Width := -1;

    ACanvas := GridView.ViewInfo.Canvas;
    Result := EditingProperties.GetEditSize(ACanvas, AEditStyle, True, Null, AEditSizeProperties);
    if Result.cx <> 0 then
    begin
      Result.cx := Max(Result.cx, GridView.ScaleFactor.Apply(cxGridLayoutViewEditMinWidth));
      Inc(Result.cx, 2 * cxGridEditOffset);
    end;
    if Result.cy <> 0 then
      Inc(Result.cy, 2 * cxGridEditOffset);
  finally
    ReleaseCellStyle;
  end;
end;

function TcxGridLayoutViewItem.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewItem.GetOptions: TcxGridLayoutViewItemOptions;
begin
  Result := TcxGridLayoutViewItemOptions(inherited Options);
end;

function TcxGridLayoutViewItem.GetStyles: TcxGridLayoutViewItemStyles;
begin
  Result := TcxGridLayoutViewItemStyles(inherited Styles);
end;

function TcxGridLayoutViewItem.GetFocusedLayoutViewItemViewInfo: TcxGridLayoutViewItemViewInfo;
begin
  Result := TcxGridLayoutViewItemViewInfo(inherited FocusedCellViewInfo);
end;

function TcxGridLayoutViewItem.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridLayoutViewItemOptions;
end;

function TcxGridLayoutViewItem.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxGridLayoutViewItemStyles;
end;

procedure TcxGridLayoutViewItem.DoSetVisible(Value: Boolean);
begin
  inherited;
  if (LayoutItem <> nil) and ([csDestroying, csReading, csLoading] * GridView.ComponentState = []) and
    not GridView.IsAssigningItems then
    if Value then
      LayoutItem.Parent := LayoutItem.Container.Root
    else
      LayoutItem.Parent := nil;
end;

function TcxGridLayoutViewItem.IsFocusedCellViewInfoPartVisible: Boolean;
begin
  Result := inherited IsFocusedCellViewInfoPartVisible or
    not cxRectContain(FocusedCellViewInfo.VisibleBounds, FocusedCellViewInfo.EditBounds);
end;

function TcxGridLayoutViewItem.GetVisible: Boolean;
begin
  if (LayoutItem = nil) or ([csDestroying, csReading, csLoading] * GridView.ComponentState <> []) or
    GridView.IsAssigningItems then
    Result := inherited GetVisible
  else
    Result := not LayoutItem.IsAvailable;
end;

function TcxGridLayoutViewItem.GetVisibleCaption: string;
begin
  Result := CaptionToDisplayCaption(inherited GetVisibleCaption);
end;

function TcxGridLayoutViewItem.CaptionToDisplayCaption(const ACaption: string): string;
begin
  if GridView.OptionsView.CaptionSeparator <> #0 then
    Result := ACaption + GridView.OptionsView.CaptionSeparator
  else
    Result := ACaption;
end;

function TcxGridLayoutViewItem.DisplayCaptionToCaption(const ADisplayCaption: string): string;
begin
  Result := ADisplayCaption;
  if (GridView.OptionsView.CaptionSeparator <> #0) and (Length(Result) > 0) then
    Delete(Result, Length(Result), 1);
end;

procedure TcxGridLayoutViewItem.SetLayoutItem(const Value: TcxGridLayoutItem);
begin
  if FLayoutItem <> Value then
  begin
    if FLayoutItem <> nil then
    begin
      if FLayoutItem.Container.IsRestoring then
        FLayoutItem.RemoveFreeNotification(Self)
      else
        FLayoutItem.Free;
    end;
    FLayoutItem := Value;
    if FLayoutItem <> nil then
    begin
      FLayoutItem.GridViewItem := Self;
      FLayoutItem.FreeNotification(Self);
    end;
  end;
end;

procedure TcxGridLayoutViewItem.SetOptions(Value: TcxGridLayoutViewItemOptions);
begin
  inherited Options := Value;
end;

procedure TcxGridLayoutViewItem.SetStyles(Value: TcxGridLayoutViewItemStyles);
begin
  inherited Styles := Value;
end;

{ TcxGridLayoutViewRecord }

constructor TcxGridLayoutViewRecord.Create(
  AViewData: TcxCustomGridTableViewData; AIndex: Integer;
  const ARecordInfo: TcxRowInfo);
begin
  inherited Create(AViewData, AIndex, ARecordInfo);
  FCloneDataStream := TMemoryStream.Create;
end;

destructor TcxGridLayoutViewRecord.Destroy;
begin
  FreeAndNil(FCloneDataStream);
  inherited Destroy;
end;

function TcxGridLayoutViewRecord.GetViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited ViewInfo);
end;

function TcxGridLayoutViewRecord.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  if GridView.OptionsView.ViewMode = lvvmCarousel then
    Result := TcxGridLayoutViewCarouselModeRecordViewInfoCacheItem
  else
    Result := TcxGridLayoutViewRecordViewInfoCacheItem;
end;

function TcxGridLayoutViewRecord.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewRecord.GetExpanded: Boolean;
begin
  Result := inherited GetExpanded or not GridView.OptionsView.RecordCaption.Visible;
end;

{ TcxGridLayoutViewControllerHelper }

function TcxGridLayoutViewControllerHelper.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
const
  ResultMap: array[Boolean] of TcxMouseWheelScrollingKind = (mwskVertical, mwskHorizontal);
begin
  Result := ResultMap[GetIsRecordsScrollHorizontal];
end;

function TcxGridLayoutViewControllerHelper.GetScrollDelta: Integer;
begin
  if ViewInfo.RecordsViewInfo.Bands.VisibleCount > 0 then
    case ViewInfo.GetGridView.OptionsView.ViewMode of
      lvvmMultiRow: Result := ViewInfo.RecordsViewInfo.Bands.VisibleCount;
      lvvmMultiColumn: Result := ViewInfo.RecordsViewInfo.Bands[0].VisibleCount;
    else
      Result := 1;
    end
  else
    Result := 0;
end;

procedure TcxGridLayoutViewControllerHelper.KeyDown(var Key: Word; Shift: TShiftState);
var
  AKey: Word;
begin
  inherited;
  if ViewInfo.UseRightToLeftAlignment then
    AKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key)
  else
    AKey := Key;
  case AKey of
    VK_LEFT, VK_UP:
      Controller.GoToPrev(not (ssShift in Shift), False);
    VK_RIGHT, VK_DOWN:
      Controller.GoToNext(not (ssShift in Shift), False);
  end;
end;

procedure TcxGridLayoutViewControllerHelper.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  ALayoutContainerViewInfo: TcxGridLayoutContainerCloneViewInfo;
begin
  inherited;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if Controller.MouseCaptureViewInfo = nil then
    if AHitTest is TcxGridRecordHitTest then
    begin
      ALayoutContainerViewInfo := GetLayoutContainerViewInfo(TcxGridRecordHitTest(AHitTest));
      ALayoutContainerViewInfo.MouseMove(Shift, X, Y);
      ALayoutContainerViewInfo.MouseDown(Button, Shift, X, Y);
    end;
end;

procedure TcxGridLayoutViewControllerHelper.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  inherited;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if Controller.MouseCaptureViewInfo = nil then
    if AHitTest is TcxGridRecordHitTest then
      GetLayoutContainerViewInfo(TcxGridRecordHitTest(AHitTest)).MouseMove(Shift, X, Y);
end;

procedure TcxGridLayoutViewControllerHelper.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  inherited;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if Controller.MouseCaptureViewInfo = nil then
    if AHitTest is TcxGridRecordHitTest then
      GetLayoutContainerViewInfo(TcxGridRecordHitTest(AHitTest)).MouseUp(Button, Shift, X, Y);
end;

function TcxGridLayoutViewControllerHelper.CanScrollBarVisible(AKind: TScrollBarKind): Boolean;
begin
  Result := inherited CanScrollBarVisible(AKind) or NeedContentScrollBar;
end;

procedure TcxGridLayoutViewControllerHelper.DoInitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean);
begin
  if IsDataScrollBar(AKind) then
    inherited DoInitScrollBarParameters(AKind, ACanHide)
  else
  begin
    Controller.ContentScrollBarPosition := Controller.ContentScrollBarPosition;
    Controller.SetScrollBarInfo(AKind, 0, GetContentScrollSize - 1, 1,
      GetContentPageSize, Controller.ContentScrollBarPosition, True, ACanHide)
  end;
end;

function TcxGridLayoutViewControllerHelper.GetContentScrollSize: Integer;
begin
  Result := ViewInfo.RecordsViewInfo.RecordIndent * 2;
end;

function TcxGridLayoutViewControllerHelper.NeedContentScrollBar: Boolean;
begin
  Result := False;
end;

function TcxGridLayoutViewControllerHelper.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutViewControllerHelper.GetLayoutContainerViewInfo(AHitTest: TcxGridRecordHitTest): TcxGridLayoutContainerCloneViewInfo;
var
  ARecordViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  ARecordViewInfo := TcxGridLayoutViewRecordViewInfo(AHitTest.GridRecord.ViewInfo);
  Result := ARecordViewInfo.LayoutViewInfo;
end;

function TcxGridLayoutViewControllerHelper.GetViewInfo: TcxGridLayoutViewViewInfo;
begin
  Result := TcxGridLayoutViewViewInfo(inherited ViewInfo);
end;

{ TcxGridLayoutViewControllerHorizontalHelper }

function TcxGridLayoutViewControllerHorizontalHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbHorizontal;
end;

function TcxGridLayoutViewControllerHorizontalHelper.GetContentPageSize: Integer;
begin
  Result := cxRectHeight(Controller.GridView.ViewInfo.RecordsViewInfo.Bounds);
end;

function TcxGridLayoutViewControllerHorizontalHelper.GetContentScrollSize: Integer;
var
  ARecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  ARecordsViewInfo := Controller.GridView.ViewInfo.RecordsViewInfo;
  if (ARecordsViewInfo.FirstRecordIndex >= 0) and (ARecordsViewInfo.ViewData.RecordCount > 0) then
    Result := ARecordsViewInfo[0].CalculateHeight
  else
    Result := 0;
  Inc(Result, inherited GetContentScrollSize);
end;

{ TcxGridLayoutViewControllerVerticalHelper }

function TcxGridLayoutViewControllerVerticalHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbVertical;
end;

function TcxGridLayoutViewControllerVerticalHelper.GetContentPageSize: Integer;
begin
  Result := cxRectWidth(Controller.GridView.ViewInfo.RecordsViewInfo.Bounds);
end;

function TcxGridLayoutViewControllerVerticalHelper.GetContentScrollSize: Integer;
var
  ARecordsViewInfo: TcxGridLayoutViewRecordsViewInfo;
begin
  ARecordsViewInfo := Controller.GridView.ViewInfo.RecordsViewInfo;
  if (ARecordsViewInfo.FirstRecordIndex >= 0) and (ARecordsViewInfo.ViewData.RecordCount > 0) then
    Result := ARecordsViewInfo[0].CalculateWidth
  else
    Result := 0;
  Inc(Result, inherited GetContentScrollSize);
end;

{ TcxGridLayoutViewController }

function CompareItemsByTabOrder(AItem1, AItem2: Pointer): Integer;
begin
  Result := TcxGridLayoutViewItemViewInfo(AItem1).LayoutItemViewInfo.TabOrder -
    TcxGridLayoutViewItemViewInfo(AItem2).LayoutItemViewInfo.TabOrder;
end;

constructor TcxGridLayoutViewController.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FLayoutCustomizationFormHelper := TcxGridLayoutCustomizationFormHelper.Create(Self);
end;

destructor TcxGridLayoutViewController.Destroy;
begin
  FreeAndNil(FLayoutCustomizationFormHelper);
  inherited Destroy;
end;

function TcxGridLayoutViewController.GetCustomizationFormBounds: TRect;
begin
  Result := LayoutCustomizationFormHelper.GetCustomizationFormBounds;
end;

function TcxGridLayoutViewController.FindNextItem(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean;
  out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer;
begin
  Result := GetNextIndex(AFocusedItemIndex, AGoForward, AGoOnCycle, ACycleChanged);
end;

function TcxGridLayoutViewController.FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean;
var
  ACycleChanged: Boolean;
  AIndex: Integer;
begin
  Result := FocusedRecordHasCells(True);
  if Result then
  begin
    if FocusedItem = nil then
      AIndex := -1
    else
      AIndex := FocusedItem.VisibleIndex;
    AIndex := GetNextIndex(AIndex, AGoForward, AGoOnCycle, ACycleChanged);
    Result := AIndex <> -1;
    if Result then
      GridView.VisibleItems[AIndex].Focused := True;
  end;
end;

function TcxGridLayoutViewController.IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean;
begin
  Result := (GridView.OptionsView.ViewMode in [lvvmSingleRecord, lvvmSingleRow]) or
    inherited IsDataFullyVisible(AIsCallFromMaster);
end;

procedure TcxGridLayoutViewController.MakeItemVisible(AItem: TcxCustomGridTableItem);
var
  AViewInfo: TdxCustomLayoutItemViewInfo;
begin
  inherited;
  if (AItem <> nil) and (FocusedRecordViewInfo <> nil) and FocusedRecordViewInfo.LayoutViewInfo.FindItemViewInfo((AItem as TcxGridLayoutViewItem).LayoutItem, AViewInfo) then
    TdxCustomLayoutItemViewInfoAccess(AViewInfo).MakeFullyVisible;
end;

function TcxGridLayoutViewController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    Result := AHitTest.HitTestCode in [htExpandButton, htLayoutViewItem, htCell, htRecord];
end;

function TcxGridLayoutViewController.GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass;
begin
  case GridView.OptionsView.ViewMode of
    lvvmSingleRecord, lvvmSingleRow, lvvmMultiRow:
      Result := TcxGridLayoutViewControllerHorizontalHelper;
    lvvmCarousel:
      Result := TcxGridLayoutViewControllerCarouselHelper
  else
    Result := TcxGridLayoutViewControllerVerticalHelper;
  end;
end;

procedure TcxGridLayoutViewController.CheckFocusItem(AItemViewInfo: TcxGridLayoutViewItemViewInfo);

  function GetNextFocusedItem(AGroupViewInfo: TdxLayoutGroupViewInfo; out AFocusedItem: TcxGridLayoutViewItem): Boolean;
  var
    I: Integer;
  begin
    Result := TdxLayoutGroupViewInfoAccess(AGroupViewInfo).IsExpanded and TdxLayoutGroupViewInfoAccess(AGroupViewInfo).GetEnabled;
    if Result then
    begin
      Result := False;
      for I := 0 to AGroupViewInfo.ItemViewInfoCount - 1 do
      begin
        if AGroupViewInfo.ItemViewInfos[I] is TdxLayoutGroupViewInfo then
          Result := GetNextFocusedItem(AGroupViewInfo.ItemViewInfos[I] as TdxLayoutGroupViewInfo, AFocusedItem)
        else
          if (AGroupViewInfo.ItemViewInfos[I] is TcxGridLayoutItemViewInfo) and
            TcxGridLayoutItemViewInfo(AGroupViewInfo.ItemViewInfos[I]).GridItemViewInfo.CanFocus then
          begin
            Result := True;
            AFocusedItem := TcxGridLayoutItemViewInfo(AGroupViewInfo.ItemViewInfos[I]).GridItemViewInfo.Item;
          end;
        if Result then
          Break;
      end;
    end;
  end;

  function GetParentViewInfo(AItemViewInfo: TdxCustomLayoutItemViewInfo; out AParentViewInfo: TdxLayoutGroupViewInfo): Boolean;
  begin
    Result := AItemViewInfo.ParentViewInfo <> nil;
    if Result then
    begin
      Result := TdxCustomLayoutItemViewInfoAccess(AItemViewInfo.ParentViewInfo).IsExpanded and TdxCustomLayoutItemViewInfoAccess(AItemViewInfo.ParentViewInfo).ActuallyVisible;
      if Result then
        AParentViewInfo := AItemViewInfo.ParentViewInfo
      else
        Result := GetParentViewInfo(AItemViewInfo.ParentViewInfo, AParentViewInfo);
    end;
    if not Result then
      AParentViewInfo := nil;
  end;

var
  AFocusedItem: TcxGridLayoutViewItem;
  AParentViewInfo: TdxLayoutGroupViewInfo;
begin
  if (AItemViewInfo.Item = FocusedItem) and not AItemViewInfo.CanFocus then
  begin
    if AItemViewInfo.Item.Visible and (AItemViewInfo.LayoutItemViewInfo <> nil) and
        GetParentViewInfo(AItemViewInfo.LayoutItemViewInfo, AParentViewInfo) and GetNextFocusedItem(AParentViewInfo, AFocusedItem) then
      FocusedItem := AFocusedItem
    else
      if not FocusFirstAvailableItem then
        FocusedItem := nil;
  end;
end;

function TcxGridLayoutViewController.CreateCustomizationForm: TForm;
begin
  Result := TcxGridLayoutViewCustomizationForm.Create(nil);
end;

procedure TcxGridLayoutViewController.CustomizationChanged;
begin
  LayoutCustomizationFormHelper.CustomizationChanged;
end;

procedure TcxGridLayoutViewController.DoCreateCustomizationForm;
begin
  inherited DoCreateCustomizationForm;
  LayoutCustomizationFormHelper.InitializeCustomizationForm;
end;

function TcxGridLayoutViewController.CanCustomize: Boolean;
begin
  Result := True;
end;

procedure TcxGridLayoutViewController.CheckCustomizationFormBounds(var R: TRect);
begin
  LayoutCustomizationFormHelper.CheckCustomizationFormBounds(R);
end;

function TcxGridLayoutViewController.GetCustomizationFormDefaultHeight: Integer;
begin
  Result := cxGridLayoutViewCustomizationFormDefaultHeight;
end;

procedure TcxGridLayoutViewController.ScrollContentScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited ScrollContentScrollBar(AScrollCode, AScrollPos);
  case AScrollCode of
    scLineUp:
      ContentScrollBarPosition := ContentScrollBarPosition - 1;
    scLineDown:
      ContentScrollBarPosition := ContentScrollBarPosition + 1;
    scPageUp:
      ContentScrollBarPosition := ContentScrollBarPosition - Helper.GetContentPageSize;
    scPageDown:
      ContentScrollBarPosition := ContentScrollBarPosition + Helper.GetContentPageSize;
  else
    ContentScrollBarPosition := AScrollPos;
  end;
  AScrollPos := ContentScrollBarPosition;
end;

function TcxGridLayoutViewController.GetNextIndex(AFocusedIndex: Integer; AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): Integer;
var
  AList: TList;

  procedure PopulateItems(AList: TList);
  begin
    if GridView.IsDestroying then
      Exit;
    if FocusedRecordViewInfo <> nil then
      FocusedRecordViewInfo.PopulateTabOrderList(AList)
    else
      GridView.Container.ViewInfo.PopulateTabOrderList(AList);
  end;

  function CheckIndex(var AIndex: Integer): Boolean;
  begin
    Result := True;
    if AGoForward then
      if AIndex > AList.Count - 1 then
        if AGoOnCycle then
        begin
          AIndex := 0;
          ACycleChanged := True;
        end
        else
          Result := False
      else
    else
      if AIndex < 0 then
        if AGoOnCycle then
        begin
          AIndex := AList.Count - 1;
          ACycleChanged := True;
        end
        else
          Result := False;
  end;

begin
  AList := TList.Create;
  try
    PopulateItems(AList);
    if AList.Count = 0 then
      Result := -1
    else
    begin
      Result := AFocusedIndex;
      if Result = -1 then
      begin
        if AGoForward then
          Result := 0
        else
          if AGoOnCycle then
            Result := AList.Count - 1
          else
            Result := -1;
      end
      else
      begin
        Result := AList.IndexOf(GridView.VisibleItems[Result]);
        if AGoForward then
          Inc(Result)
        else
          Dec(Result);
        if not CheckIndex(Result) then
          Result := -1;
      end;
      if Result <> -1 then
        Result := TcxGridLayoutViewItem(AList[Result]).VisibleIndex;
    end;
  finally
    AList.Free;
  end;
end;

function TcxGridLayoutViewController.GetCustomizationFormDefaultWidth: Integer;
begin
  Result := cxGridLayoutViewCustomizationFormDefaultWidth;
end;

function TcxGridLayoutViewController.GetFocusedItem: TcxGridLayoutViewItem;
begin
  Result := TcxGridLayoutViewItem(inherited FocusedItem);
end;

function TcxGridLayoutViewController.GetFocusedRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  Result := TcxGridLayoutViewRecordViewInfo(inherited FocusedRecordViewInfo);
end;

function TcxGridLayoutViewController.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewController.GetHelper: TcxGridLayoutViewControllerHelper;
begin
  Result := TcxGridLayoutViewControllerHelper(inherited Helper);
end;

function TcxGridLayoutViewController.GetViewInfo: TcxGridLayoutViewViewInfo;
begin
  Result := TcxGridLayoutViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridLayoutViewController.SetContentScrollBarPosition(Value: Integer);
begin
  Value := Min(Helper.GetContentScrollSize - Helper.GetContentPageSize, Max(0, Value));
  if FContentScrollBarPosition <> Value then
  begin
    FContentScrollBarPosition := Value;
    if GridView.IsMaster then
      GridView.SizeChanged(True, True)
    else
      GridView.LayoutChanged;
  end;
end;

procedure TcxGridLayoutViewController.SetFocusedItem(Value: TcxGridLayoutViewItem);
begin
  inherited FocusedItem := Value;
end;

{ TcxGridLayoutViewOptionsSelection }

constructor TcxGridLayoutViewOptionsSelection.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FRecordBorderSelection := True;
end;

procedure TcxGridLayoutViewOptionsSelection.SetRecordBorderSelection(Value: Boolean);
begin
  if FRecordBorderSelection <> Value then
  begin
    FRecordBorderSelection := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewOptionsSelection.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridLayoutViewOptionsSelection then
    RecordBorderSelection := TcxGridLayoutViewOptionsSelection(Source).RecordBorderSelection;
end;

{ TcxGridLayoutViewOptionsBehavior }

constructor TcxGridLayoutViewOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FExpandRecordOnDblClick := True;
  FItemHotTrack := True;
end;

procedure TcxGridLayoutViewOptionsBehavior.SetExpandRecordOnDblClick(
  const Value: Boolean);
begin
  if FExpandRecordOnDblClick <> Value then
  begin
    FExpandRecordOnDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridLayoutViewOptionsBehavior.SetItemHotTrack(
  const Value: Boolean);
begin
  if FItemHotTrack <> Value then
  begin
    FItemHotTrack := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewOptionsBehavior.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewOptionsBehavior then
  begin
    ExpandRecordOnDblClick := TcxGridLayoutViewOptionsBehavior(Source).ExpandRecordOnDblClick;
    ItemHotTrack := TcxGridLayoutViewOptionsBehavior(Source).ItemHotTrack;
  end;
end;

{ TcxGridLayoutViewOptionsCustomize }

procedure TcxGridLayoutViewOptionsCustomize.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewOptionsCustomize then
    GroupExpanding := TcxGridLayoutViewOptionsCustomize(Source).GroupExpanding;
end;

function TcxGridLayoutViewOptionsCustomize.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewOptionsCustomize.GetGroupExpanding: Boolean;
begin
  Result := cfmiExpandButton in GridView.Container.MenuItems;
end;

function TcxGridLayoutViewOptionsCustomize.GetRecordExpanding: Boolean;
begin
  Result := Expandable;
end;

procedure TcxGridLayoutViewOptionsCustomize.SetGroupExpanding(const Value: Boolean);
begin
  with GridView.Container do
  begin
    if Value then
      MenuItems := MenuItems + [cfmiExpandButton]
    else
      MenuItems := MenuItems - [cfmiExpandButton];
  end;
end;

procedure TcxGridLayoutViewOptionsCustomize.SetRecordExpanding(Value: Boolean);
begin
  Expandable := Value;
end;

{ TcxGridLayoutViewCarouselMode }

constructor TcxGridLayoutViewCarouselMode.Create(AOptionsView: TcxGridLayoutViewOptionsView);
begin
  inherited Create(AOptionsView.GridView);
  FAnimatedDelay := cxGridLayoutViewCarouselModeDefaultAnimatedDelay;
  FBackgroundRecordEndScale := cxGridLayoutViewCarouselModeDefaultBackgroundRecordEndScale;
  FBackgroundRecordStartScale := cxGridLayoutViewCarouselModeDefaultBackgroundRecordStartScale;
  FInterpolationMode := imDefault;
  FPitchAngle := cxGridLayoutViewCarouselModeDefaultPitchAngle;
  FRecordCount := cxGridLayoutViewCarouselModeDefaultRecordCount;
  FRollAngle := cxGridLayoutViewCarouselModeDefaultRollAngle;
end;

procedure TcxGridLayoutViewCarouselMode.Assign(Source: TPersistent);
begin
  if Source is TcxGridLayoutViewCarouselMode then
  begin
    AnimationInterval := TcxGridLayoutViewCarouselMode(Source).AnimationInterval;
    AutoPitchAngle := TcxGridLayoutViewCarouselMode(Source).AutoPitchAngle;
    BackgroundRecordAlphaLevel := TcxGridLayoutViewCarouselMode(Source).BackgroundRecordAlphaLevel;
    BackgroundRecordEndScale := TcxGridLayoutViewCarouselMode(Source).BackgroundRecordEndScale;
    BackgroundRecordStartScale := TcxGridLayoutViewCarouselMode(Source).BackgroundRecordStartScale;
    InterpolationMode := TcxGridLayoutViewCarouselMode(Source).InterpolationMode;
    PitchAngle := TcxGridLayoutViewCarouselMode(Source).PitchAngle;
    Radius := TcxGridLayoutViewCarouselMode(Source).Radius;
    RecordCount := TcxGridLayoutViewCarouselMode(Source).RecordCount;
    RollAngle := TcxGridLayoutViewCarouselMode(Source).RollAngle;
  end;
  inherited Assign(Source);
end;

procedure TcxGridLayoutViewCarouselMode.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  if Radius > 0 then
    Radius := Max(MulDiv(Radius, M, D), 1);
end;

function TcxGridLayoutViewCarouselMode.GetDeltaAngle: Double;
begin
  Result := 2 * Pi / RecordCount;
end;

function TcxGridLayoutViewCarouselMode.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewCarouselMode.IsPitchAngleStored: Boolean;
begin
  Result := PitchAngle <> cxGridLayoutViewCarouselModeDefaultPitchAngle
end;

function TcxGridLayoutViewCarouselMode.IsRollAngleStored: Boolean;
begin
  Result := RollAngle <> cxGridLayoutViewCarouselModeDefaultRollAngle;
end;

procedure TcxGridLayoutViewCarouselMode.SetAutoPitchAngle(Value: Boolean);
begin
  if FAutoPitchAngle <> Value then
  begin
    FAutoPitchAngle := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetBackgroundRecordAlphaLevel(Value: Integer);
begin
  if FBackgroundRecordAlphaLevel <> Value then
  begin
    FBackgroundRecordAlphaLevel := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetBackgroundRecordEndScale(Value: Cardinal);
begin
  if (BackgroundRecordEndScale <> Value) and (Value <= 100) then
  begin
    FBackgroundRecordEndScale := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetBackgroundRecordStartScale(const Value: Cardinal);
begin
  if (BackgroundRecordStartScale <> Value) and (Value <= 100) then
  begin
    FBackgroundRecordStartScale := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetInterpolationMode(Value: TdxGPInterpolationMode);
begin
  if InterpolationMode <> Value then
  begin
    FInterpolationMode := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetPitchAngle(Value: Double);
begin
  if Value > 180 then
    Value := 180;
  if Value < 0 then
    Value := 0;
  if FPitchAngle <> Value then
  begin
    FPitchAngle := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetRadius(Value: Cardinal);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetRecordCount(Value: Integer);
begin
  if (FRecordCount <> Value) and (Value > 0) then
  begin
    FRecordCount := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewCarouselMode.SetRollAngle(Value: Double);
begin
  if Value > 360 then
    Value := 360;
  if Value < 0 then
    Value := 0;
  if FRollAngle <> Value then
  begin
    FRollAngle := Value;
    Changed(vcLayout);
  end;
end;

{ TcxGridLayoutViewRecordCaption }

constructor TcxGridLayoutViewRecordCaption.Create(
  AOptionsView: TcxGridLayoutViewOptionsView);
begin
  inherited Create(AOptionsView.GridView);
  FExpandButtonAlignment := rebaRight;
  FHeight := 0;
  FTextAlignmentHorz := taLeftJustify;
  FTextAlignmentVert := vaCenter;
  FVisible := True;
  DisplayMask := cxGetResourceString(@scxGridLayoutViewRecordCaptionDefaultMask);
end;

procedure TcxGridLayoutViewRecordCaption.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewRecordCaption then
  begin
    DisplayMask := TcxGridLayoutViewRecordCaption(Source).DisplayMask;
    ExpandButtonAlignment := TcxGridLayoutViewRecordCaption(Source).ExpandButtonAlignment;
    Height := TcxGridLayoutViewRecordCaption(Source).Height;
    TextAlignmentHorz := TcxGridLayoutViewRecordCaption(Source).TextAlignmentHorz;
    TextAlignmentVert := TcxGridLayoutViewRecordCaption(Source).TextAlignmentVert;
    Visible := TcxGridLayoutViewRecordCaption(Source).Visible;
  end;
end;

function TcxGridLayoutViewRecordCaption.GetFormattedText(
  AGridRecord: TcxGridLayoutViewRecord): string;
var
  ARecordIndex, ARecordCount: string;
begin
  if AGridRecord = nil then
  begin
    Result := '';
    Exit;
  end;
  if IsNeedRecordIndex then
    ARecordIndex := IntToStr(AGridRecord.Index + 1)
  else
    ARecordIndex := '';
  if IsNeedRecordCount then
    ARecordCount := IntToStr(AGridRecord.DataController.RowCount)
  else
    ARecordCount := '';
  Result := Format(FFormatString, [ARecordIndex, ARecordCount]);
end;

procedure TcxGridLayoutViewRecordCaption.BuildFormatString;
begin
  FFormatString := cxGetNavigatorFormatString(FDisplayMask, FIsNeedRecordIndex, FIsNeedRecordCount);
end;

procedure TcxGridLayoutViewRecordCaption.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  if Height > 0 then
    Height := Max(MulDiv(Height, M, D), 1);
end;

procedure TcxGridLayoutViewRecordCaption.SetDisplayMask(const Value: string);
begin
  if FDisplayMask <> Value then
  begin
    FDisplayMask := Value;
    BuildFormatString;
    Changed(vcLayout);
  end;
end;

function TcxGridLayoutViewRecordCaption.IsDisplayMaskStored: Boolean;
begin
  Result := scxGridLayoutViewRecordCaptionDefaultMask <> DisplayMask;
end;

procedure TcxGridLayoutViewRecordCaption.SetExpandButtonAlignment(
  Value: TcxGridLayoutViewRecordExpandButtonAlignment);
begin
  if FExpandButtonAlignment <> Value then
  begin
    FExpandButtonAlignment := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewRecordCaption.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewRecordCaption.SetTextAlignmentHorz(
  Value: TAlignment);
begin
  if FTextAlignmentHorz <> Value then
  begin
    FTextAlignmentHorz := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewRecordCaption.SetTextAlignmentVert(
  Value: TcxAlignmentVert);
begin
  if FTextAlignmentVert <> Value then
  begin
    FTextAlignmentVert := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewRecordCaption.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(vcSize);
  end;
end;

{ TcxGridLayoutViewOptionsView }

constructor TcxGridLayoutViewOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FCenterRecords := True;
  FMinValueWidth := cxGridLayoutViewDefaultMinValueWidth;
  FRecordBorderWidth := cxGridLayoutViewDefaultRecordBorderWidth;
  FCarouselMode := GetCarouselModeClass.Create(Self);
  FRecordCaption := GetRecordCaptionClass.Create(Self);
  CreateRecordSize;
  FShowOnlyEntireRecords := True;
  FViewMode := lvvmSingleRecord;
end;

destructor TcxGridLayoutViewOptionsView.Destroy;
begin
  DestroyRecordSize;
  FreeAndNil(FRecordCaption);
  FreeAndNil(FCarouselMode);
  inherited Destroy;
end;

procedure TcxGridLayoutViewOptionsView.Assign(Source: TPersistent);
var
  ASource: TcxGridLayoutViewOptionsView;
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewOptionsView then
  begin
    ASource := TcxGridLayoutViewOptionsView(Source);
    CarouselMode := ASource.CarouselMode;
    CenterRecords := ASource.CenterRecords;
    ItemPadding := ASource.ItemPadding;
    MaxColumnCount := ASource.MaxColumnCount;
    MaxRowCount := ASource.MaxRowCount;
    MinValueWidth := ASource.MinValueWidth;
    RecordBorderWidth := ASource.RecordBorderWidth;
    RecordCaption := ASource.RecordCaption;
    RecordSize := ASource.RecordSize;
    ShowOnlyEntireRecords := ASource.ShowOnlyEntireRecords;
    SingleRecordStretch := ASource.SingleRecordStretch;
    ViewMode := ASource.ViewMode;
  end;
end;

function TcxGridLayoutViewOptionsView.CanRecordScrollingHorizontal: Boolean;
begin
  Result := (RecordSize.Width > 0) or NeedStretchRecordWidth;
end;

function TcxGridLayoutViewOptionsView.CanRecordScrollingVertical: Boolean;
begin
  Result := (RecordSize.Height > 0) or NeedStretchRecordHeight;
end;

procedure TcxGridLayoutViewOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  CarouselMode.ChangeScale(M, D);
  RecordBorderWidth := MulDiv(RecordBorderWidth, M, D);
  MinValueWidth := MulDiv(MinValueWidth, M, D);
  RecordCaption.ChangeScale(M, D);
  RecordSize.ChangeScale(M, D);
end;

function TcxGridLayoutViewOptionsView.GetCarouselModeClass: TcxGridLayoutViewCarouselModeClass;
begin
  Result := TcxGridLayoutViewCarouselMode;
end;

function TcxGridLayoutViewOptionsView.GetRecordCaptionClass: TcxGridLayoutViewRecordHeaderClass;
begin
  Result := TcxGridLayoutViewRecordCaption;
end;

function TcxGridLayoutViewOptionsView.NeedStretchRecordHeight: Boolean;
begin
  Result := (ViewMode = lvvmSingleRecord) and (SingleRecordStretch in [srsVertical, srsClient]);
end;

function TcxGridLayoutViewOptionsView.NeedStretchRecordWidth: Boolean;
begin
  Result := (ViewMode = lvvmSingleRecord) and (SingleRecordStretch in [srsHorizontal, srsClient]);
end;

function TcxGridLayoutViewOptionsView.GetRecordIndent: Integer;
begin
  Result := inherited Indent;
end;

procedure TcxGridLayoutViewOptionsView.SetCarouselMode(Value: TcxGridLayoutViewCarouselMode);
begin
  FCarouselMode.Assign(Value);
end;

function TcxGridLayoutViewOptionsView.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

function TcxGridLayoutViewOptionsView.GetItemPadding: TcxLayoutViewLookAndFeelItemPadding;
begin
  Result := TcxLayoutViewLookAndFeelItemPadding(GridView.Container.LayoutLookAndFeel.ItemOptions.Padding);
end;

procedure TcxGridLayoutViewOptionsView.SetCenterRecords(Value: Boolean);
begin
  if FCenterRecords <> Value then
  begin
    FCenterRecords := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetItemPadding(
  const Value: TcxLayoutViewLookAndFeelItemPadding);
begin
  GridView.Container.LayoutLookAndFeel.ItemOptions.Padding.Assign(Value);
end;

procedure TcxGridLayoutViewOptionsView.SetRecordSize(AValue: TcxSize);
begin
  RecordSize.Assign(AValue);
end;

procedure TcxGridLayoutViewOptionsView.SetMaxColumnCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxColumnCount <> Value then
  begin
    FMaxColumnCount := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetMaxRowCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxRowCount <> Value then
  begin
    FMaxRowCount := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetMinValueWidth(Value: Integer);
begin
  Value := Max(Value, 8);
  if FMinValueWidth <> Value then
  begin
    FMinValueWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetRecordBorderWidth(Value: Integer);
begin
  if FRecordBorderWidth <> Value then
  begin
    FRecordBorderWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetRecordCaption(Value: TcxGridLayoutViewRecordCaption);
begin
  FRecordCaption.Assign(Value);
end;

procedure TcxGridLayoutViewOptionsView.SetRecordIndent(Value: Integer);
begin
  inherited Indent := Value;
end;

procedure TcxGridLayoutViewOptionsView.SetShowOnlyEntireRecords(Value: Boolean);
begin
  if FShowOnlyEntireRecords <> Value then
  begin
    FShowOnlyEntireRecords := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetSingleRecordStretch(Value: TcxGridLayoutViewSingleRecordStretch);
begin
  if FSingleRecordStretch <> Value then
  begin
    FSingleRecordStretch := Value;
    GridView.Container.UpdateRootOptions;
  end;
end;

procedure TcxGridLayoutViewOptionsView.SetViewMode(Value: TcxGridLayoutViewViewMode);
begin
  if FViewMode <> Value then
  begin
    FViewMode := Value;
    GridView.Controller.LayoutDirectionChanged;
    GridView.Container.UpdateRootOptions;
    GridView.DataChanged;
//    Changed(vcLayout);
  end;
end;

procedure TcxGridLayoutViewOptionsView.CreateRecordSize;
begin
  FRecordSize := TcxSize.Create(nil);
  FRecordSize.OnChange := RecordSizeChangeHandler;
end;

procedure TcxGridLayoutViewOptionsView.DestroyRecordSize;
begin
  FreeAndNil(FRecordSize);
end;

procedure TcxGridLayoutViewOptionsView.RecordSizeChangeHandler;
begin
  GridView.Container.UpdateRootOptions;
end;

{ TcxGridLayoutViewStyles }

constructor TcxGridLayoutViewStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
end;

procedure TcxGridLayoutViewStyles.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridLayoutViewStyles then
    with TcxGridLayoutViewStyles(Source) do
    begin
      Self.Group := Group;
      Self.Item := Item;
      Self.ItemHottrack := ItemHottrack;
      Self.RecordBorder := RecordBorder;
      Self.RecordCaption := RecordCaption;
      Self.OnGetGroupStyle := OnGetGroupStyle;
      Self.OnGetItemStyle := OnGetItemStyle;
      Self.OnGetRecordBorderStyle := OnGetRecordBorderStyle;
      Self.OnGetRecordCaptionStyle := OnGetRecordCaptionStyle;
    end;
end;

function TcxGridLayoutViewStyles.GetBackgroundBitmapIndex(
  Index: Integer): Integer;
begin
  Result := -1;
end;

function TcxGridLayoutViewStyles.GetDefaultBitmap(Index: Integer): TBitmap;
begin
  Index := GetBackgroundBitmapIndex(Index);
  if Index = -1 then
    Result := nil
  else
    Result := GridView.BackgroundBitmaps.GetBitmap(Index);
end;

procedure TcxGridLayoutViewStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  with AParams, LookAndFeelPainter do
    case Index of
      vsRecordCaption:
        begin
          Color := DefaultLayoutViewCaptionColor(cxbsNormal);
          TextColor := DefaultLayoutViewContentTextColor(cxbsNormal);
        end;
      vsRecordBorder:
        begin
          Color := DefaultLayoutViewContentColor;
          TextColor := DefaultLayoutViewContentTextColor(cxbsNormal);
        end;
      vsItem:
        TextColor := DefaultLayoutViewContentTextColor(cxbsNormal);
      vsItemHottrack:
        TextColor := DefaultLayoutViewContentTextColor(cxbsHot);
      vsGroup:
        begin
          TextColor := clDefault;
          Color := clDefault;
        end;
      vsSelection:
        if IsRecordCaptionParams then
        begin
          Color := DefaultLayoutViewCaptionColor(cxbsHot);
          TextColor := DefaultLayoutViewCaptionTextColor(cxgpTop, cxbsPressed);
        end
        else
          TextColor := DefaultLayoutViewContentTextColor(cxbsPressed);
      vsInactive:
        if IsRecordCaptionParams then
        begin
          Color := DefaultLayoutViewCaptionColor(cxbsDisabled);
          TextColor := DefaultLayoutViewCaptionTextColor(cxgpTop, cxbsDisabled);
        end
        else
          TextColor := DefaultLayoutViewContentTextColor(cxbsDisabled);
    end;
  AParams.Bitmap := GetDefaultBitmap(Index);
end;

function TcxGridLayoutViewStyles.GetGridViewValue: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GridView);
end;

procedure TcxGridLayoutViewStyles.GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetGroupStyle) then
    FOnGetGroupStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsGroup, ADataCellPos, AStyle, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridLayoutViewStyles.GetItemParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetItemStyle) then
    FOnGetItemStyle(GridView, ARecord, AItem, AStyle);

  if GridView.OptionsBehavior.ItemHotTrack and (AItem <> nil) and
    GridView.DrawDataCellSelected(ARecord, AItem, False, nil) then
    GetSelectionParams(ARecord, AItem, AParams)
  else
  begin
    ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
    try
      GetViewParams(vsItem, ADataCellPos, AStyle, AParams);
    finally
      ADataCellPos.Free;
    end;
  end;
end;

procedure TcxGridLayoutViewStyles.GetItemHottrackParams(
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  GetViewParams(vsItemHottrack, AItem, nil, AParams);
end;

procedure TcxGridLayoutViewStyles.GetRecordBorderParams(
  ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetRecordBorderStyle) then
    FOnGetRecordBorderStyle(GridView, ARecord, AStyle);
  GetViewParams(vsRecordBorder, ARecord, AStyle, AParams);
end;

procedure TcxGridLayoutViewStyles.GetRecordCaptionParams(
  ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetRecordCaptionStyle) then
    FOnGetRecordCaptionStyle(GridView, ARecord, AStyle);
  FIsRecordCaptionParams := True;
  try
    if (ARecord <> nil) and GridView.DrawRecordBorderSelected(ARecord) then
      GetSelectionParams(ARecord, nil, AParams)
    else
      GetViewParams(vsRecordCaption, ARecord, AStyle, AParams);
  finally
    FIsRecordCaptionParams := False;
  end;
end;

procedure TcxGridLayoutViewStyles.GetRecordBorderVisualParams(
  ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
begin
  if GridView.OptionsSelection.RecordBorderSelection and
    GridView.DrawRecordBorderSelected(ARecord) then
    GetSelectionParams(ARecord, nil, AParams)
  else
    GetRecordBorderParams(ARecord, AParams);
end;

procedure TcxGridLayoutViewStyles.SetOnGetItemStyle(
  Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetItemStyle, Value) then
  begin
    FOnGetItemStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridLayoutViewStyles.SetOnGetRecordBorderStyle(Value: TcxGridGetRecordStyleEvent);
begin
  if not dxSameMethods(FOnGetRecordBorderStyle, Value) then
  begin
    FOnGetRecordBorderStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridLayoutViewStyles.SetOnGetRecordCaptionStyle(Value: TcxGridGetRecordStyleEvent);
begin
  if not dxSameMethods(FOnGetRecordCaptionStyle, Value) then
  begin
    FOnGetRecordCaptionStyle := Value;
    GridView.Changed(vcSize);
  end;
end;

{ TcxGridLayoutViewStyleSheet }

class function TcxGridLayoutViewStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxGridLayoutViewStyles;
end;

function TcxGridLayoutViewStyleSheet.GetStylesValue: TcxGridLayoutViewStyles;
begin
  Result := TcxGridLayoutViewStyles(GetStyles);
end;

procedure TcxGridLayoutViewStyleSheet.SetStylesValue(
  Value: TcxGridLayoutViewStyles);
begin
  SetStyles(Value);
end;

{ TcxGridLayoutViewStylesAdapter }

constructor TcxGridLayoutViewStylesAdapter.Create(AGridView: TcxGridLayoutView);
begin
  inherited Create;
  FGridView := AGridView;
end;

procedure TcxGridLayoutViewStylesAdapter.GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if Helper <> nil then
    Helper.GetContentParams(ARecord, AItem, AParams)
  else
    Styles.GetContentParams(ARecord, AItem, AParams);
end;

procedure TcxGridLayoutViewStylesAdapter.GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if Helper <> nil then
    Helper.GetGroupParams(ARecord, AItem, AParams)
  else
    Styles.GetGroupParams(ARecord, AItem, AParams);
end;

procedure TcxGridLayoutViewStylesAdapter.GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if Helper <> nil then
    Helper.GetItemParams(ARecord, AItem, AParams)
  else
    Styles.GetItemParams(ARecord, AItem, AParams);
end;

procedure TcxGridLayoutViewStylesAdapter.GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
begin
  if Helper <> nil then
    Helper.GetRecordCaptionParams(ARecord, AParams)
  else
    Styles.GetRecordCaptionParams(ARecord, AParams);
end;

function TcxGridLayoutViewStylesAdapter.GetStyles: TcxGridLayoutViewStyles;
begin
  Result := GridView.Styles;
end;

{ TcxGridLayoutView }

constructor TcxGridLayoutView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCloneData := TMemoryStream.Create;
  FStylesAdapter := TcxGridLayoutViewStylesAdapter.Create(Self);
  FLayoutLookAndFeel := CreateLayoutLookAndFeel;

  CreateLayoutContainer;
end;

constructor TcxGridLayoutView.CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True);
begin
  Create(AControl);
  SetControl(AControl);
end;

destructor TcxGridLayoutView.Destroy;
begin
  DestroyLayoutContainer;
  FreeAndNil(FLayoutLookAndFeel);
  FreeAndNil(FStylesAdapter);
  FreeAndNil(FDefaultCloneData);
  inherited Destroy;
end;

function TcxGridLayoutView.CreateItem: TcxGridLayoutViewItem;
begin
  Result := inherited CreateItem as TcxGridLayoutViewItem;
end;

function TcxGridLayoutView.SizeChanged(AUpdateSelfOnly: Boolean = False;
  AKeepMaster: Boolean = False): Boolean;
begin
  IsDefaultViewInfoCalculated := False;
  Result := inherited SizeChanged(AUpdateSelfOnly, AKeepMaster);
end;

function TcxGridLayoutView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridLayoutViewOptionsBehavior;
end;

function TcxGridLayoutView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxGridLayoutViewOptionsCustomize;
end;

function TcxGridLayoutView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxGridLayoutViewOptionsData;
end;

function TcxGridLayoutView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxGridLayoutViewOptionsSelection;
end;

function TcxGridLayoutView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridLayoutViewOptionsView;
end;

procedure TcxGridLayoutView.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  if Root = Owner then
    Container.StoreChildren(Proc);
end;

function TcxGridLayoutView.CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
  ACellViewInfo: TcxGridTableCellViewInfo): Boolean;
begin
//  if TcxGridCardViewRow(AItem).Kind = rkCaption then
//    Result := inherited CalculateDataCellSelected(ARecord, AItem, AUseViewInfo, ACellViewInfo)
//  else
    Result := DrawRecordFocused(ARecord) and AItem.Focused;
end;

function TcxGridLayoutView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridLayoutViewController;
end;

function TcxGridLayoutView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDataController;
end;

function TcxGridLayoutView.GetDateTimeHandling: TcxGridLayoutViewDateTimeHandling;
begin
  Result := TcxGridLayoutViewDateTimeHandling(inherited DateTimeHandling);
end;

function TcxGridLayoutView.GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass;
begin
  Result := TcxGridLayoutViewDateTimeHandling;
end;

function TcxGridLayoutView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridLayoutViewPainter;
end;

function TcxGridLayoutView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridLayoutViewViewData;
end;

function TcxGridLayoutView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridLayoutViewViewInfo;
end;

procedure TcxGridLayoutView.SetControl(Value: TcxControl);
begin
  if (Control <> Value) and (Container <> nil) then
    Container.UnregisterControlFont;
  inherited SetControl(Value);
 // FLayoutLookAndFeel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TcxGridLayoutView.SetDateTimeHandling(Value: TcxGridLayoutViewDateTimeHandling);
begin
  inherited DateTimeHandling := Value;
end;

procedure TcxGridLayoutView.SetFiltering(Value: TcxGridLayoutViewFiltering);
begin
  inherited Filtering := Value;
end;

procedure TcxGridLayoutView.SetIsDefaultViewInfoCalculated(AValue: Boolean);
begin
  FIsDefaultViewInfoCalculated := AValue;
  if DefaultLayoutViewInfo <> nil then
    DefaultLayoutViewInfo.FIsValid := FIsDefaultViewInfoCalculated;
end;

function TcxGridLayoutView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridLayoutViewItem;
end;

procedure TcxGridLayoutView.AfterRestoring;
begin
  Container.IsRestoring := False;
  Container.EndUpdate;
  inherited AfterRestoring;
end;

procedure TcxGridLayoutView.BeforeRestoring;
begin
  Container.BeginUpdate;
  Container.IsRestoring := True;
  inherited BeforeRestoring;
end;

function TcxGridLayoutView.HandleSetStoredPropertyValueError(Sender: TcxStorage; const AName: string; const AValue: Variant): Boolean;
const
  SortOrderMap: array[0..2] of TdxSortOrder = (soNone, soAscending, soDescending);
begin
  Result := (Sender.StoredObject is TcxGridLayoutViewItem) and (AName = 'SortOrder');
  if Result then
    TcxGridLayoutViewItem(Sender.StoredObject).SortOrder := SortOrderMap[Integer(AValue)];
end;

procedure TcxGridLayoutView.BiDiModeChanged;
begin
  IsDefaultViewInfoCalculated := False;
  inherited BiDiModeChanged;
end;

procedure TcxGridLayoutView.ChangeScale(M, D: Integer);
begin
  Container.BeginUpdate;
  try
    inherited ChangeScale(M, D);
    Container.ChangeScale(M, D);
    LayoutLookAndFeel.ChangeScale(M, D);
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TcxGridLayoutView.DoAssign(ASource: TcxCustomGridView);
var
  ALayoutViewSource: TcxGridLayoutView;
begin
  Container.BeginUpdate;
  try
    if ASource is TcxGridLayoutView then
    begin
      ALayoutViewSource := TcxGridLayoutView(ASource);
      LayoutLookAndFeel.Assign(ALayoutViewSource.LayoutLookAndFeel);
      if not AssigningSettings then
        CopyLayoutStructure(ALayoutViewSource.Container);
      IsDefaultViewInfoCalculated := False;
      OnCustomDrawRecordCaption := ALayoutViewSource.OnCustomDrawRecordCaption;
    end;
    inherited DoAssign(ASource);
  finally
    Container.EndUpdate;
  end;
  CheckItemVisibles;
end;

procedure TcxGridLayoutView.DoStylesChanged;
begin
  inherited DoStylesChanged;
  UpdateContainerStyles(nil, FLayoutLookAndFeel);
end;

procedure TcxGridLayoutView.GetStoredChildren(AChildren: TStringList);
begin
  inherited GetStoredChildren(AChildren);
  AChildren.AddObject('Layout', Container);
end;

procedure TcxGridLayoutView.LookAndFeelChanged;
begin
  inherited;
  UpdateContainerStyles(nil, FLayoutLookAndFeel);
end;

procedure TcxGridLayoutView.Init;
begin
  Container.CheckIndexes;
  Container.LayoutChanged;
  inherited Init;
end;

function TcxGridLayoutView.IsRecordPixelScrolling: Boolean;
begin
  Result := inherited IsRecordPixelScrolling and not OptionsView.ShowOnlyEntireRecords and
    not (OptionsView.ViewMode in [lvvmSingleRecord, lvvmMultiRow, lvvmMultiColumn, lvvmCarousel]);
end;

function TcxGridLayoutView.ShowGridViewEditor: Boolean;
begin
  Result := True;
  Controller.Customization := True;
end;

procedure TcxGridLayoutView.AssignDefaultBounds(AViewInfo: TcxGridLayoutContainerCloneViewInfo);
begin
  if not IsDefaultViewInfoCalculated then
  begin
    AViewInfo.Calculate(True);
    DefaultLayoutViewInfo.RecreateViewInfos;
    DefaultLayoutViewInfo.AssignBounds(AViewInfo);
    IsDefaultViewInfoCalculated := True;
  end
  else
    AViewInfo.AssignBounds(DefaultLayoutViewInfo);
end;

procedure TcxGridLayoutView.RecalculateCloneData;
begin
  DefaultLayoutViewInfo.PrepareData(True);
  FDefaultCloneData.Size := DefaultLayoutViewInfo.GetCloneDataSize;
  FDefaultCloneData.Position := 0;
  DefaultLayoutViewInfo.SaveCloneData(FDefaultCloneData);
  IsDefaultViewInfoCalculated := False;
  ResetCloneData;
end;

procedure TcxGridLayoutView.ResetCalculatedLayoutViewInfo;
var
  I: Integer;
  AViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  for I := 0 to ViewInfo.RecordsViewInfo.Count - 1 do
  begin
    AViewInfo := ViewInfo.RecordsViewInfo[I];
    if AViewInfo <> nil then
      AViewInfo.ResetCalculated;
  end;
end;

procedure TcxGridLayoutView.ResetCloneData;
var
  I: Integer;
  AViewInfo: TcxGridLayoutViewRecordViewInfo;
begin
  for I := 0 to ViewInfo.RecordsViewInfo.Count - 1 do
  begin
    AViewInfo := ViewInfo.RecordsViewInfo[I];
    if AViewInfo <> nil then
      AViewInfo.ResetCloneData;
  end;
end;

procedure TcxGridLayoutView.UpdateContainerStyles(ARecord: TcxCustomGridRecord; var ALayoutLookAndFeel: TcxGridLayoutLookAndFeel);

  procedure UpdateCaptionOptions(AOptions: TdxLayoutLookAndFeelCaptionOptions; AParams: TcxViewParams);
  begin
    AOptions.TextColor := AParams.TextColor;
    AOptions.Font := AParams.Font;
  end;

var
  AParams: TcxViewParams;
begin
  ALayoutLookAndFeel.BeginUpdate;
  try
    ALayoutLookAndFeel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
    StylesAdapter.GetItemParams(ARecord, nil, AParams);
    UpdateCaptionOptions(ALayoutLookAndFeel.ItemOptions.CaptionOptions, AParams);
    StylesAdapter.GetGroupParams(ARecord, nil, AParams);
    UpdateCaptionOptions(ALayoutLookAndFeel.GroupOptions.CaptionOptions, AParams);
    ALayoutLookAndFeel.GroupOptions.Color := AParams.Color;
  finally
    ALayoutLookAndFeel.EndUpdate;
  end;
end;

function TcxGridLayoutView.GetLayoutContainer: TdxLayoutContainer;
begin
  Result := Container;
end;

procedure TcxGridLayoutView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
  BeginUpdate;
  try
    if ALayoutView is TcxGridLayoutView then
      CopyLayoutStructure(TcxGridLayoutView(ALayoutView).Container);
    inherited AssignLayout(ALayoutView);
  finally
    EndUpdate;
  end;
end;

function TcxGridLayoutView.GetLayoutCustomizationFormButtonCaption: string;
begin
  Result := cxGetResourceString(@scxGridLayoutViewCustomizeLayoutButtonCaption);
end;

procedure TcxGridLayoutView.CopyLayoutStructure(AContainer: TdxLayoutContainer);
begin
  SaveItemVisibles;
  Container.CustomizationHelper.CopyStructure(AContainer);
  CheckItemVisibles;
end;

function TcxGridLayoutView.CreateLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
begin
  Result := TcxGridLayoutLookAndFeel.Create(Self);
  Result.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  UpdateContainerStyles(nil, Result);
end;

function TcxGridLayoutView.GetLayoutContainerClass: TcxGridLayoutContainerClass;
begin
  Result := TcxGridLayoutContainer;
end;

function TcxGridLayoutView.GetLayoutGroupClass: TdxLayoutGroupClass;
begin
  Result := TdxLayoutGroup;
end;

procedure TcxGridLayoutView.UpdateSizeDefaultViewInfo;
begin
  IsDefaultViewInfoCalculated := not UseRightToLeftAlignment and IsDefaultViewInfoCalculated and
    not (OptionsView.NeedStretchRecordHeight or OptionsView.NeedStretchRecordWidth);
end;

function TcxGridLayoutView.GetDefaultFont: TFont;
begin
  if Control = nil then
    Result := TcxControlAccess(Site).Font
  else
    Result := Control.Font;
end;

procedure TcxGridLayoutView.SetStyles(Value: TcxGridLayoutViewStyles);
begin
  inherited Styles := Value;
end;

function TcxGridLayoutView.GetCanvas: TcxCanvas;
begin
  Result := ViewInfo.Canvas;
end;

function TcxGridLayoutView.GetClientBounds: TRect;
begin
  Result := ViewInfo.RecordsViewInfo.ContentBounds;
  Result := cxRectInflate(Result, -OptionsView.RecordBorderWidth, -OptionsView.RecordBorderWidth);
  Inc(Result.Top, ViewInfo.GetRecordCaptionHeight);
end;

function TcxGridLayoutView.GetClientRect: TRect;
begin
  Result := GetClientBounds;
  Result := cxRectOffset(Result, Result.TopLeft, False);
end;

function TcxGridLayoutView.GetContainer: TdxLayoutContainer;
begin
  Result := Container;
end;

function TcxGridLayoutView.GetFiltering: TcxGridLayoutViewFiltering;
begin
  Result := TcxGridLayoutViewFiltering(inherited Filtering);
end;

function TcxGridLayoutView.GetFilteringClass: TcxCustomGridTableFilteringClass;
begin
  Result := TcxGridLayoutViewFiltering;
end;

function TcxGridLayoutView.GetHighlightRoot: Boolean;
begin
  Result := False;
end;

procedure TcxGridLayoutView.CreateLayoutContainer;
begin
  FContainer := GetLayoutContainerClass.Create(Self);
  FContainer.BeginUpdate;
  FContainer.Initialize;
  FContainer.LayoutLookAndFeel := FLayoutLookAndFeel;
  FContainer.OnChanged := LayoutChangedHandler;
  FContainer.CancelUpdate;
  FDefaultLayoutViewInfo := TcxGridLayoutContainerCloneViewInfo.Create(Container, nil);
end;

procedure TcxGridLayoutView.DestroyLayoutContainer;
begin
  FreeAndNil(FDefaultLayoutViewInfo);
  FreeAndNil(FContainer);
end;

procedure TcxGridLayoutView.RecreateLayoutContainer;
begin
  DestroyLayoutContainer;
  CreateLayoutContainer;
end;

procedure TcxGridLayoutView.SetName(const NewName: TComponentName);
var
  AOldName: string;
begin
  AOldName := Name;
  inherited;
  Container.CheckItemNames(AOldName, Name);
end;

procedure TcxGridLayoutView.DoCustomDrawRecordHeader(ACanvas: TcxCanvas;
  AViewInfo: TcxGridLayoutViewRecordCaptionViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawRecordHeader then
    FOnCustomDrawRecordCaption(Self, ACanvas, AViewInfo, ADone);
end;

function TcxGridLayoutView.HasCustomDrawRecordHeader: Boolean;
begin
  Result := Assigned(FOnCustomDrawRecordCaption);
end;

function TcxGridLayoutView.DrawRecordBorderSelected(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := DrawRecordSelected(ARecord) and DrawSelection;
end;

function TcxGridLayoutView.GetControl: TcxCustomGrid;
begin
  Result := TcxCustomGrid(inherited Control);
end;

function TcxGridLayoutView.GetController: TcxGridLayoutViewController;
begin
  Result := TcxGridLayoutViewController(inherited Controller);
end;

function TcxGridLayoutView.GetDataController: TcxGridDataController;
begin
  Result := TcxGridDataController(FDataController);
end;

function TcxGridLayoutView.GetItem(Index: Integer): TcxGridLayoutViewItem;
begin
  Result := TcxGridLayoutViewItem(inherited Items[Index]);
end;

function TcxGridLayoutView.GetOptionsBehavior: TcxGridLayoutViewOptionsBehavior;
begin
  Result := TcxGridLayoutViewOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridLayoutView.GetOptionsCustomize: TcxGridLayoutViewOptionsCustomize;
begin
  Result := TcxGridLayoutViewOptionsCustomize(inherited OptionsCustomize);
end;

function TcxGridLayoutView.GetOptionsData: TcxGridLayoutViewOptionsData;
begin
  Result := TcxGridLayoutViewOptionsData(inherited OptionsData);
end;

function TcxGridLayoutView.GetOptionsSelection: TcxGridLayoutViewOptionsSelection;
begin
  Result := TcxGridLayoutViewOptionsSelection(inherited OptionsSelection);
end;

function TcxGridLayoutView.GetOptionsView: TcxGridLayoutViewOptionsView;
begin
  Result := TcxGridLayoutViewOptionsView(inherited OptionsView);
end;

function TcxGridLayoutView.GetRoot: TdxLayoutGroup;
begin
  Result := Container.Root;
end;

function TcxGridLayoutView.GetStyles: TcxGridLayoutViewStyles;
begin
  Result := TcxGridLayoutViewStyles(inherited Styles);
end;

function TcxGridLayoutView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridLayoutViewStyles;
end;

function TcxGridLayoutView.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
var
  AHitTest: TcxCustomGridHitTest;
  AContainerViewInfo: TcxGridLayoutContainerCloneViewInfo;
begin
  Result := nil;
  AContainerViewInfo := nil;
  AHitTest := ViewInfo.GetHitTest(APoint.X, APoint.Y);
  if Controller.MouseCaptureViewInfo = nil then
    if AHitTest is TcxGridRecordHitTest then
      AContainerViewInfo := Controller.Helper.GetLayoutContainerViewInfo(TcxGridRecordHitTest(AHitTest));
  if AContainerViewInfo <> nil then
    Result := AContainerViewInfo.GetTouchScrollUIOwner(APoint);
  if Result = nil then
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

function TcxGridLayoutView.GetViewInfo: TcxGridLayoutViewViewInfo;
begin
  Result := TcxGridLayoutViewViewInfo(inherited ViewInfo);
end;

function TcxGridLayoutView.GetVisibleItem(Index: Integer): TcxGridLayoutViewItem;
begin
  Result := TcxGridLayoutViewItem(inherited VisibleItems[Index]);
end;

procedure TcxGridLayoutView.LayoutChangedHandler(Sender: TObject);
begin
  IsDefaultViewInfoCalculated := False;
  Changed(vcSize);
end;

procedure TcxGridLayoutView.SetDataController(Value: TcxGridDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridLayoutView.SetItem(Index: Integer; Value: TcxGridLayoutViewItem);
begin
  inherited Items[Index] := Value;
end;

procedure TcxGridLayoutView.SetOnCustomDrawRecordCaption(Value: TcxGridLayoutViewCustomDrawRecordHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawRecordCaption, Value) then
  begin
    FOnCustomDrawRecordCaption := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridLayoutView.SetOptionsBehavior(Value: TcxGridLayoutViewOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxGridLayoutView.SetOptionsCustomize(Value: TcxGridLayoutViewOptionsCustomize);
begin
  inherited OptionsCustomize := Value;
end;

procedure TcxGridLayoutView.SetOptionsData(Value: TcxGridLayoutViewOptionsData);
begin
  inherited OptionsData := Value;
end;

procedure TcxGridLayoutView.SetOptionsSelection(Value: TcxGridLayoutViewOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TcxGridLayoutView.SetOptionsView(Value: TcxGridLayoutViewOptionsView);
begin
  inherited OptionsView := Value;
end;

initialization
  RegisterClasses([TcxGridLayoutViewItem, TcxGridBaseLayoutItem, TcxGridLayoutItem]);
  cxGridRegisteredViews.Register(TcxGridLayoutView, 'Layout');
  Classes.RegisterClasses([TcxGridLayoutView, TcxGridLayoutViewStyleSheet]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridLayoutView);

end.
