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

unit cxGridChartView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Messages, Classes, Graphics, Controls, Forms, Menus, StdCtrls,
  dxCore, dxCoreClasses, cxClasses, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  cxStyles, cxControls, cxStorage, cxEdit, cxCheckBox, cxCheckListBox, cxDropDownEdit,
  cxLabel, cxPC, cxVariants, cxCustomData, cxData, cxDataStorage, dxOffice11, dxUIElementPopupWindow,
  cxGridCommon, cxGridCustomView, dxGDIPlusClasses, dxGDIPlusApi, cxListBox;

const
  cxGridChartCustomizationFormDefaultWidth = 200;

  htChartBase = 50;
  htChartTitle = htChartBase + 1;
  htLegend = htChartBase + 2;
  htLegendItem = htChartBase + 3;
  htChartToolBox = htChartBase + 4;
  htDataLevelInfo = htChartBase + 5;
  htDataLevelActiveValueInfo = htChartBase + 6;
  htChartCustomizeButton = htChartBase + 7;
  htDiagramSelector = htChartBase + 8;
  htChartValue = htChartBase + 9;
  // histogram
  htPlot = htChartBase + 10;
  htCategoryAxisTitle = htChartBase + 11;
  htValueAxisTitle = htChartBase + 12;
  // line diagram
  htChartValueLine = htChartBase + 13;
  // area diagram
  htChartValueArea = htChartBase + 14;
  // pie diagram
  htSeriesSite = htChartBase + 15;
  htSeriesSiteCaption = htChartBase + 16;

  ckToolBox = 2;

  dsDiagramFirst = 0;
  dsLegend = dsDiagramFirst;
  dsValueCaptions = dsDiagramFirst + 1;
  dsValues = dsDiagramFirst + 2;
  dsAxis = dsDiagramFirst + 3;
  dsCategoryAxis = dsDiagramFirst + 4;
  dsValueAxis = dsDiagramFirst + 5;
  dsAxisTitle = dsDiagramFirst + 6;
  dsCategoryAxisTitle = dsDiagramFirst + 7;
  dsValueAxisTitle = dsDiagramFirst + 8;
  dsGridLines = dsDiagramFirst + 9;
  dsCategoryGridLines = dsDiagramFirst + 10;
  dsValueGridLines = dsDiagramFirst + 11;
  dsPlot = dsDiagramFirst + 12;
  dsValueMarkers = dsDiagramFirst + 13;
  dsSeriesSiteCaptions = dsDiagramFirst + 14;
  dsSeriesSites = dsDiagramFirst + 15;
  dsDiagramLast = dsSeriesSites;

  ssSeriesFirst = 0;
  ssValues = ssSeriesFirst;
  ssSeriesLast = ssValues;

  vsChartFirst = vsCustomLast + 1;
  vsTitle = vsChartFirst;
  vsLegend = vsChartFirst + 1;
  vsToolBox = vsChartFirst + 2;
  vsDiagramSelector = vsChartFirst + 3;
  vsDataLevelsInfo = vsChartFirst + 4;
  vsDataLevelActiveValueInfo = vsChartFirst + 5;
  vsActiveDataLevelInfo = vsChartFirst + 6;
  vsChartLast = vsActiveDataLevelInfo;

  cxGridChartColumnDiagramDefaultBorderWidth = 1;
  cxGridChartLineDiagramDefaultHotSpotSize = 15;
  cxGridChartLineDiagramDefaultLineWidth = 1;
  cxGridChartLineDiagramDefaultMarkerSize = 7;
  cxGridChartDefaultDataLevelActiveValueDropDownCount = 15;
  cxGridChartItemDefaultSummaryKind = skSum;
  cxGridChartSideBySideDefaultIndentWidth = 2;
  cxGridChartFullStackedValue = 100;

type
  TcxGridChartCustomizationForm = class;
  TcxGridChartController = class;
  TcxCustomGridChartTitleViewInfo = class;
  TcxGridChartLegendItemViewInfo = class;
  TcxGridChartLegendViewInfo = class;
  TcxGridChartDiagramValueViewInfo = class;
  TcxGridChartDiagramViewInfo = class;
  TcxGridChartHistogramTickMarkLabelsViewInfo = class;
  TcxGridChartHistogramViewInfo = class;
  TcxGridChartColumnDiagramViewInfo = class;
  TcxGridChartLineDiagramLegendItemViewInfo = class;
  TcxGridChartLineDiagramValueViewInfoClass = class of TcxGridChartLineDiagramValueViewInfo;
  TcxGridChartLineDiagramValueViewInfo = class;
  TcxGridChartLineDiagramViewInfo = class;
  TcxGridChartAreaDiagramValueViewInfo = class;
  TcxGridChartPieDiagramValueViewInfo = class;
  TcxGridChartPieSeriesSiteViewInfo = class;
  TcxGridChartPieDiagramViewInfo = class;
  TcxGridChartToolBoxItemViewInfo = class;
  TcxGridChartToolBoxDataLevelActiveValueViewInfo = class;
  TcxGridChartToolBoxDataLevelInfoViewInfo = class;
  TcxGridChartToolBoxCustomizeButtonViewInfo = class;
  TcxGridChartToolBoxDiagramSelectorViewInfo = class;
  TcxGridChartToolBoxViewInfo = class;
  TcxGridChartViewInfo = class;
  TcxCustomGridChartTitle = class;
  TcxGridChartDiagram = class;
  TcxGridChartHistogramAxisTitle = class;
  TcxGridChartHistogramAxis = class;
  TcxGridChartHistogramAxisValue = class;
  TcxGridChartHistogram = class;
  TcxGridChartColumnDiagram = class;
  TcxGridChartLineDiagram = class;
  TcxGridChartAreaDiagram = class;
  TcxGridChartPieDiagram = class;
  TcxGridChartStackedAreaDiagram = class;
  TcxGridChartStackedColumnDiagram = class;
  TcxGridChartStackedBarDiagram = class;
  TcxGridChartItemDataBinding = class;
  TcxGridChartItemClass = class of TcxGridChartItem;
  TcxGridChartItem = class;
  TcxGridChartDataGroup = class;
  TcxGridChartSeries = class;
  TcxGridChartView = class;

  TcxGridChartDataGroupContainerKind = Integer;

  TcxGridChartPartAlignment = (cpaDefault, cpaStart, cpaCenter, cpaEnd);
  TcxGridChartPartOrientation = (cpoDefault, cpoHorizontal, cpoVertical);
  TcxGridChartPartPosition = (cppDefault, cppNone, cppLeft, cppTop, cppRight, cppBottom);

  TcxGridChartColumnDiagramValueCaptionPosition = (cdvcpNone, cdvcpInsideBase,
    cdvcpCenter, cdvcpInsideEnd, cdvcpOutsideEnd);

  TcxGridChartLineDiagramValueCaptionPosition = (ldvcpNone, ldvcpLeft, ldvcpAbove,
    ldvcpRight, ldvcpBelow, ldvcpCenter, ldvcpAboveRight, ldvcpBelowRight, ldvcpAboveLeft, ldvcpBelowLeft);
  TcxGridChartLineStyle = (clsNone, clsSolid, clsDash, clsDot, clsDashDot, clsDashDotDot);
  TcxGridChartMarkerStyle = (cmsNone, cmsSquare, cmsTriangle, cmsDiamond, cmsCircle);

  TcxGridChartPieDiagramValueCaptionPosition = (pdvcpNone, pdvcpCenter,
    pdvcpInsideEnd, pdvcpOutsideEnd, pdvcpOutsideEndWithLeaderLines);

  TcxGridChartValuesStacking = (vsNone, vsNormal, vs100Percent);

  { hit tests }

  TcxGridChartTitleHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridChartLegendHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridChartLegendItemHitTest = class(TcxCustomGridViewHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    Index: Integer;
    Series: TcxGridChartSeries;
    ValueIndex: Integer;
  end;

  TcxGridChartToolBoxHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridChartDataLevelInfoHitTest = class(TcxCustomGridViewHitTest)
  private
    FDataLevel: Integer;
    FDataLevelObject: TcxGridChartDataGroup;
    procedure SetDataLevel(Value: Integer);
    procedure SetDataLevelObject(Value: TcxGridChartDataGroup);
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    DataLevelObjectContainerKind: TcxGridChartDataGroupContainerKind;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
    property DataLevel: Integer read FDataLevel write SetDataLevel;
    property DataLevelObject: TcxGridChartDataGroup read FDataLevelObject write SetDataLevelObject;
  end;

  TcxGridChartDataLevelActiveValueInfoHitTest = class(TcxGridChartDataLevelInfoHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    CanDropDown: Boolean;
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridChartCustomizeButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridChartDiagramSelectorHitTest = class(TcxCustomGridViewHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    CanDropDown: Boolean;
    function Cursor: TCursor; override;
  end;

  TcxGridChartValueHitTest = class(TcxCustomGridViewHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    CanDrillDown: Boolean;
    Series: TcxGridChartSeries;
    ValueIndex: Integer;
    function Cursor: TCursor; override;
  end;

  TcxCustomGridChartDiagramHitTest = class(TcxCustomGridViewHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
  public
    Diagram: TcxGridChartDiagram;
  end;

  // histogram

  TcxGridChartHistogramPlotHitTest = class(TcxCustomGridChartDiagramHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxCustomGridChartHistogramAxisHitTest = class(TcxCustomGridChartDiagramHitTest);

  TcxGridChartHistogramCategoryAxisTitleHitTest = class(TcxCustomGridChartHistogramAxisHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridChartHistogramValueAxisTitleHitTest = class(TcxCustomGridChartHistogramAxisHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // line diagram

  TcxGridChartValueLineHitTest = class(TcxGridChartValueHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // area diagram

  TcxGridChartValueAreaHitTest = class(TcxGridChartValueHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // pie diagram

  TcxGridChartPieSeriesSiteHitTest = class(TcxCustomGridChartDiagramHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    Series: TcxGridChartSeries;
  end;

  TcxGridChartPieSeriesSiteCaptionHitTest = class(TcxGridChartPieSeriesSiteHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  { controller }

  // drag & drop objects

  TcxCustomGridChartMovingObject = class(TcxCustomGridMovingObject)
  private
    function GetController: TcxGridChartController;
    function GetCustomizationForm: TcxGridChartCustomizationForm;
    function GetGridView: TcxGridChartView;
    function GetViewInfo: TcxGridChartViewInfo;
  protected
    property Controller: TcxGridChartController read GetController;
    property CustomizationForm: TcxGridChartCustomizationForm read GetCustomizationForm;
    property GridView: TcxGridChartView read GetGridView;
    property ViewInfo: TcxGridChartViewInfo read GetViewInfo;
  end;

  TcxGridChartDataGroupMovingObjectClass = class of TcxGridChartDataGroupMovingObject;

  TcxGridChartDataGroupMovingObject = class(TcxCustomGridChartMovingObject)
  private
    FDestDataLevel: Integer;
    FSourceItemContainerKind: TcxGridChartDataGroupContainerKind;
    function GetSourceItem: TcxGridChartDataGroup;
    procedure SetDestDataLevel(Value: Integer);
    procedure SetSourceItem(Value: TcxGridChartDataGroup);
  protected
    function CanRemove: Boolean; override;
    function GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect; override;
    function GetCustomizationFormListBox: TcxCustomGridItemsListBox; override;
    function GetSourceItemViewInfo: TcxCustomGridCellViewInfo; override;
    function IsSourceCustomizationForm: Boolean; override;
    function IsValidDestination: Boolean; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    property DestDataLevel: Integer read FDestDataLevel write SetDestDataLevel;
    property SourceItem: TcxGridChartDataGroup read GetSourceItem write SetSourceItem;
    property SourceItemContainerKind: TcxGridChartDataGroupContainerKind
      read FSourceItemContainerKind write FSourceItemContainerKind;
  public
    constructor Create(AControl: TcxControl); override;
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  // customization form

  TcxGridChartSeriesInnerCheckListBox = class(TcxCustomInnerCheckListBox)
  protected
    function GetMetrics: TcxCheckListBoxMetrics; override;
  public
    function GetSeriesImageSize: Integer; virtual;
  end;

  TcxGridChartSeriesCheckListBoxClass = class of TcxGridChartSeriesCheckListBox;

  TcxGridChartSeriesCheckListBox = class(TcxCheckListBox)
  private
    FDraggingSeries: TcxGridChartSeries;
    FGridView: TcxGridChartView;
    FLockRefreshItems: Boolean;
    FSeriesImages: TImageList;
    function GetSeries(AIndex: Integer): TcxGridChartSeries;
  protected
    procedure CheckClicked(Sender: TObject; AIndex: Integer;
      APrevState, ANewState: TcxCheckBoxState); virtual;
    procedure CreateWnd; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DrawSeriesImage(ACanvas: TcxCanvas; const R: TRect; ASeries: TcxGridChartSeries); virtual;
    function GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure RefreshImages;
    procedure RefreshSeriesImages; virtual;

    property DraggingSeries: TcxGridChartSeries read FDraggingSeries write FDraggingSeries;
    property GridView: TcxGridChartView read FGridView;
    property LockRefreshItems: Boolean read FLockRefreshItems write FLockRefreshItems;
    property Series[AIndex: Integer]: TcxGridChartSeries read GetSeries;
    property SeriesImages: TImageList read FSeriesImages;
  public
    constructor Create(AOwner: TComponent; AGridView: TcxGridChartView); reintroduce; virtual;
    destructor Destroy; override;
    procedure RefreshItems; virtual;
    function StartDrag(DragObject: TDragObject): Boolean; override;
  end;

  TcxGridChartSortBySeriesComboBoxClass = class of TcxGridChartSortBySeriesComboBox;

  TcxGridChartSortBySeriesComboBox = class(TcxComboBox)
  private
    FDirectionButton: TcxEditButton;
    FGridView: TcxGridChartView;
  protected
    procedure Click; override;
    procedure DirectionButtonClick; virtual;
    procedure DoButtonClick(AButtonVisibleIndex: Integer); override;
    procedure DrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas;
      AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState); virtual;
    procedure MeasureItem(AControl: TcxCustomComboBox; AIndex: Integer;
      ACanvas: TcxCanvas; var AHeight: Integer); virtual;
    procedure RefreshDirectionButton; virtual;

    property DirectionButton: TcxEditButton read FDirectionButton;
    property GridView: TcxGridChartView read FGridView;
  public
    constructor Create(AOwner: TComponent; AGridView: TcxGridChartView); reintroduce; virtual;
    procedure RefreshItems; virtual;
  end;

  TcxGridChartDataGroupsListBoxClass = class of TcxGridChartDataGroupsListBox;

  TcxGridChartDataGroupsListBox = class(TcxCustomGridItemsListBox)
  private
    function GetGridView: TcxGridChartView;
  protected
    function CalculateItemHeight: Integer; override;
    procedure DoRefreshItems; override;
    function GetDragAndDropParams: TcxCustomGridHitTest; override;
    property GridView: TcxGridChartView read GetGridView;
  public
    procedure PaintItem(ACanvas: TcxCanvas; R: TRect; AIndex: Integer; AFocused: Boolean); override;
  end;

  TcxGridChartOptionsTreeViewClass = class of TcxGridChartOptionsTreeView;

  TcxGridChartOptionsTreeView = class(TcxGridOptionsTreeView)
  private
    FGridView: TcxGridChartView;
  protected
    LegendID: Integer;
    LegendAlignmentID: Integer;
    LegendBorderID: Integer;
    LegendKeyBorderID: Integer;
    LegendOrientationID: Integer;
    LegendPositionID: Integer;
    TitleID: Integer;
    TitleAlignmentID: Integer;
    TitlePositionID: Integer;
    ToolBoxID: Integer;
    ToolBoxBorderID: Integer;
    ToolBoxDiagramSelectorID: Integer;
    ToolBoxPositionID: Integer;
    OtherID: Integer;
    OtherValueHintsID: Integer;

    procedure AddItems; override;
    function IsItemChecked(AParentID, AID: Integer): Boolean; override;
    procedure ItemClicked(AParentID, AID: Integer); override;

    property GridView: TcxGridChartView read FGridView;
  public
    constructor Create(AOwner: TComponent; AGridView: TcxGridChartView); reintroduce; virtual;
  end;

  TcxGridChartCustomizationForm = class(TcxCustomGridCustomizationForm)
  private
    FDataGroupsListBox: TcxGridChartDataGroupsListBox;
    FDataGroupsPage: TcxTabSheet;
    FOptionsPage: TcxTabSheet;
    FOptionsTreeView: TcxGridChartOptionsTreeView;
    FSeriesCheckListBox: TcxGridChartSeriesCheckListBox;
    FSortBySeriesComboBox: TcxGridChartSortBySeriesComboBox;
    FSortBySeriesLabel: TcxLabel;
    FSeriesPage: TcxTabSheet;
    function GetGridView: TcxGridChartView;
  protected
    function CanChangeSortedSeries: Boolean; virtual;
    procedure CreateControls; override;
    function GetDataGroupsListBoxClass: TcxGridChartDataGroupsListBoxClass; virtual;
    function GetDataGroupsPageVisible: Boolean; virtual;
    function GetOptionsPageVisible: Boolean; virtual;
    function GetOptionsTreeViewClass: TcxGridChartOptionsTreeViewClass; virtual;
    function GetSeriesCheckListBoxClass: TcxGridChartSeriesCheckListBoxClass; virtual;
    function GetSeriesPageVisible: Boolean; virtual;
    function GetSortBySeriesComboBoxClass: TcxGridChartSortBySeriesComboBoxClass; virtual;
    procedure GridViewChanged; override;
    procedure InitPageControl; override;

    property GridView: TcxGridChartView read GetGridView;
  public
    procedure PrepareForDataGroupDragAndDrop; virtual;
    procedure RefreshData; override;

    property DataGroupsListBox: TcxGridChartDataGroupsListBox read FDataGroupsListBox;
    property DataGroupsPage: TcxTabSheet read FDataGroupsPage;
    property OptionsPage: TcxTabSheet read FOptionsPage;
    property OptionsTreeView: TcxGridChartOptionsTreeView read FOptionsTreeView;
    property SeriesCheckListBox: TcxGridChartSeriesCheckListBox read FSeriesCheckListBox;
    property SeriesPage: TcxTabSheet read FSeriesPage;
    property SortBySeriesComboBox: TcxGridChartSortBySeriesComboBox read FSortBySeriesComboBox;
    property SortBySeriesLabel: TcxLabel read FSortBySeriesLabel;
  end;

  // popups

  IcxGridChartDataLevelActiveValuePopupOwner = interface(IcxCustomGridPopupOwner)
    ['{94AE5C9F-9A81-401D-8B76-E3278CDD0CEB}']
    function GetDataGroup: TcxGridChartDataGroup;
  end;

  TcxGridChartDataLevelActiveValuePopupClass = class of TcxGridChartDataLevelActiveValuePopup;

  TcxGridChartDataLevelActiveValuePopup = class(TcxCustomGridPopup)
  private
    FDataGroup: TcxGridChartDataGroup;
    FListBox: TcxGridPopupListBox;
    function GetGridView: TcxGridChartView;
    procedure ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
  protected
    Values: TVariantArray;
    procedure AddValueItems; virtual;
    procedure AdjustListBoxSize; virtual;
    procedure InitPopup; override;
    procedure PopulateListBox(AStrings: TStrings); virtual;
    procedure SetDataGroupActiveValue(AItemIndex: Integer); virtual;

    property ListBox: TcxGridPopupListBox read FListBox;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    property DataGroup: TcxGridChartDataGroup read FDataGroup;
    property GridView: TcxGridChartView read GetGridView;
  end;

  TcxGridChartDiagramSelectorPopupClass = class of TcxGridChartDiagramSelectorPopup;

  TcxGridChartDiagramSelectorPopup = class(TcxCustomGridPopup)
  private
    FListBox: TcxGridPopupListBox;
    function GetGridView: TcxGridChartView;
    procedure ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
  protected
    procedure AddDiagramItems; virtual;
    function GetImageOffset: Integer; virtual;
    procedure InitPopup; override;
    procedure SelectDiagram(AItemIndex: Integer); virtual;
    property ListBox: TcxGridPopupListBox read FListBox;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    property GridView: TcxGridChartView read GetGridView;
    property ImageOffset: Integer read GetImageOffset;
  end;

  // controller

  TcxGridChartController = class(TcxCustomGridController)
  private
    FDataLevelActiveValuePopup: TcxGridChartDataLevelActiveValuePopup;
    FDiagramSelectorPopup: TcxGridChartDiagramSelectorPopup;
    FFirstVisibleCategoryIndex: Integer;
    FFirstVisibleCategoryIndexes: array of Integer;
    FMovingDataGroup: TcxGridChartDataGroup;
    FStoredFirstVisibleCategoryIndex: Integer;
    function GetCustomizationForm: TcxGridChartCustomizationForm;
    function GetDataLevelActiveValuePopup: TcxGridChartDataLevelActiveValuePopup;
    function GetDiagramSelectorPopup: TcxGridChartDiagramSelectorPopup;
    function GetGridView: TcxGridChartView;
    function GetIsDataGroupMoving: Boolean;
    function GetViewInfo: TcxGridChartViewInfo;
    function GetVisibleCategoryCountValue: Integer;
    procedure SetFirstVisibleCategoryIndex(Value: Integer);
  protected
    procedure ActiveDataLevelChanged(APrevActiveDataLevel, AActiveDataLevel: Integer); virtual;
    procedure ActiveDiagramChanged(ADiagram: TcxGridChartDiagram); virtual;
    procedure CheckCoordinates; override;
    procedure DataLevelsChanged; virtual;
    procedure FirstVisibleCategoryIndexChanged; virtual;
    function GetDataLevelActiveValuePopupClass: TcxGridChartDataLevelActiveValuePopupClass; virtual;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function GetDiagramSelectorPopupClass: TcxGridChartDiagramSelectorPopupClass; virtual;
    function GetPatternObject(AObject: TPersistent): TPersistent; override;

    function GetFirstVisibleCategoryIndex: Integer; virtual;
    function GetVisibleCategoryCount(ACheckCount: Boolean): Integer; virtual;

    function CanDataDrillDown(AValueIndex: Integer): Boolean; virtual;
    function CanDataDrillUp: Boolean; virtual;
    function DoDataDrillDown(AValueIndex: Integer): Boolean; virtual;
    function DoDataDrillUp: Boolean; virtual;
    function MayDataDrillDown(ACheckDesignTime: Boolean = True): Boolean; virtual;

    //scrolling
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    // scrollbar
    function CanShowScrollBar: Boolean; virtual;
    function GetScrollBarKind: TScrollBarKind; virtual;
    function GetScrollBarPageSize: Integer; virtual;
    function GetScrollBarPos: Integer; virtual;
    function GetScrollBarSize: Integer; virtual;
    function ScrollPosToCategoryIndex(AScrollPos: Integer): Integer; virtual;

    function CanShowDataLevelActiveValuePopup(ACheckDesignTime: Boolean = True): Boolean; virtual;

    // customization
    procedure CustomizationChanged; override;
    function GetCustomizationFormClass: TcxCustomGridCustomizationFormClass; override;
    function GetCustomizationFormDefaultWidth: Integer; override;
    function GetDataLevelInfoDragAndDropObjectClass: TcxGridChartDataGroupMovingObjectClass; virtual;

    property ViewInfo: TcxGridChartViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    function HasDataLevelActiveValuePopup: Boolean;
    function HasDiagramSelectorPopup: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure InitScrollBarsParameters; override;

    function GetValueHintText(ASeries: TcxGridChartSeries; AValueIndex: Integer): string; virtual;

    // data drill down
    function DataDrillDown(AValueIndex: Integer): Boolean; virtual;
    function DataDrillUp: Boolean; virtual;
    function IsDataDrillDownPossible(AValueIndex: Integer): Boolean;

    // paging
    function ActiveDiagramSupportsPaging: Boolean;
    property FirstVisibleCategoryIndex: Integer read GetFirstVisibleCategoryIndex write SetFirstVisibleCategoryIndex;
    property VisibleCategoryCount: Integer read GetVisibleCategoryCountValue;

    property CustomizationForm: TcxGridChartCustomizationForm read GetCustomizationForm;
    property DataLevelActiveValuePopup: TcxGridChartDataLevelActiveValuePopup read GetDataLevelActiveValuePopup;
    property DiagramSelectorPopup: TcxGridChartDiagramSelectorPopup read GetDiagramSelectorPopup;
    property GridView: TcxGridChartView read GetGridView;
    property IsDataGroupMoving: Boolean read GetIsDataGroupMoving;
    property MovingDataGroup: TcxGridChartDataGroup read FMovingDataGroup;
  end;

  { DataController }

  {$HPPEMIT '__interface IcxGridChartItem;'}

  IcxGridChartItem = interface
    ['{CDDAE712-6292-4814-A69E-7D871B299EB2}']
    procedure DataChanged;
    function GetDataBinding: TcxGridChartItemDataBinding;
    function GetID: Integer;
    procedure ValueTypeClassChanged;
  end;

  IcxGridChartViewItemsProvider = interface
    ['{852B466D-5068-4600-BBC6-0061BF69A0B0}']
    function GetItem(AItemClass: TcxGridChartItemClass; AIndex: Integer): TcxGridChartItem;
    procedure GetItemCaptions(AItemClass: TcxGridChartItemClass; ACaptions: TStringList);
    procedure InitItem(AItem: TcxGridChartItem; AIndex: Integer);
  end;

  TcxGridChartDataController = class(TcxDataController, IcxCustomGridDataController)
  private
    function GetGridViewValue: TcxGridChartView;
    function GetOnAfterSummary: TcxAfterSummaryEvent;
    function GetOnSummary: TcxSummaryEvent;
    procedure SetOnAfterSummary(Value: TcxAfterSummaryEvent);
    procedure SetOnSummary(Value: TcxSummaryEvent);
  protected
    { IcxCustomGridDataController }
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure CreateAllItems(AMissingItemsOnly: Boolean);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;

    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    //function GetChartViewItemIndex: Integer; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    procedure Unlocked; override;
  public
    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    function GetItem(Index: Integer): TObject; override;
    property GridView: TcxGridChartView read GetGridViewValue;
  published
    property Options;
    property OnAfterSummary: TcxAfterSummaryEvent read GetOnAfterSummary write SetOnAfterSummary;
    property OnCompare;
    property OnDataChanged;
    property OnFilterRecord;
    property OnRecordChanged;
    property OnSummary: TcxSummaryEvent read GetOnSummary write SetOnSummary;
  end;

  { painter helper }

  TcxGridChartViewPainterHelperClass = class of TcxGridChartViewPainterHelper;

  TcxGridChartViewPainterHelper = class
  private
    FAlphaChannel: Byte;
    FCanvas: TcxCanvas;
    FPenColor: TColor;
    FPenStyle: TPenStyle;
    FPenWidth: Integer;
    FSmoothingMode: TdxGPSmoothingMode;
  private
    function CreatePen: HPen;
    function RealPenColor: TColor;
  public
    constructor Create(ACanvas: TcxCanvas); virtual;
    procedure Pie(const R: TRect; AStartAngle, ASweepAngle: Integer); virtual;
    procedure Polygon(const Points: array of TPoint); virtual;
    procedure Polyline(const Points: array of TPoint); virtual;

    property AlphaChannel: Byte read FAlphaChannel write FAlphaChannel;
    property Canvas: TcxCanvas read FCanvas;
    property PenColor: TColor read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenWidth: Integer read FPenWidth write FPenWidth;
    property SmoothingMode: TdxGPSmoothingMode read FSmoothingMode write FSmoothingMode;
  end;

  TcxGridChartViewGDIPlusPainterHelper = class(TcxGridChartViewPainterHelper)
  public
    procedure Pie(const R: TRect; AStartAngle, ASweepAngle: Integer); override;
    procedure Polygon(const Points: array of TPoint); override;
    procedure Polyline(const Points: array of TPoint); override;
  end;

  // title

  TcxGridChartTitlePainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxCustomGridChartTitleViewInfo;
  protected
    procedure PrepareCanvasForDrawText; override;
    procedure UnprepareCanvasForDrawText; override;
  public
    property ViewInfo: TcxCustomGridChartTitleViewInfo read GetViewInfo;
  end;

  // legend

  TcxGridChartLegendItemPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartLegendItemViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawLegendKey; virtual;
    property ViewInfo: TcxGridChartLegendItemViewInfo read GetViewInfo;
  end;

  TcxGridChartLegendPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartLegendViewInfo;
  protected
    procedure DrawContent; override;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridChartLegendViewInfo read GetViewInfo;
  end;

  // custom diagram

  TcxGridChartDiagramPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartDiagramViewInfo;
  protected
    procedure DrawValueCaptions; virtual;
    procedure DrawValues; virtual;
    procedure FillAndExcludeRect(const R: TRect);
    function NeedsPainting: Boolean; override;
    procedure Paint; override;
    procedure PrepareClipRegionForValueDrawing(AValueViewInfo: TcxGridChartDiagramValueViewInfo;
      AOriginalClipRegion: TcxRegion); virtual;
    property ViewInfo: TcxGridChartDiagramViewInfo read GetViewInfo;
  end;

  // histogram

  TcxGridChartHistogramTickMarkLabelsPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartHistogramTickMarkLabelsViewInfo;
  protected
    procedure DrawCaptions; virtual;
    procedure DrawContent; override;
  public
    property ViewInfo: TcxGridChartHistogramTickMarkLabelsViewInfo read GetViewInfo;
  end;

  TcxGridChartHistogramPainter = class(TcxGridChartDiagramPainter)
  private
    function GetViewInfo: TcxGridChartHistogramViewInfo;
  protected
    procedure DrawCategoryAxis; virtual;
    procedure DrawCategoryGridLines; virtual;
    procedure DrawContent; override;
    procedure DrawPlotBackground; virtual;
    procedure DrawValueAxis; virtual;
    procedure DrawValueGridLines; virtual;
    procedure DrawValuesBackground; virtual;
    function GetTransparentValues: Boolean; virtual; abstract;
    function GetValuesClipRect: TRect; virtual;
    procedure SetValuesClipRegion; virtual;

    property TransparentValues: Boolean read GetTransparentValues;
    property ViewInfo: TcxGridChartHistogramViewInfo read GetViewInfo;
  end;

  // column diagram

  TcxGridChartColumnDiagramValuePainter = class(TcxCustomGridCellPainter)
  protected
    function ExcludeFromClipRect: Boolean; override;
  end;

  TcxGridChartColumnDiagramPainter = class(TcxGridChartHistogramPainter)
  protected
    function GetTransparentValues: Boolean; override;
  end;

  TcxGridChartColumnValueCaptionPainter = class(TcxCustomGridCellPainter)
  protected
    procedure DrawBackground; override;
  end;

  // line diagram

  TcxGridChartLineDiagramLegendItemPainter = class(TcxGridChartLegendItemPainter)
  private
    function GetViewInfo: TcxGridChartLineDiagramLegendItemViewInfo;
  protected
    procedure DrawLegendKey; override;
    procedure DrawLine; virtual;
    procedure DrawMarker; virtual;
    property ViewInfo: TcxGridChartLineDiagramLegendItemViewInfo read GetViewInfo;
  end;

  TcxGridChartLineDiagramValuePainterClass = class of TcxGridChartLineDiagramValuePainter;

  TcxGridChartLineDiagramValuePainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartLineDiagramValueViewInfo;
  protected
    function CanDrawDesignSelection: Boolean; override;
    procedure DrawContent; override;
    procedure DrawLine; virtual;
    procedure DrawMarker; virtual;
    property ViewInfo: TcxGridChartLineDiagramValueViewInfo read GetViewInfo;
  public
    class procedure DrawLineEx(ACanvas: TcxCanvas; const AStart, AFinish: TPoint;
      AStyle: TcxGridChartLineStyle; AWidth: Integer; AColor: TColor; Antialiasing: Boolean); virtual;
    class procedure DrawLines(ACanvas: TcxCanvas; const APoints: TPoints;
      AStyle: TcxGridChartLineStyle; AWidth: Integer; AColor: TColor; Antialiasing: Boolean); virtual;
    class procedure DrawMarkerEx(ACanvas: TcxCanvas; const ABounds: TRect;
      const APoints: TPoints; AStyle: TcxGridChartMarkerStyle;
      const AParams: TcxViewParams; Antialiasing: Boolean); virtual;
  end;

  TcxGridChartLineDiagramPainter = class(TcxGridChartHistogramPainter)
  protected
    procedure DrawContent; override;
    procedure DrawValuesDesignSelection; virtual;
    function GetTransparentValues: Boolean; override;
  end;

  // area diagram

  TcxGridChartAreaDiagramLegendItemPainter = class(TcxGridChartLineDiagramLegendItemPainter)
  protected
    procedure DrawAreaBackground; virtual;
    procedure DrawAreaBorder; virtual;
    procedure DrawLegendKey; override;
  end;

  TcxGridChartAreaDiagramValuePainter = class(TcxGridChartLineDiagramValuePainter)
  private
    function GetViewInfo: TcxGridChartAreaDiagramValueViewInfo;
  protected
    procedure DoDrawArea; virtual;
    procedure DrawArea; virtual;
    property ViewInfo: TcxGridChartAreaDiagramValueViewInfo read GetViewInfo;
  end;

  TcxGridChartAreaDiagramPainter = class(TcxGridChartLineDiagramPainter)
  protected
    procedure DrawValuesBackground; override;
//  function GetTransparentValues: Boolean; override;
  end;

  // pie diagram

  TcxGridChartPieDiagramValuePainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartPieDiagramValueViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawLeaderLine; virtual;
    procedure DrawSlice; virtual;
    property ViewInfo: TcxGridChartPieDiagramValueViewInfo read GetViewInfo;
  public
    procedure MainPaint; override;
  end;

  TcxGridChartPieSeriesSiteCaptionPainter = class(TcxCustomGridCellPainter)
  protected
    function ExcludeFromClipRect: Boolean; override;
  end;

  TcxGridChartPieSeriesSitePainter = class(TcxCustomGridCellPainter)
  protected
    function CanDrawDesignSelection: Boolean; override;
    function ExcludeFromClipRect: Boolean; override;
  end;

  TcxGridChartPieDiagramPainter = class(TcxGridChartDiagramPainter)
  private
    function GetViewInfo: TcxGridChartPieDiagramViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawSeriesDesignSelection; virtual;
    procedure DrawSeriesSites; virtual;
    procedure DrawValues; override;
    procedure ExcludeSeriesPieAreasFromClipRegion; virtual;
    procedure PrepareClipRegionForValueDrawing(AValueViewInfo: TcxGridChartDiagramValueViewInfo;
      AOriginalClipRegion: TcxRegion); override;
    property ViewInfo: TcxGridChartPieDiagramViewInfo read GetViewInfo;
  end;

  // ToolBox

  TcxGridChartToolBoxItemSeparatorPainter = class(TcxCustomGridCellPainter)
  protected
    procedure DrawContent; override;
  end;

  TcxGridChartToolBoxDataLevelActiveValuePainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartToolBoxDataLevelActiveValueViewInfo;
  protected
    procedure PrepareCanvasForDrawText; override;
    property ViewInfo: TcxGridChartToolBoxDataLevelActiveValueViewInfo read GetViewInfo;
  end;

  TcxGridChartToolBoxDataLevelInfoPainter = class(TcxCustomGridCellPainter)
  protected
    procedure DrawBackground; override;
    function ExcludeFromClipRect: Boolean; override;
  end;

  TcxGridChartToolBoxCustomizeButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartToolBoxCustomizeButtonViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxGridChartToolBoxCustomizeButtonViewInfo read GetViewInfo;
  end;

  TcxGridChartToolBoxDiagramSelectorPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartToolBoxDiagramSelectorViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawImage; virtual;
    procedure PrepareCanvasForDrawText; override;
    property ViewInfo: TcxGridChartToolBoxDiagramSelectorViewInfo read GetViewInfo;
  end;

  TcxGridChartToolBoxPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridChartToolBoxViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawDataLevelInfoConnectors; virtual;
    procedure DrawItems(AOpaqueItems: Boolean); virtual;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridChartToolBoxViewInfo read GetViewInfo;
  end;

  // view

  TcxGridChartPainter = class(TcxCustomGridPainter)
  private
    function GetViewInfo: TcxGridChartViewInfo;
  protected
    procedure PaintAfter; override;
    procedure PaintContent; override;
  public
    property ViewInfo: TcxGridChartViewInfo read GetViewInfo;
  end;

  { view data }

  TcxGridChartViewData = class(TcxCustomGridViewData)
  private
    FDataGroupIndex: Integer;
    FSumOfValues: TVariantArray;
    function GetCategory(AIndex: Integer): Variant;
    function GetCategoryCount: Integer;
    function GetChildCount: Integer;
    function GetChildDataGroupIndex(Index: Integer): Integer;
    function GetChildLevel: Integer;
    function GetChildRecordIndex(Index: Integer): Integer;
    function GetDataGroupIndex: Integer;
    function GetGridView: TcxGridChartView;
    function GetGroupValue(ADataGroupIndex, AIndex: Integer): Variant;
    function GetIsEmptyValue(ASeriesIndex, AIndex: Integer): Boolean;
    function GetSumOfValues(ASeriesIndex: Integer): Variant;
    function GetValue(ASeriesIndex, AIndex: Integer): Variant;
    function GetVisibleCategory(AIndex: Integer): Variant;
    function GetVisibleCategoryCount: Integer;
    function GetVisibleGroupValue(AVisibleDataGroupIndex, AIndex: Integer): Variant;
    function GetVisibleGroupValueCount(AVisibleDataGroupIndex: Integer): Integer;
    function GetVisibleValue(ASeriesIndex, AIndex: Integer): Variant;
    procedure SetCategory(AIndex: Integer; const Value: Variant);
    procedure SetCategoryCount(Value: Integer);
    procedure SetGroupValue(ADataGroupIndex, AIndex: Integer; const Value: Variant);
    procedure SetValue(ASeriesIndex, AIndex: Integer; const Value: Variant);
  protected
    function CalculateDataGroupIndex: Integer; virtual;
    function CalculateSumOfValues(ASeriesIndex: Integer): Variant; virtual;
    procedure CheckValueAtLevel(ALevel: Integer; var AValue: Variant);
    function GetDataGroupIndexByLevel(ALevel: Integer): Integer;
    function GetRecordIndexByValueIndex(AValueIndex: Integer): Integer;
    procedure DataLevelsChanged; virtual;
    function IsDataGrouped: Boolean;
    function IsSummaryLevel: Boolean;
    procedure CalculateSumsOfValues;
    procedure SeriesPosChanged(ASeries: TcxGridChartSeries); virtual;
    procedure Update(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateDataGroupIndex;
    procedure UpdateSumsOfValues;

    property ChildCount: Integer read GetChildCount;
    property ChildDataGroupIndex[Index: Integer]: Integer read GetChildDataGroupIndex;
    property ChildLevel: Integer read GetChildLevel;
    property ChildRecordIndex[Index: Integer]: Integer read GetChildRecordIndex;
    property DataGroupIndex: Integer read GetDataGroupIndex;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure GetVisibleGroupValues(ADataGroup: TcxGridChartDataGroup; AStrings: TStrings;
      out AValues: TVariantArray);

    property GridView: TcxGridChartView read GetGridView;
    property SumOfValues[ASeriesIndex: Integer]: Variant read GetSumOfValues;

    // all values
    property Categories[AIndex: Integer]: Variant read GetCategory write SetCategory;
    property CategoryCount: Integer read GetCategoryCount write SetCategoryCount;
    property GroupValues[ADataGroupIndex, AIndex: Integer]: Variant read GetGroupValue write SetGroupValue;
    property Values[ASeriesIndex, AIndex: Integer]: Variant read GetValue write SetValue;

    // visible values
    property IsEmptyValue[ASeriesIndex, AIndex: Integer]: Boolean read GetIsEmptyValue;
    property VisibleCategories[AIndex: Integer]: Variant read GetVisibleCategory;
    property VisibleCategoryCount: Integer read GetVisibleCategoryCount;
    property VisibleValues[ASeriesIndex, AIndex: Integer]: Variant read GetVisibleValue;

    property VisibleGroupValueCount[AVisibleDataGroupIndex: Integer]: Integer read GetVisibleGroupValueCount;
    property VisibleGroupValues[AVisibleDataGroupIndex, AIndex: Integer]: Variant read GetVisibleGroupValue;
  end;

  { view info }

  // part

  TcxCustomGridChartPartViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    function GetGridView: TcxGridChartView;
    function GetGridViewInfo: TcxGridChartViewInfo;
  protected
    function GetAlignment: TcxGridChartPartAlignment; virtual; abstract;
    function GetHeight: Integer; override;
    function GetOrientation: TcxGridChartPartOrientation; virtual; abstract;
    function GetPosition: TcxGridChartPartPosition; virtual; abstract;
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
  public
    property Alignment: TcxGridChartPartAlignment read GetAlignment;
    property GridView: TcxGridChartView read GetGridView;
    property GridViewInfo: TcxGridChartViewInfo read GetGridViewInfo;
    property Orientation: TcxGridChartPartOrientation read GetOrientation;
    property Position: TcxGridChartPartPosition read GetPosition;
  end;

  // custom title

  TcxCustomGridChartTitleViewInfo = class(TcxCustomGridChartPartViewInfo)
  private
    FTitle: TcxCustomGridChartTitle;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignment: TcxGridChartPartAlignment; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetDrawTextRotationAngle: TcxRotationAngle; override;
    function GetMultiLine: Boolean; override;
    function GetOrientation: TcxGridChartPartOrientation; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetPosition: TcxGridChartPartPosition; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    function HasBackground: Boolean; override;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo; ATitle: TcxCustomGridChartTitle); reintroduce; virtual;
    property Title: TcxCustomGridChartTitle read FTitle;
  end;

  // custom legend

  TcxGridChartLegendItemViewInfoClass = class of TcxGridChartLegendItemViewInfo;

  TcxGridChartLegendItemViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FContainer: TcxGridChartLegendViewInfo;
    FIndex: Integer;
    function GetDiagram: TcxGridChartDiagram;
    function GetGridView: TcxGridChartView;
    function GetLegendKeyHeight: Integer;
    function GetLegendKeyWidth: Integer;
    function GetSeries: TcxGridChartSeries;
  protected
    function CalculateContentBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CalculateLegendKeyHeight: Integer; virtual;
    function CalculateLegendKeySize: Integer; virtual;
    function CalculateLegendKeyWidth: Integer; virtual;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoCalculateParams; override;
    function GetDesignSelectionBounds: TRect; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDesignSelected: Boolean; override;
    function GetLegendKeyBorders: TcxBorders; virtual;
    function GetLegendKeyBounds: TRect; virtual;
    function GetLegendKeyOffset: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealLegendKeyBounds: TRect; virtual;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasBackground: Boolean; override;
    function HasCustomDraw: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    property LegendKeyHeight: Integer read GetLegendKeyHeight;
    property LegendKeyOffset: Integer read GetLegendKeyOffset;
    property LegendKeyWidth: Integer read GetLegendKeyWidth;
  public
    LegendKeyParams: TcxViewParams;
    constructor Create(AContainer: TcxGridChartLegendViewInfo; AIndex: Integer); reintroduce; virtual;
    function GetAreaBoundsForPainting: TRect; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property Container: TcxGridChartLegendViewInfo read FContainer;
    property Diagram: TcxGridChartDiagram read GetDiagram;
    property GridView: TcxGridChartView read GetGridView;
    property Index: Integer read FIndex;
    property LegendKeyBorders: TcxBorders read GetLegendKeyBorders;
    property LegendKeyBounds: TRect read GetRealLegendKeyBounds;
    property Series: TcxGridChartSeries read GetSeries;
  end;

  TcxGridChartLegendKind = (lkSeries, lkCategories);

  TcxGridChartLegendViewInfoClass = class of TcxGridChartLegendViewInfo;

  TcxGridChartLegendViewInfo = class(TcxCustomGridChartPartViewInfo)
  private
    FDiagram: TcxGridChartDiagram;
    FItemLegendKeyWidth: Integer;
    FItemLegendKeyHeight: Integer;
    FItemOffset: Integer;
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxGridChartLegendItemViewInfo;
    function GetItemLegendKeyWidth: Integer;
    function GetItemLegendKeyHeight: Integer;
    function GetItemOffset: Integer;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CalculateItemLegendKeyWidth: Integer; virtual;
    function CalculateItemLegendKeyHeight: Integer; virtual;
    function CalculateItemOffset: Integer; virtual;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetAlignment: TcxGridChartPartAlignment; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetOrientation: TcxGridChartPartOrientation; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetPosition: TcxGridChartPartPosition; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasCustomDraw: Boolean; override;

    function AddItem: TcxGridChartLegendItemViewInfo;
    procedure CalculateItems; virtual;
    function GetItemCaption(AIndex: Integer): string; virtual;
    function GetItemClass: TcxGridChartLegendItemViewInfoClass; virtual;
    function GetItemCount: Integer; virtual;
    function GetItemObjectIndex(AIndex: Integer): Integer; virtual; abstract;
    function GetItemSeriesIndex(AIndex: Integer): Integer; virtual;
    function GetItemValueIndex(AIndex: Integer): Integer; virtual;
    function GetKind: TcxGridChartLegendKind; virtual; abstract;
    //function ItemLegendKeyBorderIsValueBorder: Boolean; virtual;

    property ItemLegendKeyWidth: Integer read GetItemLegendKeyWidth;
    property ItemLegendKeyHeight: Integer read GetItemLegendKeyHeight;
    property ItemOffset: Integer read GetItemOffset;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo; ADiagram: TcxGridChartDiagram); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure GetItemLegendKeyParams(AIndex: Integer; out AParams: TcxViewParams); virtual;
    property Diagram: TcxGridChartDiagram read FDiagram;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxGridChartLegendItemViewInfo read GetItem;
    property Kind: TcxGridChartLegendKind read GetKind;
  end;

  // custom diagram

  TcxGridChartDiagramValueCaptionViewInfoClass = class of TcxGridChartDiagramValueCaptionViewInfo;

  TcxGridChartDiagramValueCaptionViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FValueViewInfo: TcxGridChartDiagramValueViewInfo;
  protected
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    function GetTextAttributes(AForPainting: Boolean): Integer; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasBackground: Boolean; override;
    function HasCustomDraw: Boolean; override;
    function HasVisualCompensation: Boolean; virtual;
  public
    constructor Create(AValueViewInfo: TcxGridChartDiagramValueViewInfo); reintroduce; virtual;
    property ValueViewInfo: TcxGridChartDiagramValueViewInfo read FValueViewInfo;
  end;

  TcxGridChartDiagramValueViewInfoClass = class of TcxGridChartDiagramValueViewInfo;

  TcxGridChartDiagramValueViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCaptionViewInfo: TcxGridChartDiagramValueCaptionViewInfo;
    FDiagramViewInfo: TcxGridChartDiagramViewInfo;
    FSeriesIndex: Integer;
    FVisibleValueIndex: Integer;
    function GetController: TcxGridChartController;
    function GetDiagram: TcxGridChartDiagram;
    function GetGridView: TcxGridChartView;
    function GetSeries: TcxGridChartSeries;
    function GetValueIndex: Integer;
  protected
    function CalculateCaptionBounds: TRect; virtual; abstract;
    function CanShowHint: Boolean; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetCaptionViewInfoClass: TcxGridChartDiagramValueCaptionViewInfoClass; virtual;
    function GetCaptionText: string; virtual;
    procedure GetCaptionViewParams(var AParams: TcxViewParams); virtual;
    function GetCellBoundsForHint: TRect; override;
    function GetHintText: string; override;
    function GetHintTextRect(const AMousePos: TPoint): TRect; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetIsDesignSelected: Boolean; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasCaption: Boolean; virtual;
    function HasCaptionVisualCompensation: Boolean; virtual;
    function HasCustomDraw: Boolean; override;
    function HasHintPoint(const P: TPoint): Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function IsHintAtMousePos: Boolean; override;
    function IsHintForText: Boolean; override;
    function IsHintMultiLine: Boolean; override;
    procedure Offset(DX, DY: Integer); override;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    property Controller: TcxGridChartController read GetController;
    property Diagram: TcxGridChartDiagram read GetDiagram;
  public
    constructor Create(ADiagramViewInfo: TcxGridChartDiagramViewInfo;
      ASeriesIndex, AVisibleValueIndex: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property CaptionViewInfo: TcxGridChartDiagramValueCaptionViewInfo read FCaptionViewInfo;
    property DiagramViewInfo: TcxGridChartDiagramViewInfo read FDiagramViewInfo;
    property GridView: TcxGridChartView read GetGridView;
    property Series: TcxGridChartSeries read GetSeries;
    property SeriesIndex: Integer read FSeriesIndex;
    property ValueIndex: Integer read GetValueIndex;
    property VisibleValueIndex: Integer read FVisibleValueIndex;
  end;

  TcxGridChartDiagramViewInfoClass = class of TcxGridChartDiagramViewInfo;

  TcxGridChartDiagramViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FDiagram: TcxGridChartDiagram;
    FValueViewInfos: TList;
    function GetCategory(AIndex: Integer): string;
    function GetGridView: TcxGridChartView;
    function GetSeries(AIndex: Integer): TcxGridChartSeries;
    function GetSeriesCount: Integer;
    function GetValueCount: Integer;
    function GetValueViewInfoValue(Index: Integer): TcxGridChartDiagramValueViewInfo;
    function GetValueViewInfoCount: Integer;
    function GetViewData: TcxGridChartViewData;
  protected
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    function CreateValueViewInfo(ASeriesIndex, AVisibleValueIndex: Integer): TcxGridChartDiagramValueViewInfo;
    function GetValue(ASeriesIndex, AIndex: Integer): Variant; virtual;
    function GetValueCaption(ASeriesIndex, AVisibleValueIndex: Integer): string; virtual;
    function GetValueIndex(AVisibleValueIndex: Integer): Integer; virtual;
    class function GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass; virtual;  // abstract; - because of CLR, BCB
    function GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer; virtual; abstract;
    function GetVisibleCategory(AIndex: Integer): string; virtual;
    function GetVisibleValue(ASeriesIndex, AIndex: Integer): Variant; virtual;
    function GetVisibleValueCount: Integer; virtual;

    property Categories[AIndex: Integer]: string read GetCategory;
    property Values[ASeriesIndex, AIndex: Integer]: Variant read GetValue;
    property Series[AIndex: Integer]: TcxGridChartSeries read GetSeries;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo; ADiagram: TcxGridChartDiagram); reintroduce; virtual;
    destructor Destroy; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetValueViewInfo(ASeriesIndex, AVisibleIndex: Integer): TcxGridChartDiagramValueViewInfo;
    property Diagram: TcxGridChartDiagram read FDiagram;
    property GridView: TcxGridChartView read GetGridView;
    property SeriesCount: Integer read GetSeriesCount;
    property ValueCount: Integer read GetValueCount;
    property ValueViewInfoCount: Integer read GetValueViewInfoCount;
    property ValueViewInfos[Index: Integer]: TcxGridChartDiagramValueViewInfo read GetValueViewInfoValue;
    property ViewData: TcxGridChartViewData read GetViewData;
    property VisibleCategories[AIndex: Integer]: string read GetVisibleCategory;
    property VisibleValueCount: Integer read GetVisibleValueCount;
    property VisibleValues[ASeriesIndex, AIndex: Integer]: Variant read GetVisibleValue;
  end;

  // histogram legend

  TcxGridChartHistogramLegendViewInfo = class(TcxGridChartLegendViewInfo)
  private
    function GetDiagram: TcxGridChartHistogram;
  protected
    function GetItemObjectIndex(AIndex: Integer): Integer; override;
    function GetItemsInReverseOrder: Boolean; virtual;
    function GetKind: TcxGridChartLegendKind; override;
    property ItemsInReverseOrder: Boolean read GetItemsInReverseOrder;
  public
    property Diagram: TcxGridChartHistogram read GetDiagram;
  end;

  // histogram

  TcxGridChartHistogramValueViewInfo = class(TcxGridChartDiagramValueViewInfo)
  private
    function GetCategoryDirection: TcxDirection;
    function GetDiagramViewInfo: TcxGridChartHistogramViewInfo;
    function GetValueDirection: TcxDirection;
  public
    property CategoryDirection: TcxDirection read GetCategoryDirection;
    property DiagramViewInfo: TcxGridChartHistogramViewInfo read GetDiagramViewInfo;
    property ValueDirection: TcxDirection read GetValueDirection;
  end;

  TcxGridChartHistogramAxisTitleViewInfoClass = class of TcxGridChartHistogramAxisTitleViewInfo;

  TcxGridChartHistogramAxisTitleViewInfo = class(TcxCustomGridChartTitleViewInfo)
  private
    FContainer: TcxGridChartHistogramTickMarkLabelsViewInfo;
    function GetTitle: TcxGridChartHistogramAxisTitle;
  protected
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(AContainer: TcxGridChartHistogramTickMarkLabelsViewInfo;
      ATitle: TcxCustomGridChartTitle); reintroduce; virtual;
    property Container: TcxGridChartHistogramTickMarkLabelsViewInfo read FContainer;
    property Title: TcxGridChartHistogramAxisTitle read GetTitle;
  end;

  TcxGridChartAxisPosition = cppLeft..cppBottom;

  TcxGridChartHistogramTickMarkLabelsViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCaptionBounds: TRects;
    FCaptionHeight: Integer;
    FCaptionWidths: array of Integer;
    FDiagramViewInfo: TcxGridChartHistogramViewInfo;
    FIsTwoRowLayout: Boolean;
    FTitleViewInfo: TcxGridChartHistogramAxisTitleViewInfo;
    function GetCaptionBounds(Index: Integer): TRect;
    function GetCaptionHeight: Integer;
    function GetCaptionWidth(Index: Integer): Integer;
    function GetDiagram: TcxGridChartHistogram;
    function GetIsVertical: Boolean;
    procedure SetCaptionBounds(Index: Integer; const Value: TRect);
  protected
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass; virtual;

    function GetCaption(Index: Integer): string; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetIsReverseOrder: Boolean; virtual; abstract;
    function GetValue(Index: Integer): Variant; virtual; abstract;

    function CalculateCaptionBounds(AIndex: Integer): TRect; virtual;
    procedure CalculateCaptionsBounds; virtual;
    function CalculateHeight(AWidth, ALeftReduction, ARightReduction: Integer): Integer; {overload;}reintroduce; virtual;
    function CalculateWidth: Integer; override;
    function CalculateIsTwoRowLayout(AWidth, ALeftReduction, ARightReduction: Integer): Boolean; virtual;
    procedure CalculateTitleBounds(out ATitleBounds, AContentBounds: TRect); virtual;
    function GetAlignmentHorz: TAlignment; override;
    function GetAxis: TcxGridChartHistogramAxis; virtual; abstract;
    function GetCaptionPosition(AIndex: Integer): Integer; virtual; abstract;
    function GetCaptionsVisible: Boolean; virtual;
    procedure GetEdgeCaptionsReductions(var ALeftReduction, ARightReduction: Integer); virtual;
    function GetIsEdgeCaptions: Boolean; virtual; abstract;
    function GetPosition: TcxGridChartAxisPosition; virtual; abstract;
    function GetTextAttributes(AForPainting: Boolean): Integer; override;
    procedure GetTitleViewParams(var AParams: TcxViewParams); virtual; abstract;
    function GetVisible: Boolean; override;

    property Axis: TcxGridChartHistogramAxis read GetAxis;
    property CaptionHeight: Integer read GetCaptionHeight;
    property CaptionWidths[Index: Integer]: Integer read GetCaptionWidth;
    property Diagram: TcxGridChartHistogram read GetDiagram;
    property IsEdgeCaptions: Boolean read GetIsEdgeCaptions;
    property IsReverseOrder: Boolean read GetIsReverseOrder;
    property IsTwoRowLayout: Boolean read FIsTwoRowLayout write FIsTwoRowLayout;
    property IsVertical: Boolean read GetIsVertical;
  public
    constructor Create(ADiagramViewInfo: TcxGridChartHistogramViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure FitCaptionsHorz(AWidth: Integer; var ALeftReduction, ARightReduction: Integer); virtual;
    procedure FitCaptionsVert(var ATopReduction, ABottomReduction: Integer); virtual;
    function GetCaptionSpace(AWidth, ALeftReduction, ARightReduction: Integer): Integer; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property CaptionBounds[Index: Integer]: TRect read GetCaptionBounds write SetCaptionBounds;
    property Captions[Index: Integer]: string read GetCaption;
    property CaptionsVisible: Boolean read GetCaptionsVisible;
    property Count: Integer read GetCount;
    property DiagramViewInfo: TcxGridChartHistogramViewInfo read FDiagramViewInfo;
    property Position: TcxGridChartAxisPosition read GetPosition;
    property TitleViewInfo: TcxGridChartHistogramAxisTitleViewInfo read FTitleViewInfo;
    property Values[Index: Integer]: Variant read GetValue;
  end;

  TcxGridChartHistogramCategoryAxisTitleViewInfo = class(TcxGridChartHistogramAxisTitleViewInfo)
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
  end;

  TcxGridChartHistogramCategoryTickMarkLabelsViewInfoClass = class of TcxGridChartHistogramCategoryTickMarkLabelsViewInfo;

  TcxGridChartHistogramCategoryTickMarkLabelsViewInfo = class(TcxGridChartHistogramTickMarkLabelsViewInfo)
  protected
    function GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass; override;

    function GetCount: Integer; override;
    function GetIsReverseOrder: Boolean; override;
    function GetValue(Index: Integer): Variant; override;

    function GetAxis: TcxGridChartHistogramAxis; override;
    function GetCaptionPosition(AIndex: Integer): Integer; override;
    function GetIsEdgeCaptions: Boolean; override;
    function GetPosition: TcxGridChartAxisPosition; override;
    procedure GetTitleViewParams(var AParams: TcxViewParams); override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
  end;

  TcxGridChartHistogramValueAxisTitleViewInfo = class(TcxGridChartHistogramAxisTitleViewInfo)
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
  end;

  TcxGridChartHistogramValueTickMarkLabelsViewInfoClass = class of TcxGridChartHistogramValueTickMarkLabelsViewInfo;

  TcxGridChartHistogramValueTickMarkLabelsViewInfo = class(TcxGridChartHistogramTickMarkLabelsViewInfo)
  private
    function GetAxisValue: TcxGridChartHistogramAxisValue;
  protected
    function GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass; override;

    function GetCaption(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetIsReverseOrder: Boolean; override;
    function GetValue(Index: Integer): Variant; override;

    function GetAxis: TcxGridChartHistogramAxis; override;
    function GetCaptionPosition(AIndex: Integer): Integer; override;
    function GetIsEdgeCaptions: Boolean; override;
    function GetPosition: TcxGridChartAxisPosition; override;
    procedure GetTitleViewParams(var AParams: TcxViewParams); override;
    procedure GetViewParams(var AParams: TcxViewParams); override;

    property Axis: TcxGridChartHistogramAxisValue read GetAxisValue;
  end;

  TcxGridChartHistogramMark = record
    Value: Variant;
    Position: Integer;
  end;
  TcxGridChartHistogramMarks = array of TcxGridChartHistogramMark;

  TcxGridChartHistogramViewInfo = class(TcxGridChartDiagramViewInfo)
  private
    FCategoryTickMarkLabelsViewInfo: TcxGridChartHistogramCategoryTickMarkLabelsViewInfo;
    FCategoryMarkHeight: Integer;
    FCategoryMarks: TcxGridChartHistogramMarks;
    FHasValuesOutOfRange: Boolean;
    FIsPlotBoundsCalculated: Boolean;
    FMinValue: Extended;
    FMaxValue: Extended;
    FMinVisualValue: Extended;
    FMaxVisualValue: Extended;
    FPlotBounds: TRect;
    FSumOfValues: array of TVariantArray;
    FValueTickMarkLabelsViewInfo: TcxGridChartHistogramValueTickMarkLabelsViewInfo;
    FValueMarkHeight: Integer;
    FValueMarks: TcxGridChartHistogramMarks;
    FValueStep: Extended;
    function GetCategoryAxisColor: TColor;
    function GetCategoryGridLineColor: TColor;
    function GetCategoryMarkCount: Integer;
    function GetCategoryMarkPosition(Index: Integer): Integer;
    function GetCategoryMarkValue(Index: Integer): Integer;
    function GetDiagram: TcxGridChartHistogram;
    function GetHasCategoryAxis: Boolean;
    function GetHasCategoryGridLines: Boolean;
    function GetHasValueAxis: Boolean;
    function GetHasValueGridLines: Boolean;
    function GetIsCategoriesInReverseOrder: Boolean;
    function GetIsCategoryAxisHorz: Boolean;
    function GetIsValueAxisVert: Boolean;
    function GetPlotBounds: TRect;
    function GetSumOfValues(AGroupIndex, AValueIndex: Integer): Variant;
    function GetValueAxisColor: TColor;
    function GetValueGridLineColor: TColor;
    function GetValueMarkCount: Integer;
    function GetValueMarkPosition(Index: Integer): Integer;
    function GetValueMarkValue(Index: Integer): Extended;
    function GetZeroValueOffset: Integer;
    procedure SetCategoryMarkPosition(Index: Integer; Value: Integer);
    procedure SetValueMarkPosition(Index: Integer; Value: Integer);
  protected
    function GetCategoryTickMarkLabelsViewInfoClass: TcxGridChartHistogramCategoryTickMarkLabelsViewInfoClass; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetValueTickMarkLabelsViewInfoClass: TcxGridChartHistogramValueTickMarkLabelsViewInfoClass; virtual;

    procedure DoCalculateParams; override;
    procedure GetPlotBackgroundParams(out AParams: TcxViewParams); virtual;

    function GetCategoryDirection: TcxDirection; virtual;
    function GetCategoryTickMarkLabelOffsetUnits(AIndex: Integer): Integer; virtual; abstract;
    function GetCategoryTickMarkOffsetUnits(AIndex: Integer; AValueAxisBetweenCategories: Boolean): Integer; virtual; abstract;
    function GetUnitAdjustedOffset(AIndex: Integer): Integer; virtual;
    function GetUnitCount: Integer; virtual; abstract;
    function GetUnitOffset(AIndex: Integer): Integer; virtual;
    function IsEdgeCategoryTickMarkLabels: Boolean; virtual; abstract;

    procedure CalculateMinMaxValues(out AMinValue, AMaxValue: Extended;
      out AHasValuesOutOfRange: Boolean); virtual;
    procedure CalculateMinMaxStackedValues(APercentage: Boolean; out AMinValue, AMaxValue: Extended;
      out AHasValuesOutOfRange: Boolean);
    procedure CalculateMinMaxVisualValues(out AMinVisualValue, AMaxVisualValue: Extended); virtual;
    function CalculateSumOfValues(AGroupIndex, AValueIndex: Integer): Variant; virtual;
    function CalculateValueStep: Extended; virtual;
    function GetStackedValue(ASeriesIndex, AIndex: Integer): Variant; virtual;
    function GetValue(ASeriesIndex, AIndex: Integer): Variant; override;
    function GetValueDirection(ASeriesIndex, AVisibleValueIndex: Integer): TcxDirection; virtual;
    function GetValueOffset(const AValue: Variant): Integer; virtual;
    function GetVisibleGroupIndex(ASeriesIndex: Integer): Integer; virtual;
    function GetZeroValue: Extended; virtual;
    procedure InitializeMinMaxValues(out AMinValue, AMaxValue: Extended);
    function IsDataInteger: Boolean;
    function IsSeriesBeginOfGroup(ASeriesIndex: Integer): Boolean; virtual;
    function IsValuePositive(ASeriesIndex, AVisibleValueIndex: Integer): Boolean; virtual;
    function NeedsMinMaxValuesOffset: Boolean; virtual;

    procedure AddMark(var AMarks: TcxGridChartHistogramMarks; const AValue: Variant);

    procedure AddCategoryMark(AValue: Integer);
    procedure CalculateCategoryMarkPositions;
    procedure CalculateCategoryMarkValues; virtual;

    procedure AddValueMark(const AValue: Extended);
    procedure CalculateValueMarkPositions;
    procedure CalculateValueMarkValues; virtual;

    function CalculateCategoryMarkHeight: Integer; virtual;
    function CalculateValueMarkHeight: Integer; virtual;
    function CalculatePlotBounds: TRect; virtual;
    procedure CalculatePlotOffsets(APlotAreaWidth: Integer; var AOffsets: TRect); virtual;
    procedure CalculateValues; virtual; abstract;
    function GetCategoryAxisBounds: TRect; virtual;
    function GetCategoryAxisPosition: TcxGridChartAxisPosition; virtual;
    function GetCategoryGridLineBounds(AIndex: Integer): TRect; virtual;
    function GetCategoryMarkBounds(Index: Integer): TRect; virtual;
    function GetCategoryTickMarkLabelsBounds: TRect; virtual;
    function GetTickMarkLabelsBounds(APosition: TcxGridChartAxisPosition): TRect; virtual;
    function GetValueAxisBounds: TRect; virtual;
    function GetValueAxisPosition: TcxGridChartAxisPosition; virtual;
    //function GetValueBounds(AValueIndex, ASeriesIndex: Integer): TRect; virtual; abstract;
    function GetValueGridLineBounds(AIndex: Integer): TRect; virtual;
    function GetValueMarkBounds(Index: Integer): TRect; virtual;
    function GetValueTickMarkLabelsBounds: TRect; virtual;

    function CustomDrawPlot(ACanvas: TcxCanvas): Boolean; virtual;
    function DoCustomDrawPlot(ACanvas: TcxCanvas): Boolean; virtual;
    function HasCustomDrawPlot: Boolean; virtual;

    property CategoryMarkHeight: Integer read FCategoryMarkHeight;
    property CategoryMarkValues[Index: Integer]: Integer read GetCategoryMarkValue;
    property HasValuesOutOfRange: Boolean read FHasValuesOutOfRange;
    property IsCategoriesInReverseOrder: Boolean read GetIsCategoriesInReverseOrder;
    property IsCategoryAxisHorz: Boolean read GetIsCategoryAxisHorz;
    property IsValueAxisVert: Boolean read GetIsValueAxisVert;
    property MinValue: Extended read FMinValue;
    property MaxValue: Extended read FMaxValue;
    property MinVisualValue: Extended read FMinVisualValue;
    property MaxVisualValue: Extended read FMaxVisualValue;
    property SumOfValues[AGroupIndex, AValueIndex: Integer]: Variant read GetSumOfValues;
    property UnitCount: Integer read GetUnitCount;
    property ValueMarkHeight: Integer read FValueMarkHeight;
    property ValueMarkValues[Index: Integer]: Extended read GetValueMarkValue;
    property ValueStep: Extended read FValueStep;
    property ZeroValue: Extended read GetZeroValue;
    property ZeroValueOffset: Integer read GetZeroValueOffset;
  public
    PlotParams: TcxViewParams;
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo; ADiagram: TcxGridChartDiagram); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property CategoryAxisColor: TColor read GetCategoryAxisColor;
    property CategoryAxisBounds: TRect read GetCategoryAxisBounds;
    property CategoryAxisPosition: TcxGridChartAxisPosition read GetCategoryAxisPosition;
    property CategoryGridLineBounds[AIndex: Integer]: TRect read GetCategoryGridLineBounds;
    property CategoryGridLineColor: TColor read GetCategoryGridLineColor;
    property CategoryMarkBounds[Index: Integer]: TRect read GetCategoryMarkBounds;
    property CategoryTickMarkLabelsViewInfo: TcxGridChartHistogramCategoryTickMarkLabelsViewInfo read FCategoryTickMarkLabelsViewInfo;
    property CategoryMarkCount: Integer read GetCategoryMarkCount;
    property CategoryMarkPositions[Index: Integer]: Integer read GetCategoryMarkPosition write SetCategoryMarkPosition;
    property Diagram: TcxGridChartHistogram read GetDiagram;
    property HasCategoryAxis: Boolean read GetHasCategoryAxis;
    property HasCategoryGridLines: Boolean read GetHasCategoryGridLines;
    property HasValueAxis: Boolean read GetHasValueAxis;
    property HasValueGridLines: Boolean read GetHasValueGridLines;
    property PlotBounds: TRect read GetPlotBounds;
    property ValueAxisBounds: TRect read GetValueAxisBounds;
    property ValueAxisColor: TColor read GetValueAxisColor;
    property ValueAxisPosition: TcxGridChartAxisPosition read GetValueAxisPosition;
    property ValueGridLineBounds[AIndex: Integer]: TRect read GetValueGridLineBounds;
    property ValueGridLineColor: TColor read GetValueGridLineColor;
    property ValueMarkBounds[Index: Integer]: TRect read GetValueMarkBounds;
    property ValueTickMarkLabelsViewInfo: TcxGridChartHistogramValueTickMarkLabelsViewInfo read FValueTickMarkLabelsViewInfo;
    property ValueMarkCount: Integer read GetValueMarkCount;
    property ValueMarkPositions[Index: Integer]: Integer read GetValueMarkPosition write SetValueMarkPosition;
  end;

  // column diagram legend

  TcxGridChartColumnDiagramLegendViewInfo = class(TcxGridChartHistogramLegendViewInfo)
  protected
    function GetItemsInReverseOrder: Boolean; override;
  end;

  // column diagram

  TcxGridChartColumnDiagramValueViewInfo = class(TcxGridChartHistogramValueViewInfo)
  private
    function GetCaptionPosition: TcxGridChartColumnDiagramValueCaptionPosition;
    function GetDiagramViewInfo: TcxGridChartColumnDiagramViewInfo;
  protected
    function CalculateCaptionBounds: TRect; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function HasCaption: Boolean; override;
    function HasCaptionVisualCompensation: Boolean; override;
  public
    property CaptionPosition: TcxGridChartColumnDiagramValueCaptionPosition read GetCaptionPosition;
    property DiagramViewInfo: TcxGridChartColumnDiagramViewInfo read GetDiagramViewInfo;
  end;

  TcxGridChartColumnDiagramViewInfo = class(TcxGridChartHistogramViewInfo)
  private
    function GetDiagram: TcxGridChartColumnDiagram;
  protected
    function GetPainterClass: TcxCustomGridCellPainterClass; override;

    class function GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass; override;
    function GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer; override;

    function GetCategoryTickMarkLabelOffsetUnits(AIndex: Integer): Integer; override;
    function GetCategoryTickMarkOffsetUnits(AIndex: Integer; AValueAxisBetweenCategories: Boolean): Integer; override;
    function GetUnitCount: Integer; override;
    function GetValueGroupAreaSizeUnits: Integer; virtual;
    function GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex: Integer): Integer; virtual;
    function IsEdgeCategoryTickMarkLabels: Boolean; override;

    procedure CalculateValues; override;
    function GetValueBounds(AVisibleValueIndex, ASeriesIndex: Integer): TRect; virtual;
    function MakeValueBounds(const ACategoryStart, ACategoryFinish, AValueStart, AValueFinish: Integer): TRect;
  public
    property Diagram: TcxGridChartColumnDiagram read GetDiagram;
  end;

  // stacked bar diagram

  TcxGridChartStackedColumnDiagramViewInfo = class(TcxGridChartColumnDiagramViewInfo)
  private
    function GetDiagram: TcxGridChartStackedColumnDiagram;
    function GetSeriesGroupCount: Integer;
  protected
    procedure CalculateValues; override;
    procedure CalculateMinMaxValues(out AMinValue, AMaxValue: Extended;
      out AHasValuesOutOfRange: Boolean); override;
    function GetValue(ASeriesIndex, AIndex: Integer): Variant; override;
    function GetValueGroupAreaSizeUnits: Integer; override;
    function IsSeriesBeginOfGroup(ASeriesIndex: Integer): Boolean; override;
    function StackedGroupIndentWidth: Integer;

    property Diagram: TcxGridChartStackedColumnDiagram read GetDiagram;
    property SeriesGroupCount: Integer read GetSeriesGroupCount;
  end;

  // line diagram legend

  TcxGridChartLineDiagramLegendItemViewInfo = class(TcxGridChartLegendItemViewInfo)
  private
    function GetDiagram: TcxGridChartLineDiagram;
    function GetLineStyle: TcxGridChartLineStyle;
    function GetLineWidth: Integer;
    function GetMarkerSize: Integer;
    function GetMarkerStyle: TcxGridChartMarkerStyle;
  protected
    function CalculateHeight: Integer; override;
    function CalculateLegendKeyWidth: Integer; override;
    procedure CalculateMarkerPoints; virtual;
    procedure DoCalculateParams; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetDiagramValueViewInfoClass: TcxGridChartLineDiagramValueViewInfoClass;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetLineColor: TColor; virtual;
    function GetLineStart: TPoint; virtual;
    function GetLineFinish: TPoint; virtual;
    function GetMarkerBounds: TRect; virtual;
    property MarkerSize: Integer read GetMarkerSize;
  public
    MarkerParams: TcxViewParams;
    MarkerPoints: TPoints;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property Diagram: TcxGridChartLineDiagram read GetDiagram;
    property LineColor: TColor read GetLineColor;
    property LineStart: TPoint read GetLineStart;
    property LineFinish: TPoint read GetLineFinish;
    property LineStyle: TcxGridChartLineStyle read GetLineStyle;
    property LineWidth: Integer read GetLineWidth;
    property MarkerBounds: TRect read GetMarkerBounds;
    property MarkerStyle: TcxGridChartMarkerStyle read GetMarkerStyle;
  end;

  TcxGridChartLineDiagramLegendViewInfo = class(TcxGridChartHistogramLegendViewInfo)
  protected
    function GetItemClass: TcxGridChartLegendItemViewInfoClass; override;
  public
    procedure GetItemLegendKeyMarkerParams(AIndex: Integer; out AParams: TcxViewParams); virtual;
  end;

  // line diagram

  TcxGridChartLineDiagramValueViewInfo = class(TcxGridChartHistogramValueViewInfo)
  private
    FLineStartY: Integer;
    FLineFinishY: Integer;
    function GetCaptionPosition: TcxGridChartLineDiagramValueCaptionPosition;
    function GetDiagram: TcxGridChartLineDiagram;
    function GetDiagramViewInfo: TcxGridChartLineDiagramViewInfo;
    function GetLineStyle: TcxGridChartLineStyle;
    function GetLineWidth: Integer;
    function GetMarkerStyle: TcxGridChartMarkerStyle;
  protected
    function CalculateCaptionBounds: TRect; override;
    procedure CalculateMarkerParams(var AParams: TcxViewParams); virtual;
    procedure CalculateMarkerPoints; virtual;
    procedure DoCalculateParams; override;
    function GetCellBoundsForHint: TRect; override;
    function GetDesignSelectionBounds: TRect; override;
    function GetHotSpotBounds: TRect; virtual;
    function GetLineColor: TColor; virtual;
    function GetLineHotZoneMinWidth: Integer; virtual;
    function GetLineStart: TPoint; virtual;
    function GetLineFinish: TPoint; virtual;
    function GetMarkerBounds: TRect; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    class function GetPainterClassEx: TcxGridChartLineDiagramValuePainterClass; virtual;
    function GetPosition: TPoint; virtual;
    procedure MakeRealBounds(var ABounds: TRect);
    function HasBackground: Boolean; override;
    function HasCaption: Boolean; override;

    property Diagram: TcxGridChartLineDiagram read GetDiagram;
    property HotSpotBounds: TRect read GetHotSpotBounds;
    property LineHotZoneMinWidth: Integer read GetLineHotZoneMinWidth;
  public
    MarkerParams: TcxViewParams;
    MarkerPoints: TPoints;
    procedure Calculate(const ABounds: TRect; ALineStartY, ALineFinishY: Integer); virtual;
    class procedure CalculateMarkerPointsEx(const ABounds: TRect;
      AStyle: TcxGridChartMarkerStyle; var APoints: TPoints); virtual;
    function GetAreaBoundsForPainting: TRect; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetRealBounds: TRect; reintroduce;
    function HasPoint(const P: TPoint): Boolean; override;

    property CaptionPosition: TcxGridChartLineDiagramValueCaptionPosition read GetCaptionPosition;
    property DiagramViewInfo: TcxGridChartLineDiagramViewInfo read GetDiagramViewInfo;
    property LineColor: TColor read GetLineColor;
    property LineStart: TPoint read GetLineStart;
    property LineFinish: TPoint read GetLineFinish;
    property LineStartY: Integer read FLineStartY;
    property LineFinishY: Integer read FLineFinishY;
    property LineStyle: TcxGridChartLineStyle read GetLineStyle;
    property LineWidth: Integer read GetLineWidth;
    property MarkerBounds: TRect read GetMarkerBounds;
    property MarkerStyle: TcxGridChartMarkerStyle read GetMarkerStyle;
    property Position: TPoint read GetPosition;
  end;

  TcxGridChartLineDiagramViewInfo = class(TcxGridChartHistogramViewInfo)
  private
    function GetDiagram: TcxGridChartLineDiagram;
  protected
    function GetPainterClass: TcxCustomGridCellPainterClass; override;

    class function GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass; override;
    function GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer; override;

    function GetCategoryTickMarkLabelOffsetUnits(AIndex: Integer): Integer; override;
    function GetCategoryTickMarkOffsetUnits(AIndex: Integer; AValueAxisBetweenCategories: Boolean): Integer; override;
    function GetUnitCount: Integer; override;
    function GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex: Integer): Integer; virtual;
    function GetValueY(AVisibleValueIndex, ASeriesIndex: Integer): Integer; virtual;
    function HasGap(AValueIndex: Integer): Boolean;
    function IsEdgeCategoryTickMarkLabels: Boolean; override;
    function IsGapValue(ASeriesIndex, AValueIndex: Integer): Boolean;

    procedure CalculatePlotOffsets(APlotAreaWidth: Integer; var AOffsets: TRect); override;
    procedure CalculateValues; override;
    function GetValueBounds(AVisibleValueIndex, ASeriesIndex: Integer): TRect; virtual;
  public
    property Diagram: TcxGridChartLineDiagram read GetDiagram;
  end;

  // area diagram legend

  TcxGridChartAreaDiagramLegendItemViewInfo = class(TcxGridChartLineDiagramLegendItemViewInfo)
  protected
    function CalculateHeight: Integer; override;
    function CalculateLegendKeyHeight: Integer; override;
    function CalculateLegendKeyWidth: Integer; override;
    function GetLineColor: TColor; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
  end;

  TcxGridChartAreaDiagramLegendViewInfo = class(TcxGridChartLineDiagramLegendViewInfo)
  protected
    function GetItemClass: TcxGridChartLegendItemViewInfoClass; override;
    function GetItemsInReverseOrder: Boolean; override;
    //function ItemLegendKeyBorderIsValueBorder: Boolean; override;
  end;

  // area diagram

  TcxGridChartAreaDiagramValueViewInfo = class(TcxGridChartLineDiagramValueViewInfo)
  private
    function GetDiagram: TcxGridChartAreaDiagram;
  protected
    procedure CalculateAreaPoints; virtual;
    function CustomDrawBackground(ACanvas: TcxCanvas): Boolean; override;
    function GetAreaColor: TColor; virtual;
    function GetHotSpotBounds: TRect; override;
    function GetLineColor: TColor; override;
    function GetLineHotZoneMinWidth: Integer; override;
    class function GetPainterClassEx: TcxGridChartLineDiagramValuePainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetZeroValueOffset: Integer; virtual;
    function HasCustomDrawBackground: Boolean; override;

    property Diagram: TcxGridChartAreaDiagram read GetDiagram;
    property ZeroValueOffset: Integer read GetZeroValueOffset;
  public
    AreaPoints: TPoints;
    procedure Calculate(const ABounds: TRect; ALineStartY, ALineFinishY: Integer); override;
    function CreateAreaRegion: TcxRegion; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    property AreaColor: TColor read GetAreaColor;
  end;

  TcxGridChartAreaDiagramViewInfo = class(TcxGridChartLineDiagramViewInfo)
  protected
    function ExcludeEachSeriesArea: Boolean; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    class function GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass; override;
  end;

  TcxGridChartStackedAreaDiagramViewInfo = class(TcxGridChartAreaDiagramViewInfo)
  private
    function GetDiagram: TcxGridChartStackedAreaDiagram;
  protected
    procedure CalculateValues; override;
    function ExcludeEachSeriesArea: Boolean; override;
    function GetSeriesCountAt(AValueIndex: Integer): Integer;
    function GetValue(ASeriesIndex, AIndex: Integer): Variant; override;
  public
    property Diagram: TcxGridChartStackedAreaDiagram read GetDiagram;
  end;

  // pie diagram legend

  TcxGridChartPieDiagramLegendViewInfo = class(TcxGridChartLegendViewInfo)
  private
    function GetDiagram: TcxGridChartPieDiagram;
  protected
    function GetItemObjectIndex(AIndex: Integer): Integer; override;
    function GetKind: TcxGridChartLegendKind; override;
  public
    property Diagram: TcxGridChartPieDiagram read GetDiagram;
  end;

  // pie diagram

  TcxGridChartPieDiagramValueViewInfo = class(TcxGridChartDiagramValueViewInfo)
  private
    FStartAngle: Integer;
    FFinishAngle: Integer;
    function GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
    function GetCenter: TPoint;
    function GetDiagramViewInfo: TcxGridChartPieDiagramViewInfo;
    function GetRadius: Integer;
  protected
    function CalculateCaptionBounds: TRect; override;
    function CalculateCaptionCenter(AAngle, ACaptionWidth, ACaptionHeight: Integer): TPoint; virtual;
    procedure CalculateLeaderLinePoints(AAngle: Integer); virtual;
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetIsDesignSelected: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function HasBackground: Boolean; override;
    function HasCaption: Boolean; override;
    procedure MakeCaptionVisible(var ACaptionBounds: TRect; AAngle: Integer); virtual;
  public
    LeaderLinePoints: TPoints;
    procedure Calculate(const R: TRect; AStartAngle, AFinishAngle: Integer); virtual;
    function HasPoint(const P: TPoint): Boolean; override;

    property CaptionPosition: TcxGridChartPieDiagramValueCaptionPosition read GetCaptionPosition;
    property Center: TPoint read GetCenter;
    property DiagramViewInfo: TcxGridChartPieDiagramViewInfo read GetDiagramViewInfo;
    property Radius: Integer read GetRadius;
    property StartAngle: Integer read FStartAngle;
    property FinishAngle: Integer read FFinishAngle;
  end;

  TcxGridChartPieSeriesSiteCaptionViewInfoClass = class of TcxGridChartPieSeriesSiteCaptionViewInfo;

  TcxGridChartPieSeriesSiteCaptionViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FSiteViewInfo: TcxGridChartPieSeriesSiteViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetText: string; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasCustomDraw: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(ASiteViewInfo: TcxGridChartPieSeriesSiteViewInfo); reintroduce; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property SiteViewInfo: TcxGridChartPieSeriesSiteViewInfo read FSiteViewInfo;
  end;

  TcxGridChartPieSeriesSiteViewInfoClass = class of TcxGridChartPieSeriesSiteViewInfo;

  TcxGridChartPieSeriesSiteViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCaptionBounds: TRect;
    FCaptionViewInfo: TcxGridChartPieSeriesSiteCaptionViewInfo;
    FDiagramViewInfo: TcxGridChartPieDiagramViewInfo;
    FPieAreaBounds: TRect;
    FSeries: TcxGridChartSeries;
    function GetDiagram: TcxGridChartPieDiagram;
  protected
    Angles: array of Integer;
    procedure CalculateAngles; virtual;
    procedure CalculateCaptionAndPieAreaBounds(var ACaptionBounds, APieAreaBounds: TRect); virtual;
    function CalculateContentBounds: TRect; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetCaptionViewInfoClass: TcxGridChartPieSeriesSiteCaptionViewInfoClass; virtual;
    function GetDesignSelectionBounds: TRect; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDesignSelected: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetValueViewInfo(AVisibleValueIndex: Integer): TcxGridChartPieDiagramValueViewInfo;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasCustomDraw: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    property CaptionBounds: TRect read FCaptionBounds;
    property Diagram: TcxGridChartPieDiagram read GetDiagram;
  public
    constructor Create(ADiagramViewInfo: TcxGridChartPieDiagramViewInfo; ASeries: TcxGridChartSeries); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function IsPieEmpty: Boolean; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    procedure Paint(ACanvas: TcxCanvas = nil); override;
    property CaptionViewInfo: TcxGridChartPieSeriesSiteCaptionViewInfo read FCaptionViewInfo;
    property DiagramViewInfo: TcxGridChartPieDiagramViewInfo read FDiagramViewInfo;
    property PieAreaBounds: TRect read FPieAreaBounds;
    property Series: TcxGridChartSeries read FSeries;
  end;

  TcxGridChartPieDiagramViewInfo = class(TcxGridChartDiagramViewInfo)
  private
    FPieAreaValueCaptionCompensation: Integer;
    FSeriesSiteViewInfos: TList;
    function GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
    function GetDiagram: TcxGridChartPieDiagram;
    function GetSeriesSiteViewInfo(Index: Integer): TcxGridChartPieSeriesSiteViewInfo;
    function GetSeriesSiteViewInfoCount: Integer;
  protected
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetSeriesSiteViewInfoClass: TcxGridChartPieSeriesSiteViewInfoClass; virtual;
    class function GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass; override;
    function GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer; override;

    function CalculatePieAreaValueCaptionCompensation: Integer; virtual;
    function CalculateSeriesSiteViewInfoBounds(AIndex: Integer): TRect; virtual;
    function CalculateValueCaptionMaxWidth: Integer;
    function CalculateValueCaptionMaxHeight: Integer;

    property CaptionPosition: TcxGridChartPieDiagramValueCaptionPosition read GetCaptionPosition;
    property PieAreaValueCaptionCompensation: Integer read FPieAreaValueCaptionCompensation;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo; ADiagram: TcxGridChartDiagram); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    property Diagram: TcxGridChartPieDiagram read GetDiagram;
    property SeriesSiteViewInfoCount: Integer read GetSeriesSiteViewInfoCount;
    property SeriesSiteViewInfos[Index: Integer]: TcxGridChartPieSeriesSiteViewInfo read GetSeriesSiteViewInfo;
  end;

  // toolbox

  TcxGridChartToolBoxItemAlignment = TcxGridChartPartPosition;  // cppLeft, cppRight only

  TcxGridChartToolBoxItemViewInfoClass = class of TcxGridChartToolBoxItemViewInfo;

  TcxGridChartToolBoxItemViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FAlignment: TcxGridChartToolBoxItemAlignment;
    FContainer: TcxGridChartToolBoxViewInfo;
    function GetGridView: TcxGridChartView;
  protected
    procedure GetViewParams(var AParams: TcxViewParams); override;
  public
    constructor Create(AContainer: TcxGridChartToolBoxViewInfo;
      AAlignment: TcxGridChartToolBoxItemAlignment); reintroduce; virtual;
    property Alignment: TcxGridChartToolBoxItemAlignment read FAlignment write FAlignment;
    property Container: TcxGridChartToolBoxViewInfo read FContainer;
    property GridView: TcxGridChartView read GetGridView;
  end;

  TcxGridChartToolBoxItemSeparatorViewInfoClass = class of TcxGridChartToolBoxItemSeparatorViewInfo;

  TcxGridChartToolBoxItemSeparatorViewInfo = class(TcxGridChartToolBoxItemViewInfo)
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasBackground: Boolean; override;
  end;

  TcxGridChartToolBoxDataLevelActiveValueViewInfoClass = class of TcxGridChartToolBoxDataLevelActiveValueViewInfo;

  TcxGridChartToolBoxDataLevelActiveValueViewInfo = class(TcxCustomGridViewCellViewInfo,
    IcxGridChartDataLevelActiveValuePopupOwner)
  private
    FCanShowDropDownWindow: Boolean;
    FContainer: TcxGridChartToolBoxDataLevelInfoViewInfo;
    function GetGridView: TcxGridChartView;
  protected
    { IcxGridChartDataLevelActiveValuePopupOwner }
    function GetDataGroup: TcxGridChartDataGroup;

    function CalculateContentBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    function GetDropDownWindowOwnerBounds: TRect; override;
  public
    constructor Create(AContainer: TcxGridChartToolBoxDataLevelInfoViewInfo); reintroduce; virtual;
    property CanShowDropDownWindow: Boolean read FCanShowDropDownWindow;
    property Container: TcxGridChartToolBoxDataLevelInfoViewInfo read FContainer;
    property GridView: TcxGridChartView read GetGridView;
  end;

  TcxGridChartToolBoxDataLevelInfoViewInfoClass = class of TcxGridChartToolBoxDataLevelInfoViewInfo;

  TcxGridChartToolBoxDataLevelInfoViewInfo = class(TcxGridChartToolBoxItemViewInfo)
  private
    FActiveValueViewInfo: TcxGridChartToolBoxDataLevelActiveValueViewInfo;
    FDataLevel: Integer;
    function GetActive: Boolean;
    function GetDataLevelObject: TcxGridChartDataGroup;
  protected
    function CalculateActiveValueViewInfoBounds: TRect; virtual;
    function CalculateContentBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetActiveValueViewInfoClass: TcxGridChartToolBoxDataLevelActiveValueViewInfoClass; virtual;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetDesignObject: TPersistent; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDesignSelected: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasActiveValue: Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(AContainer: TcxGridChartToolBoxViewInfo;
      AAlignment: TcxGridChartToolBoxItemAlignment; ADataLevel: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    procedure Paint(ACanvas: TcxCanvas = nil); override;
    property Active: Boolean read GetActive;
    property ActiveValueViewInfo: TcxGridChartToolBoxDataLevelActiveValueViewInfo read FActiveValueViewInfo;
    property DataLevel: Integer read FDataLevel;
    property DataLevelObject: TcxGridChartDataGroup read GetDataLevelObject;
  end;

  TcxGridChartToolBoxCustomizeButtonViewInfoClass = class of TcxGridChartToolBoxCustomizeButtonViewInfo;

  TcxGridChartToolBoxCustomizeButtonViewInfo = class(TcxGridChartToolBoxItemViewInfo)
  private
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    procedure Click; override;
    function GetActualState: TcxGridCellState; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    function HasBackground: Boolean; override;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TcxGridChartToolBoxDiagramSelectorViewInfoClass = class of TcxGridChartToolBoxDiagramSelectorViewInfo;

  TcxGridChartToolBoxDiagramSelectorViewInfo = class(TcxGridChartToolBoxItemViewInfo)
  private
    FCanShowDropDownWindow: Boolean;
    function GetDropDownWindowValue: TcxGridChartDiagramSelectorPopup;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetImageBounds: TRect; virtual;
    function GetImageHeight: Integer; virtual;
    function GetImageIndex: Integer; virtual;
    function GetImageWidth: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealImageBounds: TRect; virtual;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasBackground: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    function CalculateCanShowDropDownWindow: Boolean; virtual;
    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    function GetDropDownWindowOwnerBounds: TRect; override;
    property DropDownWindow: TcxGridChartDiagramSelectorPopup read GetDropDownWindowValue;

    property ImageWidth: Integer read GetImageWidth;
    property ImageHeight: Integer read GetImageHeight;
  public
    constructor Create(AContainer: TcxGridChartToolBoxViewInfo; AAlignment: TcxGridChartToolBoxItemAlignment); override;
    property CanShowDropDownWindow: Boolean read FCanShowDropDownWindow;
    property ImageBounds: TRect read GetRealImageBounds;
    property ImageIndex: Integer read GetImageIndex;
  end;

  TcxGridChartToolBoxViewInfoClass = class of TcxGridChartToolBoxViewInfo;

  TcxGridChartToolBoxViewInfo = class(TcxCustomGridChartPartViewInfo)
  private
    FCustomizeButton: TcxGridChartToolBoxCustomizeButtonViewInfo;
    FDataLevelInfos: TList;
    FDiagramSelector: TcxGridChartToolBoxDiagramSelectorViewInfo;
    FItems: TList;
    function GetCount: Integer;
    function GetDataLevelInfoConnectorCount: Integer;
    function GetDataLevelInfo(AIndex: Integer): TcxGridChartToolBoxDataLevelInfoViewInfo;
    function GetDataLevelInfoCount: Integer;
    function GetItem(AIndex: Integer): TcxGridChartToolBoxItemViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CalculateContentBounds: TRect; override;
    procedure CalculateItems; virtual;
    function GetAlignment: TcxGridChartPartAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBorders: TcxBorders; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetDataGroupInsertionAreaBounds: TRect; virtual;
    function GetDataLevelInfoConnector(AIndex: Integer): TRect; virtual;
    function GetDataLevelInfoConnectorColor: TColor; virtual;
    function GetFirstSeparator: TcxGridChartToolBoxItemViewInfo;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetItemsAreaBounds: TRect; virtual;
    function GetOrientation: TcxGridChartPartOrientation; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetPosition: TcxGridChartPartPosition; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;

    function AddItem(AItem: TcxGridChartToolBoxItemViewInfo): TcxGridChartToolBoxItemViewInfo;
    procedure AddSeparator(AAlignment: TcxGridChartToolBoxItemAlignment);
    procedure CreateItems; virtual;
    function GetCustomizeButtonClass: TcxGridChartToolBoxCustomizeButtonViewInfoClass; virtual;
    function GetDataLevelInfoClass: TcxGridChartToolBoxDataLevelInfoViewInfoClass; virtual;
    function GetDiagramSelectorClass: TcxGridChartToolBoxDiagramSelectorViewInfoClass; virtual;
    function GetItemSeparatorClass: TcxGridChartToolBoxItemSeparatorViewInfoClass; virtual;
    function IsCustomizeButtonVisible: Boolean; virtual;
    function IsDataLevelsInfoVisible: Boolean; virtual;
    function IsDiagramSelectorVisible: Boolean; virtual;
    function IsSeparator(AItem: TcxGridChartToolBoxItemViewInfo): Boolean;

    property ItemsAreaBounds: TRect read GetItemsAreaBounds;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetDataGroupInsertionBounds(AIndex: Integer): TRect; virtual;
    function GetDataGroupInsertionIndex(const P: TPoint): Integer; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure InvalidateCustomizeButton;
    property Count: Integer read GetCount;
    property CustomizeButton: TcxGridChartToolBoxCustomizeButtonViewInfo read FCustomizeButton;
    property DataLevelInfoConnectorColor: TColor read GetDataLevelInfoConnectorColor;
    property DataLevelInfoConnectorCount: Integer read GetDataLevelInfoConnectorCount;
    property DataLevelInfoConnectors[AIndex: Integer]: TRect read GetDataLevelInfoConnector;
    property DataLevelInfoCount: Integer read GetDataLevelInfoCount;
    property DataLevelInfos[AIndex: Integer]: TcxGridChartToolBoxDataLevelInfoViewInfo read GetDataLevelInfo;
    property DiagramSelector: TcxGridChartToolBoxDiagramSelectorViewInfo read FDiagramSelector;
    property Items[AIndex: Integer]: TcxGridChartToolBoxItemViewInfo read GetItem;
  end;

  // view

  TcxGridChartTitleViewInfoClass = class of TcxGridChartTitleViewInfo;

  TcxGridChartTitleViewInfo = class(TcxCustomGridChartTitleViewInfo)
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
  end;

  TcxGridChartViewInfo = class(TcxCustomGridViewInfo)
  private
    FDiagramViewInfo: TcxGridChartDiagramViewInfo;
    FIsImage: Boolean;
    FLegendViewInfo: TcxGridChartLegendViewInfo;
    FTitleViewInfo: TcxGridChartTitleViewInfo;
    FToolBoxViewInfo: TcxGridChartToolBoxViewInfo;
    function GetGridView: TcxGridChartView;
  protected
    procedure CreateViewInfos; override;
    procedure DestroyViewInfos(AIsRecreating: Boolean); override;

    function GetTitleViewInfoClass: TcxGridChartTitleViewInfoClass; virtual;
    function GetToolBoxViewInfoClass: TcxGridChartToolBoxViewInfoClass; virtual;

    procedure Calculate; override;
    function CalculateClientBounds: TRect; override;
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer;
      var AFullyVisible: Boolean); override;
    procedure CalculatePartBounds(APart: TcxCustomGridChartPartViewInfo;
      var ABounds, APartBounds: TRect); virtual;
    procedure CalculatePartsBounds(out ADiagramBounds, ALegendBounds, ATitleBounds,
      AToolBoxBounds: TRect); virtual;
    function DoGetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property IsImage: Boolean read FIsImage write FIsImage;
  public
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property DiagramViewInfo: TcxGridChartDiagramViewInfo read FDiagramViewInfo;
    property GridView: TcxGridChartView read GetGridView;
    property LegendViewInfo: TcxGridChartLegendViewInfo read FLegendViewInfo;
    property TitleViewInfo: TcxGridChartTitleViewInfo read FTitleViewInfo;
    property ToolBoxViewInfo: TcxGridChartToolBoxViewInfo read FToolBoxViewInfo;
  end;

  { view }

  // custom title

  TcxCustomGridChartTitle = class(TcxCustomGridOptions)
  private
    FAlignment: TcxGridChartPartAlignment;
    FPosition: TcxGridChartPartPosition;
    FText: string;
    procedure SetAlignment(Value: TcxGridChartPartAlignment);
    procedure SetPosition(Value: TcxGridChartPartPosition);
    procedure SetText(Value: string);
  protected
    procedure GetStoredProperties(AProperties: TStrings); override;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); override;

    function GetDefaultAlignment: TcxGridChartPartAlignment; virtual;
    function GetDefaultOrientation: TcxGridChartPartOrientation; virtual;
    function GetDefaultPosition: TcxGridChartPartPosition; virtual; abstract;
    function GetDefaultText: string; virtual;

    property Position: TcxGridChartPartPosition read FPosition write SetPosition default cppDefault;
  public
    procedure Assign(Source: TPersistent); override;
    function GetAlignment: TcxGridChartPartAlignment;
    function GetOrientation: TcxGridChartPartOrientation;
    function GetPosition: TcxGridChartPartPosition;
    function GetText: string;
  published
    property Alignment: TcxGridChartPartAlignment read FAlignment write SetAlignment default cpaDefault;
    property Text: string read FText write SetText;
  end;

  // legend

  TcxGridChartLegendBorder = (lbDefault, lbNone, lbSingle);

  TcxGridChartLegendClass = class of TcxGridChartLegend;

  TcxGridChartLegend = class(TcxCustomGridOptions)
  private
    FAlignment: TcxGridChartPartAlignment;
    FBorder: TcxGridChartLegendBorder;
    FKeyBorder: TcxGridChartLegendBorder;
    FOrientation: TcxGridChartPartOrientation;
    FParent: TcxGridChartLegend;
    FPosition: TcxGridChartPartPosition;
    procedure SetAlignment(Value: TcxGridChartPartAlignment);
    procedure SetBorder(Value: TcxGridChartLegendBorder);
    procedure SetKeyBorder(Value: TcxGridChartLegendBorder);
    procedure SetOrientation(Value: TcxGridChartPartOrientation);
    procedure SetPosition(Value: TcxGridChartPartPosition);
  protected
    procedure GetStoredProperties(AProperties: TStrings); override;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); override;

    function GetDefaultAlignment: TcxGridChartPartAlignment; virtual;
    function GetDefaultBorder: TcxGridChartLegendBorder; virtual;
    function GetDefaultKeyBorder: TcxGridChartLegendBorder; virtual;
    function GetDefaultOrientation(APosition: TcxGridChartPartPosition): TcxGridChartPartOrientation; virtual;
    function GetDefaultPosition: TcxGridChartPartPosition; virtual;
    property Parent: TcxGridChartLegend read FParent write FParent;
  public
    procedure Assign(Source: TPersistent); override;
    function GetAlignment: TcxGridChartPartAlignment;
    function GetBorder: TcxGridChartLegendBorder;
    function GetKeyBorder: TcxGridChartLegendBorder;
    function GetOrientation(APosition: TcxGridChartPartPosition = cppDefault): TcxGridChartPartOrientation;
    function GetPosition: TcxGridChartPartPosition;
  published
    property Alignment: TcxGridChartPartAlignment read FAlignment write SetAlignment default cpaDefault;
    property Border: TcxGridChartLegendBorder read FBorder write SetBorder default lbDefault;
    property KeyBorder: TcxGridChartLegendBorder read FKeyBorder write SetKeyBorder default lbDefault;
    property Orientation: TcxGridChartPartOrientation read FOrientation
      write SetOrientation default cpoDefault;
    property Position: TcxGridChartPartPosition read FPosition write SetPosition default cppDefault;
  end;

  // diagram

  { TcxCustomGridChartDiagramOptions }

  TcxGridDiagramChange = (dcProperty, dcLayout, dcSize);

  TcxCustomGridChartDiagramOptions = class(TPersistent)
  strict private
    FDiagram: TcxGridChartDiagram;

    function GetGridView: TcxGridChartView;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure Changed(AChange: TcxGridDiagramChange = dcLayout); virtual;
    procedure ChangeScale(M, D: Integer); virtual;

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    constructor Create(ADiagram: TcxGridChartDiagram); virtual;
    procedure Assign(Source: TPersistent); override;
    //
    property Diagram: TcxGridChartDiagram read FDiagram;
    property GridView: TcxGridChartView read GetGridView;
  end;

  { TcxGridChartDiagramStyles }

  TcxGridChartDiagramStylesClass = class of TcxGridChartDiagramStyles;
  TcxGridChartDiagramStyles = class(TcxCustomGridStyles)
  strict private
    function GetDiagram: TcxGridChartDiagram;
    function GetGridViewValue: TcxGridChartView;
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetGridView: TcxCustomGridView; override;
    function GetVaryColorsByCategory: Boolean; virtual; abstract;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure GetBackgroundParams(out AParams: TcxViewParams); virtual;
    procedure GetValueParams(AVisibleSeriesIndex, AValueIndex: Integer; out AParams: TcxViewParams); virtual;
    procedure Assign(Source: TPersistent); override;
    property Diagram: TcxGridChartDiagram read GetDiagram;
    property GridView: TcxGridChartView read GetGridViewValue;
  published
    property Legend: TcxStyle index dsLegend read GetValue write SetValue;
    property ValueCaptions: TcxStyle index dsValueCaptions read GetValue write SetValue;
    property Values: TcxStyle index dsValues read GetValue write SetValue;
  end;

  { TcxGridChartDiagram }

  TcxGridChartDiagramLegendCustomDrawEvent = procedure(Sender: TcxGridChartDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean) of object;
  TcxGridChartDiagramLegendItemCustomDrawEvent = procedure(Sender: TcxGridChartDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean) of object;
  TcxGridChartDiagramValueCustomDrawEvent = procedure(Sender: TcxGridChartDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean) of object;
  TcxGridChartDiagramValueCaptionCustomDrawEvent = procedure(Sender: TcxGridChartDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueCaptionViewInfo; var ADone: Boolean) of object;

  TcxGridChartDiagramClass = class of TcxGridChartDiagram;
  TcxGridChartDiagram = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FGridView: TcxGridChartView;
    FLegend: TcxGridChartLegend;
    FStyles: TcxGridChartDiagramStyles;

    FOnCustomDrawLegend: TcxGridChartDiagramLegendCustomDrawEvent;
    FOnCustomDrawLegendItem: TcxGridChartDiagramLegendItemCustomDrawEvent;
    FOnCustomDrawValue: TcxGridChartDiagramValueCustomDrawEvent;
    FOnCustomDrawValueCaption: TcxGridChartDiagramValueCaptionCustomDrawEvent;

    function GetActive: Boolean;
    function GetID: string;
    procedure SetActive(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetLegend(Value: TcxGridChartLegend);
    procedure SetStyles(Value: TcxGridChartDiagramStyles);
    procedure SetOnCustomDrawLegend(Value: TcxGridChartDiagramLegendCustomDrawEvent);
    procedure SetOnCustomDrawLegendItem(Value: TcxGridChartDiagramLegendItemCustomDrawEvent);
    procedure SetOnCustomDrawValue(Value: TcxGridChartDiagramValueCustomDrawEvent);
    procedure SetOnCustomDrawValueCaption(Value: TcxGridChartDiagramValueCaptionCustomDrawEvent);
  protected
    FSubClassEvents: TNotifyEvent;

    function GetOwner: TPersistent; override;

    procedure CreateSubObjects(AGridView: TcxGridChartView); virtual;
    procedure DestroySubObjects; virtual;
    procedure SetGridView(Value: TcxGridChartView);

    function GetStylesClass: TcxGridChartDiagramStylesClass; virtual;

    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; virtual; abstract;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; virtual; abstract;

    procedure Changed(AChange: TcxGridDiagramChange = dcLayout); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetDisplayText: string; virtual;
    function GetImageIndex: Integer; virtual;
    function HorizontalPaging: Boolean; virtual;
    function PagingInOppositeDirection: Boolean; virtual;
    function SupportsPaging: Boolean; virtual;
    function SupportsValueHotTrack: Boolean; virtual;

    procedure DoCustomDrawLegend(ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawLegendItem(ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawValue(ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean); virtual;
    function HasCustomDrawLegend: Boolean; virtual;
    function HasCustomDrawLegendItem: Boolean; virtual;
    function HasCustomDrawValue: Boolean; virtual;
    function HasCustomDrawValueCaption: Boolean; virtual;

    property ImageIndex: Integer read GetImageIndex;
    property Styles: TcxGridChartDiagramStyles read FStyles write SetStyles;
  public
    constructor Create(AGridView: TcxGridChartView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetNamePath: string; override;
    function GetStackedValueCaption(ASeries: TcxGridChartSeries; AValueIndex: Integer; APercentage: Boolean): string; virtual;
    function GetValueCaption(ASeries: TcxGridChartSeries; AValueIndex: Integer): string; virtual;
    function IsAvailable: Boolean; virtual;
    property DisplayText: string read GetDisplayText;
    property GridView: TcxGridChartView read FGridView;
    property ID: string read GetID;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Legend: TcxGridChartLegend read FLegend write SetLegend;
    property OnCustomDrawLegend: TcxGridChartDiagramLegendCustomDrawEvent read FOnCustomDrawLegend
      write SetOnCustomDrawLegend;
    property OnCustomDrawLegendItem: TcxGridChartDiagramLegendItemCustomDrawEvent
      read FOnCustomDrawLegendItem write SetOnCustomDrawLegendItem;
    property OnCustomDrawValue: TcxGridChartDiagramValueCustomDrawEvent read FOnCustomDrawValue
      write SetOnCustomDrawValue;
    property OnCustomDrawValueCaption: TcxGridChartDiagramValueCaptionCustomDrawEvent read FOnCustomDrawValueCaption
      write SetOnCustomDrawValueCaption;
  end;

  // histogram

  TcxGridChartHistogramAxisTitleClass = class of TcxGridChartHistogramAxisTitle;

  TcxGridChartHistogramAxisTitle = class(TcxCustomGridChartTitle)
  private
    FAxis: TcxGridChartHistogramAxis;
  protected
    function GetDefaultPosition: TcxGridChartPartPosition; override;
    function GetDefaultText: string; override;
    function GetGridViewValue: TcxCustomGridView; override;
  public
    constructor Create(AAxis: TcxGridChartHistogramAxis); reintroduce; virtual;
    property Axis: TcxGridChartHistogramAxis read FAxis;
  end;

  TcxGridChartHistogramTickMarkKind = (tmkNone, tmkCross, tmkInside, tmkOutside);

  TcxGridChartHistogramAxis = class(TcxCustomGridChartDiagramOptions)
  private
    FGridLines: Boolean;
    FTickMarkKind: TcxGridChartHistogramTickMarkKind;
    FTickMarkLabels: Boolean;
    FTitle: TcxGridChartHistogramAxisTitle;
    FVisible: Boolean;
    function GetDiagram: TcxGridChartHistogram;
    procedure SetGridLines(Value: Boolean);
    procedure SetTickMarkKind(Value: TcxGridChartHistogramTickMarkKind);
    procedure SetTickMarkLabels(Value: Boolean);
    procedure SetTitle(Value: TcxGridChartHistogramAxisTitle);
    procedure SetVisible(Value: Boolean);
  protected
    function GetTitleClass: TcxGridChartHistogramAxisTitleClass; virtual;
    function GetTitleDefaultText: string; virtual;
  public
    constructor Create(ADiagram: TcxGridChartDiagram); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetPosition: TcxGridChartAxisPosition; virtual; abstract;
    property Diagram: TcxGridChartHistogram read GetDiagram;
  published
    property GridLines: Boolean read FGridLines write SetGridLines default True;
    property TickMarkKind: TcxGridChartHistogramTickMarkKind read FTickMarkKind write SetTickMarkKind default tmkOutside;
    property TickMarkLabels: Boolean read FTickMarkLabels write SetTickMarkLabels default True;
    property Title: TcxGridChartHistogramAxisTitle read FTitle write SetTitle;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TcxGridChartHistogramAxisCategoryClass = class of TcxGridChartHistogramAxisCategory;

  TcxGridChartHistogramAxisCategory = class(TcxGridChartHistogramAxis)
  private
    FCategoriesInReverseOrder: Boolean;
    FValueAxisAtMaxCategory: Boolean;
    FValueAxisBetweenCategories: Boolean;
    procedure SetCategoriesInReverseOrder(Value: Boolean);
    procedure SetValueAxisAtMaxCategory(Value: Boolean);
    procedure SetValueAxisBetweenCategories(Value: Boolean);
  protected
    function GetDefaultValueAxisBetweenCategories: Boolean; virtual;
    function GetTitleDefaultText: string; override;
  public
    constructor Create(ADiagram: TcxGridChartDiagram); override;
    procedure Assign(Source: TPersistent); override;
    function GetPosition: TcxGridChartAxisPosition; override;
  published
    property CategoriesInReverseOrder: Boolean read FCategoriesInReverseOrder
      write SetCategoriesInReverseOrder default False;
    property ValueAxisAtMaxCategory: Boolean read FValueAxisAtMaxCategory
      write SetValueAxisAtMaxCategory default False;
    property ValueAxisBetweenCategories: Boolean read FValueAxisBetweenCategories
      write SetValueAxisBetweenCategories default True;
  end;

  TcxGridChartHistogramMinMaxValues = (mmvZeroBasedAuto, mmvAuto, mmvCustom);

  TcxGridChartHistogramAxisValueClass = class of TcxGridChartHistogramAxisValue;

  TcxGridChartHistogramAxisValue = class(TcxGridChartHistogramAxis)
  private
    FMaxValue: Extended;
    FMinMaxValues: TcxGridChartHistogramMinMaxValues;
    FMinValue: Extended;
    FTickMarkLabelFormat: string;
    procedure SetMaxValue(const Value: Extended);
    procedure SetMinMaxValues(Value: TcxGridChartHistogramMinMaxValues);
    procedure SetMinValue(const Value: Extended);
    procedure SetTickMarkLabelFormat(const Value: string);
  protected
    function GetTitleDefaultText: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetPosition: TcxGridChartAxisPosition; override;
    function GetTickMarkLabel(const ATickMarkValue: Variant): string; virtual;
    function GetTickMarkLabelFormat(AConsiderSeriesFormat: Boolean): string; virtual;
  published
    property MinMaxValues: TcxGridChartHistogramMinMaxValues read FMinMaxValues
      write SetMinMaxValues default mmvZeroBasedAuto;
    property MinValue: Extended read FMinValue write SetMinValue;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property TickMarkLabelFormat: string read FTickMarkLabelFormat write SetTickMarkLabelFormat;
  end;

  TcxGridChartHistogramStyles = class(TcxGridChartDiagramStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetVaryColorsByCategory: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Axis: TcxStyle index dsAxis read GetValue write SetValue;
    property AxisTitle: TcxStyle index dsAxisTitle read GetValue write SetValue;
    property CategoryAxis: TcxStyle index dsCategoryAxis read GetValue write SetValue;
    property CategoryAxisTitle: TcxStyle index dsCategoryAxisTitle read GetValue write SetValue;
    property CategoryGridLines: TcxStyle index dsCategoryGridLines read GetValue write SetValue;
    property GridLines: TcxStyle index dsGridLines read GetValue write SetValue;
    property Plot: TcxStyle index dsPlot read GetValue write SetValue;
    property ValueAxis: TcxStyle index dsValueAxis read GetValue write SetValue;
    property ValueAxisTitle: TcxStyle index dsValueAxisTitle read GetValue write SetValue;
    property ValueGridLines: TcxStyle index dsValueGridLines read GetValue write SetValue;
  end;

  { TcxGridChartHistogramValues }

  TcxGridChartHistogramValuesClass = class of TcxGridChartHistogramValues;
  TcxGridChartHistogramValues = class(TcxCustomGridChartDiagramOptions)
  strict private
    FStacking: TcxGridChartValuesStacking;
    FVaryColorsByCategory: Boolean;

    procedure SetStacking(Value: TcxGridChartValuesStacking);
    procedure SetVaryColorsByCategory(Value: Boolean);
  protected
    property Stacking: TcxGridChartValuesStacking read FStacking write SetStacking default vsNone;
  public
    procedure Assign(Source: TPersistent); override;
    function GetVaryColorsByCategory: Boolean; virtual;
  published
    property VaryColorsByCategory: Boolean read FVaryColorsByCategory write SetVaryColorsByCategory default False;
  end;

  { TcxGridChartHistogram }

  TcxGridChartHistogramPlotCustomDrawEvent = procedure(Sender: TcxGridChartHistogram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartHistogramViewInfo;
    const ABounds: TRect; var ADone: Boolean) of object;

  TcxGridChartEmptyPointsDisplayMode = (epdmZero, epdmGap);

  TcxGridChartHistogram = class(TcxGridChartDiagram)
  strict private
    FAxisCategory: TcxGridChartHistogramAxisCategory;
    FAxisValue: TcxGridChartHistogramAxisValue;
    FEmptyPointsDisplayMode: TcxGridChartEmptyPointsDisplayMode;
    FValues: TcxGridChartHistogramValues;

    FOnCustomDrawPlot: TcxGridChartHistogramPlotCustomDrawEvent;

    function GetStyles: TcxGridChartHistogramStyles;
    procedure SetAxisCategory(Value: TcxGridChartHistogramAxisCategory);
    procedure SetAxisValue(Value: TcxGridChartHistogramAxisValue);
    procedure SetEmptyPointsDisplayMode(Value: TcxGridChartEmptyPointsDisplayMode);
    procedure SetStyles(Value: TcxGridChartHistogramStyles);
    procedure SetValues(Value: TcxGridChartHistogramValues);
    procedure SetOnCustomDrawPlot(Value: TcxGridChartHistogramPlotCustomDrawEvent);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateSubObjects(AGridView: TcxGridChartView); override;
    procedure DestroySubObjects; override;

    function GetAxisCategoryClass: TcxGridChartHistogramAxisCategoryClass; virtual;
    function GetAxisValueClass: TcxGridChartHistogramAxisValueClass; virtual;
    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; override;
    function GetStylesClass: TcxGridChartDiagramStylesClass; override;
    function GetValuesClass: TcxGridChartHistogramValuesClass; virtual;

    function PagingInOppositeDirection: Boolean; override;
    function SupportsPaging: Boolean; override;

    procedure DoCustomDrawPlot(ACanvas: TcxCanvas; AViewInfo: TcxGridChartHistogramViewInfo; const ABounds: TRect; var ADone: Boolean); virtual;
    function HasCustomDrawPlot: Boolean; virtual;

    property EmptyPointsDisplayMode: TcxGridChartEmptyPointsDisplayMode read FEmptyPointsDisplayMode write SetEmptyPointsDisplayMode default epdmZero;
  public
    procedure Assign(Source: TPersistent); override;
    function GetCategoryAxisPosition: TcxGridChartAxisPosition; virtual;
    function GetValueAxisPosition: TcxGridChartAxisPosition; virtual;

    property AxisCategory: TcxGridChartHistogramAxisCategory read FAxisCategory write SetAxisCategory;
    property AxisValue: TcxGridChartHistogramAxisValue read FAxisValue write SetAxisValue;
    property Styles: TcxGridChartHistogramStyles read GetStyles write SetStyles;
    property Values: TcxGridChartHistogramValues read FValues write SetValues;
    property ValuesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
  published
    property OnCustomDrawPlot: TcxGridChartHistogramPlotCustomDrawEvent read FOnCustomDrawPlot write SetOnCustomDrawPlot;
  end;

  // column diagram

  { TcxGridChartColumnDiagramValues }

  TcxGridChartColumnDiagramValues = class(TcxGridChartHistogramValues)
  strict private
    FBorderWidth: Integer;
    FCaptionPosition: TcxGridChartColumnDiagramValueCaptionPosition;

    procedure SetBorderWidth(Value: Integer);
    procedure SetCaptionPosition(Value: TcxGridChartColumnDiagramValueCaptionPosition);
  public
    constructor Create(ADiagram: TcxGridChartDiagram); override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default cxGridChartColumnDiagramDefaultBorderWidth;
    property CaptionPosition: TcxGridChartColumnDiagramValueCaptionPosition read FCaptionPosition write SetCaptionPosition default cdvcpNone;
  end;

  { TcxGridChartColumnDiagram }

  TcxGridChartColumnDiagramClass = class of TcxGridChartColumnDiagram;
  TcxGridChartColumnDiagram = class(TcxGridChartHistogram)
  strict private
    function GetValues: TcxGridChartColumnDiagramValues;
    procedure SetValues(Value: TcxGridChartColumnDiagramValues);
  protected
    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; override;
    function GetValuesClass: TcxGridChartHistogramValuesClass; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;

    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;
  published
    property AxisCategory;
    property AxisValue;
    property Styles;
    property Values: TcxGridChartColumnDiagramValues read GetValues write SetValues;
  end;

  // bar diagram

  { TcxGridChartBarDiagram }

  TcxGridChartBarDiagramClass = class of TcxGridChartBarDiagram;
  TcxGridChartBarDiagram = class(TcxGridChartColumnDiagram)
  protected
    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;

    function HorizontalPaging: Boolean; override;
    function PagingInOppositeDirection: Boolean; override;
  public
    function GetCategoryAxisPosition: TcxGridChartAxisPosition; override;
    function GetValueAxisPosition: TcxGridChartAxisPosition; override;
  end;

  // line diagram

  TcxGridChartLineDiagramAxisCategory = class(TcxGridChartHistogramAxisCategory)
  protected
    function GetDefaultValueAxisBetweenCategories: Boolean; override;
  published
    property ValueAxisBetweenCategories default False;
  end;

  TcxGridChartLineDiagramStyles = class(TcxGridChartHistogramStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function ValueMarkerHasBorderByDefault: Boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetValueMarkerParams(AVisibleSeriesIndex, AValueIndex: Integer; out AParams: TcxViewParams); virtual;
  published
    property ValueMarkers: TcxStyle index dsValueMarkers read GetValue write SetValue;
  end;

  { TcxGridChartLineDiagramValues }

  TcxGridChartLineDiagramGetLineStyleEvent = procedure(Sender: TcxGridChartLineDiagram;
    ASeries: TcxGridChartSeries; var AStyle: TcxGridChartLineStyle) of object;
  TcxGridChartLineDiagramGetMarkerStyleEvent = procedure(Sender: TcxGridChartLineDiagram;
    ASeries: TcxGridChartSeries; var AStyle: TcxGridChartMarkerStyle) of object;

//  TcxGridChartEmptyPointLineStyle = (eplsDefault, eplsNone, eplsSolid, eplsDash, eplsDot, eplsDashDot, eplsDashDotDot);

  TcxGridChartLineDiagramValues = class(TcxGridChartHistogramValues)
  strict private
    FCaptionPosition: TcxGridChartLineDiagramValueCaptionPosition;
    FHotSpotSize: Integer;
    FLineStyle: TcxGridChartLineStyle;
    FLineWidth: Integer;
    FMarkerSize: Integer;
    FMarkerStyle: TcxGridChartMarkerStyle;

    FOnGetLineStyle: TcxGridChartLineDiagramGetLineStyleEvent;
    FOnGetMarkerStyle: TcxGridChartLineDiagramGetMarkerStyleEvent;

    function GetDiagram: TcxGridChartLineDiagram;
    procedure SetCaptionPosition(Value: TcxGridChartLineDiagramValueCaptionPosition);
    procedure SetHotSpotSize(Value: Integer);
    procedure SetLineStyle(Value: TcxGridChartLineStyle);
    procedure SetLineWidth(Value: Integer);
    procedure SetMarkerSize(Value: Integer);
    procedure SetMarkerStyle(Value: TcxGridChartMarkerStyle);
    procedure SetOnGetLineStyle(Value: TcxGridChartLineDiagramGetLineStyleEvent);
    procedure SetOnGetMarkerStyle(Value: TcxGridChartLineDiagramGetMarkerStyleEvent);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoGetLineStyle(ASeries: TcxGridChartSeries; var AStyle: TcxGridChartLineStyle); virtual;
    procedure DoGetMarkerStyle(ASeries: TcxGridChartSeries; var AStyle: TcxGridChartMarkerStyle); virtual;
  public
    constructor Create(ADiagram: TcxGridChartDiagram); override;
    procedure Assign(Source: TPersistent); override;
    function GetHotSpotSize: Integer; virtual;
    function GetLineStyle(ASeries: TcxGridChartSeries): TcxGridChartLineStyle; virtual;
    function GetMarkerStyle(ASeries: TcxGridChartSeries): TcxGridChartMarkerStyle; virtual;
    property Diagram: TcxGridChartLineDiagram read GetDiagram;
  published
    property CaptionPosition: TcxGridChartLineDiagramValueCaptionPosition read FCaptionPosition write SetCaptionPosition default ldvcpNone;
    property HotSpotSize: Integer read FHotSpotSize write SetHotSpotSize default cxGridChartLineDiagramDefaultHotSpotSize;
    property LineStyle: TcxGridChartLineStyle read FLineStyle write SetLineStyle default clsSolid;
    property LineWidth: Integer read FLineWidth write SetLineWidth default cxGridChartLineDiagramDefaultLineWidth;
    property MarkerSize: Integer read FMarkerSize write SetMarkerSize default cxGridChartLineDiagramDefaultMarkerSize;
    property MarkerStyle: TcxGridChartMarkerStyle read FMarkerStyle write SetMarkerStyle default cmsNone;
    property Stacking;
    property OnGetLineStyle: TcxGridChartLineDiagramGetLineStyleEvent read FOnGetLineStyle write SetOnGetLineStyle;
    property OnGetMarkerStyle: TcxGridChartLineDiagramGetMarkerStyleEvent read FOnGetMarkerStyle write SetOnGetMarkerStyle;
  end;

  { TcxGridChartLineDiagram }

  TcxGridChartLineDiagramClass = class of TcxGridChartLineDiagram;
  TcxGridChartLineDiagram = class(TcxGridChartHistogram)
  strict private
    function GetAxisCategory: TcxGridChartLineDiagramAxisCategory;
    function GetStyles: TcxGridChartLineDiagramStyles;
    function GetValues: TcxGridChartLineDiagramValues;
    procedure SetAxisCategory(Value: TcxGridChartLineDiagramAxisCategory);
    procedure SetStyles(Value: TcxGridChartLineDiagramStyles);
    procedure SetValues(Value: TcxGridChartLineDiagramValues);
  protected
    function GetAxisCategoryClass: TcxGridChartHistogramAxisCategoryClass; override;
    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; override;
    function GetStylesClass: TcxGridChartDiagramStylesClass; override;
    function GetValuesClass: TcxGridChartHistogramValuesClass; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;

    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;
    function SupportsValueHotTrack: Boolean; override;
  published
    property AxisCategory: TcxGridChartLineDiagramAxisCategory read GetAxisCategory write SetAxisCategory;
    property AxisValue;
    property EmptyPointsDisplayMode;
    property Styles: TcxGridChartLineDiagramStyles read GetStyles write SetStyles;
    property Values: TcxGridChartLineDiagramValues read GetValues write SetValues;
    property ValuesEvents;
  end;

  // area diagram

  { TcxGridChartAreaDiagramStyles }

  TcxGridChartAreaDiagramStyles = class(TcxGridChartLineDiagramStyles)
  protected
    function ValueMarkerHasBorderByDefault: Boolean; override;
  end;

  { TcxGridChartAreaDiagram }

  TcxGridChartAreaDiagramValueAreaCustomDrawEvent = procedure(Sender: TcxGridChartAreaDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartAreaDiagramValueViewInfo; var ADone: Boolean) of object;

  TcxGridChartAreaDiagramClass = class of TcxGridChartAreaDiagram;
  TcxGridChartAreaDiagram = class(TcxGridChartLineDiagram)
  strict private
    FTransparency: Byte;

    FOnCustomDrawValueArea: TcxGridChartAreaDiagramValueAreaCustomDrawEvent;

    function GetStyles: TcxGridChartAreaDiagramStyles;
    procedure SetStyles(Value: TcxGridChartAreaDiagramStyles);
    procedure SetTransparency(Value: Byte);
    procedure SetOnCustomDrawValueArea(Value: TcxGridChartAreaDiagramValueAreaCustomDrawEvent);
  protected
    function CheckGapPoints: Boolean; virtual;
    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; override;
    function GetStylesClass: TcxGridChartDiagramStylesClass; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;

    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;

    procedure DoCustomDrawValueArea(ACanvas: TcxCanvas;
      AViewInfo: TcxGridChartAreaDiagramValueViewInfo; var ADone: Boolean); virtual;
    function HasCustomDrawValueArea: Boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Transparency: Byte read FTransparency write SetTransparency default 0;
    property Styles: TcxGridChartAreaDiagramStyles read GetStyles write SetStyles;
    property OnCustomDrawValueArea: TcxGridChartAreaDiagramValueAreaCustomDrawEvent read FOnCustomDrawValueArea write SetOnCustomDrawValueArea;
  end;

  // stacked area & bars & columns

  TcxGridChartStackedAreaDiagramClass = class of TcxGridChartStackedAreaDiagram;
  TcxGridChartStackedColumnDiagramClass = class of TcxGridChartStackedColumnDiagram;
  TcxGridChartStackedBarDiagramClass = class of TcxGridChartStackedBarDiagram;

  TcxGridChartStackedAreaDiagramStyle = (sasDefault, sas100Percent);
  TcxGridChartStackedDiagramStyle = (sdsDefault, sds100Percent, sdsSideBySide, sdsSideBySide100Percent);

  { TcxGridChartStackedAreaDiagram }

  TcxGridChartStackedAreaDiagram = class(TcxGridChartAreaDiagram)
  strict private
    FStackedStyle: TcxGridChartStackedAreaDiagramStyle;

    procedure SetStackedStyle(Value: TcxGridChartStackedAreaDiagramStyle);
  public
    function CheckGapPoints: Boolean; override;
    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;
    function GetValueCaption(ASeries: TcxGridChartSeries; AValueIndex: Integer): string; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;
  published
    property StackedStyle: TcxGridChartStackedAreaDiagramStyle read FStackedStyle write SetStackedStyle default sasDefault;
  end;

  { TcxGridChartStackedColumnDiagram }

  TcxGridChartStackedColumnDiagram = class(TcxGridChartColumnDiagram)
  strict private
    FSideBySideIndentWidth: Integer;
    FStackedStyle: TcxGridChartStackedDiagramStyle;

    procedure SetSideBySideIndentWidth(Value: Integer);
    procedure SetStackedStyle(Value: TcxGridChartStackedDiagramStyle);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;
  public
    constructor Create(AGridView: TcxGridChartView); override;
    procedure Assign(Source: TPersistent); override;
    function GetValueCaption(ASeries: TcxGridChartSeries; AValueIndex: Integer): string; override;
  published
    property StackedStyle: TcxGridChartStackedDiagramStyle read FStackedStyle write SetStackedStyle default sdsDefault;
    property SideBySideIndentWidth: Integer read FSideBySideIndentWidth write SetSideBySideIndentWidth default cxGridChartSideBySideDefaultIndentWidth;
  end;

  { TcxGridChartStackedBarDiagram }

  TcxGridChartStackedBarDiagram = class(TcxGridChartStackedColumnDiagram)
  public
    function GetCategoryAxisPosition: TcxGridChartAxisPosition; override;
    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;
    function GetValueAxisPosition: TcxGridChartAxisPosition; override;
    function HorizontalPaging: Boolean; override;
    function PagingInOppositeDirection: Boolean; override;
  end;

  // pie diagram

  TcxGridChartPieDiagramStyles = class(TcxGridChartDiagramStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetVaryColorsByCategory: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SeriesSiteCaptions: TcxStyle index dsSeriesSiteCaptions read GetValue write SetValue;
    property SeriesSites: TcxStyle index dsSeriesSites read GetValue write SetValue;
  end;

  { TcxGridChartPieDiagramValues }

  TcxGridChartPieDiagramValueCaptionItem = (pdvciCategory, pdvciValue, pdvciPercentage);
  TcxGridChartPieDiagramValueCaptionItems = set of TcxGridChartPieDiagramValueCaptionItem;

  TcxGridChartPieDiagramValuesClass = class of TcxGridChartPieDiagramValues;
  TcxGridChartPieDiagramValues = class(TcxCustomGridChartDiagramOptions)
  strict private
    FAngleOfFirstSlice: Integer;
    FCaptionItems: TcxGridChartPieDiagramValueCaptionItems;
    FCaptionItemSeparator: string;
    FCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
    FPercentageCaptionFormat: string;

    procedure SetAngleOfFirstSlice(Value: Integer);
    procedure SetCaptionItems(Value: TcxGridChartPieDiagramValueCaptionItems);
    procedure SetCaptionItemSeparator(const Value: string);
    procedure SetCaptionPosition(Value: TcxGridChartPieDiagramValueCaptionPosition);
    procedure SetPercentageCaptionFormat(const Value: string);
  protected
    function GetDefaultCaptionItemSeparator: string; virtual;
    function GetDefaultPercentageCaptionFormat: string; virtual;
  public
    constructor Create(ADiagram: TcxGridChartDiagram); override;
    procedure Assign(Source: TPersistent); override;
    function GetCaptionItemSeparator: string;
    function GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition; virtual;
    function GetPercentageCaptionFormat: string;
  published
    property AngleOfFirstSlice: Integer read FAngleOfFirstSlice write SetAngleOfFirstSlice default 0;
    property CaptionPosition: TcxGridChartPieDiagramValueCaptionPosition read FCaptionPosition write SetCaptionPosition default pdvcpNone;
    property CaptionItems: TcxGridChartPieDiagramValueCaptionItems read FCaptionItems write SetCaptionItems default [pdvciValue];
    property CaptionItemSeparator: string read FCaptionItemSeparator write SetCaptionItemSeparator;
    property PercentageCaptionFormat: string read FPercentageCaptionFormat write SetPercentageCaptionFormat;
  end;

  { TcxGridChartPieDiagram }

  TcxGridChartPieDiagramSeriesSiteCaptionCustomDrawEvent = procedure(Sender: TcxGridChartPieDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartPieSeriesSiteCaptionViewInfo; var ADone: Boolean) of object;
  TcxGridChartPieDiagramSeriesSiteCustomDrawEvent = procedure(Sender: TcxGridChartPieDiagram;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartPieSeriesSiteViewInfo; var ADone: Boolean) of object;

  TcxGridChartPieDiagramClass = class of TcxGridChartPieDiagram;
  TcxGridChartPieDiagram = class(TcxGridChartDiagram)
  strict private
    FSeriesCaptions: Boolean;
    FSeriesColumnCount: Integer;
    FSeriesSites: Boolean;
    FValues: TcxGridChartPieDiagramValues;

    FOnCustomDrawSeriesSite: TcxGridChartPieDiagramSeriesSiteCustomDrawEvent;
    FOnCustomDrawSeriesSiteCaption: TcxGridChartPieDiagramSeriesSiteCaptionCustomDrawEvent;

    function GetStyles: TcxGridChartPieDiagramStyles;
    procedure SetSeriesCaptions(Value: Boolean);
    procedure SetSeriesColumnCount(Value: Integer);
    procedure SetSeriesSites(Value: Boolean);
    procedure SetStyles(Value: TcxGridChartPieDiagramStyles);
    procedure SetValues(Value: TcxGridChartPieDiagramValues);
    procedure SetOnCustomDrawSeriesSite(Value: TcxGridChartPieDiagramSeriesSiteCustomDrawEvent);
    procedure SetOnCustomDrawSeriesSiteCaption(Value: TcxGridChartPieDiagramSeriesSiteCaptionCustomDrawEvent);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateSubObjects(AGridView: TcxGridChartView); override;
    procedure DestroySubObjects; override;

    function GetStylesClass: TcxGridChartDiagramStylesClass; override;
    function GetValuesClass: TcxGridChartPieDiagramValuesClass; virtual;

    function GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass; override;
    function GetViewInfoClass: TcxGridChartDiagramViewInfoClass; override;

    function GetDisplayText: string; override;
    function GetImageIndex: Integer; override;

    procedure DoCustomDrawSeriesSite(ACanvas: TcxCanvas;
      AViewInfo: TcxGridChartPieSeriesSiteViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawSeriesSiteCaption(ACanvas: TcxCanvas;
      AViewInfo: TcxGridChartPieSeriesSiteCaptionViewInfo; var ADone: Boolean); virtual;
    function HasCustomDrawSeriesSite: Boolean; virtual;
    function HasCustomDrawSeriesSiteCaption: Boolean; virtual;
  public
    constructor Create(AGridView: TcxGridChartView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetSeriesColumnCount: Integer; virtual;
    function GetValueCaption(ASeries: TcxGridChartSeries; AValueIndex: Integer): string; override;
  published
    property SeriesCaptions: Boolean read FSeriesCaptions write SetSeriesCaptions default True;
    property SeriesColumnCount: Integer read FSeriesColumnCount write SetSeriesColumnCount default 0;
    property SeriesSites: Boolean read FSeriesSites write SetSeriesSites default False;
    property Styles: TcxGridChartPieDiagramStyles read GetStyles write SetStyles;
    property Values: TcxGridChartPieDiagramValues read FValues write SetValues;

    property OnCustomDrawSeriesSite: TcxGridChartPieDiagramSeriesSiteCustomDrawEvent
      read FOnCustomDrawSeriesSite write SetOnCustomDrawSeriesSite;
    property OnCustomDrawSeriesSiteCaption: TcxGridChartPieDiagramSeriesSiteCaptionCustomDrawEvent
      read FOnCustomDrawSeriesSiteCaption write SetOnCustomDrawSeriesSiteCaption;
  end;

  // chart item data binding

  TcxGridChartItemDataBindingClass = class of TcxGridChartItemDataBinding;
  TcxGridChartItemDataBinding = class(TcxInterfacedPersistent, IcxEditRepositoryItemListener)
  private
    FData: TObject;
    FDataField: TcxCustomDataField;
    FDefaultRepositoryItem: TcxEditRepositoryItem;
    FDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
    FDefaultValueTypeClass: TcxValueTypeClass;
    FGridView: TcxGridChartView;
    FID: Integer;
    FIsValue: Boolean;
    FSummaryItem: TcxDataSummaryItem;
    function GetDataController: TcxCustomDataController;
    function GetDataIndex: Integer;
    function GetDataItem: TObject;
    function GetDefaultProperties: TcxCustomEditProperties;
    function GetDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
    function GetGroupIndex: Integer;
    function GetSortIndex: Integer;
    function GetSortOrder: TcxDataSortOrder;
    function GetSummaryIndex: Integer;
    function GetSummaryKind: TcxSummaryKind;
    function GetValueType: string;
    function GetValueTypeClass: TcxValueTypeClass;
    procedure SetData(Value: TObject);
    procedure SetDataField(Value: TcxCustomDataField);
    procedure SetGroupIndex(Value: Integer);
    procedure SetSortIndex(Value: Integer);
    procedure SetSortOrder(Value: TcxDataSortOrder);
    procedure SetSummaryKind(Value: TcxSummaryKind);
    procedure SetValueType(const Value: string);
    procedure SetValueTypeClass(Value: TcxValueTypeClass);
  protected
    // IcxEditRepositoryItemListener
    procedure ItemRemoved(Sender: TcxEditRepositoryItem);
    procedure PropertiesChanged(Sender: TcxEditRepositoryItem);

    procedure CreateSummaryItem(var ASummaryItem: TcxDataSummaryItem); virtual;
    function GetDefaultDisplayText: string; virtual;
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; virtual;
    function GetDefaultValueTypeClass: TcxValueTypeClass; virtual;
    function GetSummaryItem: TcxDataSummaryItem; virtual;
    procedure InitDefaultValuesProvider(ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider); virtual;
    function IsValueTypeInteger: Boolean;
    function IsValueTypeStored: Boolean; virtual;
    procedure UpdateSummaryItemValue;
    procedure ValueTypeClassChanged; virtual;

    function GetDefaultRepositoryItem: TcxEditRepositoryItem; virtual;
    procedure UpdateDefaultRepositoryItemValue;

    property DataField: TcxCustomDataField read FDataField write SetDataField;
    property DefaultProperties: TcxCustomEditProperties read GetDefaultProperties;
    property DefaultRepositoryItem: TcxEditRepositoryItem read FDefaultRepositoryItem;
    property DefaultValuesProvider: TcxCustomEditDefaultValuesProvider read GetDefaultValuesProvider;
    property DataItem: TObject read GetDataItem;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
    property ID: Integer read FID write FID;
    property IsValue: Boolean read FIsValue;
    property SortIndex: Integer read GetSortIndex write SetSortIndex;
    property SortOrder: TcxDataSortOrder read GetSortOrder write SetSortOrder;
    property SummaryKind: TcxSummaryKind read GetSummaryKind write SetSummaryKind;
  public
    constructor Create(AGridView: TcxGridChartView; AIsValue: Boolean;
      ADefaultValueTypeClass: TcxValueTypeClass); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValueDisplayText(const AValue: Variant): string; virtual;
    function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean; virtual;
    property Data: TObject read FData write SetData;
    property DataIndex: Integer read GetDataIndex;
    property DataController: TcxCustomDataController read GetDataController;
    property GridView: TcxGridChartView read FGridView;
    property SummaryIndex: Integer read GetSummaryIndex;
    property SummaryItem: TcxDataSummaryItem read FSummaryItem;
    property ValueTypeClass: TcxValueTypeClass read GetValueTypeClass write SetValueTypeClass;
  published
    property ValueType: string read GetValueType write SetValueType stored IsValueTypeStored;
  end;

  TcxGridChartGetValueDisplayTextEvent = procedure(Sender: TObject;
    const AValue: Variant; var ADisplayText: string) of object;
  TcxGridChartItemGetStoredPropertiesEvent = procedure(Sender: TcxGridChartItem;
    AProperties: TStrings) of object;
  TcxGridChartItemGetStoredPropertyValueEvent = procedure(Sender: TcxGridChartItem;
    const AName: string; var AValue: Variant) of object;
  TcxGridChartItemSetStoredPropertyValueEvent = procedure(Sender: TcxGridChartItem;
    const AName: string; const AValue: Variant) of object;

  TcxGridChartItem = class(TcxComponent, IcxStoredObject, IcxGridChartItem)
  private
    FDataBinding: TcxGridChartItemDataBinding;
    FDisplayText: string;
    FGridView: TcxGridChartView;
    FLastDataBindingDefaultDisplayText: string;
    FOrder: Integer;
    FVisible: Boolean;
    FVisibleForCustomization: Boolean;
    FVisibleIndex: Integer;
    FOnGetStoredProperties: TcxGridChartItemGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGridChartItemGetStoredPropertyValueEvent;
    FOnGetValueDisplayText: TcxGridChartGetValueDisplayTextEvent;
    FOnSetStoredPropertyValue: TcxGridChartItemSetStoredPropertyValueEvent;
    function GetID: Integer;
    function GetIndex: Integer;
    function GetSortOrder: TcxDataSortOrder;
    function GetTag: TcxTag;
    function GetVisibleDisplayText(AIndex: Integer): string;
    procedure SetDataBinding(Value: TcxGridChartItemDataBinding);
    procedure SetDisplayText(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetOnGetStoredProperties(Value: TcxGridChartItemGetStoredPropertiesEvent);
    procedure SetOnGetStoredPropertyValue(Value: TcxGridChartItemGetStoredPropertyValueEvent);
    procedure SetOnGetValueDisplayText(Value: TcxGridChartGetValueDisplayTextEvent);
    procedure SetOnSetStoredPropertyValue(Value: TcxGridChartItemSetStoredPropertyValueEvent);
    procedure SetSortOrder(Value: TcxDataSortOrder);
    procedure SetTag(Value: TcxTag);
    procedure SetVisible(Value: Boolean);
    procedure SetVisibleForCustomization(Value: Boolean);
    function IsTagStored: Boolean;
  protected
    FSubObjectEvents: TNotifyEvent;

    // IcxStoredObject
    function GetObjectName: string; virtual;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    procedure SetRestoredIndex(AValue: Integer); virtual; abstract;
    // IcxGridChartItem
    procedure DataChanged; virtual;
    function GetDataBinding: TcxGridChartItemDataBinding;
    procedure ValueTypeClassChanged;

    procedure SetParentComponent(AParent: TComponent); override;

    procedure Changed(AChange: TcxGridViewChangeKind = vcLayout); virtual;
    procedure DisplayTextChanged; virtual;
    procedure DoGetValueDisplayText(const AValue: Variant; var ADisplayText: string); virtual;
    function GetDefaultDisplayText: string; virtual;
    function GetDefaultValueTypeClass: TcxValueTypeClass; virtual; abstract;
    procedure SetGridView(Value: TcxGridChartView);
    procedure SetName(const NewName: TComponentName); override;

    function GetValue(AIndex: Integer): Variant; virtual; abstract;
    function GetValueCount: Integer; virtual; abstract;
    function GetVisibleValue(AIndex: Integer): Variant; virtual; abstract;
    function GetVisibleValueCount: Integer; virtual; abstract;
    procedure SetValue(AIndex: Integer; const Value: Variant); virtual; abstract;
    procedure SetValueCount(Value: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    function GetDisplayText: string;
    function GetValueDisplayText(const AValue: Variant): string;
    class function IsValue: Boolean; virtual;  // abstract; - because of CLR, BCB

    property GridView: TcxGridChartView read FGridView;
    property ID: Integer read GetID;
    property Index: Integer read GetIndex write SetIndex;
    property Order: Integer read FOrder;
    property ValueCount: Integer read GetValueCount write SetValueCount;
    property Values[AIndex: Integer]: Variant read GetValue write SetValue; default;
    property VisibleDisplayTexts[AIndex: Integer]: string read GetVisibleDisplayText;
    property VisibleIndex: Integer read FVisibleIndex;
    property VisibleValueCount: Integer read GetVisibleValueCount;
    property VisibleValues[AIndex: Integer]: Variant read GetVisibleValue;
  published
    property DataBinding: TcxGridChartItemDataBinding read FDataBinding
      write SetDataBinding;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property SortOrder: TcxDataSortOrder read GetSortOrder write SetSortOrder default soNone;
    property Tag: TcxTag read GetTag write SetTag stored IsTagStored;
    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
    property OnGetStoredProperties: TcxGridChartItemGetStoredPropertiesEvent read FOnGetStoredProperties write SetOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGridChartItemGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write SetOnGetStoredPropertyValue;
    property OnGetValueDisplayText: TcxGridChartGetValueDisplayTextEvent read FOnGetValueDisplayText write SetOnGetValueDisplayText;
    property OnSetStoredPropertyValue: TcxGridChartItemSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write SetOnSetStoredPropertyValue;
  end;

  // categories

  TcxGridChartCategoriesClass = class of TcxGridChartCategories;

  TcxGridChartCategories = class(TcxCustomGridOptions, IcxGridChartItem)
  private
    FDataBinding: TcxGridChartItemDataBinding;
    FDisplayText: string;
    FOnGetValueDisplayText: TcxGridChartGetValueDisplayTextEvent;
    function GetGridView: TcxGridChartView;
    function GetID: Integer;
    function GetSortOrder: TcxDataSortOrder;
    function GetValue(Index: Integer): Variant;
    function GetValueCount: Integer;
    function GetVisibleDisplayText(Index: Integer): string;
    function GetVisibleValue(Index: Integer): Variant;
    function GetVisibleValueCount: Integer;
    procedure SetDataBinding(Value: TcxGridChartItemDataBinding);
    procedure SetDisplayText(const Value: string);
    procedure SetOnGetValueDisplayText(Value: TcxGridChartGetValueDisplayTextEvent);
    procedure SetSortOrder(Value: TcxDataSortOrder);
    procedure SetValue(Index: Integer; const Value: Variant);
    procedure SetValueCount(Value: Integer);
  protected
    // IcxGridChartItem
    procedure DataChanged; virtual;
    function GetDataBinding: TcxGridChartItemDataBinding;
    procedure ValueTypeClassChanged;

    procedure DoGetValueDisplayText(const AValue: Variant; var ADisplayText: string); virtual;
    function GetDefaultDisplayText: string; virtual;
    function GetDefaultValueTypeClass: TcxValueTypeClass; virtual;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayText: string;
    function GetNamePath: string; override;
    function GetValueDisplayText(const AValue: Variant): string;

    property GridView: TcxGridChartView read GetGridView;
    property ID: Integer read GetID;
    property ValueCount: Integer read GetValueCount write SetValueCount;
    property Values[Index: Integer]: Variant read GetValue write SetValue; default;
    property VisibleDisplayTexts[Index: Integer]: string read GetVisibleDisplayText;
    property VisibleValueCount: Integer read GetVisibleValueCount;
    property VisibleValues[Index: Integer]: Variant read GetVisibleValue;
  published
    property DataBinding: TcxGridChartItemDataBinding read FDataBinding
      write SetDataBinding;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property SortOrder: TcxDataSortOrder read GetSortOrder write SetSortOrder default soNone;
    property OnGetValueDisplayText: TcxGridChartGetValueDisplayTextEvent read FOnGetValueDisplayText
      write SetOnGetValueDisplayText;
  end;

  // data groups

  TcxGridChartDataGroupClass = class of TcxGridChartDataGroup;

  TcxGridChartDataGroup = class(TcxGridChartItem)
  private
    FActiveValue: Variant;
    function GetActive: Boolean;
    function GetActiveValueDisplayText: string;
    function GetDataLevel: Integer;
    function GetGroupIndex: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetActiveValue(const Value: Variant);
    procedure SetDataLevel(Value: Integer);
    procedure SetGroupIndex(Value: Integer);
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    procedure SetRestoredIndex(AValue: Integer); override;

    procedure CheckActiveValue;
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;

    function GetValue(AIndex: Integer): Variant; override;
    function GetValueCount: Integer; override;
    function GetVisibleValue(AIndex: Integer): Variant; override;
    function GetVisibleValueCount: Integer; override;
    procedure SetValue(AIndex: Integer; const Value: Variant); override;
    procedure SetValueCount(Value: Integer); override;

    function CanMove: Boolean; virtual;

    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
  public
    function HasActiveValue: Boolean;
    class function IsValue: Boolean; override;
    property Active: Boolean read GetActive write SetActive;
    property ActiveValue: Variant read FActiveValue write SetActiveValue;
    property ActiveValueDisplayText: string read GetActiveValueDisplayText;
    property DataLevel: Integer read GetDataLevel write SetDataLevel;
  published
    property SortOrder default soAscending;
  end;

  // series

  TcxGridChartSeriesGetValueStyleEvent = procedure(Sender: TcxGridChartSeries;
    AValueIndex: Integer; var AStyle: TcxStyle) of object;

  TcxGridChartSeriesStylesClass = class of TcxGridChartSeriesStyles;

  TcxGridChartSeriesStyles = class(TcxCustomGridStyles)
  private
    FOnGetValueStyle: TcxGridChartSeriesGetValueStyleEvent;
    function GetSeries: TcxGridChartSeries;
    procedure SetOnGetValueStyle(Value: TcxGridChartSeriesGetValueStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetGridView: TcxCustomGridView; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetValueParams(AValueIndex: Integer; AVaryColorsByCategory: Boolean;
      out AParams: TcxViewParams); virtual;
    property Series: TcxGridChartSeries read GetSeries;
  published
    property Values: TcxStyle index ssValues read GetValue write SetValue;
    property OnGetValueStyle: TcxGridChartSeriesGetValueStyleEvent read FOnGetValueStyle
      write SetOnGetValueStyle;
  end;

  TcxGridChartSeriesValueCustomDrawEvent = procedure(Sender: TcxGridChartSeries;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean) of object;
  TcxGridChartValueClickEvent = procedure(Sender: TcxGridChartView; ASeries: TcxGridChartSeries;
    AValueIndex: Integer; var AHandled: Boolean) of object;

  TcxGridChartSeriesClass = class of TcxGridChartSeries;

  TcxGridChartSeries = class(TcxGridChartItem)
  private
    FGroupIndex: Integer;
    FStyles: TcxGridChartSeriesStyles;
    FValueCaptionFormat: string;
    FVisibleGroupIndex: Integer;
    FOnCustomDrawValue: TcxGridChartSeriesValueCustomDrawEvent;
    FOnValueClick: TcxGridChartValueClickEvent;
    function GetGroupSummaryKind: TcxSummaryKind;
    function GetSumOfValues: Variant;
    procedure SetGroupIndex(Value: Integer);
    procedure SetGroupSummaryKind(Value: TcxSummaryKind);
    procedure SetStyles(Value: TcxGridChartSeriesStyles);
    procedure SetValueCaptionFormat(const Value: string);
    procedure SetOnCustomDrawValue(Value: TcxGridChartSeriesValueCustomDrawEvent);
    procedure SetOnValueClick(Value: TcxGridChartValueClickEvent);
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    procedure SetRestoredIndex(AValue: Integer); override;

    procedure DoGetValueDisplayText(const AValue: Variant; var ADisplayText: string); override;
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;
    function GetStylesClass: TcxGridChartSeriesStylesClass; virtual;
    function IsGroupSummaryKindValid(AValue: TcxSummaryKind): Boolean; virtual;

    function GetValue(AIndex: Integer): Variant; override;
    function GetValueCount: Integer; override;
    function GetVisibleValue(AIndex: Integer): Variant; override;
    function GetVisibleValueCount: Integer; override;
    procedure SetValue(AIndex: Integer; const Value: Variant); override;
    procedure SetValueCount(Value: Integer); override;

    procedure DoCustomDrawValue(ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueViewInfo;
      var ADone: Boolean); virtual;
    function DoValueClick(AValueIndex: Integer): Boolean; virtual;
    function HasCustomDrawValue: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddValue(const AValue: Variant): Integer;
    class function IsValue: Boolean; override;

    property SumOfValues: Variant read GetSumOfValues;
    property VisibleGroupIndex: Integer read FVisibleGroupIndex;
  published
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property GroupSummaryKind: TcxSummaryKind read GetGroupSummaryKind write SetGroupSummaryKind
      default cxGridChartItemDefaultSummaryKind;
    property Styles: TcxGridChartSeriesStyles read FStyles write SetStyles;
    property ValueCaptionFormat: string read FValueCaptionFormat write SetValueCaptionFormat;

    property StylesEvents: TNotifyEvent read FSubObjectEvents write FSubObjectEvents;

    property OnCustomDrawValue: TcxGridChartSeriesValueCustomDrawEvent read FOnCustomDrawValue
      write SetOnCustomDrawValue;
    property OnValueClick: TcxGridChartValueClickEvent read FOnValueClick write SetOnValueClick;
  end;

  TcxGridChartItemKind = (cikItem, cikSeries, cikDataGroup);

  // view title

  TcxGridChartTitleClass = class of TcxGridChartTitle;

  TcxGridChartTitle = class(TcxCustomGridChartTitle)
  protected
    function GetDefaultPosition: TcxGridChartPartPosition; override;
  published
    property Position;
  end;

  // view

  TcxGridChartDataLevelsInfoVisible = (dlivNever, dlivNonEmpty, dlivAlways);
  TcxGridChartToolBoxBorder = (tbNone, tbSingle);
  TcxGridChartToolBoxPosition = (tpTop, tpBottom);
  TcxGridChartToolBoxVisible = (tvNever, tvNonEmpty, tvAlways);

  TcxGridChartToolBoxClass = class of TcxGridChartToolBox;

  TcxGridChartToolBox = class(TcxCustomGridOptions)
  private
    FBorder: TcxGridChartToolBoxBorder;
    FCustomizeButton: Boolean;
    FDataLevelActiveValueDropDownCount: Integer;
    FDataLevelActiveValueDropDownWidth: Integer;
    FDataLevelsInfoVisible: TcxGridChartDataLevelsInfoVisible;
    FDiagramSelector: Boolean;
    FPosition: TcxGridChartToolBoxPosition;
    FVisible: TcxGridChartToolBoxVisible;
    function GetGridView: TcxGridChartView;
    procedure SetBorder(Value: TcxGridChartToolBoxBorder);
    procedure SetCustomizeButton(Value: Boolean);
    procedure SetDataLevelActiveValueDropDownCount(Value: Integer);
    procedure SetDataLevelActiveValueDropDownWidth(Value: Integer);
    procedure SetDataLevelsInfoVisible(Value: TcxGridChartDataLevelsInfoVisible);
    procedure SetDiagramSelector(Value: Boolean);
    procedure SetPosition(Value: TcxGridChartToolBoxPosition);
    procedure SetVisible(Value: TcxGridChartToolBoxVisible);
  protected
    procedure GetStoredProperties(AProperties: TStrings); override;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); override;

    function IsDataLevelsInfoNonEmpty: Boolean; virtual;
    function IsNonEmpty: Boolean; virtual;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetDataLevelsInfoVisible: Boolean;
    function GetVisible: Boolean;
    property GridView: TcxGridChartView read GetGridView;
  published
    property Border: TcxGridChartToolBoxBorder read FBorder write SetBorder default tbSingle;
    property CustomizeButton: Boolean read FCustomizeButton write SetCustomizeButton default False;
    property DataLevelActiveValueDropDownCount: Integer read FDataLevelActiveValueDropDownCount
      write SetDataLevelActiveValueDropDownCount default cxGridChartDefaultDataLevelActiveValueDropDownCount;
    property DataLevelActiveValueDropDownWidth: Integer read FDataLevelActiveValueDropDownWidth
      write SetDataLevelActiveValueDropDownWidth default 0;
    property DataLevelsInfoVisible: TcxGridChartDataLevelsInfoVisible read FDataLevelsInfoVisible
      write SetDataLevelsInfoVisible default dlivNonEmpty;
    property DiagramSelector: Boolean read FDiagramSelector write SetDiagramSelector default False;
    property Position: TcxGridChartToolBoxPosition read FPosition write SetPosition default tpTop;
    property Visible: TcxGridChartToolBoxVisible read FVisible write SetVisible default tvNonEmpty;
  end;

  TcxGridChartValueHotTrack = (vhDefault, vhNever, vhAlways);

  TcxGridChartOptionsBehavior = class(TcxCustomGridOptionsBehavior)
  private
    FValueHints: Boolean;
    FValueHotTrack: TcxGridChartValueHotTrack;
    function GetGridView: TcxGridChartView;
    procedure SetValueHints(Value: Boolean);
    procedure SetValueHotTrack(Value: TcxGridChartValueHotTrack);
  protected
    procedure GetStoredProperties(AProperties: TStrings); override;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); override;

    function GetDefaultValueHotTrack(AValueIndex: Integer): Boolean; virtual;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetValueHotTrack(AValueIndex: Integer): Boolean; virtual;
    property GridView: TcxGridChartView read GetGridView;
  published
    property HintHidePause;
    property SuppressHintOnMouseDown;
    property ValueHints: Boolean read FValueHints write SetValueHints default True;
    property ValueHotTrack: TcxGridChartValueHotTrack read FValueHotTrack write SetValueHotTrack default vhDefault;
  end;

  TcxGridChartDataDrillUpMethod = (ddumNone, ddumValueMouseRightButtonClick, ddumMouseRightButtonClick);

  TcxGridChartOptionsCustomizeClass = class of TcxGridChartOptionsCustomize;

  TcxGridChartOptionsCustomize = class(TcxCustomGridOptions)
  private
    FDataDrillDown: Boolean;
    FDataDrillUpMethod: TcxGridChartDataDrillUpMethod;
    FDataGroupHiding: Boolean;
    FDataGroupMoving: Boolean;
    FOptionsCustomization: Boolean;
    FSeriesCustomization: Boolean;
    procedure SetDataDrillDown(Value: Boolean);
    procedure SetDataDrillUpMethod(Value: TcxGridChartDataDrillUpMethod);
    procedure SetDataGroupHiding(Value: Boolean);
    procedure SetDataGroupMoving(Value: Boolean);
    procedure SetOptionsCustomization(Value: Boolean);
    procedure SetSeriesCustomization(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DataDrillDown: Boolean read FDataDrillDown write SetDataDrillDown default True;
    property DataDrillUpMethod: TcxGridChartDataDrillUpMethod read FDataDrillUpMethod
      write SetDataDrillUpMethod default ddumMouseRightButtonClick;
    property DataGroupHiding: Boolean read FDataGroupHiding write SetDataGroupHiding default False;
    property DataGroupMoving: Boolean read FDataGroupMoving write SetDataGroupMoving default True;
    property OptionsCustomization: Boolean read FOptionsCustomization write SetOptionsCustomization default True;
    property SeriesCustomization: Boolean read FSeriesCustomization write SetSeriesCustomization default True;
  end;

  TcxGridChartOptionsViewClass = class of TcxGridChartOptionsView;

  TcxGridChartOptionsView = class(TcxCustomGridOptionsView)
  private
    FAntialiasing: Boolean;
    FCategoriesPerPage: Integer;
    FTransparentCaptions: Boolean;
    function GetGridView: TcxGridChartView;
    procedure SetAntialiasing(Value: Boolean);
    procedure SetCategoriesPerPage(Value: Integer);
    procedure SetTransparentCaptions(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxGridChartView read GetGridView;
  published
    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default True;
    property CategoriesPerPage: Integer read FCategoriesPerPage write SetCategoriesPerPage default 0;
    property TransparentCaptions: Boolean read FTransparentCaptions write SetTransparentCaptions default True;
  end;

  TcxGridChartViewStyles = class(TcxCustomGridViewStyles)
  private
    function GetGridViewValue: TcxGridChartView;
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetDataLevelInfoParams(ADataLevel: Integer; out AParams: TcxViewParams); virtual;
    property GridView: TcxGridChartView read GetGridViewValue;
  published
    property ActiveDataLevelInfo: TcxStyle index vsActiveDataLevelInfo read GetValue write SetValue;
    property DataLevelActiveValueInfo: TcxStyle index vsDataLevelActiveValueInfo read GetValue write SetValue;
    property DataLevelsInfo: TcxStyle index vsDataLevelsInfo read GetValue write SetValue;
    property DiagramSelector: TcxStyle index vsDiagramSelector read GetValue write SetValue;
    property Legend: TcxStyle index vsLegend read GetValue write SetValue;
    property Title: TcxStyle index vsTitle read GetValue write SetValue;
    property ToolBox: TcxStyle index vsToolBox read GetValue write SetValue;
  end;

  TcxGridOpenChartItemList = class(TcxOpenList)
  private
    function GetItem(Index: Integer): TcxGridChartItem;
    procedure SetItem(Index: Integer; Value: TcxGridChartItem);
  public
    property Items[Index: Integer]: TcxGridChartItem read GetItem write SetItem; default;
  end;

  TcxGridChartDataGroupEvent = procedure(Sender: TcxGridChartView; ADataGroup: TcxGridChartDataGroup) of object;
  TcxGridChartDiagramEvent = procedure(Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram) of object;
  TcxGridChartGetValueHintEvent = procedure(Sender: TcxGridChartView;
    ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: string) of object;
  TcxGridChartLegendCustomDrawEvent = procedure(Sender: TcxGridChartView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean) of object;
  TcxGridChartLegendItemCustomDrawEvent = procedure(Sender: TcxGridChartView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean) of object;
  TcxGridChartSeriesEvent = procedure(Sender: TcxGridChartView; ASeries: TcxGridChartSeries) of object;

(*

User can customize:
  ToolBox:
    ActiveDiagram
    ActiveDataLevel
    DataGroups:
      ActiveValue
      Index
      Visible
  CustomizationForm:
    Series:
      Index
      Visible
      SortOrder
    DataGroups:
      Visible
    Options:
      Legend:
        Position
        Alignment
        Orientation
        Border
        KeyBorder
      Title:
        Position
        Alignment
      ToolBox:
        Position
        Border
        DiagramSelector
      Other:
        ValueHints
  DataDrilling:
    ActiveDataLevel
    DataGroups:
      ActiveValue

Store/Restore:
  View:
    ActiveDiagram
    ActiveDataLevel
    Options:
      Legend:
        Position
        Alignment
        Orientation
        Border
        KeyBorder
      Title:
        Position
        Alignment
      ToolBox:
        Position
        Border
        DiagramSelector
      Other:
        ValueHints
  DataGroup:
    ActiveValue
    Index
    Visible
  Series:
    Index
    Visible
    SortOrder

*)

  TcxGridChartView = class(TcxCustomGridView)
  private
    FActiveDataLevel: Integer;
    FActiveDiagram: TcxGridChartDiagram;
    FAvailableDiagrams: TList;
    FCategories: TcxGridChartCategories;
    FDataGroupActiveValuesUpdateNeeded: Boolean;
    FDataGroups: TList;
    FDiagramArea: TcxGridChartAreaDiagram;
    FDiagramBar: TcxGridChartBarDiagram;
    FDiagramStackedArea: TcxGridChartStackedAreaDiagram;
    FDiagramStackedBar: TcxGridChartStackedBarDiagram;
    FDiagramStackedColumn: TcxGridChartStackedColumnDiagram;
    FDiagramColumn: TcxGridChartColumnDiagram;
    FDiagramLine: TcxGridChartLineDiagram;
    FDiagramPie: TcxGridChartPieDiagram;
    FDiagrams: TList;
    FIDs: TList;
    FLegend: TcxGridChartLegend;
    FOptionsCustomize: TcxGridChartOptionsCustomize;
    FRestoredActiveDataLevel: Integer;
    FRestoringDataGroups: TcxGridOpenChartItemList;
    FRestoringSeries: TcxGridOpenChartItemList;
    FSeries: TList;
    FTitle: TcxGridChartTitle;
    FToolBox: TcxGridChartToolBox;
    FVisibleDataGroups: TList;
    FVisibleSeries: TList;
    FVisibleSeriesGroups: TList;

    FOnActiveDataLevelChanged: TNotifyEvent;
    FOnActiveDiagramChanged: TcxGridChartDiagramEvent;
    FOnCustomDrawLegend: TcxGridChartLegendCustomDrawEvent;
    FOnCustomDrawLegendItem: TcxGridChartLegendItemCustomDrawEvent;
    FOnDataGroupPosChanged: TcxGridChartDataGroupEvent;
    FOnFirstVisibleCategoryIndexChanged: TNotifyEvent;
    FOnGetValueHint: TcxGridChartGetValueHintEvent;
    FOnSeriesPosChanged: TcxGridChartSeriesEvent;
    FOnValueClick: TcxGridChartValueClickEvent;
    FSubClassEvents: TNotifyEvent;

    function GetActiveDataGroup: TcxGridChartDataGroup;
    function GetAvailableDiagram(Index: Integer): TcxGridChartDiagram;
    function GetAvailableDiagramCount: Integer;
    function GetController: TcxGridChartController;
    function GetDataController: TcxGridChartDataController;
    function GetDataGroup(Index: Integer): TcxGridChartDataGroup;
    function GetDataGroupCount: Integer;
    function GetDiagram(Index: Integer): TcxGridChartDiagram;
    function GetDiagramCount: Integer;
    function GetItem(Index: Integer): IcxGridChartItem;
    function GetItemCount: Integer;
    function GetOptionsBehavior: TcxGridChartOptionsBehavior;
    function GetOptionsView: TcxGridChartOptionsView;
    function GetSeries(Index: Integer): TcxGridChartSeries;
    function GetSeriesCount: Integer;
    function GetSortedSeries: TcxGridChartSeries;
    function GetStyles: TcxGridChartViewStyles;
    function GetViewData: TcxGridChartViewData;
    function GetViewInfo: TcxGridChartViewInfo;
    function GetVisibleDataGroup(Index: Integer): TcxGridChartDataGroup;
    function GetVisibleDataGroupCount: Integer;
    function GetVisibleSeries(Index: Integer): TcxGridChartSeries;
    function GetVisibleSeriesCount: Integer;
    function GetVisibleSeriesGroupCount: Integer;
    function GetVisibleSeriesGroupIndex(Index: Integer): Integer;
    procedure SetActiveDataGroup(Value: TcxGridChartDataGroup);
    procedure SetActiveDataLevel(Value: Integer);
    procedure SetActiveDiagram(Value: TcxGridChartDiagram);
    procedure SetCategories(Value: TcxGridChartCategories);
    procedure SetDataController(Value: TcxGridChartDataController);
    procedure SetDataGroup(Index: Integer; Value: TcxGridChartDataGroup);
    procedure SetDiagramArea(Value: TcxGridChartAreaDiagram);
    procedure SetDiagramBar(Value: TcxGridChartBarDiagram);
    procedure SetDiagramColumn(Value: TcxGridChartColumnDiagram);
    procedure SetDiagramLine(Value: TcxGridChartLineDiagram);
    procedure SetDiagramPie(Value: TcxGridChartPieDiagram);
    procedure SetDiagramStackedArea(Value: TcxGridChartStackedAreaDiagram);
    procedure SetDiagramStackedBar(Value: TcxGridChartStackedBarDiagram);
    procedure SetDiagramStackedColumn(Value: TcxGridChartStackedColumnDiagram);
    procedure SetLegend(Value: TcxGridChartLegend);
    procedure SetOptionsBehavior(Value: TcxGridChartOptionsBehavior);
    procedure SetOptionsCustomize(Value: TcxGridChartOptionsCustomize);
    procedure SetOptionsView(Value: TcxGridChartOptionsView);
    procedure SetSeries(Index: Integer; Value: TcxGridChartSeries);
    procedure SetSortedSeries(Value: TcxGridChartSeries);
    procedure SetStyles(Value: TcxGridChartViewStyles);
    procedure SetTitle(Value: TcxGridChartTitle);
    procedure SetToolBox(Value: TcxGridChartToolBox);
    procedure SetOnActiveDataLevelChanged(Value: TNotifyEvent);
    procedure SetOnActiveDiagramChanged(Value: TcxGridChartDiagramEvent);
    procedure SetOnCustomDrawLegend(Value: TcxGridChartLegendCustomDrawEvent);
    procedure SetOnCustomDrawLegendItem(Value: TcxGridChartLegendItemCustomDrawEvent);
    procedure SetOnDataGroupPosChanged(Value: TcxGridChartDataGroupEvent);
    procedure SetOnFirstVisibleCategoryIndexChanged(Value: TNotifyEvent);
    procedure SetOnGetValueHint(Value: TcxGridChartGetValueHintEvent);
    procedure SetOnSeriesPosChanged(Value: TcxGridChartSeriesEvent);
    procedure SetOnValueClick(Value: TcxGridChartValueClickEvent);

  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IcxStoredParent
    function CreateStoredObject(const AObjectName, AClassName: string): TObject; override;
    procedure GetStoredChildren(AChildren: TStringList); override;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    function GetLayoutCustomizationFormButtonCaption: string; override;
    function HasLayoutCustomizationForm: Boolean; override;

    procedure CreateHandlers; override;
    procedure DestroyHandlers; override;
    procedure CreateOptions; override;
    procedure DestroyOptions; override;

    procedure ActiveDataLevelChanged(APrevActiveDataLevel: Integer); virtual;
    procedure ActiveDiagramChanged(ADiagram: TcxGridChartDiagram); virtual;
    procedure BeforeAssign(ASource: TcxCustomGridView); override;
    procedure AfterAssign(ASource: TcxCustomGridView); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure BeforeRestoring; override;
    procedure AfterRestoring; override;
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DataControllerUnlocked; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure GetFakeComponentLinks(AList: TList); override;
    procedure RestoringComplete; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;

    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemDataBindingClass: TcxGridChartItemDataBindingClass; virtual;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    function GetLegendClass: TcxGridChartLegendClass; virtual;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxGridChartOptionsCustomizeClass; virtual;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;
    function GetTitleClass: TcxGridChartTitleClass; virtual;
    function GetToolBoxClass: TcxGridChartToolBoxClass; virtual;

    function GetAreaDiagramClass: TcxGridChartAreaDiagramClass; virtual;
    function GetBarDiagramClass: TcxGridChartBarDiagramClass; virtual;
    function GetCategoriesClass: TcxGridChartCategoriesClass; virtual;
    function GetColumnDiagramClass: TcxGridChartColumnDiagramClass; virtual;
    function GetLineDiagramClass: TcxGridChartLineDiagramClass; virtual;
    function GetPieDiagramClass: TcxGridChartPieDiagramClass; virtual;
    function GetStackedAreaDiagramClass: TcxGridChartStackedAreaDiagramClass; virtual;
    function GetStackedColumnDiagramClass: TcxGridChartStackedColumnDiagramClass; virtual;
    function GetStackedBarDiagramClass: TcxGridChartStackedBarDiagramClass; virtual;

    function GetCategoriesNamePath: string; virtual;

    procedure AddDiagram(ADiagram: TcxGridChartDiagram);
    procedure RemoveDiagram(ADiagram: TcxGridChartDiagram);
    procedure ClearDiagrams;
    function CreateDiagram(ADiagramClass: TcxGridChartDiagramClass): TcxGridChartDiagram;
    procedure CreateDiagrams; virtual;
    procedure DiagramRemoved(ADiagram: TcxGridChartDiagram); virtual;
    function GetDiagramNamePath(ADiagram: TcxGridChartDiagram): string; virtual;
    function GetFirstAvailableDiagram: TcxGridChartDiagram;
    procedure RefreshAvailableDiagramList;

    function GetNextID: Integer;
    procedure ReleaseID(AID: Integer);

    procedure DataSortingChanged(AItem: TObject); virtual;
    procedure DataSortingChanging(AItem: TObject); virtual;
    procedure UpdateDataController(AItem: TObject; ADataBinding: TcxGridChartItemDataBinding; AAdd: Boolean);
    procedure UpdateSummaryItemValues;

    function GetItemList(AItemClass: TcxGridChartItemClass): TList; overload; virtual;
    function GetItemList(AItemKind: TcxGridChartItemKind): TList; overload; virtual;
    function GetItemList(AItem: TcxGridChartItem): TList; overload;
    function GetVisibleItemList(AItem: TcxGridChartItem): TList; virtual;
    procedure AddItem(AItem: TcxGridChartItem);
    procedure RemoveItem(AItem: TcxGridChartItem);
    function GetItemIndex(AItem: TcxGridChartItem): Integer;
    procedure SetItemIndex(AItem: TcxGridChartItem; AIndex: Integer);
    procedure DataGroupVisibilityChanged(ADataGroup: TcxGridChartDataGroup); virtual;
    procedure ItemDisplayTextChanged(AItem: TcxGridChartItem); virtual;
    procedure ItemIndexChanged(AItem: TcxGridChartItem); virtual;
    procedure ItemPosChanged(AItem: TcxGridChartItem); virtual;
    procedure ItemVisibilityChanged(AItem: TcxGridChartItem); virtual;
    procedure ItemVisibilityForCustomizationChanged(AItem: TcxGridChartItem); virtual;
    procedure SeriesVisibilityChanged(ASeries: TcxGridChartSeries); virtual;
    procedure RefreshVisibleItemsList(AItems, AVisibleItems: TList);
    procedure UpdateItemsOrder(AItemClass: TcxGridChartItemClass);
    procedure UpdateSeriesVisibleGroups;

    procedure ClearItems(AItemClass: TcxGridChartItemClass);
    function CreateItem(AItemClass: TcxGridChartItemClass): TcxGridChartItem;
    function FindItemByID(AItemClass: TcxGridChartItemClass; AID: Integer): TcxGridChartItem;
    function FindItemByName(AItemClass: TcxGridChartItemClass; const AName: string): TcxGridChartItem;
    function FindItemByTag(AItemClass: TcxGridChartItemClass; ATag: TcxTag): TcxGridChartItem;

    function GetAvailableDataLevel(ALevel: Integer): Integer;
    function GetDataLevelCount: Integer; virtual;
    function GetDataLevelObject(Index: Integer): TcxGridChartDataGroup; virtual;
    function GetDataObjectLevel(AObject: TcxGridChartDataGroup): Integer; virtual;
    procedure SetDataObjectLevel(AObject: TcxGridChartDataGroup; ALevel: Integer); virtual;
    function GetIsDataGrouped: Boolean; virtual;
    procedure UpdateDataGroupActiveValues;
    procedure UpdateDataLevels;
    procedure UpdateDataSortingBySummary;

    procedure CalculateImageWidth(var AWidth: Integer); virtual;
    procedure CalculateImageHeight(var AHeight: Integer); virtual;

    procedure DoActiveDataLevelChanged; virtual;
    procedure DoActiveDiagramChanged(ADiagram: TcxGridChartDiagram); virtual;
    procedure DoCustomDrawLegend(ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawLegendItem(ACanvas: TcxCanvas; AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean); virtual;
    procedure DoDataGroupPosChanged(ADataGroup: TcxGridChartDataGroup); virtual;
    procedure DoFirstVisibleCategoryIndexChanged; virtual;
    procedure DoGetValueHint(ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: string); virtual;
    procedure DoSeriesPosChanged(ASeries: TcxGridChartSeries); virtual;
    function DoValueClick(ASeries: TcxGridChartSeries; AValueIndex: Integer): Boolean; virtual;
    function HasCustomDrawLegend: Boolean; virtual;
    function HasCustomDrawLegendItem: Boolean; virtual;

    property DataGroupActiveValuesUpdateNeeded: Boolean read FDataGroupActiveValuesUpdateNeeded;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: IcxGridChartItem read GetItem;
    property RestoringDataGroups: TcxGridOpenChartItemList read FRestoringDataGroups;
    property RestoringSeries: TcxGridOpenChartItemList read FRestoringSeries;
  public
    // base
    function CreateImage(AGraphicClass: TGraphicClass; AWidth: Integer = 0; AHeight: Integer = 0): TGraphic;
    property Controller: TcxGridChartController read GetController;
    property ViewData: TcxGridChartViewData read GetViewData;
    property ViewInfo: TcxGridChartViewInfo read GetViewInfo;

    // diagrams
    function FindDiagramByDisplayText(const ADisplayText: string): TcxGridChartDiagram;
    function FindDiagramByID(const AID: string): TcxGridChartDiagram;
    property AvailableDiagramCount: Integer read GetAvailableDiagramCount;
    property AvailableDiagrams[Index: Integer]: TcxGridChartDiagram read GetAvailableDiagram;
    property DiagramCount: Integer read GetDiagramCount;
    property Diagrams[Index: Integer]: TcxGridChartDiagram read GetDiagram;

    // series
    procedure ClearSeries;
    function CreateSeries: TcxGridChartSeries;
    function FindSeriesByID(AID: Integer): TcxGridChartSeries;
    function FindSeriesByName(const AName: string): TcxGridChartSeries;
    function FindSeriesByTag(ATag: TcxTag): TcxGridChartSeries;
    function GetSeriesClass: TcxGridChartSeriesClass; virtual;

    property Series[Index: Integer]: TcxGridChartSeries read GetSeries write SetSeries;
    property SeriesCount: Integer read GetSeriesCount;
    property SortedSeries: TcxGridChartSeries read GetSortedSeries write SetSortedSeries;
    property VisibleSeries[Index: Integer]: TcxGridChartSeries read GetVisibleSeries;
    property VisibleSeriesCount: Integer read GetVisibleSeriesCount;
    property VisibleSeriesGroupCount: Integer read GetVisibleSeriesGroupCount;
    property VisibleSeriesGroupIndex[Index: Integer]: Integer read GetVisibleSeriesGroupIndex;

    // data groups
    procedure ClearDataGroups;
    function CreateDataGroup: TcxGridChartDataGroup;
    function FindDataGroupByID(AID: Integer): TcxGridChartDataGroup;
    function FindDataGroupByName(const AName: string): TcxGridChartDataGroup;
    function FindDataGroupByTag(ATag: TcxTag): TcxGridChartDataGroup;
    function GetDataGroupClass: TcxGridChartDataGroupClass; virtual;

    function CanActivateDataLevel(ALevel: Integer): Boolean;

    property ActiveDataGroup: TcxGridChartDataGroup read GetActiveDataGroup write SetActiveDataGroup;
    property ActiveDataLevel: Integer read FActiveDataLevel write SetActiveDataLevel;
    property DataLevelCount: Integer read GetDataLevelCount;
    property DataLevelObjects[Index: Integer]: TcxGridChartDataGroup read GetDataLevelObject;
    property DataGroupCount: Integer read GetDataGroupCount;
    property DataGroups[Index: Integer]: TcxGridChartDataGroup read GetDataGroup write SetDataGroup;
    property IsDataGrouped: Boolean read GetIsDataGrouped;
    property VisibleDataGroupCount: Integer read GetVisibleDataGroupCount;
    property VisibleDataGroups[Index: Integer]: TcxGridChartDataGroup read GetVisibleDataGroup;
  published
    property ActiveDiagram: TcxGridChartDiagram read FActiveDiagram write SetActiveDiagram stored False;
    property Categories: TcxGridChartCategories read FCategories write SetCategories;
    property DataController: TcxGridChartDataController read GetDataController write SetDataController;
    property DiagramArea: TcxGridChartAreaDiagram read FDiagramArea write SetDiagramArea;
    property DiagramBar: TcxGridChartBarDiagram read FDiagramBar write SetDiagramBar;
    property DiagramColumn: TcxGridChartColumnDiagram read FDiagramColumn write SetDiagramColumn;
    property DiagramLine: TcxGridChartLineDiagram read FDiagramLine write SetDiagramLine;
    property DiagramPie: TcxGridChartPieDiagram read FDiagramPie write SetDiagramPie;
    property DiagramStackedArea: TcxGridChartStackedAreaDiagram read FDiagramStackedArea write SetDiagramStackedArea;
    property DiagramStackedBar: TcxGridChartStackedBarDiagram read FDiagramStackedBar write SetDiagramStackedBar;
    property DiagramStackedColumn: TcxGridChartStackedColumnDiagram read FDiagramStackedColumn write SetDiagramStackedColumn;
    property Legend: TcxGridChartLegend read FLegend write SetLegend;
    property OptionsBehavior: TcxGridChartOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxGridChartOptionsCustomize read FOptionsCustomize write SetOptionsCustomize;
    property OptionsView: TcxGridChartOptionsView read GetOptionsView write SetOptionsView;
    property Styles: TcxGridChartViewStyles read GetStyles write SetStyles;
    property Title: TcxGridChartTitle read FTitle write SetTitle;
    property ToolBox: TcxGridChartToolBox read FToolBox write SetToolBox;

    property CategoriesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramAreaEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramBarEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramColumnEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramLineEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramPieEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramStackedAreaEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramStackedBarEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DiagramStackedColumnEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;

    property OnActiveDataLevelChanged: TNotifyEvent read FOnActiveDataLevelChanged write SetOnActiveDataLevelChanged;
    property OnActiveDiagramChanged: TcxGridChartDiagramEvent read FOnActiveDiagramChanged write SetOnActiveDiagramChanged;
    property OnCustomDrawLegend: TcxGridChartLegendCustomDrawEvent read FOnCustomDrawLegend write SetOnCustomDrawLegend;
    property OnCustomDrawLegendItem: TcxGridChartLegendItemCustomDrawEvent read FOnCustomDrawLegendItem write SetOnCustomDrawLegendItem;
    property OnCustomization;
    property OnDataGroupPosChanged: TcxGridChartDataGroupEvent read FOnDataGroupPosChanged write SetOnDataGroupPosChanged;
    property OnFirstVisibleCategoryIndexChanged: TNotifyEvent read FOnFirstVisibleCategoryIndexChanged write SetOnFirstVisibleCategoryIndexChanged;
    property OnGetValueHint: TcxGridChartGetValueHintEvent read FOnGetValueHint write SetOnGetValueHint;
    property OnInitStoredObject;
    property OnSeriesPosChanged: TcxGridChartSeriesEvent read FOnSeriesPosChanged write SetOnSeriesPosChanged;
    property OnValueClick: TcxGridChartValueClickEvent read FOnValueClick write SetOnValueClick;
  end;

var
  cxGridChartDiagramImages: TcxImageList;

procedure cxGridChartDiagramImages_Add(const AResourceName: string);

function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;

implementation

{$R cxGridChartView.res}

uses
  Types, StrUtils,
{$IFNDEF NONDB}
  FMTBcd,
{$ENDIF}
  SysUtils, Math, ImgList,
  cxEditDataRegisteredRepositoryItems, cxGrid, cxGridLevel, cxGridStrs,
  cxTextEdit, cxLibraryConsts, cxGeometry, DateUtils, dxDPIAwareUtils;

const
  ChartPartOffset = 10;

  MarkWidth = 1;
  CategoryGridLineWidth = 1;
  ValueGridLineWidth = 1;
  ValueAxisWidth = CategoryGridLineWidth;
  CategoryAxisWidth = ValueGridLineWidth;

  TickMarkLabelsOffset = 10;

  ColumnDiagramValueOffsetUnits = 3;
  ColumnDiagramValueSizeUnits = 4;

  LineDiagramCaptionOffset = 2;
  LineDiagramValueSizeUnits = 2;
  LineDiagramLineHotZoneMinWidth = 5;

  AreaDiagramLegendKeyOffset = 2;
  AreaDiagramLegendKeySpace = 3;

  PieMinSize = 100;
  PieSeriesSiteOffset = 10;
  PieAreaOffset = 20;
  PieValueCaptionOffset = 4;
  PieLeaderLineSegment1 = 12;
  PieLeaderLineSegment2 = 9;

  ToolBoxItemOffset = 5;
  ToolBoxItemSeparatorWidth = 1;
  ToolBoxDataLevelInfoConnectorWidth = 1;
  ToolBoxDiagramImageOffset = 2;

  DefaultItemDisplayText = '<no display text>';

  ValueHintOffset = 27;

  PainterHelperClasses: array[Boolean] of TcxGridChartViewPainterHelperClass = (
    TcxGridChartViewPainterHelper, TcxGridChartViewGDIPlusPainterHelper
  );

procedure cxGridChartDiagramImages_Add(const AResourceName: string);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.LoadFromResourceName(HInstance, AResourceName);
    cxGridChartDiagramImages.Add(B, nil);
  finally
    B.Free;
  end;
end;

function GetDefaultValueColor(AIndex: Integer): TColor;
const
  ColorCount = 24;
  Colors: array[0..ColorCount - 1] of TColor =
    ($60C1FF, $B4835C, $7C58A5, $657C6C, $6379E6, $9AA05B, $605DCF, $6A8846,
     $61A3F5, $58999E, $5A8CFF, $AD977A, $808E54, $95C9B9, $6763A5, $AC8C4D,
     $80E4FB, $956349, $4D50C0, $67B48B, $D6A584, $73D8DD, $89674D, $9CB5A5);
begin
  Result := Colors[AIndex mod ColorCount];
end;

{//todo: msn need port colors from .net
function GetDefaultValueColor(AIndex: Integer): TColor;
const
  ColorCount = 36;
  Colors: array[0..ColorCount - 1] of TColor =
  ($1E92CE, $09A866, $1B4CC8, $3AADC8, $46C777, $173A98,
   $008800, $00A19E, $719F37, $68ACCE, $C0C073, $7393EA,
   $60C1FF, $B4835C, $7C58A5, $657C6C, $6379E6, $9AA05B, $605DCF, $6A8846,
   $61A3F5, $58999E, $5A8CFF, $AD977A, $808E54, $95C9B9, $6763A5, $AC8C4D,
   $80E4FB, $956349, $4D50C0, $67B48B, $D6A584, $73D8DD, $89674D, $9CB5A5);
begin
  Result := Colors[AIndex mod ColorCount];
end;
}

function GetHotColor(AColor: TColor): TColor;
const
  Alpha = 80;
  //Delta = 25;
var
  R, G, B: Byte;
begin
  Result := ColorToRGB(AColor);
  R := GetRValue(Result);
  G := GetGValue(Result);
  B := GetBValue(Result);
{  // MS Money style - does not give good results on light colors (clRed, clLime, ...)
  Result := RGB(
    Min(R + Delta, 255),
    Min(G + Delta, 255),
    Min(B + Delta, 255)
  );}
  // alpha blending with white color
  Result := RGB(
    MulDiv(R, Alpha, 100) + MulDiv(255, 100 - Alpha, 100),
    MulDiv(G, Alpha, 100) + MulDiv(255, 100 - Alpha, 100),
    MulDiv(B, Alpha, 100) + MulDiv(255, 100 - Alpha, 100)
  );
end;

procedure PositionRect(out R: TRect; var AClientR: TRect; AWidth, AHeight, AOffset: Integer;
  APosition: TcxGridChartPartPosition);
begin
  R := AClientR;
  case APosition of
    cppLeft:
      begin
        R.Right := R.Left + AWidth;
        AClientR.Left := R.Right + AOffset;
      end;
    cppTop:
      begin
        R.Bottom := R.Top + AHeight;
        AClientR.Top := R.Bottom + AOffset;
      end;
    cppRight:
      begin
        R.Left := R.Right - AWidth;
        AClientR.Right := R.Left - AOffset;
      end;
    cppBottom:
      begin
        R.Top := R.Bottom - AHeight;
        AClientR.Bottom := R.Top - AOffset;
      end;
  end;
end;

procedure AlignRect(var R: TRect; AWidth, AHeight: Integer; AIsHorizontal: Boolean;
  AAlignment: TcxGridChartPartAlignment);
var
  AOriginalR: TRect;
begin
  AOriginalR := R;
  case AAlignment of
    cpaStart:
      if AIsHorizontal then
        R.Right := R.Left + AWidth
      else
        R.Bottom := R.Top + AHeight;
    cpaCenter:
      if AIsHorizontal then
      begin
        R.Left := (R.Left + R.Right - AWidth) div 2;
        R.Right := R.Left + AWidth;
      end
      else
      begin
        R.Top := (R.Top + R.Bottom - AHeight) div 2;
        R.Bottom := R.Top + AHeight;
      end;
    cpaEnd:
      if AIsHorizontal then
        R.Left := R.Right - AWidth
      else
        R.Top := R.Bottom - AHeight;
  end;
//  IntersectRect(R, R, AOriginalR);
end;

procedure CheckRectBounds(var R: TRect);
begin
  with R do
  begin
    if Left > Right then
      SwapIntegers(Left, Right);
    if Top > Bottom then
      SwapIntegers(Top, Bottom);
  end;
end;

procedure GetCenteredRect(const ACenter: TPoint; AWidth, AHeight: Integer; out R: TRect);
begin
  R.Left := ACenter.X - AWidth div 2;
  R.Right := R.Left + AWidth;
  R.Top := ACenter.Y - AHeight div 2;
  R.Bottom := R.Top + AHeight;
end;

const
  ErrorValue = 1e-15;

function Fraction(const AValue: Extended): Extended;
begin
  Result := Frac(AValue);
  if Result <> 0 then  // to fix the bug in standard implementation
  begin
    Result := Frac(AValue + Sign(AValue) * ErrorValue);
    if Abs(Result) < ErrorValue then
      Result := 0
    else
    begin
      Result := Frac(AValue - Sign(AValue) * ErrorValue);
      if 1 - Abs(Result) < ErrorValue then
        Result := 0;
    end;
  end;
end;

function Ceiling(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Fraction(X) > 0 then Inc(Result);
end;

procedure CheckZero(var AValue: Extended);
begin
  if Abs(AValue) < ErrorValue then AValue := 0;
end;

function IsPositive(const AValue: Extended): Boolean;
begin
  Result := AValue >= 0;
end;

function IsValueTypeValid(AValueType: TVarType; AOnlyIntegerTypes: Boolean): Boolean;
const
  ValidIntegerValueTypes = [varSmallint, varInteger, varByte, varShortInt,
    varWord, varLongWord, varInt64];
  ValidFloatValueTypes = [varSingle, varDouble, varCurrency];
begin
  Result := (AValueType in ValidIntegerValueTypes) or not AOnlyIntegerTypes and
    ((AValueType in ValidFloatValueTypes){$IFNDEF NONDB} or (AValueType = VarFMTBcd){$ENDIF});
end;

function IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;
begin
  Result := IsValueTypeValid(AValueTypeClass.GetVarType, False);
end;

function IsValueValid(const AValue: Variant): Boolean;
begin
  Result := IsValueTypeValid(VarType(AValue), False);
end;

function GetPointOnCircle(const ACenter: TPoint; ARadius, AAngle: Integer): TPoint;
var
  ASin, ACos: Extended;
begin
  SinCos(DegToRad(AAngle - 90), ASin, ACos);
  Result.X := ACenter.X + Round(ARadius * ACos);
  Result.Y := ACenter.Y + Round(ARadius * ASin);
end;

function LineHasPoint(const ALineStart, ALineFinish: TPoint; ALineWidth: Integer;
  const P: TPoint; AHotZoneMinWidth: Integer): Boolean;
var
  ALineIntersectionY, AHotZoneSizeY: Integer;
begin
  Result := (ALineStart.X <= P.X) and (P.X < ALineFinish.X);
  if Result then
  begin
    ALineIntersectionY := ALineStart.Y + MulDiv(ALineFinish.Y - ALineStart.Y,
      P.X - ALineStart.X, ALineFinish.X - ALineStart.X);
    AHotZoneSizeY := Abs(Round(Max(AHotZoneMinWidth, ALineWidth div 2) *
      Sqrt(1 + Sqr((ALineFinish.Y - ALineStart.Y) / (ALineFinish.X - ALineStart.X)))));
    Result :=
      (ALineIntersectionY - AHotZoneSizeY <= P.Y) and
      (P.Y <= ALineIntersectionY + AHotZoneSizeY);
  end;
end;

{ TcxGridChartTitleHitTest }

class function TcxGridChartTitleHitTest.GetHitTestCode: Integer;
begin
  Result := htChartTitle;
end;

{ TcxGridChartLegendHitTest }

class function TcxGridChartLegendHitTest.GetHitTestCode: Integer;
begin
  Result := htLegend;
end;

{ TcxGridChartLegendItemHitTest }

procedure TcxGridChartLegendItemHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartLegendItemHitTest then
  begin
    Index := TcxGridChartLegendItemHitTest(Source).Index;
    Series := TcxGridChartLegendItemHitTest(Source).Series;
    ValueIndex := TcxGridChartLegendItemHitTest(Source).ValueIndex;
  end;
end;

class function TcxGridChartLegendItemHitTest.GetHitTestCode: Integer;
begin
  Result := htLegendItem;
end;

{ TcxGridChartToolBoxHitTest }

class function TcxGridChartToolBoxHitTest.GetHitTestCode: Integer;
begin
  Result := htChartToolBox;
end;

{ TcxGridChartDataLevelInfoHitTest }

procedure TcxGridChartDataLevelInfoHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartDataLevelInfoHitTest then
  begin
    FDataLevel := TcxGridChartDataLevelInfoHitTest(Source).FDataLevel;
    FDataLevelObject := TcxGridChartDataLevelInfoHitTest(Source).FDataLevelObject;
    DataLevelObjectContainerKind := TcxGridChartDataLevelInfoHitTest(Source).DataLevelObjectContainerKind;
  end;
end;

procedure TcxGridChartDataLevelInfoHitTest.SetDataLevel(Value: Integer);
begin
  FDataLevel := Value;
  if FDataLevel = -1 then
    FDataLevelObject := nil
  else
    FDataLevelObject := TcxGridChartView(GridView).DataLevelObjects[FDataLevel];
end;

procedure TcxGridChartDataLevelInfoHitTest.SetDataLevelObject(Value: TcxGridChartDataGroup);
begin
  FDataLevelObject := Value;
  if FDataLevelObject = nil then
    FDataLevel := -1
  else
    FDataLevel := FDataLevelObject.DataLevel;
end;

class function TcxGridChartDataLevelInfoHitTest.GetHitTestCode: Integer;
begin
  Result := htDataLevelInfo;
end;

function TcxGridChartDataLevelInfoHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  if (DataLevelObject <> nil) and DataLevelObject.CanMove then
    Result := TcxGridChartView(GridView).Controller.GetDataLevelInfoDragAndDropObjectClass
  else
    Result := nil;
end;

{ TcxGridChartDataLevelActiveValueInfoHitTest }

procedure TcxGridChartDataLevelActiveValueInfoHitTest.Assign(
  Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartDataLevelActiveValueInfoHitTest then
    CanDropDown := TcxGridChartDataLevelActiveValueInfoHitTest(Source).CanDropDown;
end;

class function TcxGridChartDataLevelActiveValueInfoHitTest.GetHitTestCode: Integer;
begin
  Result := htDataLevelActiveValueInfo;
end;

function TcxGridChartDataLevelActiveValueInfoHitTest.Cursor: TCursor;
begin
  if CanDropDown then
    Result := crcxHandPoint
  else
    Result := inherited Cursor;
end;

function TcxGridChartDataLevelActiveValueInfoHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  if CanDropDown then
    Result := nil
  else
    Result := inherited DragAndDropObjectClass;
end;

{ TcxGridChartCustomizeButtonHitTest }

class function TcxGridChartCustomizeButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htChartCustomizeButton;
end;

{ TcxGridChartDiagramSelectorHitTest }

procedure TcxGridChartDiagramSelectorHitTest.Assign(
  Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartDiagramSelectorHitTest then
    CanDropDown := TcxGridChartDiagramSelectorHitTest(Source).CanDropDown;
end;

class function TcxGridChartDiagramSelectorHitTest.GetHitTestCode: Integer;
begin
  Result := htDiagramSelector;
end;

function TcxGridChartDiagramSelectorHitTest.Cursor: TCursor;
begin
  if CanDropDown then
    Result := crcxHandPoint
  else
    Result := inherited Cursor;
end;

{ TcxGridChartValueHitTest }

procedure TcxGridChartValueHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartValueHitTest then
  begin
    CanDrillDown := TcxGridChartValueHitTest(Source).CanDrillDown;
    Series := TcxGridChartValueHitTest(Source).Series;
    ValueIndex := TcxGridChartValueHitTest(Source).ValueIndex;
  end;
end;

class function TcxGridChartValueHitTest.GetHitTestCode: Integer;
begin
  Result := htChartValue;
end;

function TcxGridChartValueHitTest.Cursor: TCursor;
begin
  if CanDrillDown then
    Result := crcxGridMagnifier
  else
    Result := inherited Cursor;
end;

{ TcxGridChartHistogramPlotHitTest }

class function TcxGridChartHistogramPlotHitTest.GetHitTestCode: Integer;
begin
  Result := htPlot;
end;

{ TcxGridChartHistogramCategoryAxisTitleHitTest }

class function TcxGridChartHistogramCategoryAxisTitleHitTest.GetHitTestCode: Integer;
begin
  Result := htCategoryAxisTitle;
end;

{ TcxGridChartHistogramValueAxisTitleHitTest }

class function TcxGridChartHistogramValueAxisTitleHitTest.GetHitTestCode: Integer;
begin
  Result := htValueAxisTitle;
end;

{ TcxGridChartValueLineHitTest }

class function TcxGridChartValueLineHitTest.GetHitTestCode: Integer;
begin
  Result := htChartValueLine;
end;

{ TcxGridChartValueAreaHitTest }

class function TcxGridChartValueAreaHitTest.GetHitTestCode: Integer;
begin
  Result := htChartValueArea;
end;

{ TcxGridChartPieSeriesSiteHitTest }

procedure TcxGridChartPieSeriesSiteHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridChartPieSeriesSiteHitTest then
    Series := TcxGridChartPieSeriesSiteHitTest(Source).Series;
end;

class function TcxGridChartPieSeriesSiteHitTest.GetHitTestCode: Integer;
begin
  Result := htSeriesSite;
end;

{ TcxGridChartPieSeriesSiteCaptionHitTest }

class function TcxGridChartPieSeriesSiteCaptionHitTest.GetHitTestCode: Integer;
begin
  Result := htSeriesSiteCaption;
end;

{ TcxCustomGridChartDiagramHitTest }

procedure TcxCustomGridChartDiagramHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxCustomGridChartDiagramHitTest then
    Diagram := TcxCustomGridChartDiagramHitTest(Source).Diagram;
end;

{ TcxCustomGridChartMovingObject }

function TcxCustomGridChartMovingObject.GetController: TcxGridChartController;
begin
  Result := TcxGridChartController(inherited Controller);
end;

function TcxCustomGridChartMovingObject.GetCustomizationForm: TcxGridChartCustomizationForm;
begin
  Result := TcxGridChartCustomizationForm(inherited CustomizationForm);
end;

function TcxCustomGridChartMovingObject.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxCustomGridChartMovingObject.GetViewInfo: TcxGridChartViewInfo;
begin
  Result := TcxGridChartViewInfo(inherited ViewInfo);
end;

{ TcxGridChartDataGroupMovingObject }

constructor TcxGridChartDataGroupMovingObject.Create(AControl: TcxControl);
begin
  inherited;
  FDestDataLevel := -1;
end;

function TcxGridChartDataGroupMovingObject.GetSourceItem: TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(inherited SourceItem);
end;

procedure TcxGridChartDataGroupMovingObject.SetDestDataLevel(Value: Integer);
begin
  if FDestDataLevel <> Value then
  begin
    Dirty := True;
    FDestDataLevel := Value;
  end;
end;

procedure TcxGridChartDataGroupMovingObject.SetSourceItem(Value: TcxGridChartDataGroup);
begin
  inherited SourceItem := Value;
end;

function TcxGridChartDataGroupMovingObject.CanRemove: Boolean;
begin
  Result := SourceItem.Visible and SourceItem.VisibleForCustomization and
    (GridView.OptionsCustomize.DataGroupHiding or Controller.Customization);
end;

function TcxGridChartDataGroupMovingObject.GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect;
begin
  Result := ViewInfo.ToolBoxViewInfo.GetDataGroupInsertionBounds(DestDataLevel);
end;

function TcxGridChartDataGroupMovingObject.GetCustomizationFormListBox: TcxCustomGridItemsListBox;
begin
  Result := CustomizationForm.DataGroupsListBox;
end;

function TcxGridChartDataGroupMovingObject.GetSourceItemViewInfo: TcxCustomGridCellViewInfo;
begin
  if SourceItemContainerKind = ckToolBox then
    Result := ViewInfo.ToolBoxViewInfo.DataLevelInfos[SourceItem.DataLevel]
  else
    Result := inherited GetSourceItemViewInfo;
end;

function TcxGridChartDataGroupMovingObject.IsSourceCustomizationForm: Boolean;
begin
  Result := SourceItemContainerKind = ckCustomizationForm;
end;

function TcxGridChartDataGroupMovingObject.IsValidDestination: Boolean;
begin
  Result := (DestDataLevel <> -1) and (not SourceItem.Visible or
    (DestDataLevel < SourceItem.DataLevel) or (DestDataLevel > SourceItem.DataLevel + 1));
end;

procedure TcxGridChartDataGroupMovingObject.BeginDragAndDrop;
begin
  if CustomizationForm <> nil then
    CustomizationForm.PrepareForDataGroupDragAndDrop;
  Controller.FMovingDataGroup := SourceItem;
  inherited;
end;

procedure TcxGridChartDataGroupMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  DestDataLevel := ViewInfo.ToolBoxViewInfo.GetDataGroupInsertionIndex(P);
  Accepted := (ViewInfo.GetHitTest(P).HitTestCode = htCustomizationForm) or
    (DestDataLevel <> -1);
  inherited;
end;

procedure TcxGridChartDataGroupMovingObject.EndDragAndDrop(Accepted: Boolean);

  function GetNewDataLevel: Integer;
  begin
    Result := DestDataLevel;
    if SourceItem.Visible and (SourceItem.DataLevel < Result) then
      Dec(Result);
  end;

var
  ADataGroupPositionChanged: Boolean;
begin
  inherited;
  Controller.FMovingDataGroup := nil;
  if not Accepted then Exit;
  ADataGroupPositionChanged := False;
  if DestDataLevel = -1 then
    if CanRemove then
    begin
      SourceItem.Visible := False;
      ADataGroupPositionChanged := True;
    end
    else
  else
    if IsValidDestination then
    begin
      SourceItem.DataLevel := GetNewDataLevel;
      ADataGroupPositionChanged := True;
    end;
  if ADataGroupPositionChanged then
    GridView.DoDataGroupPosChanged(SourceItem);
end;

procedure TcxGridChartDataGroupMovingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  with AParams as TcxGridChartDataLevelInfoHitTest do
  begin
    SourceItem := DataLevelObject;
    SourceItemContainerKind := DataLevelObjectContainerKind;
  end;
end;

{ TcxGridChartSeriesInnerCheckListBox }

function TcxGridChartSeriesInnerCheckListBox.GetMetrics: TcxCheckListBoxMetrics;
begin
  Result := inherited GetMetrics;
  Inc(Result.CheckFrameWidth, ScaleFactor.Apply(cxTextOffset));
  Inc(Result.ContentOffset, ScaleFactor.Apply(cxTextOffset));
  Inc(Result.ImageFrameWidth, 1);
  Result.TextAreaOffset := 0;
  Inc(Result.TextOffset, 1);
end;

function TcxGridChartSeriesInnerCheckListBox.GetSeriesImageSize: Integer;
begin
  Result := GetStandardItemHeight - 2 * (Metrics.ImageFrameWidth + 1);
end;

{ TcxGridChartSeriesCheckListBox }

constructor TcxGridChartSeriesCheckListBox.Create(AOwner: TComponent;
  AGridView: TcxGridChartView);
begin
  inherited Create(AOwner);
  FGridView := AGridView;
  DragCursor := crcxGridChartDrag;
  DragMode := dmAutomatic;
  ImageLayout := ilAfterChecks;
  Style.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
  Style.HotTrack := False;
  OnClickCheck := CheckClicked;
  FSeriesImages := TImageList.Create(nil);
  EditValueFormat := cvfIndices;
end;

destructor TcxGridChartSeriesCheckListBox.Destroy;
begin
  FreeAndNil(FSeriesImages);
  inherited;
end;

function TcxGridChartSeriesCheckListBox.GetSeries(AIndex: Integer): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(Items[AIndex].ItemObject);
end;

procedure TcxGridChartSeriesCheckListBox.CheckClicked(Sender: TObject; AIndex: Integer;
  APrevState, ANewState: TcxCheckBoxState);
begin
  LockRefreshItems := True;
  try
    Series[AIndex].Visible := ANewState = cbsChecked;
  finally
    LockRefreshItems := False;
  end;
  GridView.DoSeriesPosChanged(Series[AIndex]);
end;

procedure TcxGridChartSeriesCheckListBox.CreateWnd;
begin
  inherited;
  RefreshItems;
end;

procedure TcxGridChartSeriesCheckListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  AItemIndex: Integer;
begin
  inherited;
  AItemIndex := ItemAtPos(Point(X, Y), True);
  Accept := AItemIndex <> -1;
  if Accept and (Series[AItemIndex] <> FDraggingSeries) then
  begin
    FDraggingSeries.Index := Series[AItemIndex].Index;
    ItemIndex := AItemIndex;
    GridView.DoSeriesPosChanged(FDraggingSeries);
  end;
end;

procedure TcxGridChartSeriesCheckListBox.DrawSeriesImage(ACanvas: TcxCanvas;
  const R: TRect; ASeries: TcxGridChartSeries);
var
  AParams: TcxViewParams;
begin
  ASeries.Styles.GetValueParams(-1, False, AParams);
  ACanvas.Rectangle(R, AParams, cxBordersAll, clWindowText);
end;

function TcxGridChartSeriesCheckListBox.GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass;
begin
  Result := TcxGridChartSeriesInnerCheckListBox;
end;

procedure TcxGridChartSeriesCheckListBox.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  RefreshImages;
end;

procedure TcxGridChartSeriesCheckListBox.RefreshImages;
begin
  if SeriesImages = nil then Exit;
  Images := nil;
  RefreshSeriesImages;
  Images := SeriesImages;
end;

procedure TcxGridChartSeriesCheckListBox.RefreshSeriesImages;
var
  AImage: TBitmap;
  AImageCanvas: TcxCanvas;
  I: Integer;
begin
  SeriesImages.Clear;
  SeriesImages.Masked := False;
  SeriesImages.Width := TcxGridChartSeriesInnerCheckListBox(InnerCheckListBox).GetSeriesImageSize;
  SeriesImages.Height := SeriesImages.Width;

  AImage := TBitmap.Create;
  AImageCanvas := TcxCanvas.Create(AImage.Canvas);
  try
    AImage.Width := SeriesImages.Width;
    AImage.Height := SeriesImages.Height;
    for I := 0 to Items.Count - 1 do
    begin
      DrawSeriesImage(AImageCanvas, Rect(0, 0, AImage.Width, AImage.Height), Series[I]);
      SeriesImages.Add(AImage, nil);
    end;
  finally
    AImageCanvas.Free;
    AImage.Free;
  end;
end;

procedure TcxGridChartSeriesCheckListBox.RefreshItems;
var
  I: Integer;
  ASeries: TcxGridChartSeries;
begin
  if LockRefreshItems then
    Exit;
  with Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to GridView.SeriesCount - 1 do
      begin
        ASeries := GridView.Series[I];
        if ASeries.VisibleForCustomization then
          with Add do
          begin
            ItemObject := ASeries;
            Text := ASeries.GetDisplayText;
            Checked := ASeries.Visible;
            ImageIndex := Index;
          end;
      end;
      RefreshImages;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxGridChartSeriesCheckListBox.StartDrag(DragObject: TDragObject): Boolean;
var
  AItemIndex: Integer;
begin
  Result := inherited StartDrag(DragObject);
  if Result then
  begin
    AItemIndex := ItemAtPos(ScreenToClient(GetMouseCursorPos), True);
    Result := AItemIndex <> -1;
    if Result then
      FDraggingSeries := Series[AItemIndex];
  end;
end;

{ TcxGridChartSortBySeriesComboBox }

constructor TcxGridChartSortBySeriesComboBox.Create(AOwner: TComponent;
  AGridView: TcxGridChartView);
begin
  inherited Create(AOwner);
  FGridView := AGridView;
  Properties.DropDownListStyle := lsFixedList;
  FDirectionButton := Properties.Buttons.Add;
  FDirectionButton.Kind := bkGlyph;
  Properties.OnDrawItem := DrawItem;
  Properties.OnMeasureItem := MeasureItem;
  Style.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
  Style.HotTrack := False;
  RefreshItems;
end;

procedure TcxGridChartSortBySeriesComboBox.Click;
begin
  GridView.SortedSeries := TcxGridChartSeries(ItemObject);
  inherited;
end;

procedure TcxGridChartSortBySeriesComboBox.DirectionButtonClick;
begin
  with GridView.SortedSeries do
    if SortOrder = soAscending then
      SortOrder := soDescending
    else
      SortOrder := soAscending;
end;

procedure TcxGridChartSortBySeriesComboBox.DoButtonClick(AButtonVisibleIndex: Integer);
begin
  if AButtonVisibleIndex = DirectionButton.Index then
    DirectionButtonClick;
  inherited;
end;

procedure TcxGridChartSortBySeriesComboBox.DrawItem(AControl: TcxCustomComboBox;
  ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);

  function GetKeyBounds: TRect;
  const
    KeyOffset = 1;
  var
    AKeySize: Integer;
  begin
    Result := ARect;
    AKeySize := cxTextHeight(ACanvas.Handle) - 2 * ScaleFactor.Apply(KeyOffset);
    InflateRect(Result, -ScaleFactor.Apply(cxTextOffset + KeyOffset),
      -(Result.Bottom - Result.Top - ScaleFactor.Apply(AKeySize)) div 2);
    Result.Right := Result.Left + AKeySize;
  end;

var
  ASeries: TcxGridChartSeries;
  AParams: TcxViewParams;
  ABrushColor: TColor;
  ATextRect, AKeyRect: TRect;
  ADrawTextFlags: Integer;
begin
  ASeries := TcxGridChartSeries(Properties.Items.Objects[AIndex]);
  if ASeries <> nil then
  begin
    ASeries.Styles.GetValueParams(-1, False, AParams);
    ABrushColor := ACanvas.Brush.Color;
    AKeyRect := GetKeyBounds;
    if UseRightToLeftAlignment then
      AKeyRect := TdxRightToLeftLayoutConverter.ConvertRect(AKeyRect, ARect);
    ACanvas.Rectangle(AKeyRect, AParams, cxBordersAll, clWindowText, 1, True);
    ACanvas.Brush.Color := ABrushColor;
  end;

  ACanvas.FillRect(ARect);

  ATextRect := ARect;
  ATextRect.Left := GetKeyBounds.Right + 2 * ScaleFactor.Apply(cxTextOffset);
  ADrawTextFlags := cxSingleLine or cxAlignVCenter or cxShowEndEllipsis;
  if UseRightToLeftAlignment then
  begin
    ATextRect := TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, ARect);
    ADrawTextFlags := ADrawTextFlags or cxAlignRight;
  end;
  if UseRightToLeftReading then
    ADrawTextFlags := ADrawTextFlags or cxRtlReading;
  ACanvas.DrawText(Properties.Items[AIndex], ATextRect, ADrawTextFlags);
end;

procedure TcxGridChartSortBySeriesComboBox.MeasureItem(
  AControl: TcxCustomComboBox; AIndex: Integer; ACanvas: TcxCanvas; var AHeight: Integer);
begin
  Inc(AHeight, 2 * ScaleFactor.Apply(cxTextOffset));
end;

procedure TcxGridChartSortBySeriesComboBox.RefreshDirectionButton;
const
  GlyphWidth = 13;
  GlyphHeight = 13;
  LineSize = 1;
  LineOffset = 1;
  LineChange = LineSize + LineOffset;
var
  ASortOrder: TcxDataSortOrder;
  R: TRect;
  I: Integer;
  ABitmap: TcxBitmap;
begin
  if GridView.SortedSeries = nil then
    ASortOrder := soNone
  else
    ASortOrder := GridView.SortedSeries.SortOrder;

  DirectionButton.Visible := ASortOrder <> soNone;
  if DirectionButton.Visible then
  begin
    ABitmap := TcxBitmap.CreateSize(GlyphWidth, GlyphHeight);
    try
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, clFuchsia);
      if ASortOrder = soAscending then
        R := Rect(0, LineOffset, LineOffset + LineChange, LineOffset + LineSize)
      else
        R := Rect(0, LineOffset, GlyphWidth, LineOffset + LineSize);

      for I := 0 to GlyphHeight div LineChange - 1 do
      begin
        ABitmap.cxCanvas.FillRect(R, clBtnText);
        if ASortOrder = soAscending then
          Inc(R.Right, LineChange)
        else
          Dec(R.Right, LineChange);

        OffsetRect(R, 0, LineChange);
      end;
      DirectionButton.Glyph.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TcxGridChartSortBySeriesComboBox.RefreshItems;
var
  I: Integer;
  ASeries: TcxGridChartSeries;
begin
  with Properties.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add(cxGetResourceString(@scxGridChartCustomizationFormNoSortedSeries));
      for I := 0 to GridView.SeriesCount - 1 do
      begin
        ASeries := GridView.Series[I];
        if ASeries.VisibleForCustomization then
          AddObject(ASeries.GetDisplayText, ASeries);
      end;
      ItemObject := GridView.SortedSeries;
      RefreshDirectionButton;
      Properties.DropDownRows := Count;
    finally
      EndUpdate;
    end;
  end;
end;

{ TcxGridChartDataGroupsListBox }

function TcxGridChartDataGroupsListBox.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartDataGroupsListBox.CalculateItemHeight: Integer;
begin
  Result := 2 * (LookAndFeelPainter.ChartToolBoxDataLevelInfoBorderSize + ScaleFactor.Apply(cxTextOffset)) +
    cxTextHeight(Canvas.Handle);
end;

procedure TcxGridChartDataGroupsListBox.DoRefreshItems;
var
  I: Integer;
  AItem: TcxGridChartDataGroup;
begin
  inherited;
  with Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to GridView.DataGroupCount - 1 do
      begin
        AItem := GridView.DataGroups[I];
        if AItem.VisibleForCustomization and not AItem.Visible then
          AddObject(AItem.GetDisplayText, AItem);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxGridChartDataGroupsListBox.GetDragAndDropParams: TcxCustomGridHitTest;
begin
  Result := TcxGridChartDataLevelInfoHitTest.Instance(Point(-1, -1));
  with TcxGridChartDataLevelInfoHitTest(Result) do
  begin
    GridView := Self.GridView;
    DataLevelObject := TcxGridChartDataGroup(DragAndDropItem);
    DataLevelObjectContainerKind := ckCustomizationForm;
  end;
end;

procedure TcxGridChartDataGroupsListBox.PaintItem(ACanvas: TcxCanvas; R: TRect;
  AIndex: Integer; AFocused: Boolean);
var
  ABorderSize: Integer;
  AParams: TcxViewParams;
  ADrawTextFlags: Integer;
begin
  ABorderSize := LookAndFeelPainter.ChartToolBoxDataLevelInfoBorderSize;
  if AFocused then
    GridView.Styles.GetViewParams(vsActiveDataLevelInfo, nil, nil, AParams)
  else
    GridView.Styles.GetViewParams(vsDataLevelsInfo, nil, nil, AParams);
  ACanvas.Rectangle(R, AParams, cxBordersAll, LookAndFeelPainter.DefaultChartToolBoxDataLevelInfoBorderColor, ABorderSize);
  R := cxRectInflate(R, -(ABorderSize + ScaleFactor.Apply(cxTextOffset)));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := AParams.TextColor;
  ADrawTextFlags := cxSingleLine or cxShowEndEllipsis;
  if UseRightToLeftAlignment then
    ADrawTextFlags := ADrawTextFlags or cxAlignRight;
  if UseRightToLeftReading then
    ADrawTextFlags := ADrawTextFlags or cxRtlReading;
  ACanvas.DrawText(Items[AIndex], R, ADrawTextFlags);
  ACanvas.Brush.Style := bsSolid;
end;

{ TcxGridChartOptionsTreeView }

constructor TcxGridChartOptionsTreeView.Create(AOwner: TComponent; AGridView: TcxGridChartView);
begin
  inherited Create(AOwner);
  FGridView := AGridView;
  Style.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
  Style.HotTrack := False;
end;

function GetChartPartPositionItemCaption(ATypeItem: Integer): string;
begin
  Result := '';
  case TcxGridChartPartPosition(ATypeItem) of
    cppDefault:
      Result := cxGetResourceString(@scxGridChartPositionDefault);
    cppNone:
      Result := cxGetResourceString(@scxGridChartPositionNone);
    cppLeft:
      Result := cxGetResourceString(@scxGridChartPositionLeft);
    cppTop:
      Result := cxGetResourceString(@scxGridChartPositionTop);
    cppRight:
      Result := cxGetResourceString(@scxGridChartPositionRight);
    cppBottom:
      Result := cxGetResourceString(@scxGridChartPositionBottom);
  end;
end;

function GetChartPartAlignmentItemCaption(ATypeItem: Integer): string;
begin
  Result := '';
  case TcxGridChartPartAlignment(ATypeItem) of
    cpaDefault:
      Result := cxGetResourceString(@scxGridChartAlignmentDefault);
    cpaStart:
      Result := cxGetResourceString(@scxGridChartAlignmentStart);
    cpaCenter:
      Result := cxGetResourceString(@scxGridChartAlignmentCenter);
    cpaEnd:
      Result := cxGetResourceString(@scxGridChartAlignmentEnd);
  end;
end;

function GetChartPartOrientationItemCaption(ATypeItem: Integer): string;
begin
  Result := '';
  case TcxGridChartPartOrientation(ATypeItem) of
    cpoDefault:
      Result := cxGetResourceString(@scxGridChartOrientationDefault);
    cpoHorizontal:
      Result := cxGetResourceString(@scxGridChartOrientationHorizontal);
    cpoVertical:
      Result := cxGetResourceString(@scxGridChartOrientationVertical);
  end;
end;

function GetChartToolBoxPositionItemCaption(ATypeItem: Integer): string;
begin
  Result := '';
  case TcxGridChartToolBoxPosition(ATypeItem) of
    tpTop:
      Result := cxGetResourceString(@scxGridChartPositionTop);
    tpBottom:
      Result := cxGetResourceString(@scxGridChartPositionBottom);
  end;
end;

procedure TcxGridChartOptionsTreeView.AddItems;
begin
  // Legend
  LegendID := AddCategory(-1, -1, cxGetResourceString(@scxGridChartLegend));
    LegendPositionID :=  AddCategory(LegendID, -1, cxGetResourceString(@scxGridChartPosition));
      AddRadioButtons(LegendPositionID, TypeInfo(TcxGridChartPartPosition), @GetChartPartPositionItemCaption);
    LegendAlignmentID :=  AddCategory(LegendID, -1, cxGetResourceString(@scxGridChartAlignment));
      AddRadioButtons(LegendAlignmentID, TypeInfo(TcxGridChartPartAlignment), @GetChartPartAlignmentItemCaption);
    LegendOrientationID :=  AddCategory(LegendID, -1, cxGetResourceString(@scxGridChartOrientation));
      AddRadioButtons(LegendOrientationID, TypeInfo(TcxGridChartPartOrientation), @GetChartPartOrientationItemCaption);
    LegendBorderID := AddCheckBox(LegendID, -1, cxGetResourceString(@scxGridChartBorder));
    LegendKeyBorderID := AddCheckBox(LegendID, -1, cxGetResourceString(@scxGridChartLegendKeyBorder));
  // Title
  TitleID := AddCategory(-1, -1, cxGetResourceString(@scxGridChartTitle));
    TitlePositionID :=  AddCategory(TitleID, -1, cxGetResourceString(@scxGridChartPosition));
      AddRadioButtons(TitlePositionID, TypeInfo(TcxGridChartPartPosition), @GetChartPartPositionItemCaption);
    TitleAlignmentID :=  AddCategory(TitleID, -1, cxGetResourceString(@scxGridChartAlignment));
      AddRadioButtons(TitleAlignmentID, TypeInfo(TcxGridChartPartAlignment), @GetChartPartAlignmentItemCaption);
  // ToolBox
  ToolBoxID := AddCategory(-1, -1, cxGetResourceString(@scxGridChartToolBox));
    ToolBoxPositionID := AddCategory(ToolBoxID, -1, cxGetResourceString(@scxGridChartPosition));
      AddRadioButtons(ToolBoxPositionID, TypeInfo(TcxGridChartToolBoxPosition), @GetChartToolBoxPositionItemCaption);
    ToolBoxBorderID := AddCheckBox(ToolBoxID, -1, cxGetResourceString(@scxGridChartBorder));
    ToolBoxDiagramSelectorID := AddCheckBox(ToolBoxID, -1, cxGetResourceString(@scxGridChartDiagramSelector));
  // Other
  OtherID := AddCategory(-1, -1, cxGetResourceString(@scxGridChartOther));
    OtherValueHintsID := AddCheckBox(OtherID, -1, cxGetResourceString(@scxGridChartValueHints));
end;

function TcxGridChartOptionsTreeView.IsItemChecked(AParentID, AID: Integer): Boolean;
begin
  Result := False;
  // Legend
  if AParentID = LegendAlignmentID then
    Result := Ord(GridView.ActiveDiagram.Legend.Alignment) = AID;
  if AID = LegendBorderID then
    Result := GridView.ActiveDiagram.Legend.GetBorder = lbSingle;
  if AID = LegendKeyBorderID then
    Result := GridView.ActiveDiagram.Legend.GetKeyBorder = lbSingle;
  if AParentID = LegendOrientationID then
    Result := Ord(GridView.ActiveDiagram.Legend.Orientation) = AID;
  if AParentID = LegendPositionID then
    Result := Ord(GridView.ActiveDiagram.Legend.Position) = AID;
  // Title
  if AParentID = TitleAlignmentID then
    Result := Ord(GridView.Title.Alignment) = AID;
  if AParentID = TitlePositionID then
    Result := Ord(GridView.Title.Position) = AID;
  // ToolBox
  if AID = ToolBoxBorderID then
    Result := GridView.ToolBox.Border = tbSingle;
  if AID = ToolBoxDiagramSelectorID then
    Result := GridView.ToolBox.DiagramSelector;
  if AParentID = ToolBoxPositionID then
    Result := Ord(GridView.ToolBox.Position) = AID;
  // Other
  if AID = OtherValueHintsID then
    Result := GridView.OptionsBehavior.ValueHints;
end;

procedure TcxGridChartOptionsTreeView.ItemClicked(AParentID, AID: Integer);
begin
  // Legend
  if AParentID = LegendAlignmentID then
    GridView.ActiveDiagram.Legend.Alignment := TcxGridChartPartAlignment(AID);
  if AID = LegendBorderID then
    with GridView.ActiveDiagram.Legend do
      if GetBorder = lbNone then
        Border := lbSingle
      else
        Border := lbNone;
  if AID = LegendKeyBorderID then
    with GridView.ActiveDiagram.Legend do
      if GetKeyBorder = lbNone then
        KeyBorder := lbSingle
      else
        KeyBorder := lbNone;
  if AParentID = LegendOrientationID then
    GridView.ActiveDiagram.Legend.Orientation := TcxGridChartPartOrientation(AID);
  if AParentID = LegendPositionID then
    GridView.ActiveDiagram.Legend.Position := TcxGridChartPartPosition(AID);
  // Title
  if AParentID = TitleAlignmentID then
    GridView.Title.Alignment := TcxGridChartPartAlignment(AID);
  if AParentID = TitlePositionID then
    GridView.Title.Position := TcxGridChartPartPosition(AID);
  // ToolBox
  if AID = ToolBoxBorderID then
    with GridView.ToolBox do
      if Border = tbNone then
        Border := tbSingle
      else
        Border := tbNone;
  if AID = ToolBoxDiagramSelectorID then
    with GridView.ToolBox do
      DiagramSelector := not DiagramSelector;
  if AParentID = ToolBoxPositionID then
    GridView.ToolBox.Position := TcxGridChartToolBoxPosition(AID);
  // Other
  if AID = OtherValueHintsID then
    with GridView.OptionsBehavior do
      ValueHints := not ValueHints;
end;

{ TcxGridChartCustomizationForm }

function TcxGridChartCustomizationForm.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartCustomizationForm.CanChangeSortedSeries: Boolean;
begin
  Result := (GridView.SortedSeries = nil) or GridView.SortedSeries.VisibleForCustomization;
end;

procedure TcxGridChartCustomizationForm.CreateControls;
begin
  inherited;
  // Series
  FSeriesCheckListBox := GetSeriesCheckListBoxClass.Create(Self, GridView);
  with FSeriesCheckListBox do
  begin
    Align := alClient;
    Parent := FSeriesPage;
  end;
  FSortBySeriesLabel := TcxLabel.Create(Self);
  with FSortBySeriesLabel do
  begin
    Align := alBottom;
    Caption := cxGetResourceString(@scxGridChartCustomizationFormSortBySeries);
    Style.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
    Transparent := True;
    Visible := CanChangeSortedSeries;
    Parent := FSeriesPage;
  end;
  FSortBySeriesComboBox := GetSortBySeriesComboBoxClass.Create(Self, GridView);
  with FSortBySeriesComboBox do
  begin
    Align := alBottom;
    Visible := CanChangeSortedSeries;
    Parent := FSeriesPage;
  end;
  // Data Groups
  FDataGroupsListBox := GetDataGroupsListBoxClass.Create(Self);
  with FDataGroupsListBox do
  begin
    Align := alClient;
    Parent := FDataGroupsPage;
    RefreshItems;
  end;
  // Options
  FOptionsTreeView := GetOptionsTreeViewClass.Create(Self, GridView);
  with FOptionsTreeView do
  begin
    Align := alClient;
    Parent := FOptionsPage;
  end;
end;

function TcxGridChartCustomizationForm.GetDataGroupsListBoxClass: TcxGridChartDataGroupsListBoxClass;
begin
  Result := TcxGridChartDataGroupsListBox;
end;

function TcxGridChartCustomizationForm.GetDataGroupsPageVisible: Boolean;
begin
  Result := GridView.OptionsCustomize.DataGroupMoving and
    (GridView.ToolBox.DataLevelsInfoVisible <> dlivNever);
end;

function TcxGridChartCustomizationForm.GetOptionsPageVisible: Boolean;
begin
  Result := GridView.OptionsCustomize.OptionsCustomization;
end;

function TcxGridChartCustomizationForm.GetOptionsTreeViewClass: TcxGridChartOptionsTreeViewClass;
begin
  Result := TcxGridChartOptionsTreeView;
end;

function TcxGridChartCustomizationForm.GetSeriesCheckListBoxClass: TcxGridChartSeriesCheckListBoxClass;
begin
  Result := TcxGridChartSeriesCheckListBox;
end;

function TcxGridChartCustomizationForm.GetSeriesPageVisible: Boolean;
begin
  Result := GridView.OptionsCustomize.SeriesCustomization;
end;

function TcxGridChartCustomizationForm.GetSortBySeriesComboBoxClass: TcxGridChartSortBySeriesComboBoxClass;
begin
  Result := TcxGridChartSortBySeriesComboBox;
end;

procedure TcxGridChartCustomizationForm.GridViewChanged;
begin
  inherited;
  OptionsTreeView.RefreshItemValues;
end;

procedure TcxGridChartCustomizationForm.InitPageControl;
begin
  inherited;
  FSeriesPage := CreatePage(
    cxGetResourceString(@scxGridChartCustomizationFormSeriesPageCaption),
    GetSeriesPageVisible);
  FDataGroupsPage := CreatePage(
    cxGetResourceString(@scxGridChartCustomizationFormDataGroupsPageCaption),
    GetDataGroupsPageVisible);
  FOptionsPage := CreatePage(
    cxGetResourceString(@scxGridChartCustomizationFormOptionsPageCaption),
    GetOptionsPageVisible);
end;

procedure TcxGridChartCustomizationForm.PrepareForDataGroupDragAndDrop;
begin
  ActivatePage(DataGroupsPage);
end;

procedure TcxGridChartCustomizationForm.RefreshData;
begin
  inherited;
  SeriesCheckListBox.RefreshItems;
  SortBySeriesComboBox.RefreshItems;
  DataGroupsListBox.RefreshItems;
end;

{ TcxGridChartDataLevelActiveValuePopup }

constructor TcxGridChartDataLevelActiveValuePopup.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FListBox := TcxGridPopupListBox.Create(Self);
  FListBox.OnAction := ListBoxAction;
end;

function TcxGridChartDataLevelActiveValuePopup.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartDataLevelActiveValuePopup.ListBoxAction(Sender: TdxCustomListBox;
  AItemIndex: Integer);
begin
  SetDataGroupActiveValue(AItemIndex);
end;

procedure TcxGridChartDataLevelActiveValuePopup.AddValueItems;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    GridView.ViewData.GetVisibleGroupValues(FDataGroup, AStrings, Values);
    PopulateListBox(AStrings);
  finally
    AStrings.Free;
  end;
end;

procedure TcxGridChartDataLevelActiveValuePopup.AdjustListBoxSize;
begin
  ListBox.VisibleItemCount := GridView.ToolBox.DataLevelActiveValueDropDownCount;
  ListBox.VisibleWidth := GridView.ToolBox.DataLevelActiveValueDropDownWidth;
  ListBox.AutoSize := True;
  ListBox.Constraints.MinWidth := ClientMinWidth;
end;

procedure TcxGridChartDataLevelActiveValuePopup.InitPopup;
var
  AIntf: IcxGridChartDataLevelActiveValuePopupOwner;
begin
  Supports(Owner, IcxGridChartDataLevelActiveValuePopupOwner, AIntf);
  FDataGroup := AIntf.GetDataGroup;
  inherited;
  ListBox.BeginUpdate;
  try
    AddValueItems;
    AdjustListBoxSize;
    ListBox.ItemIndex := VarIndex(Values, FDataGroup.ActiveValue);
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TcxGridChartDataLevelActiveValuePopup.PopulateListBox(AStrings: TStrings);
var
  I: Integer;
begin
  ListBox.BeginUpdate;
  try
    ListBox.Clear;
    for I := 0 to AStrings.Count - 1 do
      ListBox.Items.Add(AStrings[I]);
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TcxGridChartDataLevelActiveValuePopup.SetDataGroupActiveValue(AItemIndex: Integer);
begin
  GridView.BeginUpdate;
  try
    FDataGroup.ActiveValue := Values[AItemIndex];
    if FDataGroup.DataLevel >= GridView.ActiveDataLevel then
      GridView.ActiveDataLevel := FDataGroup.DataLevel + 1;
  finally
    GridView.EndUpdate;
  end;
end;

{ TcxGridChartDiagramSelectorPopup }

constructor TcxGridChartDiagramSelectorPopup.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FListBox := TcxGridPopupListBox.Create(Self);
  FListBox.OnAction := ListBoxAction;
  FListBox.Images := cxGridChartDiagramImages;
end;

function TcxGridChartDiagramSelectorPopup.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartDiagramSelectorPopup.ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
begin
  SelectDiagram(AItemIndex);
end;

procedure TcxGridChartDiagramSelectorPopup.AddDiagramItems;
var
  I: Integer;
  ADiagram: TcxGridChartDiagram;
begin
  ListBox.BeginUpdate;
  try
    ListBox.Clear;
    for I := 0 to GridView.AvailableDiagramCount - 1 do
    begin
      ADiagram := GridView.AvailableDiagrams[I];
      ListBox.Items.Add(ADiagram.DisplayText, ADiagram.ImageIndex, ADiagram);
    end;
  finally
    ListBox.EndUpdate;
  end;
end;

function TcxGridChartDiagramSelectorPopup.GetImageOffset: Integer;
begin
  Result := ScaleFactor.Apply(cxTextOffset);
end;

procedure TcxGridChartDiagramSelectorPopup.InitPopup;
begin
  inherited;
  ListBox.BeginUpdate;
  try
    AddDiagramItems;
    ListBox.Sorted := True;
    ListBox.AutoSize := True;
    ListBox.Constraints.MinWidth := ClientMinWidth;
    ListBox.ItemObject := GridView.ActiveDiagram;
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TcxGridChartDiagramSelectorPopup.SelectDiagram(AItemIndex: Integer);
var
  ADiagram: TcxGridChartDiagram;
begin
  ADiagram := TcxGridChartDiagram(FListBox.Items[AItemIndex].Data);
  ADiagram.Active := True;
  if GridView.IsDesigning then
  begin
    GridView.Controller.DesignController.SelectObject(ADiagram, True);
    GridView.Controller.DesignerModified;
  end;
end;

{ TcxGridChartController }

constructor TcxGridChartController.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  DataLevelsChanged;
end;

destructor TcxGridChartController.Destroy;
begin
  FDataLevelActiveValuePopup.Free;
  FDiagramSelectorPopup.Free;
  inherited;
end;

function TcxGridChartController.GetCustomizationForm: TcxGridChartCustomizationForm;
begin
  Result := TcxGridChartCustomizationForm(inherited CustomizationForm);
end;

function TcxGridChartController.GetDataLevelActiveValuePopup: TcxGridChartDataLevelActiveValuePopup;
begin
  if FDataLevelActiveValuePopup = nil then
    FDataLevelActiveValuePopup := GetDataLevelActiveValuePopupClass.Create(GridView);
  Result := FDataLevelActiveValuePopup;
end;

function TcxGridChartController.GetDiagramSelectorPopup: TcxGridChartDiagramSelectorPopup;
begin
  if FDiagramSelectorPopup = nil then
    FDiagramSelectorPopup := GetDiagramSelectorPopupClass.Create(GridView);
  Result := FDiagramSelectorPopup;
end;

function TcxGridChartController.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartController.GetIsDataGroupMoving: Boolean;
begin
  Result := FMovingDataGroup <> nil;
end;

function TcxGridChartController.GetViewInfo: TcxGridChartViewInfo;
begin
  Result := TcxGridChartViewInfo(inherited ViewInfo);
end;

function TcxGridChartController.GetVisibleCategoryCountValue: Integer;
begin
  Result := GetVisibleCategoryCount(False);
end;

procedure TcxGridChartController.SetFirstVisibleCategoryIndex(Value: Integer);
begin
  if GridView.IsUpdateLocked then
    Value := 0
  else
  begin
    Value := Max(Value, 0);
    Value := Min(Value, GridView.ViewData.VisibleCategoryCount - VisibleCategoryCount);
  end;
  if FFirstVisibleCategoryIndex <> Value then
  begin
    FFirstVisibleCategoryIndex := Value;
    GridView.LayoutChanged;
    FirstVisibleCategoryIndexChanged;
  end;
end;

procedure TcxGridChartController.ActiveDataLevelChanged(APrevActiveDataLevel, AActiveDataLevel: Integer);
begin
  if (0 <= APrevActiveDataLevel) and (APrevActiveDataLevel < GridView.DataLevelCount) then
    FFirstVisibleCategoryIndexes[APrevActiveDataLevel] := FFirstVisibleCategoryIndex;
  if AActiveDataLevel <> -1 then
    if ActiveDiagramSupportsPaging then
      FirstVisibleCategoryIndex := FFirstVisibleCategoryIndexes[AActiveDataLevel]
    else
      FFirstVisibleCategoryIndex := FFirstVisibleCategoryIndexes[AActiveDataLevel];
end;

procedure TcxGridChartController.ActiveDiagramChanged(ADiagram: TcxGridChartDiagram);
begin
  FirstVisibleCategoryIndexChanged;
end;

procedure TcxGridChartController.CheckCoordinates;
begin
  inherited;
  if ActiveDiagramSupportsPaging then
    FirstVisibleCategoryIndex := FirstVisibleCategoryIndex;
end;

procedure TcxGridChartController.DataLevelsChanged;
begin
  FFirstVisibleCategoryIndexes := nil;
  SetLength(FFirstVisibleCategoryIndexes, GridView.DataLevelCount);
end;

procedure TcxGridChartController.FirstVisibleCategoryIndexChanged;
begin
  if FStoredFirstVisibleCategoryIndex <> FirstVisibleCategoryIndex then
  begin
    FStoredFirstVisibleCategoryIndex := FirstVisibleCategoryIndex;
    GridView.DoFirstVisibleCategoryIndexChanged;
  end;
end;

function TcxGridChartController.GetDataLevelActiveValuePopupClass: TcxGridChartDataLevelActiveValuePopupClass;
begin
  Result := TcxGridChartDataLevelActiveValuePopup;
end;

function TcxGridChartController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    Result := AHitTest.HitTestCode in [htLegendItem, htDataLevelInfo, htDiagramSelector,
      htChartValue, htChartValueLine, htChartValueArea, htSeriesSite, htSeriesSiteCaption];
end;

function TcxGridChartController.GetDiagramSelectorPopupClass: TcxGridChartDiagramSelectorPopupClass;
begin
  Result := TcxGridChartDiagramSelectorPopup;
end;

function TcxGridChartController.GetPatternObject(AObject: TPersistent): TPersistent;
begin
  if AObject is GridView.GetCategoriesClass then
    Result := TcxGridChartView(GridView.PatternGridView).Categories
  else
    if AObject is GridView.GetDataGroupClass then
      Result := TcxGridChartView(GridView.PatternGridView).FindDataGroupByID(TcxGridChartDataGroup(AObject).ID)
    else
      if AObject is GridView.GetSeriesClass then
        Result := TcxGridChartView(GridView.PatternGridView).FindSeriesByID(TcxGridChartSeries(AObject).ID)
      else
        Result := inherited GetPatternObject(AObject);
end;

function TcxGridChartController.GetFirstVisibleCategoryIndex: Integer;
begin
  if ActiveDiagramSupportsPaging then
    Result := FFirstVisibleCategoryIndex
  else
    Result := 0;
end;

function TcxGridChartController.GetVisibleCategoryCount(ACheckCount: Boolean): Integer;
begin
  Result := GridView.ViewData.VisibleCategoryCount;
  if ActiveDiagramSupportsPaging and (GridView.OptionsView.CategoriesPerPage <> 0) then
  begin
    Result := Min(Result, GridView.OptionsView.CategoriesPerPage);
    if ACheckCount then
      Result := Min(Result, Max(0, GridView.ViewData.VisibleCategoryCount - FirstVisibleCategoryIndex));
  end;
end;

function TcxGridChartController.CanDataDrillDown(AValueIndex: Integer): Boolean;
begin
  Result := GridView.ActiveDataLevel < GridView.DataLevelCount - 1;
end;

function TcxGridChartController.CanDataDrillUp: Boolean;
begin
  Result := GridView.ActiveDataLevel > 0;
end;

function TcxGridChartController.DoDataDrillDown(AValueIndex: Integer): Boolean;
begin
  Result := MayDataDrillDown and DataDrillDown(AValueIndex);
end;

function TcxGridChartController.DoDataDrillUp: Boolean;
begin
  Result := MayDataDrillDown and DataDrillUp;
end;

function TcxGridChartController.MayDataDrillDown(ACheckDesignTime: Boolean = True): Boolean;
begin
  Result := (not ACheckDesignTime or not GridView.IsDesigning) and
    GridView.OptionsCustomize.DataDrillDown;
end;

procedure TcxGridChartController.DoScroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  ADelta: Integer;
begin
  if AScrollBarKind <> GetScrollBarKind then Exit;
  ADelta := 0;
  case AScrollCode of
    scLineUp:
      ADelta := -1;
    scLineDown:
      ADelta := 1;
    scPageUp:
      ADelta := -GetScrollBarPageSize;
    scPageDown:
      ADelta := GetScrollBarPageSize;
    scTrack:
      FirstVisibleCategoryIndex := ScrollPosToCategoryIndex(AScrollPos);
  end;
  if ADelta <> 0 then
  begin
    if GridView.ActiveDiagram.PagingInOppositeDirection xor UseRightToLeftAlignment then
      ADelta := -ADelta;
    FirstVisibleCategoryIndex := FirstVisibleCategoryIndex + ADelta;
  end;
  AScrollPos := GetScrollBarPos;
end;

function TcxGridChartController.CanShowScrollBar: Boolean;
begin
  Result := ActiveDiagramSupportsPaging;
end;

function TcxGridChartController.GetScrollBarKind: TScrollBarKind;
begin
  if GridView.ActiveDiagram.HorizontalPaging then
    Result := sbHorizontal
  else
    Result := sbVertical;
end;

function TcxGridChartController.GetScrollBarPageSize: Integer;
begin
  Result := VisibleCategoryCount;
end;

function TcxGridChartController.GetScrollBarPos: Integer;
begin
  Result := ScrollPosToCategoryIndex(FirstVisibleCategoryIndex);
end;

function TcxGridChartController.GetScrollBarSize: Integer;
begin
  Result := GridView.ViewData.VisibleCategoryCount;
end;

function TcxGridChartController.ScrollPosToCategoryIndex(AScrollPos: Integer): Integer;
begin
  if GridView.ActiveDiagram.PagingInOppositeDirection xor UseRightToLeftAlignment then
    Result := GetScrollBarSize - GetScrollBarPageSize - AScrollPos
  else
    Result := AScrollPos;
end;

function TcxGridChartController.CanShowDataLevelActiveValuePopup(ACheckDesignTime: Boolean = True): Boolean;
begin
  Result := MayDataDrillDown(ACheckDesignTime);
end;

procedure TcxGridChartController.CustomizationChanged;
begin
  GridView.ViewInfo.ToolBoxViewInfo.InvalidateCustomizeButton;
  inherited;
end;

function TcxGridChartController.GetCustomizationFormClass: TcxCustomGridCustomizationFormClass;
begin
  Result := TcxGridChartCustomizationForm;
end;

function TcxGridChartController.GetCustomizationFormDefaultWidth: Integer;
begin
  Result := cxGridChartCustomizationFormDefaultWidth;
end;

function TcxGridChartController.GetDataLevelInfoDragAndDropObjectClass: TcxGridChartDataGroupMovingObjectClass;
begin
  Result := TcxGridChartDataGroupMovingObject;
end;

function TcxGridChartController.HasDataLevelActiveValuePopup: Boolean;
begin
  Result := FDataLevelActiveValuePopup <> nil;
end;

function TcxGridChartController.HasDiagramSelectorPopup: Boolean;
begin
  Result := FDiagramSelectorPopup <> nil;
end;

procedure TcxGridChartController.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button = mbRight) and not (ssDouble in Shift) and
    (GridView.OptionsCustomize.DataDrillUpMethod = ddumMouseRightButtonClick) then
    DoDataDrillUp;
end;

procedure TcxGridChartController.InitScrollBarsParameters;
begin
  if CanShowScrollBar then
    SetScrollBarInfo(GetScrollBarKind, 0, GetScrollBarSize - 1, 1,
      GetScrollBarPageSize, GetScrollBarPos, True, True);
end;

function TcxGridChartController.GetValueHintText(ASeries: TcxGridChartSeries; AValueIndex: Integer): string;
begin
  Result := Format(cxGetResourceString(@scxGridChartValueHintFormat),
    [ASeries.GetDisplayText, GridView.Categories.VisibleDisplayTexts[AValueIndex],
     ASeries.VisibleDisplayTexts[AValueIndex]]);
  GridView.DoGetValueHint(ASeries, AValueIndex, Result);
end;

function TcxGridChartController.DataDrillDown(AValueIndex: Integer): Boolean;
begin
  Result := CanDataDrillDown(AValueIndex);
  if not Result then Exit;
  GridView.BeginUpdate;
  try
    GridView.ActiveDataGroup.ActiveValue := GridView.ActiveDataGroup.VisibleValues[AValueIndex];
    GridView.ActiveDataLevel := GridView.ActiveDataLevel + 1;
  finally
    GridView.EndUpdate;
  end;
end;

function TcxGridChartController.DataDrillUp: Boolean;
begin
  Result := CanDataDrillUp;
  if Result then
    GridView.ActiveDataLevel := GridView.ActiveDataLevel - 1;
end;

function TcxGridChartController.IsDataDrillDownPossible(AValueIndex: Integer): Boolean;
begin
  Result := MayDataDrillDown and CanDataDrillDown(AValueIndex);
end;

function TcxGridChartController.ActiveDiagramSupportsPaging: Boolean;
begin
  Result := (GridView.ActiveDiagram <> nil) and GridView.ActiveDiagram.SupportsPaging;
end;

{ TcxGridChartDataController }

function TcxGridChartDataController.GetGridViewValue: TcxGridChartView;
begin
  Result := TcxGridChartView(GetOwner);
end;

function TcxGridChartDataController.GetOnAfterSummary: TcxAfterSummaryEvent;
begin
  Result := Summary.OnAfterSummary;
end;

function TcxGridChartDataController.GetOnSummary: TcxSummaryEvent;
begin
  Result := Summary.DefaultGroupSummaryItems.OnSummary;
end;

procedure TcxGridChartDataController.SetOnAfterSummary(Value: TcxAfterSummaryEvent);
begin
  Summary.OnAfterSummary := Value;
end;

procedure TcxGridChartDataController.SetOnSummary(Value: TcxSummaryEvent);
begin
  Summary.DefaultGroupSummaryItems.OnSummary := Value;
end;

procedure TcxGridChartDataController.AssignData(ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridChartDataController.CreateAllItems(AMissingItemsOnly: Boolean);
begin
end;

procedure TcxGridChartDataController.DeleteAllItems;
begin
end;

procedure TcxGridChartDataController.GetFakeComponentLinks(AList: TList);
begin
end;

function TcxGridChartDataController.GetGridView: TcxCustomGridView;
begin
  Result := GridView;
end;

function TcxGridChartDataController.HasAllItems: Boolean;
begin
  Result := True;
end;

function TcxGridChartDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridChartDataController.IsDataLinked: Boolean;
begin
  Result := True;
end;

function TcxGridChartDataController.SupportsCreateAllItems: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartDataController.DoValueTypeClassChanged(AItemIndex: Integer);
var
  AChartItem: IcxGridChartItem;
begin
  inherited;
  if Supports(GetItem(AItemIndex), IcxGridChartItem, AChartItem) then
    AChartItem.ValueTypeClassChanged;
end;

{function TcxGridChartDataController.GetChartViewItemIndex: Integer;
begin
  if GridView.SortedSeries = nil then
    Result := -1
  else
    Result := GridView.SortedSeries.DataBinding.DataIndex;
end;}

function TcxGridChartDataController.GetItemID(AItem: TObject): Integer;
var
  AChartItem: IcxGridChartItem;
begin
  if Supports(AItem, IcxGridChartItem, AChartItem) then
    Result := AChartItem.GetID
  else
    Result := -1;
end;

function TcxGridChartDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

procedure TcxGridChartDataController.Unlocked;
begin
  inherited;
  GridView.DataControllerUnlocked;
end;

procedure TcxGridChartDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridChartDataController.EndFullUpdate;
begin
  inherited;
  GridView.EndUpdate;
end;

function TcxGridChartDataController.GetItem(Index: Integer): TObject;
begin
  Result := Fields[Index].Item;
end;

{ TcxGridChartViewPainterHelper }

constructor TcxGridChartViewPainterHelper.Create(ACanvas: TcxCanvas);
begin
  FCanvas := ACanvas;
  FPenWidth := 1;
  FAlphaChannel := 255;
  FPenColor := clDefault;
  FSmoothingMode := smAntiAlias;
end;

procedure TcxGridChartViewPainterHelper.Pie(
  const R: TRect; AStartAngle, ASweepAngle: Integer);
begin
  Canvas.Pie(R, AStartAngle, ASweepAngle);
end;

procedure TcxGridChartViewPainterHelper.Polygon(const Points: array of TPoint);
begin
  Canvas.Polygon(Points);
end;

procedure TcxGridChartViewPainterHelper.Polyline(const Points: array of TPoint);
var
  APen: HPEN;
begin
  APen := SelectObject(Canvas.Handle, CreatePen);
  Canvas.Polyline(Points);
  APen := SelectObject(Canvas.Handle, APen);
  DeleteObject(APen);
end;

function TcxGridChartViewPainterHelper.CreatePen: HPen;
var
  APenParams: TLogBrush;
const
  PenTypes: array[Boolean] of Integer = (PS_COSMETIC, PS_GEOMETRIC);
  PenStyles: array[psSolid..psInsideFrame] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_SOLID);
begin
  APenParams.lbStyle := BS_SOLID;
  APenParams.lbColor := RealPenColor;
  APenParams.lbHatch := 0;
  Result := ExtCreatePen(PenTypes[PenWidth <> 1] or PenStyles[PenStyle], PenWidth,
    APenParams, 0, nil);
end;

function TcxGridChartViewPainterHelper.RealPenColor: TColor;
begin
  if PenColor = clDefault then
    Result := Canvas.Pen.Color
  else
    Result := ColorToRgb(PenColor);
end;

{ TcxGridChartViewGDIPlusPainterHelper }

procedure TcxGridChartViewGDIPlusPainterHelper.Pie(
  const R: TRect; AStartAngle, ASweepAngle: Integer);
var
  AGraphics: TdxGPGraphics;
begin
  if Canvas.RectVisible(R) then
  begin
    AGraphics := dxGpBeginPaint(Canvas.Handle, R);
    try
      AGraphics.SmoothingMode := SmoothingMode;
      AGraphics.Pie(R, AStartAngle, ASweepAngle,
        RealPenColor, Canvas.Brush.Color, PenWidth - 0.2, PenStyle, 255, 255);
    finally
      dxGpEndPaint(AGraphics);
    end;
  end;
end;

procedure TcxGridChartViewGDIPlusPainterHelper.Polygon(const Points: array of TPoint);
var
  AGraphics: TdxGPGraphics;
  APenAlphaChannel, ABrushAlphaChannel: Integer;
begin
  AGraphics := dxGpBeginPaint(Canvas.Handle, cxPointsBox(Points));
  try
    AGraphics.SmoothingMode := SmoothingMode;
    ABrushAlphaChannel := AlphaChannel;
    APenAlphaChannel := AlphaChannel;
    if Canvas.Brush.Style = bsClear then
      ABrushAlphaChannel := 0;
    if PenStyle = psClear then
      APenAlphaChannel := 0;
    AGraphics.Polygon(Points, RealPenColor, Canvas.Brush.Color, PenWidth - 0.2, PenStyle, APenAlphaChannel, ABrushAlphaChannel);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TcxGridChartViewGDIPlusPainterHelper.Polyline(const Points: array of TPoint);
var
  AGraphics: TdxGPGraphics;
begin
  AGraphics := dxGpBeginPaint(Canvas.Handle, cxPointsBox(Points));
  try
    AGraphics.SmoothingMode := SmoothingMode;
    AGraphics.Polyline(Points, RealPenColor, PenWidth - 0.2, PenStyle, 255);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

{ TcxGridChartTitlePainter }

function TcxGridChartTitlePainter.GetViewInfo: TcxCustomGridChartTitleViewInfo;
begin
  Result := TcxCustomGridChartTitleViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartTitlePainter.PrepareCanvasForDrawText;
begin
  inherited;
  if ViewInfo.Orientation = cpoVertical then
    Canvas.SetFontAngle(90);
end;

procedure TcxGridChartTitlePainter.UnprepareCanvasForDrawText;
begin
  if ViewInfo.Orientation = cpoVertical then
    Canvas.SetFontAngle(0);
  inherited;
end;

{ TcxGridChartLegendItemPainter }

function TcxGridChartLegendItemPainter.GetViewInfo: TcxGridChartLegendItemViewInfo;
begin
  Result := TcxGridChartLegendItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartLegendItemPainter.DrawContent;
begin
  inherited;
  DrawLegendKey;
end;

procedure TcxGridChartLegendItemPainter.DrawLegendKey;
begin
  Canvas.Rectangle(ViewInfo.LegendKeyBounds, ViewInfo.LegendKeyParams,
    ViewInfo.LegendKeyBorders, dxGetDarkerColor(ViewInfo.LegendKeyParams.Color, 75));
end;

{ TcxGridChartLegendPainter }

function TcxGridChartLegendPainter.GetViewInfo: TcxGridChartLegendViewInfo;
begin
  Result := TcxGridChartLegendViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartLegendPainter.DrawContent;
var
  I: Integer;
begin
  inherited;
  for I := 0 to ViewInfo.Count - 1 do
    ViewInfo.Items[I].Paint(Canvas);
end;

function TcxGridChartLegendPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartDiagramPainter }

function TcxGridChartDiagramPainter.GetViewInfo: TcxGridChartDiagramViewInfo;
begin
  Result := TcxGridChartDiagramViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartDiagramPainter.DrawValueCaptions;
var
  AClipRegion, AUsedSpaceRegion, ARegion: TcxRegion;
  I: Integer;
  AValueCaptionViewInfo: TcxGridChartDiagramValueCaptionViewInfo;
  ABounds: TRect;
begin
  AClipRegion := Canvas.GetClipRegion;
  AUsedSpaceRegion := TcxRegion.Create;
  try
    for I := 0 to ViewInfo.ValueViewInfoCount - 1 do
    begin
      PrepareClipRegionForValueDrawing(ViewInfo.ValueViewInfos[I], AClipRegion);
      AValueCaptionViewInfo := ViewInfo.ValueViewInfos[I].CaptionViewInfo;
      if AValueCaptionViewInfo.Visible then
      begin
        ABounds := AValueCaptionViewInfo.Bounds;
        ARegion := TcxRegion.Create(ABounds);
        try
          ARegion.Combine(AUsedSpaceRegion, roIntersect, False);
          if ARegion.IsEmpty or not ViewInfo.GridView.OptionsView.TransparentCaptions then
          begin
            AValueCaptionViewInfo.Paint(Canvas);
            AUsedSpaceRegion.Combine(ABounds, roAdd);
          end;
        finally
          ARegion.Free;
        end;
      end;
    end;
  finally
    AUsedSpaceRegion.Free;
    AClipRegion.Free;
  end;
end;

procedure TcxGridChartDiagramPainter.DrawValues;
var
  AClipRegion: TcxRegion;
  I: Integer;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    for I := 0 to ViewInfo.ValueViewInfoCount - 1 do
    begin
      PrepareClipRegionForValueDrawing(ViewInfo.ValueViewInfos[I], AClipRegion);
      ViewInfo.ValueViewInfos[I].Paint(Canvas);
    end;
  finally
    AClipRegion.Free;
  end;
end;

procedure TcxGridChartDiagramPainter.FillAndExcludeRect(const R: TRect);
begin
  Canvas.FillRect(R);
  Canvas.ExcludeClipRect(R);
end;

function TcxGridChartDiagramPainter.NeedsPainting: Boolean;
var
  I: Integer;
  AValueCaptionViewInfo: TcxGridChartDiagramValueCaptionViewInfo;
begin
  Result := inherited NeedsPainting;
  if not Result then
    for I := 0 to ViewInfo.ValueViewInfoCount - 1 do
    begin
      AValueCaptionViewInfo := ViewInfo.ValueViewInfos[I].CaptionViewInfo;
      Result := AValueCaptionViewInfo.Visible and
        Canvas.RectVisible(AValueCaptionViewInfo.GetAreaBoundsForPainting);
      if Result then Break;
    end;
end;

procedure TcxGridChartDiagramPainter.Paint;
begin
  inherited;
  DrawValueCaptions;
end;

procedure TcxGridChartDiagramPainter.PrepareClipRegionForValueDrawing(
  AValueViewInfo: TcxGridChartDiagramValueViewInfo; AOriginalClipRegion: TcxRegion);
begin
end;

{ TcxGridChartHistogramTickMarkLabelsPainter }

function TcxGridChartHistogramTickMarkLabelsPainter.GetViewInfo: TcxGridChartHistogramTickMarkLabelsViewInfo;
begin
  Result := TcxGridChartHistogramTickMarkLabelsViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartHistogramTickMarkLabelsPainter.DrawCaptions;
var
  I: Integer;
  ABounds, APrevBounds: TRect;
  ATextAttributes: Integer;
begin
  APrevBounds := cxInvalidRect;
  Canvas.SetParams(ViewInfo.Params);
  Canvas.Brush.Style := bsClear;
  ATextAttributes := ViewInfo.GetTextAttributes(True);
  for I := 0 to ViewInfo.Count - 1 do
  begin
    ABounds := ViewInfo.CaptionBounds[I];
    if not ViewInfo.IsVertical or not cxRectIntersect(ABounds, APrevBounds) then
    begin
      Canvas.DrawText(ViewInfo.Captions[I], ABounds, ATextAttributes);
      if ViewInfo.IsVertical then
        Canvas.ExcludeClipRect(ABounds);
      APrevBounds := ABounds;
    end;
  end;
  Canvas.Brush.Style := bsSolid;
end;

procedure TcxGridChartHistogramTickMarkLabelsPainter.DrawContent;
begin
  if ViewInfo.CaptionsVisible then
    DrawCaptions;
  ViewInfo.TitleViewInfo.Paint(Canvas);
end;

{ TcxGridChartHistogramPainter }

function TcxGridChartHistogramPainter.GetViewInfo: TcxGridChartHistogramViewInfo;
begin
  Result := TcxGridChartHistogramViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartHistogramPainter.DrawCategoryAxis;
var
  R: TRect;
  I: Integer;
begin
  Canvas.Brush.Color := ViewInfo.CategoryAxisColor;

  R := ViewInfo.CategoryAxisBounds;
  FillAndExcludeRect(R);

  for I := 0 to ViewInfo.CategoryMarkCount - 1 do
  begin
    R := ViewInfo.CategoryMarkBounds[I];
    FillAndExcludeRect(R);
  end;
end;

procedure TcxGridChartHistogramPainter.DrawCategoryGridLines;
var
  I: Integer;
  R: TRect;
begin
  Canvas.Brush.Color := ViewInfo.CategoryGridLineColor;
  for I := 0 to ViewInfo.CategoryMarkCount - 1 do
  begin
    R := ViewInfo.CategoryGridLineBounds[I];
    FillAndExcludeRect(R);
  end;
end;

procedure TcxGridChartHistogramPainter.DrawContent;
var
  AClipRegion, APlotClipRegion, AValuesClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    if ViewInfo.HasCategoryAxis then DrawCategoryAxis;
    if ViewInfo.HasValueAxis then DrawValueAxis;
    APlotClipRegion := Canvas.GetClipRegion;
    try
      APlotClipRegion.Combine(ViewInfo.PlotBounds, roSubtract);
      if not TransparentValues then DrawValues;
      SetValuesClipRegion;
      AValuesClipRegion := Canvas.GetClipRegion;
      if ViewInfo.HasCategoryGridLines then DrawCategoryGridLines;
      if ViewInfo.HasValueGridLines then DrawValueGridLines;
      if not ViewInfo.DoCustomDrawPlot(Canvas) then
        DrawPlotBackground;
      Canvas.SetClipRegion(AValuesClipRegion, roSet);
      DrawValuesBackground;
    finally
      Canvas.SetClipRegion(APlotClipRegion, roSet);
    end;
    inherited;
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
  ViewInfo.CategoryTickMarkLabelsViewInfo.Paint(Canvas);
  ViewInfo.ValueTickMarkLabelsViewInfo.Paint(Canvas);
  if TransparentValues then
  begin
    AClipRegion := Canvas.GetClipRegion;
    try
      SetValuesClipRegion;
      DrawValues;
    finally
      Canvas.SetClipRegion(AClipRegion, roSet);
    end;
  end;
end;

procedure TcxGridChartHistogramPainter.DrawPlotBackground;
begin
  Canvas.FillRect(ViewInfo.PlotBounds, ViewInfo.PlotParams);
  Canvas.ExcludeClipRect(ViewInfo.PlotBounds);
end;

procedure TcxGridChartHistogramPainter.DrawValueAxis;
var
  R: TRect;
  I: Integer;
begin
  Canvas.Brush.Color := ViewInfo.ValueAxisColor;

  R := ViewInfo.ValueAxisBounds;
  FillAndExcludeRect(R);

  for I := 0 to ViewInfo.ValueMarkCount - 1 do
  begin
    R := ViewInfo.ValueMarkBounds[I];
    FillAndExcludeRect(R);
  end;
end;

procedure TcxGridChartHistogramPainter.DrawValueGridLines;
var
  I: Integer;
  R: TRect;
begin
  Canvas.Brush.Color := ViewInfo.ValueGridLineColor;
  for I := 0 to ViewInfo.ValueMarkCount - 1 do
  begin
    R := ViewInfo.ValueGridLineBounds[I];
    FillAndExcludeRect(R);
  end;
end;

procedure TcxGridChartHistogramPainter.DrawValuesBackground;
begin
end;

function TcxGridChartHistogramPainter.GetValuesClipRect: TRect;
begin
  Result := ViewInfo.Bounds;
  if ViewInfo.IsValueAxisVert then
  begin
    Result.Top := ViewInfo.PlotBounds.Top;
    Result.Bottom := ViewInfo.PlotBounds.Bottom;
  end
  else
  begin
    Result.Left := ViewInfo.PlotBounds.Left;
    Result.Right := ViewInfo.PlotBounds.Right;
  end;
end;

procedure TcxGridChartHistogramPainter.SetValuesClipRegion;
begin
  if ViewInfo.HasValuesOutOfRange then
    Canvas.SetClipRegion(TcxRegion.Create(GetValuesClipRect), roIntersect);
end;

{ TcxGridChartColumnDiagramValuePainter }

function TcxGridChartColumnDiagramValuePainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartColumnDiagramPainter }

function TcxGridChartColumnDiagramPainter.GetTransparentValues: Boolean;
begin
  Result := False;
end;

{ TcxGridChartColumnValueCaptionPainter }

procedure TcxGridChartColumnValueCaptionPainter.DrawBackground;
begin
  inherited DrawBackground;
end;

{ TcxGridChartLineDiagramLegendItemPainter }

function TcxGridChartLineDiagramLegendItemPainter.GetViewInfo: TcxGridChartLineDiagramLegendItemViewInfo;
begin
  Result := TcxGridChartLineDiagramLegendItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartLineDiagramLegendItemPainter.DrawLegendKey;
begin
  DrawMarker;
  DrawLine;
end;

procedure TcxGridChartLineDiagramLegendItemPainter.DrawLine;
begin
  ViewInfo.GetDiagramValueViewInfoClass.GetPainterClassEx.DrawLineEx(Canvas,
    ViewInfo.LineStart, ViewInfo.LineFinish, ViewInfo.LineStyle, ViewInfo.LineWidth,
    ViewInfo.LineColor, ViewInfo.GridView.OptionsView.Antialiasing);
end;

procedure TcxGridChartLineDiagramLegendItemPainter.DrawMarker;
begin
  ViewInfo.GetDiagramValueViewInfoClass.GetPainterClassEx.DrawMarkerEx(
    Canvas, ViewInfo.MarkerBounds, ViewInfo.MarkerPoints, ViewInfo.MarkerStyle,
    ViewInfo.MarkerParams, ViewInfo.GridView.OptionsView.Antialiasing);
end;

{ TcxGridChartLineDiagramValuePainter }

function TcxGridChartLineDiagramValuePainter.GetViewInfo: TcxGridChartLineDiagramValueViewInfo;
begin
  Result := TcxGridChartLineDiagramValueViewInfo(inherited ViewInfo);
end;

function TcxGridChartLineDiagramValuePainter.CanDrawDesignSelection: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartLineDiagramValuePainter.DrawContent;
begin
  inherited;
  DrawMarker;
  DrawLine;
end;

procedure TcxGridChartLineDiagramValuePainter.DrawLine;
begin
  DrawLineEx(Canvas, ViewInfo.LineStart, ViewInfo.LineFinish, ViewInfo.LineStyle,
    ViewInfo.LineWidth, ViewInfo.LineColor, ViewInfo.GridView.OptionsView.Antialiasing);
end;

procedure TcxGridChartLineDiagramValuePainter.DrawMarker;
begin
  DrawMarkerEx(Canvas, ViewInfo.MarkerBounds, ViewInfo.MarkerPoints,
    ViewInfo.MarkerStyle, ViewInfo.MarkerParams, ViewInfo.GridView.OptionsView.Antialiasing);
end;

class procedure TcxGridChartLineDiagramValuePainter.DrawLineEx(ACanvas: TcxCanvas;
  const AStart, AFinish: TPoint; AStyle: TcxGridChartLineStyle;
  AWidth: Integer; AColor: TColor; Antialiasing: Boolean);
var
  APoints: TPoints;
begin
  SetLength(APoints, 2);
  APoints[0] := AStart;
  APoints[1] := AFinish;
  DrawLines(ACanvas, APoints, AStyle, AWidth, AColor, Antialiasing);
end;

class procedure TcxGridChartLineDiagramValuePainter.DrawLines(ACanvas: TcxCanvas;
  const APoints: TPoints; AStyle: TcxGridChartLineStyle; AWidth: Integer;
  AColor: TColor; Antialiasing: Boolean);
begin
  if AStyle = clsNone then Exit;
  with PainterHelperClasses[Antialiasing and CheckGdiPlus].Create(ACanvas) do
  try
    PenColor := AColor;
    PenWidth := AWidth;
    PenStyle := TPenStyle(Byte(AStyle) - 1);
    Polyline(APoints);
  finally
    Free;
  end;
end;

//todo: msn - in below antialiasing not implemented !!!

class procedure TcxGridChartLineDiagramValuePainter.DrawMarkerEx(ACanvas: TcxCanvas;
  const ABounds: TRect; const APoints: TPoints; AStyle: TcxGridChartMarkerStyle;
  const AParams: TcxViewParams; Antialiasing: Boolean);
var
  ARegion, AInternalRegion: TcxRegion;
  ARgn: HRGN;
  R: TRect;
begin
  case AStyle of
    cmsSquare, cmsTriangle, cmsDiamond:
      begin
        if AStyle = cmsSquare then
          ARgn := CreateRectRgnIndirect(ABounds)
        else
          ARgn := CreatePolygonRgn(APoints[0], Length(APoints), WINDING);
        ARegion := TcxRegion.Create(ARgn);
        try
          ACanvas.DrawRegion(ARegion, AParams.Color, AParams.TextColor);
        finally
          ACanvas.SetClipRegion(ARegion, roSubtract);
        end;
      end;
    cmsCircle:
      begin
        R := ABounds;
        Inc(R.Right);
        Inc(R.Bottom);
        ARegion := TcxRegion.Create(CreateEllipticRgnIndirect(R));
        try
          ACanvas.FillRegion(ARegion, AParams.TextColor);
          if ABounds.Right - ABounds.Left = 4 then
            InflateRect(R, 0, -1)
          else
            InflateRect(R, -1, -1);
          AInternalRegion := TcxRegion.Create(CreateEllipticRgnIndirect(R));
          try
            ACanvas.FillRegion(AInternalRegion, AParams.Color);
          finally
            AInternalRegion.Free;
          end;
        finally
          ACanvas.SetClipRegion(ARegion, roSubtract);
        end;
      end;
  end;
end;

{ TcxGridChartLineDiagramPainter }

procedure TcxGridChartLineDiagramPainter.DrawContent;
begin
  inherited;
  DrawValuesDesignSelection;
end;

procedure TcxGridChartLineDiagramPainter.DrawValuesDesignSelection;
var
  I: Integer;
  AViewInfo: TcxGridChartDiagramValueViewInfo;
begin
  for I := 0 to ViewInfo.ValueViewInfoCount - 1 do
  begin
    AViewInfo := ViewInfo.ValueViewInfos[I];
    AViewInfo.GetPainterClass.DoDrawDesignSelection(Canvas, AViewInfo);
  end;
end;

function TcxGridChartLineDiagramPainter.GetTransparentValues: Boolean;
begin
  Result := True;
end;

{ TcxGridChartAreaDiagramLegendItemPainter }

procedure TcxGridChartAreaDiagramLegendItemPainter.DrawAreaBackground;
begin
  Canvas.FillRect(ViewInfo.LegendKeyBounds, ViewInfo.LegendKeyParams);
end;

procedure TcxGridChartAreaDiagramLegendItemPainter.DrawAreaBorder;

  function GetPointsRect: TRect;
  begin
    Result := ViewInfo.LegendKeyBounds;
    InflateRect(Result, -ViewInfo.LineWidth div 2, -ViewInfo.LineWidth div 2);
    if Odd(ViewInfo.LineWidth) then
    begin
      Dec(Result.Right);
      Dec(Result.Bottom);
    end;
  end;

  procedure CalculatePointsForRect(const ARect: TRect; var APoints: TPoints);
  begin
    SetLength(APoints, 5);
    APoints[0] := ARect.TopLeft;
    APoints[1] := Point(ARect.Right, ARect.Top);
    APoints[2] := ARect.BottomRight;
    APoints[3] := Point(ARect.Left, ARect.Bottom);
    APoints[4] := APoints[0];
  end;

var
  APoints: TPoints;
begin
  if ViewInfo.LineStyle = clsNone then Exit;
  CalculatePointsForRect(GetPointsRect, APoints);
  ViewInfo.GetDiagramValueViewInfoClass.GetPainterClassEx.DrawLines(Canvas,
    APoints, ViewInfo.LineStyle, ViewInfo.LineWidth, ViewInfo.LineColor,
    ViewInfo.GridView.OptionsView.Antialiasing);
end;

procedure TcxGridChartAreaDiagramLegendItemPainter.DrawLegendKey;
begin
  DrawMarker;
  DrawAreaBackground;
  DrawAreaBorder;
end;

{ TcxGridChartAreaDiagramValuePainter }

function TcxGridChartAreaDiagramValuePainter.GetViewInfo: TcxGridChartAreaDiagramValueViewInfo;
begin
  Result := TcxGridChartAreaDiagramValueViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartAreaDiagramValuePainter.DoDrawArea;
begin
  if not ViewInfo.DoCustomDrawBackground(Canvas) then
    DrawArea;
end;

procedure TcxGridChartAreaDiagramValuePainter.DrawArea;
begin
  with PainterHelperClasses[ViewInfo.GridView.OptionsView.Antialiasing and CheckGdiPlus].Create(Canvas) do
  try
    PenColor := ViewInfo.AreaColor;
    PenStyle := psClear;
    Canvas.Brush.Color := ViewInfo.AreaColor;
    AlphaChannel := 255 - ViewInfo.Diagram.Transparency;
    Polygon(ViewInfo.AreaPoints);
  finally
    Free;
  end;
end;

{ TcxGridChartAreaDiagramPainter }

procedure TcxGridChartAreaDiagramPainter.DrawValuesBackground;
var
  I: Integer;
  AValueViewInfo: TcxGridChartAreaDiagramValueViewInfo;
  ABackgroundRegion: TcxRegion;
begin
  inherited;
  ABackgroundRegion := nil;
  for I := 0 to ViewInfo.ValueViewInfoCount - 1 do
  begin
    AValueViewInfo := TcxGridChartAreaDiagramValueViewInfo(ViewInfo.ValueViewInfos[I]);
    with TcxGridChartAreaDiagramValuePainter(AValueViewInfo.GetPainterClass.Create(Canvas, AValueViewInfo)) do
      try
        DoDrawArea;
      finally
        Free;
      end;
    if ABackgroundRegion = nil then
      ABackgroundRegion := TcxRegion.Create;
    ABackgroundRegion.Combine(AValueViewInfo.CreateAreaRegion, roAdd);
    if TcxGridChartAreaDiagramViewInfo(ViewInfo).ExcludeEachSeriesArea and ((I = ViewInfo.ValueViewInfoCount - 1) or
      (ViewInfo.ValueViewInfos[I + 1].SeriesIndex <> AValueViewInfo.SeriesIndex)) then
    begin
      Canvas.SetClipRegion(ABackgroundRegion, roSubtract);
      ABackgroundRegion := nil;
    end;
  end;
  if ABackgroundRegion <> nil then
    Canvas.SetClipRegion(ABackgroundRegion, roSubtract);
end;

{unction TcxGridChartAreaDiagramPainter.GetTransparentValues: Boolean;
begin
  Result := False;
end;}

{ TcxGridChartPieDiagramValuePainter }

function TcxGridChartPieDiagramValuePainter.GetViewInfo: TcxGridChartPieDiagramValueViewInfo;
begin
  Result := TcxGridChartPieDiagramValueViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartPieDiagramValuePainter.DrawContent;
begin
  inherited;
  DrawSlice;
end;

procedure TcxGridChartPieDiagramValuePainter.DrawLeaderLine;
begin
  with PainterHelperClasses[ViewInfo.GridView.OptionsView.Antialiasing and CheckGdiPlus].Create(Canvas) do
  try
    Canvas.Pen.Color := ViewInfo.Params.TextColor;
    Polyline(ViewInfo.LeaderLinePoints);
  finally
    Free;
  end;
end;

procedure TcxGridChartPieDiagramValuePainter.DrawSlice;
begin
  with PainterHelperClasses[ViewInfo.GridView.OptionsView.Antialiasing and CheckGdiPlus].Create(Canvas) do
  try
    Canvas.SetParams(ViewInfo.Params);
    PenColor := Canvas.Pen.Color;
    Pie(ViewInfo.Bounds, 90 - ViewInfo.FinishAngle,
      ViewInfo.FinishAngle - ViewInfo.StartAngle);
  finally
    Free;
  end;
end;

procedure TcxGridChartPieDiagramValuePainter.MainPaint;
begin
  inherited;
  if NeedsPainting and (ViewInfo.CaptionPosition = pdvcpOutsideEndWithLeaderLines) then
    DrawLeaderLine;
end;

{ TcxGridChartPieSeriesSiteCaptionPainter }

function TcxGridChartPieSeriesSiteCaptionPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartPieSeriesSitePainter }

function TcxGridChartPieSeriesSitePainter.CanDrawDesignSelection: Boolean;
begin
  Result := False;
end;

function TcxGridChartPieSeriesSitePainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartPieDiagramPainter }

function TcxGridChartPieDiagramPainter.GetViewInfo: TcxGridChartPieDiagramViewInfo;
begin
  Result := TcxGridChartPieDiagramViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartPieDiagramPainter.DrawContent;
var
  AClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    ExcludeSeriesPieAreasFromClipRegion;
    DrawSeriesSites;
    inherited;
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
  DrawValues;
  DrawSeriesDesignSelection;
end;

procedure TcxGridChartPieDiagramPainter.DrawSeriesDesignSelection;
var
  I: Integer;
  ASeriesSiteViewInfo: TcxGridChartPieSeriesSiteViewInfo;
begin
  for I := 0 to ViewInfo.SeriesSiteViewInfoCount - 1 do
  begin
    ASeriesSiteViewInfo := ViewInfo.SeriesSiteViewInfos[I];
    ASeriesSiteViewInfo.GetPainterClass.DoDrawDesignSelection(Canvas, ASeriesSiteViewInfo);
  end;
end;

procedure TcxGridChartPieDiagramPainter.DrawSeriesSites;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.SeriesSiteViewInfoCount - 1 do
    ViewInfo.SeriesSiteViewInfos[I].Paint(Canvas);
end;

procedure TcxGridChartPieDiagramPainter.DrawValues;
var
  AClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    inherited;
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
end;

procedure TcxGridChartPieDiagramPainter.ExcludeSeriesPieAreasFromClipRegion;
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to ViewInfo.SeriesSiteViewInfoCount - 1 do
    if not ViewInfo.SeriesSiteViewInfos[I].IsPieEmpty then
    begin
      R := ViewInfo.SeriesSiteViewInfos[I].PieAreaBounds;
      InflateRect(R, -1, -1);  // to fill the gap between drawn pie and elliptic region
      if not IsRectEmpty(R) then
        Canvas.SetClipRegion(TcxRegion.Create(CreateEllipticRgnIndirect(R)), roSubtract);
    end;
end;

procedure TcxGridChartPieDiagramPainter.PrepareClipRegionForValueDrawing(
  AValueViewInfo: TcxGridChartDiagramValueViewInfo; AOriginalClipRegion: TcxRegion);
var
  ARegion: TcxRegion;
begin
  if AValueViewInfo.VisibleValueIndex <> 0 then Exit;
  ARegion := TcxRegion.Create(ViewInfo.SeriesSiteViewInfos[AValueViewInfo.SeriesIndex].ContentBounds);
  ARegion.Combine(AOriginalClipRegion, roIntersect, False);
  Canvas.SetClipRegion(ARegion, roSet);
end;

{ TcxGridChartToolBoxItemSeparatorPainter }

procedure TcxGridChartToolBoxItemSeparatorPainter.DrawContent;
begin
  Canvas.Pen.Color := ViewInfo.Params.Color;
  Canvas.Pen.Style := psDot;
  with ViewInfo.Bounds do
    Canvas.Line(Left, Top, Left, Bottom);
  Canvas.Pen.Style := psSolid;
{  Canvas.Brush.Color := ViewInfo.ItemSeparatorColor;
  Canvas.FillRect(ViewInfo.Bounds);}
end;

{ TcxGridChartToolBoxDataLevelActiveValuePainter }

function TcxGridChartToolBoxDataLevelActiveValuePainter.GetViewInfo: TcxGridChartToolBoxDataLevelActiveValueViewInfo;
begin
  Result := TcxGridChartToolBoxDataLevelActiveValueViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartToolBoxDataLevelActiveValuePainter.PrepareCanvasForDrawText;
begin
  inherited;
  if ViewInfo.GridView.Controller.CanShowDataLevelActiveValuePopup(False) then
    with Canvas.Font do
      Style := Style + [fsUnderline];
end;

{ TcxGridChartToolBoxDataLevelInfoPainter }

procedure TcxGridChartToolBoxDataLevelInfoPainter.DrawBackground;
begin
  DrawBackground(ViewInfo.ContentBounds);
end;

function TcxGridChartToolBoxDataLevelInfoPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartToolBoxCustomizeButtonPainter }

function TcxGridChartToolBoxCustomizeButtonPainter.GetViewInfo: TcxGridChartToolBoxCustomizeButtonViewInfo;
begin
  Result := TcxGridChartToolBoxCustomizeButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartToolBoxCustomizeButtonPainter.Paint;
begin
  Canvas.Font := ViewInfo.Params.Font;
  ViewInfo.LookAndFeelPainter.DrawScaledButton(Canvas, ViewInfo.Bounds, ViewInfo.Text, ViewInfo.ButtonState, ScaleFactor);
end;

{ TcxGridChartToolBoxDiagramSelectorPainter }

function TcxGridChartToolBoxDiagramSelectorPainter.GetViewInfo: TcxGridChartToolBoxDiagramSelectorViewInfo;
begin
  Result := TcxGridChartToolBoxDiagramSelectorViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartToolBoxDiagramSelectorPainter.DrawContent;
begin
  inherited;
  DrawImage;
end;

procedure TcxGridChartToolBoxDiagramSelectorPainter.DrawImage;
begin
  if ViewInfo.ImageIndex <> -1 then
    cxDrawImage(Canvas, ViewInfo.ImageBounds, nil, cxGridChartDiagramImages, ViewInfo.ImageIndex, True, nil, ScaleFactor);
end;

procedure TcxGridChartToolBoxDiagramSelectorPainter.PrepareCanvasForDrawText;
begin
  inherited;
  if ViewInfo.CanShowDropDownWindow then
    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
end;

{ TcxGridChartToolBoxPainter }

function TcxGridChartToolBoxPainter.GetViewInfo: TcxGridChartToolBoxViewInfo;
begin
  Result := TcxGridChartToolBoxViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartToolBoxPainter.DrawContent;
var
  AClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    Canvas.IntersectClipRect(ViewInfo.ClientBounds);
    DrawItems(True);
    DrawDataLevelInfoConnectors;
    inherited;
    DrawItems(False);
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
end;

procedure TcxGridChartToolBoxPainter.DrawDataLevelInfoConnectors;
var
  I: Integer;
begin
  Canvas.Brush.Color := ViewInfo.DataLevelInfoConnectorColor;
  for I := 0 to ViewInfo.DataLevelInfoConnectorCount - 1 do
    Canvas.FillRect(ViewInfo.DataLevelInfoConnectors[I], nil, True);
end;

procedure TcxGridChartToolBoxPainter.DrawItems(AOpaqueItems: Boolean);
var
  I: Integer;
begin
  for I := 0 to ViewInfo.Count - 1 do
    if ViewInfo.Items[I].HasBackground = AOpaqueItems then
      ViewInfo.Items[I].Paint(Canvas);
end;

function TcxGridChartToolBoxPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridChartPainter }

function TcxGridChartPainter.GetViewInfo: TcxGridChartViewInfo;
begin
  Result := TcxGridChartViewInfo(inherited ViewInfo);
end;

procedure TcxGridChartPainter.PaintAfter;
begin
  inherited;
  ViewInfo.TitleViewInfo.Paint(Canvas);
  if ViewInfo.DiagramViewInfo <> nil then
    ViewInfo.DiagramViewInfo.Paint(Canvas);
end;

procedure TcxGridChartPainter.PaintContent;
begin
  inherited;
  if ViewInfo.DiagramViewInfo <> nil then
    ExcludeFromBackground(ViewInfo.DiagramViewInfo.Bounds);
  ViewInfo.ToolBoxViewInfo.Paint(Canvas);
  if ViewInfo.LegendViewInfo <> nil then
    ViewInfo.LegendViewInfo.Paint(Canvas);
end;

{ TcxGridChartViewData }

constructor TcxGridChartViewData.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  UpdateDataGroupIndex;
end;

function TcxGridChartViewData.GetCategory(AIndex: Integer): Variant;
begin
  Result := DataController.Values[AIndex, GridView.Categories.DataBinding.DataIndex];
end;

function TcxGridChartViewData.GetCategoryCount: Integer;
begin
  Result := DataController.RecordCount;
end;

function TcxGridChartViewData.GetChildCount: Integer;
begin
  Result := DataController.Groups.ChildCount[DataGroupIndex];
end;

function TcxGridChartViewData.GetChildDataGroupIndex(Index: Integer): Integer;
begin
  Result := DataController.Groups.ChildDataGroupIndex[DataGroupIndex, Index];
end;

function TcxGridChartViewData.GetChildLevel: Integer;
begin
  Result := GridView.ActiveDataLevel;
end;

function TcxGridChartViewData.GetChildRecordIndex(Index: Integer): Integer;
begin
  Result := DataController.Groups.ChildRecordIndex[DataGroupIndex, Index];
end;

function TcxGridChartViewData.GetDataGroupIndex: Integer;
begin
  if FDataGroupIndex = -2 then
    FDataGroupIndex := CalculateDataGroupIndex;
  Result := FDataGroupIndex;
end;

function TcxGridChartViewData.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartViewData.GetGroupValue(ADataGroupIndex, AIndex: Integer): Variant;
begin
  Result := DataController.Values[AIndex,
    GridView.DataGroups[ADataGroupIndex].DataBinding.DataIndex];
end;

function TcxGridChartViewData.GetIsEmptyValue(ASeriesIndex, AIndex: Integer): Boolean;
var
  AValue: Variant;
begin
  ASeriesIndex := GridView.VisibleSeries[ASeriesIndex].Index;
  if IsDataGrouped then
    if IsSummaryLevel then
    begin
      AValue := DataController.Summary.GroupSummaryValues[
        ChildDataGroupIndex[AIndex],
        GridView.Series[ASeriesIndex].DataBinding.SummaryIndex];
    end
    else
      AValue := DataController.Values[ChildRecordIndex[AIndex],
        GridView.Series[ASeriesIndex].DataBinding.DataIndex]
  else
    AValue := DataController.Values[AIndex,
      GridView.Series[ASeriesIndex].DataBinding.DataIndex];
  Result := not IsValueValid(AValue);
end;

function TcxGridChartViewData.GetSumOfValues(ASeriesIndex: Integer): Variant;
begin
  if FSumOfValues = nil then CalculateSumsOfValues;
  Result := FSumOfValues[ASeriesIndex];
end;

function TcxGridChartViewData.GetValue(ASeriesIndex, AIndex: Integer): Variant;
begin
  Result := DataController.Values[AIndex,
    GridView.Series[ASeriesIndex].DataBinding.DataIndex];
  if not IsValueValid(Result) then
    Result := 0;
end;

function TcxGridChartViewData.GetVisibleCategory(AIndex: Integer): Variant;
begin
  if IsDataGrouped then
    if IsSummaryLevel then
      Result := DataController.Groups.GroupValues[ChildDataGroupIndex[AIndex]]
    else
      Result := Categories[ChildRecordIndex[AIndex]]
  else
    Result := Categories[GetRecordIndexByValueIndex(AIndex)];
end;

function TcxGridChartViewData.GetVisibleCategoryCount: Integer;
begin
  if IsDataGrouped then
    Result := ChildCount
  else
    Result := DataController.RowCount;
end;

function TcxGridChartViewData.GetVisibleGroupValue(AVisibleDataGroupIndex, AIndex: Integer): Variant;
begin
  Result := DataController.Groups.GroupValues[
    DataController.Groups.ChildDataGroupIndex[GetDataGroupIndexByLevel(AVisibleDataGroupIndex), AIndex]];
end;

function TcxGridChartViewData.GetVisibleGroupValueCount(AVisibleDataGroupIndex: Integer): Integer;
begin
  Result := DataController.Groups.ChildCount[GetDataGroupIndexByLevel(AVisibleDataGroupIndex)];
end;

function TcxGridChartViewData.GetVisibleValue(ASeriesIndex, AIndex: Integer): Variant;
begin
  if IsDataGrouped then
    if IsSummaryLevel then
    begin
      Result := DataController.Summary.GroupSummaryValues[
        ChildDataGroupIndex[AIndex],
        GridView.Series[ASeriesIndex].DataBinding.SummaryIndex];
      if not IsValueValid(Result) then
        Result := 0;
    end
    else
      Result := Values[ASeriesIndex, ChildRecordIndex[AIndex]]
  else
    Result := Values[ASeriesIndex, GetRecordIndexByValueIndex(AIndex)];
end;

procedure TcxGridChartViewData.SetCategory(AIndex: Integer; const Value: Variant);
begin
  DataController.Values[AIndex, GridView.Categories.DataBinding.DataIndex] := Value;
end;

procedure TcxGridChartViewData.SetCategoryCount(Value: Integer);
begin
  DataController.RecordCount := Value;
end;

procedure TcxGridChartViewData.SetGroupValue(ADataGroupIndex, AIndex: Integer;
  const Value: Variant);
begin
  DataController.Values[AIndex,
    GridView.DataGroups[ADataGroupIndex].DataBinding.DataIndex] := Value;
end;

procedure TcxGridChartViewData.SetValue(ASeriesIndex, AIndex: Integer;
  const Value: Variant);
begin
  DataController.Values[AIndex,
    GridView.Series[ASeriesIndex].DataBinding.DataIndex] := Value;
end;

function TcxGridChartViewData.CalculateDataGroupIndex: Integer;
begin
  Result := GetDataGroupIndexByLevel(ChildLevel);
end;

function TcxGridChartViewData.CalculateSumOfValues(ASeriesIndex: Integer): Variant;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleCategoryCount - 1 do
    Result := Result + Abs(VisibleValues[ASeriesIndex, I]);
end;

procedure TcxGridChartViewData.CheckValueAtLevel(ALevel: Integer; var AValue: Variant);
var
  ADataGroupIndex: Integer;
begin
  ADataGroupIndex := GetDataGroupIndexByLevel(ALevel);
  if (ADataGroupIndex = -2) or
    (DataController.Groups.GetDataGroupIndexByGroupValue(ADataGroupIndex, AValue) = -1) then
    AValue := Null;
end;

function TcxGridChartViewData.GetDataGroupIndexByLevel(ALevel: Integer): Integer;
var
  I, ADataGroupIndex: Integer;
begin
  Result := -1;
  for I := 0 to ALevel - 1 do
  begin
    ADataGroupIndex := DataController.Groups.GetDataGroupIndexByGroupValue(Result,
      (DataController.GetItem(DataController.Groups.GroupingItemIndex[I]) as TcxGridChartDataGroup).ActiveValue);
    if ADataGroupIndex = -1 then
    begin
      Result := -2;
      Break;
    end;
{    if ADataGroupIndex = -1 then
      ADataGroupIndex := DataController.Groups.ChildDataGroupIndex[Result, 0];}
    Result := ADataGroupIndex;
  end;
end;

function TcxGridChartViewData.GetRecordIndexByValueIndex(AValueIndex: Integer): Integer;
begin
  Result := DataController.GetRowInfo(AValueIndex).RecordIndex;
end;

procedure TcxGridChartViewData.DataLevelsChanged;
begin
  UpdateDataGroupIndex;
  UpdateSumsOfValues;
end;

function TcxGridChartViewData.IsDataGrouped: Boolean;
begin
  Result := DataController.Groups.GroupingItemCount <> 0;
end;

function TcxGridChartViewData.IsSummaryLevel: Boolean;
begin
  Result := ChildLevel < DataController.Groups.GroupingItemCount;
end;

procedure TcxGridChartViewData.CalculateSumsOfValues;
var
  I: Integer;
begin
  SetLength(FSumOfValues, GridView.SeriesCount);
  for I := 0 to GridView.SeriesCount - 1 do
    FSumOfValues[I] := CalculateSumOfValues(I);
end;

procedure TcxGridChartViewData.SeriesPosChanged(ASeries: TcxGridChartSeries);
begin
  UpdateSumsOfValues;
end;

procedure TcxGridChartViewData.Update(AInfo: TcxUpdateControlInfo);
begin
  if (AInfo is TcxDataChangedInfo) or (AInfo is TcxLayoutChangedInfo) then
  begin
    DataLevelsChanged;
    GridView.UpdateDataGroupActiveValues;
  end;
end;

procedure TcxGridChartViewData.UpdateDataGroupIndex;
begin
  FDataGroupIndex := -2;
end;

procedure TcxGridChartViewData.UpdateSumsOfValues;
begin
  FSumOfValues := nil;
end;

procedure TcxGridChartViewData.GetVisibleGroupValues(ADataGroup: TcxGridChartDataGroup;
  AStrings: TStrings; out AValues: TVariantArray);
var
  ADataGroupIndex, I: Integer;
  AValue: Variant;
begin
  ADataGroupIndex := GetDataGroupIndexByLevel(ADataGroup.VisibleIndex);
  I := DataController.Groups.ChildCount[ADataGroupIndex];
  SetLength(AValues, I);
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for I := 0 to I - 1 do
    begin
      AValue := DataController.Groups.GroupValues[
        DataController.Groups.ChildDataGroupIndex[ADataGroupIndex, I]];
      AStrings.Add(ADataGroup.GetValueDisplayText(AValue));
      AValues[I] := AValue;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

{ TcxCustomGridChartPartViewInfo }

function TcxCustomGridChartPartViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxCustomGridChartPartViewInfo.GetGridViewInfo: TcxGridChartViewInfo;
begin
  Result := TcxGridChartViewInfo(inherited GridViewInfo);
end;

function TcxCustomGridChartPartViewInfo.GetHeight: Integer;
begin
  if inherited GetHeight = 0 then
    Height := CalculateHeight;
  Result := inherited GetHeight;
end;

function TcxCustomGridChartPartViewInfo.GetVisible: Boolean;
begin
  Result := Position <> cppNone;
end;

function TcxCustomGridChartPartViewInfo.GetWidth: Integer;
begin
  if inherited GetWidth = 0 then
    Width := CalculateWidth;
  Result := inherited GetWidth;
end;

{ TcxCustomGridChartTitleViewInfo }

constructor TcxCustomGridChartTitleViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo;
  ATitle: TcxCustomGridChartTitle);
begin
  inherited Create(AGridViewInfo);
  FTitle := ATitle;
end;

function TcxCustomGridChartTitleViewInfo.CalculateHeight: Integer;
begin
  if Title.GetOrientation = cpoVertical then
    Result := TextWidth
  else
    Result := TextHeight;
end;

function TcxCustomGridChartTitleViewInfo.CalculateWidth: Integer;
begin
  Result := TextWidth;
  if Title.GetOrientation = cpoVertical then
  begin
    Width := Result;
    Result := TextHeight;
  end;
end;

function TcxCustomGridChartTitleViewInfo.GetAlignment: TcxGridChartPartAlignment;
begin
  Result := FTitle.GetAlignment;
end;

function TcxCustomGridChartTitleViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TcxCustomGridChartTitleViewInfo.GetDrawTextRotationAngle: TcxRotationAngle;
const
  Angle: array[Boolean] of  TcxRotationAngle = (ra0, raPlus90);
begin
  Result := Angle[Title.GetOrientation = cpoVertical];
end;

function TcxCustomGridChartTitleViewInfo.GetMultiLine: Boolean;
begin
  Result := True;
end;

function TcxCustomGridChartTitleViewInfo.GetOrientation: TcxGridChartPartOrientation;
begin
  Result := cpoHorizontal;
end;

function TcxCustomGridChartTitleViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartTitlePainter;
end;

function TcxCustomGridChartTitleViewInfo.GetPosition: TcxGridChartPartPosition;
begin
  Result := FTitle.GetPosition;
end;

function TcxCustomGridChartTitleViewInfo.GetText: string;
begin
  Result := FTitle.GetText;
end;

function TcxCustomGridChartTitleViewInfo.GetTextAreaBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxCustomGridChartTitleViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

{ TcxGridChartLegendItemViewInfo }

constructor TcxGridChartLegendItemViewInfo.Create(AContainer: TcxGridChartLegendViewInfo;
  AIndex: Integer);
begin
  inherited Create(AContainer.GridViewInfo);
  FContainer := AContainer;
  FIndex := AIndex;
end;

function TcxGridChartLegendItemViewInfo.GetDiagram: TcxGridChartDiagram;
begin
  Result := FContainer.Diagram;
end;

function TcxGridChartLegendItemViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartLegendItemViewInfo.GetLegendKeyHeight: Integer;
begin
  Result := FContainer.ItemLegendKeyHeight;
end;

function TcxGridChartLegendItemViewInfo.GetLegendKeyWidth: Integer;
begin
  Result := FContainer.ItemLegendKeyWidth;
end;

function TcxGridChartLegendItemViewInfo.GetSeries: TcxGridChartSeries;
begin
  if Container.GetItemSeriesIndex(Index) = -1 then
    Result := nil
  else
    Result := GridView.VisibleSeries[Container.GetItemSeriesIndex(Index)];
end;

function TcxGridChartLegendItemViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  InflateRect(Result, -LegendKeyOffset div 2, 0);
end;

function TcxGridChartLegendItemViewInfo.CalculateHeight: Integer;
begin
  Result := TextHeight;
end;

function TcxGridChartLegendItemViewInfo.CalculateWidth: Integer;
begin
  Result := TextWidth;
  Inc(Result, LegendKeyOffset div 2 * 2 + LegendKeyWidth + LegendKeyOffset);
end;

function TcxGridChartLegendItemViewInfo.CalculateLegendKeyHeight: Integer;
begin
  Result := CalculateLegendKeySize;
end;

function TcxGridChartLegendItemViewInfo.CalculateLegendKeySize: Integer;
begin
  Result := 2 * LegendKeyOffset;
end;

function TcxGridChartLegendItemViewInfo.CalculateLegendKeyWidth: Integer;
begin
  Result := CalculateLegendKeySize;
end;

function TcxGridChartLegendItemViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
  begin
    Diagram.DoCustomDrawLegendItem(ACanvas, Self, Result);
    if not Result then
      GridView.DoCustomDrawLegendItem(ACanvas, Self, Result);
  end;
end;

procedure TcxGridChartLegendItemViewInfo.DoCalculateParams;
begin
  inherited;
  Container.GetItemLegendKeyParams(Index, LegendKeyParams);
end;

function TcxGridChartLegendItemViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, DesignSelectionWidth, DesignSelectionWidth);
end;

function TcxGridChartLegendItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartLegendItemHitTest;
end;

function TcxGridChartLegendItemViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := (Container.Kind = lkSeries) and GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(Series);
end;

function TcxGridChartLegendItemViewInfo.GetLegendKeyBorders: TcxBorders;
begin
  if Diagram.Legend.GetKeyBorder = lbSingle then
    Result := cxBordersAll
  else
    Result := [];
end;

function TcxGridChartLegendItemViewInfo.GetLegendKeyBounds: TRect;
begin
  CalculateParams;
  Result := ContentBounds;
  with Result do
  begin
    Right := Left + LegendKeyWidth;
    Top := MulDiv(Top + Bottom - LegendKeyHeight, 1, 2);
    Bottom := Top + LegendKeyHeight;
  end;
end;

function TcxGridChartLegendItemViewInfo.GetLegendKeyOffset: Integer;
begin
  CalculateParams;
  Result := MulDiv(Canvas.FontHeight(Params.Font), 3, 8);
end;

function TcxGridChartLegendItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartLegendItemPainter;
end;

function TcxGridChartLegendItemViewInfo.GetRealLegendKeyBounds: TRect;
begin
  Result := GetLegendKeyBounds;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ContentBounds);
end;

function TcxGridChartLegendItemViewInfo.GetText: string;
begin
  Result := FContainer.GetItemCaption(FIndex);
end;

function TcxGridChartLegendItemViewInfo.GetTextAreaBounds: TRect;
begin
  Result := ContentBounds;
  Inc(Result.Left, LegendKeyWidth + LegendKeyOffset);
end;

procedure TcxGridChartLegendItemViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FContainer.GetViewParams(AParams);
end;

function TcxGridChartLegendItemViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

function TcxGridChartLegendItemViewInfo.HasCustomDraw: Boolean;
begin
  Result := Diagram.HasCustomDrawLegendItem or GridView.HasCustomDrawLegendItem;
end;

procedure TcxGridChartLegendItemViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  with AHitTest as TcxGridChartLegendItemHitTest do
  begin
    Index := Self.Index;
    Series := Self.Series;
    ValueIndex := Container.GetItemValueIndex(Self.Index);
  end;
end;

function TcxGridChartLegendItemViewInfo.GetAreaBoundsForPainting: TRect;
begin
  Result := DesignSelectionBounds;
end;

function TcxGridChartLegendItemViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (Container.Kind = lkSeries) and
    (AButton = mbLeft) and not (ssDouble in AShift) and GridView.IsDesigning then
  begin
    GridView.Controller.DesignController.SelectObject(Series, not (ssShift in AShift));
    Result := True;
  end;
end;

{ TcxGridChartLegendViewInfo }

constructor TcxGridChartLegendViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo;
  ADiagram: TcxGridChartDiagram);
var
  I: Integer;
begin
  inherited Create(AGridViewInfo);
  FDiagram := ADiagram;
  FItems := TList.Create;
  for I := 0 to GetItemCount - 1 do
    AddItem;
end;

destructor TcxGridChartLegendViewInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Free;
  inherited;
end;

function TcxGridChartLegendViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridChartLegendViewInfo.GetItem(Index: Integer): TcxGridChartLegendItemViewInfo;
begin
  Result := TcxGridChartLegendItemViewInfo(FItems[Index]);
end;

function TcxGridChartLegendViewInfo.GetItemLegendKeyWidth: Integer;
begin
  if FItemLegendKeyWidth = 0 then
    FItemLegendKeyWidth := CalculateItemLegendKeyWidth;
  Result := FItemLegendKeyWidth;
end;

function TcxGridChartLegendViewInfo.GetItemLegendKeyHeight: Integer;
begin
  if FItemLegendKeyHeight = 0 then
    FItemLegendKeyHeight := CalculateItemLegendKeyHeight;
  Result := FItemLegendKeyHeight;
end;

function TcxGridChartLegendViewInfo.GetItemOffset: Integer;
begin
  if FItemOffset = 0 then
    FItemOffset := CalculateItemOffset;
  Result := FItemOffset;
end;

function TcxGridChartLegendViewInfo.CalculateHeight: Integer;
var
  I: Integer;
begin
  CalculateParams;
  if Orientation = cpoHorizontal then
  begin
    Result := 0;
    for I := 0 to Count - 1 do
      Result := Max(Result, Items[I].CalculateHeight);
  end
  else
  begin
    Result := ItemOffset * (Count - 1);
    for I := 0 to Count - 1 do
      Inc(Result, Items[I].CalculateHeight);
  end;
  Inc(Result, BorderSize[bTop] + BorderSize[bBottom] + 2 * ItemOffset);
end;

function TcxGridChartLegendViewInfo.CalculateWidth: Integer;
var
  I: Integer;
begin
  CalculateParams;
  if Orientation = cpoHorizontal then
  begin
    Result := ItemOffset * (Count - 1);
    for I := 0 to Count - 1 do
      Inc(Result, Items[I].CalculateWidth);
  end
  else
  begin
    Result := 0;
    for I := 0 to Count - 1 do
      Result := Max(Result, Items[I].CalculateWidth);
  end;
  Inc(Result, BorderSize[bLeft] + BorderSize[bRight] + 2 * ItemOffset);
end;

function TcxGridChartLegendViewInfo.CalculateItemLegendKeyWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].CalculateLegendKeyWidth);
end;

function TcxGridChartLegendViewInfo.CalculateItemLegendKeyHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].CalculateLegendKeyHeight);
end;

function TcxGridChartLegendViewInfo.CalculateItemOffset: Integer;
begin
  CalculateParams;
  Result := MulDiv(Canvas.FontHeight(Params.Font), 1, 4);
end;

function TcxGridChartLegendViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
  begin
    Diagram.DoCustomDrawLegend(ACanvas, Self, Result);
    if not Result then
      GridView.DoCustomDrawLegend(ACanvas, Self, Result);
  end;
end;

function TcxGridChartLegendViewInfo.GetAlignment: TcxGridChartPartAlignment;
begin
  Result := Diagram.Legend.GetAlignment;
end;

function TcxGridChartLegendViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := Params.TextColor;
end;

function TcxGridChartLegendViewInfo.GetBorders: TcxBorders;
begin
  if Diagram.Legend.GetBorder = lbSingle then
    Result := cxBordersAll
  else
    Result := [];
end;

function TcxGridChartLegendViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridChartLegendViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartLegendHitTest;
end;

function TcxGridChartLegendViewInfo.GetOrientation: TcxGridChartPartOrientation;
begin
  Result := Diagram.Legend.GetOrientation;
end;

function TcxGridChartLegendViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartLegendPainter;
end;

function TcxGridChartLegendViewInfo.GetPosition: TcxGridChartPartPosition;
begin
  Result := Diagram.Legend.GetPosition;
end;

procedure TcxGridChartLegendViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsLegend, nil, nil, AParams);
end;

function TcxGridChartLegendViewInfo.GetVisible: Boolean;
begin
  Result := inherited GetVisible and (Count <> 0);
end;

function TcxGridChartLegendViewInfo.HasCustomDraw: Boolean;
begin
  Result := Diagram.HasCustomDrawLegend or GridView.HasCustomDrawLegend;
end;

function TcxGridChartLegendViewInfo.AddItem: TcxGridChartLegendItemViewInfo;
begin
  Result := GetItemClass.Create(Self, Count);
  FItems.Add(Result);
end;

procedure TcxGridChartLegendViewInfo.CalculateItems;
var
  AItemLeftOffset, AItemTopOffset, I: Integer;
begin
  AItemLeftOffset := ContentBounds.Left + ItemOffset;
  AItemTopOffset := ContentBounds.Top + ItemOffset;
  for I := 0 to Count - 1 do
  begin
    Items[I].Calculate(AItemLeftOffset, AItemTopOffset);
    if Orientation = cpoHorizontal then
      AItemLeftOffset := Items[I].Bounds.Right + ItemOffset
    else
      AItemTopOffset := Items[I].Bounds.Bottom + ItemOffset;
  end;
end;

function TcxGridChartLegendViewInfo.GetItemCaption(AIndex: Integer): string;
begin
  if Kind = lkSeries then
    Result := GridView.VisibleSeries[GetItemSeriesIndex(AIndex)].GetDisplayText
  else
  begin
    Result := GridView.Categories.VisibleDisplayTexts[GetItemValueIndex(AIndex)];
    if Result = '' then
      Result := '<' + IntToStr(1 + AIndex) + '>';
  end;
end;

function TcxGridChartLegendViewInfo.GetItemClass: TcxGridChartLegendItemViewInfoClass;
begin
  Result := TcxGridChartLegendItemViewInfo;
end;

function TcxGridChartLegendViewInfo.GetItemCount: Integer;
begin
  if Kind = lkSeries then
    Result := GridView.VisibleSeriesCount
  else
    Result := GridView.Categories.VisibleValueCount;
end;

function TcxGridChartLegendViewInfo.GetItemSeriesIndex(AIndex: Integer): Integer;
begin
  if Kind = lkSeries then
    Result := GetItemObjectIndex(AIndex)
  else
    if GridView.VisibleSeriesCount = 1 then
      Result := 0
    else
      Result := -1;
end;

function TcxGridChartLegendViewInfo.GetItemValueIndex(AIndex: Integer): Integer;
begin
  if Kind = lkSeries then
    Result := -1
  else
    Result := GetItemObjectIndex(AIndex);
end;

{function TcxGridChartLegendViewInfo.ItemLegendKeyBorderIsValueBorder: Boolean;
begin
  Result := False;
end;}

procedure TcxGridChartLegendViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  CalculateItems;
end;

procedure TcxGridChartLegendViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridChartLegendViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if Result <> nil then
    for I := 0 to Count - 1 do
    begin
      AHitTest := Items[I].GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Break;
      end;
    end;
end;

procedure TcxGridChartLegendViewInfo.GetItemLegendKeyParams(AIndex: Integer;
  out AParams: TcxViewParams);
begin
  Diagram.Styles.GetValueParams(GetItemSeriesIndex(AIndex), GetItemValueIndex(AIndex), AParams);
{  if not ItemLegendKeyBorderIsValueBorder then
    AParams.TextColor := Params.TextColor;}
end;

{ TcxGridChartDiagramValueCaptionViewInfo }

constructor TcxGridChartDiagramValueCaptionViewInfo.Create(AValueViewInfo: TcxGridChartDiagramValueViewInfo);
begin
  inherited Create(AValueViewInfo.GridViewInfo);
  FValueViewInfo := AValueViewInfo;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := Params.TextColor;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetBorders: TcxBorders;
begin
  if HasBackground then
    Result := cxBordersAll
  else
    Result := [];
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartColumnValueCaptionPainter;
end;

function TcxGridChartDiagramValueCaptionViewInfo.CalculateHeight: Integer;
begin
  Result := TextHeight;
  if HasVisualCompensation then
    Inc(Result, 2 * ScaleFactor.Apply(cxTextOffset));
end;

function TcxGridChartDiagramValueCaptionViewInfo.CalculateWidth: Integer;
begin
  Result := TextWidthWithOffset;
  if HasVisualCompensation then
    Inc(Result, 2 * ScaleFactor.Apply(cxTextOffset));
end;

function TcxGridChartDiagramValueCaptionViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  with ValueViewInfo do
    Diagram.OnCustomDrawValueCaption(Diagram, ACanvas, Self, Result);
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetText: string;
begin
  Result := FValueViewInfo.GetCaptionText;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetTextAreaBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetTextAttributes(AForPainting: Boolean): Integer;
begin
  Result := inherited GetTextAttributes(AForPainting) or cxDontClip;
end;

procedure TcxGridChartDiagramValueCaptionViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FValueViewInfo.GetCaptionViewParams(AParams);
end;

function TcxGridChartDiagramValueCaptionViewInfo.GetVisible: Boolean;
begin
  Result := FValueViewInfo.HasCaption;
end;

function TcxGridChartDiagramValueCaptionViewInfo.HasBackground: Boolean;
begin
  Result := not TcxGridChartView(GridView).OptionsView.TransparentCaptions;
end;

function TcxGridChartDiagramValueCaptionViewInfo.HasCustomDraw: Boolean;
begin
  Result := ValueViewInfo.Diagram.HasCustomDrawValueCaption;
end;

function TcxGridChartDiagramValueCaptionViewInfo.HasVisualCompensation: Boolean;
begin
  Result := FValueViewInfo.HasCaptionVisualCompensation;
end;

{ TcxGridChartDiagramValueViewInfo }

constructor TcxGridChartDiagramValueViewInfo.Create(ADiagramViewInfo: TcxGridChartDiagramViewInfo;
  ASeriesIndex, AVisibleValueIndex: Integer);
begin
  inherited Create(ADiagramViewInfo.GridViewInfo);
  FDiagramViewInfo := ADiagramViewInfo;
  FSeriesIndex := ASeriesIndex;
  FVisibleValueIndex := AVisibleValueIndex;
  FCaptionViewInfo := GetCaptionViewInfoClass.Create(Self);
end;

destructor TcxGridChartDiagramValueViewInfo.Destroy;
begin
  FCaptionViewInfo.Free;
  inherited;
end;

function TcxGridChartDiagramValueViewInfo.GetController: TcxGridChartController;
begin
  Result := TcxGridChartController(inherited Controller);
end;

function TcxGridChartDiagramValueViewInfo.GetDiagram: TcxGridChartDiagram;
begin
  Result := FDiagramViewInfo.Diagram;
end;

function TcxGridChartDiagramValueViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartDiagramValueViewInfo.GetSeries: TcxGridChartSeries;
begin
  Result := GridView.VisibleSeries[FSeriesIndex];
end;

function TcxGridChartDiagramValueViewInfo.GetValueIndex: Integer;
begin
  Result := DiagramViewInfo.GetValueIndex(VisibleValueIndex);
end;

function TcxGridChartDiagramValueViewInfo.CanShowHint: Boolean;
begin
  Result := GridView.OptionsBehavior.ValueHints;
end;

function TcxGridChartDiagramValueViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
  begin
    Series.DoCustomDrawValue(ACanvas, Self, Result);
    if not Result then
      Diagram.DoCustomDrawValue(ACanvas, Self, Result);
  end;
end;

function TcxGridChartDiagramValueViewInfo.GetCaptionViewInfoClass: TcxGridChartDiagramValueCaptionViewInfoClass;
begin
  Result := TcxGridChartDiagramValueCaptionViewInfo;
end;

function TcxGridChartDiagramValueViewInfo.GetCaptionText: string;
begin
  Result := DiagramViewInfo.GetValueCaption(SeriesIndex, VisibleValueIndex);
end;

procedure TcxGridChartDiagramValueViewInfo.GetCaptionViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsValueCaptions, Series, nil, AParams);
end;

function TcxGridChartDiagramValueViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := Bounds;
end;

function TcxGridChartDiagramValueViewInfo.GetHintText: string;
begin
  Result := Controller.GetValueHintText(Series, ValueIndex);
end;

function TcxGridChartDiagramValueViewInfo.GetHintTextRect(const AMousePos: TPoint): TRect;
begin
  Result.TopLeft := AMousePos;
  Inc(Result.Top, ScaleFactor.Apply(ValueHintOffset));
  Result.BottomRight := Result.TopLeft;
end;

function TcxGridChartDiagramValueViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartValueHitTest;
end;

function TcxGridChartDiagramValueViewInfo.GetHotTrack: Boolean;
begin
  Result := GridView.OptionsBehavior.GetValueHotTrack(ValueIndex);
end;

function TcxGridChartDiagramValueViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    Controller.DesignController.IsObjectSelected(Series);
end;

procedure TcxGridChartDiagramValueViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetValueParams(SeriesIndex, ValueIndex, AParams);
  if State <> gcsNone then
    AParams.Color := GetHotColor(AParams.Color);
end;

function TcxGridChartDiagramValueViewInfo.HasCaption: Boolean;
begin
  Result := False;
end;

function TcxGridChartDiagramValueViewInfo.HasCaptionVisualCompensation: Boolean;
begin
  Result := False;
end;

function TcxGridChartDiagramValueViewInfo.HasCustomDraw: Boolean;
begin
  Result := Series.HasCustomDrawValue or Diagram.HasCustomDrawValue;
end;

function TcxGridChartDiagramValueViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := HasPoint(P);
end;

procedure TcxGridChartDiagramValueViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridChartValueHitTest(AHitTest).CanDrillDown := Controller.IsDataDrillDownPossible(ValueIndex);
  TcxGridChartValueHitTest(AHitTest).Series := Series;
  TcxGridChartValueHitTest(AHitTest).ValueIndex := ValueIndex;
end;

function TcxGridChartDiagramValueViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := True;
end;

function TcxGridChartDiagramValueViewInfo.IsHintForText: Boolean;
begin
  Result := False;
end;

function TcxGridChartDiagramValueViewInfo.IsHintMultiLine: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartDiagramValueViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  if HasCaption then
    CaptionViewInfo.Offset(DX, DY);
end;

procedure TcxGridChartDiagramValueViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  CalculateParamsNeeded;
  inherited StateChanged(APrevState);
end;

procedure TcxGridChartDiagramValueViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  if HasCaption then
    CaptionViewInfo.Calculate(CalculateCaptionBounds);
end;

function TcxGridChartDiagramValueViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if not (ssDouble in AShift) then
    if GridView.IsDesigning then
      if AButton = mbLeft then
      begin
        Controller.DesignController.SelectObject(Series, not (ssShift in AShift));
        Result := True;
      end
      else
    else
    begin
      if AButton = mbLeft then
        Result := Series.DoValueClick(ValueIndex);
      if (AHitTest as TcxGridChartValueHitTest).CanDrillDown then
        case AButton of
          mbLeft:
            if not Result then
              Result := Controller.DoDataDrillDown(ValueIndex);
          mbRight:
            if GridView.OptionsCustomize.DataDrillUpMethod = ddumValueMouseRightButtonClick then
              Result := Controller.DoDataDrillUp;
        end;
    end;
end;

{ TcxGridChartDiagramViewInfo }

constructor TcxGridChartDiagramViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo;
  ADiagram: TcxGridChartDiagram);
begin
  inherited Create(AGridViewInfo);
  FDiagram := ADiagram;
  FValueViewInfos := TList.Create;
end;

destructor TcxGridChartDiagramViewInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to ValueViewInfoCount - 1 do
    ValueViewInfos[I].Free;
  FValueViewInfos.Free;
  inherited;
end;

function TcxGridChartDiagramViewInfo.GetCategory(AIndex: Integer): string;
begin
  Result := GridView.Categories.VisibleDisplayTexts[AIndex];
end;

function TcxGridChartDiagramViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartDiagramViewInfo.GetSeries(AIndex: Integer): TcxGridChartSeries;
begin
  Result := GridView.VisibleSeries[AIndex];
end;

function TcxGridChartDiagramViewInfo.GetSeriesCount: Integer;
begin
  Result := GridView.VisibleSeriesCount;
end;

function TcxGridChartDiagramViewInfo.GetValueCount: Integer;
begin
  Result := ViewData.VisibleCategoryCount;
end;

function TcxGridChartDiagramViewInfo.GetValueViewInfoValue(Index: Integer): TcxGridChartDiagramValueViewInfo;
begin
  Result := TcxGridChartDiagramValueViewInfo(FValueViewInfos[Index]);
end;

function TcxGridChartDiagramViewInfo.GetValueViewInfoCount: Integer;
begin
  Result := FValueViewInfos.Count;
end;

function TcxGridChartDiagramViewInfo.GetViewData: TcxGridChartViewData;
begin
  Result := GridView.ViewData;
end;

procedure TcxGridChartDiagramViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetBackgroundParams(AParams);
end;

procedure TcxGridChartDiagramViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  (AHitTest as TcxCustomGridChartDiagramHitTest).Diagram := Diagram;
end;

function TcxGridChartDiagramViewInfo.CreateValueViewInfo(ASeriesIndex, AVisibleValueIndex: Integer): TcxGridChartDiagramValueViewInfo;
begin
  Result := GetValueViewInfoClass.Create(Self, ASeriesIndex, AVisibleValueIndex);
  FValueViewInfos.Add(Result);
end;

function TcxGridChartDiagramViewInfo.GetValue(ASeriesIndex, AIndex: Integer): Variant;
begin
  Result := GridView.VisibleSeries[ASeriesIndex].VisibleValues[AIndex];
end;

function TcxGridChartDiagramViewInfo.GetValueCaption(ASeriesIndex, AVisibleValueIndex: Integer): string;
begin
  Result := Diagram.GetValueCaption(GridView.VisibleSeries[ASeriesIndex],
    GetValueIndex(AVisibleValueIndex));
end;

function TcxGridChartDiagramViewInfo.GetValueIndex(AVisibleValueIndex: Integer): Integer;
begin
  Result := GridView.Controller.FirstVisibleCategoryIndex + AVisibleValueIndex;
end;

class function TcxGridChartDiagramViewInfo.GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass;
begin
  Result := nil;
end;

function TcxGridChartDiagramViewInfo.GetVisibleCategory(AIndex: Integer): string;
begin
  Result := Categories[GetValueIndex(AIndex)];
end;

function TcxGridChartDiagramViewInfo.GetVisibleValue(ASeriesIndex, AIndex: Integer): Variant;
begin
  Result := Values[ASeriesIndex, GetValueIndex(AIndex)];
end;

function TcxGridChartDiagramViewInfo.GetVisibleValueCount: Integer;
begin
  Result := GridView.Controller.GetVisibleCategoryCount(True);
end;

function TcxGridChartDiagramViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ValueViewInfoCount - 1 do
  begin
    Result := ValueViewInfos[I].GetHitTest(P);
    if Result <> nil then Break;
  end;
end;

function TcxGridChartDiagramViewInfo.GetValueViewInfo(ASeriesIndex, AVisibleIndex: Integer): TcxGridChartDiagramValueViewInfo;
begin
  Result := ValueViewInfos[GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex)];
end;

{ TcxGridChartHistogramLegendViewInfo }

function TcxGridChartHistogramLegendViewInfo.GetDiagram: TcxGridChartHistogram;
begin
  Result := TcxGridChartHistogram(inherited Diagram);
end;

function TcxGridChartHistogramLegendViewInfo.GetItemObjectIndex(AIndex: Integer): Integer;
begin
  if ItemsInReverseOrder then
    Result := Count - 1 - AIndex
  else
    Result := AIndex;
end;

function TcxGridChartHistogramLegendViewInfo.GetItemsInReverseOrder: Boolean;
begin
  Result := False;
end;

function TcxGridChartHistogramLegendViewInfo.GetKind: TcxGridChartLegendKind;
begin
  if Diagram.Values.GetVaryColorsByCategory then
    Result := lkCategories
  else
    Result := lkSeries;
end;

{ TcxGridChartHistogramValueViewInfo }

function TcxGridChartHistogramValueViewInfo.GetCategoryDirection: TcxDirection;
begin
  Result := DiagramViewInfo.GetCategoryDirection;
end;

function TcxGridChartHistogramValueViewInfo.GetDiagramViewInfo: TcxGridChartHistogramViewInfo;
begin
  Result := TcxGridChartHistogramViewInfo(inherited DiagramViewInfo);
end;

function TcxGridChartHistogramValueViewInfo.GetValueDirection: TcxDirection;
begin
  Result := DiagramViewInfo.GetValueDirection(SeriesIndex, VisibleValueIndex);
end;

{ TcxGridChartHistogramAxisTitleViewInfo }

constructor TcxGridChartHistogramAxisTitleViewInfo.Create(AContainer: TcxGridChartHistogramTickMarkLabelsViewInfo;
  ATitle: TcxCustomGridChartTitle);
begin
  inherited Create(AContainer.GridViewInfo, ATitle);
  FContainer := AContainer;
end;

function TcxGridChartHistogramAxisTitleViewInfo.GetTitle: TcxGridChartHistogramAxisTitle;
begin
  Result := TcxGridChartHistogramAxisTitle(inherited Title);
end;

procedure TcxGridChartHistogramAxisTitleViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FContainer.GetTitleViewParams(AParams);
end;

procedure TcxGridChartHistogramAxisTitleViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FContainer.DiagramViewInfo.InitHitTest(AHitTest);
  inherited;
end;

{ TcxGridChartHistogramTickMarkLabelsViewInfo }

constructor TcxGridChartHistogramTickMarkLabelsViewInfo.Create(ADiagramViewInfo: TcxGridChartHistogramViewInfo);
begin
  inherited Create(ADiagramViewInfo.GridViewInfo);
  FDiagramViewInfo := ADiagramViewInfo;
  FTitleViewInfo := GetTitleViewInfoClass.Create(Self, Axis.Title);
end;

destructor TcxGridChartHistogramTickMarkLabelsViewInfo.Destroy;
begin
  FreeAndNil(FTitleViewInfo);
  inherited;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaptionBounds(Index: Integer): TRect;
begin
  Result := FCaptionBounds[Index];
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaptionHeight: Integer;
begin
  if FCaptionHeight = 0 then
  begin
    CalculateParams;
    Canvas.Font := Params.Font;
    FCaptionHeight := cxTextHeight(Canvas.Handle);
  end;
  Result := FCaptionHeight;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaptionWidth(Index: Integer): Integer;
begin
  if Index >= Length(FCaptionWidths) then
    SetLength(FCaptionWidths, Index + 1);
  if FCaptionWidths[Index] = 0 then
  begin
    CalculateParams;
    Canvas.Font := Params.Font;
    FCaptionWidths[Index] := Canvas.TextWidth(Captions[Index]);
  end;
  Result := FCaptionWidths[Index];
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetDiagram: TcxGridChartHistogram;
begin
  Result := FDiagramViewInfo.Diagram;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetIsVertical: Boolean;
begin
  Result := Position in [cppLeft, cppRight];
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.SetCaptionBounds(Index: Integer;
  const Value: TRect);
begin
  FCaptionBounds[Index] := Value;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartHistogramTickMarkLabelsPainter;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass;
begin
  Result := TcxGridChartHistogramAxisTitleViewInfo;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaption(Index: Integer): string;
begin
  Result := Values[Index];
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateCaptionBounds(AIndex: Integer): TRect;
var
  ACaptionWidth, ADirection: Integer;
begin
  Result := ContentBounds;
  if IsVertical then
  begin
    Result.Top := GetCaptionPosition(AIndex) - CaptionHeight div 2;
    Result.Bottom := Result.Top + CaptionHeight;
  end
  else
  begin
    ACaptionWidth := CaptionWidths[AIndex];
    Result.Left := GetCaptionPosition(AIndex) - ACaptionWidth div 2;
    Result.Right := Result.Left + ACaptionWidth;
    if IsTwoRowLayout then
    begin
      if IsReverseOrder then
        ADirection := -1
      else
        ADirection := 1;
      if AIndex > 0 then
        Result.Left := Max(Result.Left, GetCaptionPosition(AIndex - ADirection) + ScaleFactor.Apply(cxTextOffset));
      if AIndex < Count - 1 then
        Result.Right := Min(Result.Right, GetCaptionPosition(AIndex + ADirection) - ScaleFactor.Apply(cxTextOffset));
      if Odd(AIndex) then
        Result.Top := Result.Bottom - CaptionHeight;
    end;
  end;
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateCaptionsBounds;
var
  I: Integer;
begin
  SetLength(FCaptionBounds, Count);
  for I := 0 to Count - 1 do
    CaptionBounds[I] := CalculateCaptionBounds(I);
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateHeight(
  AWidth, ALeftReduction, ARightReduction: Integer): Integer;
begin
  IsTwoRowLayout := CalculateIsTwoRowLayout(AWidth, ALeftReduction, ARightReduction);
  if IsVertical then
    Result := 0
  else
  begin
    if CaptionsVisible then
    begin
      Result := CaptionHeight;
      if IsTwoRowLayout then
        Inc(Result, 2 * ScaleFactor.Apply(cxTextOffset) + CaptionHeight);
    end
    else
      Result := 0;
    if TitleViewInfo.Visible then
    begin
      if CaptionsVisible then
        Inc(Result, ScaleFactor.Apply(ChartPartOffset));
      Inc(Result, TitleViewInfo.Height);
    end;
  end;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateWidth: Integer;
var
  I: Integer;
begin
  if IsVertical then
  begin
    Result := 0;
    if CaptionsVisible then
      for I := 0 to Count - 1 do
        Result := Max(Result, CaptionWidths[I]);
    if TitleViewInfo.Visible then
    begin
      if CaptionsVisible then
        Inc(Result, ScaleFactor.Apply(ChartPartOffset));
      Inc(Result, TitleViewInfo.Width);
    end;
  end
  else
    Result := 0;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateIsTwoRowLayout(
  AWidth, ALeftReduction, ARightReduction: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if IsVertical or (Count <= 1) then Exit;
  AWidth := GetCaptionSpace(AWidth, ALeftReduction, ARightReduction);
  for I := 0 to Count - 1 do
  begin
    Result := CaptionWidths[I] > AWidth;
    if Result then Exit;
  end;
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.CalculateTitleBounds(out ATitleBounds, AContentBounds: TRect);
begin
  AContentBounds := CalculateContentBounds;
  if TitleViewInfo.Visible then
  begin
    PositionRect(ATitleBounds, AContentBounds, TitleViewInfo.Width,
      TitleViewInfo.Height, ScaleFactor.Apply(ChartPartOffset), Position);

    with DiagramViewInfo.PlotBounds do
      if IsVertical then
      begin
        ATitleBounds.Top := Top;
        ATitleBounds.Bottom := Bottom;
      end
      else
      begin
        ATitleBounds.Left := Left;
        ATitleBounds.Right := Right;
      end;

    AlignRect(ATitleBounds, TitleViewInfo.Width, TitleViewInfo.Height, not IsVertical, TitleViewInfo.Alignment);
  end
  else
    SetRectEmpty(ATitleBounds);
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetAlignmentHorz: TAlignment;
begin
  if IsVertical then
    if Position = cppLeft then
      Result := taRightJustify
    else
      Result := taLeftJustify
  else
    Result := taCenter;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaptionsVisible: Boolean;
begin
  Result := Axis.TickMarkLabels;
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.GetEdgeCaptionsReductions(
  var ALeftReduction, ARightReduction: Integer);
begin
  ALeftReduction := Max(ALeftReduction, CaptionWidths[0] div 2);
  ARightReduction := Max(ARightReduction, CaptionWidths[Count - 1] div 2);
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetTextAttributes(AForPainting: Boolean): Integer;
begin
  Result := inherited GetTextAttributes(AForPainting) or cxDontClip or cxShowEndEllipsis;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetVisible: Boolean;
begin
 Result := CaptionsVisible or TitleViewInfo.Visible;
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
var
  ATitleBounds: TRect;
begin
  inherited;
  CalculateTitleBounds(ATitleBounds, FContentBounds);
  CalculateCaptionsBounds;
  if TitleViewInfo.Visible then
    TitleViewInfo.Calculate(ATitleBounds);
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.FitCaptionsHorz(AWidth: Integer;
  var ALeftReduction, ARightReduction: Integer);
var
  ACaptionSpace, ALeftOffset, ARightOffset: Integer;
begin
  if not CaptionsVisible then Exit;
  if IsTwoRowLayout and not IsEdgeCaptions then
  begin
    ACaptionSpace := GetCaptionSpace(AWidth, ALeftReduction, ARightReduction);
    ALeftOffset := Max(0, (CaptionWidths[0] - ACaptionSpace) div 2 - ALeftReduction);
    ARightOffset := Max(0, (CaptionWidths[Count - 1] - ACaptionSpace) div 2 - ARightReduction);

    Inc(ALeftReduction, MulDiv((2 * ARightOffset + (4 * Count - 2) * ALeftOffset), 1, 4 * Count - 4));
    Inc(ARightReduction, MulDiv((2 * ALeftOffset + (4 * Count - 2) * ARightOffset), 1, 4 * Count - 4));
  end
  else
    if IsEdgeCaptions and (Count <> 0) then
      GetEdgeCaptionsReductions(ALeftReduction, ARightReduction);
end;

procedure TcxGridChartHistogramTickMarkLabelsViewInfo.FitCaptionsVert(
  var ATopReduction, ABottomReduction: Integer);
begin
  if not CaptionsVisible then Exit;
  if IsEdgeCaptions then
  begin
    ATopReduction := Max(ATopReduction, CaptionHeight div 2);
    ABottomReduction := Max(ABottomReduction, CaptionHeight div 2);
  end;
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetCaptionSpace(
  AWidth, ALeftReduction, ARightReduction: Integer): Integer;
var
  ACount: Integer;
begin
  ACount := Count;
  if IsEdgeCaptions then
  begin
    Dec(ACount);
    GetEdgeCaptionsReductions(ALeftReduction, ARightReduction);
  end;
  Dec(AWidth, ALeftReduction + ARightReduction);
  Result := AWidth div ACount - 4 * ScaleFactor.Apply(cxTextOffset);
end;

function TcxGridChartHistogramTickMarkLabelsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := FTitleViewInfo.GetHitTest(P);
end;

{ TcxGridChartHistogramCategoryAxisTitleViewInfo }

function TcxGridChartHistogramCategoryAxisTitleViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartHistogramCategoryAxisTitleHitTest;
end;

{ TcxGridChartHistogramCategoryTickMarkLabelsViewInfo }

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass;
begin
  Result := TcxGridChartHistogramCategoryAxisTitleViewInfo;
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetCount: Integer;
begin
  Result := DiagramViewInfo.VisibleValueCount;
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetIsReverseOrder: Boolean;
begin
  Result := DiagramViewInfo.IsCategoriesInReverseOrder;
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetValue(Index: Integer): Variant;
begin
  Result := DiagramViewInfo.VisibleCategories[Index];
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetAxis: TcxGridChartHistogramAxis;
begin
  Result := Diagram.AxisCategory;
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetCaptionPosition(AIndex: Integer): Integer;
begin
  Result := DiagramViewInfo.GetUnitOffset(DiagramViewInfo.GetCategoryTickMarkLabelOffsetUnits(AIndex));
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetIsEdgeCaptions: Boolean;
begin
  Result := DiagramViewInfo.IsEdgeCategoryTickMarkLabels;
end;

function TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetPosition: TcxGridChartAxisPosition;
begin
  Result := DiagramViewInfo.CategoryAxisPosition;
end;

procedure TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetTitleViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsCategoryAxisTitle, nil, nil, AParams);
end;

procedure TcxGridChartHistogramCategoryTickMarkLabelsViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsCategoryAxis, nil, nil, AParams);
end;

{ TcxGridChartHistogramValueAxisTitleViewInfo }

function TcxGridChartHistogramValueAxisTitleViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartHistogramValueAxisTitleHitTest;
end;

{ TcxGridChartHistogramValueTickMarkLabelsViewInfo }

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetAxisValue: TcxGridChartHistogramAxisValue;
begin
  Result := TcxGridChartHistogramAxisValue(inherited Axis);
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetTitleViewInfoClass: TcxGridChartHistogramAxisTitleViewInfoClass;
begin
  Result := TcxGridChartHistogramValueAxisTitleViewInfo;
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetCaption(Index: Integer): string;
begin
  Result := Axis.GetTickMarkLabel(Values[Index]);
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetCount: Integer;
begin
  Result := DiagramViewInfo.ValueMarkCount;
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetIsReverseOrder: Boolean;
begin
  Result := False;
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetValue(Index: Integer): Variant;
begin
  Result := DiagramViewInfo.ValueMarkValues[Index];
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetAxis: TcxGridChartHistogramAxis;
begin
  Result := Diagram.AxisValue;
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetCaptionPosition(AIndex: Integer): Integer;
begin
  Result := DiagramViewInfo.ValueMarkPositions[AIndex];
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetIsEdgeCaptions: Boolean;
begin
  Result := True;
end;

function TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetPosition: TcxGridChartAxisPosition;
begin
  Result := DiagramViewInfo.ValueAxisPosition;
end;

procedure TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetTitleViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsValueAxisTitle, nil, nil, AParams);
end;

procedure TcxGridChartHistogramValueTickMarkLabelsViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsValueAxis, nil, nil, AParams);
end;

{ TcxGridChartHistogramViewInfo }

constructor TcxGridChartHistogramViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo;
  ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FCategoryTickMarkLabelsViewInfo := GetCategoryTickMarkLabelsViewInfoClass.Create(Self);
  FValueTickMarkLabelsViewInfo := GetValueTickMarkLabelsViewInfoClass.Create(Self);
end;

destructor TcxGridChartHistogramViewInfo.Destroy;
begin
  FValueTickMarkLabelsViewInfo.Free;
  FCategoryTickMarkLabelsViewInfo.Free;
  inherited;
end;

function TcxGridChartHistogramViewInfo.GetCategoryAxisColor: TColor;
var
  AParams: TcxViewParams;
begin
  Diagram.Styles.GetViewParams(dsCategoryAxis, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxGridChartHistogramViewInfo.GetCategoryGridLineColor: TColor;
var
  AParams: TcxViewParams;
begin
  Diagram.Styles.GetViewParams(dsCategoryGridLines, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxGridChartHistogramViewInfo.GetCategoryMarkCount: Integer;
begin
  Result := Length(FCategoryMarks);
end;

function TcxGridChartHistogramViewInfo.GetCategoryMarkPosition(Index: Integer): Integer;
begin
  Result := FCategoryMarks[Index].Position;
end;

function TcxGridChartHistogramViewInfo.GetCategoryMarkValue(Index: Integer): Integer;
begin
  Result := FCategoryMarks[Index].Value;
end;

function TcxGridChartHistogramViewInfo.GetDiagram: TcxGridChartHistogram;
begin
  Result := TcxGridChartHistogram(inherited Diagram);
end;

function TcxGridChartHistogramViewInfo.GetHasCategoryAxis: Boolean;
begin
  Result := Diagram.AxisCategory.Visible and (VisibleValueCount <> 0);
end;

function TcxGridChartHistogramViewInfo.GetHasCategoryGridLines: Boolean;
begin
  Result := Diagram.AxisCategory.GridLines;
end;

function TcxGridChartHistogramViewInfo.GetHasValueAxis: Boolean;
begin
  Result := Diagram.AxisValue.Visible and (VisibleValueCount <> 0);
end;

function TcxGridChartHistogramViewInfo.GetHasValueGridLines: Boolean;
begin
  Result := Diagram.AxisValue.GridLines;
end;

function TcxGridChartHistogramViewInfo.GetIsCategoriesInReverseOrder: Boolean;
begin
  Result := Diagram.AxisCategory.CategoriesInReverseOrder;
end;

function TcxGridChartHistogramViewInfo.GetIsCategoryAxisHorz: Boolean;
begin
  Result := CategoryAxisPosition in [cppTop, cppBottom];
end;

function TcxGridChartHistogramViewInfo.GetIsValueAxisVert: Boolean;
begin
  Result := ValueAxisPosition in [cppLeft, cppRight];
end;

function TcxGridChartHistogramViewInfo.GetPlotBounds: TRect;
begin
  if not FIsPlotBoundsCalculated then
  begin
    FPlotBounds := CalculatePlotBounds;
    FIsPlotBoundsCalculated := True;
  end;
  Result := FPlotBounds;
end;

function TcxGridChartHistogramViewInfo.GetSumOfValues(AGroupIndex, AValueIndex: Integer): Variant;
begin
  if FSumOfValues = nil then
    SetLength(FSumOfValues, GridView.VisibleSeriesGroupCount, ValueCount);
  if VarIsEmpty(FSumOfValues[AGroupIndex, AValueIndex]) then
    FSumOfValues[AGroupIndex, AValueIndex] := CalculateSumOfValues(AGroupIndex, AValueIndex);
  Result := FSumOfValues[AGroupIndex, AValueIndex];
end;

function TcxGridChartHistogramViewInfo.GetValueAxisColor: TColor;
var
  AParams: TcxViewParams;
begin
  Diagram.Styles.GetViewParams(dsValueAxis, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxGridChartHistogramViewInfo.GetValueGridLineColor: TColor;
var
  AParams: TcxViewParams;
begin
  Diagram.Styles.GetViewParams(dsValueGridLines, nil, nil, AParams);
  Result := AParams.TextColor;
end;

function TcxGridChartHistogramViewInfo.GetValueMarkCount: Integer;
begin
  Result := Length(FValueMarks);
end;

function TcxGridChartHistogramViewInfo.GetValueMarkPosition(Index: Integer): Integer;
begin
  Result := FValueMarks[Index].Position;
end;

function TcxGridChartHistogramViewInfo.GetValueMarkValue(Index: Integer): Extended;
begin
  Result := FValueMarks[Index].Value;
end;

function TcxGridChartHistogramViewInfo.GetZeroValueOffset: Integer;
begin
  Result := GetValueOffset(ZeroValue);
end;

procedure TcxGridChartHistogramViewInfo.SetCategoryMarkPosition(Index: Integer; Value: Integer);
begin
  FCategoryMarks[Index].Position := Value;
end;

procedure TcxGridChartHistogramViewInfo.SetValueMarkPosition(Index: Integer; Value: Integer);
begin
  FValueMarks[Index].Position := Value;
end;

function TcxGridChartHistogramViewInfo.GetCategoryTickMarkLabelsViewInfoClass: TcxGridChartHistogramCategoryTickMarkLabelsViewInfoClass;
begin
  Result := TcxGridChartHistogramCategoryTickMarkLabelsViewInfo;
end;

function TcxGridChartHistogramViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartHistogramPainter;
end;

function TcxGridChartHistogramViewInfo.GetValueTickMarkLabelsViewInfoClass: TcxGridChartHistogramValueTickMarkLabelsViewInfoClass;
begin
  Result := TcxGridChartHistogramValueTickMarkLabelsViewInfo;
end;

procedure TcxGridChartHistogramViewInfo.DoCalculateParams;
begin
  inherited;
  GetPlotBackgroundParams(PlotParams);
end;

procedure TcxGridChartHistogramViewInfo.GetPlotBackgroundParams(out AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsPlot, nil, nil, AParams);
end;

function TcxGridChartHistogramViewInfo.GetCategoryDirection: TcxDirection;
begin
  if IsCategoryAxisHorz then
    if IsCategoriesInReverseOrder then
      Result := dirLeft
    else
      Result := dirRight
  else
    if IsCategoriesInReverseOrder then
      Result := dirDown
    else
      Result := dirUp;
end;

function TcxGridChartHistogramViewInfo.GetUnitAdjustedOffset(AIndex: Integer): Integer;
begin
  Result := GetUnitOffset(AIndex);
  if IsCategoryAxisHorz and (AIndex = UnitCount) or
    not IsCategoryAxisHorz and (AIndex = 0) then
    Dec(Result, CategoryGridLineWidth);
end;

function TcxGridChartHistogramViewInfo.GetUnitOffset(AIndex: Integer): Integer;
var
  AOffset: Integer;
begin
  with PlotBounds do
    if IsCategoryAxisHorz then
    begin
      AOffset := MulDiv(Right - Left, AIndex, UnitCount);
      if IsCategoriesInReverseOrder then
        Result := Right - AOffset
      else
        Result := Left + AOffset;
    end
    else
    begin
      AOffset := MulDiv(Bottom - Top, AIndex, UnitCount);
      if IsCategoriesInReverseOrder then
        Result := Top + AOffset
      else
        Result := Bottom - AOffset;
    end;
end;

procedure TcxGridChartHistogramViewInfo.CalculateMinMaxValues(out AMinValue, AMaxValue: Extended;
  out AHasValuesOutOfRange: Boolean);
var
  I, J: Integer;
  AValue: Extended;
begin
  InitializeMinMaxValues(AMinValue, AMaxValue);
  AHasValuesOutOfRange := False;
  for I := 0 to ValueCount - 1 do
    for J := 0 to SeriesCount - 1 do
    begin
      AValue := Values[J, I];
      if Diagram.AxisValue.MinMaxValues = mmvCustom then
      begin
        AHasValuesOutOfRange := (AValue < AMinValue) or (AValue > AMaxValue);
        if AHasValuesOutOfRange then Exit;
      end
      else
        if (Diagram.AxisValue.MinMaxValues = mmvAuto) and (I = 0) and (J = 0) then
        begin
          AMinValue := AValue;
          AMaxValue := AValue;
        end
        else
        begin
          AMinValue := Min(AMinValue, AValue);
          AMaxValue := Max(AMaxValue, AValue);
        end;
    end;
end;


procedure TcxGridChartHistogramViewInfo.CalculateMinMaxStackedValues(
  APercentage: Boolean; out AMinValue, AMaxValue: Extended; out AHasValuesOutOfRange: Boolean);
var
  I, J: Integer;
  ASummaryValue, ANegativeSummaryValue, AMinSummaryValue, AMaxSummaryValue: Extended;
begin
  AHasValuesOutOfRange := False;
  ASummaryValue := AMinValue;
  ANegativeSummaryValue := AMinValue;
  AMaxSummaryValue := AMinValue;
  AMinSummaryValue := AMaxValue;
  if (Diagram.AxisValue.MinMaxValues = mmvZeroBasedAuto) then
    AMinSummaryValue := 0;
  if APercentage then
  begin
    AMaxValue := 100;
    AMinValue := 0;
    AHasValuesOutOfRange := (AMinSummaryValue < AMinValue) or (AMaxSummaryValue > AMaxValue);
    Exit;
  end;
  for I := 0 to ValueCount - 1 do
  begin
    for J := 0 to SeriesCount - 1 do
    begin
      if IsSeriesBeginOfGroup(J) then
      begin
        ASummaryValue := Values[J, I];
        ANegativeSummaryValue := Values[J, I];
      end
      else
        if Values[J, I] >= 0 then
          ASummaryValue := ASummaryValue + Values[J, I]
        else
        begin
          if ANegativeSummaryValue > 0 then
            ANegativeSummaryValue := Values[J, I]
          else
            ANegativeSummaryValue := ANegativeSummaryValue + Values[J, I];
        end;
      if (J = 0) or (ASummaryValue > AMaxSummaryValue) then
        AMaxSummaryValue := ASummaryValue;
      if (J = 0) or (ANegativeSummaryValue < AMinSummaryValue) then
        AMinSummaryValue := ANegativeSummaryValue;
    end;
    if Diagram.AxisValue.MinMaxValues = mmvCustom then
    begin
      AHasValuesOutOfRange := (AMinSummaryValue < AMinValue) or (AMaxSummaryValue > AMaxValue);
      if AHasValuesOutOfRange then Exit;
    end
    else
    begin
      AMinValue := Min(AMinValue, AMinSummaryValue);
      AMaxValue := Max(AMaxValue, AMaxSummaryValue);
    end;
  end;
end;

procedure TcxGridChartHistogramViewInfo.CalculateMinMaxVisualValues(out AMinVisualValue, AMaxVisualValue: Extended);
begin
  if (ValueStep = 0) or (Diagram.AxisValue.MinMaxValues = mmvCustom) then
  begin
    AMinVisualValue := MinValue;
    AMaxVisualValue := MaxValue;
  end
  else
  begin
    AMinVisualValue := Floor(MinValue / ValueStep);
    if NeedsMinMaxValuesOffset and
      (MinValue <> 0) and (Fraction(MinValue / ValueStep) = 0) then
      AMinVisualValue := AMinVisualValue - 1;
    AMinVisualValue := AMinVisualValue * ValueStep;

    AMaxVisualValue := Ceiling(MaxValue / ValueStep);
    if NeedsMinMaxValuesOffset and
      (MaxValue <> 0) and (Fraction(MaxValue / ValueStep) = 0) then
      AMaxVisualValue := AMaxVisualValue + 1;
    AMaxVisualValue := AMaxVisualValue * ValueStep;
  end;
  if AMaxVisualValue - AMinVisualValue = 0 then
    AMaxVisualValue := AMinVisualValue + 1;
end;

function TcxGridChartHistogramViewInfo.CalculateSumOfValues(AGroupIndex, AValueIndex: Integer): Variant;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SeriesCount - 1 do
    if GetVisibleGroupIndex(I) = AGroupIndex then
      Result := Result + Abs(inherited GetValue(I, AValueIndex));
end;

function TcxGridChartHistogramViewInfo.CalculateValueStep: Extended;
const
  PowerBase: Extended = 10;
  MaxStepCount = 10;
  BaseStepCount = 3;
  BaseSteps: array[0..BaseStepCount - 1] of Integer = (1, 2, 5);
var
  AStep: Extended;
  AIntMultiple10, ABaseStepIndex: Integer;
begin
  AStep := (FMaxValue - FMinValue) / MaxStepCount;
  if AStep = 0 then
  begin
    Result := 0;
    Exit;
  end;
  AIntMultiple10 := Floor(Log10(AStep));
  if (AIntMultiple10 < 0) and IsDataInteger then
    AIntMultiple10 := 0;
  ABaseStepIndex := 0;
  repeat
    Result := BaseSteps[ABaseStepIndex] * IntPower(PowerBase, AIntMultiple10);
    if Result >= AStep then Break;
    Inc(ABaseStepIndex);
    if ABaseStepIndex = BaseStepCount then
    begin
      ABaseStepIndex := 0;
      Inc(AIntMultiple10);
    end;
  until False;
  CheckZero(Result);
end;

function TcxGridChartHistogramViewInfo.GetStackedValue(ASeriesIndex, AIndex: Integer): Variant;
var
  ASum: Variant;
begin
  Result := GridView.VisibleSeries[ASeriesIndex].VisibleValues[AIndex];
  if Diagram.Values.Stacking <> vs100Percent then Exit;
  ASum := SumOfValues[GetVisibleGroupIndex(ASeriesIndex),  AIndex];
  if ASum = 0 then
    Result := 0
  else
    Result := 100 * Result / ASum;
end;

function TcxGridChartHistogramViewInfo.GetValue(ASeriesIndex, AIndex: Integer): Variant;
var
  I: Integer;
  AIsPositive: Boolean;
  AValue, ASum: Variant;
begin
  Result := inherited GetValue(ASeriesIndex, AIndex);
  case Diagram.Values.Stacking of
    vsNormal:
      for I := 0 to ASeriesIndex - 1 do
        Result := Result + inherited GetValue(I, AIndex);
    vs100Percent:
      begin
        AIsPositive := IsPositive(Result);
        for I := 0 to ASeriesIndex - 1 do
        begin
          AValue := inherited GetValue(I, AIndex);
          if IsPositive(AValue) = AIsPositive then
            Result := Result + AValue;
        end;
        ASum := SumOfValues[GetVisibleGroupIndex(ASeriesIndex),  AIndex];
        if ASum = 0 then
          Result := 0
        else
          Result := 100 * Result / ASum;
      end;
  end;
end;

function TcxGridChartHistogramViewInfo.GetValueDirection(ASeriesIndex, AVisibleValueIndex: Integer): TcxDirection;
begin
  if IsValueAxisVert then
    if IsValuePositive(ASeriesIndex, AVisibleValueIndex) then
      Result := dirUp
    else
      Result := dirDown
  else
    if IsValuePositive(ASeriesIndex, AVisibleValueIndex) then
      Result := dirRight
    else
      Result := dirLeft;
end;

function TcxGridChartHistogramViewInfo.GetValueOffset(const AValue: Variant): Integer;
var
  V: Variant;
  AIsVertical: Boolean;
  AAreaSize, AOffset: Integer;
begin
  AIsVertical := IsValueAxisVert;
  with PlotBounds do
  begin
    V := AValue;
    if V < MinVisualValue then
      V := MinVisualValue
    else
      if V > MaxVisualValue then
        V := MaxVisualValue;
    if AIsVertical then
      AAreaSize := Bottom - Top - ValueGridLineWidth
    else
      AAreaSize := Right - Left - ValueGridLineWidth;
    try
      AOffset := AAreaSize * (V - MinVisualValue) / (MaxVisualValue - MinVisualValue);
    except
      on EVariantOverflowError do
        AOffset := cxMaxRectSize - Left;
    end;
    if AIsVertical then
      Result := Bottom - ValueGridLineWidth - AOffset
    else
      Result := Left + AOffset;
  end;
end;

function TcxGridChartHistogramViewInfo.GetVisibleGroupIndex(ASeriesIndex: Integer): Integer;
begin
  Result := Series[ASeriesIndex].VisibleGroupIndex;
end;

function TcxGridChartHistogramViewInfo.GetZeroValue: Extended;
begin
  Result := 0;
  if Diagram.AxisValue.MinMaxValues <> mmvZeroBasedAuto then
  begin
    if Result < MinVisualValue then
      Result := MinVisualValue;
    if Result > MaxVisualValue then
      Result := MaxVisualValue;
  end;
end;

procedure TcxGridChartHistogramViewInfo.InitializeMinMaxValues(
  out AMinValue, AMaxValue: Extended);
begin
  if Diagram.AxisValue.MinMaxValues = mmvCustom then
  begin
    AMinValue := Min(Diagram.AxisValue.MinValue, Diagram.AxisValue.MaxValue);
    AMaxValue := Max(Diagram.AxisValue.MinValue, Diagram.AxisValue.MaxValue)
  end
  else
  begin
    AMinValue := 0;
    AMaxValue := 0;
  end;
end;

function TcxGridChartHistogramViewInfo.IsDataInteger: Boolean;
var
  I: Integer;
begin
  for I := 0 to SeriesCount - 1 do
  begin
    Result := Series[I].DataBinding.IsValueTypeInteger;
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxGridChartHistogramViewInfo.IsSeriesBeginOfGroup(ASeriesIndex: Integer): Boolean;
begin
  Result := ASeriesIndex = 0;
end;

function TcxGridChartHistogramViewInfo.IsValuePositive(ASeriesIndex, AVisibleValueIndex: Integer): Boolean;
begin
  Result := IsPositive(VisibleValues[ASeriesIndex, AVisibleValueIndex] - ZeroValue);
end;

function TcxGridChartHistogramViewInfo.NeedsMinMaxValuesOffset: Boolean;
begin
  Result := Diagram.Values.Stacking <> vs100Percent;
end;

procedure TcxGridChartHistogramViewInfo.AddMark(var AMarks: TcxGridChartHistogramMarks;
  const AValue: Variant);
begin
  SetLength(AMarks, Length(AMarks) + 1);
  AMarks[Length(AMarks) - 1].Value := AValue;
end;

procedure TcxGridChartHistogramViewInfo.AddCategoryMark(AValue: Integer);
begin
  AddMark(FCategoryMarks, AValue);
end;

procedure TcxGridChartHistogramViewInfo.CalculateCategoryMarkPositions;
var
  I: Integer;
begin
  for I := 0 to CategoryMarkCount - 1 do
    CategoryMarkPositions[I] := GetUnitAdjustedOffset(CategoryMarkValues[I]);
end;

procedure TcxGridChartHistogramViewInfo.CalculateCategoryMarkValues;
var
  ACount, I: Integer;
begin
  if VisibleValueCount = 0 then Exit;
  ACount := VisibleValueCount;
  if Diagram.AxisCategory.ValueAxisBetweenCategories then
    Inc(ACount);
  for I := 0 to ACount - 1 do
    AddCategoryMark(GetCategoryTickMarkOffsetUnits(I, Diagram.AxisCategory.ValueAxisBetweenCategories));
end;

procedure TcxGridChartHistogramViewInfo.AddValueMark(const AValue: Extended);
begin
  AddMark(FValueMarks, AValue);
end;

procedure TcxGridChartHistogramViewInfo.CalculateValueMarkPositions;
var
  I: Integer;
begin
  for I := 0 to ValueMarkCount - 1 do
    ValueMarkPositions[I] := GetValueOffset(ValueMarkValues[I]);
end;

procedure TcxGridChartHistogramViewInfo.CalculateValueMarkValues;
var
  AValue: Extended;
begin
  if ValueStep = 0 then Exit;
  AValue := MinVisualValue;
  repeat
    CheckZero(AValue);
    AddValueMark(AValue);
    AValue := AValue + ValueStep;
  until AValue > MaxVisualValue;
end;

function TcxGridChartHistogramViewInfo.CalculateCategoryMarkHeight: Integer;
begin
  Result := MulDiv(CategoryTickMarkLabelsViewInfo.CaptionHeight, 1, 4);
end;

function TcxGridChartHistogramViewInfo.CalculateValueMarkHeight: Integer;
begin
  Result := MulDiv(ValueTickMarkLabelsViewInfo.CaptionHeight, 1, 4);
end;

function TcxGridChartHistogramViewInfo.CalculatePlotBounds: TRect;
var
  AOffsets: TRect;
begin
  Result := Bounds;
  SetRectEmpty(AOffsets);
  CalculatePlotOffsets(Result.Right - Result.Left, AOffsets);
  Inc(Result.Left, AOffsets.Left);
  Dec(Result.Right, AOffsets.Right);
  Inc(Result.Top, AOffsets.Top);
  Dec(Result.Bottom, AOffsets.Bottom);
  with Result do
  begin
    if Left > Right then Left := Right;
    if Top > Bottom then Top := Bottom;
  end;
end;

procedure TcxGridChartHistogramViewInfo.CalculatePlotOffsets(APlotAreaWidth: Integer;
  var AOffsets: TRect);

  procedure ProcessVerticalAxisCaptions(ACaptionsViewInfo: TcxGridChartHistogramTickMarkLabelsViewInfo;
    var AOffsets: TRect);
  var
    AOffset: Integer;
  begin
    if not ACaptionsViewInfo.Visible then
      Exit;

    AOffset := ScaleFactor.Apply(TickMarkLabelsOffset) + ACaptionsViewInfo.CalculateWidth;
    if ACaptionsViewInfo.Position = cppLeft then
      AOffsets.Left := Max(AOffsets.Left, AOffset)
    else
      AOffsets.Right := Max(AOffsets.Right, AOffset);
    ACaptionsViewInfo.FitCaptionsVert(AOffsets.Top, AOffsets.Bottom);
  end;

  procedure ProcessHorizontalAxisCaptions(ACaptionsViewInfo: TcxGridChartHistogramTickMarkLabelsViewInfo;
    var AOffsets: TRect; APlotAreaWidth: Integer);
  var
    AOffset: Integer;
  begin
    if not ACaptionsViewInfo.Visible then
      Exit;

    AOffset := ScaleFactor.Apply(TickMarkLabelsOffset) +
      ACaptionsViewInfo.CalculateHeight(APlotAreaWidth, AOffsets.Left, AOffsets.Right);
    if ACaptionsViewInfo.Position = cppTop then
      AOffsets.Top := Max(AOffsets.Top, AOffset)
    else
      AOffsets.Bottom := Max(AOffsets.Bottom, AOffset);
    ACaptionsViewInfo.FitCaptionsHorz(APlotAreaWidth, AOffsets.Left, AOffsets.Right);
  end;

var
  ACaptionsViewInfoHorz, ACaptionsViewInfoVert: TcxGridChartHistogramTickMarkLabelsViewInfo;
begin
  if ValueTickMarkLabelsViewInfo.IsVertical then
  begin
    ACaptionsViewInfoHorz := CategoryTickMarkLabelsViewInfo;
    ACaptionsViewInfoVert := ValueTickMarkLabelsViewInfo;
  end
  else
  begin
    ACaptionsViewInfoHorz := ValueTickMarkLabelsViewInfo;
    ACaptionsViewInfoVert := CategoryTickMarkLabelsViewInfo;
  end;

  ProcessVerticalAxisCaptions(ACaptionsViewInfoVert, AOffsets);
  ProcessHorizontalAxisCaptions(ACaptionsViewInfoHorz, AOffsets, APlotAreaWidth);
end;

function TcxGridChartHistogramViewInfo.GetCategoryAxisBounds: TRect;
begin
  Result := PlotBounds;
  if IsCategoryAxisHorz then
  begin
    Result.Top := ZeroValueOffset;
    Result.Bottom := Result.Top + CategoryAxisWidth;
  end
  else
  begin
    Result.Left := ZeroValueOffset;
    Result.Right := Result.Left + CategoryAxisWidth;
  end;
end;

function TcxGridChartHistogramViewInfo.GetCategoryAxisPosition: TcxGridChartAxisPosition;
begin
  Result := Diagram.GetCategoryAxisPosition;
end;

function TcxGridChartHistogramViewInfo.GetCategoryGridLineBounds(AIndex: Integer): TRect;
begin
  Result := PlotBounds;
  if IsCategoryAxisHorz then
  begin
    Result.Left := CategoryMarkPositions[AIndex];
    Result.Right := Result.Left + CategoryGridLineWidth;
  end
  else
  begin
    Result.Top := CategoryMarkPositions[AIndex];
    Result.Bottom := Result.Top + CategoryGridLineWidth;
  end;
end;

function TcxGridChartHistogramViewInfo.GetCategoryMarkBounds(Index: Integer): TRect;
var
  AIsHorizontal: Boolean;
begin
  AIsHorizontal := IsCategoryAxisHorz;

  Result := CategoryAxisBounds;
  if AIsHorizontal then
  begin
    Result.Left := CategoryMarkPositions[Index];
    Result.Right := Result.Left + ScaleFactor.Apply(MarkWidth);
  end
  else
  begin
    Result.Top := CategoryMarkPositions[Index];
    Result.Bottom := Result.Top + ScaleFactor.Apply(MarkWidth);
  end;

  if Diagram.AxisCategory.TickMarkKind in [tmkCross, tmkInside] then
    if AIsHorizontal then
      Dec(Result.Top, CategoryMarkHeight)
    else
      Inc(Result.Right, CategoryMarkHeight);

  if Diagram.AxisCategory.TickMarkKind in [tmkCross, tmkOutside] then
    if AIsHorizontal then
      Inc(Result.Bottom, CategoryMarkHeight)
    else
      Dec(Result.Left, CategoryMarkHeight);
end;

function TcxGridChartHistogramViewInfo.GetCategoryTickMarkLabelsBounds: TRect;
begin
  Result := GetTickMarkLabelsBounds(CategoryAxisPosition);
end;

function TcxGridChartHistogramViewInfo.GetTickMarkLabelsBounds(APosition: TcxGridChartAxisPosition): TRect;
begin
  Result := Bounds;
  case APosition of
    cppLeft:
      Result.Right := PlotBounds.Left - ScaleFactor.Apply(TickMarkLabelsOffset);
    cppRight:
      Result.Left := PlotBounds.Right + ScaleFactor.Apply(TickMarkLabelsOffset);
    cppTop:
      Result.Bottom := PlotBounds.Top - ScaleFactor.Apply(TickMarkLabelsOffset);
    cppBottom:
      Result.Top := PlotBounds.Bottom + ScaleFactor.Apply(TickMarkLabelsOffset);
  end;
end;

function TcxGridChartHistogramViewInfo.GetValueAxisBounds: TRect;
begin
  Result := PlotBounds;
  if IsValueAxisVert then
  begin
    if (ValueAxisPosition = cppLeft) xor IsCategoriesInReverseOrder then
      Result.Left := CategoryMarkPositions[0]
    else
      Result.Left := CategoryMarkPositions[CategoryMarkCount - 1];
    Result.Right := Result.Left + ValueAxisWidth;
  end
  else
  begin
    if (ValueAxisPosition = cppBottom) xor IsCategoriesInReverseOrder then
      Result.Top := CategoryMarkPositions[0]
    else
      Result.Top := CategoryMarkPositions[CategoryMarkCount - 1];
    Result.Bottom := Result.Top + ValueAxisWidth;
  end;
end;

function TcxGridChartHistogramViewInfo.GetValueAxisPosition: TcxGridChartAxisPosition;
begin
  Result := Diagram.GetValueAxisPosition;
end;

{function TcxGridChartHistogramViewInfo.GetValueBounds(AValueIndex, ASeriesIndex: Integer): TRect;
var
  ACategoryOffset, ACategoryOffsetStart, ACategoryOffsetFinish, AValueOffsetStart, AValueOffsetFinish: Integer;
begin
  ACategoryOffset := GetValueOffsetUnits(AValueIndex, ASeriesIndex);
  ACategoryOffsetStart := GetUnitOffset(ACategoryOffset);
  ACategoryOffsetFinish := GetUnitOffset(ACategoryOffset + ValueSizeUnits);
  AValueOffsetStart := ZeroValueOffset;
  AValueOffsetFinish := GetValueOffset(Values[ASeriesIndex, AValueIndex]);

  if IsValueAxisVert then
  begin
    Result.Left := ACategoryOffsetStart;
    Result.Right := ACategoryOffsetFinish;
    Result.Bottom := AValueOffsetStart;
    Result.Top := AValueOffsetFinish;
    CheckRectBounds(Result);
    Inc(Result.Bottom, ValueGridLineWidth);
  end
  else
  begin
    Result.Bottom := ACategoryOffsetStart;
    Result.Top := ACategoryOffsetFinish;
    Result.Left := AValueOffsetStart;
    Result.Right := AValueOffsetFinish;
    CheckRectBounds(Result);
    Inc(Result.Right, ValueGridLineWidth);
  end;
end;}

function TcxGridChartHistogramViewInfo.GetValueGridLineBounds(AIndex: Integer): TRect;
begin
  Result := PlotBounds;
  if IsValueAxisVert then
  begin
    Result.Top := ValueMarkPositions[AIndex];
    Result.Bottom := Result.Top + ValueGridLineWidth;
  end
  else
  begin
    Result.Left := ValueMarkPositions[AIndex];
    Result.Right := Result.Left + ValueGridLineWidth;
  end;
end;

function TcxGridChartHistogramViewInfo.GetValueMarkBounds(Index: Integer): TRect;
var
  AIsVertical: Boolean;
  AMarkKind: TcxGridChartHistogramTickMarkKind;
begin
  Result := ValueAxisBounds;
  AIsVertical := IsValueAxisVert;
  if AIsVertical then
  begin
    Result.Top := ValueMarkPositions[Index];
    Result.Bottom := Result.Top + ScaleFactor.Apply(MarkWidth);
  end
  else
  begin
    Result.Left := ValueMarkPositions[Index];
    Result.Right := Result.Left + ScaleFactor.Apply(MarkWidth);
  end;
  AMarkKind := Diagram.AxisValue.TickMarkKind;
  if (AMarkKind = tmkCross) or
    (ValueAxisPosition in [cppRight, cppTop]) and (AMarkKind = tmkInside) or
    (ValueAxisPosition in [cppLeft, cppBottom]) and (AMarkKind = tmkOutside) then
    if AIsVertical then
      Dec(Result.Left, ValueMarkHeight)
    else
      Inc(Result.Bottom, ValueMarkHeight);
  if (AMarkKind = tmkCross) or
    (ValueAxisPosition in [cppRight, cppTop]) and (AMarkKind = tmkOutside) or
    (ValueAxisPosition in [cppLeft, cppBottom]) and (AMarkKind = tmkInside) then
    if AIsVertical then
      Inc(Result.Right, ValueMarkHeight)
    else
      Dec(Result.Top, ValueMarkHeight);
end;

function TcxGridChartHistogramViewInfo.GetValueTickMarkLabelsBounds: TRect;
begin
  Result := GetTickMarkLabelsBounds(ValueAxisPosition);
end;

function TcxGridChartHistogramViewInfo.CustomDrawPlot(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
  Diagram.DoCustomDrawPlot(ACanvas, Self, PlotBounds, Result);
end;

function TcxGridChartHistogramViewInfo.DoCustomDrawPlot(ACanvas: TcxCanvas): Boolean;
begin
  Result := HasCustomDrawPlot;
  if Result then
  begin
    ACanvas.SetParams(PlotParams);
    Result := CustomDrawPlot(ACanvas);
    if not Result then
      ACanvas.GetParams(PlotParams);
  end;
end;

function TcxGridChartHistogramViewInfo.HasCustomDrawPlot: Boolean;
begin
  Result := Diagram.HasCustomDrawPlot;
end;

procedure TcxGridChartHistogramViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  CalculateMinMaxValues(FMinValue, FMaxValue, FHasValuesOutOfRange);
  FValueStep := CalculateValueStep;
  CalculateMinMaxVisualValues(FMinVisualValue, FMaxVisualValue);
  CalculateCategoryMarkValues;
  CalculateValueMarkValues;

  FCategoryMarkHeight := CalculateCategoryMarkHeight;
  FValueMarkHeight := CalculateValueMarkHeight;
  if not UseRightToLeftAlignment then
  begin
    CalculateCategoryMarkPositions;
    CalculateValueMarkPositions;
    if CategoryTickMarkLabelsViewInfo.Visible then
      CategoryTickMarkLabelsViewInfo.Calculate(GetCategoryTickMarkLabelsBounds);
    if ValueTickMarkLabelsViewInfo.Visible then
      ValueTickMarkLabelsViewInfo.Calculate(GetValueTickMarkLabelsBounds);

    CalculateValues;
  end;
end;

procedure TcxGridChartHistogramViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited;
  CalculateCategoryMarkPositions;
  CalculateValueMarkPositions;
  if CategoryTickMarkLabelsViewInfo.Visible then
    CategoryTickMarkLabelsViewInfo.Calculate(GetCategoryTickMarkLabelsBounds);
  if ValueTickMarkLabelsViewInfo.Visible then
    ValueTickMarkLabelsViewInfo.Calculate(GetValueTickMarkLabelsBounds);

  CalculateValues;
end;

function TcxGridChartHistogramViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if Result = nil then
    if PtInRect(PlotBounds, P) then
    begin
      Result := TcxGridChartHistogramPlotHitTest.Instance(P);
      InitHitTest(Result);
    end
    else
    begin
      Result := CategoryTickMarkLabelsViewInfo.GetHitTest(P);
      if Result = nil then
        Result := ValueTickMarkLabelsViewInfo.GetHitTest(P);
    end;
end;

{ TcxGridChartColumnDiagramLegendViewInfo }

function TcxGridChartColumnDiagramLegendViewInfo.GetItemsInReverseOrder: Boolean;
begin
  Result :=
     (Diagram.GetCategoryAxisPosition in [cppTop, cppBottom]) and
       Diagram.AxisCategory.CategoriesInReverseOrder or
     not (Diagram.GetCategoryAxisPosition in [cppTop, cppBottom]) and
       not Diagram.AxisCategory.CategoriesInReverseOrder;
end;

{ TcxGridChartColumnDiagramValueViewInfo }

function TcxGridChartColumnDiagramValueViewInfo.GetCaptionPosition: TcxGridChartColumnDiagramValueCaptionPosition;
begin
  Result := DiagramViewInfo.Diagram.Values.CaptionPosition;
end;

function TcxGridChartColumnDiagramValueViewInfo.GetDiagramViewInfo: TcxGridChartColumnDiagramViewInfo;
begin
  Result := TcxGridChartColumnDiagramViewInfo(inherited DiagramViewInfo);
end;

function TcxGridChartColumnDiagramValueViewInfo.CalculateCaptionBounds: TRect;

  function GetVerticalValueCaptionBounds(ACaptionWidth, ACaptionHeight: Integer;
    AIsUpDirection: Boolean): TRect;
  begin
    Result := ContentBounds;
    with Result do
    begin
      Left := GetRangeCenter(Left, Right) - ACaptionWidth div 2;
      Right := Left + ACaptionWidth;
      case CaptionPosition of
        cdvcpInsideBase:
          if AIsUpDirection then
            Top := Bottom - ACaptionHeight
          else
            Bottom := Top + ACaptionHeight;
        cdvcpInsideEnd:
          if AIsUpDirection then
            Bottom := Top + ACaptionHeight
          else
            Top := Bottom - ACaptionHeight;
        cdvcpOutsideEnd:
          if AIsUpDirection then
          begin
            Bottom := Self.Bounds.Top;
            Top := Bottom - ACaptionHeight;
          end
          else
          begin
            Top := Self.Bounds.Bottom;
            Bottom := Top + ACaptionHeight;
          end;
        cdvcpCenter:
          begin
            Top := GetRangeCenter(Top, Bottom) - ACaptionHeight div 2;
            Bottom := Top + ACaptionHeight;
          end;
      end;
    end;
  end;

  function GetHorizontalValueCaptionBounds(ACaptionWidth, ACaptionHeight: Integer;
    AIsRightDirection: Boolean): TRect;
  begin
    Result := ContentBounds;
    with Result do
    begin
      Top := GetRangeCenter(Top, Bottom) - ACaptionHeight div 2;
      Bottom := Top + ACaptionHeight;
      case CaptionPosition of
        cdvcpInsideBase:
          if AIsRightDirection then
            Right := Left + ACaptionWidth
          else
            Left := Right - ACaptionWidth;
        cdvcpInsideEnd:
          if AIsRightDirection then
            Left := Right - ACaptionWidth
          else
            Right := Left + ACaptionWidth;
        cdvcpOutsideEnd:
          if AIsRightDirection then
          begin
            Left := Self.Bounds.Right;
            Right := Left + ACaptionWidth;
          end
          else
          begin
            Right := Self.Bounds.Left;
            Left :=  Right - ACaptionWidth;
          end;
        cdvcpCenter:
          begin
            Left := GetRangeCenter(Left, Right) - ACaptionWidth div 2;
            Right := Left + ACaptionWidth;
          end
          else
          begin
            Right := Self.Bounds.Left;
            Left := Right - ACaptionWidth;
          end;
      end;
    end;
  end;

var
  AIsPositive: Boolean;
begin
  AIsPositive := DiagramViewInfo.IsValuePositive(SeriesIndex, VisibleValueIndex);
  if DiagramViewInfo.IsValueAxisVert then
    Result := GetVerticalValueCaptionBounds(CaptionViewInfo.CalculateWidth,
      CaptionViewInfo.CalculateHeight, AIsPositive)
  else
    Result := GetHorizontalValueCaptionBounds(CaptionViewInfo.CalculateWidth,
      CaptionViewInfo.CalculateHeight, AIsPositive);
end;

function TcxGridChartColumnDiagramValueViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := dxGetDarkerColor(Params.Color, 75);
end;

function TcxGridChartColumnDiagramValueViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxGridChartColumnDiagramValueViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := DiagramViewInfo.Diagram.Values.BorderWidth;
end;

function TcxGridChartColumnDiagramValueViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartColumnDiagramValuePainter;
end;

function TcxGridChartColumnDiagramValueViewInfo.HasCaption: Boolean;
begin
  Result := CaptionPosition <> cdvcpNone;
end;

function TcxGridChartColumnDiagramValueViewInfo.HasCaptionVisualCompensation: Boolean;
begin
  Result := True;
end;

{ TcxGridChartColumnDiagramViewInfo }

function TcxGridChartColumnDiagramViewInfo.GetDiagram: TcxGridChartColumnDiagram;
begin
  Result := TcxGridChartColumnDiagram(inherited Diagram);
end;

function TcxGridChartColumnDiagramViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartColumnDiagramPainter;
end;

class function TcxGridChartColumnDiagramViewInfo.GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass;
begin
  Result := TcxGridChartColumnDiagramValueViewInfo;
end;

function TcxGridChartColumnDiagramViewInfo.GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer;
begin
  Result := AVisibleIndex * SeriesCount + ASeriesIndex;
end;

function TcxGridChartColumnDiagramViewInfo.GetCategoryTickMarkLabelOffsetUnits(AIndex: Integer): Integer;
begin
  Result := GetCategoryTickMarkOffsetUnits(AIndex, False);
end;

function TcxGridChartColumnDiagramViewInfo.GetCategoryTickMarkOffsetUnits(AIndex: Integer;
  AValueAxisBetweenCategories: Boolean): Integer;
begin
  if AValueAxisBetweenCategories then
    Result := 0
  else
    Result := 1;
  Result := GetValueGroupAreaSizeUnits * (Result + 2 * AIndex) div 2;
end;

function TcxGridChartColumnDiagramViewInfo.GetUnitCount: Integer;
begin
  Result := VisibleValueCount * GetValueGroupAreaSizeUnits;
end;

function TcxGridChartColumnDiagramViewInfo.GetValueGroupAreaSizeUnits: Integer;
begin
  Result := SeriesCount * ScaleFactor.Apply(ColumnDiagramValueSizeUnits) + 2 * ScaleFactor.Apply(ColumnDiagramValueOffsetUnits);
end;

function TcxGridChartColumnDiagramViewInfo.GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex: Integer): Integer;
begin
  Result := AVisibleValueIndex * GetValueGroupAreaSizeUnits + ScaleFactor.Apply(ColumnDiagramValueOffsetUnits) +
    ASeriesIndex * ScaleFactor.Apply(ColumnDiagramValueSizeUnits);
end;

function TcxGridChartColumnDiagramViewInfo.IsEdgeCategoryTickMarkLabels: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartColumnDiagramViewInfo.CalculateValues;
var
  AValueIndex, ASeriesIndex: Integer;
begin
  for AValueIndex := 0 to VisibleValueCount - 1 do
    for ASeriesIndex := 0 to SeriesCount - 1 do
      CreateValueViewInfo(ASeriesIndex, AValueIndex).Calculate(GetValueBounds(AValueIndex, ASeriesIndex));
end;

function TcxGridChartColumnDiagramViewInfo.GetValueBounds(AVisibleValueIndex, ASeriesIndex: Integer): TRect;
var
  ACategoryOffset, ACategoryOffsetStart, ACategoryOffsetFinish, AValueOffsetStart, AValueOffsetFinish: Integer;
begin
  ACategoryOffset := GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex);
  ACategoryOffsetStart := GetUnitOffset(ACategoryOffset);
  ACategoryOffsetFinish := GetUnitOffset(ACategoryOffset + ScaleFactor.Apply(ColumnDiagramValueSizeUnits));
  AValueOffsetStart := ZeroValueOffset;
  AValueOffsetFinish := GetValueOffset(VisibleValues[ASeriesIndex, AVisibleValueIndex]);
  Result := MakeValueBounds(ACategoryOffsetStart, ACategoryOffsetFinish, AValueOffsetStart, AValueOffsetFinish);
end;

function TcxGridChartColumnDiagramViewInfo.MakeValueBounds(
  const ACategoryStart, ACategoryFinish, AValueStart, AValueFinish: Integer): TRect;
begin
  if IsValueAxisVert then
  begin
    Result.Left := ACategoryStart;
    Result.Right := ACategoryFinish;
    Result.Bottom := AValueStart;
    Result.Top := AValueFinish;
    if Result.Top < PlotBounds.Top then
      Result.Top := PlotBounds.Top;
    if Result.Bottom > PlotBounds.Bottom then
      Result.Bottom := PlotBounds.Bottom;
    CheckRectBounds(Result);
    Inc(Result.Bottom, ValueGridLineWidth);
  end
  else
  begin
    Result.Bottom := ACategoryStart;
    Result.Top := ACategoryFinish;
    Result.Left := AValueStart;
    Result.Right := AValueFinish;
    CheckRectBounds(Result);
    Inc(Result.Right, ValueGridLineWidth);
  end;
end;

{ TcxGridChartStackedColumnDiagramViewInfo }

procedure TcxGridChartStackedColumnDiagramViewInfo.CalculateValues;
var
  AValueIndex, ASeriesIndex, ANegativeValueStart, APositiveValueStart,
  AValueSize, AValueStart, AValueFinish, ACategoryOffset, ACategoryStart, ACategoryFinish: Integer;
begin
  APositiveValueStart := ZeroValueOffset;                   //UpdateSeriesVisibleGroups
  ANegativeValueStart := ZeroValueOffset;
  for AValueIndex := 0 to VisibleValueCount - 1 do
    for ASeriesIndex := 0 to SeriesCount - 1 do
    begin
      if IsSeriesBeginOfGroup(ASeriesIndex) then
      begin
        APositiveValueStart := ZeroValueOffset;
        ANegativeValueStart := ZeroValueOffset;
      end;
      ACategoryOffset := GetValueOffsetUnits(AValueIndex, GetVisibleGroupIndex(ASeriesIndex));
      ACategoryStart := GetUnitOffset(ACategoryOffset) + StackedGroupIndentWidth;
      ACategoryFinish := GetUnitOffset(ACategoryOffset + ScaleFactor.Apply(ColumnDiagramValueSizeUnits));
      AValueSize := (GetValueOffset(VisibleValues[ASeriesIndex, AValueIndex]) - ZeroValueOffset);
      if IsValuePositive(ASeriesIndex, AValueIndex) then
      begin
        AValueStart := APositiveValueStart;
        AValueFinish := AValueStart + AValueSize;
        APositiveValueStart := AValueFinish;
      end
      else
      begin
        AValueStart := ANegativeValueStart;
        AValueFinish := AValueStart + AValueSize;
        ANegativeValueStart := AValueFinish;
      end;
      CreateValueViewInfo(ASeriesIndex, AValueIndex).Calculate(
        MakeValueBounds(ACategoryStart, ACategoryFinish, AValueStart, AValueFinish));
    end;
end;

procedure TcxGridChartStackedColumnDiagramViewInfo.CalculateMinMaxValues(
  out AMinValue, AMaxValue: Extended; out AHasValuesOutOfRange: Boolean);
begin
  inherited CalculateMinMaxValues(AMinValue, AMaxValue, AHasValuesOutOfRange);
  CalculateMinMaxStackedValues(Diagram.Values.Stacking = vs100Percent,
    AMinValue, AMaxValue, AHasValuesOutOfRange);
end;

function TcxGridChartStackedColumnDiagramViewInfo.GetValue(ASeriesIndex, AIndex: Integer): Variant;
begin
  Result := GetStackedValue(ASeriesIndex, AIndex);
end;

function TcxGridChartStackedColumnDiagramViewInfo.GetValueGroupAreaSizeUnits: Integer;
begin
  Result := SeriesGroupCount * ScaleFactor.Apply(ColumnDiagramValueSizeUnits) + 2 * ScaleFactor.Apply(ColumnDiagramValueOffsetUnits);
end;

function TcxGridChartStackedColumnDiagramViewInfo.IsSeriesBeginOfGroup(
  ASeriesIndex: Integer): Boolean;
begin
  if Diagram.StackedStyle in [sdsDefault, sds100Percent] then
    Result := ASeriesIndex = 0
  else
    Result := (ASeriesIndex = 0) or
     (Series[ASeriesIndex].GroupIndex <> Series[ASeriesIndex - 1].GroupIndex);
end;

function TcxGridChartStackedColumnDiagramViewInfo.StackedGroupIndentWidth: Integer;
begin
  Result := 0;
  if GridView.VisibleSeriesGroupCount > 1 then
    Result := Diagram.SideBySideIndentWidth;
end;

function TcxGridChartStackedColumnDiagramViewInfo.GetDiagram: TcxGridChartStackedColumnDiagram;
begin
  Result := inherited Diagram as TcxGridChartStackedColumnDiagram;
end;

function TcxGridChartStackedColumnDiagramViewInfo.GetSeriesGroupCount: Integer;
begin
  Result := GridView.VisibleSeriesGroupCount;
end;

{ TcxGridChartLineDiagramLegendItemViewInfo }

function TcxGridChartLineDiagramLegendItemViewInfo.GetDiagram: TcxGridChartLineDiagram;
begin
  Result := TcxGridChartLineDiagram(inherited Diagram);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetLineStyle: TcxGridChartLineStyle;
begin
  Result := Diagram.Values.GetLineStyle(Series);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetLineWidth: Integer;
begin
  Result := Diagram.Values.LineWidth;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetMarkerSize: Integer;
begin
  Result := Diagram.Values.MarkerSize;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetMarkerStyle: TcxGridChartMarkerStyle;
begin
  Result := Diagram.Values.GetMarkerStyle(Series);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.CalculateHeight: Integer;
begin
  Result := inherited CalculateHeight;
  if LineStyle <> clsNone then
    Result := Max(Result, LineWidth);
  if MarkerStyle <> cmsNone then
    Result := Max(Result, MarkerSize);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.CalculateLegendKeyWidth: Integer;
begin
  Result := 0;
  if LineStyle <> clsNone then
    Result := Max(Result, 20 + 10 * LineWidth);
  if MarkerStyle <> cmsNone then
    if LineStyle = clsNone then
      Result := Max(Result, MarkerSize)
    else
      Result := Max(Result, 3 * MarkerSize);
end;

procedure TcxGridChartLineDiagramLegendItemViewInfo.CalculateMarkerPoints;
begin
  GetDiagramValueViewInfoClass.CalculateMarkerPointsEx(MarkerBounds, MarkerStyle,
    MarkerPoints);
end;

procedure TcxGridChartLineDiagramLegendItemViewInfo.DoCalculateParams;
begin
  inherited;
  TcxGridChartLineDiagramLegendViewInfo(Container).GetItemLegendKeyMarkerParams(Index, MarkerParams);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetDiagramValueViewInfoClass: TcxGridChartLineDiagramValueViewInfoClass;
begin
  Result := TcxGridChartLineDiagramValueViewInfoClass(Diagram.GetViewInfoClass.GetValueViewInfoClass);
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartLineDiagramLegendItemPainter;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetLineColor: TColor;
begin
  Result := LegendKeyParams.Color;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetLineStart: TPoint;
begin
  with LegendKeyBounds do
  begin
    Result.X := Left + RoundDiv(LineWidth, 2);
    Result.Y := GetRangeCenter(Top, Bottom);
  end;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetLineFinish: TPoint;
begin
  with LegendKeyBounds do
  begin
    Result.X := Right - RoundDiv(LineWidth, 2);
    Result.Y := GetRangeCenter(Top, Bottom);
  end;
end;

function TcxGridChartLineDiagramLegendItemViewInfo.GetMarkerBounds: TRect;
var
  R: TRect;
begin
  R := LegendKeyBounds;
  GetCenteredRect(Point(GetRangeCenter(R.Left, R.Right), GetRangeCenter(R.Top, R.Bottom)),
    MarkerSize, MarkerSize, Result);
end;

procedure TcxGridChartLineDiagramLegendItemViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  if not UseRightToLeftAlignment then
    CalculateMarkerPoints;
end;

procedure TcxGridChartLineDiagramLegendItemViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited;
  CalculateMarkerPoints;
end;

{ TcxGridChartLineDiagramLegendViewInfo }

function TcxGridChartLineDiagramLegendViewInfo.GetItemClass: TcxGridChartLegendItemViewInfoClass;
begin
  Result := TcxGridChartLineDiagramLegendItemViewInfo;
end;

procedure TcxGridChartLineDiagramLegendViewInfo.GetItemLegendKeyMarkerParams(AIndex: Integer;
  out AParams: TcxViewParams);
begin
  TcxGridChartLineDiagram(Diagram).Styles.GetValueMarkerParams(GetItemSeriesIndex(AIndex),
    GetItemValueIndex(AIndex), AParams);
end;

{ TcxGridChartLineDiagramValueViewInfo }

function TcxGridChartLineDiagramValueViewInfo.GetCaptionPosition: TcxGridChartLineDiagramValueCaptionPosition;
begin
  Result := Diagram.Values.CaptionPosition;
end;

function TcxGridChartLineDiagramValueViewInfo.GetDiagram: TcxGridChartLineDiagram;
begin
  Result := TcxGridChartLineDiagram(inherited Diagram);
end;

function TcxGridChartLineDiagramValueViewInfo.GetDiagramViewInfo: TcxGridChartLineDiagramViewInfo;
begin
  Result := TcxGridChartLineDiagramViewInfo(inherited DiagramViewInfo);
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineStyle: TcxGridChartLineStyle;
begin
  Result := Diagram.Values.GetLineStyle(Series);
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineWidth: Integer;
begin
  Result := Diagram.Values.LineWidth;
end;

function TcxGridChartLineDiagramValueViewInfo.GetMarkerStyle: TcxGridChartMarkerStyle;
begin
  Result := Diagram.Values.GetMarkerStyle(Series);
end;

function TcxGridChartLineDiagramValueViewInfo.CalculateCaptionBounds: TRect;
var
  ACaptionWidth, ACaptionHeight: Integer;
begin
  Result := cxRectInflate(MarkerBounds, ScaleFactor.Apply(LineDiagramCaptionOffset));
  ACaptionWidth := CaptionViewInfo.CalculateWidth + ScaleFactor.Apply(cxTextOffset) * 2;
  ACaptionHeight := CaptionViewInfo.CalculateHeight + ScaleFactor.Apply(cxTextOffset) * 2;

  case CaptionPosition of
    ldvcpLeft, ldvcpAboveLeft, ldvcpBelowLeft:
      begin
        Result.Right := Result.Left;
        Dec(Result.Left, ACaptionWidth);
      end;
    ldvcpRight, ldvcpAboveRight, ldvcpBelowRight:
      begin
        Result.Left := Result.Right;
        Inc(Result.Right, ACaptionWidth);
      end;
  else
    Result.Left := GetRangeCenter(Result.Left, Result.Right) - ACaptionWidth div 2;
    Result.Right := Result.Left + ACaptionWidth;
  end;
  case CaptionPosition of
    ldvcpAbove, ldvcpAboveLeft, ldvcpAboveRight:
      begin
        Result.Bottom := Result.Top;
        Dec(Result.Top, ACaptionHeight);
      end;
    ldvcpBelow, ldvcpBelowLeft, ldvcpBelowRight:
      begin
        Result.Top := Result.Bottom;
        Inc(Result.Bottom, ACaptionHeight);
      end;
  else
    Result.Top := GetRangeCenter(Result.Top, Result.Bottom) - ACaptionHeight div 2;
    Result.Bottom := Result.Top + ACaptionHeight;
  end;
end;

procedure TcxGridChartLineDiagramValueViewInfo.CalculateMarkerParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetValueMarkerParams(SeriesIndex, ValueIndex, AParams);
end;

procedure TcxGridChartLineDiagramValueViewInfo.CalculateMarkerPoints;
begin
  CalculateMarkerPointsEx(MarkerBounds, MarkerStyle, MarkerPoints);
end;

procedure TcxGridChartLineDiagramValueViewInfo.DoCalculateParams;
begin
  inherited;
  CalculateMarkerParams(MarkerParams);
end;

function TcxGridChartLineDiagramValueViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := HotSpotBounds;
end;

function TcxGridChartLineDiagramValueViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := HotSpotBounds;
end;

function TcxGridChartLineDiagramValueViewInfo.GetHotSpotBounds: TRect;
var
  ASize: Integer;
begin
  ASize := Diagram.Values.GetHotSpotSize;
  GetCenteredRect(Position, ASize, ASize, Result);
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineColor: TColor;
begin
  Result := Params.Color;
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineHotZoneMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(LineDiagramLineHotZoneMinWidth);
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineStart: TPoint;
begin
  Result.X := Bounds.Left;
  Result.Y := LineStartY;
end;

function TcxGridChartLineDiagramValueViewInfo.GetLineFinish: TPoint;
begin
  Result.X := Bounds.Right;
  Result.Y := LineFinishY;
end;

function TcxGridChartLineDiagramValueViewInfo.GetMarkerBounds: TRect;
var
  ASize: Integer;
begin
  ASize := Diagram.Values.MarkerSize;
  GetCenteredRect(Position, ASize, ASize, Result);
end;

function TcxGridChartLineDiagramValueViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := GetPainterClassEx;
end;

class function TcxGridChartLineDiagramValueViewInfo.GetPainterClassEx: TcxGridChartLineDiagramValuePainterClass;
begin
  Result := TcxGridChartLineDiagramValuePainter;
end;

function TcxGridChartLineDiagramValueViewInfo.GetPosition: TPoint;
begin
  if CategoryDirection = dirRight then
    Result.X := Bounds.Right
  else
    Result.X := Bounds.Left;
  if ValueDirection = dirUp then
    Result.Y := Bounds.Top
  else
    Result.Y := Bounds.Bottom;
end;

procedure TcxGridChartLineDiagramValueViewInfo.MakeRealBounds(var ABounds: TRect);
begin
  ABounds.Top := Min(ABounds.Top, Min(LineStartY, LineFinishY));
  ABounds.Bottom := Max(ABounds.Bottom, Max(LineStartY, LineFinishY));
end;

function TcxGridChartLineDiagramValueViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

function TcxGridChartLineDiagramValueViewInfo.HasCaption: Boolean;
begin
  Result := CaptionPosition <> ldvcpNone;
end;

procedure TcxGridChartLineDiagramValueViewInfo.Calculate(const ABounds: TRect;
  ALineStartY, ALineFinishY: Integer);
begin
  inherited Calculate(ABounds);
  FLineStartY := ALineStartY;
  FLineFinishY := ALineFinishY;
  CalculateMarkerPoints;
end;

class procedure TcxGridChartLineDiagramValueViewInfo.CalculateMarkerPointsEx(const ABounds: TRect;
  AStyle: TcxGridChartMarkerStyle; var APoints: TPoints);
var
  AOrigin: TPoint;
  ASize: Integer;
begin
  if not (AStyle in [cmsTriangle, cmsDiamond]) then Exit;
  with ABounds do
  begin
    AOrigin := TopLeft;
    ASize := Right - Left;
  end;
  if not Odd(ASize) then
  begin
    Dec(ASize);
    Inc(AOrigin.X);
    Inc(AOrigin.Y);
  end;
  if AStyle = cmsTriangle then
  begin
    SetLength(APoints, 3);
    APoints[0] := Point(AOrigin.X, AOrigin.Y + ASize - 1 - 1);
    APoints[1] := Point(AOrigin.X + ASize div 2, AOrigin.Y - 1);
    APoints[2] := Point(AOrigin.X + ASize, AOrigin.Y + ASize - 1);
  end
  else
  begin
    SetLength(APoints, 4);
    APoints[0] := Point(AOrigin.X, AOrigin.Y + ASize div 2);
    APoints[1] := Point(AOrigin.X + ASize div 2, AOrigin.Y - 1);
    APoints[2] := Point(AOrigin.X + ASize, AOrigin.Y + ASize div 2);
    APoints[3] := Point(AOrigin.X + ASize div 2, AOrigin.Y + ASize);
  end;
end;

function TcxGridChartLineDiagramValueViewInfo.GetAreaBoundsForPainting: TRect;
begin
  Result := inherited GetAreaBoundsForPainting;
  if LineStyle <> clsNone then
  begin
    MakeRealBounds(Result);
    InflateRect(Result, RoundDiv(LineWidth, 2), RoundDiv(LineWidth, 2));
  end;
  if MarkerStyle <> cmsNone then
    UnionRect(Result, Result, MarkerBounds);
end;

function TcxGridChartLineDiagramValueViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result = nil) and (LineStyle <> clsNone) and
    LineHasPoint(LineStart, LineFinish, LineWidth, P, LineHotZoneMinWidth) then
  begin
    Result := TcxGridChartValueLineHitTest.Instance(P);
    InitHitTest(Result);
    TcxGridChartValueLineHitTest(Result).CanDrillDown := False;
  end;
end;

function TcxGridChartLineDiagramValueViewInfo.GetRealBounds: TRect;
begin
  Result := Bounds;
  MakeRealBounds(Result);
end;

function TcxGridChartLineDiagramValueViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(HotSpotBounds, P);
end;

{ TcxGridChartLineDiagramViewInfo }

function TcxGridChartLineDiagramViewInfo.GetDiagram: TcxGridChartLineDiagram;
begin
  Result := TcxGridChartLineDiagram(inherited Diagram);
end;

function TcxGridChartLineDiagramViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartLineDiagramPainter;
end;

class function TcxGridChartLineDiagramViewInfo.GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass;
begin
  Result := TcxGridChartLineDiagramValueViewInfo;
end;

function TcxGridChartLineDiagramViewInfo.GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer;
begin
  Result := ASeriesIndex * VisibleValueCount + AVisibleIndex;
end;

function TcxGridChartLineDiagramViewInfo.GetCategoryTickMarkLabelOffsetUnits(AIndex: Integer): Integer;
begin
  Result := GetValueOffsetUnits(AIndex, -1);
end;

function TcxGridChartLineDiagramViewInfo.GetCategoryTickMarkOffsetUnits(
  AIndex: Integer; AValueAxisBetweenCategories: Boolean): Integer;
begin
  Result := ScaleFactor.Apply(LineDiagramValueSizeUnits) * AIndex;
end;

function TcxGridChartLineDiagramViewInfo.GetUnitCount: Integer;
begin
  Result := ScaleFactor.Apply(LineDiagramValueSizeUnits) * VisibleValueCount;
  if not Diagram.AxisCategory.ValueAxisBetweenCategories then
    Dec(Result, ScaleFactor.Apply(LineDiagramValueSizeUnits));
end;

function TcxGridChartLineDiagramViewInfo.GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex: Integer): Integer;
begin
  if Diagram.AxisCategory.ValueAxisBetweenCategories then
    Result := ScaleFactor.Apply(LineDiagramValueSizeUnits) div 2
  else
    Result := 0;
  Inc(Result, ScaleFactor.Apply(LineDiagramValueSizeUnits) * AVisibleValueIndex);
end;

function TcxGridChartLineDiagramViewInfo.GetValueY(AVisibleValueIndex, ASeriesIndex: Integer): Integer;
begin
  Result := GetValueOffset(VisibleValues[ASeriesIndex, AVisibleValueIndex]);
end;

function TcxGridChartLineDiagramViewInfo.IsEdgeCategoryTickMarkLabels: Boolean;
begin
  Result := not Diagram.AxisCategory.ValueAxisBetweenCategories;
end;

function TcxGridChartLineDiagramViewInfo.HasGap(AValueIndex: Integer): Boolean;
var
  ASeriesIndex: Integer;
begin
  Result := Diagram.EmptyPointsDisplayMode = epdmGap;
  if not Result then Exit;
  for ASeriesIndex := 0 to SeriesCount - 1 do
  begin
    Result := IsGapValue(ASeriesIndex, AValueIndex);
    if Result then Break;
  end;
end;

function TcxGridChartLineDiagramViewInfo.IsGapValue(
  ASeriesIndex, AValueIndex: Integer): Boolean;
begin
  Result := Diagram.EmptyPointsDisplayMode = epdmGap;
  if Result then
    Result := GridView.ViewData.IsEmptyValue[ASeriesIndex, GetValueIndex(AValueIndex)];
end;

procedure TcxGridChartLineDiagramViewInfo.CalculatePlotOffsets(APlotAreaWidth: Integer;
  var AOffsets: TRect);
var
  ACompensation: Integer;
begin
  inherited;
  if Diagram.Values.LineStyle = clsNone then
    ACompensation := 0
  else
    ACompensation := RoundDiv(Diagram.Values.LineWidth, 2);
  if Diagram.Values.MarkerStyle <> cmsNone then
    ACompensation := Max(ACompensation, RoundDiv(Diagram.Values.MarkerSize, 2));
  if ACompensation <> 0 then
  begin
    AOffsets.Left := Max(AOffsets.Left, ACompensation);
    AOffsets.Top := Max(AOffsets.Top, ACompensation);
    AOffsets.Right := Max(AOffsets.Right, ACompensation);
    AOffsets.Bottom := Max(AOffsets.Bottom, ACompensation);
  end;
end;

procedure TcxGridChartLineDiagramViewInfo.CalculateValues;
var
  R: TRect;
  AEmptyPointFound: Boolean;
  AValueIndex, ASeriesIndex, ALineStartY, ALineFinishY: Integer;
begin
  ALineStartY := 0;  // to avoid warning
  for ASeriesIndex := 0 to SeriesCount - 1 do
  begin
    AEmptyPointFound := False;
    for AValueIndex := 0 to VisibleValueCount - 1 do
    begin
      if IsGapValue(ASeriesIndex, AValueIndex) then
      begin
        AEmptyPointFound := True;
        Continue;
      end;
      ALineFinishY := GetValueY(AValueIndex, ASeriesIndex);
      R := GetValueBounds(AValueIndex, ASeriesIndex);
      if (AValueIndex = 0) or AEmptyPointFound then
      begin
        ALineStartY := ALineFinishY;
        R.Left := R.Right;
      end;
      if IsCategoriesInReverseOrder then
        SwapIntegers(ALineStartY, ALineFinishY);
      TcxGridChartLineDiagramValueViewInfo(CreateValueViewInfo(ASeriesIndex, AValueIndex)).Calculate(
        R, ALineStartY, ALineFinishY);
      if not IsCategoriesInReverseOrder then
        ALineStartY := ALineFinishY;
      AEmptyPointFound := False;
    end;
  end;
end;

function TcxGridChartLineDiagramViewInfo.GetValueBounds(AVisibleValueIndex, ASeriesIndex: Integer): TRect;
var
  ACategoryOffsetStart, ACategoryOffsetFinish, AValueOffsetStart, AValueOffsetFinish: Integer;
begin
  ACategoryOffsetFinish := GetUnitAdjustedOffset(GetValueOffsetUnits(AVisibleValueIndex, ASeriesIndex));
  if AVisibleValueIndex = 0 then
    ACategoryOffsetStart := ACategoryOffsetFinish
  else
    ACategoryOffsetStart := GetUnitAdjustedOffset(GetValueOffsetUnits(AVisibleValueIndex - 1, ASeriesIndex));
  AValueOffsetStart := ZeroValueOffset;
  AValueOffsetFinish := GetValueY(AVisibleValueIndex, ASeriesIndex);

  Result.Left := ACategoryOffsetStart;
  Result.Right := ACategoryOffsetFinish;
  Result.Bottom := AValueOffsetStart;
  Result.Top := AValueOffsetFinish;
  CheckRectBounds(Result);
end;

{ TcxGridChartAreaDiagramLegendItemViewInfo }

function TcxGridChartAreaDiagramLegendItemViewInfo.CalculateHeight: Integer;
begin
  Result := Max(inherited CalculateHeight, 2 * ScaleFactor.Apply(AreaDiagramLegendKeyOffset) + LegendKeyHeight);
end;

function TcxGridChartAreaDiagramLegendItemViewInfo.CalculateLegendKeyHeight: Integer;
begin
  Result := 2 * ScaleFactor.Apply(AreaDiagramLegendKeySpace);
  if LineStyle <> clsNone then
    Inc(Result, 2 * LineWidth);
  if MarkerStyle <> cmsNone then
    Inc(Result, MarkerSize);
  Result := Max(Result, inherited CalculateLegendKeyHeight);
end;

function TcxGridChartAreaDiagramLegendItemViewInfo.CalculateLegendKeyWidth: Integer;
begin
  Result := inherited CalculateLegendKeyWidth;
  if Result = 0 then
    Result := LegendKeyHeight;
end;

function TcxGridChartAreaDiagramLegendItemViewInfo.GetLineColor: TColor;
begin
  Result := LegendKeyParams.TextColor;
end;

function TcxGridChartAreaDiagramLegendItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartAreaDiagramLegendItemPainter;
end;

{ TcxGridChartAreaDiagramLegendViewInfo }

function TcxGridChartAreaDiagramLegendViewInfo.GetItemClass: TcxGridChartLegendItemViewInfoClass;
begin
  Result := TcxGridChartAreaDiagramLegendItemViewInfo;
end;

function TcxGridChartAreaDiagramLegendViewInfo.GetItemsInReverseOrder: Boolean;
begin
  Result := (Orientation = cpoVertical) and (Diagram.Values.Stacking <> vsNone);
end;

{function TcxGridChartAreaDiagramLegendViewInfo.ItemLegendKeyBorderIsValueBorder: Boolean;
begin
  Result := True;
end;}

{ TcxGridChartAreaDiagramValueViewInfo }

function TcxGridChartAreaDiagramValueViewInfo.GetDiagram: TcxGridChartAreaDiagram;
begin
  Result := TcxGridChartAreaDiagram(inherited Diagram);
end;

procedure TcxGridChartAreaDiagramValueViewInfo.CalculateAreaPoints;
var
  APrevSeriesValuePoints: TPoints;
begin
  SetLength(AreaPoints, 4);
  AreaPoints[1] := LineStart;
  AreaPoints[2] := LineFinish;
  if not Diagram.CheckGapPoints and (Diagram.Values.Stacking <> vsNone) and (SeriesIndex > 0) then
  begin
    APrevSeriesValuePoints := TcxGridChartAreaDiagramValueViewInfo(
      DiagramViewInfo.GetValueViewInfo(SeriesIndex - 1, VisibleValueIndex)).AreaPoints;
    AreaPoints[0] := APrevSeriesValuePoints[1];
    AreaPoints[3] := APrevSeriesValuePoints[2];
  end
  else
  begin
    AreaPoints[0] := Point(Bounds.Left, ZeroValueOffset);
    AreaPoints[3] := Point(Bounds.Right, ZeroValueOffset);
  end;
end;

function TcxGridChartAreaDiagramValueViewInfo.CustomDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDrawBackground(ACanvas);
  if not Result then
    Diagram.DoCustomDrawValueArea(ACanvas, Self, Result);
end;

function TcxGridChartAreaDiagramValueViewInfo.GetAreaColor: TColor;
begin
  Result := Params.Color;
end;

function TcxGridChartAreaDiagramValueViewInfo.GetHotSpotBounds: TRect;
var
  ASize: TPoint;
  APrevSeriesValuePosition: Integer;
begin
  ASize.X := Diagram.Values.GetHotSpotSize;
  if MarkerStyle = cmsNone then
    ASize.Y := 0
  else
    ASize.Y := ASize.X;
  GetCenteredRect(Position, ASize.X, ASize.Y, Result);

  if (Diagram.Values.Stacking <> vsNone) and (SeriesIndex > 0) then
  begin
    APrevSeriesValuePosition := TcxGridChartAreaDiagramValueViewInfo(
      DiagramViewInfo.GetValueViewInfo(SeriesIndex - 1, VisibleValueIndex)).Position.Y;
    if Result.Top < APrevSeriesValuePosition then
      Result.Bottom := APrevSeriesValuePosition
    else
      Result.Top := APrevSeriesValuePosition;
  end
  else
  begin
    Result.Top := Min(Result.Top, ZeroValueOffset);
    Result.Bottom := Max(Result.Bottom, ZeroValueOffset);
  end;
end;

function TcxGridChartAreaDiagramValueViewInfo.GetLineColor: TColor;
begin
  Result := Params.TextColor;
end;

function TcxGridChartAreaDiagramValueViewInfo.GetLineHotZoneMinWidth: Integer;
begin
  Result := 0;
end;

class function TcxGridChartAreaDiagramValueViewInfo.GetPainterClassEx: TcxGridChartLineDiagramValuePainterClass;
begin
  Result := TcxGridChartAreaDiagramValuePainter;
end;

procedure TcxGridChartAreaDiagramValueViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  inherited;
end;

function TcxGridChartAreaDiagramValueViewInfo.GetZeroValueOffset: Integer;
begin
  if ValueDirection = dirUp then
    Result := Bounds.Bottom
  else
    Result := Bounds.Top;
end;

function TcxGridChartAreaDiagramValueViewInfo.HasCustomDrawBackground: Boolean;
begin
  Result := Diagram.HasCustomDrawValueArea;
end;

procedure TcxGridChartAreaDiagramValueViewInfo.Calculate(const ABounds: TRect;
  ALineStartY, ALineFinishY: Integer);
begin
  inherited;
  CalculateAreaPoints;
end;

function TcxGridChartAreaDiagramValueViewInfo.CreateAreaRegion: TcxRegion;
begin
  Result := TcxRegion.Create(CreatePolygonRgn(AreaPoints[0], Length(AreaPoints), WINDING));
end;

function TcxGridChartAreaDiagramValueViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  ARegion: TcxRegion;
begin
  Result := inherited GetHitTest(P);
  if Result = nil then
  begin
    ARegion := CreateAreaRegion;
    try
      if ARegion.PtInRegion(P) then
      begin
        Result := TcxGridChartValueAreaHitTest.Instance(P);
        InitHitTest(Result);
        TcxGridChartValueAreaHitTest(Result).CanDrillDown := False;
      end;
    finally
      ARegion.Free;
    end;
  end;
end;

{ TcxGridChartAreaDiagramViewInfo }

function TcxGridChartAreaDiagramViewInfo.ExcludeEachSeriesArea: Boolean;
begin
  Result := True;
end;

function TcxGridChartAreaDiagramViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartAreaDiagramPainter;
end;

class function TcxGridChartAreaDiagramViewInfo.GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass;
begin
  Result := TcxGridChartAreaDiagramValueViewInfo;
end;

{ TcxGridChartStackedAreaDiagramViewInfo }

procedure TcxGridChartStackedAreaDiagramViewInfo.CalculateValues;
var
  R: TRect;
  AEmptyPointFound: Boolean;
  AValueIndex, ASeriesIndex, ALineStartY, ALineFinishY, AActualLineFinishY: Integer;
begin
  if Diagram.StackedStyle <> sas100Percent then
  begin
    inherited CalculateValues;
    Exit;
  end;
  ALineStartY := 0;  // to avoid warning
  for ASeriesIndex := 0 to SeriesCount - 1 do
  begin
    AEmptyPointFound := False;
    for AValueIndex := 0 to VisibleValueCount - 1 do
    begin
      if IsGapValue(ASeriesIndex, AValueIndex) then
      begin
        AEmptyPointFound := True;
        Continue;
      end;
      ALineFinishY := GetValueY(AValueIndex, ASeriesIndex);
      AActualLineFinishY := ALineFinishY;
      if (AValueIndex > 0) and HasGap(AValueIndex - 1) and (GetValue(ASeriesIndex, AValueIndex - 1) = 100)  then
        AActualLineFinishY := ALineStartY;
      R := GetValueBounds(AValueIndex, ASeriesIndex);
      if (AValueIndex = 0) or AEmptyPointFound then
      begin
        ALineStartY := AActualLineFinishY;
        R.Left := R.Right;
      end;
      if (AValueIndex > 0) and (GetSeriesCountAt(AValueIndex) = 1) then
      begin
        AActualLineFinishY := GetValueOffset(cxGridChartFullStackedValue);
        ALineStartY := GetValueOffset(cxGridChartFullStackedValue);
      end;
      if IsCategoriesInReverseOrder then
        SwapIntegers(ALineStartY, ALineFinishY);
      TcxGridChartLineDiagramValueViewInfo(CreateValueViewInfo(ASeriesIndex, AValueIndex)).Calculate(
        R, ALineStartY, AActualLineFinishY);
      if not IsCategoriesInReverseOrder then
        ALineStartY := ALineFinishY;
      AEmptyPointFound := False;
    end;
  end;
end;

function TcxGridChartStackedAreaDiagramViewInfo.ExcludeEachSeriesArea: Boolean;
begin
  Result := True;
end;

function TcxGridChartStackedAreaDiagramViewInfo.GetValue(ASeriesIndex, AIndex: Integer): Variant;
var
  I: Integer;
begin
  Result := GetStackedValue(ASeriesIndex, AIndex);
  if HasGap(AIndex) and (Diagram.StackedStyle = sas100Percent) then
  begin
    Result := 100;
    for I := SeriesCount - 1 downto ASeriesIndex + 1 do
      Result := Result - GetStackedValue(I, AIndex);
  end
  else
    if not IsSeriesBeginOfGroup(ASeriesIndex) then
      for I := ASeriesIndex - 1 downto 0 do
      begin
        Result := Result + GetStackedValue(I, AIndex);
        if IsSeriesBeginOfGroup(I) then Break;
      end;
end;

function TcxGridChartStackedAreaDiagramViewInfo.GetSeriesCountAt(AValueIndex: Integer): Integer;
var
  ASeriesIndex: Integer;
begin
  Result := 0;
  for ASeriesIndex := 0 to SeriesCount - 1 do
    if not IsGapValue(ASeriesIndex, AValueIndex - 1) and not IsGapValue(ASeriesIndex, AValueIndex) then
      Inc(Result);
end;

function TcxGridChartStackedAreaDiagramViewInfo.GetDiagram: TcxGridChartStackedAreaDiagram;
begin
  Result := TcxGridChartStackedAreaDiagram(inherited Diagram);
end;

{ TcxGridChartPieDiagramLegendViewInfo }

function TcxGridChartPieDiagramLegendViewInfo.GetDiagram: TcxGridChartPieDiagram;
begin
  Result := TcxGridChartPieDiagram(inherited Diagram);
end;

function TcxGridChartPieDiagramLegendViewInfo.GetItemObjectIndex(AIndex: Integer): Integer;
begin
  Result := AIndex;
end;

function TcxGridChartPieDiagramLegendViewInfo.GetKind: TcxGridChartLegendKind;
begin
  if GridView.VisibleSeriesCount = 0 then
    Result := lkSeries
  else
    Result := lkCategories;
end;

{ TcxGridChartPieDiagramValueViewInfo }

function TcxGridChartPieDiagramValueViewInfo.GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
begin
  Result := DiagramViewInfo.CaptionPosition;
end;

function TcxGridChartPieDiagramValueViewInfo.GetCenter: TPoint;
begin
  with Bounds do
  begin
    Result.X := GetRangeCenter(Left, Right);
    Result.Y := GetRangeCenter(Top, Bottom);
  end;
end;

function TcxGridChartPieDiagramValueViewInfo.GetDiagramViewInfo: TcxGridChartPieDiagramViewInfo;
begin
  Result := TcxGridChartPieDiagramViewInfo(inherited DiagramViewInfo);
end;

function TcxGridChartPieDiagramValueViewInfo.GetRadius: Integer;
begin
  with Bounds do
    Result := (Right - Left) div 2;
end;

function TcxGridChartPieDiagramValueViewInfo.CalculateCaptionBounds: TRect;
var
  ACaptionWidth, ACaptionHeight, AAngle: Integer;
begin
  ACaptionWidth := CaptionViewInfo.CalculateWidth;
  ACaptionHeight := CaptionViewInfo.CalculateHeight;
  if not GridView.OptionsView.TransparentCaptions then
    Inc(ACaptionHeight, ScaleFactor.Apply(cxTextOffset) * 2);
  AAngle := GetRangeCenter(StartAngle, FinishAngle) mod 360;
  GetCenteredRect(CalculateCaptionCenter(AAngle, ACaptionWidth, ACaptionHeight), ACaptionWidth, ACaptionHeight, Result);
  OffsetRect(Result, 0, -1);  // to center text inside text rect
  if CaptionPosition = pdvcpOutsideEndWithLeaderLines then
    MakeCaptionVisible(Result, AAngle);
end;

function TcxGridChartPieDiagramValueViewInfo.CalculateCaptionCenter(AAngle, ACaptionWidth, ACaptionHeight: Integer): TPoint;
const
  SpecialCaseAngleDelta = 25;
var
  ARadius: Integer;
  AIsSpecialCase: Boolean;
  ASin, ACos: Extended;
begin
  if CaptionPosition = pdvcpOutsideEndWithLeaderLines then
  begin
    CalculateLeaderLinePoints(AAngle);
    Result := LeaderLinePoints[Length(LeaderLinePoints) - 1];
    if AAngle < 180 then
      Inc(Result.X, RoundDiv(ACaptionWidth, 2))
    else
      Dec(Result.X, RoundDiv(ACaptionWidth, 2) - 1);
  end
  else
  begin
    ARadius := Radius;
    AIsSpecialCase := False;
    case CaptionPosition of
      pdvcpCenter:
        ARadius := ARadius div 2;
      pdvcpInsideEnd:
        Dec(ARadius, ScaleFactor.Apply(PieValueCaptionOffset) + Max(ACaptionWidth div 2, ACaptionHeight div 2));
      pdvcpOutsideEnd:
        begin
          Inc(ARadius, ScaleFactor.Apply(PieValueCaptionOffset) + ACaptionHeight div 2);
          AIsSpecialCase := not (AAngle mod 180 in [SpecialCaseAngleDelta..180 - SpecialCaseAngleDelta]);
          if AIsSpecialCase then
          begin
            SinCos(DegToRad(AAngle mod 180), ASin, ACos);
            if ACaptionWidth div 2 >= Abs(ASin / ACos * ARadius) then
              ARadius := Round(Abs(ARadius / ACos))
            else
              ARadius := Round(ACaptionWidth div 2 * ASin + Sqrt(Sqr(ARadius) - Sqr(ACaptionWidth div 2 * ACos)));
          end;
        end;
    end;
    Result := GetPointOnCircle(Center, ARadius, AAngle);
    if CaptionPosition = pdvcpOutsideEnd then
      if AIsSpecialCase then
        if AAngle in [180 - SpecialCaseAngleDelta..180 + SpecialCaseAngleDelta] then
          Inc(Result.Y, ACaptionHeight div 2)
        else
          Dec(Result.Y, ACaptionHeight div 2)
      else
        if AAngle < 180 then
          Inc(Result.X, ACaptionWidth div 2)
        else
          Dec(Result.X, ACaptionWidth div 2);
  end;
end;

procedure TcxGridChartPieDiagramValueViewInfo.CalculateLeaderLinePoints(AAngle: Integer);
begin
  SetLength(LeaderLinePoints, 3);
  LeaderLinePoints[0] := GetPointOnCircle(Center, Radius, AAngle);
  LeaderLinePoints[1] := GetPointOnCircle(Center, Radius + ScaleFactor.Apply(PieLeaderLineSegment1), AAngle);
  LeaderLinePoints[2] := LeaderLinePoints[1];
  if AAngle < 180 then
    Inc(LeaderLinePoints[2].X, ScaleFactor.Apply(PieLeaderLineSegment2))
  else
    Dec(LeaderLinePoints[2].X, ScaleFactor.Apply(PieLeaderLineSegment2));
end;

function TcxGridChartPieDiagramValueViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  ACanvas.Pen.Color := Params.TextColor;
  Result := inherited DoCustomDraw(ACanvas);
end;

function TcxGridChartPieDiagramValueViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := False;
end;

function TcxGridChartPieDiagramValueViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartPieDiagramValuePainter;
end;

function TcxGridChartPieDiagramValueViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

function TcxGridChartPieDiagramValueViewInfo.HasCaption: Boolean;
begin
  Result := (Radius <> 0) and (CaptionPosition <> pdvcpNone);
end;

procedure TcxGridChartPieDiagramValueViewInfo.MakeCaptionVisible(var ACaptionBounds: TRect;
  AAngle: Integer);
var
  APrevValueIndex, ACaptionOffset: Integer;
  APrevCaptionBounds, AIntersection: TRect;
begin
  if VisibleValueIndex = 0 then Exit;
  APrevValueIndex := VisibleValueIndex;
  repeat
    Dec(APrevValueIndex);
    APrevCaptionBounds :=
      DiagramViewInfo.GetValueViewInfo(SeriesIndex, APrevValueIndex).CaptionViewInfo.Bounds;
    if not EqualRect(ACaptionBounds, APrevCaptionBounds) and
      IntersectRect(AIntersection, ACaptionBounds, APrevCaptionBounds) then
    begin
      if AAngle < 180 then
        ACaptionOffset := APrevCaptionBounds.Right - ACaptionBounds.Left
      else
        ACaptionOffset := APrevCaptionBounds.Left - ACaptionBounds.Right;
      OffsetRect(ACaptionBounds, ACaptionOffset, 0);
      if LeaderLinePoints <> nil then
        Inc(LeaderLinePoints[Length(LeaderLinePoints) - 1].X, ACaptionOffset);
      APrevValueIndex := VisibleValueIndex;
    end;
  until APrevValueIndex = 0;
end;

procedure TcxGridChartPieDiagramValueViewInfo.Calculate(const R: TRect;
  AStartAngle, AFinishAngle: Integer);
begin
  FStartAngle := AStartAngle;
  FFinishAngle := AFinishAngle;
  inherited Calculate(R);
end;

function TcxGridChartPieDiagramValueViewInfo.HasPoint(const P: TPoint): Boolean;
var
  APointRadius, APointAngle: Integer;
begin
  Result := inherited HasPoint(P);
  if Result then
  begin
    APointRadius := Round(Sqrt(Sqr(P.X - Center.X) + Sqr(P.Y - Center.Y)));
    Result := APointRadius <= Radius;
    if Result then
    begin
      APointAngle := 90 + Round(RadToDeg(ArcTan2(P.Y - Center.Y, P.X - Center.X)));
      if APointAngle < 0 then Inc(APointAngle, 360);
      Result := (StartAngle <= APointAngle) and (APointAngle < FinishAngle);
      if not Result then
      begin
        Inc(APointAngle, 360);
        Result := (StartAngle <= APointAngle) and (APointAngle < FinishAngle);
      end;
    end;
  end;
end;

{ TcxGridChartPieSeriesSiteCaptionViewInfo }

constructor TcxGridChartPieSeriesSiteCaptionViewInfo.Create(ASiteViewInfo: TcxGridChartPieSeriesSiteViewInfo);
begin
  inherited Create(ASiteViewInfo.GridViewInfo);
  FSiteViewInfo := ASiteViewInfo;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.CalculateHeight: Integer;
begin
  Result := BorderSize[bTop] + TextHeightWithOffset + BorderSize[bBottom];
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
    FSiteViewInfo.Diagram.DoCustomDrawSeriesSiteCaption(ACanvas, Self, Result);
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := FSiteViewInfo.BorderColor[AIndex];
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetBorders: TcxBorders;
begin
  if FSiteViewInfo.Visible then
    Result := [bBottom]
  else
    Result := [];
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := FSiteViewInfo.BorderWidth[AIndex];
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartPieSeriesSiteCaptionHitTest;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetText: string;
begin
  Result := FSiteViewInfo.Series.GetDisplayText;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartPieSeriesSiteCaptionPainter;
end;

procedure TcxGridChartPieSeriesSiteCaptionViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FSiteViewInfo.Diagram.Styles.GetViewParams(dsSeriesSiteCaptions, nil, nil, AParams);
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.GetVisible: Boolean;
begin
  Result := FSiteViewInfo.Diagram.SeriesCaptions;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.HasCustomDraw: Boolean;
begin
  Result := FSiteViewInfo.Diagram.HasCustomDrawSeriesSiteCaption;
end;

procedure TcxGridChartPieSeriesSiteCaptionViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FSiteViewInfo.InitHitTest(AHitTest);
  inherited;
end;

function TcxGridChartPieSeriesSiteCaptionViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and not (ssDouble in AShift) and GridView.IsDesigning then
  begin
    GridView.Controller.DesignController.SelectObject(FSiteViewInfo.Series, not (ssShift in AShift));
    Result := True;
  end;
end;

{ TcxGridChartPieSeriesSiteViewInfo }

constructor TcxGridChartPieSeriesSiteViewInfo.Create(ADiagramViewInfo: TcxGridChartPieDiagramViewInfo;
  ASeries: TcxGridChartSeries);
begin
  inherited Create(ADiagramViewInfo.GridViewInfo);
  FDiagramViewInfo := ADiagramViewInfo;
  FSeries := ASeries;
  FCaptionViewInfo := GetCaptionViewInfoClass.Create(Self);
end;

destructor TcxGridChartPieSeriesSiteViewInfo.Destroy;
begin
  FCaptionViewInfo.Free;
  inherited;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetDiagram: TcxGridChartPieDiagram;
begin
  Result := FDiagramViewInfo.Diagram;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.CalculateAngles;
var
  ASumOfValues, AValue: Variant;
  AAngleOffset, I: Integer;
begin
  ASumOfValues := Series.SumOfValues;
  AAngleOffset := Diagram.Values.AngleOfFirstSlice;
  SetLength(Angles, Series.VisibleValueCount + 1);
  if ASumOfValues <> 0 then
  begin
    AValue := 0;
    for I := 0 to Series.VisibleValueCount - 1 do
    begin
      Angles[I] := AAngleOffset + Min(Round(360 * AValue / ASumOfValues), 360);
      AValue := AValue + Abs(Series.VisibleValues[I]);
    end;
    Angles[Series.VisibleValueCount] := AAngleOffset + 360;
  end;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.CalculateCaptionAndPieAreaBounds(var ACaptionBounds, APieAreaBounds: TRect);
var
  ASize: Integer;
begin
  ACaptionBounds := ContentBounds;
  if FCaptionViewInfo.Visible and not IsRectEmpty(ACaptionBounds) then
  begin
    ACaptionBounds.Bottom := ACaptionBounds.Top;
    Dec(ACaptionBounds.Top, FCaptionViewInfo.CalculateHeight);
  end;

  APieAreaBounds := cxRectInflate(ContentBounds, -ScaleFactor.Apply(PieAreaOffset));
  with APieAreaBounds do
  begin
    ASize := Max(ScaleFactor.Apply(PieMinSize), Min(Right - Left, Bottom - Top) - 2 * DiagramViewInfo.PieAreaValueCaptionCompensation);
    if ASize > Min(ContentWidth, ContentHeight) then
      SetRectEmpty(APieAreaBounds)
    else
    begin
      Left := (Left + Right - ASize) div 2;
      Right := Left + ASize;
      Top := (Top + Bottom - ASize) div 2;
      Bottom := Top + ASize;
    end;
  end;
end;

function TcxGridChartPieSeriesSiteViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  if FCaptionViewInfo.Visible then
    Inc(Result.Top, FCaptionViewInfo.CalculateHeight);
  if IsRectEmpty(Result) then SetRectEmpty(Result);  // for negative values
end;

function TcxGridChartPieSeriesSiteViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
    Diagram.DoCustomDrawSeriesSite(ACanvas, Self, Result);
end;

function TcxGridChartPieSeriesSiteViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := Params.TextColor;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetCaptionViewInfoClass: TcxGridChartPieSeriesSiteCaptionViewInfoClass;
begin
  Result := TcxGridChartPieSeriesSiteCaptionViewInfo;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := ClientBounds;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartPieSeriesSiteHitTest;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(Series);
end;

function TcxGridChartPieSeriesSiteViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartPieSeriesSitePainter;
end;

function TcxGridChartPieSeriesSiteViewInfo.GetValueViewInfo(AVisibleValueIndex: Integer): TcxGridChartPieDiagramValueViewInfo;
begin
  Result := FDiagramViewInfo.GetValueViewInfo(FSeries.VisibleIndex, AVisibleValueIndex) as TcxGridChartPieDiagramValueViewInfo;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Diagram.Styles.GetViewParams(dsSeriesSites, nil, nil, AParams);
end;

function TcxGridChartPieSeriesSiteViewInfo.GetVisible: Boolean;
begin
  Result := Diagram.SeriesSites;
end;

function TcxGridChartPieSeriesSiteViewInfo.HasCustomDraw: Boolean;
begin
  Result := Diagram.HasCustomDrawSeriesSite;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FDiagramViewInfo.InitHitTest(AHitTest);
  inherited;
  (AHitTest as TcxGridChartPieSeriesSiteHitTest).Series := Series;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.Calculate(const ABounds: TRect);
var
  I: Integer;
begin
  inherited;
  CalculateCaptionAndPieAreaBounds(FCaptionBounds, FPieAreaBounds);
  if FCaptionViewInfo.Visible then
    FCaptionViewInfo.Calculate(CaptionBounds);
  CalculateAngles;
  for I := 0 to DiagramViewInfo.VisibleValueCount - 1 do
    GetValueViewInfo(I).Calculate(PieAreaBounds, Angles[I], Angles[I + 1]);
end;

procedure TcxGridChartPieSeriesSiteViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
  APrevBounds: TRect;
  AOffsetX: Integer;
begin
  APrevBounds := Bounds;
  inherited;
  AOffsetX := Bounds.Left - APrevBounds.Left;
  FPieAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect(FPieAreaBounds, ABounds);
  FCaptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionBounds, ABounds);
  if FCaptionViewInfo.Visible then
    FCaptionViewInfo.RightToLeftConversion(ABounds);
  for I := 0 to DiagramViewInfo.VisibleValueCount - 1 do
    GetValueViewInfo(I).Offset(AOffsetX, 0);
end;

function TcxGridChartPieSeriesSiteViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := FCaptionViewInfo.GetHitTest(P);
  if Result = nil then
    Result := inherited GetHitTest(P);
end;

function TcxGridChartPieSeriesSiteViewInfo.IsPieEmpty: Boolean;
begin
  Result := (Angles = nil) or (Angles[Length(Angles) - 1] = 0);
end;

function TcxGridChartPieSeriesSiteViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and not (ssDouble in AShift) and GridView.IsDesigning then
  begin
    GridView.Controller.DesignController.SelectObject(Series, not (ssShift in AShift));
    Result := True;
  end;
end;

procedure TcxGridChartPieSeriesSiteViewInfo.Paint(ACanvas: TcxCanvas = nil);
begin
  FCaptionViewInfo.Paint(ACanvas);
  inherited;
end;

{ TcxGridChartPieDiagramViewInfo }

constructor TcxGridChartPieDiagramViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo;
  ADiagram: TcxGridChartDiagram);
var
  I: Integer;
begin
  inherited;
  FSeriesSiteViewInfos := TList.Create;
  for I := 0 to SeriesCount - 1 do
    FSeriesSiteViewInfos.Add(GetSeriesSiteViewInfoClass.Create(Self, Series[I]));
end;

destructor TcxGridChartPieDiagramViewInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to SeriesSiteViewInfoCount - 1 do
    SeriesSiteViewInfos[I].Free;
  FSeriesSiteViewInfos.Free;
  inherited;
end;

function TcxGridChartPieDiagramViewInfo.GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
begin
  Result := Diagram.Values.GetCaptionPosition;
end;

function TcxGridChartPieDiagramViewInfo.GetDiagram: TcxGridChartPieDiagram;
begin
  Result := TcxGridChartPieDiagram(inherited Diagram);
end;

function TcxGridChartPieDiagramViewInfo.GetSeriesSiteViewInfo(Index: Integer): TcxGridChartPieSeriesSiteViewInfo;
begin
  Result := TcxGridChartPieSeriesSiteViewInfo(FSeriesSiteViewInfos[Index]);
end;

function TcxGridChartPieDiagramViewInfo.GetSeriesSiteViewInfoCount: Integer;
begin
  Result := FSeriesSiteViewInfos.Count;
end;

function TcxGridChartPieDiagramViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartPieDiagramPainter;
end;

function TcxGridChartPieDiagramViewInfo.GetSeriesSiteViewInfoClass: TcxGridChartPieSeriesSiteViewInfoClass;
begin
  Result := TcxGridChartPieSeriesSiteViewInfo;
end;

class function TcxGridChartPieDiagramViewInfo.GetValueViewInfoClass: TcxGridChartDiagramValueViewInfoClass;
begin
  Result := TcxGridChartPieDiagramValueViewInfo;
end;

function TcxGridChartPieDiagramViewInfo.GetValueViewInfoIndex(ASeriesIndex, AVisibleIndex: Integer): Integer;
begin
  Result := ASeriesIndex * VisibleValueCount + AVisibleIndex;
end;

function TcxGridChartPieDiagramViewInfo.CalculatePieAreaValueCaptionCompensation: Integer;
begin
  case CaptionPosition of
    pdvcpOutsideEnd:
      Result := ScaleFactor.Apply(PieValueCaptionOffset) + CalculateValueCaptionMaxHeight div 2 + CalculateValueCaptionMaxWidth;
    pdvcpOutsideEndWithLeaderLines:
      Result := ScaleFactor.Apply(PieLeaderLineSegment1) + ScaleFactor.Apply(PieLeaderLineSegment2) + CalculateValueCaptionMaxWidth;
  else
    Result := 0;
  end;
end;

function TcxGridChartPieDiagramViewInfo.CalculateSeriesSiteViewInfoBounds(AIndex: Integer): TRect;
var
  AColumnCount, ARowCount, ASiteWidth, ASiteHeight: Integer;
begin
  AColumnCount := Diagram.GetSeriesColumnCount;
  ARowCount := RoundDiv(SeriesSiteViewInfoCount, AColumnCount);

  ASiteWidth := (Bounds.Right - Bounds.Left - (AColumnCount - 1) * ScaleFactor.Apply(PieSeriesSiteOffset)) div AColumnCount;
  ASiteHeight := (Bounds.Bottom - Bounds.Top - (ARowCount - 1) * ScaleFactor.Apply(PieSeriesSiteOffset)) div ARowCount;

  Result := Bounds;
  Inc(Result.Left, AIndex mod AColumnCount * (ASiteWidth + ScaleFactor.Apply(PieSeriesSiteOffset)));
  Result.Right := Result.Left + ASiteWidth;
  Inc(Result.Top, AIndex div AColumnCount * (ASiteHeight + ScaleFactor.Apply(PieSeriesSiteOffset)));
  Result.Bottom := Result.Top + ASiteHeight;
end;

function TcxGridChartPieDiagramViewInfo.CalculateValueCaptionMaxWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ValueViewInfoCount - 1 do
    Result := Max(Result, ValueViewInfos[I].CaptionViewInfo.CalculateWidth);
end;

function TcxGridChartPieDiagramViewInfo.CalculateValueCaptionMaxHeight: Integer;
begin
  if ValueViewInfoCount = 0 then
    Result := 0
  else
    Result := ValueViewInfos[0].CaptionViewInfo.CalculateHeight;
end;

procedure TcxGridChartPieDiagramViewInfo.Calculate(const ABounds: TRect);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to SeriesCount - 1 do
    for J := 0 to VisibleValueCount - 1 do
      CreateValueViewInfo(I, J);
  FPieAreaValueCaptionCompensation := CalculatePieAreaValueCaptionCompensation;
  for I := 0 to SeriesSiteViewInfoCount - 1 do
    SeriesSiteViewInfos[I].Calculate(CalculateSeriesSiteViewInfoBounds(I));
end;

procedure TcxGridChartPieDiagramViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited;
  for I := 0 to SeriesSiteViewInfoCount - 1 do
    SeriesSiteViewInfos[I].RightToLeftConversion(ABounds);
end;

function TcxGridChartPieDiagramViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  Result := inherited GetHitTest(P);
  if Result = nil then
    for I := 0 to SeriesSiteViewInfoCount - 1 do
    begin
      Result := SeriesSiteViewInfos[I].GetHitTest(P);
      if Result <> nil then Break;
    end;
end;

{ TcxGridChartToolBoxItemViewInfo }

constructor TcxGridChartToolBoxItemViewInfo.Create(AContainer: TcxGridChartToolBoxViewInfo;
  AAlignment: TcxGridChartToolBoxItemAlignment);
begin
  inherited Create(AContainer.GridViewInfo);
  FContainer := AContainer;
  FAlignment := AAlignment;
end;

function TcxGridChartToolBoxItemViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartToolBoxItemViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FContainer.GetViewParams(AParams);
end;

{ TcxGridChartToolBoxItemSeparatorViewInfo }

function TcxGridChartToolBoxItemSeparatorViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridChartToolBoxItemSeparatorViewInfo.CalculateWidth: Integer;
begin
  Result := ToolBoxItemSeparatorWidth;
end;

function TcxGridChartToolBoxItemSeparatorViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := nil;
end;

function TcxGridChartToolBoxItemSeparatorViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxItemSeparatorPainter;
end;

procedure TcxGridChartToolBoxItemSeparatorViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  AParams.Color := LookAndFeelPainter.DefaultChartToolBoxItemSeparatorColor;
end;

function TcxGridChartToolBoxItemSeparatorViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

{ TcxGridChartToolBoxDataLevelActiveValueViewInfo }

constructor TcxGridChartToolBoxDataLevelActiveValueViewInfo.Create(AContainer: TcxGridChartToolBoxDataLevelInfoViewInfo);
begin
  inherited Create(AContainer.GridViewInfo);
  FContainer := AContainer;
  FCanShowDropDownWindow := GridView.Controller.CanShowDataLevelActiveValuePopup;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetDataGroup: TcxGridChartDataGroup;
begin
  Result := FContainer.DataLevelObject;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  InflateRect(Result, -ScaleFactor.Apply(cxTextOffset), 0);
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.CalculateHeight: Integer;
begin
  Result := TextHeightWithOffset;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.CalculateWidth: Integer;
begin
  Result := BorderSize[bLeft] + 2 * ScaleFactor.Apply(cxTextOffset) + TextWidthWithOffset;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := FContainer.BorderColor[AIndex];
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetBorders: TcxBorders;
begin
  Result := [bLeft];
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := FContainer.BorderWidth[AIndex];
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartDataLevelActiveValueInfoHitTest;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxDataLevelActiveValuePainter;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetText: string;
begin
  if FContainer.DataLevelObject.HasActiveValue then
    Result := FContainer.DataLevelObject.ActiveValueDisplayText
  else
    Result := cxGetResourceString(@scxGridChartToolBoxDataLevelSelectValue);
end;

procedure TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsDataLevelActiveValueInfo, nil, nil, AParams);
end;

procedure TcxGridChartToolBoxDataLevelActiveValueViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FContainer.InitHitTest(AHitTest);
  inherited;
  TcxGridChartDataLevelActiveValueInfoHitTest(AHitTest).CanDropDown := CanShowDropDownWindow;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.DropDownWindowExists: Boolean;
begin
  Result := CanShowDropDownWindow and GridView.Controller.HasDataLevelActiveValuePopup;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  if CanShowDropDownWindow then
    Result := GridView.Controller.DataLevelActiveValuePopup
  else
    Result := nil;
end;

function TcxGridChartToolBoxDataLevelActiveValueViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := FContainer.Bounds;
  if IsRightToLeftConverted then
    Result.Right := Bounds.Right
  else
    Result.Left := Bounds.Left;
end;

{ TcxGridChartToolBoxDataLevelInfoViewInfo }

constructor TcxGridChartToolBoxDataLevelInfoViewInfo.Create(AContainer: TcxGridChartToolBoxViewInfo;
  AAlignment: TcxGridChartToolBoxItemAlignment; ADataLevel: Integer);
begin
  inherited Create(AContainer, AAlignment);
  FDataLevel := ADataLevel;
  if HasActiveValue then
    FActiveValueViewInfo := GetActiveValueViewInfoClass.Create(Self);
end;

destructor TcxGridChartToolBoxDataLevelInfoViewInfo.Destroy;
begin
  FActiveValueViewInfo.Free;
  inherited;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetActive: Boolean;
begin
  Result := GridView.ActiveDataLevel = FDataLevel;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetDataLevelObject: TcxGridChartDataGroup;
begin
  Result := GridView.DataLevelObjects[FDataLevel];
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.CalculateActiveValueViewInfoBounds: TRect;
begin
  Result := ClientBounds;
  Result.Left := Result.Right - FActiveValueViewInfo.CalculateWidth;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  if FActiveValueViewInfo <> nil then
    if IsRightToLeftConverted then
      Inc(Result.Left, FActiveValueViewInfo.CalculateWidth + 1)
    else
      Dec(Result.Right, FActiveValueViewInfo.CalculateWidth);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.CalculateHeight: Integer;
begin
  Result := TextHeightWithOffset;
  if FActiveValueViewInfo <> nil then
    Result := Max(Result, FActiveValueViewInfo.CalculateHeight);
  Inc(Result, BorderSize[bTop] + BorderSize[bBottom]);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.CalculateWidth: Integer;
begin
  Result := BorderSize[bLeft] + 2 * ScaleFactor.Apply(cxTextOffset) + TextWidthWithOffset + BorderSize[bRight];
  if FActiveValueViewInfo <> nil then
    Inc(Result, FActiveValueViewInfo.CalculateWidth);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetActiveValueViewInfoClass: TcxGridChartToolBoxDataLevelActiveValueViewInfoClass;
begin
  Result := TcxGridChartToolBoxDataLevelActiveValueViewInfo;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := LookAndFeelPainter.DefaultChartToolBoxDataLevelInfoBorderColor;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.ChartToolBoxDataLevelInfoBorderSize;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetDesignObject: TPersistent;
begin
  if DataLevelObject = nil then
    Result := GridView.Categories
  else
    Result := DataLevelObject;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartDataLevelInfoHitTest;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(GetDesignObject);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxDataLevelInfoPainter;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetText: string;
begin
  if DataLevelObject = nil then
    Result := GridView.Categories.GetDisplayText
  else
    Result := DataLevelObject.GetDisplayText;
end;

procedure TcxGridChartToolBoxDataLevelInfoViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataLevelInfoParams(DataLevel, AParams);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.HasActiveValue: Boolean;
begin
  Result := DataLevelObject <> nil;
  if Result then
    if GridView.Controller.MayDataDrillDown(False) then
      Result := GridView.CanActivateDataLevel(DataLevel)
    else
      Result := DataLevelObject.HasActiveValue;
end;

procedure TcxGridChartToolBoxDataLevelInfoViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  with TcxGridChartDataLevelInfoHitTest(AHitTest) do
  begin
    DataLevel := Self.DataLevel;
    DataLevelObjectContainerKind := ckToolBox;
  end;
end;

procedure TcxGridChartToolBoxDataLevelInfoViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  if FActiveValueViewInfo <> nil then
    FActiveValueViewInfo.Calculate(CalculateActiveValueViewInfoBounds);
end;

procedure TcxGridChartToolBoxDataLevelInfoViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited;
  if FActiveValueViewInfo <> nil then
    FActiveValueViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result <> nil) and not GridView.IsDesigning and (FActiveValueViewInfo <> nil) then
  begin
    AHitTest := FActiveValueViewInfo.GetHitTest(P);
    if AHitTest <> nil then Result := AHitTest;
  end;
end;

function TcxGridChartToolBoxDataLevelInfoViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and not (ssDouble in AShift) then
    if GridView.IsDesigning then
    begin
      GridView.Controller.DesignController.SelectObject(GetDesignObject, not (ssShift in AShift));
      Result := True;
    end
    else
      if GridView.Controller.MayDataDrillDown then
        GridView.ActiveDataLevel := DataLevel;
end;

procedure TcxGridChartToolBoxDataLevelInfoViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if FActiveValueViewInfo <> nil then
    FActiveValueViewInfo.Paint(ACanvas);
  inherited;
end;

{ TcxGridChartToolBoxCustomizeButtonViewInfo }

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetChecked: Boolean;
begin
  Result := GridView.Controller.Customization;
end;

procedure TcxGridChartToolBoxCustomizeButtonViewInfo.SetChecked(Value: Boolean);
begin
  GridView.Controller.Customization := Value;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.CalculateHeight: Integer;
begin
  Result := BorderWidth[bTop] + 2 * LookAndFeelPainter.ScaledButtonTextOffset(ScaleFactor) +
    TextHeightWithOffset + BorderWidth[bBottom];
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.CalculateWidth: Integer;
begin
  Result := BorderWidth[bLeft] + 2 * LookAndFeelPainter.ScaledButtonTextOffset(ScaleFactor) +
    TextWidthWithOffset + BorderWidth[bRight];
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

procedure TcxGridChartToolBoxCustomizeButtonViewInfo.Click;
begin
  inherited;
  Checked := not Checked;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetActualState: TcxGridCellState;
begin
  if Checked then
    Result := gcsPressed
  else
    Result := inherited GetActualState;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.ButtonBorderSize;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartCustomizeButtonHitTest;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxCustomizeButtonPainter;
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@scxGridChartToolBoxCustomizeButtonCaption);
end;

function TcxGridChartToolBoxCustomizeButtonViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

{ TcxGridChartToolBoxDiagramSelectorViewInfo }

constructor TcxGridChartToolBoxDiagramSelectorViewInfo.Create(AContainer: TcxGridChartToolBoxViewInfo;
  AAlignment: TcxGridChartToolBoxItemAlignment);
begin
  inherited;
  FCanShowDropDownWindow := CalculateCanShowDropDownWindow;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetDropDownWindowValue: TcxGridChartDiagramSelectorPopup;
begin
  Result := TcxGridChartDiagramSelectorPopup(inherited DropDownWindow);
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.CalculateHeight: Integer;
begin
  Result := Max(ImageHeight, TextHeightWithOffset);
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.CalculateWidth: Integer;
begin
  Result := ImageWidth + ScaleFactor.Apply(ToolBoxDiagramImageOffset) + TextWidthWithOffset;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartDiagramSelectorHitTest;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetImageBounds: TRect;
begin
  Result := ContentBounds;
  Result.Right := Result.Left + ImageWidth;
  AlignRect(Result, 0, ImageHeight, False, cpaCenter);
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetImageHeight: Integer;
begin
  Result := dxGetImageSize(cxGridChartDiagramImages, ScaleFactor).cy;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetImageIndex: Integer;
begin
  if GridView.ActiveDiagram = nil then
    Result := -1
  else
    Result := GridView.ActiveDiagram.ImageIndex;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetImageWidth: Integer;
begin
  Result := dxGetImageSize(cxGridChartDiagramImages, ScaleFactor).cx;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxDiagramSelectorPainter;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetRealImageBounds: TRect;
begin
  Result := GetImageBounds;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ContentBounds);
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetText: string;
begin
  if GridView.ActiveDiagram = nil then
    Result := cxGetResourceString(@scxGridChartNoneDiagramDisplayText)
  else
    Result := GridView.ActiveDiagram.DisplayText;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  Inc(Result.Left, ImageWidth + ScaleFactor.Apply(ToolBoxDiagramImageOffset));
end;

procedure TcxGridChartToolBoxDiagramSelectorViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsDiagramSelector, nil, nil, AParams);
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.HasBackground: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartToolBoxDiagramSelectorViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridChartDiagramSelectorHitTest(AHitTest).CanDropDown := CanShowDropDownWindow;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.CalculateCanShowDropDownWindow: Boolean;
begin
  Result := GridView.AvailableDiagramCount > 1;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.DropDownWindowExists: Boolean;
begin
  Result := CanShowDropDownWindow and GridView.Controller.HasDiagramSelectorPopup;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  if CanShowDropDownWindow then
    Result := GridView.Controller.DiagramSelectorPopup
  else
    Result := nil;
end;

function TcxGridChartToolBoxDiagramSelectorViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := Bounds;
  Dec(Result.Left, DropDownWindow.BorderWidths[bLeft] + DropDownWindow.ImageOffset);
end;

{ TcxGridChartToolBoxViewInfo }

constructor TcxGridChartToolBoxViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo);
begin
  inherited;
  FDataLevelInfos := TList.Create;
  FItems := TList.Create;
  CreateItems;
end;

destructor TcxGridChartToolBoxViewInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I].Free;
  FItems.Free;
  FDataLevelInfos.Free;
  inherited;
end;

function TcxGridChartToolBoxViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfoConnectorCount: Integer;
begin
  Result := DataLevelInfoCount - 1;
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfo(AIndex: Integer): TcxGridChartToolBoxDataLevelInfoViewInfo;
begin
  Result := TcxGridChartToolBoxDataLevelInfoViewInfo(FDataLevelInfos[AIndex]);
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfoCount: Integer;
begin
  Result := FDataLevelInfos.Count;
end;

function TcxGridChartToolBoxViewInfo.GetItem(AIndex: Integer): TcxGridChartToolBoxItemViewInfo;
begin
  Result := TcxGridChartToolBoxItemViewInfo(FItems[AIndex]);
end;

function TcxGridChartToolBoxViewInfo.CalculateHeight: Integer;
var
  I: Integer;
begin
  CalculateParams;
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].CalculateHeight);
  Inc(Result, BorderSize[bTop] + 2 * ScaleFactor.Apply(ToolBoxItemOffset) + BorderSize[bBottom]);
end;

function TcxGridChartToolBoxViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridChartToolBoxViewInfo.CalculateContentBounds: TRect;
begin
  Result := cxRectInflate(inherited CalculateContentBounds, -ScaleFactor.Apply(ToolBoxItemOffset));
end;

procedure TcxGridChartToolBoxViewInfo.CalculateItems;

  function GetItemBounds(AItem: TcxGridChartToolBoxItemViewInfo; var ARestSpace: TRect): TRect;
  begin
    PositionRect(Result, ARestSpace, AItem.CalculateWidth, 0, ScaleFactor.Apply(ToolBoxItemOffset), AItem.Alignment);
    if IsSeparator(AItem) then
    begin
      Result.Top := ClientBounds.Top;
      Result.Bottom := ClientBounds.Bottom;
    end
    else
      AlignRect(Result, 0, AItem.CalculateHeight, False, cpaCenter);
  end;

var
  ARestSpace, R: TRect;
  I, J, ARestWidth, AOffset: Integer;
  AItem: TcxGridChartToolBoxItemViewInfo;
begin
  ARestSpace := ItemsAreaBounds;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Alignment = cppLeft then
      AItem.Calculate(GetItemBounds(AItem, ARestSpace));
  end;
  for I := Count - 1 downto 0 do
  begin
    AItem := Items[I];
    if AItem.Alignment = cppRight then
    begin
      R := GetItemBounds(AItem, ARestSpace);
      ARestWidth := ARestSpace.Right - ARestSpace.Left + ScaleFactor.Apply(ToolBoxItemOffset);
      if ARestWidth < 0 then
      begin
        AOffset := -ARestWidth;
        Inc(ARestSpace.Right, AOffset);
        OffsetRect(R, AOffset, 0);
        for J := I + 1 to Count - 1 do
          if Items[J].Alignment = cppRight then
            Items[J].Offset(AOffset, 0);
      end;
      AItem.Calculate(R);
    end;
  end;
end;

function TcxGridChartToolBoxViewInfo.GetAlignment: TcxGridChartPartAlignment;
begin
  Result := cpaDefault;
end;

function TcxGridChartToolBoxViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridChartToolBoxViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := Params.TextColor;
end;

function TcxGridChartToolBoxViewInfo.GetBorders: TcxBorders;
begin
  if GridView.ToolBox.Border = tbSingle then
    Result := cxBordersAll
  else
    Result := [];
end;

function TcxGridChartToolBoxViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 1;
end;

function TcxGridChartToolBoxViewInfo.GetDataGroupInsertionAreaBounds: TRect;
begin
  Result := ClientBounds;
  if GetFirstSeparator <> nil then
    Result.Right := GetFirstSeparator.Bounds.Left;
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfoConnector(AIndex: Integer): TRect;
begin
  Result := DataLevelInfos[AIndex].Bounds;
  if IsRightToLeftConverted then
  begin
    Result.Right := Result.Left;
    Result.Left := DataLevelInfos[AIndex + 1].Bounds.Right;
  end
  else
  begin
    Result.Left := Result.Right;
    Result.Right := DataLevelInfos[AIndex + 1].Bounds.Left;
  end;
  Result.Top := (Result.Top + Result.Bottom - ToolBoxDataLevelInfoConnectorWidth) div 2;
  Result.Bottom := Result.Top + ToolBoxDataLevelInfoConnectorWidth;
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfoConnectorColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultChartToolBoxDataLevelInfoBorderColor;
end;

function TcxGridChartToolBoxViewInfo.GetFirstSeparator: TcxGridChartToolBoxItemViewInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if IsSeparator(Result) then Exit;
  end;
  Result := nil;
end;

function TcxGridChartToolBoxViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartToolBoxHitTest;
end;

function TcxGridChartToolBoxViewInfo.GetItemsAreaBounds: TRect;
begin
  Result := ContentBounds;
  if Text <> '' then
    Inc(Result.Left, TextWidthWithOffset + ScaleFactor.Apply(ToolBoxItemOffset));
end;

function TcxGridChartToolBoxViewInfo.GetOrientation: TcxGridChartPartOrientation;
begin
  Result := cpoHorizontal;
end;

function TcxGridChartToolBoxViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridChartToolBoxPainter;
end;

function TcxGridChartToolBoxViewInfo.GetPosition: TcxGridChartPartPosition;
begin
  if GridView.ToolBox.Position = tpTop then
    Result := cppTop
  else
    Result := cppBottom;
end;

function TcxGridChartToolBoxViewInfo.GetText: string;
begin
  if IsDataLevelsInfoVisible then
    Result := cxGetResourceString(@scxGridChartToolBoxDataLevels)
  else
    Result := '';
end;

procedure TcxGridChartToolBoxViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsToolBox, nil, nil, AParams);
end;

function TcxGridChartToolBoxViewInfo.GetVisible: Boolean;
begin
  Result := GridView.ToolBox.GetVisible;
end;

function TcxGridChartToolBoxViewInfo.AddItem(AItem: TcxGridChartToolBoxItemViewInfo): TcxGridChartToolBoxItemViewInfo;
begin
  Result := AItem;
  FItems.Add(AItem);
end;

procedure TcxGridChartToolBoxViewInfo.AddSeparator(AAlignment: TcxGridChartToolBoxItemAlignment);
begin
  if Count <> 0 then
    AddItem(GetItemSeparatorClass.Create(Self, AAlignment));
end;

procedure TcxGridChartToolBoxViewInfo.CreateItems;
var
  I: Integer;
begin
  if IsDataLevelsInfoVisible then
    for I := 0 to GridView.DataLevelCount - 1 do
      FDataLevelInfos.Add(AddItem(GetDataLevelInfoClass.Create(Self, cppLeft, I)));
  if IsCustomizeButtonVisible then
  begin
    AddSeparator(cppRight);
    FCustomizeButton := GetCustomizeButtonClass.Create(Self, cppRight);
    AddItem(FCustomizeButton);
  end;
  if IsDiagramSelectorVisible then
  begin
    AddSeparator(cppRight);
    FDiagramSelector := GetDiagramSelectorClass.Create(Self, cppRight);
    AddItem(FDiagramSelector);
  end;
end;

function TcxGridChartToolBoxViewInfo.GetCustomizeButtonClass: TcxGridChartToolBoxCustomizeButtonViewInfoClass;
begin
  Result := TcxGridChartToolBoxCustomizeButtonViewInfo;
end;

function TcxGridChartToolBoxViewInfo.GetDataLevelInfoClass: TcxGridChartToolBoxDataLevelInfoViewInfoClass;
begin
  Result := TcxGridChartToolBoxDataLevelInfoViewInfo;
end;

function TcxGridChartToolBoxViewInfo.GetDiagramSelectorClass: TcxGridChartToolBoxDiagramSelectorViewInfoClass;
begin
  Result := TcxGridChartToolBoxDiagramSelectorViewInfo;
end;

function TcxGridChartToolBoxViewInfo.GetItemSeparatorClass: TcxGridChartToolBoxItemSeparatorViewInfoClass;
begin
  Result := TcxGridChartToolBoxItemSeparatorViewInfo;
end;

function TcxGridChartToolBoxViewInfo.IsCustomizeButtonVisible: Boolean;
begin
  Result := not GridViewInfo.IsImage and GridView.ToolBox.CustomizeButton;
end;

function TcxGridChartToolBoxViewInfo.IsDataLevelsInfoVisible: Boolean;
begin
  Result := GridView.ToolBox.GetDataLevelsInfoVisible;
end;

function TcxGridChartToolBoxViewInfo.IsDiagramSelectorVisible: Boolean;
begin
  Result := not GridViewInfo.IsImage and GridView.ToolBox.DiagramSelector;
end;

function TcxGridChartToolBoxViewInfo.IsSeparator(AItem: TcxGridChartToolBoxItemViewInfo): Boolean;
begin
  Result := AItem is GetItemSeparatorClass;
end;

procedure TcxGridChartToolBoxViewInfo.Calculate(ALeftBound, ATopBound, AWidth,
  AHeight: Integer);
begin
  inherited;
  CalculateItems;
end;

procedure TcxGridChartToolBoxViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridChartToolBoxViewInfo.GetDataGroupInsertionBounds(AIndex: Integer): TRect;
begin
  if DataLevelInfoCount = 0 then
    Result := ItemsAreaBounds
  else
  begin
    Result := DataLevelInfos[AIndex].Bounds;
    Dec(Result.Left, MulDiv(ScaleFactor.Apply(ToolBoxItemOffset), 1, 2));
  end;
end;

function TcxGridChartToolBoxViewInfo.GetDataGroupInsertionIndex(const P: TPoint): Integer;
var
  R: TRect;
begin
  Result := -1;
  if not PtInRect(GetDataGroupInsertionAreaBounds, P) then Exit;
  for Result := 0 to DataLevelInfoCount - 1 do
    if DataLevelInfos[Result].DataLevelObject <> nil then
    begin
      R := DataLevelInfos[Result].Bounds;
      if P.X < GetRangeCenter(R.Left, R.Right) then Exit;
    end;
  Result := GridView.VisibleDataGroupCount;
end;

function TcxGridChartToolBoxViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if Result <> nil then
    for I := 0 to Count - 1 do
    begin
      AHitTest := Items[I].GetHitTest(P);
      if AHitTest <> nil then
      begin
        Result := AHitTest;
        Break;
      end;
    end;
end;

procedure TcxGridChartToolBoxViewInfo.InvalidateCustomizeButton;
begin
  if CustomizeButton <> nil then
    CustomizeButton.Invalidate;
end;

{ TcxGridChartTitleViewInfo }

function TcxGridChartTitleViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridChartTitleHitTest;
end;

procedure TcxGridChartTitleViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsTitle, nil, nil, AParams);
end;

{ TcxGridChartViewInfo }

function TcxGridChartViewInfo.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartViewInfo.CreateViewInfos;
var
  ADiagram: TcxGridChartDiagram;
begin
  inherited;
  ADiagram := GridView.ActiveDiagram;
  if ADiagram <> nil then
  begin
    FDiagramViewInfo := ADiagram.GetViewInfoClass.Create(Self, ADiagram);
    FLegendViewInfo := ADiagram.GetLegendViewInfoClass.Create(Self, ADiagram);
  end;
  FTitleViewInfo := GetTitleViewInfoClass.Create(Self, GridView.Title);
  FToolBoxViewInfo := GetToolBoxViewInfoClass.Create(Self);
end;

procedure TcxGridChartViewInfo.DestroyViewInfos(AIsRecreating: Boolean);
begin
  FreeAndNil(FToolBoxViewInfo);
  FreeAndNil(FTitleViewInfo);
  FreeAndNil(FLegendViewInfo);
  FreeAndNil(FDiagramViewInfo);
  inherited;
end;

function TcxGridChartViewInfo.GetTitleViewInfoClass: TcxGridChartTitleViewInfoClass;
begin
  Result := TcxGridChartTitleViewInfo;
end;

function TcxGridChartViewInfo.GetToolBoxViewInfoClass: TcxGridChartToolBoxViewInfoClass;
begin
  Result := TcxGridChartToolBoxViewInfo;
end;

procedure TcxGridChartViewInfo.Calculate;
var
  ADiagramBounds, ALegendBounds, ATitleBounds, AToolBoxBounds: TRect;
begin
  RecreateViewInfos;
  inherited;
  CalculatePartsBounds(ADiagramBounds, ALegendBounds, ATitleBounds, AToolBoxBounds);
  if FDiagramViewInfo <> nil then
    FDiagramViewInfo.Calculate(ADiagramBounds);
  if FLegendViewInfo <> nil then
    FLegendViewInfo.Calculate(ALegendBounds);
  FTitleViewInfo.Calculate(ATitleBounds);
  if FToolBoxViewInfo.Visible then
    FToolBoxViewInfo.Calculate(AToolBoxBounds);
end;

function TcxGridChartViewInfo.CalculateClientBounds: TRect;
begin
  Result := cxRectInflate(inherited CalculateClientBounds, ScaleFactor.Apply(-ChartPartOffset));
end;

procedure TcxGridChartViewInfo.CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean);
begin
  AHeight := cxMaxRectSize;
  AFullyVisible := False;
  inherited;
end;

procedure TcxGridChartViewInfo.CalculatePartBounds(
  APart: TcxCustomGridChartPartViewInfo; var ABounds, APartBounds: TRect);
begin
  if APart.Visible then
  begin
    PositionRect(APartBounds, ABounds, APart.Width, APart.Height, ScaleFactor.Apply(ChartPartOffset), APart.Position);
    AlignRect(APartBounds, APart.Width, APart.Height, APart.Position in [cppTop, cppBottom], APart.Alignment);
  end
  else
    SetRectEmpty(APartBounds);
end;

procedure TcxGridChartViewInfo.CalculatePartsBounds(out ADiagramBounds, ALegendBounds,
  ATitleBounds, AToolBoxBounds: TRect);
begin
  ADiagramBounds := ClientBounds;
  CalculatePartBounds(FToolBoxViewInfo, ADiagramBounds, AToolBoxBounds);
  CalculatePartBounds(FTitleViewInfo, ADiagramBounds, ATitleBounds);
  if FLegendViewInfo <> nil then
    CalculatePartBounds(FLegendViewInfo, ADiagramBounds, ALegendBounds);
end;

function TcxGridChartViewInfo.DoGetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if FDiagramViewInfo <> nil then
    Result := FDiagramViewInfo.GetHitTest(P)
  else
    Result := nil;
  if Result = nil then
  begin
    Result := FToolBoxViewInfo.GetHitTest(P);
    if Result = nil then
    begin
      if FLegendViewInfo <> nil then
        Result := FLegendViewInfo.GetHitTest(P);
      if Result = nil then
      begin
        Result := FTitleViewInfo.GetHitTest(P);
        if Result = nil then
          Result := inherited DoGetHitTest(P);
      end;
    end;
  end;
end;

procedure TcxGridChartViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited;
  if FDiagramViewInfo <> nil then
    FDiagramViewInfo.RightToLeftConversion(ABounds);
  if FLegendViewInfo <> nil then
    FLegendViewInfo.RightToLeftConversion(ABounds);
  FTitleViewInfo.RightToLeftConversion(ABounds);
  if FToolBoxViewInfo.Visible then
    FToolBoxViewInfo.RightToLeftConversion(ABounds);
end;

{ TcxCustomGridChartTitle }

procedure TcxCustomGridChartTitle.SetAlignment(Value: TcxGridChartPartAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridChartTitle.SetPosition(Value: TcxGridChartPartPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridChartTitle.SetText(Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridChartTitle.GetStoredProperties(AProperties: TStrings);
begin
  AProperties.Add('Title.Alignment');
  AProperties.Add('Title.Position');
  inherited;
end;

procedure TcxCustomGridChartTitle.GetStoredPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'Title.Alignment' then
    AValue := Variant(Alignment)
  else
    if AName = 'Title.Position' then
      AValue := Variant(Position);
  inherited;
end;

procedure TcxCustomGridChartTitle.SetStoredPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'Title.Alignment' then
    Alignment :=  TcxGridChartPartAlignment((AValue))
  else
    if AName = 'Title.Position' then
      Position := TcxGridChartPartPosition((AValue));
  inherited;
end;

function TcxCustomGridChartTitle.GetDefaultAlignment: TcxGridChartPartAlignment;
begin
  Result := cpaCenter;
end;

function TcxCustomGridChartTitle.GetDefaultOrientation: TcxGridChartPartOrientation;
begin
  if GetPosition in [cppTop, cppBottom] then
    Result := cpoHorizontal
  else
    Result := cpoVertical;
end;

function TcxCustomGridChartTitle.GetDefaultText: string;
begin
  Result := '';
end;

procedure TcxCustomGridChartTitle.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridChartTitle then
    with TcxCustomGridChartTitle(Source) do
    begin
      Self.Alignment := Alignment;
      Self.Position := Position;
      Self.Text := Text;
    end;
  inherited;
end;

function TcxCustomGridChartTitle.GetAlignment: TcxGridChartPartAlignment;
begin
  Result := FAlignment;
  if Result = cpaDefault then
    Result := GetDefaultAlignment;
end;

function TcxCustomGridChartTitle.GetOrientation: TcxGridChartPartOrientation;
begin
  Result := GetDefaultOrientation;
end;

function TcxCustomGridChartTitle.GetPosition: TcxGridChartPartPosition;
begin
  if GetText = '' then
    Result := cppNone
  else
  begin
    Result := FPosition;
    if Result = cppDefault then
      Result := GetDefaultPosition;
  end;
end;

function TcxCustomGridChartTitle.GetText: string;
begin
  Result := FText;
  if Result = '' then
    Result := GetDefaultText;
end;

{ TcxGridChartLegend }

procedure TcxGridChartLegend.SetAlignment(Value: TcxGridChartPartAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartLegend.SetBorder(Value: TcxGridChartLegendBorder);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartLegend.SetKeyBorder(Value: TcxGridChartLegendBorder);
begin
  if FKeyBorder <> Value then
  begin
    FKeyBorder := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartLegend.SetOrientation(Value: TcxGridChartPartOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartLegend.SetPosition(Value: TcxGridChartPartPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartLegend.GetStoredProperties(AProperties: TStrings);
begin
  AProperties.Add('Legend.Alignment');
  AProperties.Add('Legend.Border');
  AProperties.Add('Legend.KeyBorder');
  AProperties.Add('Legend.Orientation');
  AProperties.Add('Legend.Position');
  inherited;
end;

procedure TcxGridChartLegend.GetStoredPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'Legend.Alignment' then
    AValue := Variant(Alignment)
  else
    if AName = 'Legend.Border' then
      AValue := Variant(Border)
    else
      if AName = 'Legend.KeyBorder' then
        AValue := Variant(KeyBorder)
      else
        if AName = 'Legend.Orientation' then
          AValue := Variant(Orientation)
        else
          if AName = 'Legend.Position' then
            AValue := Variant(Position);
  inherited;
end;

procedure TcxGridChartLegend.SetStoredPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'Legend.Alignment' then
    Alignment := TcxGridChartPartAlignment((AValue))
  else
    if AName = 'Legend.Border' then
      Border := TcxGridChartLegendBorder((AValue))
    else
      if AName = 'Legend.KeyBorder' then
        KeyBorder := TcxGridChartLegendBorder((AValue))
      else
        if AName = 'Legend.Orientation' then
          Orientation := TcxGridChartPartOrientation((AValue))
        else
          if AName = 'Legend.Position' then
            Position := TcxGridChartPartPosition((AValue));
  inherited;
end;

function TcxGridChartLegend.GetDefaultAlignment: TcxGridChartPartAlignment;
begin
  Result := cpaCenter;
end;

function TcxGridChartLegend.GetDefaultBorder: TcxGridChartLegendBorder;
begin
  Result := lbNone;
end;

function TcxGridChartLegend.GetDefaultKeyBorder: TcxGridChartLegendBorder;
begin
  Result := lbSingle;
end;

function TcxGridChartLegend.GetDefaultOrientation(APosition: TcxGridChartPartPosition): TcxGridChartPartOrientation;
begin
  if APosition in [cppTop, cppBottom] then
    Result := cpoHorizontal
  else
    Result := cpoVertical;
end;

function TcxGridChartLegend.GetDefaultPosition: TcxGridChartPartPosition;
begin
  Result := cppRight;
end;

procedure TcxGridChartLegend.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartLegend then
    with TcxGridChartLegend(Source) do
    begin
      Self.Alignment := Alignment;
      Self.Border := Border;
      Self.KeyBorder := KeyBorder;
      Self.Orientation := Orientation;
      Self.Position := Position;
    end;
  inherited;
end;

function TcxGridChartLegend.GetAlignment: TcxGridChartPartAlignment;
begin
  Result := FAlignment;
  if Result = cpaDefault then
    if Parent = nil then
      Result := GetDefaultAlignment
    else
      Result := Parent.GetAlignment;
end;

function TcxGridChartLegend.GetBorder: TcxGridChartLegendBorder;
begin
  Result := FBorder;
  if Result = lbDefault then
    if Parent = nil then
      Result := GetDefaultBorder
    else
      Result := Parent.GetBorder;
end;

function TcxGridChartLegend.GetKeyBorder: TcxGridChartLegendBorder;
begin
  Result := FKeyBorder;
  if Result = lbDefault then
    if Parent = nil then
      Result := GetDefaultKeyBorder
    else
      Result := Parent.GetKeyBorder;
end;

function TcxGridChartLegend.GetOrientation(APosition: TcxGridChartPartPosition = cppDefault): TcxGridChartPartOrientation;
begin
  Result := FOrientation;
  if Result = cpoDefault then
    if Parent = nil then
      Result := GetDefaultOrientation(APosition)
    else
    begin
      if APosition = cppDefault then
        APosition := GetPosition;
      Result := Parent.GetOrientation(APosition);
    end;
end;

function TcxGridChartLegend.GetPosition: TcxGridChartPartPosition;
begin
  Result := FPosition;
  if Result = cppDefault then
    if Parent = nil then
      Result := GetDefaultPosition
    else
      Result := Parent.GetPosition;
end;

{ TcxCustomGridChartDiagramOptions }

constructor TcxCustomGridChartDiagramOptions.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
end;

function TcxCustomGridChartDiagramOptions.GetGridView: TcxGridChartView;
begin
  Result := FDiagram.GridView;
end;

function TcxCustomGridChartDiagramOptions.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FDiagram.GridView.LookAndFeelPainter;
end;

procedure TcxCustomGridChartDiagramOptions.Changed(AChange: TcxGridDiagramChange = dcLayout);
begin
  FDiagram.Changed(AChange);
end;

procedure TcxCustomGridChartDiagramOptions.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TcxCustomGridChartDiagramOptions.Assign(Source: TPersistent);
begin
  if not (Source is TcxCustomGridChartDiagramOptions) then
    inherited;
end;

{ TcxGridChartDiagramValueData }

type
  TcxGridChartDiagramValueData = class
  public
    ValueIndex: Integer;
    VisibleSeriesIndex: Integer;
  end;

{ TcxGridChartDiagramStyles }

constructor TcxGridChartDiagramStyles.Create(AOwner: TPersistent);
begin
  inherited;
  BitmapInViewParams := True;
end;

function TcxGridChartDiagramStyles.GetDiagram: TcxGridChartDiagram;
begin
  Result := TcxGridChartDiagram(Owner);
end;

function TcxGridChartDiagramStyles.GetGridViewValue: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartDiagramStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
var
  P: TcxViewParams;
begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      dsLegend:
        GridView.Styles.GetViewParams(vsLegend, AData, nil, AParams);
      dsValueCaptions:
        begin
          TcxGridChartSeries(AData).Styles.GetValueParams(-1, False, P);
          TextColor := P.Color;
          Color := DefaultContentColor;
        end;
      dsValues:
        with TcxGridChartDiagramValueData(AData) do
        begin
          if VisibleSeriesIndex <> -1 then
            GridView.VisibleSeries[VisibleSeriesIndex].Styles.GetValueParams(ValueIndex, GetVaryColorsByCategory, AParams)
          else
            Color := GetDefaultValueColor(ValueIndex);
          TextColor := dxGetDarkerColor(Color, 75);
        end;
    end;
end;

function TcxGridChartDiagramStyles.GetGridView: TcxCustomGridView;
begin
  Result := Diagram.GridView;
end;

procedure TcxGridChartDiagramStyles.GetBackgroundParams(out AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsBackground, nil, nil, AParams);
end;

procedure TcxGridChartDiagramStyles.GetValueParams(AVisibleSeriesIndex, AValueIndex: Integer; out AParams: TcxViewParams);
var
  AData: TcxGridChartDiagramValueData;
begin
  AData := TcxGridChartDiagramValueData.Create;
  try
    AData.VisibleSeriesIndex := AVisibleSeriesIndex;
    AData.ValueIndex := AValueIndex;
    GetViewParams(dsValues, AData, nil, AParams);
  finally
    AData.Free;
  end;
end;

procedure TcxGridChartDiagramStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartDiagramStyles then
    with TcxGridChartDiagramStyles(Source) do
    begin
      Self.Legend := Legend;
      Self.ValueCaptions := ValueCaptions;
      Self.Values := Values;
    end;
end;

{ TcxGridChartDiagram }

constructor TcxGridChartDiagram.Create(AGridView: TcxGridChartView);
begin
  inherited Create;
  FEnabled := True;
  CreateSubObjects(AGridView);
  AGridView.AddDiagram(Self);
end;

destructor TcxGridChartDiagram.Destroy;
begin
  FGridView.RemoveDiagram(Self);
  DestroySubObjects;
  inherited;
end;

function TcxGridChartDiagram.GetActive: Boolean;
begin
  Result := FGridView.ActiveDiagram = Self;
end;

function TcxGridChartDiagram.GetID: string;
begin
  Result := ClassName;
  Delete(Result, 1, Length('TcxGridChart'));
  Insert(' ', Result, Pos('Diagram', Result));
end;

procedure TcxGridChartDiagram.SetActive(Value: Boolean);
begin
  if Value then
    FGridView.ActiveDiagram := Self;
end;

procedure TcxGridChartDiagram.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FGridView.RefreshAvailableDiagramList;
    if IsAvailable then
      if FGridView.ActiveDiagram = nil then
        Active := True
      else
    else
      if Active then
        FGridView.ActiveDiagram := nil;
    Changed(dcLayout);
  end;
end;

procedure TcxGridChartDiagram.SetLegend(Value: TcxGridChartLegend);
begin
  FLegend.Assign(Value);
end;

procedure TcxGridChartDiagram.SetStyles(Value: TcxGridChartDiagramStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxGridChartDiagram.SetOnCustomDrawLegend(Value: TcxGridChartDiagramLegendCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawLegend, Value) then
  begin
    FOnCustomDrawLegend := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartDiagram.SetOnCustomDrawLegendItem(Value: TcxGridChartDiagramLegendItemCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawLegendItem, Value) then
  begin
    FOnCustomDrawLegendItem := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartDiagram.SetOnCustomDrawValue(Value: TcxGridChartDiagramValueCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawValue, Value) then
  begin
    FOnCustomDrawValue := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartDiagram.SetOnCustomDrawValueCaption(
  Value: TcxGridChartDiagramValueCaptionCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawValueCaption, Value) then
  begin
    FOnCustomDrawValueCaption := Value;
    Changed(dcProperty);
  end;
end;

function TcxGridChartDiagram.GetOwner: TPersistent;
begin
  Result := FGridView;
end;

procedure TcxGridChartDiagram.CreateSubObjects(AGridView: TcxGridChartView);
begin
  FLegend := AGridView.GetLegendClass.Create(AGridView);
  FLegend.Parent := AGridView.Legend;
  FStyles := GetStylesClass.Create(Self);
end;

procedure TcxGridChartDiagram.DestroySubObjects;
begin
  FreeAndNil(FStyles);
  FreeAndNil(FLegend);
end;

procedure TcxGridChartDiagram.SetGridView(Value: TcxGridChartView);
begin
  FGridView := Value;
end;

function TcxGridChartDiagram.GetStylesClass: TcxGridChartDiagramStylesClass;
begin
  Result := TcxGridChartDiagramStyles;
end;

procedure TcxGridChartDiagram.Changed(AChange: TcxGridDiagramChange = dcLayout);
begin
  //if FGridView <> nil then
    FGridView.Changed(TcxGridViewChangeKind(AChange));
end;

procedure TcxGridChartDiagram.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TcxGridChartDiagram.GetDisplayText: string;
begin
  Result := ID;
end;

function TcxGridChartDiagram.GetImageIndex: Integer;
begin
  Result := -1;
end;

function TcxGridChartDiagram.HorizontalPaging: Boolean;
begin
  Result := True;
end;

function TcxGridChartDiagram.PagingInOppositeDirection: Boolean;
begin
  Result := False;
end;

function TcxGridChartDiagram.SupportsPaging: Boolean;
begin
  Result := False;
end;

function TcxGridChartDiagram.SupportsValueHotTrack: Boolean;
begin
  Result := True;
end;

procedure TcxGridChartDiagram.DoCustomDrawLegend(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawLegend then
    FOnCustomDrawLegend(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridChartDiagram.DoCustomDrawLegendItem(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawLegendItem then
    FOnCustomDrawLegendItem(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridChartDiagram.DoCustomDrawValue(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawValue then
    FOnCustomDrawValue(Self, ACanvas, AViewInfo, ADone);
end;

function TcxGridChartDiagram.HasCustomDrawLegend: Boolean;
begin
  Result := Assigned(FOnCustomDrawLegend);
end;

function TcxGridChartDiagram.HasCustomDrawLegendItem: Boolean;
begin
  Result := Assigned(FOnCustomDrawLegendItem);
end;

function TcxGridChartDiagram.HasCustomDrawValue: Boolean;
begin
  Result := Assigned(FOnCustomDrawValue);
end;

function TcxGridChartDiagram.HasCustomDrawValueCaption: Boolean;
begin
  Result := Assigned(FOnCustomDrawValueCaption)
end;

procedure TcxGridChartDiagram.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartDiagram then
    with TcxGridChartDiagram(Source) do
    begin
      Self.Enabled := Enabled;  // must be assigned before Active
      Self.Active := Active;
      Self.Legend := Legend;
      Self.Styles := Styles;
      Self.OnCustomDrawLegend := OnCustomDrawLegend;
      Self.OnCustomDrawLegendItem := OnCustomDrawLegendItem;
      Self.OnCustomDrawValue := OnCustomDrawValue;
      Self.OnCustomDrawValueCaption := OnCustomDrawValueCaption;
    end
  else
    inherited;
end;

function TcxGridChartDiagram.GetNamePath: string;
begin
  Result := FGridView.GetDiagramNamePath(Self);
end;

function TcxGridChartDiagram.GetStackedValueCaption(
  ASeries: TcxGridChartSeries; AValueIndex: Integer; APercentage: Boolean): string;
var
  AValue: Variant;
  AFormat: string;
begin
  if APercentage then
  begin
    AFormat := ASeries.ValueCaptionFormat;
    AValue := TcxGridChartHistogramViewInfo(GridView.ViewInfo.DiagramViewInfo).GetStackedValue(ASeries.VisibleIndex, AValueIndex);
//    AValue := GridView.ViewInfo.DiagramViewInfo.GetSValue(ASeries.VisibleIndex, AValueIndex);
    if VarIsNull(AValue) then
      AValue := 0;
    if AFormat = '' then
      AFormat := '0%';
    Result := FormatFloat(AFormat, AValue);
  end
  else
    Result := ASeries.VisibleDisplayTexts[AValueIndex];
end;

function TcxGridChartDiagram.GetValueCaption(ASeries: TcxGridChartSeries;
  AValueIndex: Integer): string;
begin
  Result := ASeries.VisibleDisplayTexts[AValueIndex];
end;

function TcxGridChartDiagram.IsAvailable: Boolean;
begin
  Result := FEnabled;
end;

{ TcxGridChartHistogramAxisTitle }

constructor TcxGridChartHistogramAxisTitle.Create(AAxis: TcxGridChartHistogramAxis);
begin
  FAxis := AAxis;
  inherited Create(nil);
end;

function TcxGridChartHistogramAxisTitle.GetDefaultPosition: TcxGridChartPartPosition;
begin
  Result := FAxis.GetPosition;
end;

function TcxGridChartHistogramAxisTitle.GetDefaultText: string;
begin
  Result := inherited GetDefaultText;
  if Result = '' then
    Result := FAxis.GetTitleDefaultText;
end;

function TcxGridChartHistogramAxisTitle.GetGridViewValue: TcxCustomGridView;
begin
  Result := FAxis.GridView;
end;

{ TcxGridChartHistogramAxis }

constructor TcxGridChartHistogramAxis.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FGridLines := True;
  FTickMarkKind := tmkOutside;
  FTickMarkLabels := True;
  FTitle := GetTitleClass.Create(Self);
  FVisible := True;
end;

destructor TcxGridChartHistogramAxis.Destroy;
begin
  FreeAndNil(FTitle);
  inherited;
end;

function TcxGridChartHistogramAxis.GetDiagram: TcxGridChartHistogram;
begin
  Result := TcxGridChartHistogram(inherited Diagram);
end;

procedure TcxGridChartHistogramAxis.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxis.SetTickMarkKind(Value: TcxGridChartHistogramTickMarkKind);
begin
  if FTickMarkKind <> Value then
  begin
    FTickMarkKind := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxis.SetTickMarkLabels(Value: Boolean);
begin
  if FTickMarkLabels <> Value then
  begin
    FTickMarkLabels := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxis.SetTitle(Value: TcxGridChartHistogramAxisTitle);
begin
  FTitle.Assign(Value);
end;

procedure TcxGridChartHistogramAxis.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

function TcxGridChartHistogramAxis.GetTitleClass: TcxGridChartHistogramAxisTitleClass;
begin
  Result := TcxGridChartHistogramAxisTitle;
end;

function TcxGridChartHistogramAxis.GetTitleDefaultText: string;
begin
  Result := '';
end;

procedure TcxGridChartHistogramAxis.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartHistogramAxis then
    with TcxGridChartHistogramAxis(Source) do
    begin
      Self.GridLines := GridLines;
      Self.TickMarkKind := TickMarkKind;
      Self.TickMarkLabels := TickMarkLabels;
      Self.Title := Title;
      Self.Visible := Visible;
    end;
  inherited;
end;

{ TcxGridChartHistogramAxisCategory }

constructor TcxGridChartHistogramAxisCategory.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FValueAxisBetweenCategories := GetDefaultValueAxisBetweenCategories;
end;

procedure TcxGridChartHistogramAxisCategory.SetCategoriesInReverseOrder(Value: Boolean);
begin
  if FCategoriesInReverseOrder <> Value then
  begin
    FCategoriesInReverseOrder := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxisCategory.SetValueAxisAtMaxCategory(Value: Boolean);
begin
  if FValueAxisAtMaxCategory <> Value then
  begin
    FValueAxisAtMaxCategory := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxisCategory.SetValueAxisBetweenCategories(Value: Boolean);
begin
  if FValueAxisBetweenCategories <> Value then
  begin
    FValueAxisBetweenCategories := Value;
    Changed;
  end;
end;

function TcxGridChartHistogramAxisCategory.GetDefaultValueAxisBetweenCategories: Boolean;
begin
  Result := True;
end;

function TcxGridChartHistogramAxisCategory.GetTitleDefaultText: string;
begin
  Result := inherited GetTitleDefaultText;
  if Result = '' then
    if GridView.ViewData.IsSummaryLevel then
      Result := GridView.ActiveDataGroup.DisplayText
    else
      Result := GridView.Categories.DisplayText;
end;

procedure TcxGridChartHistogramAxisCategory.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartHistogramAxisCategory then
    with TcxGridChartHistogramAxisCategory(Source) do
    begin
      Self.CategoriesInReverseOrder := CategoriesInReverseOrder;
      Self.ValueAxisAtMaxCategory := ValueAxisAtMaxCategory;
      Self.ValueAxisBetweenCategories := ValueAxisBetweenCategories;
    end;
  inherited;
end;

function TcxGridChartHistogramAxisCategory.GetPosition: TcxGridChartAxisPosition;
begin
  Result := Diagram.GetCategoryAxisPosition;
end;

{ TcxGridChartHistogramAxisValue }

procedure TcxGridChartHistogramAxisValue.SetMaxValue(const Value: Extended);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    if not GridView.IsLoading and (FMaxValue <> 0) then
      MinMaxValues := mmvCustom;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxisValue.SetMinMaxValues(Value: TcxGridChartHistogramMinMaxValues);
begin
  if FMinMaxValues <> Value then
  begin
    FMinMaxValues := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxisValue.SetMinValue(const Value: Extended);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    if not GridView.IsLoading and (FMinValue <> 0) then
      MinMaxValues := mmvCustom;
    Changed;
  end;
end;

procedure TcxGridChartHistogramAxisValue.SetTickMarkLabelFormat(const Value: string);
begin
  if FTickMarkLabelFormat <> Value then
  begin
    FTickMarkLabelFormat := Value;
    Changed;
  end;
end;

function TcxGridChartHistogramAxisValue.GetTitleDefaultText: string;
begin
  Result := inherited GetTitleDefaultText;
  if (Result = '') and (GridView.VisibleSeriesCount = 1) then
    Result := GridView.VisibleSeries[0].DisplayText;
end;

procedure TcxGridChartHistogramAxisValue.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartHistogramAxisValue then
    with TcxGridChartHistogramAxisValue(Source) do
    begin
      Self.MinValue := MinValue;
      Self.MaxValue := MaxValue;
      Self.MinMaxValues := MinMaxValues;  // should be after Min/MaxValue
      Self.TickMarkLabelFormat := TickMarkLabelFormat;
    end;
  inherited;
end;

function TcxGridChartHistogramAxisValue.GetPosition: TcxGridChartAxisPosition;
begin
  Result := Diagram.GetValueAxisPosition;
end;

function TcxGridChartHistogramAxisValue.GetTickMarkLabel(const ATickMarkValue: Variant): string;
var
  ATickMarkLabelFormat: string;
begin
  ATickMarkLabelFormat := GetTickMarkLabelFormat(False);
  if ATickMarkLabelFormat = '' then
    case GridView.VisibleSeriesCount of
      0: Result := ATickMarkValue;
      1: Result := GridView.VisibleSeries[0].GetValueDisplayText(ATickMarkValue);
    else
      ATickMarkLabelFormat := GetTickMarkLabelFormat(True);
      if ATickMarkLabelFormat = '' then
        Result := GridView.VisibleSeries[0].DataBinding.GetValueDisplayText(ATickMarkValue)
      else
        Result := FormatFloat(ATickMarkLabelFormat, ATickMarkValue);
    end
  else
    Result := FormatFloat(ATickMarkLabelFormat, ATickMarkValue);
end;

function TcxGridChartHistogramAxisValue.GetTickMarkLabelFormat(AConsiderSeriesFormat: Boolean): string;
var
  I: Integer;
  AValueCaptionFormat: string;
begin
  if Diagram.Values.Stacking = vs100Percent then
    Result := cxGetResourceString(@scxGridChartPercentValueTickMarkLabelFormat)
  else
  begin
    Result := TickMarkLabelFormat;
    if (Result = '') and AConsiderSeriesFormat then
      for I := 0 to GridView.VisibleSeriesCount - 1 do
      begin
        AValueCaptionFormat := GridView.VisibleSeries[I].ValueCaptionFormat;
        if (I <> 0) and (AValueCaptionFormat <> Result) then
        begin
          Result := '';
          Break;
        end;
        Result := AValueCaptionFormat;
      end;
  end;
end;

{ TcxGridChartHistogramStyles }

procedure TcxGridChartHistogramStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      dsAxis:
        begin
          Color := DefaultChartHistogramAxisColor;
          TextColor := DefaultContentTextColor;
        end;
      dsCategoryAxis, dsValueAxis:
        GetViewParams(dsAxis, nil, nil, AParams);
      dsAxisTitle:
        TextColor := DefaultContentTextColor;
      dsCategoryAxisTitle, dsValueAxisTitle:
        GetViewParams(dsAxisTitle, nil, nil, AParams);
      dsGridLines:
        begin
          Color := DefaultChartHistogramGridLineColor;
          TextColor := Color;
        end;
      dsCategoryGridLines, dsValueGridLines:
        GetViewParams(dsGridLines, nil, nil, AParams);
      dsPlot:
        Color := DefaultChartHistogramPlotColor;
    end;
end;

function TcxGridChartHistogramStyles.GetVaryColorsByCategory: Boolean;
begin
  Result := TcxGridChartColumnDiagram(Diagram).Values.GetVaryColorsByCategory;
end;

procedure TcxGridChartHistogramStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartHistogramStyles then
    with TcxGridChartHistogramStyles(Source) do
    begin
      Self.Axis := Axis;
      Self.AxisTitle := AxisTitle;
      Self.CategoryAxis := CategoryAxis;
      Self.CategoryAxisTitle := CategoryAxisTitle;
      Self.CategoryGridLines := CategoryGridLines;
      Self.GridLines := GridLines;
      Self.Plot := Plot;
      Self.ValueAxis := ValueAxis;
      Self.ValueAxisTitle := ValueAxisTitle;
      Self.ValueGridLines := ValueGridLines;
    end;
end;

{ TcxGridChartHistogramValues }

procedure TcxGridChartHistogramValues.SetStacking(Value: TcxGridChartValuesStacking);
begin
  if FStacking <> Value then
  begin
    FStacking := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramValues.SetVaryColorsByCategory(Value: Boolean);
begin
  if FVaryColorsByCategory <> Value then
  begin
    FVaryColorsByCategory := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogramValues.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartHistogramValues then
    with TcxGridChartHistogramValues(Source) do
    begin
      Self.Stacking := Stacking;
      Self.VaryColorsByCategory := VaryColorsByCategory;
    end;
end;

function TcxGridChartHistogramValues.GetVaryColorsByCategory: Boolean;
begin
  Result := (GridView.VisibleSeriesCount = 1) and FVaryColorsByCategory;
end;

{ TcxGridChartHistogram }

function TcxGridChartHistogram.GetStyles: TcxGridChartHistogramStyles;
begin
  Result := TcxGridChartHistogramStyles(inherited Styles);
end;

procedure TcxGridChartHistogram.SetAxisCategory(Value: TcxGridChartHistogramAxisCategory);
begin
  FAxisCategory.Assign(Value);
end;

procedure TcxGridChartHistogram.SetAxisValue(Value: TcxGridChartHistogramAxisValue);
begin
  FAxisValue.Assign(Value);
end;

procedure TcxGridChartHistogram.SetEmptyPointsDisplayMode(
  Value: TcxGridChartEmptyPointsDisplayMode);
begin
  if FEmptyPointsDisplayMode <> Value then
  begin
    FEmptyPointsDisplayMode := Value;
    Changed;
  end;
end;

procedure TcxGridChartHistogram.SetStyles(Value: TcxGridChartHistogramStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridChartHistogram.SetValues(Value: TcxGridChartHistogramValues);
begin
  FValues.Assign(Value);
end;

procedure TcxGridChartHistogram.SetOnCustomDrawPlot(Value: TcxGridChartHistogramPlotCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawPlot, Value) then
  begin
    FOnCustomDrawPlot := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartHistogram.ChangeScale(M, D: Integer);
begin
  inherited;
  Values.ChangeScale(M, D);
end;

procedure TcxGridChartHistogram.CreateSubObjects(AGridView: TcxGridChartView);
begin
  inherited;
  FAxisCategory := GetAxisCategoryClass.Create(Self);
  FAxisValue := GetAxisValueClass.Create(Self);
  FValues := GetValuesClass.Create(Self);
end;

procedure TcxGridChartHistogram.DestroySubObjects;
begin
  FreeAndNil(FValues);
  FreeAndNil(FAxisValue);
  FreeAndNil(FAxisCategory);
  inherited;
end;

function TcxGridChartHistogram.GetAxisCategoryClass: TcxGridChartHistogramAxisCategoryClass;
begin
  Result := TcxGridChartHistogramAxisCategory;
end;

function TcxGridChartHistogram.GetAxisValueClass: TcxGridChartHistogramAxisValueClass;
begin
  Result := TcxGridChartHistogramAxisValue;
end;

function TcxGridChartHistogram.GetStylesClass: TcxGridChartDiagramStylesClass;
begin
  Result := TcxGridChartHistogramStyles;
end;

function TcxGridChartHistogram.GetValuesClass: TcxGridChartHistogramValuesClass;
begin
  Result := TcxGridChartHistogramValues;
end;

function TcxGridChartHistogram.GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass;
begin
  Result := TcxGridChartHistogramLegendViewInfo;
end;

function TcxGridChartHistogram.PagingInOppositeDirection: Boolean;
begin
  Result := AxisCategory.CategoriesInReverseOrder;
end;

function TcxGridChartHistogram.SupportsPaging: Boolean;
begin
  Result := True;
end;

procedure TcxGridChartHistogram.DoCustomDrawPlot(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartHistogramViewInfo; const ABounds: TRect; var ADone: Boolean);
begin
  if HasCustomDrawPlot then
    FOnCustomDrawPlot(Self, ACanvas, AViewInfo, ABounds, ADone);
end;

function TcxGridChartHistogram.HasCustomDrawPlot: Boolean;
begin
  Result := Assigned(FOnCustomDrawPlot);
end;

procedure TcxGridChartHistogram.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartHistogram then
    with TcxGridChartHistogram(Source) do
    begin
      Self.AxisCategory := AxisCategory;
      Self.AxisValue := AxisValue;
      Self.Values := Values;
      Self.EmptyPointsDisplayMode := EmptyPointsDisplayMode;
      Self.OnCustomDrawPlot := OnCustomDrawPlot;
    end;
  inherited;
end;

function TcxGridChartHistogram.GetCategoryAxisPosition: TcxGridChartAxisPosition;
begin
  Result := cppBottom;
end;

function TcxGridChartHistogram.GetValueAxisPosition: TcxGridChartAxisPosition;
begin
  if AxisCategory.ValueAxisAtMaxCategory xor AxisCategory.CategoriesInReverseOrder then
    Result := cppRight
  else
    Result := cppLeft;
end;

{ TcxGridChartColumnDiagramValues }

constructor TcxGridChartColumnDiagramValues.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FBorderWidth := cxGridChartColumnDiagramDefaultBorderWidth;
end;

procedure TcxGridChartColumnDiagramValues.SetBorderWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end
end;

procedure TcxGridChartColumnDiagramValues.SetCaptionPosition(Value: TcxGridChartColumnDiagramValueCaptionPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    Changed;
  end;
end;

procedure TcxGridChartColumnDiagramValues.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartColumnDiagramValues then
    with TcxGridChartColumnDiagramValues(Source) do
    begin
      Self.BorderWidth := BorderWidth;
      Self.CaptionPosition := CaptionPosition;
    end;
end;

{ TcxGridChartColumnDiagram }

function TcxGridChartColumnDiagram.GetValues: TcxGridChartColumnDiagramValues;
begin
  Result := TcxGridChartColumnDiagramValues(inherited Values);
end;

procedure TcxGridChartColumnDiagram.SetValues(Value: TcxGridChartColumnDiagramValues);
begin
  inherited Values := Value;
end;

function TcxGridChartColumnDiagram.GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass;
begin
  Result := TcxGridChartColumnDiagramLegendViewInfo;
end;

function TcxGridChartColumnDiagram.GetValuesClass: TcxGridChartHistogramValuesClass;
begin
  Result := TcxGridChartColumnDiagramValues;
end;

function TcxGridChartColumnDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartColumnDiagramViewInfo;
end;

function TcxGridChartColumnDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartColumnDiagramDisplayText);
end;

function TcxGridChartColumnDiagram.GetImageIndex: Integer;
begin
  Result := 2;
end;

{ TcxGridChartBarDiagram }

function TcxGridChartBarDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartBarDiagramDisplayText);
end;

function TcxGridChartBarDiagram.GetImageIndex: Integer;
begin
  Result := 1;
end;

function TcxGridChartBarDiagram.HorizontalPaging: Boolean;
begin
  Result := False;
end;

function TcxGridChartBarDiagram.PagingInOppositeDirection: Boolean;
begin
  Result := not inherited PagingInOppositeDirection;
end;

function TcxGridChartBarDiagram.GetCategoryAxisPosition: TcxGridChartAxisPosition;
begin
  Result := cppLeft;
end;

function TcxGridChartBarDiagram.GetValueAxisPosition: TcxGridChartAxisPosition;
begin
  if AxisCategory.ValueAxisAtMaxCategory xor AxisCategory.CategoriesInReverseOrder then
    Result := cppTop
  else
    Result := cppBottom;
end;

{ TcxGridChartStackedAreaDiagram }

function TcxGridChartStackedAreaDiagram.CheckGapPoints: Boolean;
begin
  Result := (StackedStyle = sas100Percent) and (EmptyPointsDisplayMode = epdmGap);
end;

function TcxGridChartStackedAreaDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartStackedAreaDiagramDisplayText);
end;

function TcxGridChartStackedAreaDiagram.GetImageIndex: Integer;
begin
  Result := 5;
end;

function TcxGridChartStackedAreaDiagram.GetValueCaption(
  ASeries: TcxGridChartSeries; AValueIndex: Integer): string;
begin
  Result := GetStackedValueCaption(ASeries, AValueIndex, StackedStyle = sas100Percent);
end;

function TcxGridChartStackedAreaDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartStackedAreaDiagramViewInfo;
end;

procedure TcxGridChartStackedAreaDiagram.SetStackedStyle(
  Value: TcxGridChartStackedAreaDiagramStyle);
begin
  if FStackedStyle <> Value then
  begin
    FStackedStyle := Value;
    if Value = sasDefault then
      Values.Stacking := vsNone
    else
      Values.Stacking := vs100Percent;
    GridView.UpdateSeriesVisibleGroups;
    Changed;
  end;
end;

{ TcxGridChartStackedColumnDiagram }

constructor TcxGridChartStackedColumnDiagram.Create(AGridView: TcxGridChartView);
begin
  inherited Create(AGridView);
  FSideBySideIndentWidth := cxGridChartSideBySideDefaultIndentWidth;
end;

procedure TcxGridChartStackedColumnDiagram.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridChartStackedColumnDiagram then
  begin
    StackedStyle := TcxGridChartStackedColumnDiagram(Source).StackedStyle;
    SideBySideIndentWidth := TcxGridChartStackedColumnDiagram(Source).SideBySideIndentWidth;
  end;
end;

function TcxGridChartStackedColumnDiagram.GetValueCaption(
  ASeries: TcxGridChartSeries; AValueIndex: Integer): string;
begin
  Result := GetStackedValueCaption(ASeries, AValueIndex, StackedStyle in  [sds100Percent, sdsSideBySide100Percent]);
end;

procedure TcxGridChartStackedColumnDiagram.ChangeScale(M, D: Integer);
begin
  inherited;
  SideBySideIndentWidth := MulDiv(SideBySideIndentWidth, M, D);
end;

function TcxGridChartStackedColumnDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartStackedColumnDiagramDisplayText);
end;

function TcxGridChartStackedColumnDiagram.GetImageIndex: Integer;
begin
  Result := 7;
end;

function TcxGridChartStackedColumnDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartStackedColumnDiagramViewInfo
end;

procedure TcxGridChartStackedColumnDiagram.SetSideBySideIndentWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FSideBySideIndentWidth <> Value then
  begin
    FSideBySideIndentWidth := Value;
    Changed;
  end;
end;

procedure TcxGridChartStackedColumnDiagram.SetStackedStyle(
  Value: TcxGridChartStackedDiagramStyle);
const
  StackingStyleToValuesStacking: array[TcxGridChartStackedDiagramStyle] of TcxGridChartValuesStacking =
    (vsNone, vs100Percent, vsNone, vs100Percent);
begin
  if FStackedStyle <> Value then
  begin
    FStackedStyle := Value;
    Values.Stacking := StackingStyleToValuesStacking[Value];
    GridView.UpdateSeriesVisibleGroups;
    Changed;
  end;
end;

{ TcxGridChartStackedBarDiagram }

function TcxGridChartStackedBarDiagram.GetCategoryAxisPosition: TcxGridChartAxisPosition;
begin
  Result := cppLeft;
end;

function TcxGridChartStackedBarDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartStackedBarDiagramDisplayText);
end;

function TcxGridChartStackedBarDiagram.GetImageIndex: Integer;
begin
  Result := 6;
end;

function TcxGridChartStackedBarDiagram.GetValueAxisPosition: TcxGridChartAxisPosition;
begin
  if AxisCategory.ValueAxisAtMaxCategory xor AxisCategory.CategoriesInReverseOrder then
    Result := cppTop
  else
    Result := cppBottom;
end;

function TcxGridChartStackedBarDiagram.HorizontalPaging: Boolean;
begin
  Result := False;
end;

function TcxGridChartStackedBarDiagram.PagingInOppositeDirection: Boolean;
begin
  Result := not inherited PagingInOppositeDirection;
end;

{ TcxGridChartLineDiagramAxisCategory }

function TcxGridChartLineDiagramAxisCategory.GetDefaultValueAxisBetweenCategories: Boolean;
begin
  Result := False;
end;

{ TcxGridChartLineDiagramStyles }

procedure TcxGridChartLineDiagramStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  if Index = dsValueMarkers then
  begin
    with TcxGridChartDiagramValueData(AData) do
      GetValueParams(VisibleSeriesIndex, ValueIndex, AParams);
    if not ValueMarkerHasBorderByDefault then
      AParams.TextColor := AParams.Color;
  end;
end;

function TcxGridChartLineDiagramStyles.ValueMarkerHasBorderByDefault: Boolean;
begin
  Result := False;
end;

procedure TcxGridChartLineDiagramStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartLineDiagramStyles then
    ValueMarkers := TcxGridChartLineDiagramStyles(Source).ValueMarkers;
end;

procedure TcxGridChartLineDiagramStyles.GetValueMarkerParams(AVisibleSeriesIndex, AValueIndex: Integer;
  out AParams: TcxViewParams);
var
  AData: TcxGridChartDiagramValueData;
begin
  AData := TcxGridChartDiagramValueData.Create;
  try
    AData.VisibleSeriesIndex := AVisibleSeriesIndex;
    AData.ValueIndex := AValueIndex;
    GetViewParams(dsValueMarkers, AData, nil, AParams);
  finally
    AData.Free;
  end;
end;

{ TcxGridChartLineDiagramValues }

constructor TcxGridChartLineDiagramValues.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FHotSpotSize := cxGridChartLineDiagramDefaultHotSpotSize;
  FLineStyle := clsSolid;
  FLineWidth := cxGridChartLineDiagramDefaultLineWidth;
  FMarkerSize := cxGridChartLineDiagramDefaultMarkerSize;
end;

function TcxGridChartLineDiagramValues.GetDiagram: TcxGridChartLineDiagram;
begin
  Result := TcxGridChartLineDiagram(inherited Diagram);
end;

procedure TcxGridChartLineDiagramValues.SetCaptionPosition(
  Value: TcxGridChartLineDiagramValueCaptionPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetHotSpotSize(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FHotSpotSize <> Value then
  begin
    FHotSpotSize := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetLineStyle(Value: TcxGridChartLineStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetLineWidth(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetMarkerSize(Value: Integer);
begin
  if Value < 3 then Value := 3;
  if FMarkerSize <> Value then
  begin
    FMarkerSize := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetMarkerStyle(Value: TcxGridChartMarkerStyle);
begin
  if FMarkerStyle <> Value then
  begin
    FMarkerStyle := Value;
    Changed;
  end;
end;

procedure TcxGridChartLineDiagramValues.SetOnGetLineStyle(Value: TcxGridChartLineDiagramGetLineStyleEvent);
begin
  if not dxSameMethods(FOnGetLineStyle, Value) then
  begin
    FOnGetLineStyle := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartLineDiagramValues.SetOnGetMarkerStyle(Value: TcxGridChartLineDiagramGetMarkerStyleEvent);
begin
  if not dxSameMethods(FOnGetMarkerStyle, Value) then
  begin
    FOnGetMarkerStyle := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartLineDiagramValues.ChangeScale(M, D: Integer);
begin
  inherited;
  LineWidth := MulDiv(LineWidth, M, D);
  MarkerSize := MulDiv(MarkerSize, M, D);
  HotSpotSize := MulDiv(HotSpotSize, M, D);
end;

procedure TcxGridChartLineDiagramValues.DoGetLineStyle(ASeries: TcxGridChartSeries; var AStyle: TcxGridChartLineStyle);
begin
  if Assigned(FOnGetLineStyle) then FOnGetLineStyle(Diagram, ASeries, AStyle);
end;

procedure TcxGridChartLineDiagramValues.DoGetMarkerStyle(ASeries: TcxGridChartSeries;
  var AStyle: TcxGridChartMarkerStyle);
begin
  if Assigned(FOnGetMarkerStyle) then FOnGetMarkerStyle(Diagram, ASeries, AStyle);
end;

procedure TcxGridChartLineDiagramValues.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartLineDiagramValues then
    with TcxGridChartLineDiagramValues(Source) do
    begin
      Self.CaptionPosition := CaptionPosition;
      Self.HotSpotSize := HotSpotSize;
      Self.LineStyle := LineStyle;
      Self.LineWidth := LineWidth;
      Self.MarkerSize := MarkerSize;
      Self.MarkerStyle := MarkerStyle;
      Self.OnGetLineStyle := OnGetLineStyle;
      Self.OnGetMarkerStyle := OnGetMarkerStyle;
    end;
end;

function TcxGridChartLineDiagramValues.GetHotSpotSize: Integer;
begin
  Result := FHotSpotSize;
  if MarkerStyle <> cmsNone then
    Result := Max(Result, MarkerSize);
end;

function TcxGridChartLineDiagramValues.GetLineStyle(ASeries: TcxGridChartSeries): TcxGridChartLineStyle;
begin
  Result := FLineStyle;
  DoGetLineStyle(ASeries, Result);
end;

function TcxGridChartLineDiagramValues.GetMarkerStyle(ASeries: TcxGridChartSeries): TcxGridChartMarkerStyle;
begin
  Result := FMarkerStyle;
  DoGetMarkerStyle(ASeries, Result);
end;

{ TcxGridChartLineDiagram }

function TcxGridChartLineDiagram.GetAxisCategory: TcxGridChartLineDiagramAxisCategory;
begin
  Result := TcxGridChartLineDiagramAxisCategory(inherited AxisCategory);
end;

function TcxGridChartLineDiagram.GetStyles: TcxGridChartLineDiagramStyles;
begin
  Result := TcxGridChartLineDiagramStyles(inherited Styles);
end;

function TcxGridChartLineDiagram.GetValues: TcxGridChartLineDiagramValues;
begin
  Result := TcxGridChartLineDiagramValues(inherited Values);
end;

procedure TcxGridChartLineDiagram.SetAxisCategory(Value: TcxGridChartLineDiagramAxisCategory);
begin
  inherited AxisCategory := Value;
end;

procedure TcxGridChartLineDiagram.SetStyles(Value: TcxGridChartLineDiagramStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridChartLineDiagram.SetValues(Value: TcxGridChartLineDiagramValues);
begin
  inherited Values := Value;
end;

function TcxGridChartLineDiagram.GetAxisCategoryClass: TcxGridChartHistogramAxisCategoryClass;
begin
  Result := TcxGridChartLineDiagramAxisCategory;
end;

function TcxGridChartLineDiagram.GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass;
begin
  Result := TcxGridChartLineDiagramLegendViewInfo;
end;

function TcxGridChartLineDiagram.GetStylesClass: TcxGridChartDiagramStylesClass;
begin
  Result := TcxGridChartLineDiagramStyles;
end;

function TcxGridChartLineDiagram.GetValuesClass: TcxGridChartHistogramValuesClass;
begin
  Result := TcxGridChartLineDiagramValues;
end;

function TcxGridChartLineDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartLineDiagramViewInfo;
end;

function TcxGridChartLineDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartLineDiagramDisplayText);
end;

function TcxGridChartLineDiagram.GetImageIndex: Integer;
begin
  Result := 3;
end;

function TcxGridChartLineDiagram.SupportsValueHotTrack: Boolean;
begin
  Result := False;
end;

{ TcxGridChartAreaDiagramStyles }

function TcxGridChartAreaDiagramStyles.ValueMarkerHasBorderByDefault: Boolean;
begin
  Result := True;
end;

{ TcxGridChartAreaDiagram }

function TcxGridChartAreaDiagram.GetStyles: TcxGridChartAreaDiagramStyles;
begin
  Result := TcxGridChartAreaDiagramStyles(inherited Styles);
end;

procedure TcxGridChartAreaDiagram.SetStyles(Value: TcxGridChartAreaDiagramStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridChartAreaDiagram.SetTransparency(Value: Byte);
begin
  if Value <> FTransparency then
  begin
    FTransparency := Value;
    Changed();
  end;
end;

procedure TcxGridChartAreaDiagram.SetOnCustomDrawValueArea(Value: TcxGridChartAreaDiagramValueAreaCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawValueArea, Value) then
  begin
    FOnCustomDrawValueArea := Value;
    Changed(dcProperty);
  end;
end;

function TcxGridChartAreaDiagram.CheckGapPoints: Boolean;
begin
  Result := False;
end;

function TcxGridChartAreaDiagram.GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass;
begin
  Result := TcxGridChartAreaDiagramLegendViewInfo;
end;

function TcxGridChartAreaDiagram.GetStylesClass: TcxGridChartDiagramStylesClass;
begin
  Result := TcxGridChartAreaDiagramStyles;
end;

function TcxGridChartAreaDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartAreaDiagramViewInfo;
end;

function TcxGridChartAreaDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartAreaDiagramDisplayText);
end;

function TcxGridChartAreaDiagram.GetImageIndex: Integer;
begin
  Result := 0;
end;

procedure TcxGridChartAreaDiagram.DoCustomDrawValueArea(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartAreaDiagramValueViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawValueArea then
    FOnCustomDrawValueArea(Self, ACanvas, AViewInfo, ADone);
end;

function TcxGridChartAreaDiagram.HasCustomDrawValueArea: Boolean;
begin
  Result := Assigned(FOnCustomDrawValueArea);
end;

procedure TcxGridChartAreaDiagram.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartAreaDiagram then
  begin
    OnCustomDrawValueArea := TcxGridChartAreaDiagram(Source).OnCustomDrawValueArea;
    Transparency := TcxGridChartAreaDiagram(Source).Transparency;
  end;
end;

{ TcxGridChartPieDiagramStyles }

procedure TcxGridChartPieDiagramStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      dsSeriesSiteCaptions:
        begin
          Color := DefaultChartPieDiagramSeriesSiteCaptionColor;
          TextColor := DefaultChartPieDiagramSeriesSiteCaptionTextColor;
        end;
      dsSeriesSites:
        begin
          Color := DefaultContentColor;
          TextColor := DefaultChartPieDiagramSeriesSiteBorderColor;
        end;
    end;
end;

function TcxGridChartPieDiagramStyles.GetVaryColorsByCategory: Boolean;
begin
  Result := True;
end;

procedure TcxGridChartPieDiagramStyles.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartPieDiagramStyles then
    with TcxGridChartPieDiagramStyles(Source) do
    begin
      Self.SeriesSiteCaptions := SeriesSiteCaptions;
      Self.SeriesSites := SeriesSites;
    end;
  inherited;
end;

{ TcxGridChartPieDiagramValues }

constructor TcxGridChartPieDiagramValues.Create(ADiagram: TcxGridChartDiagram);
begin
  inherited;
  FCaptionItems := [pdvciValue];
end;

procedure TcxGridChartPieDiagramValues.SetAngleOfFirstSlice(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > 360 then Value := 360;
  if FAngleOfFirstSlice <> Value then
  begin
    FAngleOfFirstSlice := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagramValues.SetCaptionItems(Value: TcxGridChartPieDiagramValueCaptionItems);
begin
  if FCaptionItems <> Value then
  begin
    FCaptionItems := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagramValues.SetCaptionItemSeparator(const Value: string);
begin
  if FCaptionItemSeparator <> Value then
  begin
    FCaptionItemSeparator := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagramValues.SetCaptionPosition(Value: TcxGridChartPieDiagramValueCaptionPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagramValues.SetPercentageCaptionFormat(const Value: string);
begin
  if FPercentageCaptionFormat <> Value then
  begin
    FPercentageCaptionFormat := Value;
    Changed;
  end;
end;

function TcxGridChartPieDiagramValues.GetDefaultCaptionItemSeparator: string;
begin
  Result := '; ';
end;

function TcxGridChartPieDiagramValues.GetDefaultPercentageCaptionFormat: string;
begin
  Result := '0%';
end;

procedure TcxGridChartPieDiagramValues.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartPieDiagramValues then
    with TcxGridChartPieDiagramValues(Source) do
    begin
      Self.AngleOfFirstSlice := AngleOfFirstSlice;
      Self.CaptionItems := CaptionItems;
      Self.CaptionItemSeparator := CaptionItemSeparator;
      Self.CaptionPosition := CaptionPosition;
      Self.PercentageCaptionFormat := PercentageCaptionFormat;
    end;
  inherited;
end;

function TcxGridChartPieDiagramValues.GetCaptionItemSeparator: string;
begin
  Result := FCaptionItemSeparator;
  if Result = '' then
    Result := GetDefaultCaptionItemSeparator;
end;

function TcxGridChartPieDiagramValues.GetCaptionPosition: TcxGridChartPieDiagramValueCaptionPosition;
begin
  if FCaptionItems = [] then
    Result := pdvcpNone
  else
    Result := FCaptionPosition;
end;

function TcxGridChartPieDiagramValues.GetPercentageCaptionFormat: string;
begin
  Result := FPercentageCaptionFormat;
  if Result = '' then
    Result := GetDefaultPercentageCaptionFormat;
end;

{ TcxGridChartPieDiagram }

constructor TcxGridChartPieDiagram.Create(AGridView: TcxGridChartView);
begin
  inherited;
  FSeriesCaptions := True;
end;

destructor TcxGridChartPieDiagram.Destroy;
begin
  inherited;
end;

function TcxGridChartPieDiagram.GetStyles: TcxGridChartPieDiagramStyles;
begin
  Result := TcxGridChartPieDiagramStyles(inherited Styles);
end;

procedure TcxGridChartPieDiagram.SetSeriesCaptions(Value: Boolean);
begin
  if FSeriesCaptions <> Value then
  begin
    FSeriesCaptions := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagram.SetSeriesColumnCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSeriesColumnCount <> Value then
  begin
    FSeriesColumnCount := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagram.SetSeriesSites(Value: Boolean);
begin
  if FSeriesSites <> Value then
  begin
    FSeriesSites := Value;
    Changed;
  end;
end;

procedure TcxGridChartPieDiagram.SetStyles(Value: TcxGridChartPieDiagramStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridChartPieDiagram.SetValues(Value: TcxGridChartPieDiagramValues);
begin
  FValues.Assign(Value);
end;

procedure TcxGridChartPieDiagram.SetOnCustomDrawSeriesSite(Value: TcxGridChartPieDiagramSeriesSiteCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawSeriesSite, Value) then
  begin
    FOnCustomDrawSeriesSite := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartPieDiagram.SetOnCustomDrawSeriesSiteCaption(Value: TcxGridChartPieDiagramSeriesSiteCaptionCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawSeriesSiteCaption, Value) then
  begin
    FOnCustomDrawSeriesSiteCaption := Value;
    Changed(dcProperty);
  end;
end;

procedure TcxGridChartPieDiagram.ChangeScale(M, D: Integer);
begin
  inherited;
  Values.ChangeScale(M, D);
end;

procedure TcxGridChartPieDiagram.CreateSubObjects(AGridView: TcxGridChartView);
begin
  inherited;
  FValues := GetValuesClass.Create(Self);
end;

procedure TcxGridChartPieDiagram.DestroySubObjects;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TcxGridChartPieDiagram.GetStylesClass: TcxGridChartDiagramStylesClass;
begin
  Result := TcxGridChartPieDiagramStyles;
end;

function TcxGridChartPieDiagram.GetValuesClass: TcxGridChartPieDiagramValuesClass;
begin
  Result := TcxGridChartPieDiagramValues;
end;

function TcxGridChartPieDiagram.GetLegendViewInfoClass: TcxGridChartLegendViewInfoClass;
begin
  Result := TcxGridChartPieDiagramLegendViewInfo;
end;

function TcxGridChartPieDiagram.GetViewInfoClass: TcxGridChartDiagramViewInfoClass;
begin
  Result := TcxGridChartPieDiagramViewInfo;
end;

function TcxGridChartPieDiagram.GetDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartPieDiagramDisplayText);
end;

function TcxGridChartPieDiagram.GetImageIndex: Integer;
begin
  Result := 4;
end;

procedure TcxGridChartPieDiagram.DoCustomDrawSeriesSite(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartPieSeriesSiteViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawSeriesSite then
    FOnCustomDrawSeriesSite(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridChartPieDiagram.DoCustomDrawSeriesSiteCaption(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartPieSeriesSiteCaptionViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawSeriesSiteCaption then
    FOnCustomDrawSeriesSiteCaption(Self, ACanvas, AViewInfo, ADone);
end;

function TcxGridChartPieDiagram.HasCustomDrawSeriesSite: Boolean;
begin
  Result := Assigned(FOnCustomDrawSeriesSite);
end;

function TcxGridChartPieDiagram.HasCustomDrawSeriesSiteCaption: Boolean;
begin
  Result := Assigned(FOnCustomDrawSeriesSiteCaption);
end;

procedure TcxGridChartPieDiagram.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartPieDiagram then
    with TcxGridChartPieDiagram(Source) do
    begin
      Self.SeriesCaptions := SeriesCaptions;
      Self.SeriesColumnCount := SeriesColumnCount;
      Self.SeriesSites := SeriesSites;
      Self.Values := Values;
      Self.OnCustomDrawSeriesSite := OnCustomDrawSeriesSite;
      Self.OnCustomDrawSeriesSiteCaption := OnCustomDrawSeriesSiteCaption;
    end;
  inherited;
end;

function TcxGridChartPieDiagram.GetSeriesColumnCount: Integer;
begin
  Result := SeriesColumnCount;
  if (Result = 0) or (Result > GridView.VisibleSeriesCount) then
    Result := GridView.VisibleSeriesCount;
end;

function TcxGridChartPieDiagram.GetValueCaption(ASeries: TcxGridChartSeries;
  AValueIndex: Integer): string;

  function GetItemText(AItem: TcxGridChartPieDiagramValueCaptionItem): string;
  var
    AValue: Variant;
  begin
    case AItem of
      pdvciCategory:
        Result := GridView.Categories.VisibleDisplayTexts[AValueIndex];
      pdvciValue:
        Result := ASeries.VisibleDisplayTexts[AValueIndex];
      pdvciPercentage:
        begin
          if ASeries.SumOfValues = 0 then
            AValue := 100
          else
            AValue := Abs(100 * ASeries.VisibleValues[AValueIndex] / ASeries.SumOfValues);
          Result := FormatFloat(Values.GetPercentageCaptionFormat, AValue);
        end;
    end;
  end;

var
  ACaptionItem: TcxGridChartPieDiagramValueCaptionItem;
begin
  Result := '';
  for ACaptionItem := Low(ACaptionItem) to High(ACaptionItem) do
    if ACaptionItem in Values.CaptionItems then
      if Result = '' then
        Result := GetItemText(ACaptionItem)
      else
        Result := Result + Values.GetCaptionItemSeparator + GetItemText(ACaptionItem);
end;

{ TcxGridChartItemDataBinding }

constructor TcxGridChartItemDataBinding.Create(AGridView: TcxGridChartView;
  AIsValue: Boolean; ADefaultValueTypeClass: TcxValueTypeClass);
begin
  inherited Create(nil);
  FGridView := AGridView;
  FIsValue := AIsValue;
  FDefaultValueTypeClass := ADefaultValueTypeClass;
  FDefaultValuesProvider := GetDefaultValuesProviderClass.Create(Self);
end;

destructor TcxGridChartItemDataBinding.Destroy;
begin
  FDefaultValuesProvider.Free;
  inherited;
end;

function TcxGridChartItemDataBinding.GetDataController: TcxCustomDataController;
begin
  Result := FDataField.DataController;
end;

function TcxGridChartItemDataBinding.GetDataIndex: Integer;
begin
  Result := FDataField.Index;
end;

function TcxGridChartItemDataBinding.GetDataItem: TObject;
begin
  Result := FDataField.Item;
end;

function TcxGridChartItemDataBinding.GetDefaultProperties: TcxCustomEditProperties;
begin
  Result := FDefaultRepositoryItem.Properties;
  with Result do
  begin
    LockUpdate(True);
    IDefaultValuesProvider := Self.DefaultValuesProvider;
    LockUpdate(False);
  end;
end;

function TcxGridChartItemDataBinding.GetDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
begin
  Result := FDefaultValuesProvider;
  InitDefaultValuesProvider(Result);
end;

function TcxGridChartItemDataBinding.GetGroupIndex: Integer;
begin
  Result := DataController.Groups.ItemGroupIndex[DataIndex];
end;

function TcxGridChartItemDataBinding.GetSortIndex: Integer;
begin
  Result := DataController.GetItemSortingIndex(DataIndex);
end;

function TcxGridChartItemDataBinding.GetSortOrder: TcxDataSortOrder;
begin
  Result := DataController.GetItemSortOrder(DataIndex);
end;

function TcxGridChartItemDataBinding.GetSummaryIndex: Integer;
begin
  Result := FSummaryItem.Index;
end;

function TcxGridChartItemDataBinding.GetSummaryKind: TcxSummaryKind;
begin
  Result := FSummaryItem.Kind;
end;

function TcxGridChartItemDataBinding.GetValueType: string;
begin
  if ValueTypeClass = nil then
    Result := ''
  else
    Result := ValueTypeClass.Caption;
end;

function TcxGridChartItemDataBinding.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := DataController.GetItemValueTypeClass(DataIndex);
end;

procedure TcxGridChartItemDataBinding.SetData(Value: TObject);
begin
  if FData <> Value then
  begin
    FData := Value;
    FGridView.Changed(vcProperty);
  end;
end;

procedure TcxGridChartItemDataBinding.SetDataField(Value: TcxCustomDataField);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    if FDataField <> nil then
    begin
      ValueTypeClass := GetDefaultValueTypeClass;
      if IsValue then
        CreateSummaryItem(FSummaryItem);
    end
    else
      FreeAndNil(FSummaryItem);
    UpdateDefaultRepositoryItemValue;
  end;
end;

procedure TcxGridChartItemDataBinding.SetGroupIndex(Value: Integer);
begin
  DataController.Groups.ChangeGrouping(DataIndex, Value);
end;

procedure TcxGridChartItemDataBinding.SetSortIndex(Value: Integer);
begin
  DataController.ChangeItemSortingIndex(DataIndex, Value);
end;

procedure TcxGridChartItemDataBinding.SetSortOrder(Value: TcxDataSortOrder);
begin
  if SortOrder <> Value then
  begin
    GridView.BeginUpdate;
    try
      GridView.DataSortingChanging(DataItem);
      DataController.ChangeSorting(DataIndex, Value);
      GridView.DataSortingChanged(DataItem);
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridChartItemDataBinding.SetSummaryKind(Value: TcxSummaryKind);
begin
  FSummaryItem.Kind := Value;
end;

procedure TcxGridChartItemDataBinding.SetValueType(const Value: string);
begin
  if ValueType <> Value then
    ValueTypeClass := cxValueTypeClassList.ItemByCaption(Value);
end;

procedure TcxGridChartItemDataBinding.SetValueTypeClass(Value: TcxValueTypeClass);
begin
  DataController.ChangeValueTypeClass(DataIndex, Value);
end;

procedure TcxGridChartItemDataBinding.ItemRemoved(Sender: TcxEditRepositoryItem);
begin
  UpdateDefaultRepositoryItemValue;
end;

procedure TcxGridChartItemDataBinding.PropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  if not GridView.IsPattern then GridView.Changed(vcLayout);
end;

procedure TcxGridChartItemDataBinding.CreateSummaryItem(var ASummaryItem: TcxDataSummaryItem);
begin
  DataController.Summary.DefaultGroupSummaryItems.BeginUpdate;
  try
    ASummaryItem := DataController.Summary.DefaultGroupSummaryItems.Add;
    ASummaryItem.Kind := cxGridChartItemDefaultSummaryKind;
    ASummaryItem.Position := spGroup;
    ASummaryItem.ItemLink := DataItem;
  finally
    DataController.Summary.DefaultGroupSummaryItems.EndUpdate;
  end;
end;

function TcxGridChartItemDataBinding.GetDefaultDisplayText: string;
begin
  Result := '';
end;

function TcxGridChartItemDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomEditDefaultValuesProvider;
end;

function TcxGridChartItemDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := FDefaultValueTypeClass;
end;

function TcxGridChartItemDataBinding.GetSummaryItem: TcxDataSummaryItem;
begin
  Result := DataController.Summary.DefaultGroupSummaryItems.ItemOfItemLink(DataItem);
end;

procedure TcxGridChartItemDataBinding.InitDefaultValuesProvider(ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider);
begin
end;

function TcxGridChartItemDataBinding.IsValueTypeInteger: Boolean;
begin
  Result := (ValueTypeClass = nil) or IsValueTypeValid(ValueTypeClass.GetVarType, True);
end;

function TcxGridChartItemDataBinding.IsValueTypeStored: Boolean;
begin
  Result := ValueTypeClass <> GetDefaultValueTypeClass;
end;

procedure TcxGridChartItemDataBinding.UpdateSummaryItemValue;
begin
  if IsValue then
    FSummaryItem := GetSummaryItem;
end;

procedure TcxGridChartItemDataBinding.ValueTypeClassChanged;
begin
  UpdateDefaultRepositoryItemValue;
  GridView.Changed(vcLayout);
end;

function TcxGridChartItemDataBinding.GetDefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDataRepositoryItems.GetItem(ValueTypeClass);
end;

procedure TcxGridChartItemDataBinding.UpdateDefaultRepositoryItemValue;
begin
  if FDefaultRepositoryItem <> nil then
    FDefaultRepositoryItem.RemoveListener(Self);
  if FDataField = nil then
    FDefaultRepositoryItem := nil
  else
    FDefaultRepositoryItem := GetDefaultRepositoryItem;
  if FDefaultRepositoryItem <> nil then
    FDefaultRepositoryItem.AddListener(Self);
end;

procedure TcxGridChartItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartItemDataBinding then
    with TcxGridChartItemDataBinding(Source) do
    begin
      Self.Data := Data;
      Self.ValueTypeClass := ValueTypeClass;
    end
  else
    inherited;
end;

function TcxGridChartItemDataBinding.GetValueDisplayText(const AValue: Variant): string;
begin
  Result := DefaultProperties.GetDisplayText(AValue);
end;

function TcxGridChartItemDataBinding.IsValueTypeClassValid(AValueTypeClass: TcxValueTypeClass): Boolean;
begin
  if IsValue then
    Result := cxGridChartView.IsValueTypeClassValid(AValueTypeClass)
  else
    Result := True;
end;

{ TcxGridChartItem }

constructor TcxGridChartItem.Create(AOwner: TComponent);
begin
  inherited;
  FVisible := True;
  FVisibleForCustomization := True;
  FVisibleIndex := -1;
end;

destructor TcxGridChartItem.Destroy;
begin
  if not GridView.IsDestroying and GridView.IsDesigning then
    GridView.Controller.DesignController.UnselectObject(Self);
  FGridView.RemoveItem(Self);
  FDataBinding.Free;
  inherited;
end;

function TcxGridChartItem.GetID: Integer;
begin
  Result := FDataBinding.ID;
end;

function TcxGridChartItem.GetIndex: Integer;
begin
  Result := FGridView.GetItemIndex(Self);
end;

function TcxGridChartItem.GetSortOrder: TcxDataSortOrder;
begin
  Result := FDataBinding.SortOrder;
end;

function TcxGridChartItem.GetTag: TcxTag;
begin
  Result := inherited Tag;
end;

function TcxGridChartItem.GetVisibleDisplayText(AIndex: Integer): string;
begin
  Result := GetValueDisplayText(VisibleValues[AIndex]);
end;

procedure TcxGridChartItem.SetDataBinding(Value: TcxGridChartItemDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxGridChartItem.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
  begin
    FDisplayText := Value;
    DisplayTextChanged;
  end;
end;

procedure TcxGridChartItem.SetIndex(Value: Integer);
begin
  FGridView.SetItemIndex(Self, Value);
end;

procedure TcxGridChartItem.SetOnGetStoredProperties(Value: TcxGridChartItemGetStoredPropertiesEvent);
begin
  if not dxSameMethods(FOnGetStoredProperties, Value) then
  begin
    FOnGetStoredProperties := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartItem.SetOnGetStoredPropertyValue(Value: TcxGridChartItemGetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnGetStoredPropertyValue, Value) then
  begin
    FOnGetStoredPropertyValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartItem.SetOnGetValueDisplayText(Value: TcxGridChartGetValueDisplayTextEvent);
begin
  if not dxSameMethods(FOnGetValueDisplayText, Value) then
  begin
    FOnGetValueDisplayText := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartItem.SetOnSetStoredPropertyValue(Value: TcxGridChartItemSetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnSetStoredPropertyValue, Value) then
  begin
    FOnSetStoredPropertyValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartItem.SetSortOrder(Value: TcxDataSortOrder);
begin
  FDataBinding.SortOrder := Value;
end;

procedure TcxGridChartItem.SetTag(Value: TcxTag);
begin
  if Tag <> Value then
  begin
    inherited Tag := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    FGridView.ItemVisibilityChanged(Self);
    Changed;
  end;
end;

procedure TcxGridChartItem.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    GridView.ItemVisibilityForCustomizationChanged(Self);
    Changed(vcProperty);
  end;
end;

function TcxGridChartItem.IsTagStored: Boolean;
begin
  Result := Tag <> 0;
end;

function TcxGridChartItem.GetObjectName: string;
begin
  if GridView.IsStoringNameMode then
    Result := IntToStr(ID)
  else
    Result := Name;
end;

function TcxGridChartItem.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Index');
  AProperties.Add('Visible');
  if Assigned(FOnGetStoredProperties) then
    FOnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TcxGridChartItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Index' then
    AValue := Index
  else
    if AName = 'Visible' then
      AValue := Visible
    else
      if Assigned(FOnGetStoredPropertyValue) then
        FOnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxGridChartItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Index' then
    SetRestoredIndex(AValue)
  else
    if AName = 'Visible' then
      Visible := AValue
    else
      if Assigned(FOnSetStoredPropertyValue) then
        FOnSetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxGridChartItem.DataChanged;
begin
  if DataBinding.GetDefaultDisplayText <> FLastDataBindingDefaultDisplayText then
  begin
    FLastDataBindingDefaultDisplayText := DataBinding.GetDefaultDisplayText;
    DisplayTextChanged;
  end;
end;

function TcxGridChartItem.GetDataBinding: TcxGridChartItemDataBinding;
begin
  Result := FDataBinding;
end;

procedure TcxGridChartItem.ValueTypeClassChanged;
begin
  FDataBinding.ValueTypeClassChanged;
end;

procedure TcxGridChartItem.Changed(AChange: TcxGridViewChangeKind = vcLayout);
begin
  FGridView.Changed(AChange);
end;

procedure TcxGridChartItem.DisplayTextChanged;
begin
  Changed;
  GridView.ItemDisplayTextChanged(Self);
end;

procedure TcxGridChartItem.DoGetValueDisplayText(const AValue: Variant;
  var ADisplayText: string);
begin
  if Assigned(FOnGetValueDisplayText) then
    FOnGetValueDisplayText(Self, AValue, ADisplayText);
end;

function TcxGridChartItem.GetDefaultDisplayText: string;
begin
  Result := DataBinding.GetDefaultDisplayText;
  if Result = '' then
    Result := Name;
  if Result = '' then
    Result := DefaultItemDisplayText;
end;

procedure TcxGridChartItem.SetGridView(Value: TcxGridChartView);
begin
  FGridView := Value;
  if FDataBinding = nil then
    FDataBinding := GridView.GetItemDataBindingClass.Create(GridView, IsValue,
      GetDefaultValueTypeClass);
end;

procedure TcxGridChartItem.SetName(const NewName: TComponentName);
begin
  inherited;
  DisplayTextChanged;
end;

procedure TcxGridChartItem.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartItem then
    with TcxGridChartItem(Source) do
    begin
      Self.DataBinding := DataBinding;
      Self.DisplayText := DisplayText;
      Self.SortOrder := SortOrder;
      Self.Tag := Tag;
      Self.Visible := Visible;
      Self.VisibleForCustomization := VisibleForCustomization;
      Self.OnGetStoredProperties := OnGetStoredProperties;
      Self.OnGetStoredPropertyValue := OnGetStoredPropertyValue;
      Self.OnGetValueDisplayText := OnGetValueDisplayText;
      Self.OnSetStoredPropertyValue := OnSetStoredPropertyValue;
    end
  else
    inherited;
end;

function TcxGridChartItem.GetParentComponent: TComponent;
begin
  Result := FGridView;
end;

function TcxGridChartItem.HasParent: Boolean;
begin
  Result := FGridView <> nil;
end;

procedure TcxGridChartItem.SetParentComponent(AParent: TComponent);
begin
  if AParent is TcxGridChartView then
    TcxGridChartView(AParent).AddItem(Self);
end;

function TcxGridChartItem.GetDisplayText: string;
begin
  Result := DisplayText;
  if Result = '' then
    Result := GetDefaultDisplayText;
end;

function TcxGridChartItem.GetValueDisplayText(const AValue: Variant): string;
begin
  Result := DataBinding.GetValueDisplayText(AValue);
  DoGetValueDisplayText(AValue, Result);
end;

class function TcxGridChartItem.IsValue: Boolean;
begin
  Result := True;
end;

{ TcxGridChartCategories }

constructor TcxGridChartCategories.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FDataBinding := GridView.GetItemDataBindingClass.Create(GridView, False,
    GetDefaultValueTypeClass);
end;

destructor TcxGridChartCategories.Destroy;
begin
  FDataBinding.Free;
  inherited;
end;

function TcxGridChartCategories.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

function TcxGridChartCategories.GetID: Integer;
begin
  Result := FDataBinding.ID;
end;

function TcxGridChartCategories.GetSortOrder: TcxDataSortOrder;
begin
  Result := FDataBinding.SortOrder;
end;

function TcxGridChartCategories.GetValue(Index: Integer): Variant;
begin
  Result := GridView.ViewData.Categories[Index];
end;

function TcxGridChartCategories.GetValueCount: Integer;
begin
  Result := GridView.ViewData.CategoryCount;
end;

function TcxGridChartCategories.GetVisibleDisplayText(Index: Integer): string;
begin
  Result := GetValueDisplayText(VisibleValues[Index]);
end;

function TcxGridChartCategories.GetVisibleValue(Index: Integer): Variant;
begin
  Result := GridView.ViewData.VisibleCategories[Index];
end;

function TcxGridChartCategories.GetVisibleValueCount: Integer;
begin
  Result := GridView.ViewData.VisibleCategoryCount;
end;

procedure TcxGridChartCategories.SetDataBinding(Value: TcxGridChartItemDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxGridChartCategories.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
  begin
    FDisplayText := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartCategories.SetOnGetValueDisplayText(Value: TcxGridChartGetValueDisplayTextEvent);
begin
  if not dxSameMethods(FOnGetValueDisplayText, Value) then
  begin
    FOnGetValueDisplayText := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartCategories.SetSortOrder(Value: TcxDataSortOrder);
begin
  FDataBinding.SortOrder := Value;
end;

procedure TcxGridChartCategories.SetValue(Index: Integer; const Value: Variant);
begin
  GridView.ViewData.Categories[Index] := Value;
end;

procedure TcxGridChartCategories.SetValueCount(Value: Integer);
begin
  GridView.ViewData.CategoryCount := Value;
end;

procedure TcxGridChartCategories.DataChanged;
begin
end;

function TcxGridChartCategories.GetDataBinding: TcxGridChartItemDataBinding;
begin
  Result := FDataBinding;
end;

procedure TcxGridChartCategories.ValueTypeClassChanged;
begin
  FDataBinding.ValueTypeClassChanged;
end;

procedure TcxGridChartCategories.DoGetValueDisplayText(const AValue: Variant;
  var ADisplayText: string);
begin
  if Assigned(FOnGetValueDisplayText) then
    FOnGetValueDisplayText(Self, AValue, ADisplayText);
end;

function TcxGridChartCategories.GetDefaultDisplayText: string;
begin
  Result := cxGetResourceString(@scxGridChartCategoriesDisplayText);
end;

function TcxGridChartCategories.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxStringValueType;
end;

procedure TcxGridChartCategories.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartCategories then
    with TcxGridChartCategories(Source) do
    begin
      Self.DataBinding := DataBinding;
      Self.DisplayText := DisplayText;
      Self.SortOrder := SortOrder;
      Self.OnGetValueDisplayText := OnGetValueDisplayText;
    end;
  inherited;
end;

function TcxGridChartCategories.GetDisplayText: string;
begin
  Result := FDisplayText;
  if Result = '' then
    Result := GetDefaultDisplayText;
end;

function TcxGridChartCategories.GetNamePath: string;
begin
  Result := GridView.GetCategoriesNamePath;
end;

function TcxGridChartCategories.GetValueDisplayText(const AValue: Variant): string;
begin
  if GridView.ActiveDataGroup = nil then
    Result := DataBinding.GetValueDisplayText(AValue)
  else
    Result := GridView.ActiveDataGroup.GetValueDisplayText(AValue);
  DoGetValueDisplayText(AValue, Result);
end;

{ TcxGridChartDataGroup }

function TcxGridChartDataGroup.GetActive: Boolean;
begin
  Result := GridView.ActiveDataGroup = Self;
end;

function TcxGridChartDataGroup.GetActiveValueDisplayText: string;
begin
  Result := GetValueDisplayText(FActiveValue);
end;

function TcxGridChartDataGroup.GetDataLevel: Integer;
begin
  Result := GridView.GetDataObjectLevel(Self);
end;

function TcxGridChartDataGroup.GetGroupIndex: Integer;
begin
  Result := DataBinding.GroupIndex;
end;

procedure TcxGridChartDataGroup.SetActive(Value: Boolean);
begin
  GridView.ActiveDataGroup := Self;
end;

procedure TcxGridChartDataGroup.SetActiveValue(const Value: Variant);
begin
  if not VarSameValue(FActiveValue, Value) then
  begin
    FActiveValue := Value;
    GridView.ViewData.DataLevelsChanged;
    GridView.UpdateDataGroupActiveValues;
    Changed;
  end;
end;

procedure TcxGridChartDataGroup.SetDataLevel(Value: Integer);
begin
  GridView.SetDataObjectLevel(Self, Value);
end;

procedure TcxGridChartDataGroup.SetGroupIndex(Value: Integer);
begin
  DataBinding.GroupIndex := Value;
end;

function TcxGridChartDataGroup.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('ActiveValue');
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxGridChartDataGroup.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ActiveValue' then
    AValue := ActiveValue
  else
    inherited;
end;

procedure TcxGridChartDataGroup.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ActiveValue' then
    ActiveValue := AValue
  else
    inherited;
end;

procedure TcxGridChartDataGroup.SetRestoredIndex(AValue: Integer);
begin
  GridView.RestoringDataGroups[AValue] := Self;
end;

procedure TcxGridChartDataGroup.CheckActiveValue;
begin
  if HasActiveValue then
    if Visible then
      GridView.ViewData.CheckValueAtLevel(VisibleIndex, FActiveValue)
    else
      FActiveValue := Null;
end;

function TcxGridChartDataGroup.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxVariantValueType;
end;

function TcxGridChartDataGroup.GetValue(AIndex: Integer): Variant;
begin
  Result := GridView.ViewData.GroupValues[Index, AIndex];
end;

function TcxGridChartDataGroup.GetValueCount: Integer;
begin
  Result := GridView.ViewData.CategoryCount;
end;

function TcxGridChartDataGroup.GetVisibleValue(AIndex: Integer): Variant;
begin
  Result := GridView.ViewData.VisibleGroupValues[VisibleIndex, AIndex];
end;

function TcxGridChartDataGroup.GetVisibleValueCount: Integer;
begin
  Result := GridView.ViewData.VisibleGroupValueCount[VisibleIndex];
end;

procedure TcxGridChartDataGroup.SetValue(AIndex: Integer; const Value: Variant);
begin
  GridView.ViewData.GroupValues[Index, AIndex] := Value;
end;

procedure TcxGridChartDataGroup.SetValueCount(Value: Integer);
begin
  GridView.ViewData.CategoryCount := Value;
end;

function TcxGridChartDataGroup.CanMove: Boolean;
begin
  Result := IsDesigning or GridView.OptionsCustomize.DataGroupMoving;
end;

function TcxGridChartDataGroup.HasActiveValue: Boolean;
begin
  Result := not VarIsEmpty(FActiveValue);
end;

class function TcxGridChartDataGroup.IsValue: Boolean;
begin
  Result := False;
end;

{ TcxGridChartSeriesStyles }

constructor TcxGridChartSeriesStyles.Create(AOwner: TPersistent);
begin
  inherited;
  BitmapInViewParams := True;
end;

function TcxGridChartSeriesStyles.GetSeries: TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(Owner);
end;

procedure TcxGridChartSeriesStyles.SetOnGetValueStyle(Value: TcxGridChartSeriesGetValueStyleEvent);
begin
  if not dxSameMethods(FOnGetValueStyle, Value) then
  begin
    FOnGetValueStyle := Value;
    Series.Changed(vcProperty);
  end;
end;

procedure TcxGridChartSeriesStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  if Index = ssValues then
  begin
    if Integer(AData) = -1 then
      AParams.Color := GetDefaultValueColor(Series.Order)
    else
      AParams.Color := GetDefaultValueColor(Integer(AData));
    AParams.TextColor := LookAndFeelPainter.DefaultChartDiagramValueBorderColor;
  end;
end;

function TcxGridChartSeriesStyles.GetGridView: TcxCustomGridView;
begin
  Result := Series.GridView;
end;

procedure TcxGridChartSeriesStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartSeriesStyles then
    with TcxGridChartSeriesStyles(Source) do
    begin
      Self.Values := Values;
      Self.OnGetValueStyle := OnGetValueStyle;
    end;
end;

procedure TcxGridChartSeriesStyles.GetValueParams(AValueIndex: Integer;
  AVaryColorsByCategory: Boolean; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (AValueIndex <> -1) and Assigned(FOnGetValueStyle) then
    FOnGetValueStyle(Series, AValueIndex, AStyle);
  if not AVaryColorsByCategory then AValueIndex := -1;
  GetViewParams(ssValues, TObject(AValueIndex), AStyle, AParams);
end;

{ TcxGridChartSeries }

constructor TcxGridChartSeries.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := GetStylesClass.Create(Self);
end;

destructor TcxGridChartSeries.Destroy;
begin
  FStyles.Free;
  inherited;
end;

function TcxGridChartSeries.GetGroupSummaryKind: TcxSummaryKind;
begin
  Result := DataBinding.SummaryKind;
end;

function TcxGridChartSeries.GetSumOfValues: Variant;
begin
  Result := GridView.ViewData.SumOfValues[Index];
end;

procedure TcxGridChartSeries.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    FGridView.ItemVisibilityChanged(Self);
    Changed;
  end;
end;

procedure TcxGridChartSeries.SetGroupSummaryKind(Value: TcxSummaryKind);
begin
  if IsGroupSummaryKindValid(Value) then
    DataBinding.SummaryKind := Value;
end;

procedure TcxGridChartSeries.SetStyles(Value: TcxGridChartSeriesStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxGridChartSeries.SetValueCaptionFormat(const Value: string);
begin
  if FValueCaptionFormat <> Value then
  begin
    FValueCaptionFormat := Value;
    Changed;
  end;
end;

procedure TcxGridChartSeries.SetOnCustomDrawValue(Value: TcxGridChartSeriesValueCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawValue, Value) then
  begin
    FOnCustomDrawValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartSeries.SetOnValueClick(Value: TcxGridChartValueClickEvent);
begin
  if not dxSameMethods(FOnValueClick, Value) then
  begin
    FOnValueClick := Value;
    Changed(vcProperty);
  end;
end;

function TcxGridChartSeries.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('SortOrder');
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxGridChartSeries.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'SortOrder' then
    AValue := Variant(SortOrder)
  else
    inherited;
end;

procedure TcxGridChartSeries.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'SortOrder' then
    SortOrder := TcxDataSortOrder((AValue))
  else
    inherited;
end;

procedure TcxGridChartSeries.SetRestoredIndex(AValue: Integer);
begin
  GridView.RestoringSeries[AValue] := Self;
end;

procedure TcxGridChartSeries.DoGetValueDisplayText(const AValue: Variant;
  var ADisplayText: string);
begin
  if (ValueCaptionFormat <> '') and not VarIsNull(AValue) then
    ADisplayText := FormatFloat(ValueCaptionFormat, AValue);
  inherited;
end;

function TcxGridChartSeries.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxFloatValueType;
end;

function TcxGridChartSeries.GetStylesClass: TcxGridChartSeriesStylesClass;
begin
  Result := TcxGridChartSeriesStyles;
end;

function TcxGridChartSeries.IsGroupSummaryKindValid(AValue: TcxSummaryKind): Boolean;
begin
  Result := AValue <> skNone;
end;

function TcxGridChartSeries.GetValue(AIndex: Integer): Variant;
begin
  Result := GridView.ViewData.Values[Index, AIndex];
end;

function TcxGridChartSeries.GetValueCount: Integer;
begin
  Result := GridView.ViewData.CategoryCount;
end;

function TcxGridChartSeries.GetVisibleValue(AIndex: Integer): Variant;
begin
  Result := GridView.ViewData.VisibleValues[Index, AIndex];
end;

function TcxGridChartSeries.GetVisibleValueCount: Integer;
begin
  Result := GridView.ViewData.VisibleCategoryCount;
end;

procedure TcxGridChartSeries.SetValue(AIndex: Integer; const Value: Variant);
begin
  GridView.ViewData.Values[Index, AIndex] := Value;
end;

procedure TcxGridChartSeries.SetValueCount(Value: Integer);
begin
  GridView.ViewData.CategoryCount := Value;
end;

procedure TcxGridChartSeries.DoCustomDrawValue(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawValue then
    FOnCustomDrawValue(Self, ACanvas, AViewInfo, ADone);
end;

function TcxGridChartSeries.DoValueClick(AValueIndex: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnValueClick) then
    FOnValueClick(GridView, Self, AValueIndex, Result);
  if not Result then
    Result := GridView.DoValueClick(Self, AValueIndex);
end;

function TcxGridChartSeries.HasCustomDrawValue: Boolean;
begin
  Result := Assigned(FOnCustomDrawValue);
end;

procedure TcxGridChartSeries.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartSeries then
    with TcxGridChartSeries(Source) do
    begin
      Self.GroupSummaryKind := GroupSummaryKind;
      Self.Styles := Styles;
      Self.ValueCaptionFormat := ValueCaptionFormat;
      Self.OnCustomDrawValue := OnCustomDrawValue;
      Self.OnValueClick := OnValueClick;
    end
end;

function TcxGridChartSeries.AddValue(const AValue: Variant): Integer;
begin
  ValueCount := ValueCount + 1;
  Result := ValueCount - 1;
  Values[Result] := AValue;
end;

class function TcxGridChartSeries.IsValue: Boolean;
begin
  Result := True;
end;

{ TcxGridChartTitle }

function TcxGridChartTitle.GetDefaultPosition: TcxGridChartPartPosition;
begin
  Result := cppTop;
end;

{ TcxGridChartToolBox }

constructor TcxGridChartToolBox.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FBorder := tbSingle;
  FDataLevelActiveValueDropDownCount := cxGridChartDefaultDataLevelActiveValueDropDownCount;
  FDataLevelsInfoVisible := dlivNonEmpty;
  FVisible := tvNonEmpty;
end;

function TcxGridChartToolBox.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartToolBox.SetBorder(Value: TcxGridChartToolBoxBorder);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.SetCustomizeButton(Value: Boolean);
begin
  if FCustomizeButton <> Value then
  begin
    FCustomizeButton := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.SetDataLevelActiveValueDropDownCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FDataLevelActiveValueDropDownCount <> Value then
  begin
    FDataLevelActiveValueDropDownCount := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartToolBox.SetDataLevelActiveValueDropDownWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FDataLevelActiveValueDropDownWidth <> Value then
  begin
    FDataLevelActiveValueDropDownWidth := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartToolBox.SetDataLevelsInfoVisible(Value: TcxGridChartDataLevelsInfoVisible);
begin
  if FDataLevelsInfoVisible <> Value then
  begin
    FDataLevelsInfoVisible := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.SetDiagramSelector(Value: Boolean);
begin
  if FDiagramSelector <> Value then
  begin
    FDiagramSelector := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.SetPosition(Value: TcxGridChartToolBoxPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.SetVisible(Value: TcxGridChartToolBoxVisible);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartToolBox.GetStoredProperties(AProperties: TStrings);
begin
  AProperties.Add('ToolBox.Border');
  AProperties.Add('ToolBox.DiagramSelector');
  AProperties.Add('ToolBox.Position');
  inherited;
end;

procedure TcxGridChartToolBox.GetStoredPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'ToolBox.Border' then
    AValue := Variant(Border)
  else
    if AName = 'ToolBox.DiagramSelector' then
      AValue := DiagramSelector
    else
      if AName = 'ToolBox.Position' then
        AValue := Variant(Position);
  inherited;
end;

procedure TcxGridChartToolBox.SetStoredPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'ToolBox.Border' then
    Border := TcxGridChartToolBoxBorder((AValue))
  else
    if AName = 'ToolBox.DiagramSelector' then
      DiagramSelector := AValue
    else
      if AName = 'ToolBox.Position' then
        Position := TcxGridChartToolBoxPosition((AValue));
  inherited;
end;

function TcxGridChartToolBox.IsDataLevelsInfoNonEmpty: Boolean;
begin
  Result := GridView.IsDataGrouped;
end;

function TcxGridChartToolBox.IsNonEmpty: Boolean;
begin
  Result := GetDataLevelsInfoVisible or CustomizeButton or DiagramSelector;
end;

procedure TcxGridChartToolBox.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartToolBox then
    with TcxGridChartToolBox(Source) do
    begin
      Self.Border := Border;
      Self.CustomizeButton := CustomizeButton;
      Self.DataLevelActiveValueDropDownCount := DataLevelActiveValueDropDownCount;
      Self.DataLevelActiveValueDropDownWidth := DataLevelActiveValueDropDownWidth;
      Self.DataLevelsInfoVisible := DataLevelsInfoVisible;
      Self.DiagramSelector := DiagramSelector;
      Self.Position := Position;
      Self.Visible := Visible;
    end;
end;

function TcxGridChartToolBox.GetDataLevelsInfoVisible: Boolean;
begin
  Result := (FDataLevelsInfoVisible = dlivAlways) or
    (FDataLevelsInfoVisible = dlivNonEmpty) and IsDataLevelsInfoNonEmpty;
end;

function TcxGridChartToolBox.GetVisible: Boolean;
begin
  Result := (FVisible = tvAlways) or (FVisible = tvNonEmpty) and IsNonEmpty;
end;

{ TcxGridChartOptionsBehavior }

constructor TcxGridChartOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FValueHints := True;
end;

function TcxGridChartOptionsBehavior.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartOptionsBehavior.SetValueHints(Value: Boolean);
begin
  if FValueHints <> Value then
  begin
    FValueHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsBehavior.SetValueHotTrack(Value: TcxGridChartValueHotTrack);
begin
  if FValueHotTrack <> Value then
  begin
    FValueHotTrack := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartOptionsBehavior.GetStoredProperties(AProperties: TStrings);
begin
  AProperties.Add('OptionsBehavior.ValueHints');
  inherited;
end;

procedure TcxGridChartOptionsBehavior.GetStoredPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'OptionsBehavior.ValueHints' then
    AValue := ValueHints;
  inherited;
end;

procedure TcxGridChartOptionsBehavior.SetStoredPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'OptionsBehavior.ValueHints' then
    ValueHints := AValue;
  inherited;
end;

function TcxGridChartOptionsBehavior.GetDefaultValueHotTrack(AValueIndex: Integer): Boolean;
begin
  Result := GridView.Controller.IsDataDrillDownPossible(AValueIndex);
end;

procedure TcxGridChartOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxGridChartOptionsBehavior then
    with TcxGridChartOptionsBehavior(Source) do
    begin
      Self.ValueHints := ValueHints;
      Self.ValueHotTrack := ValueHotTrack;
    end;
  inherited;
end;

function TcxGridChartOptionsBehavior.GetValueHotTrack(AValueIndex: Integer): Boolean;
begin
  Result := GridView.ActiveDiagram.SupportsValueHotTrack and
    ((FValueHotTrack = vhAlways) or
     (FValueHotTrack = vhDefault) and GetDefaultValueHotTrack(AValueIndex));
end;

{ TcxGridChartOptionsCustomize }

constructor TcxGridChartOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FDataDrillDown := True;
  FDataDrillUpMethod := ddumMouseRightButtonClick;
  FDataGroupMoving := True;
  FOptionsCustomization := True;
  FSeriesCustomization := True;
end;

procedure TcxGridChartOptionsCustomize.SetDataDrillDown(Value: Boolean);
begin
  if FDataDrillDown <> Value then
  begin
    FDataDrillDown := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartOptionsCustomize.SetDataDrillUpMethod(Value: TcxGridChartDataDrillUpMethod);
begin
  if FDataDrillUpMethod <> Value then
  begin
    FDataDrillUpMethod := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsCustomize.SetDataGroupHiding(Value: Boolean);
begin
  if FDataGroupHiding <> Value then
  begin
    FDataGroupHiding := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsCustomize.SetDataGroupMoving(Value: Boolean);
begin
  if FDataGroupMoving <> Value then
  begin
    FDataGroupMoving := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsCustomize.SetOptionsCustomization(Value: Boolean);
begin
  if FOptionsCustomization <> Value then
  begin
    FOptionsCustomization := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsCustomize.SetSeriesCustomization(Value: Boolean);
begin
  if FSeriesCustomization <> Value then
  begin
    FSeriesCustomization := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartOptionsCustomize.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartOptionsCustomize then
    with TcxGridChartOptionsCustomize(Source) do
    begin
      Self.DataDrillDown := DataDrillDown;
      Self.DataDrillUpMethod := DataDrillUpMethod;
      Self.DataGroupHiding := DataGroupHiding;
      Self.DataGroupMoving := DataGroupMoving;
      Self.OptionsCustomization := OptionsCustomization;
      Self.SeriesCustomization := SeriesCustomization;
    end;
end;

{ TcxGridChartOptionsView }

constructor TcxGridChartOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  Antialiasing := True;
  FTransparentCaptions := True;
end;

procedure TcxGridChartOptionsView.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartOptionsView then
    with TcxGridChartOptionsView(Source) do
    begin
      Self.Antialiasing := Antialiasing;
      Self.CategoriesPerPage := CategoriesPerPage;
      Self.TransparentCaptions := TransparentCaptions;
    end;
end;

function TcxGridChartOptionsView.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartOptionsView.SetAntialiasing(Value: Boolean);
begin
  if FAntialiasing <> Value then
  begin
    FAntialiasing := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartOptionsView.SetCategoriesPerPage(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCategoriesPerPage <> Value then
  begin
    FCategoriesPerPage := Value;
    GridView.Controller.FirstVisibleCategoryIndex := 0;
    Changed(vcLayout);
  end;
end;

procedure TcxGridChartOptionsView.SetTransparentCaptions(Value: Boolean);
begin
  if FTransparentCaptions <> Value then
  begin
    FTransparentCaptions := Value;
    Changed(vcLayout);
  end;
end;
{ TcxGridChartViewStyles }

constructor TcxGridChartViewStyles.Create(AOwner: TPersistent);
begin
  inherited;
  BitmapInViewParams := True;
end;

procedure TcxGridChartViewStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      vsBackground:
        Color := DefaultContentColor;
      vsActiveDataLevelInfo:
        begin
          GetViewParams(vsDataLevelsInfo, nil, nil, AParams);
          Bitmap := nil;
          Color := DefaultSelectionColor;
          TextColor := DefaultSelectionTextColor;
        end;
      vsDataLevelActiveValueInfo:
        begin
          GetViewParams(vsDataLevelsInfo, nil, nil, AParams);
          if GridView.Controller.CanShowDataLevelActiveValuePopup(False) then
            TextColor := DefaultHyperlinkTextColor;
        end;
      vsDataLevelsInfo:
        GetViewParams(vsToolBox, nil, nil, AParams);
      vsDiagramSelector:
        begin
          GetViewParams(vsToolBox, nil, nil, AParams);
          TextColor := DefaultHyperlinkTextColor;
        end;
      vsLegend, vsToolBox:
        begin
          Color := DefaultContentColor;
          TextColor := DefaultContentTextColor;
        end;
      vsTitle:
        TextColor := DefaultContentTextColor;
    end;
end;

function TcxGridChartViewStyles.GetGridViewValue: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridChartViewStyles then
    with TcxGridChartViewStyles(Source) do
    begin
      Self.ActiveDataLevelInfo := ActiveDataLevelInfo;
      Self.DataLevelActiveValueInfo := DataLevelActiveValueInfo;
      Self.DataLevelsInfo := DataLevelsInfo;
      Self.DiagramSelector := DiagramSelector;
      Self.Legend := Legend;
      Self.Title := Title;
      Self.ToolBox := ToolBox;
    end;
end;

procedure TcxGridChartViewStyles.GetDataLevelInfoParams(ADataLevel: Integer;
  out AParams: TcxViewParams);
begin
  if GridView.ActiveDataLevel = ADataLevel then
    GetViewParams(vsActiveDataLevelInfo, nil, nil, AParams)
  else
    GetViewParams(vsDataLevelsInfo, nil, nil, AParams);
end;

{ TcxGridOpenChartItemList }

function TcxGridOpenChartItemList.GetItem(Index: Integer): TcxGridChartItem;
begin
  Result := TcxGridChartItem(inherited Items[Index]);
end;

procedure TcxGridOpenChartItemList.SetItem(Index: Integer; Value: TcxGridChartItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxGridChartView }

function TcxGridChartView.GetActiveDataGroup: TcxGridChartDataGroup;
begin
  if ActiveDataLevel < DataLevelCount - 1 then
    Result := VisibleDataGroups[ActiveDataLevel]
  else
    Result := nil;
end;

function TcxGridChartView.GetAvailableDiagramCount: Integer;
begin
  Result := FAvailableDiagrams.Count;
end;

function TcxGridChartView.GetAvailableDiagram(Index: Integer): TcxGridChartDiagram;
begin
  Result := TcxGridChartDiagram(FAvailableDiagrams[Index]);
end;

function TcxGridChartView.GetController: TcxGridChartController;
begin
  Result := TcxGridChartController(inherited Controller);
end;

function TcxGridChartView.GetDataController: TcxGridChartDataController;
begin
  Result := TcxGridChartDataController(FDataController);
end;

function TcxGridChartView.GetDataGroup(Index: Integer): TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(FDataGroups[Index]);
end;

function TcxGridChartView.GetDataGroupCount: Integer;
begin
  Result := FDataGroups.Count;
end;

function TcxGridChartView.GetDiagram(Index: Integer): TcxGridChartDiagram;
begin
  Result := TcxGridChartDiagram(FDiagrams[Index]);
end;

function TcxGridChartView.GetDiagramCount: Integer;
begin
  Result := FDiagrams.Count;
end;

function TcxGridChartView.GetItem(Index: Integer): IcxGridChartItem;
begin
  FDataController.GetItem(Index).GetInterface(IcxGridChartItem, Result);
end;

function TcxGridChartView.GetItemCount: Integer;
begin
  Result := FDataController.ItemCount;
end;

function TcxGridChartView.GetOptionsBehavior: TcxGridChartOptionsBehavior;
begin
  Result := TcxGridChartOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridChartView.GetOptionsView: TcxGridChartOptionsView;
begin
  Result := TcxGridChartOptionsView(inherited OptionsView);
end;

function TcxGridChartView.GetSeries(Index: Integer): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(FSeries[Index]);
end;

function TcxGridChartView.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

function TcxGridChartView.GetSortedSeries: TcxGridChartSeries;
var
  I: Integer;
begin
  for I := 0 to SeriesCount - 1 do
  begin
    Result := Series[I];
    if Result.SortOrder <> soNone then Exit;
  end;
  Result := nil;
end;

function TcxGridChartView.GetStyles: TcxGridChartViewStyles;
begin
  Result := TcxGridChartViewStyles(inherited Styles);
end;

function TcxGridChartView.GetViewData: TcxGridChartViewData;
begin
  Result := TcxGridChartViewData(inherited ViewData);
end;

function TcxGridChartView.GetViewInfo: TcxGridChartViewInfo;
begin
  Result := TcxGridChartViewInfo(inherited ViewInfo);
end;

function TcxGridChartView.GetVisibleDataGroup(Index: Integer): TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(FVisibleDataGroups[Index]);
end;

function TcxGridChartView.GetVisibleDataGroupCount: Integer;
begin
  Result := FVisibleDataGroups.Count;
end;

function TcxGridChartView.GetVisibleSeries(Index: Integer): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(FVisibleSeries[Index]);
end;

function TcxGridChartView.GetVisibleSeriesCount: Integer;
begin
  Result := FVisibleSeries.Count;
end;

function TcxGridChartView.GetVisibleSeriesGroupCount: Integer;
begin
  Result := FVisibleSeriesGroups.Count;
end;

function TcxGridChartView.GetVisibleSeriesGroupIndex(Index: Integer): Integer;
begin
  Result := Integer(FVisibleSeriesGroups[Index]);
end;

procedure TcxGridChartView.SetActiveDataGroup(Value: TcxGridChartDataGroup);
begin
  if Value = nil then
    ActiveDataLevel := DataLevelCount - 1
  else
    if Value.Visible then
      ActiveDataLevel := Value.VisibleIndex;
end;

procedure TcxGridChartView.SetActiveDataLevel(Value: Integer);
var
  APrevActiveDataLevel: Integer;
begin
  if Value < 0 then Value := 0;
  if Value > DataLevelCount - 1 then Value := DataLevelCount - 1;
  Value := GetAvailableDataLevel(Value);
  if FActiveDataLevel <> Value then
  begin
    APrevActiveDataLevel := FActiveDataLevel;
    FActiveDataLevel := Value;
    ViewData.DataLevelsChanged;
    Changed(vcLayout);
    ActiveDataLevelChanged(APrevActiveDataLevel);
  end;
end;

procedure TcxGridChartView.SetActiveDiagram(Value: TcxGridChartDiagram);
begin
  if Value = nil then
    Value := GetFirstAvailableDiagram;
  if (FActiveDiagram <> Value) and ((Value = nil) or Value.IsAvailable) then
  begin
    FActiveDiagram := Value;
    UpdateSeriesVisibleGroups;
    Changed(vcLayout);
    ActiveDiagramChanged(FActiveDiagram);
  end;
end;

procedure TcxGridChartView.SetCategories(Value: TcxGridChartCategories);
begin
  FCategories.Assign(Value);
end;

procedure TcxGridChartView.SetDataController(Value: TcxGridChartDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridChartView.SetDataGroup(Index: Integer; Value: TcxGridChartDataGroup);
begin
  DataGroups[Index].Assign(Value);
end;

procedure TcxGridChartView.SetDiagramArea(Value: TcxGridChartAreaDiagram);
begin
  FDiagramArea.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramBar(Value: TcxGridChartBarDiagram);
begin
  FDiagramBar.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramColumn(Value: TcxGridChartColumnDiagram);
begin
  FDiagramColumn.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramLine(Value: TcxGridChartLineDiagram);
begin
  FDiagramLine.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramPie(Value: TcxGridChartPieDiagram);
begin
  FDiagramPie.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramStackedArea(Value: TcxGridChartStackedAreaDiagram);
begin
  FDiagramStackedArea.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramStackedBar(Value: TcxGridChartStackedBarDiagram);
begin
  FDiagramStackedBar.Assign(Value);
end;

procedure TcxGridChartView.SetDiagramStackedColumn(Value: TcxGridChartStackedColumnDiagram);
begin
  FDiagramStackedColumn.Assign(Value);
end;

procedure TcxGridChartView.SetLegend(Value: TcxGridChartLegend);
begin
  FLegend.Assign(Value);
end;

procedure TcxGridChartView.SetOptionsBehavior(Value: TcxGridChartOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxGridChartView.SetOptionsCustomize(Value: TcxGridChartOptionsCustomize);
begin
  FOptionsCustomize.Assign(Value);
end;

procedure TcxGridChartView.SetOptionsView(Value: TcxGridChartOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxGridChartView.SetSeries(Index: Integer; Value: TcxGridChartSeries);
begin
  Series[Index].Assign(Value);
end;

procedure TcxGridChartView.SetSortedSeries(Value: TcxGridChartSeries);
begin
  if SortedSeries <> Value then
    if Value = nil then
      SortedSeries.SortOrder := soNone
    else
      Value.SortOrder := soAscending;
end;

procedure TcxGridChartView.SetStyles(Value: TcxGridChartViewStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridChartView.SetTitle(Value: TcxGridChartTitle);
begin
  FTitle.Assign(Value);
end;

procedure TcxGridChartView.SetToolBox(Value: TcxGridChartToolBox);
begin
  FToolBox.Assign(Value);
end;

procedure TcxGridChartView.SetOnActiveDataLevelChanged(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnActiveDataLevelChanged, Value) then
  begin
    FOnActiveDataLevelChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnActiveDiagramChanged(Value: TcxGridChartDiagramEvent);
begin
  if not dxSameMethods(FOnActiveDiagramChanged, Value) then
  begin
    FOnActiveDiagramChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnCustomDrawLegend(Value: TcxGridChartLegendCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawLegend, Value) then
  begin
    FOnCustomDrawLegend := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnCustomDrawLegendItem(Value: TcxGridChartLegendItemCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawLegendItem, Value) then
  begin
    FOnCustomDrawLegendItem := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnDataGroupPosChanged(Value: TcxGridChartDataGroupEvent);
begin
  if not dxSameMethods(FOnDataGroupPosChanged, Value) then
  begin
    FOnDataGroupPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnFirstVisibleCategoryIndexChanged(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnFirstVisibleCategoryIndexChanged, Value) then
  begin
    FOnFirstVisibleCategoryIndexChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnGetValueHint(Value: TcxGridChartGetValueHintEvent);
begin
  if not dxSameMethods(FOnGetValueHint, Value) then
  begin
    FOnGetValueHint := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnSeriesPosChanged(Value: TcxGridChartSeriesEvent);
begin
  if not dxSameMethods(FOnSeriesPosChanged, Value) then
  begin
    FOnSeriesPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridChartView.SetOnValueClick(Value: TcxGridChartValueClickEvent);
begin
  if not dxSameMethods(FOnValueClick, Value) then
  begin
    FOnValueClick := Value;
    Changed(vcProperty);
  end;
end;

function TcxGridChartView.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('ActiveDataLevel');
  AProperties.Add('ActiveDiagram');
  Legend.GetStoredProperties(AProperties);
  OptionsBehavior.GetStoredProperties(AProperties);
  Title.GetStoredProperties(AProperties);
  ToolBox.GetStoredProperties(AProperties);
  Result := inherited GetProperties(AProperties);
end;

procedure TcxGridChartView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ActiveDataLevel' then
    AValue := ActiveDataLevel
  else
    if AName = 'ActiveDiagram' then
      if ActiveDiagram = nil then
        AValue := ''
      else
        AValue := ActiveDiagram.ID
    else
    begin
      Legend.GetStoredPropertyValue(AName, AValue);
      OptionsBehavior.GetStoredPropertyValue(AName, AValue);
      Title.GetStoredPropertyValue(AName, AValue);
      ToolBox.GetStoredPropertyValue(AName, AValue);
    end;
  inherited;
end;

procedure TcxGridChartView.SetPropertyValue(const AName: string; const AValue: Variant);
var
  ADiagram: TcxGridChartDiagram;
begin
  if AName = 'ActiveDataLevel' then
    FRestoredActiveDataLevel := AValue
  else
    if AName = 'ActiveDiagram' then
    begin
      ADiagram := FindDiagramByID(AValue);
      if ADiagram <> nil then
        ADiagram.Active := True;
    end
    else
    begin
      Legend.SetStoredPropertyValue(AName, AValue);
      OptionsBehavior.SetStoredPropertyValue(AName, AValue);
      Title.SetStoredPropertyValue(AName, AValue);
      ToolBox.SetStoredPropertyValue(AName, AValue);
    end;
  inherited;
end;

function TcxGridChartView.CreateStoredObject(const AObjectName, AClassName: string): TObject;
begin
  if AClassName = GetDataGroupClass.ClassName then
    Result := CreateDataGroup
  else
    if AClassName = GetSeriesClass.ClassName then
      Result := CreateSeries
    else
      Result := inherited CreateStoredObject(AObjectName, AClassName);
end;

procedure TcxGridChartView.GetStoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to DataGroupCount - 1 do
    AChildren.AddObject('', DataGroups[I]);
  for I := 0 to SeriesCount - 1 do
    AChildren.AddObject('', Series[I]);
  inherited;
end;

procedure TcxGridChartView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
  inherited;
  Assign(ALayoutView);
end;

function TcxGridChartView.GetLayoutCustomizationFormButtonCaption: string;
begin
  Result := 'Chart Customization...';
end;

function TcxGridChartView.HasLayoutCustomizationForm: Boolean;
begin
  Result := True;
end;

procedure TcxGridChartView.CreateHandlers;
begin
  FCategories := GetCategoriesClass.Create(Self);
  FDiagrams := TList.Create;
  FAvailableDiagrams := TList.Create;
  FDataGroups := TList.Create;
  FSeries := TList.Create;
  FVisibleDataGroups := TList.Create;
  FVisibleSeries := TList.Create;
  FVisibleSeriesGroups := TList.Create;
  CreateDiagrams;
  inherited;
  UpdateDataController(FCategories, FCategories.DataBinding, True);
end;

procedure TcxGridChartView.DestroyHandlers;
begin
  ClearDiagrams;
  ClearDataGroups;
  ClearSeries;
  UpdateDataController(FCategories, FCategories.DataBinding, False);
  inherited;
  FreeAndNil(FVisibleSeries);
  FreeAndNil(FVisibleSeriesGroups);
  FreeAndNil(FVisibleDataGroups);
  FreeAndNil(FSeries);
  FreeAndNil(FDataGroups);
  FreeAndNil(FAvailableDiagrams);
  FreeAndNil(FDiagrams);
  FreeAndNil(FCategories);
end;

procedure TcxGridChartView.CreateOptions;
begin
  inherited;
  FLegend := GetLegendClass.Create(Self);
  FOptionsCustomize := GetOptionsCustomizeClass.Create(Self);
  FTitle := GetTitleClass.Create(Self);
  FToolBox := GetToolBoxClass.Create(Self);
end;

procedure TcxGridChartView.DestroyOptions;
begin
  FreeAndNil(FToolBox);
  FreeAndNil(FTitle);
  FreeAndNil(FOptionsCustomize);
  FreeAndNil(FLegend);
  inherited;
end;

procedure TcxGridChartView.ActiveDataLevelChanged(APrevActiveDataLevel: Integer);
begin
  if IsDestroying then Exit;
  DoActiveDataLevelChanged;
  Controller.ActiveDataLevelChanged(APrevActiveDataLevel, ActiveDataLevel);
end;

procedure TcxGridChartView.ActiveDiagramChanged(ADiagram: TcxGridChartDiagram);
begin
  if IsDestroying then Exit;
  DoActiveDiagramChanged(ADiagram);
  if Controller <> nil then
    Controller.ActiveDiagramChanged(ADiagram);
end;

procedure TcxGridChartView.BeforeAssign(ASource: TcxCustomGridView);
begin
  inherited;
  UpdateSummaryItemValues;
end;

procedure TcxGridChartView.AfterAssign(ASource: TcxCustomGridView);
begin
  inherited;
  UpdateSummaryItemValues;
end;

procedure TcxGridChartView.DoAssign(ASource: TcxCustomGridView);

  procedure AssignItems(ASource: TcxGridChartView; AItemClass: TcxGridChartItemClass; AItemKind: TcxGridChartItemKind);
  var
    ASourceItems, AItems: TList;
    I: Integer;
    ASourceItem, AItem: TcxGridChartItem;
  begin
    ASourceItems := ASource.GetItemList(AItemKind);
    for I := 0 to ASourceItems.Count - 1 do
    begin
      ASourceItem := TcxGridChartItem(ASourceItems[I]);
      AItem := FindItemByID(AItemClass, ASourceItem.ID);
      if AItem = nil then
      begin
        AItem := CreateItem(AItemClass);
        AItem.DataBinding.ID := ASourceItem.ID;
      end;
      AItem.FOrder := ASourceItem.Order;
      AItem.Index := I;
      AItem.Assign(ASourceItem);
    end;
    AItems := GetItemList(AItemClass);
    for I := AItems.Count - 1 downto ASourceItems.Count do
      TcxGridChartItem(AItems[I]).Free;
  end;

begin
  if ASource is TcxGridChartView then
  begin
    if not AssigningSettings then
    begin
      Categories := TcxGridChartView(ASource).Categories;
      Categories.DataBinding.ID := TcxGridChartView(ASource).Categories.ID;
      AssignItems(TcxGridChartView(ASource), GetDataGroupClass, cikDataGroup);
      AssignItems(TcxGridChartView(ASource), GetSeriesClass, cikSeries);
    end;
    with TcxGridChartView(ASource) do
    begin
      Self.DiagramArea := DiagramArea;
      Self.DiagramBar := DiagramBar;
      Self.DiagramColumn := DiagramColumn;
      Self.DiagramLine := DiagramLine;
      Self.DiagramPie := DiagramPie;
      Self.DiagramStackedBar := DiagramStackedBar;
      Self.DiagramStackedArea := DiagramStackedArea;
      Self.DiagramStackedColumn := DiagramStackedColumn;
      Self.Legend := Legend;
      Self.OptionsCustomize := OptionsCustomize;
      Self.Title := Title;
      Self.ToolBox := ToolBox;
      Self.OnActiveDataLevelChanged := OnActiveDataLevelChanged;
      Self.OnActiveDiagramChanged := OnActiveDiagramChanged;
      Self.OnCustomDrawLegend := OnCustomDrawLegend;
      Self.OnCustomDrawLegendItem := OnCustomDrawLegendItem;
      Self.OnDataGroupPosChanged := OnDataGroupPosChanged;
      Self.OnFirstVisibleCategoryIndexChanged := OnFirstVisibleCategoryIndexChanged;
      Self.OnGetValueHint := OnGetValueHint;
      Self.OnSeriesPosChanged := OnSeriesPosChanged;
      Self.OnValueClick := OnValueClick;
    end;
  end;
  inherited;
end;

procedure TcxGridChartView.BeforeRestoring;
begin
  inherited;
  FRestoringDataGroups := TcxGridOpenChartItemList.Create;
  FRestoringSeries := TcxGridOpenChartItemList.Create;
end;

procedure TcxGridChartView.AfterRestoring;

  procedure AssignItemIndexes(AItems: TcxGridOpenChartItemList);
  var
    I, J: Integer;
  begin
    J := 0;
    for I := 0 to AItems.Count - 1 do
      if AItems[I] <> nil then
      begin
        AItems[I].Index := J;
        Inc(J);
      end;
  end;

begin
  try
    AssignItemIndexes(RestoringDataGroups);
    AssignItemIndexes(RestoringSeries);
  finally
    FRestoringSeries.Free;
    FRestoringDataGroups.Free;
    inherited;
  end;
end;

procedure TcxGridChartView.ChangeScale(M: Integer; D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DiagramCount - 1 do
    Diagrams[I].ChangeScale(M, D);
end;

procedure TcxGridChartView.DataControllerUnlocked;
begin
  if DataGroupActiveValuesUpdateNeeded then
    UpdateDataGroupActiveValues;
end;

procedure TcxGridChartView.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DataGroupCount - 1 do
    if DataGroups[I].Owner = Root then Proc(DataGroups[I]);
  for I := 0 to SeriesCount - 1 do
    if Series[I].Owner = Root then Proc(Series[I]);
end;

procedure TcxGridChartView.GetFakeComponentLinks(AList: TList);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DiagramCount - 1 do
    Diagrams[I].Styles.GetFakeComponentLinks(AList);
  for I := 0 to SeriesCount - 1 do
    Series[I].Styles.GetFakeComponentLinks(AList);
end;

procedure TcxGridChartView.RestoringComplete;
begin
  inherited;
  ActiveDataLevel := FRestoredActiveDataLevel;
end;

procedure TcxGridChartView.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited;
  if Child is GetDataGroupClass then
    TcxGridChartDataGroup(Child).Index := Order;
  if Child is GetSeriesClass then
    TcxGridChartSeries(Child).Index := Order - DataGroupCount;
end;

function ChartViewGetDataGroup(ACaller: TComponent; Index: Integer): TComponent;
begin
  Result := TcxGridChartView(ACaller).DataGroups[Index];
end;

function ChartViewGetSeries(ACaller: TComponent; Index: Integer): TComponent;
begin
  Result := TcxGridChartView(ACaller).Series[Index];
end;

procedure TcxGridChartView.SetName(const NewName: TComponentName);
var
  AOldName: TComponentName;
begin
  AOldName := Name;
  inherited;
  if Name <> AOldName then
  begin
    RenameComponents(Self, Owner, Name, AOldName, DataGroupCount, @ChartViewGetDataGroup);
    RenameComponents(Self, Owner, Name, AOldName, SeriesCount, @ChartViewGetSeries);
  end;
end;

procedure TcxGridChartView.UpdateControl(AInfo: TcxUpdateControlInfo);
var
  I: Integer;
begin
  ViewData.Update(AInfo);
  if not (IsLoading or IsDestroying) and
    not (AInfo is TcxFocusedRecordChangedInfo) and not (AInfo is TcxFocusedRowChangedInfo) then
  begin
    LayoutChanged;
    if (AInfo is TcxDataChangedInfo) or (AInfo is TcxLayoutChangedInfo) then
    begin
      Synchronize;
      if AInfo is TcxDataChangedInfo then
        for I := 0 to ItemCount - 1 do
          Items[I].DataChanged;
    end;
  end;
  inherited;
end;

function TcxGridChartView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridChartController;
end;

function TcxGridChartView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridChartDataController;
end;

function TcxGridChartView.GetItemDataBindingClass: TcxGridChartItemDataBindingClass;
begin
  Result := TcxGridChartItemDataBinding;
end;

function TcxGridChartView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridChartPainter;
end;

function TcxGridChartView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridChartViewData;
end;

function TcxGridChartView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridChartViewInfo;
end;

function TcxGridChartView.GetLegendClass: TcxGridChartLegendClass;
begin
  Result := TcxGridChartLegend;
end;

function TcxGridChartView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridChartOptionsBehavior;
end;

function TcxGridChartView.GetOptionsCustomizeClass: TcxGridChartOptionsCustomizeClass;
begin
  Result := TcxGridChartOptionsCustomize;
end;

function TcxGridChartView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridChartOptionsView;
end;

function TcxGridChartView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridChartViewStyles;
end;

function TcxGridChartView.GetTitleClass: TcxGridChartTitleClass;
begin
  Result := TcxGridChartTitle;
end;

function TcxGridChartView.GetToolBoxClass: TcxGridChartToolBoxClass;
begin
  Result := TcxGridChartToolBox;
end;

function TcxGridChartView.GetAreaDiagramClass: TcxGridChartAreaDiagramClass;
begin
  Result := TcxGridChartAreaDiagram;
end;

function TcxGridChartView.GetBarDiagramClass: TcxGridChartBarDiagramClass;
begin
  Result := TcxGridChartBarDiagram;
end;

function TcxGridChartView.GetCategoriesClass: TcxGridChartCategoriesClass;
begin
  Result := TcxGridChartCategories;
end;

function TcxGridChartView.GetColumnDiagramClass: TcxGridChartColumnDiagramClass;
begin
  Result := TcxGridChartColumnDiagram;
end;

function TcxGridChartView.GetLineDiagramClass: TcxGridChartLineDiagramClass;
begin
  Result := TcxGridChartLineDiagram;
end;

function TcxGridChartView.GetPieDiagramClass: TcxGridChartPieDiagramClass;
begin
  Result := TcxGridChartPieDiagram;
end;

function TcxGridChartView.GetStackedAreaDiagramClass: TcxGridChartStackedAreaDiagramClass;
begin
  Result := TcxGridChartStackedAreaDiagram;
end;

function TcxGridChartView.GetStackedColumnDiagramClass: TcxGridChartStackedColumnDiagramClass;
begin
  Result := TcxGridChartStackedColumnDiagram;
end;

function TcxGridChartView.GetStackedBarDiagramClass: TcxGridChartStackedBarDiagramClass;
begin
  Result := TcxGridChartStackedBarDiagram;
end;

function TcxGridChartView.GetCategoriesNamePath: string;
begin
  Result := Name;
  if Result <> '' then
    Result := Result + '.';
  Result := Result + GetSubobjectName(Self, Categories);
end;

procedure TcxGridChartView.AddDiagram(ADiagram: TcxGridChartDiagram);
begin
  FDiagrams.Add(ADiagram);
  if ADiagram.IsAvailable then
    FAvailableDiagrams.Add(ADiagram);
  ADiagram.SetGridView(Self);
  if ActiveDiagram = nil then
    ActiveDiagram := GetFirstAvailableDiagram;
end;

procedure TcxGridChartView.RemoveDiagram(ADiagram: TcxGridChartDiagram);
begin
  ADiagram.SetGridView(nil);
  FAvailableDiagrams.Remove(ADiagram);
  FDiagrams.Remove(ADiagram);
  DiagramRemoved(ADiagram);
  if ActiveDiagram = ADiagram then
    ActiveDiagram := nil;
end;

procedure TcxGridChartView.ClearDiagrams;
var
  I: Integer;
begin
  for I := DiagramCount - 1 downto 0 do
    Diagrams[I].Free;
end;

function TcxGridChartView.CreateDiagram(ADiagramClass: TcxGridChartDiagramClass): TcxGridChartDiagram;
begin
  Result := ADiagramClass.Create(Self);
end;

procedure TcxGridChartView.CreateDiagrams;
begin
  FDiagramColumn := TcxGridChartColumnDiagram(CreateDiagram(GetColumnDiagramClass));  // should first to be default
  FDiagramBar := TcxGridChartBarDiagram(CreateDiagram(GetBarDiagramClass));
  FDiagramStackedArea := TcxGridChartStackedAreaDiagram(CreateDiagram(GetStackedAreaDiagramClass));
  FDiagramStackedBar := TcxGridChartStackedBarDiagram(CreateDiagram(GetStackedBarDiagramClass));
  FDiagramStackedColumn := TcxGridChartStackedColumnDiagram(CreateDiagram(GetStackedColumnDiagramClass));
  FDiagramLine := TcxGridChartLineDiagram(CreateDiagram(GetLineDiagramClass));
  FDiagramArea := TcxGridChartAreaDiagram(CreateDiagram(GetAreaDiagramClass));
  FDiagramPie := TcxGridChartPieDiagram(CreateDiagram(GetPieDiagramClass));
end;

procedure TcxGridChartView.DiagramRemoved(ADiagram: TcxGridChartDiagram);
begin
  if ADiagram = FDiagramArea then
    FDiagramArea := nil;
  if ADiagram = FDiagramBar then
    FDiagramBar := nil;
  if ADiagram = FDiagramColumn then
    FDiagramColumn := nil;
  if ADiagram = FDiagramLine then
    FDiagramLine := nil;
  if ADiagram = FDiagramPie then
    FDiagramPie := nil;
  if ADiagram = FDiagramStackedBar then
    FDiagramStackedBar := nil;
  if ADiagram = FDiagramStackedColumn then
    FDiagramStackedColumn := nil;
  if ADiagram = FDiagramStackedArea then
    FDiagramStackedArea := nil;
end;

function TcxGridChartView.GetDiagramNamePath(ADiagram: TcxGridChartDiagram): string;
begin
  Result := Name;
  if Result <> '' then
    Result := Result + '.';
  Result := Result + GetSubobjectName(Self, ADiagram);
end;

function TcxGridChartView.GetFirstAvailableDiagram: TcxGridChartDiagram;
begin
  if AvailableDiagramCount = 0 then
    Result := nil
  else
    Result := AvailableDiagrams[0];
end;

procedure TcxGridChartView.RefreshAvailableDiagramList;
var
  I: Integer;
begin
  FAvailableDiagrams.Clear;
  for I := 0 to DiagramCount - 1 do
    if Diagrams[I].IsAvailable then
      FAvailableDiagrams.Add(Diagrams[I]);
end;

function TcxGridChartView.GetNextID: Integer;
begin
  if FIDs = nil then
    FIDs := TList.Create;
  Result := FIDs.Add(Pointer(1));
end;

procedure TcxGridChartView.ReleaseID(AID: Integer);
var
  I: Integer;
begin
  FIDs[AID] := nil;
  if AID = FIDs.Count - 1 then
  begin
    I := FIDs.Count - 1;
    while (I >= 0) and (FIDs[I] = nil) do
      Dec(I);
    FIDs.Count := I + 1;
    if FIDs.Count = 0 then
      FreeAndNil(FIDs);
  end;
end;

procedure TcxGridChartView.DataSortingChanged(AItem: TObject);
begin
  if AItem is GetSeriesClass then
  begin
    if TcxGridChartSeries(AItem).SortOrder <> soNone then
      TcxGridChartSeries(AItem).DataBinding.SortIndex := 0;
    UpdateDataSortingBySummary;
    RefreshCustomizationForm;
  end;
end;

procedure TcxGridChartView.DataSortingChanging(AItem: TObject);

  procedure ClearSeriesSorting;
  var
    I: Integer;
    ASortedItem: TObject;
  begin
    for I := FDataController.GetSortingItemCount - 1 downto 0 do
    begin
      ASortedItem := FDataController.GetItem(FDataController.GetSortingItemIndex(I));
      if ASortedItem is GetSeriesClass then
        TcxGridChartSeries(ASortedItem).SortOrder := soNone;
    end;
  end;

begin
  if (AItem is GetSeriesClass) and (TcxGridChartSeries(AItem).SortOrder = soNone) then
    ClearSeriesSorting;
end;

procedure TcxGridChartView.UpdateDataController(AItem: TObject;
  ADataBinding: TcxGridChartItemDataBinding; AAdd: Boolean);
begin
  BeginUpdate;
  try
    if AAdd then
    begin
      ADataBinding.ID := GetNextID;
      ADataBinding.DataField := FDataController.AddItem(AItem);
    end
    else
    begin
      ReleaseID(ADataBinding.ID);
      ADataBinding.DataField := nil;
      FDataController.RemoveItem(AItem);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridChartView.UpdateSummaryItemValues;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].GetDataBinding.UpdateSummaryItemValue;
  UpdateDataSortingBySummary;
end;

function TcxGridChartView.GetItemList(AItemClass: TcxGridChartItemClass): TList;
begin
  if AItemClass = GetSeriesClass then
    Result := FSeries
  else
    if AItemClass = GetDataGroupClass then
      Result := FDataGroups
    else
      Result := nil;
end;

function TcxGridChartView.GetItemList(AItemKind: TcxGridChartItemKind): TList;
begin
  if AItemKind = cikSeries then
    Result := FSeries
  else
    if AItemKind = cikDataGroup then
      Result := FDataGroups
    else
      Result := nil;
end;

function TcxGridChartView.GetItemList(AItem: TcxGridChartItem): TList;
begin
  Result := GetItemList(TcxGridChartItemClass(AItem.ClassType));
end;

function TcxGridChartView.GetVisibleItemList(AItem: TcxGridChartItem): TList;
begin
  if AItem is GetSeriesClass then
    Result := FVisibleSeries
  else
    if AItem is GetDataGroupClass then
      Result := FVisibleDataGroups
    else
      Result := nil;
end;

procedure TcxGridChartView.AddItem(AItem: TcxGridChartItem);
begin
  if csTransient in ComponentStyle then
    AItem.FComponentStyle := AItem.FComponentStyle + [csTransient];
  BeginUpdate;
  try
    GetItemList(AItem).Add(AItem);
    AItem.SetGridView(Self);
    UpdateDataController(AItem, AItem.DataBinding, True);
    ItemPosChanged(AItem);
    if AItem.Visible then
      ItemVisibilityChanged(AItem);
    Changed(vcLayout);
  finally
    EndUpdate;
  end;
  RefreshCustomizationForm;
end;

procedure TcxGridChartView.RemoveItem(AItem: TcxGridChartItem);
begin
  BeginUpdate;
  try
    AItem.SetGridView(nil);
    GetItemList(AItem).Remove(AItem);
    if AItem.Visible then
      ItemVisibilityChanged(AItem);
    ItemPosChanged(AItem);
    UpdateDataController(AItem, AItem.DataBinding, False);
    Changed(vcLayout);
  finally
    EndUpdate;
  end;
  RefreshCustomizationForm;
end;

function TcxGridChartView.GetItemIndex(AItem: TcxGridChartItem): Integer;
begin
  Result := GetItemList(AItem).IndexOf(AItem);
end;

procedure TcxGridChartView.SetItemIndex(AItem: TcxGridChartItem; AIndex: Integer);
begin
  if GetItemIndex(AItem) <> AIndex then
  begin
    GetItemList(AItem).Move(GetItemIndex(AItem), AIndex);
    ItemIndexChanged(AItem);
    Changed(vcLayout);
    RefreshCustomizationForm;
  end;
end;

procedure TcxGridChartView.DataGroupVisibilityChanged(ADataGroup: TcxGridChartDataGroup);
begin
  UpdateDataLevels;
end;

procedure TcxGridChartView.ItemDisplayTextChanged(AItem: TcxGridChartItem);
begin
  RefreshCustomizationForm;
end;

procedure TcxGridChartView.ItemIndexChanged(AItem: TcxGridChartItem);
begin
  ItemPosChanged(AItem);
  if AItem.Visible then
    ItemVisibilityChanged(AItem);
end;

procedure TcxGridChartView.ItemPosChanged(AItem: TcxGridChartItem);
begin
  UpdateItemsOrder(TcxGridChartItemClass(AItem.ClassType));
  if AItem is GetSeriesClass then
  begin
    UpdateDataSortingBySummary;
    ViewData.SeriesPosChanged(TcxGridChartSeries(AItem));
  end;
end;

function CompareSeriesByGroupIndex(ASeries1, ASeries2: TcxGridChartSeries): Integer;
begin
  Result := ASeries1.GroupIndex - ASeries2.GroupIndex;
  if Result = 0 then
    Result := ASeries1.VisibleIndex - ASeries2.VisibleIndex
end;

procedure TcxGridChartView.ItemVisibilityChanged(AItem: TcxGridChartItem);
begin
  RefreshVisibleItemsList(GetItemList(AItem), GetVisibleItemList(AItem));
  if AItem is GetDataGroupClass then
    DataGroupVisibilityChanged(TcxGridChartDataGroup(AItem));
  if AItem is GetSeriesClass then
  begin
    UpdateSeriesVisibleGroups;
    SeriesVisibilityChanged(TcxGridChartSeries(AItem));
  end;
  RefreshCustomizationForm;
end;

procedure TcxGridChartView.ItemVisibilityForCustomizationChanged(AItem: TcxGridChartItem);
begin
  RefreshCustomizationForm;
end;

procedure TcxGridChartView.SeriesVisibilityChanged(ASeries: TcxGridChartSeries);
begin
end;

procedure TcxGridChartView.RefreshVisibleItemsList(AItems, AVisibleItems: TList);
var
  I: Integer;
  AItem: TcxGridChartItem;
begin
  AVisibleItems.Clear;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := TcxGridChartItem(AItems[I]);
    if AItem.Visible then
      AItem.FVisibleIndex := AVisibleItems.Add(AItem)
    else
      AItem.FVisibleIndex := -1;
  end;
end;

function CompareItemsByID(Item1, Item2: Pointer): Integer;
begin
  Result := TcxGridChartItem(Item1).ID - TcxGridChartItem(Item2).ID;
end;

procedure TcxGridChartView.UpdateItemsOrder(AItemClass: TcxGridChartItemClass);
var
  AItems: TList;
  I: Integer;
begin
  AItems := TList.Create;
  try
    dxCopyList(GetItemList(AItemClass), AItems);
    AItems.Sort(CompareItemsByID);
    for I := 0 to AItems.Count - 1 do
      TcxGridChartItem(AItems[I]).FOrder := I;
  finally
    AItems.Free;
  end;
end;

procedure TcxGridChartView.UpdateSeriesVisibleGroups;
var
  I, AIndex: Integer;
begin
  if IsDestroying then Exit;
  FVisibleSeriesGroups.Clear;
  FVisibleSeries.Sort(@CompareSeriesByGroupIndex);
  for I := 0 to SeriesCount - 1 do
    with Series[I] do
    begin
      if Visible then
      begin
        if (ActiveDiagram is TcxGridChartStackedColumnDiagram) and
          (TcxGridChartStackedColumnDiagram(ActiveDiagram).StackedStyle in [sdsSideBySide, sdsSideBySide100Percent]) then
          AIndex := GroupIndex
        else
          AIndex := 0;
        FVisibleGroupIndex := FVisibleSeriesGroups.IndexOf(Pointer(AIndex));
        if FVisibleGroupIndex < 0 then
          FVisibleGroupIndex := FVisibleSeriesGroups.Add(Pointer(AIndex));
      end
      else
        FVisibleGroupIndex := -1
    end;
end;

procedure TcxGridChartView.ClearItems(AItemClass: TcxGridChartItemClass);
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  BeginUpdate;
  try
    for I := AItems.Count - 1 downto 0 do
      TcxGridChartItem(AItems[I]).Free;
  finally
    EndUpdate;
  end;
end;

function TcxGridChartView.CreateItem(AItemClass: TcxGridChartItemClass): TcxGridChartItem;
begin
  Result := AItemClass.Create(Owner);
  AddItem(Result);
end;

function TcxGridChartView.FindItemByID(AItemClass: TcxGridChartItemClass;
  AID: Integer): TcxGridChartItem;
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  for I := 0 to AItems.Count - 1 do
  begin
    Result := TcxGridChartItem(AItems[I]);
    if Result.ID = AID then Exit;
  end;
  Result := nil;
end;

function TcxGridChartView.FindItemByName(AItemClass: TcxGridChartItemClass;
  const AName: string): TcxGridChartItem;
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  for I := 0 to AItems.Count - 1 do
  begin
    Result := TcxGridChartItem(AItems[I]);
    if Result.Name = AName then Exit;
  end;
  Result := nil;
end;

function TcxGridChartView.FindItemByTag(AItemClass: TcxGridChartItemClass;
  ATag: TcxTag): TcxGridChartItem;
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  for I := 0 to AItems.Count - 1 do
  begin
    Result := TcxGridChartItem(AItems[I]);
    if Result.Tag = ATag then Exit;
  end;
  Result := nil;
end;

function TcxGridChartView.GetAvailableDataLevel(ALevel: Integer): Integer;
begin
  for Result := 0 to ALevel - 1 do
    if not DataLevelObjects[Result].HasActiveValue then Exit;
  Result := ALevel;
end;

function TcxGridChartView.GetDataLevelCount: Integer;
begin
  Result := VisibleDataGroupCount + 1;
end;

function TcxGridChartView.GetDataLevelObject(Index: Integer): TcxGridChartDataGroup;
begin
  if Index < VisibleDataGroupCount then
    Result := VisibleDataGroups[Index]
  else
    Result := nil;
end;

function TcxGridChartView.GetDataObjectLevel(AObject: TcxGridChartDataGroup): Integer;
begin
  Result := AObject.VisibleIndex;
end;

procedure TcxGridChartView.SetDataObjectLevel(AObject: TcxGridChartDataGroup; ALevel: Integer);
begin
  if ALevel = -1 then
    AObject.Visible := False
  else
    if (0 <= ALevel) and (ALevel < VisibleDataGroupCount + Ord(not AObject.Visible)) then
    begin
      BeginUpdate;
      try
        AObject.Visible := True;
        AObject.Index := DataLevelObjects[ALevel].Index;
      finally
        EndUpdate;
      end;
    end;
end;

function TcxGridChartView.GetIsDataGrouped: Boolean;
begin
  Result := VisibleDataGroupCount <> 0;
end;

procedure TcxGridChartView.UpdateDataGroupActiveValues;
var
  I: Integer;
begin
  if FDataController.LockCount <> 0 then
  begin
    FDataGroupActiveValuesUpdateNeeded := True;
    Exit;
  end;
  FDataGroupActiveValuesUpdateNeeded := False;

  for I := 0 to DataGroupCount - 1 do
    DataGroups[I].CheckActiveValue;
  ActiveDataLevel := ActiveDataLevel;
end;

procedure TcxGridChartView.UpdateDataLevels;
var
  I: Integer;
begin
  BeginUpdate;
  try
//    ActiveDataLevel := ActiveDataLevel;
    FDataController.Groups.ClearGrouping;
    for I := 0 to VisibleDataGroupCount - 1 do
      VisibleDataGroups[I].GroupIndex := I;
    UpdateDataGroupActiveValues;
    Controller.DataLevelsChanged;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridChartView.UpdateDataSortingBySummary;
var
  ASeries: TcxGridChartSeries;
begin
  ASeries := SortedSeries;
  if ASeries = nil then
    FDataController.SortingBySummaryDataItemIndex := -1
  else
    FDataController.SortingBySummaryDataItemIndex := ASeries.DataBinding.DataIndex;
end;

procedure TcxGridChartView.CalculateImageWidth(var AWidth: Integer);
const
  DefaultWidth = 700;
begin
  if AWidth = 0 then
  begin
    if Control <> nil then
      AWidth := Control.Width;
    if AWidth = 0 then
      AWidth := DefaultWidth;
  end;
end;

procedure TcxGridChartView.CalculateImageHeight(var AHeight: Integer);
const
  DefaultHeight = 500;
begin
  if AHeight = 0 then
  begin
    AHeight := TcxGridLevel(Level).Parent.MaxDetailHeight;
    if AHeight = 0 then
    begin
      if Control <> nil then
        AHeight := Control.Height;
      if AHeight = 0 then
        AHeight := DefaultHeight;
    end;
  end;
end;

procedure TcxGridChartView.DoActiveDataLevelChanged;
begin
  if Assigned(FOnActiveDataLevelChanged) then FOnActiveDataLevelChanged(Self);
end;

procedure TcxGridChartView.DoActiveDiagramChanged(ADiagram: TcxGridChartDiagram);
begin
  if Assigned(FOnActiveDiagramChanged) then FOnActiveDiagramChanged(Self, ADiagram);
end;

procedure TcxGridChartView.DoCustomDrawLegend(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartLegendViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawLegend then
    FOnCustomDrawLegend(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridChartView.DoCustomDrawLegendItem(ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartLegendItemViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawLegendItem then
    FOnCustomDrawLegendItem(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridChartView.DoDataGroupPosChanged(ADataGroup: TcxGridChartDataGroup);
begin
  if Assigned(FOnDataGroupPosChanged) then FOnDataGroupPosChanged(Self, ADataGroup);
end;

procedure TcxGridChartView.DoFirstVisibleCategoryIndexChanged;
begin
  if Assigned(FOnFirstVisibleCategoryIndexChanged) then
    FOnFirstVisibleCategoryIndexChanged(Self);
end;

procedure TcxGridChartView.DoGetValueHint(ASeries: TcxGridChartSeries; AValueIndex: Integer;
  var AHint: string);
begin
  if Assigned(FOnGetValueHint) then
    FOnGetValueHint(Self, ASeries, AValueIndex, AHint);
end;

procedure TcxGridChartView.DoSeriesPosChanged(ASeries: TcxGridChartSeries);
begin
  if Assigned(FOnSeriesPosChanged) then FOnSeriesPosChanged(Self, ASeries);
end;

function TcxGridChartView.DoValueClick(ASeries: TcxGridChartSeries; AValueIndex: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnValueClick) then
    FOnValueClick(Self, ASeries, AValueIndex, Result);
end;

function TcxGridChartView.HasCustomDrawLegend: Boolean;
begin
  Result := Assigned(FOnCustomDrawLegend);
end;

function TcxGridChartView.HasCustomDrawLegendItem: Boolean;
begin
  Result := Assigned(FOnCustomDrawLegendItem);
end;

function TcxGridChartView.CreateImage(AGraphicClass: TGraphicClass;
  AWidth: Integer = 0; AHeight: Integer = 0): TGraphic;

  procedure DrawImage(AImageCanvas: TCanvas);
  var
    AViewInfo: TcxCustomGridViewInfo;
    ACanvas: TcxCanvas;
  begin
    AViewInfo := CreateViewInfo;
    try
      AViewInfo.IsInternalUse := True;
      (AViewInfo as TcxGridChartViewInfo).IsImage := True;
      AViewInfo.MainCalculate(Rect(0, 0, AWidth, AHeight));
      ACanvas := TcxCanvas.Create(AImageCanvas);
      try
        Painter.Paint(ACanvas, AViewInfo);
      finally
        ACanvas.Free;
      end;
    finally
      AViewInfo.Free;
    end;
  end;

var
  AMetaFileCanvas: TMetaFileCanvas;
begin
  if IsPattern or
    not (AGraphicClass.InheritsFrom(TMetafile) or AGraphicClass.InheritsFrom(TBitmap)) then
  begin
    Result := nil;
    Exit;
  end;
  CalculateImageWidth(AWidth);
  CalculateImageHeight(AHeight);
  Result := AGraphicClass.Create;
  Result.Width := AWidth;
  Result.Height := AHeight;
  if Result is TMetaFile then
  begin
    Result.Transparent := True;
    AMetaFileCanvas := TMetaFileCanvas.Create(TMetaFile(Result), 0);
    try
      DrawImage(AMetaFileCanvas);
    finally
      AMetaFileCanvas.Free;
    end;
  end
  else
    DrawImage(TBitmap(Result).Canvas);
end;

function TcxGridChartView.FindDiagramByDisplayText(const ADisplayText: string): TcxGridChartDiagram;
var
  I: Integer;
begin
  for I := 0 to DiagramCount - 1 do
  begin
    Result := Diagrams[I];
    if Result.DisplayText = ADisplayText then Exit;
  end;
  Result := nil;
end;

function TcxGridChartView.FindDiagramByID(const AID: string): TcxGridChartDiagram;
var
  I: Integer;
begin
  for I := 0 to DiagramCount - 1 do
  begin
    Result := Diagrams[I];
    if Result.ID = AID then Exit;
  end;
  Result := nil;
end;

procedure TcxGridChartView.ClearSeries;
begin
  ClearItems(GetSeriesClass);
end;

function TcxGridChartView.CreateSeries: TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(CreateItem(GetSeriesClass));
end;

function TcxGridChartView.FindSeriesByID(AID: Integer): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(FindItemByID(GetSeriesClass, AID));
end;

function TcxGridChartView.FindSeriesByName(const AName: string): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(FindItemByName(GetSeriesClass, AName));
end;

function TcxGridChartView.FindSeriesByTag(ATag: TcxTag): TcxGridChartSeries;
begin
  Result := TcxGridChartSeries(FindItemByTag(GetSeriesClass, ATag));
end;

function TcxGridChartView.GetSeriesClass: TcxGridChartSeriesClass;
begin
  Result := TcxGridChartSeries;
end;

procedure TcxGridChartView.ClearDataGroups;
begin
  ClearItems(GetDataGroupClass);
end;

function TcxGridChartView.CreateDataGroup: TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(CreateItem(GetDataGroupClass));
end;

function TcxGridChartView.FindDataGroupByID(AID: Integer): TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(FindItemByID(GetDataGroupClass, AID));
end;

function TcxGridChartView.FindDataGroupByName(const AName: string): TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(FindItemByName(GetDataGroupClass, AName));
end;

function TcxGridChartView.FindDataGroupByTag(ATag: TcxTag): TcxGridChartDataGroup;
begin
  Result := TcxGridChartDataGroup(FindItemByTag(GetDataGroupClass, ATag));
end;

function TcxGridChartView.GetDataGroupClass: TcxGridChartDataGroupClass;
begin
  Result := TcxGridChartDataGroup;
end;

function TcxGridChartView.CanActivateDataLevel(ALevel: Integer): Boolean;
begin
  Result := GetAvailableDataLevel(ALevel) = ALevel;
end;

initialization
  cxGridRegisteredViews.Register(TcxGridChartView, 'Chart');
  Classes.RegisterClasses([TcxGridChartDataGroup, TcxGridChartSeries]);

  cxGridChartDiagramImages := TcxImageList.CreateSize(32, 32);
  cxGridChartDiagramImages.SourceDPI := dxDefaultDPI;
  cxGridChartDiagramImages_Add('CXGRIDCHARTAREADIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTBARDIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTCOLUMNDIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTLINEDIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTPIEDIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTSTACKEDAREADIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTSTACKEDBARDIAGRAMBITMAP');
  cxGridChartDiagramImages_Add('CXGRIDCHARTSTACKEDCOLUMNDIAGRAMBITMAP');

  Screen.Cursors[crcxGridMagnifier] := LoadCursor(HInstance, 'CXGRIDCHARTMAGNIFIERCURSOR');
  Screen.Cursors[crcxGridChartDrag] := LoadCursor(HInstance, 'CXGRIDCHARTDRAGCURSOR');

finalization
  FreeAndNil(cxGridChartDiagramImages);

  cxGridRegisteredViews.Unregister(TcxGridChartView);

end.


