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

unit cxGridCustomTableView;

{$I cxVer.inc}

interface

{$UNDEF DXLOGGING}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs, Generics.Defaults, Generics.Collections,
  cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxGeometry,
  cxStorage, cxPC, cxFilterControl, cxNavigator, cxListBox, cxEdit, cxButtons,
  cxDataStorage, cxCustomData, cxData, cxFilter, cxDataUtils, cxContainer,
  cxCheckBox, cxCheckListBox, cxStyles, cxGridCommon, cxGridCustomView, dxCore,
  dxCoreClasses, dxGdiPlusClasses, dxAnimation, cxMemo, cxDropDownEdit, cxMRUEdit,
  cxTextEdit, dxIncrementalFiltering, dxUIElementPopupWindow, cxFindPanel,
  dxSpreadSheetTypes, dxSpreadSheetStyles, dxSpreadSheetConditionalFormatting, cxDataControllerConditionalFormatting,
  dxFilterPopupWindow, dxFilterValueContainer, cxDateUtils, dxDateRanges, dxFilterBox;

const
  cxGridFilterDefaultItemMRUItemsListCount = 5;
  cxGridFilterDefaultItemPopupMaxDropDownItemCount = 15;
  cxGridFilterDefaultMRUItemsListCount = 10;
  cxGridItemDefaultMinWidth = 20;
  cxGridNavigatorDefaultOffset = cxInplaceNavigatorDefaultOffset;
  cxRecordIndexNone = -1;

  cxGridFindPanelDefaultMRUItemsListDropDownCount = cxFindPanelDefaultMRUItemsListDropDownCount;
  cxGridFindPanelDefaultMRUItemsListCount = cxFindPanelDefaultMRUItemsListCount;

  cxGridPinHeight = 10;
  cxGridPinWidth = 10;
  cxGridPinLeftMargin = 4;
  cxGridPinRightMargin = 4;

  htCustomGridTableBase = 100;
  htFilter = htCustomGridTableBase + 1;
  htFilterActivateButton = htCustomGridTableBase + 2;
  htFilterCloseButton = htCustomGridTableBase + 3;
  htFilterDropDownButton = htCustomGridTableBase + 4;
  htFilterCustomizeButton = htCustomGridTableBase + 5;
  htRecord = htCustomGridTableBase + 6;
  htCell = htCustomGridTableBase + 7;
  htExpandButton = htCustomGridTableBase + 8;
  htFindPanel = htCustomGridTableBase + 9;
  htFindPanelCloseButton = htCustomGridTableBase + 10;
  htFindPanelFindButton = htCustomGridTableBase + 11;
  htFindPanelClearButton = htCustomGridTableBase + 12;
  htCheckBox = htCustomGridTableBase + 13;
  htPin = htCustomGridTableBase + 14;

  // record kind
  rkNone = -1;
  rkNormal = 0;
  rkNewItem = 1;

  isCustomItemFirst = 0;
  isContent = isCustomItemFirst;
  isCustomItemLast = isContent;

  bbCustomTableFirst = bbCustomLast + 1;
  bbContent = bbCustomTableFirst;
  bbFilterBox = bbCustomTableFirst + 1;
  bbCustomTableLast = bbFilterBox;

  vsCustomTableFirst = vsCustomLast + 1;
  vsContent = vsCustomTableFirst;
  vsContentEven = vsCustomTableFirst + 1;
  vsContentOdd = vsCustomTableFirst + 2;
  vsFilterBox = vsCustomTableFirst + 3;
  vsInactive = vsCustomTableFirst + 4;
  vsIncSearch = vsCustomTableFirst + 5;
  vsNavigator = vsCustomTableFirst + 6;
  vsSelection = vsCustomTableFirst + 7;
  vsNavigatorInfoPanel = vsCustomTableFirst + 8;
  vsSearchResultHighlight = vsCustomTableFirst + 9;
  vsFindPanel = vsCustomTableFirst + 10;
  vsCustomTableLast = vsFindPanel;

type
  TcxGridDateTimeFilter = TdxDateTimeFilter;
  TcxGridDateTimeFilters = TdxDateTimeFilters;
  TcxGridDateTimeGrouping = TdxDateTimeGrouping;
  TcxGridFilterVisible = TcxFilterBoxVisible;
  TcxGridFilterPosition = TcxFilterBoxPosition;
  TcxGridShowItemFilterButtons = TcxShowFilterButtons;
  TcxGridItemFilterButtonShowMode = TcxFilterButtonShowMode;
  TcxGridFilterButtonAlignment = TcxFilterBoxButtonAlignment;
  TcxGridItemFilterPopupApplyChangesMode = TdxFilterApplyChangesMode;

const
  dtfRelativeDays = TcxGridDateTimeFilter.dtfRelativeDays;
  dtfRelativeDayPeriods = TcxGridDateTimeFilter.dtfRelativeDayPeriods;
  dtfRelativeWeeks = TcxGridDateTimeFilter.dtfRelativeWeeks;
  dtfRelativeMonths = TcxGridDateTimeFilter.dtfRelativeMonths;
  dtfRelativeYears = TcxGridDateTimeFilter.dtfRelativeYears;
  dtfPastFuture = TcxGridDateTimeFilter.dtfPastFuture;
  dtfMonths = TcxGridDateTimeFilter.dtfMonths;
  dtfYears = TcxGridDateTimeFilter.dtfYears;
  {$NODEFINE dtfRelativeDays}
  {$NODEFINE dtfRelativeDayPeriods}
  {$NODEFINE dtfRelativeWeeks}
  {$NODEFINE dtfRelativeMonths}
  {$NODEFINE dtfRelativeYears}
  {$NODEFINE dtfPastFuture}
  {$NODEFINE dtfMonths}
  {$NODEFINE dtfYears}

  dtgDefault = TcxGridDateTimeGrouping.dtgDefault;
  dtgByDateAndTime = TcxGridDateTimeGrouping.dtgByDateAndTime;
  dtgRelativeToToday = TcxGridDateTimeGrouping.dtgRelativeToToday;
  dtgByHour = TcxGridDateTimeGrouping.dtgByHour;
  dtgByDate = TcxGridDateTimeGrouping.dtgByDate;
  dtgByMonth = TcxGridDateTimeGrouping.dtgByMonth;
  dtgByYear = TcxGridDateTimeGrouping.dtgByYear;
  {$NODEFINE dtgDefault}
  {$NODEFINE dtgByDateAndTime}
  {$NODEFINE dtgRelativeToToday}
  {$NODEFINE dtgByHour}
  {$NODEFINE dtgByDate}
  {$NODEFINE dtgByMonth}
  {$NODEFINE dtgByYear}

  fvNever = TcxGridFilterVisible.fvNever;
  fvNonEmpty = TcxGridFilterVisible.fvNonEmpty;
  fvAlways = TcxGridFilterVisible.fvAlways;
  {$NODEFINE fvNever}
  {$NODEFINE fvNonEmpty}
  {$NODEFINE fvAlways}

  fpTop = TcxGridFilterPosition.fpTop;
  fpBottom = TcxGridFilterPosition.fpBottom;
  {$NODEFINE fpTop}
  {$NODEFINE fpBottom}

  sfbAlways = TcxGridShowItemFilterButtons.sfbAlways;
  sfbWhenSelected = TcxGridShowItemFilterButtons.sfbWhenSelected;
  sfbDefault = TcxGridShowItemFilterButtons.sfbDefault;
  {$NODEFINE sfbAlways}
  {$NODEFINE sfbWhenSelected}
  {$NODEFINE sfbDefault}

  fbmDefault = TcxGridItemFilterButtonShowMode.fbmDefault;
  fbmButton = TcxGridItemFilterButtonShowMode.fbmButton;
  fbmSmartTag = TcxGridItemFilterButtonShowMode.fbmSmartTag;
  {$NODEFINE fbmDefault}
  {$NODEFINE fbmButton}
  {$NODEFINE fbmSmartTag}

  fbaLeft = TcxGridFilterButtonAlignment.fbaLeft;
  fbaRight = TcxGridFilterButtonAlignment.fbaRight;
  {$NODEFINE fbaLeft}
  {$NODEFINE fbaRight}

  fpacImmediately = TcxGridItemFilterPopupApplyChangesMode.fpacImmediately;
  fpacOnButtonClick = TcxGridItemFilterPopupApplyChangesMode.fpacOnButtonClick;
  {$NODEFINE fpacImmediately}
  {$NODEFINE fpacOnButtonClick}

type
  TcxGridItemDataBindingClass = class of TcxGridItemDataBinding;
  TcxCustomGridRecord = class;
  TcxCustomGridTableViewData = class;
  TcxCustomGridTableItemsListBox = class;
  TcxCustomGridTableCustomizationForm = class;
  TcxCustomGridCustomizationPopup = class;
  TcxCustomGridTableController = class;
  TcxGridFilterActivateButtonViewInfo = class;
  TcxGridFilterButtonsViewInfo = class;
  TcxGridFilterViewInfo = class;
  TcxGridTableDataCellViewInfo = class;
  TcxCustomGridRecordViewInfoClass = class of TcxCustomGridRecordViewInfo;
  TcxCustomGridRecordViewInfo = class;
  TcxCustomGridRecordsViewInfo = class;
  TcxCustomGridTableViewInfo = class;
  TcxCustomGridTableViewInfoCacheItem = class;
  TcxCustomGridTableItem = class;
  TcxCustomGridTableDateTimeHandling = class;
  TcxCustomGridTableFiltering = class;
  TcxCustomGridTableView = class;
  TcxGridViewNavigator = class;
  TcxGridFindPanelViewInfo = class;
  TcxGridCustomButtonViewInfo = class;
  TcxGridCheckBoxViewInfo = class;
  TcxGridPinViewInfo = class;
  TcxGridFindPanelButtonsViewInfo = class;
  TcxGridFindPanelEditViewInfo = class;
  TcxGridFindPanel = class;
  TcxGridPartContainerButtonsViewInfo = class;
  TcxGridPartContainerViewInfo = class;

  { changes }

  TcxGridDataChange = class(TcxCustomGridViewChange)
  public
    procedure Execute; override;
    function IsLockable: Boolean; override;
  end;

  TcxGridRecordChange = class(TcxCustomGridViewChange)
  private
    FItem: TcxCustomGridTableItem;
    FRecord: TcxCustomGridRecord;
    FRecordIndex: Integer;
    function GetGridView: TcxCustomGridTableView;
    function GetRecordViewInfo: TcxCustomGridRecordViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView; ARecord: TcxCustomGridRecord;
      ARecordIndex: Integer; AItem: TcxCustomGridTableItem = nil); reintroduce; virtual;
    procedure Execute; override;
    function IsCompatibleWith(AChange: TcxCustomGridChange): Boolean; override;
    function IsEqual(AChange: TcxCustomGridChange): Boolean; override;
    function IsItemVisible: Boolean;
    property GridRecord: TcxCustomGridRecord read FRecord;
    property GridView: TcxCustomGridTableView read GetGridView;
    property Item: TcxCustomGridTableItem read FItem;
    property RecordIndex: Integer read FRecordIndex;
    property RecordViewInfo: TcxCustomGridRecordViewInfo read GetRecordViewInfo;
  end;

  TcxGridFocusedRecordChange = class(TcxCustomGridViewChange)
  private
    FFocusedDataRecordIndex: Integer;
    FFocusedRecordIndex: Integer;
    FNewItemRecordFocusingChanged: Boolean;
    FPrevFocusedDataRecordIndex: Integer;
    FPrevFocusedRecordIndex: Integer;
  public
    constructor Create(AGridView: TcxCustomGridView;
      APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); reintroduce; virtual;
    function CanExecuteWhenLocked: Boolean; override;
    procedure Execute; override;
    property FocusedRecordIndex: Integer read FFocusedRecordIndex;
    property NewItemRecordFocusingChanged: Boolean read FNewItemRecordFocusingChanged;
    property PrevFocusedRecordIndex: Integer read FPrevFocusedRecordIndex;
  end;

  { hit tests }

  TcxGridFilterHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFilterCloseButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFilterActivateButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFilterDropDownButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFilterCustomizeButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFindPanelHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFindPanelCloseButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFindPanelFindButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFindPanelClearButtonHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridCheckBoxHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridPinHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridRecordHitTest = class(TcxCustomGridViewHitTest)
  private
    FGridRecordIndex: Integer;
    FGridRecordKind: Integer;
    FViewData: TcxCustomGridTableViewData;
    function GetGridRecord: TcxCustomGridRecord;
    procedure SetGridRecord(Value: TcxCustomGridRecord);
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; virtual;

    property GridRecord: TcxCustomGridRecord read GetGridRecord write SetGridRecord;
  end;

  TcxGridRecordCellHitTest = class(TcxGridRecordHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    Item: TcxCustomGridTableItem;

    function Cursor: TCursor; override;
  end;

  TcxGridExpandButtonHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  { data definitions }

  TcxGridDefaultValuesProvider = class(TcxCustomEditDefaultValuesProvider)
  public
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;

  IcxGridDataController = interface
  ['{FEEE7E69-BD54-4B5D-BA0B-B6116B69C0CC}']
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function DoScrollPage(AForward: Boolean): Boolean;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass;
    function GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
    function GetNavigatorIsBof: Boolean;
    function GetNavigatorIsEof: Boolean;
    //function GetFilterPropertyValue(const AName: string; var AValue: Variant): Boolean;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    //function SetFilterPropertyValue(const AName: string; const AValue: Variant): Boolean;
    function SetScrollBarPos(Value: Integer): Boolean;
  end;

  TcxGridDataController = class(TcxDataController,
    IcxCustomGridDataController, IcxGridDataController)
  private
    FLoadedData: TMemoryStream;
    function GetGridViewValue: TcxCustomGridTableView;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
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
    { IcxGridDataController }
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function DoScrollPage(AForward: Boolean): Boolean;
    //function GetFilterPropertyValue(const AName: string; var AValue: Variant): Boolean;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass;
    function GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
    function GetNavigatorIsBof: Boolean;
    function GetNavigatorIsEof: Boolean;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    //function SetFilterPropertyValue(const AName: string; const AValue: Variant): Boolean;
    function SetScrollBarPos(Value: Integer): Boolean;

    function CanSelectRow(ARowIndex: Integer): Boolean; override;
    function CompareByField(ARecordIndex1, ARecordIndex2: Integer;
      AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    procedure FilterChanged; override;
    function GetDefaultActiveRelationIndex: Integer; override;
    function GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    //function GetIncrementalSearchText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
  public
    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    function CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
      ARecordIndex: Integer): TObject; override;
    procedure FocusControl(AItemIndex: Integer; var Done: Boolean); override;
    function GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController; override;
    function GetDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemSortByDisplayText(AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    procedure Loaded; override;
    procedure UpdateData; override;

    property GridView: TcxCustomGridTableView read GetGridViewValue;
  published
    property Filter;
    property Options;
    property Summary;
    property OnAfterCancel;
    property OnAfterDelete;
    property OnAfterInsert;
    property OnAfterPost;
    property OnBeforeCancel;
    property OnBeforeDelete;
    property OnBeforeInsert;
    property OnBeforePost;
    property OnNewRecord;
    property OnCompare;
    property OnDataChanged;
    property OnDetailCollapsing;
    property OnDetailCollapsed;
    property OnDetailExpanding;
    property OnDetailExpanded;
    property OnFilterRecord;
    property OnGroupingChanged;
    property OnRecordChanged;
    property OnSortingChanged;
  end;

  TcxCustomGridDateRangeClass = TdxCustomDateRangeClass;
  TcxCustomGridDateRange = TdxCustomDateRange;

  TcxGridHourRange = class(TdxHourRange);

  TcxGridDayRange = class(TdxDayRange);

  TcxGridMonthRange = class(TdxMonthRange);

  TcxGridYearRange = class(TdxYearRange);

  TcxGridYesterdayRange = class(TdxYesterdayRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridTodayRange = class(TdxTodayRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridTomorrowRange = class(TdxTomorrowRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridLastWeekRange = class(TdxLastWeekRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridThisWeekRange = class(TdxThisWeekRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridNextWeekRange = class(TdxNextWeekRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridLastMonthRange = class(TdxLastMonthRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridThisMonthRange = class(TdxThisMonthRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridNextMonthRange = class(TdxNextMonthRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridLastYearRange = class(TdxLastYearRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridThisYearRange = class(TdxThisYearRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridNextYearRange = class(TdxNextYearRange)
  public
    function GetDisplayText(const ADate: TDateTime): string; override;
  end;

  TcxGridDateRanges = TdxDateRanges;
  TcxGridDateRangesClass = TdxDateRangesClass;

  TcxGridFilteringDateRanges = class(TdxFilteringDateRanges)
  private
    function GetDateTimeHandling: TcxCustomGridTableDateTimeHandling;
  protected
    function GetMonthRange: TdxCustomDateRangeClass; override;
    function GetYearRange: TdxCustomDateRangeClass; override;
  public
    property DateTimeHandling: TcxCustomGridTableDateTimeHandling read GetDateTimeHandling;
  end;
  TcxGridFilteringDateRangesClass = class of TcxGridFilteringDateRanges;

  TcxGridGroupingDateRanges = class(TdxGroupingDateRanges)
  private
    function GetDateTimeHandling: TcxCustomGridTableDateTimeHandling;
  protected
    function GetDayRangeClass: TdxCustomDateRangeClass; override;
    function GetHourRangeClass: TdxCustomDateRangeClass; override;
    function GetLastMonthRangeClass: TdxCustomDateRangeClass; override;
    function GetLastWeekRangeClass: TdxCustomDateRangeClass; override;
    function GetLastYearRangeClass: TdxCustomDateRangeClass; override;
    function GetMonthRangeClass: TdxCustomDateRangeClass; override;
    function GetNextMonthRangeClass: TdxCustomDateRangeClass; override;
    function GetNextWeekRangeClass: TdxCustomDateRangeClass; override;
    function GetNextYearRangeClass: TdxCustomDateRangeClass; override;
    function GetThisMonthRangeClass: TdxCustomDateRangeClass; override;
    function GetThisWeekRangeClass: TdxCustomDateRangeClass; override;
    function GetThisYearRangeClass: TdxCustomDateRangeClass; override;
    function GetTodayRangeClass: TdxCustomDateRangeClass; override;
    function GetTomorrowRangeClass: TdxCustomDateRangeClass; override;
    function GetYearRangeClass: TdxCustomDateRangeClass; override;
    function GetYesterdayRangeClass: TdxCustomDateRangeClass; override;
  public
    property DateTimeHandling: TcxCustomGridTableDateTimeHandling read GetDateTimeHandling;
  end;
  TcxGridGroupingDateRangesClass = class of TcxGridGroupingDateRanges;

  TcxGridFilterValueListClass = class of TcxGridFilterValueList;

  TcxGridFilterValueList = class(TcxDataFilterValueList)
  protected
    procedure AddDateTimeAbsoluteFilters(ADateRange: TcxCustomGridDateRange;
      AIgnoreTime: Boolean); overload; virtual;
    procedure AddDateTimeAbsoluteFilters(AItem: TcxCustomGridTableItem); overload; virtual;
    procedure AddDateTimeRelativeFilters(AItem: TcxCustomGridTableItem); virtual;
    function IsSpecialOperatorKindSupported(AKind: TcxFilterOperatorKind): Boolean; virtual;
    procedure Load(AItem: TcxCustomGridTableItem; AInitSortByDisplayText: Boolean; AUseFilteredValues: Boolean;
      AAddValueItems: Boolean; AUniqueOnly: Boolean); reintroduce; overload;
  public
    procedure ApplyFilter(AItem: TcxCustomGridTableItem; AIndex: Integer;
      AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);
    function GetIndexByCriteriaItem(ACriteriaItem: TcxFilterCriteriaItem): Integer; override;
    procedure Load(AItem: TcxCustomGridTableItem; AInitSortByDisplayText: Boolean = True; AUseFilteredValues: Boolean = False;
      AAddValueItems: Boolean = True); reintroduce; overload; virtual;
  end;

  TcxGridFilterMRUValueItem = TcxFilterMRUValueItem;
  TcxGridFilterMRUValueItemsClass = TcxFilterMRUValueItemsClass;
  TcxGridFilterMRUValueItems = TcxFilterMRUValueItems;

  TcxGridItemDataBinding = class(TPersistent)
  private
    FData: TObject;
    FDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
    FItem: TcxCustomGridTableItem;
    FFilterMRUValueItems: TcxGridFilterMRUValueItems;
    function GetDataController: TcxCustomDataController;
    function GetFilter: TcxDataFilterCriteria;
    function GetFilterCriteriaItem: TcxFilterCriteriaItem;
    function GetFiltered: Boolean;
    function GetGridView: TcxCustomGridTableView;
    function GetValueType: string;
    function GetValueTypeClass: TcxValueTypeClass;
    procedure SetData(Value: TObject);
    procedure SetFiltered(Value: Boolean);
    procedure SetValueType(const Value: string);
    procedure SetValueTypeClass(Value: TcxValueTypeClass);
  protected
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
    function GetDefaultValueTypeClass: TcxValueTypeClass; virtual;
    function GetFilterFieldName: string; virtual;
    function GetFilterMRUValueItemsClass: TcxGridFilterMRUValueItemsClass; virtual;
    procedure GetFilterStrings(AStrings: TStrings; AValueList: TcxGridFilterValueList;
      AValuesOnly: Boolean; AUniqueOnly: Boolean); overload;
    procedure GetFilterValues(AValueList: TcxGridFilterValueList; AValuesOnly: Boolean;
      AInitSortByDisplayText: Boolean; ACanUseFilteredValues: Boolean; AUniqueOnly: Boolean); overload;
    procedure Init; virtual;
    function IsValueTypeStored: Boolean; virtual;
    procedure SetFilterActiveValue(AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; const ADisplayText: string);
    procedure SetFilterActiveValueIndex(AValueList: TcxGridFilterValueList; AIndex: Integer;
      AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);
    procedure SetFilterActiveValues(AValueInfos: TdxFastObjectList; ABoolOperator: TcxFilterBoolOperatorKind);

    property DefaultValuesProvider: TcxCustomEditDefaultValuesProvider read FDefaultValuesProvider;
  public
    constructor Create(AItem: TcxCustomGridTableItem); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function DefaultCaption: string; virtual;
    function DefaultRepositoryItem: TcxEditRepositoryItem; virtual;
    function DefaultWidth(ATakeHeaderIntoAccount: Boolean = True): Integer; virtual;
    function GetAllowedSummaryKinds: TcxSummaryKinds;
    function GetDefaultValuesProvider(AProperties: TcxCustomEditProperties = nil): IcxEditDefaultValuesProvider;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;

    function AddToFilter(AParent: TcxFilterCriteriaItemList; AOperatorKind: TcxFilterOperatorKind;
      const AValue: Variant; const ADisplayText: string = '';
      AReplaceExistent: Boolean = True): TcxFilterCriteriaItem;
    procedure GetFilterDisplayText(const AValue: Variant; var ADisplayText: string);
    procedure GetFilterStrings(AStrings: TStrings; AValueList: TcxGridFilterValueList); overload;
    procedure GetFilterValues(AValueList: TcxGridFilterValueList; AValuesOnly: Boolean = True;
      AInitSortByDisplayText: Boolean = False; ACanUseFilteredValues: Boolean = False); overload;
    procedure GetFilterActiveValueIndexes(AValueList: TcxGridFilterValueList; var AIndexes: TcxGridIndexes);
    procedure SetFilterActiveValueIndexes(AValueList: TcxGridFilterValueList; const AIndexes: TcxGridIndexes;
      AAddToMRUItemsList: Boolean = False);

    property Data: TObject read FData write SetData;
    property DataController: TcxCustomDataController read GetDataController;
    property Filter: TcxDataFilterCriteria read GetFilter;
    property FilterCriteriaItem: TcxFilterCriteriaItem read GetFilterCriteriaItem;
    property Filtered: Boolean read GetFiltered write SetFiltered;
    property FilterFieldName: string read GetFilterFieldName;
    property FilterMRUValueItems: TcxGridFilterMRUValueItems read FFilterMRUValueItems;
    property GridView: TcxCustomGridTableView read GetGridView;
    property Item: TcxCustomGridTableItem read FItem;
    property ValueTypeClass: TcxValueTypeClass read GetValueTypeClass write SetValueTypeClass;
  published
    property ValueType: string read GetValueType write SetValueType stored IsValueTypeStored;
  end;

  { view data }

  // record

  TcxCustomGridRecordClass = class of TcxCustomGridRecord;

  TcxCustomGridRecord = class
  private
    //FData: Pointer;
    FIndex: Integer;
    FViewData: TcxCustomGridTableViewData;
    FViewInfo: TcxCustomGridRecordViewInfo;
    function GetController: TcxCustomGridTableController;
    function GetDataController: TcxCustomDataController;
    function GetDragHighlighted: Boolean;
    function GetFocused: Boolean;
    function GetGridView: TcxCustomGridTableView;
    function GetIsEditing: Boolean;
    function GetIsNewItemRecord: Boolean;
    function GetIsValid: Boolean;
    function GetLastParentRecordCount: Integer;
    function GetLevel: Integer;
    function GetPartVisible: Boolean;
    function GetRecordIndex: Integer;
    procedure SetExpanded(Value: Boolean);
    procedure SetFocused(Value: Boolean);
  protected
    FPixelScrollPosition: Integer;
    RecordInfo: TcxRowInfo;
    procedure RefreshRecordInfo; virtual;

    procedure DoCollapse(ARecurse: Boolean); virtual;
    procedure DoExpand(ARecurse: Boolean); virtual;
    //function GetDestroyingOnExpanding: Boolean; virtual;
    function GetExpandable: Boolean; virtual;
    function GetExpanded: Boolean; virtual;
    procedure ToggleExpanded; virtual;

    function GetHasCells: Boolean; virtual;
    function GetIsData: Boolean; virtual;
    function GetIsFirst: Boolean; virtual;
    function GetIsLast: Boolean; virtual;
    function GetIsParent: Boolean; virtual;
    function GetIsParentRecordLast(AIndex: Integer): Boolean; virtual;
    function GetParentRecord: TcxCustomGridRecord; virtual;
    function GetSelected: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    procedure SetSelected(Value: Boolean); virtual;

    function GetDisplayText(Index: Integer): string; virtual;
    function GetValueCount: Integer; virtual;
    function GetValue(Index: Integer): Variant; virtual;
    procedure SetDisplayText(Index: Integer; const Value: string); virtual;
    procedure SetValue(Index: Integer; const Value: Variant); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;

    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; virtual; abstract;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; virtual; abstract;

    property IsParent: Boolean read GetIsParent;
    property IsParentRecordLast[AIndex: Integer]: Boolean read GetIsParentRecordLast;
    property IsValid: Boolean read GetIsValid;
    property LastParentRecordCount: Integer read GetLastParentRecordCount;

    property Controller: TcxCustomGridTableController read GetController;
    property DataController: TcxCustomDataController read GetDataController;
    //property DestroyingOnExpanding: Boolean read GetDestroyingOnExpanding;
  public
    constructor Create(AViewData: TcxCustomGridTableViewData; AIndex: Integer;
      const ARecordInfo: TcxRowInfo); virtual;
    destructor Destroy; override;
    function CanFocus: Boolean; virtual;
    function CanFocusCells: Boolean; virtual;
    procedure Collapse(ARecurse: Boolean);
    procedure Expand(ARecurse: Boolean);
    function GetFirstFocusableChild: TcxCustomGridRecord; virtual;
    function GetLastFocusableChild(ARecursive: Boolean): TcxCustomGridRecord; virtual;
    procedure Invalidate(AItem: TcxCustomGridTableItem = nil); virtual;
    procedure MakeVisible;

    //property Data: Pointer read FData write FData;
    property DragHighlighted: Boolean read GetDragHighlighted;
    property DisplayTexts[Index: Integer]: string read GetDisplayText write SetDisplayText;
    property Expandable: Boolean read GetExpandable;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Focused: Boolean read GetFocused write SetFocused;
    property GridView: TcxCustomGridTableView read GetGridView;
    property HasCells: Boolean read GetHasCells;
    property Index: Integer read FIndex;
    property IsData: Boolean read GetIsData;
    property IsEditing: Boolean read GetIsEditing;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property IsNewItemRecord: Boolean read GetIsNewItemRecord;
    property Level: Integer read GetLevel;
    property ParentRecord: TcxCustomGridRecord read GetParentRecord;
    property PartVisible: Boolean read GetPartVisible;
    property PixelScrollPosition: Integer read FPixelScrollPosition;
    property RecordIndex: Integer read GetRecordIndex;
    property Selected: Boolean read GetSelected write SetSelected;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: Variant read GetValue write SetValue;
    property ViewData: TcxCustomGridTableViewData read FViewData;
    property ViewInfo: TcxCustomGridRecordViewInfo read FViewInfo;
    property Visible: Boolean read GetVisible;
  end;

  // view data

  TcxGridDataOperation = (doSorting, doGrouping, doFiltering);

  TcxCustomGridTableViewData = class(TcxCustomGridViewData)
  private
    FEditingRecord: TcxCustomGridRecord;
    FNewItemRecord: TcxCustomGridRecord;
    FRecords: TdxFastList;
    function GetController: TcxCustomGridTableController;
    function GetEditingRecord: TcxCustomGridRecord;
    function GetGridView: TcxCustomGridTableView;
    function GetInternalRecord(Index: Integer): TcxCustomGridRecord;
    function GetRecord(Index: Integer): TcxCustomGridRecord;
    function GetRecordCount: Integer;
    function GetViewInfo: TcxCustomGridTableViewInfo;
    function CreateRecord(AIndex: Integer): TcxCustomGridRecord;
  protected
    function GetFilterValueListClass: TcxGridFilterValueListClass; virtual;
    function GetRecordByKind(AKind, AIndex: Integer): TcxCustomGridRecord; virtual;
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; virtual; abstract;
    function GetRecordKind(ARecord: TcxCustomGridRecord): Integer; virtual;

    procedure AssignEditingRecord;

    procedure CreateNewItemRecord;
    procedure DestroyNewItemRecord;
    function GetNewItemRecordClass: TcxCustomGridRecordClass; virtual;
    procedure RecreateNewItemRecord;

    property Controller: TcxCustomGridTableController read GetController;
    property InternalRecords[Index: Integer]: TcxCustomGridRecord read GetInternalRecord;
    property ViewInfo: TcxCustomGridTableViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure Collapse(ARecurse: Boolean); virtual;
    procedure DestroyRecords;
    procedure Expand(ARecurse: Boolean); virtual;
    function GetRecordByIndex(AIndex: Integer): TcxCustomGridRecord;
    function GetRecordByRecordIndex(ARecordIndex: Integer): TcxCustomGridRecord;
    function GetRecordIndexByRecord(ARecord: TcxCustomGridRecord): Integer;
    function IsRecordIndexValid(AIndex: Integer): Boolean;
    procedure Refresh(ARecordCount: Integer); virtual;
    procedure RefreshRecords;

    procedure CheckNewItemRecord;
    function HasNewItemRecord: Boolean; virtual;

    function AddItemToFilter(AParent: TcxFilterCriteriaItemList; AItem: TcxCustomGridTableItem;
      AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; ADisplayText: string = '';
      AReplaceExistent: Boolean = True): TcxFilterCriteriaItem;
    function CreateFilterValueList: TcxGridFilterValueList;
    function GetDisplayText(ARecordIndex, AItemIndex: Integer; out AText: string;
      AUseCustomValue: Boolean = False; ACustomValueOperation: TcxGridDataOperation = doGrouping): Boolean; virtual;
    function GetDisplayTextFromValue(AItem: TcxCustomGridTableItem; AProperties: TcxCustomEditProperties;
      const AValue: Variant; AUseCustomValue: Boolean = False;
      ACustomValueOperation: TcxGridDataOperation = doGrouping): string; overload;
    function GetDisplayTextFromValue(ARecordIndex, AItemIndex: Integer; const AValue: Variant;
      AUseCustomValue: Boolean = False; ACustomValueOperation: TcxGridDataOperation = doGrouping): string; overload;

    // custom data handling
    function CustomCompareDataValues(AField: TcxCustomDataField; const AValue1, AValue2: Variant;
      AMode: TcxDataControllerComparisonMode): Integer; virtual;
    function GetCustomDataDisplayText(ARecordIndex, AItemIndex: Integer;
      AOperation: TcxGridDataOperation): string; overload; virtual;
    function GetCustomDataDisplayText(AItemIndex: Integer; const AValue: Variant): string; overload; virtual;
    function GetCustomDataValue(AField: TcxCustomDataField; const AValue: Variant;
      AOperation: TcxGridDataOperation): Variant; overload;
    function GetCustomDataValue(AItem: TcxCustomGridTableItem; const AValue: Variant;
      AOperation: TcxGridDataOperation): Variant; overload; virtual;
    function HasCustomDataHandling(AField: TcxCustomDataField; AOperation: TcxGridDataOperation): Boolean; overload;
    function HasCustomDataHandling(AItem: TcxCustomGridTableItem; AOperation: TcxGridDataOperation): Boolean; overload; virtual;
    function NeedsCustomDataComparison(AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Boolean; virtual;

    property EditingRecord: TcxCustomGridRecord read FEditingRecord;
    property GridView: TcxCustomGridTableView read GetGridView;
    property NewItemRecord: TcxCustomGridRecord read FNewItemRecord;
    property RecordCount: Integer read GetRecordCount;
    property Records[Index: Integer]: TcxCustomGridRecord read GetRecord;
  end;

  { controller }

  // drag & drop objects

  TcxCustomGridTableMovingObject = class(TcxCustomGridMovingObject)
  private
    function GetController: TcxCustomGridTableController;
    function GetCustomizationForm: TcxCustomGridTableCustomizationForm;
  protected
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    property Controller: TcxCustomGridTableController read GetController;
    property CustomizationForm: TcxCustomGridTableCustomizationForm read GetCustomizationForm;
  end;

  TcxGridItemContainerKind = Integer;

  TcxGridItemContainerZone = class
  public
    ItemIndex: Integer;
    constructor Create(AItemIndex: Integer);
    function IsEqual(Value: TcxGridItemContainerZone): Boolean; virtual;
  end;

  TcxCustomGridTableItemMovingObject = class(TcxCustomGridTableMovingObject)
  private
    FDestItemContainerKind: TcxGridItemContainerKind;
    FDestZone: TcxGridItemContainerZone;
    FSourceItemContainerKind: TcxGridItemContainerKind;

    procedure SetDestItemContainerKind(Value: TcxGridItemContainerKind);
    procedure SetDestZone(Value: TcxGridItemContainerZone);
  protected
    procedure CalculateDestParams(AHitTest: TcxCustomGridHitTest;
      out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone); virtual;
    procedure CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind); virtual;
    function GetCustomizationFormListBox: TcxCustomGridItemsListBox; override;
    function IsSourceCustomizationForm: Boolean; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    property DestItemContainerKind: TcxGridItemContainerKind read FDestItemContainerKind
      write SetDestItemContainerKind;
    property DestZone: TcxGridItemContainerZone read FDestZone write SetDestZone;
    property SourceItemContainerKind: TcxGridItemContainerKind read FSourceItemContainerKind
      write FSourceItemContainerKind;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;
  end;

  // customization form

  TcxCustomGridTableItemsListBoxClass = class of TcxCustomGridTableItemsListBox;

  TcxCustomGridTableItemsListBox = class(TcxCustomGridItemsListBox)
  private
    function GetGridView: TcxCustomGridTableView;
  protected
    procedure RefreshItemsAsTableItems;
    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  TcxCustomGridTableCustomizationForm = class(TcxCustomGridCustomizationForm)
  private
    FItemsListBox: TcxCustomGridTableItemsListBox;
    FItemsPage: TcxTabSheet;
  protected
    procedure CreateControls; override;
    function GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass; virtual;
    function GetItemsPageCaption: string; virtual; abstract;
    function GetItemsPageVisible: Boolean; virtual;
    procedure InitPageControl; override;

    property ItemsListBox: TcxCustomGridTableItemsListBox read FItemsListBox;
  public
    procedure RefreshData; override;
    property ItemsPage: TcxTabSheet read FItemsPage;
  end;

  // popups

  TcxGridFilterPopup = class(TdxFilterPopupWindow)
  private
    FGridView: TcxCustomGridTableView;

    function GetItem: TcxCustomGridTableItem;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;

    property GridView: TcxCustomGridTableView read FGridView;
    property Item: TcxCustomGridTableItem read GetItem;
  end;

  TcxGridFilterPopupClass = class of TcxGridFilterPopup;

  TcxGridFilterMRUItemsPopupClass = class of TcxGridFilterMRUItemsPopup;

  TcxGridFilterMRUItemsPopup = class(TdxFilterBoxMRUItemsPopup)
  private
    FGridView: TcxCustomGridTableView;
  protected
    procedure ApplyFilter(AItemIndex: Integer); override;
    function GetMRUItemCount: Integer; override;
    function GetMRUItems: TdxFilterBoxMRUItems; override;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;

    property GridView: TcxCustomGridTableView read FGridView;
  end;

  TdxCustomGridQuickCustomizationControl = class(TdxQuickCustomizationCustomControl)
  private
    procedure CheckAllItems(Sender: TObject);
    procedure SortItems(Sender: TObject);

    function GetGridView: TcxCustomGridTableView;
    function GetPopup: TcxCustomGridCustomizationPopup;
  protected
    procedure DoCheckAllItems(AValue: Boolean);
    function GetCheckingAllState: TcxCheckBoxState;
    procedure PopulateCommandListBox; override;
    procedure PopulateCheckListBox; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property GridView: TcxCustomGridTableView read GetGridView;
    property Popup: TcxCustomGridCustomizationPopup read GetPopup;
  end;

  TcxCustomGridCustomizationPopup = class(TcxCustomGridPopup)
  private
    FCustomizationControl: TdxCustomGridQuickCustomizationControl;
    FChangingItems: TList;
    FChangingItemsCheckStates: TdxIntegerIndexes;
    FSelectionInitializingInProcess: Boolean;
    function GetCheckListBox: TdxQuickCustomizationCustomCheckListBox;
    function GetGridView: TcxCustomGridTableView;
    procedure CheckListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
    procedure CheckListBoxCheckClicked(AIndex: Integer; AChecked: Boolean);
    procedure CheckListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CheckListBoxInitializeSelection(Sender: TObject);
    procedure CheckListBoxItemDragOver(AItem: Pointer; var AAccept: Boolean);
    procedure CheckListBoxSelectionChanged(Sender: TObject);
    procedure CheckListSelectedItemCheckedStateChanging(Sender: TObject);
    procedure CheckListSelectedItemCheckedStateChanged(Sender: TObject);
    procedure CustomizationControlInitialize(Sender: TObject);
  protected
    procedure AddCheckListBoxItems; virtual; abstract;
    procedure ApplyCheckListBoxSorting; virtual;
    procedure AdjustCustomizationControlSize;
    function CanMoveItem(AItem: Pointer): Boolean; virtual;
    procedure CreateCustomizationControl; virtual;
    function GetDropDownCount: Integer; virtual; abstract;
    procedure InitPopup; override;
    function IsCheckListBoxSorted: Boolean; virtual;
    function IsMultiColumnMode: Boolean; virtual;
    procedure RefreshCheckListBoxItems;
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure SetQuickCustomizationSorted(AValue: Boolean); virtual; abstract;
    procedure SetQuickCustomizationSortOptions;
    procedure SetVisibleRowCount(ACount: Integer);
    function SupportsItemMoving: Boolean; virtual; abstract;
    procedure SynchronizeTableItemsVisibilityWithListBox; virtual; abstract;
    procedure UpdateCommandListBoxItemsChecking; virtual;

    procedure GetCheckListBoxSelectedItems(AItems: TList; var AFocusedItem: TObject);
    function CanAddCommands: Boolean; virtual;
    function IsGridItem(AItem: TObject): Boolean; virtual; abstract;
    function IsVisibleGridItem(AItem: TObject): Boolean; virtual; abstract;
    procedure SetCheckListBoxSelectedItems(AItems: TList; AFocusedItem: TObject);

    procedure DoItemPosChanged(AItem: TObject); virtual; abstract;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); virtual; abstract;
    procedure ItemClicked(AItem: TObject; AChecked: Boolean); virtual;
    function GetItemIndex(AItem: TObject): Integer; virtual; abstract;
    procedure SetItemIndex(AItem: TObject; AIndex: Integer); virtual; abstract;
    procedure SetGridModified;

    property CheckListBox: TdxQuickCustomizationCustomCheckListBox read GetCheckListBox;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure CloseUp; override;
    procedure CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize); override;
    procedure Popup; override;
    //
    property CustomizationControl: TdxCustomGridQuickCustomizationControl read FCustomizationControl;
    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  TcxCustomGridItemsCustomizationPopupClass = class of TcxCustomGridItemsCustomizationPopup;

  TcxCustomGridItemsCustomizationPopup = class(TcxCustomGridCustomizationPopup)
  protected
    procedure AddCheckListBoxItems; override;
    function CanMoveItem(AItem: Pointer): Boolean; override;
    function GetDropDownCount: Integer; override;
    procedure SetQuickCustomizationSorted(AValue: Boolean); override;
    function SupportsItemMoving: Boolean; override;
    procedure SynchronizeTableItemsVisibilityWithListBox; override;

    procedure DoItemPosChanged(AItem: TObject); override;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); override;
    function IsGridItem(AItem: TObject): Boolean; override;
    function IsVisibleGridItem(AItem: TObject): Boolean; override;

    function GetItemIndex(AItem: TObject): Integer; override;
    procedure SetItemIndex(AItem: TObject; AIndex: Integer); override;
  end;

  // controllers

  TcxGridEditingController = class;

  TcxGridEditingControllerClass = class of TcxGridEditingController;

  TcxGridEditingController = class(TcxCustomEditingController)
  private
    FController: TcxCustomGridTableController;
    FEditingItem: TcxCustomGridTableItem;
    FEditingItemSetting: Boolean;
    FEditShowingTimerItem: TcxCustomGridTableItem;
    FIsEditAutoHeight: Boolean;
    FMultiLineEdit: TcxAutoHeightInplaceEdit;
    FMultiLineEditMinHeight: Integer;
    FNeedValidateEditVisibility: Boolean;
    FUpdateEditStyleNeeded: Boolean;
    function GetEditingCellViewInfo: TcxGridTableDataCellViewInfo; inline;
    function GetEditingProperties: TcxCustomEditProperties;
    function GetGridView: TcxCustomGridTableView;
    function GetAutoHeight: TcxInplaceEditAutoHeight;
    procedure SetEditingItem(Value: TcxCustomGridTableItem);
  protected
    //multiline editing
    procedure AfterAssignMultilineEditProperties;
    procedure BeforeAssignMultilineEditProperties;
    function CanUpdateMultilineEditHeight: Boolean;
    function CanUseAutoHeightEditor: Boolean; virtual;
    function GetAdjustedMultilineEditBounds(const ABounds: TRect): TRect;
    procedure MultilineEditTextChanged; override;
    procedure StartEditAutoHeight(AHeightChanged: Boolean); override;
    procedure UpdateMultilineEditBounds(const ABounds: TRect);

    procedure AfterViewInfoCalculate; virtual;
    procedure BeforeViewInfoCalculate; virtual;
    function CanInitEditing: Boolean; override;
    function CanUpdateEdit: Boolean; virtual;
    function CanUpdateEditValue: Boolean; override;
    function CellEditBoundsToEditBounds(ACellEditBounds: TRect): TRect;
    procedure CheckEdit;
    procedure CheckUsingMultilineEdit;
    procedure ClearEditingItem; override;
    function CreateMultilineEdit: TcxAutoHeightInplaceEdit; virtual;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoHideEdit(Accept: Boolean); override;
    procedure DoUpdateEdit; override;
    procedure FocusedRecordChanged; virtual;
    function GetCancelEditingOnExit: Boolean; override;
    function GetEditParent: TWinControl; override;
    function GetFocusedCellBounds: TRect; override;
    function GetFocusRectBounds: TRect; override;
    function GetHideEditOnExit: Boolean; override;
    function GetHideEditOnFocusedRecordChange: Boolean; override;
    function GetIsEditing: Boolean; override;
    procedure HideEditOnScrollContent; override;
    procedure InitEdit; override;
    procedure InitMultilineEdit; virtual;
    function IsNeedInvokeEditChangedEventsBeforePost: Boolean; override;
    function NeedCellViewInfoWhenUpdateEditValue: Boolean; virtual;
    procedure PostEditingData; virtual;
    function PrepareEdit(AItem: TcxCustomGridTableItem; AOnMouseEvent: Boolean): Boolean; virtual;
    procedure StartEditingByTimer; override;
    procedure UpdateEditBounds(const AEditBounds: TRect);
    procedure UpdateEditStyle(AEditCellViewInfo: TcxGridTableDataCellViewInfo);
    procedure UpdateInplaceParamsPosition; override;
    procedure ValidateEditVisibility;

    //editing value
    function GetDataController: TcxCustomDataController; override;
    function GetValue: TcxEditValue; override;
    procedure SetValue(const AValue: TcxEditValue); override;

    procedure EditAfterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure EditChanged(Sender: TObject); override;
    procedure EditDblClick(Sender: TObject); override;
    procedure EditFocusChanged(Sender: TObject); override;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure EditKeyPress(Sender: TObject; var Key: Char); override;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure EditValueChanged(Sender: TObject); override;

    property AutoHeight: TcxInplaceEditAutoHeight read GetAutoHeight;
    property EditingCellViewInfo: TcxGridTableDataCellViewInfo read GetEditingCellViewInfo;
    property EditingProperties: TcxCustomEditProperties read GetEditingProperties;
    property IsEditAutoHeight: Boolean read FIsEditAutoHeight;
    property MultiLineEdit: TcxAutoHeightInplaceEdit read FMultiLineEdit;
    property MultiLineEditMinHeight: Integer read FMultiLineEditMinHeight;
  public
    constructor Create(AController: TcxCustomGridTableController); virtual;
    destructor Destroy; override;

    procedure HideEdit(Accept: Boolean); override;
    function IsRecordModified: Boolean; // for internal use
    procedure ShowEdit; overload; override;
    procedure ShowEdit(AItem: TcxCustomGridTableItem); reintroduce; overload;
    procedure ShowEdit(AItem: TcxCustomGridTableItem; Key: Char); reintroduce; overload;
    procedure ShowEdit(AItem: TcxCustomGridTableItem; Shift: TShiftState; X, Y: Integer); reintroduce; overload;
    procedure UpdateEdit; override;

    procedure StartEditShowingTimer(AItem: TcxCustomGridTableItem);

    property Controller: TcxCustomGridTableController read FController;
    property EditingItem: TcxCustomGridTableItem read FEditingItem write SetEditingItem;
    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  TcxGridDragOpenInfoExpand = class(TcxCustomGridDragOpenInfo)
  public
    GridRecord: TcxCustomGridRecord;
    constructor Create(AGridRecord: TcxCustomGridRecord); virtual;
    function Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean; override;
    procedure Run; override;
  end;

  TcxCustomGridTableCanItemFocus = function(AOwner: TcxCustomGridTableView;
    AItemIndex: Integer; AData: TObject): Boolean;

  TcxGridTableClickedDataCellInfo = record
    Item: TcxCustomGridTableItem;
    Cell: TcxGridTableDataCellViewInfo;
    RecordIndex: Integer;
  end;

  TcxCustomGridTableController = class(TcxCustomGridController)
  private
    FAllowAppendRecord: Boolean;
    FAllowCheckEdit: Boolean;
    FBlockRecordKeyboardHandling: Boolean;
    FCheckEditNeeded: Boolean;
    FCheckingCoordinate: Boolean;
    FClickedCellViewInfo: TcxGridTableDataCellViewInfo;
    FClickedDataCellInfo: TcxGridTableClickedDataCellInfo;
    FDragDropText: string;
    FDragScrollDirection: TcxDirection;
    FDragScrollTimer: TcxTimer;
    FDragHighlightedRecord: TcxCustomGridRecord;
    FEatKeyPress: Boolean;
    FEditingController: TcxGridEditingController;
    FFilterMRUItemsPopup: TcxGridFilterMRUItemsPopup;
    FFilterPopup: TcxGridFilterPopup;
    FFindPanel: TcxGridFindPanel;
    FFocusedItem: TcxCustomGridTableItem;
    FFocusOnRecordFocusing: Boolean;
    FForcingWidthItem: TcxCustomGridTableItem;
    FGridModeBufferCountUpdateNeeded: Boolean;
    FGridModeBufferCountUpdateTimer: TcxTimer;
    FInternalTopRecordIndex: Integer;
    FIsDetailRecordFocusing: Boolean;
    FIsPullFocusing: Boolean;
    FIsReadyForImmediateEditing: Boolean;
    FIsRecordUnselecting: Boolean;
    FItemsCustomizationPopup: TcxCustomGridItemsCustomizationPopup;
    FMakeVisibleDetailRecordBounds: TRect;
    FMovingItem: TcxCustomGridTableItem;
    FNeedUpdateSiteOnFocusedRecordChanged: Boolean;
    FPixelScrollRecordOffset: Integer;
    FPixelScrollTopRecordIndexMaxValue: Integer;
    FPixelScrollTopRecordOffsetMaxValue: Integer;
    FPrevFocusedRecordIndex: Integer;
    FPullFocusingItem: TcxCustomGridTableItem;
    FPullFocusingMousePos: TPoint;
    FPullFocusingOriginHitTest: TcxCustomGridHitTest;
    FPullFocusingRecordId: Variant;
    FPullFocusingScrollingDirection: TcxDirection;
    FPullFocusingScrollingTimer: TcxTimer;
    FScrollDirection: TcxDirection;
    FScrollTimer: TcxTimer;
    FTracking: Boolean;
    FUnselectingRecordIndex: Integer;

    function GetEditingItem: TcxCustomGridTableItem; inline;
    function GetFilterMRUItemsPopup: TcxGridFilterMRUItemsPopup;
    function GetFilterPopup: TcxGridFilterPopup;
    function GetFocusedItemIndex: Integer;
    function GetFocusedRecordIndex: Integer;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetIncSearchingItem: TcxCustomGridTableItem;
    function GetIncSearchingText: string;
    function GetInternalTopRecordIndex: Integer;
    function GetIsEditing: Boolean; inline;
    function GetIsIncSearching: Boolean;
    function GetIsItemMoving: Boolean;
    function GetItemsCustomizationPopup: TcxCustomGridItemsCustomizationPopup;
    function GetLastVisibleRecordIndex: Integer;
    function GetMasterController: TcxCustomGridTableController;
    function GetMultiSelect: Boolean;
    function GetNewItemRecordFocused: Boolean;
    function GetPrevFocusedRecordIndex: Integer;
    function GetScrollRecordCount: Integer;
    function GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
    function GetSelectedRecordCount: Integer;
    function GetViewData: TcxCustomGridTableViewData; inline;
    function GetViewInfo: TcxCustomGridTableViewInfo; inline;
    procedure SetDragHighlightedRecord(Value: TcxCustomGridRecord);
    procedure SetEditingItem(Value: TcxCustomGridTableItem);
    procedure SetFocusedItem(Value: TcxCustomGridTableItem);
    procedure SetFocusedItemIndex(Value: Integer);
    procedure SetFocusedRecordIndex(Value: Integer);
    procedure SetIncSearchingText(const Value: string);
    procedure SetInternalTopRecordIndex(Value: Integer);
    procedure SetNewItemRecordFocused(Value: Boolean);
    procedure SetPixelScrollRecordOffset(const Value: Integer);
    procedure SetScrollDirection(Value: TcxDirection);
    procedure SetTopRecordIndex(Value: Integer);
    procedure SetTracking(Value: Boolean);

    procedure DragScrollTimerHandler(Sender: TObject);
    procedure GridModeBufferCountUpdateTimerHandler(Sender: TObject);
    procedure PullFocusingScrollingTimerHandler(Sender: TObject);

    procedure CreateScrollTimer;
    procedure DestroyScrollTimer;
    procedure ScrollTimerHandler(Sender: TObject);
  protected
    procedure AfterPaint; override;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure BeforePaint; override;
    function CanCancelDragStartOnCaptureObjectClear: Boolean; override;
    function CanFocusOnClick(X, Y: Integer): Boolean; override;
    procedure CheckInternalTopRecordIndex(var AIndex: Integer); virtual;
    procedure DetailFocused(ADetail: TcxCustomGridView); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure RemoveFocus; override;
    procedure SetFocus(ANotifyMaster: Boolean); override;

    procedure AfterOffset; virtual;
    procedure BeforeOffset; virtual;
    function CanAppend(ACheckOptions: Boolean): Boolean; virtual;
    procedure CancelCheckEditPost;
    function CanChangeFocusedRecordIndex(APrevRecordIndex, ANewRecordIndex: Integer): Boolean; virtual;
    function CanCopyToClipboard: Boolean; virtual;
    function CanDataPost: Boolean; virtual;
    function CanDelete(ACheckOptions: Boolean): Boolean; virtual;
    function CanEdit: Boolean; virtual;
    function CanEditFocusedItem: Boolean; virtual;
    procedure CheckCoordinates; override;
    function CanHScrollBarHide: Boolean; virtual;
    function CanInsert(ACheckOptions: Boolean): Boolean; virtual;
    function CanMakeRecordVisible(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function CanNormalSelectionOnMouse: Boolean; virtual;
    function CanUseAutoHeightEditing: Boolean; virtual;
    function CheckBrowseModeOnRecordChanging(ANewRecordIndex: Integer): Boolean; virtual;
    procedure CheckEdit; virtual;
    procedure CheckTopRecordIndex(var Value: Integer); virtual;
    procedure CheckTopRecordIndexAndOffset(var AIndex, AOffset: Integer); virtual;
    function DataScrollSize: Integer; virtual;
    function DoGetScrollBarPos: Integer; virtual;
    procedure DoMakeRecordVisible(ARecord: TcxCustomGridRecord); virtual;
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure DoSetScrollBarPos(Value: Integer); virtual;
    procedure EndGestureScroll; override;
    function FindNextCustomItem(AFocusedItemIndex, AItemCount: Integer;
      AGoForward, AGoOnCycle: Boolean; ACanFocus: TcxCustomGridTableCanItemFocus;
      AData: TObject; var AItemIndex: Integer; out ACycleChanged: Boolean): Boolean;
    procedure FocusedItemChanged(APrevFocusedItem: TcxCustomGridTableItem); virtual;
    procedure FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); virtual;
    procedure GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer); virtual;
    function GetCancelEditingOnExit: Boolean; virtual;
    function GetDlgCode: Integer; override;
    function GetFilterMRUItemsPopupClass: TcxGridFilterMRUItemsPopupClass; virtual;
    function GetFilterPopupClass: TcxGridFilterPopupClass; virtual;
    function GetFindFilterStartPosition(ARecordIndex: TdxNativeInt; AItemIndex: Integer;
      out AHighlightedText: string): Integer;
    function GetFocusedRecord: TcxCustomGridRecord; virtual;
    function GetIsRecordsScrollHorizontal: Boolean; virtual; abstract;
    function GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass; virtual;
    function GetMaxTopRecordIndexValue: Integer; virtual;
    procedure GetPixelScrollTopRecordIndexAndOffsetByBottomRecord(ABottomRecordIndex: Integer;
      out ATopRecordIndex, ATopRecordPixelScrollOffset: Integer); virtual;
    procedure GetPixelScrollTopRecordIndexAndOffsetMaxValues(out ARecordIndex, APixelScrollOffset: Integer); virtual;
    function GetPageVisibleRecordCount(AFirstRecordIndex: Integer; ACalculateDown: Boolean = True): Integer;
    function GetPatternObject(AObject: TPersistent): TPersistent; override;
    function GetScrollBarOffsetBegin: Integer; virtual;
    function GetScrollBarOffsetEnd: Integer; virtual;
    function GetScrollBarPos: Integer; virtual;
    function GetScrollBarRecordCount: Integer; virtual;
    function GetScrollDelta: Integer; virtual;
    function GetTopRecordIndex: Integer; virtual;
    function GetTopRecordIndexByBottomRecord(ABottomRecordIndex: Integer): Integer; virtual;
    function GetVisibleRecordCount(AFirstRecordIndex: Integer; ACalculateDown: Boolean = True): Integer; virtual;
    function IsClickedCell(ACell: TcxGridTableDataCellViewInfo): Boolean;
    function IsPixelBasedScrollDataPos: Boolean;
    function IsRecordPixelScrolling: Boolean;
    function NeedCheckInternalTopRecordIndex: Boolean; virtual;
    procedure PostCheckEdit;
    procedure ProcessCheckEditPost;
    procedure ScrollData(ADirection: TcxDirection); virtual;
    procedure SetClickedCellInfo(ACell: TcxGridTableDataCellViewInfo);
    procedure SetFocusedRecord(Value: TcxCustomGridRecord); virtual;
    procedure SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer); override;
    procedure SetScrollBarPos(Value: Integer); virtual;
    procedure SetTopRecordIndexWithOffset(ATopRecordIndex, ATopRecordOffset: Integer);
    procedure ValidateEditVisibility; virtual;
    function VisibleDataScrollSize: Integer; virtual;
    function WantSpecialKey(AKey: Word): Boolean; override;

    procedure CancelGridModeBufferCountUpdate;
    procedure CheckGridModeBufferCountUpdatePost;
    procedure PostGridModeBufferCountUpdate;
    property GridModeBufferCountUpdateNeeded: Boolean read FGridModeBufferCountUpdateNeeded;

    //find
    function CreateFindPanel: TcxGridFindPanel; virtual;
    //scrolling
    procedure AfterScrolling; override;
    procedure BeforeScrolling; override;
    procedure BeginGestureScroll(APos: TPoint); override;
    procedure CheckPixelScrollTopRecordIndexAndOffsetValues(var ATopRecordIndex, APixelScrollRecordOffset: Integer);
    procedure DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;
    function GetFirstScrollRecordIndex: Integer; virtual;
    function GetFirstIndexInScrollBand(ARecordIndexInBand: Integer): Integer; virtual;
    function GetLastIndexInScrollBand(ARecordIndexInBand: Integer): Integer;
    function GetLastScrollRecordIndex: Integer; virtual;
    function GetPixelScrollSize: Integer;
    function GetRecordCountInScrollBand(ARecordIndexInBand: Integer): Integer; virtual;
    function IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean; virtual;
    function IsRecordFullyVisibleAtTop(ARecordIndex: Integer): Boolean; virtual;
    function IsRecordPartVisibleAtTop(ARecordIndex: Integer): Boolean; virtual;
    procedure UpdatePixelScrollTopRecordIndexAndOffsetMaxValues;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); override;

    // internal drag and drop data scrolling
    function CanScrollData(ADirection: TcxDirection): Boolean; virtual;
    function GetScrollDataTimeInterval(ADirection: TcxDirection): Integer; virtual;
    property ScrollDirection: TcxDirection read FScrollDirection write SetScrollDirection;

    // selection
    function CanPostponeRecordSelection(AShift: TShiftState): Boolean; virtual;
    function CanProcessMultiSelect(AIsKeyboard: Boolean): Boolean; overload; virtual;
    function CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest;
      AShift: TShiftState): Boolean; overload; virtual;
    function CanProcessMultiSelect(AKey: Word; AShift: TShiftState;
      AFocusedRecordChanged: Boolean): Boolean; overload; virtual;
    procedure ChangeRecordSelection(ARecord: TcxCustomGridRecord; Value: Boolean);
    procedure CheckFocusedRecordSelectionWhenExit(ARecord: TcxCustomGridRecord);
    procedure DoMouseNormalSelection(AHitTest: TcxCustomGridHitTest); virtual;
    procedure DoMouseRangeSelection(AClearSelection: Boolean = True; AData: TObject = nil); virtual;
    procedure DoNormalSelection; virtual;
    procedure DoRangeSelection(AClearSelection: Boolean = True);
    procedure DoToggleRecordSelection;
    procedure FinishSelection; virtual;
    procedure InvalidateFocusedRecord; virtual;
    procedure InvalidateSelection; virtual;
    function IsKeyForMultiSelect(AKey: Word; AShift: TShiftState; AFocusedRecordChanged: Boolean): Boolean; virtual;
    function IsMultiSelectPersistent: Boolean;
    function IsRecordSelected(ARecord: TcxCustomGridRecord): Boolean;
    procedure MultiSelectKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure MultiSelectMouseDown(AHitTest: TcxCustomGridHitTest; AShift: TShiftState); virtual;
    procedure MultiSelectMouseUp(AHitTest: TcxCustomGridHitTest; AShift: TShiftState); virtual;
    procedure SelectFocusedRecord;
    procedure SetSelectionAnchor(AGridRecordIndex: Integer);
    function SupportsAdditiveSelection: Boolean; virtual;
    function SupportsRecordSelectionToggling: Boolean; virtual;

    // navigation
    function CanFocusNextItem(AFocusedItemIndex, ANextItemIndex: Integer;
      AGoForward, AGoOnCycle, AGoToNextRecordOnCycle: Boolean): Boolean; virtual;
    function FocusedRecordHasCells(ACheckCellSelectionAbility: Boolean): Boolean;
    procedure FocusNextPage(ASyncSelection: Boolean); virtual;
    procedure FocusPrevPage(ASyncSelection: Boolean); virtual;
    function GetPageRecordCount: Integer; virtual;
    function IsFirstPageRecordFocused: Boolean; virtual;
    function IsKeyForController(AKey: Word; AShift: TShiftState): Boolean; virtual;
    procedure ScrollPage(AForward: Boolean); virtual;
    procedure ScrollRecords(AForward: Boolean; ACount: Integer); virtual;
    procedure ShowNextPage; virtual;
    procedure ShowPrevPage; virtual;

    // pull focusing
    procedure DoPullFocusing(AHitTest: TcxGridRecordHitTest); virtual;
    procedure DoPullFocusingScrolling(ADirection: TcxDirection); virtual;
    function GetPullFocusingScrollingDirection(X, Y: Integer; out ADirection: TcxDirection): Boolean; virtual;
    function IsPullFocusingPosChanged: Boolean;
    procedure SavePullFocusingPos;
    procedure StartPullFocusing(AHitTest: TcxCustomGridHitTest); virtual;
    procedure StopPullFocusing;
    procedure StartPullFocusingScrolling(ADirection: TcxDirection);
    procedure StopPullFocusingScrolling;
    function SupportsPullFocusing: Boolean; virtual;
    property PullFocusingOriginHitTest: TcxCustomGridHitTest read FPullFocusingOriginHitTest;

    // delphi drag and drop
    function GetDragScrollDirection(X, Y: Integer): TcxDirection; virtual;
    function GetDragScrollInterval: Integer; virtual;
    function IsFirstRecordForDragScroll(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function IsLastRecordForDragScroll(ARecord: TcxCustomGridRecord): Boolean; virtual;
    procedure ProcessDragFocusing(X, Y: Integer);
    procedure StartDragScroll(ADirection: TcxDirection);
    procedure StopDragScroll;
    function IsDragScroll: Boolean;
    //---
    function GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo; virtual;
    function IsDragOpenHitTest(AHitTest: TcxCustomGridHitTest;
      out ADragOpenInfo: TcxCustomGridDragOpenInfo): Boolean;
    //---
    property DragHighlightedRecord: TcxCustomGridRecord read FDragHighlightedRecord write SetDragHighlightedRecord;

    // incremental searching
    function GetItemForIncSearching: TcxCustomGridTableItem; virtual;
    procedure IncSearchKeyDown(var Key: Word; Shift: TShiftState); virtual;
    property ItemForIncSearching: TcxCustomGridTableItem read GetItemForIncSearching;

    function GetEditingControllerClass: TcxGridEditingControllerClass; virtual;
    procedure InternalDoMakeRecordVisible(ARecord: TcxCustomGridRecord);

    property AllowAppendRecord: Boolean read FAllowAppendRecord write FAllowAppendRecord;
    property AllowCheckEdit: Boolean read FAllowCheckEdit write FAllowCheckEdit;
    property BlockRecordKeyboardHandling: Boolean read FBlockRecordKeyboardHandling
      write FBlockRecordKeyboardHandling;
    property CancelEditingOnExit: Boolean read GetCancelEditingOnExit;
    property EatKeyPress: Boolean read FEatKeyPress write FEatKeyPress;
    property FindPanel: TcxGridFindPanel read FFindPanel;
    property FirstScrollRecordIndex: Integer read GetFirstScrollRecordIndex;
    property FocusOnRecordFocusing: Boolean read FFocusOnRecordFocusing write FFocusOnRecordFocusing;
    property ForcingWidthItem: TcxCustomGridTableItem read FForcingWidthItem write FForcingWidthItem;
    property InternalTopRecordIndex: Integer read GetInternalTopRecordIndex write SetInternalTopRecordIndex;
    property IsReadyForImmediateEditing: Boolean read FIsReadyForImmediateEditing
      write FIsReadyForImmediateEditing;
    property IsRecordsScrollHorizontal: Boolean read GetIsRecordsScrollHorizontal;
    property LastScrollRecordIndex: Integer read GetLastScrollRecordIndex;
    property LastVisibleRecordIndex: Integer read GetLastVisibleRecordIndex;
    property MakeVisibleDetailRecordBounds: TRect read FMakeVisibleDetailRecordBounds write FMakeVisibleDetailRecordBounds;
    property MultiSelect: Boolean read GetMultiSelect;
    property NeedUpdateSiteOnFocusedRecordChanged: Boolean read FNeedUpdateSiteOnFocusedRecordChanged
      write FNeedUpdateSiteOnFocusedRecordChanged;
    property PixelScrollRecordOffset: Integer read FPixelScrollRecordOffset write SetPixelScrollRecordOffset;
    property ScrollBarOffsetBegin: Integer read GetScrollBarOffsetBegin;
    property ScrollBarOffsetEnd: Integer read GetScrollBarOffsetEnd;
    property ScrollBarRecordCount: Integer read GetScrollBarRecordCount;
    property ScrollBarPos: Integer read GetScrollBarPos write SetScrollBarPos;
    property ScrollDelta: Integer read GetScrollDelta;
    property ScrollRecordCount: Integer read GetScrollRecordCount;
    property Tracking: Boolean read FTracking write SetTracking;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
    property ViewInfo: TcxCustomGridTableViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure BeginDragAndDrop; override;
    procedure ControlFocusChanged; override;
    procedure DoCancelMode; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function FindNextItem(AFocusedItemIndex: Integer;
      AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean; out ACycleChanged: Boolean;
      ARecord: TcxCustomGridRecord): Integer; virtual;
    function FindNextRecord(AFocusedRecordIndex: Integer;
      AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): Integer;
    function HasFilterMRUItemsPopup: Boolean;
    function HasFilterPopup: Boolean;
    function HasFocusedControls: Boolean; override;
    function HasItemsCustomizationPopup: Boolean;
    function IMEComposition(var AMessage: TMessage): Boolean; override;
    function IMEStartComposition: Boolean; override;
    function IsClickableRecordHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
    function IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function ProcessDetailDialogChar(ADetail: TcxCustomGridView; ACharCode: Word): Boolean; override;
    function ProcessDialogChar(ACharCode: Word): Boolean; override;
    function SupportsTabAccelerators(AGridRecord: TcxCustomGridRecord): Boolean; virtual;

    procedure BeforeStartDrag; override;
    function CanDrag(X, Y: Integer): Boolean; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DrawDragImage(ACanvas: TcxCanvas; R: TRect); override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragDropText(ADragObject: TDragObject): string; virtual;
    procedure GetDragDropTextViewParams(out AParams: TcxViewParams); virtual;
    function GetDragImagesSize: TPoint; override;
    function HasDragImages: Boolean; override;
    procedure StartDrag(var DragObject: TDragObject); override;
    property DragDropText: string read FDragDropText;

    //find panel
    procedure ApplyFindFilterText(const AText: string);
    procedure ClearFindFilterText;
    function GetFindFilterText: string;
    procedure HideFindPanel;
    function IsFindPanelVisible: Boolean;
    procedure ShowFindPanel;

    procedure CancelIncSearching;
    function CheckEditing(var AFocusedRecordIndex: Integer; AGoForward: Boolean): Boolean;
    procedure CheckScrolling(const P: TPoint); virtual;
    procedure ClearSelection; virtual;
    procedure CreateNewRecord(AtEnd: Boolean); virtual;
    procedure DeleteSelection; virtual;
    function FocusFirstAvailableItem: Boolean; virtual;
    function FocusNextCell(AGoForward: Boolean; AProcessCellsOnly: Boolean = True;
      AAllowCellsCycle: Boolean = True; AFollowVisualOrder: Boolean = True; ANeedNormalizeSelection: Boolean = False): Boolean; virtual;
    function FocusNextItem(AFocusedItemIndex: Integer;
      AGoForward, AGoOnCycle, AGoToNextRecordOnCycle, AFollowVisualOrder: Boolean; ANeedNormalizeSelection: Boolean = False): Boolean; virtual;
    function FocusNextRecord(AFocusedRecordIndex: Integer;
      AGoForward, AGoOnCycle, AGoIntoDetail, AGoOutOfDetail: Boolean): Boolean;
    function FocusNextRecordWithSelection(AFocusedRecordIndex: Integer;
      AGoForward, AGoOnCycle, AGoIntoDetail: Boolean; ASyncSelection: Boolean = True): Boolean;
    function FocusRecord(AFocusedRecordIndex: Integer; ASyncSelection: Boolean): Boolean;
    procedure MakeFocusedItemVisible;
    procedure MakeFocusedRecordVisible;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); virtual; abstract;
    procedure MakeRecordVisible(ARecord: TcxCustomGridRecord); virtual;
    procedure SelectAll; virtual;
    procedure SelectAllRecords;

    function GoToFirst(ASyncSelection: Boolean = True): Boolean;
    function GoToLast(AGoIntoDetail: Boolean; ASyncSelection: Boolean = True): Boolean;
    function GoToNext(AGoIntoDetail: Boolean; ASyncSelection: Boolean = True): Boolean;
    function GoToPrev(AGoIntoDetail: Boolean; ASyncSelection: Boolean = True): Boolean;
    function IsFinish: Boolean;
    function IsStart: Boolean;

    property ClickedCellViewInfo: TcxGridTableDataCellViewInfo read FClickedCellViewInfo write FClickedCellViewInfo;
    property EditingController: TcxGridEditingController read FEditingController;
    property EditingItem: TcxCustomGridTableItem read GetEditingItem write SetEditingItem;
    property FilterMRUItemsPopup: TcxGridFilterMRUItemsPopup read GetFilterMRUItemsPopup;
    property FilterPopup: TcxGridFilterPopup read GetFilterPopup;
    property FocusedItem: TcxCustomGridTableItem read FFocusedItem write SetFocusedItem;
    property FocusedItemIndex: Integer read GetFocusedItemIndex write SetFocusedItemIndex;
    property FocusedRecord: TcxCustomGridRecord read GetFocusedRecord write SetFocusedRecord;
    property FocusedRecordIndex: Integer read GetFocusedRecordIndex write SetFocusedRecordIndex;
    property GridView: TcxCustomGridTableView read GetGridView;
    property IncSearchingItem: TcxCustomGridTableItem read GetIncSearchingItem;
    property IncSearchingText: string read GetIncSearchingText write SetIncSearchingText;
    property IsPullFocusing: Boolean read FIsPullFocusing;
    property IsEditing: Boolean read GetIsEditing;
    property IsIncSearching: Boolean read GetIsIncSearching;
    property IsItemMoving: Boolean read GetIsItemMoving;
    property ItemsCustomizationPopup: TcxCustomGridItemsCustomizationPopup read GetItemsCustomizationPopup;
    property MasterController: TcxCustomGridTableController read GetMasterController;
    property MovingItem: TcxCustomGridTableItem read FMovingItem;
    property NewItemRecordFocused: Boolean read GetNewItemRecordFocused write SetNewItemRecordFocused;
    property PrevFocusedRecordIndex: Integer read GetPrevFocusedRecordIndex write FPrevFocusedRecordIndex;
    property SelectedRecordCount: Integer read GetSelectedRecordCount;
    property SelectedRecords[Index: Integer]: TcxCustomGridRecord read GetSelectedRecord;
    property TopRecordIndex: Integer read GetTopRecordIndex write SetTopRecordIndex;
  end;

  { painters }

  TcxCustomGridPartPainter = class(TcxCustomGridCellPainter);

  TcxGridCustomButtonPainter = class(TcxCustomGridCellPainter)
  protected
    function CanDrawFocusRect: Boolean; virtual; abstract;
    procedure DoDrawFocusRect; virtual;
    procedure DrawFocusRect;
    function ExcludeFromClipRect: Boolean; override;
    function GetGridViewInfo: TcxCustomGridViewInfo; virtual; abstract;
    function GetFocusRect: TRect; virtual;
  end;

  TcxGridCheckBoxPainter = class(TcxGridCustomButtonPainter)
  private
    function GetViewInfo: TcxGridCheckBoxViewInfo;
  protected
    procedure DrawContent; override;
    function ExcludeFromClipRect: Boolean; override;

    property ViewInfo: TcxGridCheckBoxViewInfo read GetViewInfo;
  end;

  { TcxGridPinPainter }

  TcxGridPinPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridPinViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawImage; virtual;

    property ViewInfo: TcxGridPinViewInfo read GetViewInfo;
  end;

  TcxGridPartCustomButtonPainter = class(TcxGridCustomButtonPainter)
  private
    function GetViewInfo: TcxGridCustomButtonViewInfo; inline;
  protected
    function CanDrawFocusRect: Boolean; override;
    function GetGridViewInfo: TcxCustomGridViewInfo; override;

    property ViewInfo: TcxGridCustomButtonViewInfo read GetViewInfo;
  end;

  TcxGridPartCloseButtonPainter = class(TcxGridPartCustomButtonPainter)
  protected
    procedure DrawContent; override;
  end;

  TcxGridPartButtonPainter = class(TcxGridPartCustomButtonPainter)
  protected
    procedure Paint; override;
  end;

  TcxGridFilterActivateButtonPainter = class(TcxGridPartCustomButtonPainter)
  private
    function GetViewInfo: TcxGridFilterActivateButtonViewInfo; inline;
  protected
    procedure DrawContent; override;

    property ViewInfo: TcxGridFilterActivateButtonViewInfo read GetViewInfo;
  end;

  TcxGridFilterDropDownButtonPainter = class(TcxGridPartCustomButtonPainter)
  protected
    procedure DrawContent; override;
  end;

  TcxGridPartContainerPainter = class(TcxCustomGridPartPainter)
  private
    function GetViewInfo: TcxGridPartContainerViewInfo; inline;
  protected
    procedure DrawButtons; virtual;
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;

    property ViewInfo: TcxGridPartContainerViewInfo read GetViewInfo;
  end;

  TcxGridFilterPainter = class(TcxGridPartContainerPainter)
  private
    FTextWasUnderlined: Boolean;

    function GetViewInfo: TcxGridFilterViewInfo; inline;
  protected
    procedure DrawBackground(const R: TRect); override;
    procedure PrepareCanvasForDrawText; override;
    procedure UnprepareCanvasForDrawText; override;

    property ViewInfo: TcxGridFilterViewInfo read GetViewInfo;
  end;

  TcxGridFindPanelPainter = class(TcxGridPartContainerPainter)
  private
    function GetViewInfo: TcxGridFindPanelViewInfo; inline;
  protected
    procedure DrawBackground(const R: TRect); override;
    procedure DrawBorders; override;

    property ViewInfo: TcxGridFindPanelViewInfo read GetViewInfo;
  end;

  // records

  TcxGridTableDataCellPainterClass = class of TcxGridTableDataCellPainter;

  TcxGridTableDataCellPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridTableDataCellViewInfo; inline;
  protected
    procedure DrawContent; override;

    function CanDrawFocusRect: Boolean; virtual;
    procedure DoDrawFocusRect; virtual;
    procedure DrawEditViewInfo; virtual;
    procedure DrawFocusRect;
    function GetFocusRect: TRect; virtual;
    procedure Paint; override;
    property ViewInfo: TcxGridTableDataCellViewInfo read GetViewInfo;
  end;

  TcxCustomGridRecordPainterClass = class of TcxCustomGridRecordPainter;

  TcxCustomGridRecordPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxCustomGridRecordViewInfo; inline;
  protected
    procedure AfterPaint; override;
    procedure BeforePaint; override;
    procedure DrawBackground; override;
    procedure DrawExpandButton; virtual;
    function DrawExpandButtonBeforePaint: Boolean; virtual;
    procedure DrawFocusRect; virtual;
    function GetExpandButtonState: TcxExpandButtonState; virtual;
    procedure Paint; override;
    property ViewInfo: TcxCustomGridRecordViewInfo read GetViewInfo;
  end;

  TcxCustomGridRecordsPainterClass = class of TcxCustomGridRecordsPainter;

  TcxCustomGridRecordsPainter = class
  private
    FCanvas: TcxCanvas;
    FViewInfo: TcxCustomGridRecordsViewInfo;
  protected
    //procedure BeforePaint; virtual;
    procedure DrawItems; virtual;
    procedure Paint; virtual;
    property Canvas: TcxCanvas read FCanvas;
    property ViewInfo: TcxCustomGridRecordsViewInfo read FViewInfo;
  public
    constructor Create(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridRecordsViewInfo); virtual;
    procedure MainPaint;
  end;

  // view

  TcxNavigatorSitePainter = class(TcxCustomGridCellPainter)
  protected
    function ExcludeFromClipRect: Boolean; override;
  end;

  TcxCustomGridTablePainter = class(TcxCustomGridPainter)
  private
    function GetController: TcxCustomGridTableController; inline;
    function GetViewInfo: TcxCustomGridTableViewInfo; inline;
    function GetGridView: TcxCustomGridTableView; inline;
  protected
    function CanOffset(AItemsOffset, DX, DY: Integer): Boolean; virtual;
    procedure DrawBackground; override;
    procedure DrawFilterBar; virtual;
    procedure DrawFindPanel; virtual;
    procedure DrawInfoText; virtual;
    procedure DrawNavigator; virtual;
    procedure DrawRecords; virtual;
    procedure Offset(AItemsOffset: Integer); overload; virtual;
    procedure Offset(DX, DY: Integer); overload; virtual;
    procedure PaintBefore; override;
    procedure PaintContent; override;
  public
    procedure DoOffset(AItemsOffset, DX, DY: Integer);
    procedure DrawFocusRect(const R: TRect; AHideFocusRect: Boolean); override;
    property Controller: TcxCustomGridTableController read GetController;
    property GridView: TcxCustomGridTableView read GetGridView;
    property ViewInfo: TcxCustomGridTableViewInfo read GetViewInfo;
  end;

  { view infos }

  // part

  TcxGridPartAlignment = (gpaTop, gpaBottom);

  TcxCustomGridPartViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FHeight: Integer;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetGridViewInfo: TcxCustomGridTableViewInfo; inline;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  protected
    function CalculateBounds: TRect; virtual;
    procedure CalculateInvisible; virtual;
    procedure CalculateVisible; virtual;
    function CustomDrawBackground(ACanvas: TcxCanvas): Boolean; override;
    function GetAlignment: TcxGridPartAlignment; virtual; abstract;
    //function GetHeight: Integer; override;
    function GetIsAutoWidth: Boolean; virtual; abstract;
    function GetIsPart: Boolean; virtual;
    function GetIsScrollable: Boolean; virtual; abstract;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function HasCustomDrawBackground: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    property Height: Integer read FHeight write FHeight;
    property IsPart: Boolean read GetIsPart;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure MainCalculate;
    property Alignment: TcxGridPartAlignment read GetAlignment;
    property GridView: TcxCustomGridTableView read GetGridView;
    property GridViewInfo: TcxCustomGridTableViewInfo read GetGridViewInfo;
    property Index: Integer read GetIndex write SetIndex;
    property IsAutoWidth: Boolean read GetIsAutoWidth;
    property IsScrollable: Boolean read GetIsScrollable;
  end;

  TcxGridPartCustomCellViewInfo = class(TcxCustomGridViewCellViewInfo)
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    function DoCalculateHeight: Integer; virtual; abstract;
    function DoCalculateWidth: Integer; virtual; abstract;
    function GetAdornerTargetElementName: string; virtual;
    function GetButtonState: TcxButtonState; override;
    function GetEnabled: Boolean; virtual;
    function GetHotTrack: Boolean; override;
    function GetFocused: Boolean; virtual; abstract;
  public
    property Enabled: Boolean read GetEnabled;
    property Focused: Boolean read GetFocused;
  end;

  TcxGridCustomToggleCellViewInfo = class(TcxGridPartCustomCellViewInfo)
  protected
    function CanToggle: Boolean; virtual;
    function GetFocused: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure Toggle; virtual; abstract;
  end;

  TcxGridCheckBoxViewInfo = class(TcxGridCustomToggleCellViewInfo)
  private
    FCheckBoxState: TcxCheckBoxState;
  protected
    function CalculateCheckBoxState: TcxCheckBoxState; virtual;
    procedure Click; override;
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    procedure Toggle; override;

    property CheckBoxState: TcxCheckBoxState read FCheckBoxState;
  public
    procedure Calculate(const ABounds: TRect); override;
  end;

  TcxGridPinViewInfo = class(TcxGridCustomToggleCellViewInfo)
  private
    FPinned: Boolean;
  protected
    function CalculatePinned: Boolean; virtual;
    function CanDrawImage: Boolean; virtual;
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetImageBounds: TRect; virtual;
    function GetImageSize: TSize; virtual;
    function GetMargins: TRect; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure Toggle; override;

    property Pinned: Boolean read FPinned;
  public
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean; override;
  end;

  TcxGridPartContainerItemViewInfoClass = class of TcxGridPartContainerItemViewInfo;

  TcxGridPartContainerItemViewInfo = class(TcxGridPartCustomCellViewInfo)
  private
    FContainer: TcxGridPartContainerButtonsViewInfo;

    function GetGridView: TcxCustomGridTableView; inline;
  protected
    function GetAdornerTargetElementVisible: Boolean; override;
    function GetAlignment: TcxGridFilterButtonAlignment; virtual;
    function GetCanvas: TcxCanvas; override;
    function GetVisible: Boolean; override;
    function GetEnabled: Boolean; override;
    procedure MouseLeave; override;
  public
    constructor Create(AContainer: TcxGridPartContainerButtonsViewInfo); reintroduce; virtual;

    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    function MouseMove(AHitTest: TcxCustomGridHitTest;
      AShift: TShiftState): Boolean; override;
    function MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property Alignment: TcxGridFilterButtonAlignment read GetAlignment;
    property Container: TcxGridPartContainerButtonsViewInfo read FContainer;
    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  TcxGridCustomButtonViewInfo = class(TcxGridPartContainerItemViewInfo);

  TcxGridPartCloseButtonViewInfo = class(TcxGridCustomButtonViewInfo)
  protected
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAdornerTargetElementName: string; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetFocused: Boolean; override;
  end;

  TcxGridPartButtonViewInfo = class(TcxGridCustomButtonViewInfo)
  protected
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetFocused: Boolean; override;
  end;

  // find panel parts

  TcxGridFindPanelEditViewInfo = class(TcxGridPartContainerItemViewInfo)
  private
    function GetContainer: TcxGridFindPanelButtonsViewInfo;
    function GetController: TcxCustomGridTableController;
    function GetFindPanel: TcxGridFindPanel;
  protected
    procedure AdjustWidth(var AWidth: Integer); virtual;
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAdornerTargetElementName: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetFocused: Boolean; override;

    procedure ShowEdit;
    procedure HideEdit;

    property Container: TcxGridFindPanelButtonsViewInfo read GetContainer;
    property Controller: TcxCustomGridTableController read GetController;
    property FindPanel: TcxGridFindPanel read GetFindPanel;
  public
    procedure Calculate(ALeftBound: Integer; ATopBound: Integer;
      AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure Paint(ACanvas: TcxCanvas = nil); override;
  end;

  TcxGridFindPanelCloseButtonViewInfo = class(TcxGridPartCloseButtonViewInfo)
  private
    function GetContainer: TcxGridFindPanelButtonsViewInfo;
    function GetController: TcxCustomGridTableController;
  protected
    procedure Click; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetVisible: Boolean; override;
    function GetFocused: Boolean; override;

    property Container: TcxGridFindPanelButtonsViewInfo read GetContainer;
    property Controller: TcxCustomGridTableController read GetController;
  end;

  TcxGridFindPanelButtonViewInfo = class(TcxGridPartButtonViewInfo)
  private
    function GetContainer: TcxGridFindPanelButtonsViewInfo;
    function GetController: TcxCustomGridTableController;
  protected
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAlignment: TcxGridFilterButtonAlignment; override;

    property Container: TcxGridFindPanelButtonsViewInfo read GetContainer;
    property Controller: TcxCustomGridTableController read GetController;
  end;

  TcxGridFindPanelFindButtonViewInfo = class(TcxGridFindPanelButtonViewInfo)
  protected
    procedure Click; override;
    function GetAdornerTargetElementName: string; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetText: string; override;
    function GetVisible: Boolean; override;
    function GetFocused: Boolean; override;
  public
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
  end;

  TcxGridFindPanelClearButtonViewInfo = class(TcxGridFindPanelButtonViewInfo)
  protected
    procedure Click; override;
    function GetAdornerTargetElementName: string; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetText: string; override;
    function GetVisible: Boolean; override;
    function GetFocused: Boolean; override;
  public
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
  end;

  //filter parts

  TcxGridFilterCloseButtonViewInfo = class(TcxGridPartCloseButtonViewInfo)
  private
    function GetContainer: TcxGridFilterButtonsViewInfo;
    function GetFilter: TcxDataFilterCriteria;
  protected
    procedure Click; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetVisible: Boolean; override;

    property Container: TcxGridFilterButtonsViewInfo read GetContainer;
    property Filter: TcxDataFilterCriteria read GetFilter;
  end;

  TcxGridFilterActivateButtonViewInfo = class(TcxGridCustomButtonViewInfo)
  private
    function GetChecked: Boolean;
    function GetContainer: TcxGridFilterButtonsViewInfo;
    function GetFilter: TcxDataFilterCriteria;
  protected
    procedure Click; override;
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAdornerTargetElementName: string; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    function GetFocused: Boolean; override;

    property Container: TcxGridFilterButtonsViewInfo read GetContainer;
    property Filter: TcxDataFilterCriteria read GetFilter;
  public
    property Checked: Boolean read GetChecked;
  end;

  TcxGridFilterDropDownButtonViewInfo = class(TcxGridCustomButtonViewInfo)
  private
    function GetContainer: TcxGridFilterButtonsViewInfo;
    function GetDropDownWindowValue: TcxGridFilterMRUItemsPopup;
  protected
    procedure BeforeStateChange; override;
    function CaptureMouseOnPress: Boolean; override;
    function DoCalculateHeight: Integer; override;
    function DoCalculateWidth: Integer; override;
    function GetAdornerTargetElementName: string; override;
    function GetAlignment: TcxGridFilterButtonAlignment; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    function GetFocused: Boolean; override;

    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    function GetDropDownWindowOwnerBounds: TRect; override;

    property Container: TcxGridFilterButtonsViewInfo read GetContainer;
    property DropDownWindow: TcxGridFilterMRUItemsPopup read GetDropDownWindowValue;
  public
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
  end;

  TcxGridFilterCustomizeButtonViewInfo = class(TcxGridPartButtonViewInfo)
  private
    function GetContainer: TcxGridFilterButtonsViewInfo;
  protected
    procedure Click; override;
    function GetAdornerTargetElementName: string; override;
    function GetAlignment: TcxGridFilterButtonAlignment; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;

    property Container: TcxGridFilterButtonsViewInfo read GetContainer;
  end;

  // containers

  { TcxGridPartCustomContainerButtonsViewInfo }

  TcxGridPartContainerButtonsViewInfo = class
  private
    FIsRightToLeftConverted: Boolean;
    FItems: TdxFastObjectList;
    FOwner: TcxCustomGridPartViewInfo;

    function GetCount: Integer; inline;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetHeight: Integer;
    function GetItem(Index: Integer): TcxGridPartContainerItemViewInfo;
    function GetScaleFactor: TdxScaleFactor;
    function GetWidthLeftPart: Integer;
    function GetWidthRightPart: Integer;
  protected
    procedure AddItems; virtual; abstract;
    procedure DestroyItems; virtual;

    procedure AddAdornerTargetElements(AList: TStrings); virtual;
    function CanCalculateItem(AItem: TcxGridPartCustomCellViewInfo): Boolean; virtual;
    function GetButtonsOffset: Integer; virtual;
    function GetFirstOffset: Integer; virtual;
    function GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer; virtual;
    function GetEnabled: Boolean; virtual;

    property GridView: TcxCustomGridTableView read GetGridView;
    property Owner: TcxCustomGridPartViewInfo read FOwner;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TcxCustomGridPartViewInfo); virtual;
    destructor Destroy; override;

    function AddItem(AItemClass: TcxGridPartContainerItemViewInfoClass): TcxGridPartContainerItemViewInfo;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled;
    property Height: Integer read GetHeight;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Items[Index: Integer]: TcxGridPartContainerItemViewInfo read GetItem; default;
    property WidthLeftPart: Integer read GetWidthLeftPart;
    property WidthRightPart: Integer read GetWidthRightPart;
  end;

  { TcxGridFilterButtonsViewInfo }

  TcxGridFilterButtonsViewInfo = class(TcxGridPartContainerButtonsViewInfo)
  private
    function GetFilterViewInfo: TcxGridFilterViewInfo;
  protected
    FDropDownButtonViewInfo: TcxGridFilterDropDownButtonViewInfo;

    procedure AddItems; override;
    procedure DestroyItems; override;

    function GetButtonsOffset: Integer; override;
    function GetFirstOffset: Integer; override;
    function GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer; override;
    function GetEnabled: Boolean; override;

    property FilterViewInfo: TcxGridFilterViewInfo read GetFilterViewInfo;
  public
    property DropDownButtonViewInfo: TcxGridFilterDropDownButtonViewInfo read FDropDownButtonViewInfo;
  end;

  { TcxGridFindPanelButtonsViewInfo }

  TcxGridFindPanelButtonsViewInfo = class(TcxGridPartContainerButtonsViewInfo)
  private
    FEditViewInfo: TcxGridFindPanelEditViewInfo;
    FFindButton: TcxGridFindPanelFindButtonViewInfo;

    function GetFindPanelViewInfo: TcxGridFindPanelViewInfo;
  protected
    procedure AddItems; override;
    procedure DestroyItems; override;

    procedure CalculateEditViewInfo(const ABounds: TRect);
    function CanCalculateItem(AItem: TcxGridPartCustomCellViewInfo): Boolean; override;
    function GetButtonsOffset: Integer; override;
    function GetFirstOffset: Integer; override;
    function GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer; override;

    property EditViewInfo: TcxGridFindPanelEditViewInfo read FEditViewInfo;
    property FindButton: TcxGridFindPanelFindButtonViewInfo read FFindButton;
    property FindPanelViewInfo: TcxGridFindPanelViewInfo read GetFindPanelViewInfo;
  public
    procedure Calculate(const ABounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
  end;

  { TcxGridPartCustomContainerViewInfo }

  TcxGridPartContainerViewInfo = class(TcxCustomGridPartViewInfo)
  private
    FButtonsViewInfo: TcxGridPartContainerButtonsViewInfo;
  protected
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    function CalculateButtonsViewInfoBounds: TRect; virtual;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetHotTrack: Boolean; override;
    function GetIsAutoWidth: Boolean; override;
    function GetIsCheck: Boolean; override;
    function GetIsScrollable: Boolean; override;
    function InvalidateOnStateChange: Boolean; override;

    function CreateButtonsViewInfo: TcxGridPartContainerButtonsViewInfo; virtual; abstract;
    procedure DestroyButtonsViewInfo; virtual;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property ButtonsViewInfo: TcxGridPartContainerButtonsViewInfo read FButtonsViewInfo;
  end;

  { TcxGridFilterViewInfo }

  TcxGridFilterViewInfoClass = class of TcxGridFilterViewInfo;

  TcxGridFilterViewInfo = class(TcxGridPartContainerViewInfo)
  private
    function GetButtonsViewInfo: TcxGridFilterButtonsViewInfo;
    function GetFilter: TcxDataFilterCriteria;
    function GetFiltering: TcxCustomGridTableFiltering;
  protected
    function CalculateHeight: Integer; override;
    function CreateButtonsViewInfo: TcxGridPartContainerButtonsViewInfo; override;
    function GetAlignment: TcxGridPartAlignment; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasMouse(AHitTest: TcxCustomGridHitTest): Boolean; override;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    property Filter: TcxDataFilterCriteria read GetFilter;
    property Filtering: TcxCustomGridTableFiltering read GetFiltering;
  public
    property ButtonsViewInfo: TcxGridFilterButtonsViewInfo read GetButtonsViewInfo;
  end;

  { TcxGridFindPanelViewInfo }

  TcxGridFindPanelViewInfo = class(TcxGridPartContainerViewInfo)
  private
    function GetButtonsViewInfo: TcxGridFindPanelButtonsViewInfo;
    function GetController: TcxCustomGridTableController;
    function GetFindPanel: TcxGridFindPanel;
  protected
    procedure CalculateInvisible; override;
    procedure CalculateVisible; override;
    function CreateButtonsViewInfo: TcxGridPartContainerButtonsViewInfo; override;
    function GetAlignment: TcxGridPartAlignment; override;
    function GetBorders: TcxBorders; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    property Controller: TcxCustomGridTableController read GetController;
    property FindPanel: TcxGridFindPanel read GetFindPanel;
  public
    property ButtonsViewInfo: TcxGridFindPanelButtonsViewInfo read GetButtonsViewInfo;
  end;

  // cells

  TcxGridTableCellViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FRecordViewInfo: TcxCustomGridRecordViewInfo;
    FSelected: Boolean;
    FSelectedCalculated: Boolean;
    function GetCacheItem: TcxCustomGridTableViewInfoCacheItem; inline;
    function GetController: TcxCustomGridTableController; inline;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetGridRecord: TcxCustomGridRecord; inline;
    function GetSelected: Boolean;
  protected
    function CalculateSelected: Boolean; virtual;
    procedure CalculateSelectedNeeded;
    function GetAlwaysSelected: Boolean; virtual;
    function GetCanvas: TcxCanvas; override;
    function GetGridViewInfo: TcxCustomGridTableViewInfo;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetTransparent: Boolean; override;
    procedure HotTrackChanged; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    property AlwaysSelected: Boolean read GetAlwaysSelected;
    property CacheItem: TcxCustomGridTableViewInfoCacheItem read GetCacheItem;
    property Controller: TcxCustomGridTableController read GetController;
  public
    constructor Create(ARecordViewInfo: TcxCustomGridRecordViewInfo); reintroduce; virtual;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CanDrawSelected: Boolean; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property GridRecord: TcxCustomGridRecord read GetGridRecord;
    property GridView: TcxCustomGridTableView read GetGridView;
    property GridViewInfo: TcxCustomGridTableViewInfo read GetGridViewInfo;
    property RecordViewInfo: TcxCustomGridRecordViewInfo read FRecordViewInfo;
    property Selected: Boolean read GetSelected;
  end;

  { TcxGridTableDataCellViewInfo }

  TcxGridTableDataCellViewInfo = class(TcxGridTableCellViewInfo, IcxEditOwner)
  private
    FEditViewData: TcxCustomEditViewData;
    FEditViewInfo: TcxCustomEditViewInfo;
    FIsLocalCopyOfEditViewData: Boolean;
    FItem: TcxCustomGridTableItem;
    FProperties: TcxCustomEditProperties;
    FStyle: TcxEditStyle;
    FWasFocusedBeforeClick: Boolean;
    procedure AfterDrawCellBackgroundHandler(ACanvas: TcxCanvas);
    procedure AfterDrawCellValueHandler(ACanvas: TcxCanvas);
    procedure CalculateEditorBoundsHandler(AViewInfo: TcxCustomEditViewInfo; var R: TRect);
    procedure CanDrawEditValueHandler(Sender: TcxCustomEditViewInfo; var Allow: Boolean);
    function GetEditing: Boolean;
    function GetMousePos: TPoint;
    function GetProperties: TcxCustomEditProperties;
    function GetShowButtons: Boolean;
  protected
    procedure AfterCustomDraw(ACanvas: TcxCanvas); override;
    procedure BeforeCustomDraw(ACanvas: TcxCanvas); override;
    procedure CalculateEditViewInfo(AEditViewInfo: TcxCustomEditViewInfo;
      const AMousePos: TPoint); virtual;
    function CalculateHeight: Integer; override;
    function CalculateSelected: Boolean; override;
    function CanActivateEditOnMouseDown(AHitTest: TcxCustomGridHitTest;
      AButton: TMouseButton; AShift: TShiftState): Boolean; virtual;
    function CanAutoHeight: Boolean; virtual;
    function CanImmediateEdit(AHitTest: TcxCustomGridHitTest): Boolean; virtual;
    function CanProcessMouseLeave: Boolean; override;
    function CanShowEdit: Boolean; virtual;
    procedure CheckEditHotTrack(const AMousePos: TPoint);
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoCalculateParams; override;
    procedure EditHotTrackChanged;
    function GetAreaBounds: TRect; override;
    function GetAutoHeight: Boolean; virtual;
    function GetDisplayValue: TcxEditValue; virtual;
    function GetEditBounds: TRect; virtual;
    function GetEditSize(AEditSizeProperties: TcxEditSizeProperties): TSize;
    function GetEditViewDataBounds: TRect; virtual;
    procedure GetEditViewDataContentOffsets(var R: TRect); virtual;
    function GetFocused: Boolean; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetItemProperties: TcxCustomEditProperties; virtual;
    function GetMaxLineCount: Integer; virtual;
    function GetMultiLine: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealEditViewDataBounds: TRect; virtual;
    procedure GetRealEditViewDataContentOffsets(var R: TRect);
    function GetShowEndEllipsis: Boolean; override;
    function GetStartTextSelectionInfo(out AParams: TcxViewParams; out ASelStart, ASelLength: Integer): Boolean;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    function IsValidTextEdit(AEdit: TcxCustomEdit): Boolean;
    function GetValue: Variant; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasCustomDraw: Boolean; override;
    function HasFocusRect: Boolean; virtual;
    function HasHiddenEditorOnScrollContent: Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure InitializeByHiddenEditor(AEditViewInfo: TcxCustomEditViewInfo); virtual;
    procedure InitTextSelection; virtual;
    function InvalidateOnStateChange: Boolean; override;
    function IsTextSelected: Boolean; virtual;
    procedure MouseLeave; override;
    function NeedHighlightFindFilterText: Boolean; virtual;
    function NeedLocalCopyOfEditViewData: Boolean; virtual;
    function NeedShowEditOnDblClick: Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;
    procedure RestoreParams(const AParams: TcxViewParams); override;
    procedure SaveParams(out AParams: TcxViewParams); override;
    procedure StateChanged(APrevState: TcxGridCellState); override;
    function SupportsZeroHeight: Boolean; virtual;

    function CanShowAutoHint: Boolean; virtual;
    function CanShowCustomHint: Boolean; virtual;
    function CanShowHint: Boolean; override;
    function GetCellBoundsForHint: TRect; override;
    function NeedShowHint(const AMousePos: TPoint; out AHintText: TCaption;
      out AIsHintMultiLine: Boolean; out ATextRect: TRect): Boolean; override;
    function UseStandardNeedShowHint: Boolean; virtual;

    procedure InitStyle; virtual;
    procedure ValidateDrawValue; virtual;

    function CreateEditViewInfo: TcxCustomEditViewInfo;

    procedure CreateEditViewData;
    procedure DestroyEditViewData;

    function SupportsEditing: Boolean; virtual;
    procedure UpdateEdit;

    // IcxEditOwner
    function EditOwnerGetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
    procedure EditOwnerInvalidate(const R: TRect; AEraseBackground: Boolean = True);
    function IcxEditOwner.GetViewData = EditOwnerGetViewData;
    procedure IcxEditOwner.Invalidate = EditOwnerInvalidate;

    property AutoHeight: Boolean read GetAutoHeight;
    property EditViewData: TcxCustomEditViewData read FEditViewData;
    property EditViewDataBounds: TRect read GetRealEditViewDataBounds;
    property MaxLineCount: Integer read GetMaxLineCount;
    property MousePos: TPoint read GetMousePos;
    property ShowButtons: Boolean read GetShowButtons;
    property WasFocusedBeforeClick: Boolean read FWasFocusedBeforeClick;
  public
    constructor Create(ARecordViewInfo: TcxCustomGridRecordViewInfo;
      AItem: TcxCustomGridTableItem); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CanDrawSelected: Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetInplaceEditPosition: TcxInplaceEditPosition;
    procedure Invalidate(ARecalculate: Boolean); reintroduce; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;
    function MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property DisplayValue: TcxEditValue read GetDisplayValue;
    property EditBounds: TRect read GetEditBounds;
    property Editing: Boolean read GetEditing;
    property EditViewInfo: TcxCustomEditViewInfo read FEditViewInfo;
    property Focused: Boolean read GetFocused;
    property Item: TcxCustomGridTableItem read FItem;
    property Properties: TcxCustomEditProperties read GetProperties;
    property Style: TcxEditStyle read FStyle;
    property Value: Variant read GetValue;
  end;

  TcxGridTableDataCellViewInfoClass = class of TcxGridTableDataCellViewInfo;

  // records

  TcxCustomGridRecordViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FExpandButtonBounds: TRect;
    FExpanded: Boolean;
    FExpandedCalculated: Boolean;
    FIsRecordViewInfoAssigned: Boolean;
    FRecordsViewInfo: TcxCustomGridRecordsViewInfo;
    FSelected: Boolean;
    FSelectedCalculated: Boolean;
    function GetCacheItem: TcxCustomGridTableViewInfoCacheItem;
    function GetExpanded: Boolean;
    function GetFocused: Boolean;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetGridViewInfo: TcxCustomGridTableViewInfo; inline;
    function GetIndex: Integer;
    function GetRealFocusRectBounds: TRect;
    function GetSelected: Boolean;
  protected
    FRecord: TcxCustomGridRecord;
    function AdjustToIntegralBottomBound(var ABound: Integer): Boolean; virtual;
    procedure CalculateExpandButtonBounds(var ABounds: TRect); virtual; abstract;
    function CalculateMultilineEditMinHeight: Integer; virtual;
    function CalculateSelected: Boolean; virtual;
    function CanGenerateExpandButtonHitTest: Boolean; virtual;
    function CanShowDataCellHint: Boolean; virtual;
    procedure ChildHotTrackChanged; virtual;
    procedure ControlFocusChanged; virtual;
    function GetAutoHeight: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBackgroundBounds: TRect; virtual;
    function GetBackgroundBitmapBounds: TRect; virtual;
    function GetCanvas: TcxCanvas; override;
    function GetCellTransparent(ACell: TcxGridTableCellViewInfo): Boolean; virtual;
    function GetContentBounds: TRect; virtual;
    function GetExpandButtonAreaBounds: TRect; virtual;
    function GetExpandButtonOwnerCellBounds: TRect; virtual;
    function GetFocusRectBounds: TRect; virtual;
    function GetFullyVisible: Boolean; virtual;
    function GetHeight: Integer; override;
    function GetHideFocusRectOnExit: Boolean; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPixelScrollSize: Integer; virtual;
    function GetRealExpandButtonAreaBounds: TRect; virtual;
    function GetVisible: Boolean; override;
    function HasFocusRect: Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function InvalidateOnChildHotTrackChanged: Boolean; virtual;
    function IsClickHitTest(AHitTest: TcxCustomGridHitTest): Boolean; virtual;
    function IsDetailVisible(ADetail: TcxCustomGridView): Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    property AutoHeight: Boolean read GetAutoHeight;
    property BackgroundBitmapBounds: TRect read GetBackgroundBitmapBounds;
    property CacheItem: TcxCustomGridTableViewInfoCacheItem read GetCacheItem;
    property ExpandButtonAreaBounds: TRect read GetRealExpandButtonAreaBounds;
    property Expanded: Boolean read GetExpanded;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
      ARecord: TcxCustomGridRecord); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    function Click(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; reintroduce; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetBoundsForInvalidate(AItem: TcxCustomGridTableItem): TRect; virtual;
    function GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect; virtual;
    function GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo; virtual;
    {procedure GetDataCellViewParams(AItem: TcxCustomGridTableItem;
      ACellViewInfo: TcxGridTableCellViewInfo; var AParams: TcxViewParams); virtual;}
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure MainCalculate(ALeftBound, ATopBound: Integer); virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    function ProcessDialogChar(ACharCode: Word): Boolean; virtual;
    procedure Recalculate;

    property ContentBounds: TRect read GetContentBounds;
    property ExpandButtonBounds: TRect read FExpandButtonBounds;
    property Focused: Boolean read GetFocused;
    property FocusRectBounds: TRect read GetRealFocusRectBounds;
    property FullyVisible: Boolean read GetFullyVisible;
    property GridView: TcxCustomGridTableView read GetGridView;
    property GridRecord: TcxCustomGridRecord read FRecord;
    property GridViewInfo: TcxCustomGridTableViewInfo read GetGridViewInfo;
    property Index: Integer read GetIndex;
    property HideFocusRectOnExit: Boolean read GetHideFocusRectOnExit;
    property RecordsViewInfo: TcxCustomGridRecordsViewInfo read FRecordsViewInfo;
    property Selected: Boolean read GetSelected;
  end;

  TcxCustomGridRecordsViewInfoClass = class of TcxCustomGridRecordsViewInfo;

  TcxCustomGridRecordsViewInfo = class
  private
    FBackgroundBitmap: TBitmap;
    FBounds: TRect;
    FContentBounds: TRect;
    FGridViewInfo: TcxCustomGridTableViewInfo;
    FIncSearchingCellViewInfo: TcxGridTableDataCellViewInfo;
    FIsIncSearchingCellViewInfoCalculated: Boolean;
    FIsRightToLeftConverted: Boolean;
    FRecordPixelScrollSizeCalculatingLockCount: Integer;
    FItems: TdxFastList;
    FItemsOffset: Integer;
    FPrevFocusedItemBounds: TRect;

    function GetCanvas: TcxCanvas; inline;
    function GetController: TcxCustomGridTableController; inline;
    function GetCount: Integer; inline;
    function GetFirstRecordIndex: Integer; inline;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetIncSearchingCellViewInfo: TcxGridTableDataCellViewInfo;
    function GetIsRecordPixelScrollSizeCalculating: Boolean;
    function GetItem(Index: Integer): TcxCustomGridRecordViewInfo;
    function GetMaxCount: Integer;
    function GetPixelScrollRecordOffset: Integer;
    function GetScaleFactor: TdxScaleFactor;
    function GetTopRecordIndex: Integer;
    function GetViewData: TcxCustomGridTableViewData; inline;

    procedure CreateItems;
    procedure DestroyItems;
    function CreateRecordViewInfo(AIndex: Integer): TcxCustomGridRecordViewInfo; overload;
  protected
    FPartVisibleCount: Integer;
    FVisibleCount: Integer;

    function AddRecordViewInfo(AViewInfo: TcxCustomGridRecordViewInfo): Integer;
    procedure AdjustEditorBoundsToIntegralHeight(var AEditorBounds: TRect);
    procedure AfterCalculate; virtual;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; virtual;
    procedure BeforeCalculate; virtual;
    procedure BeforeItemRecalculation; virtual;
    procedure AfterOffset; virtual;
    procedure BeforeOffset; virtual;
    procedure Calculate; virtual;
    function CalculateBounds: TRect; virtual;
    function CalculateContentBounds: TRect; virtual;
    function CalculateIncSearchingCellViewInfo: TcxGridTableDataCellViewInfo; virtual;
    procedure CalculatePixelScrollInfo(var ARecordIndex, ARecordOffset: Integer; AMaxRecordIndex, AMaxRecordOffset: Integer;
      ADelta: Integer; out AOverPan: Boolean); virtual;
    procedure CalculateVisibleCount; virtual;
    procedure Clear;
    procedure CreateEditViewDatas;
    function CreateRecordViewInfo(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo; overload;
    procedure DeleteItem(AIndex: Integer); virtual;
    procedure DestroyEditViewDatas;
    function GetAreaBoundsForCell(ACellViewInfo: TcxGridTableDataCellViewInfo): TRect; virtual;
    function GetAutoDataCellHeight: Boolean; virtual;
    function GetAutoDataRecordHeight: Boolean; virtual;
    function GetAutoRecordHeight: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; virtual;
    function GetPixelScrollContentSize: Integer; virtual;
    function GetItemLeftBound(AIndex: Integer): Integer; virtual; abstract;
    function GetItemPixelScrollSize(AItem: TcxCustomGridRecordViewInfo): Integer; virtual;
    function GetItemsOffset(AItemCountDelta: Integer): Integer; virtual; abstract;
    function GetItemTopBound(AIndex: Integer): Integer; virtual; abstract;
    function GetRecordIndex(AViewInfoIndex: Integer): Integer;
    function GetRecordScrollSize(ARecordIndex: Integer): Integer; virtual;
    function GetRecordViewInfo(ARecordIndex: Integer; out ANewlyCreated: Boolean): TcxCustomGridRecordViewInfo;
    function GetViewInfoIndexByRecordIndex(ARecordIndex: Integer): Integer; virtual;
    function IsEmpty: Boolean; virtual;
    procedure InsertItem(AIndex: Integer; AItem: Pointer); virtual;
    procedure OffsetItem(AIndex, AOffset: Integer); virtual; abstract;

    procedure ControlFocusChanged; virtual;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    function GetPainterClass: TcxCustomGridRecordsPainterClass; virtual;

    property Canvas: TcxCanvas read GetCanvas;
    property Controller: TcxCustomGridTableController read GetController;
    property FirstRecordIndex: Integer read GetFirstRecordIndex;
    property GridViewInfo: TcxCustomGridTableViewInfo read FGridViewInfo;
    property IncSearchingCellViewInfo: TcxGridTableDataCellViewInfo read GetIncSearchingCellViewInfo;
    property IsRecordPixelScrollSizeCalculating: Boolean read GetIsRecordPixelScrollSizeCalculating;
    property MaxCount: Integer read GetMaxCount;
    property PixelScrollRecordOffset: Integer read GetPixelScrollRecordOffset;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property TopRecordIndex: Integer read GetTopRecordIndex;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); virtual;
    destructor Destroy; override;
    function CanOffset(AItemCountDelta: Integer): Boolean; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetCellHeight(ACellContentHeight: Integer): Integer; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetRealItem(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo; virtual;
    function IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean; virtual;
    procedure MainCalculate; virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure OffsetRecords(AItemCountDelta, APixelScrollRecordOffsetDelta: Integer); virtual;
    procedure Paint;
    procedure RightToLeftConversion(const ABounds: TRect);

    property AutoDataCellHeight: Boolean read GetAutoDataCellHeight;
    property AutoDataRecordHeight: Boolean read GetAutoDataRecordHeight;
    property AutoRecordHeight: Boolean read GetAutoRecordHeight;
    property BackgroundBitmap: TBitmap read FBackgroundBitmap write FBackgroundBitmap;
    property Bounds: TRect read FBounds;
    property ContentBounds: TRect read FContentBounds;
    property Count: Integer read GetCount;
    property GridView: TcxCustomGridTableView read GetGridView;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Items[Index: Integer]: TcxCustomGridRecordViewInfo read GetItem; default;
    property ItemsOffset: Integer read FItemsOffset;
    property PartVisibleCount: Integer read FPartVisibleCount;
    property PrevFocusedItemBounds: TRect read FPrevFocusedItemBounds write FPrevFocusedItemBounds;
    property VisibleCount: Integer read FVisibleCount;
  end;

  // view

  TcxGridCustomTableNavigatorViewInfo = class(TcxInplaceNavigatorViewInfo)
  private
    FGridViewInfo: TcxCustomGridTableViewInfo;
  protected
    function GetControl: TcxControl; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetNavigatorOffset: Integer; override;
    procedure UpdateNavigatorSiteBounds(const ABounds: TRect); override;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); reintroduce; virtual;
  end;
  TcxGridCustomTableNavigatorViewInfoClass = class of TcxGridCustomTableNavigatorViewInfo;

  TcxNavigatorSiteViewInfoClass = class of TcxNavigatorSiteViewInfo;

  TcxNavigatorSiteViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCalculatedHeight: Integer;
    FNavigatorViewInfo: TcxGridCustomTableNavigatorViewInfo;
    function GetGridViewInfo: TcxCustomGridTableViewInfo; inline;
    function GetNavigator: TcxGridViewNavigator; inline;
  protected
    function CalculateHeight: Integer; override;
    procedure CalculateNavigator;
    function CalculateWidth: Integer; override;
    procedure GetAdornerTargetElements(AList: TStrings); override;
    function GetHeight: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetNavigatorBounds: TRect;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure MouseLeave; override;
    procedure NavigatorStateChanged;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    function IsNavigatorSizeChanged: Boolean;
    procedure ResetCalculatedHeight;

    property Navigator: TcxGridViewNavigator read GetNavigator;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;
    function MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property GridViewInfo: TcxCustomGridTableViewInfo read GetGridViewInfo;
    property NavigatorViewInfo: TcxGridCustomTableNavigatorViewInfo read FNavigatorViewInfo;
  end;

  TcxCustomGridTableViewInfo = class(TcxCustomGridViewInfo,
    IcxNavigatorOwner,
    IcxNavigatorOwner2)
  private
    FCalculateDown: Boolean;
    FFilterViewInfo: TcxGridFilterViewInfo;
    FFindPanelViewInfo: TcxGridFindPanelViewInfo;
    FFirstRecordIndex: Integer;
    FNavigatorSiteViewInfo: TcxNavigatorSiteViewInfo;
    FParts: TdxFastList;
    FPixelScrollRecordOffset: Integer;
    FRecordsViewInfo: TcxCustomGridRecordsViewInfo;

    function GetController: TcxCustomGridTableController; inline;
    function GetExpandButtonSize: Integer;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetNavigatorViewInfo: TcxNavigatorViewInfo; inline;
    function GetPart(Index: Integer): TcxCustomGridPartViewInfo; inline;
    function GetPartCount: Integer; inline;
    function GetPartsBottomHeight: Integer;
    function GetPartsCustomHeight(AAlignment: TcxGridPartAlignment): Integer;
    function GetPartsTopHeight: Integer;
    function GetScrollableAreaWidth: Integer;
    function GetViewData: TcxCustomGridTableViewData; inline;
    function GetVisibleRecordCount: Integer; inline;

    procedure AddPart(AItem: TcxCustomGridPartViewInfo);
    procedure RemovePart(AItem: TcxCustomGridPartViewInfo);
  protected
    // IcxNavigatorOwner
    function GetNavigatorBounds: TRect; virtual;
    function GetNavigatorButtons: TcxCustomNavigatorButtons;
    function GetNavigatorCanvas: TCanvas;
    function GetNavigatorControl: TWinControl;
    function GetNavigatorFocused: Boolean;
    function GetNavigatorLookAndFeel: TcxLookAndFeel;
    function GetNavigatorOwner: TComponent;
    function GetNavigatorShowHint: Boolean;
    function GetNavigatorTabStop: Boolean;
    procedure NavigatorStateChanged;
    procedure NavigatorChanged(AChangeType: TcxNavigatorChangeType);
    procedure RefreshNavigator;
    // IcxNavigatorOwner2
    function GetNavigatorInfoPanel: TcxCustomNavigatorInfoPanel;

    procedure CreateViewInfos; override;
    procedure DestroyViewInfos(AIsRecreating: Boolean); override;

    function CreateFindPanelViewInfo: TcxGridFindPanelViewInfo; virtual;

    procedure AfterCalculating; override;
    procedure AfterOffset; virtual;
    procedure BeforeCalculating; override;
    procedure BeforeOffset; virtual;
    procedure Calculate; override;
    function CalculateClientBounds: TRect; override;
    function GetEqualHeightRecordScrollSize: Integer; virtual;
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer;
      var AFullyVisible: Boolean); override;
    procedure CalculateNavigator;
    function CalculatePartBounds(APart: TcxCustomGridPartViewInfo): TRect; virtual;
    function CalculateVisibleEqualHeightRecordCount: Integer; virtual;
    procedure ControlFocusChanged; override;
    function DoGetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetBottomNonClientHeight: Integer; override;
    function GetContentBounds: TRect; override;
    function GetDefaultGridModeBufferCount: Integer; virtual;
    function GetFirstRecordIndex: Integer; virtual;
    procedure GetHScrollBarBounds(var ABounds: TRect); override;
    function GetFilterViewInfoClass: TcxGridFilterViewInfoClass; virtual;
    function GetIsInternalUse: Boolean; override;
    function GetMultilineEditorBounds(const ACellEditBounds: TRect; ACalculatedHeight: Integer;
      AAutoHeight: TcxInplaceEditAutoHeight): TRect; virtual;
    function GetNavigatorOffset: Integer; virtual;
    function GetNavigatorSiteViewInfoClass: TcxNavigatorSiteViewInfoClass; virtual;
    function GetNavigatorViewInfoClass: TcxGridCustomTableNavigatorViewInfoClass; virtual;
    function GetNoDataInfoText: string; virtual;
    function GetNoDataInfoTextAreaBounds: TRect; virtual;
    procedure GetNoDataInfoTextParams(out AParams: TcxViewParams); virtual;
    function GetNoDataInfoTextAreaVisible: Boolean; virtual;
    function GetNonRecordsAreaHeight(ACheckScrollBar: Boolean): Integer; virtual;
    function GetPixelScrollRecordOffset: Integer; virtual;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; virtual; abstract;
    function GetScrollableAreaBounds: TRect; virtual;
    function GetScrollableAreaBoundsForEdit: TRect; virtual;
    function GetScrollableAreaBoundsHorz: TRect; virtual;
    function GetScrollableAreaBoundsVert: TRect; virtual;
    procedure GetVScrollBarBounds(var ABounds: TRect); override;
    function IsFirstRecordIndexAssigned: Boolean;
    function IsPixelScrollRecordOffsetAssigned: Boolean;
    procedure Offset(DX, DY: Integer); virtual;
    procedure OffsetRecords(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer); virtual;
    procedure VisibilityChanged(AVisible: Boolean); override;

    property Controller: TcxCustomGridTableController read GetController;
    property PartCount: Integer read GetPartCount;
    property Parts[Index: Integer]: TcxCustomGridPartViewInfo read GetPart;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    function CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer): Boolean; virtual;
    function CanOffsetView(ARecordCountDelta: Integer): Boolean; virtual;
    procedure DoOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;

    // for extended lookup edit
    function GetNearestPopupHeight(AHeight: Integer; AAdditionalRecord: Boolean = False): Integer; virtual;
    function GetPopupHeight(ADropDownRecordCount: Integer): Integer; virtual;

    property CalculateDown: Boolean read FCalculateDown write FCalculateDown;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
    property FilterViewInfo: TcxGridFilterViewInfo read FFilterViewInfo;
    property FindPanelViewInfo: TcxGridFindPanelViewInfo read FFindPanelViewInfo;
    property FirstRecordIndex: Integer read GetFirstRecordIndex write FFirstRecordIndex;
    property GridView: TcxCustomGridTableView read GetGridView;
    property NavigatorBounds: TRect read GetNavigatorBounds;
    property NavigatorOffset: Integer read GetNavigatorOffset;
    property NavigatorSiteViewInfo: TcxNavigatorSiteViewInfo read FNavigatorSiteViewInfo;
    property NavigatorViewInfo: TcxNavigatorViewInfo read GetNavigatorViewInfo;
    property NoDataInfoText: string read GetNoDataInfoText;
    property NoDataInfoTextAreaBounds: TRect read GetNoDataInfoTextAreaBounds;
    property NoDataInfoTextAreaVisible: Boolean read GetNoDataInfoTextAreaVisible;
    property PartsBottomHeight: Integer read GetPartsBottomHeight;
    property PartsTopHeight: Integer read GetPartsTopHeight;
    property PixelScrollRecordOffset: Integer read GetPixelScrollRecordOffset write FPixelScrollRecordOffset;
    property RecordsViewInfo: TcxCustomGridRecordsViewInfo read FRecordsViewInfo;
    property ScrollableAreaBounds: TRect read GetScrollableAreaBounds;
    property ScrollableAreaBoundsForEdit: TRect read GetScrollableAreaBoundsForEdit;
    property ScrollableAreaBoundsHorz: TRect read GetScrollableAreaBoundsHorz;
    property ScrollableAreaBoundsVert: TRect read GetScrollableAreaBoundsVert;
    property ScrollableAreaWidth: Integer read GetScrollableAreaWidth;
    property VisibleRecordCount: Integer read GetVisibleRecordCount;
  end;

  // cache

  TcxCustomGridTableViewInfoCacheItem = class(TcxCustomGridViewInfoCacheItem)
  private
    FHeight: Integer;
    FIsHeightAssigned: Boolean;
    function GetGridRecord: TcxCustomGridRecord;
    procedure SetHeight(Value: Integer);
  protected
    property GridRecord: TcxCustomGridRecord read GetGridRecord;
  public
    procedure UnassignValues(AKeepMaster: Boolean); override;
    property Height: Integer read FHeight write SetHeight;
    property IsHeightAssigned: Boolean read FIsHeightAssigned write FIsHeightAssigned;
  end;

  TcxCustomGridTableViewInfoCache = class(TcxCustomGridViewInfoCache)
  private
    function GetViewData: TcxCustomGridTableViewData;
  protected
    function GetItemClass(Index: Integer): TcxCustomGridViewInfoCacheItemClass; override;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
  end;

  { view }

  // custom item

  TcxGridTableItemChange = (ticProperty, ticLayout, ticSize);

  TcxCustomGridTableItemCustomOptions = class(TcxInterfacedPersistent)
  private
    function GetGridView: TcxCustomGridTableView; inline;
    function GetItem: TcxCustomGridTableItem;
  protected
    procedure Changed(AChange: TcxGridTableItemChange = ticLayout); virtual;
  public
    constructor Create(AItem: TcxCustomGridTableItem); reintroduce; virtual;

    procedure Assign(Source: TPersistent); override;

    property GridView: TcxCustomGridTableView read GetGridView;
    property Item: TcxCustomGridTableItem read GetItem;
  end;

  TcxGridItemShowEditButtons = (isebDefault, isebNever, isebAlways);
  TcxGridItemSortByDisplayText = (isbtDefault, isbtOn, isbtOff);

  TcxCustomGridTableItemOptionsClass = class of TcxCustomGridTableItemOptions;

  TcxCustomGridTableItemOptions = class(TcxCustomGridTableItemCustomOptions,
    IdxFilterPopupWindowOptions,
    IdxExcelFilterPopupWindowOptions)
  private
    FEditAutoHeight: TcxItemInplaceEditAutoHeight;
    FEditing: Boolean;
    FFiltering: Boolean;
    FFilteringAddValueItems: Boolean;
    FFilteringExcelPopupApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
    FFilteringExcelPopupDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
    FFilteringExcelPopupDefaultPage: TdxExcelFilterValueContainerDefaultPage;
    FFilteringExcelPopupNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;
    FFilteringFilteredItemsList: Boolean;
    FFilteringMRUItemsList: Boolean;
    FFilteringPopup: Boolean;
    FFilteringPopupIncrementalFiltering: Boolean;
    FFilteringPopupIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
    FFilteringPopupMode: TdxFilterPopupWindowMode;
    FFilteringPopupMultiSelect: Boolean;
    FFilteringWithFindPanel: Boolean;
    FFocusing: Boolean;
    FGrouping: Boolean;
    FIgnoreTimeForFiltering: Boolean;
    FIncSearch: Boolean;
    FMoving: Boolean;
    FShowCaption: Boolean;
    FShowEditButtons: TcxGridItemShowEditButtons;
    FSortByDisplayText: TcxGridItemSortByDisplayText;
    FSorting: Boolean;

    procedure SetEditAutoHeight(Value: TcxItemInplaceEditAutoHeight);
    procedure SetEditing(Value: Boolean);
    procedure SetFiltering(Value: Boolean);
    procedure SetFilteringAddValueItems(Value: Boolean);
    procedure SetFilteringExcelPopupApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
    procedure SetFilteringExcelPopupDateTimeValuesPageType(const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
    procedure SetFilteringExcelPopupDefaultPage(const AValue: TdxExcelFilterValueContainerDefaultPage);
    procedure SetFilteringExcelPopupNumericValuesPageType(const AValue: TdxExcelFilterValueContainerNumericValuesPageType);
    procedure SetFilteringFilteredItemsList(Value: Boolean);
    procedure SetFilteringMRUItemsList(Value: Boolean);
    procedure SetFilteringPopup(Value: Boolean);
    procedure SetFilteringPopupMode(const AValue: TdxFilterPopupWindowMode);
    procedure SetFilteringPopupMultiSelect(Value: Boolean);
    procedure SetFilteringWithFindPanel(AValue: Boolean);
    procedure SetFocusing(Value: Boolean);
    procedure SetGrouping(Value: Boolean);
    procedure SetIgnoreTimeForFiltering(Value: Boolean);
    procedure SetIncSearch(Value: Boolean);
    procedure SetMoving(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetShowEditButtons(Value: TcxGridItemShowEditButtons);
    procedure SetSortByDisplayText(Value: TcxGridItemSortByDisplayText);
    procedure SetSorting(Value: Boolean);
  protected
    // IdxFilterPopupWindowOptions
    function IdxFilterPopupWindowOptions.GetApplyMode = GetFilterPopupWindowApplyMode;
    function IdxFilterPopupWindowOptions.GetButtonCaption = GetFilterPopupWindowButtonCaption;
    function IdxFilterPopupWindowOptions.GetIncrementalFiltering = GetFilterPopupWindowIncrementalFiltering;
    function IdxFilterPopupWindowOptions.GetIncrementalFilteringOptions = GetFilterPopupWindowIncrementalFilteringOptions;
    function IdxFilterPopupWindowOptions.GetVisibleItemCount = GetFilterPopupWindowVisibleItemCount;
    function IdxFilterPopupWindowOptions.GetShowCheckBoxes = GetFilterPopupWindowShowCheckBoxes;
    function IdxFilterPopupWindowOptions.GetWidth = GetFilterPopupWindowWidth;
    function GetFilterPopupWindowApplyMode: TdxFilterApplyChangesMode;
    function GetFilterPopupWindowButtonCaption: string;
    function GetFilterPopupWindowIncrementalFiltering: Boolean;
    function GetFilterPopupWindowIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
    function GetFilterPopupWindowVisibleItemCount: Integer;
    function GetFilterPopupWindowShowCheckBoxes: Boolean;
    function GetFilterPopupWindowWidth: Integer;
    // IdxExcelFilterPopupWindowOptions
    function IdxExcelFilterPopupWindowOptions.GetDateTimeValuesPageType = GetExcelFilterPopupDateTimeValuesPageType;
    function IdxExcelFilterPopupWindowOptions.GetDefaultPage = GetExcelFilterPopupDefaultPage;
    function IdxExcelFilterPopupWindowOptions.GetApplyChanges = GetExcelFilterPopupApplyChanges;
    function IdxExcelFilterPopupWindowOptions.GetNumericValuesPageType = GetExcelFilterPopupNumericValuesPageType;
    function GetExcelFilterPopupApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
    function GetExcelFilterPopupDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
    function GetExcelFilterPopupDefaultPage: TdxExcelFilterValueContainerDefaultPage;
    function GetExcelFilterPopupNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;

    procedure BeforeShowCaptionChange; virtual;

    property EditAutoHeight: TcxItemInplaceEditAutoHeight read FEditAutoHeight write SetEditAutoHeight default ieahDefault;
    property Grouping: Boolean read FGrouping write SetGrouping default True;
    property Moving: Boolean read FMoving write SetMoving default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property SortByDisplayText: TcxGridItemSortByDisplayText read FSortByDisplayText
      write SetSortByDisplayText default isbtDefault;
    property Sorting: Boolean read FSorting write SetSorting default True;
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;

    procedure Assign(Source: TPersistent); override;

    property FilteringAddValueItems: Boolean read FFilteringAddValueItems write SetFilteringAddValueItems default True;
    property FilteringExcelPopupApplyChanges: TdxExcelFilterValueContainerApplyChangesMode read FFilteringExcelPopupApplyChanges
      write SetFilteringExcelPopupApplyChanges default efacDefault;
    property FilteringExcelPopupDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType
      read FFilteringExcelPopupDateTimeValuesPageType write SetFilteringExcelPopupDateTimeValuesPageType default dvptDefault;
    property FilteringExcelPopupDefaultPage: TdxExcelFilterValueContainerDefaultPage read FFilteringExcelPopupDefaultPage
      write SetFilteringExcelPopupDefaultPage default dpDefault;
    property FilteringExcelPopupNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType
      read FFilteringExcelPopupNumericValuesPageType write SetFilteringExcelPopupNumericValuesPageType default nvptDefault;
    property FilteringFilteredItemsList: Boolean read FFilteringFilteredItemsList write SetFilteringFilteredItemsList default True;
    property FilteringMRUItemsList: Boolean read FFilteringMRUItemsList write SetFilteringMRUItemsList default True;
    property FilteringPopup: Boolean read FFilteringPopup write SetFilteringPopup default True;
    property FilteringPopupIncrementalFiltering: Boolean read FFilteringPopupIncrementalFiltering write FFilteringPopupIncrementalFiltering default False;
    property FilteringPopupIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions
      read FFilteringPopupIncrementalFilteringOptions write FFilteringPopupIncrementalFilteringOptions
      default [ifoHighlightSearchText, ifoUseContainsOperator];
    property FilteringPopupMode: TdxFilterPopupWindowMode read FFilteringPopupMode write SetFilteringPopupMode default fpmDefault;
    property FilteringPopupMultiSelect: Boolean read FFilteringPopupMultiSelect write SetFilteringPopupMultiSelect default True;
    property ShowEditButtons: TcxGridItemShowEditButtons read FShowEditButtons write SetShowEditButtons default isebDefault;
  published
    property Editing: Boolean read FEditing write SetEditing default True;
    property Filtering: Boolean read FFiltering write SetFiltering default True;
    property FilteringWithFindPanel: Boolean read FFilteringWithFindPanel write SetFilteringWithFindPanel default True;
    property Focusing: Boolean read FFocusing write SetFocusing default True;
    property IgnoreTimeForFiltering: Boolean read FIgnoreTimeForFiltering write SetIgnoreTimeForFiltering default True;
    property IncSearch: Boolean read FIncSearch write SetIncSearch default True;
  end;

  TcxGridGetCellStyleEvent = procedure(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
    AItem: TcxCustomGridTableItem; {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AStyle: TcxStyle) of object;

  TcxGridGetRecordStyleEvent = procedure(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
    {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AStyle: TcxStyle) of object;

  TcxCustomGridTableItemStylesClass = class of TcxCustomGridTableItemStyles;

  TcxCustomGridTableItemStyles = class(TcxCustomGridStyles)
  private
    FOnGetContentStyle: TcxGridGetCellStyleEvent;
    function GetGridViewValue: TcxCustomGridTableView; inline;
    function GetItem: TcxCustomGridTableItem; inline;
    procedure SetOnGetContentStyle(Value: TcxGridGetCellStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetGridView: TcxCustomGridView; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetContentParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    property GridView: TcxCustomGridTableView read GetGridViewValue;
    property Item: TcxCustomGridTableItem read GetItem;
  published
    property Content: TcxStyle index isContent read GetValue write SetValue;
    property OnGetContentStyle: TcxGridGetCellStyleEvent read FOnGetContentStyle write SetOnGetContentStyle;
  end;

  TcxGridSortOrder = TcxDataSortOrder;

  TcxGridTableDataCellCustomDrawEvent = procedure(Sender: TcxCustomGridTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean) of object;

  TcxGridGetCellHintEvent = procedure(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
    ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
    var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect) of object;
  TcxGridGetDataTextEvent = procedure(Sender: TcxCustomGridTableItem;
    ARecordIndex: Integer; var AText: string) of object;
  TcxGridGetDisplayTextEvent = procedure(Sender: TcxCustomGridTableItem;
    ARecord: TcxCustomGridRecord; var AText: string) of object;
  TcxGridGetFilterDisplayTextEvent = procedure(Sender: TcxCustomGridTableItem;
    const AValue: Variant; var ADisplayText: string) of object;
  TcxGridGetFilterValuesEvent = procedure(Sender: TcxCustomGridTableItem;
    AValueList: TcxDataFilterValueList) of object;
  TcxGridGetPropertiesEvent = procedure(Sender: TcxCustomGridTableItem;
    ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties) of object;
  TcxGridInitDateRangesEvent = procedure(Sender: TcxCustomGridTableItem; ADateRanges: TcxGridDateRanges) of object;
  TcxGridTableItemGetStoredPropertiesEvent = procedure(Sender: TcxCustomGridTableItem;
    AProperties: TStrings) of object;
  TcxGridTableItemGetStoredPropertyValueEvent = procedure(Sender: TcxCustomGridTableItem;
    const AName: string; var AValue: Variant) of object;
  TcxGridTableItemSetStoredPropertyValueEvent = procedure(Sender: TcxCustomGridTableItem;
    const AName: string; const AValue: Variant) of object;
  TcxGridUserFilteringEvent = procedure(Sender: TcxCustomGridTableItem;
    const AValue: Variant; const ADisplayText: string) of object;
  TcxGridUserFilteringExEvent = procedure(Sender: TcxCustomGridTableItem;
    AFilterList: TcxFilterCriteriaItemList; const AValue: Variant; const ADisplayText: string) of object;

  TcxGridValidateDrawValueEvent = procedure(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: TcxEditValue; AData: TcxEditValidateInfo) of object;

  TcxCustomGridTableItemClass = class of TcxCustomGridTableItem;

  TcxCustomGridTableItem = class(TcxComponent,
    IcxEditRepositoryItemListener,
    IcxStoredObject,
    IdxScaleFactor,
    IdxFilterableComponent,
    IdxExcelFilterableComponent,
    IdxDateTimeHandling,
    IdxFilteringDateTimeHandling,
    IdxGroupingDateTimeHandling)
  private
    FAlternateCaption: string;
    FBestFitMaxWidth: Integer;
    FCaption: string;
    FCells: TdxFastList;
    FCellStyle: TcxEditStyle;
    FCellStyleUseCounter: Integer;
    FDataBinding: TcxGridItemDataBinding;
    FDateTimeGrouping: TcxGridDateTimeGrouping;
    FEditData: TcxCustomEditData;
    FEditViewData: TcxCustomEditViewData;
    FFilteringDateRanges: TcxGridFilteringDateRanges;
    FGridView: TcxCustomGridTableView;
    FGroupingDateRanges: TcxGridGroupingDateRanges;
    FHeaderAlignmentHorz: TAlignment;
    FHeaderAlignmentVert: TcxAlignmentVert;
    FHeaderHint: string;
    FID: Integer;
    FIgnoreLoadingStatus: Boolean;
    FIndex: Integer;
    FIsCaptionAssigned: Boolean;
    FIsHeaderAlignmentHorzAssigned: Boolean;
    FIsHeaderAlignmentVertAssigned: Boolean;
    FIsWidthAssigned: Boolean;
    FLastEditingProperties: TcxCustomEditProperties;
    FLastUsedDefaultRepositoryItem: TcxEditRepositoryItem;
    FMinWidth: Integer;
    FOptions: TcxCustomGridTableItemOptions;
    FProperties: TcxCustomEditProperties;
    FPropertiesClass: TcxCustomEditPropertiesClass;
    FPropertiesValue: TcxCustomEditProperties;
    FRepositoryItem: TcxEditRepositoryItem;
    FSavedVisible: Boolean;
    FStyles: TcxCustomGridTableItemStyles;
    FVisible: Boolean;
    FVisibleForCustomization: Boolean;
    FVisibleIndex: Integer;
    FWasVisibleBeforeGrouping: Boolean;
    FWidth: Integer;

    FOnCustomDrawCell: TcxGridTableDataCellCustomDrawEvent;
    FOnGetCellHint: TcxGridGetCellHintEvent;
    FOnGetDataText: TcxGridGetDataTextEvent;
    FOnGetDisplayText: TcxGridGetDisplayTextEvent;
    FOnGetFilterDisplayText: TcxGridGetFilterDisplayTextEvent;
    FOnGetFilterValues: TcxGridGetFilterValuesEvent;
    FOnGetProperties: TcxGridGetPropertiesEvent;
    FOnGetPropertiesForEdit: TcxGridGetPropertiesEvent;
    FOnGetStoredProperties: TcxGridTableItemGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGridTableItemGetStoredPropertyValueEvent;
    FOnInitFilteringDateRanges: TcxGridInitDateRangesEvent;
    FOnInitGroupingDateRanges: TcxGridInitDateRangesEvent;
    FOnSetStoredPropertyValue: TcxGridTableItemSetStoredPropertyValueEvent;
    FOnUserFiltering: TcxGridUserFilteringEvent;
    FOnUserFilteringEx: TcxGridUserFilteringExEvent;
    FOnValidateDrawValue: TcxGridValidateDrawValueEvent;
    FSubClassEvents: TNotifyEvent;

    function GetActualMinWidth: Integer;
    function GetCaption: string;
    function GetCell(Index: Integer): TcxGridTableDataCellViewInfo;
    function GetCellCount: Integer;
    function GetController: TcxCustomGridTableController; inline;
    function GetDataController: TcxCustomDataController; inline;
    function GetEditing: Boolean;
    function GetEditingProperties: TcxCustomEditProperties;
    function GetFilterCaption: string;
    function GetFiltered: Boolean;
    function GetFocused: Boolean;
    function GetGroupIndex: Integer;
    function GetHeaderAlignmentHorz: TAlignment;
    function GetHeaderAlignmentVert: TcxAlignmentVert;
    function GetHidden: Boolean;
    function GetIsLoading: Boolean;
    function GetIncSearching: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
    function GetIsReading: Boolean;
    function GetIsUpdating: Boolean;
    function GetMinWidth: Integer;
    function GetPropertiesClassName: string;
    function GetSortIndex: Integer;
    function GetSortOrder: TcxGridSortOrder;
    function GetTag: TcxTag;
    function GetViewData: TcxCustomGridTableViewData; inline;
    function GetWidth: Integer;
    procedure SetAlternateCaption(const Value: string);
    procedure SetBestFitMaxWidth(Value: Integer);
    procedure SetCaption(const Value: string);
    procedure SetDataBinding(Value: TcxGridItemDataBinding);
    procedure SetDateTimeGrouping(Value: TcxGridDateTimeGrouping);
    procedure SetEditing(Value: Boolean);
    procedure SetFiltered(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetHeaderAlignmentHorz(Value: TAlignment);
    procedure SetHeaderAlignmentVert(Value: TcxAlignmentVert);
    procedure SetHeaderHint(const Value: string);
    procedure SetHidden(Value: Boolean);
    procedure SetIndex(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetOnCustomDrawCell(Value: TcxGridTableDataCellCustomDrawEvent);
    procedure SetOnGetCellHint(Value: TcxGridGetCellHintEvent);
    procedure SetOnGetDataText(Value: TcxGridGetDataTextEvent);
    procedure SetOnGetDisplayText(Value: TcxGridGetDisplayTextEvent);
    procedure SetOnGetFilterDisplayText(Value: TcxGridGetFilterDisplayTextEvent);
    procedure SetOnGetFilterValues(Value: TcxGridGetFilterValuesEvent);
    procedure SetOnGetProperties(Value: TcxGridGetPropertiesEvent);
    procedure SetOnGetPropertiesForEdit(Value: TcxGridGetPropertiesEvent);
    procedure SetOnGetStoredProperties(Value: TcxGridTableItemGetStoredPropertiesEvent);
    procedure SetOnGetStoredPropertyValue(Value: TcxGridTableItemGetStoredPropertyValueEvent);
    procedure SetOnInitFilteringDateRanges(Value: TcxGridInitDateRangesEvent);
    procedure SetOnInitGroupingDateRanges(Value: TcxGridInitDateRangesEvent);
    procedure SetOnSetStoredPropertyValue(Value: TcxGridTableItemSetStoredPropertyValueEvent);
    procedure SetOnUserFiltering(Value: TcxGridUserFilteringEvent);
    procedure SetOnUserFilteringEx(Value: TcxGridUserFilteringExEvent);
    procedure SetOnValidateDrawValue(Value: TcxGridValidateDrawValueEvent);

    procedure SetOptions(Value: TcxCustomGridTableItemOptions);
    procedure SetProperties(Value: TcxCustomEditProperties);
    procedure SetPropertiesClass(Value: TcxCustomEditPropertiesClass);
    procedure SetPropertiesClassName(const Value: string);
    procedure SetRepositoryItem(Value: TcxEditRepositoryItem);
    procedure SetSortIndex(Value: Integer);
    procedure SetSortOrder(Value: TcxGridSortOrder);
    procedure SetStyles(Value: TcxCustomGridTableItemStyles);
    procedure SetTag(Value: TcxTag);
    procedure SetVisible(Value: Boolean);
    procedure SetVisibleForCustomization(Value: Boolean);
    procedure SetWidth(Value: Integer);

    procedure ReadHidden(Reader: TReader);
    procedure ReadIsCaptionAssigned(Reader: TReader);
    procedure WriteIsCaptionAssigned(Writer: TWriter);

    function IsCaptionStored: Boolean;
    function IsSortOrderStored: Boolean;
    function IsTagStored: Boolean;
    function IsWidthStored: Boolean;

    function GetDataBindingClass: TcxGridItemDataBindingClass;

    procedure CreateProperties;
    procedure DestroyProperties;
    procedure RecreateProperties;

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetParentComponent(AParent: TComponent); override;

    // IcxEditRepositoryItemListener
    procedure ItemRemoved(Sender: TcxEditRepositoryItem);
    procedure IcxEditRepositoryItemListener.PropertiesChanged = RepositoryItemPropertiesChanged;
    procedure RepositoryItemPropertiesChanged(Sender: TcxEditRepositoryItem);
    // IcxStoredObject
    function GetObjectName: string; virtual;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IdxScaleFactor
    procedure ChangeScale(M, D: Integer); virtual;
    function GetScaleFactor: TdxScaleFactor;
    //IdxFilterableComponent
    procedure FilteringApplied;
    function GetFilter: TcxFilterCriteria;
    procedure GetFilterActiveValueIndexes(AValues: TcxFilterValueList; var AIndexes: TdxIntegerIndexes);
    function GetFilterSelectedValueIndex(AValues: TcxFilterValueList): Integer;
    function GetFilterValuesClass: TcxFilterValueListClass;
    procedure PopulateFilterValues(AValues: TcxFilterValueList; ADisplayValues: TStrings;
      AValuesOnly: Boolean; AUniqueOnly: Boolean);
    procedure SetFilterActiveValueIndex(AValues: TcxFilterValueList; AIndex: Integer; AFilterList: TcxFilterCriteriaItemList;
      AReplaceExistent, AAddToMRUItemsList: Boolean);
    procedure SetFilterActiveValueIndexes(AValues: TcxFilterValueList; const AIndexes: TdxIntegerIndexes;
      AAddToMRUItemsList: Boolean = False);
    //IdxExcelFilterableComponent
    function GetFilterItemLink: TObject;
    function GetFilterProperties: TcxCustomEditProperties;
    function GetFilterValueTypeClass: TcxValueTypeClass;
    procedure SetFilterActiveValue(AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; const ADisplayText: string);
    procedure SetFilterActiveValues(AValueInfos: TdxFastObjectList; ABoolOperator: TcxFilterBoolOperatorKind);
    //IdxDateTimeHandling
    function IdxDateTimeHandling.GetDateFormat = GetDateTimeHandlingDateFormat;
    function IdxDateTimeHandling.GetHourFormat = GetDateTimeHandlingHourFormat;
    function IdxDateTimeHandling.GetMonthFormat = GetDateTimeHandlingMonthFormat;
    function IdxDateTimeHandling.GetYearFormat = GetDateTimeHandlingYearFormat;
    function GetDateTimeHandlingDateFormat: string;
    function GetDateTimeHandlingHourFormat: string;
    function GetDateTimeHandlingMonthFormat: string;
    function GetDateTimeHandlingYearFormat: string;
    //IdxFilteringDateTimeHandling
    function IdxFilteringDateTimeHandling.GetFilters = GetDateTimeHandlingFilters;
    function GetDateTimeHandlingFilters: TdxDateTimeFilters;
    //IdxGroupingDateTimeHandling
    function IdxGroupingDateTimeHandling.GetGrouping = GetDateTimeHandlingGrouping;
    function GetDateTimeHandlingGrouping: TdxDateTimeGrouping;

    procedure CreateDataBinding; virtual;
    procedure DestroyDataBinding; virtual;

    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    function GetFilteringDateRangesClass: TcxGridFilteringDateRangesClass; virtual;
    function GetGroupingDateRangesClass: TcxGridGroupingDateRangesClass; virtual;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; virtual;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; virtual;

    function IsHeaderAlignmentHorzStored: Boolean;
    function IsHeaderAlignmentVertStored: Boolean;

    procedure BestFitApplied(AFireEvents: Boolean); virtual;
    function CalculateBestFitWidth: Integer; virtual;
    function CanAutoHeight: Boolean; virtual;
    function CanEdit: Boolean; virtual;
    function CanEditAutoHeight: Boolean; virtual;
    function CanFilter(AVisually: Boolean): Boolean; virtual;
    function CanFilterUsingChecks: Boolean; virtual;
    function CanFilterMRUValueItems: Boolean; virtual;
    function CanSearch: Boolean; virtual;
    function CanFocus(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function CanFocusInplaceEditFormItem(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function CanGroup: Boolean; virtual;
    function CanHide: Boolean; virtual;
    function CanHorzSize: Boolean; virtual;
    function CanIgnoreTimeForFiltering: Boolean; virtual;
    function CanIncSearch: Boolean; virtual;
    function CanInitEditing: Boolean; virtual;
    function CanMove: Boolean; virtual;
    function CanScroll: Boolean; virtual;
    function CanSort: Boolean; virtual;
    procedure CaptionChanged; virtual;
    procedure Changed(AChange: TcxGridTableItemChange); virtual;
    procedure ChangeGroupIndex(Value: Integer); virtual;
    procedure ChangeGrouping(AGroupIndex: Integer; AMergeWithLeftColumn: Boolean = False; AMergeWithRightColumn: Boolean = False);
    procedure ChangeSortIndex(Value: Integer);
    procedure CheckUsingInFindFiltering;
    procedure CheckWidthValue(var Value: Integer);
    procedure DataChanged; virtual;
    procedure DateTimeGroupingChanged; virtual;
    procedure DoSetVisible(Value: Boolean); virtual;
    procedure ForceWidth(Value: Integer); virtual;
    function GetActuallyVisible: Boolean; virtual;
    procedure GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer); virtual;
    function GetBestFitWidth: Integer; virtual;
    function GetEditable: Boolean; virtual;
    function GetEditAutoHeight: TcxInplaceEditAutoHeight; virtual;
    function GetEditPartVisible: Boolean; virtual;
    function GetEditValue: Variant; virtual;
    function GetFilterable: Boolean; virtual;
    procedure GetFilterDisplayText(const AValue: Variant; var ADisplayText: string); virtual;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; virtual;
    function GetFixed: Boolean; virtual;
    function GetFocusedCellViewInfo: TcxGridTableDataCellViewInfo; virtual;
    function GetPropertiesForEdit: TcxCustomEditProperties; virtual;
    function GetPropertiesValue: TcxCustomEditProperties;
    function GetVisible: Boolean; virtual;
    function GetVisibleCaption: string; virtual;
    function GetVisibleForCustomization: Boolean; virtual;
    function GetVisibleIndex: Integer;
    function GetVisibleInQuickCustomizationPopup: Boolean; virtual;
    procedure GroupingChanging; virtual;
    function HasCustomDrawCell: Boolean;
    function HasFixedWidth: Boolean; virtual;
    procedure InitFilteringDateRanges; virtual;
    procedure InitGroupingDateRanges; virtual;
    procedure InitProperties(AProperties: TcxCustomEditProperties); virtual;
    function IsFocusedCellViewInfoPartVisible: Boolean; virtual;
    function IsSortingByDisplayText(ASortByDisplayText: Boolean): Boolean;
    function IsVisibleForRecordChange: Boolean; virtual;
    function IsVisibleStored: Boolean; virtual;
    function IsVisibleForCustomizationStored: Boolean; virtual;
    function CanDataCellScroll: Boolean; virtual;
    procedure PropertiesChanged;
    procedure PropertiesChangedHandler(Sender: TObject);
    procedure PropertiesValueChanged;
    procedure RecalculateDefaultWidth;
    procedure SetEditValue(const Value: Variant); virtual;
    procedure SetGridView(Value: TcxCustomGridTableView); virtual;
    function ShowButtons(AFocused: Boolean): Boolean; virtual;
    function ShowOnUngrouping: Boolean; virtual;
    function SupportsGroupingDateRanges(ACheckCustomHandlers: Boolean): Boolean; virtual;
    function UseFilteredValuesForFilterValueList: Boolean; virtual;
    function UseOwnProperties: Boolean;
    function UseValueItemsForFilterValueList: Boolean; virtual;
    procedure ValidateEditData(AEditProperties: TcxCustomEditProperties);
    procedure ValueTypeClassChanged; virtual;
    procedure VisibleChanged; virtual;
    procedure VisibleForCustomizationChanged; virtual;

    function DefaultAlternateCaption: string; virtual;
    function DefaultCaption: string; virtual;
    function DefaultHeaderAlignmentHorz: TAlignment;
    function DefaultHeaderAlignmentVert: TcxAlignmentVert;
    function DefaultRepositoryItem: TcxEditRepositoryItem;
    function DefaultWidth: Integer; virtual;

    function GetCellStyle: TcxEditStyle;
    procedure InitStyle(AStyle: TcxCustomEditStyle; const AParams: TcxViewParams;
      AFocused: Boolean); virtual;
    procedure ReleaseCellStyle;

    procedure AddCell(ACell: TcxGridTableDataCellViewInfo);
    procedure RemoveCell(ACell: TcxGridTableDataCellViewInfo);
    property CellCount: Integer read GetCellCount;
    property Cells[Index: Integer]: TcxGridTableDataCellViewInfo read GetCell;

    function CreateEditViewData(AProperties: TcxCustomEditProperties): TcxCustomEditViewData;
    procedure DestroyEditViewData(var AEditViewData: TcxCustomEditViewData);
    procedure DoCreateEditViewData;
    procedure DoDestroyEditViewData;
    procedure EditViewDataGetDisplayTextHandler(Sender: TcxCustomEditViewData; var AText: string);
    function GetEditViewData(AProperties: TcxCustomEditProperties;
      out AIsLocalCopy: Boolean; ANeedLocalCopy: Boolean = False): TcxCustomEditViewData;
    procedure ReleaseEditViewData(var AEditViewData: TcxCustomEditViewData; AIsLocalCopy: Boolean);
    property EditViewData: TcxCustomEditViewData read FEditViewData;

    procedure CheckVisible;
    procedure SaveVisible;
    property SavedVisible: Boolean read FSavedVisible;

    procedure DoCustomDrawCell(ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean); virtual;
    procedure DoGetCellHint(ARecord: TcxCustomGridRecord; ACellViewInfo: TcxGridTableDataCellViewInfo;
      const AMousePos: TPoint; var AHintText: TCaption; var AIsHintMultiLine: Boolean;
      var AHintTextRect: TRect); virtual;
    procedure DoGetDataText(ARecordIndex: Integer; var AText: string); virtual;
    procedure DoGetDisplayText(ARecord: TcxCustomGridRecord; var AText: string); virtual;
    procedure DoGetFilterValues(AValueList: TcxDataFilterValueList); virtual;
    function DoGetProperties(ARecord: TcxCustomGridRecord): TcxCustomEditProperties; virtual;
    procedure DoGetPropertiesForEdit(ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties); virtual;
    procedure DoInitFilteringDateRanges; virtual;
    procedure DoInitGroupingDateRanges; virtual;
    procedure DoUserFiltering(const AValue: Variant; const ADisplayText: string); virtual;
    procedure DoUserFilteringEx(AFilterList: TcxFilterCriteriaItemList; const AValue: Variant; const ADisplayText: string); virtual;
    procedure DoValidateDrawValue(ARecord: TcxCustomGridRecord; const AValue: TcxEditValue; AData: TcxEditValidateInfo); virtual;
    function HasCellHintHandler: Boolean;
    function HasCustomPropertiesForEditHandler: Boolean;
    function HasCustomPropertiesHandler: Boolean;
    function HasDataTextHandler: Boolean;
    function HasInitFilteringDateRangesHandler: Boolean;
    function HasInitFilteringDateRangesHandlers: Boolean;
    function HasInitGroupingDateRangesHandler: Boolean;
    function HasInitGroupingDateRangesHandlers: Boolean;
    function HasValidateDrawValueHandler: Boolean;

    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsReading: Boolean read GetIsReading;
    property IsUpdating: Boolean read GetIsUpdating;

    property ActualMinWidth: Integer read GetActualMinWidth;
    property Controller: TcxCustomGridTableController read GetController;
    property DataController: TcxCustomDataController read GetDataController;
    property DateTimeGrouping: TcxGridDateTimeGrouping read FDateTimeGrouping write SetDateTimeGrouping default dtgDefault;
    property EditingProperties: TcxCustomEditProperties read GetEditingProperties;
    property EditPartVisible: Boolean read GetEditPartVisible;
    property Filterable: Boolean read GetFilterable;
    property FilterCaption: string read GetFilterCaption;
    property Fixed: Boolean read GetFixed;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default -1;
    property GroupingDateRanges: TcxGridGroupingDateRanges read FGroupingDateRanges;
    property HeaderAlignmentHorz: TAlignment read GetHeaderAlignmentHorz write SetHeaderAlignmentHorz
      stored IsHeaderAlignmentHorzStored;
    property HeaderAlignmentVert: TcxAlignmentVert read GetHeaderAlignmentVert
      write SetHeaderAlignmentVert stored IsHeaderAlignmentVertStored;
    property HeaderHint: string read FHeaderHint write SetHeaderHint;
    property Hidden: Boolean read GetHidden write SetHidden;  // obsolete, use VisibleForCustomization
    property IgnoreLoadingStatus: Boolean read FIgnoreLoadingStatus write FIgnoreLoadingStatus;
    property InternalVisible: Boolean read FVisible;
    property IsCaptionAssigned: Boolean read FIsCaptionAssigned;
    property MinWidth: Integer read GetMinWidth write SetMinWidth
      default cxGridItemDefaultMinWidth;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
    property VisibleForCustomization: Boolean read GetVisibleForCustomization
      write SetVisibleForCustomization stored IsVisibleForCustomizationStored;
    property VisibleInQuickCustomizationPopup: Boolean read GetVisibleInQuickCustomizationPopup;
    property WasVisibleBeforeGrouping: Boolean read FWasVisibleBeforeGrouping;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;

    property OnInitGroupingDateRanges: TcxGridInitDateRangesEvent read FOnInitGroupingDateRanges write SetOnInitGroupingDateRanges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    procedure ApplyBestFit(ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False); virtual;
    function CalculateDefaultCellHeight(ACanvas: TcxCanvas; AFont: TFont): Integer; virtual;
    function CreateEditStyle: TcxEditStyle; virtual;
    procedure FocusWithSelection; virtual;
    function GetAlternateCaption: string;
    function GetDefaultValuesProvider(AProperties: TcxCustomEditProperties = nil): IcxEditDefaultValuesProvider;
    function GetProperties: TcxCustomEditProperties; overload;
    function GetProperties(ARecord: TcxCustomGridRecord): TcxCustomEditProperties; overload;
    function GetProperties(ARecordIndex: Integer): TcxCustomEditProperties; overload;
    function GetRepositoryItem: TcxEditRepositoryItem;
    procedure MakeVisible;
    procedure RestoreDefaults; virtual;

    property ActuallyVisible: Boolean read GetActuallyVisible;
    property BestFitMaxWidth: Integer read FBestFitMaxWidth write SetBestFitMaxWidth default 0;
    property Editable: Boolean read GetEditable;
    property Editing: Boolean read GetEditing write SetEditing;
    property EditValue: Variant read GetEditValue write SetEditValue;
    property Filtered: Boolean read GetFiltered write SetFiltered;
    property FilteringDateRanges: TcxGridFilteringDateRanges read FFilteringDateRanges;
    property Focused: Boolean read GetFocused write SetFocused;
    property FocusedCellViewInfo: TcxGridTableDataCellViewInfo read GetFocusedCellViewInfo;
    property GridView: TcxCustomGridTableView read FGridView;
    property Hideable: Boolean read CanHide;
    property ID: Integer read FID;
    property IncSearching: Boolean read GetIncSearching;
    property Index: Integer read FIndex write SetIndex;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property Options: TcxCustomGridTableItemOptions read FOptions write SetOptions;
    property PropertiesClass: TcxCustomEditPropertiesClass read FPropertiesClass write SetPropertiesClass;
    property SortIndex: Integer read GetSortIndex write SetSortIndex default -1;
    property SortOrder: TcxGridSortOrder read GetSortOrder write SetSortOrder stored IsSortOrderStored;
    property Styles: TcxCustomGridTableItemStyles read FStyles write SetStyles;
    property VisibleCaption: string read GetVisibleCaption;
    property VisibleIndex: Integer read FVisibleIndex;
  published
    property AlternateCaption: string read FAlternateCaption write SetAlternateCaption;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property DataBinding: TcxGridItemDataBinding read FDataBinding write SetDataBinding;
    property PropertiesClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Properties: TcxCustomEditProperties read FProperties write SetProperties;
    property RepositoryItem: TcxEditRepositoryItem read FRepositoryItem write SetRepositoryItem;
    property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored default True;
    property PropertiesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property StylesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property Tag: TcxTag read GetTag write SetTag stored IsTagStored;
    property OnCustomDrawCell: TcxGridTableDataCellCustomDrawEvent read FOnCustomDrawCell write SetOnCustomDrawCell;
    property OnGetCellHint: TcxGridGetCellHintEvent read FOnGetCellHint write SetOnGetCellHint;
    property OnGetDataText: TcxGridGetDataTextEvent read FOnGetDataText write SetOnGetDataText;
    property OnGetDisplayText: TcxGridGetDisplayTextEvent read FOnGetDisplayText write SetOnGetDisplayText;
    property OnGetFilterDisplayText: TcxGridGetFilterDisplayTextEvent read FOnGetFilterDisplayText write SetOnGetFilterDisplayText;
    property OnGetFilterValues: TcxGridGetFilterValuesEvent read FOnGetFilterValues write SetOnGetFilterValues;
    property OnGetProperties: TcxGridGetPropertiesEvent read FOnGetProperties write SetOnGetProperties;
    property OnGetPropertiesForEdit: TcxGridGetPropertiesEvent read FOnGetPropertiesForEdit write SetOnGetPropertiesForEdit;
    property OnGetStoredProperties: TcxGridTableItemGetStoredPropertiesEvent read FOnGetStoredProperties write SetOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGridTableItemGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write SetOnGetStoredPropertyValue;
    property OnInitFilteringDateRanges: TcxGridInitDateRangesEvent read FOnInitFilteringDateRanges write SetOnInitFilteringDateRanges;
    property OnSetStoredPropertyValue: TcxGridTableItemSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write SetOnSetStoredPropertyValue;
    property OnUserFiltering: TcxGridUserFilteringEvent read FOnUserFiltering write SetOnUserFiltering;
    property OnUserFilteringEx: TcxGridUserFilteringExEvent read FOnUserFilteringEx write SetOnUserFilteringEx;
    property OnValidateDrawValue: TcxGridValidateDrawValueEvent read FOnValidateDrawValue write SetOnValidateDrawValue;
  end;

  // grid view options

  TcxCustomGridTableBackgroundBitmaps = class(TcxCustomGridBackgroundBitmaps)
  protected
    function GetBitmapStyleIndex(Index: Integer): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Content: TBitmap index bbContent read GetValue write SetValue;
    property FilterBox: TBitmap index bbFilterBox read GetValue write SetValue;
  end;

  TcxCustomGridTableDateTimeHandlingClass = class of TcxCustomGridTableDateTimeHandling;

  TcxCustomGridTableDateTimeHandling = class(TcxCustomGridOptions)
  private
    FDateFormat: string;
    FFilters: TcxGridDateTimeFilters;
    FGrouping: TcxGridDateTimeGrouping;
    FHourFormat: string;
    FIgnoreTimeForFiltering: Boolean;
    FMonthFormat: string;
    FUseLongDateFormat: Boolean;
    FUseShortTimeFormat: Boolean;
    FYearFormat: string;
    function GetGridView: TcxCustomGridTableView; inline;
    procedure SetDateFormat(const Value: string);
    procedure SetFilters(Value: TcxGridDateTimeFilters);
    procedure SetGrouping(Value: TcxGridDateTimeGrouping);
    procedure SetHourFormat(const Value: string);
    procedure SetIgnoreTimeForFiltering(Value: Boolean);
    procedure SetMonthFormat(const Value: string);
    procedure SetUseLongDateFormat(Value: Boolean);
    procedure SetUseShortTimeFormat(Value: Boolean);
    procedure SetYearFormat(const Value: string);
  protected
    property DateFormat: string read FDateFormat write SetDateFormat;
    property Grouping: TcxGridDateTimeGrouping read FGrouping write SetGrouping default dtgByDateAndTime;
    property HourFormat: string read FHourFormat write SetHourFormat;
    property UseLongDateFormat: Boolean read FUseLongDateFormat write SetUseLongDateFormat default True;
    property UseShortTimeFormat: Boolean read FUseShortTimeFormat write SetUseShortTimeFormat default True;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetDateFormat: string;
    function GetHourFormat: string;
    function GetMonthFormat: string;
    function GetYearFormat: string;
    procedure GroupingChanged;
    property GridView: TcxCustomGridTableView read GetGridView;
  published
    property Filters: TcxGridDateTimeFilters read FFilters write SetFilters default [];
    property IgnoreTimeForFiltering: Boolean read FIgnoreTimeForFiltering write SetIgnoreTimeForFiltering default False;
    property MonthFormat: string read FMonthFormat write SetMonthFormat;
    property YearFormat: string read FYearFormat write SetYearFormat;
  end;

  TcxGridFilterMRUItem = class(TdxFilterBoxMRUItem);

  TcxGridFilterMRUItems = class(TdxFilterBoxMRUItems)
  private
    FFiltering: TcxCustomGridTableFiltering;
    FLockCount: Integer;
    function GetIsLocked: Boolean;
  protected
    function GetCurrentFilter: TcxDataFilterCriteria; override;
    function GetItemClass: TdxFilterBoxMRUItemClass; override;
    procedure RefreshVisibleItemsList; override;
    procedure VisibleCountChanged(APrevVisibleCount: Integer); override;

    property IsLocked: Boolean read GetIsLocked;
  public
    constructor Create(AFiltering: TcxCustomGridTableFiltering); reintroduce; virtual;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Filtering: TcxCustomGridTableFiltering read FFiltering;
  end;
  TcxGridFilterMRUItemsClass = class of TcxGridFilterMRUItems;

  TcxGridFilterBoxClass = class of TcxGridFilterBox;

  TcxGridFilterBox = class(TcxCustomGridOptions)
  private
    FCustomizeButtonAlignment: TcxGridFilterButtonAlignment;
    FCustomizeDialog: Boolean;
    FMRUItemsListDropDownCount: Integer;
    FPosition: TcxGridFilterPosition;
    FVisible: TcxGridFilterVisible;
    procedure SetButtonPosition(const Value: TcxGridFilterButtonAlignment);
    procedure SetCustomizeDialog(Value: Boolean);
    procedure SetMRUItemsListDropDownCount(Value: Integer);
    procedure SetPosition(Value: TcxGridFilterPosition);
    procedure SetVisible(Value: TcxGridFilterVisible);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property CustomizeButtonAlignment: TcxGridFilterButtonAlignment
      read FCustomizeButtonAlignment write SetButtonPosition default fbaRight;
    property CustomizeDialog: Boolean read FCustomizeDialog write SetCustomizeDialog default True;
    property MRUItemsListDropDownCount: Integer read FMRUItemsListDropDownCount
      write SetMRUItemsListDropDownCount default 0;
    property Position: TcxGridFilterPosition read FPosition write SetPosition default fpBottom;
    property Visible: TcxGridFilterVisible read FVisible write SetVisible default fvNonEmpty;
  end;

  TcxGridFindPanel = class(TcxCustomFindPanel)
  private
    FController: TcxCustomGridTableController;

    function GetGridView: TcxCustomGridTableView;
  protected
    procedure ApplyText(const AText: string); override;
    procedure ClearText; override;
    procedure FocusChanged; override;
    procedure FocusControl; override;
    function FocusData: Boolean; override;
    function GetClearButtonCaption: string; override;
    procedure GetContentViewParams(var AParams: TcxViewParams); override;
    function GetDefaultInfoText: string; override;
    function GetDefaultMRUItemsListCount: Integer; override;
    function GetDefaultMRUItemsListDropDownCount: Integer; override;
    function GetEditLookAndFeel: TcxLookAndFeel; override;
    function GetFindButtonCaption: string; override;
    function GetOwner: TComponent; override;
    function GetParent: TWinControl; override;
    function GetText: string; override;
    function GetUseExtendedSyntax: Boolean; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure VisibilityChanged; override;
    procedure SetUseExtendedSyntax(AValue: Boolean); override;
  public
    constructor Create(AController: TcxCustomGridTableController); reintroduce; virtual;

    procedure Changed; override;

    property Controller: TcxCustomGridTableController read FController;
    property GridView: TcxCustomGridTableView read GetGridView;
  end;

  TcxGridFindPanelOptions = class(TcxCustomGridOptions)
  private
    function GetApplyInputDelay: Integer;
    function GetClearFindFilterTextOnClose: Boolean;
    function GetDisplayMode: TcxFindPanelDisplayMode;
    function GetFocusViewOnApplyFilter: Boolean;
    function GetGridView: TcxCustomGridTableView;
    function GetHighlightSearchResults: Boolean;
    function GetInfoText: string;
    function GetOptions: TcxFindPanelOptions;
    function GetMRUItems: TStrings;
    function GetMRUItemsListCount: Integer;
    function GetMRUItemsListDropDownCount: Integer;
    function GetPosition: TcxFindPanelPosition;
    function GetShowClearButton: Boolean;
    function GetShowCloseButton: Boolean;
    function GetShowFindButton: Boolean;
    function GetUseDelayedFind: Boolean;
    function GetUseExtendedSyntax: Boolean;
    function IsInfoTextStored: Boolean;
    procedure SetApplyInputDelay(AValue: Integer);
    procedure SetClearFindFilterTextOnClose(AValue: Boolean);
    procedure SetDisplayMode(AValue: TcxFindPanelDisplayMode);
    procedure SetFocusViewOnApplyFilter(AValue: Boolean);
    procedure SetHighlightSearchResults(AValue: Boolean);
    procedure SetInfoText(AValue: string);
    procedure SetOptions(AValue: TcxFindPanelOptions);
    procedure SetMRUItems(AValue: TStrings);
    procedure SetMRUItemsListCount(AValue: Integer);
    procedure SetMRUItemsListDropDownCount(AValue: Integer);
    procedure SetPosition(AValue: TcxFindPanelPosition);
    procedure SetShowClearButton(AValue: Boolean);
    procedure SetShowCloseButton(AValue: Boolean);
    procedure SetShowFindButton(AValue: Boolean);
    procedure SetUseExtendedSyntax(AValue: Boolean);
    procedure SetUseDelayedFind(AValue: Boolean);
  protected
    function IsCloseButtonVisible: Boolean;

    property Options: TcxFindPanelOptions read GetOptions write SetOptions;
  public
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxCustomGridTableView read GetGridView;
    property MRUItems: TStrings read GetMRUItems write SetMRUItems;
  published
    property ApplyInputDelay: Integer read GetApplyInputDelay write SetApplyInputDelay default 1000;
    property ClearFindFilterTextOnClose: Boolean read GetClearFindFilterTextOnClose write SetClearFindFilterTextOnClose default True;
    property DisplayMode: TcxFindPanelDisplayMode read GetDisplayMode write SetDisplayMode default fpdmNever;
    property FocusViewOnApplyFilter: Boolean read GetFocusViewOnApplyFilter write SetFocusViewOnApplyFilter default False;
    property HighlightSearchResults: Boolean read GetHighlightSearchResults write SetHighlightSearchResults default True;
    property InfoText: string read GetInfoText write SetInfoText stored IsInfoTextStored;
    property MRUItemsListCount: Integer read GetMRUItemsListCount write SetMRUItemsListCount default cxGridFindPanelDefaultMRUItemsListCount;
    property MRUItemsListDropDownCount: Integer read GetMRUItemsListDropDownCount write SetMRUItemsListDropDownCount default cxGridFindPanelDefaultMRUItemsListDropDownCount;
    property Position: TcxFindPanelPosition read GetPosition write SetPosition default fppTop;
    property ShowClearButton: Boolean read GetShowClearButton write SetShowClearButton default True;
    property ShowCloseButton: Boolean read GetShowCloseButton write SetShowCloseButton default True;
    property ShowFindButton: Boolean read GetShowFindButton write SetShowFindButton default True;
    property UseDelayedFind: Boolean read GetUseDelayedFind write SetUseDelayedFind default True;
    property UseExtendedSyntax: Boolean read GetUseExtendedSyntax write SetUseExtendedSyntax default False;
  end;

  { TcxGridItemFilterPopupOptions }

  TcxGridItemFilterPopupOptionsClass = class of TcxGridItemFilterPopupOptions;
  TcxGridItemFilterPopupOptions = class(TcxCustomGridOptions)
  strict private
    FApplyMultiSelectChanges: TcxGridItemFilterPopupApplyChangesMode;
    FDropDownWidth: Integer;
    FMaxDropDownItemCount: Integer;
    FMultiSelect: Boolean;

    procedure SetApplyMultiSelectChanges(AValue: TcxGridItemFilterPopupApplyChangesMode);
    procedure SetDropDownWidth(AValue: Integer);
    procedure SetMaxDropDownItemCount(AValue: Integer);
    procedure SetMultiSelect(AValue: Boolean);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;

    procedure Assign(ASource: TPersistent); override;
  published
    property ApplyMultiSelectChanges: TcxGridItemFilterPopupApplyChangesMode read FApplyMultiSelectChanges write SetApplyMultiSelectChanges default fpacImmediately;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 0;
    property MaxDropDownItemCount: Integer read FMaxDropDownItemCount write SetMaxDropDownItemCount default cxGridFilterDefaultItemPopupMaxDropDownItemCount;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default True;
  end;

  { TcxGridItemExcelFilterPopupOptions }

  TcxGridItemExcelFilterPopupOptionsClass = class of TcxGridItemExcelFilterPopupOptions;
  TcxGridItemExcelFilterPopupOptions = class(TcxCustomGridOptions)
  strict private
    FApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
    FDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
    FDefaultPage: TdxExcelFilterValueContainerDefaultPage;
    FNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;

    procedure SetApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
    procedure SetDateTimeValuesPageType(const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
    procedure SetDefaultPage(const AValue: TdxExcelFilterValueContainerDefaultPage);
    procedure SetNumericValuesPageType(const AValue: TdxExcelFilterValueContainerNumericValuesPageType);
  public
    procedure Assign(ASource: TPersistent); override;
  published
    property ApplyChanges: TdxExcelFilterValueContainerApplyChangesMode read FApplyChanges write SetApplyChanges default efacDefault;
    property DateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType read FDateTimeValuesPageType
      write SetDateTimeValuesPageType default dvptDefault;
    property DefaultPage: TdxExcelFilterValueContainerDefaultPage read FDefaultPage write SetDefaultPage default dpDefault;
    property NumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType read FNumericValuesPageType
      write SetNumericValuesPageType default nvptDefault;
  end;

  { TcxCustomGridTableFiltering }

  TcxCustomGridTableFilteringClass = class of TcxCustomGridTableFiltering;
  TcxCustomGridTableFiltering = class(TcxCustomGridOptions)
  private
    FItemAddValueItems: Boolean;
    FItemExcelPopup: TcxGridItemExcelFilterPopupOptions;
    FItemFilteredItemsList: Boolean;
    FItemMRUItemsList: Boolean;
    FItemMRUItemsListCount: Integer;
    FItemPopup: TcxGridItemFilterPopupOptions;
    FItemPopupMode: TdxFilterPopupWindowMode;
    FMRUItems: TcxGridFilterMRUItems;
    FMRUItemsList: Boolean;
    FMRUItemsListCount: Integer;
    FSynchronizeWithControlDialog: Boolean;

    function GetCustomizeDialog: Boolean;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetMRUItemsListDropDownCount: Integer;
    function GetPosition: TcxGridFilterPosition;
    function GetVisible: TcxGridFilterVisible;
    procedure SetCustomizeDialog(Value: Boolean);
    procedure SetItemAddValueItems(Value: Boolean);
    procedure SetItemExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
    procedure SetItemFilteredItemsList(Value: Boolean);
    procedure SetItemMRUItemsList(Value: Boolean);
    procedure SetItemMRUItemsListCount(Value: Integer);
    procedure SetItemPopup(Value: TcxGridItemFilterPopupOptions);
    procedure SetItemPopupMode(Value: TdxFilterPopupWindowMode);
    procedure SetMRUItemsList(Value: Boolean);
    procedure SetMRUItemsListCount(Value: Integer);
    procedure SetMRUItemsListDropDownCount(Value: Integer);
    procedure SetPosition(Value: TcxGridFilterPosition);
    procedure SetVisible(Value: TcxGridFilterVisible);
    //
    procedure AfterFilterControlDialogApply(Sender: TObject);
    procedure BeforeFilterControlDialogApply(Sender: TObject);
    procedure FilterControlDialogApply(Sender: TObject);
    //
    procedure ReadCustomizeDialog(Reader: TReader);
    procedure ReadMRUItemsListDropDownCount(Reader: TReader);
    procedure ReadPosition(Reader: TReader);
    procedure ReadVisible(Reader: TReader);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure FilterChanged; virtual;
    function GetItemPopupClass: TcxGridItemFilterPopupOptionsClass; virtual;
    function GetItemExcelPopupClass: TcxGridItemExcelFilterPopupOptionsClass; virtual;
    function GetMRUItemsClass: TcxGridFilterMRUItemsClass; virtual;
    function IsFilterBoxEnabled: Boolean; virtual;
    function IsMRUItemsListAvailable: Boolean; virtual;
    procedure MRUItemsVisibleCountChanged(AOldCount, ANewCount: Integer); virtual;

    property ItemAddValueItems: Boolean read FItemAddValueItems write SetItemAddValueItems default True;
    property ItemExcelPopup: TcxGridItemExcelFilterPopupOptions read FItemExcelPopup write SetItemExcelPopup;
    property ItemFilteredItemsList: Boolean read FItemFilteredItemsList write SetItemFilteredItemsList default False;
    property ItemMRUItemsList: Boolean read FItemMRUItemsList write SetItemMRUItemsList default True;
    property ItemMRUItemsListCount: Integer read FItemMRUItemsListCount write SetItemMRUItemsListCount default cxGridFilterDefaultItemMRUItemsListCount;
    property ItemPopup: TcxGridItemFilterPopupOptions read FItemPopup write SetItemPopup;
    property ItemPopupMode: TdxFilterPopupWindowMode read FItemPopupMode write SetItemPopupMode default fpmDefault;

    function GetItemPopupDropDownWidth: Integer;
    function GetItemPopupMaxDropDownItemCount: Integer;
    procedure SetItemPopupDropDownWidth(Value: Integer);
    procedure SetItemPopupMaxDropDownItemCount(Value: Integer);
    procedure ReadItemPopupDropDownWidth(Reader: TReader);
    procedure ReadItemPopupMaxDropDownCount(Reader: TReader);
    // obsolete - use ItemPopup.DropDownWidth
    property DropDownWidth: Integer read GetItemPopupDropDownWidth write SetItemPopupDropDownWidth;
    // obsolete - use ItemPopup.MaxDropDownItemCount
    property MaxDropDownCount: Integer read GetItemPopupMaxDropDownItemCount write SetItemPopupMaxDropDownItemCount;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure AddFilterToMRUItems(AFilter: TcxDataFilterCriteria = nil);
    procedure Assign(Source: TPersistent); override;
    procedure RunCustomizeDialog(AItem: TcxCustomGridTableItem = nil); virtual;

    property GridView: TcxCustomGridTableView read GetGridView;
    property MRUItems: TcxGridFilterMRUItems read FMRUItems;
    // obsolete - use GridView.FilterBox
    property CustomizeDialog: Boolean read GetCustomizeDialog write SetCustomizeDialog;
    property MRUItemsListDropDownCount: Integer read GetMRUItemsListDropDownCount write SetMRUItemsListDropDownCount;
    property Position: TcxGridFilterPosition read GetPosition write SetPosition;
    property Visible: TcxGridFilterVisible read GetVisible write SetVisible;
  published
    property MRUItemsList: Boolean read FMRUItemsList write SetMRUItemsList default True;
    property MRUItemsListCount: Integer read FMRUItemsListCount write SetMRUItemsListCount
      default cxGridFilterDefaultMRUItemsListCount;
  end;

  { TcxGridViewNavigator }

  TcxGridViewNavigatorButtons = class(TcxNavigatorControlButtons)
  private
    FGridView: TcxCustomGridTableView;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;

    property GridView: TcxCustomGridTableView read FGridView;
  published
    property ConfirmDelete default False;
  end;

  TcxGridViewNavigatorButtonsClass = class of TcxGridViewNavigatorButtons;

  TcxGridViewNavigatorInfoPanel = class(TcxCustomNavigatorInfoPanel)
  private
    FGridView: TcxCustomGridTableView;
  protected
    function GetAdornerTargetElementVisible: Boolean; override;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;
    function GetViewParams: TcxViewParams; override;

    property GridView: TcxCustomGridTableView read FGridView;
  published
    property DisplayMask;
    property Visible;
    property Width;
  end;

  { TcxGridViewNavigator }

  TcxGridViewNavigator = class(TcxCustomGridOptions)
  strict private
    FButtons: TcxGridViewNavigatorButtons;
    FInfoPanel: TcxGridViewNavigatorInfoPanel;
    FVisible: Boolean;

    function GetGridView: TcxCustomGridTableView; inline;
    function GetIRecordPosition: IcxNavigatorRecordPosition;
    procedure SetButtons(Value: TcxGridViewNavigatorButtons);
    procedure SetInfoPanel(Value: TcxGridViewNavigatorInfoPanel);
    procedure SetVisible(Value: Boolean);
  protected
    function GetNavigatorButtonsClass: TcxGridViewNavigatorButtonsClass; virtual;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxCustomGridTableView read GetGridView;
  published
    property Buttons: TcxGridViewNavigatorButtons read FButtons write SetButtons;
    property InfoPanel: TcxGridViewNavigatorInfoPanel read FInfoPanel write SetInfoPanel;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  TcxGridViewNavigatorClass = class of TcxGridViewNavigator;

  TcxCustomGridTableShowLockedStateImageOptions = class(TcxCustomGridShowLockedStateImageOptions)
  private
    FBestFit: TcxGridShowLockedStateImageMode;
    FFiltering: TcxGridShowLockedStateImageMode;
    FGrouping: TcxGridShowLockedStateImageMode;
    FSorting: TcxGridShowLockedStateImageMode;
    FPosting: TcxGridShowLockedStateImageMode;
  protected
    property BestFit: TcxGridShowLockedStateImageMode read FBestFit write FBestFit default lsimPending;
    property Filtering: TcxGridShowLockedStateImageMode read FFiltering write FFiltering default lsimPending;
    property Grouping: TcxGridShowLockedStateImageMode read FGrouping write FGrouping default lsimPending;
    property Sorting: TcxGridShowLockedStateImageMode read FSorting write FSorting default lsimPending;
    property Posting: TcxGridShowLockedStateImageMode read FPosting write FPosting default lsimNever;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TcxGridDragFocusing = (dfNone, dfDragOver, dfDragDrop);

  TcxCustomGridTableOptionsBehavior = class(TcxCustomGridOptionsBehavior)
  private
    FAlwaysShowEditor: Boolean;
    FBestFitMaxRecordCount: Integer;
    FCellHints: Boolean;
    FCopyCaptionsToClipboard: Boolean;
    FCopyRecordsToClipboard: Boolean;
    FDragDropText: Boolean;
    FDragFocusing: TcxGridDragFocusing;
    FDragHighlighting: Boolean;
    FDragOpening: Boolean;
    FDragScrolling: Boolean;
    FEditAutoHeight: TcxInplaceEditAutoHeight;
    FFocusCellOnCycle: Boolean;
    FFocusCellOnTab: Boolean;
    FFocusFirstCellOnNewRecord: Boolean;
    FGoToNextCellOnEnter: Boolean;
    FImmediateEditor: Boolean;
    FIncSearch: Boolean;
    FIncSearchItem: TcxCustomGridTableItem;
    FNavigatorHints: Boolean;
    FPullFocusing: Boolean;
    FRecordScrollMode: TcxRecordScrollMode;
    FRepaintVisibleRecordsOnScroll: Boolean;

    function GetGridView: TcxCustomGridTableView; inline;
    function GetShowLockedStateImageOptions: TcxCustomGridTableShowLockedStateImageOptions;
    procedure SetAlwaysShowEditor(Value: Boolean);
    procedure SetBestFitMaxRecordCount(Value: Integer);
    procedure SetCellHints(Value: Boolean);
    procedure SetCopyCaptionsToClipboard(Value: Boolean);
    procedure SetCopyRecordsToClipboard(Value: Boolean);
    procedure SetDragDropText(Value: Boolean);
    procedure SetDragFocusing(Value: TcxGridDragFocusing);
    procedure SetDragHighlighting(Value: Boolean);
    procedure SetDragOpening(Value: Boolean);
    procedure SetDragScrolling(Value: Boolean);
    procedure SetEditAutoHeight(Value: TcxInplaceEditAutoHeight);
    procedure SetFocusCellOnCycle(Value: Boolean);
    procedure SetFocusCellOnTab(Value: Boolean);
    procedure SetFocusFirstCellOnNewRecord(Value: Boolean);
    procedure SetGoToNextCellOnEnter(Value: Boolean);
    procedure SetImmediateEditor(Value: Boolean);
    procedure SetIncSearch(Value: Boolean);
    procedure SetIncSearchItem(Value: TcxCustomGridTableItem);
    procedure SetNavigatorHints(Value: Boolean);
    procedure SetPullFocusing(Value: Boolean);
    procedure SetRecordScrollMode(Value: TcxRecordScrollMode);
    procedure SetRepaintVisibleRecordsOnScroll(Value: Boolean);
    procedure SetShowLockedStateImageOptions(Value: TcxCustomGridTableShowLockedStateImageOptions);
  protected
    function GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass; override;

    property EditAutoHeight: TcxInplaceEditAutoHeight read FEditAutoHeight write SetEditAutoHeight default eahNone;
    property FocusCellOnCycle: Boolean read FFocusCellOnCycle write SetFocusCellOnCycle default False;
    property RecordScrollMode: TcxRecordScrollMode read FRecordScrollMode write SetRecordScrollMode default rsmDefault;
    property ShowLockedStateImageOptions: TcxCustomGridTableShowLockedStateImageOptions
      read GetShowLockedStateImageOptions write SetShowLockedStateImageOptions;
    property PullFocusing: Boolean read FPullFocusing write SetPullFocusing default False;
    property RepaintVisibleRecordsOnScroll: Boolean read FRepaintVisibleRecordsOnScroll write SetRepaintVisibleRecordsOnScroll default False;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;

    property BestFitMaxRecordCount: Integer read FBestFitMaxRecordCount write SetBestFitMaxRecordCount default 0;
    property ImmediateEditor: Boolean read FImmediateEditor write SetImmediateEditor default True;
    property GridView: TcxCustomGridTableView read GetGridView;
  published
    property AlwaysShowEditor: Boolean read FAlwaysShowEditor write SetAlwaysShowEditor default False;
    property CellHints: Boolean read FCellHints write SetCellHints default False;
    property CopyCaptionsToClipboard: Boolean read FCopyCaptionsToClipboard write SetCopyCaptionsToClipboard default True;
    property CopyRecordsToClipboard: Boolean read FCopyRecordsToClipboard write SetCopyRecordsToClipboard default True;
    property DragDropText: Boolean read FDragDropText write SetDragDropText default False;
    property DragFocusing: TcxGridDragFocusing read FDragFocusing write SetDragFocusing default dfNone;
    property DragHighlighting: Boolean read FDragHighlighting write SetDragHighlighting default True;
    property DragOpening: Boolean read FDragOpening write SetDragOpening default True;
    property DragScrolling: Boolean read FDragScrolling write SetDragScrolling default True;
    property FocusCellOnTab: Boolean read FFocusCellOnTab write SetFocusCellOnTab default False;
    property FocusFirstCellOnNewRecord: Boolean read FFocusFirstCellOnNewRecord
      write SetFocusFirstCellOnNewRecord default False;
    property GoToNextCellOnEnter: Boolean read FGoToNextCellOnEnter write SetGoToNextCellOnEnter default False;
    property HintHidePause;
    property IncSearch: Boolean read FIncSearch write SetIncSearch default False;
    property IncSearchItem: TcxCustomGridTableItem read FIncSearchItem write SetIncSearchItem;
    property NavigatorHints: Boolean read FNavigatorHints write SetNavigatorHints default False;
    property ShowHourglassCursor;
    property SuppressHintOnMouseDown;
  end;

  TcxGridQuickCustomizationReordering = (qcrDefault, qcrEnabled, qcrDisabled);

  TcxCustomGridTableOptionsCustomizeClass = class of TcxCustomGridTableOptionsCustomize;

  TcxCustomGridTableOptionsCustomize = class(TcxCustomGridOptions)
  private
    FItemFiltering: Boolean;
    FItemGrouping: Boolean;
    FItemHiding: Boolean;
    FItemMoving: Boolean;
    FItemSorting: Boolean;
    FItemsQuickCustomization: Boolean;
    FItemsQuickCustomizationMaxDropDownCount: Integer;
    FItemsQuickCustomizationMultiColumnMode: Boolean;
    FItemsQuickCustomizationReordering: TcxGridQuickCustomizationReordering;
    FItemsQuickCustomizationShowCommands: Boolean;
    FItemsQuickCustomizationSorted: Boolean;
    procedure SetItemFiltering(Value: Boolean);
    procedure SetItemGrouping(Value: Boolean);
    procedure SetItemHiding(Value: Boolean);
    procedure SetItemMoving(Value: Boolean);
    procedure SetItemSorting(Value: Boolean);
    procedure SetItemsQuickCustomization(Value: Boolean);
    procedure SetItemsQuickCustomizationMaxDropDownCount(Value: Integer);
    procedure SetItemsQuickCustomizationMultiColumnMode(Value: Boolean);
    procedure SetItemsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
    procedure SetItemsQuickCustomizationShowCommands(Value: Boolean);
    procedure SetItemsQuickCustomizationSorted(Value: Boolean);
  protected
    property ItemFiltering: Boolean read FItemFiltering write SetItemFiltering default True;
    property ItemGrouping: Boolean read FItemGrouping write SetItemGrouping default True;
    property ItemHiding: Boolean read FItemHiding write SetItemHiding default False;
    property ItemMoving: Boolean read FItemMoving write SetItemMoving default True;
    property ItemSorting: Boolean read FItemSorting write SetItemSorting default True;
    property ItemsQuickCustomization: Boolean read FItemsQuickCustomization
      write SetItemsQuickCustomization default False;
    property ItemsQuickCustomizationMaxDropDownCount: Integer read FItemsQuickCustomizationMaxDropDownCount
      write SetItemsQuickCustomizationMaxDropDownCount default 0;
    property ItemsQuickCustomizationMultiColumnMode: Boolean read FItemsQuickCustomizationMultiColumnMode
      write SetItemsQuickCustomizationMultiColumnMode default True;
    property ItemsQuickCustomizationReordering: TcxGridQuickCustomizationReordering
      read FItemsQuickCustomizationReordering write SetItemsQuickCustomizationReordering default qcrDefault;
    property ItemsQuickCustomizationShowCommands: Boolean read FItemsQuickCustomizationShowCommands
      write SetItemsQuickCustomizationShowCommands default True;
    property ItemsQuickCustomizationSorted: Boolean
      read FItemsQuickCustomizationSorted write SetItemsQuickCustomizationSorted default False;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function SupportsItemsQuickCustomizationReordering: Boolean; virtual;
  end;

  TcxCustomGridTableOptionsData = class(TcxCustomGridOptionsData)
  private
    FAppending: Boolean;
    FCancelOnExit: Boolean;
    FDeleting: Boolean;
    FDeletingConfirmation: Boolean;
    FEditing: Boolean;
    FInserting: Boolean;
    function GetGridView: TcxCustomGridTableView; inline;
    procedure SetAppending(Value: Boolean);
    procedure SetCancelOnExit(Value: Boolean);
    procedure SetDeleting(Value: Boolean);
    procedure SetDeletingConfirmation(Value: Boolean);
    procedure SetEditing(Value: Boolean);
    procedure SetInserting(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxCustomGridTableView read GetGridView;
  published
    property Appending: Boolean read FAppending write SetAppending default False;
    property CancelOnExit: Boolean read FCancelOnExit write SetCancelOnExit default True;
    property Deleting: Boolean read FDeleting write SetDeleting default True;
    property DeletingConfirmation: Boolean read FDeletingConfirmation
      write SetDeletingConfirmation default True;
    property Editing: Boolean read FEditing write SetEditing default True;
    property Inserting: Boolean read FInserting write SetInserting default True;
  end;

  TcxCustomGridTableOptionsSelection = class(TcxCustomGridOptionsSelection)
  private
    FCellSelect: Boolean;
    FHideFocusRectOnExit: Boolean;
    FHideSelection: Boolean;
    FInvertSelect: Boolean;
    FUnselectFocusedRecordOnExit: Boolean;
    function GetGridView: TcxCustomGridTableView; inline;
    function GetMultiSelect: Boolean;
    procedure SetHideFocusRectOnExit(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetUnselectFocusedRecordOnExit(Value: Boolean);
  protected
    function IsMultiSelectPersistent: Boolean; virtual;
    procedure SetCellSelect(Value: Boolean); virtual;
    procedure SetInvertSelect(Value: Boolean); virtual;
    procedure SetMultiSelect(Value: Boolean); virtual;

    property GridView: TcxCustomGridTableView read GetGridView;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;

    property HideFocusRect: Boolean read FHideFocusRectOnExit write SetHideFocusRectOnExit stored False;  // for compatibility
    property HideFocusRectOnExit: Boolean read FHideFocusRectOnExit write SetHideFocusRectOnExit default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default False;
    property InvertSelect: Boolean read FInvertSelect write SetInvertSelect default True;
    property UnselectFocusedRecordOnExit: Boolean read FUnselectFocusedRecordOnExit
      write SetUnselectFocusedRecordOnExit default True;
  published
    property CellSelect: Boolean read FCellSelect write SetCellSelect default True;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
  end;

  { TcxCustomGridTableOptionsView }

  TcxGridShowEditButtons = (gsebNever, gsebForFocusedRecord, gsebAlways);

  TcxCustomGridTableOptionsView = class(TcxCustomGridOptionsView)
  strict private
    FCellAutoHeight: Boolean;
    FCellEndEllipsis: Boolean;
    FCellTextMaxLineCount: Integer;
    FEditAutoHeightBorderColor: TColor;
    FFocusRect: Boolean;
    FItemCaptionAutoHeight: Boolean;
    FItemCaptionEndEllipsis: Boolean;
    FItemFilterButtonShowMode: TcxGridItemFilterButtonShowMode;
    FNavigatorOffset: Integer;
    FNoDataToDisplayInfoText: string;
    FShowEditButtons: TcxGridShowEditButtons;
    FShowItemFilterButtons: TcxGridShowItemFilterButtons;

    function GetGridView: TcxCustomGridTableView; inline;
    procedure ReadNavigator(Reader: TReader);
    procedure SetCellAutoHeight(Value: Boolean);
    procedure SetCellEndEllipsis(Value: Boolean);
    procedure SetCellTextMaxLineCount(Value: Integer);
    procedure SetEditAutoHeightBorderColor(Value: TColor);
    procedure SetFocusRect(Value: Boolean);
    procedure SetItemCaptionAutoHeight(Value: Boolean);
    procedure SetItemCaptionEndEllipsis(Value: Boolean);
    procedure SetItemFilterButtonShowMode(Value: TcxGridItemFilterButtonShowMode);
    procedure SetNavigatorOffset(Value: Integer);
    procedure SetNoDataToDisplayInfoText(const Value: string);
    procedure SetShowEditButtons(Value: TcxGridShowEditButtons);
    procedure SetShowItemFilterButtons(Value: TcxGridShowItemFilterButtons);
    function IsNoDataToDisplayInfoTextAssigned: Boolean;
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ItemCaptionAutoHeightChanged; dynamic;

    property CellAutoHeight: Boolean read FCellAutoHeight write SetCellAutoHeight default False;
    property CellTextMaxLineCount: Integer read FCellTextMaxLineCount write SetCellTextMaxLineCount default 0;
    property EditAutoHeightBorderColor: TColor read FEditAutoHeightBorderColor write SetEditAutoHeightBorderColor default clDefault;
    property ItemCaptionAutoHeight: Boolean read FItemCaptionAutoHeight write SetItemCaptionAutoHeight default False;
    property ItemFilterButtonShowMode: TcxGridItemFilterButtonShowMode read FItemFilterButtonShowMode write SetItemFilterButtonShowMode default fbmDefault;
    property ItemCaptionEndEllipsis: Boolean read FItemCaptionEndEllipsis write SetItemCaptionEndEllipsis default False;
    property ShowItemFilterButtons: TcxGridShowItemFilterButtons read FShowItemFilterButtons write SetShowItemFilterButtons default sfbDefault;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetGridLineColor: TColor; virtual;
    function GetNoDataToDisplayInfoText: string;

    property GridView: TcxCustomGridTableView read GetGridView;
  published
    property CellEndEllipsis: Boolean read FCellEndEllipsis write SetCellEndEllipsis default False;
    property FocusRect: Boolean read FFocusRect write SetFocusRect default True;
    property NavigatorOffset: Integer read FNavigatorOffset write SetNavigatorOffset default cxGridNavigatorDefaultOffset;
    property NoDataToDisplayInfoText: string read FNoDataToDisplayInfoText write SetNoDataToDisplayInfoText stored IsNoDataToDisplayInfoTextAssigned;
    property ScrollBars;
    property ShowEditButtons: TcxGridShowEditButtons read FShowEditButtons write SetShowEditButtons default gsebNever;
  end;

  TcxGridCellPos = class
    GridRecord: TcxCustomGridRecord;
    Item: TObject;
    constructor Create(AGridRecord: TcxCustomGridRecord; AItem: TObject);
  end;

  TcxGridDataCellPos = class
    GridRecord: TcxCustomGridRecord;
    Item: TcxCustomGridTableItem;
    constructor Create(AGridRecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem); overload;
  end;

  TcxCustomGridTableViewStyles = class(TcxCustomGridViewStyles)
  private
    FUseOddEvenStyles: TdxDefaultBoolean;
    FOnGetContentStyle: TcxGridGetCellStyleEvent;
    function GetGridViewValue: TcxCustomGridTableView; inline;
    procedure SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
    procedure SetOnGetContentStyle(Value: TcxGridGetCellStyleEvent);
  protected
    function ActuallyUseOddEvenStyles: Boolean; inline;
    procedure GetConditionalFormattingContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AParams: TcxViewParams);
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    procedure GetSelectionParams(ARecord: TcxCustomGridRecord; AItem: TObject;
      out AParams: TcxViewParams); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCellContentParams(ARecord: TcxCustomGridRecord; AItem: TObject;
      out AParams: TcxViewParams); virtual;
    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetDataCellContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetDataCellParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams; AUseViewInfo: Boolean = False; ACellViewInfo: TcxGridTableCellViewInfo = nil;
      AIgnoreSelection: Boolean = False); virtual;
    procedure GetRecordContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;

    property GridView: TcxCustomGridTableView read GetGridViewValue;
    property Inactive: TcxStyle index vsInactive read GetValue write SetValue;
    property Selection: TcxStyle index vsSelection read GetValue write SetValue;
  published
    property Content: TcxStyle index vsContent read GetValue write SetValue;
    property ContentEven: TcxStyle index vsContentEven read GetValue write SetValue;
    property ContentOdd: TcxStyle index vsContentOdd read GetValue write SetValue;
    property FilterBox: TcxStyle index vsFilterBox read GetValue write SetValue;
    property FindPanel: TcxStyle index vsFindPanel read GetValue write SetValue;
    property IncSearch: TcxStyle index vsIncSearch read GetValue write SetValue;
    property Navigator: TcxStyle index vsNavigator read GetValue write SetValue;
    property NavigatorInfoPanel: TcxStyle index vsNavigatorInfoPanel read GetValue write SetValue;
    property SearchResultHighlight: TcxStyle index vsSearchResultHighlight read GetValue write SetValue;
    property UseOddEvenStyles: TdxDefaultBoolean read FUseOddEvenStyles write SetUseOddEvenStyles default bDefault;
    property OnGetContentStyle: TcxGridGetCellStyleEvent read FOnGetContentStyle write SetOnGetContentStyle;
  end;

  { TcxGridConditionalFormattingProvider }

  TcxGridConditionalFormattingProvider = class(TcxDataControllerConditionalFormattingProvider)
  strict private
    FOwner: TcxCustomGridTableView;
  protected
    procedure Changed; override;
    procedure ConditionalFormattingChanged; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function DoGetParentForm: TCustomForm; override;
    function EditCellViewInfoToPoint(ACellViewInfo: TObject): TPoint; override;
    function GetCellBounds(ACellViewInfo: TObject): TRect; override;
    function GetDisplayName(AField: TcxCustomDataField): string; overload; override;
    function GetFocusedItem: TObject; override;
    function GetEditProperties(AItem: TObject): TcxCustomEditProperties; override;
    function GetLookAndFeel: TcxLookAndFeel; override;
    function GetOwner: TComponent; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function IsFieldVisible(AField: TcxCustomDataField): Boolean; overload; override;
    function IsRightToLeft: Boolean; override;
    function IsRowVisible(const ARow: Integer): Boolean; override;
    procedure SynchronizeMasterClasses; virtual;

    property Owner: TcxCustomGridTableView read FOwner;
  public
    constructor Create(AOwner: TcxCustomGridTableView); reintroduce;
  end;

  { TcxGridDetailModeConditionalFormattingProvider }

  TcxGridDetailModeConditionalFormattingProvider = class(TcxGridConditionalFormattingProvider)
  strict private
    FCustomAreaInfoCache: TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>;
  protected
    function CalculateStyle(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow: Integer; AColumn: Integer; ACell: IdxSpreadSheetCellData): Boolean; override;
    procedure ClearCacheCore; override;
    function IsDetailMode: Boolean; override;
    procedure SynchronizeMasterClasses; override;
  public
    destructor Destroy; override;
  end;

  // grid view

  TcxGridOpenTableItemList = class(TcxOpenList)
  private
    function GetItem(Index: Integer): TcxCustomGridTableItem;
    procedure SetItem(Index: Integer; Value: TcxCustomGridTableItem);
  public
    property Items[Index: Integer]: TcxCustomGridTableItem read GetItem write SetItem; default;
  end;

  TcxGridViewRestoringFilterAttribute = (rfaActive, rfaData, rfaMRUItems);
  TcxGridViewRestoringFilterAttributes = set of TcxGridViewRestoringFilterAttribute;

  TcxGridTableCellCustomDrawEvent = procedure(Sender: TcxCustomGridTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean) of object;

  TcxGridAllowRecordOperationEvent = procedure(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; var AAllow: Boolean) of object;
  TcxGridCellClickEvent = procedure(Sender: TcxCustomGridTableView;
    ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
    AShift: TShiftState; var AHandled: Boolean) of object;
  TcxGridCustomTableViewEvent = procedure(Sender: TcxCustomGridTableView) of object;
  TcxGridEditingEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem; var AAllow: Boolean) of object;
  TcxGridEditDblClickEvent = procedure(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
    AEdit: TcxCustomEdit) of object;
  TcxGridEditKeyEvent = procedure(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
    AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState) of object;
  TcxGridEditKeyPressEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Char) of object;
  TcxGridCustomTableItemEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem) of object;
  TcxGridFilterCustomizationEvent = procedure(Sender: TcxCustomGridTableView;
    var ADone: Boolean) of object;
  TcxGridFilterDialogShowEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem; var ADone: Boolean) of object;
  TcxGridFindPanelVisibilityChangedEvent = procedure(Sender: TcxCustomGridTableView;
    const AVisible: Boolean) of object;
  TcxGridFocusedItemChangedEvent = procedure(Sender: TcxCustomGridTableView;
    APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem) of object;
  TcxGridFocusedRecordChangedEvent = procedure(Sender: TcxCustomGridTableView;
    APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean) of object;
  TcxGridGetCellHeightEvent = procedure(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
    ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer) of object;
  TcxGridGetDragDropTextEvent = procedure(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AText: string) of object;
  TcxGridInitEditEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit) of object;
  TcxGridInitEditValueEvent = procedure(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var AValue: TcxEditValue) of object;
  TcxGridPartCustomDrawBackgroundEvent = procedure(Sender: TcxCustomGridTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo; var ADone: Boolean) of object;
  TcxGridRecordEvent = procedure(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord) of object;

  TcxGridDataControllerChange = (dccItemAdded, dccItemRemoved, dccIndexesChanged);

  TcxCustomGridTableView = class(TcxCustomGridView,
    IcxFilterControl,
    IcxNavigator,
    IcxNavigatorRecordPosition,
    IcxDataControllerConditionalFormattingProviderOwner)
  private
    FAssigningGroupedItems: TcxGridOpenTableItemList;
    FAssigningSortedItems: TcxGridOpenTableItemList;
    FConditionalFormattingProvider: TcxGridConditionalFormattingProvider;
    FCopyToClipboardItems: TList;
    FCopyToClipboardStr: string;
    FDateTimeHandling: TcxCustomGridTableDateTimeHandling;
    FDontMakeMasterRecordVisible: Boolean;
    FFilterableItems: TList;
    FFilterBox: TcxGridFilterBox;
    FFiltering: TcxCustomGridTableFiltering;
    FFindPanel: TcxGridFindPanelOptions;
    FHighlightIncrementalFilteringText: Boolean;
    FIgnorePropertiesChanges: Boolean;
    FInDataControlFocusing: Boolean;
    FIsAfterAssigningItems: Boolean;
    FAssigningItemsLockCount: Integer;
    FItems: TList;
    FNavigator: TcxGridViewNavigator;
    FNavigatorNotifier: TcxNavigatorControlNotifier;
    FNextID: Integer;
    FOptionsCustomize: TcxCustomGridTableOptionsCustomize;
    FRestoringFilterActive: Boolean;
    FRestoringFilterAttributes: TcxGridViewRestoringFilterAttributes;
    FRestoringFilterData: AnsiString;
    FRestoringFilterMRUItems: TStringList;
    FRestoringItems: TcxGridOpenTableItemList;
    FVisibleItems: TList;
    FOnCanFocusRecord: TcxGridAllowRecordOperationEvent;
    FOnCanSelectRecord: TcxGridAllowRecordOperationEvent;
    FOnCellClick: TcxGridCellClickEvent;
    FOnCellDblClick: TcxGridCellClickEvent;
    FOnCustomDrawCell: TcxGridTableDataCellCustomDrawEvent;
    FOnCustomDrawPartBackground: TcxGridPartCustomDrawBackgroundEvent;
    FOnEditing: TcxGridEditingEvent;
    FOnEditChanged: TcxGridCustomTableItemEvent;
    FOnEditDblClick: TcxGridEditDblClickEvent;
    FOnEditKeyDown: TcxGridEditKeyEvent;
    FOnEditKeyPress: TcxGridEditKeyPressEvent;
    FOnEditKeyUp: TcxGridEditKeyEvent;
    FOnEditValueChanged: TcxGridCustomTableItemEvent;
    FOnFilterControlDialogShow: TNotifyEvent;
    FOnFilterCustomization: TcxGridFilterCustomizationEvent;
    FOnFilterDialogShow: TcxGridFilterDialogShowEvent;
    FOnFindPanelVisibilityChanged: TcxGridFindPanelVisibilityChangedEvent;
    FOnFocusedItemChanged: TcxGridFocusedItemChangedEvent;
    FOnFocusedRecordChanged: TcxGridFocusedRecordChangedEvent;
    FOnGetCellHeight: TcxGridGetCellHeightEvent;
    FOnGetDragDropText: TcxGridGetDragDropTextEvent;
    FOnInitFilteringDateRanges: TcxGridInitDateRangesEvent;
    FOnInitGroupingDateRanges: TcxGridInitDateRangesEvent;
    FOnInitEdit: TcxGridInitEditEvent;
    FOnInitEditValue: TcxGridInitEditValueEvent;
    FOnSelectionChanged: TcxGridCustomTableViewEvent;
    FOnTopRecordIndexChanged: TNotifyEvent;
    FOnUpdateEdit: TcxGridInitEditEvent;
    FSubClassEvents: TNotifyEvent;

    function GetBackgroundBitmaps: TcxCustomGridTableBackgroundBitmaps;
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
    function GetController: TcxCustomGridTableController; inline;
    function GetFilterableItem(Index: Integer): TcxCustomGridTableItem;
    function GetFilterableItemCount: Integer;
    function GetGroupedItem(Index: Integer): TcxCustomGridTableItem;
    function GetGroupedItemCount: Integer;
    function GetInternalConditionalFormattingProvider: TcxGridConditionalFormattingProvider;
    function GetItem(Index: Integer): TcxCustomGridTableItem; inline;
    function GetItemCount: Integer; inline;
    function GetMasterGridRecord: TcxCustomGridRecord;
    function GetNavigatorButtons: TcxNavigatorControlButtons;
    function GetOptionsBehavior: TcxCustomGridTableOptionsBehavior; inline;
    function GetOptionsData: TcxCustomGridTableOptionsData; inline;
    function GetOptionsSelection: TcxCustomGridTableOptionsSelection; inline;
    function GetOptionsView: TcxCustomGridTableOptionsView; inline;
    function GetPainter: TcxCustomGridTablePainter; inline;
    function GetPatternGridView: TcxCustomGridTableView;
    function GetNavigatorButtonsControl: IcxNavigator;
    function GetSortedItem(Index: Integer): TcxCustomGridTableItem;
    function GetSortedItemCount: Integer;
    function GetStyles: TcxCustomGridTableViewStyles;
    function GetViewData: TcxCustomGridTableViewData;
    function GetViewInfo: TcxCustomGridTableViewInfo;
    function GetVisibleItem(Index: Integer): TcxCustomGridTableItem;
    function GetVisibleItemCount: Integer;
    procedure SetBackgroundBitmaps(Value: TcxCustomGridTableBackgroundBitmaps);
    procedure SetDateTimeHandling(Value: TcxCustomGridTableDateTimeHandling);
    procedure SetFilterBox(Value: TcxGridFilterBox);
    procedure SetFiltering(Value: TcxCustomGridTableFiltering);
    procedure SetFindPanel(AValue: TcxGridFindPanelOptions);
    procedure SetItem(Index: Integer; Value: TcxCustomGridTableItem);
    procedure SetNavigator(Value: TcxGridViewNavigator);
    procedure SetNavigatorButtons(Value: TcxNavigatorControlButtons);
    procedure SetOnCanFocusRecord(Value: TcxGridAllowRecordOperationEvent);
    procedure SetOnCanSelectRecord(Value: TcxGridAllowRecordOperationEvent);
    procedure SetOnCellClick(Value: TcxGridCellClickEvent);
    procedure SetOnCellDblClick(Value: TcxGridCellClickEvent);
    procedure SetOnCustomDrawCell(Value: TcxGridTableDataCellCustomDrawEvent);
    procedure SetOnCustomDrawPartBackground(Value: TcxGridPartCustomDrawBackgroundEvent);
    procedure SetOnEditChanged(Value: TcxGridCustomTableItemEvent);
    procedure SetOnEditDblClick(Value: TcxGridEditDblClickEvent);
    procedure SetOnEditing(Value: TcxGridEditingEvent);
    procedure SetOnEditKeyDown(Value: TcxGridEditKeyEvent);
    procedure SetOnEditKeyPress(Value: TcxGridEditKeyPressEvent);
    procedure SetOnEditKeyUp(Value: TcxGridEditKeyEvent);
    procedure SetOnEditValueChanged(Value: TcxGridCustomTableItemEvent);
    procedure SetOnFilterControlDialogShow(Value: TNotifyEvent);
    procedure SetOnFilterCustomization(Value: TcxGridFilterCustomizationEvent);
    procedure SetOnFilterDialogShow(Value: TcxGridFilterDialogShowEvent);
    procedure SetOnFindPanelVisibilityChanged(Value: TcxGridFindPanelVisibilityChangedEvent);
    procedure SetOnFocusedItemChanged(Value: TcxGridFocusedItemChangedEvent);
    procedure SetOnFocusedRecordChanged(Value: TcxGridFocusedRecordChangedEvent);
    procedure SetOnGetCellHeight(Value: TcxGridGetCellHeightEvent);
    procedure SetOnGetDragDropText(Value: TcxGridGetDragDropTextEvent);
    procedure SetOnInitFilteringDateRanges(Value: TcxGridInitDateRangesEvent);
    procedure SetOnInitGroupingDateRanges(Value: TcxGridInitDateRangesEvent);
    procedure SetOnInitEdit(Value: TcxGridInitEditEvent);
    procedure SetOnInitEditValue(Value: TcxGridInitEditValueEvent);
    procedure SetOnSelectionChanged(Value: TcxGridCustomTableViewEvent);
    procedure SetOnTopRecordIndexChanged(Value: TNotifyEvent);
    procedure SetOnUpdateEdit(Value: TcxGridInitEditEvent);
    procedure SetOptionsBehavior(Value: TcxCustomGridTableOptionsBehavior);
    procedure SetOptionsCustomize(Value: TcxCustomGridTableOptionsCustomize);
    procedure SetOptionsData(Value: TcxCustomGridTableOptionsData);
    procedure SetOptionsSelection(Value: TcxCustomGridTableOptionsSelection);
    procedure SetOptionsView(Value: TcxCustomGridTableOptionsView);
    procedure SetStyles(Value: TcxCustomGridTableViewStyles);

    procedure CopyForEachRowProc(ARowIndex: Integer; ARowInfo: TcxRowInfo);
    function HasConditionalFormattingProvider: Boolean;
    procedure RefreshItemIndexes;
  protected
    procedure DefineProperties(Filer: TFiler); override;

    // IcxFilterControl
    function IcxFilterControl.GetCaption = GetFilterCaption;
    function IcxFilterControl.GetCount = GetFilterCount;
    function IcxFilterControl.GetCriteria = GetFilterCriteria;
    function IcxFilterControl.GetFieldName = GetFilterFieldName;
    function IcxFilterControl.GetItemLink = GetFilterItemLink;
    function IcxFilterControl.GetItemLinkID = GetFilterItemLinkID;
    function IcxFilterControl.GetItemLinkName = GetFilterItemLinkName;
    function IcxFilterControl.GetProperties = GetFilterProperties;
    function IcxFilterControl.GetValueType = GetFilterValueType;
    function GetFilterCaption(Index: Integer): string;
    function GetFilterCount: Integer;
    function GetFilterCriteria: TcxFilterCriteria;
    function GetFilterFieldName(Index: Integer): string;
    function GetFilterItemLink(Index: Integer): TObject;
    function GetFilterItemLinkID(Index: Integer): Integer;
    function GetFilterItemLinkName(Index: Integer): string;
    function GetFilterProperties(Index: Integer): TcxCustomEditProperties;
    function GetFilterValueType(Index: Integer): cxDataStorage.TcxValueTypeClass;
    // IcxNavigator
    function IcxNavigator.IsActive = NavigatorIsActive;
    function IcxNavigator.IsBof = NavigatorIsBof;
    function IcxNavigator.IsEof = NavigatorIsEof;
    function IcxNavigator.CanAppend = NavigatorCanAppend;
    function IcxNavigator.CanEdit = NavigatorCanEdit;
    function IcxNavigator.CanDelete = NavigatorCanDelete;
    function IcxNavigator.CanInsert = NavigatorCanInsert;
    function IcxNavigator.IsEditing = NavigatorIsEditing;
    procedure IcxNavigator.ClearBookmark = NavigatorClearBookmark;
    function IcxNavigator.IsBookmarkAvailable = NavigatorIsBookmarkAvailable;
    procedure IcxNavigator.DoAction = NavigatorDoAction;
    function IcxNavigator.GetNotifier = NavigatorGetNotifier;
    function IcxNavigator.IsActionSupported = NavigatorIsActionSupported;
    function NavigatorIsActive: Boolean;
    function NavigatorIsBof: Boolean;
    function NavigatorIsEof: Boolean;
    function NavigatorCanAppend: Boolean;
    function NavigatorCanEdit: Boolean;
    function NavigatorCanDelete: Boolean;
    function NavigatorCanInsert: Boolean;
    function NavigatorIsEditing: Boolean;
    procedure NavigatorClearBookmark;
    function NavigatorIsBookmarkAvailable: Boolean;
    procedure NavigatorDoAction(AButtonIndex: Integer);
    function NavigatorGetNotifier: TcxNavigatorControlNotifier;
    function NavigatorIsActionSupported(AButtonIndex: Integer): Boolean;
    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider; virtual;
    // IcxNavigatorRecordPosition
    function IcxNavigatorRecordPosition.GetRecordCount = NavigatorGetRecordCount;
    function IcxNavigatorRecordPosition.GetRecordIndex = NavigatorGetRecordIndex;
    function NavigatorGetRecordCount: Integer;
    function NavigatorGetRecordIndex: Integer;
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IcxStoredParent
    function CreateStoredObject(const AObjectName, AClassName: string): TObject; override;
    procedure GetStoredChildren(AChildren: TStringList); override;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    procedure BeforeEditLayout(ALayoutView: TcxCustomGridView); override;
    function GetSystemSizeScrollBars: TcxScrollStyle; override;
    function HasLayoutCustomizationForm: Boolean; override;
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    procedure ApplyRestoredFilter; virtual;
    procedure BeforeRestoring; override;
    procedure AfterRestoring; override;
    procedure ReadState(Reader: TReader); override;
    procedure Loaded; override;
    property RestoringItems: TcxGridOpenTableItemList read FRestoringItems;

    procedure AssignEditingRecord; virtual;
    procedure BeginAssignItems;
    procedure DoAfterAssignItems; virtual;
    procedure DoBeforeAssignItems; virtual;
    procedure DoItemsAssigned; virtual;
    procedure DoStylesChanged; override;
    procedure EndAssignItems;
    function IsAssigningItems: Boolean;
    property AssigningGroupedItems: TcxGridOpenTableItemList read FAssigningGroupedItems;
    property AssigningSortedItems: TcxGridOpenTableItemList read FAssigningSortedItems;
    property IsAfterAssigningItems: Boolean read FIsAfterAssigningItems;
    function CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean; virtual;
    function CanSelectRecord(ARecordIndex: Integer): Boolean; virtual;
    function CanTabStop: Boolean; override;
    procedure DetailVisibleChanged(ADetailLevel: TComponent; APrevVisibleDetailCount, AVisibleDetailCount: Integer); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    function FindItemByObjectName(const AObjectName: string): TcxCustomGridTableItem; virtual;
    procedure FindPanelVisibilityChanged(AVisible: Boolean); virtual;
    procedure FocusDataControl(AItemIndex: Integer; var ADone: Boolean);
    procedure FocusEdit(AItemIndex: Integer; var ADone: Boolean); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetDefaultActiveDetailIndex: Integer; virtual;
    function GetEditAutoHeight: TcxInplaceEditAutoHeight; virtual;
    procedure GetFakeComponentLinks(AList: TList); override;
    function GetIsControlFocused: Boolean; override;
    procedure GetItemsListForClipboard(AItems: TList; ACopyAll: Boolean); virtual;
    function GetItemSortByDisplayText(AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource;
    procedure GetVisibleItemsList(AItems: TList); virtual;
    function HasCustomDrawCell: Boolean;
    function HasCustomDrawPartBackground: Boolean;
    function HasPixelScrollingMaster: Boolean;
    procedure Init; override;
    function IsDetailVisible(AGridView: TcxCustomGridView): Boolean; override;
    function IsEqualScrollSizeRecords: Boolean; virtual;
    function IsRecordHeightDependsOnData: Boolean; virtual;
    function IsRecordHeightDependsOnFocus: Boolean; virtual;
    function IsRecordPixelScrolling: Boolean; override;
    procedure LoadingComplete; override;
    procedure Offset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;
    function SpaceForHorizontalScrollbarNeeded: Boolean; override;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
    procedure UpdateData(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateDataController(AChange: TcxGridDataControllerChange; AItem: TcxCustomGridTableItem = nil);
    procedure UpdateFocusedRecord(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateRecord; virtual;
    procedure ValidateEditVisibility; virtual;

    procedure CreateHandlers; override;
    procedure DestroyHandlers; override;
    procedure CreateOptions; override;
    procedure DestroyOptions; override;

    function CreateFindPanel: TcxGridFindPanelOptions; virtual;

    procedure AddItem(AItem: TcxCustomGridTableItem); virtual;
    procedure RemoveItem(AItem: TcxCustomGridTableItem); virtual;
    procedure AssignVisibleItemsIndexes;
    procedure ChangeItemIndex(AItem: TcxCustomGridTableItem; Value: Integer); virtual;
    procedure CheckItemVisibles;
    procedure SaveItemVisibles;
    procedure ItemIndexChanged(AItem: TcxCustomGridTableItem; AOldIndex: Integer); virtual;
    procedure ItemVisibilityChanged(AItem: TcxCustomGridTableItem; Value: Boolean); virtual;
    procedure RefreshVisibleItemsList; virtual;

    function GetItemClass: TcxCustomGridTableItemClass; virtual; abstract;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass; virtual;
    function GetNextID: Integer;
    procedure ReleaseID(AID: Integer);

    procedure AfterDestroyingLockedStateImage; override;
    procedure BeforeCreatingLockedStateImage; override;

    procedure Deactivate; override;
    procedure DestroyingSiteHandle; override;
    procedure DataChanged; virtual;
    procedure DataLayoutChanged; virtual;
    function DoCellClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState): Boolean; virtual;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState): Boolean; virtual;
    function DoEditing(AItem: TcxCustomGridTableItem): Boolean; virtual;
    procedure DoTopRecordIndexChanged; virtual;
    procedure DoUnlockNotification(AInfo: TcxUpdateControlInfo); override;
    procedure FilterChanged; virtual;
    procedure FocusedItemChanged(APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem); virtual;
    procedure FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); virtual;
    procedure GroupingChanging; virtual;
    procedure ItemCaptionChanged(AItem: TcxCustomGridTableItem); virtual;
    procedure ItemValueTypeClassChanged(AItemIndex: Integer); virtual;
    function IsLongFilterOperation: Boolean;
    procedure LookAndFeelChanged; override;
    function NeedChangeLayoutOnSelectionChanged(AInfo: TcxSelectionChangedInfo): Boolean; virtual;
    procedure NotifySelectionChanged(AInfo: TcxSelectionChangedInfo = nil);
    procedure RecalculateDefaultWidths;
    procedure RecordChanged(ARecordIndex: Integer); virtual;
    procedure RecordCountChanged; virtual;
    procedure RefreshFilterableItemsList;
    procedure RefreshNavigators;
    procedure SearchChanged; virtual;
    procedure SelectionChanged(AInfo: TcxSelectionChangedInfo); virtual;

    function CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
      ACellViewInfo: TcxGridTableCellViewInfo): Boolean; virtual;
    function CanMakeMasterGridRecordVisible: Boolean;
    function DrawDataCellSelected(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      AUseViewInfo: Boolean = False; ACellViewInfo: TcxGridTableCellViewInfo = nil): Boolean; virtual;
    function DrawRecordActive(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function DrawRecordFocused(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function DrawRecordSelected(ARecord: TcxCustomGridRecord): Boolean; virtual;
    function DrawSelection: Boolean; virtual;

    function DoCanFocusRecord(ARecord: TcxCustomGridRecord): Boolean; virtual;
    procedure DoCustomDrawCell(ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean); virtual;
    procedure DoCustomDrawPartBackground(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo;
      var ADone: Boolean); virtual;
    procedure DoEditChanged(AItem: TcxCustomGridTableItem); virtual;
    procedure DoEditDblClick(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit); virtual;
    procedure DoEditKeyDown(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit;
      var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyPress(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit;
      var Key: Char); virtual;
    procedure DoEditKeyUp(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit;
      var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditValueChanged(AItem: TcxCustomGridTableItem); virtual;
    function DoFilterCustomization: Boolean; virtual;
    function DoFilterDialogShow(AItem: TcxCustomGridTableItem): Boolean; virtual;
    procedure DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); virtual;
    procedure DoGetCellHeight(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer); virtual;
    function DoGetDragDropText(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem): string; virtual;
    procedure DoInitEdit(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit); virtual;
    procedure DoInitEditValue(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var AValue: TcxEditValue); virtual;
    procedure DoInitFilteringDateRanges(AItem: TcxCustomGridTableItem); virtual;
    procedure DoInitGroupingDateRanges(AItem: TcxCustomGridTableItem); virtual;
    procedure DoMakeMasterGridRecordVisible; virtual;
    procedure DoUpdateEdit(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit); virtual;
    procedure DoSelectionChanged; virtual;
    function HasCustomProperties: Boolean;
    function HasInitFilteringDateRangesHandler: Boolean;
    function HasInitGroupingDateRangesHandler: Boolean;
    function IsGetCellHeightAssigned: Boolean;
    function NeedHighlightFindFilterText: Boolean;

    function CreateConditionalFormattingProvider: TcxGridConditionalFormattingProvider; virtual;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoCacheClass: TcxCustomGridViewInfoCacheClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; override;
    function GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass; virtual;
    function GetFilterBoxClass: TcxGridFilterBoxClass; virtual;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; virtual;
    function GetNavigatorClass: TcxGridViewNavigatorClass; virtual;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; virtual;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;

    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; virtual;
    function GetSummaryItemClass: TcxDataSummaryItemClass; virtual;

    property BackgroundBitmaps: TcxCustomGridTableBackgroundBitmaps read GetBackgroundBitmaps write SetBackgroundBitmaps;
    property ConditionalFormattingProvider: TcxGridConditionalFormattingProvider read GetInternalConditionalFormattingProvider;
    property DontMakeMasterRecordVisible: Boolean read FDontMakeMasterRecordVisible write FDontMakeMasterRecordVisible;
    property FilterableItemCount: Integer read GetFilterableItemCount;
    property FilterableItems[Index: Integer]: TcxCustomGridTableItem read GetFilterableItem;
    property HighlightIncrementalFilteringText: Boolean read FHighlightIncrementalFilteringText write FHighlightIncrementalFilteringText;
    property IgnorePropertiesChanges: Boolean read FIgnorePropertiesChanges write FIgnorePropertiesChanges;
    property ItemsList: TList read FItems;
    property NavigatorNotifier: TcxNavigatorControlNotifier read FNavigatorNotifier;
    property NextID: Integer read FNextID;
    property VisibleItemsList: TList read FVisibleItems;

    property OnInitGroupingDateRanges: TcxGridInitDateRangesEvent read FOnInitGroupingDateRanges write SetOnInitGroupingDateRanges;
  public
    destructor Destroy; override;

    procedure ApplyBestFit(AItem: TcxCustomGridTableItem = nil; ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False); virtual;
    procedure ClearItems;
    procedure CopyToClipboard(ACopyAll: Boolean);
    function CreateItem: TcxCustomGridTableItem;
    function FindItemByID(AID: Integer): TcxCustomGridTableItem;
    function FindItemByName(const AName: string): TcxCustomGridTableItem;
    function FindItemByTag(ATag: TcxTag): TcxCustomGridTableItem;
    function IndexOfItem(AItem: TcxCustomGridTableItem): Integer;
    procedure MakeMasterGridRecordVisible;
    procedure RestoreDefaults; override;

    procedure BeginBestFitUpdate;
    procedure EndBestFitUpdate;
    procedure BeginFilteringUpdate;
    procedure EndFilteringUpdate;
    procedure BeginGroupingUpdate;
    procedure EndGroupingUpdate;
    procedure BeginSortingUpdate;
    procedure EndSortingUpdate;

    function IsEqualHeightRecords: Boolean; virtual;

    // for extended lookup edit
    class function CanBeLookupList: Boolean; virtual;

    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
    property Controller: TcxCustomGridTableController read GetController;
    property DateTimeHandling: TcxCustomGridTableDateTimeHandling read FDateTimeHandling write SetDateTimeHandling;
    property Filtering: TcxCustomGridTableFiltering read FFiltering write SetFiltering;
    property GroupedItemCount: Integer read GetGroupedItemCount;
    property GroupedItems[Index: Integer]: TcxCustomGridTableItem read GetGroupedItem;
    property InDataControlFocusing: Boolean read FInDataControlFocusing;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TcxCustomGridTableItem read GetItem write SetItem;
    property MasterGridRecord: TcxCustomGridRecord read GetMasterGridRecord;
    property OptionsBehavior: TcxCustomGridTableOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxCustomGridTableOptionsCustomize read FOptionsCustomize write SetOptionsCustomize;
    property OptionsData: TcxCustomGridTableOptionsData read GetOptionsData write SetOptionsData;
    property OptionsSelection: TcxCustomGridTableOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxCustomGridTableOptionsView read GetOptionsView write SetOptionsView;
    property Painter: TcxCustomGridTablePainter read GetPainter;
    property PatternGridView: TcxCustomGridTableView read GetPatternGridView;
    property SortedItemCount: Integer read GetSortedItemCount;
    property SortedItems[Index: Integer]: TcxCustomGridTableItem read GetSortedItem;
    property Styles: TcxCustomGridTableViewStyles read GetStyles write SetStyles;
    property ViewData: TcxCustomGridTableViewData read GetViewData;
    property ViewInfo: TcxCustomGridTableViewInfo read GetViewInfo;
    property VisibleItemCount: Integer read GetVisibleItemCount;
    property VisibleItems[Index: Integer]: TcxCustomGridTableItem read GetVisibleItem;
  published
    property Navigator: TcxGridViewNavigator read FNavigator write SetNavigator;
    property FilterBox: TcxGridFilterBox read FFilterBox write SetFilterBox;
    property FindPanel: TcxGridFindPanelOptions read FFindPanel write SetFindPanel;

    property NavigatorButtons: TcxNavigatorControlButtons read GetNavigatorButtons write SetNavigatorButtons stored False;
    property NavigatorEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property OnCanFocusRecord: TcxGridAllowRecordOperationEvent read FOnCanFocusRecord write SetOnCanFocusRecord;
    property OnCanSelectRecord: TcxGridAllowRecordOperationEvent read FOnCanSelectRecord write SetOnCanSelectRecord;
    property OnCellClick: TcxGridCellClickEvent read FOnCellClick write SetOnCellClick;
    property OnCellDblClick: TcxGridCellClickEvent read FOnCellDblClick write SetOnCellDblClick;
    property OnCustomDrawCell: TcxGridTableDataCellCustomDrawEvent read FOnCustomDrawCell write SetOnCustomDrawCell;
    property OnCustomDrawPartBackground: TcxGridPartCustomDrawBackgroundEvent
      read FOnCustomDrawPartBackground write SetOnCustomDrawPartBackground;
    property OnEditing: TcxGridEditingEvent read FOnEditing write SetOnEditing;
    property OnEditChanged: TcxGridCustomTableItemEvent read FOnEditChanged write SetOnEditChanged;
    property OnEditDblClick: TcxGridEditDblClickEvent read FOnEditDblClick write SetOnEditDblClick;
    property OnEditKeyDown: TcxGridEditKeyEvent read FOnEditKeyDown write SetOnEditKeyDown;
    property OnEditKeyPress: TcxGridEditKeyPressEvent read FOnEditKeyPress write SetOnEditKeyPress;
    property OnEditKeyUp: TcxGridEditKeyEvent read FOnEditKeyUp write SetOnEditKeyUp;
    property OnEditValueChanged: TcxGridCustomTableItemEvent read FOnEditValueChanged write SetOnEditValueChanged;
    property OnFilterControlDialogShow: TNotifyEvent read FOnFilterControlDialogShow
      write SetOnFilterControlDialogShow;
    property OnFilterCustomization: TcxGridFilterCustomizationEvent read FOnFilterCustomization
      write SetOnFilterCustomization;
    property OnFilterDialogShow: TcxGridFilterDialogShowEvent read FOnFilterDialogShow
      write SetOnFilterDialogShow;
    property OnFindPanelVisibilityChanged: TcxGridFindPanelVisibilityChangedEvent read FOnFindPanelVisibilityChanged
      write SetOnFindPanelVisibilityChanged;
    property OnFocusedItemChanged: TcxGridFocusedItemChangedEvent read FOnFocusedItemChanged
      write SetOnFocusedItemChanged;
    property OnFocusedRecordChanged: TcxGridFocusedRecordChangedEvent read FOnFocusedRecordChanged
      write SetOnFocusedRecordChanged;
    property OnGetCellHeight: TcxGridGetCellHeightEvent read FOnGetCellHeight write SetOnGetCellHeight;
    property OnGetDragDropText: TcxGridGetDragDropTextEvent read FOnGetDragDropText write SetOnGetDragDropText;
    property OnInitEdit: TcxGridInitEditEvent read FOnInitEdit write SetOnInitEdit;
    property OnInitEditValue: TcxGridInitEditValueEvent read FOnInitEditValue write SetOnInitEditValue;
    property OnInitFilteringDateRanges: TcxGridInitDateRangesEvent read FOnInitFilteringDateRanges write SetOnInitFilteringDateRanges;
    property OnInitStoredObject;
    property OnSelectionChanged: TcxGridCustomTableViewEvent read FOnSelectionChanged write SetOnSelectionChanged;
    property OnTopRecordIndexChanged: TNotifyEvent read FOnTopRecordIndexChanged write SetOnTopRecordIndexChanged;
    property OnUpdateEdit: TcxGridInitEditEvent read FOnUpdateEdit write SetOnUpdateEdit;
  end;

  { TcxCustomGridTableControllerAccess }

  TcxCustomGridTableControllerAccess = class
  public
    class procedure FocusNextPage(AInstance: TcxCustomGridTableController; ASyncSelection: Boolean); static;
    class procedure FocusPrevPage(AInstance: TcxCustomGridTableController; ASyncSelection: Boolean); static;
  end;

  { TcxCustomGridTableItemAccess }

  TcxCustomGridTableItemAccess = class
  public
    class function CanFilter(AInstance: TcxCustomGridTableItem; AVisually: Boolean): Boolean; static;
    class function CanGroup(AInstance: TcxCustomGridTableItem): Boolean; static;
    class function CanHide(AInstance: TcxCustomGridTableItem): Boolean; static;
    class function CanHorzSize(AInstance: TcxCustomGridTableItem): Boolean; static;
    class function CanSort(AInstance: TcxCustomGridTableItem): Boolean; static;
    class procedure CheckWidthValue(AInstance: TcxCustomGridTableItem; var Value: Integer); static;
    class procedure DoGetDataText(AInstance: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string); static;
    class procedure DoGetDisplayText(AInstance: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string); static;
    class function GetGroupIndex(AInstance: TcxCustomGridTableItem): Integer; static;

    class function GetFilterCaption(AInstance: TcxCustomGridTableItem): string; static;
  end;

  { TcxCustomGridTableOptionsBehaviorAccess }

  TcxCustomGridTableOptionsBehaviorAccess = class
  public
    class function GetPullFocusing(AInstance: TcxCustomGridTableOptionsBehavior): Boolean; static;
    class procedure SetPullFocusing(AInstance: TcxCustomGridTableOptionsBehavior; Value: Boolean); static;
  end;

  { TcxCustomGridTableOptionsViewAccess }

  TcxCustomGridTableOptionsViewAccess = class
  public
    class function GetCellAutoHeight(AInstance: TcxCustomGridTableOptionsView): Boolean; static;
  end;

  { TcxCustomGridTableViewAccess }

  TcxCustomGridTableViewAccess = class
  public
    class function CanSelectRecord(AInstance: TcxCustomGridTableView; ARecordIndex: Integer): Boolean; static;
    class procedure FilterChanged(AInstance: TcxCustomGridTableView); static;
    class function FindItemByObjectName(AInstance: TcxCustomGridTableView; const AObjectName: string): TcxCustomGridTableItem; static;
    class procedure FocusEdit(AInstance: TcxCustomGridTableView; AItemIndex: Integer; var ADone: Boolean); static;
    class procedure FocusDataControl(AInstance: TcxCustomGridTableView; AItemIndex: Integer; var ADone: Boolean); static;
    class function GetDefaultActiveDetailIndex(AInstance: TcxCustomGridTableView): Integer; static;
    class function GetItemClass(AInstance: TcxCustomGridTableView): TcxCustomGridTableItemClass; static;
    class function GetItemSortByDisplayText(AInstance: TcxCustomGridTableView; AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean; static;
    class function GetItemValueSource(AInstance: TcxCustomGridTableView; AItemIndex: Integer): TcxDataEditValueSource; static;
    class function GetSummaryGroupItemLinkClass(AInstance: TcxCustomGridTableView): TcxDataSummaryGroupItemLinkClass; static;
    class function GetSummaryItemClass(AInstance: TcxCustomGridTableView): TcxDataSummaryItemClass; static;
    class function IsEqualHeightRecords(AInstance: TcxCustomGridTableView): Boolean; static;
    class function IsGetCellHeightAssigned(AInstance: TcxCustomGridTableView): Boolean; static;
    class procedure ItemValueTypeClassChanged(AInstance: TcxCustomGridTableView; AItemIndex: Integer); static;
    class procedure RefreshNavigators(AInstance: TcxCustomGridTableView); static;
    class procedure UpdateRecord(AInstance: TcxCustomGridTableView); static;
    class procedure BeginLockedStatePaintOnPost(AInstance: TcxCustomGridTableView); static;
    class procedure EndLockedStatePaint(AInstance: TcxCustomGridTableView); static;
  end;

  { TcxCustomGridTableViewInfoAccess }

  TcxCustomGridTableViewInfoAccess = class
  public
    class function GetDefaultGridModeBufferCount(AInstance: TcxCustomGridTableViewInfo): Integer; static;
  end;

function GetViewItemName(AView: TcxCustomGridTableView): string;
function CreateViewItem(AView: TcxCustomGridTableView): TcxCustomGridTableItem;

implementation

uses
  Types, DateUtils, Math, SysUtils, TypInfo, RTLConsts, Clipbrd,
  dxTypeHelpers, cxLibraryConsts, cxFormats, cxVariants, cxDrawTextUtils,
  cxScrollBar, cxEditUtils, cxFilterDialog, cxFilterControlDialog,
  cxEditDataRegisteredRepositoryItems, cxFilterConsts,
  cxGrid, cxGridStrs, cxGridLevel, dxDPIAwareUtils, cxLibraryStrs;

const
  PullFocusingScrollingTimeInterval = 50;
  // Copy to text format
  ColumnSeparator = #9;

  FilterButtonsFirstOffset = 4;
  FilterButtonsOffset = 4;
  FilterTextOffset = 3;

  FindPanelButtonsFirstOffset = 12;
  FindPanelButtonsOffset = 4;

  cxGridFindPanelEditMinWidth = 52;
  cxGridFindPanelEditMaxWidth = 348;
  cxGridFindPanelButtonMinWidth = 60;
  cxGridFindPanelButtonMinHeight = 22;

  DragDropTextAreaOffset = 25;
  DragDropTextBorderSize = 3;
  DragDropTextIndent = DragDropTextBorderSize + cxGridCellTextOffset;

  ScrollTimeInterval = 35;

  FilterMRUItemBaseName = 'FilterMRUItem';

  CustomizationPopupCheckListBoxColumnOffset = 10;
  CustomizationPopupCheckListBoxScrollZoneWidth = 15;
  CustomizationPopupCheckListBoxScrollTimeInterval = 400;

  UnassignedPixelScrollOffset = MaxInt;

type
  TControlAccess = class(TControl);
  TcxControlAccess = class(TcxControl);
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomGridAccess = class(TcxCustomGrid);
  TcxCustomGridViewAccess = class(TcxCustomGridView);
  TcxGridLevelAccess = class(TcxGridLevel);
  TcxGridSiteAccess = class(TcxGridSite);
  TcxEditValidateInfoAccess = class(TcxEditValidateInfo);
  TcxCustomEditViewInfoAccess = class(TcxCustomEditViewInfo);
  TcxCustomMRUEditPropertiesAccess = class(TcxCustomMRUEditProperties);
  TcxDataControllerConditionalFormattingAccess = class(TcxDataControllerConditionalFormatting);
  TdxCustomCheckListBoxAccess = class(TdxCustomCheckListBox);
  TdxQuickCustomizationCustomControlAccess = class(TdxQuickCustomizationCustomControl);
  TcxCustomDataControllerAccess = class(TcxCustomDataController);
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);

function GetViewItemName(AView: TcxCustomGridTableView): string;
var
  I: Integer;
begin
  Result := TcxCustomGridTableViewAccess.GetItemClass(AView).ClassName;
  for I := Length(Result) downto 1 do
    if dxCharInSet(Result[I], ['A'..'Z']) then
    begin
      Delete(Result, 1, I - 1);
      Break;
    end;
end;

function CreateViewItem(AView: TcxCustomGridTableView): TcxCustomGridTableItem;
begin
  Result := AView.CreateItem;
  Result.Name := GetViewItemUniqueName(AView, Result, GetViewItemName(AView));
end;

function cxCustomGridTableControllerCanFocusItem(AOwner: TcxCustomGridTableView;
  AItemIndex: Integer; AData: TObject): Boolean;
begin
  Result := AOwner.VisibleItems[AItemIndex].CanFocus(TcxCustomGridRecord(AData));
end;

function cxCustomGridTableControllerCanFocusRecord(
  AOwner: TcxCustomGridTableView; AItemIndex: Integer;
  AData: TObject): Boolean;
begin
  Result := AOwner.ViewData.Records[AItemIndex].CanFocus;
end;

function cxCustomGridTableViewGetItem(ACaller: TComponent;
  Index: Integer): TComponent;
begin
  Result := TcxCustomGridTableView(ACaller).Items[Index];
end;

{ TcxGridDataChange }

procedure TcxGridDataChange.Execute;
begin
  with GridView as TcxCustomGridTableView do
  begin
    RecordCountChanged;
    //Controller.MakeFocusedRecordVisible;   removed because of group nodes expanding
  end;
end;

function TcxGridDataChange.IsLockable: Boolean;
begin
  Result := False;
end;

{ TcxGridRecordChange }

constructor TcxGridRecordChange.Create(AGridView: TcxCustomGridView;
  ARecord: TcxCustomGridRecord; ARecordIndex: Integer; AItem: TcxCustomGridTableItem = nil);
begin
  inherited Create(AGridView);
  FRecord := ARecord;
  FRecordIndex := ARecordIndex;
  FItem := AItem;
end;

function TcxGridRecordChange.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridRecordChange.GetRecordViewInfo: TcxCustomGridRecordViewInfo;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.GetRealItem(FRecord);
end;

procedure TcxGridRecordChange.Execute;
begin
  if (RecordViewInfo = nil) or (FItem <> nil) and not IsItemVisible or
    not GridView.Changeable then Exit;
  RecordViewInfo.Recalculate;
  GridView.ViewChanged(RecordViewInfo.GetBoundsForInvalidate(FItem));
end;

function TcxGridRecordChange.IsCompatibleWith(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsCompatibleWith(AChange) or
    ((AChange is TcxGridLayoutChange) or (AChange is TcxGridSizeChange) or (AChange is TcxGridDataChange)) and
    (TcxCustomGridViewChange(AChange).GridView <> nil) and (TcxCustomGridViewChange(AChange).GridView = GridView);
end;

function TcxGridRecordChange.IsEqual(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsEqual(AChange) and
    (GridRecord = TcxGridRecordChange(AChange).GridRecord) and
    (RecordIndex = TcxGridRecordChange(AChange).RecordIndex) and
    (Item = TcxGridRecordChange(AChange).Item);
end;

function TcxGridRecordChange.IsItemVisible: Boolean;
begin
  Result := (GridView.IndexOfItem(FItem) <> -1) and FItem.IsVisibleForRecordChange;// ActuallyVisible or FItem.SpecialVisible);
end;

{ TcxGridFocusedRecordChange }

constructor TcxGridFocusedRecordChange.Create(AGridView: TcxCustomGridView;
  APrevFocusedRecordIndex, AFocusedRecordIndex,
  APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
begin
  inherited Create(AGridView);
  FPrevFocusedRecordIndex := APrevFocusedRecordIndex;
  FFocusedRecordIndex := AFocusedRecordIndex;
  FPrevFocusedDataRecordIndex := APrevFocusedDataRecordIndex;
  FFocusedDataRecordIndex := AFocusedDataRecordIndex;
  FNewItemRecordFocusingChanged := ANewItemRecordFocusingChanged;
end;

function TcxGridFocusedRecordChange.CanExecuteWhenLocked: Boolean;
begin
  Result := False;
end;

procedure TcxGridFocusedRecordChange.Execute;
begin
  with GridView as TcxCustomGridTableView do
    //if Changeable then - should work with Visible = False (to show details)
    begin
      if DataController.IsGridMode then Controller.UpdateScrollBars;
      if (FFocusedRecordIndex <> FPrevFocusedRecordIndex) or FNewItemRecordFocusingChanged then
      begin
        Controller.PrevFocusedRecordIndex := FPrevFocusedRecordIndex;
        try
          Controller.MakeFocusedRecordVisible;
        finally
          Controller.PrevFocusedRecordIndex := -1;
        end;
      end;
      if IsRecordHeightDependsOnFocus then
        SizeChanged
      else
        with ViewData do
        begin
          if IsRecordIndexValid(FPrevFocusedRecordIndex) then
            Records[FPrevFocusedRecordIndex].Invalidate;
          if IsRecordIndexValid(FFocusedRecordIndex) then
            Records[FFocusedRecordIndex].Invalidate;
          if FNewItemRecordFocusingChanged and HasNewItemRecord then
            NewItemRecord.Invalidate;
        end;
      Controller.CheckEdit;
      Controller.EditingController.UpdateEditValue;
      DoFocusedRecordChanged(FPrevFocusedRecordIndex, FFocusedRecordIndex,
        FPrevFocusedDataRecordIndex, FFocusedDataRecordIndex,
        FNewItemRecordFocusingChanged);  // see 23172
    end;
end;

{ TcxGridDataController }

function TcxGridDataController.GetGridViewValue: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(GetGridView);
end;

procedure TcxGridDataController.ReadData(Stream: TStream);
var
  ASize: Integer;
begin
  Stream.Read(ASize, SizeOf(ASize));
  if FLoadedData = nil then
    FLoadedData := TMemoryStream.Create
  else
    FLoadedData.Clear;
  FLoadedData.CopyFrom(Stream, ASize);
  FLoadedData.Position := 0;
end;

procedure TcxGridDataController.WriteData(Stream: TStream);
var
  AStream: TMemoryStream;
  ASize: Integer;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    ASize := AStream.Size;
    Stream.Write(ASize, SizeOf(ASize));
    AStream.SaveToStream(Stream);
  finally
    AStream.Free;
  end;
end;

procedure TcxGridDataController.AssignData(ADataController: TcxCustomDataController);
var
  AStream: TMemoryStream;
begin
  if ADataController.IsProviderMode then
  begin
    CustomDataSource := nil;
    CustomDataSource := ADataController.CustomDataSource;
  end
  else
  begin
    AStream := TMemoryStream.Create;
    try
      ADataController.SaveToStream(AStream);
      AStream.Position := 0;
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TcxGridDataController.CreateAllItems(AMissingItemsOnly: Boolean);
begin
end;

procedure TcxGridDataController.DeleteAllItems;
begin
end;

procedure TcxGridDataController.GetFakeComponentLinks(AList: TList);
begin
end;

function TcxGridDataController.GetGridView: TcxCustomGridView;
begin
  Result := TcxCustomGridView(GetOwner);
end;

function TcxGridDataController.HasAllItems: Boolean;
begin
  Result := True;
end;

function TcxGridDataController.IsDataChangeable: Boolean;
begin
  Result := True;
end;

function TcxGridDataController.IsDataLinked: Boolean;
begin
  Result := True;
end;

function TcxGridDataController.SupportsCreateAllItems: Boolean;
begin
  Result := False;
end;

procedure TcxGridDataController.CheckGridModeBufferCount;
begin
end;

function TcxGridDataController.DoScroll(AForward: Boolean): Boolean;
begin
  Result := False;
end;

function TcxGridDataController.DoScrollPage(AForward: Boolean): Boolean;
begin
  Result := False;
end;

{function TcxGridDataController.GetFilterPropertyValue(const AName: string;
  var AValue: Variant): Boolean;
begin
  Result := False;
end;}

function TcxGridDataController.GetItemDataBindingClass: TcxGridItemDataBindingClass;
begin
  Result := TcxGridItemDataBinding;
end;

function TcxGridDataController.GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxGridDefaultValuesProvider;
end;

function TcxGridDataController.GetNavigatorIsBof: Boolean;
begin
  Result := GridView.Controller.IsStart;
end;

function TcxGridDataController.GetNavigatorIsEof: Boolean;
begin
  Result := GridView.Controller.IsFinish;
end;

function TcxGridDataController.GetScrollBarPos: Integer;
begin
  Result := -1;
end;

function TcxGridDataController.GetScrollBarRecordCount: Integer;
begin
  Result := -1;
end;

{function TcxGridDataController.SetFilterPropertyValue(const AName: string;
  const AValue: Variant): Boolean;
begin
  Result := False;
end;}

function TcxGridDataController.SetScrollBarPos(Value: Integer): Boolean;
begin
  Result := False;
end;

function TcxGridDataController.CanSelectRow(ARowIndex: Integer): Boolean;
begin
  Result := GridView.CanSelectRecord(ARowIndex);
end;

function TcxGridDataController.CompareByField(ARecordIndex1, ARecordIndex2: Integer;
  AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer;
begin
  if GridView.ViewData.NeedsCustomDataComparison(AField, AMode) then
    Result := GridView.ViewData.CustomCompareDataValues(AField,
      GetComparedValue(ARecordIndex1, AField), GetComparedValue(ARecordIndex2, AField), AMode)
  else
    Result := inherited CompareByField(ARecordIndex1, ARecordIndex2, AField, AMode);
end;

procedure TcxGridDataController.DefineProperties(Filer: TFiler);

  function HasData: Boolean;
  var
    AAncestor: TcxCustomDataController;
    AStream1, AStream2: TMemoryStream;
  begin
    if Filer.Ancestor = nil then
      Result := RecordCount <> 0
    else
    begin
      AAncestor := Filer.Ancestor as TcxCustomDataController;
      if (RecordCount = 0) and (AAncestor.RecordCount = 0) then
        Exit(False);
      AStream1 := TMemoryStream.Create;
      AStream2 := TMemoryStream.Create;
      try
        SaveToStream(AStream1);
        AAncestor.SaveToStream(AStream2);
        Result := not StreamsEqual(AStream1, AStream2);
      finally
        AStream2.Free;
        AStream1.Free;
      end;
    end;
  end;

begin
  inherited;
  if not IsProviderMode then
    Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure TcxGridDataController.DoValueTypeClassChanged(AItemIndex: Integer);
begin
  inherited;
  GridView.ItemValueTypeClassChanged(AItemIndex);
end;

procedure TcxGridDataController.FilterChanged;
begin
  inherited;
  GridView.FilterChanged;
end;

function TcxGridDataController.GetDefaultActiveRelationIndex: Integer;
begin
  Result := GridView.GetDefaultActiveDetailIndex;
end;

function TcxGridDataController.GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  if GridView.ViewData.HasCustomDataHandling(Fields[AItemIndex], doFiltering) then
    Result := GridView.ViewData.GetCustomDataDisplayText(ARecordIndex, AItemIndex, doFiltering)
  else
    Result := inherited GetFilterDisplayText(ARecordIndex, AItemIndex);
end;

{function TcxGridDataController.GetIncrementalSearchText(ARecordIndex, AItemIndex: Integer): string;
begin
  if not GridView.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetIncrementalSearchText(ARecordIndex, AItemIndex);
end;}

function TcxGridDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := TcxCustomGridTableItem(AItem).ID
  else
    Result := -1;
end;

function TcxGridDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

function TcxGridDataController.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := GridView.GetSummaryGroupItemLinkClass;
  if Result = nil then
    Result := inherited GetSummaryGroupItemLinkClass;
end;

function TcxGridDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := GridView.GetSummaryItemClass;
  if Result = nil then
    Result := inherited GetSummaryItemClass;
end;

procedure TcxGridDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridDataController.EndFullUpdate;
begin
  inherited;
  GridView.EndUpdate;
end;

function TcxGridDataController.CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
  ARecordIndex: Integer): TObject;
begin
  Result := TcxGridLevelAccess(ARelation.Item).CreateLinkObject(ARelation, ARecordIndex);
end;

procedure TcxGridDataController.FocusControl(AItemIndex: Integer; var Done: Boolean);
begin
  inherited;
  GridView.FocusDataControl(AItemIndex, Done);
end;

function TcxGridDataController.GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController;
begin
  if ALinkObject = nil then
    Result := nil
  else
    Result := TcxCustomGridView(ALinkObject).DataController;
end;

function TcxGridDataController.GetDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  if not GridView.ViewData.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetDisplayText(ARecordIndex, AItemIndex);
  GridView.Items[AItemIndex].DoGetDataText(ARecordIndex, Result);
end;

function TcxGridDataController.GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
begin
  Result := inherited GetFilterDataValue(ARecordIndex, AField);
  if GridView.ViewData.HasCustomDataHandling(AField, doFiltering) then
    Result := GridView.ViewData.GetCustomDataValue(AField, Result, doFiltering);
end;

function TcxGridDataController.GetFilterItemFieldCaption(AItem: TObject): string;
begin
  Result := TcxCustomGridTableItem(AItem).FilterCaption;
end;

function TcxGridDataController.GetItem(Index: Integer): TObject;
begin
  Result := GridView.Items[Index];
end;

function TcxGridDataController.GetItemSortByDisplayText(AItemIndex: Integer;
  ASortByDisplayText: Boolean): Boolean;
begin
  Result := GridView.GetItemSortByDisplayText(AItemIndex, ASortByDisplayText);
end;

function TcxGridDataController.GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := GridView.GetItemValueSource(AItemIndex);
end;

procedure TcxGridDataController.Loaded;
begin
  inherited;
  if FLoadedData <> nil then
  try
    if not IsProviderMode then
      LoadFromStream(FLoadedData);
  finally
    FreeAndNil(FLoadedData);
  end;
end;

procedure TcxGridDataController.UpdateData;
begin
  inherited;
  GridView.UpdateRecord;
end;

{ TcxGridFilterValueList }

procedure TcxGridFilterValueList.AddDateTimeAbsoluteFilters(ADateRange: TcxCustomGridDateRange;
  AIgnoreTime: Boolean);
var
  I: Integer;
  ADateTime: TDateTime;
  ADateRangeValue: Variant;
begin
  I := StartValueIndex;
  while I < Count do
  begin
    ADateTime := Items[I].Value;
    if ADateRange.Contains(ADateTime) then
    begin
      ADateRangeValue := ADateRange.GetRangeValue(ADateTime, AIgnoreTime);
      if FindItemByKind(fviSpecial, ADateRangeValue) = -1 then
      begin
        Add(fviSpecial, ADateRangeValue, ADateRange.GetDisplayText(ADateTime), True);
        Inc(I);
      end;
    end;
    Inc(I);
  end;
end;

procedure TcxGridFilterValueList.AddDateTimeAbsoluteFilters(AItem: TcxCustomGridTableItem);
var
  I: Integer;
begin
  AItem.InitFilteringDateRanges;
  for I := 0 to AItem.FilteringDateRanges.Count - 1 do
    AddDateTimeAbsoluteFilters(AItem.FilteringDateRanges[I], AItem.CanIgnoreTimeForFiltering);
end;

procedure TcxGridFilterValueList.AddDateTimeRelativeFilters(AItem: TcxCustomGridTableItem);
var
  AFilters: TcxGridDateTimeFilters;
  AFilter: TcxGridDateTimeFilter;
begin
  AFilters := AItem.GetDateTimeHandlingFilters;
  for AFilter := Low(AFilter) to High(AFilter) do
    if AFilter in AFilters then
      case AFilter of
        dtfRelativeDays:
          begin
            Add(fviSpecial, foYesterday, cxGetResourceString(@scxGridYesterday), True);
            Add(fviSpecial, foToday, cxGetResourceString(@scxGridToday), True);
            Add(fviSpecial, foTomorrow, cxGetResourceString(@scxGridTomorrow), True);
          end;
        dtfRelativeDayPeriods:
          begin
            Add(fviSpecial, foLast30Days, cxGetResourceString(@scxGridLast30Days), True);
            Add(fviSpecial, foLast14Days, cxGetResourceString(@scxGridLast14Days), True);
            Add(fviSpecial, foLast7Days, cxGetResourceString(@scxGridLast7Days), True);
            Add(fviSpecial, foNext7Days, cxGetResourceString(@scxGridNext7Days), True);
            Add(fviSpecial, foNext14Days, cxGetResourceString(@scxGridNext14Days), True);
            Add(fviSpecial, foNext30Days, cxGetResourceString(@scxGridNext30Days), True);
          end;
        dtfRelativeWeeks:
          begin
            Add(fviSpecial, foLastTwoWeeks, cxGetResourceString(@scxGridLastTwoWeeks), True);
            Add(fviSpecial, foLastWeek, cxGetResourceString(@scxGridLastWeek), True);
            Add(fviSpecial, foThisWeek, cxGetResourceString(@scxGridThisWeek), True);
            Add(fviSpecial, foNextWeek, cxGetResourceString(@scxGridNextWeek), True);
            Add(fviSpecial, foNextTwoWeeks, cxGetResourceString(@scxGridNextTwoWeeks), True);
          end;
        dtfRelativeMonths:
          begin
            Add(fviSpecial, foLastMonth, cxGetResourceString(@scxGridLastMonth), True);
            Add(fviSpecial, foThisMonth, cxGetResourceString(@scxGridThisMonth), True);
            Add(fviSpecial, foNextMonth, cxGetResourceString(@scxGridNextMonth), True);
          end;
        dtfRelativeYears:
          begin
            Add(fviSpecial, foLastYear, cxGetResourceString(@scxGridLastYear), True);
            Add(fviSpecial, foThisYear, cxGetResourceString(@scxGridThisYear), True);
            Add(fviSpecial, foNextYear, cxGetResourceString(@scxGridNextYear), True);
          end;
        dtfPastFuture:
          begin
            Add(fviSpecial, foInPast, cxGetResourceString(@scxGridPast), True);
            Add(fviSpecial, foInFuture, cxGetResourceString(@scxGridFuture), True);
          end;
      end;
           (*               !!!
  if dtfRelativeDay in AFilterItems then
  begin
    Add(fviSpecial, foYesterday, cxGetResourceString(@scxGridYesterday), True);
    Add(fviSpecial, foToday, cxGetResourceString(@scxGridToday), True);
    Add(fviSpecial, foTomorrow, cxGetResourceString(@scxGridTomorrow), True);
  end;
  if dtfRelativeWeek in AFilterItems then
    Add(fviSpecial, foLastWeek, cxGetResourceString(@scxGridLastWeek), True);
  if dtfRelativeMonth in AFilterItems then
    Add(fviSpecial, foLastMonth, cxGetResourceString(@scxGridLastMonth), True);
  if dtfRelativeYear in AFilterItems then
    Add(fviSpecial, foLastYear, cxGetResourceString(@scxGridLastYear), True);
  if dtfRelativeWeek in AFilterItems then
    Add(fviSpecial, foThisWeek, cxGetResourceString(@scxGridThisWeek), True);
  if dtfRelativeMonth in AFilterItems then
    Add(fviSpecial, foThisMonth, cxGetResourceString(@scxGridThisMonth), True);
  if dtfRelativeYear in AFilterItems then
    Add(fviSpecial, foThisYear, cxGetResourceString(@scxGridThisYear), True);
  if dtfRelativeWeek in AFilterItems then
    Add(fviSpecial, foNextWeek, cxGetResourceString(@scxGridNextWeek), True);
  if dtfRelativeMonth in AFilterItems then
    Add(fviSpecial, foNextMonth, cxGetResourceString(@scxGridNextMonth), True);
  if dtfRelativeYear in AFilterItems then
    Add(fviSpecial, foNextYear, cxGetResourceString(@scxGridNextYear), True);
            *)
  Add(fviMRUSeparator, Null, '', True);
end;

function TcxGridFilterValueList.IsSpecialOperatorKindSupported(AKind: TcxFilterOperatorKind): Boolean;
begin
  Result := AKind in [foYesterday..foInFuture];
end;

procedure TcxGridFilterValueList.Load(AItem: TcxCustomGridTableItem; AInitSortByDisplayText: Boolean;
  AUseFilteredValues: Boolean; AAddValueItems: Boolean; AUniqueOnly: Boolean);
begin
  inherited Load(AItem.Index, AInitSortByDisplayText, AUseFilteredValues, AAddValueItems, AUniqueOnly);
  if IsDateTimeValueTypeClass(AItem.DataBinding.ValueTypeClass) then
  begin
    if dxContainDateTimeRelativeFilters(AItem.GetDateTimeHandlingFilters) then
      AddDateTimeRelativeFilters(AItem);
    if dxContainDateTimeAbsoluteFilters(AItem.GetDateTimeHandlingFilters) or AItem.HasInitFilteringDateRangesHandlers then
      AddDateTimeAbsoluteFilters(AItem);
  end;
end;

procedure TcxGridFilterValueList.ApplyFilter(AItem: TcxCustomGridTableItem; AIndex: Integer;
  AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);

  function GetVarArrayDisplayText(const AValue: Variant): string;
  begin
    Result :=
      AItem.GetProperties.GetDisplayText(AValue[0], True, False) + ';' +
      AItem.GetProperties.GetDisplayText(AValue[1], True, False);
  end;

  procedure BeginFiltering;
  begin
    AItem.DataBinding.Filter.BeginUpdate;
  end;

  procedure EndFiltering(AKind: TcxFilterValueItemKind);
  begin
    AItem.GridView.BeginFilteringUpdate;
    try
      if not (AKind in [fviAll, fviCustom]) then
        AItem.DataBinding.Filter.Active := True;
      AItem.DataBinding.Filter.EndUpdate;
    finally
      AItem.GridView.EndFilteringUpdate;
    end;
  end;

var
  AListItem: TcxFilterValueItem;
begin
  AListItem := Items[AIndex];
  BeginFiltering;
  try
    case AListItem.Kind of
      fviAll:
        AItem.Filtered := False;
      fviCustom:
        AItem.GridView.Filtering.RunCustomizeDialog(AItem);
      fviBlanks:
        AItem.DataBinding.AddToFilter(AFilterList, foEqual, AListItem.Value,
          cxGetResourceString(@cxSFilterBlankCaption), AReplaceExistent);
      fviNonBlanks:
        AItem.DataBinding.AddToFilter(AFilterList, foNotEqual, AListItem.Value,
          cxGetResourceString(@cxSFilterBlankCaption), AReplaceExistent);
      fviUser:
        AItem.DoUserFiltering(AListItem.Value, AListItem.DisplayText);
      fviUserEx:
        AItem.DoUserFilteringEx(AFilterList, AListItem.Value, AListItem.DisplayText);
      fviValue, fviMRU:
        begin
          AItem.DataBinding.AddToFilter(AFilterList, foEqual, AListItem.Value,
            AListItem.DisplayText, AReplaceExistent);
          if AAddToMRUItemsList then
            AItem.DataBinding.FilterMRUValueItems.Add(AListItem.Value, AListItem.DisplayText);
        end;
      fviSpecial:
        if VarIsArray(AListItem.Value) then
          AItem.DataBinding.AddToFilter(AFilterList, foBetween, AListItem.Value,
            GetVarArrayDisplayText(AListItem.Value), AReplaceExistent)
        else
          if IsSpecialOperatorKindSupported(TcxFilterOperatorKind(AListItem.Value)) then
            AItem.DataBinding.AddToFilter(AFilterList, TcxFilterOperatorKind(AListItem.Value),
              Null, '', AReplaceExistent);
    end;
  finally
    EndFiltering(AListItem.Kind);
  end;
end;

function TcxGridFilterValueList.GetIndexByCriteriaItem(ACriteriaItem: TcxFilterCriteriaItem): Integer;
begin
  Result := -1;
  if ACriteriaItem <> nil then
  begin
    Result := FindItemByKind(fviUserEx, ACriteriaItem.Value);
    if Result = -1 then
      if IsSpecialOperatorKindSupported(ACriteriaItem.OperatorKind) then
        Result := FindItemByKind(fviSpecial, ACriteriaItem.OperatorKind)
      else
        if ACriteriaItem.OperatorKind = foBetween then
          Result := FindItemByKind(fviSpecial, ACriteriaItem.Value);
  end;
  if Result = -1 then
    Result := inherited GetIndexByCriteriaItem(ACriteriaItem);
end;

procedure TcxGridFilterValueList.Load(AItem: TcxCustomGridTableItem;
  AInitSortByDisplayText: Boolean = True; AUseFilteredValues: Boolean = False;
  AAddValueItems: Boolean = True);
begin
  Load(AItem, AInitSortByDisplayText, AUseFilteredValues, AAddValueItems, True);
end;

{ TcxGridItemDataBinding  }

constructor TcxGridItemDataBinding.Create(AItem: TcxCustomGridTableItem);
begin
  inherited Create;
  FItem := AItem;
  FDefaultValuesProvider := GetDefaultValuesProviderClass.Create(Self);
  FFilterMRUValueItems := GetFilterMRUValueItemsClass.Create;
  FFilterMRUValueItems.MaxCount := GridView.Filtering.ItemMRUItemsListCount;
end;

destructor TcxGridItemDataBinding.Destroy;
begin
  FFilterMRUValueItems.Free;
  FDefaultValuesProvider.Free;
  inherited;
end;

function TcxGridItemDataBinding.GetDataController: TcxCustomDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridItemDataBinding.GetFilter: TcxDataFilterCriteria;
begin
  Result := DataController.Filter;
end;

function TcxGridItemDataBinding.GetFilterCriteriaItem: TcxFilterCriteriaItem;
begin
  Result := Filter.FindItemByItemLink(FItem);
end;

function TcxGridItemDataBinding.GetFiltered: Boolean;
begin
  Result := FilterCriteriaItem <> nil;
end;

function TcxGridItemDataBinding.GetGridView: TcxCustomGridTableView;
begin
  Result := FItem.GridView;
end;

function TcxGridItemDataBinding.GetValueType: string;
begin
  if ValueTypeClass = nil then
    Result := ''
  else
    Result := ValueTypeClass.Caption;
end;

function TcxGridItemDataBinding.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := DataController.GetItemValueTypeClass(FItem.Index);
end;

procedure TcxGridItemDataBinding.SetData(Value: TObject);
begin
  if FData <> Value then
  begin
    FData := Value;
    FItem.Changed(ticProperty);
  end;
end;

procedure TcxGridItemDataBinding.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
    if not Value then
      Filter.RemoveItemByItemLink(FItem);
end;

procedure TcxGridItemDataBinding.SetValueType(const Value: string);
begin
  if ValueType <> Value then
    ValueTypeClass := cxValueTypeClassList.ItemByCaption(Value);
end;

procedure TcxGridItemDataBinding.SetValueTypeClass(Value: TcxValueTypeClass);
begin
  DataController.ChangeValueTypeClass(FItem.Index, Value);
end;

function TcxGridItemDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
var
  AIcxGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) then
    Result := AIcxGridDataController.GetItemDefaultValuesProviderClass
  else
    Result := nil;
end;

function TcxGridItemDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxStringValueType;
end;

function TcxGridItemDataBinding.GetFilterFieldName: string;
begin
  Result := '';
end;

function TcxGridItemDataBinding.GetFilterMRUValueItemsClass: TcxGridFilterMRUValueItemsClass;
begin
  Result := TcxGridFilterMRUValueItems;
end;

procedure TcxGridItemDataBinding.GetFilterStrings(AStrings: TStrings; AValueList: TcxGridFilterValueList;
  AValuesOnly: Boolean; AUniqueOnly: Boolean);
var
  I: Integer;
  S: string;
begin
  GetFilterValues(AValueList, AValuesOnly, True, True, AUniqueOnly);
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    for I := 0 to AValueList.Count - 1 do
    begin
      S := AValueList[I].DisplayText;
      GetFilterDisplayText(AValueList[I].Value, S);
      AStrings.AddObject(S, TObject(AValueList[I]));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TcxGridItemDataBinding.GetFilterValues(AValueList: TcxGridFilterValueList; AValuesOnly: Boolean;
  AInitSortByDisplayText: Boolean; ACanUseFilteredValues: Boolean; AUniqueOnly: Boolean);
var
  I: Integer;
begin
  AValueList.Load(Item, AInitSortByDisplayText,
    ACanUseFilteredValues and Item.UseFilteredValuesForFilterValueList,
    Item.UseValueItemsForFilterValueList, AUniqueOnly);
  Item.DoGetFilterValues(AValueList);
  if AValuesOnly then
    for I := AValueList.Count - 1 downto 0 do
      if AValueList[I].Kind <> fviValue then
        AValueList.Delete(I)
      else
  else
    if Item.CanFilterMRUValueItems then
      FilterMRUValueItems.AddItemsTo(AValueList);
end;

procedure TcxGridItemDataBinding.Init;
begin
end;

function TcxGridItemDataBinding.IsValueTypeStored: Boolean;
begin
  Result := ValueTypeClass <> GetDefaultValueTypeClass;
end;

procedure TcxGridItemDataBinding.SetFilterActiveValue(AOperatorKind: TcxFilterOperatorKind;
  const AValue: Variant; const ADisplayText: string);
begin
  GridView.BeginFilteringUpdate;
  try
    Filter.BeginUpdate;
    try
      AddToFilter(nil, AOperatorKind, AValue, ADisplayText);
      Filter.Active := True;
    finally
      Filter.EndUpdate;
    end;
  finally
    GridView.EndFilteringUpdate;
  end;
end;

procedure TcxGridItemDataBinding.SetFilterActiveValueIndex(AValueList: TcxGridFilterValueList; AIndex: Integer;
  AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);
begin
  AValueList.ApplyFilter(Item, AIndex, AFilterList, AReplaceExistent, AAddToMRUItemsList);
end;

procedure TcxGridItemDataBinding.SetFilterActiveValues(AValueInfos: TdxFastObjectList;
  ABoolOperator: TcxFilterBoolOperatorKind);
var
  I: Integer;
  AList: TcxFilterCriteriaItemList;
  AInfo: TdxExcelFilterValueContainerValueInfo;
begin
  GridView.BeginFilteringUpdate;
  try
    Filter.BeginUpdate;
    try
      AList := nil;
      Filter.RemoveItemByItemLink(Item);
      if Filter.Root.BoolOperatorKind <> ABoolOperator then
        if Filter.IsEmpty then
          Filter.Root.BoolOperatorKind := ABoolOperator
        else
          AList := Filter.Root.AddItemList(ABoolOperator);
      for I := 0 to AValueInfos.Count - 1 do
      begin
        AInfo := AValueInfos[I] as TdxExcelFilterValueContainerValueInfo;
        AddToFilter(AList, AInfo.OperatorKind, AInfo.Value, AInfo.DisplayText, False);
      end;
      Filter.Active := True;
    finally
      Filter.EndUpdate;
    end;
  finally
    GridView.EndFilteringUpdate;
  end;
end;

procedure TcxGridItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridItemDataBinding then
    with TcxGridItemDataBinding(Source) do
    begin
      Self.Data := Data;
      Self.ValueTypeClass := ValueTypeClass;
    end
  else
    inherited;
end;

function TcxGridItemDataBinding.DefaultCaption: string;
begin
  Result := ''{Item.Name};
end;

function TcxGridItemDataBinding.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDataRepositoryItems.GetItem(ValueTypeClass);
end;

function TcxGridItemDataBinding.DefaultWidth(ATakeHeaderIntoAccount: Boolean = True): Integer;
begin
  Result := dxGetScaleFactor(GridView).Apply(64);
end;

function TcxGridItemDataBinding.GetAllowedSummaryKinds: TcxSummaryKinds;
begin
  Result := DataController.GetAllowedSummaryKinds(Item.Index);
end;

function TcxGridItemDataBinding.GetDefaultValuesProvider(AProperties: TcxCustomEditProperties = nil): IcxEditDefaultValuesProvider;
begin
  Result := FDefaultValuesProvider;
end;

function TcxGridItemDataBinding.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result :=
    DataController.IsDisplayFormatDefined(FItem.Index, not AIsCurrencyValueAccepted) or
    FItem.HasDataTextHandler;
end;

function TcxGridItemDataBinding.AddToFilter(AParent: TcxFilterCriteriaItemList;
  AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; const ADisplayText: string;
  AReplaceExistent: Boolean): TcxFilterCriteriaItem;
begin
  Result := GridView.ViewData.AddItemToFilter(AParent, FItem, AOperatorKind,
    AValue, ADisplayText, AReplaceExistent);
end;

procedure TcxGridItemDataBinding.GetFilterDisplayText(const AValue: Variant;
  var ADisplayText: string);
begin
  FItem.GetFilterDisplayText(AValue, ADisplayText);
end;

procedure TcxGridItemDataBinding.GetFilterStrings(AStrings: TStrings; AValueList: TcxGridFilterValueList);
begin
  GetFilterStrings(AStrings, AValueList, False, True);
end;

procedure TcxGridItemDataBinding.GetFilterValues(AValueList: TcxGridFilterValueList;
  AValuesOnly: Boolean = True; AInitSortByDisplayText: Boolean = False;
  ACanUseFilteredValues: Boolean = False);
begin
  GetFilterValues(AValueList, AValuesOnly, AInitSortByDisplayText, ACanUseFilteredValues, True);
end;

procedure TcxGridItemDataBinding.GetFilterActiveValueIndexes(AValueList: TcxGridFilterValueList;
  var AIndexes: TcxGridIndexes);

  procedure CheckFilterItem(AFilterItem: TcxFilterCriteriaItem);
  var
    AValueIndex: Integer;
  begin
    if AFilterItem.ItemLink <> FItem then Exit;
    AValueIndex := AValueList.GetIndexByCriteriaItem(AFilterItem);
    if (AValueIndex <> -1) and (AValueList[AValueIndex].Kind <> fviCustom) then
    begin
      SetLength(AIndexes, Length(AIndexes) + 1);
      AIndexes[Length(AIndexes) - 1] := AValueIndex;
    end;
  end;

var
  AFilterItem: TcxFilterCriteriaItem;
  AFilterList: TcxFilterCriteriaItemList;
  I: Integer;
begin
  AIndexes := nil;
  AFilterItem := FilterCriteriaItem;
  if AFilterItem = nil then Exit;
  AFilterList := AFilterItem.Parent;
  case AFilterList.BoolOperatorKind of
    fboOr:
      for I := 0 to AFilterList.Count - 1 do
        if AFilterList[I] is TcxFilterCriteriaItem then
          CheckFilterItem(TcxFilterCriteriaItem(AFilterList[I]));
    fboAnd:
      CheckFilterItem(AFilterItem);
  end;
end;

procedure TcxGridItemDataBinding.SetFilterActiveValueIndexes(AValueList: TcxGridFilterValueList; const AIndexes: TcxGridIndexes;
  AAddToMRUItemsList: Boolean = False);

  function GetFilterList: TcxFilterCriteriaItemList;
  begin
    if (Length(AIndexes) > 1) and (Filter.Root.BoolOperatorKind = fboAnd) then
      Result := Filter.Root.AddItemList(fboOr)
    else
      Result := nil;
  end;

var
  I: Integer;
  AFilterList: TcxFilterCriteriaItemList;
begin
  GridView.BeginFilteringUpdate;
  try
    Filter.BeginUpdate;
    try
      Filter.RemoveItemByItemLink(FItem);
      AFilterList := GetFilterList;
      for I := 0 to Length(AIndexes) - 1 do
        SetFilterActiveValueIndex(AValueList, AIndexes[I], AFilterList, False, AAddToMRUItemsList);
    finally
      Filter.EndUpdate;
    end;
  finally
    GridView.EndFilteringUpdate;
  end;
end;

{ TcxGridYesterdayRange }

function TcxGridYesterdayRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridYesterday);
end;

{ TcxGridTodayRange }

function TcxGridTodayRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridToday);
end;

{ TcxGridTomorrowRange }

function TcxGridTomorrowRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridTomorrow);
end;

{ TcxGridLastWeekRange }

function TcxGridLastWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridLastWeek);
end;

{ TcxGridThisWeekRange }

function TcxGridThisWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridThisWeek);
end;

{ TcxGridNextWeekRange }

function TcxGridNextWeekRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridNextWeek);
end;

{ TcxGridLastMonthRange }

function TcxGridLastMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridLastMonth);
end;

{ TcxGridThisMonthRange }

function TcxGridThisMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridThisMonth);
end;

{ TcxGridNextMonthRange }

function TcxGridNextMonthRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridNextMonth);
end;

{ TcxGridLastYearRange }

function TcxGridLastYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridLastYear);
end;

{ TcxGridThisYearRange }

function TcxGridThisYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridThisYear);
end;

{ TcxGridNextYearRange }

function TcxGridNextYearRange.GetDisplayText(const ADate: TDateTime): string;
begin
  Result := cxGetResourceString(@scxGridNextYear);
end;

{ TcxGridFilteringDateRanges }

function TcxGridFilteringDateRanges.GetMonthRange: TdxCustomDateRangeClass;
begin
  Result := TcxGridMonthRange;
end;

function TcxGridFilteringDateRanges.GetYearRange: TdxCustomDateRangeClass;
begin
  Result := TcxGridYearRange;
end;

function TcxGridFilteringDateRanges.GetDateTimeHandling: TcxCustomGridTableDateTimeHandling;
begin
  Result := TcxCustomGridTableDateTimeHandling(inherited DateTimeHandling);
end;

{ TcxGridGroupingDateRanges }

function TcxGridGroupingDateRanges.GetDayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridDayRange;
end;

function TcxGridGroupingDateRanges.GetHourRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridHourRange;
end;

function TcxGridGroupingDateRanges.GetLastMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridLastMonthRange;
end;

function TcxGridGroupingDateRanges.GetLastWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridLastWeekRange;
end;

function TcxGridGroupingDateRanges.GetLastYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridLastYearRange;
end;

function TcxGridGroupingDateRanges.GetMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridMonthRange;
end;

function TcxGridGroupingDateRanges.GetNextMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridNextMonthRange;
end;

function TcxGridGroupingDateRanges.GetNextWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridNextWeekRange;
end;

function TcxGridGroupingDateRanges.GetNextYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridNextYearRange;
end;

function TcxGridGroupingDateRanges.GetThisMonthRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridThisMonthRange;
end;

function TcxGridGroupingDateRanges.GetThisWeekRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridThisWeekRange;
end;

function TcxGridGroupingDateRanges.GetThisYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridThisYearRange;
end;

function TcxGridGroupingDateRanges.GetTodayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridTodayRange;
end;

function TcxGridGroupingDateRanges.GetTomorrowRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridTomorrowRange;
end;

function TcxGridGroupingDateRanges.GetYearRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridYearRange;
end;

function TcxGridGroupingDateRanges.GetYesterdayRangeClass: TdxCustomDateRangeClass;
begin
  Result := TcxGridYesterdayRange;
end;

function TcxGridGroupingDateRanges.GetDateTimeHandling: TcxCustomGridTableDateTimeHandling;
begin
  Result := TcxCustomGridTableDateTimeHandling(inherited DateTimeHandling);
end;

{ TcxCustomGridRecord }

constructor TcxCustomGridRecord.Create(AViewData: TcxCustomGridTableViewData;
  AIndex: Integer; const ARecordInfo: TcxRowInfo);
begin
  inherited Create;
  FViewData := AViewData;
  FIndex := AIndex;
  RecordInfo := ARecordInfo;
end;

destructor TcxCustomGridRecord.Destroy;
begin
  //GridView.DoRecordDestroying(Self);
  if FViewInfo <> nil then
    FViewInfo.FRecord := nil;
  inherited;
end;

function TcxCustomGridRecord.GetController: TcxCustomGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxCustomGridRecord.GetDataController: TcxCustomDataController;
begin
  Result := FViewData.DataController;
end;

function TcxCustomGridRecord.GetDragHighlighted: Boolean;
begin
  Result := FViewData.Controller.DragHighlightedRecord = Self;
end;

function TcxCustomGridRecord.GetFocused: Boolean;
begin
  Result := FViewData.Controller.FocusedRecord = Self;
end;

function TcxCustomGridRecord.GetGridView: TcxCustomGridTableView;
begin
  Result := FViewData.GridView;
end;

function TcxCustomGridRecord.GetIsEditing: Boolean;
begin
  Result := FViewData.EditingRecord = Self;
end;

function TcxCustomGridRecord.GetIsNewItemRecord: Boolean;
begin
  Result := FViewData.NewItemRecord = Self;
end;

function TcxCustomGridRecord.GetIsValid: Boolean;
begin
  Result := (0 <= RecordIndex) and (RecordIndex < ViewData.DataController.RecordCount);
end;

function TcxCustomGridRecord.GetLastParentRecordCount: Integer;
begin
  if Level = 0 then
    Result := 0
  else
    for Result := 0 to Level - 1 do
      if not IsParentRecordLast[Result] then Break;
end;

function TcxCustomGridRecord.GetLevel: Integer;
begin
  Result := RecordInfo.Level;
end;

function TcxCustomGridRecord.GetPartVisible: Boolean;
begin
  Result := FViewInfo <> nil;
end;

function TcxCustomGridRecord.GetRecordIndex: Integer;
begin
  Result := RecordInfo.RecordIndex;
end;

procedure TcxCustomGridRecord.SetExpanded(Value: Boolean);
begin
  if Expanded <> Value then
    if Value then
      Expand(False)
    else
      Collapse(False);
end;

procedure TcxCustomGridRecord.SetFocused(Value: Boolean);
begin
  if Value then
    FViewData.Controller.FocusedRecord := Self
  else
    if Focused then
      FViewData.Controller.FocusedRecord := nil;
end;

procedure TcxCustomGridRecord.RefreshRecordInfo;
begin
  if IsNewItemRecord then
    with RecordInfo do
    begin
      Expanded := False;
      Level := 0;
      RecordIndex := DataController.NewItemRecordIndex;
    end
  else
    RecordInfo := DataController.GetRowInfo(FIndex);
end;

procedure TcxCustomGridRecord.DoCollapse(ARecurse: Boolean);
begin
end;

procedure TcxCustomGridRecord.DoExpand(ARecurse: Boolean);
begin
end;

{function TcxCustomGridRecord.GetDestroyingOnExpanding: Boolean;
begin
  Result := False;
end;}

function TcxCustomGridRecord.GetExpandable: Boolean;
begin
  Result := False;
end;

function TcxCustomGridRecord.GetExpanded: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRecord.ToggleExpanded;
begin
  Expanded := not Expanded;
end;

function TcxCustomGridRecord.GetHasCells: Boolean;
begin
  Result := False;
end;

function TcxCustomGridRecord.GetIsData: Boolean;
begin
  Result := True;
end;

function TcxCustomGridRecord.GetIsFirst: Boolean;
begin
  Result := Index = 0;
end;

function TcxCustomGridRecord.GetIsLast: Boolean;
begin
  Result := Index = FViewData.RecordCount - 1;
end;

function TcxCustomGridRecord.GetIsParent: Boolean;
begin
  Result := False;
end;

function TcxCustomGridRecord.GetIsParentRecordLast(AIndex: Integer): Boolean;
begin
  Result := IsLast or (ViewData.Records[Index + 1].Level < Level - AIndex);
end;

function TcxCustomGridRecord.GetParentRecord: TcxCustomGridRecord;
begin
  if Level <> 0 then
  begin
    Result := Self;
    repeat
      Result := ViewData.Records[Result.Index - 1];
    until Result.Level < Level;
  end
  else
    if GridView.IsDetail then
      Result := GridView.MasterGridRecord
    else
      Result := nil;
end;

function TcxCustomGridRecord.GetSelected: Boolean;
begin
  if IsNewItemRecord then
    Result := Controller.NewItemRecordFocused
  else
    Result := Controller.IsRecordSelected(Self);
end;

function TcxCustomGridRecord.GetVisible: Boolean;
begin
  Result := IsNewItemRecord or PartVisible and
    FViewData.ViewInfo.RecordsViewInfo.GetRealItem(Self).FullyVisible;
end;

procedure TcxCustomGridRecord.SetSelected(Value: Boolean);
begin
  if IsNewItemRecord then
    Controller.NewItemRecordFocused := Value
  else
    Controller.ChangeRecordSelection(Self, Value);
end;

function TcxCustomGridRecord.GetDisplayText(Index: Integer): string;
begin
  Result := DataController.GetRowDisplayText(RecordInfo, Index);
end;

function TcxCustomGridRecord.GetValueCount: Integer;
begin
  Result := DataController.GetItemCount;
end;

function TcxCustomGridRecord.GetValue(Index: Integer): Variant;
begin
  Result := DataController.GetRowValue(RecordInfo, Index);
end;

procedure TcxCustomGridRecord.SetDisplayText(Index: Integer; const Value: string);
begin
  DataController.SetDisplayText(RecordIndex, Index, Value);
end;

procedure TcxCustomGridRecord.SetValue(Index: Integer; const Value: Variant);
begin
  DataController.SetValue(RecordIndex, Index, Value);
end;

procedure TcxCustomGridRecord.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ADD:
      if Expandable then
      begin
        Controller.EatKeyPress := True;
        Expanded := True;
        Key := 0;
      end;
    VK_SUBTRACT:
      if Expandable then
      begin
        Controller.EatKeyPress := True;
        Expanded := False;
        Key := 0;
      end;
    VK_RETURN, VK_F2, VK_PROCESSKEY:
      if HasCells and (Controller.FocusedItem <> nil) and
        ((Shift = []) or (Key = VK_RETURN) and (Shift = [ssShift])) then
      begin
        Controller.FocusedItem.Editing := True;
        if Controller.FocusedItem.Editing and (Key <> VK_PROCESSKEY) then
          Key := 0;
      end;
  end;
end;

function TcxCustomGridRecord.CanFocus: Boolean;
begin
  Result := GridView.DoCanFocusRecord(Self); //!!!
end;

function TcxCustomGridRecord.CanFocusCells: Boolean;
begin
  Result := GridView.OptionsSelection.CellSelect;
end;

procedure TcxCustomGridRecord.Collapse(ARecurse: Boolean);
begin
  if Expandable then DoCollapse(ARecurse);
end;

procedure TcxCustomGridRecord.Expand(ARecurse: Boolean);
begin
  if Expandable then DoExpand(ARecurse);
end;

function TcxCustomGridRecord.GetFirstFocusableChild: TcxCustomGridRecord;
begin
  Result := nil;
end;

function TcxCustomGridRecord.GetLastFocusableChild(ARecursive: Boolean): TcxCustomGridRecord;
begin
  Result := nil;
end;

procedure TcxCustomGridRecord.Invalidate(AItem: TcxCustomGridTableItem = nil);
begin
  if PartVisible then
    GridView.Changed(TcxGridRecordChange.Create(GridView, Self, Index, AItem));
end;

procedure TcxCustomGridRecord.MakeVisible;
begin
  Controller.MakeRecordVisible(Self);
end;

{ TcxCustomGridTableViewData }

constructor TcxCustomGridTableViewData.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FRecords := TdxFastList.Create;
end;

destructor TcxCustomGridTableViewData.Destroy;
begin
  DestroyNewItemRecord;
  DestroyRecords;
  FRecords.Free;
  inherited;
end;

function TcxCustomGridTableViewData.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxCustomGridTableViewData.GetEditingRecord: TcxCustomGridRecord;
begin
  if DataController.IsEditing then
    Result := GetRecordByRecordIndex(DataController.EditingRecordIndex)
  else
    Result := nil;
end;

function TcxCustomGridTableViewData.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableViewData.GetInternalRecord(Index: Integer): TcxCustomGridRecord;
begin
  Result := TcxCustomGridRecord(FRecords[Index]);
end;

function TcxCustomGridTableViewData.GetRecord(Index: Integer): TcxCustomGridRecord;
begin
  Result := InternalRecords[Index];
  if Result = nil then
  begin
    Result := CreateRecord(Index);
    FRecords[Index] := Result;
    //GridView.DoRecordCreated(Result);
  end;
end;

function TcxCustomGridTableViewData.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TcxCustomGridTableViewData.GetViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited ViewInfo);
end;

function TcxCustomGridTableViewData.CreateRecord(AIndex: Integer): TcxCustomGridRecord;
var
  ARecordInfo: TcxRowInfo;
begin
  ARecordInfo := DataController.GetRowInfo(AIndex);
  Result := GetRecordClass(ARecordInfo).Create(Self, AIndex, ARecordInfo);
end;

function TcxCustomGridTableViewData.GetFilterValueListClass: TcxGridFilterValueListClass;
begin
  Result := TcxGridFilterValueList;
end;

function TcxCustomGridTableViewData.GetRecordByKind(AKind, AIndex: Integer): TcxCustomGridRecord;
begin
  case AKind of
    rkNormal:
      if (0 <= AIndex) and (AIndex < RecordCount) then
        Result := Records[AIndex]
      else
        Result := nil;
    rkNewItem:
      Result := NewItemRecord;
  else
    Result := nil;
  end;
end;

function TcxCustomGridTableViewData.GetRecordKind(ARecord: TcxCustomGridRecord): Integer;
begin
  if ARecord.IsNewItemRecord then
    Result := rkNewItem
  else
    Result := rkNormal;
end;

procedure TcxCustomGridTableViewData.AssignEditingRecord;
begin
  FEditingRecord := GetEditingRecord;
end;

procedure TcxCustomGridTableViewData.CreateNewItemRecord;
var
  ARowInfo: TcxRowInfo;
begin
  FNewItemRecord := GetNewItemRecordClass.Create(Self, -1, ARowInfo);
  FNewItemRecord.RefreshRecordInfo;
  //GridView.DoRecordCreated(FNewItemRecord);
end;

procedure TcxCustomGridTableViewData.DestroyNewItemRecord;
begin
  FreeAndNil(FNewItemRecord);
end;

function TcxCustomGridTableViewData.GetNewItemRecordClass: TcxCustomGridRecordClass;
begin
  Result := nil;
end;

procedure TcxCustomGridTableViewData.RecreateNewItemRecord;
begin
  if HasNewItemRecord then
  begin
    DestroyNewItemRecord;
    CreateNewItemRecord;
  end;
end;

procedure TcxCustomGridTableViewData.Collapse(ARecurse: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := RecordCount - 1 downto 0 do
      Records[I].Collapse(ARecurse);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableViewData.DestroyRecords;
var
  I: Integer;
  ARecord: TcxCustomGridRecord;
begin
  for I := 0 to RecordCount - 1 do
  begin
    ARecord := InternalRecords[I];
    if ARecord <> nil then
    begin
      ARecord.Free;
      FRecords[I] := nil;
    end;
  end;
end;

procedure TcxCustomGridTableViewData.Expand(ARecurse: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to RecordCount - 1 do
      Records[I].Expand(ARecurse);
  finally
    EndUpdate;
  end;
end;

function TcxCustomGridTableViewData.GetRecordByIndex(AIndex: Integer): TcxCustomGridRecord;
begin
  if (0 <= AIndex) and (AIndex < RecordCount) then
    Result := Records[AIndex]
  else
    Result := nil;
end;

function TcxCustomGridTableViewData.GetRecordByRecordIndex(ARecordIndex: Integer): TcxCustomGridRecord;
var
  I: Integer;
begin
  if HasNewItemRecord and (ARecordIndex = DataController.NewItemRecordIndex) then
    Result := FNewItemRecord
  else
    if (0 <= ARecordIndex) and (ARecordIndex < DataController.RecordCount) then
    begin
      // following loop was created because during sorting
      // row order in data controller is new already, but it is old in view data
      // (used in TcxCustomGridTableItem.GetProperties(ARecordIndex))
      if GridView.IsUpdateLocked or not DataController.IsRowInfoValid then
        for I := 0 to RecordCount - 1 do
        begin
          Result := InternalRecords[I];
          if (Result <> nil) and Result.IsData and (Result.RecordIndex = ARecordIndex) then
            Exit;
        end;
      Result := GetRecordByIndex(DataController.GetRowIndexByRecordIndex(ARecordIndex, False));
    end
    else
      Result := nil;
end;

function TcxCustomGridTableViewData.GetRecordIndexByRecord(ARecord: TcxCustomGridRecord): Integer;
begin
  if ARecord = nil then
    Result := -1  //!!! can be internal record index
  else
    Result := ARecord.RecordIndex;
end;

function TcxCustomGridTableViewData.IsRecordIndexValid(AIndex: Integer): Boolean;
begin
  Result := (0 <= AIndex) and (AIndex < RecordCount);
end;

procedure TcxCustomGridTableViewData.Refresh(ARecordCount: Integer);
begin
  RecreateNewItemRecord;
  DestroyRecords;
  FRecords.Clear;
  FRecords.Count := ARecordCount;
  AssignEditingRecord;
  GridView.SizeChanged(GridView.IsPattern);
end;

procedure TcxCustomGridTableViewData.RefreshRecords;
var
  I: Integer;
  ARecord: TcxCustomGridRecord;
begin
  if HasNewItemRecord then FNewItemRecord.RefreshRecordInfo;
  for I := 0 to RecordCount - 1 do
  begin
    ARecord := InternalRecords[I];
    if ARecord <> nil then ARecord.RefreshRecordInfo;
  end;
  AssignEditingRecord;
end;

procedure TcxCustomGridTableViewData.CheckNewItemRecord;
begin
  if HasNewItemRecord then
    CreateNewItemRecord
  else
    DestroyNewItemRecord;
end;

function TcxCustomGridTableViewData.HasNewItemRecord: Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableViewData.AddItemToFilter(AParent: TcxFilterCriteriaItemList;
  AItem: TcxCustomGridTableItem; AOperatorKind: TcxFilterOperatorKind;
  const AValue: Variant; ADisplayText: string; AReplaceExistent: Boolean): TcxFilterCriteriaItem;
begin
  if (ADisplayText = '') and not (VarIsNull(AValue) or VarIsArray(AValue)) then
    ADisplayText := AValue;
  AItem.DataBinding.GetFilterDisplayText(AValue, ADisplayText);
  if AReplaceExistent then
    DataController.Filter.RemoveItemByItemLink(AItem);
  Result := DataController.Filter.AddItem(AParent, AItem, AOperatorKind, AValue, ADisplayText);
end;

function TcxCustomGridTableViewData.CreateFilterValueList: TcxGridFilterValueList;
begin
  Result := GetFilterValueListClass.Create(DataController.Filter);
end;

function TcxCustomGridTableViewData.GetDisplayText(ARecordIndex, AItemIndex: Integer;
  out AText: string; AUseCustomValue: Boolean = False; ACustomValueOperation: TcxGridDataOperation = doGrouping): Boolean;
var
  AItem: TcxCustomGridTableItem;
  AProperties: TcxCustomEditProperties;
  AValue: Variant;
begin
  AItem := GridView.Items[AItemIndex];
  AProperties := AItem.GetProperties(ARecordIndex);
  Result := AUseCustomValue or (AProperties.GetEditValueSource(False) = evsValue);
  if Result then
  begin
    AValue := DataController.Values[ARecordIndex, AItemIndex];
    AText := GetDisplayTextFromValue(AItem, AProperties, AValue, AUseCustomValue, ACustomValueOperation);
  end;
end;

function TcxCustomGridTableViewData.GetDisplayTextFromValue(AItem: TcxCustomGridTableItem;
  AProperties: TcxCustomEditProperties; const AValue: Variant; AUseCustomValue: Boolean = False;
  ACustomValueOperation: TcxGridDataOperation = doGrouping): string;
var
  V: Variant;
begin
  if AUseCustomValue then
  begin
    V := GetCustomDataValue(AItem, AValue, ACustomValueOperation);
    Result := AProperties.GetDisplayText(V, True)
  end
  else
    Result := AProperties.GetDisplayText(AValue, True);
end;

function TcxCustomGridTableViewData.GetDisplayTextFromValue(ARecordIndex, AItemIndex: Integer; const AValue: Variant;
   AUseCustomValue: Boolean = False; ACustomValueOperation: TcxGridDataOperation = doGrouping): string;
var
  AItem: TcxCustomGridTableItem;
  AProperties: TcxCustomEditProperties;
begin
  AItem := GridView.Items[AItemIndex];
  AProperties := AItem.GetProperties(ARecordIndex);
  Result := GetDisplayTextFromValue(AItem, AProperties, AValue, AUseCustomValue, ACustomValueOperation);
end;

function TcxCustomGridTableViewData.CustomCompareDataValues(AField: TcxCustomDataField;
  const AValue1, AValue2: Variant; AMode: TcxDataControllerComparisonMode): Integer;
var
  AOperation: TcxGridDataOperation;
  ACustomValue1, ACustomValue2: Variant;
begin
  if AMode = dccmSorting then
    AOperation := doSorting
  else
    AOperation := doGrouping;
  ACustomValue1 := GetCustomDataValue(AField, AValue1, AOperation);
  ACustomValue2 := GetCustomDataValue(AField, AValue2, AOperation);
  Result := VarCompare(ACustomValue1, ACustomValue2);
end;

function TcxCustomGridTableViewData.GetCustomDataDisplayText(ARecordIndex, AItemIndex: Integer;
  AOperation: TcxGridDataOperation): string;
begin
  Result := '';
  if AOperation = doGrouping then
    Result := GetCustomDataDisplayText(AItemIndex,  DataController.Values[ARecordIndex, AItemIndex]);
  if Result = '' then
    GetDisplayText(ARecordIndex, AItemIndex, Result, True, AOperation);
end;

function TcxCustomGridTableViewData.GetCustomDataDisplayText(AItemIndex: Integer; const AValue: Variant): string;
var
  ADate: TDateTime;
  ADateRange: TcxCustomGridDateRange;
begin
  Result := '';
  if not VarIsNull(AValue) then
  begin
    if GridView.Items[AItemIndex].GroupingDateRanges.NeedTime then
      ADate := AValue
    else
      ADate := dxDateOf(AValue);
    ADateRange := GridView.Items[AItemIndex].GroupingDateRanges.GetRange(ADate);
    if ADateRange <> nil then
      Result := ADateRange.GetDisplayText(ADate);
  end;
end;

function TcxCustomGridTableViewData.GetCustomDataValue(AField: TcxCustomDataField;
  const AValue: Variant; AOperation: TcxGridDataOperation): Variant;
begin
  Result := GetCustomDataValue(TcxCustomGridTableItem(AField.Item), AValue, AOperation);
end;

function TcxCustomGridTableViewData.GetCustomDataValue(AItem: TcxCustomGridTableItem;
  const AValue: Variant; AOperation: TcxGridDataOperation): Variant;
var
  ADate: TDateTime;
  ADateRange: TcxCustomGridDateRange;
begin
  if VarIsNull(AValue) then
    Result := AValue
  else
    if AOperation in [doSorting, doGrouping] then
    begin
      if AItem.GroupingDateRanges.NeedTime then
        ADate := AValue
      else
        ADate := dxDateOf(AValue);
      ADateRange := AItem.GroupingDateRanges.GetRange(ADate);
      if ADateRange = nil then
        Result := ADate
      else
        if AOperation = doGrouping then
          Result := ADateRange.GetValue(ADate)
        else
          Result := ADateRange.GetSortingValue(ADate);
    end
    else
      Result := dxDateOf(AValue);
end;

function TcxCustomGridTableViewData.HasCustomDataHandling(AField: TcxCustomDataField;
  AOperation: TcxGridDataOperation): Boolean;
begin
  Result := (AField.Item <> nil) and
    HasCustomDataHandling(TcxCustomGridTableItem(AField.Item), AOperation);
end;

function TcxCustomGridTableViewData.HasCustomDataHandling(AItem: TcxCustomGridTableItem;
  AOperation: TcxGridDataOperation): Boolean;
begin
  Result :=
    (AOperation = doGrouping) and AItem.SupportsGroupingDateRanges(False) or
    (AOperation = doFiltering) and AItem.CanIgnoreTimeForFiltering;
end;

function TcxCustomGridTableViewData.NeedsCustomDataComparison(AField: TcxCustomDataField;
  AMode: TcxDataControllerComparisonMode): Boolean;
var
  AItem: TcxCustomGridTableItem;
  AItemGroupIndex: Integer;
begin
  AItem := TcxCustomGridTableItem(AField.Item);
  Result := (AItem <> nil) and ((AMode = dccmGrouping) or (AMode = dccmSorting)) and
    HasCustomDataHandling(AField, doGrouping);
  if Result and (AMode = dccmSorting) then
  begin
    AItemGroupIndex := AItem.GroupIndex;
    Result := (AItemGroupIndex <> -1) and
      ((AItemGroupIndex < GridView.GroupedItemCount - 1) or AItem.GroupingDateRanges.NeedSortingByTime);
  end;
end;

{ TcxCustomGridTableMovingObject }

function TcxCustomGridTableMovingObject.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxCustomGridTableMovingObject.GetCustomizationForm: TcxCustomGridTableCustomizationForm;
begin
  Result := TcxCustomGridTableCustomizationForm(inherited CustomizationForm);
end;

procedure TcxCustomGridTableMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited;
  Controller.CheckScrolling(P);
end;

{ TcxGridItemContainerZone }

constructor TcxGridItemContainerZone.Create(AItemIndex: Integer);
begin
  inherited Create;
  ItemIndex := AItemIndex;
end;

function TcxGridItemContainerZone.IsEqual(Value: TcxGridItemContainerZone): Boolean;
begin
  Result := (Value <> nil) and (ItemIndex = Value.ItemIndex);
end;

{ TcxCustomGridTableItemMovingObject }

constructor TcxCustomGridTableItemMovingObject.Create(AControl: TcxControl);
begin
  inherited;
  FDestItemContainerKind := ckNone;
end;

destructor TcxCustomGridTableItemMovingObject.Destroy;
begin
  DestZone := nil;
  inherited;
end;

procedure TcxCustomGridTableItemMovingObject.SetDestItemContainerKind(Value: TcxGridItemContainerKind);
begin
  CheckDestItemContainerKind(Value);
  if FDestItemContainerKind <> Value then
  begin
    Dirty := True;
    FDestItemContainerKind := Value;
  end;
end;

procedure TcxCustomGridTableItemMovingObject.SetDestZone(Value: TcxGridItemContainerZone);
begin
  if (FDestZone <> Value) and
    ((FDestZone = nil) or not FDestZone.IsEqual(Value) or
     (Value = nil) or not Value.IsEqual(FDestZone)) then
  begin
    Dirty := True;
    FDestZone.Free;
    FDestZone := Value;
  end
  else
    Value.Free;
end;

procedure TcxCustomGridTableItemMovingObject.CalculateDestParams(AHitTest: TcxCustomGridHitTest;
  out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone);
begin
  if AHitTest.HitTestCode = htCustomizationForm then
    AContainerKind := ckCustomizationForm
  else
    AContainerKind := ckNone;
  AZone := nil;
end;

procedure TcxCustomGridTableItemMovingObject.CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind);
begin
  if (SourceItemContainerKind <> ckCustomizationForm) and
    (AValue = ckCustomizationForm) and not CanRemove then
    AValue := ckNone;
end;

function TcxCustomGridTableItemMovingObject.GetCustomizationFormListBox: TcxCustomGridItemsListBox;
begin
  Result := CustomizationForm.ItemsListBox;
end;

function TcxCustomGridTableItemMovingObject.IsSourceCustomizationForm: Boolean;
begin
  Result := FSourceItemContainerKind = ckCustomizationForm;
end;

procedure TcxCustomGridTableItemMovingObject.BeginDragAndDrop;
begin
  if CustomizationForm <> nil then
    with CustomizationForm do
      ActivatePage(ItemsPage);
  Controller.FMovingItem := SourceItem as TcxCustomGridTableItem;
  inherited;
end;

procedure TcxCustomGridTableItemMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AHitTest: TcxCustomGridHitTest;
  ADestContainerKind: TcxGridItemContainerKind;
  ADestZone: TcxGridItemContainerZone;
begin
  AHitTest := ViewInfo.GetHitTest(P);
  CalculateDestParams(AHitTest, ADestContainerKind, ADestZone);
  DestZone := ADestZone;
  DestItemContainerKind := ADestContainerKind;
  Accepted := FDestItemContainerKind <> ckNone;
  inherited;
end;

procedure TcxCustomGridTableItemMovingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  Controller.FMovingItem := nil;
end;

{ TcxCustomGridTableItemsListBox }

function TcxCustomGridTableItemsListBox.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

procedure TcxCustomGridTableItemsListBox.RefreshItemsAsTableItems;
var
  I: Integer;
  AItem: TcxCustomGridTableItem;
begin
  inherited;
  with Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to GridView.ItemCount - 1 do
      begin
        AItem := GridView.Items[I];
        if AItem.CanMove and AItem.VisibleForCustomization and not AItem.Visible then
          AddObject(AItem.GetAlternateCaption, AItem);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

{ TcxCustomGridTableCustomizationForm }

procedure TcxCustomGridTableCustomizationForm.CreateControls;
begin
  inherited;
  FItemsListBox := GetItemsListBoxClass.Create(Self);
  with FItemsListBox do
  begin
    Align := alClient;
    Parent := FItemsPage;
    RefreshItems;
  end;
end;

function TcxCustomGridTableCustomizationForm.GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass;
begin
  Result := TcxCustomGridTableItemsListBox;
end;

function TcxCustomGridTableCustomizationForm.GetItemsPageVisible: Boolean;
begin
  Result := TcxCustomGridTableView(GridView).OptionsCustomize.ItemMoving;
end;

procedure TcxCustomGridTableCustomizationForm.InitPageControl;
begin
  inherited;
  FItemsPage := CreatePage(GetItemsPageCaption, GetItemsPageVisible);
end;

procedure TcxCustomGridTableCustomizationForm.RefreshData;
begin
  inherited;
  FItemsListBox.RefreshItems;
end;

{ TcxGridFilterPopup }

constructor TcxGridFilterPopup.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView.Site);
  FGridView := AGridView;
end;

function TcxGridFilterPopup.GetItem: TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(LinkComponent);
end;

{ TcxGridFilterMRUItemsPopup }

constructor TcxGridFilterMRUItemsPopup.Create(AGridView: TcxCustomGridTableView);
begin
  FGridView := AGridView;
  inherited Create(GridView.Site);
end;

procedure TcxGridFilterMRUItemsPopup.ApplyFilter(AItemIndex: Integer);
begin
  if GridView.DataController.Filter.Active then
  begin
    GridView.BeginFilteringUpdate;
    try
      inherited ApplyFilter(AItemIndex);
    finally
      GridView.EndFilteringUpdate;
    end;
  end
  else
    inherited ApplyFilter(AItemIndex);
end;

function TcxGridFilterMRUItemsPopup.GetMRUItemCount: Integer;
begin
  Result := GridView.FilterBox.MRUItemsListDropDownCount;
end;

function TcxGridFilterMRUItemsPopup.GetMRUItems: TdxFilterBoxMRUItems;
begin
  Result := GridView.Filtering.MRUItems;
end;

{ TdxCustomGridQuickCustomizationControl }

procedure TdxCustomGridQuickCustomizationControl.DoCheckAllItems(AValue: Boolean);
var
  I, AExpectedCount: Integer;
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    GridView.BeginUpdate();
    CheckListBox.BeginUpdate;
    try
      repeat
        I := 0;
        while I <= CheckListBox.Count - 1 do
        begin
          CheckListBox.Checked[I] := AValue;
          Popup.CheckListBoxCheckClicked(I, AValue);
          Inc(I);
        end;
        if AValue then
          AExpectedCount := CheckListBox.Count
        else
          AExpectedCount := 0;
      until TdxCustomCheckListBoxAccess(CheckListBox).GetCheckedCount = AExpectedCount;
    finally
      CheckListBox.EndUpdate;
      GridView.EndUpdate;
    end;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TdxCustomGridQuickCustomizationControl.CheckAllItems(Sender: TObject);
begin
  DoCheckAllItems((Sender as TdxQuickCustomizationCustomCommand).Checked);
end;

procedure TdxCustomGridQuickCustomizationControl.SortItems(Sender: TObject);
begin
  Popup.SetQuickCustomizationSorted((Sender as TdxQuickCustomizationCustomCommand).Checked);
  PopulateCheckListBox;
  Popup.SetGridModified;
end;

procedure TdxCustomGridQuickCustomizationControl.PopulateCommandListBox;
begin
  if Popup.CanAddCommands then
  begin
    AddCommand(cxGetResourceString(@scxQuickCustomizationAllCommandCaption), True, CheckAllItems);
    AddCommand(cxGetResourceString(@scxQuickCustomizationSortedCommandCaption), True, SortItems);
  end;
end;

procedure TdxCustomGridQuickCustomizationControl.PopulateCheckListBox;
begin
  Popup.AddCheckListBoxItems;
  CheckListBox.ItemMoving := Popup.SupportsItemMoving;
end;

function TdxCustomGridQuickCustomizationControl.GetGridView: TcxCustomGridTableView;
begin
  Result := Popup.GridView;
end;

function TdxCustomGridQuickCustomizationControl.GetPopup: TcxCustomGridCustomizationPopup;
begin
  Result := Owner as TcxCustomGridCustomizationPopup;
end;

function TdxCustomGridQuickCustomizationControl.GetCheckingAllState: TcxCheckBoxState;
const
  AState: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  I, ACount: Integer;
begin
  Result := cbsUnchecked;
  ACount := 0;
  for I := 0 to CheckListBox.Count - 1 do
  begin
    if CheckListBox.Checked[I] then
      Inc(ACount);
    if (ACount > 0) and (ACount < I + 1) then
    begin
      Result := cbsGrayed;
      Break;
    end;
  end;
  if Result <> cbsGrayed then
    Result := AState[ACount = CheckListBox.Count];
end;

procedure TdxCustomGridQuickCustomizationControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Popup.CloseUp;
  end;
end;

{ TcxCustomGridCustomizationPopup }

constructor TcxCustomGridCustomizationPopup.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FChangingItems := TList.Create;
  CreateCustomizationControl;
end;

destructor TcxCustomGridCustomizationPopup.Destroy;
begin
  FreeAndNil(FChangingItems);
  inherited Destroy;
end;

procedure TcxCustomGridCustomizationPopup.CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize);

  function GetScrollbarAreaSize: TSize;
  begin
    Result := cxSize(TcxControlAccess(CheckListBox).GetVScrollBarDefaultAreaWidth,
      TcxControlAccess(CheckListBox).GetHScrollbarDefaultAreaHeight);
  end;

var
  AOwnerScreenBounds, ADesktopWorkArea: TRect;
  ADesktopSpace, AMaxRowCount, AColumnCount: Integer;
begin
  AOwnerScreenBounds := OwnerScreenBounds;
  ADesktopWorkArea := GetDesktopWorkArea(OwnerScreenBounds.TopLeft);

  ADesktopSpace := 0;
  if (APosition.Y < ADesktopWorkArea.Top) or
    ((APosition.Y < AOwnerScreenBounds.Bottom) and (APosition.Y + ASize.cy > AOwnerScreenBounds.Top)) or
     (APosition.Y + ASize.cy > ADesktopWorkArea.Bottom) then
  begin
    if ADesktopWorkArea.Bottom - AOwnerScreenBounds.Bottom > AOwnerScreenBounds.Top - ADesktopWorkArea.Top then
      ADesktopSpace := ADesktopWorkArea.Bottom - AOwnerScreenBounds.Bottom
    else
      ADesktopSpace := AOwnerScreenBounds.Top - ADesktopWorkArea.Top;
  end;

  if ADesktopSpace <> 0 then
  begin
    AMaxRowCount := (ADesktopSpace - CustomizationControl.CheckListBox.Top) div CustomizationControl.CheckListBox.ItemHeight - 1;
    SetVisibleRowCount(AMaxRowCount);
    if IsMultiColumnMode then
    begin
      AColumnCount := CustomizationControl.CheckListBox.Count div AMaxRowCount +
        Integer(CustomizationControl.CheckListBox.Count mod AMaxRowCount <> 0);
      repeat
        RestoreControlsBounds;
        SetVisibleRowCount(Min(AMaxRowCount, CustomizationControl.CheckListBox.Count div AColumnCount + 1));
        CustomizationControl.CheckListBox.Columns := AColumnCount;
        CustomizationControl.AdjustBounds(True);
        ASize := CalculateSize;
        AlignVert := pavBottom;
        APosition := CalculatePosition(ASize);
        Dec(AColumnCount);
      until (APosition.X + ASize.cx <= ADesktopWorkArea.Right) or (APosition.X - ASize.cx >= ADesktopWorkArea.Left);
    end
    else
    begin
      RestoreControlsBounds;
      CustomizationControl.AdjustBounds(True);
      ASize := CalculateSize;
      AlignVert := pavBottom;
      APosition := CalculatePosition(ASize);
    end;
  end;
end;

function TcxCustomGridCustomizationPopup.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

procedure TcxCustomGridCustomizationPopup.ApplyCheckListBoxSorting;
begin
  CheckListBox.Sorted := IsCheckListBoxSorted;
end;

procedure TcxCustomGridCustomizationPopup.AdjustCustomizationControlSize;
begin
  TdxQuickCustomizationCustomControlAccess(CustomizationControl).AdjustSize;
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
var
  AListBox: TdxQuickCustomizationCustomCheckListBox absolute Sender;
begin
  CheckListBoxCheckClicked(AItemIndex, AListBox.Checked[AItemIndex]);
  if CustomizationControl.HasCommands then
    CustomizationControl.CommandListBox.States[0] := CustomizationControl.GetCheckingAllState;
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxCheckClicked(AIndex: Integer; AChecked: Boolean);
begin
  ItemClicked(CheckListBox.Items[AIndex].Data, AChecked);
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxItemDragOver(AItem: Pointer; var AAccept: Boolean);
begin
  AAccept := CanMoveItem(AItem);
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxInitializeSelection(Sender: TObject);
var
  I: Integer;
begin
  FSelectionInitializingInProcess := True;
  try
    if GridView.IsDesigning then
      for I := 0 to CheckListBox.Count - 1 do
        if GridView.Controller.DesignController.IsObjectSelected(CheckListBox.Items[I].Data as TPersistent) then
        begin
          CheckListBox.Selected[I] := True;
          if CheckListBox.ItemIndex = -1 then
            CheckListBox.ItemIndex := I;
        end;
  finally
    FSelectionInitializingInProcess := False;
  end;
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxSelectionChanged(Sender: TObject);
var
  AItems: TList;
  AFocusedItem: TObject;
begin
  if not GridView.IsDesigning or FSelectionInitializingInProcess then Exit;
  AItems := TList.Create;
  try
    GetCheckListBoxSelectedItems(AItems, AFocusedItem);
    GridView.Controller.DesignController.SelectObjects(AItems);
  finally
    AItems.Free;
  end;
end;

procedure TcxCustomGridCustomizationPopup.CheckListSelectedItemCheckedStateChanging(Sender: TObject);
var
  AFocusedItem: TObject;
  I: Integer;
begin
  GridView.BeginUpdate();
  FChangingItems.Clear;
  GetCheckListBoxSelectedItems(FChangingItems, AFocusedItem);
  SetLength(FChangingItemsCheckStates, FChangingItems.Count);
  for I := 0 to FChangingItems.Count - 1 do
    FChangingItemsCheckStates[I] := Integer(IsVisibleGridItem(FChangingItems[I]));
end;

procedure TcxCustomGridCustomizationPopup.CheckListSelectedItemCheckedStateChanged(Sender: TObject);
var
  I, AIndex: Integer;
  AItems: TList;
  AFocusedItem: TObject;
begin
  SynchronizeTableItemsVisibilityWithListBox;

  if TdxCustomCheckListBoxAccess(CheckListBox).GetSelectionCount > 0 then
  begin
    AItems := TList.Create;
    try
      GetCheckListBoxSelectedItems(AItems, AFocusedItem);
      for I := 0 to FChangingItems.Count - 1 do
      begin
        AIndex := CheckListBox.Items.IndexOfObject(FChangingItems[I]);
        if (AIndex = -1) or
           (Integer((IsVisibleGridItem(CheckListBox.Items[AIndex].Data))) <> FChangingItemsCheckStates[I]) then
          HandleItemClicked(FChangingItems[I], IsVisibleGridItem(FChangingItems[I]));
      end;
      SetCheckListBoxSelectedItems(AItems, AFocusedItem);
    finally
      AItems.Free;
    end;
  end;
  FChangingItems.Clear;
  GridView.EndUpdate;

  if CustomizationControl.HasCommands then
    CustomizationControl.CommandListBox.States[0] := CustomizationControl.GetCheckingAllState;
end;

procedure TcxCustomGridCustomizationPopup.CheckListBoxDragDrop(Sender, Source: TObject;
  X, Y: Integer);

  procedure MoveItems(AItems: TList; AIndex: Integer);
  var
    I: Integer;
  begin
    GridView.BeginUpdate;
    try
      for I := 0 to AItems.Count - 1 do
      begin
        SetItemIndex(AItems[I], AIndex);
        AIndex := GetItemIndex(AItems[I]) + 1;
      end;
    finally
      GridView.EndUpdate;
    end;
  end;

var
  AIndex: Integer;
  AItems: TList;
  AFocusedItem: TObject;
begin
  AIndex := TdxCustomCheckListBoxAccess(CheckListBox).GetDragItemInsertionIndex(X, Y);
  if AIndex = -1 then Exit;
  if AIndex = CheckListBox.Count then
    AIndex := GetItemIndex(CheckListBox.Items[AIndex - 1].Data) + 1
  else
    AIndex := GetItemIndex(CheckListBox.Items[AIndex].Data);

  AItems := TList.Create;
  CheckListBox.BeginUpdate;
  try
    GetCheckListBoxSelectedItems(AItems, AFocusedItem);
    MoveItems(AItems, AIndex);
    TdxQuickCustomizationCustomControlAccess(CustomizationControl).PopulateCheckListBox;
    SetCheckListBoxSelectedItems(AItems, AFocusedItem);
  finally
    CheckListBox.EndUpdate;
    AItems.Free;
  end;
end;

function TcxCustomGridCustomizationPopup.CanMoveItem(AItem: Pointer): Boolean;
begin
  Result := not GridView.OptionsCustomize.ItemsQuickCustomizationSorted;
end;

procedure TcxCustomGridCustomizationPopup.CreateCustomizationControl;
begin
  FCustomizationControl := TdxCustomGridQuickCustomizationControl.Create(Self);
  CustomizationControl.OnInitialize := CustomizationControlInitialize;

  CustomizationControl.Style.BorderStyle := cbsNone;
  CustomizationControl.Style.Edges := [];
  CustomizationControl.Style.HotTrack := False;
  CustomizationControl.Style.TransparentBorder := False;
  CustomizationControl.Style.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;

  CustomizationControl.CheckListBox.OnAction := CheckListBoxAction;
  CustomizationControl.CheckListBox.OnDragDrop := CheckListBoxDragDrop;
  CustomizationControl.CheckListBox.OnItemDragOver := CheckListBoxItemDragOver;
  CustomizationControl.CheckListBox.OnSelectionChanged := CheckListBoxSelectionChanged;
  CustomizationControl.CheckListBox.OnSelectedItemCheckedStateChanging := CheckListSelectedItemCheckedStateChanging;
  CustomizationControl.CheckListBox.OnSelectedItemCheckedStateChanged := CheckListSelectedItemCheckedStateChanged;
end;

procedure TcxCustomGridCustomizationPopup.CustomizationControlInitialize;
begin
  UpdateCommandListBoxItemsChecking;
  CheckListBoxInitializeSelection(CheckListBox);
end;

function TcxCustomGridCustomizationPopup.GetCheckListBox: TdxQuickCustomizationCustomCheckListBox;
begin
  Result := CustomizationControl.CheckListBox;
end;

procedure TcxCustomGridCustomizationPopup.InitPopup;
begin
  inherited;
  CustomizationControl.Canvas.Font := Font;
  CustomizationControl.CheckListBox.Columns := 0;
  AlignHorz := GetDefaultAlignHorz;
  AlignVert := pavBottom;
  Direction := pdVertical;
  SetVisibleRowCount(GetDropDownCount);
  CustomizationControl.Initialize(Self);
end;

function TcxCustomGridCustomizationPopup.IsCheckListBoxSorted: Boolean;
begin
  Result := GridView.OptionsCustomize.ItemsQuickCustomizationSorted;
end;

function TcxCustomGridCustomizationPopup.IsMultiColumnMode: Boolean;
begin
  Result := GridView.OptionsCustomize.ItemsQuickCustomizationMultiColumnMode;
end;

procedure TcxCustomGridCustomizationPopup.ItemClicked(AItem: TObject; AChecked: Boolean);
begin
  if IsVisibleGridItem(AItem) <> AChecked then
    HandleItemClicked(AItem, AChecked);
end;

procedure TcxCustomGridCustomizationPopup.SetGridModified;
begin
  GridView.Controller.DesignerModified;
end;

procedure TcxCustomGridCustomizationPopup.RefreshCheckListBoxItems;
begin
  AddCheckListBoxItems;
end;

procedure TcxCustomGridCustomizationPopup.ScaleFactorChanged(M, D: Integer);
begin
  inherited;
  AdjustCustomizationControlSize;
end;

procedure TcxCustomGridCustomizationPopup.SetQuickCustomizationSortOptions;
begin
  TdxCustomCheckListBoxAccess(CheckListBox).SortOptions := [lbsAnsiSort, lbsCaseInsensitive];
end;

procedure TcxCustomGridCustomizationPopup.SetVisibleRowCount(ACount: Integer);
begin
  CustomizationControl.CheckListBoxVisibleRowCount := ACount;
end;

procedure TcxCustomGridCustomizationPopup.GetCheckListBoxSelectedItems(AItems: TList; var AFocusedItem: TObject);
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Selected[I] and IsGridItem(CheckListBox.Items[I].Data) then
      AItems.Add(CheckListBox.Items[I].Data);
  AFocusedItem := nil;
  if CheckListBox.Items.IsValidIndex(CheckListBox.ItemIndex) then
    AFocusedItem := CheckListBox.Items[CheckListBox.ItemIndex].Data;
end;

procedure TcxCustomGridCustomizationPopup.SetCheckListBoxSelectedItems(AItems: TList; AFocusedItem: TObject);
var
  I, AIndex, AItemIndex, AItemIndex2: Integer;
begin
  AItemIndex := -1;
  AItemIndex2 := -1;
  TdxCustomCheckListBoxAccess(CheckListBox).Selection.Clear;
  for I := 0 to AItems.Count - 1 do
  begin
    AIndex := CheckListBox.Items.IndexOfObject(AItems[I]);
    if AIndex <> -1 then
    begin
      CheckListBox.Selected[AIndex] := True;
      if AItemIndex2 = -1 then
        AItemIndex2 := AIndex;
      if AItems[I] = AFocusedItem then
        AItemIndex := AIndex;
    end;
  end;
  if AItemIndex = -1 then
    AItemIndex := CheckListBox.Items.IndexOfObject(AFocusedItem);
  CheckListBox.ItemIndex := IfThen(AItemIndex <> -1, AItemIndex, AItemIndex2);
end;

procedure TcxCustomGridCustomizationPopup.CloseUp;
begin
  try
    inherited CloseUp;
  finally
    CustomizationControl.Clear;
  end;
end;

function TcxCustomGridCustomizationPopup.CanAddCommands: Boolean;
begin
  Result := GridView.OptionsCustomize.ItemsQuickCustomizationShowCommands;
end;

procedure TcxCustomGridCustomizationPopup.UpdateCommandListBoxItemsChecking;
begin
  if CustomizationControl.HasCommands then
  begin
    CustomizationControl.CommandListBox.States[0] := CustomizationControl.GetCheckingAllState;
    CustomizationControl.CommandListBox.Checked[1] := IsCheckListBoxSorted;
  end;
end;

procedure TcxCustomGridCustomizationPopup.Popup;
begin
  GridView.Focused := True;  // to prevent popup from closing because of ViewInfo recalculation
  inherited Popup;
end;

{ TcxCustomGridItemsCustomizationPopup }

procedure TcxCustomGridItemsCustomizationPopup.AddCheckListBoxItems;
var
  AItem: TcxCustomGridTableItem;
  I: Integer;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    CheckListBox.Sorted := False;
    for I := 0 to GridView.ItemCount - 1 do
    begin
      AItem := GridView.Items[I];
      if AItem.VisibleInQuickCustomizationPopup then
        CheckListBox.Items.AddObject(AItem.GetAlternateCaption, AItem).Checked := AItem.Visible;
    end;
    SetQuickCustomizationSortOptions;
    ApplyCheckListBoxSorting;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

function TcxCustomGridItemsCustomizationPopup.GetDropDownCount: Integer;
begin
  Result := GridView.OptionsCustomize.ItemsQuickCustomizationMaxDropDownCount;
end;

procedure TcxCustomGridItemsCustomizationPopup.SetQuickCustomizationSorted(AValue: Boolean);
begin
  GridView.OptionsCustomize.ItemsQuickCustomizationSorted := AValue;
end;

function TcxCustomGridItemsCustomizationPopup.SupportsItemMoving: Boolean;
begin
  Result := GridView.OptionsCustomize.SupportsItemsQuickCustomizationReordering;
end;

procedure TcxCustomGridItemsCustomizationPopup.SynchronizeTableItemsVisibilityWithListBox;
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    (CheckListBox.Items[I].Data as TcxCustomGridTableItem).Visible := CheckListBox.Checked[I];
end;

procedure TcxCustomGridItemsCustomizationPopup.DoItemPosChanged(AItem: TObject);
begin
//
end;

procedure TcxCustomGridItemsCustomizationPopup.HandleItemClicked(AItem: TObject; AChecked: Boolean);
begin
  TcxCustomGridTableItem(AItem).Visible := AChecked;
  DoItemPosChanged(AItem);
  SetGridModified;
end;

function TcxCustomGridItemsCustomizationPopup.IsGridItem(AItem: TObject): Boolean;
begin
  Result := AItem is TcxCustomGridTableItem;
end;

function TcxCustomGridItemsCustomizationPopup.IsVisibleGridItem(AItem: TObject): Boolean;
begin
  Result := IsGridItem(AItem) and TcxCustomGridTableItem(AItem).Visible;
end;

function TcxCustomGridItemsCustomizationPopup.GetItemIndex(AItem: TObject): Integer;
begin
  Result := TcxCustomGridTableItem(AItem).Index;
end;

procedure TcxCustomGridItemsCustomizationPopup.SetItemIndex(AItem: TObject;
  AIndex: Integer);
begin
  if TcxCustomGridTableItem(AItem).Index < AIndex then
    Dec(AIndex);
  TcxCustomGridTableItem(AItem).Index := AIndex;
  GridView.Controller.DesignerModified;
end;

function TcxCustomGridItemsCustomizationPopup.CanMoveItem(AItem: Pointer): Boolean;
begin
  Result := TcxCustomGridTableItem(AItem).Options.Moving and
    inherited CanMoveItem(AItem);
end;


{ TcxGridEditingController }

constructor TcxGridEditingController.Create(AController: TcxCustomGridTableController);
begin
  inherited Create(AController.GridView);
  FController := AController;
  FMultilineEdit := CreateMultilineEdit;
end;

destructor TcxGridEditingController.Destroy;
begin
  FMultilineEdit.Free;
  inherited Destroy;
end;

function TcxGridEditingController.GetEditingCellViewInfo: TcxGridTableDataCellViewInfo;
begin
  Result := FEditingItem.FocusedCellViewInfo;
end;

function TcxGridEditingController.GetEditingProperties: TcxCustomEditProperties;
begin
  if FEditingItem = nil then
    Result := nil
  else
    Result := FEditingItem.GetPropertiesForEdit;
end;

function TcxGridEditingController.GetGridView: TcxCustomGridTableView;
begin
  Result := FController.GridView;
end;

function TcxGridEditingController.GetIsEditing: Boolean;
begin
  Result := FEditingItem <> nil;
end;

procedure TcxGridEditingController.HideEditOnScrollContent;
begin
  inherited HideEditOnScrollContent;
  if EditingCellViewInfo <> nil then
    EditingCellViewInfo.Update;
end;

procedure TcxGridEditingController.SetEditingItem(Value: TcxCustomGridTableItem);
begin
  if FEditingItem <> Value then
  begin
    if FEditingItemSetting then Exit;
    FEditingItemSetting := True;
    try
      if Value <> nil then
      begin
        if not Value.CanEdit or not GridView.DoEditing(Value) then Exit;
        Value.FocusWithSelection;
      end;
      HideEdit(False);
      FEditingItem := Value;
      if IsEditing then
        try
          ShowEdit(Value);
          if not FEditPreparing and (FEdit = nil) then
            FEditingItem := nil;
        except
          FEditingItem := nil;
          raise;
        end;
    finally
      FEditingItemSetting := False;
    end;
  end;
end;

procedure TcxGridEditingController.AfterAssignMultilineEditProperties;
begin
  if Assigned(Edit.RepositoryItem) then
    with MultiLineEdit.ActiveProperties do
    begin
      if not Assigned(OnChange) then
        OnChange := Edit.InternalProperties.OnChange;
      if not Assigned(OnEditValueChanged) then
        OnEditValueChanged := Edit.InternalProperties.OnEditValueChanged;
      if not Assigned(OnValidate) then
        OnValidate := Edit.InternalProperties.OnValidate;
    end;
  MultiLineEdit.ActiveProperties.ValidateOnEnter := False;
end;

procedure TcxGridEditingController.BeforeAssignMultilineEditProperties;
begin
  MultiLineEdit.AutoHeight := AutoHeight;
  MultiLineEdit.ActiveProperties.WantReturns := False;
  MultiLineEdit.BorderColor := GridView.OptionsView.EditAutoHeightBorderColor;
  if MultiLineEdit.BorderColor <> clNone then
  begin
    if MultiLineEdit.BorderColor = clDefault then
      MultiLineEdit.BorderColor := GridView.OptionsView.GetGridLineColor;
  end;
  MultiLineEdit.ActiveProperties.ScrollBars := ssNone;
  MultiLineEdit.ActiveProperties.VisibleLineCount := 0;
  MultiLineEdit.ActiveProperties.WantTabs := False;
  MultiLineEdit.ActiveProperties.WordWrap := True;
end;

function TcxGridEditingController.CanUpdateMultilineEditHeight: Boolean;
begin
  Result := CanUpdateEditValue and not Edit.IsHiding;
end;

function TcxGridEditingController.CanUseAutoHeightEditor: Boolean;
begin
  Result := Controller.CanUseAutoHeightEditing and EditingItem.CanEditAutoHeight and
    (esoEditingAutoHeight in FEdit.ActiveProperties.GetSupportedOperations);
end;

function TcxGridEditingController.GetAdjustedMultilineEditBounds(const ABounds: TRect): TRect;
begin
  Result := GridView.ViewInfo.GetMultilineEditorBounds(ABounds,
    Max(MultiLineEdit.CalculatedHeight, MultiLineEditMinHeight),
    MultiLineEdit.AutoHeight);
end;

procedure TcxGridEditingController.MultilineEditTextChanged;
var
  R: TRect;
begin
  if CanUpdateMultilineEditHeight then
  begin
    R := GetAdjustedMultilineEditBounds(MultiLineEdit.BoundsRect);
    if cxRectIsEqual(MultiLineEdit.BoundsRect, R) then
      Exit;
    if AutoHeight = eahRow then
    begin
      if MultiLineEdit.ModifiedAfterEnter then
      begin
        GridView.SizeChanged(True);
        if cxRectHeight(R) > MultiLineEdit.Height then
          MultiLineEdit.Height := cxRectHeight(R);
      end;
    end
    else
      MultiLineEdit.BoundsRect := R;
  end;
end;

procedure TcxGridEditingController.StartEditAutoHeight(AHeightChanged: Boolean);
begin
  if CanUpdateMultilineEditHeight then
  begin
    if AutoHeight = eahEditor then
      MultiLineEdit.Invalidate
    else
      if AHeightChanged then
        GridView.SizeChanged(True);
  end;
end;

procedure TcxGridEditingController.UpdateMultilineEditBounds(const ABounds: TRect);
begin
  if not MultiLineEdit.IsHiding then
  begin
    if MultiLineEdit.ModifiedAfterEnter then
      MultiLineEdit.BoundsRect := GetAdjustedMultilineEditBounds(ABounds)
    else
      MultiLineEdit.BoundsRect := ABounds;
  end;
end;

procedure TcxGridEditingController.AfterViewInfoCalculate;
begin
  ValidateEditVisibility;
end;

procedure TcxGridEditingController.BeforeViewInfoCalculate;
begin
  FNeedValidateEditVisibility := True;
end;

function TcxGridEditingController.CanInitEditing: Boolean;
begin
  Result := EditingItem.CanInitEditing;
end;

function TcxGridEditingController.CanUpdateEdit: Boolean;
begin
  Result := (FEditingItem <> nil) and (EditingCellViewInfo <> nil) and not FEditingItem.EditPartVisible and
    not Controller.Tracking;
end;

function TcxGridEditingController.CanUpdateEditValue: Boolean;
begin
  Result := not GridView.IsPattern and inherited CanUpdateEditValue and
    not (NeedCellViewInfoWhenUpdateEditValue and (EditingCellViewInfo = nil));
end;

function TcxGridEditingController.CellEditBoundsToEditBounds(ACellEditBounds: TRect): TRect;
begin
  Result := ACellEditBounds;
  if AutoHeight = eahEditor then
    InflateRect(Result, 1, 1);
end;

procedure TcxGridEditingController.CheckEdit;
begin
  if IsEditing and (FEditingItem.FocusedCellViewInfo = nil) then
  begin
    if Controller.FocusedRecord = nil then
      HideEdit(False);
  end
  else
    ShowEdit;
end;

procedure TcxGridEditingController.CheckUsingMultilineEdit;
begin
  FIsEditAutoHeight := CanUseAutoHeightEditor;
  if not FIsEditAutoHeight then Exit;
  FMultiLineEdit.ActiveProperties.BeginUpdate;
  try
    BeforeAssignMultilineEditProperties;
    FMultiLineEdit.ActiveProperties.Assign(FEdit.ActiveProperties);
    AfterAssignMultilineEditProperties;
  finally
    FMultiLineEdit.ActiveProperties.EndUpdate;
  end;
  FEdit := FMultiLineEdit;
end;

procedure TcxGridEditingController.ClearEditingItem;
begin
  EditingItem := nil;
end;

function TcxGridEditingController.CreateMultilineEdit: TcxAutoHeightInplaceEdit;
begin
  Result := TcxAutoHeightInplaceEdit.Create(Self);
end;

procedure TcxGridEditingController.DoEditKeyDown(var Key: Word; Shift: TShiftState);
var
  AModified: Boolean;
begin
  GridView.DoEditKeyDown(FEditingItem, Edit, Key, Shift);
  case Key of
    VK_RETURN:
      begin
        HideEdit(True);
        if GridView.OptionsBehavior.GoToNextCellOnEnter then
        begin
          FController.BlockRecordKeyboardHandling := True;
          try
            FController.DoKeyDown(Key, Shift);
          finally
            FController.BlockRecordKeyboardHandling := False;
          end;
          ShowEdit;
        end
        else
          Controller.CheckEdit;
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        AModified := Edit.EditModified;
        HideEdit(False);
        Controller.CheckEdit;
        if AModified then Key := 0;
      end;
    VK_DELETE:
      if Shift = [ssCtrl] then
        Controller.DoKeyDown(Key, Shift);
  end;
end;

procedure TcxGridEditingController.DoHideEdit(Accept: Boolean);
var
  APrevAllowCheckEdit: Boolean;
begin
  if Accept then
  begin
    Edit.Deactivate;
    PostEditingData;
    Edit.ActiveProperties.Update(FEditingItem.GetProperties(Controller.FocusedRecord));
  end;
  EditingItem := nil;
  if GridView.Focused then GridView.TabStop := True;
  if Edit <> nil then
  begin
    UninitEdit;
    if Edit.Focused and GridView.Focused then
    begin
      Edit.EditModified := False;
      if not GridView.IsDestroying then
      begin
        APrevAllowCheckEdit := FController.AllowCheckEdit;
        FController.AllowCheckEdit := False;
        try
          FController.DoSetFocus(False);
        finally
          FController.AllowCheckEdit := APrevAllowCheckEdit;
        end;
      end;
    end
    else
      Controller.InvalidateFocusedRecord;
  end;
end;

procedure TcxGridEditingController.DoUpdateEdit;
var
  ACellViewInfo: TcxGridTableDataCellViewInfo;
begin
  CancelEditUpdatePost;
  if CanUpdateEdit then
  begin
    ACellViewInfo := EditingCellViewInfo;
    if IsRectEmpty(ACellViewInfo.Bounds) then Exit;
    if EditPreparing then
    begin
      UpdateEditStyle(ACellViewInfo);
      GridView.DoInitEdit(FEditingItem, FEdit);
      ACellViewInfo := EditingCellViewInfo;
      FEditPlaceBounds := CellEditBoundsToEditBounds(ACellViewInfo.EditBounds);
    end
    else
    begin
      if FUpdateEditStyleNeeded then
        UpdateEditStyle(ACellViewInfo);
      UpdateEditBounds(CellEditBoundsToEditBounds(ACellViewInfo.EditBounds));
      GridView.DoUpdateEdit(FEditingItem, Edit);
    end;
  end;
end;

procedure TcxGridEditingController.FocusedRecordChanged;
begin
  if HideEditOnFocusedRecordChange then
    HideEdit(GridView.DataController.IsEditing)
  else
    FUpdateEditStyleNeeded := True;
end;

function TcxGridEditingController.GetAutoHeight: TcxInplaceEditAutoHeight;
begin
  if FIsEditAutoHeight then
    Result := EditingItem.GetEditAutoHeight
  else
    Result := eahNone;
end;

function TcxGridEditingController.GetCancelEditingOnExit: Boolean;
begin
  Result := FController.CancelEditingOnExit;
end;

function TcxGridEditingController.GetEditParent: TWinControl;
begin
  Result := FController.Site;
end;

function TcxGridEditingController.GetFocusedCellBounds: TRect;
var
  ACellViewInfo: TcxGridTableDataCellViewInfo;
begin
  ACellViewInfo := EditingCellViewInfo;
  if ACellViewInfo = nil then
    Result := cxNullRect
  else
    Result := ACellViewInfo.Bounds;
end;

function TcxGridEditingController.GetFocusRectBounds: TRect;
var
  AFocusedRecordViewInfo: TcxCustomGridRecordViewInfo;
begin
  Result := cxNullRect;
  if GridView.OptionsView.FocusRect then
  begin
    AFocusedRecordViewInfo := Controller.FocusedRecord.ViewInfo;
    if AFocusedRecordViewInfo <> nil then
      Result := AFocusedRecordViewInfo.FocusRectBounds
  end;
end;

function TcxGridEditingController.GetHideEditOnExit: Boolean;
begin
  Result := True{
    not GridView.OptionsBehavior.AlwaysShowEditor or
    GridView.Control.IsFocused};
end;

function TcxGridEditingController.GetHideEditOnFocusedRecordChange: Boolean;
begin
  Result := not GridView.OptionsBehavior.AlwaysShowEditor or
    (FEditingItem <> nil) and
      (IsEditAutoHeight or
       FEditingItem.HasCustomPropertiesHandler or FEditingItem.HasCustomPropertiesForEditHandler or
       FEditingItem.ShowButtons(False) or
       (esoAlwaysHotTrack in FEditingItem.GetProperties.GetSupportedOperations)) or
       Assigned(GridView.OnEditing) or Assigned(GridView.OnInitEdit) or Assigned(GridView.OnUpdateEdit);
end;

procedure TcxGridEditingController.InitEdit;
begin
  CheckUsingMultilineEdit;
  inherited InitEdit;
  if IsEditAutoHeight then
    InitMultilineEdit;
end;

procedure TcxGridEditingController.InitMultilineEdit;
var
  ARowViewInfo: TcxCustomGridRecordViewInfo;
begin
  ARowViewInfo := Controller.GetViewInfo.RecordsViewInfo.GetRealItem(Controller.FocusedRecord);
  if ARowViewInfo = nil then
    FMultiLineEditMinHeight := -1
  else
    FMultiLineEditMinHeight := ARowViewInfo.CalculateMultilineEditMinHeight;
end;

function TcxGridEditingController.IsNeedInvokeEditChangedEventsBeforePost: Boolean;
begin
  Result := Edit.ActiveProperties.ImmediatePost and
    (dcoImmediatePost in GridView.DataController.Options);
end;

function TcxGridEditingController.NeedCellViewInfoWhenUpdateEditValue: Boolean;
begin
  Result := True;
end;

procedure TcxGridEditingController.PostEditingData;
begin
  GridView.DataController.PostEditingData;
end;

function TcxGridEditingController.PrepareEdit(AItem: TcxCustomGridTableItem;
  AOnMouseEvent: Boolean): Boolean;
var
  AProperties: TcxCustomEditProperties;
  AAssignRepositoryItem: Boolean;
begin
  Result := False;
  try
    FController.CancelCheckEditPost;
    if EditPreparing or EditHiding or (AItem = nil) or (Controller.FocusedRecord = nil) then
      Exit;
    FEditPreparing := True;
    try
      if AItem.Editing and not FEditingItemSetting then
      begin
        Result := (Edit <> nil) and (FController.Site.Focused and not Edit.IsFocused or AOnMouseEvent);
        if Result then
          EditingCellViewInfo.Invalidate(True);
        Exit;
      end;
      Result := FController.Site.Focused;
      if not Result then
        Exit;

      AItem.Editing := True;
      Result := AItem.Editing;
      if not Result then
        Exit;

      Result := not AItem.EditPartVisible;
      if not Result then
      begin
        AItem.Editing := False;
        Exit;
      end;

      try
        AProperties := EditingProperties;
        AItem.ValidateEditData(AProperties);
        AAssignRepositoryItem := NeedAssignRepositoryItem(AItem.Properties, AItem.RepositoryItem, AProperties);

        GridView.PatternGridView.IgnorePropertiesChanges := True;
        try
          FEdit := EditList.GetEdit(AProperties);
          if AAssignRepositoryItem then
            Edit.RepositoryItem := AItem.RepositoryItem;
        finally
          GridView.PatternGridView.IgnorePropertiesChanges := False;
        end;
      except
        AItem.Editing := False;
        Result := False;
        raise;
      end;

      EditingCellViewInfo.Invalidate(True);
      InitEdit;
    finally
      FEditPreparing := False;
    end;
  finally
    if Result then
      GridView.TabStop := False;
  end;
end;

procedure TcxGridEditingController.StartEditingByTimer;
begin
  FEditShowingTimerItem.Editing := True;
end;

procedure TcxGridEditingController.UpdateEditBounds(const AEditBounds: TRect);
begin
  if IsEditAutoHeight then
    UpdateMultilineEditBounds(AEditBounds)
  else
    FEdit.BoundsRect := AEditBounds;
end;

procedure TcxGridEditingController.UpdateEditStyle(
  AEditCellViewInfo: TcxGridTableDataCellViewInfo);
begin
  AEditCellViewInfo.InitStyle;
  FEdit.Style := AEditCellViewInfo.Style;
  FUpdateEditStyleNeeded := False;
end;

procedure TcxGridEditingController.UpdateInplaceParamsPosition;
var
  ACellViewInfo: TcxGridTableDataCellViewInfo;
begin
  ACellViewInfo := EditingCellViewInfo;
  if ACellViewInfo <> nil then
    Edit.InplaceParams.Position := ACellViewInfo.GetInplaceEditPosition;
end;

procedure TcxGridEditingController.ValidateEditVisibility;
begin
  if IsEditing and (FNeedValidateEditVisibility or not IsEditVisible or FEditingItem.EditPartVisible) then
  begin
    CancelEditUpdatePost;
    if not EditPreparing then
      HideEditOnScrollContent;
  end;
end;

function TcxGridEditingController.GetDataController: TcxCustomDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridEditingController.GetValue: TcxEditValue;
begin
  Result := EditingItem.EditValue;
  GridView.DoInitEditValue(EditingItem, Edit, Result);
end;

procedure TcxGridEditingController.SetValue(const AValue: TcxEditValue);
begin
  FEditingItem.EditValue := AValue;
end;

procedure TcxGridEditingController.EditAfterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AGridViewLink: TcxObjectLink;
begin
  AGridViewLink := cxAddObjectLink(GridView);
  try
    if FController.IsKeyForController(Key, Shift) then
      FController.DoKeyDown(Key, Shift);
    if AGridViewLink.Ref = nil then
      Abort;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxGridEditingController.EditChanged(Sender: TObject);
begin
  inherited EditChanged(Sender);
  GridView.DoEditChanged(FEditingItem);
end;

procedure TcxGridEditingController.EditDblClick(Sender: TObject);
begin
  GridView.DoEditDblClick(FEditingItem, Edit);
end;

procedure TcxGridEditingController.EditFocusChanged(Sender: TObject);
begin
  TcxControlAccess(FController.Site).FocusChanged;
end;

procedure TcxGridEditingController.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_CONTROL, VK_SHIFT, VK_MENU]) then
    FController.MakeFocusedItemVisible;
  DoEditKeyDown(Key, Shift);
end;

procedure TcxGridEditingController.EditKeyPress(Sender: TObject; var Key: Char);
begin
  GridView.DoEditKeyPress(FEditingItem, Edit, Key);
end;

procedure TcxGridEditingController.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GridView.DoEditKeyUp(FEditingItem, Edit, Key, Shift);
end;

procedure TcxGridEditingController.EditValueChanged(Sender: TObject);
begin
  inherited EditValueChanged(Sender);
  GridView.DoEditValueChanged(FEditingItem);
end;

procedure TcxGridEditingController.HideEdit(Accept: Boolean);
begin
  FController.CancelCheckEditPost;
  inherited HideEdit(Accept);
end;

function TcxGridEditingController.IsRecordModified: Boolean;
begin
  Result := (dceModified in GridView.DataController.EditState) or
    (Edit <> nil) and Edit.ModifiedAfterEnter;
end;

procedure TcxGridEditingController.ShowEdit;
begin
  ShowEdit(nil);
end;

procedure TcxGridEditingController.ShowEdit(AItem: TcxCustomGridTableItem);
begin
  try
    if AItem = nil then
      AItem := FController.FocusedItem;
    if PrepareEdit(AItem, False) then
      Edit.Activate(AItem.FEditData);
  except
    HideEdit(False);
    raise;
  end;
end;

procedure TcxGridEditingController.ShowEdit(AItem: TcxCustomGridTableItem; Key: Char);
begin
  try
    if PrepareEdit(AItem, False) then
      Edit.ActivateByKey(Key, AItem.FEditData);
  except
    HideEdit(False);
    raise;
  end;
end;

procedure TcxGridEditingController.ShowEdit(AItem: TcxCustomGridTableItem; Shift: TShiftState; X, Y: Integer);
begin
  try
    if PrepareEdit(AItem, True) then
    begin
      GridView.HideTouchScrollUI(True);
      Edit.ActivateByMouse(Shift, X, Y, AItem.FEditData);
    end;
  except
    HideEdit(False);
    raise;
  end;
end;

procedure TcxGridEditingController.UpdateEdit;
begin
  inherited UpdateEdit;
  FNeedValidateEditVisibility := False;
end;

procedure TcxGridEditingController.StartEditShowingTimer(AItem: TcxCustomGridTableItem);
begin
  FEditShowingTimerItem := AItem;
  inherited StartEditShowingTimer;
end;

(*{ TcxCustomGridDragOpenInfo }

function TcxCustomGridDragOpenInfo.Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  Result := ClassType = AInfo.ClassType;
end;
*)
{ TcxGridDragOpenInfoExpand }

constructor TcxGridDragOpenInfoExpand.Create(AGridRecord: TcxCustomGridRecord);
begin
  inherited Create;
  GridRecord := AGridRecord;
end;

function TcxGridDragOpenInfoExpand.Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  Result := inherited Equals(AInfo) and
    (GridRecord = TcxGridDragOpenInfoExpand(AInfo).GridRecord);
end;

procedure TcxGridDragOpenInfoExpand.Run;
begin
  GridRecord.Expand(False);
end;

{ TcxCustomGridTableController }

constructor TcxCustomGridTableController.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FAllowAppendRecord := True;
  FAllowCheckEdit := True;
  FEditingController := GetEditingControllerClass.Create(Self);
  FFindPanel := CreateFindPanel;
  FFocusOnRecordFocusing := True;
  FPrevFocusedRecordIndex := cxRecordIndexNone;
end;

destructor TcxCustomGridTableController.Destroy;
begin
  FFilterPopup.Free;
  FFilterMRUItemsPopup.Free;
  FItemsCustomizationPopup.Free;
  CancelGridModeBufferCountUpdate;
  FreeAndNil(FFindPanel);
  FreeAndNil(FEditingController);
  inherited Destroy;
end;

function TcxCustomGridTableController.GetEditingItem: TcxCustomGridTableItem;
begin
  Result := FEditingController.EditingItem;
end;

function TcxCustomGridTableController.GetFilterMRUItemsPopup: TcxGridFilterMRUItemsPopup;
begin
  if FFilterMRUItemsPopup = nil then
    FFilterMRUItemsPopup := GetFilterMRUItemsPopupClass.Create(GridView);
  Result := FFilterMRUItemsPopup;
end;

function TcxCustomGridTableController.GetFilterPopup: TcxGridFilterPopup;
begin
  if FFilterPopup = nil then
    FFilterPopup := GetFilterPopupClass.Create(GridView);
  Result := FFilterPopup;
end;

function TcxCustomGridTableController.GetFocusedItemIndex: Integer;
begin
  if FFocusedItem = nil then
    Result := -1
  else
    Result := FFocusedItem.VisibleIndex;
end;

function TcxCustomGridTableController.GetFocusedRecordIndex: Integer;
begin
  Result := DataController.GetFocusedRowIndex;
end;

function TcxCustomGridTableController.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableController.GetIncSearchingItem: TcxCustomGridTableItem;
begin
  if IsIncSearching then
    Result := GridView.Items[DataController.Search.ItemIndex]
  else
    Result := nil;
end;

function TcxCustomGridTableController.GetIncSearchingText: string;
begin
  Result := DataController.Search.SearchText;
end;

function TcxCustomGridTableController.GetInternalTopRecordIndex: Integer;
begin
  if NeedCheckInternalTopRecordIndex then
    CheckInternalTopRecordIndex(FInternalTopRecordIndex);
  Result := FInternalTopRecordIndex;
end;

function TcxCustomGridTableController.GetIsEditing: Boolean;
begin
  Result := FEditingController.IsEditing;
end;

function TcxCustomGridTableController.GetIsIncSearching: Boolean;
begin
  Result := DataController.Search.Searching;
end;

function TcxCustomGridTableController.GetIsItemMoving: Boolean;
begin
  Result := FMovingItem <> nil;
end;

function TcxCustomGridTableController.GetItemsCustomizationPopup: TcxCustomGridItemsCustomizationPopup;
begin
  if FItemsCustomizationPopup = nil then
    FItemsCustomizationPopup := GetItemsCustomizationPopupClass.Create(GridView);
  Result := FItemsCustomizationPopup;
end;

function TcxCustomGridTableController.GetLastVisibleRecordIndex: Integer;
begin
  if IsRecordPixelScrolling then
    Result := InternalTopRecordIndex + ViewInfo.RecordsViewInfo.PartVisibleCount - 1
  else
    Result := InternalTopRecordIndex + ViewInfo.VisibleRecordCount - 1;
end;

function TcxCustomGridTableController.GetMasterController: TcxCustomGridTableController;
begin
  if GridView.IsDetail then
    Result := GridView.MasterGridView.Controller as TcxCustomGridTableController
  else
    Result := nil;
end;

function TcxCustomGridTableController.GetMultiSelect: Boolean;
begin
  Result := GridView.OptionsSelection.MultiSelect;
end;

function TcxCustomGridTableController.GetNewItemRecordFocused: Boolean;
begin
  Result := DataController.NewItemRowFocused;
end;

function TcxCustomGridTableController.GetPrevFocusedRecordIndex: Integer;
begin
  if FPrevFocusedRecordIndex = -1 then
    Result := FocusedRecordIndex
  else
    Result := FPrevFocusedRecordIndex;
end;

function TcxCustomGridTableController.GetScrollRecordCount: Integer;
begin
  Result := LastScrollRecordIndex - FirstScrollRecordIndex + 1;
end;

function TcxCustomGridTableController.GetSelectedRecord(Index: Integer): TcxCustomGridRecord;
begin
  Index := DataController.GetSelectedRowIndex(Index);
  if Index = -1 then
    Result := nil
  else
    Result := ViewData.Records[Index];
end;

function TcxCustomGridTableController.GetSelectedRecordCount: Integer;
begin
  Result := DataController.GetSelectedCount;
end;

function TcxCustomGridTableController.GetViewData: TcxCustomGridTableViewData;
begin
  Result := TcxCustomGridTableViewData(inherited ViewData);
end;

function TcxCustomGridTableController.GetViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited ViewInfo);
end;

procedure TcxCustomGridTableController.SetDragHighlightedRecord(Value: TcxCustomGridRecord);
var
  APrevDragHighlightedRecord: TcxCustomGridRecord;
begin
  if FDragHighlightedRecord <> Value then
  begin
    APrevDragHighlightedRecord := FDragHighlightedRecord;
    FDragHighlightedRecord := Value;
    if APrevDragHighlightedRecord <> nil then
      APrevDragHighlightedRecord.Invalidate;
    if FDragHighlightedRecord <> nil then
      FDragHighlightedRecord.Invalidate;
  end;
end;

procedure TcxCustomGridTableController.SetEditingItem(Value: TcxCustomGridTableItem);
begin
  FEditingController.EditingItem := Value;
end;

procedure TcxCustomGridTableController.SetFocusedItem(Value: TcxCustomGridTableItem);
var
  APrevFocusedItem: TcxCustomGridTableItem;
begin
  if (Value <> nil) and not Value.CanFocus(FocusedRecord) then Exit;
  if FFocusedItem <> Value then
  begin
    APrevFocusedItem := FFocusedItem;
    if (FFocusedItem <> nil) and not FEditingController.FEditingItemSetting then
      FEditingController.HideEdit(True);
    FFocusedItem := Value;
    FocusedItemChanged(APrevFocusedItem);
    CheckEdit;
  end
  else
    MakeFocusedItemVisible;
end;

procedure TcxCustomGridTableController.SetFocusedItemIndex(Value: Integer);
begin
  if (Value < -1) or (Value >= GridView.VisibleItemCount) then Exit;
  if Value = -1 then
    FocusedItem := nil
  else
    FocusedItem := GridView.VisibleItems[Value];
end;

procedure TcxCustomGridTableController.SetFocusedRecordIndex(Value: Integer);
var
  AIndexesAreEqual: Boolean;
begin
  if not CanChangeFocusedRecordIndex(FocusedRecordIndex, Value) then
    Exit;
  AIndexesAreEqual := FocusedRecordIndex = Value;
  if not DataController.ChangeFocusedRowIndex(Value) then Exit;
  if FFocusOnRecordFocusing then
  begin
    if AIndexesAreEqual then MakeFocusedRecordVisible;
    if FocusedRecord <> nil then GridView.Focused := True;
  end;
end;

procedure TcxCustomGridTableController.SetIncSearchingText(const Value: string);

  function GetItemIndex: Integer;
  begin
    if IsIncSearching then
      Result := IncSearchingItem.Index
    else
      if ItemForIncSearching = nil then
        Result := -1
      else
        Result := ItemForIncSearching.Index;
  end;

begin
  if (IncSearchingText <> Value) and (GetItemIndex <> -1) then
    if Value = '' then
      CancelIncSearching
    else
      DataController.Search.Locate(GetItemIndex, Value, False, not IsMultiSelectPersistent)
end;

procedure TcxCustomGridTableController.SetInternalTopRecordIndex(Value: Integer);
begin
  if (FInternalTopRecordIndex <> Value) or (PixelScrollRecordOffset <> 0) then
  begin
    SetTopRecordIndexWithOffset(Value, 0);
  end;
end;

procedure TcxCustomGridTableController.SetNewItemRecordFocused(Value: Boolean);
begin
  DataController.NewItemRowFocused := Value;
end;

procedure TcxCustomGridTableController.SetPixelScrollRecordOffset(const Value: Integer);
begin
  if FPixelScrollRecordOffset <> Value then
    SetTopRecordIndexWithOffset(InternalTopRecordIndex, Value);
end;

procedure TcxCustomGridTableController.SetScrollDirection(Value: TcxDirection);
begin
  if FScrollDirection <> Value then
  begin
    DestroyScrollTimer;
    FScrollDirection := Value;
    if FScrollDirection <> dirNone then
      CreateScrollTimer;
  end;
end;

procedure TcxCustomGridTableController.SetTopRecordIndex(Value: Integer);
begin
  CheckTopRecordIndex(Value);
  InternalTopRecordIndex := Value;
end;

procedure TcxCustomGridTableController.SetTracking(Value: Boolean);
begin
  if Value <> Tracking then
  begin
    FTracking := Value;
    if IsEditing then
      if Tracking then
        EditingController.HideEditOnScrollContent
      else
        EditingController.ShowEditAfterScrollContent;
  end;
end;

procedure TcxCustomGridTableController.DragScrollTimerHandler(Sender: TObject);
begin
  Site.ScrollContent(FDragScrollDirection);
  Site.Update;
end;

procedure TcxCustomGridTableController.GridModeBufferCountUpdateTimerHandler(Sender: TObject);
begin
  CheckGridModeBufferCountUpdatePost;
end;

procedure TcxCustomGridTableController.PullFocusingScrollingTimerHandler(Sender: TObject);
begin
  DoPullFocusingScrolling(FPullFocusingScrollingDirection);
  Site.Update;
  SavePullFocusingPos;
end;

procedure TcxCustomGridTableController.CreateScrollTimer;
begin
  FScrollTimer := TcxTimer.Create(nil);
  with FScrollTimer do
  begin
    Interval := GetScrollDataTimeInterval(FScrollDirection);
    OnTimer := ScrollTimerHandler;
  end;
end;

procedure TcxCustomGridTableController.DestroyScrollTimer;
begin
  FreeAndNil(FScrollTimer);
end;

procedure TcxCustomGridTableController.ScrollTimerHandler(Sender: TObject);
var
  AAccepted: Boolean;
begin
  if CanScrollData(FScrollDirection) then
  begin
    BeforeScrolling;
    ScrollData(FScrollDirection);
    if DragAndDropObject <> nil then
    begin
      Site.Update;
      AfterScrolling;
      TcxControlAccess(Site).DragAndDrop(
        Site.ScreenToClient(GetMouseCursorPos), AAccepted);
    end;
  end;
end;

procedure TcxCustomGridTableController.DoEnter;
begin
  inherited;
  PostCheckEdit;
end;

procedure TcxCustomGridTableController.DoExit;
begin
  if CancelEditingOnExit then
    DataController.Cancel
  else
    DataController.PostEditingData;
  inherited;
end;

procedure TcxCustomGridTableController.AfterPaint;
begin
  inherited;
  ProcessCheckEditPost;
end;

function TcxCustomGridTableController.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := inherited AllowPan(AScrollKind) and ViewInfo.RecordsViewInfo.AllowPan(AScrollKind);
end;

procedure TcxCustomGridTableController.BeforePaint;
begin
  inherited;
  FEditingController.CheckEditUpdatePost;
end;

function TcxCustomGridTableController.CanCancelDragStartOnCaptureObjectClear: Boolean;
begin
  Result := not EditingController.EditHiding;
end;

function TcxCustomGridTableController.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := inherited CanFocusOnClick(X, Y) and
    (((Site <> nil) and not Site.IsFocused) or
     not IsEditing or
    (not GridView.OptionsBehavior.AlwaysShowEditor and
     not (ViewInfo.GetHitTest(X, Y).HitTestCode = htNavigator) and
     TcxCustomEditAccess(FEditingController.Edit).InternalValidateEdit));
end;

procedure TcxCustomGridTableController.CheckInternalTopRecordIndex(var AIndex: Integer);
begin
  CheckTopRecordIndex(AIndex);
end;

procedure TcxCustomGridTableController.DetailFocused(ADetail: TcxCustomGridView);
var
  APrevFocusOnRecordFocusing: Boolean;
begin
  APrevFocusOnRecordFocusing := FFocusOnRecordFocusing;
  FFocusOnRecordFocusing := False;
  try
    inherited;
    FIsDetailRecordFocusing := True;
    try
      FocusedRecordIndex := ADetail.MasterGridRecordIndex;
    finally
      FIsDetailRecordFocusing := False;
    end;
  finally
    FFocusOnRecordFocusing := APrevFocusOnRecordFocusing;
  end;
end;

procedure TcxCustomGridTableController.RemoveFocus;
begin
  inherited RemoveFocus;
  CheckFocusedRecordSelectionWhenExit(FocusedRecord);
  InvalidateFocusedRecord;
end;

procedure TcxCustomGridTableController.SetFocus(ANotifyMaster: Boolean);
begin
  if not FEditingController.CanRemoveEditFocus then Exit;
  inherited;
  InvalidateFocusedRecord;
  PostCheckEdit;
end;

procedure TcxCustomGridTableController.AfterOffset;
begin
  Site.PostMouseMove;
  FEditingController.UpdateEdit;
  FEditingController.AfterViewInfoCalculate;
  UpdateScrollBars;
end;

procedure TcxCustomGridTableController.BeforeOffset;
begin
  FEditingController.BeforeViewInfoCalculate;
end;

function TcxCustomGridTableController.CanAppend(ACheckOptions: Boolean): Boolean;
begin
  Result := AllowAppendRecord and (dceoAppend in DataController.EditOperations) and
    (not ACheckOptions or GridView.OptionsData.Appending);
end;

procedure TcxCustomGridTableController.CancelCheckEditPost;
begin
  FCheckEditNeeded := False;
end;

function TcxCustomGridTableController.CanChangeFocusedRecordIndex(APrevRecordIndex, ANewRecordIndex: Integer): Boolean;
begin
  Result := False;
  if EditingController.IsErrorOnPost then
    Exit;
  if (0 <= ANewRecordIndex) and (ANewRecordIndex < ViewData.RecordCount) and not ViewData.Records[ANewRecordIndex].CanFocus then
    Exit;
  Result := CheckBrowseModeOnRecordChanging(ANewRecordIndex);
end;

function TcxCustomGridTableController.CanCopyToClipboard: Boolean;
begin
  Result := GridView.OptionsBehavior.CopyRecordsToClipboard and not IsEditing;
end;

function TcxCustomGridTableController.CanDataPost: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableController.CanDelete(ACheckOptions: Boolean): Boolean;
begin
  Result := (dceoDelete in DataController.EditOperations) and
    (not ACheckOptions or GridView.OptionsData.Deleting);
end;

function TcxCustomGridTableController.CanEdit: Boolean;
begin
  Result := (FocusedItem <> nil) and GridView.OptionsData.Editing and
    ([dceoEdit, dceoShowEdit] * DataController.EditOperations = [dceoEdit, dceoShowEdit]);
end;

function TcxCustomGridTableController.CanEditFocusedItem: Boolean;
begin
  Result := (FocusedItem <> nil) and FocusedItem.CanEdit;
end;

function TcxCustomGridTableController.CanHScrollBarHide: Boolean;
begin
  Result := not ViewInfo.NavigatorSiteViewInfo.Visible;
end;

function TcxCustomGridTableController.CanInsert(ACheckOptions: Boolean): Boolean;
begin
  Result := (dceoInsert in DataController.EditOperations) and
    (not ACheckOptions or GridView.OptionsData.Inserting);
end;

function TcxCustomGridTableController.CanMakeRecordVisible(ARecord: TcxCustomGridRecord): Boolean;
var
  AFocusedView: TcxCustomGridView;
begin
  Result := (ARecord <> nil) and (GridView.Control <> nil) and
    (ARecord.Index >= FirstScrollRecordIndex) and (ARecord.Index <= LastScrollRecordIndex);
  if Result then
  begin
    AFocusedView := TcxCustomGrid(GridView.Control).FocusedView;
    Result := GridView.Focused or not ((AFocusedView is TcxCustomGridTableView) and
      AFocusedView.HasAsMaster(GridView) and TcxCustomGridTableView(AFocusedView).DontMakeMasterRecordVisible);
  end;
end;

function TcxCustomGridTableController.CanNormalSelectionOnMouse: Boolean;
begin
  Result := not IsMultiSelectPersistent;
end;

function TcxCustomGridTableController.CanUseAutoHeightEditing: Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableController.CheckBrowseModeOnRecordChanging(ANewRecordIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridTableController.CheckEdit;
begin
  CancelCheckEditPost;
  if FAllowCheckEdit and GridView.OptionsBehavior.AlwaysShowEditor then
    if GridView.IsControlLocked then
      PostCheckEdit
    else
      FEditingController.CheckEdit;
end;

procedure TcxCustomGridTableController.CheckCoordinates;
var
  ARecordIndex, ARecordOffset: Integer;
begin
  inherited CheckCoordinates;
  if IsRecordPixelScrolling then
  begin
    ARecordIndex := InternalTopRecordIndex;
    ARecordOffset := PixelScrollRecordOffset;
    CheckTopRecordIndexAndOffset(ARecordIndex, ARecordOffset);
    SetTopRecordIndexWithOffset(ARecordIndex, ARecordOffset);
  end
  else
    TopRecordIndex := InternalTopRecordIndex;
end;

procedure TcxCustomGridTableController.CheckTopRecordIndex(var Value: Integer);
var
  AMaxPixelScrollRecordOffset: Integer;
begin
  AMaxPixelScrollRecordOffset := 0;
  CheckTopRecordIndexAndOffset(Value, AMaxPixelScrollRecordOffset);
end;

procedure TcxCustomGridTableController.CheckTopRecordIndexAndOffset(var AIndex, AOffset: Integer);

  function IsGridModeAndScrollable: Boolean;
  begin
    Result := DataController.IsGridMode and not (dceInsert in DataController.EditState) and
      not IsCheckingCoordinates;
  end;

var
  APrevAllowHideSite: Boolean;
  AMaxValue: Integer;
begin
  if FCheckingCoordinate then Exit;
  FCheckingCoordinate := True;
  APrevAllowHideSite := ViewInfo.AllowHideSite;
  ViewInfo.AllowHideSite := False;
  try
    if AIndex < FirstScrollRecordIndex then
    begin
      if IsGridModeAndScrollable and not DataController.IsBOF then
        DataController.Scroll(AIndex);
      AIndex := FirstScrollRecordIndex;
    end;
    if (not IsRecordPixelScrolling or (ScrollRecordCount = 0)) and (AIndex > LastScrollRecordIndex) then
    begin
      if DataController.IsGridMode and not DataController.IsEOF then
        DataController.Scroll(AIndex - LastScrollRecordIndex);
      AIndex := LastScrollRecordIndex;
    end;

    if (AIndex < FirstScrollRecordIndex) or not DataController.IsGridMode and (AIndex = InternalTopRecordIndex) and
      (ViewInfo.VisibleRecordCount < ScrollRecordCount - AIndex) then
      Exit;

    if IsRecordPixelScrolling then
      CheckPixelScrollTopRecordIndexAndOffsetValues(AIndex, AOffset)
    else
      if AIndex <> FirstScrollRecordIndex then
      begin
        AMaxValue := GetMaxTopRecordIndexValue;
        if AIndex > AMaxValue then
        begin
          if IsGridModeAndScrollable and not DataController.IsEOF then
          begin
            DataController.Scroll(AIndex - AMaxValue);
            AMaxValue := GetMaxTopRecordIndexValue;
          end;
          AIndex := AMaxValue;
        end;
      end;
  finally
    ViewInfo.AllowHideSite := APrevAllowHideSite;
    FCheckingCoordinate := False;
  end;
end;

function TcxCustomGridTableController.DataScrollSize: Integer;
begin
  if not IsPixelBasedScrollDataPos then
    Result := ScrollBarRecordCount
  else
  begin
    if GridView.IsEqualScrollSizeRecords then
      Result := ScrollBarRecordCount * ViewInfo.GetEqualHeightRecordScrollSize
    else
      Result := GetPixelScrollSize;
    Inc(Result, GridView.GetHorizontalScrollBarAreaHeight);
  end;
end;

function TcxCustomGridTableController.DoGetScrollBarPos: Integer;
var
  AScrollRecordDelta: Integer;
begin
  if (ViewInfo.VisibleRecordCount > 0) and (InternalTopRecordIndex >= FirstScrollRecordIndex) then
  begin
    AScrollRecordDelta := InternalTopRecordIndex - FirstScrollRecordIndex;
    if IsPixelBasedScrollDataPos then
      if GridView.IsEqualScrollSizeRecords then
        Result := AScrollRecordDelta * ViewInfo.GetEqualHeightRecordScrollSize - PixelScrollRecordOffset
      else
        Result := ViewData.Records[InternalTopRecordIndex].PixelScrollPosition - PixelScrollRecordOffset
    else
      Result := ScrollBarOffsetBegin + AScrollRecordDelta
  end
  else
    Result := 0
end;

procedure TcxCustomGridTableController.DoMakeRecordVisible(ARecord: TcxCustomGridRecord);

  procedure CalculateMakeVisibleRecordBounds(ARecord: TcxCustomGridRecord);
  var
    ARecordViewInfo: TcxCustomGridRecordViewInfo;
  begin
    ARecordViewInfo := ViewInfo.RecordsViewInfo.GetRealItem(ARecord);
    if (ARecordViewInfo <> nil) and ARecordViewInfo.Calculated then
      MakeVisibleDetailRecordBounds := ARecordViewInfo.Bounds
    else
      MakeVisibleDetailRecordBounds := cxNullRect;
  end;

begin
  if ARecord.Index <> -1 then
  begin
    InternalDoMakeRecordVisible(ARecord);
    if GridView.IsDetail and (GridView.MasterGridRecord <> nil) and
      (IsRecordPixelScrolling or GridView.HasPixelScrollingMaster) then
      CalculateMakeVisibleRecordBounds(ARecord);
    GridView.MakeMasterGridRecordVisible;
  end;
end;

procedure TcxCustomGridTableController.DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);
begin
  Tracking := AScrollCode = scTrack;
  inherited DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxCustomGridTableController.DoSetScrollBarPos(Value: Integer);

  procedure CalculateTopRecordIndexAndOffset(Value: Integer; out ATopRecordIndex, ATopRecordPixelScrollOffset: Integer);
  var
    I, APrevPosition: Integer;
    ARecord: TcxCustomGridRecord;
  begin
    APrevPosition := 0;
    ATopRecordIndex := 0;
    ATopRecordPixelScrollOffset := 0;
    if GridView.IsEqualScrollSizeRecords then
    begin
      ATopRecordIndex := FirstScrollRecordIndex + Value div ViewInfo.GetEqualHeightRecordScrollSize;
      ATopRecordPixelScrollOffset := - Value mod ViewInfo.GetEqualHeightRecordScrollSize;
    end
    else
      for I := LastScrollRecordIndex downto FirstScrollRecordIndex do
      begin
        ARecord := ViewData.Records[I];
        if ARecord.PixelScrollPosition <= Value then
          if ARecord.PixelScrollPosition >= APrevPosition then
          begin
            APrevPosition := ARecord.PixelScrollPosition;
            ATopRecordIndex := I;
            ATopRecordPixelScrollOffset := ARecord.PixelScrollPosition - Value;
          end
          else
            Break;
      end;
  end;

var
  ATopRecordIndex, ATopRecordPixelScrollOffset: Integer;
begin
  ATopRecordIndex := FirstScrollRecordIndex + Value;
  if IsPixelBasedScrollDataPos then
  begin
    CalculateTopRecordIndexAndOffset(Value, ATopRecordIndex, ATopRecordPixelScrollOffset);
    SetTopRecordIndexWithOffset(ATopRecordIndex, ATopRecordPixelScrollOffset);
    Site.Update;
  end
  else
  begin
    if IsRecordPixelScrolling then
    begin
      ATopRecordPixelScrollOffset := 0;
      CheckTopRecordIndexAndOffset(ATopRecordIndex, ATopRecordPixelScrollOffset);
      SetTopRecordIndexWithOffset(ATopRecordIndex, ATopRecordPixelScrollOffset);
    end
    else
      InternalTopRecordIndex := ATopRecordIndex;
  end;
end;

procedure TcxCustomGridTableController.EndGestureScroll;
begin
  inherited EndGestureScroll;
  Tracking := False;
end;

function TcxCustomGridTableController.FindNextCustomItem(AFocusedItemIndex, AItemCount: Integer;
  AGoForward, AGoOnCycle: Boolean; ACanFocus: TcxCustomGridTableCanItemFocus;
  AData: TObject; var AItemIndex: Integer; out ACycleChanged: Boolean): Boolean;
var
  AFromIndex: Integer;

  function GetFromIndex: Integer;
  begin
    if AFocusedItemIndex = -1 then
      if AGoForward then
        Result := 0
      else
        if AGoOnCycle then
          Result := AItemCount - 1
        else
          Result := -1
    else
      if AGoForward then
        Result := AFocusedItemIndex + 1
      else
        Result := AFocusedItemIndex - 1;
  end;

  function CheckIndex(var AIndex: Integer): Boolean;
  begin
    Result := True;
    if AGoForward then
      if AIndex > AItemCount - 1 then
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
          AIndex := AItemCount - 1;
          ACycleChanged := True;
        end
        else
          Result := False;
  end;

  procedure GetNextIndex(var AIndex: Integer);
  begin
    if AGoForward then
      Inc(AIndex)
    else
      Dec(AIndex);
  end;

begin
  Result := False;
  ACycleChanged := False;
  if AItemCount = 0 then Exit;
  AFromIndex := GetFromIndex;
  AItemIndex := AFromIndex;
  repeat
    if not CheckIndex(AItemIndex) then Exit;
    if (AItemIndex = AFocusedItemIndex) and not ACycleChanged then Exit;
    Result := ACanFocus(GridView, AItemIndex, AData);
    if Result or
      ACycleChanged and ((AItemIndex = AFocusedItemIndex) or (AFocusedItemIndex = -1)) then Exit;
    GetNextIndex(AItemIndex);
  until (AItemIndex = AFromIndex) or (AItemIndex = -1) and (AFocusedItemIndex = -1);
end;

procedure TcxCustomGridTableController.FocusedItemChanged(APrevFocusedItem: TcxCustomGridTableItem);
begin
  if GridView.IsLoading or GridView.IsDestroying then Exit;
  IsReadyForImmediateEditing := False;
  CancelIncSearching;
  MakeFocusedItemVisible;
  if FocusedRecord <> nil then
  begin
    FocusedRecord.Invalidate(APrevFocusedItem);
    FocusedRecord.Invalidate(FocusedItem);
  end;
  if (APrevFocusedItem = nil) or (FocusedItem = nil) then
    GridView.RefreshNavigators;
  GridView.FocusedItemChanged(APrevFocusedItem, FFocusedItem);
end;

procedure TcxCustomGridTableController.FocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
begin
  IsReadyForImmediateEditing := False;
  EditingController.FocusedRecordChanged;
  if not DataController.IsSelectionAnchorExist then
    SetSelectionAnchor(AFocusedRecordIndex);
  GridView.FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
    APrevFocusedDataRecordIndex, AFocusedDataRecordIndex, ANewItemRecordFocusingChanged);
end;

procedure TcxCustomGridTableController.GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer);
begin
  if GridView.OptionsBehavior.BestFitMaxRecordCount > 0 then
  begin
    AFirstIndex := Max(FirstScrollRecordIndex, InternalTopRecordIndex);
    ALastIndex := AFirstIndex + GridView.OptionsBehavior.BestFitMaxRecordCount;
    ALastIndex := Min(ALastIndex, LastScrollRecordIndex);
  end
  else
  begin
    AFirstIndex := FirstScrollRecordIndex;
    ALastIndex := LastScrollRecordIndex;
  end;
end;

function TcxCustomGridTableController.GetCancelEditingOnExit: Boolean;
begin
  Result :=
    GridView.OptionsData.CancelOnExit and
    (DataController.EditState * [dceInsert, dceChanging, dceModified] = [dceInsert]);
end;

function TcxCustomGridTableController.GetDlgCode: Integer;
begin
  Result := inherited GetDlgCode;
  if FindPanel.IsFocused then
    Result := Result or DLGC_WANTALLKEYS;
end;

function TcxCustomGridTableController.GetFilterMRUItemsPopupClass: TcxGridFilterMRUItemsPopupClass;
begin
  Result := TcxGridFilterMRUItemsPopup;
end;

function TcxCustomGridTableController.GetFilterPopupClass: TcxGridFilterPopupClass;
begin
  Result := TcxGridFilterPopup;
end;

function TcxCustomGridTableController.GetFindFilterStartPosition(
  ARecordIndex: TdxNativeInt; AItemIndex: Integer; out AHighlightedText: string): Integer;
begin
  Result := DataController.GetFindFilterTextStartPosition(ARecordIndex, AItemIndex, AHighlightedText);
end;

function TcxCustomGridTableController.GetFocusedRecord: TcxCustomGridRecord;
begin
  if (0 <= FocusedRecordIndex) and (FocusedRecordIndex < ViewData.RecordCount) then
    Result := ViewData.Records[FocusedRecordIndex]
  else
    Result := nil;
end;

function TcxCustomGridTableController.GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass;
begin
  Result := TcxCustomGridItemsCustomizationPopup;
end;

function TcxCustomGridTableController.GetMaxTopRecordIndexValue: Integer;
begin
  Result := LastScrollRecordIndex - GetPageVisibleRecordCount(LastScrollRecordIndex, False) + 1;
end;

procedure TcxCustomGridTableController.GetPixelScrollTopRecordIndexAndOffsetByBottomRecord(ABottomRecordIndex: Integer;
  out ATopRecordIndex, ATopRecordPixelScrollOffset: Integer);
var
  AOverPan: Boolean;
begin
  ATopRecordIndex := ABottomRecordIndex;
  ATopRecordPixelScrollOffset := -ViewInfo.RecordsViewInfo.GetRecordScrollSize(ABottomRecordIndex);
  if ABottomRecordIndex = LastScrollRecordIndex then
    Dec(ATopRecordPixelScrollOffset, GridView.GetHorizontalScrollBarAreaHeight);
  ViewInfo.RecordsViewInfo.CalculatePixelScrollInfo(ATopRecordIndex, ATopRecordPixelScrollOffset,
    cxRecordIndexNone, UnassignedPixelScrollOffset, ViewInfo.RecordsViewInfo.GetPixelScrollContentSize, AOverPan);
end;

procedure TcxCustomGridTableController.GetPixelScrollTopRecordIndexAndOffsetMaxValues(
  out ARecordIndex, APixelScrollOffset: Integer);
begin
  GetPixelScrollTopRecordIndexAndOffsetByBottomRecord(LastScrollRecordIndex,
    ARecordIndex, APixelScrollOffset);
end;

function TcxCustomGridTableController.GetPageVisibleRecordCount(AFirstRecordIndex: Integer; ACalculateDown: Boolean = True): Integer;
begin
  Result := GetVisibleRecordCount(AFirstRecordIndex, ACalculateDown);
  if (Result = 0) and (ScrollRecordCount > 0) then
    Result := 1;
end;

function TcxCustomGridTableController.GetPatternObject(AObject: TPersistent): TPersistent;
begin
  if AObject is TcxCustomGridTableItem then
    Result := TcxCustomGridTableView(GridView.PatternGridView).FindItemByID(TcxCustomGridTableItem(AObject).ID)
  else
    Result := inherited GetPatternObject(AObject);
end;

function TcxCustomGridTableController.GetScrollBarOffsetBegin: Integer;
begin
  if DataController.IsGridMode then
    Result := Ord(not DataController.IsBOF)
  else
    Result := 0;
end;

function TcxCustomGridTableController.GetScrollBarOffsetEnd: Integer;
begin
  if DataController.IsGridMode then
    Result := Ord(not DataController.IsEOF)
  else
    Result := 0;
end;

function TcxCustomGridTableController.GetScrollBarPos: Integer;
var
  AIcxGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) then
    Result := AIcxGridDataController.GetScrollBarPos
  else
    Result := -1;
  if Result = -1 then
    Result := DoGetScrollBarPos;
end;

function TcxCustomGridTableController.GetScrollBarRecordCount: Integer;
var
  AIcxGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) then
    Result := AIcxGridDataController.GetScrollBarRecordCount
  else
    Result := -1;
  if Result = -1 then
    Result := ScrollRecordCount + ScrollBarOffsetBegin + ScrollBarOffsetEnd;
end;

function TcxCustomGridTableController.GetScrollDelta: Integer;
begin
  Result := 1;
end;

function TcxCustomGridTableController.GetTopRecordIndex: Integer;
begin
  Result := InternalTopRecordIndex;
end;

function TcxCustomGridTableController.GetTopRecordIndexByBottomRecord(ABottomRecordIndex: Integer): Integer;
begin
  Result := ABottomRecordIndex - GetPageVisibleRecordCount(ABottomRecordIndex, False) + 1;
end;

function TcxCustomGridTableController.GetVisibleRecordCount(AFirstRecordIndex: Integer;
  ACalculateDown: Boolean = True): Integer;
var
  AVisibleEqualHeightRecordCount: Integer;
  AViewInfo: TcxCustomGridTableViewInfo;

  function CanCalculateVisibleEqualHeightRecordCount: Boolean;
  begin
    AVisibleEqualHeightRecordCount := ViewInfo.CalculateVisibleEqualHeightRecordCount;
    Result := AVisibleEqualHeightRecordCount <> -1;
  end;

begin
  if IsRectEmpty(ViewInfo.Bounds) then
    Result := 0
  else
    if GridView.IsEqualHeightRecords and CanCalculateVisibleEqualHeightRecordCount then
    begin  // -2-4 ms
      Result := AVisibleEqualHeightRecordCount;
      if ACalculateDown then
        if AFirstRecordIndex + Result - 1 > LastScrollRecordIndex then
          Result := LastScrollRecordIndex - AFirstRecordIndex + 1
        else
      else
        if AFirstRecordIndex - Result + 1 < FirstScrollRecordIndex then
          Result := AFirstRecordIndex - FirstScrollRecordIndex + 1;
    end
    else
    begin
      ViewInfo.CalculateDown := ACalculateDown;
      AViewInfo := TcxCustomGridTableViewInfo(GridView.CreateViewInfo);
      try
        AViewInfo.CalculateDown := ACalculateDown;
        AViewInfo.FirstRecordIndex := AFirstRecordIndex;
        AViewInfo.PixelScrollRecordOffset := 0;
        AViewInfo.MainCalculate(ViewInfo.Bounds);
        Result := AViewInfo.VisibleRecordCount;
      finally
        AViewInfo.Free;
        ViewInfo.CalculateDown := True;
      end;
    end;
end;

procedure TcxCustomGridTableController.PostCheckEdit;
begin
  if FAllowCheckEdit then FCheckEditNeeded := True;
end;

procedure TcxCustomGridTableController.ProcessCheckEditPost;
begin
  if FCheckEditNeeded then CheckEdit;
end;

procedure TcxCustomGridTableController.ScrollData(ADirection: TcxDirection);
begin
end;

procedure TcxCustomGridTableController.SetClickedCellInfo(
  ACell: TcxGridTableDataCellViewInfo);
begin
  if ACell <> nil then
    with FClickedDataCellInfo do
    begin
      Cell := ACell;
      Item := ACell.Item;
      RecordIndex := ACell.GridRecord.RecordIndex;
    end
  else
    FillChar(FClickedDataCellInfo, SizeOf(FClickedDataCellInfo), 0);
end;

procedure TcxCustomGridTableController.SetFocusedRecord(Value: TcxCustomGridRecord);
begin
  if Value = nil then
    FocusedRecordIndex := -1
  else
    FocusedRecordIndex := Value.Index;
end;

procedure TcxCustomGridTableController.SetScrollBarPos(Value: Integer);
var
  AIcxGridDataController: IcxGridDataController;
begin
  if not Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) or
    not AIcxGridDataController.SetScrollBarPos(Value) then
    if DataController.IsGridMode then
      TopRecordIndex := Value - ScrollBarOffsetBegin
    else
      DoSetScrollBarPos(Value);
end;

procedure TcxCustomGridTableController.SetTopRecordIndexWithOffset(ATopRecordIndex, ATopRecordOffset: Integer);

  function GetAdjustedTopRecordIndex(ATopRecordIndex: Integer): Integer;
  begin
    if ViewData.IsRecordIndexValid(ATopRecordIndex) then
      Result := GetFirstIndexInScrollBand(ATopRecordIndex)
    else
      Result := ATopRecordIndex;
  end;

var
  ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer;
  APrevTopRecordIndex: Integer;
begin
  TcxGridSiteAccess(Site).ShowTouchScrollUI(Site, True);
  ATopRecordIndex := GetAdjustedTopRecordIndex(ATopRecordIndex);
  ARecordCountDelta := ATopRecordIndex - InternalTopRecordIndex;
  APixelScrollRecordOffsetDelta := ATopRecordOffset - FPixelScrollRecordOffset;
  APrevTopRecordIndex := InternalTopRecordIndex;
  FInternalTopRecordIndex := ATopRecordIndex;
  FPixelScrollRecordOffset := ATopRecordOffset;
  if ATopRecordIndex <> -1 then
  begin
    ViewInfo.AllowCheckCoordinates := False;
    try
      if GridView.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta) then
      begin
        if ((ARecordCountDelta <> 0) or (APixelScrollRecordOffsetDelta <> 0)) then
          GridView.Offset(ARecordCountDelta, APixelScrollRecordOffsetDelta, 0, 0)
      end
      else
        if GridView.IsMaster then
          GridView.SizeChanged(True, True)
        else
          GridView.LayoutChanged;
    finally
      ViewInfo.AllowCheckCoordinates := True;
    end;
  end;
  if InternalTopRecordIndex <> APrevTopRecordIndex then
    GridView.DoTopRecordIndexChanged;
end;

procedure TcxCustomGridTableController.ValidateEditVisibility;
begin
  EditingController.ValidateEditVisibility;
end;

function TcxCustomGridTableController.VisibleDataScrollSize: Integer;
begin
  if IsPixelBasedScrollDataPos then
    Result := ViewInfo.RecordsViewInfo.GetPixelScrollContentSize
  else
    Result := ViewInfo.VisibleRecordCount;
end;

function TcxCustomGridTableController.WantSpecialKey(AKey: Word): Boolean;
begin
  Result := inherited WantSpecialKey(AKey) or ((AKey = VK_RETURN) and (FocusedRecord <> nil) and
    (CanEditFocusedItem or GridView.OptionsBehavior.GoToNextCellOnEnter));
end;

procedure TcxCustomGridTableController.CancelGridModeBufferCountUpdate;
begin
  FGridModeBufferCountUpdateNeeded := False;
  FreeAndNil(FGridModeBufferCountUpdateTimer);
end;

procedure TcxCustomGridTableController.CheckGridModeBufferCountUpdatePost;
begin
  if FGridModeBufferCountUpdateNeeded and ViewInfo.Calculated then
  begin
    CancelGridModeBufferCountUpdate;
    (DataController as IcxGridDataController).CheckGridModeBufferCount;
  end;
end;

procedure TcxCustomGridTableController.PostGridModeBufferCountUpdate;
begin
  if DataController.IsGridMode and not FGridModeBufferCountUpdateNeeded then
  begin
    FGridModeBufferCountUpdateNeeded := True;
    FGridModeBufferCountUpdateTimer := TcxTimer.Create(nil);
    with FGridModeBufferCountUpdateTimer do
    begin
      Interval := 1;
      OnTimer := GridModeBufferCountUpdateTimerHandler;
    end;
  end;
end;

function TcxCustomGridTableController.CreateFindPanel: TcxGridFindPanel;
begin
  Result := TcxGridFindPanel.Create(Self);
end;

procedure TcxCustomGridTableController.AfterScrolling;
begin
  if (Site.DragAndDropState <> ddsNone) and (DragAndDropObject <> nil) then
    DragAndDropObject.AfterScrolling;
  inherited AfterScrolling;
  ValidateEditVisibility;
end;

procedure TcxCustomGridTableController.BeforeScrolling;
var
  ADropDownEdit: TcxCustomDropDownEdit;
begin
  inherited BeforeScrolling;
  if IsEditing then
  begin
    EditingController.HideEditOnScrollContent;
    ADropDownEdit := Safe<TcxCustomDropDownEdit>.Cast(EditingController.Edit);
    if (ADropDownEdit <> nil) and ADropDownEdit.DroppedDown then
      ADropDownEdit.DroppedDown := False;
  end;
  if (Site.DragAndDropState <> ddsNone) and (DragAndDropObject <> nil) then
    DragAndDropObject.BeforeScrolling;
end;

procedure TcxCustomGridTableController.BeginGestureScroll(APos: TPoint);
begin
  Tracking := True;
  inherited BeginGestureScroll(APos);
  if ScrollRecordCount > 0 then
    UpdatePixelScrollTopRecordIndexAndOffsetMaxValues;
end;

procedure TcxCustomGridTableController.CheckPixelScrollTopRecordIndexAndOffsetValues(var ATopRecordIndex, APixelScrollRecordOffset: Integer);
begin
  UpdatePixelScrollTopRecordIndexAndOffsetMaxValues;
  if (ATopRecordIndex > FPixelScrollTopRecordIndexMaxValue) or
    (ATopRecordIndex = FPixelScrollTopRecordIndexMaxValue) and (APixelScrollRecordOffset < FPixelScrollTopRecordOffsetMaxValue) then
  begin
    ATopRecordIndex := FPixelScrollTopRecordIndexMaxValue;
    APixelScrollRecordOffset := FPixelScrollTopRecordOffsetMaxValue;
  end;
end;

procedure TcxCustomGridTableController.DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer);
var
  AOverpan: TPoint;
begin
  if AScrollKind = sbHorizontal then
    AOverpan := Point(ADelta, 0)
  else
    AOverpan := Point(0, ADelta);
  TcxGridSiteAccess(Site).CheckOverpan(AScrollKind, 0, 1, -1, AOverpan.X, AOverpan.Y);
end;

function TcxCustomGridTableController.GetFirstScrollRecordIndex: Integer;
begin
  Result := 0;
end;

function TcxCustomGridTableController.GetFirstIndexInScrollBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := ARecordIndexInBand;
end;

function TcxCustomGridTableController.GetLastIndexInScrollBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := GetFirstIndexInScrollBand(ARecordIndexInBand) + GetRecordCountInScrollBand(ARecordIndexInBand) - 1;
end;

function TcxCustomGridTableController.GetLastScrollRecordIndex: Integer;
begin
  Result := ViewData.RecordCount - 1;
end;

function TcxCustomGridTableController.GetPixelScrollSize: Integer;
var
  I, ANextBandRecordIndex: Integer;
begin
  Result := 0;
  if ScrollRecordCount > 0 then
    ANextBandRecordIndex := FirstScrollRecordIndex + Max(1, GetRecordCountInScrollBand(FirstScrollRecordIndex))
  else
    ANextBandRecordIndex := 0;
  for I := FirstScrollRecordIndex to LastScrollRecordIndex do
  begin
    if I >= ANextBandRecordIndex then
    begin
      Inc(ANextBandRecordIndex, GetRecordCountInScrollBand(I));
      Result := Result + ViewInfo.RecordsViewInfo.GetRecordScrollSize(I - 1);
    end;
    ViewData.Records[I].FPixelScrollPosition := Result;
  end;
  if LastScrollRecordIndex > -1 then
    Result := Result + ViewInfo.RecordsViewInfo.GetRecordScrollSize(LastScrollRecordIndex);
end;

function TcxCustomGridTableController.GetRecordCountInScrollBand(ARecordIndexInBand: Integer): Integer;
begin
  Result := ScrollDelta;
end;

function TcxCustomGridTableController.IsRecordFullyVisibleAtBottom(ARecordIndex: Integer): Boolean;
begin
  Result := ARecordIndex < InternalTopRecordIndex + ViewInfo.VisibleRecordCount;
end;

function TcxCustomGridTableController.IsRecordFullyVisibleAtTop(ARecordIndex: Integer): Boolean;
begin
  Result := (ARecordIndex >= InternalTopRecordIndex) and not IsRecordPartVisibleAtTop(ARecordIndex);
end;

function TcxCustomGridTableController.IsRecordPartVisibleAtTop(ARecordIndex: Integer): Boolean;
begin
  Result := (PixelScrollRecordOffset <> 0) and(GetFirstIndexInScrollBand(ARecordIndex) = InternalTopRecordIndex);
end;

procedure TcxCustomGridTableController.UpdatePixelScrollTopRecordIndexAndOffsetMaxValues;
begin
  GetPixelScrollTopRecordIndexAndOffsetMaxValues(FPixelScrollTopRecordIndexMaxValue,
    FPixelScrollTopRecordOffsetMaxValue);
end;

procedure TcxCustomGridTableController.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
var
  ARecordIndex, APixelScrollRecordOffset: Integer;
  AOverPan: Boolean;
begin
  if (ADelta = 0) or ViewData.IsEmpty then Exit;
  ARecordIndex := InternalTopRecordIndex;
  APixelScrollRecordOffset := PixelScrollRecordOffset;
  ViewInfo.RecordsViewInfo.CalculatePixelScrollInfo(ARecordIndex, APixelScrollRecordOffset,
    FPixelScrollTopRecordIndexMaxValue, FPixelScrollTopRecordOffsetMaxValue, ADelta, AOverPan);
  SetTopRecordIndexWithOffset(ARecordIndex, APixelScrollRecordOffset);
  Site.Update;
  if AOverPan then
    DoOverpan(AScrollKind, ADelta);
end;

function TcxCustomGridTableController.CanScrollData(ADirection: TcxDirection): Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableController.GetScrollDataTimeInterval(ADirection: TcxDirection): Integer;
begin
  Result := ScrollTimeInterval;
end;

function TcxCustomGridTableController.CanPostponeRecordSelection(AShift: TShiftState): Boolean;
begin
  Result := not ((ssLeft in AShift) and IsEditing) and (not (ssDouble in AShift) or (ssRight in AShift));
end;

function TcxCustomGridTableController.CanProcessMultiSelect(AIsKeyboard: Boolean): Boolean;
begin
  Result := MultiSelect and (not AIsKeyboard or GridView.Focused);
end;

function TcxCustomGridTableController.CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := //not (ssDouble in AShift) and
    CanProcessMultiSelect(False) and IsClickableRecordHitTest(AHitTest) and
    (TcxGridRecordHitTest(AHitTest).GridRecord <> nil) and
    TcxGridRecordHitTest(AHitTest).GridRecord.Focused;
end;

function TcxCustomGridTableController.CanProcessMultiSelect(AKey: Word;
  AShift: TShiftState; AFocusedRecordChanged: Boolean): Boolean;
begin
  Result := CanProcessMultiSelect(True) and
    IsKeyForMultiSelect(AKey, AShift, AFocusedRecordChanged);
end;

procedure TcxCustomGridTableController.ChangeRecordSelection(ARecord: TcxCustomGridRecord;
  Value: Boolean);
begin
  if MultiSelect then
    DataController.ChangeRowSelection(ARecord.Index, Value);
end;

procedure TcxCustomGridTableController.CheckFocusedRecordSelectionWhenExit(ARecord: TcxCustomGridRecord);
begin
  if GridView.OptionsSelection.UnselectFocusedRecordOnExit and MultiSelect and (ARecord <> nil) and (SelectedRecordCount = 1) and
    ARecord.Selected and not IsMultiSelectPersistent then
    ARecord.Selected := False;
end;

procedure TcxCustomGridTableController.DoMouseNormalSelection(AHitTest: TcxCustomGridHitTest);
begin
  if CanNormalSelectionOnMouse then
    DoNormalSelection;
end;

procedure TcxCustomGridTableController.DoMouseRangeSelection(AClearSelection: Boolean = True;
  AData: TObject = nil);
begin
  DoRangeSelection(AClearSelection);
end;

procedure TcxCustomGridTableController.DoNormalSelection;
begin
  if (SelectedRecordCount = 1) and (SelectedRecords[0] = FocusedRecord) then
    Exit;
  BeginUpdate;
  try
    ClearSelection;
    if FocusedRecord <> nil then
      FocusedRecord.Selected := True;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableController.DoRangeSelection(AClearSelection: Boolean = True);
begin                            //!!!
  if (FocusedRecordIndex <> -1) {and not (dceInsert in DataController.EditState) }then
    DataController.SelectFromAnchor(FocusedRecordIndex, IsMultiSelectPersistent or not AClearSelection);
end;

procedure TcxCustomGridTableController.DoToggleRecordSelection;
begin
  if FocusedRecord <> nil then
    with FocusedRecord do
      Selected := not Selected;
end;

procedure TcxCustomGridTableController.FinishSelection;
begin
  FIsRecordUnselecting := False;
end;

procedure TcxCustomGridTableController.InvalidateFocusedRecord;
begin
  if FocusedRecord <> nil then FocusedRecord.Invalidate;
end;

procedure TcxCustomGridTableController.InvalidateSelection;
var
  I: Integer;
  ARecord: TcxCustomGridRecord;
begin
  for I := 0 to SelectedRecordCount - 1 do
  begin
    ARecord := SelectedRecords[I];
    if ARecord <> nil then ARecord.Invalidate;
  end;
  InvalidateFocusedRecord;
end;

function TcxCustomGridTableController.IsKeyForMultiSelect(AKey: Word;
  AShift: TShiftState; AFocusedRecordChanged: Boolean): Boolean;
begin
  Result := (AKey = VK_SPACE) or (AKey = VK_PRIOR) or (AKey = VK_NEXT) or
    AFocusedRecordChanged and not (((AKey = VK_INSERT) or (AKey = Ord('C'))) and (AShift = [ssCtrl])); // for grid mode
end;

function TcxCustomGridTableController.IsMultiSelectPersistent: Boolean;
begin
  Result := GridView.OptionsSelection.IsMultiSelectPersistent;
end;

function TcxCustomGridTableController.IsRecordSelected(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := DataController.IsRowSelected(ARecord.Index);
end;

procedure TcxCustomGridTableController.MultiSelectKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_SPACE) then
  begin
    DoToggleRecordSelection;
    FEatKeyPress := True;
  end
  else
    if (ssShift in Shift) and (Key <> 0) then
      DoRangeSelection
    else
    begin
      SetSelectionAnchor(FocusedRecordIndex);
      if (Shift <> [ssCtrl]) and not IsMultiSelectPersistent then
        DoNormalSelection;
    end;
end;

procedure TcxCustomGridTableController.MultiSelectMouseDown(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState);
var
  APressedRecord: TcxCustomGridRecord;
  ASelectionShift: TShiftState;

  procedure AssignSelectionAnchor;
  begin
    if (ASelectionShift = []) or (ASelectionShift = [ssCtrl]) then
      SetSelectionAnchor(APressedRecord.Index);
  end;

  function MustPostponeDoing: Boolean;
  begin
    Result := CanPostponeRecordSelection(AShift- [ssPen, ssTouch]) and
      APressedRecord.Selected;
  end;

  procedure PostponeDoing;
  begin
    FIsRecordUnselecting := True;
    FUnselectingRecordIndex := APressedRecord.Index;
  end;

  procedure ProcessRecordSelectionToggling;
  begin
    if MustPostponeDoing then
      PostponeDoing
    else
      DoToggleRecordSelection;
  end;

  procedure ProcessNormalSelection;
  begin
    if MustPostponeDoing then
      PostponeDoing
    else
      DoMouseNormalSelection(AHitTest);
  end;

begin
  APressedRecord := TcxGridRecordHitTest(AHitTest).GridRecord;
  ASelectionShift := AShift - [ssLeft, ssRight, ssMiddle, ssDouble, ssPen, ssTouch];
  AssignSelectionAnchor;
  if (ASelectionShift = [ssCtrl]) and SupportsRecordSelectionToggling then
    ProcessRecordSelectionToggling
  else
    if (ASelectionShift = [ssShift]) or (ASelectionShift = [ssCtrl, ssShift]) then
      DoMouseRangeSelection(not (ssCtrl in AShift) or not SupportsAdditiveSelection, AHitTest)
    else
      ProcessNormalSelection;
end;

procedure TcxCustomGridTableController.MultiSelectMouseUp(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState);
var
  ASelectionShift: TShiftState;
begin
  if FIsRecordUnselecting and (FUnselectingRecordIndex <> -1) and
    (FUnselectingRecordIndex = TcxGridRecordHitTest(AHitTest).GridRecord.Index) then
  begin
    ASelectionShift := AShift - [ssLeft, ssRight, ssMiddle, ssDouble, ssPen, ssTouch];
    if (ASelectionShift = [ssCtrl]) and SupportsRecordSelectionToggling then
      DoToggleRecordSelection
    else
      if CanNormalSelectionOnMouse then
        DoNormalSelection;
  end;
end;

procedure TcxCustomGridTableController.SelectFocusedRecord;
begin
  if CanProcessMultiSelect(True) then
  begin
    SetSelectionAnchor(FocusedRecordIndex);
    DoNormalSelection;
  end;
end;

procedure TcxCustomGridTableController.SetSelectionAnchor(AGridRecordIndex: Integer);
begin
  if AGridRecordIndex <> -1 then
    DataController.SetSelectionAnchor(AGridRecordIndex);
end;

function TcxCustomGridTableController.SupportsAdditiveSelection: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableController.SupportsRecordSelectionToggling: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableController.CanFocusNextItem(AFocusedItemIndex, ANextItemIndex: Integer;
  AGoForward, AGoOnCycle, AGoToNextRecordOnCycle: Boolean): Boolean;
begin
  Result := ANextItemIndex <> -1;
end;

function TcxCustomGridTableController.FocusedRecordHasCells(ACheckCellSelectionAbility: Boolean): Boolean;
begin
  Result := (FocusedRecord <> nil) and FocusedRecord.HasCells and
    (not ACheckCellSelectionAbility or FocusedRecord.CanFocusCells);
end;

procedure TcxCustomGridTableController.FocusNextPage(ASyncSelection: Boolean);
begin
  MakeFocusedRecordVisible;
  if FocusedRecordIndex = LastVisibleRecordIndex then
    ShowNextPage;
  FocusRecord(LastVisibleRecordIndex, ASyncSelection);
  if not MultiSelect then
    Site.Update;
end;

procedure TcxCustomGridTableController.FocusPrevPage(ASyncSelection: Boolean);
begin
  MakeFocusedRecordVisible;
  if IsFirstPageRecordFocused then
    ShowPrevPage;
  FocusRecord(InternalTopRecordIndex, ASyncSelection);
  if not MultiSelect then
    Site.Update;
end;

function TcxCustomGridTableController.GetPageRecordCount: Integer;
begin
  Result := ViewInfo.VisibleRecordCount;
end;

function TcxCustomGridTableController.IsFirstPageRecordFocused: Boolean;
begin
  Result := FocusedRecordIndex = InternalTopRecordIndex;
end;

function TcxCustomGridTableController.IsKeyForController(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result :=
    (AKey = VK_TAB) or (AKey = VK_UP) or (AKey = VK_DOWN) or (AKey = VK_PRIOR) or
    (AKey = VK_NEXT) or (AKey = VK_INSERT) or (AKey = VK_ESCAPE);
  Result := Result or ((AKey = Ord('F')) and (AShift = [ssCtrl]) and FindPanel.CanShow);
  if not Result and GridView.OptionsBehavior.AlwaysShowEditor then
    Result := (AKey = VK_LEFT) or (AKey = VK_RIGHT);
end;

procedure TcxCustomGridTableController.ScrollPage(AForward: Boolean);
var
  AIcxGridDataController: IcxGridDataController;
begin
  if not Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) or
    not AIcxGridDataController.DoScrollPage(AForward) then
    if AForward then
      ShowNextPage
    else
      ShowPrevPage;
end;

procedure TcxCustomGridTableController.ScrollRecords(AForward: Boolean; ACount: Integer);
var
  APrevAllowAppendRecord: Boolean;
  AIcxGridDataController: IcxGridDataController;
  ARecordIndex, ARecordOffset: Integer;
begin
  APrevAllowAppendRecord := AllowAppendRecord;
  AllowAppendRecord := False;
  try
    if not Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) or
      not AIcxGridDataController.DoScroll(AForward) then
    begin
      ARecordIndex := InternalTopRecordIndex;
      if AForward then
        Inc(ARecordIndex, ACount)
      else
        if not IsRecordPixelScrolling or (PixelScrollRecordOffset = 0) then
          Dec(ARecordIndex, ACount);
      if IsRecordPixelScrolling then
      begin
        ARecordOffset := 0;
        CheckTopRecordIndexAndOffset(ARecordIndex, ARecordOffset);
        SetTopRecordIndexWithOffset(ARecordIndex, ARecordOffset);
      end
      else
        TopRecordIndex := ARecordIndex;
    end;
  finally
    AllowAppendRecord := APrevAllowAppendRecord;
  end;
end;

procedure TcxCustomGridTableController.ShowNextPage;
var
  ATopRecordIndex, APixelScrollRecordOffset: Integer;
begin
  ATopRecordIndex := InternalTopRecordIndex + GetPageRecordCount;
  if IsRecordPixelScrolling then
  begin
    APixelScrollRecordOffset := 0;
    CheckTopRecordIndexAndOffset(ATopRecordIndex, APixelScrollRecordOffset);
    SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
  end
  else
    TopRecordIndex := ATopRecordIndex;
end;

procedure TcxCustomGridTableController.ShowPrevPage;
begin
  TopRecordIndex := InternalTopRecordIndex - GetPageRecordCount;
end;

procedure TcxCustomGridTableController.DoPullFocusing(AHitTest: TcxGridRecordHitTest);
begin
  AHitTest.GridRecord.Focused := True;
  if AHitTest is TcxGridRecordCellHitTest then
    TcxGridRecordCellHitTest(AHitTest).Item.Focused := True;
  if MultiSelect and IsPullFocusingPosChanged then
    DoMouseRangeSelection(True, FPullFocusingOriginHitTest);
end;

procedure TcxCustomGridTableController.DoPullFocusingScrolling(ADirection: TcxDirection);
var
  APrevAllowAppendRecord: Boolean;
begin
  if ADirection in [dirUp, dirDown] then
  begin
    case ADirection of
      dirUp:
        FocusedRecordIndex := InternalTopRecordIndex;
      dirDown:
        FocusedRecordIndex := InternalTopRecordIndex + ViewInfo.VisibleRecordCount - 1;
    end;
    APrevAllowAppendRecord := AllowAppendRecord;
    AllowAppendRecord := False;
    try
      FocusNextRecord(FocusedRecordIndex, ADirection = dirDown, False, False, False);
    finally
      AllowAppendRecord := APrevAllowAppendRecord;
    end;
  end;
  if MultiSelect and IsPullFocusingPosChanged then
    DoMouseRangeSelection(True, FPullFocusingOriginHitTest);
end;

function TcxCustomGridTableController.GetPullFocusingScrollingDirection(X, Y: Integer;
  out ADirection: TcxDirection): Boolean;
var
  R: TRect;
begin
  Result := False;
  R := ViewInfo.RecordsViewInfo.Bounds;
  if IsRecordsScrollHorizontal and (X < R.Left) or
    not IsRecordsScrollHorizontal and (Y < R.Top) then
  begin
    ADirection := dirUp;
    Result := True;
  end;
  if IsRecordsScrollHorizontal and (X >= R.Right) or
    not IsRecordsScrollHorizontal and (Y >= R.Bottom) then
  begin
    ADirection := dirDown;
    Result := True;
  end;
end;

function TcxCustomGridTableController.IsPullFocusingPosChanged: Boolean;
begin
  Result := (FPullFocusingItem <> FocusedItem) or
    not VarEqualsExact(FPullFocusingRecordId, DataController.GetRowId(FocusedRecordIndex));
end;

procedure TcxCustomGridTableController.SavePullFocusingPos;
begin
  FPullFocusingRecordId := DataController.GetRowId(FocusedRecordIndex);
  FPullFocusingItem := FocusedItem;
end;

procedure TcxCustomGridTableController.StartPullFocusing(AHitTest: TcxCustomGridHitTest);
begin
  FIsPullFocusing := True;
  FPullFocusingMousePos := ViewInfo.MousePos;// Point(-1, -1);
  FPullFocusingOriginHitTest := AHitTest;
  FPullFocusingRecordId := Null;
end;

procedure TcxCustomGridTableController.StopPullFocusing;
begin
  StopPullFocusingScrolling;
  FPullFocusingRecordId := Null;
  FPullFocusingItem := nil;
  FIsPullFocusing := False;
end;

procedure TcxCustomGridTableController.StartPullFocusingScrolling(ADirection: TcxDirection);
begin
  FPullFocusingScrollingDirection := ADirection;
  if FPullFocusingScrollingTimer <> nil then Exit;
  FPullFocusingScrollingTimer := TcxTimer.Create(nil);
  with FPullFocusingScrollingTimer do
  begin
    Interval := PullFocusingScrollingTimeInterval;
    OnTimer := PullFocusingScrollingTimerHandler;
  end;
end;

procedure TcxCustomGridTableController.StopPullFocusingScrolling;
begin
  FreeAndNil(FPullFocusingScrollingTimer);
end;

function TcxCustomGridTableController.SupportsPullFocusing: Boolean;
begin
  Result := GridView.OptionsBehavior.PullFocusing;
end;

function TcxCustomGridTableController.GetDragScrollDirection(X, Y: Integer): TcxDirection;
const
  ADirections: array[Boolean, Boolean] of TcxDirection = ((dirUp, dirDown), (dirLeft, dirRight));
var
  AHitTest: TcxCustomGridHitTest;
  ARecord: TcxCustomGridRecord;
begin
  Result := dirNone;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest is TcxGridRecordHitTest then
  begin
    ARecord := TcxGridRecordHitTest(AHitTest).GridRecord;
    if IsFirstRecordForDragScroll(ARecord) then
      Result := ADirections[IsRecordsScrollHorizontal, False]
    else
      if IsLastRecordForDragScroll(ARecord) then
        Result := ADirections[IsRecordsScrollHorizontal, True];
  end;
end;

function TcxCustomGridTableController.GetDragScrollInterval: Integer;
begin
  Result := 20;
end;

function TcxCustomGridTableController.IsFirstRecordForDragScroll(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := ARecord.Index = InternalTopRecordIndex;
end;

function TcxCustomGridTableController.IsLastRecordForDragScroll(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := ARecord.ViewInfo.Index >= ViewInfo.RecordsViewInfo.VisibleCount - 1;
end;

procedure TcxCustomGridTableController.ProcessDragFocusing(X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest is TcxGridRecordHitTest then
    TcxGridRecordHitTest(AHitTest).GridRecord.Focused := True
  else
    if AHitTest is TcxCustomGridViewHitTest then
      TcxCustomGridViewHitTest(AHitTest).GridView.Focused := True;
end;

procedure TcxCustomGridTableController.StartDragScroll(ADirection: TcxDirection);
begin
  FDragScrollDirection := ADirection;
  if FDragScrollTimer <> nil then Exit;
  FDragScrollTimer := TcxTimer.Create(nil);
  with FDragScrollTimer do
  begin
    Interval := GetDragScrollInterval;
    OnTimer := DragScrollTimerHandler;
  end;
end;

procedure TcxCustomGridTableController.StopDragScroll;
begin
  FreeAndNil(FDragScrollTimer);
end;

function TcxCustomGridTableController.IsDragScroll: Boolean;
begin
  Result := FDragScrollTimer <> nil;
end;

function TcxCustomGridTableController.GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo;
begin
  if AHitTest.HitTestCode = htExpandButton then
    Result := TcxGridDragOpenInfoExpand.Create(TcxGridExpandButtonHitTest(AHitTest).GridRecord)
  else
    Result := nil;
end;

function TcxCustomGridTableController.IsDragOpenHitTest(AHitTest: TcxCustomGridHitTest;
  out ADragOpenInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  ADragOpenInfo := GetDragOpenInfo(AHitTest);
  Result := ADragOpenInfo <> nil;
end;

function TcxCustomGridTableController.GetItemForIncSearching: TcxCustomGridTableItem;
begin
  if GridView.OptionsSelection.CellSelect then
    Result := FocusedItem
  else
  begin
    Result := GridView.OptionsBehavior.IncSearchItem;
    if (Result = nil) and (GridView.VisibleItemCount <> 0) then
      Result := GridView.VisibleItems[0];
  end;
end;

procedure TcxCustomGridTableController.IncSearchKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        CancelIncSearching;
        Key := 0;
      end;
    VK_BACK:
      begin
        IncSearchingText := Copy(IncSearchingText, 1, Length(IncSearchingText) - 1);
        Key := 0;
      end;
    VK_UP, VK_DOWN:
      if Shift = [ssCtrl] then
      begin
        DataController.Search.LocateNext(Key = VK_DOWN, False, not IsMultiSelectPersistent);
        Key := 0;
      end;
  end;
end;

function TcxCustomGridTableController.GetEditingControllerClass: TcxGridEditingControllerClass;
begin
  Result := TcxGridEditingController;
end;

procedure TcxCustomGridTableController.BeginDragAndDrop;
begin
  FEditingController.HideEdit(True);
  inherited;
end;

procedure TcxCustomGridTableController.ControlFocusChanged;
begin
  inherited;
  FindPanel.ControlFocusChanged(GridView.IsControlFocused);
  InvalidateSelection;
end;

procedure TcxCustomGridTableController.DoCancelMode;
begin
  inherited;
  ClickedCellViewInfo := nil;
  StopPullFocusing;
  FinishSelection;
end;

procedure TcxCustomGridTableController.DoKeyDown(var Key: Word; Shift: TShiftState);
var
  APrevTopRecordIndex, APrevFocusedRecordIndex: Integer;
  AGridViewLink: TcxObjectLink;
var
  ASaveKey: Word;
begin
  if FindPanel.IsFocused then
  begin
    FindPanel.KeyDown(Key, Shift);
    Exit;
  end;
  ASaveKey := Key;
  APrevTopRecordIndex := InternalTopRecordIndex;
  AGridViewLink := cxAddObjectLink(GridView);
  try
    try
      if IsIncSearching then
      begin
        IncSearchKeyDown(Key, Shift);
        if AGridViewLink.Ref = nil then
          Exit;
      end;
      APrevFocusedRecordIndex := FocusedRecordIndex;
      inherited;
      if AGridViewLink.Ref <> nil then
        if not (ASaveKey in [VK_TAB, VK_RETURN]) and CanProcessMultiSelect(Key, Shift, FocusedRecordIndex <> APrevFocusedRecordIndex) then
          MultiSelectKeyDown(Key, Shift);
    finally
      if AGridViewLink.Ref <> nil then
      begin
        if InternalTopRecordIndex <> APrevTopRecordIndex then
          Site.Update;
      end
      else
        Key := 0;
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxCustomGridTableController.EndDragAndDrop(Accepted: Boolean);
begin
  ScrollDirection := dirNone;
  inherited;
  CheckEdit;
end;

function TcxCustomGridTableController.FindNextItem(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean; out ACycleChanged: Boolean;
  ARecord: TcxCustomGridRecord): Integer;
begin
  if not FindNextCustomItem(AFocusedItemIndex, GridView.VisibleItemCount,
    AGoForward, AGoOnCycle, @cxCustomGridTableControllerCanFocusItem, ARecord, Result, ACycleChanged) then
    Result := -1;
end;

function TcxCustomGridTableController.FindNextRecord(AFocusedRecordIndex: Integer;
  AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): Integer;
begin
  if DataController.IsGridMode then
    if AGoForward then
      if not DataController.IsEOF and (AFocusedRecordIndex = ViewData.RecordCount - 1) then
      begin
        DataController.Scroll(1);
        if not DataController.IsEOF then
          Dec(AFocusedRecordIndex);
      end
      else
    else
      if (AFocusedRecordIndex = 0) and not DataController.IsBOF then
      begin
        DataController.Scroll(-1);
        if not DataController.IsBOF then
          Inc(AFocusedRecordIndex);
      end;
  if not FindNextCustomItem(AFocusedRecordIndex, ViewData.RecordCount, AGoForward,
    AGoOnCycle, @cxCustomGridTableControllerCanFocusRecord, nil, Result, ACycleChanged) then
    Result := -1;
end;

function TcxCustomGridTableController.HasFilterMRUItemsPopup: Boolean;
begin
  Result := FFilterMRUItemsPopup <> nil;
end;

function TcxCustomGridTableController.HasFilterPopup: Boolean;
begin
  Result := FFilterPopup <> nil;
end;

function TcxCustomGridTableController.HasFocusedControls: Boolean;
begin
  Result := inherited HasFocusedControls or
    {IsEditing}(FEditingController.Edit <> nil) and FEditingController.Edit.IsFocused or
    FindPanel.IsEditFocused;
end;

function TcxCustomGridTableController.HasItemsCustomizationPopup: Boolean;
begin
  Result := FItemsCustomizationPopup <> nil;
end;

function TcxCustomGridTableController.IMEComposition(var AMessage: TMessage): Boolean;
begin
  Result := EditingController.IMEComposition(AMessage);
end;

function TcxCustomGridTableController.IMEStartComposition: Boolean;
begin
  Result := EditingController.IMEStartComposition;
end;

function TcxCustomGridTableController.IsClickableRecordHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := (AHitTest is TcxGridRecordHitTest) and TcxGridRecordHitTest(AHitTest).CanClick;
end;

function TcxCustomGridTableController.IsClickedCell(
  ACell: TcxGridTableDataCellViewInfo): Boolean;
begin
  Result := (ACell <> nil) and (FClickedDataCellInfo.Cell <> nil) and
    ((ACell = FClickedDataCellInfo.Cell) or
     (ACell.Item = FClickedDataCellInfo.Item) and (ACell.GridRecord.RecordIndex = FClickedDataCellInfo.RecordIndex)
    );
end;

function TcxCustomGridTableController.IsPixelBasedScrollDataPos: Boolean;
begin
  Result := IsRecordPixelScrolling and (ScrollRecordCount < ViewInfo.RecordsViewInfo.GetPixelScrollContentSize);
end;

function TcxCustomGridTableController.IsRecordPixelScrolling: Boolean;
begin
  Result := GridView.IsRecordPixelScrolling;
end;

function TcxCustomGridTableController.NeedCheckInternalTopRecordIndex: Boolean;
begin
  Result := not ViewData.IsRecordIndexValid(FInternalTopRecordIndex) and
    not (DataController.IsGridMode and (FInternalTopRecordIndex = cxRecordIndexNone));
end;

function TcxCustomGridTableController.IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean;
begin
  Result := ViewInfo.VisibleRecordCount = ScrollRecordCount;
end;

procedure TcxCustomGridTableController.KeyDown(var Key: Word; Shift: TShiftState);
var
  ARemoveFocus: Boolean;
begin
  inherited;
  if not FBlockRecordKeyboardHandling and (FocusedRecord <> nil) then
    FocusedRecord.KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if (Shift = []) and CanInsert(True) then
        CreateNewRecord(False)
      else
        if (Shift = [ssCtrl]) and CanCopyToClipboard then
          GridView.CopyToClipboard(False);
    VK_DELETE:
      if ((Shift = []) or (Shift = [ssCtrl])) and CanDelete(True) then
      begin
        DeleteSelection;
        Key := 0;
      end;
    VK_ESCAPE:
      if DataController.IsEditing then
      begin
        DataController.Cancel;
        if not DataController.IsEditing and MultiSelect and (FocusedRecord <> nil) then
          FocusedRecord.Selected := True;
        Key := 0;
      end;
    VK_RETURN, VK_TAB:
      if (Key = VK_RETURN) and GridView.OptionsBehavior.GoToNextCellOnEnter or
        (Key = VK_TAB) and (GridView.OptionsBehavior.FocusCellOnTab or IsFindPanelVisible) then
      begin
        ARemoveFocus := False;
        if (Key = VK_TAB) and IsFindPanelVisible and not GridView.OptionsBehavior.FocusCellOnTab then
          ARemoveFocus := not EditingController.IsEditing
        else
          if Shift + [ssShift] = [ssShift] then
            if FocusNextCell(Shift = [], False, True, False, MultiSelect and not IsMultiSelectPersistent) or
              (Key = VK_RETURN) then
              Key := 0
            else
              ARemoveFocus := not EditingController.IsEditing
          else
            ARemoveFocus := (Key = VK_TAB) and (Shift + [ssShift, ssCtrl] = [ssShift, ssCtrl]);
        if ARemoveFocus then
          TcxCustomGrid(GridView.Control).RemoveFocus(not (ssShift in Shift));
      end;
    VK_PRIOR:
      FocusPrevPage(False);
    VK_NEXT:
      FocusNextPage(False);
    Ord('A'):
      if Shift = [ssCtrl] then SelectAll;
    Ord('C'):
      if (Shift = [ssCtrl]) and CanCopyToClipboard then
        GridView.CopyToClipboard(False);
    Ord('F'):
      if Shift = [ssCtrl] then
      begin
        ShowFindPanel;
        Key := 0;
      end;
  end;
end;

procedure TcxCustomGridTableController.KeyPress(var Key: Char);
begin
  if FindPanel.IsFocused then
  begin
    FindPanel.KeyPress(Key);
    Exit;
  end;
  inherited;
  if FEatKeyPress then
  begin
    FEatKeyPress := False;
    Exit;
  end;
  // inc search
  if IsIncSearchStartChar(Key) and
    (ItemForIncSearching <> nil) and ItemForIncSearching.CanIncSearch and
    not DataController.IsEditing then
  begin
    if Key <> #8 then
      IncSearchingText := IncSearchingText + Key;
    Key := #0;
  end;
  // editing
  if IsEditStartChar(Key) and
    (FocusedRecord <> nil) and FocusedRecord.HasCells and (FocusedItem <> nil) then
  begin
    EditingController.ShowEdit(FocusedItem, Key);
    Key := #0;
  end;
end;

procedure TcxCustomGridTableController.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FindPanel.IsFocused then
  begin
    FindPanel.KeyUp(Key, Shift);
    Exit;
  end;
  inherited KeyUp(Key, Shift);
end;

procedure TcxCustomGridTableController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  AGridViewLink: TcxObjectLink;
begin
  FindPanel.FocusedItem := fpfiNone;
  FEditingController.StopEditShowingTimer;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  AGridViewLink := cxAddObjectLink(GridView);
  try
    try
      EditingController.ClearErrorState;
      inherited;
    finally
      if AGridViewLink.Ref <> nil then
      begin
        if not (AHitTest is TcxCustomGridViewHitTest) or
          (TcxCustomGridViewHitTest(AHitTest).GridView = AGridViewLink.Ref) then
        begin
          if CanProcessMultiSelect(AHitTest, Shift) then
            MultiSelectMouseDown(AHitTest, Shift);
          if cxShiftStateLeftOnly(Shift) and IsClickableRecordHitTest(AHitTest) and
            SupportsPullFocusing and Site.MouseCapture and (GridView.DragMode = dmManual) then
            StartPullFocusing(AHitTest);
        end;
      end;
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxCustomGridTableController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  ADirection: TcxDirection;
begin
  inherited;
  if FIsPullFocusing then
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    if (AHitTest is TcxGridRecordHitTest) and CanHandleHitTest(AHitTest) then
      if (FPullFocusingMousePos.X <> X) or (FPullFocusingMousePos.Y <> Y) then
      begin
        StopPullFocusingScrolling;
        DoPullFocusing(TcxGridRecordHitTest(AHitTest));
        SavePullFocusingPos;
        Site.Update;
      end
      else
    else
      if GetPullFocusingScrollingDirection(X, Y, ADirection) then
        StartPullFocusingScrolling(ADirection);
    FPullFocusingMousePos := Point(X, Y);
  end;
end;

procedure TcxCustomGridTableController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  inherited;
  ClickedCellViewInfo := nil;
  StopPullFocusing;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if (Button = mbLeft) and CanProcessMultiSelect(AHitTest, Shift) then
    MultiSelectMouseUp(AHitTest, Shift);
  FinishSelection;
end;

function TcxCustomGridTableController.ProcessDetailDialogChar(ADetail: TcxCustomGridView;
  ACharCode: Word): Boolean;
var
  ARecord: TcxCustomGridRecord;
begin
  Result := inherited ProcessDetailDialogChar(ADetail, ACharCode);
  if not Result then
  begin
    ARecord := ViewData.GetRecordByIndex(ADetail.MasterGridRecordIndex);
    Result := (ARecord <> nil) and (ARecord.ViewInfo <> nil) and
      ARecord.ViewInfo.ProcessDialogChar(ACharCode);
  end;
end;

function TcxCustomGridTableController.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result :=
    (FocusedRecord <> nil) and (FocusedRecord.ViewInfo <> nil) and
    FocusedRecord.ViewInfo.ProcessDialogChar(ACharCode) or
    inherited ProcessDialogChar(ACharCode);
end;

procedure TcxCustomGridTableController.SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer);
begin
  inherited SetFocusOnMouseClick(AButton, X, Y);
  if EditingController.IsErrorOnPost then
  begin
    EditingController.ClearErrorState;
    Abort;
  end;
end;

function TcxCustomGridTableController.SupportsTabAccelerators(AGridRecord: TcxCustomGridRecord): Boolean;
begin
  Result := AGridRecord.Focused;
end;

procedure TcxCustomGridTableController.BeforeStartDrag;
begin
  inherited;
  if DataController.IsEditing then
    if (dceModified in DataController.EditState) and CanDataPost then
      DataController.Post
    else
      DataController.Cancel;
end;

function TcxCustomGridTableController.CanDrag(X, Y: Integer): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited CanDrag(X, Y);
  if Result then
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    Result := IsClickableRecordHitTest(AHitTest);
  end;
end;

procedure TcxCustomGridTableController.DragDrop(Source: TObject; X, Y: Integer);
begin
  if GridView.OptionsBehavior.DragFocusing = dfDragDrop then
    ProcessDragFocusing(X, Y);
  inherited;
end;

procedure TcxCustomGridTableController.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

  procedure ProcessScrolling;
  var
    ADirection: TcxDirection;
  begin
    ADirection := GetDragScrollDirection(X, Y);
    if (ADirection = dirNone) or (State = dsDragLeave) then
      StopDragScroll
    else
      StartDragScroll(ADirection);
  end;

  procedure ProcessOpening;
  var
    AHitTest: TcxCustomGridHitTest;
    ADragOpenInfo: TcxCustomGridDragOpenInfo;
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    if (State <> dsDragLeave) and IsDragOpenHitTest(AHitTest, ADragOpenInfo) then
      TcxCustomGrid(Control).Controller.StartDragOpen(ADragOpenInfo)
    else
      TcxCustomGrid(Control).Controller.StopDragOpen;
  end;

  procedure ProcessDragHighlighting;
  var
    AHitTest: TcxCustomGridHitTest;
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    if (State <> dsDragLeave) and (AHitTest is TcxGridRecordHitTest) then
      DragHighlightedRecord := TcxGridRecordHitTest(AHitTest).GridRecord
    else
      DragHighlightedRecord := nil;
  end;

begin
  inherited;
  if GridView.OptionsBehavior.DragScrolling then
    ProcessScrolling;
  if GridView.OptionsBehavior.DragOpening then
    ProcessOpening;
  if GridView.OptionsBehavior.DragHighlighting then
    ProcessDragHighlighting;
  if (State <> dsDragLeave) and (GridView.OptionsBehavior.DragFocusing = dfDragOver) then
    ProcessDragFocusing(X, Y);
end;

procedure TcxCustomGridTableController.DrawDragImage(ACanvas: TcxCanvas; R: TRect);
var
  AParams: TcxViewParams;

  procedure DrawBorder(R: TRect);
  var
    I: Integer;
  begin
    for I := 1 to DragDropTextBorderSize do
    begin
      ACanvas.DrawFocusRect(R);
      InflateRect(R, -1, -1);
    end;
  end;

  procedure DrawText(const R: TRect);
  begin
    with ACanvas do
    begin
      Font := AParams.Font;
      Font.Color := AParams.TextColor;
      Brush.Style := bsClear;
      DrawText(FDragDropText, R, 0);
      Brush.Style := bsSolid;
    end;
  end;

begin
  if FDragDropText = '' then Exit;
  GetDragDropTextViewParams(AParams);
  with ACanvas do
  begin
    Brush.Color := AParams.Color;
    FillRect(R);
  end;
  Inc(R.Left, DragDropTextAreaOffset);
  DrawBorder(R);
  InflateRect(R, -DragDropTextIndent, -DragDropTextIndent);
  DrawText(R);
end;

procedure TcxCustomGridTableController.EndDrag(Target: TObject; X, Y: Integer);
begin
  DragHighlightedRecord := nil;
  TcxCustomGrid(Control).Controller.StopDragOpen;
  StopDragScroll;
  inherited;
end;

function TcxCustomGridTableController.GetDragDropText(ADragObject: TDragObject): string;
begin
  if GridView.OptionsBehavior.DragDropText then
    Result := GridView.DoGetDragDropText(FocusedRecord, FocusedItem)
  else
    Result := '';
end;

procedure TcxCustomGridTableController.GetDragDropTextViewParams(out AParams: TcxViewParams);
begin
  GridView.Styles.GetContentParams(nil, nil, AParams);
end;

function TcxCustomGridTableController.GetDragImagesSize: TPoint;
var
  AParams: TcxViewParams;
  ACanvas: TcxCanvas;
begin
  if FDragDropText = '' then
    Result := Point(0, 0)
  else
  begin
    GetDragDropTextViewParams(AParams);
    ACanvas := ViewInfo.Canvas;
    ACanvas.Font := AParams.Font;
    Result := Point(
      DragDropTextAreaOffset + DragDropTextIndent + ACanvas.TextWidth(FDragDropText) + DragDropTextIndent,
      DragDropTextIndent + ACanvas.TextHeight(FDragDropText) + DragDropTextIndent);
  end;
end;

function TcxCustomGridTableController.HasDragImages: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridTableController.StartDrag(var DragObject: TDragObject);

  function GetCursor: TCursor;
  begin
    if SelectedRecordCount > 1 then
      Result := crcxGridMultiDrag
    else
      Result := crcxGridDrag;
  end;

begin
  inherited;
  TControlAccess(Site).DragCursor := GetCursor;
  FDragDropText := GetDragDropText(DragObject);
end;

procedure TcxCustomGridTableController.ApplyFindFilterText(const AText: string);
begin
  DataController.FindFilterText := AText;
end;

procedure TcxCustomGridTableController.ClearFindFilterText;
begin
  DataController.ClearFindFilterText;
end;

function TcxCustomGridTableController.GetFindFilterText: string;
begin
  Result := DataController.FindFilterText;
end;

procedure TcxCustomGridTableController.HideFindPanel;
begin
  FindPanel.Hide;
end;

function TcxCustomGridTableController.IsFindPanelVisible: Boolean;
begin
  Result := FindPanel.Visible;
end;

procedure TcxCustomGridTableController.ShowFindPanel;
begin
  FindPanel.Show;
end;

procedure TcxCustomGridTableController.CancelIncSearching;
begin
  DataController.Search.Cancel;
end;

function TcxCustomGridTableController.CheckEditing(var AFocusedRecordIndex: Integer;
  AGoForward: Boolean): Boolean;

  function CheckFocusedRecordIndex(AImmediatePost: Boolean): Boolean;
  begin
    Result := (AFocusedRecordIndex <> -1) and
      (not AImmediatePost or (dceChanging in DataController.EditState));
  end;

var
  AGridViewLink: TcxObjectLink;
  ACheckFocusedRecordIndex: Boolean;
begin
  Result := False;
  //AGridView.BeginUpdate;  - commented because of dialog calling in OnBeforePost
  AGridViewLink := cxAddObjectLink(GridView);
  try
    try
      if DataController.IsEditing then
      begin
        ACheckFocusedRecordIndex := CheckFocusedRecordIndex(DataController.IsImmediatePost);
        EditingController.UpdateValue;
        if not (dceModified in DataController.EditState) then
        begin
          if DataController.EditState = [dceInsert] then
          begin
            Result := AGoForward xor DataController.IsEOF;
            if Result then DataController.Cancel;
          end;
          if ACheckFocusedRecordIndex then
            AFocusedRecordIndex := FocusedRecordIndex;
          Exit;
        end;
        if CanDataPost then
          DataController.Post
        else
          DataController.Cancel;
        if AGridViewLink.Ref = nil then
	  Exit;
        if ACheckFocusedRecordIndex then
          AFocusedRecordIndex := FocusedRecordIndex;
      end;
    finally
      if AGridViewLink.Ref = nil then
        Result := True
      else
      begin
        //AGridView.EndUpdate;
      end;
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxCustomGridTableController.CheckScrolling(const P: TPoint);
begin
end;

procedure TcxCustomGridTableController.ClearSelection;
begin
  DataController.ClearSelection;
end;

procedure TcxCustomGridTableController.CreateNewRecord(AtEnd: Boolean);
var
  AIsEditing: Boolean;
begin
  GridView.Focused := True;
  AIsEditing := IsEditing;
  if AtEnd then
    if CanAppend(False) then
      DataController.Append
    else
  else
    if CanInsert(False) then
    begin
      DataController.Insert;
      if MultiSelect and (ViewData.EditingRecord <> nil) then
        ViewData.EditingRecord.Selected := True;
    end;
  if (dceInsert in DataController.EditState) and
    GridView.OptionsBehavior.FocusFirstCellOnNewRecord then
  begin
    FocusFirstAvailableItem;
    if AIsEditing then
      FEditingController.ShowEdit;
  end;
end;

procedure TcxCustomGridTableController.DeleteSelection;
var
  AMultiSelect: Boolean;

  function GetConfirmationText: string;
  begin
    if AMultiSelect then
      Result := cxGetResourceString(@scxGridDeletingSelectedConfirmationText)
    else
      Result := cxGetResourceString(@scxGridDeletingFocusedConfirmationText);
  end;

begin
  if not CanDelete(False) then Exit;
  AMultiSelect := MultiSelect and (SelectedRecordCount <> 0) and
    ((SelectedRecordCount > 1) or (SelectedRecords[0] = nil) or
     (FocusedRecordIndex <> SelectedRecords[0].Index));

  if not GridView.OptionsData.DeletingConfirmation or
    (MessageDlg(GetConfirmationText, mtConfirmation, [mbOK, mbCancel], 0) = mrOk) then
  begin
    if IsEditing then
      EditingController.HideEdit(False);
    if AMultiSelect then
      DataController.DeleteSelection
    else
      DataController.DeleteFocused;
  end;
end;

function TcxCustomGridTableController.FocusFirstAvailableItem: Boolean;
begin
  Result := FocusNextItem(-1, True, False, False, not IsMultiSelectPersistent);
end;

function TcxCustomGridTableController.FocusNextCell(AGoForward: Boolean;
  AProcessCellsOnly: Boolean = True; AAllowCellsCycle: Boolean = True;
  AFollowVisualOrder: Boolean = True; ANeedNormalizeSelection: Boolean = False): Boolean;
begin
  if FocusedRecordHasCells(True) then
    Result := FocusNextItem(FocusedItemIndex, AGoForward, False,
      AAllowCellsCycle and GridView.OptionsBehavior.FocusCellOnCycle, AFollowVisualOrder, ANeedNormalizeSelection)
  else
    if AProcessCellsOnly or not GridView.OptionsBehavior.FocusCellOnCycle then
      Result := False
    else
    begin
      Result := FocusNextRecord(FocusedRecordIndex, AGoForward, False, False, False);
      if FocusedRecordHasCells(True) then
        FocusNextItem(-1, AGoForward, True, False, AFollowVisualOrder, ANeedNormalizeSelection);
    end;
end;

function TcxCustomGridTableController.FocusNextItem(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle, AGoToNextRecordOnCycle, AFollowVisualOrder: Boolean; ANeedNormalizeSelection: Boolean = False): Boolean;
var
  ANextItemIndex: Integer;
  ACycleChanged: Boolean;
begin
  ANextItemIndex := FindNextItem(AFocusedItemIndex, AGoForward,
    AGoOnCycle or AGoToNextRecordOnCycle, AFollowVisualOrder, ACycleChanged, FocusedRecord);
  Result := CanFocusNextItem(AFocusedItemIndex, ANextItemIndex, AGoForward,
    AGoOnCycle, AGoToNextRecordOnCycle);
  if Result then
  begin
    if ACycleChanged and AGoToNextRecordOnCycle then
      if FocusedRecord.IsNewItemRecord then
      begin
        if CanDataPost then
          DataController.Post
        else
          DataController.Cancel;
        if ViewData.NewItemRecord <> nil then
          ViewData.NewItemRecord.Focused := True;
        Result := True;
      end
      else
        Result := FocusNextRecord(FocusedRecordIndex, AGoForward, False, False, False){ and
          FocusedRecordHasCells(True)}
    else
      Result := True;
    if Result then
    begin
      GridView.VisibleItems[ANextItemIndex].Focused := True;
      if ANeedNormalizeSelection then
        DoNormalSelection;
    end;
  end;
end;

function TcxCustomGridTableController.FocusNextRecord(AFocusedRecordIndex: Integer;
  AGoForward, AGoOnCycle, AGoIntoDetail, AGoOutOfDetail: Boolean): Boolean;
var
  AGridViewLink: TcxObjectLink;
  APrevFocused, ACycleChanged: Boolean;
  APrevFocusedRecord, ANextRecord, AChildRecord: TcxCustomGridRecord;
  ANextRecordIndex: Integer;

  procedure CheckGridMode;
  begin
    if DataController.IsGridMode and (AFocusedRecordIndex = -1) then
      if AGoForward then
        DataController.GotoFirst
      else
        if AGoOnCycle then
          DataController.GotoLast;
  end;

  procedure ProcessMultiSelect;
  var
    AFocusedView: TcxCustomGridView;
    AFocusedCustomTableView: TcxCustomGridTableView;
  begin
    if GridView.Focused = APrevFocused then Exit;
    CheckFocusedRecordSelectionWhenExit(APrevFocusedRecord);
    AFocusedView := TcxCustomGrid(Control).FocusedView;
    if AFocusedView is TcxCustomGridTableView then
    begin
      AFocusedCustomTableView := TcxCustomGridTableView(AFocusedView);
      if not AFocusedCustomTableView.Controller.IsMultiSelectPersistent then
        AFocusedCustomTableView.Controller.SelectFocusedRecord;
    end;
  end;

begin
  Result := CheckEditing(AFocusedRecordIndex, AGoForward);
  if Result then Exit;
  AGridViewLink := cxAddObjectLink(GridView);
  try
    CheckGridMode;
    APrevFocused := GridView.Focused;
    APrevFocusedRecord := FocusedRecord;
    try
      if AGoForward and AGoIntoDetail and GridView.IsMaster and
        (AFocusedRecordIndex <> -1) then
        ANextRecord := ViewData.Records[AFocusedRecordIndex].GetFirstFocusableChild
      else
        ANextRecord := nil;
      if ANextRecord = nil then
      begin
        ANextRecordIndex := FindNextRecord(AFocusedRecordIndex, AGoForward,
          AGoOnCycle, ACycleChanged);
        if ANextRecordIndex <> -1 then
        begin
          ANextRecord := ViewData.Records[ANextRecordIndex];
          if not AGoForward and AGoIntoDetail and GridView.IsMaster then
          begin
            AChildRecord := ANextRecord.GetLastFocusableChild(True);
            if AChildRecord <> nil then
              ANextRecord := AChildRecord;
          end;
        end
        else
          if AGoForward and CanAppend(True) and (DataController.EditState <> [dceInsert]) then
          begin
            CreateNewRecord(True);
            Result := True;
            Exit;
          end
          else
            if GridView.IsDetail and AGoOutOfDetail then
              if not AGoForward and GridView.MasterGridRecord.CanFocus then
                ANextRecord := GridView.MasterGridRecord
              else
              begin
                Result := not DataController.IsGridMode and
                  MasterController.FocusNextRecord(GridView.MasterGridRecordIndex,
                    AGoForward, AGoOnCycle, not AGoForward, True);
                Exit;
              end;
      end;
      Result := ANextRecord <> nil;
      if Result then
        ANextRecord.Focused := True;
    finally
      if (AGridViewLink.Ref <> nil) and Result then
      begin
        ProcessMultiSelect;
        //Site.Update;
      end;
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

function TcxCustomGridTableController.FocusNextRecordWithSelection(AFocusedRecordIndex: Integer;
  AGoForward, AGoOnCycle, AGoIntoDetail: Boolean; ASyncSelection: Boolean = True): Boolean;
begin
  Result := FocusNextRecord(AFocusedRecordIndex, AGoForward, AGoOnCycle,
    AGoIntoDetail, AGoIntoDetail);
  if Result and ASyncSelection then
    SelectFocusedRecord;
end;

function TcxCustomGridTableController.FocusRecord(AFocusedRecordIndex: Integer;
  ASyncSelection: Boolean): Boolean;
begin
  FocusedRecordIndex := AFocusedRecordIndex;
  Result := FocusedRecordIndex = AFocusedRecordIndex;
  if Result and ASyncSelection then
    SelectFocusedRecord;
end;

procedure TcxCustomGridTableController.MakeFocusedItemVisible;
begin
  MakeItemVisible(FocusedItem);
end;

procedure TcxCustomGridTableController.MakeFocusedRecordVisible;
begin
  if not IsFocusing and not (FIsDetailRecordFocusing and (IsRecordPixelScrolling or GridView.HasPixelScrollingMaster)) then
    MakeRecordVisible(FocusedRecord);
end;

procedure TcxCustomGridTableController.MakeRecordVisible(ARecord: TcxCustomGridRecord);
begin
  if CanMakeRecordVisible(ARecord) then
    DoMakeRecordVisible(ARecord);
end;

procedure TcxCustomGridTableController.SelectAll;
begin
  SelectAllRecords;
end;

procedure TcxCustomGridTableController.SelectAllRecords;
begin
  DataController.SelectAll;
end;

function TcxCustomGridTableController.GoToFirst(ASyncSelection: Boolean = True): Boolean;
begin
  Result := FocusNextRecordWithSelection(-1, True, False, False, ASyncSelection);
end;

function TcxCustomGridTableController.GoToLast(AGoIntoDetail: Boolean;
  ASyncSelection: Boolean = True): Boolean;
begin
  Result := FocusNextRecordWithSelection(-1, False, True, AGoIntoDetail, ASyncSelection);
end;

function TcxCustomGridTableController.GoToNext(AGoIntoDetail: Boolean;
  ASyncSelection: Boolean = True): Boolean;
begin
  Result := FocusNextRecordWithSelection(FocusedRecordIndex, True, False,
    AGoIntoDetail, ASyncSelection);
end;

function TcxCustomGridTableController.GoToPrev(AGoIntoDetail: Boolean;
  ASyncSelection: Boolean = True): Boolean;
begin
  Result := FocusNextRecordWithSelection(FocusedRecordIndex, False, False,
    AGoIntoDetail, ASyncSelection);
end;

procedure TcxCustomGridTableController.InternalDoMakeRecordVisible(ARecord: TcxCustomGridRecord);
var
  AIndex, APrevTopRecordIndex: Integer;
  ATopIndex, ATopOffset: Integer;
begin
  AIndex := ARecord.Index;
  if not IsRecordFullyVisibleAtTop(AIndex) then
    if IsRecordPartVisibleAtTop(AIndex) then
      SetTopRecordIndexWithOffset(InternalTopRecordIndex, 0)
    else
      InternalTopRecordIndex := AIndex;
  if ViewInfo.VisibleRecordCount = 0 then
    if AIndex > InternalTopRecordIndex then
      InternalTopRecordIndex := AIndex
    else
  else
  begin
    Site.LockScrollBars;
    try
      while not IsRecordFullyVisibleAtBottom(AIndex) do
        begin
          APrevTopRecordIndex := InternalTopRecordIndex;
          if IsRecordPixelScrolling then
          begin
            GetPixelScrollTopRecordIndexAndOffsetByBottomRecord(AIndex, ATopIndex, ATopOffset);
            if AIndex = ATopIndex then
              ATopOffset := 0;
            SetTopRecordIndexWithOffset(ATopIndex, ATopOffset);
          end
          else
            InternalTopRecordIndex := GetTopRecordIndexByBottomRecord(AIndex);
          if InternalTopRecordIndex = APrevTopRecordIndex then Break;
        end;
    finally
      Site.UnlockScrollBars;
    end;
  end;
end;

function TcxCustomGridTableController.IsFinish: Boolean;
var
  ACycleChanged: Boolean;
begin
  if DataController.IsGridMode then
    Result := DataController.IsEOF
  else
    Result := FindNextRecord(FocusedRecordIndex, True, False, ACycleChanged) = -1;
end;

function TcxCustomGridTableController.IsStart: Boolean;
var
  ACycleChanged: Boolean;
begin
  if DataController.IsGridMode then
    Result := DataController.IsBOF
  else
    Result := FindNextRecord(FocusedRecordIndex, False, False, ACycleChanged) = -1;
end;


{ TcxGridCustomButtonPainter }

procedure TcxGridCustomButtonPainter.DoDrawFocusRect;
begin
  GetGridViewInfo.Painter.DrawFocusRect(GetFocusRect, False);
end;

procedure TcxGridCustomButtonPainter.DrawFocusRect;
begin
  if CanDrawFocusRect then
    DoDrawFocusRect;
end;

function TcxGridCustomButtonPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

function TcxGridCustomButtonPainter.GetFocusRect: TRect;
begin
  Result := GetGridViewInfo.LookAndFeelPainter.ScaledButtonFocusRect(Canvas, ViewInfo.Bounds, ScaleFactor);
end;

{ TcxGridCheckBoxPainter }

procedure TcxGridCheckBoxPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledCheckButton(Canvas,
    cxRectCenter(ViewInfo.Bounds, ViewInfo.LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor)),
    ViewInfo.ButtonState, ViewInfo.CheckBoxState, ScaleFactor);
end;

function TcxGridCheckBoxPainter.ExcludeFromClipRect: Boolean;
begin
  Result := False;
end;

function TcxGridCheckBoxPainter.GetViewInfo: TcxGridCheckBoxViewInfo;
begin
  Result := TcxGridCheckBoxViewInfo(inherited ViewInfo);
end;

{ TcxGridPinPainter }

procedure TcxGridPinPainter.DrawContent;
begin
  inherited DrawContent;
  if ViewInfo.CanDrawImage then
    DrawImage;
end;

procedure TcxGridPinPainter.DrawImage;
begin
  ViewInfo.LookAndFeelPainter.DrawPin(ViewInfo.Canvas, ViewInfo.GetImageBounds, ViewInfo.Params.TextColor, ViewInfo.Pinned);
end;

function TcxGridPinPainter.GetViewInfo: TcxGridPinViewInfo;
begin
  Result := TcxGridPinViewInfo(inherited ViewInfo);
end;

{ TcxCustomGridFilterButtonPainter }

function TcxGridPartCustomButtonPainter.CanDrawFocusRect: Boolean;
begin
  Result := ViewInfo.Focused;
end;

function TcxGridPartCustomButtonPainter.GetGridViewInfo: TcxCustomGridViewInfo;
begin
  Result := ViewInfo.GridViewInfo;
end;

function TcxGridPartCustomButtonPainter.GetViewInfo: TcxGridCustomButtonViewInfo;
begin
  Result := TcxGridCustomButtonViewInfo(inherited ViewInfo);
end;

{  TcxGridFilterCloseButtonPainter }

procedure TcxGridPartCloseButtonPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledFilterCloseButton(Canvas, ViewInfo.Bounds, ViewInfo.ButtonState, ScaleFactor);
  DrawFocusRect;
end;

{ TcxGridFilterCustomizeButtonPainter }

procedure TcxGridPartButtonPainter.Paint;
begin
  Canvas.Font := ViewInfo.Params.Font;
  ViewInfo.LookAndFeelPainter.DrawScaledButton(Canvas, ViewInfo.Bounds, ViewInfo.Text, ViewInfo.ButtonState, ScaleFactor);
  DrawFocusRect;
end;

{ TcxGridFilterActivateButtonPainter }

function TcxGridFilterActivateButtonPainter.GetViewInfo: TcxGridFilterActivateButtonViewInfo;
begin
  Result := TcxGridFilterActivateButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridFilterActivateButtonPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledFilterActivateButton(Canvas,
    cxRectCenter(ViewInfo.Bounds, ViewInfo.LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor)),
    ViewInfo.ButtonState, ViewInfo.Checked, ScaleFactor);
end;

{ TcxGridFilterDropDownButtonPainter }

procedure TcxGridFilterDropDownButtonPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledFilterDropDownButton(Canvas, ViewInfo.Bounds, ViewInfo.ButtonState, False, ScaleFactor);
end;

{ TcxGridCustomPanelPainter }

function TcxGridPartContainerPainter.GetViewInfo: TcxGridPartContainerViewInfo;
begin
  Result := TcxGridPartContainerViewInfo(inherited ViewInfo);
end;

procedure TcxGridPartContainerPainter.DrawButtons;
var
  I: Integer;
begin
  with ViewInfo.ButtonsViewInfo do
    for I := 0 to Count - 1 do
      Items[I].Paint;
end;

function TcxGridPartContainerPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxGridPartContainerPainter.Paint;
begin
//  DrawButtons;  - commented because of XP
  inherited;
  DrawButtons;
end;

{ TcxGridFilterPainter }

procedure TcxGridFilterPainter.DrawBackground(const R: TRect);
begin
  with ViewInfo do
    LookAndFeelPainter.DrawFilterPanel(Canvas, R, Transparent, Params.Color, BackgroundBitmap);
end;

procedure TcxGridFilterPainter.PrepareCanvasForDrawText;
begin
  inherited;
  FTextWasUnderlined := False;
  if ViewInfo.State in [gcsSelected, gcsPressed] then
    with Canvas.Font do
      if not (fsUnderline in Style) then
      begin
        Style := Style + [fsUnderline];
        FTextWasUnderlined := True;
      end;
end;

procedure TcxGridFilterPainter.UnprepareCanvasForDrawText;
begin
  if FTextWasUnderlined then
    with Canvas.Font do
      Style := Style - [fsUnderline];
  inherited;
end;

function TcxGridFilterPainter.GetViewInfo: TcxGridFilterViewInfo;
begin
  Result := TcxGridFilterViewInfo(inherited ViewInfo);
end;

{ TcxGridFindPanelPainter }

procedure TcxGridFindPanelPainter.DrawBackground(const R: TRect);
begin
  ViewInfo.LookAndFeelPainter.DrawFindPanel(Canvas, R, ViewInfo.Transparent, ViewInfo.Params.Color, ViewInfo.BackgroundBitmap);
end;

procedure TcxGridFindPanelPainter.DrawBorders;
begin
  ViewInfo.LookAndFeelPainter.DrawFindPanelBorder(Canvas, ViewInfo.Bounds, ViewInfo.Borders);
end;

function TcxGridFindPanelPainter.GetViewInfo: TcxGridFindPanelViewInfo;
begin
  Result := TcxGridFindPanelViewInfo(inherited ViewInfo);
end;

{ TcxGridTableDataCellPainter }

function TcxGridTableDataCellPainter.GetViewInfo: TcxGridTableDataCellViewInfo;
begin
  Result := TcxGridTableDataCellViewInfo(inherited ViewInfo);
end;

procedure TcxGridTableDataCellPainter.DrawContent;
begin
  if ViewInfo.Transparent and (ViewInfo.BackgroundBitmap <> nil) then
    DrawBackground;
  DrawEditViewInfo;
end;

function TcxGridTableDataCellPainter.CanDrawFocusRect: Boolean;
begin
  Result := ViewInfo.Focused and ViewInfo.HasFocusRect;
end;

procedure TcxGridTableDataCellPainter.DoDrawFocusRect;
begin
  ViewInfo.GridViewInfo.Painter.DrawFocusRect(GetFocusRect, ViewInfo.GridView.OptionsSelection.HideFocusRectOnExit);
end;

procedure TcxGridTableDataCellPainter.DrawEditViewInfo;
begin
  ViewInfo.EditViewInfo.Paint(Canvas);
end;

procedure TcxGridTableDataCellPainter.DrawFocusRect;
begin
  if CanDrawFocusRect then
    DoDrawFocusRect;
end;

function TcxGridTableDataCellPainter.GetFocusRect: TRect;
begin
  Result := ViewInfo.ContentBounds;
end;

procedure TcxGridTableDataCellPainter.Paint;
begin
  inherited Paint;
  DrawFocusRect;
end;

{ TcxCustomGridRecordPainter }

function TcxCustomGridRecordPainter.GetViewInfo: TcxCustomGridRecordViewInfo;
begin
  Result := TcxCustomGridRecordViewInfo(inherited ViewInfo);
end;

procedure TcxCustomGridRecordPainter.AfterPaint;
begin
  DrawFocusRect;
  if ViewInfo.GridRecord.Expandable and not DrawExpandButtonBeforePaint then
    DrawExpandButton;
  ViewInfo.GridViewInfo.Painter.ExcludeFromBackground(ViewInfo.GetBackgroundBounds);
  inherited;
end;

procedure TcxCustomGridRecordPainter.BeforePaint;
begin
  inherited;
  if ViewInfo.GridRecord.Expandable and DrawExpandButtonBeforePaint then
    DrawExpandButton;
  DrawBackground;
end;

procedure TcxCustomGridRecordPainter.DrawBackground;
begin
  if ViewInfo.Transparent then
    DrawBackground(ViewInfo.BackgroundBitmapBounds);
end;

procedure TcxCustomGridRecordPainter.DrawExpandButton;
var
  AClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    ViewInfo.GridViewInfo.LookAndFeelPainter.DrawScaledExpandButton(
      Canvas, ViewInfo.ExpandButtonBounds, ViewInfo.Expanded, ScaleFactor, clDefault, GetExpandButtonState);
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);  // for speed
  end;
end;

function TcxCustomGridRecordPainter.DrawExpandButtonBeforePaint: Boolean;
begin
  Result := False;  // ViewInfo.GridViewInfo.LookAndFeelPainter.DrawExpandButtonFirst; - for speed
end;

procedure TcxCustomGridRecordPainter.DrawFocusRect;
begin
  if ViewInfo.Focused and ViewInfo.HasFocusRect then
    ViewInfo.GridViewInfo.Painter.DrawFocusRect(ViewInfo.FocusRectBounds, ViewInfo.HideFocusRectOnExit);
end;

function TcxCustomGridRecordPainter.GetExpandButtonState: TcxExpandButtonState;
begin
  if not ViewInfo.Selected then
    Result := cebsNormal
  else
    if ViewInfo.GridView.DrawRecordActive(ViewInfo.GridRecord) then
      Result := cebsSelected
    else
      Result := cebsInactive;
end;

procedure TcxCustomGridRecordPainter.Paint;
begin
end;

{ TcxCustomGridRecordsPainter }

constructor TcxCustomGridRecordsPainter.Create(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridRecordsViewInfo);
begin
  inherited Create;
  FCanvas := ACanvas;
  FViewInfo := AViewInfo;
end;

(*procedure TcxCustomGridRecordsPainter.BeforePaint;
begin  {4}
  with ViewInfo do   //!!! cache bitmaps!
    BackgroundBitmap := GetBackgroundBitmap;
end;*)

procedure TcxCustomGridRecordsPainter.DrawItems;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.Count - 1 do
    if ViewInfo.Items[I].Calculated then
      ViewInfo.Items[I].Paint;
end;

procedure TcxCustomGridRecordsPainter.Paint;
begin
  DrawItems;
end;

procedure TcxCustomGridRecordsPainter.MainPaint;
begin
  //BeforePaint;
  Paint;
end;

{ TcxNavigatorSitePainter }

function TcxNavigatorSitePainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxCustomGridTablePainter }

function TcxCustomGridTablePainter.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxCustomGridTablePainter.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTablePainter.GetViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited ViewInfo);
end;

function TcxCustomGridTablePainter.CanOffset(AItemsOffset, DX, DY: Integer): Boolean;
begin
  Result := not GridView.IsDesigning and  // because of the DesignSelector
    not GridView.OptionsBehavior.RepaintVisibleRecordsOnScroll and
    (ViewInfo.BackgroundBitmap = nil) and (ViewInfo.RecordsViewInfo.BackgroundBitmap = nil) and
    not ViewInfo.NoDataInfoTextAreaVisible;
end;

procedure TcxCustomGridTablePainter.DrawBackground;
begin
  inherited;
  if ViewInfo.NoDataInfoTextAreaVisible then
    DrawInfoText;
end;

procedure TcxCustomGridTablePainter.DrawFilterBar;
begin
  ViewInfo.FilterViewInfo.Paint;
end;

procedure TcxCustomGridTablePainter.DrawFindPanel;
begin
  ViewInfo.FindPanelViewInfo.Paint;
end;

procedure TcxCustomGridTablePainter.DrawInfoText;
var
  AParams: TcxViewParams;
begin
  ViewInfo.GetNoDataInfoTextParams(AParams);
  with Canvas do
  begin
    SetParams(AParams);
    Brush.Style := bsClear;
    DrawText(ViewInfo.NoDataInfoText, ViewInfo.NoDataInfoTextAreaBounds,
      cxAlignCenter or cxWordBreak, True);
    Brush.Style := bsSolid;
  end;
end;

procedure TcxCustomGridTablePainter.DrawNavigator;
begin
  with ViewInfo do
    if NavigatorSiteViewInfo.Visible then
    begin
      NavigatorViewInfo.Paint;
      NavigatorSiteViewInfo.Paint;
    end;
end;

procedure TcxCustomGridTablePainter.DrawRecords;
begin
  ViewInfo.RecordsViewInfo.Paint;
end;

procedure TcxCustomGridTablePainter.Offset(AItemsOffset: Integer);
begin
  GridView.ViewChanged;
end;

procedure TcxCustomGridTablePainter.Offset(DX, DY: Integer);
begin
  GridView.ViewChanged;
end;

procedure TcxCustomGridTablePainter.PaintBefore;
begin
  inherited;
  DrawNavigator;
end;

procedure TcxCustomGridTablePainter.PaintContent;
begin
  inherited;
  DrawRecords;
end;

procedure TcxCustomGridTablePainter.DoOffset(AItemsOffset, DX, DY: Integer);
begin
  if not Site.HandleAllocated then Exit;
  if CanOffset(AItemsOffset, DX, DY) then
  begin
    Controller.BeforePaint;
    try
      if AItemsOffset = 0 then
        Offset(DX, DY)
      else
        Offset(AItemsOffset);
    finally
      Controller.AfterPaint;
    end;
  end
  else
    GridView.ViewChanged;
end;

procedure TcxCustomGridTablePainter.DrawFocusRect(const R: TRect; AHideFocusRect: Boolean);
begin
  if ViewInfo.GridView.OptionsView.FocusRect then inherited;
end;

{ TcxCustomGridPartViewInfo }

constructor TcxCustomGridPartViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited Create(AGridViewInfo);
  if IsPart then
    GridViewInfo.AddPart(Self);
end;

destructor TcxCustomGridPartViewInfo.Destroy;
begin
  if IsPart then
    GridViewInfo.RemovePart(Self);
  inherited;
end;

function TcxCustomGridPartViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridPartViewInfo.GetGridViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited GridViewInfo);
end;

function TcxCustomGridPartViewInfo.GetIndex: Integer;
begin
  Result := GridViewInfo.FParts.IndexOf(Self);
end;

procedure TcxCustomGridPartViewInfo.SetIndex(Value: Integer);
begin
  if Index <> Value then
    GridViewInfo.FParts.Move(Index, Value);
end;

function TcxCustomGridPartViewInfo.CalculateBounds: TRect;
begin
  Result := GridViewInfo.CalculatePartBounds(Self);
end;

procedure TcxCustomGridPartViewInfo.CalculateInvisible;
begin
  FHeight := 0;
  inherited Calculate(0, 0, 0, 0);
end;

procedure TcxCustomGridPartViewInfo.CalculateVisible;
begin
  FHeight := CalculateHeight;
  with CalculateBounds do
    Calculate(Left, Top, Right - Left, Bottom - Top);
end;

function TcxCustomGridPartViewInfo.CustomDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDrawBackground(ACanvas);
  if not Result then
    GridView.DoCustomDrawPartBackground(ACanvas, Self, Result);
end;

{function TcxCustomGridPartViewInfo.GetHeight: Integer;
begin
  Result := inherited GetHeight;
  if Result = 0 then Result := FHeight;
end;}

function TcxCustomGridPartViewInfo.GetIsPart: Boolean;
begin
  Result := True;
end;

function TcxCustomGridPartViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxCustomGridPartPainter;
end;

function TcxCustomGridPartViewInfo.HasCustomDrawBackground: Boolean;
begin
  Result := GridView.HasCustomDrawPartBackground;
end;

procedure TcxCustomGridPartViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  GridViewInfo.InitHitTest(AHitTest);
end;

procedure TcxCustomGridPartViewInfo.MainCalculate;
begin
  if Visible then
    CalculateVisible
  else
    CalculateInvisible;
end;

{ TcxGridPartCustomCellViewInfo }

function TcxGridPartCustomCellViewInfo.CalculateHeight: Integer;
begin
  Result := DoCalculateHeight;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxGridPartCustomCellViewInfo.CalculateWidth: Integer;
begin
  Result := DoCalculateWidth;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxGridPartCustomCellViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

function TcxGridPartCustomCellViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'Unknown';
end;

function TcxGridPartCustomCellViewInfo.GetButtonState: TcxButtonState;
begin
  if not Enabled then
    Result := cxbsDisabled
  else
    if Focused then
      Result := cxbsDefault
    else
      Result := inherited GetButtonState;
end;

function TcxGridPartCustomCellViewInfo.GetEnabled: Boolean;
begin
  Result := True;
end;

function TcxGridPartCustomCellViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

{ TcxGridCustomToggleCellViewInfo }

function TcxGridCustomToggleCellViewInfo.CanToggle: Boolean;
begin
  Result := True;
end;

function TcxGridCustomToggleCellViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

function TcxGridCustomToggleCellViewInfo.GetHeight: Integer;
begin
  Result := CalculateHeight;
end;

function TcxGridCustomToggleCellViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

{ TcxGridCheckViewInfo }

procedure TcxGridCheckBoxViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  FCheckBoxState := CalculateCheckBoxState;
end;

function TcxGridCheckBoxViewInfo.CalculateCheckBoxState: TcxCheckBoxState;
begin
  Result := cbsUnchecked;
end;

procedure TcxGridCheckBoxViewInfo.Click;
begin
  inherited Click;
  if CanToggle then
    Toggle;
end;

function TcxGridCheckBoxViewInfo.DoCalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cy;
end;

function TcxGridCheckBoxViewInfo.DoCalculateWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cx;
end;

function TcxGridCheckBoxViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCheckBoxPainter;
end;

function TcxGridCheckBoxViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridCheckBoxHitTest;
end;

procedure TcxGridCheckBoxViewInfo.Toggle;
begin
  if FCheckBoxState = cbsChecked then
    FCheckBoxState := cbsUnchecked
  else
    FCheckBoxState := cbsChecked;
end;

{ TcxGridDataCellPinViewInfo }

procedure TcxGridPinViewInfo.Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  FPinned := CalculatePinned;
end;

function TcxGridPinViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and CanToggle then
    Toggle;
end;

function TcxGridPinViewInfo.CalculatePinned: Boolean;
begin
  Result := False;
end;

function TcxGridPinViewInfo.CanDrawImage: Boolean;
begin
  Result := True;
end;

function TcxGridPinViewInfo.DoCalculateHeight: Integer;
begin
  Result := GetImageSize.cy + cxMarginsHeight(GetMargins);
end;

function TcxGridPinViewInfo.DoCalculateWidth: Integer;
begin
  Result := GetImageSize.cx + cxMarginsWidth(GetMargins);
end;

function TcxGridPinViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridPinHitTest;
end;

function TcxGridPinViewInfo.GetImageBounds: TRect;
begin
  Result := cxRectCenter(cxRectContent(Bounds, GetMargins), GetImageSize);
end;

function TcxGridPinViewInfo.GetImageSize: TSize;
begin
  Result := ScaleFactor.Apply(cxClasses.Size(cxGridPinWidth, cxGridPinHeight));
end;

function TcxGridPinViewInfo.GetMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(cxGridPinLeftMargin, 0, cxGridPinRightMargin, 0));
end;

function TcxGridPinViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridPinPainter;
end;

procedure TcxGridPinViewInfo.Toggle;
begin
  FPinned := not FPinned;
end;

{ TcxGridPartCustomItemViewInfo }

constructor TcxGridPartContainerItemViewInfo.Create(AContainer: TcxGridPartContainerButtonsViewInfo);
begin
  inherited Create(AContainer.Owner.GridViewInfo);
  FContainer := AContainer;
end;

function TcxGridPartContainerItemViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;
  Result := inherited MouseDown(AHitTest, AButton, AShift);
end;

function TcxGridPartContainerItemViewInfo.MouseMove(
  AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;
  Result := inherited MouseMove(AHitTest, AShift);
end;

function TcxGridPartContainerItemViewInfo.MouseUp(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;
  Result := inherited MouseUp(AHitTest, AButton, AShift);
end;

function TcxGridPartContainerItemViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridPartContainerItemViewInfo.GetAdornerTargetElementVisible: Boolean;
begin
  Result := inherited GetAdornerTargetElementVisible and Container.Owner.Visible;
end;

function TcxGridPartContainerItemViewInfo.GetAlignment: TcxGridFilterButtonAlignment;
begin
  Result := fbaLeft;
end;

function TcxGridPartContainerItemViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FContainer.Owner.Canvas;
end;

function TcxGridPartContainerItemViewInfo.GetVisible: Boolean;
begin
  Result := True;
end;

function TcxGridPartContainerItemViewInfo.GetEnabled: Boolean;
begin
  Result := Container.Enabled;
end;

procedure TcxGridPartContainerItemViewInfo.MouseLeave;
begin
  if not Enabled then
    Exit;
  inherited MouseLeave;
end;

{ TcxGridCustomPanelEditViewInfo }

procedure TcxGridFindPanelEditViewInfo.Calculate(ALeftBound: Integer;
  ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  if AWidth <> -1 then
    AdjustWidth(AWidth);
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  if not UseRightToLeftAlignment then
    FindPanel.UpdateEditBounds(Bounds);
end;

procedure TcxGridFindPanelEditViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FindPanel.UpdateEditBounds(Bounds);
end;

function TcxGridFindPanelEditViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := nil;
end;

procedure TcxGridFindPanelEditViewInfo.Paint(ACanvas: TcxCanvas = nil);
begin
//do nothing
end;

procedure TcxGridFindPanelEditViewInfo.AdjustWidth(var AWidth: Integer);
begin
  AWidth := EnsureRange(AWidth, ScaleFactor.Apply(cxGridFindPanelEditMinWidth), ScaleFactor.Apply(cxGridFindPanelEditMaxWidth));
end;

function TcxGridFindPanelEditViewInfo.DoCalculateHeight: Integer;
begin
  Result := FindPanel.EditHeight;
end;

function TcxGridFindPanelEditViewInfo.DoCalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridFindPanelEditViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'FindBox';
end;

procedure TcxGridFindPanelEditViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsFindPanel, nil, nil, AParams);
end;

function TcxGridFindPanelEditViewInfo.GetFocused: Boolean;
begin
  Result := Controller.FindPanel.IsEditFocused;
end;

procedure TcxGridFindPanelEditViewInfo.ShowEdit;
begin
  FindPanel.ShowEdit;
end;

procedure TcxGridFindPanelEditViewInfo.HideEdit;
begin
  FindPanel.HideEdit;
end;

function TcxGridFindPanelEditViewInfo.GetContainer: TcxGridFindPanelButtonsViewInfo;
begin
  Result := TcxGridFindPanelButtonsViewInfo(inherited Container);
end;

function TcxGridFindPanelEditViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxGridFindPanelEditViewInfo.GetFindPanel: TcxGridFindPanel;
begin
  Result := Controller.FindPanel;
end;

{ TcxGridPartCloseButtonViewInfo }

function TcxGridPartCloseButtonViewInfo.DoCalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterCloseButtonSize(ScaleFactor).Y;
end;

function TcxGridPartCloseButtonViewInfo.DoCalculateWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterCloseButtonSize(ScaleFactor).X;
end;

function TcxGridPartCloseButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'CloseButton'
end;

function TcxGridPartCloseButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridPartCloseButtonPainter;
end;

function TcxGridPartCloseButtonViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

{ TcxGridPartButtonViewInfo }

function TcxGridPartButtonViewInfo.DoCalculateHeight: Integer;
begin
  Result := BorderWidth[bTop] + BorderWidth[bBottom] + TextHeightWithOffset +
    2 * LookAndFeelPainter.ScaledButtonTextOffset(ScaleFactor);
end;

function TcxGridPartButtonViewInfo.DoCalculateWidth: Integer;
begin
  Result := BorderWidth[bLeft] + BorderWidth[bRight] + TextWidthWithOffset +
    2 * LookAndFeelPainter.ScaledButtonTextOffset(ScaleFactor);
end;

function TcxGridPartButtonViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taCenter;
end;

function TcxGridPartButtonViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridPartButtonViewInfo.GetBorders: TcxBorders;
begin
  Result := [bLeft, bTop, bRight, bBottom];
end;

function TcxGridPartButtonViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.ButtonBorderSize;
end;

function TcxGridPartButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridPartButtonPainter;
end;

procedure TcxGridPartButtonViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  AParams.Font := Container.Owner.Params.Font;
end;

function TcxGridPartButtonViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

{ TcxGridFindPanelCloseButtonViewInfo }

procedure TcxGridFindPanelCloseButtonViewInfo.Click;
begin
  inherited Click;
  Controller.FindPanel.CloseButtonExecute;
end;

function TcxGridFindPanelCloseButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFindPanelCloseButtonHitTest;
end;

function TcxGridFindPanelCloseButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridView.FindPanel.IsCloseButtonVisible;
end;

function TcxGridFindPanelCloseButtonViewInfo.GetFocused: Boolean;
begin
  Result := (Controller.FindPanel.FocusedItem = fpfiCloseButton)
    and GridView.IsControlFocused;
end;

function TcxGridFindPanelCloseButtonViewInfo.GetContainer: TcxGridFindPanelButtonsViewInfo;
begin
  Result := TcxGridFindPanelButtonsViewInfo(inherited Container);
end;

function TcxGridFindPanelCloseButtonViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

{ TcxGridFindPanelButtonViewInfo }

function TcxGridFindPanelButtonViewInfo.DoCalculateHeight: Integer;
begin
  Result := Max(inherited DoCalculateHeight, ScaleFactor.Apply(cxGridFindPanelButtonMinHeight));
end;

function TcxGridFindPanelButtonViewInfo.DoCalculateWidth: Integer;
begin
  Result := Max(inherited DoCalculateWidth, ScaleFactor.Apply(cxGridFindPanelButtonMinWidth));
end;

function TcxGridFindPanelButtonViewInfo.GetAlignment: TcxGridFilterButtonAlignment;
begin
  Result := fbaRight;
end;

function TcxGridFindPanelButtonViewInfo.GetContainer: TcxGridFindPanelButtonsViewInfo;
begin
  Result := TcxGridFindPanelButtonsViewInfo(inherited Container);
end;

function TcxGridFindPanelButtonViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

{ TcxGridFindPanelFindButtonViewInfo }

procedure TcxGridFindPanelFindButtonViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  ALeftBound := Container.EditViewInfo.Bounds.Right + Container.GetButtonsOffset;
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
end;

procedure TcxGridFindPanelFindButtonViewInfo.Click;
begin
  inherited Click;
  Controller.FindPanel.FindButtonExecute;
end;

function TcxGridFindPanelFindButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'FindButton';
end;

function TcxGridFindPanelFindButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFindPanelFindButtonHitTest;
end;

function TcxGridFindPanelFindButtonViewInfo.GetText: string;
begin
  Result := Controller.FindPanel.FindButtonCaption;
end;

function TcxGridFindPanelFindButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridView.FindPanel.ShowFindButton;
end;

function TcxGridFindPanelFindButtonViewInfo.GetFocused: Boolean;
begin
  Result := (Controller.FindPanel.FocusedItem = fpfiFindButton) and
    GridView.IsControlFocused;
end;

{ TcxGridFindPanelClearButtonViewInfo }

procedure TcxGridFindPanelClearButtonViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
var
  ALeftPosition: Integer;
begin
  if Container.FindButton.Visible then
    ALeftPosition := Container.FindButton.Bounds.Right
  else
    ALeftPosition := Container.EditViewInfo.Bounds.Right;
  ALeftBound := ALeftPosition + Container.GetButtonsOffset;
  inherited;
end;

procedure TcxGridFindPanelClearButtonViewInfo.Click;
begin
  inherited Click;
  Controller.FindPanel.ClearButtonExecute;
end;

function TcxGridFindPanelClearButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'ClearButton';
end;

function TcxGridFindPanelClearButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFindPanelClearButtonHitTest;
end;

function TcxGridFindPanelClearButtonViewInfo.GetText: string;
begin
  Result := Controller.FindPanel.ClearButtonCaption;
end;

function TcxGridFindPanelClearButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridView.FindPanel.ShowClearButton;
end;

function TcxGridFindPanelClearButtonViewInfo.GetFocused: Boolean;
begin
  Result := (Controller.FindPanel.FocusedItem = fpfiClearButton) and
    GridView.IsControlFocused;
end;

{ TcxGridFilterCloseButtonViewInfo }

procedure TcxGridFilterCloseButtonViewInfo.Click;
begin
  inherited;
  Filter.Clear;
end;

function TcxGridFilterCloseButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFilterCloseButtonHitTest;
end;

function TcxGridFilterCloseButtonViewInfo.GetVisible: Boolean;
begin
  Result := not Filter.IsEmpty;
end;

function TcxGridFilterCloseButtonViewInfo.GetContainer: TcxGridFilterButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo(inherited Container);
end;

function TcxGridFilterCloseButtonViewInfo.GetFilter: TcxDataFilterCriteria;
begin
  Result := Container.FilterViewInfo.Filter;
end;

{ TcxGridFilterActivateButtonViewInfo }

function TcxGridFilterActivateButtonViewInfo.GetChecked: Boolean;
begin
  Result := Filter.Active;
end;

function TcxGridFilterActivateButtonViewInfo.GetContainer: TcxGridFilterButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo(inherited Container);
end;

function TcxGridFilterActivateButtonViewInfo.GetFilter: TcxDataFilterCriteria;
begin
  Result := Container.FilterViewInfo.Filter;
end;

procedure TcxGridFilterActivateButtonViewInfo.Click;
var
  AGridView: TcxCustomGridTableView;
begin
  inherited;
  if not Filter.Active or GridView.IsLongFilterOperation then
  begin
    AGridView := GridView;
    AGridView.BeginFilteringUpdate;
    try
      Filter.Active := not Filter.Active;
    finally
      AGridView.EndFilteringUpdate;
    end;
  end
  else
    Filter.Active := not Filter.Active;
end;

function TcxGridFilterActivateButtonViewInfo.DoCalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterActivateButtonSize(ScaleFactor).Y;
end;

function TcxGridFilterActivateButtonViewInfo.DoCalculateWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterActivateButtonSize(ScaleFactor).X;
end;

function TcxGridFilterActivateButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'ActivationButton';
end;

function TcxGridFilterActivateButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFilterActivateButtonHitTest;
end;

function TcxGridFilterActivateButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridFilterActivateButtonPainter;
end;

function TcxGridFilterActivateButtonViewInfo.GetVisible: Boolean;
begin
  Result := not Filter.IsEmpty;
end;

function TcxGridFilterActivateButtonViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

{ TcxGridFilterDropDownButtonViewInfo }

function TcxGridFilterDropDownButtonViewInfo.GetContainer: TcxGridFilterButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo(inherited Container);
end;

function TcxGridFilterDropDownButtonViewInfo.GetDropDownWindowValue: TcxGridFilterMRUItemsPopup;
begin
  Result := TcxGridFilterMRUItemsPopup(inherited DropDownWindow);
end;

procedure TcxGridFilterDropDownButtonViewInfo.BeforeStateChange;
begin
  inherited;
  if State = gcsPressed then
    Container.FilterViewInfo.State := gcsNone;
end;

function TcxGridFilterDropDownButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := False;
end;

function TcxGridFilterDropDownButtonViewInfo.DoCalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterCloseButtonSize(ScaleFactor).Y;
end;

function TcxGridFilterDropDownButtonViewInfo.DoCalculateWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledFilterCloseButtonSize(ScaleFactor).X;
end;

function TcxGridFilterDropDownButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'MRUListButton';
end;

function TcxGridFilterDropDownButtonViewInfo.GetAlignment: TcxGridFilterButtonAlignment;
begin
  Result := fbaRight;
end;

function TcxGridFilterDropDownButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFilterDropDownButtonHitTest;
end;

function TcxGridFilterDropDownButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridFilterDropDownButtonPainter;
end;

function TcxGridFilterDropDownButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridView.Filtering.IsMRUItemsListAvailable;
end;

function TcxGridFilterDropDownButtonViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

function TcxGridFilterDropDownButtonViewInfo.DropDownWindowExists: Boolean;
begin
  Result := GridView.Controller.HasFilterMRUItemsPopup;
end;

function TcxGridFilterDropDownButtonViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := GridView.Controller.FilterMRUItemsPopup;
end;

function TcxGridFilterDropDownButtonViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := Bounds;
  if IsRightToLeftConverted then
    Result.Right := Container.FilterViewInfo.TextAreaBounds.Right +
      (DropDownWindow.BorderWidths[bLeft] + DropDownWindow.TextOffsetHorz)
  else
    Result.Left := Container.FilterViewInfo.TextAreaBounds.Left -
      (DropDownWindow.BorderWidths[bLeft] + DropDownWindow.TextOffsetHorz);
end;

procedure TcxGridFilterDropDownButtonViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  ALeftBound := Min(ALeftBound, Container.FilterViewInfo.TextBounds.Right +
    ScaleFactor.Apply(cxGridCellTextOffset) + FilterTextOffset + Container.GetButtonsOffset);
  inherited;
end;

{ TcxGridFilterCustomizeButtonViewInfo }

procedure TcxGridFilterCustomizeButtonViewInfo.Click;
begin
  inherited;
  GridView.Filtering.RunCustomizeDialog;
end;

function TcxGridFilterCustomizeButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := 'CustomizationButton';
end;

function TcxGridFilterCustomizeButtonViewInfo.GetAlignment: TcxGridFilterButtonAlignment;
begin
  Result := GridView.FilterBox.CustomizeButtonAlignment;
end;

function TcxGridFilterCustomizeButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFilterCustomizeButtonHitTest;
end;

function TcxGridFilterCustomizeButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@scxGridFilterCustomizeButtonCaption);
end;

procedure TcxGridFilterCustomizeButtonViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  AParams.Font := Container.FilterViewInfo.Params.Font;
end;

function TcxGridFilterCustomizeButtonViewInfo.GetVisible: Boolean;
begin
  Result := GridView.FilterBox.CustomizeDialog;
end;

function TcxGridFilterCustomizeButtonViewInfo.GetContainer: TcxGridFilterButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo(inherited Container);
end;

{ TcxGridPartCustomContainerButtonsViewInfo }

constructor TcxGridPartContainerButtonsViewInfo.Create(AOwner: TcxCustomGridPartViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TdxFastObjectList.Create;
  AddItems;
end;

destructor TcxGridPartContainerButtonsViewInfo.Destroy;
begin
  DestroyItems;
  FItems.Free;
  inherited Destroy;
end;

function TcxGridPartContainerButtonsViewInfo.AddItem(
  AItemClass: TcxGridPartContainerItemViewInfoClass): TcxGridPartContainerItemViewInfo;
begin
  Result := AItemClass.Create(Self);
  FItems.Add(Result);
end;

procedure TcxGridPartContainerButtonsViewInfo.Calculate(const ABounds: TRect);
var
  ALeftMargin, ARightMargin, ALeft, I: Integer;
begin
  ALeftMargin := ABounds.Left + GetFirstOffset;
  ARightMargin := ABounds.Right - GetFirstOffset;
  for I := 0 to Count - 1 do
    if CanCalculateItem(Items[I]) then
    begin
      if Items[I].Alignment = fbaLeft then
        ALeft := ALeftMargin
      else
        ALeft := ARightMargin - Items[I].CalculateWidth;
      Items[I].Calculate(ALeft, MulDiv(ABounds.Top + ABounds.Bottom - Items[I].CalculateHeight, 1, 2));
      if Items[I].Alignment = fbaLeft then
        ALeftMargin := Items[I].Bounds.Right + GetButtonsOffset
      else
        ARightMargin := Items[I].Bounds.Left - GetButtonsOffset;
    end;
end;

function TcxGridPartContainerButtonsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitTest(P);
    if Result <> nil then Break;
  end;
end;

procedure TcxGridPartContainerButtonsViewInfo.DestroyItems;
begin
  FItems.Clear;
end;

procedure TcxGridPartContainerButtonsViewInfo.AddAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AList.AddObject(Items[I].GetAdornerTargetElementName, Items[I]);
end;

function TcxGridPartContainerButtonsViewInfo.CanCalculateItem(
  AItem: TcxGridPartCustomCellViewInfo): Boolean;
begin
  Result := AItem.Visible;
end;

procedure TcxGridPartContainerButtonsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CanCalculateItem(Items[I]) then
      Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridPartContainerButtonsViewInfo.GetButtonsOffset: Integer;
begin
  Result := 0;
end;

function TcxGridPartContainerButtonsViewInfo.GetFirstOffset: Integer;
begin
  Result := 0;
end;

function TcxGridPartContainerButtonsViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridPartContainerButtonsViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(FOwner.GridView);
end;

function TcxGridPartContainerButtonsViewInfo.GetHeight: Integer;
var
  I, AItemHeight: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Visible then
    begin
      AItemHeight := Items[I].CalculateHeight;
      if Result < AItemHeight then Result := AItemHeight;
    end;
  if Result <> 0 then Inc(Result, 2 * GetFirstOffset);
end;

function TcxGridPartContainerButtonsViewInfo.GetItem(Index: Integer): TcxGridPartContainerItemViewInfo;
begin
  Result := TcxGridPartContainerItemViewInfo(FItems[Index]);
end;

function TcxGridPartContainerButtonsViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TcxGridPartContainerButtonsViewInfo.GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer;
var
  I: Integer;
  AItem: TcxGridPartContainerItemViewInfo;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if CanCalculateItem(AItem) and (AItem.Alignment = AAlignment )then
      Inc(Result, AItem.CalculateWidth + GetButtonsOffset);
  end;
end;

function TcxGridPartContainerButtonsViewInfo.GetEnabled: Boolean;
begin
  Result := True;
end;

function TcxGridPartContainerButtonsViewInfo.GetWidthLeftPart: Integer;
begin
  Result := GetWidth(fbaLeft);
end;

function TcxGridPartContainerButtonsViewInfo.GetWidthRightPart: Integer;
begin
  Result := GetWidth(fbaRight);
end;

procedure TcxGridPartContainerButtonsViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridFilterButtonsViewInfo }

procedure TcxGridFilterButtonsViewInfo.AddItems;
begin
  AddItem(TcxGridFilterCustomizeButtonViewInfo);
  AddItem(TcxGridFilterCloseButtonViewInfo);
  AddItem(TcxGridFilterActivateButtonViewInfo);
  FDropDownButtonViewInfo := AddItem(TcxGridFilterDropDownButtonViewInfo) as TcxGridFilterDropDownButtonViewInfo;
end;

procedure TcxGridFilterButtonsViewInfo.DestroyItems;
begin
  inherited DestroyItems;
  FDropDownButtonViewInfo := nil;
end;

function TcxGridFilterButtonsViewInfo.GetButtonsOffset: Integer;
begin
  Result := ScaleFactor.Apply(FilterButtonsOffset);
end;

function TcxGridFilterButtonsViewInfo.GetFirstOffset: Integer;
begin
  Result := ScaleFactor.Apply(FilterButtonsFirstOffset);
end;

function TcxGridFilterButtonsViewInfo.GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer;
begin
  Result := inherited GetWidth(AAlignment);
  if Result <> 0 then
    Inc(Result, 2 * GetFirstOffset - GetButtonsOffset);
end;

function TcxGridFilterButtonsViewInfo.GetEnabled: Boolean;
begin
  Result := GridView.Filtering.IsFilterBoxEnabled;
end;

function TcxGridFilterButtonsViewInfo.GetFilterViewInfo: TcxGridFilterViewInfo;
begin
  Result := TcxGridFilterViewInfo(inherited Owner);
end;

{ TcxGridFindPanelButtonsViewInfo }

procedure TcxGridFindPanelButtonsViewInfo.Calculate(const ABounds: TRect);
begin
  CalculateEditViewInfo(ABounds);
  inherited Calculate(ABounds);
end;

procedure TcxGridFindPanelButtonsViewInfo.AddItems;
begin
  AddItem(TcxGridFindPanelCloseButtonViewInfo);
  FEditViewInfo := TcxGridFindPanelEditViewInfo(AddItem(TcxGridFindPanelEditViewInfo));
  FFindButton := TcxGridFindPanelFindButtonViewInfo(AddItem(TcxGridFindPanelFindButtonViewInfo));
  AddItem(TcxGridFindPanelClearButtonViewInfo);
end;

procedure TcxGridFindPanelButtonsViewInfo.DestroyItems;
begin
  inherited DestroyItems;
  FEditViewInfo := nil;
  FFindButton := nil;
end;

procedure TcxGridFindPanelButtonsViewInfo.CalculateEditViewInfo(const ABounds: TRect);
var
  ALeftPartWidth: Integer;
  AAvailableWidth: Integer;
  ALeftOffset, ARightOffset: Integer;
begin
  ALeftPartWidth := WidthLeftPart;
  if ALeftPartWidth = 0 then
    ALeftPartWidth := GetFirstOffset;
  ALeftOffset := ABounds.Left + ALeftPartWidth;
  ARightOffset := WidthRightPart;
  if ARightOffset = 0 then
    ARightOffset := GetFirstOffset;
  AAvailableWidth := Max(0, Owner.ContentWidth - ALeftOffset - ARightOffset);
  FEditViewInfo.Calculate(ALeftOffset,
    MulDiv(ABounds.Top + ABounds.Bottom - FEditViewInfo.CalculateHeight, 1, 2), AAvailableWidth);
end;

function TcxGridFindPanelButtonsViewInfo.CanCalculateItem(AItem: TcxGridPartCustomCellViewInfo): Boolean;
begin
  Result := inherited CanCalculateItem(AItem) and (AItem <> EditViewInfo);
end;

procedure TcxGridFindPanelButtonsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FEditViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridFindPanelButtonsViewInfo.GetButtonsOffset: Integer;
begin
  Result := FindPanelButtonsOffset;
end;

function TcxGridFindPanelButtonsViewInfo.GetFirstOffset: Integer;
begin
  Result := FindPanelButtonsFirstOffset;
end;

function TcxGridFindPanelButtonsViewInfo.GetWidth(AAlignment: TcxGridFilterButtonAlignment): Integer;
begin
  Result := inherited GetWidth(AAlignment);
  if Result <> 0 then
    Inc(Result, GetFirstOffset);
end;

function TcxGridFindPanelButtonsViewInfo.GetFindPanelViewInfo: TcxGridFindPanelViewInfo;
begin
  Result := TcxGridFindPanelViewInfo(inherited Owner);
end;

{ TcxGridPartCustomContainerViewInfo }

constructor TcxGridPartContainerViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited;
  FButtonsViewInfo := CreateButtonsViewInfo;
end;

destructor TcxGridPartContainerViewInfo.Destroy;
begin
  DestroyButtonsViewInfo;
  inherited;
end;

procedure TcxGridPartContainerViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  FButtonsViewInfo.Calculate(CalculateButtonsViewInfoBounds);
end;

function TcxGridPartContainerViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := FButtonsViewInfo.GetHitTest(P);
  if Result = nil then
    Result := inherited GetHitTest(P);
end;

procedure TcxGridPartContainerViewInfo.GetAdornerTargetElements(AList: TStrings);
begin
  inherited GetAdornerTargetElements(AList);
  ButtonsViewInfo.AddAdornerTargetElements(AList);
end;

function TcxGridPartContainerViewInfo.CalculateButtonsViewInfoBounds: TRect;
begin
  Result := Bounds;
end;

function TcxGridPartContainerViewInfo.CalculateHeight: Integer;
begin
  Result := ButtonsViewInfo.Height;
end;

function TcxGridPartContainerViewInfo.CalculateWidth: Integer;
begin
  Result := GridViewInfo.ClientWidth;
end;

function TcxGridPartContainerViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridPartContainerViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbFilterBox);
end;

function TcxGridPartContainerViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridPartContainerViewInfo.GetIsAutoWidth: Boolean;
begin
  Result := True;
end;

function TcxGridPartContainerViewInfo.GetIsCheck: Boolean;
begin
  Result := True;
end;

function TcxGridPartContainerViewInfo.GetIsScrollable: Boolean;
begin
  Result := False;
end;

function TcxGridPartContainerViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := False;
end;

procedure TcxGridPartContainerViewInfo.DestroyButtonsViewInfo;
begin
  FreeAndNil(FButtonsViewInfo);
end;

procedure TcxGridPartContainerViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FButtonsViewInfo.RightToLeftConversion(ABounds);
end;

{ TcxGridFilterViewInfo }

function TcxGridFilterViewInfo.CalculateHeight: Integer;
var
  AButtonsHeight: Integer;
begin
  Result := FilterTextOffset + TextHeightWithOffset + FilterTextOffset;
  AButtonsHeight := inherited CalculateHeight;
  if Result < AButtonsHeight then Result := AButtonsHeight;
end;

function TcxGridFilterViewInfo.CreateButtonsViewInfo: TcxGridPartContainerButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo.Create(Self);
end;

function TcxGridFilterViewInfo.GetAlignment: TcxGridPartAlignment;
begin
  Result := TcxGridPartAlignment(GridView.FilterBox.Position);
end;

function TcxGridFilterViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFilterHitTest;
end;

function TcxGridFilterViewInfo.GetHotTrack: Boolean;
begin
  Result := Filtering.IsMRUItemsListAvailable;
end;

function TcxGridFilterViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridFilterPainter;
end;

function TcxGridFilterViewInfo.GetText: string;
begin
  Result := Filter.FilterCaption;
  if Result = '' then
    Result := cxGetResourceString(@scxGridFilterIsEmpty);
end;

function TcxGridFilterViewInfo.GetTextAreaBounds: TRect;
var
  AOffsets: TRect;
begin
  Result := inherited GetTextAreaBounds;
  AOffsets := cxNullRect;
  AOffsets.Left := FButtonsViewInfo.WidthLeftPart + FilterTextOffset;
  AOffsets.Right := FButtonsViewInfo.WidthRightPart + FilterTextOffset;
  Result := cxRectContent(Result, AOffsets)
end;

procedure TcxGridFilterViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsFilterBox, nil, nil, AParams);
end;

function TcxGridFilterViewInfo.GetVisible: Boolean;
begin
  Result := (GridView.FilterBox.Visible = fvAlways) or
    (GridView.FilterBox.Visible = fvNonEmpty) and not Filter.IsEmpty;
end;

function TcxGridFilterViewInfo.HasMouse(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited HasMouse(AHitTest) and PtInRect(TextBounds, AHitTest.Pos);
end;

procedure TcxGridFilterViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  if not IsDestroying then
    GridView.ViewChanged(TextBounds);
  inherited;
  if State = gcsPressed then
    ButtonsViewInfo.DropDownButtonViewInfo.State := gcsPressed;
end;

function TcxGridFilterViewInfo.GetButtonsViewInfo: TcxGridFilterButtonsViewInfo;
begin
  Result := TcxGridFilterButtonsViewInfo(inherited ButtonsViewInfo);
end;

function TcxGridFilterViewInfo.GetFilter: TcxDataFilterCriteria;
begin
  Result := GridView.DataController.Filter;
end;

function TcxGridFilterViewInfo.GetFiltering: TcxCustomGridTableFiltering;
begin
  Result := TcxCustomGridTableView(GridView).Filtering;
end;

{ TcxGridFindPanelViewInfo }

procedure TcxGridFindPanelViewInfo.CalculateInvisible;
begin
  inherited CalculateInvisible;
  ButtonsViewInfo.EditViewInfo.HideEdit;
end;

procedure TcxGridFindPanelViewInfo.CalculateVisible;
begin
  CalculateParams;
  inherited CalculateVisible;
  FindPanel.UpdateEditTextHint;
  ButtonsViewInfo.EditViewInfo.ShowEdit;
end;

function TcxGridFindPanelViewInfo.CreateButtonsViewInfo: TcxGridPartContainerButtonsViewInfo;
begin
  Result := TcxGridFindPanelButtonsViewInfo.Create(Self);
end;

function TcxGridFindPanelViewInfo.GetAlignment: TcxGridPartAlignment;
begin
  Result := TcxGridPartAlignment(FindPanel.Options.Position);
end;

function TcxGridFindPanelViewInfo.GetBorders: TcxBorders;
begin
  Result := inherited GetBorders;
  if Alignment = gpaTop then
    Result := Result + [bBottom]
  else
    Result := Result + [bTop];
end;

function TcxGridFindPanelViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFindPanelHitTest;
end;

function TcxGridFindPanelViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridFindPanelPainter;
end;

procedure TcxGridFindPanelViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsFindPanel, nil, nil, AParams);
end;

function TcxGridFindPanelViewInfo.GetVisible: Boolean;
begin
  Result := GridView.Controller.IsFindPanelVisible;
end;

procedure TcxGridFindPanelViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  if AVisible and Visible then
    ButtonsViewInfo.EditViewInfo.ShowEdit;
end;

function TcxGridFindPanelViewInfo.GetButtonsViewInfo: TcxGridFindPanelButtonsViewInfo;
begin
  Result := TcxGridFindPanelButtonsViewInfo(inherited ButtonsViewInfo);
end;

function TcxGridFindPanelViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxGridFindPanelViewInfo.GetFindPanel: TcxGridFindPanel;
begin
  Result := Controller.FindPanel;
end;

{ TcxGridTableCellViewInfo }

constructor TcxGridTableCellViewInfo.Create(ARecordViewInfo: TcxCustomGridRecordViewInfo);
begin
  inherited Create(ARecordViewInfo.GridViewInfo);
  FRecordViewInfo := ARecordViewInfo;
end;

function TcxGridTableCellViewInfo.GetCacheItem: TcxCustomGridTableViewInfoCacheItem;
begin
  Result := FRecordViewInfo.CacheItem;
end;

function TcxGridTableCellViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxGridTableCellViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridTableCellViewInfo.GetGridRecord: TcxCustomGridRecord;
begin
  Result := FRecordViewInfo.GridRecord;
end;

function TcxGridTableCellViewInfo.GetSelected: Boolean;
begin
  if not FSelectedCalculated then
  begin
    FSelected := CalculateSelected;
    FSelectedCalculated := True;
  end;
  Result := FSelected;
end;

function TcxGridTableCellViewInfo.GetGridViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := FRecordViewInfo.GridViewInfo;
end;

function TcxGridTableCellViewInfo.CalculateSelected: Boolean;
begin
  Result := GridView.DrawDataCellSelected(GridRecord, nil, True, Self);
end;

procedure TcxGridTableCellViewInfo.CalculateSelectedNeeded;
begin
  FSelectedCalculated := False;
end;

function TcxGridTableCellViewInfo.GetAlwaysSelected: Boolean;
begin
  Result := False;
end;

function TcxGridTableCellViewInfo.GetCanvas: TcxCanvas;
begin
  Result := GridViewInfo.Canvas;
end;

function TcxGridTableCellViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridRecordHitTest;
end;

function TcxGridTableCellViewInfo.GetHotTrack: Boolean;
begin
  Result := inherited GetHotTrack or RecordViewInfo.InvalidateOnChildHotTrackChanged;
end;

function TcxGridTableCellViewInfo.GetTransparent: Boolean;
begin                                {4}
  Result := FRecordViewInfo.GetCellTransparent(Self);
end;

procedure TcxGridTableCellViewInfo.HotTrackChanged;
begin
  inherited HotTrackChanged;
  RecordViewInfo.ChildHotTrackChanged;
end;

procedure TcxGridTableCellViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridRecordHitTest(AHitTest).GridRecord := GridRecord;
end;

procedure TcxGridTableCellViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  CalculateSelectedNeeded;
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
end;

function TcxGridTableCellViewInfo.CanDrawSelected: Boolean;
begin
  Result := False;
end;

function TcxGridTableCellViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  ASelfLink: TcxObjectLink;
begin
  ASelfLink := cxAddObjectLink(Self);
  try
    Result := FRecordViewInfo.MouseDown(AHitTest, AButton, AShift);
    if Result and (ASelfLink.Ref <> nil) then
      inherited MouseDown(AHitTest, AButton, AShift);
  finally
    cxRemoveObjectLink(ASelfLink);
  end;
end;

{ TcxGridTableDataCellViewInfo }

constructor TcxGridTableDataCellViewInfo.Create(ARecordViewInfo: TcxCustomGridRecordViewInfo;
  AItem: TcxCustomGridTableItem);
begin
  inherited Create(ARecordViewInfo);
  FItem := AItem;
  FItem.AddCell(Self);
  FProperties := GetItemProperties;
  FEditViewInfo := CreateEditViewInfo;
  FStyle := FItem.GetCellStyle;
end;

destructor TcxGridTableDataCellViewInfo.Destroy;
begin
  if FItem <> nil then
  begin
    if FItem.Controller.ClickedCellViewInfo = Self then
      FItem.Controller.ClickedCellViewInfo := nil;
    if not GridView.IsDestroying then
      FItem.ReleaseCellStyle;
    FItem.RemoveCell(Self);
  end;
  FEditViewInfo.Free;
  inherited Destroy;
end;

procedure TcxGridTableDataCellViewInfo.AfterDrawCellBackgroundHandler(ACanvas: TcxCanvas);
begin
  if GridView.HasConditionalFormattingProvider then
    GridView.ConditionalFormattingProvider.AfterDrawCellBackground(Self, EditViewInfo.BackgroundColor, ACanvas);
end;

procedure TcxGridTableDataCellViewInfo.AfterDrawCellValueHandler(ACanvas: TcxCanvas);
begin
  if GridView.HasConditionalFormattingProvider then
    GridView.ConditionalFormattingProvider.AfterDrawCellValue(Self, ACanvas);
end;

procedure TcxGridTableDataCellViewInfo.CalculateEditorBoundsHandler(AViewInfo: TcxCustomEditViewInfo; var R: TRect);
begin
  if GridView.HasConditionalFormattingProvider then
    GridView.ConditionalFormattingProvider.CalculateCellEditorBounds(Self, R);
end;

procedure TcxGridTableDataCellViewInfo.CanDrawEditValueHandler(Sender: TcxCustomEditViewInfo; var Allow: Boolean);
begin
  if GridView.HasConditionalFormattingProvider then
    GridView.ConditionalFormattingProvider.CanDrawCellValue(Self, Allow);
end;

function TcxGridTableDataCellViewInfo.GetEditing: Boolean;
begin
  Result := FItem.Editing and Focused;
end;

function TcxGridTableDataCellViewInfo.GetMousePos: TPoint;
begin
  Result := GridViewInfo.MousePos;
end;

function TcxGridTableDataCellViewInfo.GetProperties: TcxCustomEditProperties;
begin
  Result := FProperties;
  FItem.InitProperties(Result);
end;

function TcxGridTableDataCellViewInfo.GetShowButtons: Boolean;
begin
  Result := FItem.ShowButtons(RecordViewInfo.Focused);
end;

procedure TcxGridTableDataCellViewInfo.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  FEditViewInfo.BackgroundColor := ACanvas.Brush.Color;
  if FEditViewInfo is TcxCustomTextEditViewInfo then
    with TcxCustomTextEditViewInfo(FEditViewInfo) do
    begin
      Font := ACanvas.Font;
      TextColor := ACanvas.Font.Color;
    end;
end;

procedure TcxGridTableDataCellViewInfo.BeforeCustomDraw(ACanvas: TcxCanvas);
begin
  ACanvas.Brush.Color := FEditViewInfo.BackgroundColor;
  if FEditViewInfo is TcxCustomTextEditViewInfo then
    with TcxCustomTextEditViewInfo(FEditViewInfo) do
    begin
      ACanvas.Font := Font;
      ACanvas.Font.Color := TextColor;
    end;
end;

procedure TcxGridTableDataCellViewInfo.InitializeByHiddenEditor(AEditViewInfo: TcxCustomEditViewInfo);
var
  AEdit: TcxCustomEdit;
  ATextEdit: TcxCustomTextEdit;
begin
  Style.ButtonTransparency := ebtNone;
  EditViewData.Focused := Focused;
  if Properties.UseLeftAlignmentOnEditing then
    EditViewData.HorzAlignment := taLeftJustify;
  AEdit := Controller.EditingController.Edit;
  if IsValidTextEdit(AEdit) then
  begin
    ATextEdit := TcxCustomTextEdit(AEdit);
    TcxCustomTextEditViewInfo(AEditViewInfo).Text := ATextEdit.Text;

    EditViewData.SelStart := ATextEdit.SelStart;
    EditViewData.SelLength := ATextEdit.SelLength;
    EditViewData.SelBackgroundColor := clHighlight;
    EditViewData.SelTextColor := clHighlightText;
    TcxCustomTextEditViewData(EditViewData).PrepareSelection(AEditViewInfo);
  end
  else
    EditViewData.EditValueToDrawValue(Value, AEditViewInfo);
end;

procedure TcxGridTableDataCellViewInfo.CalculateEditViewInfo(AEditViewInfo: TcxCustomEditViewInfo; const AMousePos: TPoint);
begin
  InitStyle;
  CreateEditViewData;
  try
    FEditViewData.UseRightToLeftAlignment := Control.UseRightToLeftAlignment;
    FEditViewData.UseRightToLeftReading := Control.UseRightToLeftReading;
    FEditViewData.UseRightToLeftScrollBar := Control.UseRightToLeftScrollBar;
    FEditViewData.InplaceEditParams.Position := GetInplaceEditPosition;
    GetRealEditViewDataContentOffsets(FEditViewData.ContentOffset);
    FEditViewData.IsSelected := Selected;
    AEditViewInfo.Transparent := Transparent;  {4}
    if HasHiddenEditorOnScrollContent then
      InitializeByHiddenEditor(AEditViewInfo)
    else
    begin
      FEditViewData.PaintOptions := [];
      if AutoHeight or MultiLine then
      begin
        Include(FEditViewData.PaintOptions, epoAutoHeight);
        FEditViewData.MaxLineCount := MaxLineCount;
      end;
      if ShowEndEllipsis then
        Include(FEditViewData.PaintOptions, epoShowEndEllipsis);
      InitTextSelection;
      EditViewData.EditValueToDrawValue(DisplayValue, AEditViewInfo);
    end;
    if GridView.HasConditionalFormattingProvider then
      GridView.ConditionalFormattingProvider.CalculateStyleViewInfo(Self);
    FEditViewData.Calculate(Canvas, EditViewDataBounds, AMousePos, cxmbNone, [], AEditViewInfo, True);
  finally
    DestroyEditViewData;
  end;
end;

function TcxGridTableDataCellViewInfo.CalculateHeight: Integer;
var
  AEditSizeProperties: TcxEditSizeProperties;
begin
  CalculateParams;
  if CanAutoHeight then
  begin
    CreateEditViewData;
    try
      SetRectEmpty(FEditViewData.ContentOffset);
      Include(FEditViewData.PaintOptions, epoAllowZeroHeight);
      with AEditSizeProperties do
      begin
        Height := -1;
        MaxLineCount := Self.MaxLineCount;
        Width := cxRectWidth(TextAreaBounds);
        Inc(Width, 2 * (ScaleFactor.Apply(cxGridCellTextOffset) - cxGridEditOffset));
      end;
      if AEditSizeProperties.Width > 0 then
        Result := GetEditSize(AEditSizeProperties).cy
      else
        Result := 0;
      if Result <> 0 then
        Inc(Result, 2 * cxGridEditOffset)
      else
        if not SupportsZeroHeight then
          Result := FItem.CalculateDefaultCellHeight(Canvas, Params.Font);
    finally
      DestroyEditViewData;
    end;
  end
  else
    Result := FItem.CalculateDefaultCellHeight(Canvas, Params.Font);
  GridView.DoGetCellHeight(GridRecord, FItem, Self, Result);
end;

function TcxGridTableDataCellViewInfo.CalculateSelected: Boolean;
begin
  Result := GridView.DrawDataCellSelected(GridRecord, FItem, True, Self);
end;

function TcxGridTableDataCellViewInfo.CanActivateEditOnMouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := CanShowEdit and ((AButton = mbLeft) and CanImmediateEdit(AHitTest));
end;

function TcxGridTableDataCellViewInfo.CanAutoHeight: Boolean;
begin
  Result := FItem.CanAutoHeight;
end;

function TcxGridTableDataCellViewInfo.CanImmediateEdit(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := GridView.OptionsBehavior.AlwaysShowEditor or
    (HotTrack and EditViewInfo.IsHotTrack(AHitTest.Pos));
end;

function TcxGridTableDataCellViewInfo.CanProcessMouseLeave: Boolean;
var
  AControl: TcxControl;
begin
  AControl := Control;
  Result := (AControl <> nil) and AControl.HandleAllocated;
end;

function TcxGridTableDataCellViewInfo.CanShowEdit: Boolean;
begin
  Result := True;
end;

procedure TcxGridTableDataCellViewInfo.CheckEditHotTrack(const AMousePos: TPoint);
var
  AEditViewInfo: TcxCustomEditViewInfo;
begin
  if not GridView.Visible then Exit;
  AEditViewInfo := CreateEditViewInfo;
  try
    CalculateEditViewInfo(AEditViewInfo, AMousePos);
    if FEditViewInfo.Repaint(Control, AEditViewInfo) then
      EditHotTrackChanged;
  finally
    AEditViewInfo.Free;
  end;
end;

function TcxGridTableDataCellViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
  begin
    FItem.DoCustomDrawCell(ACanvas, Self, Result);
    if not Result then
      GridView.DoCustomDrawCell(ACanvas, Self, Result);
  end;
end;

procedure TcxGridTableDataCellViewInfo.DoCalculateParams;
begin
  inherited;
  InitStyle;
  ValidateDrawValue;
end;

procedure TcxGridTableDataCellViewInfo.EditHotTrackChanged;
var
  AEditUpdateNeeded: Boolean;
begin
  AEditUpdateNeeded := GridView.Controller.EditingController.EditUpdateNeeded;
  Invalidate(True);
  if not AEditUpdateNeeded then
    GridView.Controller.EditingController.CancelEditUpdatePost;
end;

function TcxGridTableDataCellViewInfo.EditOwnerGetViewData(out AIsViewDataCreated: Boolean): TcxCustomEditViewData;
begin
  AIsViewDataCreated := False;
  Result := EditViewData;
end;

procedure TcxGridTableDataCellViewInfo.EditOwnerInvalidate(const R: TRect; AEraseBackground: Boolean);
begin
  Invalidate(False);
end;

function TcxGridTableDataCellViewInfo.GetAreaBounds: TRect;
begin
  Result := RecordViewInfo.RecordsViewInfo.GetAreaBoundsForCell(Self);
end;

function TcxGridTableDataCellViewInfo.GetAutoHeight: Boolean;
begin
  Result := RecordViewInfo.AutoHeight;
end;

function TcxGridTableDataCellViewInfo.GetDisplayValue: TcxEditValue;
begin
  if Properties.GetEditValueSource(False) = evsValue then
    Result := GridRecord.Values[FItem.Index]
  else
    Result := GridRecord.DisplayTexts[FItem.Index];
  if GridView.HasConditionalFormattingProvider then
    Result := GridView.ConditionalFormattingProvider.FormatDisplayValue(Self, Result);
end;

function TcxGridTableDataCellViewInfo.GetEditBounds: TRect;
var
  R: TRect;
begin
  Result := EditViewDataBounds;
  InflateRect(Result, -cxGridEditOffset, -cxGridEditOffset);
  if FItem.CanScroll then
  begin
    R := GridViewInfo.ScrollableAreaBoundsForEdit;
    if UseRightToLeftAlignment then
    begin
      if Result.Left < R.Left then
        Result.Left := R.Left;
    end
    else
    begin
      if Result.Right > R.Right then
        Result.Right := R.Right;
    end;
    if Result.Bottom > R.Bottom then
      Result.Bottom := R.Bottom;
  end;
end;

function TcxGridTableDataCellViewInfo.GetEditSize(AEditSizeProperties: TcxEditSizeProperties): TSize;
begin
  FEditViewData.InplaceEditParams.Position := GetInplaceEditPosition;
  with Controller do
  begin
    if AutoHeight and (EditingItem = Item) and EditingController.IsEditAutoHeight and RecordViewInfo.Focused then
      Result := FEditViewData.GetEditingSize(Canvas, EditingController.Edit.EditingValue, AEditSizeProperties)
    else
      Result := FEditViewData.GetEditSize(Canvas, DisplayValue, AEditSizeProperties);
  end
end;

function TcxGridTableDataCellViewInfo.GetEditViewDataBounds: TRect;
begin
  Result := ContentBounds;
end;

procedure TcxGridTableDataCellViewInfo.GetEditViewDataContentOffsets(var R: TRect);
begin
  R := Rect(cxGridEditOffset, cxGridEditOffset, cxGridEditOffset, cxGridEditOffset);
end;

function TcxGridTableDataCellViewInfo.GetFocused: Boolean;
begin
  Result := FItem.Focused and RecordViewInfo.Focused and (FItem.FocusedCellViewInfo = Self);
end;

function TcxGridTableDataCellViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridRecordCellHitTest;
end;

function TcxGridTableDataCellViewInfo.GetHotTrack: Boolean;
begin
  Result := inherited GetHotTrack or ShowButtons or (esoAlwaysHotTrack in FProperties.GetSupportedOperations) and
    (GridView.OptionsBehavior.ImmediateEditor or Focused);
end;

function TcxGridTableDataCellViewInfo.GetItemProperties: TcxCustomEditProperties;
begin
  Result := FItem.GetProperties(GridRecord);
end;

function TcxGridTableDataCellViewInfo.GetMaxLineCount: Integer;
begin
  Result := GridView.OptionsView.CellTextMaxLineCount;
end;

function TcxGridTableDataCellViewInfo.GetMultiLine: Boolean;
begin
  Result := RecordViewInfo.RecordsViewInfo.IsCellMultiLine(Item);
end;

function TcxGridTableDataCellViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridTableDataCellPainter;
end;

function TcxGridTableDataCellViewInfo.GetRealEditViewDataBounds: TRect;
begin
  Result := GetEditViewDataBounds;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ContentBounds);
end;

procedure TcxGridTableDataCellViewInfo.GetRealEditViewDataContentOffsets(var R: TRect);
begin
  GetEditViewDataContentOffsets(R);
  if UseRightToLeftAlignment then
    R := TdxRightToLeftLayoutConverter.ConvertOffsets(R);
end;

function TcxGridTableDataCellViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.CellEndEllipsis;
end;

function TcxGridTableDataCellViewInfo.GetStartTextSelectionInfo(out AParams: TcxViewParams; out ASelStart, ASelLength: Integer): Boolean;
var
  AStyleIndex: Integer;
  AText: string;
  APos: Integer;
begin
  AStyleIndex := 0;
  Result := False;
  if NeedHighlightFindFilterText then
  begin
    ASelStart := Controller.GetFindFilterStartPosition(RecordViewInfo.GridRecord.RecordIndex, Item.Index, AText);
    ASelLength := Length(AText);
    AStyleIndex := vsSearchResultHighlight;
    Result := ASelStart <> -1;
  end
  else
    if RecordViewInfo.RecordsViewInfo.IncSearchingCellViewInfo = Self then
    begin
      ASelStart := 0;
      ASelLength := Length(Controller.IncSearchingText);
      AStyleIndex := vsIncSearch;
      Result := ASelLength > 0;
    end
    else
    begin
      if GridView.HighlightIncrementalFilteringText and Controller.DataController.HasIncrementalFilter and
        (Item.Index = Controller.DataController.GetIncrementalFilterField.Index) then
      begin
        AText := Controller.DataController.GetIncrementalFilterText;
        APos := Pos(AnsiUpperCase(AText), AnsiUpperCase(DisplayValue));
        if APos > 0  then
        begin
          ASelStart := APos - 1;
          ASelLength := Length(AText);
          AStyleIndex := vsIncSearch;
          Result := ASelLength > 0;
        end;
      end;
    end;
  if Result then
    GridView.Styles.GetViewParams(AStyleIndex, nil, nil, AParams);
end;

function TcxGridTableDataCellViewInfo.GetText: string;
begin
  if FEditViewInfo is TcxCustomTextEditViewInfo then
    Result := TcxCustomTextEditViewInfo(FEditViewInfo).Text
  else
    Result := '';
end;

function TcxGridTableDataCellViewInfo.GetTextAreaBounds: TRect;
begin
  Result := cxRectInflate(EditViewDataBounds, -ScaleFactor.Apply(cxGridCellTextOffset));
end;

function TcxGridTableDataCellViewInfo.IsValidTextEdit(AEdit: TcxCustomEdit): Boolean;
begin
  Result := AEdit.HandleAllocated and (AEdit is TcxCustomTextEdit);
end;

function TcxGridTableDataCellViewInfo.GetValue: Variant;
begin
  Result := GridRecord.Values[FItem.Index];
end;

procedure TcxGridTableDataCellViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataCellParams(GridRecord, FItem, AParams, True, Self);
end;

function TcxGridTableDataCellViewInfo.HasCustomDraw: Boolean;
begin
  Result := FItem.HasCustomDrawCell or GridView.HasCustomDrawCell;
end;

function TcxGridTableDataCellViewInfo.HasFocusRect: Boolean;
begin
  Result := not GridView.OptionsSelection.InvertSelect and not Editing;
end;

function TcxGridTableDataCellViewInfo.HasHiddenEditorOnScrollContent: Boolean;
begin
  Result := Controller.EditingController.HasHiddenEditorOnScrollContent and GetFocused;
end;

procedure TcxGridTableDataCellViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridRecordCellHitTest(AHitTest).Item := Item;
end;

procedure TcxGridTableDataCellViewInfo.InitTextSelection;
var
  AParams: TcxViewParams;
  ASelStart, ASelLength: Integer;
begin
  with FEditViewData do
    if GetStartTextSelectionInfo(AParams, ASelStart, ASelLength) then
    begin
      SelStart := ASelStart;
      SelLength := ASelLength;
      SelBackgroundColor := AParams.Color;
      SelTextColor := AParams.TextColor;
    end
    else
      SelLength := 0;
end;

function TcxGridTableDataCellViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := False;
end;

function TcxGridTableDataCellViewInfo.IsTextSelected: Boolean;
begin
  Result := RecordViewInfo.RecordsViewInfo.IncSearchingCellViewInfo = Self;
end;

procedure TcxGridTableDataCellViewInfo.MouseLeave;
begin
  inherited MouseLeave;
  if HotTrack then
    CheckEditHotTrack(Bounds.BottomRight);
end;

function TcxGridTableDataCellViewInfo.NeedHighlightFindFilterText: Boolean;
begin
  Result := GridView.NeedHighlightFindFilterText and Item.CanSearch and (RecordViewInfo.GridRecord.RecordIndex >= 0);
end;

function TcxGridTableDataCellViewInfo.NeedLocalCopyOfEditViewData: Boolean;
begin
  Result := HasHiddenEditorOnScrollContent;
end;

function TcxGridTableDataCellViewInfo.NeedShowEditOnDblClick: Boolean;
begin
  Result := False;
end;

procedure TcxGridTableDataCellViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  FEditViewInfo.Offset(DX, DY);
end;

procedure TcxGridTableDataCellViewInfo.RestoreParams(const AParams: TcxViewParams);
begin
  FEditViewInfo.BackgroundColor := AParams.Color;
  if FEditViewInfo is TcxCustomTextEditViewInfo then
    with TcxCustomTextEditViewInfo(FEditViewInfo) do
    begin
      Font := AParams.Font;
      TextColor := AParams.TextColor;
    end;
end;

procedure TcxGridTableDataCellViewInfo.SaveParams(out AParams: TcxViewParams);
begin
  AParams.Color := FEditViewInfo.BackgroundColor;
  if FEditViewInfo is TcxCustomTextEditViewInfo then
    with TcxCustomTextEditViewInfo(FEditViewInfo) do
    begin
      AParams.Font := Font;
      AParams.TextColor := TextColor;
    end;
end;

procedure TcxGridTableDataCellViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  inherited;
  if State = gcsNone then EditHotTrackChanged;
end;

function TcxGridTableDataCellViewInfo.SupportsZeroHeight: Boolean;
begin
  Result := False;
end;

function TcxGridTableDataCellViewInfo.CanShowAutoHint: Boolean;
begin
  Result := RecordViewInfo.CanShowDataCellHint;
end;

function TcxGridTableDataCellViewInfo.CanShowCustomHint: Boolean;
begin
  Result := Item.HasCellHintHandler;
end;

function TcxGridTableDataCellViewInfo.CanShowHint: Boolean;
begin
  Result := (CanShowAutoHint or CanShowCustomHint) and not Editing;
end;

function TcxGridTableDataCellViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := ContentBounds;
end;

function TcxGridTableDataCellViewInfo.NeedShowHint(const AMousePos: TPoint;
  out AHintText: TCaption; out AIsHintMultiLine: Boolean; out ATextRect: TRect): Boolean;
begin
  if CanShowAutoHint then
    if UseStandardNeedShowHint then
      Result := inherited NeedShowHint(AMousePos, AHintText, AIsHintMultiLine, ATextRect)
    else
      Result := FEditViewInfo.NeedShowHint(Canvas, AMousePos, GetAreaBoundsForHint,
        AHintText, AIsHintMultiLine, ATextRect, GetMaxLineCount)
  else
    Result := False;

  if CanShowCustomHint and (Result or HasHintPoint(AMousePos)) then
  begin
    if not Result then
    begin
      AHintText := '';
      AIsHintMultiLine := False;
      ATextRect := GetHintTextRect(AMousePos);
    end;
    Item.DoGetCellHint(GridRecord, Self, AMousePos, AHintText, AIsHintMultiLine, ATextRect);
    Result := AHintText <> '';
  end;
end;

function TcxGridTableDataCellViewInfo.UseStandardNeedShowHint: Boolean;
begin
  Result := False;
end;

procedure TcxGridTableDataCellViewInfo.InitStyle;
begin
  FItem.InitStyle(FStyle, Params, RecordViewInfo.Focused);
end;

procedure TcxGridTableDataCellViewInfo.ValidateDrawValue;
begin
  FEditViewInfo.ResetValidationInfo;
  if Item.HasValidateDrawValueHandler then
    FItem.DoValidateDrawValue(GridRecord, DisplayValue, FEditViewInfo.ErrorData);
end;

function TcxGridTableDataCellViewInfo.CreateEditViewInfo: TcxCustomEditViewInfo;
begin
  Result := Properties.GetViewInfoClass.Create as TcxCustomEditViewInfo;
  Result.Owner := Self;
  Result.OnAfterDrawBackground.Add(AfterDrawCellBackgroundHandler);
  Result.OnAfterDrawValue.Add(AfterDrawCellValueHandler);
  Result.OnCanDrawEditValue.Add(CanDrawEditValueHandler);
  Result.OnCalculateEditorBounds.Add(CalculateEditorBoundsHandler);
end;

procedure TcxGridTableDataCellViewInfo.CreateEditViewData;
begin
  FEditViewData := FItem.GetEditViewData(GetProperties, FIsLocalCopyOfEditViewData, NeedLocalCopyOfEditViewData);
  FEditViewData.Data := GridRecord;
end;

procedure TcxGridTableDataCellViewInfo.DestroyEditViewData;
begin
  FItem.ReleaseEditViewData(FEditViewData, FIsLocalCopyOfEditViewData);
end;

function TcxGridTableDataCellViewInfo.SupportsEditing: Boolean;
begin
  Result := True;
end;

procedure TcxGridTableDataCellViewInfo.UpdateEdit;
begin
  if not Controller.EditingController.EditPreparing then
    Controller.EditingController.UpdateEdit;
end;

procedure TcxGridTableDataCellViewInfo.BeforeRecalculation;
begin
  GridViewInfo.UpdateMousePos;
  inherited BeforeRecalculation;
end;

procedure TcxGridTableDataCellViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  CalculateEditViewInfo(FEditViewInfo, MousePos);
  Text := GetText;
  if not GridViewInfo.IsInternalUse and Editing then UpdateEdit;
end;

function TcxGridTableDataCellViewInfo.CanDrawSelected: Boolean;
begin
  Result := True;
end;

procedure TcxGridTableDataCellViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  CalculateEditViewInfo(FEditViewInfo, MousePos);
  if not GridViewInfo.IsInternalUse and Editing then UpdateEdit;
end;

function TcxGridTableDataCellViewInfo.GetInplaceEditPosition: TcxInplaceEditPosition;
begin
  Result.Item := FItem;
  Result.RecordIndex := RecordViewInfo.GridRecord.RecordIndex;
end;

procedure TcxGridTableDataCellViewInfo.Invalidate(ARecalculate: Boolean);
begin
  if IsDestroying then Exit;
  if ARecalculate then Recalculate;
  inherited Invalidate;
  {if ARecalculate then
    GridRecord.Invalidate(FItem)
  else
    inherited Invalidate;}
end;

function TcxGridTableDataCellViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AGridViewLink, ASelfLink: TcxObjectLink;
  AItem: TcxCustomGridTableItem;
  AActivateEdit, AHotTrack, APostCheckEdit: Boolean;
begin
  FWasFocusedBeforeClick := Focused;
  AGridViewLink := cxAddObjectLink(GridView);
  try
    AItem := FItem;
    AActivateEdit := CanActivateEditOnMouseDown(AHitTest, AButton, AShift);
    AHotTrack := HotTrack and EditViewInfo.IsHotTrack(AHitTest.Pos);

    APostCheckEdit := False;
    Controller.AllowCheckEdit := False;
    GridView.DontMakeMasterRecordVisible := True;
    ASelfLink := cxAddObjectLink(Self);
    try
      if ssDouble in AShift then
      begin
        Result := GridView.DoCellDblClick(Self, AButton, AShift);
        if Result or (ASelfLink.Ref = nil) then
        begin
          Result := False;
          Exit;
        end;
      end;
      AItem.Controller.SetClickedCellInfo(Self);
      Result := inherited MouseDown(AHitTest, AButton, AShift);
      if AGridViewLink.Ref <> nil then
      begin
        if Result and not AItem.Controller.EditingController.IsErrorOnPost then
        begin
          AItem.Focused := True;
          if AItem.Focused then
            if (AButton <> mbMiddle) and (AShift * [ssCtrl, ssShift] = []) and AActivateEdit then
            begin
              AItem.GridView.Site.FinishDragAndDrop(False);
              if AHotTrack then
                AItem.Controller.EditingController.ShowEdit(AItem, AShift, AHitTest.Pos.X, AHitTest.Pos.Y)
              else
                AItem.Controller.EditingController.ShowEdit(AItem);
            end
            else
              AItem.Controller.IsReadyForImmediateEditing := True
          else
            APostCheckEdit := True;
          if AItem.Controller.EditingItem = AItem then
            AItem.Controller.SetClickedCellInfo(nil);
        end
        else
          AItem.Controller.SetClickedCellInfo(nil);
      end
    finally
      cxRemoveObjectLink(ASelfLink);
      if AGridViewLink.Ref <> nil then
      begin
        AItem.GridView.DontMakeMasterRecordVisible := False;
        AItem.Controller.AllowCheckEdit := True;
        if APostCheckEdit then
          AItem.Controller.PostCheckEdit;
      end;
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

function TcxGridTableDataCellViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := inherited MouseMove(AHitTest, AShift);
  if HotTrack then
    CheckEditHotTrack(AHitTest.Pos);
end;

function TcxGridTableDataCellViewInfo.MouseUp(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AGridView: TcxCustomGridTableView;
  AController: TcxCustomGridTableController;

  procedure ShowEdit;
  begin
    AGridView.DontMakeMasterRecordVisible := True;
    try
      AController.EditingController.ShowEdit(FItem, AShift, AHitTest.Pos.X, AHitTest.Pos.Y);
    finally
      AGridView.DontMakeMasterRecordVisible := False;
    end;
  end;

begin
  inherited MouseUp(AHitTest, AButton, AShift);
  AGridView := GridView;
  AController := AGridView.Controller;

  if AController.IsClickedCell(Self) then
    Result := AGridView.DoCellClick(Self, AButton, AShift)
  else
    Result := False;
  AController.SetClickedCellInfo(nil);
  if Result then Exit;

  if (AButton = mbLeft) and (AShift * [ssCtrl, ssShift] = []) and Focused and CanShowEdit then
  begin
    if not Editing then
      if NeedShowEditOnDblClick then
      begin
        if AController.IsDblClick then
          ShowEdit
      end
      else
        if AGridView.OptionsBehavior.ImmediateEditor then
        begin
          if AController.IsReadyForImmediateEditing then
            ShowEdit
        end
        else
          if FWasFocusedBeforeClick and not AController.IsDblClick then
            AController.EditingController.StartEditShowingTimer(FItem);
    Result := True;
  end;
end;

{ TcxCustomGridRecordViewInfo }

constructor TcxCustomGridRecordViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  ARecord: TcxCustomGridRecord);
begin
  inherited Create(ARecordsViewInfo.GridViewInfo);
  FRecordsViewInfo := ARecordsViewInfo;
  FRecord := ARecord;
  if FRecord <> nil then
  begin
    FIsRecordViewInfoAssigned := FRecord.ViewInfo <> nil;
    if not FIsRecordViewInfoAssigned then
      FRecord.FViewInfo := Self;
  end;
end;

destructor TcxCustomGridRecordViewInfo.Destroy;
begin
  if not FIsRecordViewInfoAssigned and (FRecord <> nil) then
    FRecord.FViewInfo := nil;
  inherited;
end;

function TcxCustomGridRecordViewInfo.GetCacheItem: TcxCustomGridTableViewInfoCacheItem;
begin
  if FRecord = nil then
    Result := nil
  else
    Result := TcxCustomGridTableViewInfoCacheItem(GridView.ViewInfoCache[FRecord.Index]);
end;

function TcxCustomGridRecordViewInfo.GetExpanded: Boolean;
begin
  if not FExpandedCalculated then
  begin
    FExpanded := (FRecord <> nil) and FRecord.Expanded;
    FExpandedCalculated := True;
  end;
  Result := FExpanded;
end;

function TcxCustomGridRecordViewInfo.GetFocused: Boolean;
begin
  Result := GridView.DrawRecordFocused(GridRecord);
end;

function TcxCustomGridRecordViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := GridViewInfo.GridView;
end;

function TcxCustomGridRecordViewInfo.GetGridViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := FRecordsViewInfo.GridViewInfo;
end;

function TcxCustomGridRecordViewInfo.GetIndex: Integer;
begin
  Result := FRecordsViewInfo.FItems.IndexOf(Self);
end;

function TcxCustomGridRecordViewInfo.GetRealFocusRectBounds: TRect;
begin
  Result := GetFocusRectBounds;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxCustomGridRecordViewInfo.GetSelected: Boolean;
begin
  if not FSelectedCalculated then
  begin
    FSelected := CalculateSelected;
    FSelectedCalculated := True;
  end;
  Result := FSelected;
end;

function TcxCustomGridRecordViewInfo.AdjustToIntegralBottomBound(var ABound: Integer): Boolean;
begin
  Result := ContentBounds.Bottom - 1 >= ABound;
  if Result then
    ABound := ContentBounds.Bottom - 1;
end;

function TcxCustomGridRecordViewInfo.CalculateMultilineEditMinHeight: Integer;
begin
  Result := Height;
end;

function TcxCustomGridRecordViewInfo.CalculateSelected: Boolean;
begin
  Result := GridView.DrawRecordSelected(GridRecord);
end;

function TcxCustomGridRecordViewInfo.CanGenerateExpandButtonHitTest: Boolean;
begin
  Result := True;
end;

function TcxCustomGridRecordViewInfo.CanShowDataCellHint: Boolean;
begin
  Result := GridView.OptionsBehavior.CellHints;
end;

procedure TcxCustomGridRecordViewInfo.ChildHotTrackChanged;
begin
  if InvalidateOnChildHotTrackChanged then
    Invalidate;
end;

procedure TcxCustomGridRecordViewInfo.ControlFocusChanged;
begin
end;

function TcxCustomGridRecordViewInfo.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxCustomGridRecordViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := FRecordsViewInfo.BackgroundBitmap;
end;

function TcxCustomGridRecordViewInfo.GetBackgroundBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridRecordViewInfo.GetBackgroundBitmapBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxCustomGridRecordViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FRecordsViewInfo.Canvas;
end;

function TcxCustomGridRecordViewInfo.GetCellTransparent(ACell: TcxGridTableCellViewInfo): Boolean;
begin
  Result := Transparent;
end;

function TcxCustomGridRecordViewInfo.GetContentBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridRecordViewInfo.GetExpandButtonAreaBounds: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxCustomGridRecordViewInfo.GetFocusRectBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridRecordViewInfo.GetFullyVisible: Boolean;
begin
  Result := Index < FRecordsViewInfo.VisibleCount;
end;

function TcxCustomGridRecordViewInfo.GetHeight: Integer;
begin
  if CacheItem.IsHeightAssigned then
    Result := CacheItem.Height
  else
  begin
    Result := CalculateHeight;
    CacheItem.Height := Result;
  end;
end;

function TcxCustomGridRecordViewInfo.GetHideFocusRectOnExit: Boolean;
begin
  Result := GridView.OptionsSelection.HideFocusRectOnExit;
end;

function TcxCustomGridRecordViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridRecordHitTest;
end;

function TcxCustomGridRecordViewInfo.GetPixelScrollSize: Integer;
begin
  Result := Height;
end;

function TcxCustomGridRecordViewInfo.GetRealExpandButtonAreaBounds: TRect;
begin
  Result := GetExpandButtonAreaBounds;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, GetExpandButtonOwnerCellBounds);
end;

function TcxCustomGridRecordViewInfo.GetVisible: Boolean;
begin
  Result := Index < FRecordsViewInfo.VisibleCount;
end;

function TcxCustomGridRecordViewInfo.HasFocusRect: Boolean;
begin
  Result := not GridRecord.CanFocusCells or GridView.OptionsSelection.InvertSelect;
end;

procedure TcxCustomGridRecordViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  (AHitTest as TcxGridRecordHitTest).GridRecord := GridRecord;
end;

function TcxCustomGridRecordViewInfo.InvalidateOnChildHotTrackChanged: Boolean;
begin
  Result := False;
end;

function TcxCustomGridRecordViewInfo.IsClickHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := TcxCustomGridTableController(Controller).IsClickableRecordHitTest(AHitTest);
end;

function TcxCustomGridRecordViewInfo.IsDetailVisible(ADetail: TcxCustomGridView): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRecordViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  OffsetRect(FExpandButtonBounds, DX, DY);
end;

procedure TcxCustomGridRecordViewInfo.VisibilityChanged(AVisible: Boolean);
begin
end;

procedure TcxCustomGridRecordViewInfo.BeforeRecalculation;
begin
  inherited BeforeRecalculation;
  FExpandedCalculated := not Expanded and FExpandedCalculated;
  FRecordsViewInfo.BeforeItemRecalculation;
end;

function TcxCustomGridRecordViewInfo.Click(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AGridViewLink: TcxObjectLink;
begin
  Result := ssDouble in AShift;
  if not Result then
  begin
    AGridViewLink := cxAddObjectLink(GridView);
    try
      GridViewInfo.Controller.FocusedRecord := GridRecord;
    finally
      Result := AGridViewLink.Ref <> nil;
      cxRemoveObjectLink(AGridViewLink);
    end;
  end;
end;

procedure TcxCustomGridRecordViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if GridRecord.Expandable then
    FExpandButtonBounds := TdxRightToLeftLayoutConverter.ConvertRect(FExpandButtonBounds, ABounds);
end;

function TcxCustomGridRecordViewInfo.GetBoundsForInvalidate(AItem: TcxCustomGridTableItem): TRect;
begin
  if AItem = nil then
    Result := Bounds
  else
    Result := GetBoundsForItem(AItem);
end;

function TcxCustomGridRecordViewInfo.GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TcxCustomGridRecordViewInfo.GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo;
begin
  Result := nil;
end;

function TcxCustomGridRecordViewInfo.GetExpandButtonOwnerCellBounds: TRect;
begin
  Result := Bounds;
end;

{procedure TcxCustomGridRecordViewInfo.GetDataCellViewParams(AItem: TcxCustomGridTableItem;
  ACellViewInfo: TcxGridTableCellViewInfo; var AParams: TcxViewParams);
begin
  RecordsViewInfo.GetDataCellViewParams(GridRecord, AItem, ACellViewInfo, AParams);
end;}

function TcxCustomGridRecordViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if CanGenerateExpandButtonHitTest and
    GridRecord.Expandable and PtInRect(ExpandButtonAreaBounds, P) then
  begin
    Result := TcxGridExpandButtonHitTest.Instance(P);
    InitHitTest(Result);
  end
  else
    Result := inherited GetHitTest(P);
end;

procedure TcxCustomGridRecordViewInfo.MainCalculate(ALeftBound, ATopBound: Integer);
begin
  FSelectedCalculated := False;
  Calculate(ALeftBound, ATopBound, Width, Height);
  if GridRecord.Expandable then
    CalculateExpandButtonBounds(FExpandButtonBounds);
end;

function TcxCustomGridRecordViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if not Result and (AButton = mbLeft) and (AHitTest.HitTestCode = htExpandButton) then
  begin
    GridRecord.ToggleExpanded;
    Result := True;
  end;
  if not Result and (AButton <> mbMiddle) and IsClickHitTest(AHitTest) and
    GridView.Site.IsFocused then
    Result := Click(AHitTest, AButton, AShift);
end;

function TcxCustomGridRecordViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRecordViewInfo.Recalculate;
begin
  BeforeRecalculation;
  MainCalculate(Bounds.Left, Bounds.Top);
  AfterRecalculation;
  if UseRightToLeftAlignment then
    RightToLeftConversion(Bounds);
end;

{ TcxCustomGridRecordsViewInfo }

constructor TcxCustomGridRecordsViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited Create;
  FGridViewInfo := AGridViewInfo;
  CreateItems;
end;

destructor TcxCustomGridRecordsViewInfo.Destroy;
begin
  DestroyItems;
  inherited Destroy;
end;

function TcxCustomGridRecordsViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FGridViewInfo.Canvas;
end;

function TcxCustomGridRecordsViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := FGridViewInfo.Controller;
end;

function TcxCustomGridRecordsViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxCustomGridRecordsViewInfo.GetFirstRecordIndex: Integer;
begin
  Result := FGridViewInfo.FirstRecordIndex;
end;

function TcxCustomGridRecordsViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := FGridViewInfo.GridView;
end;

function TcxCustomGridRecordsViewInfo.GetIncSearchingCellViewInfo: TcxGridTableDataCellViewInfo;
begin
  if not FIsIncSearchingCellViewInfoCalculated then
    FIncSearchingCellViewInfo := CalculateIncSearchingCellViewInfo;
  Result := FIncSearchingCellViewInfo;
end;

function TcxCustomGridRecordsViewInfo.GetIsRecordPixelScrollSizeCalculating: Boolean;
begin
  Result := FRecordPixelScrollSizeCalculatingLockCount > 0;
end;

function TcxCustomGridRecordsViewInfo.GetItem(Index: Integer): TcxCustomGridRecordViewInfo;
begin
  if Index < Count then
  begin
    Result := TcxCustomGridRecordViewInfo(FItems[Index]);
    if Result.GridRecord = nil then
      Result := nil;
  end
  else
  begin
    Result := CreateRecordViewInfo(Index);
    AddRecordViewInfo(Result);
  end;
end;

function TcxCustomGridRecordsViewInfo.GetMaxCount: Integer;
begin
  if (FirstRecordIndex < Controller.FirstScrollRecordIndex) or (Controller.ScrollRecordCount = 0) then
    Result := 0
  else
    if GridViewInfo.CalculateDown then
      Result := Max(Controller.LastScrollRecordIndex - FirstRecordIndex + 1, 0)
    else
      Result := FirstRecordIndex - Controller.FirstScrollRecordIndex + 1;
end;

function TcxCustomGridRecordsViewInfo.GetPixelScrollRecordOffset: Integer;
begin
  Result := GridViewInfo.PixelScrollRecordOffset;
end;

function TcxCustomGridRecordsViewInfo.GetTopRecordIndex: Integer;
begin
  Result := GridViewInfo.Controller.TopRecordIndex;
end;

function TcxCustomGridRecordsViewInfo.GetViewData: TcxCustomGridTableViewData;
begin
  Result := FGridViewInfo.ViewData;
end;

procedure TcxCustomGridRecordsViewInfo.CreateItems;
begin
  FItems := TdxFastList.Create;
end;

procedure TcxCustomGridRecordsViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(FItems[I]).Free;
  FItems.Free;
end;

function TcxCustomGridRecordsViewInfo.CreateRecordViewInfo(AIndex: Integer): TcxCustomGridRecordViewInfo;
begin
  Result := CreateRecordViewInfo(ViewData.Records[GetRecordIndex(AIndex)]);
end;

function TcxCustomGridRecordsViewInfo.AddRecordViewInfo(AViewInfo: TcxCustomGridRecordViewInfo): Integer;
begin
  Result := FItems.IndexOf(AViewInfo);
  if Result = -1 then
    Result := FItems.Add(AViewInfo);
end;

procedure TcxCustomGridRecordsViewInfo.AdjustEditorBoundsToIntegralHeight(var AEditorBounds: TRect);
var
  I, ARowIndex: Integer;
  ARowViewInfo: TcxCustomGridRecordViewInfo;
begin
  ARowViewInfo := GetRealItem(Controller.FocusedRecord);
  if ARowViewInfo = nil then
    Exit;
  ARowIndex := ARowViewInfo.Index;
  for I := ARowIndex to PartVisibleCount - 1 do
  begin
    ARowViewInfo := Items[I];
    if ARowViewInfo.AdjustToIntegralBottomBound(AEditorBounds.Bottom) then
      Break;
    if not ARowViewInfo.FullyVisible then
      Break;
  end;
end;

procedure TcxCustomGridRecordsViewInfo.AfterCalculate;
begin
  DestroyEditViewDatas;
  if FGridViewInfo.Visible then
    Controller.EditingController.AfterViewInfoCalculate;
end;

function TcxCustomGridRecordsViewInfo.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridRecordsViewInfo.BeforeCalculate;
begin
  FBackgroundBitmap := GetBackgroundBitmap;
  if FGridViewInfo.Visible then
    Controller.EditingController.BeforeViewInfoCalculate;
  CreateEditViewDatas;
end;

procedure TcxCustomGridRecordsViewInfo.BeforeItemRecalculation;
begin
  GridViewInfo.UpdateMousePos;
  FIsIncSearchingCellViewInfoCalculated := False;
end;

procedure TcxCustomGridRecordsViewInfo.AfterOffset;
begin
end;

procedure TcxCustomGridRecordsViewInfo.BeforeOffset;
begin
  FIsIncSearchingCellViewInfoCalculated := False;
end;

procedure TcxCustomGridRecordsViewInfo.Calculate;
begin
  IsRightToLeftConverted := False;
  FBounds := CalculateBounds;
  FContentBounds := CalculateContentBounds;
end;

function TcxCustomGridRecordsViewInfo.CalculateBounds: TRect;
begin
  Result := FGridViewInfo.ClientBounds;
end;

function TcxCustomGridRecordsViewInfo.CalculateContentBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridRecordsViewInfo.CalculateIncSearchingCellViewInfo: TcxGridTableDataCellViewInfo;
begin
  if Controller.IsIncSearching then
  begin
    Result := Controller.IncSearchingItem.FocusedCellViewInfo;
    FIsIncSearchingCellViewInfoCalculated := Result <> nil;
  end
  else
  begin
    Result := nil;
    FIsIncSearchingCellViewInfoCalculated := True;
  end;
end;

procedure TcxCustomGridRecordsViewInfo.CalculatePixelScrollInfo(
  var ARecordIndex, ARecordOffset: Integer; AMaxRecordIndex, AMaxRecordOffset: Integer;
  ADelta: Integer; out AOverPan: Boolean);

  function NextRecordNeeded(AIndex: Integer; var ASize: Integer): Boolean;
  var
    ARecordSize: Integer;
  begin
    ARecordSize := GetRecordScrollSize(AIndex);
    Result := (ASize >= ARecordSize) and (AIndex < AMaxRecordIndex);
    if Result then
      Dec(ASize, ARecordSize);
  end;

var
  ASize, AScrollDelta, ALastIndexInScrollBand: Integer;
begin
  if ADelta < 0 then
  begin
    ASize := -ADelta - ARecordOffset;
    while NextRecordNeeded(ARecordIndex, ASize) do
    begin
      AScrollDelta := Controller.GetRecordCountInScrollBand(ARecordIndex);
      ARecordIndex := Min(ARecordIndex + AScrollDelta, AMaxRecordIndex);
    end;
    ARecordOffset := -ASize;
    if (ARecordIndex = AMaxRecordIndex) then
      ARecordOffset := Max(ARecordOffset, AMaxRecordOffset);
    AOverPan := (ARecordIndex = AMaxRecordIndex) and (ARecordOffset <> -ASize);
    //FItemsOffset := ADelta + ARecordOffset + ASize;
  end
  else
  begin
    ASize := ADelta + ARecordOffset;
    while (ASize > 0) and (ARecordIndex > Controller.FirstScrollRecordIndex) do
    begin
      AScrollDelta := Controller.GetRecordCountInScrollBand(ARecordIndex);
      ALastIndexInScrollBand := Controller.GetLastIndexInScrollBand(ARecordIndex);
      ARecordIndex := Max(ALastIndexInScrollBand - AScrollDelta, 0);
      Dec(ASize, GetRecordScrollSize(ARecordIndex));
    end;
    ARecordOffset := Min(ASize, 0);
    AOverPan := (ARecordIndex = 0) and (ASize > 0);
    //FItemsOffset := ADelta + ARecordOffset - ASize;
  end;
end;

procedure TcxCustomGridRecordsViewInfo.CalculateVisibleCount;
begin
  FVisibleCount := 0;
  FPartVisibleCount := 0;
end;

procedure TcxCustomGridRecordsViewInfo.Clear;
begin
  FItems.Clear;
end;

procedure TcxCustomGridRecordsViewInfo.CreateEditViewDatas;
var
  I: Integer;
begin
  for I := 0 to GridView.ItemCount - 1 do
    GridView.Items[I].DoCreateEditViewData;
end;

function TcxCustomGridRecordsViewInfo.CreateRecordViewInfo(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo;
begin
  Result := ARecord.GetViewInfoClass.Create(Self, ARecord);
end;

procedure TcxCustomGridRecordsViewInfo.DestroyEditViewDatas;
var
  I: Integer;
begin
  for I := 0 to GridView.ItemCount - 1 do
    GridView.Items[I].DoDestroyEditViewData;
end;

function TcxCustomGridRecordsViewInfo.GetAreaBoundsForCell(ACellViewInfo: TcxGridTableDataCellViewInfo): TRect;
begin
  Result := GridViewInfo.ScrollableAreaBounds;
end;

function TcxCustomGridRecordsViewInfo.GetAutoDataCellHeight: Boolean;
begin
  Result := AutoRecordHeight;
end;

function TcxCustomGridRecordsViewInfo.GetAutoDataRecordHeight: Boolean;
begin
  Result := AutoDataCellHeight;
end;

function TcxCustomGridRecordsViewInfo.GetAutoRecordHeight: Boolean;
begin
  Result := GridView.OptionsView.CellAutoHeight or GridView.IsGetCellHeightAssigned;
end;

function TcxCustomGridRecordsViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbContent);
end;

function TcxCustomGridRecordsViewInfo.GetPixelScrollContentSize: Integer;
begin
  Result := cxRectHeight(ContentBounds);
end;

function TcxCustomGridRecordsViewInfo.GetItemPixelScrollSize(AItem: TcxCustomGridRecordViewInfo): Integer;
begin
  Result := AItem.GetPixelScrollSize;
end;

function TcxCustomGridRecordsViewInfo.GetRecordIndex(AViewInfoIndex: Integer): Integer;
begin
  if FGridViewInfo.CalculateDown then
    Result := FGridViewInfo.FirstRecordIndex + AViewInfoIndex
  else
    Result := FGridViewInfo.FirstRecordIndex - AViewInfoIndex;
end;

procedure TcxCustomGridRecordsViewInfo.DeleteItem(AIndex: Integer);
var
  AItem: TcxCustomGridRecordViewInfo;
begin
  AItem := Items[AIndex];
  FItems.Delete(AIndex);
  AItem.Free;
end;

function TcxCustomGridRecordsViewInfo.GetRecordScrollSize(ARecordIndex: Integer): Integer;
var
  ARecordViewInfo: TcxCustomGridRecordViewInfo;
  ANewlyCreated: Boolean;
begin
  Inc(FRecordPixelScrollSizeCalculatingLockCount);
  try
    ARecordViewInfo := GetRecordViewInfo(ARecordIndex, ANewlyCreated);
    Result := GetItemPixelScrollSize(ARecordViewInfo);
    if ANewlyCreated then
      ARecordViewInfo.Free;
  finally
    Dec(FRecordPixelScrollSizeCalculatingLockCount);
  end;
end;

function TcxCustomGridRecordsViewInfo.GetRecordViewInfo(ARecordIndex: Integer;
  out ANewlyCreated: Boolean): TcxCustomGridRecordViewInfo;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := ViewData.Records[ARecordIndex];
  Result := GetRealItem(ARecord);
  ANewlyCreated := Result = nil;
  if ANewlyCreated then
    Result := CreateRecordViewInfo(ARecord);
end;

function TcxCustomGridRecordsViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := GridViewInfo.ScaleFactor;
end;

function TcxCustomGridRecordsViewInfo.GetViewInfoIndexByRecordIndex(ARecordIndex: Integer): Integer;
begin
  if FGridViewInfo.CalculateDown then
    Result := ARecordIndex - FGridViewInfo.FirstRecordIndex
  else
    Result := FGridViewInfo.FirstRecordIndex - ARecordIndex;
  if (Result < -1) or (Result >= Count) then
    Result := -1
end;

function TcxCustomGridRecordsViewInfo.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TcxCustomGridRecordsViewInfo.ControlFocusChanged;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].ControlFocusChanged;
end;

procedure TcxCustomGridRecordsViewInfo.VisibilityChanged(AVisible: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].VisibilityChanged(AVisible);
end;

function TcxCustomGridRecordsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxCustomGridRecordsPainter;
end;

procedure TcxCustomGridRecordsViewInfo.InsertItem(AIndex: Integer; AItem: Pointer);
begin
  FItems.Insert(AIndex, AItem);
end;

function TcxCustomGridRecordsViewInfo.CanOffset(AItemCountDelta: Integer): Boolean;
begin
  Result := (Count <> 0) and (Abs(AItemCountDelta) < Count);
end;

procedure TcxCustomGridRecordsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
  FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(FContentBounds, ABounds);
end;

function TcxCustomGridRecordsViewInfo.GetCellHeight(ACellContentHeight: Integer): Integer;
begin
  Result := ACellContentHeight;
end;

function TcxCustomGridRecordsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitTest(P);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TcxCustomGridRecordsViewInfo.GetRealItem(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo;
var
  AIndex: Integer;
begin
  Result := ARecord.ViewInfo;
  if Result = nil then
  begin
    AIndex := GetViewInfoIndexByRecordIndex(ARecord.Index);
    if AIndex = -1 then
      Result := nil
    else
      Result := Items[AIndex];
  end;
end;

function TcxCustomGridRecordsViewInfo.IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRecordsViewInfo.MainCalculate;
begin
  BeforeCalculate;
  try
    Calculate;
  finally
    AfterCalculate;
  end;
end;

procedure TcxCustomGridRecordsViewInfo.Offset(DX, DY: Integer);
begin
  OffsetRect(FBounds, DX, DY);
  OffsetRect(FContentBounds, DX, DY);
end;

procedure TcxCustomGridRecordsViewInfo.OffsetRecords(AItemCountDelta, APixelScrollRecordOffsetDelta: Integer);

  procedure DeleteItems;
  var
    I, AIndexFrom: Integer;
  begin
    if AItemCountDelta < 0 then
      AIndexFrom := Count + AItemCountDelta
    else
      AIndexFrom := 0;
    for I := AIndexFrom to AIndexFrom + Abs(AItemCountDelta) - 1 do
      Items[I].Free;
  end;

  procedure OffsetItemsList(AMovedRecordCount: Integer);
  var
    AIndexFrom: Integer;
  begin
    if AItemCountDelta < 0 then
      AIndexFrom := 0
    else
      AIndexFrom := AItemCountDelta;
    with FItems do
      System.Move(List[AIndexFrom], List[AIndexFrom - AItemCountDelta], SizeOf(Pointer) * AMovedRecordCount);
  end;

  procedure OffsetItems(AMovedRecordCount: Integer);
  var
    AIndexFrom, I: Integer;
  begin
    if AItemCountDelta < 0 then
      AIndexFrom := Abs(AItemCountDelta)
    else
      AIndexFrom := 0;
    for I := AIndexFrom to AIndexFrom + AMovedRecordCount - 1 do
      OffsetItem(I, FItemsOffset);
  end;

  procedure CreateItems;
  var
    I: Integer;
  begin
    if AItemCountDelta < 0 then
      I := 0
    else
      I := Count - AItemCountDelta;
    if Count > MaxCount then
        FItems.Count := MaxCount;
    for I := I to I + Abs(AItemCountDelta) - 1 do
      if I < Count then
      begin
        FItems[I] := CreateRecordViewInfo(I);
        Items[I].MainCalculate(GetItemLeftBound(I), GetItemTopBound(I));
        if GridView.UseRightToLeftAlignment then
          Items[I].RightToLeftConversion(Bounds);
      end;
  end;

  procedure CheckForAppearedItems;
  var
    I: Integer;
  begin
    for I := 0 to Count - 1 do
      if not Items[I].Calculated then
      begin
        Items[I].MainCalculate(GetItemLeftBound(I), GetItemTopBound(I));
        if GridView.UseRightToLeftAlignment then
          Items[I].RightToLeftConversion(Bounds);
      end;
  end;

  function GetPrevFocusedItem: TcxCustomGridRecordViewInfo;
  var
    I: Integer;
    APrevFocusedIndex: Integer;
  begin
    Result := nil;
    APrevFocusedIndex := ViewData.Controller.PrevFocusedRecordIndex;
    if ViewData.IsRecordIndexValid(APrevFocusedIndex) then
      for I := 0 to Count - 1 do
        if Items[I].GridRecord.Index = APrevFocusedIndex then
        begin
          Result := Items[I];
          Break;
        end;
  end;

  procedure CalculateContentOffset(AItemsOffset: Integer);
  var
    APrevFocusedItem: TcxCustomGridRecordViewInfo;
  begin
    FItemsOffset := AItemsOffset;
    FPrevFocusedItemBounds := cxEmptyRect;
    if FItemsOffset = 0 then Exit;
    APrevFocusedItem := GetPrevFocusedItem;
    if APrevFocusedItem <> nil then
      FPrevFocusedItemBounds := APrevFocusedItem.Bounds;
  end;

var
  AMovedRecordCount: Integer;
begin
  FItemsOffset := 0;
  AMovedRecordCount := Count - Abs(AItemCountDelta);
  if AItemCountDelta = 0 then
  begin
    CalculateContentOffset(APixelScrollRecordOffsetDelta);
    OffsetItems(AMovedRecordCount);
  end
  else
  begin
    if AItemCountDelta > 0 then
      CalculateContentOffset(GetItemsOffset(AItemCountDelta) + APixelScrollRecordOffsetDelta);
    DeleteItems;
    OffsetItemsList(AMovedRecordCount);
    if AItemCountDelta > 0 then
      OffsetItems(AMovedRecordCount);
    CreateItems;
    if AItemCountDelta < 0 then
    begin
      CalculateContentOffset(GetItemsOffset(AItemCountDelta) + APixelScrollRecordOffsetDelta);
      OffsetItems(AMovedRecordCount);
    end;
  end;
  CalculateVisibleCount;
  CheckForAppearedItems;
end;

procedure TcxCustomGridRecordsViewInfo.Paint;
begin
  with GetPainterClass.Create(Canvas, Self) do
    try
      MainPaint;
    finally
      Free;
    end;
end;

procedure TcxCustomGridRecordsViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridCustomTableNavigatorHelper }

constructor TcxGridCustomTableNavigatorViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited Create(AGridViewInfo);
  FGridViewInfo := AGridViewInfo;
end;

function TcxGridCustomTableNavigatorViewInfo.GetControl: TcxControl;
begin
  Result := FGridViewInfo.Site;
end;

function TcxGridCustomTableNavigatorViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FGridViewInfo.LookAndFeelPainter;
end;

function TcxGridCustomTableNavigatorViewInfo.GetNavigatorOffset: Integer;
begin
  Result := FGridViewInfo.NavigatorOffset;
end;

procedure TcxGridCustomTableNavigatorViewInfo.UpdateNavigatorSiteBounds(const ABounds: TRect);
begin
  FGridViewInfo.NavigatorSiteViewInfo.Calculate(ABounds);
end;

{ TcxNavigatorSiteViewInfo }

constructor TcxNavigatorSiteViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo);
begin
  inherited Create(AGridViewInfo);
  FNavigatorViewInfo := GridViewInfo.GetNavigatorViewInfoClass.Create(GridViewInfo);
end;

destructor TcxNavigatorSiteViewInfo.Destroy;
begin
  if State <> gcsNone then
    MouseLeave;
  FreeAndNil(FNavigatorViewInfo);
  inherited Destroy;
end;

procedure TcxNavigatorSiteViewInfo.Calculate(ALeftBound, ATopBound, AWidth,
  AHeight: Integer);
begin
  ResetCalculatedHeight;
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
end;

function TcxNavigatorSiteViewInfo.GetGridViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited GridViewInfo);
end;

function TcxNavigatorSiteViewInfo.GetNavigator: TcxGridViewNavigator;
begin
  Result := GetGridViewInfo.GridView.Navigator;
end;

function TcxNavigatorSiteViewInfo.CalculateHeight: Integer;
begin
  Result := NavigatorViewInfo.GetHeight;
end;

procedure TcxNavigatorSiteViewInfo.CalculateNavigator;
begin
  NavigatorViewInfo.Calculate;
end;

function TcxNavigatorSiteViewInfo.CalculateWidth: Integer;
begin
  Result := NavigatorViewInfo.GetWidth;
end;

procedure TcxNavigatorSiteViewInfo.GetAdornerTargetElements(AList: TStrings);
begin
  inherited GetAdornerTargetElements(AList);
  NavigatorViewInfo.AddAdornerTargetElements(AList);
end;

function TcxNavigatorSiteViewInfo.GetHeight: Integer;
begin
  if FCalculatedHeight = 0 then
    FCalculatedHeight := CalculateHeight;
  Result := FCalculatedHeight;
end;

function TcxNavigatorSiteViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridNavigatorHitTest;
end;

function TcxNavigatorSiteViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxNavigatorSiteViewInfo.GetNavigatorBounds: TRect;
begin
  Result := NavigatorViewInfo.GetNavigatorBounds;
end;

function TcxNavigatorSiteViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxNavigatorSitePainter;
end;

function TcxNavigatorSiteViewInfo.GetVisible: Boolean;
begin
  Result := Navigator.Visible;
end;

procedure TcxNavigatorSiteViewInfo.MouseLeave;
begin
  inherited MouseLeave;
  if not GridView.IsDestroying then
    NavigatorViewInfo.MouseLeave;
end;

procedure TcxNavigatorSiteViewInfo.NavigatorStateChanged;
begin
  NavigatorViewInfo.Update;
end;

procedure TcxNavigatorSiteViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridViewInfo.GridView.Styles.GetViewParams(vsNavigator, nil, nil, AParams);
end;

function TcxNavigatorSiteViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxNavigatorSiteViewInfo.IsNavigatorSizeChanged: Boolean;
begin
  Result := NavigatorViewInfo.IsNavigatorSizeChanged;
end;

procedure TcxNavigatorSiteViewInfo.ResetCalculatedHeight;
begin
  FCalculatedHeight := 0;
end;

function TcxNavigatorSiteViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  with AHitTest.Pos do
    NavigatorViewInfo.MouseDown(X, Y);
  Result := True;
end;

function TcxNavigatorSiteViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  inherited MouseMove(AHitTest, AShift);
  with AHitTest.Pos do
    NavigatorViewInfo.MouseMove(X, Y);
  Result := True;
end;

function TcxNavigatorSiteViewInfo.MouseUp(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  with AHitTest.Pos do
    NavigatorViewInfo.MouseUp(AButton, X, Y);
  Result := True;
end;

{ TcxCustomGridTableViewInfo }

constructor TcxCustomGridTableViewInfo.Create(AGridView: TcxCustomGridView);
begin
  FParts := TdxFastList.Create;
  inherited;
  FCalculateDown := True;
  FFirstRecordIndex := cxRecordIndexNone;
  FPixelScrollRecordOffset := UnassignedPixelScrollOffset;
end;

destructor TcxCustomGridTableViewInfo.Destroy;
begin
  FreeAndNil(FParts);
  inherited Destroy;
end;

function TcxCustomGridTableViewInfo.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxCustomGridTableViewInfo.GetExpandButtonSize: Integer;
begin
  Result := LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor);
end;

function TcxCustomGridTableViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableViewInfo.GetNavigatorViewInfo: TcxNavigatorViewInfo;
begin
  Result := FNavigatorSiteViewInfo.NavigatorViewInfo;
end;

function TcxCustomGridTableViewInfo.GetPart(Index: Integer): TcxCustomGridPartViewInfo;
begin
  Result := TcxCustomGridPartViewInfo(FParts[Index]);
end;

function TcxCustomGridTableViewInfo.GetPartCount: Integer;
begin
  Result := FParts.Count;
end;

function TcxCustomGridTableViewInfo.GetPartsBottomHeight: Integer;
begin
  Result := GetPartsCustomHeight(gpaBottom);
end;

function TcxCustomGridTableViewInfo.GetPartsCustomHeight(AAlignment: TcxGridPartAlignment): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PartCount - 1 do
    with Parts[I] do
      if Alignment = AAlignment then Inc(Result, Height);
end;

function TcxCustomGridTableViewInfo.GetPartsTopHeight: Integer;
begin
  Result := GetPartsCustomHeight(gpaTop);
end;

function TcxCustomGridTableViewInfo.GetScrollableAreaWidth: Integer;
begin
  with ScrollableAreaBoundsHorz do
    Result := Right - Left;
end;

function TcxCustomGridTableViewInfo.GetViewData: TcxCustomGridTableViewData;
begin
  Result := TcxCustomGridTableViewData(inherited ViewData);
end;

function TcxCustomGridTableViewInfo.GetVisibleRecordCount: Integer;
begin
  Result := FRecordsViewInfo.VisibleCount;
end;

procedure TcxCustomGridTableViewInfo.AddPart(AItem: TcxCustomGridPartViewInfo);
begin
  FParts.Add(AItem);
end;

procedure TcxCustomGridTableViewInfo.RemovePart(AItem: TcxCustomGridPartViewInfo);
begin
  FParts.Remove(AItem);
end;

function TcxCustomGridTableViewInfo.GetNavigatorBounds: TRect;
begin
  Result := FNavigatorSiteViewInfo.GetNavigatorBounds;
end;

function TcxCustomGridTableViewInfo.GetNavigatorButtons: TcxCustomNavigatorButtons;
begin
  Result := GridView.Navigator.Buttons;
end;

function TcxCustomGridTableViewInfo.GetNavigatorCanvas: TCanvas;
begin
  Result := Canvas.Canvas;
end;

function TcxCustomGridTableViewInfo.GetNavigatorControl: TWinControl;
begin
  Result := Site;
end;

function TcxCustomGridTableViewInfo.GetNavigatorFocused: Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableViewInfo.GetNavigatorLookAndFeel: TcxLookAndFeel;
begin
  Result := GridView.LookAndFeel;
end;

function TcxCustomGridTableViewInfo.GetNavigatorOwner: TComponent;
begin
  Result := GridView;
end;

function TcxCustomGridTableViewInfo.GetNavigatorShowHint: Boolean;
begin
  Result := GridView.OptionsBehavior.NavigatorHints;
end;

function TcxCustomGridTableViewInfo.GetNavigatorTabStop: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridTableViewInfo.NavigatorStateChanged;
begin
  FNavigatorSiteViewInfo.NavigatorStateChanged;
end;

procedure TcxCustomGridTableViewInfo.NavigatorChanged(AChangeType: TcxNavigatorChangeType);
const
  ChangeKinds: array[TcxNavigatorChangeType] of TcxGridViewChangeKind = (vcProperty, vcLayout, vcLayout);
begin
  GridView.Changed(ChangeKinds[AChangeType]);
end;

procedure TcxCustomGridTableViewInfo.RefreshNavigator;
begin
  if GridView.IsPattern or GridView.AssigningPattern or (GridView.Control = nil) then
    Exit;
  if FNavigatorSiteViewInfo.IsNavigatorSizeChanged then
    GridView.LayoutChanged
  else
    GridView.ViewChanged;
end;

function TcxCustomGridTableViewInfo.GetNavigatorInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := GridView.Navigator.InfoPanel;
end;

procedure TcxCustomGridTableViewInfo.CreateViewInfos;
begin
  FFindPanelViewInfo := CreateFindPanelViewInfo;
  FFilterViewInfo := GetFilterViewInfoClass.Create(Self);
  inherited;
  FNavigatorSiteViewInfo := GetNavigatorSiteViewInfoClass.Create(Self);
  FRecordsViewInfo := GetRecordsViewInfoClass.Create(Self);
end;

procedure TcxCustomGridTableViewInfo.DestroyViewInfos(AIsRecreating: Boolean);
begin
  FreeAndNil(FRecordsViewInfo);
  FNavigatorSiteViewInfo.Free;
  FNavigatorSiteViewInfo := nil;
  inherited;
  FreeAndNil(FFilterViewInfo);
  FreeAndNil(FFindPanelViewInfo);
end;

function TcxCustomGridTableViewInfo.CreateFindPanelViewInfo: TcxGridFindPanelViewInfo;
begin
  Result := TcxGridFindPanelViewInfo.Create(Self);
end;

procedure TcxCustomGridTableViewInfo.AfterCalculating;
begin
  inherited;
  if Visible then Controller.PostCheckEdit;
end;

procedure TcxCustomGridTableViewInfo.AfterOffset;
begin
  FRecordsViewInfo.AfterOffset;
end;

procedure TcxCustomGridTableViewInfo.BeforeCalculating;
begin
  inherited BeforeCalculating;
  NavigatorSiteViewInfo.ResetCalculatedHeight;
end;

procedure TcxCustomGridTableViewInfo.BeforeOffset;
begin
  FRecordsViewInfo.BeforeOffset;
end;

procedure TcxCustomGridTableViewInfo.Calculate;
begin
  FRecordsViewInfo.MainCalculate;
  inherited;
  CalculateNavigator;
end;

function TcxCustomGridTableViewInfo.CalculateClientBounds: TRect;
begin
  Result := inherited CalculateClientBounds;
  Inc(Result.Top, PartsTopHeight);
  Dec(Result.Bottom, PartsBottomHeight);
end;

function TcxCustomGridTableViewInfo.GetEqualHeightRecordScrollSize: Integer;
begin
  Result := -1;
end;

procedure TcxCustomGridTableViewInfo.CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer;
  var AFullyVisible: Boolean);
begin
  if NavigatorSiteViewInfo.Visible and (TcxControlAccess(Site).IsPopupScrollBars or not Site.HScrollBarVisible) then
    Inc(AHeight, NavigatorSiteViewInfo.Height);
  inherited;
end;

procedure TcxCustomGridTableViewInfo.CalculateNavigator;
begin
  if FNavigatorSiteViewInfo.Visible then
    FNavigatorSiteViewInfo.CalculateNavigator;
end;

function TcxCustomGridTableViewInfo.CalculatePartBounds(APart: TcxCustomGridPartViewInfo): TRect;
var
  I: Integer;
begin
  Result := ClientBounds;
  if APart.IsAutoWidth then
  begin
    Result.Left := Bounds.Left;
    Result.Right := Bounds.Right;
  end
  else
    Result.Right := Result.Left + APart.CalculateWidth;

  for I := PartCount - 1 downto APart.Index do
    case Parts[I].Alignment of
      gpaTop:
        Dec(Result.Top, Parts[I].Height);
      gpaBottom:
        Inc(Result.Bottom, Parts[I].Height);
    end;
  case APart.Alignment of
    gpaTop:
      Result.Bottom := Result.Top + APart.Height;
    gpaBottom:
      Result.Top := Result.Bottom - APart.Height;
  end;
end;

function TcxCustomGridTableViewInfo.CalculateVisibleEqualHeightRecordCount: Integer;
begin
  Result := -1;
end;

procedure TcxCustomGridTableViewInfo.ControlFocusChanged;
begin
  inherited ControlFocusChanged;
  RecordsViewInfo.ControlFocusChanged;
end;

function TcxCustomGridTableViewInfo.DoGetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  if PtInRect(Site.ClientBounds, P) then
  begin
    for I := 0 to PartCount - 1 do
      if Parts[I].Visible then
      begin
        Result := Parts[I].GetHitTest(P);
        if Result <> nil then Exit;
      end;
    Result := RecordsViewInfo.GetHitTest(P);
  end
  else
    Result := FNavigatorSiteViewInfo.GetHitTest(P);
  if Result = nil then
    Result := inherited DoGetHitTest(P);
end;

function TcxCustomGridTableViewInfo.GetBottomNonClientHeight: Integer;
begin
  if CalculateDown and (Control <> nil) and NavigatorSiteViewInfo.Visible then
    Result := NavigatorSiteViewInfo.Height
  else
    Result := inherited GetBottomNonClientHeight;
end;

function TcxCustomGridTableViewInfo.GetContentBounds: TRect;
begin
  Result := RecordsViewInfo.ContentBounds;
end;

function TcxCustomGridTableViewInfo.GetDefaultGridModeBufferCount: Integer;
begin
  Result := 0;
end;

function TcxCustomGridTableViewInfo.GetFirstRecordIndex: Integer;
begin
  if IsFirstRecordIndexAssigned then
    Result := FFirstRecordIndex
  else
    Result := Controller.InternalTopRecordIndex;
end;

procedure TcxCustomGridTableViewInfo.GetHScrollBarBounds(var ABounds: TRect);
begin
  inherited GetHScrollBarBounds(ABounds);
  case TcxGridSiteAccess(Site).GetScrollbarMode of
    sbmTouch:
      begin
        ABounds.Bottom := ScrollableAreaBoundsHorz.Bottom;
        ABounds.Top := ABounds.Bottom - Site.HScrollBar.Height;
      end;
    sbmClassic:
      begin
        if FNavigatorSiteViewInfo.Visible then
        begin
          Inc(ABounds.Left, FNavigatorSiteViewInfo.Width);
          ABounds.Bottom := ABounds.Top + FNavigatorSiteViewInfo.Height;
        end;
      end
  end;
end;

function TcxCustomGridTableViewInfo.GetFilterViewInfoClass: TcxGridFilterViewInfoClass;
begin
  Result := TcxGridFilterViewInfo;
end;

function TcxCustomGridTableViewInfo.GetIsInternalUse: Boolean;
begin
  Result := inherited GetIsInternalUse or IsFirstRecordIndexAssigned or
    (RecordsViewInfo <> nil) and RecordsViewInfo.IsRecordPixelScrollSizeCalculating;
end;

function TcxCustomGridTableViewInfo.GetMultilineEditorBounds(
  const ACellEditBounds: TRect; ACalculatedHeight: Integer; AAutoHeight: TcxInplaceEditAutoHeight): TRect;
begin
  Result := ACellEditBounds;
end;

function TcxCustomGridTableViewInfo.GetNavigatorOffset: Integer;
begin
  Result := GridView.OptionsView.NavigatorOffset;
end;

function TcxCustomGridTableViewInfo.GetNavigatorSiteViewInfoClass: TcxNavigatorSiteViewInfoClass;
begin
  Result := TcxNavigatorSiteViewInfo;
end;

function TcxCustomGridTableViewInfo.GetNavigatorViewInfoClass: TcxGridCustomTableNavigatorViewInfoClass;
begin
  Result := TcxGridCustomTableNavigatorViewInfo;
end;

function TcxCustomGridTableViewInfo.GetNoDataInfoText: string;
begin
  Result := GridView.OptionsView.GetNoDataToDisplayInfoText;
end;

function TcxCustomGridTableViewInfo.GetNoDataInfoTextAreaBounds: TRect;
begin
  if IsRectEmpty(RecordsViewInfo.ContentBounds) then
    Result := ClientBounds
  else
    IntersectRect(Result, ClientBounds, RecordsViewInfo.ContentBounds);
end;

procedure TcxCustomGridTableViewInfo.GetNoDataInfoTextParams(out AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsBackground, nil, nil, AParams);
end;

function TcxCustomGridTableViewInfo.GetNoDataInfoTextAreaVisible: Boolean;
begin
  Result := RecordsViewInfo.IsEmpty and (NoDataInfoText <> '');
end;

function TcxCustomGridTableViewInfo.GetNonRecordsAreaHeight(ACheckScrollBar: Boolean): Integer;
begin
  Result := PartsTopHeight + PartsBottomHeight;
  if ACheckScrollBar then
  begin
    AddScrollBarHeight(Result);
    if NavigatorSiteViewInfo.Visible and
      (TcxControlAccess(Site).IsPopupScrollBars or not Site.HScrollBarVisible) then
      Inc(Result, NavigatorSiteViewInfo.Height);
  end;
end;

function TcxCustomGridTableViewInfo.GetPixelScrollRecordOffset: Integer;
begin
  if IsPixelScrollRecordOffsetAssigned then
    Result := FPixelScrollRecordOffset
  else
    Result := Controller.PixelScrollRecordOffset;
end;

function TcxCustomGridTableViewInfo.GetScrollableAreaBounds: TRect;
begin
  Result := ClientBounds;
end;

function TcxCustomGridTableViewInfo.GetScrollableAreaBoundsForEdit: TRect;
begin
  Result := ScrollableAreaBounds;
end;

function TcxCustomGridTableViewInfo.GetScrollableAreaBoundsHorz: TRect;
begin
  Result := ScrollableAreaBounds;
end;

function TcxCustomGridTableViewInfo.GetScrollableAreaBoundsVert: TRect;
begin
  Result := ScrollableAreaBounds;
end;

function TcxCustomGridTableViewInfo.IsFirstRecordIndexAssigned: Boolean;
begin
  Result := FFirstRecordIndex <> cxRecordIndexNone;
end;

function TcxCustomGridTableViewInfo.IsPixelScrollRecordOffsetAssigned: Boolean;
begin
  Result := FPixelScrollRecordOffset <> UnassignedPixelScrollOffset;
end;

procedure TcxCustomGridTableViewInfo.Offset(DX, DY: Integer);
begin
  RecordsViewInfo.Offset(DX, DY);
end;

procedure TcxCustomGridTableViewInfo.OffsetRecords(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer);
begin
  RecordsViewInfo.OffsetRecords(ARecordCountDelta, APixelScrollRecordOffsetDelta);
end;

procedure TcxCustomGridTableViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  RecordsViewInfo.VisibilityChanged(AVisible);
  inherited VisibilityChanged(AVisible);  // should be here for correct hiding (focus)
  FindPanelViewInfo.VisibilityChanged(AVisible);
end;

function TcxCustomGridTableViewInfo.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer): Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableViewInfo.CanOffsetView(ARecordCountDelta: Integer): Boolean;
begin
  Result := not cxRectIsEmpty(Bounds) and
    RecordsViewInfo.CanOffset(ARecordCountDelta);
end;

procedure TcxCustomGridTableViewInfo.DoOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer);
begin
  if CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY) then
  begin
    Controller.BeforeOffset;
    BeforeOffset;
    if (ARecordCountDelta <> 0) or (APixelScrollRecordOffsetDelta <> 0) then
      OffsetRecords(ARecordCountDelta, APixelScrollRecordOffsetDelta)
    else
      Offset(DX, DY);
    AfterOffset;
    Controller.AfterOffset;
  end
  else
    Recalculate;
end;

procedure TcxCustomGridTableViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FRecordsViewInfo.RightToLeftConversion(ABounds);
end;

function TcxCustomGridTableViewInfo.GetNearestPopupHeight(AHeight: Integer;
  AAdditionalRecord: Boolean = False): Integer;
begin
  Result := AHeight;
end;

function TcxCustomGridTableViewInfo.GetPopupHeight(ADropDownRecordCount: Integer): Integer;
begin
  Result := 0;
end;

procedure TcxCustomGridTableViewInfo.GetVScrollBarBounds(var ABounds: TRect);
begin
  inherited GetVScrollBarBounds(ABounds);
  if TcxGridSiteAccess(Site).IsTouchScrollUIMode then
  begin
    ABounds.Top := ScrollableAreaBoundsVert.Top;
    if not TcxGridSiteAccess(Site).IsScrollBarActive(sbHorizontal) then
      ABounds.Bottom := ScrollableAreaBoundsVert.Bottom
    else
      ABounds.Bottom := ScrollableAreaBoundsHorz.Bottom - Site.HScrollBar.Height;
  end;
end;

{ TcxCustomGridTableViewInfoCacheItem }

function TcxCustomGridTableViewInfoCacheItem.GetGridRecord: TcxCustomGridRecord;
begin
  Result := TcxCustomGridTableViewInfoCache(Owner).ViewData.Records[Index];
end;

procedure TcxCustomGridTableViewInfoCacheItem.SetHeight(Value: Integer);
begin
  FHeight := Value;
  FIsHeightAssigned := True;
end;

procedure TcxCustomGridTableViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  inherited;
  FIsHeightAssigned := False;
end;

{ TcxCustomGridTableViewInfoCache }

function TcxCustomGridTableViewInfoCache.GetViewData: TcxCustomGridTableViewData;
begin
  Result := TcxCustomGridTableViewData(inherited ViewData);
end;

function TcxCustomGridTableViewInfoCache.GetItemClass(Index: Integer): TcxCustomGridViewInfoCacheItemClass;
begin
  Result := ViewData.Records[Index].GetViewInfoCacheItemClass;
end;

{ TcxCustomGridTableItemCustomOptions }

constructor TcxCustomGridTableItemCustomOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited Create(AItem);
end;

function TcxCustomGridTableItemCustomOptions.GetGridView: TcxCustomGridTableView;
begin
  Result := Item.GridView;
end;

function TcxCustomGridTableItemCustomOptions.GetItem: TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(Owner);
end;

procedure TcxCustomGridTableItemCustomOptions.Changed(AChange: TcxGridTableItemChange = ticLayout);
begin
  Item.Changed(AChange);
end;

procedure TcxCustomGridTableItemCustomOptions.Assign(Source: TPersistent);
begin
  if not (Source is TcxCustomGridTableItemCustomOptions) then
    inherited;
end;

{ TcxCustomGridTableItemOptions }

constructor TcxCustomGridTableItemOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited Create(AItem);
  FEditing := True;
  FFiltering := True;
  FFilteringAddValueItems := True;
  FFilteringFilteredItemsList := True;
  FFilteringMRUItemsList := True;
  FFilteringPopup := True;
  FFilteringPopupIncrementalFilteringOptions := [ifoHighlightSearchText, ifoUseContainsOperator];
  FFilteringPopupMultiSelect := True;
  FFilteringWithFindPanel := True;
  FFocusing := True;
  FGrouping := True;
  FIgnoreTimeForFiltering := True;
  FIncSearch := True;
  FMoving := True;
  FShowCaption := True;
  FSorting := True;
end;

procedure TcxCustomGridTableItemOptions.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not FEditing then Item.Editing := False;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFiltering(Value: Boolean);
begin
  if FFiltering <> Value then
  begin
    FFiltering := Value;
    GridView.RefreshFilterableItemsList;
    Changed;
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringAddValueItems(Value: Boolean);
begin
  if FFilteringAddValueItems <> Value then
  begin
    FFilteringAddValueItems := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringExcelPopupApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
begin
  if AValue <> FilteringExcelPopupApplyChanges then
  begin
    FFilteringExcelPopupApplyChanges := AValue;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringExcelPopupDateTimeValuesPageType(
  const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
begin
  if AValue <> FilteringExcelPopupDateTimeValuesPageType then
  begin
    FFilteringExcelPopupDateTimeValuesPageType := AValue;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringExcelPopupDefaultPage(const AValue: TdxExcelFilterValueContainerDefaultPage);
begin
  if AValue <> FilteringExcelPopupDefaultPage then
  begin
    FFilteringExcelPopupDefaultPage := AValue;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringExcelPopupNumericValuesPageType(
  const AValue: TdxExcelFilterValueContainerNumericValuesPageType);
begin
  if AValue <> FilteringExcelPopupNumericValuesPageType then
  begin
    FFilteringExcelPopupNumericValuesPageType := AValue;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringFilteredItemsList(Value: Boolean);
begin
  if FFilteringFilteredItemsList <> Value then
  begin
    FFilteringFilteredItemsList := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringMRUItemsList(Value: Boolean);
begin
  if FFilteringMRUItemsList <> Value then
  begin
    FFilteringMRUItemsList := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetEditAutoHeight(Value: TcxItemInplaceEditAutoHeight);
begin
  if FEditAutoHeight <> Value then
  begin
    FEditAutoHeight := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringPopup(Value: Boolean);
begin
  if FFilteringPopup <> Value then
  begin
    FFilteringPopup := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringPopupMode(const AValue: TdxFilterPopupWindowMode);
begin
  if AValue <> FilteringPopupMode then
  begin
    FFilteringPopupMode := AValue;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringPopupMultiSelect(Value: Boolean);
begin
  if FFilteringPopupMultiSelect <> Value then
  begin
    FFilteringPopupMultiSelect := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFilteringWithFindPanel(AValue: Boolean);
begin
  if AValue <> FFilteringWithFindPanel then
  begin
    FFilteringWithFindPanel := AValue;
    Item.CheckUsingInFindFiltering;
  end;
end;

procedure TcxCustomGridTableItemOptions.SetFocusing(Value: Boolean);
begin
  if FFocusing <> Value then
  begin
    FFocusing := Value;
    if not FFocusing then Item.Focused := False;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetGrouping(Value: Boolean);
begin
  if FGrouping <> Value then
  begin
    FGrouping := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetIgnoreTimeForFiltering(Value: Boolean);
begin
  if FIgnoreTimeForFiltering <> Value then
  begin
    FIgnoreTimeForFiltering := Value;
    GridView.DataController.Refresh;
  end;
end;

procedure TcxCustomGridTableItemOptions.SetIncSearch(Value: Boolean);
begin
  if FIncSearch <> Value then
  begin
    if not Value and Item.IncSearching then
      GridView.Controller.CancelIncSearching;
    FIncSearch := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetMoving(Value: Boolean);
begin
  if FMoving <> Value then
  begin
    FMoving := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    BeforeShowCaptionChange;
    FShowCaption := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetShowEditButtons(Value: TcxGridItemShowEditButtons);
begin
  if FShowEditButtons <> Value then
  begin
    FShowEditButtons := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItemOptions.SetSortByDisplayText(Value: TcxGridItemSortByDisplayText);
begin
  if FSortByDisplayText <> Value then
  begin
    FSortByDisplayText := Value;
    GridView.DataController.SortByDisplayTextChanged;
  end;
end;

procedure TcxCustomGridTableItemOptions.SetSorting(Value: Boolean);
begin
  if FSorting <> Value then
  begin
    FSorting := Value;
    Changed(ticProperty);
  end;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowApplyMode: TdxFilterApplyChangesMode;
begin
  Result := GridView.Filtering.ItemPopup.ApplyMultiSelectChanges;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowButtonCaption: string;
begin
  Result := cxGetResourceString(@scxGridFilterApplyButtonCaption);
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowIncrementalFiltering: Boolean;
begin
  Result := FilteringPopupIncrementalFiltering;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
begin
  Result := FilteringPopupIncrementalFilteringOptions;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowVisibleItemCount: Integer;
begin
  Result := GridView.Filtering.ItemPopup.MaxDropDownItemCount;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowShowCheckBoxes: Boolean;
begin
  Result := Item.CanFilterUsingChecks;
end;

function TcxCustomGridTableItemOptions.GetFilterPopupWindowWidth: Integer;
begin
  Result := GridView.Filtering.ItemPopup.DropDownWidth;
end;

function TcxCustomGridTableItemOptions.GetExcelFilterPopupApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
begin
  Result := FilteringExcelPopupApplyChanges;
  if Result = efacDefault then
    Result := GridView.Filtering.ItemExcelPopup.ApplyChanges;
end;

function TcxCustomGridTableItemOptions.GetExcelFilterPopupDateTimeValuesPageType:
  TdxExcelFilterValueContainerDateTimeValuesPageType;
begin
  Result := FilteringExcelPopupDateTimeValuesPageType;
  if Result = dvptDefault then
    Result := GridView.Filtering.ItemExcelPopup.DateTimeValuesPageType;
end;

function TcxCustomGridTableItemOptions.GetExcelFilterPopupDefaultPage: TdxExcelFilterValueContainerDefaultPage;
begin
  Result := FilteringExcelPopupDefaultPage;
  if Result = dpDefault then
    Result := GridView.Filtering.ItemExcelPopup.DefaultPage;
end;

function TcxCustomGridTableItemOptions.GetExcelFilterPopupNumericValuesPageType:
  TdxExcelFilterValueContainerNumericValuesPageType;
begin
  Result := FilteringExcelPopupNumericValuesPageType;
  if Result = nvptDefault then
    Result := GridView.Filtering.ItemExcelPopup.NumericValuesPageType;
end;

procedure TcxCustomGridTableItemOptions.BeforeShowCaptionChange;
begin
end;

procedure TcxCustomGridTableItemOptions.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableItemOptions then
    with TcxCustomGridTableItemOptions(Source) do
    begin
      Self.Editing := Editing;
      Self.EditAutoHeight := EditAutoHeight;
      Self.Filtering := Filtering;
      Self.FilteringAddValueItems := FilteringAddValueItems;
      Self.FilteringExcelPopupDateTimeValuesPageType := FilteringExcelPopupDateTimeValuesPageType;
      Self.FilteringExcelPopupDefaultPage := FilteringExcelPopupDefaultPage;
      Self.FilteringExcelPopupApplyChanges := FilteringExcelPopupApplyChanges;
      Self.FilteringExcelPopupNumericValuesPageType := FilteringExcelPopupNumericValuesPageType;
      Self.FilteringFilteredItemsList := FilteringFilteredItemsList;
      Self.FilteringMRUItemsList := FilteringMRUItemsList;
      Self.FilteringPopup := FilteringPopup;
      Self.FilteringPopupIncrementalFiltering := FilteringPopupIncrementalFiltering;
      Self.FilteringPopupIncrementalFilteringOptions := FilteringPopupIncrementalFilteringOptions;
      Self.FilteringPopupMode := FilteringPopupMode;
      Self.FilteringPopupMultiSelect := FilteringPopupMultiSelect;
      Self.FilteringWithFindPanel := FilteringWithFindPanel;
      Self.Focusing := Focusing;
      Self.Grouping := Grouping;
      Self.IgnoreTimeForFiltering := IgnoreTimeForFiltering;
      Self.IncSearch := IncSearch;
      Self.Moving := Moving;
      Self.ShowCaption := ShowCaption;
      Self.ShowEditButtons := ShowEditButtons;
      Self.SortByDisplayText := SortByDisplayText;
      Self.Sorting := Sorting;
    end;
  inherited;
end;

{ TcxCustomGridTableItemStyles }

function TcxCustomGridTableItemStyles.GetGridViewValue: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableItemStyles.GetItem: TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(GetOwner);
end;

procedure TcxCustomGridTableItemStyles.SetOnGetContentStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetContentStyle, Value) then
  begin
    FOnGetContentStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItemStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  case Index of
    isContent:
      GridView.Styles.GetRecordContentParams(TcxCustomGridRecord(AData), Item, AParams);
  else
    inherited;
  end;
end;

function TcxCustomGridTableItemStyles.GetGridView: TcxCustomGridView;
begin
  Result := Item.GridView;
end;

procedure TcxCustomGridTableItemStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxCustomGridTableItemStyles then
    with TcxCustomGridTableItemStyles(Source) do
    begin
      Self.Content := Content;
      Self.OnGetContentStyle := OnGetContentStyle;
    end;
end;

procedure TcxCustomGridTableItemStyles.GetContentParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetContentStyle) then
    FOnGetContentStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isContent, ARecord, AStyle, AParams);
  GridView.Styles.GetConditionalFormattingContentParams(ARecord, Item, AParams);
end;

{ TcxCustomGridTableItem }

constructor TcxCustomGridTableItem.Create(AOwner: TComponent);
begin
  inherited;
  FCells := TdxFastList.Create;
  FIndex := -1;
  FMinWidth := cxGridItemDefaultMinWidth;
  FVisibleIndex := -1;
  FVisibleForCustomization := True;
  CreateSubClasses;
  FHeaderAlignmentVert := DefaultHeaderAlignmentVert;
end;

destructor TcxCustomGridTableItem.Destroy;
var
  I: Integer;
begin
  if not GridView.IsDestroying and GridView.IsDesigning then
    Controller.DesignController.UnselectObject(Self);
  DataController.RemoveFindFilterItem(Index);
  RepositoryItem := nil;
  FGridView.RemoveItem(Self);
  {if FLastUsedDefaultRepositoryItem <> nil then
    ItemRemoved(FLastUsedDefaultRepositoryItem);}
  DestroySubClasses;
  FreeAndNil(FEditData);
  FreeAndNil(FCellStyle);
  for I := 0 to CellCount - 1 do Cells[I].FItem := nil;
  FCells.Free;
  inherited Destroy;
end;

function TcxCustomGridTableItem.GetActualMinWidth: Integer;
begin
  if HasFixedWidth then
    Result := 0
  else
    Result := FMinWidth;
end;

function TcxCustomGridTableItem.GetCaption: string;
begin
  if IsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TcxCustomGridTableItem.GetCell(Index: Integer): TcxGridTableDataCellViewInfo;
begin
  Result := TcxGridTableDataCellViewInfo(FCells[Index]);
end;

function TcxCustomGridTableItem.GetCellCount: Integer;
begin
  Result := FCells.Count;
end;

function TcxCustomGridTableItem.GetController: TcxCustomGridTableController;
begin
  Result := FGridView.Controller;
end;

function TcxCustomGridTableItem.GetDataController: TcxCustomDataController;
begin
  Result := FGridView.DataController;
end;

function TcxCustomGridTableItem.GetEditing: Boolean;
begin
  Result := Controller.EditingItem = Self;
end;

function TcxCustomGridTableItem.GetEditingProperties: TcxCustomEditProperties;
begin
  if Controller.EditingController.IsEditing then
    Result := Controller.EditingController.Edit.ActiveProperties
  else
    Result := GetPropertiesForEdit;
end;

function TcxCustomGridTableItem.GetFilterCaption: string;
begin
  Result := GetAlternateCaption;
end;

function TcxCustomGridTableItem.GetFiltered: Boolean;
begin
  Result := FDataBinding.Filtered;
end;

function TcxCustomGridTableItem.GetFocused: Boolean;
begin
  Result := Controller.FocusedItem = Self;
end;

function TcxCustomGridTableItem.GetGroupIndex: Integer;
begin
  Result := DataController.Groups.ItemGroupIndex[Index];
end;

function TcxCustomGridTableItem.GetHeaderAlignmentHorz: TAlignment;
begin
  if FIsHeaderAlignmentHorzAssigned then
    Result := FHeaderAlignmentHorz
  else
    Result := DefaultHeaderAlignmentHorz;
end;

function TcxCustomGridTableItem.GetHeaderAlignmentVert: TcxAlignmentVert;
begin
  if FIsHeaderAlignmentVertAssigned then
    Result := FHeaderAlignmentVert
  else
    Result := DefaultHeaderAlignmentVert;
end;

function TcxCustomGridTableItem.GetHidden: Boolean;
begin
  Result := not VisibleForCustomization;
end;

function TcxCustomGridTableItem.GetIsLoading: Boolean;
begin
  Result := not FIgnoreLoadingStatus and
    ((csLoading in ComponentState) or FGridView.IsLoading);
end;

function TcxCustomGridTableItem.GetIncSearching: Boolean;
begin
  Result := Controller.IncSearchingItem = Self;
end;

function TcxCustomGridTableItem.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxCustomGridTableItem.GetIsFirst: Boolean;
begin
  Result := VisibleIndex = 0;
end;

function TcxCustomGridTableItem.GetIsLast: Boolean;
begin
  Result := VisibleIndex = FGridView.VisibleItemCount - 1;
end;

function TcxCustomGridTableItem.GetIsReading: Boolean;
begin
  Result := csReading in ComponentState;
end;

function TcxCustomGridTableItem.GetIsUpdating: Boolean;
begin
  Result := csUpdating in ComponentState;
end;

function TcxCustomGridTableItem.GetMinWidth: Integer;
begin
  if HasFixedWidth then
    Result := Width
  else
    Result := FMinWidth;
end;

function TcxCustomGridTableItem.GetPropertiesClassName: string;
begin
  if FProperties = nil then
    Result := ''
  else
    Result := FProperties.ClassName;
end;

function TcxCustomGridTableItem.GetSortIndex: Integer;
begin
  Result := DataController.GetItemSortingIndex(Index);
end;

function TcxCustomGridTableItem.GetSortOrder: TcxGridSortOrder;
begin
  Result := DataController.GetItemSortOrder(Index);
end;

function TcxCustomGridTableItem.GetTag: TcxTag;
begin
  Result := inherited Tag;
end;

function TcxCustomGridTableItem.GetViewData: TcxCustomGridTableViewData;
begin
  Result := FGridView.ViewData;
end;

function TcxCustomGridTableItem.GetWidth: Integer;
begin
  if FIsWidthAssigned then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

procedure TcxCustomGridTableItem.SetAlternateCaption(const Value: string);
begin
  if FAlternateCaption <> Value then
  begin
    FAlternateCaption := Value;
    CaptionChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetBestFitMaxWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBestFitMaxWidth <> Value then
  begin
    FBestFitMaxWidth := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    FIsCaptionAssigned := True;
    CaptionChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetDataBinding(Value: TcxGridItemDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxCustomGridTableItem.SetDateTimeGrouping(Value: TcxGridDateTimeGrouping);
begin
  if FDateTimeGrouping <> Value then
  begin
    FDateTimeGrouping := Value;
    DateTimeGroupingChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetEditing(Value: Boolean);
begin
  if Value then
    Controller.EditingItem := Self
  else
    if Editing then
      Controller.EditingItem := nil;
end;

procedure TcxCustomGridTableItem.SetFiltered(Value: Boolean);
begin
  FDataBinding.Filtered := Value;
end;

procedure TcxCustomGridTableItem.SetFocused(Value: Boolean);
begin
  if Value then
    Controller.FocusedItem := Self
  else
    if Focused then
      if not Controller.FocusNextItem(VisibleIndex, True, True, False, not Controller.IsMultiSelectPersistent) then
        Controller.FocusedItem := nil;
end;

procedure TcxCustomGridTableItem.SetGroupIndex(Value: Integer);
begin
  if FGridView.IsAssigningItems and (Value <> -1) then
    FGridView.AssigningGroupedItems[Value] := Self
  else
    ChangeGroupIndex(Value);
end;

procedure TcxCustomGridTableItem.SetHeaderAlignmentHorz(Value: TAlignment);
begin
  if HeaderAlignmentHorz <> Value then
  begin
    FHeaderAlignmentHorz := Value;
    FIsHeaderAlignmentHorzAssigned := True;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridTableItem.SetHeaderAlignmentVert(Value: TcxAlignmentVert);
begin
  if HeaderAlignmentVert <> Value then
  begin
    FHeaderAlignmentVert := Value;
    FIsHeaderAlignmentVertAssigned := True;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridTableItem.SetHeaderHint(const Value: string);
begin
  if FHeaderHint <> Value then
  begin
    FHeaderHint := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetHidden(Value: Boolean);
begin
  VisibleForCustomization := not Value;
end;

procedure TcxCustomGridTableItem.SetIndex(Value: Integer);
begin
  if FGridView.IsRestoring then
    FGridView.RestoringItems[Value] := Self
  else
    FGridView.ChangeItemIndex(Self, Value);
end;

procedure TcxCustomGridTableItem.SetMinWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    if IsLoading then Exit;
    if Width < FMinWidth then
      Width := FMinWidth
    else
      Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItem.SetOnCustomDrawCell(Value: TcxGridTableDataCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawCell, Value) then
  begin
    FOnCustomDrawCell := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetCellHint(Value: TcxGridGetCellHintEvent);
begin
  if not dxSameMethods(FOnGetCellHint, Value) then
  begin
    FOnGetCellHint := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetDataText(Value: TcxGridGetDataTextEvent);
begin
  if not dxSameMethods(FOnGetDataText, Value) then
  begin
    FOnGetDataText := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetDisplayText(Value: TcxGridGetDisplayTextEvent);
begin
  if not dxSameMethods(FOnGetDisplayText, Value) then
  begin
    FOnGetDisplayText := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetFilterDisplayText(Value: TcxGridGetFilterDisplayTextEvent);
begin
  if not dxSameMethods(FOnGetFilterDisplayText, Value) then
  begin
    FOnGetFilterDisplayText := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetFilterValues(Value: TcxGridGetFilterValuesEvent);
begin
  if not dxSameMethods(FOnGetFilterValues, Value) then
  begin
    FOnGetFilterValues := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetProperties(Value: TcxGridGetPropertiesEvent);
begin
  if not dxSameMethods(FOnGetProperties, Value) then
  begin
    FOnGetProperties := Value;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetPropertiesForEdit(Value: TcxGridGetPropertiesEvent);
begin
  if not dxSameMethods(FOnGetPropertiesForEdit, Value) then
  begin
    FOnGetPropertiesForEdit := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetStoredProperties(Value: TcxGridTableItemGetStoredPropertiesEvent);
begin
  if not dxSameMethods(FOnGetStoredProperties, Value) then
  begin
    FOnGetStoredProperties := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnGetStoredPropertyValue(Value: TcxGridTableItemGetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnGetStoredPropertyValue, Value) then
  begin
    FOnGetStoredPropertyValue := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnInitFilteringDateRanges(Value: TcxGridInitDateRangesEvent);
begin
  if not dxSameMethods(FOnInitFilteringDateRanges, Value) then
  begin
    FOnInitFilteringDateRanges := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnInitGroupingDateRanges(Value: TcxGridInitDateRangesEvent);
begin
  if not dxSameMethods(FOnInitGroupingDateRanges, Value) then
  begin
    FOnInitGroupingDateRanges := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnSetStoredPropertyValue(Value: TcxGridTableItemSetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnSetStoredPropertyValue, Value) then
  begin
    FOnSetStoredPropertyValue := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnUserFiltering(Value: TcxGridUserFilteringEvent);
begin
  if not dxSameMethods(FOnUserFiltering, Value) then
  begin
    FOnUserFiltering := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnUserFilteringEx(Value: TcxGridUserFilteringExEvent);
begin
  if not dxSameMethods(FOnUserFilteringEx, Value) then
  begin
    FOnUserFilteringEx := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOnValidateDrawValue(Value: TcxGridValidateDrawValueEvent);
begin
  if not dxSameMethods(FOnValidateDrawValue, Value) then
  begin
    FOnValidateDrawValue := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetOptions(Value: TcxCustomGridTableItemOptions);
begin
  FOptions.Assign(Value);
end;

procedure TcxCustomGridTableItem.SetProperties(Value: TcxCustomEditProperties);
begin
  if (FProperties <> nil) and (Value <> nil) then FProperties.Assign(Value);
end;

procedure TcxCustomGridTableItem.SetPropertiesClass(Value: TcxCustomEditPropertiesClass);
begin
  if FPropertiesClass <> Value then
  begin
    if FProperties <> nil then
      Controller.EditingController.RemoveEdit(FProperties);
    FPropertiesClass := Value;
    RecreateProperties;
    PropertiesValueChanged;
    PropertiesChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetPropertiesClassName(const Value: string);
begin
  PropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByClassName(Value));
end;

procedure TcxCustomGridTableItem.SetRepositoryItem(Value: TcxEditRepositoryItem);
begin
  if FRepositoryItem <> Value then
  begin
    if FRepositoryItem <> nil then
    begin
      FRepositoryItem.RemoveListener(Self);
      Controller.EditingController.RemoveEdit(FRepositoryItem.Properties);
    end;
    FRepositoryItem := Value;
    if FRepositoryItem <> nil then
      FRepositoryItem.AddListener(Self);
    PropertiesValueChanged;
    PropertiesChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetSortIndex(Value: Integer);
begin
  if FGridView.IsAssigningItems and (Value <> -1) then
    FGridView.AssigningSortedItems[Value] := Self
  else
    ChangeSortIndex(Value);
end;

procedure TcxCustomGridTableItem.SetSortOrder(Value: TcxGridSortOrder);
begin
  if SortOrder <> Value then
  begin
    GridView.BeginSortingUpdate;
    try
      DataController.ChangeSorting(Index, Value);
    finally
      GridView.EndSortingUpdate;
    end;
  end;
end;

procedure TcxCustomGridTableItem.SetStyles(Value: TcxCustomGridTableItemStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxCustomGridTableItem.SetTag(Value: TcxTag);
begin
  if Tag <> Value then
  begin
    inherited Tag := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridTableItem.SetVisible(Value: Boolean);
begin
  if Visible <> Value then
  begin
    GridView.SaveItemVisibles;
    DoSetVisible(Value);
    VisibleChanged;
    GridView.CheckItemVisibles;
    GridView.RefreshCustomizationForm;
    CheckUsingInFindFiltering;
  end;
end;

procedure TcxCustomGridTableItem.SetVisibleForCustomization(Value: Boolean);
begin
  if VisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    VisibleForCustomizationChanged;
  end;
end;

procedure TcxCustomGridTableItem.SetWidth(Value: Integer);
begin
  CheckWidthValue(Value);
  if IsLoading or (Width <> Value) then
  begin
    FWidth := Value;
    FIsWidthAssigned := True;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridTableItem.ReadHidden(Reader: TReader);
begin
  Hidden := Reader.ReadBoolean;
end;

procedure TcxCustomGridTableItem.ReadIsCaptionAssigned(Reader: TReader);
begin
  FIsCaptionAssigned := Reader.ReadBoolean;
end;

procedure TcxCustomGridTableItem.WriteIsCaptionAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(IsCaptionAssigned);
end;

function TcxCustomGridTableItem.IsCaptionStored: Boolean;
begin
  Result := IsCaptionAssigned and (FCaption <> DefaultCaption);
end;

function TcxCustomGridTableItem.IsSortOrderStored: Boolean;
begin
  Result := (SortOrder <> soNone) and (SortIndex <> -1);
end;

function TcxCustomGridTableItem.IsTagStored: Boolean;
begin
  Result := Tag <> 0;
end;

function TcxCustomGridTableItem.IsWidthStored: Boolean;
begin
  Result := FIsWidthAssigned //and (FWidth <> DefaultWidth);
end;

function TcxCustomGridTableItem.GetDataBindingClass: TcxGridItemDataBindingClass;
begin
  Result := FGridView.GetItemDataBindingClass;
end;

procedure TcxCustomGridTableItem.CreateProperties;
begin
  if FPropertiesClass <> nil then
  begin
    FProperties := FPropertiesClass.Create(Self);
    FProperties.OnPropertiesChanged := PropertiesChangedHandler;
  end;
end;

procedure TcxCustomGridTableItem.DestroyProperties;
begin
  FreeAndNil(FProperties);
end;

procedure TcxCustomGridTableItem.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TcxCustomGridTableItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Hidden', ReadHidden, nil, True);
  Filer.DefineProperty('IsCaptionAssigned', ReadIsCaptionAssigned, WriteIsCaptionAssigned,
    IsCaptionAssigned and (Caption = ''));
end;

procedure TcxCustomGridTableItem.SetParentComponent(AParent: TComponent);
begin
  if AParent is TcxCustomGridTableView then
    TcxCustomGridTableView(AParent).AddItem(Self);
end;

procedure TcxCustomGridTableItem.ItemRemoved(Sender: TcxEditRepositoryItem);
begin
  if Sender = FLastUsedDefaultRepositoryItem then
    PropertiesValueChanged
  else
    RepositoryItem := nil;
end;

procedure TcxCustomGridTableItem.RepositoryItemPropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  PropertiesChanged;
end;

function TcxCustomGridTableItem.GetObjectName: string;
begin
  if GridView.IsStoringNameMode then
    Result := IntToStr(ID)
  else
    Result := Name;
end;

function TcxCustomGridTableItem.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    if GetProperties <> nil then
      Add('AlignmentHorz');
    Add('Index');
    Add('Visible');
    Add('SortOrder');
    Add('SortIndex');
    Add('WasVisibleBeforeGrouping');
  end;
  if Assigned(FOnGetStoredProperties) then
    FOnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TcxCustomGridTableItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'AlignmentHorz' then
    AValue := Variant(GetProperties.Alignment.Horz)
  else
    if AName = 'Index' then
      AValue := Index
    else
      if AName = 'Visible' then
        AValue := Visible
      else
        if AName = 'SortOrder' then
          AValue := Variant(SortOrder)
        else
          if AName = 'SortIndex' then
            AValue := SortIndex
          else
            if AName = 'WasVisibleBeforeGrouping' then
              AValue := WasVisibleBeforeGrouping
            else
              if Assigned(FOnGetStoredPropertyValue) then
                FOnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxCustomGridTableItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'AlignmentHorz' then
    if GetProperties.Alignment.Horz <> TAlignment((AValue)) then
    begin
      if (FRepositoryItem = nil) and (FProperties = nil) then
        PropertiesClass := TcxCustomEditPropertiesClass(GetProperties.ClassType);
      GetProperties.Alignment.Horz := TAlignment((AValue));
    end
    else
  else
    if AName = 'Index' then
      Index := AValue
    else
      if AName = 'Visible' then
        Visible := AValue
      else
        if AName = 'SortOrder' then
          SortOrder := TcxDataSortOrder((AValue))
        else
          if AName = 'SortIndex' then
            SortIndex := AValue
          else
            if AName = 'WasVisibleBeforeGrouping' then
              FWasVisibleBeforeGrouping := AValue
            else
              if Assigned(FOnSetStoredPropertyValue) then
                FOnSetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxCustomGridTableItem.ChangeScale(M, D: Integer);
begin
  BestFitMaxWidth := MulDiv(BestFitMaxWidth, M, D);
  if not HasFixedWidth then
    MinWidth := MulDiv(MinWidth, M, D);
  if FIsWidthAssigned then
    Width := MulDiv(Width, M, D);
end;

function TcxCustomGridTableItem.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactor(GridView);
end;

procedure TcxCustomGridTableItem.FilteringApplied;
begin
  GridView.Filtering.AddFilterToMRUItems;
end;

function TcxCustomGridTableItem.GetFilter: TcxFilterCriteria;
begin
  Result := DataBinding.Filter;
end;

procedure TcxCustomGridTableItem.GetFilterActiveValueIndexes(AValues: TcxFilterValueList; var AIndexes: TdxIntegerIndexes);
var
  AValueList: TcxGridFilterValueList absolute AValues;
begin
  DataBinding.GetFilterActiveValueIndexes(AValueList, AIndexes);
end;

function TcxCustomGridTableItem.GetFilterSelectedValueIndex(AValues: TcxFilterValueList): Integer;
begin
  Result := AValues.GetIndexByCriteriaItem(DataBinding.FilterCriteriaItem)
end;

function TcxCustomGridTableItem.GetFilterValuesClass: TcxFilterValueListClass;
begin
  Result := ViewData.GetFilterValueListClass;
end;

procedure TcxCustomGridTableItem.PopulateFilterValues(AValues: TcxFilterValueList; ADisplayValues: TStrings;
  AValuesOnly: Boolean; AUniqueOnly: Boolean);
var
  AValueList: TcxGridFilterValueList absolute AValues;
begin
  DataBinding.GetFilterStrings(ADisplayValues, AValueList, AValuesOnly, AUniqueOnly);
end;

procedure TcxCustomGridTableItem.SetFilterActiveValueIndex(AValues: TcxFilterValueList; AIndex: Integer;
  AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);
var
  AValueList: TcxGridFilterValueList absolute AValues;
begin
  DataBinding.SetFilterActiveValueIndex(AValueList, AIndex, AFilterList, AReplaceExistent, AAddToMRUItemsList);
end;

procedure TcxCustomGridTableItem.SetFilterActiveValueIndexes(AValues: TcxFilterValueList; const AIndexes: TdxIntegerIndexes;
  AAddToMRUItemsList: Boolean = False);
var
  AValueList: TcxGridFilterValueList absolute AValues;
begin
  DataBinding.SetFilterActiveValueIndexes(AValueList, AIndexes, AAddToMRUItemsList);
end;

function TcxCustomGridTableItem.GetFilterItemLink: TObject;
begin
  Result := Self;
end;

function TcxCustomGridTableItem.GetFilterProperties: TcxCustomEditProperties;
begin
  Result := GetProperties;
end;

function TcxCustomGridTableItem.GetFilterValueTypeClass: TcxValueTypeClass;
begin
  Result := DataBinding.ValueTypeClass;
end;

procedure TcxCustomGridTableItem.SetFilterActiveValue(AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
  const ADisplayText: string);
begin
  DataBinding.SetFilterActiveValue(AOperatorKind, AValue, ADisplayText);
end;

procedure TcxCustomGridTableItem.SetFilterActiveValues(AValueInfos: TdxFastObjectList;
  ABoolOperator: TcxFilterBoolOperatorKind);
begin
  DataBinding.SetFilterActiveValues(AValueInfos, ABoolOperator);
end;

function TcxCustomGridTableItem.GetDateTimeHandlingDateFormat: string;
begin
  Result := GridView.DateTimeHandling.GetDateFormat;
end;

function TcxCustomGridTableItem.GetDateTimeHandlingHourFormat: string;
begin
  Result := GridView.DateTimeHandling.GetHourFormat;
end;

function TcxCustomGridTableItem.GetDateTimeHandlingMonthFormat: string;
begin
  Result := GridView.DateTimeHandling.GetMonthFormat;
end;

function TcxCustomGridTableItem.GetDateTimeHandlingYearFormat: string;
begin
  Result := GridView.DateTimeHandling.GetYearFormat;
end;

function TcxCustomGridTableItem.GetDateTimeHandlingFilters: TdxDateTimeFilters;
begin
  Result := GridView.DateTimeHandling.Filters;
end;

function TcxCustomGridTableItem.GetDateTimeHandlingGrouping: TdxDateTimeGrouping;
begin
  Result := FDateTimeGrouping;
  if Result = dtgDefault then
    Result := GridView.DateTimeHandling.Grouping;
end;

procedure TcxCustomGridTableItem.CreateDataBinding;
begin
  FDataBinding := GetDataBindingClass.Create(Self);
  FDataBinding.ValueTypeClass := FDataBinding.GetDefaultValueTypeClass;
end;

procedure TcxCustomGridTableItem.DestroyDataBinding;
begin
  FreeAndNil(FDataBinding);
end;

procedure TcxCustomGridTableItem.CreateSubClasses;
begin
  FFilteringDateRanges := GetFilteringDateRangesClass.Create;
  FGroupingDateRanges := GetGroupingDateRangesClass.Create;
  FOptions := GetOptionsClass.Create(Self);
  FStyles := GetStylesClass.Create(Self);
end;

procedure TcxCustomGridTableItem.DestroySubClasses;
begin
  DestroyProperties;
  FreeAndNil(FStyles);
  FreeAndNil(FOptions);
  FreeAndNil(FGroupingDateRanges);
  FreeAndNil(FFilteringDateRanges);
end;

function TcxCustomGridTableItem.GetFilteringDateRangesClass: TcxGridFilteringDateRangesClass;
begin
  Result := TcxGridFilteringDateRanges;
end;

function TcxCustomGridTableItem.GetGroupingDateRangesClass: TcxGridGroupingDateRangesClass;
begin
  Result := TcxGridGroupingDateRanges;
end;

function TcxCustomGridTableItem.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxCustomGridTableItemOptions;
end;

function TcxCustomGridTableItem.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxCustomGridTableItemStyles;
end;

function TcxCustomGridTableItem.IsHeaderAlignmentHorzStored: Boolean;
begin
  Result := FIsHeaderAlignmentHorzAssigned and
    (FHeaderAlignmentHorz <> DefaultHeaderAlignmentHorz);
end;

function TcxCustomGridTableItem.IsHeaderAlignmentVertStored: Boolean;
begin
  Result := FIsHeaderAlignmentVertAssigned and
    (FHeaderAlignmentVert <> DefaultHeaderAlignmentVert);
end;

procedure TcxCustomGridTableItem.BestFitApplied(AFireEvents: Boolean);
begin
  Controller.DesignerModified;
end;

function TcxCustomGridTableItem.CalculateBestFitWidth: Integer;
var
  ACanvas: TcxCanvas;
  AIsCalcByValue: Boolean;
  AEditSizeProperties: TcxEditSizeProperties;
  AParams: TcxViewParams;
  AEditViewData: TcxCustomEditViewData;
  I, AWidth: Integer;
  ARecord: TcxCustomGridRecord;
  AValue: Variant;
  AEditMinContentSize: TSize;
  AFirstIndex, ALastIndex: Integer;
  AErrorData: TcxEditValidateInfo;
begin
  Result := 0;
  ACanvas := FGridView.Painter.Canvas;
  AIsCalcByValue := GetProperties.GetEditValueSource(False) = evsValue;
  with AEditSizeProperties do
  begin
    Height := -1;
    MaxLineCount := 0;
    Width := -1;
  end;
  AEditViewData := CreateEditViewData(GetProperties);
  try
    if GridView.ViewInfo.RecordsViewInfo.AutoDataCellHeight or
      GridView.ViewInfo.RecordsViewInfo.IsCellMultiLine(Self) then
    begin
      Include(AEditViewData.PaintOptions, epoAutoHeight);
      AEditViewData.MaxLineCount := GridView.OptionsView.CellTextMaxLineCount;
      AEditSizeProperties.MaxLineCount := AEditViewData.MaxLineCount;
    end;
    AEditViewData.InplaceEditParams.Position.Item := Self;

    AErrorData := TcxEditValidateInfo.Create;
    try
      GetBestFitRecordRange(AFirstIndex, ALastIndex);
      for I := AFirstIndex to ALastIndex do
      begin
        ARecord := ViewData.Records[I];
        if ARecord.HasCells then
        begin
          FStyles.GetContentParams(ARecord, AParams);
          InitStyle(AEditViewData.Style, AParams, True);
          if AIsCalcByValue then
            AValue := ARecord.Values[FIndex]
          else
            AValue := ARecord.DisplayTexts[FIndex];
          AEditViewData.InplaceEditParams.Position.RecordIndex := ARecord.RecordIndex;
          AEditViewData.Data := ARecord;
          TcxEditValidateInfoAccess(AErrorData).Reset;
          if Assigned(OnValidateDrawValue) then
            OnValidateDrawValue(Self, ARecord, AValue, AErrorData);

          AWidth := AEditViewData.GetEditContentSize(ACanvas, AValue, AEditSizeProperties, AErrorData).cx;
          if GridView.HasConditionalFormattingProvider then
            AWidth := GridView.ConditionalFormattingProvider.CalculateBestFitWidth(TPoint.Create(GridView.ConditionalFormattingProvider.FindFieldByItem(Self), ARecord.Index), AWidth);
          if AWidth > Result then Result := AWidth;
        end;
      end;
    finally
      FreeAndNil(AErrorData);
    end;

    FStyles.GetContentParams(nil, AParams);
    InitStyle(AEditViewData.Style, AParams, True);
    AWidth := AEditViewData.GetEditConstantPartSize(ACanvas, AEditSizeProperties, AEditMinContentSize).cx;
    if Result < AEditMinContentSize.cx then
      Result := AEditMinContentSize.cx;
    Inc(Result, AWidth);
  finally
    DestroyEditViewData(AEditViewData);
  end;
  if Result <> 0 then
    Inc(Result, 2 * cxGridEditOffset);
end;

function TcxCustomGridTableItem.CanAutoHeight: Boolean;
begin
  Result := esoAutoHeight in GetProperties.GetSupportedOperations;
end;

function TcxCustomGridTableItem.CanEditAutoHeight: Boolean;
begin
  Result := (FOptions.EditAutoHeight <> ieahNone) and
    (GridView.GetEditAutoHeight <> eahNone) and
    (esoEditingAutoHeight in GetProperties.GetSupportedOperations);
end;

function TcxCustomGridTableItem.CanEdit: Boolean;
begin
  Result := CanFocus(Controller.FocusedRecord) and Editable and
    (FocusedCellViewInfo <> nil) and FocusedCellViewInfo.SupportsEditing and (dceoShowEdit in DataController.EditOperations);
end;

function TcxCustomGridTableItem.CanFilter(AVisually: Boolean): Boolean;
begin
  Result :=
    (esoFiltering in GetProperties.GetSupportedOperations) and FOptions.Filtering and
    (not AVisually or GridView.OptionsCustomize.ItemFiltering and FOptions.FilteringPopup);
end;

function TcxCustomGridTableItem.CanFilterUsingChecks: Boolean;
begin
  Result := GridView.Filtering.ItemPopup.MultiSelect and FOptions.FilteringPopupMultiSelect;
end;

function TcxCustomGridTableItem.CanFilterMRUValueItems: Boolean;
begin
  Result := GridView.Filtering.ItemMRUItemsList and FOptions.FilteringMRUItemsList and
    not CanFilterUsingChecks;
end;

function TcxCustomGridTableItem.CanSearch: Boolean;
begin
  Result := Options.FilteringWithFindPanel and Visible;
end;

function TcxCustomGridTableItem.CanFocus(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := (ActuallyVisible or CanFocusInplaceEditFormItem(ARecord)) and FOptions.Focusing and
    ((ARecord = nil) and GridView.OptionsSelection.CellSelect or
     (ARecord <> nil) and ARecord.CanFocusCells);
end;

function TcxCustomGridTableItem.CanFocusInplaceEditFormItem(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableItem.CanGroup: Boolean;
begin
  Result := (esoSorting in GetProperties.GetSupportedOperations) and
    GridView.OptionsCustomize.ItemGrouping and FOptions.Grouping;
end;

function TcxCustomGridTableItem.CanHide: Boolean;
begin
  Result := not (IsFirst and IsLast) and CanMove;
end;

function TcxCustomGridTableItem.CanHorzSize: Boolean;
begin
  Result := not HasFixedWidth;
end;

function TcxCustomGridTableItem.CanIgnoreTimeForFiltering: Boolean;
begin
  Result := IsDateTimeValueTypeClass(FDataBinding.ValueTypeClass) and
    GridView.DateTimeHandling.IgnoreTimeForFiltering and FOptions.IgnoreTimeForFiltering;
end;

function TcxCustomGridTableItem.CanIncSearch: Boolean;
begin
  Result := (esoIncSearch in GetProperties.GetSupportedOperations) and
    GridView.OptionsBehavior.IncSearch and FOptions.IncSearch;
end;

function TcxCustomGridTableItem.CanInitEditing: Boolean;
begin
  Result := DataController.CanInitEditing(Index);
end;

function TcxCustomGridTableItem.CanMove: Boolean;
begin
  Result := IsDesigning or GridView.OptionsCustomize.ItemMoving and Options.Moving;
end;

function TcxCustomGridTableItem.CanScroll: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableItem.CanSort: Boolean;
begin
  Result := (esoSorting in GetProperties.GetSupportedOperations) and
    GridView.OptionsCustomize.ItemSorting and Options.Sorting;
end;

procedure TcxCustomGridTableItem.CaptionChanged;
begin
  GridView.ItemCaptionChanged(Self);
end;

procedure TcxCustomGridTableItem.Changed(AChange: TcxGridTableItemChange);
begin
  if GridView <> nil then
    GridView.Changed(TcxGridViewChangeKind(AChange));
end;

procedure TcxCustomGridTableItem.ChangeGroupIndex(Value: Integer);
begin
  if GroupIndex <> Value then
    ChangeGrouping(Value);
end;

procedure TcxCustomGridTableItem.ChangeGrouping(AGroupIndex: Integer; AMergeWithLeftColumn: Boolean = False;
  AMergeWithRightColumn: Boolean = False);
begin
  GridView.BeginGroupingUpdate;
  try
    if GroupIndex = -1 then
      if IsLoading then
        FWasVisibleBeforeGrouping := True
      else
        if not GridView.IsAfterAssigningItems then
          FWasVisibleBeforeGrouping := Visible;
    DataController.Groups.ChangeGrouping(Index, AGroupIndex, AMergeWithLeftColumn, AMergeWithRightColumn);
  finally
    GridView.EndGroupingUpdate;
  end;
end;

procedure TcxCustomGridTableItem.ChangeSortIndex(Value: Integer);
begin
  if SortIndex <> Value then
  begin
    GridView.BeginSortingUpdate;
    try
      DataController.ChangeItemSortingIndex(Index, Value);
    finally
      GridView.EndSortingUpdate;
    end;
  end;
end;

procedure TcxCustomGridTableItem.CheckUsingInFindFiltering;
begin
  if IsDestroying then
    Exit;
  if CanSearch then
    DataController.AddFindFilterItem(Index)
  else
    DataController.RemoveFindFilterItem(Index);
end;

procedure TcxCustomGridTableItem.CheckWidthValue(var Value: Integer);
begin
  if Value < ActualMinWidth then Value := ActualMinWidth;
end;

procedure TcxCustomGridTableItem.DataChanged;
begin
  FDataBinding.Init;
end;

procedure TcxCustomGridTableItem.DateTimeGroupingChanged;
begin
  InitGroupingDateRanges;
  DataController.Refresh;
end;

procedure TcxCustomGridTableItem.DoSetVisible(Value: Boolean);
begin
  FVisible := Value;
end;

procedure TcxCustomGridTableItem.ForceWidth(Value: Integer);
begin
  Controller.ForcingWidthItem := Self;
  try
    Width := Value;
  finally
    Controller.ForcingWidthItem := nil;
  end;
end;

function TcxCustomGridTableItem.GetActuallyVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomGridTableItem.GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer);
begin
  Controller.GetBestFitRecordRange(AFirstIndex, ALastIndex);
end;

function TcxCustomGridTableItem.GetBestFitWidth: Integer;
begin
  Result := CalculateBestFitWidth;
  if (FBestFitMaxWidth <> 0) and (Result > FBestFitMaxWidth) then
    Result := FBestFitMaxWidth;
end;

function TcxCustomGridTableItem.GetEditable: Boolean;
begin
  Result := GridView.OptionsData.Editing and FOptions.Editing;
end;

function TcxCustomGridTableItem.GetEditAutoHeight: TcxInplaceEditAutoHeight;
begin
  if CanEditAutoHeight then
    Result := GridView.GetEditAutoHeight
  else
    Result := eahNone;
end;

function TcxCustomGridTableItem.GetEditPartVisible: Boolean;
begin
  if CanDataCellScroll then
    Result := IsFocusedCellViewInfoPartVisible
  else
    Result := False;
end;

function TcxCustomGridTableItem.GetEditValue: Variant;
begin
  if Controller.FocusedRecord <> nil then
    Result := DataController.GetEditValue(Index, EditingProperties.GetEditValueSource(True))
  else
    Result := Unassigned;
end;

function TcxCustomGridTableItem.GetFilterable: Boolean;
begin
  Result := CanFilter(False);
end;

procedure TcxCustomGridTableItem.GetFilterDisplayText(const AValue: Variant;
  var ADisplayText: string);
begin
  if Assigned(FOnGetFilterDisplayText) then
    FOnGetFilterDisplayText(Self, AValue, ADisplayText);
end;

function TcxCustomGridTableItem.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := Options.FilteringPopupMode;
  if Result = fpmDefault then
    Result := GridView.Filtering.ItemPopupMode;
end;

function TcxCustomGridTableItem.GetFixed: Boolean;
begin
  Result := (Controller.ForcingWidthItem = Self) or HasFixedWidth;
end;

function TcxCustomGridTableItem.GetFocusedCellViewInfo: TcxGridTableDataCellViewInfo;
begin
  with Controller do
    if FocusedRecordHasCells(False) and (FocusedRecord.ViewInfo <> nil) and
      FocusedRecord.ViewInfo.Calculated then
      Result := FocusedRecord.ViewInfo.GetCellViewInfoByItem(Self)
    else
      Result := nil;
end;

function TcxCustomGridTableItem.GetPropertiesForEdit: TcxCustomEditProperties;
begin
  Result := GetProperties(Controller.FocusedRecord);
  DoGetPropertiesForEdit(Controller.FocusedRecord, Result);
  InitProperties(Result);
end;

function TcxCustomGridTableItem.GetPropertiesValue: TcxCustomEditProperties;
var
  ARepositoryItem: TcxEditRepositoryItem;
begin
  if FLastUsedDefaultRepositoryItem <> nil then
  begin
    FLastUsedDefaultRepositoryItem.RemoveListener(Self);
    FLastUsedDefaultRepositoryItem := nil;
  end;
  if FGridView = nil then  // because of EditViewData - it needs Style which needs GridView
    Result := nil
  else
    if UseOwnProperties then
      Result := FProperties
    else
    begin
      ARepositoryItem := GetRepositoryItem;
      if ARepositoryItem = nil then
        Result := nil
      else
        Result := ARepositoryItem.Properties;
    end;
end;

function TcxCustomGridTableItem.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TcxCustomGridTableItem.GetVisibleCaption: string;
begin
  Result := Caption;
end;

function TcxCustomGridTableItem.GetVisibleForCustomization: Boolean;
begin
  Result := FVisibleForCustomization;
end;

function TcxCustomGridTableItem.GetVisibleIndex: Integer;
begin
  Result := FGridView.FVisibleItems.IndexOf(Self);
end;

function TcxCustomGridTableItem.GetVisibleInQuickCustomizationPopup: Boolean;
begin
  Result := VisibleForCustomization;
end;

procedure TcxCustomGridTableItem.GroupingChanging;
begin
  if SupportsGroupingDateRanges(True) then
    InitGroupingDateRanges;
end;

function TcxCustomGridTableItem.HasCustomDrawCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawCell);
end;

function TcxCustomGridTableItem.HasFixedWidth: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridTableItem.InitFilteringDateRanges;
begin
  FilteringDateRanges.Init(Self);
  DoInitFilteringDateRanges;
end;

procedure TcxCustomGridTableItem.InitGroupingDateRanges;
begin
  GroupingDateRanges.Init(Self);
  DoInitGroupingDateRanges;
end;

procedure TcxCustomGridTableItem.InitProperties(AProperties: TcxCustomEditProperties);
begin
  if AProperties <> nil then
  begin
    AProperties.LockUpdate(True);
    AProperties.IDefaultValuesProvider := GetDefaultValuesProvider(AProperties);
    AProperties.LockUpdate(False);
  end;
end;

function TcxCustomGridTableItem.IsFocusedCellViewInfoPartVisible: Boolean;
var
  AScrollableAreaBounds, AEditBounds: TRect;
begin
  if FocusedCellViewInfo = nil then
    Exit(False);
  AScrollableAreaBounds := GridView.ViewInfo.ScrollableAreaBoundsForEdit;
  AEditBounds := FocusedCellViewInfo.EditBounds;
  Result := not cxRectContain(AScrollableAreaBounds, AEditBounds);
end;

function TcxCustomGridTableItem.IsSortingByDisplayText(ASortByDisplayText: Boolean): Boolean;
begin
  if FOptions.SortByDisplayText = isbtDefault then
    Result := HasDataTextHandler or ASortByDisplayText and
      (esoSortingByDisplayText in GetProperties.GetSupportedOperations)
  else
    Result := FOptions.SortByDisplayText = isbtOn;
end;

function TcxCustomGridTableItem.IsVisibleForRecordChange: Boolean;
begin
  Result := ActuallyVisible;
end;

function TcxCustomGridTableItem.IsVisibleStored: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableItem.IsVisibleForCustomizationStored: Boolean;
begin
  Result := not VisibleForCustomization;
end;

function TcxCustomGridTableItem.CanDataCellScroll: Boolean;
begin
  Result := CanScroll;
end;

procedure TcxCustomGridTableItem.PropertiesChanged;
begin
  if GridView.PatternGridView.IgnorePropertiesChanges then Exit;
  if not IsDestroying then
    GridView.RefreshFilterableItemsList;
  if FEditData <> nil then FEditData.Clear;
  if not IsDestroying and not DataController.ItemPropertiesChanged(Index) then
    Changed(ticSize);
end;

procedure TcxCustomGridTableItem.PropertiesChangedHandler(Sender: TObject);
begin
  PropertiesChanged;
end;

procedure TcxCustomGridTableItem.PropertiesValueChanged;
var
  APrevPropertiesValue: TcxCustomEditProperties;
begin
  APrevPropertiesValue := FPropertiesValue;
  FPropertiesValue := GetPropertiesValue;
  if not IsDestroying and (FPropertiesValue <> nil) and (FPropertiesValue <> APrevPropertiesValue) then
    DataController.SortByDisplayTextChanged;  // for Options.SortByDisplayText = isbtDefault
end;

procedure TcxCustomGridTableItem.RecalculateDefaultWidth;
begin
  if not FIsWidthAssigned then
    FWidth := DefaultWidth;
end;

procedure TcxCustomGridTableItem.SetEditValue(const Value: Variant);
begin
  DataController.SetEditValue(Index, Value, EditingProperties.GetEditValueSource(True));
end;

procedure TcxCustomGridTableItem.SetGridView(Value: TcxCustomGridTableView);
begin
  FGridView := Value;
  if Value <> nil then
    CreateDataBinding
  else
    DestroyDataBinding;
  PropertiesValueChanged;
  if GridView <> nil then InitGroupingDateRanges;
end;

function TcxCustomGridTableItem.ShowButtons(AFocused: Boolean): Boolean;
var
  AGridShowEditButtons: TcxGridShowEditButtons;
begin
  AGridShowEditButtons := FGridView.OptionsView.ShowEditButtons;
  Result :=
    (FOptions.ShowEditButtons = isebAlways) or
    (FOptions.ShowEditButtons = isebDefault) and
    ((AGridShowEditButtons = gsebAlways) or
     (AGridShowEditButtons = gsebForFocusedRecord) and AFocused);
end;

function TcxCustomGridTableItem.ShowOnUngrouping: Boolean;
begin
  Result := True; //!!!
end;

function TcxCustomGridTableItem.SupportsGroupingDateRanges(ACheckCustomHandlers: Boolean): Boolean;
begin
  Result := IsDateTimeValueTypeClass(FDataBinding.ValueTypeClass) and
    (ACheckCustomHandlers and HasInitGroupingDateRangesHandlers or
     not ACheckCustomHandlers and not GroupingDateRanges.IsEmpty);
end;

function TcxCustomGridTableItem.UseFilteredValuesForFilterValueList: Boolean;
begin
  Result := GridView.Filtering.ItemFilteredItemsList and FOptions.FilteringFilteredItemsList;
end;

function TcxCustomGridTableItem.UseOwnProperties: Boolean;
begin
  Result := (FRepositoryItem = nil) and (FProperties <> nil);
end;

function TcxCustomGridTableItem.UseValueItemsForFilterValueList: Boolean;
begin
  Result := Options.FilteringAddValueItems and GridView.Filtering.ItemAddValueItems;
end;

procedure TcxCustomGridTableItem.ValidateEditData(AEditProperties: TcxCustomEditProperties);
begin
  if AEditProperties <> FLastEditingProperties then
  begin
    FLastEditingProperties := AEditProperties;
    FreeAndNil(FEditData);
  end;
end;

procedure TcxCustomGridTableItem.ValueTypeClassChanged;
begin
  FDataBinding.FilterMRUValueItems.ClearItems;
  FDataBinding.Init;
  PropertiesValueChanged;
  if FProperties <> nil then
    FProperties.Changed;
end;

procedure TcxCustomGridTableItem.VisibleChanged;
begin
end;

procedure TcxCustomGridTableItem.VisibleForCustomizationChanged;
begin
  with FGridView do
  begin
    //RefreshFilterableItemsList;
    RefreshCustomizationForm;
  end;
  Changed(ticProperty);
end;

function TcxCustomGridTableItem.DefaultAlternateCaption: string;
begin
  Result := Caption;
end;

function TcxCustomGridTableItem.DefaultCaption: string;
begin
  Result := FDataBinding.DefaultCaption;
end;

function TcxCustomGridTableItem.DefaultHeaderAlignmentHorz: TAlignment;
begin
  Result := taLeftJustify;
end;

function TcxCustomGridTableItem.DefaultHeaderAlignmentVert: TcxAlignmentVert;
begin
  Result := cxDefaultAlignmentVert;
end;

function TcxCustomGridTableItem.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  if FDataBinding = nil then
    Result := nil
  else
    Result := FDataBinding.DefaultRepositoryItem;
end;

function TcxCustomGridTableItem.DefaultWidth: Integer;
begin
  Result := FDataBinding.DefaultWidth;
end;

function TcxCustomGridTableItem.GetCellStyle: TcxEditStyle;
begin
  if FCellStyle = nil then
    FCellStyle := CreateEditStyle;
  Result := FCellStyle;
  Inc(FCellStyleUseCounter);
end;

procedure TcxCustomGridTableItem.InitStyle(AStyle: TcxCustomEditStyle;
  const AParams: TcxViewParams; AFocused: Boolean);
begin
  with AParams do
  begin
    AStyle.Color := Color;
    AStyle.Font := Font;
    AStyle.StyleData.FontColor := TextColor;
  end;
  with AStyle do
  begin
    if Self.ShowButtons(AFocused) then
      ButtonTransparency := ebtNone
    else
      ButtonTransparency := ebtHideInactive;
    HotTrack := True;
  end;
end;

procedure TcxCustomGridTableItem.ReleaseCellStyle;
begin
  Dec(FCellStyleUseCounter);
  if FCellStyleUseCounter = 0 then FreeAndNil(FCellStyle);
end;

procedure TcxCustomGridTableItem.AddCell(ACell: TcxGridTableDataCellViewInfo);
begin
  FCells.Add(ACell);
end;

procedure TcxCustomGridTableItem.RemoveCell(ACell: TcxGridTableDataCellViewInfo);
begin
  FCells.Remove(ACell);
end;

function TcxCustomGridTableItem.CreateEditViewData(AProperties: TcxCustomEditProperties): TcxCustomEditViewData;
begin
  if AProperties <> nil then
  begin
    Result := AProperties.CreateViewData(GetCellStyle, True);
    Result.OnGetDisplayText := EditViewDataGetDisplayTextHandler;
    Result.ScaleFactor.Assign(GetScaleFactor);
  end
  else
    Result := nil;
end;

procedure TcxCustomGridTableItem.DestroyEditViewData(var AEditViewData: TcxCustomEditViewData);
begin
  if AEditViewData <> nil then
  begin
    FreeAndNil(AEditViewData);
    ReleaseCellStyle;
  end;
end;

procedure TcxCustomGridTableItem.DoCreateEditViewData;
begin
  FEditViewData := CreateEditViewData(GetProperties);
end;

procedure TcxCustomGridTableItem.DoDestroyEditViewData;
begin
  DestroyEditViewData(FEditViewData);
end;

procedure TcxCustomGridTableItem.EditViewDataGetDisplayTextHandler(Sender: TcxCustomEditViewData;
  var AText: string);
var
  AGridRecord: TcxCustomGridRecord;
begin
  if Sender.Data is TcxCustomGridRecord then
    AGridRecord := TcxCustomGridRecord(Sender.Data)
  else
    AGridRecord := nil;
  DoGetDisplayText(AGridRecord, AText);
end;

function TcxCustomGridTableItem.GetEditViewData(AProperties: TcxCustomEditProperties;
  out AIsLocalCopy: Boolean; ANeedLocalCopy: Boolean = False): TcxCustomEditViewData;
begin
  AIsLocalCopy := ANeedLocalCopy or HasCustomPropertiesHandler or (FEditViewData = nil);
  if AIsLocalCopy then
    Result := CreateEditViewData(AProperties)
  else
    Result := FEditViewData;
end;

procedure TcxCustomGridTableItem.ReleaseEditViewData(var AEditViewData: TcxCustomEditViewData;
  AIsLocalCopy: Boolean);
begin
  if AIsLocalCopy then
    DestroyEditViewData(AEditViewData)
  else
    AEditViewData := nil;
end;

procedure TcxCustomGridTableItem.CheckVisible;
begin
  if ActuallyVisible <> FSavedVisible then
    GridView.ItemVisibilityChanged(Self, ActuallyVisible);
end;

procedure TcxCustomGridTableItem.SaveVisible;
begin
  FSavedVisible := ActuallyVisible;
end;

procedure TcxCustomGridTableItem.DoCustomDrawCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawCell then
    FOnCustomDrawCell(GridView, ACanvas, AViewInfo, ADone);
end;

procedure TcxCustomGridTableItem.DoGetCellHint(ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
  var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
begin
  if HasCellHintHandler then
    FOnGetCellHint(Self, ARecord, ACellViewInfo, AMousePos, AHintText, AIsHintMultiLine, AHintTextRect);
end;

procedure TcxCustomGridTableItem.DoGetDataText(ARecordIndex: Integer;
  var AText: string);
begin
  if HasDataTextHandler then FOnGetDataText(Self, ARecordIndex, AText);
end;

procedure TcxCustomGridTableItem.DoGetDisplayText(ARecord: TcxCustomGridRecord;
  var AText: string);
begin
  if Assigned(FOnGetDisplayText) then FOnGetDisplayText(Self, ARecord, AText);
end;

procedure TcxCustomGridTableItem.DoGetFilterValues(AValueList: TcxDataFilterValueList);
begin
  if Assigned(FOnGetFilterValues) then FOnGetFilterValues(Self, AValueList);
end;

function TcxCustomGridTableItem.DoGetProperties(ARecord: TcxCustomGridRecord): TcxCustomEditProperties;
begin
  Result := FPropertiesValue;
  if HasCustomPropertiesHandler then FOnGetProperties(Self, ARecord, Result);
end;

procedure TcxCustomGridTableItem.DoGetPropertiesForEdit(ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
begin
  if HasCustomPropertiesForEditHandler then
    FOnGetPropertiesForEdit(Self, ARecord, AProperties);
end;

procedure TcxCustomGridTableItem.DoInitFilteringDateRanges;
begin
  GridView.DoInitFilteringDateRanges(Self);
  if HasInitFilteringDateRangesHandler then
    FOnInitFilteringDateRanges(Self, FilteringDateRanges);
end;

procedure TcxCustomGridTableItem.DoInitGroupingDateRanges;
begin
  GridView.DoInitGroupingDateRanges(Self);
  if HasInitGroupingDateRangesHandler then
    FOnInitGroupingDateRanges(Self, GroupingDateRanges);
end;

procedure TcxCustomGridTableItem.DoUserFiltering(const AValue: Variant; const ADisplayText: string);
begin
  if Assigned(FOnUserFiltering) then FOnUserFiltering(Self, AValue, ADisplayText);
end;

procedure TcxCustomGridTableItem.DoUserFilteringEx(AFilterList: TcxFilterCriteriaItemList;
  const AValue: Variant; const ADisplayText: string);
begin
  if Assigned(FOnUserFilteringEx) then FOnUserFilteringEx(Self, AFilterList, AValue, ADisplayText);
end;

procedure TcxCustomGridTableItem.DoValidateDrawValue(ARecord: TcxCustomGridRecord;
  const AValue: TcxEditValue; AData: TcxEditValidateInfo);
begin
  if Assigned(FOnValidateDrawValue) then
    FOnValidateDrawValue(Self, ARecord, AValue, AData);
end;

function TcxCustomGridTableItem.HasCellHintHandler: Boolean;
begin
  Result := Assigned(FOnGetCellHint);
end;

function TcxCustomGridTableItem.HasCustomPropertiesForEditHandler: Boolean;
begin
  Result := Assigned(FOnGetPropertiesForEdit);
end;

function TcxCustomGridTableItem.HasCustomPropertiesHandler: Boolean;
begin
  Result := Assigned(FOnGetProperties);
end;

function TcxCustomGridTableItem.HasDataTextHandler: Boolean;
begin
  Result := Assigned(FOnGetDataText);
end;

function TcxCustomGridTableItem.HasInitFilteringDateRangesHandler: Boolean;
begin
  Result := Assigned(FOnInitFilteringDateRanges);
end;

function TcxCustomGridTableItem.HasInitFilteringDateRangesHandlers: Boolean;
begin
  Result := GridView.HasInitFilteringDateRangesHandler or HasInitFilteringDateRangesHandler;
end;

function TcxCustomGridTableItem.HasInitGroupingDateRangesHandler: Boolean;
begin
  Result := Assigned(FOnInitGroupingDateRanges);
end;

function TcxCustomGridTableItem.HasInitGroupingDateRangesHandlers: Boolean;
begin
  Result := GridView.HasInitGroupingDateRangesHandler or HasInitGroupingDateRangesHandler;
end;

function TcxCustomGridTableItem.HasValidateDrawValueHandler: Boolean;
begin
  Result := Assigned(FOnValidateDrawValue);
end;

procedure TcxCustomGridTableItem.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableItem then
    with TcxCustomGridTableItem(Source) do
    begin
      Self.AlternateCaption := AlternateCaption;
      Self.BestFitMaxWidth := BestFitMaxWidth;
      Self.DataBinding := DataBinding;
      Self.FIsCaptionAssigned := FIsCaptionAssigned;
      Self.Caption := Caption;
      Self.DateTimeGrouping := DateTimeGrouping;
      Self.GroupIndex := GroupIndex;
      Self.FIsHeaderAlignmentHorzAssigned := FIsHeaderAlignmentHorzAssigned;
      Self.HeaderAlignmentHorz := HeaderAlignmentHorz;
      Self.FIsHeaderAlignmentVertAssigned := FIsHeaderAlignmentVertAssigned;
      Self.HeaderAlignmentVert := HeaderAlignmentVert;
      Self.HeaderHint := HeaderHint;
      Self.MinWidth := MinWidth;
      Self.Options := Options;
      Self.PropertiesClass := PropertiesClass;
      Self.Properties := Properties;
      Self.RepositoryItem := RepositoryItem;
      Self.SortOrder := SortOrder;
      Self.SortIndex := SortIndex;
      Self.Styles := Styles;
      Self.Tag := Tag;
      Self.Visible := Visible;
      Self.VisibleForCustomization := VisibleForCustomization;
      Self.FIsWidthAssigned := FIsWidthAssigned;
      Self.Width := Width;
      Self.OnCustomDrawCell := OnCustomDrawCell;
      Self.OnGetCellHint := OnGetCellHint;
      Self.OnGetDataText := OnGetDataText;
      Self.OnGetDisplayText := OnGetDisplayText;
      Self.OnGetFilterDisplayText := OnGetFilterDisplayText;
      Self.OnGetFilterValues := OnGetFilterValues;
      Self.OnGetProperties := OnGetProperties;
      Self.OnGetPropertiesForEdit := OnGetPropertiesForEdit;
      Self.OnGetStoredProperties := OnGetStoredProperties;
      Self.OnGetStoredPropertyValue := OnGetStoredPropertyValue;
      Self.OnInitFilteringDateRanges := OnInitFilteringDateRanges;
      Self.OnInitGroupingDateRanges := OnInitGroupingDateRanges;
      Self.OnSetStoredPropertyValue := OnSetStoredPropertyValue;
      Self.OnUserFiltering := OnUserFiltering;
      Self.OnUserFilteringEx := OnUserFilteringEx;
      Self.OnValidateDrawValue := OnValidateDrawValue;
    end
  else
    inherited;
end;

function TcxCustomGridTableItem.GetParentComponent: TComponent;
begin
  Result := FGridView;
end;

function TcxCustomGridTableItem.HasParent: Boolean;
begin
  Result := FGridView <> nil;
end;

procedure TcxCustomGridTableItem.ApplyBestFit(ACheckSizingAbility: Boolean = False;
  AFireEvents: Boolean = False);
begin
  if GridView.IsPattern or ACheckSizingAbility and not CanHorzSize then Exit;
  GridView.BeginBestFitUpdate;
  try
    ForceWidth(GetBestFitWidth);
    Changed(ticSize);
  finally
    GridView.EndBestFitUpdate;
    BestFitApplied(AFireEvents);
  end;
end;

function TcxCustomGridTableItem.CalculateDefaultCellHeight(ACanvas: TcxCanvas; AFont: TFont): Integer;
var
  AEditStyle: TcxEditStyle;
  AEditSizeProperties: TcxEditSizeProperties;
begin
  AEditStyle := GetCellStyle;
  try
    AEditStyle.Font := AFont;
    AEditSizeProperties.Height := -1;
    AEditSizeProperties.MaxLineCount := 0;
    AEditSizeProperties.Width := -1;
    Result := GetProperties.GetEditSize(ACanvas, AEditStyle, True, Null, AEditSizeProperties).cy;
    if Result <> 0 then
      Inc(Result, 2 * cxGridEditOffset);
  finally
    ReleaseCellStyle;
  end;
end;

function TcxCustomGridTableItem.CreateEditStyle: TcxEditStyle;
begin
  Result := GetProperties.GetStyleClass.Create(nil, True) as TcxEditStyle;
  if GridView.Control <> nil then
    Result.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
end;

procedure TcxCustomGridTableItem.FocusWithSelection;
begin
  Focused := True;
end;

function TcxCustomGridTableItem.GetAlternateCaption: string;
begin
  Result := FAlternateCaption;
  if Result = '' then
    Result := DefaultAlternateCaption;
end;

function TcxCustomGridTableItem.GetDefaultValuesProvider(AProperties: TcxCustomEditProperties = nil): IcxEditDefaultValuesProvider;
begin
  if FDataBinding = nil then
    Result := nil
  else
    Result := FDataBinding.GetDefaultValuesProvider(AProperties);
end;

function TcxCustomGridTableItem.GetProperties: TcxCustomEditProperties;
begin
  Result := FPropertiesValue;
  InitProperties(Result);
end;

function TcxCustomGridTableItem.GetProperties(ARecord: TcxCustomGridRecord): TcxCustomEditProperties;
begin
  Result := DoGetProperties(ARecord);
  InitProperties(Result);
end;

function TcxCustomGridTableItem.GetProperties(ARecordIndex: Integer): TcxCustomEditProperties;
var
  ARecord: TcxCustomGridRecord;
begin
  if HasCustomPropertiesHandler then
  begin
    ARecord := ViewData.GetRecordByRecordIndex(ARecordIndex);
    if ARecord = nil then
      Result := GetProperties
    else
      Result := GetProperties(ARecord);
  end
  else
    Result := GetProperties;
end;

function TcxCustomGridTableItem.GetRepositoryItem: TcxEditRepositoryItem;
begin
  Result := FRepositoryItem;
  if (Result = nil) and not IsDestroying then
  begin
    Result := DefaultRepositoryItem;
    if Result <> nil then
    begin
      Result.AddListener(Self);
      FLastUsedDefaultRepositoryItem := Result;
    end;
  end;
end;

procedure TcxCustomGridTableItem.MakeVisible;
begin
  Controller.MakeItemVisible(Self);
end;

procedure TcxCustomGridTableItem.RestoreDefaults;
begin
  FIsCaptionAssigned := False;
  FIsHeaderAlignmentHorzAssigned := False;
  FIsHeaderAlignmentVertAssigned := False;
  FIsWidthAssigned := False;
  Changed(ticSize);
end;

{ TcxCustomGridTableBackgroundBitmaps }

function TcxCustomGridTableBackgroundBitmaps.GetBitmapStyleIndex(Index: Integer): Integer;
begin
  case Index of
    bbContent:
      Result := vsContent;
    bbFilterBox:
      Result := vsFilterBox;
  else
    Result := inherited GetBitmapStyleIndex(Index);
  end;
end;

procedure TcxCustomGridTableBackgroundBitmaps.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableBackgroundBitmaps then
    with TcxCustomGridTableBackgroundBitmaps(Source) do
    begin
      Self.Content := Content;
      Self.FilterBox := FilterBox;
    end;
  inherited;
end;

{ TcxCustomGridTableDateTimeHandling }

constructor TcxCustomGridTableDateTimeHandling.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FGrouping := dtgByDateAndTime;
  FUseLongDateFormat := True;
  FUseShortTimeFormat := True;
end;

function TcxCustomGridTableDateTimeHandling.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

procedure TcxCustomGridTableDateTimeHandling.SetDateFormat(const Value: string);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetFilters(Value: TcxGridDateTimeFilters);
begin
  if FFilters <> Value then
  begin
    FFilters := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetGrouping(Value: TcxGridDateTimeGrouping);
begin
  if Value = dtgDefault then Value := dtgByDateAndTime;
  if FGrouping <> Value then
  begin
    FGrouping := Value;
    GroupingChanged;
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetHourFormat(const Value: string);
begin
  if FHourFormat <> Value then
  begin
    FHourFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetIgnoreTimeForFiltering(Value: Boolean);
begin
  if FIgnoreTimeForFiltering <> Value then
  begin
    FIgnoreTimeForFiltering := Value;
    GridView.DataController.Refresh;
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetMonthFormat(const Value: string);
begin
  if FMonthFormat <> Value then
  begin
    FMonthFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetUseLongDateFormat(Value: Boolean);
begin
  if FUseLongDateFormat <> Value then
  begin
    FUseLongDateFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetUseShortTimeFormat(Value: Boolean);
begin
  if FUseShortTimeFormat <> Value then
  begin
    FUseShortTimeFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.SetYearFormat(const Value: string);
begin
  if FYearFormat <> Value then
  begin
    FYearFormat := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableDateTimeHandling.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxCustomGridTableDateTimeHandling then
    with TcxCustomGridTableDateTimeHandling(Source) do
    begin
      Self.DateFormat := DateFormat;
      Self.Filters := Filters;
      Self.Grouping := Grouping;
      Self.HourFormat := HourFormat;
      Self.IgnoreTimeForFiltering := IgnoreTimeForFiltering;
      Self.MonthFormat := MonthFormat;
      Self.UseLongDateFormat := UseLongDateFormat;
      Self.UseShortTimeFormat := UseShortTimeFormat;
      Self.YearFormat := YearFormat;
    end;
end;

function TcxCustomGridTableDateTimeHandling.GetDateFormat: string;
begin
  Result := FDateFormat;
  if Result = '' then
    if UseLongDateFormat then
      Result := dxFormatSettings.LongDateFormat
    else
      Result := '';
end;

function TcxCustomGridTableDateTimeHandling.GetHourFormat: string;
begin
  Result := FHourFormat;
  if Result = '' then
    if UseShortTimeFormat then
      Result := dxFormatSettings.ShortTimeFormat
    else
      Result := '';
end;

function TcxCustomGridTableDateTimeHandling.GetMonthFormat: string;
begin
  Result := FMonthFormat;
  if Result = '' then
    Result := cxGetResourceString(@scxGridMonthFormat);
end;

function TcxCustomGridTableDateTimeHandling.GetYearFormat: string;
begin
  Result := FYearFormat;
  if Result = '' then
    Result := cxGetResourceString(@scxGridYearFormat);
end;

procedure TcxCustomGridTableDateTimeHandling.GroupingChanged;
var
  I: Integer;
begin
  if GridView.ItemCount = 0 then
    Changed(vcProperty)
  else
  begin
    GridView.BeginUpdate;
    try
      for I := 0 to GridView.ItemCount - 1 do
        GridView.Items[I].DateTimeGroupingChanged;
    finally
      GridView.EndUpdate;
    end;
  end;
end;

{ TcxGridFilterMRUItems }

constructor TcxGridFilterMRUItems.Create(AFiltering: TcxCustomGridTableFiltering);
begin
  inherited Create;
  FFiltering := AFiltering;
end;

procedure TcxGridFilterMRUItems.BeginUpdate;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    Filtering.GridView.BeginUpdate;
end;

procedure TcxGridFilterMRUItems.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
  begin
    RefreshVisibleItemsList;
    Filtering.GridView.EndUpdate;
  end;
end;

function TcxGridFilterMRUItems.GetCurrentFilter: TcxDataFilterCriteria;
begin
  Result := Filtering.GridView.DataController.Filter;
end;

function TcxGridFilterMRUItems.GetItemClass: TdxFilterBoxMRUItemClass;
begin
  Result := TcxGridFilterMRUItem;
end;

procedure TcxGridFilterMRUItems.RefreshVisibleItemsList;
begin
  if IsLocked then
    Exit;
  inherited RefreshVisibleItemsList;
end;

procedure TcxGridFilterMRUItems.VisibleCountChanged(APrevVisibleCount: Integer);
begin
  FFiltering.MRUItemsVisibleCountChanged(APrevVisibleCount, VisibleCount);
end;

function TcxGridFilterMRUItems.GetIsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

{ TcxGridFilterBox }

constructor TcxGridFilterBox.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCustomizeButtonAlignment := fbaRight;
  FCustomizeDialog := True;
  FPosition := fpBottom;
  FVisible := fvNonEmpty;
end;

procedure TcxGridFilterBox.SetButtonPosition(
  const Value: TcxGridFilterButtonAlignment);
begin
  if FCustomizeButtonAlignment <> Value then
  begin
    FCustomizeButtonAlignment := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridFilterBox.SetCustomizeDialog(Value: Boolean);
begin
  if FCustomizeDialog <> Value then
  begin
    FCustomizeDialog := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridFilterBox.SetMRUItemsListDropDownCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMRUItemsListDropDownCount <> Value then
  begin
    FMRUItemsListDropDownCount := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridFilterBox.SetPosition(Value: TcxGridFilterPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridFilterBox.SetVisible(Value: TcxGridFilterVisible);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridFilterBox.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridFilterBox then
    with TcxGridFilterBox(Source) do
    begin
      Self.CustomizeButtonAlignment := CustomizeButtonAlignment;
      Self.CustomizeDialog := CustomizeDialog;
      Self.MRUItemsListDropDownCount := MRUItemsListDropDownCount;
      Self.Position := Position;
      Self.Visible := Visible;
    end;
end;

{ TcxGridFindPanel }

constructor TcxGridFindPanel.Create(AController: TcxCustomGridTableController);
begin
  FController := AController;
  inherited Create;
end;

procedure TcxGridFindPanel.Changed;
begin
  GridView.Changed(vcSize);
end;

procedure TcxGridFindPanel.ApplyText(const AText: string);
begin
  Controller.ApplyFindFilterText(AText);
end;

procedure TcxGridFindPanel.ClearText;
begin
  Controller.ClearFindFilterText;
end;

procedure TcxGridFindPanel.FocusChanged;
begin
  TcxControlAccess(GridView.Site).FocusChanged;
end;

procedure TcxGridFindPanel.FocusControl;
begin
  GridView.Site.SetFocus;
end;

function TcxGridFindPanel.FocusData: Boolean;
begin
  Result := Controller.FocusRecord(0, True);
end;

function TcxGridFindPanel.GetClearButtonCaption: string;
begin
  Result := cxGetResourceString(@scxGridFindPanelClearButtonCaption);
end;

procedure TcxGridFindPanel.GetContentViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsContent, nil, nil, AParams);
end;

function TcxGridFindPanel.GetDefaultInfoText: string;
begin
  Result := cxGetResourceString(@scxGridFindPanelInfoText);
end;

function TcxGridFindPanel.GetDefaultMRUItemsListCount: Integer;
begin
  Result := cxGridFindPanelDefaultMRUItemsListCount;
end;

function TcxGridFindPanel.GetDefaultMRUItemsListDropDownCount: Integer;
begin
  Result := cxGridFindPanelDefaultMRUItemsListDropDownCount;
end;

function TcxGridFindPanel.GetEditLookAndFeel: TcxLookAndFeel;
begin
  Result := GridView.LookAndFeel;
end;

function TcxGridFindPanel.GetFindButtonCaption: string;
begin
  Result := cxGetResourceString(@scxGridFindPanelFindButtonCaption);
end;

function TcxGridFindPanel.GetOwner: TComponent;
begin
  Result := GridView;
end;

function TcxGridFindPanel.GetParent: TWinControl;
begin
  Result := GridView.Site;
end;

function TcxGridFindPanel.GetText: string;
begin
  Result := Controller.GetFindFilterText;
end;

function TcxGridFindPanel.GetUseExtendedSyntax: Boolean;
begin
  Result := GridView.DataController.UseExtendedFindFilterTextSyntax;
end;

procedure TcxGridFindPanel.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsFindPanel, nil, nil, AParams);
end;

procedure TcxGridFindPanel.VisibilityChanged;
begin
  inherited VisibilityChanged;
  if not GridView.OptionsBehavior.FocusCellOnTab then
    if Visible then
      GridView.Site.Keys := GridView.Site.Keys + [kTab]
    else
      GridView.Site.Keys := GridView.Site.Keys - [kTab];
  GridView.FindPanelVisibilityChanged(Visible);
end;

procedure TcxGridFindPanel.SetUseExtendedSyntax(AValue: Boolean);
begin
  GridView.DataController.UseExtendedFindFilterTextSyntax := AValue;
end;

function TcxGridFindPanel.GetGridView: TcxCustomGridTableView;
begin
  Result := Controller.GridView;
end;

{ TcxGridFindPanelOptions }

procedure TcxGridFindPanelOptions.Assign(Source: TPersistent);
var
  AOptions: TcxGridFindPanelOptions;
begin
  inherited Assign(Source);
  if Source is TcxGridFindPanelOptions then
  begin
    AOptions := TcxGridFindPanelOptions(Source);
    Options := AOptions.Options;
  end;
end;

function TcxGridFindPanelOptions.IsCloseButtonVisible: Boolean;
begin
  Result := Options.IsCloseButtonVisible;
end;

function TcxGridFindPanelOptions.GetApplyInputDelay: Integer;
begin
  Result := Options.ApplyInputDelay;
end;

function TcxGridFindPanelOptions.GetClearFindFilterTextOnClose: Boolean;
begin
  Result := Options.ClearOnClose;
end;

function TcxGridFindPanelOptions.GetDisplayMode: TcxFindPanelDisplayMode;
begin
  Result := Options.DisplayMode;
end;

function TcxGridFindPanelOptions.GetFocusViewOnApplyFilter: Boolean;
begin
  Result := Options.FocusContentOnApply;
end;

function TcxGridFindPanelOptions.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridFindPanelOptions.GetHighlightSearchResults: Boolean;
begin
  Result := Options.HighlightSearchResults;
end;

function TcxGridFindPanelOptions.GetInfoText: string;
begin
  Result := Options.InfoText;
end;

function TcxGridFindPanelOptions.GetOptions: TcxFindPanelOptions;
begin
  Result := GridView.Controller.FindPanel.Options;
end;

function TcxGridFindPanelOptions.GetMRUItems: TStrings;
begin
  Result := Options.MRUItems;
end;

function TcxGridFindPanelOptions.GetMRUItemsListCount: Integer;
begin
  Result := Options.MRUItemsListCount;
end;

function TcxGridFindPanelOptions.GetMRUItemsListDropDownCount: Integer;
begin
  Result := Options.MRUItemsListDropDownCount;
end;

function TcxGridFindPanelOptions.GetPosition: TcxFindPanelPosition;
begin
  Result := Options.Position;
end;

function TcxGridFindPanelOptions.GetShowClearButton: Boolean;
begin
  Result := Options.ShowClearButton;
end;

function TcxGridFindPanelOptions.GetShowCloseButton: Boolean;
begin
  Result := Options.ShowCloseButton;
end;

function TcxGridFindPanelOptions.GetShowFindButton: Boolean;
begin
  Result := Options.ShowFindButton;
end;

function TcxGridFindPanelOptions.GetUseDelayedFind: Boolean;
begin
  Result := Options.UseDelayedFind;
end;

function TcxGridFindPanelOptions.GetUseExtendedSyntax: Boolean;
begin
  Result := Options.UseExtendedSyntax;
end;

function TcxGridFindPanelOptions.IsInfoTextStored: Boolean;
begin
  Result := Options.InfoTextAssigned;
end;

procedure TcxGridFindPanelOptions.SetApplyInputDelay(AValue: Integer);
begin
  Options.ApplyInputDelay := AValue;
end;

procedure TcxGridFindPanelOptions.SetClearFindFilterTextOnClose(AValue: Boolean);
begin
  Options.ClearOnClose := AValue;
end;

procedure TcxGridFindPanelOptions.SetDisplayMode(AValue: TcxFindPanelDisplayMode);
begin
  Options.DisplayMode := AValue;
end;

procedure TcxGridFindPanelOptions.SetFocusViewOnApplyFilter(AValue: Boolean);
begin
  Options.FocusContentOnApply := AValue;
end;

procedure TcxGridFindPanelOptions.SetHighlightSearchResults(AValue: Boolean);
begin
  Options.HighlightSearchResults := AValue;
end;

procedure TcxGridFindPanelOptions.SetInfoText(AValue: string);
begin
  Options.InfoText := AValue;
end;

procedure TcxGridFindPanelOptions.SetOptions(AValue: TcxFindPanelOptions);
begin
  GridView.Controller.FindPanel.Options := AValue;
end;

procedure TcxGridFindPanelOptions.SetMRUItems(AValue: TStrings);
begin
  Options.MRUItems := AValue;
end;

procedure TcxGridFindPanelOptions.SetMRUItemsListCount(AValue: Integer);
begin
  Options.MRUItemsListCount := AValue;
end;

procedure TcxGridFindPanelOptions.SetMRUItemsListDropDownCount(AValue: Integer);
begin
  Options.MRUItemsListDropDownCount := AValue;
end;

procedure TcxGridFindPanelOptions.SetPosition(AValue: TcxFindPanelPosition);
begin
  Options.Position := AValue;
end;

procedure TcxGridFindPanelOptions.SetShowClearButton(AValue: Boolean);
begin
  Options.ShowClearButton := AValue;
end;

procedure TcxGridFindPanelOptions.SetShowCloseButton(AValue: Boolean);
begin
  Options.ShowCloseButton := AValue;
end;

procedure TcxGridFindPanelOptions.SetShowFindButton(AValue: Boolean);
begin
  Options.ShowFindButton := AValue;
end;

procedure TcxGridFindPanelOptions.SetUseExtendedSyntax(AValue: Boolean);
begin
  Options.UseExtendedSyntax := AValue;
end;

procedure TcxGridFindPanelOptions.SetUseDelayedFind(AValue: Boolean);
begin
  Options.UseDelayedFind := AValue;
end;

{ TcxGridFilterHitTest }

class function TcxGridFilterHitTest.GetHitTestCode: Integer;
begin
  Result := htFilter;
end;

{ TcxGridFilterCloseButtonHitTest }

class function TcxGridFilterCloseButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFilterCloseButton;
end;

{ TcxGridFilterActivateButtonHitTest }

class function TcxGridFilterActivateButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFilterActivateButton;
end;

{ TcxGridFilterDropDownButtonHitTest }

class function TcxGridFilterDropDownButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFilterDropDownButton;
end;

{ TcxGridFilterCustomizeButtonHitTest }

class function TcxGridFilterCustomizeButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFilterCustomizeButton;
end;

{ TcxGridFindPanelHitTest }

class function TcxGridFindPanelHitTest.GetHitTestCode: Integer;
begin
  Result := htFindPanel;
end;

{ TcxGridFindPanelCloseButtonHitTest }

class function TcxGridFindPanelCloseButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFindPanelCloseButton;
end;

{ TcxGridFindPanelFindButtonHitTest }

class function TcxGridFindPanelFindButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFindPanelFindButton;
end;

{ TcxGridFindPanelClearButtonHitTest }

class function TcxGridFindPanelClearButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htFindPanelClearButton;
end;

{ TcxGridCheckBoxHitTest }

class function TcxGridCheckBoxHitTest.GetHitTestCode: Integer;
begin
  Result := htCheckBox;
end;

{ TcxGridPinHitTest }

class function TcxGridPinHitTest.GetHitTestCode: Integer;
begin
  Result := htCustomGridTableBase;
end;

{ TcxGridRecordHitTest }

procedure TcxGridRecordHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridRecordHitTest then
  begin
    FGridRecordIndex := TcxGridRecordHitTest(Source).FGridRecordIndex;
    FGridRecordKind := TcxGridRecordHitTest(Source).FGridRecordKind;
    FViewData := TcxGridRecordHitTest(Source).FViewData;
  end;
end;

function TcxGridRecordHitTest.GetGridRecord: TcxCustomGridRecord;
begin
  if FViewData = nil then
    Result := nil
  else
    Result := FViewData.GetRecordByKind(FGridRecordKind, FGridRecordIndex);
end;

procedure TcxGridRecordHitTest.SetGridRecord(Value: TcxCustomGridRecord);
begin
  if Value <> nil then
  begin
    FViewData := Value.ViewData;
    FGridRecordIndex := Value.Index;
    FGridRecordKind := FViewData.GetRecordKind(Value);
  end
  else
  begin
    FViewData := nil;
    FGridRecordIndex := -1;
    FGridRecordKind := rkNone;
  end;
end;

class function TcxGridRecordHitTest.GetHitTestCode: Integer;
begin
  Result := htRecord;
end;

class function TcxGridRecordHitTest.CanClick: Boolean;
begin
  Result := True;
end;

{ TcxGridRecordCellHitTest }

procedure TcxGridRecordCellHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridRecordCellHitTest then
    Item := TcxGridRecordCellHitTest(Source).Item;
end;

function TcxGridRecordCellHitTest.Cursor: TCursor;
begin
  Result := inherited Cursor;
  if (ViewInfo is TcxGridTableDataCellViewInfo) and (TcxGridTableDataCellViewInfo(ViewInfo).EditViewInfo <> nil) and
      Item.CanFocus(GridRecord) and TcxGridTableDataCellViewInfo(ViewInfo).HotTrack then
    Result := TcxCustomEditViewInfoAccess(TcxGridTableDataCellViewInfo(ViewInfo).EditViewInfo).GetCurrentCursor(Pos);
end;

class function TcxGridRecordCellHitTest.GetHitTestCode: Integer;
begin
  Result := htCell;
end;

{ TcxGridExpandButtonHitTest }

class function TcxGridExpandButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htExpandButton;
end;

class function TcxGridExpandButtonHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridDefaultValuesProvider }

function TcxGridDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := TcxGridItemDataBinding(Owner).IsDisplayFormatDefined(AIsCurrencyValueAccepted);
end;

{ TcxGridItemFilterPopupOptions }

constructor TcxGridItemFilterPopupOptions.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FMaxDropDownItemCount := cxGridFilterDefaultItemPopupMaxDropDownItemCount;
  FMultiSelect := True;
end;

procedure TcxGridItemFilterPopupOptions.ChangeScale(M: Integer; D: Integer);
begin
  inherited;
  DropDownWidth := MulDiv(DropDownWidth, M, D);
end;

procedure TcxGridItemFilterPopupOptions.SetApplyMultiSelectChanges(AValue: TcxGridItemFilterPopupApplyChangesMode);
begin
  if FApplyMultiSelectChanges <> AValue then
  begin
    FApplyMultiSelectChanges := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemFilterPopupOptions.SetDropDownWidth(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FDropDownWidth <> AValue then
  begin
    FDropDownWidth := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemFilterPopupOptions.SetMaxDropDownItemCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FMaxDropDownItemCount <> AValue then
  begin
    FMaxDropDownItemCount := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemFilterPopupOptions.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect <> AValue then
  begin
    FMultiSelect := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemFilterPopupOptions.Assign(ASource: TPersistent);
var
  AOptions: TcxGridItemFilterPopupOptions;
begin
  inherited Assign(ASource);
  if ASource is TcxGridItemFilterPopupOptions then
  begin
    AOptions := TcxGridItemFilterPopupOptions(ASource);
    ApplyMultiSelectChanges := AOptions.ApplyMultiSelectChanges;
    DropDownWidth := AOptions.DropDownWidth;
    MaxDropDownItemCount := AOptions.MaxDropDownItemCount;
    MultiSelect := AOptions.MultiSelect;
  end;
end;

{ TcxGridItemExcelFilterPopupOptions }

procedure TcxGridItemExcelFilterPopupOptions.Assign(ASource: TPersistent);
var
  AOptions: TcxGridItemExcelFilterPopupOptions;
begin
  inherited Assign(ASource);
  if ASource is TcxGridItemExcelFilterPopupOptions then
  begin
    AOptions := TcxGridItemExcelFilterPopupOptions(ASource);
    DateTimeValuesPageType := AOptions.DateTimeValuesPageType;
    DefaultPage := AOptions.DefaultPage;
    ApplyChanges := AOptions.ApplyChanges;
    NumericValuesPageType := AOptions.NumericValuesPageType;
  end;
end;

procedure TcxGridItemExcelFilterPopupOptions.SetApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
begin
  if AValue <> ApplyChanges then
  begin
    FApplyChanges := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemExcelFilterPopupOptions.SetDateTimeValuesPageType(
  const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
begin
  if AValue <> DateTimeValuesPageType then
  begin
    FDateTimeValuesPageType := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemExcelFilterPopupOptions.SetDefaultPage(const AValue: TdxExcelFilterValueContainerDefaultPage);
begin
  if AValue <> DefaultPage then
  begin
    FDefaultPage := AValue;
    Changed(vcProperty);
  end;
end;

procedure TcxGridItemExcelFilterPopupOptions.SetNumericValuesPageType(
  const AValue: TdxExcelFilterValueContainerNumericValuesPageType);
begin
  if AValue <> NumericValuesPageType then
  begin
    FNumericValuesPageType := AValue;
    Changed(vcProperty);
  end;
end;

{ TcxCustomGridTableFiltering }

constructor TcxCustomGridTableFiltering.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FItemAddValueItems := True;
  FItemMRUItemsList := True;
  FItemMRUItemsListCount := cxGridFilterDefaultItemMRUItemsListCount;
  FItemPopup := GetItemPopupClass.Create(GridView);
  FItemExcelPopup := GetItemExcelPopupClass.Create(GridView);
  FMRUItems := GetMRUItemsClass.Create(Self);
  FMRUItemsList := True;
  MRUItemsListCount := cxGridFilterDefaultMRUItemsListCount;
end;

destructor TcxCustomGridTableFiltering.Destroy;
begin
  FMRUItems.Free;
  FItemExcelPopup.Free;
  FItemPopup.Free;
  inherited;
end;

function TcxCustomGridTableFiltering.GetCustomizeDialog: Boolean;
begin
  Result := GridView.FilterBox.CustomizeDialog;
end;

function TcxCustomGridTableFiltering.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableFiltering.GetMRUItemsListDropDownCount: Integer;
begin
  Result := GridView.FilterBox.MRUItemsListDropDownCount;
end;

function TcxCustomGridTableFiltering.GetPosition: TcxGridFilterPosition;
begin
  Result := GridView.FilterBox.Position;
end;

function TcxCustomGridTableFiltering.GetVisible: TcxGridFilterVisible;
begin
  Result := GridView.FilterBox.Visible;
end;

procedure TcxCustomGridTableFiltering.SetCustomizeDialog(Value: Boolean);
begin
  GridView.FilterBox.CustomizeDialog := Value;
end;

procedure TcxCustomGridTableFiltering.SetItemAddValueItems(Value: Boolean);
begin
  if FItemAddValueItems <> Value then
  begin
    FItemAddValueItems := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetItemExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
begin
  FItemExcelPopup.Assign(Value);
end;

procedure TcxCustomGridTableFiltering.SetItemFilteredItemsList(Value: Boolean);
begin
  if FItemFilteredItemsList <> Value then
  begin
    FItemFilteredItemsList := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetItemMRUItemsList(Value: Boolean);
begin
  if FItemMRUItemsList <> Value then
  begin
    FItemMRUItemsList := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetItemMRUItemsListCount(Value: Integer);
var
  I: Integer;
begin
  if Value < 0 then Value := 0;
  if FItemMRUItemsListCount <> Value then
  begin
    FItemMRUItemsListCount := Value;
    for I := 0 to GridView.ItemCount - 1 do
      GridView.Items[I].DataBinding.FilterMRUValueItems.MaxCount := FItemMRUItemsListCount;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetItemPopup(Value: TcxGridItemFilterPopupOptions);
begin
  FItemPopup.Assign(Value);
end;

procedure TcxCustomGridTableFiltering.SetItemPopupMode(Value: TdxFilterPopupWindowMode);
begin
  if ItemPopupMode <> Value then
  begin
    FItemPopupMode := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetMRUItemsList(Value: Boolean);
begin
  if FMRUItemsList <> Value then
  begin
    FMRUItemsList := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableFiltering.SetMRUItemsListCount(Value: Integer);

  function GetMRUItemsMaxCount: Integer;
  begin
    Result := FMRUItemsListCount;
    if Result <> 0 then Inc(Result);  // for current filter
  end;

begin
  if Value < 0 then Value := 0;
  if FMRUItemsListCount <> Value then
  begin
    FMRUItemsListCount := Value;
    FMRUItems.SetMaxCount(GetMRUItemsMaxCount);
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableFiltering.SetMRUItemsListDropDownCount(Value: Integer);
begin
  GridView.FilterBox.MRUItemsListDropDownCount := Value;
end;

procedure TcxCustomGridTableFiltering.SetPosition(Value: TcxGridFilterPosition);
begin
  GridView.FilterBox.Position := Value;
end;

procedure TcxCustomGridTableFiltering.SetVisible(Value: TcxGridFilterVisible);
begin
  GridView.FilterBox.Visible := Value;
end;

procedure TcxCustomGridTableFiltering.AfterFilterControlDialogApply(Sender: TObject);
begin
  if FSynchronizeWithControlDialog then
  begin
    FSynchronizeWithControlDialog := False;
    GridView.EndFilteringUpdate;
  end;
end;

procedure TcxCustomGridTableFiltering.BeforeFilterControlDialogApply(Sender: TObject);
begin
  FSynchronizeWithControlDialog := (Sender as TfmFilterControlDialog).FilterControl.IsNeedSynchronize;
  if FSynchronizeWithControlDialog then
    GridView.BeginFilteringUpdate;
end;

procedure TcxCustomGridTableFiltering.FilterControlDialogApply(Sender: TObject);
begin
  with GridView.DataController.Filter do
  begin
    if not IsEmpty and not Active then
    begin
      GridView.BeginFilteringUpdate;
      try
        Active := True;
      finally
        GridView.EndFilteringUpdate;
      end;
    end;
  end;
  AddFilterToMRUItems;
end;

procedure TcxCustomGridTableFiltering.ReadCustomizeDialog(Reader: TReader);
begin
  CustomizeDialog := Reader.ReadBoolean;
end;

procedure TcxCustomGridTableFiltering.ReadMRUItemsListDropDownCount(Reader: TReader);
begin
  MRUItemsListDropDownCount := Reader.ReadInteger;
end;

procedure TcxCustomGridTableFiltering.ReadPosition(Reader: TReader);
begin
  Position := TcxGridFilterPosition(GetEnumValue(TypeInfo(TcxGridFilterPosition), Reader.ReadIdent));
end;

procedure TcxCustomGridTableFiltering.ReadVisible(Reader: TReader);
begin
  Visible := TcxGridFilterVisible(GetEnumValue(TypeInfo(TcxGridFilterVisible), Reader.ReadIdent));
end;

procedure TcxCustomGridTableFiltering.ChangeScale(M, D: Integer);
begin
  inherited;
  ItemPopup.ChangeScale(M, D);
end;

procedure TcxCustomGridTableFiltering.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('CustomizeDialog', ReadCustomizeDialog, nil, True);
  Filer.DefineProperty('DropDownWidth', ReadItemPopupDropDownWidth, nil, True);
  Filer.DefineProperty('MaxDropDownCount', ReadItemPopupMaxDropDownCount, nil, True);
  Filer.DefineProperty('MRUItemsListDropDownCount', ReadMRUItemsListDropDownCount, nil, True);
  Filer.DefineProperty('Position', ReadPosition, nil, True);
  Filer.DefineProperty('Visible', ReadVisible, nil, True);
end;

procedure TcxCustomGridTableFiltering.FilterChanged;
begin
  FMRUItems.FilterChanged;
end;

function TcxCustomGridTableFiltering.GetItemPopupClass: TcxGridItemFilterPopupOptionsClass;
begin
  Result := TcxGridItemFilterPopupOptions;
end;

function TcxCustomGridTableFiltering.GetItemExcelPopupClass: TcxGridItemExcelFilterPopupOptionsClass;
begin
  Result := TcxGridItemExcelFilterPopupOptions;
end;

function TcxCustomGridTableFiltering.GetMRUItemsClass: TcxGridFilterMRUItemsClass;
begin
  Result := TcxGridFilterMRUItems;
end;

function TcxCustomGridTableFiltering.IsFilterBoxEnabled: Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableFiltering.IsMRUItemsListAvailable: Boolean;
begin
  Result := FMRUItemsList and (FMRUItems.VisibleCount <> 0);
end;

procedure TcxCustomGridTableFiltering.MRUItemsVisibleCountChanged(AOldCount, ANewCount: Integer);
begin
  if FMRUItemsList and ((AOldCount = 0) or (ANewCount = 0)) then
    Changed(vcSize);
end;

function TcxCustomGridTableFiltering.GetItemPopupDropDownWidth: Integer;
begin
  Result := FItemPopup.DropDownWidth;
end;

function TcxCustomGridTableFiltering.GetItemPopupMaxDropDownItemCount: Integer;
begin
  Result := FItemPopup.MaxDropDownItemCount;
end;

procedure TcxCustomGridTableFiltering.SetItemPopupDropDownWidth(Value: Integer);
begin
  FItemPopup.DropDownWidth := Value;
end;

procedure TcxCustomGridTableFiltering.SetItemPopupMaxDropDownItemCount(Value: Integer);
begin
  FItemPopup.MaxDropDownItemCount := Value;
end;

procedure TcxCustomGridTableFiltering.ReadItemPopupDropDownWidth(Reader: TReader);
begin
  DropDownWidth := Reader.ReadInteger;
end;

procedure TcxCustomGridTableFiltering.ReadItemPopupMaxDropDownCount(Reader: TReader);
begin
  MaxDropDownCount := Reader.ReadInteger;
end;

procedure TcxCustomGridTableFiltering.AddFilterToMRUItems(AFilter: TcxDataFilterCriteria = nil);
begin
  if AFilter = nil then
    AFilter := GridView.DataController.Filter;
  FMRUItems.Add(AFilter);
end;

procedure TcxCustomGridTableFiltering.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableFiltering then
    with TcxCustomGridTableFiltering(Source) do
    begin
      Self.ItemAddValueItems := ItemAddValueItems;
      Self.ItemExcelPopup := ItemExcelPopup;
      Self.ItemFilteredItemsList := ItemFilteredItemsList;
      Self.ItemMRUItemsList := ItemMRUItemsList;
      Self.ItemMRUItemsListCount := ItemMRUItemsListCount;
      Self.ItemPopup := ItemPopup;
      Self.ItemPopupMode := ItemPopupMode;
      Self.MRUItemsList := MRUItemsList;
      Self.MRUItemsListCount := MRUItemsListCount;
    end
  else
    inherited;
end;

procedure TcxCustomGridTableFiltering.RunCustomizeDialog(AItem: TcxCustomGridTableItem);
begin
  if GridView.DoFilterDialogShow(AItem) then Exit;
  if (AItem = nil) or IsFilterControlDialogNeeded(GridView.DataController.Filter) then
    if not GridView.DoFilterCustomization then
      ExecuteFilterControlDialog(GridView, GridView.LookAndFeel,
        FilterControlDialogApply, GridView.OnFilterControlDialogShow, clDefault,
        '', TcxCustomGrid(GridView.Control).Font, BeforeFilterControlDialogApply,
        AfterFilterControlDialogApply, GridView.Control)
    else
  else
    if ShowFilterDialog(GridView.DataController.Filter, AItem,
      AItem.GetProperties, AItem.FilterCaption, AItem.DataBinding.ValueTypeClass,
      GridView.LookAndFeel, TcxCustomGrid(GridView.Control).Font, GridView.Control) then
    begin
      GridView.DataController.Filter.Active := True;
      AddFilterToMRUItems;
    end;
end;

{ TcxGridViewNavigatorButtons }

constructor TcxGridViewNavigatorButtons.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView.ViewInfo);
  FGridView := AGridView;
  ConfirmDelete := False;
end;

{ TcxGridViewNavigatorInfoPanel }

constructor TcxGridViewNavigatorInfoPanel.Create(
  AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView.ViewInfo);
  FGridView := AGridView;
end;

function TcxGridViewNavigatorInfoPanel.GetViewParams: TcxViewParams;
begin
  GridView.Styles.GetViewParams(vsNavigatorInfoPanel, nil, nil, Result);
end;

function TcxGridViewNavigatorInfoPanel.GetAdornerTargetElementVisible: Boolean;
begin
  Result := inherited GetAdornerTargetElementVisible and GridView.ViewInfo.NavigatorSiteViewInfo.Visible;
end;

{ TcxGridViewNavigator }

constructor TcxGridViewNavigator.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FButtons := GetNavigatorButtonsClass.Create(GridView) as TcxGridViewNavigatorButtons;
  FButtons.OnGetControl := GridView.GetNavigatorButtonsControl;
  FInfoPanel := TcxGridViewNavigatorInfoPanel.Create(GridView);
  FInfoPanel.OnGetIRecordPosition := GetIRecordPosition;
end;

destructor TcxGridViewNavigator.Destroy;
begin
  FreeAndNil(FButtons);
  FreeAndNil(FInfoPanel);
  inherited Destroy;
end;

procedure TcxGridViewNavigator.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxGridViewNavigator then
  begin
    Buttons := TcxGridViewNavigator(Source).Buttons;
    InfoPanel := TcxGridViewNavigator(Source).InfoPanel;
    Visible := TcxGridViewNavigator(Source).Visible;
  end;
end;

function TcxGridViewNavigator.GetNavigatorButtonsClass: TcxGridViewNavigatorButtonsClass;
begin
  Result := TcxGridViewNavigatorButtons;
end;

function TcxGridViewNavigator.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxGridViewNavigator.GetIRecordPosition: IcxNavigatorRecordPosition;
begin
  Supports(GridView, IcxNavigatorRecordPosition, Result);
end;

procedure TcxGridViewNavigator.SetButtons(Value: TcxGridViewNavigatorButtons);
begin
  FButtons.Assign(Value);
end;

procedure TcxGridViewNavigator.SetInfoPanel(Value: TcxGridViewNavigatorInfoPanel);
begin
  FInfoPanel.Assign(Value);
end;

procedure TcxGridViewNavigator.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(vcSize);
  end;
end;

{ TcxCustomGridTableShowLockedStateImageOptions }

constructor TcxCustomGridTableShowLockedStateImageOptions.Create;
begin
  inherited Create;
  FBestFit := lsimPending;
  FFiltering := lsimPending;
  FGrouping := lsimPending;
  FSorting := lsimPending;
  FPosting := lsimNever;
end;

procedure TcxCustomGridTableShowLockedStateImageOptions.Assign(
  Source: TPersistent);
begin
  if Source is TcxCustomGridTableShowLockedStateImageOptions then
  begin
    BestFit := TcxCustomGridTableShowLockedStateImageOptions(Source).BestFit;
    Filtering := TcxCustomGridTableShowLockedStateImageOptions(Source).Filtering;
    Grouping := TcxCustomGridTableShowLockedStateImageOptions(Source).Grouping;
    Sorting := TcxCustomGridTableShowLockedStateImageOptions(Source).Sorting;
    Posting := TcxCustomGridTableShowLockedStateImageOptions(Source).Posting;
  end;
end;

{ TcxCustomGridTableOptionsBehavior }

constructor TcxCustomGridTableOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FCopyCaptionsToClipboard := True;
  FCopyRecordsToClipboard := True;
  FDragHighlighting := True;
  FDragOpening := True;
  FDragScrolling := True;
  FImmediateEditor := True;
end;

function TcxCustomGridTableOptionsBehavior.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableOptionsBehavior.GetShowLockedStateImageOptions: TcxCustomGridTableShowLockedStateImageOptions;
begin
  Result := TcxCustomGridTableShowLockedStateImageOptions(inherited ShowLockedStateImageOptions);
end;

procedure TcxCustomGridTableOptionsBehavior.SetAlwaysShowEditor(Value: Boolean);
begin
  if FAlwaysShowEditor <> Value then
  begin
    FAlwaysShowEditor := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetBestFitMaxRecordCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBestFitMaxRecordCount <> Value then
  begin
    FBestFitMaxRecordCount := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetCellHints(Value: Boolean);
begin
  if FCellHints <> Value then
  begin
    FCellHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetCopyCaptionsToClipboard(Value: Boolean);
begin
  if FCopyCaptionsToClipboard <> Value then
  begin
    FCopyCaptionsToClipboard := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetCopyRecordsToClipboard(Value: Boolean);
begin
  if FCopyRecordsToClipboard <> Value then
  begin
    FCopyRecordsToClipboard := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetDragDropText(Value: Boolean);
begin
  if FDragDropText <> Value then
  begin
    FDragDropText := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetDragFocusing(Value: TcxGridDragFocusing);
begin
  if FDragFocusing <> Value then
  begin
    FDragFocusing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetDragHighlighting(Value: Boolean);
begin
  if FDragHighlighting <> Value then
  begin
    FDragHighlighting := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetDragOpening(Value: Boolean);
begin
  if FDragOpening <> Value then
  begin
    FDragOpening := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetDragScrolling(Value: Boolean);
begin
  if FDragScrolling <> Value then
  begin
    FDragScrolling := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetEditAutoHeight(Value: TcxInplaceEditAutoHeight);
begin
  if FEditAutoHeight <> Value then
  begin
    FEditAutoHeight := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetFocusCellOnCycle(Value: Boolean);
begin
  if FFocusCellOnCycle <> Value then
  begin
    FFocusCellOnCycle := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetFocusCellOnTab(Value: Boolean);
begin
  if FFocusCellOnTab <> Value then
  begin
    FFocusCellOnTab := Value;
    if not GridView.Controller.IsFindPanelVisible then
      if FFocusCellOnTab then
        GridView.Site.Keys := GridView.Site.Keys + [kTab]
      else
        GridView.Site.Keys := GridView.Site.Keys - [kTab];
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetFocusFirstCellOnNewRecord(Value: Boolean);
begin
  if FFocusFirstCellOnNewRecord <> Value then
  begin
    FFocusFirstCellOnNewRecord := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetGoToNextCellOnEnter(Value: Boolean);
begin
  if FGoToNextCellOnEnter <> Value then
  begin
    FGoToNextCellOnEnter := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetImmediateEditor(Value: Boolean);
begin
  if FImmediateEditor <> Value then
  begin
    FImmediateEditor := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetIncSearch(Value: Boolean);
begin
  if FIncSearch <> Value then
  begin
    if not Value then
      GridView.Controller.CancelIncSearching;
    FIncSearch := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetIncSearchItem(Value: TcxCustomGridTableItem);
begin
  if FIncSearchItem <> Value then
  begin
    FIncSearchItem := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetNavigatorHints(Value: Boolean);
begin
  if FNavigatorHints <> Value then
  begin
    FNavigatorHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetPullFocusing(Value: Boolean);
begin
  if FPullFocusing <> Value then
  begin
    FPullFocusing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetRecordScrollMode(Value: TcxRecordScrollMode);
begin
  if FRecordScrollMode <> Value then
  begin
    FRecordScrollMode := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetRepaintVisibleRecordsOnScroll(Value: Boolean);
begin
  if FRepaintVisibleRecordsOnScroll <> Value then
  begin
    FRepaintVisibleRecordsOnScroll := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsBehavior.SetShowLockedStateImageOptions(
  Value: TcxCustomGridTableShowLockedStateImageOptions);
begin
  inherited ShowLockedStateImageOptions.Assign(Value);
end;

function TcxCustomGridTableOptionsBehavior.GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass;
begin
  Result := TcxCustomGridTableShowLockedStateImageOptions;
end;

procedure TcxCustomGridTableOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableOptionsBehavior then
    with TcxCustomGridTableOptionsBehavior(Source) do
    begin
      Self.AlwaysShowEditor := AlwaysShowEditor;
      Self.BestFitMaxRecordCount := BestFitMaxRecordCount;
      Self.CellHints := CellHints;
      Self.CopyCaptionsToClipboard := CopyCaptionsToClipboard;
      Self.CopyRecordsToClipboard := CopyRecordsToClipboard;
      Self.DragDropText := DragDropText;
      Self.DragFocusing := DragFocusing;
      Self.DragHighlighting := DragHighlighting;
      Self.DragOpening := DragOpening;
      Self.DragScrolling := DragScrolling;
      Self.EditAutoHeight := EditAutoHeight;
      Self.FocusCellOnCycle := FocusCellOnCycle;
      Self.FocusCellOnTab := FocusCellOnTab;
      Self.FocusFirstCellOnNewRecord := FocusFirstCellOnNewRecord;
      Self.GoToNextCellOnEnter := GoToNextCellOnEnter;
      Self.ImmediateEditor := ImmediateEditor;
      Self.IncSearch := IncSearch;
      Self.IncSearchItem := IncSearchItem;
      Self.NavigatorHints := NavigatorHints;
      Self.RecordScrollMode := RecordScrollMode;
      Self.PullFocusing := PullFocusing;
      Self.RepaintVisibleRecordsOnScroll := RepaintVisibleRecordsOnScroll;
    end;
  inherited Assign(Source);
end;

{ TcxCustomGridTableOptionsCustomize }

constructor TcxCustomGridTableOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FItemFiltering := True;
  FItemGrouping := True;
  FItemMoving := True;
  FItemSorting := True;
  FItemsQuickCustomizationMultiColumnMode := True;
  FItemsQuickCustomizationShowCommands := True;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemFiltering(Value: Boolean);
begin
  if FItemFiltering <> Value then
  begin
    FItemFiltering := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemGrouping(Value: Boolean);
begin
  if FItemGrouping <> Value then
  begin
    FItemGrouping := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemHiding(Value: Boolean);
begin
  if FItemHiding <> Value then
  begin
    FItemHiding := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemMoving(Value: Boolean);
begin
  if FItemMoving <> Value then
  begin
    FItemMoving := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemSorting(Value: Boolean);
begin
  if FItemSorting <> Value then
  begin
    FItemSorting := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomization(Value: Boolean);
begin
  if FItemsQuickCustomization <> Value then
  begin
    FItemsQuickCustomization := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomizationMaxDropDownCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FItemsQuickCustomizationMaxDropDownCount <> Value then
  begin
    FItemsQuickCustomizationMaxDropDownCount := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomizationMultiColumnMode(Value: Boolean);
begin
  if FItemsQuickCustomizationMultiColumnMode <> Value then
  begin
    FItemsQuickCustomizationMultiColumnMode := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
begin
  if FItemsQuickCustomizationReordering <> Value then
  begin
    FItemsQuickCustomizationReordering := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomizationShowCommands(Value: Boolean);
begin
  FItemsQuickCustomizationShowCommands := Value;
end;

procedure TcxCustomGridTableOptionsCustomize.SetItemsQuickCustomizationSorted(Value: Boolean);
begin
  FItemsQuickCustomizationSorted := Value;
end;

procedure TcxCustomGridTableOptionsCustomize.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableOptionsCustomize then
    with TcxCustomGridTableOptionsCustomize(Source) do
    begin
      Self.ItemFiltering := ItemFiltering;
      Self.ItemGrouping := ItemGrouping;
      Self.ItemHiding := ItemHiding;
      Self.ItemMoving := ItemMoving;
      Self.ItemSorting := ItemSorting;
      Self.ItemsQuickCustomization := ItemsQuickCustomization;
      Self.ItemsQuickCustomizationMaxDropDownCount := ItemsQuickCustomizationMaxDropDownCount;
      Self.ItemsQuickCustomizationMultiColumnMode := ItemsQuickCustomizationMultiColumnMode;
      Self.ItemsQuickCustomizationReordering := ItemsQuickCustomizationReordering;
      Self.ItemsQuickCustomizationShowCommands := ItemsQuickCustomizationShowCommands;
      Self.ItemsQuickCustomizationSorted := ItemsQuickCustomizationSorted;
    end;
  inherited;
end;

function TcxCustomGridTableOptionsCustomize.SupportsItemsQuickCustomizationReordering: Boolean;
begin
  Result := not ItemsQuickCustomizationSorted and
    (GridView.IsDesigning or
    (ItemsQuickCustomizationReordering = qcrEnabled) or
    (ItemsQuickCustomizationReordering = qcrDefault) and ItemMoving);
end;

{ TcxCustomGridTableOptionsData }

constructor TcxCustomGridTableOptionsData.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCancelOnExit := True;
  FDeleting := True;
  FDeletingConfirmation := True;
  FEditing := True;
  FInserting := True;
end;

function TcxCustomGridTableOptionsData.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

procedure TcxCustomGridTableOptionsData.SetAppending(Value: Boolean);
begin
  if FAppending <> Value then
  begin
    FAppending := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsData.SetCancelOnExit(Value: Boolean);
begin
  if FCancelOnExit <> Value then
  begin
    FCancelOnExit := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsData.SetDeleting(Value: Boolean);
begin
  if FDeleting <> Value then
  begin
    FDeleting := Value;
    GridView.RefreshNavigators;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsData.SetDeletingConfirmation(Value: Boolean);
begin
  if FDeletingConfirmation <> Value then
  begin
    FDeletingConfirmation := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsData.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not FEditing then
      GridView.Controller.EditingItem := nil;
    GridView.RefreshNavigators;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsData.SetInserting(Value: Boolean);
begin
  if FInserting <> Value then
  begin
    FInserting := Value;
    GridView.RefreshNavigators;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsData.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableOptionsData then
    with TcxCustomGridTableOptionsData(Source) do
    begin
      Self.Appending := Appending;
      Self.CancelOnExit := CancelOnExit;
      Self.Deleting := Deleting;
      Self.DeletingConfirmation := DeletingConfirmation;
      Self.Editing := Editing;
      Self.Inserting := Inserting;
    end;
  inherited;
end;

{ TcxCustomGridTableOptionsSelection }

constructor TcxCustomGridTableOptionsSelection.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FCellSelect := True;
  FHideFocusRectOnExit := True;
  FInvertSelect := True;
  FUnselectFocusedRecordOnExit := True;
end;

function TcxCustomGridTableOptionsSelection.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

function TcxCustomGridTableOptionsSelection.GetMultiSelect: Boolean;
begin
  Result := GridView.DataController.MultiSelect;
end;

procedure TcxCustomGridTableOptionsSelection.SetHideFocusRectOnExit(Value: Boolean);
begin
  if FHideFocusRectOnExit <> Value then
  begin
    FHideFocusRectOnExit := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsSelection.SetHideSelection(Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsSelection.SetUnselectFocusedRecordOnExit(Value: Boolean);
begin
  if FUnselectFocusedRecordOnExit <> Value then
  begin
    FUnselectFocusedRecordOnExit := Value;
    Changed(vcProperty);
  end;
end;

function TcxCustomGridTableOptionsSelection.IsMultiSelectPersistent: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridTableOptionsSelection.SetCellSelect(Value: Boolean);
begin
  if FCellSelect <> Value then
  begin
    FCellSelect := Value;
    if FCellSelect then
      GridView.Controller.FocusFirstAvailableItem
    else
      GridView.Controller.FocusedItem := nil;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsSelection.SetInvertSelect(Value: Boolean);
begin
  if FInvertSelect <> Value then
  begin
    FInvertSelect := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsSelection.SetMultiSelect(Value: Boolean);
begin
  if MultiSelect <> Value then
  begin
    GridView.DataController.MultiSelect := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsSelection.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableOptionsSelection then
    with TcxCustomGridTableOptionsSelection(Source) do
    begin
      Self.CellSelect := CellSelect;
      Self.HideFocusRectOnExit := HideFocusRectOnExit;
      Self.HideSelection := HideSelection;
      Self.InvertSelect := InvertSelect;
      Self.MultiSelect := MultiSelect;
      Self.UnselectFocusedRecordOnExit := UnselectFocusedRecordOnExit;
    end;
  inherited;
end;

{ TcxCustomGridTableOptionsView }

constructor TcxCustomGridTableOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FEditAutoHeightBorderColor := clDefault;
  FFocusRect := True;
  FItemFilterButtonShowMode := fbmDefault;
  FNavigatorOffset := cxGridNavigatorDefaultOffset;
  FNoDataToDisplayInfoText := scxGridNoDataInfoText;
  FShowItemFilterButtons := sfbDefault;
end;

function TcxCustomGridTableOptionsView.GetGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GridView);
end;

procedure TcxCustomGridTableOptionsView.ReadNavigator(Reader: TReader);
begin
  GridView.Navigator.Visible := Reader.ReadBoolean;
end;

procedure TcxCustomGridTableOptionsView.SetCellAutoHeight(Value: Boolean);
begin
  if FCellAutoHeight <> Value then
  begin
    FCellAutoHeight := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetCellEndEllipsis(Value: Boolean);
begin
  if FCellEndEllipsis <> Value then
  begin
    FCellEndEllipsis := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetCellTextMaxLineCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCellTextMaxLineCount <> Value then
  begin
    FCellTextMaxLineCount := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetEditAutoHeightBorderColor(Value: TColor);
begin
  if FEditAutoHeightBorderColor <> Value then
  begin
    FEditAutoHeightBorderColor := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetFocusRect(Value: Boolean);
begin
  if FFocusRect <> Value then
  begin
    FFocusRect := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetItemCaptionAutoHeight(Value: Boolean);
begin
  if FItemCaptionAutoHeight <> Value then
  begin
    FItemCaptionAutoHeight := Value;
    ItemCaptionAutoHeightChanged;
  end;
end;

procedure TcxCustomGridTableOptionsView.SetItemCaptionEndEllipsis(Value: Boolean);
begin
  if FItemCaptionEndEllipsis <> Value then
  begin
    FItemCaptionEndEllipsis := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetItemFilterButtonShowMode(
  Value: TcxGridItemFilterButtonShowMode);
begin
  if FItemFilterButtonShowMode <> Value then
  begin
    FItemFilterButtonShowMode := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetNavigatorOffset(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FNavigatorOffset <> Value then
  begin
    FNavigatorOffset := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetNoDataToDisplayInfoText(const Value: string);
begin
  if FNoDataToDisplayInfoText <> Value then
  begin
    FNoDataToDisplayInfoText := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetShowEditButtons(Value: TcxGridShowEditButtons);
begin
  if FShowEditButtons <> Value then
  begin
    FShowEditButtons := Value;
    Changed(vcSize);
  end;
end;

procedure TcxCustomGridTableOptionsView.SetShowItemFilterButtons(Value: TcxGridShowItemFilterButtons);
begin
  if FShowItemFilterButtons <> Value then
  begin
    FShowItemFilterButtons := Value;
    Changed(vcSize);
  end;
end;

function TcxCustomGridTableOptionsView.IsNoDataToDisplayInfoTextAssigned: Boolean;
begin
  Result := FNoDataToDisplayInfoText <> scxGridNoDataInfoText;
end;

procedure TcxCustomGridTableOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  NavigatorOffset := MulDiv(NavigatorOffset, M, D);
end;

procedure TcxCustomGridTableOptionsView.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Navigator', ReadNavigator, nil, False);
end;

procedure TcxCustomGridTableOptionsView.ItemCaptionAutoHeightChanged;
begin
  Changed(vcSize);
end;

procedure TcxCustomGridTableOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridTableOptionsView then
    with TcxCustomGridTableOptionsView(Source) do
    begin
      Self.CellAutoHeight := CellAutoHeight;
      Self.CellEndEllipsis := CellEndEllipsis;
      Self.CellTextMaxLineCount := CellTextMaxLineCount;
      Self.EditAutoHeightBorderColor := EditAutoHeightBorderColor;
      Self.FocusRect := FocusRect;
      Self.ItemCaptionAutoHeight := ItemCaptionAutoHeight;
      Self.ItemCaptionEndEllipsis := ItemCaptionEndEllipsis;
      Self.ItemFilterButtonShowMode := ItemFilterButtonShowMode;
      Self.NavigatorOffset := NavigatorOffset;
      Self.NoDataToDisplayInfoText := NoDataToDisplayInfoText;
      Self.ShowEditButtons := ShowEditButtons;
      Self.ShowItemFilterButtons := ShowItemFilterButtons;
    end;
  inherited;
end;

function TcxCustomGridTableOptionsView.GetGridLineColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultGridLineColor;
end;

function TcxCustomGridTableOptionsView.GetNoDataToDisplayInfoText: string;
begin
  if IsNoDataToDisplayInfoTextAssigned then
    Result := FNoDataToDisplayInfoText
  else
    Result := cxGetResourceString(@scxGridNoDataInfoText);
end;

{ TcxGridCellPos }

constructor TcxGridCellPos.Create(AGridRecord: TcxCustomGridRecord; AItem: TObject);
begin
  inherited Create;
  GridRecord := AGridRecord;
  Item := AItem;
end;

{ TcxGridDataCellPos }

constructor TcxGridDataCellPos.Create(AGridRecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem);
begin
  inherited Create;
  GridRecord := AGridRecord;
  Item := AItem;
end;

{ TcxCustomGridTableViewStyles }

constructor TcxCustomGridTableViewStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FUseOddEvenStyles := bDefault;
end;

procedure TcxCustomGridTableViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxCustomGridTableViewStyles then
    with TcxCustomGridTableViewStyles(Source) do
    begin
      Self.Content := Content;
      Self.ContentEven := ContentEven;
      Self.ContentOdd := ContentOdd;
      Self.FilterBox := FilterBox;
      Self.FindPanel := FindPanel;
      Self.SearchResultHighlight := SearchResultHighlight;
      Self.Inactive := Inactive;
      Self.IncSearch := IncSearch;
      Self.Navigator := Navigator;
      Self.NavigatorInfoPanel := NavigatorInfoPanel;
      Self.Selection := Selection;
      Self.OnGetContentStyle := OnGetContentStyle;
      Self.FUseOddEvenStyles := UseOddEvenStyles;
    end;
end;

function TcxCustomGridTableViewStyles.ActuallyUseOddEvenStyles: Boolean;
begin
  Result := ((UseOddEvenStyles = bDefault) and (Assigned(ContentOdd) or Assigned(ContentEven) or
    LookAndFeelPainter.GridLikeControlDefaultUseOddEvenStyle)) or (UseOddEvenStyles = bTrue);
end;

procedure TcxCustomGridTableViewStyles.GetConditionalFormattingContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AParams: TcxViewParams);
begin
  if (ARecord <> nil) and (AItem <> nil) and GridView.HasConditionalFormattingProvider then
    with GridView.ConditionalFormattingProvider do
      CalculateViewParams(AParams, ARecord.RecordIndex, FindFieldByItem(AItem));
end;

function TcxCustomGridTableViewStyles.GetGridViewValue: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited GetGridView);
end;

procedure TcxCustomGridTableViewStyles.SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
begin
  if AValue <> FUseOddEvenStyles then
  begin
    FUseOddEvenStyles := AValue;
    Changed(vsContent);
  end;
end;

procedure TcxCustomGridTableViewStyles.SetOnGetContentStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetContentStyle, Value) then
  begin
    FOnGetContentStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableViewStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
const
  StyleIndexes: array[Boolean] of Integer = (vsContentEven, vsContentOdd);
begin
  inherited;
  if ((Index = vsContentEven) or (Index = vsContentOdd)) and not ActuallyUseOddEvenStyles then
    Index := vsContent;

  with AParams, LookAndFeelPainter do
    case Index of
      vsContent:
        begin
          if (AData is TcxCustomGridRecord) and ActuallyUseOddEvenStyles then
            GetDefaultViewParams(StyleIndexes[Odd(TcxCustomGridRecord(AData).Index)],
              AData, AParams)
          else
          begin
            Color := GridLikeControlContentColor;
            TextColor := GridLikeControlContentTextColor;
          end;
        end;
      vsContentEven:
        begin
          Color := GridLikeControlContentEvenColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      vsContentOdd:
        begin
          Color := GridLikeControlContentOddColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      vsFilterBox:
        begin
          Color := DefaultFilterBoxColor;
          TextColor := DefaultFilterBoxTextColor;
        end;
      vsFindPanel:
        begin
          Color := DefaultFilterBoxColor;
          TextColor := DefaultContentTextColor;
        end;
      vsInactive:
        begin
          if AData <> nil then
            with TcxGridCellPos(AData) do
              GetCellContentParams(GridRecord, Item, AParams);
          Color := DefaultInactiveColor;
          TextColor := DefaultInactiveTextColor;
        end;
      vsIncSearch, vsSearchResultHighlight:
        begin
          Color := clDefault;
          TextColor := clDefault;
        end;
      vsNavigator:
        begin
          Color := DefaultGridDetailsSiteColor;
          TextColor := DefaultHeaderTextColor;
        end;
      vsNavigatorInfoPanel:
        begin
          Color := GridLikeControlContentColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      vsSelection:
        begin
          if AData <> nil then
            with TcxGridCellPos(AData) do
              GetCellContentParams(GridRecord, Item, AParams);
          Color := DefaultSelectionColor;
          TextColor := DefaultSelectionTextColor;
        end;
    end;
end;

procedure TcxCustomGridTableViewStyles.GetSelectionParams(ARecord: TcxCustomGridRecord;
  AItem: TObject; out AParams: TcxViewParams);
var
  ACellPos: TcxGridCellPos;
begin
  if ARecord <> nil then
    ACellPos := TcxGridCellPos.Create(ARecord, AItem)
  else
    ACellPos := nil;
  try
    if GridView.DrawRecordActive(ARecord) then
      GetViewParams(vsSelection, ACellPos, nil, AParams)
    else
      GetViewParams(vsInactive, ACellPos, nil, AParams);
  finally
    ACellPos.Free;
  end;
  AParams.Bitmap := nil;  // to use selection color even if [content] bitmap is assigned
end;

procedure TcxCustomGridTableViewStyles.GetCellContentParams(ARecord: TcxCustomGridRecord;
  AItem: TObject; out AParams: TcxViewParams);
begin
  if (AItem = nil) or (AItem is TcxCustomGridTableItem) then
    GetDataCellContentParams(ARecord, TcxCustomGridTableItem(AItem), AParams);
end;

procedure TcxCustomGridTableViewStyles.GetContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
const
  StyleIndexes: array[Boolean] of Integer = (vsContentEven, vsContentOdd);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetContentStyle) then
    FOnGetContentStyle(GridView, ARecord, AItem, AStyle);
  if (ARecord <> nil) and ActuallyUseOddEvenStyles and (GetValue(StyleIndexes[Odd(ARecord.Index)]) <> nil) then
  begin
    ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
    try
      GetViewParams(StyleIndexes[Odd(ARecord.Index)], ADataCellPos, AStyle, AParams);
    finally
      ADataCellPos.Free;
    end;
  end
  else
    GetViewParams(vsContent, ARecord, AStyle, AParams);
end;

procedure TcxCustomGridTableViewStyles.GetDataCellContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if AItem = nil then
    GetRecordContentParams(ARecord, AItem, AParams)
  else
    AItem.Styles.GetContentParams(ARecord, AParams);
end;

procedure TcxCustomGridTableViewStyles.GetDataCellParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams;
  AUseViewInfo: Boolean = False; ACellViewInfo: TcxGridTableCellViewInfo = nil;
  AIgnoreSelection: Boolean = False);
begin
  if not AIgnoreSelection and
    (AUseViewInfo and ACellViewInfo.Selected or
     not AUseViewInfo and GridView.DrawDataCellSelected(ARecord, AItem, False, nil)) then
    GetSelectionParams(ARecord, AItem, AParams)
  else
    GetDataCellContentParams(ARecord, AItem, AParams);
end;

procedure TcxCustomGridTableViewStyles.GetRecordContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  GetContentParams(ARecord, AItem, AParams);
end;

{ TcxGridConditionalFormattingProvider }

constructor TcxGridConditionalFormattingProvider.Create(AOwner: TcxCustomGridTableView);
begin
  inherited Create(AOwner.DataController);
  FOwner := AOwner;
  SynchronizeMasterClasses;
end;

procedure TcxGridConditionalFormattingProvider.SynchronizeMasterClasses;
begin
// do nothing
end;

procedure TcxGridConditionalFormattingProvider.Changed;
begin
  inherited Changed;
  FOwner.LayoutChanged;
end;

procedure TcxGridConditionalFormattingProvider.ConditionalFormattingChanged;
var
  I: Integer;
begin
  inherited ConditionalFormattingChanged;
  if not IsDetailMode then
  begin
    for I := 0 to FOwner.CloneCount - 1 do
      TcxCustomGridTableView(FOwner.Clones[I]).ConditionalFormattingProvider.ConditionalFormattingChanged;
  end;
end;

procedure TcxGridConditionalFormattingProvider.DoBeginUpdate;
begin
  FOwner.BeginUpdate(lsimImmediate);
end;

procedure TcxGridConditionalFormattingProvider.DoEndUpdate;
begin
  FOwner.EndUpdate;
end;

function TcxGridConditionalFormattingProvider.GetCellBounds(
  ACellViewInfo: TObject): TRect;
var
  ACell: TcxGridTableDataCellViewInfo absolute ACellViewInfo;
  R: TRect;
begin
  Result := ACell.EditViewDataBounds;
  ACell.GetRealEditViewDataContentOffsets(R);
  Result := cxRectContent(Result, R);
end;

function TcxGridConditionalFormattingProvider.GetDisplayName(
  AField: TcxCustomDataField): string;
begin
  if (AField.Item <> nil) and (AField.Item is TcxCustomGridTableItem) then
    Result := TcxCustomGridTableItem(AField.Item).Caption
  else
    Result := '';
end;

function TcxGridConditionalFormattingProvider.GetFocusedItem: TObject;
begin
  Result := FOwner.Controller.FocusedItem;
end;

function TcxGridConditionalFormattingProvider.GetEditProperties(AItem: TObject): TcxCustomEditProperties;
var
  ATableItem: TcxCustomGridTableItem absolute AItem;
begin
  if (AItem <> nil) and (AItem is TcxCustomGridTableItem) then
    Result := TcxCustomGridTableItem(AItem).GetProperties
  else
    Result := nil;
end;

function TcxGridConditionalFormattingProvider.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.LookAndFeel;
end;

function TcxGridConditionalFormattingProvider.GetOwner: TComponent;
begin
  Result := FOwner;
end;

function TcxGridConditionalFormattingProvider.DoGetParentForm: TCustomForm;
begin
  if FOwner.Site = nil then
    Result := nil
  else
    Result := Forms.GetParentForm(FOwner.Site);
end;

function TcxGridConditionalFormattingProvider.EditCellViewInfoToPoint(
  ACellViewInfo: TObject): TPoint;
var
  ACell: TcxGridTableDataCellViewInfo absolute ACellViewInfo;
begin
  Result := TPoint.Create(FindFieldByItem(ACell.Item), ACell.GridRecord.RecordIndex);
end;

function TcxGridConditionalFormattingProvider.GetScaleFactor: TdxScaleFactor;
begin
  Result := FOwner.ScaleFactor;
end;

function TcxGridConditionalFormattingProvider.IsFieldVisible(
  AField: TcxCustomDataField): Boolean;
begin
  Result := (AField.Item <> nil) and (AField.Item is TcxCustomGridTableItem) and
    (TcxCustomGridTableItem(AField.Item).Visible or TcxCustomGridTableItem(AField.Item).VisibleForCustomization);
end;

function TcxGridConditionalFormattingProvider.IsRightToLeft: Boolean;
begin
  Result := SysLocale.MiddleEast and (FOwner.Site <> nil) and (FOwner.Site.BiDiMode <> bdLeftToRight);
end;

function TcxGridConditionalFormattingProvider.IsRowVisible(const ARow: Integer): Boolean;
begin
  Result := (ARow >= FOwner.Controller.TopRecordIndex) and
    (ARow <= FOwner.Controller.LastVisibleRecordIndex);
end;

{ TcxGridDetailModeConditionalFormattingProvider }

destructor TcxGridDetailModeConditionalFormattingProvider.Destroy;
begin
  FreeAndNil(FCustomAreaInfoCache);
  inherited Destroy;
end;

function TcxGridDetailModeConditionalFormattingProvider.IsDetailMode: Boolean;
begin
  Result := True;
end;

function TcxGridDetailModeConditionalFormattingProvider.CalculateStyle(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow: Integer; AColumn: Integer;
  ACell: IdxSpreadSheetCellData): Boolean;
var
  AConditionalFormatting: TcxDataControllerConditionalFormattingAccess;
  APrevProvider: TcxDataControllerConditionalFormattingProvider;
  APrevAreaInfoCache: TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>;
begin
  AConditionalFormatting := TcxDataControllerConditionalFormattingAccess(ConditionalFormatting);
  APrevProvider := AConditionalFormatting.Provider;
  APrevAreaInfoCache := AConditionalFormatting.FAreaInfoCache;
  AConditionalFormatting.SetCustomAreaInfoCache(FCustomAreaInfoCache);
  AConditionalFormatting.SetCustomProvider(Self);
  try
    Result := inherited CalculateStyle(AStyle, ARow, AColumn, ACell);
  finally
    AConditionalFormatting.SetCustomAreaInfoCache(APrevAreaInfoCache);
    AConditionalFormatting.SetCustomProvider(APrevProvider);
  end;
end;

procedure TcxGridDetailModeConditionalFormattingProvider.ClearCacheCore;
begin
  inherited ClearCacheCore;
  FCustomAreaInfoCache.Clear;
end;

procedure TcxGridDetailModeConditionalFormattingProvider.SynchronizeMasterClasses;
begin
  FCellStyles := Owner.PatternGridView.ConditionalFormattingProvider.CellStyles;
  FFormulaController := Owner.PatternGridView.ConditionalFormattingProvider.FormulaController;
  FConditionalFormatting := Owner.PatternGridView.ConditionalFormattingProvider.ConditionalFormatting;
  FFormatSettings := Owner.PatternGridView.ConditionalFormattingProvider.FormatSettings;
  FCustomAreaInfoCache := TObjectDictionary<string, TdxSpreadSheetConditionalFormattingAreaInfo>.Create([doOwnsValues]);
end;

{ TcxGridOpenTableItemList }

function TcxGridOpenTableItemList.GetItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(inherited Items[Index]);
end;

procedure TcxGridOpenTableItemList.SetItem(Index: Integer; Value: TcxCustomGridTableItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxCustomGridTableView }

destructor TcxCustomGridTableView.Destroy;
begin
  Controller.EditingController.EditingItem := nil;
  inherited;
end;

function TcxCustomGridTableView.GetBackgroundBitmaps: TcxCustomGridTableBackgroundBitmaps;
begin
  Result := TcxCustomGridTableBackgroundBitmaps(inherited BackgroundBitmaps);
end;

function TcxCustomGridTableView.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := ConditionalFormattingProvider.ConditionalFormatting;
end;

function TcxCustomGridTableView.GetController: TcxCustomGridTableController;
begin
  Result := TcxCustomGridTableController(inherited Controller);
end;

function TcxCustomGridTableView.GetFilterableItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(FFilterableItems[Index]);
end;

function TcxCustomGridTableView.GetFilterableItemCount: Integer;
begin
  Result := FFilterableItems.Count;
end;

function TcxCustomGridTableView.GetGroupedItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := Items[DataController.Groups.GroupingItemIndex[Index]];
end;

function TcxCustomGridTableView.GetGroupedItemCount: Integer;
begin
  Result := DataController.Groups.GroupingItemCount;
end;

function TcxCustomGridTableView.GetItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(FItems[Index]);
end;

function TcxCustomGridTableView.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxCustomGridTableView.GetMasterGridRecord: TcxCustomGridRecord;
begin
  if MasterGridRecordIndex = -1 then
    Result := nil
  else
    if (MasterGridView as TcxCustomGridTableView).ViewData.IsRecordIndexValid(MasterGridRecordIndex) then
      Result := TcxCustomGridTableView(MasterGridView).ViewData.Records[MasterGridRecordIndex]
    else
      Result := nil;
end;

function TcxCustomGridTableView.GetNavigatorButtons: TcxNavigatorControlButtons;
begin
  Result := Navigator.Buttons;
end;

function TcxCustomGridTableView.GetOptionsBehavior: TcxCustomGridTableOptionsBehavior;
begin
  Result := TcxCustomGridTableOptionsBehavior(inherited OptionsBehavior);
end;

function TcxCustomGridTableView.GetOptionsData: TcxCustomGridTableOptionsData;
begin
  Result := TcxCustomGridTableOptionsData(inherited OptionsData);
end;

function TcxCustomGridTableView.GetOptionsSelection: TcxCustomGridTableOptionsSelection;
begin
  Result := TcxCustomGridTableOptionsSelection(inherited OptionsSelection);
end;

function TcxCustomGridTableView.GetOptionsView: TcxCustomGridTableOptionsView;
begin
  Result := TcxCustomGridTableOptionsView(inherited OptionsView);
end;

function TcxCustomGridTableView.GetPainter: TcxCustomGridTablePainter;
begin
  Result := TcxCustomGridTablePainter(inherited Painter);
end;

function TcxCustomGridTableView.GetPatternGridView: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(inherited PatternGridView);
end;

function TcxCustomGridTableView.GetNavigatorButtonsControl: IcxNavigator;
begin
  Result := Self;
end;

function TcxCustomGridTableView.GetSortedItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := Items[DataController.GetSortingItemIndex(Index)];
end;

function TcxCustomGridTableView.GetSortedItemCount: Integer;
begin
  Result := DataController.GetSortingItemCount;
end;

function TcxCustomGridTableView.GetStyles: TcxCustomGridTableViewStyles;
begin
  Result := TcxCustomGridTableViewStyles(inherited Styles);
end;

function TcxCustomGridTableView.GetViewData: TcxCustomGridTableViewData;
begin
  Result := TcxCustomGridTableViewData(inherited ViewData);
end;

function TcxCustomGridTableView.GetViewInfo: TcxCustomGridTableViewInfo;
begin
  Result := TcxCustomGridTableViewInfo(inherited ViewInfo);
end;

function TcxCustomGridTableView.GetVisibleItem(Index: Integer): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(FVisibleItems[Index]);
end;

function TcxCustomGridTableView.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

procedure TcxCustomGridTableView.SetBackgroundBitmaps(Value: TcxCustomGridTableBackgroundBitmaps);
begin
  inherited BackgroundBitmaps := Value;
end;

procedure TcxCustomGridTableView.SetDateTimeHandling(Value: TcxCustomGridTableDateTimeHandling);
begin
  FDateTimeHandling.Assign(Value);
end;

procedure TcxCustomGridTableView.SetFilterBox(Value: TcxGridFilterBox);
begin
  FFilterBox.Assign(Value);
end;

procedure TcxCustomGridTableView.SetFiltering(Value: TcxCustomGridTableFiltering);
begin
  FFiltering.Assign(Value);
end;

procedure TcxCustomGridTableView.SetFindPanel(AValue: TcxGridFindPanelOptions);
begin
  FFindPanel.Assign(AValue);
end;

procedure TcxCustomGridTableView.SetItem(Index: Integer; Value: TcxCustomGridTableItem);
begin
  Items[Index].Assign(Value);
end;

procedure TcxCustomGridTableView.SetNavigator(Value: TcxGridViewNavigator);
begin
  FNavigator.Assign(Value);
end;

procedure TcxCustomGridTableView.SetNavigatorButtons(Value: TcxNavigatorControlButtons);
begin
  FNavigator.Buttons.Assign(Value);
end;

procedure TcxCustomGridTableView.SetOnCanFocusRecord(Value: TcxGridAllowRecordOperationEvent);
begin
  if not dxSameMethods(FOnCanFocusRecord, Value) then
  begin
    FOnCanFocusRecord := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnCanSelectRecord(Value: TcxGridAllowRecordOperationEvent);
begin
  if not dxSameMethods(FOnCanSelectRecord, Value) then
  begin
    FOnCanSelectRecord := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnCellClick(Value: TcxGridCellClickEvent);
begin
  if not dxSameMethods(FOnCellClick, Value) then
  begin
    FOnCellClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnCellDblClick(Value: TcxGridCellClickEvent);
begin
  if not dxSameMethods(FOnCellDblClick, Value) then
  begin
    FOnCellDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnCustomDrawCell(Value: TcxGridTableDataCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawCell, Value) then
  begin
    FOnCustomDrawCell := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnCustomDrawPartBackground(Value: TcxGridPartCustomDrawBackgroundEvent);
begin
  if not dxSameMethods(FOnCustomDrawPartBackground, Value) then
  begin
    FOnCustomDrawPartBackground := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditChanged(Value: TcxGridCustomTableItemEvent);
begin
  if not dxSameMethods(FOnEditChanged, Value) then
  begin
    FOnEditChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditDblClick(Value: TcxGridEditDblClickEvent);
begin
  if not dxSameMethods(FOnEditDblClick, Value) then
  begin
    FOnEditDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditing(Value: TcxGridEditingEvent);
begin
  if not dxSameMethods(FOnEditing, Value) then
  begin
    FOnEditing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditKeyDown(Value: TcxGridEditKeyEvent);
begin
  if not dxSameMethods(FOnEditKeyDown, Value) then
  begin
    FOnEditKeyDown := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditKeyPress(Value: TcxGridEditKeyPressEvent);
begin
  if not dxSameMethods(FOnEditKeyPress, Value) then
  begin
    FOnEditKeyPress := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditKeyUp(Value: TcxGridEditKeyEvent);
begin
  if not dxSameMethods(FOnEditKeyUp, Value) then
  begin
    FOnEditKeyUp := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnEditValueChanged(Value: TcxGridCustomTableItemEvent);
begin
  if not dxSameMethods(FOnEditValueChanged, Value) then
  begin
    FOnEditValueChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFilterControlDialogShow(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnFilterControlDialogShow, Value) then
  begin
    FOnFilterControlDialogShow := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFilterCustomization(Value: TcxGridFilterCustomizationEvent);
begin
  if not dxSameMethods(FOnFilterCustomization, Value) then
  begin
    FOnFilterCustomization := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFilterDialogShow(Value: TcxGridFilterDialogShowEvent);
begin
  if not dxSameMethods(FOnFilterDialogShow, Value) then
  begin
    FOnFilterDialogShow := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFindPanelVisibilityChanged(Value: TcxGridFindPanelVisibilityChangedEvent);
begin
  if not dxSameMethods(FOnFilterDialogShow, Value) then
  begin
    FOnFindPanelVisibilityChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFocusedItemChanged(Value: TcxGridFocusedItemChangedEvent);
begin
  if not dxSameMethods(FOnFocusedItemChanged, Value) then
  begin
    FOnFocusedItemChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnFocusedRecordChanged(Value: TcxGridFocusedRecordChangedEvent);
begin
  if not dxSameMethods(FOnFocusedRecordChanged, Value) then
  begin
    FOnFocusedRecordChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnGetCellHeight(Value: TcxGridGetCellHeightEvent);
begin
  if not dxSameMethods(FOnGetCellHeight, Value) then
  begin
    FOnGetCellHeight := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnGetDragDropText(Value: TcxGridGetDragDropTextEvent);
begin
  if not dxSameMethods(FOnGetDragDropText, Value) then
  begin
    FOnGetDragDropText := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnInitFilteringDateRanges(Value: TcxGridInitDateRangesEvent);
begin
  if not dxSameMethods(FOnInitFilteringDateRanges, Value) then
  begin
    FOnInitFilteringDateRanges := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnInitGroupingDateRanges(Value: TcxGridInitDateRangesEvent);
begin
  if not dxSameMethods(FOnInitGroupingDateRanges, Value) then
  begin
    FOnInitGroupingDateRanges := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnInitEdit(Value: TcxGridInitEditEvent);
begin
  if not dxSameMethods(FOnInitEdit, Value) then
  begin
    FOnInitEdit := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnInitEditValue(Value: TcxGridInitEditValueEvent);
begin
  if not dxSameMethods(FOnInitEditValue, Value) then
  begin
    FOnInitEditValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnSelectionChanged(Value: TcxGridCustomTableViewEvent);
begin
  if not dxSameMethods(FOnSelectionChanged, Value) then
  begin
    FOnSelectionChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnTopRecordIndexChanged(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnTopRecordIndexChanged, Value) then
  begin
    FOnTopRecordIndexChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOnUpdateEdit(Value: TcxGridInitEditEvent);
begin
  if not dxSameMethods(FOnUpdateEdit, Value) then
  begin
    FOnUpdateEdit := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridTableView.SetOptionsBehavior(Value: TcxCustomGridTableOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxCustomGridTableView.SetOptionsCustomize(Value: TcxCustomGridTableOptionsCustomize);
begin
  FOptionsCustomize.Assign(Value);
end;

procedure TcxCustomGridTableView.SetOptionsData(Value: TcxCustomGridTableOptionsData);
begin
  inherited OptionsData := Value;
end;

procedure TcxCustomGridTableView.SetOptionsSelection(Value: TcxCustomGridTableOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TcxCustomGridTableView.SetOptionsView(Value: TcxCustomGridTableOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxCustomGridTableView.SetStyles(Value: TcxCustomGridTableViewStyles);
begin
  inherited Styles := Value;
end;

procedure TcxCustomGridTableView.CopyForEachRowProc(ARowIndex: Integer; ARowInfo: TcxRowInfo);
var
  I, AIndex: Integer;
begin
  for I := 0 to FCopyToClipboardItems.Count - 1 do
  begin
    AIndex := TcxCustomGridTableItem(FCopyToClipboardItems[I]).Index;
    FCopyToClipboardStr := FCopyToClipboardStr +
      DataController.GetRowDisplayText(ARowInfo, AIndex) + ColumnSeparator;
    if ARowInfo.Level < DataController.Groups.LevelCount then  // it's a group row
      Break;
  end;
  if FCopyToClipboardItems.Count <> 0 then
    FCopyToClipboardStr := Copy(FCopyToClipboardStr, 1,
      Length(FCopyToClipboardStr) - Length(ColumnSeparator));
  FCopyToClipboardStr := FCopyToClipboardStr + dxCRLF;
end;

function TcxCustomGridTableView.HasConditionalFormattingProvider: Boolean;
begin
  Result := (FConditionalFormattingProvider <> nil) or
    ((PatternGridView <> Self) and PatternGridView.HasConditionalFormattingProvider);
end;

procedure TcxCustomGridTableView.RefreshItemIndexes;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].FIndex := I;
end;

procedure TcxCustomGridTableView.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  ConditionalFormattingProvider.DefineBinaryProperty(Filer);
end;

function TcxCustomGridTableView.GetFilterCaption(Index: Integer): string;
begin
  Result := FilterableItems[Index].FilterCaption;
end;

function TcxCustomGridTableView.GetFilterCount: Integer;
begin
  Result := FilterableItemCount;
end;

function TcxCustomGridTableView.GetFilterCriteria: TcxFilterCriteria;
begin
  Result := DataController.Filter;
end;

function TcxCustomGridTableView.GetFilterFieldName(Index: Integer): string;
begin
  Result := FilterableItems[Index].DataBinding.FilterFieldName;
end;

function TcxCustomGridTableView.GetFilterItemLink(Index: Integer): TObject;
begin
  Result := FilterableItems[Index];
end;

function TcxCustomGridTableView.GetFilterItemLinkID(Index: Integer): Integer;
begin
  Result := FilterableItems[Index].ID;
end;

function TcxCustomGridTableView.GetFilterItemLinkName(Index: Integer): string;
begin
  Result := FilterableItems[Index].Name;
end;

function TcxCustomGridTableView.GetFilterProperties(Index: Integer): TcxCustomEditProperties;
begin
  Result := FilterableItems[Index].GetProperties;
end;

function TcxCustomGridTableView.GetFilterValueType(Index: Integer): cxDataStorage.TcxValueTypeClass;
begin
  Result := FilterableItems[Index].DataBinding.ValueTypeClass;
end;

function TcxCustomGridTableView.NavigatorIsActive: Boolean;
begin
  Result := DataController.Active;
end;

function TcxCustomGridTableView.NavigatorIsBof: Boolean;
var
  AIcxGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) then
    Result := AIcxGridDataController.GetNavigatorIsBof
  else
    Result := False;
end;

function TcxCustomGridTableView.NavigatorIsEof: Boolean;
var
  AIcxGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIcxGridDataController) then
    Result := AIcxGridDataController.GetNavigatorIsEof
  else
    Result := False;
end;

function TcxCustomGridTableView.NavigatorCanAppend: Boolean;
begin
  Result := Controller.CanAppend(True);
end;

function TcxCustomGridTableView.NavigatorCanEdit: Boolean;
begin
  Result := Controller.CanEdit;
end;

function TcxCustomGridTableView.NavigatorCanDelete: Boolean;
begin
  Result := Controller.CanDelete(True);
end;

function TcxCustomGridTableView.NavigatorCanInsert: Boolean;
begin
  Result := Controller.CanInsert(True);
end;

function TcxCustomGridTableView.NavigatorIsEditing: Boolean;
begin
  Result := DataController.IsEditing;
end;

procedure TcxCustomGridTableView.NavigatorClearBookmark;
begin
  DataController.ClearBookmark;
end;

function TcxCustomGridTableView.NavigatorIsBookmarkAvailable: Boolean;
begin
  Result := DataController.IsBookmarkAvailable;
end;

procedure TcxCustomGridTableView.NavigatorDoAction(AButtonIndex: Integer);
var
  APrevAllowAppendRecord: Boolean;
begin
  case AButtonIndex of
    NBDI_FIRST:
      Controller.GoToFirst(not Controller.IsMultiSelectPersistent);
    NBDI_PRIORPAGE:
      Controller.FocusPrevPage(not Controller.IsMultiSelectPersistent);
    NBDI_PRIOR:
      Controller.GoToPrev(False, not Controller.IsMultiSelectPersistent);
    NBDI_NEXT:
      begin
        APrevAllowAppendRecord := Controller.AllowAppendRecord;
        Controller.AllowAppendRecord := False;
        try
          Controller.GoToNext(False, not Controller.IsMultiSelectPersistent);
        finally
          Controller.AllowAppendRecord := APrevAllowAppendRecord;
        end;
      end;
    NBDI_NEXTPAGE:
      Controller.FocusNextPage(not Controller.IsMultiSelectPersistent);
    NBDI_LAST:
      Controller.GoToLast(False, not Controller.IsMultiSelectPersistent);
    NBDI_INSERT, NBDI_APPEND:
      Controller.CreateNewRecord(AButtonIndex = NBDI_APPEND);
    NBDI_DELETE:
      Controller.DeleteSelection;
    NBDI_EDIT:
      begin
        DataController.Edit;
        Controller.FocusedItem.Editing := True;
      end;
    NBDI_POST:
      begin
        Controller.EditingController.UpdateValue;
        DataController.Post(True);
      end;
    NBDI_CANCEL:
      DataController.Cancel;
    NBDI_REFRESH:
      DataController.RefreshExternalData;
    NBDI_SAVEBOOKMARK:
      DataController.SaveBookmark;
    NBDI_GOTOBOOKMARK:
      DataController.GotoBookmark;
    NBDI_FILTER:
      FFiltering.RunCustomizeDialog;
  end;
end;

function TcxCustomGridTableView.NavigatorGetNotifier: TcxNavigatorControlNotifier;
begin
  Result := FNavigatorNotifier;
end;

function TcxCustomGridTableView.NavigatorIsActionSupported(AButtonIndex: Integer): Boolean;
begin
  Result := True;
end;

function TcxCustomGridTableView.GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
begin
  Result := ConditionalFormattingProvider;
end;

function TcxCustomGridTableView.NavigatorGetRecordCount: Integer;
begin
  Result := DataController.DataRowCount;
end;

function TcxCustomGridTableView.NavigatorGetRecordIndex: Integer;
begin
  Result := DataController.FocusedDataRowIndex;
end;

function TcxCustomGridTableView.GetProperties(AProperties: TStrings): Boolean;

  procedure AddFilter;
  var
    I: Integer;
  begin
    with AProperties do
    begin
      Add('FilterActive');
      Add('Filter');
      if Filtering.MRUItemsList then
        for I := Filtering.MRUItems.Count - 1 downto 0 do
          Add(FilterMRUItemBaseName + IntToStr(I));
    end;
  end;

begin
  if gsoUseFilter in StorageOptions then
    AddFilter;
  Result := inherited GetProperties(AProperties);
end;

procedure TcxCustomGridTableView.GetPropertyValue(const AName: string;
  var AValue: Variant);

  function GetFilter: Boolean;
  var
    AStream: TMemoryStream;
    AIndex: Integer;
  begin
    Result := True;
    if AName = 'FilterActive' then
      AValue := DataController.Filter.Active
    else
      if AName = 'Filter' then
      begin
        AStream := TMemoryStream.Create;
        try
          DataController.Filter.WriteData(AStream);
          AValue := StreamToString(AStream);
        finally
          AStream.Free;
        end;
      end
      else
        if Pos(FilterMRUItemBaseName, AName) = 1 then
        begin
          AIndex := StrToInt(Copy(AName, Length(FilterMRUItemBaseName) + 1, MaxInt));
          AStream := Filtering.MRUItems[AIndex].GetStream;
          try
            AValue := StreamToString(AStream);
          finally
            AStream.Free;
          end;
        end
        else
          Result := False;
  end;

begin
  if (gsoUseFilter in StorageOptions) and GetFilter then Exit;
  inherited;
end;

procedure TcxCustomGridTableView.SetPropertyValue(const AName: string;
  const AValue: Variant);

  function SetFilter: Boolean;
  begin
    Result := True;
    if AName = 'FilterActive' then
    begin
      FRestoringFilterActive := StringToBoolean(AValue);
      Include(FRestoringFilterAttributes, rfaActive);
    end
    else
      if AName = 'Filter' then
      begin
        FRestoringFilterData := dxVariantToAnsiString(AValue);
        Include(FRestoringFilterAttributes, rfaData);
      end
      else
        if Pos(FilterMRUItemBaseName, AName) = 1 then
        begin
          FRestoringFilterMRUItems.Add(AValue);
          Include(FRestoringFilterAttributes, rfaMRUItems);
        end
        else
          Result := False;
  end;

begin
  if (gsoUseFilter in StorageOptions) and SetFilter then Exit;
  inherited;
end;

function TcxCustomGridTableView.CreateStoredObject(const AObjectName, AClassName: string): TObject;

  function GetSummaryGroupIndex: Integer;
  var
    I: Integer;
    AIndex: string;
  begin
    AIndex := '';
    I := Length('SummaryGroup') + 1;
    while AObjectName[I] <> '_' do
    begin
      AIndex := AIndex + AObjectName[I];
      if I = Length(AObjectName) then
        Break;
      Inc(I);
    end;
    if not TryStrToInt(AIndex, Result) then
      Result := -1;
  end;

  function CreateSummary: TObject;
  var
    ASummaryGroupIndex: Integer;
  begin
    Result := nil;
    if Pos('FooterSummaryItem', AObjectName) = 1 then
      Result := DataController.Summary.FooterSummaryItems.Add
    else
      if Pos('DefaultGroupSummaryItem', AObjectName) = 1 then
        Result := DataController.Summary.DefaultGroupSummaryItems.Add
      else
        if Pos('SummaryGroup', AObjectName) = 1 then
        begin
          ASummaryGroupIndex := GetSummaryGroupIndex;
          if ASummaryGroupIndex >= 0 then
            if Pos('Item', AObjectName) <> 0 then
            begin
              if ASummaryGroupIndex < DataController.Summary.SummaryGroups.Count then
                Result := DataController.Summary.SummaryGroups[ASummaryGroupIndex].SummaryItems.Add;
            end
            else
              if Pos('Link', AObjectName) <> 0 then
              begin
                if ASummaryGroupIndex < DataController.Summary.SummaryGroups.Count then
                  Result := DataController.Summary.SummaryGroups[ASummaryGroupIndex].Links.Add;
              end
              else
                Result := DataController.Summary.SummaryGroups.Add;
        end;
  end;

begin
  Result := nil;
  if AClassName = GetItemClass.ClassName then
    Result := CreateItem
  else
    if gsoUseSummary in StorageOptions then
      Result := CreateSummary;
  if Result = nil then
    Result := inherited CreateStoredObject(AObjectName, AClassName);
end;

procedure TcxCustomGridTableView.GetStoredChildren(AChildren: TStringList);
var
  I: Integer;

  procedure AddSummary;
  var
    I, J: Integer;
  begin
    with DataController.Summary.FooterSummaryItems do
      for I := 0 to Count - 1 do
        AChildren.AddObject('FooterSummaryItem' + IntToStr(I), Items[I]);
    with DataController.Summary.DefaultGroupSummaryItems do
      for I := 0 to Count - 1 do
        AChildren.AddObject('DefaultGroupSummaryItem' + IntToStr(I), Items[I]);
    with DataController.Summary.SummaryGroups do
      for I := 0 to Count - 1 do
      begin
        AChildren.AddObject('SummaryGroup' + IntToStr(I), Items[I]);
        with Items[I].SummaryItems do
          for J := 0 to Count - 1 do
            AChildren.AddObject('SummaryGroup' + IntToStr(I) + '_Item' + IntToStr(J), Items[J]);
        with Items[I].Links do
          for J := 0 to Count - 1 do
            AChildren.AddObject('SummaryGroup' + IntToStr(I) + '_Link' + IntToStr(J), Items[J]);
      end;
  end;

begin
  for I := 0 to ItemCount - 1 do
    AChildren.AddObject('', Items[I]);
  if gsoUseSummary in StorageOptions then
    AddSummary;
  AChildren.AddObject('ConditionalFormattingProvider', ConditionalFormattingProvider);
end;

procedure TcxCustomGridTableView.AssignLayout(ALayoutView: TcxCustomGridView);
var
  I: Integer;
  ATag: TcxTag;
  ALayoutItem, AItem: TcxCustomGridTableItem;
begin
  inherited;
  with ALayoutView as TcxCustomGridTableView do
  begin
    Self.BeginAssignItems;
    try
      for I := 0 to ItemCount - 1 do
      begin
        ALayoutItem := Items[I];
        AItem := TcxCustomGridTableItem(ALayoutItem.DataBinding.Data);
        ATag := AItem.Tag;
        AItem.Assign(ALayoutItem);
        AItem.Tag := ATag;
        AItem.Index := ALayoutItem.Index;
      end;
      if GetConditionalFormattingProvider <> nil then
        Self.ConditionalFormatting.Assign(ConditionalFormatting);
    finally
      Self.EndAssignItems;
    end;
{
    Self.DataController.Filter.AssignItems(DataController.Filter);
    Self.DataController.Filter.Active := DataController.Filter.Active;}
  end;
end;

procedure TcxCustomGridTableView.BeforeEditLayout(ALayoutView: TcxCustomGridView);
var
  I: Integer;
begin
  inherited;
  with ALayoutView as TcxCustomGridTableView do
  begin
    with OptionsData do
    begin
      Appending := True;
      Deleting := True;
      Editing := True;
      Inserting := True;
    end;
    OptionsSelection.CellSelect := True;
    Navigator.Visible := True;

    for I := 0 to ItemCount - 1 do
      Items[I].DataBinding.Data := Self.Items[I];
  end;
end;

function TcxCustomGridTableView.GetSystemSizeScrollBars: TcxScrollStyle;
begin
  if Navigator.Visible then
    Result := ssVertical
  else
    Result := ssBoth;
end;

function TcxCustomGridTableView.HasLayoutCustomizationForm: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridTableView.GetAdornerTargetElements(AList: TStrings);
begin
  inherited GetAdornerTargetElements(AList);
  AList.AddObject('FindPanel', ViewInfo.FindPanelViewInfo);
  AList.AddObject('FilterPanel', ViewInfo.FilterViewInfo);
  AList.AddObject('Navigator', ViewInfo.NavigatorSiteViewInfo);
end;

procedure TcxCustomGridTableView.ApplyRestoredFilter;
var
  AStream: TMemoryStream;
  AFilter: TcxDataFilterCriteria;
  I: Integer;
begin
  if rfaActive in FRestoringFilterAttributes then
    DataController.Filter.Active := FRestoringFilterActive;
  if rfaData in FRestoringFilterAttributes then
  begin
    AStream := TMemoryStream.Create;
    try
      StringToStream(FRestoringFilterData, AStream);
      AStream.Position := 0;
      DataController.Filter.ReadData(AStream);
    finally
      AStream.Free;
    end;
  end;
  if rfaMRUItems in FRestoringFilterAttributes then
  begin
    Filtering.MRUItems.BeginUpdate;
    try
      Filtering.MRUItems.AllowDuplicates := True;
      for I := 0 to FRestoringFilterMRUItems.Count - 1 do
      begin
        AFilter := DataController.CreateFilter;
        try
          AStream := TMemoryStream.Create;
          try
            StringToStream(dxVariantToAnsiString(FRestoringFilterMRUItems[I]), AStream);
            AStream.Position := 0;
            AFilter.ReadData(AStream);
            Filtering.MRUItems.Add(AFilter);
          finally
            AStream.Free;
          end;
        finally
          AFilter.Free;
        end;
      end;
    finally
      Filtering.MRUItems.AllowDuplicates := False;
      Filtering.MRUItems.EndUpdate;
    end;
  end;
end;

procedure TcxCustomGridTableView.BeforeRestoring;
begin
  inherited;
  BeginAssignItems;
  FRestoringItems := TcxGridOpenTableItemList.Create;
  FRestoringFilterAttributes := [];
  FRestoringFilterMRUItems := TStringList.Create;
end;

procedure TcxCustomGridTableView.AfterRestoring;
var
  I: Integer;
begin
  ConditionalFormattingProvider.BeginUpdateFieldsOrder;
  try
    for I := 0 to FRestoringItems.Count - 1 do
      if FRestoringItems[I] <> nil then
        FRestoringItems[I].Index := I;
    EndAssignItems;
    ApplyRestoredFilter;
  finally
    FRestoringFilterMRUItems.Free;
    FRestoringItems.Free;
    inherited AfterRestoring;
    ConditionalFormattingProvider.EndUpdateFieldsOrder;
  end;
end;

procedure TcxCustomGridTableView.ReadState(Reader: TReader);
begin
  BeginAssignItems;
  try
    inherited;
  finally
    EndAssignItems;
  end;
end;

procedure TcxCustomGridTableView.Loaded;
begin
  inherited Loaded;
  if HasConditionalFormattingProvider then
    ConditionalFormattingProvider.Loaded;
end;

procedure TcxCustomGridTableView.AssignEditingRecord;
begin
  ViewData.AssignEditingRecord;
end;

procedure TcxCustomGridTableView.BeginAssignItems;
begin
  if not IsAssigningItems then
    DoBeforeAssignItems;
  Inc(FAssigningItemsLockCount);
end;

procedure TcxCustomGridTableView.DoAfterAssignItems;
begin
  FAssigningSortedItems.Free;
  FAssigningGroupedItems.Free;
end;

procedure TcxCustomGridTableView.DoBeforeAssignItems;
begin
  FAssigningGroupedItems := TcxGridOpenTableItemList.Create;
  FAssigningSortedItems := TcxGridOpenTableItemList.Create;
end;

procedure TcxCustomGridTableView.DoItemsAssigned;
var
  I: Integer;
begin
  for I := 0 to FAssigningGroupedItems.Count - 1 do
    if FAssigningGroupedItems[I] <> nil then  // because of inherited forms
      FAssigningGroupedItems[I].GroupIndex := I;
  for I := 0 to FAssigningSortedItems.Count - 1 do
    if FAssigningSortedItems[I] <> nil then  // because of inherited forms
      FAssigningSortedItems[I].SortIndex := I;
end;

procedure TcxCustomGridTableView.DoStylesChanged;
begin
  inherited DoStylesChanged;
  Controller.FindPanel.StylesChanged;
end;

procedure TcxCustomGridTableView.EndAssignItems;
begin
  Dec(FAssigningItemsLockCount);
  if not IsAssigningItems then
  begin
    FIsAfterAssigningItems := True;
    try
      DoItemsAssigned;
    finally
      DoAfterAssignItems;
      FIsAfterAssigningItems := False;
    end;
  end;
end;

function TcxCustomGridTableView.IsAssigningItems: Boolean;
begin
  Result := FAssigningItemsLockCount > 0;
end;

function TcxCustomGridTableView.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean;
begin
  Result := not TcxCustomGrid(Control).UpdateLocked and Visible and
    ViewInfo.CanOffsetView(ARecordCountDelta);
end;

function TcxCustomGridTableView.CanSelectRecord(ARecordIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanSelectRecord) then
    FOnCanSelectRecord(Self, ViewData.GetRecordByIndex(ARecordIndex), Result);
end;

function TcxCustomGridTableView.CanTabStop: Boolean;
begin
  Result := inherited CanTabStop and not Controller.IsEditing;
end;

procedure TcxCustomGridTableView.AfterDestroyingLockedStateImage;
var
  AEdit: TcxCustomEdit;
begin
  AEdit := Controller.EditingController.Edit;
  if (AEdit <> nil) and AEdit.HandleAllocated then
    SendMessage(AEdit.Handle, WM_SETREDRAW, 1, 0);
end;

procedure TcxCustomGridTableView.BeforeCreatingLockedStateImage;
var
  AEdit: TcxCustomEdit;
begin
  AEdit := Controller.EditingController.Edit;
  if (AEdit <> nil) and AEdit.HandleAllocated then
    SendMessage(AEdit.Handle, WM_SETREDRAW, 0, 0);
end;

procedure TcxCustomGridTableView.Deactivate;
begin
  Controller.EditingController.HideEdit(not IsDestroying);
  if not IsDestroying and DataController.IsEditing then
  begin
    DontMakeMasterRecordVisible := True;
    try
      try
        DataController.CheckBrowseMode;
      except
        Controller.EditingController.CheckEdit;
        raise;
      end;
    finally
      DontMakeMasterRecordVisible := False;
    end;
  end;
  inherited Deactivate;
end;

procedure TcxCustomGridTableView.DestroyingSiteHandle;
begin
  if (Controller <> nil) and not (csDestroying in ComponentState) then
    Controller.EditingController.CloseEdit;
end;

procedure TcxCustomGridTableView.DetailVisibleChanged(ADetailLevel: TComponent; APrevVisibleDetailCount, AVisibleDetailCount: Integer);
var
  I: Integer;
begin
  inherited;
  if IsPattern then
  begin
    BeginUpdate;
    try
      for I := 0 to CloneCount - 1 do
        TcxCustomGridViewAccess(Clones[I]).DetailVisibleChanged(ADetailLevel,
          APrevVisibleDetailCount, AVisibleDetailCount);
    finally
      EndUpdate;
    end;
  end
  else
    if (APrevVisibleDetailCount = 0) or (AVisibleDetailCount = 0) then
      DataChanged;
end;

procedure TcxCustomGridTableView.DoAssign(ASource: TcxCustomGridView);

  procedure AssignItems;
  var
    I: Integer;
    AItem: TcxCustomGridTableItem;
  begin
    with TcxCustomGridTableView(ASource) do
    begin
      Self.BeginAssignItems;
      try
        for I := 0 to ItemCount - 1 do
        begin
          AItem := Self.FindItemByID(Items[I].ID);
          if AItem = nil then
          begin
            AItem := Self.CreateItem;
            AItem.FID := Items[I].ID;
          end;
          AItem.Index := I;
          AItem.Assign(Items[I]);
        end;
        for I := Self.ItemCount - 1 downto ItemCount do
          Self.Items[I].Free;
      finally
        Self.EndAssignItems;
      end;
      Self.FNextID := FNextID;
    end;
  end;

begin
  if ASource is TcxCustomGridTableView then
  begin
    BeginUpdate;  //!!! is it needed?
    try
      if not AssigningSettings then
        AssignItems;
      with TcxCustomGridTableView(ASource) do
      begin
        Self.DateTimeHandling := DateTimeHandling;
        Self.FilterBox := FilterBox;
        Self.Filtering := Filtering;
        Self.FindPanel := FindPanel;
        Self.Navigator := Navigator;
        Self.OptionsCustomize := OptionsCustomize;
        if HasConditionalFormattingProvider and not Self.AssigningPattern and
            (Self.GetConditionalFormattingProvider <> nil) then
          Self.ConditionalFormatting.Assign(ConditionalFormatting);

        Self.OnCanFocusRecord := OnCanFocusRecord;
        Self.OnCanSelectRecord := OnCanSelectRecord;
        Self.OnCellClick := OnCellClick;
        Self.OnCellDblClick := OnCellDblClick;
        Self.OnCustomDrawCell := OnCustomDrawCell;
        Self.OnCustomDrawPartBackground := OnCustomDrawPartBackground;
        Self.OnEditing := OnEditing;
        Self.OnEditChanged := OnEditChanged;
        Self.OnEditDblClick := OnEditDblClick;
        Self.OnEditKeyDown := OnEditKeyDown;
        Self.OnEditKeyPress := OnEditKeyPress;
        Self.OnEditKeyUp := OnEditKeyUp;
        Self.OnEditValueChanged := OnEditValueChanged;
        Self.OnFilterControlDialogShow := OnFilterControlDialogShow;
        Self.OnFilterCustomization := OnFilterCustomization;
        Self.OnFilterDialogShow := OnFilterDialogShow;
        Self.OnFindPanelVisibilityChanged := OnFindPanelVisibilityChanged;
        Self.OnFocusedItemChanged := OnFocusedItemChanged;
        Self.OnFocusedRecordChanged := OnFocusedRecordChanged;
        Self.OnGetCellHeight := OnGetCellHeight;
        Self.OnGetDragDropText := OnGetDragDropText;
        Self.OnInitFilteringDateRanges := OnInitFilteringDateRanges;
        Self.OnInitGroupingDateRanges := OnInitGroupingDateRanges;
        Self.OnInitEdit := OnInitEdit;
        Self.OnInitEditValue := OnInitEditValue;
        Self.OnUpdateEdit := OnUpdateEdit;
        Self.OnSelectionChanged := OnSelectionChanged;
        Self.OnTopRecordIndexChanged := OnTopRecordIndexChanged;
      end;
    finally
      EndUpdate;
    end;
  end;
  inherited;
end;

function TcxCustomGridTableView.FindItemByObjectName(const AObjectName: string): TcxCustomGridTableItem;
begin
  if IsStoringNameMode then
    if AObjectName = '' then
      Result := nil
    else
      Result := FindItemByID(StrToInt(AObjectName))
  else
    Result := FindItemByName(AObjectName);
end;

procedure TcxCustomGridTableView.FindPanelVisibilityChanged(AVisible: Boolean);
begin
  if Assigned(FOnFindPanelVisibilityChanged) then FOnFindPanelVisibilityChanged(Self, AVisible);
end;

procedure TcxCustomGridTableView.FocusDataControl(AItemIndex: Integer; var ADone: Boolean);
begin
  FInDataControlFocusing := True;
  try
    FocusEdit(AItemIndex, ADone);
  finally
    FInDataControlFocusing := False;
  end;
end;

procedure TcxCustomGridTableView.FocusEdit(AItemIndex: Integer; var ADone: Boolean);
begin
  Items[AItemIndex].Editing := True;
  ADone := Items[AItemIndex].Editing;
end;

procedure TcxCustomGridTableView.GetChildren(Proc: TGetChildProc; Root: TComponent);
const
  scxDoesNotMatchPropertyType = 'The Properties property type of the %s component doesn' +
    '''t match the property type of the component introduced in an ancestor form';
var
  I: Integer;
  AItem: TcxCustomGridTableItem;
begin
  inherited;
  for I := 0 to ItemCount - 1 do
  begin
    AItem := Items[I];
    if AItem.Owner = Root then
    try
      Proc(AItem);
    except
      if csAncestor in ComponentState then
        raise Exception.Create(Format(scxDoesNotMatchPropertyType, [AItem.Name]))
      else
        raise;
    end;
  end;
end;

function TcxCustomGridTableView.GetDefaultActiveDetailIndex: Integer;
var
  AAvailableLevel: TcxGridLevel;
begin
  AAvailableLevel := TcxGridLevel(Level).GetAvailableItem;
  if AAvailableLevel = nil then
    Result := -1
  else
    Result := AAvailableLevel.Index;
end;

function TcxCustomGridTableView.GetEditAutoHeight: TcxInplaceEditAutoHeight;
begin
  Result := OptionsBehavior.EditAutoHeight;
  if (Result = eahRow) and not OptionsView.CellAutoHeight then
    Result := eahNone;
end;

procedure TcxCustomGridTableView.GetFakeComponentLinks(AList: TList);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ItemCount - 1 do
    Items[I].Styles.GetFakeComponentLinks(AList);
end;

function TcxCustomGridTableView.GetInternalConditionalFormattingProvider: TcxGridConditionalFormattingProvider;
begin
  if FConditionalFormattingProvider = nil then
    FConditionalFormattingProvider := CreateConditionalFormattingProvider;
  Result := FConditionalFormattingProvider;
end;

function TcxCustomGridTableView.GetIsControlFocused: Boolean;
begin
  Result := inherited GetIsControlFocused or
    Controller.EditingController.IsEditing and Controller.EditingController.Edit.IsFocused or
    Controller.HasFilterMRUItemsPopup and Controller.FilterMRUItemsPopup.Visible or
    Controller.HasFilterPopup and Controller.FilterPopup.Visible or
    Controller.HasItemsCustomizationPopup and Controller.ItemsCustomizationPopup.Visible or
    Controller.FindPanel.IsEditFocused;
end;

procedure TcxCustomGridTableView.GetItemsListForClipboard(AItems: TList; ACopyAll: Boolean);
begin
  GetVisibleItemsList(AItems);
end;

function TcxCustomGridTableView.GetItemSortByDisplayText(AItemIndex: Integer;
  ASortByDisplayText: Boolean): Boolean;
begin
  Result := Items[AItemIndex].IsSortingByDisplayText(ASortByDisplayText);
end;

function TcxCustomGridTableView.GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource;
var
  AProperties: TcxCustomEditProperties;
begin
  AProperties := Items[AItemIndex].GetProperties;
  if AProperties = nil then
    Result := evsText  //!!!
  else
    Result := AProperties.GetEditValueSource(True);
end;

procedure TcxCustomGridTableView.GetVisibleItemsList(AItems: TList);
begin
  dxCopyList(FVisibleItems, AItems);
end;

function TcxCustomGridTableView.HasCustomDrawCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawCell);
end;

function TcxCustomGridTableView.HasCustomDrawPartBackground: Boolean;
begin
  Result := Assigned(FOnCustomDrawPartBackground);
end;

function TcxCustomGridTableView.HasPixelScrollingMaster: Boolean;
begin
  Result := IsDetail and
   ((MasterGridView as TcxCustomGridTableView).IsRecordPixelScrolling or
    (MasterGridView as TcxCustomGridTableView).HasPixelScrollingMaster);
end;

procedure TcxCustomGridTableView.Init;
begin
  inherited Init;
  if Controller.FocusedItem = nil then
    Controller.FocusFirstAvailableItem;
end;

function TcxCustomGridTableView.IsDetailVisible(AGridView: TcxCustomGridView): Boolean;
var
  AViewInfo: TcxCustomGridRecordViewInfo;
begin
  if AGridView.MasterGridRecordIndex = -1 then
    Result := False
  else
  begin
    Result := ViewData.IsRecordIndexValid(AGridView.MasterGridRecordIndex);
    if Result then
    begin
      AViewInfo := ViewData.Records[AGridView.MasterGridRecordIndex].ViewInfo;
      Result := (AViewInfo <> nil) and AViewInfo.IsDetailVisible(AGridView);
    end;
  end;
end;

function TcxCustomGridTableView.IsEqualHeightRecords: Boolean;
begin
  Result := not ViewInfo.RecordsViewInfo.AutoDataRecordHeight;
end;

function TcxCustomGridTableView.IsEqualScrollSizeRecords: Boolean;
begin
  Result := IsEqualHeightRecords and
    (ViewInfo.GetEqualHeightRecordScrollSize <> -1);
end;

function TcxCustomGridTableView.IsRecordHeightDependsOnData: Boolean;
begin
  Result := ViewInfo.RecordsViewInfo.AutoRecordHeight or
    ViewInfo.RecordsViewInfo.AutoDataRecordHeight or HasCustomProperties;
end;

function TcxCustomGridTableView.IsRecordHeightDependsOnFocus: Boolean;
begin
  Result := ViewInfo.RecordsViewInfo.AutoDataRecordHeight and
    (OptionsView.ShowEditButtons = gsebForFocusedRecord);
end;

function TcxCustomGridTableView.IsRecordPixelScrolling: Boolean;
begin
  Result := not DataController.IsGridMode and
    ((OptionsBehavior.RecordScrollMode = rsmByPixel) or
    ((OptionsBehavior.RecordScrollMode = rsmDefault) and cxIsTouchModeEnabled));
end;

procedure TcxCustomGridTableView.LoadingComplete;
begin
  BeginUpdate;
  try
    inherited;
    DataChanged;
  finally
    EndUpdate;
  end;
  Controller.MakeFocusedRecordVisible;
end;

procedure TcxCustomGridTableView.Offset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer);
begin
  ViewInfo.DoOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY);
  if (ARecordCountDelta <> 0) or (APixelScrollRecordOffsetDelta <> 0) then
    Painter.DoOffset(ViewInfo.RecordsViewInfo.ItemsOffset, 0, 0)
  else
    Painter.DoOffset(0, DX, DY);
end;

procedure TcxCustomGridTableView.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ItemCount - 1 do
    Items[I].ChangeScale(M, D);
end;

procedure TcxCustomGridTableView.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited;
  if Child is TcxCustomGridTableItem then
    TcxCustomGridTableItem(Child).Index := Order;
end;

procedure TcxCustomGridTableView.SetName(const NewName: TComponentName);
var
  AOldName: TComponentName;
begin
  AOldName := Name;
  inherited;
  if Name <> AOldName then
    RenameComponents(Self, Owner, Name, AOldName, ItemCount, @cxCustomGridTableViewGetItem);
end;

function TcxCustomGridTableView.SpaceForHorizontalScrollbarNeeded: Boolean;
begin
  Result := inherited SpaceForHorizontalScrollbarNeeded and
    (ViewInfo.Bounds.Bottom <= ViewInfo.RecordsViewInfo.ContentBounds.Bottom);
end;

procedure TcxCustomGridTableView.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if IsDestroying then Exit;
  if AInfo is TcxDataChangedInfo then
    UpdateData(AInfo)
  else
    if AInfo is TcxLayoutChangedInfo then
      DataLayoutChanged
    else
      if AInfo is TcxFocusedRecordChangedInfo then
        UpdateFocusedRecord(AInfo)
      else
        if AInfo is TcxSelectionChangedInfo then
          SelectionChanged(TcxSelectionChangedInfo(AInfo))
        else
          if AInfo is TcxSearchChangedInfo then
            SearchChanged
          else
            if AInfo is TcxUpdateRecordInfo then
              RecordChanged(TcxUpdateRecordInfo(AInfo).RecordIndex)
            else
              if AInfo is TcxGroupingChangingInfo then
                GroupingChanging;
  if not Assigned(AInfo) or AInfo.CanNavigatorUpdate then
    RefreshNavigators;
  inherited UpdateControl(AInfo);
  if AInfo is TcxFindFilterChangedInfo then
    Controller.FindPanel.FindChanged;
end;

procedure TcxCustomGridTableView.UpdateData(AInfo: TcxUpdateControlInfo);
begin
  DataChanged;
end;

procedure TcxCustomGridTableView.UpdateDataController(AChange: TcxGridDataControllerChange;
  AItem: TcxCustomGridTableItem);
begin
  if {not IsDestroying and }(DataController <> nil) then
    with DataController do
      case AChange of
        dccItemAdded:
          AddItem(AItem);
        dccItemRemoved:
          RemoveItem(AItem);
        dccIndexesChanged:
          UpdateItemIndexes;
      end;
end;

procedure TcxCustomGridTableView.UpdateFocusedRecord(AInfo: TcxUpdateControlInfo);
var
  AChangedInfo: TcxFocusedRecordChangedInfo;
begin
  AChangedInfo := TcxFocusedRecordChangedInfo(AInfo);
  Controller.FocusedRecordChanged(AChangedInfo.PrevFocusedRowIndex, AChangedInfo.FocusedRowIndex,
    AChangedInfo.PrevFocusedRecordIndex, AChangedInfo.FocusedRecordIndex, AChangedInfo.NewItemRowFocusingChanged);
end;

procedure TcxCustomGridTableView.UpdateRecord;
begin
  Controller.EditingController.UpdateValue;
end;

procedure TcxCustomGridTableView.ValidateEditVisibility;
begin
  Controller.ValidateEditVisibility;
end;

procedure TcxCustomGridTableView.CreateHandlers;
begin
  FItems := TList.Create;
  FVisibleItems := TList.Create;
  FFilterableItems := TList.Create;
  FNavigatorNotifier := TcxNavigatorControlNotifier.Create;
  inherited CreateHandlers;
  FNavigator := GetNavigatorClass.Create(Self);
end;

procedure TcxCustomGridTableView.DestroyHandlers;
begin
  FreeAndNil(FConditionalFormattingProvider);
  ClearItems;
  FreeAndNil(FNavigator);
  inherited DestroyHandlers;
  FreeAndNil(FNavigatorNotifier);
  FreeAndNil(FFilterableItems);
  FreeAndNil(FVisibleItems);
  FreeAndNil(FItems);
end;

procedure TcxCustomGridTableView.CreateOptions;
begin
  inherited;
  FDateTimeHandling := GetDateTimeHandlingClass.Create(Self);
  FFilterBox := GetFilterBoxClass.Create(Self);
  FFiltering := GetFilteringClass.Create(Self);
  FFindPanel := CreateFindPanel;
  FOptionsCustomize := GetOptionsCustomizeClass.Create(Self);
end;

procedure TcxCustomGridTableView.DestroyOptions;
begin
  FreeAndNil(FOptionsCustomize);
  FreeAndNil(FFindPanel);
  FreeAndNil(FFiltering);
  FreeAndNil(FFilterBox);
  FreeAndNil(FDateTimeHandling);
  inherited;
end;

function TcxCustomGridTableView.CreateFindPanel: TcxGridFindPanelOptions;
begin
  Result := TcxGridFindPanelOptions.Create(Self);
end;

procedure TcxCustomGridTableView.AddItem(AItem: TcxCustomGridTableItem);
begin
  if csTransient in ComponentStyle then
    AItem.FComponentStyle := AItem.FComponentStyle + [csTransient];
  FItems.Add(AItem);
  ItemIndexChanged(AItem, -1);
  AItem.FID := GetNextID;
  UpdateDataController(dccItemAdded, AItem);
  AItem.SetGridView(Self);  // needs Field
  AItem.Visible := True;
  RefreshFilterableItemsList;
  AItem.CheckUsingInFindFiltering;
end;

procedure TcxCustomGridTableView.RemoveItem(AItem: TcxCustomGridTableItem);
var
  AOldItemIndex: Integer;
begin
  BeginUpdate;
  try
    if AItem = OptionsBehavior.IncSearchItem then
      OptionsBehavior.IncSearchItem := nil;
    AItem.Visible := False;
    AOldItemIndex := AItem.Index;
    FItems.Remove(AItem);
    AItem.FIndex := -1;
    ItemIndexChanged(AItem, AOldItemIndex);
    UpdateDataController(dccItemRemoved, AItem);
  finally
    EndUpdate;
  end;
  AItem.SetGridView(nil);
  FFiltering.MRUItems.DeleteEmptyItems;
  ReleaseID(AItem.ID);
  RefreshFilterableItemsList;
  AItem.CheckUsingInFindFiltering;
  RefreshCustomizationForm;
  Synchronize;
end;

procedure TcxCustomGridTableView.AssignVisibleItemsIndexes;
var
  I: Integer;
begin
  for I := 0 to VisibleItemCount - 1 do
    with VisibleItems[I] do
      FVisibleIndex := GetVisibleIndex;
end;

procedure TcxCustomGridTableView.ChangeItemIndex(AItem: TcxCustomGridTableItem;
  Value: Integer);
var
  AOldItemIndex: Integer;
begin
  if Value < 0 then Value := 0;
  if Value >= ItemCount then Value := ItemCount - 1;
  if AItem.Index <> Value then
  begin
    AOldItemIndex := AItem.Index;
    FItems.Move(AItem.Index, Value);
    ItemIndexChanged(AItem, AOldItemIndex);
    if AItem.Visible then RefreshVisibleItemsList;
    UpdateDataController(dccIndexesChanged, AItem);
  end;
end;

procedure TcxCustomGridTableView.CheckItemVisibles;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
      Items[I].CheckVisible;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableView.SaveItemVisibles;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].SaveVisible;
end;

procedure TcxCustomGridTableView.ItemIndexChanged(AItem: TcxCustomGridTableItem;
  AOldIndex: Integer);
begin
  RefreshItemIndexes;
end;

procedure TcxCustomGridTableView.ItemVisibilityChanged(AItem: TcxCustomGridTableItem;
  Value: Boolean);
begin
  if Value then
  begin
    RefreshVisibleItemsList;
    AItem.Changed(ticSize);
    if Controller.FocusedItem = nil then
      AItem.Focused := True;
  end
  else
  begin
    if AItem.IncSearching then Controller.CancelIncSearching;
    RefreshVisibleItemsList;
    AItem.Changed(ticSize);
    AItem.Focused := False;
  end;
end;

procedure TcxCustomGridTableView.RefreshVisibleItemsList;
var
  I: Integer;
begin
  FVisibleItems.Clear;
  for I := 0 to ItemCount - 1 do
    if Items[I].ActuallyVisible then
      Items[I].FVisibleIndex := FVisibleItems.Add(Items[I])
    else
      Items[I].FVisibleIndex := -1;
end;

function TcxCustomGridTableView.GetItemDataBindingClass: TcxGridItemDataBindingClass;
var
  AIGridDataController: IcxGridDataController;
begin
  if Supports(TObject(DataController), IcxGridDataController, AIGridDataController) then
    Result := AIGridDataController.GetItemDataBindingClass
  else
    Result := nil;
end;

function TcxCustomGridTableView.GetNextID: Integer;
begin
  Result := FNextID;
  Inc(FNextID);
end;

procedure TcxCustomGridTableView.ReleaseID(AID: Integer);
begin
  if AID = FNextID - 1 then Dec(FNextID);
end;

procedure TcxCustomGridTableView.DataChanged;
var
  I: Integer;
begin
  Controller.DragHighlightedRecord := nil;
  for I := 0 to ItemCount - 1 do
    Items[I].DataChanged;
  Changed(TcxGridDataChange.Create(Self));
  Controller.EditingController.UpdateEditValue;
  Synchronize;
end;

procedure TcxCustomGridTableView.DataLayoutChanged;
begin
  ViewData.RefreshRecords;
  SizeChanged;
  Controller.EditingController.UpdateEditValue;
  Synchronize;
end;

function TcxCustomGridTableView.DoCellClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := False;
  if Assigned(FOnCellClick) then
    FOnCellClick(Self, ACellViewInfo, AButton, AShift, Result);
end;

function TcxCustomGridTableView.DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := False;
  if Assigned(FOnCellDblClick) then
    FOnCellDblClick(Self, ACellViewInfo, AButton, AShift, Result);
end;

function TcxCustomGridTableView.DoEditing(AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := True;
  if Assigned(FOnEditing) then FOnEditing(Self, AItem, Result);
end;

procedure TcxCustomGridTableView.DoTopRecordIndexChanged;
begin
  if Assigned(FOnTopRecordIndexChanged) then FOnTopRecordIndexChanged(Self);
end;

procedure TcxCustomGridTableView.DoUnlockNotification(AInfo: TcxUpdateControlInfo);
begin
  if AInfo is TcxSelectionChangedInfo then
    DoSelectionChanged;
end;

procedure TcxCustomGridTableView.FilterChanged;
begin
  FFiltering.FilterChanged;
end;

procedure TcxCustomGridTableView.FocusedItemChanged(APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
begin
  if Assigned(OnFocusedItemChanged) then
    FOnFocusedItemChanged(Self, APrevFocusedItem, AFocusedItem);
end;

procedure TcxCustomGridTableView.FocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
begin
  Changed(TcxGridFocusedRecordChange.Create(Self, APrevFocusedRecordIndex,
    AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex,
    ANewItemRecordFocusingChanged));
end;

procedure TcxCustomGridTableView.GroupingChanging;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].GroupingChanging;
end;

procedure TcxCustomGridTableView.ItemCaptionChanged(AItem: TcxCustomGridTableItem);
begin
  if (AItem = nil) or AItem.ActuallyVisible or (AItem.GroupIndex <> -1) or
    AItem.DataBinding.Filtered then
    Changed(vcSize)
  else
    Changed(vcProperty);
  RefreshCustomizationForm;
end;

procedure TcxCustomGridTableView.ItemValueTypeClassChanged(AItemIndex: Integer);
begin
  Items[AItemIndex].ValueTypeClassChanged;
end;

procedure TcxCustomGridTableView.RecalculateDefaultWidths;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
      Items[I].RecalculateDefaultWidth;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableView.RecordChanged(ARecordIndex: Integer);
var
  ARecord: TcxCustomGridRecord;
begin
  AssignEditingRecord;
  if IsRecordHeightDependsOnData then
    SizeChanged
  else
  begin
    ARecord := ViewData.GetRecordByRecordIndex(ARecordIndex);
    if ARecord = nil then
      LayoutChanged
    else
      ARecord.Invalidate;
  end;
  Controller.EditingController.UpdateEditValue;
end;

procedure TcxCustomGridTableView.RecordCountChanged;
begin
  ViewInfoCache.Count := DataController.GetRowCount;
  ViewData.Refresh(DataController.GetRowCount);
  if Control <> nil then  {7}
    TcxCustomGrid(Control).SendNotifications(gnkRecordCountChanged);
end;

procedure TcxCustomGridTableView.RefreshFilterableItemsList;
var
  I: Integer;
begin
  if IsDestroying then Exit;
  FFilterableItems.Clear;
  for I := 0 to ItemCount - 1 do
    if Items[I].Filterable then
      FFilterableItems.Add(Items[I]);
end;

procedure TcxCustomGridTableView.RefreshNavigators;
begin
  if ViewInfo.NavigatorSiteViewInfo.Visible then
    ViewInfo.NavigatorStateChanged;
  FNavigatorNotifier.RefreshNavigatorButtons;
  if Focused then
    TcxCustomGridAccess(Control).RefreshNavigators;
end;

procedure TcxCustomGridTableView.SearchChanged;
begin
  if Controller.FocusedRecord <> nil then
    Controller.FocusedRecord.Invalidate(Controller.IncSearchingItem);
end;

procedure TcxCustomGridTableView.SelectionChanged(AInfo: TcxSelectionChangedInfo);
var
  I: Integer;
  CB67026A92E44809A4CC81B8F2B2BF3A: TcxCustomGridRecord;
begin
  if NeedChangeLayoutOnSelectionChanged(AInfo) then
    LayoutChanged
  else
    for I := 0 to AInfo.Count - 1 do
    begin
      CB67026A92E44809A4CC81B8F2B2BF3A := ViewData.GetRecordByIndex(AInfo.RowIndexes[I]);
      if CB67026A92E44809A4CC81B8F2B2BF3A <> nil then CB67026A92E44809A4CC81B8F2B2BF3A.Invalidate;
    end;
  if not IsLoading then
    NotifySelectionChanged(TcxSelectionChangedInfo(AInfo.Clone));
end;

function TcxCustomGridTableView.CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean; ACellViewInfo: TcxGridTableCellViewInfo): Boolean;

  function GetRecordFocused: Boolean;
  begin
    if AUseViewInfo then
      Result := ARecord.ViewInfo.Focused
    else
      Result := DrawRecordFocused(ARecord);
  end;

begin
  if not (ARecord.HasCells and ARecord.CanFocusCells) then
    Result := True
  else
    if OptionsSelection.InvertSelect then
      Result := (AItem = nil) or not AItem.Focused or not GetRecordFocused
    else
      Result := (OptionsSelection.MultiSelect or
        (AItem <> nil) and AItem.Focused and GetRecordFocused) and
        (not (ACellViewInfo is TcxGridTableDataCellViewInfo) or
         not TcxGridTableDataCellViewInfo(ACellViewInfo).Editing);
end;

function TcxCustomGridTableView.DrawDataCellSelected(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean = False;
  ACellViewInfo: TcxGridTableCellViewInfo = nil): Boolean;

  function GetRecordSelected: Boolean;
  begin
    if AUseViewInfo then
      Result := ARecord.ViewInfo.Selected
    else
      Result := DrawRecordSelected(ARecord);
  end;

  function GetCellSelected: Boolean;
  begin
    if (ACellViewInfo <> nil) and ACellViewInfo.AlwaysSelected then
      Result := True
    else
      Result := CalculateDataCellSelected(ARecord, AItem, AUseViewInfo, ACellViewInfo);
  end;

begin
  Result :=
    ((ACellViewInfo = nil) or ACellViewInfo.CanDrawSelected) and
    (ARecord <> nil) and GetRecordSelected and GetCellSelected and
    DrawSelection;
end;

function TcxCustomGridTableView.DrawRecordActive(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := IsControlFocused or ARecord.DragHighlighted;
end;

function TcxCustomGridTableView.DrawRecordFocused(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := ARecord.Focused and Focused;
end;

function TcxCustomGridTableView.DrawRecordSelected(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result :=
    ARecord.Selected and (Focused or OptionsSelection.MultiSelect) or
    ARecord.DragHighlighted;
end;

function TcxCustomGridTableView.DrawSelection: Boolean;
begin
  Result := IsControlFocused or not OptionsSelection.HideSelection;
end;

function TcxCustomGridTableView.DoCanFocusRecord(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := True;
  if Assigned(FOnCanFocusRecord) then FOnCanFocusRecord(Self, ARecord, Result);
end;

procedure TcxCustomGridTableView.DoCustomDrawCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawCell then
    FOnCustomDrawCell(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxCustomGridTableView.DoCustomDrawPartBackground(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawPartBackground then
    FOnCustomDrawPartBackground(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxCustomGridTableView.DoEditChanged(AItem: TcxCustomGridTableItem);
begin
  if Assigned(FOnEditChanged) then FOnEditChanged(Self, AItem);
end;

procedure TcxCustomGridTableView.DoEditDblClick(AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit);
begin
  if Assigned(FOnEditDblClick) then FOnEditDblClick(Self, AItem, AEdit);
end;

procedure TcxCustomGridTableView.DoEditKeyDown(AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnEditKeyDown) then FOnEditKeyDown(Self, AItem, AEdit, Key, Shift);
end;

procedure TcxCustomGridTableView.DoEditKeyPress(AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var Key: Char);
begin
  if Assigned(FOnEditKeyPress) then FOnEditKeyPress(Self, AItem, AEdit, Key);
end;

procedure TcxCustomGridTableView.DoEditKeyUp(AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnEditKeyUp) then FOnEditKeyUp(Self, AItem, AEdit, Key, Shift);
end;

procedure TcxCustomGridTableView.DoEditValueChanged(AItem: TcxCustomGridTableItem);
begin
  if Assigned(FOnEditValueChanged) then FOnEditValueChanged(Self, AItem);
end;

function TcxCustomGridTableView.DoFilterCustomization: Boolean;
begin
  Result := False;
  if Assigned(FOnFilterCustomization) then FOnFilterCustomization(Self, Result);
end;

function TcxCustomGridTableView.DoFilterDialogShow(AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := False;
  if Assigned(FOnFilterDialogShow) then FOnFilterDialogShow(Self, AItem, Result);
end;

procedure TcxCustomGridTableView.DoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
var
  APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord;
begin
  if Control <> nil then
    TcxCustomGrid(Control).SendNotifications(gnkFocusedRecordChanged);
  if Controller.NeedUpdateSiteOnFocusedRecordChanged then
  begin
    Controller.EditingController.CheckEditUpdatePost;
    Site.Update;
  end;
  if Assigned(FOnFocusedRecordChanged) then
  begin
    AFocusedRecord := ViewData.GetRecordByIndex(AFocusedRecordIndex);
    if APrevFocusedDataRecordIndex = AFocusedDataRecordIndex then
      APrevFocusedRecord := AFocusedRecord
    else
      APrevFocusedRecord := ViewData.GetRecordByIndex(APrevFocusedRecordIndex);
    FOnFocusedRecordChanged(Self, APrevFocusedRecord, AFocusedRecord, ANewItemRecordFocusingChanged);
  end;
end;

procedure TcxCustomGridTableView.DoGetCellHeight(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; ACellViewInfo: TcxGridTableDataCellViewInfo;
  var AHeight: Integer);
begin
  if IsGetCellHeightAssigned then
    FOnGetCellHeight(Self, ARecord, AItem, ACellViewInfo, AHeight);
end;

function TcxCustomGridTableView.DoGetDragDropText(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem): string;
begin
  if (ARecord <> nil) and (AItem <> nil) then
    Result := ARecord.DisplayTexts[AItem.Index]
  else
    Result := '';
  if Assigned(FOnGetDragDropText) then
    FOnGetDragDropText(Self, ARecord, AItem, Result);  // for vic
end;

procedure TcxCustomGridTableView.DoInitEdit(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
begin
  if Assigned(FOnInitEdit) then FOnInitEdit(Self, AItem, AEdit);
end;

procedure TcxCustomGridTableView.DoInitEditValue(AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var AValue: TcxEditValue);
begin
  if Assigned(FOnInitEditValue) then FOnInitEditValue(Self, AItem, AEdit, AValue);
end;

procedure TcxCustomGridTableView.DoInitFilteringDateRanges(AItem: TcxCustomGridTableItem);
begin
  if HasInitFilteringDateRangesHandler then
    FOnInitFilteringDateRanges(AItem, AItem.FilteringDateRanges);
end;

procedure TcxCustomGridTableView.DoInitGroupingDateRanges(AItem: TcxCustomGridTableItem);
begin
  if HasInitGroupingDateRangesHandler then
    FOnInitGroupingDateRanges(AItem, AItem.GroupingDateRanges);
end;

procedure TcxCustomGridTableView.DoMakeMasterGridRecordVisible;
begin
  MasterGridRecord.MakeVisible;
end;

procedure TcxCustomGridTableView.DoUpdateEdit(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
begin
  if Assigned(FOnUpdateEdit) then FOnUpdateEdit(Self, AItem, AEdit);
end;

{procedure TcxCustomGridTableView.DoRecordCreated(ARecord: TcxCustomGridRecord);
begin
  if Assigned(FOnRecordCreated) then FOnRecordCreated(Self, ARecord);
end;

procedure TcxCustomGridTableView.DoRecordDestroying(ARecord: TcxCustomGridRecord);
begin
  if Assigned(FOnRecordDestroying) then FOnRecordDestroying(Self, ARecord);
end;}

procedure TcxCustomGridTableView.DoSelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(Self);
end;

function TcxCustomGridTableView.HasCustomProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I].HasCustomPropertiesHandler;
    if Result then Exit;
  end;
  Result := False;
end;

function TcxCustomGridTableView.HasInitFilteringDateRangesHandler: Boolean;
begin
  Result := Assigned(FOnInitFilteringDateRanges);
end;

function TcxCustomGridTableView.HasInitGroupingDateRangesHandler: Boolean;
begin
  Result := Assigned(FOnInitGroupingDateRanges);
end;

function TcxCustomGridTableView.IsGetCellHeightAssigned: Boolean;
begin
  Result := Assigned(FOnGetCellHeight);
end;

function TcxCustomGridTableView.NeedHighlightFindFilterText: Boolean;
begin
  Result := DataController.IsFindFiltering and FindPanel.HighlightSearchResults;
end;

function TcxCustomGridTableView.IsLongFilterOperation: Boolean;
begin
  Result := (DataController.GetSortingItemCount > 0) or
    (DataController.Groups.GroupingItemCount > 0);
end;

procedure TcxCustomGridTableView.LookAndFeelChanged;
begin
  Controller.EditingController.HideEdit(True);
  inherited LookAndFeelChanged;
  Controller.FindPanel.LookAndFeelChanged;
end;

function TcxCustomGridTableView.NeedChangeLayoutOnSelectionChanged(AInfo: TcxSelectionChangedInfo): Boolean;
begin
  Result := (AInfo.Count = 0);
end;

procedure TcxCustomGridTableView.NotifySelectionChanged(AInfo: TcxSelectionChangedInfo = nil);
begin
  if IsUpdateLocked then
  begin
    if AInfo = nil then
      AInfo := TcxSelectionChangedInfo.Create;
    AddNotification(AInfo);
  end
  else
  begin
    AInfo.Free;
    DoSelectionChanged;
  end;
end;

function TcxCustomGridTableView.CreateConditionalFormattingProvider: TcxGridConditionalFormattingProvider;
begin
  if PatternGridView <> Self then
    Result := TcxGridDetailModeConditionalFormattingProvider.Create(Self)
  else
    Result := TcxGridConditionalFormattingProvider.Create(Self);
end;

function TcxCustomGridTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxCustomGridTableController;
end;

function TcxCustomGridTableView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxCustomGridTablePainter;
end;

function TcxCustomGridTableView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxCustomGridTableViewData;
end;

function TcxCustomGridTableView.GetViewInfoCacheClass: TcxCustomGridViewInfoCacheClass;
begin
  Result := TcxCustomGridTableViewInfoCache;
end;

function TcxCustomGridTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxCustomGridTableViewInfo;
end;

function TcxCustomGridTableView.GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass;
begin
  Result := TcxCustomGridTableBackgroundBitmaps;
end;

function TcxCustomGridTableView.GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass;
begin
  Result := TcxCustomGridTableDateTimeHandling;
end;

function TcxCustomGridTableView.GetFilterBoxClass: TcxGridFilterBoxClass;
begin
  Result := TcxGridFilterBox;
end;

function TcxCustomGridTableView.GetFilteringClass: TcxCustomGridTableFilteringClass;
begin
  Result := TcxCustomGridTableFiltering;
end;

function TcxCustomGridTableView.GetNavigatorClass: TcxGridViewNavigatorClass;
begin
  Result := TcxGridViewNavigator;
end;

function TcxCustomGridTableView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxCustomGridTableOptionsBehavior;
end;

function TcxCustomGridTableView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxCustomGridTableOptionsCustomize;
end;

function TcxCustomGridTableView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxCustomGridTableOptionsData;
end;

function TcxCustomGridTableView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxCustomGridTableOptionsSelection;
end;

function TcxCustomGridTableView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxCustomGridTableOptionsView;
end;

function TcxCustomGridTableView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxCustomGridTableViewStyles;
end;

procedure TcxCustomGridTableView.ApplyBestFit(AItem: TcxCustomGridTableItem = nil;
  ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False);
var
  I: Integer;
begin
  if AItem = nil then
  begin
    BeginBestFitUpdate;
    try
      for I := 0 to VisibleItemCount - 1 do
        VisibleItems[I].ApplyBestFit(ACheckSizingAbility, AFireEvents)
    finally
      EndBestFitUpdate;
    end;
  end
  else
    AItem.ApplyBestFit(ACheckSizingAbility, AFireEvents);
end;

procedure TcxCustomGridTableView.ClearItems;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := ItemCount - 1 downto 0 do Items[I].Free;
    FNextID := 0;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableView.CopyToClipboard(ACopyAll: Boolean);

  procedure AddHeaders;
  var
    I: Integer;
  begin
    for I := 0 to FCopyToClipboardItems.Count - 1 do
      FCopyToClipboardStr := FCopyToClipboardStr +
        TcxCustomGridTableItem(FCopyToClipboardItems[I]).GetAlternateCaption + ColumnSeparator;
    if FCopyToClipboardItems.Count <> 0 then
      FCopyToClipboardStr := Copy(FCopyToClipboardStr, 1,
        Length(FCopyToClipboardStr) - Length(ColumnSeparator));
    FCopyToClipboardStr := FCopyToClipboardStr + dxCRLF;
  end;

begin
  FCopyToClipboardItems := TList.Create;
  try
    GetItemsListForClipboard(FCopyToClipboardItems, ACopyAll);
    FCopyToClipboardStr := '';
    if OptionsBehavior.CopyCaptionsToClipboard then
      AddHeaders;

    if Control <> nil then
      TcxCustomGrid(Control).BeginUpdate;
    try
      DataController.ForEachRow(not ACopyAll, CopyForEachRowProc);
    finally
      if Control <> nil then
        TcxCustomGrid(Control).EndUpdate;
    end;
    if DataController.IsGridMode then
      Controller.MakeFocusedRecordVisible;

    if (Length(FCopyToClipboardStr) >= Length(dxCRLF)) and
      (Copy(FCopyToClipboardStr, Length(FCopyToClipboardStr) - Length(dxCRLF) + 1, Length(dxCRLF)) = dxCRLF) then
      SetLength(FCopyToClipboardStr, Length(FCopyToClipboardStr) - Length(dxCRLF));
  finally
    FCopyToClipboardItems.Free;
  end;
  Clipboard.AsText := FCopyToClipboardStr;
end;

function TcxCustomGridTableView.CreateItem: TcxCustomGridTableItem;
begin
  Result := GetItemClass.Create(Owner);
  AddItem(Result);
end;

function TcxCustomGridTableView.FindItemByID(AID: Integer): TcxCustomGridTableItem;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I];
    if Result.ID = AID then Exit;
  end;
  Result := nil;
end;

function TcxCustomGridTableView.FindItemByName(const AName: string): TcxCustomGridTableItem;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I];
    if Result.Name = AName then Exit;
  end;
  Result := nil;
end;

function TcxCustomGridTableView.FindItemByTag(ATag: TcxTag): TcxCustomGridTableItem;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I];
    if Result.Tag = ATag then Exit;
  end;
  Result := nil;
end;

function TcxCustomGridTableView.IndexOfItem(AItem: TcxCustomGridTableItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TcxCustomGridTableView.MakeMasterGridRecordVisible;
begin
  if CanMakeMasterGridRecordVisible then
    DoMakeMasterGridRecordVisible;
end;

procedure TcxCustomGridTableView.RestoreDefaults;
var
  I: Integer;
begin
  inherited;
  BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridTableView.BeginBestFitUpdate;
begin
  ShowHourglassCursor;
  GridBeginLockedStatePaint(OptionsBehavior.ShowLockedStateImageOptions.BestFit);
end;

procedure TcxCustomGridTableView.EndBestFitUpdate;
begin
  GridEndLockedStatePaint;
  HideHourglassCursor;
end;


procedure TcxCustomGridTableView.BeginFilteringUpdate;
begin
  ShowHourglassCursor;
  GridBeginLockedStatePaint(OptionsBehavior.ShowLockedStateImageOptions.Filtering);
end;

procedure TcxCustomGridTableView.EndFilteringUpdate;
begin
  GridEndLockedStatePaint;
  HideHourglassCursor;
end;

procedure TcxCustomGridTableView.BeginGroupingUpdate;
begin
  ShowHourglassCursor;
  GridBeginUpdate(OptionsBehavior.ShowLockedStateImageOptions.Grouping);
end;

procedure TcxCustomGridTableView.EndGroupingUpdate;
begin
  GridEndUpdate;
  HideHourglassCursor;
end;

procedure TcxCustomGridTableView.BeginSortingUpdate;
begin
  ShowHourglassCursor;
  BeginUpdate(OptionsBehavior.ShowLockedStateImageOptions.Sorting);
end;

procedure TcxCustomGridTableView.EndSortingUpdate;
begin
  EndUpdate;
  HideHourglassCursor;
end;

class function TcxCustomGridTableView.CanBeLookupList: Boolean;
begin
  Result := False;
end;

function TcxCustomGridTableView.CanMakeMasterGridRecordVisible: Boolean;
begin
  Result := IsDetail and (MasterGridRecord <> nil) and
    (not DontMakeMasterRecordVisible or HasPixelScrollingMaster) and
    not MasterGridView.DataController.IsDetailExpanding;
end;

function TcxCustomGridTableView.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := nil;
end;

function TcxCustomGridTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := nil;
end;

{ TcxCustomGridTableControllerAccess }

class procedure TcxCustomGridTableControllerAccess.FocusNextPage(AInstance: TcxCustomGridTableController;
  ASyncSelection: Boolean);
begin
  AInstance.FocusNextPage(ASyncSelection);
end;

class procedure TcxCustomGridTableControllerAccess.FocusPrevPage(AInstance: TcxCustomGridTableController;
  ASyncSelection: Boolean);
begin
  AInstance.FocusPrevPage(ASyncSelection);
end;

{ TcxCustomGridTableItemAccess }

class function TcxCustomGridTableItemAccess.CanFilter(AInstance: TcxCustomGridTableItem; AVisually: Boolean): Boolean;
begin
  Result := AInstance.CanFilter(AVisually);
end;

class function TcxCustomGridTableItemAccess.CanGroup(AInstance: TcxCustomGridTableItem): Boolean;
begin
  Result := AInstance.CanGroup;
end;

class function TcxCustomGridTableItemAccess.CanHide(AInstance: TcxCustomGridTableItem): Boolean;
begin
  Result := AInstance.CanHide;
end;

class function TcxCustomGridTableItemAccess.CanHorzSize(AInstance: TcxCustomGridTableItem): Boolean;
begin
  Result := AInstance.CanHorzSize;
end;

class function TcxCustomGridTableItemAccess.CanSort(AInstance: TcxCustomGridTableItem): Boolean;
begin
  Result := AInstance.CanSort;
end;

class procedure TcxCustomGridTableItemAccess.CheckWidthValue(
  AInstance: TcxCustomGridTableItem; var Value: Integer);
begin
  AInstance.CheckWidthValue(Value);
end;

class procedure TcxCustomGridTableItemAccess.DoGetDataText(
  AInstance: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string);
begin
  AInstance.DoGetDataText(ARecordIndex, AText);
end;

class procedure TcxCustomGridTableItemAccess.DoGetDisplayText(
  AInstance: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AText: string);
begin
  AInstance.DoGetDisplayText(ARecord, AText);
end;

class function TcxCustomGridTableItemAccess.GetGroupIndex(
  AInstance: TcxCustomGridTableItem): Integer;
begin
  Result := AInstance.GroupIndex;
end;

class function TcxCustomGridTableItemAccess.GetFilterCaption(
  AInstance: TcxCustomGridTableItem): string;
begin
  Result := AInstance.FilterCaption;
end;

{ TcxCustomGridTableOptionsBehaviorAccess }

class function TcxCustomGridTableOptionsBehaviorAccess.GetPullFocusing(
  AInstance: TcxCustomGridTableOptionsBehavior): Boolean;
begin
  Result := AInstance.PullFocusing;
end;

class procedure TcxCustomGridTableOptionsBehaviorAccess.SetPullFocusing(
  AInstance: TcxCustomGridTableOptionsBehavior; Value: Boolean);
begin
  AInstance.PullFocusing := Value;
end;

{ TcxCustomGridTableOptionsViewAccess }

class function TcxCustomGridTableOptionsViewAccess.GetCellAutoHeight(
  AInstance: TcxCustomGridTableOptionsView): Boolean;
begin
  Result := AInstance.CellAutoHeight;
end;

{ TcxCustomGridTableViewAccess }

class function TcxCustomGridTableViewAccess.CanSelectRecord(
  AInstance: TcxCustomGridTableView; ARecordIndex: Integer): Boolean;
begin
  Result := AInstance.CanSelectRecord(ARecordIndex);
end;

class procedure TcxCustomGridTableViewAccess.FilterChanged(
  AInstance: TcxCustomGridTableView);
begin
  AInstance.FilterChanged;
end;

class function TcxCustomGridTableViewAccess.FindItemByObjectName(AInstance: TcxCustomGridTableView;
  const AObjectName: string): TcxCustomGridTableItem;
begin
  Result := AInstance.FindItemByObjectName(AObjectName);
end;

class procedure TcxCustomGridTableViewAccess.FocusEdit(
  AInstance: TcxCustomGridTableView; AItemIndex: Integer; var ADone: Boolean);
begin
  AInstance.FocusEdit(AItemIndex, ADone);
end;

class procedure TcxCustomGridTableViewAccess.FocusDataControl(AInstance: TcxCustomGridTableView; AItemIndex: Integer; var ADone: Boolean);
begin
  AInstance.FocusDataControl(AItemIndex, ADone);
end;

class function TcxCustomGridTableViewAccess.GetDefaultActiveDetailIndex(
  AInstance: TcxCustomGridTableView): Integer;
begin
  Result := AInstance.GetDefaultActiveDetailIndex;
end;

class function TcxCustomGridTableViewAccess.GetItemClass(AInstance: TcxCustomGridTableView): TcxCustomGridTableItemClass;
begin
  Result := AInstance.GetItemClass;
end;

class function TcxCustomGridTableViewAccess.GetItemSortByDisplayText(
  AInstance: TcxCustomGridTableView; AItemIndex: Integer;
  ASortByDisplayText: Boolean): Boolean;
begin
  Result := AInstance.GetItemSortByDisplayText(AItemIndex, ASortByDisplayText);
end;

class function TcxCustomGridTableViewAccess.GetItemValueSource(
  AInstance: TcxCustomGridTableView; AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := AInstance.GetItemValueSource(AItemIndex);
end;

class function TcxCustomGridTableViewAccess.GetSummaryGroupItemLinkClass(
  AInstance: TcxCustomGridTableView): TcxDataSummaryGroupItemLinkClass;
begin
  Result := AInstance.GetSummaryGroupItemLinkClass;
end;

class function TcxCustomGridTableViewAccess.GetSummaryItemClass(
  AInstance: TcxCustomGridTableView): TcxDataSummaryItemClass;
begin
  Result := AInstance.GetSummaryItemClass;
end;

class function TcxCustomGridTableViewAccess.IsEqualHeightRecords(
  AInstance: TcxCustomGridTableView): Boolean;
begin
  Result := AInstance.IsEqualHeightRecords;
end;

class function TcxCustomGridTableViewAccess.IsGetCellHeightAssigned(
  AInstance: TcxCustomGridTableView): Boolean;
begin
  Result := AInstance.IsGetCellHeightAssigned;
end;

class procedure TcxCustomGridTableViewAccess.ItemValueTypeClassChanged(
  AInstance: TcxCustomGridTableView; AItemIndex: Integer);
begin
  AInstance.ItemValueTypeClassChanged(AItemIndex);
end;

class procedure TcxCustomGridTableViewAccess.RefreshNavigators(
  AInstance: TcxCustomGridTableView);
begin
  AInstance.RefreshNavigators;
end;

class procedure TcxCustomGridTableViewAccess.UpdateRecord(
  AInstance: TcxCustomGridTableView);
begin
  AInstance.UpdateRecord;
end;

class procedure TcxCustomGridTableViewAccess.BeginLockedStatePaintOnPost(AInstance: TcxCustomGridTableView);
begin
  AInstance.GridBeginLockedStatePaint(AInstance.OptionsBehavior.ShowLockedStateImageOptions.Posting);
end;

class procedure TcxCustomGridTableViewAccess.EndLockedStatePaint(AInstance: TcxCustomGridTableView);
begin
  AInstance.GridEndLockedStatePaint;
end;

{ TcxCustomGridTableViewInfoAccess }

class function TcxCustomGridTableViewInfoAccess.GetDefaultGridModeBufferCount(AInstance: TcxCustomGridTableViewInfo): Integer;
begin
  Result := AInstance.GetDefaultGridModeBufferCount;
end;

initialization
  RegisterClasses([TcxGridItemDataBinding]);

end.
