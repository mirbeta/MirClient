{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxFilterValueContainer;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Classes, Windows, Controls, ComCtrls, cxContainer, cxControls, cxListBox, cxLookAndFeelPainters, dxCore, dxCoreClasses,
  cxCurrencyEdit, dxIncrementalFiltering, cxFilter, cxButtons, cxClasses, cxTextEdit, dxLayoutControl, dxLayoutContainer,
  dxLayoutLookAndFeels, dxRangeTrackBar, cxDropDownEdit, cxSpinEdit, cxRadioGroup, cxButtonEdit, cxCheckBox, cxDataStorage,
  cxCalendar, cxEdit, dxUIElementPopupWindow, cxGraphics, dxTreeView, Generics.Collections, cxFilterControlUtils;

type
  TdxExcelFilterValueContainerApplyChangesMode = (efacDefault, efacImmediately, efacOnTabOrOKButtonClick);
  TdxExcelFilterValueContainerNumericValuesPageType = (nvptDefault, nvptRange, nvptList);
  TdxExcelFilterValueContainerDateTimeValuesPageType = (dvptDefault, dvptTree, dvptList);
  TdxExcelFilterValueContainerDefaultPage = (dpDefault, dpValues, dpFilters);

const
  dxDefaultExcelFilterValueContainerApplyChanges: TdxExcelFilterValueContainerApplyChangesMode = efacImmediately;
  dxDefaultExcelFilterValueContainerNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType = nvptRange;
  dxDefaultExcelFilterValueContainerDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType = dvptTree;
  dxDefaultExcelFilterValueContainerDefaultPage: TdxExcelFilterValueContainerDefaultPage = dpValues;

type
  TdxExcelFilterValueContainerConditionContainer = class;
  TdxExcelFilterValueContainerCustomFilterConditionContainer = class;
  TdxExcelFilterValueContainerRangeModeValuesPage = class;
  TdxFilterValueContainerListBox = class;
  TdxCustomFilterValueContainer = class;
  TdxFilterValueContainer = class;
  TdxExcelFilterValueContainer = class;

  TdxFilterApplyChangesMode = (fpacImmediately, fpacOnButtonClick);

  TdxExcelFilterValueContainerType = (ctListBox, ctRange, ctDateTreeView, ctTimeTreeView, ctLookup, ctCheck);

  TdxExcelFilterValueContainerConditionType = (ctSpecificDatePeriods, ctEquals, ctDoesNotEqual,
    ctBeginsWith, ctEndsWith, ctContains, ctDoesNotContain, ctIsBlank, ctIsNotBlank, ctBetween, ctGreaterThan,
    ctGreaterThanOrEqualTo, ctLessThan, ctLessEqualThanOrEqualTo, ctTopN, ctBottomN, ctAboveAverage, ctBelowAverage,
    ctBefore, ctAfter, ctYesterday, ctToday, ctTomorrow, ctLastWeek, ctThisWeek, ctNextWeek, ctLastMonth, ctThisMonth,
    ctNextMonth, ctLastYear, ctThisYear, ctNextYear, ctCustomFilter);
  TdxExcelFilterValueContainerConditionTypes = set of TdxExcelFilterValueContainerConditionType;

  { IdxFilterableComponent }

  IdxFilterableComponent = interface
  ['{019D17DA-5693-46B5-8DA9-764E8AEA77C9}']
    procedure FilteringApplied;
    function GetFilter: TcxFilterCriteria;
    procedure GetFilterActiveValueIndexes(AValues: TcxFilterValueList; var AIndexes: TdxIntegerIndexes);
    function GetFilterSelectedValueIndex(AValues: TcxFilterValueList): Integer;
    function GetFilterValuesClass: TcxFilterValueListClass;
    procedure PopulateFilterValues(AValues: TcxFilterValueList; ADisplayValues: TStrings;
      AValuesOnly: Boolean; AUniqueOnly: Boolean);
    procedure SetFilterActiveValueIndex(AValueList: TcxFilterValueList; AIndex: Integer;
      AFilterList: TcxFilterCriteriaItemList; AReplaceExistent, AAddToMRUItemsList: Boolean);
    procedure SetFilterActiveValueIndexes(AValues: TcxFilterValueList; const AIndexes: TdxIntegerIndexes;
      AAddToMRUItemsList: Boolean = False);
  end;

  { IdxExcelFilterableComponent }

  IdxExcelFilterableComponent = interface(IdxFilterableComponent)
  ['{C38F02A1-09C0-4F71-ADE9-A27215899C2E}']
    function GetFilterItemLink: TObject;
    function GetFilterProperties: TcxCustomEditProperties;
    function GetFilterValueTypeClass: TcxValueTypeClass;
    procedure SetFilterActiveValue(AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; const ADisplayText: string);
    procedure SetFilterActiveValues(AValueInfos: TdxFastObjectList; ABoolOperator: TcxFilterBoolOperatorKind);
  end;

  { TdxExcelFilterValueContainerValueInfo }

  TdxExcelFilterValueContainerValueInfo = class
  strict private
    FDisplayText: string;
    FOperatorKind: TcxFilterOperatorKind;
    FValue: Variant;
  public
    constructor Create(AValue: Variant; ADisplayText: string; AOperatorKind: TcxFilterOperatorKind);

    property DisplayText: string read FDisplayText;
    property OperatorKind: TcxFilterOperatorKind read FOperatorKind;
    property Value: Variant read FValue;
  end;

  { TdxFilterValueContainerIncrementalFilteringHelper }

  TdxFilterValueContainerIncrementalFilteringHelper = class(TdxCustomIncrementalFilteringContainerHelper)
  strict private
    FOwner: TdxFilterValueContainer;

    function GetListBox: TdxFilterValueContainerListBox;
  protected
    function GetItemCount: Integer; override;
    function GetItemIndex: Integer; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetVisibleItemCount: Integer; override;
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure SearchEditMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean); override;
    procedure SearchEditValueChanged(Sender: TObject); override;
    procedure SetItemIndex(const Value: Integer); override;

    property ListBox: TdxFilterValueContainerListBox read GetListBox;
    property Owner: TdxFilterValueContainer read FOwner;
  public
    constructor Create(AOwner: TdxFilterValueContainer);
  end;

  { TdxCustomFilterValueContainerListBox }

  TdxCustomFilterValueContainerListBox = class(TdxCustomCheckListBox)
  strict private
    FFilterValueContainer: TdxCustomFilterValueContainer;
    FIncrementalFilteringText: string;
  protected
    procedure ApplyCheckAction(AItemIndex: Integer); virtual;
    procedure ApplyClickAction(AItemIndex: Integer); virtual;
    procedure ApplyFilter; virtual;
    function CanFocusIndex(AItemIndex: Integer): Boolean; override;
    procedure DoUpdateItems; virtual;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DrawItemText(const ARect: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure FilterValuesChanged; virtual;
    function GetCheckedItemIndexes: TdxIntegerIndexes; virtual;
    function GetItemIndexByFilterValueIndex(AIndex: Integer): Integer; virtual;
    function GetFilterValue(AItemIndex: Integer): TcxFilterValueItem; virtual;
    function GetFilterValueIndexByItemIndex(AItemIndex: Integer): Integer; virtual;
    function HasCheckBox(AItemIndex: Integer): Boolean; override;
    function HasIncrementalFiltering: Boolean; virtual;
    function IsCheck(AItemIndex: Integer): Boolean; virtual;
    function IsFilterValueVisible(AFilterValue: TcxFilterValueItem; ACaption: string): Boolean; virtual;
    function IsMRUSeparatorItem(AItemIndex: Integer): Boolean; virtual;
    function IsFilterValueCheck(AValue: TcxFilterValueItem): Boolean; virtual;
    procedure DoItemAction(AItemIndex: Integer); override;
    function NeedHighlightIncrementalFilteringText: Boolean; virtual;
    procedure UpdateCheckedFilterValueIndexes(AItemIndex: Integer); virtual;
    procedure UpdateItems;
    procedure UpdateItemStates; virtual;
    function UseContainsIncrementalFiltering: Boolean; virtual;

    property IncrementalFilteringText: string read FIncrementalFilteringText write FIncrementalFilteringText;
  public
    constructor Create(AFilterValueContainer: TdxCustomFilterValueContainer); reintroduce; virtual;

    property FilterValueContainer: TdxCustomFilterValueContainer read FFilterValueContainer;
  end;

  { TdxFilterValueContainerListBox }

  TdxFilterValueContainerListBox = class(TdxCustomFilterValueContainerListBox)
  strict private
    function GetFilterValueContainer: TdxFilterValueContainer;
  protected
    procedure ApplyClickAction(AItemIndex: Integer); override;
    procedure DoClick; override;
    function IsFilterValueVisible(AFilterValue: TcxFilterValueItem; ACaption: string): Boolean; override;
    function NeedHighlightIncrementalFilteringText: Boolean; override;
    function NeedHotTrack: Boolean; override;
    function NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean; override;
    procedure UpdateItemStates; override;
    function UseContainsIncrementalFiltering: Boolean; override;
  public
    function CanFocus: Boolean; override;

    property FilterValueContainer: TdxFilterValueContainer read GetFilterValueContainer;
  end;

  { TdxExcelFilterValueContainerListBox }

  TdxExcelFilterValueContainerListBox = class(TdxCustomFilterValueContainerListBox)
  strict private const
    dxAllItemIndex = 0;
  protected
    procedure ApplyAllItemAction(AChecked: Boolean); virtual;
    procedure ApplyCheckAction(AItemIndex: Integer); override;
    procedure DoUpdateItems; override;
    function GetAllItem: TdxCustomCheckListBoxItem; virtual;
    function GetDefaultBounds: TRect; virtual;
    function IsFilterValueCheck(AValue: TcxFilterValueItem): Boolean; override;
    function NeedHandleClick: Boolean; override;
    procedure UpdateAllItemState; virtual;
    procedure UpdateCheckedFilterValueIndexes(AItemIndex: Integer); override;
    procedure UpdateItemStates; override;
  public
    constructor Create(AFilterValueContainer: TdxCustomFilterValueContainer); override;
  end;
  TdxExcelFilterValueContainerListBoxClass = class of TdxExcelFilterValueContainerListBox;

  { TdxFilterValueContainerViewInfo }

  TdxFilterValueContainerViewInfo = class(TdxCustomIncrementalFilteringContainerViewInfo)
  strict private
    function GetFilterValueContainer: TdxFilterValueContainer;
  public
    function IsSearchInfoPanelVisible: Boolean; override;

    property FilterValueContainer: TdxFilterValueContainer read GetFilterValueContainer;
  end;

  { TdxCustomFilterValueContainer }

  TdxCustomFilterValueContainer = class(TcxContainer)
  strict private
    FCheckedValueIndexes: TdxIntegerIndexes;
    FDisplayValues: TStringList;
    FFilter: TcxFilterCriteria;
    FFilterableComponent: IdxFilterableComponent;
    FIsModified: Boolean;
    FFilterValuesClass: TcxFilterValueListClass;
    FLinkComponent: TComponent;
    FLockCount: Integer;
    FValues: TcxFilterValueList;

    procedure SetIsModified(AValue: Boolean);
    procedure SetLinkComponent(AValue: TComponent);
  protected
    procedure AddCheckedIndex(AIndex: Integer); virtual;
    function CreateDisplayValues: TStringList; virtual;
    function CreateValues: TcxFilterValueList; virtual;
    procedure DataChanged; virtual;
    procedure DeleteCheckedIndex(AIndex: Integer); virtual;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      AIsMouseEvent: Boolean): Boolean; override;
    procedure DoUpdateFiltering; virtual;
    procedure DoUpdateLayout; virtual;
    procedure ModifiedChanged; virtual;
    function GetDefaultBounds: TRect; virtual;
    function NeedImmediateApply: Boolean; virtual;
    function IsChildFocused: Boolean; virtual;
    function IsContainerClass: Boolean; override;
    function IsContainerFocused: Boolean; override;
    function IsLinkComponentSupported(AValue: TComponent): Boolean; virtual;
    function IsValueCheck(AValue: TcxFilterValueItem): Boolean; virtual;
    procedure LayoutChanged; virtual;
    procedure LinkComponentChanged; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopulateCheckedValueIndexes; virtual;
    procedure PopulateValues; virtual;
    procedure ResetCheckedValueIndexes; virtual;
    procedure SetActiveValueIndex(AFilterValueIndex: Integer); virtual;
    procedure SetActiveValueIndexes; virtual;
    procedure SetAllCheckedValueIndexes; virtual;
    procedure SetChildFocus; virtual;
    procedure UpdateFiltering;
    procedure UpdateLayout;
    procedure UpdateValues; virtual;

    property CheckedValueIndexes: TdxIntegerIndexes read FCheckedValueIndexes;
    property DisplayValues: TStringList read FDisplayValues;
    property Filter: TcxFilterCriteria read FFilter;
    property FilterableComponent: IdxFilterableComponent read FFilterableComponent;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property FilterValuesClass: TcxFilterValueListClass read FFilterValuesClass;
    property Values: TcxFilterValueList read FValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsLocked: Boolean;

    function Focused: Boolean; override;
    procedure GetTabOrderList(List: TList); override;
    procedure SetFocus; override;

    procedure ApplyFilter; virtual;
    procedure FilteringApplied; virtual;

    property LinkComponent: TComponent read FLinkComponent write SetLinkComponent;
    property Style;
  end;
  TdxCustomFilterValueContainerClass = class of TdxCustomFilterValueContainer;

  { TdxFilterValueContainer }

  TdxFilterValueContainer = class(TdxCustomFilterValueContainer)
  strict private
    FApplyMode: TdxFilterApplyChangesMode;
    FButton: TcxButton;
    FIncHelper: TdxFilterValueContainerIncrementalFilteringHelper;
    FIncrementalFiltering: Boolean;
    FIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
    FListBox: TdxFilterValueContainerListBox;
    FShowCheckBoxes: Boolean;

    function GetButtonCaption: string;
    function GetSearchEdit: TcxCustomTextEdit;
    function GetSearchEditOffsets: TRect;
    function GetSearchText: string;
    function GetSelectedValueIndex: Integer;
    function GetViewInfo: TdxFilterValueContainerViewInfo;
    procedure SetApplyMode(const AValue: TdxFilterApplyChangesMode);
    procedure SetButtonCaption(const AValue: string);
    procedure SetIncrementalFiltering(const AValue: Boolean);
    procedure SetIncrementalFilteringOptions(const AValue: TcxTextEditIncrementalFilteringOptions);
    procedure SetShowCheckBoxes(const AValue: Boolean);

    procedure ButtonClickHandler(Sender: TObject);
  protected
    procedure ApplyFilterOnButtonClick; virtual;
    function CreateButton: TcxButton; virtual;
    function CreateIncHelper: TdxFilterValueContainerIncrementalFilteringHelper; virtual;
    function CreateListBox: TdxFilterValueContainerListBox; virtual;
    procedure DoUpdateLayout; override;
    procedure ModifiedChanged; override;
    procedure FocusSearchEdit; virtual;
    function GetDefaultButtonCaption: string; virtual;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function NeedImmediateApply: Boolean; override;
    function HasIncrementalFiltering: Boolean; virtual;
    procedure InitButton; virtual;
    procedure InitIncHelper; virtual;
    procedure InitListBox; virtual;
    function IsButtonCaptionStored: Boolean; virtual;
    function IsButtonVisible: Boolean; virtual;
    function IsChildFocused: Boolean; override;
    function IsSearchInfoPanelVisible: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure RefreshChildFocus; virtual;
    procedure ResetSearchText; virtual;
    procedure SetChildFocus; override;
    procedure UpdateButtonEnabled; virtual;
    procedure UpdateButtonPosition; virtual;
    procedure UpdateValues; override;
    procedure UpdateListBoxIndexes; virtual;
    procedure UpdateListBoxItems; virtual;
    procedure UpdateListBoxPosition; virtual;
    procedure UpdateSearchEditPosition; virtual;
    procedure UpdateSearchInfoPanelPosition; virtual;

    property Button: TcxButton read FButton;
    property DefaultButtonCaption: string read GetDefaultButtonCaption;
    property IncHelper: TdxFilterValueContainerIncrementalFilteringHelper read FIncHelper;
    property ListBox: TdxFilterValueContainerListBox read FListBox;
    property SearchEdit: TcxCustomTextEdit read GetSearchEdit;
    property SearchEditOffsets: TRect read GetSearchEditOffsets;
    property SearchText: string read GetSearchText;
    property SelectedValueIndex: Integer read GetSelectedValueIndex;
    property ViewInfo: TdxFilterValueContainerViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; override;

    property ApplyMode: TdxFilterApplyChangesMode read FApplyMode write SetApplyMode default fpacImmediately;
    property ButtonCaption: string read GetButtonCaption write SetButtonCaption stored IsButtonCaptionStored;
    property IncrementalFiltering: Boolean read FIncrementalFiltering write SetIncrementalFiltering default False;
    property IncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions read FIncrementalFilteringOptions
      write SetIncrementalFilteringOptions default [ifoHighlightSearchText, ifoUseContainsOperator];
    property ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes default True;
  end;

  { TdxExcelFilterValueContainerCustomDateTimeTreeView }

  TdxExcelFilterValueContainerCustomDateTimeTreeView = class(TdxCustomTreeView)
  strict private type
    TItemStateChangedType = (sctNone, sctUpdateStates, sctLvl2ValueStateChanged,
      sctLvl1ValueStateChanged, sctLvl0ValueStateChanged, sctAllNodeStateChanged);
  strict private
    FFilterValueContainer: TdxExcelFilterValueContainer;
    FIncrementalFilteringText: string;
    FItemStateChangedType: TItemStateChangedType;

    procedure SetIncrementalFilteringText(const AValue: string);
  protected
    procedure ApplyFilter; virtual;
    procedure FilterValuesChanged; virtual;
    function GetFilterValueIndexByNode(ANode: TdxTreeViewNode): Integer; virtual;
    function GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string; virtual;
    function GetNodeByFilterValue(AFilterValue: TcxFilterValueItem): TdxTreeViewNode; virtual;
    function HasIncrementalFiltering: Boolean; virtual;
    function IsAllNode(ANode: TdxTreeViewNode): Boolean; virtual;
    function IsTextVisible(AText: string): Boolean; virtual;
    function IsValueVisible(AFilterValue: TcxFilterValueItem; AText: string): Boolean; virtual;
    procedure NodeStateChanged(ANode: TdxTreeViewNode); override;
    procedure PopulateItems; virtual;
    procedure UpdateDayItemStates; virtual;
    procedure UpdateItems; virtual;
    procedure UpdateItemStates; virtual;
    procedure UpdateParentItemStates; virtual;

    property FilterValueContainer: TdxExcelFilterValueContainer read FFilterValueContainer;
    property IncrementalFilteringText: string read FIncrementalFilteringText write SetIncrementalFilteringText;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); reintroduce; virtual;
  end;

  { TdxExcelFilterValueContainerTimeTreeView }

  TdxExcelFilterValueContainerTimeTreeView = class(TdxExcelFilterValueContainerCustomDateTimeTreeView)
  strict private
    FHourFormat: string;
  protected
    function GetHourFormat: string; virtual;
    function GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string; override;

    property HourFormat: string read FHourFormat;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); override;
  end;

  { TdxExcelFilterValueContainerDateTreeView }

  TdxExcelFilterValueContainerDateTreeView = class(TdxExcelFilterValueContainerCustomDateTimeTreeView)
  protected
    function GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string; override;
  end;

  { TdxExcelFilterValueContainerValuesComboBoxCalendar }

  TdxExcelFilterValueContainerValuesComboBoxCalendar = class(TcxCustomCalendar);

  { TdxExcelFilterValueContainerComboBox }

  TdxExcelFilterValueContainerComboBox = class(TcxComboBox)
  strict private
    FFilterValueContainer: TdxExcelFilterValueContainer;
  protected
    procedure PopulateItems; virtual;
    procedure UpdateItems; virtual;

    property FilterValueContainer: TdxExcelFilterValueContainer read FFilterValueContainer;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); reintroduce; virtual;
  end;

  { TdxExcelFilterValueContainerValuesComboBoxProperties }

  TdxExcelFilterValueContainerValuesComboBoxProperties = class(TcxComboBoxProperties)
  strict private
    FFilterValueContainer: TdxExcelFilterValueContainer;
  protected
    function IsEditValueNumeric: Boolean; override;

    property FilterValueContainer: TdxExcelFilterValueContainer read FFilterValueContainer write FFilterValueContainer;
  end;

  { TdxExcelFilterValueContainerValuesComboBox }

  TdxExcelFilterValueContainerValuesComboBox = class(TdxExcelFilterValueContainerComboBox)
  strict private
    FCalendar: TdxExcelFilterValueContainerValuesComboBoxCalendar;
    FCalendarButton: TcxEditButton;
    FCalendarPopup: TdxUIElementPopupWindow;

    procedure ButtonClickHandler(Sender: TObject; AButtonIndex: Integer);
    procedure DateTimeChangedHandler(Sender: TObject);
    function GetProperties: TdxExcelFilterValueContainerValuesComboBoxProperties;
    procedure SetProperties(Value: TdxExcelFilterValueContainerValuesComboBoxProperties);
    procedure ValidateHandler(ASender: TObject; var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);
  protected
    procedure AddCalendar; virtual;
    procedure AddCalendarButton; virtual;
    procedure CalendarButtonClick; virtual;
    procedure CalendarDropDown; virtual;
    function CreateCalendar: TdxExcelFilterValueContainerValuesComboBoxCalendar; virtual;
    function CreateCalendarPopup: TdxUIElementPopupWindow; virtual;
    function GetFilterDisplayText: string; virtual;
    function GetFilterValue: Variant; virtual;
    procedure Initialize; override;
    procedure InitProperties; virtual;
    procedure InitPropertiesFormat; virtual;
    procedure InitPropertiesMask; virtual;
    function IsEmpty: Boolean; virtual;
    function IsValueValid(AValue: Variant): Boolean; virtual;
    procedure PopulateItems; override;

    property Calendar: TdxExcelFilterValueContainerValuesComboBoxCalendar read FCalendar;
    property CalendarButton: TcxEditButton read FCalendarButton;
    property CalendarPopup: TdxUIElementPopupWindow read FCalendarPopup;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); override;
    destructor Destroy; override;

    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property Properties: TdxExcelFilterValueContainerValuesComboBoxProperties read GetProperties write SetProperties;
  end;

  { TdxExcelFilterValueContainerConditionTypesComboBox }

  TdxExcelFilterValueContainerConditionTypesComboBox = class(TdxExcelFilterValueContainerComboBox)
  strict private
    FSupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes;
  protected
    procedure PopulateItems; override;

    property SupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes read FSupportedConditionTypes
      write FSupportedConditionTypes;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); override;
  end;

  { TdxExcelFilterValueContainerCustomItem }

  TdxExcelFilterValueContainerCustomItem = class
  strict private
    FFilterValueContainer: TdxExcelFilterValueContainer;
    FParentGroup: TdxLayoutGroup;

    function GetFilterableComponent: IdxExcelFilterableComponent;
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); virtual;
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure UpdateData; virtual;

    property FilterableComponent: IdxExcelFilterableComponent read GetFilterableComponent;
    property FilterValueContainer: TdxExcelFilterValueContainer read FFilterValueContainer;
    property ParentGroup: TdxLayoutGroup read FParentGroup;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); virtual;

    procedure Generate(AParentGroup: TdxLayoutGroup);
  end;

  { TdxExcelFilterValueContainerCondition }

  TdxExcelFilterValueContainerCondition = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    FConditionContainer: TdxExcelFilterValueContainerConditionContainer;
  protected
    function GetBoolOperator: TcxFilterBoolOperatorKind; virtual;
    function GetDisplayText: string; virtual;
    function GetOperator: TcxFilterOperatorKind; virtual;
    function GetValue: Variant; virtual;
    function IsAppliedOnChoosing: Boolean; virtual;
    function IsComplex: Boolean; virtual;
    function IsEditable: Boolean; virtual;
    function IsEmpty: Boolean; virtual;
    function IsPresentInFilter: Boolean; virtual;
    procedure PopulateValueInfos(AList: TdxFastObjectList); virtual;
    procedure SetCaption(AValue: string); virtual;

    property ConditionContainer: TdxExcelFilterValueContainerConditionContainer read FConditionContainer;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer;
      AConditionContainer: TdxExcelFilterValueContainerConditionContainer); reintroduce; virtual;
  end;
  TdxExcelFilterValueContainerConditionClass = class of TdxExcelFilterValueContainerCondition;

  { TdxExcelFilterValueContainerAverageCondition }

  TdxExcelFilterValueContainerAverageCondition = class(TdxExcelFilterValueContainerCondition)
  private
    FAverageValue: Variant;
  protected
    procedure CalculateAverageValue; virtual;
    function GetDisplayText: string; override;
    function GetValue: Variant; override;
    function IsEmpty: Boolean; override;
    procedure UpdateData; override;
  end;

  { TdxExcelFilterValueContainerEditableCondition }

  TdxExcelFilterValueContainerEditableCondition = class(TdxExcelFilterValueContainerCondition)
  protected
    procedure EditChangeHandler(ASender: TObject); virtual;
    function IsEditable: Boolean; override;
    procedure UpdateData; override;
    procedure UpdateValues; virtual;
    procedure ValueChanged; virtual;
  end;

  { TdxExcelFilterValueContainerTextEditCondition }

  TdxExcelFilterValueContainerTextEditCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FTextEdit: TcxTextEdit;
    FTextEditLayoutItem: TdxLayoutItem;
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateTextEdit; virtual;
    function GetDisplayText: string; override;
    function GetValue: Variant; override;
    function IsEmpty: Boolean; override;
    procedure SetCaption(AValue: string); override;
    procedure UpdateValues; override;

    property TextEdit: TcxTextEdit read FTextEdit;
    property TextEditLayoutItem: TdxLayoutItem read FTextEditLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerComboBoxCondition }

  TdxExcelFilterValueContainerComboBoxCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FComboBox: TdxExcelFilterValueContainerValuesComboBox;
    FComboBoxLayoutItem: TdxLayoutItem;
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateComboBox; virtual;
    function GetDisplayText: string; override;
    function GetValue: Variant; override;
    function IsEmpty: Boolean; override;
    procedure SetCaption(AValue: string); override;
    procedure UpdateData; override;
    procedure UpdateValues; override;

    property ComboBox: TdxExcelFilterValueContainerValuesComboBox read FComboBox;
    property ComboBoxLayoutItem: TdxLayoutItem read FComboBoxLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerBetweenCondition }

  TdxExcelFilterValueContainerBetweenCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FFromComboBox: TdxExcelFilterValueContainerValuesComboBox;
    FFromLayoutItem: TdxLayoutItem;
    FToComboBox: TdxExcelFilterValueContainerValuesComboBox;
    FToLayoutItem: TdxLayoutItem;
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateFromComboBox; virtual;
    procedure GenerateToComboBox; virtual;
    function GetDisplayText: string; override;
    function GetOperator: TcxFilterOperatorKind; override;
    function GetValue: Variant; override;
    function GetValueFrom: Variant; virtual;
    function GetValueTo: Variant; virtual;
    function IsEmpty: Boolean; override;
    procedure UpdateData; override;
    procedure UpdateValues; override;

    property FromComboBox: TdxExcelFilterValueContainerValuesComboBox read FFromComboBox;
    property FromLayoutItem: TdxLayoutItem read FFromLayoutItem;
    property ToComboBox: TdxExcelFilterValueContainerValuesComboBox read FToComboBox;
    property ToLayoutItem: TdxLayoutItem read FToLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCounterCondition }

  TdxExcelFilterValueContainerCounterCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FDisplayText: string;
    FFilterValueItemCount: Integer;
    FValue: Variant;
    FValueLayoutItem: TdxLayoutItem;
    FValueSpinEdit: TcxSpinEdit;
    FValueTypeComboBox: TcxComboBox;
    FValueTypeLayoutItem: TdxLayoutItem;

    procedure ValueTypeChangedHandler(ASender: TObject);
  protected
    function IsAppliedOnChoosing: Boolean; override;
    procedure CalculateFilterValueItemCount; virtual;
    procedure CalculateValues; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateValueSpinEdit; virtual;
    procedure GenerateValueTypeComboBox; virtual;
    function GetDisplayText: string; override;
    function GetOperator: TcxFilterOperatorKind; override;
    function GetValueSpinEditMaxCount: Integer; virtual;
    function GetValue: Variant; override;
    function IsEmpty: Boolean; override;
    procedure UpdateData; override;
    procedure UpdateValueSpinEditMinMax; virtual;
    procedure ValueChanged; override;

    property DisplayText: string read FDisplayText;
    property FilterValueItemCount: Integer read FFilterValueItemCount;
    property Value: Variant read FValue;
    property ValueLayoutItem: TdxLayoutItem read FValueLayoutItem;
    property ValueSpinEdit: TcxSpinEdit read FValueSpinEdit;
    property ValueTypeComboBox: TcxComboBox read FValueTypeComboBox;
    property ValueTypeLayoutItem: TdxLayoutItem read FValueTypeLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod }

  TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    FCheckBoxList: TdxFastObjectList;
    FGroup: TdxLayoutGroup;

    function GetCheckBox(AIndex: Integer): TcxCheckBox;
    function GetCheckBoxCount: Integer;
  protected
    function AddCheckBox(AOperator: TcxFilterOperatorKind): TcxCheckBox; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateGroup; virtual;
    procedure GenerateLayoutItems; virtual;
    function GetCheckedCount: Integer; virtual;
    procedure UpdateCheckBoxStates(AOperators: TcxFilterOperatorKinds); virtual;

    property CheckBoxes[AIndex: Integer]: TcxCheckBox read GetCheckBox;
    property CheckBoxCount: Integer read GetCheckBoxCount;
    property CheckBoxList: TdxFastObjectList read FCheckBoxList;
    property Group: TdxLayoutGroup read FGroup;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); override;
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerSpecificDatePeriodsCondition }

  TdxExcelFilterValueContainerSpecificDatePeriodsCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FDayWeekPeriodsGroup: TdxLayoutGroup;
    FPeriodsGroup: TdxLayoutGroup;
    FPeriodList: TdxFastObjectList;
    FMonthYearPeriodsGroup: TdxLayoutGroup;

    function GetPeriod(AIndex: Integer): TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
    function GetPeriodCount: Integer;
  protected
    function AddPeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod; virtual;
    procedure AddPeriodCheckBox(APeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
      AOperator: TcxFilterOperatorKind); virtual;
    function CreatePeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateGroups; virtual;
    procedure GeneratePeriods; virtual;
    function GetBoolOperator: TcxFilterBoolOperatorKind; override;
    function GetCheckedCount: Integer; virtual;
    function GetOperator: TcxFilterOperatorKind; override;
    function IsComplex: Boolean; override;
    function IsEmpty: Boolean; override;
    procedure PopulateValueInfos(AList: TdxFastObjectList); override;
    procedure UpdatePeriodCheckBoxStates(AOperators: TcxFilterOperatorKinds); virtual;
    procedure UpdateValues; override;

    property DayWeekPeriodsGroup: TdxLayoutGroup read FDayWeekPeriodsGroup;
    property PeriodCount: Integer read GetPeriodCount;
    property PeriodList: TdxFastObjectList read FPeriodList;
    property Periods[AIndex: Integer]: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod read GetPeriod;
    property PeriodsGroup: TdxLayoutGroup read FPeriodsGroup;
    property MonthYearsPeriodsGroup: TdxLayoutGroup read FMonthYearPeriodsGroup;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCustomFilterConditionAndOr }

  TdxExcelFilterValueContainerCustomFilterConditionAndOr = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    FAndLayoutItem: TdxLayoutItem;
    FAndRadioButton: TcxRadioButton;
    FOrLayoutItem: TdxLayoutItem;
    FOrRadioButton: TcxRadioButton;
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateAndRadioButton; virtual;
    procedure GenerateOrRadioButton; virtual;

    property AndLayoutItem: TdxLayoutItem read FAndLayoutItem;
    property AndRadioButton: TcxRadioButton read FAndRadioButton;
    property OrLayoutItem: TdxLayoutItem read FOrLayoutItem;
    property OrRadioButton: TcxRadioButton read FOrRadioButton;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCustomFilterCondition }

  TdxExcelFilterValueContainerCustomFilterCondition = class(TdxExcelFilterValueContainerEditableCondition)
  strict private
    FAndOr: TdxExcelFilterValueContainerCustomFilterConditionAndOr;
    FAndOrGroup: TdxLayoutGroup;
    FChildConditionContainer1: TdxExcelFilterValueContainerCustomFilterConditionContainer;
    FChildConditionContainer2: TdxExcelFilterValueContainerCustomFilterConditionContainer;
  protected
    procedure AndOrClickHandler(ASender: TObject); virtual;
    function CreateConditionContainer: TdxExcelFilterValueContainerCustomFilterConditionContainer; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure GenerateAndOr; virtual;
    procedure GenerateChildConditionContainer1; virtual;
    procedure GenerateChildConditionContainer2; virtual;
    function GetBoolOperator: TcxFilterBoolOperatorKind; override;
    function GetDisplayText: string; override;
    function GetOperator: TcxFilterOperatorKind; override;
    function GetSupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes; virtual;
    function GetValue: Variant; override;
    function IsComplex: Boolean; override;
    function IsEmpty: Boolean; override;
    procedure PopulateActiveCriteriaItems(AContainer: TdxExcelFilterValueContainerCustomFilterConditionContainer); virtual;
    procedure PopulateValueInfos(AList: TdxFastObjectList); override;
    procedure SetAndOrClickHandler(AEvent: TNotifyEvent); virtual;
    procedure SetAndOrKeyDownHandler(AEvent: TKeyEvent); virtual;
    procedure UpdateData; override;
    procedure UpdateValues; override;

    property AndOr: TdxExcelFilterValueContainerCustomFilterConditionAndOr read FAndOr;
    property AndOrGroup: TdxLayoutGroup read FAndOrGroup;
    property ChildConditionContainer1: TdxExcelFilterValueContainerCustomFilterConditionContainer read FChildConditionContainer1;
    property ChildConditionContainer2: TdxExcelFilterValueContainerCustomFilterConditionContainer read FChildConditionContainer2;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerConditionContainer }

  TdxExcelFilterValueContainerConditionContainer = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    FActiveBoolOperator: TcxFilterBoolOperatorKind;
    FActiveCriteriaItems: TList<TcxFilterCriteriaItem>;
    FCondition: TdxExcelFilterValueContainerCondition;
    FConditionTypeComboBox: TdxExcelFilterValueContainerConditionTypesComboBox;
    FConditionTypeLayoutItem: TdxLayoutItem;
    FCustomFilterConditionClass: TdxExcelFilterValueContainerConditionClass;
    FLayoutGroup: TdxLayoutGroup;
    FNeedUpdateConditionTypeItems: Boolean;
    FSupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes;

    procedure ConditionTypeChangedHandler(Sender: TObject);
    function PopulateActiveCriteriaItemsByAndItemList(AItemList: TcxFilterCriteriaItemList): Boolean;
    function PopulateActiveCriteriaItemsByOrItemList(AItemList: TcxFilterCriteriaItemList): Boolean;
    procedure SetSupportedConditionTypes(const AValue: TdxExcelFilterValueContainerConditionTypes);
  protected
    procedure ApplyFilter; virtual;
    procedure ConditionChanged; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FilterValuesChanged; virtual;
    procedure FocusMainItem; virtual;
    procedure GenerateCondition; virtual;
    procedure GenerateConditionTypeComboBox; virtual;
    procedure GenerateLayoutGroup; virtual;
    function GetActiveConditionType(out AHasActiveCondition: Boolean): TdxExcelFilterValueContainerConditionType; virtual;
    function GetConditionType: TdxExcelFilterValueContainerConditionType; virtual;
    function GetConditionTypeItemIndex(AConditionType: TdxExcelFilterValueContainerConditionType): Integer; virtual;
    function GetDefaultConditionTypeIndex: Integer; virtual;
    function HasSelectedConditionType: Boolean; virtual;
    function IsActiveCriteriaItemsContainBetweenCondition: Boolean; virtual;
    function IsConditionPresentInFilter: Boolean; virtual;
    function IsConditionEmpty: Boolean; virtual;
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function PopulateActiveCriteriaItems(AItemList: TcxFilterCriteriaItemList): Boolean; virtual;
    procedure RegenerateCondition; virtual;
    procedure SetCaption(AValue: string); virtual;
    procedure UpdateActiveCriteriaItems; virtual;
    procedure UpdateConditionCaption; virtual;
    procedure UpdateConditionType; virtual;
    procedure UpdateData; override;

    property ActiveBoolOperator: TcxFilterBoolOperatorKind read FActiveBoolOperator;
    property ActiveCriteriaItems: TList<TcxFilterCriteriaItem> read FActiveCriteriaItems;
    property Condition: TdxExcelFilterValueContainerCondition read FCondition;
    property ConditionTypeComboBox: TdxExcelFilterValueContainerConditionTypesComboBox read FConditionTypeComboBox;
    property ConditionTypeLayoutItem: TdxLayoutItem read FConditionTypeLayoutItem;
    property CustomFilterConditionClass: TdxExcelFilterValueContainerConditionClass read FCustomFilterConditionClass
      write FCustomFilterConditionClass;
    property LayoutGroup: TdxLayoutGroup read FLayoutGroup;
    property NeedUpdateConditionTypeItems: Boolean read FNeedUpdateConditionTypeItems;
    property SupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes read FSupportedConditionTypes
      write SetSupportedConditionTypes;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer); override;
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCustomFilterConditionContainer }

  TdxExcelFilterValueContainerCustomFilterConditionContainer = class(TdxExcelFilterValueContainerConditionContainer)
  strict private
    FCustomFilterCondition: TdxExcelFilterValueContainerCustomFilterCondition;
  protected
    procedure ApplyFilter; override;
    function PopulateActiveCriteriaItems(AItemList: TcxFilterCriteriaItemList): Boolean; override;

    property CustomFilterCondition: TdxExcelFilterValueContainerCustomFilterCondition read FCustomFilterCondition;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer;
      ACustomFilterCondition: TdxExcelFilterValueContainerCustomFilterCondition); reintroduce; virtual;
  end;

  { TdxExcelFilterValueContainerRangeModeValuesPageBetween }

  TdxExcelFilterValueContainerRangeModeValuesPageBetween = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    FFromEdit: TcxCustomEdit;
    FFromLayoutItem: TdxLayoutItem;
    FOwner: TdxExcelFilterValueContainerRangeModeValuesPage;
    FToEdit: TcxCustomEdit;
    FToLayoutItem: TdxLayoutItem;

    procedure EditChangeHandler(ASender: TObject);
    procedure EditValidateHandler(ASender: TObject; var ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean);
  protected
    function CreateEdit: TcxCustomEdit; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FocusMainItem; virtual;
    procedure GenerateFromEdit; virtual;
    procedure GenerateToEdit; virtual;
    procedure InitEdit(AEdit: TcxCustomEdit); virtual;
    function IsValueValid(AValue: Variant): Boolean; virtual;
    procedure UpdateData; override;
    procedure UpdateEditValue(AEdit: TcxCustomEdit; AValue: Variant); virtual;
    procedure ValueChanged; virtual;

    property FromEdit: TcxCustomEdit read FFromEdit;
    property FromLayoutItem: TdxLayoutItem read FFromLayoutItem;
    property Owner: TdxExcelFilterValueContainerRangeModeValuesPage read FOwner;
    property ToEdit: TcxCustomEdit read FToEdit;
    property ToLayoutItem: TdxLayoutItem read FToLayoutItem;
  public
    constructor Create(AFilterValueContainer: TdxExcelFilterValueContainer;
      AOwner: TdxExcelFilterValueContainerRangeModeValuesPage); reintroduce; virtual;
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCustomPage }

  TdxExcelFilterValueContainerCustomPage = class(TdxExcelFilterValueContainerCustomItem)
  protected
    procedure ApplyFilter; virtual;
    procedure FocusMainItem; virtual;
  end;

  { TdxExcelFilterValueContainerCustomValuesPage }

  TdxExcelFilterValueContainerCustomValuesPage = class(TdxExcelFilterValueContainerCustomPage);

  { TdxExcelFilterValueContainerSearchableValuesPage }

  TdxExcelFilterValueContainerSearchableValuesPage = class(TdxExcelFilterValueContainerCustomValuesPage)
  strict private
    FDelayedTimer: TcxTimer;
    FSearchEdit: TcxButtonEdit;
    FSearchLayoutItem: TdxLayoutItem;
    FSearchText: string;

    procedure CreateTimer;
    procedure DestroyTimer;
    procedure OnDelayedSearchTimer(Sender: TObject);
  protected
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure DrawSearchEditGlyph;
    procedure FocusMainItem; override;
    procedure GenerateSearchEdit; virtual;
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure SearchEditKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure SearchEditValueChanged(ASender: TObject); virtual;
    procedure SearchTextChanged; virtual;
    procedure StartDelayedSearch; virtual;
    procedure UpdateData; override;
    procedure UpdateValues; virtual;

    property SearchEdit: TcxButtonEdit read FSearchEdit;
    property SearchLayoutItem: TdxLayoutItem read FSearchLayoutItem;
    property SearchText: string read FSearchText;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerListBoxModeValuesPage }

  TdxExcelFilterValueContainerListBoxModeValuesPage = class(TdxExcelFilterValueContainerSearchableValuesPage)
  strict private
    FListBox: TdxExcelFilterValueContainerListBox;
    FListBoxLayoutItem: TdxLayoutItem;
  protected
    procedure ApplyFilter; override;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FocusMainItem; override;
    procedure GenerateListBox; virtual;
    procedure UpdateValues; override;

    property ListBox: TdxExcelFilterValueContainerListBox read FListBox;
    property ListBoxLayoutItem: TdxLayoutItem read FListBoxLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage }

  TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage = class(TdxExcelFilterValueContainerSearchableValuesPage)
  strict private
    FTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView;
    FTreeViewLayoutItem: TdxLayoutItem;
  protected
    procedure ApplyFilter; override;
    function CreateTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FocusMainItem; override;
    procedure GenerateTreeView; virtual;
    procedure UpdateValues; override;

    property TreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView read FTreeView;
    property TreeViewLayoutItem: TdxLayoutItem read FTreeViewLayoutItem;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerDateTreeViewModeValuesPage }

  TdxExcelFilterValueContainerDateTreeViewModeValuesPage = class(TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage)
  protected
    function CreateTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView; override;
  end;

  { TdxExcelFilterValueContainerTimeTreeViewModeValuesPage }

  TdxExcelFilterValueContainerTimeTreeViewModeValuesPage = class(TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage)
  protected
    function CreateTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView; override;
  end;

  { TdxExcelFilterValueContainerRangeModeValuesPage }

  TdxExcelFilterValueContainerRangeModeValuesPage = class(TdxExcelFilterValueContainerCustomValuesPage)
  strict private
    FBetweenGroup: TdxLayoutGroup;
    FBetweenValues: TdxExcelFilterValueContainerRangeModeValuesPageBetween;
    FFromValue: Variant;
    FTracking: Boolean;
    FMaxValue: Variant;
    FMinValue: Variant;
    FToValue: Variant;
    FRangeLayoutItem: TdxLayoutItem;
    FRangeTrackBar: TdxRangeTrackBar;

    procedure BeginTracking;
    procedure EndTracking;

    //handlers
    procedure RangeTrackBarGetPositionHintHandler(Sender: TObject; const AMinPosition, AMaxPosition: Integer;
      var AHintText: string; var ACanShow, AIsHintMultiLine: Boolean);
    procedure RangeTrackBarMouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RangeTrackBarMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RangeTrackBarValueChangedHandler(AValue: TObject);
  protected
    procedure ApplyFilter; override;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FilterValuesChanged; virtual;
    procedure FocusMainItem; override;
    procedure GenerateFromToGroup; virtual;
    procedure GenerateFromToValues; virtual;
    procedure GenerateRangeTrackBar; virtual;
    function GetDisplayTextByValue(AValue: Variant): string; virtual;
    function GetRangeTrackBarFrequency: Integer; virtual;
    function GetRangeTrackBarMax: Variant; virtual;
    function GetRangeTrackBarMin: Variant; virtual;
    function GetRangeTrackBarRangeMax: Integer; virtual;
    function GetRangeTrackBarRangeMin: Integer; virtual;
    function IsIntegerValueType: Boolean; virtual;
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    function MapRangeTrackBarValueToValue(AValue: Integer): Variant; virtual;
    function MapValueToRangeTrackBarValue(AValue: Variant): Integer; virtual;
    procedure SetValues(AFromValue, AToValue: Variant); virtual;
    procedure TrackValuesChanged; virtual;
    procedure UpdateData; override;
    procedure UpdateFromToValues; virtual;
    procedure UpdateMaxMinValues; virtual;
    procedure UpdateRangeTrackBar; virtual;

    property BetweenGroup: TdxLayoutGroup read FBetweenGroup;
    property BetweenValues: TdxExcelFilterValueContainerRangeModeValuesPageBetween read FBetweenValues;
    property FromValue: Variant read FFromValue;
    property MaxValue: Variant read FMaxValue;
    property MinValue: Variant read FMinValue;
    property ToValue: Variant read FToValue;
    property RangeLayoutItem: TdxLayoutItem read FRangeLayoutItem;
    property RangeTrackBar: TdxRangeTrackBar read FRangeTrackBar;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerFiltersPage }

  TdxExcelFilterValueContainerFiltersPage = class(TdxExcelFilterValueContainerCustomPage)
  strict private
    FConditionContainer: TdxExcelFilterValueContainerConditionContainer;
  protected
    procedure ApplyFilter; override;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FocusMainItem; override;
    procedure GenerateConditionContainer; virtual;
    function GetSupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes; virtual;
    procedure UpdateData; override;

    property ConditionContainer: TdxExcelFilterValueContainerConditionContainer read FConditionContainer;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerLayout }

  TdxExcelFilterValueContainerLayout = class(TdxExcelFilterValueContainerCustomItem)
  strict private
    procedure TabChangedHandler(ASender: TObject);
  protected
    procedure ApplyFilter; virtual;
    procedure DestroyPageGroups; virtual;
    procedure DestroyPages; virtual;
    procedure DoGenerate(AParentGroup: TdxLayoutGroup); override;
    procedure FocusMainItem; virtual;
    procedure GeneratePageGroups; virtual;
    procedure GeneratePages; virtual;
    function GetActivePage: TdxExcelFilterValueContainerCustomPage; virtual;
    function GetDefaultPageIndex: Integer; virtual;
    procedure TabChanged; virtual;
    procedure UpdateData; override;
  public
    destructor Destroy; override;
  end;

  { TdxExcelFilterValueContainerValuesLayout }

  TdxExcelFilterValueContainerValuesLayout = class(TdxExcelFilterValueContainerLayout)
  strict private
    FValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
    FValuesPageGroup: TdxLayoutGroup;
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; virtual;
    procedure DestroyPageGroups; override;
    procedure DestroyPages; override;
    procedure GeneratePageGroups; override;
    procedure GeneratePages; override;
    procedure GenerateValuesPage; virtual;
    procedure GenerateValuesPageGroup; virtual;
    function GetActivePage: TdxExcelFilterValueContainerCustomPage; override;
    function GetDefaultPageIndex: Integer; override;
    function GetValuesPageCaption: string; virtual;

    property ValuesPage: TdxExcelFilterValueContainerCustomValuesPage read FValuesPage;
    property ValuesPageGroup: TdxLayoutGroup read FValuesPageGroup;
  end;

  { TdxExcelFilterValueContainerFiltersValuesLayouts }

  TdxExcelFilterValueContainerFiltersValuesLayout = class(TdxExcelFilterValueContainerValuesLayout)
  strict private
    FFiltersPage: TdxExcelFilterValueContainerFiltersPage;
    FFiltersPageGroup: TdxLayoutGroup;
  protected
    function CreateFiltersPage: TdxExcelFilterValueContainerFiltersPage; virtual;
    procedure DestroyPageGroups; override;
    procedure DestroyPages; override;
    procedure GenerateFiltersPage; virtual;
    procedure GenerateFiltersPageGroup; virtual;
    procedure GeneratePages; override;
    procedure GeneratePageGroups; override;
    function GetActivePage: TdxExcelFilterValueContainerCustomPage; override;
    function GetDefaultPageIndex: Integer; override;
    function GetFiltersPageCaption: string; virtual;
    procedure TabChanged; override;

    property FiltersPage: TdxExcelFilterValueContainerFiltersPage read FFiltersPage;
    property FiltersPageGroup: TdxLayoutGroup read FFiltersPageGroup;
  end;

  { TdxExcelFilterValueContainerCheckModeLayout }

  TdxExcelFilterValueContainerCheckModeLayout = class(TdxExcelFilterValueContainerValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
  end;

  { TdxExcelFilterValueContainerLookupModeLayout }

  TdxExcelFilterValueContainerLookupModeLayout = class(TdxExcelFilterValueContainerValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
  end;

  { TdxExcelFilterValueContainerDateTreeViewModeLayout }

  TdxExcelFilterValueContainerDateTreeViewModeLayout = class(TdxExcelFilterValueContainerFiltersValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
    function GetFiltersPageCaption: string; override;
  end;

  { TdxExcelFilterValueContainerTimeTreeViewModeLayout }

  TdxExcelFilterValueContainerTimeTreeViewModeLayout = class(TdxExcelFilterValueContainerFiltersValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
    function GetFiltersPageCaption: string; override;
  end;

  { TdxExcelFilterValueContainerListBoxModeLayout }

  TdxExcelFilterValueContainerListBoxModeLayout = class(TdxExcelFilterValueContainerFiltersValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
    function GetFiltersPageCaption: string; override;
  end;

  { TdxExcelFilterValueContainerRangeModeLayout }

  TdxExcelFilterValueContainerRangeModeLayout = class(TdxExcelFilterValueContainerFiltersValuesLayout)
  protected
    function CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage; override;
    function GetFiltersPageCaption: string; override;
  end;

  { TdxExcelFilterValueContainer }

  TdxExcelFilterValueContainer = class(TdxCustomFilterValueContainer)
  strict private
    FApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
    FDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
    FDefaultPage: TdxExcelFilterValueContainerDefaultPage;
    FFilterHelper: TcxCustomFilterEditHelperClass;
    FFilterItemLink: TObject;
    FFilterProperties: TcxCustomEditProperties;
    FFilterValueTypeClass: TcxValueTypeClass;
    FLayout: TdxExcelFilterValueContainerLayout;
    FLayoutControl: TdxLayoutControl;
    FLayoutLookAndFeel: TdxLayoutCxLookAndFeel;
    FNonUniqueDisplayValues: TStringList;
    FNonUniqueValues: TcxFilterValueList;
    FNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;

    procedure SetApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
    function GetFilterHelper: TcxCustomFilterEditHelperClass;
    function GetFilterableComponent: IdxExcelFilterableComponent;
    function GetLayoutContainer: TdxLayoutContainer;
    function GetNonUniqueDisplayValues: TStringList;
    function GetNonUniqueValues: TcxFilterValueList;
    procedure SetDateTimeValuesPageType(const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
    procedure SetDefaultPage(AValue: TdxExcelFilterValueContainerDefaultPage);
    procedure SetNumericValuesPageType(const AValue: TdxExcelFilterValueContainerNumericValuesPageType);

    //handlers
    procedure LayoutControlKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure ApplyModeChanged; virtual;
    function CreateLayout: TdxExcelFilterValueContainerValuesLayout; virtual;
    function CreateLayoutControl: TdxLayoutControl; virtual;
    function CreateLayoutLookAndFeel: TdxLayoutCxLookAndFeel; virtual;
    procedure DoUpdateFiltering; override;
    procedure DoUpdateLayout; override;
    procedure FocusLayoutMainItem; virtual;
    function GetDataType: TcxFilterDataType; virtual;
    function GetDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType; virtual;
    function GetDefaultBounds: TRect; override;
    function GetDisplayTextByValue(AValue: Variant): string; virtual;
    function GetListBoxClass: TdxExcelFilterValueContainerListBoxClass; virtual;
    function GetNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType; virtual;
    function GetType: TdxExcelFilterValueContainerType; virtual;
    procedure InitLayoutControl; virtual;
    procedure InitLayoutLookAndFeel; virtual;
    function IsLinkComponentSupported(AValue: TComponent): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LayoutKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState); virtual;
    function NeedImmediateApply: Boolean; override;
    procedure NonUniqueValuesNeeded; virtual;
    procedure PopulateNonUniqueValues; virtual;
    procedure PopulateValues; override;
    procedure UpdateLayoutControlPosition; virtual;
    procedure UpdateValues; override;

    property FilterableComponent: IdxExcelFilterableComponent read GetFilterableComponent;
    property FilterHelper: TcxCustomFilterEditHelperClass read GetFilterHelper;
    property FilterItemLink: TObject read FFilterItemLink;
    property FilterProperties: TcxCustomEditProperties read FFilterProperties;
    property FilterValueTypeClass: TcxValueTypeClass read FFilterValueTypeClass;
    property Layout: TdxExcelFilterValueContainerLayout read FLayout;
    property LayoutContainer: TdxLayoutContainer read GetLayoutContainer;
    property LayoutControl: TdxLayoutControl read FLayoutControl;
    property LayoutLookAndFeel: TdxLayoutCxLookAndFeel read FLayoutLookAndFeel;
    property NonUniqueDisplayValues: TStringList read GetNonUniqueDisplayValues;
    property NonUniqueValues: TcxFilterValueList read GetNonUniqueValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; override;
    procedure ResetFilter; virtual;

    property ApplyChanges: TdxExcelFilterValueContainerApplyChangesMode read FApplyChanges write SetApplyChanges default efacDefault;
    property DateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType read FDateTimeValuesPageType
      write SetDateTimeValuesPageType default dvptDefault;
    property DefaultPage: TdxExcelFilterValueContainerDefaultPage read FDefaultPage write SetDefaultPage default dpDefault;
    property NumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType read FNumericValuesPageType
      write SetNumericValuesPageType default nvptDefault;
  end;

implementation

uses
  Types, SysUtils, Graphics, Math, Variants, DateUtils, dxGDIPlusClasses, cxGeometry, cxDrawTextUtils, cxEditConsts, cxTrackBar,
  cxFormats, cxFilterConsts, cxVariants, RTLConsts, dxMessages, dxCustomTree, cxMaskEdit, cxTimeEdit;

const
  dxDatePeriodOperators = [foYesterday..foTomorrow, foLastWeek, foLastMonth, foLastYear, foThisWeek..foThisYear,
    foNextWeek, foNextMonth, foNextYear];

  dxGetConditionTypeCaption: array[TdxExcelFilterValueContainerConditionType] of TcxResourceStringID =
    (@sdxExcelFilterSpecificDatePeriodsConditionText, @sdxExcelFilterEqualsConditionText, @sdxExcelFilterDoesNotEqualConditionText,
    @sdxExcelFilterBeginsWithConditionText, @sdxExcelFilterEndsWithConditionText, @sdxExcelFilterContainsConditionText,
    @sdxExcelFilterDoesNotContainConditionText, @sdxExcelFilterIsBlankConditionText, @sdxExcelFilterIsNotBlankConditionText,
    @sdxExcelFilterBetweenConditionText, @sdxExcelFilterGreaterThanConditionText, @sdxExcelFilterGreaterThanOrEqualToConditionText,
    @sdxExcelFilterLessThanConditionText, @sdxExcelFilterLessEqualThanOrEqualToConditionText, @sdxExcelFilterTopNConditionText,
    @sdxExcelFilterBottomNConditionText, @sdxExcelFilterAboveAverageConditionText, @sdxExcelFilterBelowAverageConditionText,
    @sdxExcelFilterBeforeConditionText, @sdxExcelFilterAfterConditionText, @sdxExcelFilterYesterdayConditionText,
    @sdxExcelFilterTodayConditionTypeText, @sdxExcelFilterTomorrowConditionText, @sdxExcelFilterLastWeekConditionText,
    @sdxExcelFilterThisWeekConditionText, @sdxExcelFilterNextWeekConditionText, @sdxExcelFilterLastMonthConditionText,
    @sdxExcelFilterThisMonthConditionText, @sdxExcelFilterNextMonthConditionText, @sdxExcelFilterLastYearConditionText,
    @sdxExcelFilterThisYearConditionText, @sdxExcelFilterNextYearConditionText, @sdxExcelFilterCustomFilterConditionText);

  dxOperatorByConditionTypeMap: array[TdxExcelFilterValueContainerConditionType] of TcxFilterOperatorKind =
    (foEqual, foEqual, foNotEqual, foBeginsWith, foEndsWith, foContains, foNotContains, foEqual, foNotEqual,
    foBetween, foGreater, foGreaterEqual, foLess, foLessEqual, foInList, foInList, foGreater, foLess, foLess,
    foGreater, foYesterday, foToday, foTomorrow, foLastWeek, foThisWeek, foNextWeek, foLastMonth, foThisMonth,
    foNextMonth, foLastYear, foThisYear, foNextYear, foEqual);

  dxConditionTypeByOperatorMap: array[TcxFilterOperatorKind] of TdxExcelFilterValueContainerConditionType =
    (ctEquals, ctDoesNotEqual, ctLessThan, ctLessEqualThanOrEqualTo, ctGreaterThan, ctGreaterThanOrEqualTo, ctEquals,
    ctEquals, ctBetween, ctEquals, ctEquals, ctEquals, ctYesterday, ctToday, ctTomorrow, ctEquals, ctLastWeek, ctEquals,
    ctEquals, ctEquals, ctLastMonth, ctLastYear, ctEquals, ctThisWeek, ctThisMonth, ctThisYear, ctEquals, ctNextWeek, ctEquals,
    ctEquals, ctEquals, ctNextMonth, ctNextYear, ctEquals, ctContains, ctDoesNotContain, ctBeginsWith, ctEndsWith);

  dxFilterValueContainerListBoxDefaultHeight = 200;
  dxFilterValueContainerListBoxDefaultWidth = 100;
  dxFilterValueContainerDefaultHeight = 200;
  dxFilterValueContainerDefaultWidth = 100;
  dxExcelFilterValueContainerDefaultHeight = 232;
  dxExcelFilterValueContainerDefaultWidth = 255;

  dxApplyFilter = 'Apply Filter';

  dxDateTreeViewNodeLevelIndent = 18;
  dxDateTreeViewNodeHeight = 20;
  dxDateTreeViewNodeCheckBoxMargin = 2;

  dxRangeTrackBarValueDigit = -2;
  dxRangeTrackBarHeight = 56;
  dxRangeTrackBarHintDelay = 1;

  dxCounterConditionStartCount = 10;

type
  { TdxCustomControl }

  TdxCustomControl = class helper for TcxCustomControl
  protected
    function GetScaleFactor: TdxScaleFactor;
  end;

  { TcxCustomEditHelper }

  TcxCustomEditHelper = class helper for TcxCustomEdit
  public
    function GetDisplayValue: TcxEditValue;
    function GetProperties: TcxCustomEditProperties;
  end;

  { TcxCustomTrackBarHelper }

  TcxCustomTrackBarHelper = class helper for TcxCustomTrackBar
  public
    function GetViewInfo: TcxCustomTrackBarViewInfo;
  end;

  { TcxCustomLookAndFeelPainterHelper }

  TcxCustomLookAndFeelPainterHelper = class helper for TcxCustomLookAndFeelPainter
  public
    function GetCalendarButtonGlyph: TdxSmartImage;
  end;

function dxGetConditionTypeResourceCaption(AType: TdxExcelFilterValueContainerConditionType): string;
begin
  Result := cxGetResourceString(dxGetConditionTypeCaption[AType]);
end;

function dxGetConditionTypeByOperator(AOperator: TcxFilterOperatorKind; AValue: Variant): TdxExcelFilterValueContainerConditionType;
begin
  Result := dxConditionTypeByOperatorMap[AOperator];
  if VarIsNull(AValue) then
    if Result = ctEquals then
      Result := ctIsBlank
    else
      if Result = ctDoesNotEqual then
        Result := ctIsNotBlank;
end;

{ TdxCustomControl }

function TdxCustomControl.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScaleFactor;
end;

{ TcxCustomEditHelper }

function TcxCustomEditHelper.GetDisplayValue: TcxEditValue;
begin
  Result := DisplayValue;
end;

function TcxCustomEditHelper.GetProperties: TcxCustomEditProperties;
begin
  Result := Properties;
end;

{ TdxRangeTrackBarHelper }

function TcxCustomTrackBarHelper.GetViewInfo: TcxCustomTrackBarViewInfo;
begin
  Result := ViewInfo;
end;

function TcxCustomLookAndFeelPainterHelper.GetCalendarButtonGlyph: TdxSmartImage;
begin
  Result := CalendarButtonGlyph;
end;

{ TdxExcelFilterValueContainerValueInfo }

constructor TdxExcelFilterValueContainerValueInfo.Create(AValue: Variant; ADisplayText: string;
  AOperatorKind: TcxFilterOperatorKind);
begin
  inherited Create;
  FValue := AValue;
  FDisplayText := ADisplayText;
  FOperatorKind := AOperatorKind;
end;

{ TdxFilterPopupWindowIncrementalFilteringHelper }

constructor TdxFilterValueContainerIncrementalFilteringHelper.Create(AOwner: TdxFilterValueContainer);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxFilterValueContainerIncrementalFilteringHelper.GetItemCount: Integer;
begin
  Result := ListBox.Count;
end;

function TdxFilterValueContainerIncrementalFilteringHelper.GetItemIndex: Integer;
begin
  Result := ListBox.ItemIndex;
end;

function TdxFilterValueContainerIncrementalFilteringHelper.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.LookAndFeel.Painter;
end;

function TdxFilterValueContainerIncrementalFilteringHelper.GetVisibleItemCount: Integer;
begin
  Result := ListBox.GetVisibleItemCount;
end;

procedure TdxFilterValueContainerIncrementalFilteringHelper.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited SearchEditKeyDown(Sender, Key, Shift);
  if ItemIndex <> -1 then
    ListBox.KeyDown(Key, Shift);
end;

procedure TdxFilterValueContainerIncrementalFilteringHelper.SearchEditMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := ListBox.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TdxFilterValueContainerIncrementalFilteringHelper.SearchEditValueChanged(Sender: TObject);
begin
  Owner.DataChanged;
end;

procedure TdxFilterValueContainerIncrementalFilteringHelper.SetItemIndex(const Value: Integer);
begin
  ListBox.ItemIndex := Max(Value, -1);
end;

function TdxFilterValueContainerIncrementalFilteringHelper.GetListBox: TdxFilterValueContainerListBox;
begin
  Result := Owner.ListBox;
end;

{ TdxCustomFilterValueContainerListBox }

constructor TdxCustomFilterValueContainerListBox.Create(AFilterValueContainer: TdxCustomFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  FFilterValueContainer := AFilterValueContainer;
end;

procedure TdxCustomFilterValueContainerListBox.ApplyCheckAction(AItemIndex: Integer);
begin
  UpdateCheckedFilterValueIndexes(AItemIndex);
  FilterValuesChanged;
end;

procedure TdxCustomFilterValueContainerListBox.ApplyClickAction(AItemIndex: Integer);
begin
  FilterValueContainer.SetActiveValueIndex(GetFilterValueIndexByItemIndex(AItemIndex));
end;

procedure TdxCustomFilterValueContainerListBox.ApplyFilter;
begin
  FilterValueContainer.SetActiveValueIndexes;
  FilterValueContainer.IsModified := False;
end;

function TdxCustomFilterValueContainerListBox.CanFocusIndex(AItemIndex: Integer): Boolean;
begin
  Result := not IsMRUSeparatorItem(AItemIndex);
end;

procedure TdxCustomFilterValueContainerListBox.DoUpdateItems;
var
  I: Integer;
  ADisplayValue: string;
  AFilterValueItem: TcxFilterValueItem;
begin
  Clear;
  for I := 0 to FilterValueContainer.Values.Count - 1 do
  begin
    AFilterValueItem := FilterValueContainer.Values[I];
    ADisplayValue := FilterValueContainer.DisplayValues[I];
    if IsFilterValueVisible(AFilterValueItem, ADisplayValue) then
      Items.AddObject(ADisplayValue, AFilterValueItem);
  end;
end;

procedure TdxCustomFilterValueContainerListBox.DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
begin
  if IsMRUSeparatorItem(AItem.Index) then
    LookAndFeelPainter.DrawSeparator(Canvas, R, False)
  else
    inherited;
end;

procedure TdxCustomFilterValueContainerListBox.DrawItemText(const ARect: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  ASelStart: Integer;
  AText: string;
  R: TRect;
begin
  if NeedHighlightIncrementalFilteringText then
  begin
    R := ARect;
    AText := AItem.Caption;
    ASelStart := Pos(AnsiUpperCase(IncrementalFilteringText), AnsiUpperCase(AText)) - 1;
    cxTextOut(Canvas.Canvas, AText, R, CXTO_LEFT or CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS,
      ASelStart, Length(IncrementalFilteringText), Canvas.Font, clHighlight, clHighlightText, 0, 0, 0,
        GetTextColor(AItem, AState));
  end
  else
    inherited;
end;

procedure TdxCustomFilterValueContainerListBox.FilterValuesChanged;
begin
  FilterValueContainer.IsModified := True;
  if FilterValueContainer.NeedImmediateApply then
    ApplyFilter;
end;

function TdxCustomFilterValueContainerListBox.GetCheckedItemIndexes: TdxIntegerIndexes;
var
  I: Integer;
  AItemIndex: Integer;
begin
  Result := nil;
  for I := 0 to High(FilterValueContainer.CheckedValueIndexes) do
  begin
    AItemIndex := GetItemIndexByFilterValueIndex(FilterValueContainer.CheckedValueIndexes[I]);
    if AItemIndex <> -1 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := AItemIndex;
    end;
  end;
end;

function TdxCustomFilterValueContainerListBox.GetItemIndexByFilterValueIndex(AIndex: Integer): Integer;
begin
  if HasIncrementalFiltering then
    Result := Items.IndexOfObject(FilterValueContainer.Values[AIndex])
  else
    Result := AIndex;
end;

function TdxCustomFilterValueContainerListBox.GetFilterValue(AItemIndex: Integer): TcxFilterValueItem;
begin
  Result := TcxFilterValueItem(Items[AItemIndex].Data);
end;

function TdxCustomFilterValueContainerListBox.GetFilterValueIndexByItemIndex(AItemIndex: Integer): Integer;
begin
  if HasIncrementalFiltering then
    Result := FilterValueContainer.Values.ItemsList.IndexOf(GetFilterValue(AItemIndex))
  else
    Result := AItemIndex;
end;

function TdxCustomFilterValueContainerListBox.HasCheckBox(AItemIndex: Integer): Boolean;
begin
  Result := inherited HasCheckBox(AItemIndex) and IsCheck(AItemIndex);
end;

function TdxCustomFilterValueContainerListBox.HasIncrementalFiltering: Boolean;
begin
  Result := IncrementalFilteringText <> '';
end;

function TdxCustomFilterValueContainerListBox.IsCheck(AItemIndex: Integer): Boolean;
begin
  Result := IsFilterValueCheck(GetFilterValue(AItemIndex));
end;

function TdxCustomFilterValueContainerListBox.IsFilterValueVisible(AFilterValue: TcxFilterValueItem; ACaption: string): Boolean;
var
  APos: Integer;
begin
  if HasIncrementalFiltering then
  begin
    APos := AnsiPos(AnsiUpperCase(IncrementalFilteringText), AnsiUpperCase(ACaption));
    Result := (APos = 1) or (APos > 1) and UseContainsIncrementalFiltering;
  end
  else
    Result := True;
end;

function TdxCustomFilterValueContainerListBox.IsMRUSeparatorItem(AItemIndex: Integer): Boolean;
begin
  Result := GetFilterValue(AItemIndex).Kind = fviMRUSeparator;
end;

function TdxCustomFilterValueContainerListBox.IsFilterValueCheck(AValue: TcxFilterValueItem): Boolean;
begin
  Result := FilterValueContainer.IsValueCheck(AValue);
end;

procedure TdxCustomFilterValueContainerListBox.DoItemAction(AItemIndex: Integer);
begin
  inherited DoItemAction(AItemIndex);
  if HasCheckBox(AItemIndex) then
    ApplyCheckAction(AItemIndex)
  else
    ApplyClickAction(AItemIndex);
end;

function TdxCustomFilterValueContainerListBox.NeedHighlightIncrementalFilteringText: Boolean;
begin
  Result := HasIncrementalFiltering;
end;

procedure TdxCustomFilterValueContainerListBox.UpdateCheckedFilterValueIndexes(AItemIndex: Integer);
var
  AFilterValueIndex: Integer;
begin
  AFilterValueIndex := GetFilterValueIndexByItemIndex(AItemIndex);
  if Checked[AItemIndex] then
    FilterValueContainer.AddCheckedIndex(AFilterValueIndex)
  else
    FilterValueContainer.DeleteCheckedIndex(AFilterValueIndex);
end;

procedure TdxCustomFilterValueContainerListBox.UpdateItems;
begin
  ShowHourglassCursor;
  try
    BeginUpdate;
    try
      DoUpdateItems;
    finally
      EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TdxCustomFilterValueContainerListBox.UpdateItemStates;
var
  ACheckedIndexes: TdxIntegerIndexes;
begin
  if ShowCheckBoxes then
    ACheckedIndexes := GetCheckedItemIndexes
  else
    ACheckedIndexes := nil;
  CheckedIndexes := ACheckedIndexes;
end;

function TdxCustomFilterValueContainerListBox.UseContainsIncrementalFiltering: Boolean;
begin
  Result := True;
end;

{ TdxFilterPopupWindowListBox }

function TdxFilterValueContainerListBox.CanFocus: Boolean;
begin
  Result := inherited CanFocus and not FilterValueContainer.IncrementalFiltering;
end;

procedure TdxFilterValueContainerListBox.ApplyClickAction(AItemIndex: Integer);
begin
  inherited ApplyClickAction(AItemIndex);
  if not (FilterValueContainer.Values[AItemIndex].Kind in [fviCustom, fviUser]) then
    FilterValueContainer.FilteringApplied;
end;

procedure TdxFilterValueContainerListBox.DoClick;
begin
  if FilterValueContainer.IncrementalFiltering then
    FilterValueContainer.FocusSearchEdit;
  inherited DoClick;
end;

function TdxFilterValueContainerListBox.IsFilterValueVisible(AFilterValue: TcxFilterValueItem; ACaption: string): Boolean;
begin
  Result := inherited IsFilterValueVisible(AFilterValue, ACaption) and
    (not HasIncrementalFiltering or (AFilterValue.Kind = fviValue));
end;

function TdxFilterValueContainerListBox.NeedHighlightIncrementalFilteringText: Boolean;
begin
  Result := inherited NeedHighlightIncrementalFilteringText and
    (ifoHighlightSearchText in FilterValueContainer.IncrementalFilteringOptions);
end;

function TdxFilterValueContainerListBox.NeedHotTrack: Boolean;
begin
  Result := True;
end;

function TdxFilterValueContainerListBox.NeedUseSelectionColor(AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
begin
  Result := inherited NeedUseSelectionColor(AItem, AState) or Checked[AItem.Index];
end;

procedure TdxFilterValueContainerListBox.UpdateItemStates;
begin
  inherited UpdateItemStates;
  if CheckedIndexes = nil then
    ItemIndex := GetItemIndexByFilterValueIndex(FilterValueContainer.SelectedValueIndex)
  else
    ItemIndex := -1;
end;

function TdxFilterValueContainerListBox.UseContainsIncrementalFiltering: Boolean;
begin
  Result := ifoUseContainsOperator in FilterValueContainer.IncrementalFilteringOptions
end;

function TdxFilterValueContainerListBox.GetFilterValueContainer: TdxFilterValueContainer;
begin
  Result := TdxFilterValueContainer(inherited FilterValueContainer);
end;

{ TdxExcelFilterValueContainerListBox }

constructor TdxExcelFilterValueContainerListBox.Create(AFilterValueContainer: TdxCustomFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  UpdateBoundsRect(GetDefaultBounds);
end;

procedure TdxExcelFilterValueContainerListBox.ApplyAllItemAction(AChecked: Boolean);
var
  I: Integer;
begin
  for I := dxAllItemIndex + 1 to Count - 1 do
    if Checked[I] <> AChecked then
    begin
      Checked[I] := AChecked;
      UpdateCheckedFilterValueIndexes(I);
    end;
  FilterValuesChanged;
end;

procedure TdxExcelFilterValueContainerListBox.ApplyCheckAction(AItemIndex: Integer);
begin
  if GetFilterValue(AItemIndex).Kind = fviAll then
    ApplyAllItemAction(Checked[AItemIndex])
  else
  begin
    inherited ApplyCheckAction(AItemIndex);
    UpdateAllItemState;
  end;
end;

procedure TdxExcelFilterValueContainerListBox.DoUpdateItems;
begin
  inherited DoUpdateItems;
  ItemIndex := 0;
end;

function TdxExcelFilterValueContainerListBox.GetAllItem: TdxCustomCheckListBoxItem;
begin
  Result := nil;
  if (Count > 0) and (GetFilterValue(dxAllItemIndex).Kind = fviAll) then
    Result := Items[dxAllItemIndex];
end;

function TdxExcelFilterValueContainerListBox.GetDefaultBounds: TRect;
begin
  Result := cxRectSetSize(cxEmptyRect, dxFilterValueContainerListBoxDefaultWidth, dxFilterValueContainerListBoxDefaultHeight);
end;

function TdxExcelFilterValueContainerListBox.IsFilterValueCheck(AValue: TcxFilterValueItem): Boolean;
begin
  Result := inherited IsFilterValueCheck(AValue) or (AValue.Kind = fviAll);
end;

function TdxExcelFilterValueContainerListBox.NeedHandleClick: Boolean;
begin
  Result := inherited NeedHandleClick and HitAtItemCheckBox(ItemIndex);
end;

procedure TdxExcelFilterValueContainerListBox.UpdateAllItemState;
var
  I, AIndex: Integer;
  AState: TcxCheckBoxState;
  AItem: TdxCustomCheckListBoxItem;
begin
  AItem := GetAllItem;
  if AItem = nil then
    Exit;
  if Count = 1 then
    AState := cbsUnchecked
  else
  begin
    AIndex := dxAllItemIndex + 1;
    AState := States[AIndex];
    Inc(AIndex);
    for I := AIndex to Count - 1 do
      if States[I] <> AState then
      begin
        AState := cbsGrayed;
        Break;
      end;
  end;
  AItem.State := AState;
end;

procedure TdxExcelFilterValueContainerListBox.UpdateCheckedFilterValueIndexes(AItemIndex: Integer);
begin
  if GetFilterValue(AItemIndex).Kind <> fviAll then
    inherited UpdateCheckedFilterValueIndexes(AItemIndex);
end;

procedure TdxExcelFilterValueContainerListBox.UpdateItemStates;
begin
  inherited UpdateItemStates;
  UpdateAllItemState;
end;

{ TdxFilterValueContainerViewInfo }

function TdxFilterValueContainerViewInfo.IsSearchInfoPanelVisible: Boolean;
begin
  Result := FilterValueContainer.IsSearchInfoPanelVisible;
end;

function TdxFilterValueContainerViewInfo.GetFilterValueContainer: TdxFilterValueContainer;
begin
  Result := TdxFilterValueContainer(Owner);
end;

{ TdxCustomFilterValueContainer }

constructor TdxCustomFilterValueContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateBoundsRect(GetDefaultBounds);
end;

destructor TdxCustomFilterValueContainer.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FDisplayValues);
  inherited Destroy;
end;

procedure TdxCustomFilterValueContainer.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomFilterValueContainer.EndUpdate;
begin
  Dec(FLockCount);
  UpdateFiltering;
end;

function TdxCustomFilterValueContainer.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading;
end;

function TdxCustomFilterValueContainer.Focused: Boolean;
begin
  Result := IsChildFocused;
end;

procedure TdxCustomFilterValueContainer.GetTabOrderList(List: TList);
begin
  List.Remove(Self);
  inherited GetTabOrderList(List);
end;

procedure TdxCustomFilterValueContainer.SetFocus;
begin
  if not Focused then
    SetChildFocus;
end;

procedure TdxCustomFilterValueContainer.ApplyFilter;
begin
  FilteringApplied;
  IsModified := False;
end;

procedure TdxCustomFilterValueContainer.FilteringApplied;
begin
  FilterableComponent.FilteringApplied;
end;

procedure TdxCustomFilterValueContainer.AddCheckedIndex(AIndex: Integer);
var
  ALength, AHighIndex: Integer;
begin
  if not IsValueCheck(Values[AIndex]) then
    Exit;
  ALength := Length(CheckedValueIndexes);
  SetLength(FCheckedValueIndexes, ALength + 1);
  AHighIndex := High(CheckedValueIndexes);
  FCheckedValueIndexes[AHighIndex] := AIndex;
end;

function TdxCustomFilterValueContainer.CreateDisplayValues: TStringList;
begin
  Result := TStringList.Create;
end;

function TdxCustomFilterValueContainer.CreateValues: TcxFilterValueList;
begin
  Result := FilterValuesClass.Create(Filter);
end;

procedure TdxCustomFilterValueContainer.DataChanged;
begin
  if not IsLocked then
  begin
    UpdateValues;
    LayoutChanged;
  end;
end;

procedure TdxCustomFilterValueContainer.DeleteCheckedIndex(AIndex: Integer);
var
  I, J: Integer;
begin
  for I := 0 to High(FCheckedValueIndexes) do
  begin
    if CheckedValueIndexes[I] = AIndex then
    begin
      for J := I to High(CheckedValueIndexes) - 1 do
        CheckedValueIndexes[J] := CheckedValueIndexes[J + 1];
      Break;
    end;
  end;
  SetLength(FCheckedValueIndexes, Length(CheckedValueIndexes) - 1);
end;

function TdxCustomFilterValueContainer.DoRefreshContainer(const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  if not AIsMouseEvent then
    UpdateLayout;
end;

procedure TdxCustomFilterValueContainer.DoUpdateFiltering;
begin
  FFilter := nil;
  FFilterValuesClass := nil;
  FreeAndNil(FValues);
  FreeAndNil(FDisplayValues);
  ResetCheckedValueIndexes;
  if FilterableComponent <> nil then
  begin
    FFilter := FilterableComponent.GetFilter;
    FFilterValuesClass := FilterableComponent.GetFilterValuesClass;
    FValues := CreateValues;
    FDisplayValues := CreateDisplayValues;
    PopulateValues;
    PopulateCheckedValueIndexes;
  end;
  DataChanged;
  IsModified := False;
end;

procedure TdxCustomFilterValueContainer.DoUpdateLayout;
begin
//do nothing
end;

procedure TdxCustomFilterValueContainer.ModifiedChanged;
begin
//do nothing
end;

function TdxCustomFilterValueContainer.GetDefaultBounds: TRect;
begin
  Result := cxRectSetSize(cxEmptyRect, dxFilterValueContainerDefaultWidth, dxFilterValueContainerDefaultHeight);
end;

function TdxCustomFilterValueContainer.NeedImmediateApply: Boolean;
begin
  Result := True;
end;

function TdxCustomFilterValueContainer.IsChildFocused: Boolean;
begin
  Result := False;
end;

function TdxCustomFilterValueContainer.IsContainerClass: Boolean;
begin
  Result := True;
end;

function TdxCustomFilterValueContainer.IsContainerFocused: Boolean;
begin
  Result := False;
end;

function TdxCustomFilterValueContainer.IsLinkComponentSupported(AValue: TComponent): Boolean;
begin
  Result := Supports(AValue, IdxFilterableComponent);
end;

function TdxCustomFilterValueContainer.IsValueCheck(AValue: TcxFilterValueItem): Boolean;
begin
  Result := AValue.Kind in [fviBlanks, fviValue, fviSpecial, fviUserEx];
end;

procedure TdxCustomFilterValueContainer.LayoutChanged;
begin
  UpdateLayout;
end;

procedure TdxCustomFilterValueContainer.LinkComponentChanged;
begin
  Supports(LinkComponent, IdxFilterableComponent, FFilterableComponent);
  UpdateFiltering;
end;

procedure TdxCustomFilterValueContainer.Loaded;
begin
  inherited Loaded;
  UpdateFiltering;
end;

procedure TdxCustomFilterValueContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = LinkComponent) then
    LinkComponent := nil;
end;

procedure TdxCustomFilterValueContainer.PopulateCheckedValueIndexes;
var
  I: Integer;
  AActiveValueIndexes: TdxIntegerIndexes;
begin
  FilterableComponent.GetFilterActiveValueIndexes(Values, AActiveValueIndexes);
  for I := 0 to Length(AActiveValueIndexes) - 1 do
    AddCheckedIndex(AActiveValueIndexes[I]);
end;

procedure TdxCustomFilterValueContainer.PopulateValues;
begin
  FilterableComponent.PopulateFilterValues(Values, DisplayValues, False, True);
end;

procedure TdxCustomFilterValueContainer.ResetCheckedValueIndexes;
begin
  FCheckedValueIndexes := nil;
end;

procedure TdxCustomFilterValueContainer.SetActiveValueIndex(AFilterValueIndex: Integer);
begin
  FilterableComponent.SetFilterActiveValueIndex(Values, AFilterValueIndex, nil, True, True);
end;

procedure TdxCustomFilterValueContainer.SetActiveValueIndexes;
begin
  FilterableComponent.SetFilterActiveValueIndexes(Values, CheckedValueIndexes);
end;

procedure TdxCustomFilterValueContainer.SetAllCheckedValueIndexes;
var
  I: Integer;
begin
  for I := 0 to Values.Count - 1 do
    AddCheckedIndex(I);
end;

procedure TdxCustomFilterValueContainer.SetChildFocus;
begin
//do nothing
end;

procedure TdxCustomFilterValueContainer.UpdateFiltering;
begin
  if not IsLocked then
  begin
    ShowHourglassCursor;
    try
      DoUpdateFiltering;
    finally
      HideHourglassCursor;
    end;
  end;
end;

procedure TdxCustomFilterValueContainer.UpdateLayout;
begin
  if not IsLocked then
    DoUpdateLayout;
end;

procedure TdxCustomFilterValueContainer.UpdateValues;
begin
//do nothing
end;

procedure TdxCustomFilterValueContainer.SetIsModified(AValue: Boolean);
begin
  if AValue <> IsModified then
  begin
    FIsModified := AValue;
    ModifiedChanged;
  end;
end;

procedure TdxCustomFilterValueContainer.SetLinkComponent(AValue: TComponent);
begin
  if not IsLinkComponentSupported(AValue) then
    AValue := nil;
  if LinkComponent <> AValue then
  begin
    if LinkComponent <> nil then
      LinkComponent.RemoveFreeNotification(Self);
    FLinkComponent := AValue;
    if LinkComponent <> nil then
      LinkComponent.FreeNotification(Self);
    LinkComponentChanged;
  end;
end;

{ TdxFilterValueContainer }

constructor TdxFilterValueContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := CreateButton;
  FIncHelper := CreateIncHelper;
  FListBox := CreateListBox;

  InitButton;
  InitIncHelper;
  InitListBox;

  FIncrementalFilteringOptions := [ifoHighlightSearchText, ifoUseContainsOperator];
  FShowCheckBoxes := True;
end;

destructor TdxFilterValueContainer.Destroy;
begin
  FreeAndNil(FListBox);
  FreeAndNil(FIncHelper);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TdxFilterValueContainer.ApplyFilter;
begin
  ListBox.ApplyFilter;
  inherited ApplyFilter;
  SetChildFocus;
end;

procedure TdxFilterValueContainer.ApplyFilterOnButtonClick;
begin
  ApplyFilter;
end;

function TdxFilterValueContainer.CreateButton: TcxButton;
begin
  Result := TcxButton.Create(Self);
end;

function TdxFilterValueContainer.CreateIncHelper: TdxFilterValueContainerIncrementalFilteringHelper;
begin
  Result := TdxFilterValueContainerIncrementalFilteringHelper.Create(Self);
end;

function TdxFilterValueContainer.CreateListBox: TdxFilterValueContainerListBox;
begin
  Result := TdxFilterValueContainerListBox.Create(Self);
end;

procedure TdxFilterValueContainer.DoUpdateLayout;
begin
  UpdateSearchEditPosition;
  UpdateButtonPosition;
  UpdateListBoxPosition;
  UpdateSearchInfoPanelPosition;
end;

procedure TdxFilterValueContainer.ModifiedChanged;
begin
  inherited ModifiedChanged;
  UpdateButtonEnabled;
end;

procedure TdxFilterValueContainer.FocusSearchEdit;
begin
  IncHelper.FocusSearchControl;
end;

function TdxFilterValueContainer.GetDefaultButtonCaption: string;
begin
  Result := dxApplyFilter;
end;

function TdxFilterValueContainer.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxFilterValueContainerViewInfo;
end;

function TdxFilterValueContainer.NeedImmediateApply: Boolean;
begin
  Result := (ApplyMode = fpacImmediately) or not ShowCheckBoxes;
end;

function TdxFilterValueContainer.HasIncrementalFiltering: Boolean;
begin
  Result := SearchText <> '';
end;

procedure TdxFilterValueContainer.InitButton;
begin
  Button.Caption := DefaultButtonCaption;
  Button.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  Button.ParentFont := True;
  Button.Enabled := False;
  Button.OnClick := ButtonClickHandler;
  Button.Parent := Self;
end;

procedure TdxFilterValueContainer.InitIncHelper;
begin
  IncHelper.CheckSearchControl(Self);
  IncHelper.SearchEdit.Visible := False;
  IncHelper.SearchEdit.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxFilterValueContainer.InitListBox;
begin
  ListBox.BorderStyle := cxcbsNone;
  ListBox.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  ListBox.Parent := Self;
end;

function TdxFilterValueContainer.IsButtonCaptionStored: Boolean;
begin
  Result := ButtonCaption <> DefaultButtonCaption;
end;

function TdxFilterValueContainer.IsButtonVisible: Boolean;
begin
  Result := not NeedImmediateApply;
end;

function TdxFilterValueContainer.IsChildFocused: Boolean;
begin
  Result := inherited IsChildFocused or ListBox.Focused or Button.Focused or SearchEdit.Focused;
end;

function TdxFilterValueContainer.IsSearchInfoPanelVisible: Boolean;
begin
  Result := HasIncrementalFiltering and (ListBox.Count = 0);
end;

procedure TdxFilterValueContainer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_ESCAPE) and HasIncrementalFiltering then
  begin
    SearchEdit.Clear;
    Key := 0;
  end;
end;

procedure TdxFilterValueContainer.UpdateButtonPosition;
var
  ARect: TRect;
begin
  Button.Visible := IsButtonVisible;
  if Button.Visible then
  begin
    ARect := ClientBounds;
    ARect.Top := ARect.Bottom - Button.GetOptimalSize.cy;
    Button.BoundsRect := ARect;
  end;
end;

procedure TdxFilterValueContainer.UpdateValues;
begin
  if FilterableComponent <> nil then
  begin
    ListBox.IncrementalFilteringText := SearchText;
    UpdateListBoxItems;
    UpdateListBoxIndexes;
  end;
end;

procedure TdxFilterValueContainer.RefreshChildFocus;
begin
  if Focused and not Button.Focused then
    SetChildFocus;
end;

procedure TdxFilterValueContainer.ResetSearchText;
begin
  IncHelper.ResetSearchText;
end;

procedure TdxFilterValueContainer.SetChildFocus;
begin
  inherited SetChildFocus;
  if SearchEdit.CanFocus then
    FocusSearchEdit
  else
    if ListBox.CanFocus then
      ListBox.SetFocus
    else
      if Button.CanFocus then
        Button.SetFocus;
end;

procedure TdxFilterValueContainer.UpdateButtonEnabled;
begin
  Button.Enabled := IsModified;
end;

procedure TdxFilterValueContainer.UpdateListBoxIndexes;
begin
  ListBox.UpdateItemStates;
end;

procedure TdxFilterValueContainer.UpdateListBoxItems;
begin
  ListBox.UpdateItems;
end;

procedure TdxFilterValueContainer.UpdateListBoxPosition;
var
  ARect: TRect;
begin
  ListBox.Visible := not IsSearchInfoPanelVisible;
  ARect := ClientBounds;
  if HasIncrementalFiltering then
    ARect.Top := SearchEdit.BoundsRect.Bottom + SearchEditOffsets.Bottom;
  if IsButtonVisible then
    ARect.Bottom := Button.Top;
  ListBox.BoundsRect := ARect;
end;

procedure TdxFilterValueContainer.UpdateSearchEditPosition;
var
  ARect: TRect;
begin
  if not SearchEdit.Visible then
    Exit;
  ARect := cxRectContent(ClientBounds, SearchEditOffsets);
  ARect.Bottom := ARect.Top + SearchEdit.Height;
  if not HasIncrementalFiltering then
    ARect := cxRectOffsetVert(ARect, -ARect.Bottom);
  SearchEdit.BoundsRect := ARect;
end;

procedure TdxFilterValueContainer.UpdateSearchInfoPanelPosition;
begin
  ViewInfo.UseRightToLeftAlignment := UseRightToLeftAlignment;
  if IsSearchInfoPanelVisible then
    ViewInfo.SearchInfoPanelBounds := ListBox.BoundsRect
  else
    ViewInfo.SearchInfoPanelBounds := cxNullRect;
end;

function TdxFilterValueContainer.GetButtonCaption: string;
begin
  Result := Button.Caption;
end;

function TdxFilterValueContainer.GetSearchEdit: TcxCustomTextEdit;
begin
  Result := IncHelper.SearchEdit;
end;

function TdxFilterValueContainer.GetSearchEditOffsets: TRect;
begin
  Result := IncHelper.GetSearchEditOffsets;
end;

function TdxFilterValueContainer.GetSearchText: string;
begin
  if IncrementalFiltering then
    Result := IncHelper.GetSearchText
  else
    Result := '';
end;

function TdxFilterValueContainer.GetSelectedValueIndex: Integer;
begin
  Result := FilterableComponent.GetFilterSelectedValueIndex(Values);
end;

function TdxFilterValueContainer.GetViewInfo: TdxFilterValueContainerViewInfo;
begin
  Result := TdxFilterValueContainerViewInfo(inherited ViewInfo);
end;

procedure TdxFilterValueContainer.SetApplyMode(const AValue: TdxFilterApplyChangesMode);
begin
  if ApplyMode <> AValue then
  begin
    FApplyMode := AValue;
    LayoutChanged;
  end;
end;

procedure TdxFilterValueContainer.SetButtonCaption(const AValue: string);
begin
  Button.Caption := AValue;
end;

procedure TdxFilterValueContainer.SetIncrementalFiltering(const AValue: Boolean);
begin
  if IncrementalFiltering <> AValue then
  begin
    FIncrementalFiltering := AValue;
    SearchEdit.Visible := IncrementalFiltering;
    RefreshChildFocus;
    LayoutChanged;
  end;
end;

procedure TdxFilterValueContainer.SetIncrementalFilteringOptions(const AValue: TcxTextEditIncrementalFilteringOptions);
begin
  if IncrementalFilteringOptions <> AValue then
  begin
    FIncrementalFilteringOptions := AValue;
    if HasIncrementalFiltering then
      DataChanged;
  end;
end;

procedure TdxFilterValueContainer.SetShowCheckBoxes(const AValue: Boolean);
begin
  if ShowCheckBoxes <> AValue then
  begin
    FShowCheckBoxes := AValue;
    ListBox.ShowCheckBoxes := ShowCheckBoxes;
    DataChanged;
  end;
end;

procedure TdxFilterValueContainer.ButtonClickHandler(Sender: TObject);
begin
  ApplyFilterOnButtonClick;
end;

{ TdxExcelFilterValueContainerCustomDateTimeTreeView }

constructor TdxExcelFilterValueContainerCustomDateTimeTreeView.Create(AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(nil);
  FFilterValueContainer := AFilterValueContainer;
  OptionsView.RowSelect := True;
  OptionsView.ShowCheckBoxes := True;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.ApplyFilter;
begin
  FilterValueContainer.SetActiveValueIndexes;
  FilterValueContainer.IsModified := False;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.FilterValuesChanged;
begin
  FilterValueContainer.IsModified := True;
  if FilterValueContainer.NeedImmediateApply then
    ApplyFilter;
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.GetFilterValueIndexByNode(ANode: TdxTreeViewNode): Integer;
begin
  Result := FilterValueContainer.Values.ItemsList.IndexOf(TcxFilterValueItem(ANode.Data))
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string;
begin
  Result := VarToStr(AFilterValue.Value);
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.GetNodeByFilterValue(AFilterValue: TcxFilterValueItem): TdxTreeViewNode;
var
  I: Integer;
begin
  for I := 0 to AbsoluteVisibleNodes.Count - 1 do
  begin
    Result := AbsoluteVisibleNodes[I];
    if Result.Data = AFilterValue then
      Exit;
  end;
  Result := nil;
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.HasIncrementalFiltering: Boolean;
begin
  Result := IncrementalFilteringText <> '';
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.IsAllNode(ANode: TdxTreeViewNode): Boolean;
begin
  Result := (ANode.Data <> nil) and (TcxFilterValueItem(ANode.Data).Kind = fviAll);
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.IsTextVisible(AText: string): Boolean;
begin
  Result := AnsiPos(AnsiUpperCase(IncrementalFilteringText), AnsiUpperCase(AText)) > 0;
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeView.IsValueVisible(AFilterValue: TcxFilterValueItem; AText: string): Boolean;
var
  AText0, AText1, AText2: string;
begin
  Result := not HasIncrementalFiltering;
  if Result then
    Exit;
  if AFilterValue.Kind = fviValue then
  begin
    AText0 := GetValueTextByLvl(AFilterValue, 0);
    AText1 := GetValueTextByLvl(AFilterValue, 1);
    AText2 := GetValueTextByLvl(AFilterValue, 2);
    Result := IsTextVisible(AText0) or IsTextVisible(AText1) or IsTextVisible(AText2);
  end
  else
    Result := IsTextVisible(AText);
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.NodeStateChanged(ANode: TdxTreeViewNode);

  function GetItemStateChangedTypeByNode(ANode: TdxTreeViewNode): TItemStateChangedType;
  begin
    case ANode.Level of
      0:
        if IsAllNode(ANode) then
          Result := sctAllNodeStateChanged
        else
          Result := sctLvl0ValueStateChanged;
      1:
        Result := sctLvl1ValueStateChanged;
      2:
        Result := sctLvl2ValueStateChanged;
      else
        Result := sctNone;
    end;
  end;

var
  I: Integer;
  AParentNode: TdxTreeViewNode;
  APrevType: TItemStateChangedType;
begin
  inherited NodeStateChanged(ANode);
  if FItemStateChangedType = sctUpdateStates then
    Exit;
  APrevType := FItemStateChangedType;
  FItemStateChangedType := GetItemStateChangedTypeByNode(ANode);
  try
    if (ANode.Data <> nil) and (FItemStateChangedType <> sctAllNodeStateChanged) then
      if ANode.Checked then
        FilterValueContainer.AddCheckedIndex(GetFilterValueIndexByNode(ANode))
      else
        FilterValueContainer.DeleteCheckedIndex(GetFilterValueIndexByNode(ANode))
    else
      if (APrevType = sctNone) or (FItemStateChangedType = sctLvl1ValueStateChanged) and (APrevType = sctLvl0ValueStateChanged) or
        (FItemStateChangedType = sctLvl0ValueStateChanged) and (APrevType = sctAllNodeStateChanged) then
      begin
        if FItemStateChangedType = sctAllNodeStateChanged then
          AParentNode := Root
        else
          AParentNode := ANode;
        for I := 0 to AParentNode.Count - 1 do
          if not IsAllNode(AParentNode.Items[I]) then
            AParentNode.Items[I].Checked := ANode.Checked;
      end;
    if APrevType = sctNone then
    begin
      FilterValuesChanged;
      UpdateParentItemStates;
    end;
  finally
    FItemStateChangedType := APrevType;
  end;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.PopulateItems;
var
  I: Integer;
  AHasNewNode0, AHasNewNode1: Boolean;
  ANode0, ANode1: TdxTreeViewNode;
  AFilterValue: TcxFilterValueItem;
  AText0, AText1, APrevText0, APrevText1: string;
begin
  ANode0 := nil;
  ANode1 := nil;
  APrevText0 := '';
  APrevText1 := '';
  for I := 0 to FilterValueContainer.Values.Count - 1 do
  begin
    AFilterValue := FilterValueContainer.Values[I];
    if not IsValueVisible(AFilterValue, FilterValueContainer.DisplayValues[I]) then
      Continue;
    if AFilterValue.Kind = fviValue then
    begin
      AText0 := GetValueTextByLvl(AFilterValue, 0);
      AHasNewNode0 := (ANode0 = nil) or (AText0 <> APrevText0);
      if AHasNewNode0 then
        ANode0 := Root.AddChild(AText0);
      AText1 := GetValueTextByLvl(AFilterValue, 1);
      AHasNewNode1 := AHasNewNode0 or (ANode1 = nil) or (AText1 <> APrevText1);
      if AHasNewNode1 then
        ANode1 := ANode0.AddChild(AText1);
      ANode1.AddChild(GetValueTextByLvl(AFilterValue, 2), AFilterValue);
      APrevText0 := AText0;
      APrevText1 := AText1;
    end
    else
      Root.AddChild(FilterValueContainer.DisplayValues[I], AFilterValue);
  end;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.UpdateDayItemStates;
var
  I, AIndex: Integer;
  ANode: TdxTreeViewNode;
begin
  for I := 0 to High(FilterValueContainer.CheckedValueIndexes) do
  begin
    AIndex := FilterValueContainer.CheckedValueIndexes[I];
    ANode := GetNodeByFilterValue(FilterValueContainer.Values[AIndex]);
    if ANode <> nil then
      ANode.Checked := True;
  end;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.UpdateItems;
begin
  Root.Clear;
  PopulateItems;
  if AbsoluteVisibleNodes.Count > 0 then
    FocusedNode := Root.First;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.UpdateItemStates;
var
  APrevType: TItemStateChangedType;
begin
  APrevType := FItemStateChangedType;
  FItemStateChangedType := sctUpdateStates;
  try
    UpdateDayItemStates;
    UpdateParentItemStates;
  finally
    FItemStateChangedType := APrevType;
  end;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.UpdateParentItemStates;

  procedure UpdateParentStateByChildNodeState(AChildNodeState: TcxCheckBoxState;
    var AParentState: TcxCheckBoxState; var AIsFirstChildNode: Boolean);
  begin
    if AIsFirstChildNode then
    begin
      AParentState := AChildNodeState;
      AIsFirstChildNode := False;
    end
    else
      if (AChildNodeState = cbsGrayed) or (AChildNodeState = cbsChecked) and (AParentState = cbsUnchecked) or
        (AChildNodeState = cbsUnchecked) and (AParentState = cbsChecked) then
        AParentState := cbsGrayed;
  end;

var
  I: Integer;
  ANode: TdxTreeViewNode;
  AIsFirstItemForFirstLvl, AIsFirstItemForSecondLvl, AIsFirstItemForThirdlvl: Boolean;
  AFirstLvlNodeState, ASecondLvlNodeState, AAllNodeState: TcxCheckBoxState;
begin
  AIsFirstItemForThirdlvl := True;
  AIsFirstItemForSecondLvl := True;
  AIsFirstItemForFirstLvl := True;
  ASecondLvlNodeState := cbsUnchecked;
  AFirstLvlNodeState := cbsUnchecked;
  AAllNodeState := cbsUnchecked;
  for I := AbsoluteVisibleNodes.Count - 1 downto 0 do
  begin
    ANode := AbsoluteVisibleNodes[I];
    case ANode.Level of
      0:
        if ANode.Data = nil then
        begin
          ANode.State := AFirstLvlNodeState;
          AFirstLvlNodeState := cbsUnchecked;
          UpdateParentStateByChildNodeState(ANode.State, AAllNodeState, AIsFirstItemForFirstLvl);
          AIsFirstItemForSecondLvl := True;
        end
        else
          if IsAllNode(ANode) then
            ANode.State := AAllNodeState
          else
            UpdateParentStateByChildNodeState(ANode.State, AAllNodeState, AIsFirstItemForFirstLvl);
      1:
        begin
          ANode.State := ASecondLvlNodeState;
          ASecondLvlNodeState := cbsUnchecked;
          UpdateParentStateByChildNodeState(ANode.State, AFirstLvlNodeState, AIsFirstItemForSecondLvl);
          AIsFirstItemForThirdlvl := True;
        end;
      2:
        UpdateParentStateByChildNodeState(ANode.State, ASecondLvlNodeState, AIsFirstItemForThirdlvl);
    end;
  end;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeView.SetIncrementalFilteringText(const AValue: string);
begin
  if AValue <> IncrementalFilteringText then
  begin
    FIncrementalFilteringText := AValue;
    HighlightedText := IncrementalFilteringText;
  end;
end;

{ TdxExcelFilterValueContainerTimeTreeView }

constructor TdxExcelFilterValueContainerTimeTreeView.Create(AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  FHourFormat := GetHourFormat;
end;

function TdxExcelFilterValueContainerTimeTreeView.GetHourFormat: string;
var
  ATimeEditProperties: TcxCustomTimeEditProperties;
begin
  ATimeEditProperties := Safe<TcxCustomTimeEditProperties>.Cast(FilterValueContainer.FilterProperties);
  if (ATimeEditProperties <> nil) and ATimeEditProperties.Use24HourFormat then
    Result := 'hh'
  else
    Result := 'hh ampm';
end;

function TdxExcelFilterValueContainerTimeTreeView.GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string;
begin
  case ALvl of
    0:
      Result := FormatDateTime(HourFormat, AFilterValue.Value);
    1:
      Result := FormatDateTime(':nn', AFilterValue.Value);
    2:
      Result := FormatDateTime(':ss', AFilterValue.Value);
    else
      Result := inherited GetValueTextByLvl(AFilterValue, ALvl);
  end;
end;

{ TdxExcelFilterValueContainerDateTreeView }

function TdxExcelFilterValueContainerDateTreeView.GetValueTextByLvl(AFilterValue: TcxFilterValueItem; ALvl: Integer): string;
begin
  case ALvl of
    0:
      Result := FormatDateTime('yyyy', AFilterValue.Value);
    1:
      Result := FormatDateTime('mmmm', AFilterValue.Value);
    2:
      Result := FormatDateTime('d', AFilterValue.Value);
    else
      Result := inherited GetValueTextByLvl(AFilterValue, ALvl);
  end;
end;

{ TdxExcelFilterValueContainerComboBox }

constructor TdxExcelFilterValueContainerComboBox.Create(AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  FFilterValueContainer := AFilterValueContainer;
  inherited Create(nil);
  Properties.DropDownSizeable := True;
  Style.TransparentBorder := False;
end;

procedure TdxExcelFilterValueContainerComboBox.PopulateItems;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerComboBox.UpdateItems;
begin
  LockChangeEvents(True);
  try
    Properties.Items.Clear;
    PopulateItems;
  finally
    LockChangeEvents(False, False);
  end;
end;

{ TdxExcelFilterValueContainerValuesComboBoxProperties }

function TdxExcelFilterValueContainerValuesComboBoxProperties.IsEditValueNumeric: Boolean;
begin
  Result := FilterValueContainer.GetDataType in [fdtNumeric, fdtDate];
end;

{ TdxExcelFilterValueContainerValuesComboBox }

constructor TdxExcelFilterValueContainerValuesComboBox.Create(
  AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  InitProperties;
  if FilterValueContainer.GetDataType = fdtDate then
  begin
    AddCalendarButton;
    AddCalendar;
  end;
end;

destructor TdxExcelFilterValueContainerValuesComboBox.Destroy;
begin
  FreeAndNil(FCalendar);
  FreeAndNil(FCalendarPopup);
  inherited Destroy;
end;

class function TdxExcelFilterValueContainerValuesComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxExcelFilterValueContainerValuesComboBoxProperties;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.AddCalendar;
begin
  FCalendar := CreateCalendar;
  FCalendarPopup := CreateCalendarPopup;
  Calendar.ShowToday := True;
  Calendar.Parent := CalendarPopup;
  Calendar.OnDateTimeChanged := DateTimeChangedHandler;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.AddCalendarButton;
begin
  FCalendarButton := Properties.Buttons.Add;
  CalendarButton.Glyph.SourceHeight := 16;
  CalendarButton.Glyph.SourceWidth := 16;
  CalendarButton.Glyph.Assign(LookAndFeelPainter.GetCalendarButtonGlyph);
  CalendarButton.Kind := bkGlyph;
  Properties.OnButtonClick := ButtonClickHandler;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.CalendarButtonClick;
begin
  CalendarDropDown;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.CalendarDropDown;
begin
  if ItemIndex <> -1 then
    if ItemObject <> nil then
      Calendar.SelectDate := TcxFilterValueItem(ItemObject).Value
    else
      Calendar.SelectDate := StrToDate(DisplayValue);
  Calendar.Color := ActiveStyle.Color;
  Calendar.LookAndFeel.MasterLookAndFeel := ActiveStyle.LookAndFeel;
  Calendar.CalendarButtons := [btnClear];
  Calendar.Calculate;
  CalendarPopup.LookAndFeel.MasterLookAndFeel := ActiveStyle.LookAndFeel;
  CalendarPopup.BorderStyle := ActiveStyle.LookAndFeel.Painter.PopupBorderStyle;
  CalendarPopup.OwnerParent := Self;
  CalendarPopup.OwnerBounds := ViewInfo.ButtonsInfo[CalendarButton.Index].Bounds;
  PostMessage(CalendarPopup.Handle, DXM_SHOWPOPUPWINDOW, 0, 0);
end;

function TdxExcelFilterValueContainerValuesComboBox.CreateCalendar: TdxExcelFilterValueContainerValuesComboBoxCalendar;
begin
  Result := TdxExcelFilterValueContainerValuesComboBoxCalendar.Create(nil);
end;

function TdxExcelFilterValueContainerValuesComboBox.CreateCalendarPopup: TdxUIElementPopupWindow;
begin
  Result := TdxUIElementPopupWindow.Create(Self);
end;

function TdxExcelFilterValueContainerValuesComboBox.GetFilterDisplayText: string;
begin
  Result := FilterValueContainer.GetDisplayTextByValue(EditingValue);
end;

function TdxExcelFilterValueContainerValuesComboBox.GetFilterValue: Variant;
begin
  if ItemObject <> nil then
    Result := TcxFilterValueItem(ItemObject).Value
  else
    if Properties.IsMasked then
      Result := DisplayValue
    else
      Result := EditingValue;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.Initialize;
begin
  inherited Initialize;
  Properties.FilterValueContainer := FilterValueContainer;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.InitProperties;
begin
  InitPropertiesFormat;
  InitPropertiesMask;
  Properties.IDefaultValuesProvider := FilterValueContainer.FilterProperties.IDefaultValuesProvider;
  Properties.Alignment.Horz := taLeftJustify;
  Properties.Alignment.Vert := taVCenter;
  Properties.ValidationOptions := [evoAllowLoseFocus];
  Properties.OnValidate := ValidateHandler;
  if FilterValueContainer.GetDataType = fdtDate then
    TextHint := cxGetResourceString(@sdxExcelFilterSelectDateHintText)
  else
    TextHint := cxGetResourceString(@sdxExcelFilterSelectValueHintText);
end;

procedure TdxExcelFilterValueContainerValuesComboBox.InitPropertiesFormat;
var
  ATextEditProperties: TcxCustomTextEditProperties;
begin
  if FilterValueContainer.FilterProperties is TcxCustomTextEditProperties then
  begin
    ATextEditProperties := TcxCustomTextEditProperties(FilterValueContainer.FilterProperties);
    Properties.DisplayFormat := ATextEditProperties.DisplayFormat;
    Properties.EditFormat := ATextEditProperties.EditFormat;
  end;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.InitPropertiesMask;
var
  AMaskEditProperties: TcxCustomMaskEditProperties;
begin
  if FilterValueContainer.FilterProperties is TcxCustomMaskEditProperties then
  begin
    AMaskEditProperties := TcxCustomMaskEditProperties(FilterValueContainer.FilterProperties);
    Properties.AlwaysShowBlanksAndLiterals := AMaskEditProperties.AlwaysShowBlanksAndLiterals;
    Properties.IgnoreMaskBlank := AMaskEditProperties.IgnoreMaskBlank;
    Properties.MaskKind := AMaskEditProperties.MaskKind;
    Properties.EditMask := AMaskEditProperties.EditMask;
  end;
end;

function TdxExcelFilterValueContainerValuesComboBox.IsEmpty: Boolean;
var
  AError: Boolean;
  AText: TCaption;
  AValue: Variant;
begin
  if Properties.IsMasked then
    AValue := DisplayValue
  else
    AValue := EditingValue;
  AText := '';
  AError := False;
  DoValidateDisplayValue(AValue, AText, AError);
  Result := AError or VarIsSoftNull(EditingValue) and (ItemIndex = -1);;
end;

function TdxExcelFilterValueContainerValuesComboBox.IsValueValid(AValue: Variant): Boolean;
var
  ADouble: Double;
  ADate: TDateTime;
  ADataType: TcxFilterDataType;
begin
  ADataType := FilterValueContainer.GetDataType;
  Result := (ADataType = fdtText) or
    (ADataType = fdtNumeric) and (VarIsNumeric(AValue) or VarIsStr(AValue) and TryStrToFloat(AValue, ADouble)) or
    (ADataType = fdtDate) and (VarIsDate(AValue) or VarIsStr(AValue) and TryStrToDateTime(AValue, ADate)) or
    (ADataType = fdtTime) and (VarIsDate(AValue) or VarIsStr(AValue) and TryStrToTime(AValue, ADate));
end;

procedure TdxExcelFilterValueContainerValuesComboBox.PopulateItems;

  function GetFirstValueIndex: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to FilterValueContainer.Values.Count - 1 do
      if FilterValueContainer.Values[I].Kind = fviValue then
        Exit(I);
  end;

var
  I, AIndex: Integer;
begin
  AIndex := GetFirstValueIndex;
  if AIndex = -1 then
    Exit;
  for I := AIndex to FilterValueContainer.DisplayValues.Count - 1 do
    Properties.Items.AddObject(FilterValueContainer.Values[I].Value, FilterValueContainer.Values[I]);
end;

procedure TdxExcelFilterValueContainerValuesComboBox.ButtonClickHandler(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = CalendarButton.Index then
    CalendarButtonClick;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.DateTimeChangedHandler(Sender: TObject);
begin
  CalendarPopup.CloseUp;
  if VarIsNullDate(Calendar.SelectDate) then
    EditingText := ''
  else
    EditingText := DateToStr(Calendar.SelectDate);
  SynchronizeItemIndex;
end;

function TdxExcelFilterValueContainerValuesComboBox.GetProperties: TdxExcelFilterValueContainerValuesComboBoxProperties;
begin
  Result := TdxExcelFilterValueContainerValuesComboBoxProperties(inherited Properties);
end;

procedure TdxExcelFilterValueContainerValuesComboBox.SetProperties(Value: TdxExcelFilterValueContainerValuesComboBoxProperties);
begin
  inherited Properties := Value;
end;

procedure TdxExcelFilterValueContainerValuesComboBox.ValidateHandler(ASender: TObject;
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);
var
  AValue: Variant;
  AEdit: TcxCustomEdit absolute ASender;
begin
  if Properties.IsMasked then
    AValue := ADisplayValue
  else
    if ItemIndex <> -1 then
      AValue := TcxFilterValueItem(ItemObject).Value
    else
      AEdit.PrepareEditValue(ADisplayValue, AValue, AEdit.Focused);
  AError := AError or not IsValueValid(AValue);
  if AError then
    ADisplayValue := '';
end;

{ TdxExcelFilterValueContainerConditionTypesComboBox }

constructor TdxExcelFilterValueContainerConditionTypesComboBox.Create(
  AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  Properties.DropDownListStyle := lsFixedList;
  Properties.DropDownRows := MaxInt;
end;

procedure TdxExcelFilterValueContainerConditionTypesComboBox.PopulateItems;
var
  AType: TdxExcelFilterValueContainerConditionType;
begin
  for AType := Low(TdxExcelFilterValueContainerConditionType) to High(TdxExcelFilterValueContainerConditionType) do
    if AType in SupportedConditionTypes then
      Properties.Items.AddObject(dxGetConditionTypeResourceCaption(AType), TObject(AType));
end;

{ TdxExcelFilterValueContainerCustomItem }

constructor TdxExcelFilterValueContainerCustomItem.Create(AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create;
  FFilterValueContainer := AFilterValueContainer;
end;

procedure TdxExcelFilterValueContainerCustomItem.Generate(AParentGroup: TdxLayoutGroup);
begin
  FilterValueContainer.LayoutContainer.BeginUpdate;
  try
    DoGenerate(AParentGroup);
  finally
    FilterValueContainer.LayoutContainer.EndUpdate;
  end;
end;

procedure TdxExcelFilterValueContainerCustomItem.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  FParentGroup := AParentGroup;
end;

procedure TdxExcelFilterValueContainerCustomItem.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FilterValueContainer.LayoutKeyDown(Sender, Key, Shift);
end;

procedure TdxExcelFilterValueContainerCustomItem.UpdateData;
begin
//do nothing
end;

function TdxExcelFilterValueContainerCustomItem.GetFilterableComponent: IdxExcelFilterableComponent;
begin
  Result := FilterValueContainer.FilterableComponent;
end;

{ TdxExcelFilterValueContainerCondition }

function TdxExcelFilterValueContainerCondition.GetBoolOperator: TcxFilterBoolOperatorKind;
begin
  Result := fboAnd;
end;

function TdxExcelFilterValueContainerCondition.GetDisplayText: string;
begin
  Result := cxGetResourceString(@cxSFilterBlankCaption)
end;

function TdxExcelFilterValueContainerCondition.GetOperator: TcxFilterOperatorKind;
begin
  Result := dxOperatorByConditionTypeMap[ConditionContainer.GetConditionType];
end;

function TdxExcelFilterValueContainerCondition.GetValue: Variant;
begin
  Result := Null;
end;

function TdxExcelFilterValueContainerCondition.IsAppliedOnChoosing: Boolean;
begin
  Result := not IsEditable;
end;

function TdxExcelFilterValueContainerCondition.IsComplex: Boolean;
begin
  Result := False;
end;

function TdxExcelFilterValueContainerCondition.IsEditable: Boolean;
begin
  Result := False;
end;

function TdxExcelFilterValueContainerCondition.IsEmpty: Boolean;
begin
  Result := False;
end;

function TdxExcelFilterValueContainerCondition.IsPresentInFilter: Boolean;
begin
  Result := ConditionContainer.IsConditionPresentInFilter;
end;

procedure TdxExcelFilterValueContainerCondition.PopulateValueInfos(AList: TdxFastObjectList);
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerCondition.SetCaption(AValue: string);
begin
//do nothing
end;

constructor TdxExcelFilterValueContainerCondition.Create(AFilterValueContainer: TdxExcelFilterValueContainer;
  AConditionContainer: TdxExcelFilterValueContainerConditionContainer);
begin
  inherited Create(AFilterValueContainer);
  FConditionContainer := AConditionContainer;
end;

{ TdxExcelFilterValueContainerAverageCondition }

procedure TdxExcelFilterValueContainerAverageCondition.CalculateAverageValue;
var
  I, ACount: Integer;
  ASum, AValue: Variant;
  ANonUniqueValues: TcxFilterValueList;
begin
  FAverageValue := 0;
  ANonUniqueValues := FilterValueContainer.NonUniqueValues;
  ACount := ANonUniqueValues.Count;
  if ACount = 0 then
    Exit;
  ASum := 0;
  for I := 0 to ACount - 1 do
  begin
    AValue := ANonUniqueValues[I].Value;
    if not VarIsNull(AValue) then
      ASum := AValue + ASum;
  end;
  FAverageValue := ASum / ACount;
end;

function TdxExcelFilterValueContainerAverageCondition.GetDisplayText: string;
begin
  Result := FilterValueContainer.GetDisplayTextByValue(FAverageValue);
end;

function TdxExcelFilterValueContainerAverageCondition.GetValue: Variant;
begin
  Result := FAverageValue;
end;

function TdxExcelFilterValueContainerAverageCondition.IsEmpty: Boolean;
begin
  Result := FilterValueContainer.NonUniqueValues.Count = 0;
end;

procedure TdxExcelFilterValueContainerAverageCondition.UpdateData;
begin
  inherited UpdateData;
  CalculateAverageValue;
end;

{ TdxExcelFilterValueContainerEditableCondition }

procedure TdxExcelFilterValueContainerEditableCondition.EditChangeHandler(ASender: TObject);
var
  AEdit: TcxCustomEdit absolute ASender;
begin
  if AEdit.EditModified then
    ValueChanged;
end;

function TdxExcelFilterValueContainerEditableCondition.IsEditable: Boolean;
begin
  Result := True;
end;

procedure TdxExcelFilterValueContainerEditableCondition.UpdateData;
begin
  inherited UpdateData;
  if IsPresentInFilter then
    UpdateValues;
end;

procedure TdxExcelFilterValueContainerEditableCondition.UpdateValues;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerEditableCondition.ValueChanged;
begin
  ConditionContainer.ConditionChanged;
end;

{ TdxExcelFilterValueContainerTextEditCondition }

destructor TdxExcelFilterValueContainerTextEditCondition.Destroy;
begin
  FreeAndNil(FTextEdit);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerTextEditCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateTextEdit;
end;

procedure TdxExcelFilterValueContainerTextEditCondition.GenerateTextEdit;
begin
  FTextEdit := TcxTextEdit.Create(nil);
  TextEdit.Style.TransparentBorder := False;
  TextEdit.TextHint := cxGetResourceString(@sdxExcelFilterEnterValueHintText);
  TextEdit.OnKeyDown := KeyDownHandler;
  TextEdit.Properties.OnChange := EditChangeHandler;
  FTextEditLayoutItem := ParentGroup.CreateItemForControl(TextEdit);
  TextEditLayoutItem.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerTextEditCondition.GetDisplayText: string;
begin
  Result := GetValue;
end;

function TdxExcelFilterValueContainerTextEditCondition.GetValue: Variant;
begin
  Result := TextEdit.Text;
end;

function TdxExcelFilterValueContainerTextEditCondition.IsEmpty: Boolean;
begin
  Result := GetValue = '';
end;

procedure TdxExcelFilterValueContainerTextEditCondition.SetCaption(AValue: string);
begin
  TextEditLayoutItem.CaptionOptions.Text := AValue;
end;

procedure TdxExcelFilterValueContainerTextEditCondition.UpdateValues;
begin
  inherited UpdateValues;
  TextEdit.LockChangeEvents(True);
  try
    TextEdit.EditValue := ConditionContainer.ActiveCriteriaItems[0].Value;
  finally
    TextEdit.LockChangeEvents(False, False);
  end;
end;

{ TdxExcelFilterValueContainerSimpleCondition }

destructor TdxExcelFilterValueContainerComboBoxCondition.Destroy;
begin
  FreeAndNil(FComboBox);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerComboBoxCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateComboBox;
end;

procedure TdxExcelFilterValueContainerComboBoxCondition.GenerateComboBox;
begin
  FComboBox := TdxExcelFilterValueContainerValuesComboBox.Create(FilterValueContainer);
  ComboBox.OnKeyDown := KeyDownHandler;
  ComboBox.Properties.OnChange := EditChangeHandler;
  FComboBoxLayoutItem := ParentGroup.CreateItemForControl(ComboBox);
  ComboBoxLayoutItem.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerComboBoxCondition.GetDisplayText: string;
begin
  Result := ComboBox.GetFilterDisplayText;
end;

function TdxExcelFilterValueContainerComboBoxCondition.GetValue: Variant;
begin
  Result := ComboBox.GetFilterValue;
end;

function TdxExcelFilterValueContainerComboBoxCondition.IsEmpty: Boolean;
begin
  Result := ComboBox.IsEmpty;
end;

procedure TdxExcelFilterValueContainerComboBoxCondition.SetCaption(AValue: string);
begin
  ComboBoxLayoutItem.CaptionOptions.Text := AValue;
end;

procedure TdxExcelFilterValueContainerComboBoxCondition.UpdateData;
begin
  ComboBox.UpdateItems;
  inherited UpdateData;
end;

procedure TdxExcelFilterValueContainerComboBoxCondition.UpdateValues;
begin
  inherited UpdateValues;
  ComboBox.LockChangeEvents(True);
  try
    ComboBox.EditValue := ConditionContainer.ActiveCriteriaItems[0].Value;
  finally
    ComboBox.LockChangeEvents(False, False);
  end;
end;

{ TdxExcelFilterValueContainerBetweenCondition }

destructor TdxExcelFilterValueContainerBetweenCondition.Destroy;
begin
  FreeAndNil(FToComboBox);
  FreeAndNil(FFromComboBox);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerBetweenCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateFromComboBox;
  GenerateToComboBox;
end;

procedure TdxExcelFilterValueContainerBetweenCondition.GenerateFromComboBox;
begin
  FFromComboBox := TdxExcelFilterValueContainerValuesComboBox.Create(FilterValueContainer);
  FromComboBox.OnKeyDown := KeyDownHandler;
  FromComboBox.Properties.OnChange := EditChangeHandler;
  FFromLayoutItem := ParentGroup.CreateItemForControl(FromComboBox);
  FromLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterFromValueText);
  FromLayoutItem.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerBetweenCondition.GenerateToComboBox;
begin
  FToComboBox := TdxExcelFilterValueContainerValuesComboBox.Create(FilterValueContainer);
  ToComboBox.OnKeyDown := KeyDownHandler;
  ToComboBox.Properties.OnChange := EditChangeHandler;
  FToLayoutItem := ParentGroup.CreateItemForControl(ToComboBox);
  ToLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterToValueText);
  ToLayoutItem.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerBetweenCondition.GetDisplayText: string;
begin
  if FromComboBox.IsEmpty then
    Result := ToComboBox.GetFilterDisplayText
  else
    if ToComboBox.IsEmpty then
      Result := FromComboBox.GetFilterDisplayText
    else
      if VarCompare(GetValueFrom, GetValueTo) < 0 then
        Result := FromComboBox.GetFilterDisplayText + ';' + ToComboBox.GetFilterDisplayText
      else
        Result := ToComboBox.GetFilterDisplayText + ';' + FromComboBox.GetFilterDisplayText;
end;

function TdxExcelFilterValueContainerBetweenCondition.GetOperator: TcxFilterOperatorKind;
begin
  if FromComboBox.IsEmpty then
    Result := foLessEqual
  else
    if ToComboBox.IsEmpty then
      Result := foGreaterEqual
    else
      Result := inherited GetOperator;
end;

function TdxExcelFilterValueContainerBetweenCondition.GetValue: Variant;
begin
  if FromComboBox.IsEmpty then
    Result := GetValueTo
  else
    if ToComboBox.IsEmpty then
      Result := GetValueFrom
    else
      if VarCompare(GetValueFrom, GetValueTo) < 0 then
        Result := VarBetweenArrayCreate(GetValueFrom, GetValueTo)
      else
        Result := VarBetweenArrayCreate(GetValueTo, GetValueFrom);
end;

function TdxExcelFilterValueContainerBetweenCondition.GetValueFrom: Variant;
begin
  Result := FromComboBox.GetFilterValue;
end;

function TdxExcelFilterValueContainerBetweenCondition.GetValueTo: Variant;
begin
  Result := ToComboBox.GetFilterValue;
end;

function TdxExcelFilterValueContainerBetweenCondition.IsEmpty: Boolean;
begin
  Result := FromComboBox.IsEmpty and ToComboBox.IsEmpty;
end;

procedure TdxExcelFilterValueContainerBetweenCondition.UpdateData;
begin
  FromComboBox.UpdateItems;
  ToComboBox.Properties.Items := FromComboBox.Properties.Items;
  inherited UpdateData;
end;

procedure TdxExcelFilterValueContainerBetweenCondition.UpdateValues;
var
  AFromValue, AToValue: Variant;
begin
  inherited UpdateValues;
  if ConditionContainer.ActiveCriteriaItems[0].OperatorKind = foBetween then
  begin
    AFromValue := ConditionContainer.ActiveCriteriaItems[0].Value[0];
    AToValue := ConditionContainer.ActiveCriteriaItems[0].Value[1];
  end
  else
  begin
    AFromValue := ConditionContainer.ActiveCriteriaItems[0].Value;
    AToValue := ConditionContainer.ActiveCriteriaItems[1].Value;
  end;
  FromComboBox.LockChangeEvents(True);
  try
    FromComboBox.EditValue := AFromValue;
  finally
    FromComboBox.LockChangeEvents(False, False);
  end;
  ToComboBox.LockChangeEvents(True);
  try
    ToComboBox.EditValue := AToValue;
  finally
    ToComboBox.LockChangeEvents(False, False);
  end;
end;

{ TdxExcelFilterValueContainerCounterCondition }

destructor TdxExcelFilterValueContainerCounterCondition.Destroy;
begin
  FreeAndNil(FValueTypeComboBox);
  FreeAndNil(FValueSpinEdit);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerCounterCondition.CalculateFilterValueItemCount;
begin
  FFilterValueItemCount := FilterValueContainer.NonUniqueValues.Count;
end;

function TdxExcelFilterValueContainerCounterCondition.IsAppliedOnChoosing: Boolean;
begin
  Result := True;
end;

procedure TdxExcelFilterValueContainerCounterCondition.CalculateValues;

  function GetFilterValueIndexByIndex(AIndex: Integer): Integer;
  begin
    if ConditionContainer.GetConditionType = ctTopN then
      Result := FilterValueItemCount - AIndex - 1
    else
      Result := AIndex;
  end;

  function IsIndexInRange(AIndex: Integer): Boolean;
  begin
    if ValueTypeComboBox.ItemIndex = 0 then
      Result := AIndex + 1 <= ValueSpinEdit.Value
    else
      Result := (AIndex + 1) / FilterValueItemCount * 100 <= ValueSpinEdit.Value;
  end;

var
  AFilterDisplayValue: string;
  AFilterValueItem: TcxFilterValueItem;
  I, AFilterValueIndex: Integer;
begin
  FValue := Null;
  FDisplayText := '';
  for I := 0 to FilterValueItemCount - 1 do
    if IsIndexInRange(I) then
    begin
      AFilterValueIndex := GetFilterValueIndexByIndex(I);
      AFilterValueItem := FilterValueContainer.NonUniqueValues[AFilterValueIndex];
      AFilterDisplayValue := FilterValueContainer.NonUniqueDisplayValues[AFilterValueIndex];
      if I = 0 then
      begin
        FValue := VarListArrayCreate(AFilterValueItem.Value);
        FDisplayText := AFilterDisplayValue;
      end
      else
      begin
        VarListArrayAddValue(FValue, AFilterValueItem.Value);
        FDisplayText := FDisplayText + ';' + AFilterDisplayValue;
      end;
    end
    else
      Break;
end;

procedure TdxExcelFilterValueContainerCounterCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateValueSpinEdit;
  GenerateValueTypeComboBox;
end;

procedure TdxExcelFilterValueContainerCounterCondition.GenerateValueSpinEdit;
begin
  FValueSpinEdit := TcxSpinEdit.Create(nil);
  ValueSpinEdit.Value := dxCounterConditionStartCount;
  ValueSpinEdit.Style.TransparentBorder := False;
  ValueSpinEdit.Properties.OnChange := EditChangeHandler;
  ValueSpinEdit.OnKeyDown := KeyDownHandler;
  FValueLayoutItem := ParentGroup.CreateItemForControl(ValueSpinEdit);
  ValueLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterTopNBottomNValueLabel);
  ValueLayoutItem.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerCounterCondition.GenerateValueTypeComboBox;
begin
  FValueTypeComboBox := TcxComboBox.Create(nil);
  ValueTypeComboBox.Style.TransparentBorder := False;
  ValueTypeComboBox.Properties.DropDownListStyle := lsFixedList;
  ValueTypeComboBox.OnKeyDown := KeyDownHandler;
  ValueTypeComboBox.Properties.Items.Add(cxGetResourceString(@sdxExcelFilterTopNBottomNItemsText));
  ValueTypeComboBox.Properties.Items.Add(cxGetResourceString(@sdxExcelFilterTopNBottomNPercentText));
  ValueTypeComboBox.ItemIndex := 0;
  ValueTypeComboBox.Properties.OnChange := ValueTypeChangedHandler;
  FValueTypeLayoutItem := ParentGroup.CreateItemForControl(ValueTypeComboBox);
  ValueTypeLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterTopNBottomNTypeLabel);
  ValueTypeLayoutItem.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerCounterCondition.GetDisplayText: string;
begin
  Result := DisplayText;
end;

function TdxExcelFilterValueContainerCounterCondition.GetOperator: TcxFilterOperatorKind;
begin
  Result := foInList;
end;

function TdxExcelFilterValueContainerCounterCondition.GetValueSpinEditMaxCount: Integer;
begin
  if ValueTypeComboBox.ItemIndex = 0 then
    Result := FFilterValueItemCount
  else
    Result := 100;
end;

function TdxExcelFilterValueContainerCounterCondition.GetValue: Variant;
begin
  Result := Value;
end;

function TdxExcelFilterValueContainerCounterCondition.IsEmpty: Boolean;
begin
  Result := VarIsNull(Value);
end;

procedure TdxExcelFilterValueContainerCounterCondition.UpdateData;
begin
  inherited UpdateData;
  CalculateFilterValueItemCount;
  UpdateValueSpinEditMinMax;
  CalculateValues;
end;

procedure TdxExcelFilterValueContainerCounterCondition.UpdateValueSpinEditMinMax;
begin
  ValueSpinEdit.LockChangeEvents(True);
  try
    ValueSpinEdit.Properties.MaxValue := GetValueSpinEditMaxCount;
    ValueSpinEdit.Properties.AssignedValues.MaxValue := True;
    ValueSpinEdit.Properties.MinValue := 0;
    ValueSpinEdit.Properties.AssignedValues.MinValue := True;
  finally
    ValueSpinEdit.LockChangeEvents(False, False);
  end;
end;

procedure TdxExcelFilterValueContainerCounterCondition.ValueChanged;
begin
  CalculateValues;
  inherited ValueChanged;
end;

procedure TdxExcelFilterValueContainerCounterCondition.ValueTypeChangedHandler(ASender: TObject);
begin
  UpdateValueSpinEditMinMax;
  ValueChanged;
end;

{ TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod }

constructor TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.Create(
  AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  FCheckBoxList := TdxFastObjectList.Create;
end;

destructor TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.Destroy;
begin
  FreeAndNil(FCheckBoxList);
  FreeAndNil(FGroup);
  inherited Destroy;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.AddCheckBox(
  AOperator: TcxFilterOperatorKind): TcxCheckBox;
begin
  Result := TcxCheckBox.Create(nil);
  Result.Tag := NativeInt(AOperator);
  Result.Caption := dxGetConditionTypeResourceCaption(dxConditionTypeByOperatorMap[AOperator]);
  Result.Transparent := True;
  CheckBoxList.Add(Result);
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateGroup;
  GenerateLayoutItems;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.GenerateGroup;
begin
  FGroup := ParentGroup.CreateGroup;
  Group.ShowBorder := False;
  Group.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.GenerateLayoutItems;
var
  I: Integer;
  ALayoutItem: TdxLayoutItem;
begin
  for I := 0 to CheckBoxCount - 1 do
  begin
    ALayoutItem := Group.CreateItemForControl(CheckBoxes[I]);
    if I <> 0 then
      ALayoutItem.Offsets.Top := -8;
  end;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.GetCheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to CheckBoxCount - 1 do
    if CheckBoxes[I].Checked then
      Inc(Result);
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.UpdateCheckBoxStates(
  AOperators: TcxFilterOperatorKinds);
var
  I: Integer;
begin
  for I := 0 to CheckBoxCount - 1 do
  begin
    CheckBoxes[I].LockChangeEvents(True);
    try
      CheckBoxes[I].Checked := TcxFilterOperatorKind(CheckBoxes[I].Tag) in AOperators;
    finally
      CheckBoxes[I].LockChangeEvents(False, False);
    end;
  end;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.GetCheckBox(AIndex: Integer): TcxCheckBox;
begin
  Result := TcxCheckBox(CheckBoxList[AIndex]);
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.GetCheckBoxCount: Integer;
begin
  Result := CheckBoxList.Count;
end;

{ TdxExcelFilterValueContainerSpecificDatePeriodsCondition }

destructor TdxExcelFilterValueContainerSpecificDatePeriodsCondition.Destroy;
begin
  FreeAndNil(FPeriodList);
  FreeAndNil(FMonthYearPeriodsGroup);
  FreeAndNil(FDayWeekPeriodsGroup);
  FreeAndNil(FPeriodsGroup);
  inherited Destroy;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.AddPeriod:
  TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  Result := CreatePeriod;
  PeriodList.Add(Result);
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.AddPeriodCheckBox(
  APeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
  AOperator: TcxFilterOperatorKind);
var
  ACheckBox: TcxCheckBox;
begin
  ACheckBox := APeriod.AddCheckBox(AOperator);
  ACheckBox.Properties.OnChange := EditChangeHandler;
  ACheckBox.OnKeyDown := KeyDownHandler;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.CreatePeriod:
  TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  Result := TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod.Create(FilterValueContainer);
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateGroups;
  GeneratePeriods;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GenerateGroups;
begin
  FPeriodsGroup := ParentGroup.CreateGroup;
  PeriodsGroup.LayoutDirection := ldHorizontal;
  PeriodsGroup.ShowBorder := False;
  FDayWeekPeriodsGroup := PeriodsGroup.CreateGroup;
  DayWeekPeriodsGroup.ShowBorder := False;
  DayWeekPeriodsGroup.LayoutDirection := ldVertical;
  DayWeekPeriodsGroup.AlignHorz := ahClient;
  FMonthYearPeriodsGroup := PeriodsGroup.CreateGroup;
  MonthYearsPeriodsGroup.ShowBorder := False;
  MonthYearsPeriodsGroup.LayoutDirection := ldVertical;
  MonthYearsPeriodsGroup.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GeneratePeriods;
var
  APeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  FPeriodList := TdxFastObjectList.Create;
  APeriod := AddPeriod;
  AddPeriodCheckBox(APeriod, foYesterday);
  AddPeriodCheckBox(APeriod, foToday);
  AddPeriodCheckBox(APeriod, foTomorrow);
  APeriod.Generate(DayWeekPeriodsGroup);
  APeriod := AddPeriod;
  AddPeriodCheckBox(APeriod, foLastMonth);
  AddPeriodCheckBox(APeriod, foThisMonth);
  AddPeriodCheckBox(APeriod, foNextMonth);
  APeriod.Generate(MonthYearsPeriodsGroup);
  APeriod := AddPeriod;
  AddPeriodCheckBox(APeriod, foLastWeek);
  AddPeriodCheckBox(APeriod, foThisWeek);
  AddPeriodCheckBox(APeriod, foNextWeek);
  APeriod.Generate(DayWeekPeriodsGroup);
  APeriod := AddPeriod;
  AddPeriodCheckBox(APeriod, foLastYear);
  AddPeriodCheckBox(APeriod, foThisYear);
  AddPeriodCheckBox(APeriod, foNextYear);
  APeriod.Generate(MonthYearsPeriodsGroup);
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GetBoolOperator: TcxFilterBoolOperatorKind;
begin
  Result := fboOr;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GetCheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PeriodCount - 1 do
    Inc(Result, Periods[I].GetCheckedCount);
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GetOperator: TcxFilterOperatorKind;
var
  ACheckBox: TcxCheckBox;
  APeriodIndex, ACheckBoxIndex: Integer;
  APeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  Result := inherited GetOperator;
  for APeriodIndex := 0 to PeriodCount - 1 do
  begin
    APeriod := Periods[APeriodIndex];
    for ACheckBoxIndex := 0 to APeriod.CheckBoxCount - 1 do
    begin
      ACheckBox := APeriod.CheckBoxes[ACheckBoxIndex];
      if ACheckBox.Checked then
        Exit(TcxFilterOperatorKind(ACheckBox.Tag));
    end;
  end;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.IsComplex: Boolean;
begin
  Result := GetCheckedCount > 1;
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.IsEmpty: Boolean;
begin
  Result := GetCheckedCount = 0;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.PopulateValueInfos(AList: TdxFastObjectList);
var
  ACheckBox: TcxCheckBox;
  APeriodIndex, ACheckBoxIndex: Integer;
  AInfo: TdxExcelFilterValueContainerValueInfo;
  APeriod: TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  for APeriodIndex := 0 to PeriodCount - 1 do
  begin
    APeriod := Periods[APeriodIndex];
    for ACheckBoxIndex := 0 to APeriod.CheckBoxCount - 1 do
    begin
      ACheckBox := APeriod.CheckBoxes[ACheckBoxIndex];
      if ACheckBox.Checked then
      begin
        AInfo := TdxExcelFilterValueContainerValueInfo.Create(GetValue, GetDisplayText,
          TcxFilterOperatorKind(ACheckBox.Tag));
        AList.Add(AInfo);
      end;
    end;
  end;
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.UpdatePeriodCheckBoxStates(
  AOperators: TcxFilterOperatorKinds);
var
  I: Integer;
begin
  for I := 0 to PeriodCount - 1 do
    Periods[I].UpdateCheckBoxStates(AOperators);
end;

procedure TdxExcelFilterValueContainerSpecificDatePeriodsCondition.UpdateValues;
var
  I: Integer;
  AOperators: TcxFilterOperatorKinds;
begin
  inherited UpdateValues;
  AOperators := [];
  for I := 0 to ConditionContainer.ActiveCriteriaItems.Count - 1 do
    Include(AOperators, ConditionContainer.ActiveCriteriaItems[I].OperatorKind);
  UpdatePeriodCheckBoxStates(AOperators);
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GetPeriod(
  AIndex: Integer): TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod;
begin
  Result := TdxExcelFilterValueContainerSpecificDatePeriodsConditionPeriod(PeriodList[AIndex]);
end;

function TdxExcelFilterValueContainerSpecificDatePeriodsCondition.GetPeriodCount: Integer;
begin
  Result := PeriodList.Count;
end;

{ TdxExcelFilterValueContainerCustomFilterConditionAndOr }

destructor TdxExcelFilterValueContainerCustomFilterConditionAndOr.Destroy;
begin
  FreeAndNil(FOrRadioButton);
  FreeAndNil(FAndRadioButton);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerCustomFilterConditionAndOr.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateAndRadioButton;
  GenerateOrRadioButton;
end;

procedure TdxExcelFilterValueContainerCustomFilterConditionAndOr.GenerateAndRadioButton;
begin
  FAndRadioButton := TcxRadioButton.Create(nil);
  AndRadioButton.GroupIndex := 1;
  AndRadioButton.Caption := cxGetResourceString(@sdxExcelFilterCustomFilterAndOperatorLabel);
  AndRadioButton.AutoSize := True;
  AndRadioButton.Transparent := True;
  FAndLayoutItem := ParentGroup.CreateItemForControl(AndRadioButton);
  FAndLayoutItem.CaptionOptions.Text := ' ';
end;

procedure TdxExcelFilterValueContainerCustomFilterConditionAndOr.GenerateOrRadioButton;
begin
  FOrRadioButton := TcxRadioButton.Create(nil);
  OrRadioButton.GroupIndex := 1;
  OrRadioButton.Caption := cxGetResourceString(@sdxExcelFilterCustomFilterOrOperatorLabel);
  OrRadioButton.AutoSize := True;
  OrRadioButton.Checked := True;
  OrRadioButton.Transparent := True;
  FOrLayoutItem := ParentGroup.CreateItemForControl(OrRadioButton);
  FOrLayoutItem.CaptionOptions.Visible := False;
end;

{ TdxExcelFilterValueContainerCustomFilterCondition }

destructor TdxExcelFilterValueContainerCustomFilterCondition.Destroy;
begin
  FreeAndNil(FChildConditionContainer2);
  FreeAndNil(FAndOr);
  FreeAndNil(FAndOrGroup);
  FreeAndNil(FChildConditionContainer1);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateChildConditionContainer1;
  GenerateAndOr;
  GenerateChildConditionContainer2;
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.AndOrClickHandler(ASender: TObject);
begin
  ValueChanged;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.CreateConditionContainer:
  TdxExcelFilterValueContainerCustomFilterConditionContainer;
begin
  Result := TdxExcelFilterValueContainerCustomFilterConditionContainer.Create(FilterValueContainer, Self);
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.GenerateAndOr;
begin
  FAndOrGroup := ParentGroup.CreateGroup;
  AndOrGroup.ShowBorder := False;
  AndOrGroup.LayoutDirection := ldHorizontal;
  FAndOr := TdxExcelFilterValueContainerCustomFilterConditionAndOr.Create(FilterValueContainer);
  AndOr.Generate(AndOrGroup);
  SetAndOrClickHandler(AndOrClickHandler);
  SetAndOrKeyDownHandler(KeyDownHandler);
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.GenerateChildConditionContainer1;
begin
  FChildConditionContainer1 := CreateConditionContainer;
  ChildConditionContainer1.Generate(ParentGroup);
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.GenerateChildConditionContainer2;
begin
  FChildConditionContainer2 := CreateConditionContainer;
  ChildConditionContainer2.Generate(ParentGroup);
end;

function TdxExcelFilterValueContainerCustomFilterCondition.GetBoolOperator: TcxFilterBoolOperatorKind;
begin
  if AndOr.AndRadioButton.Checked then
    Result := fboAnd
  else
    Result  := fboOr;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.GetDisplayText: string;
begin
  if not ChildConditionContainer1.IsConditionEmpty then
    Result := ChildConditionContainer1.Condition.GetDisplayText
  else
    if not ChildConditionContainer2.IsConditionEmpty then
      Result := ChildConditionContainer2.Condition.GetDisplayText
    else
      Result := inherited GetDisplayText;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.GetOperator: TcxFilterOperatorKind;
begin
  if not ChildConditionContainer1.IsConditionEmpty then
    Result := ChildConditionContainer1.Condition.GetOperator
  else
    if not ChildConditionContainer2.IsConditionEmpty then
      Result := ChildConditionContainer2.Condition.GetOperator
    else
      Result := inherited GetOperator;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.GetSupportedConditionTypes:
  TdxExcelFilterValueContainerConditionTypes;
begin
  case FilterValueContainer.GetDataType of
    fdtText:
      Result := [ctEquals..ctIsNotBlank];
    fdtNumeric:
      Result := [ctEquals, ctDoesNotEqual, ctGreaterThan..ctLessEqualThanOrEqualTo];
    fdtDate:
      Result := [ctEquals, ctDoesNotEqual, ctBefore, ctAfter];
    else
      Result := [];
  end;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.GetValue: Variant;
begin
  if not ChildConditionContainer1.IsConditionEmpty then
    Result := ChildConditionContainer1.Condition.GetValue
  else
    if not ChildConditionContainer2.IsConditionEmpty then
      Result := ChildConditionContainer2.Condition.GetValue
    else
      Result := inherited GetValue;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.IsComplex: Boolean;
begin
  Result := not ChildConditionContainer1.IsConditionEmpty and not ChildConditionContainer2.IsConditionEmpty;
end;

function TdxExcelFilterValueContainerCustomFilterCondition.IsEmpty: Boolean;
begin
  Result := ChildConditionContainer1.IsConditionEmpty and ChildConditionContainer2.IsConditionEmpty;
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.PopulateActiveCriteriaItems(
  AContainer: TdxExcelFilterValueContainerCustomFilterConditionContainer);
var
  ACriteriaItem: TcxFilterCriteriaItem;
begin
  if ConditionContainer.ActiveCriteriaItems.Count = 2 then
    if AContainer = ChildConditionContainer1 then
      ACriteriaItem := ConditionContainer.ActiveCriteriaItems[0]
    else
      ACriteriaItem := ConditionContainer.ActiveCriteriaItems[1]
  else
    if AContainer = ChildConditionContainer1 then
      ACriteriaItem := ConditionContainer.ActiveCriteriaItems[0]
    else
      ACriteriaItem := nil;
  if ACriteriaItem <> nil then
    AContainer.ActiveCriteriaItems.Add(ACriteriaItem)
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.PopulateValueInfos(AList: TdxFastObjectList);
var
  AInfo: TdxExcelFilterValueContainerValueInfo;
begin
  AInfo := TdxExcelFilterValueContainerValueInfo.Create(ChildConditionContainer1.Condition.GetValue,
    ChildConditionContainer1.Condition.GetDisplayText, ChildConditionContainer1.Condition.GetOperator);
  AList.Add(AInfo);
  AInfo := TdxExcelFilterValueContainerValueInfo.Create(ChildConditionContainer2.Condition.GetValue,
    ChildConditionContainer2.Condition.GetDisplayText, ChildConditionContainer2.Condition.GetOperator);
  AList.Add(AInfo);
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.SetAndOrClickHandler(AEvent: TNotifyEvent);
begin
  AndOr.AndRadioButton.OnClick := AEvent;
  AndOr.OrRadioButton.OnClick := AEvent;
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.SetAndOrKeyDownHandler(AEvent: TKeyEvent);
begin
  AndOr.AndRadioButton.OnKeyDown := AEvent;
  AndOr.OrRadioButton.OnKeyDown := AEvent;
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.UpdateData;
begin
  inherited UpdateData;
  ChildConditionContainer1.SupportedConditionTypes := GetSupportedConditionTypes;
  ChildConditionContainer2.SupportedConditionTypes := GetSupportedConditionTypes;
  ChildConditionContainer1.UpdateData;
  ChildConditionContainer2.UpdateData;
  ChildConditionContainer1.SetCaption(cxGetResourceString(@sdxExcelFilterCustomFilterFirstConditionLabel));
  ChildConditionContainer2.SetCaption(cxGetResourceString(@sdxExcelFilterCustomFilterSecondConditionLabel));
end;

procedure TdxExcelFilterValueContainerCustomFilterCondition.UpdateValues;
begin
  inherited UpdateValues;
  SetAndOrClickHandler(nil);
  try
    AndOr.OrRadioButton.Checked := ConditionContainer.ActiveBoolOperator = fboOr;
    AndOr.AndRadioButton.Checked := ConditionContainer.ActiveBoolOperator = fboAnd;
  finally
    SetAndOrClickHandler(EditChangeHandler);
  end;
end;

{ TdxExcelFilterValueContainerConditionContainer }

constructor TdxExcelFilterValueContainerConditionContainer.Create(AFilterValueContainer: TdxExcelFilterValueContainer);
begin
  inherited Create(AFilterValueContainer);
  FActiveCriteriaItems := TList<TcxFilterCriteriaItem>.Create;
  FCustomFilterConditionClass := TdxExcelFilterValueContainerCustomFilterCondition;
end;

destructor TdxExcelFilterValueContainerConditionContainer.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FConditionTypeComboBox);
  FreeAndNil(FLayoutGroup);
  FreeAndNil(FActiveCriteriaItems);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerConditionContainer.ApplyFilter;
var
  AValueInfos: TdxFastObjectList;
begin
  if IsConditionEmpty then
    FilterValueContainer.ResetFilter
  else
    if Condition.IsComplex then
    begin
      AValueInfos := TdxFastObjectList.Create;
      try
        Condition.PopulateValueInfos(AValueInfos);
        FilterableComponent.SetFilterActiveValues(AValueInfos, Condition.GetBoolOperator);
      finally
        AValueInfos.Free;
      end;
    end
    else
      FilterableComponent.SetFilterActiveValue(Condition.GetOperator, Condition.GetValue, Condition.GetDisplayText);
  UpdateActiveCriteriaItems;
  FilterValueContainer.IsModified := False;
end;

procedure TdxExcelFilterValueContainerConditionContainer.ConditionChanged;
begin
  FilterValuesChanged;
end;

procedure TdxExcelFilterValueContainerConditionContainer.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateLayoutGroup;
  GenerateConditionTypeComboBox;
end;

procedure TdxExcelFilterValueContainerConditionContainer.FilterValuesChanged;
begin
  FilterValueContainer.IsModified := True;
  if FilterValueContainer.NeedImmediateApply then
    ApplyFilter;
end;

procedure TdxExcelFilterValueContainerConditionContainer.FocusMainItem;
begin
  ConditionTypeComboBox.SetFocus;
end;

procedure TdxExcelFilterValueContainerConditionContainer.GenerateCondition;
var
  AConditionClass: TdxExcelFilterValueContainerConditionClass;
begin
  case GetConditionType of
    ctEquals, ctDoesNotEqual, ctGreaterThan..ctLessEqualThanOrEqualTo, ctBefore, ctAfter:
      AConditionClass := TdxExcelFilterValueContainerComboBoxCondition;
    ctBetween:
      AConditionClass := TdxExcelFilterValueContainerBetweenCondition;
    ctTopN, ctBottomN:
      AConditionClass := TdxExcelFilterValueContainerCounterCondition;
    ctBeginsWith..ctDoesNotContain:
      AConditionClass := TdxExcelFilterValueContainerTextEditCondition;
    ctSpecificDatePeriods:
      AConditionClass := TdxExcelFilterValueContainerSpecificDatePeriodsCondition;
    ctCustomFilter:
      AConditionClass := CustomFilterConditionClass;
    ctBelowAverage, ctAboveAverage:
      AConditionClass := TdxExcelFilterValueContainerAverageCondition;
    else
      AConditionClass := TdxExcelFilterValueContainerCondition;
  end;
  FCondition := AConditionClass.Create(FilterValueContainer, Self);
  Condition.Generate(LayoutGroup);
end;

procedure TdxExcelFilterValueContainerConditionContainer.GenerateConditionTypeComboBox;
begin
  FConditionTypeComboBox := TdxExcelFilterValueContainerConditionTypesComboBox.Create(nil);
  ConditionTypeComboBox.OnKeyDown := KeyDownHandler;
  ConditionTypeComboBox.Properties.OnChange := ConditionTypeChangedHandler;
  FConditionTypeLayoutItem := LayoutGroup.CreateItemForControl(ConditionTypeComboBox);
  ConditionTypeLayoutItem.AlignHorz := ahClient;
  FNeedUpdateConditionTypeItems := True;
end;

procedure TdxExcelFilterValueContainerConditionContainer.GenerateLayoutGroup;
begin
  FLayoutGroup := ParentGroup.CreateGroup;
  LayoutGroup.ShowBorder := False;
  LayoutGroup.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerConditionContainer.GetActiveConditionType(
  out AHasActiveCondition: Boolean): TdxExcelFilterValueContainerConditionType;
begin
  Result := ctEquals;
  AHasActiveCondition := ActiveCriteriaItems.Count > 0;
  if not AHasActiveCondition then
    Exit;
  if (FilterValueContainer.GetDataType = fdtDate) and (ActiveCriteriaItems.First.OperatorKind in dxDatePeriodOperators) then
    Result := ctSpecificDatePeriods
  else
    if ActiveCriteriaItems.Count = 2 then
      if IsActiveCriteriaItemsContainBetweenCondition then
        Result := ctBetween
      else
        Result := ctCustomFilter
    else
      if ActiveCriteriaItems.Count = 1 then
        Result := dxGetConditionTypeByOperator(ActiveCriteriaItems[0].OperatorKind, ActiveCriteriaItems[0].Value);
end;

function TdxExcelFilterValueContainerConditionContainer.GetConditionType: TdxExcelFilterValueContainerConditionType;
begin
  Result := TdxExcelFilterValueContainerConditionType(ConditionTypeComboBox.ItemObject);
end;

function TdxExcelFilterValueContainerConditionContainer.GetConditionTypeItemIndex(
  AConditionType: TdxExcelFilterValueContainerConditionType): Integer;
begin
  Result := ConditionTypeComboBox.Properties.Items.IndexOfObject(TObject(AConditionType));
end;

function TdxExcelFilterValueContainerConditionContainer.GetDefaultConditionTypeIndex: Integer;
begin
  Result := 0;
end;

function TdxExcelFilterValueContainerConditionContainer.HasSelectedConditionType: Boolean;
begin
  Result := ConditionTypeComboBox.ItemIndex <> -1;
end;

function TdxExcelFilterValueContainerConditionContainer.IsActiveCriteriaItemsContainBetweenCondition: Boolean;
begin
  Result := (ActiveCriteriaItems.Count > 0) and ((ActiveCriteriaItems[0].OperatorKind = foBetween) or
    (FilterValueContainer.GetDataType in [fdtNumeric, fdtTime]) and (ActiveBoolOperator = fboAnd) and
    ((ActiveCriteriaItems[0].OperatorKind = foLessEqual) and (ActiveCriteriaItems[1].OperatorKind = foGreaterEqual) or
    (ActiveCriteriaItems[0].OperatorKind = foGreaterEqual) and (ActiveCriteriaItems[1].OperatorKind = foLessEqual)));
end;

function TdxExcelFilterValueContainerConditionContainer.IsConditionPresentInFilter: Boolean;
var
  AHasActiveCondition: Boolean;
  AActiveType, ASelectedType: TdxExcelFilterValueContainerConditionType;
begin
  AActiveType := GetActiveConditionType(AHasActiveCondition);
  ASelectedType := TdxExcelFilterValueContainerConditionType(ConditionTypeComboBox.ItemObject);
  Result := AHasActiveCondition and ((ASelectedType = AActiveType) or (ASelectedType = ctCustomFilter) and
    (AActiveType in [ctEquals..ctIsNotBlank, ctGreaterThan..ctLessEqualThanOrEqualTo]));
end;

function TdxExcelFilterValueContainerConditionContainer.IsConditionEmpty: Boolean;
begin
  Result := Condition.IsEmpty;
end;

procedure TdxExcelFilterValueContainerConditionContainer.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Sender = ConditionTypeComboBox) then
    ConditionTypeComboBox.ModifiedAfterEnter := False;
  inherited KeyDownHandler(Sender, Key, Shift);
end;

function TdxExcelFilterValueContainerConditionContainer.PopulateActiveCriteriaItems(
  AItemList: TcxFilterCriteriaItemList): Boolean;
begin
  case AItemList.BoolOperatorKind of
    fboOr:
      Result := PopulateActiveCriteriaItemsByOrItemList(AItemList);
    fboAnd:
      Result := PopulateActiveCriteriaItemsByAndItemList(AItemList);
    else
      Result := False;
  end;
end;

procedure TdxExcelFilterValueContainerConditionContainer.RegenerateCondition;
begin
  FreeAndNil(FCondition);
  GenerateCondition;
end;

procedure TdxExcelFilterValueContainerConditionContainer.SetCaption(AValue: string);
begin
  ConditionTypeLayoutItem.CaptionOptions.Text := AValue;
  UpdateConditionCaption;
end;

procedure TdxExcelFilterValueContainerConditionContainer.UpdateActiveCriteriaItems;
begin
  ActiveCriteriaItems.Clear;
  PopulateActiveCriteriaItems(FilterValueContainer.Filter.Root);
end;

procedure TdxExcelFilterValueContainerConditionContainer.UpdateConditionCaption;
begin
  if ConditionTypeLayoutItem.CaptionOptions.Text = '' then
    Condition.SetCaption('')
  else
    Condition.SetCaption(' ');
end;

procedure TdxExcelFilterValueContainerConditionContainer.UpdateConditionType;
var
  AType: TdxExcelFilterValueContainerConditionType;
  AHasActiveCondition: Boolean;
begin
  AType := GetActiveConditionType(AHasActiveCondition);
  if AHasActiveCondition then
    ConditionTypeComboBox.ItemIndex := GetConditionTypeItemIndex(AType)
  else
    if not HasSelectedConditionType then
      ConditionTypeComboBox.ItemIndex := GetDefaultConditionTypeIndex;
end;

procedure TdxExcelFilterValueContainerConditionContainer.UpdateData;
begin
  inherited UpdateData;
  ConditionTypeComboBox.SupportedConditionTypes := SupportedConditionTypes;
  if NeedUpdateConditionTypeItems then
  begin
    ConditionTypeComboBox.UpdateItems;
    FNeedUpdateConditionTypeItems := False;
  end;
  UpdateActiveCriteriaItems;
  UpdateConditionType;
  Condition.UpdateData;
end;

procedure TdxExcelFilterValueContainerConditionContainer.ConditionTypeChangedHandler(Sender: TObject);
begin
  RegenerateCondition;
  UpdateConditionCaption;
  Condition.UpdateData;
  if Condition.IsAppliedOnChoosing and not IsConditionPresentInFilter then
    FilterValuesChanged;
end;

function TdxExcelFilterValueContainerConditionContainer.PopulateActiveCriteriaItemsByAndItemList(
  AItemList: TcxFilterCriteriaItemList): Boolean;
var
  I, ACount: Integer;
  AFilterItem: TcxFilterCriteriaItem;
  AFilterList: TcxFilterCriteriaItemList;
  ABoolOperator: TcxFilterBoolOperatorKind;
  AConditionType: TdxExcelFilterValueContainerConditionType;
begin
  ACount := 0;
  Result := False;
  ABoolOperator := ActiveBoolOperator;
  for I := 0 to AItemList.Count - 1 do
  begin
    if not AItemList.Items[I].IsItemList then
    begin
      AFilterItem := TcxFilterCriteriaItem(AItemList.Items[I]);
      if AFilterItem.ItemLink = FilterValueContainer.FilterItemLink then
      begin
        Inc(ACount);
        Result := True;
        AConditionType := dxGetConditionTypeByOperator(AFilterItem.OperatorKind, AFilterItem.Value);
        if (AFilterItem.OperatorKind = dxOperatorByConditionTypeMap[AConditionType]) and
          (AConditionType in SupportedConditionTypes) then
          if ACount > 2 then
          begin
            ActiveCriteriaItems.Clear;
            FActiveBoolOperator := ABoolOperator;
            Break
          end
          else
          begin
            ActiveCriteriaItems.Add(AFilterItem);
            FActiveBoolOperator := fboAnd;
          end;
      end;
    end
    else
      if not Result then
      begin
        AFilterList := TcxFilterCriteriaItemList(AItemList.Items[I]);
        Result := PopulateActiveCriteriaItems(AFilterList);
        if Result then
          Break;
      end;
  end;
end;

function TdxExcelFilterValueContainerConditionContainer.PopulateActiveCriteriaItemsByOrItemList(
  AItemList: TcxFilterCriteriaItemList): Boolean;
var
  I: Integer;
  ABreak, AHasNonPeriodItem: Boolean;
  AFilterItem: TcxFilterCriteriaItem;
  ABoolOperator: TcxFilterBoolOperatorKind;
  AConditionType: TdxExcelFilterValueContainerConditionType;
begin
  Result := False;
  if (AItemList.Count > 2) and (FilterValueContainer.GetDataType <> fdtDate) then
    Exit;
  AHasNonPeriodItem := False;
  ABoolOperator := ActiveBoolOperator;
  for I := 0 to AItemList.Count - 1 do
  begin
    ABreak := AItemList.Items[I].IsItemList;
    if not ABreak then
    begin
      AFilterItem := TcxFilterCriteriaItem(AItemList.Items[I]);
      ABreak := AFilterItem.ItemLink <> FilterValueContainer.FilterItemLink;
      if not ABreak then
      begin
        AConditionType := dxGetConditionTypeByOperator(AFilterItem.OperatorKind, AFilterItem.Value);
        ABreak := (AFilterItem.OperatorKind <> dxOperatorByConditionTypeMap[AConditionType]) or
          not(AConditionType in SupportedConditionTypes);
        if not ABreak then
        begin
          AHasNonPeriodItem := AHasNonPeriodItem or (FilterValueContainer.GetDataType <> fdtDate) or
            not (AFilterItem.OperatorKind in dxDatePeriodOperators);
          ABreak := AHasNonPeriodItem and (I > 1);
          if not ABreak then
          begin
            Result := True;
            ActiveCriteriaItems.Add(AFilterItem);
            FActiveBoolOperator := fboOr;
          end;
        end;
      end;
    end;
    if ABreak then
    begin
      ActiveCriteriaItems.Clear;
      FActiveBoolOperator := ABoolOperator;
      Break;
    end;
  end;
end;

procedure TdxExcelFilterValueContainerConditionContainer.SetSupportedConditionTypes(
  const AValue: TdxExcelFilterValueContainerConditionTypes);
begin
  if AValue <> SupportedConditionTypes then
  begin
    FSupportedConditionTypes := AValue;
    FNeedUpdateConditionTypeItems := True;
  end;
end;

{ TdxExcelFilterValueContainerCustomFilterConditionContainer }

constructor TdxExcelFilterValueContainerCustomFilterConditionContainer.Create(
  AFilterValueContainer: TdxExcelFilterValueContainer; ACustomFilterCondition: TdxExcelFilterValueContainerCustomFilterCondition);
begin
  inherited Create(AFilterValueContainer);
  FCustomFilterCondition := ACustomFilterCondition;
end;

procedure TdxExcelFilterValueContainerCustomFilterConditionContainer.ApplyFilter;
begin
  CustomFilterCondition.ConditionContainer.ApplyFilter;
end;

function TdxExcelFilterValueContainerCustomFilterConditionContainer.PopulateActiveCriteriaItems(
  AItemList: TcxFilterCriteriaItemList): Boolean;
begin
  Result := CustomFilterCondition.IsPresentInFilter;
  if Result then
    CustomFilterCondition.PopulateActiveCriteriaItems(Self);
end;

{ TdxExcelFilterValueContainerRangeModeValuesPageBetween }

constructor TdxExcelFilterValueContainerRangeModeValuesPageBetween.Create(
  AFilterValueContainer: TdxExcelFilterValueContainer; AOwner: TdxExcelFilterValueContainerRangeModeValuesPage);
begin
  inherited Create(AFilterValueContainer);
  FOwner := AOwner;
end;

destructor TdxExcelFilterValueContainerRangeModeValuesPageBetween.Destroy;
begin
  FreeAndNil(FFromEdit);
  FreeAndNil(FToEdit);
  inherited Destroy;
end;

function TdxExcelFilterValueContainerRangeModeValuesPageBetween.CreateEdit: TcxCustomEdit;
begin
  Result := FilterValueContainer.FilterHelper.GetFilterEdit(FilterValueContainer.FilterProperties);
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateFromEdit;
  GenerateToEdit;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.FocusMainItem;
begin
  FromEdit.SetFocus;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.GenerateFromEdit;
begin
  FFromEdit := CreateEdit;
  InitEdit(FromEdit);
  FFromLayoutItem := ParentGroup.CreateItemForControl(FromEdit);
  FromLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterFromValueText);
  FromLayoutItem.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.GenerateToEdit;
begin
  FToEdit := CreateEdit;
  InitEdit(ToEdit);
  FToLayoutItem := ParentGroup.CreateItemForControl(ToEdit);
  ToLayoutItem.CaptionOptions.Text := cxGetResourceString(@sdxExcelFilterToValueText);
  ToLayoutItem.AlignHorz := ahClient;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.InitEdit(AEdit: TcxCustomEdit);
begin
  AEdit.Style.TransparentBorder := False;
  AEdit.OnKeyDown := KeyDownHandler;
  AEdit.GetProperties.ValidationOptions := [evoAllowLoseFocus];
  AEdit.GetProperties.OnChange := EditChangeHandler;
  AEdit.GetProperties.OnValidate := EditValidateHandler;
end;

function TdxExcelFilterValueContainerRangeModeValuesPageBetween.IsValueValid(AValue: Variant): Boolean;
var
  AVal: Double;
begin
  Result := VarIsNumeric(AValue) or VarIsStr(AValue) and TryStrToFloat(AValue, AVal);
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.UpdateData;
begin
  inherited UpdateData;
  UpdateEditValue(FromEdit, Owner.FromValue);
  UpdateEditValue(ToEdit, Owner.ToValue);
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.UpdateEditValue(AEdit: TcxCustomEdit; AValue: Variant);
begin
  AEdit.LockChangeEvents(True);
  try
    AEdit.EditValue := AValue;
  finally
    AEdit.LockChangeEvents(False, False);
  end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.ValueChanged;
begin
  Owner.SetValues(FromEdit.EditingValue, ToEdit.EditingValue);
  Owner.UpdateRangeTrackBar;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.EditChangeHandler(ASender: TObject);
var
  AError: Boolean;
  AErrorText: TCaption;
  ADisplayValue: Variant;
  AEdit: TcxCustomEdit absolute ASender;
begin
  if AEdit.EditModified then
  begin
    AError := False;
    AErrorText := '';
    ADisplayValue := AEdit.GetDisplayValue;
    AEdit.GetProperties.ValidateDisplayValue(ADisplayValue, AErrorText, AError, AEdit);
    if not AError then
      ValueChanged;
  end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPageBetween.EditValidateHandler(
  ASender: TObject; var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);
var
  AEditValue: Variant;
  AEdit: TcxCustomEdit absolute ASender;
begin
  AEdit.PrepareEditValue(ADisplayValue, AEditValue, AEdit.Focused);
  AError := AError or not IsValueValid(AEditValue);
  if (AEdit = ToEdit) and (AError or (VarCompare(AEditValue, Owner.FromValue) < 0)) then
    AEdit.GetProperties.PrepareDisplayValue(Owner.FromValue, ADisplayValue, AEdit.Focused)
  else
    if (AEdit = FromEdit) then
      if AError then
        AEdit.GetProperties.PrepareDisplayValue(Owner.MinValue, ADisplayValue, AEdit.Focused)
      else
        if VarCompare(AEditValue, Owner.ToValue) > 0 then
          AEdit.GetProperties.PrepareDisplayValue(Owner.ToValue, ADisplayValue, AEdit.Focused)
end;

{ TdxExcelFilterValueContainerCustomValuesPage }

procedure TdxExcelFilterValueContainerCustomPage.ApplyFilter;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerCustomPage.FocusMainItem;
begin
//do nothing
end;

{ TdxExcelFilterValueContainerSearchableValuesPage }

destructor TdxExcelFilterValueContainerSearchableValuesPage.Destroy;
begin
  DestroyTimer;
  FreeAndNil(FSearchEdit);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateSearchEdit;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.DrawSearchEditGlyph;
var
  ASize: TSize;
  ABitmap: TcxBitmap32;
  APainter: TcxCustomLookAndFeelPainter;
begin
  APainter := SearchEdit.Style.LookAndFeel.Painter;
  ASize := APainter.ScaledSearchButtonGlyphSize(SearchEdit.GetScaleFactor);
  ABitmap := TcxBitmap32.CreateSize(ASize.cx, ASize.cy, True);
  try
    APainter.DrawScaledSearchEditButtonGlyph(ABitmap.cxCanvas, ABitmap.ClientRect, cxbsNormal, SearchEdit.GetScaleFactor);
    SearchEdit.Properties.Buttons[0].Glyph.Assign(ABitmap);
    SearchEdit.Properties.Buttons[0].Kind := bkGlyph;
  finally
    ABitmap.Free;
  end;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.FocusMainItem;
begin
  SearchEdit.SetFocus;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.GenerateSearchEdit;
begin
  FSearchEdit := TcxButtonEdit.Create(nil);
  SearchEdit.Style.TransparentBorder := False;
  SearchEdit.OnKeyDown := KeyDownHandler;
  SearchEdit.Properties.OnChange := SearchEditValueChanged;
  DrawSearchEditGlyph;
  FSearchLayoutItem := ParentGroup.CreateItemForControl(SearchEdit);
  SearchLayoutItem.AlignHorz := ahClient;
  SearchLayoutItem.Padding.Bottom := -3;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited KeyDownHandler(Sender, Key, Shift);
  if Sender = SearchEdit then
    SearchEditKeyDown(Key, Shift)
  else
    if (Key = Ord('F')) and (Shift = [ssCtrl]) then
      SearchEdit.SetFocus;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.SearchEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SearchTextChanged;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.SearchEditValueChanged(ASender: TObject);
begin
  if SearchEdit.EditModified then
    StartDelayedSearch;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.SearchTextChanged;
begin
  DestroyTimer;
  UpdateValues;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.StartDelayedSearch;
begin
  DestroyTimer;
  CreateTimer;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.UpdateData;
begin
  inherited UpdateData;
  FilterValueContainer.ResetCheckedValueIndexes;
  FilterValueContainer.PopulateCheckedValueIndexes;
  UpdateValues;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.UpdateValues;
begin
  FSearchText := SearchEdit.Text;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.CreateTimer;
begin
  FDelayedTimer := TcxTimer.Create(nil);
  FDelayedTimer.OnTimer := OnDelayedSearchTimer;
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.DestroyTimer;
begin
  FreeAndNil(FDelayedTimer);
end;

procedure TdxExcelFilterValueContainerSearchableValuesPage.OnDelayedSearchTimer(Sender: TObject);
begin
  SearchTextChanged;
end;

{ TdxExcelFilterValueContainerListBoxModeValuesPage }

destructor TdxExcelFilterValueContainerListBoxModeValuesPage.Destroy;
begin
  FreeAndNil(FListBox);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerListBoxModeValuesPage.ApplyFilter;
begin
  inherited ApplyFilter;
  ListBox.ApplyFilter;
end;

procedure TdxExcelFilterValueContainerListBoxModeValuesPage.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateListBox;
end;

procedure TdxExcelFilterValueContainerListBoxModeValuesPage.FocusMainItem;
begin
  ListBox.SetFocus;
end;

procedure TdxExcelFilterValueContainerListBoxModeValuesPage.GenerateListBox;
begin
  FListBox := FilterValueContainer.GetListBoxClass.Create(FilterValueContainer);
  ListBox.OnKeyDown := KeyDownHandler;
  FListBoxLayoutItem := ParentGroup.CreateItemForControl(ListBox);
  ListBoxLayoutItem.AlignHorz := ahClient;
  ListBoxLayoutItem.AlignVert := avClient;
end;

procedure TdxExcelFilterValueContainerListBoxModeValuesPage.UpdateValues;
begin
  inherited UpdateValues;
  ListBox.IncrementalFilteringText := SearchText;
  ListBox.UpdateItems;
  ListBox.UpdateItemStates;
end;

{ TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage }

destructor TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.Destroy;
begin
  FreeAndNil(FTreeView);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.ApplyFilter;
begin
  inherited ApplyFilter;
  TreeView.ApplyFilter;
end;

function TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.CreateTreeView:
  TdxExcelFilterValueContainerCustomDateTimeTreeView;
begin
  Result := TdxExcelFilterValueContainerCustomDateTimeTreeView.Create(FilterValueContainer);
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateTreeView;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.FocusMainItem;
begin
  TreeView.SetFocus;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.GenerateTreeView;
begin
  FTreeView := CreateTreeView;
  TreeView.OnKeyDown := KeyDownHandler;
  FTreeViewLayoutItem := ParentGroup.CreateItemForControl(TreeView);
  TreeViewLayoutItem.AlignHorz := ahClient;
  TreeViewLayoutItem.AlignVert := avClient;
end;

procedure TdxExcelFilterValueContainerCustomDateTimeTreeViewModeValuesPage.UpdateValues;
begin
  inherited UpdateValues;
  TreeView.IncrementalFilteringText := SearchText;
  TreeView.UpdateItems;
  TreeView.UpdateItemStates;
end;

{ TdxExcelFilterValueContainerDateTreeViewModeValuesPage }

function TdxExcelFilterValueContainerDateTreeViewModeValuesPage.CreateTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView;
begin
  Result := TdxExcelFilterValueContainerDateTreeView.Create(FilterValueContainer);
end;

{ TdxExcelFilterValueContainerTimeTreeViewModeValuesPage }

function TdxExcelFilterValueContainerTimeTreeViewModeValuesPage.CreateTreeView: TdxExcelFilterValueContainerCustomDateTimeTreeView;
begin
  Result := TdxExcelFilterValueContainerTimeTreeView.Create(FilterValueContainer);
end;

{ TdxExcelFilterValueContainerRangeModeValuesPage }

destructor TdxExcelFilterValueContainerRangeModeValuesPage.Destroy;
begin
  FreeAndNil(FBetweenValues);
  FreeAndNil(FBetweenGroup);
  FreeAndNil(FRangeTrackBar);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.ApplyFilter;
var
  AValueInfos: TdxFastObjectList;
  AInfo: TdxExcelFilterValueContainerValueInfo;
begin
  inherited ApplyFilter;
  AValueInfos := TdxFastObjectList.Create;
  try
    AInfo := TdxExcelFilterValueContainerValueInfo.Create(FromValue, GetDisplayTextByValue(FromValue), foGreaterEqual);
    AValueInfos.Add(AInfo);
    AInfo := TdxExcelFilterValueContainerValueInfo.Create(ToValue, GetDisplayTextByValue(ToValue), foLessEqual);
    AValueInfos.Add(AInfo);
    FilterableComponent.SetFilterActiveValues(AValueInfos, fboAnd);
  finally
    AValueInfos.Free;
  end;
  FilterValueContainer.IsModified := False;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateFromToGroup;
  GenerateFromToValues;
  GenerateRangeTrackBar;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.FilterValuesChanged;
begin
  FilterValueContainer.IsModified := True;
  if FilterValueContainer.NeedImmediateApply then
    ApplyFilter;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.FocusMainItem;
begin
  BetweenValues.FocusMainItem;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.GenerateFromToGroup;
begin
  FBetweenGroup := ParentGroup.CreateGroup;
  BetweenGroup.LayoutDirection := ldHorizontal;
  BetweenGroup.AlignHorz := ahClient;
  BetweenGroup.ShowBorder := False;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.GenerateFromToValues;
begin
  FBetweenValues := TdxExcelFilterValueContainerRangeModeValuesPageBetween.Create(FilterValueContainer, Self);
  BetweenValues.Generate(BetweenGroup);
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.GenerateRangeTrackBar;
begin
  FRangeTrackBar := TdxRangeTrackBar.Create(nil);
  RangeTrackBar.Style.TransparentBorder := False;
  RangeTrackBar.Height := dxRangeTrackBarHeight;
  RangeTrackBar.Transparent := True;
  RangeTrackBar.OnKeyDown := KeyDownHandler;
  RangeTrackBar.OnMouseDown := RangeTrackBarMouseDownHandler;
  RangeTrackBar.OnMouseUp := RangeTrackBarMouseUpHandler;
  RangeTrackBar.GetViewInfo.PositionHintHelper.SetDelay(dxRangeTrackBarHintDelay);
  RangeTrackBar.Properties.ShowPositionHint := True;
  RangeTrackBar.Properties.ThumbStep := cxtsJump;
  RangeTrackBar.Properties.TickMarks := cxtmTopLeft;
  RangeTrackBar.Properties.OnChange := RangeTrackBarValueChangedHandler;
  RangeTrackBar.Properties.OnGetPositionHint := RangeTrackBarGetPositionHintHandler;
  FRangeLayoutItem := ParentGroup.CreateItemForControl(RangeTrackBar);
  RangeLayoutItem.AlignHorz := ahClient;
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetDisplayTextByValue(AValue: Variant): string;
begin
  Result := FilterValueContainer.GetDisplayTextByValue(AValue);
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetRangeTrackBarFrequency: Integer;
var
  ARange: Integer;
begin
  if IsIntegerValueType then
  begin
    ARange := GetRangeTrackBarMax - GetRangeTrackBarMin;
    if ARange > 10 then
      Result := ARange div 10
    else
      Result := 1;
  end
  else
    Result := 10;
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetRangeTrackBarMax: Variant;
begin
  if IsIntegerValueType then
    Result := MaxValue
  else
    if MinValue = MaxValue then
      Result := 0
    else
      Result := 100;
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetRangeTrackBarMin: Variant;
begin
  if IsIntegerValueType then
    Result := MinValue
  else
    Result := 0;
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetRangeTrackBarRangeMax: Integer;
begin
  Result := MapValueToRangeTrackBarValue(ToValue);
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.GetRangeTrackBarRangeMin: Integer;
begin
  Result := MapValueToRangeTrackBarValue(FromValue);
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.IsIntegerValueType: Boolean;
begin
  Result := FilterValueContainer.FilterValueTypeClass.GetVarType in [varSmallint, varInteger, varByte, varShortInt,
    varWord, varLongWord, varInt64];
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Sender = RangeTrackBar) then
    RangeTrackBar.ModifiedAfterEnter := False;
  inherited KeyDownHandler(Sender, Key, Shift);
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.MapRangeTrackBarValueToValue(AValue: Integer): Variant;
var
  AMapValue: Variant;
begin
  if IsIntegerValueType then
    Result := AValue
  else
    if (AValue = GetRangeTrackBarMax) or (GetRangeTrackBarMax = GetRangeTrackBarMin) then
      Result := MaxValue
    else
      if AValue = GetRangeTrackBarMin then
        Result := MinValue
      else
      begin
        AMapValue := (AValue - GetRangeTrackBarMin) * (MaxValue - MinValue) / (GetRangeTrackBarMax - GetRangeTrackBarMin) + MinValue;
        Result := RoundTo(AMapValue, dxRangeTrackBarValueDigit);
      end;
end;

function TdxExcelFilterValueContainerRangeModeValuesPage.MapValueToRangeTrackBarValue(AValue: Variant): Integer;
var
  AMapValue: Variant;
begin
  if IsIntegerValueType then
    Result := AValue
  else
    if (CompareValue(AValue, MaxValue) = 0) or (CompareValue(MaxValue, MinValue) = 0) then
      Result := GetRangeTrackBarMax
    else
      if (CompareValue(AValue, MinValue) = 0) then
        Result := GetRangeTrackBarMin
      else
      begin
        AMapValue := (AValue - MinValue) * (GetRangeTrackBarMax - GetRangeTrackBarMin) /
          (MaxValue - MinValue) + GetRangeTrackBarMin;
        Result := Round(AMapValue);
      end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.SetValues(AFromValue, AToValue: Variant);
begin
  FFromValue := AFromValue;
  FToValue := AToValue;
  FilterValuesChanged;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.TrackValuesChanged;
var
  AFromValue, AToValue: Variant;
begin
  if FTracking then
    Exit;
  AFromValue := MapRangeTrackBarValueToValue(RangeTrackBar.Range.Min);
  AToValue := MapRangeTrackBarValueToValue(RangeTrackBar.Range.Max);
  SetValues(AFromValue, AToValue);
  BetweenValues.UpdateData;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.UpdateData;
begin
  inherited UpdateData;
  UpdateMaxMinValues;
  UpdateFromToValues;
  BetweenValues.UpdateData;
  UpdateRangeTrackBar;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.UpdateFromToValues;

  function IsSingleItem(AItem: TcxFilterCriteriaItem): Boolean;
  var
    I: Integer;
    AParent: TcxFilterCriteriaItemList;
  begin
    Result := True;
    AParent := AItem.Parent;
    for I := 0 to AParent.Count - 1 do
      if not AParent.Items[I].IsItemList and (AParent.Items[I] <> AItem) and
        (TcxFilterCriteriaItem(AParent.Items[I]).ItemLink = FilterValueContainer.FilterItemLink) then
      begin
        Result := False;
        Break;
      end;
  end;

  function IsBetweenCondition(AItem: TcxFilterCriteriaItem; out AValue1, AValue2: Variant): Boolean;
  var
    I: Integer;
    AValueItem: TcxFilterCriteriaItem;
    AParent: TcxFilterCriteriaItemList;
  begin
    Result := False;
    if AItem.Parent.BoolOperatorKind <> fboAnd then
      Exit;
    AValue1 := AItem.Value;
    AValue2 := Null;
    AParent := AItem.Parent;
    for I := 0 to AParent.Count - 1 do
    begin
      if AParent.Items[I].IsItemList then
        Continue;
      AValueItem := TcxFilterCriteriaItem(AParent.Items[I]);
      if (AValueItem = AItem) or (AValueItem.ItemLink <> FilterValueContainer.FilterItemLink) then
        Continue;
      Result := not Result and (AItem.OperatorKind in [foGreater, foGreaterEqual]) and (AValueItem.OperatorKind in [foLess, foLessEqual]) or
        (AItem.OperatorKind in [foLess, foLessEqual]) and (AValueItem.OperatorKind in [foGreater, foGreaterEqual]);
      if Result then
        AValue2 := AValueItem.Value
      else
        Break;
    end;
  end;

var
  AValue1, AValue2: Variant;
  AItem: TcxFilterCriteriaItem;
begin
  FFromValue := MinValue;
  FToValue := MaxValue;
  AItem := FilterValueContainer.Filter.FindItemByItemLink(FilterValueContainer.FilterItemLink);
  if AItem = nil then
    Exit;
  case AItem.OperatorKind of
    foEqual:
      if IsSingleItem(AItem) then
      begin
        FFromValue := AItem.Value;
        FToValue := AItem.Value;
      end;
    foBetween:
      if IsSingleItem(AItem) then
      begin
        FFromValue := AItem.Value[0];
        FToValue := AItem.Value[1];
      end;
    foGreater, foGreaterEqual, foLess, foLessEqual:
      if IsBetweenCondition(AItem, AValue1, AValue2) then
      begin
        FFromValue := Min(AValue1, AValue2);
        FToValue := Max(AValue1, AValue2);
      end;
  end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.UpdateMaxMinValues;
var
  I: Integer;
begin
  FMinValue := 0;
  for I := 0 to FilterValueContainer.Values.Count - 1 do
    if FilterValueContainer.Values[I].Kind = fviValue then
    begin
      FMinValue := FilterValueContainer.Values[I].Value;
      Break;
    end;
  FMaxValue := 0;
  for I := FilterValueContainer.Values.Count - 1 downto 0 do
    if FilterValueContainer.Values[I].Kind = fviValue then
    begin
      FMaxValue := FilterValueContainer.Values[I].Value;
      Break;
    end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.UpdateRangeTrackBar;
begin
  RangeTrackBar.LockChangeEvents(True);
  try
    RangeTrackBar.Properties.Min := GetRangeTrackBarMin;
    RangeTrackBar.Properties.Max := GetRangeTrackBarMax;
    RangeTrackBar.Properties.Frequency := GetRangeTrackBarFrequency;
    RangeTrackBar.Range.Max := GetRangeTrackBarRangeMax;
    RangeTrackBar.Range.Min := GetRangeTrackBarRangeMin;
  finally
    RangeTrackBar.LockChangeEvents(False, False);
  end;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.BeginTracking;
begin
  FTracking := True;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.EndTracking;
begin
  FTracking := False;
  TrackValuesChanged;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.RangeTrackBarGetPositionHintHandler(Sender: TObject;
  const AMinPosition, AMaxPosition: Integer; var AHintText: string; var ACanShow, AIsHintMultiLine: Boolean);
var
  AFromValue, AToValue: Variant;
begin
  AFromValue := MapRangeTrackBarValueToValue(AMinPosition);
  AToValue := MapRangeTrackBarValueToValue(AMaxPosition);
  AHintText := cxGetResourceString(@sdxExcelFilterFromValueText) + ' ' + GetDisplayTextByValue(AFromValue) + ' ' +
    cxGetResourceString(@sdxExcelFilterToValueText) + ' ' + GetDisplayTextByValue(AToValue);
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.RangeTrackBarMouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BeginTracking;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.RangeTrackBarMouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EndTracking;
end;

procedure TdxExcelFilterValueContainerRangeModeValuesPage.RangeTrackBarValueChangedHandler(AValue: TObject);
begin
  TrackValuesChanged;
end;

{ TdxExcelFilterValueContainerFiltersPage }

destructor TdxExcelFilterValueContainerFiltersPage.Destroy;
begin
  FreeAndNil(FConditionContainer);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerFiltersPage.ApplyFilter;
begin
  inherited ApplyFilter;
  ConditionContainer.ApplyFilter;
end;

procedure TdxExcelFilterValueContainerFiltersPage.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GenerateConditionContainer;
end;

procedure TdxExcelFilterValueContainerFiltersPage.FocusMainItem;
begin
  ConditionContainer.FocusMainItem;
end;

procedure TdxExcelFilterValueContainerFiltersPage.GenerateConditionContainer;
begin
  FConditionContainer := TdxExcelFilterValueContainerConditionContainer.Create(FilterValueContainer);
  ConditionContainer.Generate(ParentGroup);
end;

function TdxExcelFilterValueContainerFiltersPage.GetSupportedConditionTypes: TdxExcelFilterValueContainerConditionTypes;
begin
  case FilterValueContainer.GetDataType of
    fdtText:
      Result := [ctEquals..ctIsNotBlank, ctCustomFilter];
    fdtNumeric:
      Result := [ctEquals, ctDoesNotEqual, ctBetween..ctBelowAverage, ctCustomFilter];
    fdtDate:
      Result := [ctSpecificDatePeriods..ctDoesNotEqual, ctBetween, ctBefore..ctCustomFilter];
    fdtTime:
      Result := [ctEquals, ctDoesNotEqual, ctBetween..ctLessEqualThanOrEqualTo, ctCustomFilter];
    else
      Result := [];
  end;
end;

procedure TdxExcelFilterValueContainerFiltersPage.UpdateData;
begin
  inherited UpdateData;
  ConditionContainer.SupportedConditionTypes := GetSupportedConditionTypes;
  ConditionContainer.UpdateData;
end;

{ TdxExcelFilterValueContainerLayout }

destructor TdxExcelFilterValueContainerLayout.Destroy;
begin
  DestroyPages;
  DestroyPageGroups;
  ParentGroup.OnTabChanged := nil;
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainerLayout.ApplyFilter;
var
  APage: TdxExcelFilterValueContainerCustomPage;
begin
  APage := GetActivePage;
  if APage <> nil then
    APage.ApplyFilter;
end;

procedure TdxExcelFilterValueContainerLayout.DestroyPageGroups;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerLayout.DestroyPages;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerLayout.DoGenerate(AParentGroup: TdxLayoutGroup);
begin
  inherited DoGenerate(AParentGroup);
  GeneratePageGroups;
  ParentGroup.ItemIndex := GetDefaultPageIndex;
  GeneratePages;
  ParentGroup.OnTabChanged := TabChangedHandler;
end;

procedure TdxExcelFilterValueContainerLayout.FocusMainItem;
var
  APage: TdxExcelFilterValueContainerCustomPage;
begin
  APage := GetActivePage;
  if APage <> nil then
    APage.FocusMainItem;
end;

procedure TdxExcelFilterValueContainerLayout.GeneratePageGroups;
begin
//do nothing
end;

procedure TdxExcelFilterValueContainerLayout.GeneratePages;
begin
//do nothing
end;

function TdxExcelFilterValueContainerLayout.GetActivePage: TdxExcelFilterValueContainerCustomPage;
begin
  Result := nil;
end;

function TdxExcelFilterValueContainerLayout.GetDefaultPageIndex: Integer;
begin
  Result := ParentGroup.ItemIndex;
end;

procedure TdxExcelFilterValueContainerLayout.TabChanged;
begin
  UpdateData;
end;

procedure TdxExcelFilterValueContainerLayout.UpdateData;
var
  APage: TdxExcelFilterValueContainerCustomPage;
begin
  APage := GetActivePage;
  if APage <> nil then
    APage.UpdateData;
end;

procedure TdxExcelFilterValueContainerLayout.TabChangedHandler(ASender: TObject);
begin
  TabChanged;
end;

{ TdxExcelFilterValueContainerValuesLayout }

function TdxExcelFilterValueContainerValuesLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerCustomValuesPage.Create(FilterValueContainer);
end;

procedure TdxExcelFilterValueContainerValuesLayout.DestroyPageGroups;
begin
  FreeAndNil(FValuesPageGroup);
  inherited DestroyPageGroups;
end;

procedure TdxExcelFilterValueContainerValuesLayout.DestroyPages;
begin
  FreeAndNil(FValuesPage);
  inherited DestroyPages;
end;

procedure TdxExcelFilterValueContainerValuesLayout.GeneratePageGroups;
begin
  inherited GeneratePageGroups;
  GenerateValuesPageGroup;
end;

procedure TdxExcelFilterValueContainerValuesLayout.GeneratePages;
begin
  inherited GeneratePages;
  GenerateValuesPage;
end;

procedure TdxExcelFilterValueContainerValuesLayout.GenerateValuesPage;
begin
  FValuesPage := CreateValuesPage;
  ValuesPage.Generate(ValuesPageGroup);
end;

procedure TdxExcelFilterValueContainerValuesLayout.GenerateValuesPageGroup;
begin
  FValuesPageGroup := ParentGroup.CreateGroup;
  ValuesPageGroup.CaptionOptions.Text := GetValuesPageCaption;
  ValuesPageGroup.AlignHorz := ahClient;
  ValuesPageGroup.AlignVert := avClient;
end;

function TdxExcelFilterValueContainerValuesLayout.GetActivePage: TdxExcelFilterValueContainerCustomPage;
begin
  if ParentGroup.ItemIndex = ValuesPageGroup.Index then
    Result := ValuesPage
  else
    Result := inherited GetActivePage;
end;

function TdxExcelFilterValueContainerValuesLayout.GetDefaultPageIndex: Integer;
begin
  Result := ValuesPageGroup.Index;
end;

function TdxExcelFilterValueContainerValuesLayout.GetValuesPageCaption: string;
begin
  Result := cxGetResourceString(@sdxExcelFilterValuesTabCaption);
end;

{ TdxExcelFilterValueContainerCustomLayout }

function TdxExcelFilterValueContainerFiltersValuesLayout.CreateFiltersPage: TdxExcelFilterValueContainerFiltersPage;
begin
  Result := TdxExcelFilterValueContainerFiltersPage.Create(FilterValueContainer);
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.DestroyPageGroups;
begin
  FreeAndNil(FFiltersPageGroup);
  inherited DestroyPageGroups;
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.DestroyPages;
begin
  FreeAndNil(FFiltersPage);
  inherited DestroyPages;
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.GenerateFiltersPage;
begin
  FFiltersPage := CreateFiltersPage;
  FiltersPage.Generate(FiltersPageGroup);
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.GenerateFiltersPageGroup;
begin
  FFiltersPageGroup := ParentGroup.CreateGroup;
  FiltersPageGroup.CaptionOptions.Text := GetFiltersPageCaption;
  FiltersPageGroup.AlignHorz := ahClient;
  FiltersPageGroup.AlignVert := avClient;
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.GeneratePages;
begin
  inherited GeneratePages;
  GenerateFiltersPage;
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.GeneratePageGroups;
begin
  inherited GeneratePageGroups;
  GenerateFiltersPageGroup;
end;

function TdxExcelFilterValueContainerFiltersValuesLayout.GetActivePage: TdxExcelFilterValueContainerCustomPage;
begin
  if ParentGroup.ItemIndex = FiltersPageGroup.Index then
    Result := FiltersPage
  else
    Result := inherited GetActivePage;
end;

function TdxExcelFilterValueContainerFiltersValuesLayout.GetDefaultPageIndex: Integer;
begin
  if (FilterValueContainer.DefaultPage = dpFilters) or (FilterValueContainer.DefaultPage = dpDefault) and
    (dxDefaultExcelFilterValueContainerDefaultPage = dpFilters) then
    Result := FiltersPageGroup.Index
  else
    Result := inherited GetDefaultPageIndex;
end;

function TdxExcelFilterValueContainerFiltersValuesLayout.GetFiltersPageCaption: string;
begin
  Result := '';
end;

procedure TdxExcelFilterValueContainerFiltersValuesLayout.TabChanged;
begin
  if FilterValueContainer.IsModified then
    if GetActivePage = FiltersPage then
      ValuesPage.ApplyFilter
    else
      FiltersPage.ApplyFilter;
  inherited TabChanged;
end;

{ TdxExcelFilterValueContainerCheckModeLayout }

function TdxExcelFilterValueContainerCheckModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerListBoxModeValuesPage.Create(FilterValueContainer);
end;

{ TdxExcelFilterValueContainerLookupModeLayout }

function TdxExcelFilterValueContainerLookupModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerListBoxModeValuesPage.Create(FilterValueContainer);
end;

{ TdxExcelFilterValueContainerDateTreeViewModeLayout }

function TdxExcelFilterValueContainerDateTreeViewModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerDateTreeViewModeValuesPage.Create(FilterValueContainer);
end;

function TdxExcelFilterValueContainerDateTreeViewModeLayout.GetFiltersPageCaption: string;
begin
  Result := cxGetResourceString(@sdxExcelFilterDateFiltersTabCaption);
end;

{ TdxExcelFilterValueContainerTimeTreeViewModeLayout }

function TdxExcelFilterValueContainerTimeTreeViewModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerTimeTreeViewModeValuesPage.Create(FilterValueContainer);
end;

function TdxExcelFilterValueContainerTimeTreeViewModeLayout.GetFiltersPageCaption: string;
begin
  Result := cxGetResourceString(@sdxExcelFilterTimeFiltersTabCaption);
end;

{ TdxExcelFilterValueContainerListBoxModeLayout }

function TdxExcelFilterValueContainerListBoxModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerListBoxModeValuesPage.Create(FilterValueContainer);
end;

function TdxExcelFilterValueContainerListBoxModeLayout.GetFiltersPageCaption: string;
begin
  case FilterValueContainer.GetDataType of
    fdtDate:
      Result := cxGetResourceString(@sdxExcelFilterDateFiltersTabCaption);
    fdtNumeric:
      Result := cxGetResourceString(@sdxExcelFilterNumericFiltersTabCaption);
    else
      Result := cxGetResourceString(@sdxExcelFilterTextFiltersTabCaption);
  end;
end;

{ TdxExcelFilterValueContainerRangeModeLayout }

function TdxExcelFilterValueContainerRangeModeLayout.CreateValuesPage: TdxExcelFilterValueContainerCustomValuesPage;
begin
  Result := TdxExcelFilterValueContainerRangeModeValuesPage.Create(FilterValueContainer);
end;

function TdxExcelFilterValueContainerRangeModeLayout.GetFiltersPageCaption: string;
begin
  Result := cxGetResourceString(@sdxExcelFilterNumericFiltersTabCaption);
end;

{ TdxExcelFilterValueContainer }

constructor TdxExcelFilterValueContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayoutControl := CreateLayoutControl;
  FLayoutLookAndFeel := CreateLayoutLookAndFeel;

  InitLayoutControl;
  InitLayoutLookAndFeel;
end;

destructor TdxExcelFilterValueContainer.Destroy;
begin
  FreeAndNil(FLayout);
  FreeAndNil(FLayoutLookAndFeel);
  FreeAndNil(FLayoutControl);
  inherited Destroy;
end;

procedure TdxExcelFilterValueContainer.ApplyFilter;
begin
  Layout.ApplyFilter;
  inherited ApplyFilter;
end;

procedure TdxExcelFilterValueContainer.ResetFilter;
begin
  Filter.RemoveItemByItemLink(FilterItemLink);
end;

procedure TdxExcelFilterValueContainer.ApplyModeChanged;
begin
  LayoutChanged;
end;

function TdxExcelFilterValueContainer.CreateLayout: TdxExcelFilterValueContainerValuesLayout;
begin
  case GetType of
    ctRange:
      Result := TdxExcelFilterValueContainerRangeModeLayout.Create(Self);
    ctLookup:
      Result := TdxExcelFilterValueContainerLookupModeLayout.Create(Self);
    ctCheck:
      Result := TdxExcelFilterValueContainerCheckModeLayout.Create(Self);
    ctDateTreeView:
      Result := TdxExcelFilterValueContainerDateTreeViewModeLayout.Create(Self);
    ctTimeTreeView:
      Result := TdxExcelFilterValueContainerTimeTreeViewModeLayout.Create(Self);
    else
      Result := TdxExcelFilterValueContainerListBoxModeLayout.Create(Self);
  end;
end;

function TdxExcelFilterValueContainer.CreateLayoutControl: TdxLayoutControl;
begin
  Result := TdxLayoutControl.Create(Self);
end;

function TdxExcelFilterValueContainer.CreateLayoutLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := TdxLayoutCxLookAndFeel.Create(nil);
end;

procedure TdxExcelFilterValueContainer.DoUpdateFiltering;
begin
  FreeAndNil(FNonUniqueValues);
  FreeAndNil(FNonUniqueDisplayValues);
  FFilterItemLink := nil;
  FFilterProperties := nil;
  FFilterValueTypeClass := nil;
  FFilterHelper := nil;
  FreeAndNil(FLayout);
  if FilterableComponent <> nil then
  begin
    FFilterItemLink := FilterableComponent.GetFilterItemLink;
    FFilterProperties := FilterableComponent.GetFilterProperties;
    FFilterValueTypeClass := FilterableComponent.GetFilterValueTypeClass;
    FFilterHelper := FilterEditsController.FindHelper(FilterProperties.ClassType);
    FLayout := CreateLayout;
    Layout.Generate(LayoutContainer.Root);
  end;
  inherited DoUpdateFiltering;
end;

procedure TdxExcelFilterValueContainer.DoUpdateLayout;
begin
  UpdateLayoutControlPosition;
end;

procedure TdxExcelFilterValueContainer.FocusLayoutMainItem;
begin
  Layout.FocusMainItem;
end;

function TdxExcelFilterValueContainer.GetDataType: TcxFilterDataType;
begin
  Result := FilterHelper.GetFilterDataType(FilterValueTypeClass);
end;

function TdxExcelFilterValueContainer.GetDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
begin
  if (DateTimeValuesPageType = dvptList) or (DateTimeValuesPageType = dvptDefault) and
    (dxDefaultExcelFilterValueContainerDateTimeValuesPageType = dvptList) then
    Result := dvptList
  else
    Result := dvptTree;
end;

function TdxExcelFilterValueContainer.GetDefaultBounds: TRect;
begin
  Result := cxRectSetSize(cxEmptyRect, dxExcelFilterValueContainerDefaultWidth,
    dxExcelFilterValueContainerDefaultHeight);
end;

function TdxExcelFilterValueContainer.GetDisplayTextByValue(AValue: Variant): string;
begin
  Result := FilterProperties.GetDisplayText(AValue);
end;

function TdxExcelFilterValueContainer.GetListBoxClass: TdxExcelFilterValueContainerListBoxClass;
begin
  Result := TdxExcelFilterValueContainerListBox;
end;

function TdxExcelFilterValueContainer.GetNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;
begin
  if (NumericValuesPageType = nvptList) or (NumericValuesPageType = nvptDefault) and
    (dxDefaultExcelFilterValueContainerNumericValuesPageType = nvptList) then
    Result := nvptList
  else
    Result := nvptRange;
end;

function TdxExcelFilterValueContainer.GetType: TdxExcelFilterValueContainerType;
begin
  case GetDataType of
    fdtNumeric:
      if GetNumericValuesPageType = nvptList then
        Result := ctListBox
      else
        Result := ctRange;
    fdtDate:
      if GetDateTimeValuesPageType = dvptList then
        Result := ctListBox
      else
        Result := ctDateTreeView;
    fdtTime:
      if GetDateTimeValuesPageType = dvptList then
        Result := ctListBox
      else
        Result := ctTimeTreeView;
    fdtLookup:
      Result := ctLookup;
    fdtCheck:
      Result := ctCheck;
    else
      Result := ctListBox;
  end;
end;

procedure TdxExcelFilterValueContainer.InitLayoutControl;
begin
  LayoutContainer.Root.AlignHorz := ahClient;
  LayoutContainer.Root.AlignVert := avClient;
  LayoutContainer.Root.LayoutDirection := ldTabbed;
  LayoutContainer.LayoutLookAndFeel := LayoutLookAndFeel;
  LayoutControl.Parent := Self;
  LayoutControl.OnKeyDown := LayoutControlKeyDownHandler;
end;

procedure TdxExcelFilterValueContainer.InitLayoutLookAndFeel;
begin
  LayoutLookAndFeel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  LayoutLookAndFeel.Offsets.RootItemsAreaOffsetHorz := 2;
  LayoutLookAndFeel.Offsets.RootItemsAreaOffsetVert := 2;
end;

function TdxExcelFilterValueContainer.IsLinkComponentSupported(AValue: TComponent): Boolean;
begin
  Result := inherited IsLinkComponentSupported(AValue) and Supports(AValue, IdxExcelFilterableComponent);
end;

procedure TdxExcelFilterValueContainer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  LayoutKeyDown(Self, Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TdxExcelFilterValueContainer.LayoutKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
begin
//do nothing
end;

function TdxExcelFilterValueContainer.NeedImmediateApply: Boolean;
begin
  Result := (ApplyChanges = efacImmediately) or (ApplyChanges = efacDefault) and
    (dxDefaultExcelFilterValueContainerApplyChanges in [efacDefault, efacImmediately]);
end;

procedure TdxExcelFilterValueContainer.NonUniqueValuesNeeded;
begin
  if FNonUniqueValues = nil then
  begin
    FNonUniqueValues := CreateValues;
    FNonUniqueDisplayValues := CreateDisplayValues;
    PopulateNonUniqueValues;
  end;
end;

procedure TdxExcelFilterValueContainer.PopulateNonUniqueValues;
begin
  FilterableComponent.PopulateFilterValues(FNonUniqueValues, FNonUniqueDisplayValues, True, False);
end;

procedure TdxExcelFilterValueContainer.PopulateValues;
var
  I: Integer;
begin
  inherited PopulateValues;
  for I := Values.Count - 1 downto 0 do
    if not (Values[I].Kind in [fviAll, fviBlanks, fviValue]) then
    begin
      Values.Delete(I);
      DisplayValues.Delete(I);
    end;
end;

procedure TdxExcelFilterValueContainer.UpdateLayoutControlPosition;
begin
  LayoutControl.BoundsRect := ClientBounds;
end;

procedure TdxExcelFilterValueContainer.UpdateValues;
begin
  if Layout <> nil then
    Layout.UpdateData;
  inherited UpdateValues;
end;

procedure TdxExcelFilterValueContainer.SetApplyChanges(AValue: TdxExcelFilterValueContainerApplyChangesMode);
begin
  if AValue <> ApplyChanges then
  begin
    FApplyChanges := AValue;
    ApplyModeChanged;
  end;
end;

function TdxExcelFilterValueContainer.GetFilterHelper: TcxCustomFilterEditHelperClass;
begin
  Result := FFilterHelper;
  if Result = nil then
    Result := TcxFilterTextEditHelper;
end;

function TdxExcelFilterValueContainer.GetFilterableComponent: IdxExcelFilterableComponent;
begin
  Result := inherited FilterableComponent as IdxExcelFilterableComponent;
end;

function TdxExcelFilterValueContainer.GetLayoutContainer: TdxLayoutContainer;
begin
  Result := LayoutControl.Container;
end;

function TdxExcelFilterValueContainer.GetNonUniqueDisplayValues: TStringList;
begin
  NonUniqueValuesNeeded;
  Result := FNonUniqueDisplayValues;
end;

function TdxExcelFilterValueContainer.GetNonUniqueValues: TcxFilterValueList;
begin
  NonUniqueValuesNeeded;
  Result := FNonUniqueValues;
end;

procedure TdxExcelFilterValueContainer.SetDateTimeValuesPageType(const AValue: TdxExcelFilterValueContainerDateTimeValuesPageType);
begin
  if AValue <> DateTimeValuesPageType then
  begin
    FDateTimeValuesPageType := AValue;
    LayoutChanged;
  end;
end;

procedure TdxExcelFilterValueContainer.SetDefaultPage(AValue: TdxExcelFilterValueContainerDefaultPage);
begin
  if AValue <> DefaultPage then
  begin
    FDefaultPage := AValue;
    LayoutChanged;
  end;
end;

procedure TdxExcelFilterValueContainer.SetNumericValuesPageType(const AValue: TdxExcelFilterValueContainerNumericValuesPageType);
begin
  if AValue <> NumericValuesPageType then
  begin
    FNumericValuesPageType := AValue;
    LayoutChanged;
  end;
end;

procedure TdxExcelFilterValueContainer.LayoutControlKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  LayoutKeyDown(Sender, Key, Shift);
end;

end.
