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

unit cxGridTableView;

{$I cxVer.inc}

interface

{$UNDEF DXLOGGING}

uses
  Types, Variants, Windows, Messages, Classes, Graphics, Controls, ImgList, Forms,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, cxClasses, cxControls, cxGraphics, dxTypeHelpers,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxStorage, cxPC, cxListBox,
  cxContainer, cxEdit, cxTextEdit, cxGrid, cxGridCommon, cxGridLevel, cxFilterControl,
  cxGridCustomView, cxGridCustomTableView, cxGridDetailsSite, cxCustomData, cxImageComboBox, dxFilterPopupWindow,
  cxData, cxDataStorage, cxFilter, Menus, dxCoreClasses, cxGridInplaceEditForm, dxUIElementPopupWindow,
  dxLayoutContainer, dxLayoutLookAndFeels, dxCore, cxGridViewLayoutContainer, cxGeometry, dxGDIPlusClasses, dxFilterValueContainer;

const
  htGridBase = 200;
  htGroupByBox = htGridBase + 1;
  htColumnHeader = htGridBase + 2;
  htColumnHeaderHorzSizingEdge = htGridBase + 3;
  htColumnHeaderFilterButton = htGridBase + 4;
  htFooter = htGridBase + 5;
  htFooterCell = htGridBase + 6;
  htGroupFooter = htGridBase + 7;
  htGroupFooterCell = htGridBase + 8;
  htRowIndicator = htGridBase + 9;
  htRowSizingEdge = htGridBase + 10;
  htIndicator = htGridBase + 11;
  htIndicatorHeader = htGridBase + 12;
  htRowLevelIndent = htGridBase + 13;
  htHeader = htGridBase + 14;
  htGroupSummary = htGridBase + 15;

  ckHeader = 2;
  ckGroupByBox = 3;
  ckFooter = 4;

  cxGridDefaultIndicatorWidth = 12;

  cxGridCustomRowSeparatorDefaultWidth = 6;
  cxGridCustomRowSeparatorMinWidth = 2;

  cxGridDefaultMergedGroupSeparator = '; ';

  cxGridPreviewDefaultLeftIndent = 20;
  cxGridPreviewDefaultMaxLineCount = 3;
  cxGridPreviewDefaultRightIndent = 5;

  cxGridHeaderSizingEdgeSize = 8;
  cxGridRowSizingEdgeSize = 8;

  cxGridOffice11GroupRowSeparatorWidth: Integer = 2;

  // record kind
  rkFiltering = 2;

  isColumnFirst = isCustomItemLast + 1;
  isFooter = isColumnFirst;
  isGroupSummary = isColumnFirst + 1;
  isHeader = isColumnFirst + 2;
  isColumnLast = isHeader;

  bbTableFirst = bbCustomTableLast + 1;
  bbFooter = bbTableFirst;
  bbHeader = bbTableFirst + 1;
  bbGroup = bbTableFirst + 2;
  bbGroupByBox = bbTableFirst + 3;
  bbIndicator = bbTableFirst + 4;
  bbPreview = bbTableFirst + 5;
  bbTableLast = bbPreview;

  vsTableFirst = vsCustomTableLast + 1;
  vsFilterRowInfoText = vsTableFirst;
  vsFooter = vsTableFirst + 1;
  vsGroup = vsTableFirst + 2;
  vsGroupByBox = vsTableFirst + 3;
  vsGroupFooterSortedSummary = vsTableFirst + 4;
  vsGroupSortedSummary = vsTableFirst + 5;
  vsGroupSummary = vsTableFirst + 6;
  vsHeader = vsTableFirst + 7;
  vsNewItemRowInfoText = vsTableFirst + 8;
  vsIndicator = vsTableFirst + 9;
  vsPreview = vsTableFirst + 10;

  vsInplaceEditFormGroup = vsTableFirst + 11;
  vsInplaceEditFormItem = vsTableFirst + 12;
  vsInplaceEditFormItemHotTrack = vsTableFirst + 13;

  vsTableLast = vsInplaceEditFormItemHotTrack;

  cxGridFilterRowDelayDefault = 1000;

type
  TcxGridTableViewInplaceEditForm = class;
  TcxGridTableCustomizationForm = class;
  TcxGridTableController = class;
  TcxCustomGridRow = class;
  TcxGridDataRow = class;
  TcxGridMasterDataRow = class;
  TcxGridGroupRow = class;
  TcxGridViewData = class;
  TcxGridColumnHeaderAreaPainterClass = class of TcxGridColumnHeaderAreaPainter;
  TcxGridColumnContainerViewInfo = class;
  TcxGridColumnHeaderAreaViewInfoClass = class of TcxGridColumnHeaderAreaViewInfo;
  TcxGridColumnHeaderAreaViewInfo = class;
  TcxGridColumnHeaderFilterButtonViewInfo = class;
  TcxGridColumnHeaderGlyphViewInfo = class;
  TcxGridColumnHeaderCheckBoxAreaViewInfo = class;
  TcxGridColumnHeaderViewInfoClass = class of TcxGridColumnHeaderViewInfo;
  TcxGridColumnHeaderViewInfo = class;
  TcxGridHeaderViewInfo = class;
  TcxGridGroupByBoxViewInfo = class;
  TcxGridFooterViewInfo = class;
  TcxCustomGridIndicatorItemViewInfo = class;
  TcxGridIndicatorHeaderItemViewInfo = class;
  TcxGridIndicatorRowItemViewInfo = class;
  TcxGridIndicatorFooterItemViewInfo = class;
  TcxGridIndicatorViewInfo = class;
  TcxGridRowFooterViewInfo = class;
  TcxGridRowFootersViewInfo = class;
  TcxCustomGridRowViewInfo = class;
  TcxGridFixedDataRowsViewInfo = class;
  TcxGridRowsViewInfo = class;
  TcxGridTableViewInfo = class;
  TcxGridTableViewInfoCacheItem = class;
  TcxGridColumn = class;
  TcxGridTableView = class;
  TcxCustomGridColumn = class;

  TcxGridColumnContainerKind = Integer;

  { hit tests }

  // custom column

  TcxCustomGridColumnHitTest = class(TcxCustomGridViewHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
  public
    Column: TcxGridColumn;
    ColumnContainerKind: TcxGridColumnContainerKind;
  end;

  // group by box

  TcxGridGroupByBoxHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // column header

  TcxGridColumnHeaderHitTest = class(TcxCustomGridColumnHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridColumnHeaderHorzSizingEdgeHitTest = class(TcxCustomGridColumnHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridColumnHeaderFilterButtonHitTest = class(TcxCustomGridColumnHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // header

  TcxGridHeaderHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // footer

  TcxGridFooterHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridFooterCellHitTest = class(TcxCustomGridColumnHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    SummaryItem: TcxDataSummaryItem;
  end;

  TcxGridGroupFooterHitTest = class(TcxGridFooterHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridGroupFooterCellHitTest = class(TcxGridFooterCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // indicator

  TcxGridRowIndicatorHitTest = class(TcxGridRecordHitTest)
  private
    function GetGridView: TcxGridTableView;
    procedure SetGridView(AValue: TcxGridTableView);
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
    function UseSelectRowCursor: Boolean; virtual;
  public
    MultiSelect: Boolean;

    function Cursor: TCursor; override;
    property GridView: TcxGridTableView read GetGridView write SetGridView;
  end;

  TcxGridRowSizingEdgeHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridIndicatorHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridIndicatorHeaderHitTest = class(TcxGridIndicatorHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  // row

  TcxGridRowLevelIndentHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridGroupSummaryHitTest = class(TcxGridRecordHitTest)
  private
    function GetColumn: TcxGridColumn;
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    SummaryItem: TcxDataSummaryItem;
    property Column: TcxGridColumn read GetColumn;
  end;

  // data row fixing menu

  TcxGridDataRowFixingMenu = class(TdxCustomDropDownListBox)
  private
    FDataRow: TcxGridDataRow;
  protected
    procedure AddOperation(AFixedState: TcxDataControllerRowFixedState);
    procedure DoSelectItem(AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean); override;
    function GetOperationImageIndex(AFixedState: TcxDataControllerRowFixedState): Integer;
    function GetOperationText(AFixedState: TcxDataControllerRowFixedState): string;
    procedure RecreateOperations;

    property DataRow: TcxGridDataRow read FDataRow;
  public
    procedure Popup(ADataRow: TcxGridDataRow; AForBounds: TRect); reintroduce; virtual;
  end;

  { inplace edit form }

  TcxGridTableViewInplaceEditFormDataCellViewInfo = class(TcxGridInplaceEditFormDataCellViewInfo)
  private
    function GetGridView: TcxGridTableView; inline;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetItem: TcxGridColumn;
  protected
    function CanFocus: Boolean; override;
    procedure GetCaptionParams(var AParams: TcxViewParams); override;
    function InvalidateOnStateChange: Boolean; override;
  public
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property Item: TcxGridColumn read GetItem;
  end;

  TcxGridTableViewInplaceEditFormContainerViewInfo = class(TcxGridInplaceEditFormContainerViewInfo)
  protected
    function FindGridItemViewInfo(AViewInfo: TcxGridCustomLayoutItemViewInfo): TcxGridTableDataCellViewInfo; override;
  end;

  TcxGridTableViewInplaceEditFormContainer = class(TcxGridInplaceEditFormContainer)
  private
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
    function GetGridView: TcxGridTableView;
    function GetViewInfo: TcxGridTableViewInplaceEditFormContainerViewInfo;
  protected
    function CanCreateLayoutItemForGridItem(AItem: TcxCustomGridTableItem): Boolean; override;
    function GetClientBounds: TRect; override;
    function GetClientRect: TRect; override;
    function GetViewInfoClass: TdxLayoutContainerViewInfoClass; override;
    function IsItemVisibleForEditForm(AItem: TcxGridInplaceEditFormLayoutItem): Boolean; override;
  public
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
    property GridView: TcxGridTableView read GetGridView;
    property ViewInfo: TcxGridTableViewInplaceEditFormContainerViewInfo read GetViewInfo;
  end;

  TcxGridTableViewInplaceEditForm = class(TcxGridInplaceEditForm)
  private
    function GetContainer: TcxGridTableViewInplaceEditFormContainer;
    function GetGridView: TcxGridTableView;
  protected
    function CanShowCustomizationForm: Boolean; override;
    procedure Changed(AHardUpdate: Boolean = False); override;
    function GetContainerClass: TcxGridInplaceEditFormContainerClass; override;
    function IsAssigningOptions: Boolean; override;
    function GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel; override;
    function GetVisible: Boolean; override;
    procedure PopulateTabOrderList(AList: TList);
    procedure ResetEditingRecordIndex; override;
  public
    procedure CheckFocusedItem(AItemViewInfo: TcxGridTableViewInplaceEditFormDataCellViewInfo);
    function IsInplaceEditFormMode: Boolean; override;
    procedure ValidateEditVisibility;

    property Container: TcxGridTableViewInplaceEditFormContainer read GetContainer;
    property GridView: TcxGridTableView read GetGridView;
  end;

  TcxGridTableViewInplaceEditFormClass = class of TcxGridTableViewInplaceEditForm;

  TcxGridEditFormOptions = class(TcxCustomGridOptions)
  private
    FIsAssigning: Boolean;

    function GetDefaultColumnCount: Integer;
    function GetDefaultStretch: TcxGridInplaceEditFormStretch;
    function GetGridView: TcxGridTableView;
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
    function GetItemHotTrack: Boolean;
    function GetMasterRowDblClickAction: TcxGridMasterRowDblClickAction;
    function GetUseDefaultLayout: Boolean;
    procedure SetDefaultColumnCount(AValue: Integer);
    procedure SetDefaultStretch(AValue: TcxGridInplaceEditFormStretch);
    procedure SetItemHotTrack(AValue: Boolean);
    procedure SetMasterRowDblClickAction(AValue: TcxGridMasterRowDblClickAction);
    procedure SetUseDefaultLayout(AValue: Boolean);
  protected
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
    property IsAssigning: Boolean read FIsAssigning;
  public
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridTableView read GetGridView;
  published
    property DefaultColumnCount: Integer read GetDefaultColumnCount write SetDefaultColumnCount default cxGridInplaceEditFormDefaultColumnCount;
    property ItemHotTrack: Boolean read GetItemHotTrack write SetItemHotTrack default False;
    property MasterRowDblClickAction: TcxGridMasterRowDblClickAction read GetMasterRowDblClickAction
      write SetMasterRowDblClickAction default cxGridInplaceEditFormDefaultMasterRowDblClickAction;
    property DefaultStretch: TcxGridInplaceEditFormStretch read GetDefaultStretch write SetDefaultStretch default fsNone;
    property UseDefaultLayout: Boolean read GetUseDefaultLayout write SetUseDefaultLayout default True;
  end;

  { view data }

  TcxCustomGridRowClass = class of TcxCustomGridRow;

  TcxCustomGridRow = class(TcxCustomGridRecord)
  private
    function GetAsGroupRow: TcxGridGroupRow;
    function GetAsMasterDataRow: TcxGridMasterDataRow;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    function GetGridViewLevel: TcxGridLevel;
    function GetIsFilterRow: Boolean;
    function GetIsNewItemRow: Boolean;
    function GetViewData: TcxGridViewData;
    function GetViewInfo: TcxCustomGridRowViewInfo;
    function HasParentGroup: Boolean;
  protected
    function GetCheckBoxState: TcxCheckBoxState; virtual;
    function GetFixedState: TcxDataControllerRowFixedState; virtual;
    function GetIndicatorKind: TcxIndicatorKind; virtual;
    function GetIsParentRecordLast(AIndex: Integer): Boolean; override;
    function GetTopGroupIndex(ALevel: Integer = 0): Integer; virtual;
    function IsSpecial: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function ShowCheckBox: Boolean; virtual;
    procedure SetFixedState(AValue: TcxDataControllerRowFixedState); virtual;
    procedure ToggleCheckBox; virtual;
    procedure ToggleFixedState(AMenuForBounds: TRect); virtual; abstract;

    property CheckBoxState: TcxCheckBoxState read GetCheckBoxState;
    property Controller: TcxGridTableController read GetController;
    property FixedState: TcxDataControllerRowFixedState read GetFixedState write SetFixedState;
    property IndicatorKind: TcxIndicatorKind read GetIndicatorKind;
    property TopGroupIndex[ALevel: Integer]: Integer read GetTopGroupIndex;
  public
    function ExpandOnDblClick: Boolean; virtual;
    function SupportsCellMultiSelect: Boolean; virtual;

    property AsGroupRow: TcxGridGroupRow read GetAsGroupRow;
    property AsMasterDataRow: TcxGridMasterDataRow read GetAsMasterDataRow;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewLevel: TcxGridLevel read GetGridViewLevel;
    property IsFilterRow: Boolean read GetIsFilterRow;
    property IsNewItemRow: Boolean read GetIsNewItemRow;
    property ViewData: TcxGridViewData read GetViewData;
    property ViewInfo: TcxCustomGridRowViewInfo read GetViewInfo;
  end;

  TcxGridDataRow = class(TcxCustomGridRow)
  private
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
  protected
    //inplace edit form
    function GetEditFormVisible: Boolean; virtual;
    function GetInplaceEditFormClientBounds: TRect; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetEditFormVisible(AValue: Boolean); virtual;
    function ShowCheckBox: Boolean; override;

    function GetFixedState: TcxDataControllerRowFixedState; override;
    function GetExpandable: Boolean; override;
    function GetHasCells: Boolean; override;
    function GetIndicatorKind: TcxIndicatorKind; override;
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    procedure SetFixedState(AValue: TcxDataControllerRowFixedState); override;
    procedure ToggleFixedState(AMenuForBounds: TRect); override;

    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
  public
    function ExpandOnDblClick: Boolean; override;
    function SupportsCellMultiSelect: Boolean; override;
    procedure ToggleEditFormVisibility; virtual;

    property EditFormVisible: Boolean read GetEditFormVisible write SetEditFormVisible;
    property FixedState;
  end;

  TcxGridNewItemRowClass = class of TcxGridNewItemRow;

  TcxGridNewItemRow = class(TcxGridDataRow)
  protected
    function GetIndicatorKind: TcxIndicatorKind; override;
    function IsSpecial: Boolean; override;
    procedure SetEditFormVisible(AValue: Boolean); override;
  public
    function SupportsCellMultiSelect: Boolean; override;
  end;

  TcxGridFilterRowClass = class of TcxGridFilterRow;

  TcxGridFilterRow = class(TcxGridNewItemRow)
  private
    FSelected: Boolean;

    procedure ActualizeProperties(AProperties: TcxCustomEditProperties);
    function GetFilterCriteriaItem(Index: Integer): TcxFilterCriteriaItem;
    function GetOperator(Index: Integer): TcxFilterOperatorKind; overload;
    function GetOperator(Index: Integer; AValue: Variant): TcxFilterOperatorKind; overload;
    procedure SetOperator(Index: Integer; AOperator: TcxFilterOperatorKind);
  protected
    function GetIndicatorKind: TcxIndicatorKind; override;
    procedure RefreshRecordInfo; override;

    function GetSelected: Boolean; override;
    function GetVisible: Boolean; override;
    procedure SetSelected(Value: Boolean); override;

    function GetDisplayText(Index: Integer): string; override;
    function GetValue(Index: Integer): Variant; override;
    procedure SetDisplayText(Index: Integer; const Value: string); override;
    procedure SetValue(Index: Integer; const Value: Variant); override;

    function GetDisplayTextForValue(AIndex: Integer; const AValue: Variant): string; virtual;
    function GetDefaultFilterOperatorKind(const AValue: Variant; ACheckMask: Boolean): TcxFilterOperatorKind; virtual;
    function IsCriteriaItemSupported(AIndex: Integer; ACriteriaItem: TcxFilterCriteriaItem): Boolean; virtual;
    procedure ResetOperators; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property FilterCriteriaItems[Index: Integer]: TcxFilterCriteriaItem read GetFilterCriteriaItem;
    property InternalSelected: Boolean read FSelected write FSelected;
  public
    destructor Destroy; override;
    function CanFocusCells: Boolean; override;
    function IsEmpty: Boolean;

    property Operators[Index: Integer]: TcxFilterOperatorKind read GetOperator write SetOperator;
  end;

  TcxGridMasterDataRow = class(TcxGridDataRow)
  private
    function GetActiveDetailGridView: TcxCustomGridView;
    function GetActiveDetailGridViewExists: Boolean;
    function GetActiveDetailIndex: Integer;
    function GetActiveDetailLevel: TcxGridLevel;
    function GetDetailGridView(Index: Integer): TcxCustomGridView;
    function GetDetailGridViewCount: Integer;
    function GetDetailGridViewExists(Index: Integer): Boolean;
    function GetDetailGridViewHasData(Index: Integer): Boolean;
    function GetInternalActiveDetailGridView: TcxCustomGridView;
    function GetInternalActiveDetailGridViewExists: Boolean;
    function GetInternalActiveDetailIndex: Integer;
    procedure SetActiveDetailIndex(Value: Integer);
    procedure SetActiveDetailLevel(Value: TcxGridLevel);
  protected
    procedure DoCollapse(ARecurse: Boolean); override;
    procedure DoExpand(ARecurse: Boolean); override;
    function GetExpandable: Boolean; override;
    function GetExpanded: Boolean; override;
    function GetHasChildren: Boolean; virtual;
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ToggleExpanded; override;
    property InternalActiveDetailGridView: TcxCustomGridView read GetInternalActiveDetailGridView;
    property InternalActiveDetailGridViewExists: Boolean read GetInternalActiveDetailGridViewExists;
    property InternalActiveDetailIndex: Integer read GetInternalActiveDetailIndex;
  public
    function ExpandOnDblClick: Boolean; override;
    function GetFirstFocusableChild: TcxCustomGridRecord; override;
    function GetLastFocusableChild(ARecursive: Boolean): TcxCustomGridRecord; override;

    property ActiveDetailGridView: TcxCustomGridView read GetActiveDetailGridView;
    property ActiveDetailGridViewExists: Boolean read GetActiveDetailGridViewExists;
    property ActiveDetailIndex: Integer read GetActiveDetailIndex write SetActiveDetailIndex;
    property ActiveDetailLevel: TcxGridLevel read GetActiveDetailLevel write SetActiveDetailLevel;
    property DetailGridViewCount: Integer read GetDetailGridViewCount;
    property DetailGridViewExists[Index: Integer]: Boolean read GetDetailGridViewExists;
    property DetailGridViewHasData[Index: Integer]: Boolean read GetDetailGridViewHasData;
    property DetailGridViews[Index: Integer]: TcxCustomGridView read GetDetailGridView;
    property HasChildren: Boolean read GetHasChildren;
  end;

  TcxGridGroupRow = class(TcxCustomGridRow)
  private
    function GetDisplayCaptionByGroupedColumn(AIndex: Integer): string;
    function GetDisplayTextByGroupedColumn(AGroupedColumnIndex: Integer): string;
    function GetDisplayTextValueByGroupedColumn(AIndex: Integer): string;
    function GetMainGroupedColumn: TcxGridColumn;
    function GetGroupedColumn(AGroupedColumnIndex: Integer): TcxGridColumn;
    function GetGroupedColumnCount: Integer;
    function GetGroupedColumnValue(AGroupedColumnIndex: Integer): Variant;
    function GetGroupSummaryItems: TcxDataGroupSummaryItems;
  protected
    procedure DoCollapse(ARecurse: Boolean); override;
    procedure DoExpand(ARecurse: Boolean); override;
    //function GetDestroyingOnExpanding: Boolean; override;
    function GetExpandable: Boolean; override;
    function GetExpanded: Boolean; override;

    function GetCheckBoxState: TcxCheckBoxState; override;
    function GetDisplayCaption: string; virtual;
    function GetDisplayText(Index: Integer): string; override;
    function GetDisplayTextValue: string; virtual;
    function GetFixedState: TcxDataControllerRowFixedState; override;
    function GetIsData: Boolean; override;
    function GetIsParent: Boolean; override;
    function GetTopGroupIndex(ALevel: Integer = 0): Integer; override;
    function GetValue: Variant; reintroduce; virtual;
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;
    procedure SetDisplayText(Index: Integer; const Value: string); override;
    procedure SetValue(Index: Integer; const Value: Variant); override;
    function ShowCheckBox: Boolean; override;
  public
    function GetGroupSummaryInfo(var ASummaryItems: TcxDataSummaryItems;
      var ASummaryValues: PVariant; AGroupedColumnIndex: Integer = 0): Boolean;

    property DisplayCaption: string read GetDisplayCaption;
    property DisplayCaptions[AGroupedColumnIndex: Integer]: string read GetDisplayCaptionByGroupedColumn;
    property DisplayText: string read GetDisplayTextValue;
    property GroupedColumn: TcxGridColumn read GetMainGroupedColumn;
    property GroupedColumns[AGroupedColumnIndex: Integer]: TcxGridColumn read GetGroupedColumn;
    property GroupedColumnCount: Integer read GetGroupedColumnCount;
    property GroupedColumnValue[AGroupedColumnIndex: Integer]: Variant read GetGroupedColumnValue;
    property GroupSummaryItems: TcxDataGroupSummaryItems read GetGroupSummaryItems;
    property Value: Variant read GetValue;
  end;

  TcxGridViewData = class(TcxCustomGridTableViewData)
  private
    FFilterRow: TcxGridFilterRow;

    function GetFixedBottomRowCount: Integer;
    function GetFixedTopRowCount: Integer;
    function GetNewItemRow: TcxGridNewItemRow;
    function GetRow(Index: Integer): TcxCustomGridRow;
    function GetRowCount: Integer;
  protected
    function GetFirstVisibleExpandedMasterRow: TcxGridMasterDataRow; virtual;
    function GetNewItemRecordClass: TcxCustomGridRecordClass; override;
    function GetRecordByKind(AKind, AIndex: Integer): TcxCustomGridRecord; override;
    function GetRecordKind(ARecord: TcxCustomGridRecord): Integer; override;
    function GetDataRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; virtual;
    function GetGroupRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; virtual;
    function GetMasterRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; virtual;
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
    function GetTopGroup(ARowIndex: Integer; ALevel: Integer = 0): TcxCustomGridRow; virtual;
    function GetTopGroupIndex(ARowIndex: Integer; ALevel: Integer = 0): Integer; virtual;

    procedure CreateFilterRow;
    procedure DestroyFilterRow;
    procedure CheckFilterRow;
    //procedure RecreateFilterRow;
    function GetFilterRowClass: TcxGridFilterRowClass; virtual;
  public
    destructor Destroy; override;
    procedure Collapse(ARecurse: Boolean); override;
    procedure Expand(ARecurse: Boolean); override;
    function HasFilterRow: Boolean; virtual;
    function HasNewItemRecord: Boolean; override;
    function MakeDetailVisible(ADetailLevel: TComponent{TcxGridLevel}): TcxCustomGridView; override;
    //procedure Refresh(ARecordCount: Integer); override;

    property FilterRow: TcxGridFilterRow read FFilterRow;
    property FixedBottomRowCount: Integer read GetFixedBottomRowCount;
    property FixedTopRowCount: Integer read GetFixedTopRowCount;
    property NewItemRow: TcxGridNewItemRow read GetNewItemRow;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TcxCustomGridRow read GetRow;
  end;

  { controller }

  // drag & drop objects

  TcxGridColumnHeaderMergeIndicatorClass = class of TcxGridColumnHeaderMergeIndicator;
  TcxGridColumnHeaderMergeIndicator = class(TcxDragImage)
  strict private
    FScaleFactor: TdxScaleFactor;
    FTransparent: Boolean;

    function GetTransparent: Boolean;
  protected
    function CalculateBounds(const APosition: TPoint): TRect; virtual;
    procedure Draw; virtual;
    procedure DrawBackground; virtual;
    procedure DrawIndicator; virtual;
    function GetHeight: Integer; virtual;
    function GetBackColor: TColor; virtual;
    function GetIndicatorPoints(const ABounds: TRect): TPoints; virtual;
    function GetWidth: Integer; virtual;
    procedure Init(const APosition: TPoint); overload;

    property BackColor: TColor read GetBackColor;
    property Transparent: Boolean read GetTransparent;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(ATransparent: Boolean); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TcxGridColumnHeaderMovingObjectClass = class of TcxGridColumnHeaderMovingObject;

  TcxGridColumnHeaderMovingObject = class(TcxCustomGridTableItemMovingObject)
  private
    FMergeIndicator: TcxGridColumnHeaderMergeIndicator;
    FOriginalDestColumnContainerKind: TcxGridColumnContainerKind;
    FUnmergeableColumns: TList;

    function AllowMergeWithLeftColumn: Boolean;
    function AllowMergeWithRightColumn: Boolean;
    function ColumnGrouping: Boolean;
    function ColumnMovingToHeader: Boolean;
    function ColumnRemoving: Boolean;
    function GetGridView: TcxGridTableView;
    function GetGroupByIndex: Integer;
    function GetLeftGroupColumn: TcxGridColumn;
    function GetRightGroupColumn: TcxGridColumn;
    function GetSourceItem: TcxGridColumn;
    function GetViewInfo: TcxGridTableViewInfo;
    function MergeWithLeftColumn: Boolean;
    function MergeWithRightColumn: Boolean;
    procedure SetSourceItem(Value: TcxGridColumn);
    function UpdateColumnPosition: Boolean;
  protected
    procedure CalculateDestParams(AHitTest: TcxCustomGridHitTest;
      out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone); override;
    function CanMergeByCtrl: Boolean; virtual;
    function CanRemove: Boolean; override;
    procedure CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind); override;
    procedure DirtyChanged; override;
    procedure DoColumnMovingToHeader; virtual;
    procedure DoGetUnmergeableColumns; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect; override;
    function GetArrowAreaBoundsForGroupByBox: TRect; virtual;
    function GetArrowAreaBoundsForHeader(APlace: TcxGridArrowPlace): TRect; virtual;
    function GetArrowsClientRect: TRect; override;
    function GetMergeIndicatorClass: TcxGridColumnHeaderMergeIndicatorClass; virtual;
    function GetMergeIndicatorPosition: TPoint; virtual;
    function GetSourceItemViewInfo: TcxCustomGridCellViewInfo; override;
    function HasMergeIndicator: Boolean; virtual;
    procedure HeaderBeginUpdate(AFromGroupByBox: Boolean); virtual;
    procedure HeaderEndUpdate(AFromGroupByBox: Boolean); virtual;
    procedure InitMergeIndicator; virtual;
    procedure InitDragObjects; override;
    function IsCtrlPressed: Boolean; virtual;
    function IsMerging: Boolean; virtual;
    function IsValidDestination: Boolean; override;
    function IsValidDestinationForVisibleSource: Boolean; virtual;
    procedure MergeIndicatorPositionChanged(AVisible: Boolean = True); virtual;
    function NeedMergeIndicator: Boolean; virtual;
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean; override;
    function ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean; override;

    property GridView: TcxGridTableView read GetGridView;
    property MergeIndicator: TcxGridColumnHeaderMergeIndicator read FMergeIndicator;
    property OriginalDestColumnContainerKind: TcxGridColumnContainerKind
      read FOriginalDestColumnContainerKind write FOriginalDestColumnContainerKind;
    property UnmergeableColumns: TList read FUnmergeableColumns;
    property SourceItem: TcxGridColumn read GetSourceItem write SetSourceItem;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  TcxCustomGridSizingObject = class(TcxCustomGridDragAndDropObject)
  private
    FDestPointX: Integer;
    FDestPointY: Integer;
    FOriginalSize: Integer;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    function GetViewInfo: TcxGridTableViewInfo;
    procedure SetDestPointX(Value: Integer);
    procedure SetDestPointY(Value: Integer);
  protected
    procedure DirtyChanged; override;
    function GetCurrentSize: Integer; virtual;
    function GetDeltaSize: Integer; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetHorzSizingMarkBounds: TRect; virtual;
    function GetImmediateStart: Boolean; override;
    function GetIsHorizontalSizing: Boolean; virtual;
    function GetSizingItemBounds: TRect; virtual; abstract;
    function GetSizingMarkBounds: TRect; virtual;
    function GetSizingMarkWidth: Integer; virtual; abstract;
    function GetVertSizingMarkBounds: TRect; virtual;

    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;

    property Controller: TcxGridTableController read GetController;
    property CurrentSize: Integer read GetCurrentSize;
    property DeltaSize: Integer read GetDeltaSize;
    property DestPointX: Integer read FDestPointX write SetDestPointX;
    property DestPointY: Integer read FDestPointY write SetDestPointY;
    property GridView: TcxGridTableView read GetGridView;
    property IsHorizontalSizing: Boolean read GetIsHorizontalSizing;
    property OriginalSize: Integer read FOriginalSize write FOriginalSize;
    property SizingItemBounds: TRect read GetSizingItemBounds;
    property SizingMarkBounds: TRect read GetSizingMarkBounds;
    property SizingMarkWidth: Integer read GetSizingMarkWidth;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  public
    procedure BeforeScrolling; override;
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  TcxCustomGridColumnSizingObject = class(TcxCustomGridSizingObject)
  private
    FColumn: TcxGridColumn;
    function GetColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo;
  protected
    function GetSizingItemBounds: TRect; override;
    function GetSizingMarkWidth: Integer; override;
    property Column: TcxGridColumn read FColumn write FColumn;
    property ColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo read GetColumnHeaderViewInfo;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  TcxGridColumnHorzSizingObject = class(TcxCustomGridColumnSizingObject)
  protected
    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetCurrentSize: Integer; override;
  end;

  TcxGridRowSizingObject = class(TcxCustomGridSizingObject)
  private
    FRow: TcxCustomGridRow;
    function GetRowViewInfo: TcxCustomGridRowViewInfo;
  protected
    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetCurrentSize: Integer; override;
    function GetIsHorizontalSizing: Boolean; override;
    function GetSizingItemBounds: TRect; override;
    function GetSizingMarkWidth: Integer; override;
    property Row: TcxCustomGridRow read FRow;
    property RowViewInfo: TcxCustomGridRowViewInfo read GetRowViewInfo;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  // customization form

  TcxGridTableItemsListBox = class(TcxCustomGridTableItemsListBox)
  private
    function GetGridView: TcxGridTableView;
    function GetTextColor: TColor;
  protected
    function CalculateItemHeight: Integer; override;
    function DrawItemDrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual; abstract;
    function GetItemEndEllipsis: Boolean; virtual; abstract;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure UpdateBackgroundColor;
    //
    property GridView: TcxGridTableView read GetGridView;
    property TextColor: TColor read GetTextColor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PaintItem(ACanvas: TcxCanvas; R: TRect; AIndex: Integer; AFocused: Boolean); override;
  end;

  TcxGridTableColumnsListBox = class(TcxGridTableItemsListBox)
  protected
    procedure DoRefreshItems; override;
    function DrawItemDrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    function GetDragAndDropParams: TcxCustomGridHitTest; override;
    function GetItemEndEllipsis: Boolean; override;
  end;

  TcxGridTableCustomizationForm = class(TcxCustomGridTableCustomizationForm)
  private
    function GetColumnsListBox: TcxGridTableColumnsListBox;
    function GetColumnsPage: TcxTabSheet;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    function GetViewInfo: TcxGridTableViewInfo;
  protected
    function GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass; override;
    function GetItemsPageCaption: string; override;

    property ColumnsListBox: TcxGridTableColumnsListBox read GetColumnsListBox;
    property GridView: TcxGridTableView read GetGridView;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  public
    property Controller: TcxGridTableController read GetController;
    property ColumnsPage: TcxTabSheet read GetColumnsPage;
  end;

  // drag open info

  TcxGridDragOpenInfoMasterDataRowTab = class(TcxGridDragOpenInfoTab)
  public
    GridRow: TcxGridMasterDataRow;
    constructor Create(ALevel: TcxGridLevel; AGridRow: TcxGridMasterDataRow); reintroduce; virtual;
    function Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean; override;
    procedure Run; override;
  end;

  // popup

  TcxGridColumnsCustomizationPopup = class(TcxCustomGridItemsCustomizationPopup)
  private
    function GetGridView: TcxGridTableView;
  protected
    procedure DoItemPosChanged(AItem: TObject); override;
    procedure SetItemIndex(AItem: TObject; AIndex: Integer); override;
  public
    property GridView: TcxGridTableView read GetGridView;
  end;

  // controllers

  TcxGridTableEditingController = class(TcxGridEditingController)
  strict private
    FApplyingImmediateFiltering: Boolean;
    FDelayedFilteringTimer: TcxTimer;
    FUpdateButtonEnabled: Boolean;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    procedure SetUpdateButtonEnabled(const Value: Boolean);
  protected
    procedure ApplyFilterRowFiltering;
    function CanInitEditing: Boolean; override;
    function CanUpdateEditValue: Boolean; override;
    procedure CheckInvalidateUpdateButton;
    procedure DoEditChanged; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    function GetHideEditOnFocusedRecordChange: Boolean; override;
    procedure InitEdit; override;
    function IsNeedInvokeEditChangedEventsBeforePost: Boolean; override;
    function NeedCellViewInfoWhenUpdateEditValue: Boolean; override;
    procedure PostEditingData; override;
    procedure ResetUpdateButtonEnabled;
    procedure StartDelayedFiltering;
    procedure OnDelayedFilteringTimer(Sender: TObject);

    property ApplyingImmediateFiltering: Boolean read FApplyingImmediateFiltering write FApplyingImmediateFiltering;
    property UpdateButtonEnabled: Boolean read FUpdateButtonEnabled write SetUpdateButtonEnabled;
  public
    destructor Destroy; override;
    procedure HideEdit(Accept: Boolean); override;

    property Controller: TcxGridTableController read GetController;
    property GridView: TcxGridTableView read GetGridView;
  end;

  TcxGridFocusedItemKind = (fikNone, fikGridItem, fikUpdateButton, fikCancelButton);

  TcxGridTableController = class(TcxCustomGridTableController)
  private
    FCellSelectionAnchor: TcxGridColumn;
    FDataRowFixingMenu: TcxGridDataRowFixingMenu;
    FFilterRowOperatorMenu: TcxFilterDropDownMenu;
    FFocusedItemKind: TcxGridFocusedItemKind;
    FHorzSizingColumn: TcxGridColumn;
    FIsFilterPopupOpenedFromHeader: Boolean;
    FKeepFilterRowFocusing: Boolean;
    FLeftPos: Integer;
    FPressedColumn: TcxGridColumn;
    FSelectedColumns: TList;

    function GetColumnsCustomizationPopup: TcxGridColumnsCustomizationPopup;
    function GetCustomizationForm: TcxGridTableCustomizationForm;
    function GetDataRowFixingMenu: TcxGridDataRowFixingMenu;
    function GetEditingController: TcxGridTableEditingController;
    function GetFilterRowOperatorMenu: TcxFilterDropDownMenu;
    function GetFocusedColumn: TcxGridColumn;
    function GetFocusedColumnIndex: Integer;
    function GetFocusedRow: TcxCustomGridRow;
    function GetFocusedRowIndex: Integer;
    function GetGridView: TcxGridTableView;
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
    function GetIsColumnHorzSizing: Boolean;
    function GetSelectedColumn(Index: Integer): TcxGridColumn;
    function GetSelectedColumnCount: Integer;
    function GetSelectedRow(Index: Integer): TcxCustomGridRow;
    function GetSelectedRowCount: Integer;
    function GetTopRowIndex: Integer;
    function GetViewData: TcxGridViewData;
    function GetViewInfo: TcxGridTableViewInfo;
    procedure SetFocusedColumn(Value: TcxGridColumn);
    procedure SetFocusedColumnIndex(Value: Integer);
    procedure SetFocusedRow(Value: TcxCustomGridRow);
    procedure SetFocusedRowIndex(Value: Integer);
    procedure SetFocusedItemKind(AValue: TcxGridFocusedItemKind);
    procedure SetLeftPos(Value: Integer);
    procedure SetPressedColumn(Value: TcxGridColumn);
    procedure SetTopRowIndex(Value: Integer);

    procedure AddSelectedColumn(AColumn: TcxGridColumn);
    procedure MakeMasterRecordVisible(ARecord: TcxCustomGridRecord);
    procedure RemoveSelectedColumn(AColumn: TcxGridColumn);
  protected
    procedure AdjustRowPositionForFixedGroupMode(ARow: TcxCustomGridRow); virtual;
    function CanAppend(ACheckOptions: Boolean): Boolean; override;
    function CanDataPost: Boolean; override;
    function CanDelete(ACheckOptions: Boolean): Boolean; override;
    function CanEdit: Boolean; override;
    function CanInsert(ACheckOptions: Boolean): Boolean; override;
    function CanMakeItemVisible(AItem: TcxCustomGridTableItem): Boolean;
    function CanUseAutoHeightEditing: Boolean; override;
    function CheckBrowseModeOnRecordChanging(ANewRecordIndex: Integer): Boolean; override;
    procedure CheckCoordinates; override;
    procedure CheckLeftPos(var Value: Integer);
    procedure CheckInternalTopRecordIndex(var AIndex: Integer); override;
    procedure DoMakeRecordVisible(ARecord: TcxCustomGridRecord); override;
    procedure FocusedItemChanged(APrevFocusedItem: TcxCustomGridTableItem); override;
    procedure FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); override;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function GetDlgCode: Integer; override;
    function GetFilterRowSupportedOperators: TcxFilterOperatorKinds; virtual;
    function GetFocusedRecord: TcxCustomGridRecord; override;
    function GetGroupIndexByFocusedRowIndex: Integer; virtual;
    function GetGroupIndexByRowIndex(AIndex: Integer): Integer; virtual;
    function GetIsRecordsScrollHorizontal: Boolean; override;
    function GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass; override;
    function GetMaxTopRecordIndexValue: Integer; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollBarRecordCount: Integer; override;
    function GetTopRecordIndex: Integer; override;
    function HasDataRowFixingMenu: Boolean; virtual;
    function HasFilterRowOperatorMenu: Boolean; virtual;
    function IsColumnFixedDuringHorzSizing(AColumn: TcxGridColumn): Boolean; virtual;
    function IsFirstPageRecordFocused: Boolean; override;
    function IsKeyForMultiSelect(AKey: Word; AShift: TShiftState;
      AFocusedRecordChanged: Boolean): Boolean; override;
    function IsPixelScrollBar(AKind: TScrollBarKind): Boolean; override;
    procedure LeftPosChanged; virtual;
    function NeedsAdditionalRowsScrolling(AIsCallFromMaster: Boolean = False): Boolean; virtual;
    procedure RemoveFocus; override;
    procedure ScrollData(ADirection: TcxDirection); override;
    procedure SetFocusedRecord(Value: TcxCustomGridRecord); override;
    procedure ShowDataRowFixingMenu(ADataRow: TcxGridDataRow; AForBounds: TRect); virtual;
    procedure ShowNextPage; override;
    procedure ShowPrevPage; override;
    function WantSpecialKey(AKey: Word): Boolean; override;

    //design
    procedure CreateGridViewItem(Sender: TObject); virtual;
    procedure DeleteGridViewItem(AItem: TPersistent); virtual;
    procedure DeleteGridViewItems(Sender: TObject); virtual;
    procedure PopulateColumnHeaderDesignPopupMenu(AMenu: TPopupMenu); virtual;

    //scrolling
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    function GetFirstScrollRecordIndex: Integer; override;
    function GetLastScrollRecordIndex: Integer; override;

    // internal drag and drop data scrolling
    function CanScrollData(ADirection: TcxDirection): Boolean; override;

    // selection
    function CanPostponeRecordSelection(AShift: TShiftState): Boolean; override;
    function CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest;
      AShift: TShiftState): Boolean; override;
    procedure DoMouseNormalSelection(AHitTest: TcxCustomGridHitTest); override;
    procedure DoMouseRangeSelection(AClearSelection: Boolean = True; AData: TObject = nil); override;
    procedure DoNormalSelection; override;
    procedure MultiSelectKeyDown(var Key: Word; Shift: TShiftState); override;
    function SupportsAdditiveSelection: Boolean; override;
    function SupportsRecordSelectionToggling: Boolean; override;

    //inplace edit form item focusing
    function CanCloseInplaceEditForm: Boolean;
    procedure CheckFocusedItem(AItemViewInfo: TcxGridTableViewInplaceEditFormDataCellViewInfo);
    function CloseInplaceEditFormOnRecordInserting: Boolean;
    function FocusNextInplaceEditFormItem(AGoForward: Boolean): Boolean;
    procedure GetBackwardInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList);
    procedure GetForwardInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList);
    function GetNextInplaceEditFormItemIndex(AFocusedIndex: Integer; AGoForward: Boolean): Integer;
    function GetNextInplaceButton: TcxGridFocusedItemKind;
    procedure ValidateInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList; AGoForward: Boolean);

    // special row focusing
    function DefocusSpecialRow: Boolean; virtual;
    function FocusSpecialRow: Boolean; virtual;
    procedure FilterRowFocusChanged; virtual;
    procedure FilterRowFocusChanging(AValue: Boolean); virtual;

    // pull focusing
    procedure DoPullFocusingScrolling(ADirection: TcxDirection); override;
    function GetPullFocusingScrollingDirection(X, Y: Integer; out ADirection: TcxDirection): Boolean; override;
    function SupportsPullFocusing: Boolean; override;

    // delphi drag and drop
    function GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo; override;
    function GetDragScrollDirection(X, Y: Integer): TcxDirection; override;

    // customization
    procedure CheckCustomizationFormBounds(var R: TRect); override;
    function GetColumnHeaderDragAndDropObjectClass: TcxGridColumnHeaderMovingObjectClass; virtual;
    function GetCustomizationFormClass: TcxCustomGridCustomizationFormClass; override;

    // cells selection
    function CanNormalSelectionOnMouse: Boolean; override;
    function CanProcessCellMultiSelect(APrevFocusedColumn: TcxGridColumn): Boolean; virtual;
    procedure CellMultiSelectKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoNormalCellSelection;
    procedure DoRangeCellSelection;
    function GetCellMultiSelect: Boolean; virtual;
    property CellMultiSelect: Boolean read GetCellMultiSelect;

    // BeginsWith mask
    procedure AddBeginsWithMask(var AValue: Variant);
    procedure RemoveBeginsWithMask(var AValue: Variant);
    function GetBeginsWithMaskPos(const AValue: string): Integer;

    function GetEditingControllerClass: TcxGridEditingControllerClass; override;

    property DataRowFixingMenu: TcxGridDataRowFixingMenu read GetDataRowFixingMenu;
    property FilterRowOperatorMenu: TcxFilterDropDownMenu read GetFilterRowOperatorMenu;
    property FocusedItemKind: TcxGridFocusedItemKind read FFocusedItemKind write SetFocusedItemKind;
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
    property IsFilterPopupOpenedFromHeader: Boolean read FIsFilterPopupOpenedFromHeader write FIsFilterPopupOpenedFromHeader;
    property KeepFilterRowFocusing: Boolean read FKeepFilterRowFocusing write FKeepFilterRowFocusing;
    property ViewData: TcxGridViewData read GetViewData;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    function CanShowGroupFooter(ALevel: Integer): Boolean;
    procedure CreateNewRecord(AtEnd: Boolean); override;
    procedure CheckScrolling(const P: TPoint); override;
    procedure ClearGrouping;
    procedure ClearSelection; override;
    procedure DoCancelMode; override;
    function FocusFirstAvailableItem: Boolean; override;
    function FocusNextCell(AGoForward: Boolean; AProcessCellsOnly: Boolean = True;
      AAllowCellsCycle: Boolean = True; AFollowVisualOrder: Boolean = True; ANeedNormalizeSelection: Boolean = False): Boolean; override;
    function FocusNextItem(AFocusedItemIndex: Integer;
      AGoForward, AGoOnCycle, AGoToNextRecordOnCycle, AFollowVisualOrder: Boolean; ANeedNormalizeSelection: Boolean = False): Boolean; override;
    function IsFilterRowFocused: Boolean;
    function IsFilterRowOperatorSupported(AItemIndex: Integer; AOperator: TcxFilterOperatorKind): Boolean; virtual;
    function IsNewItemRowFocused: Boolean;
    function IsSpecialRowFocused: Boolean; virtual;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); override;
    procedure SelectAll; override;
    procedure ShowEditFormCustomizationDialog;

    procedure InitScrollBarsParameters; override;
    function IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean; override;

    procedure EndDragAndDrop(Accepted: Boolean); override;

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // cells selection
    procedure ClearCellSelection;
    procedure SelectAllColumns;
    procedure SelectCells(AFromColumn, AToColumn: TcxGridColumn;
      AFromRowIndex, AToRowIndex: Integer);
    procedure SelectColumns(AFromColumn, AToColumn: TcxGridColumn);

    property CellSelectionAnchor: TcxGridColumn read FCellSelectionAnchor write FCellSelectionAnchor;
    property ColumnsCustomizationPopup: TcxGridColumnsCustomizationPopup read GetColumnsCustomizationPopup;
    property CustomizationForm: TcxGridTableCustomizationForm read GetCustomizationForm;
    property EditingController: TcxGridTableEditingController read GetEditingController;
    property FocusedColumn: TcxGridColumn read GetFocusedColumn write SetFocusedColumn;
    property FocusedColumnIndex: Integer read GetFocusedColumnIndex write SetFocusedColumnIndex;
    property FocusedRow: TcxCustomGridRow read GetFocusedRow write SetFocusedRow;
    property FocusedRowIndex: Integer read GetFocusedRowIndex write SetFocusedRowIndex;
    property GridView: TcxGridTableView read GetGridView;
    property HorzSizingColumn: TcxGridColumn read FHorzSizingColumn;
    property IsColumnHorzSizing: Boolean read GetIsColumnHorzSizing;
    property LeftPos: Integer read FLeftPos write SetLeftPos;
    property PressedColumn: TcxGridColumn read FPressedColumn write SetPressedColumn;
    property SelectedColumnCount: Integer read GetSelectedColumnCount;
    property SelectedColumns[Index: Integer]: TcxGridColumn read GetSelectedColumn;
    property SelectedRowCount: Integer read GetSelectedRowCount;
    property SelectedRows[Index: Integer]: TcxCustomGridRow read GetSelectedRow;
    property TopRowIndex: Integer read GetTopRowIndex write SetTopRowIndex;
  end;

  { painters }

  // column container

  TcxGridColumnContainerPainter = class(TcxCustomGridPartPainter)
  private
    function GetViewInfo: TcxGridColumnContainerViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawItems; virtual;
    function DrawItemsFirst: Boolean; virtual;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridColumnContainerViewInfo read GetViewInfo;
  end;

  // header

  TcxGridColumnHeaderAreaPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridColumnHeaderAreaViewInfo;
  protected
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridColumnHeaderAreaViewInfo read GetViewInfo;
  end;

  TcxGridColumnHeaderSortingMarkPainter = class(TcxGridColumnHeaderAreaPainter)
  protected
    procedure Paint; override;
  end;

  TcxGridColumnHeaderFilterButtonPainter = class(TcxGridColumnHeaderAreaPainter)
  private
    function GetSmartTagState: TcxFilterSmartTagState;
    function GetViewInfo: TcxGridColumnHeaderFilterButtonViewInfo;
  protected
    procedure Paint; override;

    property SmartTagState: TcxFilterSmartTagState read GetSmartTagState;
    property ViewInfo: TcxGridColumnHeaderFilterButtonViewInfo read GetViewInfo;
  end;

  TcxGridColumnHeaderGlyphPainter = class(TcxGridColumnHeaderAreaPainter)
  private
    function GetViewInfo: TcxGridColumnHeaderGlyphViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridColumnHeaderGlyphViewInfo read GetViewInfo;
  end;

  TcxGridColumnHeaderCheckBoxPainter = class(TcxGridColumnHeaderAreaPainter)
  private
    function GetViewInfo: TcxGridColumnHeaderCheckBoxAreaViewInfo;
  protected
    procedure Paint; override;

    property ViewInfo: TcxGridColumnHeaderCheckBoxAreaViewInfo read GetViewInfo;
  end;

  TcxGridColumnHeaderPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridColumnHeaderViewInfo;
  protected
    procedure BeforePaint; override;
    procedure DrawAreas; virtual;
    procedure DrawBorders; override;
    procedure DrawContent; override;
    procedure DrawPressed; virtual;
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;
    property ViewInfo: TcxGridColumnHeaderViewInfo read GetViewInfo;
  end;

  TcxGridHeaderPainter = class(TcxGridColumnContainerPainter)
  protected
    function DrawItemsFirst: Boolean; override;
  end;

  // group by box

  TcxGridGroupByBoxPainter = class(TcxGridColumnContainerPainter)
  private
    function GetViewInfo: TcxGridGroupByBoxViewInfo;
  protected
    procedure DrawBackground(const R: TRect); override;
    procedure DrawConnectors;
    procedure DrawContent; override;
    function DrawItemsFirst: Boolean; override;

    property ViewInfo: TcxGridGroupByBoxViewInfo read GetViewInfo;
  end;

  // footer

  TcxGridFooterCellPainter = class(TcxGridColumnHeaderPainter)
  protected
    procedure DrawBorders; override;
    procedure DrawContent; override;
  end;

  TcxGridFooterPainterClass = class of TcxGridFooterPainter;

  TcxGridFooterPainter = class(TcxGridColumnContainerPainter)
  private
    function GetViewInfo: TcxGridFooterViewInfo;
  protected
    procedure DrawBackground(const R: TRect); override;
    procedure DrawBorders; override;
    function DrawItemsFirst: Boolean; override;
    procedure DrawSeparator; virtual;
    property ViewInfo: TcxGridFooterViewInfo read GetViewInfo;
  end;

  // indicator

  TcxCustomGridIndicatorItemPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxCustomGridIndicatorItemViewInfo;
  protected
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;

    property ViewInfo: TcxCustomGridIndicatorItemViewInfo read GetViewInfo;
  end;

  TcxGridIndicatorHeaderItemPainter = class(TcxCustomGridIndicatorItemPainter)
  private
    function GetViewInfo: TcxGridIndicatorHeaderItemViewInfo;
  protected
    function DrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    procedure DrawContent; override;
    procedure DrawQuickCustomizationMark; virtual;
    property ViewInfo: TcxGridIndicatorHeaderItemViewInfo read GetViewInfo;
  end;

  TcxGridIndicatorRowItemPainter = class(TcxCustomGridIndicatorItemPainter)
  private
    function GetViewInfo: TcxGridIndicatorRowItemViewInfo;
  protected
    procedure DrawContent; override;
    property ViewInfo: TcxGridIndicatorRowItemViewInfo read GetViewInfo;
  end;

  TcxGridIndicatorFooterItemPainter = class(TcxCustomGridIndicatorItemPainter)
  private
    function GetViewInfo: TcxGridIndicatorFooterItemViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawBorders; override;
    property ViewInfo: TcxGridIndicatorFooterItemViewInfo read GetViewInfo;
  end;

  TcxGridIndicatorPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridIndicatorViewInfo;
  protected
    procedure DrawContent; override;
    procedure DrawItems; virtual;
    function DrawItemsFirst: Boolean; virtual;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridIndicatorViewInfo read GetViewInfo;
  end;

  // custom row

  TcxCustomGridRowPainter = class(TcxCustomGridRecordPainter)
  private
    function GetViewInfo: TcxCustomGridRowViewInfo;
  protected
    procedure DrawFooters; virtual;
    procedure DrawIndent; virtual;
    procedure DrawIndentPart(ALevel: Integer; const ABounds: TRect); virtual;
    procedure DrawLastHorzGridLine; virtual;
    procedure DrawSeparator; virtual;
    procedure Paint; override;
    property ViewInfo: TcxCustomGridRowViewInfo read GetViewInfo;
  end;

  // rows

  TcxGridFixedDataRowsPainterClass = class of TcxGridFixedDataRowsPainter;

  TcxGridFixedDataRowsPainter = class
  private
    FCanvas: TcxCanvas;
    FViewInfo: TcxGridFixedDataRowsViewInfo;
  protected
    procedure DrawBottomItems; virtual;
    procedure DrawTopItems; virtual;
    procedure Paint; virtual;

    property Canvas: TcxCanvas read FCanvas;
    property ViewInfo: TcxGridFixedDataRowsViewInfo read FViewInfo;
  public
    constructor Create(ACanvas: TcxCanvas; AViewInfo: TcxGridFixedDataRowsViewInfo); virtual;

    procedure MainPaint;
  end;

  TcxGridRowsPainterClass = class of TcxGridRowsPainter;

  TcxGridRowsPainter = class(TcxCustomGridRecordsPainter)
  private
    function GetViewInfo: TcxGridRowsViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxGridRowsViewInfo read GetViewInfo;
  public
    class procedure DrawDataRowCells(ARowViewInfo: TcxCustomGridRowViewInfo); virtual;
  end;

  // table

  TcxGridTablePainter = class(TcxCustomGridTablePainter)
  private
    FGridLines: TList;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    function GetViewInfo: TcxGridTableViewInfo;
  protected
    function CanOffset(AItemsOffset, DX, DY: Integer): Boolean; override;
    procedure ClearGridLines;
    procedure DrawFooter; virtual;
    procedure DrawGridLines; virtual;
    procedure DrawGroupByBox; virtual;
    procedure DrawHeader; virtual;
    procedure DrawIndicator; virtual;
    procedure DrawRecords; override;
    procedure Offset(AItemsOffset: Integer); override;
    procedure Offset(DX, DY: Integer); override;
    procedure PaintContent; override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure AddGridLine(const R: TRect);

    property Controller: TcxGridTableController read GetController;
    property GridView: TcxGridTableView read GetGridView;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
  end;

  //selection check boxes

  TcxGridHeaderCheckBoxViewInfo = class(TcxGridCheckBoxViewInfo)
  private
    function GetController: TcxGridTableController;
  protected
    function CalculateCheckBoxState: TcxCheckBoxState; override;
    procedure Toggle; override;

    property Controller: TcxGridTableController read GetController;
  end;

  TcxGridRowCheckBoxViewInfo = class(TcxGridCheckBoxViewInfo)
  private
    FRowViewInfo: TcxCustomGridRowViewInfo;

    function GetGridView: TcxGridTableView;
  protected
    function CalculateCheckBoxState: TcxCheckBoxState; override;
    function GetIsVisibleForPainting: Boolean; override;
    procedure Toggle; override;

    property RowViewInfo: TcxCustomGridRowViewInfo read FRowViewInfo;
  public
    constructor Create(ARowViewInfo: TcxCustomGridRowViewInfo); reintroduce; virtual;

    property GridView: TcxGridTableView read GetGridView;
  end;

  { view infos }

  // column container

  TcxGridColumnContainerViewInfo = class(TcxCustomGridPartViewInfo)
  private
    FItemHeight: Integer;
    FItems: TList;
    function GetController: TcxGridTableController;
    function GetCount: Integer;
    function GetGridView: TcxGridTableView;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetInternalItem(Index: Integer): TcxGridColumnHeaderViewInfo;
    function GetItem(Index: Integer): TcxGridColumnHeaderViewInfo;
    function GetItemHeight: Integer;
  protected
    function CreateItem(AIndex: Integer): TcxGridColumnHeaderViewInfo; virtual;
    procedure CreateItems; virtual;
    procedure DestroyItems; virtual;
    function GetColumn(Index: Integer): TcxGridColumn; virtual; abstract;
    function GetColumnCount: Integer; virtual; abstract;
    function GetItemClass: TcxGridColumnHeaderViewInfoClass; virtual;

    function CalculateItemHeight: Integer; virtual;
    function FindItem(AColumn: TcxCustomGridColumn): TcxGridColumnHeaderViewInfo; virtual;
    function GetAutoHeight: Boolean; virtual;
    function GetColumnAdditionalWidth(AColumn: TcxGridColumn): Integer; virtual;
    function GetColumnMinWidth(AColumn: TcxGridColumn): Integer; virtual;
    function GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors; virtual;
    function GetColumnWidth(AColumn: TcxGridColumn): Integer; virtual;
    function GetItemAdditionWidth(AItem: TcxGridColumnHeaderViewInfo): Integer; virtual;
    function GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect; virtual;
    function GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean; virtual;
    function GetItemsAreaBounds: TRect; virtual;
    function GetItemsHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetKind: TcxGridColumnContainerKind; virtual; abstract;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetZonesAreaBounds: TRect; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure Offset(DX, DY: Integer); override;

    property AutoHeight: Boolean read GetAutoHeight;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxGridColumn read GetColumn;
    property Controller: TcxGridTableController read GetController;
    property ZonesAreaBounds: TRect read GetZonesAreaBounds;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); override;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetZone(const P: TPoint): TcxGridItemContainerZone; virtual;
    property Count: Integer read GetCount;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property InternalItems[Index: Integer]: TcxGridColumnHeaderViewInfo read GetInternalItem;
    property ItemHeight: Integer read GetItemHeight;
    property Items[Index: Integer]: TcxGridColumnHeaderViewInfo read GetItem; default;
    property ItemsAreaBounds: TRect read GetItemsAreaBounds;
    property Kind: TcxGridColumnContainerKind read GetKind;
  end;

  // column header areas

  TcxGridColumnHeaderAreaViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo;
    function GetColumn: TcxCustomGridColumn;
    function GetGridView: TcxGridTableView;
    function GetGridViewInfo: TcxGridTableViewInfo;
  protected
    function CanShowContainerHint: Boolean; virtual;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetCanvas: TcxCanvas; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function HasMouse(AHitTest: TcxCustomGridHitTest): Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    //procedure Invalidate; virtual;
    function NeedsContainerHotTrack: Boolean; virtual;
    function OccupiesSpace: Boolean; virtual;
    function ResidesInContent: Boolean; virtual;

    property Column: TcxCustomGridColumn read GetColumn;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
  public
    constructor Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo); reintroduce; virtual;
    procedure Calculate(const ABounds: TRect; var ATextAreaBounds: TRect); reintroduce; virtual;
    property AlignmentHorz: TAlignment read GetAlignmentHorz;
    property AlignmentVert: TcxAlignmentVert read GetAlignmentVert;
    property ColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo read FColumnHeaderViewInfo;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  TcxGridColumnHeaderSortingMarkViewInfo = class(TcxGridColumnHeaderAreaViewInfo)
  private
    function GetSortOrder: TcxGridSortOrder;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CanShowContainerHint: Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;

    property SortOrder: TcxGridSortOrder read GetSortOrder;
  end;

  TcxGridColumnHeaderHorzSizingEdgeViewInfo = class(TcxGridColumnHeaderAreaViewInfo)
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function OccupiesSpace: Boolean; override;
    function ResidesInContent: Boolean; override;
  public
    procedure Calculate(const ABounds: TRect; var ATextAreaBounds: TRect); override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
  end;

  TcxGridColumnHeaderFilterButtonViewInfo = class(TcxGridColumnHeaderAreaViewInfo,
    IdxFilterPopupWindowOwner)
  private
    function GetActive: Boolean;
    function GetActuallyVisible: Boolean;
    function GetDropDownWindowValue: TdxFilterPopupWindow;
  protected
    { IdxFilterPopupWindowOwner }
    function IdxFilterPopupWindowOwner.GetLinkComponent = GetFilterDropDownWindowLinkComponent;
    function IdxFilterPopupWindowOwner.GetMode = GetFilterPopupMode;
    function IdxFilterPopupWindowOwner.GetOptions = GetFilterDropDownWindowOptions;
    function GetFilterDropDownWindowLinkComponent: TComponent; virtual;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; virtual;
    function GetFilterDropDownWindowOptions: TObject; virtual;

    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    procedure DropDown; override;
    function EmulateMouseMoveAfterCalculate: Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetAlwaysVisible: Boolean; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsVisibleForPainting: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function NeedsContainerHotTrack: Boolean; override;
    function OccupiesSpace: Boolean; override;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    function CloseDropDownWindowOnDestruction: Boolean; override;
    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    function GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz; override;
    function GetDropDownWindowOwnerBounds: TRect; override;
    function IsDropDownWindowOwner: Boolean; override;
    function IsSmartTag: Boolean;
    property DropDownWindow: TdxFilterPopupWindow read GetDropDownWindowValue;

    property ActuallyVisible: Boolean read GetActuallyVisible;
    property AlwaysVisible: Boolean read GetAlwaysVisible;
  public
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;
    property Active: Boolean read GetActive;
  end;

  TcxGridColumnHeaderGlyphViewInfo = class(TcxGridColumnHeaderAreaViewInfo)
  private
    FUseImages: Boolean;
    function GetGlyph: TdxSmartGlyph;
    function GetImageIndex: TcxImageIndex;
    function GetImages: TCustomImageList;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CanShowContainerHint: Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
  public
    constructor Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo); override;
    property Glyph: TdxSmartGlyph read GetGlyph;
    property ImageIndex: TcxImageIndex read GetImageIndex;
    property Images: TCustomImageList read GetImages;
  end;

  TcxGridColumnHeaderCheckBoxAreaViewInfo = class(TcxGridColumnHeaderAreaViewInfo)
  private
    FCheckBox: TcxGridHeaderCheckBoxViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetCheckBoxRect: TRect; virtual;
    function GetMargins: TRect; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure Offset(DX: Integer; DY: Integer); override;

    property CheckBox: TcxGridHeaderCheckBoxViewInfo read FCheckBox;
  public
    constructor Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo); override;
    destructor Destroy; override;

    procedure AfterRecalculation; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(const ABounds: TRect; var ATextAreaBounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
  end;

  // column header

  TcxGridColumnHeaderViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FAdditionalHeightAtTop: Integer;
    FAdditionalWidthAtLeft: Integer;
    FAreaViewInfos: TList;
    FCellBoundsForHint: TRect;
    FColumn: TcxGridColumn;
    FContainer: TcxGridColumnContainerViewInfo;
    FDrawPressed: Boolean;
    FIsFilterActive: Boolean;
    FNeighbors: TcxNeighbors;
    FOriginalWidth: Integer;
    FRealWidth: Integer;
    FSortByGroupSummary: Boolean;
    FTextAreaBounds: TRect;
    FWidth: Integer;

    function GetAreaViewInfoCount: Integer;
    function GetAreaViewInfo(Index: Integer): TcxGridColumnHeaderAreaViewInfo;
    function GetGridView: TcxGridTableView;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetHasTextOffsetLeft: Boolean;
    function GetHasTextOffsetRight: Boolean;
    function GetIndex: Integer;
    function GetIsFixed: Boolean;
    function GetOriginalWidth: Integer;
    function GetRealWidth: Integer;

    procedure EnumAreaViewInfoClasses(AClass: TClass);
    procedure CreateAreaViewInfos;
    procedure DestroyAreaViewInfos;
  protected
    function AreasNeedHotTrack: Boolean;
    procedure CalculateCellBoundsForHint; virtual;
    function CalculateHasTextOffset(ASide: TAlignment): Boolean; virtual;
    function CalculateHeight: Integer; override;
    function CalculateOriginalWidth(Value: Integer): Integer;
    function CalculateRealWidth(Value: Integer): Integer;
    procedure CalculateTextAreaBounds; virtual;
    procedure CalculateVisible(ALeftBound, AWidth: Integer); virtual;
    function CalculateWidth: Integer; override;
    function CanFilter: Boolean; virtual;
    function CanHorzSize: Boolean; virtual;
    function CanPress: Boolean; virtual;
    function CanShowHint: Boolean; override;
    function CanSort: Boolean; virtual;
    procedure CheckWidth(var Value: Integer); virtual;
    function CaptureMouseOnPress: Boolean; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoCalculateParams; override;
    function FindArea(AAreaClass: TcxGridColumnHeaderAreaViewInfoClass): TcxGridColumnHeaderAreaViewInfo; virtual;
    function GetActualState: TcxGridCellState; override;
    function GetAdditionalWidth: Integer; virtual;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetAreaBounds: TRect; override;
    procedure GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc); virtual;
    function GetAutoWidthSizable: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetCanvas: TcxCanvas; override;
    function GetCaption: string; virtual;
    class function GetCellBorderWidth(ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer; virtual;
    class function GetCellHeight(ATextHeight: Integer;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; override;
    function GetCheckBoxAreaWidth: Integer; virtual;
    function GetDataOffset: Integer; virtual;
    function GetHeight: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetIsDesignSelected: Boolean; override;
    function GetIsPressed: Boolean; virtual;
    function GetMaxWidth: Integer; virtual;
    function GetMinWidth: Integer; virtual;
    function GetMultiLine: Boolean; override;
    function GetMultiLinePainting: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealBounds: TRect; override;
    function GetRealTextAreaBounds: TRect; override;
    function GetShowEndEllipsis: Boolean; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    function GetTextHeightWithOffset: Integer; override;
    function GetTextWidthWithOffset: Integer; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    function HasCheckBox: Boolean; virtual;
    function HasCustomDraw: Boolean; override;
    function HasFixedContentSpace: Boolean; virtual;
    function HasGlyph: Boolean; virtual;
    function HasHeaderAsContainer: Boolean;
    function HasDataCellCheckBoxAdditionalWidth: Boolean; virtual;
    function HasDataCellPinAdditionalWidth: Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    // design
    function DesignMouseDown(AHitTest: TcxCustomGridHitTest;
      AButton: TMouseButton; AShift: TShiftState): Boolean; virtual;
    function HasDesignPopupMenu: Boolean; override;
    procedure PopulateDesignPopupMenu(AMenu: TPopupMenu); override;

    function GetCellBoundsForHint: TRect; override;
    function GetHintText: string; override;
    function GetHintTextRect(const AMousePos: TPoint): TRect; override;
    function HasCustomHint: Boolean; virtual;
    function IsHintForText: Boolean; override;
    function IsHintMultiLine: Boolean; override;

    procedure Offset(DX, DY: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    property Caption: string read GetCaption;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property HasTextOffsetLeft: Boolean read GetHasTextOffsetLeft;
    property HasTextOffsetRight: Boolean read GetHasTextOffsetRight;
    property OriginalWidth: Integer read GetOriginalWidth;
    property SortByGroupSummary: Boolean read FSortByGroupSummary;
  public
    constructor Create(AContainer: TcxGridColumnContainerViewInfo; AColumn: TcxGridColumn); reintroduce; virtual;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetBestFitWidth: Integer; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure InitAutoWidthItem(AAutoWidthItem: TcxAutoWidthItem);
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean; override;

    property AreaViewInfoCount: Integer read GetAreaViewInfoCount;
    property AreaViewInfos[Index: Integer]: TcxGridColumnHeaderAreaViewInfo read GetAreaViewInfo;
    property AutoWidthSizable: Boolean read GetAutoWidthSizable;
    property Column: TcxGridColumn read FColumn;
    property Container: TcxGridColumnContainerViewInfo read FContainer;
    property DataOffset: Integer read GetDataOffset;
    property DrawPressed: Boolean read FDrawPressed write FDrawPressed;
    property Index: Integer read GetIndex;
    property IsFilterActive: Boolean read FIsFilterActive;
    property IsFixed: Boolean read GetIsFixed;
    property IsPressed: Boolean read GetIsPressed;
    property MaxWidth: Integer read GetMaxWidth;
    property MinWidth: Integer read GetMinWidth;
    property Neighbors: TcxNeighbors read FNeighbors write FNeighbors;
    property RealWidth: Integer read GetRealWidth;
  end;

  // header

  TcxGridHeaderViewInfoSpecificClass = class of TcxGridHeaderViewInfoSpecific;

  TcxGridHeaderViewInfoSpecific = class
  private
    FContainerViewInfo: TcxGridHeaderViewInfo;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetItemHeight: Integer;
  protected
    function CalculateHeight: Integer; virtual;
    function GetHeight: Integer; virtual;
  public
    constructor Create(AContainerViewInfo: TcxGridHeaderViewInfo); virtual;
    property ContainerViewInfo: TcxGridHeaderViewInfo read FContainerViewInfo;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property Height: Integer read GetHeight;
    property ItemHeight: Integer read GetItemHeight;
  end;

  TcxGridHeaderViewInfoClass = class of TcxGridHeaderViewInfo;

  TcxGridHeaderViewInfo = class(TcxGridColumnContainerViewInfo)
  private
    FSpecific: TcxGridHeaderViewInfoSpecific;
  protected
    function GetColumn(Index: Integer): TcxGridColumn; override;
    function GetColumnCount: Integer; override;

    procedure AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo; ATopBound: Integer); virtual;
    procedure CalculateColumnAutoWidths; virtual;
    procedure CalculateColumnWidths; virtual;
    function CalculateHeight: Integer; override;
    procedure CalculateInvisible; override;
    function CalculateItemHeight: Integer; override;
    procedure CalculateItems; virtual;
    procedure CalculateVisible; override;
    function CalculateWidth: Integer; override;
    function CanCalculateAutoWidths: Boolean; virtual;
    function DrawColumnBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual;
    function GetAlignment: TcxGridPartAlignment; override;
    function GetAutoHeight: Boolean; override;
    function GetColumnBackgroundBitmap: TBitmap; virtual;
    function GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsAutoWidth: Boolean; override;
    function GetIsScrollable: Boolean; override;
    function GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean; override;
    function GetKind: TcxGridColumnContainerKind; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
    function GetZonesAreaBounds: TRect; override;
    function IsAlwaysVisibleForCalculation: Boolean; virtual;
    function IsHeightAssigned: Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;
    procedure RecalculateItemVisibles;

    property ColumnBackgroundBitmap: TBitmap read GetColumnBackgroundBitmap;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); override;
    destructor Destroy; override;
    procedure AssignColumnWidths;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property Specific: TcxGridHeaderViewInfoSpecific read FSpecific;
  end;

  // group by box

  TcxGridGroupByBoxColumnHeaderViewInfo = class(TcxGridColumnHeaderViewInfo)
  private
    function GetContainer: TcxGridGroupByBoxViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function GetCaption: string; override;
    function HasFixedContentSpace: Boolean; override;
    function InheritedCalculateHeight: Integer;
  public
    property Container: TcxGridGroupByBoxViewInfo read GetContainer;
  end;

  TcxGridGroupByBoxViewInfoClass = class of TcxGridGroupByBoxViewInfo;

  TcxGridGroupByBoxViewInfo = class(TcxGridColumnContainerViewInfo)
  private
    FCalculatingColumnWidth: Boolean;
    function GetGroupByBoxVerOffset: Integer;
    function GetLinkLineBounds(Index: Integer; Horizontal: Boolean): TRect;
  protected
    function GetColumn(Index: Integer): TcxGridColumn; override;
    function GetColumnCount: Integer; override;
    function GetItemClass: TcxGridColumnHeaderViewInfoClass; override;

    function CalculateHeight: Integer; override;
    function CalculateItemHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignment: TcxGridPartAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetColumnWidth(AColumn: TcxGridColumn): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsAutoWidth: Boolean; override;
    function GetIsScrollable: Boolean; override;
    function GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect; override;
    function GetKind: TcxGridColumnContainerKind; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function IsSingleLine: Boolean; virtual;

    property CalculatingColumnWidth: Boolean read FCalculatingColumnWidth;
    property GroupByBoxVerOffset: Integer read GetGroupByBoxVerOffset;
  public
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property LinkLineBounds[Index: Integer; Horizontal: Boolean]: TRect read GetLinkLineBounds;
  end;

  // footer

  TcxGridFooterCellData = class
  private
    FSummaryItem: TcxDataSummaryItem;
  public
    constructor Create(ASummaryItem: TcxDataSummaryItem); virtual;

    property SummaryItem: TcxDataSummaryItem read FSummaryItem;
  end;

  TcxGridFooterCellViewInfoClass = class of TcxGridFooterCellViewInfo;

  TcxGridFooterCellViewInfo = class(TcxGridColumnHeaderViewInfo)
  private
    FData: TcxGridFooterCellData;

    function GetContainer: TcxGridFooterViewInfo;
    function GetSummary: TcxDataSummary;
    function GetSummaryItem: TcxDataSummaryItem;
  protected
    procedure AfterCalculateBounds(var ABounds: TRect); override;
    function CanPress: Boolean; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetBackgroundBitmap: TBitmap; override;
    procedure GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc); override;
    function GetBorders: TcxBorders; override;
    class function GetCellBorderWidth(ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDesignSelected: Boolean; override;
    function GetIsPressed: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasCustomDraw: Boolean; override;
    function HasCustomHint: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure PopulateDesignPopupMenu(AMenu: TPopupMenu); override;

    property Summary: TcxDataSummary read GetSummary;
  public
    constructor Create(AContainer: TcxGridColumnContainerViewInfo; AData: TcxGridFooterCellData); reintroduce; virtual;
    function GetBestFitWidth: Integer; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property Container: TcxGridFooterViewInfo read GetContainer;
    property Data: TcxGridFooterCellData read FData;
    property SummaryItem: TcxDataSummaryItem read GetSummaryItem;
  end;

  TcxGridFooterViewInfoClass = class of TcxGridFooterViewInfo;

  TcxGridFooterViewInfo = class(TcxGridHeaderViewInfo)
  private
    FRowCount: Integer;
    FCellDataList: TdxFastObjectList;

    function GetCellData(AIndex: Integer): TcxGridFooterCellData;
    function GetMultipleSummaries: Boolean;
    function GetRowCount: Integer;
    function GetRowHeight: Integer;
  protected
    procedure AddAdornerTargetElementForColumn(AList: TStrings; AColumn: TcxCustomGridColumn; AName: string); virtual;
    procedure AddCellData(ASummaryItem: TcxDataSummaryItem); virtual;
    procedure CreateCellDataList; virtual;
    function CreateItem(AIndex: Integer): TcxGridColumnHeaderViewInfo; override;
    procedure CreateItems; override;
    function CreateCellData(ASummaryItem: TcxDataSummaryItem): TcxGridFooterCellData; virtual;
    procedure DestroyItems; override;
    function GetColumn(Index: Integer): TcxGridColumn; override;
    function GetColumnCount: Integer; override;
    function GetItemClass: TcxGridColumnHeaderViewInfoClass; override;
    procedure PopulateCellDataList(var AColumnHasSummaries: array of Boolean); overload; virtual;
    procedure PopulateCellDataList(ASummaryItems: TcxDataSummaryItems; var AColumnHasSummaries: array of Boolean); overload; virtual;
    procedure Prepare(ACellDataList: TdxFastObjectList); virtual;

    function CalculateBounds: TRect; override;
    function CalculateHeight: Integer; override;
    function CalculateItemHeight: Integer; override;
    procedure CalculateItem(AIndex: Integer); virtual;
    procedure CalculateItems; override;
    function CalculateRowCount: Integer; virtual;
    function CanCalculateAutoWidths: Boolean; override;
    function GetAlignment: TcxGridPartAlignment; override;
    function GetAutoHeight: Boolean; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBordersBounds: TRect; virtual;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetColumnWidth(AColumn: TcxGridColumn): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsAutoWidth: Boolean; override;
    function GetIsScrollable: Boolean; override;
    function GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect; override;
    function GetItemHeight(AColumn: TcxGridColumn): Integer; overload; virtual;
    function GetItemHeight(AIndex: Integer): Integer; overload;
    function GetItemHitTestClass: TcxCustomGridHitTestClass; virtual;
    function GetItemLeftBound(AColumn: TcxGridColumn): Integer; overload; virtual;
    function GetItemLeftBound(AIndex: Integer): Integer; overload;
    function GetItemRowIndex(AIndex: Integer): Integer; virtual;
    function GetItemsAreaBounds: TRect; override;
    function GetItemTopBound(AColumn: TcxGridColumn): Integer; overload; virtual;
    function GetItemTopBound(AIndex: Integer): Integer; overload; virtual;
    function GetKind: TcxGridColumnContainerKind; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetSeparatorBounds: TRect; virtual;
    function GetSeparatorWidth: Integer; virtual;
    function GetSummaryItems: TcxDataSummaryItems; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function HasSeparator: Boolean; virtual;
    function IsAlwaysVisibleForCalculation: Boolean; override;
    function IsColumnOnFirstLayer(AColumnIndex: Integer): Boolean; virtual;
    function IsHeightAssigned: Boolean; override;
    function IsItemVisible(AIndex: Integer): Boolean; virtual;
    function IsMultilayerLayout: Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;

    property CellDataList[AIndex: Integer]: TcxGridFooterCellData read GetCellData;
  public
    function CanShowMultipleSummaries: Boolean; virtual;
    function GetCellBestFitWidth(AColumn: TcxGridColumn): Integer; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property BordersBounds: TRect read GetBordersBounds;
    property MultipleSummaries: Boolean read GetMultipleSummaries;
    property RowCount: Integer read GetRowCount;
    property RowHeight: Integer read GetRowHeight;
    property SeparatorBounds: TRect read GetSeparatorBounds;
    property SeparatorWidth: Integer read GetSeparatorWidth;
    property SummaryItems: TcxDataSummaryItems read GetSummaryItems;
  end;

  // indicator

  TcxCustomGridIndicatorItemViewInfoClass = class of TcxCustomGridIndicatorItemViewInfo;

  TcxCustomGridIndicatorItemViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCheckBox: TcxGridCheckBoxViewInfo;
    FContainer: TcxGridIndicatorViewInfo;

    function GetGridView: TcxGridTableView;
    function GetGridViewInfo: TcxGridTableViewInfo;
  protected
    function CalculateWidth: Integer; override;
    function CreateCheckBox: TcxGridCheckBoxViewInfo; virtual; abstract;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetCheckBoxAreaBounds: TRect; virtual;
    function GetCheckBoxBounds: TRect; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetImageAreaBounds: TRect; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasCheckBox: Boolean; virtual;
    function HasCustomDraw: Boolean; override;
    procedure Offset(DX: Integer; DY: Integer); override;
    function ShowCheckBox: Boolean; virtual;

    property CheckBox: TcxGridCheckBoxViewInfo read FCheckBox;
  public
    constructor Create(AContainer: TcxGridIndicatorViewInfo); reintroduce; virtual;
    destructor Destroy; override;

    procedure Calculate(ALeftBound: Integer; ATopBound: Integer; AWidth: Integer = -1; AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property Container: TcxGridIndicatorViewInfo read FContainer;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
  end;

  TcxGridIndicatorHeaderItemViewInfo = class(TcxCustomGridIndicatorItemViewInfo)
  private
    function GetDropDownWindowValue: TcxCustomGridCustomizationPopup;
  protected
    function CalculateHeight: Integer; override;
    function CanShowHint: Boolean; override;
    function CreateCheckBox: TcxGridCheckBoxViewInfo; override;
    function GetCellBoundsForHint: TRect; override;
    function GetHintTextRect(const AMousePos: TPoint): TRect; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function IsHintForText: Boolean; override;
    function IsHintMultiLine: Boolean; override;
    function ShowCheckBox: Boolean; override;
    function SupportsQuickCustomization: Boolean; virtual;

    function CloseDropDownWindowOnDestruction: Boolean; override;
    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    property DropDownWindow: TcxCustomGridCustomizationPopup read GetDropDownWindowValue;
  end;

  TcxGridIndicatorRowItemViewInfoClass = class of TcxGridIndicatorRowItemViewInfo;

  TcxGridIndicatorRowItemViewInfo = class(TcxCustomGridIndicatorItemViewInfo)
  private
    FRowViewInfo: TcxCustomGridRowViewInfo;

    function GetController: TcxGridTableController;
    function GetGridRecord: TcxCustomGridRow;
    function GetGridView: TcxGridTableView;
  protected
    function CalculateHeight: Integer; override;
    function CreateCheckBox: TcxGridCheckBoxViewInfo; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetCheckBoxAreaBounds: TRect; override;
    function GetIndicatorKind: TcxIndicatorKind; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetNeighbors: TcxNeighbors; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRowSizingEdgeBounds: TRect; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function ShowCheckBox: Boolean; override;

    property Controller: TcxGridTableController read GetController;
    property RowSizingEdgeBounds: TRect read GetRowSizingEdgeBounds;
  public
    constructor Create(AContainer: TcxGridIndicatorViewInfo; ARowViewInfo: TcxCustomGridRowViewInfo); reintroduce; virtual;
    destructor Destroy; override;

    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property GridRecord: TcxCustomGridRow read GetGridRecord;
    property GridView: TcxGridTableView read GetGridView;
    property IndicatorKind: TcxIndicatorKind read GetIndicatorKind;
    property RowViewInfo: TcxCustomGridRowViewInfo read FRowViewInfo;
  end;

  TcxGridIndicatorFooterItemViewInfo = class(TcxCustomGridIndicatorItemViewInfo)
  private
    function GetSeparatorWidth: Integer;
  protected
    function CalculateHeight: Integer; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBorders: TcxBorders; override;
    function GetBordersBounds: TRect; virtual;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetSeparatorBounds: TRect; virtual;
    function HasSeparator: Boolean;
    function ShowCheckBox: Boolean; override;
  public
    property BordersBounds: TRect read GetBordersBounds;
    property SeparatorBounds: TRect read GetSeparatorBounds;
    property SeparatorWidth: Integer read GetSeparatorWidth;
  end;

  TcxGridIndicatorViewInfoClass = class of TcxGridIndicatorViewInfo;

  TcxGridIndicatorViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetGridView: TcxGridTableView;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetItem(Index: Integer): TcxCustomGridIndicatorItemViewInfo;
    procedure DestroyItems;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlwaysVisible: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; override;
    function GetCheckBoxAreaWidth: Integer; virtual;
    function GetCheckBoxAreaMargins: TRect; virtual;
    function GetIndicatorAreaWidth: Integer; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRowItemClass(ARowViewInfo: TcxCustomGridRowViewInfo): TcxGridIndicatorRowItemViewInfoClass; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
    function ShowCheckBoxes: Boolean; virtual;
  public
    constructor Create(AGridViewInfo: TcxGridTableViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    function AddItem(AItemClass: TcxCustomGridIndicatorItemViewInfoClass): TcxCustomGridIndicatorItemViewInfo; overload;
    function AddItem(ATopBound, AHeight: Integer;
      AItemClass: TcxCustomGridIndicatorItemViewInfoClass): TcxCustomGridIndicatorItemViewInfo; overload;
    function AddRowItem(ARowViewInfo: TcxCustomGridRowViewInfo): TcxCustomGridIndicatorItemViewInfo;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure CalculateRowItem(ARowViewInfo: TcxCustomGridRowViewInfo;
      AItem: TcxCustomGridIndicatorItemViewInfo);
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetRowItemBounds(AGridRecord: TcxCustomGridRow): TRect;

    property AlwaysVisible: Boolean read GetAlwaysVisible;
    property Count: Integer read GetCount;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property Items[Index: Integer]: TcxCustomGridIndicatorItemViewInfo read GetItem;
  end;

  // custom row

  TcxGridRowFooterCellData = class(TcxGridFooterCellData)
  private
    FGroupedColumnIndex: Integer;
  public
    constructor Create(ASummaryItem: TcxDataSummaryItem; AGroupedColumnIndex: Integer); reintroduce; virtual;

    property GroupedColumnIndex: Integer read FGroupedColumnIndex;
  end;

  TcxGridRowFooterCellViewInfo = class(TcxGridFooterCellViewInfo)
  private
    function GetContainer: TcxGridRowFooterViewInfo;
    function GetGridRecord: TcxCustomGridRow;
    function GetData: TcxGridRowFooterCellData;
  protected
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
  public
    property Container: TcxGridRowFooterViewInfo read GetContainer;
    property GridRecord: TcxCustomGridRow read GetGridRecord;
    property Data: TcxGridRowFooterCellData read GetData;
  end;

  TcxGridRowFooterViewInfoClass = class of TcxGridRowFooterViewInfo;

  TcxGridRowFooterViewInfo = class(TcxGridFooterViewInfo)
  private
    FContainer: TcxGridRowFootersViewInfo;
    FPopulatedGroupedColumnIndex: Integer;
    FLevel: Integer;
    function GetIndent: Integer;
    function GetGridRecord: TcxCustomGridRow;
    function GetGroupLevel: Integer;
    function GetRowViewInfo: TcxCustomGridRowViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CreateCellData(ASummaryItem: TcxDataSummaryItem): TcxGridFooterCellData; override;
    function GetBorders: TcxBorders; override;
    function GetColumnWidth(AColumn: TcxGridColumn): Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsPart: Boolean; override;
    function GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect; override;
    function GetItemClass: TcxGridColumnHeaderViewInfoClass; override;
    function GetItemHitTestClass: TcxCustomGridHitTestClass; override;
    function GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean; override;
    function GetSummaryItems: TcxDataSummaryItems; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    function GetVisualLevel: Integer; virtual;
    function HasSeparator: Boolean; override;
    procedure PopulateCellDataList(var AColumnHasSummaries: array of Boolean); override;
    procedure Prepare(ACellDataList: TdxFastObjectList); override;
    property Indent: Integer read GetIndent;
  public
    constructor Create(AContainer: TcxGridRowFootersViewInfo; ALevel: Integer); reintroduce; virtual;
    function CanShowMultipleSummaries: Boolean; override;
    property Container: TcxGridRowFootersViewInfo read FContainer;
    property GridRecord: TcxCustomGridRow read GetGridRecord;
    property GroupLevel: Integer read GetGroupLevel;
    property Level: Integer read FLevel;
    property RowViewInfo: TcxCustomGridRowViewInfo read GetRowViewInfo;
    property VisualLevel: Integer read GetVisualLevel;
  end;

  TcxGridRowFootersViewInfoClass = class of TcxGridRowFootersViewInfo;

  TcxGridRowFootersViewInfo = class
  private
    FHeight: Integer;
    FIsRightToLeftConverted: Boolean;
    FItems: TList;
    FRowViewInfo: TcxCustomGridRowViewInfo;
    function GetCount: Integer;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetHeight: Integer;
    function GetItem(Index: Integer): TcxGridRowFooterViewInfo;
    function GetVisibleItem(ALevel: Integer): TcxGridRowFooterViewInfo;
    procedure CreateItems;
    procedure DestroyItems;
  protected
    procedure BeforeRecalculation; virtual;
    procedure Calculate(ALeftBound, ATopBound: Integer); virtual;
    function CalculateHeight: Integer; virtual;
    function GetItemClass: TcxGridRowFooterViewInfoClass; virtual;
  public
    constructor Create(ARowViewInfo: TcxCustomGridRowViewInfo); virtual;
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetCellBestFitWidth(AColumn: TcxGridColumn): Integer;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetTopBound(ALevel: Integer; var ATopBound: Integer): Boolean;
    procedure Offset(DX, DY: Integer); virtual;
    procedure Paint;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Count: Integer read GetCount;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property Items[Index: Integer]: TcxGridRowFooterViewInfo read GetItem; default;
    property Height: Integer read GetHeight;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property RowViewInfo: TcxCustomGridRowViewInfo read FRowViewInfo;
    property VisibleItems[ALevel: Integer]: TcxGridRowFooterViewInfo read GetVisibleItem;
  end;

  TcxCustomGridRowViewInfoClass = class of TcxCustomGridRowViewInfo;

  TcxCustomGridRowViewInfo = class(TcxCustomGridRecordViewInfo)
  private
    FFootersViewInfo: TcxGridRowFootersViewInfo;
    FIndicatorItem: TcxCustomGridIndicatorItemViewInfo;

    function GetCacheItem: TcxGridTableViewInfoCacheItem;
    function GetCheckBoxState: TcxCheckBoxState;
    function GetController: TcxGridTableController;
    function GetGridView: TcxGridTableView;
    function GetGridLines: TcxGridLines;
    function GetGridRecord: TcxCustomGridRow;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetFocusedItemKind: TcxGridFocusedItemKind;
    function GetLevel: Integer;
    function GetLevelIndent: Integer;
    function GetLevelIndentBounds(Index: Integer): TRect;
    function GetLevelIndentHorzLineBounds(Index: Integer): TRect;
    function GetLevelIndentSpaceBounds(Index: Integer): TRect;
    function GetLevelIndentVertLineBounds(Index: Integer): TRect;
    function GetRecordsViewInfo: TcxGridRowsViewInfo;
    procedure CreateFootersViewInfo;
    procedure DestroyFootersViewInfo;
    procedure RecreateFootersViewInfo;
  protected
    procedure AddAdornerTargetElementsForColumn(AList: TStrings; AColumn: TcxCustomGridColumn; AName: string); virtual;
    procedure AfterRowsViewInfoCalculate; virtual;
    procedure AfterRowsViewInfoOffset; virtual;
    procedure CalculateExpandButtonBounds(var ABounds: TRect); override;
    function CalculateHeight: Integer; override;
    function CalculateLevelIndentHorzLineBounds(ALevel: Integer; const ABounds: TRect): TRect;
    function CalculateLevelIndentSpaceBounds(ALevel: Integer; const ABounds: TRect): TRect;
    function CalculateLevelIndentVertLineBounds(ALevel: Integer; const ABounds: TRect): TRect;
    function CalculateWidth: Integer; override;
    function CanSize: Boolean; virtual;
    procedure CheckRowHeight(var AValue: Integer); virtual;
    procedure DoToggleExpanded; virtual;
    function GetAutoHeight: Boolean; override;
    function GetBaseHeight: Integer; virtual;
    function GetBottomPartHeight: Integer; virtual;
    function GetCellTransparent(ACell: TcxGridTableCellViewInfo): Boolean; override;
    function GetContentBounds: TRect; override;
    function GetContentIndent: Integer; virtual;
    function GetContentWidth: Integer; override;
    function GetDataHeight: Integer; virtual;
    function GetDataIndent: Integer; virtual;
    function GetDataWidth: Integer; virtual;
    function GetFixedState: TcxDataControllerRowFixedState; virtual;
    function GetFocusRectBounds: TRect; override;
    function GetFootersViewInfoClass: TcxGridRowFootersViewInfoClass; virtual;
    function GetLastHorzGridLineBounds: TRect; virtual;
    function GetMaxHeight: Integer; virtual;
    function GetNonBaseHeight: Integer; virtual;
    function GetRowHeight: Integer; virtual;
    function GetSeparatorBounds: TRect; virtual;
    function GetSeparatorColor: TColor; virtual;
    function GetSeparatorWidth: Integer; virtual;
    function GetShowSeparator: Boolean; virtual;
    function GetTopPartHeight: Integer; virtual;
    function GetVisible: Boolean; override;
    function GetVisualLevel: Integer; virtual;
    function GetWidth: Integer; override;
    function HasAnyFooter(ALevel: Integer): Boolean;
    function HasFooter(ALevel: Integer): Boolean; virtual;
    function HasFooters: Boolean; virtual;
    function HasLastHorzGridLine: Boolean; virtual;
    function InvalidateOnChildHotTrackChanged: Boolean; override;
    function IsFullyVisible: Boolean; virtual;
    function IsSpecial: Boolean; virtual;
    function NeedToggleExpandRecord(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;
    procedure SetFixedState(AValue: TcxDataControllerRowFixedState); virtual;
    procedure SetRowHeight(Value: Integer); virtual; abstract;
    function ShowCheckBox: Boolean; virtual;
    procedure ToggleCheckBox; virtual;
    procedure ToggleFixedState(AMenuForBounds: TRect); virtual;

    property BaseHeight: Integer read GetBaseHeight;
    property BottomPartHeight: Integer read GetBottomPartHeight;
    property CacheItem: TcxGridTableViewInfoCacheItem read GetCacheItem;
    property CheckBoxState: TcxCheckBoxState read GetCheckBoxState;
    property Controller: TcxGridTableController read GetController;
    property IndicatorItem: TcxCustomGridIndicatorItemViewInfo read FIndicatorItem;
    property FixedState: TcxDataControllerRowFixedState read GetFixedState write SetFixedState;
    property FocusedItemKind: TcxGridFocusedItemKind read GetFocusedItemKind;
    property LastHorzGridLineBounds: TRect read GetLastHorzGridLineBounds;
    property Level: Integer read GetLevel;
    property LevelIndent: Integer read GetLevelIndent;
    property NonBaseHeight: Integer read GetNonBaseHeight;
    property RowHeight: Integer read GetRowHeight write SetRowHeight;
    property ShowSeparator: Boolean read GetShowSeparator;
    property TopPartHeight: Integer read GetTopPartHeight;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
      ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function Click(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetBoundsForInvalidate(AItem: TcxCustomGridTableItem): TRect; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function HasSeparator: Boolean;

    property ContentIndent: Integer read GetContentIndent;
    property DataHeight: Integer read GetDataHeight;
    property DataIndent: Integer read GetDataIndent;
    property DataWidth: Integer read GetDataWidth;
    property FootersViewInfo: TcxGridRowFootersViewInfo read FFootersViewInfo;
    property GridView: TcxGridTableView read GetGridView;
    property GridLines: TcxGridLines read GetGridLines;
    property GridRecord: TcxCustomGridRow read GetGridRecord;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property LevelIndentBounds[Index: Integer]: TRect read GetLevelIndentBounds;
    property LevelIndentHorzLineBounds[Index: Integer]: TRect read GetLevelIndentHorzLineBounds;
    property LevelIndentSpaceBounds[Index: Integer]: TRect read GetLevelIndentSpaceBounds;
    property LevelIndentVertLineBounds[Index: Integer]: TRect read GetLevelIndentVertLineBounds;
    property MaxHeight: Integer read GetMaxHeight;
    property RecordsViewInfo: TcxGridRowsViewInfo read GetRecordsViewInfo;
    property SeparatorBounds: TRect read GetSeparatorBounds;
    property SeparatorColor: TColor read GetSeparatorColor;
    property SeparatorWidth: Integer read GetSeparatorWidth;
    property VisualLevel: Integer read GetVisualLevel;
  end;

  // rows

  TcxGridFixedDataRowsViewInfo = class
  private
    FBottomItems: TdxFastObjectList;
    FIsRightToLeftConverted: Boolean;
    FRowsViewInfo: TcxGridRowsViewInfo;
    FTopItems: TdxFastObjectList;

    function GetBottomItem(Index: Integer): TcxCustomGridRowViewInfo;
    function GetBottomItemCount: Integer;
    function GetBottomItemMaxCount: Integer;
    function GetCanvas: TcxCanvas;
    function GetController: TcxGridTableController;
    function GetTopItem(Index: Integer): TcxCustomGridRowViewInfo;
    function GetTopItemCount: Integer;
    function GetTopItemMaxCount: Integer;
    function GetViewData: TcxGridViewData;
  protected
    procedure CalculateBottomItems(ALeftBound, ATopBound: Integer); virtual;
    procedure CalculateTopItems(ALeftBound, ATopBound: Integer); virtual;
    procedure ControlFocusChanged; virtual;
    function CreateRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo; virtual;
    function GetBottomContentHeight: Integer; virtual;
    function GetPainterClass: TcxGridFixedDataRowsPainterClass; virtual;
    function GetTopContentHeight: Integer; virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    property BottomItems[Index: Integer]: TcxCustomGridRowViewInfo read GetBottomItem;
    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItemMaxCount: Integer read GetBottomItemMaxCount;
    property Canvas: TcxCanvas read GetCanvas;
    property Controller: TcxGridTableController read GetController;
    property RowsViewInfo: TcxGridRowsViewInfo read FRowsViewInfo;
    property TopItems[Index: Integer]: TcxCustomGridRowViewInfo read GetTopItem;
    property TopItemCount: Integer read GetTopItemCount;
    property TopItemMaxCount: Integer read GetTopItemMaxCount;
    property ViewData: TcxGridViewData read GetViewData;
  public
    constructor Create(ARowsViewInfo: TcxGridRowsViewInfo); virtual;
    destructor Destroy; override;

    procedure Calculate(ABounds: TRect); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    procedure Paint; virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
  end;

  TcxGridRowsViewInfoClass = class of TcxGridRowsViewInfo;

  TcxGridRowsViewInfo = class(TcxCustomGridRecordsViewInfo)
  private
    FCommonPreviewHeight: Integer;
    FDataRowHeight: Integer;
    FFilterRowViewInfo: TcxCustomGridRowViewInfo;
    FFixedDataRowsViewInfo: TcxGridFixedDataRowsViewInfo;
    FGroupRowHeight: Integer;
    FNewItemRowViewInfo: TcxCustomGridRowViewInfo;
    FRestHeight: Integer;
    FRowHeight: Integer;

    function GetController: TcxGridTableController; inline;
    function GetFilterRowViewInfo: TcxCustomGridRowViewInfo;
    function GetGridView: TcxGridTableView;
    function GetGridLines: TcxGridLines;
    function GetGridViewInfo: TcxGridTableViewInfo;
    function GetHeaderViewInfo: TcxGridHeaderViewInfo;
    function GetItem(Index: Integer): TcxCustomGridRowViewInfo;
    function GetNewItemRowViewInfo: TcxCustomGridRowViewInfo;
    function GetPainterClassValue: TcxGridRowsPainterClass;
    function GetViewData: TcxGridViewData;

    procedure CalculateFixedGroupsForPixelScrolling;
    procedure DeleteOverlainItems(AFixedGroups: TdxFastList);
    procedure PopulateFixedGroups(AFixedGroups: TdxFastList);
    procedure PostFixedGroups(AFixedGroups: TdxFastList);
    procedure RecalculateFixedGroups;
    procedure CalculateFixedGroupsForRecordScrolling;
    procedure CreateMissingItems(ABottomViewInfo: TcxCustomGridRowViewInfo; ATopBound: Integer);
  protected
    FIsFirstRowFullyVisible: Boolean;
    procedure AfterCalculate; override;
    procedure AfterOffset; override;
    procedure Calculate; override;
    function CalculateBounds: TRect; override;
    procedure CalculateConsts; virtual;
    function CalculateContentBounds: TRect; override;
    function CalculateDataRowHeight: Integer; virtual;
    procedure CalculateFixedGroups;
    function CalculateGroupImagesHeight: Integer; virtual;
    function CalculateGroupRowDefaultHeight(AMinHeight: Boolean): Integer; virtual;
    function CalculateGroupRowHeight: Integer; virtual;
    procedure CalculateItems; virtual;
    function CalculatePreviewDefaultHeight: Integer; virtual;
    function CalculateRestHeight(ATopBound: Integer): Integer; virtual;
    function CalculateRowDefaultHeight: Integer; virtual;
    function CalculateRowHeight: Integer; virtual;
    procedure CalculateVisibleCount; override;
    procedure ControlFocusChanged; override;
    function CreateFixedGroupRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo; virtual;
    function CreateRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo; virtual;
    function GetAdjustedScrollPositionForFixedGroupMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
    function GetAdjustedPixelScrollPositionForFixedGroupsMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
    function GetAdjustedIndexScrollPositionForFixedGroupsMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
    function GetAutoDataCellHeight: Boolean; override;
    function GetCommonDataRowHeight: Integer; virtual;
    function GetFilterRowViewInfoClass: TcxCustomGridRowViewInfoClass; virtual;
    function GetFixedGroupsBottomBound: Integer;
    function GetFixedGroupsCount: Integer;
    function GetGroupBackgroundBitmap: TBitmap; virtual;
    function GetGroupRowSeparatorWidth: Integer; virtual;
    function GetItemLeftBound(AIndex: Integer): Integer; override;
    function GetItemsOffset(AItemCountDelta: Integer): Integer; override;
    function GetItemTopBound(AIndex: Integer): Integer; override;
    function GetIsScrollable: Boolean; virtual;
    function GetNewItemRowViewInfoClass: TcxCustomGridRowViewInfoClass; virtual;
    function GetPainterClass: TcxCustomGridRecordsPainterClass; override;
    function GetRowWidth: Integer; virtual;
    function GetSeparatorWidth: Integer; virtual;
    function GetViewInfoIndexByRecordIndex(ARecordIndex: Integer): Integer; override;
    function HasFilterRow: Boolean;
    function HasFixedDataRows: Boolean;
    function HasLastHorzGridLine(ARowViewInfo: TcxCustomGridRowViewInfo): Boolean; virtual;
    function HasNewItemRow: Boolean;
    function IsCellPartVisibleForFixedGroupsMode(ACellViewInfo: TcxGridTableDataCellViewInfo): Boolean;
    function IsFilterRowVisible: Boolean; virtual;
    function IsFixedDataRowsVisible: Boolean; virtual;
    function IsNewItemRowVisible: Boolean; virtual;
    function IsRowLocatedInGroup(ARowIndex, AGroupIndex, ALevel: Integer): Boolean; virtual;
    function NeedCalculateFixedGroups: Boolean; virtual;
    procedure NotifyItemsCalculationFinished;
    procedure OffsetItem(AIndex, AOffset: Integer); override;
    procedure UpdateVisibleCount; virtual;
    procedure VisibilityChanged(AVisible: Boolean); override;

    property CommonPreviewHeight: Integer read FCommonPreviewHeight;
    property Controller: TcxGridTableController read GetController;
    property FixedDataRowsViewInfo: TcxGridFixedDataRowsViewInfo read FFixedDataRowsViewInfo;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewInfo: TcxGridTableViewInfo read GetGridViewInfo;
    property HeaderViewInfo: TcxGridHeaderViewInfo read GetHeaderViewInfo;
    property IsScrollable: Boolean read GetIsScrollable;
    property ViewData: TcxGridViewData read GetViewData;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function CalculateCustomGroupRowHeight(AMinHeight: Boolean; AParams: TcxViewParams): Integer; virtual;
    function CanDataRowSize: Boolean; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetCellHeight(ACellContentHeight: Integer): Integer; override;
    function GetDataRowCellsAreaViewInfoClass: TClass; virtual;
    function GetFooterCellBestFitWidth(AColumn: TcxGridColumn): Integer;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetRealItem(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo; override;
    function GetRestHeight(ATopBound: Integer): Integer; virtual;
    function IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean; override;
    function IsDataRowHeightAssigned: Boolean; virtual;
    procedure Offset(DX, DY: Integer); override;

    property CommonDataRowHeight: Integer read GetCommonDataRowHeight;
    property DataRowHeight: Integer read FDataRowHeight;
    property FilterRowViewInfo: TcxCustomGridRowViewInfo read GetFilterRowViewInfo;
    property GridLines: TcxGridLines read GetGridLines;
    property GroupBackgroundBitmap: TBitmap read GetGroupBackgroundBitmap;
    property GroupRowHeight: Integer read FGroupRowHeight write FGroupRowHeight;
    property GroupRowSeparatorWidth: Integer read GetGroupRowSeparatorWidth;
    property IsFirstRowFullyVisible: Boolean read FIsFirstRowFullyVisible;
    property Items[Index: Integer]: TcxCustomGridRowViewInfo read GetItem; default;
    property NewItemRowViewInfo: TcxCustomGridRowViewInfo read GetNewItemRowViewInfo;
    property PainterClass: TcxGridRowsPainterClass read GetPainterClassValue;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowWidth: Integer read GetRowWidth;
    property SeparatorWidth: Integer read GetSeparatorWidth;
  end;

  // table

  TcxGridTableViewInfo = class(TcxCustomGridTableViewInfo)
  private
    FBorderOverlapSize: Integer;
    FDataWidth: Integer;
    FExpandButtonIndent: Integer;
    FFooterViewInfo: TcxGridFooterViewInfo;
    FGroupByBoxViewInfo: TcxGridGroupByBoxViewInfo;
    FHeaderViewInfo: TcxGridHeaderViewInfo;
    FIndicatorViewInfo: TcxGridIndicatorViewInfo;
    FLevelIndent: Integer;
    FPrevDataRowHeight: Integer;
    function GetController: TcxGridTableController; inline;
    function GetDataWidth: Integer;
    function GetGridView: TcxGridTableView; inline;
    function GetGridLineColor: TColor;
    function GetGridLines: TcxGridLines;
    function GetLeftPos: Integer;
    function GetLevelIndentBackgroundBitmap: TBitmap;
    function GetLevelIndentColor(Index: Integer): TColor;
    function GetRecordsViewInfo: TcxGridRowsViewInfo; inline;
    function GetViewData: TcxGridViewData; inline;
  protected
    procedure AfterCalculating; override;
    procedure BeforeCalculating; override;
    procedure CreateViewInfos; override;
    procedure DestroyViewInfos(AIsRecreating: Boolean); override;
    procedure Calculate; override;
    function CalculateBorderOverlapSize: Integer; virtual;
    function CalculateClientBounds: TRect; override;
    function CalculateDataWidth: Integer; virtual;
    function GetEqualHeightRecordScrollSize: Integer; override;
    procedure CalculateExpandButtonParams; virtual;
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer;
      var AFullyVisible: Boolean); override;
    function CalculatePartBounds(APart: TcxCustomGridPartViewInfo): TRect; override;
    procedure CalculateParts; virtual;
    function CalculateVisibleEqualHeightRecordCount: Integer; override;
    procedure CalculateWidth(const AMaxSize: TPoint; var AWidth: Integer); override;
    function DoGetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetColumnFooterWidth(AFooterViewInfo: TcxGridFooterViewInfo; AColumn: TcxGridColumn): Integer; virtual;
    function GetScrollContentWidth: Integer;
    function GetDefaultGridModeBufferCount: Integer; override;
    function GetFirstItemAdditionalWidth: Integer; virtual;
    function GetGridLineWidth: Integer; virtual;
    function GetLevelSeparatorColor: TColor; virtual;
    function GetMultilineEditorBounds(const ACellEditBounds: TRect; ACalculatedHeight: Integer;
      AAutoHeight: TcxInplaceEditAutoHeight): TRect; override;
    function GetNonRecordsAreaHeight(ACheckScrollBar: Boolean): Integer; override;
    function GetScrollableAreaBoundsForEdit: TRect; override;
    function GetScrollableAreaBoundsHorz: TRect; override;
    function GetScrollableAreaBoundsVert: TRect; override;
    function GetVisualLevelCount: Integer; virtual;
    function HasFirstBorderOverlap: Boolean; inline;
    procedure Offset(DX, DY: Integer); override;
    procedure RecreateViewInfos; override;
    function SupportsAutoHeight: Boolean; virtual;
    function SupportsGroupSummariesAlignedWithColumns: Boolean; virtual;
    function SupportsMultipleFooterSummaries: Boolean; virtual;

    function GetFooterPainterClass: TcxGridFooterPainterClass; virtual;
    function GetFooterViewInfoClass: TcxGridFooterViewInfoClass; virtual;
    function GetGroupByBoxViewInfoClass: TcxGridGroupByBoxViewInfoClass; virtual;
    function GetHeaderViewInfoClass: TcxGridHeaderViewInfoClass; virtual;
    function GetIndicatorViewInfoClass: TcxGridIndicatorViewInfoClass; virtual;
    function GetHeaderViewInfoSpecificClass: TcxGridHeaderViewInfoSpecificClass; virtual;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;

    property BorderOverlapSize: Integer read FBorderOverlapSize;
    property Controller: TcxGridTableController read GetController;
    property ViewData: TcxGridViewData read GetViewData;
  public
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetCellBorders(AIsRight, AIsBottom: Boolean): TcxBorders; virtual;
    function GetCellHeight(AIndex, ACellHeight: Integer): Integer; virtual;
    function GetCellTopOffset(AIndex, ACellHeight: Integer): Integer; virtual;
    function GetOffsetBounds(AItemsOffset: Integer; out AUpdateBounds: TRect): TRect; overload; virtual;
    function GetOffsetBounds(DX, DY: Integer; out AUpdateBounds: TRect): TRect; overload; virtual;
    function GetVisualLevel(ALevel: Integer): Integer; virtual;

    // for extended lookup edit
    function GetNearestPopupHeight(AHeight: Integer; AAdditionalRecord: Boolean = False): Integer; override;
    function GetPopupHeight(ADropDownRowCount: Integer): Integer; override;

    property DataWidth: Integer read GetDataWidth;
    property ExpandButtonIndent: Integer read FExpandButtonIndent write FExpandButtonIndent;
    property FirstItemAdditionalWidth: Integer read GetFirstItemAdditionalWidth;
    property FooterViewInfo: TcxGridFooterViewInfo read FFooterViewInfo;
    property GridLineColor: TColor read GetGridLineColor;
    property GridLines: TcxGridLines read GetGridLines;
    property GridLineWidth: Integer read GetGridLineWidth;
    property GridView: TcxGridTableView read GetGridView;
    property GroupByBoxViewInfo: TcxGridGroupByBoxViewInfo read FGroupByBoxViewInfo;
    property HeaderViewInfo: TcxGridHeaderViewInfo read FHeaderViewInfo;
    property IndicatorViewInfo: TcxGridIndicatorViewInfo read FIndicatorViewInfo;
    property LeftPos: Integer read GetLeftPos;
    property LevelIndent: Integer read FLevelIndent write FLevelIndent;
    property LevelIndentBackgroundBitmap: TBitmap read GetLevelIndentBackgroundBitmap;
    property LevelIndentColors[Index: Integer]: TColor read GetLevelIndentColor;
    property LevelSeparatorColor: TColor read GetLevelSeparatorColor;
    property RecordsViewInfo: TcxGridRowsViewInfo read GetRecordsViewInfo;
    property VisualLevelCount: Integer read GetVisualLevelCount;
  end;

  // cache

  TcxGridTableViewInfoCacheItem = class(TcxCustomGridTableViewInfoCacheItem)
  private
    FIsPreviewHeightAssigned: Boolean;
    FPreviewHeight: Integer;
    procedure SetPreviewHeight(Value: Integer);
  public
    procedure UnassignValues(AKeepMaster: Boolean); override;
    property IsPreviewHeightAssigned: Boolean read FIsPreviewHeightAssigned
      write FIsPreviewHeightAssigned;
    property PreviewHeight: Integer read FPreviewHeight write SetPreviewHeight;
  end;

  TcxGridMasterTableViewInfoCacheItem = class(TcxGridTableViewInfoCacheItem)
  private
    FIsDetailsSiteFullyVisibleAssigned: Boolean;
    FIsDetailsSiteHeightAssigned: Boolean;
    FIsDetailsSiteNormalHeightAssigned: Boolean;
    FIsDetailsSiteWidthAssigned: Boolean;
    FDetailsSiteFullyVisible: Boolean;
    FDetailsSiteHeight: Integer;
    FDetailsSiteNormalHeight: Integer;
    FDetailsSiteWidth: Integer;
    FUnassigningValues: Boolean;
    function GetGridRecord: TcxGridMasterDataRow;
    function GetIsDetailsSiteCachedInfoAssigned: Boolean;
    procedure SetDetailsSiteFullyVisible(Value: Boolean);
    procedure SetDetailsSiteHeight(Value: Integer);
    procedure SetDetailsSiteNormalHeight(Value: Integer);
    procedure SetDetailsSiteWidth(Value: Integer);
  protected
    property GridRecord: TcxGridMasterDataRow read GetGridRecord;
  public
    DetailsSiteCachedInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo;
    destructor Destroy; override;
    procedure UnassignValues(AKeepMaster: Boolean); override;
    property IsDetailsSiteCachedInfoAssigned: Boolean read GetIsDetailsSiteCachedInfoAssigned;
    property IsDetailsSiteFullyVisibleAssigned: Boolean read FIsDetailsSiteFullyVisibleAssigned write FIsDetailsSiteFullyVisibleAssigned;
    property IsDetailsSiteHeightAssigned: Boolean read FIsDetailsSiteHeightAssigned write FIsDetailsSiteHeightAssigned;
    property IsDetailsSiteNormalHeightAssigned: Boolean read FIsDetailsSiteNormalHeightAssigned write FIsDetailsSiteNormalHeightAssigned;
    property IsDetailsSiteWidthAssigned: Boolean read FIsDetailsSiteWidthAssigned write FIsDetailsSiteWidthAssigned;
    property DetailsSiteFullyVisible: Boolean read FDetailsSiteFullyVisible write SetDetailsSiteFullyVisible;
    property DetailsSiteHeight: Integer read FDetailsSiteHeight write SetDetailsSiteHeight;
    property DetailsSiteNormalHeight: Integer read FDetailsSiteNormalHeight write SetDetailsSiteNormalHeight;
    property DetailsSiteWidth: Integer read FDetailsSiteWidth write SetDetailsSiteWidth;
  end;

  { view }

  // column

  TcxCustomGridColumnOptions = class(TcxCustomGridTableItemOptions)
  private
    FAutoWidthSizable: Boolean;
    FCellMerging: Boolean;
    FGroupFooters: Boolean;
    FHorzSizing: Boolean;
    FShowGroupValuesWithImages: Boolean;

    function GetFilterRowOperator: TcxFilterOperatorKind;
    function GetGridView: TcxGridTableView;
    function GetItem: TcxCustomGridColumn;
    procedure SetAutoWidthSizable(Value: Boolean);
    procedure SetCellMerging(Value: Boolean);
    procedure SetFilterRowOperator(Value: TcxFilterOperatorKind);
    procedure SetGroupFooters(Value: Boolean);
    procedure SetHorzSizing(Value: Boolean);
    procedure SetShowGroupValuesWithImages(Value: Boolean);
  protected
    property GridView: TcxGridTableView read GetGridView;
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
    procedure Assign(Source: TPersistent); override;

    property AutoWidthSizable: Boolean read FAutoWidthSizable write SetAutoWidthSizable default True;
    property CellMerging: Boolean read FCellMerging write SetCellMerging default False;
    property GroupFooters: Boolean read FGroupFooters write SetGroupFooters default True;
    property HorzSizing: Boolean read FHorzSizing write SetHorzSizing default True;
    property Item: TcxCustomGridColumn read GetItem;
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
    property FilterRowOperator: TcxFilterOperatorKind read GetFilterRowOperator write SetFilterRowOperator default foEqual;
    property ShowEditButtons;
    property ShowGroupValuesWithImages: Boolean read FShowGroupValuesWithImages write SetShowGroupValuesWithImages default False;
  end;

  TcxGridColumnOptions = class(TcxCustomGridColumnOptions)
  published
    property AutoWidthSizable;
    property CellMerging;
    property EditAutoHeight;
    property GroupFooters;
    property Grouping;
    property HorzSizing;
    property Moving;
    property ShowCaption;
    property SortByDisplayText;
    property Sorting;
  end;

  TcxGridGetFooterStyleExEvent = procedure(Sender: TcxGridTableView; ARow: TcxCustomGridRow;
    AColumn: TcxGridColumn; AFooterGroupLevel: Integer; var AStyle: TcxStyle) of object;
  TcxGridGetFooterSummaryStyleEvent = procedure(AView: TcxGridTableView; ARow: TcxCustomGridRow;
    AColumn: TcxGridColumn; AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle) of object;
  TcxGridGetGroupSummaryStyleEvent = procedure(Sender: TcxGridTableView; ARow: TcxGridGroupRow;
    AColumn: TcxGridColumn; ASummaryItem: TcxDataSummaryItem; var AStyle: TcxStyle) of object;
  TcxGridGetHeaderStyleEvent = procedure(Sender: TcxGridTableView;
    AColumn: TcxGridColumn; {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AStyle: TcxStyle) of object;

  TcxGridColumnStyles = class(TcxCustomGridTableItemStyles)
  private
    FOnGetFooterStyle: TcxGridGetCellStyleEvent;
    FOnGetFooterStyleEx: TcxGridGetFooterStyleExEvent;
    FOnGetFooterSummaryStyle: TcxGridGetFooterSummaryStyleEvent;
    FOnGetGroupSummaryStyle: TcxGridGetGroupSummaryStyleEvent;
    FOnGetHeaderStyle: TcxGridGetHeaderStyleEvent;
    function GetGridViewValue: TcxGridTableView;
    function GetItem: TcxGridColumn;
    procedure SetOnGetFooterStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetFooterStyleEx(Value: TcxGridGetFooterStyleExEvent);
    procedure SetOnGetFooterSummaryStyle(Value: TcxGridGetFooterSummaryStyleEvent);
    procedure SetOnGetGroupSummaryStyle(Value: TcxGridGetGroupSummaryStyleEvent);
    procedure SetOnGetHeaderStyle(Value: TcxGridGetHeaderStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetFooterParams(ARow: TcxCustomGridRow; AFooterGroupLevel: Integer;
      ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams); virtual;
    procedure GetGroupSummaryParams(ARow: TcxGridGroupRow; ASummaryItem: TcxDataSummaryItem;
      out AParams: TcxViewParams); virtual;
    procedure GetHeaderParams(out AParams: TcxViewParams); virtual;
    property GridView: TcxGridTableView read GetGridViewValue;
    property Item: TcxGridColumn read GetItem;
  published
    property Footer: TcxStyle index isFooter read GetValue write SetValue;
    property GroupSummary: TcxStyle index isGroupSummary read GetValue write SetValue;
    property Header: TcxStyle index isHeader read GetValue write SetValue;
    property OnGetFooterStyle: TcxGridGetCellStyleEvent read FOnGetFooterStyle write SetOnGetFooterStyle;
    property OnGetFooterStyleEx: TcxGridGetFooterStyleExEvent read FOnGetFooterStyleEx write SetOnGetFooterStyleEx;
    property OnGetFooterSummaryStyle: TcxGridGetFooterSummaryStyleEvent read FOnGetFooterSummaryStyle write SetOnGetFooterSummaryStyle;
    property OnGetGroupSummaryStyle: TcxGridGetGroupSummaryStyleEvent read FOnGetGroupSummaryStyle write SetOnGetGroupSummaryStyle;
    property OnGetHeaderStyle: TcxGridGetHeaderStyleEvent read FOnGetHeaderStyle write SetOnGetHeaderStyle;
  end;

  TcxGridSummariesIndex = (siFooter, siGroupFooter, siGroup);

  TcxGridColumnSummaryClass = class of TcxGridColumnSummary;

  TcxGridColumnSummary = class(TcxCustomGridTableItemCustomOptions)
  private
    function GetDataController: TcxCustomDataController;
    function GetFormat(Index: Integer): string;
    function GetKind(Index: Integer): TcxSummaryKind;
    function GetSortByGroupFooterSummary: Boolean;
    function GetSortByGroupSummary: Boolean;
    procedure SetFormat(Index: Integer; const Value: string);
    procedure SetKind(Index: Integer; Value: TcxSummaryKind);
    procedure SetSortByGroupFooterSummary(Value: Boolean);
    procedure SetSortByGroupSummary(Value: Boolean);
  protected
    function GetSummaryItems(AIndex: TcxGridSummariesIndex): TcxDataSummaryItems;
    function GetSummaryItemsPosition(AIndex: TcxGridSummariesIndex): TcxSummaryPosition;
    property DataController: TcxCustomDataController read GetDataController;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FooterKind: TcxSummaryKind index 0 read GetKind write SetKind stored False;
    property FooterFormat: string index 0 read GetFormat write SetFormat stored False;
    property GroupFooterKind: TcxSummaryKind index 1 read GetKind write SetKind stored False;
    property GroupFooterFormat: string index 1 read GetFormat write SetFormat stored False;
    property GroupKind: TcxSummaryKind index 2 read GetKind write SetKind stored False;
    property GroupFormat: string index 2 read GetFormat write SetFormat stored False;
    property SortByGroupFooterSummary: Boolean read GetSortByGroupFooterSummary write SetSortByGroupFooterSummary stored False;
    property SortByGroupSummary: Boolean read GetSortByGroupSummary write SetSortByGroupSummary stored False;
  end;

  TcxGridColumnCompareRowValuesEvent = procedure(Sender: TcxGridColumn;
    ARow1: TcxGridDataRow; AProperties1: TcxCustomEditProperties; const AValue1: TcxEditValue;
    ARow2: TcxGridDataRow; AProperties2: TcxCustomEditProperties; const AValue2: TcxEditValue;
    var AAreEqual: Boolean) of object;
  TcxGridColumnCompareValuesEvent = procedure(Sender: TcxGridColumn;
    AProperties1: TcxCustomEditProperties; const AValue1: TcxEditValue;
    AProperties2: TcxCustomEditProperties; const AValue2: TcxEditValue; var AAreEqual: Boolean) of object;
  TcxGridColumnCustomDrawHeaderEvent = procedure(Sender: TcxGridTableView; ACanvas: TcxCanvas;
    AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean) of object;
  TcxGridGroupSummaryCellCustomDrawEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    ARow: TcxGridGroupRow; AColumn: TcxGridColumn; ASummaryItem: TcxDataSummaryItem;
    AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean) of object;

  TcxCustomGridColumn = class(TcxCustomGridTableItem)
  private
    FFooterAlignmentHorz: TAlignment;
    FGroupSummaryAlignment: TAlignment;
    FHeaderGlyph: TdxSmartGlyph;
    FHeaderGlyphAlignmentHorz: TAlignment;
    FHeaderGlyphAlignmentVert: TcxAlignmentVert;
    FHeaderImageIndex: TcxImageIndex;
    FFilterRowOperator: TcxFilterOperatorKind;
    FIsFooterAlignmentHorzAssigned: Boolean;
    FIsGroupSummaryAlignmentAssigned: Boolean;
    FLayoutItem: TcxGridInplaceEditFormLayoutItem;
    FSelected: Boolean;
    FSummary: TcxGridColumnSummary;
    FVisibleForEditForm: TdxDefaultBoolean;

    function GetController: TcxGridTableController;
    function GetFilterRowOperator: TcxFilterOperatorKind;
    function GetFooterAlignmentHorz: TAlignment;
    function GetGridView: TcxGridTableView;
    function GetGroupSummaryAlignment: TAlignment;
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
    function GetIsChildInMergedGroup: Boolean;
    function GetIsPreview: Boolean;
    function GetOptions: TcxCustomGridColumnOptions;
    function GetStyles: TcxGridColumnStyles;
    function GetViewData: TcxGridViewData;
    procedure SetFilterRowOperator(AValue: TcxFilterOperatorKind);
    procedure SetFooterAlignmentHorz(Value: TAlignment);
    procedure SetGroupSummaryAlignment(Value: TAlignment);
    procedure SetHeaderGlyph(Value: TdxSmartGlyph);
    procedure SetHeaderGlyphAlignmentHorz(Value: TAlignment);
    procedure SetHeaderGlyphAlignmentVert(Value: TcxAlignmentVert);
    procedure SetHeaderImageIndex(Value: TcxImageIndex);
    procedure SetIsChildInMergedGroup(Value: Boolean);
    procedure SetLayoutItem(const Value: TcxGridInplaceEditFormLayoutItem);
    procedure SetVisibleForEditForm(AValue: TdxDefaultBoolean);
    procedure SetOptions(Value: TcxCustomGridColumnOptions);
    procedure SetStyles(Value: TcxGridColumnStyles);
    procedure SetSummary(Value: TcxGridColumnSummary);

    function IsFooterAlignmentHorzStored: Boolean;
    function IsGroupSummaryAlignmentStored: Boolean;

    procedure HeaderGlyphChanged(Sender: TObject);
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; override;
    function GetSummaryClass: TcxGridColumnSummaryClass; virtual;

    procedure CreateNewLayoutItem; virtual;
    procedure DestroyLayoutItem; virtual;

    procedure AssignColumnWidths; virtual;
    function CanCellMerging: Boolean; virtual;
    function CanCreateLayoutItem: Boolean;
    function CanDataCellScroll: Boolean; override;
    function CanEdit: Boolean; override;
    function CanFilter(AVisually: Boolean): Boolean; override;
    function CanFilterMRUValueItems: Boolean; override;
    function CanFilterUsingChecks: Boolean; override;
    function CanFocus(ARecord: TcxCustomGridRecord): Boolean; override;
    function CanFocusInplaceEditFormItem(ARecord: TcxCustomGridRecord): Boolean; override;
    function CanGroup: Boolean; override;
    function CanHorzSize: Boolean; override;
    function CanIncSearch: Boolean; override;
    function CanScroll: Boolean; override;
    function CanShowGroupFooters: Boolean; virtual;
    function CanSort: Boolean; override;
    procedure CaptionChanged; override;
    procedure CheckAccessibilityForEditForm;
    procedure DoSetVisible(Value: Boolean); override;
    procedure ForceWidth(Value: Integer); override;
    function GetEditValue: Variant; override;
    procedure SetEditValue(const Value: Variant); override;
    function GetImageComboBoxProperties: TcxImageComboBoxProperties;
    function GetIsBottom: Boolean; virtual;
    function GetIsLeft: Boolean; virtual;
    function GetIsMostBottom: Boolean; virtual;
    function GetIsMostLeft: Boolean; virtual;
    function GetIsMostRight: Boolean; virtual;
    function GetIsRight: Boolean; virtual;
    function GetIsTop: Boolean; virtual;
    function GetVisible: Boolean; override;
    function GetVisibleForCustomization: Boolean; override;
    function HasFixedWidth: Boolean; override;
    function HideOnGrouping: Boolean; virtual;
    procedure InternalSetFilterRowOperator(AValue: TcxFilterOperatorKind);
    function InternalGetFilterRowOperator: TcxFilterOperatorKind;
    function IsFilterOperatorSupported(AOperator: TcxFilterOperatorKind): Boolean; virtual;
    function IsFilterRowIncrementalFiltering: Boolean;
    function IsFocusedCellViewInfoPartVisible: Boolean; override;
    function IsLayoutItemStored: Boolean; virtual;
    function IsVisibleForRecordChange: Boolean; override;
    function IsVisibleStored: Boolean; override;
    function IsVisibleForCustomizationStored: Boolean; override;
    procedure SetGridView(Value: TcxCustomGridTableView); override;
    function ShowGroupValuesWithImages: Boolean; virtual;
    function SupportsBeginsWithFilterOperator(ARow: TcxCustomGridRow): Boolean;
    function UseBeginWithMask: Boolean; virtual;
    //procedure VisibleChanged; dynamic;

    function GetHeaderViewInfoClass: TcxGridColumnHeaderViewInfoClass;

    function HasGlyph: Boolean;
    function IsVisibleForEditForm: Boolean;

    property Controller: TcxGridTableController read GetController;
    property FilterRowOperator: TcxFilterOperatorKind read GetFilterRowOperator write SetFilterRowOperator;
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
    property ViewData: TcxGridViewData read GetViewData;
  public
    constructor Create(AOwner: TComponent); override;
    function GroupBy(AGroupIndex: Integer; ACanShow: Boolean = True; AMergeWithLeftColumn: Boolean = False; AMergeWithRightColumn: Boolean = False): Boolean;

    property GridView: TcxGridTableView read GetGridView;
    property GroupingDateRanges;
    property Hidden;  // obsolete, use VisibleForCustomization
    property IsBottom: Boolean read GetIsBottom;
    property IsLeft: Boolean read GetIsLeft;
    property IsMostBottom: Boolean read GetIsMostBottom;
    property IsMostLeft: Boolean read GetIsMostLeft;
    property IsMostRight: Boolean read GetIsMostRight;
    property IsPreview: Boolean read GetIsPreview;
    property IsRight: Boolean read GetIsRight;
    property IsTop: Boolean read GetIsTop;
    property Selected: Boolean read FSelected;
  published
    property BestFitMaxWidth;
    property DateTimeGrouping;
    property FooterAlignmentHorz: TAlignment read GetFooterAlignmentHorz write SetFooterAlignmentHorz stored IsFooterAlignmentHorzStored;
    property GroupIndex;
    property GroupSummaryAlignment: TAlignment read GetGroupSummaryAlignment write SetGroupSummaryAlignment stored IsGroupSummaryAlignmentStored;
    property HeaderAlignmentHorz;
    property HeaderAlignmentVert;
    property HeaderGlyph: TdxSmartGlyph read FHeaderGlyph write SetHeaderGlyph;
    property HeaderGlyphAlignmentHorz: TAlignment read FHeaderGlyphAlignmentHorz write SetHeaderGlyphAlignmentHorz default taLeftJustify;
    property HeaderGlyphAlignmentVert: TcxAlignmentVert read FHeaderGlyphAlignmentVert write SetHeaderGlyphAlignmentVert default vaCenter;
    property HeaderHint;
    property HeaderImageIndex: TcxImageIndex read FHeaderImageIndex write SetHeaderImageIndex default -1;
    property IsChildInMergedGroup: Boolean read GetIsChildInMergedGroup write SetIsChildInMergedGroup default False;
    property LayoutItem: TcxGridInplaceEditFormLayoutItem read FLayoutItem write SetLayoutItem stored IsLayoutItemStored;
    property MinWidth;
    property Options: TcxCustomGridColumnOptions read GetOptions write SetOptions;
    property SortIndex;
    property SortOrder;
    property Styles: TcxGridColumnStyles read GetStyles write SetStyles;
    property Summary: TcxGridColumnSummary read FSummary write SetSummary;
    property VisibleForCustomization;
    property VisibleForEditForm: TdxDefaultBoolean read FVisibleForEditForm write SetVisibleForEditForm default bDefault;
    property Width;
  end;

  TcxGridColumn = class(TcxCustomGridColumn)
  private
    FOnCompareRowValuesForCellMerging: TcxGridColumnCompareRowValuesEvent;
    FOnCompareValuesForCellMerging: TcxGridColumnCompareValuesEvent;
    FOnCustomDrawFooterCell: TcxGridColumnCustomDrawHeaderEvent;
    FOnCustomDrawGroupSummaryCell: TcxGridGroupSummaryCellCustomDrawEvent;
    FOnCustomDrawHeader: TcxGridColumnCustomDrawHeaderEvent;
    FOnHeaderClick: TNotifyEvent;
    function GetIsPreview: Boolean;
    function GetOptions: TcxGridColumnOptions;
    function GetSelected: Boolean;
    procedure SetIsPreview(Value: Boolean);
    procedure SetOnCompareRowValuesForCellMerging(Value: TcxGridColumnCompareRowValuesEvent);
    procedure SetOnCompareValuesForCellMerging(Value: TcxGridColumnCompareValuesEvent);
    procedure SetOnCustomDrawFooterCell(Value: TcxGridColumnCustomDrawHeaderEvent);
    procedure SetOnCustomDrawGroupSummaryCell(Value: TcxGridGroupSummaryCellCustomDrawEvent);
    procedure SetOnCustomDrawHeader(Value: TcxGridColumnCustomDrawHeaderEvent);
    procedure SetOnHeaderClick(Value: TNotifyEvent);
    procedure SetOptions(Value: TcxGridColumnOptions);
    procedure SetSelected(Value: Boolean);
  protected
    procedure BestFitApplied(AFireEvents: Boolean); override;
    function CalculateBestFitWidth: Integer; override;
    function GetFixed: Boolean; override;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    procedure DoCustomDrawFooterCell(ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean); virtual;
    procedure DoCustomDrawGroupSummaryCell(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo;
      var ADone: Boolean); virtual;
    procedure DoCustomDrawHeader(ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean); virtual;
    procedure DoHeaderClick; virtual;
    function HasCustomDrawFooterCell: Boolean;
    function HasCustomDrawGroupSummaryCell: Boolean;
    function HasCustomDrawHeader: Boolean;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoCompareValuesForCellMerging(
      ARow1: TcxGridDataRow; AProperties1: TcxCustomEditProperties; const AValue1: TcxEditValue;
      ARow2: TcxGridDataRow; AProperties2: TcxCustomEditProperties; const AValue2: TcxEditValue): Boolean;
    procedure FocusWithSelection; override;

    property IsPreview: Boolean read GetIsPreview write SetIsPreview;
    property Selected: Boolean read GetSelected write SetSelected;
  published
    property Options: TcxGridColumnOptions read GetOptions write SetOptions;
    property OnCompareRowValuesForCellMerging: TcxGridColumnCompareRowValuesEvent read FOnCompareRowValuesForCellMerging write SetOnCompareRowValuesForCellMerging;
    property OnCompareValuesForCellMerging: TcxGridColumnCompareValuesEvent read FOnCompareValuesForCellMerging write SetOnCompareValuesForCellMerging;
    property OnCustomDrawFooterCell: TcxGridColumnCustomDrawHeaderEvent read FOnCustomDrawFooterCell write SetOnCustomDrawFooterCell;
    property OnCustomDrawGroupSummaryCell: TcxGridGroupSummaryCellCustomDrawEvent read FOnCustomDrawGroupSummaryCell write SetOnCustomDrawGroupSummaryCell;
    property OnCustomDrawHeader: TcxGridColumnCustomDrawHeaderEvent read FOnCustomDrawHeader write SetOnCustomDrawHeader;
    property OnHeaderClick: TNotifyEvent read FOnHeaderClick write SetOnHeaderClick;
    property OnInitGroupingDateRanges;
  end;

  // options

  TcxGridTableBackgroundBitmaps = class(TcxCustomGridTableBackgroundBitmaps)
  protected
    function GetBitmapStyleIndex(Index: Integer): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Footer: TBitmap index bbFooter read GetValue write SetValue;
    property Header: TBitmap index bbHeader read GetValue write SetValue;
    property Group: TBitmap index bbGroup read GetValue write SetValue;
    property GroupByBox: TBitmap index bbGroupByBox read GetValue write SetValue;
    property Indicator: TBitmap index bbIndicator read GetValue write SetValue;
    property Preview: TBitmap index bbPreview read GetValue write SetValue;
  end;

  TcxGridTableDateTimeHandling = class(TcxCustomGridTableDateTimeHandling)
  published
    property DateFormat;
    property Grouping;
    property HourFormat;
    property UseLongDateFormat;
    property UseShortTimeFormat;
  end;

  // Navigator

  TcxGridTableViewNavigatorButtons = class(TcxGridViewNavigatorButtons)
  private
    function GetGridView: TcxGridTableView;
  protected
    function GetButtonEnabled(ADefaultIndex: Integer): Boolean; override;
  public
    property GridView: TcxGridTableView read GetGridView;
  end;

  TcxGridTableViewNavigator = class(TcxGridViewNavigator)
  protected
    function GetNavigatorButtonsClass: TcxGridViewNavigatorButtonsClass; override;
  end;

  // behavior

  TcxGridTableShowLockedStateImageOptions = class(TcxCustomGridTableShowLockedStateImageOptions)
  published
    property BestFit;
    property Filtering;
    property Grouping;
    property Sorting;
    property Posting;
  end;

  TcxGridTableOptionsBehavior = class(TcxCustomGridTableOptionsBehavior)
  private
    FColumnHeaderHints: Boolean;
    FColumnMergedGrouping: Boolean;
    FCopyPreviewToClipboard: Boolean;
    FEditMode: TcxGridEditMode;
    FExpandMasterRowOnDblClick: Boolean;
    FFixedGroups: Boolean;

    function GetGridView: TcxGridTableView; inline;
    function GetShowLockedStateImageOptions: TcxGridTableShowLockedStateImageOptions;
    procedure SetColumnHeaderHints(Value: Boolean);
    procedure SetColumnMergedGrouping(Value: Boolean);
    procedure SetCopyPreviewToClipboard(Value: Boolean);
    procedure SetEditMode(AValue: TcxGridEditMode);
    procedure SetExpandMasterRowOnDblClick(Value: Boolean);
    procedure SetFixedGroups(Value: Boolean);
    procedure SetShowLockedStateImageOptions(Value: TcxGridTableShowLockedStateImageOptions);
  protected
    function GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass; override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property RepaintVisibleRecordsOnScroll;

    function IsInplaceEditFormMode: Boolean; virtual;
    function NeedHideCurrentRow: Boolean;

    property GridView: TcxGridTableView read GetGridView;
  published
    property BestFitMaxRecordCount;
    property ColumnHeaderHints: Boolean read FColumnHeaderHints write SetColumnHeaderHints default True;
    property ColumnMergedGrouping: Boolean read FColumnMergedGrouping write SetColumnMergedGrouping default False;
    property CopyPreviewToClipboard: Boolean read FCopyPreviewToClipboard write SetCopyPreviewToClipboard default True;
    property EditAutoHeight;
    property EditMode: TcxGridEditMode read FEditMode write SetEditMode default emInplace;
    property ExpandMasterRowOnDblClick: Boolean read FExpandMasterRowOnDblClick write SetExpandMasterRowOnDblClick default True;
    property FixedGroups: Boolean read FFixedGroups write SetFixedGroups default False;
    property FocusCellOnCycle;
    property ImmediateEditor;
    property RecordScrollMode;
    property ShowLockedStateImageOptions: TcxGridTableShowLockedStateImageOptions
      read GetShowLockedStateImageOptions write SetShowLockedStateImageOptions;
    property PullFocusing;
  end;

  // filter

  TcxGridTableFiltering = class(TcxCustomGridTableFiltering)
  private
    function GetColumnAddValueItems: Boolean;
    function GetColumnExcelPopup: TcxGridItemExcelFilterPopupOptions;
    function GetColumnFilteredItemsList: Boolean;
    function GetColumnMRUItemsList: Boolean;
    function GetColumnMRUItemsListCount: Integer;
    function GetColumnPopup: TcxGridItemFilterPopupOptions;
    function GetColumnPopupMode: TdxFilterPopupWindowMode;
    function GetGridView: TcxGridTableView; inline;
    procedure SetColumnAddValueItems(Value: Boolean);
    procedure SetColumnExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
    procedure SetColumnFilteredItemsList(Value: Boolean);
    procedure SetColumnMRUItemsList(Value: Boolean);
    procedure SetColumnMRUItemsListCount(Value: Integer);
    procedure SetColumnPopup(Value: TcxGridItemFilterPopupOptions);
    procedure SetColumnPopupMode(Value: TdxFilterPopupWindowMode);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function IsFilterBoxEnabled: Boolean; override;
  public
    procedure RunCustomizeDialog(AItem: TcxCustomGridTableItem = nil); override;

    property GridView: TcxGridTableView read GetGridView;
    // obsolete - use ColumnPopup.DropDownWidth
    property ColumnPopupDropDownWidth: Integer read GetItemPopupDropDownWidth write SetItemPopupDropDownWidth;
    property DropDownWidth;
    // obsolete - use ColumnPopup.MaxDropDownItemCount
    property ColumnPopupMaxDropDownItemCount: Integer read GetItemPopupMaxDropDownItemCount write SetItemPopupMaxDropDownItemCount;
    property MaxDropDownCount;
  published
    property ColumnAddValueItems: Boolean read GetColumnAddValueItems write SetColumnAddValueItems default True;
    property ColumnExcelPopup: TcxGridItemExcelFilterPopupOptions read GetColumnExcelPopup write SetColumnExcelPopup;
    property ColumnFilteredItemsList: Boolean read GetColumnFilteredItemsList write SetColumnFilteredItemsList default False;
    property ColumnMRUItemsList: Boolean read GetColumnMRUItemsList write SetColumnMRUItemsList default True;
    property ColumnMRUItemsListCount: Integer read GetColumnMRUItemsListCount write SetColumnMRUItemsListCount default cxGridFilterDefaultItemMRUItemsListCount;
    property ColumnPopup: TcxGridItemFilterPopupOptions read GetColumnPopup write SetColumnPopup;
    property ColumnPopupMode: TdxFilterPopupWindowMode read GetColumnPopupMode write SetColumnPopupMode default fpmDefault;
  end;

  // customize

  TcxGridTableOptionsCustomize = class(TcxCustomGridTableOptionsCustomize)
  private
    FColumnHidingOnGrouping: Boolean;
    FColumnHorzSizing: Boolean;
    FDataRowFixing: Boolean;
    FDataRowSizing: Boolean;
    FGroupBySorting: Boolean;
    FGroupRowSizing: Boolean;
    function GetColumnFiltering: Boolean;
    function GetColumnGrouping: Boolean;
    function GetColumnHiding: Boolean;
    function GetColumnMoving: Boolean;
    function GetColumnSorting: Boolean;
    function GetColumnsQuickCustomization: Boolean;
    function GetColumnsQuickCustomizationMaxDropDownCount: Integer;
    function GetColumnsQuickCustomizationMultiColumnMode: Boolean;
    function GetColumnsQuickCustomizationReordering: TcxGridQuickCustomizationReordering;
    function GetColumnsQuickCustomizationShowCommands: Boolean;
    function GetColumnsQuickCustomizationSorted: Boolean;
    function GetGridView: TcxGridTableView;
    procedure SetColumnFiltering(Value: Boolean);
    procedure SetColumnGrouping(Value: Boolean);
    procedure SetColumnHiding(Value: Boolean);
    procedure SetColumnHidingOnGrouping(Value: Boolean);
    procedure SetColumnHorzSizing(Value: Boolean);
    procedure SetColumnMoving(Value: Boolean);
    procedure SetColumnSorting(Value: Boolean);
    procedure SetColumnsQuickCustomization(Value: Boolean);
    procedure SetColumnsQuickCustomizationMaxDropDownCount(Value: Integer);
    procedure SetColumnsQuickCustomizationMultiColumnMode(Value: Boolean);
    procedure SetColumnsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
    procedure SetColumnsQuickCustomizationShowCommands(Value: Boolean);
    procedure SetColumnsQuickCustomizationSorted(Value: Boolean);
    procedure SetDataRowFixing(Value: Boolean);
    procedure SetDataRowSizing(Value: Boolean);
    procedure SetGroupBySorting(Value: Boolean);
    procedure SetGroupRowSizing(Value: Boolean);
  protected
    property ColumnsQuickCustomizationMultiColumnMode: Boolean read GetColumnsQuickCustomizationMultiColumnMode
      write SetColumnsQuickCustomizationMultiColumnMode default True;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxGridTableView read GetGridView;
  published
    property ColumnFiltering: Boolean read GetColumnFiltering write SetColumnFiltering default True;
    property ColumnGrouping: Boolean read GetColumnGrouping write SetColumnGrouping default True;
    property ColumnHiding: Boolean read GetColumnHiding write SetColumnHiding default False;
    property ColumnHidingOnGrouping: Boolean read FColumnHidingOnGrouping write SetColumnHidingOnGrouping default True;
    property ColumnHorzSizing: Boolean read FColumnHorzSizing write SetColumnHorzSizing default True;
    property ColumnMoving: Boolean read GetColumnMoving write SetColumnMoving default True;
    property ColumnSorting: Boolean read GetColumnSorting write SetColumnSorting default True;
    property ColumnsQuickCustomization: Boolean read GetColumnsQuickCustomization
      write SetColumnsQuickCustomization default False;
    property ColumnsQuickCustomizationMaxDropDownCount: Integer read GetColumnsQuickCustomizationMaxDropDownCount
      write SetColumnsQuickCustomizationMaxDropDownCount default 0;
    property ColumnsQuickCustomizationReordering: TcxGridQuickCustomizationReordering
      read GetColumnsQuickCustomizationReordering write SetColumnsQuickCustomizationReordering default qcrDefault;
    property ColumnsQuickCustomizationShowCommands: Boolean
      read GetColumnsQuickCustomizationShowCommands write SetColumnsQuickCustomizationShowCommands default True;
    property ColumnsQuickCustomizationSorted: Boolean
      read GetColumnsQuickCustomizationSorted write SetColumnsQuickCustomizationSorted default False;
    property DataRowFixing: Boolean read FDataRowFixing write SetDataRowFixing default False;
    property DataRowSizing: Boolean read FDataRowSizing write SetDataRowSizing default False;
    property GroupBySorting: Boolean read FGroupBySorting write SetGroupBySorting default False;
    property GroupRowSizing: Boolean read FGroupRowSizing write SetGroupRowSizing default False;
  end;

  // data

  TcxGridTableOptionsData = class(TcxCustomGridTableOptionsData);

  // selection

  TcxGridMultiSelectMode = (msmStandard, msmPersistent);
  TcxGridCheckBoxPosition = (cbpFirstColumn, cbpIndicator);
  TcxGridCheckBoxVisibilityOption = (cbvDataRow, cbvGroupRow, cbvColumnHeader);
  TcxGridCheckBoxVisibility = set of TcxGridCheckBoxVisibilityOption;

  TcxGridTableOptionsSelection = class(TcxCustomGridTableOptionsSelection)
  private
    FCellMultiSelect: Boolean;
    FCheckBoxPosition: TcxGridCheckBoxPosition;
    FCheckBoxVisibility: TcxGridCheckBoxVisibility;
    FClearPersistentSelectionOnOutsideClick: Boolean;
    FMultiSelectMode: TcxGridMultiSelectMode;
    FShowCheckBoxesDynamically: Boolean;

    function GetGridView: TcxGridTableView;
    procedure SetCellMultiSelect(Value: Boolean);
    procedure SetCheckBoxPosition(Value: TcxGridCheckBoxPosition);
    procedure SetCheckBoxVisibility(Value: TcxGridCheckBoxVisibility);
    procedure SetClearPersistentSelectionOnOutsideClick(Value: Boolean);
    procedure SetMultiSelectMode(Value: TcxGridMultiSelectMode);
    procedure SetShowCheckBoxesDynamically(Value: Boolean);
  protected
    function IsMultiSelectPersistent: Boolean; override;
    procedure SetCellSelect(Value: Boolean); override;
    procedure SetInvertSelect(Value: Boolean); override;
    procedure SetMultiSelect(Value: Boolean); override;

    property GridView: TcxGridTableView read GetGridView;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CellMultiSelect: Boolean read FCellMultiSelect write SetCellMultiSelect default False;
    property CheckBoxPosition: TcxGridCheckBoxPosition read FCheckBoxPosition write SetCheckBoxPosition default cbpFirstColumn;
    property CheckBoxVisibility: TcxGridCheckBoxVisibility read FCheckBoxVisibility write SetCheckBoxVisibility default [];
    property ClearPersistentSelectionOnOutsideClick: Boolean read FClearPersistentSelectionOnOutsideClick
      write SetClearPersistentSelectionOnOutsideClick default False;
    property HideFocusRect;
    property HideFocusRectOnExit;
    property HideSelection;
    property InvertSelect;
    property MultiSelectMode: TcxGridMultiSelectMode read FMultiSelectMode write SetMultiSelectMode default msmStandard;
    property ShowCheckBoxesDynamically: Boolean read FShowCheckBoxesDynamically write SetShowCheckBoxesDynamically default False;
    property UnselectFocusedRecordOnExit;
  end;

  // view

  TcxGridDataRowPinClickAction = (rpcaNone, rpcaFixToTop, rpcaFixToBottom, rpcaShowPopup);
  TcxGridDataRowPinVisibility = (rpvNever, rpvAlways, rpvHotTrack, rpvRowHotTrack);

  TcxGridFixedDataRowsOptionsClass = class of TcxGridFixedDataRowsOptions;

  TcxGridFixedDataRowsOptions = class(TcxCustomGridOptions)
  strict private
    FPinClickAction: TcxGridDataRowPinClickAction;
    FPinSize: TcxSize;
    FPinVisibility: TcxGridDataRowPinVisibility;
    FSeparatorColor: TColor;
    FSeparatorWidth: Integer;

    function DefaultSeparatorColor: TColor;
    function GetGridView: TcxGridTableView;
    procedure SetPinClickAction(AValue: TcxGridDataRowPinClickAction);
    procedure SetPinSize(AValue: TcxSize);
    procedure SetPinVisibility(AValue: TcxGridDataRowPinVisibility);
    procedure SetSeparatorColor(AValue: TColor);
    procedure SetSeparatorWidth(AValue: Integer);

    procedure CreatePinSize;
    procedure DestroyPinSize;
    procedure PinSizeChangeHandler(Sender: TObject);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetSeparatorColor: TColor;

    property GridView: TcxGridTableView read GetGridView;
  published
    property PinClickAction: TcxGridDataRowPinClickAction read FPinClickAction write SetPinClickAction default rpcaShowPopup;
    property PinSize: TcxSize read FPinSize write SetPinSize;
    property PinVisibility: TcxGridDataRowPinVisibility read FPinVisibility write SetPinVisibility default rpvNever;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clDefault;
    property SeparatorWidth: Integer read FSeparatorWidth write SetSeparatorWidth default cxGridCustomRowSeparatorDefaultWidth;
  end;

  { TcxGridSpecialRowOptions }

  TcxGridSpecialRowOptions = class(TcxCustomGridOptions)
  strict private
    FInfoText: string;
    FIsInfoTextAssigned: Boolean;
    FSeparatorColor: TColor;
    FSeparatorWidth: Integer;
    FVisible: Boolean;

    function GetGridView: TcxGridTableView;
    function GetInfoText: string;
    procedure SetInfoText(const Value: string);
    procedure SetSeparatorColor(Value: TColor);
    procedure SetSeparatorWidth(Value: Integer);
    procedure SetVisible(Value: Boolean);
    function IsInfoTextStored: Boolean;
  protected
    procedure ChangeScale(M, D: Integer); override;
    function DefaultInfoText: string; virtual; abstract;
    function DefaultSeparatorColor: TColor; virtual;
    procedure VisibleChanged; virtual; abstract;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetSeparatorColor: TColor;
    property GridView: TcxGridTableView read GetGridView;
  published
    property InfoText: string read GetInfoText write SetInfoText stored IsInfoTextStored;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clDefault;
    property SeparatorWidth: Integer read FSeparatorWidth write SetSeparatorWidth default cxGridCustomRowSeparatorDefaultWidth;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TcxGridFilterRowOptions }

  TcxGridFilterRowApplyChangesMode = (fracOnCellExit, fracImmediately, fracDelayed);

  TcxGridFilterRowOptionsClass = class of TcxGridFilterRowOptions;
  TcxGridFilterRowOptions = class(TcxGridSpecialRowOptions)
  strict private
    FOperatorCustomization: Boolean;
    FApplyChanges: TcxGridFilterRowApplyChangesMode;
    FApplyInputDelay: Cardinal;

    procedure SetApplyChanges(Value: TcxGridFilterRowApplyChangesMode);
    procedure SetApplyInputDelay(Value: Cardinal);
    procedure SetOperatorCustomization(Value: Boolean);
  protected
    function DefaultInfoText: string; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ApplyChanges: TcxGridFilterRowApplyChangesMode read FApplyChanges write SetApplyChanges default fracOnCellExit;
    property ApplyInputDelay: Cardinal read FApplyInputDelay write SetApplyInputDelay default cxGridFilterRowDelayDefault;
    property OperatorCustomization: Boolean read FOperatorCustomization write SetOperatorCustomization default False;
  end;

  { TcxGridNewItemRowOptions }

  TcxGridNewItemRowOptionsClass = class of TcxGridNewItemRowOptions;
  TcxGridNewItemRowOptions = class(TcxGridSpecialRowOptions)
  protected
    function DefaultInfoText: string; override;
    procedure VisibleChanged; override;
  end;

  TcxGridGroupByHeaderLayout = (ghlVerticallyShifted, ghlHorizontal);
  TcxGridGroupFootersMode = (gfInvisible, gfVisibleWhenExpanded, gfAlwaysVisible);
  TcxGridGroupRowStyle = (grsStandard, grsOffice11);
  TcxGridGroupSummaryLayout = (gslStandard, gslAlignWithColumns,
    gslAlignWithColumnsAndDistribute);

  TcxGridTableOptionsView = class(TcxCustomGridTableOptionsView)
  private
    FColumnAutoWidth: Boolean;
    FDataRowHeight: Integer;
    FExpandButtonsForEmptyDetails: Boolean;
    FFooter: Boolean;
    FFooterAutoHeight: Boolean;
    FFooterMultiSummaries: Boolean;
    FGridLineColor: TColor;
    FGridLines: TcxGridLines;
    FGroupByBox: Boolean;
    FGroupByHeaderLayout: TcxGridGroupByHeaderLayout;
    FGroupFooterMultiSummaries: Boolean;
    FGroupFooters: TcxGridGroupFootersMode;
    FGroupRowHeight: Integer;
    FGroupRowStyle: TcxGridGroupRowStyle;
    FGroupSummaryLayout: TcxGridGroupSummaryLayout;
    FHeader: Boolean;
    FHeaderHeight: Integer;
    FIndicator: Boolean;
    FIndicatorWidth: Integer;
    FPrevGroupFooters: TcxGridGroupFootersMode;
    FMergedGroupSeparator: string;
    FRowSeparatorColor: TColor;
    FRowSeparatorWidth: Integer;

    function GetExpandButtonsForEmptyDetails: Boolean;
    function GetGridView: TcxGridTableView;
    function GetHeaderAutoHeight: Boolean;
    function GetHeaderEndEllipsis: Boolean;
    function GetHeaderFilterButtonShowMode: TcxGridItemFilterButtonShowMode;
    function GetNewItemRow: Boolean;
    function GetNewItemRowInfoText: string;
    function GetNewItemRowSeparatorColor: TColor;
    function GetNewItemRowSeparatorWidth: Integer;
    function GetShowColumnFilterButtons: TcxGridShowItemFilterButtons;
    procedure SetColumnAutoWidth(Value: Boolean);
    procedure SetDataRowHeight(Value: Integer);
    procedure SetExpandButtonsForEmptyDetails(Value: Boolean);
    procedure SetHeaderFilterButtonShowMode(Value: TcxGridItemFilterButtonShowMode);
    procedure SetFooter(Value: Boolean);
    procedure SetFooterAutoHeight(Value: Boolean);
    procedure SetFooterMultiSummaries(Value: Boolean);
    procedure SetGridLineColor(Value: TColor);
    procedure SetGridLines(Value: TcxGridLines);
    procedure SetGroupByBox(Value: Boolean);
    procedure SetGroupByHeaderLayout(Value: TcxGridGroupByHeaderLayout);
    procedure SetGroupFooterMultiSummaries(Value: Boolean);
    procedure SetGroupFooters(Value: TcxGridGroupFootersMode);
    procedure SetGroupRowHeight(Value: Integer);
    procedure SetGroupRowStyle(Value: TcxGridGroupRowStyle);
    procedure SetGroupSummaryLayout(Value: TcxGridGroupSummaryLayout);
    procedure SetHeader(Value: Boolean);
    procedure SetHeaderAutoHeight(Value: Boolean);
    procedure SetHeaderEndEllipsis(Value: Boolean);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetIndicator(Value: Boolean);
    procedure SetIndicatorWidth(Value: Integer);
    procedure SetNewItemRow(Value: Boolean);
    procedure SetNewItemRowInfoText(const Value: string);
    procedure SetNewItemRowSeparatorColor(Value: TColor);
    procedure SetNewItemRowSeparatorWidth(Value: Integer);
    procedure SetMergedGroupSeparator(AValue: string);
    procedure SetRowSeparatorColor(Value: TColor);
    procedure SetRowSeparatorWidth(Value: Integer);
    procedure SetShowColumnFilterButtons(Value: TcxGridShowItemFilterButtons);
    procedure ReadNewItemRow(Reader: TReader);
    procedure ReadNewItemRowInfoText(Reader: TReader);
    procedure ReadNewItemRowSeparatorColor(Reader: TReader);
    procedure ReadNewItemRowSeparatorWidth(Reader: TReader);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    function IsMergedGroupSeparatorStored: Boolean; virtual;
    procedure ItemCaptionAutoHeightChanged; override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function CanShowFooterMultiSummaries: Boolean;
    function CanShowGroupFooterMultiSummaries: Boolean;
    procedure CheckDataRowHeight(var AValue: Integer); virtual;
    procedure CheckGroupRowHeight(var AValue: Integer); virtual;
    function GetGridLineColor: TColor; override;
    function GetGroupSummaryLayout: TcxGridGroupSummaryLayout;
    function GetRowSeparatorColor: TColor;

    property GridView: TcxGridTableView read GetGridView;
    // obsolete - use GridView.NewItemRow
    property NewItemRow: Boolean read GetNewItemRow write SetNewItemRow;
    property NewItemRowInfoText: string read GetNewItemRowInfoText write SetNewItemRowInfoText;
    property NewItemRowSeparatorColor: TColor read GetNewItemRowSeparatorColor write SetNewItemRowSeparatorColor;
    property NewItemRowSeparatorWidth: Integer read GetNewItemRowSeparatorWidth write SetNewItemRowSeparatorWidth;
    property PrevGroupFooters: TcxGridGroupFootersMode read FPrevGroupFooters;
  published
    property CellAutoHeight;
    property CellTextMaxLineCount;
    property ColumnAutoWidth: Boolean read FColumnAutoWidth write SetColumnAutoWidth default False;
    property DataRowHeight: Integer read FDataRowHeight write SetDataRowHeight default 0;
    property EditAutoHeightBorderColor;
    property ExpandButtonsForEmptyDetails: Boolean read GetExpandButtonsForEmptyDetails write SetExpandButtonsForEmptyDetails default True;
    property Footer: Boolean read FFooter write SetFooter default False;
    property FooterAutoHeight: Boolean read FFooterAutoHeight write SetFooterAutoHeight default False;
    property FooterMultiSummaries: Boolean read FFooterMultiSummaries write SetFooterMultiSummaries default False;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLines: TcxGridLines read FGridLines write SetGridLines default glBoth;
    property GroupByBox: Boolean read FGroupByBox write SetGroupByBox default True;
    property GroupByHeaderLayout: TcxGridGroupByHeaderLayout read FGroupByHeaderLayout write SetGroupByHeaderLayout default ghlVerticallyShifted;
    property GroupFooterMultiSummaries: Boolean read FGroupFooterMultiSummaries write SetGroupFooterMultiSummaries default False;
    property GroupFooters: TcxGridGroupFootersMode read FGroupFooters write SetGroupFooters default gfInvisible;
    property GroupRowHeight: Integer read FGroupRowHeight write SetGroupRowHeight default 0;
    property GroupRowStyle: TcxGridGroupRowStyle read FGroupRowStyle write SetGroupRowStyle default grsStandard;
    property GroupSummaryLayout: TcxGridGroupSummaryLayout read FGroupSummaryLayout write SetGroupSummaryLayout default gslStandard;
    property Header: Boolean read FHeader write SetHeader default True;
    property HeaderAutoHeight: Boolean read GetHeaderAutoHeight write SetHeaderAutoHeight default False;
    property HeaderEndEllipsis: Boolean read GetHeaderEndEllipsis write SetHeaderEndEllipsis default False;
    property HeaderFilterButtonShowMode: TcxGridItemFilterButtonShowMode read GetHeaderFilterButtonShowMode write SetHeaderFilterButtonShowMode default fbmDefault;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 0;
    property Indicator: Boolean read FIndicator write SetIndicator default False;
    property IndicatorWidth: Integer read FIndicatorWidth write SetIndicatorWidth default cxGridDefaultIndicatorWidth;
    property MergedGroupSeparator: string read FMergedGroupSeparator write SetMergedGroupSeparator stored IsMergedGroupSeparatorStored;
    property RowSeparatorColor: TColor read FRowSeparatorColor write SetRowSeparatorColor default clDefault;
    property RowSeparatorWidth: Integer read FRowSeparatorWidth write SetRowSeparatorWidth default 0;
    property ShowColumnFilterButtons: TcxGridShowItemFilterButtons read GetShowColumnFilterButtons write SetShowColumnFilterButtons default sfbDefault;
  end;

  // preview

  { TcxGridPreview }

  TcxGridPreviewPlace = (ppBottom, ppTop);

  TcxGridPreviewClass = class of TcxGridPreview;
  TcxGridPreview = class(TcxCustomGridOptions)
  strict private
    FAutoHeight: Boolean;
    FColumn: TcxGridColumn;
    FLeftIndent: Integer;
    FMaxLineCount: Integer;
    FPlace: TcxGridPreviewPlace;
    FRightIndent: Integer;
    FVisible: Boolean;

    function GetActive: Boolean;
    function GetGridView: TcxGridTableView;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetColumn(Value: TcxGridColumn);
    procedure SetLeftIndent(Value: Integer);
    procedure SetMaxLineCount(Value: Integer);
    procedure SetPlace(Value: TcxGridPreviewPlace);
    procedure SetRightIndent(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function GetFixedHeight: Integer;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure PropertyChanged;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;

    property Active: Boolean read GetActive;
    property GridView: TcxGridTableView read GetGridView;
  published
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Column: TcxGridColumn read FColumn write SetColumn;
    property LeftIndent: Integer read FLeftIndent write SetLeftIndent default cxGridPreviewDefaultLeftIndent;
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount default cxGridPreviewDefaultMaxLineCount;
    property Place: TcxGridPreviewPlace read FPlace write SetPlace default ppBottom;
    property RightIndent: Integer read FRightIndent write SetRightIndent default cxGridPreviewDefaultRightIndent;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  // styles

  TcxGridGetGroupStyleEvent = procedure(Sender: TcxGridTableView; ARecord: TcxCustomGridRecord;
    ALevel: Integer; {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AStyle: TcxStyle) of object;

  TcxGridTableViewStyles = class(TcxCustomGridTableViewStyles)
  private
    FProcessingGroupSortedSummary: Boolean;
    FOnGetFooterStyle: TcxGridGetCellStyleEvent;
    FOnGetFooterStyleEx: TcxGridGetFooterStyleExEvent;
    FOnGetFooterSummaryStyle: TcxGridGetFooterSummaryStyleEvent;
    FOnGetGroupStyle: TcxGridGetGroupStyleEvent;
    FOnGetGroupSummaryStyle: TcxGridGetGroupSummaryStyleEvent;
    FOnGetHeaderStyle: TcxGridGetHeaderStyleEvent;
    FOnGetInplaceEditFormGroupStyle: TcxGridGetCellStyleEvent;
    FOnGetInplaceEditFormItemStyle: TcxGridGetCellStyleEvent;
    FOnGetPreviewStyle: TcxGridGetCellStyleEvent;

    function GetGridViewValue: TcxGridTableView;
    procedure SetOnGetFooterStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetFooterStyleEx(Value: TcxGridGetFooterStyleExEvent);
    procedure SetOnGetFooterSummaryStyle(Value: TcxGridGetFooterSummaryStyleEvent);
    procedure SetOnGetGroupStyle(Value: TcxGridGetGroupStyleEvent);
    procedure SetOnGetGroupSummaryStyle(Value: TcxGridGetGroupSummaryStyleEvent);
    procedure SetOnGetHeaderStyle(Value: TcxGridGetHeaderStyleEvent);
    procedure SetOnGetInplaceEditFormItemStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetPreviewStyle(Value: TcxGridGetCellStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetCellContentParams(ARecord: TcxCustomGridRecord; AItem: TObject;
      out AParams: TcxViewParams); override;
    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); override;
    procedure GetFooterCellParams(ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
      AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams); virtual;
    procedure GetFooterParams(ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
      AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams); virtual;
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; ALevel: Integer;
      out AParams: TcxViewParams); virtual;
    procedure GetGroupSummaryCellContentParams(ARow: TcxGridGroupRow;
      ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams); virtual;
    procedure GetGroupSummaryCellParams(ARow: TcxGridGroupRow;
      ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams); virtual;
    procedure GetGroupSummaryParams(ARow: TcxGridGroupRow; ASummaryItem: TcxDataSummaryItem;
      out AParams: TcxViewParams); virtual;
    procedure GetHeaderParams(AItem: TcxGridColumn; out AParams: TcxViewParams); virtual;
    procedure GetInplaceEditFormGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetInplaceEditFormItemHottrackParams(AItem: TcxCustomGridTableItem; out AParams: TcxViewParams); virtual;
    procedure GetInplaceEditFormItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetPreviewParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetRecordContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); override;

    property GridView: TcxGridTableView read GetGridViewValue;
  published
    property FilterRowInfoText: TcxStyle index vsFilterRowInfoText read GetValue write SetValue;
    property Footer: TcxStyle index vsFooter read GetValue write SetValue;
    property Group: TcxStyle index vsGroup read GetValue write SetValue;
    property GroupByBox: TcxStyle index vsGroupByBox read GetValue write SetValue;
    property GroupFooterSortedSummary: TcxStyle index vsGroupFooterSortedSummary read GetValue write SetValue;
    property GroupSortedSummary: TcxStyle index vsGroupSortedSummary read GetValue write SetValue;
    property GroupSummary: TcxStyle index vsGroupSummary read GetValue write SetValue;
    property Header: TcxStyle index vsHeader read GetValue write SetValue;
    property Inactive;
    property Indicator: TcxStyle index vsIndicator read GetValue write SetValue;
    property InplaceEditFormGroup: TcxStyle index vsInplaceEditFormGroup read GetValue write SetValue;
    property InplaceEditFormItem: TcxStyle index vsInplaceEditFormItem read GetValue write SetValue;
    property InplaceEditFormItemHotTrack: TcxStyle index vsInplaceEditFormItemHotTrack read GetValue write SetValue;
    property NewItemRowInfoText: TcxStyle index vsNewItemRowInfoText read GetValue write SetValue;
    property Preview: TcxStyle index vsPreview read GetValue write SetValue;
    property Selection;
    property StyleSheet;
    property OnGetFooterStyle: TcxGridGetCellStyleEvent read FOnGetFooterStyle write SetOnGetFooterStyle;
    property OnGetFooterStyleEx: TcxGridGetFooterStyleExEvent read FOnGetFooterStyleEx write SetOnGetFooterStyleEx;
    property OnGetFooterSummaryStyle: TcxGridGetFooterSummaryStyleEvent read FOnGetFooterSummaryStyle write SetOnGetFooterSummaryStyle;
    property OnGetGroupStyle: TcxGridGetGroupStyleEvent read FOnGetGroupStyle write SetOnGetGroupStyle;
    property OnGetGroupSummaryStyle: TcxGridGetGroupSummaryStyleEvent read FOnGetGroupSummaryStyle write SetOnGetGroupSummaryStyle;
    property OnGetHeaderStyle: TcxGridGetHeaderStyleEvent read FOnGetHeaderStyle write SetOnGetHeaderStyle;
    property OnGetInplaceEditFormGroupStyle: TcxGridGetCellStyleEvent read FOnGetInplaceEditFormGroupStyle
      write FOnGetInplaceEditFormGroupStyle;
    property OnGetInplaceEditFormItemStyle: TcxGridGetCellStyleEvent read FOnGetInplaceEditFormItemStyle
      write SetOnGetInplaceEditFormItemStyle;
    property OnGetPreviewStyle: TcxGridGetCellStyleEvent read FOnGetPreviewStyle write SetOnGetPreviewStyle;
  end;

  TcxGridTableViewStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxGridTableViewStyles;
    procedure SetStylesValue(Value: TcxGridTableViewStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxGridTableViewStyles read GetStylesValue write SetStylesValue;
  end;

  // grid view

  TcxGridTableSummaryGroupItemLink = class(TcxDataSummaryGroupItemLink,
    IcxStoredObject)
  private
    function GetColumn: TcxGridColumn;
    procedure SetColumn(Value: TcxGridColumn);
    function GetGridView: TcxGridTableView;
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    property GridView: TcxGridTableView read GetGridView;
  published
    property Column: TcxGridColumn read GetColumn write SetColumn;
  end;

  IcxGridSummaryItem = interface
    ['{6F9A0C3E-E33F-4E77-9357-82F1D19CDB67}']
    function GetDisplayText: string;
    function GetVisibleForCustomization: Boolean;
    property DisplayText: string read GetDisplayText;
    property VisibleForCustomization: Boolean read GetVisibleForCustomization;
  end;

  TcxGridTableSummaryItem = class(TcxDataSummaryItem,
    IcxStoredObject, IcxGridSummaryItem)
  private
    FDisplayText: string;
    FVisibleForCustomization: Boolean;
    function GetColumn: TcxGridColumn;
    function GetGridView: TcxGridTableView;
    procedure SetColumn(Value: TcxGridColumn);
    procedure SetDisplayText(const Value: string);
    procedure SetVisibleForCustomization(Value: Boolean);
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxGridSummaryItem
    function GetDisplayText: string;
    function GetVisibleForCustomization: Boolean;

    property GridView: TcxGridTableView read GetGridView;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Column: TcxGridColumn read GetColumn write SetColumn;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property Sorted;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
  end;

  TcxGridColumnEvent = procedure(Sender: TcxGridTableView; AColumn: TcxGridColumn) of object;
  TcxGridIndicatorCellCustomDrawEvent = procedure(Sender: TcxGridTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean) of object;
  TcxGridGroupRowChangingEvent = procedure(Sender: TcxGridTableView; AGroup: TcxGridGroupRow; var AAllow: Boolean) of object;
  TcxGridGroupRowEvent = procedure(Sender: TcxGridTableView; AGroup: TcxGridGroupRow) of object;
  TcxGridGetUnmergeableColumnsEvent = procedure(Sender: TcxGridTableView; AColumn: TcxGridColumn; AUnmergeableColumns: TList) of object;

  TcxGridTableView = class(TcxCustomGridTableView,
    IdxLayoutContainerOwner)
  private
    FAllowCellMerging: Boolean;
    FAssigningIsChildInMergedGroupItems: TcxGridOpenTableItemList;
    FEditForm: TcxGridEditFormOptions;
    FFilterRow: TcxGridFilterRowOptions;
    FFixedDataRows: TcxGridFixedDataRowsOptions;
    FInplaceEditForm: TcxGridTableViewInplaceEditForm;
    FInplaceEditFormLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel;
    FNewItemRow: TcxGridNewItemRowOptions;
    FPreview: TcxGridPreview;

    FOnColumnHeaderClick: TcxGridColumnEvent;
    FOnColumnPosChanged: TcxGridColumnEvent;
    FOnColumnSizeChanged: TcxGridColumnEvent;
    FOnCustomDrawColumnHeader: TcxGridColumnCustomDrawHeaderEvent;
    FOnCustomDrawFooterCell: TcxGridColumnCustomDrawHeaderEvent;
    FOnCustomDrawGroupCell: TcxGridTableCellCustomDrawEvent;
    FOnCustomDrawGroupSummaryCell: TcxGridGroupSummaryCellCustomDrawEvent;
    FOnCustomDrawIndicatorCell: TcxGridIndicatorCellCustomDrawEvent;
    FOnGetUnmergeableColumns: TcxGridGetUnmergeableColumnsEvent;
    FOnGroupRowCollapsed: TcxGridGroupRowEvent;
    FOnGroupRowCollapsing: TcxGridGroupRowChangingEvent;
    FOnGroupRowExpanded: TcxGridGroupRowEvent;
    FOnGroupRowExpanding: TcxGridGroupRowChangingEvent;
    FOnLeftPosChanged: TNotifyEvent;

    function GetBackgroundBitmaps: TcxGridTableBackgroundBitmaps;
    function GetColumn(Index: Integer): TcxGridColumn;
    function GetColumnCount: Integer;
    function GetController: TcxGridTableController;
    function GetDataController: TcxGridDataController;
    function GetDateTimeHandling: TcxGridTableDateTimeHandling;
    function GetEditForm: TcxGridEditFormOptions;
    function GetFiltering: TcxGridTableFiltering;
    function GetGroupedColumn(Index: Integer): TcxGridColumn;
    function GetGroupedColumnCount: Integer;
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
    function GetOptionsBehavior: TcxGridTableOptionsBehavior;
    function GetOptionsCustomize: TcxGridTableOptionsCustomize;
    function GetOptionsData: TcxGridTableOptionsData;
    function GetOptionsSelection: TcxGridTableOptionsSelection;
    function GetOptionsView: TcxGridTableOptionsView;
    function GetPainter: TcxGridTablePainter;
    function GetStyles: TcxGridTableViewStyles;
    function GetViewData: TcxGridViewData;
    function GetViewInfo: TcxGridTableViewInfo;
    function GetVisibleColumn(Index: Integer): TcxGridColumn;
    function GetVisibleColumnCount: Integer;
    procedure SetBackgroundBitmaps(Value: TcxGridTableBackgroundBitmaps);
    procedure SetColumn(Index: Integer; Value: TcxGridColumn);
    procedure SetDataController(Value: TcxGridDataController);
    procedure SetDateTimeHandling(Value: TcxGridTableDateTimeHandling);
    procedure SetEditForm(AValue: TcxGridEditFormOptions);
    procedure SetFiltering(Value: TcxGridTableFiltering);
    procedure SetFilterRow(Value: TcxGridFilterRowOptions);
    procedure SetFixedDataRows(Value: TcxGridFixedDataRowsOptions);
    procedure SetNewItemRow(Value: TcxGridNewItemRowOptions);
    procedure SetOnColumnHeaderClick(Value: TcxGridColumnEvent);
    procedure SetOnColumnPosChanged(Value: TcxGridColumnEvent);
    procedure SetOnColumnSizeChanged(Value: TcxGridColumnEvent);
    procedure SetOnCustomDrawColumnHeader(Value: TcxGridColumnCustomDrawHeaderEvent);
    procedure SetOnCustomDrawFooterCell(Value: TcxGridColumnCustomDrawHeaderEvent);
    procedure SetOnCustomDrawGroupCell(Value: TcxGridTableCellCustomDrawEvent);
    procedure SetOnCustomDrawGroupSummaryCell(Value: TcxGridGroupSummaryCellCustomDrawEvent);
    procedure SetOnCustomDrawIndicatorCell(Value: TcxGridIndicatorCellCustomDrawEvent);
    procedure SetOnGetUnmergeableColumns(Value: TcxGridGetUnmergeableColumnsEvent);
    procedure SetOnLeftPosChanged(Value: TNotifyEvent);
    procedure SetOptionsBehavior(Value: TcxGridTableOptionsBehavior);
    procedure SetOptionsCustomize(Value: TcxGridTableOptionsCustomize);
    procedure SetOptionsData(Value: TcxGridTableOptionsData);
    procedure SetOptionsSelection(Value: TcxGridTableOptionsSelection);
    procedure SetOptionsView(Value: TcxGridTableOptionsView);
    procedure SetPreview(Value: TcxGridPreview);
    procedure SetStyles(Value: TcxGridTableViewStyles);

    // IdxLayoutContainer
    function IdxLayoutContainerOwner.GetContainer = GetLayoutContainer;
    function GetLayoutContainer: TdxLayoutContainer;
  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    procedure GetStoredChildren(AChildren: TStringList); override;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    procedure BeforeEditLayout(ALayoutView: TcxCustomGridView); override;
    function GetLayoutCustomizationFormButtonCaption: string; override;
    // IdxAdornerTargetElementCollection
    procedure AddAdornerTargetColumns(AList: TStrings); virtual;
    procedure GetAdornerTargetElements(AList: TStrings); override;

    procedure CreateHandlers; override;
    procedure DestroyHandlers; override;

    procedure CreateOptions; override;
    procedure DestroyOptions; override;

    function CreateInplaceEditFormLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel; virtual;
    procedure Init; override;
    procedure UpdateInplaceEditFormStyles(ARecord: TcxCustomGridRecord;
      var ALayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel);
    procedure LookAndFeelChanged; override;

    procedure AfterRestoring; override;
    procedure BeforeRestoring; override;

    procedure AfterAssign(ASource: TcxCustomGridView); override;
    procedure AssignEditingRecord; override;
    function CanCellMerging: Boolean; virtual;
    function CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean; override;
    function CanOffsetHorz: Boolean; virtual;
    function CanShowInplaceEditForm: Boolean; virtual;
    procedure DetailDataChanged(ADetail: TcxCustomGridView); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure DoItemsAssigned; override;
    procedure DoAfterAssignItems; override;
    procedure DoBeforeAssignItems; override;
    procedure DoMakeMasterGridRecordVisible; override;
    procedure DoStylesChanged; override;
    function GetInplaceEditFormClientBounds: TRect; virtual;
    function GetInplaceEditFormClientRect: TRect; virtual;
    function GetIsControlFocused: Boolean; override;
    procedure GetItemsListForClipboard(AItems: TList; ACopyAll: Boolean); override;
    function GetResizeOnBoundsChange: Boolean; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;
    function HasCellMerging: Boolean;
    function IsCheckBoxSelectionSupported: Boolean; virtual;
    function IsDataRowFixingSupported: Boolean; virtual;
    function IsFixedGroupsMode: Boolean; virtual;
    function IsMergedGroupsSupported: Boolean; virtual;
    function IsPersistentMultiSelectSupported: Boolean; virtual;
    function IsPreviewHeightFixed: Boolean;
    function IsRecordHeightDependsOnData: Boolean; override;
    procedure Loaded; override;
    function NeedChangeLayoutOnSelectionChanged(AInfo: TcxSelectionChangedInfo): Boolean; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure UpdateData(AInfo: TcxUpdateControlInfo); override;
    procedure UpdateFocusedRecord(AInfo: TcxUpdateControlInfo); override;
    procedure UpdateInplaceEditForm(AInfo: TcxUpdateControlInfo); virtual;
    function UpdateOnDetailDataChange(ADetail: TcxCustomGridView): Boolean; virtual;

    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; override;
    function GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass; override;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; override;
    function GetFilterRowOptionsClass: TcxGridFilterRowOptionsClass; virtual;
    function GetFixedDataRowsOptionsClass: TcxGridFixedDataRowsOptionsClass; virtual;
    function GetInplaceEditFormClass: TcxGridTableViewInplaceEditFormClass; virtual;
    function GetNavigatorClass: TcxGridViewNavigatorClass; override;
    function GetNewItemRowOptionsClass: TcxGridNewItemRowOptionsClass; virtual;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetPreviewClass: TcxGridPreviewClass; virtual;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;

    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;

    function GetItemClass: TcxCustomGridTableItemClass; override;
    procedure ItemVisibilityChanged(AItem: TcxCustomGridTableItem; Value: Boolean); override;

    function CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
      ACellViewInfo: TcxGridTableCellViewInfo): Boolean; override;

    procedure DoColumnHeaderClick(AColumn: TcxGridColumn); virtual;
    procedure DoColumnPosChanged(AColumn: TcxGridColumn); virtual;
    procedure DoColumnSizeChanged(AColumn: TcxGridColumn); virtual;
    procedure DoCustomDrawColumnHeader(ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawFooterCell(ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawGroupCell(ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawGroupSummaryCell(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawIndicatorCell(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean); virtual;
    procedure DoGetUnmergeableColumns(AColumn: TcxGridColumn; AUnmergeableColumns: TList); virtual;
    procedure DoLeftPosChanged; virtual;
    function HasCustomDrawColumnHeader: Boolean;
    function HasCustomDrawFooterCell: Boolean;
    function HasCustomDrawGroupCell: Boolean;
    function HasCustomDrawGroupSummaryCell: Boolean;
    function HasCustomDrawIndicatorCell: Boolean;

    procedure DoGroupRowCollapsed(AGroup: TcxGridGroupRow); virtual;
    function DoGroupRowCollapsing(AGroup: TcxGridGroupRow): Boolean; virtual;
    procedure DoGroupRowExpanded(AGroup: TcxGridGroupRow); virtual;
    function DoGroupRowExpanding(AGroup: TcxGridGroupRow): Boolean; virtual;

    property AllowCellMerging: Boolean read FAllowCellMerging write FAllowCellMerging;
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
    property InplaceEditFormLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel read FInplaceEditFormLayoutLookAndFeel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    function CreateColumn: TcxGridColumn;
    function MasterRowDblClickAction: TcxGridMasterRowDblClickAction; virtual;
    function IsEqualHeightRecords: Boolean; override;
    function IsInplaceEditFormMode: Boolean; virtual;

    function UseRestHeightForDetails: Boolean;

    // for extended lookup edit
    class function CanBeLookupList: Boolean; override;

    property AssigningIsChildInMergedGroupItems: TcxGridOpenTableItemList read FAssigningIsChildInMergedGroupItems;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxGridColumn read GetColumn write SetColumn;
    property Controller: TcxGridTableController read GetController;
    property GroupedColumnCount: Integer read GetGroupedColumnCount;
    property GroupedColumns[Index: Integer]: TcxGridColumn read GetGroupedColumn;
    property Painter: TcxGridTablePainter read GetPainter;
    property ViewData: TcxGridViewData read GetViewData;
    property ViewInfo: TcxGridTableViewInfo read GetViewInfo;
    property VisibleColumnCount: Integer read GetVisibleColumnCount;
    property VisibleColumns[Index: Integer]: TcxGridColumn read GetVisibleColumn;
  published
    property BackgroundBitmaps: TcxGridTableBackgroundBitmaps read GetBackgroundBitmaps write SetBackgroundBitmaps;
    property DataController: TcxGridDataController read GetDataController write SetDataController;
    property DateTimeHandling: TcxGridTableDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property EditForm: TcxGridEditFormOptions read GetEditForm write SetEditForm;
    property Filtering: TcxGridTableFiltering read GetFiltering write SetFiltering;
    property FilterRow: TcxGridFilterRowOptions read FFilterRow write SetFilterRow;
    property FixedDataRows: TcxGridFixedDataRowsOptions read FFixedDataRows write SetFixedDataRows;
    property Images;
    property NewItemRow: TcxGridNewItemRowOptions read FNewItemRow write SetNewItemRow;
    property OptionsBehavior: TcxGridTableOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxGridTableOptionsCustomize read GetOptionsCustomize write SetOptionsCustomize;
    property OptionsData: TcxGridTableOptionsData read GetOptionsData write SetOptionsData;
    property OptionsSelection: TcxGridTableOptionsSelection read GetOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxGridTableOptionsView read GetOptionsView write SetOptionsView;
    property Preview: TcxGridPreview read FPreview write SetPreview;
    property Styles: TcxGridTableViewStyles read GetStyles write SetStyles ;

    property OnColumnHeaderClick: TcxGridColumnEvent read FOnColumnHeaderClick write SetOnColumnHeaderClick;
    property OnColumnPosChanged: TcxGridColumnEvent read FOnColumnPosChanged write SetOnColumnPosChanged;
    property OnColumnSizeChanged: TcxGridColumnEvent read FOnColumnSizeChanged write SetOnColumnSizeChanged;
    property OnCustomDrawColumnHeader: TcxGridColumnCustomDrawHeaderEvent read FOnCustomDrawColumnHeader write SetOnCustomDrawColumnHeader;
    property OnCustomDrawFooterCell: TcxGridColumnCustomDrawHeaderEvent read FOnCustomDrawFooterCell write SetOnCustomDrawFooterCell;
    property OnCustomDrawGroupCell: TcxGridTableCellCustomDrawEvent read FOnCustomDrawGroupCell write SetOnCustomDrawGroupCell;
    property OnCustomDrawGroupSummaryCell: TcxGridGroupSummaryCellCustomDrawEvent read FOnCustomDrawGroupSummaryCell write SetOnCustomDrawGroupSummaryCell;
    property OnCustomDrawIndicatorCell: TcxGridIndicatorCellCustomDrawEvent read FOnCustomDrawIndicatorCell write SetOnCustomDrawIndicatorCell;
    property OnCustomization;

    property OnGroupRowCollapsed: TcxGridGroupRowEvent read FOnGroupRowCollapsed write FOnGroupRowCollapsed;
    property OnGroupRowCollapsing: TcxGridGroupRowChangingEvent read FOnGroupRowCollapsing write FOnGroupRowCollapsing;
    property OnGroupRowExpanded: TcxGridGroupRowEvent read FOnGroupRowExpanded write FOnGroupRowExpanded;
    property OnGroupRowExpanding: TcxGridGroupRowChangingEvent read FOnGroupRowExpanding write FOnGroupRowExpanding;

    property OnGetUnmergeableColumns: TcxGridGetUnmergeableColumnsEvent read FOnGetUnmergeableColumns write SetOnGetUnmergeableColumns;
    property OnInitGroupingDateRanges;
    property OnLeftPosChanged: TNotifyEvent read FOnLeftPosChanged write SetOnLeftPosChanged;
  end;

  { TcxGridColumnAccess }

  TcxGridColumnAccess = class
  public
    class function CanCellMerging(AInstance: TcxGridColumn): Boolean;
    class procedure DoCustomDrawGroupSummaryCell(AInstance: TcxGridColumn;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean); virtual;
    class function HasCustomDrawGroupSummaryCell(AInstance: TcxGridColumn): Boolean;
    class function GetImageComboBoxProperties(AInstance: TcxGridColumn): TcxImageComboBoxProperties;
    class function ShowGroupValuesWithImages(AInstance: TcxGridColumn): Boolean;
  end;

  { TcxGridTableViewAccess }

  TcxGridTableViewAccess = class
  public
    class procedure DoColumnPosChanged(AInstance: TcxGridTableView;
      AColumn: TcxGridColumn);
    class procedure DoCustomDrawGroupCell(AInstance: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean);
    class procedure DoCustomDrawGroupSummaryCell(AInstance: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean); virtual;
    class function GetInplaceEditForm(AInstance: TcxGridTableView): TcxGridTableViewInplaceEditForm;
    class function GetFilterRowOperatorMenu(AInstance: TcxGridTableView): TcxFilterDropDownMenu;
    class function HasCustomDrawGroupCell(AInstance: TcxGridTableView): Boolean;
    class function HasCustomDrawGroupSummaryCell(AInstance: TcxGridTableView): Boolean;
    class function IsMultiSelectPersistent(AInstance: TcxGridTableView): Boolean; static;
  end;

implementation

uses
  RTLConsts, SysUtils, Math, Contnrs, Dialogs, dxOffice11, cxVariants, cxDataUtils,
  cxFilterControlUtils, cxLibraryConsts, cxGridRows, cxGridStrs, cxNavigator, dxDPIAwareUtils;

const
  GroupByBoxLeftOffset = 6;
  GroupByBoxTopOffset = 8;
  GroupByBoxHorOffset = 4;
  GroupByBoxLineVerOffset = 4;
  GroupByBoxColumnWidth = 100;
  GroupByBoxLineWidth = 1;
  GroupByBoxLineColor = clBtnText;
  HScrollDelta = 10;

  FooterSeparatorWidth = 1;

  TopIndexNone = -2;
  RowIndexNone = -1;

  ColumnHeaderHitTestCodes = [htColumnHeader];

  ColumnHeaderMovingZoneSize = 15;

  ColumnSizingMarkWidth = 1;
  RowSizingMarkWidth = 1;

  ScrollHotZoneWidth = 15;

type
  TcxGridSiteAccess = class(TcxGridSite);
  TcxCustomGridAccess = class(TcxCustomGrid);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);

{ TcxCustomGridColumnHitTest }

procedure TcxCustomGridColumnHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxCustomGridColumnHitTest then
  begin
    Column := TcxCustomGridColumnHitTest(Source).Column;
    ColumnContainerKind := TcxCustomGridColumnHitTest(Source).ColumnContainerKind;
  end;
end;

{ TcxGridGroupByBoxHitTest }

class function TcxGridGroupByBoxHitTest.GetHitTestCode: Integer;
begin
  Result := htGroupByBox;
end;

{ TcxGridColumnHeaderHitTest }

class function TcxGridColumnHeaderHitTest.GetHitTestCode: Integer;
begin
  Result := htColumnHeader;
end;

function TcxGridColumnHeaderHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  if Column.CanMove then
    Result := TcxGridTableView(GridView).Controller.GetColumnHeaderDragAndDropObjectClass
  else
    Result := nil;
end;

{ TcxGridColumnHeaderHorzSizingEdgeHitTest }

class function TcxGridColumnHeaderHorzSizingEdgeHitTest.GetHitTestCode: Integer;
begin
  Result := htColumnHeaderHorzSizingEdge;
end;

function TcxGridColumnHeaderHorzSizingEdgeHitTest.Cursor: TCursor;
begin
  Result := crcxGridHorzSize;
end;

function TcxGridColumnHeaderHorzSizingEdgeHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := TcxGridColumnHorzSizingObject;
end;

{ TcxGridColumnHeaderFilterButtonHitTest }

class function TcxGridColumnHeaderFilterButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htColumnHeaderFilterButton;
end;

{ TcxGridHeaderHitTest }

class function TcxGridHeaderHitTest.GetHitTestCode: Integer;
begin
  Result := htHeader;
end;

{ TcxGridFooterHitTest }

class function TcxGridFooterHitTest.GetHitTestCode: Integer;
begin
  Result := htFooter;
end;

{ TcxGridFooterCellHitTest }

procedure TcxGridFooterCellHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridFooterCellHitTest then
    SummaryItem := TcxGridFooterCellHitTest(Source).SummaryItem;
end;

class function TcxGridFooterCellHitTest.GetHitTestCode: Integer;
begin
  Result := htFooterCell;
end;

{ TcxGridGroupFooterHitTest }

class function TcxGridGroupFooterHitTest.GetHitTestCode: Integer;
begin
  Result := htGroupFooter;
end;

{ TcxGridGroupFooterCellHitTest }

class function TcxGridGroupFooterCellHitTest.GetHitTestCode: Integer;
begin
  Result := htGroupFooterCell;
end;

{ TcxGridRowIndicatorHitTest }

procedure TcxGridRowIndicatorHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridRowIndicatorHitTest then
    MultiSelect := TcxGridRowIndicatorHitTest(Source).MultiSelect;
end;

class function TcxGridRowIndicatorHitTest.GetHitTestCode: Integer;
begin
  Result := htRowIndicator;
end;

function TcxGridRowIndicatorHitTest.UseSelectRowCursor: Boolean;
begin
  Result := MultiSelect and ((GridView.OptionsSelection.CheckBoxPosition <> cbpIndicator) or
    (GridView.OptionsSelection.CheckBoxVisibility = []));
end;

function TcxGridRowIndicatorHitTest.Cursor: TCursor;
begin
  if UseSelectRowCursor then
    Result := crcxGridSelectRow
  else
    Result := inherited Cursor;
end;

function TcxGridRowIndicatorHitTest.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridRowIndicatorHitTest.SetGridView(AValue: TcxGridTableView);
begin
  inherited GridView := AValue
end;

{ TcxGridRowSizingEdgeHitTest }

class function TcxGridRowSizingEdgeHitTest.GetHitTestCode: Integer;
begin
  Result := htRowSizingEdge;
end;

function TcxGridRowSizingEdgeHitTest.Cursor: TCursor;
begin
  Result := crcxGridVertSize;
end;

function TcxGridRowSizingEdgeHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := TcxGridRowSizingObject;
end;

{ TcxGridIndicatorHitTest }

class function TcxGridIndicatorHitTest.GetHitTestCode: Integer;
begin
  Result := htIndicator;
end;

{ TcxGridIndicatorHeaderHitTest }

class function TcxGridIndicatorHeaderHitTest.GetHitTestCode: Integer;
begin
  Result := htIndicatorHeader;
end;

{ TcxGridRowLevelIndentHitTest }

class function TcxGridRowLevelIndentHitTest.GetHitTestCode: Integer;
begin
  Result := htRowLevelIndent;
end;

class function TcxGridRowLevelIndentHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridGroupSummaryHitTest }

procedure TcxGridGroupSummaryHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridGroupSummaryHitTest then
    SummaryItem := TcxGridGroupSummaryHitTest(Source).SummaryItem;
end;

function TcxGridGroupSummaryHitTest.GetColumn: TcxGridColumn;
begin
  if SummaryItem = nil then
    Result := nil
  else
    Result := SummaryItem.ItemLink as TcxGridColumn;
end;

class function TcxGridGroupSummaryHitTest.GetHitTestCode: Integer;
begin
  Result := htGroupSummary;
end;

{ TcxGridDataRowFixingMenu }

procedure TcxGridDataRowFixingMenu.Popup(ADataRow: TcxGridDataRow; AForBounds: TRect);
begin
  FDataRow := ADataRow;
  RecreateOperations;
  PopupForBounds(AForBounds);
end;

procedure TcxGridDataRowFixingMenu.AddOperation(AFixedState: TcxDataControllerRowFixedState);
var
  AImageIndex: Integer;
  AText: string;
  AData: TObject;
begin
  AImageIndex := GetOperationImageIndex(AFixedState);
  AText := GetOperationText(AFixedState);
  AData := TObject(AFixedState);
  Items.Add(AText, AImageIndex, AData);
end;

procedure TcxGridDataRowFixingMenu.RecreateOperations;
begin
  BeginUpdate;
  try
    Items.Clear;
    if DataRow.FixedState <> rfsFixedToTop then
      AddOperation(rfsFixedToTop);
    if DataRow.FixedState <> rfsNotFixed then
      AddOperation(rfsNotFixed);
    if DataRow.FixedState <> rfsFixedToBottom then
      AddOperation(rfsFixedToBottom);
  finally
    EndUpdate;
  end;
end;

procedure TcxGridDataRowFixingMenu.DoSelectItem(AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean);
begin
  DataRow.FixedState := TcxDataControllerRowFixedState(AItem.Data);
end;

function TcxGridDataRowFixingMenu.GetOperationImageIndex(AFixedState: TcxDataControllerRowFixedState): Integer;
const
  ImageMap: array[TcxDataControllerRowFixedState] of Integer = (0, 1, 2);
begin
  Result := ImageMap[AFixedState];
end;

function TcxGridDataRowFixingMenu.GetOperationText(AFixedState: TcxDataControllerRowFixedState): string;
begin
  case AFixedState of
    rfsNotFixed:
      Result := cxGetResourceString(@scxGridDataRowFixingPopupCommandUnfix);
    rfsFixedToTop:
      Result := cxGetResourceString(@scxGridDataRowFixingPopupCommandFixToTop);
    rfsFixedToBottom:
      Result := cxGetResourceString(@scxGridDataRowFixingPopupCommandFixToBottom);
  end;
end;

{ TcxGridTableViewInplaceEditFormDataCellViewInfo }

function TcxGridTableViewInplaceEditFormDataCellViewInfo.CanFocus: Boolean;
begin
  Result := inherited CanFocus and Item.CanFocus(GridRecord);
end;

procedure TcxGridTableViewInplaceEditFormDataCellViewInfo.GetCaptionParams(var AParams: TcxViewParams);
begin
  if InvalidateOnStateChange and (State = gcsSelected) then
    GridView.Styles.GetInplaceEditFormItemHottrackParams(Item, AParams)
  else
    GridView.Styles.GetInplaceEditFormItemParams(GridRecord, Item, AParams);
end;

function TcxGridTableViewInplaceEditFormDataCellViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := not GridView.IsDestroying and GridView.EditForm.ItemHotTrack;
end;

function TcxGridTableViewInplaceEditFormDataCellViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewInplaceEditFormDataCellViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxGridTableViewInplaceEditFormDataCellViewInfo.GetItem: TcxGridColumn;
begin
  Result := TcxGridColumn(inherited Item);
end;

{ TcxGridTableViewInplaceEditFormContainerViewInfo }

function TcxGridTableViewInplaceEditFormContainerViewInfo.FindGridItemViewInfo(
  AViewInfo: TcxGridCustomLayoutItemViewInfo): TcxGridTableDataCellViewInfo;
var
  AGridItem: TcxCustomGridTableItem;
  AInplaceEditFormArea: TcxGridInplaceEditFormAreaViewInfo;
begin
  Result := nil;
  if HasRecordViewInfo then
  begin
    AInplaceEditFormArea := TcxGridDataRowViewInfo(RecordViewInfo).InplaceEditFormAreaViewInfo;
    if AInplaceEditFormArea <> nil then
    begin
      AGridItem := TcxGridInplaceEditFormLayoutItemViewInfo(AViewInfo).Item.GridViewItem;
      Result := AInplaceEditFormArea.FindCellViewInfo(AGridItem);
    end
  end;
end;

{ TcxGridTableViewInplaceEditFormContainer }

function TcxGridTableViewInplaceEditFormContainer.GetClientBounds: TRect;
begin
  Result := GridView.GetInplaceEditFormClientBounds;
end;

function TcxGridTableViewInplaceEditFormContainer.GetClientRect: TRect;
begin
  Result := GridView.GetInplaceEditFormClientRect;
end;

function TcxGridTableViewInplaceEditFormContainer.IsItemVisibleForEditForm(AItem: TcxGridInplaceEditFormLayoutItem): Boolean;
begin
  Result := inherited IsItemVisibleForEditForm(AItem) and (AItem.GridViewItem is TcxGridColumn) and
    TcxGridColumn(AItem.GridViewItem).IsVisibleForEditForm;
end;

function TcxGridTableViewInplaceEditFormContainer.GetViewInfoClass: TdxLayoutContainerViewInfoClass;
begin
  Result := TcxGridTableViewInplaceEditFormContainerViewInfo;
end;

function TcxGridTableViewInplaceEditFormContainer.CanCreateLayoutItemForGridItem(
  AItem: TcxCustomGridTableItem): Boolean;
begin
  if AItem is TcxGridColumn then
    Result := TcxGridColumn(AItem).CanCreateLayoutItem
  else
    Result := inherited CanCreateLayoutItemForGridItem(AItem);
end;

function TcxGridTableViewInplaceEditFormContainer.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := TcxGridTableViewInplaceEditForm(inherited InplaceEditForm);
end;

function TcxGridTableViewInplaceEditFormContainer.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewInplaceEditFormContainer.GetViewInfo: TcxGridTableViewInplaceEditFormContainerViewInfo;
begin
  Result := TcxGridTableViewInplaceEditFormContainerViewInfo(inherited ViewInfo);
end;

{ TcxGridTableViewInplaceEditForm }

procedure TcxGridTableViewInplaceEditForm.CheckFocusedItem(
  AItemViewInfo: TcxGridTableViewInplaceEditFormDataCellViewInfo);
begin
  GridView.Controller.CheckFocusedItem(AItemViewInfo);
end;

function TcxGridTableViewInplaceEditForm.IsInplaceEditFormMode: Boolean;
begin
  Result := GridView.IsInplaceEditFormMode;
end;

procedure TcxGridTableViewInplaceEditForm.ValidateEditVisibility;
begin
  GridView.ValidateEditVisibility;
end;

function TcxGridTableViewInplaceEditForm.GetContainer: TcxGridTableViewInplaceEditFormContainer;
begin
  Result := TcxGridTableViewInplaceEditFormContainer(inherited Container);
end;

function TcxGridTableViewInplaceEditForm.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewInplaceEditForm.GetContainerClass: TcxGridInplaceEditFormContainerClass;
begin
  Result := TcxGridTableViewInplaceEditFormContainer;
end;

function TcxGridTableViewInplaceEditForm.IsAssigningOptions: Boolean;
begin
  Result := GridView.EditForm.IsAssigning;
end;

function TcxGridTableViewInplaceEditForm.CanShowCustomizationForm: Boolean;
begin
  Result := GridView.IsInplaceEditFormMode and inherited CanShowCustomizationForm;
end;

procedure TcxGridTableViewInplaceEditForm.Changed(AHardUpdate: Boolean = False);
begin
  if AHardUpdate or Visible then
    GridView.EditForm.Changed(vcSize)
  else
    GridView.EditForm.Changed(vcProperty);
end;

function TcxGridTableViewInplaceEditForm.GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel;
begin
  Result := GridView.InplaceEditFormLayoutLookAndFeel;
end;

function TcxGridTableViewInplaceEditForm.GetVisible: Boolean;
begin
  Result := GridView.IsInplaceEditFormMode and inherited GetVisible;
end;

procedure TcxGridTableViewInplaceEditForm.PopulateTabOrderList(AList: TList);
var
  ARow: TcxCustomGridRecord;
  ARecordContainerViewInfo: TcxGridInplaceEditFormContainerViewInfo;
begin
  if not GridView.Visible then
    Exit;
  ARow := GridView.ViewData.GetRecordByRecordIndex(EditingRecordIndex);
  if Visible and not GridView.IsUpdateLocked and (ARow is TcxGridDataRow) and (ARow.ViewInfo is TcxGridDataRowViewInfo) then
  begin
    ARecordContainerViewInfo := TcxGridDataRowViewInfo(ARow.ViewInfo).InplaceEditFormAreaViewInfo.ContainerViewInfo;
    ARecordContainerViewInfo.PopulateTabOrderList(AList)
  end
  else
    Container.ViewInfo.PopulateTabOrderList(AList);
end;

procedure TcxGridTableViewInplaceEditForm.ResetEditingRecordIndex;
begin
  inherited ResetEditingRecordIndex;
  if not Visible then
    GridView.Controller.EditingController.ResetUpdateButtonEnabled;
end;

{ TcxGridEditFormOptions }

procedure TcxGridEditFormOptions.Assign(Source: TPersistent);
var
  AOptions: TcxGridEditFormOptions;
begin
  FIsAssigning := True;
  try
    if Source is TcxGridEditFormOptions then
    begin
      AOptions := TcxGridEditFormOptions(Source);
      DefaultColumnCount := AOptions.DefaultColumnCount;
      MasterRowDblClickAction := AOptions.MasterRowDblClickAction;
      DefaultStretch := AOptions.DefaultStretch;
      UseDefaultLayout := AOptions.UseDefaultLayout;
    end;
  finally
    FIsAssigning := False;
  end;
end;

function TcxGridEditFormOptions.GetDefaultColumnCount: Integer;
begin
  Result := InplaceEditForm.DefaultColumnCount;
end;

function TcxGridEditFormOptions.GetDefaultStretch: TcxGridInplaceEditFormStretch;
begin
  Result := InplaceEditForm.DefaultStretch;
end;

function TcxGridEditFormOptions.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridEditFormOptions.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := GridView.InplaceEditForm;
end;

function TcxGridEditFormOptions.GetItemHotTrack: Boolean;
begin
  Result := InplaceEditForm.ItemHotTrack;
end;

function TcxGridEditFormOptions.GetMasterRowDblClickAction: TcxGridMasterRowDblClickAction;
begin
  Result := InplaceEditForm.MasterRowDblClickAction;
end;

function TcxGridEditFormOptions.GetUseDefaultLayout: Boolean;
begin
  Result := InplaceEditForm.UseDefaultLayout;
end;

procedure TcxGridEditFormOptions.SetDefaultColumnCount(AValue: Integer);
begin
  InplaceEditForm.DefaultColumnCount := AValue;
end;

procedure TcxGridEditFormOptions.SetDefaultStretch(
  AValue: TcxGridInplaceEditFormStretch);
begin
  InplaceEditForm.DefaultStretch := AValue;
end;

procedure TcxGridEditFormOptions.SetItemHotTrack(AValue: Boolean);
begin
  InplaceEditForm.ItemHotTrack := AValue;
end;

procedure TcxGridEditFormOptions.SetMasterRowDblClickAction(
  AValue: TcxGridMasterRowDblClickAction);
begin
  InplaceEditForm.MasterRowDblClickAction := AValue;
end;

procedure TcxGridEditFormOptions.SetUseDefaultLayout(
  AValue: Boolean);
begin
  InplaceEditForm.UseDefaultLayout := AValue;
end;

{ TcxCustomGridRow }

function TcxCustomGridRow.GetAsGroupRow: TcxGridGroupRow;
begin
  Result := Self as TcxGridGroupRow;
end;

function TcxCustomGridRow.GetAsMasterDataRow: TcxGridMasterDataRow;
begin
  Result := Self as TcxGridMasterDataRow;
end;

function TcxCustomGridRow.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxCustomGridRow.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridRow.GetGridViewLevel: TcxGridLevel;
begin
  Result := TcxGridLevel(GridView.Level);
end;

function TcxCustomGridRow.GetIsFilterRow: Boolean;
begin
  Result := ViewData.FilterRow = Self;
end;

function TcxCustomGridRow.GetIsNewItemRow: Boolean;
begin
  Result := IsNewItemRecord;
end;

function TcxCustomGridRow.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

function TcxCustomGridRow.GetViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := TcxCustomGridRowViewInfo(inherited ViewInfo);
end;

function TcxCustomGridRow.HasParentGroup: Boolean;
begin
  Result := (ParentRecord <> nil) and not ParentRecord.IsData;
end;

function TcxCustomGridRow.GetCheckBoxState: TcxCheckBoxState;
begin
  if Selected then
    Result := cbsChecked
  else
    Result := cbsUnchecked;
end;

function TcxCustomGridRow.GetFixedState: TcxDataControllerRowFixedState;
begin
  Result := rfsNotFixed;
end;

function TcxCustomGridRow.GetIndicatorKind: TcxIndicatorKind;

  function IsMultiSelected: Boolean;
  begin
    Result := GridView.OptionsSelection.MultiSelect and Selected;
  end;

begin
  if Focused then
    if IsEditing then
      if dceInsert in DataController.EditState then
        Result := ikInsert
      else
        Result := ikEdit
    else
      if IsMultiSelected then
        Result := ikMultiArrow
      else
        Result := ikArrow
  else
    if IsMultiSelected then
      Result := ikMultiDot
    else
      Result := ikNone;
end;

function TcxCustomGridRow.GetIsParentRecordLast(AIndex: Integer): Boolean;
begin
  Result := inherited GetIsParentRecordLast(AIndex) or (Index = Controller.LastScrollRecordIndex);
end;

function TcxCustomGridRow.GetTopGroupIndex(ALevel: Integer = 0): Integer;
var
  AParentGroup: TcxGridGroupRow;
begin
  Result := -1;
  if (ALevel < Level) and HasParentGroup then
  begin
    AParentGroup := TcxGridGroupRow(ParentRecord);
    if AParentGroup.Level = ALevel then
      Result := AParentGroup.Index
    else
      Result := AParentGroup.GetTopGroupIndex(ALevel);
  end;
end;

function TcxCustomGridRow.IsSpecial: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT:
      if Expandable and Expanded then
      begin
        Expanded := False;
        Key := 0;
      end;
    VK_RIGHT:
      if Expandable and not Expanded then
      begin
        Expanded := True;
        Key := 0;
      end;
    VK_MULTIPLY:
      if Expandable then
      begin
        GridView.Controller.EatKeyPress := True;
        Expand(True);
        Key := 0;
      end;
  end;
end;

function TcxCustomGridRow.ShowCheckBox: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRow.SetFixedState(AValue: TcxDataControllerRowFixedState);
begin
//do nothing
end;

procedure TcxCustomGridRow.ToggleCheckBox;
begin
  Focused := True;
  Selected := not Selected;
end;

function TcxCustomGridRow.ExpandOnDblClick: Boolean;
begin
  Result := Expandable;
end;

function TcxCustomGridRow.SupportsCellMultiSelect: Boolean;
begin
  Result := False;
end;

{ TcxGridDataRow }

function TcxGridDataRow.ExpandOnDblClick: Boolean;
begin
  Result := inherited ExpandOnDblClick and (not GridView.IsInplaceEditFormMode or Focused);
end;

function TcxGridDataRow.SupportsCellMultiSelect: Boolean;
begin
  Result := True;
end;

procedure TcxGridDataRow.ToggleEditFormVisibility;
begin
  EditFormVisible := not EditFormVisible;
end;

function TcxGridDataRow.GetEditFormVisible: Boolean;
begin
  Result := InplaceEditForm.Visible and
    (InplaceEditForm.EditingRecordIndex = RecordIndex);
end;

function TcxGridDataRow.GetInplaceEditFormClientBounds: TRect;
begin
  if ViewInfo <> nil then
    Result := TcxGridDataRowViewInfo(ViewInfo).GetInplaceEditFormClientBounds
  else
    Result := cxEmptyRect;
end;

procedure TcxGridDataRow.KeyDown(var Key: Word; Shift: TShiftState);

  procedure DoInplaceEditFormButtonAction;
  begin
    case Controller.FocusedItemKind of
      fikUpdateButton:
        InplaceEditForm.UpdateExecute;
      fikCancelButton:
        InplaceEditForm.CancelExecute;
    end;
  end;

begin
  case Key of
    VK_RETURN, VK_F2:
      if GridView.IsInplaceEditFormMode and not EditFormVisible then
      begin
        EditFormVisible := True;
        Key := 0;
      end;
    VK_ESCAPE:
      if EditFormVisible and not DataController.IsEditing then
      begin
        EditFormVisible := False;
        GridView.Controller.EatKeyPress := True;
      end;
  end;
  inherited KeyDown(Key, Shift);
  if ((Key = VK_RETURN) or (Key = VK_SPACE)) and GridView.IsInplaceEditFormMode and
    EditFormVisible and (Controller.FocusedItemKind in [fikUpdateButton, fikCancelButton]) then
    DoInplaceEditFormButtonAction;
end;

procedure TcxGridDataRow.SetEditFormVisible(AValue: Boolean);
begin
  if AValue and GridView.IsInplaceEditFormMode then
  begin
    InplaceEditForm.EditingRecordIndex := RecordIndex;
    MakeVisible;
  end
  else
   InplaceEditForm.Close;
end;

function TcxGridDataRow.ShowCheckBox: Boolean;
begin
  Result := (cbvDataRow in GridView.OptionsSelection.CheckBoxVisibility) and not IsSpecial;
end;

function TcxGridDataRow.GetFixedState: TcxDataControllerRowFixedState;
begin
  if not IsSpecial then
    Result := DataController.RowFixedState[Index]
  else
    Result := inherited GetFixedState;
end;

function TcxGridDataRow.GetExpandable: Boolean;
begin
  Result := GridView.CanShowInplaceEditForm and (FixedState = rfsNotFixed);
end;

function TcxGridDataRow.GetHasCells: Boolean;
begin
  Result := True;
end;

function TcxGridDataRow.GetIndicatorKind: TcxIndicatorKind;
begin
  if EditFormVisible and not (dceInsert in DataController.EditState) and
    not Controller.EditingController.IsRecordModified then
    Result := ikInplaceEdit
  else
    Result := inherited GetIndicatorKind;
end;

function TcxGridDataRow.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridTableViewInfoCacheItem;
end;

function TcxGridDataRow.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridDataRowViewInfo;
end;

procedure TcxGridDataRow.SetFixedState(AValue: TcxDataControllerRowFixedState);
begin
  if not IsSpecial then
    DataController.RowFixedState[Index] := AValue
  else
    inherited SetFixedState(AValue);
end;

procedure TcxGridDataRow.ToggleFixedState(AMenuForBounds: TRect);
begin
  Focused := True;
  if GridView.FixedDataRows.PinClickAction = rpcaShowPopup then
    Controller.ShowDataRowFixingMenu(Self, AMenuForBounds)
  else
    if FixedState <> rfsNotFixed then
      FixedState := rfsNotFixed
    else
      if GridView.FixedDataRows.PinClickAction = rpcaFixToBottom then
        FixedState := rfsFixedToBottom
      else
        FixedState := rfsFixedToTop;
end;

function TcxGridDataRow.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := GridView.InplaceEditForm;
end;

{ TcxGridNewItemRow }

function TcxGridNewItemRow.SupportsCellMultiSelect: Boolean;
begin
  Result := False;
end;

function TcxGridNewItemRow.GetIndicatorKind: TcxIndicatorKind;
begin
  Result := ikInsert;
end;

function TcxGridNewItemRow.IsSpecial: Boolean;
begin
  Result := True;
end;

procedure TcxGridNewItemRow.SetEditFormVisible(AValue: Boolean);
begin
  InplaceEditForm.ResetEditingRecordIndex;
end;

{ TcxGridFilterRow }

destructor TcxGridFilterRow.Destroy;
begin
  Selected := False;
  inherited Destroy;
end;

procedure TcxGridFilterRow.ActualizeProperties(AProperties: TcxCustomEditProperties);
begin
  if not AProperties.AllowRepositorySharing then
    AProperties.RefreshNonShareable;
end;

function TcxGridFilterRow.GetFilterCriteriaItem(Index: Integer): TcxFilterCriteriaItem;
var
  ACriteriaItem: TcxFilterCriteriaItem;
begin
  ACriteriaItem := GridView.Columns[Index].DataBinding.FilterCriteriaItem;
  if (ACriteriaItem <> nil) and IsCriteriaItemSupported(Index, ACriteriaItem) then
    Result := ACriteriaItem
  else
    Result := nil;
end;

function TcxGridFilterRow.GetOperator(Index: Integer): TcxFilterOperatorKind;
begin
  Result := GetOperator(Index, GetValue(Index));
end;

function TcxGridFilterRow.GetOperator(Index: Integer; AValue: Variant): TcxFilterOperatorKind;
var
  AFilterCriteriaItem: TcxFilterCriteriaItem;
begin
  if GridView.FilterRow.OperatorCustomization then
  begin
    AFilterCriteriaItem := FilterCriteriaItems[Index];
    if (AFilterCriteriaItem = nil) then
      Result := GridView.Columns[Index].InternalGetFilterRowOperator
    else
      Result := AFilterCriteriaItem.OperatorKind;
  end
  else
    Result := GetDefaultFilterOperatorKind(AValue, True);
end;

function TcxGridFilterRow.GetIndicatorKind: TcxIndicatorKind;
begin
  Result := ikFilter;
end;

procedure TcxGridFilterRow.RefreshRecordInfo;
begin
  with RecordInfo do
  begin
    Expanded := False;
    Level := 0;
    RecordIndex := -1;
  end
end;

procedure TcxGridFilterRow.SetOperator(Index: Integer; AOperator: TcxFilterOperatorKind);
var
  AColumn: TcxGridColumn;
  AValue: Variant;
begin
  if not GridView.FilterRow.OperatorCustomization or not Controller.IsFilterRowOperatorSupported(Index, AOperator) then
    Exit;
  GridView.Controller.KeepFilterRowFocusing := True;
  try
    AColumn := GridView.Columns[Index];
    AValue := Values[Index];
    if not VarIsSoftNull(AValue) then
    begin
      DataController.Filter.BeginUpdate;
      try
        DataController.Filter.Active := True;
        AColumn.DataBinding.AddToFilter(nil, AOperator, AValue, GetDisplayTextForValue(Index, AValue), True);
      finally
        DataController.Filter.EndUpdate;
      end;
    end;
    if Controller.IsFilterRowOperatorSupported(Index, AOperator) then
      AColumn.InternalSetFilterRowOperator(AOperator);
    GridView.Changed(vcLayout);
  finally
    GridView.Controller.KeepFilterRowFocusing := False;
  end;
end;

function TcxGridFilterRow.GetSelected: Boolean;
begin
  Result := FSelected;
end;

function TcxGridFilterRow.GetVisible: Boolean;
begin
  Result := True;
end;

procedure TcxGridFilterRow.SetSelected(Value: Boolean);
begin
  if (FSelected <> Value) and not InplaceEditForm.Visible then
  begin
    GridView.Controller.FilterRowFocusChanging(Value);
    FSelected := Value;
    Invalidate;
    GridView.Controller.FilterRowFocusChanged;
  end;
end;

function TcxGridFilterRow.GetDisplayText(Index: Integer): string;
var
  AFilterCriteriaItem: TcxFilterCriteriaItem;
begin
  AFilterCriteriaItem := FilterCriteriaItems[Index];
  if AFilterCriteriaItem = nil then
    Result := ''
  else
    Result := AFilterCriteriaItem.DisplayValue;
end;

function TcxGridFilterRow.GetValue(Index: Integer): Variant;
var
  AFilterCriteriaItem: TcxFilterCriteriaItem;
begin
  AFilterCriteriaItem := FilterCriteriaItems[Index];
  if AFilterCriteriaItem = nil then
    Result := Null
  else
    Result := AFilterCriteriaItem.Value;
end;

procedure TcxGridFilterRow.SetDisplayText(Index: Integer; const Value: string);
var
  AFilterCriteriaItem: TcxFilterCriteriaItem;
begin
  AFilterCriteriaItem := FilterCriteriaItems[Index];
  if AFilterCriteriaItem <> nil then
    AFilterCriteriaItem.DisplayValue := Value;
end;

procedure TcxGridFilterRow.SetValue(Index: Integer; const Value: Variant);
var
  AGridView: TcxGridTableView;
  AColumn: TcxGridColumn;
begin
  AGridView := GridView;
  AGridView.Controller.KeepFilterRowFocusing := True;
  try
    AColumn := AGridView.Columns[Index];
    if VarIsSoftNull(Value) then
      AColumn.DataBinding.Filtered := False
    else
    begin
      DataController.Filter.BeginUpdate;
      try
        DataController.Filter.Active := True;
        AColumn.DataBinding.AddToFilter(nil, GetOperator(Index, Value), Value, GetDisplayTextForValue(Index, Value), True);
      finally
        DataController.Filter.EndUpdate;
      end;
    end;
  finally
    AGridView.Controller.KeepFilterRowFocusing := False;
  end;
end;

function TcxGridFilterRow.GetDisplayTextForValue(AIndex: Integer; const AValue: Variant): string;
var
  AColumn: TcxGridColumn;
  AProperties: TcxCustomEditProperties;
  AValueList: TcxGridFilterValueList;
  AValueIndex: Integer;
begin
  GridView.BeginFilteringUpdate;
  try
    AValueList := ViewData.CreateFilterValueList;
    try
      AColumn := GridView.Columns[AIndex];
      AColumn.DataBinding.GetFilterValues(AValueList);
      AValueIndex := AValueList.FindItemByValue(AValue);
      if AValueIndex = -1 then
      begin
        AProperties := AColumn.GetProperties(Self);
        ActualizeProperties(AProperties);
        Result := AProperties.GetDisplayText(AValue, False, False)
      end
      else
        Result := AValueList[AValueIndex].DisplayText;
    finally
      AValueList.Free;
    end;
  finally
    GridView.EndFilteringUpdate;
  end;
end;

function TcxGridFilterRow.GetDefaultFilterOperatorKind(const AValue: Variant; ACheckMask: Boolean): TcxFilterOperatorKind;

  function HasMask(const AValue: string): Boolean;
  begin
    Result :=
      (Pos(DataController.Filter.PercentWildcard, AValue) <> 0) or
      (Pos(DataController.Filter.UnderscoreWildcard, AValue) <> 0);
  end;

begin
  if VarIsStr(AValue) and (not ACheckMask or HasMask(AValue)) then
    Result := foLike
  else
    Result := foEqual;
end;

function TcxGridFilterRow.IsCriteriaItemSupported(AIndex: Integer; ACriteriaItem: TcxFilterCriteriaItem): Boolean;
var
  AOperator: TcxFilterOperatorKind;
  AValue: Variant;
begin
  AOperator := ACriteriaItem.OperatorKind;
  AValue := ACriteriaItem.Value;
  if GridView.FilterRow.OperatorCustomization then
    Result := Controller.IsFilterRowOperatorSupported(AIndex, AOperator) and not VarIsSoftNull(AValue)
  else
    Result := AOperator in [foEqual, GetDefaultFilterOperatorKind(AValue, False)];
end;

procedure TcxGridFilterRow.ResetOperators;
var
  I: Integer;
begin
  for I := 0 to GridView.GetColumnCount - 1 do
    GridView.Columns[I].InternalSetFilterRowOperator(foEqual);
end;

procedure TcxGridFilterRow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
  begin
    Focused := False;
    Key := 0;
  end;
end;

function TcxGridFilterRow.CanFocusCells: Boolean;
begin
  Result := True;
end;

function TcxGridFilterRow.IsEmpty: Boolean;
{var
  I: Integer;}
begin
{  for I := 0 to ValueCount - 1 do
  begin
    Result := FilterCriteriaItems[I] = nil;
    if not Result then Exit;
  end;
  Result := True;}
  Result := DataController.Filter.IsEmpty;
end;

{ TcxGridMasterDataRow }

function TcxGridMasterDataRow.GetActiveDetailGridView: TcxCustomGridView;
begin
  if ActiveDetailIndex = -1 then
    Result := nil
  else
    Result := DetailGridViews[ActiveDetailIndex];
end;

function TcxGridMasterDataRow.GetActiveDetailGridViewExists: Boolean;
begin
  Result := IsValid and (ActiveDetailIndex <> -1) and DetailGridViewExists[ActiveDetailIndex];
end;

function TcxGridMasterDataRow.GetActiveDetailIndex: Integer;
begin
  Result := InternalActiveDetailIndex;
  if (Result <> -1) and not GridView.IsDestroying then
    if not GridViewLevel[Result].Visible then
    begin
      GridView.BeginUpdate;
      try
        Result := GridView.GetDefaultActiveDetailIndex;
        ActiveDetailIndex := Result;
      finally
        GridView.CancelUpdate;
      end;
    end
    else
      if not GridViewLevel.Options.TabsForEmptyDetails and not DetailGridViewHasData[Result] then
      begin
        GridView.BeginUpdate;
        try
          for Result := 0 to DetailGridViewCount - 1 do
            if DetailGridViewHasData[Result] then
            begin
              ActiveDetailIndex := Result;
              Exit;
            end;
          Result := -1;
        finally
          GridView.CancelUpdate;
        end;
      end;
end;

function TcxGridMasterDataRow.GetActiveDetailLevel: TcxGridLevel;
begin
  if ActiveDetailIndex = -1 then
    Result := nil
  else
    Result := GridViewLevel[ActiveDetailIndex];
end;

function TcxGridMasterDataRow.GetDetailGridView(Index: Integer): TcxCustomGridView;
begin
  Result := DataController.GetDetailLinkObject(RecordIndex, Index) as TcxCustomGridView;
end;

function TcxGridMasterDataRow.GetDetailGridViewCount: Integer;
begin
  Result := DataController.Relations.Count;
end;

function TcxGridMasterDataRow.GetDetailGridViewExists(Index: Integer): Boolean;
begin
  Result := DataController.IsDetailDataControllerExist(RecordIndex, Index);
end;

function TcxGridMasterDataRow.GetDetailGridViewHasData(Index: Integer): Boolean;
begin
  Result := DataController.GetDetailHasChildren(RecordIndex, Index);
end;

function TcxGridMasterDataRow.GetInternalActiveDetailGridView: TcxCustomGridView;
begin
  if InternalActiveDetailIndex = -1 then
    Result := nil
  else
    Result := DetailGridViews[InternalActiveDetailIndex];
end;

function TcxGridMasterDataRow.GetInternalActiveDetailGridViewExists: Boolean;
begin
  Result := IsValid and (InternalActiveDetailIndex <> -1) and DetailGridViewExists[InternalActiveDetailIndex];
end;

function TcxGridMasterDataRow.GetInternalActiveDetailIndex: Integer;
begin
  Result := DataController.GetDetailActiveRelationIndex(RecordIndex);
end;

procedure TcxGridMasterDataRow.SetActiveDetailIndex(Value: Integer);
var
  AGridView: TcxCustomGridTableView;
  APrevValue: Integer;
  ANewActiveDetailLevel: TcxGridLevel;
begin
  AGridView := GridView;
  APrevValue := InternalActiveDetailIndex;
  if APrevValue <> Value then
  begin
    ANewActiveDetailLevel := GridViewLevel.Items[Value];
    DataController.ChangeDetailActiveRelationIndex(RecordIndex, Value);
    //if InternalActiveDetailIndex <> APrevValue then
      TcxCustomGridAccess(AGridView.Control).DoActiveTabChangedEx(ANewActiveDetailLevel, RecordIndex);
  end;
end;

procedure TcxGridMasterDataRow.SetActiveDetailLevel(Value: TcxGridLevel);
begin
  if Value.Parent = GridViewLevel then
    ActiveDetailIndex := Value.Index;
end;

procedure TcxGridMasterDataRow.DoCollapse(ARecurse: Boolean);
var
  I: Integer;
  AGridView: TcxCustomGridView;
begin
  //GridView.BeginUpdate;
  try
    if Expanded and ARecurse then
      for I := 0 to DetailGridViewCount - 1 do
        if DetailGridViewExists[I] then
        begin
          AGridView := DetailGridViews[I];
          if AGridView is TcxCustomGridTableView then
            TcxCustomGridTableView(AGridView).ViewData.Collapse(ARecurse);
        end;
    DataController.ChangeDetailExpanding(RecordIndex, False);
  finally
    //GridView.EndUpdate;
  end;
end;

procedure TcxGridMasterDataRow.DoExpand(ARecurse: Boolean);
var
  AViewData: TcxGridViewData;
  ARecordIndex, I: Integer;
  ARecord: TcxCustomGridRecord;
  AGridView: TcxCustomGridView;
begin
  AViewData := ViewData;
  ARecordIndex := RecordIndex;
  if not DataController.ChangeDetailExpanding(RecordIndex, True) then Exit;
  ARecord := AViewData.GetRecordByRecordIndex(ARecordIndex);
  if Self <> ARecord then
    if ARecord is TcxGridMasterDataRow then
      TcxGridMasterDataRow(ARecord).DoExpand(ARecurse)
    else
  else
    if Expanded and ARecurse then
      for I := 0 to DetailGridViewCount - 1 do
      begin
        AGridView := DetailGridViews[I];
        if AGridView is TcxCustomGridTableView then
          TcxCustomGridTableView(AGridView).ViewData.Expand(ARecurse);
      end;
end;

function TcxGridMasterDataRow.GetExpandable: Boolean;
begin
  Result := (GridView.OptionsView.ExpandButtonsForEmptyDetails or HasChildren or Expanded) and
    ((GridView.MasterRowDblClickAction = dcaSwitchExpandedState) or not GridView.IsInplaceEditFormMode) and
    (FixedState = rfsNotFixed) or
    inherited GetExpandable;
end;

function TcxGridMasterDataRow.GetExpanded: Boolean;
begin
  Result := DataController.GetDetailExpanding(RecordInfo.RecordIndex);
end;

function TcxGridMasterDataRow.GetHasChildren: Boolean;
var
  I: Integer;
  ADataRelation: TcxCustomDataRelation;
begin
  for I := 0 to GridViewLevel.VisibleCount - 1 do
  begin
    ADataRelation := GridViewLevel.VisibleItems[I].DataRelation;
    //if ADataRelation <> nil then   //!!!
    begin
      Result := DetailGridViewHasData[ADataRelation.Index];
      if Result then Exit;
    end;
  end;
  Result := False;
end;

function TcxGridMasterDataRow.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridMasterTableViewInfoCacheItem;
end;

function TcxGridMasterDataRow.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridMasterDataRowViewInfo;
end;

procedure TcxGridMasterDataRow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //if not ((Key = VK_LEFT) or (Key = VK_RIGHT)) then - AS5427
    inherited;
end;

procedure TcxGridMasterDataRow.ToggleExpanded;
var
  AGridView: TcxGridTableView;
  AGridRecordIndex: Integer;
begin
  if DataController.IsGridMode and not Expanded and not Focused then
  begin
    AGridView := GridView;
    AGridRecordIndex := RecordIndex;
    inherited;
    if (AGridView.DataController.FocusedRecordIndex = AGridRecordIndex) then
      AGridView.Controller.SelectFocusedRecord;
  end
  else
    inherited;
end;

function TcxGridMasterDataRow.ExpandOnDblClick: Boolean;
begin
  Result := inherited ExpandOnDblClick and
    (GridView.OptionsBehavior.ExpandMasterRowOnDblClick or (GridView.MasterRowDblClickAction <> dcaSwitchExpandedState));
end;

function TcxGridMasterDataRow.GetFirstFocusableChild: TcxCustomGridRecord;
var
  AGridView: TcxCustomGridView;
  AGridRecordIndex: Integer;
  ACycleChanged: Boolean;
begin
  Result := inherited GetFirstFocusableChild;
  if Expanded then
  begin
    AGridView := ActiveDetailGridView;
    if (AGridView is TcxGridTableView) and TcxGridTableView(AGridView).ViewData.HasFilterRow then
      Result := TcxGridTableView(AGridView).ViewData.FilterRow
    else
      if AGridView is TcxCustomGridTableView then
        with TcxCustomGridTableView(AGridView) do
          if ViewData.HasNewItemRecord then
            Result := ViewData.NewItemRecord
          else
          begin
            AGridRecordIndex := Controller.FindNextRecord(-1, True, False, ACycleChanged);
            if AGridRecordIndex <> -1 then
              Result := ViewData.Records[AGridRecordIndex];
          end;
  end;
end;

function TcxGridMasterDataRow.GetLastFocusableChild(ARecursive: Boolean): TcxCustomGridRecord;
var
  AGridView: TcxCustomGridView;
  AGridRecordIndex: Integer;
  AGridRecord: TcxCustomGridRecord;
  ACycleChanged: Boolean;
begin
  Result := inherited GetLastFocusableChild(ARecursive);
  if Expanded then
  begin
    AGridView := ActiveDetailGridView;
    if AGridView is TcxCustomGridTableView then
      with TcxCustomGridTableView(AGridView) do
      begin
        AGridRecordIndex := Controller.FindNextRecord(-1, False, True, ACycleChanged);
        if AGridRecordIndex <> -1 then
        begin
          Result := ViewData.Records[AGridRecordIndex];
          if ARecursive then
          begin
            AGridRecord := Result.GetLastFocusableChild(ARecursive);
            if AGridRecord <> nil then Result := AGridRecord;
          end;
        end
        else
          if ViewData.HasNewItemRecord then
            Result := ViewData.NewItemRecord
          else
            if (AGridView is TcxGridTableView) and
              TcxGridTableView(AGridView).ViewData.HasFilterRow then
              Result := TcxGridTableView(AGridView).ViewData.FilterRow;
      end;
  end;
end;

{ TcxGridGroupRow }

function TcxGridGroupRow.GetDisplayCaptionByGroupedColumn(AIndex: Integer): string;
begin
  Result := GroupedColumns[AIndex].GetAlternateCaption;
  if Result <> '' then
    Result := Result + ' : ';
  Result := Result + GetDisplayTextByGroupedColumn(AIndex);
end;

function TcxGridGroupRow.GetMainGroupedColumn: TcxGridColumn;
var
  AItemIndex: Integer;
begin
  AItemIndex := DataController.Groups.GetParentGroupingItemIndex(Level);
  Result := GridView.Columns[AItemIndex];
end;

function TcxGridGroupRow.GetGroupedColumn(AGroupedColumnIndex: Integer): TcxGridColumn;
var
  AItemIndex: Integer;
begin
  AItemIndex := DataController.Groups.GetGroupingItemIndexByLevelGroupedItemIndex(Level, AGroupedColumnIndex);
  Result := GridView.Columns[AItemIndex];
end;

function TcxGridGroupRow.GetGroupedColumnCount: Integer;
begin
  Result := DataController.Groups.GetLevelGroupedItemCount(Level);
end;

function TcxGridGroupRow.GetGroupedColumnValue(AGroupedColumnIndex: Integer): Variant;
begin
  Result := DataController.GetGroupRowValue(RecordInfo, AGroupedColumnIndex);
end;

function TcxGridGroupRow.GetGroupSummaryItems: TcxDataGroupSummaryItems;
begin
  Result := DataController.Summary.GroupSummaryItems[Level];
end;

procedure TcxGridGroupRow.DoCollapse(ARecurse: Boolean);
var
  AIsEditing: Boolean;
  AGridView: TcxGridTableView;
  AIndex, AFocusedGroupIndex: Integer;
begin
  if not ARecurse and not Expanded or not GridView.DoGroupRowCollapsing(Self) then
    Exit;
  AGridView := GridView;
  AIsEditing := Controller.DataController.IsEditing;
  AIndex := Index;
  AFocusedGroupIndex := Controller.GetGroupIndexByFocusedRowIndex;
  if FixedState = rfsFixedToTop then
    Controller.TopRowIndex := Index - Level;
  DataController.Groups.ChangeExpanding(Index, False, ARecurse);
  if not AIsEditing or (AFocusedGroupIndex = AGridView.Controller.GetGroupIndexByFocusedRowIndex) then
    AGridView.DoGroupRowCollapsed(AGridView.ViewData.Rows[AIndex] as TcxGridGroupRow);
end;

procedure TcxGridGroupRow.DoExpand(ARecurse: Boolean);
var
  AIndex: Integer;
  AGridView: TcxGridTableView;
begin
  if not ARecurse and Expanded or not GridView.DoGroupRowExpanding(Self) then
    Exit;
  AGridView := GridView;
  AIndex := Index;
  DataController.Groups.ChangeExpanding(Index, True, ARecurse);
  AGridView.DoGroupRowExpanded(AGridView.ViewData.Rows[AIndex] as TcxGridGroupRow);
end;

{function TcxGridGroupRow.GetDestroyingOnExpanding: Boolean;
begin
  Result := True;
end;}

function TcxGridGroupRow.GetExpandable: Boolean;
begin
  Result := not (dcoGroupsAlwaysExpanded in DataController.Options);
end;

function TcxGridGroupRow.GetExpanded: Boolean;
begin
  Result := RecordInfo.Expanded;
end;

function TcxGridGroupRow.GetCheckBoxState: TcxCheckBoxState;
begin
  Result := inherited GetCheckBoxState;
  if (Result = cbsUnchecked) and (dcoMultiSelectionSyncGroupWithChildren in DataController.Options) and
    DataController.GroupContainsSelectedRows(Index) then
    Result := cbsGrayed;
end;

function TcxGridGroupRow.GetDisplayCaption: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to GroupedColumnCount - 1 do
  begin
    if Result <> '' then
      Result := Result + GridView.OptionsView.MergedGroupSeparator + ' ';
    Result := Result + DisplayCaptions[I];
  end;
end;

function TcxGridGroupRow.GetDisplayText(Index: Integer): string;
begin
  if ViewData.HasCustomDataHandling(GroupedColumn, doGrouping) then
    Result := ViewData.GetCustomDataDisplayText(RecordIndex, GroupedColumn.Index, doGrouping)
  else
    Result := inherited GetDisplayText(Index);
end;

function TcxGridGroupRow.GetDisplayTextByGroupedColumn(AGroupedColumnIndex: Integer): string;
var
  AColumn: TcxGridColumn;
  AColumnIndex: Integer;
begin
  AColumn := GroupedColumns[AGroupedColumnIndex];
  AColumnIndex := AColumn.Index;
  if ViewData.HasCustomDataHandling(AColumn, doGrouping) then
    Result := ViewData.GetCustomDataDisplayText(RecordIndex, AColumnIndex, doGrouping)
  else
    Result := DataController.GetGroupRowDisplayText(RecordInfo, AColumnIndex);
end;

function TcxGridGroupRow.GetDisplayTextValue: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to GroupedColumnCount - 1 do
  begin
    if Result <> '' then
      Result := Result + GridView.OptionsView.MergedGroupSeparator + ' ';
    Result := Result + GetDisplayTextValueByGroupedColumn(I);
  end;
end;

function TcxGridGroupRow.GetFixedState: TcxDataControllerRowFixedState;
begin
  if ViewInfo <> nil then
    Result := ViewInfo.FixedState
  else
    Result := inherited GetFixedState;
end;

function TcxGridGroupRow.GetIsData: Boolean;
begin
  Result := False;
end;

function TcxGridGroupRow.GetIsParent: Boolean;
begin
  Result := RecordInfo.Level < DataController.Groups.LevelCount;
end;

function TcxGridGroupRow.GetTopGroupIndex(ALevel: Integer = 0): Integer;
begin
  Result := inherited GetTopGroupIndex(ALevel);
  if Result = -1 then
    Result := Index;
end;

function TcxGridGroupRow.GetValue: Variant;
begin
  Result := Values[-1];
  if ViewData.HasCustomDataHandling(GroupedColumn, doGrouping) then
    Result := ViewData.GetCustomDataValue(GroupedColumn, Result, doGrouping);
end;

function TcxGridGroupRow.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridTableViewInfoCacheItem;
end;

function TcxGridGroupRow.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := TcxGridGroupRowViewInfo;
end;

procedure TcxGridGroupRow.SetDisplayText(Index: Integer; const Value: string);
begin
end;

procedure TcxGridGroupRow.SetValue(Index: Integer; const Value: Variant);
begin
end;

function TcxGridGroupRow.ShowCheckBox: Boolean;
begin
  Result := (cbvGroupRow in GridView.OptionsSelection.CheckBoxVisibility);
end;

function TcxGridGroupRow.GetDisplayTextValueByGroupedColumn(AIndex: Integer): string;
var
  ASummaryText: string;
begin
  Result := DisplayCaptions[AIndex];
  ASummaryText := DataController.Summary.GetGroupSummaryText(Index, AIndex);
  if ASummaryText <> '' then
    Result := Result + ' ' + ASummaryText;
end;

function TcxGridGroupRow.GetGroupSummaryInfo(var ASummaryItems: TcxDataSummaryItems;
  var ASummaryValues: PVariant; AGroupedColumnIndex: Integer = 0): Boolean;
begin
  Result := DataController.Summary.GetGroupSummaryInfo(Index, ASummaryItems, ASummaryValues, AGroupedColumnIndex);
end;

{ TcxGridViewData }

destructor TcxGridViewData.Destroy;
begin
  DestroyFilterRow;
  inherited;
end;

function TcxGridViewData.GetFixedBottomRowCount: Integer;
begin
  Result := DataController.FixedBottomRowCount;
end;

function TcxGridViewData.GetFixedTopRowCount: Integer;
begin
  Result := DataController.FixedTopRowCount;
end;

function TcxGridViewData.GetNewItemRow: TcxGridNewItemRow;
begin
  Result := TcxGridNewItemRow(NewItemRecord);
end;

function TcxGridViewData.GetRow(Index: Integer): TcxCustomGridRow;
begin
  Result := TcxCustomGridRow(Records[Index]);
end;

function TcxGridViewData.GetRowCount: Integer;
begin
  Result := RecordCount;
end;

function TcxGridViewData.GetFirstVisibleExpandedMasterRow: TcxGridMasterDataRow;

  function GetExistingVisibleExpandedMasterRowIndex: Integer;
  begin
    for Result := Controller.TopRecordIndex to Controller.TopRecordIndex + ViewInfo.RecordsViewInfo.VisibleCount - 1 do
      if (Rows[Result] is TcxGridMasterDataRow) and Rows[Result].Expanded then
        Exit;
    Result := -1;
  end;

  function FindMasterRowAndMakeItExpandedAndVisible: Integer;
  begin
    Result := Controller.TopRecordIndex;
    if Rows[Result] is TcxGridGroupRow then
    begin
      Rows[Result].Expand(True);
      while (Result < RowCount) and not (Rows[Result] is TcxGridMasterDataRow) do
        Inc(Result);
    end;
    if Result < RowCount then
      Rows[Result].Expanded := True
    else
      Result := -1;
  end;

var
  ARowIndex: Integer;
begin
  if (RowCount = 0) or not GridView.IsMaster then
    Result := nil
  else
  begin
    ARowIndex := GetExistingVisibleExpandedMasterRowIndex;
    if ARowIndex = -1 then
      ARowIndex := FindMasterRowAndMakeItExpandedAndVisible;
    if ARowIndex = -1 then
      Result := nil
    else
      Result := Rows[ARowIndex].AsMasterDataRow;
  end;
end;

function TcxGridViewData.GetNewItemRecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridNewItemRow;
end;

function TcxGridViewData.GetRecordByKind(AKind, AIndex: Integer): TcxCustomGridRecord;
begin
  if AKind = rkFiltering then
    if HasFilterRow then
      Result := FilterRow
    else
      Result := nil
  else
    Result := inherited GetRecordByKind(AKind, AIndex);
end;

function TcxGridViewData.GetDataRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridDataRow;
end;

function TcxGridViewData.GetGroupRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridGroupRow;
end;

function TcxGridViewData.GetMasterRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridMasterDataRow;
end;

function TcxGridViewData.GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  if ARecordInfo.Level < DataController.Groups.LevelCount then
    Result := GetGroupRecordClass(ARecordInfo)
  else
    if GridView.IsMaster then
      Result := GetMasterRecordClass(ARecordInfo)
    else
      Result := GetDataRecordClass(ARecordInfo);
end;

function TcxGridViewData.GetTopGroup(ARowIndex: Integer; ALevel: Integer = 0): TcxCustomGridRow;
var
  AGroupRowIndex: Integer;
begin
  AGroupRowIndex := GetTopGroupIndex(ARowIndex, ALevel);
  Result := Rows[AGroupRowIndex];
end;

function TcxGridViewData.GetTopGroupIndex(ARowIndex: Integer; ALevel: Integer = 0): Integer;
begin
  Result := Rows[ARowIndex].GetTopGroupIndex(ALevel);
end;

function TcxGridViewData.GetRecordKind(ARecord: TcxCustomGridRecord): Integer;
begin
  if HasFilterRow and (ARecord = FilterRow) then
    Result := rkFiltering
  else
    Result := inherited GetRecordKind(ARecord);
end;

procedure TcxGridViewData.CreateFilterRow;
var
  ARowInfo: TcxRowInfo;
begin
  FFilterRow := GetFilterRowClass.Create(Self, -1, ARowInfo);
  FFilterRow.RefreshRecordInfo;
end;

procedure TcxGridViewData.DestroyFilterRow;
begin
  FFilterRow.Free;
  FFilterRow := nil;
end;

procedure TcxGridViewData.CheckFilterRow;
begin
  if HasFilterRow then
    CreateFilterRow
  else
    DestroyFilterRow;
end;

{procedure TcxGridViewData.RecreateFilterRow;
var
  ASelected: Boolean;
begin
  if HasFilterRow then
  begin
    ASelected := FilterRow.InternalSelected;
    DestroyFilterRow;
    CreateFilterRow;
    FilterRow.InternalSelected := ASelected;
  end;
end;}

function TcxGridViewData.GetFilterRowClass: TcxGridFilterRowClass;
begin
  Result := TcxGridFilterRow;
end;

procedure TcxGridViewData.Collapse(ARecurse: Boolean);
begin
  if ARecurse then
  begin
    BeginUpdate;
    try
      DataController.Groups.FullCollapse;
      DataController.CollapseDetails;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TcxGridViewData.Expand(ARecurse: Boolean);
begin
  DataController.Groups.FullExpand;
  inherited;
end;

function TcxGridViewData.HasFilterRow: Boolean;
begin
  Result := TcxGridTableView(GridView).FilterRow.Visible;
end;

function TcxGridViewData.HasNewItemRecord: Boolean;
begin
  Result := TcxGridTableView(GridView).NewItemRow.Visible;
end;

function TcxGridViewData.MakeDetailVisible(ADetailLevel: TComponent{TcxGridLevel}): TcxCustomGridView;
var
  ARow: TcxGridMasterDataRow;
begin
  Result := inherited MakeDetailVisible(ADetailLevel);
  ARow := GetFirstVisibleExpandedMasterRow;
  if ARow <> nil then
  begin
    ARow.ActiveDetailLevel := TcxGridLevel(ADetailLevel);
    if ARow.ActiveDetailLevel = ADetailLevel then
      Result := ARow.ActiveDetailGridView;
    ARow.MakeVisible;
  end;
end;

{procedure TcxGridViewData.Refresh(ARecordCount: Integer);
begin
  RecreateFilterRow;
  inherited;
end;}

{ TcxDragAndDropArrow }

constructor TcxGridColumnHeaderMergeIndicator.Create(ATransparent: Boolean);
begin
  inherited Create;
  FScaleFactor := TdxScaleFactor.Create;
  FTransparent := ATransparent;
  AlphaBlend := False;
  if Transparent then
  begin
    TransparentColorValue := BackColor;
    TransparentColor := True;
  end;
end;

destructor TcxGridColumnHeaderMergeIndicator.Destroy;
begin
  FreeAndNil(FScaleFactor);
  inherited;
end;

function TcxGridColumnHeaderMergeIndicator.CalculateBounds(const APosition: TPoint): TRect;
begin
  Result := cxRectSetSize(cxEmptyRect, GetWidth, GetHeight);
  Result := cxRectSetOrigin(Result, APosition);
end;

procedure TcxGridColumnHeaderMergeIndicator.Draw;
begin
  DrawBackground;
  DrawIndicator;
end;

procedure TcxGridColumnHeaderMergeIndicator.DrawBackground;
begin
  Canvas.Brush.Color := BackColor;
  Canvas.FillRect(ClientRect);
end;

procedure TcxGridColumnHeaderMergeIndicator.DrawIndicator;
begin
  Canvas.Brush.Color := clLime;
  Canvas.Pen.Color := clBlack;
  Canvas.Polygon(GetIndicatorPoints(ClientRect));
end;

function TcxGridColumnHeaderMergeIndicator.GetHeight: Integer;
begin
  Result := GetWidth;
end;

function TcxGridColumnHeaderMergeIndicator.GetBackColor: TColor;
begin
  Result := clFuchsia;
end;

function TcxGridColumnHeaderMergeIndicator.GetIndicatorPoints(const ABounds: TRect): TPoints;
var
  AHalfSize: Integer;
begin
  AHalfSize := ScaleFactor.Apply(2);

  SetLength(Result, 12);
  Result[0] := Point(ABounds.Left, ABounds.Top + AHalfSize);
  Result[1] := Point(ABounds.Left + AHalfSize, ABounds.Top + AHalfSize);
  Result[2] := Point(ABounds.Left + AHalfSize, ABounds.Top);
  Result[3] := Point(ABounds.Left + 3 * AHalfSize - 1, ABounds.Top);
  Result[4] := Point(ABounds.Left + 3 * AHalfSize - 1, ABounds.Top + AHalfSize);
  Result[5] := Point(ABounds.Left + 4 * AHalfSize - 1, ABounds.Top + AHalfSize);
  Result[6] := Point(ABounds.Left + 4 * AHalfSize - 1, ABounds.Top + 3 * AHalfSize - 1);
  Result[7] := Point(ABounds.Left + 3 * AHalfSize - 1, ABounds.Top + 3 * AHalfSize - 1);
  Result[8] := Point(ABounds.Left + 3 * AHalfSize - 1, ABounds.Top + 4 * AHalfSize - 1);
  Result[9] := Point(ABounds.Left + AHalfSize, ABounds.Top + 4 * AHalfSize - 1);
  Result[10] := Point(ABounds.Left + AHalfSize, ABounds.Top + 3 * AHalfSize - 1);
  Result[11] := Point(ABounds.Left, ABounds.Top + 3 * AHalfSize - 1);
end;

function TcxGridColumnHeaderMergeIndicator.GetWidth: Integer;
begin
  Result := ScaleFactor.Apply(2) * 4;
end;

procedure TcxGridColumnHeaderMergeIndicator.Init(const APosition: TPoint);
begin
  ScaleFactor.Assign(dxGetMonitorDPI(APosition), dxDefaultDPI);
  HandleNeeded;  // so that later CreateHandle won't reset Left and Top
  BoundsRect := CalculateBounds(APosition);
  Draw;
end;

function TcxGridColumnHeaderMergeIndicator.GetTransparent: Boolean;
begin
  Result := FTransparent and Assigned(SetLayeredWindowAttributes);
end;

{ TcxGridColumnHeaderMovingObject }

procedure TcxGridColumnHeaderMovingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited Init(P, AParams);
  with AParams as TcxGridColumnHeaderHitTest do
  begin
    SourceItem := Column;
    SourceItemContainerKind := ColumnContainerKind;
  end;
end;

procedure TcxGridColumnHeaderMovingObject.CalculateDestParams(AHitTest: TcxCustomGridHitTest;
  out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone);
begin
  inherited CalculateDestParams(AHitTest, AContainerKind, AZone);
  if AContainerKind = ckNone then
  begin
    AZone := ViewInfo.GroupByBoxViewInfo.GetZone(AHitTest.Pos);
    if AZone = nil then
    begin
      AZone := ViewInfo.HeaderViewInfo.GetZone(AHitTest.Pos);
      if AZone <> nil then
        AContainerKind := ckHeader;
    end
    else
      AContainerKind := ckGroupByBox;
  end;
end;

function TcxGridColumnHeaderMovingObject.CanMergeByCtrl: Boolean;
begin
  Result := GridView.OptionsBehavior.ColumnMergedGrouping;
end;

function TcxGridColumnHeaderMovingObject.CanRemove: Boolean;
begin
  Result := SourceItem.VisibleForCustomization and
    ((SourceItemContainerKind = ckGroupByBox) and SourceItem.CanGroup or
     (SourceItemContainerKind = ckHeader) and SourceItem.CanHide and
       (FOriginalDestColumnContainerKind <> ckGroupByBox) and
       (GridView.Controller.Customization or GridView.OptionsCustomize.ColumnHiding));
end;

procedure TcxGridColumnHeaderMovingObject.CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind);
begin
  if (AValue = ckGroupByBox) and not SourceItem.CanGroup then
    AValue := ckNone;
  inherited;
end;

procedure TcxGridColumnHeaderMovingObject.DirtyChanged;
begin
  inherited DirtyChanged;
  if HasMergeIndicator then
    MergeIndicatorPositionChanged(not Dirty);
end;

procedure TcxGridColumnHeaderMovingObject.DoColumnMovingToHeader;
var
  AIndex: Integer;
begin
  if DestZone.ItemIndex = GridView.VisibleColumnCount then
    AIndex := GridView.ColumnCount - 1
  else
  begin
    AIndex := GridView.VisibleColumns[DestZone.ItemIndex].Index;
    if SourceItem.Index < AIndex then
      Dec(AIndex);
  end;
  SourceItem.Index := AIndex;
end;

procedure TcxGridColumnHeaderMovingObject.DoGetUnmergeableColumns;
begin
  GridView.DoGetUnmergeableColumns(SourceItem, UnmergeableColumns);
end;

procedure TcxGridColumnHeaderMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  if HasMergeIndicator then
    MergeIndicatorPositionChanged;
end;

procedure TcxGridColumnHeaderMovingObject.EndDragAndDrop(Accepted: Boolean);
var
  APrevGroupIndex: Integer;
  AColumnPosChanged: Boolean;
begin
  inherited EndDragAndDrop(Accepted);
  if Accepted then
  begin
    APrevGroupIndex := SourceItem.GroupIndex;
    AColumnPosChanged := UpdateColumnPosition;
    if SourceItem.GroupIndex <> APrevGroupIndex then
      Controller.MakeFocusedRecordVisible;
    if AColumnPosChanged then
      GridView.DoColumnPosChanged(SourceItem);
  end;
  FreeAndNil(FMergeIndicator);
  FreeAndNil(FUnmergeableColumns);
end;

function TcxGridColumnHeaderMovingObject.GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect;
begin
  if DestItemContainerKind = ckGroupByBox then
    Result := GetArrowAreaBoundsForGroupByBox
  else
    Result := GetArrowAreaBoundsForHeader(APlace);
end;

function TcxGridColumnHeaderMovingObject.GetArrowAreaBoundsForGroupByBox: TRect;
var
  AGroupByBoxViewInfo: TcxGridGroupByBoxViewInfo;
  AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo;
begin
  AGroupByBoxViewInfo := ViewInfo.GroupByBoxViewInfo;
  if DestZone.ItemIndex = AGroupByBoxViewInfo.Count then
    if AGroupByBoxViewInfo.Count = 0 then
    begin
      Result := AGroupByBoxViewInfo.Bounds;
      Inc(Result.Left, GroupByBoxLeftOffset);
      InflateRect(Result, 0, -GroupByBoxTopOffset);
    end
    else
    begin
      AColumnHeaderViewInfo := AGroupByBoxViewInfo.Items[AGroupByBoxViewInfo.Count - 1];
      Result := AColumnHeaderViewInfo.Bounds;
      if Control.UseRightToLeftAlignment then
        Result.Right := Result.Left - GroupByBoxHorOffset div 2
      else
        Result.Left := Result.Right + GroupByBoxHorOffset div 2;
    end
  else
  begin
    AColumnHeaderViewInfo := AGroupByBoxViewInfo.Items[DestZone.ItemIndex];
    Result := AColumnHeaderViewInfo.Bounds;
    if not AColumnHeaderViewInfo.Column.IsChildInMergedGroup then
    begin
      if Control.UseRightToLeftAlignment then
        Inc(Result.Right, GroupByBoxHorOffset div 2)
      else
        Dec(Result.Left, GroupByBoxHorOffset div 2);
      if (DestZone.ItemIndex <> 0) and not AGroupByBoxViewInfo.IsSingleLine then
        OffsetRect(Result, 0, -AGroupByBoxViewInfo.GroupByBoxVerOffset div 2);
    end;
  end;
end;

function TcxGridColumnHeaderMovingObject.GetArrowAreaBoundsForHeader(APlace: TcxGridArrowPlace): TRect;
begin
  with ViewInfo.HeaderViewInfo do
    if DestZone.ItemIndex = Count then
      if Count = 0 then
        Result := Bounds
      else
      begin
        Result := Items[Count - 1].Bounds;
        if UseRightToLeftAlignment then
          Result.Right := Result.Left
        else
          Result.Left := Result.Right;
      end
    else
      Result := Items[DestZone.ItemIndex].Bounds;
end;

function TcxGridColumnHeaderMovingObject.GetArrowsClientRect: TRect;
begin
  Result := inherited GetArrowsClientRect;
  with ViewInfo.ClientBounds do
  begin
    Result.Left := Left;
    Result.Right := Right;
  end;
end;

function TcxGridColumnHeaderMovingObject.GetMergeIndicatorClass: TcxGridColumnHeaderMergeIndicatorClass;
begin
  Result := TcxGridColumnHeaderMergeIndicator;
end;

function TcxGridColumnHeaderMovingObject.GetMergeIndicatorPosition: TPoint;
const
  AOffsetX = -2;
  AOffsetY = -6;
var
  ATopArrowArea, ADragAndDropTopArrowBounds: TRect;
  ARightToLeft: Boolean;
begin
  ATopArrowArea := GetArrowAreaBounds(TcxArrowPlace.apTop);
  ARightToLeft := TcxGridSiteAccess(GridView.Site).UseRightToLeftAlignment;
  ADragAndDropTopArrowBounds := TcxDragAndDropArrow.CalculateBounds(
    ATopArrowArea, ArrowsClientRect, TcxArrowPlace.apTop,
    TcxGridSiteAccess(GridView.Site).ScaleFactor, ARightToLeft);
  if ARightToLeft then
    Result := ADragAndDropTopArrowBounds.TopLeft
  else
    Result := Point(ADragAndDropTopArrowBounds.Right, ADragAndDropTopArrowBounds.Top);
  Result := cxPointOffset(Result, GridView.Site.ClientOrigin);
  Result := cxPointOffset(Result, AOffsetX, AOffsetY);
end;

function TcxGridColumnHeaderMovingObject.GetSourceItemViewInfo: TcxCustomGridCellViewInfo;
begin
  case SourceItemContainerKind of
    ckGroupByBox:
      Result := ViewInfo.GroupByBoxViewInfo[SourceItem.GroupIndex];
    ckHeader:
      Result := ViewInfo.HeaderViewInfo[SourceItem.VisibleIndex];
  else
    Result := inherited GetSourceItemViewInfo;
  end;
end;

function TcxGridColumnHeaderMovingObject.HasMergeIndicator: Boolean;
begin
  Result := MergeIndicator <> nil;
end;

procedure TcxGridColumnHeaderMovingObject.HeaderBeginUpdate(AFromGroupByBox: Boolean);
begin
  if AFromGroupByBox then
    GridView.BeginGroupingUpdate
  else
    GridView.GridBeginUpdate;
end;

procedure TcxGridColumnHeaderMovingObject.HeaderEndUpdate(AFromGroupByBox: Boolean);
begin
  if AFromGroupByBox then
    GridView.EndGroupingUpdate
  else
    GridView.GridEndUpdate;
end;

procedure TcxGridColumnHeaderMovingObject.InitMergeIndicator;
begin
  MergeIndicator.Init(GetMergeIndicatorPosition);
end;

procedure TcxGridColumnHeaderMovingObject.InitDragObjects;
begin
  inherited InitDragObjects;
  if CanMergeByCtrl then
  begin
    FUnmergeableColumns := TList.Create;
    DoGetUnmergeableColumns;
    FMergeIndicator := GetMergeIndicatorClass.Create(True);
  end;
end;

function TcxGridColumnHeaderMovingObject.IsCtrlPressed: Boolean;
begin
  Result := GetAsyncKeyState(VK_CONTROL) < 0;
end;

function TcxGridColumnHeaderMovingObject.IsMerging: Boolean;
begin
  Result := (DestItemContainerKind = ckGroupByBox) and CanMergeByCtrl and IsCtrlPressed;
end;

function TcxGridColumnHeaderMovingObject.IsValidDestination: Boolean;
begin
  Result := DestItemContainerKind in [ckGroupByBox, ckHeader];
  if Result then
  begin
    case DestItemContainerKind of
      ckGroupByBox:
        Result := (SourceItem.GroupIndex = -1) and (not IsMerging or AllowMergeWithLeftColumn or AllowMergeWithRightColumn);
      ckHeader:
        Result := not SourceItem.Visible;
    end;
    Result := Result or IsValidDestinationForVisibleSource;
  end;
end;

function TcxGridColumnHeaderMovingObject.IsValidDestinationForVisibleSource: Boolean;
begin
  case DestItemContainerKind of
    ckGroupByBox:
      if IsMerging then
        Result := AllowMergeWithLeftColumn and (SourceItem.GroupIndex <> DestZone.ItemIndex - 1) and
          not ((SourceItem.GroupIndex = DestZone.ItemIndex) and SourceItem.IsChildInMergedGroup) or AllowMergeWithRightColumn and
          (SourceItem.GroupIndex <> DestZone.ItemIndex) and not ((SourceItem.GroupIndex = DestZone.ItemIndex - 1) and
          GetRightGroupColumn.IsChildInMergedGroup)
      else
        Result := (DestZone.ItemIndex < SourceItem.GroupIndex) or (SourceItem.GroupIndex + 1 < DestZone.ItemIndex) or
          SourceItem.IsChildInMergedGroup or (SourceItem.GroupIndex <> GridView.GroupedColumnCount - 1) and
          GridView.GroupedColumns[SourceItem.GroupIndex + 1].IsChildInMergedGroup;
    ckHeader:
      Result :=
        (SourceItemContainerKind = ckGroupByBox) or
        (DestZone.ItemIndex < SourceItem.VisibleIndex) or
        (SourceItem.VisibleIndex + 1 < DestZone.ItemIndex);
  else
    Result := False;
  end;
end;

procedure TcxGridColumnHeaderMovingObject.MergeIndicatorPositionChanged(AVisible: Boolean = True);
begin
  AVisible := AVisible and NeedMergeIndicator;
  if AVisible then
    InitMergeIndicator;
  MergeIndicator.Visible := AVisible;
end;

function TcxGridColumnHeaderMovingObject.NeedMergeIndicator: Boolean;
begin
  Result := IsValidDestination and IsMerging;
end;

function TcxGridColumnHeaderMovingObject.ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := inherited ProcessKeyDown(AKey, AShiftState) or (CanMergeByCtrl and (ssCtrl in AShiftState));
  if HasMergeIndicator then
    MergeIndicatorPositionChanged;
end;

function TcxGridColumnHeaderMovingObject.ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := inherited ProcessKeyUp(AKey, AShiftState);
  if HasMergeIndicator then
    MergeIndicatorPositionChanged;
end;

function TcxGridColumnHeaderMovingObject.AllowMergeWithLeftColumn: Boolean;
begin
  Result := (GetLeftGroupColumn <> nil) and (UnmergeableColumns.IndexOf(GetLeftGroupColumn) = -1);
end;

function TcxGridColumnHeaderMovingObject.AllowMergeWithRightColumn: Boolean;
begin
  Result := (GetRightGroupColumn <> nil) and (UnmergeableColumns.IndexOf(GetRightGroupColumn) = -1);
end;

function TcxGridColumnHeaderMovingObject.ColumnGrouping: Boolean;
begin
  if IsValidDestination then
    Result := SourceItem.GroupBy(GetGroupByIndex, True, MergeWithLeftColumn, MergeWithRightColumn)
  else
    Result := False;
end;

function TcxGridColumnHeaderMovingObject.ColumnMovingToHeader: Boolean;
var
  AFromGroupByBox: Boolean;
begin
  Result := IsValidDestination;
  if Result then
  begin
    AFromGroupByBox := (SourceItemContainerKind = ckGroupByBox) and SourceItem.CanGroup;
    HeaderBeginUpdate(AFromGroupByBox);
    try
      DoColumnMovingToHeader;
      if AFromGroupByBox then
        SourceItem.GroupIndex := -1;
      SourceItem.Visible := True;
    finally
      HeaderEndUpdate(AFromGroupByBox);
    end;
  end;
end;

function TcxGridColumnHeaderMovingObject.ColumnRemoving: Boolean;
begin
  Result := False;
  if not CanRemove then
    Exit;
  case SourceItemContainerKind of
    ckGroupByBox:
      Result := SourceItem.GroupBy(-1,
        (DestItemContainerKind <> ckCustomizationForm) and
        (not GridView.OptionsCustomize.ColumnHiding or not SourceItem.CanHide) and
        not Controller.Customization);
    ckHeader:
      if SourceItem.CanHide then
      begin
        SourceItem.Visible := False;
        Result := True;
      end;
  end;
end;

function TcxGridColumnHeaderMovingObject.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridColumnHeaderMovingObject.GetGroupByIndex: Integer;
begin
  Result := DestZone.ItemIndex - Byte((SourceItem.GroupIndex <> -1) and (SourceItem.GroupIndex < DestZone.ItemIndex));
end;

function TcxGridColumnHeaderMovingObject.GetLeftGroupColumn: TcxGridColumn;
var
  AUseSourceIndex: Boolean;
  AIndex: Integer;
begin
  Result := nil;
  AUseSourceIndex := (SourceItem.GroupIndex <> -1) and (SourceItem.GroupIndex = DestZone.ItemIndex - 1);
  if AUseSourceIndex then
    AIndex := SourceItem.GroupIndex - 1
  else
    AIndex := DestZone.ItemIndex - 1;
  if AIndex > -1 then
    Result := GridView.GroupedColumns[AIndex];
end;

function TcxGridColumnHeaderMovingObject.GetRightGroupColumn: TcxGridColumn;
var
  AUseGroupIndex: Boolean;
  AIndex: Integer;
begin
  AUseGroupIndex := (SourceItem.GroupIndex <> -1) and (SourceItem.GroupIndex = DestZone.ItemIndex);
  if AUseGroupIndex then
    AIndex := SourceItem.GroupIndex + 1
  else
    AIndex := DestZone.ItemIndex;
  if AIndex < GridView.GroupedColumnCount then
    Result := GridView.GroupedColumns[AIndex]
  else
    Result := nil;
end;

function TcxGridColumnHeaderMovingObject.GetSourceItem: TcxGridColumn;
begin
  Result := TcxGridColumn(inherited SourceItem);
end;

function TcxGridColumnHeaderMovingObject.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

function TcxGridColumnHeaderMovingObject.MergeWithLeftColumn: Boolean;
begin
  Result := IsMerging and AllowMergeWithLeftColumn and ((SourceItem.GroupIndex = -1) or
    (SourceItem.GroupIndex <> DestZone.ItemIndex - 1) or SourceItem.IsChildInMergedGroup);
end;

function TcxGridColumnHeaderMovingObject.MergeWithRightColumn: Boolean;
begin
  Result := IsMerging and AllowMergeWithRightColumn and ((SourceItem.GroupIndex = -1) or
    (SourceItem.GroupIndex <> DestZone.ItemIndex) or GetRightGroupColumn.IsChildInMergedGroup);
end;

procedure TcxGridColumnHeaderMovingObject.SetSourceItem(Value: TcxGridColumn);
begin
  inherited SourceItem := Value;
end;

function TcxGridColumnHeaderMovingObject.UpdateColumnPosition: Boolean;
begin
  case DestItemContainerKind of
    ckGroupByBox:
      Result := ColumnGrouping;
    ckHeader:
      Result := ColumnMovingToHeader
  else
    Result := ColumnRemoving;
  end;
end;

{ TcxCustomGridSizingObject }

function TcxCustomGridSizingObject.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxCustomGridSizingObject.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridSizingObject.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

procedure TcxCustomGridSizingObject.SetDestPointX(Value: Integer);
begin
  if FDestPointX <> Value then
  begin
    Dirty := True;
    FDestPointX := Value;
  end;
end;

procedure TcxCustomGridSizingObject.SetDestPointY(Value: Integer);
begin
  if FDestPointY <> Value then
  begin
    Dirty := True;
    FDestPointY := Value;
  end;
end;

procedure TcxCustomGridSizingObject.DirtyChanged;
begin
  Canvas.InvertRect(SizingMarkBounds);
end;

procedure TcxCustomGridSizingObject.BeforeScrolling;
begin
  Control.FinishDragAndDrop(False);
end;

function TcxCustomGridSizingObject.GetCurrentSize: Integer;
begin
  Result := OriginalSize + DeltaSize;
end;

function TcxCustomGridSizingObject.GetDeltaSize: Integer;
begin
  if IsHorizontalSizing then
    if GridView.UseRightToLeftAlignment then
      Result := SourcePoint.X - DestPointX
    else
      Result := DestPointX - SourcePoint.X
  else
    Result := DestPointY - SourcePoint.Y;
end;

function TcxCustomGridSizingObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if IsHorizontalSizing then
    Result := crcxGridHorzSize
  else
    Result := crcxGridVertSize;
end;

function TcxCustomGridSizingObject.GetHorzSizingMarkBounds: TRect;
var
  AItemBounds: TRect;
begin
  AItemBounds := SizingItemBounds;
  Result.Right := AItemBounds.Left + CurrentSize;
  Result.Left := Result.Right - SizingMarkWidth;
  Result.Top := AItemBounds.Top;
  Result.Bottom := ViewInfo.Bounds.Bottom - ViewInfo.PartsBottomHeight;
  if GridView.UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, AItemBounds);
end;

function TcxCustomGridSizingObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TcxCustomGridSizingObject.GetIsHorizontalSizing: Boolean;
begin
  Result := True;
end;

function TcxCustomGridSizingObject.GetSizingMarkBounds: TRect;
begin
  if IsHorizontalSizing then
    Result := GetHorzSizingMarkBounds
  else
    Result := GetVertSizingMarkBounds;
end;

function TcxCustomGridSizingObject.GetVertSizingMarkBounds: TRect;
begin
  with Result do
  begin
    Left := ViewInfo.Bounds.Left;
    Right := ViewInfo.Bounds.Right;
    Bottom := SizingItemBounds.Top + CurrentSize;
    Top := Bottom - SizingMarkWidth;
  end;
end;

procedure TcxCustomGridSizingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  if IsHorizontalSizing then
    DestPointX := P.X
  else
    DestPointY := P.Y;
  Accepted := True;
  inherited;
end;

procedure TcxCustomGridSizingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  FDestPointX := SourcePoint.X;
  FDestPointY := SourcePoint.Y;
end;

{ TcxCustomGridColumnSizingObject }

function TcxCustomGridColumnSizingObject.GetColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo;
begin
  Result := ViewInfo.HeaderViewInfo[Column.VisibleIndex];
end;

function TcxCustomGridColumnSizingObject.GetSizingItemBounds: TRect;
begin
  Result := ColumnHeaderViewInfo.Bounds;
end;

function TcxCustomGridColumnSizingObject.GetSizingMarkWidth: Integer;
begin
  Result := ColumnSizingMarkWidth;
end;

procedure TcxCustomGridColumnSizingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  Column := (AParams as TcxCustomGridColumnHitTest).Column;
end;

{ TcxGridColumnHorzSizingObject }

procedure TcxGridColumnHorzSizingObject.BeginDragAndDrop;
begin
  OriginalSize := ColumnHeaderViewInfo.Width;
  Controller.FHorzSizingColumn := Column;
  inherited;
end;

procedure TcxGridColumnHorzSizingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  Controller.FHorzSizingColumn := nil;
  if Accepted and (CurrentSize <> OriginalSize) then
  begin
    Column.ForceWidth(ColumnHeaderViewInfo.CalculateOriginalWidth(CurrentSize));
    GridView.DoColumnSizeChanged(Column);
  end;
end;

function TcxGridColumnHorzSizingObject.GetCurrentSize: Integer;
begin
  Result := inherited GetCurrentSize;
  ColumnHeaderViewInfo.CheckWidth(Result);
end;

{ TcxGridRowSizingObject }

function TcxGridRowSizingObject.GetRowViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := TcxCustomGridRowViewInfo(FRow.ViewInfo);
end;

procedure TcxGridRowSizingObject.BeginDragAndDrop;
begin
  OriginalSize := RowViewInfo.RowHeight;
  inherited;
end;

procedure TcxGridRowSizingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  if Accepted then
    RowViewInfo.RowHeight := CurrentSize;
end;

function TcxGridRowSizingObject.GetCurrentSize: Integer;
begin
  Result := inherited GetCurrentSize;
  RowViewInfo.CheckRowHeight(Result);
end;

function TcxGridRowSizingObject.GetIsHorizontalSizing: Boolean;
begin
  Result := False;
end;

function TcxGridRowSizingObject.GetSizingItemBounds: TRect;
begin                          //!!!
  Result := RowViewInfo.Bounds;
end;

function TcxGridRowSizingObject.GetSizingMarkWidth: Integer;
begin
  Result := RowSizingMarkWidth;
end;

procedure TcxGridRowSizingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  FRow := TcxCustomGridRow((AParams as TcxGridRowSizingEdgeHitTest).GridRecord);
end;

{ TcxGridTableItemsListBox }

constructor TcxGridTableItemsListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateBackgroundColor;
end;

function TcxGridTableItemsListBox.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableItemsListBox.GetTextColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderTextColor;
end;

function TcxGridTableItemsListBox.CalculateItemHeight: Integer;
begin
  Result := 2 * (LookAndFeelPainter.HeaderBorderSize + ScaleFactor.Apply(cxGridCellTextOffset)) + cxTextHeight(Font);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

procedure TcxGridTableItemsListBox.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateBackgroundColor;
end;

procedure TcxGridTableItemsListBox.PaintItem(
  ACanvas: TcxCanvas; R: TRect; AIndex: Integer; AFocused: Boolean);
const
  States: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsHot);
var
  ATextR, ASelectionR: TRect;
  AHorzAlignment: TAlignment;
begin
  ASelectionR := R;
  ACanvas.Font.Color := TextColor;
  InflateRect(ASelectionR, -LookAndFeelPainter.HeaderBorderSize, -LookAndFeelPainter.HeaderBorderSize);
  ATextR := ASelectionR;
  InflateRect(ATextR, -ScaleFactor.Apply(cxGridCellTextOffset), 0);
  AHorzAlignment := taLeftJustify;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AHorzAlignment);
  if UseRightToLeftReading then
    ACanvas.Canvas.TextFlags := ACanvas.Canvas.TextFlags or ETO_RTLREADING;
  LookAndFeelPainter.DrawScaledHeader(ACanvas, R, ATextR, [],
    cxBordersAll, States[AFocused], AHorzAlignment, vaCenter, False, GetItemEndEllipsis, Items[AIndex],
    ACanvas.Font, ACanvas.Font.Color, Style.Color, ScaleFactor, DrawItemDrawBackgroundHandler);
  if AFocused then
    LookAndFeelPainter.DrawHeaderPressed(ACanvas, ASelectionR);
end;

procedure TcxGridTableItemsListBox.UpdateBackgroundColor;
begin
  Style.Color := LookAndFeelPainter.GetCustomizationFormListBackgroundColor;
end;

{ TcxGridTableColumnsListBox }

procedure TcxGridTableColumnsListBox.DoRefreshItems;
begin
  inherited;
  RefreshItemsAsTableItems;
end;

function TcxGridTableColumnsListBox.DrawItemDrawBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin
  Result := GridView.ViewInfo.HeaderViewInfo.DrawColumnBackgroundHandler(ACanvas, ABounds);
end;

function TcxGridTableColumnsListBox.GetDragAndDropParams: TcxCustomGridHitTest;
begin
  Result := TcxGridColumnHeaderHitTest.Instance(Point(-1, -1));
  with TcxGridColumnHeaderHitTest(Result) do
  begin
    GridView := Self.GridView;
    Column := TcxGridColumn(DragAndDropItem);
    ColumnContainerKind := ckCustomizationForm;
  end;
end;

function TcxGridTableColumnsListBox.GetItemEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.HeaderEndEllipsis;
end;

{ TcxGridTableCustomizationForm }

function TcxGridTableCustomizationForm.GetColumnsListBox: TcxGridTableColumnsListBox;
begin
  Result := TcxGridTableColumnsListBox(ItemsListBox);
end;

function TcxGridTableCustomizationForm.GetColumnsPage: TcxTabSheet;
begin
  Result := ItemsPage;
end;

function TcxGridTableCustomizationForm.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridTableCustomizationForm.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableCustomizationForm.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

function TcxGridTableCustomizationForm.GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass;
begin
  Result := TcxGridTableColumnsListBox;
end;

function TcxGridTableCustomizationForm.GetItemsPageCaption: string;
begin
  Result := cxGetResourceString(@scxGridCustomizationFormColumnsPageCaption);
end;

{ TcxGridDragOpenInfoMasterDataRowTab }

constructor TcxGridDragOpenInfoMasterDataRowTab.Create(ALevel: TcxGridLevel;
  AGridRow: TcxGridMasterDataRow);
begin
  inherited Create(ALevel);
  GridRow := AGridRow;
end;

function TcxGridDragOpenInfoMasterDataRowTab.Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  Result := inherited Equals(AInfo) and
    (GridRow = TcxGridDragOpenInfoMasterDataRowTab(AInfo).GridRow);
end;

procedure TcxGridDragOpenInfoMasterDataRowTab.Run;
begin
  GridRow.ActiveDetailIndex := Level.Index;
end;

{ TcxGridColumnsCustomizationPopup }

function TcxGridColumnsCustomizationPopup.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridColumnsCustomizationPopup.DoItemPosChanged(AItem: TObject);
begin
  GridView.DoColumnPosChanged(TcxGridColumn(AItem));
end;

procedure TcxGridColumnsCustomizationPopup.SetItemIndex(AItem: TObject; AIndex: Integer);
begin
  inherited;
  DoItemPosChanged(AItem);
end;

{ TcxGridTableEditingController }

destructor TcxGridTableEditingController.Destroy;
begin
  FreeAndNil(FDelayedFilteringTimer);
  inherited Destroy;
end;

procedure TcxGridTableEditingController.HideEdit(Accept: Boolean);
begin
  FreeAndNil(FDelayedFilteringTimer);
  inherited HideEdit(Accept);
end;

procedure TcxGridTableEditingController.ApplyFilterRowFiltering;
begin
  ApplyingImmediateFiltering := True;
  try
    EditingItem.EditValue := Edit.EditingValue;
  finally
    ApplyingImmediateFiltering := False;
  end;
end;

function TcxGridTableEditingController.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridTableEditingController.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableEditingController.CanInitEditing: Boolean;
begin
  if Controller.IsFilterRowFocused then
    Result := True
  else
    Result := inherited CanInitEditing;
end;

function TcxGridTableEditingController.CanUpdateEditValue: Boolean;
begin
  Result := inherited CanUpdateEditValue and not ApplyingImmediateFiltering;
end;

procedure TcxGridTableEditingController.CheckInvalidateUpdateButton;
begin
  if not GridView.InplaceEditForm.Visible then
    Exit;
  UpdateButtonEnabled := IsRecordModified;
end;

procedure TcxGridTableEditingController.DoEditChanged;
begin
  inherited DoEditChanged;
  if Controller.IsFilterRowFocused then
  begin
    case GridView.FilterRow.ApplyChanges of
      fracImmediately:
        ApplyFilterRowFiltering;
      fracDelayed:
        StartDelayedFiltering;
    end;
  end
  else
    CheckInvalidateUpdateButton;
end;

procedure TcxGridTableEditingController.DoEditKeyDown(var Key: Word; Shift: TShiftState);
var
  ACheckModified: Boolean;
begin
  ACheckModified := Key = VK_ESCAPE;
  if Controller.IsFilterRowFocused then
  begin
    ACheckModified := False;
    case Key of
      VK_ESCAPE:
        if GridView.FilterRow.ApplyChanges in [fracImmediately, fracDelayed] then
        begin
          Edit.Reset;
          if GridView.FilterRow.ApplyChanges = fracDelayed then
            FreeAndNil(FDelayedFilteringTimer);
        end;
      VK_RETURN:
        if GridView.FilterRow.ApplyChanges = fracDelayed then
          FreeAndNil(FDelayedFilteringTimer);
    end;
  end;
  inherited DoEditKeyDown(Key, Shift);
  if ACheckModified then
    CheckInvalidateUpdateButton;
end;

function TcxGridTableEditingController.GetHideEditOnFocusedRecordChange: Boolean;
begin
  Result := inherited GetHideEditOnFocusedRecordChange or
    GridView.ViewData.HasFilterRow;
end;

procedure TcxGridTableEditingController.InitEdit;

  procedure AddFilterValues(AStrings: TStrings);
  var
    AValueList: TcxGridFilterValueList;
    I: Integer;
  begin
    AValueList := Controller.ViewData.CreateFilterValueList;
    try
      EditingItem.DataBinding.GetFilterValues(AValueList, True, False, True);
      with AStrings do
      begin
        BeginUpdate;
        try
          for I := 0 to AValueList.Count - 1 do
            if VarIsArray(AValueList[I].Value) then
              Add(AValueList[I].DisplayText)
            else
              Add(AValueList[I].Value);
        finally
          EndUpdate;
        end;
      end;
    finally
      AValueList.Free;
    end;
  end;

begin
  inherited;
  if Controller.IsFilterRowFocused then
  begin
    // won't work if both Properties and RepositoryItem are assigned to column
    Edit.InternalProperties.ReadOnly := False;
    if (GridView.FilterRow.ApplyChanges = fracOnCellExit) and
      (Edit is TcxCustomTextEdit) and (Edit.ActiveProperties = TcxCustomTextEdit(Edit).Properties) then
      AddFilterValues(TcxCustomTextEdit(Edit).Properties.LookupItems);
  end;
end;

function TcxGridTableEditingController.IsNeedInvokeEditChangedEventsBeforePost: Boolean;
begin
  Result := inherited IsNeedInvokeEditChangedEventsBeforePost and
    not Controller.IsFilterRowFocused;
end;

function TcxGridTableEditingController.NeedCellViewInfoWhenUpdateEditValue: Boolean;
begin
  Result := inherited NeedCellViewInfoWhenUpdateEditValue and not Controller.IsFilterRowFocused;
end;

procedure TcxGridTableEditingController.OnDelayedFilteringTimer(
  Sender: TObject);
begin
  FreeAndNil(FDelayedFilteringTimer);
  ApplyFilterRowFiltering;
end;

procedure TcxGridTableEditingController.PostEditingData;
begin
  if Controller.IsFilterRowFocused then
    UpdateValue
  else
    inherited PostEditingData;
end;

procedure TcxGridTableEditingController.ResetUpdateButtonEnabled;
begin
  FUpdateButtonEnabled := False;
end;

procedure TcxGridTableEditingController.SetUpdateButtonEnabled(
  const Value: Boolean);
begin
  if FUpdateButtonEnabled <> Value then
  begin
    FUpdateButtonEnabled := Value;
    if not (dceModified in GridView.DataController.EditState) then
    begin
      GridView.InplaceEditForm.InvalidateUpdateButton;
      GridView.ViewInfo.IndicatorViewInfo.Invalidate;
    end;
  end;
end;

procedure TcxGridTableEditingController.StartDelayedFiltering;
begin
  FreeAndNil(FDelayedFilteringTimer);
  FDelayedFilteringTimer := TcxTimer.Create(nil);
  FDelayedFilteringTimer.Interval := GridView.FilterRow.ApplyInputDelay;
  FDelayedFilteringTimer.OnTimer := OnDelayedFilteringTimer;
end;

{ TcxGridTableController }

constructor TcxGridTableController.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FSelectedColumns := TList.Create;
end;

destructor TcxGridTableController.Destroy;
begin
  FreeAndNil(FSelectedColumns);
  FreeAndNil(FFilterRowOperatorMenu);
  FreeAndNil(FDataRowFixingMenu);
  inherited Destroy;
end;

function TcxGridTableController.GetColumnsCustomizationPopup: TcxGridColumnsCustomizationPopup;
begin
  Result := TcxGridColumnsCustomizationPopup(ItemsCustomizationPopup);
end;

function TcxGridTableController.GetCustomizationForm: TcxGridTableCustomizationForm;
begin
  Result := TcxGridTableCustomizationForm(inherited CustomizationForm);
end;

function TcxGridTableController.GetDataRowFixingMenu: TcxGridDataRowFixingMenu;
begin
  if not HasDataRowFixingMenu then
  begin
    FDataRowFixingMenu := TcxGridDataRowFixingMenu.CreateEx(Site, Site);
    FDataRowFixingMenu.Images := cxDataRowFixingImages;
  end;
  Result := FDataRowFixingMenu;
end;

function TcxGridTableController.GetEditingController: TcxGridTableEditingController;
begin
  Result := TcxGridTableEditingController(inherited EditingController);
end;

function TcxGridTableController.GetFilterRowOperatorMenu: TcxFilterDropDownMenu;
begin
  if not HasFilterRowOperatorMenu then
    FFilterRowOperatorMenu := TcxFilterDropDownMenu.Create(Site);
  Result := FFilterRowOperatorMenu;
end;

function TcxGridTableController.GetFocusedColumn: TcxGridColumn;
begin
  Result := TcxGridColumn(FocusedItem);
end;

function TcxGridTableController.GetFocusedColumnIndex: Integer;
begin
  Result := FocusedItemIndex;
end;

function TcxGridTableController.GetFocusedRow: TcxCustomGridRow;
begin
  Result := TcxCustomGridRow(FocusedRecord);
end;

function TcxGridTableController.GetFocusedRowIndex: Integer;
begin
  Result := FocusedRecordIndex;
end;

function TcxGridTableController.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableController.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := GridView.InplaceEditForm;
end;

function TcxGridTableController.GetIsColumnHorzSizing: Boolean;
begin
  Result := FHorzSizingColumn <> nil;
end;

function TcxGridTableController.GetSelectedColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(FSelectedColumns[Index]);
end;

function TcxGridTableController.GetSelectedColumnCount: Integer;
begin
  Result := FSelectedColumns.Count;
end;

function TcxGridTableController.GetSelectedRow(Index: Integer): TcxCustomGridRow;
begin
  Result := TcxCustomGridRow(SelectedRecords[Index]);
end;

function TcxGridTableController.GetSelectedRowCount: Integer;
begin
  Result := SelectedRecordCount;
end;

function TcxGridTableController.GetTopRowIndex: Integer;
begin
  Result := TopRecordIndex;
end;

function TcxGridTableController.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

function TcxGridTableController.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

procedure TcxGridTableController.SetFocusedColumn(Value: TcxGridColumn);
begin
  FocusedItem := Value;
end;

procedure TcxGridTableController.SetFocusedColumnIndex(Value: Integer);
begin
  FocusedItemIndex := Value;
end;

procedure TcxGridTableController.SetFocusedRow(Value: TcxCustomGridRow);
begin
  FocusedRecord := Value;
end;

procedure TcxGridTableController.SetFocusedRowIndex(Value: Integer);
begin
  FocusedRecordIndex := Value;
end;

procedure TcxGridTableController.SetFocusedItemKind(AValue: TcxGridFocusedItemKind);
var
  ARow: TcxCustomGridRecord;
begin
  if AValue <> FFocusedItemKind then
  begin
    FFocusedItemKind := AValue;
    if GridView.Visible and not GridView.IsUpdateLocked then
    begin
      ARow := ViewData.GetRecordByRecordIndex(InplaceEditForm.EditingRecordIndex);
      if InplaceEditForm.Visible and (ARow.ViewInfo is TcxGridDataRowViewInfo) then
        TcxGridDataRowViewInfo(ARow.ViewInfo).InplaceEditFormAreaViewInfo.ButtonsPanelViewInfo.Invalidate;
    end;
  end;
end;

procedure TcxGridTableController.SetLeftPos(Value: Integer);
var
  APrevLeftPos: Integer;
begin
  CheckLeftPos(Value);
  if FLeftPos <> Value then
  begin
    TcxGridSiteAccess(Site).ShowTouchScrollUI(Site, True);
    APrevLeftPos := FLeftPos;
    FLeftPos := Value;
    if GridView.CanOffsetHorz then
      if UseRightToLeftAlignment then
        GridView.Offset(0, 0, -APrevLeftPos + FLeftPos, 0)
      else
        GridView.Offset(0, 0, APrevLeftPos - FLeftPos, 0)
    else
      LeftPosChanged;
    GridView.DoLeftPosChanged;
  end;
end;

procedure TcxGridTableController.SetPressedColumn(Value: TcxGridColumn);
var
  R1, R2: TRect;

  procedure GetUpdateRects;

    function GetUpdateRect(AContainerViewInfo: TcxGridColumnContainerViewInfo;
      AIndex: Integer): TRect;
    begin
      if AIndex <> -1 then
        Result := AContainerViewInfo[AIndex].Bounds
      else
        Result := Rect(0, 0, 0, 0);
    end;

  begin
    R1 := GetUpdateRect(ViewInfo.GroupByBoxViewInfo, FPressedColumn.GroupIndex);
    R2 := GetUpdateRect(ViewInfo.HeaderViewInfo, FPressedColumn.VisibleIndex);
  end;

  procedure InvalidateRects;
  begin
    GridView.ViewChanged(R1);
    GridView.ViewChanged(R2);
  end;

begin
  if FPressedColumn <> Value then
    if ViewInfo.IsCalculating then
      FPressedColumn := Value
    else
    begin
      if Value = nil then GetUpdateRects;
      FPressedColumn := Value;
      if Value <> nil then GetUpdateRects;
      InvalidateRects;
    end;
end;

procedure TcxGridTableController.SetTopRowIndex(Value: Integer);
begin
  TopRecordIndex := Value;
end;

procedure TcxGridTableController.AddSelectedColumn(AColumn: TcxGridColumn);
begin
  AColumn.FSelected := True;
  FSelectedColumns.Add(AColumn);
  //GridView.LayoutChanged;
  InvalidateSelection;
end;

procedure TcxGridTableController.RemoveSelectedColumn(AColumn: TcxGridColumn);
begin
  AColumn.FSelected := False;
  FSelectedColumns.Remove(AColumn);
  //GridView.LayoutChanged;
  InvalidateSelection;
end;

procedure TcxGridTableController.AdjustRowPositionForFixedGroupMode(ARow: TcxCustomGridRow);
var
  APosition, AAdjustedPosition: Integer;
begin
  APosition := DoGetScrollBarPos;
  AAdjustedPosition := ViewInfo.RecordsViewInfo.GetAdjustedScrollPositionForFixedGroupMode(ARow, APosition);
  if APosition <> AAdjustedPosition then
    ScrollBarPos := AAdjustedPosition;
end;

function TcxGridTableController.CanAppend(ACheckOptions: Boolean): Boolean;
begin
  Result := inherited CanAppend(ACheckOptions) and not ViewData.HasNewItemRecord;
end;

function TcxGridTableController.CanDataPost: Boolean;
begin
  Result := inherited CanDataPost;
  if InplaceEditForm.Visible then
    Result := Result and (InplaceEditForm.CloseQuery = mrYes);
end;

function TcxGridTableController.CanDelete(ACheckOptions: Boolean): Boolean;
begin
  Result := inherited CanDelete(ACheckOptions) and
    not IsFilterRowFocused and not IsNewItemRowFocused;
end;

function TcxGridTableController.CanEdit: Boolean;
begin
  Result := inherited CanEdit and not IsFilterRowFocused;
end;

function TcxGridTableController.CanInsert(ACheckOptions: Boolean): Boolean;
begin
  Result := inherited CanInsert(ACheckOptions) and not IsFilterRowFocused;
end;

function TcxGridTableController.CanMakeItemVisible(AItem: TcxCustomGridTableItem): Boolean;

  function IsValidItemVisibleIndex: Boolean;
  begin
    Result := (AItem.VisibleIndex <> -1) and (AItem.VisibleIndex < ViewInfo.HeaderViewInfo.Count);{!!!}
  end;

  function IsItemVisibleInEditForm: Boolean;
  begin
    Result := InplaceEditForm.Visible and (AItem as TcxGridColumn).IsVisibleForEditForm;
  end;

begin
  Result := (AItem <> nil) and
    (IsValidItemVisibleIndex or IsItemVisibleInEditForm);
end;

function TcxGridTableController.CanUseAutoHeightEditing: Boolean;
begin
  Result := not IsSpecialRowFocused;
end;

function TcxGridTableController.CheckBrowseModeOnRecordChanging(ANewRecordIndex: Integer): Boolean;
var
  ARecord: TcxCustomGridRecord;
begin
  Result := inherited CheckBrowseModeOnRecordChanging(ANewRecordIndex);
  if Result then
  begin
    if InplaceEditForm.Visible then
    begin
      ARecord := ViewData.GetRecordByIndex(ANewRecordIndex);
      if (ARecord <> nil) and ((InplaceEditForm.EditingRecordIndex <> ARecord.RecordIndex) or not ARecord.IsData) then
      begin
        if GridView.OptionsBehavior.AlwaysShowEditor then
          EditingController.UpdateValue;
        Result := InplaceEditForm.Close(False);
      end
      else
        Result := False;
    end;
  end;
end;

procedure TcxGridTableController.CheckCoordinates;
begin
  inherited;
  LeftPos := LeftPos;
end;

procedure TcxGridTableController.CheckLeftPos(var Value: Integer);
begin
  if Value > ViewInfo.GetScrollContentWidth - ViewInfo.ClientWidth then
    Value := ViewInfo.GetScrollContentWidth - ViewInfo.ClientWidth;
  if Value < 0 then Value := 0;
end;

procedure TcxGridTableController.CheckInternalTopRecordIndex(var AIndex: Integer);
begin
  if GridView.IsFixedGroupsMode then
  begin
    AIndex := cxRecordIndexNone;
    if ScrollRecordCount > 0 then
      AIndex := LastScrollRecordIndex;
  end
  else
    inherited CheckInternalTopRecordIndex(AIndex);
end;

procedure TcxGridTableController.DoMakeRecordVisible(ARecord: TcxCustomGridRecord);
var
  ARow: TcxCustomGridRow;
begin
  ARow := TcxCustomGridRow(ARecord);
  if ARow.FixedState = rfsNotFixed then
    inherited DoMakeRecordVisible(ARow);
  if GridView.IsFixedGroupsMode and not ARow.IsSpecial then
    AdjustRowPositionForFixedGroupMode(ARow);
end;

procedure TcxGridTableController.FocusedItemChanged(APrevFocusedItem: TcxCustomGridTableItem);

  procedure CheckFocusedItemKind;
  begin
    if FocusedItem <> nil then
      FocusedItemKind := fikGridItem
    else
      if not InplaceEditForm.Visible then
        FocusedItemKind := fikNone;
  end;

begin
  if not GridView.IsDestroying then
    CheckFocusedItemKind;
  if CellSelectionAnchor = nil then
    CellSelectionAnchor := FocusedColumn;
  inherited;
end;

procedure TcxGridTableController.FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
  APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
begin
  if IsFilterRowFocused and KeepFilterRowFocusing then Exit;
  inherited;
  if ViewData.HasFilterRow and
    ((AFocusedRecordIndex <> APrevFocusedRecordIndex) or ANewItemRecordFocusingChanged) then
    ViewData.FilterRow.Focused := False;
end;

function TcxGridTableController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    if AHitTest is TcxGridFooterCellHitTest then
      Result := TcxGridFooterCellHitTest(AHitTest).SummaryItem <> nil
    else
      Result := AHitTest.HitTestCode in [htColumnHeader, htExpandButton, htTab,
        htIndicatorHeader, htGroupSummary];
end;

function TcxGridTableController.GetDlgCode: Integer;
begin
  Result := inherited GetDlgCode;
  if InplaceEditForm.Visible then
    Result := Result or DLGC_WANTALLKEYS;
end;

function TcxGridTableController.GetFilterRowSupportedOperators: TcxFilterOperatorKinds;
begin
  if GridView.FilterRow.OperatorCustomization then
    Result := [foEqual..foNotLike, foContains..foEndsWith]
  else
    Result := [foEqual];
end;

function TcxGridTableController.GetFocusedRecord: TcxCustomGridRecord;
begin
  if ViewData.HasFilterRow and ViewData.FilterRow.Selected then
    Result := ViewData.FilterRow
  else
  begin
    Result := inherited GetFocusedRecord;
    if (Result = nil) and ViewData.HasNewItemRecord and ViewData.NewItemRow.Selected then
      Result := ViewData.NewItemRow;
  end;
end;

function TcxGridTableController.GetGroupIndexByFocusedRowIndex: Integer;
begin
  Result := GetGroupIndexByRowIndex(FocusedRowIndex);
end;

function TcxGridTableController.GetGroupIndexByRowIndex(AIndex: Integer): Integer;
begin
  Result := DataController.Groups.DataGroupIndexByRowIndex[AIndex];
end;

function TcxGridTableController.GetIsRecordsScrollHorizontal: Boolean;
begin
  Result := False;
end;

function TcxGridTableController.GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass;
begin
  Result := TcxGridColumnsCustomizationPopup;
end;

function TcxGridTableController.GetMaxTopRecordIndexValue: Integer;
begin
  if NeedsAdditionalRowsScrolling then
    Result := InternalTopRecordIndex + 1
  else
    Result := inherited GetMaxTopRecordIndexValue;
end;

function TcxGridTableController.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskVertical;
end;

function TcxGridTableController.GetScrollBarRecordCount: Integer;
begin
  Result := inherited GetScrollBarRecordCount;
  if NeedsAdditionalRowsScrolling then
    Inc(Result);
end;

function TcxGridTableController.GetTopRecordIndex: Integer;
begin
  Result := inherited GetTopRecordIndex;
  if (Result <> -1) and GridView.IsFixedGroupsMode then
    Result := ViewData.GetTopGroupIndex(Result);
end;

function TcxGridTableController.HasDataRowFixingMenu: Boolean;
begin
  Result := FDataRowFixingMenu <> nil;
end;

function TcxGridTableController.HasFilterRowOperatorMenu: Boolean;
begin
  Result := FFilterRowOperatorMenu <> nil;
end;

function TcxGridTableController.IsColumnFixedDuringHorzSizing(AColumn: TcxGridColumn): Boolean;
begin
  Result :=
    (AColumn = ForcingWidthItem) or
    not ForcingWidthItem.IsLast and (AColumn.VisibleIndex < ForcingWidthItem.VisibleIndex);
end;

function TcxGridTableController.IsFirstPageRecordFocused: Boolean;
var
  ARecordViewInfo: TcxCustomGridRecordViewInfo;
begin
  Result := inherited IsFirstPageRecordFocused;
  if not Result and (FocusedRow <> nil) and GridView.IsFixedGroupsMode then
  begin
    Result := not FocusedRow.IsData and (FocusedRow.FixedState = rfsFixedToTop);
    if not Result then
    begin
      ARecordViewInfo := ViewInfo.RecordsViewInfo.GetRealItem(FocusedRow);
      if ARecordViewInfo <> nil then
        Result := ARecordViewInfo.Bounds.Top = ViewInfo.RecordsViewInfo.GetFixedGroupsBottomBound;
    end;
  end;
end;

function TcxGridTableController.IsKeyForMultiSelect(AKey: Word; AShift: TShiftState;
  AFocusedRecordChanged: Boolean): Boolean;
begin
  Result := inherited IsKeyForMultiSelect(AKey, AShift, AFocusedRecordChanged) or
    (AKey = VK_UP) or (AKey = VK_DOWN) or
    ((AKey = VK_HOME) or (AKey = VK_END)) and
      (not GridView.OptionsSelection.CellSelect or (FocusedRecord = nil) or not FocusedRecord.HasCells);
end;

function TcxGridTableController.IsPixelScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbHorizontal;
end;

procedure TcxGridTableController.LeftPosChanged;
begin
  GridView.LayoutChanged;
end;

procedure TcxGridTableController.MakeMasterRecordVisible(ARecord: TcxCustomGridRecord);

  function GetRecordDataHeight(AIndex: Integer): Integer;
  var
    ANewlyCreated: Boolean;
    ARecordViewInfo: TcxGridMasterDataRowViewInfo;
  begin
    ARecordViewInfo := ViewInfo.RecordsViewInfo.GetRecordViewInfo(AIndex, ANewlyCreated) as TcxGridMasterDataRowViewInfo;
    try
      Result := ARecordViewInfo.DataHeight;
    finally
      if ANewlyCreated then
        ARecordViewInfo.Free;
    end;
  end;

  function GetDetailRecordOffset(AMasterRecordDataHeight: Integer): Integer;
  var
    ATopRecord: TcxCustomGridRecord;
  begin
    ATopRecord := ViewData.Records[TopRecordIndex];
    Result := ARecord.PixelScrollPosition - ATopRecord.PixelScrollPosition +
      ViewInfo.RecordsViewInfo.ContentBounds.Top + AMasterRecordDataHeight + PixelScrollRecordOffset;
  end;

const
  UnassignedPixelScrollOffset = MaxInt;
  RecordIndexNone = -1;

var
  AIndex: Integer;
  AContentBounds: TRect;
  APixelScrollRecordOffset, ATopRecordIndex: Integer;
  AOverPan: Boolean;
  ADetailRecordBounds: TRect;
  ADataHeight: Integer;
  AOffset: TPoint;
  I: Integer;
begin
  AIndex := ARecord.Index;
  if AIndex <> -1 then
  begin
    AContentBounds := ViewInfo.RecordsViewInfo.ContentBounds;
    ADataHeight := GetRecordDataHeight(AIndex);
    if IsRecordPixelScrolling then
    begin
      ADetailRecordBounds := cxRectOffset(MakeVisibleDetailRecordBounds, 0, GetDetailRecordOffset(ADataHeight));

      if AIndex = TopRecordIndex then
      begin
        if (ADetailRecordBounds.Top < AContentBounds.Top) or (ADetailRecordBounds.Height > AContentBounds.Height) then
        begin
          ATopRecordIndex := AIndex;
          APixelScrollRecordOffset := Min(AContentBounds.Top - ADetailRecordBounds.Top + PixelScrollRecordOffset, 0);
          CheckTopRecordIndexAndOffset(ATopRecordIndex, APixelScrollRecordOffset);
          SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
        end
        else
          if ADetailRecordBounds.Bottom > AContentBounds.Bottom then
          begin
            ATopRecordIndex := AIndex;
            APixelScrollRecordOffset := AContentBounds.Bottom - ADetailRecordBounds.Bottom + PixelScrollRecordOffset;
            CheckTopRecordIndexAndOffset(ATopRecordIndex, APixelScrollRecordOffset);
            SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
          end;
      end
      else
        if AIndex < TopRecordIndex then
        begin
          ATopRecordIndex := AIndex;
          APixelScrollRecordOffset := - ADataHeight - MakeVisibleDetailRecordBounds.Top;
          CheckTopRecordIndexAndOffset(ATopRecordIndex, APixelScrollRecordOffset);
          SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
        end
        else
          if AIndex >= TopRecordIndex + ViewInfo.VisibleRecordCount then
          begin
            if ADetailRecordBounds.Bottom > AContentBounds.Bottom then
            begin
              ATopRecordIndex := AIndex;
              APixelScrollRecordOffset := - ADataHeight - MakeVisibleDetailRecordBounds.Bottom;
              ViewInfo.RecordsViewInfo.CalculatePixelScrollInfo(ATopRecordIndex, APixelScrollRecordOffset,
                RecordIndexNone, UnassignedPixelScrollOffset, ViewInfo.RecordsViewInfo.GetPixelScrollContentSize, AOverPan);
              SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
            end;
          end;
      MakeVisibleDetailRecordBounds := cxRectOffset(MakeVisibleDetailRecordBounds, 0, GetDetailRecordOffset(ADataHeight));
    end
    else
    begin
      InternalDoMakeRecordVisible(ARecord);
      AOffset := cxNullPoint;
      for I := TopRecordIndex to ARecord.Index - 1 do
        AOffset.Y := AOffset.Y + ViewInfo.RecordsViewInfo.GetRecordScrollSize(I);
      AOffset.Y := AOffset.Y + ViewInfo.RecordsViewInfo.ContentBounds.Top + ADataHeight;
      MakeVisibleDetailRecordBounds := cxRectOffset(MakeVisibleDetailRecordBounds, AOffset);
    end;
  end;

  GridView.MakeMasterGridRecordVisible;
end;

function TcxGridTableController.NeedsAdditionalRowsScrolling(AIsCallFromMaster: Boolean = False): Boolean;

  function IsMaxScrollPos: Boolean;
  begin
    Result := ScrollBarPos = inherited GetScrollBarRecordCount - ViewInfo.VisibleRecordCount;
  end;

var
  ALastRow: TcxGridMasterDataRow;
begin
  Result := False;
  if GridView.IsMaster and not IsRecordPixelScrolling and (ScrollRecordCount <> 0) and
    ((ViewInfo.VisibleRecordCount > 1) or AIsCallFromMaster) and
    (IsMaxScrollPos or AIsCallFromMaster and (ScrollRecordCount = 1)) and
    (ViewData.Rows[LastScrollRecordIndex] is TcxGridMasterDataRow) then
  begin
    ALastRow := ViewData.Rows[LastScrollRecordIndex].AsMasterDataRow;
    if ALastRow.Expanded and ALastRow.ActiveDetailGridViewExists then
      Result :=
        not ALastRow.ActiveDetailGridView.Controller.IsDataFullyVisible(True) and
        TcxGridMasterDataRowViewInfo(ALastRow.ViewInfo).DetailsSiteVisible and
        not TcxGridMasterDataRowViewInfo(ALastRow.ViewInfo).DetailsSiteViewInfo.HasMaxHeight;
  end;
end;

procedure TcxGridTableController.RemoveFocus;
begin
  with ViewData do
  begin
    if HasFilterRow then
      FilterRow.Focused := False;
    if HasNewItemRecord then
      NewItemRow.Focused := False;
  end;
  inherited RemoveFocus;
end;

procedure TcxGridTableController.ScrollData(ADirection: TcxDirection);
begin
  case ADirection of
    dirLeft:
      LeftPos := LeftPos - HScrollDelta;
    dirRight:
      LeftPos := LeftPos + HScrollDelta;
    dirUp:
      ScrollRecords(False, 1);
    dirDown:
      ScrollRecords(True, 1);
  end;
end;

procedure TcxGridTableController.SetFocusedRecord(Value: TcxCustomGridRecord);
var
  AIndex: Integer;
  AGridViewLink: TcxObjectLink;
begin
  if (FocusedRecord <> Value) and ViewData.HasNewItemRecord then
    if Value = ViewData.NewItemRow then
    begin
      Value.Selected := True;
      Value := ViewData.NewItemRow;
    end
    else
      if (FocusedRecord = ViewData.NewItemRow) and (Value = nil) then
        FocusedRecord.Selected := False;

  if ViewData.HasFilterRow then
    if Value = ViewData.FilterRow then
      if FocusedRecord = Value then
        Exit
      else
      begin
        ViewData.FilterRow.Selected := True;
        Value := ViewData.GetRecordByIndex(FocusedRecordIndex);
      end
    else
      if FocusedRow = ViewData.FilterRow then
      begin
        if Value = nil then
          AIndex := -1
        else
          AIndex := Value.Index;
        FocusedRow.Selected := False;
        if AIndex = -1 then
          Exit
        else
          Value := ViewData.GetRecordByIndex(AIndex);
      end;

  AGridViewLink := cxAddObjectLink(GridView);
  try
    inherited SetFocusedRecord(Value);
    if AGridViewLink.Ref <> nil then
    begin
      if ViewData.HasFilterRow and ViewData.FilterRow.Focused and (FocusedColumn = nil) then
        FocusNextItem(FocusedItemIndex, True, False, False, not IsMultiSelectPersistent);
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxGridTableController.ShowDataRowFixingMenu(ADataRow: TcxGridDataRow; AForBounds: TRect);
begin
  DataRowFixingMenu.BiDiMode := Control.BiDiMode;
  if UseRightToLeftAlignment then
    DataRowFixingMenu.AlignHorz := pahRight
  else
    DataRowFixingMenu.AlignHorz := pahLeft;
  DataRowFixingMenu.Popup(ADataRow, AForBounds);
end;

procedure TcxGridTableController.ShowNextPage;
var
  ATopRecordIndex, APixelScrollRecordOffset: Integer;
begin
  if InternalTopRecordIndex <> -1 then
  begin
    ATopRecordIndex := InternalTopRecordIndex + Max(1, GetPageRecordCount - 1);
    if IsRecordPixelScrolling then
    begin
      APixelScrollRecordOffset := 0;
      CheckTopRecordIndexAndOffset(ATopRecordIndex, APixelScrollRecordOffset);
      SetTopRecordIndexWithOffset(ATopRecordIndex, APixelScrollRecordOffset);
    end
    else
      TopRecordIndex := ATopRecordIndex;
  end;
end;

procedure TcxGridTableController.ShowPrevPage;
var
  AVisibleRowCount: Integer;
begin
  if InternalTopRecordIndex = -1 then Exit;
  if InternalTopRecordIndex = 0 then
    if DataController.IsGridMode then
      AVisibleRowCount := GetPageRecordCount
    else
    begin
      if IsRecordPixelScrolling  then
        TopRecordIndex := 0;
      Exit;
    end
  else
  begin
    AVisibleRowCount := GetPageVisibleRecordCount(InternalTopRecordIndex, False);
    if DataController.IsGridMode and (InternalTopRecordIndex - (AVisibleRowCount - 1) = 0) and
      (AVisibleRowCount < GetPageRecordCount) then
      AVisibleRowCount := GetPageRecordCount;
  end;
  if AVisibleRowCount = 1 then
    TopRecordIndex := InternalTopRecordIndex - 1
  else
    if DataController.IsGridMode then
      TopRecordIndex := InternalTopRecordIndex - (AVisibleRowCount - 1)
    else
      InternalTopRecordIndex := InternalTopRecordIndex - (AVisibleRowCount - 1);
end;

function TcxGridTableController.WantSpecialKey(AKey: Word): Boolean;
begin
  Result := inherited WantSpecialKey(AKey) or
    ((AKey = VK_RETURN) and (IsFilterRowFocused or GridView.IsInplaceEditFormMode));
end;

procedure TcxGridTableController.CreateGridViewItem(Sender: TObject);
var
  AItem: TcxCustomGridTableItem;
begin
  if DesignController.CanAddComponent then
  begin
    AItem := CreateViewItem(GridView.PatternGridView);
    DesignController.SelectObject(AItem, True);
    DesignController.NotifyEditors;
  end;
end;

procedure TcxGridTableController.DeleteGridViewItem(AItem: TPersistent);
begin
  if AItem is GridView.GetItemClass then
    if DesignController.CanDeleteComponent(TComponent(AItem)) then
      AItem.Free;
end;

procedure TcxGridTableController.DeleteGridViewItems(Sender: TObject);
var
  I: Integer;
  AList: TObjectList;
  AGridView: TcxCustomGridView;
begin
  AGridView := GridView.PatternGridView;
  AGridView.BeginUpdate;
  try
    AList := TObjectList.Create(False);
    try
      DesignController.GetSelection(AList);
      for I := 0 to AList.Count - 1 do
        if AList[I] is TPersistent then
          DeleteGridViewItem(TPersistent(AList[I]));
    finally
      AList.Free;
    end;
  finally
    AGridView.EndUpdate;
    DesignController.SelectObject(AGridView, True);
  end;
end;

procedure TcxGridTableController.PopulateColumnHeaderDesignPopupMenu(AMenu: TPopupMenu);
begin
  AMenu.Items.Add(NewItem('Create Column', 0, False, True, CreateGridViewItem, 0, 'chmiCreateColumn'));
  AMenu.Items.Add(NewLine);
  AMenu.Items.Add(NewItem('Delete', 0, False, True, DeleteGridViewItems, 0, 'chmiDelete'));
end;

procedure TcxGridTableController.DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);

  procedure ScrollHorizontal;
  begin
    case AScrollCode of
      scLineUp:
        ScrollData(dirLeft);
      scLineDown:
        ScrollData(dirRight);
      scPageUp:
        LeftPos := LeftPos - ViewInfo.ScrollableAreaWidth;
      scPageDown:
        LeftPos := LeftPos + ViewInfo.ScrollableAreaWidth;
      scTrack:
        LeftPos := AScrollPos;
    end;
    AScrollPos := LeftPos;
  end;

  procedure ScrollVertical;
  begin
    case AScrollCode of
      scLineUp:
        ScrollData(dirUp);
      scLineDown:
        ScrollData(dirDown);
      scPageUp:
        ScrollPage(False);
      scPageDown:
        ScrollPage(True);
      scTrack:
        if not DataController.IsGridMode then
          ScrollBarPos := AScrollPos;
      scPosition:
        if DataController.IsGridMode then
          ScrollBarPos := AScrollPos;
    end;
    AScrollPos := ScrollBarPos;
  end;

begin
  inherited DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
  case AScrollBarKind of
    sbHorizontal:
      ScrollHorizontal;
    sbVertical:
      ScrollVertical;
  end;
end;

function TcxGridTableController.GetFirstScrollRecordIndex: Integer;
begin
  Result := inherited GetFirstScrollRecordIndex + ViewData.FixedTopRowCount;
end;

function TcxGridTableController.GetLastScrollRecordIndex: Integer;
begin
  Result := inherited GetLastScrollRecordIndex - ViewData.FixedBottomRowCount;
end;

function TcxGridTableController.CanScrollData(ADirection: TcxDirection): Boolean;
var
  Value: Integer;
begin
  case ADirection of
    dirLeft:
      Result := LeftPos <> 0;
    dirRight:
      begin
        Value := LeftPos + HScrollDelta;
        CheckLeftPos(Value);
        Result := LeftPos <> Value;
      end;
    dirUp:
      Result := InternalTopRecordIndex <> 0;
    dirDown:
      begin
        Value := InternalTopRecordIndex + 1;
        CheckTopRecordIndex(Value);
        Result := InternalTopRecordIndex <> Value;
      end;
  else
    Result := False;
  end;
end;

function TcxGridTableController.CanPostponeRecordSelection(AShift: TShiftState): Boolean;
begin
  Result := inherited CanPostponeRecordSelection(AShift) and not CellMultiSelect;
end;

function TcxGridTableController.CanProcessMultiSelect(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := inherited CanProcessMultiSelect(AHitTest, AShift) and
    not (IsClickableRecordHitTest(AHitTest) and
      TcxCustomGridRow(TcxGridRecordHitTest(AHitTest).GridRecord).IsFilterRow) and
    (not CellMultiSelect or (ssLeft in AShift) or
     not (AHitTest.ViewInfo is TcxGridTableDataCellViewInfo) or
     not TcxGridTableDataCellViewInfo(AHitTest.ViewInfo).Selected);
end;

procedure TcxGridTableController.DoMouseNormalSelection(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  if CellMultiSelect and (AHitTest is TcxGridRowIndicatorHitTest) then
    SelectAllColumns;
end;

procedure TcxGridTableController.DoMouseRangeSelection(AClearSelection: Boolean = True;
  AData: TObject = nil);
begin
  inherited;
  if CellMultiSelect then
    if AData is TcxGridRowIndicatorHitTest then
      SelectAllColumns
    else
      DoRangeCellSelection;
end;

procedure TcxGridTableController.DoNormalSelection;
begin
  BeginUpdate;
  try
    inherited DoNormalSelection;
    if CellMultiSelect then
    begin
      if (SelectedColumnCount = 1) and SelectedColumns[0].Focused then Exit;
      ClearCellSelection;
      if FocusedColumn <> nil then
      begin
        FocusedColumn.Selected := True;
        CellSelectionAnchor := FocusedColumn;
      end;
      GridView.NotifySelectionChanged;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridTableController.MultiSelectKeyDown(var Key: Word; Shift: TShiftState);
begin
  if CellMultiSelect then
    if ssShift in Shift then
      DoRangeSelection
    else
    begin
      SetSelectionAnchor(FocusedRecordIndex);
      DoNormalSelection;
    end
  else
    inherited MultiSelectKeyDown(Key, Shift);
end;

function TcxGridTableController.SupportsAdditiveSelection: Boolean;
begin
  Result := not CellMultiSelect;
end;

function TcxGridTableController.SupportsRecordSelectionToggling: Boolean;
begin
  Result := not CellMultiSelect;
end;

function TcxGridTableController.CanCloseInplaceEditForm: Boolean;
begin
  Result := InplaceEditForm.Visible and not EditingController.IsErrorOnPost;
end;

procedure TcxGridTableController.CheckFocusedItem(AItemViewInfo: TcxGridTableViewInplaceEditFormDataCellViewInfo);

  function GetNextFocusedItem(AGroupViewInfo: TdxLayoutGroupViewInfo; out AFocusedItem: TcxCustomGridTableItem): Boolean;
  var
    I: Integer;
    ALayoutItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
  begin
    Result := TdxCustomLayoutItemViewInfoAccess(AGroupViewInfo).IsExpanded and TdxCustomLayoutItemViewInfoAccess(AGroupViewInfo).GetEnabled;
    if Result then
    begin
      Result := False;
      for I := 0 to AGroupViewInfo.ItemViewInfoCount - 1 do
      begin
        if AGroupViewInfo.ItemViewInfos[I] is TdxLayoutGroupViewInfo then
          Result := GetNextFocusedItem(AGroupViewInfo.ItemViewInfos[I] as TdxLayoutGroupViewInfo, AFocusedItem)
        else
          if (AGroupViewInfo.ItemViewInfos[I] is TcxGridInplaceEditFormLayoutItemViewInfo) then
          begin
            ALayoutItemViewInfo := TcxGridInplaceEditFormLayoutItemViewInfo(AGroupViewInfo.ItemViewInfos[I]);
            if TcxGridTableViewInplaceEditFormDataCellViewInfo(ALayoutItemViewInfo.GridItemViewInfo).CanFocus then
            begin
              Result := True;
              AFocusedItem := TcxGridInplaceEditFormLayoutItemViewInfo(AGroupViewInfo.ItemViewInfos[I]).GridItemViewInfo.Item;
            end;
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
  AFocusedItem: TcxCustomGridTableItem;
  AParentViewInfo: TdxLayoutGroupViewInfo;
begin
  if (AItemViewInfo.Item = FocusedItem) and not AItemViewInfo.CanFocus then
  begin
    if (AItemViewInfo.LayoutItemViewInfo <> nil) and
        GetParentViewInfo(AItemViewInfo.LayoutItemViewInfo, AParentViewInfo) and GetNextFocusedItem(AParentViewInfo, AFocusedItem) then
      FocusedItem := AFocusedItem
    else
      if not FocusFirstAvailableItem then
        FocusedItem := nil;
  end;
end;

function TcxGridTableController.CloseInplaceEditFormOnRecordInserting: Boolean;
begin
  Result := not InplaceEditForm.Visible;
  if not Result and CanCloseInplaceEditForm then
    Result := InplaceEditForm.CloseOnRecordInserting;
end;

function TcxGridTableController.FocusNextInplaceEditFormItem(AGoForward: Boolean): Boolean;
var
  AIndex: Integer;
begin
  if FocusedItem = nil then
    AIndex := -1
  else
    AIndex := FocusedItem.Index;
  AIndex := GetNextInplaceEditFormItemIndex(AIndex, AGoForward);
  if AIndex <> -1 then
    GridView.Items[AIndex].Focused := True
  else
    FocusedItem := nil;
  Result := not (FocusedItemKind = fikNone);
end;

procedure TcxGridTableController.GetBackwardInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList);
begin
  if AIndex = -1 then
    case FFocusedItemKind of
      fikNone, fikGridItem:
         FocusedItemKind := fikCancelButton;
      fikCancelButton:
        begin
          FocusedItemKind := GetNextInplaceButton;
          if FocusedItemKind = fikCancelButton then
          begin
            AIndex := AItemList.Count - 1;
            FocusedItemKind := fikGridItem;
          end;
        end;
      fikUpdateButton:
        begin
          AIndex := AItemList.Count - 1;
          FocusedItemKind := fikGridItem;
        end;
    end
  else
    Dec(AIndex);
  ValidateInplaceEditFormItemIndex(AIndex, AItemList, False);
end;

procedure TcxGridTableController.GetForwardInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList);
begin
  if AIndex = -1 then
    case FFocusedItemKind of
      fikGridItem:
        FocusedItemKind := GetNextInplaceButton;
      fikUpdateButton:
        FocusedItemKind := fikCancelButton;
      fikNone, fikCancelButton:
        begin
          AIndex := 0;
          FocusedItemKind := fikGridItem;
        end;
    end
  else
    Inc(AIndex);
  ValidateInplaceEditFormItemIndex(AIndex, AItemList, True);
end;

function TcxGridTableController.GetNextInplaceEditFormItemIndex(AFocusedIndex: Integer;
  AGoForward: Boolean): Integer;

  procedure CheckPopulateGridItemsCanFocused(AList: TList);
  var
    I: Integer;
  begin
    for I := AList.Count - 1 downto 0 do
      if not TcxGridColumn(AList[I]).CanFocus(FocusedRow) then
        AList.Delete(I);
  end;

  procedure PopulateEditFormItems(AList: TList);
  begin
    if GridView.IsDestroying then
      Exit;
    InplaceEditForm.PopulateTabOrderList(AList);
    CheckPopulateGridItemsCanFocused(AList);
  end;

var
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateEditFormItems(AList);
    if AFocusedIndex <> -1 then
      Result := AList.IndexOf(GridView.Items[AFocusedIndex])
    else
      Result := AFocusedIndex;
    if AGoForward then
      GetForwardInplaceEditFormItemIndex(Result, AList)
    else
      GetBackwardInplaceEditFormItemIndex(Result, AList);
    if (Result <> -1) then
      Result := TcxGridColumn(AList[Result]).Index;
  finally
    AList.Free;
  end;
end;

function TcxGridTableController.GetNextInplaceButton: TcxGridFocusedItemKind;
begin
  if InplaceEditForm.IsUpdateButtonEnabled then
    Result := fikUpdateButton
  else
    Result := fikCancelButton;
end;

procedure TcxGridTableController.ValidateInplaceEditFormItemIndex(var AIndex: Integer; AItemList: TList; AGoForward: Boolean);
begin
  if AGoForward then
  begin
    if AIndex > AItemList.Count - 1 then
    begin
      AIndex := -1;
      FocusedItemKind := GetNextInplaceButton;
    end;
  end
  else
    if (AIndex < 0) and (FFocusedItemKind in [fikNone, fikGridItem]) then
    begin
      AIndex := -1;
      FocusedItemKind := fikCancelButton;
    end
end;

function TcxGridTableController.DefocusSpecialRow: Boolean;

  function FocusTopRow: Boolean;
  begin
    if ViewData.FixedTopRowCount > 0 then
    begin
      FocusedRowIndex := 0;
      Result := FocusedRowIndex = 0;
    end
    else
      if InternalTopRecordIndex > -1 then
      begin
        FocusedRowIndex := InternalTopRecordIndex;
        Result := FocusedRowIndex = InternalTopRecordIndex;
      end
      else
        Result := False;
  end;

begin
  Result := IsFilterRowFocused;
  if Result then
    if ViewData.HasNewItemRecord then
    begin
      ViewData.NewItemRow.Focused := True;
      Result := ViewData.NewItemRow.Focused;
    end
    else
    begin
      ViewData.FilterRow.Focused := False;
      Result := FocusTopRow;
      if not Result then
        ViewData.FilterRow.Focused := True;
    end
  else
    Result := IsNewItemRowFocused and FocusTopRow;
end;

function TcxGridTableController.FocusSpecialRow: Boolean;
begin
  Result := ViewData.HasNewItemRecord and not ViewData.NewItemRow.Focused;
  if Result then
  begin
    Result := not IsFilterRowFocused and IsStart;
    if Result then
      ViewData.NewItemRow.Focused := True;
  end
  else
  begin
    Result := ViewData.HasFilterRow and not ViewData.FilterRow.Focused;
    if Result then
    begin
      Result := ViewData.HasNewItemRecord or IsStart;
      if Result then
        ViewData.FilterRow.Focused := True;
    end;
  end;
end;

procedure TcxGridTableController.FilterRowFocusChanged;
begin
  inherited FocusedRecordChanged(FocusedRecordIndex, FocusedRecordIndex,
    DataController.FocusedRecordIndex, DataController.FocusedRecordIndex,
    NewItemRecordFocused);
  GridView.RefreshNavigators;
end;

procedure TcxGridTableController.FilterRowFocusChanging(AValue: Boolean);
var
  AFocusedRecordIndex: Integer;
begin
  if AValue then
  begin
    AFocusedRecordIndex := -1;
    CheckEditing(AFocusedRecordIndex, False);
  end
  else
    EditingController.HideEdit(not GridView.IsDestroying);
end;

procedure TcxGridTableController.DoPullFocusingScrolling(ADirection: TcxDirection);
begin
  if ADirection in [dirLeft, dirRight] then
    FocusNextCell(ADirection = dirRight, True, False);
  inherited;
end;

function TcxGridTableController.GetPullFocusingScrollingDirection(X, Y: Integer;
  out ADirection: TcxDirection): Boolean;
var
  R: TRect;
begin
  Result := inherited GetPullFocusingScrollingDirection(X, Y, ADirection);
  if not Result then
  begin
    R := ViewInfo.ScrollableAreaBoundsVert;
    if X < R.Left then
    begin
      ADirection := dirLeft;
      Result := True;
    end;
    if X >= R.Right then
    begin
      ADirection := dirRight;
      Result := True;
    end;
  end;
end;

function TcxGridTableController.SupportsPullFocusing: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited SupportsPullFocusing or CellMultiSelect;
end;

function TcxGridTableController.GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo;
begin
  Result := inherited GetDragOpenInfo(AHitTest);
  if (Result = nil) and (AHitTest.HitTestCode = htTab) then
    with TcxGridDetailsSiteTabHitTest(AHitTest) do
      Result := TcxGridDragOpenInfoMasterDataRowTab.Create(Level, Owner as TcxGridMasterDataRow);
end;

function TcxGridTableController.GetDragScrollDirection(X, Y: Integer): TcxDirection;
var
  R: TRect;
begin
  Result := dirNone;

  R := ViewInfo.ScrollableAreaBoundsVert;
  if PtInRect(R, Point(X, Y)) then
    if Y < R.Top + ScrollHotZoneWidth then
      Result := dirUp
    else
      if Y >= R.Bottom - ScrollHotZoneWidth then
        Result := dirDown;

  if Result = dirNone then
  begin
    R := ViewInfo.ScrollableAreaBoundsHorz;
    if PtInRect(R, Point(X, Y)) then
      if X < R.Left + ScrollHotZoneWidth then
        Result := dirLeft
      else
        if X >= R.Right - ScrollHotZoneWidth then
          Result := dirRight;
  end;
end;

procedure TcxGridTableController.CheckCustomizationFormBounds(var R: TRect);
var
  AHeaderBottomBound: Integer;
begin
  inherited;
  AHeaderBottomBound := Site.ClientToScreen(ViewInfo.HeaderViewInfo.Bounds.BottomRight).Y;
  if R.Top < AHeaderBottomBound then
    OffsetRect(R, 0, AHeaderBottomBound - R.Top);
end;

function TcxGridTableController.GetColumnHeaderDragAndDropObjectClass: TcxGridColumnHeaderMovingObjectClass;
begin
  Result := TcxGridColumnHeaderMovingObject;
end;

function TcxGridTableController.GetCustomizationFormClass: TcxCustomGridCustomizationFormClass;
begin
  Result := TcxGridTableCustomizationForm;
end;

function TcxGridTableController.CanNormalSelectionOnMouse: Boolean;
begin
  Result := inherited CanNormalSelectionOnMouse or GridView.OptionsSelection.ClearPersistentSelectionOnOutsideClick;
end;

function TcxGridTableController.CanProcessCellMultiSelect(APrevFocusedColumn: TcxGridColumn): Boolean;
begin
  Result := CellMultiSelect and (FocusedColumn <> APrevFocusedColumn);
end;

procedure TcxGridTableController.CellMultiSelectKeyDown(var Key: Word; Shift: TShiftState);
begin
  if ssShift in Shift then
    DoRangeCellSelection
  else
    DoNormalCellSelection;
end;

procedure TcxGridTableController.DoNormalCellSelection;
begin
  DoNormalSelection;
  SetSelectionAnchor(FocusedRowIndex);
  //GridView.DoSelectionChanged;
end;

procedure TcxGridTableController.DoRangeCellSelection;
begin
  SelectColumns(FCellSelectionAnchor, FocusedColumn);
end;

function TcxGridTableController.GetCellMultiSelect: Boolean;
begin
  Result := GridView.OptionsSelection.CellMultiSelect;
end;

procedure TcxGridTableController.AddBeginsWithMask(var AValue: Variant);
begin
  if VarIsStr(AValue) and (AValue <> '') and (GetBeginsWithMaskPos(AValue) = 0) then
    AValue := AValue + DataController.Filter.PercentWildcard;
end;

procedure TcxGridTableController.RemoveBeginsWithMask(var AValue: Variant);
var
  APos: Integer;
  S: string;
begin
  if VarIsStr(AValue) then
  begin
    APos := GetBeginsWithMaskPos(AValue);
    if APos <> 0 then
    begin
      S := AValue;
      Delete(S, APos, Length(DataController.Filter.PercentWildcard));
      AValue := S;
    end;
  end;
end;

function TcxGridTableController.GetBeginsWithMaskPos(const AValue: string): Integer;
begin
  if (Length(AValue) = 0) or (AValue[Length(AValue)] <> DataController.Filter.PercentWildcard) then
    Result := 0
  else
    Result := Length(AValue);
end;

function TcxGridTableController.GetEditingControllerClass: TcxGridEditingControllerClass;
begin
  Result := TcxGridTableEditingController;
end;

function TcxGridTableController.CanShowGroupFooter(ALevel: Integer): Boolean;
var
  I, AIndex: Integer;
begin
  Result := True;
  for I := 0 to DataController.Groups.GetLevelGroupedItemCount(ALevel) - 1 do
  begin
    AIndex := DataController.Groups.GetGroupingItemIndexByLevelGroupedItemIndex(ALevel, I);
    Result := Result and GridView.Columns[AIndex].CanShowGroupFooters;
  end;
end;

procedure TcxGridTableController.CreateNewRecord(AtEnd: Boolean);
begin
  if CloseInplaceEditFormOnRecordInserting then
    inherited CreateNewRecord(AtEnd);
end;

procedure TcxGridTableController.CheckScrolling(const P: TPoint);
var
  R: TRect;
  ADirection: TcxDirection;
begin
  R := ViewInfo.ScrollableAreaBoundsHorz;
  if PtInRect(R, P) then
    if P.X < R.Left + ScrollHotZoneWidth then
      ADirection := dirLeft
    else
      if R.Right - ScrollHotZoneWidth <= P.X then
        ADirection := dirRight
      else
        ADirection := dirNone
  else
    ADirection := dirNone;
  if UseRightToLeftAlignment then
    case ADirection of
      dirLeft:
        ADirection := dirRight;
      dirRight:
        ADirection := dirLeft;
    end;
  ScrollDirection := ADirection;
end;

procedure TcxGridTableController.ClearGrouping;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to GridView.GroupedColumnCount - 1 do
      GridView.GroupedColumns[I].Visible := True;
    DataController.Groups.ClearGrouping;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridTableController.ClearSelection;
begin
  ClearCellSelection;
  inherited;
end;

procedure TcxGridTableController.DoCancelMode;
begin
  inherited;
  PressedColumn := nil;
end;

function TcxGridTableController.FocusFirstAvailableItem: Boolean;
begin
  if InplaceEditForm.Visible then
    FocusedItemKind := fikNone;
  Result := inherited FocusFirstAvailableItem;
end;

function TcxGridTableController.FocusNextCell(AGoForward: Boolean; AProcessCellsOnly: Boolean = True;
  AAllowCellsCycle: Boolean = True; AFollowVisualOrder: Boolean = True; ANeedNormalizeSelection: Boolean = False): Boolean;
begin
  if InplaceEditForm.Visible then
    Result := FocusNextInplaceEditFormItem(AGoForward)
  else
    Result := inherited FocusNextCell(AGoForward, AProcessCellsOnly,
      AAllowCellsCycle, AFollowVisualOrder, ANeedNormalizeSelection);
end;

function TcxGridTableController.FocusNextItem(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle, AGoToNextRecordOnCycle, AFollowVisualOrder: Boolean; ANeedNormalizeSelection: Boolean = False): Boolean;
var
  AIndex: Integer;
begin
  if not GridView.IsDestroying and InplaceEditForm.Visible then
  begin
    AIndex := GetNextInplaceEditFormItemIndex(AFocusedItemIndex, AGoForward);
    if AIndex <> -1 then
      GridView.Items[AIndex].Focused := True
    else
      FocusedItem := nil;
    Result := not (FocusedItemKind = fikNone);
  end
  else
    Result := inherited FocusNextItem(AFocusedItemIndex, AGoForward,
      AGoOnCycle, AGoToNextRecordOnCycle, AFollowVisualOrder, ANeedNormalizeSelection);
end;

function TcxGridTableController.IsFilterRowFocused: Boolean;
begin
  Result := ViewData.HasFilterRow and ViewData.FilterRow.Focused;
end;

function TcxGridTableController.IsFilterRowOperatorSupported(AItemIndex: Integer; AOperator: TcxFilterOperatorKind): Boolean;
begin
  Result := (AOperator in GetFilterRowSupportedOperators) and GridView.Columns[AItemIndex].IsFilterOperatorSupported(AOperator);
end;

function TcxGridTableController.IsNewItemRowFocused: Boolean;
begin
  Result := ViewData.HasNewItemRecord and ViewData.NewItemRow.Focused;
end;

function TcxGridTableController.IsSpecialRowFocused: Boolean;
begin
  Result := IsFilterRowFocused or IsNewItemRowFocused;
end;

procedure TcxGridTableController.MakeItemVisible(AItem: TcxCustomGridTableItem);

  function GetColumnBounds: TRect;
  begin
    Result := ViewInfo.HeaderViewInfo[AItem.VisibleIndex].Bounds;
  end;

  procedure ScrollLeft(AColumnBounds, AScrollableAreaBounds: TRect);
  begin
    if UseRightToLeftAlignment then
      LeftPos := LeftPos - (AColumnBounds.Right - AScrollableAreaBounds.Right)
    else
      LeftPos := LeftPos + (AColumnBounds.Right - AScrollableAreaBounds.Right);
  end;

  procedure ScrollRight(AColumnBounds, AScrollableAreaBounds: TRect);
  begin
    if UseRightToLeftAlignment then
      LeftPos := LeftPos + (AScrollableAreaBounds.Left - AColumnBounds.Left)
    else
      LeftPos := LeftPos - (AScrollableAreaBounds.Left - AColumnBounds.Left);
  end;

var
  AColumnBounds, AScrollableAreaBounds: TRect;
  ADataRowViewInfo: TcxGridDataRowViewInfo;
begin
  if not CanMakeItemVisible(AItem) then
    Exit;
  MakeFocusedRecordVisible;
  if InplaceEditForm.Visible then
  begin
    ADataRowViewInfo := TcxGridDataRowViewInfo(FocusedRecord.ViewInfo);
    if (ADataRowViewInfo <> nil) and ADataRowViewInfo.HasInplaceEditFormArea then
      ADataRowViewInfo.InplaceEditFormAreaViewInfo.MakeItemVisible(AItem)
  end
  else
    if TcxGridColumn(AItem).CanScroll then
    begin
      AColumnBounds := GetColumnBounds;
      AScrollableAreaBounds := ViewInfo.ScrollableAreaBoundsHorz;
      if AColumnBounds.Width >= AScrollableAreaBounds.Width then
        if UseRightToLeftAlignment then
          ScrollLeft(AColumnBounds, AScrollableAreaBounds)
        else
          ScrollRight(AColumnBounds, AScrollableAreaBounds)
      else
      begin
        if AColumnBounds.Right > AScrollableAreaBounds.Right then
        begin
          ScrollLeft(AColumnBounds, AScrollableAreaBounds);
          AColumnBounds := GetColumnBounds;
        end;
        if AColumnBounds.Left < AScrollableAreaBounds.Left then
          ScrollRight(AColumnBounds, AScrollableAreaBounds);
      end;
    end;
end;

procedure TcxGridTableController.SelectAll;
begin
  BeginUpdate;
  try
    inherited;
    if CellMultiSelect then
      SelectAllColumns;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridTableController.ShowEditFormCustomizationDialog;
begin
  InplaceEditForm.ShowCustomizationForm;
end;

procedure TcxGridTableController.InitScrollBarsParameters;
var
  APos: Integer;
  AScrollContentWidth, AScrollContentHeight: Integer;
begin
  if ViewInfo.ScrollableAreaWidth > 0 then
    APos := LeftPos
  else
    APos := -1;
  AScrollContentWidth := ViewInfo.GetScrollContentWidth;
  Controller.SetScrollBarInfo(sbHorizontal, 0, AScrollContentWidth - 1, HScrollDelta, ViewInfo.ClientWidth, APos, True, CanHScrollBarHide);
  AScrollContentHeight := DataScrollSize;
  Controller.SetScrollBarInfo(sbVertical, 0, AScrollContentHeight - 1, 1, VisibleDataScrollSize, ScrollBarPos, True, True);
end;

function TcxGridTableController.IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean;
begin
  Result := inherited IsDataFullyVisible(AIsCallFromMaster) and
    ViewInfo.RecordsViewInfo.IsFirstRowFullyVisible;
  if Result and GridView.IsMaster then
    Result := not NeedsAdditionalRowsScrolling(AIsCallFromMaster);
end;

procedure TcxGridTableController.EndDragAndDrop(Accepted: Boolean);
begin
  PressedColumn := nil;
  inherited;
end;

procedure TcxGridTableController.DoKeyDown(var Key: Word; Shift: TShiftState);
var
  AFocusedColumn: TcxGridColumn;
begin
  AFocusedColumn := FocusedColumn;
  inherited;
  if (Key <> 0) and CanProcessCellMultiSelect(AFocusedColumn) then
    CellMultiSelectKeyDown(Key, Shift);
end;

procedure TcxGridTableController.KeyDown(var Key: Word; Shift: TShiftState);
var
  AGridViewLink: TcxObjectLink;
  AFocusedRowIndex: Integer;
  AGoForward: Boolean;
begin
  case Key of
    VK_LEFT, VK_RIGHT:
      begin
        AGoForward := UseRightToLeftAlignment xor (Key = VK_RIGHT);
        if FocusNextCell(AGoForward) then Exit;//Key := 0;
      end;
    VK_UP, VK_DOWN:
      begin
        if GridView.IsInplaceEditFormMode and InplaceEditForm.Visible then
        begin
          FocusNextInplaceEditFormItem(Key = VK_DOWN);
          Exit;
        end
      end;
    VK_PRIOR:
      if FocusSpecialRow then
        Exit
      else
        if IsSpecialRowFocused then
        begin
          Key := 0;
          Exit;
        end;
    VK_NEXT:
      if DefocusSpecialRow then Exit;
  end;
  inherited;
  AGridViewLink := cxAddObjectLink(GridView);
  try
    case Key of
      VK_LEFT:
          if UseRightToLeftAlignment then
            ScrollData(dirRight)
          else
            ScrollData(dirLeft);
      VK_RIGHT:
          if UseRightToLeftAlignment then
            ScrollData(dirLeft)
          else
            ScrollData(dirRight);
      VK_UP:
          if not FocusSpecialRow then
          begin
            if IsSpecialRowFocused then
              AFocusedRowIndex := -1
            else
              AFocusedRowIndex := FocusedRowIndex;
            NeedUpdateSiteOnFocusedRecordChanged := True;
            try
              if not FocusNextRecord(AFocusedRowIndex, False, False, not (ssShift in Shift),
                not (ssShift in Shift)) and IsSpecialRowFocused then
                Key := 0;
            finally
              NeedUpdateSiteOnFocusedRecordChanged := False;
            end;
          end;
      VK_DOWN:
          if not DefocusSpecialRow then
          begin
            NeedUpdateSiteOnFocusedRecordChanged := True;
            try
              FocusNextRecord(FocusedRowIndex, True, False, not (ssShift in Shift), not (ssShift in Shift));
            finally
              NeedUpdateSiteOnFocusedRecordChanged := False;
            end;
          end;
      VK_HOME:
        if (ssCtrl in Shift) or not FocusedRecordHasCells(True) then
          GoToFirst(False)
        else
          FocusNextItem(-1, True, False, False, not IsMultiSelectPersistent);
      VK_END:
        if (ssCtrl in Shift) or not FocusedRecordHasCells(True) then
          GoToLast(False, False)
        else
          FocusNextItem(-1, False, True, False, not IsMultiSelectPersistent);
    end;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
end;

procedure TcxGridTableController.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  procedure ProcessSorting(AColumn: TcxGridColumn);

    procedure RemoveColumnSorting;
    var
      I: Integer;
    begin
      if GridView.OptionsCustomize.GroupBySorting then
        with AColumn do
          if (GroupIndex = 0) and CanGroup then
          begin
            GroupIndex := -1;
            Visible := True;
          end;
      AColumn.SortOrder := soNone;
      if GridView.OptionsCustomize.GroupBySorting then
        for I := 0 to GridView.SortedItemCount - 1 do
          with TcxGridColumn(GridView.SortedItems[I]) do
            if CanGroup then
            begin
              GroupIndex := 0;
              Break;
            end;
    end;

    procedure AddColumnSorting;
    var
      ASortOrder: TcxGridSortOrder;
    begin
      if AColumn.SortOrder = soAscending then
        ASortOrder := soDescending
      else
        ASortOrder := soAscending;
      if not (ssShift in Shift) and (AColumn.GroupIndex = -1) then
      begin
        if GridView.OptionsCustomize.GroupBySorting and AColumn.CanGroup then
          ClearGrouping;
        DataController.ClearSorting(True);
      end;
      AColumn.SortOrder := ASortOrder;
      if GridView.OptionsCustomize.GroupBySorting and (AColumn.SortIndex = 0) and
        (GridView.GroupedColumnCount = 0) and AColumn.CanGroup then
        AColumn.GroupIndex := 0;
    end;

  begin
    if not AColumn.CanSort then Exit;
    try
      GridView.BeginSortingUpdate;
      try
        if ssCtrl in Shift then
          RemoveColumnSorting
        else
          AddColumnSorting;
      finally
        GridView.EndSortingUpdate;
      end;
    finally
      MakeFocusedRecordVisible;
      DesignerModified;
    end;
  end;

begin
  try
    inherited;
    if Site.IsMouseInPressedArea(X, Y) and (PressedColumn <> nil) then
    begin
      PressedColumn.DoHeaderClick;
      ProcessSorting(PressedColumn);
    end;
  finally
    PressedColumn := nil;
  end;
end;

procedure TcxGridTableController.ClearCellSelection;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := SelectedColumnCount - 1 downto 0 do
      SelectedColumns[I].Selected := False;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridTableController.SelectAllColumns;
begin
  SelectColumns(nil, nil);
end;

procedure TcxGridTableController.SelectCells(AFromColumn, AToColumn: TcxGridColumn;
  AFromRowIndex, AToRowIndex: Integer);
begin
  BeginUpdate;
  try
    if AFromRowIndex = -1 then AFromRowIndex := 0;
    if AToRowIndex = -1 then AToRowIndex := DataController.RowCount - 1;
    DataController.ClearSelection;
    DataController.SelectRows(AFromRowIndex, AToRowIndex);

    SelectColumns(AFromColumn, AToColumn);
  finally
    EndUpdate;
  end;
end;

procedure TcxGridTableController.SelectColumns(AFromColumn, AToColumn: TcxGridColumn);

  procedure GetNewSelectedColumns(AColumns: TList);
  var
    AStartIndex, AFinishIndex, I: Integer;
  begin
    AStartIndex := AColumns.IndexOf(AFromColumn);
    if AStartIndex = -1 then AStartIndex := 0;
    AFinishIndex := AColumns.IndexOf(AToColumn);
    if AFinishIndex = -1 then
      AFinishIndex := AColumns.Count - 1;
    if (AStartIndex > AFinishIndex) and (AFinishIndex <> -1) then
    begin
      I := AStartIndex;
      AStartIndex := AFinishIndex;
      AFinishIndex := I;
    end;

    AColumns.Count := AFinishIndex + 1;
    for I := 0 to AStartIndex - 1 do
      AColumns.Delete(0);
  end;

  function IsSelectionChanged(ANewSelectedColumns: TList): Boolean;
  var
    I: Integer;
  begin
    Result := ANewSelectedColumns.Count <> SelectedColumnCount;
    if not Result then
      for I := 0 to ANewSelectedColumns.Count - 1 do
      begin
        Result := not TcxGridColumn(ANewSelectedColumns[I]).Selected;
        if Result then Break;
      end;
  end;

var
  ASelectionChanged: Boolean;
  AColumns: TList;
  I: Integer;
begin
  ASelectionChanged := False;
  BeginUpdate;
  AColumns := TList.Create;
  try
    GridView.GetVisibleItemsList(AColumns);
    GetNewSelectedColumns(AColumns);
    ASelectionChanged := IsSelectionChanged(AColumns);

    if ASelectionChanged then
    begin
      ClearCellSelection;
      for I := 0 to AColumns.Count - 1 do
        TcxGridColumn(AColumns[I]).Selected := True;
    end;
  finally
    AColumns.Free;
    EndUpdate;
    if ASelectionChanged then
      GridView.DoSelectionChanged;
  end;
end;

{ TcxGridColumnContainerPainter }

function TcxGridColumnContainerPainter.GetViewInfo: TcxGridColumnContainerViewInfo;
begin
  Result := TcxGridColumnContainerViewInfo(inherited ViewInfo);
end;

procedure TcxGridColumnContainerPainter.DrawContent;
var
  AClipRegion: TcxRegion;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    Canvas.IntersectClipRect(ViewInfo.Bounds);
    if DrawItemsFirst then
    begin
      DrawItems;
      inherited;
    end
    else
    begin
      inherited;
      DrawItems;
    end
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
end;

procedure TcxGridColumnContainerPainter.DrawItems;
var
  I: Integer;
  AViewInfo: TcxGridColumnHeaderViewInfo;
begin
  for I := 0 to ViewInfo.Count - 1 do
  begin
    AViewInfo := ViewInfo.InternalItems[I];
    if AViewInfo <> nil then AViewInfo.Paint;
  end;
end;

function TcxGridColumnContainerPainter.DrawItemsFirst: Boolean;
begin
  Result := True;
end;

function TcxGridColumnContainerPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridColumnHeaderAreaPainter }

function TcxGridColumnHeaderAreaPainter.GetViewInfo: TcxGridColumnHeaderAreaViewInfo;
begin
  Result := TcxGridColumnHeaderAreaViewInfo(inherited ViewInfo);
end;

function TcxGridColumnHeaderAreaPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridColumnHeaderSortingMarkPainter }

procedure TcxGridColumnHeaderSortingMarkPainter.Paint;
begin
  with TcxGridColumnHeaderSortingMarkViewInfo(ViewInfo) do
    if ColumnHeaderViewInfo.SortByGroupSummary then
      LookAndFeelPainter.DrawScaledSummarySortingMark(Self.Canvas, Bounds, SortOrder = soAscending, ScaleFactor)
    else
      LookAndFeelPainter.DrawScaledSortingMark(Self.Canvas, Bounds, SortOrder = soAscending, ScaleFactor);
end;

{ TcxGridColumnHeaderFilterButtonPainter }

function TcxGridColumnHeaderFilterButtonPainter.GetSmartTagState: TcxFilterSmartTagState;
begin
  with ViewInfo do
    case ButtonState of
      cxbsHot:
        Result := fstsHot;
      cxbsPressed:
        Result := fstsPressed;
      else
        if ColumnHeaderViewInfo.State = gcsSelected then
          Result := fstsParentHot
        else
          Result := fstsNormal
    end;
end;

function TcxGridColumnHeaderFilterButtonPainter.GetViewInfo: TcxGridColumnHeaderFilterButtonViewInfo;
begin
  Result := TcxGridColumnHeaderFilterButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridColumnHeaderFilterButtonPainter.Paint;
begin
  with ViewInfo do
    if IsSmartTag then
      LookAndFeelPainter.DrawScaledFilterSmartTag(Self.Canvas, Bounds, SmartTagState, Active, ScaleFactor)
    else
      LookAndFeelPainter.DrawScaledFilterDropDownButton(Self.Canvas, Bounds, ButtonState, Active, ScaleFactor);
end;

{ TcxGridColumnHeaderGlyphPainter }

function TcxGridColumnHeaderGlyphPainter.GetViewInfo: TcxGridColumnHeaderGlyphViewInfo;
begin
  Result := TcxGridColumnHeaderGlyphViewInfo(inherited ViewInfo);
end;

procedure TcxGridColumnHeaderGlyphPainter.Paint;
begin
  with ViewInfo do
    cxDrawImage(Self.Canvas.Handle, Bounds, Bounds, Glyph, Images, ImageIndex, idmNormal);
end;

{ TcxGridColumnHeaderCheckBoxPainter }

function TcxGridColumnHeaderCheckBoxPainter.GetViewInfo: TcxGridColumnHeaderCheckBoxAreaViewInfo;
begin
  Result := TcxGridColumnHeaderCheckBoxAreaViewInfo(inherited ViewInfo);
end;

procedure TcxGridColumnHeaderCheckBoxPainter.Paint;
begin
  ViewInfo.CheckBox.Paint;
end;

{ TcxGridColumnHeaderPainter }

function TcxGridColumnHeaderPainter.GetViewInfo: TcxGridColumnHeaderViewInfo;
begin
  Result := TcxGridColumnHeaderViewInfo(inherited ViewInfo);
end;

procedure TcxGridColumnHeaderPainter.BeforePaint;
begin
  inherited BeforePaint;
  ViewInfo.DrawPressed := ViewInfo.IsPressed;
end;

procedure TcxGridColumnHeaderPainter.DrawAreas;
var
  AClipRegion: TcxRegion;
  I: Integer;
begin
  AClipRegion := Canvas.GetClipRegion;
  try
    for I := 0 to ViewInfo.AreaViewInfoCount - 1 do
      ViewInfo.AreaViewInfos[I].Paint(Canvas);
  finally
    Canvas.SetClipRegion(AClipRegion, roSet);
  end;
end;

procedure TcxGridColumnHeaderPainter.DrawBorders;
begin
  // inherited;
end;

procedure TcxGridColumnHeaderPainter.DrawContent;
var
  AState: TcxButtonState;
begin
  with ViewInfo do
  begin
    if IsMainCanvasInUse then
      AState := ButtonState
    else
      AState := cxbsNormal;
    LookAndFeelPainter.DrawScaledHeader(Self.Canvas, Bounds, TextAreaBounds, Neighbors,
      Borders, AState, AlignmentHorz, AlignmentVert, MultiLinePainting, ShowEndEllipsis,
      Text, Params.Font, Params.TextColor, Params.Color, ScaleFactor,
      ViewInfo.GridViewInfo.HeaderViewInfo.DrawColumnBackgroundHandler, Column.IsMostRight,
      ViewInfo.Container.Kind = ckGroupByBox);
  end;
  DrawAreas;
end;

procedure TcxGridColumnHeaderPainter.DrawPressed;
begin
  with ViewInfo do
    LookAndFeelPainter.DrawHeaderPressed(Self.Canvas, Bounds);
end;

function TcxGridColumnHeaderPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxGridColumnHeaderPainter.Paint;
begin
  inherited Paint;
  if ViewInfo.DrawPressed and IsMainCanvasInUse then
    DrawPressed;
end;

{ TcxGridHeaderPainter }

function TcxGridHeaderPainter.DrawItemsFirst: Boolean;
begin
  Result := ViewInfo.LookAndFeelPainter.GridDrawHeaderCellsFirst;
end;

{ TcxGridGroupByBoxPainter }

procedure TcxGridGroupByBoxPainter.DrawBackground(const R: TRect);
begin
  with ViewInfo do
    LookAndFeelPainter.DrawGroupByBox(Canvas, R, Transparent, Params.Color,
      BackgroundBitmap);
end;

procedure TcxGridGroupByBoxPainter.DrawContent;
begin
  inherited DrawContent;
  DrawConnectors;
end;

function TcxGridGroupByBoxPainter.DrawItemsFirst: Boolean;
begin
  Result := ViewInfo.LookAndFeelPainter.HeaderDrawCellsFirst;
end;

procedure TcxGridGroupByBoxPainter.DrawConnectors;
var
  I: Integer;
  R: TRect;
  J: Boolean;
begin
  Canvas.Brush.Color := GroupByBoxLineColor;
  if ViewInfo.IsSingleLine then
    for I := 0 to ViewInfo.Count - 2 do
    begin
      R := ViewInfo.LinkLineBounds[I, True];
      Canvas.FillRect(R);
    end
  else
    for I := 0 to ViewInfo.Count - 2 do
      for J := Low(J) to High(J) do
      begin
        R := ViewInfo.LinkLineBounds[I, J];
        Canvas.FillRect(R);
      end;
end;

function TcxGridGroupByBoxPainter.GetViewInfo: TcxGridGroupByBoxViewInfo;
begin
  Result := TcxGridGroupByBoxViewInfo(inherited ViewInfo);
end;

{ TcxGridFooterCellPainter }

procedure TcxGridFooterCellPainter.DrawBorders;
begin
  // inherited;
end;

procedure TcxGridFooterCellPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawFooterCell(Canvas, ViewInfo.Bounds, ViewInfo.AlignmentHorz,
    ViewInfo.AlignmentVert, ViewInfo.MultiLinePainting, ViewInfo.Text, ViewInfo.Params.Font, ViewInfo.Params.TextColor,
    ViewInfo.Params.Color, DrawBackgroundHandler);
end;

{ TcxGridFooterPainter }

function TcxGridFooterPainter.GetViewInfo: TcxGridFooterViewInfo;
begin
  Result := TcxGridFooterViewInfo(inherited ViewInfo);
end;

procedure TcxGridFooterPainter.DrawBackground(const R: TRect);
begin
  ViewInfo.LookAndFeelPainter.DrawFooterContent(Canvas, R, ViewInfo.Params);
end;

procedure TcxGridFooterPainter.DrawBorders;
begin
  if ViewInfo.HasSeparator then DrawSeparator;
  ViewInfo.LookAndFeelPainter.DrawFooterBorderEx(Canvas, ViewInfo.BordersBounds, ViewInfo.Borders);
end;

function TcxGridFooterPainter.DrawItemsFirst: Boolean;
begin
  Result := ViewInfo.LookAndFeelPainter.FooterDrawCellsFirst;
end;

procedure TcxGridFooterPainter.DrawSeparator;
begin
  ViewInfo.LookAndFeelPainter.DrawFooterSeparator(Canvas, ViewInfo.SeparatorBounds);
end;

{ TcxCustomGridIndicatorItemPainter }

function TcxCustomGridIndicatorItemPainter.GetViewInfo: TcxCustomGridIndicatorItemViewInfo;
begin
  Result := TcxCustomGridIndicatorItemViewInfo(inherited ViewInfo);
end;

function TcxCustomGridIndicatorItemPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridIndicatorItemPainter.Paint;
begin
  inherited Paint;
  if ViewInfo.HasCheckBox then
    ViewInfo.CheckBox.Paint;
end;

{ TcxGridIndicatorHeaderItemPainter }

function TcxGridIndicatorHeaderItemPainter.GetViewInfo: TcxGridIndicatorHeaderItemViewInfo;
begin
  Result := TcxGridIndicatorHeaderItemViewInfo(inherited ViewInfo);
end;

function TcxGridIndicatorHeaderItemPainter.DrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := ViewInfo.GridViewInfo.HeaderViewInfo.DrawColumnBackgroundHandler(ACanvas, ABounds);
end;

procedure TcxGridIndicatorHeaderItemPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledHeader(Canvas, ViewInfo.Bounds, ViewInfo.Bounds, [nRight], cxBordersAll,
    GridCellStateToButtonState(ViewInfo.State), taLeftJustify, vaTop, False,
    False, '', nil, clNone, ViewInfo.Params.Color, ViewInfo.ScaleFactor, DrawBackgroundHandler);
  if ViewInfo.SupportsQuickCustomization then
    DrawQuickCustomizationMark;
  if ViewInfo.State = gcsPressed then
    ViewInfo.LookAndFeelPainter.DrawHeaderPressed(Canvas, ViewInfo.Bounds);
end;

procedure TcxGridIndicatorHeaderItemPainter.DrawQuickCustomizationMark;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledIndicatorCustomizationMark(Canvas, ViewInfo.GetImageAreaBounds,
    ViewInfo.Params.TextColor, ScaleFactor);
end;

{ TcxGridIndicatorRowItemPainter }

function TcxGridIndicatorRowItemPainter.GetViewInfo: TcxGridIndicatorRowItemViewInfo;
begin
  Result := TcxGridIndicatorRowItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridIndicatorRowItemPainter.DrawContent;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledIndicatorItem(Canvas, ViewInfo.Bounds, ViewInfo.GetImageAreaBounds,
    ViewInfo.IndicatorKind, ViewInfo.Params.Color, ScaleFactor, DrawBackgroundHandler, ViewInfo.GetNeighbors);
end;

{ TcxGridIndicatorFooterItemPainter }

function TcxGridIndicatorFooterItemPainter.GetViewInfo: TcxGridIndicatorFooterItemViewInfo;
begin
  Result := TcxGridIndicatorFooterItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridIndicatorFooterItemPainter.DrawBorders;
begin
  if ViewInfo.HasSeparator then
    ViewInfo.LookAndFeelPainter.DrawFooterSeparator(Canvas, ViewInfo.SeparatorBounds);
  ViewInfo.LookAndFeelPainter.DrawFooterBorderEx(Canvas, ViewInfo.BordersBounds, ViewInfo.Borders);
end;

procedure TcxGridIndicatorFooterItemPainter.DrawContent;
begin
  if ViewInfo.GridView.LookAndFeel.SkinPainter = nil then
    inherited DrawContent
  else
    with ViewInfo do
      LookAndFeelPainter.DrawScaledHeader(Canvas, Bounds, Bounds, [], [], cxbsNormal, taLeftJustify,
        vaTop, False, False, '', nil, clNone, Params.Color, ScaleFactor, DrawBackgroundHandler);
end;

{ TcxGridIndicatorPainter }

function TcxGridIndicatorPainter.GetViewInfo: TcxGridIndicatorViewInfo;
begin
  Result := TcxGridIndicatorViewInfo(inherited ViewInfo);
end;

procedure TcxGridIndicatorPainter.DrawContent;
begin
  if DrawItemsFirst then
  begin
    DrawItems;
    inherited;
  end
  else
  begin
    inherited;
    DrawItems;
  end;
end;

procedure TcxGridIndicatorPainter.DrawItems;
var
  I: Integer;
begin
  with ViewInfo do
    for I := 0 to Count - 1 do
      Items[I].Paint;
end;

function TcxGridIndicatorPainter.DrawItemsFirst: Boolean;
begin
  Result := ViewInfo.LookAndFeelPainter.IndicatorDrawItemsFirst;
end;

function TcxGridIndicatorPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxCustomGridRowPainter }

function TcxCustomGridRowPainter.GetViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := TcxCustomGridRowViewInfo(inherited ViewInfo);
end;

procedure TcxCustomGridRowPainter.DrawFooters;
begin
  ViewInfo.FootersViewInfo.Paint;
end;

procedure TcxCustomGridRowPainter.DrawIndent;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.VisualLevel - 1 do
    DrawIndentPart(I, ViewInfo.LevelIndentBounds[I]);
end;

procedure TcxCustomGridRowPainter.DrawIndentPart(ALevel: Integer; const ABounds: TRect);
begin
  with Canvas, ViewInfo do
  begin
    if GridViewInfo.LevelIndentBackgroundBitmap = nil then
    begin
      Brush.Color := GridViewInfo.LevelIndentColors[ALevel];
      FillRect(CalculateLevelIndentSpaceBounds(ALevel, ABounds));
    end
    else
      FillRect(CalculateLevelIndentSpaceBounds(ALevel, ABounds),
        GridViewInfo.LevelIndentBackgroundBitmap);

    Brush.Color := GridViewInfo.LevelSeparatorColor;
    FillRect(CalculateLevelIndentVertLineBounds(ALevel, ABounds));

    Brush.Color := GridViewInfo.GridLineColor;
    FillRect(CalculateLevelIndentHorzLineBounds(ALevel, ABounds));
  end;
end;

procedure TcxCustomGridRowPainter.DrawLastHorzGridLine;
begin
  with Canvas do
  begin
    Brush.Color := ViewInfo.GridViewInfo.GridLineColor;
    FillRect(ViewInfo.LastHorzGridLineBounds);
  end;
end;

procedure TcxCustomGridRowPainter.DrawSeparator;
begin
  Canvas.Brush.Color := ViewInfo.SeparatorColor;
  Canvas.FillRect(ViewInfo.SeparatorBounds);
end;

procedure TcxCustomGridRowPainter.Paint;
begin
  if ViewInfo.HasFooters then DrawFooters;
  DrawIndent;
  if ViewInfo.HasLastHorzGridLine then DrawLastHorzGridLine;
  if ViewInfo.HasSeparator then DrawSeparator;
  inherited Paint;
end;

{ TcxGridFixedRowsPainter }

constructor TcxGridFixedDataRowsPainter.Create(ACanvas: TcxCanvas; AViewInfo: TcxGridFixedDataRowsViewInfo);
begin
  inherited Create;
  FCanvas := ACanvas;
  FViewInfo := AViewInfo;
end;

procedure TcxGridFixedDataRowsPainter.MainPaint;
begin
  Paint;
end;

procedure TcxGridFixedDataRowsPainter.DrawBottomItems;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.BottomItemCount - 1 do
    if ViewInfo.BottomItems[I].Calculated then
      ViewInfo.BottomItems[I].Paint;
end;

procedure TcxGridFixedDataRowsPainter.DrawTopItems;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.TopItemCount - 1 do
    if ViewInfo.TopItems[I].Calculated then
      ViewInfo.TopItems[I].Paint;
end;

procedure TcxGridFixedDataRowsPainter.Paint;
begin
  DrawBottomItems;
  DrawTopItems;
end;

{ TcxGridRowsPainter }

function TcxGridRowsPainter.GetViewInfo: TcxGridRowsViewInfo;
begin
  Result := TcxGridRowsViewInfo(inherited ViewInfo);
end;

procedure TcxGridRowsPainter.Paint;
begin
  if ViewInfo.HasFilterRow then
    ViewInfo.FilterRowViewInfo.Paint;
  if ViewInfo.HasNewItemRow then
    ViewInfo.NewItemRowViewInfo.Paint;
  if ViewInfo.HasFixedDataRows then
    ViewInfo.FixedDataRowsViewInfo.Paint;
  inherited Paint;
end;

class procedure TcxGridRowsPainter.DrawDataRowCells(ARowViewInfo: TcxCustomGridRowViewInfo);
var
  I: Integer;
  ACellViewInfo: TcxGridDataCellViewInfo;
begin
  with ARowViewInfo as TcxGridDataRowViewInfo do
  begin
    for I := 0 to CellViewInfoCount - 1 do
    begin
      ACellViewInfo := InternalCellViewInfos[I];
      if ACellViewInfo <> nil then ACellViewInfo.Paint;
    end;
    CellsAreaViewInfo.Paint;
  end;
end;

{ TcxGridTablePainter }

constructor TcxGridTablePainter.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FGridLines := TList.Create;
end;

destructor TcxGridTablePainter.Destroy;
begin
  FreeAndNil(FGridLines);
  inherited Destroy;
end;

procedure TcxGridTablePainter.AddGridLine(const R: TRect);
var
  AR: PRect;
begin
  New(AR);
  AR^ := R;
  FGridLines.Add(AR);
end;

function TcxGridTablePainter.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridTablePainter.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTablePainter.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

function TcxGridTablePainter.CanOffset(AItemsOffset, DX, DY: Integer): Boolean;
begin
  Result := inherited CanOffset(AItemsOffset, DX, DY) and
    ((AItemsOffset <> 0) or not GridView.IsMaster) and
    (ViewInfo.RecordsViewInfo.GroupBackgroundBitmap = nil) and
    ((AItemsOffset <> 0) or (ViewInfo.HeaderViewInfo.ColumnBackgroundBitmap = nil)) and
    (ViewInfo.FooterViewInfo.BackgroundBitmap = nil) and
    ((AItemsOffset <> 0) or not GridView.FilterRow.Visible or
     (GridView.FilterRow.InfoText = '')) and
    ((AItemsOffset <> 0) or not GridView.NewItemRow.Visible or
     (GridView.NewItemRow.InfoText = '')) and
    ((AItemsOffset = 0) or not GridView.HasCellMerging) and
    not GridView.IsInplaceEditFormMode and
    not GridView.IsFixedGroupsMode;
end;

procedure TcxGridTablePainter.ClearGridLines;
var
  I: Integer;
begin
  for I := 0 to FGridLines.Count - 1 do
    Dispose(PRect(FGridLines[I]));
  FGridLines.Clear;
end;

procedure TcxGridTablePainter.DrawFooter;
begin
  ViewInfo.FooterViewInfo.Paint;
end;

procedure TcxGridTablePainter.DrawGridLines;
var
  I: Integer;
  N: array of DWORD;
  P: array of TPoint;
  R: TRect;
begin
  SetLength(P, FGridLines.Count * 2);
  SetLength(N, FGridLines.Count);
  for I := 0 to FGridLines.Count - 1 do
  begin
    R := PRect(FGridLines[I])^;
    P[2 * I] := R.TopLeft;
    if R.Bottom = R.Top + 1 then
      P[2 * I + 1] := Point(R.Right, R.Top)
    else
      P[2 * I + 1] := Point(R.Left, R.Bottom);
    N[I] := 2;
  end;
  with Canvas do
  begin
    Pen.Color := ViewInfo.GridLineColor;
    PolyPolyLine(Handle, P[0], N[0], FGridLines.Count);
  end;
  N := nil;
  P := nil;
end;

procedure TcxGridTablePainter.DrawGroupByBox;
begin
  ViewInfo.GroupByBoxViewInfo.Paint;
end;

procedure TcxGridTablePainter.DrawHeader;
begin
  ViewInfo.HeaderViewInfo.Paint;
end;

procedure TcxGridTablePainter.DrawIndicator;
begin
  ViewInfo.IndicatorViewInfo.Paint;
end;

procedure TcxGridTablePainter.DrawRecords;
begin
  inherited DrawRecords;
  DrawGridLines;
  ClearGridLines;
end;

procedure TcxGridTablePainter.Offset(AItemsOffset: Integer);
var
  R, AUpdateBounds: TRect;
begin
  R := ViewInfo.GetOffsetBounds(AItemsOffset, AUpdateBounds);
  if not IsRectEmpty(R) then
    Site.ScrollWindow(0, AItemsOffset, R);
  if not IsRectEmpty(AUpdateBounds) then
    Site.InvalidateRect(AUpdateBounds, True);
  Controller.InvalidateFocusedRecord;
  if Controller.IsEditing then
    cxRedrawWindow(Controller.EditingController.Edit.Handle, RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TcxGridTablePainter.Offset(DX, DY: Integer);
var
  R, AUpdateBounds: TRect;
begin
  R := ViewInfo.GetOffsetBounds(DX, DY, AUpdateBounds);
  if not IsRectEmpty(R) then
    Site.ScrollWindow(DX, 0, R);
  if not IsRectEmpty(AUpdateBounds) then
    Site.InvalidateRect(AUpdateBounds, True);
  Controller.InvalidateFocusedRecord;
  Site.Update;
end;

procedure TcxGridTablePainter.PaintContent;
begin
  DrawFindPanel;
  DrawGroupByBox;
  DrawFilterBar;
  DrawIndicator;
  DrawHeader;
  DrawFooter;
  inherited PaintContent;
end;

{ TcxGridColumnContainerViewInfo }

constructor TcxGridColumnContainerViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited;
  FItemHeight := -1;
  CreateItems;
end;

destructor TcxGridColumnContainerViewInfo.Destroy;
begin
  DestroyItems;
  inherited;
end;

function TcxGridColumnContainerViewInfo.GetController: TcxGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxGridColumnContainerViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridColumnContainerViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridColumnContainerViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxGridColumnContainerViewInfo.GetInternalItem(Index: Integer): TcxGridColumnHeaderViewInfo;
begin
  Result := TcxGridColumnHeaderViewInfo(FItems[Index]);
end;

function TcxGridColumnContainerViewInfo.GetItem(Index: Integer): TcxGridColumnHeaderViewInfo;
begin
  Result := InternalItems[Index];
  if Result = nil then
  begin
    Result := CreateItem(Index);
    FItems[Index] := Result;
  end;
end;

function TcxGridColumnContainerViewInfo.GetItemHeight: Integer;
begin
  if FItemHeight = -1 then
    FItemHeight := CalculateItemHeight;
  Result := FItemHeight;
end;

function TcxGridColumnContainerViewInfo.CreateItem(AIndex: Integer): TcxGridColumnHeaderViewInfo;
begin
  Result := GetItemClass.Create(Self, Columns[AIndex]);
end;

procedure TcxGridColumnContainerViewInfo.CreateItems;
begin
  FItems := TList.Create;
  FItems.Count := ColumnCount;
end;

procedure TcxGridColumnContainerViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do InternalItems[I].Free;
  FreeAndNil(FItems);
end;

function TcxGridColumnContainerViewInfo.GetItemClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridColumnHeaderViewInfo;
end;

function TcxGridColumnContainerViewInfo.CalculateItemHeight: Integer;
begin
  CalculateParams;
  Result := GetItemClass.GetCellHeight(GridViewInfo.GetFontHeight(Params.Font), LookAndFeelPainter, ScaleFactor);
end;

function TcxGridColumnContainerViewInfo.FindItem(AColumn: TcxCustomGridColumn): TcxGridColumnHeaderViewInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Column = AColumn then
      Exit;
  end;
  Result := nil;
end;

function TcxGridColumnContainerViewInfo.GetAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxGridColumnContainerViewInfo.GetColumnAdditionalWidth(AColumn: TcxGridColumn): Integer;
begin
  if AColumn.IsMostLeft then
    Result := GridViewInfo.FirstItemAdditionalWidth
  else
    Result := 0;
end;

function TcxGridColumnContainerViewInfo.GetColumnMinWidth(AColumn: TcxGridColumn): Integer;
begin
  if not AColumn.Options.AutoWidthSizable and AColumn.GridView.OptionsView.ColumnAutoWidth then
    Result := AColumn.Width
  else
    Result := AColumn.MinWidth;
  Result := Result + GetColumnAdditionalWidth(AColumn) + GetItemAdditionWidth(FindItem(AColumn))
end;

function TcxGridColumnContainerViewInfo.GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors;
begin
  Result := [];
end;

function TcxGridColumnContainerViewInfo.GetColumnWidth(AColumn: TcxGridColumn): Integer;
begin
  Result := AColumn.Width + GetColumnAdditionalWidth(AColumn) + GetItemAdditionWidth(FindItem(AColumn));
end;

function TcxGridColumnContainerViewInfo.GetItemAdditionWidth(AItem: TcxGridColumnHeaderViewInfo): Integer;
begin
  Result := AItem.GetAdditionalWidth;
end;

function TcxGridColumnContainerViewInfo.GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect;
begin
  Result := GridViewInfo.ScrollableAreaBoundsHorz;
end;

function TcxGridColumnContainerViewInfo.GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean;
begin
  Result := False;
end;

function TcxGridColumnContainerViewInfo.GetItemsAreaBounds: TRect;
begin
  Result := Bounds;
end;

function TcxGridColumnContainerViewInfo.GetItemsHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

function TcxGridColumnContainerViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnContainerPainter;
end;

function TcxGridColumnContainerViewInfo.GetZonesAreaBounds: TRect;
begin
  Result := Bounds;
  Result.Right := GridViewInfo.ClientBounds.Right;
end;

procedure TcxGridColumnContainerViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  if AHitTest is TcxCustomGridColumnHitTest then
    TcxCustomGridColumnHitTest(AHitTest).ColumnContainerKind := Kind;
end;

procedure TcxGridColumnContainerViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, DY);
end;

procedure TcxGridColumnContainerViewInfo.BeforeRecalculation;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].BeforeRecalculation;
end;

function TcxGridColumnContainerViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := GetItemsHitTest(P);
  if Result = nil then
    Result := inherited GetHitTest(P);
end;

function TcxGridColumnContainerViewInfo.GetZone(const P: TPoint): TcxGridItemContainerZone;
var
  R: TRect;
  AEndPos: Integer;
  ALastBound, I: Integer;
begin
  Result := nil;
  if not Visible then Exit;
  R := ZonesAreaBounds;
  if not PtInRect(R, P) then Exit;
  if IsRightToLeftConverted then
    ALastBound := R.Left
  else
    ALastBound := R.Right;
  for I := 0 to Count do
  begin
    if I = Count then
      AEndPos := ALastBound
    else
      AEndPos := (Items[I].Bounds.Left + Items[I].Bounds.Right) div 2;
    if IsRightToLeftConverted then
      R.Left := AEndPos
    else
      R.Right := AEndPos;
    if PtInRect(R, P) then
    begin
      Result := TcxGridItemContainerZone.Create(I);
      Break;
    end;
    if IsRightToLeftConverted then
      R.Right := R.Left
    else
      R.Left := R.Right;
  end;
end;

{ TcxGridColumnHeaderAreaViewInfo }

constructor TcxGridColumnHeaderAreaViewInfo.Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo);
begin
  inherited Create(AColumnHeaderViewInfo.GridViewInfo);
  FColumnHeaderViewInfo := AColumnHeaderViewInfo;
end;

function TcxGridColumnHeaderAreaViewInfo.GetColumn: TcxCustomGridColumn;
begin
  Result := FColumnHeaderViewInfo.Column;
end;

function TcxGridColumnHeaderAreaViewInfo.GetGridView: TcxGridTableView;
begin
  Result := FColumnHeaderViewInfo.GridView;
end;

function TcxGridColumnHeaderAreaViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := FColumnHeaderViewInfo.GridViewInfo;
end;

function TcxGridColumnHeaderAreaViewInfo.CanShowContainerHint: Boolean;
begin
  Result := False;
end;

function TcxGridColumnHeaderAreaViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridColumnHeaderAreaViewInfo.GetCanvas: TcxCanvas;
begin
  Result := GridViewInfo.Canvas;
end;

function TcxGridColumnHeaderAreaViewInfo.GetHeight: Integer;
begin
  Result := CalculateHeight;
end;

function TcxGridColumnHeaderAreaViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxGridColumnHeaderAreaViewInfo.HasMouse(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited HasMouse(AHitTest);
  if Result then
    with TcxCustomGridColumnHitTest(AHitTest) do
      Result := (Column = Self.Column) and
        (ColumnContainerKind = ColumnHeaderViewInfo.Container.Kind);
end;

procedure TcxGridColumnHeaderAreaViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FColumnHeaderViewInfo.InitHitTest(AHitTest);
  inherited;
end;

{procedure TcxGridColumnHeaderAreaViewInfo.Invalidate;
begin
  if GridView <> nil then
    GridView.ViewChanged(Bounds);
end;}

function TcxGridColumnHeaderAreaViewInfo.NeedsContainerHotTrack: Boolean;
begin
  Result := False;
end;

function TcxGridColumnHeaderAreaViewInfo.OccupiesSpace: Boolean;
begin
  Result := True;
end;

function TcxGridColumnHeaderAreaViewInfo.ResidesInContent: Boolean;
begin
  Result := True;
end;

procedure TcxGridColumnHeaderAreaViewInfo.Calculate(const ABounds: TRect;
  var ATextAreaBounds: TRect);

  procedure AlignHorizontally;
  var
    AAreaAndTextWidth: Integer;
  begin
    case AlignmentHorz of
      taLeftJustify:
        begin
          Bounds.Right := Bounds.Left + Width;
          if OccupiesSpace then
            ATextAreaBounds.Left := Bounds.Right;
        end;
      taRightJustify:
        begin
          Bounds.Left := Bounds.Right - Width;
          if OccupiesSpace then
            ATextAreaBounds.Right := Bounds.Left;
        end;
      taCenter:
        if OccupiesSpace and (ColumnHeaderViewInfo.AlignmentHorz = taCenter) and
          not ((AlignmentVert = vaTop) and (ColumnHeaderViewInfo.AlignmentVert = vaBottom) or
               (AlignmentVert = vaBottom) and (ColumnHeaderViewInfo.AlignmentVert = vaTop)) then
        begin
          AAreaAndTextWidth := Width + ColumnHeaderViewInfo.TextWidthWithOffset;
          if AAreaAndTextWidth < Bounds.Right - Bounds.Left then
            Inc(Bounds.Left, (Bounds.Right - Bounds.Left - AAreaAndTextWidth) div 2);
          Bounds.Right := Bounds.Left + Width;
          ATextAreaBounds.Left := Bounds.Right;
          ATextAreaBounds.Right :=
            Min(ATextAreaBounds.Right, ATextAreaBounds.Left + ColumnHeaderViewInfo.TextWidthWithOffset);
        end
        else
        begin
          Inc(Bounds.Left, (Bounds.Right - Bounds.Left - Width) div 2);
          Bounds.Right := Bounds.Left + Width;
        end;
    end;
  end;

  procedure AlignVertically;
  begin
    case AlignmentVert of
      vaTop:
        Bounds.Bottom := Bounds.Top + Height;
      vaBottom:
        Bounds.Top := Bounds.Bottom - Height;
      vaCenter:
        begin
          Inc(Bounds.Top, (Bounds.Bottom - Bounds.Top - Height) div 2);
          Bounds.Bottom := Bounds.Top + Height;
        end;
    end;
  end;

begin
  if ResidesInContent then
    Bounds := ATextAreaBounds
  else
    Bounds := ABounds;
  if Width <> 0 then AlignHorizontally;
  if Height <> 0 then AlignVertically;
  with Bounds do
    inherited Calculate(Left, Top, Right - Left, Bottom - Top);
end;

{ TcxGridColumnHeaderSortingMarkViewInfo }

function TcxGridColumnHeaderSortingMarkViewInfo.GetSortOrder: TcxGridSortOrder;
begin
  Result := Column.SortOrder;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.CalculateHeight: Integer;
begin
  Result := LookAndFeelPainter.ScaledSortingMarkAreaSize(ScaleFactor).Y;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.CalculateWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledSortingMarkAreaSize(ScaleFactor).X;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.CanShowContainerHint: Boolean;
begin
  Result := True;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taRightJustify;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := nil;
end;

function TcxGridColumnHeaderSortingMarkViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnHeaderSortingMarkPainter;
end;

{ TcxGridColumnHeaderHorzSizingEdgeViewInfo }

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.CalculateWidth: Integer;
begin
  Result := ScaleFactor.Apply(cxGridHeaderSizingEdgeSize);
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taRightJustify;
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridColumnHeaderHorzSizingEdgeHitTest;
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := nil;
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.OccupiesSpace: Boolean;
begin
  Result := False;
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.ResidesInContent: Boolean;
begin
  Result := False;
end;

procedure TcxGridColumnHeaderHorzSizingEdgeViewInfo.Calculate(const ABounds: TRect;
  var ATextAreaBounds: TRect);
begin
  inherited;
  OffsetRect(Bounds, Width div 2, 0);
end;

function TcxGridColumnHeaderHorzSizingEdgeViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AColumn: TcxCustomGridColumn;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and (ssDouble in AShift) then
  begin
    AColumn := Column;
    AColumn.ApplyBestFit(True, True);
    Result := True;
  end;
end;

{ TcxGridColumnHeaderFilterButtonViewInfo }

function TcxGridColumnHeaderFilterButtonViewInfo.GetActive: Boolean;
begin
  Result := ColumnHeaderViewInfo.IsFilterActive;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetActuallyVisible: Boolean;
begin
  Result := Visible and (AlwaysVisible or (ColumnHeaderViewInfo.State <> gcsNone) or
    (State = gcsPressed) or (IsSmartTag and Active));
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetDropDownWindowValue: TdxFilterPopupWindow;
begin
  Result := TdxFilterPopupWindow(inherited DropDownWindow);
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetFilterDropDownWindowLinkComponent: TComponent;
begin
  Result := Column;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := Column.GetFilterPopupMode;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetFilterDropDownWindowOptions: TObject;
begin
  Result := Column.Options;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.CalculateHeight: Integer;
begin
  if IsSmartTag then
    Result := LookAndFeelPainter.ScaledFilterSmartTagSize(ScaleFactor).cy
  else
    Result := LookAndFeelPainter.ScaledFilterDropDownButtonSize(ScaleFactor).Y;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.CalculateWidth: Integer;
begin
  if IsSmartTag then
    Result := LookAndFeelPainter.ScaledFilterSmartTagSize(ScaleFactor).cx
  else
    Result := LookAndFeelPainter.ScaledFilterDropDownButtonSize(ScaleFactor).X;
end;

procedure TcxGridColumnHeaderFilterButtonViewInfo.DropDown;
begin
  GridView.Controller.IsFilterPopupOpenedFromHeader := ColumnHeaderViewInfo.HasHeaderAsContainer;
  inherited;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.EmulateMouseMoveAfterCalculate: Boolean;
begin
  Result := True;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taRightJustify;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  if IsSmartTag then
    Result := vaTop
  else
    Result := inherited GetAlignmentVert
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetAlwaysVisible: Boolean;
begin
  Result := (GridView.OptionsView.ShowColumnFilterButtons = sfbAlways) or
    (GridView.OptionsView.ShowColumnFilterButtons = sfbDefault) and cxIsTouchModeEnabled;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  if GridView.IsDesigning then
    Result := nil
  else
    Result := TcxGridColumnHeaderFilterButtonHitTest;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetIsVisibleForPainting: Boolean;
begin
  Result := ActuallyVisible;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnHeaderFilterButtonPainter;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.NeedsContainerHotTrack: Boolean;
begin
  Result := not AlwaysVisible;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.OccupiesSpace: Boolean;
begin
  Result := ColumnHeaderViewInfo.HasFixedContentSpace or ActuallyVisible;
end;

procedure TcxGridColumnHeaderFilterButtonViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  if not IsDestroying and not ActuallyVisible then
  begin
    if State in [gcsSelected] then
      ColumnHeaderViewInfo.State := gcsSelected;
    ColumnHeaderViewInfo.Update;
  end;
  inherited;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.CloseDropDownWindowOnDestruction: Boolean;
begin
  Result := False;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.DropDownWindowExists: Boolean;
begin
  Result := GridView.Controller.HasFilterPopup;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := GridView.Controller.FilterPopup;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz;
begin
  if dxGetFilterPopupActualMode(GetFilterPopupMode) = fpmExcel then
    Result := pahLeft
  else
    Result := inherited GetDropDownWindowDefaultAlignHorz;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := inherited GetDropDownWindowOwnerBounds;
  if dxGetFilterPopupActualMode(GetFilterPopupMode) <> fpmExcel then
  begin
    Result.Left := ColumnHeaderViewInfo.Bounds.Left;
    Result.Right := ColumnHeaderViewInfo.Bounds.Right;
  end;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.IsDropDownWindowOwner: Boolean;
begin
  Result := inherited IsDropDownWindowOwner and (DropDownWindow.LinkComponent = GetFilterDropDownWindowLinkComponent) and
    (GridView.Controller.IsFilterPopupOpenedFromHeader = ColumnHeaderViewInfo.HasHeaderAsContainer);
end;

function TcxGridColumnHeaderFilterButtonViewInfo.IsSmartTag: Boolean;
begin
  Result := (GridView.OptionsView.ItemFilterButtonShowMode = fbmSmartTag) or
    (GridView.OptionsView.ItemFilterButtonShowMode = fbmDefault) and not cxIsTouchModeEnabled;
end;

function TcxGridColumnHeaderFilterButtonViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := inherited MouseMove(AHitTest, AShift);
  if State = gcsPressed then
    ColumnHeaderViewInfo.State := gcsSelected;
end;

{ TcxGridColumnHeaderGlyphViewInfo }

constructor TcxGridColumnHeaderGlyphViewInfo.Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo);
begin
  inherited Create(AColumnHeaderViewInfo);
  FUseImages := Glyph.Empty;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := Column.HeaderGlyph;
end;

function TcxGridColumnHeaderGlyphViewInfo.CalculateHeight: Integer;
begin
  Result := dxGetImageSize(Glyph, Images, ImageIndex, ScaleFactor).cy;
end;

function TcxGridColumnHeaderGlyphViewInfo.CalculateWidth: Integer;
begin
  Result := dxGetImageSize(Glyph, Images, ImageIndex, ScaleFactor).cx;
end;

function TcxGridColumnHeaderGlyphViewInfo.CanShowContainerHint: Boolean;
begin
  Result := True;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := Column.HeaderGlyphAlignmentHorz;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := Column.HeaderGlyphAlignmentVert;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := nil;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetImageIndex: TcxImageIndex;
begin
  Result := Column.HeaderImageIndex;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetImages: TCustomImageList;
begin
  Result := Column.GridView.GetImages;
end;

function TcxGridColumnHeaderGlyphViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnHeaderGlyphPainter;
end;

{ TcxGridColumnHeaderCheckBoxViewInfo }

function TcxGridHeaderCheckBoxViewInfo.CalculateCheckBoxState: TcxCheckBoxState;
begin
  if GridView.DataController.AreAllRowsSelected then
    Result := cbsChecked
  else
    if Controller.SelectedRowCount > 0 then
      Result := cbsGrayed
    else
      Result := cbsUnchecked;
end;

procedure TcxGridHeaderCheckBoxViewInfo.Toggle;
begin
  if CheckBoxState = cbsChecked then
    Controller.ClearSelection
  else
    Controller.SelectAllRecords;
end;

function TcxGridHeaderCheckBoxViewInfo.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

{ TcxGridRowCheckBoxViewInfo }

constructor TcxGridRowCheckBoxViewInfo.Create(ARowViewInfo: TcxCustomGridRowViewInfo);
begin
  FRowViewInfo := ARowViewInfo;
  inherited Create(RowViewInfo.GridViewInfo);
end;

function TcxGridRowCheckBoxViewInfo.CalculateCheckBoxState: TcxCheckBoxState;
begin
  Result := RowViewInfo.CheckBoxState;
end;

function TcxGridRowCheckBoxViewInfo.GetIsVisibleForPainting: Boolean;
begin
  Result := inherited GetIsVisibleForPainting and ((GridView.OptionsSelection.CheckBoxPosition = cbpIndicator) or
    not GridView.OptionsSelection.ShowCheckBoxesDynamically or ((CheckBoxState in [cbsChecked, cbsGrayed]) or IsHotTracked or
    (ButtonState = cxbsPressed) or RowViewInfo.Focused or RowViewInfo.IsHotTracked));
end;

procedure TcxGridRowCheckBoxViewInfo.Toggle;
begin
  RowViewInfo.ToggleCheckBox;
end;

function TcxGridRowCheckBoxViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

{ TcxGridColumnHeaderCheckBoxViewInfo }

constructor TcxGridColumnHeaderCheckBoxAreaViewInfo.Create(AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo);
begin
  inherited Create(AColumnHeaderViewInfo);
  FCheckBox := TcxGridHeaderCheckBoxViewInfo.Create(GridViewInfo);
end;

destructor TcxGridColumnHeaderCheckBoxAreaViewInfo.Destroy;
begin
  FreeAndNil(FCheckBox);
  inherited Destroy;
end;

procedure TcxGridColumnHeaderCheckBoxAreaViewInfo.AfterRecalculation;
begin
  CheckBox.AfterRecalculation;
  inherited AfterRecalculation;
end;

procedure TcxGridColumnHeaderCheckBoxAreaViewInfo.BeforeRecalculation;
begin
  inherited BeforeRecalculation;
  CheckBox.BeforeRecalculation;
end;

procedure TcxGridColumnHeaderCheckBoxAreaViewInfo.Calculate(const ABounds: TRect; var ATextAreaBounds: TRect);
begin
  inherited Calculate(ABounds, ATextAreaBounds);
  CheckBox.Calculate(GetCheckBoxRect);
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := CheckBox.GetHitTest(P);
  if not GridView.IsDesigning and (AHitTest <> nil) then
    Result := AHitTest
  else
    Result := nil;
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.CalculateHeight: Integer;
begin
  Result := GetMargins.Top + CheckBox.Height + GetMargins.Bottom;
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.CalculateWidth: Integer;
begin
  Result := GetMargins.Left + CheckBox.Width + GetMargins.Right;
end;

procedure TcxGridColumnHeaderCheckBoxAreaViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  CheckBox.RightToLeftConversion(ABounds);
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.GetCheckBoxRect: TRect;
begin
  Result := Bounds;
  Inc(Result.Left, GetMargins.Left);
  Inc(Result.Top, GetMargins.Top);
  Dec(Result.Right, GetMargins.Right);
  Dec(Result.Bottom, GetMargins.Bottom);
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.GetMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(2, 0, 2, 0));
end;

function TcxGridColumnHeaderCheckBoxAreaViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnHeaderCheckBoxPainter;
end;

procedure TcxGridColumnHeaderCheckBoxAreaViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  CheckBox.DoOffset(DX, DY);
end;

{ TcxGridColumnHeaderViewInfo }

constructor TcxGridColumnHeaderViewInfo.Create(AContainer: TcxGridColumnContainerViewInfo;
  AColumn: TcxGridColumn);
begin
  inherited Create(AContainer.GridViewInfo);
  FAreaViewInfos := TList.Create;
  FContainer := AContainer;
  FColumn := AColumn;
  FWidth := -1;
  Width := -1;
  CreateAreaViewInfos;
end;

destructor TcxGridColumnHeaderViewInfo.Destroy;
begin
  DestroyAreaViewInfos;
  FAreaViewInfos.Free;
  inherited Destroy;
end;

function TcxGridColumnHeaderViewInfo.GetAreaViewInfoCount: Integer;
begin
  Result := FAreaViewInfos.Count;
end;

function TcxGridColumnHeaderViewInfo.GetAreaViewInfo(Index: Integer): TcxGridColumnHeaderAreaViewInfo;
begin
  Result := TcxGridColumnHeaderAreaViewInfo(FAreaViewInfos[Index]);
end;

function TcxGridColumnHeaderViewInfo.GetGridView: TcxGridTableView;
begin
  Result := FContainer.GridView;
//  if Result.IsDestroying then Result := nil;
end;

function TcxGridColumnHeaderViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := FContainer.GridViewInfo;
end;

function TcxGridColumnHeaderViewInfo.GetHasTextOffsetLeft: Boolean;
begin
  Result := CalculateHasTextOffset(taLeftJustify);
end;

function TcxGridColumnHeaderViewInfo.GetHasTextOffsetRight: Boolean;
begin
  Result := CalculateHasTextOffset(taRightJustify);
end;

function TcxGridColumnHeaderViewInfo.GetIndex: Integer;
begin
  Result := FColumn.VisibleIndex;
end;

function TcxGridColumnHeaderViewInfo.GetIsFixed: Boolean;
begin
  Result := FColumn.Fixed;
end;

function TcxGridColumnHeaderViewInfo.GetOriginalWidth: Integer;
begin
  if FOriginalWidth = 0 then
    FOriginalWidth := CalculateOriginalWidth(Width);
  Result := FOriginalWidth;
end;

function TcxGridColumnHeaderViewInfo.GetRealWidth: Integer;
begin
  if FRealWidth = 0 then
    FRealWidth := CalculateRealWidth(Width);
  Result := FRealWidth;
end;

procedure TcxGridColumnHeaderViewInfo.EnumAreaViewInfoClasses(AClass: TClass);
begin
  FAreaViewInfos.Add(TcxGridColumnHeaderAreaViewInfoClass(AClass).Create(Self));
end;

procedure TcxGridColumnHeaderViewInfo.CreateAreaViewInfos;
begin
  GetAreaViewInfoClasses(EnumAreaViewInfoClasses);
end;

procedure TcxGridColumnHeaderViewInfo.DestroyAreaViewInfos;
var
  I: Integer;
begin
  for I := 0 to AreaViewInfoCount - 1 do
    AreaViewInfos[I].Free;
end;

function TcxGridColumnHeaderViewInfo.AreasNeedHotTrack: Boolean;
var
  I: Integer;
begin
  for I := 0 to AreaViewInfoCount - 1 do
  begin
    Result := AreaViewInfos[I].NeedsContainerHotTrack;
    if Result then Exit;
  end;
  Result := False;
end;

procedure TcxGridColumnHeaderViewInfo.CalculateCellBoundsForHint;
var
  I: Integer;
begin
  if IsHintForText then
    FCellBoundsForHint := inherited GetCellBoundsForHint
  else
  begin
    FCellBoundsForHint := cxRectInflate(ClientBounds, -ScaleFactor.Apply(cxGridCellTextOffset));
    for I := 0 to AreaViewInfoCount - 1 do
      with AreaViewInfos[I] do
        if OccupiesSpace and not CanShowContainerHint then
          case AlignmentHorz of
            taRightJustify:
              FCellBoundsForHint.Right := Min(Bounds.Left, FCellBoundsForHint.Right);
            taLeftJustify:
              FCellBoundsForHint.Left := Max(Bounds.Right, FCellBoundsForHint.Left);
          end;
  end;
end;

function TcxGridColumnHeaderViewInfo.CalculateHasTextOffset(ASide: TAlignment): Boolean;
var
  I: Integer;
begin
  if Text <> '' then
  begin
    Result := True;
    for I := 0 to AreaViewInfoCount - 1 do
      with AreaViewInfos[I] do
        if OccupiesSpace and (AlignmentHorz = ASide) then Exit;
  end;
  Result := False;
end;

function TcxGridColumnHeaderViewInfo.CalculateHeight: Integer;
var
  ATouchableElementHeight: Integer;
begin
  Result := GetTextCellHeight(GridViewInfo, LookAndFeelPainter);
  if cxIsTouchModeEnabled then
  begin
    ATouchableElementHeight := Result;
    Dec(ATouchableElementHeight, 2 * ScaleFactor.Apply(cxGridCellTextOffset));
    dxAdjustToTouchableSize(ATouchableElementHeight, ScaleFactor);
    Inc(ATouchableElementHeight, 2 * ScaleFactor.Apply(cxGridCellTextOffset));
    Result := ATouchableElementHeight;
  end;
end;

function TcxGridColumnHeaderViewInfo.CalculateOriginalWidth(Value: Integer): Integer;
begin
  Result := CalculateRealWidth(Value) - Container.GetItemAdditionWidth(Self);
end;

function TcxGridColumnHeaderViewInfo.CalculateRealWidth(Value: Integer): Integer;
begin
  Result := Value - FContainer.GetColumnAdditionalWidth(Column);
end;

procedure TcxGridColumnHeaderViewInfo.CalculateTextAreaBounds;
var
  I: Integer;
begin
  FTextAreaBounds := cxRectContent(ContentBounds, LookAndFeelPainter.HeaderContentOffsets(ScaleFactor));
  for I := 0 to AreaViewInfoCount - 1 do
    AreaViewInfos[I].Calculate(Bounds, FTextAreaBounds);
  if HasTextOffsetLeft then
    Inc(FTextAreaBounds.Left, ScaleFactor.Apply(cxGridCellTextOffset));
  if HasTextOffsetRight then
    Dec(FTextAreaBounds.Right, ScaleFactor.Apply(cxGridCellTextOffset));
end;

procedure TcxGridColumnHeaderViewInfo.CalculateVisible(ALeftBound, AWidth: Integer);
begin
  with GridViewInfo.ClientBounds do
    Visible := (ALeftBound < Right) and (ALeftBound + AWidth > Left);
end;

function TcxGridColumnHeaderViewInfo.CalculateWidth: Integer;
begin
  if FWidth = -1 then
    FWidth := FContainer.GetColumnWidth(Column);
  Result := FWidth;
end;

function TcxGridColumnHeaderViewInfo.CanFilter: Boolean;
begin
  Result := FColumn.CanFilter(True);
end;

function TcxGridColumnHeaderViewInfo.CanHorzSize: Boolean;
begin
  Result := FColumn.CanHorzSize and (Container.Kind = ckHeader);
end;

function TcxGridColumnHeaderViewInfo.CanPress: Boolean;
begin
  Result := True;
end;

function TcxGridColumnHeaderViewInfo.CanShowHint: Boolean;
begin
  Result := GridView.OptionsBehavior.ColumnHeaderHints;
end;

function TcxGridColumnHeaderViewInfo.CanSort: Boolean;
begin
  Result := FColumn.SortOrder <> soNone;
end;

function TcxGridColumnHeaderViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

procedure TcxGridColumnHeaderViewInfo.CheckWidth(var Value: Integer);
begin
  if Value < MinWidth then Value := MinWidth;
  if Value > MaxWidth then Value := MaxWidth;
end;

function TcxGridColumnHeaderViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
  begin
    FColumn.DoCustomDrawHeader(ACanvas, Self, Result);
    if not Result then
      GridView.DoCustomDrawColumnHeader(ACanvas, Self, Result);
  end;
end;

procedure TcxGridColumnHeaderViewInfo.DoCalculateParams;
begin
  FNeighbors := FContainer.GetColumnNeighbors(Column);
  inherited;
  CalculateTextAreaBounds;
end;

function TcxGridColumnHeaderViewInfo.FindArea(AAreaClass: TcxGridColumnHeaderAreaViewInfoClass): TcxGridColumnHeaderAreaViewInfo;
var
  I: Integer;
begin
  for I := 0 to AreaViewInfoCount - 1 do
  begin
    Result := AreaViewInfos[I];
    if Result.ClassType = AAreaClass then
      Exit;
  end;
  Result := nil;
end;

function TcxGridColumnHeaderViewInfo.GetActualState: TcxGridCellState;
begin
  if IsPressed then
    Result := gcsPressed
  else
    Result := inherited GetActualState;
end;

function TcxGridColumnHeaderViewInfo.GetAdditionalWidth: Integer;
var
  APinWidth: Integer;
  AWidth: Integer;
begin
  Result := 0;
  if HasCheckBox then
    Inc(Result, GetCheckBoxAreaWidth);
  if HasDataCellCheckBoxAdditionalWidth then
  begin
    AWidth := ScaleFactor.Apply(cxGridDataCellCheckBoxLeftMargin) + LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cx +
      ScaleFactor.Apply(cxGridDataCellCheckBoxRightMargin);
    Result := Max(Result, AWidth);
  end;
  if HasDataCellPinAdditionalWidth then
  begin
    if GridView.FixedDataRows.PinSize.Width > 0 then
      APinWidth := GridView.FixedDataRows.PinSize.Width
    else
      APinWidth := cxGridPinWidth;
    AWidth := ScaleFactor.Apply(APinWidth + cxGridPinLeftMargin + cxGridPinRightMargin);
    if HasDataCellCheckBoxAdditionalWidth then
      Inc(Result, AWidth)
    else
      Result := Max(Result, AWidth);
  end;
end;

function TcxGridColumnHeaderViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := FColumn.HeaderAlignmentHorz;
end;

function TcxGridColumnHeaderViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := FColumn.HeaderAlignmentVert;
end;

function TcxGridColumnHeaderViewInfo.GetAreaBounds: TRect;
begin
  Result := Container.GetItemAreaBounds(Self);
end;

procedure TcxGridColumnHeaderViewInfo.GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc);
begin
  if HasCheckBox then
    AProc(TcxGridColumnHeaderCheckBoxAreaViewInfo);
  if CanHorzSize then
    AProc(TcxGridColumnHeaderHorzSizingEdgeViewInfo);
  if CanFilter then
    AProc(TcxGridColumnHeaderFilterButtonViewInfo);
  if CanSort then
    AProc(TcxGridColumnHeaderSortingMarkViewInfo);
  if HasGlyph then
    AProc(TcxGridColumnHeaderGlyphViewInfo);
end;

function TcxGridColumnHeaderViewInfo.GetAutoWidthSizable: Boolean;
begin
  Result := Column.Options.AutoWidthSizable and not IsFixed;
end;

function TcxGridColumnHeaderViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridViewInfo.HeaderViewInfo.ColumnBackgroundBitmap;
end;

function TcxGridColumnHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := LookAndFeelPainter.HeaderBorders(Neighbors);
end;

function TcxGridColumnHeaderViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := GetCellBorderWidth(LookAndFeelPainter);
end;

function TcxGridColumnHeaderViewInfo.GetCanvas: TcxCanvas;
begin
  Result := GridViewInfo.Canvas;
end;

function TcxGridColumnHeaderViewInfo.GetCaption: string;
begin
  Result := Column.VisibleCaption;
end;

class function TcxGridColumnHeaderViewInfo.GetCellBorderWidth(ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer;
begin
  Result := ALookAndFeelPainter.HeaderBorderSize;
end;

class function TcxGridColumnHeaderViewInfo.GetCellHeight(ATextHeight: Integer;
  ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ATextHeight + 2 * GetCellBorderWidth(ALookAndFeelPainter) +
    cxMarginsHeight(ALookAndFeelPainter.HeaderContentOffsets(AScaleFactor));
end;

function TcxGridColumnHeaderViewInfo.GetCheckBoxAreaWidth: Integer;
begin
  Result := FindArea(TcxGridColumnHeaderCheckBoxAreaViewInfo).Width;
end;

function TcxGridColumnHeaderViewInfo.GetDataOffset: Integer;
begin
  if IsRightToLeftConverted then
    Result := Bounds.Left
  else
    Result := Bounds.Right - RealWidth;
end;

function TcxGridColumnHeaderViewInfo.GetHeight: Integer;
begin
  Result := inherited GetHeight - FAdditionalHeightAtTop;
end;

function TcxGridColumnHeaderViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridColumnHeaderHitTest;
end;

function TcxGridColumnHeaderViewInfo.GetHotTrack: Boolean;
begin
  Result := LookAndFeelPainter.IsHeaderHotTrack or AreasNeedHotTrack;
end;

function TcxGridColumnHeaderViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(FColumn);
end;

function TcxGridColumnHeaderViewInfo.GetIsPressed: Boolean;
begin
  Result := (State = gcsPressed) or (GridViewInfo.Controller.PressedColumn = Column);
end;

function TcxGridColumnHeaderViewInfo.GetMaxWidth: Integer;
var
  AIndex, I: Integer;
begin
  if GridView.OptionsView.ColumnAutoWidth then
  begin
    Result := GridViewInfo.ClientWidth;
    AIndex := Column.VisibleIndex;
    if AIndex = FContainer.Count - 1 then
      for I := 0 to AIndex - 1 do
        Dec(Result, FContainer[I].MinWidth)
    else
      for I := 0 to FContainer.Count - 1 do
      begin
        if I < AIndex then
          Dec(Result, FContainer[I].Width);
        if I > AIndex then
          Dec(Result, FContainer[I].MinWidth);
      end;
    if Result < MinWidth then Result := MinWidth;
  end
  else
    Result := cxMaxRectSize;
end;

function TcxGridColumnHeaderViewInfo.GetMinWidth: Integer;
begin
  if IsFixed then
    Result := CalculateWidth
  else
    Result := FContainer.GetColumnMinWidth(Column);
end;

function TcxGridColumnHeaderViewInfo.GetMultiLine: Boolean;
begin
  Result := FContainer.AutoHeight;
end;

function TcxGridColumnHeaderViewInfo.GetMultiLinePainting: Boolean;
begin
  Result := inherited GetMultiLinePainting or FContainer.GetItemMultiLinePainting(Self);
end;

function TcxGridColumnHeaderViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridColumnHeaderPainter;
end;

function TcxGridColumnHeaderViewInfo.GetRealBounds: TRect;
begin
  Result := inherited GetRealBounds;
  Inc(Result.Left, FAdditionalWidthAtLeft);
  Inc(Result.Top, FAdditionalHeightAtTop);
end;

function TcxGridColumnHeaderViewInfo.GetRealTextAreaBounds: TRect;
begin
  Result := GetTextAreaBounds;
end;

function TcxGridColumnHeaderViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.HeaderEndEllipsis;
end;

function TcxGridColumnHeaderViewInfo.GetText: string;
begin
  if Column.Options.ShowCaption then
    Result := Caption
  else
    Result := '';
end;

function TcxGridColumnHeaderViewInfo.GetTextAreaBounds: TRect;
begin
  Result := FTextAreaBounds;
end;

function TcxGridColumnHeaderViewInfo.GetTextHeightWithOffset: Integer;
begin
  Result := TextHeight + cxMarginsHeight(LookAndFeelPainter.HeaderContentOffsets(ScaleFactor));
end;

function TcxGridColumnHeaderViewInfo.GetTextWidthWithOffset: Integer;
begin
  Result := TextWidth + cxMarginsWidth(LookAndFeelPainter.HeaderContentOffsets(ScaleFactor));
end;

procedure TcxGridColumnHeaderViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Column.Styles.GetHeaderParams(AParams);
end;

function TcxGridColumnHeaderViewInfo.GetWidth: Integer;
begin
  Result := inherited GetWidth - FAdditionalWidthAtLeft;
end;

function TcxGridColumnHeaderViewInfo.HasCheckBox: Boolean;
begin
  Result := (Container.Kind = ckHeader) and Column.IsMostLeft and
    (cbvColumnHeader in GridView.OptionsSelection.CheckBoxVisibility) and
    (GridView.OptionsSelection.CheckBoxPosition = cbpFirstColumn);
end;

function TcxGridColumnHeaderViewInfo.HasCustomDraw: Boolean;
begin
  Result := Column.HasCustomDrawHeader or GridView.HasCustomDrawColumnHeader;
end;

function TcxGridColumnHeaderViewInfo.HasFixedContentSpace: Boolean;
begin
  Result := False;
end;

function TcxGridColumnHeaderViewInfo.HasGlyph: Boolean;
begin
  Result := FColumn.HasGlyph;
end;

function TcxGridColumnHeaderViewInfo.HasHeaderAsContainer: Boolean;
begin
  Result := FContainer = GridViewInfo.HeaderViewInfo;
end;

function TcxGridColumnHeaderViewInfo.HasDataCellCheckBoxAdditionalWidth: Boolean;
begin
  Result := (Container.Kind = ckHeader) and Column.IsMostLeft and
    (cbvDataRow in GridView.OptionsSelection.CheckBoxVisibility) and
    (GridView.OptionsSelection.CheckBoxPosition = cbpFirstColumn);
end;

function TcxGridColumnHeaderViewInfo.HasDataCellPinAdditionalWidth: Boolean;
begin
  Result := (Container.Kind = ckHeader) and Column.IsMostLeft and (GridView.FixedDataRows.PinVisibility <> rpvNever);
end;

procedure TcxGridColumnHeaderViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FContainer.InitHitTest(AHitTest);
  inherited;
  (AHitTest as TcxCustomGridColumnHitTest).Column := Column;
end;

function TcxGridColumnHeaderViewInfo.DesignMouseDown(
  AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := True;
  if AButton = mbRight then
  begin
    if not GridView.Controller.DesignController.IsObjectSelected(FColumn) then
      GridView.Controller.DesignController.SelectObject(FColumn, not (ssShift in AShift));
  end
  else
    GridView.Controller.DesignController.SelectObject(FColumn, not (ssShift in AShift));
end;

function TcxGridColumnHeaderViewInfo.HasDesignPopupMenu: Boolean;
begin
  Result := True;
end;

procedure TcxGridColumnHeaderViewInfo.PopulateDesignPopupMenu(AMenu: TPopupMenu);
begin
  GridView.Controller.PopulateColumnHeaderDesignPopupMenu(AMenu);
end;

function TcxGridColumnHeaderViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := FCellBoundsForHint;
end;

function TcxGridColumnHeaderViewInfo.GetHintText: string;
begin
  if HasCustomHint then
    Result := Column.HeaderHint
  else
    Result := inherited GetHintText;
end;

function TcxGridColumnHeaderViewInfo.GetHintTextRect(const AMousePos: TPoint): TRect;
begin
  if IsHintForText then
    Result := inherited GetHintTextRect(AMousePos)
  else
    Result := GetBoundsForHint;
end;

function TcxGridColumnHeaderViewInfo.HasCustomHint: Boolean;
begin
  Result := Column.HeaderHint <> '';
end;

function TcxGridColumnHeaderViewInfo.IsHintForText: Boolean;
begin
  Result := not HasCustomHint;
end;

function TcxGridColumnHeaderViewInfo.IsHintMultiLine: Boolean;
begin
  Result := not HasCustomHint and inherited IsHintMultiLine;
end;

procedure TcxGridColumnHeaderViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  OffsetRect(FTextAreaBounds, DX, DY);
  OffsetRect(FCellBoundsForHint, DX, DY);
  for I := 0 to AreaViewInfoCount - 1 do
    AreaViewInfos[I].DoOffset(DX, DY);
end;

procedure TcxGridColumnHeaderViewInfo.SetWidth(Value: Integer);
begin
  inherited;
  FWidth := Value;
end;

procedure TcxGridColumnHeaderViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  if not IsDestroying and AreasNeedHotTrack then
    Recalculate;
  inherited;
end;

procedure TcxGridColumnHeaderViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);

  procedure CheckHiddenBorders(var AAdditionalWidthAtLeft, AAdditionalHeightAtTop: Integer);
  var
    AHiddenBorders: TcxBorders;
  begin
    CalculateParams;
    AHiddenBorders := cxBordersAll - Borders;
    if AHiddenBorders <> [] then
    begin
      if bLeft in AHiddenBorders then
      begin
        Dec(ALeftBound, BorderWidth[bLeft]);
        Inc(AWidth, BorderWidth[bLeft]);
        Inc(AAdditionalWidthAtLeft, BorderWidth[bLeft]);
      end;
      if bTop in AHiddenBorders then
      begin
        Dec(ATopBound, BorderWidth[bTop]);
        Inc(AHeight, BorderWidth[bTop]);
        Inc(AAdditionalHeightAtTop, BorderWidth[bTop]);
      end;
      Borders := cxBordersAll;
    end;
  end;

begin
  FAdditionalWidthAtLeft := 0;
  FAdditionalHeightAtTop := 0;
  if AWidth = -1 then
    AWidth := CalculateWidth;
  CalculateVisible(ALeftBound, AWidth);
  FIsFilterActive := Column.Filtered;
  CheckHiddenBorders(FAdditionalWidthAtLeft, FAdditionalHeightAtTop);
  inherited;
  {if Visible then }CalculateTextAreaBounds;
  CalculateCellBoundsForHint;
end;

procedure TcxGridColumnHeaderViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  FCellBoundsForHint := TdxRightToLeftLayoutConverter.ConvertRect(FCellBoundsForHint, ABounds);
  FTextAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextAreaBounds, ABounds);
  for I := 0 to AreaViewInfoCount - 1 do
    AreaViewInfos[I].RightToLeftConversion(ABounds);
end;

function TcxGridColumnHeaderViewInfo.GetBestFitWidth: Integer;
var
  I: Integer;
begin
  Result := inherited GetBestFitWidth - FAdditionalWidthAtLeft - Container.GetItemAdditionWidth(Self);
  if HasTextOffsetLeft then
    Inc(Result, ScaleFactor.Apply(cxGridCellTextOffset));
  if HasTextOffsetRight then
    Inc(Result, ScaleFactor.Apply(cxGridCellTextOffset));
  for I := 0 to AreaViewInfoCount - 1 do
    with AreaViewInfos[I] do
      if OccupiesSpace then Inc(Result, Width);
end;

function TcxGridColumnHeaderViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to AreaViewInfoCount - 1 do
  begin
    Result := AreaViewInfos[I].GetHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := inherited GetHitTest(P);
end;

procedure TcxGridColumnHeaderViewInfo.InitAutoWidthItem(AAutoWidthItem: TcxAutoWidthItem);
begin
  AAutoWidthItem.MinWidth := MinWidth;
  AAutoWidthItem.Width := CalculateWidth;
  AAutoWidthItem.Fixed := not GetAutoWidthSizable;
end;

function TcxGridColumnHeaderViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if not (ssDouble in AShift) and CanPress then
  begin
    if GridView.IsDesigning then
      Result := DesignMouseDown(AHitTest, AButton, AShift)
    else
      if AButton = mbLeft then
      begin
        GridView.Controller.PressedColumn := FColumn;
        Result := True;
      end;
  end;
end;

{ TcxGridHeaderViewInfoSpecific }

constructor TcxGridHeaderViewInfoSpecific.Create(AContainerViewInfo: TcxGridHeaderViewInfo);
begin
  inherited Create;
  FContainerViewInfo := AContainerViewInfo;
end;

function TcxGridHeaderViewInfoSpecific.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := FContainerViewInfo.GridViewInfo;
end;

function TcxGridHeaderViewInfoSpecific.GetItemHeight: Integer;
begin
  Result := FContainerViewInfo.ItemHeight;
end;

function TcxGridHeaderViewInfoSpecific.CalculateHeight: Integer;
begin
  Result := ItemHeight;
end;

function TcxGridHeaderViewInfoSpecific.GetHeight: Integer;
begin
  Result := CalculateHeight;
end;

{ TcxGridHeaderViewInfo }

constructor TcxGridHeaderViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited;
  FSpecific := GridViewInfo.GetHeaderViewInfoSpecificClass.Create(Self);
end;

destructor TcxGridHeaderViewInfo.Destroy;
begin
  FSpecific.Free;
  inherited;
end;

function TcxGridHeaderViewInfo.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := GridView.VisibleColumns[Index];
end;

function TcxGridHeaderViewInfo.GetColumnCount: Integer;
begin
  Result := GridView.VisibleColumnCount;
end;

procedure TcxGridHeaderViewInfo.AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo;
  ATopBound: Integer);
begin
  AIndicatorViewInfo.AddItem(ATopBound, Height, TcxGridIndicatorHeaderItemViewInfo);
end;

procedure TcxGridHeaderViewInfo.CalculateColumnAutoWidths;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(Count);
  try
    for I := 0 to Count - 1 do
      Items[I].InitAutoWidthItem(AAutoWidthObject.AddItem);
    AAutoWidthObject.AvailableWidth := GridViewInfo.ClientWidth - IfThen(Count > 0, GridView.GetVerticalScrollBarAreaWidth);
    AAutoWidthObject.Calculate;
    for I := 0 to Count - 1 do
      Items[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

procedure TcxGridHeaderViewInfo.CalculateColumnWidths;
begin
  if CanCalculateAutoWidths then CalculateColumnAutoWidths;
end;

function TcxGridHeaderViewInfo.CalculateHeight: Integer;
begin
  Result := FSpecific.Height;
end;

procedure TcxGridHeaderViewInfo.CalculateInvisible;
begin
  if IsAlwaysVisibleForCalculation then
  begin
    CalculateVisible;
    Height := 0;
    Bounds := Rect(0, 0, 0, 0);
  end
  else
    inherited;
end;

function TcxGridHeaderViewInfo.CalculateItemHeight: Integer;
var
  I, AColumnHeight: Integer;
begin
  if IsHeightAssigned then
    Result := GridView.OptionsView.HeaderHeight
  else
  begin
    Result := 0;
    CalculateParams;
    for I := 0 to Count - 1 do
      if Items[I].Visible then
      begin
        AColumnHeight := Items[I].CalculateHeight;
        if AColumnHeight > Result then Result := AColumnHeight;
      end;
    if Result = 0 then
      Result := inherited CalculateItemHeight;
  end;
end;

procedure TcxGridHeaderViewInfo.CalculateItems;
var
  ALeftBound, ATopBound, I, AWidth: Integer;
  AItem: TcxGridColumnHeaderViewInfo;
begin
  with ItemsAreaBounds do
  begin
    ALeftBound := Left;
    ATopBound := Top;
  end;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    AWidth := AItem.CalculateWidth;
    AItem.Calculate(ALeftBound, ATopBound, AWidth, ItemHeight);
    Inc(ALeftBound, AWidth);
  end;
end;

procedure TcxGridHeaderViewInfo.CalculateVisible;
begin
  CalculateColumnWidths;
  inherited;
end;

function TcxGridHeaderViewInfo.CalculateWidth: Integer;
begin
  Result := GridViewInfo.RecordsViewInfo.RowWidth;
end;

function TcxGridHeaderViewInfo.CanCalculateAutoWidths: Boolean;
begin
  Result := GridView.OptionsView.ColumnAutoWidth;
end;

function TcxGridHeaderViewInfo.DrawColumnBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin
  Result := ColumnBackgroundBitmap <> nil;
  if Result then
    ACanvas.FillRect(ABounds, ColumnBackgroundBitmap);
end;

function TcxGridHeaderViewInfo.GetAlignment: TcxGridPartAlignment;
begin
  Result := gpaTop;
end;

function TcxGridHeaderViewInfo.GetAutoHeight: Boolean;
begin
  Result := GridViewInfo.SupportsAutoHeight and GridView.OptionsView.HeaderAutoHeight;
end;

function TcxGridHeaderViewInfo.GetColumnBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbHeader);
end;

function TcxGridHeaderViewInfo.GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors;
begin
  Result := [];
  if not AColumn.IsLeft or GridViewInfo.HasFirstBorderOverlap then
    Include(Result, nLeft);
  if not AColumn.IsRight then
    Include(Result, nRight);
end;

function TcxGridHeaderViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridHeaderHitTest;
end;

function TcxGridHeaderViewInfo.GetIsAutoWidth: Boolean;
begin
  Result := False;
end;

function TcxGridHeaderViewInfo.GetIsScrollable: Boolean;
begin
  Result := True;
end;

function TcxGridHeaderViewInfo.GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean;
begin
  Result := inherited GetItemMultiLinePainting(AItem) or IsHeightAssigned;
end;

function TcxGridHeaderViewInfo.GetKind: TcxGridColumnContainerKind;
begin
  Result := ckHeader;
end;

function TcxGridHeaderViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridHeaderPainter;
end;

procedure TcxGridHeaderViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetHeaderParams(nil, AParams);
end;

function TcxGridHeaderViewInfo.GetVisible: Boolean;
begin
  Result := GridView.OptionsView.Header;
end;

function TcxGridHeaderViewInfo.GetWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].CalculateWidth);
end;

function TcxGridHeaderViewInfo.GetZonesAreaBounds: TRect;
begin
  Result := inherited GetZonesAreaBounds;
  Result.Left := GridViewInfo.ClientBounds.Left;
  InflateRect(Result, 0, ColumnHeaderMovingZoneSize);
end;

function TcxGridHeaderViewInfo.IsAlwaysVisibleForCalculation: Boolean;
begin
  Result := True;
end;

function TcxGridHeaderViewInfo.IsHeightAssigned: Boolean;
begin
  Result := GridView.OptionsView.HeaderHeight <> 0;
end;

procedure TcxGridHeaderViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  RecalculateItemVisibles;
end;

procedure TcxGridHeaderViewInfo.RecalculateItemVisibles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      CalculateVisible(Bounds.Left, Bounds.Right - Bounds.Left);
end;

procedure TcxGridHeaderViewInfo.AssignColumnWidths;
var
  I: Integer;
begin
  GridView.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      with Items[I] do
        Column.Width := OriginalWidth;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridHeaderViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  CalculateItems;
end;

procedure TcxGridHeaderViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

{ TcxGridGroupByBoxColumnHeaderViewInfo }

function TcxGridGroupByBoxColumnHeaderViewInfo.GetContainer: TcxGridGroupByBoxViewInfo;
begin
  Result := TcxGridGroupByBoxViewInfo(inherited Container);
end;

function TcxGridGroupByBoxColumnHeaderViewInfo.CalculateHeight: Integer;
begin
  Result := TcxGridGroupByBoxViewInfo(Container).ItemHeight;
end;

function TcxGridGroupByBoxColumnHeaderViewInfo.GetCaption: string;
begin
  Result := Column.GetAlternateCaption;
end;

function TcxGridGroupByBoxColumnHeaderViewInfo.HasFixedContentSpace: Boolean;
begin
  Result := Container.CalculatingColumnWidth;
end;

function TcxGridGroupByBoxColumnHeaderViewInfo.InheritedCalculateHeight: Integer;
begin
  Result := inherited CalculateHeight;
end;

{ TcxGridGroupByBoxViewInfo }

function TcxGridGroupByBoxViewInfo.GetGroupByBoxVerOffset: Integer;
begin
  Result := ItemHeight div 2;
end;

function TcxGridGroupByBoxViewInfo.GetLinkLineBounds(Index: Integer;
  Horizontal: Boolean): TRect;
begin
  Result := cxEmptyRect;
  if (Index + 1 < Count) and Items[Index + 1].Column.IsChildInMergedGroup then
    Exit;
  Result := Items[Index].Bounds;
  if IsSingleLine then
  begin
    Result.Left := Result.Right;
    Result.Right := Result.Left + GroupByBoxHorOffset;
    Result.Top := Result.Top + GetGroupByBoxVerOffset;
    Result.Bottom := Result.Top + GroupByBoxLineWidth;
  end
  else
  begin
    Result.Left := Result.Right - 2 * GroupByBoxHorOffset;
    Result.Top := Result.Bottom;
    Inc(Result.Bottom, GroupByBoxLineVerOffset);
    if Horizontal then
    begin
      Result.Top := Result.Bottom - GroupByBoxLineWidth;
      Inc(Result.Right, GroupByBoxHorOffset);
    end
    else
      Result.Right := Result.Left + GroupByBoxLineWidth;
  end;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Items[Index].Bounds);
end;

function TcxGridGroupByBoxViewInfo.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := GridView.GroupedColumns[Index];
end;

function TcxGridGroupByBoxViewInfo.GetColumnCount: Integer;
begin
  Result := GridView.GroupedColumnCount;
end;

function TcxGridGroupByBoxViewInfo.GetItemClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridGroupByBoxColumnHeaderViewInfo;
end;

function TcxGridGroupByBoxViewInfo.CalculateHeight: Integer;

  function TextHeight: Integer;
  begin
    CalculateParams;
    Result := GridViewInfo.GetFontHeight(Params.Font);
    GetCellTextAreaSize(Result, ScaleFactor);
    Inc(Result, 2);
  end;

begin
  Result := GridView.DataController.Groups.LevelCount;
  if Result = 0 then
    Result := 2 * GroupByBoxTopOffset + TextHeight
  else
  begin
    if IsSingleLine then
      Result := 1;
    Result := 2 * GroupByBoxTopOffset +
      ItemHeight div 2 * (Result + 1) + Byte(Odd(ItemHeight));
  end;
end;

function TcxGridGroupByBoxViewInfo.CalculateItemHeight: Integer;
var
  I, AColumnHeight: Integer;
begin
  Result := 0;
  CalculateParams;
  for I := 0 to Count - 1 do
  begin
    AColumnHeight := TcxGridGroupByBoxColumnHeaderViewInfo(Items[I]).InheritedCalculateHeight;
    if AColumnHeight > Result then Result := AColumnHeight;
  end;
  if Result = 0 then
    Result := inherited CalculateItemHeight;
end;

function TcxGridGroupByBoxViewInfo.CalculateWidth: Integer;
begin
  Result := GridViewInfo.ClientWidth;
end;

function TcxGridGroupByBoxViewInfo.GetAlignment: TcxGridPartAlignment;
begin
  Result := gpaTop;
end;

function TcxGridGroupByBoxViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxGridGroupByBoxViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbGroupByBox);
end;

function TcxGridGroupByBoxViewInfo.GetColumnWidth(AColumn: TcxGridColumn): Integer;
begin
  FCalculatingColumnWidth := True;
  Result := Items[AColumn.GroupIndex].GetBestFitWidth;
  FCalculatingColumnWidth := False;
end;

function TcxGridGroupByBoxViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridGroupByBoxHitTest;
end;

function TcxGridGroupByBoxViewInfo.GetIsAutoWidth: Boolean;
begin
  Result := True;
end;

function TcxGridGroupByBoxViewInfo.GetIsScrollable: Boolean;
begin
  Result := False;
end;

function TcxGridGroupByBoxViewInfo.GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect;
begin
  SetRectEmpty(Result);
end;

function TcxGridGroupByBoxViewInfo.GetKind: TcxGridColumnContainerKind;
begin
  Result := ckGroupByBox;
end;

function TcxGridGroupByBoxViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridGroupByBoxPainter;
end;

function TcxGridGroupByBoxViewInfo.GetText: string;
begin
  if Count = 0 then
    Result := cxGetResourceString(@scxGridGroupByBoxCaption)
  else
    Result := '';
end;

function TcxGridGroupByBoxViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  Inc(Result.Left, GroupByBoxLeftOffset);
end;

procedure TcxGridGroupByBoxViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsGroupByBox, nil, nil, AParams);
end;

function TcxGridGroupByBoxViewInfo.GetVisible: Boolean;
begin
  Result := GridView.OptionsView.GroupByBox;
end;

function TcxGridGroupByBoxViewInfo.IsSingleLine: Boolean;
begin
  Result := GridView.OptionsView.GroupByHeaderLayout = ghlHorizontal;
end;

procedure TcxGridGroupByBoxViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
var
  I, ALevel: Integer;
  AColumnHeaderViewInfo: TcxGridColumnHeaderViewInfo;
  ASummaryItems: TcxDataGroupSummaryItems;
begin
  inherited;
  with Bounds do
  begin
    ALeftBound := Left + GroupByBoxLeftOffset;
    ATopBound := Top + GroupByBoxTopOffset;
  end;
  for I := 0 to Count - 1 do
  begin
    AColumnHeaderViewInfo := Items[I];

    if not AColumnHeaderViewInfo.Column.IsChildInMergedGroup then
    begin
      ALevel := GridView.DataController.Groups.GetLevelByItemGroupIndex(AColumnHeaderViewInfo.Column.GroupIndex);
      ASummaryItems := GridView.DataController.Summary.GroupSummaryItems[ALevel];
      AColumnHeaderViewInfo.FSortByGroupSummary := ASummaryItems.SortedSummaryItem <> nil;
    end;

    AColumnHeaderViewInfo.Calculate(ALeftBound, ATopBound);
    if I = Count - 1 then
      Break;

    Inc(ALeftBound, AColumnHeaderViewInfo.Width);
    if Items[I + 1].Column.IsChildInMergedGroup then
      Dec(ALeftBound, 1)
    else
    begin
      Inc(ALeftBound, GroupByBoxHorOffset);
      if not IsSingleLine then
        Inc(ATopBound, GroupByBoxVerOffset);
    end;
  end;
end;

procedure TcxGridGroupByBoxViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

{ TcxGridFooterCellData }

constructor TcxGridFooterCellData.Create(ASummaryItem: TcxDataSummaryItem);
begin
  inherited Create;
  FSummaryItem := ASummaryItem;
end;

{ TcxGridFooterCellViewInfo }

constructor TcxGridFooterCellViewInfo.Create(AContainer: TcxGridColumnContainerViewInfo; AData: TcxGridFooterCellData);
begin
  FData := AData;
  inherited Create(AContainer, TcxGridColumn(SummaryItem.ItemLink));
end;

function TcxGridFooterCellViewInfo.GetContainer: TcxGridFooterViewInfo;
begin
  Result := TcxGridFooterViewInfo(inherited Container);
end;

function TcxGridFooterCellViewInfo.GetSummary: TcxDataSummary;
begin
  Result := SummaryItem.SummaryItems.Summary;
end;

function TcxGridFooterCellViewInfo.GetSummaryItem: TcxDataSummaryItem;
begin
  Result := Data.SummaryItem;
end;

procedure TcxGridFooterCellViewInfo.AfterCalculateBounds(var ABounds: TRect);
begin
  inherited;
  with LookAndFeelPainter do
    InflateRect(ABounds, -FooterCellOffset, -FooterCellOffset);
end;

function TcxGridFooterCellViewInfo.CanPress: Boolean;
begin
  Result := False;
end;

function TcxGridFooterCellViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
  Column.DoCustomDrawFooterCell(ACanvas, Self, Result);
  if not Result then
    GridView.DoCustomDrawFooterCell(ACanvas, Self, Result);
end;

function TcxGridFooterCellViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := Column.FooterAlignmentHorz;
end;

function TcxGridFooterCellViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := Container.BackgroundBitmap;
end;

procedure TcxGridFooterCellViewInfo.GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc);
begin
end;

function TcxGridFooterCellViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

class function TcxGridFooterCellViewInfo.GetCellBorderWidth(ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer;
begin
  Result := ALookAndFeelPainter.FooterCellBorderSize;
end;

function TcxGridFooterCellViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := Container.GetItemHitTestClass;
end;

function TcxGridFooterCellViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(SummaryItem);
end;

function TcxGridFooterCellViewInfo.GetIsPressed: Boolean;
begin
  Result := False;
end;

function TcxGridFooterCellViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridFooterCellPainter;
end;

function TcxGridFooterCellViewInfo.GetText: string;
begin
  try
    Result := Summary.FooterSummaryTexts[SummaryItem.Index];
  except
    Application.HandleException(Self);
  end;
end;

procedure TcxGridFooterCellViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetFooterCellParams(nil, Column, -1, SummaryItem, AParams);
end;

function TcxGridFooterCellViewInfo.HasCustomDraw: Boolean;
begin
  Result := Column.HasCustomDrawFooterCell or GridView.HasCustomDrawFooterCell;
end;

function TcxGridFooterCellViewInfo.HasCustomHint: Boolean;
begin
  Result := False;
end;

procedure TcxGridFooterCellViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  (AHitTest as TcxGridFooterCellHitTest).SummaryItem := SummaryItem;
end;

procedure TcxGridFooterCellViewInfo.PopulateDesignPopupMenu(AMenu: TPopupMenu);
begin
end;

function TcxGridFooterCellViewInfo.GetBestFitWidth: Integer;
begin
  Result := inherited GetBestFitWidth + 2 * LookAndFeelPainter.FooterCellOffset;
end;

function TcxGridFooterCellViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if GridView.IsDesigning and (AButton = mbLeft) then
  begin
    GridView.Controller.DesignController.SelectObject(SummaryItem, not (ssShift in AShift));
    Result := True;
  end;
end;

{ TcxGridFooterViewInfo }

function TcxGridFooterViewInfo.GetCellData(AIndex: Integer): TcxGridFooterCellData;
begin
  Result := TcxGridFooterCellData(FCellDataList[AIndex]);
end;

function TcxGridFooterViewInfo.GetMultipleSummaries: Boolean;
begin
  Result := RowCount > 1;
end;

function TcxGridFooterViewInfo.GetRowCount: Integer;
begin
  if FRowCount = 0 then
    FRowCount := CalculateRowCount;
  Result := FRowCount;
end;

function TcxGridFooterViewInfo.GetRowHeight: Integer;
begin
  if MultipleSummaries then
    Result := ItemHeight
  else
    Result := inherited CalculateHeight;
end;

procedure TcxGridFooterViewInfo.AddCellData(ASummaryItem: TcxDataSummaryItem);
begin
  FCellDataList.Add(CreateCellData(ASummaryItem));
end;

procedure TcxGridFooterViewInfo.AddAdornerTargetElementForColumn(AList: TStrings; AColumn: TcxCustomGridColumn; AName: string);
var
  I, AIndex: Integer;
  AFooterItem: TcxGridFooterCellViewInfo;
begin
  AIndex := 1;
  for I := 0 to Count - 1 do
  begin
    AFooterItem := TcxGridFooterCellViewInfo(Items[I]);
    if AFooterItem.Column = AColumn then
    begin
      AList.AddObject(AName + IntToStr(AIndex), AFooterItem);
      Inc(AIndex);
    end;
  end;
end;

procedure TcxGridFooterViewInfo.CreateCellDataList;
var
  AColumnHasSummaries: array of Boolean;
begin
  FCellDataList := TdxFastObjectList.Create;
  SetLength(AColumnHasSummaries, GridView.VisibleColumnCount);
  PopulateCellDataList(AColumnHasSummaries);
  Prepare(FCellDataList);
end;

function TcxGridFooterViewInfo.CreateItem(AIndex: Integer): TcxGridColumnHeaderViewInfo;
begin
  Result := TcxGridFooterCellViewInfoClass(GetItemClass).Create(Self, CellDataList[AIndex]);
end;

procedure TcxGridFooterViewInfo.CreateItems;
begin
  CreateCellDataList;
  inherited CreateItems;
end;

function TcxGridFooterViewInfo.CreateCellData(ASummaryItem: TcxDataSummaryItem): TcxGridFooterCellData;
begin
  Result := TcxGridFooterCellData.Create(ASummaryItem);
end;

procedure TcxGridFooterViewInfo.DestroyItems;
begin
  inherited;
  FreeAndNil(FCellDataList);
end;

function TcxGridFooterViewInfo.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(CellDataList[Index].SummaryItem.ItemLink);
end;

function TcxGridFooterViewInfo.GetColumnCount: Integer;
begin
  Result := FCellDataList.Count;
end;

function TcxGridFooterViewInfo.GetItemClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridFooterCellViewInfo;
end;

procedure TcxGridFooterViewInfo.PopulateCellDataList(var AColumnHasSummaries: array of Boolean);
begin
  PopulateCellDataList(GridView.DataController.Summary.FooterSummaryItems, AColumnHasSummaries);
end;

procedure TcxGridFooterViewInfo.PopulateCellDataList(ASummaryItems: TcxDataSummaryItems;
  var AColumnHasSummaries: array of Boolean);
var
  I, AColumnVisibleIndex: Integer;
  ASummaryItem: TcxDataSummaryItem;
begin
  for I := 0 to ASummaryItems.Count - 1 do
  begin
    ASummaryItem := ASummaryItems[I];
    if (ASummaryItem.Position = spFooter) and (ASummaryItem.ItemLink is TcxGridColumn) then
    begin
      AColumnVisibleIndex := TcxGridColumn(ASummaryItem.ItemLink).VisibleIndex;
      if (AColumnVisibleIndex <> -1) and
        (CanShowMultipleSummaries or not AColumnHasSummaries[AColumnVisibleIndex]) then
      begin
        AddCellData(ASummaryItem);
        AColumnHasSummaries[AColumnVisibleIndex] := True;
      end;
    end;
  end;
end;

procedure TcxGridFooterViewInfo.Prepare(ACellDataList: TdxFastObjectList);
begin
end;

function TcxGridFooterViewInfo.CalculateBounds: TRect;
begin
  Result := inherited CalculateBounds;
  with GridViewInfo.HeaderViewInfo.CalculateBounds do
  begin
    Result.Left := Left;
    Result.Right := Right;
  end;
  if (GridViewInfo.IndicatorViewInfo.Width > 0) and (Result.Left = Result.Right) then
    Result.Right := Result.Left + GridViewInfo.ClientWidth;
end;

function TcxGridFooterViewInfo.CalculateHeight: Integer;
begin
  CalculateParams;
  Result := BorderSize[bTop] + RowCount * RowHeight + BorderSize[bBottom];
  Inc(Result, SeparatorWidth);
end;

function TcxGridFooterViewInfo.CalculateItemHeight: Integer;
begin
  Result := inherited CalculateItemHeight + 2 * LookAndFeelPainter.FooterCellOffset;
end;

procedure TcxGridFooterViewInfo.CalculateItem(AIndex: Integer);
begin
  Items[AIndex].Calculate(GetItemLeftBound(AIndex), GetItemTopBound(AIndex),
    -1, GetItemHeight(AIndex));
  if GridViewInfo.IsRightToLeftConverted then
    Items[AIndex].RightToLeftConversion(Items[AIndex].Bounds);
end;

procedure TcxGridFooterViewInfo.CalculateItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if IsItemVisible(I) then CalculateItem(I);
end;

function TcxGridFooterViewInfo.CalculateRowCount: Integer;
var
  I: Integer;
  ItemCount: array of Integer;
begin
  Result := 1;
  if Count = 0 then Exit;
  SetLength(ItemCount, GridView.VisibleItemCount);
  for I := 0 to Count - 1 do
    Inc(ItemCount[Items[I].Column.VisibleIndex]);
  for I := 0 to Length(ItemCount) - 1 do
    Result := Max(Result, ItemCount[I]);
end;

function TcxGridFooterViewInfo.CanCalculateAutoWidths: Boolean;
begin
  Result := False;
end;

function TcxGridFooterViewInfo.GetAlignment: TcxGridPartAlignment;
begin
  Result := gpaBottom;
end;

function TcxGridFooterViewInfo.GetAutoHeight: Boolean;
begin
  Result := GridViewInfo.SupportsAutoHeight and
    GridView.OptionsView.FooterAutoHeight and not MultipleSummaries;
end;

function TcxGridFooterViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbFooter);
end;

function TcxGridFooterViewInfo.GetBordersBounds: TRect;
begin
  Result := Bounds;
  Inc(Result.Top, SeparatorWidth);
end;

function TcxGridFooterViewInfo.GetBorders: TcxBorders;
begin
  Result := LookAndFeelPainter.FooterBorders;
  if not GridViewInfo.HasFirstBorderOverlap then
    Include(Result, bLeft);
end;

function TcxGridFooterViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.FooterBorderSize;
end;

function TcxGridFooterViewInfo.GetColumnWidth(AColumn: TcxGridColumn): Integer;
begin
  Result := GridViewInfo.GetColumnFooterWidth(Self, AColumn);
end;

function TcxGridFooterViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFooterHitTest;
end;

function TcxGridFooterViewInfo.GetIsAutoWidth: Boolean;
begin
  Result := GridViewInfo.HeaderViewInfo.IsAutoWidth;
end;

function TcxGridFooterViewInfo.GetIsScrollable: Boolean;
begin
  Result := GridViewInfo.HeaderViewInfo.IsScrollable;
end;

function TcxGridFooterViewInfo.GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect;
begin
  Result := GridViewInfo.HeaderViewInfo.GetItemAreaBounds(AItem);
end;

function TcxGridFooterViewInfo.GetItemHeight(AColumn: TcxGridColumn): Integer;
begin
  if MultipleSummaries then
    Result := ItemHeight
  else
    Result := GridViewInfo.GetCellHeight(AColumn.VisibleIndex, ItemHeight);
end;

function TcxGridFooterViewInfo.GetItemHeight(AIndex: Integer): Integer;
begin
  Result := GetItemHeight(Items[AIndex].Column);
end;

function TcxGridFooterViewInfo.GetItemHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridFooterCellHitTest;
end;

function TcxGridFooterViewInfo.GetItemLeftBound(AColumn: TcxGridColumn): Integer;
begin
  if AColumn.IsMostLeft and not GridViewInfo.IsRightToLeftConverted  then
    Result := ItemsAreaBounds.Left
  else
  begin
    Result := GridViewInfo.HeaderViewInfo[AColumn.VisibleIndex].RealBounds.Left;
    if AColumn.IsLeft and not IsRightToLeftConverted then
      Inc(Result, GridViewInfo.BorderOverlapSize);
  end;
end;

function TcxGridFooterViewInfo.GetItemLeftBound(AIndex: Integer): Integer;
begin
  Result := GetItemLeftBound(Items[AIndex].Column);
end;

function TcxGridFooterViewInfo.GetItemRowIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AIndex - 1 do
    if Items[I].Column = Items[AIndex].Column then
      Inc(Result);
end;

function TcxGridFooterViewInfo.GetItemsAreaBounds: TRect;
begin
  Result := BordersBounds;
  with Result do
  begin
    Inc(Left, BorderSize[bLeft]);
    Inc(Top, BorderSize[bTop]);
    Dec(Right, BorderSize[bRight]);
    Dec(Bottom, BorderSize[bBottom]);
  end;
end;

function TcxGridFooterViewInfo.GetItemTopBound(AColumn: TcxGridColumn): Integer;
begin
  Result := ItemsAreaBounds.Top + GridViewInfo.GetCellTopOffset(AColumn.VisibleIndex, ItemHeight);
end;

function TcxGridFooterViewInfo.GetItemTopBound(AIndex: Integer): Integer;
begin
  Result := GetItemTopBound(Items[AIndex].Column) + GetItemRowIndex(AIndex) * RowHeight;
end;

function TcxGridFooterViewInfo.GetKind: TcxGridColumnContainerKind;
begin
  Result := ckFooter;
end;

function TcxGridFooterViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := GridViewInfo.GetFooterPainterClass;
end;

function TcxGridFooterViewInfo.GetSeparatorBounds: TRect;
begin
  Result := Bounds;
  Result.Bottom := Result.Top + SeparatorWidth;
end;

function TcxGridFooterViewInfo.GetSeparatorWidth: Integer;
begin
  if HasSeparator then
    Result := LookAndFeelPainter.FooterSeparatorSize
  else
    Result := 0;
end;

function TcxGridFooterViewInfo.GetSummaryItems: TcxDataSummaryItems;
begin
  Result := GridView.FDataController.Summary.FooterSummaryItems;
end;

procedure TcxGridFooterViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetFooterParams(nil, nil, -1, nil, AParams);
end;

function TcxGridFooterViewInfo.GetVisible: Boolean;
begin
  Result := GridView.OptionsView.Footer;
end;

function TcxGridFooterViewInfo.HasSeparator: Boolean;
begin
  Result := True;
end;

function TcxGridFooterViewInfo.IsAlwaysVisibleForCalculation: Boolean;
begin
  Result := False;
end;

function TcxGridFooterViewInfo.IsColumnOnFirstLayer(AColumnIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxGridFooterViewInfo.IsHeightAssigned: Boolean;
begin
  Result := False;
end;

function TcxGridFooterViewInfo.IsItemVisible(AIndex: Integer): Boolean;
begin
  Result := GridViewInfo.HeaderViewInfo[Items[AIndex].Column.VisibleIndex].Visible;
end;

function TcxGridFooterViewInfo.IsMultilayerLayout: Boolean;
begin
  Result := False;
end;

procedure TcxGridFooterViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  if DX <> 0 then
    for I := 0 to Count - 1 do
      if IsItemVisible(I) then
        if not Items[I].Calculated then
          CalculateItem(I)
        else
      else
        Items[I].Calculated := False;
end;

function TcxGridFooterViewInfo.CanShowMultipleSummaries: Boolean;
begin
  Result := GridView.OptionsView.CanShowFooterMultiSummaries;
end;

function TcxGridFooterViewInfo.GetCellBestFitWidth(AColumn: TcxGridColumn): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Column = AColumn then
      Result := Max(Result, Items[I].GetBestFitWidth);
  if AColumn.IsMostLeft then
    Inc(Result, BorderSize[bLeft]);
  if AColumn.IsMostRight then
    Inc(Result, BorderSize[bRight]);
end;

function TcxGridFooterViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;

  function GetCellBounds(AColumn: TcxGridColumn): TRect;
  begin
    Result.Left := GetItemLeftBound(AColumn);
    Result.Right := Result.Left + GetColumnWidth(AColumn);
    if AColumn.IsLeft and IsRightToLeftConverted then
      Dec(Result.Right, GridViewInfo.BorderOverlapSize);
    if MultipleSummaries then
    begin
      Result.Top := ItemsAreaBounds.Top;
      Result.Bottom := ItemsAreaBounds.Bottom;
    end
    else
    begin
      Result.Top := GetItemTopBound(AColumn);
      Result.Bottom := Result.Top + GetItemHeight(AColumn);
    end;
  end;

var
  I: Integer;
  AColumn: TcxGridColumn;
  ColumnNotFound: Boolean;
begin
  Result := GetItemsHitTest(P);
  ColumnNotFound := True;
  if Result = nil then
  begin
    Result := inherited GetHitTest(P);
    if Result <> nil then
      for I := 0 to GridView.VisibleColumnCount - 1 do
      begin
        AColumn := GridView.VisibleColumns[I];
        if PtInRect(GetCellBounds(AColumn), P) and
          (ColumnNotFound or IsColumnOnFirstLayer(I)) then
        begin
          Result := GetItemHitTestClass.Instance(P);
          InitHitTest(Result);
          TcxGridFooterCellHitTest(Result).Column := AColumn;
          TcxGridFooterCellHitTest(Result).SummaryItem := nil;
          ColumnNotFound := False;
          if not IsMultilayerLayout then
            Break;
        end;
      end;
  end;
end;

{ TcxCustomGridIndicatorItemViewInfo }

constructor TcxCustomGridIndicatorItemViewInfo.Create(AContainer: TcxGridIndicatorViewInfo);
begin
  inherited Create(AContainer.GridViewInfo);
  FContainer := AContainer;
  if ShowCheckBox then
    FCheckBox := CreateCheckBox;
end;

destructor TcxCustomGridIndicatorItemViewInfo.Destroy;
begin
  FreeAndNil(FCheckBox);
  FContainer.FItems.Remove(Self);
  inherited;
end;

procedure TcxCustomGridIndicatorItemViewInfo.Calculate(ALeftBound: Integer; ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited Calculate(ALeftBound, ATopBound, AWidth, AHeight);
  if HasCheckBox then
    CheckBox.Calculate(GetCheckBoxBounds);
end;

function TcxCustomGridIndicatorItemViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetHitTest(P);
  if (Result = nil) or (Result.ClassType <> GetHitTestClass) then
    Exit;
  if HasCheckBox then
  begin
    AHitTest := CheckBox.GetHitTest(P);
    if AHitTest <> nil then
      Result := AHitTest;
  end;
end;

function TcxCustomGridIndicatorItemViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridIndicatorItemViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxCustomGridIndicatorItemViewInfo.CalculateWidth: Integer;
begin
  Result := FContainer.Width;
end;

function TcxCustomGridIndicatorItemViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  if not Result then
    GridView.DoCustomDrawIndicatorCell(ACanvas, Self, Result);
end;

procedure TcxCustomGridIndicatorItemViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if HasCheckBox then
    CheckBox.RightToLeftConversion(ABounds);
end;

function TcxCustomGridIndicatorItemViewInfo.GetCheckBoxAreaBounds: TRect;
begin
  Result := Bounds;
  Result.Left := GetImageAreaBounds.Right;
end;

function TcxCustomGridIndicatorItemViewInfo.GetCheckBoxBounds: TRect;
begin
  Result := GetCheckBoxAreaBounds;
  Inc(Result.Left, Container.GetCheckBoxAreaMargins.Left);
  Dec(Result.Right, Container.GetCheckBoxAreaMargins.Right);
  Result := cxRectCenterHorizontally(Result, CheckBox.Width);
  Inc(Result.Top, Container.GetCheckBoxAreaMargins.Top);
  Dec(Result.Bottom, Container.GetCheckBoxAreaMargins.Bottom);
  Result := cxRectCenterVertically(Result, CheckBox.Height);
end;

function TcxCustomGridIndicatorItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridIndicatorHitTest;
end;

function TcxCustomGridIndicatorItemViewInfo.GetImageAreaBounds: TRect;
begin
  Result := cxRectSetWidth(Bounds, Container.GetIndicatorAreaWidth);
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxCustomGridIndicatorItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxCustomGridIndicatorItemPainter;
end;

procedure TcxCustomGridIndicatorItemViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetViewParams(vsIndicator, nil, nil, AParams);
end;

function TcxCustomGridIndicatorItemViewInfo.HasCheckBox: Boolean;
begin
  Result := CheckBox <> nil;
end;

function TcxCustomGridIndicatorItemViewInfo.HasCustomDraw: Boolean;
begin
  Result := GridView.HasCustomDrawIndicatorCell;
end;

procedure TcxCustomGridIndicatorItemViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited Offset(DX, DY);
  if HasCheckBox then
    CheckBox.DoOffset(DX, DY);
end;

function TcxCustomGridIndicatorItemViewInfo.ShowCheckBox: Boolean;
begin
  Result := Container.ShowCheckBoxes;
end;

{ TcxGridIndicatorHeaderItemViewInfo }

function TcxGridIndicatorHeaderItemViewInfo.GetDropDownWindowValue: TcxCustomGridCustomizationPopup;
begin
  Result := TcxCustomGridCustomizationPopup(inherited DropDownWindow);
end;

function TcxGridIndicatorHeaderItemViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridIndicatorHeaderItemViewInfo.CanShowHint: Boolean;
begin
  Result := SupportsQuickCustomization;
end;

function TcxGridIndicatorHeaderItemViewInfo.CreateCheckBox: TcxGridCheckBoxViewInfo;
begin
  Result := TcxGridHeaderCheckBoxViewInfo.Create(GridViewInfo);
end;

function TcxGridIndicatorHeaderItemViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := Bounds;
end;

function TcxGridIndicatorHeaderItemViewInfo.GetHintTextRect(const AMousePos: TPoint): TRect;
begin
  Result := Bounds;
  OffsetRect(Result, 0, Height + 5);
end;

function TcxGridIndicatorHeaderItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridIndicatorHeaderHitTest;
end;

function TcxGridIndicatorHeaderItemViewInfo.GetHotTrack: Boolean;
begin
  Result := SupportsQuickCustomization;
end;

function TcxGridIndicatorHeaderItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridIndicatorHeaderItemPainter;
end;

function TcxGridIndicatorHeaderItemViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@scxGridColumnsQuickCustomizationHint);
end;

procedure TcxGridIndicatorHeaderItemViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetHeaderParams(nil, AParams);
end;

function TcxGridIndicatorHeaderItemViewInfo.IsHintForText: Boolean;
begin
  Result := False;
end;

function TcxGridIndicatorHeaderItemViewInfo.IsHintMultiLine: Boolean;
begin
  Result := False;
end;

function TcxGridIndicatorHeaderItemViewInfo.ShowCheckBox: Boolean;
begin
  Result := inherited ShowCheckBox and (cbvColumnHeader in GridView.OptionsSelection.CheckBoxVisibility);
end;

function TcxGridIndicatorHeaderItemViewInfo.SupportsQuickCustomization: Boolean;
begin
  Result := GridView.OptionsCustomize.ColumnsQuickCustomization;
end;

function TcxGridIndicatorHeaderItemViewInfo.CloseDropDownWindowOnDestruction: Boolean;
begin
  Result := False;
end;

function TcxGridIndicatorHeaderItemViewInfo.DropDownWindowExists: Boolean;
begin
  Result := GridView.Controller.HasItemsCustomizationPopup;
end;

function TcxGridIndicatorHeaderItemViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := GridView.Controller.ItemsCustomizationPopup;
end;

{ TcxGridIndicatorRowItemViewInfo }

constructor TcxGridIndicatorRowItemViewInfo.Create(AContainer: TcxGridIndicatorViewInfo;
  ARowViewInfo: TcxCustomGridRowViewInfo);
begin
  FRowViewInfo := ARowViewInfo;
  inherited Create(AContainer);
end;

destructor TcxGridIndicatorRowItemViewInfo.Destroy;
begin
  FRowViewInfo.FIndicatorItem := nil;
  inherited;
end;

function TcxGridIndicatorRowItemViewInfo.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridIndicatorRowItemViewInfo.GetGridRecord: TcxCustomGridRow;
begin
  Result := FRowViewInfo.GridRecord;
end;

function TcxGridIndicatorRowItemViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridIndicatorRowItemViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridIndicatorRowItemViewInfo.CreateCheckBox: TcxGridCheckBoxViewInfo;
begin
  Result := TcxGridRowCheckBoxViewInfo.Create(RowViewInfo);
end;

function TcxGridIndicatorRowItemViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbIndicator);
end;

function TcxGridIndicatorRowItemViewInfo.GetCheckBoxAreaBounds: TRect;
begin
  Result := inherited GetCheckBoxAreaBounds;
  Result.Top := RowViewInfo.ContentBounds.Top;
  Result.Bottom := RowViewInfo.ContentBounds.Bottom;
end;

function TcxGridIndicatorRowItemViewInfo.GetIndicatorKind: TcxIndicatorKind;
begin
  Result := GridRecord.IndicatorKind;
end;

function TcxGridIndicatorRowItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridRowIndicatorHitTest;
end;

function TcxGridIndicatorRowItemViewInfo.GetNeighbors: TcxNeighbors;
begin
  Result := [nTop, nBottom];
  if (GridRecord.FixedState = rfsFixedToBottom) and (GridRecord.Index = GridView.Controller.LastScrollRecordIndex + 1) then
    Exclude(Result, nTop);
end;

function TcxGridIndicatorRowItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridIndicatorRowItemPainter;
end;

function TcxGridIndicatorRowItemViewInfo.GetRowSizingEdgeBounds: TRect;
begin
  Result := Bounds;
  with Result do
  begin
    Top := Bottom - ScaleFactor.Apply(cxGridRowSizingEdgeSize) div 2;
    Inc(Bottom, ScaleFactor.Apply(cxGridRowSizingEdgeSize) div 2);
  end;
end;

procedure TcxGridIndicatorRowItemViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridRecordHitTest(AHitTest).GridRecord := GridRecord;
  if AHitTest is TcxGridRowIndicatorHitTest then
  begin
    AHitTest.ViewInfo := GridRecord.ViewInfo;
    TcxGridRowIndicatorHitTest(AHitTest).MultiSelect := GridView.Controller.MultiSelect;
  end;
end;

function TcxGridIndicatorRowItemViewInfo.ShowCheckBox: Boolean;
begin
  Result := inherited ShowCheckBox and GridRecord.ShowCheckBox;
end;

function TcxGridIndicatorRowItemViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if RowViewInfo.CanSize and PtInRect(RowSizingEdgeBounds, P) then
  begin
    Result := TcxGridRowSizingEdgeHitTest.Instance(P);
    InitHitTest(Result);
  end
  else
    Result := inherited GetHitTest(P);
end;

function TcxGridIndicatorRowItemViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if (AButton = mbLeft) and (ssDouble in AShift) and
    (AHitTest.HitTestCode = htRowSizingEdge) then
    RowViewInfo.RowHeight := 0;
end;

{ TcxGridIndicatorFooterItemViewInfo }

function TcxGridIndicatorFooterItemViewInfo.GetSeparatorWidth: Integer;
begin
  Result := GridViewInfo.FooterViewInfo.SeparatorWidth;
end;

function TcxGridIndicatorFooterItemViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridIndicatorFooterItemViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridViewInfo.FooterViewInfo.BackgroundBitmap;
end;

function TcxGridIndicatorFooterItemViewInfo.GetBorders: TcxBorders;
begin
  Result := LookAndFeelPainter.FooterBorders;
  Include(Result, bLeft);
  Exclude(Result, bRight);
end;

function TcxGridIndicatorFooterItemViewInfo.GetBordersBounds: TRect;
begin
  Result := Bounds;
  Inc(Result.Top, SeparatorWidth);
end;

function TcxGridIndicatorFooterItemViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.FooterBorderSize;
  if AIndex = bTop then Inc(Result, SeparatorWidth);
end;

function TcxGridIndicatorFooterItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridIndicatorFooterItemPainter;
end;

function TcxGridIndicatorFooterItemViewInfo.GetSeparatorBounds: TRect;
begin
  Result := Bounds;
  Result.Bottom := Result.Top + SeparatorWidth;
end;

function TcxGridIndicatorFooterItemViewInfo.HasSeparator: Boolean;
begin
  Result := GridViewInfo.FooterViewInfo.HasSeparator;
end;

function TcxGridIndicatorFooterItemViewInfo.ShowCheckBox: Boolean;
begin
  Result := False;
end;

{ TcxGridIndicatorViewInfo }

constructor TcxGridIndicatorViewInfo.Create(AGridViewInfo: TcxGridTableViewInfo);
begin
  inherited Create(AGridViewInfo);
  FItems := TList.Create;
end;

destructor TcxGridIndicatorViewInfo.Destroy;
begin
  DestroyItems;
  FItems.Free;
  inherited;
end;

function TcxGridIndicatorViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridIndicatorViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridIndicatorViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxGridIndicatorViewInfo.GetItem(Index: Integer): TcxCustomGridIndicatorItemViewInfo;
begin
  Result := TcxCustomGridIndicatorItemViewInfo(FItems[Index]);
end;

procedure TcxGridIndicatorViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do Items[I].Free;
end;

function TcxGridIndicatorViewInfo.CalculateHeight: Integer;
begin
  with GridViewInfo do
    Result := HeaderViewInfo.Height + ClientHeight + FooterViewInfo.Height;
end;

function TcxGridIndicatorViewInfo.CalculateWidth: Integer;
begin
  if Visible then
  begin
    Result := GetIndicatorAreaWidth;
    if ShowCheckBoxes then
      Inc(Result, GetCheckBoxAreaWidth);
  end
  else
    Result := 0;
end;

function TcxGridIndicatorViewInfo.GetAlwaysVisible: Boolean;
begin
  Result := GridView.OptionsCustomize.ColumnsQuickCustomization;
end;

function TcxGridIndicatorViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := GridViewInfo.BackgroundBitmap;
end;

function TcxGridIndicatorViewInfo.GetCheckBoxAreaWidth: Integer;
begin
  Result := LookAndFeelPainter.ScaledCheckButtonSize(ScaleFactor).cx;
  dxAdjustToTouchableSize(Result, ScaleFactor);
  Result := cxMarginsWidth(GetCheckBoxAreaMargins) + Result;
end;

function TcxGridIndicatorViewInfo.GetCheckBoxAreaMargins: TRect;
begin
  Result := Rect(0, 0, ScaleFactor.Apply(4), 0);
end;

function TcxGridIndicatorViewInfo.GetIndicatorAreaWidth: Integer;
begin
  Result := GridView.OptionsView.IndicatorWidth;
end;

function TcxGridIndicatorViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridIndicatorHitTest;
end;

function TcxGridIndicatorViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridIndicatorPainter;
end;

function TcxGridIndicatorViewInfo.GetRowItemClass(ARowViewInfo: TcxCustomGridRowViewInfo): TcxGridIndicatorRowItemViewInfoClass;
begin
  Result := TcxGridIndicatorRowItemViewInfo;
end;

procedure TcxGridIndicatorViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  AParams.Color := GridViewInfo.BackgroundColor;
end;

function TcxGridIndicatorViewInfo.GetVisible: Boolean;
begin
  Result := GridView.OptionsView.Indicator or AlwaysVisible;
end;

function TcxGridIndicatorViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxGridIndicatorViewInfo.ShowCheckBoxes: Boolean;
begin
  Result := (GridView.OptionsSelection.CheckBoxPosition = cbpIndicator) and
    (GridView.OptionsSelection.CheckBoxVisibility <> []);
end;

function TcxGridIndicatorViewInfo.AddItem(AItemClass: TcxCustomGridIndicatorItemViewInfoClass): TcxCustomGridIndicatorItemViewInfo;
begin
  Result := AItemClass.Create(Self);
  FItems.Add(Result);
end;

function TcxGridIndicatorViewInfo.AddItem(ATopBound, AHeight: Integer;
  AItemClass: TcxCustomGridIndicatorItemViewInfoClass): TcxCustomGridIndicatorItemViewInfo;
begin
  Result := AddItem(AItemClass);
  Result.Calculate(Bounds.Left, ATopBound, Width, AHeight);
end;

function TcxGridIndicatorViewInfo.AddRowItem(ARowViewInfo: TcxCustomGridRowViewInfo): TcxCustomGridIndicatorItemViewInfo;
begin
  Result := GetRowItemClass(ARowViewInfo).Create(Self, ARowViewInfo);
  FItems.Add(Result);
end;

procedure TcxGridIndicatorViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  if GridViewInfo.HeaderViewInfo.Visible then
    GridViewInfo.HeaderViewInfo.AddIndicatorItems(Self, Bounds.Top);
  if GridViewInfo.FooterViewInfo.Visible then
    AddItem(Bounds.Bottom - GridViewInfo.FooterViewInfo.Height,
      GridViewInfo.FooterViewInfo.Height, TcxGridIndicatorFooterItemViewInfo);
end;

procedure TcxGridIndicatorViewInfo.CalculateRowItem(ARowViewInfo: TcxCustomGridRowViewInfo;
  AItem: TcxCustomGridIndicatorItemViewInfo);
begin
  AItem.Calculate(Bounds.Left, ARowViewInfo.Bounds.Top, Width, ARowViewInfo.Height);
end;

procedure TcxGridIndicatorViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridIndicatorViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := inherited GetHitTest(P);
end;

function TcxGridIndicatorViewInfo.GetRowItemBounds(AGridRecord: TcxCustomGridRow): TRect;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I] is TcxGridIndicatorRowItemViewInfo) and
      (TcxGridIndicatorRowItemViewInfo(Items[I]).GridRecord = AGridRecord) then
    begin
      Result := Items[I].Bounds;
      Exit;
    end;
  Result := Rect(0, 0, 0, 0);
end;

{ TcxGridRowFooterCellData }

constructor TcxGridRowFooterCellData.Create(ASummaryItem: TcxDataSummaryItem; AGroupedColumnIndex: Integer);
begin
  inherited Create(ASummaryItem);
  FGroupedColumnIndex := AGroupedColumnIndex;
end;

{ TcxGridRowFooterCellViewInfo }

function TcxGridRowFooterCellViewInfo.GetContainer: TcxGridRowFooterViewInfo;
begin
  Result := TcxGridRowFooterViewInfo(inherited Container);
end;

function TcxGridRowFooterCellViewInfo.GetGridRecord: TcxCustomGridRow;
begin
  Result := Container.GridRecord;
end;

function TcxGridRowFooterCellViewInfo.GetData: TcxGridRowFooterCellData;
begin
  Result := TcxGridRowFooterCellData(inherited Data);
end;

function TcxGridRowFooterCellViewInfo.GetText: string;
begin
  try
    Result := Summary.GetGroupFooterSummaryText(GridRecord.Index, Container.GroupLevel,
      SummaryItem.Index, Data.GroupedColumnIndex);
  except
    Application.HandleException(Self);
  end;
end;

procedure TcxGridRowFooterCellViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetFooterCellParams(GridRecord, Column, Container.GroupLevel,
    SummaryItem, AParams);
end;

{ TcxGridRowFooterViewInfo }

constructor TcxGridRowFooterViewInfo.Create(AContainer: TcxGridRowFootersViewInfo;
  ALevel: Integer);
begin
  FContainer := AContainer;
  FLevel := ALevel;
  inherited Create(AContainer.GridViewInfo);
end;

function TcxGridRowFooterViewInfo.GetIndent: Integer;
begin
  Result := VisualLevel * GridViewInfo.LevelIndent;
end;

function TcxGridRowFooterViewInfo.GetGridRecord: TcxCustomGridRow;
begin
  Result := RowViewInfo.GridRecord;
end;

function TcxGridRowFooterViewInfo.GetGroupLevel: Integer;
begin
  Result := RowViewInfo.Level - FLevel;
  if GridView.OptionsView.GroupFooters = gfVisibleWhenExpanded then
    Dec(Result);
end;

function TcxGridRowFooterViewInfo.GetRowViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := FContainer.RowViewInfo;
end;

function TcxGridRowFooterViewInfo.CalculateHeight: Integer;
begin
  Result := inherited CalculateHeight;
  Height := Result;
end;

function TcxGridRowFooterViewInfo.CalculateWidth: Integer;
begin
  Result := inherited CalculateWidth - Indent;
end;

function TcxGridRowFooterViewInfo.CreateCellData(ASummaryItem: TcxDataSummaryItem): TcxGridFooterCellData;
begin
  Result := TcxGridRowFooterCellData.Create(ASummaryItem, FPopulatedGroupedColumnIndex);
end;

function TcxGridRowFooterViewInfo.GetBorders: TcxBorders;
begin
  Result := LookAndFeelPainter.FooterBorders;
  if not GridViewInfo.HasFirstBorderOverlap and (VisualLevel = 0) then
    Include(Result, bLeft);
end;

function TcxGridRowFooterViewInfo.GetColumnWidth(AColumn: TcxGridColumn): Integer;
begin
  Result := inherited GetColumnWidth(AColumn);
  if AColumn.IsMostLeft then
    Dec(Result, Indent);
end;

function TcxGridRowFooterViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridGroupFooterHitTest;
end;

function TcxGridRowFooterViewInfo.GetIsPart: Boolean;
begin
  Result := False;
end;

function TcxGridRowFooterViewInfo.GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect;
begin
  Result := Container.GridViewInfo.FooterViewInfo.GetItemAreaBounds(AItem);
end;

function TcxGridRowFooterViewInfo.GetItemClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridRowFooterCellViewInfo;
end;

function TcxGridRowFooterViewInfo.GetItemHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridGroupFooterCellHitTest;
end;

function TcxGridRowFooterViewInfo.GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean;
begin
  Result := GridViewInfo.FooterViewInfo.GetItemMultiLinePainting(AItem);
end;

function TcxGridRowFooterViewInfo.GetSummaryItems: TcxDataSummaryItems;
begin
  Result := GridView.FDataController.Summary.GroupSummaryItems[GroupLevel];
end;

procedure TcxGridRowFooterViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetFooterParams(GridRecord, nil, GroupLevel, nil, AParams);
end;

function TcxGridRowFooterViewInfo.GetVisible: Boolean;
begin
  Result := True;
end;

function TcxGridRowFooterViewInfo.GetVisualLevel: Integer;
begin
  Result := Container.GridViewInfo.GetVisualLevel(RowViewInfo.Level - FLevel);
end;

function TcxGridRowFooterViewInfo.HasSeparator: Boolean;
begin
  Result := False;
end;

procedure TcxGridRowFooterViewInfo.PopulateCellDataList(var AColumnHasSummaries: array of Boolean);
var
  I, ACount: Integer;
  ASummaryItems: TcxDataGroupSummaryItems;
begin
  ACount := GridView.DataController.Groups.GetLevelGroupedItemCount(GroupLevel);
  for I := 0 to ACount - 1 do
  begin
    ASummaryItems := GridView.DataController.Summary.GetGroupSummaryItems(GroupLevel, I);
    FPopulatedGroupedColumnIndex := I;
    PopulateCellDataList(ASummaryItems, AColumnHasSummaries);
  end;
  FPopulatedGroupedColumnIndex := 0;
end;

procedure TcxGridRowFooterViewInfo.Prepare(ACellDataList: TdxFastObjectList);
begin
  GridViewInfo.FooterViewInfo.Prepare(ACellDataList);
end;

function TcxGridRowFooterViewInfo.CanShowMultipleSummaries: Boolean;
begin
  Result := GridView.OptionsView.CanShowGroupFooterMultiSummaries;
end;

{ TcxGridRowFootersViewInfo }

constructor TcxGridRowFootersViewInfo.Create(ARowViewInfo: TcxCustomGridRowViewInfo);
begin
  inherited Create;
  FRowViewInfo := ARowViewInfo;
  FHeight := -1;
  CreateItems;
end;

destructor TcxGridRowFootersViewInfo.Destroy;
begin
  DestroyItems;
  inherited;
end;

function TcxGridRowFootersViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridRowFootersViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := FRowViewInfo.GridViewInfo;
end;

function TcxGridRowFootersViewInfo.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TcxGridRowFootersViewInfo.GetItem(Index: Integer): TcxGridRowFooterViewInfo;
begin
  Result := TcxGridRowFooterViewInfo(FItems[Index]);
end;

function TcxGridRowFootersViewInfo.GetVisibleItem(ALevel: Integer): TcxGridRowFooterViewInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Level = ALevel then
      Exit;
  end;
  Result := nil;
end;

procedure TcxGridRowFootersViewInfo.CreateItems;
var
  I: Integer;
begin
  FItems := TList.Create;
  for I := 0 to FRowViewInfo.Level do
    if FRowViewInfo.HasFooter(I) then
      FItems.Add(GetItemClass.Create(Self, I));
end;

procedure TcxGridRowFootersViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Free;
end;

procedure TcxGridRowFootersViewInfo.BeforeRecalculation;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BeforeRecalculation;
end;

procedure TcxGridRowFootersViewInfo.Calculate(ALeftBound, ATopBound: Integer);
var
  I: Integer;
begin
  IsRightToLeftConverted := False;
  for I := 0 to Count - 1 do
  begin
    Items[I].Calculate(ALeftBound + Items[I].Indent, ATopBound);
    Inc(ATopBound, Items[I].Height);
  end;
end;

function TcxGridRowFootersViewInfo.CalculateHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].CalculateHeight);
end;

procedure TcxGridRowFootersViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridRowFootersViewInfo.GetItemClass: TcxGridRowFooterViewInfoClass;
begin
  Result := TcxGridRowFooterViewInfo;
end;

function TcxGridRowFootersViewInfo.GetCellBestFitWidth(AColumn: TcxGridColumn): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Max(Result, Items[I].GetCellBestFitWidth(AColumn));
end;

function TcxGridRowFootersViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
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

function TcxGridRowFootersViewInfo.GetTopBound(ALevel: Integer; var ATopBound: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Level >= ALevel;
    if Result then
    begin
      ATopBound := Items[I].Bounds.Top;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TcxGridRowFootersViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, DY);
end;

procedure TcxGridRowFootersViewInfo.Paint;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Paint;
end;

procedure TcxGridRowFootersViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxCustomGridRowViewInfo }

constructor TcxCustomGridRowViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  ARecord: TcxCustomGridRecord);
begin
  inherited;
  FIndicatorItem := GridViewInfo.IndicatorViewInfo.AddRowItem(Self);
  if HasFooters then
    CreateFootersViewInfo;
end;

destructor TcxCustomGridRowViewInfo.Destroy;
begin
  DestroyFootersViewInfo;
  FIndicatorItem.Free;
  inherited;
end;

function TcxCustomGridRowViewInfo.GetCacheItem: TcxGridTableViewInfoCacheItem;
begin
  Result := TcxGridTableViewInfoCacheItem(inherited CacheItem);
end;

function TcxCustomGridRowViewInfo.GetCheckBoxState: TcxCheckBoxState;
begin
  Result := GridRecord.CheckBoxState;
end;

function TcxCustomGridRowViewInfo.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxCustomGridRowViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridRowViewInfo.GetGridLines: TcxGridLines;
begin
  Result := RecordsViewInfo.GridLines;
end;

function TcxCustomGridRowViewInfo.GetGridRecord: TcxCustomGridRow;
begin
  Result := TcxCustomGridRow(inherited GridRecord);
end;

function TcxCustomGridRowViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxCustomGridRowViewInfo.GetFocusedItemKind: TcxGridFocusedItemKind;
begin
  Result := GridView.Controller.FocusedItemKind;
end;

function TcxCustomGridRowViewInfo.GetLevel: Integer;
begin
  Result := GridRecord.Level;
end;

function TcxCustomGridRowViewInfo.GetLevelIndent: Integer;
begin
  Result := VisualLevel * GridViewInfo.LevelIndent;
end;

function TcxCustomGridRowViewInfo.GetLevelIndentBounds(Index: Integer): TRect;
begin
  Result := Bounds;
  if Index = -1 then
    Result.Right := ContentIndent
  else
  begin
    Inc(Result.Left, GridViewInfo.LevelIndent * Index);
    Result.Right := Result.Left + GridViewInfo.LevelIndent;
    if HasFooters then
      FootersViewInfo.GetTopBound(Level - Index, Result.Bottom);
  end;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxCustomGridRowViewInfo.GetLevelIndentHorzLineBounds(Index: Integer): TRect;
begin
  Result := CalculateLevelIndentHorzLineBounds(Index, LevelIndentBounds[Index]);
end;

function TcxCustomGridRowViewInfo.GetLevelIndentSpaceBounds(Index: Integer): TRect;
begin
  Result := CalculateLevelIndentSpaceBounds(Index, LevelIndentBounds[Index]);
end;

function TcxCustomGridRowViewInfo.GetLevelIndentVertLineBounds(Index: Integer): TRect;
begin
  Result := CalculateLevelIndentVertLineBounds(Index, LevelIndentBounds[Index]);
end;

function TcxCustomGridRowViewInfo.GetRecordsViewInfo: TcxGridRowsViewInfo;
begin
  Result := TcxGridRowsViewInfo(inherited RecordsViewInfo);
end;

procedure TcxCustomGridRowViewInfo.CreateFootersViewInfo;
begin
  FFootersViewInfo := GetFootersViewInfoClass.Create(Self);
end;

procedure TcxCustomGridRowViewInfo.DestroyFootersViewInfo;
begin
  FFootersViewInfo.Free;
end;

procedure TcxCustomGridRowViewInfo.RecreateFootersViewInfo;
begin
  DestroyFootersViewInfo;
  if HasFooters then
    CreateFootersViewInfo;
end;

procedure TcxCustomGridRowViewInfo.AddAdornerTargetElementsForColumn(AList: TStrings; AColumn: TcxCustomGridColumn; AName: string);
begin
//do nothing
end;

procedure TcxCustomGridRowViewInfo.AfterRowsViewInfoCalculate;
begin
end;

procedure TcxCustomGridRowViewInfo.AfterRowsViewInfoOffset;
begin
end;

procedure TcxCustomGridRowViewInfo.CalculateExpandButtonBounds(var ABounds: TRect);
begin
  if IsRectEmpty(Bounds) then
    ABounds := cxNullRect
  else
  begin
    Inc(ABounds.Left, GridViewInfo.ExpandButtonIndent);
    ABounds.Right := ABounds.Left + GridViewInfo.ExpandButtonSize;
    ABounds.Top := (ABounds.Top + ABounds.Bottom - GridViewInfo.ExpandButtonSize) div 2;
    ABounds.Bottom := ABounds.Top + GridViewInfo.ExpandButtonSize;
  end;
end;

function TcxCustomGridRowViewInfo.CalculateHeight: Integer;
begin
  Result := TopPartHeight + BottomPartHeight;
end;

function TcxCustomGridRowViewInfo.CalculateLevelIndentHorzLineBounds(ALevel: Integer;
  const ABounds: TRect): TRect;
begin
  Result := ABounds;
  Result.Top := CalculateLevelIndentSpaceBounds(ALevel, ABounds).Bottom;
  Result.Bottom := Min(Result.Top + GridViewInfo.GridLineWidth, ABounds.Bottom);
end;

function TcxCustomGridRowViewInfo.CalculateLevelIndentSpaceBounds(ALevel: Integer;
  const ABounds: TRect): TRect;
var
  AIsParentRecordLast: Boolean;
begin
  AIsParentRecordLast := GridRecord.IsParentRecordLast[Level - ALevel - 1];
  Result := ABounds;
  if GridLines in [glBoth, glHorizontal] then
  if UseRightToLeftAlignment then
    Inc(Result.Left, GridViewInfo.GridLineWidth)
  else
    Dec(Result.Right, GridViewInfo.GridLineWidth);
  if (GridLines <> glNone) and ((GridLines <> glVertical) and AIsParentRecordLast) then
    Dec(Result.Bottom, GridViewInfo.GridLineWidth);
  if AIsParentRecordLast and not HasAnyFooter(Level - ALevel) then
    Dec(Result.Bottom, SeparatorWidth);
end;

function TcxCustomGridRowViewInfo.CalculateLevelIndentVertLineBounds(ALevel: Integer;
  const ABounds: TRect): TRect;
begin
  Result := ABounds;
  with CalculateLevelIndentSpaceBounds(ALevel, ABounds) do
  begin
    if UseRightToLeftAlignment then
      Result.Right := Left
    else
      Result.Left := Right;
    Result.Bottom := Bottom;
  end;
end;

function TcxCustomGridRowViewInfo.CalculateWidth: Integer;
begin
  Result := 0{Width};
end;

function TcxCustomGridRowViewInfo.CanSize: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridRowViewInfo.CheckRowHeight(var AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
end;

procedure TcxCustomGridRowViewInfo.DoToggleExpanded;
begin
  GridRecord.ToggleExpanded
end;

function TcxCustomGridRowViewInfo.GetAutoHeight: Boolean;
begin
  Result := RecordsViewInfo.AutoRecordHeight;
end;

function TcxCustomGridRowViewInfo.GetBaseHeight: Integer;
begin
  Result := DataHeight;
end;

function TcxCustomGridRowViewInfo.GetBottomPartHeight: Integer;
begin
  Result := SeparatorWidth;
  if HasFooters then
    Inc(Result, FFootersViewInfo.Height);
  if HasLastHorzGridLine then
    Inc(Result, GridViewInfo.GridLineWidth);
end;

function TcxCustomGridRowViewInfo.GetCellTransparent(ACell: TcxGridTableCellViewInfo): Boolean;
begin
  Result := inherited GetCellTransparent(ACell) and not ACell.Selected;
end;

function TcxCustomGridRowViewInfo.GetContentBounds: TRect;
begin
  Result := inherited GetContentBounds;
  Result.Left := ContentIndent;
  Result.Top := Result.Top + TopPartHeight;
  Result.Bottom := Result.Top + DataHeight;
end;

function TcxCustomGridRowViewInfo.GetContentIndent: Integer;
begin
  Result := Bounds.Left + LevelIndent;
end;

function TcxCustomGridRowViewInfo.GetContentWidth: Integer;
begin
  Result := Width - LevelIndent;
end;

function TcxCustomGridRowViewInfo.GetDataHeight: Integer;
begin
  Result := Height - TopPartHeight - BottomPartHeight;
end;

function TcxCustomGridRowViewInfo.GetDataIndent: Integer;
begin
  Result := ContentIndent;
end;

function TcxCustomGridRowViewInfo.GetDataWidth: Integer;
begin
  Result := ContentWidth;
end;

function TcxCustomGridRowViewInfo.GetFixedState: TcxDataControllerRowFixedState;
begin
  Result := GridRecord.FixedState;
end;

function TcxCustomGridRowViewInfo.GetFocusRectBounds: TRect;
begin
  Result := inherited GetFocusRectBounds;
  Result.Left := DataIndent;
  if GridLines <> glNone then
    Dec(Result.Right, GridViewInfo.GridLineWidth);
  Inc(Result.Top, TopPartHeight);
  Result.Bottom := Result.Top + DataHeight;
  if GridLines in [glBoth, glHorizontal] then
    Dec(Result.Bottom, GridViewInfo.GridLineWidth);
end;

function TcxCustomGridRowViewInfo.GetFootersViewInfoClass: TcxGridRowFootersViewInfoClass;
begin
  Result := TcxGridRowFootersViewInfo;
end;

function TcxCustomGridRowViewInfo.GetLastHorzGridLineBounds: TRect;
begin
  Result := Bounds;
  Result.Top := Result.Bottom - GridViewInfo.GridLineWidth;
end;

function TcxCustomGridRowViewInfo.GetMaxHeight: Integer;
begin
  Result := Height;
end;

function TcxCustomGridRowViewInfo.GetNonBaseHeight: Integer;
begin
  Result := Height - BaseHeight;
end;

function TcxCustomGridRowViewInfo.GetRowHeight: Integer;
begin
  Result := Height;
end;

function TcxCustomGridRowViewInfo.GetSeparatorBounds: TRect;
var
  ASeparatorVisualLevel: Integer;
begin
  Result.Left := ContentIndent;
  Result.Right := Result.Left + ContentWidth;
  ASeparatorVisualLevel := GridViewInfo.GetVisualLevel(Level - GridRecord.LastParentRecordCount);
  Dec(Result.Left, (VisualLevel - ASeparatorVisualLevel) * GridViewInfo.LevelIndent);
  Result.Bottom := Bounds.Bottom;
  Result.Top := Result.Bottom - SeparatorWidth;
end;

function TcxCustomGridRowViewInfo.GetSeparatorColor: TColor;
begin
  Result := GridView.OptionsView.GetRowSeparatorColor;
end;

function TcxCustomGridRowViewInfo.GetSeparatorWidth: Integer;
begin
  if ShowSeparator then
    Result := RecordsViewInfo.SeparatorWidth
  else
    Result := 0;
end;

function TcxCustomGridRowViewInfo.GetShowSeparator: Boolean;
begin
  Result := True;
end;

function TcxCustomGridRowViewInfo.GetTopPartHeight: Integer;
begin
  Result := 0;
end;

function TcxCustomGridRowViewInfo.GetVisible: Boolean;
begin
  Result := Index < RecordsViewInfo.PartVisibleCount;
end;

function TcxCustomGridRowViewInfo.GetVisualLevel: Integer;
begin
  Result := GridViewInfo.GetVisualLevel(Level);
end;

function TcxCustomGridRowViewInfo.GetWidth: Integer;
begin
  Result := RecordsViewInfo.RowWidth;
end;

function TcxCustomGridRowViewInfo.HasAnyFooter(ALevel: Integer): Boolean;
var
  AFooterTopBound: Integer;
begin
  Result := HasFooters and FootersViewInfo.GetTopBound(ALevel, AFooterTopBound);
end;

function TcxCustomGridRowViewInfo.HasFooter(ALevel: Integer): Boolean;
begin
  if GridView.OptionsView.GroupFooters = gfInvisible then
    Result := False
  else
  begin
    if GridView.OptionsView.GroupFooters = gfAlwaysVisible then
      Dec(ALevel);
    Result := (0 <= ALevel) and (ALevel < Level) and
      GridRecord.IsParentRecordLast[ALevel] and
      Controller.CanShowGroupFooter(Level - 1 - ALevel);
  end;
end;

function TcxCustomGridRowViewInfo.HasFooters: Boolean;
begin
  Result := True;
end;

function TcxCustomGridRowViewInfo.HasLastHorzGridLine: Boolean;
begin
  Result := RecordsViewInfo.HasLastHorzGridLine(Self) and (GridRecord.IsLast or
    (GridRecord.Index = Controller.LastScrollRecordIndex));
end;

function TcxCustomGridRowViewInfo.InvalidateOnChildHotTrackChanged: Boolean;
begin
  Result := inherited InvalidateOnChildHotTrackChanged or GridRecord.ShowCheckBox and
    (GridView.OptionsSelection.CheckBoxPosition = cbpFirstColumn);
end;

function TcxCustomGridRowViewInfo.IsFullyVisible: Boolean;
begin
  Result := Height = MaxHeight;
end;

function TcxCustomGridRowViewInfo.IsSpecial: Boolean;
begin
  Result := GridRecord.IsSpecial;
end;

function TcxCustomGridRowViewInfo.NeedToggleExpandRecord(
  AHitTest: TcxCustomGridHitTest; AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := (ssDouble in AShift) and GridRecord.ExpandOnDblClick;
end;

procedure TcxCustomGridRowViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  FIndicatorItem.DoOffset(0, DY);
  if HasFooters then
    FFootersViewInfo.Offset(DX, DY);
end;

procedure TcxCustomGridRowViewInfo.SetFixedState(AValue: TcxDataControllerRowFixedState);
begin
  GridRecord.FixedState := AValue;
end;

function TcxCustomGridRowViewInfo.ShowCheckBox: Boolean;
begin
  Result := GridRecord.ShowCheckBox and (GridView.OptionsSelection.CheckBoxPosition = cbpFirstColumn);
end;

procedure TcxCustomGridRowViewInfo.ToggleCheckBox;
begin
  GridRecord.ToggleCheckBox;
end;

procedure TcxCustomGridRowViewInfo.ToggleFixedState(AMenuForBounds: TRect);
begin
  GridRecord.ToggleFixedState(AMenuForBounds);
end;

procedure TcxCustomGridRowViewInfo.BeforeRecalculation;
begin
  inherited;
  if HasFooters then
    FFootersViewInfo.BeforeRecalculation;
end;

procedure TcxCustomGridRowViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  RecreateFootersViewInfo;
  inherited;
  GridViewInfo.IndicatorViewInfo.CalculateRowItem(Self, FIndicatorItem);
  if HasFooters then
    FFootersViewInfo.Calculate(Bounds.Left, ATopBound + Height - BottomPartHeight);
end;

function TcxCustomGridRowViewInfo.Click(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  ASelfLink: TcxObjectLink;
begin
  ASelfLink := cxAddObjectLink(Self);
  try
    Result := inherited Click(AHitTest, AButton, AShift);
    if Result and (ASelfLink.Ref <> nil) and NeedToggleExpandRecord(AHitTest, AButton, AShift) then
      DoToggleExpanded;
  finally
    cxRemoveObjectLink(ASelfLink);
  end;
end;

procedure TcxCustomGridRowViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if GridViewInfo.IsRightToLeftConverted then
    FIndicatorItem.RightToLeftConversion(FIndicatorItem.Bounds);
  if HasFooters then
    FFootersViewInfo.RightToLeftConversion(ABounds);
end;

function TcxCustomGridRowViewInfo.GetBoundsForInvalidate(AItem: TcxCustomGridTableItem): TRect;
var
  R: TRect;
  AIndicatorViewInfo: TcxGridIndicatorViewInfo;
begin
  Result := inherited GetBoundsForInvalidate(AItem);
  if AItem = nil then
  begin
    AIndicatorViewInfo := GridViewInfo.IndicatorViewInfo;
    if AIndicatorViewInfo.Visible then
    begin
      R := AIndicatorViewInfo.GetRowItemBounds(GridRecord);
      if R.Left < Result.Left then Result.Left := R.Left;
      if R.Right > Result.Right then Result.Right := R.Right;
    end;
  end;
end;

function TcxCustomGridRowViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if HasFooters then
    Result := FFootersViewInfo.GetHitTest(P)
  else
    Result := nil;
  if Result = nil then
  begin
    Result := inherited GetHitTest(P);
    if (Result <> nil) and PtInRect(LevelIndentBounds[-1], P) then
    begin
      Result := TcxGridRowLevelIndentHitTest.Instance(P);
      InitHitTest(Result);
    end;
  end;
end;

function TcxCustomGridRowViewInfo.HasSeparator: Boolean;
begin
  Result := SeparatorWidth <> 0;
end;

{ TcxGridFixedRowsViewInfo }

constructor TcxGridFixedDataRowsViewInfo.Create(ARowsViewInfo: TcxGridRowsViewInfo);
begin
  inherited Create;
  FRowsViewInfo := ARowsViewInfo;
  FTopItems := TdxFastObjectList.Create;
  FBottomItems := TdxFastObjectList.Create;
end;

destructor TcxGridFixedDataRowsViewInfo.Destroy;
begin
  FBottomItems.Free;
  FTopItems.Free;
  inherited Destroy;
end;

procedure TcxGridFixedDataRowsViewInfo.Calculate(ABounds: TRect);
begin
  CalculateTopItems(ABounds.Left, ABounds.Top);
  CalculateBottomItems(ABounds.Left, ABounds.Bottom);
end;

function TcxGridFixedDataRowsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
  AHitTest: TcxCustomGridHitTest;
begin
  Result := nil;
  for I := 0 to TopItemCount - 1 do
  begin
    AHitTest := TopItems[I].GetHitTest(P);
    if AHitTest <> nil then
    begin
      Result := AHitTest;
      Exit;
    end;
  end;
  for I := 0 to BottomItemCount - 1 do
  begin
    AHitTest := BottomItems[I].GetHitTest(P);
    if AHitTest <> nil then
    begin
      Result := AHitTest;
      Exit;
    end;
  end;
end;

procedure TcxGridFixedDataRowsViewInfo.Paint;
begin
  with GetPainterClass.Create(Canvas, Self) do
    try
      MainPaint;
    finally
      Free;
    end;
end;

procedure TcxGridFixedDataRowsViewInfo.CalculateBottomItems(ALeftBound, ATopBound: Integer);
var
  I: Integer;
begin
  for I := BottomItemMaxCount - 1 downto 0 do
  begin
    Dec(ATopBound, BottomItems[I].Height);
    BottomItems[I].MainCalculate(ALeftBound, ATopBound);
  end;
end;

procedure TcxGridFixedDataRowsViewInfo.CalculateTopItems(ALeftBound, ATopBound: Integer);
var
  I: Integer;
begin
  for I := 0 to TopItemMaxCount - 1 do
  begin
    TopItems[I].MainCalculate(ALeftBound, ATopBound);
    Inc(ATopBound, TopItems[I].Height);
  end;
end;

procedure TcxGridFixedDataRowsViewInfo.ControlFocusChanged;
var
  I: Integer;
begin
  for I := 0 to TopItemCount - 1 do
    TopItems[I].ControlFocusChanged;
  for I := 0 to BottomItemCount - 1 do
    BottomItems[I].ControlFocusChanged;
end;

function TcxGridFixedDataRowsViewInfo.CreateRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo;
begin
  Result := RowsViewInfo.CreateRowViewInfo(ARow);
end;

procedure TcxGridFixedDataRowsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to TopItemCount - 1 do
    TopItems[I].RightToLeftConversion(ABounds);
  for I := 0 to BottomItemCount - 1 do
    BottomItems[I].RightToLeftConversion(ABounds);
end;

function TcxGridFixedDataRowsViewInfo.GetBottomContentHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to BottomItemMaxCount - 1 do
    Inc(Result, BottomItems[I].Height);
end;

function TcxGridFixedDataRowsViewInfo.GetPainterClass: TcxGridFixedDataRowsPainterClass;
begin
  Result := TcxGridFixedDataRowsPainter;
end;

function TcxGridFixedDataRowsViewInfo.GetTopContentHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TopItemMaxCount - 1 do
    Inc(Result, TopItems[I].Height);
end;

procedure TcxGridFixedDataRowsViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to TopItemCount - 1 do
    TopItems[I].DoOffset(DX, 0);
  for I := 0 to BottomItemCount - 1 do
    BottomItems[I].DoOffset(DX, 0);
end;

procedure TcxGridFixedDataRowsViewInfo.VisibilityChanged(AVisible: Boolean);
var
  I: Integer;
begin
  for I := 0 to TopItemCount - 1 do
    TopItems[I].VisibilityChanged(AVisible);
  for I := 0 to BottomItemCount - 1 do
    BottomItems[I].VisibilityChanged(AVisible);
end;

function TcxGridFixedDataRowsViewInfo.GetBottomItem(Index: Integer): TcxCustomGridRowViewInfo;
var
  ARowIndex: Integer;
  ARow: TcxCustomGridRow;
begin
  if Index >= BottomItemCount then
  begin
    ARowIndex := Controller.LastScrollRecordIndex + Index + 1;
    ARow := ViewData.Rows[ARowIndex];
    FBottomItems.Add(CreateRowViewInfo(ARow));
  end;
  Result := TcxCustomGridRowViewInfo(FBottomItems[Index])
end;

function TcxGridFixedDataRowsViewInfo.GetBottomItemCount: Integer;
begin
  Result := FBottomItems.Count;
end;

function TcxGridFixedDataRowsViewInfo.GetBottomItemMaxCount: Integer;
begin
  Result := ViewData.FixedBottomRowCount;
end;

function TcxGridFixedDataRowsViewInfo.GetCanvas: TcxCanvas;
begin
  Result := RowsViewInfo.Canvas;
end;

function TcxGridFixedDataRowsViewInfo.GetController: TcxGridTableController;
begin
  Result := RowsViewInfo.Controller;
end;

function TcxGridFixedDataRowsViewInfo.GetTopItem(Index: Integer): TcxCustomGridRowViewInfo;
var
  ARow: TcxCustomGridRow;
begin
  if Index >= TopItemCount then
  begin
    ARow := ViewData.Rows[Index];
    FTopItems.Add(CreateRowViewInfo(ARow));
  end;
  Result := TcxCustomGridRowViewInfo(FTopItems[Index]);
end;

function TcxGridFixedDataRowsViewInfo.GetTopItemCount: Integer;
begin
  Result := FTopItems.Count;
end;

function TcxGridFixedDataRowsViewInfo.GetTopItemMaxCount: Integer;
begin
  Result := ViewData.FixedTopRowCount;
end;

function TcxGridFixedDataRowsViewInfo.GetViewData: TcxGridViewData;
begin
  Result := RowsViewInfo.ViewData;
end;

procedure TcxGridFixedDataRowsViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridRowsViewInfo }

destructor TcxGridRowsViewInfo.Destroy;
begin
  FFixedDataRowsViewInfo.Free;
  FFilterRowViewInfo.Free;
  FNewItemRowViewInfo.Free;
  inherited;
end;

function TcxGridRowsViewInfo.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridRowsViewInfo.GetFilterRowViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := FFilterRowViewInfo;
  if (Result <> nil) and (Result.GridRecord = nil) then
    Result := nil;
end;

function TcxGridRowsViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridRowsViewInfo.GetGridLines: TcxGridLines;
begin
  Result := GridViewInfo.GridLines;
end;

function TcxGridRowsViewInfo.GetGridViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited GridViewInfo);
end;

function TcxGridRowsViewInfo.GetHeaderViewInfo: TcxGridHeaderViewInfo;
begin
  Result := GridViewInfo.HeaderViewInfo;
end;

function TcxGridRowsViewInfo.GetItem(Index: Integer): TcxCustomGridRowViewInfo;
begin
  Result := TcxCustomGridRowViewInfo(inherited Items[Index]);
end;

function TcxGridRowsViewInfo.GetNewItemRowViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := FNewItemRowViewInfo;
  if (Result <> nil) and (Result.GridRecord = nil) then
    Result := nil;
end;

function TcxGridRowsViewInfo.GetPainterClassValue: TcxGridRowsPainterClass;
begin
  Result := TcxGridRowsPainterClass(GetPainterClass);
end;

function TcxGridRowsViewInfo.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

procedure TcxGridRowsViewInfo.CalculateFixedGroupsForPixelScrolling;
var
  AFixedGroups: TdxFastList;
begin
  AFixedGroups := TdxFastList.Create;
  try
    PopulateFixedGroups(AFixedGroups);
    DeleteOverlainItems(AFixedGroups);
    PostFixedGroups(AFixedGroups);
    RecalculateFixedGroups;
    UpdateVisibleCount;
  finally
    AFixedGroups.Free;
  end;
end;

procedure TcxGridRowsViewInfo.DeleteOverlainItems(AFixedGroups: TdxFastList);
var
  I: Integer;
  AViewInfo: TcxCustomGridRowViewInfo;
begin
  if AFixedGroups.Empty then
    Exit;
  AViewInfo := TcxCustomGridRowViewInfo(AFixedGroups.Last);
  for I := Count - 1 downto 0 do
    if Items[I].Bounds.Bottom <= AViewInfo.Bounds.Bottom then
      DeleteItem(I);
end;

procedure TcxGridRowsViewInfo.PopulateFixedGroups(AFixedGroups: TdxFastList);

  function GetRowHeight(ARow: TcxCustomGridRow): Integer;
  var
    AViewInfo: TcxCustomGridRecordViewInfo;
    ANewlyCreated: Boolean;
  begin
    AViewInfo := GetRecordViewInfo(ARow.Index, ANewlyCreated);
    try
      Result := AViewInfo.Height;
    finally
      if ANewlyCreated then
        AViewInfo.Free;
    end;
  end;

var
  I, ARowIndex, ATopBound, AGroupRowHeight, AAvailableHeight: Integer;
  ARow, AGroup: TcxCustomGridRow;
  AGroupViewInfo: TcxCustomGridRecordViewInfo;
begin
  ATopBound := ContentBounds.Top;
  ARowIndex := Controller.InternalTopRecordIndex;
  ARow := ViewData.Rows[ARowIndex];
  AAvailableHeight := ARow.ViewInfo.Height + Controller.PixelScrollRecordOffset;
  for I := 0 to GridView.DataController.Groups.LevelCount - 1 do
  begin
    AGroup := ViewData.GetTopGroup(ARowIndex, I);
    AGroupRowHeight := GetRowHeight(AGroup);
    if AAvailableHeight < AGroupRowHeight then
    begin
      Inc(ARowIndex);
      if IsRowLocatedInGroup(ARowIndex, AGroup.Index, I) then
        Inc(AAvailableHeight, GetRowHeight(ViewData.Rows[ARowIndex]))
      else
      begin
        if AAvailableHeight > 0 then
        begin
          if AGroup.ViewInfo <> nil then
            DeleteItem(AGroup.ViewInfo.Index);
          Inc(ATopBound, AAvailableHeight - AGroupRowHeight);
          AGroupViewInfo := CreateFixedGroupRowViewInfo(AGroup);
          AGroupViewInfo.MainCalculate(ContentBounds.Left, ATopBound);
          AFixedGroups.Add(AGroupViewInfo);
        end;
        Break;
      end;
    end;
    if AGroup.ViewInfo <> nil then
      DeleteItem(AGroup.ViewInfo.Index);
    AGroupViewInfo := CreateFixedGroupRowViewInfo(AGroup);
    AGroupViewInfo.MainCalculate(ContentBounds.Left, ATopBound);
    Inc(ATopBound, AGroupRowHeight);
    Dec(AAvailableHeight, AGroupRowHeight);
    AFixedGroups.Add(AGroupViewInfo);
  end;
end;

procedure TcxGridRowsViewInfo.PostFixedGroups(AFixedGroups: TdxFastList);
var
  I: Integer;
begin
  for I := 0 to AFixedGroups.Count - 1 do
    InsertItem(I, AFixedGroups[I]);
end;

procedure TcxGridRowsViewInfo.RecalculateFixedGroups;
var
  I: Integer;
begin
  for I := 0 to GetFixedGroupsCount - 1 do
    Items[I].Recalculate;
end;

procedure TcxGridRowsViewInfo.CalculateFixedGroupsForRecordScrolling;

  procedure ReplaceTopRowsToFixedGroups;

    function GetItemIndex(AIndex: Integer): Integer;
    begin
      if GridViewInfo.CalculateDown then
        Result := AIndex
      else
        Result := VisibleCount - 1;
    end;

    function GetRowIndex(AIndex: Integer): Integer;
    begin
      Result := Items[GetItemIndex(AIndex)].GridRecord.Index;
    end;

  var
    I, ACount: Integer;
    AGroup: TcxCustomGridRow;
  begin
    ACount := Min(GridView.DataController.Groups.LevelCount, Count);
    AGroup := ViewData.GetTopGroup(GetRowIndex(0));
    for I := 0 to ACount - 1 do
      if IsRowLocatedInGroup(GetRowIndex(I), AGroup.Index, AGroup.Level) then
      begin
        AGroup := ViewData.GetTopGroup(GetRowIndex(I), I);
        DeleteItem(GetItemIndex(I));
        InsertItem(I, CreateFixedGroupRowViewInfo(AGroup));
      end
      else
        Break;
  end;

var
  I, ATopBound, AVisibleCount: Integer;
begin
  ReplaceTopRowsToFixedGroups;
  AVisibleCount := VisibleCount;
  FVisibleCount := 0;
  FPartVisibleCount := 0;
  ATopBound := ContentBounds.Top;
  for I := 0 to Count - 1 do
  begin
    Inc(ATopBound, Items[I].Height);
    Inc(FPartVisibleCount);
    if ATopBound <= ContentBounds.Bottom then
      Inc(FVisibleCount)
    else
      Break;
  end;
  if (Count > 0) and (ATopBound <= ContentBounds.Bottom) then
    CreateMissingItems(Items[Count - 1], ATopBound);
  if not GridViewInfo.CalculateDown and (AVisibleCount < VisibleCount) then
    FVisibleCount := AVisibleCount;
  if GridViewInfo.CalculateDown then
    CalculateItems;
end;

procedure TcxGridRowsViewInfo.CreateMissingItems(ABottomViewInfo: TcxCustomGridRowViewInfo; ATopBound: Integer);

  function ValidateIndex(var AIndex: Integer): Boolean;
  var
    ALastIndex: Integer;
  begin
    ALastIndex := Controller.LastScrollRecordIndex;
    Result := (AIndex <= ALastIndex) or ViewData.Rows[ALastIndex].IsData;
    if Result and (AIndex > ALastIndex) then
      AIndex := ALastIndex;
  end;

  procedure CreateMissingFixedGroups(out AIndex: Integer);
  var
    AGroup: TcxCustomGridRow;
    AViewInfo: TcxCustomGridRowViewInfo;
  begin
    AGroup := ABottomViewInfo.GridRecord;
    AIndex := Controller.InternalTopRecordIndex + GetFixedGroupsCount;
    while ValidateIndex(AIndex) and (ATopBound <= ContentBounds.Bottom) and (AGroup.Level <> GridView.DataController.Groups.LevelCount - 1) and
      AGroup.Expanded and IsRowLocatedInGroup(AIndex, AGroup.Index, AGroup.Level) do
    begin
      AGroup := ViewData.GetTopGroup(AIndex, Count);
      AViewInfo := CreateFixedGroupRowViewInfo(AGroup);
      InsertItem(Count, AViewInfo);
      Inc(ATopBound, AViewInfo.Height);
      Inc(FPartVisibleCount);
      if ATopBound <= ContentBounds.Bottom then
        Inc(FVisibleCount);
      Inc(AIndex);
    end;
  end;

  procedure CreateMissingRows(AIndex: Integer);
  var
    ARow: TcxCustomGridRow;
    AViewInfo: TcxCustomGridRowViewInfo;
  begin
    while (ATopBound <= ContentBounds.Bottom) and
      ViewData.IsRecordIndexValid(AIndex) do
    begin
      ARow := ViewData.Rows[AIndex];
      AViewInfo := CreateRowViewInfo(ARow);
      AddRecordViewInfo(AViewInfo);
      Inc(ATopBound, AViewInfo.Height);
      Inc(FPartVisibleCount);
      if ATopBound <= ContentBounds.Bottom then
        Inc(FVisibleCount);
      Inc(AIndex);
    end;
  end;

var
  AIndex: Integer;
begin
  if not ABottomViewInfo.GridRecord.IsData and (ABottomViewInfo.FixedState = rfsFixedToTop) then
    CreateMissingFixedGroups(AIndex)
  else
    AIndex := ABottomViewInfo.GridRecord.Index + 1;
  CreateMissingRows(AIndex);
end;

procedure TcxGridRowsViewInfo.AfterCalculate;
begin
  NotifyItemsCalculationFinished;
  inherited;
end;

procedure TcxGridRowsViewInfo.AfterOffset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AfterRowsViewInfoOffset;
  NotifyItemsCalculationFinished;
  inherited;
end;

procedure TcxGridRowsViewInfo.Calculate;
var
  ATopBound: Integer;
begin
  CalculateConsts;
  inherited Calculate;
  if GridViewInfo.CalculateDown then
  begin
    ATopBound := Bounds.Top;
    if HasFilterRow then
    begin
      FilterRowViewInfo.MainCalculate(ContentBounds.Left, ATopBound);
      Inc(ATopBound, FilterRowViewInfo.Height);
    end;
    if HasNewItemRow then
    begin
      NewItemRowViewInfo.MainCalculate(ContentBounds.Left, ATopBound);
      Inc(ATopBound, NewItemRowViewInfo.Height);
    end;
    if HasFixedDataRows then
      FixedDataRowsViewInfo.Calculate(Rect(ContentBounds.Left, ATopBound, -1, Bounds.Bottom));
  end;
  CalculateVisibleCount;
  if (FirstRecordIndex <> -1) and (PartVisibleCount > 0) then
  begin
    if GridViewInfo.CalculateDown then
      CalculateItems;
    if NeedCalculateFixedGroups then
      CalculateFixedGroups;
  end;
end;

function TcxGridRowsViewInfo.CalculateBounds: TRect;
begin
  Result := inherited CalculateBounds;
  if IsScrollable then
    Dec(Result.Left, GridViewInfo.LeftPos);
  Result.Right := Result.Left + RowWidth;
end;

procedure TcxGridRowsViewInfo.CalculateConsts;
begin
  FRowHeight := CalculateRowHeight;
  FDataRowHeight := CalculateDataRowHeight;
  FGroupRowHeight := CalculateGroupRowHeight;
  FCommonPreviewHeight := CalculatePreviewDefaultHeight;
end;

function TcxGridRowsViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  if HasFilterRow then
    Inc(Result.Top, FilterRowViewInfo.Height);
  if HasNewItemRow then
    Inc(Result.Top, NewItemRowViewInfo.Height);
  if HasFixedDataRows then
  begin
    Inc(Result.Top, FixedDataRowsViewInfo.GetTopContentHeight);
    Dec(Result.Bottom, FixedDataRowsViewInfo.GetBottomContentHeight);
  end;
end;

function TcxGridRowsViewInfo.CalculateDataRowHeight: Integer;
begin
  Result := FRowHeight;
end;

procedure TcxGridRowsViewInfo.CalculateFixedGroups;
begin
  if Controller.IsRecordPixelScrolling then
    CalculateFixedGroupsForPixelScrolling
  else
    CalculateFixedGroupsForRecordScrolling;
end;

function TcxGridRowsViewInfo.CalculateGroupImagesHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GridView.GroupedColumnCount - 1 do
    if GridView.GroupedColumns[I].ShowGroupValuesWithImages then
      Result := Max(Result, GridView.GroupedColumns[I].GetImageComboBoxProperties.Images.Height);
end;

function TcxGridRowsViewInfo.CalculateGroupRowDefaultHeight(AMinHeight: Boolean): Integer;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetGroupParams(nil, 0, AParams);
  Result := CalculateCustomGroupRowHeight(AMinHeight, AParams);
end;

function TcxGridRowsViewInfo.CalculateGroupRowHeight: Integer;
begin
  Result := GridView.OptionsView.GroupRowHeight;
  if Result = 0 then
  begin
    Result := CalculateGroupRowDefaultHeight(False);
    dxAdjustToTouchableSize(Result, ScaleFactor);
  end;
end;

procedure TcxGridRowsViewInfo.CalculateItems;
var
  I: Integer;
begin
  for I := 0 to FPartVisibleCount - 1 do
    Items[I].MainCalculate(GetItemLeftBound(I), GetItemTopBound(I));
end;

function TcxGridRowsViewInfo.CalculatePreviewDefaultHeight: Integer;
begin
  Result := GridView.Preview.GetFixedHeight;
  if Result <> 0 then
    Result := GetCellHeight(Result);
end;

function TcxGridRowsViewInfo.CalculateRestHeight(ATopBound: Integer): Integer;
begin
  Result := ContentBounds.Bottom - ATopBound;
  {if not GridViewInfo.IsCalculating or GridViewInfo.CalculateDown then
    Result := ContentBounds.Bottom - ATopBound
  else
    Result := MaxInt - 100000;}
end;

function TcxGridRowsViewInfo.CalculateRowDefaultHeight: Integer;
var
  I, AFontHeight: Integer;
  AParams: TcxViewParams;
begin
  if GridView.VisibleColumnCount = 0 then
  begin
    GridView.Styles.GetContentParams(nil, nil, AParams);
    Result := GridViewInfo.GetFontHeight(AParams.Font);
    GetCellTextAreaSize(Result, ScaleFactor);
  end
  else
  begin
    Result := 0;
    for I := 0 to HeaderViewInfo.Count - 1 do
    begin
      GridView.Styles.GetDataCellParams(nil, HeaderViewInfo[I].Column, AParams);
      AFontHeight := HeaderViewInfo[I].Column.CalculateDefaultCellHeight(Canvas, AParams.Font);
      if AFontHeight > Result then Result := AFontHeight;
    end;
  end;
  Result := GetCellHeight(Result);
end;

function TcxGridRowsViewInfo.CalculateRowHeight: Integer;
begin
  if IsDataRowHeightAssigned then
    Result := GridView.OptionsView.DataRowHeight
  else
  begin
    Result := CalculateRowDefaultHeight;
    dxAdjustToTouchableSize(Result, ScaleFactor);
  end;
end;

procedure TcxGridRowsViewInfo.CalculateVisibleCount;
var
  ALastBottom, I, AHeight, AContentBottom: Integer;
begin
  inherited CalculateVisibleCount;
  FIsFirstRowFullyVisible := True;
  if FirstRecordIndex = -1 then Exit;
  ALastBottom := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset;
  AContentBottom := ContentBounds.Bottom;
  for I := 0 to MaxCount - 1 do
  begin
    Inc(FPartVisibleCount);
    FRestHeight := CalculateRestHeight(ALastBottom);
    AHeight := Items[I].MaxHeight;
    Inc(ALastBottom, AHeight);
    if I = MaxCount - 1 then
      Dec(AContentBottom, GridView.GetHorizontalScrollBarAreaHeight);
    if ALastBottom > AContentBottom then Break;
    Inc(FVisibleCount);
    if ALastBottom = AContentBottom then Break;
  end;
  if MaxCount > 0 then
  begin
    if (FVisibleCount = FPartVisibleCount) and
      (GridViewInfo.CalculateDown and not Items[FVisibleCount - 1].IsFullyVisible or
       not GridViewInfo.CalculateDown and not Items[0].IsFullyVisible) then
      Dec(FVisibleCount);
    if FVisibleCount = 0 then
    begin
      FVisibleCount := 1;
      FIsFirstRowFullyVisible := False;
    end;
  end;
end;

procedure TcxGridRowsViewInfo.ControlFocusChanged;
begin
  inherited ControlFocusChanged;
  if HasFixedDataRows then
    FixedDataRowsViewInfo.ControlFocusChanged;
end;

function TcxGridRowsViewInfo.CreateFixedGroupRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo;
begin
  Result := CreateRowViewInfo(ARow);
  Result.FixedState := rfsFixedToTop;
end;

function TcxGridRowsViewInfo.CreateRowViewInfo(ARow: TcxCustomGridRow): TcxCustomGridRowViewInfo;
begin
  Result := TcxCustomGridRowViewInfo(CreateRecordViewInfo(ARow));
end;

function TcxGridRowsViewInfo.GetAdjustedScrollPositionForFixedGroupMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
begin
  if Controller.IsRecordPixelScrolling then
    Result := GetAdjustedPixelScrollPositionForFixedGroupsMode(ARow, APosition)
  else
    Result := GetAdjustedIndexScrollPositionForFixedGroupsMode(ARow, APosition);
end;

function TcxGridRowsViewInfo.GetAdjustedPixelScrollPositionForFixedGroupsMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
var
  I, ARowTopPosition: Integer;
  AItem: TcxCustomGridRowViewInfo;
  AViewInfo: TcxCustomGridRecordViewInfo;
begin
  Result := APosition;
  AViewInfo := GetRealItem(ARow);
  if AViewInfo = nil then
    Exit;
  ARowTopPosition := AViewInfo.Bounds.Top;
  if (AViewInfo.Index = 0) and (ARowTopPosition < ContentBounds.Top) then
    Result := Result + ARowTopPosition - ContentBounds.Top
  else
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      if (AItem = AViewInfo) or (AItem.FixedState = rfsNotFixed) then
        Break;
      if ARowTopPosition < AItem.Bounds.Bottom then
        Result := Result + ARowTopPosition - AItem.Bounds.Bottom;
    end;
end;

function TcxGridRowsViewInfo.GetAdjustedIndexScrollPositionForFixedGroupsMode(ARow: TcxCustomGridRow; APosition: Integer): Integer;
var
  ARowTopPosition: Integer;
begin
  Result := APosition;
  if ARow.FixedState = rfsNotFixed then
  begin
    ARowTopPosition := ARow.Index - Controller.InternalTopRecordIndex;
    if (ARowTopPosition < ARow.Level) then
      Result := Result + ARowTopPosition - ARow.Level;
  end;
end;

function TcxGridRowsViewInfo.GetAutoDataCellHeight: Boolean;
begin
  Result := inherited GetAutoDataCellHeight and
    GridViewInfo.SupportsAutoHeight and
    (not IsDataRowHeightAssigned or GridView.IsGetCellHeightAssigned);
end;

function TcxGridRowsViewInfo.GetCommonDataRowHeight: Integer;
begin
  Result := FDataRowHeight + SeparatorWidth;
end;

function TcxGridRowsViewInfo.GetFilterRowViewInfoClass: TcxCustomGridRowViewInfoClass;
begin
  Result := TcxGridFilterRowViewInfo;
end;

function TcxGridRowsViewInfo.GetFixedGroupsBottomBound: Integer;
var
  AFixedGroupCount: Integer;
begin
  Result := ContentBounds.Top;
  AFixedGroupCount := GetFixedGroupsCount;
  if AFixedGroupCount > 0 then
    Result := Items[AFixedGroupCount - 1].Bounds.Bottom;
end;

function TcxGridRowsViewInfo.GetFixedGroupsCount: Integer;
var
  I: Integer;
  AItem: TcxCustomGridRowViewInfo;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if (AItem = nil) or (Items[I].FixedState = rfsNotFixed) then
      Break;
    Inc(Result);
  end;
end;

function TcxGridRowsViewInfo.GetGroupBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbGroup);
end;

function TcxGridRowsViewInfo.GetGroupRowSeparatorWidth: Integer;
begin
  if GridView.OptionsView.GroupRowStyle = grsOffice11 then
    Result := cxGridOffice11GroupRowSeparatorWidth
  else
    Result := 0;
end;

function TcxGridRowsViewInfo.GetItemLeftBound(AIndex: Integer): Integer;
begin
  Result := ContentBounds.Left;
end;

function TcxGridRowsViewInfo.GetItemsOffset(AItemCountDelta: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Abs(AItemCountDelta) - 1 do
    Inc(Result, Items[I].Height);
  if AItemCountDelta > 0 then
    Result := -Result;
end;

function TcxGridRowsViewInfo.GetItemTopBound(AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := ContentBounds.Top + GridViewInfo.PixelScrollRecordOffset
  else
    Result := Items[AIndex - 1].Bounds.Bottom;
end;

function TcxGridRowsViewInfo.GetIsScrollable: Boolean;
begin
  Result := HeaderViewInfo.IsScrollable;
end;

function TcxGridRowsViewInfo.GetNewItemRowViewInfoClass: TcxCustomGridRowViewInfoClass;
begin
  Result := TcxGridNewItemRowViewInfo;
end;

function TcxGridRowsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxGridRowsPainter;
end;

function TcxGridRowsViewInfo.GetRowWidth: Integer;
begin
  Result := GridViewInfo.DataWidth;
end;

function TcxGridRowsViewInfo.GetSeparatorWidth: Integer;
begin
  Result := GridView.OptionsView.RowSeparatorWidth;
end;

function TcxGridRowsViewInfo.GetViewInfoIndexByRecordIndex(ARecordIndex: Integer): Integer;
begin
  if GridView.IsFixedGroupsMode then
    Result := -1
  else
    Result := inherited GetViewInfoIndexByRecordIndex(ARecordIndex);
end;

function TcxGridRowsViewInfo.HasFilterRow: Boolean;
begin
  Result := FilterRowViewInfo <> nil;
end;

function TcxGridRowsViewInfo.HasFixedDataRows: Boolean;
begin
  Result := FixedDataRowsViewInfo <> nil;
end;

function TcxGridRowsViewInfo.HasLastHorzGridLine(ARowViewInfo: TcxCustomGridRowViewInfo): Boolean;
begin
  Result := (GridLines = glVertical) and
    ((ARowViewInfo = nil) and (SeparatorWidth = 0) or
     (ARowViewInfo <> nil) and not ARowViewInfo.HasSeparator);
end;

function TcxGridRowsViewInfo.HasNewItemRow: Boolean;
begin
  Result := NewItemRowViewInfo <> nil;
end;

function TcxGridRowsViewInfo.IsCellPartVisibleForFixedGroupsMode(ACellViewInfo: TcxGridTableDataCellViewInfo): Boolean;
var
  AEditTopBound, AFixedGroupsBottomBound: Integer;
begin
  AEditTopBound := ACellViewInfo.EditBounds.Top;
  AFixedGroupsBottomBound := GetFixedGroupsBottomBound;
  Result := AFixedGroupsBottomBound > AEditTopBound;
end;

function TcxGridRowsViewInfo.IsFilterRowVisible: Boolean;
begin
  Result := GridView.FilterRow.Visible;
end;

function TcxGridRowsViewInfo.IsFixedDataRowsVisible: Boolean;
begin
  Result := ViewData.FixedTopRowCount + ViewData.FixedBottomRowCount > 0;
end;

function TcxGridRowsViewInfo.IsNewItemRowVisible: Boolean;
begin
  Result := GridView.NewItemRow.Visible;
end;

function TcxGridRowsViewInfo.IsRowLocatedInGroup(ARowIndex, AGroupIndex, ALevel: Integer): Boolean;
begin
  Result := ViewData.IsRecordIndexValid(ARowIndex) and
    (AGroupIndex = ViewData.GetTopGroupIndex(ARowIndex, ALevel));
end;

function TcxGridRowsViewInfo.NeedCalculateFixedGroups: Boolean;
begin
  Result := GridView.IsFixedGroupsMode and
    ((FirstRecordIndex <> 0) or (PixelScrollRecordOffset <> 0)) and
    (GridViewInfo.CalculateDown or not Controller.IsRecordPixelScrolling);
end;

procedure TcxGridRowsViewInfo.NotifyItemsCalculationFinished;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AfterRowsViewInfoCalculate;
end;

procedure TcxGridRowsViewInfo.OffsetItem(AIndex, AOffset: Integer);
begin
  Items[AIndex].DoOffset(0, AOffset);
end;

procedure TcxGridRowsViewInfo.UpdateVisibleCount;
var
  I, AVisibleCount: Integer;
  AItem: TcxCustomGridRowViewInfo;
begin
  AVisibleCount := VisibleCount;
  FPartVisibleCount := Count;
  FVisibleCount := 0;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if (ContentBounds.Bottom >= AItem.Bounds.Bottom) then
      Inc(FVisibleCount);
  end;
  if not GridViewInfo.CalculateDown and (AVisibleCount < VisibleCount) then
    FVisibleCount := AVisibleCount;
end;

procedure TcxGridRowsViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  inherited VisibilityChanged(AVisible);
  if HasFixedDataRows then
    FixedDataRowsViewInfo.VisibilityChanged(AVisible);
end;

procedure TcxGridRowsViewInfo.AfterConstruction;
begin
  inherited;
  if IsFilterRowVisible then
    FFilterRowViewInfo := GetFilterRowViewInfoClass.Create(Self, ViewData.FilterRow);
  if IsNewItemRowVisible then
    FNewItemRowViewInfo := GetNewItemRowViewInfoClass.Create(Self, ViewData.NewItemRow);
  if IsFixedDataRowsVisible then
    FFixedDataRowsViewInfo := TcxGridFixedDataRowsViewInfo.Create(Self);
end;

function TcxGridRowsViewInfo.CalculateCustomGroupRowHeight(AMinHeight: Boolean; AParams: TcxViewParams): Integer;
begin
  Result := Max(GridViewInfo.GetFontHeight(AParams.Font), GridViewInfo.ExpandButtonSize);
  if GridView.OptionsView.GroupRowStyle = grsStandard then
    Result := Max(Result, CalculateGroupImagesHeight);
  GetCellTextAreaSize(Result, ScaleFactor);
  if GridView.OptionsView.GroupRowStyle = grsStandard then
    Result := GetCellHeight(Result)
  else
  begin
    if not AMinHeight then
      Result := 2 * Result;
    Inc(Result, GroupRowSeparatorWidth);
  end;
  if GridView.OptionsView.GroupRowStyle = grsOffice11 then
    Result := Max(Result, CalculateGroupImagesHeight);
  Inc(Result, cxMarginsHeight(GridViewInfo.LookAndFeelPainter.DefaultGroupContentOffsets));
end;

function TcxGridRowsViewInfo.CanDataRowSize: Boolean;
begin
  Result := GridViewInfo.SupportsAutoHeight and GridView.OptionsCustomize.DataRowSizing;
end;

procedure TcxGridRowsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  if HasFilterRow then
    FilterRowViewInfo.RightToLeftConversion(ABounds);
  if HasNewItemRow then
    NewItemRowViewInfo.RightToLeftConversion(ABounds);
  if HasFixedDataRows then
    FixedDataRowsViewInfo.RightToLeftConversion(ABounds);
  if (FirstRecordIndex <> -1) and (PartVisibleCount > 0) then
  begin
    for I := 0 to FPartVisibleCount - 1 do
      Items[I].RightToLeftConversion(ABounds);
//    if NeedCalculateFixedGroups then
//      CalculateFixedGroups;
  end;
end;

function TcxGridRowsViewInfo.GetCellHeight(ACellContentHeight: Integer): Integer;
begin
  Result := inherited GetCellHeight(ACellContentHeight);
  if GridLines in [glBoth, glHorizontal] then
    Inc(Result, GridViewInfo.GridLineWidth);
end;

function TcxGridRowsViewInfo.GetDataRowCellsAreaViewInfoClass: TClass;
begin
  Result := TcxGridDataRowCellsAreaViewInfo;
end;

function TcxGridRowsViewInfo.GetFooterCellBestFitWidth(AColumn: TcxGridColumn): Integer;
var
  I: Integer;
  ARowViewInfo: TcxCustomGridRowViewInfo;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    ARowViewInfo := Items[I];
    if (ARowViewInfo <> nil) and ARowViewInfo.HasFooters then
      Result := Max(Result, ARowViewInfo.FootersViewInfo.GetCellBestFitWidth(AColumn));
  end;
end;

function TcxGridRowsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if HasFilterRow then
  begin
    Result := FilterRowViewInfo.GetHitTest(P);
    if Result <> nil then
      Exit;
  end;
  if HasNewItemRow then
  begin
    Result := NewItemRowViewInfo.GetHitTest(P);
    if Result <> nil then
      Exit;
  end;
  if HasFixedDataRows then
  begin
    Result := FixedDataRowsViewInfo.GetHitTest(P);
    if Result <> nil then
      Exit;
  end;
  Result := inherited GetHitTest(P);
end;

function TcxGridRowsViewInfo.GetRealItem(ARecord: TcxCustomGridRecord): TcxCustomGridRecordViewInfo;
begin
  if ViewData.HasFilterRow and (ARecord = ViewData.FilterRow) then
    Result := FilterRowViewInfo
  else
    if ViewData.HasNewItemRecord and ARecord.IsNewItemRecord then
      Result := NewItemRowViewInfo
    else
      if ARecord.IsData and (TcxCustomGridRow(ARecord).FixedState <> rfsNotFixed) then
        Result := ARecord.ViewInfo
      else
        Result := inherited GetRealItem(ARecord);
end;

function TcxGridRowsViewInfo.GetRestHeight(ATopBound: Integer): Integer;
begin
  if IsRecordPixelScrollSizeCalculating then
    Result := cxMaxRectSize
  else
    if GridViewInfo.IsCalculating then
      Result := FRestHeight
    else
      Result := CalculateRestHeight(ATopBound);
end;

function TcxGridRowsViewInfo.IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := inherited IsCellMultiLine(AItem) or IsDataRowHeightAssigned;
end;

function TcxGridRowsViewInfo.IsDataRowHeightAssigned: Boolean;
begin
  Result := GridViewInfo.SupportsAutoHeight and (GridView.OptionsView.DataRowHeight <> 0);
end;

procedure TcxGridRowsViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited Offset(DX, DY);
  if HasFilterRow then
    FilterRowViewInfo.DoOffset(DX, 0);
  if HasNewItemRow then
    NewItemRowViewInfo.DoOffset(DX, 0);
  if HasFixedDataRows then
    FixedDataRowsViewInfo.Offset(DX, 0);
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, 0);
end;

{ TcxGridTableViewInfo }

function TcxGridTableViewInfo.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridTableViewInfo.GetDataWidth: Integer;
begin
  if FDataWidth = 0 then
    FDataWidth := CalculateDataWidth;
  Result := FDataWidth;
end;

function TcxGridTableViewInfo.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewInfo.GetGridLines: TcxGridLines;
begin
  Result := GridView.OptionsView.GridLines;
end;

function TcxGridTableViewInfo.GetLeftPos: Integer;
begin
  Result := Controller.LeftPos;
end;

function TcxGridTableViewInfo.GetLevelIndentBackgroundBitmap: TBitmap;
begin
  Result := RecordsViewInfo.GroupBackgroundBitmap;
end;

function TcxGridTableViewInfo.GetLevelIndentColor(Index: Integer): TColor;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetGroupParams(nil, Index, AParams);
  Result := AParams.Color;
end;

function TcxGridTableViewInfo.GetRecordsViewInfo: TcxGridRowsViewInfo;
begin
  Result := TcxGridRowsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridTableViewInfo.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

procedure TcxGridTableViewInfo.AfterCalculating;
begin
  if Visible and (RecordsViewInfo.DataRowHeight <> FPrevDataRowHeight) then
    Controller.PostGridModeBufferCountUpdate;
  inherited;
end;

procedure TcxGridTableViewInfo.BeforeCalculating;
begin
  inherited;
  CalculateExpandButtonParams;
  if Visible then
    FPrevDataRowHeight := RecordsViewInfo.DataRowHeight;
  FBorderOverlapSize := CalculateBorderOverlapSize;
end;

procedure TcxGridTableViewInfo.CreateViewInfos;

  function GetFindPanelViewInfoIndex: Integer;
  begin
    if (Controller.FindPanel = nil) or (FindPanelViewInfo.Alignment = gpaTop) then
      Result := FGroupByBoxViewInfo.Index
    else
      Result := FFooterViewInfo.Index;
  end;

  function GetFilterViewInfoIndex: Integer;
  begin
    if FilterViewInfo.Alignment = gpaTop then
      Result := FHeaderViewInfo.Index
    else
      Result := FFooterViewInfo.Index;
  end;

begin
//  inherited; - because of new item row view info in banded view
  FGroupByBoxViewInfo := GetGroupByBoxViewInfoClass.Create(Self);
  FHeaderViewInfo := GetHeaderViewInfoClass.Create(Self);
  FFooterViewInfo := GetFooterViewInfoClass.Create(Self);
  FIndicatorViewInfo := GetIndicatorViewInfoClass.Create(Self);
  inherited CreateViewInfos;
  FindPanelViewInfo.Index := GetFindPanelViewInfoIndex;
  FilterViewInfo.Index := GetFilterViewInfoIndex;
end;

procedure TcxGridTableViewInfo.DestroyViewInfos(AIsRecreating: Boolean);
begin
  inherited;
  FreeAndNil(FIndicatorViewInfo);
  FreeAndNil(FFooterViewInfo);
  FreeAndNil(FHeaderViewInfo);
  FreeAndNil(FGroupByBoxViewInfo);
end;


procedure TcxGridTableViewInfo.Calculate;
begin
  try
    RecreateViewInfos;
    CalculateParts;
    ClientBounds := CalculateClientBounds;
    IndicatorViewInfo.Calculate(Bounds.Left, ClientBounds.Top - HeaderViewInfo.Height);
  finally
    inherited;
  end;
end;

function TcxGridTableViewInfo.CalculateBorderOverlapSize: Integer;
begin
  if GridView.IsExportMode then
    Result := 0
  else
    Result := LookAndFeelPainter.GridBordersOverlapSize;
end;

function TcxGridTableViewInfo.CalculateClientBounds: TRect;
begin
  Result := inherited CalculateClientBounds;
  Inc(Result.Left, IndicatorViewInfo.Width);
end;

function TcxGridTableViewInfo.CalculateDataWidth: Integer;
begin
  Result := HeaderViewInfo.Width;
  if (Result = 0) and GridView.OptionsView.ColumnAutoWidth then
    Result := ClientWidth;
end;

function TcxGridTableViewInfo.GetEqualHeightRecordScrollSize: Integer;
begin
  Result := RecordsViewInfo.CommonDataRowHeight + RecordsViewInfo.CommonPreviewHeight;
end;

procedure TcxGridTableViewInfo.CalculateExpandButtonParams;
begin
  FExpandButtonIndent := 3;
  FLevelIndent := FExpandButtonIndent + ExpandButtonSize + FExpandButtonIndent;
  if cxIsTouchModeEnabled and not dxElementSizeFitsForTouch(FLevelIndent, ScaleFactor) then
  begin
    dxAdjustToTouchableSize(FLevelIndent, ScaleFactor);
    FExpandButtonIndent := (FLevelIndent - ExpandButtonSize) div 2;
    if (FLevelIndent - ExpandButtonSize) mod 2 <> 0 then
      Inc(FExpandButtonIndent);
    FLevelIndent := FExpandButtonIndent + ExpandButtonSize + FExpandButtonIndent;
  end;
end;

procedure TcxGridTableViewInfo.CalculateHeight(const AMaxSize: TPoint;
  var AHeight: Integer; var AFullyVisible: Boolean);
begin
  MainCalculate(Classes.Bounds(cxInvisibleCoordinate, 0, AMaxSize.X, AMaxSize.Y));
  if VisibleRecordCount = 0 then
    AHeight := GetNonRecordsAreaHeight(False) + RecordsViewInfo.DataRowHeight
  else
    AHeight := RecordsViewInfo.Items[RecordsViewInfo.Count - 1].Bounds.Bottom +
      PartsBottomHeight;
  AFullyVisible := (VisibleRecordCount = 0) or
    (VisibleRecordCount = Controller.LastScrollRecordIndex - FirstRecordIndex + 1) and
    Controller.IsDataFullyVisible(True);
  inherited;
end;

function TcxGridTableViewInfo.CalculatePartBounds(APart: TcxCustomGridPartViewInfo): TRect;
begin
  Result := inherited CalculatePartBounds(APart);
  if APart.IsScrollable then
    OffsetRect(Result, -LeftPos, 0);
end;

procedure TcxGridTableViewInfo.CalculateParts;
begin
  FindPanelViewInfo.MainCalculate;
  GroupByBoxViewInfo.MainCalculate;
  FilterViewInfo.MainCalculate;
  HeaderViewInfo.MainCalculate;
  FooterViewInfo.MainCalculate;
end;

function TcxGridTableViewInfo.CalculateVisibleEqualHeightRecordCount: Integer;
var
  ANonRecordsAreaHeight: Integer;
begin
  ANonRecordsAreaHeight := GetNonRecordsAreaHeight(False);
  Inc(ANonRecordsAreaHeight, GridView.GetHorizontalScrollBarAreaHeight);
  Result := (Bounds.Bottom - Bounds.Top - ANonRecordsAreaHeight) div GetEqualHeightRecordScrollSize;
end;

procedure TcxGridTableViewInfo.CalculateWidth(const AMaxSize: TPoint; var AWidth: Integer);
begin
  if GridView.OptionsView.ColumnAutoWidth then
    with Site.ClientBounds do
      AWidth := Right - Left
  else
    AWidth := IndicatorViewInfo.Width + GetScrollContentWidth;
  inherited;
end;

function TcxGridTableViewInfo.DoGetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if IndicatorViewInfo.HasHitTestPoint(P) then
  begin
    Result := IndicatorViewInfo.GetHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := inherited DoGetHitTest(P);
end;

procedure TcxGridTableViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  IndicatorViewInfo.RightToLeftConversion(ABounds);
  FindPanelViewInfo.RightToLeftConversion(ABounds);
  GroupByBoxViewInfo.RightToLeftConversion(ABounds);
  FilterViewInfo.RightToLeftConversion(ABounds);
  HeaderViewInfo.RightToLeftConversion(ABounds);
  FooterViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridTableViewInfo.GetColumnFooterWidth(AFooterViewInfo: TcxGridFooterViewInfo; AColumn: TcxGridColumn): Integer;
begin
  Result := HeaderViewInfo[AColumn.VisibleIndex].Width;
  if AColumn.IsMostLeft then
    Dec(Result, AFooterViewInfo.BorderSize[bLeft]);
  if AColumn.IsMostRight then
    Dec(Result, AFooterViewInfo.BorderSize[bRight]);
end;

function TcxGridTableViewInfo.GetDefaultGridModeBufferCount: Integer;
begin
  if RecordsViewInfo.DataRowHeight = 0 then
  begin
    Controller.PostGridModeBufferCountUpdate;
    Result := 0;
  end
  else
    Result := Screen.Height div RecordsViewInfo.DataRowHeight + 2;
end;

function TcxGridTableViewInfo.GetFirstItemAdditionalWidth: Integer;
begin
  Result := VisualLevelCount * LevelIndent;
end;

function TcxGridTableViewInfo.GetGridLineColor: TColor;
begin
  Result := GridView.OptionsView.GetGridLineColor;
end;

function TcxGridTableViewInfo.GetGridLineWidth: Integer;
begin
  Result := 1;
end;

function TcxGridTableViewInfo.GetLevelSeparatorColor: TColor;
begin
  Result := GridLineColor;
end;

function TcxGridTableViewInfo.GetMultilineEditorBounds(
  const ACellEditBounds: TRect; ACalculatedHeight: Integer;
  AAutoHeight: TcxInplaceEditAutoHeight): TRect;
var
  AMaxBottomBound: Integer;
begin
  Result := ACellEditBounds;
  AMaxBottomBound := RecordsViewInfo.ContentBounds.Bottom;
  Result.Bottom := Result.Top + ACalculatedHeight;
  if AAutoHeight = eahEditor then
    RecordsViewInfo.AdjustEditorBoundsToIntegralHeight(Result);
  Result.Bottom := Min(Result.Bottom, AMaxBottomBound);
end;

function TcxGridTableViewInfo.GetNonRecordsAreaHeight(ACheckScrollBar: Boolean): Integer;
begin
  Result := inherited GetNonRecordsAreaHeight(ACheckScrollBar);
  if RecordsViewInfo.HasFilterRow then
    Inc(Result, RecordsViewInfo.FilterRowViewInfo.Height);
  if RecordsViewInfo.HasNewItemRow then
    Inc(Result, RecordsViewInfo.NewItemRowViewInfo.Height);
  if RecordsViewInfo.HasFixedDataRows then
  begin
    Inc(Result, RecordsViewInfo.FixedDataRowsViewInfo.GetTopContentHeight);
    Inc(Result, RecordsViewInfo.FixedDataRowsViewInfo.GetBottomContentHeight);
  end;
end;

function TcxGridTableViewInfo.GetScrollableAreaBoundsForEdit: TRect;
var
  AFocusedRow: TcxCustomGridRow;
begin
  Result := inherited GetScrollableAreaBoundsForEdit;
  AFocusedRow := Controller.FocusedRow;
  if (AFocusedRow = nil) or AFocusedRow.IsSpecial then
    Exit;
  if RecordsViewInfo.HasFilterRow then
    Inc(Result.Top, RecordsViewInfo.FilterRowViewInfo.Height);
  if RecordsViewInfo.HasNewItemRow then
    Inc(Result.Top, RecordsViewInfo.NewItemRowViewInfo.Height);
  if RecordsViewInfo.HasFixedDataRows then
    if AFocusedRow.FixedState <> rfsFixedToBottom then
    begin
      Dec(Result.Bottom, RecordsViewInfo.FixedDataRowsViewInfo.GetBottomContentHeight);
      if AFocusedRow.FixedState = rfsNotFixed then
        Inc(Result.Top, RecordsViewInfo.FixedDataRowsViewInfo.GetTopContentHeight);
    end;
end;

function TcxGridTableViewInfo.GetScrollableAreaBoundsHorz: TRect;
begin
  Result := inherited GetScrollableAreaBoundsHorz;
  Dec(Result.Top, HeaderViewInfo.Height);
  Inc(Result.Bottom, FooterViewInfo.Height);
end;

function TcxGridTableViewInfo.GetScrollableAreaBoundsVert: TRect;
begin
  Result := inherited GetScrollableAreaBoundsVert;
  if UseRightToLeftAlignment then
    Inc(Result.Right, IndicatorViewInfo.Width)
  else
    Dec(Result.Left, IndicatorViewInfo.Width);
  Result.Top := RecordsViewInfo.ContentBounds.Top;
  Result.Bottom := RecordsViewInfo.ContentBounds.Bottom;
end;

function TcxGridTableViewInfo.GetVisualLevelCount: Integer;
begin
  Result := GridView.DataController.Groups.LevelCount;
  if (Result <> 0) and (GridView.OptionsView.GroupRowStyle = grsOffice11) then
    Dec(Result);
  if GridView.IsMaster then Inc(Result);
end;

function TcxGridTableViewInfo.HasFirstBorderOverlap: Boolean;
begin
  Result := (BorderOverlapSize > 0) and IndicatorViewInfo.Visible;
end;

procedure TcxGridTableViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to PartCount - 1 do
    with Parts[I] do
      if IsScrollable then DoOffset(DX, 0);
  inherited;
end;

procedure TcxGridTableViewInfo.RecreateViewInfos;
begin
  FDataWidth := 0;
  inherited;
end;

function TcxGridTableViewInfo.SupportsAutoHeight: Boolean;
begin
  Result := True;
end;

function TcxGridTableViewInfo.SupportsGroupSummariesAlignedWithColumns: Boolean;
begin
  Result := True;
end;

function TcxGridTableViewInfo.SupportsMultipleFooterSummaries: Boolean;
begin
  Result := True;
end;

function TcxGridTableViewInfo.GetFooterPainterClass: TcxGridFooterPainterClass;
begin
  Result := TcxGridFooterPainter;
end;

function TcxGridTableViewInfo.GetFooterViewInfoClass: TcxGridFooterViewInfoClass;
begin
  Result := TcxGridFooterViewInfo;
end;

function TcxGridTableViewInfo.GetGroupByBoxViewInfoClass: TcxGridGroupByBoxViewInfoClass;
begin
  Result := TcxGridGroupByBoxViewInfo;
end;

function TcxGridTableViewInfo.GetHeaderViewInfoClass: TcxGridHeaderViewInfoClass;
begin
  Result := TcxGridHeaderViewInfo;
end;

function TcxGridTableViewInfo.GetIndicatorViewInfoClass: TcxGridIndicatorViewInfoClass;
begin
  Result := TcxGridIndicatorViewInfo;
end;

function TcxGridTableViewInfo.GetHeaderViewInfoSpecificClass: TcxGridHeaderViewInfoSpecificClass;
begin
  Result := TcxGridHeaderViewInfoSpecific;
end;

function TcxGridTableViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridRowsViewInfo;
end;

function TcxGridTableViewInfo.GetCellBorders(AIsRight, AIsBottom: Boolean): TcxBorders;
begin
  case GridLines of
    glBoth:
      Result := [bRight, bBottom];
    glNone:
      Result := [];
    glVertical:
      Result := [bRight];
    glHorizontal:
      begin
        if AIsRight then
          Result := [bRight]
        else
          Result := [];
        Include(Result, bBottom);
      end;
  end;
end;

function TcxGridTableViewInfo.GetCellHeight(AIndex, ACellHeight: Integer): Integer;
begin
  Result := ACellHeight;
end;

function TcxGridTableViewInfo.GetCellTopOffset(AIndex, ACellHeight: Integer): Integer;
begin
  Result := 0;
end;

function TcxGridTableViewInfo.GetScrollContentWidth: Integer;
begin
  Result := DataWidth;
  if ((HeaderViewInfo.Width <> 0) or not GridView.OptionsView.ColumnAutoWidth) then
    Inc(Result, GridView.GetVerticalScrollBarAreaWidth);
end;

function TcxGridTableViewInfo.GetOffsetBounds(AItemsOffset: Integer; out AUpdateBounds: TRect): TRect;
var
  AExcludeFocusedRecordFromOffsetBounds: Boolean;
  APrevFocusedRecordDisplayBounds: TRect;
  R: TRect;
begin
  Result := ScrollableAreaBoundsVert;
  R := Result;
  AUpdateBounds := Result;
  AExcludeFocusedRecordFromOffsetBounds :=
    cxRectIntersect(APrevFocusedRecordDisplayBounds, RecordsViewInfo.PrevFocusedItemBounds, Result);
  if AExcludeFocusedRecordFromOffsetBounds then
  begin
    AExcludeFocusedRecordFromOffsetBounds :=
     ((ViewData.Controller.FocusedRecordIndex <> ViewData.Controller.PrevFocusedRecordIndex) or
      (AItemsOffset < 0) and (APrevFocusedRecordDisplayBounds.Bottom > Result.Bottom + AItemsOffset) or
      (AItemsOffset > 0) and (APrevFocusedRecordDisplayBounds.Top < Result.Top + AItemsOffset));
  end;
  if AItemsOffset < 0 then
  begin
    Inc(Result.Top, -AItemsOffset);
    if AExcludeFocusedRecordFromOffsetBounds then
      Result.Bottom := APrevFocusedRecordDisplayBounds.Top;
    AUpdateBounds.Top := Result.Bottom + AItemsOffset;
  end
  else
  begin
    Dec(Result.Bottom, AItemsOffset);
    if AExcludeFocusedRecordFromOffsetBounds then
      Result.Top := APrevFocusedRecordDisplayBounds.Bottom;
    AUpdateBounds.Bottom := Result.Top + AItemsOffset;
  end;
  AUpdateBounds.Top := Max(AUpdateBounds.Top, R.Top);
  AUpdateBounds.Bottom := Min(AUpdateBounds.Bottom, R.Bottom);
end;

function TcxGridTableViewInfo.GetOffsetBounds(DX, DY: Integer; out AUpdateBounds: TRect): TRect;
begin
  Result := ScrollableAreaBoundsHorz;
  AUpdateBounds := Result;
  if DX < 0 then
  begin
    Inc(Result.Left, -DX);
    AUpdateBounds.Left := Max(AUpdateBounds.Left, AUpdateBounds.Right + DX);
  end
  else
  begin
    Dec(Result.Right, DX);
    AUpdateBounds.Right := Min(AUpdateBounds.Left + DX, AUpdateBounds.Right);
  end;
end;

function TcxGridTableViewInfo.GetVisualLevel(ALevel: Integer): Integer;
begin
  Result := ALevel;
  if (Result <> 0) and (Result = GridView.DataController.Groups.LevelCount) and
    (GridView.OptionsView.GroupRowStyle = grsOffice11) then
    Dec(Result);
end;

function TcxGridTableViewInfo.GetNearestPopupHeight(AHeight: Integer;
  AAdditionalRecord: Boolean = False): Integer;
var
  ARowCount: Integer;
begin
  ARowCount := (AHeight - GetNonRecordsAreaHeight(True)) div GetEqualHeightRecordScrollSize;
  if ARowCount < 1 then
    ARowCount := 1;
  if ARowCount > Controller.ScrollRecordCount + Ord(AAdditionalRecord) then
    ARowCount := Controller.ScrollRecordCount + Ord(AAdditionalRecord);
  Result := GetNonRecordsAreaHeight(True) + ARowCount * GetEqualHeightRecordScrollSize;
end;

function TcxGridTableViewInfo.GetPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  Result := GetNonRecordsAreaHeight(True) + ADropDownRowCount * GetEqualHeightRecordScrollSize;
  if GridLines in [glNone, glVertical] then
    Inc(Result, GridLineWidth);
end;

{ TcxGridTableViewInfoCacheItem }

procedure TcxGridTableViewInfoCacheItem.SetPreviewHeight(Value: Integer);
begin
  FPreviewHeight := Value;
  IsPreviewHeightAssigned := True;
end;

procedure TcxGridTableViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  inherited;
  IsPreviewHeightAssigned := False;
end;

{ TcxGridMasterTableViewInfoCacheItem }

destructor TcxGridMasterTableViewInfoCacheItem.Destroy;
begin
  if IsDetailsSiteCachedInfoAssigned then
    FreeAndNil(DetailsSiteCachedInfo);
  inherited;
end;

function TcxGridMasterTableViewInfoCacheItem.GetGridRecord: TcxGridMasterDataRow;
begin
  Result := TcxGridMasterDataRow(inherited GridRecord);
end;

function TcxGridMasterTableViewInfoCacheItem.GetIsDetailsSiteCachedInfoAssigned: Boolean;
begin
  Result := DetailsSiteCachedInfo <> nil;
end;

procedure TcxGridMasterTableViewInfoCacheItem.SetDetailsSiteFullyVisible(Value: Boolean);
begin
  FDetailsSiteFullyVisible := Value;
  IsDetailsSiteFullyVisibleAssigned := True;
end;

procedure TcxGridMasterTableViewInfoCacheItem.SetDetailsSiteHeight(Value: Integer);
begin
  FDetailsSiteHeight := Value;
  IsDetailsSiteHeightAssigned := True;
end;

procedure TcxGridMasterTableViewInfoCacheItem.SetDetailsSiteNormalHeight(Value: Integer);
begin
  FDetailsSiteNormalHeight := Value;
  IsDetailsSiteNormalHeightAssigned := True;
end;

procedure TcxGridMasterTableViewInfoCacheItem.SetDetailsSiteWidth(Value: Integer);
begin
  FDetailsSiteWidth := Value;
  IsDetailsSiteWidthAssigned := True;
end;

procedure TcxGridMasterTableViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  if FUnassigningValues then Exit;
  FUnassigningValues := True;
  try
    inherited;
    IsDetailsSiteFullyVisibleAssigned := False;
    IsDetailsSiteHeightAssigned := False;
    IsDetailsSiteNormalHeightAssigned := False;
    IsDetailsSiteWidthAssigned := False;
    if GridRecord.InternalActiveDetailGridViewExists and
      (GridRecord.InternalActiveDetailGridView.ViewInfoCache <> nil) then
      GridRecord.InternalActiveDetailGridView.ViewInfoCache.UnassignValues(AKeepMaster);
  finally
    FUnassigningValues := False;
  end;
end;

{ TcxGridColumnOptions }

constructor TcxCustomGridColumnOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FAutoWidthSizable := True;
  FGroupFooters := True;
  FHorzSizing := True;
end;

function TcxCustomGridColumnOptions.GetFilterRowOperator: TcxFilterOperatorKind;
begin
  Result := Item.FilterRowOperator;
end;

function TcxCustomGridColumnOptions.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridColumnOptions.GetItem: TcxCustomGridColumn;
begin
  Result := TcxCustomGridColumn(inherited Item);
end;

procedure TcxCustomGridColumnOptions.SetAutoWidthSizable(Value: Boolean);
begin
  if FAutoWidthSizable <> Value then
  begin
    FAutoWidthSizable := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridColumnOptions.SetCellMerging(Value: Boolean);
begin
  if FCellMerging <> Value then
  begin
    FCellMerging := Value;
    Changed;
  end;
end;

procedure TcxCustomGridColumnOptions.SetFilterRowOperator(Value: TcxFilterOperatorKind);
begin
  Item.FilterRowOperator := Value;
end;

procedure TcxCustomGridColumnOptions.SetGroupFooters(Value: Boolean);
begin
  if FGroupFooters <> Value then
  begin
    FGroupFooters := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridColumnOptions.SetHorzSizing(Value: Boolean);
begin
  if FHorzSizing <> Value then
  begin
    FHorzSizing := Value;
    Changed;
  end;
end;

procedure TcxCustomGridColumnOptions.SetShowGroupValuesWithImages(Value: Boolean);
begin
  if ShowGroupValuesWithImages <> Value then
  begin
    FShowGroupValuesWithImages := Value;
    Changed(ticSize);
  end;
end;

procedure TcxCustomGridColumnOptions.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridColumnOptions then
    with TcxCustomGridColumnOptions(Source) do
    begin
      Self.AutoWidthSizable := AutoWidthSizable;
      Self.CellMerging := CellMerging;
      Self.GroupFooters := GroupFooters;
      Self.HorzSizing := HorzSizing;
    end;
  inherited;
end;

{ TcxGridRowFooterCellPos }

type
  TcxGridRowFooterCellPos = class
  public
    Column: TcxGridColumn;
    FooterGroupLevel: Integer;
    Row: TcxCustomGridRow;
    SummaryItem: TcxDataSummaryItem;
    constructor Create(ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
      AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem);
  end;

constructor TcxGridRowFooterCellPos.Create(ARow: TcxCustomGridRow; AColumn: TcxGridColumn;
  AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem);
begin
  inherited Create;
  Row := ARow;
  Column := AColumn;
  FooterGroupLevel := AFooterGroupLevel;
  SummaryItem := ASummaryItem;
end;

{ TcxGridGroupSummaryInfo }

type
  TcxGridGroupSummaryInfo = class
  public
    Row: TcxGridGroupRow;
    SummaryItem: TcxDataSummaryItem;
    constructor Create(ARow: TcxGridGroupRow; ASummaryItem: TcxDataSummaryItem);
  end;

constructor TcxGridGroupSummaryInfo.Create(ARow: TcxGridGroupRow; ASummaryItem: TcxDataSummaryItem);
begin
  inherited Create;
  Row := ARow;
  SummaryItem := ASummaryItem;
end;

{ TcxGridColumnStyles }

function TcxGridColumnStyles.GetGridViewValue: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridColumnStyles.GetItem: TcxGridColumn;
begin
  Result := TcxGridColumn(inherited Item);
end;

procedure TcxGridColumnStyles.SetOnGetFooterStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetFooterStyle, Value) then
  begin
    FOnGetFooterStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridColumnStyles.SetOnGetFooterStyleEx(Value: TcxGridGetFooterStyleExEvent);
begin
  if not dxSameMethods(FOnGetFooterStyleEx, Value) then
  begin
    FOnGetFooterStyleEx := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridColumnStyles.SetOnGetFooterSummaryStyle(Value: TcxGridGetFooterSummaryStyleEvent);
begin
  if not dxSameMethods(FOnGetFooterSummaryStyle, Value) then
  begin
    FOnGetFooterSummaryStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridColumnStyles.SetOnGetGroupSummaryStyle(Value: TcxGridGetGroupSummaryStyleEvent);
begin
  if not dxSameMethods(FOnGetGroupSummaryStyle, Value) then
  begin
    FOnGetGroupSummaryStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridColumnStyles.SetOnGetHeaderStyle(Value: TcxGridGetHeaderStyleEvent);
begin
  if not dxSameMethods(FOnGetHeaderStyle, Value) then
  begin
    FOnGetHeaderStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridColumnStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  case Index of
    isFooter:
      GridView.Styles.GetFooterParams(TcxGridRowFooterCellPos(AData).Row, Item,
        TcxGridRowFooterCellPos(AData).FooterGroupLevel,
        TcxGridRowFooterCellPos(AData).SummaryItem, AParams);
    isGroupSummary:
      GridView.Styles.GetGroupSummaryParams(TcxGridGroupSummaryInfo(AData).Row,
        TcxGridGroupSummaryInfo(AData).SummaryItem, AParams);
    isHeader:
      GridView.Styles.GetHeaderParams(Item, AParams);
  else
    inherited;
  end;
end;

procedure TcxGridColumnStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridColumnStyles then
    with TcxGridColumnStyles(Source) do
    begin
      Self.Footer := Footer;
      Self.GroupSummary := GroupSummary;
      Self.Header := Header;
      Self.OnGetFooterStyle := OnGetFooterStyle;
      Self.OnGetFooterStyleEx := OnGetFooterStyleEx;
      Self.OnGetFooterSummaryStyle := OnGetFooterSummaryStyle;
      Self.OnGetGroupSummaryStyle := OnGetGroupSummaryStyle;
      Self.OnGetHeaderStyle := OnGetHeaderStyle;
    end;
end;

procedure TcxGridColumnStyles.GetFooterParams(ARow: TcxCustomGridRow;
  AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ARowFooterCellPos: TcxGridRowFooterCellPos;
begin
  AStyle := nil;
  if Assigned(FOnGetFooterStyle) then
    FOnGetFooterStyle(GridView, ARow, Item, AStyle);
  if Assigned(FOnGetFooterStyleEx) then
    FOnGetFooterStyleEx(GridView, ARow, Item, AFooterGroupLevel, AStyle);
  if Assigned(FOnGetFooterSummaryStyle) and (ASummaryItem <> nil) then
    FOnGetFooterSummaryStyle(GridView, ARow, Item, AFooterGroupLevel, ASummaryItem, AStyle);
  ARowFooterCellPos := TcxGridRowFooterCellPos.Create(ARow, Item, AFooterGroupLevel, ASummaryItem);
  try
    GetViewParams(isFooter, ARowFooterCellPos, AStyle, AParams);
  finally
    ARowFooterCellPos.Free;
  end;
end;

procedure TcxGridColumnStyles.GetGroupSummaryParams(ARow: TcxGridGroupRow;
  ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ASummaryInfo: TcxGridGroupSummaryInfo;
begin
  AStyle := nil;
  if (ARow <> nil) and Assigned(FOnGetGroupSummaryStyle) then
    FOnGetGroupSummaryStyle(GridView, ARow, Item, ASummaryItem, AStyle);
  ASummaryInfo := TcxGridGroupSummaryInfo.Create(ARow, ASummaryItem);
  try
    GetViewParams(isGroupSummary, ASummaryInfo, AStyle, AParams);
  finally
    ASummaryInfo.Free;
  end;
end;

procedure TcxGridColumnStyles.GetHeaderParams(out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetHeaderStyle) then
    FOnGetHeaderStyle(GridView, Item, AStyle);
  GetViewParams(isHeader, nil, AStyle, AParams);
end;

{ TcxGridColumnSummary }

function TcxGridColumnSummary.GetDataController: TcxCustomDataController;
begin
  Result := TcxGridTableView(GridView).FDataController;
end;

function TcxGridColumnSummary.GetFormat(Index: Integer): string;
begin
  Result := GetSummaryItems(TcxGridSummariesIndex(Index)).GetDataItemFormat(
    Item.Index, GetSummaryItemsPosition(TcxGridSummariesIndex(Index)));
end;

function TcxGridColumnSummary.GetKind(Index: Integer): TcxSummaryKind;
begin
  Result := GetSummaryItems(TcxGridSummariesIndex(Index)).GetDataItemKind(
    Item.Index, GetSummaryItemsPosition(TcxGridSummariesIndex(Index)));
end;

function TcxGridColumnSummary.GetSortByGroupFooterSummary: Boolean;
begin
  Result := GetSummaryItems(siGroup).GetDataItemSorted(Item.Index, spFooter);
end;

function TcxGridColumnSummary.GetSortByGroupSummary: Boolean;
begin
  Result := GetSummaryItems(siGroup).GetDataItemSorted(Item.Index, spGroup);
end;

procedure TcxGridColumnSummary.SetFormat(Index: Integer; const Value: string);
begin
  GetSummaryItems(TcxGridSummariesIndex(Index)).SetDataItemFormat(
    Item.Index, GetSummaryItemsPosition(TcxGridSummariesIndex(Index)), Value);
end;

procedure TcxGridColumnSummary.SetKind(Index: Integer; Value: TcxSummaryKind);
begin
  GetSummaryItems(TcxGridSummariesIndex(Index)).SetDataItemKind(
    Item.Index, GetSummaryItemsPosition(TcxGridSummariesIndex(Index)), Value);
end;

procedure TcxGridColumnSummary.SetSortByGroupFooterSummary(Value: Boolean);
begin
  GetSummaryItems(siGroup).SetDataItemSorted(Item.Index, spFooter, Value);
end;

procedure TcxGridColumnSummary.SetSortByGroupSummary(Value: Boolean);
begin
  GetSummaryItems(siGroup).SetDataItemSorted(Item.Index, spGroup, Value);
end;

function TcxGridColumnSummary.GetSummaryItems(AIndex: TcxGridSummariesIndex): TcxDataSummaryItems;
begin
  with DataController.Summary do
    if AIndex = siFooter then
      Result := FooterSummaryItems
    else
      Result := DefaultGroupSummaryItems;
end;

function TcxGridColumnSummary.GetSummaryItemsPosition(AIndex: TcxGridSummariesIndex): TcxSummaryPosition;
begin
  if AIndex = siGroup then
    Result := spGroup
  else
    Result := spFooter;
end;

procedure TcxGridColumnSummary.Assign(Source: TPersistent);
begin
  if Source is TcxGridColumnSummary then
    with TcxGridColumnSummary(Source) do
    begin
      Self.FooterFormat := FooterFormat;
      Self.FooterKind := FooterKind;
      Self.GroupFooterFormat := GroupFooterFormat;
      Self.GroupFooterKind := GroupFooterKind;
      Self.GroupFormat := GroupFormat;
      Self.GroupKind := GroupKind;
      Self.SortByGroupFooterSummary := SortByGroupFooterSummary;
      Self.SortByGroupSummary := SortByGroupSummary;
    end;
  inherited;
end;

{ TcxCustomGridColumn }

constructor TcxCustomGridColumn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaderGlyphAlignmentVert := vaCenter;
  FHeaderImageIndex := -1;
  FVisibleForEditForm := bDefault;
end;

function TcxCustomGridColumn.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxCustomGridColumn.GetFilterRowOperator: TcxFilterOperatorKind;
begin
  if ViewData.HasFilterRow then
    Result := ViewData.FilterRow.Operators[Index]
  else
    Result := InternalGetFilterRowOperator;
end;

function TcxCustomGridColumn.GetFooterAlignmentHorz: TAlignment;
begin
  if FIsFooterAlignmentHorzAssigned then
    Result := FFooterAlignmentHorz
  else
    Result := GetDefaultValuesProvider.DefaultAlignment;
end;

function TcxCustomGridColumn.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxCustomGridColumn.GetGroupSummaryAlignment: TAlignment;
begin
  if FIsGroupSummaryAlignmentAssigned then
    Result := FGroupSummaryAlignment
  else
    Result := GetDefaultValuesProvider.DefaultAlignment;
end;

function TcxCustomGridColumn.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := GridView.InplaceEditForm;
end;

function TcxCustomGridColumn.GetIsChildInMergedGroup: Boolean;
begin
  Result := DataController.Groups.IsChildInMergedGroup[Index];
end;

function TcxCustomGridColumn.GetIsPreview: Boolean;
begin
  Result := GridView.Preview.Column = Self;
end;

function TcxCustomGridColumn.GetOptions: TcxCustomGridColumnOptions;
begin
  Result := TcxCustomGridColumnOptions(inherited Options);
end;

function TcxCustomGridColumn.GetStyles: TcxGridColumnStyles;
begin
  Result := TcxGridColumnStyles(inherited Styles);
end;

function TcxCustomGridColumn.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

procedure TcxCustomGridColumn.SetFilterRowOperator(AValue: TcxFilterOperatorKind);
begin
  if IsLoading or not ViewData.HasFilterRow then
    InternalSetFilterRowOperator(AValue)
  else
    ViewData.FilterRow.Operators[Index] := AValue;
end;

procedure TcxCustomGridColumn.SetFooterAlignmentHorz(Value: TAlignment);
begin
  if (FooterAlignmentHorz <> Value) or IsLoading then
  begin
    FFooterAlignmentHorz := Value;
    FIsFooterAlignmentHorzAssigned := True;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridColumn.SetGroupSummaryAlignment(Value: TAlignment);
begin
  if (GroupSummaryAlignment <> Value) or IsLoading then
  begin
    FGroupSummaryAlignment := Value;
    FIsGroupSummaryAlignmentAssigned := True;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridColumn.SetHeaderGlyph(Value: TdxSmartGlyph);
begin
  FHeaderGlyph.Assign(Value);
end;

procedure TcxCustomGridColumn.SetHeaderGlyphAlignmentHorz(Value: TAlignment);
begin
  if FHeaderGlyphAlignmentHorz <> Value then
  begin
    FHeaderGlyphAlignmentHorz := Value;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridColumn.SetHeaderGlyphAlignmentVert(Value: TcxAlignmentVert);
begin
  if FHeaderGlyphAlignmentVert <> Value then
  begin
    FHeaderGlyphAlignmentVert := Value;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridColumn.SetHeaderImageIndex(Value: TcxImageIndex);
begin
  if FHeaderImageIndex <> Value then
  begin
    FHeaderImageIndex := Value;
    Changed(ticLayout);
  end;
end;

procedure TcxCustomGridColumn.SetIsChildInMergedGroup(Value: Boolean);
begin
  if GridView.IsAssigningItems and Value then
    GridView.AssigningIsChildInMergedGroupItems.Add(Self)
  else
    DataController.Groups.IsChildInMergedGroup[Index] := Value;
end;

procedure TcxCustomGridColumn.SetLayoutItem(const Value: TcxGridInplaceEditFormLayoutItem);
begin
  if FLayoutItem <> Value then
  begin
    if FLayoutItem <> nil then
    begin
      if FLayoutItem.IsContainerRestoring then
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

procedure TcxCustomGridColumn.SetVisibleForEditForm(AValue: TdxDefaultBoolean);
begin
  if AValue <> FVisibleForEditForm then
  begin
    FVisibleForEditForm := AValue;
    CheckAccessibilityForEditForm;
    Changed(ticProperty);
  end;
end;

procedure TcxCustomGridColumn.SetOptions(Value: TcxCustomGridColumnOptions);
begin
  inherited Options := Value;
end;

procedure TcxCustomGridColumn.SetStyles(Value: TcxGridColumnStyles);
begin
  inherited Styles := Value;
end;

procedure TcxCustomGridColumn.SetSummary(Value: TcxGridColumnSummary);
begin
  FSummary.Assign(Value);
end;

function TcxCustomGridColumn.IsFooterAlignmentHorzStored: Boolean;
begin
  Result := FIsFooterAlignmentHorzAssigned and
    (FFooterAlignmentHorz <> GetDefaultValuesProvider.DefaultAlignment);
end;

function TcxCustomGridColumn.IsGroupSummaryAlignmentStored: Boolean;
begin
  Result := FIsGroupSummaryAlignmentAssigned and
    (FGroupSummaryAlignment <> GetDefaultValuesProvider.DefaultAlignment);
end;

procedure TcxCustomGridColumn.HeaderGlyphChanged(Sender: TObject);
begin
  Changed(ticLayout);
end;

function TcxCustomGridColumn.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('FilterRowOperator');
    Add('GroupIndex');
    Add('IsChildInMergedGroup');
    Add('Width');
  end;
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxCustomGridColumn.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Width' then
    AValue := Width
  else
    if AName = 'GroupIndex' then
      AValue := GroupIndex
    else
      if AName = 'IsChildInMergedGroup' then
        AValue := IsChildInMergedGroup
      else
        if AName = 'FilterRowOperator' then
          AValue := Options.FilterRowOperator
        else
          inherited GetPropertyValue(AName, AValue);
end;

procedure TcxCustomGridColumn.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Width' then
    Width := AValue
  else
    if AName = 'GroupIndex' then
      GroupIndex := AValue
    else
      if AName = 'IsChildInMergedGroup' then
        IsChildInMergedGroup := AValue
      else
        if AName = 'FilterRowOperator' then
          Options.FilterRowOperator := AValue
        else
          inherited SetPropertyValue(AName, AValue);
end;

procedure TcxCustomGridColumn.GetAdornerTargetElements(AList: TStrings);
var
  ANewItemRow, AFilterRow: TcxCustomGridRowViewInfo;
  AHeader: TcxGridHeaderViewInfo;
  AGroupByBox: TcxGridGroupByBoxViewInfo;
  AFooter: TcxGridFooterViewInfo;
  ARows: TcxGridRowsViewInfo;
begin
  inherited GetAdornerTargetElements(AList);
  AHeader := GridView.ViewInfo.HeaderViewInfo;
  if AHeader <> nil then
    AList.AddObject('Header', AHeader.FindItem(Self));
  AGroupByBox := GridView.ViewInfo.GroupByBoxViewInfo;
  if AGroupByBox <> nil then
    AList.AddObject('HeaderInGroupByBox', AGroupByBox.FindItem(Self));
  AFooter := GridView.ViewInfo.FooterViewInfo;
  if AFooter <> nil then
    AFooter.AddAdornerTargetElementForColumn(AList, Self, 'FooterCell');
  ARows := GridView.ViewInfo.RecordsViewInfo;
  if ARows <> nil then
  begin
    AFilterRow := ARows.FilterRowViewInfo;
    if AFilterRow <> nil then
      AFilterRow.AddAdornerTargetElementsForColumn(AList, Self, 'FilterRowCell');
    ANewItemRow := ARows.NewItemRowViewInfo;
    if ANewItemRow <> nil then
      ANewItemRow.AddAdornerTargetElementsForColumn(AList, Self, 'NewItemRowCell');
  end;
end;

procedure TcxCustomGridColumn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLayoutItem) then
    FLayoutItem := nil;
end;

procedure TcxCustomGridColumn.CreateSubClasses;
begin
  inherited;
  FHeaderGlyph := TdxSmartGlyph.Create;
  FHeaderGlyph.OnChange := HeaderGlyphChanged;
  FSummary := GetSummaryClass.Create(Self);
end;

procedure TcxCustomGridColumn.DestroySubClasses;
begin
  FreeAndNil(FSummary);
  FreeAndNil(FHeaderGlyph);
  inherited;
end;

function TcxCustomGridColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxCustomGridColumnOptions;
end;

function TcxCustomGridColumn.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxGridColumnStyles;
end;

function TcxCustomGridColumn.GetSummaryClass: TcxGridColumnSummaryClass;
begin
  Result := TcxGridColumnSummary;
end;

procedure TcxCustomGridColumn.AssignColumnWidths;
begin
  with GridView do
    if OptionsView.ColumnAutoWidth then
      ViewInfo.HeaderViewInfo.AssignColumnWidths;
end;

function TcxCustomGridColumn.CanCellMerging: Boolean;
begin
  Result := Options.CellMerging and GridView.CanCellMerging;
end;

function TcxCustomGridColumn.CanCreateLayoutItem: Boolean;
begin
  Result := not IsDestroying and (LayoutItem = nil) and not IsPreview and IsVisibleForEditForm
    and InplaceEditForm.CanCreateLayoutItem;
end;

function TcxCustomGridColumn.CanDataCellScroll: Boolean;
begin
  Result := inherited CanDataCellScroll or InplaceEditForm.Visible;
end;

function TcxCustomGridColumn.CanEdit: Boolean;
begin
  if Controller.IsFilterRowFocused then
    Result := CanFocus(Controller.FocusedRecord) and (FocusedCellViewInfo <> nil)
  else
    Result := inherited CanEdit and not CanCellMerging;
end;

function TcxCustomGridColumn.CanFilter(AVisually: Boolean): Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanFilter(AVisually);
end;

function TcxCustomGridColumn.CanFilterMRUValueItems: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanFilterMRUValueItems;
end;

function TcxCustomGridColumn.CanFilterUsingChecks: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanFilterUsingChecks;
end;

function TcxCustomGridColumn.CanFocus(ARecord: TcxCustomGridRecord): Boolean;
begin
  if ViewData.HasFilterRow and (ARecord = ViewData.FilterRow) then
    Result := ActuallyVisible and CanFilter(False)
  else
    Result := inherited CanFocus(ARecord);
end;

function TcxCustomGridColumn.CanFocusInplaceEditFormItem(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := Assigned(ARecord) and ARecord.IsData and
    TcxGridDataRow(ARecord).EditFormVisible and not IsPreview;
end;

function TcxCustomGridColumn.CanGroup: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanGroup;
end;

function TcxCustomGridColumn.CanHorzSize: Boolean;
begin
  Result := inherited CanHorzSize and GridView.OptionsCustomize.ColumnHorzSizing;
end;

function TcxCustomGridColumn.CanIncSearch: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanIncSearch;
end;

function TcxCustomGridColumn.CanScroll: Boolean;
begin
  Result := inherited CanScroll and not InplaceEditForm.Visible;
end;

function TcxCustomGridColumn.CanShowGroupFooters: Boolean;
begin
  Result := Options.GroupFooters;
end;

function TcxCustomGridColumn.CanSort: Boolean;
begin
  Result := not InplaceEditForm.Visible and inherited CanSort;
end;

procedure TcxCustomGridColumn.CaptionChanged;
begin
  if LayoutItem <> nil then
    LayoutItem.Caption := Caption;
  inherited CaptionChanged;
end;

procedure TcxCustomGridColumn.CheckAccessibilityForEditForm;
begin
  if IsLoading then
    Exit;
  if not IsVisibleForEditForm then
    DestroyLayoutItem
  else
    if CanCreateLayoutItem then
      CreateNewLayoutItem;
end;

procedure TcxCustomGridColumn.DoSetVisible(Value: Boolean);
begin
  inherited DoSetVisible(Value);
  CheckAccessibilityForEditForm;
end;

procedure TcxCustomGridColumn.CreateNewLayoutItem;
begin
  LayoutItem := InplaceEditForm.CreateLayoutItemForGridItem(Self)
end;

procedure TcxCustomGridColumn.DestroyLayoutItem;
begin
  LayoutItem := nil;
end;

procedure TcxCustomGridColumn.ForceWidth(Value: Integer);
begin
  AssignColumnWidths;
  inherited;
  AssignColumnWidths;
  Changed(ticSize);
end;

function TcxCustomGridColumn.GetEditValue: Variant;
begin
  if Controller.IsFilterRowFocused then
  begin
    Result := ViewData.FilterRow.Values[Index];
    if UseBeginWithMask then
      Controller.RemoveBeginsWithMask(Result);
  end
  else
    Result := inherited GetEditValue;
end;

procedure TcxCustomGridColumn.SetEditValue(const Value: Variant);
var
  AValue: Variant;
begin
  if Controller.IsFilterRowFocused then
  begin
    AValue := Value;
    if UseBeginWithMask then
      Controller.AddBeginsWithMask(AValue);
    ViewData.FilterRow.Values[Index] := AValue;
    if not Controller.EditingController.ApplyingImmediateFiltering then
      GridView.Filtering.AddFilterToMRUItems;
  end
  else
    inherited SetEditValue(Value);
end;

function TcxCustomGridColumn.GetImageComboBoxProperties: TcxImageComboBoxProperties;
begin
  Result := GetProperties as TcxImageComboBoxProperties;
end;

function TcxCustomGridColumn.GetIsBottom: Boolean;
begin
  Result := True;
end;

function TcxCustomGridColumn.GetIsLeft: Boolean;
begin
  Result := IsFirst;
end;

function TcxCustomGridColumn.GetIsMostBottom: Boolean;
begin
  Result := IsBottom;
end;

function TcxCustomGridColumn.GetIsMostLeft: Boolean;
begin
  Result := IsLeft;
end;

function TcxCustomGridColumn.GetIsMostRight: Boolean;
begin
  Result := IsRight;
end;

function TcxCustomGridColumn.GetIsRight: Boolean;
begin
  Result := IsLast;
end;

function TcxCustomGridColumn.GetIsTop: Boolean;
begin
  Result := True;
end;

function TcxCustomGridColumn.GetVisible: Boolean;
begin
  Result := inherited GetVisible and not IsPreview;
end;

function TcxCustomGridColumn.GetVisibleForCustomization: Boolean;
begin
  Result := inherited GetVisibleForCustomization and not IsPreview;
end;

function TcxCustomGridColumn.HasFixedWidth: Boolean;
begin
  Result := not Options.HorzSizing;
end;

function TcxCustomGridColumn.HideOnGrouping: Boolean;
begin
  Result := GridView.OptionsCustomize.ColumnHidingOnGrouping;
end;

procedure TcxCustomGridColumn.InternalSetFilterRowOperator(AValue: TcxFilterOperatorKind);
begin
  FFilterRowOperator := AValue;
end;

function TcxCustomGridColumn.InternalGetFilterRowOperator: TcxFilterOperatorKind;
begin
  Result := FFilterRowOperator;
end;

function TcxCustomGridColumn.IsFilterOperatorSupported(AOperator: TcxFilterOperatorKind): Boolean;
var
  AFilterHelper: TcxCustomFilterEditHelperClass;
  AFilterControlOperator: TcxFilterControlOperator;
  AFilterControlSupportedOperators: TcxFilterControlOperators;
begin
  AFilterHelper := FilterEditsController.FindHelper(GetProperties.ClassType);
  Result := AFilterHelper = nil;
  if Result then
    Exit;
  AFilterControlOperator := GetFilterControlOperator(AOperator, False);
  AFilterControlSupportedOperators := AFilterHelper.GetSupportedFilterOperators(GetProperties, DataBinding.ValueTypeClass, True);
  if not DataController.Filter.SupportedLike then
    AFilterControlSupportedOperators := AFilterControlSupportedOperators - [fcoLike, fcoNotLike,
      fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
  Result := AFilterControlOperator in AFilterControlSupportedOperators;
end;

function TcxCustomGridColumn.IsFilterRowIncrementalFiltering: Boolean;
begin
  Result := GridView.FilterRow.ApplyChanges in [fracImmediately, fracDelayed];
end;

function TcxCustomGridColumn.IsFocusedCellViewInfoPartVisible: Boolean;
var
  ARowViewInfo: TcxGridDataRowViewInfo;
  ARowsViewInfo: TcxGridRowsViewInfo;
begin
  Result := inherited IsFocusedCellViewInfoPartVisible;
  if not Result and InplaceEditForm.Visible then
  begin
    ARowViewInfo := TcxGridDataRowViewInfo(Controller.FocusedRecord.ViewInfo);
    if ARowViewInfo <> nil then
      Result := ARowViewInfo.IsInplaceEditFormCellPartVisible(FocusedCellViewInfo);
  end;
  if not Result and GridView.IsFixedGroupsMode and Controller.IsRecordPixelScrolling and
    not Controller.FocusedRow.IsSpecial then
  begin
    ARowsViewInfo := GridView.ViewInfo.RecordsViewInfo;
    Result := ARowsViewInfo.IsCellPartVisibleForFixedGroupsMode(FocusedCellViewInfo);
  end;
end;

function TcxCustomGridColumn.IsLayoutItemStored: Boolean;
begin
  Result := not GridView.EditForm.UseDefaultLayout and (LayoutItem <> nil);
end;

function TcxCustomGridColumn.IsVisibleForRecordChange: Boolean;
begin
  Result := inherited IsVisibleForRecordChange;
  if not Result and GridView.InplaceEditForm.Visible then
    Result := IsVisibleForEditForm;
end;

function TcxCustomGridColumn.IsVisibleStored: Boolean;
begin
  Result := inherited IsVisibleStored and not IsPreview;
end;

function TcxCustomGridColumn.IsVisibleForCustomizationStored: Boolean;
begin
  Result := inherited IsVisibleForCustomizationStored and not IsPreview;
end;

procedure TcxCustomGridColumn.SetGridView(Value: TcxCustomGridTableView);
begin
  if GridView <> nil then
    DestroyLayoutItem;
  inherited;
  if (GridView <> nil) and CanCreateLayoutItem then
    CreateNewLayoutItem;
end;

function TcxCustomGridColumn.ShowGroupValuesWithImages: Boolean;
begin
  Result := Options.ShowGroupValuesWithImages and (GetProperties is TcxImageComboBoxProperties);
end;

function TcxCustomGridColumn.SupportsBeginsWithFilterOperator(ARow: TcxCustomGridRow): Boolean;
var
  AProperties: TcxCustomEditProperties;
  AFilterHelper: TcxCustomFilterEditHelperClass;
begin
  if ARow = nil then
    AProperties := GetProperties
  else
    AProperties := GetProperties(ARow);
  AFilterHelper := FilterEditsController.FindHelper(AProperties.ClassType);
  Result := (AFilterHelper <> nil) and
    (fcoLike in AFilterHelper.GetSupportedFilterOperators(AProperties, DataBinding.ValueTypeClass));
end;

function TcxCustomGridColumn.UseBeginWithMask: Boolean;
begin
  Result := not GridView.FilterRow.OperatorCustomization and IsFilterRowIncrementalFiltering and
    SupportsBeginsWithFilterOperator(ViewData.FilterRow);
end;

{procedure TcxCustomGridColumn.VisibleChanged;
begin
  //FGridView.RefreshVisibleColumnsList;
  //FGridView.RefreshCustomizationForm;
end;}

function TcxCustomGridColumn.GetHeaderViewInfoClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridColumnHeaderViewInfo;
end;

function TcxCustomGridColumn.HasGlyph: Boolean;
begin
  Result := not HeaderGlyph.Empty or
    IsImageAssigned(GridView.GetImages, HeaderImageIndex);
end;

function TcxCustomGridColumn.IsVisibleForEditForm: Boolean;
begin
  Result := dxDefaultBooleanToBoolean(VisibleForEditForm, Visible);
end;

function TcxCustomGridColumn.GroupBy(AGroupIndex: Integer; ACanShow: Boolean = True; AMergeWithLeftColumn: Boolean = False;
  AMergeWithRightColumn: Boolean = False): Boolean;
begin
  Result := CanGroup;
  if not Result then Exit;
  GridView.BeginGroupingUpdate;
  try
    ChangeGrouping(AGroupIndex, AMergeWithLeftColumn, AMergeWithRightColumn);
    if AGroupIndex = -1 then
      if ACanShow and ShowOnUngrouping and WasVisibleBeforeGrouping then
        Visible := True
      else
    else
      if HideOnGrouping and CanHide then
        Visible := False;
  finally
    GridView.EndGroupingUpdate;
  end;
end;

{ TcxGridColumn }

destructor TcxGridColumn.Destroy;
begin
  Selected := False;
  IsPreview := False;
  inherited Destroy;
end;

function TcxGridColumn.DoCompareValuesForCellMerging(
  ARow1: TcxGridDataRow; AProperties1: TcxCustomEditProperties; const AValue1: TcxEditValue;
  ARow2: TcxGridDataRow; AProperties2: TcxCustomEditProperties; const AValue2: TcxEditValue): Boolean;
begin
  Result := (AProperties1 = AProperties2) and AProperties1.CompareDisplayValues(AValue1, AValue2);
  if Assigned(FOnCompareValuesForCellMerging) then
    FOnCompareValuesForCellMerging(Self, AProperties1, AValue1, AProperties2, AValue2, Result);
  if Assigned(FOnCompareRowValuesForCellMerging) then
    FOnCompareRowValuesForCellMerging(Self, ARow1, AProperties1, AValue1,
      ARow2, AProperties2, AValue2, Result);
end;

procedure TcxGridColumn.FocusWithSelection;
begin
  if not Focused then
  begin
    GridView.BeginUpdate;
    try
      Controller.ClearCellSelection;
      Selected := True;
      Controller.CellSelectionAnchor := Self;
    finally
      GridView.EndUpdate;
    end;
  end;
  inherited;
end;

procedure TcxGridColumn.Assign(Source: TPersistent);

  function FindItem: TcxGridInplaceEditFormLayoutItem;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to GridView.InplaceEditForm.Container.AbsoluteItemCount - 1 do
      if (GridView.InplaceEditForm.Container.AbsoluteItems[I] is TcxGridInplaceEditFormLayoutItem) and
        (TcxGridInplaceEditFormLayoutItem(GridView.InplaceEditForm.Container.AbsoluteItems[I]).GridViewItem = Source) then
      begin
        Result := TcxGridInplaceEditFormLayoutItem(GridView.InplaceEditForm.Container.AbsoluteItems[I]);
        Break;
      end;
  end;

var
  ALayoutItem: TcxGridInplaceEditFormLayoutItem;
begin
  if Source is TcxGridColumn then
    with TcxGridColumn(Source) do
    begin
      Self.FooterAlignmentHorz := FooterAlignmentHorz;
      Self.GroupSummaryAlignment := GroupSummaryAlignment;
      Self.HeaderGlyph := HeaderGlyph;
      Self.HeaderGlyphAlignmentHorz := HeaderGlyphAlignmentHorz;
      Self.HeaderGlyphAlignmentVert := HeaderGlyphAlignmentVert;
      Self.HeaderImageIndex := HeaderImageIndex;
      Self.InternalSetFilterRowOperator(InternalGetFilterRowOperator);
      Self.Summary := Summary;
      Self.VisibleForEditForm := VisibleForEditForm;
      Self.IsChildInMergedGroup := IsChildInMergedGroup;
      Self.OnCompareRowValuesForCellMerging := OnCompareRowValuesForCellMerging;
      Self.OnCompareValuesForCellMerging := OnCompareValuesForCellMerging;
      Self.OnCustomDrawFooterCell := OnCustomDrawFooterCell;
      Self.OnCustomDrawGroupSummaryCell := OnCustomDrawGroupSummaryCell;
      Self.OnCustomDrawHeader := OnCustomDrawHeader;
      Self.OnHeaderClick := OnHeaderClick;
      ALayoutItem := FindItem;
      if ALayoutItem <> nil then
        Self.LayoutItem := ALayoutItem;
    end;
  inherited Assign(Source);
end;

procedure TcxGridColumn.BestFitApplied(AFireEvents: Boolean);
begin
  inherited;
  if AFireEvents then
    GridView.DoColumnSizeChanged(Self);
end;

function TcxGridColumn.CalculateBestFitWidth: Integer;
var
  ABorders: TcxBorders;
begin
  Result := inherited CalculateBestFitWidth;
  ABorders := GridView.ViewInfo.GetCellBorders(IsMostRight, False);
  Inc(Result, (Ord(bLeft in ABorders) + Ord(bRight in ABorders)) * GridView.ViewInfo.GridLineWidth);
  if (VisibleIndex <> -1) and GridView.Visible then
  begin
    if GridView.OptionsView.Header then
      Result := Max(Result, GridView.ViewInfo.HeaderViewInfo[VisibleIndex].GetBestFitWidth);
    if GridView.OptionsView.Footer then
      Result := Max(Result, GridView.ViewInfo.FooterViewInfo.GetCellBestFitWidth(Self));
    Result := Max(Result, GridView.ViewInfo.RecordsViewInfo.GetFooterCellBestFitWidth(Self));
  end;
end;

function TcxGridColumn.GetFixed: Boolean;
begin
  Result := inherited GetFixed or
    (Controller.ForcingWidthItem <> nil) and
    Controller.IsColumnFixedDuringHorzSizing(Self);
end;

function TcxGridColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridColumnOptions;
end;

procedure TcxGridColumn.DoCustomDrawFooterCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawFooterCell then
    FOnCustomDrawFooterCell(GridView, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridColumn.DoCustomDrawHeader(ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawHeader then
    FOnCustomDrawHeader(GridView, ACanvas, AViewInfo, ADone);
end;

function TcxGridColumn.HasCustomDrawFooterCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawFooterCell);
end;

function TcxGridColumn.HasCustomDrawGroupSummaryCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawGroupSummaryCell);
end;

function TcxGridColumn.HasCustomDrawHeader: Boolean;
begin
  Result := Assigned(FOnCustomDrawHeader);
end;

procedure TcxGridColumn.DoCustomDrawGroupSummaryCell(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean);
var
  ACell: TcxGridGroupSummaryCellViewInfo;
begin
  if HasCustomDrawGroupSummaryCell then
  begin
    ACell := TcxGridGroupSummaryCellViewInfo(AViewInfo);
    FOnCustomDrawGroupSummaryCell(Self, ACanvas, ACell.RowViewInfo.GridRecord,
      Self, ACell.SummaryItem, AViewInfo, ADone);
  end;
end;

procedure TcxGridColumn.DoHeaderClick;
begin
  if Assigned(FOnHeaderClick) then FOnHeaderClick(Self);
  GridView.DoColumnHeaderClick(Self);
end;

function TcxGridColumn.GetIsPreview: Boolean;
begin
  Result := inherited IsPreview;
end;

function TcxGridColumn.GetOptions: TcxGridColumnOptions;
begin
  Result := TcxGridColumnOptions(inherited GetOptions);
end;

function TcxGridColumn.GetSelected: Boolean;
begin
  Result := inherited Selected;
end;

procedure TcxGridColumn.SetIsPreview(Value: Boolean);
begin
  if IsPreview <> Value then
    if Value then
      GridView.Preview.Column := Self
    else
      GridView.Preview.Column := nil;
end;

procedure TcxGridColumn.SetOnCompareRowValuesForCellMerging(Value: TcxGridColumnCompareRowValuesEvent);
begin
  if not dxSameMethods(FOnCompareRowValuesForCellMerging, Value) then
  begin
    FOnCompareRowValuesForCellMerging := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOnCompareValuesForCellMerging(Value: TcxGridColumnCompareValuesEvent);
begin
  if not dxSameMethods(FOnCompareValuesForCellMerging, Value) then
  begin
    FOnCompareValuesForCellMerging := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOnCustomDrawFooterCell(Value: TcxGridColumnCustomDrawHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawFooterCell, Value) then
  begin
    FOnCustomDrawFooterCell := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOnCustomDrawGroupSummaryCell(Value: TcxGridGroupSummaryCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawGroupSummaryCell, Value) then
  begin
    FOnCustomDrawGroupSummaryCell := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOnCustomDrawHeader(Value: TcxGridColumnCustomDrawHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawHeader, Value) then
  begin
    FOnCustomDrawHeader := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOnHeaderClick(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnHeaderClick, Value) then
  begin
    FOnHeaderClick := Value;
    Changed(ticProperty);
  end;
end;

procedure TcxGridColumn.SetOptions(Value: TcxGridColumnOptions);
begin
  inherited SetOptions(Value);
end;

procedure TcxGridColumn.SetSelected(Value: Boolean);
begin
  if Selected <> Value then
    if Value then
      Controller.AddSelectedColumn(Self)
    else
      Controller.RemoveSelectedColumn(Self);
end;

{ TcxGridTableBackgroundBitmaps }

function TcxGridTableBackgroundBitmaps.GetBitmapStyleIndex(Index: Integer): Integer;
begin
  case Index of
    bbFooter:
      Result := vsFooter;
    bbHeader:
      Result := vsHeader;
    bbGroup:
      Result := vsGroup;
    bbGroupByBox:
      Result := vsGroupByBox;
    bbIndicator:
      Result := vsIndicator;
    bbPreview:
      Result := vsPreview;
  else
    Result := inherited GetBitmapStyleIndex(Index);
  end;
end;

procedure TcxGridTableBackgroundBitmaps.Assign(Source: TPersistent);
begin
  if Source is TcxGridTableBackgroundBitmaps then
    with TcxGridTableBackgroundBitmaps(Source) do
    begin
      Self.Footer := Footer;
      Self.Header := Header;
      Self.Group := Group;
      Self.GroupByBox := GroupByBox;
      Self.Indicator := Indicator;
      Self.Preview := Preview;
    end;
  inherited;
end;

{ TcxGridTableViewNavigatorButtons }

function TcxGridTableViewNavigatorButtons.GetButtonEnabled(ADefaultIndex: Integer): Boolean;
begin
  if (ADefaultIndex = NBDI_FILTER) and GridView.InplaceEditForm.Visible then
    Result := False
  else
    Result := inherited GetButtonEnabled(ADefaultIndex);
end;

function TcxGridTableViewNavigatorButtons.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

{ TcxGridTableViewNavigator }

function TcxGridTableViewNavigator.GetNavigatorButtonsClass: TcxGridViewNavigatorButtonsClass;
begin
  Result := TcxGridTableViewNavigatorButtons;
end;

{ TcxGridTableOptionsBehavior }

constructor TcxGridTableOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FColumnHeaderHints := True;
  FCopyPreviewToClipboard := True;
  FEditMode := emInplace;
  FExpandMasterRowOnDblClick := True;
end;

function TcxGridTableOptionsBehavior.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableOptionsBehavior.GetShowLockedStateImageOptions: TcxGridTableShowLockedStateImageOptions;
begin
  Result := TcxGridTableShowLockedStateImageOptions(inherited ShowLockedStateImageOptions);
end;

function TcxGridTableOptionsBehavior.GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass;
begin
  Result := TcxGridTableShowLockedStateImageOptions;
end;

procedure TcxGridTableOptionsBehavior.SetColumnHeaderHints(Value: Boolean);
begin
  if FColumnHeaderHints <> Value then
  begin
    FColumnHeaderHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsBehavior.SetColumnMergedGrouping(Value: Boolean);
begin
  if ColumnMergedGrouping <> Value then
  begin
    FColumnMergedGrouping := Value and GridView.IsMergedGroupsSupported;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsBehavior.SetCopyPreviewToClipboard(Value: Boolean);
begin
  if FCopyPreviewToClipboard <> Value then
  begin
    FCopyPreviewToClipboard := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsBehavior.SetEditMode(AValue: TcxGridEditMode);
var
  AGridViewLink: TcxObjectLink;
begin
  if AValue <> FEditMode then
  begin
    if GridView.InplaceEditForm.Visible and not GridView.InplaceEditForm.Close then
        Exit;
    FEditMode := AValue;
    if not IsInplaceEditFormMode then
      GridView.EditForm.MasterRowDblClickAction := dcaSwitchExpandedState;
    if Assigned(GridView) then
    begin
      AGridViewLink := cxAddObjectLink(GridView);
      try
        GridView.Controller.EditingController.HideEdit(False);
        if AGridViewLink.Ref <> nil then
          GridView.DataController.Cancel;
        if AGridViewLink.Ref <> nil then
          GridView.Changed(vcSize);
      finally
        cxRemoveObjectLink(AGridViewLink);
      end;
    end;
  end;
end;

procedure TcxGridTableOptionsBehavior.SetExpandMasterRowOnDblClick(Value: Boolean);
begin
  if FExpandMasterRowOnDblClick <> Value then
  begin
    FExpandMasterRowOnDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsBehavior.SetFixedGroups(Value: Boolean);
begin
  if FFixedGroups <> Value then
  begin
    FFixedGroups := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsBehavior.SetShowLockedStateImageOptions(
  Value: TcxGridTableShowLockedStateImageOptions);
begin
  inherited ShowLockedStateImageOptions.Assign(Value);
end;

procedure TcxGridTableOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxGridTableOptionsBehavior then
    with TcxGridTableOptionsBehavior(Source) do
    begin
      Self.ColumnHeaderHints := ColumnHeaderHints;
      Self.ColumnMergedGrouping := ColumnMergedGrouping;
      Self.CopyPreviewToClipboard := CopyPreviewToClipboard;
      Self.EditMode := EditMode;
      Self.ExpandMasterRowOnDblClick := ExpandMasterRowOnDblClick;
      Self.FixedGroups := FixedGroups;
    end;
  inherited Assign(Source);
end;

function TcxGridTableOptionsBehavior.IsInplaceEditFormMode: Boolean;
begin
  Result := EditMode in [emInplaceEditForm, emInplaceEditFormHideCurrentRow];
end;

function TcxGridTableOptionsBehavior.NeedHideCurrentRow: Boolean;
begin
  Result := EditMode in [emInplaceEditFormHideCurrentRow];
end;

{ TcxGridTableFiltering }

procedure TcxGridTableFiltering.RunCustomizeDialog(AItem: TcxCustomGridTableItem = nil);
begin
  if GridView.InplaceEditForm.Visible then
    Exit;
  inherited RunCustomizeDialog(AItem);
end;

function TcxGridTableFiltering.GetColumnAddValueItems: Boolean;
begin
  Result := ItemAddValueItems;
end;

function TcxGridTableFiltering.GetColumnExcelPopup: TcxGridItemExcelFilterPopupOptions;
begin
  Result := ItemExcelPopup;
end;

function TcxGridTableFiltering.GetColumnFilteredItemsList: Boolean;
begin
  Result := ItemFilteredItemsList;
end;

function TcxGridTableFiltering.GetColumnMRUItemsList: Boolean;
begin
  Result := ItemMRUItemsList;
end;

function TcxGridTableFiltering.GetColumnMRUItemsListCount: Integer;
begin
  Result := ItemMRUItemsListCount;
end;

function TcxGridTableFiltering.GetColumnPopup: TcxGridItemFilterPopupOptions;
begin
  Result := ItemPopup;
end;

function TcxGridTableFiltering.GetColumnPopupMode: TdxFilterPopupWindowMode;
begin
  Result := ItemPopupMode;
end;

function TcxGridTableFiltering.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridTableFiltering.SetColumnAddValueItems(Value: Boolean);
begin
  ItemAddValueItems := Value;
end;

procedure TcxGridTableFiltering.SetColumnExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
begin
  ItemExcelPopup := Value;
end;

procedure TcxGridTableFiltering.SetColumnFilteredItemsList(Value: Boolean);
begin
  ItemFilteredItemsList := Value;
end;

procedure TcxGridTableFiltering.SetColumnMRUItemsList(Value: Boolean);
begin
  ItemMRUItemsList := Value;
end;

procedure TcxGridTableFiltering.SetColumnMRUItemsListCount(Value: Integer);
begin
  ItemMRUItemsListCount := Value;
end;

procedure TcxGridTableFiltering.SetColumnPopup(Value: TcxGridItemFilterPopupOptions);
begin
  ItemPopup := Value;
end;

procedure TcxGridTableFiltering.SetColumnPopupMode(Value: TdxFilterPopupWindowMode);
begin
  ItemPopupMode := Value;
end;

procedure TcxGridTableFiltering.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ColumnPopupDropDownWidth', ReadItemPopupDropDownWidth, nil, True);
  Filer.DefineProperty('ColumnPopupMaxDropDownItemCount', ReadItemPopupMaxDropDownCount, nil, True);
end;

function TcxGridTableFiltering.IsFilterBoxEnabled: Boolean;
begin
  Result := not GridView.InplaceEditForm.Visible;
end;

{ TcxGridTableOptionsCustomize }

constructor TcxGridTableOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FColumnHidingOnGrouping := True;
  FColumnHorzSizing := True;
end;

function TcxGridTableOptionsCustomize.GetColumnFiltering: Boolean;
begin
  Result := ItemFiltering;
end;

function TcxGridTableOptionsCustomize.GetColumnGrouping: Boolean;
begin
  Result := ItemGrouping;
end;

function TcxGridTableOptionsCustomize.GetColumnHiding: Boolean;
begin
  Result := ItemHiding;
end;

function TcxGridTableOptionsCustomize.GetColumnMoving: Boolean;
begin
  Result := ItemMoving;
end;

function TcxGridTableOptionsCustomize.GetColumnSorting: Boolean;
begin
  Result := ItemSorting;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomization: Boolean;
begin
  Result := ItemsQuickCustomization;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomizationMaxDropDownCount: Integer;
begin
  Result := ItemsQuickCustomizationMaxDropDownCount;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomizationMultiColumnMode: Boolean;
begin
  Result := ItemsQuickCustomizationMultiColumnMode;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomizationReordering: TcxGridQuickCustomizationReordering;
begin
  Result := ItemsQuickCustomizationReordering;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomizationShowCommands: Boolean;
begin
  Result := ItemsQuickCustomizationShowCommands;
end;

function TcxGridTableOptionsCustomize.GetColumnsQuickCustomizationSorted: Boolean;
begin
  Result := ItemsQuickCustomizationSorted;
end;

function TcxGridTableOptionsCustomize.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridTableOptionsCustomize.SetColumnFiltering(Value: Boolean);
begin
  ItemFiltering := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnGrouping(Value: Boolean);
begin
  ItemGrouping := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnHiding(Value: Boolean);
begin
  ItemHiding := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnHidingOnGrouping(Value: Boolean);
begin
  if FColumnHidingOnGrouping <> Value then
  begin
    FColumnHidingOnGrouping := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsCustomize.SetColumnHorzSizing(Value: Boolean);
begin
  if FColumnHorzSizing <> Value then
  begin
    FColumnHorzSizing := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsCustomize.SetColumnMoving(Value: Boolean);
begin
  ItemMoving := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnSorting(Value: Boolean);
begin
  ItemSorting := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomization(Value: Boolean);
begin
  ItemsQuickCustomization := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomizationMaxDropDownCount(Value: Integer);
begin
  ItemsQuickCustomizationMaxDropDownCount := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomizationMultiColumnMode(Value: Boolean);
begin
  ItemsQuickCustomizationMultiColumnMode := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
begin
  ItemsQuickCustomizationReordering := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomizationShowCommands(Value: Boolean);
begin
  ItemsQuickCustomizationShowCommands := Value;
end;

procedure TcxGridTableOptionsCustomize.SetColumnsQuickCustomizationSorted(Value: Boolean);
begin
  ItemsQuickCustomizationSorted := Value;
end;

procedure TcxGridTableOptionsCustomize.SetDataRowFixing(Value: Boolean);
begin
  if not GridView.IsDataRowFixingSupported then
    Value := False;
  if Value <> DataRowFixing then
  begin
    FDataRowFixing := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsCustomize.SetDataRowSizing(Value: Boolean);
begin
  if FDataRowSizing <> Value then
  begin
    FDataRowSizing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsCustomize.SetGroupBySorting(Value: Boolean);
begin
  if FGroupBySorting <> Value then
  begin
    FGroupBySorting := Value;
    GridView.BeginUpdate;
    try
      GridView.Controller.ClearGrouping;
      GridView.FDataController.ClearSorting(False);
      Changed(vcProperty);
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridTableOptionsCustomize.SetGroupRowSizing(Value: Boolean);
begin
  if FGroupRowSizing <> Value then
  begin
    FGroupRowSizing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsCustomize.Assign(Source: TPersistent);
begin
  if Source is TcxGridTableOptionsCustomize then
    with TcxGridTableOptionsCustomize(Source) do
    begin
      Self.ColumnHidingOnGrouping := ColumnHidingOnGrouping;
      Self.ColumnHorzSizing := ColumnHorzSizing;
      Self.DataRowFixing := DataRowFixing;
      Self.DataRowSizing := DataRowSizing;
      Self.GroupBySorting := GroupBySorting;
      Self.GroupRowSizing := GroupRowSizing;
      Self.ColumnsQuickCustomization := ColumnsQuickCustomization;
      Self.ColumnsQuickCustomizationMaxDropDownCount := ColumnsQuickCustomizationMaxDropDownCount;
      Self.ColumnsQuickCustomizationMultiColumnMode := ColumnsQuickCustomizationMultiColumnMode;
      Self.ColumnsQuickCustomizationReordering := ColumnsQuickCustomizationReordering;
      Self.ColumnsQuickCustomizationSorted := ColumnsQuickCustomizationSorted;
    end;
  inherited;
end;

{ TcxGridTableOptionsSelection }

function TcxGridTableOptionsSelection.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridTableOptionsSelection.SetCellMultiSelect(Value: Boolean);
begin
  if CellMultiSelect <> Value then
  begin
    FCellMultiSelect := Value;
    if Value or not IsLoading then
    begin
      CellSelect := True;
      InvertSelect := not Value;
      MultiSelect := Value;
    end;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsSelection.SetCheckBoxPosition(Value: TcxGridCheckBoxPosition);
begin
  if CheckBoxPosition <> Value then
  begin
    FCheckBoxPosition := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsSelection.SetCheckBoxVisibility(Value: TcxGridCheckBoxVisibility);
begin
  if (CheckBoxVisibility <> Value) and (IsLoading or GridView.IsCheckBoxSelectionSupported or (Value = [])) then
  begin
    FCheckBoxVisibility := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsSelection.SetClearPersistentSelectionOnOutsideClick(Value: Boolean);
begin
  if ClearPersistentSelectionOnOutsideClick <> Value then
  begin
    FClearPersistentSelectionOnOutsideClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableOptionsSelection.SetMultiSelectMode(Value: TcxGridMultiSelectMode);
begin
  if MultiSelectMode <> Value then
  begin
    FMultiSelectMode := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsSelection.SetShowCheckBoxesDynamically(Value: Boolean);
begin
  if ShowCheckBoxesDynamically <> Value then
  begin
    FShowCheckBoxesDynamically := Value;
    Changed(vcLayout);
  end;
end;

function TcxGridTableOptionsSelection.IsMultiSelectPersistent: Boolean;
begin
  Result := MultiSelect and not CellMultiSelect and (MultiSelectMode = msmPersistent) and
    GridView.IsPersistentMultiSelectSupported;
end;

procedure TcxGridTableOptionsSelection.SetCellSelect(Value: Boolean);
begin
  if IsLoading or not CellMultiSelect or Value then
    inherited;
end;

procedure TcxGridTableOptionsSelection.SetInvertSelect(Value: Boolean);
begin
  if IsLoading or not CellMultiSelect or not Value then
    inherited;
end;

procedure TcxGridTableOptionsSelection.SetMultiSelect(Value: Boolean);
begin
  if IsLoading or not CellMultiSelect or Value then
    inherited;
end;

procedure TcxGridTableOptionsSelection.Assign(Source: TPersistent);
begin
  if Source is TcxGridTableOptionsSelection then
    with TcxGridTableOptionsSelection(Source) do
    begin
      Self.CellMultiSelect := CellMultiSelect;
      Self.CheckBoxPosition := CheckBoxPosition;
      Self.CheckBoxVisibility := CheckBoxVisibility;
      Self.ClearPersistentSelectionOnOutsideClick := ClearPersistentSelectionOnOutsideClick;
      Self.MultiSelectMode := MultiSelectMode;
      Self.ShowCheckBoxesDynamically := ShowCheckBoxesDynamically;
    end;
  inherited;
end;

{ TcxGridFixedDataRowsOptions }

constructor TcxGridFixedDataRowsOptions.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  CreatePinSize;
  FPinClickAction := rpcaShowPopup;
  FSeparatorColor := clDefault;
  FSeparatorWidth := cxGridCustomRowSeparatorDefaultWidth;
end;

destructor TcxGridFixedDataRowsOptions.Destroy;
begin
  DestroyPinSize;
  inherited Destroy;
end;

procedure TcxGridFixedDataRowsOptions.Assign(Source: TPersistent);
var
  ASource: TcxGridFixedDataRowsOptions;
begin
  inherited Assign(Source);
  if Source is TcxGridFixedDataRowsOptions then
  begin
    ASource := TcxGridFixedDataRowsOptions(Source);
    PinClickAction := ASource.PinClickAction;
    PinSize := ASource.PinSize;
    PinVisibility := ASource.PinVisibility;
    SeparatorColor := ASource.SeparatorColor;
    SeparatorWidth := ASource.SeparatorWidth;
  end;
end;

procedure TcxGridFixedDataRowsOptions.ChangeScale(M, D: Integer);
begin
  inherited;
  SeparatorWidth := MulDiv(SeparatorWidth, M, D);
  PinSize.ChangeScale(M, D);
end;

function TcxGridFixedDataRowsOptions.GetSeparatorColor: TColor;
begin
  Result := FSeparatorColor;
  if Result = clDefault then
    Result := DefaultSeparatorColor;
end;

function TcxGridFixedDataRowsOptions.DefaultSeparatorColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

function TcxGridFixedDataRowsOptions.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridFixedDataRowsOptions.SetPinClickAction(AValue: TcxGridDataRowPinClickAction);
begin
  if PinClickAction <> AValue then
  begin
    FPinClickAction := AValue;
    Changed(vcLayout);
  end;
end;

procedure TcxGridFixedDataRowsOptions.SetPinSize(AValue: TcxSize);
begin
  PinSize.Assign(AValue);
end;

procedure TcxGridFixedDataRowsOptions.SetPinVisibility(AValue: TcxGridDataRowPinVisibility);
begin
  if PinVisibility <> AValue then
  begin
    FPinVisibility := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridFixedDataRowsOptions.SetSeparatorColor(AValue: TColor);
begin
  if FSeparatorColor <> AValue  then
  begin
    FSeparatorColor := AValue;
    Changed(vcLayout);
  end;
end;

procedure TcxGridFixedDataRowsOptions.SetSeparatorWidth(AValue: Integer);
begin
  if AValue < cxGridCustomRowSeparatorMinWidth then
    AValue := cxGridCustomRowSeparatorMinWidth;
  if FSeparatorWidth <> AValue then
  begin
    FSeparatorWidth := AValue;
    Changed(vcSize);
  end;
end;

procedure TcxGridFixedDataRowsOptions.CreatePinSize;
begin
  FPinSize := TcxSize.Create(nil);
  FPinSize.OnChange := PinSizeChangeHandler;
end;

procedure TcxGridFixedDataRowsOptions.DestroyPinSize;
begin
  FreeAndNil(FPinSize);
end;

procedure TcxGridFixedDataRowsOptions.PinSizeChangeHandler(Sender: TObject);
begin
  Changed(vcSize);
end;

{ TcxGridSpecialRowOptions }

constructor TcxGridSpecialRowOptions.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FInfoText := DefaultInfoText;
  FSeparatorColor := clDefault;
  FSeparatorWidth := cxGridCustomRowSeparatorDefaultWidth;
end;

function TcxGridSpecialRowOptions.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridSpecialRowOptions.GetInfoText: string;
begin
  if FIsInfoTextAssigned then
    Result := FInfoText
  else
    Result := DefaultInfoText;
end;

procedure TcxGridSpecialRowOptions.SetInfoText(const Value: string);
begin
  if FInfoText <> Value then
  begin
    FInfoText := Value;
    FIsInfoTextAssigned := Value <> DefaultInfoText;
    Changed(vcLayout);
  end;
end;

procedure TcxGridSpecialRowOptions.SetSeparatorColor(Value: TColor);
begin
  if FSeparatorColor <> Value  then
  begin
    FSeparatorColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridSpecialRowOptions.SetSeparatorWidth(Value: Integer);
begin
  if Value < cxGridCustomRowSeparatorMinWidth then
    Value := cxGridCustomRowSeparatorMinWidth;
  if FSeparatorWidth <> Value then
  begin
    FSeparatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridSpecialRowOptions.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    VisibleChanged;
  end;
end;

function TcxGridSpecialRowOptions.IsInfoTextStored: Boolean;
begin
  Result := FIsInfoTextAssigned;
end;

procedure TcxGridSpecialRowOptions.ChangeScale(M, D: Integer);
begin
  inherited;
  SeparatorWidth := MulDiv(SeparatorWidth, M, D);
end;

function TcxGridSpecialRowOptions.DefaultSeparatorColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultHeaderColor;
end;

procedure TcxGridSpecialRowOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridSpecialRowOptions then
    with TcxGridSpecialRowOptions(Source) do
    begin
      Self.InfoText := InfoText;
      Self.SeparatorColor := SeparatorColor;
      Self.SeparatorWidth := SeparatorWidth;
      Self.Visible := Visible;
    end;
end;

function TcxGridSpecialRowOptions.GetSeparatorColor: TColor;
begin
  Result := FSeparatorColor;
  if Result = clDefault then
    Result := DefaultSeparatorColor;
end;

{ TcxGridFilterRowOptions }

constructor TcxGridFilterRowOptions.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FApplyInputDelay := cxGridFilterRowDelayDefault;
end;

procedure TcxGridFilterRowOptions.SetApplyChanges(Value: TcxGridFilterRowApplyChangesMode);
begin
  if FApplyChanges <> Value then
  begin
    FApplyChanges := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridFilterRowOptions.SetApplyInputDelay(Value: Cardinal);
begin
  if FApplyInputDelay <> Value then
  begin
    FApplyInputDelay := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridFilterRowOptions.SetOperatorCustomization(Value: Boolean);
begin
  if FOperatorCustomization <> Value then
  begin
    FOperatorCustomization := Value;
    if not OperatorCustomization and GridView.ViewData.HasFilterRow then
      GridView.ViewData.FilterRow.ResetOperators;
    Changed(vcSize);
  end;
end;

function TcxGridFilterRowOptions.DefaultInfoText: string;
begin
  Result := cxGetResourceString(@scxGridFilterRowInfoText);
end;

procedure TcxGridFilterRowOptions.VisibleChanged;
begin
  GridView.ViewData.CheckFilterRow;
  Changed(vcSize);
end;

procedure TcxGridFilterRowOptions.Assign(Source: TPersistent);
var
  ASource: TcxGridFilterRowOptions;
begin
  inherited Assign(Source);
  if Source is TcxGridFilterRowOptions then
  begin
    ASource := TcxGridFilterRowOptions(Source);
    ApplyChanges := ASource.ApplyChanges;
    ApplyInputDelay := ASource.ApplyInputDelay;
    OperatorCustomization := ASource.OperatorCustomization;
  end;
end;

{ TcxGridNewItemRowOptions }

function TcxGridNewItemRowOptions.DefaultInfoText: string;
begin
  Result := cxGetResourceString(@scxGridNewItemRowInfoText);
end;

procedure TcxGridNewItemRowOptions.VisibleChanged;
begin
  GridView.ViewData.CheckNewItemRecord;
  GridView.FDataController.UseNewItemRowForEditing := Visible;
end;

{ TcxGridTableOptionsView }

constructor TcxGridTableOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FExpandButtonsForEmptyDetails := True;
  FGridLineColor := clDefault;
  FGroupByBox := True;
  FHeader := True;
  FIndicatorWidth := cxGridDefaultIndicatorWidth;
  FPrevGroupFooters := gfVisibleWhenExpanded;
  FRowSeparatorColor := clDefault;
  FGroupByHeaderLayout := ghlVerticallyShifted;
  FMergedGroupSeparator := cxGridDefaultMergedGroupSeparator;
end;

function TcxGridTableOptionsView.GetExpandButtonsForEmptyDetails: Boolean;
begin
  Result := FExpandButtonsForEmptyDetails and
    ((GridView.Level = nil) or TcxGridLevel(GridView.Level).Options.TabsForEmptyDetails);
end;

function TcxGridTableOptionsView.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableOptionsView.GetHeaderAutoHeight: Boolean;
begin
  Result := ItemCaptionAutoHeight;
end;

function TcxGridTableOptionsView.GetHeaderEndEllipsis: Boolean;
begin
  Result := ItemCaptionEndEllipsis;
end;

function TcxGridTableOptionsView.GetHeaderFilterButtonShowMode: TcxGridItemFilterButtonShowMode;
begin
  Result := ItemFilterButtonShowMode;
end;

function TcxGridTableOptionsView.GetNewItemRow: Boolean;
begin
  Result := GridView.NewItemRow.Visible;
end;

function TcxGridTableOptionsView.GetNewItemRowInfoText: string;
begin
  Result := GridView.NewItemRow.InfoText;
end;

function TcxGridTableOptionsView.GetNewItemRowSeparatorColor: TColor;
begin
 Result := GridView.NewItemRow.SeparatorColor;
end;

function TcxGridTableOptionsView.GetNewItemRowSeparatorWidth: Integer;
begin
  Result := GridView.NewItemRow.SeparatorWidth;
end;

function TcxGridTableOptionsView.GetShowColumnFilterButtons: TcxGridShowItemFilterButtons;
begin
  Result := ShowItemFilterButtons;
end;

procedure TcxGridTableOptionsView.SetColumnAutoWidth(Value: Boolean);
begin
  if FColumnAutoWidth <> Value then
  begin
    FColumnAutoWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetDataRowHeight(Value: Integer);
begin
  if not GridView.AssigningPattern then
    CheckDataRowHeight(Value);
  if FDataRowHeight <> Value then
  begin
    FDataRowHeight := Value;
    Changed(vcSize);
    GridView.Controller.DesignerModified;
  end;
end;

procedure TcxGridTableOptionsView.SetExpandButtonsForEmptyDetails(Value: Boolean);
begin
  if FExpandButtonsForEmptyDetails <> Value then
  begin
    FExpandButtonsForEmptyDetails := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsView.SetHeaderFilterButtonShowMode(
  Value: TcxGridItemFilterButtonShowMode);
begin
  ItemFilterButtonShowMode := Value;
end;

procedure TcxGridTableOptionsView.SetFooter(Value: Boolean);
begin
  if FFooter <> Value then
  begin
    FFooter := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetFooterAutoHeight(Value: Boolean);
begin
  if FFooterAutoHeight <> Value then
  begin
    FFooterAutoHeight := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetFooterMultiSummaries(Value: Boolean);
begin
  if FFooterMultiSummaries <> Value then
  begin
    FFooterMultiSummaries := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGridLineColor(Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsView.SetGridLines(Value: TcxGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupByBox(Value: Boolean);
begin
  if FGroupByBox <> Value then
  begin
    FGroupByBox := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupByHeaderLayout(Value: TcxGridGroupByHeaderLayout);
begin
  if FGroupByHeaderLayout <> Value then
  begin
    FGroupByHeaderLayout := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupFooterMultiSummaries(Value: Boolean);
begin
  if FGroupFooterMultiSummaries <> Value then
  begin
    FGroupFooterMultiSummaries := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupFooters(Value: TcxGridGroupFootersMode);
begin
  if FGroupFooters <> Value then
  begin
    FPrevGroupFooters := FGroupFooters;
    FGroupFooters := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupRowHeight(Value: Integer);
begin
  if not GridView.AssigningPattern then
    CheckGroupRowHeight(Value);
  if FGroupRowHeight <> Value then
  begin
    FGroupRowHeight := Value;
    Changed(vcSize);
    GridView.Controller.DesignerModified;
  end;
end;

procedure TcxGridTableOptionsView.SetGroupRowStyle(Value: TcxGridGroupRowStyle);
begin
  if FGroupRowStyle <> Value then
  begin
    FGroupRowStyle := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetGroupSummaryLayout(Value: TcxGridGroupSummaryLayout);
begin
  if FGroupSummaryLayout <> Value then
  begin
    FGroupSummaryLayout := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetHeader(Value: Boolean);
begin
  if FHeader <> Value then
  begin
    FHeader := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetHeaderAutoHeight(Value: Boolean);
begin
  ItemCaptionAutoHeight := Value;
end;

procedure TcxGridTableOptionsView.SetHeaderEndEllipsis(Value: Boolean);
begin
  ItemCaptionEndEllipsis := Value;
end;

procedure TcxGridTableOptionsView.SetHeaderHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetIndicator(Value: Boolean);
begin
  if FIndicator <> Value then
  begin
    FIndicator := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetIndicatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FIndicatorWidth <> Value then
  begin
    FIndicatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetNewItemRow(Value: Boolean);
begin
  GridView.NewItemRow.Visible := Value;
end;

procedure TcxGridTableOptionsView.SetNewItemRowInfoText(const Value: string);
begin
  GridView.NewItemRow.InfoText := Value;
end;

procedure TcxGridTableOptionsView.SetNewItemRowSeparatorColor(Value: TColor);
begin
  GridView.NewItemRow.SeparatorColor := Value;
end;

procedure TcxGridTableOptionsView.SetNewItemRowSeparatorWidth(Value: Integer);
begin
  GridView.NewItemRow.SeparatorWidth := Value;
end;

procedure TcxGridTableOptionsView.SetMergedGroupSeparator(AValue: string);
begin
  if MergedGroupSeparator <> AValue then
  begin
    FMergedGroupSeparator := AValue;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsView.SetRowSeparatorColor(Value: TColor);
begin
  if FRowSeparatorColor <> Value then
  begin
    FRowSeparatorColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridTableOptionsView.SetRowSeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FRowSeparatorWidth <> Value then
  begin
    FRowSeparatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridTableOptionsView.SetShowColumnFilterButtons(Value: TcxGridShowItemFilterButtons);
begin
  ShowItemFilterButtons := Value;
end;

procedure TcxGridTableOptionsView.ReadNewItemRow(Reader: TReader);
begin
  NewItemRow := Reader.ReadBoolean;
end;

procedure TcxGridTableOptionsView.ReadNewItemRowInfoText(Reader: TReader);
begin
  NewItemRowInfoText := Reader.ReadString;
end;

procedure TcxGridTableOptionsView.ReadNewItemRowSeparatorColor(Reader: TReader);
begin
  if Reader.NextValue = vaIdent then
    NewItemRowSeparatorColor := StringToColor(Reader.ReadIdent)
  else
    NewItemRowSeparatorColor := Reader.ReadInteger;
end;

procedure TcxGridTableOptionsView.ReadNewItemRowSeparatorWidth(Reader: TReader);
begin
  NewItemRowSeparatorWidth := Reader.ReadInteger;
end;

procedure TcxGridTableOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  IndicatorWidth := MulDiv(IndicatorWidth, M, D);
  if HeaderHeight > 0 then
    HeaderHeight := Max(1, MulDiv(HeaderHeight, M, D));
  if DataRowHeight > 0 then
    DataRowHeight := Max(1, MulDiv(DataRowHeight, M, D));
  if GroupRowHeight > 0 then
    GroupRowHeight := Max(1, MulDiv(GroupRowHeight, M, D));
  if RowSeparatorWidth > 0 then
    RowSeparatorWidth := Max(1, MulDiv(RowSeparatorWidth, M, D));
end;

procedure TcxGridTableOptionsView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('NewItemRow', ReadNewItemRow, nil, True);
  Filer.DefineProperty('NewItemRowInfoText', ReadNewItemRowInfoText, nil, True);
  Filer.DefineProperty('NewItemRowSeparatorColor', ReadNewItemRowSeparatorColor, nil, True);
  Filer.DefineProperty('NewItemRowSeparatorWidth', ReadNewItemRowSeparatorWidth, nil, True);
end;

function TcxGridTableOptionsView.IsMergedGroupSeparatorStored: Boolean;
begin
  Result := FMergedGroupSeparator <> cxGridDefaultMergedGroupSeparator;
end;

procedure TcxGridTableOptionsView.ItemCaptionAutoHeightChanged;
begin
  Changed(vcLayout);
end;

procedure TcxGridTableOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxGridTableOptionsView then
    with TcxGridTableOptionsView(Source) do
    begin
      Self.ColumnAutoWidth := ColumnAutoWidth;
      Self.DataRowHeight := DataRowHeight;
      Self.ExpandButtonsForEmptyDetails := ExpandButtonsForEmptyDetails;
      Self.Footer := Footer;
      Self.FooterAutoHeight := FooterAutoHeight;
      Self.FooterMultiSummaries := FooterMultiSummaries;
      Self.GridLineColor := GridLineColor;
      Self.GridLines := GridLines;
      Self.GroupByBox := GroupByBox;
      Self.GroupByHeaderLayout := GroupByHeaderLayout;
      Self.GroupFooterMultiSummaries := GroupFooterMultiSummaries;
      Self.GroupFooters := GroupFooters;
      Self.GroupRowHeight := GroupRowHeight;
      Self.GroupRowStyle := GroupRowStyle;
      Self.GroupSummaryLayout := GroupSummaryLayout;
      Self.Header := Header;
      Self.HeaderHeight := HeaderHeight;
      Self.Indicator := Indicator;
      Self.IndicatorWidth := IndicatorWidth;
      Self.FPrevGroupFooters := FPrevGroupFooters;
      Self.RowSeparatorColor := RowSeparatorColor;
      Self.RowSeparatorWidth := RowSeparatorWidth;
    end;
  inherited;
end;

function TcxGridTableOptionsView.CanShowFooterMultiSummaries: Boolean;
begin
  Result := GridView.ViewInfo.SupportsMultipleFooterSummaries and FooterMultiSummaries;
end;

function TcxGridTableOptionsView.CanShowGroupFooterMultiSummaries: Boolean;
begin
  Result := GridView.ViewInfo.SupportsMultipleFooterSummaries and GroupFooterMultiSummaries;
end;

procedure TcxGridTableOptionsView.CheckDataRowHeight(var AValue: Integer);
var
  AMinValue: Integer;
begin
  if AValue < 0 then AValue := 0;
  if AValue > 0 then
  begin
    AMinValue := GridView.ViewInfo.RecordsViewInfo.CalculateRowDefaultHeight;
    if AValue < AMinValue then AValue := AMinValue;
  end;
end;

procedure TcxGridTableOptionsView.CheckGroupRowHeight(var AValue: Integer);
var
  AMinValue: Integer;
begin
  if AValue < 0 then AValue := 0;
  if AValue > 0 then
  begin
    AMinValue :=
      GridView.ViewInfo.RecordsViewInfo.CalculateGroupRowDefaultHeight(True);
    if AValue < AMinValue then AValue := AMinValue;
  end;
end;

function TcxGridTableOptionsView.GetGridLineColor: TColor;
begin
  Result := FGridLineColor;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultGridLineColor;
end;

function TcxGridTableOptionsView.GetGroupSummaryLayout: TcxGridGroupSummaryLayout;
begin
  if GridView.ViewInfo.SupportsGroupSummariesAlignedWithColumns then
    Result := FGroupSummaryLayout
  else
    Result := gslStandard;
end;

function TcxGridTableOptionsView.GetRowSeparatorColor: TColor;
begin
  Result := FRowSeparatorColor;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultRecordSeparatorColor;
end;

{ TcxGridPreview }

constructor TcxGridPreview.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FAutoHeight := True;
  FLeftIndent := cxGridPreviewDefaultLeftIndent;
  FMaxLineCount := cxGridPreviewDefaultMaxLineCount;
  FRightIndent := cxGridPreviewDefaultRightIndent;
end;

function TcxGridPreview.GetActive: Boolean;
begin
  Result := FVisible and (FColumn <> nil);
end;

function TcxGridPreview.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridPreview.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    PropertyChanged;
  end;
end;

procedure TcxGridPreview.SetColumn(Value: TcxGridColumn);

  procedure ColumnVisibilityChanged(AColumn: TcxGridColumn);
  begin
    AColumn.VisibleChanged;
    GridView.ItemVisibilityChanged(AColumn, AColumn.ActuallyVisible);
    AColumn.VisibleForCustomizationChanged;
  end;

var
  APrevColumn: TcxGridColumn;
begin
  if (Value <> nil) and (Value.GridView <> GridView) then Value := nil;
  if FColumn <> Value then
  begin
    APrevColumn := FColumn;
    FColumn := Value;
    GridView.BeginUpdate;
    try
      if APrevColumn <> nil then
      begin
        if not GridView.IsDestroying then
        begin
          if APrevColumn.CanCreateLayoutItem then
            APrevColumn.CreateNewLayoutItem;
          APrevColumn.CheckUsingInFindFiltering;
        end;
        ColumnVisibilityChanged(APrevColumn);
      end;
      if FColumn <> nil then
      begin
        FColumn.DestroyLayoutItem;
        FColumn.CheckUsingInFindFiltering;
        ColumnVisibilityChanged(FColumn);
      end;
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridPreview.SetLeftIndent(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FLeftIndent <> Value then
  begin
    FLeftIndent := Value;
    PropertyChanged;
  end;
end;

procedure TcxGridPreview.SetMaxLineCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxLineCount <> Value then
  begin
    FMaxLineCount := Value;
    PropertyChanged;
  end;
end;

procedure TcxGridPreview.SetPlace(Value: TcxGridPreviewPlace);
begin
  if FPlace <> Value then
  begin
    FPlace := Value;
    PropertyChanged;
  end;
end;

procedure TcxGridPreview.SetRightIndent(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FRightIndent <> Value then
  begin
    FRightIndent := Value;
    PropertyChanged;
  end;
end;

procedure TcxGridPreview.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridPreview.ChangeScale(M, D: Integer);
begin
  inherited;
  LeftIndent := MulDiv(LeftIndent, M, D);
  RightIndent := MulDiv(RightIndent, M, D);
end;

function TcxGridPreview.GetFixedHeight: Integer;
var
  AParams: TcxViewParams;
  AGridViewInfo: TcxGridTableViewInfo;
begin
  if not Active or AutoHeight or (Column = nil) or (MaxLineCount = 0) then
    Result := 0
  else
  begin
    Column.Styles.GetContentParams(nil, AParams);
    AGridViewInfo := GridView.ViewInfo;
    Result := MaxLineCount * AGridViewInfo.GetFontHeight(AParams.Font);
    GetCellTextAreaSize(Result, AGridViewInfo.ScaleFactor);
    Result := AGridViewInfo.RecordsViewInfo.GetCellHeight(Result);
  end;
end;

procedure TcxGridPreview.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Column) then
    Column := nil;
end;

procedure TcxGridPreview.PropertyChanged;
begin
  if FVisible then
    Changed(vcSize)
  else
    Changed(vcProperty);
end;

procedure TcxGridPreview.Assign(Source: TPersistent);
begin
  if Source is TcxGridPreview then
    with TcxGridPreview(Source) do
    begin
      Self.AutoHeight := AutoHeight;
      if not Self.GridView.AssigningSettings then
        if Column <> nil then
          Self.Column := TcxGridColumn(Self.GridView.FindItemByID(Column.ID))
        else
          Self.Column := Column;
      Self.LeftIndent := LeftIndent;
      Self.MaxLineCount := MaxLineCount;
      Self.Place := Place;
      Self.RightIndent := RightIndent;
      Self.Visible := Visible;
    end;
  inherited;
end;

{ TcxGridTableViewStyles }

function TcxGridTableViewStyles.GetGridViewValue: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

procedure TcxGridTableViewStyles.SetOnGetFooterStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetFooterStyle, Value) then
  begin
    FOnGetFooterStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetFooterStyleEx(Value: TcxGridGetFooterStyleExEvent);
begin
  if not dxSameMethods(FOnGetFooterStyleEx, Value) then
  begin
    FOnGetFooterStyleEx := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetFooterSummaryStyle(Value: TcxGridGetFooterSummaryStyleEvent);
begin
  if not dxSameMethods(FOnGetFooterSummaryStyle, Value) then
  begin
    FOnGetFooterSummaryStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetGroupStyle(Value: TcxGridGetGroupStyleEvent);
begin
  if not dxSameMethods(FOnGetGroupStyle, Value) then
  begin
    FOnGetGroupStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetGroupSummaryStyle(Value: TcxGridGetGroupSummaryStyleEvent);
begin
  if not dxSameMethods(FOnGetGroupSummaryStyle, Value) then
  begin
    FOnGetGroupSummaryStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetHeaderStyle(Value: TcxGridGetHeaderStyleEvent);
begin
  if not dxSameMethods(FOnGetHeaderStyle, Value) then
  begin
    FOnGetHeaderStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetInplaceEditFormItemStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetInplaceEditFormItemStyle, Value) then
  begin
    FOnGetInplaceEditFormItemStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.SetOnGetPreviewStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetPreviewStyle, Value) then
  begin
    FOnGetPreviewStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableViewStyles.GetDefaultViewParams(
  Index: Integer; AData: TObject; out AParams: TcxViewParams);

  procedure GetGroupDefaultViewParams;
  begin
    inherited GetContentParams(TcxCustomGridRecord(AData), nil, AParams);
    if GridView.OptionsView.GroupRowStyle = grsStandard then
    begin
      AParams.Color := LookAndFeelPainter.DefaultGroupColor;
      AParams.TextColor := LookAndFeelPainter.DefaultGroupTextColor;
    end
    else
    begin
      AParams.Color := LookAndFeelPainter.GridGroupRowStyleOffice11ContentColor(AData <> nil);
      AParams.TextColor := LookAndFeelPainter.GridGroupRowStyleOffice11TextColor;
    end;
  end;

begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      vsFooter:
        begin
          Color := DefaultFooterColor;
          TextColor := DefaultFooterTextColor;
        end;
      vsGroup:
        GetGroupDefaultViewParams;
      vsGroupByBox:
        begin
          Color := DefaultGroupByBoxColor;
          TextColor := DefaultGroupByBoxTextColor;
        end;
      vsGroupFooterSortedSummary:
        if AData <> nil then
          TcxGridRowFooterCellPos(AData).Column.Styles.GetFooterParams(
            TcxGridRowFooterCellPos(AData).Row,
            TcxGridRowFooterCellPos(AData).FooterGroupLevel,
            TcxGridRowFooterCellPos(AData).SummaryItem, AParams)
        else
          GetFooterParams(nil, nil, -1, nil, AParams);
      vsGroupSortedSummary:
        if AData <> nil then
          GetGroupSummaryCellContentParams(TcxGridGroupSummaryInfo(AData).Row,
            TcxGridGroupSummaryInfo(AData).SummaryItem, AParams)
        else
          GetGroupSummaryCellContentParams(nil, nil, AParams);
      vsGroupSummary:
        GetRecordContentParams(TcxCustomGridRecord(AData), nil, AParams);
      vsHeader, vsIndicator:
        begin
          Color := DefaultHeaderColor;
          TextColor := DefaultHeaderTextColor;
        end;
      vsFilterRowInfoText, vsNewItemRowInfoText:
        begin
          GetContentParams(TcxCustomGridRecord(AData), nil, AParams);
          TextColor := clGrayText;
        end;
      vsPreview:
        begin
          inherited GetContentParams(TcxCustomGridRecord(AData), GridView.Preview.Column, AParams);
          TextColor := DefaultPreviewTextColor;
        end;
      vsInplaceEditFormItem:
        TextColor := DefaultLayoutViewContentTextColor(cxbsNormal);
      vsInplaceEditFormItemHotTrack:
        TextColor := DefaultLayoutViewContentTextColor(cxbsHot);
      vsInplaceEditFormGroup:
        begin
          TextColor := clDefault;
          Color := clDefault;
        end;
      vsSelection:
        if GridView.InplaceEditForm.Visible then
          TextColor := DefaultLayoutViewContentTextColor(cxbsPressed);
      vsInactive:
        if GridView.InplaceEditForm.Visible then
          TextColor := DefaultLayoutViewContentTextColor(cxbsDisabled);
    end;
end;

procedure TcxGridTableViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridTableViewStyles then
    with TcxGridTableViewStyles(Source) do
    begin
      Self.FilterRowInfoText := FilterRowInfoText;
      Self.Footer := Footer;
      Self.Group := Group;
      Self.GroupByBox := GroupByBox;
      Self.GroupFooterSortedSummary := GroupFooterSortedSummary;
      Self.GroupSortedSummary := GroupSortedSummary;
      Self.GroupSummary := GroupSummary;
      Self.Header := Header;
      Self.Indicator := Indicator;
      Self.NewItemRowInfoText := NewItemRowInfoText;
      Self.Preview := Preview;
      Self.InplaceEditFormGroup := InplaceEditFormGroup;
      Self.InplaceEditFormItem := InplaceEditFormItem;
      Self.InplaceEditFormItemHotTrack := InplaceEditFormItemHotTrack;
      Self.OnGetFooterStyle := OnGetFooterStyle;
      Self.OnGetFooterStyleEx := OnGetFooterStyleEx;
      Self.OnGetFooterSummaryStyle := OnGetFooterSummaryStyle;
      Self.OnGetGroupStyle := OnGetGroupStyle;
      Self.OnGetGroupSummaryStyle := OnGetGroupSummaryStyle;
      Self.OnGetHeaderStyle := OnGetHeaderStyle;
      Self.OnGetPreviewStyle := OnGetPreviewStyle;
    end;
end;

procedure TcxGridTableViewStyles.GetCellContentParams(ARecord: TcxCustomGridRecord;
  AItem: TObject; out AParams: TcxViewParams);
begin
  if (AItem is TcxDataSummaryItem) or (AItem is TcxDataSummaryItems) then
  begin
    if AItem is TcxDataSummaryItems then
      AItem := nil;
    GetGroupSummaryCellContentParams(ARecord as TcxGridGroupRow, TcxDataSummaryItem(AItem), AParams);
  end
  else
    inherited;
end;

procedure TcxGridTableViewStyles.GetContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if (AItem <> nil) and TcxGridColumn(AItem).IsPreview then
    GetPreviewParams(ARecord, AItem, AParams)
  else
    inherited;
end;

procedure TcxGridTableViewStyles.GetFooterCellParams(ARow: TcxCustomGridRow;
  AColumn: TcxGridColumn; AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem;
  out AParams: TcxViewParams);
var
  AFooterCellPos: TcxGridRowFooterCellPos;
begin
  if (AFooterGroupLevel <> -1) and (ASummaryItem <> nil) and ASummaryItem.Sorted then
  begin
    AFooterCellPos := TcxGridRowFooterCellPos.Create(ARow, AColumn, AFooterGroupLevel, ASummaryItem);
    try
      GetViewParams(vsGroupFooterSortedSummary, AFooterCellPos, nil, AParams);
    finally
      AFooterCellPos.Free;
    end;
  end
  else
    AColumn.Styles.GetFooterParams(ARow, AFooterGroupLevel, ASummaryItem, AParams);
end;

procedure TcxGridTableViewStyles.GetFooterParams(ARow: TcxCustomGridRow;
  AColumn: TcxGridColumn; AFooterGroupLevel: Integer; ASummaryItem: TcxDataSummaryItem;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetFooterStyle) then
    FOnGetFooterStyle(GridView, ARow, AColumn, AStyle);
  if Assigned(FOnGetFooterStyleEx) then
    FOnGetFooterStyleEx(GridView, ARow, AColumn, AFooterGroupLevel, AStyle);
  if Assigned(FOnGetFooterSummaryStyle) and (ASummaryItem <> nil) then
    FOnGetFooterSummaryStyle(GridView, ARow, AColumn, AFooterGroupLevel, ASummaryItem, AStyle);
  GetViewParams(vsFooter, nil, AStyle, AParams);
  AParams.Bitmap := GridView.BackgroundBitmaps.GetBitmap(bbFooter);
end;

procedure TcxGridTableViewStyles.GetGroupParams(ARecord: TcxCustomGridRecord;
  ALevel: Integer; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetGroupStyle) then
  begin
    if ARecord <> nil then ALevel := ARecord.Level;
    FOnGetGroupStyle(GridView, ARecord, ALevel, AStyle);
  end;
  GetViewParams(vsGroup, ARecord, AStyle, AParams);
end;

procedure TcxGridTableViewStyles.GetGroupSummaryCellContentParams(ARow: TcxGridGroupRow;
  ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams);
var
  ASummaryInfo: TcxGridGroupSummaryInfo;
begin
  if not FProcessingGroupSortedSummary and
    (ASummaryItem <> nil) and (ASummaryItem = ARow.GroupSummaryItems.SortedSummaryItem) then
  begin
    FProcessingGroupSortedSummary := True;
    ASummaryInfo := TcxGridGroupSummaryInfo.Create(ARow, ASummaryItem);
    try
      GetViewParams(vsGroupSortedSummary, ASummaryInfo, nil, AParams);
    finally
      ASummaryInfo.Free;
      FProcessingGroupSortedSummary := False;
    end;
  end
  else
    if (ASummaryItem = nil) or (ASummaryItem.ItemLink = nil) then
      GetGroupSummaryParams(ARow, ASummaryItem, AParams)
    else
      TcxGridColumn(ASummaryItem.ItemLink).Styles.GetGroupSummaryParams(ARow, ASummaryItem, AParams);
end;

procedure TcxGridTableViewStyles.GetGroupSummaryCellParams(ARow: TcxGridGroupRow;
  ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams);
begin
  if GridView.DrawDataCellSelected(ARow, nil) then
    if ASummaryItem = nil then
      GetSelectionParams(ARow, ARow.GroupSummaryItems, AParams)
    else
      GetSelectionParams(ARow, ASummaryItem, AParams)
  else
    GetGroupSummaryCellContentParams(ARow, ASummaryItem, AParams);
end;

procedure TcxGridTableViewStyles.GetGroupSummaryParams(ARow: TcxGridGroupRow;
  ASummaryItem: TcxDataSummaryItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  AColumn: TcxGridColumn;
begin
  AStyle := nil;
  if (ARow <> nil) and Assigned(FOnGetGroupSummaryStyle) then
  begin
    if ASummaryItem = nil then
      AColumn := nil
    else
      AColumn := ASummaryItem.ItemLink as TcxGridColumn;
    FOnGetGroupSummaryStyle(GridView, ARow, AColumn, ASummaryItem, AStyle);
  end;
  GetViewParams(vsGroupSummary, ARow, AStyle, AParams);
end;

procedure TcxGridTableViewStyles.GetHeaderParams(AItem: TcxGridColumn;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetHeaderStyle) then
    FOnGetHeaderStyle(GridView, AItem, AStyle);
  GetViewParams(vsHeader, nil, AStyle, AParams);
end;

procedure TcxGridTableViewStyles.GetInplaceEditFormGroupParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetInplaceEditFormGroupStyle) then
    FOnGetInplaceEditFormGroupStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsInplaceEditFormGroup, ADataCellPos, AStyle, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridTableViewStyles.GetInplaceEditFormItemHottrackParams(AItem: TcxCustomGridTableItem;
  out AParams: TcxViewParams);
begin
  GetViewParams(vsInplaceEditFormItemHotTrack, AItem, nil, AParams);
end;

procedure TcxGridTableViewStyles.GetInplaceEditFormItemParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetInplaceEditFormItemStyle) then
    FOnGetInplaceEditFormItemStyle(GridView, ARecord, AItem, AStyle);

  if (AItem <> nil) and GridView.EditForm.ItemHotTrack and
    GridView.DrawRecordFocused(ARecord) and AItem.Focused then
    GetSelectionParams(ARecord, AItem, AParams)
  else
  begin
    ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
    try
      GetViewParams(vsInplaceEditFormItem, ADataCellPos, AStyle, AParams);
    finally
      ADataCellPos.Free;
    end;
  end;
end;

procedure TcxGridTableViewStyles.GetPreviewParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetPreviewStyle) then
    FOnGetPreviewStyle(GridView, ARecord, AItem, AStyle);
  GetViewParams(vsPreview, ARecord, AStyle, AParams);
end;

procedure TcxGridTableViewStyles.GetRecordContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if ARecord is TcxGridGroupRow then
    GetGroupParams(ARecord, -1, AParams)
  else
    inherited;
end;

{ TcxGridTableViewStyleSheet }

function TcxGridTableViewStyleSheet.GetStylesValue: TcxGridTableViewStyles;
begin
  Result := TcxGridTableViewStyles(GetStyles);
end;

procedure TcxGridTableViewStyleSheet.SetStylesValue(Value: TcxGridTableViewStyles);
begin
  SetStyles(Value);
end;

class function TcxGridTableViewStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxGridTableViewStyles;
end;

{ TcxGridTableSummaryGroupItemLink }

function TcxGridTableSummaryGroupItemLink.GetColumn: TcxGridColumn;
begin
  Result := TcxGridColumn(ItemLink);
end;

procedure TcxGridTableSummaryGroupItemLink.SetColumn(Value: TcxGridColumn);
begin
  ItemLink := Value;
end;

function TcxGridTableSummaryGroupItemLink.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView((DataController as IcxCustomGridDataController).GridView);
end;

function TcxGridTableSummaryGroupItemLink.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxGridTableSummaryGroupItemLink._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridTableSummaryGroupItemLink._Release: Integer;
begin
  Result := -1;
end;

function TcxGridTableSummaryGroupItemLink.GetObjectName: string;
begin
  Result := '';
end;

function TcxGridTableSummaryGroupItemLink.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Column');
  Result := True;
end;

procedure TcxGridTableSummaryGroupItemLink.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'Column' then
    if Column <> nil then
      AValue := Column.GetObjectName
    else
      AValue := '';
end;

procedure TcxGridTableSummaryGroupItemLink.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'Column' then
    Column := TcxGridColumn(GridView.FindItemByObjectName(AValue));
end;

{ TcxGridTableSummaryItem }

constructor TcxGridTableSummaryItem.Create(Collection: TCollection);
begin
  inherited;
  FVisibleForCustomization := True;
end;

function TcxGridTableSummaryItem.GetColumn: TcxGridColumn;
begin
  Result := TcxGridColumn(ItemLink);
end;

function TcxGridTableSummaryItem.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView((DataController as IcxCustomGridDataController).GridView);
end;

procedure TcxGridTableSummaryItem.SetColumn(Value: TcxGridColumn);
begin
  ItemLink := Value;
end;

procedure TcxGridTableSummaryItem.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
  begin
    FDisplayText := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridTableSummaryItem.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    GridView.Changed(vcProperty);
  end;
end;

function TcxGridTableSummaryItem.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxGridTableSummaryItem._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridTableSummaryItem._Release: Integer;
begin
  Result := -1;
end;

function TcxGridTableSummaryItem.GetObjectName: string;
begin
  Result := '';
end;

function TcxGridTableSummaryItem.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Column');
  Result := False;
end;

procedure TcxGridTableSummaryItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Column' then
    if Column <> nil then
      AValue := Column.GetObjectName
    else
      AValue := '';
end;

procedure TcxGridTableSummaryItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Column' then
    Column := TcxGridColumn(GridView.FindItemByObjectName(AValue));
end;

function TcxGridTableSummaryItem.GetDisplayText: string;
begin
  Result := DisplayText;
end;

function TcxGridTableSummaryItem.GetVisibleForCustomization: Boolean;
begin
  Result := VisibleForCustomization;
end;

procedure TcxGridTableSummaryItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridTableSummaryItem then
    with TcxGridTableSummaryItem(Source) do
    begin
      Self.DisplayText := DisplayText;
      Self.VisibleForCustomization := VisibleForCustomization;
    end;
end;

{ TcxGridTableView }

constructor TcxGridTableView.Create(AOwner: TComponent);
begin
  inherited;
  FAllowCellMerging := True;
end;

function TcxGridTableView.GetBackgroundBitmaps: TcxGridTableBackgroundBitmaps;
begin
  Result := TcxGridTableBackgroundBitmaps(inherited BackgroundBitmaps);
end;

function TcxGridTableView.GetColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(Items[Index]);
end;

function TcxGridTableView.GetColumnCount: Integer;
begin
  Result := ItemCount;
end;

function TcxGridTableView.GetController: TcxGridTableController;
begin
  Result := TcxGridTableController(inherited Controller);
end;

function TcxGridTableView.GetDataController: TcxGridDataController;
begin
  Result := TcxGridDataController(FDataController);
end;

function TcxGridTableView.GetDateTimeHandling: TcxGridTableDateTimeHandling;
begin
  Result := TcxGridTableDateTimeHandling(inherited DateTimeHandling);
end;

function TcxGridTableView.GetEditForm: TcxGridEditFormOptions;
begin
  Result := FEditForm;
end;

function TcxGridTableView.GetFiltering: TcxGridTableFiltering;
begin
  Result := TcxGridTableFiltering(inherited Filtering);
end;

function TcxGridTableView.GetGroupedColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(GroupedItems[Index]);
end;

function TcxGridTableView.GetGroupedColumnCount: Integer;
begin
  Result := GroupedItemCount;
end;

function TcxGridTableView.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := FInplaceEditForm;
end;

function TcxGridTableView.GetOptionsBehavior: TcxGridTableOptionsBehavior;
begin
  Result := TcxGridTableOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridTableView.GetOptionsCustomize: TcxGridTableOptionsCustomize;
begin
  Result := TcxGridTableOptionsCustomize(inherited OptionsCustomize);
end;

function TcxGridTableView.GetOptionsData: TcxGridTableOptionsData;
begin
  Result := TcxGridTableOptionsData(inherited OptionsData);
end;

function TcxGridTableView.GetOptionsSelection: TcxGridTableOptionsSelection;
begin
  Result := TcxGridTableOptionsSelection(inherited OptionsSelection);
end;

function TcxGridTableView.GetOptionsView: TcxGridTableOptionsView;
begin
  Result := TcxGridTableOptionsView(inherited OptionsView);
end;

function TcxGridTableView.GetPainter: TcxGridTablePainter;
begin
  Result := TcxGridTablePainter(inherited Painter);
end;

function TcxGridTableView.GetStyles: TcxGridTableViewStyles;
begin
  Result := TcxGridTableViewStyles(inherited Styles);
end;

function TcxGridTableView.GetViewData: TcxGridViewData;
begin
  Result := TcxGridViewData(inherited ViewData);
end;

function TcxGridTableView.GetViewInfo: TcxGridTableViewInfo;
begin
  Result := TcxGridTableViewInfo(inherited ViewInfo);
end;

function TcxGridTableView.GetVisibleColumn(Index: Integer): TcxGridColumn;
begin
  Result := TcxGridColumn(VisibleItems[Index]);
end;

function TcxGridTableView.GetVisibleColumnCount: Integer;
begin
  Result := VisibleItemCount;
end;

procedure TcxGridTableView.SetBackgroundBitmaps(Value: TcxGridTableBackgroundBitmaps);
begin
  inherited BackgroundBitmaps := Value;
end;

procedure TcxGridTableView.SetColumn(Index: Integer; Value: TcxGridColumn);
begin
  Items[Index] := Value;
end;

procedure TcxGridTableView.SetDataController(Value: TcxGridDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridTableView.SetDateTimeHandling(Value: TcxGridTableDateTimeHandling);
begin
  inherited DateTimeHandling := Value;
end;

procedure TcxGridTableView.SetEditForm(AValue: TcxGridEditFormOptions);
begin
  FEditForm.Assign(AValue);
end;

procedure TcxGridTableView.SetFiltering(Value: TcxGridTableFiltering);
begin
  inherited Filtering := Value;
end;

procedure TcxGridTableView.SetFilterRow(Value: TcxGridFilterRowOptions);
begin
  FFilterRow.Assign(Value);
end;

procedure TcxGridTableView.SetFixedDataRows(Value: TcxGridFixedDataRowsOptions);
begin
  FFixedDataRows.Assign(Value);
end;

procedure TcxGridTableView.SetNewItemRow(Value: TcxGridNewItemRowOptions);
begin
  FNewItemRow.Assign(Value);
end;

procedure TcxGridTableView.SetOnColumnHeaderClick(Value: TcxGridColumnEvent);
begin
  if not dxSameMethods(FOnColumnHeaderClick, Value) then
  begin
    FOnColumnHeaderClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnColumnPosChanged(Value: TcxGridColumnEvent);
begin
  if not dxSameMethods(FOnColumnPosChanged, Value) then
  begin
    FOnColumnPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnColumnSizeChanged(Value: TcxGridColumnEvent);
begin
  if not dxSameMethods(FOnColumnSizeChanged, Value) then
  begin
    FOnColumnSizeChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnCustomDrawColumnHeader(Value: TcxGridColumnCustomDrawHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawColumnHeader, Value) then
  begin
    FOnCustomDrawColumnHeader := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnCustomDrawFooterCell(Value: TcxGridColumnCustomDrawHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawFooterCell, Value) then
  begin
    FOnCustomDrawFooterCell := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnCustomDrawGroupCell(Value: TcxGridTableCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawGroupCell, Value) then
  begin
    FOnCustomDrawGroupCell := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnCustomDrawGroupSummaryCell(Value: TcxGridGroupSummaryCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawGroupSummaryCell, Value) then
  begin
    FOnCustomDrawGroupSummaryCell := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnCustomDrawIndicatorCell(Value: TcxGridIndicatorCellCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawIndicatorCell, Value) then
  begin
    FOnCustomDrawIndicatorCell := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnGetUnmergeableColumns(Value: TcxGridGetUnmergeableColumnsEvent);
begin
  if not dxSameMethods(FOnGetUnmergeableColumns, Value) then
  begin
    FOnGetUnmergeableColumns := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOnLeftPosChanged(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnLeftPosChanged, Value) then
  begin
    FOnLeftPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridTableView.SetOptionsBehavior(Value: TcxGridTableOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxGridTableView.SetOptionsCustomize(Value: TcxGridTableOptionsCustomize);
begin
  inherited OptionsCustomize := Value;
end;

procedure TcxGridTableView.SetOptionsData(Value: TcxGridTableOptionsData);
begin
  inherited OptionsData := Value;
end;

procedure TcxGridTableView.SetOptionsSelection(Value: TcxGridTableOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TcxGridTableView.SetOptionsView(Value: TcxGridTableOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxGridTableView.SetPreview(Value: TcxGridPreview);
begin
  FPreview.Assign(Value);
end;

procedure TcxGridTableView.SetStyles(Value: TcxGridTableViewStyles);
begin
  inherited Styles := Value;
end;

function TcxGridTableView.GetLayoutContainer: TdxLayoutContainer;
begin
  Result := InplaceEditForm.Container;
end;

function TcxGridTableView.GetProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('Footer');
    Add('GroupByBox');
    Add('GroupFooters');
    Add('NewItemRow');
    if OptionsCustomize.DataRowSizing then
      Add('DataRowHeight');
    if OptionsCustomize.GroupRowSizing then
      Add('GroupRowHeight');
    AProperties.Add('EditFormUseDefaultLayout');
    Add('ColumnsQuickCustomizationSorted');
  end;
  Result := inherited GetProperties(AProperties);
end;

procedure TcxGridTableView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Footer' then
    AValue := OptionsView.Footer
  else
    if AName = 'GroupByBox' then
      AValue := OptionsView.GroupByBox
    else
      if AName = 'GroupFooters' then
        AValue := Variant(OptionsView.GroupFooters)
      else
        if AName = 'NewItemRow' then
          AValue := NewItemRow.Visible
        else
          if AName = 'DataRowHeight' then
            AValue := OptionsView.DataRowHeight
          else
            if AName = 'GroupRowHeight' then
              AValue := OptionsView.GroupRowHeight
            else
              if AName = 'EditFormUseDefaultLayout' then
                AValue := EditForm.UseDefaultLayout
              else
                if AName = 'ColumnsQuickCustomizationSorted' then
                  AValue := OptionsCustomize.ColumnsQuickCustomizationSorted
                else
                  inherited;
end;

procedure TcxGridTableView.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Footer' then
    OptionsView.Footer := AValue
  else
    if AName = 'GroupByBox' then
      OptionsView.GroupByBox := AValue
    else
      if AName = 'GroupFooters' then
        if VarIsStr(AValue) then  // version 4
          if AValue then
            OptionsView.GroupFooters := gfVisibleWhenExpanded
          else
            OptionsView.GroupFooters := gfInvisible
        else                      // version > 4
          OptionsView.GroupFooters := TcxGridGroupFootersMode((AValue))
      else
        if AName = 'NewItemRow' then
          NewItemRow.Visible := AValue
        else
          if (AName = 'DataRowHeight') and OptionsCustomize.DataRowSizing then
            OptionsView.DataRowHeight := AValue
          else
            if (AName = 'GroupRowHeight') and OptionsCustomize.GroupRowSizing then
              OptionsView.GroupRowHeight := AValue
            else
              if AName = 'EditFormUseDefaultLayout' then
                EditForm.UseDefaultLayout := AValue
              else
                if AName = 'ColumnsQuickCustomizationSorted' then
                  OptionsCustomize.ColumnsQuickCustomizationSorted := AValue
                else
                  inherited;
end;

procedure TcxGridTableView.GetStoredChildren(AChildren: TStringList);
begin
  inherited GetStoredChildren(AChildren);
  if not EditForm.UseDefaultLayout then
    AChildren.AddObject('Layout', InplaceEditForm.Container);
end;

procedure TcxGridTableView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
  inherited;
  with (ALayoutView as TcxGridTableView).OptionsView do
  begin
    Self.OptionsView.DataRowHeight := DataRowHeight;
    Self.OptionsView.Footer := Footer;
    Self.OptionsView.GroupByBox := GroupByBox;
    Self.OptionsView.GroupFooters := GroupFooters;
    Self.OptionsView.GroupRowHeight := GroupRowHeight;
  end;
end;

procedure TcxGridTableView.BeforeEditLayout(ALayoutView: TcxCustomGridView);
begin
  inherited;
  (ALayoutView as TcxGridTableView).AllowCellMerging := False;
end;

function TcxGridTableView.GetLayoutCustomizationFormButtonCaption: string;
begin
  Result := 'Columns Customization...';
end;

procedure TcxGridTableView.AddAdornerTargetColumns(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    AList.AddObject(Columns[I].Name, Columns[I]);
end;

procedure TcxGridTableView.GetAdornerTargetElements(AList: TStrings);
var
  ARows: TcxGridRowsViewInfo;
begin
  inherited GetAdornerTargetElements(AList);
  AList.AddObject('GroupByBox', ViewInfo.GroupByBoxViewInfo);
  AList.AddObject('HeaderArea', ViewInfo.HeaderViewInfo);
  ARows := ViewInfo.RecordsViewInfo;
  if ARows <> nil then
  begin
    AList.AddObject('FilterRow', ARows.FilterRowViewInfo);
    AList.AddObject('NewItemRow', ARows.NewItemRowViewInfo);
  end;
  AList.AddObject('FooterArea', ViewInfo.FooterViewInfo);
  AddAdornerTargetColumns(AList);
end;

procedure TcxGridTableView.CreateHandlers;
begin
  inherited CreateHandlers;
  FInplaceEditFormLayoutLookAndFeel := CreateInplaceEditFormLayoutLookAndFeel;
  FInplaceEditForm := GetInplaceEditFormClass.Create(Self);
end;

procedure TcxGridTableView.DestroyHandlers;
begin
  FreeAndNil(FInplaceEditForm);
  FreeAndNil(FInplaceEditFormLayoutLookAndFeel);
  inherited DestroyHandlers;
end;

procedure TcxGridTableView.CreateOptions;
begin
  inherited;
  FFilterRow := GetFilterRowOptionsClass.Create(Self);
  FFixedDataRows := GetFixedDataRowsOptionsClass.Create(Self);
  FNewItemRow := GetNewItemRowOptionsClass.Create(Self);
  FPreview := GetPreviewClass.Create(Self);
  FEditForm := TcxGridEditFormOptions.Create(Self);
end;

procedure TcxGridTableView.DestroyOptions;
begin
  FreeAndNil(FEditForm);
  FreeAndNil(FPreview);
  FreeAndNil(FNewItemRow);
  FreeAndNil(FFixedDataRows);
  FreeAndNil(FFilterRow);
  inherited;
end;

function TcxGridTableView.CreateInplaceEditFormLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel;
begin
  Result := TcxGridInplaceEditFormLayoutLookAndFeel.Create(Self);
  UpdateInplaceEditFormStyles(nil, Result);
end;

procedure TcxGridTableView.Init;
begin
  InplaceEditForm.Init;
  inherited Init;
end;

procedure TcxGridTableView.UpdateInplaceEditFormStyles(ARecord: TcxCustomGridRecord;
  var ALayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel);

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
    Styles.GetInplaceEditFormItemParams(ARecord, nil, AParams);
    UpdateCaptionOptions(ALayoutLookAndFeel.ItemOptions.CaptionOptions, AParams);
    Styles.GetInplaceEditFormGroupParams(ARecord, nil, AParams);
    UpdateCaptionOptions(ALayoutLookAndFeel.GroupOptions.CaptionOptions, AParams);
    ALayoutLookAndFeel.GroupOptions.Color := AParams.Color;
  finally
    ALayoutLookAndFeel.EndUpdate;
  end;
end;

procedure TcxGridTableView.LookAndFeelChanged;
begin
  inherited LookAndFeelChanged;
  UpdateInplaceEditFormStyles(nil, FInplaceEditFormLayoutLookAndFeel);
end;

procedure TcxGridTableView.AfterRestoring;
begin
  InplaceEditForm.AfterRestoring;
  inherited;
end;

procedure TcxGridTableView.BeforeRestoring;
begin
  InplaceEditForm.BeforeRestoring;
  inherited;
end;

procedure TcxGridTableView.AfterAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxGridTableView then
    with TcxGridTableView(ASource) do
    begin
      Self.FilterRow := FilterRow;
      Self.FixedDataRows := FixedDataRows;
      Self.NewItemRow := NewItemRow;
      Self.Preview := Preview;
    end;
  inherited AfterAssign(ASource);
end;

procedure TcxGridTableView.AssignEditingRecord;
var
  AFocusedRecord: TcxCustomGridRecord;
begin
  inherited AssignEditingRecord;
  if (dceEdit in DataController.EditState) and IsInplaceEditFormMode and not InplaceEditForm.Visible then
  begin
    AFocusedRecord := Controller.FocusedRow;
    if (AFocusedRecord is TcxGridDataRow) then
      TcxGridDataRow(AFocusedRecord).EditFormVisible := True;
  end;
end;

function TcxGridTableView.CanCellMerging: Boolean;
begin
  Result := AllowCellMerging and not FPreview.Active and (OptionsView.RowSeparatorWidth = 0);
end;

function TcxGridTableView.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean;
begin
  Result := inherited CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta) and
    not IsMaster and not IsFixedGroupsMode;
end;

function TcxGridTableView.CanOffsetHorz: Boolean;
begin
  Result := not IsUpdateLocked and (not IsMaster or (GroupedColumnCount = 0));
end;

function TcxGridTableView.CanShowInplaceEditForm: Boolean;
begin
  Result := IsInplaceEditFormMode and not Controller.IsSpecialRowFocused and (not IsMaster or (MasterRowDblClickAction = dcaShowEditForm));
end;

procedure TcxGridTableView.DetailDataChanged(ADetail: TcxCustomGridView);
begin
  inherited;
  if UpdateOnDetailDataChange(ADetail) then
    SizeChanged;
end;

procedure TcxGridTableView.DoAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxGridTableView then
    with TcxGridTableView(ASource) do
    begin
      Self.EditForm := EditForm;
      Self.OnColumnHeaderClick := OnColumnHeaderClick;
      Self.OnColumnPosChanged := OnColumnPosChanged;
      Self.OnColumnSizeChanged := OnColumnSizeChanged;
      Self.OnCustomDrawColumnHeader := OnCustomDrawColumnHeader;
      Self.OnCustomDrawFooterCell := OnCustomDrawFooterCell;
      Self.OnCustomDrawGroupCell := OnCustomDrawGroupCell;
      Self.OnCustomDrawGroupSummaryCell := OnCustomDrawGroupSummaryCell;
      Self.OnCustomDrawIndicatorCell := OnCustomDrawIndicatorCell;
      Self.OnGroupRowCollapsed := OnGroupRowCollapsed;
      Self.OnGroupRowCollapsing := OnGroupRowCollapsing;
      Self.OnGroupRowExpanded := OnGroupRowExpanded;
      Self.OnGroupRowExpanding := OnGroupRowExpanding;
      Self.OnLeftPosChanged := OnLeftPosChanged;
      Self.OnGetUnmergeableColumns := OnGetUnmergeableColumns;
      if not Self.AssigningSettings then
        Self.InplaceEditForm.AssignStructure(InplaceEditForm);
    end;
  inherited DoAssign(ASource);
end;

procedure TcxGridTableView.DoItemsAssigned;
var
  I: Integer;
  AColumn: TcxCustomGridColumn;
begin
  inherited DoItemsAssigned;
  for I := 0 to AssigningIsChildInMergedGroupItems.Count - 1 do
  begin
    AColumn := TcxCustomGridColumn(AssigningIsChildInMergedGroupItems[I]);
    if AColumn <> nil then  // because of inherited forms
      AColumn.IsChildInMergedGroup := True;
  end;
end;

procedure TcxGridTableView.DoAfterAssignItems;
begin
  FAssigningIsChildInMergedGroupItems.Free;
  inherited DoAfterAssignItems;
end;

procedure TcxGridTableView.DoBeforeAssignItems;
begin
  inherited DoBeforeAssignItems;
  FAssigningIsChildInMergedGroupItems := TcxGridOpenTableItemList.Create;
end;

procedure TcxGridTableView.DoStylesChanged;
begin
  inherited DoStylesChanged;
  UpdateInplaceEditFormStyles(nil, FInplaceEditFormLayoutLookAndFeel);
end;

function TcxGridTableView.GetInplaceEditFormClientBounds: TRect;
var
  ARecord: TcxCustomGridRecord;
  ARow: TcxCustomGridRow;
begin
  Result := cxEmptyRect;
  ARecord := ViewData.GetRecordByRecordIndex(InplaceEditForm.EditingRecordIndex);
  if ARecord <> nil then
  begin
    ARow := ViewData.Rows[ARecord.Index];
    Result := TcxGridDataRow(ARow).GetInplaceEditFormClientBounds;
  end;
end;

function TcxGridTableView.GetInplaceEditFormClientRect: TRect;
begin
  Result := GetInplaceEditFormClientBounds;
end;

function TcxGridTableView.GetIsControlFocused: Boolean;
begin
  Result := inherited GetIsControlFocused or (Controller.HasFilterRowOperatorMenu and Controller.FilterRowOperatorMenu.Visible) or
    (Controller.HasDataRowFixingMenu and Controller.DataRowFixingMenu.Visible);
end;

procedure TcxGridTableView.GetItemsListForClipboard(AItems: TList; ACopyAll: Boolean);
var
  I: Integer;
begin
  if ACopyAll or not Controller.CellMultiSelect then
    inherited GetVisibleItemsList(AItems)
  else
  begin
    inherited;
    for I := AItems.Count - 1 downto 0 do
      if not TcxGridColumn(AItems[I]).Selected then
        AItems.Delete(I);
  end;
  if OptionsBehavior.CopyPreviewToClipboard then
    if FPreview.Active then AItems.Add(FPreview.Column);
end;

function TcxGridTableView.GetResizeOnBoundsChange: Boolean;
begin
  Result := inherited GetResizeOnBoundsChange or
    OptionsView.ColumnAutoWidth or Preview.Active or IsMaster;
end;

function TcxGridTableView.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
var
  AInplaceEditFormAreaViewInfo: TcxGridInplaceEditFormAreaViewInfo;
begin
  if InplaceEditForm.Visible and (InplaceEditForm.Container <> nil) and
    (InplaceEditForm.Container.ViewInfo <> nil) and (InplaceEditForm.Container.ViewInfo.RecordViewInfo <> nil) then
  begin
    Result := nil;
    AInplaceEditFormAreaViewInfo := (InplaceEditForm.Container.ViewInfo.RecordViewInfo as TcxGridDataRowViewInfo).InplaceEditFormAreaViewInfo;
    if AInplaceEditFormAreaViewInfo <> nil then
      if GetHitTest(APoint) = AInplaceEditFormAreaViewInfo.GetHitTest(APoint) then
        Result := AInplaceEditFormAreaViewInfo;
  end
  else
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

function TcxGridTableView.HasCellMerging: Boolean;
var
  I: Integer;
begin
  for I := 0 to VisibleColumnCount - 1 do
  begin
    Result := VisibleColumns[I].CanCellMerging;
    if Result then Exit;
  end;
  Result := False;
end;

function TcxGridTableView.IsCheckBoxSelectionSupported: Boolean;
begin
  Result := True;
end;

function TcxGridTableView.IsDataRowFixingSupported: Boolean;
begin
  Result := True;
end;

function TcxGridTableView.IsFixedGroupsMode: Boolean;
begin
  Result := OptionsBehavior.FixedGroups and not DataController.IsGridMode and
    (ViewData.DataController.Groups.GroupingItemCount > 0);
end;

function TcxGridTableView.IsMergedGroupsSupported: Boolean;
begin
  Result := True;
end;

function TcxGridTableView.IsPersistentMultiSelectSupported: Boolean;
begin
  Result := True;
end;

function TcxGridTableView.IsPreviewHeightFixed: Boolean;
begin
  Result := not (Preview.Active and (Preview.AutoHeight or Assigned(Styles.OnGetPreviewStyle)));
end;

function TcxGridTableView.IsRecordHeightDependsOnData: Boolean;
begin
  Result := inherited IsRecordHeightDependsOnData or
    Preview.Active and Preview.AutoHeight;
end;

procedure TcxGridTableView.Loaded;
begin
  inherited Loaded;
  InplaceEditForm.Container.FixUpItemsOwnership;
  UpdateInplaceEditFormStyles(nil, FInplaceEditFormLayoutLookAndFeel);
end;

function TcxGridTableView.NeedChangeLayoutOnSelectionChanged(AInfo: TcxSelectionChangedInfo): Boolean;
begin
  Result := inherited NeedChangeLayoutOnSelectionChanged(AInfo) or (cbvColumnHeader in OptionsSelection.CheckBoxVisibility) or
    (cbvGroupRow in OptionsSelection.CheckBoxVisibility) and (dcoMultiSelectionSyncGroupWithChildren in DataController.Options);
end;

procedure TcxGridTableView.ChangeScale(M, D: Integer);
begin
  inherited;
  FilterRow.ChangeScale(M, D);
  NewItemRow.ChangeScale(M, D);
  FixedDataRows.ChangeScale(M, D);
  Preview.ChangeScale(M, D);
end;

procedure TcxGridTableView.SetName(const NewName: TComponentName);
var
  AOldName: string;
begin
  AOldName := Name;
  inherited;
  InplaceEditForm.CheckContainerName(AOldName, Name);
end;

procedure TcxGridTableView.UpdateData(AInfo: TcxUpdateControlInfo);
begin
  inherited UpdateData(AInfo);
  if IsInplaceEditFormMode then
    UpdateInplaceEditForm(AInfo);
end;

procedure TcxGridTableView.UpdateFocusedRecord(AInfo: TcxUpdateControlInfo);
begin
  if IsInplaceEditFormMode then
    UpdateInplaceEditForm(AInfo);
  inherited UpdateFocusedRecord(AInfo);
end;

procedure TcxGridTableView.UpdateInplaceEditForm(AInfo: TcxUpdateControlInfo);
var
  ARecord: TcxCustomGridRecord;
begin
  if AInfo is TcxFocusedRecordChangedInfo then
  begin
    if dceInsert in DataController.EditState then
    begin
      ARecord := ViewData.GetRecordByRecordIndex(DataController.EditingRecordIndex);
      if ARecord <> nil then
        TcxGridDataRow(ARecord).EditFormVisible := True;
    end
    else
      if InplaceEditForm.Visible then
        InplaceEditForm.Close;
  end
  else
    if (AInfo is TcxDataChangedInfo) and (TcxDataChangedInfo(AInfo).Kind in [dcTotal, dcRecord]) and
       (DataController.EditState = []) and InplaceEditForm.Visible then
      InplaceEditForm.Close;
end;

function TcxGridTableView.UpdateOnDetailDataChange(ADetail: TcxCustomGridView): Boolean;
begin
  Result := not OptionsView.ExpandButtonsForEmptyDetails;
end;

function TcxGridTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridTableController;
end;

function TcxGridTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDataController;
end;

function TcxGridTableView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridTablePainter;
end;

function TcxGridTableView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridViewData;
end;

function TcxGridTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridTableViewInfo;
end;

function TcxGridTableView.GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass;
begin
  Result := TcxGridTableBackgroundBitmaps;
end;

function TcxGridTableView.GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass;
begin
  Result := TcxGridTableDateTimeHandling;
end;

function TcxGridTableView.GetFilteringClass: TcxCustomGridTableFilteringClass;
begin
  Result := TcxGridTableFiltering;
end;

function TcxGridTableView.GetFilterRowOptionsClass: TcxGridFilterRowOptionsClass;
begin
  Result := TcxGridFilterRowOptions;
end;

function TcxGridTableView.GetFixedDataRowsOptionsClass: TcxGridFixedDataRowsOptionsClass;
begin
  Result := TcxGridFixedDataRowsOptions;
end;

function TcxGridTableView.GetInplaceEditFormClass: TcxGridTableViewInplaceEditFormClass;
begin
  Result := TcxGridTableViewInplaceEditForm;
end;

function TcxGridTableView.GetNavigatorClass: TcxGridViewNavigatorClass;
begin
  Result := TcxGridTableViewNavigator;
end;

function TcxGridTableView.GetNewItemRowOptionsClass: TcxGridNewItemRowOptionsClass;
begin
  Result := TcxGridNewItemRowOptions;
end;

function TcxGridTableView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridTableOptionsBehavior;
end;

function TcxGridTableView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxGridTableOptionsCustomize;
end;

function TcxGridTableView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxGridTableOptionsData;
end;

function TcxGridTableView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxGridTableOptionsSelection;
end;

function TcxGridTableView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridTableOptionsView;
end;

function TcxGridTableView.GetPreviewClass: TcxGridPreviewClass;
begin
  Result := TcxGridPreview;
end;

function TcxGridTableView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridTableViewStyles;
end;

function TcxGridTableView.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := TcxGridTableSummaryGroupItemLink;
end;

function TcxGridTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridTableSummaryItem;
end;

function TcxGridTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridColumn;
end;

procedure TcxGridTableView.ItemVisibilityChanged(AItem: TcxCustomGridTableItem;
  Value: Boolean);
begin
  if not Value and (AItem = Controller.CellSelectionAnchor) then
    Controller.CellSelectionAnchor := Controller.FocusedColumn;
  inherited;
end;

function TcxGridTableView.CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
  ACellViewInfo: TcxGridTableCellViewInfo): Boolean;
begin
  if Controller.CellMultiSelect and TcxCustomGridRow(ARecord).SupportsCellMultiSelect then
    Result := (AItem <> nil) and TcxGridColumn(AItem).Selected and
      (not (ACellViewInfo is TcxGridTableDataCellViewInfo) or
       not TcxGridTableDataCellViewInfo(ACellViewInfo).Editing)
  else
    Result := inherited CalculateDataCellSelected(ARecord, AItem, AUseViewInfo, ACellViewInfo);
end;

procedure TcxGridTableView.DoColumnHeaderClick(AColumn: TcxGridColumn);
begin
  if Assigned(FOnColumnHeaderClick) then FOnColumnHeaderClick(Self, AColumn);
end;

procedure TcxGridTableView.DoColumnPosChanged(AColumn: TcxGridColumn);
begin
  if Assigned(FOnColumnPosChanged) then FOnColumnPosChanged(Self, AColumn);
end;

procedure TcxGridTableView.DoColumnSizeChanged(AColumn: TcxGridColumn);
begin
  if Assigned(FOnColumnSizeChanged) then FOnColumnSizeChanged(Self, AColumn);
end;

procedure TcxGridTableView.DoCustomDrawColumnHeader(ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawColumnHeader then
    FOnCustomDrawColumnHeader(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridTableView.DoCustomDrawFooterCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawFooterCell then
    FOnCustomDrawFooterCell(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridTableView.DoCustomDrawGroupCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawGroupCell then
    FOnCustomDrawGroupCell(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridTableView.DoCustomDrawGroupSummaryCell(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean);
var
  ACell: TcxGridGroupSummaryCellViewInfo;
begin
  if HasCustomDrawGroupSummaryCell then
  begin
    ACell := TcxGridGroupSummaryCellViewInfo(AViewInfo);
    FOnCustomDrawGroupSummaryCell(Self, ACanvas, ACell.RowViewInfo.GridRecord,
      ACell.Column, ACell.SummaryItem, AViewInfo, ADone);
  end;
end;

procedure TcxGridTableView.DoCustomDrawIndicatorCell(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawIndicatorCell then
    FOnCustomDrawIndicatorCell(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridTableView.DoGetUnmergeableColumns(AColumn: TcxGridColumn; AUnmergeableColumns: TList);
begin
  if Assigned(FOnGetUnmergeableColumns) then
    FOnGetUnmergeableColumns(Self, AColumn, AUnmergeableColumns);
end;

procedure TcxGridTableView.DoLeftPosChanged;
begin
  if Assigned(FOnLeftPosChanged) then FOnLeftPosChanged(Self);
end;

procedure TcxGridTableView.DoMakeMasterGridRecordVisible;
var
  AMasterRecordController: TcxGridTableController;
begin
  if (IsRecordPixelScrolling or HasPixelScrollingMaster) and
    not cxRectIsNull(Controller.MakeVisibleDetailRecordBounds) then
  begin
    AMasterRecordController := MasterGridView.Controller as TcxGridTableController;
    AMasterRecordController.MakeVisibleDetailRecordBounds := Controller.MakeVisibleDetailRecordBounds;
    AMasterRecordController.MakeMasterRecordVisible(MasterGridRecord);
  end
  else
    inherited DoMakeMasterGridRecordVisible;
end;

function TcxGridTableView.HasCustomDrawColumnHeader: Boolean;
begin
  Result := Assigned(FOnCustomDrawColumnHeader);
end;

function TcxGridTableView.HasCustomDrawFooterCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawFooterCell);
end;

function TcxGridTableView.HasCustomDrawGroupCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawGroupCell);
end;

function TcxGridTableView.HasCustomDrawGroupSummaryCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawGroupSummaryCell);
end;

function TcxGridTableView.HasCustomDrawIndicatorCell: Boolean;
begin
  Result := Assigned(FOnCustomDrawIndicatorCell);
end;

procedure TcxGridTableView.DoGroupRowCollapsed(AGroup: TcxGridGroupRow);
begin
  if Assigned(FOnGroupRowCollapsed) then
    FOnGroupRowCollapsed(Self, AGroup);
end;

function TcxGridTableView.DoGroupRowCollapsing(AGroup: TcxGridGroupRow): Boolean;
begin
  Result := True;
  if Assigned(FOnGroupRowCollapsing) then
    FOnGroupRowCollapsing(Self, AGroup, Result);
end;

procedure TcxGridTableView.DoGroupRowExpanded(AGroup: TcxGridGroupRow);
begin
  if Assigned(FOnGroupRowExpanded) then
    FOnGroupRowExpanded(Self, AGroup);
end;

function TcxGridTableView.DoGroupRowExpanding(AGroup: TcxGridGroupRow): Boolean;
begin
  Result := True;
  if Assigned(FOnGroupRowExpanding) then
    FOnGroupRowExpanding(Self, AGroup, Result);
end;

procedure TcxGridTableView.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
  if not EditForm.UseDefaultLayout and (Root = Owner) then
    InplaceEditForm.StoreChildren(Proc);
end;

function TcxGridTableView.CreateColumn: TcxGridColumn;
begin
  Result := TcxGridColumn(CreateItem);
end;

function TcxGridTableView.MasterRowDblClickAction: TcxGridMasterRowDblClickAction;
begin
  Result := EditForm.MasterRowDblClickAction
end;

function TcxGridTableView.IsEqualHeightRecords: Boolean;
begin
  Result := inherited IsEqualHeightRecords and
    not ViewInfo.RecordsViewInfo.HasLastHorzGridLine(nil) and
    IsPreviewHeightFixed and
    (GroupedColumnCount = 0) and not IsMaster and
    not InplaceEditForm.Visible;
end;

function TcxGridTableView.IsInplaceEditFormMode: Boolean;
begin
  Result := OptionsBehavior.IsInplaceEditFormMode
end;

function TcxGridTableView.UseRestHeightForDetails: Boolean;
begin
  Result := not Controller.IsRecordPixelScrolling;
end;

class function TcxGridTableView.CanBeLookupList: Boolean;
begin
  Result := True;
end;

{ TcxGridColumnAccess }

class function TcxGridColumnAccess.CanCellMerging(AInstance: TcxGridColumn): Boolean;
begin
  Result := AInstance.CanCellMerging;
end;

class procedure TcxGridColumnAccess.DoCustomDrawGroupSummaryCell(AInstance: TcxGridColumn;
  ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean);
begin
  AInstance.DoCustomDrawGroupSummaryCell(ACanvas, AViewInfo, ADone);
end;

class function TcxGridColumnAccess.HasCustomDrawGroupSummaryCell(AInstance: TcxGridColumn): Boolean;
begin
  Result := AInstance.HasCustomDrawGroupSummaryCell;
end;

class function TcxGridColumnAccess.GetImageComboBoxProperties(AInstance: TcxGridColumn): TcxImageComboBoxProperties;
begin
  Result := AInstance.GetImageComboBoxProperties;
end;

class function TcxGridColumnAccess.ShowGroupValuesWithImages(AInstance: TcxGridColumn): Boolean;
begin
  Result := AInstance.ShowGroupValuesWithImages;
end;

{ TcxGridTableViewAccess }

class procedure TcxGridTableViewAccess.DoColumnPosChanged(AInstance: TcxGridTableView;
  AColumn: TcxGridColumn);
begin
  AInstance.DoColumnPosChanged(AColumn);
end;

class procedure TcxGridTableViewAccess.DoCustomDrawGroupCell(AInstance: TcxGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean);
begin
  AInstance.DoCustomDrawGroupCell(ACanvas, AViewInfo, ADone);
end;

class procedure TcxGridTableViewAccess.DoCustomDrawGroupSummaryCell(AInstance: TcxGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean);
begin
  AInstance.DoCustomDrawGroupSummaryCell(ACanvas, AViewInfo, ADone);
end;

class function TcxGridTableViewAccess.GetInplaceEditForm(AInstance: TcxGridTableView): TcxGridTableViewInplaceEditForm;
begin
  Result := AInstance.InplaceEditForm;
end;

class function TcxGridTableViewAccess.GetFilterRowOperatorMenu(AInstance: TcxGridTableView): TcxFilterDropDownMenu;
begin
  Result := AInstance.Controller.FilterRowOperatorMenu;
end;

class function TcxGridTableViewAccess.HasCustomDrawGroupCell(AInstance: TcxGridTableView): Boolean;
begin
  Result := AInstance.HasCustomDrawGroupCell;
end;

class function TcxGridTableViewAccess.HasCustomDrawGroupSummaryCell(AInstance: TcxGridTableView): Boolean;
begin
  Result := AInstance.HasCustomDrawGroupSummaryCell;
end;

class function TcxGridTableViewAccess.IsMultiSelectPersistent(AInstance: TcxGridTableView): Boolean;
begin
  Result := AInstance.Controller.IsMultiSelectPersistent;
end;

initialization
  cxGridRegisteredViews.Register(TcxGridTableView, 'Table');
  Classes.RegisterClasses([TcxGridColumn, TcxGridTableViewStyleSheet]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridTableView);

end.
