{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
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

unit cxTL;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Classes, SysUtils, Clipbrd, Windows, Messages, ExtCtrls, Controls,
  Forms, StdCtrls, Menus, Graphics, ImgList, ComCtrls, Contnrs, Math,
{$IFDEF DELPHI103}
  Generics.Collections,
{$ENDIF}
  dxCore, dxCoreClasses, dxMessages, cxControls, cxContainer, cxGraphics, cxVariants, dxForms,
  cxStorage, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, dxCustomHint,
  cxInplaceContainer, cxCheckBox, cxCheckListBox, cxNavigator, cxEdit, cxListBox,
  cxData, cxDataUtils, cxCustomData, cxStyles, cxGeometry, cxMemo, dxGDIPlusClasses,
  cxTLStrs, cxScrollBar, cxLibraryConsts, cxDataStorage, cxPC, cxLookupEdit, dxUxTheme,
  dxSpreadSheetTypes, dxSpreadSheetConditionalFormatting, dxUIElementPopupWindow,
  cxDataControllerConditionalFormatting, cxFilter, cxFilterControl, dxFilterPopupWindow;

const
  WM_CHECKSTATE = WM_DX + 100;

  // default size values for TreeList items
  cxTreeListDefWidth           = 100;
  cxTreeListDefMinWidth        = 20;

  // default values for TreeList options view
  cxTreeListHeaderMovingZoneSize   = 20;
  cxTreeListDefIndicatorWidth      = 12;
  cxTreeListDefSeparatorWidth      = 2;
  cxTreeListDefDropArrowColor      = $00FF00;
  CustomizationPopupColumnOffset   = 10;

  // TreeList styles predefined indexes
  tlcs_Header = ecs_EditContainerStylesMaxIndex + 1;
  tlcs_Footer = ecs_EditContainerStylesMaxIndex + 2;

  // for band styles
  tlbs_Content          = 0;
  tlbs_Footer           = 1;
  tlbs_Header           = 2;
  tlbs_HeaderBackground = 3;

  // for control styles
  tlsv_Content        = ecs_Content;
  tlsv_BandBackground = ecs_EditingStylesMaxIndex + 1;
  tlsv_BandContent    = ecs_EditingStylesMaxIndex + 2;
  tlsv_BandHeader     = ecs_EditingStylesMaxIndex + 3;
  tlsv_ColumnFooter   = ecs_EditingStylesMaxIndex + 4;
  tlsv_ColumnHeader   = ecs_EditingStylesMaxIndex + 5;
  tlsv_ContentEven    = ecs_EditingStylesMaxIndex + 6;
  tlsv_ContentOdd     = ecs_EditingStylesMaxIndex + 7;
  tlsv_Footer         = ecs_EditingStylesMaxIndex + 8;
  tlsv_HotTrack       = ecs_EditingStylesMaxIndex + 12;
  tlsv_IncSearch      = ecs_EditingStylesMaxIndex + 9;
  tlsv_Indicator      = ecs_EditingStylesMaxIndex + 10;
  tlsv_Preview        = ecs_EditingStylesMaxIndex + 11;

  // base constants for descendants
  cxtlMaxControlStyleIndex = tlsv_Preview;
  cxtlMaxColumnStyleIndex  = tlcs_Footer;
  cxtlMaxBandStyleIndex    = tlbs_HeaderBackground;

  // base index for HitCodes bits
  tlhc_Base                          = echc_MaxIndex + 1;
  tlhc_HitAtBackground               = tlhc_Base;
  tlhc_HitAtBand                     = tlhc_Base + 1;
  tlhc_HitAtBandContainer            = tlhc_Base + 2;
  tlhc_HitAtBandCustomizing          = tlhc_Base + 3;
  tlhc_HitAtBandHeader               = tlhc_Base + 4;
  tlhc_HitAtButton                   = tlhc_Base + 5;
  tlhc_HitAtCheckButton              = tlhc_Base + 6;
  tlhc_HitAtColumn                   = tlhc_Base + 7;
  tlhc_HitAtColumnCustomizing        = tlhc_Base + 8;
  tlhc_HitAtColumnHeader             = tlhc_Base + 9;
  tlhc_HitAtFooter                   = tlhc_Base + 10;
  tlhc_HitAtFooterItem               = tlhc_Base + 11;
  tlhc_HitAtGroupFooter              = tlhc_Base + 12;
  tlhc_HitAtGroupFooterItem          = tlhc_Base + 13;
  tlhc_HitAtImage                    = tlhc_Base + 14;
  tlhc_HitAtIndent                   = tlhc_Base + 15;
  tlhc_HitAtIndicator                = tlhc_Base + 16;
  tlhc_HitAtNode                     = tlhc_Base + 17;
  tlhc_HitAtNodePreview              = tlhc_Base + 18;
  tlhc_HitAtSeparator                = tlhc_Base + 19;
  tlhc_HitAtSizingHorz               = tlhc_Base + 20;
  tlhc_HitAtSizingVert               = tlhc_Base + 21;
  tlhc_HitAtStateImage               = tlhc_Base + 22;
  tlhc_HitAtNavigator                = tlhc_Base + 23;
  tlhc_HitAtColumnHeaderFilterButton = tlhc_Base + 24;

  // events state
  tlesNone      = $0000;
  tlesChanged   = $0001;
  tlesSelection = $0002;

  // hit test and sizing constants
  cxtlHitDelta = 3;
  cxtlScrollDelta = 10;
  cxtlSizingMarkWidth = 1;

  //
  cxtlEditCell         = 1;
  cxtlPreviewCell      = 2;
  cxtlBandPartCell     = 3;
  cxtlColumnHeaderCell = 4;
  cxtlBandHeaderCell   = 5;
  cxtlIndentCell       = 6;
  cxtlIndicatorCell    = 7;
  cxtlBackgroundCell   = 8;
  cxtlFooterCell       = 9;

  // popup menu commands

  // Summary commands
  tlcmNone     = 0;
  tlcmSum      = 1;
  tlcmMin      = 2;
  tlcmMax      = 3;
  tlcmCount    = 4;
  tlcmAverage  = 5;
  tlcmAllNodes = 6;
  // Column Header commands
  tlcmSortAscending                   = 20;
  tlcmSortDescending                  = 21;
  tlcmClearSorting                    = 22;
  tlcmFooter                          = 23;
  tlcmGroupFootersInvisible           = 24;
  tlcmGroupFootersVisibleWhenExpanded = 25;
  tlcmGroupFootersAlwaysVisible       = 26;
  tlcmRemoveThisColumn                = 27;
  tlcmFieldChooser                    = 28;
  tlcmHorzAlignmentLeft               = 29;
  tlcmHorzAlignmentRight              = 30;
  tlcmHorzAlignmentCenter             = 31;
  tlcmVertAlignmentTop                = 32;
  tlcmVertAlignmentBottom             = 33;
  tlcmVertAlignmentCenter             = 34;
  tlcmBestFit                         = 35;
  tlcmBestFitAllColumns               = 36;
  tlcmUser                            = tlcmBestFitAllColumns + 1000;

  // overs
  cvis_ViewInfoCalculate = 4;
  //
  cxColumnTextSeparator = #9;
  cxTreeListScrollWidthDragInterval = 50;
  cxTreeListIndentOffsetSize       = 6;
  cxTreeListEditCellHeightOffset   = 2;
  cxTreeListDragDropTextAreaOffset: TPoint = (X: 20; Y:0);


type
  EcxTreeList = class(Exception);

  TcxCustomTreeListClass = class of TcxCustomTreeList;
  TcxTreeListController = class;
  TcxTreeListDataController = class;

  TcxTreeListColumn = class;
  TcxTreeListColumnSummary = class;

  TcxTreeListPopupMenu = class;
  TcxTreeListPopupMenus = class;

  TcxTreeListSummary = class;
  TcxTreeListSummaryItem = class;
  TcxTreeListSummaryItems = class;

  TcxTreeListBand = class;
  TcxTreeListBands = class;
  TcxTreeListBandRow = class;
  TcxTreeListBandRows = class;

  TcxTreeListOptionsBehavior = class;
  TcxTreeListOptionsSelection = class;

  TcxTreeListOptionsCustomizing = class;
  TcxTreeListOptionsCustomizingClass = class of TcxTreeListOptionsCustomizing;

  TcxTreeListHitTest = class;

  TcxTreeListNodeViewData = class;
  TcxTreeListLevelInfo = class;

  TcxTreeListViewInfo = class;
  TcxTreeListCustomCellViewInfo = class;
  TcxTreeListCustomCellViewInfoClass = class of TcxTreeListCustomCellViewInfo;

  TcxTreeListCustomHeaderCellViewInfo = class;
  TcxTreeListHeaderCellViewInfo = class;
  TcxTreeListColumnHeaderCellViewInfo = class;
  TcxTreeListEditCellViewInfo = class;
  TcxTreeListIndicatorCellViewInfo = class;

  TcxTreeListCustomizationForm = class;

  TcxTreeListOptionsView = class;

  TcxTreeListPainter = class;

  TcxTreeListNode = class;
  TcxTreeListNodeClass = class of TcxTreeListNode;

  TcxTreeListPreview = class;
  TcxTreeListPreviewClass = class of TcxTreeListPreview;
  TcxCustomTreeList = class;
  TcxTreeListConditionalFormattingProvider = class;

  TcxTreeListCustomizing = class;
  TcxTreeListItemsCustomizeListBox = class;

  TcxTreeListStyles = class;

  // overriding types
  PIntArray = ^TIntArray;

  TIntArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;

  TcxTreeListBandFixedKind = (tlbfNone, tlbfLeft, tlbfRight);

  TcxTreeListBandExpandable = (tlbeDefault, tlbeExpandable, tlbeNotExpandable);

  TcxTreeListGridLines = (tlglNone, tlglHorz, tlglVert, tlglBoth);

  TcxTreeListImageIndexType = (tlitImageIndex, tlitSelectedIndex, tlitStateIndex, tlitOverlayIndex, tlitOverlayStateIndex);

  TcxTreeListImageIndexes = set of TcxTreeListImageIndexType;

  TcxTreeListNodeAttachMode = (tlamAdd, tlamAddFirst, tlamAddChild, tlamAddChildFirst, tlamInsert);
  TcxTreeListNodeAddMode = (tlnaAdd, tlnaAddFirst, tlnaInsert);
  TcxTreeListNodeImageIndexes = array[TcxTreeListImageIndexType] of TcxImageIndex;

  TcxTreeListCompareFunc = function(AItem1, AItem2: TcxTreeListNode): Integer;

  TcxCachedContentParams = array[Boolean] of TcxViewParams;

  TcxTreeListFindFunc = function(ANode: TcxTreeListNode; AData: Pointer): Boolean;

  { IcxTreeListListener }

  IcxTreeListDesigner = interface
  ['{1F4D4387-57E1-4756-9093-1124077A0F54}']
    procedure ComponentModified;
    procedure ComponentRemoved(Sender: TObject);
  end;

  IcxTreeListDesignTimeOperations = interface
  ['{781A0614-EA26-4214-85D2-10C7DA2AF22B}']
    function HasAllItems: Boolean;
    function SupportBandColumnEditor: Boolean;
    function SupportItemsEditor: Boolean;
    function SupportCreateAllItems: Boolean;
    procedure CreateAllItems(AMissingItemsOnly: Boolean = False);
  end;

  PcxTreeListNodeData = ^TcxTreeListNodeData;
  TcxTreeListNodeData = record
    Node: TcxTreeListNode;
    NodeData: Pointer;
  end;

  TcxTreeListChange = (tcStructure, tcData, tcLayout, tcImages, tcColumns,
                       tcSortOrder, tcLoading, tcSelection, tcFocusedNode, tcSummary);
  TcxTreeListChanges = set of TcxTreeListChange;

  { TcxTreeListSummaryInfo }

  TcxTreeListSummaryInfo = record
    Value: Variant;
    TempValue: Variant;
    Count: Integer;
  end;

  TcxTreeListSummaryInfos = array of TcxTreeListSummaryInfo;

  { TcxTreeListNode }

  TcxTreeListNodeState = (nsDeleting, nsHasChildren, nsValuesAssigned, nsCollapsed, nsValidIndexes, nsInternalDelete, nsHidden,
    nsHeightAssigned, nsInserting, nsEditing, nsCheck, nsCheckStateInvalid, nsDisabled, nsSaveExpanded, nsHiddenByFilter);

  TcxTreeListNodeStates = set of TcxTreeListNodeState;

  TcxTreeListNodeCheckInfo = (nciChecked, nciGrayed, nciAllowGrayed, nciCheckGroup,
                              nciRadioGroup, nciCheckValid, nciChangeCheck);

  TcxTreeListNodeCheckInfos = set of TcxTreeListNodeCheckInfo;

  TcxTreeListNodeCheckGroupType = (ncgNone, ncgCheckGroup, ncgRadioGroup);

  TcxTreeListNode = class(TPersistent, IUnknown, IcxDragSizing)
  private
    FAbsoluteIndex: Integer;
    FCount: Integer;
    FData: Pointer;
    FFirst: TcxTreeListNode;
    FHandle: Pointer;
    FHeight: Integer;
    FIndex: Integer;
    FLast: TcxTreeListNode;
    FNext: TcxTreeListNode;
    FOriginalIndex: Integer;
    FTreeList: TcxCustomTreeList;
    FParent: TcxTreeListNode;
    FPixelScrollPosition: Integer;
    FPrev: TcxTreeListNode;
    FViewData: TcxTreeListNodeViewData;
    function GetAbsoluteIndex: Integer;
    function GetAllowGrayed: Boolean;
    function GetChecked: Boolean;
    function GetCheckedCount: Integer;
    function GetChildVisibleCount: Integer;
    function GetCheckGroupType: TcxTreeListNodeCheckGroupType;
    function GetCheckState: TcxCheckBoxState;
    function GetEnabled: Boolean;
    function GetExpanded: Boolean;
    function GetFocused: Boolean;
    function GetGrayedCount: Integer;
    function GetHasCheckbox: Boolean;
    function GetHasChildren: Boolean;
    function GetHasVisibleChildren: Boolean;
    function GetHiddenByFilter: Boolean;
    function GetHotTrack: Boolean;
    function GetIndex: Integer;
    function GetIsDeleting: Boolean;
    function GetIsEditing: Boolean;
    function GetIsFirst: Boolean;
    function GetIsGroupNode: Boolean;
    function GetIsHidden: Boolean;
    function GetIsLast: Boolean;
    function GetIsPrinted: Boolean;
    function GetIsRadioGroup: Boolean;
    function GetIsVisible: Boolean;
    function GetItem(AIndex: Integer): TcxTreeListNode;
    function GetLevel: Integer;
    function GetRoot: TcxTreeListNode;
    function GetSelected: Boolean;
    function GetSummary: TcxTreeListSummary;
    function GetNodeImageIndex(AIndex: Integer): TcxImageIndex;
    function GetText(AIndex: Integer): string;
    function GetValue(AIndex: Integer): Variant;
    function GetValueCount: Integer;
    function GetVisible: Boolean;
    function GetVisibleIndex: Integer;
    procedure AdjustIndexes;
    procedure InternalInsert(AValue: TcxTreeListNode);
    procedure SetAllowGrayed(AValue: Boolean);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckGroupType(AValue: TcxTreeListNodeCheckGroupType);
    procedure SetEnabled(AValue: Boolean);
    procedure SetExpanded(AValue: Boolean);
    procedure SetFirst(AValue: TcxTreeListNode);
    procedure SetFocused(AValue: Boolean);
    procedure SetFooterSummaryValue(AIndex: Integer; AValue: Variant);
    procedure SetHasChildren(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetHiddenByFilter(AValue: Boolean);
    procedure SetItem(AIndex: Integer; AValue: TcxTreeListNode);
    procedure SetLast(AValue: TcxTreeListNode);
    procedure SetNodeImageIndex(AIndex: Integer; AValue: TcxImageIndex);
    procedure SetParentFor(AValue: TcxTreeListNode; AValidateIndexes: Boolean = True);
    procedure SetText(AIndex: Integer; const AValue: string);
    procedure SetSelected(AValue: Boolean);
    procedure SetValue(AIndex: Integer; const AValue: Variant);
    procedure SetVisible(AValue: Boolean);
  protected
    FImageIndexes: TcxTreeListNodeImageIndexes;
    FSummaryInfo: TcxTreeListSummaryInfos;
    FVisibleIndex: Integer;
    CheckInfo: TcxTreeListNodeCheckInfos;
    State: TcxTreeListNodeStates;
    procedure AssignData(ASource: TcxTreeListNode); virtual;
    function CanChecked: Boolean; inline;
    procedure ExtractFromParent;
    function GetFooterSummaryCount: Integer; virtual;
    function GetFooterSummaryText(AIndex: Integer): string; virtual;
    function GetFooterSummaryValue(AIndex: Integer): Variant; virtual;
    function GetInserting: Boolean; virtual;
    function GetIsFirstVisible: Boolean; virtual;
    function GetIsLastVisible: Boolean; virtual;
    function GetIsRoot: Boolean; virtual;
    function GetNextVisibleEx(IsPrev: Boolean): TcxTreeListNode;
    function GetNextSiblingEx(ANode: TcxTreeListNode; AForward: Boolean): TcxTreeListNode;
    function GetOwner: TPersistent; override;
    function GetRootParent: TcxTreeListNode;
    procedure InternalFree;
    procedure InitializeHandle; virtual;
    procedure ReadData(AStream: TStream; AVersion: Integer); virtual;
    procedure RestoreStateAfterRefresh; virtual;
    procedure SaveStateBeforeRefresh; virtual;
    procedure SetCheckState(AValue: TcxCheckBoxState); virtual;
    procedure SetChildrenCheckState(AValue: TcxCheckBoxState; AExclude: TcxTreeListNode);
    procedure UpdateCheckStates;
    procedure WriteData(AStream: TStream); virtual;
    // IcxDragSizing
    function CanSizing(ADirection: TcxDragSizingDirection): Boolean;
    function GetSizingBoundsRect(ADirection: TcxDragSizingDirection): TRect;
    function GetSizingIncrement(ADirection: TcxDragSizingDirection): Integer;
    function IsDynamicUpdate: Boolean;
    procedure PopulateItems(AList: TdxFastList);
    procedure SetSizeDelta(ADirection: TcxDragSizingDirection; ADelta: Integer);
    procedure UpdateItems(AList: TdxFastList);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property CheckedCount: Integer read GetCheckedCount;
    property GrayedCount: Integer read GetGrayedCount;
    property Handle: Pointer read FHandle write FHandle;
    property HiddenByFilter: Boolean read GetHiddenByFilter write SetHiddenByFilter;
    property OriginalIndex: Integer read FOriginalIndex write FOriginalIndex;
    property IsPrinted: Boolean read GetIsPrinted;
    property IsRadioGroup: Boolean read GetIsRadioGroup;
    property IsRoot: Boolean read GetIsRoot;
    property Summary: TcxTreeListSummary read GetSummary;
    property ViewData: TcxTreeListNodeViewData read FViewData write FViewData;
  public
    constructor Create(AOwner: TcxCustomTreeList); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValues(const AValues: array of Variant);
    function AddChild: TcxTreeListNode; virtual;
    function AddChildFirst: TcxTreeListNode; virtual;
    procedure AlphaSort(ARecurse: Boolean = False);
    function CanCollapse: Boolean;
    function CanExpand: Boolean;
    function CanMove(ADest: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode): Boolean;
    procedure CheckClick;
    procedure Collapse(Recurse: Boolean);
    procedure CustomSort(ASortProc: TcxTreeListCompareFunc; ARecurse: Boolean = False);
    procedure Delete;
    procedure DeleteChildren;
    function DisplayRect(AEntryOnly: Boolean): TRect;
    procedure EndEdit(Cancel: Boolean);
    procedure Expand(Recurse: Boolean);
    function GetNext: TcxTreeListNode;
    function GetNextChild(ANode: TcxTreeListNode): TcxTreeListNode;
    function getNextSibling: TcxTreeListNode; inline;{GetNextSibling conflicts with C++ macro}
    function GetNextSiblingVisible: TcxTreeListNode;
    function GetNextVisible: TcxTreeListNode;
    function GetPrev: TcxTreeListNode;
    function GetPrevChild(ANode: TcxTreeListNode): TcxTreeListNode;
    function getPrevSibling: TcxTreeListNode; inline;{GetPrevSibling conflicts with a C++ macro}
    function GetPrevSiblingVisible: TcxTreeListNode;
    function GetPrevVisible: TcxTreeListNode;
    function getFirstChild: TcxTreeListNode; inline;{GetFirstChild conflicts with C++ macro}
    function GetFirstChildVisible: TcxTreeListNode;
    function GetLastChild: TcxTreeListNode; inline;
    function GetLastChildVisible: TcxTreeListNode;
    //
    function HasAsParent(ANode: TcxTreeListNode): Boolean;
    function IndexOf(ANode: TcxTreeListNode): Integer;
    function InsertChild(ABeforeNode: TcxTreeListNode): TcxTreeListNode; virtual;
    procedure Invalidate;
    function IsMultiThreadedSorting: Boolean;
    function IsSibling(ANode: TcxTreeListNode): Boolean;
    procedure LoadChildren;
    procedure MakeVisible;
    procedure MoveTo(ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode); virtual;
    procedure Repaint(ARecalculate: Boolean); virtual;

    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property AllowGrayed: Boolean read GetAllowGrayed write SetAllowGrayed;
    property Checked: Boolean read GetChecked write SetChecked;
    property CheckGroupType: TcxTreeListNodeCheckGroupType read GetCheckGroupType write SetCheckGroupType;
    property CheckState: TcxCheckBoxState read GetCheckState write SetCheckState;
    property ChildVisibleCount: Integer read GetChildVisibleCount;
    property Count: Integer read FCount;
    property Data: Pointer read FData write FData;
    property Deleting: Boolean read GetIsDeleting;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Focused: Boolean read GetFocused write SetFocused;
    property FooterSummaryCount: Integer read GetFooterSummaryCount;
    property FooterSummaryTexts[Index: Integer]: string read GetFooterSummaryText;
    property FooterSummaryValues[Index: Integer]: Variant read GetFooterSummaryValue
      write SetFooterSummaryValue;
    property HasCheckbox: Boolean read GetHasCheckbox;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property HasVisibleChildren: Boolean read GetHasVisibleChildren;
    property Height: Integer read FHeight write SetHeight;
    property HotTrack: Boolean read GetHotTrack;
    property ImageIndex: TcxImageIndex index 0 read GetNodeImageIndex write SetNodeImageIndex;
    property Index: Integer read GetIndex;
    property Inserting: Boolean read GetInserting;
    property IsEditing: Boolean read GetIsEditing;
    property IsFirst: Boolean read GetIsFirst;
    property IsFirstVisible: Boolean read GetIsFirstVisible;
    property IsGroupNode: Boolean read GetIsGroupNode;
    property IsHidden: Boolean read GetIsHidden;
    property IsLast: Boolean read GetIsLast;
    property IsLastVisible: Boolean read GetIsLastVisible;
    property IsVisible: Boolean read GetIsVisible;
    property Items[Index: Integer]: TcxTreeListNode read GetItem write SetItem; default;
    property Level: Integer read GetLevel;
    property OverlayIndex: TcxImageIndex index 3 read GetNodeImageIndex write SetNodeImageIndex;
    property OverlayStateIndex: TcxImageIndex index 4 read GetNodeImageIndex write SetNodeImageIndex;
    property Parent: TcxTreeListNode read FParent;
    property Root: TcxTreeListNode read GetRoot;
    property Selected: Boolean read GetSelected write SetSelected;
    property SelectedIndex: TcxImageIndex index 1 read GetNodeImageIndex write SetNodeImageIndex;
    property StateIndex: TcxImageIndex index 2 read GetNodeImageIndex write SetNodeImageIndex;
    property Texts[Index: Integer]: string read GetText write SetText;
    property TreeList: TcxCustomTreeList read FTreeList;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: Variant read GetValue write SetValue;
    property Visible: Boolean read GetVisible write SetVisible;
    property VisibleIndex: Integer read GetVisibleIndex;
  end;

  { TcxTreeListRootNode }

  TcxTreeListRootNode = class(TcxTreeListNode)
  protected
    function GetFooterSummaryCount: Integer; override;
    function GetFooterSummaryText(AIndex: Integer): string; override;
    function GetIsRoot: Boolean; override;
    procedure InitializeHandle; override;
  end;

  { TcxUnboundTreeListNode }

  TcxUnboundTreeListNode = class(TcxTreeListNode)
  protected
    procedure ReadData(AStream: TStream; AVersion: Integer); override;
    procedure WriteData(AStream: TStream); override;
  public
    constructor Create(AOwner: TcxCustomTreeList); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxTreeListColumnStyles }

  TcxTreeListColumnStyles = class(TcxEditContainerStyles)
  private
    function GetBand: TcxTreeListBand;
    function GetColumn: TcxTreeListColumn;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetContentParams(ANode: TcxTreeListNode): TcxViewParams;

    property Band: TcxTreeListBand read GetBand;
    property Column: TcxTreeListColumn read GetColumn;
    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property Header: TcxStyle index tlcs_Header read GetValue write SetValue;
    property Footer: TcxStyle index tlcs_Footer read GetValue write SetValue;
    property Content;
  end;

  { TcxTreeListColumnPosition }

  TcxTreeListColumnPosition = class(TcxOwnedPersistent)
  private
    FBand: TcxTreeListBand;
    FBandIndex: Integer;
    FColIndex: Integer;
    FLineCount: Integer;
    FRow: TcxTreeListBandRow;
    FRowIndex: Integer;
    FVisibleColIndex: Integer;
    function GetBandIndex: Integer;
    function GetColIndex: Integer;
    function GetColumn: TcxTreeListColumn;
    function GetIsUpdating: Boolean;
    function GetRowIndex: Integer;
    function GetTreeList: TcxCustomTreeList;
    function GetVisibleBandIndex: Integer;
    function GetVisibleRowIndex: Integer;
    procedure SetBandIndex(AValue: Integer);
    procedure SetColIndex(AValue: Integer);
    procedure SetLineCount(AValue: Integer);
    procedure SetRow(AValue: TcxTreeListBandRow);
    procedure SetRowIndex(AValue: Integer);
  protected
    procedure Changed; virtual;
    function IsPositionChanged: Boolean; virtual;
    procedure Restore(ABandsIndexOnly: Boolean); virtual;
    procedure Store(ABandIndexOnly: Boolean); virtual;

    property IsUpdating: Boolean read GetIsUpdating;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetPosition(AColIndex, ARowIndex: Integer; IsInsertRow: Boolean = False);
    procedure SetPositionEx(ABandIndex, AColIndex, ARowIndex: Integer; IsInsertRow: Boolean = False);

    property Band: TcxTreeListBand read FBand;
    property Column: TcxTreeListColumn read GetColumn;
    property Row: TcxTreeListBandRow read FRow write SetRow;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleBandIndex: Integer read GetVisibleBandIndex;
    property VisibleColIndex: Integer read FVisibleColIndex;
    property VisibleRowIndex: Integer read GetVisibleRowIndex;
  published
    property ColIndex: Integer read GetColIndex write SetColIndex;
    property LineCount: Integer read FLineCount write SetLineCount default 1;
    property RowIndex: Integer read GetRowIndex write SetRowIndex;
    property BandIndex: Integer read GetBandIndex write SetBandIndex;
  end;

  TcxTreeListColumnPositionClass = class of TcxTreeListColumnPosition;

  { TcxTreeListCaption }

  TcxTreeListCaption = class(TcxOwnedPersistent)
  private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FGlyph: TdxSmartGlyph;
    FGlyphAlignHorz: TAlignment;
    FGlyphAlignVert: TcxAlignmentVert;
    FMultiLine: Boolean;
    FShowEndEllipsis: Boolean;
    FText: string;
    FOnChange: TNotifyEvent;

    procedure GlyphChanged(Sender: TObject);
    function IsAlignVertStored: Boolean;
    function IsGlyphAlignVertStored: Boolean;
    procedure SetAlignHorz(AValue: TAlignment);
    procedure SetAlignVert(AValue: TcxAlignmentVert);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetGlyphAlignHorz(AValue: TAlignment);
    procedure SetGlyphAlignVert(AValue: TcxAlignmentVert);
    procedure SetMultiLine(AValue: Boolean);
    procedure SetShowEndEllipsis(AValue: Boolean);
    procedure SetText(const AValue: string);
  protected
    procedure Changed; virtual;
    function GetText: string; virtual;
    function IsTextStored: Boolean; virtual;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AlignHorz: TAlignment read FAlignHorz write SetAlignHorz default taLeftJustify;
    property AlignVert: TcxAlignmentVert read FAlignVert write SetAlignVert stored IsAlignVertStored;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property GlyphAlignHorz: TAlignment read FGlyphAlignHorz write SetGlyphAlignHorz default taLeftJustify;
    property GlyphAlignVert: TcxAlignmentVert read FGlyphAlignVert write SetGlyphAlignVert stored IsGlyphAlignVertStored;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default True;
    property Text: string read GetText write SetText stored IsTextStored;
  end;

  { TcxTreeListColumnFilterPopupIncrementalFilteringOptions }

  TcxTreeListColumnFilterPopupIncrementalFilteringOptions = class(TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptions)
  published
    property Enabled;
    property Options;
  end;

  { TcxTreeListColumnFilterPopupOptions }

  TcxTreeListColumnFilterPopupOptions = class(TcxCustomEditContainerItemFilterPopupOptions)
  private
    function GetIncrementalFiltering: TcxTreeListColumnFilterPopupIncrementalFilteringOptions;
    procedure SetIncrementalFiltering(AValue: TcxTreeListColumnFilterPopupIncrementalFilteringOptions);
  protected
    function GetIncrementalFilteringOptionsClass: TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptionsClass; override;
  published
    property AddValueItems;
    property Enabled;
    property FilteredItemsList;
    property IncrementalFiltering: TcxTreeListColumnFilterPopupIncrementalFilteringOptions read GetIncrementalFiltering
      write SetIncrementalFiltering;
    property MRUItemsList;
    property MultiSelect;
  end;

  { TcxTreeListColumnExcelFilterPopupOptions }

  TcxTreeListColumnExcelFilterPopupOptions = class(TcxCustomEditContainerItemExcelFilterPopupOptions)
  published
    property ApplyChanges;
    property DateTimeValuesPageType;
    property DefaultPage;
    property FilteredItemsList;
    property NumericValuesPageType;
  end;

  { TcxTreeListColumnOptions }

  TcxTreeListColumnOptions = class(TcxCustomEditContainerItemOptions)
  private
    FCellEndEllipsis: Boolean;
    FEditAutoHeight: TcxItemInplaceEditAutoHeight;
    FFooter: Boolean;
    FGroupFooter: Boolean;
    FHidden: Boolean;
    FSizing: Boolean;
    FVertSizing: Boolean;
    function GetColumn: TcxTreeListColumn;
    function GetExcelFilterPopup: TcxTreeListColumnExcelFilterPopupOptions;
    function GetFilterPopup: TcxTreeListColumnFilterPopupOptions;
    procedure SetCellEndEllipsis(AValue: Boolean);
    procedure SetEditAutoHeight(AValue: TcxItemInplaceEditAutoHeight);
    procedure SetExcelFilterPopup(AValue: TcxTreeListColumnExcelFilterPopupOptions);
    procedure SetFilterPopup(AValue: TcxTreeListColumnFilterPopupOptions);
    procedure SetFooter(AValue: Boolean);
    procedure SetGroupFooter(AValue: Boolean);
    procedure SetSizing(AValue: Boolean);
    procedure SetVertSizing(AValue: Boolean);
  protected
    procedure Changed; override;
    function GetExcelFilterPopupOptionsClass: TcxCustomEditContainerItemExcelFilterPopupOptionsClass; override;
    function GetFilterPopupOptionsClass: TcxCustomEditContainerItemFilterPopupOptionsClass; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;

    property Column: TcxTreeListColumn read GetColumn;
  published
    property CellEndEllipsis: Boolean read FCellEndEllipsis write SetCellEndEllipsis default True;
    property EditAutoHeight: TcxItemInplaceEditAutoHeight read FEditAutoHeight write SetEditAutoHeight default ieahDefault;
    property Footer: Boolean read FFooter write SetFooter default True;
    property GroupFooter: Boolean read FGroupFooter write SetGroupFooter default True;
    property Hidden: Boolean read FHidden write FHidden default False;
    property Sizing: Boolean read FSizing write SetSizing default True;
    property VertSizing: Boolean read FVertSizing write SetVertSizing default True;
    property Customizing;
    property Editing;
    property ExcelFilterPopup: TcxTreeListColumnExcelFilterPopupOptions read GetExcelFilterPopup write SetExcelFilterPopup;
    property Filtering;
    property FilterPopup: TcxTreeListColumnFilterPopupOptions read GetFilterPopup write SetFilterPopup;
    property FilterPopupMode;
    property FilteringWithFindPanel;
    property Focusing;
    property IgnoreTimeForFiltering;
    property IncSearch;
    property Moving;
    property ShowEditButtons;
    property Sorting;
    property TabStop;
  end;

  { TcxTreeListSummaryItem }

  TcxTreeListSummaryItemGetTextEvent = procedure(Sender: TcxTreeListSummaryItem;
    const AValue: Variant; var AText: string) of object;

  TcxTreeListSummaryItem = class(TcxCustomDataSummaryItem)
  private
    FAbsoluteIndex: Integer;
    FAlignHorz: TAlignment;
    FAlignHorzAssigned: Boolean;
    FAlignVert: TcxAlignmentVert;
    FAlignVertAssigned: Boolean;
    FAllNodes: Boolean;
    FCalculatedColumn: TcxTreeListColumn;
    FMultiLine: Boolean;
    FVisible: Boolean;
    FVisibleIndexInColumn: Integer;
    FOnGetText: TcxTreeListSummaryItemGetTextEvent;
    function GetAlignHorz: TAlignment;
    function GetAlignVert: TcxAlignmentVert;
    function GetCalculatedColumn: TcxTreeListColumn;
    function GetColumn: TcxTreeListColumn;
    function IsAlignHorzStored: Boolean;
    function IsAlignVertStored: Boolean;
    function IsCalculatedColumnStored: Boolean;
    procedure SetAlignHorz(AValue: TAlignment);
    procedure SetAlignVert(AValue: TcxAlignmentVert);
    procedure SetAllNodes(AValue: Boolean);
    procedure SetCalculatedColumn(AValue: TcxTreeListColumn);
    procedure SetMultiLine(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    //
    procedure ReadAlignHorz(Reader: TReader);
    procedure ReadAlignVert(Reader: TReader);
    procedure WriteAlignHorz(Writer: TWriter);
    procedure WriteAlignVert(Writer: TWriter);
  protected
    procedure AssignValues(Source: TcxCustomDataSummaryItem); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDataController: TcxCustomDataController; override;
    function GetValueFormat(AValueType: TcxSummaryValueType; const AValue: Variant;
      AIsFooter: Boolean): string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
  public
    constructor Create(Collection: TCollection); override;
    function FormatValue(const AValue: Variant; AIsFooter: Boolean): string; override;

    property AbsoluteIndex: Integer read FAbsoluteIndex write FAbsoluteIndex;
    property AlignHorzAssigned: Boolean read FAlignHorzAssigned write FAlignHorzAssigned;
    property AlignVertAssigned: Boolean read FAlignVertAssigned write FAlignVertAssigned;
    property Column: TcxTreeListColumn read GetColumn;
    property VisibleIndexInColumn: Integer read FVisibleIndexInColumn;
  published
    property AlignHorz: TAlignment read GetAlignHorz write SetAlignHorz stored IsAlignHorzStored;
    property AlignVert: TcxAlignmentVert read GetAlignVert write SetAlignVert stored IsAlignVertStored;
    property AllNodes: Boolean read FAllNodes write SetAllNodes default True;
    property CalculatedColumn: TcxTreeListColumn read GetCalculatedColumn write SetCalculatedColumn stored IsCalculatedColumnStored;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Format;
    property Kind;
    property OnGetText: TcxTreeListSummaryItemGetTextEvent read FOnGetText write FOnGetText;
  end;

  TcxTreeListSummaryItemClass = class of TcxTreeListSummaryItem;

  { TcxTreeListColumnSummary }

  TcxTreeListColumnSummary = class(TcxOwnedPersistent)
  private
    FFooterSummaryItems: TcxTreeListSummaryItems;
    FGroupFooterSummaryItems: TcxTreeListSummaryItems;
    function GetColumn: TcxTreeListColumn;
    function GetFooterVisibleCount: Integer;
    function GetGroupFooterVisibleCount: Integer;
    procedure SetFooterSummaryItems(AValue: TcxTreeListSummaryItems);
    procedure SetGroupFooterSummaryItems(AValue: TcxTreeListSummaryItems);
  protected
    procedure Changed(ARedrawOnly: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Column: TcxTreeListColumn read GetColumn;
    property FooterVisibleCount: Integer read GetFooterVisibleCount;
    property GroupFooterVisibleCount: Integer read GetGroupFooterVisibleCount;
  published
    property FooterSummaryItems: TcxTreeListSummaryItems read FFooterSummaryItems write SetFooterSummaryItems;
    property GroupFooterSummaryItems: TcxTreeListSummaryItems read FGroupFooterSummaryItems write SetGroupFooterSummaryItems;
  end;

  { TcxTreeListSummaryItems }

  TcxTreeListSummaryItems = class(TcxCollection)
  private
    FSummary: TcxTreeListColumnSummary;
    FVisibleCount: Integer;
    function GetColumn: TcxTreeListColumn;
    function GetItem(AIndex: Integer): TcxTreeListSummaryItem;
    procedure SetItem(AIndex: Integer; AValue: TcxTreeListSummaryItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ASummary: TcxTreeListColumnSummary; AItemClass: TcxTreeListSummaryItemClass); virtual;
    function Add: TcxTreeListSummaryItem;
    function GetItemByKind(AKind: TcxSummaryKind): TcxTreeListSummaryItem;

    property Column: TcxTreeListColumn read GetColumn;
    property Items[Index: Integer]: TcxTreeListSummaryItem read GetItem write SetItem; default;
    property Summary: TcxTreeListColumnSummary read FSummary;
    property VisibleCount: Integer read FVisibleCount;
  end;

  TcxTreeListColumnSummaryClass = class of TcxTreeListColumnSummary;

  { TcxTreeListColumn }

  TcxTreeListOnGetDisplayTextEvent = procedure(Sender: TcxTreeListColumn;
    ANode: TcxTreeListNode; var Value: string) of object;
  TcxTreeListGetEditPropertiesEvent = procedure(Sender: TcxTreeListColumn;
    ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties) of object;
  TcxTreeListValidateDrawValueEvent = procedure(Sender: TcxTreeListColumn;
    ANode: TcxTreeListNode; const AValue: Variant; AData: TcxEditValidateInfo) of object;

  TcxTreeListColumn = class(TcxCustomInplaceEditContainer, IcxDragSizing, IcxStoredObject)
  private
    FBestFitMaxWidth: Integer;
    FCaption: TcxTreeListCaption;
    FCalculatedWidth: Integer;
    FData: Integer;
    FHeaderCell: TcxTreeListColumnHeaderCellViewInfo;
    FStatusHint: string;
    FIsCurrency: Boolean;
    FIsTextStored: Boolean;
    FMinWidth: Integer;
    FOnChangeName: TNotifyEvent;
    FPosition: TcxTreeListColumnPosition;
    FSortIndex: Integer;
    FSortOrder: TcxDataSortOrder;
    FStoreID: Integer;
    FSummary: TcxTreeListColumnSummary;
    FSummaryFooter: TPersistent;
    FVisible: Boolean;
    FWidth: Integer;
    FOnGetDisplayText: TcxTreeListOnGetDisplayTextEvent;
    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;
    FOnValidateDrawValue: TcxTreeListValidateDrawValueEvent;
    function GetActuallyVisible: Boolean;
    function GetDataController: TcxTreeListDataController;
    function GetDataBinding: TcxItemDataBinding;
    function GetDisplayText(ANode: TcxTreeListNode): string;
    function GetDisplayWidth: Integer;
    function GetEditingProperties: TcxTreeListGetEditPropertiesEvent;
    function GetEditProperties: TcxTreeListGetEditPropertiesEvent;
    function GetHasIndent: Boolean;
    function GetID: Integer;
    function GetIndentWidth: Integer;
    function GetIsFixed: Boolean;
    function GetIsHidden: Boolean;
    function GetIsLeft: Boolean;
    function GetIsPreview: Boolean;
    function GetIsRight: Boolean;
    function GetIsLoading: Boolean;
    function GetIsReading: Boolean;
    function GetIsUpdating: Boolean;
    function GetIsWidthStored: Boolean;
    function GetItemIndex: Integer;
    function GetOptions: TcxTreeListColumnOptions;
    function GetSortIndex: Integer;
    function GetStyles: TcxTreeListColumnStyles;
    function GetTreeList: TcxCustomTreeList;
    function GetValueByNode(ANode: TcxTreeListNode): Variant;
    function GetValueDef: TcxValueDef;
    function GetVisibleIndex: Integer;
    function GetWidth: Integer;
    procedure SetBestFitMaxWidth(AValue: Integer);
    procedure SetCaption(AValue: TcxTreeListCaption);
    procedure SetDataBinding(AValue: TcxItemDataBinding);
    procedure SetDisplayWidth(AValue: Integer);
    procedure SetEditingProperties(AValue: TcxTreeListGetEditPropertiesEvent);
    procedure SetEditProperties(AValue: TcxTreeListGetEditPropertiesEvent);
    procedure SetIsPreview(AValue: Boolean);
    procedure SetItemIndex(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetOptions(AValue: TcxTreeListColumnOptions);
    procedure SetPosition(AValue: TcxTreeListColumnPosition);
    procedure SetSortIndex(AValue: Integer);
    procedure SetSortOrder(AValue: TcxDataSortOrder);
    procedure SetStyles(AValue: TcxTreeListColumnStyles);
    procedure SetSummary(AValue: TcxTreeListColumnSummary);
    procedure SetSummaryFooter(AValue: TPersistent);
    procedure SetValueByNode(ANode: TcxTreeListNode; const AValue: Variant);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected
    //editing
    function CanEdit: Boolean; override;
    function CanEditAutoHeight: Boolean; virtual;
    function GetEditValue: Variant; override;
    function CanInitEditing: Boolean; override;

    procedure AssignWidth;
    procedure CancelSorting;
    function CanFind: Boolean; override;
    function CanFocus: Boolean; override;
    function CanMoving: Boolean; virtual;
    function CanSort: Boolean; virtual;
    procedure ChangeCaption(Sender: TObject); virtual;
    procedure Changed; override;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure ForceWidth(AWidth: Integer);
    function GetEditAutoHeight: TcxInplaceEditAutoHeight;
    function GetFilterCaption: string; override;
    function GetHeaderFooterBestFitSize: Integer; virtual;
    function GetOptionsClass: TcxCustomEditContainerItemOptionsClass; override;
    function GetRealSortOrder: TcxDataSortOrder; virtual;
    function GetStylesClass: TcxEditContainerStylesClass; override;
    function GetSummaryBestFitSize(ANode: TcxTreeListNode; AItems: TcxTreeListSummaryItems): Integer;
    function GetSummaryClass: TcxTreeListColumnSummaryClass; virtual;
    procedure SetEditingControl(Value: TcxEditingControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    // IcxTreeListDragSizing implementation
    function CanSizing(ADirection: TcxDragSizingDirection): Boolean; virtual;
    function GetAvailableMaxWidth: Integer; virtual;
    function GetRealMinSize: Integer;
    function GetSizingBoundsRect(ADirection: TcxDragSizingDirection): TRect; virtual;
    function GetSizingIncrement(ADirection: TcxDragSizingDirection): Integer; virtual;
    function IsDynamicUpdate: Boolean; virtual;
    procedure SetSizeDelta(ADirection: TcxDragSizingDirection; ADelta: Integer); virtual;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    function GetPropertyIndex(const AName: string): Integer;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // column methods
    procedure ChangeSortOrder(ASortOrder: TcxDataSortOrder; AShift: TShiftState);
    procedure ConvertOldFooterSummaries;
    procedure DoGetDisplayText(ARecordIndex: TdxNativeInt; var AText: string); override;
    function DoGetNodeDisplayText(ANode: TcxTreeListNode; const AValue: Variant): Variant; virtual;
    function DoOnGetDisplayText(ANode: TcxTreeListNode; AsText: Boolean = False;
      AValueOnly: Boolean = False): Variant; virtual;
    procedure DoValidateDrawValue(ANode: TcxTreeListNode; const AValue: Variant; AData: TcxEditValidateInfo); virtual;
    function GetCellHeight(ANode: TcxTreeListNode; AWidth, ALines: Integer;
      AFont: TFont; const AValue: Variant): Integer;
    function GetCurrentValue: Variant; override;
    function GetPositionClass: TcxTreeListColumnPositionClass; virtual;
    function GetIsCurrency: Boolean; virtual;
    function GetIsTextStored: Boolean; virtual;
    function HasDataTextHandler: Boolean; override;
    procedure InitAutoWidthItem(AItem: TcxAutoWidthItem); virtual;
    procedure InitializeValueDef;
    function IsVisibleInQuickCustomizationPopup: Boolean;
    procedure SetCurrentValue(const Value: Variant); override;
    procedure ValidateDrawValue(const AValue: Variant; AEditViewInfo: TcxEditCellViewInfo); override;

    property Data: Integer read FData write FData;
    property DataController: TcxTreeListDataController read GetDataController;
    property HasIndent: Boolean read GetHasIndent;
    property HeaderCell: TcxTreeListColumnHeaderCellViewInfo read FHeaderCell;
    property ID: Integer read GetID;
    property IsCurrency: Boolean read GetIsCurrency;
    property IsTextStored: Boolean read FIsTextStored;
    property IsFixed: Boolean read GetIsFixed;
    property IsLoading: Boolean read GetIsLoading;
    property IsReading: Boolean read GetIsReading;
    property IsUpdating: Boolean read GetIsUpdating;
    property StoreID: Integer read FStoreID write FStoreID;
    property ValueDef: TcxValueDef read GetValueDef;
    property Controller;
    property EditingControl;
    property EditViewData;
    property OnChangeName: TNotifyEvent read FOnChangeName write FOnChangeName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyBestFit;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure MakeVisible;
    procedure RestoreDefaults; virtual;
    procedure RestoreWidths; virtual;

    property ActuallyVisible: Boolean read GetActuallyVisible;
    property DisplayTexts[ANode: TcxTreeListNode]: string read GetDisplayText;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
    property IndentWidth: Integer read GetIndentWidth;
    property IsHidden: Boolean read GetIsHidden;
    property IsLeft: Boolean read GetIsLeft;
    property IsPreview: Boolean read GetIsPreview write SetIsPreview;
    property IsRight: Boolean read GetIsRight;
    property PropertiesValue;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleIndex: Integer read GetVisibleIndex;
    property Editing;
    property EditValue;
    property Filtered;
    property FilteringDateRanges;
    property Focused;
    property ItemIndex read GetItemIndex write SetItemIndex;
    property Value;
    property ValueCount;
    property Values[ANode: TcxTreeListNode]: Variant read GetValueByNode write SetValueByNode;
  published
    property SummaryFooter: TPersistent read FSummaryFooter write SetSummaryFooter stored False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property BestFitMaxWidth: Integer read FBestFitMaxWidth write SetBestFitMaxWidth default 0;
    property Caption: TcxTreeListCaption read FCaption write SetCaption;
    property DataBinding: TcxItemDataBinding read GetDataBinding write SetDataBinding;
    property MinWidth: Integer read FMinWidth write SetMinWidth default cxTreeListDefMinWidth;
    property Options: TcxTreeListColumnOptions read GetOptions write SetOptions;
    property Width: Integer read GetWidth write SetWidth stored GetIsWidthStored;
    property Position: TcxTreeListColumnPosition read FPosition write SetPosition;
    property SortOrder: TcxDataSortOrder read FSortOrder write SetSortOrder default soNone;
    property SortIndex: Integer read GetSortIndex write SetSortIndex default -1;
    property StatusHint: string read FStatusHint write FStatusHint;
    property Styles: TcxTreeListColumnStyles read GetStyles write SetStyles;
    property Summary: TcxTreeListColumnSummary read FSummary write SetSummary;
    property OnGetDisplayText: TcxTreeListOnGetDisplayTextEvent read FOnGetDisplayText write FOnGetDisplayText;
    property OnGetEditingProperties: TcxTreeListGetEditPropertiesEvent read GetEditingProperties write SetEditingProperties;
    property OnGetEditProperties: TcxTreeListGetEditPropertiesEvent read GetEditProperties write SetEditProperties;
    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;
    property OnGetFilterValues;
    property OnInitFilteringDateRanges;
    property OnValidateDrawValue: TcxTreeListValidateDrawValueEvent read FOnValidateDrawValue write FOnValidateDrawValue;
    property OnUserFiltering;
    property OnUserFilteringEx;
    property PropertiesClassName;
    property Properties;
    property PropertiesEvents;
    property RepositoryItem;
  end;

  TcxTreeListColumnClass = class of TcxTreeListColumn;

  { TcxTreeListSizingDragAndDropObject }

  TcxTreeListSizingDragAndDropObject = class(TcxSizingDragAndDropObject)
  private
    FOffset: TPoint;
    FDragItem: TObject;
  protected
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetSizingMarkBounds: TRect; override;
  end;

  { TcxTreeListFindPanel }

  TcxTreeListFindPanel = class(TcxControlFindPanel)
  protected
    function GetClearButtonCaption: string; override;
    function GetDefaultInfoText: string; override;
    function GetFindButtonCaption: string; override;
  end;

  { TcxTreeListDragAndDropObject }

  TcxTreeListDropInfo = class
  public
    Accepted: Boolean;
    Band: TcxTreeListBand;
    ColIndex: Integer;
    Customizing: Boolean;
    DropPos: TPoint;
    Position: TcxPosition;
  end;

  TcxTreeListDragAndDropObject = class(TcxCustomControlDragAndDropObject)
  private
    FArrows: TcxPlaceArrows;
    FDropInfo: TcxTreeListDropInfo;
    function DrawArrowsNeeded: Boolean;
    function GetActualDropPosition: TcxPosition;
    function GetBands: TcxTreeListBands;
    function GetCustomizing: Boolean;
    function GetDestBand: TcxTreeListBand;
    function GetDragHeader: TcxTreeListHeaderCellViewInfo;
    function GetDropPosition: TcxPosition;
    function GetHitTest: TcxTreeListHitTest;
    function GetOptionsCustomize: TcxTreeListOptionsCustomizing;
    function GetPainter: TcxTreeListPainter;
    function GetTreeList: TcxCustomTreeList;
    function GetViewInfo: TcxTreeListViewInfo;
  protected
    FOrigin: TPoint;
    procedure BeginDragAndDrop; override;
    procedure ChangeArrowsPosition;
    function CanCustomize: Boolean; virtual; abstract;
    function CanDrop: Boolean; virtual; abstract;
    function CanRemove: Boolean; virtual; abstract;
    function CheckInCustomizing(const APoint: TPoint): Boolean;
    procedure CheckDragPosition; virtual; abstract;
    function CreateDropInfo: TcxTreeListDropInfo; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function Drop: Boolean; virtual; abstract;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetArrowAreaBounds: TRect; virtual;
    function GetDisplayRect: TRect; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    procedure InitDropInfo(const P: TPoint); virtual;
    procedure InitializeScrollArea(const Area: TRect); virtual;
    procedure MakeCustomizingPageVisible; virtual; abstract;
    procedure Paint; override;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    property ActualDropPosition: TcxPosition read GetActualDropPosition;
    property Arrows: TcxPlaceArrows read FArrows;
    property Bands: TcxTreeListBands read GetBands;
    property Customizing: Boolean read GetCustomizing;
    property DestBand: TcxTreeListBand read GetDestBand;
    property DragHeader: TcxTreeListHeaderCellViewInfo read GetDragHeader;
    property DropInfo: TcxTreeListDropInfo read FDropInfo;
    property DropPosition: TcxPosition read GetDropPosition;
    property HitTest: TcxTreeListHitTest read GetHitTest;
    property OptionsCustomizing: TcxTreeListOptionsCustomizing read GetOptionsCustomize;
    property Painter: TcxTreeListPainter read GetPainter;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property ViewInfo: TcxTreeListViewInfo read GetViewInfo;
  end;

  { TcxTreeListDragAndDropBandObject }

  TcxTreeListDragAndDropBandObject = class(TcxTreeListDragAndDropObject)
  private
    procedure AfterDropPositionChange;
    procedure BeforeDropPositionChange;
    function GetBand: TcxTreeListBand;
    function GetDropColIndex: Integer;
    function GetDropParentBandIndex: Integer;
  protected
    function CanCustomize: Boolean; override;
    function CanDrop: Boolean; override;
    function CanRemove: Boolean; override;
    function CheckBandInsertAt(ABand: TcxTreeListBand): Boolean;
    procedure CheckDragPosition; override;
    function Drop: Boolean; override;
    function GetBoundsForInsert(ABand: TcxTreeListBand): TRect;
    procedure MakeCustomizingPageVisible; override;
  public
    property Band: TcxTreeListBand read GetBand;
  end;

  { TcxTreeListDragAndDropColumnObject }

  TcxTreeListColumnDropInfo = class(TcxTreeListDropInfo)
  public
    Area: TRect;
    Column: TcxTreeListColumn;
    RowIndex: Integer;
    InsertRow: Boolean;
  end;

  TcxTreeListDragAndDropColumnObject = class(TcxTreeListDragAndDropObject)
  private
    function GetColumn: TcxTreeListColumn;
    function GetDropInfo: TcxTreeListColumnDropInfo;
  protected
    function CanCustomize: Boolean; override;
    function CanDrop: Boolean; override;
    procedure CheckDropColumnIndex(ARow: TcxTreeListBandRow; var AColIndex: Integer);
    function CanRemove: Boolean; override;
    function ChangeColumnOnly: Boolean;
    function CheckBandDropArea(ABand: TcxTreeListBand): Boolean;
    procedure CheckDragPosition; override;
    function CheckOnlyOwnColumns: Boolean;
    function CreateDropInfo: TcxTreeListDropInfo; override;
    function Drop: Boolean; override;
    function GetArrowAreaBounds: TRect; override;
    procedure InitDropInfoFromColumn(AColumn: TcxTreeListColumn);
    procedure MakeCustomizingPageVisible; override;
  public
    property Column: TcxTreeListColumn read GetColumn;
    property DropInfo: TcxTreeListColumnDropInfo read GetDropInfo;
  end;

  { TcxTreeListCellNavigator }

  TcxTreeListCellNavigator = class(TcxCustomCellNavigator)
  private
    FColumnChanged: Boolean;
    FCurrentNavigationColumn: TcxTreeListColumn;
    function GetCellIndex(AColumn: TcxTreeListColumn; ARow: TcxTreeListBandRow): Integer;
    function GetController: TcxTreeListController;
    function GetFocusedNode: TcxTreeListNode;
    function GetTreeList: TcxCustomTreeList;
    function GoToNextCellInBand(AForward: Boolean; AColumn: TcxTreeListColumn; var ACellIndex: Integer): Boolean;
    function GoToNextNodeCell(AForward: Boolean; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var ACellIndex: Integer): Boolean;
  protected
    procedure AppendRecord(var ARowIndex, ACellIndex: Integer);
    procedure CalcNextRow(AForward: Boolean; var ARowIndex, ACellIndex: Integer); override;
    function GetCount(ARecordIndex: Integer): Integer; override;
    function GetCellContainer(ARowIndex, ACellIndex: Integer): TcxCustomInplaceEditContainer; override;
    function GetNode(ARowIndex: Integer): TcxTreeListNode;
    procedure Init(var ARowIndex, ACellIndex, ARowCount: Integer); override;
    function IsGroupRow(ARowIndex: Integer): Boolean;
    procedure SaveCurrentNavigationColumn;
    procedure SetFocusCell(ARowIndex, ACellIndex: Integer; AShift: TShiftState); override;
  public
    function FocusNextCell(AForward, ANextRow: Boolean; AShift: TShiftState = []): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Refresh; override;

    property Controller: TcxTreeListController read GetController;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property FocusedNode: TcxTreeListNode read GetFocusedNode;
  end;

  { TcxTreeListPopup }

  TcxTreeListPopup = class(TcxCustomPopupWindow)
  strict private
    FOwner: TcxTreeListIndicatorCellViewInfo;

    function GetTreeList: TcxCustomTreeList;
  protected
    function CalculateOwnerBounds: TRect; virtual;
    procedure CreateCustomizationControl; virtual; abstract;
    function GetClientMinWidth: Integer; virtual;
    procedure InitPopup; override;
    function NeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle; AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean; override;
    procedure Paint; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    procedure Popup; reintroduce; virtual;
    property BorderWidths;

    property TreeList: TcxCustomTreeList read GetTreeList;
    property ClientMinWidth: Integer read GetClientMinWidth;
    property Owner: TcxTreeListIndicatorCellViewInfo read FOwner write FOwner;
  end;

  { TcxTreeListCustomizationPopup }

  TcxTreeListCustomizationPopup = class(TcxTreeListPopup)
  strict private
    FCustomizationControl: TdxQuickCustomizationCustomControl;
    FChangingItems: TList;
    FChangingItemsCheckStates: TdxIntegerIndexes;
    FSelectionInitializingInProcess: Boolean;
    procedure CheckListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
    procedure CheckListBoxInitializeSelection(Sender: TObject);
    procedure CheckListBoxSelectionChanged(Sender: TObject);
    procedure CheckListSelectedItemCheckedStateChanging(Sender: TObject);
    procedure CheckListSelectedItemCheckedStateChanged(Sender: TObject);
    procedure CustomizationControlInitialize(Sender: TObject);
    function GetCheckListBox: TdxQuickCustomizationCustomCheckListBox;
  protected
    procedure AddCheckListBoxItems; virtual; abstract;
    procedure AdjustCustomizationControlSize;
    function CanAddCommands: Boolean; virtual; abstract;
    procedure CheckListBoxCheckClicked(AIndex: Integer; AChecked: Boolean);
    procedure CreateCustomizationControl; override;
    procedure DoItemPosChanged(AItem: TObject); virtual; abstract;
    procedure GetCheckListBoxSelectedItems(AItems: TList; var AFocusedItem: TObject);
    function GetDropDownCount: Integer; virtual; abstract;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); virtual; abstract;
    procedure InitPopup; override;
    function IsCheckListBoxSorted: Boolean; virtual; abstract;
    function IsMultiColumnMode: Boolean; virtual; abstract;
    function IsVisibleTreeListItem(AItem: TObject): Boolean; virtual; abstract;
    procedure ItemClicked(AItem: TObject; AChecked: Boolean); virtual;
    procedure RefreshCheckListBoxItems;
    procedure SetCheckListBoxSelectedItems(AItems: TList; AFocusedItem: TObject);
    procedure SetQuickCustomizationSorted(AValue: Boolean); virtual; abstract;
    procedure SetQuickCustomizationSortOptions;
    procedure SetVisibleRowCount(ACount: Integer);
    procedure SynchronizeTreeListItemsVisibilityWithListBox; virtual; abstract;
    procedure UpdateCommandListBoxItemsChecking;

    property CheckListBox: TdxQuickCustomizationCustomCheckListBox read GetCheckListBox;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;
    procedure CloseUp; override;
    procedure CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize); override;

    property CustomizationControl: TdxQuickCustomizationCustomControl read FCustomizationControl;
  end;

 { TcxTreeListColumnsCustomizationPopup }

  TcxTreeListColumnsCustomizationPopup = class(TcxTreeListCustomizationPopup)
  protected
    procedure AddCheckListBoxItems; override;
    function CanAddCommands: Boolean; override;
    procedure DoItemPosChanged(AItem: TObject); override;
    function GetDropDownCount: Integer; override;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); override;
    function IsCheckListBoxSorted: Boolean; override;
    function IsMultiColumnMode: Boolean; override;
    function IsVisibleTreeListItem(AItem: TObject): Boolean; override;
    procedure SetQuickCustomizationSorted(AValue: Boolean); override;
    procedure SynchronizeTreeListItemsVisibilityWithListBox; override;
  end;

  { TcxTreeListBandsCustomizationPopup }

  TcxTreeListBandsCustomizationPopup = class(TcxTreeListCustomizationPopup)
  protected
    procedure AddCheckListBoxItems; override;
    function CanAddCommands: Boolean; override;
    procedure DoItemPosChanged(AItem: TObject); override;
    function GetDropDownCount: Integer; override;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); override;
    function IsCheckListBoxSorted: Boolean; override;
    function IsMultiColumnMode: Boolean; override;
    function IsVisibleTreeListItem(AItem: TObject): Boolean; override;
    procedure SetQuickCustomizationSorted(AValue: Boolean); override;
    procedure SynchronizeTreeListItemsVisibilityWithListBox; override;
  end;

  { TdxCustomTreeListQuickCustomizationControl }

  TdxCustomTreeListQuickCustomizationControl = class(TdxQuickCustomizationCustomControl)
  private
    procedure CheckAllItems(Sender: TObject);
    procedure SortItems(Sender: TObject);

    function GetPopup: TcxTreeListCustomizationPopup;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure DoCheckAllItems(AValue: Boolean);
    function GetCheckingAllState: TcxCheckBoxState;
    procedure PopulateCommandListBox; override;
    procedure PopulateCheckListBox; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property Popup: TcxTreeListCustomizationPopup read GetPopup;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxTreeListEditingController }

  TcxTreeListEditingController = class(TcxEditingController)
  private
    FIsEditAutoHeight: Boolean;
    FMultilineEdit: TcxAutoHeightInplaceEdit;
    function GetAutoHeight: TcxInplaceEditAutoHeight;
    function GetColumn: TcxTreeListColumn;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure AfterAssignMultilineEditProperties;
    procedure BeforeAssignMultilineEditProperties;
    function CanUseAutoHeightEditor: Boolean;
    function CanUpdateEditValue: Boolean; override;
    function CanUpdateMultilineEditHeight: Boolean;
    procedure CheckMultilineEditBounds(var ABounds: TRect);
    procedure CheckUsingMultilineEdit;
    function CreateMultilineEdit: TcxAutoHeightInplaceEdit; virtual;
    function GetAdjustedMultilineEditBounds(const ABounds: TRect): TRect;
    function GetFocusRectBounds: TRect; override;
    procedure InitEdit; override;
    procedure MultilineEditTextChanged; override;
    procedure StartEditAutoHeight(AHeightChanged: Boolean); override;

    property AutoHeight: TcxInplaceEditAutoHeight read GetAutoHeight;
    property Column: TcxTreeListColumn read GetColumn;
    property IsEditAutoHeight: Boolean read FIsEditAutoHeight write FIsEditAutoHeight;
    property MultilineEdit: TcxAutoHeightInplaceEdit read FMultilineEdit;
    property TreeList: TcxCustomTreeList read GetTreeList;
  public
    constructor Create(AController: TcxCustomControlController); override;
    destructor Destroy; override;
  end;

  { TcxTreeListNodeDragInfoImage }

  TcxNodeDragInfoState = (ndiNone, ndiInsertBefore, ndiInsertAfter, ndiAddChild);

  TcxTreeListNodeDragInfoImage = class(TcxLayeredDragImage)
  strict private
    FImageIndex: Integer;
    FImages: TcxImageList;
    FPrevPosition: TPoint;
    FPrevImageIndex: Integer;
    FTreeList: TcxCustomTreeList;

    function GetState: TcxNodeDragInfoState;
    function IsImageIndexValid: Boolean;
    procedure LoadResourceImages;
    procedure SetImageIndex(AValue: Integer);
    procedure SetState(const Value: TcxNodeDragInfoState);
    procedure UpdateImage;
  protected
    function MoveTo(APosition: TPoint): Boolean;
    //
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property TreeList: TcxCustomTreeList read FTreeList;
  public
    constructor Create(ATreeList: TcxCustomTreeList); reintroduce; virtual;
    destructor Destroy; override;
    property State: TcxNodeDragInfoState read GetState write SetState;
  end;

  { TcxTreeListController }

  TcxTreeListController = class(TcxCustomControlController)
  private
    FBandsCustomizationPopup: TcxTreeListBandsCustomizationPopup;
    FClickedObject: TObject;
    FColumnsCustomizationPopup: TcxTreeListColumnsCustomizationPopup;
    FDragCursorWasChanged: Boolean;
    FDragDropInProcess: Boolean;
    FDragNodeInfoImage: TcxTreeListNodeDragInfoImage;
    FFocusedNode: TcxTreeListNode;
    FFocusedNodeIndex: Integer;
    FForceSelectionNode: TcxTreeListNode;
    FForcingWidthBand: TcxTreeListBand;
    FForcingWidthColumn: TcxTreeListColumn;
    FHotTrackColumn: TcxTreeListColumn;
    FHotTrackNode: TcxTreeListNode;
    FHotTrackShift: TShiftState;
    FIncSearchNode: TcxTreeListNode;
    FIncSearchText: string;
    FIsDblClick: Boolean;
    FPostMakeVisible: Boolean;
    FPressedHeader: TcxTreeListCustomHeaderCellViewInfo;
    FPrevDragCursor: TCursor;
    FPrevFocusedNode: TcxTreeListNode;
    FResizeDirection: TcxDragSizingDirection;
    FSearchNotification: Boolean;
    FSelectionAnchor: TcxTreeListNode;
    FSelectionLockCount: Integer;
    FSkipButtonDown: Boolean;
    FSkipCheckEdit: Boolean;
    function GetBandsCustomizationPopup: TcxTreeListBandsCustomizationPopup;
    function GetColumnsCustomizationPopup: TcxTreeListColumnsCustomizationPopup;
    function GetDataController: TcxTreeListDataController;
    function GetDropMode(X, Y: Integer): TcxNodeDragInfoState;
    function GetEditingController: TcxTreeListEditingController;
    function GetFocusedNodeIndex: Integer;
    function GetHitTest: TcxTreeListHitTest;
    function GetIsDragCopy: Boolean;
    function GetLeftPos: Integer;
    function GetOptionsBehavior: TcxTreeListOptionsBehavior;
    function GetOptionsSelection: TcxTreeListOptionsSelection;
    function GetSelection: TList;
    function GetSelectionAnchor: TcxTreeListNode;
    function GetTreeList: TcxCustomTreeList;
    procedure SetForcingWidthColumn(AColumn: TcxTreeListColumn);
    procedure SetIncSearchNode(ANode: TcxTreeListNode);
    procedure SetLeftPos(AValue: Integer);
    procedure SetPressedHeader(AHeader: TcxTreeListCustomHeaderCellViewInfo);
  protected
    CanClearSelection: Boolean;
    DragNodesList: TList;
    PrevCursor: TCursor;
    // DragDrop fields
    ExpandingNode: TcxTreeListNode;
    ExpandTimer: TTimer;
    HScrollPos: Integer;
    VScrollPos: Integer;
    ScrollControllers: array[TcxBorder] of TcxAutoScrollingEditingControlObject;
    procedure AddNodeToSelection(ANode: TcxTreeListNode);
    procedure BeforeShowEdit; override;
    function CanHandleDeleteRecordKeys: Boolean; override;
    function CheckAutoScrolling(const APoint: TPoint): Boolean; virtual;
    function GetStatusHint(const APoint: TPoint): string;
    //
    procedure CheckFocusedNodeItem;
    procedure ChangeFocusedNode(ANode: TcxTreeListNode); virtual;

    function GetFocusedRecordIndex: TdxNativeInt; override;
    procedure SetFocusedNode(ANode: TcxTreeListNode); virtual;
    procedure SetFocusedNodeItem(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn); virtual;
    procedure SetFocusedRecordIndex(Value: TdxNativeInt); override;
    // Drag drop
    procedure BeforeStartDrag; override;
    function CanDrop: Boolean;
    function CreateAutoScrollObject(Kind: TScrollBarKind;
      const ARect: TRect; ACode: TScrollCode): TcxAutoScrollingEditingControlObject;
    procedure CheckButtonTimer; virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragEnter; virtual;
    procedure DragLeave; virtual;
    procedure DragMove(Source: TObject; const P: TPoint; var Accepted: Boolean); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DropTo(ANode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode); virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetRealScrollCode(ACode: TScrollCode): TScrollCode;
    procedure OnExpandTimer(Sender: TObject); virtual;
    procedure ResetButtonTimer;
    //
    procedure DoNextNode(AForward: Boolean);
    procedure DoNextPage(AForward: Boolean; Shift: TShiftState); override;
    procedure DoShowNextPageEx(AGoForward, ASetCursor: Boolean;
      AShift: TShiftState); virtual;
    procedure FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex: Integer); override;
    procedure FocusedItemChanged(APrevFocusedItem: TcxCustomInplaceEditContainer); override;
    function IsKeyForController(AKey: Word; AShift: TShiftState): Boolean; override;
    function IsNodeKeyHandle(ANode: TcxTreeListNode;
      var AKey: Word; Shift: TShiftState): Boolean; virtual;
    function GetCancelEditingOnExit: Boolean; override;
    function GetFindPanelClass: TcxControlFindPanelClass; override;
    function GetFindStartPosition(ARecordIndex: TdxNativeInt; AItemIndex: Integer; out AHighlightedText: string): Integer; override;
    function GetFocusedCellViewInfo(AEditContainer: TcxCustomInplaceEditContainer): TcxEditCellViewInfo; override;
    function GetImmediateEditor: Boolean; override;
    function GetNavigatorClass: TcxCustomCellNavigatorClass; override;
    function GetNode(ARecordIndex: Integer): TcxTreeListNode;
    function GetResizeDirection: TcxDragSizingDirection; override;
    procedure MouseLeave; override;
    procedure DoCancelMode; override;
    // incremental search
    procedure CancelIncSearching; override;
    function DoSearch(AFromNode: TcxTreeListNode; AItemIndex: Integer;
      const AText: string; AGoForward: Boolean): Boolean; virtual;
    function GetIncSearchingItem: TcxCustomInplaceEditContainer; override;
    function GetIncSearchingText: string; override;
    function GetIsIncSearching: Boolean; override;
    function GetNextNodeForIncSearch(ANode: TcxTreeListNode; AGoForward: Boolean): TcxTreeListNode;
    procedure CheckLocate(AFound: Boolean);
    procedure SearchLocate(AItem: TcxCustomInplaceEditContainer; const AValue: string); override;
    procedure SearchLocateNext(AItem: TcxCustomInplaceEditContainer; AForward: Boolean); override;

    // design selection
    function CanCreateSelectionHelper: Boolean;
    function IsObjectSelected(AObject: TPersistent): Boolean; virtual;
    procedure SelectObject(AObject: TPersistent; AShift: TShiftState); virtual;
    procedure UnselectObject(AObject: TPersistent); virtual;
    // selection
    procedure CancelSelection(KeepPrimary: Boolean = True);
    procedure SelectRange(const AStartNode, AFinishNode: TcxTreeListNode);
    procedure ViewInfoChanged; override;

    property DataController: TcxTreeListDataController read GetDataController;
    property DragCursorWasChanged: Boolean read FDragCursorWasChanged write FDragCursorWasChanged;
    property EditingController: TcxTreeListEditingController read GetEditingController;
    property PostMakeVisible: Boolean read FPostMakeVisible write FPostMakeVisible;
    property SearchNotification: Boolean read FSearchNotification write FSearchNotification;
    property SelectionLockCount: Integer read FSelectionLockCount write FSelectionLockCount;
    property SkipButtonDown: Boolean read FSkipButtonDown write FSkipButtonDown;
    property SkipCheckEdit: Boolean read FSkipCheckEdit write FSkipCheckEdit;
  public
    destructor Destroy; override;
    procedure AfterPaint; override;
    procedure BeforePaint; override;
    procedure BeginDragAndDrop; override;
    procedure BeginUpdateSelection;
    function CanDrag(X, Y: Integer): Boolean; override;
    function CanDeleteSelection: Boolean;
    function CanInsertNode: Boolean;
    function CanSelectNode(ANode: TcxTreeListNode): Boolean;
    function CheckCustomizationPopup: Boolean;
    procedure CheckDeletedNode(ANode: TcxTreeListNode); virtual;
    procedure CheckEdit; override;
    procedure CheckFocusedNode;
    procedure CheckHeaderClick(AShift: TShiftState);
    procedure CheckNodeContentClick(Shift: TShiftState);
    procedure DblClick; override;
    function DeleteConfirmation: Boolean;
    function DoHeaderMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function DoNodeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    procedure EndUpdateSelection;
    function FindNearestFocusableNode(AVisibleIndex: Integer): TcxTreeListNode;
    function FindNearestFocusableColumn(AVisibleIndex: Integer): TcxTreeListColumn;
    function HitAtNodeContent: Boolean;
    function IsImmediatePost: Boolean; override;
    function GetCursor(X, Y: Integer): TCursor; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MakeFocusedItemVisible; override;
    procedure MakeFocusedRecordVisible; override;

    procedure Reset; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure Select(ANode: TcxTreeListNode; AShift: TShiftState); virtual;
    procedure SetSelection(AList: TList);
    procedure SetFocusedRecordItem(ARecordIndex: TdxNativeInt; AItem: TcxCustomInplaceEditContainer); override;
    procedure SetHotTrackCursor(ACursor: TCursor);
    procedure SetHotTrackNode(ANode: TcxTreeListNode; AShift: TShiftState);
    procedure ShowEdit(AShift: TShiftState; X, Y: Integer);
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure FocusChanged; override;
    procedure UpdateHotTrackNode(AShift: TShiftState); overload;
    procedure UpdateHotTrackNode(X, Y: Integer; AShift: TShiftState); overload;

    property BandsCustomizationPopup: TcxTreeListBandsCustomizationPopup read GetBandsCustomizationPopup;
    property ClickedObject: TObject read FClickedObject write FClickedObject;
    property ColumnsCustomizationPopup: TcxTreeListColumnsCustomizationPopup read GetColumnsCustomizationPopup;
    property DragDropInProcess: Boolean read FDragDropInProcess write FDragDropInProcess;
    property FilterPopup;
    property FocusedNode: TcxTreeListNode read FFocusedNode write SetFocusedNode;
    property FocusedNodeIndex: Integer read GetFocusedNodeIndex write FFocusedNodeIndex;
    property ForceSelectionNode: TcxTreeListNode read FForceSelectionNode write FForceSelectionNode;
    property ForcingWidthBand: TcxTreeListBand read FForcingWidthBand write FForcingWidthBand;
    property ForcingWidthColumn: TcxTreeListColumn read FForcingWidthColumn write SetForcingWidthColumn;
    property HitTest: TcxTreeListHitTest read GetHitTest;
    property HotTrackColumn: TcxTreeListColumn read FHotTrackColumn write FHotTrackColumn;
    property HotTrackNode: TcxTreeListNode read FHotTrackNode write FHotTrackNode;
    property HotTrackShift: TShiftState read FHotTrackShift write FHotTrackShift;
    property IncSearchNode: TcxTreeListNode read FIncSearchNode write SetIncSearchNode;
    property IncSearchText: string read FIncSearchText write FIncSearchText;
    property IsDblClick: Boolean read FIsDblClick write FIsDblClick;
    property IsDragCopy: Boolean read GetIsDragCopy;
    property LeftPos: Integer read GetLeftPos write SetLeftPos;
    property OptionsBehavior: TcxTreeListOptionsBehavior read GetOptionsBehavior;
    property OptionsSelection: TcxTreeListOptionsSelection read GetOptionsSelection;
    property PressedHeader: TcxTreeListCustomHeaderCellViewInfo read FPressedHeader write SetPressedHeader;
    property PrevFocusedNode: TcxTreeListNode read FPrevFocusedNode;
    property Selection: TList read GetSelection;
    property SelectionAnchor: TcxTreeListNode read GetSelectionAnchor write FSelectionAnchor;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

 { TcxTreeListHitTest }

  TcxTreeListHitTest = class(TcxCustomHitTestController)
  private
    FDragItem: TObject;
    FHitBand: TcxTreeListBand;
    FHitColumn: TcxTreeListColumn;
    FHitNode: TcxTreeListNode;
    FLockCount: Integer;
    function GetHitAtImages: Boolean;
    function GetHitAtFooterArea: Boolean;
    function GetHitAtHeader: Boolean;
    function GetHitCell: TcxCustomViewInfoItem;
    function GetTreeList: TcxCustomTreeList;
    function GetViewInfo: TcxTreeListViewInfo;
  protected
    function AllowDesignMouseEvents(X, Y: Integer; AShift: TShiftState): Boolean; override;
    function CanShowHint(AItem: TObject): Boolean; override;
    function CanStartDrag: Boolean;
    procedure CheckFooterColumn;
    procedure CheckSelection(AShift: TShiftState); virtual;
    procedure DoCalculate; override;
    function GetCurrentCursor: TCursor; override;
    function GetHitAtFilterBox: Boolean; override;
    function GetHitAtFindPanel: Boolean; override;
    function GetHitAtNavigator: Boolean; override;
    function GetState(Index: Integer): Boolean;
    procedure SetHitState(Index: Integer; Value: Boolean);

    property DragItem: TObject read FDragItem write FDragItem;
    property HitAtFooterArea: Boolean read GetHitAtFooterArea;
    property HitAtHeader: Boolean read GetHitAtHeader;
    property LockCount: Integer read FLockCount write FLockCount;
    property ViewInfo: TcxTreeListViewInfo read GetViewInfo;
  public
    function CanMoving: Boolean; virtual;
    function CanSizing: Boolean; virtual;

    property Cursor: TCursor read GetCurrentCursor;
    property HitAtBackground: Boolean index tlhc_HitAtBackground read GetState;
    property HitAtBand: Boolean index tlhc_HitAtBand read GetState;
    property HitAtBandContainer: Boolean index tlhc_HitAtBandContainer read GetState;
    property HitAtBandCustomizing: Boolean index tlhc_HitAtBandCustomizing read GetState;
    property HitAtBandHeader: Boolean index tlhc_HitAtBandHeader read GetState;
    property HitAtButton: Boolean index tlhc_HitAtButton read GetState;
    property HitAtCheckButton: Boolean index tlhc_HitAtCheckButton read GetState;
    property HitAtColumn: Boolean index tlhc_HitAtColumn read GetState;
    property HitAtColumnCustomizing: Boolean index tlhc_HitAtColumnCustomizing read GetState;
    property HitAtColumnHeader: Boolean index tlhc_HitAtColumnHeader read GetState;
    property HitAtColumnHeaderFilterButton: Boolean index tlhc_HitAtColumnHeaderFilterButton read GetState;
    property HitAtFooter: Boolean index tlhc_HitAtFooter read GetState;
    property HitAtFooterItem: Boolean index tlhc_HitAtFooterItem read GetState;
    property HitAtGroupFooter: Boolean index tlhc_HitAtGroupFooter read GetState;
    property HitAtGroupFooterItem: Boolean index tlhc_HitAtGroupFooterItem read GetState;
    property HitAtImage: Boolean index tlhc_HitAtImage read GetState;
    property HitAtImages: Boolean read GetHitAtImages;
    property HitAtIndent: Boolean index tlhc_HitAtIndent read GetState;
    property HitAtIndicator: Boolean index tlhc_HitAtIndicator read GetState;
    property HitAtNode: Boolean index tlhc_HitAtNode read GetState;
    property HitAtNodePreview: Boolean index tlhc_HitAtNodePreview read GetState;
    property HitAtSeparator: Boolean index tlhc_HitAtSeparator read GetState;
    property HitAtSizingHorz: Boolean index tlhc_HitAtSizingHorz read GetState;
    property HitAtSizingVert: Boolean index tlhc_HitAtSizingVert read GetState;
    property HitAtStateImage: Boolean index tlhc_HitAtStateImage read GetState;
    property HitBand: TcxTreeListBand read FHitBand;
    property HitCell: TcxCustomViewInfoItem read GetHitCell;
    property HitColumn: TcxTreeListColumn read FHitColumn;
    property HitNode: TcxTreeListNode read FHitNode;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxTreeListHitTestArea }

  TcxTreeListHitTestArea = class
  private
    FArea: TRect;
    FIsRightToLeftConverted: Boolean;
    FLink: TcxTreeListCustomCellViewInfo;
  protected
    procedure Calculate; virtual; abstract;
    function GetHitTest(AHitTest: TcxTreeListHitTest): Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxTreeListHitTest); virtual; abstract;
    procedure RightToLeftConversion(const AClientBounds: TRect); virtual;
  public
    constructor Create(ALink: TcxTreeListCustomCellViewInfo); virtual;

    property Area: TRect read FArea write FArea;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property Link: TcxTreeListCustomCellViewInfo read FLink;
  end;

  { TcxTreeListHeaderSizingArea }

  TcxTreeListHeaderSizingArea = class(TcxTreeListHitTestArea)
  private
    FDirection: TcxDragSizingDirection;
  protected
    procedure Calculate; override;
    procedure InitHitTest(AHitTest: TcxTreeListHitTest); override;
  public
    constructor CreateEx(ALink: TcxTreeListCustomCellViewInfo; ADirection: TcxDragSizingDirection);

    property Direction: TcxDragSizingDirection read FDirection;
  end;

  { TcxTreeListNodeSizingArea }

  TcxTreeListNodeSizingArea = class(TcxTreeListHitTestArea)
  protected
    procedure Calculate; override;
    function GetHitTest(AHitTest: TcxTreeListHitTest): Boolean; override;
    procedure InitHitTest(AHitTest: TcxTreeListHitTest); override;
  public
    constructor Create(ALink: TcxTreeListCustomCellViewInfo); override;
    destructor Destroy; override;
  end;

  { TcxTreeListCustomCellViewInfo }

  TcxTreeListSeparators = array[0..1] of TRect;

  TcxTreeListCustomCellViewInfo = class(TcxCustomViewInfoItem)
  private
    function GetExtPaintStyle: Boolean;
    function GetHitTestController: TcxTreeListHitTest;
    function GetOptionsView: TcxTreeListOptionsView;
    function GetStyles: TcxTreeListStyles;
    function GetTreeList: TcxCustomTreeList;
    function GetVisibleRect: TRect;
  protected
    FAttachNode: TcxTreeListNode;
    FBorderColor: TColor;
    FBorders: TcxBorders;
    FNode: TcxTreeListNode;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function CellHasOrigin: Boolean; virtual;
    function GetCellOrigin: TPoint; virtual;
    function GetClipRect: TRect;
    function GetControl: TcxEditingControl; override;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    procedure SetBounds(const ABounds, AVisibleBounds: TRect);
    procedure SetHitTestCodes(const ACodes: array of Integer);
    function GetSelected: Boolean; virtual;

    property AttachNode: TcxTreeListNode read FAttachNode;
    property HitTest: TcxTreeListHitTest read GetHitTestController;
    property OptionsView: TcxTreeListOptionsView read GetOptionsView;
    property Selected: Boolean read GetSelected;
    property Styles: TcxTreeListStyles read GetStyles;
  public
    constructor CreateEx(ATreeList: TcxCustomTreeList;
      const ABounds, AVisibleRect: TRect); virtual;
    destructor Destroy; override;

    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Borders: TcxBorders read FBorders;
    property ExtPaintStyle: Boolean read GetExtPaintStyle;
    property Node: TcxTreeListNode read FNode;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleRect: TRect read GetVisibleRect;
  end;

  { TcxTreeListBackgroundCellViewInfo }

  TcxTreeListBackgroundCellViewInfo = class(TcxTreeListCustomCellViewInfo)
  protected
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    procedure Scroll(const DX, DY: Integer); override;
  public
    class function CustomDrawID: Integer; override;
  end;

  { TcxTreeListFooterCellViewInfo }

  TcxTreeListFooterCellViewInfo = class(TcxTreeListCustomCellViewInfo,
    IcxHotTrackElement, IcxHintableObject)
  private
    FColumn: TcxTreeListColumn;
    FHidden: Boolean;
    FText: string;
    function GetShowEndEllipsis: Boolean;
  protected
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    function GetRealAlignHorz: TAlignment; virtual;
    function GetAlignHorz: TAlignment; virtual;
    function GetAlignVert: TcxAlignmentVert; virtual;
    function GetDisplayFormat: string; virtual;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    function GetItem: TcxTreeListSummaryItem; virtual;
    function GetMultiline: Boolean; virtual;
    function GetValue: Variant; virtual;
    procedure Initialize(ANode, AttachNode: TcxTreeListNode; AColumn: TcxTreeListColumn); virtual;
    procedure Scroll(const DX, DY: Integer); override;
    // IcxHotTrackElement
    function GetHintBounds: TRect; virtual;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean; virtual;
    procedure UpdateHotTrackState(const APoint: TPoint);
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
  public
    constructor CreateEx(ATreeList: TcxCustomTreeList;
      const ABounds, AVisibleRect: TRect); override;
    class function CustomDrawID: Integer; override;

    property AlignHorz: TAlignment read GetRealAlignHorz;
    property AlignVert: TcxAlignmentVert read GetAlignVert;
    property AttachNode;
    property Column: TcxTreeListColumn read FColumn;
    property DisplayFormat: string read GetDisplayFormat;
    property Hidden: Boolean read FHidden;
    property MultiLine: Boolean read GetMultiLine;
    property ShowEndEllipsis: Boolean read GetShowEndEllipsis;
    property Text: string read FText;
    property Value: Variant read GetValue;
    property TreeList;
  end;

  { TcxTreeListFooterSingleCellViewInfo }

  TcxTreeListFooterSingleCellViewInfo = class(TcxTreeListFooterCellViewInfo)
  private
    FSummaryItem: TcxTreeListSummaryItem;
  protected
    function GetItem: TcxTreeListSummaryItem; override;
    function GetSelected: Boolean; override;
    procedure Initialize(ANode, AttachNode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      ASummaryItem: TcxTreeListSummaryItem); reintroduce; overload;
  public
    property SummaryItem: TcxTreeListSummaryItem read FSummaryItem;
  end;

  { TcxTreeListFooterMultiItemsCellViewInfo }

  TcxTreeListFooterMultiItemsCellViewInfo = class(TcxTreeListFooterCellViewInfo)
  private
    FNeedShowHint: Boolean;
    FSummaryItems: TcxTreeListSummaryItems;
    FHintRect: TRect;
    FVisibleCount: Integer;
    function GetSeparator: string;
  protected
    function CanDifferentStyles: Boolean;
    procedure DrawText(ACanvas: TcxCanvas); override;
    function GetAlignHorz: TAlignment; override;
    function GetAlignVert: TcxAlignmentVert; override;
    function GetItem: TcxTreeListSummaryItem; override;
    function GetSelected: Boolean; override;
    procedure Initialize(ADataNode, AttachNode: TcxTreeListNode; AColumn: TcxTreeListColumn;
      ASummaryItems: TcxTreeListSummaryItems); reintroduce; overload;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean; override;
  public
    property Separator: string read GetSeparator;
    property SummaryItems: TcxTreeListSummaryItems read FSummaryItems;
    property VisibleCount: Integer read FVisibleCount;
  end;

  { TcxTreeListCustomHeaderCellViewInfo }

  TcxTreeListCustomHeaderCellViewInfo = class(TcxTreeListCustomCellViewInfo,
    IcxHotTrackElement, IcxHintableObject)
  private
    FIsLast: Boolean;
    FNeighbors: TcxNeighbors;
    FState: TcxButtonState;
    FTextBounds: TRect;
    function GetHotTrack: Boolean;
    function GetPressed: Boolean;
    procedure SetHotTrack(AValue: Boolean);
    procedure SetState(AValue: TcxButtonState);
  protected
    BordersMargins: TRect;
    function CanNeighborFor(ACandidate: TcxTreeListHeaderCellViewInfo): Boolean; virtual;
    procedure CheckClipping(const ADisplayRect, AAvailableRect: TRect); override;
    function CheckNeighbors(ACandidate: TcxTreeListHeaderCellViewInfo; AShift: Integer): Boolean; virtual;
    procedure Click; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetAlignHorz: TAlignment; virtual;
    function GetRealAlignHorz: TAlignment; virtual;
    function GetAlignVert: TcxAlignmentVert; virtual;
    function GetIsVisible: Boolean; virtual;
    function GetMultiline: Boolean; virtual;
    function GetShowEndEllipsis: Boolean; virtual;
    function GetText: string; virtual;
    function NeedRecalculateOnStateChanged: Boolean; virtual;
    procedure SetPressed(AValue: Boolean); virtual;
    // IcxHotTrackElement
    function GetHintBounds: TRect; virtual;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean; virtual;
    procedure UpdateHotTrackState(const APoint: TPoint); virtual;
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
  public
    constructor CreateEx(ATreeList: TcxCustomTreeList;
      const ABounds, AVisibleRect: TRect); override;

    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property IsLast: Boolean read FIsLast;
    property MultiLine: Boolean read GetMultiLine;
    property Neighbors: TcxNeighbors read FNeighbors;
    property Pressed: Boolean read GetPressed write SetPressed;
    property State: TcxButtonState read FState write SetState;
    property TextBounds: TRect read FTextBounds;
  end;

  { TcxTreeListIndicatorCellViewInfo }

  TcxTreeListIndicatorPosition = (tlipBands, tlipColumns, tlipContent, tlipFooter);

  TcxTreeListIndicatorCellViewInfo = class(TcxTreeListCustomHeaderCellViewInfo)
  private
    FKind: TcxIndicatorKind;
    FPosition: TcxTreeListIndicatorPosition;
    FSizingArea: TcxTreeListNodeSizingArea;
    procedure SetKind(AValue: TcxIndicatorKind);
  protected
    function ActualIndicatorKind: TcxIndicatorKind;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    function GetHintBounds: TRect; override;
    function GetPopup: TcxTreeListCustomizationPopup;
    procedure Initialize(ANode: TcxTreeListNode; APosition: TcxTreeListIndicatorPosition); virtual;
    function IsQuickCustomizationEnabled: Boolean;
    procedure Scroll(const DX, DY: Integer); override;
    procedure SetPressed(AValue: Boolean); override;
    procedure ShowPopup;
    function GetOwnerBounds: TRect;
    procedure PopupClosed;

    property SizingArea: TcxTreeListNodeSizingArea read FSizingArea write FSizingArea;
  public
    destructor Destroy; override;
    class function CustomDrawID: Integer; override;

    property Kind: TcxIndicatorKind read FKind write SetKind;
    property Position: TcxTreeListIndicatorPosition read FPosition;
  end;

  { TcxTreeListHeaderCellViewInfo }

  TcxTreeListHeaderCellViewInfo = class(TcxTreeListCustomHeaderCellViewInfo, IcxDragSizing)
  private
    FCaption: TcxTreeListCaption;
    FFilterButtonState: TcxButtonState;
    FFilterButtonBounds: TRect;
    FGlyphPosition: TPoint;
    FItem: TPersistent;
    FSortMarkBounds: TRect;

    function GetDragSizing: IcxDragSizing;
    function GetGlyph: TdxSmartGlyph;
    function GetGlyphAlignHorz: TAlignment;
    function GetGlyphAlignVert: TcxAlignmentVert;
    function GetGlyphSize: TSize;
    procedure SetFilterButtonState(AValue: TcxButtonState);
  protected
    FHintCalculated: Boolean;
    GlyphClipping: Boolean;

    procedure DoCalculate; override;
    procedure DoCalculateFilterButton; virtual;
    procedure DoCalculateGlyphPosition;
    procedure DoCalculateSortMark; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoDrawFilterButton(ACanvas: TcxCanvas); virtual;
    procedure DoDrawSortMark(ACanvas: TcxCanvas); virtual;
    procedure Scroll(const DX, DY: Integer); override;
    procedure FilterButtonStateChanged; virtual;
    function GetAlignHorz: TAlignment; override;
    function GetAlignVert: TcxAlignmentVert; override;
    function GetFilterButtonWidth: Integer; virtual;
    function GetFilterSmartTagHeight: Integer; virtual;
    function GetFilterSmartTagState: TcxFilterSmartTagState; virtual;
    function GetFixed: Boolean; virtual;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    function GetMultiline: Boolean; override;
    function GetSelected: Boolean; override;
    function GetShowEndEllipsis: Boolean; override;
    function GetSortMarkWidth: Integer; virtual;
    function GetSortOrder: TcxDataSortOrder; virtual;
    function GetText: string; override;
    procedure Initialize(ACaption: TcxTreeListCaption);
    procedure InvalidateFilterButton; virtual;
    function IsFilterable: Boolean; virtual;
    function IsFilterButtonActive: Boolean; virtual;
    function IsFilterButtonAlwaysVisible: Boolean; virtual;
    function IsFilterButtonSmartTag: Boolean; virtual;
    function IsFilterButtonVisible: Boolean; virtual;
    function IsSortMarkVisible: Boolean; virtual;
    procedure LinkItem; virtual;
    function NeedRecalculateOnStateChanged: Boolean; override;
    procedure ReleaseReference(var AReference: TObject);
    procedure SetHitItem; virtual;
    procedure UpdateFilterButtonHotTrackState(const APoint: TPoint); virtual;
    procedure UpdateFilterButtonStateOnMouseDown; virtual;
    procedure UpdateHotTrackState(const APoint: TPoint); override;

    property DragSizing: IcxDragSizing read GetDragSizing implements IcxDragSizing;
    property FilterButtonState: TcxButtonState read FFilterButtonState write SetFilterButtonState;
    property Item: TPersistent read FItem;
  public
    constructor CreateEx(ATreeList: TcxCustomTreeList; const ABounds, AVisibleRect: TRect); override;

    property AlignHorz: TAlignment read GetRealAlignHorz;
    property AlignVert: TcxAlignmentVert read GetAlignVert;
    property FilterButtonBounds: TRect read FFilterButtonBounds;
    property Fixed: Boolean read GetFixed;
    property Glyph: TdxSmartGlyph read GetGlyph;
    property GlyphAlignHorz: TAlignment read GetGlyphAlignHorz;
    property GlyphAlignVert: TcxAlignmentVert read GetGlyphAlignVert;
    property GlyphPosition: TPoint read FGlyphPosition;
    property ShowEndEllipsis: Boolean read GetShowEndEllipsis;
    property SortMarkBounds: TRect read FSortMarkBounds;
    property SortOrder: TcxDataSortOrder read GetSortOrder;
    property Text: string read GetText;
    property TextBounds;
    property TreeList;
  end;

  { TcxTreeListBandHeaderCellViewInfo }

  TcxTreeListBandHeaderCellViewInfo = class(TcxTreeListHeaderCellViewInfo)
  private
    function GetBand: TcxTreeListBand;
  protected
    procedure Click; override;
    procedure DoCalculate; override;
    function GetFixed: Boolean; override;
    function GetIsVisible: Boolean; override;
    procedure LinkItem; override;
    procedure SetHitItem; override;
  public
    destructor Destroy; override;
    class function CustomDrawID: Integer; override;

    property Band: TcxTreeListBand read GetBand;
  end;

  { TcxTreeListColumnHeaderCellViewInfo }

  TcxTreeListColumnHeaderCellViewInfo = class(TcxTreeListHeaderCellViewInfo,
    IdxUIElementPopupWindowOwner,
    IdxFilterPopupWindowOwner)
  private
    function GetBand: TcxTreeListBand;
    function GetColumn: TcxTreeListColumn;
  protected
    { IdxUIElementPopupWindowOwner }
    function IdxUIElementPopupWindowOwner.ClosePopupWhenSetNil = CloseFilterPopupOnDestruction;
    procedure IdxUIElementPopupWindowOwner.InitPopup = InitFilterPopup;
    procedure IdxUIElementPopupWindowOwner.PopupClosed = FilterPopupClosed;
    function CloseFilterPopupOnDestruction: Boolean; virtual;
    procedure FilterPopupClosed; virtual;
    procedure InitFilterPopup(APopup: TdxUIElementPopupWindow); virtual;
    { IdxFilterPopupWindowOwner }
    function IdxFilterPopupWindowOwner.GetLinkComponent = GetFilterPopupLinkComponent;
    function IdxFilterPopupWindowOwner.GetMode = GetFilterPopupMode;
    function IdxFilterPopupWindowOwner.GetOptions = GetFilterPopupOptions;
    function GetFilterPopupLinkComponent: TComponent; virtual;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; virtual;
    function GetFilterPopupOptions: TObject; virtual;

    procedure CheckFilterPopupOwner; virtual;
    procedure Click; override;
    procedure DoCalculate; override;
    procedure FilterButtonStateChanged; override;
    function GetFixed: Boolean; override;
    function GetIsVisible: Boolean; override;
    function GetFilterPopup: TdxFilterPopupWindow; virtual;
    function GetFilterPopupAlignHorz: TcxPopupAlignHorz; virtual;
    function GetFilterPopupOwnerBounds: TRect; virtual;
    function GetSortOrder: TcxDataSortOrder; override;
    function IsFilterable: Boolean; override;
    function IsFilterButtonActive: Boolean; override;
    function IsFilterButtonAlwaysVisible: Boolean; override;
    function IsFilterButtonSmartTag: Boolean; override;
    function IsFilterPopupExist: Boolean; virtual;
    function IsFilterPopupOwner: Boolean; virtual;
    procedure LinkItem; override;
    procedure SetHitItem; override;
    procedure ShowFilterPopup; virtual;

    property FilterPopup: TdxFilterPopupWindow read GetFilterPopup;
  public
    destructor Destroy; override;

    class function CustomDrawID: Integer; override;

    property Band: TcxTreeListBand read GetBand;
    property Column: TcxTreeListColumn read GetColumn;
  end;

  { TcxTreeListBandCellViewInfo }

  TcxTreeListBandPart = (tlbpHeader, tlbpContent, tlbpGroupFooter, tlbpFooter, tlbpSeparator);

  TcxTreeListBandCellViewInfo = class(TcxTreeListCustomCellViewInfo)
  private
    FBand: TcxTreeListBand;
    FPart: TcxTreeListBandPart;
  protected
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    procedure Initialize(ABand: TcxTreeListBand; ANode, AAttachNode: TcxTreeListNode; APart: TcxTreeListBandPart);
    procedure Scroll(const DX, DY: Integer); override;
  public
    class function CustomDrawID: Integer; override;

    property Band: TcxTreeListBand read FBand;
    property Part: TcxTreeListBandPart read FPart;
    property AttachNode;
  end;

  { TcxTreeListEditCellViewInfo }

  TcxTreeListEditCellViewInfo = class(TcxEditCellViewInfo)
  private
    FBorderColor: TColor;
    FNodeViewData: TcxTreeListNodeViewData;
    function GetBand: TcxTreeListBand;
    function GetConditionalFormattingProvider: TcxTreeListConditionalFormattingProvider;
    function GetEditing: Boolean;
    function GetNode: TcxTreeListNode;
    function GetOptionsView: TcxTreeListOptionsView;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure AfterDrawCellBackground(ACanvas: TcxCanvas); override;
    procedure AfterDrawCellValue(ACanvas: TcxCanvas); override;
    procedure CalculateCellEditorBounds(AViewInfo: TcxCustomEditViewInfo; var R: TRect); override;
    procedure CanDrawCellValue(var Allow: Boolean); override;
    function FormatDisplayValue(AValue: Variant): Variant; override;

    procedure AfterCustomDraw(ACanvas: TcxCanvas); override;
    procedure Clear;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetButtonTransparency: TcxEditButtonTransparency; override;
    function GetCellOrg: TPoint; override;
    function GetColumn: TcxTreeListColumn; virtual;
    function GetDisplayValue: Variant; override;
    function GetEditRect: TRect; override;
    function GetEditViewParams: TcxViewParams; override;
    function GetFocused: Boolean; override;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    function GetHotTrack: Boolean; virtual;
    function GetIncSearchParams: TcxViewParams;
    // IcxHotTrackElement
    function GetHintBounds: TRect; override;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint;
      out AText: TCaption; out AIsMultiLine: Boolean; out ATextRect: TRect;
      var IsNeedOffsetHint: Boolean): Boolean; override;
    function GetRecordIndex: TdxNativeInt; override;
    function GetSelected: Boolean; virtual;
    function GetSelectedTextColor: Integer; override;
    function GetSelectedBKColor: Integer; override;
    function GetViewInfoData: Pointer; override;
    function IsAutoHeight: Boolean; override;
    function IsEditHotTrack(const APoint: TPoint): Boolean; virtual;
    function IsEndEllipsis: Boolean; override;
    function IsFixed: Boolean; virtual;
    function IsTransparent: Boolean; override;
    procedure Scroll(const DX, DY: Integer); override;

    property ConditionalFormattingProvider: TcxTreeListConditionalFormattingProvider read GetConditionalFormattingProvider;
  public
    class function CustomDrawID: Integer; override;
    procedure Invalidate(const R: TRect; AEraseBackground: Boolean = False); override;

    property Band: TcxTreeListBand read GetBand;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Column: TcxTreeListColumn read GetColumn;
    property Editing: Boolean read GetEditing;
    property EditRect: TRect read GetEditRect;
    property Focused: Boolean read GetFocused;
    property HotTrack: Boolean read GetHotTrack;
    property Node: TcxTreeListNode read GetNode;
    property NodeViewData: TcxTreeListNodeViewData read FNodeViewData;
    property OptionsView: TcxTreeListOptionsView read GetOptionsView;
    property Selected: Boolean read GetSelected;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxTreeListGroupNodeEditViewInfo }

  TcxTreeListGroupNodeEditViewInfo = class(TcxTreeListEditCellViewInfo)
  protected
    function GetColumn: TcxTreeListColumn; override;
    function GetFocused: Boolean; override;
    function IsFixed: Boolean; override;
    procedure Scroll(const DX, DY: Integer); override;
  end;

  { TcxTreeListPreviewCellViewInfo }

  TcxTreeListPreviewCellViewInfo = class(TcxTreeListEditCellViewInfo)
  private
    FPreview: TcxTreeListPreview;
  protected
    function ContentOffset: TRect; override;
    procedure DoCalculate; override;
    function GetButtonTransparency: TcxEditButtonTransparency; override;
    function GetColumn: TcxTreeListColumn; override;
    function GetEditViewParams: TcxViewParams; override;
    function GetHotTrack: Boolean; override;
    function GetMaxLineCount: Integer; override;
    function IsAutoHeight: Boolean; override;
    function IsEditHotTrack(const APoint: TPoint): Boolean; override;
    procedure Scroll(const DX, DY: Integer); override;
  public
    constructor Create(AOwner: TObject); override;
    class function CustomDrawID: Integer; override;

    property Preview: TcxTreeListPreview read FPreview;
  end;

  { TcxTreeListIndentCellViewInfo }

  TcxTreeListIndentLine = (ilVertUp, ilVertDown, ilHorz);
  TcxTreeListIndentLines = set of TcxTreeListIndentLine;

  TcxTreeListNodeIndentKind = (nikLevel, nikImage, nikState, nikCheck, nikFooter);

  TcxTreeListIndentCellViewInfo = class(TcxTreeListCustomCellViewInfo,
    IcxHotTrackElement, IcxHintableObject)
  private
    FButton: Boolean;
    FGlyphRect: TRect;
    FHorzTreeLine: TRect;
    FHotTrack: Boolean;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FIndex: Integer;
    FKind: TcxTreeListNodeIndentKind;
    FLevelNode: TcxTreeListNode;
    FLines: TcxTreeListIndentLines;
    FOverlayIndex: Integer;
    FVertTreeLine: TRect;
    function GetCheckState: TcxCheckBoxState;
    function GetFixed: Boolean;
    function GetHasImage: Boolean;
    function GetIsExpanded: Boolean;
    procedure SetGlyphRect(const AValue: TRect);
  protected
    procedure CalculateTreeLines(ACenter: TPoint);
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DrawCheck(ACanvas: TcxCanvas); virtual;
    procedure DrawImage(ACanvas: TcxCanvas); virtual;
    procedure DrawLines(ACanvas: TcxCanvas); virtual;
    procedure DrawTreeLine(ACanvas: TcxCanvas; const ARect: TRect);
    function GetCheckBoxViewInfo(ACanvas: TcxCanvas): TcxCustomEditViewInfo;
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
    procedure InitializeLevelIndent(ANode, AAttachNode: TcxTreeListNode; AIndex: Integer); virtual;
    procedure InitializeImageIndent(AImages: TCustomImageList); virtual;
    procedure Scroll(const DX, DY: Integer); override;
    procedure SetSize(ATop, AHeight: Integer);
    // IcxHotTrackElement
    function GetHintBounds: TRect; virtual;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean; virtual;
    procedure UpdateHotTrackState(const APoint: TPoint);
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
  public
    class function CustomDrawID: Integer; override;

    property Button: Boolean read FButton;
    property CheckState: TcxCheckBoxState read GetCheckState;
    property Fixed: Boolean read GetFixed;
    property GlyphRect: TRect read FGlyphRect write SetGlyphRect;
    property HasImage: Boolean read GetHasImage;
    property HorzTreeLine: TRect read FHorzTreeLine;
    property HotTrack: Boolean read FHotTrack;
    property ImageIndex: Integer read FImageIndex;
    property Images: TCustomImageList read FImages;
    property Index: Integer read FIndex;
    property IsExpanded: Boolean read GetIsExpanded;
    property Kind: TcxTreeListNodeIndentKind read FKind;
    property LevelNode: TcxTreeListNode read FLevelNode;
    property Lines: TcxTreeListIndentLines read FLines;
    property OverlayIndex: Integer read FOverlayIndex;
    property VertTreeLine: TRect read FVertTreeLine;
    property ViewParams;
    property TreeList;
  end;

  { TcxTreeListFindPanelViewInfo }

  TcxTreeListFindPanelViewInfo = class(TcxControlFindPanelViewInfo)
  protected
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
  end;

  { TcxTreeListFilterBoxViewInfo }

  TcxTreeListFilterBoxViewInfo = class(TcxControlFilterBoxViewInfo)
  protected
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
  end;

  { TcxTreeListNavigatorSiteViewInfo }

  TcxTreeListNavigatorSiteViewInfo = class(TcxCustomNavigatorSiteViewInfo)
  protected
    function GetHitTest(AHitTest: TcxCustomHitTestController): Boolean; override;
  end;

  { TcxTreeListNodeViewData }

  TcxTreeListNodeViewData = class
  private
    FFake: Boolean;
    FIsRightToLeftConverted: Boolean;
  protected
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
  public
    Bounds: TRect;
    Cells: TcxCustomControlCells;
    CheckFooterRgn: Boolean;
    ContentBounds: TRect;
    DrawFocusRect: Boolean;
    Focused: Boolean;
    FocusRectBounds: TRect;
    GroupFooterCount: Integer;
    Height: Integer;
    ImageIndent: TcxTreeListIndentCellViewInfo;
    IndentWidth: Integer;
    Node: TcxTreeListNode;
    Origin: TPoint;
    PreviewHeight: Integer;
    RowsHeight: Integer;
    RowsOffset: Integer;
    Selected: Boolean;
    StateImageIndent: TcxTreeListIndentCellViewInfo;
    ViewInfo: TcxTreeListViewInfo;

    constructor Create(AViewInfo: TcxTreeListViewInfo; ANode: TcxTreeListNode; ACapacity: Integer; AFake: Boolean = False); virtual;
    destructor Destroy; override;
    function GetRealBounds: TRect;
    function GetRealContentBounds: TRect;
    procedure InvalidateIndents;
    procedure Paint(ACanvas: TcxCanvas; AHandler: TcxCustomDrawCellEvent);
    procedure RightToLeftConversion(const AClientBounds: TRect);
    procedure Scroll(DX, DY: Integer; ALeftFixed, ARightFixed: Boolean); virtual;
    function Update(AForceUpdate: Boolean): Boolean;

    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
  end;

  { TcxTreeListViewInfo }

  TcxTreeListViewInfo = class(TcxExtEditingControlViewInfo)
  private
    FBandLineHeight: Integer;
    FCanOffsetContent: Boolean;
    FCells: TcxCustomControlCells;
    FContentParts: array[TcxTreeListBandFixedKind] of TRect;
    FContentHeight: Integer;
    FContentWidth: Integer;
    FCacheVScrollPos: Integer;
    FCacheVScrollSize: Integer;
    FCheckBoxProperties: TcxCustomEditProperties;
    FCheckBoxViewData: TcxCustomEditViewData;
    FCheckBoxViewInfo: TcxCustomEditViewInfo;
    FDefaultCellHeight: Integer;
    FDragItem: TObject;
    FFakeCell: TcxTreeListEditCellViewInfo;
    FFooterLineHeight: Integer;
    FGridLineColor: TColor;
    FGridLines: TcxBorders;
    FHeaderCells: TList;
    FHeadersHeight: Integer;
    FHitTestCells: TcxObjectList;
    FHorzBackgroundCell: TcxTreeListBackgroundCellViewInfo;
    FHScrollArea: TRect;
    FHScrollInc: Integer;
    FHScrollPage: Integer;
    FHScrollPos: Integer;
    FHScrollSize: Integer;
    FIsRightToLeftConverted: Boolean;
    FLockCount: Integer;
    FMaxPixelScrollTopNode: TcxTreeListNode;
    FMaxPixelScrollTopNodeOffset: Integer;
    FMultiRows: Boolean;
    FNodesHeight: Integer;
    FNodesViewData: TcxObjectList;
    FNodesVisibleCount: Integer;
    FPixelScrollNodeOffset: Integer;
    FPrevFont: TFont;
    FPrevFontHeight: Integer;
    FPreviewLineHeight: Integer;
    FRows: PIntArray;
    FTreeLineColor: TColor;
    FVertBackgroundCell: TcxTreeListBackgroundCellViewInfo;
    FVScrollPageValue: Integer;
    function GetBands: TcxTreeListBands;
    function GetBandsHeight: Integer;
    function GetBorderSize: Integer;
    function GetBottomNonContentHeight: Integer;
    function GetCalculateInProcess: Boolean;
    function GetCheckBoxSize: TSize;
    function GetColumn(AIndex: Integer): TcxTreeListColumn;
    function GetColumnCount: Integer;
    function GetColumnDefaultEditHeight(AColumn: TcxTreeListColumn): Integer;
    function GetColumnsRowCount: Integer;
    function GetContentPart(AKind: TcxTreeListBandFixedKind): TRect;
    function GetCount: Integer;
    function GetDefaultRowHeight: Integer;
    function GetFixedSeparatorWidth: Integer;
    function GetFooterHeight: Integer;
    function GetFooterLineCount: Integer;
    function GetGroupFooterHeight: Integer;
    function GetGroupFooterLineCount: Integer;
    function GetHasFixedSeparator: Boolean;
    function GetHeadersHeight: Integer;
    function GetImages: TCustomImageList;
    function GetIndentBand: TcxTreeListBand;
    function GetIndentLeftMost: Boolean;
    function GetIndicatorWidth: Integer;
    function GetLastNode: TcxTreeListNode;
    function GetLastPartVisibleNode: TcxTreeListNode;
    function GetLevelInfo(ALevel: Integer): TcxTreeListLevelInfo;
    function GetNodeViewData(AIndex: Integer): TcxTreeListNodeViewData;
    function GetOptionsView: TcxTreeListOptionsView;
    function GetOriginalViewInfo: TcxTreeListViewInfo;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetPixelScrollNodeOffset: Integer;
    function GetPreview: TcxTreeListPreview;
    function GetRoot: TcxTreeListNode;
    function GetRowOffset(AIndex: Integer): Integer;
    function GetSummary: TcxTreeListSummary;
    function GetStateImages: TCustomImageList;
    function GetStyles: TcxTreeListStyles;
    function GetTopNode: TcxTreeListNode;
    function GetTopNonContentHeight: Integer;
    function GetTreeList: TcxCustomTreeList;
    function GetVScrollPage: Integer;
    function GetVScrollPos: Integer;
    function GetVScrollSize: Integer;
    procedure SetCalculateInProcess(AValue: Boolean);
    procedure SetHScrollPos(APosition: Integer);
    procedure SetLastNode(ANode: TcxTreeListNode);
    procedure SetRowOffset(AIndex: Integer; const AValue: Integer);
    procedure SetVScrollPos(APosition: Integer);
  protected
    FBandHeaderLineHeight: Integer;
    FHeaderLineHeight: Integer;
    function AddBackgroundPart(const ABounds: TRect): TcxTreeListBackgroundCellViewInfo;
    function AddBandHeader(ABand: TcxTreeListBand; var ABounds: TRect;
      const AClipRect: TRect): TcxTreeListBandHeaderCellViewInfo;
    function AddBandPart(APart: TcxTreeListBandPart; ABand: TcxTreeListBand; ANode, AAttachNode: TcxTreeListNode;
      ABounds, AClipRect: TRect): TcxTreeListBandCellViewInfo;
    function AddBandSeparator(ANode: TcxTreeListNode;
      var ABounds: TRect; const AClipBounds: TRect): TcxTreeListBandCellViewInfo;
    procedure AddBandsHeaders(AKind: TcxTreeListBandFixedKind; const ABounds, AClipBounds: TRect);
    procedure AddCell(ACellClass: TcxTreeListCustomCellViewInfoClass;
      const ABounds, AVisibleRect: TRect; var AInstance);
    function AddColumnHeader(AColumn: TcxTreeListColumn; var ABounds: TRect;
      const AClipBounds: TRect): TcxTreeListColumnHeaderCellViewInfo;
    function AddEditCell(AViewData: TcxTreeListNodeViewData; AColumn: TcxTreeListColumn;
      ABounds, AClipRect: TRect; AClass: TcxEditCellViewInfoClass): TcxTreeListEditCellViewInfo;
    procedure AddColumnFooterItems(ADataNode, AttachNode: TcxTreeListNode;
      AColumn: TcxTreeListColumn; const ABounds, AClipRect: TRect);
    procedure AddFixedSeparators(ABounds: TRect; AAttachNode: TcxTreeListNode);
    procedure AddFooter(AAttachNode: TcxTreeListNode; AIndex, AIndent: Integer; const ABounds: TRect);
    procedure AddHeaderHitTestAreas(ACell: TcxTreeListHeaderCellViewInfo; AHorz, AVert: Boolean);
    function AddImageIndent(ANode: TcxTreeListNode; AImages: TCustomImageList;
      var ABounds: TRect; const AClipRect: TRect; ACheckDynamic: Boolean = False): TcxTreeListIndentCellViewInfo;
    function AddIndent(ANode, AAttachNode: TcxTreeListNode; ALevel: Integer; AKind: TcxTreeListNodeIndentKind;
      var ABounds: TRect; const AClipRect: TRect): TcxTreeListIndentCellViewInfo;
    function AddIndentCheck(ANode: TcxTreeListNode;
      var ABounds: TRect; const AClipRect: TRect): TcxTreeListIndentCellViewInfo;
    function AddIndicator(ANode: TcxTreeListNode; APosition: TcxTreeListIndicatorPosition;
      var ABounds: TRect): TcxTreeListIndicatorCellViewInfo;
    procedure AddNodeBands(AViewData: TcxTreeListNodeViewData);
    procedure AddNodeCategorized(AViewData: TcxTreeListNodeViewData);
    procedure AddNodeColumns(AViewData: TcxTreeListNodeViewData);
    procedure AddNodeFooters(AViewData: TcxTreeListNodeViewData);
    procedure AddNodeIndents(AViewData: TcxTreeListNodeViewData);
    function AddNodePreview(AViewData: TcxTreeListNodeViewData): TcxTreeListEditCellViewInfo;
    procedure AddNodeStandardContent(AViewData: TcxTreeListNodeViewData);
    function AddNodeViewData(ANode: TcxTreeListNode; AInsert: Boolean; AFake: Boolean = False): TcxTreeListNodeViewData;
    procedure AdjustNodeIndents(AViewData: TcxTreeListNodeViewData);

    procedure AfterCalculate; virtual;
    procedure BeforeCalculate; virtual;
    procedure CalculateBackgroundParts; virtual;
    procedure CalculateBandsLayout; virtual;
    procedure CalculateColumnsLayout; virtual;
    function CalculateDefaultEditHeight: Integer; override;
    procedure CalculateDefaultHeights; override;
    procedure CalculateFooterLayout; virtual;
    procedure CalculateHeaderRowsLayout;
    procedure CalculateHeadersNeighbors;
    procedure CalculateHitTest(AHitTest: TcxTreeListHitTest); virtual;
    procedure CalculateHorzScrollInfo(AAvailableWidth, AContentWidth: Integer); virtual;
    function CalculateNodeAutoHeight(AViewData: TcxTreeListNodeViewData): Integer;
    procedure CalculateNodesOrigin; virtual;
    procedure CalculateNodesViewData; virtual;
    procedure CalculateNodesViewDataBackward(ANode: TcxTreeListNode; ACheckHeight: Boolean; var AvailableHeight: Integer); virtual;
    procedure CalculateNodesViewDataForward(ANode: TcxTreeListNode; var AvailableHeight: Integer); virtual;
    procedure CalculatePixelScrollInfo(var ANode: TcxTreeListNode; var APixelScrollNodeOffset: Integer;
      AMaxTopNode: TcxTreeListNode; AMaxPixelScrollTopNodeOffset: Integer; ADelta: Integer; out AOverPan: Boolean);
    procedure CellsChanged;
    procedure CheckBiDiMode; virtual;
    function CheckScrollPosition(AScrollCode: TScrollCode; APos, APage, AInc, AMax : Integer;
      var AScrollPos: Integer): Boolean; virtual;
    procedure Clear;
    procedure DeleteNodeViewData(AIndex: Integer);
    procedure DoCalculate; override;
    procedure DoCalculateBackgroundParts(AHScrollPos: Integer);
    function GetContentBounds: TRect; virtual;
    function GetRealContentBounds: TRect;
    function GetFilterBoxViewInfoClass: TcxControlFilterBoxViewInfoClass; override;
    function GetFindPanelViewInfoClass: TcxControlFindPanelViewInfoClass; override;
    function GetFontHeight(const AFont: TFont): Integer;
    function GetHScrollBarAreaHeight: Integer;
    procedure GetHScrollBarBounds(var ABounds: TRect); override;
    function GetIsIndicatorVisible: Boolean; virtual;
    function GetIsPrinting: Boolean; virtual;
    function GetLevelContentOffset(ALevel: Integer): Integer;
    function GetMultilineEditorBounds(const ACellEditBounds: TRect;
      ACalculatedHeight: Integer; AAutoHeight: TcxInplaceEditAutoHeight): TRect;
    function GetNavigatorSiteViewInfoClass: TcxCustomNavigatorSiteViewInfoClass; override;
    function GetNodeContentOffset(ANode: TcxTreeListNode): Integer;
    function GetNodePixelScrollSize(ANode: TcxTreeListNode): Integer;
    function GetPixelScrollContentSize: Integer;
    procedure GetPixelScrollTopNodeAndOffsetByBottomNode(ABottomNode: TcxTreeListNode;
      out ATopNode: TcxTreeListNode; out ATopNodePixelScrollOffset: Integer);
    procedure GetVScrollBarBounds(var ABounds: TRect); override;
    function HasFooter(ANode: TcxTreeListNode): Boolean; virtual;
    function HasStateIndent(ANode: TcxTreeListNode): Boolean;
    procedure InflateBoundsForGridLines(var ABounds, AClipRect: TRect);
    procedure InitializeHeaderRows;
    procedure InitializeRows(const ANewHeight: Integer; var AViewData: TcxTreeListNodeViewData);
    procedure InitScrollBarsParameters; virtual;
    procedure InitScrollBarsParametersCache; virtual;
    procedure InvalidateRect(const ARect: TRect);
    function IsFindPanelVisible: Boolean; override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function IsPixelBasedScrollDataPos: Boolean;
    function IsRecordPixelScrollMode: Boolean;
    procedure MakeVisible(ANode: TcxTreeListNode);
    function MeasureColumnHeaderHeight(AColumn: TcxTreeListColumn): Integer;
    procedure RecalculateHitTestCells;
    procedure RecreateCheckBoxViewData;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;
    procedure ScrollHorz(AScrollCode: TScrollCode; var AScrollPos: Integer);
    procedure ScrollVert(AScrollCode: TScrollCode; var AScrollPos: Integer);
    procedure UpdateScrollBars; virtual;
    procedure UpdateSelection; override;
    procedure UpdatePixelScrollTopNodeAndOffsetMaxValues;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;
    procedure ValidateScrollPos(var APosition: Integer; const APage, AMax: Integer);

    property CalculateInProcess: Boolean read GetCalculateInProcess write SetCalculateInProcess;
    property CanOffsetContent: Boolean read FCanOffsetContent write FCanOffsetContent;
    property FakeCell: TcxTreeListEditCellViewInfo read FFakeCell;
    property HitTestCells: TcxObjectList read FHitTestCells;
    property IsPrinting: Boolean read GetIsPrinting;
    property LockCount: Integer read FLockCount write FLockCount;
    property RowOffset[Index: Integer]: Integer read GetRowOffset write SetRowOffset;
  public
    constructor Create(AOwner: TcxEditingControl); override;
    destructor Destroy; override;
    function GetEditCell(ANode: TcxTreeListNode;
      AColumn: TcxTreeListColumn): TcxTreeListEditCellViewInfo;
    procedure SetDirty;
    function Validate: Boolean;

    property BandHeaderLineHeight: Integer read FBandHeaderLineHeight;
    property BandLineHeight: Integer read FBandLineHeight;
    property Bands: TcxTreeListBands read GetBands;
    property BandsHeight: Integer read GetBandsHeight;
    property BorderSize: Integer read GetBorderSize;
    property BottomNonContentHeight: Integer read GetBottomNonContentHeight;
    property Cells: TcxCustomControlCells read FCells write FCells;
    property CheckboxSize: TSize read GetCheckBoxSize;
    property CheckBoxViewInfo: TcxCustomEditViewInfo read FCheckBoxViewInfo;
    property CheckBoxViewData: TcxCustomEditViewData read FCheckBoxViewData;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumn;
    property ColumnsRowCount: Integer read GetColumnsRowCount;
    property ContentBounds: TRect read GetRealContentBounds;
    property ContentHeight: Integer read FContentHeight write FContentHeight ;
    property ContentParts[AKind: TcxTreeListBandFixedKind]: TRect read GetContentPart;
    property ContentWidth: Integer read FContentWidth;
    property Count: Integer read GetCount;
    property DefaultCellHeight: Integer read FDefaultCellHeight;
    property DefaultRowHeight: Integer read GetDefaultRowHeight;
    property FixedSeparatorWidth: Integer read GetFixedSeparatorWidth;
    property FooterHeight: Integer read GetFooterHeight;
    property FooterLineCount: Integer read GetFooterLineCount;
    property FooterLineHeight: Integer read FFooterLineHeight;
    property GridLineColor: TColor read FGridLineColor;
    property GridLines: TcxBorders read FGridLines write FGridLines;
    property GroupFooterHeight: Integer read GetGroupFooterHeight;
    property GroupFooterLineCount: Integer read GetGroupFooterLineCount;
    property HasFixedSeparator: Boolean read GetHasFixedSeparator;
    property HeaderLineHeight: Integer read FHeaderLineHeight;
    property HeadersHeight: Integer read GetHeadersHeight;
    property HorzBackgroundCell: TcxTreeListBackgroundCellViewInfo read FHorzBackgroundCell;
    property HScrollArea: TRect read FHScrollArea;
    property HScrollInc: Integer read FHScrollInc;
    property HScrollPage: Integer read FHScrollPage;
    property HScrollPos: Integer read FHScrollPos write SetHScrollPos;
    property HScrollSize: Integer read FHScrollSize;
    property IndentBand: TcxTreeListBand read GetIndentBand;
    property IndentLeftMost: Boolean read GetIndentLeftMost;
    property IsIndicatorVisible: Boolean read GetIsIndicatorVisible;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property LevelInfo[ALevel: Integer]: TcxTreeListLevelInfo read GetLevelInfo;
    property VScrollPage: Integer read GetVScrollPage;
    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property VScrollSize: Integer read GetVScrollSize;
    property Images: TCustomImageList read GetImages;
    property IndicatorWidth: Integer read GetIndicatorWidth;
    property LastPartVisibleNode: TcxTreeListNode read GetLastPartVisibleNode;
    property LastNode: TcxTreeListNode read GetLastNode write SetLastNode;
    property MultiRows: Boolean read FMultiRows;
    property NodesHeight: Integer read FNodesHeight;
    property NodesViewData: TcxObjectList read FNodesViewData;
    property NodesVisibleCount: Integer read FNodesVisibleCount;
    property NodeViewData[Index: Integer]: TcxTreeListNodeViewData read GetNodeViewData;
    property OptionsView: TcxTreeListOptionsView read GetOptionsView;
    property OriginalViewInfo: TcxTreeListViewInfo read GetOriginalViewInfo;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property PixelScrollNodeOffset: Integer read GetPixelScrollNodeOffset;
    property Preview: TcxTreeListPreview read GetPreview;
    property PreviewLineHeight: Integer read FPreviewLineHeight;
    property Root: TcxTreeListNode read GetRoot;
    property Summary: TcxTreeListSummary read GetSummary;
    property StateImages: TCustomImageList read GetStateImages;
    property Styles: TcxTreeListStyles read GetStyles;
    property TopNode: TcxTreeListNode read GetTopNode;
    property TopNonContentHeight: Integer read GetTopNonContentHeight;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property TreeLineColor: TColor read FTreeLineColor;
  end;

  { TcxTreeListPainter }

  TcxTreeListPainter = class(TcxExtEditingControlPainter)
  private
    FCustomDrawSupported: Boolean;
    function GetCells: TcxCustomControlCells;
    function GetTreeList: TcxCustomTreeList;
    function GetViewInfo: TcxTreeListViewInfo;
  protected
    procedure DoPaint; override;
    procedure CheckCustomDrawSupport; virtual;
    procedure CustomDrawCell(ACanvas: TcxCanvas;
      ACell: TcxCustomViewInfoItem; var ADone: Boolean); virtual;
  public
    property Cells: TcxCustomControlCells read GetCells;
    property CustomDrawSupported: Boolean read FCustomDrawSupported write FCustomDrawSupported;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property ViewInfo: TcxTreeListViewInfo read GetViewInfo;
  end;

  { TcxTreeListBandOptions }

  TcxTreeListBandOptions = class(TcxOwnedPersistent)
  private
    FCustomizing: Boolean;
    FHidden: Boolean;
    FMoving: Boolean;
    FOnlyOwnColumns: Boolean;
    FSizing: Boolean;
    FVertSizing: Boolean;
    procedure SetSizing(AValue: Boolean);
    procedure SetVertSizing(AValue: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
  published
    property Customizing: Boolean read FCustomizing write FCustomizing default True;
    property Hidden: Boolean read FHidden write FHidden default False;
    property Moving: Boolean read FMoving write FMoving default True;
    property OnlyOwnColumns: Boolean read FOnlyOwnColumns write FOnlyOwnColumns default False;
    property Sizing: Boolean read FSizing write SetSizing default True;
    property VertSizing: Boolean read FVertSizing write SetVertSizing default True;
  end;

  TcxTreeListBandOptionsClass = class of TcxTreeListBandOptions;

  { TcxTreeListBandStyles }

  TcxTreeListBandStyles = class(TcxStyles)
  private
    function GetBand: TcxTreeListBand;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure Changed(AIndex: Integer); override;
    procedure DoGetContentParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AParams: TcxViewParams); virtual;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetContentParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;

    property Band: TcxTreeListBand read GetBand;
    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property Content: TcxStyle index tlbs_Content read GetValue write SetValue;
    property Footer: TcxStyle index tlbs_Footer read GetValue write SetValue;
    property Header: TcxStyle index tlbs_Header read GetValue write SetValue;
    property HeaderBackground: TcxStyle index tlbs_HeaderBackground read GetValue write SetValue;
  end;

  TcxTreeListBandStylesClass = class of TcxTreeListBandStyles;


  { TcxTreeListBandPosition }

  TcxTreeListBandPosition = class(TcxOwnedPersistent)
  private
    FBandIndex: Integer;
    FColIndex: Integer;
    FParentBand: TcxTreeListBand;
    function GetBand: TcxTreeListBand;
    function GetBandIndex: Integer;
    function GetColIndex: Integer;
    function GetTreeList: TcxCustomTreeList;
    function GetVisibleColIndex: Integer;
    procedure SetBandIndex(AValue: Integer);
    procedure SetColIndex(AValue: Integer);
    function IsColIndexStored: Boolean;
    function IsLocked: Boolean;
  protected
    procedure Changed; virtual;
    function CheckBandIndex(var AIndex: Integer): Boolean;
    function IsPositionChanged: Boolean;
    procedure Restore;
    procedure Store;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property Band: TcxTreeListBand read GetBand;
    property ParentBand: TcxTreeListBand read FParentBand;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleColIndex: Integer read GetVisibleColIndex;
  published
    property BandIndex: Integer read GetBandIndex write SetBandIndex default -1;
    property ColIndex: Integer read GetColIndex write SetColIndex stored IsColIndexStored;
  end;

  { TcxTreeListBand }

  TcxTreeListBand = class(TcxInterfacedCollectionItem, IcxDragSizing, IcxStoredObject)
  private
    FBandRows: TcxTreeListBandRows;
    FCalculatedWidth: Integer;
    FCaption: TcxTreeListCaption;
    FChildBands: TList;
    FChildVisibleBands: TList;
    FColumns: TList;
    FExpandable: TcxTreeListBandExpandable;
    FFixedKind: TcxTreeListBandFixedKind;
    FHasEmptyArea: Boolean;
    FHeaderCell: TcxTreeListBandHeaderCellViewInfo;
    FIndex: Integer;
    FIsDestroying: Boolean;
    FLineCount: Integer;
    FMinWidth: Integer;
    FOptions: TcxTreeListBandOptions;
    FPosition: TcxTreeListBandPosition;
    FStyles: TcxTreeListBandStyles;
    FTag: TdxNativeInt;
    FVisible: Boolean;
    FVisibleColumns: TList;
    FWidth: Integer;
    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;

    function GetActuallyExpandable: Boolean;
    function GetActuallyVisible: Boolean;
    function GetBands: TcxTreeListBands;
    function GetCalculatedWidth: Integer;
    function GetColumn(AIndex: Integer): TcxTreeListColumn;
    function GetColumnCount: Integer;
    function GetChildBand(AIndex: Integer): TcxTreeListBand;
    function GetChildBandCount: Integer;
    function GetDisplayWidth: Integer;
    function GetFirstChildBottomBand: TcxTreeListBand;
    function GetHasEmptyArea: Boolean;
    function GetIndentWidth: Integer;
    function GetIsBottom: Boolean;
    function GetIsFirstInGroup: Boolean;
    function GetIsLastAsChild: Boolean;
    function GetIsLastInGroup: Boolean;
    function GetIsLeftMost: Boolean;
    function GetIsLoading: Boolean;
    function GetIsRightMost: Boolean;
    function GetIsRoot: Boolean;
    function GetIsWidthFixed: Boolean;
    function GetLevel: Integer;
    function GetParentBand: TcxTreeListBand;
    function GetParentBandWidthAssigned: TcxTreeListBand;
    function GetRootIndex: Integer;
    function GetRootParentBand: TcxTreeListBand;
    function GetTreeList: TcxCustomTreeList;
    function GetVisibleIndex: Integer;
    function GetVisibleColumn(AIndex: Integer): TcxTreeListColumn;
    function GetVisibleColumnCount: Integer;
    function GetVisibleRootIndex: Integer;
    procedure SetCaption(AValue: TcxTreeListCaption);
    procedure SetCalculatedWidth(AValue: Integer);
    procedure SetColumn(AIndex: Integer; AValue: TcxTreeListColumn);
    procedure SetDisplayWidth(AValue: Integer);
    procedure SetExpandable(AValue: TcxTreeListBandExpandable);
    procedure SetFixedKind(AValue: TcxTreeListBandFixedKind);
    procedure SetMinWidth(AValue: Integer);
    procedure SetOptions(AValue: TcxTreeListBandOptions);
    procedure SetPosition(AValue: TcxTreeListBandPosition);
    procedure SetRootIndex(AValue: Integer);
    procedure SetStyles(AValue: TcxTreeListBandStyles);
    procedure SetVisible(AValue: Boolean);
    procedure SetVisibleColumn(AIndex: Integer; AValue: TcxTreeListColumn);
    procedure SetWidth(AValue: Integer);
  protected
    function ActualMinWidth: Integer;
    procedure AdjustSubItems;
    procedure AssignChildBandWidths;
    procedure AssignColumnsWidth;
    procedure AssignWidth;
    procedure InitAutoWidthItem(AItem: TcxAutoWidthItem);
    function IsBandFixedDuringSizing: Boolean;
    function IsOnlyOwnColumns: Boolean;
    function IsVisibleInQuickCustomizationPopup: Boolean;
    procedure CalculateLineCount(ACurrentLine: Integer);
    function CanDropColumnAt(const APoint: TPoint; out ARowIndex, AColIndex: Integer): Boolean; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckExpandable(var ABand: TcxTreeListBand);
    procedure ForceWidth(AValue: Integer);
    function GetMaxDeltaWidth: Integer;
    function CanMoving: Boolean; virtual;
    procedure ChangeCaption(Sender: TObject); virtual;
    function GetOptionsClass: TcxTreeListBandOptionsClass; virtual;
    function GetStyleClass: TcxTreeListBandStylesClass; virtual;
    procedure AddBand(ABand: TcxTreeListBand);
    procedure AddColumn(AColumn: TcxTreeListColumn);
    procedure ColumnSizeChanged(AColumn: TcxTreeListColumn);
    procedure DeleteColumn(AColumn: TcxTreeListColumn);
    procedure LayoutChanged;
    procedure MoveBandsToRoot;
    procedure MoveColumnsTo(ABand: TcxTreeListBand);
    procedure Refresh;
    procedure RemoveBand(ABand: TcxTreeListBand);
    procedure RemoveChildBands;
    procedure RemoveColumns;
    procedure SetIndex(Value: Integer); override;
      // IcxTreeListDragSizing implementation
    function CanSizing(ADirection: TcxDragSizingDirection): Boolean;
    function GetSizingBoundsRect(ADirection: TcxDragSizingDirection): TRect; virtual;
    function GetSizingIncrement(ADirection: TcxDragSizingDirection): Integer; virtual;
    function IsDynamicUpdate: Boolean; virtual;
    procedure SetSizeDelta(ADirection: TcxDragSizingDirection; ADelta: Integer); virtual;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    function GetPropertyIndex(const AName: string): Integer;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;

    property CalculatedWidth: Integer read GetCalculatedWidth write SetCalculatedWidth;
    property ChildVisibleBands: TList read FChildVisibleBands;
    property FirstChildBottomBand: TcxTreeListBand read GetFirstChildBottomBand;
    property HasEmptyArea: Boolean read GetHasEmptyArea write FHasEmptyArea;
    property HeaderCell: TcxTreeListBandHeaderCellViewInfo read FHeaderCell write FHeaderCell;
    property IsFirstInGroup: Boolean read GetIsFirstInGroup;
    property IsLastInGroup: Boolean read GetIsLastInGroup;
    property IsWidthFixed: Boolean read GetIsWidthFixed;
    property ParentBandWidthAssigned: TcxTreeListBand read GetParentBandWidthAssigned;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyBestFit;
    procedure BeforeDestruction; override;
    function IndexOf(AChildBand: TcxTreeListBand): Integer;
    function HasAsParent(ABand: TcxTreeListBand): Boolean;
    procedure MoveBand(ABand: TcxTreeListBand; AColIndex: Integer);

    procedure RestoreDefaults; virtual;
    procedure RestoreWidths;

    property ActuallyExpandable: Boolean read GetActuallyExpandable;
    property ActuallyVisible: Boolean read GetActuallyVisible;
    property BandRows: TcxTreeListBandRows read FBandRows;
    property Bands: TcxTreeListBands read GetBands;
    property ChildBandCount: Integer read GetChildBandCount;
    property ChildBands[Index: Integer]: TcxTreeListBand read GetChildBand;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumn write SetColumn;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
    property IndentWidth: Integer read GetIndentWidth;
    property IsBottom: Boolean read GetIsBottom;
    property IsLastAsChild: Boolean read GetIsLastAsChild;
    property IsLeftMost: Boolean read GetIsLeftMost;
    property IsLoading: Boolean read GetIsLoading;
    property IsRightMost: Boolean read GetIsRightMost;
    property IsRoot: Boolean read GetIsRoot;
    property Level: Integer read GetLevel;
    property LineCount: Integer read FLineCount;
    property ParentBand: TcxTreeListBand read GetParentBand;
    property RootIndex: Integer read GetRootIndex write SetRootIndex;
    property RootParentBand: TcxTreeListBand read GetRootParentBand;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleColumnCount: Integer read GetVisibleColumnCount;
    property VisibleColumns[Index: Integer]: TcxTreeListColumn read GetVisibleColumn write SetVisibleColumn;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleRootIndex: Integer read GetVisibleRootIndex;
    property Index;
  published
    property Caption: TcxTreeListCaption read FCaption write SetCaption;
    property Expandable: TcxTreeListBandExpandable read FExpandable write SetExpandable default tlbeDefault;
    property FixedKind: TcxTreeListBandFixedKind read FFixedKind write SetFixedKind default tlbfNone;
    property MinWidth: Integer read FMinWidth write SetMinWidth default cxTreeListDefMinWidth;
    property Options: TcxTreeListBandOptions read FOptions write SetOptions;
    property Position: TcxTreeListBandPosition read FPosition write SetPosition;
    property Styles: TcxTreeListBandStyles read FStyles write SetStyles;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 0;
    property Tag: TdxNativeInt read FTag write FTag default 0;

    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;
  end;

  TcxTreeListBandClass = class of TcxTreeListBand;

  { TcxTreeListBands }

  TcxTreeListBands = class(TCollection, IcxStoredObject, IcxStoredParent)
  private
    FBottomItems: TList;
    FColumnsLineCount: Integer;
    FExpandableBand: TcxTreeListBand;
    FLineCount: Integer;
    FLockUpdate: Integer;
    FRootItems: TList;
    FTreeList: TcxCustomTreeList;
    FVisibleItems: TList;
    FVisibleLeftFixedCount: Integer;
    FVisibleRightFixedCount: Integer;
    FVisibleRootItems: TList;
    FVisibleRootLeftFixedCount: Integer;
    FVisibleRootRightFixedCount: Integer;
    function GetBottomItem(AIndex: Integer): TcxTreeListBand;
    function GetBottomItemCount: Integer;
    function GetFirstVisibleBand: TcxTreeListBand;
    function GetItem(AIndex: Integer): TcxTreeListBand;
    function GetLastVisibleBand: TcxTreeListBand;
    function GetRootItemCount: Integer;
    function GetRootItem(AIndex: Integer): TcxTreeListBand;
    function GetVisibleItem(AIndex: Integer): TcxTreeListBand;
    function GetVisibleItemCount: Integer;
    function GetVisibleRootItem(AIndex: Integer): TcxTreeListBand;
    function GetVisibleRootItemCount: Integer;
    procedure SetItem(AIndex: Integer; AValue: TcxTreeListBand);
  protected
    procedure Adjust(ABands: TList = nil; AWidth: Integer = 0); virtual;
    procedure AssignColumnsWidth;
    procedure AssignRootItemWidths;
    procedure AssignRowColumnsWidth(ARow: TcxTreeListBandRow);
    function CanExpandableLeftMostOnly: Boolean; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure ClearCalculatedWidths;
    function GetFirstVisibleIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
    function GetFirstVisibleRootIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
    function GetLastVisibleIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
    function GetLastVisibleRootIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
    function GetVisibleCountByKind(AFixedKind: TcxTreeListBandFixedKind): Integer;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function PopulateVisibleChildren(ABand: TcxTreeListBand): Integer;
    procedure RefreshInformation;
    procedure Update(Item: TCollectionItem); override;
    //
    function IsUpdateLocked: Boolean;
    procedure LockUpdate;
    procedure UnlockUpdate;
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject }
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure DeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure GetChildren(AChildren: TStringList); virtual;
  public
    constructor Create(AOwner: TcxCustomTreeList); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function Add: TcxTreeListBand;
    procedure RestoreDefaults; virtual;
    procedure RestoreWidths; virtual;
    function VisibleIndexOf(ABand: TcxTreeListBand): Integer;

    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItems[Index: Integer]: TcxTreeListBand read GetBottomItem;
    property ColumnsLineCount: Integer read FColumnsLineCount;
    property ExpandableBand: TcxTreeListBand read FExpandableBand;
    property FirstVisible: TcxTreeListBand read GetFirstVisibleBand;
    property Items[AIndex: Integer]: TcxTreeListBand read GetItem write SetItem; default;
    property LastVisible: TcxTreeListBand read GetLastVisibleBand;
    property LineCount: Integer read FLineCount;
    property RootItemCount: Integer read GetRootItemCount;
    property RootItems[Index: Integer]: TcxTreeListBand read GetRootItem;
    property TreeList: TcxCustomTreeList read FTreeList;
    property VisibleItemCount: Integer read GetVisibleItemCount;
    property VisibleItems[Index: Integer]: TcxTreeListBand read GetVisibleItem;
    property VisibleLeftFixedCount: Integer read FVisibleLeftFixedCount;
    property VisibleRightFixedCount: Integer read FVisibleRightFixedCount;
    property VisibleRootItemCount: Integer read GetVisibleRootItemCount;
    property VisibleRootItems[Index: Integer]: TcxTreeListBand read GetVisibleRootItem;
    property VisibleRootLeftFixedCount: Integer read FVisibleRootLeftFixedCount;
    property VisibleRootRightFixedCount: Integer read FVisibleRootRightFixedCount;
  end;

  { TcxTreeListBandRow }

  TcxTreeListBandRow = class
  private
    FBandRows: TcxTreeListBandRows;
    FItems: TList;
    FLineCount: Integer;
    FLineOffset: Integer;
    FMinWidth: Integer;
    FVisibleItems: TList;
    FWidth: Integer;
    FWidthAssigned: Boolean;
    function GetBand: TcxTreeListBand;
    function GetCount: Integer;
    function GetFirst: TcxTreeListColumn;
    function GetIndex: Integer;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
    function GetItem(AIndex: Integer): TcxTreeListColumn;
    function GetVisibleIndex: Integer;
    function GetVisibleItem(AIndex: Integer): TcxTreeListColumn;
    function GetVisibleItemCount: Integer;
    function GetLast: TcxTreeListColumn;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure AdjustColumns;
    procedure AssignColumnsWidth;
    procedure CheckEmpty;
    procedure Refresh; virtual;

    property WidthAssigned: Boolean read FWidthAssigned;
  public
    constructor Create(AOwner: TcxTreeListBandRows); virtual;
    destructor Destroy; override;
    function IndexOf(AColumn: TcxTreeListColumn): Integer;

    property Band: TcxTreeListBand read GetBand;
    property BandRows: TcxTreeListBandRows read FBandRows;
    property Count: Integer read GetCount;
    property First: TcxTreeListColumn read GetFirst;
    property Index: Integer read GetIndex;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property Items[Index: Integer]: TcxTreeListColumn read GetItem; default;
    property Last: TcxTreeListColumn read GetLast;
    property LineCount: Integer read FLineCount;
    property LineOffset: Integer read FLineOffset;
    property MinWidth: Integer read FMinWidth;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleItemCount: Integer read GetVisibleItemCount;
    property VisibleItems[Index: Integer]: TcxTreeListColumn read GetVisibleItem;
    property Width: Integer read FWidth;
  end;

  { TcxTreeListBandRows }

  TcxTreeListBandRows = class
  private
    FBand: TcxTreeListBand;
    FItems: TcxObjectList;
    FLineCount: Integer;
    FRowMaxWidth: Integer;
    FRowMinWidth: Integer;
    FVisibleItems: TList;
    function GetCount: Integer;
    function GetFirst: TcxTreeListBandRow;
    function GetItem(Index: Integer): TcxTreeListBandRow;
    function GetItemEx(Index: Integer): TcxTreeListBandRow;
    function GetLast: TcxTreeListBandRow;
    function GetVisibleItemCount: Integer;
    function GetVisibleItem(AIndex: Integer): TcxTreeListBandRow;
  protected
    function Add: TcxTreeListBandRow;
    function CheckRowIndex(ARowIndex: Integer): TcxTreeListBandRow;
    procedure CheckRowEmpty(ARowIndex: Integer);
    function Insert(AIndex: Integer): TcxTreeListBandRow;
    procedure Refresh; virtual;
  public
    constructor Create(AOwner: TcxTreeListBand); virtual;
    destructor Destroy; override;

    property Band: TcxTreeListBand read FBand;
    property Count: Integer read GetCount;
    property First: TcxTreeListBandRow read GetFirst;
    property Items[Index: Integer]: TcxTreeListBandRow read GetItem; default;
    property Last: TcxTreeListBandRow read GetLast;
    property LineCount: Integer read FLineCount;
    property RowMaxWidth: Integer read FRowMaxWidth;
    property RowMinWidth: Integer read FRowMinWidth;
    property VisibleItemCount: Integer read GetVisibleItemCount;
    property VisibleItems[Index: Integer]: TcxTreeListBandRow read GetVisibleItem;
  end;

  { TcxTreeListFilterBox }

  TcxTreeListFilterBox = class(TcxControlOptionsFilterBox)
  protected
    function GetCustomizeButtonCaption: string; override;
    function GetDefaultText: string; override;
  end;

  { TcxTreeListDateTimeHandling }

  TcxTreeListDateTimeHandling = class(TcxControlOptionsDateTimeHandling)
  protected
    function GetDefaultMonthFormat: string; override;
    function GetDefaultYearFormat: string; override;
  published
    property DateFormat;
    property Filters;
    property HourFormat;
    property IgnoreTimeForFiltering;
    property MonthFormat;
    property UseLongDateFormat;
    property UseShortTimeFormat;
    property YearFormat;
  end;

  { TcxTreeListColumnFilterPopupOptions }

  TcxTreeListFilteringColumnPopupOptions = class(TcxControlOptionsFilteringItemPopup)
  protected
    function ApplyButtonCaption: string; override;
  published
    property AddValueItems;
    property ApplyMultiSelectChanges;
    property DropDownWidth;
    property FilteredItemsList;
    property MaxDropDownItemCount;
    property MRUItemsList;
    property MRUItemsListCount;
    property MultiSelect;
  end;

  { TcxTreeListFilteringColumnExcelPopupOptions }

  TcxTreeListFilteringColumnExcelPopupOptions = class(TcxControlOptionsFilteringItemExcelPopup)
  published
    property ApplyChanges;
    property DateTimeValuesPageType;
    property DefaultPage;
    property FilteredItemsList;
    property NumericValuesPageType;
  end;

  { TcxTreeListFiltering }

  TcxTreeListFiltering = class(TcxControlOptionsFiltering)
  private
    function GetColumnExcelPopup: TcxTreeListFilteringColumnExcelPopupOptions;
    function GetColumnPopup: TcxTreeListFilteringColumnPopupOptions;
    function GetColumnPopupMode: TdxFilterPopupWindowMode;
    procedure SetColumnExcelPopup(AValue: TcxTreeListFilteringColumnExcelPopupOptions);
    procedure SetColumnPopup(AValue: TcxTreeListFilteringColumnPopupOptions);
    procedure SetColumnPopupMode(AValue: TdxFilterPopupWindowMode);
  protected
    function GetItemExcelPopupClass: TcxControlOptionsFilteringItemExcelPopupClass; override;
    function GetItemPopupClass: TcxControlOptionsFilteringItemPopupClass; override;
  published
    property ColumnExcelPopup: TcxTreeListFilteringColumnExcelPopupOptions read GetColumnExcelPopup write SetColumnExcelPopup;
    property ColumnPopup: TcxTreeListFilteringColumnPopupOptions read GetColumnPopup write SetColumnPopup;
    property ColumnPopupMode: TdxFilterPopupWindowMode read GetColumnPopupMode write SetColumnPopupMode default fpmDefault;
  end;

  { TcxTreeListOptionsView }

  TcxTreeListGroupFootersMode = (tlgfInvisible, tlgfVisibleWhenExpanded, tlgfAlwaysVisible);
  TcxTreeListTreeLineStyle = (tllsNone, tllsDot, tllsSolid);
  TcxTreeListPaintStyle = (tlpsStandard, tlpsCategorized);

  TcxTreeListOptionsView = class(TcxControlOptionsView, IUnknown)
  private
    FBandLineHeight: Integer;
    FBands: Boolean;
    FButtons: Boolean;
    FCategorizedColumn: TcxTreeListColumn;
    FCheckGroups: Boolean;
    FColumnAutoWidth: Boolean;
    FDropArrowColor: TColor;
    FDropNodeIndicator: Boolean;
    FDynamicIndent: Boolean;
    FDynamicFocusedStateImages: Boolean;
    FEditAutoHeightBorderColor: TColor;
    FExtPaintStyle: Boolean;
    FFixedSeparatorColor: TColor;
    FFixedSeparatorWidth: Integer;
    FFocusRect: Boolean;
    FFooter: Boolean;
    FGridLineColor: TColor;
    FGridLines: TcxTreeListGridLines;
    FGroupFooters: TcxTreeListGroupFootersMode;
    FHeaderAutoHeight: Boolean;
    FHeaders: Boolean;
    FIndicator: Boolean;
    FIndicatorWidth: Integer;
    FPaintStyle: TcxTreeListPaintStyle;
    FShowRoot: Boolean;
    FSimpleCustomizeBox: Boolean;
    FTreeLineColor: TColor;
    FTreeLineStyle: TcxTreeListTreeLineStyle;
    FUseImageIndexForSelected: Boolean;
    FUseNodeColorForIndent: Boolean;

    function GetHeaderFilterButtonShowMode: TcxFilterButtonShowMode;
    function GetIsIndicatorVisible: Boolean;
    function GetShowColumnFilterButtons: TcxShowFilterButtons;
    function GetTreeList: TcxCustomTreeList;
    procedure SetBandLineHeight(AValue: Integer);
    procedure SetBands(AValue: Boolean);
    procedure SetButtons(AValue: Boolean);
    procedure SetCategorizedColumn(AValue: TcxTreeListColumn);
    procedure SetCheckGroups(AValue: Boolean);
    procedure SetColumnAutoWidth(AValue: Boolean);
    procedure SetDynamicIndent(AValue: Boolean);
    procedure SetDynamicFocusedStateImages(AValue: Boolean);
    procedure SetEditAutoHeightBorderColor(AValue: TColor);
    procedure SetExtPaintStyle(AValue: Boolean);
    procedure SetFixedSeparatorColor(AValue: TColor);
    procedure SetFixedSeparatorWidth(AValue: Integer);
    procedure SetFocusRect(AValue: Boolean);
    procedure SetFooter(AValue: Boolean);
    procedure SetGridLineColor(AValue: TColor);
    procedure SetGridLines(AValue: TcxTreeListGridLines);
    procedure SetGroupFooters(AValue: TcxTreeListGroupFootersMode);
    procedure SetHeaderAutoHeight(AValue: Boolean);
    procedure SetHeaderFilterButtonShowMode(AValue: TcxFilterButtonShowMode);
    procedure SetHeaders(AValue: Boolean);
    procedure SetIndicator(AValue: Boolean);
    procedure SetIndicatorWidth(AValue: Integer);
    procedure SetPaintStyle(AValue: TcxTreeListPaintStyle);
    procedure SetShowColumnFilterButtons(Value: TcxShowFilterButtons);
    procedure SetShowRoot(AValue: Boolean);
    procedure SetSimpleCustomizeBox(AValue: Boolean);
    procedure SetTreeLineColor(AValue: TColor);
    procedure SetTreeLineStyle(AValue: TcxTreeListTreeLineStyle);
    procedure SetUseImageIndexForSelected(AValue: Boolean);
    procedure SetUseNodeColorForIndent(AValue: Boolean);
  protected
    procedure Changed; override;
    procedure ChangeScale(M, D: Integer); override;
    function GetControl: TObject;
    function IsColumnHeaderFilterButtonShowedAlways: Boolean; virtual;
    function IsColumnHeaderFilterSmartTag: Boolean; virtual;
    // helper functions
    function HorzIncrement: Integer;
    function IsCategorizedPaint: Boolean;
    function IsExtPaintStyle: Boolean;
    function VertIncrement: Integer;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    property TreeList: TcxCustomTreeList read GetTreeList;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetCategorizedColumn: TcxTreeListColumn;
    procedure RestoreDefaults; virtual;

    property IsIndicatorVisible: Boolean read GetIsIndicatorVisible;
  published
    property BandLineHeight: Integer read FBandLineHeight write SetBandLineHeight default 0;
    property Bands: Boolean read FBands write SetBands default False;
    property Buttons: Boolean read  FButtons write SetButtons default True;
    property CategorizedColumn: TcxTreeListColumn read FCategorizedColumn write SetCategorizedColumn;
    property ColumnAutoWidth: Boolean read FColumnAutoWidth write SetColumnAutoWidth default False;
    property CheckGroups: Boolean read FCheckGroups write SetCheckGroups default False;
    property DropArrowColor: TColor read FDropArrowColor write FDropArrowColor default cxTreeListDefDropArrowColor;
    property DropNodeIndicator: Boolean read FDropNodeIndicator write FDropNodeIndicator default False;
    property DynamicIndent: Boolean read FDynamicIndent write SetDynamicIndent default False;
    property DynamicFocusedStateImages: Boolean read FDynamicFocusedStateImages write SetDynamicFocusedStateImages default False;
    property EditAutoHeightBorderColor: TColor read FEditAutoHeightBorderColor write SetEditAutoHeightBorderColor default clDefault;
    property ExtPaintStyle: Boolean read FExtPaintStyle write SetExtPaintStyle default False;
    property FixedSeparatorColor: TColor read FFixedSeparatorColor write SetFixedSeparatorColor default clDefault;
    property FixedSeparatorWidth: Integer read FFixedSeparatorWidth write SetFixedSeparatorWidth default cxTreeListDefSeparatorWidth;
    property FocusRect: Boolean read FFocusRect write SetFocusRect default True;
    property Footer: Boolean read FFooter write SetFooter default False;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property GridLines: TcxTreeListGridLines read FGridLines write SetGridLines default tlglNone;
    property GroupFooters: TcxTreeListGroupFootersMode read FGroupFooters write SetGroupFooters default tlgfInvisible;
    property HeaderAutoHeight: Boolean read FHeaderAutoHeight write SetHeaderAutoHeight default False;
    property HeaderFilterButtonShowMode: TcxFilterButtonShowMode read GetHeaderFilterButtonShowMode write SetHeaderFilterButtonShowMode default fbmDefault;
    property Headers: Boolean read FHeaders write SetHeaders default True;
    property Indicator: Boolean read FIndicator write SetIndicator default False;
    property IndicatorWidth: Integer read FIndicatorWidth write SetIndicatorWidth default cxTreeListDefIndicatorWidth;
    property NavigatorOffset;
    property PaintStyle: TcxTreeListPaintStyle read FPaintStyle write SetPaintStyle default tlpsStandard;
    property ShowColumnFilterButtons: TcxShowFilterButtons read GetShowColumnFilterButtons write SetShowColumnFilterButtons default sfbDefault;
    property ShowRoot: Boolean read FShowRoot write SetShowRoot default True;
    property SimpleCustomizeBox: Boolean read FSimpleCustomizeBox write SetSimpleCustomizeBox default False;
    property TreeLineColor: TColor read FTreeLineColor write SetTreeLineColor default clDefault;
    property TreeLineStyle: TcxTreeListTreeLineStyle read FTreeLineStyle write SetTreeLineStyle default tllsDot;
    property UseImageIndexForSelected: Boolean read FUseImageIndexForSelected write SetUseImageIndexForSelected default True;
    property UseNodeColorForIndent: Boolean read FUseNodeColorForIndent write SetUseNodeColorForIndent default True;
    property CellAutoHeight;
    property CellEndEllipsis;
    property CellTextMaxLineCount;
    property ScrollBars;
    property ShowEditButtons;
  end;

  { TcxTreeListOptionsCustomizing }

  TcxTreeListOptionsCustomizing = class(TcxOwnedPersistent)
  private
    FBandCustomizing: Boolean;
    FBandHiding: Boolean;
    FBandHorzSizing: Boolean;
    FBandMoving: Boolean;
    FBandsQuickCustomization: Boolean;
    FBandsQuickCustomizationMaxDropDownCount: Integer;
    FBandsQuickCustomizationMultiColumnMode: Boolean;
    FBandsQuickCustomizationShowCommands: Boolean;
    FBandsQuickCustomizationSorted: Boolean;
    FBandVertSizing: Boolean;
    FColumnCustomizing: Boolean;
    FColumnHiding: Boolean;
    FColumnHorzSizing: Boolean;
    FColumnMoving: Boolean;
    FColumnsQuickCustomization: Boolean;
    FColumnsQuickCustomizationMaxDropDownCount: Integer;
    FColumnsQuickCustomizationMultiColumnMode: Boolean;
    FColumnsQuickCustomizationShowCommands: Boolean;
    FColumnsQuickCustomizationSorted: Boolean;
    FColumnVertSizing: Boolean;
    FDynamicSizing: Boolean;
    FNestedBands: Boolean;
    FNodeSizing: Boolean;
    FRowSizing: Boolean;
    FStackedColumns: Boolean;

    function GetColumnFiltering: TdxDefaultBoolean;
    function GetTreeList: TcxCustomTreeList;
    procedure SetBandCustomizing(AValue: Boolean);
    procedure SetBandHiding(AValue: Boolean);
    procedure SetBandHorzSizing(AValue: Boolean);
    procedure SetBandMoving(AValue: Boolean);
    procedure SetBandsQuickCustomization(AValue: Boolean);
    procedure SetBandVertSizing(AValue: Boolean);
    procedure SetColumnCustomizing(AValue: Boolean);
    procedure SetColumnFiltering(AValue: TdxDefaultBoolean);
    procedure SetColumnHiding(AValue: Boolean);
    procedure SetColumnHorzSizing(AValue: Boolean);
    procedure SetColumnMoving(AValue: Boolean);
    procedure SetColumnsQuickCustomization(AValue: Boolean);
    procedure SetColumnVertSizing(AValue: Boolean);
    procedure SetDynamicSizing(AValue: Boolean);
    procedure SetNestedBands(AValue: Boolean);
    procedure SetNodeSizing(AValue: Boolean);
    procedure SetRowSizing(AValue: Boolean);
    procedure SetStackedColumns(AValue: Boolean);
  protected
    procedure Changed; virtual;

    property BandsQuickCustomizationMultiColumnMode: Boolean read FBandsQuickCustomizationMultiColumnMode write FBandsQuickCustomizationMultiColumnMode default True;
    property ColumnsQuickCustomizationMultiColumnMode: Boolean read FColumnsQuickCustomizationMultiColumnMode write FColumnsQuickCustomizationMultiColumnMode default True;
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;

    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property BandCustomizing: Boolean read FBandCustomizing write SetBandCustomizing default True;
    property BandHorzSizing: Boolean read FBandHorzSizing write SetBandHorzSizing default True;
    property BandHiding: Boolean read FBandHiding write SetBandHiding default False;
    property BandMoving: Boolean read FBandMoving write SetBandMoving default True;
    property BandsQuickCustomization: Boolean read FBandsQuickCustomization write SetBandsQuickCustomization default False;
    property BandsQuickCustomizationMaxDropDownCount: Integer read FBandsQuickCustomizationMaxDropDownCount write FBandsQuickCustomizationMaxDropDownCount default 0;
    property BandsQuickCustomizationShowCommands: Boolean read FBandsQuickCustomizationShowCommands write FBandsQuickCustomizationShowCommands default True;
    property BandsQuickCustomizationSorted: Boolean read FBandsQuickCustomizationSorted write FBandsQuickCustomizationSorted default False;
    property BandVertSizing: Boolean read FBandVertSizing write SetBandVertSizing default True;
    property ColumnCustomizing: Boolean read FColumnCustomizing write SetColumnCustomizing default True;
    property ColumnFiltering: TdxDefaultBoolean read GetColumnFiltering write SetColumnFiltering default bDefault;
    property ColumnHiding: Boolean read FColumnHiding write SetColumnHiding default False;
    property ColumnHorzSizing: Boolean read FColumnHorzSizing write SetColumnHorzSizing default True;
    property ColumnMoving: Boolean read FColumnMoving write SetColumnMoving default True;
    property ColumnsQuickCustomization: Boolean read FColumnsQuickCustomization write SetColumnsQuickCustomization default False;
    property ColumnsQuickCustomizationMaxDropDownCount: Integer read FColumnsQuickCustomizationMaxDropDownCount write FColumnsQuickCustomizationMaxDropDownCount default 0;
    property ColumnsQuickCustomizationShowCommands: Boolean read FColumnsQuickCustomizationShowCommands write FColumnsQuickCustomizationShowCommands default True;
    property ColumnsQuickCustomizationSorted: Boolean read FColumnsQuickCustomizationSorted write FColumnsQuickCustomizationSorted default False;
    property ColumnVertSizing: Boolean read FColumnVertSizing write SetColumnVertSizing default True;
    property DynamicSizing: Boolean read FDynamicSizing write SetDynamicSizing default False;
    property NestedBands: Boolean read FNestedBands write SetNestedBands default True;
    property NodeSizing: Boolean read FNodeSizing write SetNodeSizing default False;
    property RowSizing: Boolean read FRowSizing write SetRowSizing default False;
    property StackedColumns: Boolean read FStackedColumns write SetStackedColumns default True;
  end;

  { TcxTreeListOptionsSelection }

  TcxTreeListOptionsSelection = class(TcxOwnedPersistent)
  private
    FCellSelect: Boolean;
    FHideFocusRect: Boolean;
    FHideSelection: Boolean;
    FInvertSelect: Boolean;
    FMultiSelect: Boolean;
    function GetTreeList: TcxCustomTreeList;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property CellSelect: Boolean read FCellSelect write FCellSelect default True;
    property HideFocusRect: Boolean read FHideFocusRect write FHideFocusRect default True;
    property HideSelection: Boolean read FHideSelection write FHideSelection default False;
    property InvertSelect: Boolean read FInvertSelect write FInvertSelect default True;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
  end;

  TcxTreeListOptionsSelectionClass = class of TcxTreeListOptionsSelection;

  { TcxTreeListOptionsBehavior }

  TcxTreeListOptionsBehavior = class(TcxControlOptionsBehavior)
  private
    FAutoDragCopy: Boolean;
    FBestFitMaxRecordCount: Integer;
    FConfirmDelete: Boolean;
    FCopyCaptionsToClipboard: Boolean;
    FDragCollapse: Boolean;
    FDragExpand: Boolean;
    FDragFocusing: Boolean;
    FEditAutoHeight: TcxInplaceEditAutoHeight;
    FExpandOnDblClick: Boolean;
    FExpandOnIncSearch: Boolean;
    FFooterHints: Boolean;
    FHeaderHints: Boolean;
    FHotTrack: Boolean;
    FMultiSort: Boolean;
    FRecordScrollMode: TcxRecordScrollMode;
    FShowHourGlass: Boolean;
    FSorting: Boolean;
    FWaitExpandingTime: Integer;
    function GetChangeDelay: Integer;
    function GetIncSearchItem: TcxTreeListColumn;
    function GetTreeList: TcxCustomTreeList;
    procedure ReadBoolean(AReader: TReader);
    procedure SetChangeDelay(AValue: Integer);
    procedure SetEditAutoHeight(AValue: TcxInplaceEditAutoHeight);
    procedure SetFooterHints(AValue: Boolean);
    procedure SetHeaderHints(AValue: Boolean);
    procedure SetIncSearchItem(AValue: TcxTreeListColumn);
    procedure SetRecordScrollMode(AValue: TcxRecordScrollMode);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property AutoDragCopy: Boolean read FAutoDragCopy write FAutoDragCopy default False;
    property BestFitMaxRecordCount: Integer read FBestFitMaxRecordCount write FBestFitMaxRecordCount default -1;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property CopyCaptionsToClipboard: Boolean read FCopyCaptionsToClipboard write FCopyCaptionsToClipboard default True;
    property DragCollapse: Boolean read FDragCollapse write FDragCollapse default True;
    property DragDropText;
    property DragExpand: Boolean read FDragExpand write FDragExpand default True;
    property DragFocusing: Boolean read FDragFocusing write FDragFocusing default False;
    property EditAutoHeight: TcxInplaceEditAutoHeight read FEditAutoHeight write SetEditAutoHeight default eahNone;
    property ExpandOnDblClick: Boolean read FExpandOnDblClick write FExpandOnDblClick default True;
    property ExpandOnIncSearch: Boolean read FExpandOnIncSearch write FExpandOnIncSearch default False;
    property FocusCellOnCycle;
    property FocusFirstCellOnNewRecord;
    property FooterHints: Boolean read FFooterHints write SetFooterHints default False;
    property HeaderHints: Boolean read FHeaderHints write SetHeaderHints default False;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property IncSearch;
    property IncSearchItem: TcxTreeListColumn read GetIncSearchItem write SetIncSearchItem default nil;
    property MultiSort: Boolean read FMultiSort write FMultiSort default True;
    property NavigatorHints;
    property RecordScrollMode: TcxRecordScrollMode read FRecordScrollMode write SetRecordScrollMode default rsmDefault;
    property ShowHourGlass: Boolean read FShowHourGlass write FShowHourGlass default True;
    property Sorting: Boolean read FSorting write FSorting default True;
    property WaitForExpandNodeTime: Integer read FWaitExpandingTime write FWaitExpandingTime default 500;
  end;

  { TcxTreeListOptionsData }

  TcxTreeListOptionsData = class(TcxControlOptionsData)
  private
    FAppending: Boolean;
    FDeleting: Boolean;
    FInserting: Boolean;
    FImmediatePost: Boolean;
    FMultiThreadedSorting: TdxDefaultBoolean;
    FSummaryNullIgnore: Boolean;
    function GetAnsiSort: Boolean;
    function GetCaseInsensitive: Boolean;
    procedure SetAnsiSort(Value: Boolean);
    procedure SetAppending(Value: Boolean);
    procedure SetCaseInsensitive(Value: Boolean);
    procedure SetDeleting(const Value: Boolean);
    procedure SetImmediatePost(const Value: Boolean);
    procedure SetInserting(const Value: Boolean);
    procedure SetSummaryNullIgnore(Value: Boolean);
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Appending: Boolean read FAppending write SetAppending default False;
    property AnsiSort: Boolean read GetAnsiSort write SetAnsiSort default False;
    property CaseInsensitive: Boolean read GetCaseInsensitive write SetCaseInsensitive default False;
    property Deleting: Boolean read FDeleting write SetDeleting default True;
    property ImmediatePost: Boolean read FImmediatePost write SetImmediatePost default False;
    property Inserting: Boolean read FInserting write SetInserting default False;
    property MultiThreadedSorting: TdxDefaultBoolean read FMultiThreadedSorting write FMultiThreadedSorting default bDefault;
    property SummaryNullIgnore: Boolean read FSummaryNullIgnore write SetSummaryNullIgnore default False;
  end;

  { TcxTreeListPreview }

  TcxTreeListPreviewPlace = (tlppBottom, tlppTop);

  TcxTreeListPreview = class(TcxOwnedInterfacedPersistent)
  strict private
    FAutoHeight: Boolean;
    FColumn: TcxTreeListColumn;
    FLeftIndent: Integer;
    FMaxLineCount: Integer;
    FPlace: TcxTreeListPreviewPlace;
    FRightIndent: Integer;
    FVisible: Boolean;

    function GetActive: Boolean;
    function GetTreeList: TcxCustomTreeList;
    procedure SetAutoHeight(AValue: Boolean);
    procedure SetColumn(AValue: TcxTreeListColumn);
    procedure SetLeftIndent(AValue: Integer);
    procedure SetMaxLineCount(AValue: Integer);
    procedure SetPlace(AValue: TcxTreeListPreviewPlace);
    procedure SetRightIndent(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetControl: TObject;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property Active: Boolean read GetActive;
    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Column: TcxTreeListColumn read FColumn write SetColumn;
    property LeftIndent: Integer read FLeftIndent write SetLeftIndent default 5;
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount  default 3;
    property Place: TcxTreeListPreviewPlace read FPlace write SetPlace default tlppBottom;
    property RightIndent: Integer read FRightIndent write SetRightIndent default 5;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TcxTreeListStyleSheet }

  TcxTreeListStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxTreeListStyles;
    procedure SetStylesValue(Value: TcxTreeListStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxTreeListStyles read GetStylesValue write SetStylesValue;
  end;

  { TcxTreeListStyles }

  TcxTreeListGetBandContentStyleEvent = procedure(Sender: TcxCustomTreeList;
    ABand: TcxTreeListBand; ANode: TcxTreeListNode; var AStyle: TcxStyle) of object;

  TcxTreeListGetBandStyleEvent = procedure(Sender: TcxCustomTreeList;
    ABand: TcxTreeListBand; var AStyle: TcxStyle) of object;

  TcxTreeListGetColumnFooterStyleEvent = procedure(Sender: TcxCustomTreeList;
    AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; AFooterItem: TcxTreeListSummaryItem; var AStyle: TcxStyle) of object;

  TcxTreeListGetColumnStyleEvent = procedure(Sender: TcxCustomTreeList;
    AColumn: TcxTreeListColumn; var AStyle: TcxStyle) of object;

  TcxTreeListGetContentStyleEvent = procedure(Sender: TcxCustomTreeList;
    AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle) of object;

  TcxTreeListGetNodeIndentStyleEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; ALevel: Integer; var AStyle: TcxStyle) of object;

  TcxTreeListGetPreviewStyleEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; var AStyle: TcxStyle) of object;

  TcxTreeListStyles = class(TcxCustomControlStyles)
  private
    FCanOffsetContent: Boolean;
    FUseOddEvenStyles: TdxDefaultBoolean;
    FOnGetBandBackgroundStyle: TcxTreeListGetBandStyleEvent;
    FOnGetBandContentStyle: TcxTreeListGetBandContentStyleEvent;
    FOnGetBandHeaderStyle: TcxTreeListGetBandStyleEvent;
    FOnGetBandFooterStyle: TcxTreeListGetBandContentStyleEvent;
    FOnGetColumnFooterStyle: TcxTreeListGetColumnFooterStyleEvent;
    FOnGetColumnHeaderStyle: TcxTreeListGetColumnStyleEvent;
    FOnGetContentStyle: TcxTreeListGetContentStyleEvent;
    FOnGetHotTrackStyle: TcxTreeListGetContentStyleEvent;
    FOnGetNodeIndentStyle: TcxTreeListGetNodeIndentStyleEvent;
    FOnGetPreviewStyle: TcxTreeListGetPreviewStyleEvent;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetTreeList: TcxCustomTreeList;
    procedure SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
  protected
    function ActuallyUseOddEvenStyles: Boolean; inline;
    procedure DoGetBandBackgroundParams(ABand: TcxTreeListBand; var AParams: TcxViewParams); virtual;
    procedure DoGetBandContentParams(ABand: TcxTreeListBand; ANode: TcxTreeListNode; var AParams: TcxViewParams); virtual;
    procedure DoGetBandFooterParams(ABand: TcxTreeListBand; ANode: TcxTreeListNode; var AParams: TcxViewParams); virtual;
    procedure DoGetBandHeaderParams(ABand: TcxTreeListBand; var AParams: TcxViewParams); virtual;
    procedure DoGetColumnFooterParams(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode;
      ASummaryItem: TcxTreeListSummaryItem; var AParams: TcxViewParams); virtual;
    procedure DoGetColumnHeaderParams(AColumn: TcxTreeListColumn; var AParams: TcxViewParams); virtual;
    procedure DoGetContentParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AParams: TcxViewParams); virtual;
    procedure DoGetNodeContentParams(ANode: TcxTreeListNode; AStyle: TcxStyle; var AParams: TcxViewParams); virtual;
    procedure DoGetPreviewParams(ANode: TcxTreeListNode; var AParams: TcxViewParams); virtual;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetCellSelectionParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
    procedure PrepareConditionalFormattingParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AParams: TcxViewParams);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetBandBackgroundParams(ABand: TcxTreeListBand): TcxViewParams;
    function GetBandContentParams(ABand: TcxTreeListBand; ANode: TcxTreeListNode): TcxViewParams;
    function GetBandFooterParams(ABand: TcxTreeListBand; ANode: TcxTreeListNode): TcxViewParams;
    function GetBandHeaderParams(ABand: TcxTreeListBand): TcxViewParams;
    function GetColumnFooterParams(AColumn: TcxTreeListColumn;
      ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem = nil): TcxViewParams;
    function GetColumnHeaderParams(AColumn: TcxTreeListColumn): TcxViewParams;
    function GetContentParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
    function GetFooterParams: TcxViewParams;
    function GetIncSearchParams: TcxViewParams;
    function GetIndentParams(ANode: TcxTreeListNode; AIndent: Integer): TcxViewParams;
    function GetIndicatorParams: TcxViewParams;
    function GetPreviewParams(ANode: TcxTreeListNode): TcxViewParams;

    property CanOffsetContent: Boolean read FCanOffsetContent write FCanOffsetContent;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property BandBackground: TcxStyle index tlsv_BandBackground read GetValue write SetValue;
    property BandContent: TcxStyle index tlsv_BandContent read GetValue write SetValue;
    property BandHeader: TcxStyle index tlsv_BandHeader read GetValue write SetValue;
    property ColumnFooter: TcxStyle index tlsv_ColumnFooter read GetValue write SetValue;
    property ColumnHeader: TcxStyle index tlsv_ColumnHeader read GetValue write SetValue;
    property ContentEven: TcxStyle index tlsv_ContentEven read GetValue write SetValue;
    property ContentOdd: TcxStyle index tlsv_ContentOdd read GetValue write SetValue;
    property Footer: TcxStyle index tlsv_Footer read GetValue write SetValue;
    property HotTrack: TcxStyle index tlsv_HotTrack read GetValue write SetValue;
    property IncSearch: TcxStyle index tlsv_IncSearch read GetValue write SetValue;
    property Indicator: TcxStyle index tlsv_Indicator read GetValue write SetValue;
    property Navigator;
    property NavigatorInfoPanel;
    property Preview: TcxStyle index tlsv_Preview read GetValue write SetValue;
    property UseOddEvenStyles: TdxDefaultBoolean read FUseOddEvenStyles write SetUseOddEvenStyles default bDefault;
    property OnGetBandBackgroundStyle: TcxTreeListGetBandStyleEvent read FOnGetBandBackgroundStyle write FOnGetBandBackgroundStyle;
    property OnGetBandContentStyle: TcxTreeListGetBandContentStyleEvent read FOnGetBandContentStyle write FOnGetBandContentStyle;
    property OnGetBandFooterStyle: TcxTreeListGetBandContentStyleEvent read FOnGetBandFooterStyle write FOnGetBandFooterStyle;
    property OnGetBandHeaderStyle: TcxTreeListGetBandStyleEvent read FOnGetBandHeaderStyle write FOnGetBandHeaderStyle;
    property OnGetColumnFooterStyle: TcxTreeListGetColumnFooterStyleEvent read FOnGetColumnFooterStyle write FOnGetColumnFooterStyle;
    property OnGetColumnHeaderStyle: TcxTreeListGetColumnStyleEvent read FOnGetColumnHeaderStyle write FOnGetColumnHeaderStyle;
    property OnGetContentStyle: TcxTreeListGetContentStyleEvent read FOnGetContentStyle write FOnGetContentStyle;
    property OnGetHotTrackStyle: TcxTreeListGetContentStyleEvent read FOnGetHotTrackStyle write FOnGetHotTrackStyle;
    property OnGetNodeIndentStyle: TcxTreeListGetNodeIndentStyleEvent read FOnGetNodeIndentStyle write FOnGetNodeIndentStyle;
    property OnGetPreviewStyle: TcxTreeListGetPreviewStyleEvent read FOnGetPreviewStyle write FOnGetPreviewStyle;
    property Background;
    property Content;
    property Inactive;
    property Selection;
    property StyleSheet;
  end;

  { TcxTreeListItemsCustomizeListBox }

  TcxTreeListItemsCustomizeListBox = class(TcxListBox)
  private
    FHeaders: TcxObjectList;
    FTreeList: TcxCustomTreeList;
    function GetHeader(AIndex: Integer): TcxTreeListHeaderCellViewInfo;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure AddItem(ACaption: TcxTreeListCaption);
    function CreateCell: TcxTreeListHeaderCellViewInfo; virtual; abstract;
    procedure DblClick; override;
    procedure DoDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas;
      AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); virtual;
    procedure DoDrawItemEx(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    function GetItemHeight(AItem: Integer): Integer; virtual;
    function GetOrigin(AHeader: TcxTreeListHeaderCellViewInfo): TPoint;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MakeItemVisible(AHeader: TcxTreeListHeaderCellViewInfo); virtual; abstract;
    procedure MeasureItem(AControl: TcxListBox; Index: Integer; var Height: Integer); virtual;
    procedure Paint; override;
    procedure PopulateItems; virtual; abstract;
    procedure RefreshList;
    procedure Resize; override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure UpdateBackgroundColor;
    procedure UpdateHeaderInfo(AIndex: Integer);
    procedure UpdateHeaders;

    property Headers[Index: Integer]: TcxTreeListHeaderCellViewInfo read GetHeader;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  public
    constructor CreateEx(AOwner: TComponent; ATreeList: TcxCustomTreeList; AParent: TWinControl);
    destructor Destroy; override;

    property TreeList: TcxCustomTreeList read FTreeList;
  end;

  TcxTreeListColumnsCustomizeListBox = class(TcxTreeListItemsCustomizeListBox)
  protected
    function CreateCell: TcxTreeListHeaderCellViewInfo; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    procedure MakeItemVisible(AHeader: TcxTreeListHeaderCellViewInfo); override;
    procedure PopulateItems; override;
  end;

  TcxTreeListBandsCustomizeListBox = class(TcxTreeListItemsCustomizeListBox)
  protected
    function CreateCell: TcxTreeListHeaderCellViewInfo; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    procedure MakeItemVisible(AHeader: TcxTreeListHeaderCellViewInfo); override;
    procedure PopulateItems; override;
  end;

  { TcxTreeListCustomizationForm }

  TcxTreeListCustomizationForm = class(TdxForm)
  private
    FDragDropListBox: TcxTreeListItemsCustomizeListBox;
    FHookTimer: TcxTimer;
    FOwner: TcxTreeListCustomizing;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    function GetTreeList: TcxCustomTreeList;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FinishDragDrop(Sender: TcxTreeListItemsCustomizeListBox);
    procedure StartDragDrop(Sender: TcxTreeListItemsCustomizeListBox);
    procedure TimerHandler(Sender: TObject);
  public
    constructor CreateEx(AOwner: TcxTreeListCustomizing);
    destructor Destroy; override;

    property HookTimer: TcxTimer read FHookTimer;
    property Customizing: TcxTreeListCustomizing read FOwner;
    property DragDropListBox: TcxTreeListItemsCustomizeListBox read FDragDropListBox;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxTreeListCustomizing }

  TcxTreeListCustomizingClass = class of TcxTreeListCustomizing;
  TcxTreeListCustomizing = class(TcxOwnedInterfacedPersistent, IcxTreeListDesigner)
  private
    FBandListBox: TcxTreeListItemsCustomizeListBox;
    FForm: TcxTreeListCustomizationForm;
    FHeaderListBox: TcxTreeListItemsCustomizeListBox;
    FIsPositionAssigned: Boolean;
    FPageControl: TcxPageControl;
    FPosition: TPoint;
    FRowCount: Integer;
    FSize: TSize;
    FTabSheetBands: TcxTabSheet;
    FTabSheetHeaders: TcxTabSheet;

    function GetForm: TForm;
    function GetIsSimpleMode: Boolean;
    function GetPosition: TPoint;
    function GetTreeList: TcxCustomTreeList;
    function GetVisible: Boolean;
    procedure SetPosition(const AValue: TPoint);
    procedure SetRowCount(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure BiDiModeChanged;
    procedure CalculateSize(var AWidth, AHeight: Integer);
    procedure Close(Sender: TObject; var Action: TCloseAction); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CreateControls; virtual;
    procedure CreateCustomizingForm; virtual;
    procedure FinishCustomizing(const APosition: TPoint; const ASize: TSize); virtual;
    procedure HideCustomizingForm;
    procedure RefreshInformation;
    procedure SetControlParent(AControl, AParent: TWinControl);
    procedure ShowCustomizingForm;
    procedure Update; virtual;
    // IcxTreeListDesigner
    procedure ComponentModified; virtual;
    procedure ComponentRemoved(Sender: TObject); virtual;

    property BandListBox: TcxTreeListItemsCustomizeListBox read FBandListBox;
    property HeaderListBox: TcxTreeListItemsCustomizeListBox read FHeaderListBox;
    property SimpleMode: Boolean read GetIsSimpleMode;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MakeBandPageVisible;
    procedure MakeColumnPageVisible;
    function PtInCustomizingBox(const APoint: TPoint): Boolean;

    property Form: TForm read GetForm;
    property IsPositionAssigned: Boolean read FIsPositionAssigned write FIsPositionAssigned;
    property Position: TPoint read GetPosition write SetPosition;
    property RowCount: Integer read FRowCount write SetRowCount default 10;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  { TcxTreeListDataController }

  TcxTreeListForEachNodeProc = procedure(ANode: TcxTreeListNode; Data: Pointer) of object;

  TcxTreeListDataController = class(TcxControlDataController)
  private
    FAllocatedRecords: Integer;
    FBookmarkNode: TcxTreeListNode;
    FDataChangedBusy: Boolean;
    FEditingNode: TcxTreeListNode;
    FFiltered: Boolean;
    FFilterProcessCount: Integer;
    FIsValueChanged: Boolean;
    FNodesCount: Integer;

    procedure DoFilterNodeProc(ANode: TcxTreeListNode; Data: Pointer);
    function GetHasEditData: Boolean;
    function GetRoot: TcxTreeListNode;
    function GetTreeList: TcxCustomTreeList;
    function GetValueDef(AIndex: Integer): TcxValueDef;
    procedure FreeRecordProc(ANode: TcxTreeListNode; AData: Pointer);
    procedure InsertValueDef(AColumn: TcxTreeListColumn);
    procedure InsertValueDefProc(ANode: TcxTreeListNode; AData: Pointer);
    procedure SetEditingNode(AValue: TcxTreeListNode);
    procedure SetIsValueChanged(AValue: Boolean);
    procedure RemoveValueDef(AColumn: TcxTreeListColumn);
    procedure RemoveValueDefProc(ANode: TcxTreeListNode; AData: Pointer);
    procedure ResetNodeFilteringProc(ANode: TcxTreeListNode; AData: Pointer);
  protected
    procedure CancelEditing;
    function Compare(AValueDef: TcxValueDef; AHandle1, AHandle2: Pointer): Integer;
    function CopyData(ASource: TcxTreeListNode): Pointer;
    procedure DeleteNode(ANode: TcxTreeListNode); virtual;
    procedure DestroyHandles;
    procedure DoFilterNodes; virtual;
    function DoFilterRecordEvent(ARecordIndex: Integer): Boolean; override;
    procedure FilterChanged; override;
    procedure FilterNodes; virtual;
    procedure FindFilterChanged; override;
    procedure ForEachNode(AProc: TcxTreeListForEachNodeProc; AData: Pointer);
    procedure ForEachRecord(AProc: TcxTreeListForEachNodeProc; AData: Pointer);
    function GetEditState: TcxDataControllerEditState; override;
    function GetFilteredIndexByRecordIndex(ARecordIndex: Integer): Integer; override;
    function GetFilteredRecordCount: Integer; override;
    function GetFilteredRecordIndex(Index: Integer): Integer; override;
    function GetFilteringRecordCount: Integer; override;
    function GetInternalValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    function GetNodeByRecordIndex(ARecordIndex: Integer): TcxTreeListNode; virtual;
    function GetRecordIndexByNode(ANode: TcxTreeListNode): Integer; virtual;
    function HasFilterEvent: Boolean; override;
    function IsHiddenByFilter(ANode: TcxTreeListNode): Boolean; virtual;
    procedure ResetNodesFiltering; virtual;
    // Bookmark
    function InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean; override;
    procedure InternalClearBookmark; override;
    procedure InternalGotoBookmark; override;
    function InternalSaveBookmark: Boolean; override;

    function IsDataMode: Boolean; virtual;
    function IsLoading: Boolean; override;
    procedure PostRecord; virtual;
    procedure PostValues; virtual;
    procedure SetNodeData(ASource: Pointer; ADest: TcxTreeListNode);
    function StringCompare(const V1, V2: Variant): Integer; virtual;
    procedure SyncFocused(ANode: TcxTreeListNode); virtual;

    procedure BeginFilterProcess;
    procedure EndFilterProcess;
    function IsFilterProcessing: Boolean;

    property DataChangedBusy: Boolean read FDataChangedBusy write FDataChangedBusy;
    property EditingNode: TcxTreeListNode read FEditingNode write SetEditingNode;
    property Filtered: Boolean read FFiltered;
    property IsValueChanged: Boolean read FIsValueChanged write SetIsValueChanged;
    property TreeList: TcxCustomTreeList read GetTreeList;
    property ValueDefs[AIndex: Integer]: TcxValueDef read GetValueDef;
  public
    constructor Create(AOwner: TComponent); override;
    function AddItem(AItem: TObject): TcxCustomDataField; override;
    function AllocateRecord: Pointer;
    procedure ChangeValueTypeClass(AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass); override;
    function CompareNodesByColumns(ANode1, ANode2: TcxTreeListNode; AColumns: TList): Integer; virtual;
    procedure FreeNodeRecord(ANode: TcxTreeListNode);
    procedure FreeRecord(var ARecord: Pointer);
    function GetDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetNodeDisplayText(ANode: TcxTreeListNode; AIndex: Integer): Variant; virtual;
    function GetNodeValue(ANode: TcxTreeListNode; AIndex: Integer): Variant; virtual;
    function GetValue(ARecordIndex, AItemIndex: Integer): Variant; override;
    function GetValueForCompare(ANode: TcxTreeListNode; AField: TcxCustomDataField): Variant;
    procedure InitializeNodeFromRecordIndex(ANode: TcxTreeListNode);
    procedure InitializeRecordIndexFromNode(ANode: TcxTreeListNode);
    function IsBookmarkAvailable: Boolean; override;

    procedure Cancel; override;
    procedure Edit; override;
    procedure Post(AForcePost: Boolean = False); override;
    procedure PostEditingData; override;

    procedure RemoveItem(AItem: TObject); override;
    function SetEditValue(AItemIndex: Integer; const AValue: Variant;
      AEditValueSource: TcxDataEditValueSource): Boolean; override;
    procedure SetNodeValue(ANode: TcxTreeListNode; AIndex: Integer; const AValue: Variant); virtual;
    procedure SetValue(ARecordIndex, AItemIndex: Integer; const Value: Variant); override;
    procedure UpdateItemIndexes; override;

    property AllocatedRecords: Integer read FAllocatedRecords;
    property HasEditData: Boolean read GetHasEditData;
    property NodesCount: Integer read FNodesCount;
    property Root: TcxTreeListNode read GetRoot;
  end;

  { TcxTreeListCustomPopupMenu }

  TcxTreeListCustomPopupMenu = class(TcxOwnedPersistent)
  private
    FPopupMenu: TComponent;
  protected
    function GetPopupMenu: TComponent; virtual;
    function GetRoot: TComponent; virtual; abstract;
    function GetTreeList: TcxCustomTreeList; virtual; abstract;
    function Popup(X, Y: Integer): Boolean; virtual;
    procedure SetPopupMenu(AValue: TComponent); virtual;
    property PopupMenu: TComponent read GetPopupMenu write SetPopupMenu;
  public
    property Root: TComponent read GetRoot;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  { TcxTreeListCustomBuiltInMenu }

  TcxTreeListBuiltInMenuItemType = (tlmitDefault, tlmitChecked, tlmitSubItem);

  TcxTreeListCustomBuiltInMenu = class(TcxTreeListCustomPopupMenu)
  protected
    function CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
      AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
      AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False;
      AInternal: Boolean = True): TComponent; virtual; abstract;
    procedure CreatePopupMenu; virtual;
    procedure DestroyPopupMenu; virtual;
    procedure GetBitmapFromImageList(AImages: TCustomImageList;
      AImageIndex: Integer; ABitmap: TBitmap);
    function GetImages(AInternal: Boolean): TCustomImageList;
    function GetPopupClass: TComponentClass; virtual; abstract;
    function GetRoot: TComponent; override;
    function GetTreeList: TcxCustomTreeList; override;
    procedure Initialize;
    procedure MenuItemClickHandler(Sender: TObject); virtual;
  end;

  TcxTreeListCustomBuiltInMenuClass = class of TcxTreeListCustomBuiltInMenu;

  { TcxTreeListBuiltInMenu }

  TcxTreeListBuiltInMenu = class(TcxTreeListCustomBuiltInMenu)
  private
    function GetInternalPopupMenu: TPopupMenu;
  protected
    function CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
      AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
      AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False;
      AInternal: Boolean = True): TComponent; override;
    function GetPopupClass: TComponentClass; override;
    function GetRoot: TComponent; override;
    procedure CreatePopupMenu; override;
  public
    destructor Destroy; override;
    property PopupMenu: TPopupMenu read GetInternalPopupMenu;
  end;

  { TcxTreeListPopupMenu }

  TcxTreeListPopupMenuClickEvent = procedure(Sender: TcxCustomTreeList;
    AItem: TObject; var AHandled: Boolean) of object;

  TcxTreeListPopupMenuPopupEvent = procedure(Sender: TcxCustomTreeList;
    AContextMenu: TcxTreeListPopupMenu; var AHandled: Boolean) of object;

  TcxTreeListPopupMenu = class(TcxTreeListCustomPopupMenu)
  private
    FBuiltInMenu: TcxTreeListCustomBuiltInMenu;
    FUseBuiltInMenu: Boolean;
    FUserImages: TCustomImageList;
    FOnClick: TcxTreeListPopupMenuClickEvent;
    FOnPopup: TcxTreeListPopupMenuPopupEvent;
    function GetBuiltInMenu: TcxTreeListCustomBuiltInMenu;
    function GetImages: TCustomImageList;
    procedure SetUseBuiltInMenu(AValue: Boolean);
    procedure SetUserImages(AValue: TCustomImageList);
  protected
    function CreateInternalMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
      AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
      AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False): TComponent;
    procedure CreateItems; virtual;
    procedure DoExecute(ACommand: Integer); virtual;
    procedure DoMenuItemClick(Sender: TObject);
    procedure DoClick(AItem: TObject; var AHandled: Boolean); virtual;
    function DoPopup: Boolean; virtual;
    function DoShowPopupMenu(const P: TPoint): Boolean; virtual;
    function GetResourceImagesName: string; virtual;
    function GetRoot: TComponent; override;
    function GetTreeList: TcxCustomTreeList; override;
    procedure InitializeInternalMenu;
    function IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    function Popup(X, Y: Integer): Boolean; override;
    procedure SetPopupMenu(AValue: TComponent); override;
    property Images: TCustomImageList read GetImages;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
      AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
      AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False): TComponent;
    property BuiltInMenu: TcxTreeListCustomBuiltInMenu read GetBuiltInMenu;
    property UserImages: TCustomImageList read FUserImages write SetUserImages;
  published
    property PopupMenu;
    property UseBuiltInMenu: Boolean read FUseBuiltInMenu write SetUseBuiltInMenu default False;
    property OnClick: TcxTreeListPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxTreeListPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

  TcxTreeListPopupMenuClass = class of TcxTreeListPopupMenu;

  { TcxTreeListFooterPopupMenu }

  TcxTreeListFooterPopupMenuItem = (tlfmiSum, tlfmiMin, tlfmiMax, tlfmiAverage,
    tlfmiCount, tlfmiNone, tlfmiAllNodes);
  TcxTreeListFooterPopupMenuItems = set of TcxTreeListFooterPopupMenuItem;

  TcxTreeListFooterPopupMenu = class(TcxTreeListPopupMenu)
  private
    FItems: TcxTreeListFooterPopupMenuItems;
    FSummaryItems: TcxTreeListSummaryItems;
    function GetVisibleItemKinds: TcxSummaryKinds;
    function IsAllNodes: Boolean;
    function IsItemEnabled(AKind: TcxSummaryKind): Boolean;
    function IsItemsStored: Boolean;
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function GetResourceImagesName: string; override;
    function IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean; override;

    property SummaryItems: TcxTreeListSummaryItems read FSummaryItems;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Items: TcxTreeListFooterPopupMenuItems read FItems write FItems stored IsItemsStored;
  end;

  { TcxTreeListGroupFooterPopupMenu }

  TcxTreeListGroupFooterPopupMenu = class(TcxTreeListFooterPopupMenu)
  protected
    function IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean; override;
  end;

  { TcxTreeListColumnHeaderPopupMenu }

  TcxTreeListColumnHeaderPopupMenuItem = (tlchmiSortAscending, tlchmiSortDescending,
    tlchmiClearSorting, tlchmiFooter, tlchmiGroupFooters, tlchmiRemoveThisColumn,
    tlchmiFieldChooser, tlchmiHorzAlignment, tlchmiVertAlignment, tlchmiBestFit,
    tlchmiBestFitAllColumns);
  TcxTreeListColumnHeaderPopupMenuItems = set of TcxTreeListColumnHeaderPopupMenuItem;

  TcxTreeListColumnHeaderPopupMenu = class(TcxTreeListPopupMenu)
  private
    FColumn: TcxTreeListColumn;
    FItems: TcxTreeListColumnHeaderPopupMenuItems;
    FSeparatorNeeded: Boolean;
    procedure CreateAlignmentSubMenus;
    function IsItemsStored: Boolean;
    function IsSeparatorNeeded: Boolean;
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function GetResourceImagesName: string; override;
    function IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean; override;
    property Column: TcxTreeListColumn read FColumn write FColumn;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Items: TcxTreeListColumnHeaderPopupMenuItems read FItems write FItems stored IsItemsStored;
  end;

  { TcxTreeListPopupMenus }

  TcxTreeListPopupMenus = class(TcxOwnedPersistent)
  private
    FMenus: TObjectList;
    FColumnHeaderMenu: TcxTreeListColumnHeaderPopupMenu;
    FFooterMenu: TcxTreeListFooterPopupMenu;
    FGroupFooterMenu: TcxTreeListGroupFooterPopupMenu;
    function GetHitTest: TcxTreeListHitTest;
    function GetTreeList: TcxCustomTreeList;
    procedure SetColumnHeaderMenu(AValue: TcxTreeListColumnHeaderPopupMenu);
    procedure SetFooterMenu(AValue: TcxTreeListFooterPopupMenu);
    procedure SetGroupFooterMenu(AValue: TcxTreeListGroupFooterPopupMenu);
  protected
    procedure CreateMenus;
    function DoShowPopupMenu(const P: TPoint): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    property HitTest: TcxTreeListHitTest read GetHitTest;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TreeList: TcxCustomTreeList read GetTreeList;
  published
    property ColumnHeaderMenu: TcxTreeListColumnHeaderPopupMenu read FColumnHeaderMenu write SetColumnHeaderMenu;
    property FooterMenu: TcxTreeListFooterPopupMenu read FFooterMenu write SetFooterMenu;
    property GroupFooterMenu: TcxTreeListGroupFooterPopupMenu read FGroupFooterMenu write SetGroupFooterMenu;
  end;

  { TcxTreeListSummary }

  TcxTreeListSummary = class
  private
    FAbsoluteFooterSummaryItems: TList;
    FAbsoluteGroupFooterSummaryItems: TList;
    FFooterSummaryRowCount: Integer;
    FFooterSummaryVisibleCount: Integer;
    FGroupFooterSummaryRowCount: Integer;
    FGroupFooterSummaryVisibleCount: Integer;
    FIsDirty: Boolean;
    FTreeList: TcxCustomTreeList;
    function GetFooterSummaryCount: Integer;
    function GetFooterSummaryItem(AIndex: Integer): TcxTreeListSummaryItem;
    function GetFooterSummaryText(ASummaryItem: TcxTreeListSummaryItem): string;
    function GetFooterSummaryValue(ASummaryItem: TcxTreeListSummaryItem): Variant;
    function GetGroupFooterSummaryCount: Integer;
    function GetGroupFooterSummaryItem(AIndex: Integer): TcxTreeListSummaryItem;
    function GetGroupFooterSummaryText(ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode): string;
    function GetGroupFooterSummaryValue(ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode): Variant;
    function GetRoot: TcxTreeListNode;
    procedure SetFooterSummaryValue(ASummaryItem: TcxTreeListSummaryItem; const Value: Variant);
    procedure SetGroupFooterSummaryValue(ASummaryItem: TcxTreeListSummaryItem;
      ANode: TcxTreeListNode; const Value: Variant);
  protected
    procedure AddSummaryItems(AAbsoluteSummaryItems: TList; ASummaryItems: TcxTreeListSummaryItems;
      AIsVisible: Boolean; var AVisibleCount, AVisibleRowCount: Integer);
    procedure Calculate;
    procedure CalculateNode(ADestNode, AValueNode: TcxTreeListNode; AItems: TList);
    procedure CheckChanges; virtual;
    function DoProcessSummaryValue(ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem;
      var ASummaryValue: Variant; var ASummaryCount: Integer; var AValue: Variant): Boolean;
    procedure FinalizeValues(ANode: TcxTreeListNode; AItems: TList);
    procedure InitializeValues(ANode: TcxTreeListNode; AItems: TList);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure ProcessNode(AParent: TcxTreeListNode);
    procedure ProcessSummaryValue(ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem;
      var ASummaryValue: Variant; var ASummaryCount: Integer);
    procedure ProcessSummaryValues(ADestNode, AValueNode: TcxTreeListNode; AItems: TList);

    property IsDirty: Boolean read FIsDirty write FIsDirty;
    property Root: TcxTreeListNode read GetRoot;
  public
    constructor Create(ATreeList: TcxCustomTreeList); virtual;
    destructor Destroy; override;
    procedure Recalculate;

    property FooterSummaryCount: Integer read GetFooterSummaryCount;
    property FooterSummaryRowCount: Integer read FFooterSummaryRowCount;
    property FooterSummaryVisibleCount: Integer read FFooterSummaryVisibleCount;
    property FooterSummaryItems[Index: Integer]: TcxTreeListSummaryItem read GetFooterSummaryItem;
    property FooterSummaryTexts[ASummaryItem: TcxTreeListSummaryItem]: string read GetFooterSummaryText;
    property FooterSummaryValues[ASummaryItem: TcxTreeListSummaryItem]: Variant
      read GetFooterSummaryValue write SetFooterSummaryValue;
    property GroupFooterSummaryCount: Integer read GetGroupFooterSummaryCount;
    property GroupFooterSummaryRowCount: Integer read FGroupFooterSummaryRowCount;
    property GroupFooterSummaryVisibleCount: Integer read FGroupFooterSummaryVisibleCount;
    property GroupFooterSummaryItems[Index: Integer]: TcxTreeListSummaryItem read GetGroupFooterSummaryItem;
    property GroupFooterSummaryTexts[ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode]: string read GetGroupFooterSummaryText;
    property GroupFooterSummaryValues[ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode]: Variant
      read GetGroupFooterSummaryValue write SetGroupFooterSummaryValue;
    property TreeList: TcxCustomTreeList read FTreeList;
  end;

  TcxTreeListSummaryClass = class of TcxTreeListSummary;

  { TcxTreeListLevelInfo }

  TcxTreeListLevelInfo = class
  private
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FOffset: Integer;
    FSize: TSize;
    FStateImages: TCustomImageList;
    FStateImagesChangeLink: TChangeLink;
    FTreeList: TcxCustomTreeList;
    FWidth: Integer;

    function GetTotalWidth: Integer;
    function GetDynamicIndent: Boolean;
    procedure SetImages(AValue: TCustomImageList);
    procedure SetStateImages(AValue: TCustomImageList);
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure Changed(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    function Update(const AOffset: Integer): Integer;
  public
    constructor Create(ATreeList: TcxCustomTreeList);
    destructor Destroy; override;

    property DynamicIndent: Boolean read GetDynamicIndent;
    property Images: TCustomImageList read FImages write SetImages;
    property Offset: Integer read FOffset write FOffset;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Size: TSize read FSize;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property TotalWidth: Integer read GetTotalWidth;
    property TreeList: TcxCustomTreeList read FTreeList;
    property Width: Integer read FWidth;
  end;

  { TcxTreeListLikeParams }

  TcxTreeListLikeParams = class
  private
    FPercent: Char;
    FUnderline: Char;
  public
    constructor Create(APercent: Char = '%'; AUnderline: Char = '_');
    property Percent: Char read FPercent write FPercent;
    property UnderLine: Char read FUnderline write FUnderline;
  end;

  { TcxCustomTreeList }

  TcxTreeListNodeChangedEvent = procedure(
    Sender: TcxCustomTreeList; ANode: TcxTreeListNode) of object;

  TcxTreeListNodeCheckChangedEvent = procedure(
    Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState) of object;

  TcxTreeListNodeCheckChangingEvent = procedure(
    Sender: TcxCustomTreeList; ANode: TcxTreeListNode; ACheckState: TcxCheckBoxState; var Allow: Boolean) of object;

  TcxTreeListNodeChangingEvent = procedure(
    Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean) of object;

  TcxTreeListColumnChangedEvent = procedure(
    Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn) of object;

  TcxTreeListBandChangedEvent = procedure(
    Sender: TcxCustomTreeList; ABand: TcxTreeListBand) of object;

  TcxTreeListMoveToEvent = procedure(Sender: TcxCustomTreeList; AttachNode: TcxTreeListNode;
    AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy, Done: Boolean) of object;

  TcxTreeListNodeCompareEvent = procedure(Sender: TcxCustomTreeList;
    ANode1, ANode2: TcxTreeListNode; var ACompare: Integer) of object;

  TcxTreeListFocusedNodeChangedEvent = procedure(Sender: TcxCustomTreeList;
    APrevFocusedNode, AFocusedNode: TcxTreeListNode) of object;

  TcxTreeListGetCellHintEvent = procedure(Sender: TcxCustomTreeList; ACell: TObject;
    var AText: string; var ANeedShow: Boolean) of object;

  TcxTreeListGetDragDropTextEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; var AText: string) of object;

  TcxTreeListGetLevelImagesEvent = procedure(Sender: TcxCustomTreeList; ALevel: Integer;
    var AImages, AStateImages: TCustomImageList) of object;

  TcxTreeListGetNodeImageIndexEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TcxImageIndex) of object;

  TcxTreeListGetNodeHeightEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; var AHeight: Integer) of object;

  TcxTreeListGetPreviewTextEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; var AText: string) of object;

  TcxTreeListGetNodePreviewHeightEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; const ADisplayValue: Variant; var AHeight: Integer) of object;

  TcxTreeListIsGroupNodeEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; var IsGroup: Boolean) of object;

  TcxTreeListEditingEvent = procedure(Sender: TcxCustomTreeList;
    AColumn: TcxTreeListColumn; var Allow: Boolean) of object;

  TcxTreeListFocusedColumnChanged = procedure(Sender: TcxCustomTreeList;
    APrevFocusedColumn, AFocusedColumn: TcxTreeListColumn) of object;

  TcxTreeListHotTrackNodeEvent = procedure(Sender: TcxCustomTreeList; ANode: TcxTreeListNode;
    AShift: TShiftState; var ACursor: TCursor) of object;

  TcxTreeListNodeDataChangedEvent = procedure(Sender: TcxCustomTreeList;
    ANode: TcxTreeListNode; AColumn: TcxTreeListColumn) of object;

  TcxTreeListCustomDrawBandCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListBandCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawBackgroundCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListCustomCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawHeaderCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListHeaderCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawEditCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawIndentCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListIndentCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawIndicatorCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListIndicatorCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListCustomDrawFooterCellEvent = procedure(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListFooterCellViewInfo; var ADone: Boolean) of object;

  TcxTreeListFilterNodeEvent = procedure(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Accept: Boolean) of object;

  TcxTreeListFindMode = (tlfmNormal, tlfmLike, tlfmExact);

  { TcxTreeListConditionalFormattingProvider }

  TcxTreeListConditionalFormattingProvider = class(TcxCustomControlControllerConditionalFormattingProvider)
  strict private
    FOwner: TcxCustomTreeList;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function EditCellViewInfoToPoint(ACellViewInfo: TObject): TPoint; override;
    function GetController: TcxCustomControlController; override;
    function GetItemDisplayName(AItem: TcxCustomInplaceEditContainer): string; override;
    function GetLookAndFeel: TcxLookAndFeel; override;
    function GetOwner: TComponent; override;
    function GetRecordCount: Integer; override;
    function DoGetParentForm: TCustomForm; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function IsItemVisible(AItem: TcxCustomInplaceEditContainer): Boolean; override;
    function IsRightToLeft: Boolean; override;
    function IsRowVisible(const ARow: Integer): Boolean; override;
  public
    constructor Create(AOwner: TcxCustomTreeList); reintroduce;
  end;

  { TcxTreeListFilterValueList }

  TcxTreeListFilterValueList = class(TcxControlFilterValueList)
  protected
    function GetDateTimeRelativeFilterDisplayText(AKind: TcxFilterOperatorKind): string; override;
  end;


  TcxTreeListSummaryEventArguments = record
    Node: TcxTreeListNode;
    SummaryItem: TcxTreeListSummaryItem;
  end;

  TcxTreeListSummaryEventOutArguments = record
    Value: Variant;
    SummaryValue: Variant;
    CountValue: Integer;
    Done: Boolean;
  end;

  TcxTreeListSummaryEvent = procedure(ASender: TcxCustomTreeList;
    const Arguments: TcxTreeListSummaryEventArguments;
    var OutArguments: TcxTreeListSummaryEventOutArguments) of object;

  TcxCustomTreeList = class(TcxExtEditingControl, IcxNavigator,
    IcxStoredParent,
    IcxStoredObject,
    IcxTreeListDesignTimeOperations,
    IdxSkinSupport,
    IcxDataControllerConditionalFormattingProviderOwner,
    IcxFilterControl)
  private
    FAbsoluteItems: TList;
    FAbsoluteVisibleItems: TList;
    FBands: TcxTreeListBands;
    FDefaultLevelInfo: TcxTreeListLevelInfo;
    FChanges: TcxTreeListChanges;
    FChangesLocked: Boolean;
    FConditionalFormattingProvider: TcxTreeListConditionalFormattingProvider;
    FCustomizing: TcxTreeListCustomizing;
    FDefaultIndentSize: TSize;
    FDefaultLayout: Boolean;
    FDefaultRowHeight: Integer;
    FDelayTimer: TTimer;
    FDesigners: TList;
    FDragNode: TcxTreeListNode;
    FExpansionLevel: Integer;
    FIgnoreLoadingStatus: Boolean;
    FIsCancelOperation: Boolean;
    FIsRefreshFields: Boolean;
    FIsRestoring: Boolean;
    FLevelsInfo: TcxObjectList;
    FLockChanges: Boolean;
    FNavigatorNotifier: TcxNavigatorControlNotifier;
    FNextStoreID: Integer;
    FOptionsCustomizing: TcxTreeListOptionsCustomizing;
    FOptionsSelection: TcxTreeListOptionsSelection;
    FPopupMenus: TcxTreeListPopupMenus;
    FPopupMenusEvents: TNotifyEvent;
    FPostChanged: Boolean;
    FPreview: TcxTreeListPreview;
    FRoot: TcxTreeListNode;
    FSortedColumns: TList;
    FSelectionList: TList;
    FStoredCursor: TCursor;
    FStoringName: string;
    FStylesEvents: TNotifyEvent;
    FSummary: TcxTreeListSummary;
    FTopVisibleNode: TcxTreeListNode;
    FVisibleColumns: TList;

    FOnBandHeaderClick: TcxTreeListBandChangedEvent;
    FOnBandPosChanged: TcxTreeListBandChangedEvent;
    FOnBandSizeChanged: TcxTreeListBandChangedEvent;
    FOnCanFocusNode: TcxTreeListNodeChangingEvent;
    FOnCanSelectNode: TcxTreeListNodeChangingEvent;
    FOnChange: TNotifyEvent;
    FOnColumnHeaderClick: TcxTreeListColumnChangedEvent;
    FOnColumnPosChanged: TcxTreeListColumnChangedEvent;
    FOnColumnSizeChanged: TcxTreeListColumnChangedEvent;
    FOnCompare: TcxTreeListNodeCompareEvent;

    //
    FGlassCursorRefCount: Integer;
    FOnBeginDragNode: TcxTreeListNodeChangingEvent;
    FOnCollapsed: TcxTreeListNodeChangedEvent;
    FOnCollapsing: TcxTreeListNodeChangingEvent;
    FOnCustomDrawBackgroundCell: TcxTreeListCustomDrawBackgroundCellEvent;
    FOnCustomDrawBandCell: TcxTreeListCustomDrawBandCellEvent;
    FOnCustomDrawBandHeaderCell: TcxTreeListCustomDrawHeaderCellEvent;
    FOnCustomDrawDataCell: TcxTreeListCustomDrawEditCellEvent;
    FOnCustomDrawFooterCell: TcxTreeListCustomDrawFooterCellEvent;
    FOnCustomDrawHeaderCell: TcxTreeListCustomDrawHeaderCellEvent;
    FOnCustomDrawIndentCell: TcxTreeListCustomDrawIndentCellEvent;
    FOnCustomDrawIndicatorCell: TcxTreeListCustomDrawIndicatorCellEvent;
    FOnCustomDrawPreviewCell: TcxTreeListCustomDrawEditCellEvent;
    FOnCustomizationVisibleChanged: TNotifyEvent;
    FOnDeletion: TcxTreeListNodeChangedEvent;
    FOnExpanded: TcxTreeListNodeChangedEvent;
    FOnExpanding: TcxTreeListNodeChangingEvent;
    FOnFilterNode: TcxTreeListFilterNodeEvent;
    FOnGetLevelImages: TcxTreeListGetLevelImagesEvent;
    FOnGetNodeHeight: TcxTreeListGetNodeHeightEvent;
    FOnGetNodeImageIndex: TcxTreeListGetNodeImageIndexEvent;
    FOnGetNodePreviewHeight: TcxTreeListGetNodePreviewHeightEvent;

    FOnAfterSummary: TNotifyEvent;
    FOnDataChanged: TNotifyEvent;
    FOnFocusedColumnChanged: TcxTreeListFocusedColumnChanged;
    FOnFocusedNodeChanged: TcxTreeListFocusedNodeChangedEvent;
    FOnGetCellHint: TcxTreeListGetCellHintEvent;
    FOnGetDragDropText: TcxTreeListGetDragDropTextEvent;
    FOnLeftPosChanged: TNotifyEvent;
    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnInitStoredObject: TcxInitStoredObjectEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;

    FOnHotTrackNode: TcxTreeListHotTrackNodeEvent;
    FOnIsGroupNode: TcxTreeListIsGroupNodeEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnMoveTo: TcxTreeListMoveToEvent;
    FOnNodeChanged: TcxTreeListNodeDataChangedEvent;
    FOnNodeCheckChanged: TcxTreeListNodeCheckChangedEvent;
    FOnNodeCheckChanging: TcxTreeListNodeCheckChangingEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSorted: TNotifyEvent;
    FOnSorting: TNotifyEvent;
    FOnSummary: TcxTreeListSummaryEvent;
    FOnTopRecordIndexChanged: TNotifyEvent;

    function GetAvailableContentWidth: Integer;
    function GetAbsoluteCount: Integer;
    function GetAbsoluteItem(AIndex: Integer): TcxTreeListNode;
    function GetAbsoluteItemsList: TList;
    function GetAbsoluteVisibleCount: Integer;
    function GetAbsoluteVisibleItem(AIndex: Integer): TcxTreeListNode;
    function GetAbsoluteVisibleItemsList: TList;
    function GetBands: TcxTreeListBands;
    function GetColumn(AIndex: Integer): TcxTreeListColumn;
    function GetColumnCount: Integer;
    function GetColumnsList: TList;
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
    function GetController: TcxTreeListController;
    function GetCount: Integer;
    function GetDataController: TcxTreeListDataController;
    function GetDateTimeHandling: TcxTreeListDateTimeHandling;
    function GetDefaultRowHeight: Integer;
    function GetFilter: TcxDataFilterCriteria;
    function GetFilterBox: TcxTreeListFilterBox;
    function GetFiltering: TcxTreeListFiltering;
    function GetFocusedColumn: TcxTreeListColumn;
    function GetFocusedNode: TcxTreeListNode;
    function GetHitTest: TcxTreeListHitTest;
    function GetImages: TCustomImageList;
    function GetIndentWidth: Integer;
    function GetInplaceEditor: TcxCustomEdit;
    function GetIsCancelOperation: Boolean;
    function GetIsEditing: Boolean;
    function GetIsInserting: Boolean;
    function GetItem(Index: Integer): TcxTreeListNode;
    function GetLastNode: TcxTreeListNode;
    function GetLastPartVisibleNode: TcxTreeListNode;
    function GetLastVisibleNode: TcxTreeListNode;
    function GetLevelInfo(ALevel: Integer): TcxTreeListLevelInfo;
    function GetNavigatorIsActive: Boolean;
    function GetOnEdited: TcxTreeListColumnChangedEvent;
    function GetOnEditing: TcxTreeListEditingEvent;
    function GetOnEditValueChanged: TcxTreeListColumnChangedEvent;
    function GetOptionsBehavior: TcxTreeListOptionsBehavior;
    function GetOptionsData: TcxTreeListOptionsData;
    function GetOptionsView: TcxTreeListOptionsView;
    function GetSearching: Boolean;
    function GetSearchingText: string;
    function GetSelection(AIndex: Integer): TcxTreeListNode;
    function GetSelectionCount: Integer;
    function GetSorted: Boolean;
    function GetSortedColumnCount: Integer;
    function GetSortedColumn(Index: Integer): TcxTreeListColumn;
    function GetStateImages: TCustomImageList;
    function GetStyles: TcxTreeListStyles;
    function GetTopNode: TcxTreeListNode;
    function GetTopVisibleNode: TcxTreeListNode;
    function GetVisibleCount: Integer;
    function GetViewInfo: TcxTreeListViewInfo;
    function GetVisibleColumn(AIndex: Integer): TcxTreeListColumn;
    function GetVisibleColumnCount: Integer;
    procedure SetAbsoluteItem(AIndex: Integer; AValue: TcxTreeListNode);
    procedure SetAbsoluteVisibleItem(AIndex: Integer; AValue: TcxTreeListNode);
    procedure SetBands(Value: TcxTreeListBands);
    procedure SetColumn(AIndex: Integer; Value: TcxTreeListColumn);
    procedure SetDateTimeHandling(AValue: TcxTreeListDateTimeHandling);
    procedure SetDefaultRowHeight(AValue: Integer);
    procedure SetDefaultLayout(AValue: Boolean);
    procedure SetFilterBox(AValue: TcxTreeListFilterBox);
    procedure SetFiltering(AValue: TcxTreeListFiltering);
    procedure SetFocusedColumn(AValue: TcxTreeListColumn);
    procedure SetFocusedNodeProp(Value: TcxTreeListNode);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetIsRestoring(AValue: Boolean);
    procedure SetLastVisibleNode(AValue: TcxTreeListNode);
    procedure SetOnEdited(Value: TcxTreeListColumnChangedEvent);
    procedure SetOnEditing(Value: TcxTreeListEditingEvent);
    procedure SetOnEditValueChanged(Value: TcxTreeListColumnChangedEvent);
    procedure SetOptionsBehavior(Value: TcxTreeListOptionsBehavior);
    procedure SetOptionsCustomizing(Value: TcxTreeListOptionsCustomizing);
    procedure SetOptionsData(Value: TcxTreeListOptionsData);
    procedure SetOptionsSelection(Value: TcxTreeListOptionsSelection);
    procedure SetOptionsView(Value: TcxTreeListOptionsView);
    procedure SetPopupMenus(Value: TcxTreeListPopupMenus);
    procedure SetPreview(Value: TcxTreeListPreview);
    procedure SetSearchingText(const Value: string);
    procedure SetSorted(Value: Boolean);
    procedure SetStateImages(AValue: TCustomImageList);
    procedure SetStyles(Value: TcxTreeListStyles);
    procedure SetTopVisibleNode(ANode: TcxTreeListNode);
    procedure SetVisibleColumn(Index: Integer; AValue: TcxTreeListColumn);
    procedure DoChangedTimer(Sender: TObject);
    procedure RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string;
      AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean;
      const ARestoreTreeListName: string);
    procedure StoreTo(AStorageType: TcxStorageType; const AStorageName: string;
      AStorageStream: TStream; AReCreate: Boolean; const ASaveTreeListName: string);
    procedure UpdateDesignerForms;
    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxStoredParent
    function IcxStoredParent.CreateChild = StoredCreateChild;
    procedure IcxStoredParent.DeleteChild = StoredDeleteChild;
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    // messages
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure WMCheckState(var Message: TMessage); message WM_CHECKSTATE;
  protected
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
    procedure AddChanges(AChanges: TcxTreeListChanges);
    function AddNode(ANode, ARelative: TcxTreeListNode; AData: Pointer;
      AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode; virtual;
    function AddNodeInternal(ANode, ARelative: TcxTreeListNode;
      AddMethod: TcxTreeListNodeAttachMode): TcxTreeListNode; virtual;

    procedure AfterMouseDown(AButton: TMouseButton; X, Y: Integer); override;
    procedure AssignData(ASource: TcxCustomTreeList); virtual;
    procedure AssignOptions(ASource: TcxCustomTreeList); virtual;
    procedure BeforeUpdate; override;
    procedure BiDiModeChanged; override;
    procedure BoundsChanged; override;
    procedure CalculateForceIndentWidth;
    function CanUseMultiThreadedSorting: Boolean; virtual;
    function CanCompare: Boolean; virtual;
    procedure CheckChanges; virtual;
    procedure CheckEvents;
    procedure CheckImageListReferences(AComponent: TComponent; Operation: TOperation);
    procedure CheckLevelsInfo; virtual;
    procedure CheckSelectionAndFocused;
    procedure CheckSortOrderList;
    procedure CheckStructure; virtual;
    procedure ClearCalculatedWidths;
    procedure ColumnSortOrderChanged(AColumn: TcxTreeListColumn; AShift: TShiftState);
    function CreateNode: TcxTreeListNode; virtual;
    procedure CreateSubClasses; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroySubClasses; override;
    procedure DoAfterSummary; virtual;
    procedure DoBandHeaderClick(ABand: TcxTreeListBand); virtual;
    procedure DoBandPosChanged(ABand: TcxTreeListBand); virtual;
    function DoBeginDragNode(ANode: TcxTreeListNode): Boolean;
    function DoCanFocusNode(ANode: TcxTreeListNode): Boolean; virtual;
    function DoCanNodeCollapse(ANode: TcxTreeListNode): Boolean; virtual;
    function DoCanNodeExpand(ANode: TcxTreeListNode): Boolean; virtual;
    function DoCanSelectNode(ANode: TcxTreeListNode): Boolean; virtual;
    procedure DoChanged(BlockChangedEvent: Boolean = True); virtual;
    procedure DoColumnHeaderClick(AColumn: TcxTreeListColumn); virtual;
    procedure DoColumnPosChanged(AColumn: TcxTreeListColumn); virtual;
    procedure DoCompare(ANode1, ANode2: TcxTreeListNode; var ACompare: Integer); virtual;
    procedure DoCustomizationVisibleChanged; virtual;
    procedure DoDataChangedEvent(Sender: TObject); virtual;
    procedure DoDeletion(ANode: TcxTreeListNode); virtual;
    procedure DoEditValueChanged(AItem: TcxCustomInplaceEditContainer); override;
    procedure DoExpand(ANode: TcxTreeListNode); virtual;
    function DoFilterNodeEvent(ANode: TcxTreeListNode): Boolean; virtual;
    procedure DoFilterNodes; virtual;
    procedure DoFocusedItemChanged(APrevFocusedItem, AFocusedItem: TcxCustomInplaceEditContainer); virtual;
    procedure DoFocusedNodeChanged(APrevNode, AFocusedNode: TcxTreeListNode); virtual;
    procedure DoGetCellHint(ACell: TObject; var AText: string; var ANeedShow: Boolean); override;
    procedure DoGetLevelImages(AInfo: TcxTreeListLevelInfo; ALevel: Integer); virtual;
    procedure DoGetNodeHeight(ANode: TcxTreeListNode; var AHeight: Integer); virtual;
    function DoGetNodeImageIndex(ANode: TcxTreeListNode; AIndex: TcxTreeListImageIndexType): TcxImageIndex; virtual;
    procedure DoGetPreviewHeight(ANode: TcxTreeListNode; var AHeight: Integer); virtual;
    procedure DoHotTrackNode(ANode: TcxTreeListNode; AShift: TShiftState; var ACursor: TCursor); virtual;
    procedure DoInplaceEditContainerItemAdded(AItem: TcxCustomInplaceEditContainer); override;
    procedure DoInplaceEditContainerItemRemoved(AItem: TcxCustomInplaceEditContainer); override;
    function DoIsGroupNode(ANode: TcxTreeListNode): Boolean; virtual;
    procedure DoLayoutChangedEvent; virtual;
    procedure DoLeftPosChanged; virtual;
    function DoMoveToEvent(AttachNode: TcxTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy: Boolean): Boolean; virtual;
    procedure DoNodeChanged(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn); virtual;
    procedure DoNodeCheckChanged(ANode: TcxTreeListNode; AState: TcxCheckBoxState); virtual;
    function DoNodeCheckChanging(ANode: TcxTreeListNode; AState: TcxCheckBoxState): Boolean; virtual;
    procedure DoNodeCollapsed(ANode: TcxTreeListNode); virtual;
    procedure DoNodeExpanded(ANode: TcxTreeListNode); virtual;
    procedure DoSelectAll; virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoSetNodeFocused(ANode: TcxTreeListNode; AValue: Boolean; Shift: TShiftState = []); virtual;
    procedure DoSorted; virtual;
    procedure DoSorting; virtual;
    procedure DoSortNodes; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoTopRecordIndexChanged; virtual;
    procedure FilterChanged; override;
    procedure FindFilterChanged; virtual;
    procedure FontChanged; override;
    procedure ForceLayoutChanged; virtual;
    function GetNavigatorButtonsControl: IcxNavigator; override;
    function HasFilterEvent: Boolean; virtual;

    // used classes
    function GetBandItemClass: TcxTreeListBandClass; virtual;
    function GetControllerClass: TcxCustomControlControllerClass; override;
    function GetControlStylesClass: TcxCustomControlStylesClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetDateTimeHandlingClass: TcxControlOptionsDateTimeHandlingClass; override;
    function GetEditingControllerClass: TcxEditingControllerClass; override;
    function GetFilterValueListClass: TcxControlFilterValueListClass; override;
    function GetHitTestControllerClass: TcxHitTestControllerClass; override;
    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsCustomizingClass: TcxTreeListOptionsCustomizingClass; virtual;
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    function GetOptionsFilterBoxClass: TcxControlOptionsFilterBoxClass; override;
    function GetOptionsFilteringClass: TcxControlOptionsFilteringClass; override;
    function GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass; virtual;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetPainterClass: TcxCustomControlPainterClass; override;
    function GetPreviewClass: TcxTreeListPreviewClass; virtual;
    function GetRootClass: TcxTreeListNodeClass; virtual;
    function GetSummaryClass: TcxTreeListSummaryClass; virtual;
    function GetTreeListColumnClass: TcxTreeListColumnClass; virtual;
    function GetTreeListCustomizingClass: TcxTreeListCustomizingClass; virtual;
    function GetViewInfoClass: TcxCustomControlViewInfoClass; override;
    //
    function GetCountNodeForBestFit: Integer; virtual;
    function GetDefaultColumnContainer: TcxTreeListBand;
    function GetEditAutoHeight: TcxInplaceEditAutoHeight; virtual;
    function GetSameColumn(AColumn: TcxTreeListColumn): TcxTreeListColumn;
    function GetStartNodeForBestFit: TcxTreeListNode; virtual;
    function IsLocked: Boolean; override;
    function IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean; override;
    function IsTreeListLocked: Boolean; virtual;
    procedure ImagesChanged(Sender: TObject); virtual;
    procedure InitializeFields; virtual;
    procedure InternalClearAll;
    procedure InternalCopy(ANode, ADestNode: TcxTreeListNode); virtual;
    procedure InternalDelete(ANodes: TList);
    procedure InternalMove(ANode, ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode); virtual;
    function IsNodeEdited(ANode: TcxTreeListNode): Boolean;
    function IsNodeInserting(ANode: TcxTreeListNode): Boolean; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure RefreshFields; virtual;
    procedure ScrollWindow(const DX, DY: Integer); virtual;
    procedure StructureChanged; virtual;
    procedure SetImageList(const AValue: TCustomImageList; var AFieldValue: TCustomImageList; const AChangeLink: TChangeLink);
    procedure SetNodeParent(ANewNode, ANewParent: TcxTreeListNode; AMode: TcxTreeListNodeAddMode);
    procedure SetFocusedVisibleNode(ANode: TcxTreeListNode);
    procedure UpdateFocusedNode(AColumn: TcxTreeListColumn); virtual;
    procedure ValidateStates;


    procedure BeforeShowingEdit;
    function CanMultiSelect: Boolean; virtual;

    procedure DoBandSizeChanged(ABand: TcxTreeListBand); virtual;
    procedure DoClear; virtual;
    procedure DoColumnSizeChanged(AColumn: TcxTreeListColumn); virtual;
    function DoCreateColumn: TcxTreeListColumn;
    procedure DoDeleteSelection; virtual;
    procedure DoDeleteNode(ANode: TcxTreeListNode); virtual;
    procedure DoInternalMoveTo(AttachNode: TcxTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);
    procedure DoMoveTo(AttachNode: TcxTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean); virtual;
    // doXXXX methods fire OnXXXX events
    procedure DoOnBandSizeChanged(ABand: TcxTreeListBand); virtual;
    procedure DoOnColumnSizeChanged(AColumn: TcxTreeListColumn); virtual;

    //
    function DoWriteHeaderToText: string; virtual;
    function DoWriteNodeToText(ANode: TcxTreeListNode): string; virtual;
    procedure DoWriteToClipboard(AOnlySelected: Boolean; ACheckVisible: Boolean = False); virtual;
    function DoWriteToText(AOnlySelected: Boolean; out AText: string; ACheckVisible: Boolean = False): Boolean;
    procedure DeleteNodes(AList: TList); virtual;
    function GetIsActive: Boolean; virtual;
    function GetFixedContentSize: Integer; virtual;
    function GetMaxBandWidth(ABand: TcxTreeListBand): Integer;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    procedure InitInsertingNode(ANode: TcxTreeListNode); virtual;
    function InsertNode(ARelative: TcxTreeListNode; IsAppend: Boolean): Boolean; virtual;
    procedure MakeNodeVisible(ANode: TcxTreeListNode); virtual;
    procedure RestoreCursor;
    procedure SetGlassCursor;
    procedure SortingStateChanged(AColumn: TcxTreeListColumn; AShift: TShiftState);

    // IcxTreeListDesignTimeOperations implementation
    function HasAllItems: Boolean; virtual;
    function SupportBandColumnEditor: Boolean; virtual;
    function SupportItemsEditor: Boolean; virtual;
    function SupportCreateAllItems: Boolean; virtual;
    procedure CreateAllItems(AMissingItemsOnly: Boolean = False); virtual;
    // IcxNavigatorRecordPosition
    function NavigatorGetRecordCount: Integer; override;
    function NavigatorGetRecordIndex: Integer; override;
    // IcxNavigator
    function IcxNavigator.IsActive = GetNavigatorIsActive;
    function IcxNavigator.IsEditing = NavigatorIsEditing;
    function IcxNavigator.IsBof = IsBof;
    function IcxNavigator.IsEof = IsEof;
    function NavigatorIsEditing: Boolean;

    procedure RefreshNavigatorButtons;
    procedure DoAction(AButtonIndex: Integer);
    function GetNotifier: TcxNavigatorControlNotifier;
    function IsActionSupported(AButtonIndex: Integer): Boolean; virtual;

    function CanAppend: Boolean;
    function CanEdit: Boolean;
    function CanDelete: Boolean;
    function CanInsert: Boolean;

    procedure ClearBookmark;
    procedure GotoBookmark;
    function HasData: Boolean; virtual;
    function IsBookmarkAvailable: Boolean;
    procedure SaveBookmark;
    // updates
    // nodes
//    procedure DoSetNodeSelected(ANode: TcxTreeListNode; Value: Boolean; Shift: TShiftState = []); virtual;
    function GetSelectionsEx: TList;
    // sorting
    // overridden methods
    procedure BeginGestureScroll(APos: TPoint); override;
    function CanFocusOnClick: Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckCreateDesignSelectionHelper; override;
    procedure ControlUpdateData(AInfo: TcxUpdateControlInfo); override;
    procedure DataChanged; override;
    procedure DataLayoutChanged; override;
    procedure DoLayoutChanged; override;
    function DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean; override;
    function DragDropImageDisplayRect: TRect; override;
    procedure DrawDragDropImage(ADragBitmap: TBitmap; ACanvas: TcxCanvas); override;
    function GetDragInfoImagePosition(APoint: TPoint): TPoint;
    function GetDragDropText: string; virtual;
    function GetDragDropViewParams: TcxViewParams; virtual;
    function GetDragObjectClass: TDragControlObjectClass; override;
    procedure InitScrollBarsParameters; override;
    procedure InitScrollBarsParametersCache; override;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); override;
    function CanScrollContentByGestureWithoutScrollBars: Boolean; override;
    function GetStatusHint(const APoint: TPoint): string; override;
    //
    function InternalCollapseNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean; virtual;
    function InternalExpandNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean; virtual;

    // inherits for loading bug fixing
    procedure AssignItemsPosition(ABandIndexOnly: Boolean);
    procedure ReadState(Reader: TReader); override;
    procedure SaveItemsPosition(ABandIndexOnly: Boolean);
    procedure Updated; override;
    procedure Updating; override;

    // cxStorage implementation
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    function GetStoredObjectName: string; virtual;
    function GetStoredObjectProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure StoredDeleteChild(const AObjectName: string; AObject: TObject); virtual;
    // standart control methods
    function IsUpdating: Boolean;
    procedure Loaded; override;
    procedure Modified; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;

    property AbsoluteItemsList: TList read GetAbsoluteItemsList;
    property AbsoluteVisibleItemsList: TList read GetAbsoluteVisibleItemsList;
    property AvailableContentWidth: Integer read GetAvailableContentWidth;
    property ChangeDelayTimer: TTimer read FDelayTimer;
    property Changes: TcxTreeListChanges read FChanges write FChanges;
    property ChangesLocked: Boolean read FChangesLocked write FChangesLocked;
    property ColumnsList: TList read GetColumnsList;
    property ConditionalFormattingProvider: TcxTreeListConditionalFormattingProvider read FConditionalFormattingProvider;
    property Controller: TcxTreeListController read GetController;
    property DataController: TcxTreeListDataController read GetDataController;
    property DateTimeHandling: TcxTreeListDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property FilterBox: TcxTreeListFilterBox read GetFilterBox write SetFilterBox;
    property Filtering: TcxTreeListFiltering read GetFiltering write SetFiltering;
    property IgnoreLoadingStatus: Boolean read FIgnoreLoadingStatus write FIgnoreLoadingStatus;
    property IsCancelOperation: Boolean read GetIsCancelOperation write FIsCancelOperation;
    property IndentWidth: Integer read GetIndentWidth;
    property IsRefreshFields: Boolean read FIsRefreshFields write FIsRefreshFields;
    property IsRestoring: Boolean read FIsRestoring write SetIsRestoring;
    property LockChanges: Boolean read FLockChanges write FLockChanges;
    property LevelsInfo[ALevel: Integer]: TcxTreeListLevelInfo read GetLevelInfo;
    property SortedColumnsList: TList read FSortedColumns;
    property StoredCursor: TCursor read FStoredCursor;
    property StoringName: string read FStoringName write FStoringName;
    property ViewInfo: TcxTreeListViewInfo read GetViewInfo;

    property OnAfterSummary: TNotifyEvent read FOnAfterSummary write FOnAfterSummary;
    property OnBandHeaderClick: TcxTreeListBandChangedEvent read FOnBandHeaderClick write FOnBandHeaderClick;
    property OnBandPosChanged: TcxTreeListBandChangedEvent read FOnBandPosChanged write FOnBandPosChanged;
    property OnBandSizeChanged: TcxTreeListBandChangedEvent read FOnBandSizeChanged write FOnBandSizeChanged;
    property OnBeginDragNode: TcxTreeListNodeChangingEvent read FOnBeginDragNode write FOnBeginDragNode;
    property OnCanFocusNode: TcxTreeListNodeChangingEvent read FOnCanFocusNode write FOnCanFocusNode;
    property OnCanSelectNode: TcxTreeListNodeChangingEvent read FOnCanSelectNode write FOnCanSelectNode;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCollapsed: TcxTreeListNodeChangedEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TcxTreeListNodeChangingEvent read FOnCollapsing write FOnCollapsing;
    property OnColumnHeaderClick: TcxTreeListColumnChangedEvent read FOnColumnHeaderClick write FOnColumnHeaderClick;
    property OnColumnPosChanged: TcxTreeListColumnChangedEvent read FOnColumnPosChanged write FOnColumnPosChanged;
    property OnColumnSizeChanged: TcxTreeListColumnChangedEvent read FOnColumnSizeChanged write FOnColumnSizeChanged;
    property OnCompare: TcxTreeListNodeCompareEvent read FOnCompare write FOnCompare;
    property OnCustomDrawBackgroundCell: TcxTreeListCustomDrawBackgroundCellEvent read FOnCustomDrawBackgroundCell write FOnCustomDrawBackgroundCell;
    property OnCustomDrawBandCell: TcxTreeListCustomDrawBandCellEvent read FOnCustomDrawBandCell write FOnCustomDrawBandCell;
    property OnCustomDrawBandHeaderCell: TcxTreeListCustomDrawHeaderCellEvent read FOnCustomDrawBandHeaderCell write FOnCustomDrawBandHeaderCell;
    property OnCustomDrawDataCell: TcxTreeListCustomDrawEditCellEvent read FOnCustomDrawDataCell write FOnCustomDrawDataCell;
    property OnCustomDrawFooterCell: TcxTreeListCustomDrawFooterCellEvent read FOnCustomDrawFooterCell write FOnCustomDrawFooterCell;
    property OnCustomDrawHeaderCell: TcxTreeListCustomDrawHeaderCellEvent read FOnCustomDrawHeaderCell write FOnCustomDrawHeaderCell;
    property OnCustomDrawIndentCell: TcxTreeListCustomDrawIndentCellEvent read FOnCustomDrawIndentCell write FOnCustomDrawIndentCell;
    property OnCustomDrawIndicatorCell: TcxTreeListCustomDrawIndicatorCellEvent read FOnCustomDrawIndicatorCell write FOnCustomDrawIndicatorCell;
    property OnCustomDrawPreviewCell: TcxTreeListCustomDrawEditCellEvent read FOnCustomDrawPreviewCell write FOnCustomDrawPreviewCell;
    property OnCustomizationFormVisibleChanged: TNotifyEvent read FOnCustomizationVisibleChanged write FOnCustomizationVisibleChanged;
    property OnDeletion: TcxTreeListNodeChangedEvent read FOnDeletion write FOnDeletion;
    property OnExpanded: TcxTreeListNodeChangedEvent read FOnExpanded write FOnExpanded;
    property OnExpanding: TcxTreeListNodeChangingEvent read FOnExpanding write FOnExpanding;
    property OnFilterNode: TcxTreeListFilterNodeEvent read FOnFilterNode write FOnFilterNode;
    property OnGetCellHint: TcxTreeListGetCellHintEvent read FOnGetCellHint write FOnGetCellHint;
    property OnGetDragDropText: TcxTreeListGetDragDropTextEvent read FOnGetDragDropText write FOnGetDragDropText;
    property OnGetLevelImages: TcxTreeListGetLevelImagesEvent read FOnGetLevelImages write FOnGetLevelImages;
    property OnGetNodeHeight: TcxTreeListGetNodeHeightEvent read FOnGetNodeHeight write FOnGetNodeHeight;
    property OnGetNodeImageIndex: TcxTreeListGetNodeImageIndexEvent read FOnGetNodeImageIndex write FOnGetNodeImageIndex;
    property OnGetNodePreviewHeight: TcxTreeListGetNodePreviewHeightEvent read FOnGetNodePreviewHeight write FOnGetNodePreviewHeight;
    property OnHotTrackNode: TcxTreeListHotTrackNodeEvent read FOnHotTrackNode write FOnHotTrackNode;
    property OnIsGroupNode: TcxTreeListIsGroupNodeEvent read FOnIsGroupNode write FOnIsGroupNode;
    property OnLeftPosChanged: TNotifyEvent read FOnLeftPosChanged write FOnLeftPosChanged;
    property OnMoveTo: TcxTreeListMoveToEvent read FOnMoveTo write FOnMoveTo;
    property OnNodeChanged: TcxTreeListNodeDataChangedEvent read FOnNodeChanged write FOnNodeChanged;
    property OnNodeCheckChanged: TcxTreeListNodeCheckChangedEvent read FOnNodeCheckChanged write FOnNodeCheckChanged;
    property OnNodeCheckChanging: TcxTreeListNodeCheckChangingEvent read FOnNodeCheckChanging write FOnNodeCheckChanging;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnSorted: TNotifyEvent read FOnSorted write FOnSorted;
    property OnSorting: TNotifyEvent read FOnSorting write FOnSorting;
    property OnSummary: TcxTreeListSummaryEvent read FOnSummary write FOnSummary;
    property OnTopRecordIndexChanged: TNotifyEvent read FOnTopRecordIndexChanged write FOnTopRecordIndexChanged;
    property NavigatorEvents;
    property PopupMenusEvents: TNotifyEvent read FPopupMenusEvents write FPopupMenusEvents;
    property StylesEvents: TNotifyEvent read FStylesEvents write FStylesEvents;
    //
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnEdited: TcxTreeListColumnChangedEvent read GetOnEdited write SetOnEdited;
    property OnEditing: TcxTreeListEditingEvent read GetOnEditing write SetOnEditing;
    property OnEditValueChanged: TcxTreeListColumnChangedEvent read GetOnEditValueChanged write SetOnEditValueChanged;
    property OnFocusedColumnChanged: TcxTreeListFocusedColumnChanged read FOnFocusedColumnChanged write FOnFocusedColumnChanged;
    property OnFocusedNodeChanged: TcxTreeListFocusedNodeChangedEvent read FOnFocusedNodeChanged write FOnFocusedNodeChanged;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnInitStoredObject: TcxInitStoredObjectEvent read FOnInitStoredObject write FOnInitStoredObject;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;
   //
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AdjustColumnsWidth;
    procedure ApplyBestFit;
    procedure Cancel;
    procedure Clear;
    function CreateColumn(ABand: TcxTreeListBand = nil): TcxTreeListColumn; virtual;
    procedure DeleteAllColumns; virtual;
    procedure DeleteSelection;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Edit;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MakeDefaultLayout; virtual;
    procedure Post;
    procedure FullCollapse;
    procedure FullExpand;
    procedure FullRefresh; virtual;
    function Find(AData: Pointer; AStart: TcxTreeListNode; AExpandedOnly, AForward: Boolean;
     AFilter: TcxTreeListFindFunc; AIgnoreStartNode: Boolean = False): TcxTreeListNode; virtual;
    function FindNodeByText(const AText: string; AColumn: TcxTreeListColumn; AStartNode: TcxTreeListNode = nil;
      AExpandedOnly: Boolean = False; AForward: Boolean = True; ACaseSensitive: Boolean = True; AMode: TcxTreeListFindMode = tlfmNormal;
      ALikeParams: TcxTreeListLikeParams = nil; AIgnoreStartNode: Boolean = False): TcxTreeListNode;
    procedure SelectAll;
    procedure Select(Node: TcxTreeListNode; ShiftState: TShiftState = []); overload; virtual;
    procedure Select(const ANodes: array of TcxTreeListNode); overload; virtual;
    procedure Select(ANodesList: TList); overload; virtual;
    procedure SetFocusedNode(ANode: TcxTreeListNode; AShift: TShiftState);
    function StartDrag(DragObject: TDragObject): Boolean; override;
    procedure Subselect(Node: TcxTreeListNode); virtual;
    procedure Deselect(Node: TcxTreeListNode); virtual;
    procedure ClearSelection(KeepPrimary: Boolean = False); virtual;
    procedure ClearSorting;
    function ColumnByName(const AName: string): TcxTreeListColumn;
    function GetSelections(AList: TList): TcxTreeListNode;
    // navigation
    procedure GotoBOF;
    procedure GotoEOF;
    procedure GotoNext;
    procedure GotoNextPage;
    procedure GotoPrev;
    procedure GotoPrevPage;
    function IsBOF: Boolean;
    function IsEOF: Boolean;
    // edit control
    procedure CancelEdit;
    procedure HideEdit;
    procedure ShowEdit;
    procedure ShowEditByKey(AKey: Char);
    procedure ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
    // storing layout
    procedure RestoreFromIniFile(const AStorageName: string; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreTreeListName: string = '');
    procedure RestoreFromRegistry(const AStorageName: string; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreTreeListName: string = '');
    procedure RestoreFromStream(AStream: TStream; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreTreeListName: string = '');
    procedure StoreToIniFile(AStorageName: string; AReCreate: Boolean = True;
      const ASaveTreeListName: string = '');
    procedure StoreToRegistry(AStorageName: string; AReCreate: Boolean = True;
      const ASaveTreeListName: string = '');
    procedure StoreToStream(AStream: TStream; const ASaveTreeListName: string = '');
    // searching
    function FindNext(AForward: Boolean): Boolean;
    procedure CancelSearching;
    // positions
    function CellRect(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TRect;
    function GetEditRect(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TRect;
    function GetNodeAt(X, Y: Integer): TcxTreeListNode;
    // misc
    procedure CopyAllToClipboard;
    procedure CopySelectedToClipboard(AVisibleOnly: Boolean = False);
    procedure RestoreColumnsDefaults;
    procedure RestoreColumnsWidths;
    //find panel
    procedure ApplyFindFilterText(const AText: string);
    procedure ClearFindFilterText;
    function GetFindFilterText: string;
    procedure HideFindPanel;
    function IsFindPanelVisible: Boolean;
    procedure ShowFindPanel;

    property AbsoluteCount: Integer read GetAbsoluteCount;
    property AbsoluteItems[Index: Integer]: TcxTreeListNode read GetAbsoluteItem write SetAbsoluteItem;
    property AbsoluteVisibleCount: Integer read GetAbsoluteVisibleCount;
    property AbsoluteVisibleItems[Index: Integer]: TcxTreeListNode read GetAbsoluteVisibleItem write SetAbsoluteVisibleItem;
    property Bands: TcxTreeListBands read GetBands write SetBands;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxTreeListColumn read GetColumn write SetColumn;
    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
    property Count: Integer read GetCount;
    property Customizing: TcxTreeListCustomizing read FCustomizing write FCustomizing;
    property DefaultIndentSize: TSize read FDefaultIndentSize;
    property DefaultLayout: Boolean read FDefaultLayout write SetDefaultLayout default False;
    property DefaultRowHeight: Integer read GetDefaultRowHeight write SetDefaultRowHeight default 0;
    property Designers: TList read FDesigners; // for internal use !
    property DragNode: TcxTreeListNode read FDragNode;
    property ExpansionLevel: Integer read FExpansionLevel;
    property Filter: TcxDataFilterCriteria read GetFilter;
    property FindPanel;
    property FocusedColumn: TcxTreeListColumn read GetFocusedColumn write SetFocusedColumn;
    property FocusedNode: TcxTreeListNode read GetFocusedNode write SetFocusedNodeProp;
    property HitTest: TcxTreeListHitTest read GetHitTest;
    property Images: TCustomImageList read GetImages write SetImages;
    property InplaceEditor: TcxCustomEdit read GetInplaceEditor;
    property IsActive: Boolean read GetIsActive;
    property IsEditing: Boolean read GetIsEditing;
    property IsInserting: Boolean read GetIsInserting;
    property Items[Index: Integer]: TcxTreeListNode read GetItem;
    property LastNode: TcxTreeListNode read GetLastNode;
    property LastPartVisibleNode: TcxTreeListNode read GetLastPartVisibleNode;
    property LastVisibleNode: TcxTreeListNode read GetLastVisibleNode write SetLastVisibleNode;
    property Navigator;
    property OptionsBehavior: TcxTreeListOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomizing: TcxTreeListOptionsCustomizing read FOptionsCustomizing write SetOptionsCustomizing;
    property OptionsData: TcxTreeListOptionsData read GetOptionsData write SetOptionsData;
    property OptionsSelection: TcxTreeListOptionsSelection read FOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxTreeListOptionsView read GetOptionsView write SetOptionsView;
    property PopupMenus: TcxTreeListPopupMenus read FPopupMenus write SetPopupMenus;
    property Preview: TcxTreeListPreview read FPreview write SetPreview;
    property Root: TcxTreeListNode read FRoot;
    property Searching: Boolean read GetSearching;
    property SearchingText: string read GetSearchingText write SetSearchingText;
    property SelectionCount: Integer read GetSelectionCount;
    property SelectionList: TList read FSelectionList;
    property Selections[Index: Integer]: TcxTreeListNode read GetSelection;
    property Sorted: Boolean read GetSorted write SetSorted;
    property SortedColumnCount: Integer read GetSortedColumnCount;
    property SortedColumns[Index: Integer]: TcxTreeListColumn read GetSortedColumn;
    property StateImages: TCustomImageList read GetStateImages write SetStateImages;
    property Styles: TcxTreeListStyles read GetStyles write SetStyles;
    property Summary: TcxTreeListSummary read FSummary;
    property TopNode: TcxTreeListNode read GetTopNode;
    property TopVisibleNode: TcxTreeListNode read GetTopVisibleNode write SetTopVisibleNode;
    property VisibleColumnCount: Integer read GetVisibleColumnCount;
    property VisibleColumns[Index: Integer]: TcxTreeListColumn read GetVisibleColumn write SetVisibleColumn;
    property VisibleCount: Integer read GetVisibleCount;
    property BorderSize;
    property LookAndFeel;
    property Color;
  published
    property BufferedPaint stored False;
  end;

  { TcxTreeList }

  TcxTreeList = class(TcxCustomTreeList)
  private
    FDataStream: TMemoryStream;
    function InternalCreateNode(AParent, APrev: TcxTreeListNode;
      var AIndex: Integer): TcxTreeListNode;
  protected
    procedure AssignData(ASource: TcxCustomTreeList); override;
    procedure CorrectHandles;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    function SupportItemsEditor: Boolean; override;
    procedure ReadData(AStream: TStream); virtual;
    procedure ReadStructure(AStream: TStream; AVersion: Integer);
    procedure WriteData(AStream: TStream); virtual;
    procedure WriteStructure(AStream: TStream);
  public
    function Add: TcxTreeListNode; overload;
    function Add(ASibling: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode; overload;
    function AddChild(AParent: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode;
    function AddChildFirst(AParent: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode;
    function AddFirst: TcxTreeListNode; overload;
    function AddFirst(ASibling: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode; overload;
    function AddNode(ANode, ARelative: TcxTreeListNode;
      AData: Pointer; AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode; override;
    function Insert(ASibling: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode;
    function InsertEx(ANode, ASibling: TcxTreeListNode; AData: Pointer = nil): TcxTreeListNode;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);

    property ColumnCount;
    property Columns;
    property Customizing;
    property FocusedColumn;
    property FocusedNode;
    property HitTest;
    property IsEditing;
    property Root;
    property SelectionCount;
    property Selections;
    property Sorted;
    property SortedColumnCount;
    property SortedColumns;
    property TopVisibleNode;
    property VisibleColumnCount;
    property VisibleColumns;
    property VisibleCount;
  published
    property Align;
    property Anchors;
    property Bands;
    property BiDiMode;
    property BorderStyle;
    property Constraints;
    property Cursor;
    property DateTimeHandling;
    property DefaultLayout;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FilterBox;
    property Filtering;
    property FindPanel;
    property Font;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property LookAndFeel;
    property Navigator;
    property OptionsBehavior;
    property OptionsCustomizing;
    property OptionsData;
    property OptionsSelection;
    property OptionsView;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property PopupMenus;
    property Preview;
    property StateImages;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;
    property NavigatorEvents;
    property OnAfterSummary;
    property OnBandHeaderClick;
    property OnBandPosChanged;
    property OnBandSizeChanged;
    property OnBeginDragNode;
    property OnCanFocusNode;
    property OnCanResize;
    property OnCanSelectNode;
    property OnChange;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnHeaderClick;
    property OnColumnPosChanged;
    property OnColumnSizeChanged;
    property OnCompare;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCustomDrawBackgroundCell;
    property OnCustomDrawBandCell;
    property OnCustomDrawBandHeaderCell;
    property OnCustomDrawDataCell;
    property OnCustomDrawFooterCell;
    property OnCustomDrawHeaderCell;
    property OnCustomDrawIndentCell;
    property OnCustomDrawIndicatorCell;
    property OnCustomDrawPreviewCell;
    property OnCustomizationFormVisibleChanged;
    property OnDataChanged;
    property OnDblClick;
    property OnDeletion;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFilterControlDialogShow;
    property OnFilterCustomization;
    property OnFilterDialogShow;
    property OnFilterNode;
    property OnFindPanelVisibilityChanged;
    property OnFocusedColumnChanged;
    property OnFocusedNodeChanged;
    property OnGetCellHint;
    property OnGetDragDropText;
    property OnGetLevelImages;
    property OnGetNodeHeight;
    property OnGetNodeImageIndex;
    property OnGetNodePreviewHeight;
    property OnGetSiteInfo;
    property OnGetStoredProperties;
    property OnGetStoredPropertyValue;
    property OnHotTrackNode;
    property OnInitEdit;
    property OnInitEditValue;
    property OnInitFilteringDateRanges;
    property OnInitStoredObject;
    property OnIsGroupNode;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftPosChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMoveTo;
    property OnNodeChanged;
    property OnNodeCheckChanged;
    property OnNodeCheckChanging;
    property OnResize;
    property OnSelectionChanged;
    property OnSetStoredPropertyValue;
    property OnSorted;
    property OnSorting;
    property OnStartDock;
    property OnStartDrag;
    property OnSummary;
    property OnTopRecordIndexChanged;
    property OnUnDock;
    property PopupMenusEvents;
    property StylesEvents;
  end;

procedure cxTreeListError(const Description: string);
function cxCompareNodesByLevel(ANode1, ANode2: TcxTreeListNode): Integer;

var
  cxTreeListBuiltInMenuClass: TcxTreeListCustomBuiltInMenuClass = TcxTreeListBuiltInMenu;
  DesignerNavigatorProc: procedure(AListener: TObject; AddListener: Boolean) of object;

implementation

uses
  Types, CommCtrl, RTLConsts, cxLike, cxFormats, dxThreading, dxDPIAwareUtils, cxFindPanel, cxFilterControlDialog,
  cxLibraryStrs, dxFilterBox;

{$R cxTL.res}

type
  TcxCustomEditViewInfoAccess = class(TcxCustomEditViewInfo);
  TcxCheckBoxPropertiesAccess = class(TcxCheckBoxProperties);
  TcxControlAccess = class(TcxControl);
  TdxCustomCheckListBoxAccess = class(TdxCustomCheckListBox);
  TdxQuickCustomizationCustomControlAccess = class(TdxQuickCustomizationCustomControl);

{$UNDEF CXTEST}

const
  Weights: array[TcxTreeListBandFixedKind] of Integer = (0, -1, 1);

  cxTreeListVersion = $00050000;
  SelectionChanges = [tcSelection, tcFocusedNode];

  cxInvalidIndex   =    -1;
  cxMinCapacityInc =    32;
  cxMaxCapacityInc = 32768;

  cxDefaultImageIndexes: TcxTreeListNodeImageIndexes = (0, 0, -1, -1, -1);

  // internal column state

  tlcsVisible  = $08;

  // internal band state
  tlbsVisible    = $00;
  tlbsFixedLeft  = $02;
  tlbsFixedRight = $04;
  tlbsAllFixed   = tlbsFixedRight or tlbsFixedLeft;

  ColumnPropertiesName: array[0..10] of string =
    ('Visible', 'Index', 'LineCount', 'ColIndex', 'RowIndex',
     'BandIndex', 'Caption', 'SortOrder', 'SortIndex', 'RealMinWidth', 'RealWidth');
  BandPropertiesName: array[0..6] of string =
    ('Caption', 'RealMinWidth', 'RealWidth', 'Visible', 'Index', 'BandIndex', 'ColIndex');

  GridLineBorders: array[TcxTreeListGridLines] of TcxBorders =
    ([], [bTop, bBottom], [bLeft, bRight], cxBordersAll);

  OddEvenStyleIndexes: array[Boolean] of Integer =(tlsv_ContentEven, tlsv_ContentOdd);

  GlyphIndents = [nikImage, nikState, nikCheck];


type
  TAllowExpandChangingFunc = function: Boolean of object;

  TcxDefColorFunc = function: TColor of object;

  TcxTreeListStreamHeader = packed record
    Minor, Major: Word;
    Size: Integer;
  end;

  PcxTreeListValueDefData = ^TcxTreeListValueDefData;
  TcxTreeListValueDefData = packed record
    Index: Integer;
    ValueDef: TcxValueDef;
  end;

  { TcxFakeCellViewInfo }

  TcxFakeCellViewInfo = class(TcxTreeListEditCellViewInfo)
  private
    function GetTreeListViewInfo: TcxTreeListViewInfo;
  protected
    FCalcHeight: Boolean;
    FColumn: TcxTreeListColumn;
    function GetButtonTransparency: TcxEditButtonTransparency; override;
    function GetControl: TcxEditingControl; override;
    function GetEditContainer: TcxCustomInplaceEditContainer; override;
    function GetFocused: Boolean; override;
    function GetSelected: Boolean; override;
    procedure Initialize(ANode: TcxTreeListNode; AHeight, AWidth: Integer; ACalcHeight: Boolean);
    function IsAutoHeight: Boolean; override;
    function IsEndEllipsis: Boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    function MeasureHeight(AColumn: TcxTreeListColumn;
      ANode: TcxTreeListNode; AIsCategory: Boolean): Integer;
    function MeasureWidth(AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Integer;

    property TreeListViewInfo: TcxTreeListViewInfo read GetTreeListViewInfo;
  end;

  { TcxTreeListCellPos }

  TcxTreeListCellPos = class
  public
    Item: TObject;
    Node: TcxTreeListNode;
    constructor Create(ANode: TcxTreeListNode; AItem: TObject);
  end;

function cxFind(ATreeList: TcxCustomTreeList; AData: Pointer;
  AStart: TcxTreeListNode; AExpandedOnly, AForward: Boolean;
  AFilter: TcxTreeListFindFunc; AIgnoreStartNode: Boolean): TcxTreeListNode;

  function LoadChildren(ANode: TcxTreeListNode): Boolean;
  begin
    with ANode do
    begin
      if (FCount = 0) and HasChildren and not Expanded and not AExpandedOnly then
        ANode.LoadChildren;
      Result := (FCount <> 0) and (Expanded or not AExpandedOnly);
    end;
  end;

  function GetNext(ANode: TcxTreeListNode): TcxTreeListNode;
  begin
    Result := ANode;
    if not LoadChildren(Result) then
      while Result <> nil do
      begin
        if Result.FNext <> nil then
        begin
          Result := Result.FNext;
          Break;
        end;
        while (Result <> nil) and (Result.FNext = nil) do
          Result := Result.Parent;
      end
    else
      Result := Result.FFirst;
  end;

  function GetPrev(ANode: TcxTreeListNode): TcxTreeListNode;
  begin
    Result := ANode;
    if Result.FPrev <> nil then
    begin
      Result := Result.FPrev;
      while LoadChildren(Result) do Result := Result.FLast;
    end
    else
    begin
      Result := Result.Parent;
      if Result = ATreeList.Root then
        Result := nil;
    end;
  end;

  function GetAnother(ANode: TcxTreeListNode): TcxTreeListNode;
  begin
    if AForward then
      Result := GetNext(ANode)
    else
      Result := GetPrev(ANode);
  end;

var
  ACurrent: TcxTreeListNode;
begin
  Result := nil;
  ACurrent := AStart;
  if ACurrent = nil then
    ACurrent := ATreeList.Root.FFirst;
  ATreeList.BeginUpdate;
  try
    while (ACurrent <> nil) and (Result = nil) do
    begin
      if AFilter(ACurrent, AData) then
      begin
        Result := ACurrent;
        if AIgnoreStartNode and (Result = AStart) then
        begin
          Result := nil;
          ACurrent := GetAnother(ACurrent);
        end;
      end
      else
        ACurrent := GetAnother(ACurrent);
    end;
  finally
    if (ATreeList.Changes = []) or ATreeList.ViewInfo.CalculateInProcess then
      ATreeList.CancelUpdate
    else
      ATreeList.EndUpdate;
  end;
end;

function cxGetLatest(ANode: TcxTreeListNode;
  ACheckExpanded: Boolean = True): TcxTreeListNode;
begin
  Result := ANode;
  if Result <> nil then
  begin
    if ACheckExpanded then
      while (Result.Count > 0) and not (nsCollapsed in Result.State) do
        Result := Result.FLast
    else
      while Result.HasChildren do
      begin
        Result.LoadChildren;
        if Result.Count > 0 then
          Result := Result.FLast
        else
          Break;
      end;
  end;
end;

function cxExcludeBorders(const ARect: TRect; ABorders: TcxBorders): TRect;
begin
  Result := ARect;
  with Result do
  begin
    Inc(Left, Byte(bLeft in ABorders));
    Inc(Top, Byte(bTop in ABorders));
    Dec(Right, Byte(bRight in ABorders));
    Dec(Bottom, Byte(bBottom in ABorders));
  end;
end;

function cxGetNativeColor(AColor: TColor; ADefColorFunc: TcxDefColorFunc): TColor;
begin
  if AColor = clDefault then
    Result := ADefColorFunc
  else
    Result := AColor;
end;

function cxCompareBandsByPosition(
  ABand1, ABand2: TcxTreeListBand): Integer;
begin
  Result := ABand1.FIndex - ABand2.FIndex;
end;

function cxCompareBandsByActualPositionEx(
  ABand1, ABand2: TcxTreeListBand): Integer;

  function IsBandInheritedFromParent(ABand, AParentBand: TcxTreeListBand): Boolean;
  begin
    Result := False;
    while not Result and (ABand <> nil) do
    begin
      Result := ABand.Position.ParentBand = AParentBand;
      ABand := ABand.Position.ParentBand;
    end;
  end;

  function GetParentBandInheritedFromDestination(ABand, ADestinationBand: TcxTreeListBand): TcxTreeListBand;
  begin
    Result := ABand;
    while (Result.Position.ParentBand <> nil) and (Result.Position.ParentBand <> ADestinationBand) do
      Result := Result.Position.ParentBand;
  end;

var
  AParentBand1, AParentBand2: TcxTreeListBand;
begin
  Result := 0;
  if ABand1 = ABand2 then
    Exit;
  if IsBandInheritedFromParent(ABand1, ABand2) then
    Result := 1
  else
    if IsBandInheritedFromParent(ABand2, ABand1) then
      Result := -1;
  if Result = 0 then
  begin
    AParentBand1 := nil;
    AParentBand2 := nil;
    repeat
      AParentBand1 := GetParentBandInheritedFromDestination(ABand1, AParentBand1);
      AParentBand2 := GetParentBandInheritedFromDestination(ABand2, AParentBand2);
      if AParentBand1 <> AParentBand2 then
      begin
        Result := AParentBand1.Position.ColIndex - AParentBand2.Position.ColIndex;
        if Result = 0 then
          Result := AParentBand1.Index - AParentBand2.Index;
      end;
    until (Result <> 0) or ((AParentBand1 = ABand1) and (AParentBand2 = ABand2));
  end;
end;

function cxCompareBandsByPositionEx(
  ABand1, ABand2: TcxTreeListBand): Integer;
begin
  Result := ABand1.Position.FBandIndex - ABand2.Position.FBandIndex;
  if Result = 0 then
    Result := ABand1.Position.FColIndex - ABand2.Position.FColIndex;
  if Result = 0 then
    Result := ABand1.Index - ABand2.Index;
end;

function cxCompareColumnsByActualPosition(
  AColumn1, AColumn2: TcxTreeListColumn): Integer;
begin
  Result := 0;
  if AColumn1 = AColumn2 then
    Exit;
  if AColumn1.Position.Band <> nil then
    Result := cxCompareBandsByActualPositionEx(AColumn1.Position.Band, AColumn2.Position.Band);
  if Result = 0 then
    Result := AColumn1.Position.RowIndex - AColumn2.Position.RowIndex;
  if Result = 0 then
    Result := AColumn1.Position.ColIndex - AColumn2.Position.ColIndex;
  if Result = 0 then
    Result := AColumn1.ItemIndex - AColumn2.ItemIndex;
end;

function cxCompareColumnsByPosition(
  AColumn1, AColumn2: TcxTreeListColumn): Integer;
begin
  Result := AColumn1.Position.FBandIndex - AColumn2.Position.FBandIndex;
  if Result = 0 then
    Result := AColumn1.Position.FRowIndex - AColumn2.Position.FRowIndex;
  if Result = 0 then
    Result := AColumn1.Position.FColIndex - AColumn2.Position.FColIndex;
  if Result = 0 then
    Result := AColumn1.ItemIndex - AColumn2.ItemIndex;
end;

function cxCompareColumnsBySortIndex(
  AColumn1, AColumn2: TcxTreeListColumn): Integer;
begin
  if (AColumn2.SortIndex < 0) and (AColumn1.SortIndex >= 0) then
    Result := -1
  else
    Result := AColumn1.SortIndex - AColumn2.SortIndex;
  if Result = 0 then
    Result := Byte(AColumn1.ActuallyVisible) - Byte(AColumn2.ActuallyVisible);
  if Result = 0 then
    Result := cxCompareColumnsByPosition(AColumn1, AColumn2);
end;

function cxCompareVisibleBands(AItem1, AItem2: Pointer): Integer;
begin
  with TcxTreeListBand(AItem1) do
  begin
    Result := Weights[FixedKind] - Weights[TcxTreeListBand(AItem2).FixedKind];
    if Result = 0 then
      Result := Index - TcxTreeListBand(AItem2).Index;
  end;
end;

function cxCompareBands(ABand1, ABand2: TcxTreeListBand): Integer;
const
  Weight: array[TcxTreeListBandFixedKind] of Integer = (0, -1, 1);
begin
  Result := Weight[ABand1.FixedKind] - Weight[ABand2.FixedKind];
  if Result = 0 then
    Result := ABand1.Index - ABand2.Index;
end;

function cxCompareNodes(ANode1, ANode2: TcxTreeListNode): Integer;
begin
  ANode1.TreeList.DoCompare(ANode1, ANode2, Result);
end;

function cxCompareNodesByAbsoluteIndex(
  ANode1, ANode2: TcxTreeListNode): Integer;
begin
  Result := dxCompareValues(ANode1.AbsoluteIndex, ANode2.AbsoluteIndex);
end;

function cxCompareNodesByRecordIndex(
  ANode1, ANode2: TcxTreeListNode): Integer;
begin
  Result := dxCompareValues(ANode2.Handle, ANode1.Handle);
end;

function cxCompareNodesByLevel(
  ANode1, ANode2: TcxTreeListNode): Integer;
begin
  Result := ANode2.Level - ANode1.Level;
end;


function cxCompareCells(ACell1, ACell2: TcxCustomViewInfoItem): Integer;
begin
  Result := ACell1.CustomDrawID - ACell2.CustomDrawID;
  if Result = 0 then
  begin                                               ;
    Result := ACell1.BoundsRect.Top - ACell2.BoundsRect.Top;
    if Result = 0 then
      Result := ACell1.BoundsRect.Left - ACell2.BoundsRect.Left;
  end;
end;

function cxCompareHitTestCells(ACell1, ACell2: TcxTreeListHitTestArea): Integer;
begin
  if ACell1.Link = ACell2.Link then
    Result := ACell1.Area.Top - ACell2.Area.Top
  else
    Result := cxCompareCells(ACell1.Link, ACell2.Link);
end;

procedure cxValidateSingleLine(APos: TcxTreeListColumnPosition);
begin
  APos.FRowIndex := 0;
  APos.FLineCount := 1;
end;

procedure cxTreeListError(const Description: string);
begin
  raise EcxTreeList.Create(Description);
end;

procedure cxTLUnused;
begin
  cxTreeListError('TreeList unused');
end;

procedure cxError(Condition: Boolean;
  const Description: string; Args: array of const); overload;
begin
  if Condition then
    raise Exception.CreateFmt(Description, Args);
end;

{ TcxTreeListBandOptions }

constructor TcxTreeListBandOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  RestoreDefaults;
end;

procedure TcxTreeListBandOptions.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListBandOptions then
  begin
    FCustomizing := TcxTreeListBandOptions(Source).FCustomizing;
    FHidden := TcxTreeListBandOptions(Source).FHidden;
    FMoving := TcxTreeListBandOptions(Source).FMoving;
    FOnlyOwnColumns := TcxTreeListBandOptions(Source).FOnlyOwnColumns;
    FSizing := TcxTreeListBandOptions(Source).FSizing;
    FVertSizing := TcxTreeListBandOptions(Source).VertSizing;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListBandOptions.RestoreDefaults;
begin
  FCustomizing := True;
  FHidden := False;
  FMoving := True;
  FOnlyOwnColumns := False;
  FSizing := True;
  FVertSizing := True;
  Changed;
end;

procedure TcxTreeListBandOptions.Changed;
begin
  TcxTreeListBand(GetOwner).LayoutChanged;
end;

procedure TcxTreeListBandOptions.SetSizing(AValue: Boolean);
begin
  if AValue <> FSizing then
  begin
    FSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListBandOptions.SetVertSizing(AValue: Boolean);
begin
  if AValue <> FVertSizing then
  begin
    FVertSizing := AValue;
    Changed;
  end;
end;


{ TcxTreeListBandStyles }

constructor TcxTreeListBandStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
end;

procedure TcxTreeListBandStyles.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListBandStyles then
  begin
    Content := TcxTreeListBandStyles(Source).Content;
    Footer := TcxTreeListBandStyles(Source).Footer;
    Header := TcxTreeListBandStyles(Source).Header;
    HeaderBackground := TcxTreeListBandStyles(Source).HeaderBackground;
  end;
  inherited Assign(Source);
end;

function TcxTreeListBandStyles.GetContentParams(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
begin
  if AColumn <> nil then
    Result := AColumn.Styles.GetContentParams(ANode)
  else
    DoGetContentParams(ANode, AColumn, Result);
end;

function TcxTreeListBandStyles.GetBand: TcxTreeListBand;
begin
  Result := TcxTreeListBand(GetOwner);
end;

function TcxTreeListBandStyles.GetTreeList: TcxCustomTreeList;
begin
  Result := Band.TreeList;
end;

procedure TcxTreeListBandStyles.Changed(AIndex: Integer);
begin
  inherited Changed(AIndex);
  TreeList.UpdateViewStyles;
end;

procedure TcxTreeListBandStyles.DoGetContentParams(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AParams: TcxViewParams);
var
  ACellPos: TcxTreeListCellPos;
begin
  ACellPos := TcxTreeListCellPos.Create(ANode, AColumn);
  try
    GetViewParams(tlbs_Content, ACellPos, nil, AParams);
  finally
    ACellPos.Free;
  end;
end;

procedure TcxTreeListBandStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    tlbs_Content:
      if AData is TcxTreeListCellPos then
      begin
        with TcxTreeListCellPos(AData) do
        begin
          if Item <> nil then
            TreeList.Styles.DoGetContentParams(Node, TcxTreeListColumn(Item), AParams)
          else
            TreeList.Styles.DoGetBandContentParams(Band, Node, AParams);
        end;
      end
      else
        TreeList.Styles.DoGetBandContentParams(Band, nil, AParams);
    tlbs_Footer:
      if AData is TcxTreeListCellPos then
        TreeList.Styles.GetViewParams(tlsv_ColumnFooter, TcxTreeListCellPos(AData).Item, nil, AParams)
      else
        TreeList.Styles.DoGetBandFooterParams(Band, TcxTreeListNode(AData), AParams);
    tlbs_Header:
      TreeList.Styles.DoGetBandHeaderParams(Band, AParams);
    tlbs_HeaderBackground:
      TreeList.Styles.DoGetBandBackgroundParams(Band, AParams);
  end;
end;

{ TcxTreeListBandPosition }

constructor TcxTreeListBandPosition.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBandIndex := -1;
  FColIndex := -1;
end;

procedure TcxTreeListBandPosition.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListBandPosition then
  begin
    FBandIndex := TcxTreeListBandPosition(Source).BandIndex;
    FColIndex :=  TcxTreeListBandPosition(Source).ColIndex
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListBandPosition.Changed;
begin
  Band.LayoutChanged;
end;

function TcxTreeListBandPosition.CheckBandIndex(var AIndex: Integer): Boolean;
var
  AColIndex, I: Integer;
  ABand: TcxTreeListBand;
begin
  Result := Band.Index <> AIndex;
  if TreeList.IsDestroying or not Result then Exit;
  Result := (AIndex = -1) or ((AIndex >= 0) and (AIndex < TreeList.Bands.Count));
  if not Result or (AIndex = - 1) then Exit;
  ABand := TreeList.Bands[AIndex];
  if not ABand.HasAsParent(Band) then Exit;
  AColIndex := ColIndex;
  TreeList.BeginUpdate;
  try
    for I := Band.ChildBandCount - 1 downto 0 do
      with Band.ChildBands[I].Position do
      begin
        BandIndex := Self.BandIndex;
        ColIndex := AColIndex;
      end;
    AIndex := ABand.Index;
  finally
    TreeList.EndUpdate;
  end;
end;

function TcxTreeListBandPosition.IsPositionChanged: Boolean;
begin
  Result := (BandIndex <> FBandIndex) or (ColIndex <> FColIndex);
end;

procedure TcxTreeListBandPosition.Restore;
begin
  BandIndex := FBandIndex;
  if TreeList.IsRestoring or (not Band.IsRoot) then
    ColIndex := FColIndex;
end;

procedure TcxTreeListBandPosition.Store;
begin
  FBandIndex := BandIndex;
  FColIndex :=  ColIndex;
end;

function TcxTreeListBandPosition.GetBandIndex: Integer;
begin
  if ParentBand = nil then
    Result := -1
  else
    Result := ParentBand.Index;
end;

function TcxTreeListBandPosition.GetColIndex: Integer;
begin
  if ParentBand = nil then
    Result := Band.RootIndex
  else
    Result := ParentBand.IndexOf(Band);
end;

function TcxTreeListBandPosition.GetBand: TcxTreeListBand;
begin
  Result := TcxTreeListBand(GetOwner);
end;

function TcxTreeListBandPosition.GetTreeList: TcxCustomTreeList;
begin
  Result := Band.TreeList;
end;

function TcxTreeListBandPosition.GetVisibleColIndex: Integer;
begin
  if ParentBand = nil then
    Result := Band.Bands.FVisibleRootItems.IndexOf(Self)
  else
    Result := ParentBand.FChildVisibleBands.IndexOf(Band);
end;

procedure TcxTreeListBandPosition.SetBandIndex(AValue: Integer);
begin
  if IsLocked or (TreeList.IsRestoring and not Band.FIsDestroying) then
  begin
    FBandIndex := AValue;
    Exit;
  end;
  if not CheckBandIndex(AValue) or (BandIndex = AValue) then Exit;
  TreeList.BeginUpdate;
  try
    if ParentBand <> nil then
      ParentBand.RemoveBand(Band);
    if AValue >= 0 then
    begin
      FParentBand := TreeList.Bands[AValue];
      FParentBand.AddBand(Band);
    end
    else
      FParentBand := nil;
    Band.LayoutChanged;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBandPosition.SetColIndex(AValue: Integer);
begin
  if IsLocked or TreeList.IsRestoring then
    FColIndex := AValue
  else
    if ColIndex <> AValue then
    begin
      if ParentBand = nil then
        Band.RootIndex := AValue
      else
        ParentBand.MoveBand(Band, AValue);
    end;
end;

function TcxTreeListBandPosition.IsColIndexStored: Boolean;
begin
  Result := not Band.IsRoot;
end;

function TcxTreeListBandPosition.IsLocked: Boolean;
begin
  Result := TreeList.IsDestroying or Band.IsLoading or TreeList.IsUpdating;
end;

{ TcxTreeListBand }

constructor TcxTreeListBand.Create(Collection: TCollection);
begin
  TcxTreeListBands(Collection).TreeList.BeginUpdate;
  inherited Create(Collection);
  FChildBands := TList.Create;
  FChildVisibleBands := TList.Create;
  FVisibleColumns := TList.Create;
  FBandRows := TcxTreeListBandRows.Create(Self);
  FColumns := TList.Create;
  FPosition := TcxTreeListBandPosition.Create(Self);
  FCaption := TcxTreeListCaption.Create(Self);
  FCaption.OnChange := ChangeCaption;
  FStyles := GetStyleClass.Create(Self);
  FOptions := GetOptionsClass.Create(Self);
  Visible := True;
  RestoreWidths;
  TcxTreeListBands(Collection).TreeList.EndUpdate;
end;

destructor TcxTreeListBand.Destroy;
var
  ATreeList: TcxCustomTreeList;
begin
  ATreeList := TreeList;
  ATreeList.BeginUpdate;
  if FHeaderCell <> nil then
    FHeaderCell.FItem := nil;
  TreeList.Controller.UnselectObject(Self);
  Position.BandIndex := -1;
  RemoveChildBands;
  RemoveColumns;
  FreeAndNil(FChildVisibleBands);
  FreeAndNil(FCaption);
  FreeAndNil(FColumns);
  FreeAndNil(FOptions);
  FreeAndNil(FPosition);
  FreeAndNil(FStyles);
  FreeAndNil(FBandRows);
  FreeAndNil(FVisibleColumns);
  FreeAndNil(FChildBands);
  inherited Destroy;
  ATreeList.EndUpdate;
end;

procedure TcxTreeListBand.Assign(Source: TPersistent);
var
  ABand: TcxTreeListBand;
begin
  if Source is TcxTreeListBand then
  begin
    ABand := TcxTreeListBand(Source);
    FCalculatedWidth := ABand.FCalculatedWidth;
    FMinWidth := ABand.MinWidth;
    FWidth := ABand.FWidth;
    Position := ABand.Position;
    Options := ABand.Options;
    Styles := ABand.Styles;
    Caption := ABand.Caption;
    FixedKind := ABand.FixedKind;
    FExpandable := ABand.Expandable;
    FTag := ABand.Tag;
    Visible := ABand.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListBand.ApplyBestFit;
var
  AColIndex: Integer;
begin
  TreeList.ValidateStates;
  TreeList.BeginUpdate;
  try
    TreeList.HideEdit;
    FWidth := 0;
    if VisibleColumnCount > 0 then
    begin
      for AColIndex := 0 to VisibleColumnCount - 1 do
        VisibleColumns[AColIndex].ApplyBestFit;
      BandRows.Refresh;
      FCalculatedWidth := BandRows.RowMaxWidth;
    end
    else
      FCalculatedWidth := FMinWidth;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBand.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FIsDestroying := True;
end;

function TcxTreeListBand.IndexOf(AChildBand: TcxTreeListBand): Integer;
begin
  Result := FChildBands.IndexOf(AChildBand);
end;

function TcxTreeListBand.HasAsParent(ABand: TcxTreeListBand): Boolean;
var
  AParent: TcxTreeListBand;
begin
  Result := False;
  AParent := Self;
  while not Result and (AParent <> nil) do
  begin
    AParent := AParent.ParentBand;
    Result := AParent = ABand;
  end;
end;

procedure TcxTreeListBand.MoveBand(ABand: TcxTreeListBand; AColIndex: Integer);
begin
  ABand.Position.BandIndex := Index;
  AColIndex := Min(Max(0, AColIndex), ChildBandCount - 1);
  FChildBands.Move(ABand.Position.ColIndex, AColIndex);
  LayoutChanged;
end;

procedure TcxTreeListBand.RestoreDefaults;
begin
  TreeList.BeginUpdate;
  try
    Options.RestoreDefaults;
    RestoreWidths;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBand.RestoreWidths;
begin
  FMinWidth := cxTreeListDefMinWidth;
  SetWidth(0);
end;

function TcxTreeListBand.ActualMinWidth: Integer;
var
  I, ASum: Integer;
begin
  Result := Max(MinWidth, BandRows.RowMinWidth);
  ASum := 0;
  for I := 0 to ChildVisibleBands.Count - 1 do
    Inc(ASum, TcxTreeListBand(ChildVisibleBands[I]).ActualMinWidth);
  Result := Max(Result, ASum);
end;

procedure TcxTreeListBand.AdjustSubItems;
var
  I: Integer;
begin
  BandRows.Refresh;
  if VisibleColumnCount > 0 then
  begin
    for I := 0 to BandRows.VisibleItemCount - 1 do
      BandRows.VisibleItems[I].AdjustColumns;
  end
  else
    Bands.Adjust(FChildVisibleBands, CalculatedWidth);
end;


procedure TcxTreeListBand.AssignChildBandWidths;
var
  I: Integer;
begin
  for I := 0 to ChildVisibleBands.Count - 1 do
    TcxTreeListBand(ChildVisibleBands[I]).AssignWidth;
end;

procedure TcxTreeListBand.AssignColumnsWidth;
var
  I: Integer;
begin
  for I := 0 to BandRows.Count - 1 do
    BandRows[I].AssignColumnsWidth;
end;

procedure TcxTreeListBand.AssignWidth;
begin
  if ActuallyVisible then
    FWidth := FCalculatedWidth;
end;

procedure TcxTreeListBand.InitAutoWidthItem(AItem: TcxAutoWidthItem);
begin
  AItem.Width := Width;
  if AItem.Width = 0 then
    AItem.Width := CalculatedWidth;
  AItem.Fixed := IsWidthFixed;
  AItem.MinWidth := Max(FMinWidth, BandRows.RowMinWidth);
  AItem.Width := Max(AItem.Width, AItem.MinWidth + IndentWidth);
  if AItem.Fixed then
    AItem.MinWidth := Max(AItem.MinWidth, AItem.Width);
  AItem.AutoWidth := -1;
end;

function TcxTreeListBand.IsBandFixedDuringSizing: Boolean;
var
  AForcingBand: TcxTreeListBand;
begin
  AForcingBand := TreeList.Controller.ForcingWidthBand;
  Result := (AForcingBand <> nil) and ((Self = AForcingBand) or
    not AForcingBand.IsLastAsChild and (ParentBand = AForcingBand.ParentBand) and
    (Position.VisibleColIndex < AForcingBand.Position.VisibleColIndex));
end;

function TcxTreeListBand.IsOnlyOwnColumns: Boolean;
begin
  Result := Options.OnlyOwnColumns;
end;

function TcxTreeListBand.IsVisibleInQuickCustomizationPopup: Boolean;
begin
  Result := Options.Customizing and not Options.Hidden and
    ((ParentBand = nil) or ParentBand.ActuallyVisible);
end;

procedure TcxTreeListBand.CalculateLineCount(ACurrentLine: Integer);
begin
  FLineCount := ACurrentLine - Level;
  if ParentBand <> nil then
    ParentBand.CalculateLineCount(Level);
  Refresh;
end;

function TcxTreeListBand.CanDropColumnAt(
  const APoint: TPoint; out ARowIndex, AColIndex: Integer): Boolean;
begin
  Result := True;
  ARowIndex := 0;
  AColIndex := 0;
end;

procedure TcxTreeListBand.ChangeScale(M, D: Integer);
var
  ASavedWidth: Integer;
begin
  ASavedWidth := Width;
  MinWidth := MulDiv(MinWidth, M, D);
  Width := MulDiv(ASavedWidth, M, D);
end;

procedure TcxTreeListBand.CheckExpandable(var ABand: TcxTreeListBand);
begin
  if Expandable = tlbeNotExpandable then Exit;
  if (ABand = nil) or not (ABand.Expandable = tlbeExpandable) and (Expandable <> tlbeDefault) then
    ABand := Self;
end;

procedure TcxTreeListBand.ForceWidth(AValue: Integer);

  procedure AssignBandWidths;
  var
    ABand: TcxTreeListBand;
  begin
    if ParentBandWidthAssigned <> nil then
      ParentBand.AssignChildBandWidths;
    if not TreeList.OptionsView.ColumnAutoWidth then Exit;
    ABand := ParentBand;
    while ABand <> nil do
    begin
      ABand.AssignChildBandWidths;
      ABand := ABand.ParentBand;
    end;
    Bands.AssignRootItemWidths;
  end;

begin
  TreeList.BeginUpdate;
  try
    AssignBandWidths;
    TreeList.Controller.ForcingWidthBand := Self;
    try
      Width := AValue;
      Bands.Adjust();
      AssignBandWidths;
    finally
      TreeList.Controller.ForcingWidthBand := nil;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

function TcxTreeListBand.GetMaxDeltaWidth: Integer;
begin
  Result := TreeList.GetMaxBandWidth(Self) - DisplayWidth;
end;

function TcxTreeListBand.CanMoving: Boolean;
begin
  Result := Options.Moving and TreeList.OptionsCustomizing.BandMoving;
end;

procedure TcxTreeListBand.ChangeCaption(Sender: TObject);
begin
  TreeList.LayoutChanged;
end;

function TcxTreeListBand.GetOptionsClass: TcxTreeListBandOptionsClass;
begin
  Result := TcxTreeListBandOptions;
end;

function TcxTreeListBand.GetStyleClass: TcxTreeListBandStylesClass;
begin
  Result := TcxTreeListBandStyles;
end;

procedure TcxTreeListBand.MoveColumnsTo(ABand: TcxTreeListBand);
var
  I, J: Integer;
  AColumn: TcxTreeListColumn;
begin
  if ABand = Self then Exit;
  for I := 0 to BandRows.Count - 1 do
    for J := 0 to BandRows[0].Count - 1 do
    begin
      AColumn := BandRows[0][0];
      ABand.AddColumn(AColumn);
      AColumn.Position.RowIndex := I;
      AColumn.Position.ColIndex := J;
    end;
  FColumns.Clear;
  BandRows.FItems.Clear;
  Refresh;
  ABand.Refresh;
end;

procedure TcxTreeListBand.LayoutChanged;
begin
  if Bands = nil then Exit;
  if TreeList.IsLocked then
    Bands.RefreshInformation
  else
    Bands.Update(nil);
end;

procedure TcxTreeListBand.MoveBandsToRoot;
var
  AColIndex, I: Integer;
begin
  if RootParentBand = nil then
    AColIndex := FPosition.ColIndex + 1
  else
    AColIndex := RootParentBand.Position.ColIndex + 1;
  TreeList.BeginUpdate;
  try
    for I := ChildBandCount - 1 downto 0 do
      with ChildBands[I].Position do
      begin
        BandIndex := -1;
        ColIndex := AColIndex;
      end;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBand.AddBand(ABand: TcxTreeListBand);
begin
  FChildBands.Add(ABand);
  ABand.FixedKind := FixedKind;
  MoveColumnsTo(FirstChildBottomBand);
  Refresh;
end;

procedure TcxTreeListBand.AddColumn(AColumn: TcxTreeListColumn);
begin
  FColumns.Add(AColumn);
  with AColumn.Position do
  begin
    FBand := Self;
    Row := BandRows.CheckRowIndex(0)
  end;
end;

procedure TcxTreeListBand.ColumnSizeChanged(AColumn: TcxTreeListColumn);
begin
  FCalculatedWidth := 0;
  AdjustSubItems;
  TreeList.DoColumnSizeChanged(AColumn);
end;

procedure TcxTreeListBand.DeleteColumn(AColumn: TcxTreeListColumn);
var
  AIndex: Integer;
begin
  FColumns.Remove(AColumn);
  with AColumn.Position do
  begin
    AIndex := RowIndex;
    Row.FItems.Remove(AColumn);
    FRow := nil;
  end;
  BandRows.CheckRowEmpty(AIndex);
end;

procedure TcxTreeListBand.Refresh;
var
  I: Integer;
begin
  if Bands.IsUpdateLocked then Exit;
  FVisibleColumns.Clear;
  FChildVisibleBands.Clear;
  BandRows.Refresh;
  if VisibleColumnCount > 0 then
    TreeList.FVisibleColumns.Assign(FVisibleColumns, laOr);
  for I := 0 to ChildBandCount - 1 do
    if ChildBands[I].Visible then
      FChildVisibleBands.Add(ChildBands[I]);
end;

procedure TcxTreeListBand.RemoveBand(ABand: TcxTreeListBand);
begin
  FChildBands.Remove(ABand);
  Refresh;
end;

procedure TcxTreeListBand.RemoveChildBands;
begin
  if (Bands = nil) or TreeList.IsUpdating or TreeList.IsDestroying then Exit;
  while ChildBandCount > 0 do
  begin
    ChildBands[0].Visible := False;
    ChildBands[0].Position.BandIndex := -1;
  end;
end;

procedure TcxTreeListBand.RemoveColumns;
var
  I: Integer;
begin
  if TreeList.IsDestroying or (FColumns = nil) then Exit;
  TreeList.BeginUpdate;
  try
    for I := ColumnCount - 1 downto 0 do
      Columns[I].Position.BandIndex := -1;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBand.SetIndex(Value: Integer);
var
  APrevIndex: Integer;
begin
  APrevIndex := Index;
  inherited SetIndex(Value);
  if Index <> APrevIndex then
    LayoutChanged;
end;

function TcxTreeListBand.CanSizing(ADirection: TcxDragSizingDirection): Boolean;
begin
  if ADirection = dsdVert then
    Result := Options.VertSizing and TreeList.OptionsCustomizing.BandVertSizing and IsBottom
  else
    Result := TreeList.OptionsCustomizing.BandHorzSizing and Options.Sizing;
end;

function TcxTreeListBand.GetSizingBoundsRect(
  ADirection: TcxDragSizingDirection): TRect;
begin
  if ADirection = dsdVert then
    Result := cxRectSetTop(TreeList.ClientRect,
      HeaderCell.DisplayRect.Top + TreeList.ViewInfo.BandHeaderLineHeight)
  else
  begin
    if not HeaderCell.IsRightToLeftConverted then
      Result := cxRectSetLeft(TreeList.ClientRect,  Max(HeaderCell.DisplayRect.Left +
        ActualMinWidth + IndentWidth, HeaderCell.VisibleRect.Left))
    else
      Result := cxRectSetRight(TreeList.ClientRect, Min(HeaderCell.DisplayRect.Right -
        ActualMinWidth - IndentWidth, HeaderCell.VisibleRect.Right))
  end;
end;

function TcxTreeListBand.GetSizingIncrement(
  ADirection: TcxDragSizingDirection): Integer;
begin
  Result := 1;
end;

function TcxTreeListBand.IsDynamicUpdate: Boolean;
begin
  Result := TreeList.OptionsCustomizing.DynamicSizing and TreeList.HitTest.HitAtSizingHorz;
end;

procedure TcxTreeListBand.SetSizeDelta(
  ADirection: TcxDragSizingDirection; ADelta: Integer);
begin
  if ADelta = 0 then Exit;
  if TreeList.ViewInfo.IsRightToLeftConverted and (ADirection = dsdHorz) then
    ADelta := -ADelta;
  if ADirection <> dsdHorz then
    TreeList.OptionsView.BandLineHeight :=
      (TreeList.ViewInfo.BandsHeight + ADelta) div Bands.LineCount
  else
    ForceWidth(DisplayWidth + ADelta);
  TreeList.DoBandSizeChanged(Self);
end;

function TcxTreeListBand.GetObjectName: string;
begin
  Result := 'Band' + IntToStr(ID);
end;

function TcxTreeListBand.GetProperties(
  AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  for I := Low(BandPropertiesName) to High(BandPropertiesName) do
    AProperties.Add(BandPropertiesName[I]);
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

function TcxTreeListBand.GetPropertyIndex(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(BandPropertiesName) to High(BandPropertiesName) do
    if BandPropertiesName[I] = AName then
    begin
      Result := I;
      Break;
    end;
end;

procedure TcxTreeListBand.GetPropertyValue(
  const AName: string; var AValue: Variant);
begin
  case GetPropertyIndex(AName) of
    0:
      AValue := Caption.Text;
    1:
      AValue := FMinWidth;
    2:
      AValue := FWidth;
    3:
      AValue := Visible;
    4:
      AValue := Index;
    5:
      AValue := Position.BandIndex;
    6:
      AValue := Position.ColIndex;
  else
    if Assigned(OnGetStoredPropertyValue) then
      OnGetStoredPropertyValue(Self, AName, AValue);
  end;
end;

procedure TcxTreeListBand.SetPropertyValue(
  const AName: string; const AValue: Variant);
begin
  case GetPropertyIndex(AName) of
   0:
      Caption.Text := AValue;
    1:
      FMinWidth := AValue;
    2:
      FWidth := AValue;
    3:
      Visible := AValue;
    4:
      FIndex := AValue;
    5:
      Position.BandIndex := AValue;
    6:
      Position.ColIndex := AValue;
  else
    if Assigned(OnSetStoredPropertyValue) then
      OnSetStoredPropertyValue(Self, AName, AValue);
  end;
end;

function TcxTreeListBand.GetParentBand: TcxTreeListBand;
begin
  Result := FPosition.ParentBand;
end;

function TcxTreeListBand.GetParentBandWidthAssigned: TcxTreeListBand;
begin
  Result := ParentBand;
  while Result <> nil do
  begin
    if Result.Width <> 0 then Break;
    Result := Result.ParentBand;
  end;
end;

function TcxTreeListBand.GetRootIndex: Integer;
begin
  if Bands = nil then
    Result := -1
  else
    Result := Bands.FRootItems.IndexOf(Self);
end;

function TcxTreeListBand.GetRootParentBand: TcxTreeListBand;
begin
  Result := ParentBand;
  if Result = nil then Exit;
  while Result.ParentBand <> nil do
    Result := Result.ParentBand;
end;

function TcxTreeListBand.GetTreeList: TcxCustomTreeList;
begin
  Result := Bands.TreeList;
end;

function TcxTreeListBand.GetActuallyExpandable: Boolean;
begin
  Result := FirstChildBottomBand = Bands.ExpandableBand;
end;

function TcxTreeListBand.GetActuallyVisible: Boolean;
begin
  Result := FVisible and ((ParentBand = nil) or ParentBand.ActuallyVisible);
end;

function TcxTreeListBand.GetBands: TcxTreeListBands;
begin
  Result := TcxTreeListBands(Collection);
end;

function TcxTreeListBand.GetCalculatedWidth: Integer;
var
  AWidth, I: Integer;
begin
  Refresh;
  if FCalculatedWidth = 0 then
  begin
    FCalculatedWidth := FWidth;
    if FCalculatedWidth = 0 then
    begin
      FCalculatedWidth := FWidth;
      if FCalculatedWidth = 0 then
        FCalculatedWidth := BandRows.RowMaxWidth;
      if (ChildBandCount > 0)  then
      begin
        AWidth := 0;
        for I := 0 to ChildBandCount - 1 do
          if ChildBands[I].Visible then
            Inc(AWidth, ChildBands[I].CalculatedWidth);
         FCalculatedWidth := Max(FCalculatedWidth, AWidth);
      end;
      if FCalculatedWidth = 0 then
        FCalculatedWidth := Max(cxTreeListDefWidth, MinWidth);
    end;
  end;
  Result := Max(FCalculatedWidth, BandRows.RowMinWidth);
end;

function TcxTreeListBand.GetColumn(AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FColumns[AIndex]);
end;

function TcxTreeListBand.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TcxTreeListBand.GetChildBand(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(FChildBands[AIndex]);
end;

function TcxTreeListBand.GetChildBandCount: Integer;
begin
  Result := FChildBands.Count;
end;

function TcxTreeListBand.GetDisplayWidth: Integer;
begin
  Result := CalculatedWidth;
end;

function TcxTreeListBand.GetFirstChildBottomBand: TcxTreeListBand;
begin
  Result := Self;
  while Result.ChildBandCount > 0 do
    Result := Result.ChildBands[0];
end;

function TcxTreeListBand.GetHasEmptyArea: Boolean;
begin
  Result := FHasEmptyArea or
    (BandRows.LineCount <> Bands.ColumnsLineCount);
end;

function TcxTreeListBand.GetIndentWidth: Integer;
begin
  Result := 0;
  if ActuallyExpandable then
    Result := TreeList.IndentWidth;
end;

function TcxTreeListBand.GetIsBottom: Boolean;
begin
  Result := ChildBandCount = 0;
end;

function TcxTreeListBand.GetIsFirstInGroup: Boolean;
var
  ABand: TcxTreeListBand;
begin
  Result := False;
  ABand := Bands.VisibleItems[Bands.GetFirstVisibleIndex(FixedKind)];
  while not Result and (ABand <> nil) do
  begin
    Result := ABand = Self;
    if ABand.ChildVisibleBands.Count > 0 then
      ABand := TcxTreeListBand(ABand.ChildVisibleBands[0])
    else
      ABand := nil;
  end;
end;

function TcxTreeListBand.GetIsLastAsChild: Boolean;

  function GetVisibleBandCount: Integer;
  begin
    if ParentBand = nil then
      Result := Bands.RootItemCount
    else
      Result := ParentBand.FChildVisibleBands.Count;
  end;

begin
  Result := FPosition.VisibleColIndex = GetVisibleBandCount - 1;
end;

function TcxTreeListBand.GetIsLastInGroup: Boolean;
var
  ABand: TcxTreeListBand;
begin
  Result := False;
  ABand := Bands.VisibleItems[Bands.GetLastVisibleIndex(FixedKind)];
  while not Result and (ABand <> nil) do
  begin
    Result := ABand = Self;
    if ABand.ChildVisibleBands.Count > 0 then
      ABand := TcxTreeListBand(ABand.ChildVisibleBands[ABand.ChildVisibleBands.Count - 1])
    else
      ABand := nil;
  end;
end;

function TcxTreeListBand.GetIsLeftMost: Boolean;
begin
  Result := IsFirstInGroup and
    (Bands.GetFirstVisibleIndex(FixedKind) = 0);
end;

function TcxTreeListBand.GetIsLoading: Boolean;
begin
  Result := TreeList.IsLoading and not TreeList.IgnoreLoadingStatus;
end;

function TcxTreeListBand.GetIsRightMost: Boolean;
begin
  Result := IsLastInGroup and
    (Bands.GetLastVisibleIndex(FixedKind) = Bands.VisibleItemCount - 1);
end;

function TcxTreeListBand.GetIsRoot: Boolean;
begin
  Result := Position.ParentBand = nil;
end;

function TcxTreeListBand.GetIsWidthFixed: Boolean;
var
  AForcingColumn: TcxTreeListColumn;
begin
  AForcingColumn := TreeList.Controller.ForcingWidthColumn;
  Result := not Options.Sizing or IsBandFixedDuringSizing or
      ((AForcingColumn <> nil) and (AForcingColumn.Position.Band.ParentBandWidthAssigned = Self));
  if not Result and (TreeList.Controller.ForcingWidthBand <> nil) and
    (TreeList.Controller.ForcingWidthBand.ParentBand = ParentBand) then
    Result := Position.ColIndex <= TreeList.Controller.ForcingWidthBand.Position.ColIndex;
end;

function TcxTreeListBand.GetLevel: Integer;
var
  AParent: TcxTreeListBand;
begin
  Result := 0;
  AParent := ParentBand;
  while AParent <> nil do
  begin
    Inc(Result);
    AParent := AParent.ParentBand;
  end;
end;

function TcxTreeListBand.GetVisibleColumn(AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FVisibleColumns[AIndex]);
end;

function TcxTreeListBand.GetVisibleColumnCount: Integer;
begin
  Result := FVisibleColumns.Count;
end;

function TcxTreeListBand.GetVisibleRootIndex: Integer;
begin
  Result := Bands.FVisibleRootItems.IndexOf(Self);
end;

function TcxTreeListBand.GetVisibleIndex: Integer;
begin
  Result := Bands.VisibleIndexOf(Self);
end;

procedure TcxTreeListBand.SetCalculatedWidth(AValue: Integer);
begin
  FCalculatedWidth := AValue;
  AdjustSubItems;
end;

procedure TcxTreeListBand.SetCaption(AValue: TcxTreeListCaption);
begin
  FCaption.Assign(AValue);
end;

procedure TcxTreeListBand.SetColumn(AIndex: Integer; AValue: TcxTreeListColumn);
begin
  Columns[AIndex].Assign(AValue);
end;

procedure TcxTreeListBand.SetDisplayWidth(AValue: Integer);
begin
  ForceWidth(AValue);
end;

procedure TcxTreeListBand.SetExpandable(AValue: TcxTreeListBandExpandable);
begin
  if AValue <> FExpandable then
  begin
    FExpandable := AValue;
    LayoutChanged;
  end;
end;

procedure TcxTreeListBand.SetFixedKind(AValue: TcxTreeListBandFixedKind);
var
  I: Integer;
begin
  if AValue <> FixedKind then
  begin
    FFixedKind := AValue;
    TreeList.BeginUpdate;
    try
      for I := 0 to ChildBandCount - 1 do
        ChildBands[I].FixedKind := AValue;
      if not IsRoot and (FFixedKind <> ParentBand.FixedKind) then
        Position.BandIndex := -1;
    finally
      TreeList.EndUpdate;
    end;
  end;
end;

procedure TcxTreeListBand.SetMinWidth(AValue: Integer);
begin
  if AValue <> FMinWidth then
  begin
    FMinWidth := AValue;
    LayoutChanged;
  end;
end;

procedure TcxTreeListBand.SetOptions(AValue: TcxTreeListBandOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TcxTreeListBand.SetPosition(AValue: TcxTreeListBandPosition);
begin
  FPosition.Assign(AValue);
end;

procedure TcxTreeListBand.SetRootIndex(AValue: Integer);
begin
  if not IsRoot or TreeList.IsDestroying then Exit;
  AValue := Max(0, Min(AValue, Bands.RootItemCount - 1));
  if RootIndex <> AValue then
    Index := Bands.RootItems[AValue].Index;
end;

procedure TcxTreeListBand.SetStyles(AValue: TcxTreeListBandStyles);
begin
  FStyles.Assign(AValue);
end;

procedure TcxTreeListBand.SetVisibleColumn(
  AIndex: Integer; AValue: TcxTreeListColumn);
begin
  VisibleColumns[AIndex].Assign(AValue);
end;

procedure TcxTreeListBand.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    TreeList.ClearCalculatedWidths;
    LayoutChanged;
  end;
end;

procedure TcxTreeListBand.SetWidth(AValue: Integer);
begin
  if AValue < 0 then Exit;
  if AValue > 0 then
    AValue := Max(AValue, ActualMinWidth);
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    FCalculatedWidth := AValue;
    AdjustSubItems;
    if ActuallyVisible then
      TreeList.LayoutChanged;
  end;
end;

{ TcxTreeListBands }

constructor TcxTreeListBands.Create(AOwner: TcxCustomTreeList);
begin
  inherited Create(AOwner.GetBandItemClass);
  FTreeList := AOwner;
  FRootItems := TList.Create;
  FVisibleItems := TList.Create;
  FVisibleRootItems := TList.Create;
  FBottomItems := TList.Create;
end;

destructor TcxTreeListBands.Destroy;
begin
  FBottomItems.Free;
  FVisibleRootItems.Free;
  FVisibleItems.Free;
  FRootItems.Free;
  inherited Destroy;
end;

procedure TcxTreeListBands.Assign(ASource: TPersistent);
var
  I: Integer;
begin
  if ASource is TcxTreeListBand then
  begin
    TreeList.BeginUpdate;
    try
      TreeList.SaveItemsPosition(True);
      for I := 0 to TcxTreeListBands(ASource).Count - 1 do
        Add.Assign(TcxTreeListBands(ASource)[I]);
      TreeList.AssignItemsPosition(True);
    finally
      TreeList.EndUpdate;
    end;
  end
  else
    inherited Assign(ASource);
end;

function TcxTreeListBands.Add: TcxTreeListBand;
begin
  BeginUpdate;
  try
    Result := TcxTreeListBand(inherited Add);
  finally
    EndUpdate;
  end;
end;

procedure TcxTreeListBands.RestoreDefaults;
var
  I: Integer;
begin
  TreeList.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].RestoreDefaults;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListBands.RestoreWidths;
var
  I: Integer;
begin
  TreeList.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].RestoreWidths;
  finally
    TreeList.EndUpdate;
  end;
end;

function TcxTreeListBands.VisibleIndexOf(ABand: TcxTreeListBand): Integer;
begin
  Result := FVisibleItems.IndexOf(ABand);
end;

function TcxTreeListBands.PopulateVisibleChildren(
  ABand: TcxTreeListBand): Integer;
var
  I: Integer;
begin
  Result := 0;
  if not ABand.Visible then Exit;
  Inc(Result);
  FVisibleItems.Add(ABand);
  if ABand.FixedKind = tlbfLeft then
    Inc(FVisibleLeftFixedCount)
  else
    if ABand.FixedKind = tlbfRight then
      Inc(FVisibleRightFixedCount);
  for I := 0 to ABand.ChildBandCount - 1 do
    Inc(Result, PopulateVisibleChildren(ABand.ChildBands[I]));
  if Result = 1 then
  begin
    FBottomItems.Add(ABand);
    FLineCount := Max(FLineCount, ABand.Level + 1);
  end;
end;

procedure TcxTreeListBands.RefreshInformation;
var
  I: Integer;
  ABand: TcxTreeListBand;
begin
  if IsUpdateLocked then Exit;
  FRootItems.Clear;
  FBottomItems.Clear;
  FVisibleItems.Clear;
  FVisibleRootItems.Clear;
  TreeList.FVisibleColumns.Clear;
  FLineCount := 0;
  FColumnsLineCount := 0;
  FVisibleLeftFixedCount := 0;
  FVisibleRightFixedCount := 0;
  FVisibleRootLeftFixedCount := 0;
  FVisibleRootRightFixedCount := 0;
  FExpandableBand := nil;
  if Count = 0 then Exit;
  for I := 0 to Count - 1 do
  begin
    ABand := Items[I];
    ABand.FLineCount := 0;
    if ABand.IsRoot then
    begin
      FRootItems.Add(ABand);
      if ABand.ActuallyVisible then
      begin
        if ABand.FixedKind = tlbfLeft then
          Inc(FVisibleRootLeftFixedCount)
        else
          if ABand.FixedKind = tlbfRight then
            Inc(FVisibleRootRightFixedCount);
        FVisibleRootItems.Add(ABand);
      end;
    end;
  end;
  FVisibleRootItems.Sort(@cxCompareBands);
  for I := 0 to VisibleRootItemCount - 1 do
    PopulateVisibleChildren(VisibleRootItems[I]);
  FVisibleItems.Sort(@cxCompareBands);
  FColumnsLineCount := Byte(BottomItemCount > 0);
  for I := 0 to BottomItemCount - 1 do
    with BottomItems[I] do
    begin
      CalculateLineCount(Self.FLineCount);
      CheckExpandable(FExpandableBand);
      FColumnsLineCount := Max(FColumnsLineCount, BandRows.LineCount);
    end;
  if CanExpandableLeftMostOnly and (FExpandableBand <> nil) then
    FExpandableBand := BottomItems[0];
end;

procedure TcxTreeListBands.Adjust(
  ABands: TList = nil; AWidth: Integer = 0);
var
  I: Integer;
  AAutoWidth: TcxAutoWidthObject;
begin
  if IsUpdateLocked then Exit;
  if ABands = nil then
  begin
    ABands := FVisibleRootItems;
    AWidth := TreeList.AvailableContentWidth;
    ClearCalculatedWidths;
    RefreshInformation;
  end;
  if ABands.Count = 0 then Exit;
  for I := 0 to ABands.Count - 1 do
    TcxTreeListBand(ABands[I]).AdjustSubItems;
  if (ABands <> FVisibleRootItems) or TreeList.OptionsView.ColumnAutoWidth then
  begin
    AAutoWidth := TcxAutoWidthObject.Create(ABands.Count);
    try
      AAutoWidth.AvailableWidth := AWidth;
      for I := 0 to ABands.Count - 1 do
        TcxTreeListBand(ABands[I]).InitAutoWidthItem(AAutoWidth.AddItem);
      AAutoWidth.Calculate;
      for I := 0 to ABands.Count - 1 do
        TcxTreeListBand(ABands[I]).CalculatedWidth := AAutoWidth.Items[I].AutoWidth;
    finally
      AAutoWidth.Free;
    end;
  end;
end;

procedure TcxTreeListBands.AssignColumnsWidth;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AssignColumnsWidth;
  TreeList.ClearCalculatedWidths;
end;

procedure TcxTreeListBands.AssignRootItemWidths;
var
  I: Integer;
begin
  for I := 0 to RootItemCount - 1 do
    RootItems[I].AssignWidth;
end;

procedure TcxTreeListBands.AssignRowColumnsWidth(ARow: TcxTreeListBandRow);
var
  I: Integer;
begin
  for I := 0 to BottomItemCount - 1 do
    if BottomItems[I].BandRows.VisibleItemCount > ARow.VisibleIndex then
      BottomItems[I].BandRows.VisibleItems[ARow.VisibleIndex].AssignColumnsWidth;
end;

function TcxTreeListBands.CanExpandableLeftMostOnly: Boolean;
begin
  Result := TreeList.OptionsView.IsCategorizedPaint or
    TreeList.Preview.Active;
end;

procedure TcxTreeListBands.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TcxTreeListBands.ClearCalculatedWidths;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FCalculatedWidth := 0;
end;

function TcxTreeListBands.GetFirstVisibleIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
begin
  case AFixedKind of
    tlbfNone:
      Result := VisibleLeftFixedCount;
    tlbfRight:
      Result := VisibleItemCount - VisibleRightFixedCount;
  else
    Result := 0;
  end
end;

function TcxTreeListBands.GetFirstVisibleRootIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
begin
  case AFixedKind of
    tlbfNone:
      Result := VisibleRootLeftFixedCount;
    tlbfRight:
      Result := VisibleRootItemCount - VisibleRootRightFixedCount;
  else
    Result := 0;
  end;
end;

function TcxTreeListBands.GetLastVisibleIndex(AFixedKind: TcxTreeListBandFixedKind): Integer;
begin
  case AFixedKind of
    tlbfLeft:
      Result := GetFirstVisibleIndex(tlbfNone) - 1;
    tlbfNone:
      Result := GetFirstVisibleIndex(tlbfRight) - 1;
  else
    Result := VisibleItemCount - 1;
  end;
end;

function TcxTreeListBands.GetLastVisibleRootIndex(
  AFixedKind: TcxTreeListBandFixedKind): Integer;
begin
  case AFixedKind of
    tlbfLeft:
      Result := GetFirstVisibleRootIndex(tlbfNone) - 1;
    tlbfNone:
      Result := GetFirstVisibleRootIndex(tlbfRight) - 1;
  else
    Result := VisibleRootItemCount - 1;
  end;
end;

function TcxTreeListBands.GetVisibleCountByKind(
  AFixedKind: TcxTreeListBandFixedKind): Integer;
begin
  Result := FVisibleLeftFixedCount;
  case AFixedKind of
    tlbfNone:
      Result := VisibleItemCount - FVisibleRightFixedCount - Result;
    tlbfRight:
      Result := FVisibleRightFixedCount;
  end;
end;

function TcxTreeListBands.GetOwner: TPersistent;
begin
  Result := FTreeList;
end;

procedure TcxTreeListBands.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if not TreeList.IsLocked then
    TreeList.LayoutChanged;
end;

procedure TcxTreeListBands.Notify(
  Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnExtracting) and (TreeList <> nil) then
    TcxTreeListBand(Item).RemoveColumns;
  inherited Notify(Item, Action);
end;
//

function TcxTreeListBands.IsUpdateLocked: Boolean;
begin
  Result := FLockUpdate > 0;
end;

procedure TcxTreeListBands.LockUpdate;
begin
  Inc(FLockUpdate);
end;

procedure TcxTreeListBands.UnlockUpdate;
begin
  Dec(FLockUpdate);
end;

// IInterface
function TcxTreeListBands.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

function TcxTreeListBands._AddRef: Integer;
begin
  Result := -1;
end;

function TcxTreeListBands._Release: Integer;
begin
  Result := -1;
end;

// IcxStoredObject }
function TcxTreeListBands.GetObjectName: string;
begin
  Result := 'Bands';
end;

function TcxTreeListBands.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
end;

procedure TcxTreeListBands.GetPropertyValue(
  const AName: string; var AValue: Variant);
begin
end;

procedure TcxTreeListBands.SetPropertyValue(
  const AName: string; const AValue: Variant);
begin
end;

// IcxStoredParent
function TcxTreeListBands.CreateChild(
  const AObjectName, AClassName: string): TObject;
begin
  if AClassName = 'TcxTreeListBand' then
    Result := Add
  else
    Result := nil;
end;

procedure TcxTreeListBands.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

procedure TcxTreeListBands.GetChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AChildren.AddObject('', Items[I]);
end;

function TcxTreeListBands.GetBottomItem(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(FBottomItems[AIndex]);
end;

function TcxTreeListBands.GetBottomItemCount: Integer;
begin
  Result := FBottomItems.Count;
end;

function TcxTreeListBands.GetFirstVisibleBand: TcxTreeListBand;
begin
  if VisibleItemCount > 0 then
    Result := VisibleItems[0]
  else
    Result := nil;
end;

function TcxTreeListBands.GetItem(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(inherited GetItem(AIndex));
end;

function TcxTreeListBands.GetLastVisibleBand: TcxTreeListBand;
begin
  if VisibleItemCount > 0 then
    Result := VisibleItems[VisibleItemCount - 1]
  else
    Result := nil;
end;

function TcxTreeListBands.GetRootItemCount: Integer;
begin
  Result := FRootItems.Count;
end;

function TcxTreeListBands.GetRootItem(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(FRootItems[AIndex]);
end;

function TcxTreeListBands.GetVisibleItem(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(FVisibleItems[AIndex]);
end;

function TcxTreeListBands.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxTreeListBands.GetVisibleRootItem(AIndex: Integer): TcxTreeListBand;
begin
  Result := TcxTreeListBand(FVisibleRootItems[AIndex])
end;

function TcxTreeListBands.GetVisibleRootItemCount: Integer;
begin
  Result := FVisibleRootItems.Count;
end;

procedure TcxTreeListBands.SetItem(AIndex: Integer; AValue: TcxTreeListBand);
begin
  Items[AIndex].Assign(AValue);
end;

{ TcxTreeListBandRow }

constructor TcxTreeListBandRow.Create(AOwner: TcxTreeListBandRows);
begin
  FBandRows := AOwner;
  FItems := TList.Create;
  FVisibleItems := TList.Create;
end;

destructor TcxTreeListBandRow.Destroy;
begin
  FreeAndNil(FVisibleItems);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TcxTreeListBandRow.IndexOf(AColumn: TcxTreeListColumn): Integer;
begin
  Result := FItems.IndexOf(AColumn);
end;

procedure TcxTreeListBandRow.AdjustColumns;
var
  I: Integer;
  AAutoWidth: TcxAutoWidthObject;
begin
  if Count = 0 then Exit;
  AAutoWidth := TcxAutoWidthObject.Create(VisibleItemCount);
  try
    AAutoWidth.AvailableWidth := Band.CalculatedWidth;
    for I := 0 to VisibleItemCount  - 1 do
      VisibleItems[I].InitAutoWidthItem(AAutoWidth.AddItem);
    AAutoWidth.Calculate;
    for I := 0 to VisibleItemCount - 1 do
      VisibleItems[I].FCalculatedWidth := AAutoWidth.Items[I].AutoWidth;
  finally
    AAutoWidth.Free;
  end;
end;

procedure TcxTreeListBandRow.AssignColumnsWidth;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].DisplayWidth <> 0 then
      Items[I].FWidth := Items[I].DisplayWidth;
end;

procedure TcxTreeListBandRow.CheckEmpty;
begin
  BandRows.CheckRowEmpty(Index);
end;

procedure TcxTreeListBandRow.Refresh;
var
  I: Integer;
  AColumn: TcxTreeListColumn;
begin
  FVisibleItems.Clear;
  FLineOffset := 0;
  FLineCount := 0;
  FMinWidth := 0;
  FWidth := 0;
  for I := 0 to Count - 1 do
  begin
    AColumn := Items[I];
    if AColumn.Visible and not AColumn.IsPreview then
    begin
      AColumn.Position.FVisibleColIndex := FVisibleItems.Add(AColumn);
      FLineCount := Max(FLineCount, AColumn.Position.LineCount);
      Band.FHasEmptyArea := Band.FHasEmptyArea or
        ((FLineCount <> 0) and (FLineCount <> AColumn.Position.LineCount));
      Inc(FWidth, AColumn.Width);
      Inc(FMinWidth, AColumn.MinWidth);
      FWidthAssigned := AColumn.FWidth <> 0;
    end
    else
      AColumn.Position.FVisibleColIndex := -1;
  end;
end;

function TcxTreeListBandRow.GetBand: TcxTreeListBand;
begin
  Result := FBandRows.FBand;
end;

function TcxTreeListBandRow.GetCount: Integer;
begin
  Result := FItems.Count
end;

function TcxTreeListBandRow.GetIsFirst: Boolean;
begin
  Result := BandRows.First = Self;
end;

function TcxTreeListBandRow.GetFirst: TcxTreeListColumn;
begin
  if VisibleItemCount > 0 then
    Result := VisibleItems[0]
  else
    Result := nil;
end;

function TcxTreeListBandRow.GetIndex: Integer;
begin
  Result := BandRows.FItems.IndexOf(Self);
end;

function TcxTreeListBandRow.GetIsLast: Boolean;
begin
  Result := BandRows.Last = Self;
end;

function TcxTreeListBandRow.GetItem(AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FItems[AIndex])
end;

function TcxTreeListBandRow.GetVisibleIndex: Integer;
begin
  Result := BandRows.FVisibleItems.IndexOf(Self);
end;

function TcxTreeListBandRow.GetVisibleItem(AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FVisibleItems[AIndex]);
end;

function TcxTreeListBandRow.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxTreeListBandRow.GetLast: TcxTreeListColumn;
begin
  if VisibleItemCount > 0 then
    Result := VisibleItems[VisibleItemCount - 1]
  else
    Result := nil;
end;

function TcxTreeListBandRow.GetTreeList: TcxCustomTreeList;
begin
  Result := FBandRows.FBand.GetTreeList;
end;

{ TcxTreeListBandRows }

constructor TcxTreeListBandRows.Create(AOwner: TcxTreeListBand);
begin
  FBand := AOwner;
  FItems := TcxObjectList.Create;
  FVisibleItems := TList.Create;
end;

destructor TcxTreeListBandRows.Destroy;
begin
  FVisibleItems.Free;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TcxTreeListBandRows.Add: TcxTreeListBandRow;
begin
  Result := TcxTreeListBandRow.Create(Self);
  FItems.Add(Result);
end;

function TcxTreeListBandRows.CheckRowIndex(
  ARowIndex: Integer): TcxTreeListBandRow;
begin
  ARowIndex := Max(0, ARowIndex);
  if ARowIndex < Count then
    Result := Items[ARowIndex]
  else
    Result := Add;
end;

procedure TcxTreeListBandRows.CheckRowEmpty(ARowIndex: Integer);
begin
  if (ARowIndex >= 0) and (ARowIndex < Count) and (Items[ARowIndex].Count = 0) then
  begin
    Items[ARowIndex].Free;
    FItems.Delete(ARowIndex);
  end;
end;

function TcxTreeListBandRows.Insert(AIndex: Integer): TcxTreeListBandRow;
begin
  Result := TcxTreeListBandRow.Create(Self);
  FItems.Insert(AIndex, Result);
end;

procedure TcxTreeListBandRows.Refresh;
var
  I: Integer;
  AItem: TcxTreeListBandRow;
  AWidthAssigned: Boolean;
begin
  FRowMinWidth := 0;
  FRowMaxWidth := 0;
  FLineCount := 0;
  FVisibleItems.Clear;
  Band.HasEmptyArea := False;
  AWidthAssigned := False;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    AItem.Refresh;
    if AItem.VisibleItemCount > 0 then
    begin
      FVisibleItems.Add(AItem);
      FRowMinWidth := Max(FRowMinWidth, AItem.MinWidth);
      if not AWidthAssigned or AItem.WidthAssigned then
      begin
        FRowMaxWidth := Max(FRowMaxWidth, AItem.Width);
        AWidthAssigned := AItem.WidthAssigned;
      end;
      AItem.FLineOffset := FLineCount;
      Inc(FLineCount, AItem.LineCount);
      Band.FVisibleColumns.Assign(AItem.FVisibleItems, laOr);
    end;
  end;
end;

function TcxTreeListBandRows.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxTreeListBandRows.GetFirst: TcxTreeListBandRow;
begin
  Result := GetItemEx(0);
end;

function TcxTreeListBandRows.GetItem(Index: Integer): TcxTreeListBandRow;
begin
  Result := TcxTreeListBandRow(FItems[Index]);
end;

function TcxTreeListBandRows.GetItemEx(Index: Integer): TcxTreeListBandRow;
begin
  if (Index < 0) or (Index >= Count) then
    Result := nil
  else
    Result := Items[Index];
end;

function TcxTreeListBandRows.GetLast: TcxTreeListBandRow;
begin
  Result := GetItemEx(FItems.Count - 1);
end;

function TcxTreeListBandRows.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxTreeListBandRows.GetVisibleItem(AIndex: Integer): TcxTreeListBandRow;
begin
  Result := TcxTreeListBandRow(FVisibleItems[AIndex]);
end;

{ TcxTreeListFilterBox }

function TcxTreeListFilterBox.GetCustomizeButtonCaption: string;
begin
  Result := cxGetResourceString(@scxTreeListFilterCustomizeButtonCaption);
end;

function TcxTreeListFilterBox.GetDefaultText: string;
begin
  Result := cxGetResourceString(@scxTreeListFilterIsEmpty);
end;

{ TcxTreeListDateTimeHandling }

function TcxTreeListDateTimeHandling.GetDefaultMonthFormat: string;
begin
  Result := cxGetResourceString(@scxTreeListMonthFormat);
end;

function TcxTreeListDateTimeHandling.GetDefaultYearFormat: string;
begin
  Result := cxGetResourceString(@scxTreeListYearFormat);
end;

{ TcxTreeListColumnFilterPopupOptions }

function TcxTreeListFilteringColumnPopupOptions.ApplyButtonCaption: string;
begin
  Result := cxGetResourceString(@scxTreeListFilterApplyButtonCaption);
end;

{ TcxTreeListFiltering }

function TcxTreeListFiltering.GetItemExcelPopupClass: TcxControlOptionsFilteringItemExcelPopupClass;
begin
  Result := TcxTreeListFilteringColumnExcelPopupOptions;
end;

function TcxTreeListFiltering.GetItemPopupClass: TcxControlOptionsFilteringItemPopupClass;
begin
  Result := TcxTreeListFilteringColumnPopupOptions;
end;

function TcxTreeListFiltering.GetColumnExcelPopup: TcxTreeListFilteringColumnExcelPopupOptions;
begin
  Result := TcxTreeListFilteringColumnExcelPopupOptions(ItemExcelPopup);
end;

function TcxTreeListFiltering.GetColumnPopup: TcxTreeListFilteringColumnPopupOptions;
begin
  Result := TcxTreeListFilteringColumnPopupOptions(ItemPopup);
end;

function TcxTreeListFiltering.GetColumnPopupMode: TdxFilterPopupWindowMode;
begin
  Result := ItemPopupMode;
end;

procedure TcxTreeListFiltering.SetColumnExcelPopup(AValue: TcxTreeListFilteringColumnExcelPopupOptions);
begin
  ItemExcelPopup := AValue;
end;

procedure TcxTreeListFiltering.SetColumnPopup(AValue: TcxTreeListFilteringColumnPopupOptions);
begin
  ItemPopup := AValue;
end;

procedure TcxTreeListFiltering.SetColumnPopupMode(AValue: TdxFilterPopupWindowMode);
begin
  ItemPopupMode := AValue;
end;

{ TcxTreeListOptionsView }

constructor TcxTreeListOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FIndicatorWidth := cxTreeListDefIndicatorWidth;
  FFixedSeparatorWidth := cxTreeListDefSeparatorWidth;
  FDropArrowColor := cxTreeListDefDropArrowColor;
  FButtons := True;
  FHeaders := True;
  FFixedSeparatorColor := clDefault;
  FEditAutoHeightBorderColor := clDefault;
  FFocusRect := True;
  FGridLineColor := clDefault;
  FTreeLineColor := clDefault;
  FTreeLineStyle := tllsDot;
  FUseNodeColorForIndent := True;
  FUseImageIndexForSelected := True;
  FShowRoot := True;
end;

procedure TcxTreeListOptionsView.Assign(Source: TPersistent);
var
  ASource: TcxTreeListOptionsView;
begin
  if Source is TcxTreeListOptionsView then
  begin
    ASource := TcxTreeListOptionsView(Source);
    FCategorizedColumn := TreeList.GetSameColumn(ASource.FCategorizedColumn);
    FBandLineHeight := ASource.FBandLineHeight;
    FBands := ASource.FBands;
    FButtons := ASource.FButtons;
    FColumnAutoWidth := ASource.ColumnAutoWidth;
    FCheckGroups := ASource.CheckGroups;
    FDropArrowColor := ASource.FDropArrowColor;
    FDropNodeIndicator := ASource.FDropNodeIndicator;
    FDynamicIndent := ASource.FDynamicIndent;
    FDynamicFocusedStateImages := ASource.FDynamicFocusedStateImages;
    FEditAutoHeightBorderColor := ASource.EditAutoHeightBorderColor;
    FExtPaintStyle := ASource.FExtPaintStyle;
    FFixedSeparatorColor := ASource.FFixedSeparatorColor;
    FFixedSeparatorWidth := ASource.FixedSeparatorWidth;
    FFocusRect := ASource.FocusRect;
    FGridLineColor := ASource.GridLineColor;
    FGridLines := ASource.FGridLines;
    FGroupFooters := ASource.GroupFooters;
    FHeaderAutoHeight := ASource.FHeaderAutoHeight;
    FHeaders := ASource.FHeaders;
    FIndicator := ASource.FIndicator;
    FIndicatorWidth := ASource.FIndicatorWidth;
    FPaintStyle := ASource.FPaintStyle;
    FShowRoot := ASource.FShowRoot;
    FSimpleCustomizeBox := ASource.FSimpleCustomizeBox;
    FTreeLineColor := ASource.FTreeLineColor;
    FTreeLineStyle := ASource.FTreeLineStyle;
    FUseImageIndexForSelected := ASource.FUseImageIndexForSelected;
    FUseNodeColorForIndent := ASource.FUseNodeColorForIndent;
  end;
  inherited Assign(Source);
end;

function TcxTreeListOptionsView.GetCategorizedColumn: TcxTreeListColumn;
begin
  if FCategorizedColumn <> nil then
    Result := FCategorizedColumn
  else
    if TreeList.VisibleColumnCount > 0 then
      Result := TreeList.VisibleColumns[0]
    else
      Result := nil;
end;

procedure TcxTreeListOptionsView.RestoreDefaults;
begin
  FBandLineHeight := 0;
  FBands := False;
  FButtons := True;
  FColumnAutoWidth := False;
  FCheckGroups := False;
  FDropArrowColor := cxTreeListDefDropArrowColor;
  FDynamicIndent := False;
  FDynamicFocusedStateImages := False;
  FExtPaintStyle := False;
  FFixedSeparatorColor := clDefault;
  FFixedSeparatorWidth := cxTreeListDefSeparatorWidth;
  FFooter := False;
  FGridLineColor := clDefault;
  FGridLines := tlglNone;
  FGroupFooters := tlgfInvisible;
  FHeaderAutoHeight := False;
  FHeaders := True;
  FIndicator := False;
  FIndicatorWidth := cxTreeListDefIndicatorWidth;
  FPaintStyle := tlpsStandard;
  FShowRoot := True;
  FSimpleCustomizeBox := False;
  FTreeLineColor := clDefault;
  FTreeLineStyle := tllsDot;
  FUseImageIndexForSelected := True;
  FUseNodeColorForIndent := True;
  Changed;
end;

procedure TcxTreeListOptionsView.Changed;
begin
  TreeList.LayoutChanged;
end;

procedure TcxTreeListOptionsView.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  BandLineHeight := MulDiv(BandLineHeight, M, D);
  FixedSeparatorWidth := MulDiv(FixedSeparatorWidth, M, D);
  IndicatorWidth := MulDiv(IndicatorWidth, M, D);
end;

function TcxTreeListOptionsView.GetControl: TObject;
begin
  Result := TreeList;
end;

function TcxTreeListOptionsView.IsColumnHeaderFilterButtonShowedAlways: Boolean;
begin
  Result := IsItemFilterButtonShowedAlways;
end;

function TcxTreeListOptionsView.IsColumnHeaderFilterSmartTag: Boolean;
begin
  Result := IsItemFilterSmartTag;
end;

function TcxTreeListOptionsView.HorzIncrement: Integer;
begin
  Result := Byte(GridLines in [tlglVert, tlglBoth])
end;

function TcxTreeListOptionsView.IsCategorizedPaint: Boolean;
begin
  Result := PaintStyle = tlpsCategorized;
end;

function TcxTreeListOptionsView.IsExtPaintStyle: Boolean;
begin
  Result := ExtPaintStyle and (GetTreeList.LookAndFeel.SkinPainter = nil);
end;

function TcxTreeListOptionsView.VertIncrement: Integer;
begin
  Result := Byte(GridLines in [tlglHorz, tlglBoth])
end;

function TcxTreeListOptionsView._AddRef: Integer;
begin
  Result := -1;
end;

function TcxTreeListOptionsView._Release: Integer;
begin
  Result := -1;
end;

function TcxTreeListOptionsView.QueryInterface(
  const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

function TcxTreeListOptionsView.GetHeaderFilterButtonShowMode: TcxFilterButtonShowMode;
begin
  Result := ItemFilterButtonShowMode;
end;

function TcxTreeListOptionsView.GetIsIndicatorVisible: Boolean;
begin
  with TreeList.OptionsCustomizing do
  begin
    Result := Indicator or (BandsQuickCustomization and BandCustomizing) or
      (ColumnsQuickCustomization and ColumnCustomizing);
  end;
end;

function TcxTreeListOptionsView.GetShowColumnFilterButtons: TcxShowFilterButtons;
begin
  Result := ShowItemFilterButtons;
end;

function TcxTreeListOptionsView.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(EditingControl);
end;

procedure TcxTreeListOptionsView.SetCategorizedColumn(AValue: TcxTreeListColumn);
begin
  if AValue <> FCategorizedColumn then
  begin
    FCategorizedColumn := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetCheckGroups(AValue: Boolean);
begin
  if AValue <> FCheckGroups then
  begin
    FCheckGroups := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetColumnAutoWidth(AValue: Boolean);
begin
  if AValue <> FColumnAutoWidth then
  begin
    FColumnAutoWidth := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetDynamicIndent(AValue: Boolean);
begin
  if AValue <> FDynamicIndent then
  begin
    FDynamicIndent := AValue;
    TreeList.ImagesChanged(nil);
  end;
end;

procedure TcxTreeListOptionsView.SetBandLineHeight(AValue: Integer);
begin
  if AValue < 0 then Exit;
  if AValue <> FBandLineHeight then
  begin
    FBandLineHeight := AValue;
    if Bands then
      Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetBands(AValue: Boolean);
begin
  if AValue <> Bands then
  begin
    FBands := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetButtons(AValue: Boolean);
begin
  if AValue <> FButtons then
  begin
    FButtons := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetIndicator(AValue: Boolean);
begin
  if AValue <> FIndicator then
  begin
    FIndicator := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetIndicatorWidth(AValue: Integer);
begin
  if AValue < 0 then Exit;
  if AValue <> FIndicatorWidth then
  begin
    FIndicatorWidth := AValue;
    if Indicator then
      Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetDynamicFocusedStateImages(AValue: Boolean);
begin
  if AValue <> FDynamicFocusedStateImages then
  begin
    FDynamicFocusedStateImages := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetEditAutoHeightBorderColor(AValue: TColor);
begin
  if AValue <> FEditAutoHeightBorderColor then
  begin
    FEditAutoHeightBorderColor := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetExtPaintStyle(AValue: Boolean);
begin
  if AValue <> FExtPaintStyle then
  begin
    FExtPaintStyle := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetFixedSeparatorColor(AValue: TColor);
begin
  if AValue <> FFixedSeparatorColor then
  begin
    FFixedSeparatorColor := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetFixedSeparatorWidth(AValue: Integer);
begin
   if AValue < 0 then Exit;
   if AValue <> FFixedSeparatorWidth then
   begin
     FFixedSeparatorWidth := AValue;
     Changed;
   end;
end;

procedure TcxTreeListOptionsView.SetFocusRect(AValue: Boolean);
begin
  if AValue <> FFocusRect then
  begin
    FFocusRect := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetFooter(AValue: Boolean);
begin
  if AValue <> FFooter then
  begin
    FFooter := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetGridLineColor(AValue: TColor);
begin
  if AValue <> FGridLineColor then
  begin
    FGridLineColor := AValue;
    if GridLines <> tlglNone then
      Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetGridLines(AValue: TcxTreeListGridLines);
begin
  if AValue <> FGridLines then
  begin
    FGridLines := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetGroupFooters(
  AValue: TcxTreeListGroupFootersMode);
begin
  if AValue <> FGroupFooters then
  begin
    FGroupFooters := AValue;
    TreeList.StructureChanged;
  end;
end;

procedure TcxTreeListOptionsView.SetHeaderAutoHeight(AValue: Boolean);
begin
  if AValue <> FHeaderAutoHeight then
  begin
    FHeaderAutoHeight := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetHeaderFilterButtonShowMode(AValue: TcxFilterButtonShowMode);
begin
  ItemFilterButtonShowMode := AValue;
end;

procedure TcxTreeListOptionsView.SetHeaders(AValue: Boolean);
begin
  if AValue <> FHeaders then
  begin
    FHeaders := AValue;
    TreeList.OptionsBehavior.CopyCaptionsToClipboard := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetPaintStyle(AValue: TcxTreeListPaintStyle);
begin
  if AValue <> FPaintStyle then
  begin
    FPaintStyle := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetShowColumnFilterButtons(Value: TcxShowFilterButtons);
begin
  ShowItemFilterButtons := Value;
end;

procedure TcxTreeListOptionsView.SetShowRoot(AValue: Boolean);
begin
  if AValue <> FShowRoot then
  begin
    FShowRoot := AValue;
    TreeList.ImagesChanged(nil);
  end;
end;

procedure TcxTreeListOptionsView.SetSimpleCustomizeBox(AValue: Boolean);
begin
  if AValue <> FSimpleCustomizeBox then
  begin
    FSimpleCustomizeBox := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetTreeLineColor(AValue: TColor);
begin
  if AValue <> FTreeLineColor then
  begin
    FTreeLineColor := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetTreeLineStyle(
  AValue: TcxTreeListTreeLineStyle);
begin
  if AValue <> FTreeLineStyle then
  begin
    FTreeLineStyle := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetUseImageIndexForSelected(AValue: Boolean);
begin
  if AValue <> FUseImageIndexForSelected then
  begin
    FUseImageIndexForSelected := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsView.SetUseNodeColorForIndent(AValue: Boolean);
begin
  if AValue <> FUseNodeColorForIndent then
  begin
    FUseNodeColorForIndent := AValue;
    Changed;
  end;
end;

{ TcxTreeListOptionsCustomizing }

constructor TcxTreeListOptionsCustomizing.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBandCustomizing := True;
  FBandHorzSizing := True;
  FBandMoving := True;
  FBandVertSizing := True;
  FColumnCustomizing := True;
  FColumnHorzSizing := True;
  FColumnMoving := True;
  FColumnVertSizing := True;
  FNestedBands := True;
  FStackedColumns := True;
  FBandsQuickCustomizationMultiColumnMode := True;
  FColumnsQuickCustomizationMultiColumnMode := True;
  FBandsQuickCustomizationShowCommands := True;
  FColumnsQuickCustomizationShowCommands := True;
end;

procedure TcxTreeListOptionsCustomizing.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListOptionsCustomizing then
    with TcxTreeListOptionsCustomizing(Source) do
    begin
      Self.BandCustomizing := BandCustomizing;
      Self.BandHiding := BandHiding;
      Self.BandHorzSizing := BandHorzSizing;
      Self.BandMoving := BandMoving;
      Self.BandsQuickCustomization := BandsQuickCustomization;
      Self.BandsQuickCustomizationMaxDropDownCount := BandsQuickCustomizationMaxDropDownCount;
      Self.BandsQuickCustomizationMultiColumnMode := BandsQuickCustomizationMultiColumnMode;
      Self.BandsQuickCustomizationShowCommands := BandsQuickCustomizationShowCommands;
      Self.BandsQuickCustomizationSorted := BandsQuickCustomizationSorted;
      Self.BandVertSizing := BandVertSizing;
      Self.ColumnCustomizing := ColumnCustomizing;
      Self.ColumnFiltering := ColumnFiltering;
      Self.ColumnHiding := ColumnHiding;
      Self.ColumnHorzSizing := ColumnHorzSizing;
      Self.ColumnMoving := ColumnMoving;
      Self.ColumnsQuickCustomization := ColumnsQuickCustomization;
      Self.ColumnsQuickCustomizationMaxDropDownCount := ColumnsQuickCustomizationMaxDropDownCount;
      Self.ColumnsQuickCustomizationMultiColumnMode := ColumnsQuickCustomizationMultiColumnMode;
      Self.ColumnsQuickCustomizationShowCommands := ColumnsQuickCustomizationShowCommands;
      Self.ColumnsQuickCustomizationSorted := ColumnsQuickCustomizationSorted;
      Self.ColumnVertSizing := ColumnVertSizing;
      Self.DynamicSizing := DynamicSizing;
      Self.NestedBands := NestedBands;
      Self.NodeSizing := NodeSizing;
      Self.RowSizing := RowSizing;
      Self.FStackedColumns := StackedColumns;
    end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListOptionsCustomizing.Changed;
begin
  TreeList.LayoutChanged;
end;

function TcxTreeListOptionsCustomizing.GetColumnFiltering: TdxDefaultBoolean;
begin
  Result := TreeList.OptionsBehavior.ItemFiltering;
end;

function TcxTreeListOptionsCustomizing.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

procedure TcxTreeListOptionsCustomizing.SetBandCustomizing(AValue: Boolean);
begin
  if AValue <> FBandCustomizing then
  begin
    FBandCustomizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetBandHiding(AValue: Boolean);
begin
  if AValue <> FBandHiding then
  begin
    FBandHiding := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetBandHorzSizing(AValue: Boolean);
begin
  if AValue <> FBandHorzSizing then
  begin
    FBandHorzSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetBandMoving(AValue: Boolean);
begin
  if AValue <> FBandMoving then
  begin
    FBandMoving := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetBandsQuickCustomization(
  AValue: Boolean);
begin
  if AValue <> FBandsQuickCustomization then
  begin
    FBandsQuickCustomization := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetBandVertSizing(AValue: Boolean);
begin
  if AValue <> FBandVertSizing then
  begin
    FBandVertSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnCustomizing(AValue: Boolean);
begin
  if AValue <> FColumnCustomizing then
  begin
    FColumnCustomizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnFiltering(AValue: TdxDefaultBoolean);
begin
  TreeList.OptionsBehavior.ItemFiltering := AValue;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnHiding(AValue: Boolean);
begin
  if AValue <> FColumnHiding then
  begin
    FColumnHiding := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnHorzSizing(AValue: Boolean);
begin
  if AValue <> FColumnHorzSizing then
  begin
    FColumnHorzSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnMoving(AValue: Boolean);
begin
  if AValue <> FColumnMoving then
  begin
    FColumnMoving := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnsQuickCustomization(
  AValue: Boolean);
begin
  if AValue <> FColumnsQuickCustomization then
  begin
    FColumnsQuickCustomization := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetColumnVertSizing(AValue: Boolean);
begin
  if AValue <> FColumnVertSizing then
  begin
    FColumnVertSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetDynamicSizing(AValue: Boolean);
begin
  if AValue <> FDynamicSizing then
  begin
    FDynamicSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetNestedBands(AValue: Boolean);
begin
  if AValue <> FNestedBands then
  begin
    FNestedBands := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetNodeSizing(AValue: Boolean);
begin
  if AValue <> FNodeSizing then
  begin
    FNodeSizing := AValue;
    if AValue then
      FRowSizing := False;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetRowSizing(AValue: Boolean);
begin
  if AValue <> FRowSizing then
  begin
    FRowSizing := AValue;
    if AValue then
      FNodeSizing := False;
    Changed;
  end;
end;

procedure TcxTreeListOptionsCustomizing.SetStackedColumns(AValue: Boolean);
begin
  if AValue <> FStackedColumns then
  begin
    FStackedColumns := AValue;
    Changed;
  end;
end;

{ TcxTreeListOptionsSelection }

constructor TcxTreeListOptionsSelection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCellSelect := True;
  FHideFocusRect := True;
  FInvertSelect := True;
end;

procedure TcxTreeListOptionsSelection.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListOptionsSelection then
    with TcxTreeListOptionsSelection(Source) do
    begin
      Self.CellSelect := CellSelect;
      Self.HideFocusRect := HideFocusRect;
      Self.HideSelection := HideSelection;
      Self.InvertSelect := InvertSelect;
      Self.MultiSelect := MultiSelect;
    end
  else
    inherited Assign(Source);
end;

function TcxTreeListOptionsSelection.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

{ TcxTreeListOptionsBehavior }

constructor TcxTreeListOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BestFitMaxRecordCount := -1;
  ConfirmDelete := True;
  CopyCaptionsToClipboard := True;
  DragCollapse := True;
  DragExpand := True;
  ShowHourGlass := True;
  Sorting := True;
  MultiSort := True;
  WaitForExpandNodeTime := 500;
  ExpandOnDblClick := True;
end;

procedure TcxTreeListOptionsBehavior.Assign(Source: TPersistent);
var
  ASource: TcxTreeListOptionsBehavior;
begin
  if Source is TcxTreeListOptionsBehavior then
  begin
    ASource := TcxTreeListOptionsBehavior(Source);
    FAutoDragCopy := ASource.AutoDragCopy;
    FBestFitMaxRecordCount := ASource.BestFitMaxRecordCount;
    FConfirmDelete := ASource.ConfirmDelete;
    FCopyCaptionsToClipboard := ASource.CopyCaptionsToClipboard;
    FDragCollapse := ASource.DragCollapse;
    FDragExpand := ASource.DragExpand;
    FDragFocusing := ASource.DragFocusing;
    FEditAutoHeight := ASource.EditAutoHeight;
    FExpandOnDblClick := ASource.ExpandOnDblClick;
    FExpandOnIncSearch := ASource.ExpandOnIncSearch;
    FFooterHints := ASource.FooterHints;
    FHeaderHints := ASource.HeaderHints;
    FHotTrack := ASource.HotTrack;
    FMultiSort := ASource.MultiSort;
    FShowHourGlass := ASource.ShowHourGlass;
    FSorting := ASource.Sorting;
    ChangeDelay := ASource.ChangeDelay;
    RecordScrollMode := ASource.RecordScrollMode;
    WaitForExpandNodeTime := ASource.WaitForExpandNodeTime;
    IncSearchItem := TreeList.GetSameColumn(ASource.IncSearchItem);
  end;
  inherited Assign(Source);
end;

procedure TcxTreeListOptionsBehavior.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AutomateLeftMostIndent', ReadBoolean, nil, True);
end;

function TcxTreeListOptionsBehavior.GetChangeDelay: Integer;
begin
  Result := TreeList.FDelayTimer.Interval;
end;

function TcxTreeListOptionsBehavior.GetIncSearchItem: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(inherited IncSearchItem);
end;

function TcxTreeListOptionsBehavior.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

procedure TcxTreeListOptionsBehavior.ReadBoolean(AReader: TReader);
begin
  AReader.ReadBoolean;
end;

procedure TcxTreeListOptionsBehavior.SetChangeDelay(AValue: Integer);
begin
  AValue := Max(0, AValue);
  TreeList.ChangeDelayTimer.Enabled := AValue > 0;
  TreeList.ChangeDelayTimer.Interval := AValue;
end;

procedure TcxTreeListOptionsBehavior.SetEditAutoHeight(AValue: TcxInplaceEditAutoHeight);
begin
  if FEditAutoHeight <> AValue then
  begin
    FEditAutoHeight := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsBehavior.SetFooterHints(
  AValue: Boolean);
begin
  if FFooterHints <> AValue then
  begin
    FFooterHints := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsBehavior.SetHeaderHints(
  AValue: Boolean);
begin
  if FHeaderHints <> AValue then
  begin
    FHeaderHints := AValue;
    Changed;
  end;
end;

procedure TcxTreeListOptionsBehavior.SetIncSearchItem(AValue: TcxTreeListColumn);
begin
  inherited IncSearchItem := AValue;
end;

procedure TcxTreeListOptionsBehavior.SetRecordScrollMode(AValue: TcxRecordScrollMode);
begin
  if AValue <> FRecordScrollMode then
  begin
    FRecordScrollMode := AValue;
    Changed;
  end;
end;

{ TcxTreeListOptionsData }

constructor TcxTreeListOptionsData.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDeleting := True;
  FMultiThreadedSorting := bDefault;
  FAppending := False;
end;

procedure TcxTreeListOptionsData.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListOptionsData then
  begin
    FAppending := TcxTreeListOptionsData(Source).FAppending;
    FDeleting := TcxTreeListOptionsData(Source).FDeleting;
    FInserting := TcxTreeListOptionsData(Source).FInserting;
    FImmediatePost := TcxTreeListOptionsData(Source).ImmediatePost;
    FMultiThreadedSorting := TcxTreeListOptionsData(Source).MultiThreadedSorting;
    SummaryNullIgnore := TcxTreeListOptionsData(Source).SummaryNullIgnore;
  end;
  inherited Assign(Source);
end;

procedure TcxTreeListOptionsData.Changed;
begin
  inherited Changed;
  TcxCustomTreeList(GetOwner).RefreshNavigatorButtons;
end;

function TcxTreeListOptionsData.GetAnsiSort: Boolean;
begin
  Result := dcoAnsiSort in
    TcxCustomTreeList(GetOwner).DataController.Options;
end;

function TcxTreeListOptionsData.GetCaseInsensitive: Boolean;
begin
  Result := dcoCaseInsensitive in
    TcxCustomTreeList(GetOwner).DataController.Options;
end;

procedure TcxTreeListOptionsData.SetAnsiSort(Value: Boolean);
begin
  with TcxCustomTreeList(GetOwner).DataController do
  begin
    if Value then
      Options := Options + [dcoAnsiSort]
    else
      Options := Options - [dcoAnsiSort]
  end;
end;

procedure TcxTreeListOptionsData.SetAppending(Value: Boolean);
begin
  if FAppending <> Value then
  begin
    FAppending := Value;
    Changed;
  end;
end;

procedure TcxTreeListOptionsData.SetCaseInsensitive(Value: Boolean);
begin
  with TcxCustomTreeList(GetOwner).DataController do
  begin
    if Value then
      Options := Options + [dcoCaseInsensitive]
    else
      Options := Options - [dcoCaseInsensitive]
  end;
end;

procedure TcxTreeListOptionsData.SetDeleting(const Value: Boolean);
begin
  if FDeleting <> Value then
  begin
    FDeleting := Value;
    Changed;
  end;
end;

procedure TcxTreeListOptionsData.SetImmediatePost(const Value: Boolean);
begin
  if FImmediatePost <> Value then
  begin
    FImmediatePost := Value;
    Changed;
  end;
end;

procedure TcxTreeListOptionsData.SetInserting(const Value: Boolean);
begin
  if FInserting <> Value then
  begin
    FInserting := Value;
    FAppending := Value;
    Changed;
  end;
end;

procedure TcxTreeListOptionsData.SetSummaryNullIgnore(Value: Boolean);
begin
  if FSummaryNullIgnore <> Value then
  begin
    FSummaryNullIgnore := Value;
    TcxCustomTreeList(GetOwner).Summary.Recalculate;
  end;
end;

{ TcxTreeListPreview }

constructor TcxTreeListPreview.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoHeight := True;
  FLeftIndent := 5;
  FMaxLineCount := 3;
  FRightIndent := 5;
end;

procedure TcxTreeListPreview.Assign(Source: TPersistent);
var
  APreview: TcxTreeListPreview;
begin
  if Source is TcxTreeListPreview then
  begin
    APreview := TcxTreeListPreview(Source);
    AutoHeight := APreview.AutoHeight;
    if APreview.Column <> nil then
      Column := TreeList.Columns[APreview.Column.ItemIndex];
    LeftIndent := APreview.LeftIndent;
    MaxLineCount := APreview.MaxLineCount;
    Place := APreview.Place;
    RightIndent := APreview.RightIndent;
    Visible := APreview.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListPreview.Changed;
begin
  TreeList.LayoutChanged;
end;

procedure TcxTreeListPreview.ChangeScale(M, D: Integer);
begin
  LeftIndent := MulDiv(LeftIndent, M, D);
  RightIndent := MulDiv(RightIndent, M, D);
end;

function TcxTreeListPreview.GetControl: TObject;
begin
  Result := TreeList;
end;

function TcxTreeListPreview.GetActive: Boolean;
begin
  Result := Visible and (FColumn <> nil) and (TreeList.VisibleColumnCount > 0) and ((MaxLineCount <> 0) or AutoHeight);
end;

function TcxTreeListPreview.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

procedure TcxTreeListPreview.SetAutoHeight(AValue: Boolean);
begin
  if AValue <> FAutoHeight then
  begin
    FAutoHeight := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetColumn(AValue: TcxTreeListColumn);
begin
  if AValue <> FColumn then
  begin
    FColumn := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetLeftIndent(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FLeftIndent then
  begin
    FLeftIndent := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetMaxLineCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FMaxLineCount then
  begin
    FMaxLineCount := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetPlace(AValue: TcxTreeListPreviewPlace);
begin
  if AValue <> FPlace then
  begin
    FPlace := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetRightIndent(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FRightIndent then
  begin
    FRightIndent := AValue;
    Changed;
  end;
end;

procedure TcxTreeListPreview.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

{ TcxTreeListPainter }

procedure TcxTreeListPainter.DoPaint;
var
  I: Integer;
  AClipRgn: TcxRegion;
begin
  if not Canvas.RectVisible(TreeList.ClientBounds) then
  begin
    DrawNavigator;
    Exit;
  end;
  if (TreeList.FChanges - SelectionChanges <> []) then
    TreeList.ValidateStates;
  if TreeList.IsLocked and (TreeList.Changes - SelectionChanges <> []) then
    inherited DoPaint
  else
  begin
    CheckCustomDrawSupport;
    Cells.Paint(Canvas, CustomDrawCell);
    DrawFindPanel;
    DrawFilterBox;
    DrawNavigator;
    AClipRgn := Canvas.GetClipRegion();
    try
      Canvas.IntersectClipRect(ViewInfo.ContentBounds);
      for I := 0 to ViewInfo.Count - 1 do
        ViewInfo.NodeViewData[I].Paint(Canvas, CustomDrawCell);
    finally
      Canvas.SetClipRegion(AClipRgn, roSet);
    end;
  end;
end;

procedure TcxTreeListPainter.CheckCustomDrawSupport;
begin
  with TreeList do
  begin
    FCustomDrawSupported := Assigned(OnCustomDrawBackgroundCell) or
      (OptionsView.Headers and Assigned(OnCustomDrawHeaderCell)) or
      (OptionsView.Bands and  Assigned(OnCustomDrawBandHeaderCell)) or
      (OptionsView.Footer and Assigned(OnCustomDrawFooterCell)) or
      (OptionsView.Indicator and Assigned(OnCustomDrawIndicatorCell)) or
       Assigned(OnCustomDrawBandCell) or Assigned(OnCustomDrawIndentCell) or
       Assigned(OnCustomDrawDataCell) or Assigned(OnCustomDrawPreviewCell);
  end;
end;

procedure TcxTreeListPainter.CustomDrawCell(
  ACanvas: TcxCanvas; ACell: TcxCustomViewInfoItem; var ADone: Boolean);
begin
  ADone := False;
  if not CustomDrawSupported then Exit;
  with TreeList do
  begin
    case ACell.CustomDrawID of
      cxtlBackgroundCell:
        if Assigned(OnCustomDrawBackgroundCell) then
          OnCustomDrawBackgroundCell(TreeList, ACanvas, TcxTreeListCustomCellViewInfo(ACell), ADone);
      cxtlColumnHeaderCell:
        if Assigned(OnCustomDrawHeaderCell) then
          OnCustomDrawHeaderCell(TreeList, ACanvas, TcxTreeListHeaderCellViewInfo(ACell), ADone);
      cxtlBandHeaderCell:
        if Assigned(OnCustomDrawBandHeaderCell) then
          OnCustomDrawBandHeaderCell(TreeList, ACanvas, TcxTreeListHeaderCellViewInfo(ACell), ADone);
      cxtlBandPartCell:
        if Assigned(OnCustomDrawBandCell) then
          OnCustomDrawBandCell(TreeList, ACanvas, TcxTreeListBandCellViewInfo(ACell), ADone);
      cxtlEditCell:
        if Assigned(OnCustomDrawDataCell) then
          OnCustomDrawDataCell(TreeList, ACanvas, TcxTreeListEditCellViewInfo(ACell), ADone);
      cxtlFooterCell:
        if Assigned(OnCustomDrawFooterCell) then
          OnCustomDrawFooterCell(TreeList, ACanvas, TcxTreeListFooterCellViewInfo(ACell), ADone);
      cxtlIndentCell:
        if Assigned(OnCustomDrawIndentCell) then
          OnCustomDrawIndentCell(TreeList, ACanvas, TcxTreeListIndentCellViewInfo(ACell), ADone);
      cxtlIndicatorCell:
        if Assigned(OnCustomDrawIndicatorCell) then
          OnCustomDrawIndicatorCell(TreeList, ACanvas, TcxTreeListIndicatorCellViewInfo(ACell), ADone);
      cxtlPreviewCell:
        if Assigned(OnCustomDrawPreviewCell) then
          OnCustomDrawPreviewCell(TreeList, ACanvas, TcxTreeListEditCellViewInfo(ACell), ADone);
    end;
  end;
end;

function TcxTreeListPainter.GetCells: TcxCustomControlCells;
begin
  Result := ViewInfo.Cells;
end;

function TcxTreeListPainter.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Control);
end;

function TcxTreeListPainter.GetViewInfo: TcxTreeListViewInfo;
begin
  Result := TcxTreeListViewInfo(inherited ViewInfo);
end;

{ TcxTreeListStyleSheet }

function TcxTreeListStyleSheet.GetStylesValue: TcxTreeListStyles;
begin
  Result := TcxTreeListStyles(GetStyles)
end;

procedure TcxTreeListStyleSheet.SetStylesValue(Value: TcxTreeListStyles);
begin
  SetStyles(Value);
end;

class function TcxTreeListStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxTreeListStyles;
end;

{ TcxTreeListStyles }

constructor TcxTreeListStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
  FUseOddEvenStyles := bDefault;
end;

procedure TcxTreeListStyles.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxTreeListStyles then
  begin
    for I := tlsv_BandBackground to tlsv_Preview do
      SetValue(I, TcxTreeListStyles(Source).GetValue(I));
    FUseOddEvenStyles := TcxTreeListStyles(Source).FUseOddEvenStyles;
  end;
  inherited Assign(Source);
end;

function TcxTreeListStyles.GetBandBackgroundParams(
  ABand: TcxTreeListBand): TcxViewParams;
begin
  ABand.Styles.GetViewParams(tlbs_HeaderBackground, ABand, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetBandContentParams(ABand: TcxTreeListBand;
  ANode: TcxTreeListNode): TcxViewParams;
begin
  Result := ABand.Styles.GetContentParams(ANode, nil);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetBandFooterParams(
  ABand: TcxTreeListBand; ANode: TcxTreeListNode): TcxViewParams;
begin
  ABand.Styles.GetViewParams(tlbs_Footer, ANode, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetBandHeaderParams(
  ABand: TcxTreeListBand): TcxViewParams;
begin
  if ABand = nil then
    DoGetBandHeaderParams(nil, Result)
  else
    ABand.Styles.GetViewParams(tlbs_Header, nil, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetColumnFooterParams(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem = nil): TcxViewParams;
begin
  DoGetColumnFooterParams(AColumn, ANode, ASummaryItem, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetColumnHeaderParams(
  AColumn: TcxTreeListColumn): TcxViewParams;
begin
  if AColumn = nil then
    DoGetColumnHeaderParams(nil, Result)
  else
    AColumn.Styles.GetViewParams(tlcs_Header, AColumn, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetContentParams(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn): TcxViewParams;
var
  AStyle: TcxStyle;
  ACellPos: TcxTreeListCellPos;
begin
  AStyle := nil;
  if (ANode <> nil) and ANode.HotTrack and not TreeList.IsEditing and ((AColumn = nil) or not AColumn.IsPreview) then
  begin
    if Assigned(FOnGetHotTrackStyle) then
      FOnGetHotTrackStyle(TreeList, AColumn, ANode, AStyle);
    FillChar(Result, SizeOf(Result), 0);
    ACellPos := TcxTreeListCellPos.Create(ANode, AColumn);
    try
      GetViewParams(tlsv_HotTrack, ACellPos, AStyle, Result);
    finally
      ACellPos.Free;
    end;
  end
  else
    Result := AColumn.Styles.GetContentParams(ANode);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
  PrepareConditionalFormattingParams(ANode, AColumn, Result);
end;

function TcxTreeListStyles.GetFooterParams: TcxViewParams;
begin
  GetViewParams(tlsv_Footer, nil, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetIncSearchParams: TcxViewParams;
begin
  GetViewParams(tlsv_IncSearch, nil, nil, Result);
end;

function TcxTreeListStyles.GetIndentParams(
  ANode: TcxTreeListNode; AIndent: Integer): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetContentStyle) then
    FOnGetContentStyle(TreeList, nil, ANode, AStyle);
  if Assigned(FOnGetNodeIndentStyle) then
    FOnGetNodeIndentStyle(TreeList, ANode, AIndent, AStyle);
  if not TreeList.OptionsView.UseNodeColorForIndent and (ANode <> nil) then
  begin
    AIndent := ANode.Level - AIndent + Byte(TreeList.OptionsView.ShowRoot);
    while AIndent > 0 do
    begin
      ANode := ANode.Parent;
      Dec(AIndent);
    end;
  end;
  DoGetNodeContentParams(ANode, AStyle, Result);
end;

function TcxTreeListStyles.GetIndicatorParams: TcxViewParams;
begin
  GetViewParams(tlsv_Indicator, nil, nil, Result);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.GetPreviewParams(ANode: TcxTreeListNode): TcxViewParams;
begin
  Result := GetContentParams(ANode, TreeList.Preview.Column);
  CanOffsetContent := CanOffsetContent and (Result.Bitmap = nil);
end;

function TcxTreeListStyles.ActuallyUseOddEvenStyles: Boolean;
begin
  Result := ((UseOddEvenStyles = bDefault) and (Assigned(ContentOdd) or Assigned(ContentEven) or
    LookAndFeelPainter.GridLikeControlDefaultUseOddEvenStyle)) or (UseOddEvenStyles = bTrue);
end;

procedure TcxTreeListStyles.DoGetBandBackgroundParams(
  ABand: TcxTreeListBand; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetBandBackgroundStyle) then
    FOnGetBandBackgroundStyle(TreeList, ABand, AStyle);
  GetViewParams(tlsv_BandBackground, ABand, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetBandContentParams(ABand: TcxTreeListBand;
  ANode: TcxTreeListNode; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ACellPos: TcxTreeListCellPos;
begin
  AStyle := nil;
  if Assigned(FOnGetBandContentStyle) then
    FOnGetBandContentStyle(TreeList, ABand, ANode, AStyle);
  if GetValue(tlsv_BandContent) <> nil then
  begin
    ACellPos := TcxTreeListCellPos.Create(ANode, ABand);
    try
      GetViewParams(tlsv_BandContent, ACellPos, AStyle, AParams)
    finally
      ACellPos.Free;
    end;
  end
  else
    DoGetNodeContentParams(ANode, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetBandFooterParams(ABand: TcxTreeListBand;
  ANode: TcxTreeListNode; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetBandFooterStyle) then
    FOnGetBandFooterStyle(TreeList, ABand, ANode, AStyle);
  GetViewParams(tlsv_Footer, ABand, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetBandHeaderParams(
  ABand: TcxTreeListBand; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetBandHeaderStyle) then
    FOnGetBandHeaderStyle(TreeList, ABand, AStyle);
  GetViewParams(tlsv_BandHeader, ABand, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetColumnFooterParams(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetColumnFooterStyle) then
    FOnGetColumnFooterStyle(TreeList, AColumn, ANode, ASummaryItem, AStyle);
  if AColumn <> nil then
    AColumn.Styles.GetViewParams(tlcs_Footer, ANode, AStyle, AParams)
  else
    GetViewParams(tlsv_ColumnFooter, AColumn, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetColumnHeaderParams(
  AColumn: TcxTreeListColumn; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetColumnHeaderStyle) then
    FOnGetColumnHeaderStyle(TreeList, AColumn, AStyle);
  GetViewParams(tlsv_ColumnHeader, AColumn, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetContentParams(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if AColumn.IsPreview then
  begin
    DoGetPreviewParams(ANode, AParams);
    Exit;
  end
  else
  begin
    if Assigned(FOnGetContentStyle) and (ANode <> nil)  then
      FOnGetContentStyle(TreeList, AColumn, ANode, AStyle);
    if (AStyle = nil) and (AColumn <> nil) and (AColumn.Position.Band <> nil) and Assigned(FOnGetBandContentStyle) then
      FOnGetBandContentStyle(TreeList, AColumn.Position.Band, ANode, AStyle);
    DoGetNodeContentParams(ANode, AStyle, AParams);
  end;
end;

procedure TcxTreeListStyles.DoGetNodeContentParams(
  ANode: TcxTreeListNode; AStyle: TcxStyle; var AParams: TcxViewParams);
begin
  if (ANode <> nil) and (GetValue(OddEvenStyleIndexes[Odd(ANode.VisibleIndex)]) <> nil) and ActuallyUseOddEvenStyles then
    GetViewParams(OddEvenStyleIndexes[Odd(ANode.VisibleIndex)], ANode, AStyle, AParams)
  else
    GetViewParams(ecs_Content, ANode, AStyle, AParams);
end;

procedure TcxTreeListStyles.DoGetPreviewParams(
  ANode: TcxTreeListNode; var AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetPreviewStyle) then
    FOnGetPreviewStyle(TreeList, ANode, AStyle);
  GetViewParams(tlsv_Preview, ANode, AStyle, AParams);
end;

procedure TcxTreeListStyles.GetDefaultViewParams(
  Index: Integer; AData: TObject; out AParams: TcxViewParams);
var
  ANode: TcxTreeListNode;
begin
  inherited GetDefaultViewParams(Index, AData, AParams);

  if Index = tlsv_Content then
  begin
    if AData is TcxTreeListCellPos then
      ANode := TcxTreeListCellPos(AData).Node
    else
      if AData is TcxTreeListNode then
        ANode := TcxTreeListNode(AData)
      else
        ANode := nil;

    if (ANode <> nil) and ActuallyUseOddEvenStyles then
      Index := OddEvenStyleIndexes[Odd(ANode.VisibleIndex)];
  end
  else
    if ((Index = tlsv_ContentEven) or (Index = tlsv_ContentOdd)) and not ActuallyUseOddEvenStyles then
      Index := tlsv_Content;

  with LookAndFeelPainter, AParams do
  begin
    Font := TcxCustomTreeList(Control).Font;
    case Index of
      tlsv_Preview:
        begin
          Color := GridLikeControlContentColor;
          TextColor := DefaultPreviewTextColor;
        end;
      tlsv_HotTrack:
       begin
         with TcxTreeListCellPos(AData) do
           AParams := TcxTreeListColumn(Item).Styles.GetContentParams(Node);
         TextColor := clHighlightText;
       end;
      tlsv_Content, tlsv_BandContent:
        begin
          Color := GridLikeControlContentColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      tlsv_ContentOdd:
        begin
          Color := GridLikeControlContentOddColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      tlsv_ContentEven:
        begin
          Color := GridLikeControlContentEvenColor;
          TextColor := GridLikeControlContentTextColor;
        end;
      tlsv_BandHeader, tlsv_Indicator, tlsv_ColumnHeader:
        begin
          Color := DefaultHeaderColor;
          TextColor := DefaultHeaderTextColor;
        end;
      tlsv_Footer, tlsv_ColumnFooter:
        begin
          Color := DefaultFooterColor;
          TextColor := DefaultFooterTextColor;
        end;
      tlsv_BandBackground:
        begin
          Color := DefaultHeaderBackgroundColor;
          TextColor := DefaultHeaderBackgroundTextColor;
        end;
      tlsv_IncSearch:
        begin
          Color := DefaultSelectionColor;
          TextColor := DefaultSelectionTextColor;
        end;
      ecs_Inactive, ecs_Selection:
        if AData <> nil then
          Font := TcxTreeListColumn(TcxTreeListCellPos(AData).Item).Styles.GetContentParams(TcxTreeListCellPos(AData).Node).Font;
    end;
  end;
end;

function TcxTreeListStyles.GetCellSelectionParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TcxViewParams;
var
  ACellPos: TcxTreeListCellPos;
begin
  ACellPos := TcxTreeListCellPos.Create(ANode, AColumn);
  try
  if TreeList.Controller.Focused or TreeList.IsFocused then
    GetViewParams(ecs_Selection, ACellPos, nil, Result)
  else
    GetViewParams(ecs_Inactive, ACellPos, nil, Result);
  PrepareConditionalFormattingParams(ANode, AColumn, Result);
  finally
    ACellPos.Free;
  end;
end;

procedure TcxTreeListStyles.PrepareConditionalFormattingParams(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AParams: TcxViewParams);
begin
  if (ANode <> nil) and (AColumn <> nil) then
    TreeList.ConditionalFormattingProvider.CalculateViewParams(AParams, ANode.AbsoluteIndex, AColumn.ItemIndex);
end;

function TcxTreeListStyles.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TreeList.LookAndFeel;
end;

function TcxTreeListStyles.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

procedure TcxTreeListStyles.SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
begin
  if AValue <> FUseOddEvenStyles then
  begin
    FUseOddEvenStyles := AValue;
    Changed(ecs_Content);
  end;
end;

{ TcxTreeListViewInfo }

constructor TcxTreeListViewInfo.Create(AOwner: TcxEditingControl);
begin
  inherited Create(AOwner);
  FHitTestCells := TcxObjectList.Create;
  FHeaderCells := TList.Create;
  FFakeCell := TcxFakeCellViewInfo.Create(Self);
  FCells := TcxCustomControlCells.Create;
  FNodesViewData := TcxObjectList.Create;
  FCheckBoxProperties := TcxCheckBoxProperties.Create(nil);
  TcxCheckBoxProperties(FCheckBoxProperties).AllowGrayed := True;
  TcxCheckBoxProperties(FCheckBoxProperties).ValueChecked := Integer(cbsChecked);
  TcxCheckBoxProperties(FCheckBoxProperties).ValueUnchecked := Integer(cbsUnchecked);
  TcxCheckBoxProperties(FCheckBoxProperties).ValueGrayed := Integer(cbsGrayed);
  TcxCheckBoxPropertiesAccess(FCheckBoxProperties).ScaleFactor.Owner := ScaleFactor;
  FCheckBoxViewInfo := TcxCustomEditViewInfo(FCheckBoxProperties.GetViewInfoClass.Create);
  RecreateCheckBoxViewData;
end;

destructor TcxTreeListViewInfo.Destroy;
begin
  Clear;
  FreeAndNil(FCheckBoxViewData);
  FreeAndNil(FCheckBoxViewInfo);
  FreeAndNil(FCheckBoxProperties);
  FreeAndNil(FHeaderCells);
  FreeAndNil(FNodesViewData);
  FreeAndNil(FFakeCell);
  FreeAndNil(FCells);
  FreeAndNil(FHitTestCells);
  inherited Destroy;
end;

function TcxTreeListViewInfo.GetEditCell(ANode: TcxTreeListNode;
  AColumn: TcxTreeListColumn): TcxTreeListEditCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if (ANode = nil) or (ANode.ViewData = nil) then Exit;
  for I := 0 to ANode.ViewData.Cells.Count - 1 do
    if (TObject(ANode.ViewData.Cells[I]) is TcxTreeListEditCellViewInfo) and
      (TcxTreeListEditCellViewInfo(ANode.ViewData.Cells[I]).Column = AColumn) then
    begin
      Result := TcxTreeListEditCellViewInfo(ANode.ViewData.Cells[I]);
      Break;
    end;
end;

function TcxTreeListViewInfo.Validate: Boolean;
begin
  Result := (IsDirty or (TreeList.Changes - SelectionChanges <> [])) and
    not (CalculateInProcess or TreeList.IsLocked);
  if Result then
    TreeList.ForceLayoutChanged;
  if IsDirty then
    Clear;
  if (((Count = 0) and (TreeList.Root.Count > 0)) and not TreeList.IsLocked) or
    (IsDirty and (TreeList.Changes - SelectionChanges = [])) then
  begin
    Invalidate;
    if TreeList.Changes <> [] then
      DoCalculate;
  end;
  if Cells.Count = 0 then
    DoCalculate;
end;


function TcxTreeListViewInfo.AddBackgroundPart(
  const ABounds: TRect): TcxTreeListBackgroundCellViewInfo;
begin
  AddCell(TcxTreeListBackgroundCellViewInfo, ABounds, ABounds, Result);
  Result.ItemViewParams := Styles.GetBackgroundParams;
end;

function TcxTreeListViewInfo.AddBandHeader(ABand: TcxTreeListBand;
  var ABounds: TRect; const AClipRect: TRect): TcxTreeListBandHeaderCellViewInfo;
var
  R: TRect;
begin
  R := ABounds;
  R.Right := ABounds.Left + ABand.DisplayWidth;
  R.Bottom := R.Top + ABand.LineCount * BandLineHeight;
  AddCell(TcxTreeListBandHeaderCellViewInfo, R, AClipRect, Result);
  Result.Initialize(ABand.Caption);
  ABounds.Left := R.Right;
  if cxRectHeight(ABounds) > 0 then
    AddHeaderHitTestAreas(Result, ABand.CanSizing(dsdHorz), ABand.CanSizing(dsdVert));
  Include(Result.FBorders, bRight);
  FHeaderCells.Add(Result);
end;

function TcxTreeListViewInfo.AddBandPart(APart: TcxTreeListBandPart;
  ABand: TcxTreeListBand; ANode, AAttachNode: TcxTreeListNode;
  ABounds, AClipRect: TRect): TcxTreeListBandCellViewInfo;
begin
  if APart = tlbpContent then
    InflateBoundsForGridLines(ABounds, AClipRect);
  AddCell(TcxTreeListBandCellViewInfo, ABounds, AClipRect, Result);
  if APart = tlbpContent then
    Result.FBorders := GridLines;
  if APart = tlbpHeader then
    Include(Result.FBorders, bRight);
  Result.Initialize(ABand, ANode, AAttachNode, APart);
end;

function TcxTreeListViewInfo.AddBandSeparator(ANode: TcxTreeListNode;
  var ABounds: TRect; const AClipBounds: TRect): TcxTreeListBandCellViewInfo;
begin
  Result := AddBandPart(tlbpSeparator, nil, ANode, ANode,
    cxRectSetWidth(ABounds, FixedSeparatorWidth), AClipBounds);
  Result.FBorders := [];
  Inc(ABounds.Left, FixedSeparatorWidth);
end;

procedure TcxTreeListViewInfo.AddBandsHeaders(
  AKind: TcxTreeListBandFixedKind; const ABounds, AClipBounds: TRect);

  procedure AddSubItems(AParent: TcxTreeListBand; var R: TRect);
  var
    I: Integer;
    ASubBounds: TRect;
  begin
    if not AParent.Visible then Exit;
    ASubBounds := R;
    R.Bottom := R.Top + AParent.LineCount * BandLineHeight;
    AddBandHeader(AParent, R, AClipBounds);
    ASubBounds.Top := R.Bottom;
    for I := 0 to AParent.ChildBandCount - 1 do
      AddSubItems(AParent.ChildBands[I], ASubBounds);
  end;

var
  I: Integer;
  R: TRect;
  ABand: TcxTreeListBand;
begin
  R := ABounds;
  for I := 0 to Bands.VisibleRootItemCount - 1 do
  begin
    ABand := Bands.VisibleRootItems[I];
    if ABand.FixedKind = AKind then
      AddSubItems(ABand, R);
  end;
end;

procedure TcxTreeListViewInfo.AddCell(
  ACellClass: TcxTreeListCustomCellViewInfoClass;
  const ABounds, AVisibleRect: TRect; var AInstance);
begin
  Pointer(AInstance) := ACellClass.CreateEx(TreeList, ABounds, AVisibleRect);
  TcxTreeListCustomCellViewInfo(AInstance).FBorderColor := GridLineColor;
  Cells.Add(Pointer(AInstance));
end;

function TcxTreeListViewInfo.AddColumnHeader(AColumn: TcxTreeListColumn;
  var ABounds: TRect; const AClipBounds: TRect): TcxTreeListColumnHeaderCellViewInfo;
var
  R: TRect;
begin
  R := ABounds;
  R.Right := R.Left + AColumn.DisplayWidth;
  AddCell(TcxTreeListColumnHeaderCellViewInfo, R, AClipBounds, Result);
  Result.Initialize(AColumn.Caption);
  ABounds.Left := R.Right;
  FHeaderCells.Add(Result);
  if cxRectHeight(ABounds) > 0 then
     AddHeaderHitTestAreas(Result, AColumn.CanSizing(dsdHorz), AColumn.CanSizing(dsdVert));
end;

function TcxTreeListViewInfo.AddEditCell(AViewData: TcxTreeListNodeViewData;
  AColumn: TcxTreeListColumn; ABounds, AClipRect: TRect;
  AClass: TcxEditCellViewInfoClass): TcxTreeListEditCellViewInfo;
begin
  InflateBoundsForGridLines(ABounds, AClipRect);
  Result := TcxTreeListEditCellViewInfo(AClass.Create(AColumn));
  Result.FNodeViewData := AViewData;
  Result.CellBorders := GridLines;
  Result.BorderColor := GridLineColor;
  Result.CheckClipping(ABounds, AClipRect);
  AColumn.InitEditViewInfo(Result);
  Result.ViewData.UseRightToLeftAlignment := UseRightToLeftAlignment;
  Result.ViewData.UseRightToLeftReading := UseRightToLeftReading;
  Result.ViewData.UseRightToLeftScrollBar := UseRightToLeftScrollBar;
  Result.CheckVisibleInfo;
  Cells.Add(Result);
end;

procedure TcxTreeListViewInfo.AddColumnFooterItems(
  ADataNode, AttachNode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; const ABounds, AClipRect: TRect);
var
  I: Integer;
  R: TRect;
  AMultiLine: Boolean;
  AItems: TcxTreeListSummaryItems;
  ACell: TcxTreeListFooterCellViewInfo;
  AOffset: Integer;
begin
  R := cxRectSetTop(ABounds, AClipRect.Top +
    AColumn.Position.Row.LineOffset * FooterLineHeight,
    AColumn.Position.LineCount * FooterLineHeight);
  AOffset := 0;
  if not IsPrinting then
  begin
    InflateRect(R, -Painter.FooterCellOffset, 0);
    Dec(R.Bottom);
    AOffset := Painter.FooterCellOffset;
  end;
  if ADataNode = Root then
  begin
    AItems := AColumn.Summary.FooterSummaryItems;
    AMultiLine := not MultiRows and (Summary.FooterSummaryRowCount > 1);
  end
  else
    begin
      AItems := AColumn.Summary.GroupFooterSummaryItems;
      AMultiLine := not MultiRows and (Summary.GroupFooterSummaryRowCount > 1);
    end;
  if not AMultiLine or (AItems.Count = 0) then
  begin
    AddCell(TcxTreeListFooterMultiItemsCellViewInfo,
      cxRectSetHeight(R, cxRectHeight(R) - AOffset) , AClipRect, ACell);
    TcxTreeListFooterMultiItemsCellViewInfo(ACell).Initialize(ADataNode, AttachNode, AColumn, AItems);
  end
  else
    for I := 0 to AItems.Count - 1 do
      if AItems[I].Visible then
      begin
        AddCell(TcxTreeListFooterSingleCellViewInfo, cxRectSetHeight(R, cxRectHeight(R) - AOffset), AClipRect, ACell);
        TcxTreeListFooterSingleCellViewInfo(ACell).Initialize(ADataNode, AttachNode, AColumn, AItems[I]);
        OffsetRect(R, 0, FooterLineHeight * AColumn.Position.LineCount);
      end;
end;

procedure TcxTreeListViewInfo.AddFixedSeparators(ABounds: TRect;
  AAttachNode: TcxTreeListNode);
var
  R: TRect;
begin
  if not HasFixedSeparator then Exit;
  R := cxRectSetWidth(ABounds, FixedSeparatorWidth);
  if Bands.VisibleLeftFixedCount > 0 then
  begin
    R := cxRectSetLeft(R, ContentParts[tlbfLeft].Right);
    AddBandPart(tlbpSeparator, nil, AAttachNode, AAttachNode, R, R);
  end;
  if (Bands.VisibleRootRightFixedCount > 0) and ((R.Right <= ContentParts[tlbfRight].Left) or
    (Bands.VisibleRootLeftFixedCount = 0)) then
  begin
    R := cxRectSetRight(R, ContentParts[tlbfRight].Left);
    if R.Left < IndicatorWidth then
      R.Left := IndicatorWidth;
    if R.Left >= R.Right then Exit;
    AddBandPart(tlbpSeparator, nil, AAttachNode, AAttachNode, R, R);
  end;
end;

procedure TcxTreeListViewInfo.AddFooter(AAttachNode: TcxTreeListNode;
  AIndex, AIndent: Integer; const ABounds: TRect);
var
  I, J: Integer;
  ABand: TcxTreeListBand;
  AClipRect, ACellsClipRect, R: TRect;
  AColumn: TcxTreeListColumn;
  ADataNode: TcxTreeListNode;
  APart: TcxTreeListBandCellViewInfo;
const
  BandPart: array[Boolean] of tcxTreeListBandPart = (tlbpGroupFooter, tlbpFooter);
begin
  ADataNode := AAttachNode;
  if AAttachNode = Root then
    AAttachNode := nil;
  for I := 0 to AIndex - 1 do
    ADataNode := ADataNode.Parent;
  for I := 0 to Bands.BottomItemCount - 1 do
  begin
    ABand := Bands.BottomItems[I];
    R := cxRectSetYPos(ABand.HeaderCell.DisplayRect,
      ABounds.Top, ABounds.Bottom);
    AClipRect := cxRectSetYPos(ABand.HeaderCell.VisibleRect,
      ABounds.Top, ABounds.Bottom);
    if (ADataNode <> Root) and (ABand = Bands.ExpandableBand) then
    begin
      Inc(R.Left, AIndent);
      if I = 0 then
        AClipRect.Left := Min(AClipRect.Left, R.Left);
    end;
    ACellsClipRect := R;
    if (I = 0) and not IsPrinting then
      Inc(ACellsClipRect.Left);
    if I = Bands.BottomItemCount - 1 then
      Dec(ACellsClipRect.Right);
    APart := AddBandPart(BandPart[ADataNode = Root], ABand, ADataNode, AAttachNode, R, AClipRect);
    APart.FBorders := [bTop, bBottom];
    APart.BorderColor := Painter.FooterSeparatorColor;
    if (Bands.ExpandableBand = ABand) and (APart.Part = tlbpGroupFooter) then
      Include(APart.FBorders, bLeft);
    if (I = (Bands.BottomItemCount - 1)) or (APart.Part = tlbpGroupFooter) and
      (Bands.BottomItems[I + 1] = Bands.ExpandableBand) then
      Include(APart.FBorders, bRight);
    if not IsPrinting then
    begin
      Inc(AClipRect.Top, Painter.FooterSeparatorSize +
        Painter.FooterSeparatorSize + Painter.FooterCellOffset);
      Dec(AClipRect.Bottom, Painter.FooterBorderSize);
    end;
    for J := 0 to ABand.VisibleColumnCount - 1 do
    begin
      AColumn := ABand.VisibleColumns[J];
      if ((ADataNode = Root) and not AColumn.Options.Footer) or
        ((ADataNode <> Root) and not AColumn.Options.GroupFooter) then Continue;
      if cxRectIntersect(R, ACellsClipRect, cxRectSetYPos(AColumn.HeaderCell.DisplayRect,
        AClipRect.Top, AClipRect.Bottom)) then
      begin
        if (ABand = Bands.ExpandableBand) and (I <> 0) and not IsPrinting then
          Inc(R.Left);
        AddColumnFooterItems(ADataNode, AAttachNode, ABand.VisibleColumns[J], R, AClipRect);
      end;
    end;
  end;
  AddFixedSeparators(ABounds, AAttachNode);
end;

procedure TcxTreeListViewInfo.AddHeaderHitTestAreas(ACell: TcxTreeListHeaderCellViewInfo; AHorz, AVert: Boolean);
begin
  if AHorz then
    FHitTestCells.Add(TcxTreeListHeaderSizingArea.CreateEx(ACell, dsdHorz));
  if AVert then
    FHitTestCells.Add(TcxTreeListHeaderSizingArea.CreateEx(ACell, dsdVert));
end;

function TcxTreeListViewInfo.AddImageIndent(ANode: TcxTreeListNode; AImages: TCustomImageList;
  var ABounds: TRect; const AClipRect: TRect; ACheckDynamic: Boolean = False): TcxTreeListIndentCellViewInfo;
begin
  if not ACheckDynamic or not TreeList.OptionsView.DynamicIndent then
    ABounds.Right := ABounds.Left + LevelInfo[ANode.Level].Size.cx
  else
    ABounds.Right := ABounds.Left + dxGetImageSize(AImages, ScaleFactor).cx + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2;

  Result := AddIndent(ANode, ANode, -1, nikImage, ABounds, AClipRect);
  Result.InitializeImageIndent(AImages);
end;

function TcxTreeListViewInfo.AddIndent(ANode, AAttachNode: TcxTreeListNode;
  ALevel: Integer; AKind: TcxTreeListNodeIndentKind;
  var ABounds: TRect; const AClipRect: TRect): TcxTreeListIndentCellViewInfo;
begin
  if ALevel <> -1 then
    ABounds.Right := ABounds.Left + LevelInfo[ALevel].Size.cx
  else
    ALevel := ANode.Level;
  AddCell(TcxTreeListIndentCellViewInfo, ABounds, AClipRect, Result);
  Result.FKind := AKind;
  Result.InitializeLevelIndent(ANode, AAttachNode, ALevel);
  if AKind in GlyphIndents then
    Result.FLines := [];
  ABounds.Left := ABounds.Right;
  if (bBottom in GridLines) and ANode.IsLastVisible and ((AKind = nikFooter) or not HasFooter(ANode)) then
    Include(Result.FBorders, bBottom);
end;

function TcxTreeListViewInfo.AddIndentCheck(ANode: TcxTreeListNode;
  var ABounds: TRect; const AClipRect: TRect): TcxTreeListIndentCellViewInfo;
begin
  ABounds.Right := ABounds.Left + Max(LevelInfo[ANode.Level].Size.cx, GetCheckBoxSize.cx);
  Result := AddIndent(ANode, ANode, -1, nikCheck, ABounds, AClipRect);
end;

function TcxTreeListViewInfo.AddIndicator(ANode: TcxTreeListNode;
  APosition: TcxTreeListIndicatorPosition; var ABounds: TRect): TcxTreeListIndicatorCellViewInfo;
var
  R: TRect;
  APopup: TcxTreeListPopup;
begin
  R := ABounds;
  R.Right := R.Left + IndicatorWidth;
  AddCell(TcxTreeListIndicatorCellViewInfo, R, ABounds, Result);
  Result.Initialize(ANode, APosition);
 if APosition in [tlipBands, tlipColumns] then
 begin
   FHeaderCells.Add(Result);
   APopup := Result.GetPopup;
   if APopup <> nil then
   begin
     APopup.Owner := Result;
     Result.Pressed := APopup.Visible;
   end;
 end;
  ABounds.Left := R.Right;
  if (ANode <> nil) and (TreeList.OptionsCustomizing.NodeSizing or
    TreeList.OptionsCustomizing.RowSizing) then
  begin
    Result.SizingArea := TcxTreeListNodeSizingArea.Create(Result);
    FHitTestCells.Add(Result.SizingArea);
  end;
end;

procedure TcxTreeListViewInfo.AddNodeBands(AViewData: TcxTreeListNodeViewData);
var
  I: Integer;
  ABand: TcxTreeListBand;
  AClipRect, R: TRect;
begin
  AClipRect := cxRectSetTop(cxNullRect, RowOffset[0], AViewData.RowsHeight);
  for I := 0 to Bands.BottomItemCount - 1 do
  begin
    ABand := Bands.BottomItems[I];
    R := cxRectSetTop(ABand.HeaderCell.DisplayRect, RowOffset[0], AViewData.RowsHeight);
    AClipRect := cxRectSetTop(ABand.HeaderCell.VisibleRect, RowOffset[0], AViewData.RowsHeight);
    if I = 0 then
      AViewData.Bounds.TopLeft := R.TopLeft;
    if ABand.ActuallyExpandable then
      Inc(R.Left, AViewData.IndentWidth + Byte(bLeft in GridLines));
    if I = 0 then
      AViewData.ContentBounds.TopLeft := R.TopLeft;
    AViewData.ContentBounds.BottomRight := R.BottomRight;
    AViewData.Bounds.BottomRight := R.BottomRight;
    AddBandPart(tlbpContent, ABand, AViewData.Node, AViewData.Node, R, AClipRect);
  end;
  AddFixedSeparators(AClipRect, AViewData.Node);
end;

procedure TcxTreeListViewInfo.AddNodeCategorized(
  AViewData: TcxTreeListNodeViewData);
var
  AHeight: Integer;
begin
  CanOffsetContent := False;
  AHeight := AViewData.Node.Height;
  if AHeight = 0 then
    AHeight := DefaultRowHeight;
  if OptionsView.CellAutoHeight then
  begin
    AHeight := Max(AHeight, LevelInfo[AViewData.Node.Level].Size.cy);
    AHeight := TcxFakeCellViewInfo(FakeCell).MeasureHeight(
      OptionsView.GetCategorizedColumn, AViewData.Node, True);
  end;
  TreeList.DoGetNodeHeight(AViewData.Node, AHeight);
  AViewData.Bounds := cxRectSetTop(ContentBounds, AViewData.Height, AHeight);
  if Bands.VisibleLeftFixedCount = 0 then
    OffsetRect(AViewData.Bounds, -HScrollPos, 0);
  AViewData.ContentBounds := AViewData.Bounds;
  Inc(AViewData.ContentBounds.Left, AViewData.IndentWidth + IndicatorWidth + Byte(bLeft in GridLines));
  AViewData.Bounds.Right := Min(AViewData.Bounds.Right, Bounds.Right) + Byte(HScrollSize > cxRectWidth(ClientRect));
  AViewData.ContentBounds.Right := AViewData.Bounds.Right;
  AddEditCell(AViewData, OptionsView.GetCategorizedColumn, AViewData.ContentBounds,
    AViewData.Bounds, TcxTreeListGroupNodeEditViewInfo);
  AViewData.RowsHeight := AHeight;
  Inc(AViewData.Height, AHeight);
end;

procedure TcxTreeListViewInfo.AddNodeColumns(AViewData: TcxTreeListNodeViewData);
var
  I: Integer;
  AClipRect, R: TRect;
  ARow: TcxTreeListBandRow;
  AColumn: TcxTreeListColumn;
begin
  for I := ColumnCount - 1 downto 0 do
  begin
    AColumn := Columns[I];
    ARow := AColumn.Position.Row;
    R := cxRectSetYPos(AColumn.HeaderCell.DisplayRect, RowOffset[ARow.LineOffset],
      RowOffset[ARow.LineOffset + AColumn.Position.LineCount]);
    if AColumn.HasIndent then
      Inc(R.Left, AViewData.IndentWidth + Byte(bLeft in GridLines));
    AClipRect := cxRectSetYPos(AColumn.HeaderCell.VisibleRect, R.Top, R.Bottom);
    AddEditCell(AViewData, AColumn, R, AClipRect, TcxTreeListEditCellViewInfo);
  end;
end;

procedure TcxTreeListViewInfo.AddNodeFooters(AViewData: TcxTreeListNodeViewData);
var
  R, ABounds: TRect;
  ANode, AAttachLevelNode: TcxTreeListNode;
  AIndex, ALevel, AOffset, AFooterLevel: Integer;
begin
  AIndex := 1;
  ANode := AViewData.Node;
  AAttachLevelNode := ANode;
  AViewData.CheckFooterRgn := [bTop, bBottom] * GridLines <> [];
  AFooterLevel := ANode.Level;
  if not ANode.HasChildren or ANode.Expanded or
    (OptionsView.GroupFooters <> tlgfAlwaysVisible) and (ANode.Parent <> Root) then
    ANode := ANode.Parent
  else
    AIndex := 0;
  if (OptionsView.GroupFooters = tlgfAlwaysVisible) and
    (ANode <> AViewData.Node) and not (ANode = Root) then
    Dec(AFooterLevel);
  repeat
    ABounds := cxRectSetTop(AViewData.Bounds, AViewData.Height - 1 +
      Byte(IsPrinting), GroupFooterHeight + 1 - Byte(IsPrinting));
    AOffset := LevelInfo[AFooterLevel].Offset;
    if OptionsView.ShowRoot or (AFooterLevel > 0) then
      Inc(AOffset, TreeList.DefaultIndentSize.cx);
    AddFooter(AViewData.Node, AIndex, AOffset, ABounds);
    Inc(AViewData.GroupFooterCount);
    if Bands.ExpandableBand <> nil then
    begin
      R := cxRectSetLeft(ABounds, Bands.ExpandableBand.HeaderCell.DisplayRect.Left);
      ABounds := cxRectSetLeft(ABounds, Bands.ExpandableBand.HeaderCell.VisibleRect.Left);
      for ALevel := Byte(not OptionsView.ShowRoot) to AFooterLevel do
        AddIndent(AViewData.Node, AViewData.Node, ALevel, nikFooter, R, ABounds);
    end;
    Inc(AViewData.Height, GroupFooterHeight);
    if ANode.GetNextSiblingVisible <> nil then Break;
    ANode := ANode.Parent;
    AAttachLevelNode := AAttachLevelNode.Parent;
    Dec(AFooterLevel);
    Inc(AIndex);
  until (ANode = nil) or (ANode = Root) {or ((AAttachLevelNode.GetNextSiblingVisible <> nil)};
end;

procedure TcxTreeListViewInfo.AddNodeIndents(AViewData: TcxTreeListNodeViewData);
var
  I, ALevel: Integer;
  ABounds, AClipRect: TRect;
  ALinesIndent, AIndent: TcxTreeListIndentCellViewInfo;
begin
  AIndent := nil;
  ALinesIndent := nil;
  ALevel := AViewData.Node.Level;
  with Bands.ExpandableBand do
  begin
    ABounds := cxRectSetHeight(HeaderCell.DisplayRect, DefaultRowHeight);
    AClipRect := cxRectSetHeight(HeaderCell.VisibleRect, DefaultRowHeight);
    AViewData.IndentWidth := ABounds.Left;
  end;
  for I := Byte(not OptionsView.ShowRoot) to ALevel do
    AIndent := AddIndent(AViewData.Node, AViewData.Node, I, nikLevel, ABounds, AClipRect);
  if AViewData.Node.HasCheckbox then
  begin
    AIndent := AddIndentCheck(AViewData.Node, ABounds, AClipRect);
    ALinesIndent := AIndent;
    AIndent.InitializeLevelIndent(AViewData.Node, AViewData.Node, ALevel);
  end;
  if HasStateIndent(AViewData.Node) then
  begin
    AIndent := AddImageIndent(AViewData.Node, LevelInfo[ALevel].StateImages, ABounds, AClipRect);
    AIndent.FKind := nikState;
    AViewData.StateImageIndent := AIndent;
    if ALinesIndent = nil then
      ALinesIndent := AIndent;
  end;
  if LevelInfo[ALevel].Images <> nil then
  begin
    AIndent := AddImageIndent(AViewData.Node, LevelInfo[ALevel].Images, ABounds, AClipRect, True);
    AViewData.ImageIndent := AIndent;
    if ALinesIndent = nil then
      ALinesIndent := AIndent;
  end;
  if ALinesIndent <> nil then
    ALinesIndent.InitializeLevelIndent(AViewData.Node, AViewData.Node, ALevel);
  if AIndent <> nil then
    AViewData.IndentWidth := AIndent.BoundsRect.Right - AViewData.IndentWidth
  else
    AViewData.IndentWidth := 0;
end;

function TcxTreeListViewInfo.AddNodePreview(AViewData: TcxTreeListNodeViewData): TcxTreeListEditCellViewInfo;
var
  AClipRect, ABounds: TRect;
  APrevHeight, AHeight: Integer;
begin
  CanOffsetContent := False;
  if Preview.MaxLineCount > 0 then
    AHeight := Preview.MaxLineCount * PreviewLineHeight + ScaleFactor.Apply(cxTextOffset) + OptionsView.VertIncrement
  else
    AHeight := ContentHeight;
  AClipRect := cxRectSetTop(ContentBounds, AViewData.Height, AHeight);
  Inc(AClipRect.Left, IndicatorWidth);
  ABounds := AClipRect;
  if IndentLeftMost then
  begin
    Inc(ABounds.Left, AViewData.IndentWidth + Byte(bLeft in GridLines));
    if Bands.VisibleLeftFixedCount = 0 then
      OffsetRect(ABounds, -HScrollPos, 0);
  end;
  Result := AddEditCell(AViewData, Preview.Column, ABounds, AClipRect,
    TcxTreeListPreviewCellViewInfo) as TcxTreeListPreviewCellViewInfo;
  APrevHeight := AHeight;
  if Preview.AutoHeight then
    AHeight := Min(AHeight, Result.CalculateEditHeight - ScaleFactor.Apply(cxTextOffset) - OptionsView.VertIncrement * 2);
  TreeList.DoGetPreviewHeight(AViewData.Node, AHeight);
  if AHeight < 0 then
    AHeight := 0;
  if AHeight <> APrevHeight then
  begin
    FCells.Delete(FCells.Count - 1);
    FreeAndNil(Result);
    if AHeight > 0 then
    begin
      if Preview.AutoHeight then
        Inc(AHeight, OptionsView.VertIncrement * 2 + ScaleFactor.Apply(cxTextOffset));

      ABounds := cxRectSetHeight(ABounds, AHeight);
      Result := AddEditCell(AViewData, Preview.Column, ABounds, cxRectSetHeight(AClipRect, AHeight), TcxTreeListPreviewCellViewInfo);
    end;
  end;
  AViewData.PreviewHeight := AHeight;
  Inc(AViewData.Height, AHeight);
end;

procedure TcxTreeListViewInfo.AddNodeStandardContent(
  AViewData: TcxTreeListNodeViewData);
var
  AHeight: Integer;
begin
  AHeight := AViewData.Node.Height;
  if AHeight = 0 then
  begin
    AHeight := DefaultRowHeight * ColumnsRowCount;
    if TreeList.DefaultRowHeight = 0 then
      AHeight := Max(AHeight, LevelInfo[AViewData.Node.Level].Size.cy);
  end;
  if OptionsView.CellAutoHeight then
  begin
    InitializeRows(AHeight, AViewData);
    Dec(AViewData.Height, AHeight);
    AHeight := CalculateNodeAutoHeight(AViewData);
  end;
  TreeList.DoGetNodeHeight(AViewData.Node, AHeight);
  InitializeRows(AHeight, AViewData);
  AddNodeBands(AViewData);
  AddNodeColumns(AViewData);
end;

function TcxTreeListViewInfo.AddNodeViewData(ANode: TcxTreeListNode;
  AInsert: Boolean; AFake: Boolean = False): TcxTreeListNodeViewData;
var
  R: TRect;
begin
  Result := TcxTreeListNodeViewData.Create(Self, ANode,
    ColumnCount + Byte(Preview.Active) + ANode.Level + 5, AFake);
  Result.DrawFocusRect := not TreeList.OptionsSelection.CellSelect or
    TreeList.OptionsSelection.InvertSelect;
  FCells := Result.Cells;
  if AInsert then
    NodesViewData.Insert(0, Result)
  else
    NodesViewData.Add(Result);
  if (Bands.ExpandableBand <> nil) then
     AddNodeIndents(Result);
  if Preview.Active and (Preview.Place = tlppTop) then
    AddNodePreview(Result);
  if (ColumnCount > 0) and ANode.IsGroupNode then
  begin
    Result.DrawFocusRect := False;
    AddNodeCategorized(Result);
  end
  else
    AddNodeStandardContent(Result);

  Result.FocusRectBounds := cxRectSetTop(Result.ContentBounds, Result.PreviewHeight, Result.RowsHeight - OptionsView.VertIncrement);
  if Preview.Active and (Preview.Place = tlppBottom) then
    AddNodePreview(Result);
  AdjustNodeIndents(Result);
  Result.ContentBounds := cxRectSetTop(Result.ContentBounds, 0, Result.Height - Byte(bRight in GridLines));
  if HasFooter(ANode) then
    AddNodeFooters(Result);
  if IsIndicatorVisible then
  begin
    if Bands.VisibleItemCount = 0 then
    begin
      Result.Height := DefaultRowHeight;
      Result.Bounds.Right := IndicatorWidth
    end;
    R := cxRectSetTop(Bounds, 0, Result.Height);
    AddIndicator(ANode, tlipContent, R);
  end;
  Result.Bounds := cxRectSetTop(Result.Bounds, 0, Result.Height);
  Result.Bounds.Left := Bounds.Left;
end;

procedure TcxTreeListViewInfo.AdjustNodeIndents(
  AViewData: TcxTreeListNodeViewData);
var
  I, AHeight, ATop: Integer;
begin
  ATop := 0;
  AHeight := AViewData.Height;
  if not IndentLeftMost then
  begin
    ATop := RowOffset[0];
    AHeight := AViewData.RowsHeight;
  end;
  for I := 0 to Cells.Count - 1 do
    if Cells[I] is TcxTreeListIndentCellViewInfo then
      TcxTreeListIndentCellViewInfo(Cells[I]).SetSize(ATop, AHeight);
end;

function TcxTreeListViewInfo.CalculateDefaultEditHeight: Integer;
var
  I: Integer;
begin
  Result := TreeList.FDefaultRowHeight;
  FDefaultCellHeight := 0;
  if Result = 0 then
  begin
    for I := 0 to ColumnCount - 1 do
      if not Columns[I].IsPreview then
      begin
        FDefaultCellHeight := Max(FDefaultCellHeight, GetColumnDefaultEditHeight(Columns[I]));
        Result := Max(Result, FDefaultCellHeight);
      end;
  end;
  if Preview.Active then
    FPreviewLineHeight := GetColumnDefaultEditHeight(Preview.Column)
  else
    FPreviewLineHeight := 0;
  if Result = 0 then
    Result := GetFontHeight(TreeList.Font) + ScaleFactor.Apply(cxTextOffset);
  Inc(Result, OptionsView.VertIncrement);
  FHScrollInc := cxTextWidth(TreeList.Font, 'W');
  if FDefaultCellHeight = 0 then
    FDefaultCellHeight := GetFontHeight(TreeList.Font) + ScaleFactor.Apply(cxTextOffset);
  if TreeList.FDefaultRowHeight = 0 then
  begin
    Inc(Result, cxTreeListEditCellHeightOffset);
    Result := Max(Result, TreeList.DefaultIndentSize.cy);
  end;
end;

procedure TcxTreeListViewInfo.CalculateDefaultHeights;
var
  I: Integer;
begin
  FFooterLineHeight := 0;
  FHeaderLineHeight := 0;
  if not OptionsView.Bands then
    FBandLineHeight := 0
  else
    begin
      FBandHeaderLineHeight := 0;
      for I := 0 to Bands.VisibleItemCount - 1 do
        FBandHeaderLineHeight := Max(FBandHeaderLineHeight, Painter.ScaledHeaderHeight(
          GetFontHeight(Styles.GetBandHeaderParams(Bands.VisibleItems[I]).Font), ScaleFactor));
      FBandLineHeight := OptionsView.BandLineHeight;
      if FBandLineHeight = 0 then
        FBandLineHeight := FBandHeaderLineHeight;
    end;
  for I := 0 to ColumnCount - 1 do
  begin
    if OptionsView.Headers then
      FHeaderLineHeight := Max(FHeaderLineHeight, Painter.ScaledHeaderHeight(
        GetFontHeight(Styles.GetColumnHeaderParams(Columns[I]).Font), ScaleFactor));
    if OptionsView.Footer and Columns[I].Options.Footer then
      FFooterLineHeight := Max(FFooterLineHeight, Painter.ScaledHeaderHeight(
        GetFontHeight(Styles.GetColumnFooterParams(Columns[I], Root).Font), ScaleFactor));
  end;
  if OptionsView.Footer then
  begin
    if FFooterLineHeight = 0 then
      FFooterLineHeight := Painter.ScaledHeaderHeight(GetFontHeight(TreeList.Font), ScaleFactor);
  end;
  if FHeaderLineHeight = 0 then
    FHeaderLineHeight := Painter.ScaledHeaderHeight(GetFontHeight(TreeList.Font), ScaleFactor);
  if FFooterLineHeight = 0 then
    FFooterLineHeight := Painter.ScaledHeaderHeight(GetFontHeight(TreeList.Font), ScaleFactor);
  Inc(FFooterLineHeight, Painter.FooterCellOffset);
  inherited CalculateDefaultHeights;
end;

procedure TcxTreeListViewInfo.AfterCalculate;
var
  I: Integer;
begin
  if FDragItem = nil then Exit;
  TreeList.Controller.DragItem := nil;
  TreeList.Customizing.RefreshInformation;
  for I := 0 to TreeList.ColumnCount - 1 do
    if (TreeList.Columns[I].HeaderCell <> nil) and
      (TreeList.Columns[I].HeaderCell.Item = FDragItem) then
        TreeList.Controller.DragItem := TreeList.Columns[I].HeaderCell;
  for I := 0 to TreeList.Bands.Count - 1 do
    if (TreeList.Bands[I].HeaderCell <> nil) and
      (TreeList.Bands[I].HeaderCell.Item = FDragItem) then
        TreeList.Controller.DragItem := TreeList.Bands[I].HeaderCell;
end;

procedure TcxTreeListViewInfo.BeforeCalculate;
begin
  Bands.RefreshInformation;
  RecreateCheckBoxViewData;
  CanOffsetContent := not TreeList.OptionsView.CellAutoHeight;
  Styles.CanOffsetContent := not TreeList.OptionsView.CellAutoHeight;
  FRows := AllocMem((ColumnsRowCount + 1) * SizeOf(Integer));
  FGridLineColor := OptionsView.GridLineColor;
  if FGridLineColor = clDefault then
    FGridLineColor := Painter.DefaultTreeListGridLineColor;
  FGridLines := GridLineBorders[OptionsView.GridLines];
  FTreeLineColor := ColorToRGB(cxGetNativeColor(
    OptionsView.TreeLineColor, Painter.DefaultTreeListTreeLineColor));
  FFakeCell.Clear;
end;

procedure TcxTreeListViewInfo.CalculateBackgroundParts;
begin
  DoCalculateBackgroundParts(HScrollPos);
end;

procedure TcxTreeListViewInfo.CalculateBandsLayout;
var
  I: Integer;
  AKind: TcxTreeListBandFixedKind;
  ABounds, AClipBounds, R: TRect;
  ALengths: array[TcxTreeListBandFixedKind] of Integer;
begin
  FContentWidth := 0;
  ABounds := Bounds;
  if FindPanelPosition = fppTop then
    Inc(ABounds.Top, FindPanelHeight);
  if FilterBoxPosition = fpTop then
    Inc(ABounds.Top, FilterBoxHeight);
  ABounds := cxRectSetHeight(ABounds, BandsHeight + HeadersHeight);
  if IsIndicatorVisible then
  begin
    R := cxRectSetHeight(ABounds, BandsHeight);
    AddIndicator(nil, tlipBands, R);
    FContentWidth := IndicatorWidth;
    Inc(ABounds.Left, IndicatorWidth);
  end;
  AClipBounds := ABounds;
  FillChar(ALengths, SizeOf(ALengths), 0);
  FillChar(FContentParts, SizeOf(FContentParts), 0);
  for I := 0 to Bands.BottomItemCount - 1 do
    with Bands.BottomItems[I] do
      Inc(ALengths[FixedKind], DisplayWidth);
  Inc(FContentWidth, ALengths[tlbfLeft] + ALengths[tlbfNone] + ALengths[tlbfRight]);
  if Bands.VisibleLeftFixedCount > 0 then
  begin
    FContentParts[tlbfLeft] := cxRectSetWidth(ABounds, ALengths[tlbfLeft]);
    AddBandsHeaders(tlbfLeft, FContentParts[tlbfLeft], AClipBounds);
    Inc(ABounds.Left, ALengths[tlbfLeft]);
    AddBandSeparator(nil, ABounds, ABounds);
    AClipBounds.Left := ABounds.Left;
    Inc(FContentWidth, FixedSeparatorWidth);
  end;
  FContentParts[tlbfNone] := cxRectSetLeft(ABounds, ABounds.Left, ALengths[tlbfNone]);
  if Bands.VisibleRightFixedCount > 0 then
  begin
    FContentParts[tlbfRight] := cxRectSetLeft(ABounds, Min(FContentParts[tlbfNone].Right,
      Bounds.Right - ALengths[tlbfRight] - FixedSeparatorWidth),
      ALengths[tlbfRight] + FixedSeparatorWidth);
    ABounds.Right := FContentParts[tlbfRight].Left;
    AClipBounds.Left := Max(FContentParts[tlbfRight].Left, ABounds.Left);
    AddBandSeparator(nil, FContentParts[tlbfRight], AClipBounds);
    AddBandsHeaders(tlbfRight, FContentParts[tlbfRight], AClipBounds);
    Inc(FContentWidth, FixedSeparatorWidth);
  end;
  for AKind := Low(TcxTreeListBandFixedKind) to High(TcxTreeListBandFixedKind) do
    FContentParts[AKind].Bottom := Bounds.Bottom;
  CalculateHorzScrollInfo(cxRectWidth(ABounds), ALengths[tlbfNone]);
  OffsetRect(FContentParts[tlbfNone], -HScrollPos, 0);
  AddBandsHeaders(tlbfNone, FContentParts[tlbfNone], ABounds);
end;

procedure TcxTreeListViewInfo.CalculateColumnsLayout;
var
  I, ARowIndex, AColIndex, ATop: Integer;
  ABand: TcxTreeListBand;
  ARow: TcxTreeListBandRow;
  AColumn: TcxTreeListColumn;
  ABounds, AClipBounds, ARowBounds: TRect;
begin
  FMultiRows := False;
  ATop := Bounds.Top + BandsHeight;
  if FindPanelPosition = fppTop then
    Inc(ATop, FindPanelHeight);
  if FilterBoxPosition = fpTop then
    Inc(ATop, FilterBoxHeight);
  if IsIndicatorVisible then
  begin
    ABounds := cxRectSetTop(Bounds, ATop, HeadersHeight);
    AddIndicator(nil, tlipColumns, ABounds);
  end;
  for I := 0 to Bands.BottomItemCount - 1 do
  begin
    ABand := Bands.BottomItems[I];
    ABounds := cxRectSetTop(ABand.HeaderCell.BoundsRect, ATop, HeadersHeight);
    AClipBounds := cxRectSetTop(ABand.HeaderCell.VisibleRect, ATop, HeadersHeight);
    AddBandPart(tlbpHeader, ABand, nil, nil, ABounds, AClipBounds);
    FMultiRows := FMultiRows or (ABand.BandRows.VisibleItemCount > 1);
    for ARowIndex := 0 to ABand.BandRows.VisibleItemCount - 1 do
    begin
      ARow := ABand.BandRows.VisibleItems[ARowIndex];
      ARowBounds := cxRectSetYPos(ABounds,
        RowOffset[ARow.LineOffset], RowOffset[ARow.LineOffset + ARow.LineCount]);
      for AColIndex := 0 to ARow.VisibleItemCount - 1 do
      begin
        AColumn := ARow.VisibleItems[AColIndex];
        ARowBounds.Bottom := RowOffset[ARow.LineOffset + AColumn.Position.LineCount];
        AddColumnHeader(AColumn, ARowBounds, AClipBounds);
      end;
    end;
  end;
end;

procedure TcxTreeListViewInfo.CalculateHitTest(
  AHitTest: TcxTreeListHitTest);
var
  I: Integer;
begin
  Validate;
  Cells.CalculateHitTest(AHitTest);
  if PtInRect(ContentBounds, AHitTest.HitPoint) then
  begin
    for I := 0 to Count - 1 do
      if NodeViewData[I].Cells.CalculateHitTest(AHitTest) then Break;
  end;
  for I := 0 to HitTestCells.Count - 1 do
    if TcxTreeListHitTestArea(HitTestCells[I]).GetHitTest(AHitTest) then Break;
  (NavigatorSiteViewInfo as TcxTreeListNavigatorSiteViewInfo).GetHitTest(AHitTest);
  (FindPanelViewInfo as TcxTreeListFindPanelViewInfo).GetHitTest(AHitTest);
  (FilterBoxViewInfo as TcxTreeListFilterBoxViewInfo).GetHitTest(AHitTest);
end;

procedure TcxTreeListViewInfo.CalculateHorzScrollInfo(
  AAvailableWidth, AContentWidth: Integer);
var
  APos: Integer;
  AVScrollbarAreaWidth: Integer;
begin
  FHScrollPage := AAvailableWidth;
  if (TreeList.GetScrollbarMode = sbmHybrid) and (TreeList.IsScrollBarActive(sbVertical)) and
    (Bands.VisibleRightFixedCount = 0) then
    AVScrollbarAreaWidth := TreeList.GetVScrollBarAreaWidth
  else
    AVScrollbarAreaWidth := 0;
  FHScrollSize := AContentWidth + AVScrollbarAreaWidth;
  APos := FHScrollPos;
  ValidateScrollPos(APos, FHScrollPage, FContentWidth + AVScrollbarAreaWidth);
  if AContentWidth + AVScrollbarAreaWidth > AAvailableWidth then
  begin
    FHScrollArea := cxRectSetXPos(ContentParts[tlbfNone],  Max(ContentParts[
      tlbfNone].Left, Bounds.Left), Min(ContentParts[tlbfNone].Right, Bounds.Right));
    if Bands.VisibleRightFixedCount > 0 then
      FHScrollArea.Right := ContentParts[tlbfRight].Left - FixedSeparatorWidth - 1;
    if Bands.VisibleLeftFixedCount > 0 then
      FHScrollArea.Left := ContentParts[tlbfLeft].Right + FixedSeparatorWidth;
  end
  else
    FHScrollArea := cxNullRect;
  HScrollPos := APos;
end;

procedure TcxTreeListViewInfo.CalculateFooterLayout;
var
  ABounds: TRect;
begin
  ABounds := Bounds;
  ABounds.Top := ABounds.Bottom - BottomNonContentHeight;
  ABounds := cxRectSetHeight(ABounds, FooterHeight);
  ABounds.Right := ContentBounds.Right;
  if IsIndicatorVisible then
    AddIndicator(nil, tlipFooter, ABounds);
  if (Bands.VisibleItemCount > 0) and (FooterHeight > 0) then
    AddFooter(Root, 0, 0, ABounds);
end;

procedure TcxTreeListViewInfo.CalculateHeaderRowsLayout;
var
  ARow: TcxTreeListBandRow;
  I, AFinishRow: Integer;
begin
  InitializeHeaderRows;
  if OptionsView.HeaderAutoHeight then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      ARow := Columns[I].Position.Row;
      AFinishRow := ARow.LineOffset + Columns[I].Position.LineCount;
      RowOffset[AFinishRow] := Max(RowOffset[AFinishRow],
        RowOffset[ARow.LineOffset] + MeasureColumnHeaderHeight(Columns[I]));
    end;
  end;
  FHeadersHeight := RowOffset[ColumnsRowCount] - RowOffset[0];
end;

procedure TcxTreeListViewInfo.CalculateHeadersNeighbors;
var
  I, J, AShift: Integer;
  ACell: TcxTreeListCustomHeaderCellViewInfo;
begin
  AShift := Byte(Painter.HeaderBorders([nLeft..nBottom]) <> cxBordersAll);
  for I := 0 to FHeaderCells.Count - 1 do
  begin
    ACell := TcxTreeListCustomHeaderCellViewInfo(FHeaderCells[I]);
    ACell.FIsLast := (ACell.BoundsRect.Right = ContentBounds.Right);
    for J := 0 to FHeaderCells.Count - 1 do
      ACell.CheckNeighbors(TcxTreeListHeaderCellViewInfo(FHeaderCells[J]), AShift);
  end;
end;

function TcxTreeListViewInfo.CalculateNodeAutoHeight(
  AViewData: TcxTreeListNodeViewData): Integer;
var
  ARow: TcxTreeListBandRow;
  I, AFinishRow, AHeight: Integer;
begin
  for I := 0 to ColumnCount - 1 do
  begin
    ARow := Columns[I].Position.Row;
    AHeight := TcxFakeCellViewInfo(FakeCell).MeasureHeight(Columns[I], AViewData.Node, False);
    AFinishRow := ARow.LineOffset + Columns[I].Position.LineCount;
    RowOffset[AFinishRow] := Max(RowOffset[AFinishRow] -
      RowOffset[ARow.LineOffset], RowOffset[ARow.LineOffset] + AHeight);
  end;
  Result := RowOffset[ColumnsRowCount] - RowOffset[0];
  AViewData.RowsHeight := Result;
end;

procedure TcxTreeListViewInfo.CalculateNodesOrigin;
var
  AOffset: TPoint;
  I, ALastPos: Integer;
begin
  I := 0;
  FNodesVisibleCount := 0;
  FVScrollPageValue := 0;
  AOffset := Point(0, ContentBounds.Top + PixelScrollNodeOffset);
  ALastPos := Bounds.Bottom - BottomNonContentHeight;
  FContentHeight := ALastPos;
  while I < Count do
  begin
    NodeViewData[I].Origin := AOffset;
    Inc(AOffset.Y, NodeViewData[I].Height);
    Inc(I);
    if FNodesVisibleCount = 0 then
    begin
      Inc(FNodesVisibleCount);
      Inc(FVScrollPageValue);
    end
    else
      if AOffset.Y <= ALastPos then
      begin
        Inc(FNodesVisibleCount);
        if AOffset.Y <= (ALastPos - GetHScrollBarAreaHeight) then
          Inc(FVScrollPageValue);
      end
      else
        while I < Count do
          DeleteNodeViewData(I);
  end;
  if AOffset.Y < ALastPos then
  begin
    FContentHeight := AOffset.Y;
    if FHorzBackgroundCell = nil then
      FHorzBackgroundCell := AddBackgroundPart(cxRectSetYPos(ContentBounds, AOffset.Y, ALastPos))
    else
      FHorzBackgroundCell.SetBounds(cxRectSetYPos(ContentBounds, AOffset.Y, ALastPos), Bounds);
  end
  else
    if FHorzBackgroundCell <> nil then
      FHorzBackgroundCell.SetBounds(cxEmptyRect, cxEmptyRect);
  FContentHeight := FContentHeight - ContentBounds.Top;
  if (NodesVisibleCount > 0) and (TreeList.FTopVisibleNode <> NodeViewData[0].Node) then
  begin
    TreeList.FTopVisibleNode := NodeViewData[0].Node;
    TreeList.DoTopRecordIndexChanged;
  end;
  RecalculateHitTestCells;
end;

procedure TcxTreeListViewInfo.CalculateNodesViewData;
var
  AHeight: Integer;
  ANode: TcxTreeListNode;
begin
  ANode := TreeList.TopVisibleNode;
  if (ANode = nil) or (TreeList.AbsoluteVisibleCount = 0) then
  begin
    FContentHeight := 0;
    FNodesVisibleCount := 0;
    FVScrollPageValue := 0;
    Exit;
  end;
  FContentHeight := GetPixelScrollContentSize;
  AHeight := FContentHeight;
  CalculateNodesViewDataForward(ANode, AHeight);
  CalculateNodesViewDataBackward(ANode.GetPrevVisible, True, AHeight);
  CalculateNodesOrigin;
end;

procedure TcxTreeListViewInfo.CalculateNodesViewDataBackward(
  ANode: TcxTreeListNode; ACheckHeight: Boolean; var AvailableHeight: Integer);
var
  APrevHeight: Integer;
  ACells: TcxCustomControlCells;
  AViewData: TcxTreeListNodeViewData;
begin
  Inc(FLockCount);
  ACells := FCells;
  try
    while (ANode <> nil) and (AvailableHeight > 0) do
    begin
      if AvailableHeight > -FPixelScrollNodeOffset then
      begin
        Inc(AvailableHeight, FPixelScrollNodeOffset);
        FPixelScrollNodeOffset := 0;
        APrevHeight := AvailableHeight;
        AViewData := AddNodeViewData(ANode, True);
        Dec(AvailableHeight, AViewData.Height);
        if ACheckHeight and (AvailableHeight < 0) and (Count > 1) then
        begin
          if IsRecordPixelScrollMode then
            FPixelScrollNodeOffset := AvailableHeight
          else
          begin
            FNodesViewData.Delete(0);
            AViewData.Free;
          end;
          AvailableHeight := APrevHeight;
          Break;
        end;
      end
      else
      begin
        Inc(FPixelScrollNodeOffset, AvailableHeight);
        AvailableHeight := 0;
      end;
      ANode := ANode.GetPrevVisible;
    end;
  finally
    FCells := ACells;
    Dec(FLockCount);
  end;
end;

procedure TcxTreeListViewInfo.CalculateNodesViewDataForward(
  ANode: TcxTreeListNode; var AvailableHeight: Integer);
var
  ACells: TcxCustomControlCells;
begin
  Dec(AvailableHeight, PixelScrollNodeOffset);
  Inc(FLockCount);
  ACells := FCells;
  try
    while (ANode <> nil) and (AvailableHeight > 0) do
    begin
      Dec(AvailableHeight, AddNodeViewData(ANode, False).Height);
      ANode := ANode.GetNextVisible;
    end;
    if ANode = nil then
      Dec(AvailableHeight, GetHScrollBarAreaHeight);
  finally
    FCells := ACells;
    Dec(FLockCount);
  end;
end;

procedure TcxTreeListViewInfo.CalculatePixelScrollInfo(var ANode: TcxTreeListNode; var APixelScrollNodeOffset: Integer;
  AMaxTopNode: TcxTreeListNode; AMaxPixelScrollTopNodeOffset: Integer; ADelta: Integer; out AOverPan: Boolean);

  function NextRecordNeeded(ANode: TcxTreeListNode; var ASize: Integer): Boolean;
  var
    ANodeSize: Integer;
  begin
    ANodeSize := GetNodePixelScrollSize(ANode);
    Result := (ASize >= ANodeSize) and (ANode <> AMaxTopNode);
    if Result then
      Dec(ASize, ANodeSize);
  end;

var
  ASize: Integer;
begin
  if ADelta < 0 then
  begin
    ASize := -ADelta - APixelScrollNodeOffset;
    while NextRecordNeeded(ANode, ASize) do
      ANode := ANode.GetNextVisible;
    APixelScrollNodeOffset := -ASize;
    if ANode = AMaxTopNode then
      APixelScrollNodeOffset := Max(APixelScrollNodeOffset, AMaxPixelScrollTopNodeOffset);
    AOverPan := (ANode = AMaxTopNode) and (APixelScrollNodeOffset <> -ASize);
  end
  else
  begin
    ASize := ADelta + APixelScrollNodeOffset;
    while (ASize > 0) and (ANode.VisibleIndex > 0) do
    begin
      ANode := ANode.GetPrevVisible;
      Dec(ASize, GetNodePixelScrollSize(ANode));
    end;
    APixelScrollNodeOffset := Min(ASize, 0);
    AOverPan := (ANode.VisibleIndex = 0) and (ASize > 0);
  end;
end;

procedure TcxTreeListViewInfo.CellsChanged;
begin
  TreeList.Controller.ViewInfoChanged;
end;

procedure TcxTreeListViewInfo.CheckBiDiMode;
var
  I: Integer;
begin
  if UseRightToLeftAlignment and not FIsRightToLeftConverted then
  begin
    Cells.RightToLeftConversion(Bounds);
    for I := 0 to Count - 1 do
      NodeViewData[I].RightToLeftConversion(Bounds);
    for I := 0 to HitTestCells.Count - 1 do
      TcxTreeListHitTestArea(HitTestCells[I]).RightToLeftConversion(Bounds);
    FHScrollArea := TdxRightToLeftLayoutConverter.ConvertRect(FHScrollArea, Bounds);
    FindPanelViewInfo.RightToLeftConversion(Bounds);
    FilterBoxViewInfo.RightToLeftConversion(Bounds);
    FIsRightToLeftConverted := True;
  end;
end;

function TcxTreeListViewInfo.CheckScrollPosition(AScrollCode: TScrollCode;
  APos, APage, AInc, AMax : Integer; var AScrollPos: Integer): Boolean;
begin
  case AScrollCode of
    scLineUp:
      AScrollPos := APos - AInc;
    scLineDown:
      AScrollPos := APos + AInc;
    scPageUp:
      AScrollPos := APos - APage;
    scPageDown:
      AScrollPos := APos + APage;
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := AMax;
    scTrack:
      AScrollCode := scPosition;
  end;
  if AScrollPos < 0 then
    AScrollPos := 0
  else
    if AScrollPos + APage > AMax then AScrollPos := AMax - APage;
  if AScrollCode = scPosition then cxRange(AScrollPos, 0, AMax);
  Result := AScrollPos <> APos;
end;

procedure TcxTreeListViewInfo.Clear;
begin
  FHitTestCells.Clear;
  FDragItem := nil;
  if TreeList.Controller.DragItem is TcxTreeListHeaderCellViewInfo then
  begin
    FDragItem := TcxTreeListHeaderCellViewInfo(TreeList.Controller.DragItem).Item;
    TreeList.Controller.DragItem := nil;
  end;
  FreeMem(FRows);
  FHorzBackgroundCell := nil;
  FVertBackgroundCell := nil;
  FRows := nil;
  FHeaderCells.Clear;
  FPrevFont := nil;
  FCells.Clear;
  FNodesViewData.Clear;
  FNodesVisibleCount := 0;
  FVScrollPageValue := 0;
end;

procedure TcxTreeListViewInfo.DeleteNodeViewData(AIndex: Integer);
begin
  FNodesViewData[AIndex].Free;
  FNodesViewData.Delete(AIndex);
  FNodesVisibleCount := Min(FNodesVisibleCount, FNodesViewData.Count);
  FVScrollPageValue := Min(FVScrollPageValue, FNodesViewData.Count);
end;

procedure TcxTreeListViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  FIsRightToLeftConverted := False;
  FindPanelViewInfo.IsRightToLeftConverted := False;
  FilterBoxViewInfo.IsRightToLeftConverted := False;
  CalculateInProcess := True;
  CellsChanged;
  IsDirty := False;
  Clear;
  BeforeCalculate;
  try
    CalculateHeaderRowsLayout;
    CalculateBandsLayout;
    CalculateColumnsLayout;
    CalculateNodesViewData;
    CalculateFooterLayout;
    CalculateBackgroundParts;
    Cells.Sort(@cxCompareCells);
    CalculateHeadersNeighbors;
    HitTestCells.Sort(@cxCompareHitTestCells);
    CalculateNavigator;
    CalculateFindPanel;
    CalculateFilterBox;
    CheckBiDiMode;
  finally
    AfterCalculate;
    CalculateInProcess := False;
    IsDirty := False;
  end;
end;

procedure TcxTreeListViewInfo.DoCalculateBackgroundParts(AHScrollPos: Integer);
var
  ABounds: TRect;
begin
  ABounds := Bounds;
  // right part
  Inc(ABounds.Left, ContentWidth - AHScrollPos);
  if ContentWidth = 0 then
    Inc(ABounds.Left, IndicatorWidth);
  if FVertBackgroundCell <> nil then
    FVertBackgroundCell.SetBounds(ABounds, ABounds)
  else
    if not cxRectIsEmpty(ABounds) then
      FVertBackgroundCell := AddBackgroundPart(ABounds);
  //bottom part
  ABounds := cxRectSetYPos(ContentBounds, ContentBounds.Bottom,
    Bounds.Bottom - BottomNonContentHeight);
  if FHorzBackgroundCell <> nil then
    FHorzBackgroundCell.SetBounds(ABounds, ABounds)
  else
    if not cxRectIsEmpty(ABounds) then
      FHorzBackgroundCell := AddBackgroundPart(ABounds);
end;

function TcxTreeListViewInfo.GetContentBounds: TRect;
begin
  Result := Bounds;
  Inc(Result.Top, TopNonContentHeight);
  Result.Bottom := Result.Top + FContentHeight;
  Result.Right := Result.Left + FContentWidth;
end;

function TcxTreeListViewInfo.GetRealContentBounds: TRect;
begin
  Result := GetContentBounds;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxTreeListViewInfo.GetFilterBoxViewInfoClass: TcxControlFilterBoxViewInfoClass;
begin
  Result := TcxTreeListFilterBoxViewInfo;
end;

function TcxTreeListViewInfo.GetFindPanelViewInfoClass: TcxControlFindPanelViewInfoClass;
begin
  Result := TcxTreeListFindPanelViewInfo;
end;

function TcxTreeListViewInfo.GetFontHeight(const AFont: TFont): Integer;
begin
  if AFont <> FPrevFont then
  begin
    FPrevFont := AFont;
    FPrevFontHeight := cxTextHeight(AFont);
  end;
  Result := FPrevFontHeight;
end;

function TcxTreeListViewInfo.GetHScrollBarAreaHeight: Integer;
begin
  if (TreeList.GetScrollbarMode = sbmHybrid) and TreeList.IsScrollBarActive(sbHorizontal) then
    Result := TreeList.GetHScrollBarAreaHeight
  else
    Result := 0;
end;

procedure TcxTreeListViewInfo.GetHScrollBarBounds(var ABounds: TRect);
begin
  inherited GetHScrollBarBounds(ABounds);
  if TreeList.IsTouchScrollUIMode then
    if not UseRightToLeftAlignment then
      ABounds.Left := ABounds.Left + IndicatorWidth
    else
      ABounds.Right := ABounds.Right - IndicatorWidth;
end;

function TcxTreeListViewInfo.GetIsIndicatorVisible: Boolean;
begin
  Result := OptionsView.IsIndicatorVisible;
end;

function TcxTreeListViewInfo.GetIsPrinting: Boolean;
begin
  Result := False;
end;

function TcxTreeListViewInfo.GetLevelContentOffset(ALevel: Integer): Integer;
begin
  Result := LevelInfo[ALevel].Offset + LevelInfo[ALevel].Width;
end;

function TcxTreeListViewInfo.GetMultilineEditorBounds(
  const ACellEditBounds: TRect; ACalculatedHeight: Integer;
  AAutoHeight: TcxInplaceEditAutoHeight): TRect;
var
  I, AMaxBottom, ANodeIndex: Integer;
  ANodeViewData: TcxTreeListNodeViewData;
begin
  Result := ACellEditBounds;
  AMaxBottom := ClientRect.Bottom;
  ANodeViewData := nil;
  if AAutoHeight = eahEditor then
  begin
    if TreeList.FocusedNode <> nil then
      ANodeViewData := TreeList.FocusedNode.ViewData;
    if ANodeViewData = nil then
      Exit;
    Result.Bottom := ANodeViewData.GetRealContentBounds.Bottom;
    if Result.Bottom > AMaxBottom then
    begin
      Result.Bottom := AMaxBottom;
      Exit;
    end;
    ANodeIndex := TreeList.FocusedNode.VisibleIndex - TreeList.TopVisibleNode.VisibleIndex;
    for I := ANodeIndex to NodesVisibleCount - 1 do
    begin
      ANodeViewData := NodeViewData[I];
      Result.Bottom := Min(ANodeViewData.GetRealContentBounds.Bottom, AMaxBottom);
      if Result.Bottom >= ACellEditBounds.Top + ACalculatedHeight then
        Exit;
    end;
  end;
  Result.Bottom := Min(Result.Top + ACalculatedHeight, AMaxBottom);
end;

function TcxTreeListViewInfo.GetNavigatorSiteViewInfoClass: TcxCustomNavigatorSiteViewInfoClass;
begin
  Result := TcxTreeListNavigatorSiteViewInfo;
end;

function TcxTreeListViewInfo.GetNodeContentOffset(ANode: TcxTreeListNode): Integer;
begin
  Result := GetLevelContentOffset(ANode.Level);
  if ANode.HasCheckbox then
    Result := Result + LevelInfo[ANode.Level].Size.cx;
end;

function TcxTreeListViewInfo.GetNodePixelScrollSize(ANode: TcxTreeListNode): Integer;
var
  AViewData: TcxTreeListNodeViewData;
  ACells: TcxCustomControlCells;
begin
  if ANode.ViewData = nil then
  begin
    ACells := FCells;
    AViewData := AddNodeViewData(ANode, True, True);
    Result := AViewData.Height;
    DeleteNodeViewData(0);
    FCells := ACells;
  end
  else
    Result := ANode.ViewData.Height;
end;

function TcxTreeListViewInfo.GetPixelScrollContentSize: Integer;
begin
  Result := cxRectHeight(Bounds) - TopNonContentHeight - BottomNonContentHeight;
end;

procedure TcxTreeListViewInfo.GetPixelScrollTopNodeAndOffsetByBottomNode(ABottomNode: TcxTreeListNode;
  out ATopNode: TcxTreeListNode; out ATopNodePixelScrollOffset: Integer);
var
  AOverPan: Boolean;
begin
  ATopNode := ABottomNode;
  ATopNodePixelScrollOffset := -GetNodePixelScrollSize(ABottomNode);
  CalculatePixelScrollInfo(ATopNode, ATopNodePixelScrollOffset,
    nil, -1, GetPixelScrollContentSize, AOverPan);
end;

function TcxTreeListViewInfo.HasFooter(ANode: TcxTreeListNode): Boolean;
begin
  Result := False;
  if OptionsView.GroupFooters = tlgfAlwaysVisible then
    Result := (ANode.HasChildren and not ANode.Expanded) or
      (not ANode.HasChildren and (ANode.GetNextSiblingVisible = nil) and (ANode.Parent <> Root))
  else
    if OptionsView.GroupFooters = tlgfVisibleWhenExpanded then
      Result := not ANode.Expanded and (ANode.GetNextSiblingVisible = nil) and (ANode.Level > 0);
end;

function TcxTreeListViewInfo.HasStateIndent(ANode: TcxTreeListNode): Boolean;
begin
  if LevelInfo[ANode.Level].StateImages = nil then
    Result := False
  else
    Result := (not OptionsView.DynamicFocusedStateImages or ANode.Focused) and
      cxInRange(ANode.StateIndex, 0, LevelInfo[ANode.Level].StateImages.Count - 1);
end;

procedure TcxTreeListViewInfo.InflateBoundsForGridLines(
  var ABounds, AClipRect: TRect);
begin
  if bTop in GridLines then
  begin
    if ABounds.Top = AClipRect.Top then
      Dec(AClipRect.Top);
    Dec(ABounds.Top);
  end;
  if bLeft in GridLines then
  begin
    if ABounds.Left = AClipRect.Left then
      Dec(AClipRect.Left);
    Dec(ABounds.Left);
  end;
end;

procedure TcxTreeListViewInfo.InitializeHeaderRows;
var
  I, ATop: Integer;
begin
  ATop := Bounds.Top + BandsHeight;
  if FindPanelPosition = fppTop then
    Inc(ATop, FindPanelHeight);
  if FilterBoxPosition = fpTop then
    Inc(ATop, FilterBoxHeight);
  FRows^[0] := ATop;
  for I := 1 to ColumnsRowCount do
    FRows^[I] := FHeaderLineHeight + FRows^[I - 1];
end;

procedure TcxTreeListViewInfo.InitializeRows(
  const ANewHeight: Integer; var AViewData: TcxTreeListNodeViewData);
var
  I: Integer;
  AScaleFactor: Double;
begin
  if AViewData.RowsHeight = 0 then
  begin
    AViewData.RowsOffset := AViewData.Height;
    FRows^[0] := AViewData.Height;
    if ANewHeight = 0 then Exit;
    for I := 1 to ColumnsRowCount do
      FRows^[I] := MulDiv(ANewHeight, I, ColumnsRowCount) + AViewData.RowsOffset;
  end
  else
    if AViewData.RowsHeight <> ANewHeight then
    begin
      AScaleFactor := ANewHeight / AViewData.RowsHeight;
      for I := ColumnsRowCount downto 1 do
        FRows^[I] := Round((FRows^[I] - FRows^[I - 1]) * AScaleFactor);
      for I := 1 to ColumnsRowCount do
        FRows^[I] := FRows^[I] + FRows^[I - 1];
      FRows^[ColumnsRowCount] := ANewHeight;
    end;
  AViewData.RowsHeight := ANewHeight;
  Inc(AViewData.Height, ANewHeight);
end;

procedure TcxTreeListViewInfo.InitScrollBarsParameters;
var
  AVScrollSize: Integer;
begin
  AVScrollSize := FCacheVScrollSize;
  if IsPixelBasedScrollDataPos then
    Inc(AVScrollSize, GetHScrollBarAreaHeight);
  TreeList.SetScrollBarInfo(sbVertical, 0,  AVScrollSize - 1, 1,
    VScrollPage, FCacheVScrollPos, True, True);
  TreeList.SetScrollBarInfo(sbHorizontal, 0,  HScrollSize - 1,
    HScrollInc, HScrollPage, HScrollPos, True, CanHScrollBarHide);
end;

procedure TcxTreeListViewInfo.InitScrollBarsParametersCache;
begin
  if not TreeList.IsLocked then
    ValidateDirty;
  FCacheVScrollPos := VScrollPos;
  FCacheVScrollSize := VScrollSize;
end;

procedure TcxTreeListViewInfo.InvalidateRect(const ARect: TRect);
begin
  TreeList.InvalidateRect(ARect, False);
end;

function TcxTreeListViewInfo.IsFindPanelVisible: Boolean;
begin
  Result := inherited IsFindPanelVisible and not IsPrinting;
end;

function TcxTreeListViewInfo.IsPanArea(const APoint: TPoint): Boolean;
var
  APanArea: TRect;
begin
  cxRectIntersect(APanArea, ContentBounds, ClientRect);
  Result := PtInRect(APanArea, APoint);
end;

function TcxTreeListViewInfo.IsPixelBasedScrollDataPos: Boolean;
begin
  Result := IsRecordPixelScrollMode and (TreeList.AbsoluteVisibleCount < GetPixelScrollContentSize);
end;

function TcxTreeListViewInfo.IsRecordPixelScrollMode: Boolean;
begin
  Result := (TreeList.OptionsBehavior.RecordScrollMode = rsmByPixel) or
    (TreeList.OptionsBehavior.RecordScrollMode = rsmDefault) and cxIsTouchModeEnabled;
end;

procedure TcxTreeListViewInfo.MakeVisible(ANode: TcxTreeListNode);
var
  APos: Integer;
  ATopNode: TcxTreeListNode;
  ATopNodePixelScrollOffset: Integer;
begin
  if IsRecordPixelScrollMode and
    ((ANode.ViewData = nil) or (ANode.ViewData.GetRealBounds.Bottom > ContentBounds.Bottom)) then
  begin
    GetPixelScrollTopNodeAndOffsetByBottomNode(ANode, ATopNode, ATopNodePixelScrollOffset);
    FPixelScrollNodeOffset := ATopNodePixelScrollOffset;
    if TreeList.TopVisibleNode <> ATopNode then
      TreeList.TopVisibleNode := ATopNode
    else
      TreeList.LayoutChanged;
  end
  else
  begin
    if (ANode = TreeList.TopVisibleNode) then Exit;
    APos := VScrollPos + 1;
    while (ANode.ViewData <> nil) and (ANode.ViewData.GetRealBounds.Bottom > ContentBounds.Bottom) and
      (ANode <> TreeList.TopVisibleNode) do
      ScrollVert(scLineDown, APos);
    if not Assigned(ANode.ViewData) then
      TreeList.TopVisibleNode := ANode
    else
      TreeList.ViewInfo.IsDirty := False;
  end;
  TreeList.Controller.EditingController.DoUpdateEdit;
end;

function TcxTreeListViewInfo.MeasureColumnHeaderHeight(AColumn: TcxTreeListColumn): Integer;
var
  R: TRect;
  AFlags, AWidth: Integer;
  AViewParams: TcxViewParams;
begin
  Result := FHeaderLineHeight * AColumn.Position.LineCount;
  AViewParams := Styles.GetColumnHeaderParams(AColumn);
  AWidth := AColumn.DisplayWidth - Painter.HeaderBorderSize * 2 - ScaleFactor.Apply(cxTextOffset) * 2;
  if AColumn.SortOrder <> soNone then
    Dec(AWidth, LookAndFeelPainter.ScaledSortingMarkAreaSize(ScaleFactor).X);
  if not AColumn.Caption.Glyph.Empty then
  begin
    if AColumn.Caption.GlyphAlignHorz <> taCenter then
      Dec(AWidth, dxGetImageSize(AColumn.Caption.Glyph, ScaleFactor).cx);
    Result := Max(Result, AColumn.Caption.Glyph.Height + ScaleFactor.Apply(cxTextOffset) * 2);
  end;
  AFlags := DT_TOP or DT_LEFT or DT_CALCRECT;
  if AColumn.Caption.MultiLine then
    AFlags := AFlags or DT_WORDBREAK or DT_EDITCONTROL;
  if AColumn.Caption.ShowEndEllipsis then
    AFlags := AFlags or DT_END_ELLIPSIS;
  with TcxScreenCanvas.Create do
  try
    Font := AViewParams.Font;
    R := cxRectSetWidth(cxEmptyRect, AWidth);
    cxDrawText(Handle, AColumn.Caption.Text, R, AFlags);
    Result := Max(Result, Painter.ScaledHeaderHeight(R.Bottom - R.Top, ScaleFactor));
  finally
    Free;
  end;
end;

procedure TcxTreeListViewInfo.RecalculateHitTestCells;
var
  I: Integer;
begin
  for I := 0 to HitTestCells.Count - 1 do
    TcxTreeListHitTestArea(HitTestCells[I]).Calculate;
end;

procedure TcxTreeListViewInfo.RecreateCheckBoxViewData;
begin
  FreeAndNil(FCheckBoxViewData);
  FCheckBoxViewData := FCheckBoxProperties.CreateViewData(TreeList.EditStyle, True);
  FCheckBoxViewData.ScaleFactor.Assign(ScaleFactor);
end;

procedure TcxTreeListViewInfo.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);

  procedure DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer);
  var
    AOverpan: TPoint;
  begin
    if AScrollKind = sbHorizontal then
      AOverpan := Point(ADelta, 0)
    else
      AOverpan := Point(0, ADelta);
    TreeList.CheckOverpan(AScrollKind, 0, 1, -1, AOverpan.X, AOverpan.Y);
  end;

var
  ANode: TcxTreeListNode;
  APixelScrollNodeOffset: Integer;
  AOverPan: Boolean;
begin
  if ADelta = 0 then Exit;
  if (TopNode = nil) or (Count = 0) then Exit;
  ANode := TopNode;
  APixelScrollNodeOffset := PixelScrollNodeOffset;
  CalculatePixelScrollInfo(ANode, APixelScrollNodeOffset, FMaxPixelScrollTopNode,
    FMaxPixelScrollTopNodeOffset, ADelta, AOverPan);

  if (ANode <> TopNode) or (APixelScrollNodeOffset <> PixelScrollNodeOffset) then
  begin
    FPixelScrollNodeOffset := APixelScrollNodeOffset;
    if TreeList.TopVisibleNode <> ANode then
      TreeList.TopVisibleNode := ANode
    else
      TreeList.LayoutChanged;
    TreeList.Repaint;
  end;
  if AOverPan then
    DoOverpan(sbVertical, ADelta);
end;

procedure TcxTreeListViewInfo.ScrollHorz(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  CheckScrollPosition(AScrollCode, HScrollPos, HScrollPage, HScrollInc,
    HScrollSize + HScrollInc, AScrollPos);
  HScrollPos := AScrollPos;
end;

procedure TcxTreeListViewInfo.ScrollVert(
  AScrollCode: TScrollCode; var AScrollPos: Integer);

var
  AOffsets: array of Integer;

  procedure InitOrigin;
  var
    I: Integer;
  begin
    SetLength(AOffsets, NodesViewData.Count);
    for I := 0 to Length(AOffsets) - 1 do
      AOffsets[I] := NodeViewData[I].Origin.Y - NodeViewData[0].Origin.Y;
  end;


var
  ANode: TcxTreeListNode;
  ATop, ABottom, AHeight: Integer;
  APrevPixelScrollOffset, AOriginDisplacement, APrevScrollPos: Integer;
begin
  if (TopNode = nil) or (Count = 0) then Exit;
  ATop := TopNode.VisibleIndex;
  ABottom := ATop + NodesViewData.Count - 1;
  APrevPixelScrollOffset := FPixelScrollNodeOffset;
  APrevScrollPos := VScrollPos;
  InitOrigin;
  AHeight := 1;
  AOriginDisplacement := 0;
  case AScrollCode of
    scLineUp, scPageUp:
      begin
        AOriginDisplacement := APrevPixelScrollOffset;
        FPixelScrollNodeOffset := 0;
        if not TopNode.IsFirstVisible then
        begin
          if AScrollCode = scPageUp then
            AHeight := Max(1, GetPixelScrollContentSize - NodeViewData[NodesVisibleCount - 1].Height);
          CalculateNodesViewDataBackward(TopNode.GetPrevVisible, False, AHeight);
        end
        else
          if APrevPixelScrollOffset <> 0 then
            TreeList.LayoutChanged;
      end;
    scLineDown:
      begin
        ANode := NodeViewData[Count - 1].Node;
        AHeight := 0;
        if not ANode.IsLastVisible or (Count > NodesVisibleCount) then
        begin
          AHeight := NodeViewData[0].Height - APrevPixelScrollOffset;
          FPixelScrollNodeOffset := 0;
          AOriginDisplacement := -APrevPixelScrollOffset;
          if (ANode <> TopNode) and (not ANode.IsLastVisible or (Count > NodesVisibleCount)) then
          begin
            if IsRecordPixelScrollMode and ANode.IsLastVisible then
              LastNode := ANode
            else
            begin
              DeleteNodeViewData(0);
              if (ANode.GetNextVisible = nil) and (ANode <> TopNode) then
                TreeList.TopVisibleNode := TopNode.GetNextVisible
              else
                CalculateNodesViewDataForward(ANode.GetNextVisible, AHeight);
            end;
          end
          else
            if (ANode = TopNode) and not ANode.IsLastVisible then
              TreeList.TopVisibleNode := ANode.GetNextVisible;
        end;
      end;
    scPageDown:
      begin
        FPixelScrollNodeOffset := 0;
        AOriginDisplacement := -(NodeViewData[0].Height + APrevPixelScrollOffset);
        if (LastNode = TopNode) and (LastNode.GetNextVisible <> nil) then
          TreeList.TopVisibleNode := LastNode.GetNextVisible
        else
          TreeList.TopVisibleNode := LastNode;
      end;
    scTrack, scPosition:
      begin
        VScrollPos := AScrollPos;
        if VScrollPos > APrevScrollPos then
          AOriginDisplacement := APrevPixelScrollOffset - FPixelScrollNodeOffset
        else
          AOriginDisplacement := - APrevPixelScrollOffset + FPixelScrollNodeOffset
      end;
  end;

  CalculateNodesOrigin;
  AScrollPos := VScrollPos;
  if cxInRange(TopNode.VisibleIndex, ATop, ABottom) then
    TreeList.ScrollWindow(0, -AOffsets[TopNode.VisibleIndex - ATop] + AOriginDisplacement)
  else
  begin
    InitOrigin;
    if cxInRange(LastNode.VisibleIndex, ATop, ABottom) then
      TreeList.ScrollWindow(0, AOffsets[ATop - TopNode.VisibleIndex] - AOriginDisplacement)
    else
      InvalidateRect(ContentBounds);
  end;
  if FHorzBackgroundCell <> nil then
    FHorzBackgroundCell.Invalidate;
end;

procedure TcxTreeListViewInfo.SetDirty;
begin
  IsDirty := True;
  TreeList.Controller.ViewInfoChanged;
  Clear;
end;

procedure TcxTreeListViewInfo.UpdateScrollBars;
begin
  if TreeList.HandleAllocated and not TreeList.UpdatingScrollBars then
    TreeList.UpdateScrollBars;
end;

procedure TcxTreeListViewInfo.UpdateSelection;
var
  I: Integer;
  AFullUpdate: Boolean;
begin
  if IsDirty then
    Exit;
  inherited UpdateSelection;
  if Validate or (Count = 0) then
  begin
    InvalidateRect(Bounds);
    Exit;
  end;
  AFullUpdate := False;
  for I := 0 to Count - 1 do
  begin
    if not NodeViewData[I].Update(False) then Continue;
    AFullUpdate := AFullUpdate or ((NodeViewData[I].StateImageIndent <> nil)
      <> HasStateIndent(NodeViewData[I].Node));
  end;
  if AFullUpdate then
  begin
    DoCalculate;
    if OptionsView.CellAutoHeight then
      InvalidateRect(cxRectSetYPos(Bounds,
      TopNonContentHeight, Bounds.Bottom - BottomNonContentHeight));
  end;
  TreeList.RefreshNavigatorButtons;
end;

procedure TcxTreeListViewInfo.UpdatePixelScrollTopNodeAndOffsetMaxValues;
var
  AOverPan: Boolean;
begin
  FMaxPixelScrollTopNode := TreeList.AbsoluteVisibleItems[TreeList.AbsoluteVisibleCount - 1];
  FMaxPixelScrollTopNodeOffset := -GetNodePixelScrollSize(FMaxPixelScrollTopNode) - GetHScrollBarAreaHeight;
  CalculatePixelScrollInfo(FMaxPixelScrollTopNode, FMaxPixelScrollTopNodeOffset,
    nil, -1, GetPixelScrollContentSize, AOverPan);
end;

function TcxTreeListViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := TreeList.UseRightToLeftAlignment;
end;

function TcxTreeListViewInfo.UseRightToLeftReading: Boolean;
begin
  Result := TreeList.UseRightToLeftReading;
end;

function TcxTreeListViewInfo.UseRightToLeftScrollBar: Boolean;
begin
  Result := TreeList.UseRightToLeftScrollBar;
end;

procedure TcxTreeListViewInfo.ValidateScrollPos(
  var APosition: Integer; const APage, AMax: Integer);
begin
  if APosition + APage > AMax then
    APosition := Max(0, AMax - APage)
  else
    APosition := Max(APosition, 0);
end;

function TcxTreeListViewInfo.GetBands: TcxTreeListBands;
begin
  Result := TreeList.Bands;
end;

function TcxTreeListViewInfo.GetBandsHeight: Integer;
begin
  if OptionsView.Bands then
  begin
    Result := Bands.LineCount * BandLineHeight;
    if IsIndicatorVisible and (Result = 0)  then
      Result := DefaultRowHeight;
  end
  else
    Result := 0;
end;

function TcxTreeListViewInfo.GetBorderSize: Integer;
begin
  Result := TreeList.BorderSize;
end;

function TcxTreeListViewInfo.GetBottomNonContentHeight: Integer;
begin
  Result := FooterHeight;
  if FindPanelPosition = fppBottom then
    Inc(Result, FindPanelHeight);
  if FilterBoxPosition = fpBottom then
    Inc(Result, FilterBoxHeight);
end;

function TcxTreeListViewInfo.GetCalculateInProcess: Boolean;
begin
  Result := State[cvis_ViewInfoCalculate];
end;

function TcxTreeListViewInfo.GetCheckBoxSize: TSize;
begin
  Result := Painter.ScaledCheckButtonSize(ScaleFactor);
  Inc(Result.cx, ScaleFactor.Apply(cxTextOffset) * 2);
  Inc(Result.cy, ScaleFactor.Apply(cxTextOffset) * 2);
end;

function TcxTreeListViewInfo.GetColumn(AIndex: Integer): TcxTreeListColumn;
begin
  Result := TreeList.VisibleColumns[AIndex];
end;

function TcxTreeListViewInfo.GetColumnCount: Integer;
begin
  Result := TreeList.VisibleColumnCount;
end;

function TcxTreeListViewInfo.GetColumnDefaultEditHeight(
  AColumn: TcxTreeListColumn): Integer;
begin
  Result := AColumn.GetEditDefaultHeight(
    AColumn.Styles.GetContentParams(nil).Font);
end;

function TcxTreeListViewInfo.GetColumnsRowCount: Integer;
begin
  Result := Bands.ColumnsLineCount;
end;

function TcxTreeListViewInfo.GetContentPart(
  AKind: TcxTreeListBandFixedKind): TRect;
begin
  Result := FContentParts[AKind];
end;

function TcxTreeListViewInfo.GetCount: Integer;
begin
  Result := FNodesViewData.Count;
end;

function TcxTreeListViewInfo.GetDefaultRowHeight: Integer;
begin
  Result := DefaultEditHeight;
end;

function TcxTreeListViewInfo.GetFixedSeparatorWidth: Integer;
begin
  Result := OptionsView.FixedSeparatorWidth;
end;

function TcxTreeListViewInfo.GetFooterHeight: Integer;
begin
  Result := FooterLineCount * FooterLineHeight;
  if (Result = 0) or IsPrinting then
    Exit;
  Result := Result + Painter.FooterSeparatorSize + Painter.FooterCellOffset + Painter.FooterBorderSize;
end;

function TcxTreeListViewInfo.GetFooterLineCount: Integer;
begin
  if not OptionsView.Footer then
    Result := 0
  else
    Result := Max(1, Max(ColumnsRowCount, Summary.FooterSummaryRowCount));
end;

function TcxTreeListViewInfo.GetGroupFooterHeight: Integer;
begin
  Result := GroupFooterLineCount * FooterLineHeight;
  if (Result = 0) or IsPrinting then Exit;
  Result := Result + Painter.FooterSeparatorSize +
    Painter.FooterCellOffset + Painter.FooterBorderSize;
end;

function TcxTreeListViewInfo.GetGroupFooterLineCount: Integer;
begin
  if OptionsView.GroupFooters = tlgfInvisible then
    Result := 0
  else
    if not MultiRows then
      Result := Max(1, Summary.GroupFooterSummaryRowCount)
    else
      Result := Max(1, ColumnsRowCount);
end;

function TcxTreeListViewInfo.GetHasFixedSeparator: Boolean;
begin
  Result := FixedSeparatorWidth > 0;
end;

function TcxTreeListViewInfo.GetHeadersHeight: Integer;
begin
  if OptionsView.Headers then
  begin
    Result := FHeadersHeight;
    if IsIndicatorVisible and (Result = 0) then
      Result := DefaultRowHeight;
  end
  else
    Result := 0;
end;

function TcxTreeListViewInfo.GetImages: TCustomImageList;
begin
  Result := TreeList.Images;
end;

function TcxTreeListViewInfo.GetIndentBand: TcxTreeListBand;
begin
  Result := Bands.ExpandableBand;
end;

function TcxTreeListViewInfo.GetIndentLeftMost: Boolean;
begin
  Result := (IndentBand <> nil) and
    (IndentBand = Bands.BottomItems[0]);
end;

function TcxTreeListViewInfo.GetIndicatorWidth: Integer;
begin
  if IsIndicatorVisible then
    Result := OptionsView.IndicatorWidth
  else
    Result := 0;
end;

function TcxTreeListViewInfo.GetLastNode: TcxTreeListNode;
begin
  if FNodesVisibleCount > 0 then
    Result := NodeViewData[FNodesVisibleCount - 1].Node
  else
    Result := nil;
end;

function TcxTreeListViewInfo.GetLastPartVisibleNode: TcxTreeListNode;
begin
  if NodesViewData.Count > 0 then
    Result := TcxTreeListNodeViewData(NodesViewData.Last).Node
  else
    Result := nil;
end;

function TcxTreeListViewInfo.GetLevelInfo(
  ALevel: Integer): TcxTreeListLevelInfo;
begin
  Result := TreeList.LevelsInfo[ALevel];
end;

function TcxTreeListViewInfo.GetNodeViewData(
  AIndex: Integer): TcxTreeListNodeViewData;
begin
  Result := TcxTreeListNodeViewData(FNodesViewData[AIndex])
end;

function TcxTreeListViewInfo.GetOptionsView: TcxTreeListOptionsView;
begin
  Result := TreeList.OptionsView;
end;

function TcxTreeListViewInfo.GetOriginalViewInfo: TcxTreeListViewInfo;
begin
  Result := TreeList.ViewInfo;
end;

function TcxTreeListViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := inherited LookAndFeelPainter;
end;

function TcxTreeListViewInfo.GetPixelScrollNodeOffset: Integer;
begin
  Result := FPixelScrollNodeOffset;
end;

function TcxTreeListViewInfo.GetPreview: TcxTreeListPreview;
begin
  Result := TreeList.Preview;
end;

function TcxTreeListViewInfo.GetRoot: TcxTreeListNode;
begin
  Result := TreeList.Root;
end;

function TcxTreeListViewInfo.GetRowOffset(AIndex: Integer): Integer;
begin
  Result := FRows^[AIndex];
end;

function TcxTreeListViewInfo.GetSummary: TcxTreeListSummary;
begin
  Result := TreeList.Summary;
end;

function TcxTreeListViewInfo.GetStateImages: TCustomImageList;
begin
  Result := TreeList.StateImages;
end;

function TcxTreeListViewInfo.GetStyles: TcxTreeListStyles;
begin
  Result := TreeList.Styles;
end;

function TcxTreeListViewInfo.GetTopNode: TcxTreeListNode;
begin
  Result := TreeList.TopVisibleNode;
  if (Result <> nil) and (not Result.Visible or Result.HiddenByFilter) then
    Result := Result.GetNextVisible;
  if Result = nil then
  begin
    Result := TreeList.TopVisibleNode;
    if Result <> nil then
      Result := Result.GetPrevVisible;
  end;
end;

function TcxTreeListViewInfo.GetTopNonContentHeight: Integer;
begin
  Result := HeadersHeight + BandsHeight;
  if FindPanelPosition = fppTop then
    Inc(Result, FindPanelHeight);
  if FilterBoxPosition = fpTop then
    Inc(Result, FilterBoxHeight);
end;

function TcxTreeListViewInfo.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Control)
end;

procedure TcxTreeListViewInfo.GetVScrollBarBounds(var ABounds: TRect);
begin
  inherited GetVScrollBarBounds(ABounds);
  if TreeList.IsTouchScrollUIMode then
    ABounds.Top := ABounds.Top + TopNonContentHeight;
end;

function TcxTreeListViewInfo.GetVScrollPage: Integer;
begin
  if IsPixelBasedScrollDataPos then
    Result := GetPixelScrollContentSize
  else
    Result := Max(1, FVScrollPageValue);
end;

function TcxTreeListViewInfo.GetVScrollPos: Integer;
begin
  Result := 0;
  if IsPixelBasedScrollDataPos then
  begin
    if (Count > 0) and (TopNode <> nil) then
//      if GridView.IsEqualScrollSizeRecords then
//        Result := TopRecordIndex * ViewInfo.GetEqualHeightRecordScrollSize - PixelScrollRecordOffset
//      else
        Result := TopNode.FPixelScrollPosition - PixelScrollNodeOffset
    else
      Result := 0
  end
  else
    if TopNode <> nil then
      Result := TopNode.VisibleIndex;
end;

function TcxTreeListViewInfo.GetVScrollSize: Integer;
var
  I: Integer;
  ANode: TcxTreeListNode;
begin
  if IsPixelBasedScrollDataPos then
  begin
    Result := 0;
    for I := 0 to TreeList.AbsoluteVisibleCount - 1 do
    begin
      ANode := TreeList.AbsoluteVisibleItems[I];
      ANode.FPixelScrollPosition := Result;
      Result := Result + GetNodePixelScrollSize(ANode);
    end;
  end
  else
    Result := TreeList.AbsoluteVisibleCount;
end;

procedure TcxTreeListViewInfo.SetCalculateInProcess(AValue: Boolean);
begin
  State[cvis_ViewInfoCalculate] := AValue;
end;

procedure TcxTreeListViewInfo.SetHScrollPos(APosition: Integer);
const
  ASign: array[Boolean] of Integer = (1, -1);
var
  DX, I: Integer;
begin
  ValidateScrollPos(APosition, FHScrollPage, FHScrollSize);
  DX := ASign[IsRightToLeftConverted] * (FHScrollPos - APosition);
  if DX = 0 then Exit;
  if not IsDirty then
  begin
    if FVertBackgroundCell <> nil then
      FVertBackgroundCell.IsRightToLeftConverted := False;
    DoCalculateBackgroundParts(APosition);
    if (FVertBackgroundCell <> nil) and UseRightToLeftScrollBar then
      FVertBackgroundCell.RightToLeftConversion(Bounds);
    for I := 0 to Cells.Count - 1 do
      TcxTreeListCustomCellViewInfo(Cells[I]).Scroll(DX, 0);
    for I := 0 to Count - 1 do
    begin
      NodeViewData[I].Scroll(DX, 0,
      Bands.VisibleLeftFixedCount > 0, Bands.VisibleRightFixedCount > 0);
    end;
    RecalculateHitTestCells;
    TreeList.ScrollWindow(DX, 0);
  end;
  FHScrollPos := APosition;
  TreeList.DoLeftPosChanged;
end;

procedure TcxTreeListViewInfo.SetLastNode(ANode: TcxTreeListNode);
var
  AHeight: Integer;
  ATopNode: TcxTreeListNode;
  ATopNodePixelScrollOffset: Integer;
begin
  if IsDirty then
    Calculate;
  NodesViewData.Clear;
  AHeight := Max(1, GetPixelScrollContentSize);
  FPixelScrollNodeOffset := 0;
  if IsRecordPixelScrollMode then
  begin
    GetPixelScrollTopNodeAndOffsetByBottomNode(ANode, ATopNode, ATopNodePixelScrollOffset);
    FPixelScrollNodeOffset := ATopNodePixelScrollOffset;
    if TreeList.TopVisibleNode <> ATopNode then
      TreeList.TopVisibleNode := ATopNode
    else
      TreeList.LayoutChanged;
  end
  else
  begin
    CalculateNodesViewDataBackward(ANode, False, AHeight);
    if (AHeight < 0) and (Count > 1) then
      TreeList.TopVisibleNode := NodeViewData[1].Node
    else
      TreeList.TopVisibleNode := NodeViewData[0].Node;
  end;
end;

procedure TcxTreeListViewInfo.SetRowOffset(
  AIndex: Integer; const AValue: Integer);
var
  I, D: Integer;
begin
  D := AValue - FRows^[AIndex];
  FRows^[AIndex] := AValue;
  if D > 0 then
  begin
    for I := AIndex + 1 to ColumnsRowCount do
      Inc(FRows^[I], D);
  end;
end;

procedure TcxTreeListViewInfo.SetVScrollPos(APosition: Integer);

  procedure CalculateTopNodeAndOffset(AValue: Integer;
    out ATopNode: TcxTreeListNode; out ATopNodePixelScrollOffset: Integer);
  var
    I: Integer;
    APrevPosition: Integer;
    ANode: TcxTreeListNode;
  begin
    APrevPosition := 0;
    ATopNode := nil;
    ATopNodePixelScrollOffset := 0;
//    if GridView.IsEqualScrollSizeRecords then
//    begin
//      ATopRecordIndex := AValue div ViewInfo.GetEqualHeightRecordScrollSize;
//      ATopNodePixelScrollOffset := - AValue mod ViewInfo.GetEqualHeightRecordScrollSize;
//    end
//    else
      for I := TreeList.AbsoluteVisibleCount - 1 downto 0 do
      begin
        ANode := TreeList.AbsoluteVisibleItems[I];
        if ANode.FPixelScrollPosition <= AValue then
          if ANode.FPixelScrollPosition >= APrevPosition then
          begin
            APrevPosition := ANode.FPixelScrollPosition;
            ATopNode := ANode;
            ATopNodePixelScrollOffset := ANode.FPixelScrollPosition - AValue;
          end
          else
            Break;
      end;
  end;

var
  ANode: TcxTreeListNode;
  AOffset: Integer;
begin
  if IsPixelBasedScrollDataPos then
  begin
    CalculateTopNodeAndOffset(APosition, ANode, AOffset);
    if (ANode <> TopNode) or (AOffset <> FPixelScrollNodeOffset) then
    begin
      FPixelScrollNodeOffset := AOffset;
      if TreeList.TopVisibleNode <> ANode then
        TreeList.TopVisibleNode := ANode
      else
        TreeList.LayoutChanged;
    end;
  end
  else
    TreeList.TopVisibleNode := TreeList.AbsoluteVisibleItems[Min(TreeList.AbsoluteVisibleCount - 1,
      Max(0, APosition))];
end;

{ TcxTreeListDataController }

constructor TcxTreeListDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodesCount := -1;
end;

function TcxTreeListDataController.AddItem(AItem: TObject): TcxCustomDataField;
begin
  Result := inherited AddItem(AItem);
  InsertValueDef(TcxTreeListColumn(AItem));
  TreeList.Controller.CancelIncSearching;
end;

function TcxTreeListDataController.AllocateRecord: Pointer;
begin
  Result := TcxDataStorageHelper.AllocateRecord(DataStorage);
  Inc(FAllocatedRecords);
end;

procedure TcxTreeListDataController.ChangeValueTypeClass(
  AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass);
var
  AColumn: TcxTreeListColumn;
begin
  if GetItemValueTypeClass(AItemIndex) = AValueTypeClass then Exit;
  AColumn := TcxTreeListColumn(TreeList.ContainerList[AItemIndex]);
  RemoveValueDef(AColumn);
  TcxCustomDataHelper.SetTextStored(Fields[AItemIndex], AColumn.GetIsTextStored);
  inherited ChangeValueTypeClass(AItemIndex, AValueTypeClass);
  InsertValueDef(AColumn);
end;

function TcxTreeListDataController.CompareNodesByColumns(
  ANode1, ANode2: TcxTreeListNode; AColumns: TList): Integer;
var
  I: Integer;
  V1, V2: Variant;
  AField: TcxCustomDataField;

  function GetNodeHandle(ANode: TcxTreeListNode): Pointer;
  begin
    if ANode = EditingNode then
      Result := Root.Handle
    else
      Result := ANode.Handle;
  end;

begin
  Result := Byte(AColumns.Count > 0);
  for I := 0 to AColumns.Count - 1 do
  begin
    AField := Fields[TcxTreeListColumn(AColumns[I]).ItemIndex];
    if IsConversionNeededForCompare(AField) then
    begin
      V1 := GetValueForCompare(ANode1, AField);
      V2 := GetValueForCompare(ANode2, AField);
      if VarIsStr(V1) and VarIsStr(V2) then
        Result := StringCompare(VarToStr(V1), VarToStr(V2))
      else
        Result := VarCompare(V1, V2);
    end
    else
      Result := Compare(TcxTreeListColumn(AColumns[I]).ValueDef,
        GetNodeHandle(ANode1), GetNodeHandle(ANode2));
    if TcxTreeListColumn(AColumns[I]).SortOrder = soDescending then
      Result := -Result;
    if Result <> 0 then Break;
  end;
  if Result = 0 then
  begin
    Result := ANode1.OriginalIndex - ANode2.OriginalIndex;
    if Result = 0 then
      Result := 1;
  end;
end;

procedure TcxTreeListDataController.FreeNodeRecord(ANode: TcxTreeListNode);
begin
  if ANode = nil then Exit;
  FreeRecord(ANode.FHandle);
  if ANode = EditingNode then
  begin
    if not DataChangedBusy then
      EditingNode := nil
    else
    begin
      FEditingNode := nil;
      FreeRecord(Root.FHandle);
    end;
  end;
end;

procedure TcxTreeListDataController.FreeRecord(var ARecord: Pointer);
begin
  if ARecord = nil then Exit;
  TcxDataStorageHelper.FreeRecord(DataStorage, ARecord);
  Dec(FAllocatedRecords);
end;

function TcxTreeListDataController.GetDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  Result := GetNodeByRecordIndex(ARecordIndex).Texts[AItemIndex]
end;

function TcxTreeListDataController.GetNodeDisplayText(ANode: TcxTreeListNode; AIndex: Integer): Variant;
begin
  if ANode.Handle = nil then
    Result := ''
  else
    Result := TcxDataStorageHelper.GetDisplayText(ValueDefs[AIndex], ANode.Handle);
end;

function TcxTreeListDataController.GetNodeValue(ANode: TcxTreeListNode; AIndex: Integer): Variant;
begin
  if ANode.Handle = nil then
    Result := Null
  else
    Result := TcxDataStorageHelper.GetValue(ValueDefs[AIndex], ANode.Handle);
end;

function TcxTreeListDataController.GetValue(ARecordIndex, AItemIndex: Integer): Variant;
var
  ANode: TcxTreeListNode;
begin
  if ARecordIndex = -1 then
    ANode := TreeList.FocusedNode
  else
    ANode := GetNodeByRecordIndex(ARecordIndex);
  Result := ANode.Values[AItemIndex];
end;

function TcxTreeListDataController.GetValueForCompare(
  ANode: TcxTreeListNode; AField: TcxCustomDataField): Variant;
begin
  if IsSortByDisplayTextNeeded(AField) then
    Result := TcxDataStorageHelper.GetDisplayText(
      TcxCustomDataHelper.GetValueDef(AField), ANode.Handle)
  else
    Result := TcxDataStorageHelper.GetValue(
      TcxCustomDataHelper.GetValueDef(AField), ANode.Handle);
end;

procedure TcxTreeListDataController.InitializeNodeFromRecordIndex(
  ANode: TcxTreeListNode);
begin
  ANode.Handle := TcxDataStorageHelper.RemoveRecord(
    DataStorage, Integer(ANode.Handle));
  if ANode.Handle <> nil then
    Inc(FAllocatedRecords);
end;

procedure TcxTreeListDataController.InitializeRecordIndexFromNode(
  ANode: TcxTreeListNode);
begin
  ANode.Handle := Pointer(TcxDataStorageHelper.AppendRecord(
    DataStorage, ANode.Handle));
end;

function TcxTreeListDataController.IsBookmarkAvailable: Boolean;
begin
  Result := FBookmarkNode <> nil;
end;

procedure TcxTreeListDataController.Cancel;
begin
  CancelEditing;
end;

procedure TcxTreeListDataController.Edit;
begin
  if not TreeList.OptionsData.Editing then Exit;
  if EditingNode <> TreeList.FocusedNode then
  begin
    EditingNode := TreeList.FocusedNode;
    FIsValueChanged := False;
  end;
end;

procedure TcxTreeListDataController.Post(AForcePost: Boolean = False);
begin
  if EditingNode = nil then Exit;
  TreeList.BeginUpdate;
  try
    PostEditingData;
    if not FIsValueChanged then
      Cancel
    else
    begin
      FreeNodeRecord(Root);
      TreeList.AddChanges([tcData, tcSummary]);
    end;
    try
      PostRecord;
    finally
      TreeList.DoDataChangedEvent(nil);
    end;
    EditingNode := nil;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListDataController.PostEditingData;
begin
  if (EditingNode = nil) or (Root.Handle = nil) or (nsDeleting in EditingNode.State) then Exit;
  TreeList.UpdateData;
  if IsValueChanged and (EditingNode <> nil) then
    Exclude(EditingNode.State, nsInserting);
  PostValues;
end;

procedure TcxTreeListDataController.RemoveItem(AItem: TObject);
begin
  RemoveValueDef(TcxTreeListColumn(AItem));
  TreeList.Controller.CancelIncSearching;
  inherited RemoveItem(AItem);
end;

function TcxTreeListDataController.SetEditValue(AItemIndex: Integer;
  const AValue: Variant; AEditValueSource: TcxDataEditValueSource): Boolean;
var
  V: Variant;
begin
  if EditingNode = nil then
  begin
    Result := False;
    Exit;
  end;
  TreeList.Controller.IncSearchText := '';
  AItemIndex := TcxTreeListColumn(TreeList.ContainerList[AItemIndex]).ItemIndex;
  V := AValue;
  if (AEditValueSource = evsText) and (VarToStr(AValue) = '') then
    V := Null;
  SetNodeValue(EditingNode, AItemIndex, V);
  Result := True;
end;

procedure TcxTreeListDataController.SetNodeValue(
  ANode: TcxTreeListNode; AIndex: Integer; const AValue: Variant);
var
  AColumn: TcxTreeListColumn;
begin
  if ANode = nil then Exit;
  if ANode.Handle = nil then
    ANode.InitializeHandle;
  if ANode.Handle = nil then Exit;
  AColumn := TreeList.Columns[AIndex];
  TcxDataStorageHelper.SetValue(ANode.Handle, AColumn.ValueDef, AValue);
  FIsValueChanged := True;
  if IsLoading then Exit;
  TreeList.AddChanges([tcSummary]);
  TreeList.DoNodeChanged(ANode, AColumn);
end;

procedure TcxTreeListDataController.SetValue(
  ARecordIndex, AItemIndex: Integer; const Value: Variant);
begin
  cxTLUnused;
end;

procedure TcxTreeListDataController.UpdateItemIndexes;
begin
 inherited UpdateItemIndexes;
 TreeList.AddChanges([tcColumns]);
end;

procedure TcxTreeListDataController.CancelEditing;
begin
  if EditingNode = nil then Exit;
  try
    if not EditingNode.Deleting and not EditingNode.Inserting and IsValueChanged then
      SetNodeData(Root.FHandle, EditingNode);
  finally
    FreeNodeRecord(Root);
    EditingNode := nil;
  end;
end;

function TcxTreeListDataController.Compare(
  AValueDef: TcxValueDef; AHandle1, AHandle2: Pointer): Integer;
begin
  if AHandle1 = nil then
  begin
    if AHandle2 = nil then
      Result := 0
    else
      Result := -1;
  end
  else
    if AHandle2 = nil then
      Result := 1
    else
      Result := TcxDataStorageHelper.Compare(AValueDef, AHandle1, AHandle2);
end;

function TcxTreeListDataController.CopyData(ASource: TcxTreeListNode): Pointer;
var
  I: Integer;
begin
  Result := AllocateRecord;
  for I := 0 to TreeList.ColumnCount - 1 do
  begin
    TcxDataStorageHelper(DataStorage).SetValue(Result,
      ValueDefs[I], GetNodeValue(ASource, I));
  end;
end;

procedure TcxTreeListDataController.DeleteNode(ANode: TcxTreeListNode);
begin
  if (ANode.FAbsoluteIndex >= 0) and (ANode.FAbsoluteIndex < TreeList.FAbsoluteItems.Count) then
    TreeList.FAbsoluteItems[ANode.FAbsoluteIndex] := nil;
  ANode.Free;
end;

procedure TcxTreeListDataController.DestroyHandles;
begin
  ForEachRecord(FreeRecordProc, nil);
end;

procedure TcxTreeListDataController.DoFilterNodes;
begin
  BeginFilterProcess;
  try
    UpdateFilteringInfo;
    ForEachNode(DoFilterNodeProc, nil);
    FFiltered := True;
  finally
    EndFilterProcess;
  end;
end;

function TcxTreeListDataController.DoFilterRecordEvent(ARecordIndex: Integer): Boolean;
begin
  Result := TreeList.DoFilterNodeEvent(GetNodeByRecordIndex(ARecordIndex));
end;

procedure TcxTreeListDataController.FilterChanged;
begin
  TreeList.FilterChanged;
end;

procedure TcxTreeListDataController.FilterNodes;
begin
  if HasFiltering then
    DoFilterNodes
  else
    if Filtered then
      ResetNodesFiltering;
end;

procedure TcxTreeListDataController.FindFilterChanged;
begin
  TreeList.FindFilterChanged;
end;

procedure TcxTreeListDataController.ForEachNode(
  AProc: TcxTreeListForEachNodeProc; AData: Pointer);
var
  ANode: TcxTreeListNode;
begin
  if Root = nil then Exit;
  ANode := Root;
  while ANode <> nil do
  begin
    AProc(ANode, AData);
    if ANode.FFirst <> nil then
      ANode := ANode.FFirst
    else
      if ANode.FNext <> nil then
        ANode := ANode.FNext
      else
      begin
        while (ANode <> nil) and (ANode.FNext = nil) do
          ANode := ANode.Parent;
        if ANode <> nil then
          ANode := ANode.FNext;
      end;
  end;
end;

procedure TcxTreeListDataController.ForEachRecord(
  AProc: TcxTreeListForEachNodeProc; AData: Pointer);
begin
  if AllocatedRecords = 0 then Exit;
  ForEachNode(AProc, AData);
end;

function TcxTreeListDataController.GetEditState: TcxDataControllerEditState;
begin
  Result := [];
  if TreeList.IsEditing then
    Include(Result, dceEdit);
  if TreeList.IsInserting then
    Include(Result, dceInsert);
  if (Result <> []) and IsValueChanged then
    Result := Result + [dceModified];
end;

function TcxTreeListDataController.GetFilteredIndexByRecordIndex(ARecordIndex: Integer): Integer;
begin
  Result := TreeList.AbsoluteVisibleItemsList.IndexOf(GetNodeByRecordIndex(ARecordIndex));
end;

function TcxTreeListDataController.GetFilteredRecordCount: Integer;
begin
  Result := TreeList.AbsoluteVisibleCount;
end;

function TcxTreeListDataController.GetFilteredRecordIndex(Index: Integer): Integer;
begin
  Result := TreeList.AbsoluteVisibleItems[Index].AbsoluteIndex;
end;

function TcxTreeListDataController.GetFilteringRecordCount: Integer;
begin
  Result := TreeList.AbsoluteCount;
end;

function TcxTreeListDataController.GetInternalValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
begin
  Result := GetNodeByRecordIndex(ARecordIndex).Values[AField.Index];
end;

function TcxTreeListDataController.GetNodeByRecordIndex(ARecordIndex: Integer): TcxTreeListNode;
begin
  Result := TreeList.AbsoluteItems[ARecordIndex];
end;

function TcxTreeListDataController.GetRecordIndexByNode(ANode: TcxTreeListNode): Integer;
begin
  Result := ANode.AbsoluteIndex;
end;

function TcxTreeListDataController.HasFilterEvent: Boolean;
begin
  Result := TreeList.HasFilterEvent;
end;

function TcxTreeListDataController.IsHiddenByFilter(ANode: TcxTreeListNode): Boolean;
begin
  Result := not ANode.IsRoot and not DoGlobalFilterRecord(GetRecordIndexByNode(ANode));
end;

procedure TcxTreeListDataController.ResetNodesFiltering;
begin
  BeginFilterProcess;
  try
    ForEachNode(ResetNodeFilteringProc, nil);
    FFiltered := False;
  finally
    EndFilterProcess;
  end;
end;

function TcxTreeListDataController.InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TcxTreeListDataController.InternalClearBookmark;
begin
  FBookmarkNode := nil;
end;

procedure TcxTreeListDataController.InternalGotoBookmark;
begin
  FBookmarkNode.MakeVisible;
  TreeList.FocusedNode := FBookmarkNode;
end;

function TcxTreeListDataController.InternalSaveBookmark: Boolean;
var
  ANode: TcxTreeListNode;
begin
  Result := False;
  ANode := TreeList.FocusedNode;
  if FBookmarkNode <> ANode then
  begin
    FBookmarkNode := ANode;
    Result := True;
  end;
end;

function TcxTreeListDataController.IsDataMode: Boolean;
begin
  Result := False;
end;

function TcxTreeListDataController.IsLoading: Boolean;
begin
  Result := inherited IsLoading;
end;

procedure TcxTreeListDataController.PostRecord;
begin
end;

procedure TcxTreeListDataController.PostValues;
begin
end;

procedure TcxTreeListDataController.SetNodeData(
  ASource: Pointer; ADest: TcxTreeListNode);
var
  I: Integer;
begin
  for I := 0 to TreeList.ColumnCount - 1 do
    SetNodeValue(ADest, I, TcxDataStorageHelper.GetValue(ValueDefs[I], ASource));
end;

function TcxTreeListDataController.StringCompare(const V1, V2: Variant): Integer;
begin
  if dcoAnsiSort in Options then
  begin
    if dcoCaseInsensitive in Options then
      Result := AnsiCompareText(V1, V2)
    else
      Result := AnsiCompareStr(V1, V2);
  end
  else
  begin
    if dcoCaseInsensitive in Options then
      Result := CompareStr(AnsiUpperCase(V1), AnsiUpperCase(V2))
    else
      Result := VarCompare(V1, V2);
  end;
end;

procedure TcxTreeListDataController.SyncFocused(ANode: TcxTreeListNode);
begin
end;

procedure TcxTreeListDataController.BeginFilterProcess;
begin
  if tcStructure in TreeList.Changes then
    TreeList.CheckStructure;
  Inc(FFilterProcessCount);
end;

procedure TcxTreeListDataController.EndFilterProcess;
begin
  Dec(FFilterProcessCount);
  if tcStructure in TreeList.Changes then
    TreeList.CheckStructure;
end;

function TcxTreeListDataController.IsFilterProcessing: Boolean;
begin
  Result := FFilterProcessCount > 0;
end;

procedure TcxTreeListDataController.DoFilterNodeProc(ANode: TcxTreeListNode; Data: Pointer);
begin
  ANode.HiddenByFilter := IsHiddenByFilter(ANode);
end;

function TcxTreeListDataController.GetHasEditData: Boolean;
begin
  Result := Root.Handle <> nil;
end;

function TcxTreeListDataController.GetRoot: TcxTreeListNode;
begin
  Result := TreeList.Root;
end;

function TcxTreeListDataController.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(GetOwner);
end;

function TcxTreeListDataController.GetValueDef(AIndex: Integer): TcxValueDef;
begin
  Result := TreeList.Columns[AIndex].ValueDef;
end;

procedure TcxTreeListDataController.FreeRecordProc(
  ANode: TcxTreeListNode; AData: Pointer);
begin
  FreeNodeRecord(ANode);
end;

procedure TcxTreeListDataController.InsertValueDef(AColumn: TcxTreeListColumn);
var
  AData: TcxTreeListValueDefData;
begin
  if AColumn.ValueDef = nil then Exit;
  AData.Index := AColumn.ValueDef.ValueDefs.Count;
  AColumn.InitializeValueDef;
  AData.ValueDef := AColumn.ValueDef;
  ForEachRecord(InsertValueDefProc, @AData);
end;

procedure TcxTreeListDataController.InsertValueDefProc(
  ANode: TcxTreeListNode; AData: Pointer);
var
  PSource: PAnsiChar;
begin
  if ANode.FHandle = nil then Exit;
  ReallocMem(ANode.FHandle, PcxTreeListValueDefData(AData)^.ValueDef.ValueDefs.RecordSize);
  PSource := PAnsiChar(ANode.FHandle) + PcxTreeListValueDefData(AData)^.ValueDef.Offset;
  FillChar(PSource^, PcxTreeListValueDefData(AData)^.ValueDef.BufferSize, 0);
end;

procedure TcxTreeListDataController.SetEditingNode(AValue: TcxTreeListNode);
var
  APrevNode: TcxTreeListNode;
begin
  APrevNode := FEditingNode;
  if (AValue <> FEditingNode) and (FEditingNode <> nil) then
  begin
    Exclude(EditingNode.State, nsEditing);
    if not HasEditData and (nsInserting in EditingNode.State) then
    begin
      FEditingNode := nil;
      TreeList.FAbsoluteItems.Remove(APrevNode);
      APrevNode.Free;
    end
    else
        if HasEditData then
        begin
          if EditingNode.Deleting then
          begin
            if not (nsInternalDelete in EditingNode.State) then
              Cancel
            else
              CancelEditing
          end
          else
            Post
        end
        else
          TreeList.DoNodeChanged(EditingNode, nil);
  end;
  FEditingNode := AValue;
  if (FEditingNode <> nil) and ((APrevNode <> AValue) or not HasEditData) then
  begin
    Root.Handle := CopyData(AValue);
    Include(EditingNode.State, nsEditing);
    TreeList.DoNodeChanged(EditingNode, nil);
  end;
end;

procedure TcxTreeListDataController.SetIsValueChanged(AValue: Boolean);
begin
  FIsValueChanged := AValue;
end;

procedure TcxTreeListDataController.RemoveValueDef(AColumn: TcxTreeListColumn);
var
  AData: TcxTreeListValueDefData;
begin
  AData.ValueDef := AColumn.ValueDef;
  ForEachRecord(RemoveValueDefProc, @AData);
end;

procedure TcxTreeListDataController.RemoveValueDefProc(
  ANode: TcxTreeListNode; AData: Pointer);
var
  AFreeRecord: Boolean;
  ARecordSize: Integer;
  PDest, PSource: PAnsiChar;
begin
  if (ANode.FHandle = nil) or (PcxTreeListValueDefData(AData)^.ValueDef = nil) then Exit;
  AFreeRecord := TcxDataStorageHelper.GetStoredCount(DataStorage) <= 1;
  ARecordSize := TcxDataStorageHelper.GetRecordSize(DataStorage);
  if AFreeRecord then
    FreeNodeRecord(ANode)
  else
    with PcxTreeListValueDefData(AData)^ do
    begin
      TcxDataStorageHelper.FreeBuffer(ValueDef, ANode.FHandle);
      PDest := PAnsiChar(ANode.FHandle) + ValueDef.Offset;
      PSource := PAnsiChar(PDest) + ValueDef.BufferSize;
      System.Move(PSource^, PDest^, ARecordSize - (PSource - PAnsiChar(ANode.FHandle)));
      ReallocMem(ANode.FHandle, ARecordSize - ValueDef.BufferSize);
    end;
end;

procedure TcxTreeListDataController.ResetNodeFilteringProc(ANode: TcxTreeListNode; AData: Pointer);
begin
  ANode.HiddenByFilter := False;
end;

{ TcxTreeListNodeViewData }

constructor TcxTreeListNodeViewData.Create(
  AViewInfo: TcxTreeListViewInfo; ANode: TcxTreeListNode; ACapacity: Integer; AFake: Boolean = False);
begin
  ViewInfo := AViewInfo;
  Node := ANode;
  FFake := AFake;
  Focused := (ANode <> nil) and ANode.Focused;
  Selected := (ANode <> nil) and ANode.Selected;
  if ANode <> nil then
    Node.FViewData := Self;
  Cells := TcxCustomControlCells.Create;
  Cells.Capacity := ACapacity;
end;

destructor TcxTreeListNodeViewData.Destroy;
begin
  if not ViewInfo.TreeList.IsDestroying and not FFake then
    ViewInfo.CellsChanged;
  FreeAndNil(Cells);
  if Node <> nil then
    Node.FViewData := nil;
  inherited Destroy;
end;

procedure TcxTreeListNodeViewData.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, AClientBounds);
  ContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(ContentBounds, AClientBounds);
  FocusRectBounds := TdxRightToLeftLayoutConverter.ConvertRect(FocusRectBounds, AClientBounds);
  Cells.RightToLeftConversion(AClientBounds);
end;

procedure TcxTreeListNodeViewData.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not FIsRightToLeftConverted then
    DoRightToLeftConversion(AClientBounds);
  FIsRightToLeftConverted := True;
end;

function TcxTreeListNodeViewData.GetRealBounds: TRect;
begin
  Result := cxRectOffset(Bounds, Origin);
end;

function TcxTreeListNodeViewData.GetRealContentBounds: TRect;
begin
  Result := cxRectOffset(ContentBounds, Origin);
end;

procedure TcxTreeListNodeViewData.InvalidateIndents;
var
  I: Integer;
begin
  for I := 0 to Cells.Count - 1 do
    if Cells[I].Visible and (Cells[I].CustomDrawID in [cxtlIndentCell, cxtlIndicatorCell]) then
      Cells[I].Invalidate(cxRectOffset(Cells[I].VisibleRect, Origin));
end;

procedure TcxTreeListNodeViewData.Paint(
  ACanvas: TcxCanvas; AHandler: TcxCustomDrawCellEvent);
var
  I: Integer;
  AWindowOrg: TPoint;
  AClipRgn, AFooterRgn: TcxRegion;
begin
  if not ACanvas.RectVisible(cxRectInflate(GetRealBounds, 1, 1)) and (GetRealBounds.Top <> 39) then Exit;
  AClipRgn := ACanvas.GetClipRegion();
  try
    AWindowOrg := ACanvas.WindowOrg;
    ACanvas.WindowOrg := cxPointInvert(Origin);
    Cells.Paint(ACanvas, AHandler);
    if DrawFocusRect and Node.TreeList.OptionsView.FocusRect and Node.Focused and
      (not Node.TreeList.OptionsSelection.HideFocusRect or Node.TreeList.IsFocused) then
    begin
      for I := 0 to Cells.Count - 1 do
       if Cells[I].Visible and (Cells[I].CustomDrawID = cxtlIndentCell) then
         ACanvas.ExcludeClipRect(Cells[I].VisibleRect);
      ACanvas.DrawFocusRect(FocusRectBounds);
    end;
    if CheckFooterRgn then
    begin
      for I := 0 to Cells.Count - 1 do
        if Cells[I].Visible and (Cells[I].CustomDrawID = cxtlBandPartCell) and
          (TcxTreeListBandCellViewInfo(Cells[I]).Part in [tlbpGroupFooter, tlbpFooter]) then
        begin
          AFooterRgn := TcxRegion.Create(cxRectOffset(TcxTreeListBandCellViewInfo(Cells[I]).ClipRect, Origin));
          AClipRgn.Combine(AFooterRgn, roSubtract);
        end;
    end;
    ACanvas.WindowOrg := AWindowOrg;
  finally
    ACanvas.SetClipRegion(AClipRgn, roSet);
  end;
end;

procedure TcxTreeListNodeViewData.Scroll(DX, DY: Integer;
  ALeftFixed, ARightFixed: Boolean);
var
  I: Integer;
begin
  for I := 0 to Cells.Count - 1 do
    TcxTreeListCustomCellViewInfo(Cells[I]).Scroll(DX, 0);
  Origin := cxPointOffset(Origin, 0, DY);
  if not ARightFixed then
  begin
    Inc(ContentBounds.Right, DX);
    Inc(FocusRectBounds.Right, DX);
  end;
  if not ALeftFixed then
  begin
    Inc(ContentBounds.Left, DX);
    Inc(FocusRectBounds.Left, DX);
  end;
 if (Node <> nil) and (Node.IsGroupNode or Focused) then
    Node.Invalidate;
end;

function TcxTreeListNodeViewData.Update(AForceUpdate: Boolean): Boolean;
var
  I: Integer;
  R: TRect;
begin
  Result := (Node <> nil) and ((Selected or Node.Selected) or Focused or Node.Focused or AForceUpdate);
  Selected := (Node <> nil) and Node.Selected;
  Focused := (Node <> nil) and Node.Focused;
  if not Result then Exit;
  for I := 0 to Cells.Count - 1 do
    Cells[I].VisibleInfoCalculated := False;
  R := GetRealBounds;
  R.Left := 0;
  Node.TreeList.InvalidateRect(cxRectInflate(R, 1, 1), False);
end;

{ TcxTreeListItemsCustomizeListBox }

constructor TcxTreeListItemsCustomizeListBox.CreateEx(AOwner: TComponent;
  ATreeList: TcxCustomTreeList; AParent: TWinControl);
begin
  inherited Create(AOwner);
  FTreeList := ATreeList;
  Align := alClient;
  Parent := AParent;
  Style.HotTrack := False;
  Style.LookAndFeel.MasterLookAndFeel := TreeList.LookAndFeel;
  Style.TransparentBorder := False;
  Sorted := True;
  ParentFont := True;
  OnMeasureItem := MeasureItem;
  InnerListBox.OnDrawItem := DoDrawItemEx;
  OnDrawItem := DoDrawItem;
  ListStyle := lbOwnerDrawVariable;
  ItemHeight := GetItemHeight(0);
  FHeaders := TcxObjectList.Create;
  UpdateBackgroundColor;
  RefreshList;
end;

destructor TcxTreeListItemsCustomizeListBox.Destroy;
begin
  TreeList.Controller.DragItem := nil;
  FreeAndNil(FHeaders);
  inherited Destroy;
end;

procedure TcxTreeListItemsCustomizeListBox.AddItem(
  ACaption: TcxTreeListCaption);
var
  ACell: TcxTreeListHeaderCellViewInfo;
begin
  ACell := CreateCell;
  ACell.Initialize(ACaption);
  ACell.IsRightToLeftConverted := TreeList.ViewInfo.IsRightToLeftConverted;
  Items.AddObject(ACaption.Text, ACell);
  FHeaders.Add(ACell);
end;

procedure TcxTreeListItemsCustomizeListBox.DblClick;
var
  AIndex: Integer;
begin
  AIndex := ItemAtPos(ScreenToClient(GetMouseCursorPos), True);
  if AIndex >= 0 then
  begin
    FinishDragAndDrop(False);
    MakeItemVisible(Headers[AIndex]);
  end;
end;

procedure TcxTreeListItemsCustomizeListBox.DoDrawItem(AControl: TcxListBox;
  ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  ASaveState: TcxButtonState;
begin
  ASaveState := Headers[AIndex].State;
  try
    UpdateHeaderInfo(AIndex);
    if Headers[AIndex].Visible then
    begin
      if [odFocused, odSelected] * AState <> [] then
        Headers[AIndex].FState := cxbsPressed;
      Headers[AIndex].Draw(ACanvas);
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    Headers[AIndex].FState := ASaveState;
  end;
end;

procedure TcxTreeListItemsCustomizeListBox.DoDrawItemEx(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
end;

function TcxTreeListItemsCustomizeListBox.GetItemHeight(AItem: Integer): Integer;
begin
  Result := Painter.ScaledHeaderHeight(cxTextHeight(Font), ScaleFactor);
end;

function TcxTreeListItemsCustomizeListBox.GetOrigin(AHeader: TcxTreeListHeaderCellViewInfo): TPoint;
begin
  Result := ClientToScreen(AHeader.BoundsRect.TopLeft);
end;

procedure TcxTreeListItemsCustomizeListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if DragAndDropState <> ddsNone then
    FinishDragAndDrop(False);
  inherited KeyDown(Key, Shift);
end;

procedure TcxTreeListItemsCustomizeListBox.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateBackgroundColor;
end;

procedure TcxTreeListItemsCustomizeListBox.MeasureItem(AControl: TcxListBox;
  Index: Integer; var Height: Integer);
begin
  Height := GetItemHeight(Index);
end;

procedure TcxTreeListItemsCustomizeListBox.Paint;
begin
  with Painter do
  begin
    DrawBorder(Canvas, ClientRect);
    IntersectClipRect(Handle, BorderSize, BorderSize,
      ClientWidth - BorderSize, ClientHeight - BorderSize);
    inherited Paint;
  end;
end;

procedure TcxTreeListItemsCustomizeListBox.RefreshList;
begin
  if (csDestroying in ComponentState) or
    (DragAndDropState = ddsInProcess) then Exit;
  Items.BeginUpdate;
  try
    Clear;
    FHeaders.Clear;
    PopulateItems;
    Resize;
  finally
    Items.EndUpdate;
  end;
end;

procedure TcxTreeListItemsCustomizeListBox.Resize;
begin
  inherited Resize;
  UpdateHeaders;
  Invalidate;
end;

function TcxTreeListItemsCustomizeListBox.StartDragAndDrop(
  const P: TPoint): Boolean;
var
  AIndex: Integer;
begin
  AIndex := ItemAtPos(P, True);
  Result := AIndex >= 0;
  if Result then
    TreeList.Controller.DragItem := Headers[AIndex];
end;

procedure TcxTreeListItemsCustomizeListBox.UpdateBackgroundColor;
begin
  Color := Style.LookAndFeel.Painter.GetCustomizationFormListBackgroundColor;
end;

procedure TcxTreeListItemsCustomizeListBox.UpdateHeaderInfo(AIndex: Integer);
begin
  Headers[AIndex].SetBounds(Rect(0, (AIndex - TopIndex) * ItemHeight,
    cxRectWidth(InnerListBox.ClientRect), (AIndex - TopIndex + 1) * ItemHeight), ClientRect);
end;

procedure TcxTreeListItemsCustomizeListBox.UpdateHeaders;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    UpdateHeaderInfo(I);
end;

function TcxTreeListItemsCustomizeListBox.GetHeader(
  AIndex: Integer): TcxTreeListHeaderCellViewInfo;
begin
  Result := Items.Objects[AIndex] as TcxTreeListHeaderCellViewInfo;
end;

function TcxTreeListItemsCustomizeListBox.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TreeList.LookAndFeel.Painter;
end;

{ TcxTreeListColumnsCustomizeListBox }

function TcxTreeListColumnsCustomizeListBox.CreateCell: TcxTreeListHeaderCellViewInfo;
begin
  Result := TcxTreeListColumnHeaderCellViewInfo.CreateEx(TreeList, ClientRect, ClientRect);
end;

function TcxTreeListColumnsCustomizeListBox.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TcxTreeListDragAndDropColumnObject;
end;

procedure TcxTreeListColumnsCustomizeListBox.MakeItemVisible(
  AHeader: TcxTreeListHeaderCellViewInfo);
var
  ABand: TcxTreeListBand;
  AColumn: TcxTreeListColumn;
begin
  ABand := TcxTreeListColumnHeaderCellViewInfo(AHeader).Band;
  if (ABand <> nil) and ABand.ActuallyVisible then
  begin
    AColumn := TcxTreeListColumnHeaderCellViewInfo(AHeader).Column;
    AColumn.Visible := True;
    TreeList.DoColumnPosChanged(AColumn);
  end;
end;

procedure TcxTreeListColumnsCustomizeListBox.PopulateItems;
var
  I: Integer;
begin
  for I := 0 to TreeList.ColumnCount - 1 do
    if not TreeList.Columns[I].ActuallyVisible and not TreeList.Columns[I].Options.Hidden then
      AddItem(TreeList.Columns[I].Caption);
end;

{ TcxTreeListBandsCustomizeListBox }

function TcxTreeListBandsCustomizeListBox.CreateCell: TcxTreeListHeaderCellViewInfo;
begin
  Result := TcxTreeListBandHeaderCellViewInfo.CreateEx(
    TreeList, ClientRect, ClientRect);
end;

function TcxTreeListBandsCustomizeListBox.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TcxTreeListDragAndDropBandObject;
end;

procedure TcxTreeListBandsCustomizeListBox.MakeItemVisible(
  AHeader: TcxTreeListHeaderCellViewInfo);
var
  ABand: TcxTreeListBand;
begin
  ABand := TcxTreeListBandHeaderCellViewInfo(AHeader).Band;
  if (ABand.ParentBand = nil) or ABand.ParentBand.ActuallyVisible then
  begin
    ABand.Visible := True;
    TreeList.DoBandPosChanged(ABand);
  end;
end;

procedure TcxTreeListBandsCustomizeListBox.PopulateItems;
var
  I: Integer;
begin
  for I := 0 to TreeList.Bands.Count - 1 do
    if not TreeList.Bands[I].ActuallyVisible and not TreeList.Bands[I].Options.Hidden then
      AddItem(TreeList.Bands[I].Caption);
end;

{ TcxTreeListCustomizationForm }

constructor TcxTreeListCustomizationForm.CreateEx(AOwner: TcxTreeListCustomizing);
begin
  CreateNew(nil);
  FOwner := AOwner;
  FHookTimer := cxCreateTimer(TimerHandler, 10, False);
  Visible := False;
end;

destructor TcxTreeListCustomizationForm.Destroy;
begin
  Customizing.FinishCustomizing(BoundsRect.TopLeft, ScaleFactor.Revert(cxSize(BoundsRect)));
  FreeAndNil(FHookTimer);
  inherited Destroy;
end;

function TcxTreeListCustomizationForm.GetTreeList: TcxCustomTreeList;
begin
  Result := Customizing.TreeList;
end;

procedure TcxTreeListCustomizationForm.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  Refresh;
end;

procedure TcxTreeListCustomizationForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_POPUP;
    if not TreeList.IsDestroying then
      WndParent := TreeList.Handle;
  end;
end;

procedure TcxTreeListCustomizationForm.FinishDragDrop(Sender: TcxTreeListItemsCustomizeListBox);
begin
  FDragDropListBox := nil
end;

procedure TcxTreeListCustomizationForm.StartDragDrop(Sender: TcxTreeListItemsCustomizeListBox);
begin
  FDragDropListBox := Sender;
end;

procedure TcxTreeListCustomizationForm.TimerHandler(Sender: TObject);
begin
  if IsIconic(Application.Handle) then
    Visible := False
  else
    if not IsControlVisible(TreeList) then
      Customizing.Visible := False
    else
      if not Visible then
      begin
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
        Visible := True;
      end;
  if not Active and (DragDropListBox <> nil) then
    DragDropListBox.FinishDragAndDrop(False);
end;

{ TcxTreeListCustomizing }

constructor TcxTreeListCustomizing.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FRowCount := 10;
  FSize := cxNullSize;
  FPosition := cxPoint(-2000, -2000);
  TreeList.Designers.Add(Self);
end;

destructor TcxTreeListCustomizing.Destroy;
begin
  TreeList.Designers.Remove(Self);
  Visible := False;
  inherited Destroy;
end;

procedure TcxTreeListCustomizing.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListCustomizing then
  begin
    RowCount := TcxTreeListCustomizing(Source).RowCount;
    Visible := TcxTreeListCustomizing(Visible).Visible;
  end;
end;

procedure TcxTreeListCustomizing.MakeBandPageVisible;
begin
  Visible := True;
  if (FBandListBox <> nil) and (FBandListBox.Parent = FTabSheetBands) then
    FPageControl.ActivePage := FTabSheetBands;
end;

procedure TcxTreeListCustomizing.MakeColumnPageVisible;
begin
  Visible := True;
  if FHeaderListBox.Parent = FTabSheetHeaders then
    FPageControl.ActivePage := FTabSheetHeaders;
end;

function TcxTreeListCustomizing.PtInCustomizingBox(const APoint: TPoint): Boolean;
begin
  Result := Visible and (FForm <> nil) and PtInRect(FForm.BoundsRect, APoint);
end;

procedure TcxTreeListCustomizing.BiDiModeChanged;
begin
  if FForm <> nil then
    FForm.BiDiMode := TreeList.BiDiMode;
end;

procedure TcxTreeListCustomizing.CalculateSize(var AWidth, AHeight: Integer);
begin
  AWidth := TreeList.ScaleFactor.Apply(200);
  AHeight := TreeList.ScaleFactor.Apply(300);
end;

procedure TcxTreeListCustomizing.Close(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TcxTreeListCustomizing.ChangeScale(M, D: Integer);
begin
  FSize := cxSizeScale(FSize, M, D);
end;

procedure TcxTreeListCustomizing.CreateControls;
begin
  if not SimpleMode then
  begin
    FPageControl := TcxPageControl.Create(FForm);
    FPageControl.HotTrack := True;
    FPageControl.Focusable := False;
    FPageControl.LookAndFeel.MasterLookAndFeel := TreeList.LookAndFeel;
    FPageControl.Parent := FForm;
    FPageControl.BoundsRect := cxTextRect(FForm.ClientRect);
    FPageControl.Anchors := [akTop, akLeft, akRight, akBottom];
    // bands
    FTabSheetBands := TcxTabSheet.Create(FForm);
    FTabSheetBands.Caption := cxGetResourceString(@scxBandsCaption);
    FTabSheetBands.PageControl := FPageControl;
    FTabSheetBands.TabVisible := True;
    FBandListBox := TcxTreeListBandsCustomizeListBox.CreateEx(FForm, TreeList, FTabSheetBands);
    // columns
    FTabSheetHeaders := TcxTabSheet.Create(FForm);
    FTabSheetHeaders.Caption := cxGetResourceString(@scxColumnsCaption);
    FTabSheetHeaders.PageControl := FPageControl;
    FTabSheetHeaders.Parent := FPageControl;
    FHeaderListBox := TcxTreeListColumnsCustomizeListBox.CreateEx(FForm, TreeList, FTabSheetHeaders);
  end
  else
    FHeaderListBox := TcxTreeListColumnsCustomizeListBox.CreateEx(FForm, TreeList, FForm);
end;

procedure TcxTreeListCustomizing.CreateCustomizingForm;
const
  UnusedItems: array[0..4, 0..1] of Integer = (
    (7, MF_BYPOSITION), (5, MF_BYPOSITION), (SC_MAXIMIZE, MF_BYCOMMAND),
    (SC_MINIMIZE, MF_BYCOMMAND), (SC_RESTORE, MF_BYCOMMAND)
  );
var
  I: Integer;
begin
  FForm := TcxTreeListCustomizationForm.CreateEx(Self);
  FForm.ScaleForPPI(TreeList.ScaleFactor.TargetDPI);
  FForm.BiDiMode := TreeList.BiDiMode;
  FForm.Caption := cxGetResourceString(@scxCustomizeCaption);
  FForm.BorderStyle := bsSizeToolWin;
  FForm.BorderIcons := [biSystemMenu];
  FForm.Font.Assign(TreeList.Font);
  FForm.Color := clBtnFace;
  FForm.OnClose := Close;
  for I := 0 to High(UnusedItems) do
    DeleteMenu(GetSystemMenu(FForm.Handle, False), UnusedItems[I, 0], UnusedItems[I, 1]);
  CreateControls;
end;

procedure TcxTreeListCustomizing.FinishCustomizing(const APosition: TPoint; const ASize: TSize);
begin
  FPosition := APosition;
  FSize := TreeList.ScaleFactor.Apply(ASize);
  FIsPositionAssigned := True;
  FBandListBox := nil;
  FHeaderListBox := nil;
  FPageControl := nil;
  FTabSheetBands := nil;
  FTabSheetHeaders := nil;
  FForm := nil;
  TreeList.DoCustomizationVisibleChanged;
end;

procedure TcxTreeListCustomizing.HideCustomizingForm;
begin
  if Visible then
  begin
    FForm.HookTimer.Enabled := False;
    FreeAndNil(FForm);
  end;
end;

procedure TcxTreeListCustomizing.RefreshInformation;
begin
  if FBandListBox <> nil then
    FBandListBox.UpdateHeaders;
  if FHeaderListBox <> nil then
    FHeaderListBox.UpdateHeaders;
end;

procedure TcxTreeListCustomizing.SetControlParent(AControl, AParent: TWinControl);
begin
  AControl.Parent := AParent;
  if AParent <> nil then
  begin
    AControl.BoundsRect := AParent.ClientRect;
    Visible := True;
  end;
end;

procedure TcxTreeListCustomizing.ShowCustomizingForm;
var
  AForm: TCustomForm;
begin
  if Visible then
    Exit;
  if FForm = nil then
    CreateCustomizingForm;
  if not IsPositionAssigned then
  begin
    AForm := GetParentForm(TreeList);
    if not TreeList.UseRightToLeftAlignment then
      if AForm <> nil then
        FPosition := AForm.BoundsRect.BottomRight
      else
        FPosition := TreeList.BoundsRect.BottomRight
    else
      if AForm <> nil then
        FPosition := cxRectLeftBottom(AForm.BoundsRect)
      else
        FPosition := cxRectLeftBottom(TreeList.BoundsRect);

    CalculateSize(FSize.cx, FSize.cy);
    if not TreeList.UseRightToLeftAlignment then
      FPosition := cxPointOffset(FPosition, FSize.cx, FSize.cy, False)
    else
      FPosition := cxPointOffset(FPosition, 0, FSize.cy, False);
  end;
  FForm.ScaleForPPI(TreeList.ScaleFactor.TargetDPI);
  FForm.BoundsRect := cxRectBounds(FPosition, FSize.cx, FSize.cy);
  FForm.Show;
  FForm.HookTimer.Enabled := True;
  TreeList.DoCustomizationVisibleChanged;
end;

procedure TcxTreeListCustomizing.ComponentModified;
begin
  if Visible then
  begin
    if FHeaderListBox <> nil then
      FHeaderListBox.RefreshList;
    if FBandListBox <> nil then
      FBandListBox.RefreshList;
  end
end;

procedure TcxTreeListCustomizing.ComponentRemoved(Sender: TObject);
begin
end;

procedure TcxTreeListCustomizing.Update;
begin
  if FBandListBox <> nil then
    FBandListBox.Invalidate;
  if FHeaderListBox <> nil then
    FHeaderListBox.Invalidate;
end;

function TcxTreeListCustomizing.GetForm: TForm;
begin
  Result := FForm;
end;

function TcxTreeListCustomizing.GetPosition: TPoint;
begin
  if Visible then
    Result := Point(Form.Top, Form.Left)
  else
    Result := FPosition;
end;

function TcxTreeListCustomizing.GetTreeList: TcxCustomTreeList;
begin
  Result := GetOwner as TcxCustomTreeList;
end;

function TcxTreeListCustomizing.GetIsSimpleMode: Boolean;
begin
  Result := TreeList.OptionsView.SimpleCustomizeBox or
    not TreeList.OptionsView.Bands;
end;

function TcxTreeListCustomizing.GetVisible: Boolean;
begin
  Result := (FForm <> nil) and FForm.Visible;
end;

procedure TcxTreeListCustomizing.SetPosition(const AValue: TPoint);
begin
  FPosition := AValue;
  if Visible then
  begin
    Form.Left := AValue.X;
    Form.Top := AValue.Y;
  end;
  FIsPositionAssigned := True;
end;

procedure TcxTreeListCustomizing.SetRowCount(AValue: Integer);
begin
  AValue := Max(AValue, 2);
  if AValue <> FRowCount then
    FRowCount := AValue;
end;

procedure TcxTreeListCustomizing.SetVisible(AValue: Boolean);
begin
  if AValue then
    ShowCustomizingForm
  else
    HideCustomizingForm;
end;

{ TcxTreeListHitTest }

function TcxTreeListHitTest.CanMoving: Boolean;
begin
  Result := not CanSizing and ((HitAtBandHeader and HitBand.CanMoving) or
    (HitAtColumnHeader and HitColumn.CanMoving));
end;

function TcxTreeListHitTest.CanSizing: Boolean;
begin
  Result := HitAtSizingHorz or HitAtSizingVert;
end;

function TcxTreeListHitTest.AllowDesignMouseEvents(
  X, Y: Integer; AShift: TShiftState): Boolean;
begin
  Result := not TreeList.IsLocked and (inherited AllowDesignMouseEvents(X, Y, AShift) or
    HitAtHeader or CanSizing or HitAtButton or
    (HitCell is TcxTreeListFooterCellViewInfo) and not TcxTreeListFooterCellViewInfo(HitCell).Hidden);
end;

function TcxTreeListHitTest.CanShowHint(AItem: TObject): Boolean;
begin
  if AItem is TcxTreeListHeaderCellViewInfo then
    Result := TreeList.OptionsBehavior.HeaderHints
  else
    if AItem is TcxTreeListFooterCellViewInfo then
      Result := TreeList.OptionsBehavior.FooterHints
    else
      Result := inherited CanShowHint(AItem);
  Result := Result and (LockCount = 0);
end;

function TcxTreeListHitTest.CanStartDrag: Boolean;
begin
  Result := HitAtNode;
  if Result then
  begin
    Result := HitAtColumn or HitAtNodePreview or HitAtIndicator or HitAtImages;
    if not Result then
      Result := HitAtBand and not HitAtIndent;
  end;
end;

procedure TcxTreeListHitTest.CheckFooterColumn;
var
  AColumnRect, R: TRect;
  ARow: TcxTreeListBandRow;
  AColumn: TcxTreeListColumn;
  AColIndex, ARowIndex, AVisibleRowCount: Integer;
begin
  if (HitBand = nil) or not (HitTestItem is TcxTreeListBandCellViewInfo) then Exit;
  with TcxTreeListBandCellViewInfo(HitTestItem) do
    R := cxRectOffset(DisplayRect, GetCellOrigin);
  AVisibleRowCount := HitBand.BandRows.VisibleItemCount;
  for ARowIndex := AVisibleRowCount - 1 downto 0 do
  begin
    ARow := HitBand.BandRows.VisibleItems[ARowIndex];
    for AColIndex := 0 to ARow.VisibleItemCount - 1 do
    begin
      AColumn := ARow.VisibleItems[AColIndex];
      AColumnRect := AColumn.HeaderCell.DisplayRect;
      if cxInRange(HitX, AColumnRect.Left, AColumnRect.Right) and  cxInRange(HitY,
        R.Top + MulDiv(cxRectHeight(R), ARow.LineOffset, AVisibleRowCount), R.Bottom) then
      begin
        FHitColumn := AColumn;
        SetHitState(tlhc_HitAtColumn, True);
      end;
    end;
  end;
end;

procedure TcxTreeListHitTest.CheckSelection(AShift: TShiftState);
var
  AController: TcxTreeListController;
  I: Integer;
begin
  if not TreeList.IsDesigning then Exit;
  AController := TcxTreeListController(Controller);
  if HitAtBandHeader then
    AController.SelectObject(HitBand, AShift)
  else
    if HitAtColumnHeader then
      AController.SelectObject(HitColumn, AShift)
    else
      if HitCell is TcxTreeListFooterSingleCellViewInfo then
        AController.SelectObject(TcxTreeListFooterSingleCellViewInfo(HitCell).SummaryItem, AShift)
      else
        if HitCell is TcxTreeListFooterMultiItemsCellViewInfo then
          for I := 0 to TcxTreeListFooterMultiItemsCellViewInfo(HitCell).SummaryItems.Count - 1 do
            if TcxTreeListFooterMultiItemsCellViewInfo(HitCell).SummaryItems[I].Visible then
            begin
              AController.SelectObject(TcxTreeListFooterMultiItemsCellViewInfo(HitCell).SummaryItems[I], AShift);
              AShift := [ssShift];
            end;
end;

procedure TcxTreeListHitTest.DoCalculate;
begin
  Inc(FLockCount);
  try
    ClearState;
    FHitBand := nil;
    FHitColumn := nil;
    FHitNode := nil;
    if not IsMouseEvent then
      HitTestItem := nil;
    FDragItem := nil;
    if not TreeList.IsLocked then
      ViewInfo.CalculateHitTest(Self);
    if HitAtFooter or HitAtGroupFooter then
      CheckFooterColumn;
  finally
    Dec(FLockCount);
    HitTestItem := HitTestItem;
  end;
end;

function TcxTreeListHitTest.GetCurrentCursor: TCursor;
const
  ACursors: array[Boolean, Boolean] of TCursor =
    ((crcxHorzSize, crcxVertSize), (crHSplit, crVSplit));
var
  P: TPoint;
  ACellViewInfo: TcxTreeListEditCellViewInfo;
begin
  Result := inherited GetCurrentCursor;
  if (TreeList.DragAndDropState = ddsInProcess) and
    not (TreeList.DragAndDropObject is TcxSizingDragAndDropObject) then Exit;
  if (Result = crDefault) and CanSizing then
    Result := ACursors[TreeList.DragAndDropState <> ddsNone, HitAtSizingVert]
  else
    if HitAtNode and not HitAtFooterArea and
      (HitTestItem <> nil) and (HitTestItem is TcxTreeListEditCellViewInfo) and HitColumn.Options.Focusing then
    begin
      P := HitPoint;
      ACellViewInfo := TcxTreeListEditCellViewInfo(HitTestItem);
      P.X := P.X - ACellViewInfo.CellContentRect.Left;
      P.Y := P.Y - ACellViewInfo.NodeViewData.Origin.Y;
      Result := TcxCustomEditViewInfoAccess(ACellViewInfo.ViewInfo).GetCurrentCursor(P);
    end;
end;

function TcxTreeListHitTest.GetHitAtFilterBox: Boolean;
begin
  Result := GetState(echc_HitAtFilterBox);
end;

function TcxTreeListHitTest.GetHitAtFindPanel: Boolean;
begin
  Result := GetState(echc_HitAtFindPanel);
end;

function TcxTreeListHitTest.GetHitAtNavigator: Boolean;
begin
  Result := GetState(tlhc_HitAtNavigator);
end;

function TcxTreeListHitTest.GetState(Index: Integer): Boolean;
begin
  Result := (HitState and (1 shl Index)) <> 0;
end;

procedure TcxTreeListHitTest.SetHitState(Index: Integer; Value: Boolean);
begin
  if Value then
    FHitState := FHitState or (1 shl Index)
  else
    FHitState := FHitState and not (1 shl Index);
end;

function TcxTreeListHitTest.GetHitAtImages: Boolean;
begin
  Result := HitAtImage or HitAtStateImage;
end;

function TcxTreeListHitTest.GetHitAtFooterArea: Boolean;
begin
  Result := HitAtFooter or HitAtFooterItem or
    HitAtGroupFooter or HitAtGroupFooterItem;
end;

function TcxTreeListHitTest.GetHitAtHeader: Boolean;
begin
  Result := (HitAtBandHeader or HitAtColumnHeader or HitAtBandCustomizing or
    HitAtColumnCustomizing) and (HitTestItem is TcxTreeListCustomHeaderCellViewInfo);
end;

function TcxTreeListHitTest.GetHitCell: TcxCustomViewInfoItem;
begin
  Result := HitTestItem as TcxCustomViewInfoItem;
end;

function TcxTreeListHitTest.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Control);
end;

function TcxTreeListHitTest.GetViewInfo: TcxTreeListViewInfo;
begin
  Result := TcxTreeListViewInfo(inherited ViewInfo);
end;

{ TcxTreeListHitTestArea }

constructor TcxTreeListHitTestArea.Create(
  ALink: TcxTreeListCustomCellViewInfo);
begin
  FLink := ALink;
end;

function TcxTreeListHitTestArea.GetHitTest(
  AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := PtInRect(Area, AHitTest.HitPoint);
  if Result then
    InitHitTest(AHitTest);
end;

procedure TcxTreeListHitTestArea.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    Area := TdxRightToLeftLayoutConverter.ConvertRect(Area, AClientBounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TcxTreeListHeaderSizingArea }

constructor TcxTreeListHeaderSizingArea.CreateEx(
  ALink: TcxTreeListCustomCellViewInfo; ADirection: TcxDragSizingDirection);
begin
  inherited Create(ALink);
  FDirection := ADirection;
  Calculate;
end;

procedure TcxTreeListHeaderSizingArea.Calculate;
begin
  FArea := cxRectInflate(FLink.DisplayRect, cxtlHitDelta, cxtlHitDelta);
  if Direction = dsdHorz then
  begin
    if not FLink.IsRightToLeftConverted  then
      if FLink.DisplayRect.Right > FLink.VisibleRect.Right then
        FArea := cxNullRect
      else
        FArea.Left := FArea.Right - cxtlHitDelta * 2
    else
      if FLink.DisplayRect.Left < FLink.VisibleRect.Left then
        FArea := cxNullRect
      else
        FArea.Right := FArea.Left + cxtlHitDelta * 2;
  end
  else
    FArea.Top := FArea.Bottom - cxtlHitDelta * 2;
  if FLink.HasClipping then
    IntersectRect(FArea, FArea, cxRectInflate(FLink.VisibleRect, 0, cxtlHitDelta));
  FIsRightToLeftConverted := FLink.IsRightToLeftConverted;
end;

procedure TcxTreeListHeaderSizingArea.InitHitTest(AHitTest: TcxTreeListHitTest);
const
  AStates: array[TcxDragSizingDirection] of Integer =
    (tlhc_HitAtSizingHorz, tlhc_HitAtSizingVert);
begin
  AHitTest.SetHitState(AStates[Direction], True);
  TcxTreeListHeaderCellViewInfo(Link).SetHitItem;
  AHitTest.DragItem := TcxTreeListHeaderCellViewInfo(Link).Item;
  if AHitTest.IsItemEditCell then
    AHitTest.HitTestItem := nil;
end;

{ TcxTreeListNodeSizingArea }

constructor TcxTreeListNodeSizingArea.Create(ALink: TcxTreeListCustomCellViewInfo);
begin
  inherited Create(ALink);
  Calculate;
end;

destructor TcxTreeListNodeSizingArea.Destroy;
begin
  if FLink <> nil then
    TcxTreeListIndicatorCellViewInfo(FLink).SizingArea := nil;
  inherited Destroy;
end;

procedure TcxTreeListNodeSizingArea.Calculate;
begin
  FArea := cxRectOffset(FLink.DisplayRect, FLink.GetCellOrigin);
  FArea.Top := FArea.Bottom - cxtlHitDelta;
  FArea.Bottom := FArea.Bottom + cxtlHitDelta;
end;

function TcxTreeListNodeSizingArea.GetHitTest(
  AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := PtInRect(Area, AHitTest.HitPoint);
  if Result and Link.TreeList.OptionsCustomizing.RowSizing then
    Result := Link.TreeList.TopVisibleNode = Link.Node;
  if Result then
    InitHitTest(AHitTest);
end;

procedure TcxTreeListNodeSizingArea.InitHitTest(AHitTest: TcxTreeListHitTest);
begin
  AHitTest.SetHitState(tlhc_HitAtSizingVert, True);
  AHitTest.SetHitState(tlhc_HitAtNode, True);
  AHitTest.FHitNode := TcxTreeListIndentCellViewInfo(FLink).AttachNode;
  AHitTest.FDragItem := AHitTest.FHitNode;
end;

{ TcxTreeListCustomPopupMenu }

function TcxTreeListCustomPopupMenu.GetPopupMenu: TComponent;
begin
  Result := FPopupMenu;
end;

function TcxTreeListCustomPopupMenu.Popup(X, Y: Integer): Boolean;
begin
  Result := ShowPopupMenu(TreeList, PopupMenu, X, Y);
end;

procedure TcxTreeListCustomPopupMenu.SetPopupMenu(AValue: TComponent);
begin
  FPopupMenu := AValue;
end;

{ TcxTreeListCustomBuiltInMenu }

procedure TcxTreeListCustomBuiltInMenu.CreatePopupMenu;
begin
  FPopupMenu := GetPopupClass.Create(TreeList);
end;

procedure TcxTreeListCustomBuiltInMenu.DestroyPopupMenu;
begin
  FreeAndNil(FPopupMenu);
end;

procedure TcxTreeListCustomBuiltInMenu.GetBitmapFromImageList(AImages: TCustomImageList; AImageIndex: Integer; ABitmap: TBitmap);
var
  AImage: TcxAlphaBitmap;
begin
  if IsImageAssigned(AImages, AImageIndex) then
  begin
    AImage := cxPrepareBitmapForDrawing(nil, AImages, AImageIndex, False, clNone);
    ABitmap.PixelFormat := pf32bit;
    ABitmap.SetSize(AImage.Width, AImage.Height);
    cxBitBlt(ABitmap.Canvas.Handle, AImage.Canvas.Handle, Rect(0, 0, ABitmap.Width, ABitmap.Height), cxNullPoint, SRCCOPY);
  end;
end;

function TcxTreeListCustomBuiltInMenu.GetImages(AInternal: Boolean): TCustomImageList;
begin
  if AInternal then
    Result := TcxTreeListPopupMenu(Owner).Images
  else
    Result := TcxTreeListPopupMenu(Owner).UserImages;
end;

function TcxTreeListCustomBuiltInMenu.GetRoot: TComponent;
begin
  Result := PopupMenu;
end;

function TcxTreeListCustomBuiltInMenu.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxTreeListPopupMenu(Owner).TreeList;
end;

procedure TcxTreeListCustomBuiltInMenu.Initialize;
begin
  DestroyPopupMenu;
  CreatePopupMenu;
end;

procedure TcxTreeListCustomBuiltInMenu.MenuItemClickHandler(Sender: TObject);
begin
  TcxTreeListPopupMenu(Owner).DoMenuItemClick(Sender);
end;

{ TcxTreeListBuiltInMenuItem }

type
  TcxTreeListBuiltInMenuItem = class(TMenuItem)
  private
    FInternal: Boolean;
    FBuiltInMenu: TcxTreeListBuiltInMenu;
  protected
    procedure AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; TopLevel: Boolean); override;
    procedure MeasureItem(ACanvas: TCanvas; var Width, Height: Integer); override;
  public
    constructor Create(ABuiltInMenu: TcxTreeListBuiltInMenu; AInternal: Boolean); reintroduce; virtual;
  end;

constructor TcxTreeListBuiltInMenuItem.Create(ABuiltInMenu: TcxTreeListBuiltInMenu; AInternal: Boolean);
begin
  inherited Create(nil);
  FInternal := AInternal;
  FBuiltInMenu := ABuiltInMenu;
end;

procedure TcxTreeListBuiltInMenuItem.AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; TopLevel: Boolean);
begin
  if not IsWinXPOrLater or FBuiltInMenu.PopupMenu.IsRightToLeft then
    inherited AdvancedDrawItem(ACanvas, ARect, State, TopLevel)
  else
    cxAdvancedDrawPopupMenuItem(Self, ACanvas, ARect, State, FBuiltInMenu.GetImages(FInternal));
end;

procedure TcxTreeListBuiltInMenuItem.MeasureItem(ACanvas: TCanvas; var Width, Height: Integer);
begin
  inherited;
  dxAdjustToTouchableSize(Height);
end;

{ TcxTreeListBuiltInMenu }

destructor TcxTreeListBuiltInMenu.Destroy;
begin
  DestroyPopupMenu;
  inherited Destroy;
end;

function TcxTreeListBuiltInMenu.CreateMenuItem(AOwner: TComponent; const ACaption: string;
  ACommand: TcxTag; AEnabled: Boolean; AItemType: TcxTreeListBuiltInMenuItemType;
  AChecked: Boolean; AImageIndex: Integer; AWithSeparator: Boolean; AInternal: Boolean): TComponent;
var
  ASeparator: TMenuItem;
begin
  Result := TcxTreeListBuiltInMenuItem.Create(Self, AInternal);
  if AWithSeparator then
  begin
    ASeparator := TMenuItem.Create(nil);
    ASeparator.Caption := '-';
    TMenuItem(AOwner).Add(ASeparator);
  end;
  with TcxTreeListBuiltInMenuItem(Result) do
  begin
    Caption := ACaption;
    Checked := AChecked;
    Enabled := AEnabled;
    if AInternal then
      ImageIndex := -1
    else
      ImageIndex := AImageIndex;
    Tag := ACommand;
    if AInternal then
      GetBitmapFromImageList(GetImages(AInternal), AImageIndex, Bitmap);
    OnClick := MenuItemClickHandler;
  end;
  TMenuItem(AOwner).Add(TMenuItem(Result));
end;

function TcxTreeListBuiltInMenu.GetPopupClass: TComponentClass;
begin
  Result := TPopupMenu;
end;

function TcxTreeListBuiltInMenu.GetRoot: TComponent;
begin
  Result := PopupMenu.Items;
end;

procedure TcxTreeListBuiltInMenu.CreatePopupMenu;
begin
  inherited CreatePopupMenu;
  PopupMenu.Images := GetImages(True);
end;

function TcxTreeListBuiltInMenu.GetInternalPopupMenu: TPopupMenu;
begin
  Result := TPopupMenu(FPopupMenu);
end;

type
  TcxTLPopupMenuImagesManager = class(TObject)
  private
    FImages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetImages(const AResourceName: string): TCustomImageList;
  end;

var
  FPopupImagesManager: TcxTLPopupMenuImagesManager;

constructor TcxTLPopupMenuImagesManager.Create;
begin
  inherited Create;
  FImages := TStringList.Create;
end;

destructor TcxTLPopupMenuImagesManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to FImages.Count - 1 do
    FImages.Objects[I].Free;
  FreeAndNil(FImages);
  inherited;
end;

function TcxTLPopupMenuImagesManager.GetImages(const AResourceName: string): TCustomImageList;

  function LoadResourceImages: TcxImageList;
  begin
    Result := TcxImageList.CreateSize(16, 16);
    Result.LoadImage(HInstance, AResourceName);
  end;

var
  AIndex: Integer;
begin
  AIndex := FImages.IndexOf(AResourceName);
  if AIndex = -1 then
    AIndex := FImages.AddObject(AResourceName, LoadResourceImages);
  Result := FImages.Objects[AIndex] as TcxImageList;
end;

{ TcxTreeListPopupMenu }

constructor TcxTreeListPopupMenu.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

destructor TcxTreeListPopupMenu.Destroy;
begin
  PopupMenu := nil;
  FreeAndNil(FBuiltInMenu);
  inherited Destroy;
end;

procedure TcxTreeListPopupMenu.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListPopupMenu then
  begin
    UseBuiltInMenu := TcxTreeListPopupMenu(Source).UseBuiltInMenu;
    PopupMenu := TcxTreeListPopupMenu(Source).PopupMenu;
    OnClick := TcxTreeListPopupMenu(Source).OnClick;
    OnPopup := TcxTreeListPopupMenu(Source).OnPopup;
  end
  else
    inherited Assign(Source);
end;

function TcxTreeListPopupMenu.CreateMenuItem(AOwner: TComponent; const ACaption: string;
  ACommand: TcxTag; AEnabled: Boolean; AItemType: TcxTreeListBuiltInMenuItemType;
  AChecked: Boolean; AImageIndex: Integer; AWithSeparator: Boolean): TComponent;
begin
  Result := BuiltInMenu.CreateMenuItem(AOwner, ACaption, ACommand, AEnabled,
    AItemType, AChecked, AImageIndex, AWithSeparator, False);
end;

function TcxTreeListPopupMenu.CreateInternalMenuItem(AOwner: TComponent; const ACaption: string;
  ACommand: TcxTag; AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
  AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False): TComponent;
begin
  Result := BuiltInMenu.CreateMenuItem(AOwner, ACaption, ACommand, AEnabled,
    AItemType, AChecked, AImageIndex, AWithSeparator);
end;

procedure TcxTreeListPopupMenu.CreateItems;
begin
end;

procedure TcxTreeListPopupMenu.DoExecute(ACommand: Integer);
begin
end;

procedure TcxTreeListPopupMenu.DoMenuItemClick(Sender: TObject);
var
  AHandled: Boolean;
begin
  DoClick(Sender, AHandled);
  if not AHandled then
    DoExecute(TComponent(Sender).Tag);
end;

procedure TcxTreeListPopupMenu.DoClick(AItem: TObject;
  var AHandled: Boolean);
begin
  AHandled := False;
  if Assigned(FOnClick) then
    FOnClick(TreeList, AItem, AHandled);
end;

function TcxTreeListPopupMenu.DoPopup: Boolean;
begin
  Result := False;
  if Assigned(FOnPopup) then
    FOnPopup(TreeList, Self, Result);
end;

function TcxTreeListPopupMenu.DoShowPopupMenu(const P: TPoint): Boolean;
begin
  Result := IsPopupSuitable(TcxTreeListPopupMenus(Owner).HitTest) and Popup(P.X, P.Y);
end;

function TcxTreeListPopupMenu.GetResourceImagesName: string;
begin
  Result := '';
end;

function TcxTreeListPopupMenu.GetRoot: TComponent;
begin
  Result := BuiltInMenu.Root;
end;

function TcxTreeListPopupMenu.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxTreeListPopupMenus(Owner).TreeList;
end;

procedure TcxTreeListPopupMenu.InitializeInternalMenu;
begin
  BuiltInMenu.Initialize;
  CreateItems;
end;

function TcxTreeListPopupMenu.IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := False;
end;

procedure TcxTreeListPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
   if (AComponent = PopupMenu) then
     PopupMenu := nil
   else
     if (AComponent = UserImages) then
       UserImages := nil;
end;

function TcxTreeListPopupMenu.Popup(X, Y: Integer): Boolean;
begin
  if UseBuiltInMenu then
    InitializeInternalMenu;
  Result := DoPopup;
  if not Result then
    if UseBuiltInMenu then
      Result := BuiltInMenu.Popup(X, Y)
    else
      Result := inherited Popup(X, Y);
end;

procedure TcxTreeListPopupMenu.SetPopupMenu(AValue: TComponent);
begin
  if PopupMenu <> AValue then
  begin
    if PopupMenu <> nil then
      PopupMenu.RemoveFreeNotification(TreeList);
    inherited;
    if PopupMenu <> nil then
      PopupMenu.FreeNotification(TreeList);
  end;
end;

function TcxTreeListPopupMenu.GetBuiltInMenu: TcxTreeListCustomBuiltInMenu;
begin
  if FBuiltInMenu = nil then
    FBuiltInMenu := cxTreeListBuiltInMenuClass.Create(Self);
  Result := FBuiltInMenu;
end;

function TcxTreeListPopupMenu.GetImages: TCustomImageList;
begin
  Result := FPopupImagesManager.GetImages(GetResourceImagesName);
end;

procedure TcxTreeListPopupMenu.SetUseBuiltInMenu(AValue: Boolean);
begin
  if FUseBuiltInMenu <> AValue then
  begin
    FUseBuiltInMenu := AValue;
    if not FUseBuiltInMenu then
      FreeAndNil(FBuiltInMenu);
  end;
end;

procedure TcxTreeListPopupMenu.SetUserImages(AValue: TCustomImageList);
begin
  if FUserImages <> AValue then
  begin
    if FUserImages <> nil then
      FUserImages.RemoveFreeNotification(TreeList);
    FUserImages := AValue;
    if FUserImages <> nil then
      FUserImages.FreeNotification(TreeList);
  end;
end;

{ TcxTreeListFooterPopupMenu }

constructor TcxTreeListFooterPopupMenu.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItems := [tlfmiSum, tlfmiMin, tlfmiMax, tlfmiAverage, tlfmiCount,
    tlfmiNone, tlfmiAllNodes];
end;

procedure TcxTreeListFooterPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxTreeListFooterPopupMenu then
    FItems := TcxTreeListFooterPopupMenu(Source).Items;
end;

procedure TcxTreeListFooterPopupMenu.CreateItems;
var
  ASummaryKinds: TcxSummaryKinds;
  ADoesVisibleItemExist: Boolean;
begin
  inherited;
  ASummaryKinds := GetVisibleItemKinds;
  ADoesVisibleItemExist := ASummaryKinds * [skSum..skAverage] <> [];
  if tlfmiSum in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListSumMenuItem),
      tlcmSum, IsItemEnabled(skSum), tlmitChecked, skSum in ASummaryKinds, tlcmSum - 1);
  if tlfmiMin in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListMinMenuItem),
      tlcmMin, IsItemEnabled(skMin), tlmitChecked, skMin in ASummaryKinds, tlcmMin - 1);
  if tlfmiMax in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListMaxMenuItem),
      tlcmMax, IsItemEnabled(skMax), tlmitChecked, skMax in ASummaryKinds, tlcmMax - 1);
  if tlfmiCount in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListCountMenuItem),
      tlcmCount, IsItemEnabled(skCount), tlmitChecked, skCount in ASummaryKinds, tlcmCount - 1);
  if tlfmiAverage in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListAvgMenuItem),
      tlcmAverage, IsItemEnabled(skAverage), tlmitChecked, skAverage in ASummaryKinds, tlcmAverage - 1);
  if tlfmiNone in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListNoneMenuItem),
      tlcmNone, IsItemEnabled(skNone), tlmitChecked, not ADoesVisibleItemExist, - 1, True);
  if tlfmiAllNodes in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListAllNodesMenuItem),
      tlcmAllNodes, ADoesVisibleItemExist, tlmitChecked, ADoesVisibleItemExist and IsAllNodes, - 1, True);
end;

procedure TcxTreeListFooterPopupMenu.DoExecute(ACommand: Integer);

  procedure SetItemsVisibility(AKind: TcxSummaryKind);
  var
    I: Integer;
    AValue: Boolean;
  begin
    AValue := (AKind <> skNone) and not (AKind in GetVisibleItemKinds);
    if not AValue or (SummaryItems.GetItemByKind(AKind) <> nil) then
    begin
      for I := 0 to SummaryItems.Count - 1 do
        if ((SummaryItems[I].Kind = AKind) or (AKind = skNone)) then
          SummaryItems[I].Visible := AValue;
    end
    else
      with SummaryItems.Add do
      begin
        Kind := AKind;
        AllNodes := IsAllNodes;
      end;
  end;

  procedure SetAllNodes(AValue: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to SummaryItems.Count - 1 do
      SummaryItems[I].AllNodes := AValue;
  end;

begin
  inherited;
  SummaryItems.BeginUpdate;
  try
    case ACommand of
      tlcmNone..tlcmAverage: SetItemsVisibility(TcxSummaryKind(ACommand));
      tlcmAllNodes: SetAllNodes(not IsAllNodes);
    end;
  finally
    SummaryItems.EndUpdate;
  end;
end;

function TcxTreeListFooterPopupMenu.GetResourceImagesName: string;
begin
  Result := 'TLFTRIMG';
end;

function TcxTreeListFooterPopupMenu.IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := (AHitTest.HitAtFooterItem or AHitTest.HitAtFooter) and
    (AHitTest.FHitColumn <> nil);
  if Result then
    FSummaryItems := AHitTest.FHitColumn.Summary.FooterSummaryItems;
end;

function TcxTreeListFooterPopupMenu.GetVisibleItemKinds: TcxSummaryKinds;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to SummaryItems.Count - 1 do
    if SummaryItems[I].Visible then
      Include(Result, SummaryItems[I].Kind);
end;

function TcxTreeListFooterPopupMenu.IsAllNodes: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to SummaryItems.Count - 1 do
    if SummaryItems[I].AllNodes then
    begin
      Result := True;
      Break;
    end;
end;

function TcxTreeListFooterPopupMenu.IsItemEnabled(AKind: TcxSummaryKind): Boolean;

  function IsSummaryKindAllowedForValueType(AValueTypeClass: TcxValueTypeClass): Boolean;
  const
    ANumberVarTypes = [varSmallint, varInteger, varSmallint, varSingle, varByte,
      varDouble, varCurrency, varDate, varLongWord, varWord, varInt64, varShortInt];
    ATimeVarTypes = [varDate];
  var
    AVarType: Integer;
  begin
    Result := False;
    if AValueTypeClass <> nil then
    begin
      AVarType := AValueTypeClass.GetVarType;
      Result := ((AVarType in ANumberVarTypes) or (AVarType = TcxFMTBcdValueType.GetVarType)) and
        not((AVarType in ATimeVarTypes) and (AKind in [skSum, skAverage]));
    end;
  end;

var
  AColumn: TcxTreeListColumn;
begin
  AColumn := SummaryItems.Column;
  Result := (AKind in [skCount, skNone]) or not(AColumn.Properties is TcxCustomLookupEditProperties) and
    IsSummaryKindAllowedForValueType(AColumn.ValueDef.ValueTypeClass);
end;

function TcxTreeListFooterPopupMenu.IsItemsStored: Boolean;
begin
  Result := FItems <> [tlfmiSum, tlfmiMin, tlfmiMax, tlfmiAverage, tlfmiCount,
    tlfmiNone, tlfmiAllNodes];
end;

{ TcxTreeListGroupFooterPopupMenu }

function TcxTreeListGroupFooterPopupMenu.IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := (AHitTest.HitAtGroupFooterItem or AHitTest.HitAtGroupFooter) and
    (AHitTest.FHitColumn <> nil);
  if Result then
    FSummaryItems := AHitTest.FHitColumn.Summary.GroupFooterSummaryItems;
end;

{ TcxTreeListColumnHeaderPopupMenu }

constructor TcxTreeListColumnHeaderPopupMenu.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItems := [tlchmiSortAscending, tlchmiSortDescending, tlchmiClearSorting, tlchmiFooter,
    tlchmiGroupFooters, tlchmiRemoveThisColumn, tlchmiFieldChooser, tlchmiHorzAlignment,
    tlchmiVertAlignment, tlchmiBestFit, tlchmiBestFitAllColumns];
end;

procedure TcxTreeListColumnHeaderPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxTreeListColumnHeaderPopupMenu then
    FItems := TcxTreeListColumnHeaderPopupMenu(Source).Items;
end;

procedure TcxTreeListColumnHeaderPopupMenu.CreateItems;
var
  ASubMenu: TComponent;
  AAlignmentMenuItemsNeeded: Boolean;
begin
  inherited CreateItems;
  if Column.Options.Sorting then
  begin
    if tlchmiSortAscending in Items then
      CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListSortAscendingMenuItem),
        tlcmSortAscending, True, tlmitChecked, Column.SortOrder = soAscending, 0);
    if tlchmiSortDescending in Items then
      CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListSortDescendingMenuItem),
        tlcmSortDescending, True, tlmitChecked, Column.SortOrder = soDescending, 1);
    if tlchmiClearSorting in Items then
      CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListClearSortingMenuItem),
        tlcmClearSorting, Column.SortOrder <> soNone, tlmitDefault, False, - 1);
    FSeparatorNeeded := True;
  end;
  if tlchmiFooter in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListFooterMenuItem),
      tlcmFooter, True, tlmitChecked, TreeList.OptionsView.Footer, 4, IsSeparatorNeeded);
  if tlchmiGroupFooters in Items then
  begin
    ASubMenu := CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListGroupFootersMenuItem),
      -1, True, tlmitSubItem, False, 5, IsSeparatorNeeded);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListGroupFootersInvisibleMenuItem),
      tlcmGroupFootersInvisible, True, tlmitChecked, TreeList.OptionsView.GroupFooters = tlgfInvisible);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListGroupFootersVisibleWhenExpandedMenuItem),
      tlcmGroupFootersVisibleWhenExpanded, True, tlmitChecked, TreeList.OptionsView.GroupFooters = tlgfVisibleWhenExpanded);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListGroupFootersAlwaysVisibleMenuItem),
      tlcmGroupFootersAlwaysVisible, True, tlmitChecked, TreeList.OptionsView.GroupFooters = tlgfAlwaysVisible);
  end;
  FSeparatorNeeded := True;
  if (tlchmiRemoveThisColumn in Items) and not Column.Options.Hidden and
    Column.CanMoving and (TreeList.VisibleColumnCount > 1) then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListRemoveThisColumnMenuItem),
      tlcmRemoveThisColumn, True, tlmitDefault, False, - 1, IsSeparatorNeeded);
  if tlchmiFieldChooser in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListFieldChooserMenuItem),
      tlcmFieldChooser, True, tlmitDefault, False, 2, IsSeparatorNeeded);
  FSeparatorNeeded := True;
  AAlignmentMenuItemsNeeded := Column.Properties <> nil;
  if AAlignmentMenuItemsNeeded then
    CreateAlignmentSubMenus;
  if tlchmiBestFit in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListBestFitMenuItem),
      tlcmBestFit, True, tlmitDefault, False, 3, IsSeparatorNeeded);
  if tlchmiBestFitAllColumns in Items then
    CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListBestFitAllColumnsMenuItem),
      tlcmBestFitAllColumns, True, tlmitDefault, False, - 1, True);
end;

procedure TcxTreeListColumnHeaderPopupMenu.DoExecute(ACommand: Integer);
begin
  inherited;
  case ACommand of
    tlcmSortAscending:
      Column.SortOrder := soAscending;
    tlcmSortDescending:
      Column.SortOrder := soDescending;
    tlcmClearSorting:
      Column.SortOrder := soNone;
    tlcmFooter:
      TreeList.OptionsView.Footer := not TreeList.OptionsView.Footer;
    tlcmGroupFootersInvisible..tlcmGroupFootersAlwaysVisible:
      TreeList.OptionsView.GroupFooters := TcxTreeListGroupFootersMode(ACommand - tlcmGroupFootersInvisible);
    tlcmRemoveThisColumn:
      begin
        Column.Visible := False;
        TreeList.DoColumnPosChanged(Column);
      end;
    tlcmFieldChooser:
      TreeList.Customizing.Visible := True;
    tlcmHorzAlignmentLeft..tlcmHorzAlignmentCenter:
      Column.Properties.Alignment.Horz := TcxEditHorzAlignment(ACommand - tlcmHorzAlignmentLeft);
    tlcmVertAlignmentTop..tlcmVertAlignmentCenter:
      Column.Properties.Alignment.Vert := TcxEditVertAlignment(ACommand - tlcmVertAlignmentTop);
    tlcmBestFit:
      Column.ApplyBestFit;
    tlcmBestFitAllColumns:
      TreeList.ApplyBestFit;
  end;
end;

function TcxTreeListColumnHeaderPopupMenu.GetResourceImagesName: string;
begin
  Result := 'TLHDRIMG';
end;

function TcxTreeListColumnHeaderPopupMenu.IsPopupSuitable(AHitTest: TcxTreeListHitTest): Boolean;
begin
  Result := AHitTest.HitAtColumnHeader;
  if Result then
    Column := AHitTest.HitColumn;
end;

procedure TcxTreeListColumnHeaderPopupMenu.CreateAlignmentSubMenus;
var
  ASubMenu: TComponent;
begin
  if tlchmiHorzAlignment in Items then
  begin
    ASubMenu := CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListHorizontalAlignmentMenuItem),
      -1, True, tlmitSubItem, False, -1, IsSeparatorNeeded);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListHorizontalAlignmentLeftMenuItem),
      tlcmHorzAlignmentLeft, True, tlmitChecked, Column.Properties.Alignment.Horz = taLeftJustify);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListHorizontalAlignmentCenterMenuItem),
      tlcmHorzAlignmentCenter, True, tlmitChecked, Column.Properties.Alignment.Horz = taCenter);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListHorizontalAlignmentRightMenuItem),
      tlcmHorzAlignmentRight, True, tlmitChecked, Column.Properties.Alignment.Horz = taRightJustify);
  end;
  if tlchmiVertAlignment in Items then
  begin
    ASubMenu := CreateInternalMenuItem(Root, cxGetResourceString(@scxTreeListVerticalAlignmentMenuItem),
      -1, True, tlmitSubItem, False, -1, IsSeparatorNeeded);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListVerticalAlignmentTopMenuItem),
      tlcmVertAlignmentTop, True, tlmitChecked, Column.Properties.Alignment.Vert = taTopJustify);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListVerticalAlignmentCenterMenuItem),
      tlcmVertAlignmentCenter, True, tlmitChecked, Column.Properties.Alignment.Vert = taVCenter);
    CreateInternalMenuItem(ASubMenu, cxGetResourceString(@scxTreeListVerticalAlignmentBottomMenuItem),
      tlcmVertAlignmentBottom, True, tlmitChecked, Column.Properties.Alignment.Vert = taBottomJustify);
  end;
end;

function TcxTreeListColumnHeaderPopupMenu.IsItemsStored: Boolean;
begin
  Result := FItems <> [tlchmiSortAscending, tlchmiSortDescending, tlchmiClearSorting,
    tlchmiFooter, tlchmiGroupFooters, tlchmiRemoveThisColumn, tlchmiFieldChooser,
    tlchmiHorzAlignment, tlchmiVertAlignment, tlchmiBestFit, tlchmiBestFitAllColumns];
end;

function TcxTreeListColumnHeaderPopupMenu.IsSeparatorNeeded: Boolean;
begin
  Result := FSeparatorNeeded;
  FSeparatorNeeded := False;
end;

{ TcxTreeListPopupMenus }

constructor TcxTreeListPopupMenus.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMenus := TObjectList.Create;
  CreateMenus;
end;

destructor TcxTreeListPopupMenus.Destroy;
begin
  FreeAndNil(FMenus);
  inherited Destroy;
end;

procedure TcxTreeListPopupMenus.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListPopupMenus then
  begin
    ColumnHeaderMenu := TcxTreeListPopupMenus(Source).ColumnHeaderMenu;
    GroupFooterMenu := TcxTreeListPopupMenus(Source).GroupFooterMenu;
    FooterMenu := TcxTreeListPopupMenus(Source).FooterMenu;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListPopupMenus.CreateMenus;
begin
  FFooterMenu := TcxTreeListFooterPopupMenu.Create(Self);
  FMenus.Add(FFooterMenu);
  FGroupFooterMenu := TcxTreeListGroupFooterPopupMenu.Create(Self);
  FMenus.Add(FGroupFooterMenu);
  FColumnHeaderMenu := TcxTreeListColumnHeaderPopupMenu.Create(Self);
  FMenus.Add(FColumnHeaderMenu);
end;

function TcxTreeListPopupMenus.DoShowPopupMenu(const P: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FMenus.Count - 1 do
    if TcxTreeListPopupMenu(FMenus[I]).DoShowPopupMenu(P) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TcxTreeListPopupMenus.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  for I := 0 to FMenus.Count - 1 do
    TcxTreeListPopupMenu(FMenus[I]).Notification(AComponent, Operation);
end;

function TcxTreeListPopupMenus.GetHitTest: TcxTreeListHitTest;
begin
  Result := TreeList.HitTest;
end;

function TcxTreeListPopupMenus.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Owner);
end;

procedure TcxTreeListPopupMenus.SetColumnHeaderMenu(AValue: TcxTreeListColumnHeaderPopupMenu);
begin
  FColumnHeaderMenu.Assign(AValue);
end;

procedure TcxTreeListPopupMenus.SetFooterMenu(
  AValue: TcxTreeListFooterPopupMenu);
begin
  FFooterMenu.Assign(AValue);
end;

procedure TcxTreeListPopupMenus.SetGroupFooterMenu(
  AValue: TcxTreeListGroupFooterPopupMenu);
begin
  FGroupFooterMenu.Assign(AValue);
end;

{ TcxTreeListCustomCellViewInfo }

constructor TcxTreeListCustomCellViewInfo.CreateEx(
  ATreeList: TcxCustomTreeList; const ABounds, AVisibleRect: TRect);
begin
  inherited Create(ATreeList);
  DisplayRect := ABounds;
  VisibleBounds := AVisibleRect;
  CheckClipping(DisplayRect, VisibleBounds);
end;

destructor TcxTreeListCustomCellViewInfo.Destroy;
begin
  if (TreeList <> nil) and (TreeList.Controller.DragItem = Self) then
    TreeList.Controller.DragItem := nil;
  inherited Destroy;
end;

procedure TcxTreeListCustomCellViewInfo.DoCalculate;
begin
  CheckClipping(DisplayRect, VisibleBounds);
end;

procedure TcxTreeListCustomCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  AWindowOrg: TPoint;
begin
  if not Visible then Exit;
  if not CellHasOrigin then
    ACanvas.Rectangle(BoundsRect, ViewParams, Borders, BorderColor)
  else
  begin
    AWindowOrg := ACanvas.WindowOrg;
    ACanvas.WindowOrg := cxNullPoint;
    ACanvas.Rectangle(cxRectOffset(BoundsRect, GetCellOrigin),
      ViewParams, Borders, BorderColor);
    ACanvas.WindowOrg := AWindowOrg;
  end;
end;

procedure TcxTreeListCustomCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FBorders := TdxRightToLeftLayoutConverter.ConvertBorders(Borders);
end;

function TcxTreeListCustomCellViewInfo.CellHasOrigin: Boolean;
begin
  Result := (FAttachNode <> nil) and (FAttachNode.FViewData <> nil);
end;

function TcxTreeListCustomCellViewInfo.GetClipRect: TRect;
begin
  Result := ClipRect;
  if cxRectIsEmpty(DisplayRect) then
  begin
    Result := VisibleRect;
    if Result.Left < DisplayRect.Left then
      Result.Left := DisplayRect.Left;
    if Result.Right > DisplayRect.Right then
      Result.Right := DisplayRect.Right;
  end
end;

function TcxTreeListCustomCellViewInfo.GetCellOrigin: TPoint;
begin
  if CellHasOrigin then
    Result := FAttachNode.ViewData.Origin
  else
    Result := cxNullPoint;
end;

function TcxTreeListCustomCellViewInfo.GetControl: TcxEditingControl;
begin
  Result := TcxCustomTreeList(Owner);
end;

function TcxTreeListCustomCellViewInfo.GetHitTest(
  AHitTest: TcxCustomHitTestController): Boolean;
begin
  if CellHasOrigin then
    Result := Visible and PtInRect(cxRectOffset(ClipRect, GetCellOrigin), AHitTest.HitPoint)
  else
    Result := Visible and PtInRect(ClipRect, AHitTest.HitPoint);
  if not Result then Exit;
  if not VisibleInfoCalculated then
    DoCalculate;
  if AttachNode <> nil then
  begin
    HitTest.FHitNode := AttachNode;
    if Node <> nil then
      SetHitTestCodes([tlhc_HitAtNode]);
  end;
  TcxTreeListHitTest(AHitTest).HitTestItem := Self;
end;

procedure TcxTreeListCustomCellViewInfo.SetBounds(
  const ABounds, AVisibleBounds: TRect);
begin
  CheckClipping(ABounds, AVisibleBounds);
  VisibleInfoCalculated := False;
end;

procedure TcxTreeListCustomCellViewInfo.SetHitTestCodes(
  const ACodes: array of Integer);
var
  I: Integer;
begin
  for I := Low(ACodes) to High(ACodes) do
    TreeList.HitTest.SetHitState(ACodes[I], True);
end;

function TcxTreeListCustomCellViewInfo.GetSelected: Boolean;
begin
  Result := False;
end;

function TcxTreeListCustomCellViewInfo.GetExtPaintStyle: Boolean;
begin
  Result := OptionsView.ExtPaintStyle;
end;

function TcxTreeListCustomCellViewInfo.GetHitTestController: TcxTreeListHitTest;
begin
  Result := TreeList.HitTest;
end;

function TcxTreeListCustomCellViewInfo.GetOptionsView: TcxTreeListOptionsView;
begin
  Result := TreeList.OptionsView;
end;

function TcxTreeListCustomCellViewInfo.GetStyles: TcxTreeListStyles;
begin
  Result := TreeList.Styles;
end;

function TcxTreeListCustomCellViewInfo.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Owner);
end;

function TcxTreeListCustomCellViewInfo.GetVisibleRect: TRect;
begin
  Result := VisibleBounds;
end;

{ TcxTreeListBackgroundCellViewInfo }

class function TcxTreeListBackgroundCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlBackgroundCell;
end;

function TcxTreeListBackgroundCellViewInfo.GetHitTest(
  AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    SetHitTestCodes([tlhc_HitAtBackground]);
end;

procedure TcxTreeListBackgroundCellViewInfo.Scroll(const DX, DY: Integer);
begin
end;

{ TcxTreeListFooterCellViewInfo }

constructor TcxTreeListFooterCellViewInfo.CreateEx(ATreeList: TcxCustomTreeList;
  const ABounds, AVisibleRect: TRect);
begin
  inherited CreateEx(ATreeList, ABounds, AVisibleRect);
  FBorders := cxBordersAll;
end;

class function TcxTreeListFooterCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlFooterCell;
end;

procedure TcxTreeListFooterCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  ItemViewParams := Styles.GetColumnFooterParams(Column, Node, GetItem);
  FHidden := FHidden or ((GetItem = nil) or not GetItem.Visible);
end;

procedure TcxTreeListFooterCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if Hidden then Exit;
  if not Transparent then
    Painter.DrawFooterCellContent(ACanvas, BoundsRect, AlignHorz, AlignVert, MultiLine,
      '', ViewParams.Font, ViewParams.TextColor, ViewParams.Color, DrawBackgroundHandler);
  DrawText(ACanvas);
  Painter.DrawFooterCellBorder(ACanvas, BoundsRect);
  if Selected then
  begin
    ACanvas.DrawFocusRect(cxRectInflate(DisplayRect, -1, -1));
    ACanvas.DrawFocusRect(cxRectInflate(DisplayRect, -2, -2));
  end;
end;

procedure TcxTreeListFooterCellViewInfo.DrawText(ACanvas: TcxCanvas);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(FText, cxTextRect(BoundsRect), AlignHorz, AlignVert, MultiLine, ShowEndEllipsis);
  ACanvas.Brush.Style := bsSolid;
end;

function TcxTreeListFooterCellViewInfo.GetRealAlignHorz: TAlignment;
begin
  Result := GetAlignHorz;
  if IsRightToLeftConverted then
    ChangeBiDiModeAlignment(Result);
end;

function TcxTreeListFooterCellViewInfo.GetAlignHorz: TAlignment;
begin
  Result := GetItem.AlignHorz;
end;

function TcxTreeListFooterCellViewInfo.GetAlignVert: TcxAlignmentVert;
begin
  Result := GetItem.AlignVert;
end;

function TcxTreeListFooterCellViewInfo.GetItem: TcxTreeListSummaryItem;
begin
  Result := nil;
end;

function TcxTreeListFooterCellViewInfo.GetDisplayFormat: string;
begin
  if GetItem <> nil then
    Result := GetItem.Format
  else
    Result := '';
end;

function TcxTreeListFooterCellViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
var
  R: TRect;
begin
  Result := False;
  if not Visible then Exit;
  R := ClipRect;
  if AttachNode <> nil then
    R := cxRectOffset(R, AttachNode.ViewData.Origin);
  Result := PtInRect(cxRectInflate(R, ScaleFactor.Apply(cxTextOffset)), AHitTest.HitPoint);
  if not Result then Exit;
  if not VisibleInfoCalculated then
    DoCalculate;
  HitTest.FHitNode := AttachNode;
  if AttachNode <> nil then
    SetHitTestCodes([tlhc_HitAtNode]);
  TcxTreeListHitTest(AHitTest).HitTestItem := Self;
  HitTest.FHitColumn := Column;
  SetHitTestCodes([tlhc_HitAtColumn]);
  if (Node = nil) or (Node.Parent = nil) then
    SetHitTestCodes([tlhc_HitAtFooterItem])
  else
    SetHitTestCodes([tlhc_HitAtGroupFooterItem]);
end;

function TcxTreeListFooterCellViewInfo.GetMultiline: Boolean;
begin
  Result := (Column.Position.LineCount > 1) and (GetItem <> nil) and GetItem.MultiLine;
end;

function TcxTreeListFooterCellViewInfo.GetValue: Variant;
begin
  if GetItem <> nil then
    Result := Node.FooterSummaryValues[GetItem.AbsoluteIndex]
  else
    Result := FText;
end;

procedure TcxTreeListFooterCellViewInfo.Initialize(
  ANode, AttachNode: TcxTreeListNode; AColumn: TcxTreeListColumn);
begin
  FAttachNode := AttachNode;
  FNode := ANode;
  FColumn := AColumn;
  FText := '';
end;

procedure TcxTreeListFooterCellViewInfo.Scroll(const DX, DY: Integer);
begin
  if Column.Position.Band.FixedKind <> tlbfNone then Exit;
  inherited Scroll(DX, 0);
end;

function TcxTreeListFooterCellViewInfo.GetHintBounds: TRect;
begin
  Result := cxRectOffset(DisplayRect, GetCellOrigin);
end;

function TcxTreeListFooterCellViewInfo.IsNeedHint(ACanvas: TcxCanvas;
  const P: TPoint; out AText: TCaption; out AIsMultiLine: Boolean;
  out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean;
var
  AFlags: Integer;
  AAccessibleTextBounds, ANeededTextBounds: TRect;
begin
  AFlags := cxAlignTop or cxAlignLeft or cxDontPrint;
  if AIsMultiLine then
    AFlags := AFlags or cxWordBreak;
  ACanvas.Font := ViewParams.Font;
  AIsMultiLine := MultiLine;

  AAccessibleTextBounds := cxRectOffset(cxTextRect(BoundsRect), GetCellOrigin);
  ANeededTextBounds := AAccessibleTextBounds;
  AText := FText;
  ACanvas.TextExtent(AText, ANeededTextBounds, AFlags);
  Result := PtInRect(AAccessibleTextBounds, P) and
    not cxRectContain(AAccessibleTextBounds, ANeededTextBounds);
  ATextRect := ANeededTextBounds;
  IsNeedOffsetHint := False;
end;

procedure TcxTreeListFooterCellViewInfo.UpdateHotTrackState(
  const APoint: TPoint);
begin
end;

function TcxTreeListFooterCellViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetHintBounds, P);
end;

function TcxTreeListFooterCellViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxTreeListFooterCellViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

function TcxTreeListFooterCellViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := Column.Options.CellEndEllipsis;
end;

{ TcxTreeListFooterSingleCellViewInfo }

function TcxTreeListFooterSingleCellViewInfo.GetItem: TcxTreeListSummaryItem;
begin
  Result := SummaryItem;
end;

function TcxTreeListFooterSingleCellViewInfo.GetSelected: Boolean;
begin
  Result := TreeList.Controller.IsObjectSelected(GetItem);
end;

procedure TcxTreeListFooterSingleCellViewInfo.Initialize(
  ANode, AttachNode: TcxTreeListNode;
  AColumn: TcxTreeListColumn; ASummaryItem: TcxTreeListSummaryItem);
begin
  inherited Initialize(ANode, AttachNode, AColumn);
  FSummaryItem := ASummaryItem;
  FText := Node.FooterSummaryTexts[GetItem.AbsoluteIndex];
end;

{ TcxTreeListFooterMultiItemsCellViewInfo }

function TcxTreeListFooterMultiItemsCellViewInfo.CanDifferentStyles: Boolean;
begin
  Result := True {Assigned(Styles.FOnGetColumnFooterStyle)} and (VisibleCount > 1);
end;

procedure TcxTreeListFooterMultiItemsCellViewInfo.DrawText(ACanvas: TcxCanvas);
var
  R, R1: TRect;
  AText: string;
  AWidth, ACountInLine, I, J: Integer;
begin
  if not CanDifferentStyles then
    inherited DrawText(ACanvas)
  else
  begin
    R := cxTextRect(DisplayRect);
    J := 0;
    FNeedShowHint := False;
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(R);
      FHintRect := R;
      R := cxRectSetHeight(R, cxTextHeight(ViewParams.Font) + ScaleFactor.Apply(cxTextOffset) * 2);
      ACanvas.Font := ViewParams.Font;
      ACanvas.Brush.Style := bsClear;
      AWidth := 0;
      ACountInLine := 0;
      for I := 0 to SummaryItems.Count - 1 do
      begin
        if not SummaryItems[I].Visible then Continue;
        Inc(J);
        Inc(R.Left, AWidth);
        AText := Node.FooterSummaryTexts[SummaryItems[I].AbsoluteIndex];
        if J < VisibleCount then
          AText := AText + dxFormatSettings.ListSeparator;
        AWidth := cxTextWidth(ViewParams.Font, AText);
        if (ACountInLine = 0) or (cxRectWidth(R) >= AWidth) or not MultiLine then
          Inc(ACountInLine)
        else
          if (cxRectWidth(R) < AWidth) then
          begin
            R1 := cxRectSetTop(cxTextRect(DisplayRect), R.Bottom, cxRectHeight(R));
            ACountInLine := 1;
            FNeedShowHint := FNeedShowHint or (R1.Bottom > cxTextRect(DisplayRect).Bottom);
            if FNeedShowHint then
              Inc(ACountInLine)
            else
              R := R1;
          end;
        FNeedShowHint := FNeedShowHint or ((R.Left + AWidth) > cxTextRect(DisplayRect).Right);
        ACanvas.Font.Color := Styles.GetColumnFooterParams(Column, Node, SummaryItems[I]).TextColor;
        ACanvas.Brush.Style := bsClear;
        ACanvas.DrawTexT(AText, R, AlignHorz, AlignVert, False, ShowEndEllipsis);
        ACanvas.Brush.Style := bsSolid;
      end;
      FHintRect.Bottom := R.Bottom;
      FHintRect.Right := R.Right;
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.GetAlignHorz: TAlignment;
begin
  Result := inherited GetAlignHorz;
  if CanDifferentStyles and not Multiline then
    Result := taLeftJustify;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.GetAlignVert: TcxAlignmentVert;
begin
  Result := inherited GetAlignVert;
  if CanDifferentStyles then
    Result := vaTop;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.GetItem: TcxTreeListSummaryItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to SummaryItems.Count - 1 do
  begin
    Result := SummaryItems[I];
    if Result.Visible then Break;
  end;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.GetSelected: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to SummaryItems.Count - 1 do
    if SummaryItems[I].Visible and
      TreeList.Controller.IsObjectSelected(SummaryItems[I]) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TcxTreeListFooterMultiItemsCellViewInfo.Initialize(
  ADataNode, AttachNode: TcxTreeListNode; AColumn: TcxTreeListColumn;
  ASummaryItems: TcxTreeListSummaryItems);
var
  I: Integer;
begin
  inherited Initialize(ADataNode, AttachNode, AColumn);
  FSummaryItems := ASummaryItems;
  FVisibleCount := 0;
  for I := 0 to SummaryItems.Count - 1 do
    if SummaryItems[I].Visible then
    begin
      Inc(FVisibleCount);
      if FText <> '' then
        FText := FText + Separator;
      FText := FText + Node.FooterSummaryTexts[SummaryItems[I].AbsoluteIndex];
    end;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.IsNeedHint(ACanvas: TcxCanvas;
  const P: TPoint; out AText: TCaption; out AIsMultiLine: Boolean;
  out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean;
begin
  if not CanDifferentStyles then
    Result := inherited IsNeedHint(ACanvas, P, AText, AIsMultiLine, ATextRect,
      IsNeedOffsetHint)
  else
  begin
    ACanvas.Font := ViewParams.Font;
    AIsMultiLine := MultiLine;
    Result := FNeedShowHint and PtInRect(cxRectOffset(FHintRect, GetCellOrigin), P);
    AText := FText;
    ATextRect := FHintRect;
    IsNeedOffsetHint := False;
  end;
end;

function TcxTreeListFooterMultiItemsCellViewInfo.GetSeparator: string;
begin
  Result := dxFormatSettings.ListSeparator;
  if Multiline then
    Result := dxCRLF;
end;

{ TcxTreeListCustomHeaderCellViewInfo }

constructor TcxTreeListCustomHeaderCellViewInfo.CreateEx(
  ATreeList: TcxCustomTreeList; const ABounds, AVisibleRect: TRect);
begin
  inherited CreateEx(ATreeList, ABounds, AVisibleRect);
  FBorders := cxBordersAll;
  FState := cxbsNormal;
end;

function TcxTreeListCustomHeaderCellViewInfo.CanNeighborFor(
  ACandidate: TcxTreeListHeaderCellViewInfo): Boolean;
begin
  Result := not cxRectIsEmpty(ACandidate.DisplayRect) and
    not cxRectIsEmpty(DisplayRect);
end;

procedure TcxTreeListCustomHeaderCellViewInfo.CheckClipping(
  const ADisplayRect, AAvailableRect: TRect);
begin
  inherited CheckClipping(ADisplayRect, AAvailableRect);
  ClipRect := cxRectInflate(ClipRect, BordersMargins);
end;

function TcxTreeListCustomHeaderCellViewInfo.CheckNeighbors(
  ACandidate: TcxTreeListHeaderCellViewInfo; AShift: Integer): Boolean;
begin
  Result := False;
  if AShift = 0 then Exit;
  if (BoundsRect.Right = ACandidate.BoundsRect.Left) and cxRectIsEqual(VisibleRect, ACandidate.VisibleRect) then
  begin
    Include(FNeighbors, nRight);
    Include(ACandidate.FNeighbors, nLeft);
    ACandidate.BordersMargins.Left := AShift;
    Result := True;
  end;
  if CanNeighborFor(ACandidate) and (BoundsRect.Bottom =  ACandidate.BoundsRect.Top) then
  begin
    Include(FNeighbors, nBottom);
    Include(ACandidate.FNeighbors, nTop);
    ACandidate.BordersMargins.Top := AShift;
    Result := True;
  end;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.Click;
begin
end;

procedure TcxTreeListCustomHeaderCellViewInfo.DoCalculate;
begin
  FTextBounds := cxTextRect(cxExcludeBorders(BoundsRect, Borders));
  inherited DoCalculate;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := cxRectInflate(BoundsRect, BordersMargins);
  if ExtPaintStyle then
    Painter.DrawScaledHeaderEx(ACanvas, R, TextBounds, Neighbors, Borders,
      State, GetRealAlignHorz, GetAlignVert, GetMultiline, GetShowEndEllipsis, GetText, ACanvas.Font,
      ViewParams.TextColor, ViewParams.Color, ScaleFactor, DrawBackgroundHandler)
  else
      Painter.DrawScaledHeader(ACanvas, R, TextBounds, Neighbors, Borders,
        State, GetRealAlignHorz, GetAlignVert, GetMultiline, GetShowEndEllipsis, GetText, ACanvas.Font,
        ViewParams.TextColor, ViewParams.Color, ScaleFactor, DrawBackgroundHandler, IsLast);
end;

procedure TcxTreeListCustomHeaderCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, AClientBounds);
  FNeighbors := TdxRightToLeftLayoutConverter.ConvertNeighbors(Neighbors);
  BordersMargins := TdxRightToLeftLayoutConverter.ConvertOffsets(BordersMargins);
end;

function TcxTreeListCustomHeaderCellViewInfo.GetAlignHorz: TAlignment;
begin
  Result := taLeftJustify;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetRealAlignHorz: TAlignment;
begin
  Result := GetAlignHorz;
  if IsRightToLeftConverted then
    ChangeBiDiModeAlignment(Result);
end;

function TcxTreeListCustomHeaderCellViewInfo.GetAlignVert: TcxAlignmentVert;
begin
  Result := vaTop;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetIsVisible: Boolean;
begin
  Result := True;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetMultiline: Boolean;
begin
  Result := False;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := False;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetText: string;
begin
  Result := '';
end;

function TcxTreeListCustomHeaderCellViewInfo.NeedRecalculateOnStateChanged: Boolean;
begin
  Result := False;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.SetPressed(AValue: Boolean);
begin
  if (AValue = Pressed) or not GetIsVisible then Exit;
  if AValue then
    TreeList.Controller.PressedHeader := Self
  else
    TreeList.Controller.PressedHeader := nil;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetHintBounds: TRect;
begin
  Result := DisplayRect;
end;

function TcxTreeListCustomHeaderCellViewInfo.IsNeedHint(ACanvas: TcxCanvas;
  const P: TPoint; out AText: TCaption; out AIsMultiLine: Boolean;
  out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean;
var
  R: TRect;
  AFlags: Integer;
begin
  AText := GetText;
  try
    AFlags := cxAlignTop or cxAlignLeft or cxDontPrint;
    if AIsMultiLine then
      AFlags := AFlags or cxWordBreak;
    ACanvas.Font := ViewParams.Font;
    R := TextBounds;
    ACanvas.TextExtent(GetText, R, AFlags);
    AIsMultiLine := MultiLine;
    Result := PtInRect(TextBounds, P) and
      ((cxRectWidth(R) > cxRectWidth(TextBounds)) or (cxRectHeight(R) > cxRectHeight(TextBounds)));
  finally
    AText := GetText;
    ATextRect := R;
    IsNeedOffsetHint := False;
  end;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.UpdateHotTrackState(
  const APoint: TPoint);
begin
  if Pressed then Exit;
  if (TreeList.Controller.DragItem = nil) and PtInRect(GetClipRect, APoint) then
    State := cxbsHot
  else
    State := cxbsNormal;
end;

function TcxTreeListCustomHeaderCellViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(DisplayRect, P);
end;

function TcxTreeListCustomHeaderCellViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxTreeListCustomHeaderCellViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetHotTrack: Boolean;
begin
  Result := State = cxbsHot;
end;

function TcxTreeListCustomHeaderCellViewInfo.GetPressed: Boolean;
begin
  Result := State = cxbsPressed;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.SetHotTrack(AValue: Boolean);
begin
  if Pressed or not GetIsVisible then Exit;
  if AValue then
    State := cxbsHot
  else
    State := cxbsNormal;
end;

procedure TcxTreeListCustomHeaderCellViewInfo.SetState(AValue: TcxButtonState);
begin
  if (State <> AValue) and GetIsVisible then
  begin
    FState := AValue;
    Invalidate(NeedRecalculateOnStateChanged);
  end;
end;

{ TcxTreeListIndicatorCellViewInfo }

destructor TcxTreeListIndicatorCellViewInfo.Destroy;
var
  APopup: TcxTreeListPopup;
begin
  if FSizingArea <> nil then
  begin
    TreeList.ViewInfo.HitTestCells.Remove(FSizingArea);
    FSizingArea.Free;
  end;
  if FPosition in [tlipBands, tlipColumns] then
  begin
    APopup := GetPopup;
    if APopup <> nil then
      APopup.Owner := nil;
  end;
  inherited Destroy;
end;

class function TcxTreeListIndicatorCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlIndicatorCell;
end;

function TcxTreeListIndicatorCellViewInfo.ActualIndicatorKind: TcxIndicatorKind;
begin
  Result := ikNone;
  if Node = nil then
    Exit;
  if Node.Focused then
  begin
    if TreeList.OptionsSelection.MultiSelect and Node.Selected then
      Result := ikMultiArrow
    else
      Result := ikArrow;
   if TreeList.IsNodeInserting(Node) then
      Result := ikInsert
   else
      if TreeList.IsNodeEdited(Node) then
        Result := ikEdit;
  end
  else
    if Node.Selected then
      Result := ikMultiDot;
end;

procedure TcxTreeListIndicatorCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  FKind := ActualIndicatorKind;
  case Position of
    tlipBands:
      ItemViewParams := TreeList.Styles.GetBandHeaderParams(nil);
    tlipColumns:
      ItemViewParams := TreeList.Styles.GetColumnHeaderParams(nil);
    tlipContent, tlipFooter:
      ItemViewParams := TreeList.Styles.GetIndicatorParams;
  end;
end;

procedure TcxTreeListIndicatorCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if Position = tlipContent then
  begin
    if ExtPaintStyle then
      Painter.DrawScaledIndicatorItemEx(ACanvas, DisplayRect, Kind, ViewParams.Color, ScaleFactor, DrawBackgroundHandler)
    else
      Painter.DrawScaledIndicatorItem(ACanvas, DisplayRect, Kind, ViewParams.Color, ScaleFactor, DrawBackgroundHandler);

    ACanvas.ExcludeClipRect(ClipRect);
  end
  else
    if Position = tlipFooter then
      Painter.DrawFooterPanel(ACanvas, DisplayRect, ViewParams, Borders)
    else
    begin
      inherited DoDraw(ACanvas);
      if (Position in [tlipBands, tlipColumns]) and IsQuickCustomizationEnabled then
        Painter.DrawScaledIndicatorCustomizationMark(ACanvas, DisplayRect, ViewParams.TextColor, ScaleFactor);
    end;
end;

function TcxTreeListIndicatorCellViewInfo.GetHitTest(
  AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if not Result then Exit;
  SetHitTestCodes([tlhc_HitAtIndicator]);
  case Position of
    tlipBands:
      if IsQuickCustomizationEnabled then
        SetHitTestCodes([tlhc_HitAtBandCustomizing]);
    tlipColumns:
      if IsQuickCustomizationEnabled then
        SetHitTestCodes([tlhc_HitAtColumnCustomizing]);
    tlipFooter:
      SetHitTestCodes([tlhc_HitAtFooter]);
  end;
end;

function TcxTreeListIndicatorCellViewInfo.GetHintBounds: TRect;
begin
  Result := cxRectOffset(inherited GetHintBounds, GetCellOrigin);
end;

function TcxTreeListIndicatorCellViewInfo.GetPopup: TcxTreeListCustomizationPopup;
begin
  if Position = tlipBands then
    Result := TreeList.Controller.BandsCustomizationPopup
  else
    Result := TreeList.Controller.ColumnsCustomizationPopup;
end;

procedure TcxTreeListIndicatorCellViewInfo.Initialize(ANode: TcxTreeListNode;
  APosition: TcxTreeListIndicatorPosition);
begin
  FNode := ANode;
  FAttachNode := ANode;
  FPosition := APosition;
  FState := cxbsNormal;
  FPosition := APosition;
end;

function TcxTreeListIndicatorCellViewInfo.IsQuickCustomizationEnabled: Boolean;
begin
  with TreeList.OptionsCustomizing do
  begin
    if Position = tlipBands then
      Result := BandsQuickCustomization and BandCustomizing
    else
      Result := ColumnsQuickCustomization and ColumnCustomizing;
  end;
end;

procedure TcxTreeListIndicatorCellViewInfo.Scroll(const DX, DY: Integer);
begin
  inherited Scroll(0, DY);
end;

procedure TcxTreeListIndicatorCellViewInfo.SetPressed(AValue: Boolean);
begin
  if AValue then
    State := cxbsPressed
  else
    State := cxbsNormal;
end;

procedure TcxTreeListIndicatorCellViewInfo.ShowPopup;
var
  APopup: TcxTreeListCustomizationPopup;
begin
  APopup := GetPopup;
  Pressed := True;
  APopup.Owner := Self;
  APopup.Popup;
end;

function TcxTreeListIndicatorCellViewInfo.GetOwnerBounds: TRect;
begin
  Result := DisplayRect;
end;

procedure TcxTreeListIndicatorCellViewInfo.PopupClosed;
begin
  Pressed := False;
end;

procedure TcxTreeListIndicatorCellViewInfo.SetKind(AValue: TcxIndicatorKind);
begin
  if AValue <> FKind then
  begin
    FKind := AValue;
    Invalidate;
  end;
end;

{ TcxTreeListHeaderCellViewInfo }

constructor TcxTreeListHeaderCellViewInfo.CreateEx(ATreeList: TcxCustomTreeList; const ABounds, AVisibleRect: TRect);
begin
  inherited CreateEx(ATreeList, ABounds, AVisibleRect);
  FFilterButtonState := cxbsNormal;
end;

procedure TcxTreeListHeaderCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  FTextBounds := cxRectInflate(cxExcludeBorders(DisplayRect, Borders), -ScaleFactor.Apply(cxHeaderTextOffset));
  if IsFilterButtonVisible then
    DoCalculateFilterButton;
  if IsSortMarkVisible then
    DoCalculateSortMark;
  DoCalculateGlyphPosition;
end;

procedure TcxTreeListHeaderCellViewInfo.DoCalculateFilterButton;
begin
  FFilterButtonBounds := FTextBounds;
  FFilterButtonBounds.Left := FFilterButtonBounds.Right - GetFilterButtonWidth;
  FTextBounds.Right := FFilterButtonBounds.Left;
  if IsFilterButtonSmartTag then
    FFilterButtonBounds.Bottom := FFilterButtonBounds.Top + GetFilterSmartTagHeight;
  if IsRightToLeftConverted then
  begin
    FFilterButtonBounds := TdxRightToLeftLayoutConverter.ConvertRect(FFilterButtonBounds, BoundsRect);
    FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, BoundsRect);
  end;
end;

procedure TcxTreeListHeaderCellViewInfo.DoCalculateGlyphPosition;
var
  AWidth, AHeight: Integer;
  AAlignHorz: TAlignment;
begin
  if Glyph.Empty then Exit;
  AWidth := GetGlyphSize.cx;
  AHeight := GetGlyphSize.cy;
  AAlignHorz := GlyphAlignHorz;
  if IsRightToLeftConverted then
    ChangeBiDiModeAlignment(AAlignHorz);
  case AAlignHorz of
    taLeftJustify:
      begin
        FGlyphPosition.X := FTextBounds.Left;
        Inc(FTextBounds.Left, AWidth + cxHeaderTextOffset);
      end;
    taRightJustify:
      begin
        Dec(FTextBounds.Right, AWidth);
        FGlyphPosition.X := FTextBounds.Right;
        Dec(FTextBounds.Right, cxHeaderTextOffset);
      end;
    taCenter:
      FGlyphPosition.X := (FTextBounds.Left + FTextBounds.Right - AWidth) div 2;
  end;
  case GlyphAlignVert of
    vaTop:
      FGlyphPosition.Y := FTextBounds.Top;
    vaBottom:
      FGlyphPosition.Y := FTextBounds.Bottom - AHeight;
    vaCenter:
      FGlyphPosition.Y := (FTextBounds.Bottom + FTextBounds.Top - AHeight) div 2;
  end;
  GlyphClipping := not cxInRange(FGlyphPosition.X, ClipRect.Left, ClipRect.Right - AWidth) or
    not cxInRange(FGlyphPosition.Y, ClipRect.Top, ClipRect.Bottom - AHeight);
end;

procedure TcxTreeListHeaderCellViewInfo.DoCalculateSortMark;
begin
  FSortMarkBounds := FTextBounds;
  FSortMarkBounds.Left := FSortMarkBounds.Right - GetSortMarkWidth;
  FTextBounds.Right := FSortMarkBounds.Left;
  if IsRightToLeftConverted then
  begin
    FSortMarkBounds := TdxRightToLeftLayoutConverter.ConvertRect(FSortMarkBounds, BoundsRect);
    FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, BoundsRect);
  end;
end;

procedure TcxTreeListHeaderCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  inherited DoDraw(ACanvas);

  if not Glyph.Empty then
  begin
    ACanvas.SaveClipRegion;
    try
      if GlyphClipping then
        ACanvas.IntersectClipRect(ClipRect);
      Glyph.StretchDraw(ACanvas.Handle, cxRectSetOrigin(cxRect(GetGlyphSize), FGlyphPosition));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;

  if IsSortMarkVisible then
    DoDrawSortMark(ACanvas);
  if IsFilterButtonVisible then
    DoDrawFilterButton(ACanvas);
  if State = cxbsPressed then
    Painter.DrawHeaderPressed(ACanvas, DisplayRect);
  if Selected then
  begin
    ACanvas.DrawFocusRect(cxRectInflate(cxRectInflate(DisplayRect, BordersMargins), -1));
    ACanvas.DrawFocusRect(cxRectInflate(cxRectInflate(DisplayRect, BordersMargins), -2));
  end;
end;

procedure TcxTreeListHeaderCellViewInfo.DoDrawFilterButton(ACanvas: TcxCanvas);
begin
  if IsFilterButtonSmartTag then
    Painter.DrawScaledFilterSmartTag(ACanvas, FilterButtonBounds, GetFilterSmartTagState, IsFilterButtonActive, ScaleFactor)
  else
    Painter.DrawScaledFilterDropDownButton(ACanvas, FilterButtonBounds, FilterButtonState, IsFilterButtonActive, ScaleFactor);
end;

procedure TcxTreeListHeaderCellViewInfo.DoDrawSortMark(ACanvas: TcxCanvas);
begin
  Painter.DrawScaledSortingMark(ACanvas, FSortMarkBounds, SortOrder = soAscending, ScaleFactor);
end;

procedure TcxTreeListHeaderCellViewInfo.FilterButtonStateChanged;
begin
  InvalidateFilterButton;
end;

function TcxTreeListHeaderCellViewInfo.GetDragSizing: IcxDragSizing;
begin
  Supports(Item, IcxDragSizing, Result);
end;

function TcxTreeListHeaderCellViewInfo.GetFixed: Boolean;
begin
  Result := False;
end;

function TcxTreeListHeaderCellViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    if PtInRect(FilterButtonBounds, AHitTest.HitPoint) then
      SetHitTestCodes([tlhc_HitAtColumnHeaderFilterButton])
    else
      SetHitItem;
end;

function TcxTreeListHeaderCellViewInfo.GetMultiline: Boolean;
begin
  Result := FCaption.MultiLine;
end;

function TcxTreeListHeaderCellViewInfo.GetSelected: Boolean;
begin
  Result := TreeList.Controller.IsObjectSelected(Item);
end;

function TcxTreeListHeaderCellViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := FCaption.ShowEndEllipsis;
end;

function TcxTreeListHeaderCellViewInfo.GetSortMarkWidth: Integer;
begin
  Result := Painter.ScaledSortingMarkAreaSize(ScaleFactor).X;
end;

function TcxTreeListHeaderCellViewInfo.GetSortOrder: TcxDataSortOrder;
begin
  Result := soNone;
end;

procedure TcxTreeListHeaderCellViewInfo.Initialize(ACaption: TcxTreeListCaption);
begin
  FCaption := ACaption;
  FItem := ACaption.Owner;
  LinkItem;
end;

procedure TcxTreeListHeaderCellViewInfo.InvalidateFilterButton;
begin
  TreeList.InvalidateRect(FilterButtonBounds, False);
end;

function TcxTreeListHeaderCellViewInfo.IsFilterable: Boolean;
begin
  Result := False;
end;

function TcxTreeListHeaderCellViewInfo.IsFilterButtonActive: Boolean;
begin
  Result := False;
end;

function TcxTreeListHeaderCellViewInfo.IsFilterButtonAlwaysVisible: Boolean;
begin
  Result := False;
end;

function TcxTreeListHeaderCellViewInfo.IsFilterButtonSmartTag: Boolean;
begin
  Result := False;
end;

function TcxTreeListHeaderCellViewInfo.IsFilterButtonVisible: Boolean;
begin
  Result := IsFilterable and (IsFilterButtonAlwaysVisible or (State in [cxbsHot, cxbsPressed]) or
    (FilterButtonState = cxbsPressed) or (IsFilterButtonSmartTag and IsFilterButtonActive));
end;

function TcxTreeListHeaderCellViewInfo.IsSortMarkVisible: Boolean;
begin
  Result := SortOrder <> soNone;
end;

procedure TcxTreeListHeaderCellViewInfo.LinkItem;
begin
end;

function TcxTreeListHeaderCellViewInfo.NeedRecalculateOnStateChanged: Boolean;
begin
  Result := IsFilterable and not IsFilterButtonAlwaysVisible;
end;

procedure TcxTreeListHeaderCellViewInfo.ReleaseReference(var AReference: TObject);
begin
  if AReference = Self then
    AReference := nil;
end;

procedure TcxTreeListHeaderCellViewInfo.Scroll(const DX, DY: Integer);
begin
  if Fixed then Exit;
  OffsetRect(FSortMarkBounds, DX, 0);
  OffsetRect(FTextBounds, DX,  0);
  FGlyphPosition := cxPointOffset(FGlyphPosition, DX, 0);
  inherited Scroll(DX,  0);
end;

procedure TcxTreeListHeaderCellViewInfo.SetHitItem;
begin
  HitTest.FDragItem := Self;
end;

procedure TcxTreeListHeaderCellViewInfo.UpdateFilterButtonHotTrackState(const APoint: TPoint);
begin
  if (FilterButtonState = cxbsPressed) or (TreeList.Controller.DragItem <> nil) then
    Exit;
  if PtInRect(FilterButtonBounds, APoint) then
    FilterButtonState := cxbsHot
  else
    FilterButtonState := cxbsNormal;
end;

procedure TcxTreeListHeaderCellViewInfo.UpdateFilterButtonStateOnMouseDown;
begin
  case FilterButtonState of
    cxbsPressed:
      FilterButtonState := cxbsHot;
    cxbsHot:
      FilterButtonState := cxbsPressed;
  end;
end;

procedure TcxTreeListHeaderCellViewInfo.UpdateHotTrackState(const APoint: TPoint);
begin
  inherited UpdateHotTrackState(APoint);
  UpdateFilterButtonHotTrackState(APoint);
end;

function TcxTreeListHeaderCellViewInfo.GetAlignHorz: TAlignment;
begin
  Result := FCaption.FAlignHorz;
end;

function TcxTreeListHeaderCellViewInfo.GetAlignVert: TcxAlignmentVert;
begin
  Result := FCaption.FAlignVert;
end;

function TcxTreeListHeaderCellViewInfo.GetFilterButtonWidth: Integer;
begin
  if IsFilterButtonSmartTag then
    Result := Painter.ScaledFilterSmartTagSize(ScaleFactor).cx
  else
    Result := Painter.ScaledFilterDropDownButtonSize(ScaleFactor).X;
end;

function TcxTreeListHeaderCellViewInfo.GetFilterSmartTagHeight: Integer;
begin
  Result := Painter.ScaledFilterSmartTagSize(ScaleFactor).cy;
end;

function TcxTreeListHeaderCellViewInfo.GetFilterSmartTagState: TcxFilterSmartTagState;
begin
  case FilterButtonState of
    cxbsHot:
      Result := fstsHot;
    cxbsPressed:
      Result := fstsPressed;
    else
      if State = cxbsHot then
        Result := fstsParentHot
      else
        Result := fstsNormal;
  end;
end;

function TcxTreeListHeaderCellViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := FCaption.Glyph;
end;

function TcxTreeListHeaderCellViewInfo.GetGlyphAlignHorz: TAlignment;
begin
  Result := FCaption.GlyphAlignHorz;
end;

function TcxTreeListHeaderCellViewInfo.GetGlyphAlignVert: TcxAlignmentVert;
begin
  Result := FCaption.GlyphAlignVert;
end;

function TcxTreeListHeaderCellViewInfo.GetGlyphSize: TSize;
begin
  Result := dxGetImageSize(Glyph, nil, -1, ScaleFactor);
end;

procedure TcxTreeListHeaderCellViewInfo.SetFilterButtonState(AValue: TcxButtonState);
begin
  if AValue <> FFilterButtonState then
  begin
    FFilterButtonState := AValue;
    FilterButtonStateChanged;
  end;
end;

function TcxTreeListHeaderCellViewInfo.GetText: string;
begin
  Result := FCaption.Text;
end;

{ TcxTreeListBandHeaderCellViewInfo }

destructor TcxTreeListBandHeaderCellViewInfo.Destroy;
begin
  if Band <> nil then
    ReleaseReference(TObject(Band.FHeaderCell));
  inherited Destroy;
end;

class function TcxTreeListBandHeaderCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlBandHeaderCell;
end;

procedure TcxTreeListBandHeaderCellViewInfo.Click;
begin
  TreeList.DoBandHeaderClick(Band);
end;

procedure TcxTreeListBandHeaderCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  ItemViewParams := TreeList.Styles.GetBandHeaderParams(Band);
end;

function TcxTreeListBandHeaderCellViewInfo.GetFixed: Boolean;
begin
  Result := Band.FixedKind <> tlbfNone;
end;

function TcxTreeListBandHeaderCellViewInfo.GetIsVisible: Boolean;
begin
  Result := Band.ActuallyVisible;
end;

procedure TcxTreeListBandHeaderCellViewInfo.SetHitItem;
begin
  inherited SetHitItem;
  HitTest.FHitBand := Band;
  SetHitTestCodes([tlhc_HitAtBandHeader]);
end;

procedure TcxTreeListBandHeaderCellViewInfo.LinkItem;
begin
  Band.FHeaderCell := Self;
end;

function TcxTreeListBandHeaderCellViewInfo.GetBand: TcxTreeListBand;
begin
  Result := Item as TcxTreeListBand;
end;

{ TcxTreeListColumnHeaderCellViewInfo }

destructor TcxTreeListColumnHeaderCellViewInfo.Destroy;
begin
  if IsFilterPopupOwner then
    FilterPopup.Owner := nil;
  if Column <> nil then
    ReleaseReference(TObject(Column.FHeaderCell));
  inherited Destroy;
end;

class function TcxTreeListColumnHeaderCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlColumnHeaderCell;
end;

function TcxTreeListColumnHeaderCellViewInfo.CloseFilterPopupOnDestruction: Boolean;
begin
  Result := False;
end;

procedure TcxTreeListColumnHeaderCellViewInfo.FilterPopupClosed;
begin
  FilterButtonState := cxbsNormal;
end;

procedure TcxTreeListColumnHeaderCellViewInfo.InitFilterPopup(APopup: TdxUIElementPopupWindow);
begin
  APopup.OwnerParent := TreeList;
  APopup.Font := TreeList.Font;
  APopup.LookAndFeel := TreeList.LookAndFeel;
  APopup.BorderStyle := TreeList.LookAndFeelPainter.PopupBorderStyle;
  APopup.OwnerBounds := GetFilterPopupOwnerBounds;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopupLinkComponent: TComponent;
begin
  Result := Column;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := Column.GetFilterPopupMode;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopupOptions: TObject;
begin
  Result := Column.Options;
end;

procedure TcxTreeListColumnHeaderCellViewInfo.CheckFilterPopupOwner;
begin
  if IsFilterPopupExist and FilterPopup.Visible and (FilterPopup.Owner = nil) and
    (FilterPopup.LinkComponent = GetFilterPopupLinkComponent) then
  begin
    FilterPopup.Owner := Self;
    FilterButtonState := cxbsPressed;
  end;
end;

procedure TcxTreeListColumnHeaderCellViewInfo.Click;
begin
  TreeList.DoColumnHeaderClick(Column);
end;

procedure TcxTreeListColumnHeaderCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  ItemViewParams := TreeList.Styles.GetColumnHeaderParams(Column);
end;

procedure TcxTreeListColumnHeaderCellViewInfo.FilterButtonStateChanged;
begin
  inherited FilterButtonStateChanged;
  if FilterButtonState = cxbsPressed then
    ShowFilterPopup;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFixed: Boolean;
begin
  Result := Band.FixedKind <> tlbfNone;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetIsVisible: Boolean;
begin
  Result := Column.ActuallyVisible;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopup: TdxFilterPopupWindow;
begin
  Result := TreeList.Controller.FilterPopup;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopupAlignHorz: TcxPopupAlignHorz;
begin
  if dxGetFilterPopupActualMode(GetFilterPopupMode) = fpmExcel then
    Result := pahLeft
  else
    Result := pahRight;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetFilterPopupOwnerBounds: TRect;
var
  ABounds: TRect;
begin
  Result := FilterButtonBounds;
  if dxGetFilterPopupActualMode(GetFilterPopupMode) <> fpmExcel then
  begin
    ABounds := cxRectInflate(BoundsRect, BordersMargins);
    Result.Left := ABounds.Left;
    Result.Right := ABounds.Right;
  end;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetSortOrder: TcxDataSortOrder;
begin
  Result := Column.GetRealSortOrder;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterable: Boolean;
begin
  Result := Column.FilterableByPopupMenu;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterButtonActive: Boolean;
begin
  Result := Column.Filtered;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterButtonAlwaysVisible: Boolean;
begin
  Result := TreeList.OptionsView.IsColumnHeaderFilterButtonShowedAlways;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterButtonSmartTag: Boolean;
begin
  Result := TreeList.OptionsView.IsColumnHeaderFilterSmartTag;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterPopupExist: Boolean;
begin
  Result := TreeList.Controller.HasFilterPopup;
end;

function TcxTreeListColumnHeaderCellViewInfo.IsFilterPopupOwner: Boolean;
begin
  Result := IsFilterPopupExist and (FilterPopup.Owner = Self);
end;

procedure TcxTreeListColumnHeaderCellViewInfo.LinkItem;
begin
  Column.FHeaderCell := Self;
  CheckFilterPopupOwner;
end;

procedure TcxTreeListColumnHeaderCellViewInfo.SetHitItem;
begin
  inherited SetHitItem;
  HitTest.FHitColumn := Column;
  SetHitTestCodes([tlhc_HitAtColumnHeader]);
end;

procedure TcxTreeListColumnHeaderCellViewInfo.ShowFilterPopup;
begin
  FilterPopup.Owner := Self;
  FilterPopup.AlignHorz := GetFilterPopupAlignHorz;
  FilterPopup.Popup;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetBand: TcxTreeListBand;
begin
  Result := Column.Position.Band;
end;

function TcxTreeListColumnHeaderCellViewInfo.GetColumn: TcxTreeListColumn;
begin
  Result := Item as TcxTreeListColumn;
end;

{ TcxTreeListBandCellViewInfo }

class function TcxTreeListBandCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlBandPartCell;
end;

procedure TcxTreeListBandCellViewInfo.DoCalculate;
var
  ACanSelect: Boolean;
begin
  inherited DoCalculate;
  case Part of
    tlbpHeader:
      ItemViewParams := Styles.GetBandBackgroundParams(Band);
    tlbpContent:
    begin
      ACanSelect := TreeList.OptionsSelection.CellSelect and TreeList.OptionsSelection.InvertSelect;
      if (Node <> nil) and Node.Selected and (not Node.Focused or ACanSelect) then
        ItemViewParams := Styles.GetSelectionParams
      else
        ItemViewParams := Styles.GetBandContentParams(Band, Node);
    end;
    tlbpFooter, tlbpGroupFooter:
      ItemViewParams := Styles.GetBandFooterParams(Band, Node);
    tlbpSeparator:
    begin
      ItemViewParams := Styles.GetBackgroundParams;
      ItemViewParams.Color := ColorToRgb(cxGetNativeColor(OptionsView.FFixedSeparatorColor,
        Painter.DefaultFixedSeparatorColor));
    end;
  end;
end;

procedure TcxTreeListBandCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if Part in [tlbpFooter, tlbpGroupFooter] then
    Painter.DrawFooterPanel(ACanvas, DisplayRect, ViewParams, Borders)
  else
    inherited DoDraw(ACanvas);
  if Part = tlbpSeparator then
    ACanvas.ExcludeClipRect(ClipRect);
end;

function TcxTreeListBandCellViewInfo.GetHitTest(
  AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if not Result then Exit;
  HitTest.FHitBand := Band;
  HitTest.FHitNode := Node;
  case Part of
    tlbpHeader:
      SetHitTestCodes([tlhc_HitAtBandContainer]);
    tlbpContent:
      SetHitTestCodes([tlhc_HitAtBand]);
    tlbpFooter:
      SetHitTestCodes([tlhc_HitAtFooter, tlhc_HitAtBand]);
    tlbpGroupFooter:
      SetHitTestCodes([tlhc_HitAtGroupFooter, tlhc_HitAtBand]);
    tlbpSeparator:
      SetHitTestCodes([tlhc_HitAtSeparator]);
  end;
end;

procedure TcxTreeListBandCellViewInfo.Initialize(ABand: TcxTreeListBand;
  ANode, AAttachNode: TcxTreeListNode; APart: TcxTreeListBandPart);
begin
  FBand := ABand;
  FAttachNode := AAttachNode;
  FNode := ANode;
  FPart := APart;
end;

procedure TcxTreeListBandCellViewInfo.Scroll(const DX, DY: Integer);
begin
  if (Band = nil) or (Band.FixedKind <> tlbfNone) then Exit;
  if Part <> tlbpContent then
    inherited Scroll(DX, 0)
  else
    inherited Scroll(DX, DY);
end;

{ TcxTreeListEditCellViewInfo }

class function TcxTreeListEditCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlEditCell;
end;

procedure TcxTreeListEditCellViewInfo.AfterDrawCellBackground(ACanvas: TcxCanvas);
begin
  inherited AfterDrawCellBackground(ACanvas);
  ConditionalFormattingProvider.AfterDrawCellBackground(Self, ViewParams.Color, ACanvas);
end;

procedure TcxTreeListEditCellViewInfo.AfterDrawCellValue(ACanvas: TcxCanvas);
begin
  inherited AfterDrawCellValue(ACanvas);
  ConditionalFormattingProvider.AfterDrawCellValue(Self, ACanvas);
end;

procedure TcxTreeListEditCellViewInfo.CalculateCellEditorBounds(AViewInfo: TcxCustomEditViewInfo; var R: TRect);
begin
  ConditionalFormattingProvider.CalculateCellEditorBounds(Self, R);
end;

procedure TcxTreeListEditCellViewInfo.CanDrawCellValue(var Allow: Boolean);
begin
  inherited CanDrawCellValue(Allow);
  if Allow then
    ConditionalFormattingProvider.CanDrawCellValue(Self, Allow);
end;

function TcxTreeListEditCellViewInfo.FormatDisplayValue(AValue: Variant): Variant;
begin
  Result := inherited FormatDisplayValue(AValue);
  Result := ConditionalFormattingProvider.FormatDisplayValue(Self, Result);
end;

procedure TcxTreeListEditCellViewInfo.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  inherited AfterCustomDraw(ACanvas);
  ViewInfo.TextColor := ItemViewParams.TextColor;
end;

procedure TcxTreeListEditCellViewInfo.Clear;
begin
  if IsViewDataCreated then
    FreeAndNil(ViewData);
  Properties := nil;
end;

function TcxTreeListEditCellViewInfo.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(EditContainer);
end;

procedure TcxTreeListEditCellViewInfo.DoCalculate;
begin
  ItemViewParams := GetEditViewParams;
  inherited DoCalculate;
end;

procedure TcxTreeListEditCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  AFont: TFont;
  AWindowOrg: TPoint;
begin
  if CellTransparent then
    ACanvas.FrameRect(DisplayRect, BorderColor, 1, Borders)
  else
  begin
    AWindowOrg := ACanvas.WindowOrg;
    ACanvas.WindowOrg := cxNullPoint;
    ACanvas.Rectangle(cxRectOffset(DisplayRect, NodeViewData.Origin),
      ViewParams, Borders, BorderColor);
    ACanvas.WindowOrg := AWindowOrg;
  end;
  AFont := EditViewInfo.Font;
  EditViewInfo.Font := ACanvas.Font;
  EditViewInfo.PaintEx(ACanvas);
  EditViewInfo.Font := AFont;
  if Focused and OptionsView.FocusRect and (Node.ViewData <> nil) and not Node.ViewData.DrawFocusRect and
    (not TreeList.OptionsSelection.HideFocusRect or TreeList.IsFocused) then
    ACanvas.DrawFocusRect(cxExcludeBorders(DisplayRect, Borders));
end;

function TcxTreeListEditCellViewInfo.GetButtonTransparency: TcxEditButtonTransparency;
var
  B1: TcxEditingControlEditShowButtons;
  B2: TcxEditItemShowEditButtons;
  AFocused: Boolean;
begin
  AFocused := Node = TreeList.FocusedNode;
  B1 := TreeList.OptionsView.ShowEditButtons;
  B2 := Column.Options.ShowEditButtons;
  if (B2 = eisbAlways) or (B2 = eisbDefault) and
   ((B1 = ecsbAlways) or (B1 = ecsbFocused) and AFocused) then
    Result := ebtNone
  else
    Result := ebtHideInactive;
end;

function TcxTreeListEditCellViewInfo.GetCellOrg: TPoint;
begin
  Result := NodeViewData.Origin
end;

function TcxTreeListEditCellViewInfo.GetDisplayValue: Variant;
begin
  Result := Column.DoOnGetDisplayText(Node, False, True);
end;

function TcxTreeListEditCellViewInfo.GetEditRect: TRect;
begin
  Result := CellEditRect;
  if (Node <> nil) and (Node.ViewData <> nil) then
    Result := cxRectOffset(Result, Node.ViewData.Origin);
  TreeList.Controller.EditingController.CheckMultilineEditBounds(Result);
  cxRectIntersect(Result, Result, TreeList.ClientBounds);
end;

function TcxTreeListEditCellViewInfo.GetEditViewParams: TcxViewParams;
begin
  if not Selected then
    Result := TreeList.Styles.GetContentParams(Node, Column)
  else
  begin
    Result := TreeList.Styles.GetCellSelectionParams(Node, Column);
    Result.Bitmap := nil;
  end;
end;

function TcxTreeListEditCellViewInfo.GetFocused: Boolean;
begin
  Result := Node.Focused and Column.Focused;
end;

function TcxTreeListEditCellViewInfo.GetHitTest(
  AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := PtInRect(cxRectOffset(ClipRect, NodeViewData.Origin), AHitTest.HitPoint);
  if not Result then Exit;
  TcxTreeListHitTest(AHitTest).FHitNode := Node;
  TcxTreeListHitTest(AHitTest).FHitColumn := Column;
  TcxTreeListHitTest(AHitTest).SetHitState(tlhc_HitAtNode, True);
  case CustomDrawID of
    cxtlPreviewCell:
      TcxTreeListHitTest(AHitTest).SetHitState(tlhc_HitAtNodePreview, True);
  else
    TcxTreeListHitTest(AHitTest).SetHitState(tlhc_HitAtColumn, True);
  end;
  TcxTreeListHitTest(AHitTest).HitTestItem := Self;
end;

function TcxTreeListEditCellViewInfo.GetHotTrack: Boolean;
begin
  Result := Node.HotTrack;
end;

function TcxTreeListEditCellViewInfo.GetIncSearchParams: TcxViewParams;
begin
  Result := TreeList.Styles.GetIncSearchParams;
  if Result.Color = clDefault then
    Result.Color := inherited GetSelectedBKColor;
  if Result.TextColor = clDefault then
    Result.TextColor := inherited GetSelectedTextColor;
end;


function TcxTreeListEditCellViewInfo.GetHintBounds: TRect;
begin
  Result := cxRectOffset(DisplayRect, 0, Node.ViewData.Origin.Y);
end;

function TcxTreeListEditCellViewInfo.IsNeedHint(ACanvas: TcxCanvas;
  const P: TPoint; out AText: TCaption; out AIsMultiLine: Boolean;
  out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean;
var
  ATop: Integer;
begin
  ATop := ViewInfo.Top;
  ViewInfo.Top := Node.ViewData.Origin.Y;
  Result := inherited IsNeedHint(ACanvas, P, AText, AIsMultiLine, ATextRect, IsNeedOffsetHint);
  ViewInfo.Top := ATop;
end;

function TcxTreeListEditCellViewInfo.GetRecordIndex: TdxNativeInt;
begin
  Result := TdxNativeInt(Node);
end;

function TcxTreeListEditCellViewInfo.GetSelected: Boolean;
begin
  Result := Node.Selected and not (Node.Focused and Column.Editing) and
    (not TreeList.OptionsSelection.HideSelection or TreeList.IsFocused);
  if not Node.Focused or not Result then Exit;
  if TreeList.OptionsSelection.CellSelect then
    if TreeList.OptionsSelection.InvertSelect then
      Result := not Focused
    else
      Result := Focused;
end;

function TcxTreeListEditCellViewInfo.GetSelectedTextColor: Integer;
begin
  Result := GetIncSearchParams.TextColor;
end;

function TcxTreeListEditCellViewInfo.GetSelectedBKColor: Integer;
begin
  Result := GetIncSearchParams.Color
end;

function TcxTreeListEditCellViewInfo.GetViewInfoData: Pointer;
begin
  Result := Node;
end;

procedure TcxTreeListEditCellViewInfo.Invalidate(const R: TRect; AEraseBackground: Boolean);
begin
  inherited Invalidate(cxRectOffset(R, NodeViewData.Origin), AEraseBackground);
end;

function TcxTreeListEditCellViewInfo.IsAutoHeight: Boolean;
begin
  Result := TreeList.OptionsView.CellAutoHeight;
end;

function TcxTreeListEditCellViewInfo.IsEditHotTrack(
  const APoint: TPoint): Boolean;
begin
  Result := ViewInfo.IsHotTrack(cxPointOffset(APoint,
    cxPointInvert(GetCellOrg)));
end;

function TcxTreeListEditCellViewInfo.IsEndEllipsis: Boolean;
begin
  Result := inherited IsEndEllipsis and Column.Options.CellEndEllipsis;
end;

function TcxTreeListEditCellViewInfo.IsFixed: Boolean;
begin
  Result := (Band <> nil) and (Band.FixedKind <> tlbfNone);
end;

function TcxTreeListEditCellViewInfo.IsTransparent: Boolean;
begin
  Result := True;
end;

procedure TcxTreeListEditCellViewInfo.Scroll(const DX, DY: Integer);
var
  AVisible: Boolean;
begin
  if IsFixed or (DX = 0) then Exit;
  AVisible := Visible;
  CheckClipping(cxRectOffset(DisplayRect, DX, 0), VisibleBounds);
  ViewInfo.Left := ViewInfo.Left + DX;
  if AVisible <> Visible then
    VisibleInfoCalculated := False;
end;

function TcxTreeListEditCellViewInfo.GetBand: TcxTreeListBand;
begin
  Result := Column.Position.Band;
end;

function TcxTreeListEditCellViewInfo.GetConditionalFormattingProvider: TcxTreeListConditionalFormattingProvider;
begin
  Result := TreeList.ConditionalFormattingProvider;
end;

function TcxTreeListEditCellViewInfo.GetEditing: Boolean;
begin
  Result := Column.Editing;
end;

function TcxTreeListEditCellViewInfo.GetNode: TcxTreeListNode;
begin
  if NodeViewData = nil then
    Result := nil
  else
    Result := NodeViewData.Node;
end;

function TcxTreeListEditCellViewInfo.GetOptionsView: TcxTreeListOptionsView;
begin
  Result := TreeList.OptionsView;
end;

function TcxTreeListEditCellViewInfo.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Control);
end;

{ TcxTreeListIndentCellViewInfo }

class function TcxTreeListIndentCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlIndentCell;
end;

procedure TcxTreeListIndentCellViewInfo.CalculateTreeLines(ACenter: TPoint);
var
  DY, X2: Integer;
  ARect: TRect;
begin
  DY := 0;
  ARect := DisplayRect;
  if (Kind = nikCheck) or (Kind = nikState) or ((Images <> nil) and TreeList.OptionsView.DynamicIndent) then
  begin
    ARect.Right := ARect.Left + TreeList.ViewInfo.LevelInfo[Index + 1].Size.cx;
    ACenter := cxRectCenter(ARect);
  end;
  if (Node.ViewData.PreviewHeight > 0) and (TreeList.Preview.Place = tlppTop) then
    DY := 1;
  X2 := ARect.Right;
  if IsRightToLeftConverted then
    X2 := ARect.Left;
  if ilHorz in Lines then
    FHorzTreeLine := cxRect(ACenter.X, ACenter.Y - 1 + DY, X2, ACenter.Y + DY);
  if [ilVertUp, ilVertDown] * Lines <> [] then
  begin
    FVertTreeLine := cxRect(ACenter.X, ARect.Top, ACenter.X + 1, ARect.Bottom);
    if not (ilVertUp in Lines) then
      FVertTreeLine.Top := ACenter.Y - 1;
    if not (ilVertDown in Lines) then
      FVertTreeLine.Bottom := ACenter.Y - 1;
    if (Images <> nil) or (Kind = nikCheck) then
      FVertTreeLine.Top := GlyphRect.Bottom;

    if (Images <> nil) and (FVertTreeLine.Right >= (DisplayRect.Right - ScaleFactor.Apply(cxTextOffset))) then
      FLines := FLines - [ilVertUp, ilVertDown];
  end;
end;

procedure TcxTreeListIndentCellViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  InitializeImageIndent(Images);
  ItemViewParams := Styles.GetIndentParams(Node, Index);
  FButton := TreeList.OptionsView.Buttons and (Images = nil) and (Kind = nikLevel) and (Index = Node.Level) and Node.HasVisibleChildren;
  FGlyphRect := cxInvalidRect;
  if Button then
  begin
    FGlyphRect := cxRectCenter(DisplayRect,
      Painter.ScaledSmallExpandButtonSize(ScaleFactor),
      Painter.ScaledSmallExpandButtonSize(ScaleFactor));
  end
  else
    if Images <> nil then
      FGlyphRect := cxRectCenter(DisplayRect, dxGetImageSize(Images, ScaleFactor))
    else
      if Kind = nikCheck then
      begin
        if Node.Parent.CheckGroupType = ncgRadioGroup then
          FGlyphRect := cxRectCenter(DisplayRect, Painter.ScaledRadioButtonSize(ScaleFactor))
        else
          FGlyphRect := cxRectCenter(DisplayRect, Painter.ScaledCheckButtonSize(ScaleFactor));
      end;

  if (OptionsView.TreeLineStyle <> tllsNone) and (Lines <> []) then
    CalculateTreeLines(cxRectCenter(cxRectSetWidth(DisplayRect, TreeList.DefaultIndentSize.CX)));
end;

procedure TcxTreeListIndentCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  inherited DoDraw(ACanvas);
  if Kind = nikCheck then
    DrawCheck(ACanvas)
  else
    if Images <> nil then
      DrawImage(ACanvas);
  DrawLines(ACanvas);
end;

procedure TcxTreeListIndentCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphRect, AClientBounds);
end;

procedure TcxTreeListIndentCellViewInfo.DrawCheck(ACanvas: TcxCanvas);
const
  StateMap: array[Boolean, Boolean] of TcxButtonState = (
    (cxbsDisabled, cxbsDisabled), (cxbsNormal, cxbsHot)
  );
begin
  if nsCheckStateInvalid in Node.State then
  begin
    PostMessage(TreeList.Handle, WM_CHECKSTATE, 0, Node.AbsoluteIndex);
    Exit;
  end;
  if Node.Parent.CheckGroupType = ncgRadioGroup then
    Painter.DrawScaledRadioButton(ACanvas, GlyphRect.Left, GlyphRect.Top,
      StateMap[Node.Enabled, HotTrack], CheckState = cbsChecked, False, clDefault, ScaleFactor)
  else
    GetCheckBoxViewInfo(ACanvas).Paint(ACanvas)
end;

procedure TcxTreeListIndentCellViewInfo.DrawImage(ACanvas: TcxCanvas);
begin
  if Images is TImageList then
  begin
    Images.Draw(ACanvas.Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
      dsTransparent, Images.ImageType, True);
    Images.Draw(ACanvas.Canvas, GlyphRect.Left, GlyphRect.Top, OverlayIndex,
      dsTransparent, Images.ImageType, True);
  end
  else
  begin
    cxDrawImage(ACanvas, GlyphRect, nil, Images, ImageIndex, True, nil, ScaleFactor);
    cxDrawImage(ACanvas, GlyphRect, nil, Images, OverlayIndex, True, nil, ScaleFactor);
  end;
end;

procedure TcxTreeListIndentCellViewInfo.DrawLines(ACanvas: TcxCanvas);
begin
  if [ilVertUp, ilVertDown] * Lines <> [] then
    DrawTreeLine(ACanvas, VertTreeLine);
  if ilHorz in Lines then
    DrawTreeLine(ACanvas, HorzTreeLine);
  if Button then
    Painter.DrawScaledSmallExpandButton(ACanvas, GlyphRect, IsExpanded,
      TreeList.ViewInfo.TreeLineColor, ScaleFactor, ViewParams.Color);
end;

procedure TcxTreeListIndentCellViewInfo.DrawTreeLine(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if OptionsView.TreeLineStyle = tllsDot then
    cxFillHalfToneRect(ACanvas.Canvas, ARect, ViewParams.Color, TreeList.ViewInfo.TreeLineColor)
  else
    ACanvas.FillRect(ARect, TreeList.ViewInfo.TreeLineColor);
end;

function TcxTreeListIndentCellViewInfo.GetCheckBoxViewInfo(ACanvas: TcxCanvas): TcxCustomEditViewInfo;
var
  P: TPoint;
  AViewData: TcxCustomEditViewData;
begin
  Result := TreeList.ViewInfo.CheckBoxViewInfo;
  AViewData := TreeList.ViewInfo.CheckBoxViewData;
  AViewData.EditValueToDrawValue(Integer(Node.CheckState), Result);
  P := cxInvalidPoint;
  AViewData.Enabled := Node.Enabled;
  if HotTrack then
    P := cxRectCenter(DisplayRect);
  AViewData.Calculate(ACanvas, DisplayRect, P, cxmbNone, [], Result, False);
  Result.Transparent := True;
end;

function TcxTreeListIndentCellViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
var
  R: TRect;
const
  AImages: array[Boolean] of Integer =
    (tlhc_HitAtImage, tlhc_HitAtStateImage);
begin
  Result := inherited GetHitTest(AHitTest);
  if not Result then Exit;
  HitTest.FHitNode := Node;
  SetHitTestCodes([tlhc_HitAtIndent]);
  if Images <> nil then
    SetHitTestCodes([AImages[Kind = nikState]])
  else
  begin
    if cxIsTouchModeEnabled then
      R := DisplayRect
    else
      R := cxRectInflate(GlyphRect, ScaleFactor.Apply(cxTextOffset));

    R := cxRectOffset(R, GetCellOrigin);
    if Kind = nikCheck then
      SetHitTestCodes([tlhc_HitAtCheckButton])
    else
      if Button and PtInRect(R, AHitTest.HitPoint) then
        SetHitTestCodes([tlhc_HitAtButton])
  end;
end;

procedure TcxTreeListIndentCellViewInfo.InitializeLevelIndent(ANode, AAttachNode: TcxTreeListNode; AIndex: Integer);
begin
  FIndex := AIndex;
  FNode := ANode;
  FAttachNode := AAttachNode;
  FLines := [];
  FLevelNode := ANode;
  while FLevelNode.Level <> AIndex do
    FLevelNode := FLevelNode.Parent;
  if OptionsView.TreeLineStyle <> tllsNone then
  begin
    if Kind in GlyphIndents then
    begin
      if LevelNode.Expanded and (LevelNode.HasVisibleChildren) then
        Include(FLines, ilVertDown);
    end
    else
    begin
      if Kind = nikFooter then
      begin
        if LevelNode.GetNextSiblingVisible <> nil then
          FLines := [ilVertUp, ilVertDown]
      end
      else
        begin
          if (LevelNode <> Node) or (Node.VisibleIndex <> 0) then
            Include(FLines, ilVertUp);
          if LevelNode.GetNextSiblingVisible <> nil then
            Include(FLines, ilVertDown)
          else
            if LevelNode <> Node then
              Exclude(FLines, ilVertUp);
          if (LevelNode = Node) then
            Include(FLines, ilHorz);
        end;
    end;
  end;
end;

procedure TcxTreeListIndentCellViewInfo.InitializeImageIndent(
  AImages: TCustomImageList);
begin
  FImages := AImages;
  FImageIndex := -1;
  FOverlayIndex := -1;
  if (Images = nil) or not (Kind in [nikImage, nikState]) then Exit;
  if Kind = nikImage then
  begin
    FImageIndex := TreeList.DoGetNodeImageIndex(Node, TcxTreeListImageIndexType(Node.Selected));
    if FImageIndex <> - 1 then
      FOverlayIndex := TreeList.DoGetNodeImageIndex(Node, tlitOverlayIndex);
  end
  else
  begin
    FImageIndex := TreeList.DoGetNodeImageIndex(Node, tlitStateIndex);
    if FImageIndex <> - 1 then
      FOverlayIndex := TreeList.DoGetNodeImageIndex(Node, tlitOverlayStateIndex);
  end;
end;

procedure TcxTreeListIndentCellViewInfo.Scroll(const DX, DY: Integer);
begin
  if Fixed then Exit;
  inherited Scroll(DX, 0);
end;

procedure TcxTreeListIndentCellViewInfo.SetSize(ATop, AHeight: Integer);
begin
  CheckClipping(cxRectSetTop(BoundsRect, ATop, AHeight), cxRectSetTop(VisibleBounds, ATop, AHeight));
end;

// IcxHotTrackElement
function TcxTreeListIndentCellViewInfo.GetHintBounds: TRect;
begin
  Result := cxRectOffset(DisplayRect, GetCellOrigin);
end;

function TcxTreeListIndentCellViewInfo.IsNeedHint(
  ACanvas: TcxCanvas; const P: TPoint; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect;
  var IsNeedOffsetHint: Boolean): Boolean;
begin
  AText := '';
  ATextRect := cxEmptyRect;
  AIsMultiLine := False;
  Result := False;
end;

procedure TcxTreeListIndentCellViewInfo.UpdateHotTrackState(const APoint: TPoint);
begin
  FHotTrack := PtInRect(cxRectOffset(DisplayRect, GetCellOrigin), APoint);
  TreeList.InvalidateRect(cxRectOffset(DisplayRect, GetCellOrigin), False);
end;

function TcxTreeListIndentCellViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetHintBounds, P);
end;

function TcxTreeListIndentCellViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxTreeListIndentCellViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

function TcxTreeListIndentCellViewInfo.GetCheckState: TcxCheckBoxState;
begin
  Result := Node.CheckState;
end;

function TcxTreeListIndentCellViewInfo.GetFixed: Boolean;
begin
  Result := (TreeList.Bands.ExpandableBand = nil) or
    (TreeList.Bands.ExpandableBand.FixedKind <> tlbfNone);
end;

function TcxTreeListIndentCellViewInfo.GetHasImage: Boolean;
begin
  Result := (Images <> nil) and ((ImageIndex >= 0) or (OverlayIndex >= 0));
end;

function TcxTreeListIndentCellViewInfo.GetIsExpanded: Boolean;
begin
  Result := Button and Node.Expanded;
end;

procedure TcxTreeListIndentCellViewInfo.SetGlyphRect(const AValue: TRect);
begin
  FGlyphRect := AValue;
  if Button and (Lines <> []) then
    CalculateTreeLines(cxRectCenter(FGlyphRect));
end;

{ TcxTreeListFindPanelViewInfo }

function TcxTreeListFindPanelViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    TcxCustomTreeList(Control).HitTest.SetHitState(echc_HitAtFindPanel, True);
end;

{ TcxTreeListFilterBoxViewInfo }

function TcxTreeListFilterBoxViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    TcxCustomTreeList(Control).HitTest.SetHitState(echc_HitAtFilterBox, True);
end;

{ TcxTreeListNavigatorSiteViewInfo }

function TcxTreeListNavigatorSiteViewInfo.GetHitTest(AHitTest: TcxCustomHitTestController): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    TcxCustomTreeList(OwnerControl).HitTest.SetHitState(tlhc_HitAtNavigator, True);
end;

{ TcxTreeListGroupNodeEditViewInfo }

function TcxTreeListGroupNodeEditViewInfo.GetColumn: TcxTreeListColumn;
begin
  Result := TreeList.OptionsView.GetCategorizedColumn;
end;

function TcxTreeListGroupNodeEditViewInfo.GetFocused: Boolean;
begin
  Result := Node.Focused;
end;

function TcxTreeListGroupNodeEditViewInfo.IsFixed: Boolean;
begin
  Result := TreeList.Bands.VisibleLeftFixedCount > 0;
end;

procedure TcxTreeListGroupNodeEditViewInfo.Scroll(const DX, DY: Integer);
var
  R: TRect;
begin
  if IsFixed or (DX = 0) then Exit;
  R := DisplayRect;
  Inc(R.Left, DX);
  CheckClipping(R, VisibleBounds);
  ViewInfo.Left := ViewInfo.Left + DX;
  VisibleInfoCalculated := False;
end;

{ TcxTreeListPreviewCellViewInfo }

constructor TcxTreeListPreviewCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FPreview := TreeList.Preview;
end;

class function TcxTreeListPreviewCellViewInfo.CustomDrawID: Integer;
begin
  Result := cxtlPreviewCell;
end;

function TcxTreeListPreviewCellViewInfo.ContentOffset: TRect;
begin
  Result := inherited ContentOffset;
  Result.Left := Preview.LeftIndent;
  Result.Right := Preview.RightIndent;
end;

procedure TcxTreeListPreviewCellViewInfo.DoCalculate;
begin
  Include(ViewData.PaintOptions, epoAllowZeroHeight);
  inherited DoCalculate;
end;

function TcxTreeListPreviewCellViewInfo.GetColumn: TcxTreeListColumn;
begin
  Result := Preview.Column;
end;

function TcxTreeListPreviewCellViewInfo.GetButtonTransparency: TcxEditButtonTransparency;
begin
  Result := ebtHideInactive;
end;

function TcxTreeListPreviewCellViewInfo.GetEditViewParams: TcxViewParams;
begin
  Result := TreeList.Styles.GetPreviewParams(Node);
end;

function TcxTreeListPreviewCellViewInfo.GetHotTrack: Boolean;
begin
  Result := False;
end;

function TcxTreeListPreviewCellViewInfo.GetMaxLineCount: Integer;
begin
  Result := Preview.MaxLineCount;
end;

function TcxTreeListPreviewCellViewInfo.IsAutoHeight: Boolean;
begin
  Result := True;
end;

function TcxTreeListPreviewCellViewInfo.IsEditHotTrack(
  const APoint: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxTreeListPreviewCellViewInfo.Scroll(const DX, DY: Integer);
begin
  if TreeList.Bands.VisibleLeftFixedCount = 0 then
    inherited Scroll(DX, 0);
end;

{ TcxTreeListSummary }

constructor TcxTreeListSummary.Create(ATreeList: TcxCustomTreeList);
begin
  inherited Create;
  FTreeList := ATreeList;
  FAbsoluteFooterSummaryItems := TList.Create;
  FAbsoluteGroupFooterSummaryItems := TList.Create;
end;

destructor TcxTreeListSummary.Destroy;
begin
  FreeAndNil(FAbsoluteGroupFooterSummaryItems);
  FreeAndNil(FAbsoluteFooterSummaryItems);
  inherited Destroy;
end;

procedure TcxTreeListSummary.Recalculate;
begin
  TreeList.AddChanges([tcSummary]);
  TreeList.LayoutChanged;
end;

procedure TcxTreeListSummary.AddSummaryItems(AAbsoluteSummaryItems: TList;
  ASummaryItems: TcxTreeListSummaryItems; AIsVisible: Boolean;
  var AVisibleCount, AVisibleRowCount: Integer);
var
  I: Integer;
begin
   for I := 0 to ASummaryItems.Count - 1 do
     ASummaryItems[I].AbsoluteIndex := AAbsoluteSummaryItems.Add(ASummaryItems[I]);
   if AIsVisible then
   begin
     Inc(AVisibleCount, ASummaryItems.VisibleCount);
     AVisibleRowCount := Max(AVisibleRowCount, ASummaryItems.VisibleCount *
       ASummaryItems.Column.Position.LineCount);
   end;
end;

procedure TcxTreeListSummary.Calculate;
var
  I: Integer;
begin
  FAbsoluteFooterSummaryItems.Clear;
  FAbsoluteGroupFooterSummaryItems.Clear;
  FFooterSummaryRowCount := 0;
  FFooterSummaryVisibleCount := 0;
  FGroupFooterSummaryRowCount := 0;
  FGroupFooterSummaryVisibleCount := 0;
  for I := 0 to FTreeList.ColumnCount - 1 do
  begin
    AddSummaryItems(FAbsoluteFooterSummaryItems, FTreeList.Columns[I].Summary.FooterSummaryItems,
      FTreeList.Columns[I].ActuallyVisible and FTreeList.Columns[I].Options.Footer,
      FFooterSummaryVisibleCount, FFooterSummaryRowCount);
    AddSummaryItems(FAbsoluteGroupFooterSummaryItems, FTreeList.Columns[I].Summary.GroupFooterSummaryItems,
      FTreeList.Columns[I].ActuallyVisible and FTreeList.Columns[I].Options.GroupFooter,
      FGroupFooterSummaryVisibleCount, FGroupFooterSummaryRowCount);
  end;
  IsDirty := True;
end;

procedure TcxTreeListSummary.CalculateNode(ADestNode, AValueNode: TcxTreeListNode; AItems: TList);
begin
  ProcessSummaryValues(ADestNode, AValueNode, AItems);
  while ADestNode <> Root do
  begin
    ADestNode := ADestNode.Parent;
    if ADestNode <> Root then
      ProcessSummaryValues(ADestNode, AValueNode, AItems);
  end;
end;

procedure TcxTreeListSummary.CheckChanges;
begin
  if IsDirty then
  begin
    IsDirty := False;
    ProcessNode(Root);
    TreeList.DoAfterSummary;
  end;
end;

function TcxTreeListSummary.DoProcessSummaryValue(ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem;
  var ASummaryValue: Variant; var ASummaryCount: Integer; var AValue: Variant): Boolean;
var
  AArguments: TcxTreeListSummaryEventArguments;
  AOutArguments: TcxTreeListSummaryEventOutArguments;
begin
  Result := Assigned(TreeList.OnSummary);
  if Result then
  begin
    AArguments.Node := ANode;
    AArguments.SummaryItem := ASummaryItem;
    AOutArguments.Value := AValue;
    AOutArguments.SummaryValue := ASummaryValue;
    AOutArguments.CountValue := ASummaryCount;
    AOutArguments.Done := False;
    TreeList.OnSummary(TreeList, AArguments, AOutArguments);
    ASummaryValue := AOutArguments.SummaryValue;
    ASummaryCount := AOutArguments.CountValue;
    AValue := AOutArguments.Value;
    Result := AOutArguments.Done;
  end;
end;

procedure TcxTreeListSummary.FinalizeValues(ANode: TcxTreeListNode; AItems: TList);
var
  ASummaryItem: TcxTreeListSummaryItem;
  I: Integer;
  AVarIsDate: Boolean;
  ASummaryInfos: TcxTreeListSummaryInfos;
begin
  ASummaryInfos := ANode.FSummaryInfo;
  for I := 0 to AItems.Count - 1 do
  begin
    ASummaryItem := AItems[I];
    if VarIsEmpty(ASummaryInfos[I].TempValue) then
      ASummaryInfos[I].Value := Null;
    case ASummaryItem.Kind of
      skMin, skMax, skSum:
        ASummaryInfos[I].Value := ASummaryInfos[I].TempValue;
      skCount:
        ASummaryInfos[I].Value := ASummaryInfos[I].Count;
      skAverage:
        if (ASummaryInfos[I].Count > 0) and not VarIsEmpty(ASummaryInfos[I].TempValue) then
        begin
          AVarIsDate := VarIsDate(ASummaryInfos[I].TempValue);
          ASummaryInfos[I].Value := ASummaryInfos[I].TempValue / ASummaryInfos[I].Count;
          if AVarIsDate then
            VarCast(ASummaryInfos[I].Value, ASummaryInfos[I].TempValue, varDate);
        end;
    end;
  end;
end;

procedure TcxTreeListSummary.InitializeValues(ANode: TcxTreeListNode; AItems: TList);
var
  I: Integer;
  ASummaryItem: TcxTreeListSummaryItem;
begin
  SetLength(ANode.FSummaryInfo, 0);
  SetLength(ANode.FSummaryInfo, AItems.Count);
  for I := 0 to AItems.Count - 1 do
  begin
    ASummaryItem := TcxTreeListSummaryItem(AItems[I]);
    ANode.FSummaryInfo[I].Count := 0;
    if ASummaryItem.Kind in [skSum, skAverage] then
      ANode.FSummaryInfo[I].TempValue := 0
    else
      ANode.FSummaryInfo[I].TempValue := Null;
    ANode.FSummaryInfo[I].Value := Null;
  end;
end;

procedure TcxTreeListSummary.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  Calculate;
  for I := 0 to FAbsoluteFooterSummaryItems.Count - 1 do
    TcxTreeListSummaryItem(FAbsoluteFooterSummaryItems[I]).Notification(AComponent, Operation);
  for I := 0 to FAbsoluteGroupFooterSummaryItems.Count - 1 do
    TcxTreeListSummaryItem(FAbsoluteGroupFooterSummaryItems[I]).Notification(AComponent, Operation);
end;

procedure TcxTreeListSummary.ProcessNode(AParent: TcxTreeListNode);
var
  AList: TList;
  ANode: TcxTreeListNode;
begin
  AList := FAbsoluteGroupFooterSummaryItems;
  if AParent = Root then
    AList := FAbsoluteFooterSummaryItems;
  InitializeValues(AParent, AList);
  ANode := AParent.GetFirstChildVisible;
  while ANode <> nil do
  begin
    CalculateNode(Root, ANode, FAbsoluteFooterSummaryItems);
    if AParent <> Root then
      CalculateNode(AParent, ANode, FAbsoluteGroupFooterSummaryItems);
    if ANode.HasChildren then
      ProcessNode(ANode)
    else
      SetLength(ANode.FSummaryInfo, 0);
    ANode := ANode.GetNextSiblingVisible;
  end;
  FinalizeValues(AParent, AList);
end;

procedure TcxTreeListSummary.ProcessSummaryValue(ANode: TcxTreeListNode; ASummaryItem: TcxTreeListSummaryItem;
  var ASummaryValue: Variant; var ASummaryCount: Integer);
var
  AValue: Variant;
begin
  AValue := ANode.Values[ASummaryItem.CalculatedColumn.ItemIndex];
  if not DoProcessSummaryValue(ANode, ASummaryItem, ASummaryValue, ASummaryCount, AValue) and
    not (TreeList.OptionsData.SummaryNullIgnore and VarIsNull(AValue)) then
  begin
    if VarIsNull(ASummaryValue) then
      ASummaryValue := AValue
    else
      if not VarIsNull(AValue) then
        case ASummaryItem.Kind of
          skSum, skAverage:
            ASummaryValue := ASummaryValue + AValue;
          skMin:
            if AValue < ASummaryValue then
              ASummaryValue := AValue;
          skMax:
            if AValue > ASummaryValue then
              ASummaryValue := AValue;
        end;
    Inc(ASummaryCount);
  end;
end;

procedure TcxTreeListSummary.ProcessSummaryValues(ADestNode, AValueNode: TcxTreeListNode; AItems: TList);
var
  I: Integer;
  ASummaryItem: TcxTreeListSummaryItem;
  AAbsoluteItemIndex: Integer;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    ASummaryItem := AItems[I];
    if(ASummaryItem.AllNodes or (AValueNode.Parent = ADestNode)) and
      (ASummaryItem.Kind <> skNone) then
    begin
      AAbsoluteItemIndex := ASummaryItem.AbsoluteIndex;
      ProcessSummaryValue(AValueNode, ASummaryItem, ADestNode.FSummaryInfo[AAbsoluteItemIndex].TempValue,
        ADestNode.FSummaryInfo[AAbsoluteItemIndex].Count);
    end;
  end;
end;

function TcxTreeListSummary.GetFooterSummaryCount: Integer;
begin
  Result := FAbsoluteFooterSummaryItems.Count;
end;

function TcxTreeListSummary.GetFooterSummaryItem(
  AIndex: Integer): TcxTreeListSummaryItem;
begin
  Result := TcxTreeListSummaryItem(FAbsoluteFooterSummaryItems[AIndex]);
end;

function TcxTreeListSummary.GetFooterSummaryText(
  ASummaryItem: TcxTreeListSummaryItem): string;
begin
  Result := GetGroupFooterSummaryText(ASummaryItem, Root);
end;

function TcxTreeListSummary.GetFooterSummaryValue(
  ASummaryItem: TcxTreeListSummaryItem): Variant;
begin
  Result := GetGroupFooterSummaryValue(ASummaryItem, Root);
end;

function TcxTreeListSummary.GetGroupFooterSummaryCount: Integer;
begin
  Result := FAbsoluteGroupFooterSummaryItems.Count;
end;

function TcxTreeListSummary.GetGroupFooterSummaryItem(
  AIndex: Integer): TcxTreeListSummaryItem;
begin
  Result := TcxTreeListSummaryItem(FAbsoluteGroupFooterSummaryItems[AIndex]);
end;

function TcxTreeListSummary.GetGroupFooterSummaryText(
  ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode): string;
begin
  Result := ASummaryItem.FormatValue(
    GetGroupFooterSummaryValue(ASummaryItem, ANode), True);
end;

function TcxTreeListSummary.GetGroupFooterSummaryValue(
  ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode): Variant;
begin
  Result := ANode.FooterSummaryValues[ASummaryItem.AbsoluteIndex];
end;

function TcxTreeListSummary.GetRoot: TcxTreeListNode;
begin
  Result := TreeList.Root;
end;

procedure TcxTreeListSummary.SetFooterSummaryValue(
  ASummaryItem: TcxTreeListSummaryItem; const Value: Variant);
begin
  GroupFooterSummaryValues[ASummaryItem, Root] := Value;
end;

procedure TcxTreeListSummary.SetGroupFooterSummaryValue(
  ASummaryItem: TcxTreeListSummaryItem; ANode: TcxTreeListNode;
  const Value: Variant);
begin
  ANode.FooterSummaryValues[ASummaryItem.AbsoluteIndex] := Value;
end;


{ TcxTreeListNodeDragInfoImage }

constructor TcxTreeListNodeDragInfoImage.Create(ATreeList: TcxCustomTreeList);
begin
  inherited Create;
  FTreeList := ATreeList;
  AlphaBlend := True;
  AlphaBlendValue := MaxByte;
  Image.SetSize(TreeList.ScaleFactor.Apply(15), TreeList.ScaleFactor.Apply(10));
  FImages := TcxImageList.CreateSize(Image.Width, Image.Height);
  LoadResourceImages;
  FImageIndex := -1;
end;

destructor TcxTreeListNodeDragInfoImage.Destroy;
begin
  FreeAndNil(FImages);
  inherited Destroy;
end;

function TcxTreeListNodeDragInfoImage.MoveTo(APosition: TPoint): Boolean;
var
  AWindowPosition: TPoint;
begin
  HandleNeeded;
  Result := (FImages.Count > 0) and IsImageIndexValid and
    (not cxPointIsEqual(APosition, FPrevPosition) or not Visible or (FPrevImageIndex <> ImageIndex));
  if Result then
  begin
    Hide;
    FPrevPosition := APosition;
    FPrevImageIndex := FImageIndex;
    AWindowPosition := APosition;
    AWindowPosition.Y := APosition.Y - FImages.Height div 2;
    SetBounds(AWindowPosition.X, AWindowPosition.Y, FImages.Width, FImages.Height);
    Show;
  end;
end;

function TcxTreeListNodeDragInfoImage.GetState: TcxNodeDragInfoState;
begin
  Result := TcxNodeDragInfoState(FImageIndex + 1);
end;

function TcxTreeListNodeDragInfoImage.IsImageIndexValid: Boolean;
begin
  Result := IsImageAssigned(FImages, FImageIndex);
end;

procedure TcxTreeListNodeDragInfoImage.LoadResourceImages;

  procedure AddImage(AResourceImageName: string);
  var
    AImage: TdxSmartImage;
  begin
    AImage := TdxSmartImage.Create;
    try
      AImage.LoadFromResource(HInstance, AResourceImageName, 'SVG');
      if TreeList.ViewInfo.IsRightToLeftConverted then
        AImage.Flip(True, False);
      FImages.Add(AImage);
    finally
      AImage.Free;
    end;
  end;

begin
  AddImage('TLINSERTNODEBEFORE');
  AddImage('TLINSERTNODEAFTER');
  AddImage('TLADDCHILDNODE');
end;

procedure TcxTreeListNodeDragInfoImage.SetImageIndex(AValue: Integer);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    UpdateImage;
  end;
end;

procedure TcxTreeListNodeDragInfoImage.SetState(const Value: TcxNodeDragInfoState);
begin
  ImageIndex := Ord(Value) - 1;
end;

procedure TcxTreeListNodeDragInfoImage.UpdateImage;
begin
  Image.Clear;
  if IsImageIndexValid then
    FImages.Draw(Image.Canvas, Image.ClientRect, FImageIndex);
  Update;
end;

{ TcxTreeListController }

destructor TcxTreeListController.Destroy;
begin
  FreeAndNil(DragNodesList);
  FreeAndNil(FBandsCustomizationPopup);
  FreeAndNil(FColumnsCustomizationPopup);
  inherited Destroy;
end;

procedure TcxTreeListController.AfterPaint;
begin
  inherited AfterPaint;
  if (TreeList.DragAndDropState = ddsInProcess) and
    (TreeList.DragAndDropObject is TcxTreeListSizingDragAndDropObject) then
      TcxTreeListSizingDragAndDropObject(TreeList.DragAndDropObject).DirtyChanged;
end;

procedure TcxTreeListController.BeforePaint;
begin
  if TreeList.ViewInfo.IsDirty then
   TreeList.ViewInfo.Validate;
  if TreeList.Controller.SelectionLockCount = 0 then
    inherited BeforePaint;
end;

procedure TcxTreeListController.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  TreeList.Update;
end;

procedure TcxTreeListController.BeginUpdateSelection;
begin
  if FSelectionLockCount = 0 then
  begin
    HScrollPos := TreeList.ViewInfo.HScrollPos;
    VScrollPos := TreeList.ViewInfo.VScrollPos;
  end;
  Inc(FSelectionLockCount);
end;

function TcxTreeListController.CheckAutoScrolling(
  const APoint: TPoint): Boolean;
var
  ASide: TcxBorder;
begin
  Result := False;
  for ASide := bLeft to bBottom do
    Result := Result or ScrollControllers[ASide].Check(APoint);
end;

function TcxTreeListController.GetStatusHint(const APoint: TPoint): string;
begin
  if (HitTest.HitColumn <> nil) and HitTest.HitAtColumnHeader and
    (HitTest.HitColumn.StatusHint <> '') then
    Result := HitTest.HitColumn.StatusHint
  else
    Result := TreeList.Hint;
end;

procedure TcxTreeListController.AddNodeToSelection(ANode: TcxTreeListNode);
begin
  if CanSelectNode(ANode) then
    Selection.Add(ANode);
  TreeList.AddChanges([tcSelection]);
end;

procedure TcxTreeListController.BeforeShowEdit;
begin
  if not EditingController.IsEditing then
    MakeFocusedItemVisible;
end;

function TcxTreeListController.CanHandleDeleteRecordKeys: Boolean;
begin
  Result := TreeList.OptionsData.Deleting;
end;

procedure TcxTreeListController.CheckFocusedNodeItem;
begin
  if FocusedNode = nil then
    FocusedNode := FindNearestFocusableNode(0);
  if (FocusedItem = nil) and OptionsSelection.CellSelect then
    FocusedItem := FindNearestFocusableColumn(0);
end;

procedure TcxTreeListController.ChangeFocusedNode(ANode: TcxTreeListNode);
begin
  if (ANode <> nil) and ANode.IsHidden or not TreeList.DoCanFocusNode(ANode) then Exit;
  if ANode <> FFocusedNode then
    FPrevFocusedNode := FFocusedNode;
  if ANode <> FFocusedNode then
  begin
    EditingController.HideEdit(True);
    if not DataController.IsDataMode then
    begin
      if (FFocusedNode <> nil) and not DataController.IsValueChanged and
        (FFocusedNode = TreeList.DataController.EditingNode) then
        DataController.Cancel
      else
        DataController.Post(True);
    end;
    TreeList.AddChanges([tcFocusedNode]);
    if not OptionsSelection.MultiSelect then
      TreeList.AddChanges([tcSelection]);
    if FocusedItem = nil then
    begin
      FFocusedNode := ANode;
      FocusedItem := FindNearestFocusableColumn(0);
    end;
    FFocusedNode := ANode;
  end;
  if FPrevFocusedNode <> FFocusedNode then
    TreeList.DataController.SyncFocused(FFocusedNode);
  if IncSearchNode <> FFocusedNode then
    CancelIncSearching;
end;

function TcxTreeListController.GetFocusedRecordIndex: TdxNativeInt;
begin
  Result := TdxNativeInt(FocusedNode);
end;

procedure TcxTreeListController.SetFocusedNode(ANode: TcxTreeListNode);
begin
  try
    if ANode = nil then
      FocusedNodeIndex := -1;
    ChangeFocusedNode(ANode);
  finally
    TreeList.ViewInfo.UpdateSelection;
  end;
end;

procedure TcxTreeListController.SetFocusedNodeItem(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
begin
  SetFocusedRecordItem(TdxNativeInt(ANode), AColumn);
end;

procedure TcxTreeListController.SetFocusedRecordIndex(Value: TdxNativeInt);
begin
  if TcxTreeListNode(Value) = FocusedNode then Exit;
  if Value = 0 then
    FocusedNode := nil
  else
    FocusedNode := TcxTreeListNode(Value);
end;

procedure TcxTreeListController.BeforeStartDrag;
begin
  inherited BeforeStartDrag;
  FPrevDragCursor := TreeList.DragCursor;
  DragCursorWasChanged := False;
  FreeAndNil(DragNodesList);
  DragNodesList := TreeList.GetSelectionsEx;
  if HitTest.CanStartDrag then
    TreeList.FDragNode := HitTest.HitNode
  else
    TreeList.FDragNode := nil;
end;

function TcxTreeListController.CanDrop: Boolean;
begin
  Result := HitTest.HitAtNode and not HitTest.HitAtGroupFooter and
     not HitTest.HitAtGroupFooterItem;
end;

function TcxTreeListController.CreateAutoScrollObject(Kind: TScrollBarKind;
  const ARect: TRect; ACode: TScrollCode): TcxAutoScrollingEditingControlObject;
begin
  Result := TcxControllerAutoScrollingObject.Create(Self);
  Result.SetParams(ARect, Kind, ACode, 1);
end;

procedure TcxTreeListController.CheckButtonTimer;
var
  NeedTimer: Boolean;
begin
  if (ExpandingNode <> HitTest.HitNode) or not HitTest.HitAtButton then
    ResetButtonTimer;
  if ExpandingNode = nil then
  begin
    NeedTimer := HitTest.HitAtButton and
      (OptionsBehavior.DragExpand and not HitTest.HitNode.Expanded) or
        (OptionsBehavior.DragCollapse and HitTest.HitNode.Expanded);
    if NeedTimer then
    begin
      ExpandTimer.Interval := OptionsBehavior.WaitForExpandNodeTime;
      ExpandTimer.Enabled := True;
      ExpandTimer.OnTimer := OnExpandTimer;
      ExpandingNode := HitTest.HitNode;
    end;
  end;
end;

procedure TcxTreeListController.DragDrop(Source: TObject; X, Y: Integer);
var
  AAttachMode: TcxTreeListNodeAttachMode;
  ADestNode: TcxTreeListNode;
begin
  HitTest.HitPoint := Point(X, Y);
  TreeList.BeginUpdate;
  try
    if Source is TBaseDragControlObject then
      Source := TBaseDragControlObject(Source).Control;
    if (Source = TreeList) and CanDrop then
    begin
      ADestNode := HitTest.HitNode;
      AAttachMode := tlamAddChild;
      case GetDropMode(X, Y) of
        ndiInsertAfter:
          if ADestNode.getNextSibling = nil then
            ADestNode := ADestNode.Parent
          else
          begin
            ADestNode := ADestNode.getNextSibling;
            AAttachMode := tlamInsert;
          end;
        ndiInsertBefore:
          AAttachMode := tlamInsert;
      else
        AAttachMode := tlamAddChild;
      end;
      DropTo(ADestNode, AAttachMode);
    end;
    if FocusedNode <> nil then
      FocusedNode.MakeVisible;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListController.DragEnter;
var
  R: TRect;
begin
  if DragDropInProcess then Exit;
  if TreeList.FDragNode = nil then
    TreeList.FDragNode := TreeList.FocusedNode;
  R := TreeList.ViewInfo.ContentBounds;
  ScrollControllers[bTop] := CreateAutoScrollObject(sbVertical,
    cxRectSetHeight(R, cxtlScrollDelta * 2), scLineUp);
  ScrollControllers[bBottom] := CreateAutoScrollObject(sbVertical,
    cxRectSetTop(R, R.Bottom - cxtlScrollDelta * 2), scLineDown);
  if (TreeList.Bands.ExpandableBand <> nil) and (TreeList.Bands.ExpandableBand.FixedKind = tlbfNone) then
  begin
    R.Left := TreeList.ViewInfo.ContentParts[tlbfNone].Left;
    R.Bottom := TreeList.ViewInfo.ContentParts[tlbfNone].Bottom
  end;
  ScrollControllers[bLeft] := CreateAutoScrollObject(sbHorizontal,
    cxRectSetWidth(R, cxtlScrollDelta * 2), GetRealScrollCode(scLineUp));
  ScrollControllers[bRight] := CreateAutoScrollObject(sbHorizontal,
    cxRectSetWidth(R, R.Right - cxtlScrollDelta * 2, cxtlScrollDelta * 2), GetRealScrollCode(scLineDown));
  ExpandTimer := TTimer.Create(TreeList);
  if TreeList.OptionsView.DropNodeIndicator then
    FDragNodeInfoImage := TcxTreeListNodeDragInfoImage.Create(TreeList);
end;

procedure TcxTreeListController.DragLeave;
var
  ASide: TcxBorder;
begin
  FreeAndNil(FDragNodeInfoImage);
  for ASide := bLeft to bBottom do
    FreeAndNil(ScrollControllers[ASide]);
  FreeAndNil(ExpandTimer);
  DragDropInProcess := False;
end;

procedure TcxTreeListController.DragMove(
  Source: TObject; const P: TPoint; var Accepted: Boolean);

  procedure ProcessDragMove;
  begin
    HitTest.Recalculate(P);
    if HitTest.HitAtButton then
      CheckButtonTimer
    else
      ExpandingNode := nil;
    CheckAutoScrolling(P);
  end;

begin
  ProcessDragMove;
  HitTest.ReCalculate(P);
  if CanDrop and TreeList.OptionsBehavior.DragFocusing and not HitTest.HitNode.Focused then
    HitTest.HitNode.Focused := True;
  if FDragNodeInfoImage <> nil then
    if CanDrop then
    begin
      FDragNodeInfoImage.State := GetDropMode(P.X, P.Y);
      FDragNodeInfoImage.MoveTo(TreeList.GetDragInfoImagePosition(P));
    end
    else
      FDragNodeInfoImage.Hide;
end;

procedure TcxTreeListController.DragOver(
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  case State of
    dsDragEnter:
      DragEnter;
    dsDragLeave:
      DragLeave;
    dsDragMove:
      DragMove(Source, Point(X, Y), Accept);
  end;
  inherited DragOver(Source, X, Y, State, Accept);
end;

procedure TcxTreeListController.DropTo(
  ANode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode);
begin
  if ANode <> nil then
  begin
    if TreeList.OptionsBehavior.DragFocusing and
      (DragNodesList.IndexOf(TreeList.DragNode) = cxInvalidIndex) then
      DragNodesList.Add(TreeList.DragNode);
    try
      TreeList.DoInternalMoveTo(ANode, AMode, DragNodesList,
        TreeList.Controller.IsDragCopy);
    finally
      FreeAndNil(DragNodesList);
      TreeList.Controller.CancelSelection;
    end;
  end;
end;

procedure TcxTreeListController.EndDrag(Target: TObject; X, Y: Integer);
begin
  HitTestController.ReCalculate(Point(X, Y));
  if TreeList.OptionsBehavior.AlwaysShowEditor or TreeList.OptionsBehavior.ImmediateEditor and
    ((FocusedItem <> nil) and (FocusedNode <> nil)) then  EditingController.ShowEdit();
  FreeAndNil(DragNodesList);
  if DragCursorWasChanged then
    TreeList.DragCursor := FPrevDragCursor;
  DragCursorWasChanged := False;
end;

function TcxTreeListController.GetRealScrollCode(ACode: TScrollCode): TScrollCode;
begin
  Result := ACode;
  if TreeList.UseRightToLeftAlignment then
    case Result of
      scLineUp: Result := scLineDown;
      scLineDown: Result := scLineUp;
    end;
end;

procedure TcxTreeListController.OnExpandTimer(Sender: TObject);
begin
  ResetButtonTimer;
  if HitTest.HitAtButton and (HitTest.HitNode = ExpandingNode) then
    ExpandingNode.Expanded := not ExpandingNode.Expanded
  else
    ExpandingNode := nil;
end;

procedure TcxTreeListController.ResetButtonTimer;
begin
  if ExpandTimer <> nil then
    ExpandTimer.Enabled := False;
end;

procedure TcxTreeListController.DoNextNode(AForward: Boolean);
begin
  if (FocusedNode <> nil) and (FocusedNode.GetNextVisibleEx(AForward) <> nil) then
    FocusedNode := FocusedNode.GetNextVisibleEx(AForward);
end;

procedure TcxTreeListController.DoNextPage(AForward: Boolean; Shift: TShiftState);
begin
  DoShowNextPageEx(AForward, True, Shift);
  TreeList.Controller.CheckEdit;
end;

procedure TcxTreeListController.DoShowNextPageEx(
  AGoForward, ASetCursor: Boolean; AShift: TShiftState);

  function GetNext(ANode: TcxTreeListNode; IsNext: Boolean): TcxTreeListNode;
  begin
    Result := ANode;
    if (ANode <> nil) and (TreeList.VisibleCount = 1) then
      Result := ANode.GetNextVisibleEx(not IsNext);
  end;

begin
  if TreeList.AbsoluteVisibleCount = 0 then Exit;
  if TreeList.ViewInfo.IsRecordPixelScrollMode then
    if AGoForward then
      if ASetCursor then
      begin
        TreeList.TopVisibleNode := GetNext(FocusedNode, True);
        TreeList.LastVisibleNode := TreeList.LastPartVisibleNode;
        TreeList.SetFocusedNode(TreeList.LastPartVisibleNode, AShift);
      end
      else
        TreeList.TopVisibleNode := GetNext(TreeList.LastPartVisibleNode, True)
    else
      if ASetCursor then
      begin
        TreeList.LastVisibleNode := GetNext(FocusedNode, False);
        TreeList.ViewInfo.FPixelScrollNodeOffset := 0;
        TreeList.SetFocusedNode(TreeList.TopVisibleNode, AShift);
      end
      else
        TreeList.LastVisibleNode := GetNext(TreeList.TopVisibleNode, False)
  else
    if AGoForward then
    begin
      if (TreeList.LastVisibleNode <> nil) and TreeList.LastVisibleNode.IsLastVisible and
        TreeList.LastVisibleNode.Focused and ASetCursor then Exit;
      if ASetCursor then
      begin
        TreeList.TopVisibleNode := GetNext(FocusedNode, True);
        TreeList.SetFocusedNode(TreeList.LastVisibleNode, AShift);
      end
      else
        TreeList.TopVisibleNode := GetNext(TreeList.LastVisibleNode, True);
    end
    else
    begin
      if (TreeList.TopVisibleNode <> nil) and TreeList.TopVisibleNode.IsFirstVisible and
        TreeList.TopVisibleNode.Focused and ASetCursor then Exit;
      if ASetCursor then
      begin
        TreeList.LastVisibleNode := GetNext(FocusedNode, False);
        TreeList.SetFocusedNode(TreeList.TopVisibleNode, AShift);
      end
      else
        TreeList.LastVisibleNode := GetNext(TreeList.TopVisibleNode, False);
    end;
  MakeFocusedItemVisible;
  TreeList.LayoutChanged;
end;

procedure TcxTreeListController.FocusedRecordChanged(
  APrevFocusedRecordIndex, AFocusedRecordIndex: Integer);
begin
  if AFocusedRecordIndex <> FocusedRecordIndex then
    CancelSelection(True);
end;

procedure TcxTreeListController.FocusedItemChanged(
  APrevFocusedItem: TcxCustomInplaceEditContainer);
begin
  inherited FocusedItemChanged(APrevFocusedItem);
  TreeList.ViewInfo.UpdateSelection;
  TreeList.DoFocusedItemChanged(APrevFocusedItem, FocusedItem);
end;

function TcxTreeListController.IsKeyForController(
  AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := (AKey = VK_ADD) or (AKey = VK_SUBTRACT) or (AKey = VK_MULTIPLY) or
    (AKey = VK_HOME) or (AKey = VK_END);
  Result := inherited IsKeyForController(AKey, AShift) or
    ((IsEditing and (ssCtrl in AShift) and Result) or (not IsEditing and Result));
  if ((AKey = VK_LEFT) or (AKey = VK_RIGHT)) and (IsEditing and not OptionsBehavior.AlwaysShowEditor) then
    Result := False;
end;

function TcxTreeListController.IsNodeKeyHandle(ANode: TcxTreeListNode;
  var AKey: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  case AKey of
    VK_SUBTRACT, VK_ADD:
      if (IncSearchText = '') and (ANode <> nil) and ANode.HasChildren then
        ANode.Expanded := AKey = VK_ADD
      else
        Result := False;
    VK_MULTIPLY:
      if (ANode <> nil) and ANode.HasChildren then
        ANode.Expand(AKey = VK_MULTIPLY);
    VK_HOME:
      begin
        if ssCtrl in Shift then
          TreeList.Select(TreeList.TopNode, Shift)
        else
          if TreeList.VisibleColumnCount > 0 then
            TreeList.VisibleColumns[0].Focused := True
          else
            Result := False;
      end;
    VK_SPACE:
      if (ANode <> nil) and ANode.HasCheckbox and ANode.Enabled then
        ANode.CheckClick
      else
        Result := False;
    VK_END:
      begin
        if ssCtrl in Shift then
          TreeList.Select(TreeList.LastNode, Shift)
        else
          if TreeList.VisibleColumnCount > 0 then
            TreeList.VisibleColumns[TreeList.VisibleColumnCount - 1].Focused := True
          else
            Result := False;
      end;
    VK_DELETE:
      if CanHandleDeleteRecordKeys then
      begin
        Result := (ssCtrl in Shift) and
          CanDeleteSelection and DeleteConfirmation;
        if Result then
          TreeList.DeleteSelection;
      end;
    VK_INSERT:
      begin
        if ((FocusedNode = nil) and (TreeList.AbsoluteCount > 0)) or (not CanInsertNode) then
          Result := False
        else
          TreeList.InsertNode(FocusedNode, ssCtrl in Shift);
          if OptionsBehavior.FocusFirstCellOnNewRecord  then
          begin
            if ANode.IsGroupNode then
              SetFocusedNodeItem(FocusedNode, TreeList.OptionsView.GetCategorizedColumn)
            else
              SetFocusedNodeItem(FocusedNode, FindNearestFocusableColumn(0));
            TcxTreeListCellNavigator(Navigator).SaveCurrentNavigationColumn;
          end;
      end;
  else
    Result := False
  end;
  if Result and (FocusedNode <> nil) then
  begin
    FocusedNode.MakeVisible;
    MakeFocusedItemVisible;
  end;
end;

function TcxTreeListController.CanDrag(X, Y: Integer): Boolean;
begin
  Result := not HitTest.HitAtNavigator;
  if not Result then
    Exit;
  HitTest.HitPoint := Point(X, Y);
  Result := not HitTest.CanSizing and not TreeList.IsDesigning and not IsEditing and
    (HitTest.HitAtNode and HitTest.HitNode.Selected and
    not HitTest.HitAtFooterArea);
  if (HitTest.HitAtIndent and not HitTest.HitAtImages) or not HitTest.CanStartDrag then
    Result := False;
  if Result then
    TreeList.FDragNode := HitTest.HitNode;
end;

function TcxTreeListController.DoHeaderMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if CheckCustomizationPopup then
  begin
    HitTest.RecalculateOnMouseEvent(X, Y, Shift);
    Result := True;
  end
  else
  begin
    if HitTest.HitAtHeader and not HitTest.CanSizing and
      not (ssDouble in Shift) and (Button = mbLeft) then
      FClickedObject := TcxTreeListHeaderCellViewInfo(HitTest.HitTestItem).Item;
    if HitTest.HitAtHeader and not HitTest.CanSizing and
     (ClickedObject = TcxTreeListHeaderCellViewInfo(HitTest.HitTestItem).Item) then
    begin
      TcxTreeListCustomHeaderCellViewInfo(HitTest.HitTestItem).Pressed := True;
      Result := True;
    end
    else
      if HitTest.HitAtColumnHeaderFilterButton then
      begin
        TcxTreeListHeaderCellViewInfo(HitTest.HitTestItem).UpdateFilterButtonStateOnMouseDown;
        Result := True;
      end;
  end;
end;

function TcxTreeListController.DoNodeMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if CanDrag(X, Y) and HitTest.HitAtNode and IsEditing then
    TreeList.DragAndDropState := ddsNone;
  if not (ssDouble in Shift) and (Button <> mbMiddle) and HitTest.HitAtNode then
    CheckNodeContentClick(Shift);
  if HitTest.HitAtCheckButton and (Button = mbLeft) and not SkipButtonDown then
    HitTest.HitNode.CheckClick
  else
    if HitTest.HitAtButton and (Button = mbLeft) then
    begin
      if (HitTest.HitNode.Expanded and TreeList.InternalCollapseNode(HitTest.HitNode, False)) or
       (not HitTest.HitNode.Expanded and TreeList.InternalExpandNode(HitTest.HitNode, False)) then
         Exit;
      HitTest.HitNode.Expanded := not HitTest.HitNode.Expanded;
      Result := True;
    end;
  HitTest.ReCalculate;
end;

procedure TcxTreeListController.DoMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIsHotTrack: Boolean;
  APrevFocusedColumn: TcxTreeListColumn;
  APrevFocusedNode: TcxTreeListNode;
begin
  IsDblClick := ssDouble in Shift;
  if not IsDblClick then
    FSkipButtonDown := False;
  APrevFocusedNode := TreeList.FocusedNode;
  APrevFocusedColumn := TreeList.FocusedColumn;
  inherited MouseDown(Button, Shift, X, Y);
  if HitTest.HitAtNavigator then Exit;
  EditingController.StopEditShowingTimer;
//  EditingController.HideEdit(True); // lcm dont close (AlwayShowEditor)
  HitTest.RecalculateOnMouseEvent(X, Y, Shift);
  HitTest.CheckSelection(Shift);
  CanClearSelection := [ssCtrl, ssShift] * Shift = [];
  AIsHotTrack := HitTest.HitAtColumn and not HitTest.HitAtNodePreview and
    HitTest.IsItemEditCell and TcxTreeListEditCellViewInfo(HitTest.HitTestItem).IsEditHotTrack(Point(X, Y));
  if not DoHeaderMouseDown(Button, Shift, X, Y) and not HitTest.CanSizing then
    DoNodeMouseDown(Button, Shift, X, Y);
  WasFocusedBeforeClick := HitTest.HitAtColumn and not HitTest.HitAtNodePreview and
    HitTest.HitAtNode and HitTest.HitNode.Focused and (HitTest.HitNode = APrevFocusedNode) and
    HitTest.HitColumn.Focused and (HitTest.HitColumn = APrevFocusedColumn);
//  EditingController.StopEditShowingTimer;
  if AIsHotTrack then
    ShowEdit(Shift, X, Y);
end;

procedure TcxTreeListController.DoMouseMove(
  Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
  UpdateHotTrackNode(X, Y, Shift);
end;

procedure TcxTreeListController.DoMouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if PressedHeader <> nil then
    PressedHeader.Pressed := False;
  HitTest.RecalculateOnMouseEvent(X, Y, Shift);
  DragItem := nil;
  HotTrackController.SetHotElement(nil, cxNullPoint);
  UpdateHotTrackNode(X, Y, Shift);
  if Button = mbLeft then
    CheckHeaderClick(Shift);
  ClickedObject := nil;
  (Navigator as TcxTreeListCellNavigator).SaveCurrentNavigationColumn;
  HitTest.RecalculateOnMouseEvent(X, Y, Shift);
  if not IsEditing and HitTest.HitAtNode and (HitTest.HitNode = TreeList.FocusedNode) and (Button = mbLeft) then
  begin
    if ([ssCtrl, ssShift, ssLeft]* Shift = []) and CanClearSelection and (Selection.Count <> 1) then
      CancelSelection();
    if (OptionsBehavior.ImmediateEditor and HitTest.HitAtColumn and HitTest.HitAtNode) or OptionsBehavior.AlwaysShowEditor then
      ShowEdit(Shift, X, Y)
    else
      if WasFocusedBeforeClick and not IsDblClick then
        EditingController.StartEditShowingTimer(HitTest.HitColumn);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxTreeListController.CheckDeletedNode(ANode: TcxTreeListNode);
begin
  if ANode = HotTrackNode then
    SetHotTrackNode(nil, []);
  if DragNodesList <> nil then
    DragNodesList.Remove(ANode);
  if ANode = FPrevFocusedNode then
    FPrevFocusedNode := nil;
  if ANode = DataController.FBookmarkNode then
    DataController.ClearBookmark;
  if ANode = IncSearchNode then
    IncSearchNode := nil;
  if ANode = FSelectionAnchor then
    FSelectionAnchor := nil;
  if (ANode = FFocusedNode) and (FFocusedNode <> nil) then
  begin
    FocusedNodeIndex := FFocusedNode.VisibleIndex;
    FFocusedNode := nil;
    TreeList.AddChanges(SelectionChanges);
  end;
end;

procedure TcxTreeListController.CheckEdit;
begin
  if FSelectionLockCount = 0 then
    inherited CheckEdit;
end;

procedure TcxTreeListController.CheckFocusedNode;
var
  I626F7F48B8D4786965000AA0BF134CD: TcxTreeListNode;
begin
  if (FocusedNode = nil) and (FocusedNodeIndex >= 0) then
  begin
    I626F7F48B8D4786965000AA0BF134CD := FindNearestFocusableNode(FocusedNodeIndex);
    if I626F7F48B8D4786965000AA0BF134CD <> nil then
      I626F7F48B8D4786965000AA0BF134CD.Focused := True;
  end;
end;

procedure TcxTreeListController.CheckHeaderClick(AShift: TShiftState);
const
  ASortOrder: array[Boolean, TcxDataSortOrder] of TcxDataSortOrder =
    ((soAscending, soDescending, soAscending), (soNone, soNone, soNone));
begin
  if not HitTest.HitAtColumnHeader or HitTest.CanSizing or
    TreeList.IsDesigning or (ClickedObject <> HitTest.HitColumn) or
    not OptionsBehavior.Sorting then Exit;
  TreeList.SetGlassCursor;
  try
    HitTest.HitColumn.ChangeSortOrder(ASortOrder[ssCtrl in AShift,
      HitTest.HitColumn.SortOrder], AShift);
  finally
    TreeList.RestoreCursor;
  end;
end;

procedure TcxTreeListController.CheckNodeContentClick(Shift: TShiftState);
begin
  if HitAtNodeContent then
  begin
    BeginUpdateSelection;
    TreeList.BeginUpdate;
    try
      Select(HitTest.HitNode, Shift);
      if HitTest.HitAtColumn then
        FocusedItem := FindNearestFocusableColumn(HitTest.HitColumn.VisibleIndex);
      if FocusedItem = nil then
        FocusedItem := FindNearestFocusableColumn(0);
    finally
      TreeList.CancelUpdate;
      EndUpdateSelection;
    end;
  end
  else
    if HitTest.HitAtBackground then
      CancelSelection;
end;

function TcxTreeListController.CanDeleteSelection: Boolean;
begin
  Result := TreeList.OptionsData.Deleting and not TreeList.IsEditing and
    (TreeList.FocusedNode <> nil) or (TreeList.SelectionCount > 0);
end;

function TcxTreeListController.CanInsertNode: Boolean;
begin
  Result := TreeList.OptionsData.Inserting;
  if not Result then Exit;
  DataController.CheckBrowseMode;
  if (DataController.EditingNode <> nil) or DataController.IsValueChanged then
    DataController.Post;
  Result := (DataController.EditingNode = nil) or
    (DataController.EditingNode.Inserting and DataController.IsValueChanged);
end;

function TcxTreeListController.CanSelectNode(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := (Selection.IndexOf(ANode) < 0) and TreeList.DoCanSelectNode(ANode);
end;

function TcxTreeListController.CheckCustomizationPopup: Boolean;
begin
  Result := HitTest.HitAtBandCustomizing or HitTest.HitAtColumnCustomizing;
  if Result and
    TcxTreeListIndicatorCellViewInfo(HitTest.HitTestItem).IsQuickCustomizationEnabled and
    not (ColumnsCustomizationPopup.JustClosed or BandsCustomizationPopup.JustClosed) then
    TcxTreeListIndicatorCellViewInfo(HitTest.HitTestItem).ShowPopup;
end;

procedure TcxTreeListController.DblClick;
begin
  HitTest.Recalculate;
  if IsEditing and (FocusedNode <> nil) and FocusedNode.HasChildren and not GetAlwaysShowEditor then
  begin
    if not HitTest.HitAtNode or OptionsBehavior.ExpandOnDblClick then
      EditingController.HideEdit(True);
  end;
  EditingController.StopEditShowingTimer;
  inherited DblClick;
  HitTest.Recalculate;
  if HitTest.HitAtNode and not (HitTest.HitAtButton or HitTest.HitAtFooterArea or
    HitTest.HitAtCheckButton or IsEditing) and OptionsBehavior.ExpandOnDblClick then
  begin
    HitTest.HitNode.Expanded := not HitTest.HitNode.Expanded;
    SkipButtonDown := True;
  end;
  if HitTest.HitAtSizingHorz then
  begin
    if HitTest.HitAtBandHeader then
      HitTest.HitBand.ApplyBestFit
    else
      HitTest.HitColumn.ApplyBestFit;
  end;
  ClickedObject := nil;
end;

function TcxTreeListController.FindNearestFocusableNode(
  AVisibleIndex: Integer): TcxTreeListNode;
begin
  Result := nil;
  if TreeList.AbsoluteVisibleCount <= 0 then Exit;
  AVisibleIndex := Max(0, Min(TreeList.AbsoluteVisibleCount - 1, AVisibleIndex));
  Result := TreeList.AbsoluteVisibleItems[AVisibleIndex];
  while (Result <> nil) and not TreeList.DoCanFocusNode(Result) do
    Result := Result.GetNextVisible;
  if Result <> nil then Exit;
  Result := TreeList.AbsoluteVisibleItems[AVisibleIndex];
  while (Result <> nil) and not TreeList.DoCanFocusNode(Result) do
    Result := Result.GetPrevVisible;
end;

function TcxTreeListController.FindNearestFocusableColumn(
  AVisibleIndex: Integer): TcxTreeListColumn;
var
  I: Integer;
begin
  Result := nil;
  AVisibleIndex := Max(0, Min(AVisibleIndex, TreeList.VisibleColumnCount - 1));
  if AVisibleIndex < TreeList.VisibleColumnCount then
  begin
    for I := AVisibleIndex to TreeList.VisibleColumnCount - 1 do
      if TreeList.VisibleColumns[I].Options.Focusing then
      begin
        Result := TreeList.VisibleColumns[I];
        Exit;
      end;
    for I := AVisibleIndex downto 0 do
      if TreeList.VisibleColumns[I].Options.Focusing then
      begin
        Result := TreeList.VisibleColumns[I];
        Break;
      end;
  end;
end;

procedure TcxTreeListController.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
end;

procedure TcxTreeListController.EndUpdateSelection;
begin
  Dec(FSelectionLockCount);
  if FSelectionLockCount = 0 then
  begin
    TreeList.CheckChanges;
    if (HScrollPos <> TreeList.ViewInfo.HScrollPos) or (VScrollPos <> TreeList.ViewInfo.VScrollPos) then
      TreeList.Invalidate
    else
      TreeList.ViewInfo.UpdateSelection;
    PostMakeVisible := False;
    MakeFocusedItemVisible;
    CheckEdit;
  end;
end;

function TcxTreeListController.DeleteConfirmation: Boolean;
begin
  if TreeList.OptionsBehavior.ConfirmDelete then
    Result := cxConfirmMessageBox(
      cxGetResourceString(@scxTreeListDeletingFocusedConfirmationText),
      cxGetResourceString(@scxTreeListDeletingConfirmationCaption))
  else
    Result := True;
end;

function TcxTreeListController.IsImmediatePost: Boolean;
begin
  Result := TreeList.OptionsData.ImmediatePost;
end;

function TcxTreeListController.GetCursor(X, Y: Integer): TCursor;
begin
  if TreeList.ViewInfo.LockCount > 0 then
    Result := inherited GetCursor(X, Y)
  else
  begin
    Result := crDefault;
    if not TreeList.IsDesigning and not (TreeList.FGlassCursorRefCount > 0) then
    begin
      HitTest.HitPoint := Point(X, Y);
      Result := HitTest.Cursor;
    end;
  end;
end;

function TcxTreeListController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
const
  HeaderDragObject: array[Boolean] of TcxDragAndDropObjectClass =
    (TcxTreeListDragAndDropBandObject, TcxTreeListDragAndDropColumnObject);
begin
  DragItem := HitTest.DragItem;
  if HitTest.CanSizing then
  begin
    FResizeDirection := TcxDragSizingDirection(HitTest.HitAtSizingVert);
    Result := TcxTreeListSizingDragAndDropObject;
  end
  else
    if HitTest.CanMoving then
      Result := HeaderDragObject[HitTest.HitAtColumnHeader]
    else
      if HitTest.HitAtNode then
        Result := nil
      else
        Result := inherited GetDragAndDropObjectClass;
end;

function TcxTreeListController.HitAtNodeContent: Boolean;
begin
  Result := HitTest.HitAtNode and (HitTest.HitAtIndicator or HitTest.HitAtImages or
    HitTest.HitAtColumn or HitTest.HitAtBand or HitTest.HitAtNodePreview) and
    not HitTest.HitAtFooterArea;
end;

procedure TcxTreeListController.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  AKey: Word;
  APrevNode: TcxTreeListNode;
  APrevHScrollPos, APrevVScrollPos: Integer;
begin
  APrevVScrollPos := TreeList.VScrollBar.Position;
  APrevHScrollPos := TreeList.HScrollBar.Position;
  if FindPanel.IsFocused then
  begin
    FindPanel.KeyDown(Key, Shift);
    Exit;
  end;
  AKey := Key;
  EatKeyPress := IsNodeKeyHandle(FocusedNode, Key, Shift);
  CheckFocusedNodeItem;
  BeginUpdateSelection;
  try
    if (AKey = VK_SPACE) and (FocusedNode <> nil) and
      OptionsSelection.MultiSelect and ([ssCtrl, ssShift] * Shift <> []) then
    begin
      if (ssCtrl in Shift) then
      begin
        FocusedNode.Selected := not FocusedNode.Selected;
        if FocusedNode.Selected then
          FSelectionAnchor := FocusedNode;
      end
      else
        Select(FocusedNode, Shift);
      Key := 0;
    end;
    if (TreeList.VisibleColumnCount = 1) and (FocusedNode <> nil) and (FocusedNode.HasChildren) and
       (((Key = VK_LEFT) and FocusedNode.Expanded) or ((Key = VK_RIGHT) and not FocusedNode.Expanded)) then
    begin
      FocusedNode.Expanded := not FocusedNode.Expanded;
      Key := 0;
    end;
    inherited KeyDown(Key, Shift);
    case AKey of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME, VK_END, VK_TAB:
      begin
        if Key in [VK_NEXT, VK_PRIOR, VK_HOME, VK_END] then
        begin
          APrevNode := SelectionAnchor;
          Selection.Clear;
          if (SelectionAnchor <> nil) then
            AddNodeToSelection(SelectionAnchor);
          Select(FocusedNode, Shift);
          SelectionAnchor := APrevNode;
        end
        else
          if (FocusedNode <> nil) and not FocusedNode.Selected or
            (TreeList.OptionsSelection.MultiSelect and (Selection.Count > 1)) then
            Select(FocusedNode, Shift);
      end;
      VK_MENU, VK_SHIFT, VK_CONTROL:
      begin
        if SelectionAnchor = nil then
          SelectionAnchor := FocusedNode;
        SetHotTrackNode(HotTrackNode, Shift);
      end;
    end;
  finally
    EndUpdateSelection;
  end;
  if (APrevVScrollPos <> TreeList.VScrollBar.Position) or
    (APrevHScrollPos <> TreeList.HScrollBar.Position) then
    TreeList.ShowTouchScrollUI(TreeList, True);
end;

procedure TcxTreeListController.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  case Key of
    VK_MENU, VK_SHIFT, VK_CONTROL:
      SetHotTrackNode(HotTrackNode, Shift);
  end;
end;

procedure TcxTreeListController.MakeFocusedItemVisible;
var
  AColumn: TcxTreeListColumn;
begin
  AColumn := TcxTreeListColumn(FocusedItem);
  if AColumn <> nil then
  begin
    if not TreeList.IsLocked then
      MakeFocusedRecordVisible;
    AColumn.MakeVisible;
  end;
end;

procedure TcxTreeListController.MakeFocusedRecordVisible;
begin
  if FSelectionLockCount <> 0 then
    PostMakeVisible := True
  else
    if FocusedNode <> nil then
      FocusedNode.MakeVisible;
end;

procedure TcxTreeListController.Reset;
begin
  FPressedHeader := nil;
  ClickedObject := nil;
end;

procedure TcxTreeListController.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if TreeList.IsLocked or (AScrollCode = scEndScroll) then Exit;
  TreeList.LockScrollBars;
  try
    TreeList.Bands.LockUpdate;
    try
      EditingController.PostEditUpdate;
      SkipCheckEdit := not EditingController.IsEditing;
      EditingController.StopEditShowingTimer;
      if AScrollBarKind = sbHorizontal then
        TreeList.ViewInfo.ScrollHorz(AScrollCode, AScrollPos)
      else
        TreeList.ViewInfo.ScrollVert(AScrollCode, AScrollPos);
    finally
      TreeList.Bands.UnlockUpdate;
      SkipCheckEdit := False;
    end;
  finally
    TreeList.UpdateScrollBars;
    TreeList.UnlockScrollBars;
  end;
end;

procedure TcxTreeListController.SetSelection(AList: TList);
var
  AHelper: TcxCustomDesignSelectionHelper;
begin
  if not CanCreateSelectionHelper then Exit;
  AHelper := cxDesignSelectionHelperClass.Create(TreeList);
  try
    AHelper.SetSelection(AList);
  finally
    AHelper.Free;
  end;
end;

procedure TcxTreeListController.SetFocusedRecordItem(ARecordIndex: TdxNativeInt;
  AItem: TcxCustomInplaceEditContainer);
var
  AIndex: TdxNativeInt;
begin
  if AItem = TreeList.Preview.Column then
    AItem := FocusedItem;
  if not TreeList.Dragging then
  begin
    if ARecordIndex <> 0 then
    begin
      AIndex := TcxTreeListNode(ARecordIndex).AbsoluteIndex;
      EditingController.HideEdit(True);
      ARecordIndex := TdxNativeInt(TreeList.AbsoluteItems[AIndex]);
    end;
    inherited SetFocusedRecordItem(ARecordIndex, AItem);
  end;
end;

procedure TcxTreeListController.SetHotTrackCursor(ACursor: TCursor);
begin
  TreeList.Cursor := ACursor;
end;

procedure TcxTreeListController.SetHotTrackNode(
  ANode: TcxTreeListNode; AShift: TShiftState);
var
  ACursor: TCursor;
  APrevNode: TcxTreeListNode;
begin
  if {not TreeList.Focused or} TreeList.HitTest.CanSizing then
    ANode := nil;
  if (TreeList.IsDestroying or not TreeList.OptionsBehavior.HotTrack) or
    ((ANode = HotTrackNode) and (HotTrackColumn = HitTest.HitColumn)) then Exit;
  APrevNode := HotTrackNode;
  HotTrackNode := ANode;
  HotTrackColumn := HitTest.HitColumn;
  HotTrackShift := AShift;
  if APrevNode <> nil then
    APrevNode.Repaint(True)
  else
  begin
    if TreeList.FGlassCursorRefCount > 0 then
      PrevCursor := TreeList.FStoredCursor
    else
      PrevCursor := TreeList.Cursor;
  end;
  if ANode <> nil then
  begin
    ANode.Repaint(True);
    TreeList.DoHotTrackNode(ANode, AShift, ACursor);
    SetHotTrackCursor(ACursor);
  end
  else
    SetHotTrackCursor(PrevCursor);
end;

procedure TcxTreeListController.ShowEdit(AShift: TShiftState; X, Y: Integer);
begin
  if FocusedItem = nil then Exit;
  WasFocusedBeforeClick := False;
  EditingController.ShowEdit(FocusedItem, [ssLeft] * AShift, X, Y);
end;

function TcxTreeListController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  HitTest.HitPoint := P;
  Result := HitTest.CanSizing or HitTest.CanMoving;
  //and not (HitTest.HitAtNode and HitTest.HitNode.Selected and not HitTest.HitAtIndent);
end;

procedure TcxTreeListController.FocusChanged;
begin
  inherited FocusChanged;
  if not Focused then
    HideHint;
end;

procedure TcxTreeListController.UpdateHotTrackNode(AShift: TShiftState);
begin
  with HitTest.HitPoint do
    UpdateHotTrackNode(X, Y, AShift);
end;

procedure TcxTreeListController.UpdateHotTrackNode(X, Y: Integer; AShift: TShiftState);
begin
  HitTest.RecalculateOnMouseEvent(X, Y, AShift);
  if HitTest.HitAtNode and not HitTest.HitAtFooterArea then
    SetHottrackNode(HitTest.HitNode, AShift)
  else
    SetHottrackNode(nil, AShift);
end;

function TcxTreeListController.GetCancelEditingOnExit: Boolean;
begin
  Result := TreeList.OptionsData.CancelOnExit and TreeList.IsInserting and
    not (DataController.IsValueChanged or ((EditingController.Edit <> nil) and
    EditingController.Edit.ModifiedAfterEnter));
end;

function TcxTreeListController.GetFindPanelClass: TcxControlFindPanelClass;
begin
  Result := TcxTreeListFindPanel;
end;

function TcxTreeListController.GetFindStartPosition(ARecordIndex: TdxNativeInt; AItemIndex: Integer;
  out AHighlightedText: string): Integer;
begin
  Result := inherited GetFindStartPosition(TcxTreeListNode(ARecordIndex).AbsoluteIndex, AItemIndex, AHighlightedText);
end;

function TcxTreeListController.GetFocusedCellViewInfo(
  AEditContainer: TcxCustomInplaceEditContainer): TcxEditCellViewInfo;
begin
  Result := nil;
  if (AEditContainer = nil) or TreeList.ViewInfo.IsDirty then Exit;
  Result := TreeList.ViewInfo.GetEditCell(FocusedNode, TcxTreeListColumn(AEditContainer));
end;

function TcxTreeListController.GetImmediateEditor: Boolean;
begin
  Result := TreeList.OptionsBehavior.ImmediateEditor or
    TreeList.OptionsBehavior.AlwaysShowEditor;
end;

function TcxTreeListController.GetNavigatorClass: TcxCustomCellNavigatorClass;
begin
  Result := TcxTreeListCellNavigator;
end;

function TcxTreeListController.GetNode(ARecordIndex: Integer): TcxTreeListNode;
begin
  if ARecordIndex >= 0 then
    Result := TcxTreeListNode(ARecordIndex)
  else
    Result := nil;
end;

procedure TcxTreeListController.CancelIncSearching;
begin
  if TreeList.IsDestroying or TreeList.IsLoading or
    (IncSearchNode = nil) or SearchNotification then Exit;
  IncSearchText := '';
  IncSearchNode := nil;
  inherited CancelIncSearching;
end;

function TcxTreeListController.DoSearch(AFromNode: TcxTreeListNode;
  AItemIndex: Integer; const AText: string; AGoForward: Boolean): Boolean;

  function DoSearchNode(AForward: Boolean; var ANode: TcxTreeListNode): Boolean;
  begin
    Result := False;
    ANode := AFromNode;
    while not Result and (ANode <> nil) do
    begin
      Result := ANode.Visible and not ANode.HiddenByFilter and
        DataCompareText(ANode.Texts[AItemIndex], AText, True);
      if not Result then
        ANode := GetNextNodeForIncSearch(ANode, AForward);
    end;
  end;

var
  ANode: TcxTreeListNode;
begin
  EditingController.StopEditShowingTimer;
  Result := DoSearchNode(AGoForward, ANode);
  if not Result then
    Result := DoSearchNode(not AGoForward, ANode);
  if Result then
    IncSearchNode := ANode;
end;

function TcxTreeListController.GetIncSearchingItem: TcxCustomInplaceEditContainer;
begin
  Result := TreeList.OptionsBehavior.IncSearchItem;
  if Result = nil then
    Result := TreeList.FocusedColumn;
end;

function TcxTreeListController.GetIncSearchingText: string;
begin
  Result := IncSearchText;
end;

function TcxTreeListController.GetIsIncSearching: Boolean;
begin
  Result := IncSearchText <> '';
end;

function TcxTreeListController.GetNextNodeForIncSearch(
  ANode: TcxTreeListNode; AGoForward: Boolean): TcxTreeListNode;
begin
  Result := nil;
  if TreeList.OptionsBehavior.ExpandOnIncSearch then
  begin
    if AGoForward then
    begin
      ANode.LoadChildren;
      Result := ANode.GetNext
    end
    else
      Result := ANode.GetPrev
  end
  else
    if cxInRange(ANode.VisibleIndex + cxIntOffs[AGoForward], 0, TreeList.AbsoluteVisibleCount - 1) then
      Result := TreeList.AbsoluteVisibleItems[ANode.VisibleIndex + cxIntOffs[AGoForward]];
end;

procedure TcxTreeListController.CheckLocate(AFound: Boolean);
begin
  if AFound then Exit;
  if IncSearchNode = nil then
    IncSearchText := ''
  else
    IncSearchText := Copy(IncSearchingText, 1, Length(IncSearchingText) - 1)
end;

procedure TcxTreeListController.SearchLocate(
  AItem: TcxCustomInplaceEditContainer; const AValue: string);
var
  ANode: TcxTreeListNode;
begin
  ANode := TreeList.FocusedNode;
  if ANode = nil then
    ANode := TreeList.Root.FFirst;
  if (AItem = nil) or (ANode = nil) then Exit;
  IncSearchText := AValue;
  CheckLocate(DoSearch(ANode, TcxTreeListColumn(AItem).ItemIndex, IncSearchText, True));
end;

procedure TcxTreeListController.SearchLocateNext(
  AItem: TcxCustomInplaceEditContainer; AForward: Boolean);
var
  ANode: TcxTreeListNode;
begin
  if (AItem = nil) or (TreeList.FocusedNode = nil) then Exit;
  ANode := GetNextNodeForIncSearch(TreeList.FocusedNode, AForward);
  if ANode = nil then
    ANode := TreeList.FocusedNode;
  CheckLocate(DoSearch(ANode, TcxTreeListColumn(AItem).ItemIndex, IncSearchText, AForward));
end;

function TcxTreeListController.CanCreateSelectionHelper: Boolean;
begin
  Result := TreeList.IsDesigning and not TreeList.IsDestroying and not TreeList.IsLoading and
    (cxDesignSelectionHelperClass <> nil) and TreeList.HandleAllocated;
end;

function TcxTreeListController.IsObjectSelected(AObject: TPersistent): Boolean;
var
  AHelper: TcxCustomDesignSelectionHelper;
begin
  Result := False;
  if not CanCreateSelectionHelper then Exit;
  AHelper := cxDesignSelectionHelperClass.Create(TreeList);
  try
    Result := AHelper.IsObjectSelected(AObject);
  finally
    AHelper.Free;
  end;
end;

procedure TcxTreeListController.SelectObject(
  AObject: TPersistent; AShift: TShiftState);
var
  AHelper: TcxCustomDesignSelectionHelper;
begin
  if not CanCreateSelectionHelper then Exit;
  AHelper := cxDesignSelectionHelperClass.Create(TreeList);
  try
    AHelper.Select(AObject, AShift);
  finally
    AHelper.Free;
  end;
end;

procedure TcxTreeListController.UnselectObject(AObject: TPersistent);
var
  AHelper: TcxCustomDesignSelectionHelper;
begin
  if not CanCreateSelectionHelper then Exit;
  AHelper := cxDesignSelectionHelperClass.Create(TreeList);
  try
    AHelper.UnselectObject(AObject);
  finally
    AHelper.Free;
  end;
end;

procedure TcxTreeListController.CancelSelection(KeepPrimary: Boolean = True);
begin
  if Selection.Count = 0 then
    Exit;
  if OptionsSelection.MultiSelect then
  begin
    TreeList.BeginUpdate;
    try
      SelectionAnchor := nil;
      Selection.Clear;
      if (FocusedNode <> nil) and KeepPrimary then
        FocusedNode.Selected := True;
      TreeList.AddChanges([tcSelection]);
    finally
      TreeList.EndUpdate;
    end;
  end;
end;

procedure TcxTreeListController.Select(
  ANode: TcxTreeListNode; AShift: TShiftState);
begin
  if ANode = TreeList.Root then
    ANode := nil;
  ForceSelectionNode := ANode;
  TreeList.BeginUpdate;
  try
    BeginUpdateSelection;
    if (FSelectionAnchor = nil) and OptionsSelection.MultiSelect and (ssShift in AShift) then
      FSelectionAnchor := FocusedNode;
    if ANode = nil then
    begin
      TreeList.FocusedNode := nil;
      Exit;
    end;
    if ([ssShift, ssCtrl] * AShift = []) then
    begin
      if ([ssRight, ssLeft] * AShift = []) or not ANode.Selected then
        Selection.Clear;
    end;
    SetFocusedNode(ANode);
    if not ANode.Focused then
    begin
      if FocusedNode <> nil then
        AddNodeToSelection(FocusedNode);
      Exit;
    end;
    if not OptionsSelection.MultiSelect or
      (not (ssShift in AShift) and ((ssCtrl in AShift) or not ANode.Selected)) then
      SelectionAnchor := ANode;
    if not ANode.Selected and (not OptionsSelection.MultiSelect or ([ssShift, ssCtrl] * AShift = [])) then
    begin
      Selection.Clear;
      AddNodeToSelection(ANode);
    end
    else
      if [ssCtrl, ssShift] * AShift = [ssCtrl] then
      begin
        if ANode.Selected then
        begin
          Selection.Remove(ANode);
          TreeList.AddChanges([tcSelection]);
        end
        else
          AddNodeToSelection(ANode);
      end
      else
        if (ssLeft in AShift) and ANode.Selected then
        begin
          if ssShift in AShift then
            SelectRange(SelectionAnchor, ANode);
        end
        else
          if ssShift in AShift then
          begin
            if SelectionAnchor = nil then
              FSelectionAnchor := FocusedNode;
            SelectRange(SelectionAnchor, ANode);
          end;
    if not TreeList.OptionsSelection.MultiSelect then
      AddNodeToSelection(ANode);
  finally
    ForceSelectionNode := nil;
    try
      EndUpdateSelection;
    finally
      TreeList.CancelUpdate;
    end;
  end;
end;

procedure TcxTreeListController.SelectRange(
  const AStartNode, AFinishNode: TcxTreeListNode);
var
  I: Integer;
  ASaveChanges: TcxTreeListChanges;
begin
  if AStartNode.VisibleIndex > AFinishNode.VisibleIndex then
  begin
    SelectRange(AFinishNode, AStartNode);
    Exit;
  end;
  Selection.Clear;
  Selection.Capacity := AFinishNode.VisibleIndex - AStartNode.VisibleIndex + 1;
  ASaveChanges := TreeList.Changes * SelectionChanges;
  if AStartNode.VisibleIndex >= 0 then
    for I := AStartNode.VisibleIndex to AFinishNode.VisibleIndex do
    begin
      TreeList.Changes := TreeList.Changes  - SelectionChanges;
      AddNodeToSelection(TreeList.AbsoluteVisibleItems[I]);
    end
  else
    AddNodeToSelection(AStartNode);
  TreeList.Changes := TreeList.Changes + ASaveChanges;
end;

procedure TcxTreeListController.ViewInfoChanged;
begin
  Reset;
  TreeList.ViewInfo.IsDirty := True;
  inherited ViewInfoChanged;
end;

function TcxTreeListController.GetResizeDirection: TcxDragSizingDirection;
begin
  Result := FResizeDirection;
end;

procedure TcxTreeListController.MouseLeave;
begin
  inherited MouseLeave;
  if PressedHeader <> nil then
    PressedHeader.Pressed := False;
  if not TreeList.HandleAllocated then Exit;
  with TreeList.ScreenToClient(GetMouseCursorPos) do
    UpdateHotTrackNode(-1, -1, []);
end;

procedure TcxTreeListController.DoCancelMode;
begin
  inherited DoCancelMode;
  MouseLeave;
  HideHint;
end;

function TcxTreeListController.GetBandsCustomizationPopup: TcxTreeListBandsCustomizationPopup;
begin
  if FBandsCustomizationPopup = nil then
    FBandsCustomizationPopup := TcxTreeListBandsCustomizationPopup.Create(TreeList);
  Result := FBandsCustomizationPopup;
end;

function TcxTreeListController.GetColumnsCustomizationPopup: TcxTreeListColumnsCustomizationPopup;
begin
  if FColumnsCustomizationPopup = nil then
    FColumnsCustomizationPopup := TcxTreeListColumnsCustomizationPopup.Create(TreeList);
  Result := FColumnsCustomizationPopup;
end;

function TcxTreeListController.GetDataController: TcxTreeListDataController;
begin
  Result := TreeList.DataController;
end;

function TcxTreeListController.GetDropMode(X, Y: Integer): TcxNodeDragInfoState;
begin
  if HitTest.HitAtNodePreview or HitTest.HitAtColumn then
    Result := ndiAddChild
  else
    if Y < cxRectCenter(HitTest.HitNode.ViewData.GetRealBounds).Y then
      Result := ndiInsertBefore
    else
      Result := ndiInsertAfter;
end;

function TcxTreeListController.GetEditingController: TcxTreeListEditingController;
begin
  Result := TcxTreeListEditingController(inherited EditingController);
end;

function TcxTreeListController.GetFocusedNodeIndex: Integer;
begin
  Result := FFocusedNodeIndex;
  if FFocusedNode <> nil then
    Result := FFocusedNode.VisibleIndex;
end;

function TcxTreeListController.GetHitTest: TcxTreeListHitTest;
begin
  Result := TreeList.HitTest;
end;

function TcxTreeListController.GetOptionsBehavior: TcxTreeListOptionsBehavior;
begin
  Result := TreeList.OptionsBehavior;
end;

function TcxTreeListController.GetOptionsSelection: TcxTreeListOptionsSelection;
begin
  Result := TreeList.OptionsSelection;
end;

function TcxTreeListController.GetIsDragCopy: Boolean;
begin
  Result := IsCtrlPressed and TreeList.OptionsBehavior.AutoDragCopy;
end;

function TcxTreeListController.GetLeftPos: Integer;
begin
  Result := TreeList.ViewInfo.HScrollPos;
end;

function TcxTreeListController.GetSelection: TList;
begin
  Result := TreeList.SelectionList;
end;

function TcxTreeListController.GetSelectionAnchor: TcxTreeListNode;
begin
  if FSelectionAnchor = nil then
    FSelectionAnchor := FocusedNode;
  Result := FSelectionAnchor;
  while (Result <> nil) and not Result.IsVisible  do
    Result := Result.Parent;
end;

function TcxTreeListController.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(EditingControl);
end;

procedure TcxTreeListController.SetForcingWidthColumn(
  AColumn: TcxTreeListColumn);
begin
  FForcingWidthColumn := AColumn;
end;

procedure TcxTreeListController.SetIncSearchNode(ANode: TcxTreeListNode);
var
  APrevNode: TcxTreeListNode;
begin
  if ANode = IncSearchNode then
  begin
    if (ANode <> nil) then
    begin
      ANode.MakeVisible;
      ANode.Repaint(True);
    end;
    Exit;
  end;
  FSearchNotification := True;
  try
    APrevNode := IncSearchNode;
    FIncSearchNode := ANode;
    if (APrevNode <> nil) and not APrevNode.Deleting then
      APrevNode.Repaint(True);
    if IncSearchNode <> nil then
    begin
      IncSearchNode.Focused := True;
      IncSearchNode.MakeVisible;
      IncSearchNode.Repaint(True);
    end;
  finally
    FSearchNotification := False;
  end;
end;

procedure TcxTreeListController.SetLeftPos(AValue: Integer);
begin
  TreeList.ViewInfo.HScrollPos := AValue;
  TreeList.UpdateScrollBars;
end;

procedure TcxTreeListController.SetPressedHeader(
  AHeader: TcxTreeListCustomHeaderCellViewInfo);
begin
  if (FPressedHeader <> AHeader) and (FPressedHeader <> nil) then
    FPressedHeader.State := cxbsNormal;
  FPressedHeader := AHeader;
  if FPressedHeader <> nil then
  begin
    FPressedHeader.State := cxbsPressed;
    FPressedHeader.Click;
  end;
end;

{ TcxTreeListPopup }

constructor TcxTreeListPopup.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  Color := clWindow;
  IsTopMost := True;
  OwnerParent := TreeList;
  CreateCustomizationControl;
end;

function TcxTreeListPopup.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(OwnerControl);
end;

function TcxTreeListPopup.CalculateOwnerBounds: TRect;
begin
  Result := FOwner.GetOwnerBounds;
end;

function TcxTreeListPopup.GetClientMinWidth: Integer;
begin
  Result := cxRectWidth(Owner.GetOwnerBounds) - NCWidth;
end;

procedure TcxTreeListPopup.InitPopup;
begin
  inherited InitPopup;
  BiDiMode := TreeList.BiDiMode;
  BorderStyle := TreeList.LookAndFeelPainter.PopupBorderStyle;
  Font := TreeList.Font;
  AlignHorz := GetDefaultAlignHorz;
  AlignVert := pavBottom;
  Direction := pdVertical;
  OwnerBounds := CalculateOwnerBounds;
end;

function TcxTreeListPopup.NeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle;
  AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean;
begin
  Result := cxRectPtIn(OwnerBounds, TreeList.ScreenToClient(APos));
end;

procedure TcxTreeListPopup.Paint;
begin
  DrawFrame;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientBounds);
end;

procedure TcxTreeListPopup.VisibleChanged;
begin
  inherited;
  if not Visible and (FOwner <> nil) then
    FOwner.PopupClosed;
end;

procedure TcxTreeListPopup.Popup;
begin
  SetCaptureControl(nil);
  inherited Popup(FindNextControl(nil, True, True, False));
end;

{ TcxTreeListCustomizationPopup }

constructor TcxTreeListCustomizationPopup.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  FChangingItems := TList.Create;
end;

destructor TcxTreeListCustomizationPopup.Destroy;
begin
  FreeAndNil(FChangingItems);
  inherited Destroy;
end;

procedure TcxTreeListCustomizationPopup.CloseUp;
begin
  inherited CloseUp;
  CustomizationControl.Clear;
end;

procedure TcxTreeListCustomizationPopup.CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize);

  function GetScrollbarAreaSize: TSize;
  begin
    Result := cxSize(TcxControlAccess(CheckListBox).GetVScrollBarDefaultAreaWidth,
      TcxControlAccess(CheckListBox).GetHScrollbarDefaultAreaHeight);
  end;

var
  AOwnerScreenBounds, ADesktopWorkArea: TRect;
  ADesktopSpace, AMaxRowCount, AColumnCount: Integer;
  ACustomizationControl: TdxCustomTreeListQuickCustomizationControl;
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
    ACustomizationControl := CustomizationControl as TdxCustomTreeListQuickCustomizationControl;
    AMaxRowCount := (ADesktopSpace - ACustomizationControl.CheckListBox.Top) div ACustomizationControl.CheckListBox.ItemHeight - 1;
    SetVisibleRowCount(AMaxRowCount);
    if IsMultiColumnMode then
    begin
      AColumnCount := ACustomizationControl.CheckListBox.Count div AMaxRowCount +
        Integer(ACustomizationControl.CheckListBox.Count mod AMaxRowCount <> 0);
      repeat
        RestoreControlsBounds;
        SetVisibleRowCount(Min(AMaxRowCount, ACustomizationControl.CheckListBox.Count div AColumnCount + 1));
        ACustomizationControl.CheckListBox.Columns := AColumnCount;
        ACustomizationControl.AdjustBounds(True);
        ASize := CalculateSize;
        AlignVert := pavBottom;
        APosition := CalculatePosition(ASize);
        Dec(AColumnCount);
      until (APosition.X + ASize.cx <= ADesktopWorkArea.Right) or (APosition.X - ASize.cx >= ADesktopWorkArea.Left);
    end
    else
    begin
      RestoreControlsBounds;
      ACustomizationControl.AdjustBounds(True);
      ASize := CalculateSize;
      AlignVert := pavBottom;
      APosition := CalculatePosition(ASize);
    end;
  end;
end;

procedure TcxTreeListCustomizationPopup.AdjustCustomizationControlSize;
begin
  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).AdjustSize;
end;

procedure TcxTreeListCustomizationPopup.CheckListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
var
  AListBox: TdxQuickCustomizationCustomCheckListBox absolute Sender;
begin
  CheckListBoxCheckClicked(AItemIndex, AListBox.Checked[AItemIndex]);
  if CustomizationControl.HasCommands then
    CustomizationControl.CommandListBox.States[0] :=
      TdxCustomTreeListQuickCustomizationControl(CustomizationControl).GetCheckingAllState;
end;

procedure TcxTreeListCustomizationPopup.CheckListBoxCheckClicked(AIndex: Integer; AChecked: Boolean);
begin
  ItemClicked(CheckListBox.Items[AIndex].Data, AChecked);
end;

procedure TcxTreeListCustomizationPopup.CheckListBoxInitializeSelection(Sender: TObject);
var
  I: Integer;
begin
  FSelectionInitializingInProcess := True;
  try
    if TreeList.IsDesigning then
      for I := 0 to CheckListBox.Count - 1 do
        if TreeList.Controller.IsObjectSelected(CheckListBox.Items[I].Data as TPersistent) then
        begin
          CheckListBox.Selected[I] := True;
          if CheckListBox.ItemIndex = -1 then
            CheckListBox.ItemIndex := I;
        end;
  finally
    FSelectionInitializingInProcess := False;
  end;
end;

procedure TcxTreeListCustomizationPopup.CheckListBoxSelectionChanged(Sender: TObject);
var
  AItems: TList;
  AFocusedItem: TObject;
begin
  if not TreeList.IsDesigning or FSelectionInitializingInProcess then Exit;
  AItems := TList.Create;
  try
    GetCheckListBoxSelectedItems(AItems, AFocusedItem);
    TreeList.Controller.SetSelection(AItems);
  finally
    AItems.Free;
  end;
end;

procedure TcxTreeListCustomizationPopup.CheckListSelectedItemCheckedStateChanging(Sender: TObject);
var
  AFocusedItem: TObject;
  I: Integer;
begin
  TreeList.BeginUpdate();
  FChangingItems.Clear;
  GetCheckListBoxSelectedItems(FChangingItems, AFocusedItem);
  SetLength(FChangingItemsCheckStates, FChangingItems.Count);
  for I := 0 to FChangingItems.Count - 1 do
    FChangingItemsCheckStates[I] := Integer(IsVisibleTreeListItem(FChangingItems[I]));
end;

procedure TcxTreeListCustomizationPopup.CheckListSelectedItemCheckedStateChanged(Sender: TObject);
var
  I, AIndex: Integer;
  AItems: TList;
  AFocusedItem: TObject;
begin
  SynchronizeTreeListItemsVisibilityWithListBox;

  if TdxCustomCheckListBoxAccess(CheckListBox).GetSelectionCount > 0 then
  begin
    AItems := TList.Create;
    try
      GetCheckListBoxSelectedItems(AItems, AFocusedItem);
      for I := 0 to FChangingItems.Count - 1 do
      begin
        AIndex := CheckListBox.Items.IndexOfObject(FChangingItems[I]);
        if (AIndex = -1) or
           (Integer((IsVisibleTreeListItem(CheckListBox.Items[AIndex].Data))) <> FChangingItemsCheckStates[I]) then
          HandleItemClicked(FChangingItems[I], IsVisibleTreeListItem(FChangingItems[I]));
      end;
      SetCheckListBoxSelectedItems(AItems, AFocusedItem);
    finally
      AItems.Free;
    end;
  end;
  FChangingItems.Clear;
  TreeList.EndUpdate;

  if CustomizationControl.HasCommands then
    CustomizationControl.CommandListBox.States[0] :=
      TdxCustomTreeListQuickCustomizationControl(CustomizationControl).GetCheckingAllState;
end;

procedure TcxTreeListCustomizationPopup.CreateCustomizationControl;
begin
  FCustomizationControl := TdxCustomTreeListQuickCustomizationControl.Create(Self);
  CustomizationControl.OnInitialize := CustomizationControlInitialize;

  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).Style.BorderStyle := cbsNone;
  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).Style.Edges := [];
  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).Style.HotTrack := False;
  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).Style.TransparentBorder := False;

  CustomizationControl.CheckListBox.OnAction := CheckListBoxAction;
  CustomizationControl.CheckListBox.OnSelectionChanged := CheckListBoxSelectionChanged;
  CustomizationControl.CheckListBox.OnSelectedItemCheckedStateChanging := CheckListSelectedItemCheckedStateChanging;
  CustomizationControl.CheckListBox.OnSelectedItemCheckedStateChanged := CheckListSelectedItemCheckedStateChanged;
end;


procedure TcxTreeListCustomizationPopup.CustomizationControlInitialize;
begin
  UpdateCommandListBoxItemsChecking;
  CheckListBoxInitializeSelection(CheckListBox);
  CheckListBox.ItemMoving := False;
end;

function TcxTreeListCustomizationPopup.GetCheckListBox: TdxQuickCustomizationCustomCheckListBox;
begin
  Result := CustomizationControl.CheckListBox;
end;

procedure TcxTreeListCustomizationPopup.GetCheckListBoxSelectedItems(AItems: TList; var AFocusedItem: TObject);
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Selected[I] then
      AItems.Add(CheckListBox.Items[I].Data);
  AFocusedItem := nil;
  if CheckListBox.Items.IsValidIndex(CheckListBox.ItemIndex) then
    AFocusedItem := CheckListBox.Items[CheckListBox.ItemIndex].Data;
end;

procedure TcxTreeListCustomizationPopup.InitPopup;
begin
  inherited InitPopup;
  CustomizationControl.Canvas.Font := Font;
  CustomizationControl.CheckListBox.Columns := 0;
  SetVisibleRowCount(GetDropDownCount);
  CustomizationControl.Initialize(Self);
end;

procedure TcxTreeListCustomizationPopup.ItemClicked(AItem: TObject; AChecked: Boolean);
begin
  if IsVisibleTreeListItem(AItem) <> AChecked then
    HandleItemClicked(AItem, AChecked);
end;

procedure TcxTreeListCustomizationPopup.RefreshCheckListBoxItems;
begin
  AddCheckListBoxItems;
end;

procedure TcxTreeListCustomizationPopup.SetCheckListBoxSelectedItems(AItems: TList; AFocusedItem: TObject);
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

procedure TcxTreeListCustomizationPopup.SetQuickCustomizationSortOptions;
begin
  TdxCustomCheckListBoxAccess(CheckListBox).SortOptions := [lbsAnsiSort, lbsCaseInsensitive];
end;

procedure TcxTreeListCustomizationPopup.SetVisibleRowCount(ACount: Integer);
begin
  TdxCustomTreeListQuickCustomizationControl(CustomizationControl).CheckListBoxVisibleRowCount := ACount;
end;

procedure TcxTreeListCustomizationPopup.UpdateCommandListBoxItemsChecking;
begin
  if CustomizationControl.HasCommands then
  begin
    CustomizationControl.CommandListBox.States[0] :=
      TdxCustomTreeListQuickCustomizationControl(CustomizationControl).GetCheckingAllState;
    CustomizationControl.CommandListBox.Checked[1] := IsCheckListBoxSorted;
  end;
end;

 { TcxTreeListColumnsCustomizationPopup }

procedure TcxTreeListColumnsCustomizationPopup.AddCheckListBoxItems;
var
  I: Integer;
  AList: TList;
  AColumn: TcxTreeListColumn;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    CheckListBox.Sorted := False;
    AList := TList.Create;
    try
      for I := 0 to TreeList.ColumnCount - 1 do
        if TreeList.Columns[I].IsVisibleInQuickCustomizationPopup then
          AList.Add(TreeList.Columns[I]);
      AList.Sort(@cxCompareColumnsByActualPosition);
      for I := 0 to AList.Count - 1 do
      begin
        AColumn := TcxTreeListColumn(AList[I]);
        CheckListBox.Items.AddObject(AColumn.Caption.Text, AColumn).Checked := AColumn.Visible;
      end;
    finally
      AList.Free;
    end;
    SetQuickCustomizationSortOptions;
    CheckListBox.Sorted := IsCheckListBoxSorted;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

function TcxTreeListColumnsCustomizationPopup.CanAddCommands: Boolean;
begin
  Result := TreeList.OptionsCustomizing.ColumnsQuickCustomizationShowCommands;
end;

procedure TcxTreeListColumnsCustomizationPopup.DoItemPosChanged(AItem: TObject);
begin
  TreeList.DoColumnPosChanged(TcxTreeListColumn(AItem));
end;

function TcxTreeListColumnsCustomizationPopup.GetDropDownCount: Integer;
begin
  Result := TreeList.OptionsCustomizing.ColumnsQuickCustomizationMaxDropDownCount;
end;

procedure TcxTreeListColumnsCustomizationPopup.HandleItemClicked(AItem: TObject; AChecked: Boolean);
begin
  TcxTreeListColumn(AItem).Visible := AChecked;
  DoItemPosChanged(AItem);
  TreeList.Modified;
end;

function TcxTreeListColumnsCustomizationPopup.IsCheckListBoxSorted: Boolean;
begin
  Result := TreeList.OptionsCustomizing.ColumnsQuickCustomizationSorted;
end;

function TcxTreeListColumnsCustomizationPopup.IsMultiColumnMode: Boolean;
begin
  Result := TreeList.OptionsCustomizing.ColumnsQuickCustomizationMultiColumnMode;
end;

function TcxTreeListColumnsCustomizationPopup.IsVisibleTreeListItem(AItem: TObject): Boolean;
begin
  Result := (AItem is TcxTreeListColumn) and TcxTreeListColumn(AItem).Visible;
end;

procedure TcxTreeListColumnsCustomizationPopup.SetQuickCustomizationSorted(AValue: Boolean);
begin
  TreeList.OptionsCustomizing.ColumnsQuickCustomizationSorted := AValue;
end;

procedure TcxTreeListColumnsCustomizationPopup.SynchronizeTreeListItemsVisibilityWithListBox;
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    (CheckListBox.Items[I].Data as TcxTreeListColumn).Visible := CheckListBox.Checked[I];
end;

{ TcxTreeListBandsCustomizationPopup }

procedure TcxTreeListBandsCustomizationPopup.AddCheckListBoxItems;
var
  I: Integer;
  ABand: TcxTreeListBand;
  AList: TList;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    CheckListBox.Sorted := False;
    AList := TList.Create;
    try
      for I := 0 to TreeList.Bands.Count - 1 do
        if TreeList.Bands[I].IsVisibleInQuickCustomizationPopup then
          AList.Add(TreeList.Bands[I]);
      AList.Sort(@cxCompareBandsByActualPositionEx);
      for I := 0 to AList.Count - 1 do
      begin
        ABand := TcxTreeListBand(AList[I]);
        CheckListBox.Items.AddObject(ABand.Caption.Text, ABand).Checked := ABand.Visible;
      end;
    finally
      AList.Free;
    end;
    SetQuickCustomizationSortOptions;
    CheckListBox.Sorted := IsCheckListBoxSorted;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

function TcxTreeListBandsCustomizationPopup.CanAddCommands: Boolean;
begin
  Result := TreeList.OptionsCustomizing.BandsQuickCustomizationShowCommands;
end;

procedure TcxTreeListBandsCustomizationPopup.DoItemPosChanged(AItem: TObject);
begin
  TreeList.DoBandPosChanged(TcxTreeListBand(AItem));
end;

function TcxTreeListBandsCustomizationPopup.GetDropDownCount: Integer;
begin
  Result := TreeList.OptionsCustomizing.BandsQuickCustomizationMaxDropDownCount;
end;

procedure TcxTreeListBandsCustomizationPopup.HandleItemClicked(AItem: TObject; AChecked: Boolean);
var
  AItems: TList;
  AFocusedItem: TObject;
  ACount: Integer;
  ASize: TSize;
  ACustomizationControl: TdxQuickCustomizationCustomControlAccess;
  P: TPoint;
begin
  ACount := CheckListBox.Count;
  TcxTreeListBand(AItem).Visible := AChecked;
  AItems := TList.Create;
  try
    GetCheckListBoxSelectedItems(AItems, AFocusedItem);
    RefreshCheckListBoxItems;
    SetCheckListBoxSelectedItems(AItems, AFocusedItem);
  finally
    AItems.Free;
  end;
  if ACount <> CheckListBox.Count then
  begin
    SetVisibleRowCount(GetDropDownCount);
    ACustomizationControl := TdxQuickCustomizationCustomControlAccess(CustomizationControl);
    ACustomizationControl.AdjustBounds;
    ASize := cxSize(
      BorderWidths[bLeft] + CustomizationControl.Width + BorderWidths[bRight],
      BorderWidths[bTop] + CustomizationControl.Height + BorderWidths[bBottom]);
    P := CalculatePosition(ASize);
    CorrectBoundsWithDesktopWorkArea(P, ASize);
    ACustomizationControl.RecreateWnd;
    SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
    FocusControl(CheckListBox);
  end;
  DoItemPosChanged(AItem);
  TreeList.Modified;
end;

function TcxTreeListBandsCustomizationPopup.IsCheckListBoxSorted: Boolean;
begin
  Result := TreeList.OptionsCustomizing.BandsQuickCustomizationSorted;
end;

function TcxTreeListBandsCustomizationPopup.IsMultiColumnMode: Boolean;
begin
  Result := TreeList.OptionsCustomizing.BandsQuickCustomizationMultiColumnMode;
end;

function TcxTreeListBandsCustomizationPopup.IsVisibleTreeListItem(AItem: TObject): Boolean;
begin
  Result := (AItem is TcxTreeListBand) and TcxTreeListBand(AItem).Visible;
end;

procedure TcxTreeListBandsCustomizationPopup.SetQuickCustomizationSorted(AValue: Boolean);
begin
  TreeList.OptionsCustomizing.BandsQuickCustomizationSorted := AValue;
end;

procedure TcxTreeListBandsCustomizationPopup.SynchronizeTreeListItemsVisibilityWithListBox;
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    (CheckListBox.Items[I].Data as TcxTreeListBand).Visible := CheckListBox.Checked[I];
end;

{ TdxCustomTreeListQuickCustomizationControl }

procedure TdxCustomTreeListQuickCustomizationControl.DoCheckAllItems(AValue: Boolean);
var
  I, AExpectedCount: Integer;
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    TreeList.BeginUpdate();
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
      TreeList.EndUpdate;
    end;
  finally
    Screen.Cursor := ACursor;
  end;
end;

procedure TdxCustomTreeListQuickCustomizationControl.CheckAllItems(Sender: TObject);
begin
  DoCheckAllItems((Sender as TdxQuickCustomizationCustomCommand).Checked);
end;

procedure TdxCustomTreeListQuickCustomizationControl.SortItems(Sender: TObject);
begin
  Popup.SetQuickCustomizationSorted((Sender as TdxQuickCustomizationCustomCommand).Checked);
  PopulateCheckListBox;
  TreeList.Modified;
end;

procedure TdxCustomTreeListQuickCustomizationControl.PopulateCommandListBox;
begin
  if Popup.CanAddCommands then
  begin
    AddCommand(cxGetResourceString(@scxQuickCustomizationAllCommandCaption), True, CheckAllItems);
    AddCommand(cxGetResourceString(@scxQuickCustomizationSortedCommandCaption), True, SortItems);
  end;
end;

procedure TdxCustomTreeListQuickCustomizationControl.PopulateCheckListBox;
begin
  Popup.AddCheckListBoxItems;
end;

function TdxCustomTreeListQuickCustomizationControl.GetTreeList: TcxCustomTreeList;
begin
  Result := Popup.TreeList;
end;

function TdxCustomTreeListQuickCustomizationControl.GetPopup: TcxTreeListCustomizationPopup;
begin
  Result := Owner as TcxTreeListCustomizationPopup;
end;

function TdxCustomTreeListQuickCustomizationControl.GetCheckingAllState: TcxCheckBoxState;
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

procedure TdxCustomTreeListQuickCustomizationControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Popup.CloseUp;
  end;
end;

{TcxTreeListEditingController }

constructor TcxTreeListEditingController.Create(AController: TcxCustomControlController);
begin
  inherited Create(AController);
  FMultilineEdit := CreateMultilineEdit;
end;

destructor TcxTreeListEditingController.Destroy;
begin
  FMultilineEdit.Free;
  inherited Destroy;
end;

procedure TcxTreeListEditingController.AfterAssignMultilineEditProperties;
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

procedure TcxTreeListEditingController.BeforeAssignMultilineEditProperties;
begin
  MultiLineEdit.AutoHeight := AutoHeight;
  MultiLineEdit.BorderColor := TreeList.OptionsView.EditAutoHeightBorderColor;
  MultiLineEdit.ActiveProperties.WantReturns := False;
  if MultiLineEdit.BorderColor <> clNone then
  begin
    if MultiLineEdit.BorderColor = clDefault then
      MultiLineEdit.BorderColor := TreeList.ViewInfo.GridLineColor;
  end;
end;

function TcxTreeListEditingController.CanUpdateEditValue: Boolean;
begin
  Result := inherited CanUpdateEditValue and (EditingCellViewInfo <> nil);
end;

function TcxTreeListEditingController.CanUpdateMultilineEditHeight: Boolean;
begin
  Result := CanUpdateEditValue and not Edit.IsHiding;
end;

function TcxTreeListEditingController.CanUseAutoHeightEditor: Boolean;
begin
  Result := TcxTreeListColumn(EditingItem).CanEditAutoHeight and
    (esoEditingAutoHeight in FEdit.ActiveProperties.GetSupportedOperations);
end;

procedure TcxTreeListEditingController.CheckMultilineEditBounds(var ABounds: TRect);
begin
  if FIsEditAutoHeight and (AutoHeight = eahEditor) then
    InflateRect(ABounds, 1, 1);
end;

procedure TcxTreeListEditingController.CheckUsingMultilineEdit;
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

function TcxTreeListEditingController.CreateMultilineEdit: TcxAutoHeightInplaceEdit;
begin
  Result := TcxAutoHeightInplaceEdit.Create(self);
end;

function TcxTreeListEditingController.GetAdjustedMultilineEditBounds(const ABounds: TRect): TRect;
begin
  Result := TreeList.ViewInfo.GetMultilineEditorBounds(ABounds,
    MultiLineEdit.CalculatedHeight, MultiLineEdit.AutoHeight);
end;

function TcxTreeListEditingController.GetFocusRectBounds: TRect;
begin
  if TreeList.OptionsView.FocusRect then
    Result := cxRectOffset(TreeList.FocusedNode.ViewData.FocusRectBounds, TreeList.FocusedNode.ViewData.Origin)
  else
    Result := cxNullRect;
end;

procedure TcxTreeListEditingController.InitEdit;
begin
  CheckUsingMultilineEdit;
  inherited InitEdit;
end;

procedure TcxTreeListEditingController.MultilineEditTextChanged;
var
  R: TRect;
begin
  if not CanUpdateMultilineEditHeight then Exit;
  if AutoHeight = eahRow then
  begin
    if MultiLineEdit.ModifiedAfterEnter then
      TreeList.LayoutChanged;
  end
  else
  begin
    R := GetAdjustedMultilineEditBounds(MultiLineEdit.BoundsRect);
    if not cxRectIsEqual(MultiLineEdit.BoundsRect, R) then
      MultiLineEdit.BoundsRect := R;
  end;
end;

procedure TcxTreeListEditingController.StartEditAutoHeight(AHeightChanged: Boolean);
begin
  if CanUpdateMultilineEditHeight then
  begin
    if AutoHeight = eahEditor then
      MultiLineEdit.Invalidate
    else
      if AHeightChanged then
        TreeList.LayoutChanged
  end;
end;

function TcxTreeListEditingController.GetAutoHeight: TcxInplaceEditAutoHeight;
begin
  if FIsEditAutoHeight then
    Result := Column.GetEditAutoHeight
  else
    Result := eahNone;
end;

function TcxTreeListEditingController.GetColumn: TcxTreeListColumn;
begin
  Result := EditingItem as TcxTreeListColumn;
end;

function TcxTreeListEditingController.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(EditingControl);
end;

{ TcxTreeListLevelInfo }

constructor TcxTreeListLevelInfo.Create(ATreeList: TcxCustomTreeList);
begin
  FTreeList := ATreeList;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := Changed;
  FStateImagesChangeLink := TChangeLink.Create;
  FStateImagesChangeLink.OnChange := Changed;
end;

destructor TcxTreeListLevelInfo.Destroy;
begin
  Images := nil;
  StateImages := nil;
  FImagesChangeLink.Free;
  FStateImagesChangeLink.Free;
  inherited Destroy;
end;

procedure TcxTreeListLevelInfo.Changed(Sender: TObject);
begin
  if TreeList.IsDestroying or TreeList.LockChanges then Exit;
  Update(0);
  TreeList.ImagesChanged(Sender);
end;

procedure TcxTreeListLevelInfo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation <> opRemove then Exit;
  if AComponent = FImages then
    FImages := nil;
  if AComponent = FStateImages then
    FStateImages := nil;
end;

function TcxTreeListLevelInfo.Update(const AOffset: Integer): Integer;
var
  AImageSize: TSize;
begin
  FOffset := AOffset;
  FSize := TreeList.DefaultIndentSize;
  FWidth := FSize.cx;
  if Images <> nil then
  begin
    AImageSize := dxGetImageSize(Images, ScaleFactor);
    FSize.cy := Max(FSize.cy, AImageSize.cy + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2);
    if not DynamicIndent then
    begin
      FSize.cx := Max(AImageSize.cx + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2, FSize.cx);
      Inc(FWidth, FSize.cx);
    end
    else
      Inc(FWidth, AImageSize.cx + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2);
  end;
  if StateImages <> nil then
  begin
    AImageSize := dxGetImageSize(StateImages, ScaleFactor);
    FSize.cy := Max(FSize.cy, AImageSize.cy + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2);
    FSize.cx := Max(AImageSize.cx + ScaleFactor.Apply(cxTreeListIndentOffsetSize) div 2, FSize.cx);
    Inc(FWidth, FSize.cx);
  end;
  Result := FOffset + FSize.cx;
end;

function TcxTreeListLevelInfo.GetTotalWidth: Integer;
begin
  Result := FOffset + Width;
end;

function TcxTreeListLevelInfo.GetDynamicIndent: Boolean;
begin
  Result := TreeList.OptionsView.DynamicIndent;
end;

function TcxTreeListLevelInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TreeList.ScaleFactor;
end;

procedure TcxTreeListLevelInfo.SetImages(AValue: TCustomImageList);
begin
  TreeList.SetImageList(AValue, FImages, FImagesChangeLink);
end;

procedure TcxTreeListLevelInfo.SetStateImages(AValue: TCustomImageList);
begin
  TreeList.SetImageList(AValue, FStateImages, FStateImagesChangeLink);
end;

{ TcxTreeListLikeParams }

constructor TcxTreeListLikeParams.Create(APercent: Char = '%';
  AUnderline: Char = '_');
begin
  FPercent := APercent;
  FUnderline := AUnderline;
end;

{ TcxTreeListConditionalFormattingProvider }

constructor TcxTreeListConditionalFormattingProvider.Create(AOwner: TcxCustomTreeList);
begin
  inherited Create(AOwner.DataController);
  FOwner := AOwner;
end;

procedure TcxTreeListConditionalFormattingProvider.DoBeginUpdate;
begin
  FOwner.BeginUpdate;
end;

procedure TcxTreeListConditionalFormattingProvider.DoEndUpdate;
begin
  FOwner.EndUpdate;
end;

function TcxTreeListConditionalFormattingProvider.EditCellViewInfoToPoint(ACellViewInfo: TObject): TPoint;
begin
  Result := inherited EditCellViewInfoToPoint(ACellViewInfo);
  Result.Y := TcxTreeListEditCellViewInfo(ACellViewInfo).Node.AbsoluteIndex;
end;

function TcxTreeListConditionalFormattingProvider.GetController: TcxCustomControlController;
begin
  Result := FOwner.Controller;
end;

function TcxTreeListConditionalFormattingProvider.GetItemDisplayName(AItem: TcxCustomInplaceEditContainer): string;
begin
  Result := TcxTreeListColumn(AItem).Caption.Text;
end;

function TcxTreeListConditionalFormattingProvider.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.LookAndFeel;
end;

function TcxTreeListConditionalFormattingProvider.GetOwner: TComponent;
begin
  Result := FOwner;
end;

function TcxTreeListConditionalFormattingProvider.GetRecordCount: Integer;
begin
  Result := FOwner.AbsoluteCount;
end;

function TcxTreeListConditionalFormattingProvider.DoGetParentForm: TCustomForm;
begin
  Result := Forms.GetParentForm(FOwner);
end;

function TcxTreeListConditionalFormattingProvider.GetScaleFactor: TdxScaleFactor;
begin
  Result := FOwner.ScaleFactor;
end;

function TcxTreeListConditionalFormattingProvider.IsItemVisible(AItem: TcxCustomInplaceEditContainer): Boolean;
begin
  Result := TcxTreeListColumn(AItem).Visible;
end;

function TcxTreeListConditionalFormattingProvider.IsRightToLeft: Boolean;
begin
  Result := SysLocale.MiddleEast and (FOwner.BiDiMode <> bdLeftToRight);
end;

function TcxTreeListConditionalFormattingProvider.IsRowVisible(const ARow: Integer): Boolean;
begin
  Result := TcxTreeListNode(FOwner.FAbsoluteItems[ARow]).Visible;
end;

{ TcxTreeListFilterValueList }

function TcxTreeListFilterValueList.GetDateTimeRelativeFilterDisplayText(AKind: TcxFilterOperatorKind): string;
begin
  case AKind of
    foYesterday:
      Result := cxGetResourceString(@scxTreeListYesterday);
    foToday:
      Result := cxGetResourceString(@scxTreeListToday);
    foTomorrow:
      Result := cxGetResourceString(@scxTreeListTomorrow);
    foLast30Days:
      Result := cxGetResourceString(@scxTreeListLast30Days);
    foLast14Days:
      Result := cxGetResourceString(@scxTreeListLast14Days);
    foLast7Days:
      Result := cxGetResourceString(@scxTreeListLast7Days);
    foNext7Days:
      Result := cxGetResourceString(@scxTreeListNext7Days);
    foNext14Days:
      Result := cxGetResourceString(@scxTreeListNext14Days);
    foNext30Days:
      Result := cxGetResourceString(@scxTreeListNext30Days);
    foLastTwoWeeks:
      Result := cxGetResourceString(@scxTreeListLastTwoWeeks);
    foLastWeek:
      Result := cxGetResourceString(@scxTreeListLastWeek);
    foThisWeek:
      Result := cxGetResourceString(@scxTreeListThisWeek);
    foNextWeek:
      Result := cxGetResourceString(@scxTreeListNextWeek);
    foNextTwoWeeks:
      Result := cxGetResourceString(@scxTreeListNextTwoWeeks);
    foLastMonth:
      Result := cxGetResourceString(@scxTreeListLastMonth);
    foThisMonth:
      Result := cxGetResourceString(@scxTreeListThisMonth);
    foNextMonth:
      Result := cxGetResourceString(@scxTreeListNextMonth);
    foLastYear:
      Result := cxGetResourceString(@scxTreeListLastYear);
    foThisYear:
      Result := cxGetResourceString(@scxTreeListThisYear);
    foNextYear:
      Result := cxGetResourceString(@scxTreeListNextYear);
    foInPast:
      Result := cxGetResourceString(@scxTreeListPast);
    foInFuture:
      Result := cxGetResourceString(@scxTreeListFuture);
    else
      Result := inherited GetDateTimeRelativeFilterDisplayText(AKind);
  end;
end;

{ TcxTreeListCellNavigator }

function TcxTreeListCellNavigator.GetCount(ARecordIndex: Integer): Integer;
begin
  Result := 0;
  if IsGroupRow(ARecordIndex) then
    Result := 1
  else
    if cxInRange(ARecordIndex, 0, TreeList.AbsoluteVisibleCount - 1) then
      Result := TreeList.VisibleColumnCount;
end;

procedure TcxTreeListCellNavigator.CalcNextRow(AForward: Boolean;
  var ARowIndex, ACellIndex: Integer);
var
  AColumn: TcxTreeListColumn;
  ANewRow: Integer;
  ACellFound: Boolean;
begin
  ACellFound := False;
   //Check Row index
  ACellIndex := Min(TreeList.VisibleColumnCount - 1, Max(ACellIndex, 0));
  if not IsGroupRow(ARowIndex) and (ACellIndex >= 0) then
  begin
    AColumn := TreeList.VisibleColumns[ACellIndex];
    ACellFound := GoToNextCellInBand(AForward, AColumn, ACellIndex);
  end
  else
    AColumn := nil;
  if not ACellFound then
  begin
    ANewRow := ARowIndex + cxIntOffs[AForward];
    if cxInRange(ANewRow, 0, TreeList.AbsoluteVisibleCount - 1) then
    begin
      ARowIndex := ANewRow;
      GoToNextNodeCell(AForward, TreeList.AbsoluteVisibleItems[ARowIndex], AColumn, ACellIndex);
    end
    else
      if AForward then
        AppendRecord(ARowIndex, ACellIndex);
  end
end;

procedure TcxTreeListCellNavigator.AppendRecord(
  var ARowIndex, ACellIndex: Integer);
begin
  if not TreeList.OptionsData.Appending then
    Exit;
  if TreeList.IsEditing or TreeList.IsInserting then
   Controller.EditingController.UpdateValue;
  try
    TreeList.DataController.CheckBrowseMode;
  finally
    if TcxTreeListController(Controller).CanInsertNode and TreeList.InsertNode(nil, True) then
    begin
      ARowIndex := TreeList.AbsoluteVisibleCount - 1;
      if TreeList.OptionsBehavior.FocusFirstCellOnNewRecord then
        ACellIndex := 0;
    end;
  end;
end;

function TcxTreeListCellNavigator.GetCellContainer(
  ARowIndex, ACellIndex: Integer): TcxCustomInplaceEditContainer;
begin
  if not cxInRange(ACellIndex, 0, GetCount(ARowIndex) - 1) then
    Result := nil
  else
    Result := TreeList.VisibleColumns[ACellIndex];
end;

function TcxTreeListCellNavigator.GetNode(
  ARowIndex: Integer): TcxTreeListNode;
begin
  Result := nil;
  if (ARowIndex < 0) or (ARowIndex >= TreeList.AbsoluteVisibleCount) then Exit;
  Result := TreeList.AbsoluteVisibleItems[ARowIndex];
end;

procedure TcxTreeListCellNavigator.Init(
  var ARowIndex, ACellIndex, ARowCount: Integer);
var
  ANode: TcxTreeListNode;
begin
  ARowCount := TreeList.AbsoluteVisibleCount;
  ANode := TcxTreeListController(Controller).FocusedNode;
  if ANode <> nil then
    ARowIndex := ANode.VisibleIndex
  else
    ARowIndex := 0;
  if TreeList.FocusedColumn <> nil then
    ACellIndex := TreeList.FocusedColumn.VisibleIndex
  else
    ACellIndex := 0;
end;

function TcxTreeListCellNavigator.IsGroupRow(ARowIndex: Integer): Boolean;
begin
  Result := (GetNode(ARowIndex) <> nil) and GetNode(ARowIndex).IsGroupNode;
end;

procedure TcxTreeListCellNavigator.SaveCurrentNavigationColumn;
begin
  FCurrentNavigationColumn := TcxTreeListColumn(Controller.FocusedItem);
end;

procedure TcxTreeListCellNavigator.SetFocusCell(
  ARowIndex, ACellIndex: Integer; AShift: TShiftState);
var
  ANode: TcxTreeListNode;
begin
  if ARowIndex < 0 then Exit;
  ANode := GetNode(ARowIndex);
  if ANode.IsGroupNode then
    Controller.SetFocusedNodeItem(ANode, TreeList.OptionsView.GetCategorizedColumn)
  else
    Controller.SetFocusedNodeItem(ANode, TreeList.VisibleColumns[ACellIndex]);
  if FColumnChanged then
    SaveCurrentNavigationColumn;
end;

function TcxTreeListCellNavigator.FocusNextCell(AForward, ANextRow: Boolean;
  AShift: TShiftState = []): Boolean;
begin
  FColumnChanged := not ANextRow;
  Result := (TreeList.VisibleColumnCount > 0) and inherited FocusNextCell(AForward, ANextRow, AShift);
end;

procedure TcxTreeListCellNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  ANode: TcxTreeListNode;
begin
  SaveCurrentNavigationColumn;
  if TreeList.OptionsSelection.CellSelect and
    ([ssShift, ssCtrl] * Shift = []) and (TreeList.VisibleColumnCount > 0) then
  begin
    if Controller.TreeList.UseRightToLeftAlignment then
      Key := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key);
    inherited KeyDown(Key, Shift);
  end
  else
    case Key of
      VK_LEFT, VK_RIGHT:
      begin
        if TreeList.ViewInfo.HScrollSize <> 0 then
          TreeList.ScrollContent(TcxDirection(Byte(Key = VK_RIGHT) + 1));
        Key := 0;
      end;
      VK_UP, VK_DOWN:
      begin
        if TreeList.FocusedNode <> nil then
        begin
          ANode := TreeList.FocusedNode.GetNextVisibleEx(Key = VK_UP);
          if ANode <> nil then
          begin
            TreeList.SetFocusedNode(ANode, Shift);
            if TreeList.FocusedNode = ANode then
              TreeList.FocusedNode.MakeVisible;
          end;
        end;
        Key := 0;
      end;
    end;
end;

procedure TcxTreeListCellNavigator.Refresh;
begin
  inherited Refresh;
end;

function TcxTreeListCellNavigator.GetCellIndex(
  AColumn: TcxTreeListColumn; ARow: TcxTreeListBandRow): Integer;
var
  AColumnIndex: Integer;
begin
  AColumnIndex := AColumn.Position.VisibleColIndex;
  if FCurrentNavigationColumn <> nil then
    AColumnIndex := FCurrentNavigationColumn.Position.VisibleColIndex;
  if AColumnIndex < 0 then
    AColumnIndex := 0;
  Result := ARow.VisibleItems[Min(AColumnIndex, ARow.VisibleItemCount - 1)].VisibleIndex;
end;

function TcxTreeListCellNavigator.GetController: TcxTreeListController;
begin
  Result := TcxTreeListController(inherited Controller);
end;

function TcxTreeListCellNavigator.GetFocusedNode: TcxTreeListNode;
begin
  Result := TreeList.FocusedNode;
end;

function TcxTreeListCellNavigator.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(TcxTreeListController(Controller).EditingControl);
end;

function TcxTreeListCellNavigator.GoToNextCellInBand(AForward: Boolean;
  AColumn: TcxTreeListColumn; var ACellIndex: Integer): Boolean;
var
  AIndex, ARow: Integer;
  ARows: TcxTreeListBandRows;
begin
  Result := False;
  if AColumn.Position.Band = nil then
    Exit;
  ARows := AColumn.Position.Band.BandRows;
  ARow := AColumn.Position.VisibleRowIndex + cxIntOffs[AForward];
  if ARows.VisibleItemCount <= 1 then Exit;
  AIndex := ACellIndex;
  while not Result and cxInRange(ARow, 0, ARows.VisibleItemCount - 1) do
  begin
    if ARows.VisibleItems[ARow].VisibleItemCount > 0 then
    begin
      ACellIndex := GetCellIndex(AColumn, ARows.VisibleItems[ARow]);
      Result := TreeList.VisibleColumns[ACellIndex].Options.Focusing;
      if not Result then
        ACellIndex := AIndex;
    end;
    if not Result then
      Inc(ARow, cxIntOffs[AForward]);
  end;
end;

function TcxTreeListCellNavigator.GoToNextNodeCell(AForward: Boolean;
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var ACellIndex: Integer): Boolean;
var
  ARow: Integer;
  ARows: TcxTreeListBandRows;
begin
  if AColumn = nil then
    AColumn := FCurrentNavigationColumn;
  Result := (ANode <> nil) and (AColumn <> nil) and (AColumn.Position.Band <> nil);
  if Result then
  begin
    if ANode.IsGroupNode then
      ACellIndex := 0
    else
    begin
      ARows := AColumn.Position.Band.BandRows;
      Result := ARows.VisibleItemCount > 0;
      if not Result then Exit;
      ARow := cxSetValue(AForward, 0, ARows.VisibleItemCount - 1);
      ACellIndex := GetCellIndex(AColumn, ARows.VisibleItems[ARow]);
    end;
  end;
end;

{ TcxTreeListSizingDragAndDropObject }

procedure TcxTreeListSizingDragAndDropObject.BeginDragAndDrop;
begin
  FOffset := cxNullPoint;
  FDragItem := TcxTreeListController(Controller).DragItem;
  with TcxTreeListController(Controller) do
  begin
    if DragItem is TcxTreeListCustomCellViewInfo then
    begin
      if GetResizeDirection = dsdHorz then
        FOffset.X := TcxTreeListCustomCellViewInfo(DragItem).DisplayRect.Right - CurMousePos.X
      else
        FOffset.Y := TcxTreeListCustomCellViewInfo(DragItem).DisplayRect.Bottom - CurMousePos.Y
    end
    else
      if DragItem is TcxTreeListNode then
        with TcxTreeListNode(DragItem).FViewData do
          FOffset.Y := Origin.Y + Height - CurMousePos.Y;
  end;
  inherited BeginDragAndDrop;
end;

procedure TcxTreeListSizingDragAndDropObject.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
begin
  TcxTreeListController(Controller).ClickedObject := nil;
  TcxTreeListController(Controller).DragItem := FDragItem;
  inherited;
end;

function TcxTreeListSizingDragAndDropObject.GetSizingMarkBounds: TRect;
begin
  Result := inherited GetSizingMarkBounds;
  OffsetRect(Result, FOffset.X, FOffset.Y);
end;

{ TcxTreeListFindPanel }

function TcxTreeListFindPanel.GetClearButtonCaption: string;
begin
  Result := cxGetResourceString(@scxTreeListFindPanelClearButtonCaption);
end;

function TcxTreeListFindPanel.GetDefaultInfoText: string;
begin
  Result := cxGetResourceString(@scxTreeListFindPanelInfoText);
end;

function TcxTreeListFindPanel.GetFindButtonCaption: string;
begin
  Result := cxGetResourceString(@scxTreeListFindPanelFindButtonCaption);
end;

{ TcxTreeListDragAndDropBandObject }

function TcxTreeListDragAndDropBandObject.CanCustomize: Boolean;
begin
  Result := Band.Options.Customizing and OptionsCustomizing.BandCustomizing;
end;

function TcxTreeListDragAndDropBandObject.CanDrop: Boolean;

  function GetNearestColIndex(AForward: Boolean): Integer;
  begin
    Result := Band.Position.VisibleColIndex;
    if AForward then
      Inc(Result)
    else
      Dec(Result);
  end;

begin
  Result := TreeList.Bands.VisibleItemCount = 0;
  if Result or (DestBand = nil) or (DestBand = Band) then Exit;
  Result := DestBand.FixedKind = Band.FixedKind;
  if not Result and not Band.Visible and
    (Bands.GetVisibleCountByKind(Band.FixedKind) = 0) and DestBand.IsRoot then
  begin
    case Band.FixedKind of
      tlbfLeft:
        Result := (ActualDropPosition = posLeft) and
          (DestBand.VisibleRootIndex = Bands.GetFirstVisibleRootIndex(tlbfNone));
      tlbfNone:
        Result := (ActualDropPosition = posRight) and
          (DestBand.VisibleRootIndex = Bands.GetLastVisibleRootIndex(tlbfLeft)) or
          (ActualDropPosition = posLeft) and
          (DestBand.VisibleRootIndex = Bands.GetFirstVisibleRootIndex(tlbfRight));
      tlbfRight:
        Result := (ActualDropPosition = posRight) and
          (DestBand.VisibleRootIndex = Bands.GetLastVisibleRootIndex(tlbfNone));
    end;
  end;
  if not Result or not Band.Visible then Exit;
  case DropInfo.Position of
    posLeft, posRight:
      Result := (DestBand.ParentBand <> Band.ParentBand) or
        (DestBand.Position.VisibleColIndex <> GetNearestColIndex(ActualDropPosition = posLeft));
    posTop:
      Result := (DestBand.ParentBand <> Band) or (Band.FChildVisibleBands.Count > 1);
    posBottom:
      Result := (Band.ParentBand <> DestBand) or (DestBand.FChildVisibleBands.Count > 1);
  end;
end;

function TcxTreeListDragAndDropBandObject.CanRemove: Boolean;
begin
  Result := CanCustomize and (Customizing or TreeList.OptionsCustomizing.BandHiding) and
    not Band.Options.Hidden and (not Band.IsRoot or (Bands.VisibleItemCount > 1));
end;

function TcxTreeListDragAndDropBandObject.CheckBandInsertAt(
  ABand: TcxTreeListBand): Boolean;
var
  I: Integer;
begin
  Result := PtInRect(GetBoundsForInsert(ABand), DropInfo.DropPos);
  if Result then
  begin
    DropInfo.Band := ABand;
    DropInfo.Position := GetPointPosition(ABand.HeaderCell.GetClipRect, DropInfo.DropPos,
      True, TreeList.OptionsCustomizing.NestedBands);
  end
  else
    if TreeList.OptionsCustomizing.NestedBands and not ABand.IsBottom then
    begin
      for I := 0 to ABand.ChildVisibleBands.Count - 1 do
      begin
        Result := CheckBandInsertAt(TcxTreeListBand(ABand.ChildVisibleBands[I]));
        if Result then Break;
      end;
    end;
end;

procedure TcxTreeListDragAndDropBandObject.CheckDragPosition;
var
  I: Integer;
begin
  DropInfo.Band := nil;
  DropInfo.Position := posLeft;
  for I := 0 to TreeList.Bands.VisibleRootItemCount - 1 do
    if CheckBandInsertAt(TreeList.Bands.VisibleRootItems[I]) then Break;
end;

function TcxTreeListDragAndDropBandObject.Drop: Boolean;
begin
  Result := True;
  if CanDrop then
  begin
    BeforeDropPositionChange;
    Band.Position.BandIndex := GetDropParentBandIndex;
    Band.Position.ColIndex := GetDropColIndex;
    AfterDropPositionChange;
    Band.Visible := True;
  end
  else
    if (DestBand = nil) and CanRemove then
      Band.Visible := False
    else
      if Bands.VisibleItemCount = 0 then
      begin
        Band.Position.BandIndex := -1;
        Band.Visible := True;
      end
      else
        Result := False;
  if Result then
    TreeList.DoBandPosChanged(Band);
end;

function TcxTreeListDragAndDropBandObject.GetBoundsForInsert(
  ABand: TcxTreeListBand): TRect;
begin
  if (ABand = nil) or (ABand.HeaderCell = nil) then
    Result := ABand.HeaderCell.GetClipRect;
  Result := ABand.HeaderCell.GetClipRect;
  if ABand.IsRoot then
    Dec(Result.Top, cxTreeListHeaderMovingZoneSize);
  if ABand.IsBottom then
    Inc(Result.Bottom, cxTreeListHeaderMovingZoneSize);
end;

procedure TcxTreeListDragAndDropBandObject.MakeCustomizingPageVisible;
begin
  if Band.ActuallyVisible then
    TreeList.Customizing.FHeaderListBox.ItemIndex := -1;
  TreeList.Customizing.MakeBandPageVisible;
end;

procedure TcxTreeListDragAndDropBandObject.AfterDropPositionChange;
var
  I: Integer;
begin
  case DropPosition of
    posTop:
      begin
        DestBand.Position.BandIndex := Band.Index;
        DestBand.Position.ColIndex := 0;
      end;
    posBottom:
      begin
        I := 0;
        while I < DestBand.ChildVisibleBands.Count do
          if DestBand.ChildVisibleBands[I] <> Band then
            tcxTreeListBand(DestBand.ChildVisibleBands[I]).Position.BandIndex := Band.Index
          else
            Inc(I);
      end;
  end;
end;

procedure TcxTreeListDragAndDropBandObject.BeforeDropPositionChange;
var
  AColIndex, I: Integer;
begin
  if (DropPosition <> posBottom) and (GetDropParentBandIndex = Band.Index) then
  begin
    AColIndex := Band.Position.ColIndex;
    for I := Band.ChildBandCount - 1 downto 0 do
      with Band.ChildBands[I].Position do
      begin
        BandIndex := Band.Position.BandIndex;
        ColIndex := AColIndex;
      end;
  end;
end;

function TcxTreeListDragAndDropBandObject.GetBand: TcxTreeListBand;
begin
  Result := TcxTreeListBandHeaderCellViewInfo(DragHeader).Band;
end;

function TcxTreeListDragAndDropBandObject.GetDropParentBandIndex: Integer;
begin
  if DropPosition = posBottom then
    Result := DestBand.Index
  else
  begin
    if DestBand = nil then
      Result := -1
    else
      Result := DestBand.Position.BandIndex;
  end;
end;

function TcxTreeListDragAndDropBandObject.GetDropColIndex: Integer;
begin
  if DropPosition = posBottom then
    Result := 0
  else
  begin
    if DestBand = nil then
      Result := 0
    else
      Result := DestBand.Position.ColIndex;
    if ActualDropPosition = posRight then Inc(Result);
    if Band.Position.ColIndex < Result then Dec(Result);
  end;
end;

{ TcxTreeListDragAndDropColumnObject }

function TcxTreeListDragAndDropColumnObject.CanCustomize: Boolean;
begin
  Result := Column.Options.Customizing and
    OptionsCustomizing.ColumnCustomizing;
end;

function TcxTreeListDragAndDropColumnObject.CanDrop: Boolean;
var
  AIndex: Integer;
begin
  Result := DropInfo.Accepted and (DropPosition <> posNone);
  if Result and ChangeColumnOnly then
  begin
    AIndex := DropInfo.ColIndex - Column.Position.VisibleColIndex;
    Result := (AIndex < 0) or (AIndex > 1) or not Column.Visible;
  end;
end;

function TcxTreeListDragAndDropColumnObject.CanRemove: Boolean;
begin
  Result := CanCustomize and (Customizing or OptionsCustomizing.ColumnHiding) and
    not Column.Options.Hidden and (TreeList.VisibleColumnCount > 1);
end;

procedure TcxTreeListDragAndDropColumnObject.CheckDropColumnIndex(
  ARow: TcxTreeListBandRow; var AColIndex: Integer);
begin
  if AColIndex < ARow.VisibleItemCount then
    AColIndex := ARow.VisibleItems[AColIndex].Position.ColIndex
  else
    AColIndex := ARow.Count + 1;
end;

function TcxTreeListDragAndDropColumnObject.ChangeColumnOnly: Boolean;
begin
  Result := not DropInfo.InsertRow and (DestBand = Column.Position.Band) and
    (DropInfo.RowIndex = Column.Position.RowIndex);
end;

function TcxTreeListDragAndDropColumnObject.CheckBandDropArea(
  ABand: TcxTreeListBand): Boolean;
var
  I: Integer;
begin
  DropInfo.Area := cxRectSetTop(ABand.HeaderCell.GetClipRect,
    ABand.HeaderCell.DisplayRect.Bottom,
    ViewInfo.HeadersHeight + cxTreeListHeaderMovingZoneSize);
  Result := PtInRect(DropInfo.Area, DropInfo.DropPos) and
    ((ABand <> Column.Position.Band) or not Column.Visible or (ABand.VisibleColumnCount > 1));
  if not Result then Exit;
  DropInfo.Band := ABand;
  Dec(DropInfo.Area.Bottom, cxTreeListHeaderMovingZoneSize);
  DropInfo.ColIndex := 0;
  DropInfo.Position := posBottom;
  DropInfo.InsertRow := True;
  DropInfo.RowIndex := ABand.BandRows.Count;
  if ABand.BandRows.VisibleItemCount = 0 then
    DropInfo.Position := posTop
  else
  begin
    if DropInfo.Area.Bottom > DropInfo.DropPos.Y then
    begin
      DropInfo.Area.Bottom := DropInfo.Area.Top;
      for I := 0 to ABand.BandRows.VisibleItemCount - 1 do
      begin
        Inc(DropInfo.Area.Bottom, ABand.BandRows.VisibleItems[I].LineCount * ViewInfo.HeaderLineHeight);
        DropInfo.RowIndex := ABand.BandRows.VisibleItems[I].Index + 1;
        if DropInfo.Area.Bottom > DropInfo.DropPos.Y then Break;
      end;
    end;
    Result := Result and OptionsCustomizing.StackedColumns;
    if not Result then
      DropInfo.Position := posNone;
  end;
end;

procedure TcxTreeListDragAndDropColumnObject.CheckDragPosition;
var
  I: Integer;
const
  HorzPos: array[Boolean] of TcxPosition = (posRight, posLeft);
begin
  DropInfo.InsertRow := False;
  if HitTest.HitAtBandContainer or HitTest.HitAtBandHeader then
    DropInfo.Band := HitTest.HitBand
  else
    DropInfo.Band := nil;
  DropInfo.Position := posNone;
  DropInfo.Accepted := HitTest.HitAtColumnHeader;
  if DropInfo.Accepted then
    InitDropInfoFromColumn(HitTest.HitColumn)
  else
    for I := 0 to Bands.BottomItemCount - 1 do
    begin
      DropInfo.Accepted := CheckBandDropArea(Bands.BottomItems[I]);
      if DropInfo.Accepted then Break;
    end;
  if DropInfo.Accepted and (DropInfo.Position = posNone) then
  begin
    DropInfo.Position := HorzPos[DropInfo.DropPos.X <= cxRectCenter(DropInfo.Area).X];
    if (ActualDropPosition = posRight) then
      Inc(DropInfo.ColIndex);
  end;
  DropInfo.Accepted := DropInfo.Position <> posNone;
  if (Column.Position.Band <> nil) and not CheckOnlyOwnColumns then
  begin
    DropInfo.Accepted := False;
    DropInfo.Band := nil;
  end;
end;

function TcxTreeListDragAndDropColumnObject.CheckOnlyOwnColumns: Boolean;
begin
  Result := DropInfo.Band <> nil;
  if Result then
  begin
    if Column.Position.Band.IsOnlyOwnColumns then
      Result := DropInfo.Band = Column.Position.Band
    else
      Result := (DropInfo.Band <> nil) and not DropInfo.Band.IsOnlyOwnColumns;
  end;
end;

function TcxTreeListDragAndDropColumnObject.CreateDropInfo: TcxTreeListDropInfo;
begin
  Result := TcxTreeListColumnDropInfo.Create;
end;

function TcxTreeListDragAndDropColumnObject.Drop: Boolean;
var
  AColumn: TcxTreeListColumn;
begin
  Result := True;
  AColumn := Column;
  if ((DestBand = nil) or DropInfo.Customizing) and CanRemove then
    AColumn.Visible := False
  else
    if (DestBand <> nil) and (TreeList.OptionsCustomizing.StackedColumns or not DropInfo.InsertRow)  then
    begin
      AColumn.Position.BandIndex := DestBand.Index;
      if DropInfo.InsertRow then
        AColumn.Position.Row := DestBand.BandRows.Insert(DropInfo.RowIndex)
      else
        AColumn.Position.RowIndex := DropInfo.RowIndex;
      CheckDropColumnIndex(AColumn.Position.Row, DropInfo.ColIndex);
      if DropInfo.ColIndex > AColumn.Position.ColIndex then
        Dec(DropInfo.ColIndex);
      AColumn.Position.ColIndex := DropInfo.ColIndex;
      AColumn.Visible := True;
      DestBand.AdjustSubItems;
    end
    else
      Result := False;
  if Result then
    TreeList.DoColumnPosChanged(AColumn);
end;

function TcxTreeListDragAndDropColumnObject.GetArrowAreaBounds: TRect;
begin
  Result := DropInfo.Area;
end;

procedure TcxTreeListDragAndDropColumnObject.InitDropInfoFromColumn(
  AColumn: TcxTreeListColumn);
begin
  DropInfo.Band := AColumn.Position.Band;
  DropInfo.ColIndex := AColumn.Position.VisibleColIndex;
  DropInfo.RowIndex := AColumn.Position.RowIndex;
  DropInfo.Area := AColumn.HeaderCell.GetClipRect;
end;

procedure TcxTreeListDragAndDropColumnObject.MakeCustomizingPageVisible;
begin
  if Column.ActuallyVisible then
    TreeList.Customizing.FHeaderListBox.ItemIndex := -1;
  TreeList.Customizing.MakeColumnPageVisible;
end;

function TcxTreeListDragAndDropColumnObject.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumnHeaderCellViewInfo(DragHeader).Column;
end;

function TcxTreeListDragAndDropColumnObject.GetDropInfo: TcxTreeListColumnDropInfo;
begin
  Result := TcxTreeListColumnDropInfo(inherited DropInfo);
end;

{ TcxTreeListDragAndDropObject }

constructor TcxTreeListDragAndDropObject.Create(AControl: TcxControl);
begin
  if AControl is TcxTreeListItemsCustomizeListBox then
    inherited Create(TcxTreeListItemsCustomizeListBox(AControl).TreeList)
  else
    inherited Create(AControl);

  FOrigin := dxMapWindowPoint(AControl.Handle, TreeList.Handle, FOrigin);
  OrgOffset := FOrigin;
  FArrows := TcxPlaceArrows.CreateArrows(TreeList.OptionsView.DropArrowColor, clBtnText);
  FDropInfo := CreateDropInfo;
  if TreeList.IsDesigning then
    DragHeader.Pressed := True;
end;

destructor TcxTreeListDragAndDropObject.Destroy;
begin
  FArrows.Free;
  FDropInfo.Free;
  inherited Destroy;
end;

procedure TcxTreeListDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  InitializeScrollArea(ViewInfo.HScrollArea);
  if Customizing then
    MakeCustomizingPageVisible;
end;

procedure TcxTreeListDragAndDropObject.ChangeArrowsPosition;
const
  Position2Border: array[TcxPosition] of TcxBorder =
    (bLeft, bLeft, bRight, bTop, bBottom);
begin
  if DrawArrowsNeeded then
  begin
    Arrows.MoveTo(cxRectOffset(GetArrowAreaBounds, TreeList.ClientToScreen(Point(0, 0))), Position2Border[DropPosition]);
    Arrows.Visible := True;
  end
  else
    Arrows.Visible := False;
end;

function TcxTreeListDragAndDropObject.CheckInCustomizing(
  const APoint: TPoint): Boolean;
begin
  Result := TreeList.Customizing.PtInCustomizingBox(
    TreeList.ClientToScreen(APoint));
end;


function TcxTreeListDragAndDropObject.CreateDropInfo: TcxTreeListDropInfo;
begin
  Result := TcxTreeListDropInfo.Create;
end;

procedure TcxTreeListDragAndDropObject.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
var
  APos: TPoint;
begin
  APos := cxPointOffset(P, FOrigin);
  TreeList.Controller.ClickedObject := nil;
  InitDropInfo(APos);
  ChangeArrowsPosition;
  Accepted := DropInfo.Customizing or (DropInfo.Band <> nil) or (Bands.VisibleItemCount = 0);
  inherited DragAndDrop(APos, Accepted);
end;

procedure TcxTreeListDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  DragHeader.Pressed := False;
  try
    DropInfo.Accepted := Accepted and (DropInfo.Accepted or DropInfo.Customizing or
      CanRemove) and not TreeList.Controller.DragCancel;
    inherited EndDragAndDrop(DropInfo.Accepted);
    if DropInfo.Accepted then
    begin
      TreeList.BeginUpdate;
      if not Drop then
        TreeList.CancelUpdate
      else
      begin
        TreeList.EndUpdate;
        TreeList.Modified;
      end;
    end;
  finally
    Dirty := False;
    TreeList.Controller.DragItem := nil;
  end;
end;

function TcxTreeListDragAndDropObject.GetActualDropPosition: TcxPosition;
begin
  Result := DropPosition;
  if TreeList.UseRightToLeftAlignment then
    case Result of
      posRight:
        Result := posLeft;
      posLeft:
        Result := posRight;
    end;
end;

function TcxTreeListDragAndDropObject.GetArrowAreaBounds: TRect;
begin
  Result := cxNullRect;
  if not CanDrop then Exit;
  if Bands.VisibleItemCount = 0 then
    Result := cxRectSetHeight(ViewInfo.Bounds, ViewInfo.FBandHeaderLineHeight)
  else
  begin
    Result := DestBand.HeaderCell.GetClipRect;
    case DropPosition of
      posRight:
        Result.Left := Result.Right;
      posBottom:
        Result.Top := Result.Bottom;
    end;
  end;
end;

function TcxTreeListDragAndDropObject.GetDisplayRect: TRect;
begin
  Result := cxRectOffset(DragHeader.BoundsRect, FOrigin);
end;

function TcxTreeListDragAndDropObject.GetDragAndDropCursor(
  Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crArrow
  else
    if CanRemove then
      Result := crcxRemove
    else
      Result := crcxNoDrop;
end;

procedure TcxTreeListDragAndDropObject.InitDropInfo(const P: TPoint);
begin
  HitTest.HitPoint := P;
  DropInfo.DropPos := P;
  DropInfo.Customizing := CanCustomize and CheckInCustomizing(P);
  CheckDragPosition;
  DropInfo.Accepted := CanDrop;
end;

procedure TcxTreeListDragAndDropObject.InitializeScrollArea(const Area: TRect);
begin
  if cxRectIsNull(Area) then Exit;
  AddAutoScrollingObject(cxRectSetLeft(Area,
    Area.Left, cxtlScrollDelta), sbHorizontal, TreeList.Controller.GetRealScrollCode(scLineUp));
  AddAutoScrollingObject(cxRectSetRight(Area,
    Area.Right, cxtlScrollDelta), sbHorizontal, TreeList.Controller.GetRealScrollCode(scLineDown));
end;

procedure TcxTreeListDragAndDropObject.Paint;
var
  ADone: Boolean;
  APrevState: TcxButtonState;
  ABorders: TcxBorders;
  ANeighbors: TcxNeighbors;
  AMargins: TRect;
begin
  APrevState := DragHeader.State;
  ABorders := DragHeader.Borders;
  try
    DragHeader.FState := cxbsDefault;
    DragHeader.FBorders := cxBordersAll;
    AMargins := DragHeader.BordersMargins;
    DragHeader.BordersMargins := cxNullRect;
    ANeighbors := DragHeader.FNeighbors;
    DragHeader.FNeighbors := [];
    Canvas.WindowOrg := DragHeader.DisplayRect.TopLeft;
    DragHeader.BeforeCustomDraw(Canvas);
    Painter.CustomDrawCell(Canvas, DragHeader, ADone);
    DragHeader.AfterCustomDraw(Canvas);
    if not ADone then
      DragHeader.Draw(Canvas);
  finally
    DragHeader.BordersMargins := AMargins;
    DragHeader.FNeighbors := ANeighbors;
    DragHeader.FBorders := ABorders;
    DragHeader.FState := APrevState;
  end;
end;

function TcxTreeListDragAndDropObject.DrawArrowsNeeded: Boolean;
begin
  Result := CanDrop and not DropInfo.Customizing;
end;

function TcxTreeListDragAndDropObject.GetBands: TcxTreeListBands;
begin
  Result := TreeList.Bands;
end;

function TcxTreeListDragAndDropObject.GetCustomizing: Boolean;
begin
  Result := TreeList.Customizing.Visible;
end;

function TcxTreeListDragAndDropObject.GetDestBand: TcxTreeListBand;
begin
  Result := DropInfo.Band;
end;

function TcxTreeListDragAndDropObject.GetDragHeader: TcxTreeListHeaderCellViewInfo;
begin
  Result := TreeList.Controller.DragItem as TcxTreeListHeaderCellViewInfo;
end;

function TcxTreeListDragAndDropObject.GetDropPosition: TcxPosition;
begin
  Result := DropInfo.Position;
end;

function TcxTreeListDragAndDropObject.GetHitTest: TcxTreeListHitTest;
begin
  Result := TreeList.HitTest;
end;

function TcxTreeListDragAndDropObject.GetOptionsCustomize: TcxTreeListOptionsCustomizing;
begin
  Result := TreeList.OptionsCustomizing;
end;

function TcxTreeListDragAndDropObject.GetPainter: TcxTreeListPainter;
begin
  Result := TcxTreeListPainter(TreeList.Painter);
end;

function TcxTreeListDragAndDropObject.GetTreeList: TcxCustomTreeList;
begin
  Result := Control as TcxCustomTreeList;
end;

function TcxTreeListDragAndDropObject.GetViewInfo: TcxTreeListViewInfo;
begin
  Result := TreeList.ViewInfo;
end;

{ TcxCustomTreeList }

constructor TcxCustomTreeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAbsoluteItems := TList.Create;
  FAbsoluteVisibleItems := TList.Create;
  ControlStyle := ControlStyle + [csDisplayDragImage];
  Width := 250;
  Height := 150;
  FRoot := GetRootClass.Create(Self);
  if Assigned(DesignerNavigatorProc) then
    DesignerNavigatorProc(Self, True);
  FChanges := [tcStructure];
end;

destructor TcxCustomTreeList.Destroy;
begin
  DataController.Cancel;
  BeginUpdate;
  DoClear;
  ViewInfo.Clear;
  Customizing.Visible := False;
  if Assigned(DesignerNavigatorProc) then
    DesignerNavigatorProc(Self, False);
  FreeAndNil(FRoot);
  FAbsoluteItems.Free;
  FAbsoluteVisibleItems.Free;
  inherited Destroy;
end;

procedure TcxCustomTreeList.Assign(Source: TPersistent);
begin
  if Source is TcxCustomTreeList then
  begin
    BeginUpdate;
    try
      DefaultRowHeight := TcxCustomTreeList(Source).DefaultRowHeight;
      AssignData(Source as TcxCustomTreeList);
      AssignOptions(Source as TcxCustomTreeList);
      DataController.Assign(TcxCustomTreeList(Source).DataController);
      Styles.Assign(TcxCustomTreeList(Source).DataController);
      AddChanges([tcStructure..tcSelection]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCustomTreeList.AdjustColumnsWidth;
var
  I: Integer;
  APrevValue: Boolean;
begin
  APrevValue := OptionsView.ColumnAutoWidth;
  OptionsView.ColumnAutoWidth := True;
  BeginUpdate;
  try
    for I := 0 to VisibleColumnCount - 1 do
      VisibleColumns[I].Width := VisibleColumns[I].DisplayWidth;
    for I := 0 to Bands.VisibleItemCount - 1 do
      Bands.VisibleItems[I].Width := Bands.VisibleItems[I].DisplayWidth;
  finally
    EndUpdate;
  end;
  OptionsView.ColumnAutoWidth := APrevValue;
end;

procedure TcxCustomTreeList.ApplyBestFit;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Bands.VisibleItemCount - 1 do
      Bands.VisibleItems[I].ApplyBestFit;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.Cancel;
begin
  CancelEdit;
  DataController.Cancel;
  RefreshNavigatorButtons;
  Controller.EditingController.UpdateEditValue;
  Controller.CheckEdit;
end;

procedure TcxCustomTreeList.Clear;
begin
  BeginUpdate;
  try
    CheckChanges;
    DoClear;
    if Root.Count > 0 then
      Root.DeleteChildren;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.DoBandSizeChanged(ABand: TcxTreeListBand);
begin
  Modified;
  LayoutChanged;
  DoOnBandSizeChanged(ABand);
end;

procedure TcxCustomTreeList.DoColumnSizeChanged(AColumn: TcxTreeListColumn);
begin
  Modified;
  DoOnColumnSizeChanged(AColumn);
end;

function TcxCustomTreeList.DoCreateColumn: TcxTreeListColumn;

  procedure SetColumnName(AColumn: TcxTreeListColumn);
  var
    I: Integer;
    AGeneratedName: string;
  begin
    if Owner <> nil then
      AGeneratedName := CreateUniqueName(Owner, Self, AColumn, scxTLPrefixName, '', ColumnCount)
    else
    begin
      AGeneratedName := GetTreeListColumnClass.ClassName;
      for I := Length(AGeneratedName) downto 1 do
        if dxCharInSet(AGeneratedName[I], ['A'..'Z']) then
        begin
          Delete(AGeneratedName, 1, I - 1);
          Break;
        end;
    end;
    AColumn.Name := AGeneratedName;
  end;

begin
  Result := GetTreeListColumnClass.Create(Owner);
  Result.EditingControl := Self;
  SetColumnName(Result);
  Modified;
  UpdateDesignerForms;
end;

function TcxCustomTreeList.CreateColumn(
  ABand: TcxTreeListBand = nil): TcxTreeListColumn;
begin
  BeginUpdate;
  try
    Result := DoCreateColumn;
    ClearCalculatedWidths;
    if ABand = nil then
      ABand := GetDefaultColumnContainer;
    Result.Position.BandIndex := ABand.Index;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.DeleteAllColumns;
begin
  BeginUpdate;
  try
    ViewInfo.SetDirty;
    DataController.DestroyHandles;
    while ColumnCount > 0 do
      Columns[ColumnCount - 1].Free;
  finally
    FNextStoreID := 0;
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.DeleteSelection;
begin
  DoDeleteSelection;
end;

procedure TcxCustomTreeList.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Source <> Self then
  begin
    if Assigned(OnDragDrop) then
      OnDragDrop(Self, Source, X, Y);
    Controller.DragDrop(Source, X, Y);
    Exit;
  end;
  HitTest.HitPoint := Point(X, Y);
  try
    if Assigned(OnDragDrop) then
      OnDragDrop(Self, Source, X, Y);
    Controller.DragDrop(Source, X, Y);
    FocusedNode := DragNode;
  finally
    FDragNode := nil;
    if FocusedNode <> nil then
      FocusedNode.MakeVisible;
  end;
end;

procedure TcxCustomTreeList.Edit;
begin
  if OptionsData.Editing then
    DataController.Edit;
end;

procedure TcxCustomTreeList.MakeDefaultLayout;
var
  I: Integer;
begin
  BeginUpdate;
  try
    OptionsView.Bands := False;
    while Bands.Count > 1 do
      Bands[Bands.Count - 1].Free;
    if Bands.Count = 0 then Bands.Add;
    Bands[0].RestoreDefaults;
    Bands[0].Visible := True;
    for I := 0 to ColumnCount - 1 do
      with Columns[I] do
      begin
        RestoreDefaults;
        Position.BandIndex := 0;
      end;
  finally
    EndUpdate;
    Modified;
    FDefaultLayout := True;
  end;
end;

procedure TcxCustomTreeList.Post;
begin
  DataController.Post;
end;

procedure TcxCustomTreeList.FullCollapse;
begin
  SetGlassCursor;
  try
    Root.Collapse(True);
  finally
    RestoreCursor;
  end;
end;

procedure TcxCustomTreeList.FullExpand;
begin
  SetGlassCursor;
  try
    Root.Expand(True);
  finally
    RestoreCursor;
  end;
end;

procedure TcxCustomTreeList.FullRefresh;
begin
  AddChanges([tcStructure..tcFocusedNode]);
  LayoutChanged;
end;

function TcxCustomTreeList.Find(AData: Pointer;
  AStart: TcxTreeListNode; AExpandedOnly, AForward: Boolean;
  AFilter: TcxTreeListFindFunc; AIgnoreStartNode: Boolean = False): TcxTreeListNode;
begin
  Result := cxFind(Self, AData, AStart, AExpandedOnly, AForward, AFilter, AIgnoreStartNode);
end;

type
  PFindTextInfo = ^TFindTextInfo;
  TFindTextInfo = record
    Text: string;
    Column: TcxTreeListColumn;
    CaseSensitive: Boolean;
    Mode: TcxTreeListFindMode;
    LikeParams: TcxTreeListLikeParams;
    IgnoreStartNode: Boolean;
  end;

function FindTextFilter(ANode: TcxTreeListNode; AData: Pointer): Boolean;
var
  I: Integer;
  ANodeText: string;
  APercent, AUnderline: Char;
begin
  with PFindTextInfo(AData)^ do
  begin
    if Column <> nil then
      ANodeText := ANode.Texts[Column.ItemIndex]
    else
    begin
      ANodeText := '';
      for I := 0 to ANode.ValueCount - 1 do
        ANodeText := ANodeText + ANode.Texts[I];
    end;
    if not CaseSensitive then
    begin
      ANodeText := AnsiUpperCase(ANodeText);
      Text := AnsiUpperCase(Text);
    end;
    case Mode of
      tlfmLike:
        begin
          if LikeParams = nil then
          begin
            APercent := '%';
            AUnderline := '_';
          end
          else
          begin
            APercent := LikeParams.Percent;
            AUnderline := LikeParams.UnderLine;
          end;
          Result := LikeStr(ANodeText, Text,
            APercent, AUnderLine);
        end;
      tlfmExact:
        begin
          Result := ANodeText = Text;
        end;
    else
      Result := AnsiStrPos(PChar(ANodeText), PChar(Text)) <> nil;
    end;
  end;
end;

function TcxCustomTreeList.FindNodeByText(const AText: string;
  AColumn: TcxTreeListColumn; AStartNode: TcxTreeListNode = nil;
  AExpandedOnly: Boolean = False; AForward: Boolean = True;
  ACaseSensitive: Boolean = True; AMode: TcxTreeListFindMode = tlfmNormal;
  ALikeParams: TcxTreeListLikeParams = nil; AIgnoreStartNode: Boolean = False): TcxTreeListNode;
var
  AFindInfo: TFindTextInfo;
begin
  AFindInfo.Text := AText;
  AFindInfo.Column := AColumn;
  AFindInfo.CaseSensitive := ACaseSensitive;
  AFindInfo.Mode := AMode;
  AFindInfo.LikeParams := ALikeParams;
  AFindInfo.IgnoreStartNode := AIgnoreStartNode;
  Result := Find(@AFindInfo, AStartNode, AExpandedOnly, AForward, FindTextFilter,
    AIgnoreStartNode);
end;

procedure TcxCustomTreeList.SetFocusedNode(
  ANode: TcxTreeListNode; AShift: TShiftState);
begin
  if ANode = Root then
    ANode := nil;
  if ANode <> nil then
    Controller.Select(ANode, AShift)
  else
    Controller.FocusedNode := ANode;
  if OptionsBehavior.AlwaysShowEditor then
    Controller.SetFocusedRecordItem(TdxNativeInt(ANode), FocusedColumn);
end;

procedure TcxCustomTreeList.SelectAll;
begin
  if (not OptionsSelection.MultiSelect and (Controller.FocusedNode <> nil)) or (AbsoluteVisibleCount = 0) then Exit;
  BeginUpdate;
  try
    if Controller.FocusedNode = nil then
      Controller.FocusedNode := AbsoluteVisibleItems[0];
    if OptionsSelection.MultiSelect then
      DoSelectAll;
    AddChanges([tcSelection]);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.CancelEdit;
begin
  Controller.EditingController.HideEdit(False);
end;

procedure TcxCustomTreeList.HideEdit;
begin
  Controller.EditingController.HideEdit(True);
end;

procedure TcxCustomTreeList.Select(Node: TcxTreeListNode;
  ShiftState: TShiftState = []);
begin
  Controller.Select(Node, ShiftState);
end;

procedure TcxCustomTreeList.Select(const ANodes: array of TcxTreeListNode);
var
  I: Integer;
begin
  BeginUpdate;
  try
    ClearSelection;
    for I :=  Low(ANodes) to High(ANodes) do
      ANodes[I].Selected := True;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.Select(ANodesList: TList);
begin
  BeginUpdate;
  try
    ClearSelection;
    if (ANodesList <> nil) and (ANodesList.Count > 0) then
      SelectionList.Assign(ANodesList);
    DoSelectionChanged;
  finally
    EndUpdate;
  end
end;

procedure TcxCustomTreeList.Deselect(Node: TcxTreeListNode);
begin
  Node.Selected := False;
end;

procedure TcxCustomTreeList.ShowEdit;
begin
  if not OptionsData.Editing then Exit;
  Edit;
  Controller.EditingController.ShowEdit();
end;

procedure TcxCustomTreeList.ShowEditByKey(AKey: Char);
begin
  BeforeShowingEdit;
  with Controller do
    EditingController.ShowEdit(FocusedItem, AKey);
end;

procedure TcxCustomTreeList.ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
begin
  BeforeShowingEdit;
  with Controller do
    EditingController.ShowEdit(FocusedItem, AShift, X, Y);
end;

procedure TcxCustomTreeList.RestoreFromIniFile(const AStorageName: string;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreTreeListName: string = '');
begin
  RestoreFrom(stIniFile, AStorageName, nil, AChildrenCreating,
    AChildrenDeleting, ARestoreTreeListName);
end;

procedure TcxCustomTreeList.RestoreFromRegistry(const AStorageName: string;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreTreeListName: string = '');
begin
  RestoreFrom(stRegistry, AStorageName, nil, AChildrenCreating,
    AChildrenDeleting, ARestoreTreeListName);
end;

procedure TcxCustomTreeList.RestoreFromStream(AStream: TStream;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreTreeListName: string = '');
begin
  RestoreFrom(stStream, '', AStream, AChildrenCreating,
    AChildrenDeleting, ARestoreTreeListName);
end;

procedure TcxCustomTreeList.StoreToIniFile(AStorageName: string;
  AReCreate: Boolean = True; const ASaveTreeListName: string = '');
begin
  StoreTo(stIniFile, AStorageName, nil, AReCreate, ASaveTreeListName);
end;

procedure TcxCustomTreeList.StoreToRegistry(AStorageName: string;
  AReCreate: Boolean = True; const ASaveTreeListName: string = '');
begin
  StoreTo(stRegistry, AStorageName, nil, AReCreate, ASaveTreeListName);
end;

procedure TcxCustomTreeList.StoreToStream(AStream: TStream;
  const ASaveTreeListName: string = '');
begin
  StoreTo(stStream, '', AStream, True, ASaveTreeListName);
end;

function TcxCustomTreeList.CellRect(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TRect;
var
  AViewInfo: TcxTreeListEditCellViewInfo;
begin
  AViewInfo := ViewInfo.GetEditCell(ANode, AColumn);
  if AViewInfo <> nil then
    Result := cxRectOffset(AViewInfo.ClipRect, ANode.ViewData.Origin)
  else
    Result := cxInvalidRect;
end;

// searching
function TcxCustomTreeList.FindNext(AForward: Boolean): Boolean;
var
  ANode: TcxTreeListNode;
const
  ALocateKeys: array[Boolean] of Word = (VK_UP, VK_DOWN);
begin
  if Searching then
  begin
    ANode := FocusedNode;
    Controller.IncSearchKeyDown(ALocateKeys[AForward], [ssCtrl]);
  end
  else
    ANode := nil;
  Result := ANode <> FocusedNode;
end;

procedure TcxCustomTreeList.CancelSearching;
begin
  Controller.CancelIncSearching;
end;

function TcxCustomTreeList.GetEditRect(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TRect;
var
  AViewInfo: TcxTreeListEditCellViewInfo;
begin
  AViewInfo := ViewInfo.GetEditCell(ANode, AColumn);
  if AViewInfo <> nil then
    Result := cxRectOffset(AViewInfo.EditRect, ANode.ViewData.Origin)
  else
    Result := cxInvalidRect;
end;

function TcxCustomTreeList.GetNodeAt(
  X, Y: Integer): TcxTreeListNode;
begin
  HitTest.ReCalculate(cxPoint(X, Y));
  if HitTest.HitAtNode then
    Result := HitTest.HitNode
  else
    Result := nil;
end;

procedure TcxCustomTreeList.CopyAllToClipboard;
begin
  DoWriteToClipboard(False);
end;

procedure TcxCustomTreeList.CopySelectedToClipboard(AVisibleOnly: Boolean = False);
begin
  DoWriteToClipboard(True, AVisibleOnly);
end;

procedure TcxCustomTreeList.RestoreColumnsDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ColumnCount - 1 do
      Columns[I].RestoreDefaults;
    RestoreColumnsWidths;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.RestoreColumnsWidths;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ColumnCount - 1 do
      Columns[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.ApplyFindFilterText(const AText: string);
begin
  Controller.ApplyFindFilterText(AText);
end;

procedure TcxCustomTreeList.ClearFindFilterText;
begin
  Controller.ClearFindFilterText;
end;

function TcxCustomTreeList.GetFindFilterText: string;
begin
  Result := Controller.GetFindFilterText;
end;

procedure TcxCustomTreeList.HideFindPanel;
begin
  Controller.HideFindPanel;
end;

function TcxCustomTreeList.IsFindPanelVisible: Boolean;
begin
  Result := Controller.IsFindPanelVisible;
end;

procedure TcxCustomTreeList.ShowFindPanel;
begin
  Controller.ShowFindPanel;
end;

procedure TcxCustomTreeList.Subselect(Node: TcxTreeListNode);
begin
  if not CanMultiSelect then
    cxTreeListError(cxGetResourceString(@scxMultiSelectRequired))
  else
    Node.Selected := True;
end;

procedure TcxCustomTreeList.ClearSelection(KeepPrimary: Boolean = False);
begin
  Controller.CancelSelection(KeepPrimary)
end;

procedure TcxCustomTreeList.ClearSorting;
begin
  Sorted := False;
end;

function TcxCustomTreeList.ColumnByName(
  const AName: string): TcxTreeListColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ColumnCount - 1 do
    if CompareText(Columns[I].Name, AName) = 0 then
    begin
      Result := Columns[I];
      Break;
    end;
end;

function TcxCustomTreeList.GetSelections(AList: TList): TcxTreeListNode;
begin
  AList.Clear;
  if OptionsSelection.MultiSelect then
    AList.Assign(SelectionList)
  else
    if FocusedNode <> nil then
      AList.Add(FocusedNode);
  Result := FocusedNode;
end;

procedure TcxCustomTreeList.GotoBOF;
begin
  SetFocusedVisibleNode(Root.GetFirstChild);
end;

procedure TcxCustomTreeList.GotoEOF;
begin
  SetFocusedVisibleNode(cxGetLatest(Root.GetLastChild));
end;

procedure TcxCustomTreeList.GotoNext;
begin
  if not IsEOF and (FocusedNode <> nil) then
    SetFocusedVisibleNode(FocusedNode.GetNextVisible);
end;

procedure TcxCustomTreeList.GotoNextPage;
begin
  if FocusedNode <> nil then
  begin
    Controller.DoNextPage(True, []);
    Update;
  end;
end;

procedure TcxCustomTreeList.GotoPrev;
begin
  if not IsBOF and (FocusedNode <> nil) then
    SetFocusedVisibleNode(FocusedNode.GetPrevVisible);
end;

procedure TcxCustomTreeList.GotoPrevPage;
begin
  if FocusedNode <> nil then
  begin
    Controller.DoNextPage(False, []);
    Update;
  end;
end;

function TcxCustomTreeList.IsBOF: Boolean;
begin
  Result := (FocusedNode = nil) or (FocusedNode.GetPrevVisible = nil);
end;

function TcxCustomTreeList.IsEOF: Boolean;
begin
  Result := (FocusedNode = nil) or (FocusedNode.GetNextVisible = nil);
end;

procedure TcxCustomTreeList.BeginGestureScroll(APos: TPoint);
begin
  inherited BeginGestureScroll(APos);
  Controller.EditingItem := nil;
  ViewInfo.UpdatePixelScrollTopNodeAndOffsetMaxValues;
end;

function TcxCustomTreeList.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick;
  if Result and IsFocused and IsEditing and HitTest.HitAtNavigator then
    Result := False;
end;

procedure TcxCustomTreeList.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);

  function ScaleHeight(AValue: Integer): Integer;
  begin
    if AValue > 0 then
      Result := Max(1, MulDiv(AValue, M, D))
    else
      Result := AValue;
  end;

var
  I: Integer;
begin
  BeginUpdate;
  try
    inherited ChangeScaleEx(M, D, isDpiChange);
    DefaultRowHeight := ScaleHeight(DefaultRowHeight);
    Bands.ChangeScale(M, D);
    for I := 0 to ColumnCount - 1 do
      Columns[I].ChangeScale(M, D);
    for I := 0 to AbsoluteCount - 1 do
      AbsoluteItems[I].Height := ScaleHeight(AbsoluteItems[I].Height);
    Customizing.ChangeScale(M, D);
    Preview.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.CheckCreateDesignSelectionHelper;
begin
end;

procedure TcxCustomTreeList.ControlUpdateData(AInfo: TcxUpdateControlInfo);
begin
  if IsRefreshFields then Exit;
  inherited ControlUpdateData(AInfo);
  ViewInfo.Invalidate(True);
  RefreshNavigatorButtons;
end;

procedure TcxCustomTreeList.DataChanged;
begin
  ViewInfo.SetDirty;
  inherited DataChanged;
  AddChanges([tcData, tcSummary]);
  LayoutChanged;
end;

procedure TcxCustomTreeList.DataLayoutChanged;
begin
  if IsRefreshFields then Exit;
  inherited DataLayoutChanged;
  AddChanges([tcData, tcSummary]);
  LayoutChanged;
end;

function cxCompareColumnsVisibleOrder(AColumn1, AColumn2: TcxTreeListColumn): Integer;
begin
  Result := Weights[AColumn1.Position.Band.FixedKind] - Weights[AColumn2.Position.Band.FixedKind];
  if Result <> 0 then Exit;
  Result := AColumn1.Position.Band.VisibleIndex - AColumn2.Position.Band.VisibleIndex;
  if Result = 0 then
  begin
    Result := AColumn1.Position.VisibleRowIndex - AColumn2.Position.VisibleRowIndex;
    if Result = 0 then
    begin
      Result := AColumn1.Position.VisibleColIndex - AColumn2.Position.VisibleColIndex;
      if Result = 0 then
        Result := AColumn1.VisibleIndex - AColumn2.VisibleIndex;
    end;
  end;
end;

procedure TcxCustomTreeList.DoLayoutChanged;
begin
  IsCancelOperation := False;
  CheckChanges;
  Bands.RefreshInformation;
  ViewInfo.State[cvis_StyleInvalid] := True;
  Bands.Adjust;
  FVisibleColumns.Sort(@cxCompareColumnsVisibleOrder);
  CalculateForceIndentWidth;
  ConditionalFormattingProvider.Recalculate;
  inherited DoLayoutChanged;
  UpdateDesignerForms;
  DoLayoutChangedEvent;
  DoChanged();
  RefreshNavigatorButtons;
end;

function TcxCustomTreeList.DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean;
begin
  Result := PopupMenus.DoShowPopupMenu(Point(X, Y));
  if not Result then
    Result := inherited DoShowPopupMenu(AMenu, X, Y);
end;

procedure TcxCustomTreeList.InitScrollBarsParameters;
begin
  if ([csLoading, csDestroying, csUpdating, csReading] * ComponentState <> [])
    or DataController.IsLoading or (Root = nil) then Exit;
  ViewInfo.InitScrollBarsParameters;
end;

procedure TcxCustomTreeList.InitScrollBarsParametersCache;
begin
  if ([csLoading, csDestroying, csUpdating, csReading] * ComponentState <> [])
    or DataController.IsLoading or (Root = nil) then Exit;
  ViewInfo.InitScrollBarsParametersCache;
end;

procedure TcxCustomTreeList.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
  ViewInfo.ScrollContentByGesture(AScrollKind, ADelta);
end;

function TcxCustomTreeList.CanScrollContentByGestureWithoutScrollBars: Boolean;
begin
  Result := cxIsTouchModeEnabled;
end;

function TcxCustomTreeList.GetStatusHint(const APoint: TPoint): string;
begin
  Result := Controller.GetStatusHint(APoint);
end;

function TcxCustomTreeList.InternalCollapseNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean;
begin
  Result := False;
end;

function TcxCustomTreeList.InternalExpandNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean;
begin
  Result := False;
end;

procedure TcxCustomTreeList.AssignItemsPosition(
  ABandIndexOnly: Boolean);
var
  I: Integer;
  AList: TList;
begin
  IgnoreLoadingStatus := not IsLoading;
  AList := TList.Create;
  try
    // align bands
    for I := 0 to Bands.Count - 1 do
      AList.Add(Bands[I]);
    if (Bands.Count > 0) and (Bands[0].FIndex <> Bands[Bands.Count - 1].FIndex) then
    begin
      AList.Sort(@cxCompareBandsByPosition);
      for I := 0 to AList.Count - 1 do
        TcxTreeListBand(AList[I]).Index := TcxTreeListBand(AList[I]).FIndex;
    end
    else
      AList.Sort(@cxCompareBandsByPositionEx);
    for I := 0 to AList.Count - 1 do
      TcxTreeListBand(AList[I]).Position.Restore;
    // align columns
    AList.Assign(ColumnsList);
    AList.Sort(@cxCompareColumnsByPosition);
    for I := 0 to ColumnCount - 1 do
      TcxTreeListColumn(AList[I]).Position.Restore(ABandIndexOnly);
  finally
    AList.Free;
  end;
end;

procedure TcxCustomTreeList.ReadState(Reader: TReader);
begin
  SaveItemsPosition(True);
  inherited ReadState(Reader);
  AssignItemsPosition(True);
end;

procedure TcxCustomTreeList.SaveItemsPosition(ABandIndexOnly: Boolean);
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    Columns[I].Position.Store(ABandIndexOnly);
  for I := 0 to Bands.Count - 1 do
    Bands[I].Position.Store;
end;

procedure TcxCustomTreeList.Updated;
begin
  inherited Updated;
  AssignItemsPosition(False);
end;

procedure TcxCustomTreeList.Updating;
begin
  SaveItemsPosition(False);
  inherited Updating;
end;

function TcxCustomTreeList.DragDropImageDisplayRect: TRect;
begin
  with GetDragDropViewParams do
  begin
    Result := inherited DragDropImageDisplayRect;
    Result.Bottom := cxTextHeight(Font);
    Result.Right := Max(VisibleColumns[0].DisplayWidth,
      cxTextWidth(GetDragDropViewParams.Font, GetDragDropText) + ScaleFactor.Apply(cxTextOffset) * 2);
    Result := cxRectOffset(Result, [ScreenToClient(GetMouseCursorPos), cxTreeListDragDropTextAreaOffset]);
    Result := cxRectInflate(Result, ScaleFactor.Apply(cxTextOffset));
  end;
end;

procedure TcxCustomTreeList.DrawDragDropImage(ADragBitmap: TBitmap; ACanvas: TcxCanvas);
var
  R: TRect;
begin
  cxApplyViewParams(ACanvas, GetDragDropViewParams);
  R := Rect(0, 0, ADragBitmap.Width, ADragBitmap.Height);
  ACanvas.FillRect(R);
  ACanvas.FrameRect(R, clBlack, 1, cxBordersAll);
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(GetDragDropText, cxTextRect(R), cxAlignLeft or cxAlignVCenter or cxSingleLine);
end;

function TcxCustomTreeList.GetDragInfoImagePosition(APoint: TPoint): TPoint;
var
  ADestNodeBounds: TRect;
begin
  ADestNodeBounds := HitTest.HitNode.ViewData.GetRealBounds;
  if not ViewInfo.IsRightToLeftConverted then
    Result := ClientToScreen(Point(ADestNodeBounds.Left + ViewInfo.IndicatorWidth, cxRectCenter(ADestNodeBounds).Y))
  else
    Result := ClientToScreen(Point(ADestNodeBounds.Right - ViewInfo.IndicatorWidth - Controller.FDragNodeInfoImage.Image.Width,
      cxRectCenter(ADestNodeBounds).Y));
end;

function TcxCustomTreeList.GetDragDropText: string;
begin
  Result := FDragNode.Texts[VisibleColumns[0].ItemIndex];
  if Assigned(FOnGetDragDropText) then
    FOnGetDragDropText(Self, FDragNode, Result);
end;

function TcxCustomTreeList.GetDragDropViewParams: TcxViewParams;
begin
  Result := Styles.GetSelectionParams
end;

function TcxCustomTreeList.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := nil;
end;

function TcxCustomTreeList.StartDrag(DragObject: TDragObject): Boolean;
begin
  Result := DragNode <> nil;
end;

procedure TcxCustomTreeList.GetStoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  AChildren.AddObject('', Bands);
  for I := 0 to ColumnCount - 1 do
    AChildren.AddObject('', Columns[I]);

  AChildren.AddObject('ConditionalFormattingProvider', ConditionalFormattingProvider);
end;

function TcxCustomTreeList.GetStoredObjectName: string;
begin
  if FStoringName = '' then
    Result := Name
  else
    Result := FStoringName;
end;

function TcxCustomTreeList.GetStoredObjectProperties(
  AProperties: TStrings): Boolean;
begin
  AProperties.Add('ColumnsQuickCustomizationSorted');
  AProperties.Add('BandsQuickCustomizationSorted');
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TcxCustomTreeList.GetStoredPropertyValue(
  const AName: string; var AValue: Variant);
begin
  if AName = 'ColumnsQuickCustomizationSorted' then
    AValue := OptionsCustomizing.ColumnsQuickCustomizationSorted
  else
    if AName = 'BandsQuickCustomizationSorted' then
      AValue := OptionsCustomizing.BandsQuickCustomizationSorted;
  if Assigned(OnGetStoredPropertyValue) then
    OnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxCustomTreeList.SetStoredPropertyValue(
  const AName: string; const AValue: Variant);
begin
  if AName = 'ColumnsQuickCustomizationSorted' then
    OptionsCustomizing.ColumnsQuickCustomizationSorted := AValue
  else
    if AName = 'BandsQuickCustomizationSorted' then
      OptionsCustomizing.BandsQuickCustomizationSorted := AValue;
  if Assigned(OnSetStoredPropertyValue) then
    OnSetStoredPropertyValue(Self, AName, AValue);
end;

function TcxCustomTreeList.StoredCreateChild(
  const AObjectName, AClassName: string): TObject;
begin
  if AClassName = GetTreeListColumnClass.ClassName then
  begin
    Result := CreateColumn;
    if FStoringName = '' then
      TcxTreeListColumn(Result).Name := AObjectName;
    if Assigned(OnInitStoredObject) then
      FOnInitStoredObject(Self, Result);
  end
  else
    Result := nil;
end;

procedure TcxCustomTreeList.StoredDeleteChild(
  const AObjectName: string; AObject: TObject);
begin
  if AObject <> Bands then
    AObject.Free;
end;

procedure TcxCustomTreeList.GetChildren(
  Proc: TGetChildProc; Root: TComponent);

  procedure DoStore(AColumn: TcxTreeListColumn);
  begin
    if AColumn.Owner = Root then
      Proc(AColumn);
  end;

var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to ColumnCount - 1 do DoStore(Columns[I]);
end;

function TcxCustomTreeList.IsUpdating: Boolean;
begin
  Result := csUpdating in ComponentState;
end;

procedure TcxCustomTreeList.Loaded;

  procedure ConvertOldFooterSummaries;
  var
    I: Integer;
  begin
    for I := 0 to ColumnCount - 1 do
      Columns[I].ConvertOldFooterSummaries;
  end;

begin
  FPostChanged := True;
  try
    inherited Loaded;
    FSortedColumns.Sort(@cxCompareColumnsBySortIndex);
    if FocusedNode <> nil then
      FocusedNode.MakeVisible;
    AssignItemsPosition(False);
    ConvertOldFooterSummaries;
    ConditionalFormattingProvider.Loaded;
    DataChanged;
    StructureChanged;
    ForceLayoutChanged;
  finally
    FPostChanged := False;
  end;
end;

procedure TcxCustomTreeList.Modified;
begin
  if HandleAllocated then
    inherited Modified;
  FDefaultLayout := False;
end;

procedure TcxCustomTreeList.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if PopupMenus <> nil then
    PopupMenus.Notification(AComponent, Operation);
  if not IsDestroying then
  begin
    if (Summary <> nil) and (Operation = opRemove) and (AComponent is TcxTreeListColumn) then
      Summary.Notification(AComponent, Operation);
  end;
  CheckImageListReferences(AComponent, Operation);
end;

procedure TcxCustomTreeList.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  UpdateDesignerForms;
end;

procedure TcxCustomTreeList.AddChanges(AChanges: TcxTreeListChanges);
begin
  FChanges := FChanges + AChanges;
end;

function TcxCustomTreeList.AddNode(ANode, ARelative: TcxTreeListNode;
  AData: Pointer; AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode;
begin
  Result := AddNodeInternal(ANode, ARelative, AttachMode);
  Result.FData := AData;
  if ARelative = nil then
    ARelative := Root;
  Exclude(ARelative.State, nsValidIndexes);
end;

function TcxCustomTreeList.AddNodeInternal(ANode, ARelative: TcxTreeListNode;
  AddMethod: TcxTreeListNodeAttachMode): TcxTreeListNode;
const
  IsAddChild: array[TcxTreeListNodeAttachMode] of Boolean =
    (False, False, True, True, False);
  AddMode: array[TcxTreeListNodeAttachMode] of TcxTreeListNodeAddMode =
    (tlnaAdd, tlnaAddFirst, tlnaAdd, tlnaAddFirst, tlnaInsert);
begin
  if ANode = nil then
    Result := CreateNode
  else
    Result := ANode;
  if (ARelative = nil) and (AddMethod = tlamInsert) then
    AddMethod := tlamAdd;
  if ARelative <> nil then
    SetNodeParent(Result, ARelative, AddMode[AddMethod])
  else
    SetNodeParent(Result, FRoot, AddMode[AddMethod]);
  if AddMode[AddMethod] = tlnaInsert then
    Result.FOriginalIndex := Result.Index
  else
    if Result.FOriginalIndex < 0 then
      Result.FOriginalIndex := Result.Parent.Count;
  DoDataChangedEvent(nil);
  LayoutChanged;
end;

procedure TcxCustomTreeList.AfterMouseDown(AButton: TMouseButton; X, Y: Integer);
begin
  if (DragMode = dmAutomatic) and (AButton = mbLeft) and MouseCapture and
    (not IsDesigning or AllowAutoDragAndDropAtDesignTime(X, Y, [])) and
    CanDrag(X, Y) and (cxDragDetect(Handle, ClientToScreen(Point(X, Y))) = ddDrag) then
    BeginDrag(True);
  if AButton = mbMiddle then DoScrolling;
end;

procedure TcxCustomTreeList.AssignOptions(ASource: TcxCustomTreeList);
begin
  Font := ASource.Font;
  Styles := ASource.Styles;
  Images := ASource.Images;
  StateImages := ASource.StateImages;
  OptionsCustomizing := ASource.OptionsCustomizing;
  OptionsView := ASource.OptionsView;
  OptionsBehavior := ASource.OptionsBehavior;
  OptionsSelection := ASource.OptionsSelection;
end;

procedure TcxCustomTreeList.AssignData(ASource: TcxCustomTreeList);
var
  I: Integer;
  AName: string;
  AColumn: TcxTreeListColumn;
begin
  Bands := ASource.Bands;
  SaveItemsPosition(False);
  while ContainerList.Count > ASource.ContainerList.Count do
    TComponent(ContainerList.Last).Free;
  while ContainerList.Count < ASource.ContainerList.Count do
  begin
    AName := TComponent(ASource.ContainerList[ContainerList.Count]).Name;
    AColumn := DoCreateColumn;
    if (Owner = nil) or (Owner.FindComponent(AName) = nil) then
      AColumn.Name := AName;
  end;
  for I := 0 to Min(ContainerList.Count, ASource.ContainerList.Count) - 1 do
    TcxTreeListColumn(ContainerList[I]).Assign(TPersistent(ASource.ContainerList[I]));
  AssignItemsPosition(False);
  Preview := ASource.Preview;
end;

procedure TcxCustomTreeList.BeforeUpdate;
begin
  inherited BeforeUpdate;
  CheckChanges;
end;

procedure TcxCustomTreeList.BiDiModeChanged;
begin
  inherited BiDiModeChanged;
  Customizing.BiDiModeChanged;
end;

procedure TcxCustomTreeList.BoundsChanged;
begin
  AddChanges([tcLayout]);
  inherited BoundsChanged;
end;

procedure TcxCustomTreeList.CalculateForceIndentWidth;
var
  ABand: TcxTreeListBand;
  AColumn: TcxTreeListColumn;
  I, AForceDelta, W: Integer;
begin
  if Bands.IsUpdateLocked then Exit;
  ABand := Bands.ExpandableBand;
  if (ABand = nil) or IsLoading then Exit;
  AForceDelta := ABand.BandRows.RowMinWidth + IndentWidth - ABand.DisplayWidth;
  W := ABand.Width;
  if AForceDelta > 0 then
    Bands.ExpandableBand.ForceWidth(Bands.ExpandableBand.DisplayWidth + AForceDelta);
  for I := 0 to ABand.BandRows.VisibleItemCount - 1 do
  begin
    AColumn := ABand.BandRows.VisibleItems[I].VisibleItems[0];
    AForceDelta := AColumn.MinWidth + IndentWidth - AColumn.DisplayWidth;
    if AForceDelta > 0 then
      AColumn.ForceWidth(AColumn.DisplayWidth + AForceDelta);
  end;
  if W = 0 then
    ABand.FWidth := W;
end;

function TcxCustomTreeList.CanUseMultiThreadedSorting: Boolean;
begin
  Result := OptionsData.MultiThreadedSorting <> bFalse;
end;

function TcxCustomTreeList.CanCompare: Boolean;
begin
  Result := (SortedColumnCount > 0) or Assigned(FOnCompare);
end;

procedure TcxCustomTreeList.CheckChanges;
var
  ASaveChanges: TcxTreeListChanges;
begin
  if (Changes = []) or ChangesLocked or IsLoading or IsDestroying or ViewInfo.CalculateInProcess then Exit;
  BeginUpdate;
  try
    ChangesLocked := True;
    try
      ASaveChanges := Changes;
      if tcColumns in Changes then
        Bands.RefreshInformation;
      if tcSortOrder in Changes then
        Include(FChanges, tcData);
      if tcData in Changes then
      begin
        DoFilterNodes;
        DoSortNodes;
      end;
      if (tcStructure in Changes) or (DataController.NodesCount <> FAbsoluteItems.Count)  then
        CheckStructure;
      if SelectionChanges * Changes <> [] then
        CheckSelectionAndFocused;
      if tcSummary in Changes then
        Summary.Calculate;
      if tcImages in Changes then
        CheckLevelsInfo;
      CheckEvents;
      if Changes = ASaveChanges then
        Changes := ASaveChanges * SelectionChanges
      else
      begin
        Changes := Changes - [tcData..tcColumns, tcSummary];
        if tcLoading in Changes then
          Changes := Changes - [tcStructure, tcSortOrder];
      end;
      ChangesLocked := False;
    except
      ChangesLocked := False;
      FChanges := [];
      raise;
    end;
  finally
    if ((Changes - SelectionChanges - [tcLoading]) <> []) then
    begin
      CheckChanges;
      EndUpdate;
    end
    else
      CancelUpdate;
  end;
end;

procedure TcxCustomTreeList.CheckEvents;
begin
  if Controller.SelectionLockCount > 0 then
    Exit;
  if not IsDestroying then
  begin
    if (tcFocusedNode in Changes) and
      ((Controller.PrevFocusedNode <> FocusedNode) or (FocusedNode = nil)) then
    begin
      if Controller.FocusedNode = nil then
        Controller.CheckFocusedNode;
      DoFocusedNodeChanged(Controller.PrevFocusedNode, FocusedNode);
      Controller.FPrevFocusedNode := FocusedNode;
    end;
    if [tcSelection{, tcFocusedNode}] * Changes <> [] then
      DoSelectionChanged;
  end;
  Exclude(FChanges, tcFocusedNode);
  Exclude(FChanges, tcSelection);
end;

procedure TcxCustomTreeList.CheckImageListReferences(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  if FDefaultLevelInfo <> nil then
    FDefaultLevelInfo.Notification(AComponent, Operation);
  if FLevelsInfo <> nil then
    for I := 0 to FLevelsInfo.Count - 1 do
      LevelsInfo[I].Notification(AComponent, Operation);
end;

procedure TcxCustomTreeList.CheckLevelsInfo;
var
  AOffset, I: Integer;
begin
  FDefaultIndentSize.cy := Max(ViewInfo.DefaultCellHeight,
    LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor) + ScaleFactor.Apply(cxTreeListIndentOffsetSize));
  FDefaultIndentSize.cx := LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor) + ScaleFactor.Apply(cxTreeListIndentOffsetSize);
  dxAdjustToTouchableSize(FDefaultIndentSize, ScaleFactor);
  BeginUpdate;
  try
    for I := 0 to FExpansionLevel do
    begin
      if I = FLevelsInfo.Count then
        FLevelsInfo.Add(TcxTreeListLevelInfo.Create(Self));
      DoGetLevelImages(LevelsInfo[I], I);
      if not OptionsView.DynamicIndent then
        FDefaultIndentSize.cx := Max(FDefaultIndentSize.cx, LevelsInfo[I].Size.cx);
      if not OptionsView.CellAutoHeight then
        FDefaultIndentSize.cy := Max(FDefaultIndentSize.cy, LevelsInfo[I].Size.cy);
    end;
    for I := FExpansionLevel + 1 to FLevelsInfo.Count - 1 do
      FLevelsInfo[I].Free;
    FLevelsInfo.Count := FExpansionLevel + 1;
    AOffset := 0;
    for I := 0 to FExpansionLevel do
    begin
      AOffset := LevelsInfo[I].Update(AOffset);
      if (I = 0) and not OptionsView.ShowRoot then
      begin
        Dec(LevelsInfo[I].FWidth, DefaultIndentSize.cx);
        Dec(AOffset, DefaultIndentSize.cx);
      end;
    end;
  finally
    FChanges := FChanges - [tcImages];
    CancelUpdate;
  end;
end;

procedure TcxCustomTreeList.CheckSelectionAndFocused;
begin
  if (Controller.FocusedNode = nil) and cxInRange(
    Controller.FocusedNodeIndex, 0, FAbsoluteVisibleItems.Count - 1) then
    TcxTreeListNode(FAbsoluteVisibleItems[Controller.FocusedNodeIndex]).Focused := True;
end;

procedure TcxCustomTreeList.CheckSortOrderList;
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
     Columns[I].FSortIndex := FSortedColumns.IndexOf(Columns[I])
end;

procedure TcxCustomTreeList.CheckStructure;
var
  ANode: TcxTreeListNode;
  ALevel, AHiddenLevel: Integer;
begin
  if DataController.IsFilterProcessing then
    Exit;
  ALevel := 0;
  AHiddenLevel := 0;
  FExpansionLevel := -1;
  FAbsoluteItems.Count := 0;
  FAbsoluteVisibleItems.Count := 0;
  if IsDestroying or (Root = nil) or (DataController = nil) then Exit;
  FAbsoluteItems.Capacity := Max(FAbsoluteItems.Capacity, DataController.NodesCount);
  FAbsoluteVisibleItems.Capacity := Max(FAbsoluteVisibleItems.Capacity, DataController.NodesCount);
  ANode := Root.FFirst;
  Include(FChanges, tcImages);
  while ANode <> nil do
  begin
    ANode.FAbsoluteIndex := FAbsoluteItems.Count;
    FAbsoluteItems.Add(ANode);
    ANode.FVisibleIndex := -1;
    if (AHiddenLevel = 0) and ANode.Visible and not ANode.HiddenByFilter then
    begin
      ANode.FVisibleIndex := FAbsoluteVisibleItems.Add(ANode);
      FExpansionLevel := Max(FExpansionLevel, ALevel);
    end;
    if ANode.FFirst <> nil then
    begin
      Inc(ALevel);
      if (AHiddenLevel > 0) or (nsCollapsed in ANode.State) or not ANode.Visible or ANode.HiddenByFilter then
        Inc(AHiddenLevel);
      ANode := ANode.FFirst;
    end
    else
      if ANode.FNext <> nil then
        ANode := ANode.FNext
      else
      begin
        while (ANode <> Root) and (ANode.FNext = nil) do
        begin
          if AHiddenLevel > 0 then
            Dec(AHiddenLevel);
          Dec(ALevel);
          ANode := ANode.Parent;
        end;
        if ANode <> Root then
          ANode := ANode.FNext
        else
          ANode := nil;
      end;
  end;
  CheckLevelsInfo;
  Exclude(FChanges, tcStructure);
end;

procedure TcxCustomTreeList.ClearCalculatedWidths;
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    Columns[I].FCalculatedWidth := 0;
  for I := 0 to Bands.Count - 1 do
    Bands[I].FCalculatedWidth := 0;
end;

procedure TcxCustomTreeList.ColumnSortOrderChanged(
  AColumn: TcxTreeListColumn; AShift: TShiftState);
var
  AIndex: Integer;
begin
  BeginUpdate;
  try
    if ssCtrl in AShift then
      AColumn.CancelSorting
    else
    begin
      if not IsLoading and (not OptionsBehavior.MultiSort or not (ssShift in AShift)) then
      begin
        for AIndex := 0 to ColumnCount - 1 do
          if Columns[AIndex] <> AColumn then
            Columns[AIndex].CancelSorting;
      end;
      if FSortedColumns.IndexOf(AColumn) < 0 then
      begin
        AIndex := FSortedColumns.Add(AColumn);
        if not IsLoading then
          AColumn.FSortIndex := AIndex;
      end
      else
        if AColumn.SortOrder = soNone then
          FSortedColumns.Remove(AColumn);
    end;
    FChanges := FChanges + [tcData, tcSortOrder];
  finally
    EndUpdate;
  end;
end;

function TcxCustomTreeList.CreateNode: TcxTreeListNode;
begin
  Result := TcxUnboundTreeListNode.Create(Self);
  Include(FChanges, tcStructure);
end;

procedure TcxCustomTreeList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FDefaultLevelInfo := TcxTreeListLevelInfo.Create(Self);
  FLevelsInfo := TcxObjectList.Create();
  FNavigatorNotifier := TcxNavigatorControlNotifier.Create;
  FSortedColumns := TList.Create;
  FDesigners := TList.Create;
  FSelectionList := TList.Create;
  FOptionsCustomizing := GetOptionsCustomizingClass.Create(Self);
  FOptionsSelection := GetOptionsSelectionClass.Create(Self);
  FPopupMenus := TcxTreeListPopupMenus.Create(Self);
  FPreview := GetPreviewClass.Create(Self);
  FBands := TcxTreeListBands.Create(Self);
  FVisibleColumns := TList.Create;
  FCustomizing := GetTreeListCustomizingClass.Create(Self);
  FSummary := GetSummaryClass.Create(Self);
  FDelayTimer := TTimer.Create(Self);
  FDelayTimer.Enabled := False;
  FDelayTimer.Interval := 0;
  FDelayTimer.OnTimer := DoChangedTimer;
  FConditionalFormattingProvider := TcxTreeListConditionalFormattingProvider.Create(Self);
end;

procedure TcxCustomTreeList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  ConditionalFormattingProvider.DefineBinaryProperty(Filer);
end;

procedure TcxCustomTreeList.DestroySubClasses;
begin
  UpdateDesignerForms;
  BeginUpdate;
  try
    FreeAndNil(FConditionalFormattingProvider);
    FreeAndNil(FNavigatorNotifier);
    FreeAndNil(FPreview);
    DeleteAllColumns;
    FreeAndNil(FBands);
    FreeAndNil(FPopupMenus);
    FreeAndNil(FOptionsCustomizing);
    FreeAndNil(FOptionsSelection);
    FreeAndNil(FCustomizing);
    FreeAndNil(FSortedColumns);
    FreeAndNil(FSummary);
    FreeAndNil(FDefaultLevelInfo);
    FreeAndNil(FLevelsInfo);
    FreeAndNil(FDelayTimer);
    FreeAndNil(FSelectionList);
    FreeAndNil(FDesigners);
    FreeAndNil(FOptionsCustomizing);
    FreeAndNil(FVisibleColumns);
  finally
    inherited DestroySubClasses;
  end;
end;

procedure TcxCustomTreeList.DoAfterSummary;
begin
  if Assigned(FOnAfterSummary) then
    FOnAfterSummary(Self);
end;

procedure TcxCustomTreeList.DoBandHeaderClick(ABand: TcxTreeListBand);
begin
  if Assigned(FOnBandHeaderClick) then
    FOnBandHeaderClick(Self, ABand);
end;

procedure TcxCustomTreeList.DoBandPosChanged(ABand: TcxTreeListBand);
begin
  if Assigned(FOnBandPosChanged) then
    FOnBandPosChanged(Self, ABand);
end;

function TcxCustomTreeList.DoBeginDragNode(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginDragNode) then
    FOnBeginDragNode(Self, ANode, Result);
end;

function TcxCustomTreeList.DoCanFocusNode(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := True;
  if Assigned(ANode) and not ANode.Focused and Assigned(FOnCanFocusNode) then
    FOnCanFocusNode(Self, ANode, Result);
end;

function TcxCustomTreeList.DoCanNodeCollapse(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.HasChildren;
  if Result and Assigned(FOnCollapsing) then
    FOnCollapsing(Self, ANode, Result);
end;

function TcxCustomTreeList.DoCanNodeExpand(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := ANode.HasChildren;
  if Result and Assigned(FOnExpanding) then
    FOnExpanding(Self, ANode, Result);
end;

function TcxCustomTreeList.DoCanSelectNode(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := True;
  if (ANode <> nil) and Assigned(FOnCanSelectNode) then
    FOnCanSelectNode(Self, ANode, Result);
end;

procedure TcxCustomTreeList.DoChanged(BlockChangedEvent: Boolean = True);
begin
  if IsLoading or IsDestroying or (BlockChangedEvent and (OptionsBehavior.ChangeDelay > 0)) or FPostChanged then Exit;
  try
    FPostChanged := True;
    if FPostChanged and Assigned(FOnChange) then
      FOnChange(Self);
  finally
    FPostChanged := False;
  end;
end;

procedure TcxCustomTreeList.DoColumnHeaderClick(AColumn: TcxTreeListColumn);
begin
  if Assigned(FOnColumnHeaderClick) then
    FOnColumnHeaderClick(Self, AColumn);
end;

procedure TcxCustomTreeList.DoColumnPosChanged(AColumn: TcxTreeListColumn);
begin
  if Assigned(FOnColumnPosChanged) then
    FOnColumnPosChanged(Self, AColumn);
end;

procedure TcxCustomTreeList.DoCompare(
  ANode1, ANode2: TcxTreeListNode; var ACompare: Integer);
const
  AEquals: array[Boolean] of Integer = (1, 0);
begin
  ACompare := AEquals[ANode1 = ANode2];
  if (ACompare <> 0) and (ANode1.Inserting or ANode2.Inserting) then
  begin
    if ANode2.Inserting then
      ACompare := -1;
    Exit;
  end;
  if not Assigned(FOnCompare) then
  begin
    if ACompare <> 0 then
      ACompare := DataController.CompareNodesByColumns(ANode1, ANode2, FSortedColumns)
  end
  else
    FOnCompare(Self, ANode1, ANode2, ACompare);
end;

procedure TcxCustomTreeList.DoCustomizationVisibleChanged;
begin
  if Assigned(FOnCustomizationVisibleChanged) then
    FOnCustomizationVisibleChanged(Self);
end;

procedure TcxCustomTreeList.DoDataChangedEvent(Sender: TObject);
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);
end;

procedure TcxCustomTreeList.DoDeletion(ANode: TcxTreeListNode);
begin
  if Assigned(FOnDeletion) then
    FOnDeletion(Self, ANode);
end;

procedure TcxCustomTreeList.DoEditValueChanged(
  AItem: TcxCustomInplaceEditContainer);
begin
  inherited DoEditValueChanged(AItem);
  DoChanged();
end;

procedure TcxCustomTreeList.DoExpand(ANode: TcxTreeListNode);
begin
end;

function TcxCustomTreeList.DoFilterNodeEvent(ANode: TcxTreeListNode): Boolean;
begin
  Result := True;
  if HasFilterEvent then
    FOnFilterNode(Self, ANode, Result);
end;

procedure TcxCustomTreeList.DoFilterNodes;
begin
  DataController.FilterNodes;
end;

procedure TcxCustomTreeList.DoFocusedItemChanged(
  APrevFocusedItem, AFocusedItem: TcxCustomInplaceEditContainer);
begin
  if Assigned(FOnFocusedColumnChanged) then
    FOnFocusedColumnChanged(Self, TcxTreeListColumn(APrevFocusedItem), TcxTreeListColumn(AFocusedItem));
end;

procedure TcxCustomTreeList.DoFocusedNodeChanged(
  APrevNode, AFocusedNode: TcxTreeListNode);
begin
  if Assigned(FOnFocusedNodeChanged) then
    FOnFocusedNodeChanged(Self, APrevNode, AFocusedNode);
end;

procedure TcxCustomTreeList.DoGetCellHint(ACell: TObject; var AText: string;
  var ANeedShow: Boolean);
begin
  if Assigned(FOnGetCellHint) then
    FOnGetCellHint(Self, ACell, AText, ANeedShow);
  if ANeedShow and (ACell is TcxCustomViewInfoItem) then
    cxScreenCanvas.Font.Assign(TcxCustomViewInfoItem(ACell).ViewParams.Font);
end;

procedure TcxCustomTreeList.DoGetLevelImages(
  AInfo: TcxTreeListLevelInfo; ALevel: Integer);
var
  AImages, AStateImages: TCustomImageList;
begin
  AImages := Images;
  AStateImages := StateImages;
  if Assigned(FOnGetLevelImages) then
    FOnGetLevelImages(Self, ALevel, AImages, AStateImages);
  AInfo.Images := AImages;
  AInfo.StateImages := AStateImages;
  AInfo.Update(0);
end;

procedure TcxCustomTreeList.DoGetNodeHeight(
  ANode: TcxTreeListNode; var AHeight: Integer);
begin
  if Assigned(FOnGetNodeHeight) then
    FOnGetNodeHeight(Self, ANode, AHeight)
end;

function TcxCustomTreeList.DoGetNodeImageIndex(
  ANode: TcxTreeListNode; AIndex: TcxTreeListImageIndexType): TcxImageIndex;
begin
  LockChanges := True;
  try
    if ANode = nil then
      Result := -1
    else
    begin
      Result := ANode.FImageIndexes[AIndex];
      if (AIndex = tlitSelectedIndex) and OptionsView.UseImageIndexForSelected then
        Result := ANode.FImageIndexes[tlitImageIndex];
      if Assigned(OnGetNodeImageIndex) then
        OnGetNodeImageIndex(Self, ANode, AIndex, Result);
    end;
  finally
    LockChanges := False;
  end;
end;

procedure TcxCustomTreeList.DoGetPreviewHeight(
  ANode: TcxTreeListNode; var AHeight: Integer);
begin
  if Assigned(FOnGetNodePreviewHeight) then
    FOnGetNodePreviewHeight(Self, ANode, ANode.Values[Preview.Column.ItemIndex], AHeight);
end;

procedure TcxCustomTreeList.DoHotTrackNode(
  ANode: TcxTreeListNode; AShift: TShiftState; var ACursor: TCursor);
begin
  ACursor := Cursor;
  if Assigned(FOnHotTrackNode) then
    FOnHotTrackNode(Self, ANode, AShift, ACursor);
end;

procedure TcxCustomTreeList.DoInplaceEditContainerItemAdded(AItem: TcxCustomInplaceEditContainer);
begin
  inherited DoInplaceEditContainerItemAdded(AItem);
  TcxTreeListColumn(AItem).StoreID := FNextStoreID;
  Inc(FNextStoreID);
end;

procedure TcxCustomTreeList.DoInplaceEditContainerItemRemoved(AItem: TcxCustomInplaceEditContainer);
begin
  if TcxTreeListColumn(AItem).StoreID = FNextStoreID - 1 then
    Dec(FNextStoreID);
  inherited DoInplaceEditContainerItemRemoved(AItem);
end;

function TcxCustomTreeList.DoIsGroupNode(
  ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode <> nil) and
    (OptionsView.PaintStyle = tlpsCategorized) and ANode.HasChildren;
  if Assigned(FOnIsGroupNode) then
    FOnIsGroupNode(Self, ANode, Result);
end;

procedure TcxCustomTreeList.DoLayoutChangedEvent;
begin
  if Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
end;

procedure TcxCustomTreeList.DoLeftPosChanged;
begin
  if Assigned(FOnLeftPosChanged) then
    FOnLeftPosChanged(Self);
end;

function TcxCustomTreeList.DoMoveToEvent(AttachNode: TcxTreeListNode;
  AttachMode: TcxTreeListNodeAttachMode; Nodes: TList; var IsCopy: Boolean): Boolean;
begin
  Result := False;
  if Assigned(FOnMoveTo) then
    FOnMoveTo(Self, AttachNode, AttachMode, Nodes, IsCopy, Result);
end;

procedure TcxCustomTreeList.DoNodeChanged(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
begin
  if DataController.IsLoading or IsLocked then
  begin
    Include(FChanges, tcData);
    Exit;
  end;
  if DataController.EditingNode <> ANode then
  begin
    if AColumn <> nil then
      Include(FChanges, tcData)
    else
      Include(FChanges, tcStructure);
  end
  else
    Summary.Calculate;
  LayoutChanged;
  if Assigned(FOnNodeChanged) then
    FOnNodeChanged(Self, ANode, AColumn);
end;

procedure TcxCustomTreeList.DoNodeCheckChanged(
  ANode: TcxTreeListNode; AState: TcxCheckBoxState);
begin
  if Assigned(FOnNodeCheckChanged) then
    FOnNodeCheckChanged(Self, ANode, AState);
end;

function TcxCustomTreeList.DoNodeCheckChanging(ANode: TcxTreeListNode; AState: TcxCheckBoxState): Boolean;
begin
  Result := True;
  if Assigned(FOnNodeCheckChanging) then
    FOnNodeCheckChanging(Self, ANode, AState, Result);
end;

procedure TcxCustomTreeList.DoNodeCollapsed(ANode: TcxTreeListNode);
begin
  if Assigned(FOnCollapsed) then
    FOnCollapsed(Self, ANode);
end;

procedure TcxCustomTreeList.DoNodeExpanded(ANode: TcxTreeListNode);
begin
  if Assigned(FOnExpanded) then
    FOnExpanded(Self, ANode);
end;

procedure TcxCustomTreeList.DoSetNodeFocused(
  ANode: TcxTreeListNode; AValue: Boolean; Shift: TShiftState = []);
begin
  if AValue then
    FocusedNode := ANode
  else
    if ANode.Focused then FocusedNode := nil;
end;

procedure TcxCustomTreeList.RefreshFields;
var
  I: Integer;
begin
  ViewInfo.SetDirty;
  for I := 0 to ContainerList.Count - 1 do
    TcxTreeListColumn(ContainerList[I]).DataChanged;
end;

procedure TcxCustomTreeList.SetImageList(
  const AValue: TCustomImageList; var AFieldValue: TCustomImageList; const AChangeLink: TChangeLink);
var
 AImages: TCustomImageList;
begin
  if AValue = AFieldValue then Exit;
  AImages := AFieldValue;
  cxSetImageList(AValue, AFieldValue, AChangeLink, Self);
  if AImages = nil then Exit;
  if csDestroying in AImages.ComponentState then
    Notification(AImages, opRemove)
  else
    if csDestroying in ComponentState then
      CheckImageListReferences(AImages, opRemove)
    else
       AImages.FreeNotification(Self);
end;

procedure TcxCustomTreeList.SetNodeParent(ANewNode, ANewParent: TcxTreeListNode;
  AMode: TcxTreeListNodeAddMode);
begin
  case AMode of
    tlnaAdd:
      ANewParent.SetLast(ANewNode);
    tlnaAddFirst:
      ANewParent.SetFirst(ANewNode);
    tlnaInsert:
      ANewNode.InternalInsert(ANewParent);
  end;
  AddChanges([tcStructure]);
end;

procedure TcxCustomTreeList.SetFocusedVisibleNode(ANode: TcxTreeListNode);
begin
  if ANode = nil then Exit;
  FocusedNode := ANode;
  if FocusedNode <> nil then
    FocusedNode.MakeVisible;
end;

procedure TcxCustomTreeList.ScrollWindow(const DX, DY: Integer);
var
  I: Integer;
  AMemDC: HDC;
  AMemBitmap: HBitmap;
  AScrollRect, AUpdateRect: TRect;
  AVScrollbarAreaWidth: Integer;
begin
  if Controller.SelectionLockCount <> 0 then
  begin
    Controller.PostMakeVisible := True;
    Exit;
  end;
  if (GetScrollbarMode = sbmHybrid) and (IsScrollBarActive(sbVertical)) and
    (Bands.VisibleRightFixedCount = 0) then
    AVScrollbarAreaWidth := GetVScrollBarAreaWidth
  else
    AVScrollbarAreaWidth := 0;
  if ((DX = 0) and (DY = 0)) or not HandleAllocated then Exit;
  if not Styles.CanOffsetContent or ((DX <> 0) and not ViewInfo.CanOffsetContent) then
  begin
    if ViewInfo.IsDirty then
      Invalidate
    else
      Repaint;
    Exit;
  end;
  if DX <> 0 then
  begin
    AScrollRect := ViewInfo.HScrollArea;
    AUpdateRect := ViewInfo.HScrollArea;
    if cxRectWidth(AScrollRect) <= 0 then
      Exit;
    if UseRightToLeftScrollBar then
      if DX > 0 then
      begin
        AUpdateRect.Right := AUpdateRect.Left + DX + AVScrollbarAreaWidth;
        Dec(AScrollRect.Right, DX);
        Inc(AScrollRect.Left, AVScrollbarAreaWidth);
      end
      else
      begin
        AUpdateRect.Left := AUpdateRect.Right + DX;
        Dec(AScrollRect.Left, DX);
      end
    else
      if DX > 0 then
      begin
        AUpdateRect.Right := AUpdateRect.Left + DX;
        Dec(AScrollRect.Right, DX);
      end
      else
      begin
        AUpdateRect.Left := AUpdateRect.Right + DX - AVScrollbarAreaWidth;
        Dec(AScrollRect.Left, DX);
        Dec(AScrollRect.Right, AVScrollbarAreaWidth);
      end;
  end
  else
  begin
    AScrollRect := cxRectSetTop(ClientBounds, ViewInfo.ContentBounds.Top);
    AScrollRect.Bottom := ClientBounds.Bottom - ViewInfo.BottomNonContentHeight;
    AUpdateRect := AScrollRect;
    if DY > 0 then
    begin
      AUpdateRect.Bottom := AUpdateRect.Top + DY;
      Dec(AScrollRect.Bottom, DY);
    end
    else
    begin
      AUpdateRect.Top := AUpdateRect.Bottom + DY;
      Dec(AScrollRect.Top, DY);
    end
  end;
  AMemDC := CreateCompatibleDC(Canvas.Handle);
  AMemBitmap := CreateCompatibleBitmap(Canvas.Handle, cxRectWidth(AUpdateRect), cxRectHeight(AUpdateRect));
  try
    AMemBitmap := SelectObject(AMemDC, AMemBitmap);
    SetWindowOrgEx(AMemDC, AUpdateRect.Left, AUpdateRect.Top, nil);
    PaintWindow(AMemDC);
    ScrollWindowEx(Handle, DX, DY, @AScrollRect, nil, 0, nil, SW_INVALIDATE);
    cxBitBlt(Canvas.Handle, AMemDC, AUpdateRect, AUpdateRect.TopLeft, SrcCopy);
    Windows.ValidateRect(Handle, @AUpdateRect);
    if Odd(DY) then
      for I := 0 to ViewInfo.NodesVisibleCount - 1 do
        ViewInfo.NodeViewData[I].InvalidateIndents;
    Update;
  finally
    AMemBitmap := SelectObject(AMemDC, AMemBitmap);
    DeleteObject(AMemBitmap);
    DeleteDC(AMemDC);
  end;
end;

procedure TcxCustomTreeList.StructureChanged;
begin
  Include(FChanges, tcStructure);
  LayoutChanged;
end;

procedure TcxCustomTreeList.UpdateFocusedNode(AColumn: TcxTreeListColumn);
begin
  Controller.UpdateRecord(TdxNativeInt(FocusedNode));
  if not IsLocked then
    ViewInfo.UpdateSelection;
end;

procedure TcxCustomTreeList.ValidateStates;
begin
  if (FChanges - SelectionChanges <> []) and not ChangesLocked then
    ForceLayoutChanged
  else
    if not ChangesLocked and (Bands <> nil) then
      Bands.RefreshInformation;
end;

procedure TcxCustomTreeList.DoSelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TcxCustomTreeList.DoSelectAll;
var
  I: Integer;
begin
  SelectionList.Clear;
  SelectionList.Capacity := AbsoluteVisibleCount;
  for I := 0 to AbsoluteVisibleCount - 1 do
  begin
    if not DoCanSelectNode(AbsoluteVisibleItems[I]) then Continue;
    SelectionList.Add(AbsoluteVisibleItems[I]);
  end;
end;

procedure TcxCustomTreeList.DoSorted;
begin
  if Assigned(FOnSorted) then
    FOnSorted(Self);
end;

procedure TcxCustomTreeList.DoSorting;
begin
  if Assigned(FOnSorting) then
    FOnSorting(Self);
end;

procedure TcxCustomTreeList.DoSortNodes;
begin
  if (not (tcSortOrder in Changes) and not Sorted) or IsLoading or IsDestroying or
    IsUpdating or (tcLoading in Changes) then Exit;
  if IsInserting then
    Changes := Changes - [tcSortOrder]
  else
  begin
    DoSorting;
    try
      Root.AlphaSort(True);
      Changes := Changes - [tcSortOrder];
    finally
      DoSorted;
    end;
  end;
end;

procedure TcxCustomTreeList.DoStartDrag(var DragObject: TDragObject);
begin
  DragPos := MouseDownPos;
  if HitTest.HitNode = nil then
    HitTest.FHitNode := FDragNode;
  if DoBeginDragNode(FDragNode) then
    inherited DoStartDrag(DragObject)
  else
  begin
    CancelDrag;
    FDragNode := nil;
  end;
end;

procedure TcxCustomTreeList.DoTopRecordIndexChanged;
begin
  if Assigned(FOnTopRecordIndexChanged) then
    FOnTopRecordIndexChanged(Self);
end;

procedure TcxCustomTreeList.FilterChanged;
begin
  BeginUpdate;
  try
    inherited FilterChanged;
    AddChanges([tcData]);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.FindFilterChanged;
begin
  Controller.FindPanel.FindChanged;
  FilterChanged;
end;

procedure TcxCustomTreeList.FontChanged;
begin
  inherited FontChanged;
  if FDefaultLevelInfo <> nil then
    FDefaultLevelInfo.Changed(nil);
end;

procedure TcxCustomTreeList.ForceLayoutChanged;
begin
  if ViewInfo.CalculateInProcess or IsLoading or IsDestroying then Exit;
  BeforeUpdate;
  DoLayoutChanged;
end;

function TcxCustomTreeList.GetNavigatorButtonsControl: IcxNavigator;
begin
  Result := Self;
end;

function TcxCustomTreeList.HasFilterEvent: Boolean;
begin
  Result := Assigned(FOnFilterNode);
end;

function TcxCustomTreeList.GetBandItemClass: TcxTreeListBandClass;
begin
  Result := TcxTreeListBand;
end;

function TcxCustomTreeList.GetControllerClass: TcxCustomControlControllerClass;
begin
  Result := TcxTreeListController;
end;

function TcxCustomTreeList.GetControlStylesClass: TcxCustomControlStylesClass;
begin
  Result := TcxTreeListStyles;
end;

function TcxCustomTreeList.GetHitTestControllerClass: TcxHitTestControllerClass;
begin
  Result := TcxTreeListHitTest;
end;

function TcxCustomTreeList.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxTreeListOptionsBehavior;
end;

function TcxCustomTreeList.GetOptionsCustomizingClass:
  TcxTreeListOptionsCustomizingClass;
begin
  Result := TcxTreeListOptionsCustomizing;
end;

function TcxCustomTreeList.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxTreeListOptionsData;
end;

function TcxCustomTreeList.GetOptionsFilterBoxClass: TcxControlOptionsFilterBoxClass;
begin
  Result := TcxTreeListFilterBox;
end;

function TcxCustomTreeList.GetOptionsFilteringClass: TcxControlOptionsFilteringClass;
begin
  Result := TcxTreeListFiltering;
end;

function TcxCustomTreeList.GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass;
begin
  Result := TcxTreeListOptionsSelection;
end;

function TcxCustomTreeList.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxTreeListOptionsView;
end;

function TcxCustomTreeList.GetPainterClass: TcxCustomControlPainterClass;
begin
  Result := TcxTreeListPainter;
end;

function TcxCustomTreeList.GetPreviewClass: TcxTreeListPreviewClass;
begin
  Result := TcxTreeListPreview;
end;

function TcxCustomTreeList.GetRootClass: TcxTreeListNodeClass;
begin
  Result := TcxTreeListRootNode;
end;

function TcxCustomTreeList.GetSummaryClass: TcxTreeListSummaryClass;
begin
  Result := TcxTreeListSummary;
end;

function TcxCustomTreeList.GetTreeListColumnClass: TcxTreeListColumnClass;
begin
  Result := TcxTreeListColumn;
end;

function TcxCustomTreeList.GetTreeListCustomizingClass: TcxTreeListCustomizingClass;
begin
  Result := TcxTreeListCustomizing;
end;

function TcxCustomTreeList.GetViewInfoClass: TcxCustomControlViewInfoClass;
begin
  Result := TcxTreeListViewInfo;
end;

function TcxCustomTreeList.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxTreeListDataController;
end;

function TcxCustomTreeList.GetDateTimeHandlingClass: TcxControlOptionsDateTimeHandlingClass;
begin
  Result := TcxTreeListDateTimeHandling;
end;

function TcxCustomTreeList.GetEditingControllerClass: TcxEditingControllerClass;
begin
  Result := TcxTreeListEditingController;
end;

function TcxCustomTreeList.GetFilterValueListClass: TcxControlFilterValueListClass;
begin
  Result := TcxTreeListFilterValueList;
end;

function TcxCustomTreeList.GetDefaultColumnContainer: TcxTreeListBand;
begin
  if Bands.Count = 0 then
    Result := Bands.Add
  else
    if Bands.BottomItemCount > 0 then
      Result := Bands.BottomItems[0]
    else
      Result := Bands[0];
end;

function TcxCustomTreeList.GetEditAutoHeight: TcxInplaceEditAutoHeight;
begin
  Result := OptionsBehavior.EditAutoHeight;
  if (Result = eahRow) and not OptionsView.CellAutoHeight then
    Result := eahNone;
end;

function TcxCustomTreeList.GetSameColumn(AColumn: TcxTreeListColumn): TcxTreeListColumn;
begin
  Result := nil;
  if (AColumn <> nil) and cxInRange(AColumn.ID, 0, ContainerList.Count - 1) then
    Result := ContainerList[AColumn.ID];
end;

function TcxCustomTreeList.GetStartNodeForBestFit: TcxTreeListNode;
begin
  Result := nil;
  if AbsoluteVisibleCount > 0 then
  begin
    if GetCountNodeForBestFit < 0 then
      Result := AbsoluteVisibleItems[0]
    else
      Result := ViewInfo.TopNode
  end;
end;

function TcxCustomTreeList.IsLocked: Boolean;
begin
  Result := inherited IsLocked or IsTreeListLocked;
end;

function TcxCustomTreeList.IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := not ViewInfo.IsRecordPixelScrollMode or (AScrollKind <> sbVertical);
end;

function TcxCustomTreeList.IsTreeListLocked: Boolean;
begin
  Result := (LockUpdate > 0) or ([csLoading, csDestroying, csUpdating, csReading] * ComponentState <> []);
end;

procedure TcxCustomTreeList.ImagesChanged(Sender: TObject);
begin
  Include(FChanges, tcImages);
  LayoutChanged;
end;

procedure TcxCustomTreeList.InitializeFields;
var
  I: Integer;
begin
  ViewInfo.SetDirty;
  for I := FAbsoluteItems.Count - 1 downto 0 do
    DataController.FreeNodeRecord(TcxTreeListNode(FAbsoluteItems[I]));
  for I := 0 to ContainerList.Count - 1 do
    TcxTreeListColumn(ContainerList[I]).InitializeValueDef;
end;

procedure TcxCustomTreeList.InternalClearAll;
begin
  InternalDelete(FAbsoluteItems);
  SelectionList.Clear;
end;

procedure TcxCustomTreeList.InternalCopy(
  ANode, ADestNode: TcxTreeListNode);
var
  ANewNode: TcxTreeListNode;
begin
  ANewNode := AddNode(nil, ADestNode, nil, tlamAddChild);
  ANewNode.AssignData(ANode);
  ANode := ANode.GetFirstChild;
  while ANode <> nil do
  begin
    InternalCopy(ANode, ANewNode);
    ANode := ANode.FNext;
  end;
end;

procedure TcxCustomTreeList.InternalDelete(ANodes: TList);
var
  I: Integer;
  AList: TList;
begin
  if ANodes.Count = 0 then Exit;
  BeginUpdate;
  try
    AList := TList.Create;
    try
      AList.Assign(ANodes);
      ANodes.Clear;
      AList.Sort(@cxCompareNodesByLevel);
      for I := 0 to AList.Count - 1 do
        TcxTreeListNode(AList[I]).InternalFree;
    finally
      AList.Free;
    end;
  finally
    EndUpdate;
    ANodes.Clear;
  end;
end;

procedure TcxCustomTreeList.InternalMove(
  ANode, ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode);
begin
  if (ANode = nil) or (ADestNode = nil) or ADestNode.HasAsParent(ANode) or
    ((ADestNode = ANode.Parent) and (AMode = tlamAddChild)) or (ANode = ADestNode) then Exit;
  AddNodeInternal(ANode, ADestNode, AMode);
end;

function TcxCustomTreeList.IsNodeEdited(ANode: TcxTreeListNode): Boolean;
begin
  Result := (DataController.EditingNode = ANode) and
    (nsEditing in ANode.State);
end;

function TcxCustomTreeList.IsNodeInserting(ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode <> nil) and (nsInserting in ANode.State);
end;

procedure TcxCustomTreeList.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  HideEdit;
  ConditionalFormattingProvider.ClearCache;
  Changes := Changes + [tcImages];
  ViewInfo.SetDirty;
  inherited LookAndFeelChanged(Sender, AChangedValues);
end;

function TcxCustomTreeList.CanMultiSelect: Boolean;
begin
  Result := OptionsSelection.MultiSelect;
end;

procedure TcxCustomTreeList.BeforeShowingEdit;
begin
  with Controller do
    SetFocusedRecordItem(FocusedRecordIndex, FocusedItem);
end;

procedure TcxCustomTreeList.DoClear;
begin
  CheckStructure;
  InternalClearAll;
end;

procedure TcxCustomTreeList.DoDeleteSelection;
var
  AList: TList;
begin
  CancelEdit;
  if FocusedNode = nil then
    Exit;
  if FocusedNode.Inserting then
  begin
    DataController.Cancel;
    Exit;
  end;
  BeginUpdate;
  try
    AList := GetSelectionsEx;
    try
      if AList.Count > 0 then
      begin
        AList.Sort(@cxCompareNodesByLevel);
        DeleteNodes(AList);
      end;
    finally
      FreeAndNil(AList);
    end;
  finally
    EndUpdate;
  end;
  if (Root.Count = 0) and (SelectionCount > 0) then
  begin
    SelectionList.Clear;
    DoSelectionChanged;
  end;
end;

procedure TcxCustomTreeList.DoDeleteNode(ANode: TcxTreeListNode);
begin
  Controller.CheckDeletedNode(ANode);
  if FTopVisibleNode = ANode then
  begin
    FTopVisibleNode := ANode.GetNextSiblingVisible;
    if FTopVisibleNode = nil then
      FTopVisibleNode := ANode.GetPrev;
    DoTopRecordIndexChanged;
  end;
  DataController.FreeNodeRecord(ANode);
  if HitTest.FHitNode = ANode then
    HitTest.FHitNode := nil;
  SelectionList.Remove(ANode);
  if ANode = FDragNode then
    FDragNode := nil;
  if not IsDestroying and (ANode <> Root) and (Root.FCount = 1) and (Root.FFirst = ANode) then
    Controller.FocusedNode := nil;
  if DataController.FEditingNode = ANode then
    DataController.FEditingNode := nil;
end;

procedure TcxCustomTreeList.DoInternalMoveTo(AttachNode: TcxTreeListNode;
  AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);

  function CheckNodesList: Boolean;
  var
    I: Integer;
  begin
    I := 0;
    while I < ANodes.Count do
    begin
      if TcxTreeListNode(ANodes[I]).CanMove(AttachNode, AttachMode) then
        Inc(I)
      else
        ANodes.Delete(I);
    end;
    Result := ANodes.Count > 0;
  end;

var
  I: Integer;
begin
  if IsCopy and (AttachMode = tlamInsert) then
    AttachNode := AttachNode.Parent;
  if not CheckNodesList or DoMoveToEvent(AttachNode, AttachMode, ANodes, IsCopy) then Exit;
  if (AttachMode = tlamAddChild) and not IsCopy then
  begin
    for I := 0 to ANodes.Count - 1 do
    begin
      if TcxTreeListNode(ANodes[I]).CanMove(AttachNode, AttachMode) then
        TcxTreeListNode(ANodes[I]).ExtractFromParent;
    end;
  end;
  DoMoveTo(AttachNode, AttachMode, ANodes, IsCopy);
end;

procedure TcxCustomTreeList.DoMoveTo(AttachNode: TcxTreeListNode;
 AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);
var
  I: Integer;
begin
  with ANodes do
  begin
    for I := Count - 1 downto 0 do
    begin
      if IsCopy then
        InternalCopy(TcxTreeListNode(List[I]), AttachNode)
      else
        InternalMove(TcxTreeListNode(List[I]), AttachNode, AttachMode);
    end;
  end;
end;

procedure TcxCustomTreeList.DoOnBandSizeChanged(ABand: TcxTreeListBand);
begin
  if Assigned(FOnBandSizeChanged) then
    FOnBandSizeChanged(Self, ABand);
end;

procedure TcxCustomTreeList.DoOnColumnSizeChanged(
  AColumn: TcxTreeListColumn);
begin
  if Assigned(FOnColumnSizeChanged) then
    FOnColumnSizeChanged(Self, AColumn);
end;

function TcxCustomTreeList.DoWriteHeaderToText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to VisibleColumnCount - 1 do
  begin
    if I > 0 then
      Result := Result + cxColumnTextSeparator;
    Result := Result + VisibleColumns[I].Caption.Text;
  end;
end;

function TcxCustomTreeList.DoWriteNodeToText(ANode: TcxTreeListNode): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to VisibleColumnCount - 1 do
  begin
    if I > 0 then
      Result := Result + cxColumnTextSeparator;
    Result := Result + VisibleColumns[I].DisplayTexts[ANode];
  end;
end;

procedure TcxCustomTreeList.DoWriteToClipboard(AOnlySelected: Boolean; ACheckVisible: Boolean = False);
var
  AText: string;
begin
  if DoWriteToText(AOnlySelected, AText, ACheckVisible) then
    ClipBoard.AsText := AText
end;

function TcxCustomTreeList.DoWriteToText(AOnlySelected: Boolean; out AText: string; ACheckVisible: Boolean = False): Boolean;
var
  ANode: TcxTreeListNode;
begin
  ANode := Root.GetFirstChild;
  if OptionsBehavior.CopyCaptionsToClipboard then
    AText := DoWriteHeaderToText
  else
    AText := '';
  while ANode <> nil do
  begin
    if (not AOnlySelected or ANode.Selected) and (not ACheckVisible or ANode.IsVisible) then
    begin
      if AText <> '' then
        AText := AText + dxCRLF;
      AText := AText + DoWriteNodeToText(ANode);
    end;
    ANode := ANode.GetNext;
  end;
  Result := AText <> '';
end;

procedure TcxCustomTreeList.DeleteNodes(AList: TList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AList.Count - 1 do
      TcxTreeListNode(AList[I]).Delete;
  finally
    EndUpdate;
  end;
end;

function TcxCustomTreeList.GetCountNodeForBestFit: Integer;
begin
  Result := OptionsBehavior.BestFitMaxRecordCount;
end;

function TcxCustomTreeList.GetIsActive: Boolean;
begin
  Result := Controller.Focused;
end;

function TcxCustomTreeList.GetFixedContentSize: Integer;
begin
  Result := 0;
  if (Bands.FirstVisible <> nil) and (Bands.FirstVisible.FixedKind = tlbfLeft) then
    Inc(Result, Bands.FirstVisible.DisplayWidth);
  if (Bands.LastVisible <> nil) and (Bands.LastVisible.FixedKind = tlbfRight) then
    Inc(Result, Bands.LastVisible.DisplayWidth);
end;

function TcxCustomTreeList.GetMaxBandWidth(ABand: TcxTreeListBand): Integer;
begin
  Result := MaxInt;
end;

function TcxCustomTreeList.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskVertical;
end;

procedure TcxCustomTreeList.InitInsertingNode(ANode: TcxTreeListNode);
begin
  ANode.Focused := True;
  ANode.MakeVisible;
  DataController.EditingNode := ANode;
end;

function TcxCustomTreeList.InsertNode(
  ARelative: TcxTreeListNode; IsAppend: Boolean): Boolean;
var
  ANode: TcxTreeListNode;
const
  AMode: array[Boolean] of TcxTreeListNodeAttachMode = (tlamInsert, tlamAddChild);
begin
  HideEdit;
  BeginUpdate;
  try
    ANode := ARelative;
    if (ANode <> nil) and ANode.Inserting then
      ANode := ANode.Parent;
    IsAppend := IsAppend or (ANode = nil);
    if (ARelative <> nil) and IsAppend then
      ANode.LoadChildren;
    if ANode = nil then
      ANode := Root;
    ANode := AddNode(nil, ANode, nil, AMode[IsAppend]);
    Result := ANode <> nil;
    if Result then
    begin
      if Result then
      begin
        Include(ANode.State, nsInserting);
        InitInsertingNode(ANode);
      end;
    end;
    DataController.IsValueChanged := False;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTreeList.MakeNodeVisible(ANode: TcxTreeListNode);
begin
  if (ANode = nil) or not ANode.IsVisible then Exit;
  if FTopVisibleNode = nil then
  begin
    FTopVisibleNode := ANode;
    DoTopRecordIndexChanged;
    ViewInfo.SetDirty;
  end;
  CheckChanges;
  if (ANode = FTopVisibleNode) then
   if ViewInfo.FPixelScrollNodeOffset = 0 then
      ANode.Repaint(True)
   else
   begin
     ViewInfo.FPixelScrollNodeOffset := 0;
     LayoutChanged;
   end
  else
  begin
    ViewInfo.Validate;
    if ANode.IsPrinted and (Changes - SelectionChanges = []) then
      ViewInfo.MakeVisible(ANode)
    else
    begin
      if (FTopVisibleNode = nil) or (ANode.VisibleIndex <= FTopVisibleNode.VisibleIndex) then
      begin
        ViewInfo.FPixelScrollNodeOffset := 0;
        if TopVisibleNode <> ANode then
          TopVisibleNode := ANode
        else
          LayoutChanged;
      end
      else
        LastVisibleNode := ANode;
    end;
  end;
end;

procedure TcxCustomTreeList.RestoreCursor;
begin
  if not OptionsBehavior.ShowHourGlass then Exit;
  Dec(FGlassCursorRefCount);
  if FGlassCursorRefCount = 0 then
    Screen.Cursor := FStoredCursor;
end;

procedure TcxCustomTreeList.SetGlassCursor;
begin
  if not OptionsBehavior.ShowHourGlass then Exit;
  if FGlassCursorRefCount = 0 then
  begin
    if OptionsBehavior.HotTrack and (Controller.HotTrackNode <> nil) then
      FStoredCursor := Controller.PrevCursor
    else
      FStoredCursor := Screen.Cursor;
  end;
  if HandleAllocated and Visible then
    Screen.Cursor := crHourGlass;
  Inc(FGlassCursorRefCount);
end;

procedure TcxCustomTreeList.SortingStateChanged(
  AColumn: TcxTreeListColumn; AShift: TShiftState);
begin
end;

function TcxCustomTreeList.HasAllItems: Boolean;
begin
  Result := True;
end;

function TcxCustomTreeList.SupportBandColumnEditor: Boolean;
begin
  Result := True;
end;

function TcxCustomTreeList.SupportItemsEditor: Boolean;
begin
  Result := False
end;

function TcxCustomTreeList.SupportCreateAllItems: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTreeList.CreateAllItems(AMissingItemsOnly: Boolean = False);
begin
end;

// IcxNavigatorRecordPosition
function TcxCustomTreeList.NavigatorGetRecordCount: Integer;
begin
  Result := AbsoluteCount;
end;

function TcxCustomTreeList.NavigatorGetRecordIndex: Integer;
begin
  if FocusedNode <> nil then
    Result := FocusedNode.AbsoluteIndex
  else
    Result := -1;
end;

function TcxCustomTreeList.NavigatorIsEditing: Boolean;
begin
  Result := DataController.EditingNode <> nil;
end;

// IcxNavigator
procedure TcxCustomTreeList.RefreshNavigatorButtons;
begin
  if not (IsLoading or IsDestroying) then
  begin
    if Navigator.Visible then
      NavigatorStateChanged;
    if FNavigatorNotifier <> nil then
      FNavigatorNotifier.RefreshNavigatorButtons;
  end;
end;

procedure TcxCustomTreeList.DoAction(AButtonIndex: Integer);
begin
  case AButtonIndex of
    NBDI_FIRST:
      GotoBOF;
    NBDI_PRIORPAGE:
      GotoPrevPage;
    NBDI_PRIOR:
      GotoPrev;
    NBDI_NEXT:
      GotoNext;
    NBDI_NEXTPAGE:
      GotoNextPage;
    NBDI_LAST:
      GotoEOF;
    NBDI_INSERT:
      if Controller.CanInsertNode then
        InsertNode(FocusedNode, False);
    NBDI_APPEND:
      if FocusedNode <> nil then
{        AddNode(nil, FocusedNode.Parent, nil, tlamAdd).Focused := True
      else
        AddNode(nil, Root, nil, tlamAdd).Focused := True;}
        InsertNode(FocusedNode.Parent, True)
      else
        InsertNode(Root, True);
    NBDI_DELETE:
      if Controller.CanDeleteSelection and
        Controller.DeleteConfirmation then
        DeleteSelection;
    NBDI_EDIT:
      ShowEdit;
    NBDI_POST:
        Post;
    NBDI_CANCEL:
      Cancel;
    NBDI_REFRESH:
      DataController.Refresh;
    NBDI_SAVEBOOKMARK:
      SaveBookmark;
    NBDI_GOTOBOOKMARK:
      GotoBookmark;
    NBDI_FILTER:
      RunFilterCustomizeDialog;
  end;
end;

function TcxCustomTreeList.GetNotifier: TcxNavigatorControlNotifier;
begin
  Result := FNavigatorNotifier;
end;

function TcxCustomTreeList.IsActionSupported(AButtonIndex: Integer): Boolean;
begin
  Result := AButtonIndex in [NBDI_FIRST..NBDI_FILTER];
end;

function TcxCustomTreeList.CanAppend: Boolean;
begin
  Result := OptionsData.Inserting and HasData;
end;

function TcxCustomTreeList.CanEdit: Boolean;
begin
  Result := OptionsData.Editing and (HasData or CanInsert);
end;

function TcxCustomTreeList.CanDelete: Boolean;
begin
  Result := OptionsData.Deleting and HasData and (Count > 0);
end;

function TcxCustomTreeList.CanInsert: Boolean;
begin
  Result := CanAppend {and (FocusedNode <> nil)};
end;

procedure TcxCustomTreeList.ClearBookmark;
begin
  DataController.ClearBookmark;
end;

procedure TcxCustomTreeList.GotoBookmark;
begin
  DataController.GotoBookmark;
end;

function TcxCustomTreeList.HasData: Boolean;
begin
  Result := VisibleColumnCount > 0;
end;

function TcxCustomTreeList.IsBookmarkAvailable: Boolean;
begin
  Result := DataController.IsBookmarkAvailable and HasData;
end;

procedure TcxCustomTreeList.SaveBookmark;
begin
  DataController.SaveBookmark;
end;

function TcxCustomTreeList.GetSelectionsEx: TList;
begin
  Result := TList.Create;
  GetSelections(Result);
  if (FocusedNode <> nil) and (Result.IndexOf(FocusedNode) = -1) then
    Result.Add(FocusedNode);
end;

function TcxCustomTreeList.GetOnEditing: TcxTreeListEditingEvent;
begin
  Result := TcxTreeListEditingEvent(inherited OnEditing);
end;

function TcxCustomTreeList.GetOnEdited: TcxTreeListColumnChangedEvent;
begin
  Result := TcxTreeListColumnChangedEvent(inherited OnEdited);
end;

function TcxCustomTreeList.GetOnEditValueChanged: TcxTreeListColumnChangedEvent;
begin
  Result := TcxTreeListColumnChangedEvent(inherited OnEditValueChanged);
end;

function TcxCustomTreeList.GetSearching: Boolean;
begin
  Result := Controller.IsIncSearching;
end;

function TcxCustomTreeList.GetSearchingText: string;
begin
  Result := Controller.IncSearchText
end;

function TcxCustomTreeList.GetAvailableContentWidth: Integer;
var
  AVScrollbarAreaWidth: Integer;
begin
  Result := ClientBounds.Right - ClientBounds.Left;
  if Bands.VisibleLeftFixedCount > 0 then
    Dec(Result, ViewInfo.FixedSeparatorWidth);
  if Bands.VisibleRightFixedCount > 0 then
    Dec(Result, ViewInfo.FixedSeparatorWidth);
  Dec(Result, ViewInfo.IndicatorWidth);

  if (GetScrollbarMode = sbmHybrid) and (IsScrollBarActive(sbVertical)) and
    (Bands.VisibleRightFixedCount = 0) then
    AVScrollbarAreaWidth := GetVScrollBarAreaWidth
  else
    AVScrollbarAreaWidth := 0;
  Dec(Result, AVScrollbarAreaWidth);

  if Result < 0 then
    Result := 0;
end;

function TcxCustomTreeList.GetAbsoluteCount: Integer;
begin
  CheckChanges;
  Result := FAbsoluteItems.Count;
end;

function TcxCustomTreeList.GetAbsoluteItem(
  AIndex: Integer): TcxTreeListNode;
begin
  CheckChanges;
  Result := TcxTreeListNode(FAbsoluteItems[AIndex]);
end;

function TcxCustomTreeList.GetAbsoluteItemsList: TList;
begin
  CheckChanges;
  Result := FAbsoluteItems;
end;

function TcxCustomTreeList.GetAbsoluteVisibleCount: Integer;
begin
  CheckChanges;
  Result := FAbsoluteVisibleItems.Count;
end;

function TcxCustomTreeList.GetAbsoluteVisibleItem(
  AIndex: Integer): TcxTreeListNode;
begin
  CheckChanges;
  Result := TcxTreeListNode(FAbsoluteVisibleItems[AIndex]);
end;

function TcxCustomTreeList.GetAbsoluteVisibleItemsList: TList;
begin
  CheckChanges;
  Result := FAbsoluteVisibleItems;
end;

procedure TcxCustomTreeList.SetAbsoluteItem(
  AIndex: Integer; AValue: TcxTreeListNode);
begin
  AbsoluteItems[AIndex].Assign(AValue);
end;

procedure TcxCustomTreeList.SetAbsoluteVisibleItem(
  AIndex: Integer; AValue: TcxTreeListNode);
begin
  AbsoluteVisibleItems[AIndex].Assign(AValue);
end;

function TcxCustomTreeList.GetBands: TcxTreeListBands;
begin
  Result := FBands;
end;

function TcxCustomTreeList.GetColumn(
  AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(ColumnsList[AIndex]);
end;

function TcxCustomTreeList.GetColumnCount: Integer;
begin
  Result := ColumnsList.Count;
end;

function TcxCustomTreeList.GetColumnsList: TList;
begin
  Result := ContainerList;
end;

function TcxCustomTreeList.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := ConditionalFormattingProvider.ConditionalFormatting;
end;

function TcxCustomTreeList.GetController: TcxTreeListController;
begin
  Result := TcxTreeListController(inherited Controller);
end;

function TcxCustomTreeList.GetCount: Integer;
begin
  if Root <> nil then
    Result := Root.Count
  else
    Result := 0;
end;

function TcxCustomTreeList.GetDataController: TcxTreeListDataController;
begin
  Result := TcxTreeListDataController(inherited DataController);
end;

function TcxCustomTreeList.GetDateTimeHandling: TcxTreeListDateTimeHandling;
begin
  Result := TcxTreeListDateTimeHandling(inherited DateTimeHandling);
end;

function TcxCustomTreeList.GetDefaultRowHeight: Integer;
begin
  Result := FDefaultRowHeight;
end;

function TcxCustomTreeList.GetFilter: TcxDataFilterCriteria;
begin
  Result := DataController.Filter;
end;

function TcxCustomTreeList.GetFilterBox: TcxTreeListFilterBox;
begin
  Result := TcxTreeListFilterBox(inherited FilterBox);
end;

function TcxCustomTreeList.GetFiltering: TcxTreeListFiltering;
begin
  Result := TcxTreeListFiltering(inherited Filtering);
end;

function TcxCustomTreeList.GetFocusedColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(Controller.FocusedItem);
end;

function TcxCustomTreeList.GetFocusedNode: TcxTreeListNode;
begin
  Result := Controller.FocusedNode;
end;

function TcxCustomTreeList.GetHitTest: TcxTreeListHitTest;
begin
  Result := TcxTreeListHitTest(Controller.HitTestController);
end;

function TcxCustomTreeList.GetImages: TCustomImageList;
begin
  Result := FDefaultLevelInfo.Images;
end;

function TcxCustomTreeList.GetLastNode: TcxTreeListNode;
begin
  if AbsoluteVisibleCount > 0 then
    Result := AbsoluteVisibleItems[AbsoluteVisibleCount - 1]
  else
    Result := nil;
end;

function TcxCustomTreeList.GetLastPartVisibleNode: TcxTreeListNode;
begin
  ViewInfo.Validate;
  Result := ViewInfo.LastPartVisibleNode;
end;

function TcxCustomTreeList.GetLastVisibleNode: TcxTreeListNode;
begin
  ViewInfo.Validate;
  Result := ViewInfo.LastNode;
end;

function TcxCustomTreeList.GetLevelInfo(ALevel: Integer): TcxTreeListLevelInfo;
begin
  Result := TcxTreeListLevelInfo(FLevelsInfo[ALevel]);
end;

function TcxCustomTreeList.GetNavigatorIsActive: Boolean;
begin
  Result := DataController.Active;
end;

function TcxCustomTreeList.GetIndentWidth: Integer;
begin
  CheckChanges;
  if InRange(ExpansionLevel, 0, FLevelsInfo.Count - 1) and (Bands.ExpandableBand <> nil) then
    Result := LevelsInfo[ExpansionLevel].TotalWidth
  else
    Result := 0;

  if OptionsView.CheckGroups then
    Inc(Result, ViewInfo.CheckboxSize.cx)
end;

function TcxCustomTreeList.GetInplaceEditor: TcxCustomEdit;
begin
  if IsEditing then
    Result := Controller.EditingController.Edit
  else
    Result := nil;
end;

function TcxCustomTreeList.GetIsCancelOperation: Boolean;
begin
  Result := FIsCancelOperation or (GetAsyncKeyState(VK_ESCAPE) < 0);
  FIsCancelOperation := Result;
end;

function TcxCustomTreeList.GetIsEditing: Boolean;
begin
  Result := Controller.IsEditing;
end;

function TcxCustomTreeList.GetIsInserting: Boolean;
begin
  Result := IsNodeInserting(FocusedNode);
end;

function TcxCustomTreeList.GetItem(Index: Integer): TcxTreeListNode;
begin
  Result := Root.Items[Index];
end;

function TcxCustomTreeList.GetOptionsBehavior: TcxTreeListOptionsBehavior;
begin
  Result := TcxTreeListOptionsBehavior(inherited OptionsBehavior);
end;

function TcxCustomTreeList.GetOptionsData: TcxTreeListOptionsData;
begin
  Result := TcxTreeListOptionsData(inherited OptionsData);
end;

function TcxCustomTreeList.GetOptionsView: TcxTreeListOptionsView;
begin
  Result := TcxTreeListOptionsView(inherited OptionsView);
end;

function TcxCustomTreeList.GetSelection(AIndex: Integer): TcxTreeListNode;
begin
  if (AIndex = 0) and not OptionsSelection.MultiSelect and (FocusedNode <> nil) then
    Result := FocusedNode
  else
    Result := TcxTreeListNode(SelectionList[AIndex]);
end;

function TcxCustomTreeList.GetSelectionCount: Integer;
begin
  Result := SelectionList.Count;
  if not OptionsSelection.MultiSelect and (FocusedNode <> nil) then
    Result := 1;
end;

function TcxCustomTreeList.GetSortedColumnCount: Integer;
begin
  Result := FSortedColumns.Count;
end;

function TcxCustomTreeList.GetSortedColumn(
  Index: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FSortedColumns[Index]);
end;

function TcxCustomTreeList.GetStateImages: TCustomImageList;
begin
  Result := FDefaultLevelInfo.StateImages;
end;

function TcxCustomTreeList.GetStyles: TcxTreeListStyles;
begin
  Result := TcxTreeListStyles(inherited Styles);
end;

function TcxCustomTreeList.GetSorted: Boolean;
begin
  Result := FSortedColumns.Count > 0;
end;

function TcxCustomTreeList.GetTopNode: TcxTreeListNode;
begin
  if AbsoluteVisibleCount > 0 then
    Result := AbsoluteVisibleItems[0]
  else
    Result := nil;
end;

function TcxCustomTreeList.GetTopVisibleNode: TcxTreeListNode;
begin
  Result := FTopVisibleNode;
  if Result = nil then
    Result := Root.getFirstChild;
  if (Result <> nil) and (Result.IsHidden or not Result.IsVisible) then
  begin
    if Result.GetNextVisible <> nil then
      Result := Result.GetNextVisible
    else
     if Result.GetPrevVisible <> nil then
       Result := Result.GetPrevVisible;
  end;
end;

function TcxCustomTreeList.GetVisibleCount: Integer;
begin
  Result := ViewInfo.NodesVisibleCount;
end;

function TcxCustomTreeList.GetViewInfo: TcxTreeListViewInfo;
begin
  Result := TcxTreeListViewInfo(inherited ViewInfo);
end;

function TcxCustomTreeList.GetVisibleColumn(
  AIndex: Integer): TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(FVisibleColumns[AIndex]);
end;

function TcxCustomTreeList.GetVisibleColumnCount: Integer;
begin
  Result := FVisibleColumns.Count;
end;

procedure TcxCustomTreeList.SetBands(Value: TcxTreeListBands);
begin
  FBands.Assign(Value);
end;

procedure TcxCustomTreeList.SetColumn(AIndex: Integer; Value: TcxTreeListColumn);
begin
  TcxTreeListColumn(ColumnsList[AIndex]).Assign(Value);
end;

procedure TcxCustomTreeList.SetDateTimeHandling(AValue: TcxTreeListDateTimeHandling);
begin
  inherited DateTimeHandling := AValue;
end;

procedure TcxCustomTreeList.SetDefaultRowHeight(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> DefaultRowHeight then
  begin
    FDefaultRowHeight := AValue;
    ImagesChanged(nil);
  end;
end;

procedure TcxCustomTreeList.SetDefaultLayout(AValue: Boolean);
begin
  if AValue <> FDefaultLayout then
  begin
    FDefaultLayout := AValue;
    if AValue then
      MakeDefaultLayout;
  end;
end;

procedure TcxCustomTreeList.SetFilterBox(AValue: TcxTreeListFilterBox);
begin
  inherited FilterBox := AValue;
end;

procedure TcxCustomTreeList.SetFiltering(AValue: TcxTreeListFiltering);
begin
  inherited Filtering := AValue;
end;

procedure TcxCustomTreeList.SetFocusedColumn(AValue: TcxTreeListColumn);
begin
  if (AValue <> nil) and AValue.ActuallyVisible then
    Controller.FocusedItem := AValue
  else
    if AValue = nil then
      Controller.FocusedItem := AValue;
end;

procedure TcxCustomTreeList.SetFocusedNodeProp(Value: TcxTreeListNode);
begin
  SetFocusedNode(Value, []);
  CheckEvents;
end;

procedure TcxCustomTreeList.SetImages(AValue: TCustomImageList);
begin
  FDefaultLevelInfo.Images := AValue;
end;

procedure TcxCustomTreeList.SetIsRestoring(AValue: Boolean);
var
  I: Integer;
begin
  if FIsRestoring <> AValue then
  begin
    FIsRestoring := AValue;
    if not FIsRestoring then
      AssignItemsPosition(False)
    else
      for I := 0 to ColumnCount - 1 do
        Columns[I].Position.Store(False);
  end;
end;

procedure TcxCustomTreeList.SetLastVisibleNode(AValue: TcxTreeListNode);
begin
  ViewInfo.LastNode := AValue;
  LayoutChanged;
end;

procedure TcxCustomTreeList.SetOnEdited(Value: TcxTreeListColumnChangedEvent);
begin
  inherited OnEdited := TcxecItemEvent(Value);
end;

procedure TcxCustomTreeList.SetOnEditing(Value: TcxTreeListEditingEvent);
begin
  inherited OnEditing := TcxecEditingEvent(Value);
end;

procedure TcxCustomTreeList.SetOnEditValueChanged(
  Value: TcxTreeListColumnChangedEvent);
begin
  inherited OnEditValueChanged := TcxecItemEvent(Value);
end;

procedure TcxCustomTreeList.SetOptionsBehavior(
  Value: TcxTreeListOptionsBehavior);
begin
  OptionsBehavior.Assign(Value);
end;

procedure TcxCustomTreeList.SetOptionsCustomizing(
  Value: TcxTreeListOptionsCustomizing);
begin
  FOptionsCustomizing.Assign(Value);
end;

procedure TcxCustomTreeList.SetOptionsData(Value: TcxTreeListOptionsData);
begin
  inherited OptionsData := Value;
end;

procedure TcxCustomTreeList.SetOptionsView(Value: TcxTreeListOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxCustomTreeList.SetPopupMenus(Value: TcxTreeListPopupMenus);
begin
  FPopupMenus.Assign(Value);
end;

procedure TcxCustomTreeList.SetOptionsSelection(
  Value: TcxTreeListOptionsSelection);
begin
  FOptionsSelection.Assign(Value);
end;

procedure TcxCustomTreeList.SetPreview(Value: TcxTreeListPreview);
begin
  FPreview.Assign(Value);
end;

procedure TcxCustomTreeList.SetSearchingText(const Value: string);
begin
  if Value = '' then
    CancelSearching
  else
    Controller.IncSearchingText := Value
end;

procedure TcxCustomTreeList.SetSorted(Value: Boolean);
var
  I: Integer;
begin
  if not Value then
  begin
    for I := 0 to ColumnCount - 1 do
      Columns[I].SortOrder := soNone;
  end;
  Root.AlphaSort(True);
end;

procedure TcxCustomTreeList.SetStateImages(AValue: TCustomImageList);
begin
  FDefaultLevelInfo.StateImages := AValue;
end;

procedure TcxCustomTreeList.SetStyles(Value: TcxTreeListStyles);
begin
  Styles.Assign(Value);
end;

procedure TcxCustomTreeList.SetTopVisibleNode(ANode: TcxTreeListNode);
begin
  if (ANode <> TopVisibleNode) and (ANode <> nil) and ANode.IsVisible then
  begin
    FTopVisibleNode := ANode;
    DoTopRecordIndexChanged;
    LayoutChanged;
  end;
end;

procedure TcxCustomTreeList.SetVisibleColumn(
  Index: Integer; AValue: TcxTreeListColumn);
begin
  VisibleColumns[Index].Assign(AValue);
end;

procedure TcxCustomTreeList.DoChangedTimer(Sender: TObject);
begin
  if not IsDestroying and not IsLoading then
    DoChanged(False);
end;

procedure TcxCustomTreeList.RestoreFrom(AStorageType: TcxStorageType;
  const AStorageName: string; AStorageStream: TStream;
  ACreateChildren, ADeleteChildren: Boolean; const ARestoreTreeListName: string);
var
  AStorage: TcxStorage;
  AModes: TcxStorageModes;
begin
  FStoringName := ARestoreTreeListName;
  AStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    AModes := [];
    if ACreateChildren then
      Include(AModes, smChildrenCreating);
    if ADeleteChildren then
      Include(AModes, smChildrenDeleting);
    AStorage.Modes := AModes;
    AStorage.UseInterfaceOnly := True;
    if ARestoreTreeListName = '' then
      AStorage.NamePrefix := Name;
    BeginUpdate;
    try
      IsRestoring := True;
      try
        case AStorageType of
          stIniFile: AStorage.RestoreFromIni(Self);
          stRegistry: AStorage.RestoreFromRegistry(Self);
          stStream: AStorage.RestoreFromStream(Self);
        end;
      finally
        IsRestoring := False;
      end;
    finally
      FSortedColumns.Sort(@cxCompareColumnsBySortIndex);
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomTreeList.StoreTo(AStorageType: TcxStorageType; const AStorageName: string;
  AStorageStream: TStream; AReCreate: Boolean; const ASaveTreeListName: string);
var
  AStorage: TcxStorage;
begin
  FStoringName := ASaveTreeListName;
  AStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    AStorage.UseInterfaceOnly := True;
    if ASaveTreeListName = '' then
      AStorage.NamePrefix := Name;
    AStorage.ReCreate := ARecreate;
    case AStorageType of
      stIniFile: AStorage.StoreToIni(Self);
      stRegistry: AStorage.StoreToRegistry(Self);
      stStream: AStorage.StoreToStream(Self);
    end;
    AStorage.ReCreate := False;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomTreeList.UpdateDesignerForms;
var
  I: Integer;
  AIntf: IcxTreeListDesigner;
begin
  for I := 0 to Designers.Count - 1 do
    if Supports(TObject(Designers[I]), IcxTreeListDesigner, AIntf) then
    begin
      if IsDestroying then
        AIntf.ComponentRemoved(Self)
      else
        AIntf.ComponentModified;
    end;
end;

function TcxCustomTreeList.GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
begin
  Result := FConditionalFormattingProvider;
end;

function TcxCustomTreeList.GetObjectName: string;
begin
  Result := GetStoredObjectName;
end;

function TcxCustomTreeList.GetProperties(AProperties: TStrings): Boolean;
begin
 Result := GetStoredObjectProperties(AProperties);
end;

procedure TcxCustomTreeList.GetPropertyValue(
  const AName: string; var AValue: Variant);
begin
  GetStoredPropertyValue(AName, AValue);
end;

procedure TcxCustomTreeList.SetPropertyValue(
  const AName: string; const AValue: Variant);
begin
  SetStoredPropertyValue(AName, AValue);
end;

procedure TcxCustomTreeList.CMDrag(var Message: TCMDrag);
var
  C: Integer;
const
  AcceptedCursors: array[Boolean, Boolean] of TCursor =
   ((crDrag, crDragCopy), (crcxDragMulti, crcxMultiDragCopy));
begin
  C := 0;
  if Assigned(Controller.DragNodesList) then
    C := Controller.DragNodesList.Count;
  if (DragCursor = crDefault) or Controller.DragCursorWasChanged then
  begin
    DragCursor := AcceptedCursors[C > 1,
      Controller.IsDragCopy];
    Controller.DragCursorWasChanged := True;
  end;
  inherited;
end;

procedure TcxCustomTreeList.WMCheckState(var Message: TMessage);
var
  AIndex: Integer;
begin
  AIndex := Message.LParam;
  if (AIndex >= 0) and (AIndex < AbsoluteItemsList.Count) then
    AbsoluteItems[AIndex].UpdateCheckStates;
  Invalidate;
end;

{ TcxTreeListColumnStyles }

constructor TcxTreeListColumnStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
end;

procedure TcxTreeListColumnStyles.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListColumnStyles then
  begin
    Header := TcxTreeListColumnStyles(Source).Header;
    Footer := TcxTreeListColumnStyles(Source).Footer;
  end;
  inherited Assign(Source);
end;

function TcxTreeListColumnStyles.GetContentParams(
  ANode: TcxTreeListNode): TcxViewParams;
begin
  GetViewParams(ecs_Content, ANode, nil, Result);
end;

procedure TcxTreeListColumnStyles.GetDefaultViewParams(
  Index: Integer; AData: TObject; out AParams: TcxViewParams);
var
  ACellPos: TcxTreeListCellPos;
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    ecs_Content:
      begin
        if Column.IsPreview then
          TreeList.Styles.DoGetPreviewParams(TcxTreeListNode(AData), AParams)
        else
          if Band <> nil then
            Band.Styles.DoGetContentParams(TcxTreeListNode(AData), Column, AParams)
          else
            TreeList.Styles.DoGetContentParams(TcxTreeListNode(AData), Column, AParams);
      end;
    tlcs_Footer:
      if Band <> nil then
      begin
        ACellPos := TcxTreeListCellPos.Create(TcxTreeListNode(AData), Column);
        try
          Band.Styles.GetViewParams(tlbs_Footer, ACellPos, nil, AParams);
        finally
          ACellPos.Free;
        end;
      end;
    tlcs_Header:
      TreeList.Styles.DoGetColumnHeaderParams(Column, AParams);
  end;
end;

function TcxTreeListColumnStyles.GetBand: TcxTreeListBand;
begin
  Result := Column.Position.Band;
end;

function TcxTreeListColumnStyles.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(GetOwner);
end;

function TcxTreeListColumnStyles.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(Control);
end;

{ TcxTreeListColumnPosition }

constructor TcxTreeListColumnPosition.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBandIndex := -1;
  FColIndex := -1;
  FLineCount := 1;
  FRowIndex := -1;
  FVisibleColIndex := -1;
end;

procedure TcxTreeListColumnPosition.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListColumnPosition then
  begin
    Self.FBandIndex := TcxTreeListColumnPosition(Source).BandIndex;
    Self.FRowIndex := TcxTreeListColumnPosition(Source).RowIndex;
    Self.FColIndex := TcxTreeListColumnPosition(Source).ColIndex;
    Self.FLineCount := TcxTreeListColumnPosition(Source).LineCount;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListColumnPosition.SetPosition(
  AColIndex, ARowIndex: Integer; IsInsertRow: Boolean = False);
begin
  SetPositionEx(BandIndex, AColIndex, ARowIndex, IsInsertRow);
end;

procedure TcxTreeListColumnPosition.SetPositionEx(
  ABandIndex, AColIndex, ARowIndex: Integer; IsInsertRow: Boolean = False);
begin
  TreeList.BeginUpdate;
  try
    BandIndex := ABandIndex;
    ColIndex := AColIndex;
    if Band <> nil then
    begin
      if not IsInsertRow or not cxInRange(ARowIndex, 0, Band.BandRows.Count) then
        RowIndex := ARowIndex
      else
        Row := Band.BandRows.Insert(ARowIndex);
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumnPosition.Changed;
begin
  TreeList.StructureChanged;
end;

function TcxTreeListColumnPosition.IsPositionChanged: Boolean;
begin
  Result := (BandIndex <> FBandIndex) or (ColIndex <> FColIndex) or
    (RowIndex <> FRowIndex);
end;

procedure TcxTreeListColumnPosition.Restore(ABandsIndexOnly: Boolean);
begin
  BandIndex := FBandIndex;
  if ABandsIndexOnly then Exit;
  RowIndex := FRowIndex;
  ColIndex := FColIndex;
end;

procedure TcxTreeListColumnPosition.Store(ABandIndexOnly: Boolean);
begin
  FBandIndex := BandIndex;
  if ABandIndexOnly then Exit;
  FColIndex := ColIndex;
  FRowIndex := RowIndex;
end;

function TcxTreeListColumnPosition.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(GetOwner);
end;

function TcxTreeListColumnPosition.GetIsUpdating: Boolean;
begin
  Result := Column.IsLoading or Column.IsUpdating or
    TreeList.IsRestoring or TreeList.IsLoading or TreeList.IsUpdating;
end;

function TcxTreeListColumnPosition.GetTreeList: TcxCustomTreeList;
begin
  Result := Column.TreeList;
end;

function TcxTreeListColumnPosition.GetBandIndex: Integer;
begin
  if FBand = nil then
    Result := -1
  else
    Result := FBand.Index;
end;

function TcxTreeListColumnPosition.GetColIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.IndexOf(Column);
end;

function TcxTreeListColumnPosition.GetRowIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.Index;
end;

function TcxTreeListColumnPosition.GetVisibleBandIndex: Integer;
begin
  if FBand = nil then
    Result := -1
  else
    Result := FBand.VisibleIndex;
end;

function TcxTreeListColumnPosition.GetVisibleRowIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.VisibleIndex;
end;

procedure TcxTreeListColumnPosition.SetBandIndex(AValue: Integer);
var
  ANewBand: TcxTreeListBand;
begin
  if Column.IsReading or Column.IsUpdating or
    (TreeList.IsRestoring and not Column.IsDestroying and ((Band = nil) or not Band.FIsDestroying)) then
  begin
    FBandIndex := AValue;
    Exit;
  end;
  if BandIndex = AValue then Exit;
  if (0 <= AValue) and (AValue < TreeList.Bands.Count) then
    ANewBand := TreeList.Bands[AValue]
  else
  begin
    if (Column <> nil) and Column.Focused and not TreeList.IsDestroying then
      TreeList.Controller.Navigator.FocusNextCell(False, False);
    ANewBand := nil;
  end;
  if (ANewBand <> nil) and not ANewBand.IsBottom then
    ANewBand.MoveBandsToRoot;
  TreeList.BeginUpdate;
  try
    if FBand <> nil then
      FBand.DeleteColumn(Column);
    FBand := nil;
    if ANewBand <> nil then
      ANewBand.AddColumn(Column);
    Changed;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumnPosition.SetColIndex(AValue: Integer);
begin
  if IsUpdating then
    FColIndex := AValue
  else
    if (FBand <> nil) and (ColIndex >= 0) and (AValue >= 0) then
    begin
      AValue := Min(Row.Count - 1, AValue);
      Row.FItems.Move(ColIndex, AValue);
      Changed;
    end;
end;

procedure TcxTreeListColumnPosition.SetLineCount(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FLineCount <> AValue then
  begin
    FLineCount := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnPosition.SetRow(AValue: TcxTreeListBandRow);
begin
  if AValue.Band <> Band then
    BandIndex := AValue.Band.Index;
  if FRow = AValue then Exit;
  if FRow <> nil then
  begin
    FRow.FItems.Remove(Column);
    FRow.CheckEmpty;
  end;
  if AValue <> nil then
    AValue.FItems.Add(Column);
  FRow := AValue;
  Changed;
end;

procedure TcxTreeListColumnPosition.SetRowIndex(AValue: Integer);
begin
  if IsUpdating then
    FRowIndex := AValue
  else
  begin
    if (FBand = nil) or (RowIndex = AValue) then Exit;
    if AValue >= 0 then
      Row := Band.BandRows.CheckRowIndex(AValue);
  end;
end;

{ TcxTreeListCaption }

constructor TcxTreeListCaption.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FShowEndEllipsis := True;
  FGlyphAlignVert := cxDefaultAlignmentVert;
  FAlignVert := cxDefaultAlignmentVert;
end;

destructor TcxTreeListCaption.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TcxTreeListCaption.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListCaption then
  begin
    FAlignHorz := TcxTreeListCaption(Source).FAlignHorz;
    FAlignVert := TcxTreeListCaption(Source).FAlignVert;
    FText := TcxTreeListCaption(Source).Text;
    FGlyphAlignHorz := TcxTreeListCaption(Source).FGlyphAlignHorz;
    FGlyphAlignVert := TcxTreeListCaption(Source).FGlyphAlignVert;
    FMultiLine := TcxTreeListCaption(Source).FMultiLine;
    FShowEndEllipsis := TcxTreeListCaption(Source).FShowEndEllipsis;
    Glyph := TcxTreeListCaption(Source).Glyph;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListCaption.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TcxTreeListCaption.GetText: string;
begin
  Result := FText;
  if (Result = '') and (GetOwner is TcxTreeListColumn) then
    Result := TcxTreeListColumn(GetOwner).GetDefaultCaption;
end;

function TcxTreeListCaption.IsTextStored: Boolean;
begin
  Result := FText <> '';
  if Result and (GetOwner is TcxTreeListColumn) then
    Result := FText <> TcxTreeListColumn(GetOwner).GetDefaultCaption;
end;

procedure TcxTreeListCaption.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

function TcxTreeListCaption.IsAlignVertStored: Boolean;
begin
  Result := FAlignVert <> cxDefaultAlignmentVert;
end;

function TcxTreeListCaption.IsGlyphAlignVertStored: Boolean;
begin
  Result := FGlyphAlignVert <> cxDefaultAlignmentVert;
end;

procedure TcxTreeListCaption.SetAlignHorz(AValue: TAlignment);
begin
  if AValue <> FAlignHorz then
  begin
    FAlignHorz := AValue;
    Changed;
  end;
end;

procedure TcxTreeListCaption.SetAlignVert(AValue: TcxAlignmentVert);
begin
  if AValue <> FAlignVert then
  begin
    FAlignVert := AValue;
    Changed;
  end;
end;

procedure TcxTreeListCaption.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TcxTreeListCaption.SetGlyphAlignHorz(AValue: TAlignment);
begin
  if AValue <> FGlyphAlignHorz then
  begin
    FGlyphAlignHorz := AValue;
    if not FGlyph.Empty then
      Changed;
  end;
end;

procedure TcxTreeListCaption.SetGlyphAlignVert(AValue: TcxAlignmentVert);
begin
  if AValue <> FGlyphAlignVert then
  begin
    FGlyphAlignVert := AValue;
    if not FGlyph.Empty then
      Changed;
  end;
end;

procedure TcxTreeListCaption.SetMultiLine(AValue: Boolean);
begin
  if AValue <> FMultiLine then
  begin
    FMultiLine := AValue;
    Changed;
  end;
end;

procedure TcxTreeListCaption.SetShowEndEllipsis(AValue: Boolean);
begin
  if AValue <> FShowEndEllipsis then
  begin
    FShowEndEllipsis := AValue;
    Changed;
  end;
end;

procedure TcxTreeListCaption.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Changed;
  end;
end;

{ TcxTreeListColumnFilterPopupOptions }

function TcxTreeListColumnFilterPopupOptions.GetIncrementalFilteringOptionsClass: TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptionsClass;
begin
  Result := TcxTreeListColumnFilterPopupIncrementalFilteringOptions;
end;

function TcxTreeListColumnFilterPopupOptions.GetIncrementalFiltering: TcxTreeListColumnFilterPopupIncrementalFilteringOptions;
begin
  Result := TcxTreeListColumnFilterPopupIncrementalFilteringOptions(inherited IncrementalFiltering);
end;

procedure TcxTreeListColumnFilterPopupOptions.SetIncrementalFiltering(AValue: TcxTreeListColumnFilterPopupIncrementalFilteringOptions);
begin
  inherited IncrementalFiltering := AValue;
end;

{ TcxTreeListColumnOptions }

constructor TcxTreeListColumnOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  RestoreDefaults;
end;

procedure TcxTreeListColumnOptions.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListColumnOptions then
  begin
    FCellEndEllipsis := TcxTreeListColumnOptions(Source).CellEndEllipsis;
    FEditAutoHeight := TcxTreeListColumnOptions(Source).EditAutoHeight;
    FSizing := TcxTreeListColumnOptions(Source).Sizing;
    FFooter := TcxTreeListColumnOptions(Source).Footer;
    FHidden := TcxTreeListColumnOptions(Source).Hidden;
    FGroupFooter := TcxTreeListColumnOptions(Source).FGroupFooter;
    FVertSizing := TcxTreeListColumnOptions(Source).VertSizing;
  end;
  inherited Assign(Source);
end;

procedure TcxTreeListColumnOptions.RestoreDefaults;
begin
  FCellEndEllipsis := True;
  FEditAutoHeight := ieahDefault;
  FSizing := True;
  FFooter := True;
  FHidden := False;
  FGroupFooter := True;
  Sorting := True;
  Customizing := True;
  Editing := True;
  TabStop := True;
  Focusing := True;
  Moving := True;
  VertSizing := True;
  Changed;
end;

procedure TcxTreeListColumnOptions.Changed;
begin
  Column.Changed;
end;

function TcxTreeListColumnOptions.GetExcelFilterPopupOptionsClass: TcxCustomEditContainerItemExcelFilterPopupOptionsClass;
begin
  Result := TcxTreeListColumnExcelFilterPopupOptions;
end;

function TcxTreeListColumnOptions.GetFilterPopupOptionsClass: TcxCustomEditContainerItemFilterPopupOptionsClass;
begin
  Result := TcxTreeListColumnFilterPopupOptions;
end;

function TcxTreeListColumnOptions.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(EditContainer);
end;

function TcxTreeListColumnOptions.GetExcelFilterPopup: TcxTreeListColumnExcelFilterPopupOptions;
begin
  Result := TcxTreeListColumnExcelFilterPopupOptions(inherited ExcelFilterPopup);
end;

function TcxTreeListColumnOptions.GetFilterPopup: TcxTreeListColumnFilterPopupOptions;
begin
  Result := TcxTreeListColumnFilterPopupOptions(inherited FilterPopup);
end;

procedure TcxTreeListColumnOptions.SetCellEndEllipsis(AValue: Boolean);
begin
  if AValue <> FCellEndEllipsis then
  begin
    FCellEndEllipsis := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnOptions.SetEditAutoHeight(AValue: TcxItemInplaceEditAutoHeight);
begin
  if FEditAutoHeight <> AValue then
  begin
    FEditAutoHeight := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnOptions.SetExcelFilterPopup(AValue: TcxTreeListColumnExcelFilterPopupOptions);
begin
  inherited ExcelFilterPopup := AValue;
end;

procedure TcxTreeListColumnOptions.SetFilterPopup(AValue: TcxTreeListColumnFilterPopupOptions);
begin
  inherited FilterPopup := AValue;
end;

procedure TcxTreeListColumnOptions.SetFooter(AValue: Boolean);
begin
  if AValue <> FFooter then
  begin
    FFooter := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnOptions.SetGroupFooter(AValue: Boolean);
begin
  if AValue <> FGroupFooter then
  begin
    FGroupFooter := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnOptions.SetSizing(AValue: Boolean);
begin
  if AValue <> FSizing then
  begin
    FSizing := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumnOptions.SetVertSizing(AValue: Boolean);
begin
  if AValue <> FVertSizing then
  begin
    FVertSizing := AValue;
    Changed;
  end;
end;

{ TcxTreeListSummaryItem }

constructor TcxTreeListSummaryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAbsoluteIndex := -1;
  FAllNodes := True;
  FVisible := True;
end;

function TcxTreeListSummaryItem.FormatValue(const AValue: Variant; AIsFooter: Boolean): string;
begin
  Result := inherited FormatValue(AValue, AIsFooter);
  if Assigned(FOnGetText) then
    FOnGetText(Self, AValue, Result);
end;

procedure TcxTreeListSummaryItem.AssignValues(Source: TcxCustomDataSummaryItem);
begin
  inherited AssignValues(Source);
  if Source is TcxTreeListSummaryItem then
  begin
    FCalculatedColumn := Column.TreeList.GetSameColumn(
      TcxTreeListSummaryItem(Source).CalculatedColumn);
    AllNodes := TcxTreeListSummaryItem(Source).AllNodes;
    Visible := TcxTreeListSummaryItem(Source).Visible;
    OnGetText := TcxTreeListSummaryItem(Source).OnGetText;
  end;
end;

procedure TcxTreeListSummaryItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AlignHorzAssigned', ReadAlignHorz, WriteAlignHorz, AlignHorzAssigned);
  Filer.DefineProperty('AlignVertAssigned', ReadAlignVert, WriteAlignVert, AlignVertAssigned);
end;

function TcxTreeListSummaryItem.GetDataController: TcxCustomDataController;
begin
  Result := nil;
end;

function TcxTreeListSummaryItem.GetValueFormat(AValueType: TcxSummaryValueType; const AValue: Variant;
  AIsFooter: Boolean): string;
begin
  Result := inherited GetValueFormat(AValueType, AValue, AIsFooter);
  if Result = '' then
    Result := cxDataGetDataSummaryValueDefaultFormat(AValueType, AValue, AIsFooter);
end;

procedure TcxTreeListSummaryItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCalculatedColumn) then
    CalculatedColumn := nil;
end;

function TcxTreeListSummaryItem.GetAlignHorz: TAlignment;
begin
  Result := FAlignHorz;
  if AlignHorzAssigned or (Column.PropertiesValue = nil) then Exit;
  Result := Column.PropertiesValue.Alignment.Horz;
end;

function TcxTreeListSummaryItem.GetAlignVert: TcxAlignmentVert;
begin
  Result := FAlignVert;
  if AlignVertAssigned or (Column.PropertiesValue = nil) then Exit;
  Result := TcxAlignmentVert(Column.PropertiesValue.Alignment.Vert);
end;

function TcxTreeListSummaryItem.GetCalculatedColumn: TcxTreeListColumn;
begin
  if FCalculatedColumn <> nil then
    Result := FCalculatedColumn
  else
    Result := Column;
end;

function TcxTreeListSummaryItem.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListSummaryItems(Collection).Column;
end;

function TcxTreeListSummaryItem.IsAlignHorzStored: Boolean;
begin
  Result := AlignHorzAssigned or (AlignHorz = taLeftJustify);
end;

function TcxTreeListSummaryItem.IsAlignVertStored: Boolean;
begin
  Result := AlignVertAssigned or (AlignVert = vaTop);
end;

function TcxTreeListSummaryItem.IsCalculatedColumnStored: Boolean;
begin
  Result := FCalculatedColumn <> nil;
end;

procedure TcxTreeListSummaryItem.SetAlignHorz(AValue: TAlignment);
begin
  if AValue <> FAlignHorz then
  begin
    FAlignHorz := AValue;
    FAlignHorzAssigned := True;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.SetAlignVert(AValue: TcxAlignmentVert);
begin
  if AValue <> FAlignVert then
  begin
    FAlignVert := AValue;
    FAlignVertAssigned := True;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.SetAllNodes(AValue: Boolean);
begin
  if AValue <> FAllNodes then
  begin
    FAllNodes := AValue;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.SetCalculatedColumn(AValue: TcxTreeListColumn);
begin
  if AValue <> FCalculatedColumn then
  begin
    FCalculatedColumn := AValue;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.SetMultiLine(AValue: Boolean);
begin
  if AValue <> FMultiLine then
  begin
    FMultiLine := AValue;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

procedure TcxTreeListSummaryItem.ReadAlignHorz(Reader: TReader);
begin
  FAlignHorzAssigned := Reader.ReadBoolean;
end;

procedure TcxTreeListSummaryItem.ReadAlignVert(Reader: TReader);
begin
  FAlignVertAssigned := Reader.ReadBoolean;
end;

procedure TcxTreeListSummaryItem.WriteAlignHorz(Writer: TWriter);
begin
  Writer.WriteBoolean(FAlignHorzAssigned);
end;

procedure TcxTreeListSummaryItem.WriteAlignVert(Writer: TWriter);
begin
  Writer.WriteBoolean(FAlignVertAssigned);
end;

{ TcxTreeListColumnSummary }

procedure TcxTreeListColumnSummary.Assign(Source: TPersistent);
begin
  if Source is TcxTreeListColumnSummary then
  begin
    GroupFooterSummaryItems := TcxTreeListColumnSummary(Source).GroupFooterSummaryItems;
    FooterSummaryItems := TcxTreeListColumnSummary(Source).FooterSummaryItems;
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListColumnSummary.Changed(ARedrawOnly: Boolean);
begin
  Column.TreeList.AddChanges([tcStructure, tcSummary]);
  Column.TreeList.LayoutChanged;
end;

constructor TcxTreeListColumnSummary.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFooterSummaryItems := TcxTreeListSummaryItems.Create(Self, TcxTreeListSummaryItem);
  FGroupFooterSummaryItems := TcxTreeListSummaryItems.Create(Self, TcxTreeListSummaryItem);
end;

destructor TcxTreeListColumnSummary.Destroy;
begin
  FreeAndNil(FGroupFooterSummaryItems);
  FreeAndNil(FFooterSummaryItems);
  inherited Destroy;
end;

function TcxTreeListColumnSummary.GetColumn: TcxTreeListColumn;
begin
  Result := TcxTreeListColumn(GetOwner);
end;

function TcxTreeListColumnSummary.GetFooterVisibleCount: Integer;
begin
  Result := FooterSummaryItems.Count;
end;

function TcxTreeListColumnSummary.GetGroupFooterVisibleCount: Integer;
begin
  Result := GroupFooterSummaryItems.Count;
end;

procedure TcxTreeListColumnSummary.SetFooterSummaryItems(
  AValue: TcxTreeListSummaryItems);
begin
  FFooterSummaryItems.Assign(AValue);
end;

procedure TcxTreeListColumnSummary.SetGroupFooterSummaryItems(
  AValue: TcxTreeListSummaryItems);
begin
  FGroupFooterSummaryItems.Assign(AValue);
end;

{ TcxTreeListSummaryItems }

constructor TcxTreeListSummaryItems.Create(ASummary: TcxTreeListColumnSummary; AItemClass: TcxTreeListSummaryItemClass);
begin
  inherited Create(AItemClass);
  FSummary := ASummary;
end;

function TcxTreeListSummaryItems.Add: TcxTreeListSummaryItem;
begin
  BeginUpdate;
  try
    Result := TcxTreeListSummaryItem(inherited Add);
  finally
    EndUpdate;
  end;
end;

function TcxTreeListSummaryItems.GetItemByKind(AKind: TcxSummaryKind): TcxTreeListSummaryItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Kind = AKind then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxTreeListSummaryItems.GetOwner: TPersistent;
begin
  Result := FSummary;
end;

procedure TcxTreeListSummaryItems.Update(Item: TCollectionItem);
var
  I: Integer;
  AItem: TcxTreeListSummaryItem;
begin
  FVisibleCount := 0;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Visible then
    begin
      AItem.FVisibleIndexInColumn := FVisibleCount;
      Inc(FVisibleCount);
    end
    else
      AItem.FVisibleIndexInColumn := -1;
  end;
  FSummary.Changed(Item <> nil);
end;

function TcxTreeListSummaryItems.GetItem(
  AIndex: Integer): TcxTreeListSummaryItem;
begin
  Result := TcxTreeListSummaryItem(inherited GetItem(AIndex));
end;

procedure TcxTreeListSummaryItems.SetItem(AIndex: Integer;
  AValue: TcxTreeListSummaryItem);
begin
  inherited SetItem(AIndex, AValue);
end;

function TcxTreeListSummaryItems.GetColumn: TcxTreeListColumn;
begin
  Result := Summary.Column;
end;

type
  TcxTreeListDeprecatedColumnSummaryFooter = class(TcxOwnedPersistent)
  private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FFormat: string;
    FKind: TcxSummaryKind;
    FMultiLine: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AlignHorz: TAlignment read FAlignHorz write FAlignHorz;
    property AlignVert: TcxAlignmentVert read FAlignVert write FAlignVert;
    property Format: string read FFormat write FFormat;
    property Kind: TcxSummaryKind read FKind write FKind;
    property MultiLine: Boolean read FMultiLine write FMultiLine;
  end;

{ TcxTreeListDeprecatedColumnSummaryFooter }

procedure TcxTreeListDeprecatedColumnSummaryFooter.Assign(
  Source: TPersistent);
var
  ASourceSummaryFooter: TcxTreeListDeprecatedColumnSummaryFooter;
begin
  if Source is TcxTreeListDeprecatedColumnSummaryFooter then
  begin
    ASourceSummaryFooter := TcxTreeListDeprecatedColumnSummaryFooter(Source);
    AlignHorz := ASourceSummaryFooter.AlignHorz;
    AlignVert := ASourceSummaryFooter.AlignVert;
    Format := ASourceSummaryFooter.Format;
    Kind := ASourceSummaryFooter.Kind;
    MultiLine := ASourceSummaryFooter.MultiLine;
  end
  else
    inherited Assign(Source);
end;

{ TcxTreeListColumn }

constructor TcxTreeListColumn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortIndex := -1;
  FSummaryFooter := TcxTreeListDeprecatedColumnSummaryFooter.Create(Self);
  FSummary := GetSummaryClass.Create(Self);
  FCaption := TcxTreeListCaption.Create(Self);
  FCaption.OnChange := ChangeCaption;
  FPosition := GetPositionClass.Create(Self);
  FMinWidth := cxTreeListDefMinWidth;
  FVisible := True;
end;

destructor TcxTreeListColumn.Destroy;
begin
  CancelSorting;
  Position.BandIndex := -1;
  if TreeList <> nil then
    TreeList.HideEdit;
  if FHeaderCell <> nil then
    FHeaderCell.FItem := nil;
  if (TreeList <> nil) and not TreeList.IsDestroying  then
  begin
    TreeList.AddChanges([tcColumns]);
    TreeList.UpdateDesignerForms;
    TreeList.ViewInfo.SetDirty;
    if TreeList <> nil then
      TreeList.ColumnsList.Remove(Self);
    if Controller.FocusedItem = Self then
      Controller.FocusedItem := nil;
    if TreeList.Preview.Column = Self then
      TreeList.Preview.Column := nil;
    if TreeList.OptionsView.CategorizedColumn = Self then
      TreeList.OptionsView.CategorizedColumn := nil;
  end;
  FreeAndNil(FCaption);
  FreeAndNil(FPosition);
  FreeAndNil(FSummary);
  FreeAndNil(FSummaryFooter);
  inherited Destroy;
end;

procedure TcxTreeListColumn.Assign(Source: TPersistent);
var
  AColumn: TcxTreeListColumn;
begin
  TreeList.BeginUpdate;
  try
    inherited Assign(Source);
    if Source is TcxTreeListColumn then
    begin
      AColumn := TcxTreeListColumn(Source);
      FCalculatedWidth := AColumn.FCalculatedWidth;
      FMinWidth := AColumn.FMinWidth;
      FWidth := AColumn.FWidth;
      Position := TcxTreeListColumn(Source).Position;
      Caption := AColumn.Caption;
      SortOrder := AColumn.FSortOrder;
      Summary := AColumn.Summary;
      Visible := AColumn.Visible;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumn.ApplyBestFit;
var
  ANode: TcxTreeListNode;
  AWidth, ACount: Integer;
begin
  if (TreeList = nil) or TreeList.IsLoading or TreeList.IsDestroying then Exit;
  TreeList.ValidateStates;
  ANode := TreeList.GetStartNodeForBestFit;
  if ANode = nil then Exit;
  AWidth := MinWidth + IndentWidth;
  ACount := TreeList.GetCountNodeForBestFit;
  try
    TreeList.HideEdit;
    while (ACount <> 0) and (ANode <> nil) do
    begin
      AWidth := Max(AWidth, TcxFakeCellViewInfo(
        TreeList.ViewInfo.FakeCell).MeasureWidth(Self, ANode));
      ANode := ANode.GetNextVisible;
      Dec(ACount);
    end;
    AWidth := Max(AWidth, GetHeaderFooterBestFitSize);
    if FBestFitMaxWidth <> 0 then
      AWidth := Min(AWidth, FBestFitMaxWidth);
    SetSizeDelta(dsdHorz, AWidth - DisplayWidth);
  finally
    TreeList.Controller.CheckEdit;
  end;
end;

function TcxTreeListColumn.GetDisplayText(ANode: TcxTreeListNode): string;
begin
  Result := VarToStr(DoOnGetDisplayText(ANode, True));
end;

function TcxTreeListColumn.GetParentComponent: TComponent;
begin
  Result := EditingControl;
end;

function TcxTreeListColumn.HasParent: Boolean;
begin
  Result := True;
end;

procedure TcxTreeListColumn.MakeVisible;

  function CalculateFocusedNodeIndent: Integer;
  begin
    Result := 0;
    if TreeList.FocusedNode = nil then
      Exit;
    Result := TreeList.ViewInfo.GetLevelContentOffset(TreeList.FocusedNode.Level);
    if TreeList.FocusedNode.Level > 0 then
      Result := Result - TreeList.ViewInfo.GetLevelContentOffset(TreeList.FocusedNode.Level - 1);
  end;

  procedure SetPosition(APosition: Integer);
  begin
    if Abs(APosition) <= 2 then
      APosition := 0;
    TreeList.ViewInfo.HScrollPos := APosition;
  end;

var
  AMinWidth, ANodeIndent: Integer;
  AEditRect, R: TRect;
begin
  if not ActuallyVisible or (Position.Band.FixedKind <> tlbfNone) or
    (HeaderCell = nil) or not HeaderCell.HasClipping then Exit;
  R := TreeList.ViewInfo.HScrollArea;

  AMinWidth := MinWidth;
  AEditRect := HeaderCell.DisplayRect;
  if (cxRectWidth(AEditRect) > cxRectWidth(R)) and (FocusedCellViewInfo <> nil) then
  begin
    AEditRect := FocusedCellViewInfo.BoundsRect;
    if HasIndent then
    begin
      ANodeIndent := CalculateFocusedNodeIndent;
      if not HeaderCell.IsRightToLeftConverted then
        Dec(AEditRect.Left, ANodeIndent)
      else
        Inc(AEditRect.Right, ANodeIndent);
      Inc(AMinWidth, ANodeIndent);
    end;
  end;

  if not HeaderCell.IsRightToLeftConverted then
  begin
    if (AEditRect.Right > R.Right) and (AEditRect.Left > R.Left) then
    begin
      if not HasIndent then
        SetPosition(TreeList.ViewInfo.HScrollPos + (AEditRect.Right - R.Right))
      else
        if R.Right - AEditRect.Left < AMinWidth then
        begin
          if cxRectWidth(AEditRect) <= cxRectWidth(R) then
            SetPosition(TreeList.ViewInfo.HScrollPos + AEditRect.Right - cxRectWidth(R))
          else
            SetPosition(TreeList.ViewInfo.HScrollPos + AEditRect.Left);
        end;
    end
    else
      if AEditRect.Left < R.Left then
        SetPosition(TreeList.ViewInfo.HScrollPos + AEditRect.Left - R.Left)
  end
  else
  begin
    if (AEditRect.Left < R.Left) and (AEditRect.Right < R.Right) then
    begin
      if not HasIndent then
        SetPosition(TreeList.ViewInfo.HScrollPos + (R.Left - AEditRect.Left))
      else
        if AEditRect.Right - R.Left < AMinWidth then
        begin
          if cxRectWidth(AEditRect) <= cxRectWidth(R) then
            SetPosition(TreeList.ViewInfo.HScrollPos + AEditRect.Left - cxRectWidth(R))
          else
            SetPosition(TreeList.ViewInfo.HScrollPos + AEditRect.Right);
        end;
    end
    else
      if AEditRect.Right > R.Right then
        SetPosition(TreeList.ViewInfo.HScrollPos + R.Right - AEditRect.Right);
  end;
  TreeList.UpdateScrollBars;
end;

procedure TcxTreeListColumn.RestoreDefaults;
begin
  TreeList.BeginUpdate;
  try
    Options.RestoreDefaults;
    SortOrder := soNone;
    Caption.FAlignVert := vaTop;
    Caption.FAlignHorz := taLeftJustify;
    RestoreWidths;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumn.RestoreWidths;
begin
  TreeList.BeginUpdate;
  try
    FMinWidth := cxTreeListDefMinWidth;
    SetWidth(cxTreeListDefWidth);
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumn.AssignWidth;
begin
  if FWidth = 0 then
    FWidth := DisplayWidth;
end;

function TcxTreeListColumn.CanMoving: Boolean;
begin
  Result := TreeList.OptionsCustomizing.ColumnMoving and Options.Moving;
end;

function TcxTreeListColumn.CanSort: Boolean;
begin
  Result := not Assigned(OnGetEditProperties) and
    (esoSorting in PropertiesValue.GetSupportedOperations);
  if not Result then
    Result := Assigned(TreeList.FOnCompare);
end;

procedure TcxTreeListColumn.ChangeCaption(Sender: TObject);
begin
  Changed;
end;

procedure TcxTreeListColumn.Changed;
begin
  if (TreeList <> nil) and TreeList.IsRefreshFields then Exit;
  inherited Changed;
  if TreeList <> nil then
    TreeList.Summary.Recalculate;
end;

procedure TcxTreeListColumn.ForceWidth(AWidth: Integer);
begin
  TreeList.ValidateStates;
  AWidth := Max(AWidth, MinWidth);
  if Width = AWidth then Exit;
  TreeList.Controller.ForcingWidthColumn := Self;
  TreeList.Bands.AssignRowColumnsWidth(Position.Row);
  Width := AWidth;
  TreeList.Bands.Adjust();
  TreeList.Bands.AssignRowColumnsWidth(Position.Row);
  TreeList.Controller.ForcingWidthColumn := nil;
end;

function TcxTreeListColumn.CanEdit: Boolean;
begin
  Result := Assigned(ValueDef) and CanFocus and Options.Editing and
    TreeList.Options.OptionsData.Editing and (TreeList.FocusedNode <> nil) and
      not TreeList.FocusedNode.IsHidden;
end;

function TcxTreeListColumn.GetEditValue: Variant;
begin
  if Editing then
  begin
    if TreeList.FocusedNode = nil then
      Result := Null
    else
      Result := DataController.GetNodeValue(TreeList.FocusedNode, ItemIndex);
  end
  else
    Result := Unassigned;
end;

function TcxTreeListColumn.CanEditAutoHeight: Boolean;
begin
  Result := (Options.EditAutoHeight <> ieahNone) and (TreeList.GetEditAutoHeight <> eahNone) and
    (esoEditingAutoHeight in PropertiesValue.GetSupportedOperations);
end;

function TcxTreeListColumn.CanInitEditing: Boolean;
begin
  Result := CanEdit;
  if Result then
  begin
    if TreeList.DataController.EditingNode <> TreeList.FocusedNode then
      TreeList.Post;
    TreeList.DataController.Edit;
  end;
end;

procedure TcxTreeListColumn.CancelSorting;
begin
  FSortOrder := soNone;
  FSortIndex := -1;
  TreeList.FSortedColumns.Remove(Self);
  TreeList.AddChanges([tcSortOrder]);
end;

function TcxTreeListColumn.CanFind: Boolean;
begin
  Result := inherited CanFind and Visible;
end;

function TcxTreeListColumn.CanFocus: Boolean;
begin
  Result := inherited CanFocus and TreeList.OptionsSelection.CellSelect and
    (Self <> TreeList.Preview.Column);
end;

function TcxTreeListColumn.GetRealSortOrder: TcxDataSortOrder;
begin
  Result := soNone;
  if CanSort then
    Result := FSortOrder;
end;

function TcxTreeListColumn.GetEditAutoHeight: TcxInplaceEditAutoHeight;
begin
  if CanEditAutoHeight then
    Result := TreeList.GetEditAutoHeight
  else
    Result := eahNone;
end;

function TcxTreeListColumn.GetFilterCaption: string;
begin
  Result := Caption.Text;
end;

function TcxTreeListColumn.GetHeaderFooterBestFitSize: Integer;
begin
  Result := 0;
  if TreeList.OptionsView.Headers then
  begin
    Result := cxTextWidth(TreeList.Styles.GetColumnHeaderParams(Self).Font, Caption.Text) + ScaleFactor.Apply(cxTextOffset) * 4;
    if not Caption.Glyph.Empty and (Byte(Caption.GlyphAlignHorz) < 2) then
      Inc(Result, dxGetImageSize(Caption.Glyph, ScaleFactor).cx + ScaleFactor.Apply(cxTextOffset));
    if SortOrder <> soNone then
      Inc(Result, TreeList.LookAndFeelPainter.ScaledSortingMarkAreaSize(ScaleFactor).X);
  end;
  if Options.Footer and TreeList.OptionsView.Footer and (Summary.FooterVisibleCount > 0) then
    Result := Max(Result, GetSummaryBestFitSize(TreeList.Root, Summary.FooterSummaryItems));
end;

function TcxTreeListColumn.GetOptionsClass: TcxCustomEditContainerItemOptionsClass;
begin
  Result := TcxTreeListColumnOptions;
end;

function TcxTreeListColumn.GetStylesClass: TcxEditContainerStylesClass;
begin
  Result := TcxTreeListColumnStyles;
end;

function TcxTreeListColumn.GetSummaryBestFitSize(ANode: TcxTreeListNode;
  AItems: TcxTreeListSummaryItems): Integer;
var
  I: Integer;
  AText: string;
  AItemWidth: Integer;
  AViewParams: TcxViewParams;
  AItem: TcxTreeListSummaryItem;
begin
  Result := 0;
  if AItems.VisibleCount = 0 then Exit;
  for I := 0 to AItems.Count - 1 do
  begin
    AItem := AItems[I];
    if not AItem.Visible then Continue;
    if not TreeList.ViewInfo.MultiRows and (Length(AText) > 0) then
      AText := AText + dxFormatSettings.ListSeparator;
    AText :=  TreeList.Summary.GetGroupFooterSummaryText(AItem, ANode);
    AViewParams := TreeList.Styles.GetColumnFooterParams(Self, ANode, AItem);
    AItemWidth := cxTextWidth(AViewParams.Font, AText) + (
      ScaleFactor.Apply(cxTextOffset) +
      TreeList.LookAndFeelPainter.FooterCellBorderSize +
      TreeList.LookAndFeelPainter.FooterCellOffset) * 2;
    if TreeList.ViewInfo.MultiRows then
      Result := Max(Result, AItemWidth)
    else
      Inc(Result, AItemWidth);
  end;
end;

function TcxTreeListColumn.GetSummaryClass: TcxTreeListColumnSummaryClass;
begin
  Result := TcxTreeListColumnSummary;
end;

procedure TcxTreeListColumn.SetEditingControl(Value: TcxEditingControl);
begin
  if TreeList <> nil then
  begin
    TreeList.ContainerList.Remove(Self);
    TreeList.AddChanges([tcColumns]);
  end;
  inherited SetEditingControl(Value);
  if TreeList <> nil then
  begin
    TreeList.AddChanges([tcColumns]);
    TreeList.UpdateDesignerForms;
  end;
end;

procedure TcxTreeListColumn.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if Assigned(FOnChangeName) then
    FOnChangeName(Self);
end;

procedure TcxTreeListColumn.SetParentComponent(Value: TComponent);
begin
  if Value is TcxEditingControl then
    EditingControl := Value as TcxEditingControl
  else
    EditingControl := nil;
end;

function TcxTreeListColumn.CanSizing(ADirection: TcxDragSizingDirection): Boolean;
begin
  if ADirection = dsdVert then
    Result := Options.VertSizing and TreeList.OptionsCustomizing.ColumnVertSizing
  else
    Result := Options.Sizing and TreeList.OptionsCustomizing.ColumnHorzSizing;
end;

function TcxTreeListColumn.GetAvailableMaxWidth: Integer;
var
  I: Integer;
begin
  if Position.Band.Width = 0 then
    Result := High(SmallInt)
  else
  begin
    if Position.Band.Width = 0 then
      Result := Position.Band.Width - Position.Row.MinWidth
    else
    begin
      Result := Position.Band.CalculatedWidth;
      for I := 0 to Position.Row.VisibleItemCount - 1 do
      begin
        if I = Position.VisibleColIndex then Continue;
        if (I < Position.VisibleColIndex) and
          (Position.VisibleColIndex <> Position.Row.VisibleItemCount - 1) then
          Dec(Result, Position.Row.VisibleItems[I].DisplayWidth)
        else
          Dec(Result, Position.Row.VisibleItems[I].MinWidth);
      end;
    end;
  end;
end;

function TcxTreeListColumn.GetRealMinSize: Integer;
begin
  if Options.Sizing then
    Result := FMinWidth + IndentWidth
  else
    Result := FWidth;
end;

function TcxTreeListColumn.GetSizingBoundsRect(
  ADirection: TcxDragSizingDirection): TRect;
begin
  if ADirection = dsdVert then
    Result := cxRectSetTop(TreeList.ClientRect,
      HeaderCell.DisplayRect.Top + TreeList.ViewInfo.HeaderLineHeight)
  else
  begin
    if not HeaderCell.IsRightToLeftConverted then
      Result := cxRectSetLeft(TreeList.ClientRect,
        HeaderCell.DisplayRect.Left + GetRealMinSize, GetAvailableMaxWidth - GetRealMinSize)
    else
      Result := cxRectSetRight(TreeList.ClientRect,
        HeaderCell.DisplayRect.Right - GetRealMinSize, GetAvailableMaxWidth - GetRealMinSize);
  end;
end;

function TcxTreeListColumn.GetSizingIncrement(
  ADirection: TcxDragSizingDirection): Integer;
begin
  if ADirection = dsdHorz then
    Result := 1
  else
    Result := TreeList.ViewInfo.HeaderLineHeight;
end;

function TcxTreeListColumn.IsDynamicUpdate: Boolean;
begin
  Result := TreeList.OptionsCustomizing.DynamicSizing and
    TreeList.HitTest.HitAtSizingHorz;
end;

procedure TcxTreeListColumn.SetSizeDelta(
  ADirection: TcxDragSizingDirection; ADelta: Integer);
begin
  if ADelta = 0 then Exit;
  if TreeList.ViewInfo.IsRightToLeftConverted and (ADirection = dsdHorz) then
    ADelta := -ADelta;
  TreeList.BeginUpdate;
  try
    if ADirection = dsdVert then
      Position.LineCount := Position.LineCount + ADelta
    else
      ForceWidth(DisplayWidth + ADelta);
  finally
    TreeList.EndUpdate;
  end;
  TreeList.LayoutChanged;
  TreeList.Modified;
end;

function TcxTreeListColumn.GetObjectName: string;
begin
  if TreeList.StoringName = '' then
  begin
    Result := Name;
    if Result = '' then
      Result := ClassName + IntToStr(ItemIndex);
  end
  else
    Result := IntToStr(FStoreID);
end;

function TcxTreeListColumn.GetProperties(AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  for I := Low(ColumnPropertiesName) to High(ColumnPropertiesName) do
    AProperties.Add(ColumnPropertiesName[I]);
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

function TcxTreeListColumn.GetPropertyIndex(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(ColumnPropertiesName) to High(ColumnPropertiesName) do
  begin
    if AnsiCompareText(ColumnPropertiesName[I], AName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TcxTreeListColumn.GetPropertyValue(
  const AName: string; var AValue: Variant);
begin
  case GetPropertyIndex(AName) of
    0:
      AValue := Visible;
    1:
      AValue := ItemIndex;
    2:
      AValue := Position.LineCount;
    3:
      AValue := Position.ColIndex;
    4:
      AValue := Position.RowIndex;
    5:
      AValue := Position.BandIndex;
    6:
      AValue := Caption.Text;
    7:
      AValue := SortOrder;
    9:
      AValue := FMinWidth;
    10:
      AValue := FWidth;
  else
    if Assigned(OnGetStoredPropertyValue) then
      OnGetStoredPropertyValue(Self, AName, AValue);
  end;
end;

procedure TcxTreeListColumn.SetPropertyValue(
  const AName: string; const AValue: Variant);
begin
  case GetPropertyIndex(AName) of
    0:
      Visible := AValue;
    1:
      ItemIndex := AValue;
    2:
      Position.LineCount := AValue;
    3:
      Position.ColIndex := AValue;
    4:
      Position.RowIndex := AValue;
    5:
      Position.BandIndex := AValue;
    6:
      if Caption.Text = '' then
        Caption.Text := AValue;
    7:
      FSortOrder := AValue;
    9:
      FMinWidth := AValue;
    10:
      FWidth := AValue;
  else
    if Assigned(OnSetStoredPropertyValue) then
      OnSetStoredPropertyValue(Self, AName, AValue);
  end;
end;

procedure TcxTreeListColumn.ChangeScale(M, D: Integer);
var
  ASavedWidth: Integer;
begin
  ASavedWidth := Width;
  MinWidth := MulDiv(MinWidth, M, D);
  BestFitMaxWidth := MulDiv(BestFitMaxWidth, M, D);
  Width := MulDiv(ASavedWidth, M, D);
end;

procedure TcxTreeListColumn.ChangeSortOrder(ASortOrder: TcxDataSortOrder; AShift: TShiftState);
begin
  if not Options.Sorting or not CanSort then
    ASortOrder := FSortOrder;
  if ASortOrder <> FSortOrder then
  begin
    FSortOrder := ASortOrder;
    TreeList.ColumnSortOrderChanged(Self, AShift);
    TreeList.Modified;
  end;
end;

procedure TcxTreeListColumn.ConvertOldFooterSummaries;
var
  ASummaryFooter: TcxTreeListDeprecatedColumnSummaryFooter;
begin
  ASummaryFooter := TcxTreeListDeprecatedColumnSummaryFooter(SummaryFooter);
  if ASummaryFooter.Kind <> skNone then
    with FSummary.FooterSummaryItems.Add do
    begin
      AlignHorz := ASummaryFooter.AlignHorz;
      AlignVert := ASummaryFooter.AlignVert;
      MultiLine := ASummaryFooter.MultiLine;
      Format := ASummaryFooter.Format;
      Kind := ASummaryFooter.Kind;
    end;
end;

procedure TcxTreeListColumn.DoGetDisplayText(
  ARecordIndex: TdxNativeInt; var AText: string);
begin
  if ARecordIndex <> 0 then
    AText := DoGetNodeDisplayText(TcxTreeListNode(ARecordIndex), AText);
end;

function TcxTreeListColumn.DoGetNodeDisplayText(
  ANode: TcxTreeListNode; const AValue: Variant): Variant;
var
  AText: string;
begin
  Result := AValue;
  if Assigned(OnGetDisplayText) then
  begin
    AText := VarToStr(Result);
    OnGetDisplayText(Self, ANode, AText);
    Result := AText;
  end;
end;

function TcxTreeListColumn.DoOnGetDisplayText(
  ANode: TcxTreeListNode; AsText: Boolean = False; AValueOnly: Boolean = False): Variant;
var
  AProperties: TcxCustomEditProperties;
  AText: string;
begin
  AProperties := DoGetEditProperties(ANode);
  if (AProperties.GetEditValueSource(False) = evsValue) or VarIsNull(DataController.GetNodeValue(ANode, ItemIndex)) then
    Result := DataController.GetNodeValue(ANode, ItemIndex)
  else
    Result := DataController.GetNodeDisplayText(ANode, ItemIndex);
  if AValueOnly then Exit;
  if Assigned(OnGetDisplayText) or AsText then
  begin
    if (AProperties.GetEditValueSource(False) = evsValue) or VarIsNull(DataController.GetNodeValue(ANode, ItemIndex)) then
    begin
      if AsText then
        AText := AProperties.GetDisplayText(ANode.Values[ItemIndex], True)
      else
        AText := '';
      AText := DoGetNodeDisplayText(ANode, AText);
      if (AText = '') and not AsText then
        Result := ANode.Values[ItemIndex]
      else
        Result := AText;
    end
    else
      Result := DoGetNodeDisplayText(ANode, Result);
  end;
end;

procedure TcxTreeListColumn.DoValidateDrawValue(ANode: TcxTreeListNode;
  const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if Assigned(FOnValidateDrawValue) then
    FOnValidateDrawValue(Self, ANode, AValue, AData);
end;

function TcxTreeListColumn.GetCellHeight(ANode: TcxTreeListNode;
  AWidth, ALines: Integer; AFont: TFont; const AValue: Variant): Integer;
var
  ASizeProp: TcxEditSizeProperties;
  AEditProp: TcxCustomEditProperties;
  AViewData: TcxCustomEditViewData;
begin
  ASizeProp := cxDefaultEditSizeProperties;
  AEditProp := DoGetEditProperties(ANode);
  AViewData := nil;
  try
    if AEditProp = PropertiesValue then
      AViewData := EditViewData
    else
    begin
      AViewData := AEditProp.CreateViewData(TreeList.EditStyle, True);
      AViewData.ScaleFactor.Assign(TreeList.ScaleFactor);
    end;
    AViewData.Style.Font := AFont;
    ASizeProp.Width := AWidth;
    ASizeProp.MaxLineCount := ALines;
    Result := AViewData.GetEditSize(GetControlCanvas, AValue, ASizeProp).cy;
  finally
    if AViewData <> EditViewData then
      FreeAndNil(AViewData);
  end;
end;

function TcxTreeListColumn.GetCurrentValue: Variant;
begin
  if TreeList.FocusedNode <> nil then
    Result := TreeList.FocusedNode.Values[ItemIndex]
  else
    Result := Null;
end;

function TcxTreeListColumn.GetPositionClass: TcxTreeListColumnPositionClass;
begin
  Result := TcxTreeListColumnPosition;
end;

function TcxTreeListColumn.GetIsCurrency: Boolean;
begin
  Result := ValueDef.ValueTypeClass = TcxCurrencyValueType;
end;

function TcxTreeListColumn.GetIsTextStored: Boolean;
begin
  Result := False;
end;

function TcxTreeListColumn.HasDataTextHandler: Boolean;
begin
  Result := False;
end;

procedure TcxTreeListColumn.InitAutoWidthItem(AItem: TcxAutoWidthItem);
begin
  AItem.Width := Width;
  AItem.Fixed := IsFixed;
  AItem.MinWidth := FMinWidth + IndentWidth;
  if AItem.Fixed then
  begin
    AItem.Width := Max(AItem.Width, AItem.MinWidth);
    AItem.MinWidth := AItem.Width
  end;
  AItem.AutoWidth := -1;
end;

procedure TcxTreeListColumn.InitializeValueDef;
begin
  FIsCurrency := GetIsCurrency;
  FIsTextStored := GetIsTextStored;
  if ValueDef <> nil then
    TcxDataStorageHelper.SetTextStored(ValueDef, GetIsTextStored);
end;

function TcxTreeListColumn.IsVisibleInQuickCustomizationPopup: Boolean;
begin
  Result := Options.Customizing and not Options.Hidden and
    (FPosition.Band <> nil) and FPosition.Band.ActuallyVisible;
end;

procedure TcxTreeListColumn.SetCurrentValue(const Value: Variant);
begin
  if TreeList.FocusedNode <> nil then
    TreeList.FocusedNode.Values[ItemIndex] := Value;
end;

procedure TcxTreeListColumn.ValidateDrawValue(const AValue: Variant;
  AEditViewInfo: TcxEditCellViewInfo);
begin
  inherited;
  DoValidateDrawValue(TcxTreeListEditCellViewInfo(AEditViewInfo).Node, AValue,
    AEditViewInfo.EditViewInfo.ErrorData);
end;

function TcxTreeListColumn.GetEditingProperties: TcxTreeListGetEditPropertiesEvent;
begin
  Result := TcxTreeListGetEditPropertiesEvent(inherited OnGetEditingProperties);
end;

function TcxTreeListColumn.GetEditProperties: TcxTreeListGetEditPropertiesEvent;
begin
  Result := TcxTreeListGetEditPropertiesEvent(inherited OnGetEditProperties);
end;

function TcxTreeListColumn.GetHasIndent: Boolean;
begin
  Result := (Position.Band <> nil) and (Position.VisibleColIndex = 0) and
    Position.Band.ActuallyExpandable;
end;

function TcxTreeListColumn.GetID: Integer;
begin
  Result := -1;
  if TreeList <> nil then
    Result := TreeList.ContainerList.IndexOf(Self);
end;

function TcxTreeListColumn.GetIndentWidth: Integer;
begin
  Result := 0;
  if HasIndent then
    Result := Position.Band.IndentWidth;
end;

function TcxTreeListColumn.GetActuallyVisible: Boolean;
begin
  Result := Visible and (Position.Band <> nil) and Position.Band.ActuallyVisible;
end;

function TcxTreeListColumn.GetDataController: TcxTreeListDataController;
begin
  Result := TcxTreeListDataController(inherited DataController);
end;

function TcxTreeListColumn.GetDataBinding: TcxItemDataBinding;
begin
  Result := TcxItemDataBinding(inherited DataBinding);
end;

function TcxTreeListColumn.GetDisplayWidth: Integer;
begin
  TreeList.ValidateStates;
  if not ActuallyVisible then
    Result := 0
  else
  begin
    Result := FCalculatedWidth;
    if Result = 0 then
      Result := Width;
  end;
end;

function TcxTreeListColumn.GetIsFixed: Boolean;
var
  AForcingColumn: TcxTreeListColumn;
  APos: TcxTreeListColumnPosition;
begin
  AForcingColumn := TreeList.Controller.ForcingWidthColumn;
  Result := (TreeList.Controller.ForcingWidthColumn = Self) or not Options.Sizing;
  if not Result and (AForcingColumn <> nil) then
  begin
    APos := AForcingColumn.Position;
    Result := (APos = Position) or not AForcingColumn.IsRight and
      (Position.Row = APos.Row) and (Position.VisibleColIndex < APos.VisibleColIndex);
  end;
end;

function TcxTreeListColumn.GetIsHidden: Boolean;
begin
  Result := not ActuallyVisible or IsPreview;
end;

function TcxTreeListColumn.GetIsLeft: Boolean;
begin
  Result := ActuallyVisible and (Position.VisibleColIndex = 0);
end;

function TcxTreeListColumn.GetIsPreview: Boolean;
begin
  Result := (TreeList <> nil) and (TreeList.Preview.Column = Self);
end;

function TcxTreeListColumn.GetIsRight: Boolean;
begin
  with Position do
    Result :=  ActuallyVisible and (VisibleColIndex = Row.VisibleItemCount - 1);
end;

function TcxTreeListColumn.GetIsLoading: Boolean;
begin
  Result := (csLoading in ComponentState) and
    ((TreeList = nil) or not TreeList.IgnoreLoadingStatus);
end;

function TcxTreeListColumn.GetIsReading: Boolean;
begin
  Result := csReading in ComponentState;
end;

function TcxTreeListColumn.GetIsUpdating: Boolean;
begin
  Result := csUpdating in ComponentState;
end;

function TcxTreeListColumn.GetIsWidthStored: Boolean;
begin
  Result := FWidth <> 0;
end;

function TcxTreeListColumn.GetSortIndex: Integer;
begin
  Result := FSortIndex;
  if SortOrder = soNone then
    Result := -1;
end;

function TcxTreeListColumn.GetStyles: TcxTreeListColumnStyles;
begin
  Result := TcxTreeListColumnStyles(inherited Styles);
end;

function TcxTreeListColumn.GetItemIndex: Integer;
begin
  Result := TreeList.ColumnsList.IndexOf(Self);
end;

function TcxTreeListColumn.GetOptions: TcxTreeListColumnOptions;
begin
  Result := TcxTreeListColumnOptions(inherited Options);
end;

function TcxTreeListColumn.GetTreeList: TcxCustomTreeList;
begin
  Result := TcxCustomTreeList(EditingControl);
end;

function TcxTreeListColumn.GetValueByNode(ANode: TcxTreeListNode): Variant;
begin
  Result := ANode.Values[ItemIndex];
end;

function TcxTreeListColumn.GetValueDef: TcxValueDef;
begin
  Result := TcxCustomDataHelper.GetValueDef(
    DataController.Fields[inherited ItemIndex]);
end;

function TcxTreeListColumn.GetVisibleIndex: Integer;
begin
  Result := TreeList.FVisibleColumns.IndexOf(Self)
end;

function TcxTreeListColumn.GetWidth: Integer;
begin
  Result := FWidth;
  if Result = 0 then
    Result := cxTreeListDefWidth;
  Result := Max(MinWidth, Result);
end;

procedure TcxTreeListColumn.SetBestFitMaxWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FBestFitMaxWidth <> AValue then
  begin
    FBestFitMaxWidth := AValue;
    Changed;
  end;
end;

procedure TcxTreeListColumn.SetCaption(AValue: TcxTreeListCaption);
begin
  FCaption.Assign(AValue);
end;

procedure TcxTreeListColumn.SetDataBinding(AValue: TcxItemDataBinding);
begin
  DataBinding.Assign(AValue);
end;

procedure TcxTreeListColumn.SetDisplayWidth(AValue: Integer);
begin
  ForceWidth(AValue);
end;

procedure TcxTreeListColumn.SetEditingProperties(
  AValue: TcxTreeListGetEditPropertiesEvent);
begin
  inherited OnGetEditingProperties := TcxGetEditPropertiesEvent(AValue);
end;

procedure TcxTreeListColumn.SetEditProperties(
  AValue: TcxTreeListGetEditPropertiesEvent);
begin
  inherited OnGetEditProperties := TcxGetEditPropertiesEvent(AValue);
end;

procedure TcxTreeListColumn.SetIsPreview(AValue: Boolean);
begin
  if AValue <> IsPreview then
  begin
    if AValue then
      TreeList.Preview.Column := Self
    else
      TreeList.Preview.Column := nil;
  end;
end;

procedure TcxTreeListColumn.SetItemIndex(AValue: Integer);
begin
  if (AValue < 0) or (AValue >= TreeList.ContainerList.Count) or (AValue = ItemIndex) then Exit;
  TreeList.BeginUpdate;
  try
    TreeList.ContainerList.Exchange(ItemIndex, AValue);
    TreeList.DataController.UpdateItemIndexes;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListColumn.SetMinWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> FMinWidth then
  begin
    FMinWidth := AValue;
    SetWidth(FMinWidth);
  end;
end;

procedure TcxTreeListColumn.SetOptions(AValue: TcxTreeListColumnOptions);
begin
  Options.Assign(AValue);
end;

procedure TcxTreeListColumn.SetPosition(AValue: TcxTreeListColumnPosition);
begin
  FPosition.Assign(AValue);
end;

procedure TcxTreeListColumn.SetSortIndex(AValue: Integer);
var
  I: Integer;
begin
  if SortOrder = soNone then
    AValue := -1;
  if AValue <> FSortIndex then
  begin
    FSortIndex := AValue;
    if IsLoading or TreeList.IsRestoring then Exit;
    if AValue >= 0 then
    begin
      for I := AValue to TreeList.SortedColumnCount - 1 do
        if TreeList.SortedColumns[I].FSortIndex > 0 then
          Inc(TreeList.SortedColumns[I].FSortIndex);
    end;
    TreeList.DataLayoutChanged;
  end;
end;

procedure TcxTreeListColumn.SetSortOrder(AValue: TcxDataSortOrder);
var
  AShiftState: TShiftState;
begin
  if TreeList.OptionsBehavior.MultiSort then
    AShiftState := [ssShift]
  else
    AShiftState := [];
  ChangeSortOrder(AValue, AShiftState);
end;

procedure TcxTreeListColumn.SetStyles(AValue: TcxTreeListColumnStyles);
begin
  Styles.Assign(AValue);
end;

procedure TcxTreeListColumn.SetSummary(AValue: TcxTreeListColumnSummary);
begin
  FSummary.Assign(AValue);
end;

procedure TcxTreeListColumn.SetSummaryFooter(AValue: TPersistent);
begin
  FSummaryFooter.Assign(AValue);
end;

procedure TcxTreeListColumn.SetValueByNode(
  ANode: TcxTreeListNode; const AValue: Variant);
begin
  ANode.Values[ItemIndex] := AValue;
end;

procedure TcxTreeListColumn.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    TreeList.ClearCalculatedWidths;
    CheckUsingInFind;
    Changed;
    if not Visible and Focused then
    begin
      TreeList.Controller.Navigator.FocusNextCell(True, False);
      if Focused then
        TreeList.Controller.Navigator.FocusNextCell(False, False);
    end;
  end;
end;

procedure TcxTreeListColumn.SetWidth(AValue: Integer);
begin
  AValue := Max(MinWidth + IndentWidth, AValue);
  if AValue < 0 then Exit;
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    FCalculatedWidth := AValue;
    if (Position.Band <> nil) and not IsLoading and not IsUpdating then
      Position.Band.ColumnSizeChanged(Self);
    Changed;
  end;
end;

{ TcxTreeListNode }

constructor TcxTreeListNode.Create(AOwner: TcxCustomTreeList);
begin
  FTreeList := AOwner;
  FAbsoluteIndex := -1;
  FOriginalIndex := -1;
  FillChar(FImageIndexes, SizeOf(FImageIndexes), -1);
  Inc(TreeList.DataController.FNodesCount);
  State := [nsCollapsed, nsCheckStateInvalid];
end;

destructor TcxTreeListNode.Destroy;
begin
  TreeList.BeginUpdate;
  try
    if FViewData <> nil then
      FViewData.Node := nil;
    Dec(TreeList.DataController.FNodesCount);
    Include(State, nsDeleting);
    TreeList.AddChanges([tcStructure, tcData, tcSummary]);
    if not IsRoot then
      TreeList.DoDeletion(Self);
    if Count > 0 then
      DeleteChildren;
    if not IsRoot then
      TreeList.DoDeleteNode(Self);
    TreeList.AddChanges([tcStructure]);
    ExtractFromParent;
  finally
    TreeList.EndUpdate;
    if not IsRoot then
      TreeList.DoDataChangedEvent(nil);
    inherited Destroy;
  end;
end;

procedure TcxTreeListNode.Assign(Source: TPersistent);
var
  ANode: TcxTreeListNode;
begin
  if Source is TcxTreeListNode then
  begin
    ANode := TcxTreeListNode(Source);
    Focused := ANode.Focused;
    HasChildren := ANode.HasChildren;
    AssignData(ANode);
  end
  else
    inherited Assign(Source);
end;

procedure TcxTreeListNode.AssignValues(
  const AValues: array of Variant);
var
  I, K: Integer;
begin
  K := 0;
  for I := Low(AValues) to High(AValues) do
  begin
    if K >= ValueCount then Break;
    Values[I] := AValues[I];
    Inc(K);
  end;
end;

function TcxTreeListNode.AddChild: TcxTreeListNode;
begin
  Result := TreeList.AddNode(nil, Self, nil, tlamAddChild);
end;

function TcxTreeListNode.AddChildFirst: TcxTreeListNode;
begin
  Result := TreeList.AddNode(nil, Self, nil, tlamAddChildFirst);
end;

procedure TcxTreeListNode.AlphaSort(ARecurse: Boolean = False);
begin
  CustomSort(nil, ARecurse);
end;

function TcxTreeListNode.CanCollapse: Boolean;
begin
  Result := TreeList.DoCanNodeCollapse(Self);
end;

function TcxTreeListNode.CanExpand: Boolean;
begin
  Result := TreeList.DoCanNodeExpand(Self);
end;

function TcxTreeListNode.CanMove(
  ADest: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode): Boolean;
begin
  if (ADest = nil) or ((ADest = Self) and (AMode in [tlamAddChild, tlamAddChildFirst, tlamInsert])) or
    ADest.HasAsParent(Self) or ADest.Deleting then
    Result := False
  else
    Result := True;
end;

procedure TcxTreeListNode.CheckClick;
const
  ANewState: array[Boolean, TcxCheckBoxState] of TcxCheckBoxState =
  ((cbsChecked, cbsUnchecked, cbsChecked), (cbsChecked, cbsGrayed, cbsUnchecked));
begin
  if not Enabled or not HasCheckbox or
    ((Parent.CheckGroupType = ncgRadioGroup) and Checked) then Exit;
  CheckState := ANewState[AllowGrayed, CheckState];
end;

procedure TcxTreeListNode.Collapse(Recurse: Boolean);
var
  ANode: TcxTreeListNode;
begin
  if not HasChildren then Exit;
  TreeList.BeginUpdate;
  try
    if TreeList.InternalCollapseNode(Self, Recurse) then Exit;
    Expanded := False;
    if Recurse then
    begin
      ANode := FFirst;
      while ANode <> nil do
      begin
        ANode.Collapse(True);
        ANode := ANode.FNext;
      end;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListNode.CustomSort(
  ASortProc: TcxTreeListCompareFunc; ARecurse: Boolean = False);
var
  I: Integer;
  AList: TdxFastList;
  AListSortCompare: TListSortCompare absolute ASortProc;
begin
  if not Assigned(ASortProc) and not TreeList.CanCompare and
    not TreeList.Sorted and not (tcSortOrder in TreeList.Changes) then Exit;
  TreeList.BeginUpdate;
  try
    AList := TdxFastList.Create;
    try
      PopulateItems(AList);
      if not Assigned(ASortProc) then
        ASortProc := @cxCompareNodes;
      if AList.Count > 1 then
      begin
        AList.Sort(AListSortCompare, IsMultiThreadedSorting);
        UpdateItems(AList);
      end;
      if ARecurse and (Count > 0) then
      begin
        for I := 0 to AList.Count - 1 do
          TcxTreeListNode(AList.List[I]).CustomSort(ASortProc, ARecurse);
      end;
    finally
      AList.Free;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListNode.Delete;
begin
  if not Deleting then
    TreeList.DataController.DeleteNode(Self);
end;

procedure TcxTreeListNode.DeleteChildren;
var
  ANode: TcxTreeListNode;
begin
  TreeList.BeginUpdate;
  try
    while FFirst <> nil do
    begin
      if nsInternalDelete in State then
        Include(FFirst.State, nsInternalDelete);
      ANode := FFirst;
      FFirst := FFirst.FNext;
      if not (nsInternalDelete in State) then
        TreeList.DataController.DeleteNode(ANode)
      else
      begin
        Include(ANode.State, nsDeleting);
        ANode.Free;
      end;
    end;
  finally
    FCount := 0;
    HasChildren := False;
    FFirst := nil;
    FLast := nil;
    TreeList.EndUpdate;
  end;
end;

function TcxTreeListNode.DisplayRect(AEntryOnly: Boolean): TRect;
begin
  Result := cxNullRect;
  if ViewData = nil then Exit;
  if AEntryOnly then
    Result := ViewData.GetRealBounds
  else
    Result := ViewData.GetRealContentBounds;
end;

procedure TcxTreeListNode.EndEdit(Cancel: Boolean);
begin
  TreeList.Controller.EditingController.HideEdit(not Cancel);
end;

procedure TcxTreeListNode.Expand(Recurse: Boolean);
var
  ANode: TcxTreeListNode;
begin
  if not HasChildren or TreeList.IsCancelOperation then Exit;
  TreeList.BeginUpdate;
  try
    if TreeList.InternalExpandNode(Self, Recurse) then Exit;
    Expanded := True;
    if Recurse then
    begin
      ANode := FFirst;
      while ANode <> nil do
      begin
        ANode.Expand(True);
        ANode := ANode.FNext;
      end;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

function TcxTreeListNode.GetNextChild(ANode: TcxTreeListNode): TcxTreeListNode;
begin
  if ANode <> nil then
    Result := TcxTreeListNode(ANode.FNext)
  else
    Result := nil;
end;

function TcxTreeListNode.GetFirstChild: TcxTreeListNode;
begin
  Result := FFirst
end;

function TcxTreeListNode.GetFirstChildVisible: TcxTreeListNode;
begin
  Result := FFirst;
  while (Result <> nil) and (not Result.Visible or Result.HiddenByFilter) do
    Result := Result.FNext;
end;

function TcxTreeListNode.GetLastChild: TcxTreeListNode;
begin
  Result := FLast
end;

function TcxTreeListNode.GetLastChildVisible: TcxTreeListNode;
begin
  Result := FLast;
  while (Result <> nil) and (not Result.Visible or Result.HiddenByFilter) do
    Result := Result.FPrev;
end;

function TcxTreeListNode.GetNext: TcxTreeListNode;
var
  ANode: TcxTreeListNode;
begin
  if not (nsInternalDelete in State) then
    LoadChildren;
  Result := FFirst;
  if FCount = 0 then
  begin
    ANode := Self;
    while ANode <> nil do
    begin
      if ANode.FNext <> nil then
      begin
        Result := ANode.FNext;
        Break;
      end;
      while (ANode <> nil) and (ANode.FNext = nil) do ANode := ANode.Parent;
    end;
  end;
end;

function TcxTreeListNode.GetNextSibling: TcxTreeListNode;
begin
  Result := FNext;
end;

function TcxTreeListNode.GetNextSiblingVisible: TcxTreeListNode;
begin
  Result := FNext;
  while (Result <> nil) and (not Result.Visible or Result.HiddenByFilter) do
    Result := Result.FNext;
end;

function TcxTreeListNode.GetPrev: TcxTreeListNode;
begin
  Result := FPrev;
  if Result <> nil then
    Result := cxGetLatest(Result, False)
  else
    Result := Parent;
  if Result = TreeList.Root then
    Result := nil;
end;

function TcxTreeListNode.GetPrevSibling: TcxTreeListNode;
begin
  Result := FPrev;
end;

function TcxTreeListNode.GetPrevSiblingVisible: TcxTreeListNode;
begin
  Result := FPrev;
  while (Result <> nil) and (not Result.Visible or Result.HiddenByFilter) do
    Result := Result.FPrev;
end;

function TcxTreeListNode.GetPrevChild(ANode: TcxTreeListNode): TcxTreeListNode;
begin
  if ANode <> nil then
    Result := TcxTreeListNode(ANode.FPrev)
  else
    Result := nil;
end;

function TcxTreeListNode.HasAsParent(ANode: TcxTreeListNode): Boolean;
var
  AItem: TcxTreeListNode;
begin
  Result := False;
  AItem := Parent;
  while (AItem <> nil) and not Result do
  begin
    Result := AItem = ANode;
    AItem := AItem.FParent;
  end;
end;

function TcxTreeListNode.IndexOf(ANode: TcxTreeListNode): Integer;
begin
  if ANode.Parent = Self then
    Result := ANode.Index
  else
    Result := -1;
end;

function TcxTreeListNode.InsertChild(
  ABeforeNode: TcxTreeListNode): TcxTreeListNode;
begin
  Result := TreeList.AddNode(nil, ABeforeNode, nil, tlamInsert);
end;

procedure TcxTreeListNode.Invalidate;
begin
  if ViewData <> nil then
    TreeList.InvalidateRect(DisplayRect(false), False);
end;

function TcxTreeListNode.IsMultiThreadedSorting: Boolean;
var
  AValue: TdxDefaultBoolean;
begin
  if not TreeList.CanUseMultiThreadedSorting or not dxCanUseMultiThreading then
    Result := False
  else
  begin
    AValue := TreeList.OptionsData.MultiThreadedSorting;
    if AValue = bDefault then
      Result := dxDefaultMultiThreadedSorting
    else
      Result := Boolean(AValue);
  end;
end;

function TcxTreeListNode.IsSibling(ANode: TcxTreeListNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.FParent = FParent);
end;

procedure TcxTreeListNode.LoadChildren;
begin
  if (Count <> 0) or not HasChildren or Expanded or TreeList.IsDestroying then Exit;
  TreeList.BeginUpdate;
  try
    TreeList.DoExpand(Self);
  finally
    if Count > 0 then
      TreeList.EndUpdate
    else
      TreeList.CancelUpdate;
  end;
end;

procedure TcxTreeListNode.MakeVisible;
var
  ANode: TcxTreeListNode;
begin
  if not IsVisible then
  begin
    TreeList.BeginUpdate;
    try
      ANode := FParent;
      while ANode <> nil do
      begin
        ANode.Expanded := True;
        if not ANode.Expanded then Break;
        ANode := ANode.FParent;
      end;
    finally
      TreeList.EndUpdate;
    end;
  end;
  if TreeList.IsLocked then Exit;
  if IsVisible then
    TreeList.MakeNodeVisible(Self);
  TreeList.UpdateScrollBars;
end;

procedure TcxTreeListNode.MoveTo(
  ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode);
begin
  if (ADestNode = nil) or (ADestNode = Self) then Exit;
  if AMode in [tlamAdd, tlamAddFirst] then
    ADestNode := ADestNode.Parent;
  if not ADestNode.HasAsParent(Self) then
  begin
    if AMode = tlamAddChild then
      ExtractFromParent;
    TreeList.InternalMove(Self, ADestNode, AMode);
  end;
end;

procedure TcxTreeListNode.Repaint(ARecalculate: Boolean);
begin
  if (FViewData = nil) or Deleting then Exit;
  ViewData.Update(True);
end;

function TcxTreeListNode.GetNextSiblingEx(
  ANode: TcxTreeListNode; AForward: Boolean): TcxTreeListNode;
begin
  if AForward then
    Result := ANode.FNext
  else
    Result := ANode.FPrev;
end;

procedure TcxTreeListNode.AssignData(ASource: TcxTreeListNode);
var
  I: Integer;
begin
  State := ASource.State;
  CheckInfo := ASource.CheckInfo;
  FImageIndexes := ASource.FImageIndexes;
  if (ASource.Parent <> nil) and (Parent <> nil) then
    Parent.CheckInfo := Parent.CheckInfo + ASource.Parent.CheckInfo * [nciCheckGroup, nciRadioGroup];
  for I := 0 to ASource.ValueCount - 1 do
    Values[I] := ASource.Values[I];
  TreeList.ImagesChanged(nil);
end;

function TcxTreeListNode.CanChecked: Boolean;
begin
  Result := (Parent = nil) or (Parent.CheckGroupType <> ncgNone);
end;

procedure TcxTreeListNode.ExtractFromParent;
var
  AParent: TcxTreeListNode;
begin
  AParent := FParent;
  if FParent <> nil then
  begin
    Exclude(FParent.State, nsValidIndexes);
    Dec(FParent.FCount);
    if FParent.FFirst = Self then
      FParent.FFirst := FNext;
    if FParent.FLast = Self then
      FParent.FLast := FPrev;
    if FParent.FCount = 0 then
      FParent.State := FParent.State - [nsHasChildren] + [nsCollapsed];
  end;
  if FNext <> nil then
    FNext.FPrev := FPrev;
  if FPrev <> nil then
    FPrev.FNext := FNext;
  FPrev := nil;
  FNext := nil;
  FParent := nil;
  if AParent <> nil then
    AParent.State := AParent.State + [nsCheckStateInvalid];
  State := State + [nsCheckStateInvalid];
end;

function TcxTreeListNode.GetFooterSummaryCount: Integer;
begin
  if HasChildren then
    Result := Summary.GroupFooterSummaryCount
  else
    Result := 0;
end;

function TcxTreeListNode.GetFooterSummaryText(AIndex: Integer): string;
begin
  Result := Summary.GroupFooterSummaryTexts[
    Summary.GroupFooterSummaryItems[AIndex], Self];
end;

function TcxTreeListNode.GetFooterSummaryValue(AIndex: Integer): Variant;
begin
  cxError(not cxInRange(AIndex, 0, FooterSummaryCount - 1), SListIndexError, [AIndex]);
  Summary.CheckChanges;
  Result := FSummaryInfo[AIndex].Value;
end;

function TcxTreeListNode.GetInserting: Boolean;
begin
  Result := TreeList.IsNodeInserting(Self);
end;

function TcxTreeListNode.GetIsFirstVisible: Boolean;
begin
  Result := FVisibleIndex = 0;
end;

function TcxTreeListNode.GetIsLastVisible: Boolean;
begin
  Result := (FVisibleIndex >= 0) and (FVisibleIndex = TreeList.AbsoluteVisibleCount - 1);
end;

function TcxTreeListNode.GetIsRoot: Boolean;
begin
  Result := False;
end;

function TcxTreeListNode.GetNextVisibleEx(IsPrev: Boolean): TcxTreeListNode;
begin
  if IsPrev then
    Result := GetPrevVisible
  else
    Result := GetNextVisible;
end;

function TcxTreeListNode.GetOwner: TPersistent;
begin
  Result := TreeList;
end;

procedure TcxTreeListNode.InternalFree;
begin
  Include(State, nsInternalDelete);
  Free;
end;

procedure TcxTreeListNode.InitializeHandle;
begin
  if FHandle = nil then
    FHandle := TreeList.DataController.AllocateRecord;
end;

function OldState2State(AState: Integer): TcxTreeListNodeStates;
const
  tlnsHasChildren      = $0004;
  tlnsCollapsed        = $0010;
begin
  Result := [];
  if AState and tlnsHasChildren = tlnsHasChildren then
    Include(Result, nsHasChildren);
  if AState and tlnsCollapsed = tlnsCollapsed then
    Include(Result, nsCollapsed);
end;

procedure TcxTreeListNode.ReadData(AStream: TStream; AVersion: Integer);
var
  AOldState: Integer;
begin
  if AVersion = 4 then
  begin
    AStream.ReadBuffer(AOldState, SizeOf(Integer));
    State := OldState2State(AOldState);
  end
  else
    AStream.ReadBuffer(State, SizeOf(State));
  if nsCheck in State then
    AStream.ReadBuffer(CheckInfo, SizeOf(CheckInfo));
  if nsHeightAssigned in State then
    AStream.ReadBuffer(FHeight, SizeOf(Integer));
  FCount := 0;
  if nsHasChildren in State then
    AStream.ReadBuffer(FCount, SizeOf(Integer));
end;

procedure TcxTreeListNode.RestoreStateAfterRefresh;
begin
  if (Count > 0) and (nsSaveExpanded in State) then
    Exclude(State, nsCollapsed);
  Exclude(State, nsSaveExpanded);
end;

procedure TcxTreeListNode.SaveStateBeforeRefresh;
begin
  if Expanded then
    Include(State, nsSaveExpanded);
end;

procedure TcxTreeListNode.SetChildrenCheckState(
  AValue: TcxCheckBoxState; AExclude: TcxTreeListNode);
var
  ANode: TcxTreeListNode;
begin
  if CheckGroupType = ncgNone then
    Exit;
  ANode := FLast;
  while ANode <> nil do
  begin
    if ANode.Visible and not ANode.HiddenByFilter and (ANode <> AExclude) then
      ANode.CheckState := AValue;
    ANode := ANode.FPrev;
  end;
end;

procedure TcxTreeListNode.UpdateCheckStates;
var
  AChild: TcxTreeListNode;
  AState: TcxCheckBoxState;
begin
  if TreeList.IsLocked then
  begin
    State := State + [nsCheckStateInvalid];
    Exit;
  end;
  if not (nsCheckStateInvalid in State) then Exit;
  State := State - [nsCheckStateInvalid];
  AState := CheckState;
  if CheckGroupType = ncgNone then
    Exit;
  AChild := GetFirstChildVisible;
  if AChild <> nil then
  begin
    AState := AChild.CheckState;
    AChild := AChild.FNext;
    while AChild <> nil do
    begin
      if AChild.Visible and not AChild.HiddenByFilter and (AState <> AChild.CheckState) then
      begin
        AState := cbsGrayed;
        Break;
      end;
      AChild := AChild.FNext;
    end;
  end;
  CheckState := AState;
end;

procedure TcxTreeListNode.WriteData(AStream: TStream);
begin
  if FCount > 0 then
    Include(State, nsHasChildren);
  if CheckInfo <> [] then
    Include(State, nsCheck);
  AStream.WriteBuffer(State, SizeOf(State));
  if nsCheck in State then
    AStream.WriteBuffer(CheckInfo, SizeOf(CheckInfo));
  if nsHeightAssigned in State then
    AStream.WriteBuffer(FHeight, SizeOf(Integer));
  if (FCount > 0) or (nsHasChildren in State) then
    AStream.WriteBuffer(FCount, SizeOf(Integer));
end;

function TcxTreeListNode.CanSizing(
  ADirection: TcxDragSizingDirection): Boolean;
begin
  with TreeList do
  begin
    Result := OptionsCustomizing.RowSizing and (ADirection = dsdVert);
    Result := Result and not OptionsView.CellAutoHeight
  end;
end;

function TcxTreeListNode.GetSizingBoundsRect(ADirection: TcxDragSizingDirection): TRect;
begin
  if ViewData = nil then
    Result := cxEmptyRect
  else
  begin
    Result := cxRectSetTop(TreeList.ClientBounds, FViewData.Origin.Y +
      TreeList.ViewInfo.ColumnsRowCount * (TreeList.ViewInfo.DefaultCellHeight + cxTextOffset * 2) +
      TreeList.ViewInfo.GroupFooterHeight * ViewData.GroupFooterCount, TreeList.ClientHeight);
  end;
end;

function TcxTreeListNode.GetSizingIncrement(
  ADirection: TcxDragSizingDirection): Integer;
begin
  Result := 1;
end;

function TcxTreeListNode.IsDynamicUpdate: Boolean;
begin
  Result := TreeList.OptionsCustomizing.DynamicSizing;
end;

procedure TcxTreeListNode.PopulateItems(AList: TdxFastList);
var
  I: Integer;
  ANode: TcxTreeListNode;
begin
  I := 0;
  AList.Count := Count;
  ANode := FFirst;
  while ANode <> nil do
  begin
    AList.List[I] := ANode;
    ANode := ANode.FNext;
    Inc(I);
  end;
end;

procedure TcxTreeListNode.SetSizeDelta(
  ADirection: TcxDragSizingDirection; ADelta: Integer);
begin
  if ADelta = 0 then Exit;
  if ViewData.IsRightToLeftConverted and (ADirection = dsdHorz) then
    ADelta := -ADelta;
  if TreeList.OptionsCustomizing.RowSizing then
    TreeList.DefaultRowHeight := Max(TreeList.ViewInfo.DefaultCellHeight,
      Max(TreeList.ViewInfo.DefaultCellHeight, TreeList.DefaultRowHeight) + ADelta)
  else
  begin
    if Height = 0 then
      Height := TreeList.ViewInfo.DefaultRowHeight * TreeList.ViewInfo.ColumnsRowCount;
    Height := Max(TreeList.ViewInfo.DefaultCellHeight * TreeList.ViewInfo.ColumnsRowCount,
      Height + ADelta);
  end;
  TreeList.Modified;
end;

procedure TcxTreeListNode.UpdateItems(AList: TdxFastList);
var
  I, L: Integer;
  ANode: TcxTreeListNode;
  AChanged: Boolean;
begin
  if Count = 0 then Exit;
  L := Count - 1;

  AChanged := False;
  for I := 0 to L do
  begin
    ANode := TcxTreeListNode(AList.List[I]);
    AChanged := AChanged or (ANode.Index <> I);
    ANode.FIndex := I;
    if I > 0 then
      ANode.FPrev := AList.List[I - 1]
    else
      ANode.FPrev := nil;
     if I < L then
       ANode.FNext := AList.List[I + 1]
     else
       ANode.FNext := nil;
  end;
  FFirst := TcxTreeListNode(AList.List[0]);
  FLast := TcxTreeListNode(AList.List[L]);
  if AChanged then
    TreeList.StructureChanged;
end;

function TcxTreeListNode.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

function TcxTreeListNode._AddRef: Integer;
begin
  Result := -1;
end;

function TcxTreeListNode._Release: Integer;
begin
  Result := -1;
end;

function TcxTreeListNode.GetRootParent: TcxTreeListNode;
begin
  Result := Self;
  while (Result.FParent <> nil) and (Result.FParent <> TreeList.Root) do
    Result := Result.FParent;
end;

function TcxTreeListNode.GetAbsoluteIndex: Integer;
begin
  if tcStructure in TreeList.Changes then
    TreeList.CheckStructure;
  Result := FAbsoluteIndex;
end;

function TcxTreeListNode.GetAllowGrayed: Boolean;
begin
  Result := (nciAllowGrayed in CheckInfo) and
    ((Parent = nil) or (Parent.CheckGroupType <> ncgRadioGroup));
end;

function TcxTreeListNode.GetChecked: Boolean;
begin
  Result := (nciChecked in CheckInfo) or
    ((nciGrayed in CheckInfo) and (Parent <> nil) and Parent.IsRadioGroup);
end;

function TcxTreeListNode.GetCheckedCount: Integer;
var
  ANode: TcxTreeListNode;
begin
  Result := 0;
  ANode := FFirst;
  while ANode <> nil do
  begin
    if ANode.Visible and not ANode.HiddenByFilter and (ANode.CheckState = cbsChecked) then
      Inc(Result);
    ANode := ANode.FNext;
  end;
end;

function TcxTreeListNode.GetChildVisibleCount: Integer;
var
  ANode: TcxTreeListNode;
begin
  Result := 0;
  ANode := FFirst;
  while ANode <> nil do
  begin
    if ANode.Visible and not ANode.HiddenByFilter then
      Inc(Result);
    ANode := ANode.FNext;
  end;
end;

function TcxTreeListNode.GetCheckGroupType: TcxTreeListNodeCheckGroupType;
begin
  if nciCheckGroup in CheckInfo then
    Result := ncgCheckGroup
  else
    if nciRadioGroup in CheckInfo then
      Result := ncgRadioGroup
    else
      Result := ncgNone;
end;

function TcxTreeListNode.GetCheckState: TcxCheckBoxState;
begin
  if not CanChecked then
  begin
    Result := cbsUnchecked;
    State := State - [nsCheckStateInvalid];
    Exit;
  end;
  if nsCheckStateInvalid in State then
    UpdateCheckStates;
  Result := cbsUnchecked;
  if nciGrayed in CheckInfo then
    Result := cbsGrayed
  else
    if nciChecked in CheckInfo then
      Result := cbsChecked;
end;

function TcxTreeListNode.GetEnabled: Boolean;
begin
  Result := not (nsDisabled in State);
end;

function TcxTreeListNode.GetExpanded: Boolean;
begin
  Result := (Count > 0) and (not (nsCollapsed in State) or (FParent = nil));
end;

function TcxTreeListNode.GetFocused: Boolean;
begin
  Result := TreeList.Controller.FocusedNode = Self;
end;

function TcxTreeListNode.GetGrayedCount: Integer;
var
  ANode: TcxTreeListNode;
begin
  Result := 0;
  ANode := FFirst;
  while ANode <> nil do
  begin
    if ANode.CheckState = cbsGrayed then
      Inc(Result);
    ANode := ANode.FNext;
  end;
end;

function TcxTreeListNode.GetHasCheckbox: Boolean;
begin
  Result := TreeList.OptionsView.CheckGroups and CanChecked
end;

function TcxTreeListNode.GetHasChildren: Boolean;
begin
  Result := (nsHasChildren in State) or (ChildVisibleCount > 0);
end;

function TcxTreeListNode.GetHasVisibleChildren: Boolean;
var
  ANode: TcxTreeListNode;
begin
  Result := HasChildren and (Count = 0);
  if not HasChildren or (Count = 0) then Exit;
  ANode := FFirst;
  while not Result and (ANode <> nil) do
  begin
    Result := ANode.Visible and not ANode.HiddenByFilter;
    ANode := ANode.FNext;
  end;
end;

function TcxTreeListNode.GetHiddenByFilter: Boolean;
begin
  Result := nsHiddenByFilter in State;
end;

function TcxTreeListNode.GetIndex: Integer;
begin
  if Parent <> nil then
    Parent.AdjustIndexes;
  Result := FIndex;
end;

function TcxTreeListNode.GetIsDeleting: Boolean;
var
  ANode: TcxTreeListNode;
begin
  Result := False;
  ANode := Self;
  while not Result and (ANode <> nil) do
  begin
    Result := nsDeleting in ANode.State;
    ANode := ANode.Parent;
  end;
end;

function TcxTreeListNode.GetIsEditing: Boolean;
begin
  Result := TreeList.DataController.EditingNode = Self;
end;

function TcxTreeListNode.GetHotTrack: Boolean;
begin
  Result := TreeList.Controller.HotTrackNode = Self;
end;

function TcxTreeListNode.GetIsFirst: Boolean;
begin
  Result := (FParent = nil) or (FParent.FFirst = Self);
end;

function TcxTreeListNode.GetIsGroupNode: Boolean;
begin
  Result := TreeList.DoIsGroupNode(Self);
end;

function TcxTreeListNode.GetIsHidden: Boolean;
var
  ANode: TcxTreeListNode;
begin
  ANode := Parent;
  Result := not Visible or HiddenByFilter;
  if Self = Root then Exit;
  while not Result and (ANode <> TreeList.Root)  do
  begin
    Result := not ANode.Visible or HiddenByFilter;
    ANode := ANode.Parent;
  end;
end;

function TcxTreeListNode.GetIsLast: Boolean;
begin
  Result := (FParent = nil) or (FParent.FLast = Self);
end;

function TcxTreeListNode.GetIsPrinted: Boolean;
begin
  Result := ViewData <> nil;
end;

function TcxTreeListNode.GetIsRadioGroup: Boolean;
begin
  Result := CheckGroupType = ncgRadioGroup;
end;

function TcxTreeListNode.GetIsVisible: Boolean;
var
  ANode: TcxTreeListNode;
begin
  Result := FParent <> nil;
  ANode := Self;
  while Result and (ANode.FParent <> nil) and (ANode.FParent <> TreeList.Root) do
  begin
    Result := not (nsCollapsed in ANode.FParent.State);
    ANode := ANode.FParent;
  end;
end;

function TcxTreeListNode.GetItem(AIndex: Integer): TcxTreeListNode;
begin
  cxError((AIndex < 0) or (AIndex >= FCount),
    cxGetResourceString(@scxIndexOutOfBounds), [AIndex]);
  if Parent <> nil then
    Parent.AdjustIndexes;
  if (FLast.Index shr 1) <= AIndex  then
  begin
    Result := FLast;
    while Result.FIndex <> AIndex do
      Result := Result.FPrev;
  end
  else
  begin
    Result := FFirst;
    while Result.FIndex <> AIndex do
      Result := Result.FNext;
  end;
end;

function TcxTreeListNode.GetLevel: Integer;
var
  ANode: TcxTreeListNode;
begin
  ANode := FParent;
  Result := -1;
  while ANode <> nil do
  begin
    Inc(Result);
    ANode := ANode.FParent;
  end;
end;

function TcxTreeListNode.GetRoot: TcxTreeListNode;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TcxTreeListNode.GetSelected: Boolean;
begin
  Result := TreeList.SelectionList.IndexOf(Self) <> cxInvalidIndex
end;

function TcxTreeListNode.GetSummary: TcxTreeListSummary;
begin
  Result := TreeList.Summary;
end;

function TcxTreeListNode.GetNextVisible: TcxTreeListNode;
var
  I: Integer;
begin
  Result := nil;
  for I := FAbsoluteIndex + 1 to TreeList.AbsoluteCount - 1 do
    if TcxTreeListNode(TreeList.FAbsoluteItems.List[I]).VisibleIndex >= 0 then
    begin
      Result := TcxTreeListNode(TreeList.FAbsoluteItems.List[I]);
      Break;
    end;
end;

function TcxTreeListNode.GetNodeImageIndex(AIndex: Integer): TcxImageIndex;
begin
  Result := TreeList.DoGetNodeImageIndex(Self, TcxTreeListImageIndexType(AIndex));
end;

function TcxTreeListNode.GetPrevVisible: TcxTreeListNode;
var
  I: Integer;
begin
  Result := nil;
  for I := FAbsoluteIndex - 1 downto 0 do
    if TcxTreeListNode(TreeList.FAbsoluteItems.List[I]).VisibleIndex >= 0 then
    begin
      Result := TcxTreeListNode(TreeList.FAbsoluteItems.List[I]);
      Break;
    end;
end;

function TcxTreeListNode.GetText(AIndex: Integer): string;
begin
  Result := TreeList.Columns[AIndex].DisplayTexts[Self];
end;

function TcxTreeListNode.GetValue(AIndex: Integer): Variant;
begin
  Result := TreeList.DataController.GetNodeValue(Self, AIndex);
end;

function TcxTreeListNode.GetValueCount: Integer;
begin
   Result := TreeList.ColumnCount;
end;

function TcxTreeListNode.GetVisible: Boolean;
begin
  Result := not (nsHidden in State);
end;

function TcxTreeListNode.GetVisibleIndex: Integer;
begin
  if IsVisible then
    Result := FVisibleIndex
  else
    Result := -1;
end;

procedure TcxTreeListNode.AdjustIndexes;
var
  AIndex: Integer;
  ANode: TcxTreeListNode;
begin
  if nsValidIndexes in State then Exit;
  AIndex := 0;
  ANode := FFirst;
  while ANode <> nil do
  begin
    ANode.FIndex := AIndex;
    ANode := ANode.FNext;
    Inc(AIndex);
  end;
  Include(State, nsValidIndexes);
end;

procedure TcxTreeListNode.InternalInsert(AValue: TcxTreeListNode);
begin
  if FParent <> nil then
    ExtractFromParent;
  FPrev := AValue.FPrev;
  if FPrev <> nil then
    FPrev.FNext := Self
  else
    AValue.FParent.FFirst := Self;
  FNext := AValue;
  FNext.FPrev := Self;
  AValue.FParent.SetParentFor(Self);
end;

procedure TcxTreeListNode.SetAllowGrayed(AValue: Boolean);
begin
  if AValue then
    Include(CheckInfo, nciAllowGrayed)
  else
    Exclude(CheckInfo, nciAllowGrayed);
end;

procedure TcxTreeListNode.SetChecked(AValue: Boolean);
begin
  if AValue then
    CheckState := cbsChecked
  else
    CheckState := cbsUnchecked;
end;

procedure TcxTreeListNode.SetCheckGroupType(
  AValue: TcxTreeListNodeCheckGroupType);
const
  AState: array[TcxTreeListNodeCheckGroupType] of TcxTreeListNodeCheckInfos =
    ([], [nciCheckGroup], [nciRadioGroup]);
begin
  if AValue <> CheckGroupType then
  begin
    State := State + [nsCheckStateInvalid];
    CheckInfo := CheckInfo - [nciCheckGroup, nciRadioGroup] + AState[AValue];
    TreeList.LayoutChanged;
  end;
end;

procedure TcxTreeListNode.SetCheckState(AValue: TcxCheckBoxState);
var
  ACheckedCount: Integer;
  APrevCheckState: TcxCheckBoxState;
const
  AState: array[TcxCheckBoxState] of TcxTreeListNodeCheckInfos =
    ([], [nciChecked], [nciGrayed]);
  AParentCheckState: array[Boolean] of TcxCheckBoxState = (cbsGrayed, cbsChecked);
begin
  if not CanChecked or not TreeList.DoNodeCheckChanging(Self, AValue) then
  begin
    State := State - [nsCheckStateInvalid];
    Exit;
  end;
  APrevCheckState := CheckState;
  CheckInfo := CheckInfo - [nciChecked, nciGrayed] + AState[AValue] + [nciChangeCheck];
  try
    if (CheckState in [cbsChecked, cbsUnchecked]) and HasChildren then
    begin
      LoadChildren;
      if not IsRadioGroup or (CheckState = cbsUnchecked) or (CheckedCount = 0) then
        SetChildrenCheckState(CheckState, nil);
    end;
    if (Parent <> nil){ and (Parent.CheckGroupType <> ncgNone)} then
    begin
      if Parent.IsRadioGroup and Checked then
        Parent.SetChildrenCheckState(cbsUnchecked, Self);
      if not (nciChangeCheck in Parent.CheckInfo) and (Parent <> Root) then
      begin
        ACheckedCount := Parent.CheckedCount;
        if (ACheckedCount = 0) and (Parent.GrayedCount = 0) then
          Parent.CheckState := cbsUnchecked
        else
          Parent.CheckState := AParentCheckState[(ACheckedCount = Parent.ChildVisibleCount) or
            ((ACheckedCount > 0) and Parent.IsRadioGroup)];
      end;
    end;
  finally
    CheckInfo := CheckInfo - [nciChangeCheck];
    State := State - [nsCheckStateInvalid];
    Repaint(True);
    if APrevCheckState <> CheckState then
      TreeList.DoNodeCheckChanged(Self, CheckState);
  end;
end;

procedure TcxTreeListNode.SetEnabled(AValue: Boolean);
begin
  if AValue then
    Exclude(State, nsDisabled)
  else
    Include(State, nsDisabled);
  TreeList.LayoutChanged;
end;

procedure TcxTreeListNode.SetExpanded(AValue: Boolean);
var
  ANeedMoveFocus, AExpanded: Boolean;
begin
  if AValue <> Expanded then
  begin
    AExpanded := Expanded;
    ANeedMoveFocus := not AValue and (TreeList.FocusedNode <> nil) and TreeList.FocusedNode.HasAsParent(Self);
    if Parent = nil then Exit;
    TreeList.BeginUpdate;
    try
      TreeList.AddChanges([tcStructure]);
      if AValue then
      begin
        if not TreeList.DoCanNodeExpand(Self) then Exit;
        TreeList.DoExpand(Self);
        Exclude(State, nsCollapsed);
        TreeList.DoNodeExpanded(Self);
      end
      else
      begin
        if not TreeList.DoCanNodeCollapse(Self) then Exit;
        Include(State, nsCollapsed);
        TreeList.DoNodeCollapsed(Self);
      end;
      if AExpanded <> Expanded then
        TreeList.StructureChanged;
    finally
      TreeList.EndUpdate;
      if ANeedMoveFocus then
        Focused := True;
    end
  end;
end;

procedure TcxTreeListNode.SetFirst(AValue: TcxTreeListNode);
begin
  AValue.ExtractFromParent;
  if FFirst <> nil then
  begin
    AValue.FNext := FFirst;
    FFirst.FPrev := AValue;
  end
  else
    FLast := AValue;
  FFirst := AValue;
  SetParentFor(AValue, False);
end;

procedure TcxTreeListNode.SetFocused(AValue: Boolean);
begin
  if AValue <> Focused then
  begin
    if not AValue then
      TreeList.Controller.FocusedNode := nil
    else
      TreeList.Controller.Select(Self, []);
  end;
end;

procedure TcxTreeListNode.SetFooterSummaryValue(AIndex: Integer; AValue: Variant);
begin
  cxError(not cxInRange(AIndex, 0, FooterSummaryCount - 1), SListIndexError, [AIndex]);
  FSummaryInfo[AIndex].Value := AValue;
end;

procedure TcxTreeListNode.SetHasChildren(AValue: Boolean);
begin
  if HasChildren <> AValue then
  begin
    if AValue then
      Include(State, nsHasChildren)
    else
      if FCount = 0 then
        Exclude(State, nsHasChildren);
    TreeList.AddChanges([tcSummary]);
    TreeList.DoNodeChanged(Self, nil);
  end;
end;

procedure TcxTreeListNode.SetHeight(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    if FHeight = 0 then
      Exclude(State, nsHeightAssigned)
    else
      Include(State, nsHeightAssigned);
    TreeList.LayoutChanged;
  end;
end;

procedure TcxTreeListNode.SetHiddenByFilter(AValue: Boolean);
var
  ANode: TcxTreeListNode;
  AState: TcxTreeListNodeStates;
begin
  AState := State;
  if AValue then
    Include(State, nsHiddenByFilter)
  else
  begin
    Exclude(State, nsHiddenByFilter);
    ANode := Parent;
    while (ANode <> nil) and (nsHiddenByFilter in ANode.State) do
    begin
      AState := ANode.State;
      Exclude(ANode.State, nsHiddenByFilter);
      ANode := ANode.Parent;
    end;
  end;
  if AState <> State then
  begin
    TreeList.AddChanges([tcStructure, tcSummary]);
    TreeList.DoNodeChanged(Self, nil);
    if Parent <> nil then
      Parent.State := Parent.State + [nsCheckStateInvalid];
  end;
end;

procedure TcxTreeListNode.SetItem(AIndex: Integer; AValue: TcxTreeListNode);
begin
  GetItem(AIndex).Assign(AValue);
end;

procedure TcxTreeListNode.SetLast(AValue: TcxTreeListNode);
begin
  AValue.ExtractFromParent;
  if FLast <> nil then
  begin
    AValue.FPrev := FLast;
    FLast.FNext := AValue;
  end
  else
    FFirst := AValue;
  FLast := AValue;
  SetParentFor(AValue, False);
end;

procedure TcxTreeListNode.SetNodeImageIndex(
  AIndex: Integer; AValue: TcxImageIndex);
begin
  if FImageIndexes[TcxTreeListImageIndexType(AIndex)] <> AValue then
  begin
    FImageIndexes[TcxTreeListImageIndexType(AIndex)] := AValue;
    TreeList.DoNodeChanged(Self, nil);
  end;
end;

procedure TcxTreeListNode.SetParentFor(
  AValue: TcxTreeListNode; AValidateIndexes: Boolean = True);
begin
  AValue.FParent := Self;
  if AValidateIndexes then
    Exclude(State, nsValidIndexes)
  else
    AValue.FIndex := FCount;
  Inc(FCount);
  AValue.State := AValue.State + [nsCheckStateInvalid];
  TreeList.AddChanges([tcStructure, tcSummary]);
  TreeList.DoNodeChanged(AValue, nil);
end;

procedure TcxTreeListNode.SetText(AIndex: Integer; const AValue: string);
begin
  SetValue(AIndex, AValue);
end;

procedure TcxTreeListNode.SetSelected(AValue: Boolean);
begin
  if Selected <> AValue then
  try
    if not AValue then
    begin
      TreeList.SelectionList.Remove(Self);
      TreeList.DoSelectionChanged;
    end
    else
      if TreeList.OptionsSelection.MultiSelect then
        TreeList.Controller.AddNodeToSelection(Self)
      else
        TreeList.Select(Self, []);
  finally
    TreeList.ViewInfo.UpdateSelection;
  end;
end;

procedure TcxTreeListNode.SetValue(AIndex: Integer; const AValue: Variant);
begin
  if Focused and (TreeList.DataController.EditingNode <> nil) then
    TreeList.DataController.SetEditValue(AIndex, AValue, evsValue)
  else
    TreeList.DataController.SetNodeValue(Self, AIndex, AValue);
end;

procedure TcxTreeListNode.SetVisible(AValue: Boolean);
begin
  if AValue <> Visible then
  begin
    if not AValue then
      Include(State, nsHidden)
    else
      Exclude(State, nsHidden);
    TreeList.AddChanges([tcStructure, tcSummary]);
    TreeList.DoNodeChanged(Self, nil);
    if Parent <> nil then
      Parent.State := Parent.State + [nsCheckStateInvalid];
  end;
end;

{ TcxTreeListRootNode }

function TcxTreeListRootNode.GetFooterSummaryCount: Integer;
begin
  Result := Summary.FooterSummaryCount;
end;

function TcxTreeListRootNode.GetFooterSummaryText(
  AIndex: Integer): string;
begin
  Result := Summary.FooterSummaryTexts[
    Summary.FooterSummaryItems[AIndex]];
end;

function TcxTreeListRootNode.GetIsRoot: Boolean;
begin
  Result := True;
end;

procedure TcxTreeListRootNode.InitializeHandle;
begin
  FHandle := nil;
end;

{ TcxUnboundTreeListNode }

constructor TcxUnboundTreeListNode.Create(AOwner: TcxCustomTreeList);
begin
  inherited Create(AOwner);
  FImageIndexes := cxDefaultImageIndexes;
end;

procedure TcxUnboundTreeListNode.Assign(Source: TPersistent);
begin
  if Source is TcxUnboundTreeListNode then
    FImageIndexes := TcxUnboundTreeListNode(Source).FImageIndexes;
  inherited Assign(Source);
end;

procedure TcxUnboundTreeListNode.ReadData(AStream: TStream; AVersion: Integer);
begin
  AStream.ReadBuffer(FHandle, SizeOf(Integer));
  inherited ReadData(AStream, AVersion);
  if AVersion = 4 then
    AStream.ReadBuffer(FImageIndexes, SizeOf(Integer) * 3)
  else
    AStream.ReadBuffer(FImageIndexes, SizeOf(FImageIndexes));
end;

procedure TcxUnboundTreeListNode.WriteData(AStream: TStream);
begin
  AStream.WriteBuffer(Integer(FHandle), SizeOf(Integer));
  inherited WriteData(AStream);
  AStream.WriteBuffer(FImageIndexes, SizeOf(FImageIndexes));
end;

{ TcxFakeCellViewInfo }

constructor TcxFakeCellViewInfo.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FNodeViewData := TcxTreeListNodeViewData.Create(AOwner as TcxTreeListViewInfo, nil, 0);
end;

destructor TcxFakeCellViewInfo.Destroy;
begin
  FNodeViewData.Node := nil;
  FreeAndNil(FNodeViewData);
  FColumn := nil;
  inherited Destroy;
end;

function TcxFakeCellViewInfo.MeasureHeight(AColumn: TcxTreeListColumn;
  ANode: TcxTreeListNode; AIsCategory: Boolean): Integer;

  function CalculateHeight(AHeight: Integer): Integer;
  begin
    if esoAutoHeight in Properties.GetSupportedOperations then
      Result := Max(TreeListViewInfo.DefaultEditHeight,
          Max(Column.GetEditHeight(Self), AHeight) + OptionsView.VertIncrement * 2)
    else
      Result := TreeListViewInfo.DefaultEditHeight + OptionsView.VertIncrement;
  end;

var
  W: Integer;
begin
  FColumn := AColumn;
  W := Column.DisplayWidth - OptionsView.HorzIncrement * 2;  // # Exclude GridLines
  if AIsCategory then
    W := TreeListViewInfo.ContentWidth;
  if AColumn.IndentWidth > 0 then
    W := W - TreeListViewInfo.GetNodeContentOffset(ANode);
  if not Column.IsLeft then
    Inc(W, OptionsView.HorzIncrement);
  Initialize(ANode, W, TreeListViewInfo.DefaultEditHeight, True);
  with TreeList.Controller.EditingController do
  begin
    if IsEditing and IsEditAutoHeight and (AutoHeight = eahRow) and (Column = AColumn) and
      (ANode = TreeList.DataController.EditingNode) then
    begin
      CellValue := MultilineEdit.EditingValue;
      Result := CalculateHeight(MultilineEdit.CalculatedHeight + ScaleFactor.Apply(cxTextOffset));
    end
    else
      Result := CalculateHeight(0);
  end;
end;

function TcxFakeCellViewInfo.MeasureWidth(
  AColumn: TcxTreeListColumn; ANode: TcxTreeListNode): Integer;
var
  AIndent: Integer;
begin
  FColumn := AColumn;
  Initialize(ANode, 0, TreeListViewInfo.DefaultEditHeight, False);
  AIndent := 0;
  if Column.IndentWidth > 0 then
  begin
    AIndent := TreeListViewInfo.GetNodeContentOffset(ANode);
    if TreeList.OptionsView.CheckGroups and ANode.HasCheckbox then
      Inc(AIndent, ScaleFactor.Apply(cxTextOffset));
  end;
  Result := Max(Column.MinWidth + Column.IndentWidth, AIndent + Column.GetEditWidth(Self) + OptionsView.HorzIncrement * 2);
end;

function TcxFakeCellViewInfo.GetButtonTransparency: TcxEditButtonTransparency;
begin
  Result := ebtNone;
end;

function TcxFakeCellViewInfo.GetControl: TcxEditingControl;
begin
  if TreeListViewInfo <> nil then
    Result := TreeListViewInfo.TreeList
  else
    Result := nil;
end;

function TcxFakeCellViewInfo.GetEditContainer: TcxCustomInplaceEditContainer;
begin
  Result := FColumn;
end;

function TcxFakeCellViewInfo.GetFocused: Boolean;
begin
  Result := False;
end;

function TcxFakeCellViewInfo.GetSelected: Boolean;
begin
  Result := False;
end;

procedure TcxFakeCellViewInfo.Initialize(ANode: TcxTreeListNode;
  AHeight, AWidth: Integer; ACalcHeight: Boolean);
begin
  FCalcHeight := ACalcHeight;
  FNodeViewData.Node := ANode;
  CellContentRect := cxRect(0, 0, AHeight, AWidth);
  Column.InitEditViewInfo(Self);
end;

function TcxFakeCellViewInfo.IsAutoHeight: Boolean;
begin
  Result := FCalcHeight;
end;

function TcxFakeCellViewInfo.IsEndEllipsis: Boolean;
begin
  Result := False;
end;

function TcxFakeCellViewInfo.GetTreeListViewInfo: TcxTreeListViewInfo;
begin
  if FNodeViewData <> nil then
    Result := FNodeViewData.ViewInfo
  else
    Result := nil;
end;

{ TcxTreeListCellPos }

constructor TcxTreeListCellPos.Create(
  ANode: TcxTreeListNode; AItem: TObject);
begin
  Item := AItem;
  Node := ANode;
end;

{ TcxTreeList }

function TcxTreeList.Add: TcxTreeListNode;
begin
  Result := Add(nil);
end;

function TcxTreeList.Add(ASibling: TcxTreeListNode;
  AData: Pointer = nil): TcxTreeListNode;
begin
  if ASibling <> nil then
    ASibling := ASibling.Parent;
  Result := AddNode(nil, ASibling, AData, tlamAdd);
end;

function TcxTreeList.AddChild(AParent: TcxTreeListNode;
  AData: Pointer = nil): TcxTreeListNode;
begin
  Result := AddNode(nil, AParent, AData, tlamAddChild);
end;

function TcxTreeList.AddChildFirst(AParent: TcxTreeListNode;
  AData: Pointer = nil): TcxTreeListNode;
begin
  Result := AddNode(nil, AParent, AData, tlamAddChildFirst);
end;

function TcxTreeList.AddFirst: TcxTreeListNode;
begin
  Result := AddFirst(nil);
end;

function TcxTreeList.AddFirst(ASibling: TcxTreeListNode; // need test???
  AData: Pointer = nil): TcxTreeListNode;
begin
  if ASibling <> nil then
    ASibling := ASibling.Parent;
  Result := AddNode(nil, ASibling, AData, tlamAddFirst);
end;

function TcxTreeList.AddNode(ANode, ARelative: TcxTreeListNode;
  AData: Pointer; AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode;
begin
  Result := inherited AddNode(ANode, ARelative, AData, AttachMode);
end;

function TcxTreeList.Insert(ASibling: TcxTreeListNode;
  AData: Pointer = nil): TcxTreeListNode;
begin
  Result := AddNode(nil, ASibling, AData, tlamInsert);
end;

function TcxTreeList.InsertEx(ANode, ASibling: TcxTreeListNode;
  AData: Pointer = nil): TcxTreeListNode;
begin
  Result := AddNode(ANode, ASibling, AData, tlamInsert);
end;

procedure TcxTreeList.LoadFromFile(const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AFileStream);
  finally
    FreeAndNil(AFileStream);
  end;
end;

procedure TcxTreeList.LoadFromStream(AStream: TStream);
begin
  SetGlassCursor;
  try
    ReadData(AStream);
  finally
    RestoreCursor;
  end;
end;

procedure TcxTreeList.SaveToFile(const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AFileStream);
  finally
    FreeAndNil(AFileStream);
  end;
end;

procedure TcxTreeList.SaveToStream(AStream: TStream);
begin
  SetGlassCursor;
  try
    WriteData(AStream);
  finally
    RestoreCursor;
  end;
end;

procedure TcxTreeList.AssignData(ASource: TcxCustomTreeList);
var
  AStream: TMemoryStream;
begin
  inherited AssignData(ASource);
  if ASource is TcxTreeList then
  begin
    AStream := TMemoryStream.Create;
    try
       TcxTreeList(ASource).SaveToStream(AStream);
       AStream.Position := 0;
       LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TcxTreeList.CorrectHandles;
var
  I: Integer;
begin
  if DataController.NodesCount = 0 then Exit;
  CheckStructure;
  FAbsoluteItems.Sort(@cxCompareNodesByRecordIndex);
  DataController.FAllocatedRecords := 0;
  for I := 0 to FAbsoluteItems.Count - 1 do
    DataController.InitializeNodeFromRecordIndex(TcxTreeListNode(FAbsoluteItems.List[I]));
  FAbsoluteItems.Sort(@cxCompareNodesByAbsoluteIndex);
end;

procedure TcxTreeList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, AbsoluteCount > 0);
end;

procedure TcxTreeList.Loaded;
begin
  BeginUpdate;
  try
    inherited Loaded;
    Include(FChanges, tcLoading);
    try
      if FDataStream <> nil then
      try
        ReadData(FDataStream);
      finally
        FDataStream.Free;
      end;
      Exclude(FChanges, tcLoading);
    finally
      FChanges := [tcStructure..tcSortOrder];
    end;
  finally
    EndUpdate;
  end;
end;

function TcxTreeList.SupportItemsEditor: Boolean;
begin
  Result := True;
end;

procedure TcxTreeList.ReadData(AStream: TStream);
var
  AHeader: TcxTreeListStreamHeader;
begin
  if IsLoading then
  begin
    if FDataStream = nil then
      FDataStream := TMemoryStream.Create;
    FDataStream.CopyFrom(AStream, AStream.Size - AStream.Position);
    FDataStream.Position := 0;
    Exit;
  end;
  AStream.ReadBuffer(AHeader, SizeOf(TcxTreeListStreamHeader));
  if (AHeader.Size <> AStream.Size) then
     cxTreeListError(cxGetResourceString(@scxInvalidStreamFormat));
  BeginUpdate;
  try
    DoClear;
    try
      DataController.LoadFromStream(AStream);
      ReadStructure(AStream, AHeader.Major);
    except
      DoClear;
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxTreeList.ReadStructure(AStream: TStream; AVersion: Integer);
var
  AIndex: Integer;
  ANewNode, ANode: TcxTreeListNode;
begin
  ANode := Root;
  AStream.ReadBuffer(Root.FCount, SizeOf(Integer));
  if Root.FCount = 0 then Exit;
  AIndex := 0;
  repeat
    ANewNode := InternalCreateNode(ANode, ANode.FLast, AIndex);
    ANewNode.ReadData(AStream, AVersion);
    if ANewNode.FCount > 0 then
    begin
      ANode := ANewNode;
      AIndex := 0;
      Continue;
    end
    else
      while ANewNode.FParent <> nil do
      begin
        ANode := ANewNode.FParent;
        AIndex := ANode.FLast.FIndex + 1;
        if AIndex < ANode.Count then
          Break
        else
          ANewNode := ANode;
      end;
  until (ANode = Root) and (AIndex = ANode.Count);
  if AStream.Position < AStream.Size then
    Root.ReadData(AStream, AVersion);
  CorrectHandles;
end;

procedure TcxTreeList.WriteData(AStream: TStream);
var
  I: Integer;
  AHeader: TcxTreeListStreamHeader;
begin
  AStream.Position := 0;
  PInteger(@AHeader)^ := cxTreeListVersion;
  AHeader.Size := AStream.Size;
  AStream.WriteBuffer(AHeader, SizeOf(AHeader));
  TcxDataStorageHelper.SetRecordsCapacity(DataController.DataStorage, AbsoluteCount);
  for I := 0 to AbsoluteCount - 1 do
    DataController.InitializeRecordIndexFromNode(TcxTreeListNode(FAbsoluteItems[I]));
  try
    DataController.SaveToStream(AStream);
    WriteStructure(AStream);
    AHeader.Size := AStream.Size;
    AStream.Position := TdxNativeUInt(@AHeader.Size) - TdxNativeUInt(@AHeader);
    AStream.Write(AHeader.Size, SizeOf(AHeader.Size));
    AStream.Position := AHeader.Size;
  finally
    CorrectHandles;
  end;
end;

procedure TcxTreeList.WriteStructure(AStream: TStream);
var
  ANode: TcxTreeListNode;
begin
  ANode := Root.GetFirstChild;
  AStream.WriteBuffer(Root.FCount, SizeOf(Integer));
  while ANode <> nil do
  begin
    ANode.WriteData(AStream);
    if ANode.FCount > 0 then
      ANode := ANode.GetFirstChild
    else
    begin
      while (ANode <> nil) and (ANode.FNext = nil) do
        ANode := ANode.FParent;
      if ANode <> nil then
        ANode := ANode.FNext;
    end;
  end;
  Root.WriteData(AStream);
end;

function TcxTreeList.InternalCreateNode(AParent, APrev: TcxTreeListNode;
  var AIndex: Integer): TcxTreeListNode;
begin
  Result := CreateNode;
  Result.FParent := AParent;
  if APrev <> nil then
    APrev.FNext := Result
  else
    AParent.FFirst := Result;
  Result.FIndex := AIndex;
  Result.FPrev := APrev;
  AParent.FLast := Result;
  Result.FOriginalIndex := AIndex;
  Inc(AIndex);
end;

initialization
  Classes.RegisterClass(TcxTreeListStyleSheet);
  FPopupImagesManager := TcxTLPopupMenuImagesManager.Create;

finalization
  FreeAndNil(FPopupImagesManager);
  Classes.UnRegisterClass(TcxTreeListStyleSheet);

end.
