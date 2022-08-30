{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGrid;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Classes, Windows, Controls, Messages, Forms, StdCtrls, ImgList,
  ExtCtrls, ComCtrls, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxControls, Graphics, dxUxTheme, dxThemeManager, cxGraphics,
  cxClasses, cxStyles, cxLookAndFeelPainters, cxLookAndFeels, dxCustomHint,
  cxInplaceContainer, cxEdit, cxButtons, cxNavigator, cxFilterControl, dxUIElementPopupWindow,
  cxDataControllerConditionalFormatting, cxFilter, cxDataStorage, cxCustomData, dxFilterPopupWindow,
  cxStorage, cxVGridUtils, cxGeometry, dxForms, dxMessages, cxListBox, cxPC, dxDateRanges;

const
  cxvgMinValueWidth     = 20;

  // vertical grid styles predefined indexes
  // for row styles
  vgrs_Header           = 0;
  vgrs_Content          = 1;

  // for control styles
  vgs_Content        = ecs_Content;
  vgs_Selection      = ecs_Selection;
  vgs_Category       = ecs_EditingStylesMaxIndex + 1;
  vgs_Header         = ecs_EditingStylesMaxIndex + 2;
  vgs_IncSearch      = ecs_EditingStylesMaxIndex + 3;
  vgs_ContentEven    = ecs_EditingStylesMaxIndex + 4;
  vgs_ContentOdd     = ecs_EditingStylesMaxIndex + 5;
  vgs_MaxIndex       = vgs_ContentOdd;

  // base constants for descendants
  cxvgMaxControlStyleIndex = vgs_IncSearch;
  cxvgMaxRowStyleIndex     = vgrs_Content;

  // hit codes
  vghc_Base            = echc_MaxIndex + 1;
  vghc_HitAtBandSizing = vghc_Base;
  vghc_HitAtButton     = vghc_Base + 1;
  vghc_HitAtCaption    = vghc_Base + 2;
  vghc_HitAtDivider    = vghc_Base + 3;
  vghc_HitAtEmpty      = vghc_Base + 4;
  vghc_HitAtImage      = vghc_Base + 5;
  vghc_HitAtIndent     = vghc_Base + 6;
  vghc_HitAtRowSizing  = vghc_Base + 7;
  vghc_HitAtValue      = vghc_Base + 8;
  vghc_HitAtCustomize  = vghc_Base + 9;
  vghc_HitAtSeparator  = vghc_Base + 10;
  vghc_HitAtNavigator  = vghc_Base + 11;
  vghc_HitAtCaptionFilterButton = vghc_Base + 12;

type
  EcxVerticalGridError = class(EdxException);
  TCollectionNotification = Classes.TCollectionNotification;

  TcxCustomRow = class;
  TcxCustomRowHeaderInfo = class;
  TcxCustomRowViewInfo = class;
  TcxCustomVerticalGrid = class;
  TcxVirtualVerticalGrid = class;
  TcxRowCaptionInfo = class;
  TcxRowValueInfo = class;
  TcxRowValueInfoClass = class of TcxRowValueInfo;
  TcxValueInfoList = class;
  TcxCustomEditorRow = class;
  TcxCustomMultiEditorRow = class;
  TcxMultiEditorRow = class;
  TcxVerticalGridCustomizing = class;
  TcxVerticalGridCustomizingClass = class of TcxVerticalGridCustomizing;
  TcxVerticalGridRows = class;
  TcxvgCustomRowStyles = class;
  TcxvgCustomRowStylesClass = class of TcxvgCustomRowStyles;
  TcxCategoryRowStyles = class;
  TcxEditorRowStyles = class;
  TcxvgScroller = class;
  TcxCellEdit = class;
  TcxvgController = class;
  TcxvgCustomPaintStyleCalcHelper = class;
  TcxvgCustomPaintStyleCalcHelperClass = class of TcxvgCustomPaintStyleCalcHelper;
  TcxvgCustomViewInfo = class;
  TcxvgHitTest = class;
  TcxvgPainter = class;
  TcxVerticalGridStyles = class;

  { TcxVerticalGridFindPanel }

  TcxVerticalGridFindPanel = class(TcxControlFindPanel)
  protected
    function GetClearButtonCaption: string; override;
    function GetDefaultInfoText: string; override;
    function GetFindButtonCaption: string; override;
  end;

  { TcxVerticalGridFilterValueList }

  TcxVerticalGridFilterValueList = class(TcxControlFilterValueList)
  protected
    function GetDateTimeRelativeFilterDisplayText(AKind: TcxFilterOperatorKind): string; override;
  end;

  { TcxVerticalGridConditionalFormattingProvider }

  TcxVerticalGridConditionalFormattingProvider = class(TcxCustomControlControllerConditionalFormattingProvider)
  strict private
    FOwner: TcxCustomVerticalGrid;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function GetController: TcxCustomControlController; override;
    function GetItemDisplayName(AItem: TcxCustomInplaceEditContainer): string; override;
    function GetLookAndFeel: TcxLookAndFeel; override;
    function GetOwner: TComponent; override;
    function DoGetParentForm: TCustomForm; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function IsItemVisible(AItem: TcxCustomInplaceEditContainer): Boolean; override;
    function IsRightToLeft: Boolean; override;
    function IsRowVisible(const ARow: Integer): Boolean; override;
  public
    constructor Create(AOwner: TcxCustomVerticalGrid); reintroduce;
  end;

  { TcxvgOptionsBehavior }

  TcxvgOptionsBehavior = class(TcxControlOptionsBehavior)
  private
    FAlwaysShowEditorAssigned: Boolean;
    FBandSizing: Boolean;
    FHeaderSizing: Boolean;
    FRowSizing: Boolean;
    FRowTracking: Boolean;
    function GetAlwaysShowEditor: Boolean;
    function GetRowFiltering: TdxDefaultBoolean;
    procedure SetAlwaysShowEditor(Value: Boolean);
    procedure SetRowFiltering(AValue: TdxDefaultBoolean);
  protected
    procedure InternalSetAlwaysShowEditor(Value: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
  published
    property AlwaysShowEditor: Boolean read GetAlwaysShowEditor write SetAlwaysShowEditor stored FAlwaysShowEditorAssigned;
    property BandSizing: Boolean read FBandSizing write FBandSizing default True;
    property CellHints default True;
    property FocusCellOnCycle;
    property HeaderSizing: Boolean read FHeaderSizing write FHeaderSizing default True;
    property RowFiltering: TdxDefaultBoolean read GetRowFiltering write SetRowFiltering default bDefault;
    property RowSizing: Boolean read FRowSizing write FRowSizing default False;
    property RowTracking: Boolean read FRowTracking write FRowTracking default True;
  end;

  { TcxvgMultiRecordsOptionsBehavior }

  TcxvgMultiRecordsOptionsBehavior = class(TcxvgOptionsBehavior)
  private
    FAllowChangeRecord: Boolean;
    FRecordScrollMode: TcxRecordScrollMode;
    function GetIncSearchItem: TcxCustomEditorRow;
    procedure SetAllowChangeRecord(Value: Boolean);
    procedure SetIncSearchItem(Value: TcxCustomEditorRow);
    procedure SetRecordScrollMode(AValue: TcxRecordScrollMode);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AllowChangeRecord: Boolean read FAllowChangeRecord write SetAllowChangeRecord default True;
    property FocusFirstCellOnNewRecord;
    property IncSearch;
    property IncSearchItem: TcxCustomEditorRow read GetIncSearchItem write SetIncSearchItem;
    property NavigatorHints;
    property RecordScrollMode: TcxRecordScrollMode read FRecordScrollMode write SetRecordScrollMode default rsmDefault;
  end;

  { TcxvgMultiRecordsOptionsData }

  TcxvgMultiRecordsOptionsData = class(TcxControlOptionsData)
  private
    FAppending: Boolean;
    FDeleting: Boolean;
    FInserting: Boolean;
    FDeletingConfirmation: Boolean;
    FMultiThreadedFiltering: TdxDefaultBoolean;
    procedure SetAppending(Value: Boolean);
    procedure SetDeleting(Value: Boolean);
    procedure SetInserting(Value: Boolean);
    function GetMultiThreadedFiltering: TdxDefaultBoolean;
    procedure SetMultiThreadedFiltering(const Value: TdxDefaultBoolean);
    function GetVerticalGrid: TcxVirtualVerticalGrid;
  protected
    procedure Changed; override;

    property VerticalGrid: TcxVirtualVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Appending: Boolean read FAppending write SetAppending default True;
    property Deleting: Boolean read FDeleting write SetDeleting default True;
    property DeletingConfirmation: Boolean read FDeletingConfirmation write FDeletingConfirmation default True;
    property Inserting: Boolean read FInserting write SetInserting default True;
    property MultiThreadedFiltering: TdxDefaultBoolean read GetMultiThreadedFiltering write SetMultiThreadedFiltering default bDefault;
  end;

  { TcxVerticalGridFilterBox }

  TcxVerticalGridFilterBox = class(TcxControlOptionsFilterBox)
  protected
    function GetCustomizeButtonCaption: string; override;
    function GetDefaultText: string; override;
  end;

  { TcxVerticalGridDateTimeHandling }

  TcxVerticalGridDateTimeHandling = class(TcxControlOptionsDateTimeHandling)
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

  { TcxVerticalGridFilteringRowPopupOptions }

  TcxVerticalGridFilteringRowPopupOptions = class(TcxControlOptionsFilteringItemPopup)
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

  { TcxVerticalGridFilteringRowExcelPopupOptions }

  TcxVerticalGridFilteringRowExcelPopupOptions = class(TcxControlOptionsFilteringItemExcelPopup)
  published
    property ApplyChanges;
    property DateTimeValuesPageType;
    property DefaultPage;
    property FilteredItemsList;
    property NumericValuesPageType;
  end;

  { TcxVerticalGridFiltering }

  TcxVerticalGridFiltering = class(TcxControlOptionsFiltering)
  private
    function GetRowExcelPopup: TcxVerticalGridFilteringRowExcelPopupOptions;
    function GetRowPopup: TcxVerticalGridFilteringRowPopupOptions;
    function GetRowPopupMode: TdxFilterPopupWindowMode;
    procedure SetRowExcelPopup(AValue: TcxVerticalGridFilteringRowExcelPopupOptions);
    procedure SetRowPopup(AValue: TcxVerticalGridFilteringRowPopupOptions);
    procedure SetRowPopupMode(AValue: TdxFilterPopupWindowMode);
  protected
    function GetItemExcelPopupClass: TcxControlOptionsFilteringItemExcelPopupClass; override;
    function GetItemPopupClass: TcxControlOptionsFilteringItemPopupClass; override;
  published
    property RowExcelPopup: TcxVerticalGridFilteringRowExcelPopupOptions read GetRowExcelPopup write SetRowExcelPopup;
    property RowPopup: TcxVerticalGridFilteringRowPopupOptions read GetRowPopup write SetRowPopup;
    property RowPopupMode: TdxFilterPopupWindowMode read GetRowPopupMode write SetRowPopupMode default fpmDefault;
  end;

  { TcxvgOptionsView }

  TcxvgPaintStyle = (psdotNet, psDelphi);
  TcxvgGridLines = (vglNone, vglHorizontal, vglVertical, vglBoth);

  TcxvgOptionsView = class(TcxControlOptionsView)
  private
    FAutoScaleBands: Boolean;
    FBandsInterval: Integer;
    FCategoryExplorerStyle: Boolean;
    FGridLineColor: TColor;
    FGridLineColorAssigned: Boolean;
    FValueWidth: Integer;
    FPaintStyle: TcxvgPaintStyle;
    FRowHeaderMinWidth: Integer;
    FRowHeaderWidth: Integer;
    FRowHeight: Integer;
    FShowButtons: Boolean;
    FShowHeaders: Boolean;
    FShowEmptyRowImage: Boolean;
    FGridLines: TcxvgGridLines;
    FValueMinWidth: Integer;
    function GetGridLineColor: TColor;
    function GetRowHeaderFilterButtonShowMode: TcxFilterButtonShowMode;
    function GetShowRowHeaderFilterButtons: TcxShowFilterButtons;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    function IsPaintStyleStored: Boolean;
    procedure SetAutoScaleBands(Value: Boolean);
    procedure SetBandsInterval(Value: Integer);
    procedure SetCategoryExplorerStyle(Value: Boolean);
    procedure SetGridLineColor(Value: TColor);
    procedure SetPaintStyle(Value: TcxvgPaintStyle);
    procedure SetRowHeaderFilterButtonShowMode(AValue: TcxFilterButtonShowMode);
    procedure SetRowHeaderMinWidth(Value: Integer);
    procedure SetRowHeaderWidth(Value: Integer);
    procedure SetRowHeight(Value: Integer);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowEmptyRowImage(Value: Boolean);
    procedure SetShowHeaders(Value: Boolean);
    procedure SetShowRowHeaderFilterButtons(Value: TcxShowFilterButtons);
    procedure SetGridLines(Value: TcxvgGridLines);
    procedure SetValueMinWidth(Value: Integer);
    procedure SetValueWidth(Value: Integer);
  protected
    procedure Changed; override;
    procedure ChangeScale(M, D: Integer); override;
    function GetDefaultPaintStyle: TcxvgPaintStyle; virtual;
    function IsRowHeaderFilterButtonShowedAlways: Boolean; virtual;
    function IsRowHeaderFilterSmartTag: Boolean; virtual;

    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
  published
    property AutoScaleBands: Boolean read FAutoScaleBands write SetAutoScaleBands default True;
    property BandsInterval: Integer read FBandsInterval write SetBandsInterval default 2;
    property CategoryExplorerStyle: Boolean read FCategoryExplorerStyle write SetCategoryExplorerStyle default False;
    property PaintStyle: TcxvgPaintStyle read FPaintStyle write SetPaintStyle stored IsPaintStyleStored;
    property GridLineColor: TColor read GetGridLineColor write SetGridLineColor stored FGridLineColorAssigned;
    property RowHeaderFilterButtonShowMode: TcxFilterButtonShowMode read GetRowHeaderFilterButtonShowMode
      write SetRowHeaderFilterButtonShowMode default fbmDefault;
    property RowHeaderMinWidth: Integer read FRowHeaderMinWidth write SetRowHeaderMinWidth default 24;
    property RowHeaderWidth: Integer read FRowHeaderWidth write SetRowHeaderWidth default 100;
    property RowHeight: Integer read FRowHeight write SetRowHeight default -1;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property ShowEditButtons;
    property ShowEmptyRowImage: Boolean read FShowEmptyRowImage write SetShowEmptyRowImage default False;
    property ShowHeaders: Boolean read FShowHeaders write SetShowHeaders default True;
    property ShowRowHeaderFilterButtons: TcxShowFilterButtons read GetShowRowHeaderFilterButtons
      write SetShowRowHeaderFilterButtons default sfbDefault;
    property GridLines: TcxvgGridLines read FGridLines write SetGridLines default vglBoth;
    property ValueWidth: Integer read FValueWidth write SetValueWidth default 100;
    property ValueMinWidth: Integer read FValueMinWidth write SetValueMinWidth default 40;
  end;

  { TcxvgMultiRecordsOptionsView }

  TcxMultiRecordEvenOddContentStyle = (mrcsByRow, mrcsByRecord);

  TcxvgMultiRecordsOptionsView = class(TcxvgOptionsView)
  private
    FMultiRecordEvenOddContentStyle: TcxMultiRecordEvenOddContentStyle;
    FRecordsInterval: Integer;
    procedure SetRecordsInterval(Value: Integer);
    procedure SetMultiRecordEvenOddContentStyle(const Value: TcxMultiRecordEvenOddContentStyle);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property MultiRecordEvenOddContentStyle: TcxMultiRecordEvenOddContentStyle
      read FMultiRecordEvenOddContentStyle write SetMultiRecordEvenOddContentStyle default mrcsByRow;
    property NavigatorOffset;
    property RecordsInterval: Integer read FRecordsInterval write SetRecordsInterval default 0;
  end;

  { TcxCustomRowProperties }

  TcxCustomRowProperties = class(TcxInterfacedCollectionItem)
  private
    FCollection: TCollection;
    FLocked: Boolean;
    FRow: TcxCustomRow;
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetOwner: TPersistent; override;

    property Locked: Boolean read FLocked;
  public
    constructor Create(Collection: TCollection); override;
    constructor CreateEx(ARow: TcxCustomRow); virtual;
    property Row: TcxCustomRow read FRow;
  end;

  TcxRowPropertiesClass = class of TcxCustomRowProperties;

  { TcxCaptionRowProperties }

  TcxCaptionRowProperties = class(TcxCustomRowProperties)
  private
    FHeaderAlignmentHorz: TAlignment;
    FHeaderAlignmentVert: TcxAlignmentVert;
    FIsCaptionAssigned: Boolean;
    FCaption: TCaption;
    FImageIndex: TcxImageIndex;
    FHint: string;
    function GetCaption: TCaption;
    function GetRealHeaderAlignmentHorz: TAlignment;
    function IsCaptionStored: Boolean;
    function IsHeaderAlignmentVertStored: Boolean;
    procedure SetCaption(const Value: TCaption);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetHeaderAlignmentHorz(const Value: TAlignment);
    procedure SetHeaderAlignmentVert(const Value: TcxAlignmentVert);
  protected
    function DefaultCaption: string; virtual;
    function GetEditContainer: TcxCellEdit; virtual;
    function GetFilterableByPopupMenu: Boolean; virtual;
    function GetFiltered: Boolean; virtual;
    procedure RestoreDefaults; virtual;
    procedure SetFiltered(AValue: Boolean); virtual;

    property EditContainer: TcxCellEdit read GetEditContainer;
    property FilterableByPopupMenu: Boolean read GetFilterableByPopupMenu;
    property Filtered: Boolean read GetFiltered write SetFiltered;
    property HeaderRealAlignmentHorz: TAlignment read GetRealHeaderAlignmentHorz;
  public
    constructor CreateEx(ARow: TcxCustomRow); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property HeaderAlignmentHorz: TAlignment read FHeaderAlignmentHorz write SetHeaderAlignmentHorz default taLeftJustify;
    property HeaderAlignmentVert: TcxAlignmentVert read FHeaderAlignmentVert write SetHeaderAlignmentVert stored IsHeaderAlignmentVertStored;
    property Hint: string read FHint write FHint;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
  end;

  { TcxRowOptions }

  TcxRowOptions = class(TPersistent)
  private
    FCanAutoHeight: Boolean;
    FCanMovedToCustomizationForm: Boolean;
    FCanResized: Boolean;
    FFocusing: Boolean;
    FMoving: Boolean;
    FRow: TcxCustomRow;
    FShowExpandButton: Boolean;
    FShowInCustomizationForm: Boolean;
    FTabStop: Boolean;
    procedure SetCanAutoHeight(Value: Boolean);
    procedure SetFocusing(Value: Boolean);
    procedure SetShowExpandButton(Value: Boolean);
    procedure SetShowInCustomizationForm(Value: Boolean);
  protected
    procedure Changed; virtual;
    property Row: TcxCustomRow read FRow;
  public
    constructor Create(ARow: TcxCustomRow); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
  published
    property CanAutoHeight: Boolean read FCanAutoHeight write SetCanAutoHeight default True;
    property CanMovedToCustomizationForm: Boolean read FCanMovedToCustomizationForm write FCanMovedToCustomizationForm default True;
    property CanResized: Boolean read FCanResized write FCanResized default True;
    property Focusing: Boolean read FFocusing write SetFocusing default True;
    property Moving: Boolean read FMoving write FMoving default True;
    property ShowExpandButton: Boolean read FShowExpandButton write SetShowExpandButton default True;
    property ShowInCustomizationForm: Boolean read FShowInCustomizationForm write SetShowInCustomizationForm default True;
    property TabStop: Boolean read FTabStop write FTabStop default True;
  end;

  TcxRowOptionsClass = class of TcxRowOptions;

  { TcxCustomRow }

  TcxRowList = class(TList)
  private
    FOwner: TcxCustomRow;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure UpdateIndexes;
    property Owner: TcxCustomRow read FOwner;
  public
    constructor Create(AOwner: TcxCustomRow);
  end;

  TcxCustomRowClass = class of TcxCustomRow;
  TcxCustomRow = class(TComponent,
    IcxStoredObject,
    IdxScaleFactor)
  private
    FExpanded: Boolean;
    FHeight: Integer;
    FID: Integer;
    FIndex: Integer;
    FLoadedID: Integer;
    FLoadedIndex: Integer;
    FLoadedParentID: Integer;
    FLoadingIndex: Integer;
    FLoadingParent: string;
    FOptions: TcxRowOptions;
    FParent: TcxCustomRow;
    FRows: TcxRowList;
    FStyles: TcxvgCustomRowStyles;
    FVersion: Integer;
    FVerticalGrid: TcxCustomVerticalGrid;
    FViewInfo: TcxCustomRowViewInfo;
    FVisible: Boolean;
    FVisibleIndex: Integer;

    function GetAbsoluteIndex: Integer;
    function GetCount: Integer;
    function GetFocused: Boolean;
    function GetLevel: Integer;
    function GetOwnerRows: TcxVerticalGridRows;
    function GetParent: TcxCustomRow;
    function GetRow(Index: Integer): TcxCustomRow;
    function GetViewInfo: TcxCustomRowViewInfo;
    procedure SetExpanded(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetIndex(Value: Integer);
    procedure SetOptions(Value: TcxRowOptions);
    procedure SetParent(Value: TcxCustomRow);
    procedure SetVisible(Value: Boolean);
    // storing layout
    procedure ReadID(Reader: TReader);
    procedure ReadLoadedIndex(Reader: TReader);
    procedure ReadLoadedParentID(Reader: TReader);
    procedure ReadVersion(Reader: TReader);
    procedure WriteID(Writer: TWriter);
    procedure WriteLoadedIndex(Writer: TWriter);
    procedure WriteLoadedParentID(Writer: TWriter);
    procedure WriteVersion(Writer: TWriter);
  protected
    FProperties: TcxCustomRowProperties;

    procedure ChangeScale(M, D: Integer); virtual;
    // IcxStoredObject
    function GetObjectName: string; virtual;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
    // override TComponent
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;

    procedure Add(ARow: TcxCustomRow);
    function CanFocus: Boolean; virtual;
    function CanNavigate: Boolean;
    function CanTabStop: Boolean; virtual;
    procedure Changed(ARebuild: Boolean = False); virtual;
    procedure CheckUsingInFind; virtual;
    function CreateHeaderInfo: TcxCustomRowHeaderInfo; virtual; abstract;
    function CreateViewInfo: TcxCustomRowViewInfo; virtual; abstract;
    procedure Delete(AIndex: Integer);
    function GetDefaultHeight: Integer; virtual;
    function GetEditContainer(ACellIndex: Integer): TcxCellEdit; virtual;
    function GetEditContainerCount: Integer; virtual;
    function GetPropertiesClass: TcxRowPropertiesClass; virtual;
    function GetRealHeight: Integer; virtual;
    function GetOptionsClass: TcxRowOptionsClass; virtual;
    function GetStylesClass: TcxvgCustomRowStylesClass; virtual;
    procedure Insert(AIndex: Integer; ARow: TcxCustomRow);
    function IsHeightAssigned: Boolean;
    procedure Refresh;
    procedure Remove(ARow: TcxCustomRow);
    procedure RemoveAll;
    procedure RemoveChildren;
    procedure ResetOwnerCount;
    procedure RestoreIndex;
    procedure RestoreParent;
    procedure SetVerticalGrid(Value: TcxCustomVerticalGrid); virtual;

    property ID: Integer read FID;
    property OwnerRows: TcxVerticalGridRows read GetOwnerRows;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Styles: TcxvgCustomRowStyles read FStyles write FStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // override
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    // visualization
    procedure Collapse(ARecurse: Boolean);
    procedure Expand(ARecurse: Boolean);
    function GetFirstVisibleChild: TcxCustomRow;
    function GetLastVisibleChild: TcxCustomRow;
    function HasChildren: Boolean;
    function HasVisibleChildren: Boolean;
    function IndexOf(ARow: TcxCustomRow): Integer;
    function IsCategory: Boolean; virtual;
    function IsChild(ARow: TcxCustomRow): Boolean;
    function IsFirst: Boolean;
    function IsFirstVisible: Boolean;
    function IsLast: Boolean;
    function IsLastVisible: Boolean;
    function IsRootLevel: Boolean;
    function IsParent(ARow: TcxCustomRow): Boolean;
    procedure MakeVisible;
    procedure RestoreDefaults;

    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property Count: Integer read GetCount;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Focused: Boolean read GetFocused write SetFocused;
    property Height: Integer read FHeight write SetHeight default -1;
    property Index: Integer read FIndex write SetIndex;
    property Level: Integer read GetLevel;
    property Options: TcxRowOptions read FOptions write SetOptions;
    property Parent: TcxCustomRow read GetParent write SetParent;
    property Rows[Index: Integer]: TcxCustomRow read GetRow;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid write SetVerticalGrid;
    property ViewInfo: TcxCustomRowViewInfo read GetViewInfo;
    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleIndex: Integer read FVisibleIndex;
  end;

  { TcxCategoryRow }

  TcxCategoryRow = class(TcxCustomRow)
  strict private
    function GetProperties: TcxCaptionRowProperties;
    function GetStyles: TcxCategoryRowStyles;
    procedure SetProperties(Value: TcxCaptionRowProperties);
    procedure SetStyles(Value: TcxCategoryRowStyles);
  protected
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    function GetDefaultHeight: Integer; override;
    function GetStylesClass: TcxvgCustomRowStylesClass; override;
  public
    function CreateHeaderInfo: TcxCustomRowHeaderInfo; override;
    function CreateViewInfo: TcxCustomRowViewInfo; override;
    function IsCategory: Boolean; override;
  published
    property Expanded;
    property Height;
    property Options;
    property Properties: TcxCaptionRowProperties read GetProperties write SetProperties;
    property Styles: TcxCategoryRowStyles read GetStyles write SetStyles;
    property Visible;
  end;

  TcxCustomEditorRowProperties = class;

  { TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions }

  TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions = class(TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptions)
  published
    property Enabled;
    property Options;
  end;

  { TcxEditorRowPropertiesFilterPopupOptions }

  TcxEditorRowPropertiesFilterPopupOptions = class(TcxCustomEditContainerItemFilterPopupOptions)
  private
    function GetIncrementalFiltering: TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions;
    procedure SetIncrementalFiltering(AValue: TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions);
  protected
    function GetIncrementalFilteringOptionsClass: TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptionsClass; override;
  published
    property AddValueItems;
    property Enabled;
    property FilteredItemsList;
    property IncrementalFiltering: TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions read GetIncrementalFiltering
      write SetIncrementalFiltering;
    property MRUItemsList;
    property MultiSelect;
  end;

  { TcxEditorRowPropertiesExcelFilterPopupOptions }

  TcxEditorRowPropertiesExcelFilterPopupOptions = class(TcxCustomEditContainerItemExcelFilterPopupOptions)
  published
    property ApplyChanges;
    property DateTimeValuesPageType;
    property DefaultPage;
    property FilteredItemsList;
    property NumericValuesPageType;
  end;

  { TcxEditorRowPropertiesOptions }

  TcxEditorRowPropertiesOptions = class(TcxCustomEditContainerItemOptions)
  private
    function GetExcelFilterPopup: TcxEditorRowPropertiesExcelFilterPopupOptions;
    function GetFilterPopup: TcxEditorRowPropertiesFilterPopupOptions;
    procedure SetFilterPopup(AValue: TcxEditorRowPropertiesFilterPopupOptions);
    procedure SetExcelFilterPopup(AValue: TcxEditorRowPropertiesExcelFilterPopupOptions);
  protected
    function GetExcelFilterPopupOptionsClass: TcxCustomEditContainerItemExcelFilterPopupOptionsClass; override;
    function GetFilterPopupOptionsClass: TcxCustomEditContainerItemFilterPopupOptionsClass; override;
  published
    property Editing;
    property ExcelFilterPopup: TcxEditorRowPropertiesExcelFilterPopupOptions read GetExcelFilterPopup write SetExcelFilterPopup;
    property Filtering;
    property FilteringWithFindPanel;
    property FilterPopup: TcxEditorRowPropertiesFilterPopupOptions read GetFilterPopup write SetFilterPopup;
    property FilterPopupMode;
    property IgnoreTimeForFiltering;
    property IncSearch;
    property ShowEditButtons;
  end;

  { TcxCellEdit }

  TcxCellEdit = class(TcxCustomInplaceEditContainer)
  private
    FCalculating: Boolean;
    FEditRowProperties: TcxCustomEditorRowProperties;
    FRow: TcxCustomRow;

    function GetVerticalGrid: TcxCustomVerticalGrid;
    function GetViewInfo: TcxCustomRowViewInfo;
  protected
    FCellIndex: Integer;
    procedure Calculate(ACellViewInfo: TcxRowValueInfo);
    function CanEdit: Boolean; override;
    function CanFind: Boolean; override;
    function CanFocus: Boolean; override;
    function CanInitEditing: Boolean; override;
    function CanTabStop: Boolean; override;
    procedure DoCalculateEditViewInfo(AEditViewInfo: TcxEditCellViewInfo); override;
    procedure DoGetDisplayText(ARecordIndex: TdxNativeInt; var AText: string); override;
    function DoGetPropertiesFromEvent(AEvent: TcxGetEditPropertiesEvent; AData: Pointer;
      AProperties: TcxCustomEditProperties): TcxCustomEditProperties; override;
    function GetCurrentValue: Variant; override;
    function GetDataBindingClass: TcxItemDataBindingClass; override;
    function GetDisplayValue(AProperties: TcxCustomEditProperties; ARecordIndex: Integer): Variant; override;
    function GetEditValue: Variant; override;
    function GetFilterCaption: string; override;
    function GetFilterLinkName: string; override;
    function GetOptionsClass: TcxCustomEditContainerItemOptionsClass; override;
    function GetValue(ARecordIndex: Integer): Variant; override;
    function GetValueCount: Integer; override;
    function HasDataTextHandler: Boolean; override;
    procedure SetCurrentValue(const Value: Variant); override;
    procedure SetValue(ARecordIndex: Integer; const Value: Variant); override;
    procedure ValidateDrawValue(const Value: Variant; AEditViewInfo: TcxEditCellViewInfo); override;

    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CellIndex: Integer read FCellIndex;
    property EditRowProperties: TcxCustomEditorRowProperties read FEditRowProperties;
    property Options;
    property Row: TcxCustomRow read FRow;
    property ViewInfo: TcxCustomRowViewInfo read GetViewInfo;
  end;

  TcxCellEditClass = class of TcxCellEdit;

  { TcxCustomEditorRowProperties }

  TcxVerticalGridGetDisplayTextEvent = procedure(Sender: TcxCustomEditorRowProperties;
    ARecord: Integer; var AText: string) of object;
  TcxVerticalGridGetEditPropertiesEvent = procedure(Sender: TcxCustomEditorRowProperties;
    ARecordIndex: Integer; var AProperties: TcxCustomEditProperties) of object;
  TcxVerticalGridValidateDrawValueEvent = procedure(Sender: TcxCustomEditorRowProperties;
    ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo) of object;

  TcxCustomEditorRowProperties = class(TcxCaptionRowProperties,
    IcxEditorPropertiesContainer)
  strict private
    FEditContainer: TcxCellEdit;

    FOnGetDisplayText: TcxVerticalGridGetDisplayTextEvent;
    FOnValidateDrawValue: TcxVerticalGridValidateDrawValueEvent;

    function GetDataBinding: TcxItemDataBinding;
    function GetDisplayEditProperty(Index: Integer): TcxCustomEditProperties;
    function GetDisplayText(Index: Integer): string;
    function GetEditViewData: TcxCustomEditViewData;
    function GetEditProperties: TcxCustomEditProperties;
    function GetEditPropertiesClass: TcxCustomEditPropertiesClass;
    function GetEditPropertiesClassName: string;
    function GetFilteringDateRanges: TdxFilteringDateRanges;
    function GetIEditorPropertiesContainer: IcxEditorPropertiesContainer;
    function GetItemIndex: Integer;
    function GetItemLink: TObject;
    function GetOptions: TcxEditorRowPropertiesOptions;
    function GetRepositoryItem: TcxEditRepositoryItem;
    function GetPropertiesValue: TcxCustomEditProperties;
    function GetValue: Variant;
    function GetValueByIndex(Index: Integer): Variant;
    procedure SetDataBinding(Value: TcxItemDataBinding);
    procedure SetEditProperties(Value: TcxCustomEditProperties);
    procedure SetEditPropertiesClass(Value: TcxCustomEditPropertiesClass);
    procedure SetEditPropertiesClassName(const Value: string);
    procedure SetOptions(Value: TcxEditorRowPropertiesOptions);
    procedure SetRepositoryItem(Value: TcxEditRepositoryItem);
    procedure SetValue(const Value: Variant);
    // events delegation
    function GetOnGetEditingProperties: TcxVerticalGridGetEditPropertiesEvent;
    function GetOnGetEditProperties: TcxVerticalGridGetEditPropertiesEvent;
    function GetOnGetFilterValues: TcxGetFilterValuesEvent;
    function GetOnInitFilteringDateRanges: TdxInitDateRangesEvent;
    function GetOnUserFiltering: TcxUserFilteringEvent;
    function GetOnUserFilteringEx: TcxUserFilteringExEvent;
    procedure SetOnGetEditingProperties(Value: TcxVerticalGridGetEditPropertiesEvent);
    procedure SetOnGetEditProperties(Value: TcxVerticalGridGetEditPropertiesEvent);
    procedure SetOnGetFilterValues(Value: TcxGetFilterValuesEvent);
    procedure SetOnInitFilteringDateRanges(Value: TdxInitDateRangesEvent);
    procedure SetOnUserFiltering(Value: TcxUserFilteringEvent);
    procedure SetOnUserFilteringEx(Value: TcxUserFilteringExEvent);
  protected
    function GetEditContainer: TcxCellEdit; override;
    function GetFilterableByPopupMenu: Boolean; override;
    function GetFiltered: Boolean; override;
    function GetInplaceEditContainerClass: TcxCellEditClass; virtual;
    procedure DoGetDisplayTextEvent(ARecordIndex: Integer; var Text: string); virtual;
    procedure DoValidateDrawValue(const Value: Variant; ARecordIndex: Integer; AData: TcxEditValidateInfo); virtual;
    procedure SetFiltered(AValue: Boolean); override;

    property EditViewData: TcxCustomEditViewData read GetEditViewData;
    property IEditorPropertiesContainer: IcxEditorPropertiesContainer
      read GetIEditorPropertiesContainer implements IcxEditorPropertiesContainer;
    property DataBinding: TcxItemDataBinding read GetDataBinding write SetDataBinding;
    property Options: TcxEditorRowPropertiesOptions read GetOptions write SetOptions;
    property Value: Variant read GetValue write SetValue;
    property OnGetDisplayText: TcxVerticalGridGetDisplayTextEvent read FOnGetDisplayText write FOnGetDisplayText;
    property OnGetEditProperties: TcxVerticalGridGetEditPropertiesEvent read GetOnGetEditProperties write SetOnGetEditProperties;
    property OnGetFilterValues: TcxGetFilterValuesEvent read GetOnGetFilterValues write SetOnGetFilterValues;
    property OnInitFilteringDateRanges: TdxInitDateRangesEvent read GetOnInitFilteringDateRanges write SetOnInitFilteringDateRanges;
    property OnGetEditingProperties: TcxVerticalGridGetEditPropertiesEvent read GetOnGetEditingProperties write SetOnGetEditingProperties;
    property OnValidateDrawValue: TcxVerticalGridValidateDrawValueEvent read FOnValidateDrawValue write FOnValidateDrawValue;
    property OnUserFiltering: TcxUserFilteringEvent read GetOnUserFiltering write SetOnUserFiltering;
    property OnUserFilteringEx: TcxUserFilteringExEvent read GetOnUserFilteringEx write SetOnUserFilteringEx;
  public
    constructor CreateEx(ARow: TcxCustomRow); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DisplayEditProperties[Index: Integer]: TcxCustomEditProperties read GetDisplayEditProperty;
    property DisplayTexts[Index: Integer]: string read GetDisplayText;
    property EditPropertiesClass: TcxCustomEditPropertiesClass read GetEditPropertiesClass write SetEditPropertiesClass;
    property Filtered;
    property FilteringDateRanges: TdxFilteringDateRanges read GetFilteringDateRanges;
    property ItemIndex: Integer read GetItemIndex;
    property ItemLink: TObject read GetItemLink;
    property PropertiesValue: TcxCustomEditProperties read GetPropertiesValue;
    property Values[Index: Integer]: Variant read GetValueByIndex;
  published
    property EditPropertiesClassName: string read GetEditPropertiesClassName write SetEditPropertiesClassName;
    property EditProperties: TcxCustomEditProperties read GetEditProperties write SetEditProperties;
    property RepositoryItem: TcxEditRepositoryItem read GetRepositoryItem write SetRepositoryItem;
  end;

  TcxEditorRowPropertiesClass = class of TcxCustomEditorRowProperties;

  { TcxEditorRowProperties }

  TcxEditorRowProperties = class(TcxCustomEditorRowProperties)
  public
    procedure Assign(Source: TPersistent); override;

    property Filtered;
    property FilteringDateRanges;
  published
    property DataBinding;
    property Options;
    property Value;
    property OnGetDisplayText;
    property OnGetEditProperties;
    property OnGetEditingProperties;
    property OnGetFilterValues;
    property OnInitFilteringDateRanges;
    property OnValidateDrawValue;
    property OnUserFiltering;
    property OnUserFilteringEx;
  end;

  { TcxCustomEditorRow }

  TcxCustomEditorRow = class(TcxCustomRow)
  private
    FEditPropertiesEvents: TNotifyEvent;
    FPropertiesEvents: TNotifyEvent;
    function GetProperties: TcxCustomEditorRowProperties;
    function GetStyles: TcxEditorRowStyles;
    procedure SetProperties(Value: TcxCustomEditorRowProperties);
    procedure SetStyles(Value: TcxEditorRowStyles);
  protected
    function CanFocus: Boolean; override;
    function EditContainer: TcxCellEdit;
    function GetDefaultHeight: Integer; override;
    function GetEditContainer(ACellIndex: Integer): TcxCellEdit; override;
    function GetEditContainerCount: Integer; override;
    function GetPropertiesClass: TcxRowPropertiesClass; override;
    function GetStylesClass: TcxvgCustomRowStylesClass; override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetVerticalGrid(Value: TcxCustomVerticalGrid); override;
    property Styles: TcxEditorRowStyles read GetStyles write SetStyles;
    property Properties: TcxCustomEditorRowProperties read GetProperties write SetProperties;
  public
    function CreateHeaderInfo: TcxCustomRowHeaderInfo; override;
    function CreateViewInfo: TcxCustomRowViewInfo; override;
  published
    property EditPropertiesEvents: TNotifyEvent read FEditPropertiesEvents write FEditPropertiesEvents;
    property Options;
    property PropertiesEvents: TNotifyEvent read FPropertiesEvents write FPropertiesEvents;
  end;

  { TcxEditorRow }

  TcxEditorRow = class(TcxCustomEditorRow)
  private
    function GetProperties: TcxEditorRowProperties;
    procedure SetProperties(Value: TcxEditorRowProperties);
  protected
    function GetPropertiesClass: TcxRowPropertiesClass; override;
  published
    property Expanded;
    property Height;
    property Options;
    property Properties: TcxEditorRowProperties read GetProperties write SetProperties;
    property Styles;
    property Visible;
  end;

  { TcxMultiEditorRow }

  TSeparatorKind = (skVertLine, skString);

  { TSeparatorInfo }

  TSeparatorInfo = record
    Width: Integer;
    Kind: TSeparatorKind;
    Caption: string;
    TextFlags: Integer;
    ViewParams: TcxViewParams;
  end;

  { TcxMultiEditorRowPropertiesOptions }

  TcxMultiEditorRowPropertiesOptions = class(TcxEditorRowPropertiesOptions)
  published
    property Focusing;
    property TabStop;
  end;

  { TcxCollectionItemEditorRowProperties }

  TcxCollectionItemEditorRowProperties = class(TcxCustomEditorRowProperties)
  strict private
    FEditPropertiesEvents: TNotifyEvent;
    FWidth: Integer;

    function GetOptions: TcxMultiEditorRowPropertiesOptions;
    procedure SetOptions(Value: TcxMultiEditorRowPropertiesOptions);
    procedure SetWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function GetDisplayName: string; override;
    function GetOwner: TPersistent; override;
    //
    property Options: TcxMultiEditorRowPropertiesOptions read GetOptions write SetOptions;
  public
    constructor CreateEx(ARow: TcxCustomRow); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EditPropertiesEvents: TNotifyEvent read FEditPropertiesEvents write FEditPropertiesEvents;
    property Width: Integer read FWidth write SetWidth default 50;
  end;

  { TcxEditorRowItemProperties }

  TcxEditorRowItemProperties = class(TcxCollectionItemEditorRowProperties)
  public
    procedure Assign(Source: TPersistent); override;

    property Filtered;
    property FilteringDateRanges;
  published
    property DataBinding;
    property Options;
    property Value;
    property OnGetDisplayText;
    property OnGetEditProperties;
    property OnGetEditingProperties;
    property OnGetFilterValues;
    property OnInitFilteringDateRanges;
    property OnValidateDrawValue;
    property OnUserFiltering;
    property OnUserFilteringEx;
  end;

  { TcxEditorPropertiesCollection }

  TcxEditorPropertiesCollection = class(TCollection)
  strict private
    FRow: TcxCustomMultiEditorRow;

    function GetItem(Index: Integer): TcxEditorRowItemProperties;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetCollectionItemClass: TCollectionItemClass; virtual;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ARow: TcxCustomMultiEditorRow); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;

    function Add: TcxEditorRowItemProperties;
    function GetNamePath: string; override;

    property Items[Index: Integer]: TcxEditorRowItemProperties read GetItem; default;
    property Row: TcxCustomMultiEditorRow read FRow;
  end;

  TcxEditorPropertiesCollectionClass = class of TcxEditorPropertiesCollection;

  { TcxMultiEditorRowProperties }

  TcxMultiEditorRowProperties = class(TcxCustomRowProperties)
  strict private
    FEditors: TcxEditorPropertiesCollection;
    FFixed: Boolean;
    FSeparatorAlignmentVert: TcxAlignmentVert;
    FSeparatorKind: TSeparatorKind;
    FSeparatorString: string;

    procedure SetFixed(Value: Boolean);
    procedure SetSeparatorAlignmentVert(Value: TcxAlignmentVert);
    procedure SetSeparatorKind(Value: TSeparatorKind);
    procedure SetSeparatorString(const Value: string);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function GetCollectionClass: TcxEditorPropertiesCollectionClass; virtual;
    function GetOwner: TPersistent; override;
  public
    constructor CreateEx(ARow: TcxCustomRow); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Editors: TcxEditorPropertiesCollection read FEditors write FEditors;
    property Fixed: Boolean read FFixed write SetFixed default False;
    property SeparatorAlignmentVert: TcxAlignmentVert read FSeparatorAlignmentVert write SetSeparatorAlignmentVert default vaTop;
    property SeparatorKind: TSeparatorKind read FSeparatorKind write SetSeparatorKind default skVertLine;
    property SeparatorString: string read FSeparatorString write SetSeparatorString;
  end;

  { TcxCustomMultiEditorRow }

  TcxCustomMultiEditorRow = class(TcxCustomRow)
  strict private
    function GetProperties: TcxMultiEditorRowProperties;
    function GetStyles: TcxEditorRowStyles;
    procedure SetProperties(const Value: TcxMultiEditorRowProperties);
    procedure SetStyles(Value: TcxEditorRowStyles);
  protected
    function CanFocus: Boolean; override;
    function CanTabStop: Boolean; override;
    procedure EditorsChanged; virtual;
    function GetDefaultHeight: Integer; override;
    function GetEditContainer(ACellIndex: Integer): TcxCellEdit; override;
    function GetEditContainerCount: Integer; override;
    function GetPropertiesClass: TcxRowPropertiesClass; override;
    function GetStylesClass: TcxvgCustomRowStylesClass; override;

    property Styles: TcxEditorRowStyles read GetStyles write SetStyles;
    property Properties: TcxMultiEditorRowProperties read GetProperties write SetProperties;
  public
    function CreateHeaderInfo: TcxCustomRowHeaderInfo; override;
    function CreateViewInfo: TcxCustomRowViewInfo; override;
  end;

  { TcxMultiEditorRow }

  TcxMultiEditorRow = class(TcxCustomMultiEditorRow)
  published
    property Expanded;
    property Height;
    property Options;
    property Properties;
    property Styles;
    property Visible;
  end;

  { TcxVerticalGridRows }

  TcxVerticalGridRows = class
  private
    FList: TList;
    FLockCount: Integer;
    FNextID: Integer;
    FOwner: TcxCustomVerticalGrid;
    FCount: Integer;
    FMaxVisibleLevel: Integer;
    FVisibleRows: TList;
    function GetCount: Integer;
    function GetRoot: TcxCustomRow;
    function GetRow(Index: Integer): TcxCustomRow;
    function GetVisibleRowCount: Integer;
    function GetVisibleRow(Index: Integer): TcxCustomRow;
    procedure SetRow(Index: Integer; const Value: TcxCustomRow);
  protected
    procedure Add(ARow: TcxCustomRow);
    procedure AddChild(AParent, ARow: TcxCustomRow);
    procedure BeginUpdate;
    procedure Changed(ARebuild: Boolean = False); virtual;
    procedure CheckList;
    procedure Clear;
    procedure EndUpdate;
    function FindLoadedParent(AID: Integer): TcxCustomRow;
    function FindRowByID(AID: Integer): TcxCustomRow;
    function FindRowByStoredName(const AName: string): TcxCustomRow;
    function GetNextID: Integer;
    function IsRowVisible(ARow: TcxCustomRow): Boolean;
    procedure PrepareList;
    procedure Remove(ARow: TcxCustomRow);
    procedure RestoreDefaults;
    procedure UnprepareList;
    procedure UpdateVisibleRows;

    property MaxVisibleLevel: Integer read FMaxVisibleLevel;
    property Root: TcxCustomRow read GetRoot;
    property Owner: TcxCustomVerticalGrid read FOwner;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property VisibleRows[Index: Integer]: TcxCustomRow read GetVisibleRow;
  public
    constructor Create(AOwner: TcxCustomVerticalGrid);
    destructor Destroy; override;
    procedure AssignRows(Source: TcxVerticalGridRows);
    function IndexOf(ARow: TcxCustomRow): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxCustomRow read GetRow write SetRow; default;
  end;

  TcxvgUnboundLayoutStyle = (ulsBandsView, ulsSingleRecordView);
  TcxvgLayoutStyle = (lsBandsView, lsSingleRecordView, lsMultiRecordView);

  { TBandInfoList }

  TBandInfo = record
    BandIndex: Integer;
    RowsCount: Integer;
    BandHeight: Integer;
    FirstRow: TcxCustomRow;
  end;

  TBandInfoList = class(TcxDataList)
  private
    function GetItem(Index: Integer): TBandInfo;
  public
    constructor Create;
    function Add(ABandIndex, ARowsCount, ABandHeight: Integer;
      AFirstRow: TcxCustomRow): Integer;
    property Items[Index: Integer]: TBandInfo read GetItem; default;
  end;

  { TcxvgCustomScrollStrategy }

  TcxvgCustomScrollStrategy = class
  private
    FBandsInfo: TBandInfoList;
    FCheckingCoordinate: Boolean;
    FLeftVisibleRecord: Integer;
    FPixelScrollRecordOffset: Integer;
    FScroller: TcxvgScroller;
    FTopVisibleRowIndex: Integer;
    function GetController: TcxvgController; inline;
    function GetDataController: TcxCustomDataController; inline;
    function GetScrollBarPos: Integer;
    function GetVerticalGrid: TcxCustomVerticalGrid; inline;
		function GetViewInfo: TcxvgCustomViewInfo; inline;
    function GetVisibleRowCount: Integer;
    procedure SetScrollBarPos(Value: Integer);
    procedure SetTopVisibleRowIndex(Value: Integer);
  protected
    procedure BeginGestureScroll(APos: TPoint); virtual;
    function CanContinueForward(Index: Integer): Boolean; virtual;
    function CanContinueBackward(Index: Integer): Boolean; virtual;
    function CanCalcRowsOnTheNextBand(ALeft, ANextBandIndex: Integer): Boolean; virtual;
    function CanRecordPixelScrolling: Boolean; virtual;
    procedure CheckLeftVisibleRecordAndOffset(var ALeftVisibleRecord, APixelScrollOffset: Integer); virtual;
    function DataScrollSize: Integer; virtual;
    procedure DoCheckLeftVisibleRecordAndOffset(var ALeftVisibleRecord, APixelScrollOffset: Integer); virtual;
    function DoGetScrollBarPos: Integer; virtual;
    procedure DoSetLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset: Integer);
    procedure DoSetScrollBarPos(Value: Integer); virtual;
    function GetBandInterval: Integer; virtual; abstract;
    function GetBandWidth: Integer;
    function GetBottomVisibleChild(ARow: TcxCustomRow): TcxCustomRow;
    function GetFullRowHeight(ARow: TcxCustomRow; out ChildCount: Integer): Integer;
    function GetHScrollbarAreaHeight: Integer;
    function GetLeftVisibleBand: Integer; virtual; abstract;
    function GetScrollBarOffsetBegin: Integer; virtual;
    function GetScrollBarOffsetEnd: Integer; virtual;
    function GetVisibleBandCount: Integer; virtual; abstract;
    function GetVisibleCount(ABeginIndex, AAreaHeight, AStep: Integer;
      AForward: Boolean): Integer; virtual;
    function GetVisibleValueCount: Integer; virtual; abstract;
    function GetVScrollbarAreaWidth: Integer;
    function IsBehindRightClientEdge(X: Integer): Boolean;
    function IsHideHScrollBar: Boolean; virtual;
    function IsHideVScrollBar: Boolean; virtual;
    function IsRecordsScrollMode: Boolean; virtual;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;
    procedure ScrollH(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollV(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure SetLeftVisibleBand(Value: Integer); virtual; abstract;
    procedure SetLeftVisibleRecord(Value: Integer); virtual;
    procedure SetLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset: Integer);
    function SetRecordVisible(ARecordIndex: Integer): Boolean; virtual;
    procedure SetTopVisibleRowIndexAndBand(Index: Integer);
    function VisibleDataScrollSize: Integer; virtual;

    property Scroller: TcxvgScroller read FScroller;
    property ScrollBarPos: Integer read GetScrollBarPos write SetScrollBarPos;
  public
    constructor Create(AScroller: TcxvgScroller); virtual;
    destructor Destroy; override;
    procedure CheckDecreaseLeftIndex; virtual;
    procedure CheckDecreaseTopIndex(AScrollRectHeight: Integer); virtual;
    function CheckTopVisibleIndex(AIndex, AStep: Integer): Integer; virtual;
    function FindNextCustomItem(AFocusedItemIndex, AItemCount: Integer;
       AGoForward: Boolean; var AItemIndex: Integer): Boolean;
    function FindNextRecord(AFocusedRecordIndex: Integer; AGoForward: Boolean): Integer;
    function FocusNextRecord(AFocusedRecordIndex: Integer; AGoForward: Boolean): Boolean;
		function GetBandIndexByRowIndex(ARowIndex: Integer): Integer; virtual; abstract;
    function GetVisibleCountFromBottom(ABottomIndex, AHeight: Integer): Integer;
    function GetVisibleCountFromTop(ATopIndex, AHeight: Integer): Integer;
    function GetFirstRowByBandIndex(ABandIndex: Integer): TcxCustomRow; virtual;
    procedure InitHScrollBarParameters; virtual;
    procedure InitVScrollBarParameters; virtual;
    procedure RecalcBandsInfo; virtual;
    procedure ScrollRecords(AForward: Boolean; ACount: Integer);
    procedure SetRowMaxVisible(ARow: TcxCustomRow); virtual;

    property BandInterval: Integer read GetBandInterval;
    property BandWidth: Integer read GetBandWidth;
    property BandsInfo: TBandInfoList read FBandsInfo;
    property Controller: TcxvgController read GetController;
    property DataController: TcxCustomDataController read GetDataController;
    property LeftVisibleBand: Integer read GetLeftVisibleBand write SetLeftVisibleBand;
    property LeftVisibleRecord: Integer read FLeftVisibleRecord write SetLeftVisibleRecord;
    property PixelScrollRecordOffset: Integer read FPixelScrollRecordOffset;
    property ScrollBarOffsetBegin: Integer read GetScrollBarOffsetBegin;
    property ScrollBarOffsetEnd: Integer read GetScrollBarOffsetEnd;
    property TopVisibleRowIndex: Integer read FTopVisibleRowIndex write SetTopVisibleRowIndex;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
    property VisibleBandCount: Integer read GetVisibleBandCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property VisibleValueCount: Integer read GetVisibleValueCount;
  end;

  TcxvgCustomScrollStrategyClass = class of TcxvgCustomScrollStrategy;

  { TcxvgSingleRecordScrollStrategy }

  TcxvgSingleRecordScrollStrategy = class(TcxvgCustomScrollStrategy)
  protected
    function GetBandInterval: Integer; override;
    function GetLeftVisibleBand: Integer; override;
    function GetVisibleBandCount: Integer; override;
    function GetVisibleValueCount: Integer; override;
    procedure SetLeftVisibleBand(Value: Integer); override;
  public
    function GetBandIndexByRowIndex(ARowIndex: Integer): Integer; override;
  end;

  { TcxvgMultiRecordsScrollStrategy }

  TcxvgMultiRecordsScrollStrategy = class(TcxvgCustomScrollStrategy)
  private
    FMaxLeftVisibleRecord: Integer;
    FMaxPixelScrollRecordOffset: Integer;
    function GetRecordsInterval: Integer;
    function GetRecordSpace: Integer;
  protected
    procedure BeginGestureScroll(APos: TPoint); override;
    procedure CalculatePixelScrollInfo(var ARecordIndex, ARecordOffset: Integer;
      AMaxRecordIndex, AMaxRecordOffset: Integer; ADelta: Integer; out AOverPan: Boolean);
    function CanRecordPixelScrolling: Boolean; override;
    procedure CheckMaxLeftVisibleRecordIndexAndOffset(var ALeftVisibleRecordIndex, APixelScrollRecordOffset: Integer);
    function DataScrollSize: Integer; override;
    procedure DoCheckLeftVisibleRecordAndOffset(var ALeftVisibleRecord, APixelScrollOffset: Integer); override;
    function DoGetScrollBarPos: Integer; override;
    procedure DoSetScrollBarPos(Value: Integer); override;
    function GetBandInterval: Integer; override;
    function GetLeftVisibleBand: Integer; override;
    procedure GetPixelScrollLeftRecordIndexAndOffsetByRightRecord(ARightRecord: Integer;
      out ALeftRecord, APixelScrollRecordOffset: Integer);
    function GetVisibleBandCount: Integer; override;
    function GetVisibleValueCount: Integer; override;
    function IsPixelBasedScrollDataPos: Boolean;
    function IsRecordPixelScrolling: Boolean;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); override;
    procedure SetLeftVisibleBand(Value: Integer); override;
    function SetRecordVisible(ARecordIndex: Integer): Boolean; override;
    procedure UpdatePixelScrollLeftRecordIndexAndOffsetMaxValues;
    function VisibleDataScrollSize: Integer; override;

    property RecordsInterval: Integer read GetRecordsInterval;
  public
    procedure CheckDecreaseLeftIndex; override;
    function GetBandIndexByRowIndex(ARowIndex: Integer): Integer; override;
  end;

  { TcxvgBandsScrollStrategy }

  TcxvgBandsScrollStrategy = class(TcxvgCustomScrollStrategy)
  private
    FLeftVisibleBand: Integer;
  protected
    function CanCalcRowsOnTheNextBand(ALeft, ANextBandIndex: Integer): Boolean; override;
    function GetBandInterval: Integer; override;
    function GetBandViewRowMaxVisibleTopIndex(ARow: TcxCustomRow): Integer;
    function GetLeftVisibleBand: Integer; override;
    function GetVisibleBandCount: Integer; override;
    function GetVisibleValueCount: Integer; override;
    function IsRecordsScrollMode: Boolean; override;
    function InternalSetLeftVisibleBand(ABandIndex: Integer; ACheckTopVisibleRow: Boolean): Boolean;
    procedure SetLeftVisibleBand(Value: Integer); override;
    procedure CheckTopVisibleRowIndex;
  public
    procedure CheckDecreaseTopIndex(AScrollRectHeight: Integer); override;
    function CheckTopVisibleIndex(AIndex, AStep: Integer): Integer; override;
    function GetBandIndexByRowIndex(ARowIndex: Integer): Integer; override;
    procedure InitHScrollBarParameters; override;
    function IsHideVScrollBar: Boolean; override;
    procedure RecalcBandsInfo; override;
    procedure SetRowMaxVisible(ARow: TcxCustomRow); override;
  end;

  { TcxvgScroller }

  TcxvgScroller = class
  private
    FCheckDecreaseLeftIndex: Boolean;
    FCheckDecreaseTopIndex: Boolean;
    FSaveLeftVisibleBand: Integer;
    FSaveLeftVisibleRecord: Integer;
    FSaveTopVisibleRowIndex: Integer;
    FScrollStrategy: TcxvgCustomScrollStrategy;
    FVerticalGrid: TcxCustomVerticalGrid;
    procedure CheckDecreaseLeftIndex;
    procedure CheckDecreaseTopIndex;
    function GetBandsInfo: TBandInfoList;
    function GetCheckDecrease: Boolean;
    function GetFocusedRecordIndex: Integer;
    function GetLeftVisibleBand: Integer;
    function GetLeftVisibleRecord: Integer;
    function GetPixelScrollRecordOffset: Integer;
    function GetTopVisibleRowIndex: Integer;
    function GetViewInfo: TcxvgCustomViewInfo;
    function GetVisibleCountFromBottom(ARowIndex: Integer): Integer;
    function GetVisibleRowCount: Integer;
    function GetVisibleValueCount: Integer;
    procedure SetCheckDecrease(Value: Boolean);
    procedure SetLeftVisibleBand(Value: Integer);
    procedure SetLeftVisibleRecord(Value: Integer);
    procedure SetTopVisibleRowIndex(Value: Integer);
  protected
    procedure BeginGestureScroll(APos: TPoint);
    function CreateScrollStrategy(AScroller: TcxvgScroller): TcxvgCustomScrollStrategy; virtual;
    procedure LayoutStyleChanged;
    procedure RestoreLayout; virtual;
    procedure SaveLayout; virtual;
    // other
    property FocusedRecordIndex: Integer read GetFocusedRecordIndex;
    property SaveLeftVisibleBand: Integer read FSaveLeftVisibleBand;
    property SaveLeftVisibleRecord: Integer read FSaveLeftVisibleRecord;
    property SaveTopRowIndex: Integer read FSaveTopVisibleRowIndex;
    property ScrollStrategy: TcxvgCustomScrollStrategy read FScrollStrategy;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  public
    constructor Create(AVerticalGrid: TcxCustomVerticalGrid); virtual;
    destructor Destroy; override;
    // navigation
    function GoToFirst: Boolean;
    function GoToLast: Boolean;
    function GoToNext: Boolean;
    function GoToPrev: Boolean;

    function GetBandIndexByRowIndex(ARowIndex: Integer): Integer;
    procedure InitScrollBarsParameters; virtual;
    procedure RecalcBandsInfo;
    procedure RecreateScrollStrategy;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
    function SetRecordVisible(ARecordIndex: Integer): Boolean; virtual;
    procedure SetRowVisible(ARow: TcxCustomRow); virtual;
    procedure SetRowMaxVisible(ARow: TcxCustomRow);
    property BandsInfo: TBandInfoList read GetBandsInfo;
    property CheckDecrease: Boolean read GetCheckDecrease write SetCheckDecrease;
    property LeftVisibleBand: Integer read GetLeftVisibleBand write SetLeftVisibleBand;
    property LeftVisibleRecord: Integer read GetLeftVisibleRecord write SetLeftVisibleRecord;
    property PixelScrollRecordOffset: Integer read GetPixelScrollRecordOffset;
    property TopVisibleRowIndex: Integer read GetTopVisibleRowIndex write SetTopVisibleRowIndex;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property VisibleValueCount: Integer read GetVisibleValueCount;
  end;

  { TcxvgHitTest }

  TcxvgHitTest = class(TcxCustomHitTestController)
  private
    FHitInControl: Boolean;
    FHitBandIndex: Integer;
    FHitRow: TcxCustomRow;
    FHitCellIndex: Integer;
    function GetHitAtRowHeader: Boolean;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    function GetViewInfo: TcxvgCustomViewInfo;
  protected
    FNewHitTestItem: TObject;
    function AllowDesignMouseEvents(X, Y: Integer; AShift: TShiftState): Boolean; override;
    procedure CalcBandsHitTest(AViewInfo: TcxvgCustomViewInfo); virtual;
    function CalcCustomizingHitTest: Boolean; virtual;
    procedure CalcFilterBoxHitTest;
    procedure CalcFindPanelHitTest;
    function CalcNavigatorHitTest: Boolean;
    function CalcRowHeaderHitTest(AHeaderInfo: TcxCustomRowHeaderInfo): Boolean; virtual;
    function CalcRowHitTest(ARowViewInfo: TcxCustomRowViewInfo): Boolean;
    procedure CalcRowValuesHitTest(ARowViewInfo: TcxCustomRowViewInfo); virtual;
    procedure CalcRowsHitTest(AViewInfo: TcxvgCustomViewInfo);
    function CanMoving: Boolean; virtual;
    function CanSizing: Boolean; overload;
    function CanSizing(var ASizeDirection: TcxDragSizingDirection): Boolean; overload; virtual;
    function Check(const ARect: TRect): Boolean;
    procedure DoCalculate; override;
    function GetCurrentCursor: TCursor; override;
    function GetHitAtFilterBox: Boolean; override;
    function GetHitAtFindPanel: Boolean; override;
    function GetHitAtNavigator: Boolean; override;
    function GetState(Index: Integer): Boolean;
    procedure SetHitState(Index: Integer; Value: Boolean);

    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  public
    property HitAtBandSizing: Boolean index vghc_HitAtBandSizing read GetState;
    property HitAtButton: Boolean index vghc_HitAtButton read GetState;
    property HitAtCaption: Boolean index vghc_HitAtCaption read GetState;
    property HitAtCaptionFilterButton: Boolean index vghc_HitAtCaptionFilterButton read GetState;
    property HitAtCustomize: Boolean index vghc_HitAtCustomize read GetState;
    property HitAtDivider: Boolean index vghc_HitAtDivider read GetState;
    property HitAtEmpty: Boolean index vghc_HitAtEmpty read GetState;
    property HitAtImage: Boolean index vghc_HitAtImage read GetState;
    property HitAtIndent: Boolean index vghc_HitAtIndent read GetState;
    property HitAtRowHeader: Boolean read GetHitAtRowHeader;
    property HitAtRowSizing: Boolean index vghc_HitAtRowSizing read GetState;
    property HitAtValue: Boolean index vghc_HitAtValue read GetState;
    property HitBandIndex: Integer read FHitBandIndex;
    property HitInControl: Boolean read FHitInControl;
    property HitRow: TcxCustomRow read FHitRow;
    property HitCellIndex: Integer read FHitCellIndex;
  end;

  { TcxvgCellNavigator }

  TcxvgCellNavigator = class(TcxCustomCellNavigator)
  private
    function GetController: TcxvgController;
    function GetRecordCount: Integer;
    function GetRow(Index: Integer): TcxCustomRow;
  protected
    function FindNextRecord(AForward: Boolean): Boolean;
    function FocusNextCell(AForward, ANextRow, ATabStopNavigation: Boolean): Boolean; reintroduce;
    procedure FocusRecordRowCell(ARecordIndex: Integer; ARow: TcxCustomRow; ACellIndex: Integer);
    function GetContainerCount(ARowIndex: Integer): Integer;

    function GetCellIndexForNextRow(AForward: Boolean; ANextRow: TcxCustomRow): Integer;
    function GetRowForNextRecord(AForward: Boolean): TcxCustomRow;
    function NavigateHorizontal(AForward: Boolean): Boolean;
    function NavigateMultiEditorRow(ARow: TcxCustomRow; AForward: Boolean): Boolean;
    function NavigateVertical(AForward, AForceNextRow: Boolean): Boolean;
    function ValidateNextCell(ARow: TcxCustomRow; AForward: Boolean; var ACellIndex: Integer): Boolean;
  public
    constructor Create(AController: TcxCustomControlController); override;
    function FindNextRow(var ARow: TcxCustomRow; var ACellIndex: Integer; AForward: Boolean): Boolean;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property Controller: TcxvgController read GetController;
    property RecordCount: Integer read GetRecordCount;
    property Rows[Index: Integer]: TcxCustomRow read GetRow;
  end;

  { TcxvgDragImageHelper }

  TcxvgDragImageHelper = class(TcxDragImageHelper)
  protected
    procedure DragAndDrop(const P: TPoint); override;
  end;

  { TcxvgDragRowObject }

  TcxvgDragRowObject = class(TcxDragControlObject)
  private
    function GetRow: TcxCustomRow;
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    property Row: TcxCustomRow read GetRow;
  end;

  { TcxvgController }

  TcxvgResizeKind = (rkNone, rkRowSizing, rkDivider, rkBandSizing);

  TcxvgController = class(TcxCustomControlController, IUnknown, IcxDragSizing)
  private
    FAutoScrollObject: TcxControllerAutoScrollingObject;
    FCellIndex: Integer;
    FDragFromCustomizingForm: Boolean;
    FDragRow: TcxCustomRow;
    FForceShowEditor: Boolean;
    FFocusedRow: TcxCustomRow;
    FLockIncSearch: Boolean;
    FProcessMultiEditorRow: Boolean;
    FResizeKind: TcxvgResizeKind;
    FSaveFocusedRow: TcxCustomRow;
    FSaveSeparatorPos: Integer;
    FScrollDown: Boolean;
    FScroller: TcxvgScroller;
    FScrollTimer: TTimer;
    FSizingRow: TcxCustomRow;
    FSizingValue: Integer;
    FTrackingEnabled: Boolean;
    FWasScroll: Boolean;
    procedure CheckMoveToCustomizationForm;
    function GetFocusedItem: TcxCellEdit;
    function GetHitTest: TcxvgHitTest;
    function GetNavigator: TcxvgCellNavigator;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    function GetViewInfo: TcxvgCustomViewInfo;
    procedure SetFocusedItemInternal(Value: TcxCellEdit);
    procedure SetFocusedRow(Value: TcxCustomRow);
    procedure SetResizeKind(Value: TcxvgResizeKind);
    procedure OnScrollTimer(Sender: TObject);
    procedure StartScrollTimer;
    procedure StopScrollTimer;
  protected
    FLockUpdate: Integer;
    ResizeDirection: TcxDragSizingDirection;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxDragSizing
    function CanSizing(ADirection: TcxDragSizingDirection): Boolean; virtual;
    function GetSizingBoundsRect(ADirection: TcxDragSizingDirection): TRect; virtual;
    function GetSizingIncrement(ADirection: TcxDragSizingDirection): Integer; virtual;
    function IsDynamicUpdate: Boolean; virtual;
    procedure SetSizeDelta(ADirection: TcxDragSizingDirection; ADelta: Integer); virtual;
    // drag'n'drop
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    procedure StartDrag(var DragObject: TDragObject); override;

    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BehaviorChanged; override;
    function CanAppend(ACheckOptions: Boolean): Boolean; virtual;
    function CanChangeRecord: Boolean; virtual;
    function CanDelete(ACheckOptions: Boolean): Boolean; virtual;
    function CanInsert(ACheckOptions: Boolean): Boolean; virtual;
    function CanTrack(const AShift: TShiftState): Boolean; virtual;
    procedure CheckPostData;
    procedure CheckRowTracking(Shift: TShiftState; X, Y: Integer);
    procedure DoCancelMode; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoNextPage(AForward: Boolean; Shift: TShiftState); override;
    procedure DoUpdateRowAndCell(ANewRow: TcxCustomRow; ANewCellIndex: Integer); virtual;
    procedure FocusedItemChanged(APrevFocusedItem: TcxCustomInplaceEditContainer); override;
    procedure FocusRow(ARow: TcxCustomRow; AMakeVisible: Boolean);
    function GetCancelEditingOnExit: Boolean; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetFindPanelClass: TcxControlFindPanelClass; override;
    function GetFindStartPosition(ARecordIndex: TdxNativeInt; AItemIndex: Integer;
      out AHighlightedText: string): Integer; override;
    function GetFocusedCellViewInfo(AEditContainer: TcxCustomInplaceEditContainer): TcxEditCellViewInfo; override;
    function GetImmediateEditor: Boolean; override;
    function GetNavigatorClass: TcxCustomCellNavigatorClass; override;
    function GetPostDataOnChangeItem: Boolean; virtual;
    function GetResizeDirection: TcxDragSizingDirection; override;
    procedure InternalSetRowAndCell(ARow: TcxCustomRow; ACellIndex: Integer);
    function IsImmediatePost: Boolean; override;
    function IsInternalDragging(ADragObject: TObject): Boolean;
    function IsKeyForController(AKey: Word; AShift: TShiftState): Boolean; override;
    function IsLocked: Boolean;
    procedure RefreshFocusedRow; virtual;
    procedure SetFocusedCellEdit(ACellEdit: TcxCellEdit);
    procedure SetFocusedRowAndCell(Value: TcxCustomRow; ACellIndex: Integer); virtual;
    procedure UpdatePaintStyle; virtual;
    procedure UpdateRecord(ARecordIndex: TdxNativeInt); override;

    property DragRow: TcxCustomRow read FDragRow;
    property DragFromCustomizingForm: Boolean read FDragFromCustomizingForm;
    property FocusedItem: TcxCellEdit read GetFocusedItem write SetFocusedItemInternal;
    property ForceShowEditor: Boolean read FForceShowEditor;
    property ProcessMultiEditorRow: Boolean read FProcessMultiEditorRow;
    property ResizeKind: TcxvgResizeKind read FResizeKind write SetResizeKind;
    property SaveFocusedRow: TcxCustomRow read FSaveFocusedRow;
    property SaveSeparatorPos: Integer read FSaveSeparatorPos;
  public
    // override
    constructor Create(AOwner: TcxEditingControl); override;
    destructor Destroy; override;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    procedure Clear; override;
    procedure ControlFocusChanged; override;
    function GetCursor(X, Y: Integer): TCursor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MakeFocusedItemVisible; override;
    procedure MakeFocusedRecordVisible; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    // drag and drop
    function CanDrag(X, Y: Integer): Boolean; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    // scrolling
    procedure InitScrollBarsParameters; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    // recreate viewinfo
    procedure RestoreLayout; virtual;
    procedure SaveLayout; virtual;

    procedure AppendRecord; virtual;
    procedure DeleteSelection; virtual;
    procedure InsertRecord; virtual;
    procedure MakeRowVisible(ARow: TcxCustomRow); virtual;
    function MakeRecordVisible(AIndex: Integer): Boolean; virtual;
    property CellIndex: Integer read FCellIndex;
    property FocusedRow: TcxCustomRow read FFocusedRow write SetFocusedRow;
    property HitTest: TcxvgHitTest read GetHitTest;
    property Navigator: TcxvgCellNavigator read GetNavigator;
    property Scroller: TcxvgScroller read FScroller;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  end;

  { TcxvgMultiRecordsController }

  TcxvgMultiRecordsController = class(TcxvgController)
  private
    function GetOptionsBehavior: TcxvgMultiRecordsOptionsBehavior;
    function GetOptionsData: TcxvgMultiRecordsOptionsData;
    function GetVerticalGrid: TcxVirtualVerticalGrid;
  protected
    function CanAppend(ACheckOptions: Boolean): Boolean; override;
    function CanChangeRecord: Boolean; override;
    function CanDelete(ACheckOptions: Boolean): Boolean; override;
    function CanHandleDeleteRecordKeys: Boolean; override;
    function CanInsert(ACheckOptions: Boolean): Boolean; override;
    procedure FocusedRecordChanged(APrevFocusedRowIndex, AFocusedRowIndex: Integer); override;
    function GetCancelEditingOnExit: Boolean; override;
    function GetFocusedRecordIndex: TdxNativeInt; override;
    function GetPostDataOnChangeItem: Boolean; override;
    function IncSearchKeyDown(AKey: Word; AShift: TShiftState): Word; override;
    function IsImmediatePost: Boolean; override;
    procedure RefreshIncSearchItem; virtual;
    procedure SetFocusedRecordIndex(Value: TdxNativeInt); override;
    property OptionsBehavior: TcxvgMultiRecordsOptionsBehavior read GetOptionsBehavior;
    property OptionsData: TcxvgMultiRecordsOptionsData read GetOptionsData;
    property VerticalGrid: TcxVirtualVerticalGrid read GetVerticalGrid;
  public
    procedure DeleteSelection; override;
  end;

  { TcxCustomVerticalGrid }

  TcxVerticalGridDrawHeaderEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; APainter: TcxvgPainter;
    AHeaderViewInfo: TcxCustomRowHeaderInfo;
    var Done: Boolean) of object;

  TcxVerticalGridDrawValueEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; APainter: TcxvgPainter;
    AValueInfo: TcxRowValueInfo;
    var Done: Boolean) of object;

  TcxVerticalGridDrawBackgroundEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; const R: TRect; const AViewParams: TcxViewParams;
    var Done: Boolean) of object;

  TcxVerticalGridEditingEvent = procedure(Sender: TObject;
    ARowProperties: TcxCustomEditorRowProperties; var Allow: Boolean) of object;

  TcxVerticalGridChangedEvent = procedure(Sender: TObject;
    ARowProperties: TcxCustomEditorRowProperties) of object;

  TcxVerticalGridItemChangedEvent = procedure(Sender: TObject;
    AOldRow: TcxCustomRow; AOldCellIndex: Integer) of object;

  TcxCustomVerticalGrid = class(TcxExtEditingControl,
    IcxStoredObject,
    IcxStoredParent,
    IcxDataControllerConditionalFormattingProviderOwner,
    IdxSkinSupport)
  private
    FCategoryFont: TFont;
    FClearingRows: Boolean;
    FConditionalFormattingProvider: TcxVerticalGridConditionalFormattingProvider;
    FCustomizing: TcxVerticalGridCustomizing;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FNewLoadMode: Boolean;
    FRootRow: TcxCustomRow;
    FRows: TcxVerticalGridRows;
    FSaveDragMode: TDragMode;
    FSaveDragCursor: TCursor;
    FStoringName: string;
    FStylesEvents: TNotifyEvent;
    FVersion: Integer;

    FOnCustomizationVisibleChanged: TNotifyEvent;
    FOnDrawBackground: TcxVerticalGridDrawBackgroundEvent;
    FOnDrawRowHeader: TcxVerticalGridDrawHeaderEvent;
    FOnDrawValue: TcxVerticalGridDrawValueEvent;
    FOnEdited: TcxVerticalGridChangedEvent;
    FOnEditing: TcxVerticalGridEditingEvent;
    FOnEditValueChanged: TcxVerticalGridChangedEvent;
    FOnItemChanged: TcxVerticalGridItemChangedEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnLeftVisibleBandIndexChanged: TNotifyEvent;
    FOnLeftVisibleRecordIndexChanged: TNotifyEvent;
    FOnTopRowIndexChanged: TNotifyEvent;

    procedure CategoryFontChanged(Sender: TObject);
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
    function GetController: TcxvgController;
    function GetDateTimeHandling: TcxVerticalGridDateTimeHandling;
    function GetDragHeaderInfo: TcxCustomRowHeaderInfo;
    function GetFilterBox: TcxVerticalGridFilterBox;
    function GetFiltering: TcxVerticalGridFiltering;
    function GetFocusedRow: TcxCustomRow;
    function GetHitTest: TcxvgHitTest;
    function GetInplaceEditor: TcxCustomEdit;
    function GetIsEditing: Boolean;
    function GetLeftVisibleBand: Integer;
    function GetLeftVisibleRecord: Integer;
    function GetOptionsBehavior: TcxvgOptionsBehavior;
    function GetOptionsView: TcxvgOptionsView;
    function GetPainter: TcxvgPainter;
    function GetRecordCount: Integer;
    function GetStyles: TcxVerticalGridStyles;
    function GetTopVisibleRowIndex: Integer;
    function GetViewInfo: TcxvgCustomViewInfo;
    procedure ImageListChange(Sender: TObject);
    procedure SetDateTimeHandling(AValue: TcxVerticalGridDateTimeHandling);
    procedure SetFilterBox(AValue: TcxVerticalGridFilterBox);
    procedure SetFiltering(AValue: TcxVerticalGridFiltering);
    procedure SetFocusedRow(Value: TcxCustomRow);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLeftVisibleBand(Value: Integer);
    procedure SetLeftVisibleRecord(Value: Integer);
    procedure SetOptionsBehavior(Value: TcxvgOptionsBehavior);
    procedure SetOptionsView(Value: TcxvgOptionsView);
    procedure SetStyles(Value: TcxVerticalGridStyles);
    procedure SetTopVisibleRowIndex(Value: Integer);
    // layout version
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
  protected
    // IcxStoredObject
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure DeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    // vcl methods
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    // cxControls
    procedure BiDiModeChanged; override;
    procedure BoundsChanged; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure FontChanged; override;
    procedure InitControl; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    // cxControls - drag'n'drop
    function GetDragObjectClass: TDragControlObjectClass; override;
    // cxExtEditingControl
    procedure AfterLayoutChanged; override;
    procedure CreateSubClasses; override;
    procedure DataChanged; override;
    procedure DestroySubClasses; override;
    procedure DoEdited(AItem: TcxCustomInplaceEditContainer); override;
    function DoEditing(AItem: TcxCustomInplaceEditContainer): Boolean; override;
    procedure DoEditValueChanged(AItem: TcxCustomInplaceEditContainer); override;
    procedure DoLayoutChanged; override;
    function DragDropImageDisplayRect: TRect; override;
    procedure DrawDragDropImage(ADragBitmap: TBitmap; ACanvas: TcxCanvas); override;
    function GetControllerClass: TcxCustomControlControllerClass; override;
    function GetControlStylesClass: TcxCustomControlStylesClass; override;
    function GetDateTimeHandlingClass: TcxControlOptionsDateTimeHandlingClass; override;
    function GetDragImageHelperClass: TcxDragImageHelperClass; override;
    function GetEditCellDataBindingClass: TcxItemDataBindingClass; virtual;
    function GetFilterValueListClass: TcxControlFilterValueListClass; override;
    function GetHitTestControllerClass: TcxHitTestControllerClass; override;
    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsFilterBoxClass: TcxControlOptionsFilterBoxClass; override;
    function GetOptionsFilteringClass: TcxControlOptionsFilteringClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetPainterClass: TcxCustomControlPainterClass; override;
    function HasDragDropImages: Boolean; override;
    function NeedCallChangedOnItemRemoved(AItem: TcxCustomInplaceEditContainer): Boolean; override;
    procedure RecreateViewInfo; override;
    // layout
    procedure RestoreLayout;
    // virtual
    procedure AfterLoadedRows;
    procedure BeforeLoadedRows;
    function CanBandSizing: Boolean; virtual;
    procedure CheckRowClass(ARowClass: TcxCustomRowClass); virtual;
    procedure CheckGridModeBufferCount;
    procedure CheckLayoutRealign;
    procedure FreeRowsViewInfo;
    procedure DoCustomizationVisibleChanged; virtual;
    function DoDrawBackgroundPart(const R: TRect; const AViewParams: TcxViewParams): Boolean; virtual;
    function DoDrawRowHeader(AHeaderViewInfo: TcxCustomRowHeaderInfo): Boolean; virtual;
    function DoDrawValue(AValueInfo: TcxRowValueInfo): Boolean; virtual;
    procedure DoItemChanged(AOldRow: TcxCustomRow; AOldCellIndex: Integer); virtual;
    procedure DoLeftVisibleBandIndexChanged; virtual;
    procedure DoLeftVisibleRecordIndexChanged; virtual;
    procedure DoTopRowIndexChanged; virtual;
    function GetCalcHelperClass: TcxvgCustomPaintStyleCalcHelperClass; virtual;
    function GetCellAutoHeight: Boolean; virtual;
    function GetCustomizingClass: TcxVerticalGridCustomizingClass; virtual;
    function GetEditorRowClass: TcxCustomRowClass; virtual;
    function GetMultiEditorRowClass: TcxCustomRowClass; virtual;
    function GetRowContentStyleIndex(AProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): Integer; virtual;
    function GetScrollStrategyClass: TcxvgCustomScrollStrategyClass; virtual; abstract;
    procedure InitDataController; virtual;
    function IsRecordPixelScrolling: Boolean; virtual;
    procedure PaintStyleChanged;
    procedure RemoveRowFromVerticalGrid(ARow: TcxCustomRow);
    procedure RowsChanged; virtual;
    procedure SetCustomization(AActive: Boolean);
    procedure UpdateDesignEditor; virtual;
    procedure ValidateFocusedRow;

    property CategoryFont: TFont read FCategoryFont;
    property ConditionalFormattingProvider: TcxVerticalGridConditionalFormattingProvider read FConditionalFormattingProvider;
    property Controller: TcxvgController read GetController;
    property Customizing: TcxVerticalGridCustomizing read FCustomizing write FCustomizing;
    property DateTimeHandling: TcxVerticalGridDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property DragHeaderInfo: TcxCustomRowHeaderInfo read GetDragHeaderInfo;
    property FilterBox: TcxVerticalGridFilterBox read GetFilterBox write SetFilterBox;
    property Filtering: TcxVerticalGridFiltering read GetFiltering write SetFiltering;
    property LeftVisibleRecord: Integer read GetLeftVisibleRecord write SetLeftVisibleRecord;
    property NewLoadMode: Boolean read FNewLoadMode write FNewLoadMode;
    property Painter: TcxvgPainter read GetPainter;
    property RecordCount: Integer read GetRecordCount;
    property RootRow: TcxCustomRow read FRootRow;
    property OnCustomizationFormVisibleChanged: TNotifyEvent read FOnCustomizationVisibleChanged write FOnCustomizationVisibleChanged;
    property OnDrawBackground: TcxVerticalGridDrawBackgroundEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawRowHeader: TcxVerticalGridDrawHeaderEvent read FOnDrawRowHeader write FOnDrawRowHeader;
    property OnDrawValue: TcxVerticalGridDrawValueEvent read FOnDrawValue write FOnDrawValue;
    property OnEdited: TcxVerticalGridChangedEvent read FOnEdited write FOnEdited;
    property OnEditing: TcxVerticalGridEditingEvent read FOnEditing write FOnEditing;
    property OnEditValueChanged: TcxVerticalGridChangedEvent read FOnEditValueChanged write FOnEditValueChanged;
    property OnItemChanged: TcxVerticalGridItemChangedEvent read FOnItemChanged write FOnItemChanged;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnLeftVisibleBandIndexChanged: TNotifyEvent read FOnLeftVisibleBandIndexChanged write FOnLeftVisibleBandIndexChanged;
    property OnLeftVisibleRecordIndexChanged: TNotifyEvent read FOnLeftVisibleRecordIndexChanged write FOnLeftVisibleRecordIndexChanged;
    property OnTopRowIndexChanged: TNotifyEvent read FOnTopRowIndexChanged write FOnTopRowIndexChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
    // work with rows
    function Add(ARowClass: TcxCustomRowClass): TcxCustomRow;
    function AddChild(AParent: TcxCustomRow; ARowClass: TcxCustomRowClass): TcxCustomRow;
    procedure AssignRows(Source: TcxCustomVerticalGrid); virtual;
    procedure ClearRows;
    function FirstRow: TcxCustomRow;
    function FirstVisibleRow: TcxCustomRow;
    procedure FocusRow(ARow: TcxCustomRow; ACellIndex: Integer = 0);
    procedure FullCollapse;
    procedure FullExpand;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function HasRows: Boolean;
    function HasVisibleRows: Boolean;
    function IsInternalDragging(ADragObject: TObject): Boolean;
    function IsRowVisible(ARow: TcxCustomRow): Boolean;
    function LastRow: TcxCustomRow;
    function LastVisibleRow: TcxCustomRow;
    function NextRow(ARow: TcxCustomRow): TcxCustomRow;
    function NextVisibleRow(ARow: TcxCustomRow): TcxCustomRow;
    function PrevRow(ARow: TcxCustomRow): TcxCustomRow;
    function PrevVisibleRow(ARow: TcxCustomRow): TcxCustomRow;
    procedure Remove(ARow: TcxCustomRow);
    function RowByCaption(const ACaption: string): TcxCustomRow;
    function RowByName(const AName: string): TcxCustomRow;
    // store/load
    procedure RestoreFromIniFile(const AStorageName: string);
    procedure RestoreFromRegistry(const AStorageName: string);
    procedure RestoreFromStream(AStream: TStream);
    procedure StoreToIniFile(const AStorageName: string; AReCreate: Boolean = True);
    procedure StoreToRegistry(const AStorageName: string; AReCreate: Boolean = True);
    procedure StoreToStream(AStream: TStream);
    // edit control
    procedure CancelEdit;
    procedure HideEdit;
    procedure ShowEdit;
    procedure ShowEditByKey(AKey: Char);
    procedure ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
    //find panel
    procedure ApplyFindFilterText(const AText: string);
    procedure ClearFindFilterText;
    function GetFindFilterText: string;
    procedure HideFindPanel;
    function IsFindPanelVisible: Boolean;
    procedure ShowFindPanel;

    procedure RestoreDefaults; virtual;
    // properties
    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
    property FocusedRow: TcxCustomRow read GetFocusedRow write SetFocusedRow;
    property HitTest: TcxvgHitTest read GetHitTest;
    property Images: TCustomImageList read FImages write SetImages;
    property InplaceEditor: TcxCustomEdit read GetInplaceEditor;
    property IsEditing: Boolean read GetIsEditing;
    property LeftVisibleBand: Integer read GetLeftVisibleBand write SetLeftVisibleBand;
    property LookAndFeel;
    property OptionsBehavior: TcxvgOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsData;
    property OptionsView: TcxvgOptionsView read GetOptionsView write SetOptionsView;
    property Rows: TcxVerticalGridRows read FRows write FRows;
    property StoringName: string read FStoringName write FStoringName;
    property Styles: TcxVerticalGridStyles read GetStyles write SetStyles;
    property TopVisibleRowIndex: Integer read GetTopVisibleRowIndex write SetTopVisibleRowIndex;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  published
    property BiDiMode;
    property ParentBiDiMode;
    property StylesEvents: TNotifyEvent read FStylesEvents write FStylesEvents;
  end;

  { TcxUnboundVerticalGrid }

  TcxUnboundVerticalGrid = class(TcxCustomVerticalGrid)
  private
    FLayoutStyle: TcxvgUnboundLayoutStyle;
    procedure SetLayoutStyle(Value: TcxvgUnboundLayoutStyle);
  protected
    function CanBandSizing: Boolean; override;
    function GetScrollStrategyClass: TcxvgCustomScrollStrategyClass; override;
    function GetViewInfoClass: TcxCustomControlViewInfoClass; override;
    property LayoutStyle: TcxvgUnboundLayoutStyle read FLayoutStyle write SetLayoutStyle default ulsSingleRecordView;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  IcxVGridDesignerRows = interface
  ['{D77CC392-984F-4C1E-A41C-A341FEAC93EB}']
    function GetEditorRowClass: TcxCustomRowClass;
    function GetMultiEditorRowClass: TcxCustomRowClass;
  end;

  { TcxVerticalGrid }

  TcxVerticalGrid = class(TcxUnboundVerticalGrid, IcxVGridDesignerRows)
  public
    property Customizing;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property LayoutStyle;
    property LookAndFeel;
    property OptionsView; //before OptionsBehavior
    property OptionsBehavior;
    property OptionsData;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnCustomizationFormVisibleChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawRowHeader;
    property OnDrawValue;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnInitEdit;
    property OnInitEditValue;
    property OnItemChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftVisibleBandIndexChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTopRowIndexChanged;
  end;

  { TcxVirtualVerticalGrid }

  TcxVerticalGridFocusedRecordChangedEvent = procedure(Sender: TcxVirtualVerticalGrid;
    APrevFocusedRecord, AFocusedRecord: Integer) of object;

  IcxVerticalGridDBDataContoller = interface
  ['{947072DE-3531-4010-8C44-D243FD289FDF}']
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function GetDataSetRecordCount: Integer;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    function IsRecordPixelScrollingSupported: Boolean;
    function SetScrollBarPos(APos: Integer): Boolean;
  end;

  TcxVirtualVerticalGrid = class(TcxCustomVerticalGrid, IcxVGridDesignerRows,
    IcxNavigator, IcxFilterControl)
  private
    FNavigatorNotifier: TcxNavigatorControlNotifier;
    FLayoutStyle: TcxvgLayoutStyle;
    FOnFocusedRecordChanged: TcxVerticalGridFocusedRecordChangedEvent;

    function GetFilterRecordEvent: TcxDataFilterRecordEvent;
    function GetFocusedRecordIndex: Integer;
    function GetController: TcxvgMultiRecordsController;
    function GetOptionsBehavior: TcxvgMultiRecordsOptionsBehavior;
    function GetOptionsData: TcxvgMultiRecordsOptionsData;
    function GetOptionsView: TcxvgMultiRecordsOptionsView;
    procedure SetFilterRecordEvent(Value: TcxDataFilterRecordEvent);
    procedure SetFocusedRecordIndex(Value: Integer);
    procedure SetLayoutStyle(Value: TcxvgLayoutStyle);
    procedure SetOptionsBehavior(Value: TcxvgMultiRecordsOptionsBehavior);
    procedure SetOptionsData(Value: TcxvgMultiRecordsOptionsData);
    procedure SetOptionsView(Value: TcxvgMultiRecordsOptionsView);
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
    // IcxNavigatorRecordPosition
    function NavigatorGetRecordCount: Integer; override;
    function NavigatorGetRecordIndex: Integer; override;
    // IcxNavigator
    function IcxNavigator.CanAppend = NavigatorCanAppend;
    function IcxNavigator.CanDelete = NavigatorCanDelete;
    function IcxNavigator.CanEdit = NavigatorCanEdit;
    function IcxNavigator.CanInsert = NavigatorCanInsert;
    function IcxNavigator.IsActive = NavigatorIsActive;
    function IcxNavigator.IsBof = NavigatorIsBof;
    function IcxNavigator.IsBookmarkAvailable = NavigatorIsBookmarkAvailable;
    function IcxNavigator.IsEditing = NavigatorIsEditing;
    function IcxNavigator.IsEof = NavigatorIsEof;
    procedure IcxNavigator.ClearBookmark = NavigatorClearBookmark;
    procedure IcxNavigator.DoAction = NavigatorDoAction;
    function IcxNavigator.GetNotifier = NavigatorGetNotifier;
    function IcxNavigator.IsActionSupported = NavigatorIsActionSupported;
    function NavigatorCanAppend: Boolean;
    function NavigatorCanDelete: Boolean;
    function NavigatorCanEdit: Boolean;
    function NavigatorCanInsert: Boolean;
    function NavigatorIsActive: Boolean;
    function NavigatorIsBof: Boolean;
    function NavigatorIsBookmarkAvailable: Boolean;
    function NavigatorIsEditing: Boolean;
    function NavigatorIsEof: Boolean;
    procedure NavigatorClearBookmark;
    procedure NavigatorDoAction(AButtonIndex: Integer);
    function NavigatorGetNotifier: TcxNavigatorControlNotifier;
    function NavigatorIsActionSupported(AButtonIndex: Integer): Boolean;
    procedure RefreshNavigators;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    function CanBandSizing: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    procedure ControlUpdateData(AInfo: TcxUpdateControlInfo); override;
    procedure DoFocusedRecordChanged(APrevFocusedRecord, AFocusedRecord: Integer); virtual;
    function GetCellAutoHeight: Boolean; override;
    function GetControllerClass: TcxCustomControlControllerClass; override;
    function GetNavigatorButtonsControl: IcxNavigator; override;
    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetRowContentStyleIndex(AProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): Integer; override;
    function GetScrollStrategyClass: TcxvgCustomScrollStrategyClass; override;
    function GetViewInfoClass: TcxCustomControlViewInfoClass; override;
    procedure InitDataController; override;
    function IsRecordPixelScrolling: Boolean; override;
    function IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean; override;
    procedure BeginGestureScroll(APos: TPoint); override;
    procedure DataLayoutChanged; override;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); override;
    function CanScrollContentByGestureWithoutScrollBars: Boolean; override;
    property Controller: TcxvgMultiRecordsController read GetController;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property Customizing;
    property DataController;
    property LeftVisibleRecord;
    property FocusedRecordIndex: Integer read GetFocusedRecordIndex write SetFocusedRecordIndex;
    property RecordCount;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DateTimeHandling;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FilterBox;
    property Filtering;
    property FindPanel;
    property Font;
    property Images;
    property LayoutStyle: TcxvgLayoutStyle read FLayoutStyle write SetLayoutStyle default lsSingleRecordView;
    property LookAndFeel;
    //before OptionsBehavior
    property OptionsView: TcxvgMultiRecordsOptionsView read GetOptionsView write SetOptionsView;
    property OptionsBehavior: TcxvgMultiRecordsOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsData: TcxvgMultiRecordsOptionsData read GetOptionsData write SetOptionsData;
    property Navigator;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;

    property NavigatorEvents;
    property OnClick;
    property OnContextPopup;
    property OnCustomizationFormVisibleChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawRowHeader;
    property OnDrawValue;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFilterControlDialogShow;
    property OnFilterCustomization;
    property OnFilterDialogShow;
    property OnFilterRecord: TcxDataFilterRecordEvent
      read GetFilterRecordEvent write SetFilterRecordEvent;
    property OnFindPanelVisibilityChanged;
    property OnFocusedRecordChanged: TcxVerticalGridFocusedRecordChangedEvent
      read FOnFocusedRecordChanged write FOnFocusedRecordChanged;
    property OnInitEdit;
    property OnInitEditValue;
    property OnInitFilteringDateRanges;
    property OnItemChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftVisibleBandIndexChanged;
    property OnLeftVisibleRecordIndexChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTopRowIndexChanged;
  end;

  { TcxRowValueInfo }

  TcxRowValueInfo = class(TcxEditCellViewInfo)
  private
    FFocusRect: TRect;
    FRow: TcxCustomRow;
    FRowCellIndex: Integer;
    FRecordIndex: Integer;
    function GetConditionalFormattingProvider: TcxVerticalGridConditionalFormattingProvider;
    function GetEditorRowProperties: TcxCustomEditorRowProperties;
  protected
    procedure AfterDrawCellBackground(ACanvas: TcxCanvas); override;
    procedure AfterDrawCellValue(ACanvas: TcxCanvas); override;
    procedure CalculateCellEditorBounds(AViewInfo: TcxCustomEditViewInfo; var R: TRect); override;
    procedure CanDrawCellValue(var Allow: Boolean); override;
    procedure DoCalculate; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function EditContainer: TcxCellEdit;
    function GetButtonTransparency: TcxEditButtonTransparency; override;
    function GetDisplayValue: Variant; override;
    function GetEditViewParams: TcxViewParams; override;
    function GetFocused: Boolean; override;
    function GetRecordIndex: TdxNativeInt; override;
    function GetSelectedTextColor: Integer; override;
    function GetSelectedBKColor: Integer; override;
    function FormatDisplayValue(AValue: Variant): Variant; override;
    function IncSearchParams: TcxViewParams;
    function IsAutoHeight: Boolean; override;
    function IsRecordAvailable(AIndex: Integer): Boolean;
    function NeedHighlightFindText: Boolean; override;

    property ConditionalFormattingProvider: TcxVerticalGridConditionalFormattingProvider read GetConditionalFormattingProvider;
    property EditorRowProperties: TcxCustomEditorRowProperties read GetEditorRowProperties;
  public
    function GetHeight(AContentWidth: Integer): Integer;
    procedure PaintEx(ACanvas: TcxCanvas);

    property Focused;
    property FocusRect: TRect read FFocusRect write FFocusRect;
    property Row: TcxCustomRow read FRow;
    property RowCellIndex: Integer read FRowCellIndex;
    property ViewParams;
  end;

  { TcxRowCaptionInfo }

  TcxRowCaptionInfo = class(TPersistent,
    IUnknown,
    IcxHotTrackElement,
    IcxHintableObject,
    IdxUIElementPopupWindowOwner,
    IdxFilterPopupWindowOwner)
  strict private
    FCaption: string;
    FCaptionAssigned: Boolean;
    FCaptionRect: TRect;
    FCaptionTextRect: TRect;
    FFilterButtonRect: TRect;
    FFilterButtonState: TcxButtonState;
    FHeaderInfo: TcxCustomRowHeaderInfo;
    FImageRect: TRect;
    FRowCellIndex: Integer;
    FRowProperties: TcxCaptionRowProperties;
    FState: TcxButtonState;
    FViewParams: TcxViewParams;

    function GetCaption: string;
    function GetFilterButtonWidth: Integer;
    function GetFilterPopup: TdxFilterPopupWindow;
    function GetFilterPopupAlignHorz: TcxPopupAlignHorz;
    function GetFilterPopupOwnerBounds: TRect;
    function GetFilterSmartTagHeight: Integer;
    function GetFilterSmartTagState: TcxFilterSmartTagState;
    function GetFocused: Boolean;
    function GetImageIndex: Integer;
    function GetImages: TCustomImageList;
    function GetImageSize: TSize;
    function GetIsFilterButtonActive: Boolean;
    function GetIsFilterSmartTag: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetRow: TcxCustomRow;
    function GetScaleFactor: TdxScaleFactor;
    function GetTextFlags: Integer;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    function FindPropertiesHint(const P: TPoint): string;
    function IsFilterable: Boolean;
    function IsFilterButtonAlwaysVisible: Boolean;
    function IsFilterButtonVisible: Boolean;
    function IsFilterPopupExist: Boolean;
    function IsFilterPopupOwner: Boolean;
    function IsImageVisible: Boolean;
    procedure SetCaption(AValue: string);
  protected
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxHotTrackElement
    function GetHintBounds: TRect; virtual;
    function IsNeedHint(ACanvas: TcxCanvas; const P: TPoint;
      out AText: TCaption;
      out AIsMultiLine: Boolean;
      out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean; virtual;
    procedure UpdateHotTrackState(const APoint: TPoint);
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
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

    procedure Calculate(ARect: TRect); virtual;
    function CalculateCaptionTextRect: TRect; virtual;
    function CalculateFilterButtonRect: TRect; virtual;
    function CalculateImageRect: TRect; virtual;
    procedure CheckFilterPopupOwner; virtual;
    procedure FilterButtonStateChanged; virtual;
    procedure SetViewParams(const AParams: TcxViewParams);
    procedure Invalidate(ARecalculate: Boolean); virtual;
    procedure InvalidateFilterButton; virtual;
    function NeedRecalculateOnStateChanged: Boolean; virtual;
    procedure Recalculate; virtual;
    procedure RightToLeftConversion(const AClientBounds: TRect);
    procedure SetFilterButtonState(AValue: TcxButtonState); virtual;
    procedure SetState(AValue: TcxButtonState); virtual;
    procedure ShowFilterPopup; virtual;
    procedure StateChanged; virtual;
    procedure UpdateFilterButtonHotTrackState(const APoint: TPoint); virtual;
    procedure UpdateFilterButtonStateOnMouseDown; virtual;

    property FilterButtonWidth: Integer read GetFilterButtonWidth;
    property FilterPopup: TdxFilterPopupWindow read GetFilterPopup;
    property FilterPopupOwnerBounds: TRect read GetFilterPopupOwnerBounds;
    property FilterSmartTagHeight: Integer read GetFilterSmartTagHeight;
    property HeaderInfo: TcxCustomRowHeaderInfo read FHeaderInfo;
    property ImageSize: TSize read GetImageSize;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property RowProperties: TcxCaptionRowProperties read FRowProperties;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AHeaderInfo: TcxCustomRowHeaderInfo; ARowProperties: TcxCaptionRowProperties); virtual;
    destructor Destroy; override;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    property Caption: string read GetCaption write SetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property CaptionTextRect: TRect read FCaptionTextRect;
    property FilterButtonRect: TRect read FFilterButtonRect;
    property FilterButtonState: TcxButtonState read FFilterButtonState;
    property FilterSmartTagState: TcxFilterSmartTagState read GetFilterSmartTagState;
    property Focused: Boolean read GetFocused;
    property ImageIndex: Integer read GetImageIndex;
    property ImageRect: TRect read FImageRect;
    property Images: TCustomImageList read GetImages;
    property IsFilterButtonActive: Boolean read GetIsFilterButtonActive;
    property IsFilterSmartTag: Boolean read GetIsFilterSmartTag;
    property Row: TcxCustomRow read GetRow;
    property RowCellIndex: Integer read FRowCellIndex write FRowCellIndex;
    property State: TcxButtonState read FState;
    property TextFlags: Integer read GetTextFlags;
    property ViewParams: TcxViewParams read FViewParams;
  end;

  { TcxCaptionInfoList }

  TcxCaptionInfoList = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxRowCaptionInfo;
  protected
    procedure RightToLeftConversion(const AClientBounds: TRect);
  public
    property Items[Index: Integer]: TcxRowCaptionInfo read GetItem; default;
  end;

  { TcxValueInfoList }

  TcxValueInfoList = class(TcxObjectList)
  private
    FViewInfo: TcxvgCustomViewInfo;
    function GetItem(Index: Integer): TcxRowValueInfo;
  protected
    procedure ClearRightToLeftConversionFlags;
    procedure RightToLeftConversion(const AClientBounds: TRect);
  public
    constructor Create(AViewInfo: TcxvgCustomViewInfo);
    destructor Destroy; override;

    property Items[Index: Integer]: TcxRowValueInfo read GetItem; default;
    property ViewInfo: TcxvgCustomViewInfo read FViewInfo;
  end;

  { TcxCustomRowHeaderInfo }

  TcxCustomRowHeaderInfo = class
  private
    FCaptionsInfo: TcxCaptionInfoList;
    FCategoryIndents: TIndentInfoList;
    FFocusRect: TRect;
    FIsRightToLeftConverted: Boolean;
    FLinesInfo: TLineInfoList;
    FRowIndents: TIndentInfoList;
    FTransparent: Boolean;
    FViewInfo: TcxvgCustomViewInfo;
    function GetPaintStyle: TcxvgPaintStyle;
    function GetSelected: Boolean;
    function GetShowButton: Boolean;
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    FButtonAreaBounds: TRect;
    FButtonRect: TRect;
    FButtonColor: TColor;
    FFocused: Boolean;
    FHeaderCellsRect: TRect;
    FHeaderRect: TRect;
    FIndentBounds: TRect;
    FIndentViewParams: TcxViewParams;
    FRow: TcxCustomRow;
    FViewParams: TcxViewParams;
    procedure AddBottomHorzLine(const R: TRect); virtual;
    procedure AddBoundHeaderLines; virtual;
    procedure AddNextIndentInfo(const ABounds: TRect;
      const AViewParams: TcxViewParams;
      AToCategories, AUnderline, AAddVertLine: Boolean);
    procedure AddRightVertLine(const R: TRect); virtual;
    procedure Calc(const AHeaderRect: TRect; AViewInfo: TcxvgCustomViewInfo;
      ANextRow: TcxCustomRow; ACalcBounds: Boolean);
    function CalcCaptionInfo(ARowProperties: TcxCaptionRowProperties;
      const R: TRect): TcxRowCaptionInfo;
    procedure CalcIndentBounds(ANextRow: TcxCustomRow; ACalculate: Boolean); virtual;
    function CalcIndentsInfo(ALevelCount: Integer;
      ANextRow: TcxCustomRow): TIndentRectInfoList; virtual;
    procedure CalcExpandButton;
    procedure CalcRowCaptionsInfo; virtual;
    procedure CalcViewParams(AAllowFocus: Boolean); virtual;
    procedure Clear; virtual;
    procedure DoCalcExpandButton; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function GetButtonPlaceBackgroundWidth: Integer; virtual;
    function GetCaptionViewParams: TcxViewParams; virtual;
    function GetButtonColor: TColor; virtual;
    function GetFocusRect: TRect; virtual;
    function IncreaseBoundsByLastVertLine: Boolean; virtual;
    function LeftViewPoint: Integer; virtual;
    procedure Recalculate(ANextRow: TcxCustomRow; ACalcBounds: Boolean);
    procedure RightToLeftConversion(const AClientBounds: TRect);
    procedure MakeSelectedViewParams(AFocused: Boolean);

    property IndentViewParams: TcxViewParams read FIndentViewParams;
    property PaintStyle: TcxvgPaintStyle read GetPaintStyle;
    property Selected: Boolean read GetSelected;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read FViewInfo;
  public
    constructor Create(ARow: TcxCustomRow); virtual;
    destructor Destroy; override;
    property CaptionsInfo: TcxCaptionInfoList read FCaptionsInfo;
    property CategoryIndents: TIndentInfoList read FCategoryIndents;
    property ButtonAreaBounds: TRect read FButtonAreaBounds;
    property ButtonRect: TRect read FButtonRect;
    property ButtonColor: TColor read FButtonColor;
    property Focused: Boolean read FFocused;
    property FocusRect: TRect read FFocusRect write FFocusRect;
    property HeaderCellsRect: TRect read FHeaderCellsRect;
    property HeaderRect: TRect read FHeaderRect write FHeaderRect;
    property IndentBounds: TRect read FIndentBounds;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property LinesInfo: TLineInfoList read FLinesInfo;
    property Row: TcxCustomRow read FRow;
    property RowIndents: TIndentInfoList read FRowIndents;
    property ShowButton: Boolean read GetShowButton;
    property Transparent: Boolean read FTransparent write FTransparent;
    property ViewParams: TcxViewParams read FViewParams write FViewParams;
  end;

  { TcxCustomRowViewInfo }

  TcxCustomRowViewInfo = class
  private
    FBandIndex: Integer;
    FBandRowIndex: Integer;
    FCalculatedHeight: Integer;
    FHeaderInfo: TcxCustomRowHeaderInfo;
    FInitialized: Boolean;
    FIsRightToLeftConverted: Boolean;
    FRightToLeftConversionClientBounds: TRect;
    FRow: TcxCustomRow;
    FRowRect: TRect;
    FValuesInfo: TcxValueInfoList;
    FValuesLinesInfo: TLineInfoList;
    FValuesRect: TRect;
    function GetCalculatedHeight: Integer;
    function GetRowValueInfo(ARecordIndex, ACellIndex: Integer): TcxRowValueInfo;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    function GetViewInfo: TcxvgCustomViewInfo;
  protected
    procedure AddRectValueLines(R: TRect; ALast, ABottomLineNeeded: Boolean);
    function AddValueInfo(ARecordIndex, ACellIndex: Integer): TcxRowValueInfo;
    procedure CalculateHeight(ABandWidth: Integer); virtual;
    procedure CalcValuesInfo; virtual;
    procedure CalcRowHeaderInfo(ANextRow: TcxCustomRow); virtual;
    procedure CalcPaintViewParamsLines(ANextRow: TcxCustomRow); virtual;
    procedure DoSpecialRightToLeftConversion;
    procedure ClearValuesInfo;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function GetEditContainerCount: Integer;
    function GetRowValueInfoClass: TcxRowValueInfoClass; virtual;
    function GetValuesHeight(ABandWidth: Integer; AViewInfo: TcxvgCustomViewInfo): Integer; virtual;
    procedure InitValuesInfo;
    function NeedLTRBounds: Boolean;
    procedure RightToLeftConversion(const AClientBounds: TRect);

    property RightToLeftConversionClientBounds: TRect read FRightToLeftConversionClientBounds;
    property ValuesInfo: TcxValueInfoList read FValuesInfo;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  public
    constructor Create(ARow: TcxCustomRow); virtual;
    destructor Destroy; override;
    procedure Calc(const ARowRect: TRect; AViewInfo: TcxvgCustomViewInfo; ANextRow: TcxCustomRow);
    procedure Recalculate(ANextRow: TcxCustomRow);
    procedure Update;
    procedure UpdateRecord(ARecordIndex: Integer);

    property BandIndex: Integer read FBandIndex write FBandIndex;
    property BandRowIndex: Integer read FBandRowIndex write FBandRowIndex;
    property CalculatedHeight: Integer read GetCalculatedHeight write FCalculatedHeight;
    property HeaderInfo: TcxCustomRowHeaderInfo read FHeaderInfo;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property Row: TcxCustomRow read FRow;
    property RowRect: TRect read FRowRect;
    property RowValueInfo[ARecordIndex, ACellIndex: Integer]: TcxRowValueInfo read GetRowValueInfo;
    property ValuesLinesInfo: TLineInfoList read FValuesLinesInfo;
    property ValuesRect: TRect read FValuesRect write FValuesRect;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  end;

  { TcxRowViewInfoList }

  TcxRowViewInfoList = class(TList)
  private
    function GetItem(Index: Integer): TcxCustomRowViewInfo;
  protected
    procedure RightToLeftConversion(const AClientBounds: TRect);
  public
    constructor Create;
    function Find(ARow: TcxCustomRow): TcxCustomRowViewInfo;
    property Items[Index: Integer]: TcxCustomRowViewInfo read GetItem; default;
  end;

  { TcxvgCustomViewInfo }

  TcxvgCustomViewInfo = class(TcxExtEditingControlViewInfo)
  private
    FBandBorderColor: TColor;
    FBandMinWidth: Integer;
    FBandsInterval: Integer;
    FButtonAreaSize: Integer;
    FButtonSize: Integer;
    FCalcHelper: TcxvgCustomPaintStyleCalcHelper;
    FClipRect: TRect;
    FDividerWidth: Integer;
    FExplorerButtonAreaSize: TSize;
    FExplorerButtonSize: TSize;
    FUseCategoryExplorerStyle: Boolean;
    FFocusLinesInfo: TLineInfoList;
    FFullHeaderWidth: Integer;
    FHorzLineBrush: TBrush;
    FHorzLineWidth: Integer;
    FImageSize: TSize;
    FIsRightToLeftConverted: Boolean;
    FLevelWidth: Integer;
    FLinesInfo: TLineInfoList;
    FLockDividerPos: Boolean;
    FRowHeaderMinWidth: Integer;
    FRowIndentWidth: Integer;
    FRowsViewInfo: TcxRowViewInfoList;
    FShowHeaders: Boolean;
    FVerticalGrid: TcxCustomVerticalGrid;
    FVertLineBrush: TBrush;
    FVertLineWidth: Integer;
    FViewBandWidth: Integer;
    FViewHeaderWidth: Integer;
    FViewValueWidth: Integer;
    FViewRects: TViewRects;
    procedure ClearLinesAndRows;
    function GetBandInfo: TBandInfoList;
    function GetFirstVisibleRecordIndex: Integer;
    function GetMinRowHeight: Integer;
    function GetPainter: TcxvgPainter;
    function GetScroller: TcxvgScroller;
    function GetVisibleRow(Index: Integer): TcxCustomRow;
    function GetVisibleRowCount: Integer;
    procedure UpdateScroller;
    function GetMaxVisibleLevel: Integer;
  protected
    // overriding methods
    function CalculateDefaultEditHeight: Integer; override;
    procedure DoCalculate; override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    procedure UpdateSelection; override;
    //
    procedure AddBandRowsLines(const R: TRect); virtual;
    procedure AddBottomValueSide(ARowViewInfo: TcxCustomRowViewInfo; const R: TRect);
    procedure AddEmptyRects; virtual; abstract;
    procedure AddRightValueSide(ARowViewInfo: TcxCustomRowViewInfo; const R: TRect; ALast: Boolean); virtual;
    procedure CalcBandRects; virtual;
    procedure CalcBandRowsViewInfo(var AFirstRowIndex: Integer;
      const ABandRect: TRect; ABandIndex, ABandRowCount: Integer); virtual;
    procedure CalcBandWidth; virtual;
    procedure CalculateBandsInfo; virtual;
    function CalculateClientRect: TRect; override;
    procedure CalcEmpty; virtual;
    procedure CalcCategoryExplorerStyle;
    procedure CalcLayoutGeneral; virtual;
    procedure CalcRowsHeight;
    procedure CalcRowRects(ARowViewInfo: TcxCustomRowViewInfo); virtual;
    procedure CalcRowsViewInfo; virtual;
    procedure CalcViewRects; virtual;
    function CanAddRowToBand(const ARowRect, ABandRect: TRect; ABandRowIndex: Integer): Boolean; virtual;
    function CanUpdateRecord(ARecordIndex: Integer): Boolean; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckBiDiMode; virtual;
    procedure CheckMaxRowHeaderWidth(var Value: Integer; AValueMinWidth: Integer); virtual;
    procedure CheckRowHeaderWidth;
    function CheckShowRowHeader(ARowViewInfo: TcxCustomRowViewInfo): Boolean;
    procedure Clear; virtual;
    procedure ClearValuesInfo;
    procedure CreateBand(ABandHeight, ABandWidth: Integer); virtual;
    procedure CreateBrushes; virtual;
    function CreateCalcHelper: TcxvgCustomPaintStyleCalcHelper; virtual;
    procedure DestroyBrushes; virtual;
    function GetBandSizeableRect(const ABandRect: TRect): TRect; virtual;
    function GetRowAutoHeight(ARow: TcxCustomRow): Boolean; virtual;

    function GetLTRValueRect(AValueIndex: Integer; ARowViewInfo: TcxCustomRowViewInfo): TRect; virtual;
    function GetLTRValuesRect(ARowViewInfo: TcxCustomRowViewInfo): TRect; virtual;

    function GetPixelScrollContentSize: Integer; virtual;
    function GetViewBandWidth: Integer; virtual;
    function GetViewHeaderWidth: Integer; virtual;
    function GetViewMinHeaderWidth: Integer; virtual;
    function GetViewValueWidth: Integer; virtual;
    function GetVisibleValueCount: Integer; virtual; abstract;
    procedure LayoutStyleChanged;
    procedure PrepareCalculateBandsInfo;
    procedure Reset; virtual;
    function ScaleRowRects(ARowViewInfo: TcxCustomRowViewInfo): TRectScaler; virtual;
    procedure SetDividerPos(APos: Integer); virtual;
    procedure SetValueWidth(AWidth: Integer); virtual;

    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;

    property Scroller: TcxvgScroller read GetScroller;
    property LockDividerPos: Boolean read FLockDividerPos;
    property MaxVisibleLevel: Integer read GetMaxVisibleLevel;
    property ViewBandWidth: Integer read FViewBandWidth write FViewBandWidth;
    property ViewHeaderWidth: Integer read FViewHeaderWidth write FViewHeaderWidth;
  public
    constructor Create(AOwner: TcxEditingControl); override;
    destructor Destroy; override;
    procedure CalcEditCell(const ABounds: TRect; ARowValueInfo: TcxRowValueInfo); virtual;
    function CalcRowHeight(ARow: TcxCustomRow): Integer;
    procedure ChangeFocusedRow(ANewFocus, AOldFocus: TcxCustomRow); virtual;
    function GetDefaultGridModeBufferCount: Integer; virtual;
    function GetRowViewInfo(ARow: TcxCustomRow): TcxCustomRowViewInfo;
    function GetValueRect(AValueIndex: Integer; ARowViewInfo: TcxCustomRowViewInfo): TRect; virtual;
    function GetValuesRect(ARowViewInfo: TcxCustomRowViewInfo): TRect; virtual;
    procedure UpdateRecord(ARecordIndex: Integer); virtual;
    // properties
    property BandBorderColor: TColor read FBandBorderColor write FBandBorderColor;
    property BandMinWidth: Integer read FBandMinWidth;
    property BandInfo: TBandInfoList read GetBandInfo;
    property BandsInterval: Integer read FBandsInterval;
    property ButtonAreaSize: Integer read FButtonAreaSize write FButtonAreaSize;
    property ButtonSize: Integer read FButtonSize write FButtonSize;
    property CalcHelper: TcxvgCustomPaintStyleCalcHelper read FCalcHelper;
    property ClipRect: TRect read FClipRect;
    property DividerWidth: Integer read FDividerWidth write FDividerWidth;
    property ExplorerButtonAreaSize: TSize read FExplorerButtonAreaSize;
    property ExplorerButtonSize: TSize read FExplorerButtonSize;
    property UseCategoryExplorerStyle: Boolean read FUseCategoryExplorerStyle;
    property FirstVisibleRecordIndex: Integer read GetFirstVisibleRecordIndex;
    property FocusLinesInfo: TLineInfoList read FFocusLinesInfo;
    property FullHeaderWidth: Integer read FFullHeaderWidth;
    property HorzLineBrush: TBrush read FHorzLineBrush write FHorzLineBrush;
    property HorzLineWidth: Integer read FHorzLineWidth;
    property ImageSize: TSize read FImageSize write FImageSize;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property LevelWidth: Integer read FLevelWidth write FLevelWidth;
    property LinesInfo: TLineInfoList read FLinesInfo write FLinesInfo;
    property Painter: TcxvgPainter read GetPainter;
    property RowHeaderMinWidth: Integer read FRowHeaderMinWidth;
    property RowIndentWidth: Integer read FRowIndentWidth;
    property RowsViewInfo: TcxRowViewInfoList read FRowsViewInfo;
    property ShowHeaders: Boolean read FShowHeaders;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
    property VertLineBrush: TBrush read FVertLineBrush write FVertLineBrush;
    property VertLineWidth: Integer read FVertLineWidth write FVertLineWidth;
    property ViewRects: TViewRects read FViewRects;
    property ViewValueWidth: Integer read FViewValueWidth write FViewValueWidth;
    property VisibleRows[Index: Integer]: TcxCustomRow read GetVisibleRow;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property VisibleValueCount: Integer read GetVisibleValueCount;
  end;

  { TcxvgCustomPaintStyleCalcHelper }

  TcxvgCustomPaintStyleCalcHelper = class
  private
    FScroller: TcxvgScroller;
    FViewInfo: TcxvgCustomViewInfo;
    FVerticalGrid: TcxCustomVerticalGrid;

    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateHorzLineBrush: TBrush; virtual; abstract;
    function CreateVertLineBrush: TBrush; virtual; abstract;
    function GetBandBorderColor: TColor; virtual;
    function GetDividerWidth: Integer; virtual; abstract;
    function GetIndentWidth: Integer; virtual; abstract;
  public
    constructor Create(AViewInfo: TcxvgCustomViewInfo); virtual;
    procedure AddBoundHeaderLines(ARowHeaderInfo: TcxCustomRowHeaderInfo); virtual; abstract;
    procedure AddDivider(ALinesInfo: TLineInfoList; const R: TRect;
      AColor: TColor; AIsSeparator: Boolean); virtual; abstract;
    procedure AddHeaderIndentLines(ARowHeaderInfo: TcxCustomRowHeaderInfo;
      const R: TRect; const AViewParams: TcxViewParams;
      AToCategories, AUnderline, AAddVertLine: Boolean); virtual; abstract;
    procedure CalcPaintViewParamsLines(ARowViewInfo: TcxCustomRowViewInfo;
      ANextRow: TcxCustomRow); virtual; abstract;
    function ChangeFocusedRow(ANewFocus, AOldFocus: TcxCustomRow): TRect; virtual; abstract;
    function GetBackgroundColor: TColor; virtual;
    function GetCategoryColor: TColor; virtual; abstract;
    function GetCategoryFocusRect(ARowHeaderInfo: TcxCustomRowHeaderInfo): TRect; virtual; abstract;
    function GetCategoryTextColor: TColor; virtual; abstract;
    function GetContentColor(AFocused: Boolean): TColor; virtual; abstract;
    function GetContentTextColor: TColor; virtual; abstract;
    function GetContentEvenColor(AFocused: Boolean): TColor; virtual;
    function GetContentEvenTextColor: TColor; virtual;
    function GetContentOddColor(AFocused: Boolean): TColor; virtual;
    function GetContentOddTextColor: TColor; virtual;
    function GetFindPanelColor: TColor; virtual;
    function GetHeaderColor: TColor; virtual; abstract;
    function GetHeaderTextColor: TColor; virtual; abstract;
    function GetIndentViewParams(ARow, AParentIndentRow: TcxCustomRow): TcxViewParams; virtual; abstract;
    function IsBottomLineNeeded(ANextRow: TcxCustomRow): Boolean; virtual;
    function IsDrawValueFocusRect: Boolean; virtual; abstract;

    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Scroller: TcxvgScroller read FScroller;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read FViewInfo;
  end;

  { TcxvgDotNetStyleCalcHelper }

  TcxvgDotNetStyleCalcHelper = class(TcxvgCustomPaintStyleCalcHelper)
  protected
    function CreateHorzLineBrush: TBrush; override;
    function CreateVertLineBrush: TBrush; override;
    function GetDividerWidth: Integer; override;
    function GetIndentWidth: Integer; override;
  public
    procedure AddBoundHeaderLines(ARowHeaderInfo: TcxCustomRowHeaderInfo); override;
    procedure AddDivider(ALinesInfo: TLineInfoList; const R: TRect;
      AColor: TColor; AIsSeparator: Boolean); override;
    procedure AddHeaderIndentLines(ARowHeaderInfo: TcxCustomRowHeaderInfo;
      const R: TRect; const AViewParams: TcxViewParams;
      AToCategories, AUnderline, AAddVertLine: Boolean); override;
    procedure CalcPaintViewParamsLines(ARowViewInfo: TcxCustomRowViewInfo;
      ANextRow: TcxCustomRow); override;
    function ChangeFocusedRow(ANewFocus, AOldFocus: TcxCustomRow): TRect; override;
    function GetCategoryColor: TColor; override;
    function GetCategoryFocusRect(ARowHeaderInfo: TcxCustomRowHeaderInfo): TRect; override;
    function GetCategoryTextColor: TColor; override;
    function GetContentColor(AFocused: Boolean): TColor; override;
    function GetContentTextColor: TColor; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderTextColor: TColor; override;
    function GetIndentViewParams(ARow, AParentIndentRow: TcxCustomRow): TcxViewParams; override;
    function IsDrawValueFocusRect: Boolean; override;
  end;

  { TcxvgStyle3DCalcHelper }

	TcxvgStyle3DCalcHelper = class(TcxvgCustomPaintStyleCalcHelper)
  protected
    function CreateHorzLineBrush: TBrush; override;
    function CreateVertLineBrush: TBrush; override;
    function GetDividerWidth: Integer; override;
    function GetIndentWidth: Integer; override;
  public
    procedure AddBoundHeaderLines(ARowHeaderInfo: TcxCustomRowHeaderInfo); override;
    procedure AddHeaderIndentLines(ARowHeaderInfo: TcxCustomRowHeaderInfo;
      const R: TRect; const AViewParams: TcxViewParams;
      AToCategories, AUnderline, AAddVertLine: Boolean); override;
    procedure AddDivider(ALinesInfo: TLineInfoList;
      const R: TRect; AColor: TColor; AIsSeparator: Boolean); override;
    procedure CalcPaintViewParamsLines(ARowViewInfo: TcxCustomRowViewInfo;
      ANextRow: TcxCustomRow); override;
    function ChangeFocusedRow(ANewFocus, AOldFocus: TcxCustomRow): TRect; override;
    function GetCategoryColor: TColor; override;
    function GetCategoryFocusRect(ARowHeaderInfo: TcxCustomRowHeaderInfo): TRect; override;
    function GetCategoryTextColor: TColor; override;
    function GetContentColor(AFocused: Boolean): TColor; override;
    function GetContentTextColor: TColor; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderTextColor: TColor; override;
    function GetIndentViewParams(ARow, AParentIndentRow: TcxCustomRow): TcxViewParams; override;
    function IsDrawValueFocusRect: Boolean; override;
  end;

  { TcxvgStyle3DCalcHelper }

	TcxvgSkinCalcHelper = class(TcxvgDotNetStyleCalcHelper)
  protected
    function CreateHorzLineBrush: TBrush; override;
    function CreateVertLineBrush: TBrush; override;
    function GetBandBorderColor: TColor; override;
  public
    procedure AddBoundHeaderLines(ARowHeaderInfo: TcxCustomRowHeaderInfo); override;
    procedure AddHeaderIndentLines(ARowHeaderInfo: TcxCustomRowHeaderInfo;
      const R: TRect; const AViewParams: TcxViewParams;
      AToCategories, AUnderline, AAddVertLine: Boolean); override;
    function GetBackgroundColor: TColor; override;
    function GetCategoryColor: TColor; override;
    function GetCategoryTextColor: TColor; override;
    function GetContentColor(AFocused: Boolean): TColor; override;
    function GetContentEvenColor(AFocused: Boolean): TColor; override;
    function GetContentOddColor(AFocused: Boolean): TColor; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderTextColor: TColor; override;
    function IsBottomLineNeeded(ANextRow: TcxCustomRow): Boolean; override;
  end;

  { TcxvgPainter }

  TcxvgPainter = class(TcxExtEditingControlPainter)
  private
    function GetViewInfo: TcxvgCustomViewInfo;
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure DoPaint; override;
    procedure DrawRows;
    procedure DrawStyleFeatures; virtual;
    function GetExpandButtonState(ARowHeader: TcxCustomRowHeaderInfo): TcxExpandButtonState; virtual;
    function IsExplorerStyleCategory(out ATheme: TdxTheme): Boolean;
    function IsNeedPaintValue(AValueInfo: TcxRowValueInfo): Boolean;
  public
    procedure DrawBackground; virtual;
    procedure DrawButton(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawCategoryExplorerStyleRowHeader(ARowHeader: TcxCustomRowHeaderInfo; ATheme: TdxTheme); virtual;
    procedure DrawCategoryRowHeader(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawCategoryRowIndent(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawHeaderDragImage(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawHeaderSeparators(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawImage(ACaptionInfo: TcxRowCaptionInfo); virtual;
    procedure DrawLines(ALinesInfo: TLineInfoList; R: TRect); virtual;
    procedure DrawRow(ARowViewInfo: TcxCustomRowViewInfo); virtual;
    procedure DrawRowHeader(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawRowHeaderCell(ACaptionInfo: TcxRowCaptionInfo; ATransparent: Boolean); virtual;
    procedure DrawRowHeaderCellFilterButton(ACaptionInfo: TcxRowCaptionInfo); virtual;
    procedure DrawRowHeaderCells(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawRowHeaderFocusRect(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawRowHeaderSelected(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawRowIndent(ARowHeader: TcxCustomRowHeaderInfo); virtual;
    procedure DrawRowValueCell(AValueInfo: TcxRowValueInfo); virtual;
    procedure DrawRowValues(ARowViewInfo: TcxCustomRowViewInfo); virtual;
    procedure DrawValuesSeparators(ARowViewInfo: TcxCustomRowViewInfo); virtual;

    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
    property ViewInfo: TcxvgCustomViewInfo read GetViewInfo;
  end;

  { TcxStyle3DPainter }

  TcxStyle3DPainter = class(TcxvgPainter)
  protected
    function GetExpandButtonState(ARowHeader: TcxCustomRowHeaderInfo): TcxExpandButtonState; override;
    procedure DrawStyleFeatures; override;
  end;

  // STYLES
  PcxvgContentParamsData = ^TcxvgContentParamsData;
  TcxvgContentParamsData = record
    EditorRowProperties: TcxCustomEditorRowProperties;
    Focused: Boolean;
    Index: Integer;
  end;

  TcxvgOnGetItemStyleEvent = procedure(Sender: TObject; ARow: TcxCustomRow;
    var AStyle: TcxStyle) of object;
  TcxvgOnGetContentStyleEvent = procedure(Sender: TObject;
    AEditProp: TcxCustomEditorRowProperties; AFocused: Boolean;
    ARecordIndex: Integer; var AStyle: TcxStyle) of object;

  { TcxvgCustomRowStyles }

  TcxvgCustomRowStyles = class(TcxStyles)
  private
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure Changed(AIndex: Integer); override;

    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetHeaderParams(ARow: TcxCustomRow): TcxViewParams;
  published
    property Header: TcxStyle index vgrs_Header read GetValue write SetValue;
  end;

  { TcxCategoryRowStyles }

  TcxCategoryRowStyles = class(TcxvgCustomRowStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  end;

  { TcxEditorRowStyles }

  TcxEditorRowStyles = class(TcxCategoryRowStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetContentParams(AEditorRowProperties: TcxCustomEditorRowProperties;
      AFocused: Boolean; ARecordIndex: Integer): TcxViewParams;
  published
    property Content: TcxStyle index vgrs_Content read GetValue write SetValue;
    property Header;
  end;

  { TcxVerticalGridStyleSheet }

  TcxVerticalGridStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxVerticalGridStyles;
    procedure SetStylesValue(Value: TcxVerticalGridStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxVerticalGridStyles read GetStylesValue write SetStylesValue;
  end;

  { TcxVerticalGridStyles }

  TcxVerticalGridStyles = class(TcxCustomControlStyles)
  private
    FUseOddEvenStyles: TdxDefaultBoolean;
    FOnGetCategoryStyle: TcxvgOnGetItemStyleEvent;
    FOnGetHeaderStyle: TcxvgOnGetItemStyleEvent;
    FOnGetContentStyle: TcxvgOnGetContentStyleEvent;
    function GetCalcHelper: TcxvgCustomPaintStyleCalcHelper; inline;
    function GetVerticalGrid: TcxCustomVerticalGrid; inline;
    procedure SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
  protected
    function ActuallyUseOddEvenStyles: Boolean; inline;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetFindPanelDefaultColor: TColor; override;
    function GetFindPanelDefaultTextColor: TColor; override;
    function GetSelectedHeaderParams(ARow: TcxCustomRow; AFocused: Boolean): TcxViewParams; overload;

    property CalcHelper: TcxvgCustomPaintStyleCalcHelper read GetCalcHelper;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetCategoryParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function GetContentParams(AEditorRowProperties: TcxCustomEditorRowProperties;
       AFocused: Boolean; ARecordIndex: Integer): TcxViewParams; virtual;
    function GetHeaderParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function GetIncSearchParams: TcxViewParams;
    function GetSelectedHeaderParams(ARow: TcxCustomRow): TcxViewParams; overload;
  published
    property Category: TcxStyle index vgs_Category read GetValue write SetValue;
    property ContentEven: TcxStyle index vgs_ContentEven read GetValue write SetValue;
    property ContentOdd: TcxStyle index vgs_ContentOdd read GetValue write SetValue;
    property Header: TcxStyle index vgs_Header read GetValue write SetValue;
    property IncSearch: TcxStyle index vgs_IncSearch read GetValue write SetValue;
    property UseOddEvenStyles: TdxDefaultBoolean read FUseOddEvenStyles write SetUseOddEvenStyles default bDefault;
    property OnGetCategoryStyle: TcxvgOnGetItemStyleEvent read FOnGetCategoryStyle write FOnGetCategoryStyle;
    property OnGetHeaderStyle: TcxvgOnGetItemStyleEvent read FOnGetHeaderStyle write FOnGetHeaderStyle;
    property OnGetContentStyle: TcxvgOnGetContentStyleEvent read FOnGetContentStyle write FOnGetContentStyle;
    property Background;
    property Content;
    property Inactive;
    property Navigator;
    property NavigatorInfoPanel;
    property Selection;
    property StyleSheet;
  end;

  { TcxVerticalGridItemsCustomizeListBox }

  TcxVerticalGridItemsCustomizeListBox = class(TcxListBox)
  strict private
    FDragAndDropItemIndex: Integer;
    FDragging: Boolean;
    FIsCategoryListBox: Boolean;
    FMouseDownPos: TPoint;
    FOffset: TPoint;
    FVerticalGrid: TcxCustomVerticalGrid;

    function GetDragRow: TcxCustomRow;
    function GetPainter: TcxvgPainter;
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    procedure DXMRecalculate(var Message: TWMDrawItem); message DXM_RECALCULATE;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
  protected
    procedure CalcHeaderViewInfo(const R: TRect; AHeaderInfo: TcxCustomRowHeaderInfo); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean; override;
    procedure InitDragAndDropObject; virtual;
    function IsOwnerDragDrop(ADragObject: TObject): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RefreshList;
    procedure UpdateItemHeight;

    property DragRow: TcxCustomRow read GetDragRow;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
    property Painter: TcxvgPainter read GetPainter;
  public
    constructor CreateEx(AOwner: TComponent; AVerticalGrid: TcxCustomVerticalGrid; IsCategoryListBox: Boolean);
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  { TcxVerticalGridCustomizationForm }

  TcxVerticalGridCustomizationForm = class(TdxForm)
  private
    FOwner: TcxVerticalGridCustomizing;
    FHookTimer: TcxTimer;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    procedure HookTimerHandler(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;

    property Customizing: TcxVerticalGridCustomizing read FOwner;
    property HookTimer: TcxTimer read FHookTimer;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor CreateEx(AOwner: TcxVerticalGridCustomizing);
    destructor Destroy; override;
  end;

  { TcxVerticalGridCustomizing }

  TcxVerticalGridCustomizing = class(TcxOwnedInterfacedPersistent)
  private
    FCategoryListBox: TcxVerticalGridItemsCustomizeListBox;
    FForm: TcxVerticalGridCustomizationForm;
    FRowListBox: TcxVerticalGridItemsCustomizeListBox;
    FLastBandIndex: Integer;
    FLastHeaderIndex: Integer;
    FLastPosition: TPoint;
    FLineHeight: Integer;
    FPageControl: TcxPageControl;
    FPanel: TPanel;
    FButtonNew: TcxButton;
    FButtonDelete: TcxButton;
    FRowCount: Integer;
    FShowCategoryButtons: Boolean;
    FShowCategoryTab: Boolean;
    FTabSheetCategories: TcxTabSheet;
    FTabSheetRows: TcxTabSheet;
    FVerticalGrid: TcxCustomVerticalGrid;
    procedure CreateCategoryClick(Sender: TObject);
    procedure DeleteCategoryClick(Sender: TObject);
    function GetVisible: Boolean;
    procedure SetRowCount(Value: Integer);
    procedure SetShowCategoryButtons(const Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure UpdateButtons(Sender: TObject);
  protected
    procedure AdjustControls; virtual;
    function CanDrop(const P: TPoint): Boolean; virtual;
    procedure CreateCategoryPanel;
    procedure CreateCategoryRow; virtual;
    procedure CreateControls; virtual;
    procedure FinishCustomizing(const ABounds: TRect); virtual;

    procedure BiDiModeChanged;
    procedure CreateCustomizingForm;
    procedure DisplayCustomizingForm;
    procedure HideCustomizingForm;
    procedure ShowCustomizingForm;

    procedure LookAndFeelChanged; virtual;
    procedure SetControlParent(AControl, AParent: TWinControl);
    function SizeDelta: TSize; virtual;
    procedure ValidateListBox(AListBox: TcxVerticalGridItemsCustomizeListBox; AIndex: Integer);
    procedure Update; virtual;
    // IcxVerticalGridDesigner
    procedure ComponentRemoved(Sender: TObject); virtual;
    procedure Modified; virtual;
    property CategoryListBox: TcxVerticalGridItemsCustomizeListBox read FCategoryListBox;
    property CustomizingPos: TPoint read FLastPosition write FLastPosition;
    property RowListBox: TcxVerticalGridItemsCustomizeListBox read FRowListBox;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MakeCategorySheetVisible;
    procedure MakeRowSheetVisible;

    property Form: TcxVerticalGridCustomizationForm read FForm;
    property RowCount: Integer read FRowCount write SetRowCount default 10;
    property ShowCategoryButtons: Boolean read FShowCategoryButtons write SetShowCategoryButtons;
    property ShowCategoryTab: Boolean read FShowCategoryTab write FShowCategoryTab;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

function GetTextAligmentFlags(AProperties: TcxCaptionRowProperties): Integer; overload;
function GetTextAligmentFlags(AHorz: TAlignment; AVert: TcxAlignmentVert): Integer; overload;
procedure cxVerticalGridError(const Msg: string);

implementation

uses
  Math, Variants, Types, dxThemeConsts, dxTypeHelpers,
  cxVGridConsts, cxVGridViewInfo, cxVGridNewCategory,
  dxSpreadSheetStyles, cxFindPanel, dxFilterBox,
  cxFilterControlDialog, cxDataUtils, cxEditUtils, cxDrawTextUtils, cxContainer,
  cxTextEdit, cxLibraryConsts, dxDPIAwareUtils;

{$R cxInsCur.res}

const
  cxvgScrollDelta = 32;

  // Cursors
  cxInspectorInsertCursor   = 'CXVG_INSERT';
  cxInspectorAddChildCursor = 'CXVG_ADDCHILD';
  cxInspectorAddCursor      = 'CXVG_ADD';
  cxInspectorHideCursor     = 'CXVG_HIDE';
  cxInspectorNoDragCursor   = 'CXVG_NODRAG';

  cxVerticalGridStoringVersion = 1;

  cxVerticalGridHeaderCellFilterButtonIndent = 2;

type
  TControlAccess = class(TControl);
  TcxEditingControllerAccess = class(TcxEditingController);
  TcxHotTrackControllerAccess = class(TcxHotTrackController);
  TcxCustomEditViewInfoAccess = class(TcxCustomEditViewInfo);

const
  HorzAlignment: array[TAlignment] of Integer = (CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY);
  VertAlignment: array[TcxAlignmentVert] of Integer = (CXTO_TOP, CXTO_BOTTOM, CXTO_CENTER_VERTICALLY);

function GetTextAligmentFlags(AProperties: TcxCaptionRowProperties): Integer; overload;
begin
  Result := GetTextAligmentFlags(AProperties.HeaderRealAlignmentHorz, AProperties.HeaderAlignmentVert);
end;

function GetTextAligmentFlags(AHorz: TAlignment; AVert: TcxAlignmentVert): Integer; overload;
begin
  Result := HorzAlignment[AHorz] or VertAlignment[AVert];
end;

procedure cxVerticalGridError(const Msg: string);
begin
  raise EcxVerticalGridError.Create(Msg);
end;

{ TcxVerticalGridFindPanel }

function TcxVerticalGridFindPanel.GetClearButtonCaption: string;
begin
  Result := cxGetResourceString(@scxSvgFindPanelClearButtonCaption);
end;

function TcxVerticalGridFindPanel.GetDefaultInfoText: string;
begin
  Result := cxGetResourceString(@scxSvgFindPanelInfoText);
end;

function TcxVerticalGridFindPanel.GetFindButtonCaption: string;
begin
  Result := cxGetResourceString(@scxSvgFindPanelFindButtonCaption);
end;

{ TcxVerticalGridFilterValueList }

function TcxVerticalGridFilterValueList.GetDateTimeRelativeFilterDisplayText(AKind: TcxFilterOperatorKind): string;
begin
  case AKind of
    foYesterday:
      Result := cxGetResourceString(@cxSvgYesterday);
    foToday:
      Result := cxGetResourceString(@cxSvgToday);
    foTomorrow:
      Result := cxGetResourceString(@cxSvgTomorrow);
    foLast30Days:
      Result := cxGetResourceString(@cxSvgLast30Days);
    foLast14Days:
      Result := cxGetResourceString(@cxSvgLast14Days);
    foLast7Days:
      Result := cxGetResourceString(@cxSvgLast7Days);
    foNext7Days:
      Result := cxGetResourceString(@cxSvgNext7Days);
    foNext14Days:
      Result := cxGetResourceString(@cxSvgNext14Days);
    foNext30Days:
      Result := cxGetResourceString(@cxSvgNext30Days);
    foLastTwoWeeks:
      Result := cxGetResourceString(@cxSvgLastTwoWeeks);
    foLastWeek:
      Result := cxGetResourceString(@cxSvgLastWeek);
    foThisWeek:
      Result := cxGetResourceString(@cxSvgThisWeek);
    foNextWeek:
      Result := cxGetResourceString(@cxSvgNextWeek);
    foNextTwoWeeks:
      Result := cxGetResourceString(@cxSvgNextTwoWeeks);
    foLastMonth:
      Result := cxGetResourceString(@cxSvgLastMonth);
    foThisMonth:
      Result := cxGetResourceString(@cxSvgThisMonth);
    foNextMonth:
      Result := cxGetResourceString(@cxSvgNextMonth);
    foLastYear:
      Result := cxGetResourceString(@cxSvgLastYear);
    foThisYear:
      Result := cxGetResourceString(@cxSvgThisYear);
    foNextYear:
      Result := cxGetResourceString(@cxSvgNextYear);
    foInPast:
      Result := cxGetResourceString(@cxSvgPast);
    foInFuture:
      Result := cxGetResourceString(@cxSvgFuture);
    else
      Result := inherited GetDateTimeRelativeFilterDisplayText(AKind);
  end;
end;

{ TcxVerticalGridConditionalFormattingProvider }

constructor TcxVerticalGridConditionalFormattingProvider.Create(AOwner: TcxCustomVerticalGrid);
begin
  inherited Create(AOwner.DataController);
  FOwner := AOwner;
end;

procedure TcxVerticalGridConditionalFormattingProvider.DoBeginUpdate;
begin
  FOwner.BeginUpdate;
end;

procedure TcxVerticalGridConditionalFormattingProvider.DoEndUpdate;
begin
  FOwner.EndUpdate;
end;

function TcxVerticalGridConditionalFormattingProvider.IsItemVisible(AItem: TcxCustomInplaceEditContainer): Boolean;
begin
  Result := TcxCellEdit(AItem).Row.Visible;
end;

function TcxVerticalGridConditionalFormattingProvider.IsRowVisible(const ARow: Integer): Boolean;
begin
  Result := (ARow >= FOwner.LeftVisibleRecord) and
    (ARow <= FOwner.LeftVisibleRecord + FOwner.Controller.Scroller.VisibleValueCount);
end;

function TcxVerticalGridConditionalFormattingProvider.IsRightToLeft: Boolean;
begin
  Result := SysLocale.MiddleEast and (FOwner.BiDiMode <> bdLeftToRight);
end;

function TcxVerticalGridConditionalFormattingProvider.GetController: TcxCustomControlController;
begin
  Result := FOwner.Controller;
end;

function TcxVerticalGridConditionalFormattingProvider.GetItemDisplayName(AItem: TcxCustomInplaceEditContainer): string;
begin
  Result := TcxCellEdit(AItem).EditRowProperties.Caption;
end;

function TcxVerticalGridConditionalFormattingProvider.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.LookAndFeel;
end;

function TcxVerticalGridConditionalFormattingProvider.GetOwner: TComponent;
begin
  Result := FOwner;
end;

function TcxVerticalGridConditionalFormattingProvider.DoGetParentForm: TCustomForm;
begin
  Result := Forms.GetParentForm(FOwner);
end;

function TcxVerticalGridConditionalFormattingProvider.GetScaleFactor: TdxScaleFactor;
begin
  Result := FOwner.ScaleFactor;
end;

{ TcxvgController }

constructor TcxvgController.Create(
  AOwner: TcxEditingControl);
begin
  inherited Create(AOwner);
  FScroller := TcxvgScroller.Create(VerticalGrid);
  FCellIndex := -1;
end;

destructor TcxvgController.Destroy;
begin
  FScroller.Free;
  inherited Destroy;
end;

function TcxvgController.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := cxE_NOINTERFACE;
end;

function TcxvgController.GetCursor(X, Y: Integer): TCursor;
begin
  if not VerticalGrid.IsDesigning then
  begin
    if Int64(Point(X, Y)) <> Int64(HitTestController.HitPoint) then
      TcxvgHitTest(HitTestController).RecalculateOnMouseEvent(X, Y, []);
    Result := TcxvgHitTest(HitTestController).GetCurrentCursor;
    if Result = crDefault then
      Result := inherited GetCursor(X, Y);
  end
  else
    Result := inherited GetCursor(X, Y);
end;

procedure TcxvgController.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  AIsMultiEditorRow: Boolean;
  ARemoveFocus: Boolean;

  function CanProcessProcessMultiEditorRow: Boolean;
  var
    AIndex: Integer;
  begin
    Result := False;
    if not AIsMultiEditorRow then Exit;
    with TcxCustomMultiEditorRow(FocusedRow) do
    begin
      AIndex := CellIndex;
      if Key = VK_LEFT then
      begin
        if AIndex > 0 then
        repeat
          Dec(AIndex);
          with Properties.Editors[AIndex].EditContainer do
          if CanFocus and CanTabStop then
          begin
            Result := True;
            Exit;
          end;
        until AIndex = 0;
      end
      else
        if AIndex < Properties.Editors.Count - 1 then
        repeat
          Inc(AIndex);
          with Properties.Editors[AIndex].EditContainer do
          if CanFocus and CanTabStop then
          begin
            Result := True;
            Exit;
          end;
        until AIndex = Properties.Editors.Count - 1;
    end;
  end;

  function CanChangeExpandedState: Boolean;
  begin
    Result := VerticalGrid.OptionsView.ShowButtons and (FocusedRow <> nil) and
      FocusedRow.Options.ShowExpandButton and (FocusedRow.Count > 0) and not IsEditing;
  end;

begin
  if FindPanel.IsFocused then
  begin
    FindPanel.KeyDown(Key, Shift);
    Exit;
  end;
  FForceShowEditor := (Key = VK_RETURN) and GetGoToNextCellOnEnter;
  AIsMultiEditorRow := (FocusedRow is TcxCustomMultiEditorRow) and
    (TcxCustomMultiEditorRow(FocusedRow).Properties.Editors.Count > 0);
  FProcessMultiEditorRow := AIsMultiEditorRow and
    (ForceShowEditor or ((Key = VK_TAB) and GetGoToNextCellOnTab));
  if ((Key = VK_LEFT) or (Key = VK_RIGHT)) and not CanProcessProcessMultiEditorRow then
  begin
    if not CanChangeRecord then
    begin
      if CanChangeExpandedState then
        FocusedRow.Expanded := Key = VK_RIGHT;
      EatKeyPress := True;
      Key := 0;
    end;
  end;

  if not BlockRecordKeyboardHandling then
  begin
    if IsIncSearching then Key := IncSearchKeyDown(Key, Shift);
     Navigator.KeyDown(Key, Shift);
  end;
  case Key of
    VK_ESCAPE:
      VerticalGrid.DataController.Cancel;
    VK_RETURN, VK_TAB:
      if ((Shift = []) or (Shift = [ssShift])) and (
        (Key = VK_RETURN) and GetGoToNextCellOnEnter or
        (Key = VK_TAB) and (GetGoToNextCellOnTab or IsFindPanelVisible)) then
      begin
        ARemoveFocus := False;
        if (Key = VK_TAB) and IsFindPanelVisible and not GetGoToNextCellOnTab then
          ARemoveFocus := not VerticalGrid.IsEditing
        else
          if Shift + [ssShift] = [ssShift] then
            if Navigator.FocusNextCell(Shift = [], True, False) then
              Key := 0
            else
              ARemoveFocus := not VerticalGrid.IsEditing
          else
            ARemoveFocus := (Key = VK_TAB) and (Shift + [ssShift, ssCtrl] = [ssShift, ssCtrl]);
        if ARemoveFocus and VerticalGrid.IsFocused then
          PostMessage(GetParentForm(VerticalGrid).Handle, WM_NEXTDLGCTL, WPARAM(ssShift in Shift), LPARAM(False));
      end;
    VK_PRIOR, VK_NEXT:
      DoNextPage(Key = VK_NEXT, Shift);
    VK_MULTIPLY:
      if CanChangeExpandedState then
      begin
        FocusedRow.Expand(True);
        EatKeyPress := True;
        Key := 0;
      end;
    VK_ADD, VK_SUBTRACT:
      if CanChangeExpandedState then
      begin
        FocusedRow.Expanded := Key = VK_ADD;
        EatKeyPress := True;
        Key := 0;
      end;
    VK_INSERT:
      if (Shift = []) and CanInsert(True) then
      begin
        InsertRecord;
        Key := 0;
      end;
    VK_DELETE:
      if CanHandleDeleteRecordKeys and ((Shift = []) or (Shift = [ssCtrl])) and CanDelete(True) then
      begin
        DeleteSelection;
        Key := 0;
      end;
    Ord('F'):
      if Shift = [ssCtrl] then
      begin
        ShowFindPanel;
        Key := 0;
      end;
  end;
end;

procedure TcxvgController.MakeFocusedItemVisible;
begin
  if IsLocked or (FFocusedRow = nil) then Exit;
  MakeRecordVisible(FocusedRecordIndex);
  MakeRowVisible(FFocusedRow)
end;

procedure TcxvgController.MakeFocusedRecordVisible;
begin
  MakeRecordVisible(FocusedRecordIndex);
end;

procedure TcxvgController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function IsChangeExpandState(ARow: TcxCustomRow): Boolean;
  begin
    with HitTest do
      Result := ARow.Options.ShowExpandButton and
        (HitAtButton or (IsDblClick and HitAtCaption and ARow.HasVisibleChildren));
  end;

  procedure CheckDesignSelection(ARow: TcxCustomRow);
  begin
    if VerticalGrid.IsDesigning then
    begin
      with VerticalGrid.Controller do
        if DesignSelectionHelper <> nil then
          if ARow <> nil then
          DesignSelectionHelper.Select(ARow, Shift)
        else
          DesignSelectionHelper.Select(VerticalGrid, Shift);
    end;
  end;

  function CanChangeFocusedItem(ARow: TcxCustomRow): Boolean;
  begin
    with HitTest do
      Result := (ARow <> nil) and ARow.CanFocus and
        (not IsItemEditCell or not TcxRowValueInfo(EditCellViewInfo).EditContainer.CanFocus);
  end;

  procedure CheckFocusedRow;
  var
    ARow: TcxCustomRow;
  begin
    ARow := FocusedRow;
    while (ARow <> nil) and not VerticalGrid.IsRowVisible(ARow) do
      ARow := ARow.Parent;
    if ARow <> FocusedRow then
      if (ARow <> nil) and ARow.CanFocus then
        FocusedRow := ARow
      else
        FocusedRow := nil;
  end;

var
  ARow: TcxCustomRow;
  ACellIndex: Integer;
  AChangeExpand: Boolean;
begin
  ARow := HitTest.HitRow;
  CheckDesignSelection(ARow);
  if HitTest.HitAtCaptionFilterButton then
  begin
    TcxRowCaptionInfo(HitTest.HitTestItem).UpdateFilterButtonStateOnMouseDown;
    Exit;
  end;
  AChangeExpand := (ARow <> nil) and IsChangeExpandState(ARow);
  if (ResizeKind = rkNone) and CanChangeFocusedItem(ARow) then
  begin
    if HitTest.HitCellIndex >= 0 then
      ACellIndex := HitTest.HitCellIndex
    else
      ACellIndex := cxSetValue(FocusedRow <> ARow, 0, CellIndex);
    SetFocusedRowAndCell(ARow, ACellIndex);
    AChangeExpand := AChangeExpand and (VerticalGrid.Rows.IndexOf(ARow) >= 0);
  end;
  if AChangeExpand then
  begin
    ARow.Expanded := not ARow.Expanded;
    if ARow.Expanded then
      Scroller.SetRowMaxVisible(ARow)
    else
      CheckFocusedRow;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxvgController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  CheckRowTracking(Shift, X, Y);
end;

function TcxvgController.CanDrag(X, Y: Integer): Boolean;
begin
  with HitTest do
  begin
    HitPoint := Point(X, Y);
    if HitAtValue and (dceModified in DataController.EditState) then
      Result := False
    else
      Result := not FWasScroll and CanMoving;
  end;
end;

procedure TcxvgController.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  ResizeKind := rkNone;
end;

function TcxvgController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  with HitTest do
  begin
    HitPoint := P;
    Result := CanSizing;
  end;
end;

procedure TcxvgController.InitScrollBarsParameters;
begin
  if VerticalGrid.HandleAllocated then
    Scroller.InitScrollBarsParameters;
end;

procedure TcxvgController.Scroll(
  AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);
begin
  Scroller.Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  VerticalGrid.Update;
end;

procedure TcxvgController.RestoreLayout;
begin
  if not VerticalGrid.SubClassesCreated then
    Exit;
  Scroller.RestoreLayout;
  ViewInfo.FLockDividerPos := False;
  VerticalGrid.OptionsView.FRowHeaderWidth := SaveSeparatorPos;
  FFocusedRow := FSaveFocusedRow;
  ViewInfo.IsDirty := True;
  MakeFocusedRecordVisible;
end;

procedure TcxvgController.SaveLayout;
begin
  if not VerticalGrid.SubClassesCreated then
    Exit;
  FSaveSeparatorPos := VerticalGrid.OptionsView.RowHeaderWidth;
  FSaveFocusedRow := FocusedRow;
  Scroller.SaveLayout;
end;

procedure TcxvgController.AppendRecord;
begin
  if not CanAppend(False) then Exit;
  if VerticalGrid.OptionsBehavior.FocusFirstCellOnNewRecord then
    FocusedRow := ViewInfo.VisibleRows[0];
  DataController.Append;
end;

procedure TcxvgController.DeleteSelection;
begin
end;

procedure TcxvgController.InsertRecord;
begin
  if not CanInsert(False) then Exit;
  if VerticalGrid.OptionsBehavior.FocusFirstCellOnNewRecord then
    FocusedRow := ViewInfo.VisibleRows[0];
  DataController.Insert;
end;

procedure TcxvgController.MakeRowVisible(ARow: TcxCustomRow);
var
  AParent: TcxCustomRow;
begin
  if ARow = nil then Exit;
  if VerticalGrid.IsRowVisible(ARow) then
    Scroller.SetRowVisible(ARow)
  else
    with VerticalGrid do
    begin
      AParent := ARow.FParent;
      BeginUpdate;
      try
        while AParent <> FRootRow do
        begin
          AParent.Visible := True;
          AParent.Expanded := True;
          AParent := AParent.FParent;
        end;
        Scroller.SetRowVisible(ARow);
      finally
        EndUpdate;
      end;
    end;
end;

function TcxvgController.MakeRecordVisible(AIndex: Integer): Boolean;
begin
  Result := Scroller.SetRecordVisible(AIndex);
end;

function TcxvgController._AddRef: Integer;
begin
  Result := -1;
end;

function TcxvgController._Release: Integer;
begin
  Result := -1;
end;

function TcxvgController.CanSizing(
  ADirection: TcxDragSizingDirection): Boolean;
begin
  HitTest.Recalculate(GetMouseCursorPos);
  Result := HitTest.CanSizing(ADirection);
end;

function TcxvgController.GetSizingBoundsRect(
  ADirection: TcxDragSizingDirection): TRect;
begin
  Result := ViewInfo.ClipRect;
  Dec(Result.Bottom);
end;

function TcxvgController.GetSizingIncrement(
  ADirection: TcxDragSizingDirection): Integer;
begin
  Result := 1;
end;

function TcxvgController.IsDynamicUpdate: Boolean;
begin
  Result := True;
end;

procedure TcxvgController.SetSizeDelta(
  ADirection: TcxDragSizingDirection; ADelta: Integer);
const
  ASign: array[Boolean] of Integer = (-1, 1);
begin
  Inc(FSizingValue, ASign[(ResizeKind = rkRowSizing) or not ViewInfo.IsRightToLeftConverted] * ADelta);
  case ResizeKind of
    rkRowSizing:
      FSizingRow.Height := Max(FSizingValue, ViewInfo.DefaultEditHeight);
    rkDivider:
      ViewInfo.SetDividerPos(FSizingValue);
    rkBandSizing:
      ViewInfo.SetValueWidth(FSizingValue);
  end;
  VerticalGrid.Modified;
end;

procedure TcxvgController.DragDrop(Source: TObject; X, Y: Integer);
begin
  if IsInternalDragging(Source) then
  begin
    with HitTest do
      if (HitRow <> nil) or HitAtEmpty then
      begin
        VerticalGrid.BeginUpdate;
        try
          if HitRow = nil then
          begin
            DragRow.Parent := VerticalGrid.FRootRow;
            DragRow.Index := VerticalGrid.FRootRow.Count - 1;
          end
          else
          begin
            if HitAtCaption then
              DragRow.Parent := HitRow
            else
              DragRow.Parent := HitRow.Parent;
            DragRow.Index := HitRow.Index;
          end;
          DragRow.Visible := True;
        finally
          VerticalGrid.EndUpdate;
        end;
      end
      else
        CheckMoveToCustomizationForm;
  end;
  FDragRow := nil;
end;

procedure TcxvgController.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if (not Assigned(VerticalGrid.OnDragOver) or Accept) and IsInternalDragging(Source) then
    with HitTest do
    begin
      HitPoint := cxPoint(X, Y);
      Accept := HitAtEmpty or
        HitAtRowHeader and (DragRow <> HitRow) and not DragRow.IsParent(HitRow);
    end;
end;

procedure TcxvgController.EndDrag(Target: TObject; X, Y: Integer);
begin
  FAutoScrollObject.Free;
  FAutoScrollObject := nil;
  FTrackingEnabled := False;
  inherited EndDrag(Target, X, Y);
end;

procedure TcxvgController.StartDrag(var DragObject: TDragObject);
begin
  inherited StartDrag(DragObject);
  FAutoScrollObject := TcxControllerAutoScrollingObject.Create(Self);
  FAutoScrollObject.SetBoundsParams(VerticalGrid.ClientBounds, False, True, 1);
  HitTest.HitPoint := VerticalGrid.ScreenToClient(GetMouseCursorPos);
  with VerticalGrid.Customizing do
  begin
    FDragFromCustomizingForm := Visible and HitTest.HitAtCustomize;
    if Visible and (DragRow <> nil) then
    begin
      if DragRow.IsCategory then
        MakeCategorySheetVisible
      else
        MakeRowSheetVisible;
    end;
  end;
end;

procedure TcxvgController.BeforeMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited BeforeMouseDown(Button, Shift, X, Y);
  FDragRow := nil;
end;

procedure TcxvgController.BehaviorChanged;
begin
  if not VerticalGrid.IsLocked then
    inherited BehaviorChanged;
end;

function TcxvgController.CanAppend(ACheckOptions: Boolean): Boolean;
begin
  Result := False;
end;

function TcxvgController.CanChangeRecord: Boolean;
begin
  Result := False;
end;

function TcxvgController.CanDelete(ACheckOptions: Boolean): Boolean;
begin
  Result := False;
end;

function TcxvgController.CanInsert(ACheckOptions: Boolean): Boolean;
begin
  Result := False;
end;

function TcxvgController.CanTrack(const AShift: TShiftState): Boolean;
begin
  Result := FTrackingEnabled and (ssLeft in AShift) and
    (VerticalGrid.DragAndDropState = ddsNone) and
    not VerticalGrid.Dragging and not VerticalGrid.IsLocked;
end;

procedure TcxvgController.CheckPostData;
begin
  if [dceEdit, dceModified] * DataController.EditState <> [] then
    DataController.Post;
end;

procedure TcxvgController.CheckRowTracking(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  if not CanTrack(Shift) then Exit;
  ARow := HitTest.HitRow;
  if ARow <> nil then
  begin
    if ARow.CanFocus then
    begin
      StopScrollTimer;
      SetFocusedRowAndCell(ARow, HitTest.HitCellIndex);
      Exit;
    end;
  end
  else
    for I := 0 to ViewInfo.RowsViewInfo.Count - 1 do
      with ViewInfo.RowsViewInfo[I] do
        if cxRectPtIn(RowRect, X, Y) or ((Y >= RowRect.Top) and
          (Y < RowRect.Bottom)) and Row.CanFocus then
        begin
          StopScrollTimer;
          SetFocusedRowAndCell(Row, HitTest.HitCellIndex);
          Exit;
        end;
  if Y <= VerticalGrid.ClientBounds.Top then
  begin
    FScrollDown := False;
    StartScrollTimer;
  end;
  if Y > VerticalGrid.ClientBounds.Bottom then
  begin
    FScrollDown := True;
    StartScrollTimer;
  end;
end;

procedure TcxvgController.Clear;
begin
  StopScrollTimer;
  FTrackingEnabled := False;
  inherited Clear;
  EditingController.EditingItem := nil;
  FocusedItem := nil;
  FFocusedRow := nil;
end;

procedure TcxvgController.ControlFocusChanged;

  function IsRowPressed: Boolean;
  begin
    Result := (VerticalGrid.ActivateType = atByMouse) and (HitTest.HitRow <> nil);
  end;

begin
  inherited ControlFocusChanged;
  if Focused and not IsRowPressed then
    VerticalGrid.ValidateFocusedRow;
end;

procedure TcxvgController.DoCancelMode;
begin
  StopScrollTimer;
  FTrackingEnabled := False;
  inherited DoCancelMode;
end;

procedure TcxvgController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ATopRow: Integer;
  P: TPoint;
begin
  if (VerticalGrid.DragAndDropState <> ddsNone) and HitTest.CanSizing then
  begin
    EditingController.HideEdit(True);
    Exit;
  end;
  ATopRow := Scroller.TopVisibleRowIndex;
  inherited DoMouseDown(Button, Shift, X, Y);
  if HitTest.HitAtNavigator then Exit;
  FWasScroll := ATopRow <> Scroller.TopVisibleRowIndex;
  with VerticalGrid, HitTest do
  begin
    P := ScreenToClient(GetMouseCursorPos);
    RecalculateOnMouseEvent(P.X, P.Y, Shift);
    FTrackingEnabled := (Button = mbLeft) and (DragMode <> dmAutomatic) and
      not FWasScroll and not HitAtEmpty and not HitAtCaptionFilterButton and
      not HitAtButton and OptionsBehavior.RowTracking and
      (GetCapture = Handle) and
      not (IsItemEditCell and EditCellViewInfo.EditViewInfo.IsHotTrack(P));
    if CanMoving and not FTrackingEnabled then
      FDragRow := HitRow;
  end;
end;

procedure TcxvgController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StopScrollTimer;
  FTrackingEnabled := False;
  HotTrackController.SetHotElement(nil, cxNullPoint);
  inherited DoMouseUp(Button, Shift, X, Y);
end;

procedure TcxvgController.DoNextPage(AForward: Boolean;
  Shift: TShiftState);
const
  Direction: array[Boolean] of TScrollCode = (scPageUp, scPageDown);
var
  AIndex: Integer;
begin
  if ViewInfo.VisibleRowCount = 0 then Exit;
  if ssCtrl in Shift then
    if AForward then
      FocusedRow := VerticalGrid.LastVisibleRow
    else
      FocusedRow := VerticalGrid.FirstVisibleRow
  else
  begin
    if FFocusedRow = nil then
      AIndex := 0
    else
      AIndex := FFocusedRow.VisibleIndex;
    if AForward then
      Inc(AIndex, Scroller.VisibleRowCount)
    else
      Dec(AIndex, Scroller.VisibleRowCount);
    cxRange(AIndex, 0, ViewInfo.VisibleRowCount - 1);
    FocusedRow := ViewInfo.VisibleRows[AIndex];
  end;
  MakeFocusedItemVisible;
end;

procedure TcxvgController.DoUpdateRowAndCell(ANewRow: TcxCustomRow;
  ANewCellIndex: Integer);
var
  APrevFocusedRow: TcxCustomRow;
  APrevCellIndex: Integer;
begin
  APrevFocusedRow := FFocusedRow;
  APrevCellIndex := FCellIndex;
  if FFocusedRow <> ANewRow then
  begin
    FFocusedRow := ANewRow;
    FCellIndex := cxSetValue(ANewRow = nil, -1, ANewCellIndex);
    ViewInfo.ChangeFocusedRow(ANewRow, APrevFocusedRow)
  end
  else
    if FCellIndex <> ANewCellIndex then
    begin
      FCellIndex := cxSetValue(FFocusedRow = nil, -1, ANewCellIndex);
      RefreshFocusedRow;
    end;
  MakeFocusedItemVisible;
  if (APrevFocusedRow <> FFocusedRow) or (APrevCellIndex <> FCellIndex) then
    VerticalGrid.DoItemChanged(APrevFocusedRow, APrevCellIndex);
  if VerticalGrid.IsLocked then Exit;
  AllowCheckEdit := True;
  if ForceShowEditor then
  begin
    FForceShowEditor := False;
    EditingController.ShowEdit;
  end
  else
    CheckEdit;
end;

procedure TcxvgController.FocusedItemChanged(
  APrevFocusedItem: TcxCustomInplaceEditContainer);
var
  ARow: TcxCustomRow;
  ACellIndex: Integer;
begin
  if EditingControl.ComponentState * [csLoading, csDestroying] <> [] then Exit;
  if not FLockIncSearch then
    CancelIncSearching;
  if (APrevFocusedItem <> nil) and GetPostDataOnChangeItem then
    CheckPostData;
  AllowCheckEdit := False;
  if IsLocked then Exit;
  if FocusedItem = nil then
  begin
    if (FFocusedRow = nil) or (csDestroying in FFocusedRow.ComponentState) then
      ARow := nil
    else
      ARow := FFocusedRow;
    if APrevFocusedItem <> nil then
      ACellIndex := -1
    else
      ACellIndex := FCellIndex;
  end
  else
    with TcxCellEdit(FocusedItem) do
    begin
      ARow := Row;
      ACellIndex := CellIndex;
    end;
  DoUpdateRowAndCell(ARow, ACellIndex);
end;

procedure TcxvgController.FocusRow(ARow: TcxCustomRow; AMakeVisible: Boolean);
begin
  if not AMakeVisible then
  begin
    Inc(FLockUpdate);
    try
      FocusedRow := ARow;
    finally
      Dec(FLockUpdate);
    end;
  end
  else
    FocusedRow := ARow;
end;

function TcxvgController.GetCancelEditingOnExit: Boolean;
begin
  Result := [dceEdit, dceModified] * DataController.EditState = [];
end;

function TcxvgController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  FResizeKind := rkNone;
  if HitTest.CanSizing(ResizeDirection) then
  begin
    if ResizeDirection = dsdHorz then
    begin
      if HitTest.HitAtDivider then
        ResizeKind := rkDivider
      else
        ResizeKind := rkBandSizing;
    end
    else
      ResizeKind := rkRowSizing;
    Result := TcxSizingDragAndDropObject;
    DragItem := Self;
  end
  else
    Result := nil
end;

function TcxvgController.GetFindPanelClass: TcxControlFindPanelClass;
begin
  Result := TcxVerticalGridFindPanel;
end;

function TcxvgController.GetFindStartPosition(ARecordIndex: TdxNativeInt; AItemIndex: Integer;
  out AHighlightedText: string): Integer;
begin
  ARecordIndex := DataController.DataControllerInfo.GetInternalRecordIndex(ARecordIndex);
  Result := inherited GetFindStartPosition(ARecordIndex, AItemIndex, AHighlightedText);
end;

function TcxvgController.GetFocusedCellViewInfo(
  AEditContainer: TcxCustomInplaceEditContainer): TcxEditCellViewInfo;
var
  ARowViewInfo: TcxCustomRowViewInfo;
begin
  Result := nil;
  if AEditContainer = nil then Exit;
  ARowViewInfo := TcxCellEdit(AEditContainer).ViewInfo;
  if (ARowViewInfo <> nil) and (FCellIndex >= 0) then
    Result := ARowViewInfo.RowValueInfo[FocusedRecordIndex, FCellIndex];
end;

function TcxvgController.GetImmediateEditor: Boolean;
begin
  // ignore VerticalGrid.DragMode
  with VerticalGrid.OptionsBehavior do
    Result := ImmediateEditor or AlwaysShowEditor;
end;

function TcxvgController.GetNavigatorClass: TcxCustomCellNavigatorClass;
begin
  Result := TcxvgCellNavigator;
end;

function TcxvgController.GetPostDataOnChangeItem: Boolean;
begin
  Result := True;
end;

function TcxvgController.GetResizeDirection: TcxDragSizingDirection;
begin
  if ResizeKind = rkRowSizing then Result := dsdVert else Result := dsdHorz;
end;

procedure TcxvgController.InternalSetRowAndCell(ARow: TcxCustomRow;
  ACellIndex: Integer);
begin
  FFocusedRow := ARow;
  FCellIndex := ACellIndex;
end;

function TcxvgController.IsImmediatePost: Boolean;
begin
  Result := True;
end;

function TcxvgController.IsInternalDragging(ADragObject: TObject): Boolean;
begin
  Result := (VerticalGrid.DragMode = dmAutomatic) and
    (ADragObject is TcxvgDragRowObject) and
    (TcxvgDragRowObject(ADragObject).Row = FDragRow);
end;

function TcxvgController.IsKeyForController(AKey: Word;
  AShift: TShiftState): Boolean;
begin
  Result := inherited IsKeyForController(AKey, AShift) or
    (IsEditing and (ssCtrl in AShift) and (AKey in [VK_MULTIPLY, VK_ADD, VK_SUBTRACT]));
end;

function TcxvgController.IsLocked: Boolean;
begin
  Result := FLockUpdate > 0;
end;

procedure TcxvgController.RefreshFocusedRow;
begin
  if not IsLocked and (FFocusedRow <> nil) then
    FFocusedRow.Refresh;
end;

procedure TcxvgController.SetFocusedCellEdit(ACellEdit: TcxCellEdit);
var
  I: Integer;
  ARow: TcxCustomRow;
  ACellViewInfo: TcxEditCellViewInfo;
begin
  if ACellEdit = nil then
    FocusedRow := nil
  else
  begin
    ARow := ACellEdit.Row;
    for I := 0 to ARow.GetEditContainerCount - 1 do
      if ARow.GetEditContainer(I) = ACellEdit then
      begin
        SetFocusedRowAndCell(ARow, I);
        ACellViewInfo := GetFocusedCellViewInfo(ACellEdit);
        if Assigned(ACellViewInfo) then
          ACellViewInfo.Invalidate(True);
        break;
      end;
  end;
end;

procedure TcxvgController.SetFocusedRowAndCell(
  Value: TcxCustomRow; ACellIndex: Integer);

  function CanCellFocus(AIndex: Integer): Boolean;
  begin
    Result := Value.GetEditContainer(AIndex).CanFocus;
  end;

  procedure CorrectCellIndex;
  var
    I, ACount: Integer;
  begin
    if Value = nil then
      ACount := 0
    else
      ACount := Value.GetEditContainerCount;
    if (ACellIndex >= ACount) or (ACellIndex < 0) then
      ACellIndex := 0;
    if (ACount > 0) and not CanCellFocus(ACellIndex) then
    begin
      ACellIndex := 0;
      for I := 0 to ACount - 1 do
        if CanCellFocus(I) then
        begin
          ACellIndex := I;
          break;
        end;
    end;
  end;

var
  AEditContainer: TcxCustomInplaceEditContainer;
begin
  if VerticalGrid.IsDesigning or Assigned(Value) and not Value.CanFocus then Exit;
  CorrectCellIndex;
  if (FCellIndex = ACellIndex) and (FFocusedRow = Value) then Exit;
  Inc(FLockUpdate);
  try
    if Value <> nil then
      AEditContainer := Value.GetEditContainer(ACellIndex)
    else
    begin
      AEditContainer := nil;
      Scroller.TopVisibleRowIndex := Scroller.TopVisibleRowIndex;
    end;
    if FocusedItem <> AEditContainer then
      FocusedItem := AEditContainer as TcxCellEdit;
  finally
    Dec(FLockUpdate);
    AllowCheckEdit := True;
  end;
  DoUpdateRowAndCell(Value, ACellIndex);
end;

procedure TcxvgController.UpdatePaintStyle;
begin
  ViewInfo.LayoutStyleChanged;
end;

procedure TcxvgController.UpdateRecord(ARecordIndex: TdxNativeInt);
begin
  inherited UpdateRecord(ARecordIndex);
  if not VerticalGrid.IsLocked then
    ViewInfo.UpdateRecord(ARecordIndex);
end;

procedure TcxvgController.CheckMoveToCustomizationForm;

  procedure HideWithChildren(ARow: TcxCustomRow);
  begin
    if not ARow.Options.CanMovedToCustomizationForm then
    begin
      if ARow.Parent <> nil then
        ARow.Parent := ARow.Parent.Parent
      else
        ARow.Parent := nil;
    end;
    while ARow.Count > 0 do
      HideWithChildren(ARow.Rows[0]);
    if ARow.Options.CanMovedToCustomizationForm then
    begin
      ARow.Visible := False;
      ARow.Parent := nil;
    end;
  end;

begin
  if not DragFromCustomizingForm and HitTest.HitAtCustomize and
    DragRow.Options.CanMovedToCustomizationForm then
  begin
    with VerticalGrid do
    begin
      BeginUpdate;
      try
        HideWithChildren(FDragRow);
        FocusedRow := nil;
      finally
        EndUpdate;
      end;
      Customizing.Modified;
    end;
  end;
end;

function TcxvgController.GetFocusedItem: TcxCellEdit;
begin
  Result := TcxCellEdit(inherited FocusedItem);
end;

function TcxvgController.GetHitTest: TcxvgHitTest;
begin
  Result := TcxvgHitTest(inherited HitTestController);
end;

function TcxvgController.GetNavigator: TcxvgCellNavigator;
begin
  Result := TcxvgCellNavigator(inherited Navigator);
end;

function TcxvgController.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(EditingControl);
end;

function TcxvgController.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := VerticalGrid.ViewInfo;
end;

procedure TcxvgController.SetFocusedItemInternal(Value: TcxCellEdit);
begin
  inherited FocusedItem := Value;
end;

procedure TcxvgController.SetFocusedRow(Value: TcxCustomRow);
begin
  SetFocusedRowAndCell(Value, 0);
end;

procedure TcxvgController.SetResizeKind(
  Value: TcxvgResizeKind);
begin
  FResizeKind := Value;
  case Value of
    rkDivider:
      FSizingValue := VerticalGrid.OptionsView.RowHeaderWidth;
    rkBandSizing:
      FSizingValue := VerticalGrid.OptionsView.ValueWidth;
    rkRowSizing:
      begin
        FSizingRow := HitTest.HitRow;
        FSizingValue := FSizingRow.ViewInfo.CalculatedHeight;
      end
  end;
end;

procedure TcxvgController.OnScrollTimer(Sender: TObject);
var
  ARow: TcxCustomRow;
begin
  if FScrollDown then
    ARow := VerticalGrid.NextVisibleRow(FocusedRow)
  else
    ARow := VerticalGrid.PrevVisibleRow(FocusedRow);
  if ARow <> nil then FocusedRow := ARow;
end;

procedure TcxvgController.StartScrollTimer;
begin
  if FScrollTimer <> nil then Exit;
  FScrollTimer := TTimer.Create(nil);
  FScrollTimer.Interval := 5;
  FScrollTimer.OnTimer := OnScrollTimer;
end;

procedure TcxvgController.StopScrollTimer;
begin
  FreeAndNil(FScrollTimer);
end;

{ TcxvgMultiRecordsController }

procedure TcxvgMultiRecordsController.DeleteSelection;
begin
  if not CanDelete(False) then Exit;
  if not OptionsData.DeletingConfirmation or
    cxConfirmMessageBox(
      PChar(cxGetResourceString(@cxSvgDeletingFocusedConfirmationText)),
      PChar(cxGetResourceString(@cxSvgDeletingConfirmationCaption))) then
  begin
    if IsEditing then
      EditingController.HideEdit(False);
    DataController.DeleteFocused;
  end;
end;

function TcxvgMultiRecordsController.CanAppend(ACheckOptions: Boolean): Boolean;
begin
  Result := (dceoAppend in DataController.EditOperations) and
    (not ACheckOptions or OptionsData.Appending);
end;

function TcxvgMultiRecordsController.CanChangeRecord: Boolean;
begin
  Result := OptionsBehavior.AllowChangeRecord or
    (VerticalGrid.LayoutStyle = lsMultiRecordView);
end;

function TcxvgMultiRecordsController.CanDelete(ACheckOptions: Boolean): Boolean;
begin
  Result := (dceoDelete in DataController.EditOperations) and
    (not ACheckOptions or OptionsData.Deleting) and (DataController.RowCount > 0);
end;

function TcxvgMultiRecordsController.CanHandleDeleteRecordKeys: Boolean;
begin
  Result := OptionsData.Deleting;
end;

function TcxvgMultiRecordsController.CanInsert(ACheckOptions: Boolean): Boolean;
begin
  Result := (dceoInsert in DataController.EditOperations) and
    (not ACheckOptions or OptionsData.Inserting);
end;

procedure TcxvgMultiRecordsController.FocusedRecordChanged(APrevFocusedRowIndex,
  AFocusedRowIndex: Integer);
begin
  if APrevFocusedRowIndex <> -1 then
    EditingController.HideEdit(True);
  if not MakeRecordVisible(AFocusedRowIndex) then
  begin
    if VerticalGrid.OptionsView.CellAutoHeight then
    begin
      ViewInfo.Calculate;
      VerticalGrid.Invalidate;
    end
    else
    begin
      ViewInfo.UpdateRecord(APrevFocusedRowIndex);
      ViewInfo.UpdateRecord(AFocusedRowIndex);
    end;
  end;
  VerticalGrid.UpdateScrollBars;
  CheckEdit;
  TcxEditingControllerAccess(EditingController).UpdateEditValue;
  VerticalGrid.DoFocusedRecordChanged(APrevFocusedRowIndex, AFocusedRowIndex);
  VerticalGrid.Update;
end;

function TcxvgMultiRecordsController.GetCancelEditingOnExit: Boolean;
begin
  with VerticalGrid do
    Result := OptionsData.CancelOnExit and
     (DataController.EditState * [dceInsert, dceChanging, dceModified] = [dceInsert]);
end;

function TcxvgMultiRecordsController.GetFocusedRecordIndex: TdxNativeInt;
begin
  Result := DataController.GetFocusedRowIndex;
end;

function TcxvgMultiRecordsController.GetPostDataOnChangeItem: Boolean;
begin
  Result := False;
end;

function TcxvgMultiRecordsController.IncSearchKeyDown(AKey: Word;
  AShift: TShiftState): Word;
begin
  if ItemForIncSearching = nil then
  begin
    Result := AKey;
    Exit;
  end
  else
    Result := 0;
  case AKey of
    VK_LEFT, VK_RIGHT:
      if AShift = [ssCtrl] then
        SearchLocateNext(ItemForIncSearching, AKey = VK_RIGHT)
      else
      begin
        CancelIncSearching;
        Result := AKey;
      end;
    VK_UP, VK_DOWN:
      Result := AKey;
  else
    Result := inherited IncSearchKeyDown(AKey, AShift);
  end;
end;

function TcxvgMultiRecordsController.IsImmediatePost: Boolean;
begin
  Result := False;
end;

procedure TcxvgMultiRecordsController.RefreshIncSearchItem;
var
  ACellEdit: TcxCellEdit;
begin
  if IsIncSearching then
  begin
    ACellEdit := VerticalGrid.ContainerList.List[DataController.Search.ItemIndex];
    FLockIncSearch := True;
    try
      SetFocusedCellEdit(ACellEdit);
    finally
      FLockIncSearch := False;
    end;
  end;
end;

procedure TcxvgMultiRecordsController.SetFocusedRecordIndex(Value: TdxNativeInt);
var
  AIndexesAreEqual: Boolean;
begin
  with DataController do
  begin
    if cxInRange(Value, 0, RowCount - 1) and
      not (CanFocusedRecordIndex(Value) and ChangeFocusedRowIndex(Value)) then Exit;
  end;
  AIndexesAreEqual := FocusedRecordIndex = Value;
  if AIndexesAreEqual then MakeFocusedRecordVisible;
end;

function TcxvgMultiRecordsController.GetOptionsBehavior: TcxvgMultiRecordsOptionsBehavior;
begin
  Result := VerticalGrid.OptionsBehavior;
end;

function TcxvgMultiRecordsController.GetOptionsData: TcxvgMultiRecordsOptionsData;
begin
  Result := VerticalGrid.OptionsData;
end;

function TcxvgMultiRecordsController.GetVerticalGrid: TcxVirtualVerticalGrid;
begin
  Result := inherited VerticalGrid as TcxVirtualVerticalGrid;
end;

{ TcxRowValueInfo }

function TcxRowValueInfo.GetHeight(AContentWidth: Integer): Integer;
begin
  CellContentRect := cxRect(0, 0, AContentWidth, 0);
  Result := CalculateEditHeight;
end;

procedure TcxRowValueInfo.PaintEx(ACanvas: TcxCanvas);
begin
  ViewInfo.PaintEx(ACanvas);
end;

procedure TcxRowValueInfo.AfterDrawCellBackground(ACanvas: TcxCanvas);
begin
  inherited AfterDrawCellBackground(ACanvas);

  ConditionalFormattingProvider.AfterDrawCellBackground(Self, ViewParams.Color, ACanvas);
end;

procedure TcxRowValueInfo.AfterDrawCellValue(ACanvas: TcxCanvas);
begin
  inherited AfterDrawCellValue(ACanvas);
  ConditionalFormattingProvider.AfterDrawCellValue(Self, ACanvas);
end;

procedure TcxRowValueInfo.CalculateCellEditorBounds(AViewInfo: TcxCustomEditViewInfo; var R: TRect);
begin
  ConditionalFormattingProvider.CalculateCellEditorBounds(Self, R);
end;

procedure TcxRowValueInfo.CanDrawCellValue(var Allow: Boolean);
begin
  inherited CanDrawCellValue(Allow);
  if Allow then
    ConditionalFormattingProvider.CanDrawCellValue(Self, Allow);
end;

procedure TcxRowValueInfo.DoCalculate;
begin
  EditContainer.Calculate(Self);
  if Focused then
    FFocusRect := DisplayRect
  else
    FFocusRect := cxNullRect;
end;

procedure TcxRowValueInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FFocusRect := TdxRightToLeftLayoutConverter.ConvertRect(FFocusRect, AClientBounds);
end;

function TcxRowValueInfo.EditContainer: TcxCellEdit;
begin
  Result := TcxCellEdit(inherited EditContainer);
end;

function TcxRowValueInfo.GetButtonTransparency: TcxEditButtonTransparency;

   function NeedButton: Boolean;
   begin
     Result := Focused;
     if not Result and (Control is TcxVirtualVerticalGrid) then
       with TcxVirtualVerticalGrid(Control) do
         Result := (LayoutStyle = lsMultiRecordView) and
           (RecordIndex = DataController.FocusedRowIndex);
   end;

var
  B1: TcxEditingControlEditShowButtons;
  B2: TcxEditItemShowEditButtons;
begin
  B1 := TcxCustomVerticalGrid(Control).OptionsView.ShowEditButtons;
  B2 := EditorRowProperties.Options.ShowEditButtons;
  if (B2 = eisbAlways) or (B2 = eisbDefault) and
   ((B1 = ecsbAlways) or (B1 = ecsbFocused) and NeedButton) then
    Result := ebtNone
  else
    Result := ebtHideInactive;
end;

function TcxRowValueInfo.GetDisplayValue: Variant;
begin
  if IsRecordAvailable(FRecordIndex) then
    Result := EditContainer.GetDisplayValue(Properties, FRecordIndex)
  else
    Result := Null;
end;

function TcxRowValueInfo.GetEditViewParams: TcxViewParams;
begin
  Result := TcxEditorRowStyles(Row.Styles).GetContentParams(EditorRowProperties,
    Focused, FRecordIndex);
end;

function TcxRowValueInfo.GetFocused: Boolean;
begin
  with EditContainer do
    Result := (FocusedCellViewInfo = Self) and
      (DataController.FocusedRowIndex = FRecordIndex) and
      (FRow = FRow.VerticalGrid.FocusedRow);
end;

function TcxRowValueInfo.GetRecordIndex: TdxNativeInt;
begin
  Result := FRecordIndex;
end;

function TcxRowValueInfo.GetSelectedTextColor: Integer;
begin
  Result := IncSearchParams.TextColor;
end;

function TcxRowValueInfo.GetSelectedBKColor: Integer;
begin
  Result := IncSearchParams.Color
end;

function TcxRowValueInfo.FormatDisplayValue(AValue: Variant): Variant;
begin
  Result := inherited FormatDisplayValue(AValue);
  Result := ConditionalFormattingProvider.FormatDisplayValue(Self, Result);
end;

function TcxRowValueInfo.IncSearchParams: TcxViewParams;
begin
  Result := TcxCustomVerticalGrid(Control).Styles.GetIncSearchParams;
  if Result.Color = clDefault then
    Result.Color := inherited GetSelectedBKColor;
  if Result.TextColor = clDefault then
    Result.TextColor := inherited GetSelectedTextColor;
end;

function TcxRowValueInfo.IsAutoHeight: Boolean;
begin
  Result := TcxvgCustomViewInfo(ControlViewInfo).GetRowAutoHeight(Row);
end;

function TcxRowValueInfo.IsRecordAvailable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < EditContainer.DataController.RowCount) and
    not EditContainer.DataController.IsDataLoading;
end;

function TcxRowValueInfo.NeedHighlightFindText: Boolean;
begin
  Result := inherited NeedHighlightFindText and (RecordIndex >= 0);
end;

function TcxRowValueInfo.GetConditionalFormattingProvider: TcxVerticalGridConditionalFormattingProvider;
begin
  Result := Row.VerticalGrid.ConditionalFormattingProvider;
end;

function TcxRowValueInfo.GetEditorRowProperties: TcxCustomEditorRowProperties;
begin
  Result := EditContainer.FEditRowProperties;
end;

{ TcxRowCaptionInfo }

constructor TcxRowCaptionInfo.Create(AHeaderInfo: TcxCustomRowHeaderInfo; ARowProperties: TcxCaptionRowProperties);
begin
  inherited Create;
  FState := cxbsNormal;
  FFilterButtonState := cxbsNormal;
  FHeaderInfo := AHeaderInfo;
  FRowProperties := ARowProperties;
  CheckFilterPopupOwner;
end;

destructor TcxRowCaptionInfo.Destroy;
begin
  if IsFilterPopupOwner then
    FilterPopup.Owner := nil;
  if not VerticalGrid.IsDestroying then
    VerticalGrid.Controller.HitTestController.CheckDestroyingItem(Self);
  inherited Destroy;
end;

function TcxRowCaptionInfo.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := cxE_NOINTERFACE;
end;

function TcxRowCaptionInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TcxRowCaptionInfo._Release: Integer;
begin
  Result := -1;
end;

function TcxRowCaptionInfo.GetHintBounds: TRect;
begin
  Result := CaptionRect;
end;

function TcxRowCaptionInfo.IsNeedHint(ACanvas: TcxCanvas; const P: TPoint;
  out AText: TCaption;
  out AIsMultiLine: Boolean;
  out ATextRect: TRect; var IsNeedOffsetHint: Boolean): Boolean;
var
  ASize: TSize;
  AVisibleCaptionBounds: TRect;
begin
  AText := FindPropertiesHint(P);
  ACanvas.Font := ViewParams.Font;
  if AText <> '' then
  begin
    ASize := ACanvas.TextExtent(AText);
    ATextRect := cxRectBounds(CaptionRect.TopLeft, ASize.cx, ASize.cy);
    IsNeedOffsetHint := True;
    Result := True;
  end
  else
  begin
    AText := Caption;
    ASize := ACanvas.TextExtent(AText);
    cxRectIntersect(ATextRect, CaptionTextRect, Row.ViewInfo.HeaderInfo.HeaderRect);
    cxRectIntersect(AVisibleCaptionBounds, ATextRect, VerticalGrid.ClientBounds);
    IsNeedOffsetHint := False;
    Result := (cxRectWidth(AVisibleCaptionBounds) < ASize.cx - 1) or (cxRectHeight(AVisibleCaptionBounds) < ASize.cy - 1);
  end;
  AIsMultiLine := False;
end;

procedure TcxRowCaptionInfo.UpdateHotTrackState(const APoint: TPoint);
begin
  if PtInRect(CaptionRect, APoint) then
    SetState(cxbsHot)
  else
    SetState(cxbsNormal);
  UpdateFilterButtonHotTrackState(APoint);
end;

function TcxRowCaptionInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(CaptionRect, P);
end;

function TcxRowCaptionInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxRowCaptionInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

function TcxRowCaptionInfo.CloseFilterPopupOnDestruction: Boolean;
begin
  Result := False;
end;

procedure TcxRowCaptionInfo.FilterPopupClosed;
begin
  SetFilterButtonState(cxbsNormal);
end;

procedure TcxRowCaptionInfo.InitFilterPopup(APopup: TdxUIElementPopupWindow);
begin
  APopup.OwnerParent := VerticalGrid;
  APopup.Font := VerticalGrid.Font;
  APopup.LookAndFeel := VerticalGrid.LookAndFeel;
  APopup.BorderStyle := VerticalGrid.LookAndFeelPainter.PopupBorderStyle;
  APopup.OwnerBounds := FilterPopupOwnerBounds;
end;

function TcxRowCaptionInfo.GetFilterPopupLinkComponent: TComponent;
begin
  Result := RowProperties.EditContainer;
end;

function TcxRowCaptionInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := RowProperties.EditContainer.GetFilterPopupMode;
end;

function TcxRowCaptionInfo.GetFilterPopupOptions: TObject;
begin
  Result := RowProperties.EditContainer.Options;
end;

procedure TcxRowCaptionInfo.Calculate(ARect: TRect);
begin
  SetViewParams(HeaderInfo.GetCaptionViewParams);
  FCaptionRect := ARect;
  FImageRect := CalculateImageRect;
  FFilterButtonRect := CalculateFilterButtonRect;
  FCaptionTextRect := CalculateCaptionTextRect;
end;

function TcxRowCaptionInfo.CalculateCaptionTextRect: TRect;
const
  AOffset: TRect = (Left: 2; Top: 1; Right: 2; Bottom: 1);
var
  AEditTextOffset: TRect;
begin
  Result := CaptionRect;
  if IsImageVisible then
    Result.Left := ImageRect.Right;
  if IsFilterButtonVisible then
    Result.Right := FilterButtonRect.Left;
  AEditTextOffset := GetTextEditDrawTextOffset(RowProperties.HeaderAlignmentHorz, True);
  Result := cxRectContent(Result, AEditTextOffset);
  Result := cxRectContent(Result, AOffset);
end;

function TcxRowCaptionInfo.CalculateFilterButtonRect: TRect;
begin
  if IsFilterButtonVisible then
  begin
    Result := cxRectInflate(CaptionRect, -cxVerticalGridHeaderCellFilterButtonIndent);
    Result.Left := Result.Right - FilterButtonWidth;
    if IsFilterSmartTag then
      Result.Bottom := Result.Top + FilterSmartTagHeight;
  end
  else
    Result := cxEmptyRect;
end;

function TcxRowCaptionInfo.CalculateImageRect: TRect;
begin
  Result := cxRectCenterVertically(CaptionRect, ImageSize.cy);
  Result := cxRectSetWidth(Result, ImageSize.cx);
end;

procedure TcxRowCaptionInfo.CheckFilterPopupOwner;
begin
  if IsFilterPopupExist and FilterPopup.Visible and (FilterPopup.Owner = nil) and
    (FilterPopup.LinkComponent = GetFilterPopupLinkComponent) then
  begin
    FilterPopup.Owner := Self;
    SetFilterButtonState(cxbsPressed);
  end;
end;

procedure TcxRowCaptionInfo.FilterButtonStateChanged;
begin
  InvalidateFilterButton;
  if FilterButtonState = cxbsPressed then
    ShowFilterPopup;
end;

procedure TcxRowCaptionInfo.SetViewParams(const AParams: TcxViewParams);
begin
  FViewParams := AParams;
end;

procedure TcxRowCaptionInfo.Invalidate(ARecalculate: Boolean);
begin
  if ARecalculate then
    Recalculate;
  VerticalGrid.InvalidateRect(CaptionRect, False);
end;

procedure TcxRowCaptionInfo.InvalidateFilterButton;
begin
  VerticalGrid.InvalidateRect(FilterButtonRect, False);
end;

function TcxRowCaptionInfo.NeedRecalculateOnStateChanged: Boolean;
begin
  Result := IsFilterable and not IsFilterButtonAlwaysVisible;
end;

procedure TcxRowCaptionInfo.Recalculate;
begin
  Calculate(CaptionRect);
end;

procedure TcxRowCaptionInfo.RightToLeftConversion(const AClientBounds: TRect);
begin
  FCaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionRect, AClientBounds);
  FCaptionTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionTextRect, AClientBounds);
  FFilterButtonRect := TdxRightToLeftLayoutConverter.ConvertRect(FFilterButtonRect, AClientBounds);
  FImageRect := TdxRightToLeftLayoutConverter.ConvertRect(FImageRect, AClientBounds);
end;

procedure TcxRowCaptionInfo.SetFilterButtonState(AValue: TcxButtonState);
begin
  if FilterButtonState <> AValue then
  begin
    FFilterButtonState := AValue;
    FilterButtonStateChanged;
  end;
end;

procedure TcxRowCaptionInfo.SetState(AValue: TcxButtonState);
begin
  if State <> AValue then
  begin
    FState := AValue;
    StateChanged;
  end;
end;

procedure TcxRowCaptionInfo.ShowFilterPopup;
begin
  FilterPopup.Owner := Self;
  FilterPopup.AlignHorz := GetFilterPopupAlignHorz;
  FilterPopup.Popup;
end;

procedure TcxRowCaptionInfo.StateChanged;
begin
  Invalidate(NeedRecalculateOnStateChanged);
end;

procedure TcxRowCaptionInfo.UpdateFilterButtonHotTrackState(const APoint: TPoint);
begin
  if (FilterButtonState <> cxbsPressed) then
    if PtInRect(FilterButtonRect, APoint) then
      SetFilterButtonState(cxbsHot)
    else
      SetFilterButtonState(cxbsNormal);
end;

procedure TcxRowCaptionInfo.UpdateFilterButtonStateOnMouseDown;
begin
  case FilterButtonState of
    cxbsPressed:
      SetFilterButtonState(cxbsHot);
    cxbsHot:
      SetFilterButtonState(cxbsPressed);
  end;
end;

function TcxRowCaptionInfo.GetImageSize: TSize;
begin
  if IsImageVisible then
    Result := HeaderInfo.ViewInfo.ImageSize
  else
    Result := cxSize(0, 0);
end;

function TcxRowCaptionInfo.GetIsFilterButtonActive: Boolean;
begin
  Result := RowProperties.Filtered;
end;

function TcxRowCaptionInfo.GetIsFilterSmartTag: Boolean;
begin
  Result := VerticalGrid.OptionsView.IsRowHeaderFilterSmartTag;
end;

function TcxRowCaptionInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := VerticalGrid.LookAndFeelPainter;
end;

function TcxRowCaptionInfo.GetCaption: string;
begin
  if FCaptionAssigned then
    Result := FCaption
  else
    Result := RowProperties.Caption;
end;

function TcxRowCaptionInfo.GetFilterButtonWidth: Integer;
begin
  if IsFilterSmartTag then
    Result := Painter.ScaledFilterSmartTagSize(ScaleFactor).cx
  else
    Result := Painter.ScaledFilterDropDownButtonSize(ScaleFactor).X;
end;

function TcxRowCaptionInfo.GetFilterPopup: TdxFilterPopupWindow;
begin
  Result := VerticalGrid.Controller.FilterPopup;
end;

function TcxRowCaptionInfo.GetFilterPopupAlignHorz: TcxPopupAlignHorz;
begin
  if dxGetFilterPopupActualMode(GetFilterPopupMode) = fpmExcel then
    Result := pahLeft
  else
    Result := pahRight;
end;

function TcxRowCaptionInfo.GetFilterPopupOwnerBounds: TRect;
begin
  Result := FilterButtonRect;
  if dxGetFilterPopupActualMode(GetFilterPopupMode) <> fpmExcel then
  begin
    Result.Right := CaptionRect.Right;
    Result.Left := CaptionRect.Left;
    if RowCellIndex = 0 then
      Result.Left := HeaderInfo.HeaderRect.Left;
    Result := cxRectInflate(Result, HeaderInfo.ViewInfo.VertLineWidth, 0);
  end;
end;

function TcxRowCaptionInfo.GetFilterSmartTagHeight: Integer;
begin
  Result := Painter.ScaledFilterSmartTagSize(ScaleFactor).cy;
end;

function TcxRowCaptionInfo.GetFilterSmartTagState: TcxFilterSmartTagState;
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

function TcxRowCaptionInfo.GetFocused: Boolean;
begin
  Result := Row = VerticalGrid.FocusedRow;
end;

function TcxRowCaptionInfo.GetImageIndex: Integer;
begin
  if (Images <> nil) and (RowProperties.ImageIndex >= 0) and (RowProperties.ImageIndex < Images.Count) then
    Result := RowProperties.ImageIndex
  else
    Result := -1;
end;

function TcxRowCaptionInfo.GetImages: TCustomImageList;
begin
  Result := VerticalGrid.Images;
end;

function TcxRowCaptionInfo.GetRow: TcxCustomRow;
begin
  Result := HeaderInfo.Row;
end;

function TcxRowCaptionInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := VerticalGrid.ScaleFactor;
end;

function TcxRowCaptionInfo.GetTextFlags: Integer;
begin
  Result := GetTextAligmentFlags(RowProperties);
  if VerticalGrid.OptionsView.CellEndEllipsis then
    Result := Result or CXTO_END_ELLIPSIS;
end;

function TcxRowCaptionInfo.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := HeaderInfo.VerticalGrid;
end;

function TcxRowCaptionInfo.FindPropertiesHint(const P: TPoint): string;
var
  I: Integer;
begin
  Result := '';
  if (Row.FProperties is TcxCaptionRowProperties) then
    Result := TcxCaptionRowProperties(Row.FProperties).Hint
  else
    if (Row.FProperties is TcxMultiEditorRowProperties) then
      with TcxMultiEditorRowProperties(Row.FProperties), Row.ViewInfo.HeaderInfo do
        for I := 0 to CaptionsInfo.Count - 1 do
          if cxRectPtIn(CaptionsInfo[I].CaptionRect, P) then
          begin
            Result := Editors[I].Hint;
            break;
          end;
end;

function TcxRowCaptionInfo.IsFilterable: Boolean;
begin
  Result := RowProperties.FilterableByPopupMenu;
end;

function TcxRowCaptionInfo.IsFilterButtonAlwaysVisible: Boolean;
begin
  Result := VerticalGrid.OptionsView.IsRowHeaderFilterButtonShowedAlways;
end;

function TcxRowCaptionInfo.IsFilterButtonVisible: Boolean;
begin
  Result := IsFilterable and (IsFilterButtonAlwaysVisible or (State in [cxbsHot, cxbsPressed]) or
    (FilterButtonState = cxbsPressed) or (IsFilterSmartTag and IsFilterButtonActive));
end;

function TcxRowCaptionInfo.IsFilterPopupExist: Boolean;
begin
  Result := VerticalGrid.Controller.HasFilterPopup;
end;

function TcxRowCaptionInfo.IsFilterPopupOwner: Boolean;
begin
  Result := IsFilterPopupExist and (FilterPopup.Owner = Self);
end;

function TcxRowCaptionInfo.IsImageVisible: Boolean;
begin
  Result := (Images <> nil) and ((ImageIndex >= 0) or VerticalGrid.OptionsView.ShowEmptyRowImage);
end;

procedure TcxRowCaptionInfo.SetCaption(AValue: string);
begin
  FCaption := AValue;
  FCaptionAssigned := True;
end;

{ TcxvgDragImageHelper }

procedure TcxvgDragImageHelper.DragAndDrop(const P: TPoint);
var
  CP: TPoint;
begin
  inherited DragAndDrop(P);
  with TcxCustomVerticalGrid(DragControl).Controller do
  begin
    CP := DragControl.ScreenToClient(P);
    HitTest.RecalculateOnMouseEvent(CP.X, CP.Y, []);
    if (FAutoScrollObject <> nil) and not HitTest.HitAtCustomize then
      FAutoScrollObject.CheckBounds(CP);
  end;
end;

{ TcxvgDragRowObject }

function TcxvgDragRowObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  with TcxCustomVerticalGrid(Control), HitTest do
  begin
    if HitAtCustomize then
      Result := crDrag
    else
      if not HitInControl then
        Result := inherited GetDragCursor(Accepted, X, Y)
      else
        if not Accepted then
          Result := crcxInspectorNoDrag
        else
          if HitAtCaption then
            Result := crcxInspectorAddChild
          else
            if HitAtEmpty then
              Result := crcxInspectorAdd
            else
              Result := crcxInspectorInsert;
   end;
end;

function TcxvgDragRowObject.GetRow: TcxCustomRow;
begin
  Result := TcxCustomVerticalGrid(Control).Controller.DragRow;
end;

{ TcxvgCellNavigator }

constructor TcxvgCellNavigator.Create(AController: TcxCustomControlController);
begin
  inherited Create(AController);
  DownOnTab := True;
  DownOnEnter := True;
end;

procedure TcxvgCellNavigator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_RIGHT:
      begin
        if Controller.VerticalGrid.UseRightToLeftAlignment then
          Key := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key);
        FocusNextCell(Key = VK_RIGHT, False, True);
        Key := 0;
      end;
    VK_RETURN, VK_F2:
      if (Controller.FocusedItem <> nil) and (Shift = []) then
      begin
        TcxCellEdit(Controller.FocusedItem).Editing := True;
        if Controller.FocusedItem.Editing then
          Key := 0;
      end;
    VK_UP, VK_DOWN:
      begin
        FocusNextCell(Key = VK_DOWN, True, True);
        Key := 0;
      end;
  end;
end;

function TcxvgCellNavigator.FindNextRecord(AForward: Boolean): Boolean;
begin
  Result := Controller.Scroller.ScrollStrategy.FindNextRecord(
    DataController.FocusedRowIndex, AForward) <> -1;
end;

function TcxvgCellNavigator.FocusNextCell(AForward, ANextRow, ATabStopNavigation: Boolean): Boolean;
begin
  RowCount := Controller.ViewInfo.VisibleRowCount;
  if ANextRow then
    Result := NavigateVertical(AForward, ATabStopNavigation)
  else
    Result := NavigateHorizontal(AForward);
end;

procedure TcxvgCellNavigator.FocusRecordRowCell(ARecordIndex: Integer;
  ARow: TcxCustomRow; ACellIndex: Integer);
var
  AContainer: TcxCustomInplaceEditContainer;
begin
  AContainer := ARow.GetEditContainer(ACellIndex);
  if AContainer <> nil then
    Controller.SetFocusedRecordItem(ARecordIndex, AContainer)
  else
  begin
    Controller.VerticalGrid.BeginUpdate;
    try
      Controller.SetFocusedRecordIndex(ARecordIndex);
      ARow.Focused := True;
    finally
      Controller.VerticalGrid.EndUpdate;
    end;
  end;
end;

function TcxvgCellNavigator.GetContainerCount(
  ARowIndex: Integer): Integer;
begin
  if cxInRange(ARowIndex, 0, RowCount - 1) then
    Result := Rows[ARowIndex].GetEditContainerCount
  else
    Result := 0;
end;

function TcxvgCellNavigator.FindNextRow(var ARow: TcxCustomRow; var ACellIndex: Integer; AForward: Boolean): Boolean;
var
  ARowIndex: Integer;
begin
  Result := False;
  if ARow = nil then
    ARow := GetRowForNextRecord(AForward);
  if ARow = nil then
    Exit;
  ARowIndex := ARow.VisibleIndex;
  Inc(ARowIndex, cxIntOffs[AForward]);
  while cxRange(ARowIndex, 0, RowCount - 1) do
  begin
    ARow := Rows[ARowIndex];
    Result := ARow.CanNavigate;
    if Result then
      Break;
    Inc(ARowIndex, cxIntOffs[AForward]);
  end;
  if Result then
  begin
    ACellIndex := GetCellIndexForNextRow(AForward, ARow);
    Result := ValidateNextCell(ARow, AForward, ACellIndex);
  end
end;

function TcxvgCellNavigator.GetCellIndexForNextRow(AForward: Boolean;
  ANextRow: TcxCustomRow): Integer;
var
  AContainerCount: Integer;
begin
  if ANextRow = nil then
    Result := 0
  else
  begin
    AContainerCount := ANextRow.GetEditContainerCount;
    if AForward or (AContainerCount = 0) then
      Result := 0
    else
      Result := AContainerCount - 1;
  end;
end;

function TcxvgCellNavigator.GetRowForNextRecord(AForward: Boolean): TcxCustomRow;
begin
  Result := nil;
  if RowCount > 0 then
    if AForward then
      Result := Rows[0]
    else
      Result := Rows[RowCount - 1];
end;

function TcxvgCellNavigator.NavigateHorizontal(AForward: Boolean): Boolean;
var
  ACurCell: Integer;
  ACurRow: TcxCustomRow;
  AHasEditCell: Boolean;
begin
  ACurRow  := Controller.FocusedRow;
  ACurCell := Controller.CellIndex;
  Result := Assigned(ACurRow) and ACurRow.CanFocus;
  if Result then
  begin
    Inc(ACurCell, cxIntOffs[AForward]);
    AHasEditCell := ACurRow.GetEditContainerCount > 0;
    if not AHasEditCell or not ValidateNextCell(ACurRow, AForward, ACurCell) then
    begin
      if Controller.CanChangeRecord and FindNextRecord(AForward) then
      begin
        ACurCell := GetCellIndexForNextRow(AForward, ACurRow);
        if ValidateNextCell(ACurRow, AForward, ACurCell) then
          FocusRecordRowCell(Controller.FocusedRecordIndex + cxIntOffs[AForward], ACurRow, ACurCell)
        else
          Controller.SetFocusedRowAndCell(ACurRow, ACurCell);
      end;
    end
    else
      Controller.SetFocusedRowAndCell(ACurRow, ACurCell);
  end;
end;

function TcxvgCellNavigator.NavigateMultiEditorRow(ARow: TcxCustomRow; AForward: Boolean): Boolean;
var
  ACurCell: Integer;
begin
  ACurCell := Controller.CellIndex + cxIntOffs[AForward];
  Result := ARow.CanNavigate and ValidateNextCell(ARow, AForward, ACurCell);
  if Result then
     Controller.SetFocusedRowAndCell(ARow, ACurCell);
end;

function TcxvgCellNavigator.NavigateVertical(AForward, AForceNextRow: Boolean): Boolean;
var
  ACurCell: Integer;
  ACurRow: TcxCustomRow;
begin
  ACurRow := Controller.FocusedRow;
  if ACurRow = nil then
    ACurRow := GetRowForNextRecord(AForward);
  Result := Assigned(ACurRow);
  if Result then
  begin
    if not AForceNextRow and Controller.ProcessMultiEditorRow then
    begin
      if NavigateMultiEditorRow(ACurRow, AForward) then
        Exit;
    end;
    if FindNextRow(ACurRow, ACurCell, AForward) then
      Controller.SetFocusedRowAndCell(ACurRow, ACurCell)
    else
      if Controller.GetFocusCellOnCycle then
      begin
        if ACurRow <> nil then
        begin
          ACurRow := GetRowForNextRecord(AForward);
          ACurCell := GetCellIndexForNextRow(AForward, ACurRow);
          if ValidateNextCell(ACurRow, AForward, ACurCell) or FindNextRow(ACurRow, ACurCell, AForward) then
          begin
            if Controller.CanChangeRecord then
            begin
              if FindNextRecord(AForward) then
                FocusRecordRowCell(Controller.FocusedRecordIndex + cxIntOffs[AForward], ACurRow, ACurCell)
            end
            else
              Controller.SetFocusedRowAndCell(ACurRow, ACurCell)
          end;
        end;
      end;
  end;
end;

function TcxvgCellNavigator.ValidateNextCell(ARow: TcxCustomRow; AForward: Boolean;
  var ACellIndex: Integer): Boolean;
var
  AContainer: TcxCellEdit;
  AContainerCount: Integer;
begin
  Result := ARow.CanFocus;
  if ARow.IsCategory then
    Exit;
  if Result then
  begin
    Result := False;
    AContainerCount := ARow.GetEditContainerCount;
    while cxRange(ACellIndex, 0, AContainerCount - 1) and not Result do
    begin
      AContainer := ARow.GetEditContainer(ACellIndex);
      Result := AContainer.CanTabStop and AContainer.CanFocus;
      if not Result then
        Inc(ACellIndex, cxIntOffs[AForward]);
    end;
  end;
end;

function TcxvgCellNavigator.GetController: TcxvgController;
begin
  Result := TcxvgController(inherited Controller);
end;

function TcxvgCellNavigator.GetRecordCount: Integer;
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  if DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) then
    Result := AIntf.GetDataSetRecordCount
  else
    Result := DataController.RowCount;
end;

function TcxvgCellNavigator.GetRow(Index: Integer): TcxCustomRow;
begin
  Result := Controller.ViewInfo.VisibleRows[Index];
end;

{ TcxCaptionInfoList }

function TcxCaptionInfoList.GetItem(Index: Integer): TcxRowCaptionInfo;
begin
  Result := TcxRowCaptionInfo(List[Index]);
end;

procedure TcxCaptionInfoList.RightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(AClientBounds);
end;

{ TcxValueInfoList }

constructor TcxValueInfoList.Create(AViewInfo: TcxvgCustomViewInfo);
begin
  inherited Create;
  Capacity := 64;
  FViewInfo := AViewInfo;
end;

destructor TcxValueInfoList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ViewInfo.RemoveEditCellViewInfo(GetItem(I));
  inherited Destroy;
end;

function TcxValueInfoList.GetItem(Index: Integer): TcxRowValueInfo;
begin
  Result := TcxRowValueInfo(List[Index]);
end;

procedure TcxValueInfoList.ClearRightToLeftConversionFlags;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].IsRightToLeftConverted := False;
end;

procedure TcxValueInfoList.RightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(AClientBounds);
end;

{ TcxCustomRowHeaderInfo }

constructor TcxCustomRowHeaderInfo.Create(ARow: TcxCustomRow);
begin
  FCaptionsInfo := TcxCaptionInfoList.Create;
  FCategoryIndents := TIndentInfoList.Create;
  FLinesInfo := TLineInfoList.Create;
  FRow := ARow;
  FRowIndents := TIndentInfoList.Create;
end;

destructor TcxCustomRowHeaderInfo.Destroy;
begin
  FreeAndNil(FCaptionsInfo);
  FreeAndNil(FLinesInfo);
  FreeAndNil(FRowIndents);
  FreeAndNil(FCategoryIndents);
  inherited Destroy;
end;

function TcxCustomRowHeaderInfo.GetPaintStyle: TcxvgPaintStyle;
begin
  Result := VerticalGrid.OptionsView.PaintStyle;
end;

function TcxCustomRowHeaderInfo.GetSelected: Boolean;
begin
  with VerticalGrid.Controller do
    Result := (DesignSelectionHelper <> nil) and
      DesignSelectionHelper.IsObjectSelected(Row);
end;

function TcxCustomRowHeaderInfo.GetShowButton: Boolean;
begin
  with VerticalGrid.OptionsView do
    Result := ShowButtons and ShowHeaders and Row.Options.ShowExpandButton and
      Row.HasVisibleChildren;
end;

function TcxCustomRowHeaderInfo.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Row.VerticalGrid;
end;

procedure TcxCustomRowHeaderInfo.AddBottomHorzLine(const R: TRect);
begin
  with ViewInfo do
    if HorzLineWidth > 0 then
      Self.LinesInfo.Add(R.Left, R.Bottom, R.Right - R.Left, HorzLineWidth, HorzLineBrush);
end;

procedure TcxCustomRowHeaderInfo.AddBoundHeaderLines;
begin
  ViewInfo.CalcHelper.AddBoundHeaderLines(Self);
end;

procedure TcxCustomRowHeaderInfo.AddNextIndentInfo(const ABounds: TRect;
  const AViewParams: TcxViewParams;
  AToCategories, AUnderline, AAddVertLine: Boolean);
begin
  if AToCategories then
    CategoryIndents.Add(ABounds, AViewParams)
  else
    RowIndents.Add(ABounds, AViewParams);
  ViewInfo.CalcHelper.AddHeaderIndentLines(Self, ABounds, AViewParams,
    AToCategories, AUnderline, AAddVertLine);
end;

procedure TcxCustomRowHeaderInfo.AddRightVertLine(const R: TRect);
begin
  if ViewInfo.ShowHeaders then
    ViewInfo.CalcHelper.AddDivider(LinesInfo, R, clNone, False);
end;

procedure TcxCustomRowHeaderInfo.Calc(const AHeaderRect: TRect;
  AViewInfo: TcxvgCustomViewInfo; ANextRow: TcxCustomRow;
  ACalcBounds: Boolean);
begin
  FViewInfo := AViewInfo;
  Clear;
  FTransparent := False;
  FHeaderRect := AHeaderRect;
  FFocused := FRow = VerticalGrid.FocusedRow;
  CalcViewParams(ACalcBounds);
  CalcIndentBounds(ANextRow, ACalcBounds);
  CalcExpandButton;
  AddBoundHeaderLines;
  FHeaderCellsRect := AHeaderRect;
  FHeaderCellsRect.Left := IndentBounds.Right;
  if not cxRectIsEmpty(FHeaderCellsRect) then
  begin
    AddRightVertLine(AHeaderRect);
    CalcRowCaptionsInfo;
    FFocusRect := GetFocusRect;
  end
  else
    FHeaderCellsRect := cxNullRect;
end;

function TcxCustomRowHeaderInfo.CalcCaptionInfo(ARowProperties: TcxCaptionRowProperties; const R: TRect): TcxRowCaptionInfo;
begin
  Result := TcxRowCaptionInfo.Create(Self, ARowProperties);
  Result.Calculate(R);
end;

procedure TcxCustomRowHeaderInfo.CalcExpandButton;
begin
  if GetShowButton then
  begin
    DoCalcExpandButton;
    FButtonColor := GetButtonColor;
  end;
end;

function TcxCustomRowHeaderInfo.CalcIndentsInfo(ALevelCount: Integer;
  ANextRow: TcxCustomRow): TIndentRectInfoList;
var
  ACurRow: TcxCustomRow;
  AUnderline, AIsCategory: Boolean;
  I, ANextRowLevel: Integer;
begin
  Result := TIndentRectInfoList.Create;
  if ANextRow = nil then ANextRowLevel := -1 else ANextRowLevel := ANextRow.Level;
  ACurRow := Row;
  for I := ALevelCount downto 0 do
  begin
    AIsCategory := ACurRow.IsCategory;
    if AIsCategory then
      AUnderline := (ANextRow = nil) or (ACurRow.Level >= ANextRowLevel)
    else
      AUnderline := I = ALevelCount;
    Result.Add(cxSize(ViewInfo.RowIndentWidth, cxRectHeight(HeaderRect)),
      AIsCategory, AUnderline, ViewInfo.CalcHelper.GetIndentViewParams(Row, ACurRow));
    ACurRow := ACurRow.FParent;
  end;
end;

procedure TcxCustomRowHeaderInfo.CalcIndentBounds(ANextRow: TcxCustomRow;
  ACalculate: Boolean);
var
  R: TRect;
  I, J, W, ALevelCount, ANextLevel, AHeight, ABoundsWidth: Integer;
  AIndents: TIndentRectInfoList;
begin
  AHeight := cxRectHeight(HeaderRect);
  R := cxRectBounds(HeaderRect.Left, HeaderRect.Top, 0, AHeight);
  if not ACalculate then
  begin
    FIndentBounds := R;
    Exit;
  end;
  ALevelCount := cxSetValue(ViewInfo.ShowHeaders, Row.Level, -1);
  if ANextRow = nil then ANextLevel := -1 else ANextLevel := ANextRow.Level;
  AIndents := CalcIndentsInfo(ALevelCount, ANextRow);
  ABoundsWidth := 0;
  W := ViewInfo.VertLineWidth;
  J := 0;
  try
    for I := 0 to ALevelCount do
      with AIndents[ALevelCount - I]^ do
      begin
        if IsCategory then
        begin
          if R.Right - R.Left > 0 then
          begin
            Inc(R.Right, W);
            cxRectIntersect(R, R, HeaderRect);
            if not cxRectIsEmpty(R) then
              AddNextIndentInfo(R, AIndents[ALevelCount - J].ViewParams, False, ANextLevel - I <= 0, True);
            Inc(ABoundsWidth, W);
          end;
          cxRectIntersect(R, cxRectBounds(HeaderRect.Left + ABoundsWidth,
            HeaderRect.Top, Size.cx + W, Size.cy), HeaderRect);
          if not cxRectIsEmpty(R) then
            AddNextIndentInfo(cxRectBounds(HeaderRect.Left + ABoundsWidth, HeaderRect.Top,
              Size.cx + W, Size.cy), ViewParams, True, Underline, I < ALevelCount);
					Inc(ABoundsWidth, cxSetValue(I < ALevelCount, W, cxSetValue(IncreaseBoundsByLastVertLine, W, 0)));
          R := cxRectBounds(HeaderRect.Left + ABoundsWidth + Size.cx, HeaderRect.Top, 0, AHeight);
        end
        else
        begin
          Inc(R.Right, Size.cx);
          J := I;
        end;
        Inc(ABoundsWidth, Size.cx);
      end;
    cxRectIntersect(R, R, FHeaderRect);
    if not cxRectIsEmpty(R) then
      AddNextIndentInfo(R, AIndents[ALevelCount - J].ViewParams, False, True, False);
    FIndentBounds := cxRectBounds(HeaderRect.TopLeft, ABoundsWidth, AHeight);
  finally
    AIndents.Free;
  end;
end;

procedure TcxCustomRowHeaderInfo.CalcRowCaptionsInfo;
var
  ACaptionInfo: TcxRowCaptionInfo;
begin
  ACaptionInfo := CalcCaptionInfo(TcxCaptionRowProperties(Row.FProperties), HeaderCellsRect);
  CaptionsInfo.Add(ACaptionInfo);
end;

procedure TcxCustomRowHeaderInfo.CalcViewParams(AAllowFocus: Boolean);
begin
  FIndentViewParams := Row.Styles.GetHeaderParams(Row);
  if AAllowFocus and Focused and (PaintStyle = psDotNet) then
    ViewParams := VerticalGrid.Styles.GetSelectedHeaderParams(Row)
  else
    ViewParams := IndentViewParams;
end;

procedure TcxCustomRowHeaderInfo.Clear;
begin
  FIsRightToLeftConverted := False;
  FCaptionsInfo.Clear;
  FLinesInfo.Clear;
  FRowIndents.Clear;
  FCategoryIndents.Clear;
  FButtonAreaBounds := cxNullRect;
  FButtonRect := cxNullRect;
  FFocusRect := cxNullRect;
end;

procedure TcxCustomRowHeaderInfo.DoCalcExpandButton;
var
  AWidth: Integer;
  AButtonPlace: TRect;
begin
  AWidth := GetButtonPlaceBackgroundWidth;
  AButtonPlace := cxRect(IndentBounds.Right - AWidth, IndentBounds.Top, IndentBounds.Right, IndentBounds.Bottom);
  FButtonRect := cxRectCenter(AButtonPlace, ViewInfo.ButtonSize, ViewInfo.ButtonSize);
  FButtonAreaBounds := cxRectCenter(AButtonPlace, ViewInfo.ButtonAreaSize, ViewInfo.ButtonAreaSize);
end;

procedure TcxCustomRowHeaderInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FCaptionsInfo.RightToLeftConversion(AClientBounds);
  FButtonAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect(FButtonAreaBounds, AClientBounds);
  FButtonRect := TdxRightToLeftLayoutConverter.ConvertRect(FButtonRect, AClientBounds);
  FFocusRect := TdxRightToLeftLayoutConverter.ConvertRect(FFocusRect, AClientBounds);
  FHeaderCellsRect := TdxRightToLeftLayoutConverter.ConvertRect(FHeaderCellsRect, AClientBounds);
  FHeaderRect := TdxRightToLeftLayoutConverter.ConvertRect(FHeaderRect, AClientBounds);
  FIndentBounds := TdxRightToLeftLayoutConverter.ConvertRect(FIndentBounds, AClientBounds);
  CategoryIndents.RightToLeftConversion(AClientBounds);
  RowIndents.RightToLeftConversion(AClientBounds);
  LinesInfo.RightToLeftConversion(AClientBounds);
end;

function TcxCustomRowHeaderInfo.GetButtonPlaceBackgroundWidth: Integer;
begin
  Result := ViewInfo.RowIndentWidth;
end;

function TcxCustomRowHeaderInfo.GetCaptionViewParams: TcxViewParams;
begin
  Result := FViewParams;
end;

function TcxCustomRowHeaderInfo.GetButtonColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomRowHeaderInfo.GetFocusRect: TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomRowHeaderInfo.IncreaseBoundsByLastVertLine: Boolean;
begin
  Result := False;
end;

function TcxCustomRowHeaderInfo.LeftViewPoint: Integer;
begin
  Result := FHeaderRect.Left;
  if (PaintStyle = psDotNet) and (FRowIndents.Count > 0) then
    Result := FRowIndents[FRowIndents.Count - 1].Bounds.Left;
end;

procedure TcxCustomRowHeaderInfo.MakeSelectedViewParams(AFocused: Boolean);
var
  I: Integer;
begin
  FIndentViewParams := VerticalGrid.Styles.GetSelectedHeaderParams(Row, AFocused);
  FViewParams := FIndentViewParams;
  for I := 0 to FCaptionsInfo.Count - 1 do
    FCaptionsInfo[I].SetViewParams(ViewParams);
end;

procedure TcxCustomRowHeaderInfo.Recalculate(ANextRow: TcxCustomRow; ACalcBounds: Boolean);
begin
  Calc(HeaderRect, VerticalGrid.ViewInfo, ANextRow, ACalcBounds);
end;

procedure TcxCustomRowHeaderInfo.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(AClientBounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TcxCustomRowViewInfo }

constructor TcxCustomRowViewInfo.Create(ARow: TcxCustomRow);
begin
  FRow := ARow;
  FBandIndex := -1;
  FBandRowIndex := -1;
  FValuesLinesInfo := TLineInfoList.Create;
  FValuesLinesInfo.Delta := 4;
  FValuesInfo := TcxValueInfoList.Create(VerticalGrid.ViewInfo);
  FHeaderInfo := Row.CreateHeaderInfo;
end;

destructor TcxCustomRowViewInfo.Destroy;
begin
  ClearValuesInfo;
  FRow.FViewInfo := nil;
  FreeAndNil(FValuesInfo);
  FreeAndNil(FValuesLinesInfo);
  FreeAndNil(FHeaderInfo);
  inherited Destroy;
end;

procedure TcxCustomRowViewInfo.Calc(const ARowRect: TRect;
  AViewInfo: TcxvgCustomViewInfo; ANextRow: TcxCustomRow);
begin
  FRowRect := ARowRect;
  AViewInfo.CalcRowRects(Self);
  CalcRowHeaderInfo(ANextRow);
  if not FInitialized then
    InitValuesInfo;
  CalcValuesInfo;
  CalcPaintViewParamsLines(ANextRow);
end;

procedure TcxCustomRowViewInfo.Recalculate(ANextRow: TcxCustomRow);
var
  ARowRect: TRect;
begin
  if not NeedLTRBounds then
    Calc(RowRect, ViewInfo, ANextRow)
  else
  begin
    ARowRect := TdxRightToLeftLayoutConverter.ConvertRect(RowRect, RightToLeftConversionClientBounds);
    Calc(ARowRect, ViewInfo, ANextRow);
    DoSpecialRightToLeftConversion;
  end;
end;

procedure TcxCustomRowViewInfo.Update;
begin
  with VerticalGrid, ViewInfo do
  begin
    if Row = FocusedRow then
      ChangeFocusedRow(Row, PrevVisibleRow(Row))
    else
    begin
      LinesInfo.Locked := True;
      FocusLinesInfo.Locked := True;
      try
        Self.Recalculate(NextVisibleRow(Row));
      finally
        LinesInfo.Locked := False;
        FocusLinesInfo.Locked := False;
        InvalidateRect(RowRect, False);
      end;
    end;
  end;
end;

procedure TcxCustomRowViewInfo.UpdateRecord(ARecordIndex: Integer);
var
  I: Integer;
  ACellInfo: TcxRowValueInfo;
begin
  if not NeedLTRBounds then
    CalcValuesInfo
  else
  begin
    FValuesRect := TdxRightToLeftLayoutConverter.ConvertRect(FValuesRect, RightToLeftConversionClientBounds);
    FRowRect := TdxRightToLeftLayoutConverter.ConvertRect(FRowRect, RightToLeftConversionClientBounds);
    CalcValuesInfo;
    DoSpecialRightToLeftConversion;
  end;
  for I := 0 to ValuesInfo.Count - 1 do
  begin
    ACellInfo := ValuesInfo[I];
    if ACellInfo.FRecordIndex = ARecordIndex then
    begin
      ACellInfo.Invalidate(True);
      ACellInfo.UpdateEditRect;
    end;
  end;
end;

procedure TcxCustomRowViewInfo.AddRectValueLines(R: TRect;
  ALast, ABottomLineNeeded: Boolean);
begin
  if ALast then
  begin
    if R.Left > ViewInfo.ClipRect.Right then Exit;
    if R.Right > ViewInfo.ClipRect.Right then R.Right := ViewInfo.ClipRect.Right;
  end;
  if ABottomLineNeeded then
    ViewInfo.AddBottomValueSide(Self, R);
  ViewInfo.AddRightValueSide(Self, R, ALast);
end;

function TcxCustomRowViewInfo.AddValueInfo(ARecordIndex, ACellIndex: Integer): TcxRowValueInfo;
begin
  Result := GetRowValueInfoClass.Create(Row.GetEditContainer(ACellIndex));
  Result.FRow := Row;
  Result.FRecordIndex := ARecordIndex;
  Result.FRowCellIndex := ACellIndex;
  ValuesInfo.Add(Result);
end;

procedure TcxCustomRowViewInfo.CalculateHeight(ABandWidth: Integer);
begin
  CalculatedHeight := Max(Row.GetRealHeight, ViewInfo.DefaultEditHeight);
  CalculatedHeight := Max(CalculatedHeight, GetValuesHeight(ABandWidth, ViewInfo));
end;

procedure TcxCustomRowViewInfo.CalcValuesInfo;
begin
  ValuesLinesInfo.Clear;
end;

procedure TcxCustomRowViewInfo.CalcRowHeaderInfo(ANextRow: TcxCustomRow);
begin
  HeaderInfo.Calc(HeaderInfo.HeaderRect, ViewInfo, ANextRow, True);
end;

procedure TcxCustomRowViewInfo.CalcPaintViewParamsLines(ANextRow: TcxCustomRow);
begin
	ViewInfo.CalcHelper.CalcPaintViewParamsLines(Self, ANextRow);
end;

procedure TcxCustomRowViewInfo.DoSpecialRightToLeftConversion;
begin
  ValuesInfo.ClearRightToLeftConversionFlags;
  DoRightToLeftConversion(RightToLeftConversionClientBounds);
end;

procedure TcxCustomRowViewInfo.ClearValuesInfo;
begin
  FIsRightToLeftConverted := False;
  FInitialized := False;
  ValuesInfo.Clear;
  ValuesLinesInfo.Clear;
end;

function TcxCustomRowViewInfo.GetValuesHeight(ABandWidth: Integer;
  AViewInfo: TcxvgCustomViewInfo): Integer;
begin
  Result := -1;
end;

procedure TcxCustomRowViewInfo.InitValuesInfo;
var
  I, J, ARecordIndex, ACellCount: Integer;
  AValueInfo: TcxRowValueInfo;
begin
  ClearValuesInfo;
  ACellCount := GetEditContainerCount;
  if ACellCount > 0 then
  begin
    ARecordIndex := ViewInfo.FirstVisibleRecordIndex;
    for I := 0 to ViewInfo.VisibleValueCount - 1 do
    begin
      for J := 0 to ACellCount - 1 do
      begin
        AValueInfo := AddValueInfo(ARecordIndex, J);
        AValueInfo.DoCalculate;
      end;
      Inc(ARecordIndex);
    end;
  end;
  FInitialized := True;
end;

function TcxCustomRowViewInfo.NeedLTRBounds: Boolean;
begin
  Result := IsRightToLeftConverted and ViewInfo.IsRightToLeftConverted;
end;

function TcxCustomRowViewInfo.GetEditContainerCount: Integer;
begin
  Result := Row.GetEditContainerCount;
end;

function TcxCustomRowViewInfo.GetRowValueInfoClass: TcxRowValueInfoClass;
begin
  Result := TcxRowValueInfo;
end;

function TcxCustomRowViewInfo.GetCalculatedHeight: Integer;
begin
  if not FInitialized then
  begin
    InitValuesInfo;
    CalculateHeight(ViewInfo.ViewBandWidth);
  end;
  Result := FCalculatedHeight;
end;

function TcxCustomRowViewInfo.GetRowValueInfo(ARecordIndex,
  ACellIndex: Integer): TcxRowValueInfo;
var
  I: Integer;
  ARowValueInfo: TcxRowValueInfo;
begin
  Result := nil;
  if ValuesInfo = nil then Exit;
  for I := 0 to ValuesInfo.Count - 1 do
  begin
    ARowValueInfo := ValuesInfo[I];
    with ARowValueInfo do
      if (RecordIndex = ARecordIndex) and (RowCellIndex = ACellIndex) then
      begin
        Result := ARowValueInfo;
        break
      end;
  end;
end;

function TcxCustomRowViewInfo.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Row.VerticalGrid;
end;

function TcxCustomRowViewInfo.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := Row.VerticalGrid.ViewInfo;
end;

procedure TcxCustomRowViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FRowRect := TdxRightToLeftLayoutConverter.ConvertRect(FRowRect, AClientBounds);
  HeaderInfo.RightToLeftConversion(AClientBounds);
  FValuesRect := TdxRightToLeftLayoutConverter.ConvertRect(FValuesRect, AClientBounds);
  ValuesInfo.RightToLeftConversion(AClientBounds);
  ValuesLinesInfo.RightToLeftConversion(AClientBounds);
end;

procedure TcxCustomRowViewInfo.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(AClientBounds);
    FRightToLeftConversionClientBounds := AClientBounds;
    FIsRightToLeftConverted := True;
  end;
end;

{ TcxRowViewInfoList }

constructor TcxRowViewInfoList.Create;
begin
  Capacity := 1024;
end;

function TcxRowViewInfoList.Find(ARow: TcxCustomRow): TcxCustomRowViewInfo;
var
  I: Integer;
  AViewInfo: TcxCustomRowViewInfo;
begin
  Result := nil;
  if ARow = nil then Exit;
  for I := 0 to Count - 1 do
  begin
    AViewInfo := List[I];
    if AViewInfo.Row = ARow then
    begin
      Result := AViewInfo;
      break;
    end;
  end;
end;

function TcxRowViewInfoList.GetItem(Index: Integer): TcxCustomRowViewInfo;
begin
  Result := List[Index];
end;

procedure TcxRowViewInfoList.RightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(AClientBounds);
end;

{ TcxvgCustomViewInfo }

constructor TcxvgCustomViewInfo.Create(
  AOwner: TcxEditingControl);
begin
  inherited Create(AOwner);
  FVerticalGrid := TcxCustomVerticalGrid(AOwner);
  FLinesInfo := TLineInfoList.Create;
  FFocusLinesInfo := TLineInfoList.Create;
  FRowsViewInfo := TcxRowViewInfoList.Create;
  FViewRects := TViewRects.Create;
  FHorzLineWidth := 1;
  FVertLineWidth := 1;
  FCalcHelper := CreateCalcHelper;
  FLockDividerPos := True;
  CalcLayoutGeneral;
  UpdateScroller;
end;

destructor TcxvgCustomViewInfo.Destroy;
begin
  FreeAndNil(FCalcHelper);
  FreeAndNil(FLinesInfo);
  FreeAndNil(FFocusLinesInfo);
  FreeAndNil(FRowsViewInfo);
  FreeAndNil(FViewRects);
  // destroy rows viewinfos on RecreateViewInfo
  VerticalGrid.FreeRowsViewInfo;
  DestroyBrushes;
  inherited Destroy;
end;

procedure TcxvgCustomViewInfo.CalcEditCell(
  const ABounds: TRect; ARowValueInfo: TcxRowValueInfo);
begin
  if ARowValueInfo <> nil then
    ARowValueInfo.SetBounds(ABounds, ClipRect);
end;

function TcxvgCustomViewInfo.CalcRowHeight(ARow: TcxCustomRow): Integer;
begin
  Result := ARow.ViewInfo.CalculatedHeight;
end;

procedure TcxvgCustomViewInfo.ChangeFocusedRow(
  ANewFocus, AOldFocus: TcxCustomRow);
var
  R: TRect;
begin
  if VerticalGrid.IsLocked then Exit;
  LinesInfo.Locked := True;
  try
    R := CalcHelper.ChangeFocusedRow(ANewFocus, AOldFocus);
  finally
    LinesInfo.Locked := False;
  end;
  VerticalGrid.InvalidateRect(R, False);
end;

function TcxvgCustomViewInfo.GetDefaultGridModeBufferCount: Integer;
begin
  Result := GetVisibleValueCount;
end;

function TcxvgCustomViewInfo.GetRowViewInfo(
  ARow: TcxCustomRow): TcxCustomRowViewInfo;
begin
  Result := RowsViewInfo.Find(ARow);
end;

function TcxvgCustomViewInfo.GetLTRValueRect(AValueIndex: Integer;
  ARowViewInfo: TcxCustomRowViewInfo): TRect;
begin
  with ARowViewInfo do
    Result := cxRect(ValuesRect.Left, RowRect.Top, RowRect.Right, RowRect.Bottom);
end;

function TcxvgCustomViewInfo.GetLTRValuesRect(
  ARowViewInfo: TcxCustomRowViewInfo): TRect;
begin
  with ARowViewInfo do
    Result := cxRect(RowRect.Left + ViewHeaderWidth + DividerWidth, RowRect.Top, RowRect.Right, RowRect.Bottom);
end;

function TcxvgCustomViewInfo.GetValueRect(AValueIndex: Integer; ARowViewInfo: TcxCustomRowViewInfo): TRect;
begin
  Result := GetLTRValueRect(AValueIndex, ARowViewInfo);
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxvgCustomViewInfo.GetValuesRect(ARowViewInfo: TcxCustomRowViewInfo): TRect;
begin
  Result := GetLTRValuesRect(ARowViewInfo);
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

procedure TcxvgCustomViewInfo.UpdateRecord(ARecordIndex: Integer);
var
  I: Integer;
begin
  if not CanUpdateRecord(ARecordIndex) then Exit;
  for I := 0 to RowsViewInfo.Count - 1 do
    RowsViewInfo[I].UpdateRecord(ARecordIndex);
end;

function TcxvgCustomViewInfo.CalculateDefaultEditHeight: Integer;
begin
  Result := GetMinRowHeight;
end;

procedure TcxvgCustomViewInfo.DoCalculate;
begin
  FIsRightToLeftConverted := False;
  FindPanelViewInfo.IsRightToLeftConverted := False;
  ClearLinesAndRows;
  CalcLayoutGeneral;
  CalculateBandsInfo;
  CalcViewRects;
  CalcRowsViewInfo;
  CheckRowHeaderWidth;
  CalculateNavigator;
  CalculateFindPanel;
  CalculateFilterBox;
  CheckBiDiMode;
end;

function TcxvgCustomViewInfo.IsPanArea(const APoint: TPoint): Boolean;
var
  R: TRect;
begin
  if BandInfo.Count > 0 then
  begin
    R := ClientRect;
    Inc(R.Left, GetViewHeaderWidth);
    Result := cxRectPtIn(R, APoint);
  end
  else
    Result := False;
end;

procedure TcxvgCustomViewInfo.UpdateSelection;
begin
  inherited UpdateSelection;
  VerticalGrid.Controller.RefreshFocusedRow;
end;

procedure TcxvgCustomViewInfo.AddBandRowsLines(const R: TRect);
begin
  if HorzLineWidth > 0 then
  begin
    LinesInfo.Add(R.Left, R.Top, R.Right - R.Left, HorzLineWidth, BandBorderColor);
    LinesInfo.Add(R.Left + VertLineWidth, R.Bottom - HorzLineWidth,
      (R.Right - R.Left) - 2 * VertLineWidth, HorzLineWidth, BandBorderColor);
  end;
  if VertLineWidth > 0 then
    LinesInfo.Add(R.Left, R.Top + HorzLineWidth, VertLineWidth,
      (R.Bottom - R.Top) - HorzLineWidth, BandBorderColor);
end;

procedure TcxvgCustomViewInfo.AddBottomValueSide(ARowViewInfo: TcxCustomRowViewInfo; const R: TRect);
begin
  if HorzLineWidth > 0 then
    ARowViewInfo.ValuesLinesInfo.Add(R.Left, R.Bottom, R.Right - R.Left, HorzLineWidth, HorzLineBrush);
end;

procedure TcxvgCustomViewInfo.AddRightValueSide(ARowViewInfo: TcxCustomRowViewInfo; const R: TRect; ALast: Boolean);
begin
  if VertLineWidth > 0 then
    if ALast then
      ARowViewInfo.ValuesLinesInfo.Add(R.Right, R.Top, VertLineWidth, R.Bottom - R.Top + HorzLineWidth, BandBorderColor)
    else
      ARowViewInfo.ValuesLinesInfo.Add(R.Right, R.Top, VertLineWidth, R.Bottom - R.Top + HorzLineWidth, VertLineBrush);
end;

procedure TcxvgCustomViewInfo.CalcBandRects;
var
  ASumHeight: Integer;
  I: Integer;
begin
  ASumHeight := HorzLineWidth;
  for I := Scroller.TopVisibleRowIndex to VisibleRowCount - 1 do
    Inc(ASumHeight, VisibleRows[I].ViewInfo.CalculatedHeight + HorzLineWidth);
  CreateBand(ASumHeight, ViewBandWidth);
end;

procedure TcxvgCustomViewInfo.CalcBandRowsViewInfo(
  var AFirstRowIndex: Integer; const ABandRect: TRect;
  ABandIndex, ABandRowCount: Integer);
var
  ATop, AWidth, ABandRowIndex: Integer;
  ARow, ANextRow: TcxCustomRow;
  ARowRect: TRect;
  ARowViewInfo: TcxCustomRowViewInfo;
begin
  ABandRowIndex := 0;
  ATop := ABandRect.Top + HorzLineWidth;
  ARow := VisibleRows[AFirstRowIndex];
  AWidth := cxRectWidth(ABandRect) - 2 * VertLineWidth;
  if AWidth > 0 then
    while AFirstRowIndex < VisibleRowCount do
    begin
      if ARow = nil then break;
      ANextRow := VisibleRows[AFirstRowIndex + 1];
      ARowViewInfo := ARow.ViewInfo;
      ARowRect := cxRectBounds(ABandRect.Left + VertLineWidth, ATop, AWidth, ARowViewInfo.CalculatedHeight);
      if not CanAddRowToBand(ARowRect, ABandRect, ABandRowIndex) then break;
      ARowViewInfo.BandIndex := ABandIndex;
      ARowViewInfo.BandRowIndex := ABandRowIndex;
      ARowViewInfo.Calc(ARowRect, Self, ANextRow);
      RowsViewInfo.Add(ARowViewInfo);
      AddBandRowsLines(ABandRect);
      Inc(ATop, ARowViewInfo.CalculatedHeight + HorzLineWidth);
      ARow := ANextRow;
      Inc(AFirstRowIndex);
      Inc(ABandRowIndex);
    end;
end;

procedure TcxvgCustomViewInfo.CalcBandWidth;
begin
  FViewBandWidth := GetViewBandWidth;
  FViewHeaderWidth := GetViewHeaderWidth;
  FViewValueWidth := GetViewValueWidth;
end;

procedure TcxvgCustomViewInfo.CalculateBandsInfo;
begin
  PrepareCalculateBandsInfo;
  Scroller.RecalcBandsInfo;
end;

function TcxvgCustomViewInfo.CalculateClientRect: TRect;
begin
  Result := inherited CalculateClientRect;
  if FindPanelPosition = fppTop then
    Inc(Result.Top, FindPanelHeight)
  else
    Dec(Result.Bottom, FindPanelHeight);
  if FilterBoxPosition = fpTop then
    Inc(Result.Top, FilterBoxHeight)
  else
    Dec(Result.Bottom, FilterBoxHeight);
end;

procedure TcxvgCustomViewInfo.CalcEmpty;
begin
  if ShowHeaders then
    FCalcHelper.AddDivider(LinesInfo, cxRect(ClientRect.Left, ClientRect.Top, ClientRect.Left + ViewHeaderWidth, ClientRect.Bottom),
      clNone, False);
end;

procedure TcxvgCustomViewInfo.CalcCategoryExplorerStyle;
var
  ATheme: TdxTheme;
begin
  FUseCategoryExplorerStyle := VerticalGrid.OptionsView.CategoryExplorerStyle and
    AreVisualStylesAvailable([totExplorerBar]) and (VerticalGrid.LookAndFeel.SkinPainter = nil);

  if FUseCategoryExplorerStyle then
  begin
    ATheme := OpenTheme(totExplorerBar);
    if ATheme <> 0 then
    begin
      GetThemePartSize(ATheme, 0, EBP_NORMALGROUPEXPAND, EBNGE_NORMAL, nil, TS_TRUE, @FExplorerButtonSize);
      FExplorerButtonSize := ScaleFactor.Apply(FExplorerButtonSize);
    end
    else
      FUseCategoryExplorerStyle := False;
  end;
  if not FUseCategoryExplorerStyle then
    FExplorerButtonSize := cxSize(FButtonSize, FButtonSize);

  FExplorerButtonAreaSize := FExplorerButtonSize;
  dxAdjustToTouchableSize(FExplorerButtonAreaSize, ScaleFactor);
end;

procedure TcxvgCustomViewInfo.CalcLayoutGeneral;
begin
  with VerticalGrid do
  begin
    Rows.CheckList;

    FHorzLineWidth := Byte(OptionsView.GridLines in [vglHorizontal, vglBoth]);
    FVertLineWidth := Byte(OptionsView.GridLines in [vglVertical, vglBoth]);

    FClipRect := cxRectInflate(Self.ClientRect, -FVertLineWidth, -FHorzLineWidth);

    FBandsInterval := OptionsView.BandsInterval;
    FRowHeaderMinWidth := OptionsView.RowHeaderMinWidth;
    FShowHeaders := OptionsView.ShowHeaders;
    FButtonAreaSize := LookAndFeel.Painter.ScaledExpandButtonAreaSize(ScaleFactor);
    FButtonSize := LookAndFeel.Painter.ScaledExpandButtonSize(ScaleFactor);
    if (FVertLineWidth > 0) and FShowHeaders then
      DividerWidth := FCalcHelper.GetDividerWidth
    else
      DividerWidth := 0;
    FBandBorderColor := FCalcHelper.GetBandBorderColor;
    FRowIndentWidth := FCalcHelper.GetIndentWidth;
    FBandMinWidth := FRowHeaderMinWidth + OptionsView.ValueMinWidth + DividerWidth;
    if FShowHeaders then
      FFullHeaderWidth := GetViewHeaderWidth + DividerWidth
    else
      FFullHeaderWidth := 0;
    CreateBrushes;
    CalcCategoryExplorerStyle;
  end;
end;

procedure TcxvgCustomViewInfo.CalcRowsHeight;
var
  I: Integer;
  ARowViewInfo: TcxCustomRowViewInfo;
begin
  for I := Scroller.TopVisibleRowIndex to VisibleRowCount - 1 do
  begin
    ARowViewInfo := VisibleRows[I].ViewInfo;
    ARowViewInfo.InitValuesInfo;
    ARowViewInfo.CalculateHeight(ViewValueWidth);
  end;
end;

procedure TcxvgCustomViewInfo.CalcRowRects(ARowViewInfo: TcxCustomRowViewInfo);
begin
  if not CheckShowRowHeader(ARowViewInfo) then Exit;
  with ARowViewInfo do
  begin
    FHeaderInfo.HeaderRect := cxRect(RowRect.Left, RowRect.Top, RowRect.Left + ViewHeaderWidth, RowRect.Bottom);
    FValuesRect := cxRect(RowRect.Left + ViewHeaderWidth + DividerWidth, RowRect.Top, RowRect.Right, RowRect.Bottom);
  end;
end;

procedure TcxvgCustomViewInfo.CalcRowsViewInfo;
var
  ARowIndex, ABandIndex, ALeftVisibleBand: Integer;
  R: TRect;
begin
  with Scroller do
  begin
    ARowIndex := TopVisibleRowIndex;
    ALeftVisibleBand := LeftVisibleBand;
    for ABandIndex := ALeftVisibleBand to BandsInfo.Count - 1 do
    begin
      if ABandIndex - ALeftVisibleBand > ViewRects.BandRects.Count - 1 then break;
      R := ViewRects.BandRects[ABandIndex - ALeftVisibleBand];
      if R.Left > ClientRect.Right then
        break
      else
        CalcBandRowsViewInfo(ARowIndex, R, ABandIndex, BandsInfo[ABandIndex].RowsCount);
    end;
  end;
  if VisibleRowCount = 0 then
    CalcEmpty;
end;

procedure TcxvgCustomViewInfo.CalcViewRects;
var
  R: TRect;
begin
  ViewRects.Clear;
  if VisibleRowCount = 0 then
  begin
    // make empty band
    R := ClientRect;
    R.Bottom := R.Top;
    ViewRects.BandRects.Add(R);
    ViewRects.EmptyRects.Add(ClientRect);
  end
  else
  begin
    CalcBandRects;
    AddEmptyRects;
  end;
end;

function TcxvgCustomViewInfo.CanAddRowToBand(const ARowRect, ABandRect: TRect; ABandRowIndex: Integer): Boolean;
begin
  Result := ARowRect.Top < ABandRect.Bottom;
end;

function TcxvgCustomViewInfo.CanUpdateRecord(ARecordIndex: Integer): Boolean;
begin
  Result := (ARecordIndex >= 0) and not VerticalGrid.IsLocked;
end;

procedure TcxvgCustomViewInfo.ChangeScale(M, D: Integer);
begin
  ViewBandWidth := MulDiv(ViewBandWidth, M, D);
  ViewHeaderWidth := MulDiv(ViewHeaderWidth, M, D);
  ViewValueWidth := MulDiv(ViewValueWidth, M, D);
end;

procedure TcxvgCustomViewInfo.CheckBiDiMode;
begin
  if UseRightToLeftAlignment and not FIsRightToLeftConverted then
  begin
    FRowsViewInfo.RightToLeftConversion(Bounds);
    FLinesInfo.RightToLeftConversion(Bounds);
    FFocusLinesInfo.RightToLeftConversion(Bounds);
    ViewRects.RightToLeftConversion(Bounds);
    FindPanelViewInfo.RightToLeftConversion(Bounds);
    FilterBoxViewInfo.RightToLeftConversion(Bounds);
    FClipRect := TdxRightToLeftLayoutConverter.ConvertRect(FClipRect, Bounds);
    NavigatorRightToLeftConversion;
    FIsRightToLeftConverted := True;
  end;
end;

procedure TcxvgCustomViewInfo.CheckMaxRowHeaderWidth(var Value: Integer; AValueMinWidth: Integer);
begin
  // do nothing
end;

procedure TcxvgCustomViewInfo.CheckRowHeaderWidth;
var
  ARowHeaderWidth: Integer;
begin
  if FLockDividerPos then Exit;
  ARowHeaderWidth := VerticalGrid.OptionsView.RowHeaderWidth;
  if not VerticalGrid.IsLoading then
    CheckMaxRowHeaderWidth(ARowHeaderWidth, Max(VerticalGrid.OptionsView.ValueMinWidth, 4));
  ARowHeaderWidth := Max(GetViewMinHeaderWidth, ARowHeaderWidth);
  VerticalGrid.OptionsView.RowHeaderWidth := ARowHeaderWidth;
end;

function TcxvgCustomViewInfo.CheckShowRowHeader(ARowViewInfo: TcxCustomRowViewInfo): Boolean;
begin
  Result := ShowHeaders;
  if not Result then
  begin
    with ARowViewInfo.FHeaderInfo do
    begin
      FHeaderRect := ARowViewInfo.FRowRect;
      FHeaderRect.Right := FHeaderRect.Left;
    end;
    ARowViewInfo.FValuesRect := ARowViewInfo.FRowRect;
  end;
end;

procedure TcxvgCustomViewInfo.CreateBand(ABandHeight,
  ABandWidth: Integer);
var
  ALeft, AClientHeight: Integer;
  R: TRect;
begin
  if ViewRects.BandRects.Count = 0 then
    ALeft := ClientRect.Left
  else
    ALeft := ViewRects.BandRects.Rects[ViewRects.BandRects.Count - 1].Right;
  AClientHeight := cxRectHeight(ClientRect);
  R := cxRectBounds(ALeft, ClientRect.Top, ABandWidth, Min(ABandHeight, AClientHeight));
  ViewRects.BandRects.Add(R);
  if R.Bottom - R.Top < AClientHeight then
    ViewRects.EmptyRects.Add(cxRectBounds(R.Left, R.Bottom, ABandWidth, AClientHeight - (R.Bottom - R.Top)));
end;

procedure TcxvgCustomViewInfo.CreateBrushes;
begin
  DestroyBrushes;
  FHorzLineBrush := FCalcHelper.CreateHorzLineBrush;
  FVertLineBrush := FCalcHelper.CreateVertLineBrush;
end;

function TcxvgCustomViewInfo.CreateCalcHelper: TcxvgCustomPaintStyleCalcHelper;
begin
  Result := VerticalGrid.GetCalcHelperClass.Create(Self);
end;

procedure TcxvgCustomViewInfo.DestroyBrushes;

  procedure DestroyBrush(var ABrush: TBrush);
  begin
    if ABrush <> nil then
    begin
      if ABrush.Bitmap <> nil then
        ABrush.Bitmap.Free;
      ABrush.Free;
      ABrush := nil;
    end;
  end;

begin
  DestroyBrush(FHorzLineBrush);
  DestroyBrush(FVertLineBrush);
end;

function TcxvgCustomViewInfo.GetBandSizeableRect(const ABandRect: TRect): TRect;
begin
  if not IsRightToLeftConverted then
    Result := cxRect(ABandRect.Right - 1, ABandRect.Top, ABandRect.Right + 2, ClientRect.Bottom)
  else
    Result := cxRect(ABandRect.Left - 2, ABandRect.Top, ABandRect.Left + 1, ClientRect.Bottom);
end;

function TcxvgCustomViewInfo.GetRowAutoHeight(ARow: TcxCustomRow): Boolean;
begin
  Result := VerticalGrid.OptionsView.CellAutoHeight;
  if ARow <> nil then Result := Result and ARow.Options.CanAutoHeight and
    not ARow.IsHeightAssigned;
end;

function TcxvgCustomViewInfo.GetPixelScrollContentSize: Integer;
begin
  Result := 0;
end;

function TcxvgCustomViewInfo.GetViewBandWidth: Integer;
begin
  Result := ClientRect.Right - ClientRect.Left - Scroller.ScrollStrategy.GetVScrollbarAreaWidth;
end;

function TcxvgCustomViewInfo.GetViewHeaderWidth: Integer;
begin
  if ShowHeaders then
    Result := Max(GetViewMinHeaderWidth, VerticalGrid.OptionsView.RowHeaderWidth)
  else
    Result := 0;
end;

function TcxvgCustomViewInfo.GetViewMinHeaderWidth: Integer;
begin
  Result := 1 + (RowIndentWidth + VertLineWidth) * (MaxVisibleLevel + 1);
end;

function TcxvgCustomViewInfo.GetViewValueWidth: Integer;
begin
  Result := VerticalGrid.OptionsView.ValueWidth;
end;

procedure TcxvgCustomViewInfo.LayoutStyleChanged;
begin
  if VerticalGrid.SubClassesCreated then
    Scroller.LayoutStyleChanged;
end;

procedure TcxvgCustomViewInfo.PrepareCalculateBandsInfo;
begin
  Reset;
  ClearValuesInfo;
  CalcRowsHeight;
  CalcBandWidth;
end;

procedure TcxvgCustomViewInfo.Reset;
begin
end;

function TcxvgCustomViewInfo.ScaleRowRects(ARowViewInfo: TcxCustomRowViewInfo): TRectScaler;
begin
  Result := TRectScaler.Create;
  with Result do
  begin
    Add(ViewHeaderWidth, RowHeaderMinWidth, DividerWidth);
    Add(ViewBandWidth - ViewHeaderWidth, BandMinWidth);
    ScaleRect(ARowViewInfo.RowRect);
  end;
end;

procedure TcxvgCustomViewInfo.SetDividerPos(APos: Integer);
var
  AValueWidth: Integer;
begin
  with VerticalGrid.OptionsView do
    if APos <> RowHeaderWidth then
    begin
      AValueWidth := ViewBandWidth - (APos + DividerWidth);
      if AValueWidth < ValueMinWidth then
        APos := ViewBandWidth - ValueMinWidth;
      RowHeaderWidth := APos;
    end;
end;

procedure TcxvgCustomViewInfo.SetValueWidth(AWidth: Integer);
begin
  VerticalGrid.OptionsView.ValueWidth := AWidth;
end;

function TcxvgCustomViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := VerticalGrid.UseRightToLeftAlignment;
end;

function TcxvgCustomViewInfo.UseRightToLeftReading: Boolean;
begin
  Result := VerticalGrid.UseRightToLeftReading;
end;

function TcxvgCustomViewInfo.UseRightToLeftScrollBar: Boolean;
begin
  Result := VerticalGrid.UseRightToLeftScrollBar;
end;

procedure TcxvgCustomViewInfo.Clear;
begin
  ClearLinesAndRows;
  ViewRects.Clear;
end;

procedure TcxvgCustomViewInfo.ClearValuesInfo;
var
  I: Integer;
begin
  for I := 0 to VisibleRowCount - 1 do
    VisibleRows[I].ViewInfo.ClearValuesInfo;
end;

procedure TcxvgCustomViewInfo.ClearLinesAndRows;
begin
  FLinesInfo.Clear;
  FFocusLinesInfo.Clear;
  FRowsViewInfo.Clear;
end;

function TcxvgCustomViewInfo.GetBandInfo: TBandInfoList;
begin
  Result := Scroller.ScrollStrategy.BandsInfo;
end;

function TcxvgCustomViewInfo.GetFirstVisibleRecordIndex: Integer;
begin
  if VerticalGrid.RecordCount = 0 then
    Result := -1
  else
    Result := Scroller.LeftVisibleRecord;
end;

function TcxvgCustomViewInfo.GetMaxVisibleLevel: Integer;
begin
  Result := VerticalGrid.Rows.MaxVisibleLevel;
end;

function TcxvgCustomViewInfo.GetMinRowHeight: Integer;
var
  ARowHeight: Integer;
begin
  if VerticalGrid.Images <> nil then
    FImageSize := dxGetImageSize(VerticalGrid.Images, ScaleFactor)
  else
    FImageSize := cxNullSize;

  Result := cxTextHeight(VerticalGrid.Font) + 2 * ScaleFactor.Apply(cxTextOffset);
  if FImageSize.cy + 2 > Result then
    Result := FImageSize.cy + 2;
  ARowHeight := VerticalGrid.OptionsView.RowHeight;
  if (ARowHeight >= 0) and (ARowHeight > Result) then
    Result := ARowHeight;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxvgCustomViewInfo.GetPainter: TcxvgPainter;
begin
  Result := TcxvgPainter(inherited Painter);
end;

function TcxvgCustomViewInfo.GetScroller: TcxvgScroller;
begin
  Result := VerticalGrid.Controller.Scroller;
end;

function TcxvgCustomViewInfo.GetVisibleRow(
  Index: Integer): TcxCustomRow;
begin
  if (Index >= 0) and (Index < VisibleRowCount) then
    Result := VerticalGrid.Rows.VisibleRows[Index]
  else
    Result := nil;
end;

function TcxvgCustomViewInfo.GetVisibleRowCount: Integer;
begin
  Result := VerticalGrid.Rows.VisibleRowCount;
end;

procedure TcxvgCustomViewInfo.UpdateScroller;
begin
  Scroller.RecreateScrollStrategy;
end;

{ TcxvgHitTest }

function TcxvgHitTest.AllowDesignMouseEvents(X, Y: Integer;
  AShift: TShiftState): Boolean;
begin
  // we must use GetAsyncKeyState
  if (GetAsyncKeyState(VK_CONTROL) < 0) or (GetAsyncKeyState(VK_MENU) < 0) then
  begin
    Result := False;
    Exit;
  end;
  RecalculateOnMouseEvent(X, Y, AShift);
  Result := CanSizing or HitAtRowHeader;
end;

procedure TcxvgHitTest.CalcBandsHitTest(AViewInfo: TcxvgCustomViewInfo);
var
  I: Integer;
  R, CR: TRect;
  ABandSizing, AHasDivider: Boolean;
begin
  with AViewInfo do
  begin
    ABandSizing := VerticalGrid.CanBandSizing;
    AHasDivider := (VerticalGrid.OptionsBehavior.HeaderSizing or
      VerticalGrid.IsDesigning) and ShowHeaders;
    CR := ClientRect;
    for I := 0 to ViewRects.BandRects.Count - 1 do
    begin
      R := ViewRects.BandRects[I];
      if AHasDivider and (I = 0) then
      begin
        // calc divider pos
        if not AViewInfo.IsRightToLeftConverted then
        begin
          CR.Left  := R.Left  + ViewHeaderWidth;
          CR.Right := CR.Left + DividerWidth + 2;
        end
        else
        begin
          CR.Right := R.Right - ViewHeaderWidth;
          CR.Left  := CR.Right - DividerWidth - 2;
        end;
        if Check(CR) then SetHitState(vghc_HitAtDivider, True);
      end;
      // calc first band right side
      if (I = 0) and ABandSizing and Check(GetBandSizeableRect(R)) then
        SetHitState(vghc_HitAtBandSizing, True);
      if Check(R) then
      begin
        FHitBandIndex := I;
        break;
      end;
    end;
  end;
  CalcRowsHitTest(AViewInfo);
end;

function TcxvgHitTest.CalcCustomizingHitTest: Boolean;
begin
  Result := VerticalGrid.Customizing.CanDrop(VerticalGrid.ClientToScreen(HitPoint));
  if Result then
    SetHitState(vghc_HitAtCustomize, True);
end;

procedure TcxvgHitTest.CalcFilterBoxHitTest;
begin
  if ViewInfo.IsFilterBoxVisible and PtInRect(ViewInfo.FilterBoxViewInfo.BoundsRect, HitPoint) then
  begin
    FNewHitTestItem := ViewInfo.FilterBoxViewInfo;
    SetHitState(echc_HitAtFilterBox, True);
  end;
end;

procedure TcxvgHitTest.CalcFindPanelHitTest;
begin
  if ViewInfo.IsFindPanelVisible and PtInRect(ViewInfo.FindPanelViewInfo.BoundsRect, HitPoint) then
  begin
    FNewHitTestItem := ViewInfo.FindPanelViewInfo;
    SetHitState(echc_HitAtFindPanel, True);
  end;
end;

function TcxvgHitTest.CalcNavigatorHitTest: Boolean;
begin
  Result := PtInRect(ViewInfo.NavigatorSiteViewInfo.BoundsRect, HitPoint);
  if Result then
  begin
    FNewHitTestItem := ViewInfo.NavigatorSiteViewInfo;
    SetHitState(vghc_HitAtNavigator, True);
  end;
end;

function TcxvgHitTest.CalcRowHeaderHitTest(AHeaderInfo: TcxCustomRowHeaderInfo): Boolean;
var
  I: Integer;
  ACaptionInfo: TcxRowCaptionInfo;
begin
  with AHeaderInfo do
  begin
    Result := Check(HeaderRect);
    if not Result then Exit;
    if Check(FButtonAreaBounds) then
      SetHitState(vghc_HitAtButton, True)
    else
      if Check(FHeaderCellsRect) then
        for I := 0 to CaptionsInfo.Count - 1 do
        begin
          ACaptionInfo := CaptionsInfo[I];
          with ACaptionInfo do
          begin
            if Check(ImageRect) then
            begin
              FHitCellIndex := I;
              SetHitState(vghc_HitAtImage, True);
            end
            else
              if Check(FilterButtonRect) then
              begin
                FNewHitTestItem := ACaptionInfo;
                FHitCellIndex := I;
                SetHitState(vghc_HitAtCaptionFilterButton, True);
              end
              else
                if Check(CaptionRect) then
                begin
                  FNewHitTestItem := ACaptionInfo;
                  FHitCellIndex := I;
                  SetHitState(vghc_HitAtCaption, True);
                end;
          end;
        end
      else
        SetHitState(vghc_HitAtIndent, True);
  end;
end;

function TcxvgHitTest.CalcRowHitTest(
  ARowViewInfo: TcxCustomRowViewInfo): Boolean;
begin
  with ARowViewInfo do
  begin
    Result := Check(cxRect(RowRect.Left, RowRect.Top, RowRect.Right, RowRect.Bottom + ViewInfo.HorzLineWidth));
    if not Result then Exit;
    FHitRow := Row;
    if not CalcRowHeaderHitTest(HeaderInfo) and Check(ValuesRect) then
      CalcRowValuesHitTest(ARowViewInfo)
  end;
end;

procedure TcxvgHitTest.CalcRowValuesHitTest(
  ARowViewInfo: TcxCustomRowViewInfo);
var
  I: Integer;
  AValueInfo: TcxRowValueInfo;
  AUseValue: Boolean;
begin
  AUseValue := TcxvgController(Controller).ResizeKind = rkNone;
  with ARowViewInfo do
    for I := 0 to ValuesInfo.Count - 1 do
    begin
      AValueInfo := ValuesInfo[I];
      if Check(AValueInfo.VisibleRect) then
      begin
        if AUseValue then
        begin
          FNewHitTestItem := AValueInfo;
          FHitCellIndex := AValueInfo.RowCellIndex;
        end;
        SetHitState(vghc_HitAtValue, True);
        break;
      end;
    end;
end;

procedure TcxvgHitTest.CalcRowsHitTest(
  AViewInfo: TcxvgCustomViewInfo);
var
  I: Integer;
  ARowSizing: Boolean;
  ARowViewInfo: TcxCustomRowViewInfo;
  R: TRect;
begin
  with AViewInfo do
  begin
    ARowSizing := VerticalGrid.OptionsBehavior.RowSizing;
    for I := 0 to RowsViewInfo.Count - 1 do
    begin
      ARowViewInfo := RowsViewInfo[I];
      if ARowSizing and ARowViewInfo.Row.Options.CanResized then
      begin
        R := ARowViewInfo.RowRect;
        if Check(cxRect(R.Left, R.Bottom - 1, R.Right, Min(R.Bottom + 1, ClientRect.Bottom))) then
        begin
          FHitRow := ARowViewInfo.Row;
          SetHitState(vghc_HitAtRowSizing, True);
          Exit;
        end;
      end;
      if CalcRowHitTest(ARowViewInfo) then Exit;
    end;
    SetHitState(vghc_HitAtEmpty, True);
  end;
end;

function TcxvgHitTest.CanMoving: Boolean;
var
  ADirection: TcxDragSizingDirection;
begin
  Result := ((VerticalGrid.DragMode = dmAutomatic)) and (HitRow <> nil) and
    HitRow.Options.Moving and HitAtCaption and not CanSizing(ADirection);
end;

function TcxvgHitTest.CanSizing: Boolean;
var
  ADirection: TcxDragSizingDirection;
begin
  Result := CanSizing(ADirection);
end;

function TcxvgHitTest.CanSizing(var ASizeDirection: TcxDragSizingDirection): Boolean;
begin
  Result := HitAtRowSizing or HitAtBandSizing or HitAtDivider;
  if not Result then Exit;
  if HitAtRowSizing then
    ASizeDirection := dsdVert
  else
    ASizeDirection := dsdHorz;
end;

function TcxvgHitTest.Check(const ARect: TRect): Boolean;
begin
  Result := cxRectPtIn(ARect, HitPoint);
end;

procedure TcxvgHitTest.DoCalculate;
begin
  FHitRow := nil;
  FNewHitTestItem := nil;
  FHitBandIndex := -1;
  FHitCellIndex := -1;
  FHitState := FHitState and 3;
  FHitInControl := Check(VerticalGrid.ClientBounds);
  if not CalcCustomizingHitTest and FHitInControl then
    CalcBandsHitTest(VerticalGrid.ViewInfo);
  CalcNavigatorHitTest;
  CalcFindPanelHitTest;
  CalcFilterBoxHitTest;
  HitTestItem := FNewHitTestItem;
end;

function TcxvgHitTest.GetCurrentCursor: TCursor;
var
  ADirection: TcxDragSizingDirection;
  P: TPoint;
const
  ACursors: array[TcxDragSizingDirection] of TCursor = (crHSplit, crVSplit);
begin
  Result := inherited GetCurrentCursor;
  if (VerticalGrid.DragAndDropState <> ddsNone) and
    not (VerticalGrid.DragAndDropObject is TcxSizingDragAndDropObject) then Exit;
  if Result = crDefault then
    if CanSizing(ADirection) then
      Result := ACursors[ADirection]
    else
      if (VerticalGrid.DragAndDropState = ddsNone) and IsItemEditCell and HitRow.CanFocus then
      begin
        P := cxPointOffset(VerticalGrid.ScreenToClient(GetMouseCursorPos), EditCellViewInfo.ContentRect.TopLeft, False);
        Result := TcxCustomEditViewInfoAccess(EditCellViewInfo.EditViewInfo).GetCurrentCursor(P);
      end;
end;

function TcxvgHitTest.GetHitAtFilterBox: Boolean;
begin
  Result := GetState(echc_HitAtFilterBox);
end;

function TcxvgHitTest.GetHitAtFindPanel: Boolean;
begin
  Result := GetState(echc_HitAtFindPanel);
end;

function TcxvgHitTest.GetHitAtNavigator: Boolean;
begin
  Result := GetState(vghc_HitAtNavigator);
end;

function TcxvgHitTest.GetState(Index: Integer): Boolean;
begin
  Result := (FHitState and (1 shl Index)) <> 0;
end;

procedure TcxvgHitTest.SetHitState(Index: Integer; Value: Boolean);
begin
  if Value then
    FHitState := FHitState or (1 shl Index)
  else
    FHitState := FHitState and not (1 shl Index);
end;

function TcxvgHitTest.GetHitAtRowHeader: Boolean;
begin
  Result := HitAtCaption or HitAtIndent or HitAtImage or HitAtButton or HitAtCaptionFilterButton;
end;

function TcxvgHitTest.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  // reduce calls
  Result := TcxCustomVerticalGrid(TcxvgController(Controller).EditingControl);
end;

function TcxvgHitTest.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := TcxvgCustomViewInfo(inherited ViewInfo);
end;

{ TcxCustomVerticalGrid }

constructor TcxCustomVerticalGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConditionalFormattingProvider := TcxVerticalGridConditionalFormattingProvider.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FCategoryFont := TFont.Create;
  FCategoryFont.Style := [fsBold];
  FCategoryFont.OnChange := CategoryFontChanged;
  SetBounds(Left, Top, 150, 200);
end;

destructor TcxCustomVerticalGrid.Destroy;
begin
  DataController.Cancel;
  BeginUpdate;
  Customizing.Visible := False;
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCategoryFont);
  FreeAndNil(FConditionalFormattingProvider);
  inherited Destroy;
end;

procedure TcxCustomVerticalGrid.Update;
begin
  if not IsLocked then
    inherited Update;
end;

function TcxCustomVerticalGrid.Add(ARowClass: TcxCustomRowClass): TcxCustomRow;
begin
  CheckRowClass(ARowClass);
  Result := ARowClass.Create(Owner);
  Result.SetVerticalGrid(Self);
  FRows.Add(Result);
  LayoutChanged;
end;

function TcxCustomVerticalGrid.AddChild(AParent: TcxCustomRow;
  ARowClass: TcxCustomRowClass): TcxCustomRow;
begin
  if AParent = nil then
  begin
    Result := Add(ARowClass);
    Exit;
  end;
  CheckRowClass(ARowClass);
  Result := ARowClass.Create(Owner);
  Result.SetVerticalGrid(Self);
  FRows.AddChild(AParent, Result);
  LayoutChanged;
end;

procedure TcxCustomVerticalGrid.AssignRows(Source: TcxCustomVerticalGrid);
begin
  if Source <> nil then
    Rows.AssignRows(Source.Rows);
end;

procedure TcxCustomVerticalGrid.ClearRows;
begin
  FClearingRows := True;
  BeginUpdate;
  try
    DataController.BeginUpdateFields;
    try
      Controller.Clear;
      Rows.Clear;
    finally
      DataController.EndUpdateFields;
    end;
  finally
    EndUpdate;
    FClearingRows := False;
  end;
end;

function TcxCustomVerticalGrid.FirstRow: TcxCustomRow;
begin
  if HasRows then Result := FRootRow.Rows[0] else Result := nil;
end;

function TcxCustomVerticalGrid.FirstVisibleRow: TcxCustomRow;
var
  I: Integer;
begin
  Result := nil;
  Rows.CheckList;
  for I := 0 to FRootRow.Count - 1 do
    if FRootRow.Rows[I].Visible then
    begin
      Result := FRootRow.Rows[I];
      break;
    end;
end;

procedure TcxCustomVerticalGrid.FocusRow(ARow: TcxCustomRow;
  ACellIndex: Integer = 0);
begin
  with Controller do
  begin
    SetFocusedRowAndCell(ARow, ACellIndex);
    MakeFocusedItemVisible;
  end;
end;

procedure TcxCustomVerticalGrid.FullCollapse;
var
  I: Integer;
begin
  if HasRows then
  begin
    BeginUpdate;
    try
      for I := 0 to FRootRow.Count - 1 do
        FRootRow.Rows[I].Collapse(True);
    finally
      EndUpdate;
    end;
  end
end;

procedure TcxCustomVerticalGrid.FullExpand;
begin
  if HasRows then
  begin
    BeginUpdate;
    try
      FRootRow.Expand(True);
    finally
      EndUpdate;
    end;
  end
end;

function TcxCustomVerticalGrid.HasRows: Boolean;
begin
  Result := FRootRow.Count > 0
end;

function TcxCustomVerticalGrid.HasVisibleRows: Boolean;
begin
  Result := FirstVisibleRow <> nil;
end;

function TcxCustomVerticalGrid.IsInternalDragging(ADragObject: TObject): Boolean;
begin
  Result := Controller.IsInternalDragging(ADragObject);
end;

function TcxCustomVerticalGrid.IsRowVisible(ARow: TcxCustomRow): Boolean;
begin
  Result := False;
  Rows.CheckList;
  if ARow.Visible then
    repeat
      ARow := ARow.FParent;
      if (ARow = FRootRow) then
      begin
        Result := True;
        Exit;
      end
      else
        if not (ARow.Expanded and ARow.Visible) then Exit;
    until False;
end;

function TcxCustomVerticalGrid.LastRow: TcxCustomRow;
begin
  if Rows.Count > 0 then
    Result := Rows.GetRow(Rows.Count - 1)
  else
    Result := nil;
end;

function TcxCustomVerticalGrid.LastVisibleRow: TcxCustomRow;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  Result := nil;
  for I := Rows.Count - 1 downto 0 do
  begin
    ARow := Rows.GetRow(I);
    if IsRowVisible(ARow) then
    begin
      Result := ARow;
      break;
    end;
  end;
end;

function TcxCustomVerticalGrid.NextRow(ARow: TcxCustomRow): TcxCustomRow;
var
  AIndex: Integer;
begin
  Rows.CheckList;
  if ARow = nil then
    Result := FirstRow
  else
  begin
    AIndex := Rows.FList.IndexOf(ARow) + 1;
    if (AIndex > 0) and (AIndex < Rows.Count) then
      Result := Rows[AIndex]
    else
      Result := nil;
  end;
end;

function TcxCustomVerticalGrid.NextVisibleRow(
  ARow: TcxCustomRow): TcxCustomRow;
var
  I, AIndex: Integer;
begin
  Rows.CheckList;
  if ARow = nil then
    Result := FirstVisibleRow
  else
  begin
    Result := nil;
    AIndex := Rows.FList.IndexOf(ARow) + 1;
    if AIndex > 0 then
      for I := AIndex to Rows.Count - 1 do
      begin
        ARow := Rows[I];
        if IsRowVisible(ARow) then
        begin
          Result := ARow;
          break;
        end;
      end;
  end;
end;

function TcxCustomVerticalGrid.PrevRow(ARow: TcxCustomRow): TcxCustomRow;
var
  AIndex: Integer;
begin
  Rows.CheckList;
  if ARow = nil then
    Result := LastRow
  else
  begin
    AIndex := Rows.FList.IndexOf(ARow);
    if (AIndex > 0) then Result := Rows[AIndex - 1] else Result := nil;
  end;
end;

function TcxCustomVerticalGrid.PrevVisibleRow(ARow: TcxCustomRow): TcxCustomRow;
var
  I, AIndex: Integer;
begin
  Rows.CheckList;
  if ARow = nil then
    Result := LastVisibleRow
  else
  begin
    Result := nil;
    AIndex := Rows.FList.IndexOf(ARow);
    if AIndex > 0 then
      for I := AIndex - 1 downto 0 do
      begin
        ARow := Rows[I];
        if IsRowVisible(ARow) then
        begin
          Result := ARow;
          break;
        end;
      end;
  end;
end;

procedure TcxCustomVerticalGrid.Remove(ARow: TcxCustomRow);
begin
  FreeAndNil(ARow);
end;

function TcxCustomVerticalGrid.RowByCaption(const ACaption: string): TcxCustomRow;

  function CheckCaption(ARow: TcxCustomRow): Boolean;
  var
    I: Integer;
  begin
    if ARow.FProperties is TcxCaptionRowProperties then
      with TcxCaptionRowProperties(ARow.FProperties) do
        Result := CompareText(Caption, ACaption) = 0
    else
    begin
      Result := False;
      if ARow.FProperties is TcxMultiEditorRowProperties then
        with TcxMultiEditorRowProperties(ARow.FProperties) do
          for I := 0 to Editors.Count - 1 do
            if CompareText(Editors[I].Caption, ACaption) = 0 then
            begin
              Result := True;
              break
            end;
    end;
  end;

var
  I: Integer;
  ARow: TcxCustomRow;
begin
  Result := nil;
  for I := 0 to Rows.Count - 1 do
  begin
    ARow := Rows[I];
    if CheckCaption(ARow) then
    begin
      Result := ARow;
      break;
    end;
  end;
end;

function TcxCustomVerticalGrid.RowByName(const AName: string): TcxCustomRow;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  Result := nil;
  for I := 0 to Rows.Count - 1 do
  begin
    ARow := Rows[I];
    if CompareText(ARow.Name, AName) = 0 then
    begin
      Result := ARow;
      break;
    end;
  end;
end;

procedure TcxCustomVerticalGrid.RestoreFromIniFile(const AStorageName: string);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := False;
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    BeforeLoadedRows;
    try
      AStorage.Modes := [smChildrenCreating];
      AStorage.RestoreFromIni(Self);
      AfterLoadedRows;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.RestoreFromRegistry(const AStorageName: string);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := False;
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    BeforeLoadedRows;
    try
      AStorage.Modes := [smChildrenCreating];
      AStorage.RestoreFromRegistry(Self);
      AfterLoadedRows;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.RestoreFromStream(AStream: TStream);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := False;
  AStorage := TcxStorage.Create(AStream);
  try
    BeginUpdate;
    BeforeLoadedRows;
    try
      AStorage.Modes := [smChildrenCreating];
      AStorage.RestoreFromStream(Self);
      AfterLoadedRows;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.StoreToIniFile(const AStorageName: string;
  AReCreate: Boolean = True);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := True;
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.ReCreate := AReCreate;
      AStorage.StoreToIni(Self);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.StoreToRegistry(const AStorageName: string;
  AReCreate: Boolean = True);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := True;
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.ReCreate := AReCreate;
      AStorage.StoreToRegistry(Self);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.StoreToStream(AStream: TStream);
var
  AStorage: TcxStorage;
begin
  FNewLoadMode := True;
  AStorage := TcxStorage.Create(AStream);
  try
    BeginUpdate;
    try
      AStorage.StoreToStream(Self);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomVerticalGrid.CancelEdit;
begin
  Controller.EditingController.HideEdit(False);
end;

procedure TcxCustomVerticalGrid.HideEdit;
begin
  Controller.EditingController.HideEdit(True);
end;

procedure TcxCustomVerticalGrid.ShowEdit;
begin
  if not OptionsData.Editing then Exit;
  DataController.Edit;
  if CanFocusEx then SetFocus;
  Controller.EditingController.ShowEdit;
end;

procedure TcxCustomVerticalGrid.ShowEditByKey(AKey: Char);
begin
  if not OptionsData.Editing then Exit;
  DataController.Edit;
  if CanFocusEx then SetFocus;
  with Controller do
    EditingController.ShowEdit(FocusedItem, AKey);
end;

procedure TcxCustomVerticalGrid.ShowEditByMouse(X, Y: Integer; AShift: TShiftState);
begin
  if not OptionsData.Editing then Exit;
  DataController.Edit;
  if CanFocusEx then SetFocus;
  with Controller do
    EditingController.ShowEdit(FocusedItem, AShift, X, Y);
end;

procedure TcxCustomVerticalGrid.ApplyFindFilterText(const AText: string);
begin
  Controller.ApplyFindFilterText(AText);
end;

procedure TcxCustomVerticalGrid.ClearFindFilterText;
begin
  Controller.ClearFindFilterText;
end;

function TcxCustomVerticalGrid.GetFindFilterText: string;
begin
  Result := Controller.GetFindFilterText;
end;

procedure TcxCustomVerticalGrid.HideFindPanel;
begin
  Controller.HideFindPanel;
end;

function TcxCustomVerticalGrid.IsFindPanelVisible: Boolean;
begin
  Result := Controller.IsFindPanelVisible;
end;

procedure TcxCustomVerticalGrid.ShowFindPanel;
begin
  Controller.ShowFindPanel;
end;

procedure TcxCustomVerticalGrid.RestoreDefaults;
begin
  BeginUpdate;
  try
    OptionsBehavior.RestoreDefaults;
    OptionsView.RestoreDefaults;
    Rows.RestoreDefaults;
  finally
    EndUpdate;
  end;
  Modified;
end;

function TcxCustomVerticalGrid.GetObjectName: string;
begin
  if FStoringName <> '' then
    Result := FStoringName
  else
    Result := Name;
end;

function TcxCustomVerticalGrid.GetProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('PaintStyle');
    Add('HeaderWidth');
    Add('ValueWidth');
    Add('NewLoad');
  end;
  Result := True;
end;

procedure TcxCustomVerticalGrid.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'PaintStyle' then
    AValue := OptionsView.PaintStyle
  else
    if AName = 'HeaderWidth' then
      AValue := OptionsView.RowHeaderWidth
    else
      if AName = 'ValueWidth' then
        AValue := OptionsView.ValueWidth
      else
        if AName = 'NewLoad' then
          AValue := 1
        else
          AValue := Null;
end;

procedure TcxCustomVerticalGrid.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'PaintStyle' then
    OptionsView.PaintStyle := AValue
  else
    if AName = 'HeaderWidth' then
      OptionsView.RowHeaderWidth := AValue
    else
      if AName = 'ValueWidth' then
        OptionsView.ValueWidth := AValue
      else
        if AName = 'NewLoad' then
          FNewLoadMode := True;
end;

function TcxCustomVerticalGrid.CreateChild(
  const AObjectName, AClassName: string): TObject;
var
  ARow: TcxCustomRow;
begin
  if CompareText(AClassName, 'TcxCategoryRow') = 0 then
  begin
    ARow := Add(TcxCategoryRow);
    ARow.Name := CreateUniqueName(Owner, Self, ARow, 'Tcx', 'RuntimeCreatedRow');
    Result := ARow;
  end
  else
    Result := nil;
end;

procedure TcxCustomVerticalGrid.DeleteChild(const AObjectName: string;
  AObject: TObject);
begin
end;

procedure TcxCustomVerticalGrid.GetStoredChildren(AChildren: TStringList);

  function GetStoredRowName(ARow: TcxCustomRow): string;
  begin
    if FNewLoadMode then
      Result := ARow.GetObjectName
    else
      Result := '';
  end;

var
  I: Integer;
begin
  with Rows do
    for I := 0 to Count - 1 do
      AChildren.AddObject(GetStoredRowName(Items[I]), Items[I]);

  AChildren.AddObject('ConditionalFormattingProvider', ConditionalFormattingProvider);
end;

function TcxCustomVerticalGrid.GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
begin
  Result := ConditionalFormattingProvider;
end;

procedure TcxCustomVerticalGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
  DragCursor := FSaveDragCursor;
end;

procedure TcxCustomVerticalGrid.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Version', ReadVersion, WriteVersion, True);
  ConditionalFormattingProvider.DefineBinaryProperty(Filer);
end;

procedure TcxCustomVerticalGrid.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Rows.Count - 1 do
    Proc(Rows[I]);
end;

procedure TcxCustomVerticalGrid.Loaded;
var
  ASaveLock: Boolean;
begin
  // need a lock for AutoScaleBands and changing visibility HScrollBar
  ASaveLock := ViewInfo.FLockDividerPos;
  ViewInfo.FLockDividerPos := True;
  try
    inherited Loaded;
    with OptionsBehavior do
      if not FAlwaysShowEditorAssigned then
        InternalSetAlwaysShowEditor(AlwaysShowEditor);
    ConditionalFormattingProvider.Loaded;
  finally
    ViewInfo.FLockDividerPos := ASaveLock;
    RestoreLayout;
    LayoutChanged;
  end;
end;

procedure TcxCustomVerticalGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

function cxVerticalGridGetItem(ACaller: TComponent;
  Index: Integer): TComponent;
begin
  Result := TcxCustomVerticalGrid(ACaller).Rows[Index];
end;

procedure TcxCustomVerticalGrid.SetName(const Value: TComponentName);
var
  AOldName: TComponentName;
begin
  AOldName := Name;
  inherited SetName(Value);
  if (Name <> AOldName) and (Name <> '') then
  begin
    RenameComponents(Self, Owner, Name, AOldName, Rows.Count, @cxVerticalGridGetItem);
    if not IsLoading and (Rows.Count > 0) then
      LayoutChanged;
  end;
end;

procedure TcxCustomVerticalGrid.BiDiModeChanged;
begin
  inherited BiDiModeChanged;
  Customizing.BiDiModeChanged;
end;

procedure TcxCustomVerticalGrid.BoundsChanged;
begin
  CheckLayoutRealign;
  inherited BoundsChanged;
  CheckGridModeBufferCount;
end;

procedure TcxCustomVerticalGrid.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
var
  I: Integer;
begin
  inherited ChangeScaleEx(M, D, isDpiChange);
  ViewInfo.ChangeScale(M, D);
  for I := 0 to Rows.Count - 1 do
    Rows[I].ChangeScale(M, D);
end;

procedure TcxCustomVerticalGrid.FontChanged;
begin
  Inc(FLockUpdate);
  try
    inherited FontChanged;
    FCategoryFont.Assign(Font);
    FCategoryFont.Style := Font.Style + [fsBold];
  finally
    Dec(FLockUpdate);
    LayoutChanged;
  end;
end;

procedure TcxCustomVerticalGrid.InitControl;
begin
  ViewInfo.FLockDividerPos := False;
  inherited InitControl;
end;

procedure TcxCustomVerticalGrid.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  ConditionalFormattingProvider.ClearCache;
  inherited LookAndFeelChanged(Sender, AChangedValues);
  PaintStyleChanged;
end;

function TcxCustomVerticalGrid.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TcxvgDragRowObject;
end;

procedure TcxCustomVerticalGrid.AfterLayoutChanged;
begin
  inherited AfterLayoutChanged;
  Controller.CheckEdit;
end;

procedure TcxCustomVerticalGrid.CreateSubClasses;
begin
  FRows := TcxVerticalGridRows.Create(Self);
  FRootRow := TcxCategoryRow.Create(nil);
  FRootRow.FParent := nil;
  FRootRow.FVerticalGrid := Self;
  FRootRow.FID := -1;
  inherited CreateSubClasses;
  InitDataController;
  FCustomizing := GetCustomizingClass.Create(Self);
end;

procedure TcxCustomVerticalGrid.DataChanged;
begin
  if FLockUpdate > 0 then Exit;
  Inc(FLockUpdate);
  try
    inherited DataChanged;
  finally
    Dec(FLockUpdate);
    LayoutChanged;
  end;
  Controller.MakeFocusedRecordVisible;
end;

procedure TcxCustomVerticalGrid.DestroySubClasses;
begin
  FreeAndNil(FCustomizing);
  FreeAndNil(FRootRow);
  FreeAndNil(FRows);
  inherited DestroySubClasses;
end;

procedure TcxCustomVerticalGrid.DoEdited(
  AItem: TcxCustomInplaceEditContainer);
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self, TcxCellEdit(AItem).EditRowProperties);
end;

function TcxCustomVerticalGrid.DoEditing(AItem: TcxCustomInplaceEditContainer): Boolean;
begin
  Result := True;
  if Assigned(FOnEditing) then
    FOnEditing(Self, TcxCellEdit(AItem).EditRowProperties, Result);
end;

procedure TcxCustomVerticalGrid.DoEditValueChanged(AItem: TcxCustomInplaceEditContainer);
begin
  if Assigned(FOnEditValueChanged) then
    if AItem <> nil then
      FOnEditValueChanged(Self, TcxCellEdit(AItem).EditRowProperties)
    else
      FOnEditValueChanged(Self, nil);
end;

procedure TcxCustomVerticalGrid.DoLayoutChanged;
begin
  if (csUpdating in ComponentState) or IsScaleChanging then
    Exit;
  ViewInfo.Calculate;
  if Customizing.Visible then
    Customizing.Update;
  CallNotify(OnLayoutChanged, Self);
end;

function TcxCustomVerticalGrid.DragDropImageDisplayRect: TRect;
begin
  Result := DragHeaderInfo.HeaderCellsRect;
end;

procedure TcxCustomVerticalGrid.DrawDragDropImage(ADragBitmap: TBitmap; ACanvas: TcxCanvas);
var
  AHeaderInfo: TcxCustomRowHeaderInfo;
  ASaveCanvas: TcxCanvas;
  ASaveOrg: TPoint;
begin
  ASaveCanvas := Painter.Canvas;
  AHeaderInfo := DragHeaderInfo;
  Painter.Canvas := ACanvas;
  try
    ASaveOrg := ACanvas.WindowOrg;
    ACanvas.WindowOrg := AHeaderInfo.HeaderCellsRect.TopLeft;
    ACanvas.TextFlags := Canvas.TextFlags;
    Painter.DrawHeaderDragImage(AHeaderInfo);
  finally
     ACanvas.WindowOrg := ASaveOrg;
     Painter.Canvas := ASaveCanvas;
  end;
end;

function TcxCustomVerticalGrid.GetControllerClass: TcxCustomControlControllerClass;
begin
  Result := TcxvgController;
end;

function TcxCustomVerticalGrid.GetControlStylesClass: TcxCustomControlStylesClass;
begin
  Result := TcxVerticalGridStyles;
end;

function TcxCustomVerticalGrid.GetDateTimeHandlingClass: TcxControlOptionsDateTimeHandlingClass;
begin
  Result := TcxVerticalGridDateTimeHandling;
end;

function TcxCustomVerticalGrid.GetDragImageHelperClass: TcxDragImageHelperClass;
begin
  Result := TcxvgDragImageHelper;
end;

function TcxCustomVerticalGrid.GetEditCellDataBindingClass: TcxItemDataBindingClass;
begin
  Result := TcxItemDataBinding;
end;

function TcxCustomVerticalGrid.GetFilterValueListClass: TcxControlFilterValueListClass;
begin
  Result := TcxVerticalGridFilterValueList;
end;

function TcxCustomVerticalGrid.GetHitTestControllerClass: TcxHitTestControllerClass;
begin
  Result := TcxvgHitTest;
end;

function TcxCustomVerticalGrid.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxvgOptionsBehavior;
end;

function TcxCustomVerticalGrid.GetOptionsFilterBoxClass: TcxControlOptionsFilterBoxClass;
begin
  Result := TcxVerticalGridFilterBox;
end;

function TcxCustomVerticalGrid.GetOptionsFilteringClass: TcxControlOptionsFilteringClass;
begin
  Result := TcxVerticalGridFiltering;
end;

function TcxCustomVerticalGrid.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxvgOptionsView;
end;

function TcxCustomVerticalGrid.GetPainterClass: TcxCustomControlPainterClass;
begin
  if OptionsView.PaintStyle = psdotNet then
    Result := TcxvgPainter
  else
    Result := TcxStyle3DPainter
end;

function TcxCustomVerticalGrid.HasDragDropImages: Boolean;
begin
  Result := DragHeaderInfo <> nil;
end;

function TcxCustomVerticalGrid.NeedCallChangedOnItemRemoved(AItem: TcxCustomInplaceEditContainer): Boolean;
begin
  Result := not (AItem.Owner is TcxCustomMultiEditorRow);
end;

procedure TcxCustomVerticalGrid.RecreateViewInfo;
begin
  if ViewInfo <> nil then
  begin
    HideEdit;
    Controller.SaveLayout;
    try
      inherited RecreateViewInfo;
      ViewInfo.IsDirty := True;
    finally
      Controller.RestoreLayout;
    end;
  end
  else
    inherited RecreateViewInfo;
end;

function RowIndexCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TcxCustomRow(Item1).FLoadedIndex - TcxCustomRow(Item2).FLoadedIndex;
end;

procedure TcxCustomVerticalGrid.RestoreLayout;
var
  I: Integer;
  L: TList;
begin
  if FVersion < cxVerticalGridStoringVersion then
  begin
    Modified;
    Exit;
  end;
  BeginUpdate;
  try
    L := TList.Create;
    try
      for I := 0 to Rows.Count - 1 do
        L.Add(Rows[I]);
      for I := 0 to L.Count - 1 do
        TcxCustomRow(L[I]).RestoreParent;
      if L.Count > 1 then
        L.Sort(RowIndexCompare);
      for I := 0 to L.Count - 1 do
        TcxCustomRow(L[I]).RestoreIndex;
    finally
      L.Free;
    end;
  finally
    CancelUpdate;
  end;
end;

procedure TcxCustomVerticalGrid.InitDataController;
begin
  DataController.AppendRecord;
  Controller.FocusedRecordIndex := 0;
end;

function TcxCustomVerticalGrid.IsRecordPixelScrolling: Boolean;
begin
  Result := False;
end;

procedure TcxCustomVerticalGrid.PaintStyleChanged;
begin
  if ViewInfo <> nil then
    RecreateViewInfo;
  if Controller <> nil then
    Controller.UpdatePaintStyle;
end;

procedure TcxCustomVerticalGrid.RemoveRowFromVerticalGrid(ARow: TcxCustomRow);
begin
  Inc(FLockUpdate);
  try
    if not IsDestroying then
    begin
      if IsDesigning and (Controller.DesignSelectionHelper <> nil) then
        Controller.DesignSelectionHelper.UnselectObject(ARow);
      if ARow = FocusedRow then
        Controller.Clear;
    end;
    Rows.Remove(ARow);
    ARow.RemoveChildren;
  finally
    Dec(FLockUpdate);
  end;
  LayoutChanged;
end;

procedure TcxCustomVerticalGrid.AfterLoadedRows;

  procedure SetupIndexes(ARow: TcxCustomRow);
  var
    I, AIndex: Integer;
  begin
    if ARow.Count > 0 then
    begin
      AIndex := 0;
      repeat
        for I := AIndex to ARow.Count - 1 do
          if ARow.Rows[I].FLoadingIndex = AIndex then
          begin
            ARow.Rows[I].Index := AIndex;
            Break;
          end;
        Inc(AIndex);
      until AIndex = ARow.Count;
      for I := 0 to ARow.Count - 1 do
        SetupIndexes(ARow.Rows[I]);
    end;
  end;

var
  L: TList;
  I: Integer;
  ARow: TcxCustomRow;
begin
  if not FNewLoadMode then Exit;
  L := TList.Create;
  try
    Rows.PrepareList;
    L.Assign(Rows.FList);
    for I := 0 to L.Count - 1 do
    begin
      ARow := L[I];
      if ARow.FLoadingIndex <> MaxInt then
        ARow.Parent := Rows.FindRowByStoredName(ARow.FLoadingParent);
    end;
    SetupIndexes(FRootRow);
  finally
    L.Free;
  end;
end;

procedure TcxCustomVerticalGrid.BeforeLoadedRows;
var
  I: Integer;
begin
  Rows.PrepareList;
  for I := 0 to Rows.Count - 1 do
    Rows[I].FLoadingIndex := MaxInt;
end;

function TcxCustomVerticalGrid.CanBandSizing: Boolean;
begin
  Result := OptionsBehavior.BandSizing and not OptionsView.AutoScaleBands;
end;

procedure TcxCustomVerticalGrid.CheckRowClass(ARowClass: TcxCustomRowClass);
begin
  if (ARowClass = nil) or not (ARowClass.InheritsFrom(GetEditorRowClass) or
    ARowClass.InheritsFrom(TcxCategoryRow) or ARowClass.InheritsFrom(GetMultiEditorRowClass)) then
    cxVerticalGridError(cxGetResourceString(@cxSvgInvalidRowClass));
end;

procedure TcxCustomVerticalGrid.CheckGridModeBufferCount;
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  if (DataController <> nil) and DataController.IsGridMode and
    DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) then
      AIntf.CheckGridModeBufferCount;
end;

procedure TcxCustomVerticalGrid.CheckLayoutRealign;
begin
  if Assigned(Controller) then
    Controller.Scroller.CheckDecrease := True;
end;

procedure TcxCustomVerticalGrid.FreeRowsViewInfo;
var
  I: Integer;
begin
  if not IsDestroying then
  begin
    for I := 0 to Rows.Count - 1 do
      FreeAndNil(Rows[I].FViewInfo);
  end;
end;

procedure TcxCustomVerticalGrid.DoCustomizationVisibleChanged;
begin
  if Assigned(FOnCustomizationVisibleChanged) then
    FOnCustomizationVisibleChanged(Self);
end;

function TcxCustomVerticalGrid.DoDrawBackgroundPart(const R: TRect;
  const AViewParams: TcxViewParams): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self, TcxvgPainter(Painter).Canvas, R, AViewParams, Result);
end;

function TcxCustomVerticalGrid.DoDrawRowHeader(AHeaderViewInfo: TcxCustomRowHeaderInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawRowHeader) then
    FOnDrawRowHeader(Self, TcxvgPainter(Painter).Canvas, TcxvgPainter(Painter), AHeaderViewInfo, Result);
end;

function TcxCustomVerticalGrid.DoDrawValue(AValueInfo: TcxRowValueInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawValue) then
    FOnDrawValue(Self, TcxvgPainter(Painter).Canvas, TcxvgPainter(Painter), AValueInfo, Result);
end;

procedure TcxCustomVerticalGrid.DoItemChanged(AOldRow: TcxCustomRow; AOldCellIndex: Integer);
begin
  if Assigned(FOnItemChanged) then
    FOnItemChanged(Self, AOldRow, AOldCellIndex);
end;

procedure TcxCustomVerticalGrid.DoLeftVisibleBandIndexChanged;
begin
  if Assigned(FOnLeftVisibleBandIndexChanged) then
    FOnLeftVisibleBandIndexChanged(Self);
end;

procedure TcxCustomVerticalGrid.DoLeftVisibleRecordIndexChanged;
begin
  if Assigned(FOnLeftVisibleRecordIndexChanged) then
    FOnLeftVisibleRecordIndexChanged(Self);
end;

procedure TcxCustomVerticalGrid.DoTopRowIndexChanged;
begin
  if Assigned(FOnTopRowIndexChanged) then
    FOnTopRowIndexChanged(Self);
end;

function TcxCustomVerticalGrid.GetCalcHelperClass: TcxvgCustomPaintStyleCalcHelperClass;
begin
  if LookAndFeel.SkinPainter <> nil then
    Result := TcxvgSkinCalcHelper
  else
    if OptionsView.PaintStyle = psDotNet then
      Result := TcxvgDotNetStyleCalcHelper
    else
      Result := TcxvgStyle3DCalcHelper
end;

function TcxCustomVerticalGrid.GetCellAutoHeight: Boolean;
begin
  Result := OptionsView.CellAutoHeight;
end;

function TcxCustomVerticalGrid.GetCustomizingClass: TcxVerticalGridCustomizingClass;
begin
  Result := TcxVerticalGridCustomizing;
end;

function TcxCustomVerticalGrid.GetEditorRowClass: TcxCustomRowClass;
begin
  Result := TcxEditorRow;
end;

function TcxCustomVerticalGrid.GetMultiEditorRowClass: TcxCustomRowClass;
begin
  Result := TcxMultiEditorRow;
end;

function TcxCustomVerticalGrid.GetRowContentStyleIndex(
  AProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): Integer;
begin
  if Assigned(AProperties) then
    Result := AProperties.Row.VisibleIndex
  else
    Result := ARecordIndex;
end;

procedure TcxCustomVerticalGrid.RowsChanged;
begin
  CheckLayoutRealign;
  LayoutChanged;
end;

procedure TcxCustomVerticalGrid.SetCustomization(AActive: Boolean);
begin
  if AActive then
  begin
    FSaveDragMode := DragMode;
    DragMode := dmAutomatic;
  end
  else
    DragMode := FSaveDragMode;
  DoCustomizationVisibleChanged;
end;


procedure TcxCustomVerticalGrid.UpdateDesignEditor;
begin
  LayoutChanged;
  if not IsLocked then
    Modified;
end;

procedure TcxCustomVerticalGrid.ValidateFocusedRow;
var
  ARow: TcxCustomRow;
  I: Integer;
begin
  if (FocusedRow = nil) and (Rows.VisibleRowCount > 0) then
    for I := 0 to Rows.VisibleRowCount - 1 do
    begin
      ARow := Rows.VisibleRows[I];
      if ARow.CanFocus then
      begin
        Controller.FocusRow(ARow, ActivateType = atOther);
        Break;
      end;
    end;
end;

procedure TcxCustomVerticalGrid.CategoryFontChanged(Sender: TObject);
begin
  LayoutChanged;
end;

function TcxCustomVerticalGrid.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := FConditionalFormattingProvider.ConditionalFormatting;
end;

function TcxCustomVerticalGrid.GetController: TcxvgController;
begin
  Result := TcxvgController(FController)
end;

function TcxCustomVerticalGrid.GetDateTimeHandling: TcxVerticalGridDateTimeHandling;
begin
  Result := TcxVerticalGridDateTimeHandling(inherited DateTimeHandling);
end;

function TcxCustomVerticalGrid.GetDragHeaderInfo: TcxCustomRowHeaderInfo;
begin
  with Controller do
    if DragRow <> nil then
      Result := DragRow.ViewInfo.HeaderInfo
    else
      Result := nil;
end;

function TcxCustomVerticalGrid.GetFilterBox: TcxVerticalGridFilterBox;
begin
  Result := TcxVerticalGridFilterBox(inherited FilterBox);
end;

function TcxCustomVerticalGrid.GetFiltering: TcxVerticalGridFiltering;
begin
  Result := TcxVerticalGridFiltering(inherited Filtering);
end;

function TcxCustomVerticalGrid.GetFocusedRow: TcxCustomRow;
begin
  Result := Controller.FocusedRow;
end;

function TcxCustomVerticalGrid.GetHitTest: TcxvgHitTest;
begin
  Result := TcxvgHitTest(Controller.HitTestController);
end;

function TcxCustomVerticalGrid.GetInplaceEditor: TcxCustomEdit;
begin
  if IsEditing then
    Result := Controller.EditingController.Edit
  else
    Result := nil;
end;

function TcxCustomVerticalGrid.GetIsEditing: Boolean;
begin
  Result := Controller.IsEditing;
end;

function TcxCustomVerticalGrid.GetLeftVisibleBand: Integer;
begin
  Result := Controller.Scroller.LeftVisibleBand;
end;

function TcxCustomVerticalGrid.GetLeftVisibleRecord: Integer;
begin
  Result := Controller.Scroller.LeftVisibleRecord;
end;

function TcxCustomVerticalGrid.GetOptionsBehavior: TcxvgOptionsBehavior;
begin
  Result := TcxvgOptionsBehavior(inherited OptionsBehavior);
end;

function TcxCustomVerticalGrid.GetOptionsView: TcxvgOptionsView;
begin
  Result := TcxvgOptionsView(inherited OptionsView);
end;

function TcxCustomVerticalGrid.GetPainter: TcxvgPainter;
begin
  Result := TcxvgPainter(inherited Painter);
end;

function TcxCustomVerticalGrid.GetRecordCount: Integer;
begin
  Result := DataController.RowCount;
end;

function TcxCustomVerticalGrid.GetStyles: TcxVerticalGridStyles;
begin
  Result := TcxVerticalGridStyles(inherited Styles);
end;

function TcxCustomVerticalGrid.GetTopVisibleRowIndex: Integer;
begin
  Result := Controller.Scroller.TopVisibleRowIndex;
end;

function TcxCustomVerticalGrid.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := TcxvgCustomViewInfo(inherited ViewInfo);
end;

procedure TcxCustomVerticalGrid.ImageListChange(Sender: TObject);
begin
  if HandleAllocated and (Sender = Images) then
    UpdateViewStyles;
end;

procedure TcxCustomVerticalGrid.SetDateTimeHandling(AValue: TcxVerticalGridDateTimeHandling);
begin
  inherited DateTimeHandling := AValue;
end;

procedure TcxCustomVerticalGrid.SetFilterBox(AValue: TcxVerticalGridFilterBox);
begin
  inherited FilterBox := AValue;
end;

procedure TcxCustomVerticalGrid.SetFiltering(AValue: TcxVerticalGridFiltering);
begin
  inherited Filtering := AValue;
end;

procedure TcxCustomVerticalGrid.SetFocusedRow(Value: TcxCustomRow);
begin
  with Controller do
  begin
    FocusedRow := Value;
    MakeRowVisible(FocusedRow);
  end;
end;

procedure TcxCustomVerticalGrid.SetImages(Value: TCustomImageList);
begin
  BeginUpdate;
  try
    cxSetImageList(Value, FImages, FImageChangeLink, Self);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomVerticalGrid.SetLeftVisibleBand(Value: Integer);
begin
  Controller.Scroller.LeftVisibleBand := Value;
end;

procedure TcxCustomVerticalGrid.SetLeftVisibleRecord(Value: Integer);
begin
  Controller.Scroller.LeftVisibleRecord := Value;
end;

procedure TcxCustomVerticalGrid.SetOptionsBehavior(Value: TcxvgOptionsBehavior);
begin
  OptionsBehavior.Assign(Value);
end;

procedure TcxCustomVerticalGrid.SetOptionsView(Value: TcxvgOptionsView);
begin
  OptionsView.Assign(Value);
end;

procedure TcxCustomVerticalGrid.SetStyles(Value: TcxVerticalGridStyles);
begin
  Styles.Assign(Value);
end;

procedure TcxCustomVerticalGrid.SetTopVisibleRowIndex(Value: Integer);
begin
  Controller.Scroller.TopVisibleRowIndex := Value;
end;

procedure TcxCustomVerticalGrid.ReadVersion(Reader: TReader);
begin
  FVersion := Reader.ReadInteger;
end;

procedure TcxCustomVerticalGrid.WriteVersion(Writer: TWriter);
begin
  Writer.WriteInteger(cxVerticalGridStoringVersion);
end;

{ TcxUnboundVerticalGrid }

constructor TcxUnboundVerticalGrid.Create(AOwner: TComponent);
begin
  FLayoutStyle := ulsSingleRecordView;
  inherited Create(AOwner);
end;

function TcxUnboundVerticalGrid.CanBandSizing: Boolean;
begin
  Result := (FLayoutStyle <> ulsSingleRecordView) and inherited CanBandSizing;
end;

function TcxUnboundVerticalGrid.GetScrollStrategyClass: TcxvgCustomScrollStrategyClass;
begin
  if FLayoutStyle = ulsSingleRecordView then
    Result := TcxvgSingleRecordScrollStrategy
  else
    Result := TcxvgBandsScrollStrategy;
end;

function TcxUnboundVerticalGrid.GetViewInfoClass: TcxCustomControlViewInfoClass;
begin
  if FLayoutStyle = ulsSingleRecordView then
    Result := TcxInspectorViewInfo
  else
    Result := TcxBandsViewInfo;
end;

procedure TcxUnboundVerticalGrid.SetLayoutStyle(Value: TcxvgUnboundLayoutStyle);
begin
  if FLayoutStyle <> Value then
  begin
    FLayoutStyle := Value;
    RecreateViewInfo;
    ViewInfo.LayoutStyleChanged;
  end;
end;

{ TcxVirtualVerticalGrid }

constructor TcxVirtualVerticalGrid.Create(AOwner: TComponent);
begin
  FLayoutStyle := lsSingleRecordView;
  inherited Create(AOwner);
end;

procedure TcxVirtualVerticalGrid.Assign(Source: TPersistent);
begin
  if Source is TcxVirtualVerticalGrid then
    with TcxVirtualVerticalGrid(Source) do
    begin
      Self.BeginUpdate;
      inherited Assign(Source);
      try
        Self.LayoutStyle := LayoutStyle;
      finally
        Self.EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

function TcxVirtualVerticalGrid.NavigatorGetRecordCount: Integer;
begin
  Result := DataController.DataRowCount;
end;

function TcxVirtualVerticalGrid.NavigatorGetRecordIndex: Integer;
begin
  Result := DataController.FocusedDataRowIndex;
end;

function TcxVirtualVerticalGrid.NavigatorCanAppend: Boolean;
begin
  Result := Controller.CanAppend(True);
end;

function TcxVirtualVerticalGrid.NavigatorCanDelete: Boolean;
begin
  Result := Controller.CanDelete(True);
end;

function TcxVirtualVerticalGrid.NavigatorCanEdit: Boolean;
begin
  Result := OptionsData.Editing;
end;

function TcxVirtualVerticalGrid.NavigatorCanInsert: Boolean;
begin
  Result := Controller.CanInsert(True);
end;

function TcxVirtualVerticalGrid.NavigatorIsActive: Boolean;
begin
  Result := DataController.Active;
end;

function TcxVirtualVerticalGrid.NavigatorIsBof: Boolean;
begin
  Result := DataController.IsBOF;
end;

function TcxVirtualVerticalGrid.NavigatorIsBookmarkAvailable: Boolean;
begin
  Result := DataController.IsBookmarkAvailable;
end;

function TcxVirtualVerticalGrid.NavigatorIsEditing: Boolean;
begin
  Result := DataController.IsEditing;
end;

function TcxVirtualVerticalGrid.NavigatorIsEof: Boolean;
begin
  Result := DataController.IsEOF;
end;

procedure TcxVirtualVerticalGrid.NavigatorClearBookmark;
begin
  DataController.ClearBookmark;
end;

procedure TcxVirtualVerticalGrid.NavigatorDoAction(AButtonIndex: Integer);
begin
  with DataController do
    case AButtonIndex of
      NBDI_FIRST:
        GotoFirst;
      NBDI_PRIORPAGE:
        if LayoutStyle = lsMultiRecordView then
          MoveBy(-Controller.Scroller.VisibleValueCount)
        else
          GotoPrev;
      NBDI_PRIOR:
        GotoPrev;
      NBDI_NEXT:
        GotoNext;
      NBDI_NEXTPAGE:
        if LayoutStyle = lsMultiRecordView then
          MoveBy(Controller.Scroller.VisibleValueCount)
        else
          GotoNext;
      NBDI_LAST:
        GotoLast;
      NBDI_APPEND:
        Controller.AppendRecord;
      NBDI_INSERT:
        Controller.InsertRecord;
      NBDI_DELETE:
        Controller.DeleteSelection;
      NBDI_EDIT:
        ShowEdit;
      NBDI_POST:
        begin
          PostEditingData;
          Post;
        end;
      NBDI_CANCEL:
        Cancel;
      NBDI_REFRESH:
        Refresh;
      NBDI_SAVEBOOKMARK:
        SaveBookmark;
      NBDI_GOTOBOOKMARK:
        GotoBookmark;
      NBDI_FILTER:
        RunFilterCustomizeDialog;
    end;
end;

function TcxVirtualVerticalGrid.NavigatorGetNotifier: TcxNavigatorControlNotifier;
begin
  Result := FNavigatorNotifier;
end;

function TcxVirtualVerticalGrid.NavigatorIsActionSupported(
  AButtonIndex: Integer): Boolean;
begin
  Result := AButtonIndex in [NBDI_FIRST..NBDI_FILTER];
end;

procedure TcxVirtualVerticalGrid.RefreshNavigators;
begin
  if not IsDestroying then
  begin
    if Navigator.Visible then
      NavigatorStateChanged;
    FNavigatorNotifier.RefreshNavigatorButtons;
  end;
end;

procedure TcxVirtualVerticalGrid.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FNavigatorNotifier := TcxNavigatorControlNotifier.Create;
end;

procedure TcxVirtualVerticalGrid.DestroySubClasses;
begin
  FreeAndNil(FNavigatorNotifier);
  inherited DestroySubClasses;
end;

function TcxVirtualVerticalGrid.CanBandSizing: Boolean;
begin
  Result := OptionsBehavior.BandSizing and
    ((FLayoutStyle = lsMultiRecordView) or
    ((FLayoutStyle = lsBandsView) and not OptionsView.AutoScaleBands));
end;

function TcxVirtualVerticalGrid.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick;
  if Result and IsFocused and IsEditing and
    HitTest.HitAtNavigator then
    Result := False;
end;

procedure TcxVirtualVerticalGrid.ControlUpdateData(AInfo: TcxUpdateControlInfo);
begin
  if IsDestroying then Exit;
  inherited ControlUpdateData(AInfo);
  if (AInfo is TcxSearchChangedInfo) then
    Controller.RefreshIncSearchItem;
  RefreshNavigators;
end;

procedure TcxVirtualVerticalGrid.DoFocusedRecordChanged(
  APrevFocusedRecord, AFocusedRecord: Integer);
begin
  if Assigned(FOnFocusedRecordChanged) then
    FOnFocusedRecordChanged(Self, APrevFocusedRecord, AFocusedRecord);
end;

function TcxVirtualVerticalGrid.GetCellAutoHeight: Boolean;
begin
  Result := (FLayoutStyle <> lsMultiRecordView) and inherited GetCellAutoHeight;
end;

function TcxVirtualVerticalGrid.GetControllerClass: TcxCustomControlControllerClass;
begin
  Result := TcxvgMultiRecordsController;
end;

function TcxVirtualVerticalGrid.GetNavigatorButtonsControl: IcxNavigator;
begin
  Result := Self;
end;

function TcxVirtualVerticalGrid.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxvgMultiRecordsOptionsBehavior;
end;

function TcxVirtualVerticalGrid.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxvgMultiRecordsOptionsData;
end;

function TcxVirtualVerticalGrid.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxvgMultiRecordsOptionsView;
end;

function TcxVirtualVerticalGrid.GetRowContentStyleIndex(AProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): Integer;
begin
  if (LayoutStyle = lsMultiRecordView) and
    (OptionsView.MultiRecordEvenOddContentStyle = mrcsByRecord) then
    Result := ARecordIndex
  else
    Result := inherited GetRowContentStyleIndex(AProperties, ARecordIndex);
end;

procedure TcxVirtualVerticalGrid.InitDataController;
begin
end;

function TcxVirtualVerticalGrid.IsRecordPixelScrolling: Boolean;
begin
  Result := (ViewInfo.Scroller.ScrollStrategy.CanRecordPixelScrolling and
    ((OptionsBehavior.RecordScrollMode = rsmByPixel) or
    ((OptionsBehavior.RecordScrollMode = rsmDefault) and cxIsTouchModeEnabled)))
end;

function TcxVirtualVerticalGrid.IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := not IsRecordPixelScrolling or (AScrollKind <> sbHorizontal);
end;

procedure TcxVirtualVerticalGrid.BeginGestureScroll(APos: TPoint);
begin
  inherited BeginGestureScroll(APos);
  Controller.EditingItem := nil;
  ViewInfo.Scroller.BeginGestureScroll(APos);
end;

procedure TcxVirtualVerticalGrid.DataLayoutChanged;
begin
  if not DataController.IsGridMode then
    inherited DataLayoutChanged
  else
    LayoutChanged;
end;

procedure TcxVirtualVerticalGrid.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
  ViewInfo.Scroller.ScrollStrategy.ScrollContentByGesture(AScrollKind, ADelta);
end;

function TcxVirtualVerticalGrid.CanScrollContentByGestureWithoutScrollBars: Boolean;
begin
  Result := cxIsTouchModeEnabled;
end;

function TcxVirtualVerticalGrid.GetScrollStrategyClass: TcxvgCustomScrollStrategyClass;
begin
  case FLayoutStyle of
    lsSingleRecordView: Result := TcxvgSingleRecordScrollStrategy;
    lsBandsView: Result := TcxvgBandsScrollStrategy;
  else
    Result := TcxvgMultiRecordsScrollStrategy;
  end;
end;

function TcxVirtualVerticalGrid.GetViewInfoClass: TcxCustomControlViewInfoClass;
begin
  case FLayoutStyle of
    lsSingleRecordView: Result := TcxInspectorViewInfo;
    lsBandsView: Result := TcxBandsViewInfo;
  else
    Result := TcxMultiRecordViewInfo;
  end;
end;

function TcxVirtualVerticalGrid.GetFilterRecordEvent: TcxDataFilterRecordEvent;
begin
  Result := DataController.OnFilterRecord;
end;

function TcxVirtualVerticalGrid.GetFocusedRecordIndex: Integer;
begin
  Result := DataController.FocusedRowIndex;
end;

function TcxVirtualVerticalGrid.GetController: TcxvgMultiRecordsController;
begin
  Result := TcxvgMultiRecordsController(FController);
end;

function TcxVirtualVerticalGrid.GetOptionsBehavior: TcxvgMultiRecordsOptionsBehavior;
begin
  Result := TcxvgMultiRecordsOptionsBehavior(inherited OptionsBehavior);
end;

function TcxVirtualVerticalGrid.GetOptionsData: TcxvgMultiRecordsOptionsData;
begin
  Result := TcxvgMultiRecordsOptionsData(inherited OptionsData);
end;

function TcxVirtualVerticalGrid.GetOptionsView: TcxvgMultiRecordsOptionsView;
begin
  Result := TcxvgMultiRecordsOptionsView(inherited OptionsView);
end;

procedure TcxVirtualVerticalGrid.SetFilterRecordEvent(
  Value: TcxDataFilterRecordEvent);
begin
  DataController.OnFilterRecord := Value;
end;

procedure TcxVirtualVerticalGrid.SetFocusedRecordIndex(Value: Integer);
begin
  DataController.FocusedRowIndex := Value;
end;

procedure TcxVirtualVerticalGrid.SetLayoutStyle(Value: TcxvgLayoutStyle);
begin
  if FLayoutStyle <> Value then
  begin
    FLayoutStyle := Value;
    RecreateViewInfo;
    ViewInfo.LayoutStyleChanged;
    CheckGridModeBufferCount;
  end;
end;

procedure TcxVirtualVerticalGrid.SetOptionsBehavior(
  Value: TcxvgMultiRecordsOptionsBehavior);
begin
  inherited OptionsBehavior.Assign(Value);
end;

procedure TcxVirtualVerticalGrid.SetOptionsData(
  Value: TcxvgMultiRecordsOptionsData);
begin
  inherited OptionsData.Assign(Value);
end;

procedure TcxVirtualVerticalGrid.SetOptionsView(
  Value: TcxvgMultiRecordsOptionsView);
begin
  inherited OptionsView.Assign(Value);
end;

{ TcxvgCustomPaintStyleCalcHelper }

constructor TcxvgCustomPaintStyleCalcHelper.Create(AViewInfo: TcxvgCustomViewInfo);
begin
  FViewInfo := AViewInfo;
  FVerticalGrid := AViewInfo.VerticalGrid;
  FScroller := AViewInfo.Scroller;
end;

function TcxvgCustomPaintStyleCalcHelper.GetBandBorderColor: TColor;
begin
  Result := VerticalGrid.OptionsView.GridLineColor;
end;

function TcxvgCustomPaintStyleCalcHelper.GetContentEvenColor(AFocused: Boolean): TColor;
begin
  Result := GetContentColor(AFocused);
end;

function TcxvgCustomPaintStyleCalcHelper.GetContentEvenTextColor: TColor;
begin
  Result := GetContentTextColor;
end;

function TcxvgCustomPaintStyleCalcHelper.GetContentOddColor(AFocused: Boolean): TColor;
begin
  Result := GetContentColor(AFocused);
end;

function TcxvgCustomPaintStyleCalcHelper.GetContentOddTextColor: TColor;
begin
  Result := GetContentTextColor;
end;

function TcxvgCustomPaintStyleCalcHelper.GetFindPanelColor: TColor;
begin
  Result := Painter.DefaultFilterBoxColor;
end;

function TcxvgCustomPaintStyleCalcHelper.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ViewInfo.Painter.Painter;
end;

function TcxvgCustomPaintStyleCalcHelper.GetScaleFactor: TdxScaleFactor;
begin
  Result := VerticalGrid.ScaleFactor;
end;

function TcxvgCustomPaintStyleCalcHelper.GetBackgroundColor: TColor;
begin
  Result := GetHeaderColor;
end;

function TcxvgCustomPaintStyleCalcHelper.IsBottomLineNeeded(ANextRow: TcxCustomRow): Boolean;
begin
  Result := ANextRow <> nil;
end;

{ TcxvgDotNetStyleCalcHelper }

procedure TcxvgDotNetStyleCalcHelper.AddBoundHeaderLines(
  ARowHeaderInfo: TcxCustomRowHeaderInfo);
begin
  with ARowHeaderInfo  do
    AddBottomHorzLine(cxRectBounds(IndentBounds.Right, IndentBounds.Top,
      HeaderRect.Right - IndentBounds.Right, IndentBounds.Bottom - IndentBounds.Top));
end;

procedure TcxvgDotNetStyleCalcHelper.AddDivider(
  ALinesInfo: TLineInfoList; const R: TRect; AColor: TColor; AIsSeparator: Boolean);
begin
  with ViewInfo do
    if DividerWidth > 0 then
      ALinesInfo.Add(R.Right, R.Top, VertLineWidth, R.Bottom - R.Top + HorzLineWidth, VertLineBrush);
end;

procedure TcxvgDotNetStyleCalcHelper.AddHeaderIndentLines(
  ARowHeaderInfo: TcxCustomRowHeaderInfo; const R: TRect;
  const AViewParams: TcxViewParams;
  AToCategories, AUnderline, AAddVertLine: Boolean);
var
  W: Integer;
begin
  with ViewInfo do
  begin
    if not ShowHeaders then Exit;
    if AAddVertLine and (VertLineWidth > 0) then
      ARowHeaderInfo.LinesInfo.Add(R.Right - VertLineWidth, R.Top, VertLineWidth,
        R.Bottom - R.Top + HorzLineWidth, VertLineBrush);
    if HorzLineWidth > 0 then
    begin
      W := R.Right - R.Left;
      if AAddVertLine then Dec(W, VertLineWidth);
      if AUnderline then
        ARowHeaderInfo.LinesInfo.Add(R.Left, R.Bottom, W, HorzLineWidth, HorzLineBrush)
      else
        if (AViewParams.Bitmap <> nil) and not AViewParams.Bitmap.Empty then
          ARowHeaderInfo.RowIndents.Add(cxRectBounds(R.Left, R.Bottom, W, HorzLineWidth), AViewParams)
        else
          ARowHeaderInfo.LinesInfo.Add(R.Left, R.Bottom, W, HorzLineWidth, AViewParams.Color);
    end;
  end;
end;

procedure TcxvgDotNetStyleCalcHelper.CalcPaintViewParamsLines(
  ARowViewInfo: TcxCustomRowViewInfo; ANextRow: TcxCustomRow);
begin
end;

function TcxvgDotNetStyleCalcHelper.ChangeFocusedRow(
  ANewFocus, AOldFocus: TcxCustomRow): TRect;
var
  ARecalcChild: Boolean;

  procedure ProcessRow(ARow: TcxCustomRow);
  var
    ARowViewInfo: TcxCustomRowViewInfo;
    ASaveColor: TColor;
    ANextRow, AChildRow: TcxCustomRow;
  begin
    ARowViewInfo := ViewInfo.GetRowViewInfo(ARow);
    if ARowViewInfo = nil then Exit;
    with ARowViewInfo do
    begin
      ASaveColor := HeaderInfo.IndentViewParams.Color;
      Result := cxRectUnion(Result, RowRect);
      ANextRow := VerticalGrid.NextVisibleRow(ARow);
      Recalculate(ANextRow);
      ARecalcChild := ARecalcChild or ((ASaveColor <> HeaderInfo.IndentViewParams.Color) and Row.IsCategory);
    end;
    if ARecalcChild then
    begin
      AChildRow := ANextRow;
      while AChildRow <> nil do
      begin
        if ARow.IsParent(AChildRow) then
        begin
          ARowViewInfo := AChildRow.ViewInfo;
          ARowViewInfo.HeaderInfo.Recalculate(ANextRow, True);
          Result := cxRectUnion(Result, ARowViewInfo.HeaderInfo.HeaderRect);
        end
        else
          Break;
        AChildRow := VerticalGrid.NextVisibleRow(AChildRow);
      end;
    end;
  end;

begin
  Result := cxNullRect;
  ARecalcChild := False;
  ProcessRow(ANewFocus);
  ProcessRow(AOldFocus);
end;

function TcxvgDotNetStyleCalcHelper.GetCategoryColor: TColor;
begin
  Result := Painter.DefaultHeaderColor;
end;

function TcxvgDotNetStyleCalcHelper.GetCategoryFocusRect(
  ARowHeaderInfo: TcxCustomRowHeaderInfo): TRect;
var
  ATextRect: TRect;
  ASize: TSize;
  ACaption: string;
  AHeaderInfo: TcxCategoryRowHeaderInfo;
  AProperties: TcxCaptionRowProperties;
begin
  Result := cxNullRect;
  if VerticalGrid.FocusedRow <> ARowHeaderInfo.Row then Exit;
  AHeaderInfo := TcxCategoryRowHeaderInfo(ARowHeaderInfo);
  AProperties := AHeaderInfo.Row.Properties;
  ACaption := AProperties.Caption;
  if ACaption = '' then Exit;
  ATextRect := AHeaderInfo.CaptionsInfo[0].CaptionTextRect;
  ASize := cxTextExtent(AHeaderInfo.ViewParams.Font, ACaption);
  case AProperties.HeaderAlignmentHorz of
    taLeftJustify: ATextRect.Right := ATextRect.Left + ASize.cx;
    taRightJustify: ATextRect.Left := ATextRect.Right - ASize.cx;
  else
    ATextRect := cxRectCenter(ATextRect, ASize.cx, ATextRect.Bottom - ATextRect.Top);
  end;
  case AProperties.HeaderAlignmentVert of
    vaTop: ATextRect.Bottom := ATextRect.Top + ASize.cy;
    vaBottom: ATextRect.Top := ATextRect.Bottom - ASize.cy;
  else
    ATextRect := cxRectCenter(ATextRect, ATextRect.Right - ATextRect.Left, ASize.cy);
  end;
  cxRectIntersect(Result, cxRectInflate(ATextRect, 1, 1), AHeaderInfo.HeaderRect);
end;

function TcxvgDotNetStyleCalcHelper.GetCategoryTextColor: TColor;
begin
  Result := clGrayText;
end;

function TcxvgDotNetStyleCalcHelper.GetContentColor(AFocused: Boolean): TColor;
begin
  Result := Painter.GridLikeControlContentColor;
end;

function TcxvgDotNetStyleCalcHelper.GetContentTextColor: TColor;
begin
  Result := Painter.GridLikeControlContentTextColor;
end;

function TcxvgDotNetStyleCalcHelper.GetHeaderColor: TColor;
begin
  Result := Painter.GridLikeControlContentColor;
end;

function TcxvgDotNetStyleCalcHelper.GetHeaderTextColor: TColor;
begin
  Result := Painter.DefaultContentTextColor;
end;

function TcxvgDotNetStyleCalcHelper.GetIndentViewParams(ARow,
  AParentIndentRow: TcxCustomRow): TcxViewParams;
begin
  if ARow <> AParentIndentRow then
    with AParentIndentRow.ViewInfo.HeaderInfo do
    begin
      if AParentIndentRow.VisibleIndex < Scroller.TopVisibleRowIndex then
        CalcViewParams(False);
      Result := IndentViewParams;
    end
  else
    Result := ARow.ViewInfo.HeaderInfo.ViewParams;
end;

function TcxvgDotNetStyleCalcHelper.IsDrawValueFocusRect: Boolean;
begin
  Result := True;
end;

function TcxvgDotNetStyleCalcHelper.CreateHorzLineBrush: TBrush;
begin
  Result := TBrush.Create;
  Result.Color := VerticalGrid.OptionsView.GridLineColor;
end;

function TcxvgDotNetStyleCalcHelper.CreateVertLineBrush: TBrush;
begin
  Result := TBrush.Create;
  Result.Color := VerticalGrid.OptionsView.GridLineColor;
end;

function TcxvgDotNetStyleCalcHelper.GetDividerWidth: Integer;
begin
  Result := 1;
end;

function TcxvgDotNetStyleCalcHelper.GetIndentWidth: Integer;
begin
  Result := Painter.ScaledExpandButtonAreaSize(ScaleFactor) + 4;
end;

{ TcxvgStyle3DCalcHelper }

procedure TcxvgStyle3DCalcHelper.AddBoundHeaderLines(
  ARowHeaderInfo: TcxCustomRowHeaderInfo);
begin
  if ViewInfo.HorzLineWidth > 0 then
    with ARowHeaderInfo do
      LinesInfo.Add(HeaderRect.Left, HeaderRect.Bottom, HeaderRect.Right - HeaderRect.Left,
        ViewInfo.HorzLineWidth, ViewInfo.HorzLineBrush);
end;

procedure TcxvgStyle3DCalcHelper.AddHeaderIndentLines(
  ARowHeaderInfo: TcxCustomRowHeaderInfo; const R: TRect;
  const AViewParams: TcxViewParams;
  AToCategories, AUnderline, AAddVertLine: Boolean);
begin
end;

procedure TcxvgStyle3DCalcHelper.AddDivider(ALinesInfo: TLineInfoList;
  const R: TRect; AColor: TColor; AIsSeparator: Boolean);
begin
  with ViewInfo do
    if DividerWidth > 0 then
      if AIsSeparator then
      begin
        ALinesInfo.Add(R.Right, R.Top, 2, 2, AColor);
        ALinesInfo.Add(R.Right, R.Bottom - 2, 2, 2, AColor);
        ALinesInfo.Add(R.Right, R.Top + 2, 1, R.Bottom - R.Top + HorzLineWidth - 5, clBtnShadow);
        ALinesInfo.Add(R.Right + 1, R.Top + 2, 1, R.Bottom - R.Top + HorzLineWidth - 5, clBtnHighlight);
      end
      else
      begin
        ALinesInfo.Add(R.Right, R.Top, 1, R.Bottom - R.Top + HorzLineWidth, clBtnShadow);
        ALinesInfo.Add(R.Right + 1, R.Top, 1, R.Bottom - R.Top + HorzLineWidth, clBtnHighlight);
      end;
end;

procedure TcxvgStyle3DCalcHelper.CalcPaintViewParamsLines(
  ARowViewInfo: TcxCustomRowViewInfo; ANextRow: TcxCustomRow);
var
  AFocusedRowBandIndex: Integer;
  R: TRect;
begin
  if VerticalGrid.FocusedRow = nil then Exit;
  R := ARowViewInfo.RowRect;
  if ANextRow = VerticalGrid.FocusedRow then
  begin
    AFocusedRowBandIndex := Scroller.GetBandIndexByRowIndex(ANextRow.VisibleIndex);
    if AFocusedRowBandIndex = ARowViewInfo.BandIndex then
    begin
      ViewInfo.FocusLinesInfo.Add(R.Left, R.Bottom - 1, R.Right - R.Left, 1, cl3DDkShadow);
      ViewInfo.FocusLinesInfo.Add(R.Left + 1, R.Bottom, R.Right - R.Left - 1, 1, clBtnShadow);
    end;
  end;
  if ARowViewInfo.Row = VerticalGrid.FocusedRow then
  begin
    ViewInfo.FocusLinesInfo.Add(R.Left, R.Bottom - 1, R.Right - R.Left, 1, clBtnFace);
    ViewInfo.FocusLinesInfo.Add(R.Left, R.Bottom, R.Right - R.Left, 1, clBtnHighlight);
    ViewInfo.FocusLinesInfo.Add(R.Left, R.Top - 1, 1, R.Bottom - R.Top + 1, cl3DDkShadow);
    ViewInfo.FocusLinesInfo.Add(R.Left + 1, R.Top, 1, R.Bottom - R.Top - 1, clBtnShadow);
  end;
end;

function TcxvgStyle3DCalcHelper.ChangeFocusedRow(
  ANewFocus, AOldFocus: TcxCustomRow): TRect;

  procedure ProcessRow(ARow: TcxCustomRow);
  var
    ARowViewInfo: TcxCustomRowViewInfo;
  begin
    ARowViewInfo := ViewInfo.GetRowViewInfo(ARow);
    if ARowViewInfo = nil then Exit;
    with ARowViewInfo do
    begin
      Result := cxRectUnion(Result, RowRect);
      Recalculate(VerticalGrid.NextVisibleRow(ARow));
    end;
    ARowViewInfo := ViewInfo.GetRowViewInfo(VerticalGrid.PrevVisibleRow(ARow));
    if ARowViewInfo <> nil then
    begin
      ARowViewInfo.Recalculate(ARow);
      Result := cxRectUnion(Result, ARowViewInfo.RowRect);
    end;
  end;

begin
  Result := cxNullRect;
  ViewInfo.FocusLinesInfo.Clear;
  ProcessRow(ANewFocus);
  ProcessRow(AOldFocus);
  Inc(Result.Bottom, ViewInfo.HorzLineWidth);
end;

function TcxvgStyle3DCalcHelper.GetCategoryColor: TColor;
begin
  Result := Painter.DefaultHeaderColor;
end;

function TcxvgStyle3DCalcHelper.GetCategoryFocusRect(
  ARowHeaderInfo: TcxCustomRowHeaderInfo): TRect;
begin
  Result := cxNullRect;
end;

function TcxvgStyle3DCalcHelper.GetCategoryTextColor: TColor;
begin
  Result := Painter.DefaultHeaderTextColor;
end;

function TcxvgStyle3DCalcHelper.GetContentColor(AFocused: Boolean): TColor;
begin
  if AFocused then
    Result := Painter.GridLikeControlContentColor
  else
    Result := Painter.DefaultHeaderColor;
end;

function TcxvgStyle3DCalcHelper.GetContentTextColor: TColor;
begin
  Result := clNavy;
end;

function TcxvgStyle3DCalcHelper.GetHeaderColor: TColor;
begin
  Result := Painter.DefaultHeaderColor;
end;

function TcxvgStyle3DCalcHelper.GetHeaderTextColor: TColor;
begin
  Result := Painter.DefaultHeaderTextColor;
end;

function TcxvgStyle3DCalcHelper.GetIndentViewParams(ARow,
  AParentIndentRow: TcxCustomRow): TcxViewParams;
begin
  Result := ARow.ViewInfo.HeaderInfo.ViewParams;
end;

function TcxvgStyle3DCalcHelper.IsDrawValueFocusRect: Boolean;
begin
  Result := False;
end;

function TcxvgStyle3DCalcHelper.CreateHorzLineBrush: TBrush;
begin
  Result := cxCreateHalftoneBrush(VerticalGrid.OptionsView.GridLineColor, clBtnFace);
end;

function TcxvgStyle3DCalcHelper.CreateVertLineBrush: TBrush;
begin
  Result := cxCreateHalftoneBrush(VerticalGrid.OptionsView.GridLineColor, clBtnFace);
end;

function TcxvgStyle3DCalcHelper.GetDividerWidth: Integer;
begin
  Result := 2;
end;

function TcxvgStyle3DCalcHelper.GetIndentWidth: Integer;
begin
  Result := Painter.ScaledExpandButtonAreaSize(ScaleFactor) + 4;
end;

{ TcxvgSkinCalcHelper }

procedure TcxvgSkinCalcHelper.AddBoundHeaderLines(
  ARowHeaderInfo: TcxCustomRowHeaderInfo);
begin
  if ARowHeaderInfo is TcxCategoryRowHeaderInfo then
  begin
    if ARowHeaderInfo.ViewInfo.HorzLineWidth > 0 then
    begin
      with ARowHeaderInfo do
      begin
        ARowHeaderInfo.LinesInfo.Add(IndentBounds.Right, IndentBounds.Top - ViewInfo.HorzLineWidth,
          HeaderRect.Right - IndentBounds.Right, ViewInfo.HorzLineWidth, ViewInfo.BandBorderColor);
        ARowHeaderInfo.LinesInfo.Add(IndentBounds.Right, IndentBounds.Bottom, HeaderRect.Right - IndentBounds.Right,
          ViewInfo.HorzLineWidth, ViewInfo.BandBorderColor);
      end;
    end;
  end
  else
    inherited AddBoundHeaderLines(ARowHeaderInfo);
end;

procedure TcxvgSkinCalcHelper.AddHeaderIndentLines(ARowHeaderInfo: TcxCustomRowHeaderInfo;
  const R: TRect; const AViewParams: TcxViewParams; AToCategories, AUnderline,
  AAddVertLine: Boolean);
begin
  inherited AddHeaderIndentLines(ARowHeaderInfo, R, AViewParams, AToCategories, AUnderline, AAddVertLine);
  if not ViewInfo.ShowHeaders then Exit;
  if AToCategories and (ViewInfo.VertLineWidth > 0) then
  begin
    if ARowHeaderInfo is TcxCategoryRowHeaderInfo then
      if ARowHeaderInfo.IndentBounds.Right = R.Right then
        ARowHeaderInfo.LinesInfo.Add(R.Left, R.Top - ViewInfo.HorzLineWidth,
          R.Right - R.Left, ViewInfo.HorzLineWidth, ViewInfo.BandBorderColor);
    if AAddVertLine then
      ARowHeaderInfo.LinesInfo.Add(R.Right - ViewInfo.VertLineWidth,
        R.Top - ViewInfo.HorzLineWidth, ViewInfo.VertLineWidth,
        R.Bottom - R.Top + ViewInfo.HorzLineWidth, ViewInfo.BandBorderColor);
    if AUnderline then
      ARowHeaderInfo.LinesInfo.Add(R.Left, R.Bottom, R.Right - R.Left, ViewInfo.HorzLineWidth, ViewInfo.BandBorderColor);
  end;
end;

function TcxvgSkinCalcHelper.GetBackgroundColor: TColor;
begin
  Result := Painter.DefaultVGridContentColor;
end;

function TcxvgSkinCalcHelper.GetCategoryColor: TColor;
begin
  Result := Painter.DefaultVGridCategoryColor;
end;

function TcxvgSkinCalcHelper.GetCategoryTextColor: TColor;
begin
  Result := Painter.DefaultVGridCategoryTextColor;
  if Result = clDefault then
    Result := inherited GetCategoryTextColor;
end;

function TcxvgSkinCalcHelper.GetContentColor(AFocused: Boolean): TColor;
begin
  Result := Painter.DefaultVGridContentColor;
end;

function TcxvgSkinCalcHelper.GetContentEvenColor(AFocused: Boolean): TColor;
begin
  Result := Painter.DefaultVGridContentEvenColor;
end;

function TcxvgSkinCalcHelper.GetContentOddColor(AFocused: Boolean): TColor;
begin
  Result := Painter.DefaultVGridContentOddColor;
end;

function TcxvgSkinCalcHelper.GetHeaderColor: TColor;
begin
  Result := Painter.DefaultVGridHeaderColor;
  if Result = clDefault then
    Result := inherited GetHeaderColor;
end;

function TcxvgSkinCalcHelper.GetHeaderTextColor: TColor;
begin
  Result := Painter.DefaultVGridHeaderTextColor;
  if Result = clDefault then
    Result := inherited GetHeaderTextColor;
end;

function TcxvgSkinCalcHelper.CreateHorzLineBrush: TBrush;
begin
  Result := TBrush.Create;
  Result.Color := Painter.DefaultVGridLineColor;
end;

function TcxvgSkinCalcHelper.CreateVertLineBrush: TBrush;
begin
  Result := TBrush.Create;
  Result.Color := Painter.DefaultVGridLineColor;
end;

function TcxvgSkinCalcHelper.GetBandBorderColor: TColor;
begin
  Result := Painter.DefaultVGridBandLineColor;
end;

function TcxvgSkinCalcHelper.IsBottomLineNeeded(ANextRow: TcxCustomRow): Boolean;
begin
  Result := (ANextRow = nil) or not ANextRow.IsCategory;
end;

{ TcxvgScroller }

constructor TcxvgScroller.Create(AVerticalGrid: TcxCustomVerticalGrid);
begin
  FVerticalGrid := AVerticalGrid;
end;

destructor TcxvgScroller.Destroy;
begin
  FreeAndNil(FScrollStrategy);
  inherited Destroy;
end;

function TcxvgScroller.GoToFirst: Boolean;
begin
  Result := ScrollStrategy.FocusNextRecord(-1, True);
end;

function TcxvgScroller.GoToLast: Boolean;
begin
  Result := ScrollStrategy.FocusNextRecord(-1, False);
end;

function TcxvgScroller.GoToNext: Boolean;
begin
  Result := ScrollStrategy.FocusNextRecord(FocusedRecordIndex, True);
end;

function TcxvgScroller.GoToPrev: Boolean;
begin
  Result := ScrollStrategy.FocusNextRecord(FocusedRecordIndex, False);
end;

function TcxvgScroller.GetBandIndexByRowIndex(
  ARowIndex: Integer): Integer;
begin
  Result := FScrollStrategy.GetBandIndexByRowIndex(ARowIndex);
end;

procedure TcxvgScroller.InitScrollBarsParameters;
begin
  with ScrollStrategy do
  begin
    InitHScrollBarParameters;
    InitVScrollBarParameters;
  end;
end;

procedure TcxvgScroller.RecalcBandsInfo;
begin
  ScrollStrategy.RecalcBandsInfo;
  if CheckDecrease then
  begin
    CheckDecreaseLeftIndex;
    CheckDecreaseTopIndex;
    ViewInfo.PrepareCalculateBandsInfo;
    ScrollStrategy.RecalcBandsInfo;
  end;
end;

procedure TcxvgScroller.RecreateScrollStrategy;
begin
  FreeAndNil(FScrollStrategy);
  FScrollStrategy := CreateScrollStrategy(Self);
  CheckDecrease := True;
end;

procedure TcxvgScroller.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollBarKind = sbHorizontal then
    FScrollStrategy.ScrollH(AScrollCode, AScrollPos)
  else
    FScrollStrategy.ScrollV(AScrollCode, AScrollPos);
end;

function TcxvgScroller.SetRecordVisible(ARecordIndex: Integer): Boolean;
begin
  Result := InRange(ARecordIndex, 0, VerticalGrid.RecordCount - 1) and
    ScrollStrategy.SetRecordVisible(ARecordIndex);
end;

procedure TcxvgScroller.SetRowMaxVisible(ARow: TcxCustomRow);
begin
  if (ARow = nil) or (ARow.VisibleIndex < 0) then Exit;
  if not ARow.Expanded then
    SetRowVisible(ARow)
  else
    ScrollStrategy.SetRowMaxVisible(ARow);
end;

procedure TcxvgScroller.SetRowVisible(ARow: TcxCustomRow);
var
  AIndex, ATopIndex, ACount, AStep: Integer;
begin
  if ARow = nil then Exit;
  AIndex := ARow.VisibleIndex;
  if AIndex < 0 then Exit;
  ATopIndex := TopVisibleRowIndex;
  AStep := 0;
  if AIndex < TopVisibleRowIndex then
    ATopIndex := AIndex
  else
    if TopVisibleRowIndex + VisibleRowCount <= AIndex then
    begin
      ACount := GetVisibleCountFromBottom(AIndex);
      if AIndex + 1 - ACount > TopVisibleRowIndex then
        ATopIndex := Max(0, AIndex - ACount + 1);
      AStep := 1;
    end;
  SetTopVisibleRowIndex(ScrollStrategy.CheckTopVisibleIndex(ATopIndex, AStep));
end;

procedure TcxvgScroller.BeginGestureScroll(APos: TPoint);
begin
  ScrollStrategy.BeginGestureScroll(APos);
end;

function TcxvgScroller.CreateScrollStrategy(
  AScroller: TcxvgScroller): TcxvgCustomScrollStrategy;
begin
  Result := VerticalGrid.GetScrollStrategyClass.Create(Self);
end;

procedure TcxvgScroller.LayoutStyleChanged;
begin
  CheckDecrease := True;
  VerticalGrid.LayoutChanged;
end;

procedure TcxvgScroller.RestoreLayout;
begin
  VerticalGrid.BeginUpdate;
  LeftVisibleRecord := FSaveLeftVisibleRecord;
  LeftVisibleBand := FSaveLeftVisibleBand;
  TopVisibleRowIndex := FSaveTopVisibleRowIndex;
  VerticalGrid.EndUpdate;
end;

procedure TcxvgScroller.SaveLayout;
begin
  FSaveTopVisibleRowIndex := TopVisibleRowIndex;
  FSaveLeftVisibleRecord := LeftVisibleRecord;
  FSaveLeftVisibleBand := LeftVisibleBand;
end;

procedure TcxvgScroller.CheckDecreaseLeftIndex;
begin
  if FCheckDecreaseLeftIndex then
  begin
    FScrollStrategy.CheckDecreaseLeftIndex;
    FCheckDecreaseLeftIndex := False;
  end;
end;

procedure TcxvgScroller.CheckDecreaseTopIndex;
begin
  if not FCheckDecreaseTopIndex or (TopVisibleRowIndex = 0) then Exit;
  FScrollStrategy.CheckDecreaseTopIndex(ViewInfo.ClientRect.Bottom - ViewInfo.ClientRect.Top);
  FCheckDecreaseTopIndex := False;
end;

function TcxvgScroller.GetBandsInfo: TBandInfoList;
begin
  Result := FScrollStrategy.FBandsInfo;
end;

function TcxvgScroller.GetCheckDecrease: Boolean;
begin
  Result := FCheckDecreaseLeftIndex or FCheckDecreaseTopIndex;
end;

function TcxvgScroller.GetFocusedRecordIndex: Integer;
begin
  Result := FVerticalGrid.DataController.FocusedRowIndex;
end;

function TcxvgScroller.GetLeftVisibleBand: Integer;
begin
  Result := FScrollStrategy.LeftVisibleBand;
end;

function TcxvgScroller.GetLeftVisibleRecord: Integer;
begin
  Result := FScrollStrategy.LeftVisibleRecord;
end;

function TcxvgScroller.GetPixelScrollRecordOffset: Integer;
begin
  Result := FScrollStrategy.PixelScrollRecordOffset;
end;

function TcxvgScroller.GetTopVisibleRowIndex: Integer;
begin
  Result := FScrollStrategy.TopVisibleRowIndex;
end;

function TcxvgScroller.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := FVerticalGrid.ViewInfo;
end;

function TcxvgScroller.GetVisibleCountFromBottom(
  ARowIndex: Integer): Integer;
begin
  Result := FScrollStrategy.GetVisibleCountFromBottom(ARowIndex, cxRectHeight(ViewInfo.ClientRect));
end;

function TcxvgScroller.GetVisibleRowCount: Integer;
begin
  Result := FScrollStrategy.GetVisibleRowCount;
end;

function TcxvgScroller.GetVisibleValueCount: Integer;
begin
  Result := FScrollStrategy.GetVisibleValueCount;
end;

procedure TcxvgScroller.SetCheckDecrease(Value: Boolean);
begin
  FCheckDecreaseTopIndex  := Value;
  FCheckDecreaseLeftIndex := Value;
end;

procedure TcxvgScroller.SetLeftVisibleBand(Value: Integer);
var
  AOldValue: Integer;
begin
  AOldValue := LeftVisibleBand;
  ScrollStrategy.LeftVisibleBand := Value;
  if AOldValue <> LeftVisibleBand then
    VerticalGrid.DoLeftVisibleBandIndexChanged;
end;

procedure TcxvgScroller.SetLeftVisibleRecord(
  Value: Integer);
var
  AOldValue: Integer;
begin
  AOldValue := LeftVisibleRecord;
  ScrollStrategy.LeftVisibleRecord := Value;
  if AOldValue <> LeftVisibleRecord then
    VerticalGrid.DoLeftVisibleRecordIndexChanged;
end;

procedure TcxvgScroller.SetTopVisibleRowIndex(Value: Integer);
var
  AOldValue: Integer;
begin
  AOldValue := TopVisibleRowIndex;
  ScrollStrategy.SetTopVisibleRowIndexAndBand(Value);
  if AOldValue <> TopVisibleRowIndex then
    VerticalGrid.DoTopRowIndexChanged;
end;

{ TBandInfoList }

constructor TBandInfoList.Create;
begin
  inherited Create(SizeOf(TBandInfo));
  Delta := 32;
end;

function TBandInfoList.Add(ABandIndex, ARowsCount, ABandHeight: Integer;
  AFirstRow: TcxCustomRow): Integer;
begin
  CheckCapacity;
  Result := FCount;
  Inc(FCount);
  with TBandInfo(Get(Result)^) do
  begin
    BandIndex := ABandIndex;
    RowsCount := ARowsCount;
    BandHeight := ABandHeight;
    FirstRow := AFirstRow;
  end;
end;

function TBandInfoList.GetItem(Index: Integer): TBandInfo;
begin
  Result := TBandInfo(Get(Index)^);
end;

{ TcxvgCustomScrollStrategy }

constructor TcxvgCustomScrollStrategy.Create(
  AScroller: TcxvgScroller);
begin
  FBandsInfo := TBandInfoList.Create;
  FScroller := AScroller;
end;

destructor TcxvgCustomScrollStrategy.Destroy;
begin
  FBandsInfo.Free;
  inherited Destroy;
end;

procedure TcxvgCustomScrollStrategy.CheckDecreaseLeftIndex;
begin
end;

procedure TcxvgCustomScrollStrategy.CheckDecreaseTopIndex(AScrollRectHeight: Integer);
var
  ACount, ATotal: Integer;
begin
  ATotal := ViewInfo.VisibleRowCount;
  ACount := GetVisibleCountFromBottom(ATotal - 1, AScrollRectHeight);
  if ATotal - ACount < FTopVisibleRowIndex then
    FTopVisibleRowIndex := ATotal - ACount;
end;

function TcxvgCustomScrollStrategy.CheckTopVisibleIndex(AIndex, AStep: Integer): Integer;
var
  ACount: Integer;
begin
  Result := AIndex;
  ACount := ViewInfo.VisibleRowCount;
  cxRange(Result, 0, ACount -
    GetVisibleCountFromBottom(ACount - 1, cxRectHeight(ViewInfo.ClientRect) - GetHScrollbarAreaHeight));
end;

function TcxvgCustomScrollStrategy.FindNextCustomItem(AFocusedItemIndex, AItemCount: Integer;
  AGoForward: Boolean; var AItemIndex: Integer): Boolean;

  function GetFromIndex: Integer;
  begin
    if AFocusedItemIndex = -1 then
      if AGoForward then
        Result := 0
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
        Result := False
      else
    else
      if AIndex < 0 then
        Result := False;
  end;

begin
  Result := False;
  if AItemCount = 0 then Exit;
  AItemIndex := GetFromIndex;
  Result := CheckIndex(AItemIndex);
end;

function TcxvgCustomScrollStrategy.FindNextRecord(AFocusedRecordIndex: Integer;
  AGoForward: Boolean): Integer;
begin
  if DataController.IsGridMode then
    if AGoForward then
      if not DataController.IsEOF and (AFocusedRecordIndex = DataController.RowCount - 1) then
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
  if not FindNextCustomItem(AFocusedRecordIndex, DataController.RowCount, AGoForward, Result) then
    Result := -1;
end;

function TcxvgCustomScrollStrategy.FocusNextRecord(AFocusedRecordIndex: Integer;
  AGoForward: Boolean): Boolean;

  procedure CheckEditing;
  begin
    if DataController.IsEditing then
    begin
      TcxEditingControllerAccess(Controller.EditingController).UpdateValue;
      if not (dceModified in DataController.EditState) then
      begin
        if DataController.EditState = [dceInsert] then
        begin
          Result := AGoForward xor DataController.IsEOF;
          if Result then DataController.Cancel;
        end;
        Exit;
      end;
      DataController.Post;
      AFocusedRecordIndex := DataController.FocusedRowIndex;
    end;
  end;

  procedure CheckGridMode;
  begin
    if DataController.IsGridMode and (AFocusedRecordIndex = -1) and AGoForward then
      DataController.GotoFirst
  end;

var
  ANewRecordIndex: Integer;

begin
  Result := False;
  CheckEditing;
  if Result then Exit;
  CheckGridMode;
  ANewRecordIndex := FindNextRecord(AFocusedRecordIndex, AGoForward);
  Result := ANewRecordIndex <> -1;
  if Result then
    DataController.FocusedRowIndex := ANewRecordIndex;
end;

function TcxvgCustomScrollStrategy.GetFirstRowByBandIndex(
  ABandIndex: Integer): TcxCustomRow;
begin
  if not cxInRange(ABandIndex, 0, BandsInfo.Count - 1) then
    Result := nil
  else
    Result := FBandsInfo[ABandIndex].FirstRow;
end;

procedure TcxvgCustomScrollStrategy.InitHScrollBarParameters;
begin
  VerticalGrid.SetScrollBarInfo(sbHorizontal, 0, DataScrollSize - 1, 1,
    VisibleDataScrollSize, ScrollBarPos, not IsHideHScrollBar, ViewInfo.CanHScrollBarHide);
end;

procedure TcxvgCustomScrollStrategy.InitVScrollBarParameters;
begin
  VerticalGrid.SetScrollBarInfo(sbVertical,
    0, ViewInfo.VisibleRowCount - 1, //min max
    1, VisibleRowCount, TopVisibleRowIndex, not IsHideVScrollBar, True);
end;

function TcxvgCustomScrollStrategy.GetVisibleCountFromBottom(
  ABottomIndex, AHeight: Integer): Integer;
begin
  Result := GetVisibleCount(ABottomIndex, AHeight, -1, False);
end;

function TcxvgCustomScrollStrategy.GetVisibleCountFromTop(
  ATopIndex, AHeight: Integer): Integer;
begin
  Result := GetVisibleCount(ATopIndex, AHeight, 1, True);
end;

procedure TcxvgCustomScrollStrategy.RecalcBandsInfo;
begin
  FBandsInfo.Clear;
  FBandsInfo.Add(0, ViewInfo.VisibleRowCount, -1, ViewInfo.VisibleRows[0]);
end;

procedure TcxvgCustomScrollStrategy.ScrollRecords(AForward: Boolean;
  ACount: Integer);
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  if not DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) or
    not AIntf.DoScroll(AForward) then
    if AForward then
      Scroller.LeftVisibleRecord := LeftVisibleRecord + ACount
    else
      Scroller.LeftVisibleRecord := LeftVisibleRecord - ACount;
end;

procedure TcxvgCustomScrollStrategy.SetRowMaxVisible(ARow: TcxCustomRow);
var
  N, ATopIndex, AChildCount, AGroupHeight, AClientHeight: Integer;
begin
  if ARow = nil then Exit;
  AGroupHeight := GetFullRowHeight(ARow, AChildCount);
  AClientHeight := BandsInfo.Count * cxRectHeight(ViewInfo.ClientRect);
  if AGroupHeight >= AClientHeight then
    SetTopVisibleRowIndexAndBand(ARow.VisibleIndex)
  else
  begin
    N := GetVisibleCountFromTop(FTopVisibleRowIndex, cxRectHeight(ViewInfo.ClientRect));
    ATopIndex := ARow.VisibleIndex + AChildCount - N + 1;
    if FTopVisibleRowIndex < ATopIndex then
      SetTopVisibleRowIndexAndBand(ATopIndex);
  end
end;

procedure TcxvgCustomScrollStrategy.BeginGestureScroll(APos: TPoint);
begin
end;

function TcxvgCustomScrollStrategy.CanContinueForward(Index: Integer): Boolean;
begin
  Result := Index < ViewInfo.VisibleRowCount;
end;

function TcxvgCustomScrollStrategy.CanContinueBackward(Index: Integer): Boolean;
begin
  Result := Index > -1;
end;

function TcxvgCustomScrollStrategy.CanCalcRowsOnTheNextBand(ALeft,
  ANextBandIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxvgCustomScrollStrategy.CanRecordPixelScrolling: Boolean;
begin
  Result := False;
end;

procedure TcxvgCustomScrollStrategy.CheckLeftVisibleRecordAndOffset(var ALeftVisibleRecord, APixelScrollOffset: Integer);
var
  AIsGridMode: Boolean;
begin
  if FCheckingCoordinate or ViewInfo.IsDirty then Exit;
  FCheckingCoordinate := True;
  with DataController do
  try
    AIsGridMode := IsGridMode;
    if ALeftVisibleRecord < 0 then
    begin
      if AIsGridMode and not IsBOF then
        Scroll(ALeftVisibleRecord);
      ALeftVisibleRecord := 0;
    end;
    if ALeftVisibleRecord > RowCount - 1 then
    begin
      if AIsGridMode and not IsEOF then
        Scroll(ALeftVisibleRecord - (RowCount - 1));
      ALeftVisibleRecord := RowCount - 1;
    end;
    if ALeftVisibleRecord <= 0 then Exit;
    DoCheckLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollOffset);
  finally
    FCheckingCoordinate := False;
  end;
end;

function TcxvgCustomScrollStrategy.DataScrollSize: Integer;

var
  AScrollRecordCount, AScrollPos: Integer;

  procedure GetScrollRecordParams;
  var
    AIntf: IcxVerticalGridDBDataContoller;

  begin
    if DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) then
    begin
      AScrollRecordCount := AIntf.GetScrollBarRecordCount;
      AScrollPos := AIntf.GetScrollBarPos;
    end
    else
    begin
      AScrollRecordCount := -1;
      AScrollPos := -1;
    end;
    if AScrollRecordCount = -1 then
      AScrollRecordCount := DataController.RowCount + ScrollBarOffsetBegin + ScrollBarOffsetEnd;
    if AScrollPos = -1 then
      AScrollPos := LeftVisibleRecord + ScrollBarOffsetBegin;
  end;

begin
  GetScrollRecordParams;
  Result := AScrollRecordCount;
end;

procedure TcxvgCustomScrollStrategy.DoCheckLeftVisibleRecordAndOffset(var ALeftVisibleRecord, APixelScrollOffset: Integer);
var
  AMaxValue: Integer;
begin
  AMaxValue := DataController.RowCount - VisibleValueCount;
  if ALeftVisibleRecord > AMaxValue then
  begin
    if DataController.IsGridMode and not DataController.IsEOF and not (dceInsert in DataController.EditState) then
      DataController.Scroll(ALeftVisibleRecord - AMaxValue);
    ALeftVisibleRecord := AMaxValue;
  end;
end;

function TcxvgCustomScrollStrategy.DoGetScrollBarPos: Integer;
begin
  Result := LeftVisibleRecord + ScrollBarOffsetBegin;
end;

procedure TcxvgCustomScrollStrategy.DoSetLeftVisibleRecordAndOffset(
  ALeftVisibleRecord, APixelScrollRecordOffset: Integer);
begin
  if (ALeftVisibleRecord <> -1) and
    ((FLeftVisibleRecord <> ALeftVisibleRecord) or
     (FLeftVisibleRecord = ALeftVisibleRecord) and (FPixelScrollRecordOffset <> APixelScrollRecordOffset)) then
  begin
    VerticalGrid.ShowTouchScrollUI(VerticalGrid, True);
    FLeftVisibleRecord := ALeftVisibleRecord;
    FPixelScrollRecordOffset := APixelScrollRecordOffset;
    VerticalGrid.LayoutChanged;
  end;
end;

procedure TcxvgCustomScrollStrategy.DoSetScrollBarPos(Value: Integer);
begin
  Scroller.LeftVisibleRecord := Value - ScrollBarOffsetBegin;
end;

function TcxvgCustomScrollStrategy.GetBandWidth: Integer;
begin
  Result := ViewInfo.FViewBandWidth;
end;

function TcxvgCustomScrollStrategy.GetBottomVisibleChild(
  ARow: TcxCustomRow): TcxCustomRow;
begin
  if (ARow = nil) or not (ARow.Expanded and ARow.Visible) then
  begin
    Result := nil;
    Exit
  end;
  Result := ARow.GetLastVisibleChild;
end;

function TcxvgCustomScrollStrategy.GetFullRowHeight(ARow: TcxCustomRow;
  out ChildCount: Integer): Integer;
var
  I, AIndex: Integer;
  AChild: TcxCustomRow;
begin
  ChildCount := 0;
  Result := 0;
  if ARow = nil then Exit;
  with ViewInfo do
  begin
    Result := ARow.ViewInfo.CalculatedHeight + HorzLineWidth;
    AIndex := ARow.VisibleIndex;
    if AIndex >= 0 then
      for I := AIndex + 1 to VisibleRowCount - 1 do
      begin
        AChild := VisibleRows[I];
        if ARow.IsParent(AChild) then
        begin
          Inc(ChildCount);
          Inc(Result, AChild.ViewInfo.CalculatedHeight + HorzLineWidth);
        end;
      end;
  end;
end;

function TcxvgCustomScrollStrategy.GetHScrollbarAreaHeight: Integer;
begin
  Result := 0;
  if (VerticalGrid.GetScrollbarMode = sbmHybrid) and
    (VerticalGrid.IsScrollBarActive(sbHorizontal)) then
    Result := VerticalGrid.GetHScrollBarAreaHeight;
end;

function TcxvgCustomScrollStrategy.GetScrollBarOffsetBegin: Integer;
begin
  with DataController do
  begin
    if IsGridMode and IsRecordsScrollMode then
      Result := Ord(not IsBOF)
    else
      Result := 0;
  end;
end;

function TcxvgCustomScrollStrategy.GetScrollBarOffsetEnd: Integer;
begin
  with DataController do
  begin
    if IsGridMode and IsRecordsScrollMode then
      Result := Ord(not IsEOF)
    else
      Result := 0;
  end;
end;

function TcxvgCustomScrollStrategy.GetVisibleCount(ABeginIndex, AAreaHeight,
  AStep: Integer; AForward: Boolean): Integer;

  function CheckContinue(AIndex: Integer): Boolean;
  begin
    if AForward then
      Result := CanContinueForward(AIndex)
    else
      Result := CanContinueBackward(AIndex)
  end;

var
  ARowCount, ARowHeight, ABandCount, ALeft, ABandWidth, ABandHeight: Integer;
  ARowViewInfo: TcxCustomRowViewInfo;
begin
  Result      := 0;
  ARowCount   := 0;
  ABandCount  := 0;
  ABandWidth := BandWidth;
  if ABandWidth <= 0 then Exit;
  with ViewInfo do
  begin
    ABandHeight := HorzLineWidth;
    ALeft := ClientRect.Left;
    while CheckContinue(ABeginIndex) do
    begin
      ARowViewInfo := VisibleRows[ABeginIndex].ViewInfo;
      ARowHeight := ARowViewInfo.CalculatedHeight;
      if ABandHeight + ARowHeight + HorzLineWidth > AAreaHeight then
      begin
        if ARowCount = 0 then ARowCount := 1;
        Inc(Result, ARowCount);
        ARowCount := 0;
        ABandHeight := HorzLineWidth;
        Inc(ALeft, ABandWidth + BandInterval);
        Inc(ABandCount);
        if not CanCalcRowsOnTheNextBand(ALeft, ABandCount) then break;
      end;
      Inc(ABandHeight, ARowHeight + HorzLineWidth);
      Inc(ARowCount);
      Inc(ABeginIndex, AStep);
    end;
  end;
  Inc(Result, ARowCount);
end;

function TcxvgCustomScrollStrategy.IsBehindRightClientEdge(X: Integer): Boolean;
begin
 Result := X > (ViewInfo.ClientRect.Right - ViewInfo.VertLineWidth);
end;

function TcxvgCustomScrollStrategy.IsHideHScrollBar: Boolean;
begin
  with VerticalGrid do
    Result := (ClientHeight - 2 < HScrollBar.Height) or
      (IsRecordsScrollMode and not Controller.CanChangeRecord);
end;

function TcxvgCustomScrollStrategy.IsHideVScrollBar: Boolean;
begin
  Result := cxRectWidth(ViewInfo.ClientRect) - 2 < VerticalGrid.VScrollBar.Width;
end;

function TcxvgCustomScrollStrategy.IsRecordsScrollMode: Boolean;
begin
  Result := True;
end;

procedure TcxvgCustomScrollStrategy.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
end;

procedure TcxvgCustomScrollStrategy.ScrollH(
  AScrollCode: TScrollCode; var AScrollPos: Integer);

  procedure DoScrollRecords;
  begin
    case AScrollCode of
      scLineUp:
        ScrollRecords(False, 1);
      scLineDown:
        ScrollRecords(True, 1);
      scPageUp:
        Scroller.LeftVisibleRecord := LeftVisibleRecord - VisibleValueCount;
      scPageDown:
        Scroller.LeftVisibleRecord := LeftVisibleRecord + VisibleValueCount;
      scTrack:
        if not DataController.IsGridMode then
          ScrollBarPos := AScrollPos;
      scPosition:
        if DataController.IsGridMode then
          ScrollBarPos := AScrollPos;
    end;
    AScrollPos := ScrollBarPos;
  end;

  procedure DoScrollBands;
  begin
    case AScrollCode of
      scLineUp:
        Scroller.LeftVisibleBand := LeftVisibleBand - 1;
      scLineDown:
        Scroller.LeftVisibleBand := LeftVisibleBand + 1;
      scPageUp:
        Scroller.LeftVisibleBand := LeftVisibleBand - VisibleBandCount;
      scPageDown:
        Scroller.LeftVisibleBand := LeftVisibleBand + VisibleBandCount;
      scTrack:
        Scroller.LeftVisibleBand := AScrollPos;
    end;
    AScrollPos := LeftVisibleBand;
  end;

begin
  if IsRecordsScrollMode then
    DoScrollRecords
  else
    DoScrollBands;
end;

procedure TcxvgCustomScrollStrategy.ScrollV(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollCode of
    scLineUp:
      Scroller.TopVisibleRowIndex := TopVisibleRowIndex - 1;
    scLineDown:
      Scroller.TopVisibleRowIndex := TopVisibleRowIndex + 1;
    scPageUp:
      Scroller.TopVisibleRowIndex := TopVisibleRowIndex - VisibleRowCount;
    scPageDown:
      Scroller.TopVisibleRowIndex := TopVisibleRowIndex + VisibleRowCount;
    scTrack:
      Scroller.TopVisibleRowIndex := AScrollPos;
  end;
  AScrollPos := TopVisibleRowIndex;
end;

procedure TcxvgCustomScrollStrategy.SetLeftVisibleRecord(Value: Integer);
begin
  SetLeftVisibleRecordAndOffset(Value, 0);
end;

procedure TcxvgCustomScrollStrategy.SetLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset: Integer);
begin
  CheckLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset);
  DoSetLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset);
end;

function TcxvgCustomScrollStrategy.SetRecordVisible(ARecordIndex: Integer): Boolean;
var
  ACount, AOldLeftVisibleRecord: Integer;
begin
  AOldLeftVisibleRecord := LeftVisibleRecord;
  if (ARecordIndex < AOldLeftVisibleRecord) then
    LeftVisibleRecord := ARecordIndex
  else
  begin
    ACount := VisibleValueCount;
    if AOldLeftVisibleRecord + ACount <= ARecordIndex then
      LeftVisibleRecord := ARecordIndex - ACount + 1;
  end;
  Result := LeftVisibleRecord <> AOldLeftVisibleRecord;
end;

procedure TcxvgCustomScrollStrategy.SetTopVisibleRowIndexAndBand(Index: Integer);
begin
  Index := CheckTopVisibleIndex(Index, 0);
  SetTopVisibleRowIndex(Index);
end;

function TcxvgCustomScrollStrategy.VisibleDataScrollSize: Integer;
begin
  Result := VisibleValueCount;
end;

function TcxvgCustomScrollStrategy.GetController: TcxvgController;
begin
  Result := VerticalGrid.Controller;
end;

function TcxvgCustomScrollStrategy.GetDataController: TcxCustomDataController;
begin
  Result := VerticalGrid.DataController;
end;

function TcxvgCustomScrollStrategy.GetScrollBarPos: Integer;
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  if DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) then
    Result := AIntf.GetScrollBarPos
  else
    Result := -1;
  if Result = -1 then
    Result := DoGetScrollBarPos;
end;

function TcxvgCustomScrollStrategy.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Scroller.VerticalGrid;
end;

function TcxvgCustomScrollStrategy.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := Scroller.ViewInfo;
end;

function TcxvgCustomScrollStrategy.GetVisibleRowCount: Integer;
begin
  Result := GetVisibleCountFromTop(TopVisibleRowIndex, cxRectHeight(ViewInfo.ClientRect) - GetHScrollbarAreaHeight);
end;

function TcxvgCustomScrollStrategy.GetVScrollbarAreaWidth: Integer;
begin
  Result := 0;
  if (VerticalGrid.GetScrollbarMode = sbmHybrid) and
    (VerticalGrid.IsScrollBarActive(sbVertical)) then
    Result := VerticalGrid.GetVScrollBarAreaWidth;
end;

procedure TcxvgCustomScrollStrategy.SetScrollBarPos(Value: Integer);
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  if not DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) or
    not AIntf.SetScrollBarPos(Value) then
      DoSetScrollBarPos(Value);
end;

procedure TcxvgCustomScrollStrategy.SetTopVisibleRowIndex(Value: Integer);
begin
  if FTopVisibleRowIndex <> Value then
  begin
    VerticalGrid.ShowTouchScrollUI(VerticalGrid, True);
    FTopVisibleRowIndex := Value;
    VerticalGrid.LayoutChanged;
  end;
end;

{ TcxvgSingleRecordScrollStrategy }

function TcxvgSingleRecordScrollStrategy.GetBandIndexByRowIndex(
  ARowIndex: Integer): Integer;
begin
  Result := 0;
end;

function TcxvgSingleRecordScrollStrategy.GetBandInterval: Integer;
begin
  Result := 0;
end;

function TcxvgSingleRecordScrollStrategy.GetLeftVisibleBand: Integer;
begin
  Result := 0;
end;

function TcxvgSingleRecordScrollStrategy.GetVisibleBandCount: Integer;
begin
  Result := 1;
end;

function TcxvgSingleRecordScrollStrategy.GetVisibleValueCount: Integer;
begin
  Result := 1;
end;

procedure TcxvgSingleRecordScrollStrategy.SetLeftVisibleBand(Value: Integer);
begin
end;

{ TcxvgMultiRecordsScrollStrategy }

procedure TcxvgMultiRecordsScrollStrategy.CheckDecreaseLeftIndex;
var
  AVisibleCount: Integer;
  ALeftVisibleRecordIndex, APixelScrollRecordOffset: Integer;
begin
  if IsRecordPixelScrolling then
  begin
    ALeftVisibleRecordIndex := LeftVisibleRecord;
    APixelScrollRecordOffset := PixelScrollRecordOffset;
    DoCheckLeftVisibleRecordAndOffset(ALeftVisibleRecordIndex, APixelScrollRecordOffset);
    DoSetLeftVisibleRecordAndOffset(ALeftVisibleRecordIndex, APixelScrollRecordOffset);
  end
  else
  begin
    AVisibleCount := VisibleValueCount;
    if LeftVisibleRecord + AVisibleCount > VerticalGrid.RecordCount then
      FLeftVisibleRecord := Max(0, VerticalGrid.RecordCount - AVisibleCount);
  end;
end;

function TcxvgMultiRecordsScrollStrategy.GetBandIndexByRowIndex(
  ARowIndex: Integer): Integer;
begin
  Result := 0;
end;

procedure TcxvgMultiRecordsScrollStrategy.BeginGestureScroll(APos: TPoint);
begin
  UpdatePixelScrollLeftRecordIndexAndOffsetMaxValues;
end;

procedure TcxvgMultiRecordsScrollStrategy.CalculatePixelScrollInfo(
  var ARecordIndex, ARecordOffset: Integer; AMaxRecordIndex, AMaxRecordOffset: Integer;
  ADelta: Integer; out AOverPan: Boolean);
var
  ARecordScrollSize, ASize: Integer;
begin
  AOverPan := False;
  ARecordScrollSize := ViewInfo.ViewValueWidth + GetRecordSpace;
  if ADelta < 0 then
  begin
    ASize := -ADelta - ARecordOffset;
    ARecordIndex := ARecordIndex + ASize div ARecordScrollSize;
    ARecordOffset := - ASize mod ARecordScrollSize;
    if (ARecordIndex > AMaxRecordIndex) or
      ((ARecordIndex = AMaxRecordIndex) and (ARecordOffset < AMaxRecordOffset)) then
    begin
      ARecordIndex := AMaxRecordIndex;
      ARecordOffset := AMaxRecordOffset;
      AOverPan := True;
    end;
  end
  else
  begin
    ASize := ADelta + ARecordOffset;
    if ASize > 0 then
    begin
      ARecordIndex := ARecordIndex - RoundDiv(ASize, ARecordScrollSize);
      ARecordOffset := Abs(ASize mod ARecordScrollSize) - ARecordScrollSize;
    end
    else
      ARecordOffset := ASize;
    if ARecordIndex < 0 then
    begin
      ARecordIndex := 0;
      ARecordOffset := 0;
      AOverPan := True;
    end;
  end;
end;

function TcxvgMultiRecordsScrollStrategy.CanRecordPixelScrolling: Boolean;
var
  AIntf: IcxVerticalGridDBDataContoller;
begin
  Result := not DataController.IsGridMode and
    (not DataController.GetInterface(IcxVerticalGridDBDataContoller, AIntf) or
    AIntf.IsRecordPixelScrollingSupported);
end;

procedure TcxvgMultiRecordsScrollStrategy.CheckMaxLeftVisibleRecordIndexAndOffset(
  var ALeftVisibleRecordIndex, APixelScrollRecordOffset: Integer);
begin

end;

function TcxvgMultiRecordsScrollStrategy.DataScrollSize: Integer;
begin
  if IsPixelBasedScrollDataPos then
    Result := (ViewInfo.ViewValueWidth + GetRecordSpace) * (VerticalGrid.RecordCount - 1) +
      ViewInfo.ViewValueWidth + ViewInfo.VertLineWidth + GetVScrollbarAreaWidth
  else
    Result := inherited DataScrollSize;
end;

procedure TcxvgMultiRecordsScrollStrategy.DoCheckLeftVisibleRecordAndOffset(
  var ALeftVisibleRecord, APixelScrollOffset: Integer);
begin
  if IsRecordPixelScrolling then
  begin
    UpdatePixelScrollLeftRecordIndexAndOffsetMaxValues;
    if (ALeftVisibleRecord > FMaxLeftVisibleRecord) or
      (ALeftVisibleRecord = FMaxLeftVisibleRecord) and (APixelScrollOffset < FMaxPixelScrollRecordOffset) then
    begin
      ALeftVisibleRecord := FMaxLeftVisibleRecord;
      APixelScrollOffset := FMaxPixelScrollRecordOffset;
    end;
  end
  else
    inherited DoCheckLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollOffset);
end;

function TcxvgMultiRecordsScrollStrategy.DoGetScrollBarPos: Integer;
begin
  if IsPixelBasedScrollDataPos then
    Result := LeftVisibleRecord * (ViewInfo.ViewValueWidth + GetRecordSpace) - PixelScrollRecordOffset
  else
    Result := inherited DoGetScrollBarPos;
end;

procedure TcxvgMultiRecordsScrollStrategy.DoSetScrollBarPos(Value: Integer);
var
  AOverPan: Boolean;
  ARecordIndex, AOffset: Integer;
begin
  if IsPixelBasedScrollDataPos then
  begin
    ARecordIndex := 0;
    AOffset := 0;
    CalculatePixelScrollInfo(ARecordIndex, AOffset, MaxInt, 1, -Value, AOverPan);
    DoSetLeftVisibleRecordAndOffset(ARecordIndex, AOffset);
  end
  else
    inherited DoSetScrollBarPos(Value);
end;

function TcxvgMultiRecordsScrollStrategy.GetBandInterval: Integer;
begin
  Result := 0;
end;

function TcxvgMultiRecordsScrollStrategy.GetLeftVisibleBand: Integer;
begin
  Result := 0;
end;

procedure TcxvgMultiRecordsScrollStrategy.GetPixelScrollLeftRecordIndexAndOffsetByRightRecord(
  ARightRecord: Integer; out ALeftRecord, APixelScrollRecordOffset: Integer);
var
  AOverpan: Boolean;
begin
  ALeftRecord := ARightRecord;
  APixelScrollRecordOffset := -ViewInfo.ViewValueWidth - GetRecordSpace;
  CalculatePixelScrollInfo(ALeftRecord, APixelScrollRecordOffset, MaxInt, 1, ViewInfo.GetPixelScrollContentSize + GetRecordSpace, AOverpan);
end;

function TcxvgMultiRecordsScrollStrategy.GetVisibleBandCount: Integer;
begin
  Result := 1;
end;

function TcxvgMultiRecordsScrollStrategy.GetVisibleValueCount: Integer;
var
  ACount, ARecordSpace, ARecordsWidth: Integer;
begin
  with ViewInfo do
  begin
    ARecordSpace := GetRecordSpace;
    ARecordsWidth := ViewInfo.GetPixelScrollContentSize + ARecordSpace - ViewInfo.VertLineWidth - PixelScrollRecordOffset;
    ACount := Max(1, (ARecordsWidth - GetVScrollbarAreaWidth) div (ViewValueWidth + ARecordSpace));
  end;
  Result := Min(ACount, VerticalGrid.RecordCount);
end;

function TcxvgMultiRecordsScrollStrategy.IsPixelBasedScrollDataPos: Boolean;
begin
  Result := IsRecordPixelScrolling and (VerticalGrid.RecordCount < ViewInfo.GetPixelScrollContentSize);
end;

function TcxvgMultiRecordsScrollStrategy.IsRecordPixelScrolling: Boolean;
begin
  Result := VerticalGrid.IsRecordPixelScrolling;
end;

procedure TcxvgMultiRecordsScrollStrategy.ScrollContentByGesture(
  AScrollKind: TScrollBarKind; ADelta: Integer);

  procedure DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer);
  var
    AOverpan: TPoint;
  begin
    if AScrollKind = sbHorizontal then
      AOverpan := Point(ADelta, 0)
    else
      AOverpan := Point(0, ADelta);
    VerticalGrid.CheckOverpan(AScrollKind, 0, 1, -1, AOverpan.X, AOverpan.Y);
  end;

var
  ALeftVisibleRecord, APixelScrollRecordOffset: Integer;
  AOverPan: Boolean;
begin
  if (ADelta = 0) or (VerticalGrid.RecordCount = 0) then Exit;
  ALeftVisibleRecord := LeftVisibleRecord;
  APixelScrollRecordOffset := PixelScrollRecordOffset;
  CalculatePixelScrollInfo(ALeftVisibleRecord, APixelScrollRecordOffset, FMaxLeftVisibleRecord,
    FMaxPixelScrollRecordOffset, ADelta, AOverPan);
  DoSetLeftVisibleRecordAndOffset(ALeftVisibleRecord, APixelScrollRecordOffset);
  ViewInfo.Control.Repaint;
  if AOverPan then
    DoOverpan(sbHorizontal, ADelta);
end;

procedure TcxvgMultiRecordsScrollStrategy.SetLeftVisibleBand(Value: Integer);
begin
end;

function TcxvgMultiRecordsScrollStrategy.SetRecordVisible(ARecordIndex: Integer): Boolean;
var
  AOldLeftVisibleRecord, AOldRecordPixelScrollOffset: Integer;
  ANewRecordIndex, ANewRecordPixelScrollOffset: Integer;
begin
  if IsRecordPixelScrolling then
  begin
    AOldLeftVisibleRecord := LeftVisibleRecord;
    AOldRecordPixelScrollOffset := PixelScrollRecordOffset;
    if (ARecordIndex < LeftVisibleRecord) or
      ((ARecordIndex = LeftVisibleRecord) and (PixelScrollRecordOffset <> 0)) then
      SetLeftVisibleRecordAndOffset(ARecordIndex, 0)
    else
      if LeftVisibleRecord + VisibleValueCount <= ARecordIndex then
      begin
        GetPixelScrollLeftRecordIndexAndOffsetByRightRecord(ARecordIndex, ANewRecordIndex, ANewRecordPixelScrollOffset);
        SetLeftVisibleRecordAndOffset(ANewRecordIndex, ANewRecordPixelScrollOffset);
      end;
    Result := (LeftVisibleRecord <> AOldLeftVisibleRecord) or (PixelScrollRecordOffset <> AOldRecordPixelScrollOffset);
  end
  else
    Result := inherited SetRecordVisible(ARecordIndex);
end;

procedure TcxvgMultiRecordsScrollStrategy.UpdatePixelScrollLeftRecordIndexAndOffsetMaxValues;
var
  AOverpan: Boolean;
begin
  FMaxLeftVisibleRecord := VerticalGrid.RecordCount - 1;
  FMaxPixelScrollRecordOffset := -ViewInfo.ViewValueWidth - GetRecordSpace - GetVScrollbarAreaWidth;
  CalculatePixelScrollInfo(FMaxLeftVisibleRecord, FMaxPixelScrollRecordOffset, MaxInt, 1, ViewInfo.GetPixelScrollContentSize + GetRecordSpace, AOverpan);
end;

function TcxvgMultiRecordsScrollStrategy.VisibleDataScrollSize: Integer;
begin
  if IsPixelBasedScrollDataPos then
    Result := ViewInfo.GetPixelScrollContentSize
  else
    Result := inherited VisibleDataScrollSize;
end;

function TcxvgMultiRecordsScrollStrategy.GetRecordsInterval: Integer;
begin
  Result := TcxMultiRecordViewInfo(ViewInfo).RecordsInterval;
end;

function TcxvgMultiRecordsScrollStrategy.GetRecordSpace: Integer;
begin
  Result := IfThen(RecordsInterval = 0, ViewInfo.VertLineWidth,
    2 * ViewInfo.VertLineWidth + RecordsInterval);
end;

{ TcxvgBandsScrollStrategy }

procedure TcxvgBandsScrollStrategy.CheckDecreaseTopIndex(AScrollRectHeight: Integer);
var
  ALeftBand: Integer;
begin
  ALeftBand := Max(0, BandsInfo.Count - VisibleBandCount);
  if ALeftBand < FLeftVisibleBand then
    InternalSetLeftVisibleBand(ALeftBand, True);
end;

function TcxvgBandsScrollStrategy.CheckTopVisibleIndex(
  AIndex, AStep: Integer): Integer;
var
  ARow: TcxCustomRow;
  ABandIndex: Integer;
begin
  ARow := ViewInfo.VisibleRows[AIndex];
  ABandIndex := GetBandIndexByRowIndex(AIndex);
  if ARow <> GetFirstRowByBandIndex(ABandIndex) then
  begin
    Inc(ABandIndex, AStep);
    ARow := GetFirstRowByBandIndex(ABandIndex);
  end;
  if ARow <> nil then
  begin
    AIndex := ARow.VisibleIndex;
    InternalSetLeftVisibleBand(ABandIndex, False);
  end;
  Result := AIndex;
end;

function TcxvgBandsScrollStrategy.GetBandIndexByRowIndex(
  ARowIndex: Integer): Integer;
var
  I, AFirstIndex: Integer;
  ABandInfo: TBandInfo;
begin
  Result := -1;
  for I := 0 to FBandsInfo.Count - 1 do
  begin
    ABandInfo := BandsInfo[I];
    AFirstIndex := ABandInfo.FirstRow.VisibleIndex;
    if (ARowIndex >= AFirstIndex) and (ARowIndex < AFirstIndex + ABandInfo.RowsCount) then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TcxvgBandsScrollStrategy.InitHScrollBarParameters;
begin
  if IsRecordsScrollMode then
    inherited InitHScrollBarParameters
  else
    VerticalGrid.SetScrollBarInfo(sbHorizontal, 0, BandsInfo.Count - 1, 1,
      VisibleBandCount, LeftVisibleBand, not IsHideHScrollBar, True);
end;

function TcxvgBandsScrollStrategy.IsHideVScrollBar: Boolean;
begin
  Result := True;
end;

procedure TcxvgBandsScrollStrategy.RecalcBandsInfo;
var
  I, ARowCount, AHeight, ABandIndex, AClientHeight: Integer;
  ARow, AFirstRow: TcxCustomRow;
  ARowViewInfo: TcxCustomRowViewInfo;
begin
  FBandsInfo.Clear;
  ARowCount := 0;
  ABandIndex := 0;
  with ViewInfo do
  begin
    AHeight := HorzLineWidth;
    ViewValueWidth := GetViewValueWidth;
    AClientHeight := cxRectHeight(ClientRect); // for LayoutStyleChanged
    I := 0;
    while I < VisibleRowCount do
    begin
      ARow := VisibleRows[I];
      ARowViewInfo := ARow.ViewInfo;
      if AHeight + ARowViewInfo.CalculatedHeight + HorzLineWidth > AClientHeight then
      begin
        AFirstRow := VisibleRows[I - ARowCount];
        if ARowCount = 0 then
        begin
          ARowCount := 1;
          AFirstRow := VisibleRows[I];
          AHeight := ARowViewInfo.CalculatedHeight + HorzLineWidth;
        end
        else Dec(I);
        BandsInfo.Add(ABandIndex, ARowCount, AHeight, AFirstRow);
        AHeight := HorzLineWidth;
        ARowCount := 0;
        Inc(ABandIndex);
        Inc(I);
        continue;
      end;
      Inc(AHeight, ARowViewInfo.CalculatedHeight + HorzLineWidth);
      Inc(ARowCount);
      Inc(I);
    end;
    if ARowCount <> 0 then
      BandsInfo.Add(ABandIndex, ARowCount, AHeight, VisibleRows[I - ARowCount]);
  end;
end;

procedure TcxvgBandsScrollStrategy.SetRowMaxVisible(ARow: TcxCustomRow);
begin
  SetTopVisibleRowIndexAndBand(GetBandViewRowMaxVisibleTopIndex(ARow));
end;

function TcxvgBandsScrollStrategy.CanCalcRowsOnTheNextBand(
  ALeft, ANextBandIndex: Integer): Boolean;
var
  X: Integer;
begin
  Result := True;
  if IsBehindRightClientEdge(ALeft + BandWidth + ViewInfo.BandsInterval + 1) then
  begin
    if VerticalGrid.OptionsView.AutoScaleBands then
    begin
      X := (ALeft + BandWidth) - ViewInfo.ClientRect.Right;
      if X > ANextBandIndex then Result := False;
    end
    else
      Result := False;
  end;
end;

function TcxvgBandsScrollStrategy.GetBandInterval: Integer;
begin
  Result := ViewInfo.BandsInterval;
end;

function TcxvgBandsScrollStrategy.GetBandViewRowMaxVisibleTopIndex(
  ARow: TcxCustomRow): Integer;
var
  AFirstBandIndex, ALastBandIndex, AVisBandsCount: Integer;
  ABottomChild: TcxCustomRow;
begin
  AFirstBandIndex := GetBandIndexByRowIndex(ARow.VisibleIndex);
  ABottomChild := GetBottomVisibleChild(ARow);
  if (ABottomChild <> nil) and (ABottomChild <> ARow) then
  begin
    ALastBandIndex := GetBandIndexByRowIndex(ABottomChild.VisibleIndex);
    AVisBandsCount := VisibleBandCount;
    if ALastBandIndex - AFirstBandIndex > AVisBandsCount then
    begin
      Result := ARow.VisibleIndex;
      Exit;
    end;
    if AFirstBandIndex + AVisBandsCount > ALastBandIndex then
    begin
      AFirstBandIndex := Max(0, ALastBandIndex - AVisBandsCount + 1);
      Result := GetFirstRowByBandIndex(AFirstBandIndex).VisibleIndex;
      Exit;
    end;
  end;
  Result := ARow.VisibleIndex;
end;

function TcxvgBandsScrollStrategy.GetLeftVisibleBand: Integer;
begin
  Result := FLeftVisibleBand;
end;

function TcxvgBandsScrollStrategy.GetVisibleBandCount: Integer;
var
  W, AIndents: Integer;
begin
  W := Max(1, ViewInfo.ViewBandWidth); // W = 0 ???
  AIndents := BandsInfo.Count - 1 * TcxBandsViewInfo(ViewInfo).BandsInterval;
  Result := Min(BandsInfo.Count, Max(1, (cxRectWidth(ViewInfo.ClientRect) - AIndents) div W));
end;

function TcxvgBandsScrollStrategy.GetVisibleValueCount: Integer;
begin
  Result := 1;
end;

function TcxvgBandsScrollStrategy.IsRecordsScrollMode: Boolean;
begin
  Result := Controller.CanChangeRecord;
end;

function TcxvgBandsScrollStrategy.InternalSetLeftVisibleBand(ABandIndex: Integer;
  ACheckTopVisibleRow: Boolean): Boolean;
begin
  Result := FLeftVisibleBand <> ABandIndex;
  if Result then
  begin
    FLeftVisibleBand := ABandIndex;
    if ACheckTopVisibleRow then
      CheckTopVisibleRowIndex;
    VerticalGrid.DoLeftVisibleBandIndexChanged;
  end;
end;

procedure TcxvgBandsScrollStrategy.SetLeftVisibleBand(Value: Integer);
begin
  cxRange(Value, 0, BandsInfo.Count - VisibleBandCount);
  if InternalSetLeftVisibleBand(Value, True) then
    VerticalGrid.LayoutChanged;
end;

procedure TcxvgBandsScrollStrategy.CheckTopVisibleRowIndex;
var
  AFirstBandRow: TcxCustomRow;
begin
  AFirstBandRow := GetFirstRowByBandIndex(FLeftVisibleBand);
  if AFirstBandRow = nil then
    FTopVisibleRowIndex := 0
  else
    FTopVisibleRowIndex := AFirstBandRow.VisibleIndex;
end;

{ TcxvgPainter }

procedure TcxvgPainter.DrawBackground;
var
  I: Integer;
  AParams: TcxViewParams;
  R: TRect;
begin
  AParams := VerticalGrid.Styles.GetBackgroundParams;
  with ViewInfo.ViewRects.EmptyRects do
    for I := 0 to Count - 1 do
    begin
      R := Rects[I];
      if not VerticalGrid.DoDrawBackgroundPart(R, AParams) then
      begin
        Canvas.Brush.Color := AParams.Color;
        Canvas.FillRect(R, AParams.Bitmap);
      end;
    end;
end;

procedure TcxvgPainter.DoPaint;
begin
  DrawBackground;
  DrawRows;
  with ViewInfo do
    DrawLines(LinesInfo, ClientRect);
  DrawStyleFeatures;
  DrawFindPanel;
  DrawFilterBox;
  DrawNavigator;
end;

procedure TcxvgPainter.DrawButton(ARowHeader: TcxCustomRowHeaderInfo);
begin
  if not cxRectIsEmpty(ARowHeader.ButtonRect) then
  begin
    Painter.DrawScaledExpandButton(Canvas, ARowHeader.ButtonRect, ARowHeader.Row.Expanded,
      ScaleFactor, ARowHeader.ButtonColor, GetExpandButtonState(ARowHeader));
  end;
end;

procedure TcxvgPainter.DrawCategoryExplorerStyleRowHeader(ARowHeader: TcxCustomRowHeaderInfo; ATheme: TdxTheme);
const
  Parts: array[Boolean, 0..1] of Integer = (
    (EBP_NORMALGROUPEXPAND, EBNGE_NORMAL),
    (EBP_NORMALGROUPCOLLAPSE, EBNGC_NORMAL));
var
  R: TRect;
begin
  with ARowHeader do
  begin
    DrawCategoryRowIndent(ARowHeader);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := ViewParams.Color;
    R := HeaderRect;
    if (Row.Level > 0) and (PaintStyle = psDotNet) then
      R.Left := IndentBounds.Right - (ViewInfo.RowIndentWidth + ViewInfo.VertLineWidth);
//    Canvas.FillRect(R);
    DrawThemeBackground(ATheme, Canvas.Handle, EBP_NORMALGROUPHEAD, 0, @R);
    if not cxRectIsEmpty(ButtonRect) and (ButtonRect.Left >= R.Left) then
    begin
      R := ButtonRect;
      TcxWinXPLookAndFeelPainter.DrawScaledThemeBackground(ATheme,
        Canvas.Handle, Parts[Row.Expanded, 0], Parts[Row.Expanded, 1], R, ScaleFactor, True);
    end;
    if CaptionsInfo.Count > 0 then
    begin
      DrawRowHeaderCell(CaptionsInfo[0], True);
      DrawImage(CaptionsInfo[0]);
    end;
    if not cxRectIsEmpty(FocusRect) and not VerticalGrid.IsDesigning then
      Canvas.DrawFocusRect(FocusRect);
    if Selected then
    begin
      Canvas.DrawFocusRect(HeaderRect);
      Canvas.DrawFocusRect(cxRectInflate(HeaderRect, -1, -1));
    end;
  end;
end;

procedure TcxvgPainter.DrawCategoryRowHeader(ARowHeader: TcxCustomRowHeaderInfo);
var
  ATheme: TdxTheme;
begin
  if IsExplorerStyleCategory(ATheme) then
    DrawCategoryExplorerStyleRowHeader(ARowHeader, ATheme)
  else
    DrawRowHeader(ARowHeader);
end;

procedure TcxvgPainter.DrawCategoryRowIndent(ARowHeader: TcxCustomRowHeaderInfo);
var
  I: Integer;
begin
  with ARowHeader do
  begin
    if Transparent then Exit;
    for I := 0 to CategoryIndents.Count - 2 do
      with CategoryIndents[I]^ do
        Canvas.FillRect(Bounds, ViewParams);
    for I := 0 to RowIndents.Count - 1 do
      with RowIndents[I]^ do
        Canvas.FillRect(Bounds, ViewParams);
  end;
end;

procedure TcxvgPainter.DrawHeaderDragImage(ARowHeader: TcxCustomRowHeaderInfo);
var
  I: Integer;
  R: TRect;
begin
  with ARowHeader do
  begin
    for I := 0 to CaptionsInfo.Count - 1 do
      DrawRowHeaderCell(CaptionsInfo[I], Transparent);
    if ARowHeader is TcxMultiEditorRowHeaderInfo then
    begin
      DrawLines(LinesInfo, cxNullRect);
      DrawHeaderSeparators(ARowHeader);
    end;
    R := HeaderRect;
    R.Left := IndentBounds.Right;
    Canvas.FrameRect(R, ViewInfo.BandBorderColor);
  end;
end;

procedure TcxvgPainter.DrawHeaderSeparators(ARowHeader: TcxCustomRowHeaderInfo);
var
  I: Integer;
  AOffset, R: TRect;
begin
  if not (ARowHeader is TcxMultiEditorRowHeaderInfo) then Exit;
  with TcxMultiEditorRowHeaderInfo(ARowHeader) do
  begin
    if SeparatorRects.Count = 0 then
      Exit;
    Canvas.SetParams(SeparatorInfo.ViewParams);
    AOffset := GetTextEditDrawTextOffset(taLeftJustify, True);
    for I := 0 to SeparatorRects.Count - 1 do
    begin
      R := SeparatorRects[I];
      if cxRectIsEmpty(R) then Continue;
      if not Transparent then
        Canvas.FillRect(R, SeparatorInfo.ViewParams.Bitmap);
      Inc(R.Top, AOffset.Top + 1);
      Inc(R.Left, AOffset.Left + 1);
      Dec(R.Bottom, AOffset.Bottom + 1);
      Dec(R.Right, AOffset.Right + 1);
      cxTextOut(Canvas.Canvas, SeparatorInfo.Caption, R, SeparatorInfo.TextFlags);
    end;
  end;
end;

procedure TcxvgPainter.DrawImage(ACaptionInfo: TcxRowCaptionInfo);
var
  ARgn: TcxRegion;
  R: TRect;
begin
  if ACaptionInfo.ImageIndex = -1 then
    Exit;
  ARgn := Canvas.GetClipRegion;
  try
    cxRectIntersect(R, ACaptionInfo.ImageRect, ACaptionInfo.CaptionRect);
    Canvas.IntersectClipRect(R);
    cxDrawImage(Canvas, ACaptionInfo.ImageRect, nil, ACaptionInfo.Images, ACaptionInfo.ImageIndex, True, nil, ScaleFactor);
  finally
    Canvas.SetClipRegion(ARgn, roSet);
  end;
end;

procedure TcxvgPainter.DrawLines(ALinesInfo: TLineInfoList; R: TRect);
var
  I: Integer;
begin
  for I := 0 to ALinesInfo.Count - 1 do
    with ALinesInfo[I]^ do
    begin
      if IsBrush then
        Canvas.Brush.Assign(Brush)
      else
        Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect);
    end;
end;

procedure TcxvgPainter.DrawRow(ARowViewInfo: TcxCustomRowViewInfo);
var
  AHeaderInfo: TcxCustomRowHeaderInfo;
  AIsCategoryRow: Boolean;
begin
  AHeaderInfo := ARowViewInfo.HeaderInfo;
  AHeaderInfo.Transparent := False;
  AIsCategoryRow := ARowViewInfo is TcxCategoryRowViewInfo;
  if not VerticalGrid.DoDrawRowHeader(AHeaderInfo) then
  begin
    if AIsCategoryRow then
      DrawCategoryRowHeader(AHeaderInfo)
    else
      DrawRowHeader(AHeaderInfo);
  end;
  if not AIsCategoryRow then
  begin
    Canvas.SaveClipRegion;
    try
      Canvas.ExcludeClipRect(AHeaderInfo.HeaderRect);
      DrawRowValues(ARowViewInfo);
    finally
      Canvas.RestoreClipRegion;
    end;
  end;
  DrawLines(AHeaderInfo.LinesInfo, cxNullRect);
  DrawLines(ARowViewInfo.ValuesLinesInfo, cxNullRect);
end;

procedure TcxvgPainter.DrawRowHeader(ARowHeader: TcxCustomRowHeaderInfo);
begin
  DrawHeaderSeparators(ARowHeader);
  DrawRowIndent(ARowHeader);
  DrawRowHeaderCells(ARowHeader);
  DrawRowHeaderFocusRect(ARowHeader);
  DrawRowHeaderSelected(ARowHeader);
end;

procedure TcxvgPainter.DrawRowHeaderCell(ACaptionInfo: TcxRowCaptionInfo; ATransparent: Boolean);
var
  R: TRect;
begin
  Canvas.SetParams(ACaptionInfo.ViewParams);
  if not ATransparent then
    Canvas.FillRect(ACaptionInfo.CaptionRect, ACaptionInfo.ViewParams);
  Canvas.Brush.Style := bsClear;
  R := ACaptionInfo.CaptionTextRect;
  cxTextOut(Canvas.Canvas, ACaptionInfo.Caption, R, ACaptionInfo.TextFlags);
  Canvas.Brush.Style := bsSolid;
  DrawImage(ACaptionInfo);
  DrawRowHeaderCellFilterButton(ACaptionInfo);
end;

procedure TcxvgPainter.DrawRowHeaderCellFilterButton(ACaptionInfo: TcxRowCaptionInfo);
begin
  if cxRectIsEmpty(ACaptionInfo.FilterButtonRect) then
    Exit;
  if ACaptionInfo.IsFilterSmartTag then
    Painter.DrawScaledFilterSmartTag(Canvas, ACaptionInfo.FilterButtonRect,
      ACaptionInfo.FilterSmartTagState, ACaptionInfo.IsFilterButtonActive, ScaleFactor)
  else
    Painter.DrawScaledFilterDropDownButton(Canvas, ACaptionInfo.FilterButtonRect,
      ACaptionInfo.FilterButtonState, ACaptionInfo.IsFilterButtonActive, ScaleFactor);
end;

procedure TcxvgPainter.DrawRowHeaderCells(ARowHeader: TcxCustomRowHeaderInfo);
var
  I: Integer;
begin
  for I := 0 to ARowHeader.CaptionsInfo.Count - 1 do
    DrawRowHeaderCell(ARowHeader.CaptionsInfo[I], ARowHeader.Transparent);
end;

procedure TcxvgPainter.DrawRowHeaderFocusRect(ARowHeader: TcxCustomRowHeaderInfo);
begin
  if not cxRectIsEmpty(ARowHeader.FocusRect) then
    Canvas.DrawFocusRect(ARowHeader.FocusRect);
end;

procedure TcxvgPainter.DrawRowHeaderSelected(ARowHeader: TcxCustomRowHeaderInfo);
begin
  if ARowHeader.Selected then
  begin
    Canvas.DrawFocusRect(ARowHeader.HeaderRect);
    Canvas.DrawFocusRect(cxRectInflate(ARowHeader.HeaderRect, -1, -1));
  end;
end;

procedure TcxvgPainter.DrawRowIndent(ARowHeader: TcxCustomRowHeaderInfo);
var
  I: Integer;
begin
  if ARowHeader.Transparent then Exit;
  with ARowHeader do
  begin
    for I := 0 to CategoryIndents.Count - 1 do
      with CategoryIndents[I]^ do
        Canvas.FillRect(Bounds, ViewParams);
    for I := 0 to RowIndents.Count - 1 do
      with RowIndents[I]^ do
        Canvas.FillRect(Bounds, ViewParams);
  end;
  DrawButton(ARowHeader);
end;

procedure TcxvgPainter.DrawRows;
var
  I: Integer;
  ARowInfo: TcxCustomRowViewInfo;
begin
  for I := 0 to ViewInfo.RowsViewInfo.Count - 1 do
  begin
    ARowInfo := ViewInfo.RowsViewInfo[I];
    if Canvas.RectVisible(ARowInfo.RowRect) then
      DrawRow(ARowInfo);
  end;
end;

procedure TcxvgPainter.DrawStyleFeatures;
begin
end;

function TcxvgPainter.GetExpandButtonState(ARowHeader: TcxCustomRowHeaderInfo): TcxExpandButtonState;
begin
  Result := cebsNormal;
  if ARowHeader.Focused and not (ARowHeader.Row.ViewInfo is TcxCategoryRowViewInfo) then
  begin
    if VerticalGrid.Controller.Focused or VerticalGrid.IsFocused then
      Result := cebsSelected
    else
      Result := cebsInactive;
  end;
end;

function TcxvgPainter.IsExplorerStyleCategory(out ATheme: TdxTheme): Boolean;
begin
  Result := ViewInfo.UseCategoryExplorerStyle and (VerticalGrid.LookAndFeel.SkinPainter = nil);
  if Result then
  begin
    ATheme := OpenTheme(totExplorerBar);
    Result := ATheme <> 0;
  end;
end;

function TcxvgPainter.IsNeedPaintValue(AValueInfo: TcxRowValueInfo): Boolean;
begin
  Result := not cxRectIsEmpty(AValueInfo.DisplayRect) and Canvas.RectVisible(AValueInfo.DisplayRect);
end;

procedure TcxvgPainter.DrawRowValueCell(AValueInfo: TcxRowValueInfo);
begin
  with AValueInfo do
  begin
    if IsTransparent then
      Canvas.FillRect(DisplayRect, ViewParams.Bitmap);
    PaintEx(Canvas);
    if Focused and Self.ViewInfo.CalcHelper.IsDrawValueFocusRect and not cxRectIsEmpty(FocusRect) then
      Canvas.DrawFocusRect(FocusRect);
  end;
end;

procedure TcxvgPainter.DrawRowValues(ARowViewInfo: TcxCustomRowViewInfo);
var
  I: Integer;
  AValueInfo: TcxRowValueInfo;
begin
  DrawValuesSeparators(ARowViewInfo);
  with ARowViewInfo do
  begin
    for I := 0 to ValuesInfo.Count - 1 do
    begin
      AValueInfo := ValuesInfo[I];
      if not IsNeedPaintValue(AValueInfo) then
        Continue;
      BeforeCustomDraw(AValueInfo);
      if not VerticalGrid.DoDrawValue(AValueInfo) then
      begin
        AfterCustomDraw(AValueInfo);
        AValueInfo.EditViewInfo.TextColor := Canvas.Font.Color;
        AValueInfo.EditViewInfo.BackgroundColor := Canvas.Brush.Color;
        DrawRowValueCell(AValueInfo);
      end;
    end;
  end;
end;

procedure TcxvgPainter.DrawValuesSeparators(ARowViewInfo: TcxCustomRowViewInfo);
var
  I: Integer;
  AViewParams: TcxViewParams;
  ASeparatorInfo: TSeparatorInfo;
  R, AOffset: TRect;
begin
  if not (ARowViewInfo is TcxMultiEditorRowViewInfo) then Exit;
  with TcxMultiEditorRowViewInfo(ARowViewInfo) do
  begin
    if (FValuesInfo.Count = 0) or (SeparatorRects.Count = 0) then Exit;
    AOffset := GetTextEditDrawTextOffset(taLeftJustify, True);
    for I := 0 to SeparatorRects.Count - 1 do
    begin
      R := SeparatorRects[I];
      if cxRectIsEmpty(R) then Continue;
      AViewParams := ValuesInfo[I].ViewParams;
      Canvas.SetParams(AViewParams);
      Canvas.FillRect(R, AViewParams.Bitmap);
      Inc(R.Top, AOffset.Top + 1);
      Inc(R.Left, AOffset.Left + 1);
      Dec(R.Bottom, AOffset.Bottom + 1);
      Dec(R.Right, AOffset.Right + 1);
      ASeparatorInfo := TcxMultiEditorRowHeaderInfo(HeaderInfo).SeparatorInfo;
      cxTextOut(Canvas.Canvas, ASeparatorInfo.Caption, R, ASeparatorInfo.TextFlags);
    end;
  end;
end;

function TcxvgPainter.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(Control);
end;

function TcxvgPainter.GetViewInfo: TcxvgCustomViewInfo;
begin
  Result := TcxvgCustomViewInfo(inherited ViewInfo);
end;

{ TcxStyle3DPainter }

procedure TcxStyle3DPainter.DrawStyleFeatures;
begin
  with ViewInfo do
    DrawLines(FocusLinesInfo, ClientRect);
end;

{ TcxVerticalGridRows }

constructor TcxVerticalGridRows.Create(AOwner: TcxCustomVerticalGrid);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
  FVisibleRows := TList.Create;
  FCount := -1;
end;

destructor TcxVerticalGridRows.Destroy;
begin
  FreeAndNil(FVisibleRows);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxVerticalGridRows.AssignRows(Source: TcxVerticalGridRows);

  procedure AddRowWithChildren(AParent: TcxCustomRow; ASourceRow: TcxCustomRow);
  var
    I: Integer;
    AddedRow: TcxCustomRow;
  begin
    AddedRow := Owner.AddChild(AParent, TcxCustomRowClass(ASourceRow.ClassType));
    AddedRow.FID := ASourceRow.ID;
    AddedRow.Assign(ASourceRow);
    for I := 0 to ASourceRow.Count - 1 do
      AddRowWithChildren(AddedRow, ASourceRow.Rows[I]);
  end;

var
  I: Integer;
begin
  if not (Source is TcxVerticalGridRows) then
    cxVerticalGridError(cxGetResourceString(@cxSvgAssignRowsError));
  BeginUpdate;
  try
    Clear;
    for I := 0 to Source.Root.Count - 1 do
      AddRowWithChildren(Root, Source.Root.Rows[I]);
  finally
    EndUpdate;
  end;
end;

function TcxVerticalGridRows.IndexOf(ARow: TcxCustomRow): Integer;
begin
  CheckList;
  Result := FList.IndexOf(ARow);
end;

function TcxVerticalGridRows.IsRowVisible(ARow: TcxCustomRow): Boolean;
begin
  Result := False;
  CheckList;
  if ARow.Visible then
    repeat
      ARow := ARow.FParent;
      if (ARow = Owner.RootRow) then
      begin
        Result := True;
        Exit;
      end
      else
        if not (ARow.Expanded and ARow.Visible) then Exit;
    until False;
end;

procedure TcxVerticalGridRows.Add(ARow: TcxCustomRow);
begin
  if Assigned(ARow) then
  begin
    FOwner.RootRow.Add(ARow);
    UnprepareList;
  end;
end;

procedure TcxVerticalGridRows.AddChild(AParent,
  ARow: TcxCustomRow);
begin
  if Assigned(AParent) and Assigned(ARow) then
  begin
    AParent.Add(ARow);
    UnprepareList;
  end;
end;

procedure TcxVerticalGridRows.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxVerticalGridRows.Changed(ARebuild: Boolean = False);
begin
  if ARebuild then
    PrepareList;
  if FLockCount = 0 then
    Owner.RowsChanged;
end;

procedure TcxVerticalGridRows.CheckList;
begin
  if FCount = -1 then
    PrepareList;
end;

procedure TcxVerticalGridRows.Clear;
begin
  BeginUpdate;
  try
    FVisibleRows.Clear;
    FOwner.FocusedRow := nil;
    FOwner.RootRow.RemoveAll;
    UnprepareList;
  finally
    EndUpdate;
  end;
end;

procedure TcxVerticalGridRows.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed(True);
end;

function TcxVerticalGridRows.FindLoadedParent(AID: Integer): TcxCustomRow;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  if AID = -1 then
    Result := FOwner.FRootRow
  else
  begin
    Result := nil;
    for I := 0 to Count - 1 do
    begin
      ARow := Items[I];
      if ARow.FLoadedID = AID then
      begin
        Result := ARow;
        break;
      end;
    end;
  end;
end;

function TcxVerticalGridRows.FindRowByID(AID: Integer): TcxCustomRow;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  if AID = -1 then
    Result := FOwner.FRootRow
  else
  begin
    Result := nil;
    for I := 0 to Count - 1 do
    begin
      ARow := Items[I];
      if ARow.ID = AID then
      begin
        Result := ARow;
        break;
      end;
    end;
  end;
end;

function TcxVerticalGridRows.FindRowByStoredName(const AName: string): TcxCustomRow;
var
  I: Integer;
begin
  Result := FOwner.FRootRow;
  for I := 0 to Count - 1 do
    if CompareStr(Items[I].GetObjectName, AName) = 0 then
    begin
      Result := Items[I];
      break;
    end;
end;

function TcxVerticalGridRows.GetNextID: Integer;
begin
  Result := FNextID;
  Inc(FNextID);
end;

procedure TcxVerticalGridRows.PrepareList;
var
  ARow: TcxCustomRow;

  function NextRow: TcxCustomRow;
  var
    AIndex: Integer;
    ATemp: TcxCustomRow;
  begin
    Result := nil;
    AIndex := 0;
    ATemp := ARow;
    repeat
      if ATemp.Count > AIndex then
        Result := ATemp.Rows[AIndex]
      else
      begin
        AIndex := Succ(ATemp.Index);
        ATemp := ATemp.FParent;
      end;
    until (Result <> nil) or (ATemp = nil);
  end;

begin
  FList.Clear;
  ARow := Owner.FirstRow;
  while ARow <> nil do
  begin
    FList.Add(ARow);
    ARow := NextRow;
  end;
  FCount := FList.Count;
  UpdateVisibleRows;
end;

procedure TcxVerticalGridRows.Remove(ARow: TcxCustomRow);
begin
  FList.Remove(ARow);
  UnprepareList;
end;

procedure TcxVerticalGridRows.RestoreDefaults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RestoreDefaults;
end;

procedure TcxVerticalGridRows.UnprepareList;
begin
  FCount := -1;
end;

procedure TcxVerticalGridRows.UpdateVisibleRows;
var
  ARow: TcxCustomRow;
  I: Integer;
begin
  FMaxVisibleLevel := 0;
  FVisibleRows.Clear;
  for I := 0 to Count - 1 do
  begin
    ARow := TcxCustomRow(FList[I]);
    if IsRowVisible(ARow) then
    begin
      FMaxVisibleLevel := Max(FMaxVisibleLevel, ARow.Level);
      ARow.FVisibleIndex := FVisibleRows.Count;
      FVisibleRows.Add(ARow);
    end
    else
      ARow.FVisibleIndex := -1;
  end;
end;

function TcxVerticalGridRows.GetCount: Integer;
begin
  CheckList;
  Result := FList.Count;
end;

function TcxVerticalGridRows.GetRoot: TcxCustomRow;
begin
  Result := FOwner.FRootRow;
end;

function TcxVerticalGridRows.GetRow(Index: Integer): TcxCustomRow;
begin
  if (Index < 0) or (Index >= Count) then // call Count -> refresh if need
    cxVerticalGridError(cxSvgIndexError);
  Result := TcxCustomRow(FList.List[Index]);
end;

function TcxVerticalGridRows.GetVisibleRowCount: Integer;
begin
  Result := FVisibleRows.Count;
end;

function TcxVerticalGridRows.GetVisibleRow(Index: Integer): TcxCustomRow;
begin
  Result := TcxCustomRow(FVisibleRows[Index]);
end;

procedure TcxVerticalGridRows.SetRow(Index: Integer;
  const Value: TcxCustomRow);
begin
  if (Index < 0) or (Index >= Count) then // call Count -> refresh if need
    cxVerticalGridError(cxSvgIndexError);
  FList[Index] := Value;
end;

{ TcxMultiEditorRowProperties }

constructor TcxMultiEditorRowProperties.CreateEx(ARow: TcxCustomRow);
begin
  inherited CreateEx(ARow);
  FEditors := GetCollectionClass.Create(ARow as TcxCustomMultiEditorRow);
  FSeparatorKind := skVertLine;
end;

destructor TcxMultiEditorRowProperties.Destroy;
begin
  FreeAndNil(FEditors);
  inherited Destroy;
end;

procedure TcxMultiEditorRowProperties.Assign(Source: TPersistent);
begin
  if Source is TcxMultiEditorRowProperties then
    with TcxMultiEditorRowProperties(Source) do
    begin
      Self.SeparatorAlignmentVert := SeparatorAlignmentVert;
      Self.SeparatorKind := SeparatorKind;
      Self.SeparatorString := SeparatorString;
      Self.Fixed := Fixed;
      Self.Editors.Assign(Editors);
    end
  else
    inherited Assign(Source);
end;

procedure TcxMultiEditorRowProperties.ChangeScale(M, D: Integer);
begin
  inherited;
  Editors.ChangeScale(M, D);
end;

function TcxMultiEditorRowProperties.GetCollectionClass: TcxEditorPropertiesCollectionClass;
begin
  Result := TcxEditorPropertiesCollection;
end;

function TcxMultiEditorRowProperties.GetOwner: TPersistent;
begin
  Result := Row;
end;

procedure TcxMultiEditorRowProperties.SetFixed(Value: Boolean);
begin
  if FFixed <> Value then
  begin
    FFixed := Value;
    Changed;
  end;
end;

procedure TcxMultiEditorRowProperties.SetSeparatorAlignmentVert(Value: TcxAlignmentVert);
begin
  if FSeparatorAlignmentVert <> Value then
  begin
    FSeparatorAlignmentVert := Value;
    Changed;
  end;
end;

procedure TcxMultiEditorRowProperties.SetSeparatorKind(Value: TSeparatorKind);
begin
  if FSeparatorKind <> Value then
  begin
    FSeparatorKind := Value;
    Changed;
  end;
end;

procedure TcxMultiEditorRowProperties.SetSeparatorString(const Value: string);
begin
  if FSeparatorString <> Value then
  begin
    FSeparatorString := Value;
    Changed;
  end;
end;

{ TcxCustomMultiEditorRow }

function TcxCustomMultiEditorRow.CreateHeaderInfo: TcxCustomRowHeaderInfo;
begin
  Result := TcxMultiEditorRowHeaderInfo.Create(Self);
end;

function TcxCustomMultiEditorRow.CreateViewInfo: TcxCustomRowViewInfo;
var
  I: Integer;
begin
  Result := TcxMultiEditorRowViewInfo.Create(Self);
  with TcxMultiEditorRowProperties(FProperties) do
    for I := 0 to Editors.Count - 1 do
      Editors[I].EditContainer.FCellIndex := I;
end;

function TcxCustomMultiEditorRow.CanFocus: Boolean;
var
  I: Integer;
begin
  Result := inherited CanFocus;
  if Result then
  begin
    Result := False;
    for I := 0 to Properties.Editors.Count - 1 do
    begin
      Result := Result or Properties.Editors[I].Options.Focusing;
      if Result then break;
    end;
  end;
end;

function TcxCustomMultiEditorRow.CanTabStop: Boolean;
var
  I: Integer;
begin
  Result := inherited CanTabStop;
  if Result then
  begin
    Result := False;
    for I := 0 to Properties.Editors.Count - 1 do
    begin
      Result := Result or Properties.Editors[I].Options.TabStop;
      if Result then break;
    end;
  end;
end;

procedure TcxCustomMultiEditorRow.EditorsChanged;
begin
  if Properties.Locked or VerticalGrid.IsLoading then Exit;
  ViewInfo.ClearValuesInfo;
  Changed;
end;

function TcxCustomMultiEditorRow.GetDefaultHeight: Integer;
var
  I: Integer;
  AFont: TFont;
begin
  Result := inherited GetDefaultHeight;
  for I := 0 to Properties.Editors.Count - 1 do
  begin
    AFont := VerticalGrid.Styles.GetContentParams(Properties.Editors[I], False, -1).Font;
    Result := Max(Result, Properties.Editors[I].EditContainer.GetEditDefaultHeight(AFont) + cxTextOffset);
  end;
end;

function TcxCustomMultiEditorRow.GetEditContainer(
  ACellIndex: Integer): TcxCellEdit;
begin
  if (ACellIndex >= 0) and (ACellIndex < GetEditContainerCount) then
    Result := Properties.Editors[ACellIndex].EditContainer
  else
    Result := nil;
end;

function TcxCustomMultiEditorRow.GetEditContainerCount: Integer;
begin
  Result := Properties.Editors.Count;
end;

function TcxCustomMultiEditorRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxMultiEditorRowProperties;
end;

function TcxCustomMultiEditorRow.GetStylesClass: TcxvgCustomRowStylesClass;
begin
  Result := TcxEditorRowStyles;
end;

function TcxCustomMultiEditorRow.GetProperties: TcxMultiEditorRowProperties;
begin
  Result := TcxMultiEditorRowProperties(FProperties);
end;

function TcxCustomMultiEditorRow.GetStyles: TcxEditorRowStyles;
begin
  Result := TcxEditorRowStyles(FStyles);
end;

procedure TcxCustomMultiEditorRow.SetProperties(
  const Value: TcxMultiEditorRowProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomMultiEditorRow.SetStyles(Value: TcxEditorRowStyles);
begin
  FStyles.Assign(Value);
  Changed;
end;

{ TcxCollectionItemEditorRowProperties }

constructor TcxCollectionItemEditorRowProperties.CreateEx(
  ARow: TcxCustomRow);
begin
  ARow.FProperties.FLocked := True;
  inherited CreateEx(ARow);
  FWidth := 50;
  EditContainer.EditingControl := ARow.VerticalGrid;
  ARow.FProperties.FLocked := False;
end;

procedure TcxCollectionItemEditorRowProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxCollectionItemEditorRowProperties then
    Width := TcxCollectionItemEditorRowProperties(Source).Width;
end;

procedure TcxCollectionItemEditorRowProperties.ChangeScale(M, D: Integer);
begin
  inherited;
  Width := MulDiv(Width, M, D);
end;

function TcxCollectionItemEditorRowProperties.GetDisplayName: string;
begin
  Result := Caption;
end;

function TcxCollectionItemEditorRowProperties.GetOwner: TPersistent;
begin
  Result := Collection;
end;

function TcxCollectionItemEditorRowProperties.GetOptions: TcxMultiEditorRowPropertiesOptions;
begin
  Result := TcxMultiEditorRowPropertiesOptions(EditContainer.Options);
end;

procedure TcxCollectionItemEditorRowProperties.SetOptions(Value: TcxMultiEditorRowPropertiesOptions);
begin
  EditContainer.Options.Assign(Value);
end;

procedure TcxCollectionItemEditorRowProperties.SetWidth(Value: Integer);
begin
  Value := Max(Value, cxvgMinValueWidth);
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TcxEditorRowItemProperties }

procedure TcxEditorRowItemProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxEditorRowItemProperties then
  begin
    Value := TcxEditorRowItemProperties(Source).Value;
    Options := TcxEditorRowItemProperties(Source).Options;
    DataBinding := TcxEditorRowItemProperties(Source).DataBinding;
  end;
end;

{ TcxEditorPropertiesCollection }

constructor TcxEditorPropertiesCollection.Create(ARow: TcxCustomMultiEditorRow);
begin
  inherited Create(GetCollectionItemClass);
  FRow := ARow;
end;

procedure TcxEditorPropertiesCollection.Assign(Source: TPersistent);
var
  AIndexes: TDictionary<string, Integer>;
  AIndex: Integer;
  I: Integer;
begin
  AIndexes := TDictionary<string, Integer>.Create;
  try
    Row.VerticalGrid.ConditionalFormattingProvider.BeginUpdateFieldsOrder;
    try
      for I := 0 to Count - 1 do
        AIndexes.AddOrSetValue(Items[I].EditContainer.EditRowProperties.Caption, Items[I].EditContainer.ItemIndex);
      inherited Assign(Source);
      for I := 0 to Count - 1 do
      begin
        if AIndexes.TryGetValue(Items[I].EditContainer.EditRowProperties.Caption, AIndex) then
          Row.VerticalGrid.ContainerList.Move(Items[I].EditContainer.ItemIndex, AIndex);
      end;
      Row.VerticalGrid.DataController.UpdateItemIndexes;
    finally
      Row.VerticalGrid.ConditionalFormattingProvider.EndUpdateFieldsOrder;
    end;
  finally
    AIndexes.Free;
  end;
end;

function TcxEditorPropertiesCollection.Add: TcxEditorRowItemProperties;
begin
  Result := TcxEditorRowItemProperties(inherited Add);
end;

function TcxEditorPropertiesCollection.GetNamePath: string;
var
  S, P: string;
begin
  S := Row.Name;
  if S = '' then
    S := Row.GetNamePath;
  P := PropName;
  if P = '' then
    Exit;
  Result := S + '.' + P;
end;

procedure TcxEditorPropertiesCollection.ChangeScale(M, D: Integer);
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

function TcxEditorPropertiesCollection.GetCollectionItemClass: TCollectionItemClass;
begin
  Result := TcxEditorRowItemProperties;
end;

function TcxEditorPropertiesCollection.GetOwner: TPersistent;
begin
  if FRow = nil then
    Result := nil
  else
    Result := FRow.FProperties;
end;

procedure TcxEditorPropertiesCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnDeleting) and not (csDestroying in Row.ComponentState) then
  begin
    if (Item as TcxCollectionItemEditorRowProperties).EditContainer.Focused then
      Row.VerticalGrid.Controller.FocusedItem := nil;
  end;
  inherited Notify(Item, Action);
end;

procedure TcxEditorPropertiesCollection.Update(Item: TCollectionItem);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).EditContainer.FCellIndex := I;
  Row.EditorsChanged;
end;

function TcxEditorPropertiesCollection.GetItem(
  Index: Integer): TcxEditorRowItemProperties;
begin
  Result := TcxEditorRowItemProperties(inherited Items[Index]);
end;

{ TcxCustomEditorRowProperties }

constructor TcxCustomEditorRowProperties.CreateEx(ARow: TcxCustomRow);
begin
  //for the correct work with a collection
  FEditContainer := GetInplaceEditContainerClass.Create(ARow);
  FEditContainer.FEditRowProperties := Self;
  inherited CreateEx(ARow);
end;

destructor TcxCustomEditorRowProperties.Destroy;
begin
  FreeAndNil(FEditContainer);
  inherited Destroy;
end;

procedure TcxCustomEditorRowProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomEditorRowProperties then
    FEditContainer.Assign(TcxCustomEditorRowProperties(Source).FEditContainer);
  inherited Assign(Source);
end;

function TcxCustomEditorRowProperties.GetEditContainer: TcxCellEdit;
begin
  Result := FEditContainer;
end;

function TcxCustomEditorRowProperties.GetFilterableByPopupMenu: Boolean;
begin
  Result := EditContainer.FilterableByPopupMenu;
end;

function TcxCustomEditorRowProperties.GetFiltered: Boolean;
begin
  Result := EditContainer.Filtered;
end;

function TcxCustomEditorRowProperties.GetInplaceEditContainerClass: TcxCellEditClass;
begin
  Result := TcxCellEdit;
end;

procedure TcxCustomEditorRowProperties.DoGetDisplayTextEvent(
  ARecordIndex: Integer; var Text: string);
begin
  if Assigned(FOnGetDisplayText) then
    FOnGetDisplayText(Self, ARecordIndex, Text);
end;

procedure TcxCustomEditorRowProperties.DoValidateDrawValue(const Value: Variant;
  ARecordIndex: Integer; AData: TcxEditValidateInfo);
begin
  if Assigned(FOnValidateDrawValue) then
    FOnValidateDrawValue(Self, ARecordIndex, Value, AData);
end;

procedure TcxCustomEditorRowProperties.SetFiltered(AValue: Boolean);
begin
  EditContainer.Filtered := AValue;
end;

function TcxCustomEditorRowProperties.GetDataBinding: TcxItemDataBinding;
begin
  Result := TcxItemDataBinding(EditContainer.FDataBinding);
end;

function TcxCustomEditorRowProperties.GetDisplayEditProperty(
  Index: Integer): TcxCustomEditProperties;
begin
  Result := FEditContainer.DoGetEditProperties(Pointer(Index));
end;

function TcxCustomEditorRowProperties.GetDisplayText(Index: Integer): string;
var
  ADisplayEditProperties: TcxCustomEditProperties;
  AEditContainerDisplayValue: Variant;
  AFullText: Boolean;
  ADisplayText: WideString;
begin
  AFullText := Row.VerticalGrid.OptionsView.CellAutoHeight;
  ADisplayEditProperties := DisplayEditProperties[Index];
  AEditContainerDisplayValue := EditContainer.GetDisplayValue(ADisplayEditProperties, Index);
  ADisplayText := DisplayEditProperties[Index].GetDisplayText(AEditContainerDisplayValue, AFullText);
  Result := VarToStr(ADisplayText);
  DoGetDisplayTextEvent(Index, Result);
end;

function TcxCustomEditorRowProperties.GetEditViewData: TcxCustomEditViewData;
begin
  Result := EditContainer.EditViewData;
end;

function TcxCustomEditorRowProperties.GetEditProperties: TcxCustomEditProperties;
begin
  Result := FEditContainer.Properties;
end;

function TcxCustomEditorRowProperties.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := FEditContainer.PropertiesClass;
end;

function TcxCustomEditorRowProperties.GetEditPropertiesClassName: string;
begin
  Result := FEditContainer.PropertiesClassName;
end;

function TcxCustomEditorRowProperties.GetFilteringDateRanges: TdxFilteringDateRanges;
begin
  Result := EditContainer.FilteringDateRanges;
end;

function TcxCustomEditorRowProperties.GetIEditorPropertiesContainer: IcxEditorPropertiesContainer;
begin
  Supports(TObject(FEditContainer), IcxEditorPropertiesContainer, Result);
end;

function TcxCustomEditorRowProperties.GetItemIndex: Integer;
begin
  Result := FEditContainer.ItemIndex;
end;

function TcxCustomEditorRowProperties.GetItemLink: TObject;
begin
  Result := FEditContainer;
end;

function TcxCustomEditorRowProperties.GetOptions: TcxEditorRowPropertiesOptions;
begin
  Result := TcxEditorRowPropertiesOptions(FEditContainer.Options);
end;

function TcxCustomEditorRowProperties.GetRepositoryItem: TcxEditRepositoryItem;
begin
  Result := FEditContainer.RepositoryItem;
end;

function TcxCustomEditorRowProperties.GetPropertiesValue: TcxCustomEditProperties;
begin
  Result := FEditContainer.PropertiesValue;
end;

function TcxCustomEditorRowProperties.GetValue: Variant;
begin
  Result := FEditContainer.Value;
end;

function TcxCustomEditorRowProperties.GetValueByIndex(Index: Integer): Variant;
begin
  Result := FEditContainer.Values[Index];
end;

procedure TcxCustomEditorRowProperties.SetDataBinding(
  Value: TcxItemDataBinding);
begin
  FEditContainer.FDataBinding.Assign(Value);
end;

procedure TcxCustomEditorRowProperties.SetEditProperties(
  Value: TcxCustomEditProperties);
begin
  FEditContainer.Properties := Value;
end;

procedure TcxCustomEditorRowProperties.SetEditPropertiesClass(
  Value: TcxCustomEditPropertiesClass);
begin
  FEditContainer.PropertiesClass := Value;
end;

procedure TcxCustomEditorRowProperties.SetEditPropertiesClassName(
  const Value: string);
begin
  FEditContainer.PropertiesClassName := Value;
end;

procedure TcxCustomEditorRowProperties.SetOptions(Value: TcxEditorRowPropertiesOptions);
begin
  FEditContainer.Options.Assign(Value);
end;

procedure TcxCustomEditorRowProperties.SetRepositoryItem(Value: TcxEditRepositoryItem);
begin
  FEditContainer.RepositoryItem := Value;
end;

procedure TcxCustomEditorRowProperties.SetValue(const Value: Variant);
begin
  with FEditContainer.DataController do
  begin
    if (RowCount = 0) or (FocusedRowIndex < 0) then Exit;
    if dceEdit in EditState then
      SetEditValue(FEditContainer.ItemIndex, Value, evsValue)
    else
      Values[FilteredRecordIndex[FocusedRowIndex], FEditContainer.ItemIndex] := Value;
  end;
end;

function TcxCustomEditorRowProperties.GetOnGetEditingProperties: TcxVerticalGridGetEditPropertiesEvent;
begin
  Result := TcxVerticalGridGetEditPropertiesEvent(EditContainer.OnGetEditingProperties);
end;

function TcxCustomEditorRowProperties.GetOnGetEditProperties: TcxVerticalGridGetEditPropertiesEvent;
begin
  Result := TcxVerticalGridGetEditPropertiesEvent(EditContainer.OnGetEditProperties);
end;

function TcxCustomEditorRowProperties.GetOnGetFilterValues: TcxGetFilterValuesEvent;
begin
  Result := EditContainer.OnGetFilterValues;
end;

function TcxCustomEditorRowProperties.GetOnInitFilteringDateRanges: TdxInitDateRangesEvent;
begin
  Result := EditContainer.OnInitFilteringDateRanges;
end;

function TcxCustomEditorRowProperties.GetOnUserFiltering: TcxUserFilteringEvent;
begin
  Result := EditContainer.OnUserFiltering;
end;

function TcxCustomEditorRowProperties.GetOnUserFilteringEx: TcxUserFilteringExEvent;
begin
  Result := EditContainer.OnUserFilteringEx;
end;

procedure TcxCustomEditorRowProperties.SetOnGetEditProperties(Value: TcxVerticalGridGetEditPropertiesEvent);
begin
  EditContainer.OnGetEditProperties := TcxGetEditPropertiesEvent(Value);
end;

procedure TcxCustomEditorRowProperties.SetOnGetEditingProperties(Value: TcxVerticalGridGetEditPropertiesEvent);
begin
  EditContainer.OnGetEditingProperties := TcxGetEditPropertiesEvent(Value);
end;

procedure TcxCustomEditorRowProperties.SetOnGetFilterValues(Value: TcxGetFilterValuesEvent);
begin
  EditContainer.OnGetFilterValues := Value;
end;

procedure TcxCustomEditorRowProperties.SetOnInitFilteringDateRanges(Value: TdxInitDateRangesEvent);
begin
  EditContainer.OnInitFilteringDateRanges := Value;
end;

procedure TcxCustomEditorRowProperties.SetOnUserFiltering(Value: TcxUserFilteringEvent);
begin
  EditContainer.OnUserFiltering := Value;
end;

procedure TcxCustomEditorRowProperties.SetOnUserFilteringEx(Value: TcxUserFilteringExEvent);
begin
  EditContainer.OnUserFilteringEx := Value;
end;

{ TcxEditorRowProperties }

procedure TcxEditorRowProperties.Assign(Source: TPersistent);
begin
  if Source is TcxEditorRowProperties then
  begin
    inherited Assign(Source);
    if not (Row.VerticalGrid is TcxVirtualVerticalGrid) then
      Value := TcxEditorRowProperties(Source).Value;
  end
  else
    inherited Assign(Source);
end;

{ TcxCustomEditorRow }

function TcxCustomEditorRow.CreateHeaderInfo: TcxCustomRowHeaderInfo;
begin
  Result := TcxEditorRowHeaderInfo.Create(Self);
end;

function TcxCustomEditorRow.CreateViewInfo: TcxCustomRowViewInfo;
begin
  Result := TcxEditorRowViewInfo.Create(Self);
end;

function TcxCustomEditorRow.CanFocus: Boolean;
begin
  Result := Options.Focusing and Properties.Options.Focusing;
end;

function TcxCustomEditorRow.EditContainer: TcxCellEdit;
begin
  if FProperties <> nil then
    Result := TcxCustomEditorRowProperties(FProperties).EditContainer
  else
    Result := nil;
end;

function TcxCustomEditorRow.GetDefaultHeight: Integer;
var
  AFont: TFont;
begin
  AFont := VerticalGrid.Styles.GetContentParams(Properties, False, -1).Font;
  Result := Max(inherited GetDefaultHeight, EditContainer.GetEditDefaultHeight(AFont) + cxTextOffset);
end;

function TcxCustomEditorRow.GetEditContainer(ACellIndex: Integer): TcxCellEdit;
begin
  Result := EditContainer;
end;

function TcxCustomEditorRow.GetEditContainerCount: Integer;
begin
  Result := 1;
end;

function TcxCustomEditorRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxCustomEditorRowProperties;
end;

function TcxCustomEditorRow.GetStylesClass: TcxvgCustomRowStylesClass;
begin
  Result := TcxEditorRowStyles;
end;

procedure TcxCustomEditorRow.SetParentComponent(Value: TComponent);
begin
  inherited SetParentComponent(Value);
  if Value <> nil then
    if Value is TcxCustomVerticalGrid then
      EditContainer.EditingControl := TcxCustomVerticalGrid(Value)
    else
      EditContainer.EditingControl := (Value as TcxCustomRow).VerticalGrid;
end;

procedure TcxCustomEditorRow.SetVerticalGrid(Value: TcxCustomVerticalGrid);
begin
  inherited SetVerticalGrid(Value);
  EditContainer.EditingControl := Value
end;

function TcxCustomEditorRow.GetProperties: TcxCustomEditorRowProperties;
begin
  Result := TcxCustomEditorRowProperties(FProperties);
end;

function TcxCustomEditorRow.GetStyles: TcxEditorRowStyles;
begin
  Result := TcxEditorRowStyles(FStyles);
end;

procedure TcxCustomEditorRow.SetProperties(Value: TcxCustomEditorRowProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomEditorRow.SetStyles(Value: TcxEditorRowStyles);
begin
  FStyles.Assign(Value);
  Changed;
end;

{ TcxEditorRow }

function TcxEditorRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxEditorRowProperties;
end;

function TcxEditorRow.GetProperties: TcxEditorRowProperties;
begin
  Result := TcxEditorRowProperties(FProperties);
end;

procedure TcxEditorRow.SetProperties(Value: TcxEditorRowProperties);
begin
  FProperties.Assign(Value);
end;

function TcxStyle3DPainter.GetExpandButtonState(ARowHeader: TcxCustomRowHeaderInfo): TcxExpandButtonState;
begin
  Result := cebsNormal;
end;

{ TcxvgCustomRowStyles }

constructor TcxvgCustomRowStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
end;

procedure TcxvgCustomRowStyles.Assign(Source: TPersistent);
begin
  if Source is TcxCategoryRowStyles then
    Header := TcxCategoryRowStyles(Source).Header;
  inherited Assign(Source);
end;

procedure TcxvgCustomRowStyles.Changed(AIndex: Integer);
begin
  inherited Changed(AIndex);
  VerticalGrid.UpdateViewStyles;
end;

function TcxvgCustomRowStyles.GetHeaderParams(
  ARow: TcxCustomRow): TcxViewParams;
begin
  GetViewParams(vgrs_Header, ARow, nil, Result);
end;

function TcxvgCustomRowStyles.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomRow(GetOwner).VerticalGrid;
end;

{ TcxCategoryRowStyles }

procedure TcxCategoryRowStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    vgrs_Header:
      AParams := VerticalGrid.Styles.GetCategoryParams(TcxCustomRow(AData));
  end;
end;

{ TcxEditorRowStyles }

procedure TcxEditorRowStyles.Assign(Source: TPersistent);
begin
  if Source is TcxEditorRowStyles then
    Content := TcxEditorRowStyles(Source).Content;
  inherited Assign(Source);
end;

function TcxEditorRowStyles.GetContentParams(
  AEditorRowProperties: TcxCustomEditorRowProperties; AFocused: Boolean;
  ARecordIndex: Integer): TcxViewParams;
var
  AData: TcxvgContentParamsData;
begin
  AData.EditorRowProperties := AEditorRowProperties;
  AData.Focused := AFocused;
  AData.Index := ARecordIndex;
  GetViewParams(vgrs_Content, @AData, nil, Result);
end;

procedure TcxEditorRowStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
var
  AContentInfo: PcxvgContentParamsData absolute AData;
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    vgrs_Content:
      AParams := VerticalGrid.Styles.GetContentParams(
        AContentInfo.EditorRowProperties,
        AContentInfo.Focused,
        AContentInfo.Index);
    vgrs_Header:
      AParams := VerticalGrid.Styles.GetHeaderParams(TcxCustomRow(AData));
  end;
end;

{ TcxVerticalGridStyleSheet }

function TcxVerticalGridStyleSheet.GetStylesValue: TcxVerticalGridStyles;
begin
  Result := TcxVerticalGridStyles(GetStyles)
end;

procedure TcxVerticalGridStyleSheet.SetStylesValue(Value: TcxVerticalGridStyles);
begin
  SetStyles(Value);
end;

class function TcxVerticalGridStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxVerticalGridStyles;
end;

{ TcxVerticalGridStyles }

constructor TcxVerticalGridStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  BitmapInViewParams := True;
  FUseOddEvenStyles := bDefault;
end;

procedure TcxVerticalGridStyles.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TcxVerticalGridStyles then
  begin
    for I := vgs_Category to vgs_MaxIndex do
      SetValue(I, TcxVerticalGridStyles(Source).GetValue(I));
    FUseOddEvenStyles := TcxVerticalGridStyles(Source).UseOddEvenStyles;
  end;
end;

function TcxVerticalGridStyles.GetCalcHelper: TcxvgCustomPaintStyleCalcHelper;
begin
  Result := TcxCustomVerticalGrid(Control).ViewInfo.CalcHelper;
end;

function TcxVerticalGridStyles.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(Control);
end;

procedure TcxVerticalGridStyles.SetUseOddEvenStyles(const AValue: TdxDefaultBoolean);
begin
  if AValue <> FUseOddEvenStyles then
  begin
    FUseOddEvenStyles := AValue;
    Changed(ecs_Content);
  end;
end;

function TcxVerticalGridStyles.GetCategoryParams(ARow: TcxCustomRow): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetCategoryStyle) then
    FOnGetCategoryStyle(Control, ARow, AStyle);
  GetViewParams(vgs_Category, ARow, AStyle, Result);
end;

function TcxVerticalGridStyles.GetContentParams(
  AEditorRowProperties: TcxCustomEditorRowProperties;
  AFocused: Boolean; ARecordIndex: Integer): TcxViewParams;
const
  StyleIndexes: array[Boolean] of Integer = (vgs_ContentEven, vgs_ContentOdd);
var
  AData: TcxvgContentParamsData;
  AStyle: TcxStyle;
  AStyleIndex: Integer;
begin
  AStyle := nil;
  AData.Focused := AFocused;
  AData.EditorRowProperties := AEditorRowProperties;
  if Assigned(AEditorRowProperties) then
  begin
    if Assigned(FOnGetContentStyle) then
      FOnGetContentStyle(Control, AEditorRowProperties, AFocused, ARecordIndex, AStyle);
    AData.Index := VerticalGrid.GetRowContentStyleIndex(AEditorRowProperties, ARecordIndex);
  end
  else
    AData.Index := ARecordIndex;
  AStyleIndex := StyleIndexes[Odd(AData.Index)]; // otherwise: [Fatal Error] cxVGrid.pas(12420): Internal error: C1475
  if ActuallyUseOddEvenStyles and (GetValue(AStyleIndex) <> nil) then
    GetViewParams(AStyleIndex, @AData, AStyle, Result)
  else
    GetViewParams(vgs_Content, @AData, AStyle, Result);
  if Assigned(AEditorRowProperties) then
    VerticalGrid.ConditionalFormattingProvider.CalculateViewParams(Result, ARecordIndex, AEditorRowProperties.ItemIndex);
end;

function TcxVerticalGridStyles.GetHeaderParams(ARow: TcxCustomRow): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetHeaderStyle) then
    FOnGetHeaderStyle(Control, ARow, AStyle);
  GetViewParams(vgs_Header, ARow, AStyle, Result);
end;

function TcxVerticalGridStyles.GetIncSearchParams: TcxViewParams;
begin
  GetViewParams(vgs_IncSearch, nil, IncSearch, Result);
end;

function TcxVerticalGridStyles.GetSelectedHeaderParams(ARow: TcxCustomRow; AFocused: Boolean): TcxViewParams;
const
  StyleIndexes: array[Boolean] of Integer = (ecs_Inactive, ecs_Selection);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetHeaderStyle) then
    FOnGetHeaderStyle(Control, ARow, AStyle);
  GetViewParams(StyleIndexes[AFocused], ARow, AStyle, Result);
end;

function TcxVerticalGridStyles.GetSelectedHeaderParams(ARow: TcxCustomRow): TcxViewParams;
begin
  Result := GetSelectedHeaderParams(ARow, VerticalGrid.Controller.Focused or VerticalGrid.IsFocused);
end;

function TcxVerticalGridStyles.ActuallyUseOddEvenStyles: Boolean;
begin
  Result := ((UseOddEvenStyles = bDefault) and (Assigned(ContentOdd) or Assigned(ContentEven) or
    LookAndFeelPainter.GridLikeControlDefaultUseOddEvenStyle)) or (UseOddEvenStyles = bTrue);
end;

procedure TcxVerticalGridStyles.GetDefaultViewParams(
  Index: Integer; AData: TObject; out AParams: TcxViewParams);
const
  ContentIndexMap: array[Boolean] of Integer = (vgs_ContentEven, vgs_ContentOdd);
var
  AContentInfo: PcxvgContentParamsData absolute AData;
begin
  inherited GetDefaultViewParams(Index, nil, AParams);
  with AParams do
  begin
    if not ActuallyUseOddEvenStyles and ((Index = vgs_ContentEven) or (Index = vgs_ContentOdd))  then
      Index := vgs_Content;
    case Index of
      ecs_Background:
        Color := CalcHelper.GetBackgroundColor;
      vgs_Content:
        if (AContentInfo <> nil) and (AContentInfo.EditorRowProperties <> nil) and ActuallyUseOddEvenStyles then
          GetDefaultViewParams(ContentIndexMap[Odd(AContentInfo.Index)], AData, AParams)
        else
        begin
          TextColor := CalcHelper.GetContentTextColor;
          Color := CalcHelper.GetContentColor((AContentInfo <> nil) and AContentInfo.Focused);
        end;
      vgs_ContentEven:
        begin
          Color := CalcHelper.GetContentEvenColor(AContentInfo.Focused);
          TextColor := CalcHelper.GetContentEvenTextColor;
        end;
      vgs_ContentOdd:
        begin
          Color := CalcHelper.GetContentOddColor(AContentInfo.Focused);
          TextColor := CalcHelper.GetContentOddTextColor;
        end;
      vgs_Header:
        begin
          Color := CalcHelper.GetHeaderColor;
          TextColor := CalcHelper.GetHeaderTextColor;
        end;
      vgs_Category:
        begin
          Font := VerticalGrid.CategoryFont;
          Color := CalcHelper.GetCategoryColor;
          TextColor := CalcHelper.GetCategoryTextColor;
        end;
      vgs_IncSearch:
        begin
          Color := clDefault;
          TextColor := clDefault;
        end;
    end;
  end;
end;

function TcxVerticalGridStyles.GetFindPanelDefaultColor: TColor;
begin
  Result := CalcHelper.GetFindPanelColor;
end;

function TcxVerticalGridStyles.GetFindPanelDefaultTextColor: TColor;
begin
  Result := CalcHelper.GetContentTextColor;
end;

{ TcxCellEdit }

constructor TcxCellEdit.Create(AOwner: TComponent);
begin
  FRow := TcxCustomRow(AOwner);
  inherited Create(AOwner);
end;

destructor TcxCellEdit.Destroy;
begin
  cxClearObjectLinks(Self);
  if (VerticalGrid <> nil) and not VerticalGrid.IsDestroying then
  begin
    VerticalGrid.ViewInfo.IsDirty := True;
    if Controller.FocusedItem = Self then
      Controller.FocusedItem := nil;
  end;
  inherited Destroy;
end;

procedure TcxCellEdit.Calculate(ACellViewInfo: TcxRowValueInfo);
begin
  FCalculating := True;
  try
    InitEditViewInfo(ACellViewInfo);
    ACellViewInfo.ViewData.UseRightToLeftAlignment := VerticalGrid.UseRightToLeftAlignment;
    ACellViewInfo.ViewData.UseRightToLeftReading := VerticalGrid.UseRightToLeftReading;
    ACellViewInfo.ViewData.UseRightToLeftScrollBar := VerticalGrid.UseRightToLeftScrollBar;
    if not cxRectIsEmpty(ACellViewInfo.ContentRect) then
      CalculateEditViewInfo(ACellViewInfo.CellValue, ACellViewInfo, cxInvalidPoint);
  finally
    FCalculating := False;
  end;
end;

function TcxCellEdit.CanEdit: Boolean;
begin
  Result := CanFocus and
    TcxCustomVerticalGrid(EditingControl).OptionsData.Editing and
    TcxEditorRowPropertiesOptions(Options).Editing and
    (dceoShowEdit in DataController.EditOperations);
end;

function TcxCellEdit.CanFind: Boolean;
begin
  Result := inherited CanFind and Row.Visible;
end;

function TcxCellEdit.CanFocus: Boolean;
begin
  Result := Row.Options.Focusing and inherited CanFocus;
end;

function TcxCellEdit.CanInitEditing: Boolean;
begin
  with TcxCustomVerticalGrid(EditingControl) do
  begin
    Result := inherited CanInitEditing and ((DragAndDropState = ddsNone) or
      ((DragMode <> dmAutomatic) or not HitTest.HitAtValue));
  end;
end;

function TcxCellEdit.CanTabStop: Boolean;
begin
  Result := Row.Options.TabStop and inherited CanTabStop;
end;

procedure TcxCellEdit.DoCalculateEditViewInfo(AEditViewInfo: TcxEditCellViewInfo);
begin
  inherited DoCalculateEditViewInfo(AEditViewInfo);
  VerticalGrid.ConditionalFormattingProvider.CalculateStyleViewInfo(AEditViewInfo);
end;

procedure TcxCellEdit.DoGetDisplayText(ARecordIndex: TdxNativeInt; var AText: string);
begin
  EditRowProperties.DoGetDisplayTextEvent(ARecordIndex, AText);
end;

function TcxCellEdit.DoGetPropertiesFromEvent(AEvent: TcxGetEditPropertiesEvent;
  AData: Pointer; AProperties: TcxCustomEditProperties): TcxCustomEditProperties;
begin
  Result := AProperties;
  if Assigned(AEvent) then
  begin
    AEvent(FEditRowProperties, AData, Result);
    if Result = nil then
      Result := AProperties;
  end;
  InitProperties(Result);
end;

function TcxCellEdit.GetCurrentValue: Variant;
begin
  with DataController do
    if (RowCount = 0) or (FocusedRowIndex < 0) then
      Result := Null
    else
      Result := Values[FilteredRecordIndex[FocusedRowIndex], ItemIndex];
end;

function TcxCellEdit.GetDataBindingClass: TcxItemDataBindingClass;
begin
  Result := TcxItemDataBinding;
end;

function TcxCellEdit.GetDisplayValue(AProperties: TcxCustomEditProperties;
  ARecordIndex: Integer): Variant;
begin
  with DataController do
  begin
    if RowCount = 0 then
      Result := Null
    else
      if (AProperties.GetEditValueSource(False) = evsValue) or VarIsNull(GetValue(FilteredRecordIndex[ARecordIndex], ItemIndex)) then
        Result := GetValue(FilteredRecordIndex[ARecordIndex], ItemIndex)
      else
        Result := GetDisplayText(FilteredRecordIndex[ARecordIndex], ItemIndex);
  end;
end;

function TcxCellEdit.GetEditValue: Variant;
begin
  if Editing then
  begin
    if DataController.RowCount = 0 then
      Result := Null
    else
      Result := DataController.GetEditValue(ItemIndex, EditValueSource);
  end
  else
    Result := Unassigned;
end;

function TcxCellEdit.GetFilterCaption: string;
begin
  Result := EditRowProperties.Caption;
end;

function TcxCellEdit.GetFilterLinkName: string;
begin
  Result := 'Item' + IntToStr(EditRowProperties.ID);
end;

function TcxCellEdit.GetOptionsClass: TcxCustomEditContainerItemOptionsClass;
begin
  if Row is TcxCustomMultiEditorRow then
    Result := TcxMultiEditorRowPropertiesOptions
  else
    Result := TcxEditorRowPropertiesOptions;
end;

function TcxCellEdit.GetValue(ARecordIndex: Integer): Variant;
begin
  with DataController do
    if RowCount = 0 then
      Value := Null
    else
      Result := Values[FilteredRecordIndex[ARecordIndex], ItemIndex];
end;

function TcxCellEdit.GetValueCount: Integer;
begin
  Result := DataController.RowCount;
end;

function TcxCellEdit.HasDataTextHandler: Boolean;
begin
  Result := Assigned(EditRowProperties.OnGetDisplayText);
end;

procedure TcxCellEdit.SetCurrentValue(const Value: Variant);
begin
  with DataController do
    if (RowCount > 0) and (FocusedRowIndex >= 0) then
      Values[FilteredRecordIndex[FocusedRowIndex], ItemIndex] := Value;
end;

procedure TcxCellEdit.SetValue(ARecordIndex: Integer; const Value: Variant);
begin
  with DataController do
    if RowCount > 0 then
      Values[FilteredRecordIndex[ARecordIndex], ItemIndex] := Value;
end;

procedure TcxCellEdit.ValidateDrawValue(const Value: Variant; AEditViewInfo: TcxEditCellViewInfo);
begin
  inherited;
  EditRowProperties.DoValidateDrawValue(Value, AEditViewInfo.RecordIndex, AEditViewInfo.EditViewInfo.ErrorData);
end;

function TcxCellEdit.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(EditingControl);
end;

function TcxCellEdit.GetViewInfo: TcxCustomRowViewInfo;
begin
  Result := FRow.ViewInfo;
end;

{ TcxCategoryRow }

function TcxCategoryRow.CreateHeaderInfo: TcxCustomRowHeaderInfo;
begin
  Result := TcxCategoryRowHeaderInfo.Create(Self);
end;

function TcxCategoryRow.CreateViewInfo: TcxCustomRowViewInfo;
begin
  Result := TcxCategoryRowViewInfo.Create(Self);
end;

function TcxCategoryRow.IsCategory: Boolean;
begin
  Result := True;
end;

function TcxCategoryRow.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Caption');
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxCategoryRow.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Caption' then
    AValue := Properties.Caption
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TcxCategoryRow.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if (AName = 'Caption') and not VarIsEmpty(AValue) then
    Properties.Caption := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

function TcxCategoryRow.GetDefaultHeight: Integer;
begin
  Result := Max(VerticalGrid.OptionsView.RowHeight,
    cxTextHeight(VerticalGrid.Styles.GetCategoryParams(Self).Font) + ScaleFactor.Apply(cxTextOffset) * 2);
  if VerticalGrid.ViewInfo.UseCategoryExplorerStyle and (VerticalGrid.LookAndFeel.SkinPainter = nil) then
    Result := Max(Result, VerticalGrid.ViewInfo.ExplorerButtonAreaSize.cy + 2);
end;

function TcxCategoryRow.GetStylesClass: TcxvgCustomRowStylesClass;
begin
  Result := TcxCategoryRowStyles;
end;

function TcxCategoryRow.GetProperties: TcxCaptionRowProperties;
begin
  Result := TcxCaptionRowProperties(FProperties);
end;

function TcxCategoryRow.GetStyles: TcxCategoryRowStyles;
begin
  Result := TcxCategoryRowStyles(FStyles);
end;

procedure TcxCategoryRow.SetProperties(Value: TcxCaptionRowProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCategoryRow.SetStyles(Value: TcxCategoryRowStyles);
begin
  FStyles.Assign(Value);
  Changed;
end;

{ TcxEditorRowPropertiesFilterPopupOptions }

function TcxEditorRowPropertiesFilterPopupOptions.GetIncrementalFilteringOptionsClass: TcxCustomEditContainerItemFilterPopupIncrementalFilteringOptionsClass;
begin
  Result := TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions;
end;

function TcxEditorRowPropertiesFilterPopupOptions.GetIncrementalFiltering: TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions;
begin
  Result := TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions(inherited IncrementalFiltering);
end;

procedure TcxEditorRowPropertiesFilterPopupOptions.SetIncrementalFiltering(
  AValue: TcxEditorRowPropertiesFilterPopupIncrementalFilteringOptions);
begin
  inherited IncrementalFiltering := AValue;
end;

{ TcxEditorRowPropertiesOptions }

function TcxEditorRowPropertiesOptions.GetExcelFilterPopupOptionsClass: TcxCustomEditContainerItemExcelFilterPopupOptionsClass;
begin
  Result := TcxEditorRowPropertiesExcelFilterPopupOptions;
end;

function TcxEditorRowPropertiesOptions.GetFilterPopupOptionsClass: TcxCustomEditContainerItemFilterPopupOptionsClass;
begin
  Result := TcxEditorRowPropertiesFilterPopupOptions;
end;

function TcxEditorRowPropertiesOptions.GetExcelFilterPopup: TcxEditorRowPropertiesExcelFilterPopupOptions;
begin
  Result := TcxEditorRowPropertiesExcelFilterPopupOptions(inherited ExcelFilterPopup);
end;

function TcxEditorRowPropertiesOptions.GetFilterPopup: TcxEditorRowPropertiesFilterPopupOptions;
begin
  Result := TcxEditorRowPropertiesFilterPopupOptions(inherited FilterPopup);
end;

procedure TcxEditorRowPropertiesOptions.SetFilterPopup(AValue: TcxEditorRowPropertiesFilterPopupOptions);
begin
  inherited FilterPopup := AValue;
end;

procedure TcxEditorRowPropertiesOptions.SetExcelFilterPopup(AValue: TcxEditorRowPropertiesExcelFilterPopupOptions);
begin
  inherited ExcelFilterPopup := AValue;
end;

{ TcxRowList }

constructor TcxRowList.Create(AOwner: TcxCustomRow);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TcxRowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action in [lnAdded, lnDeleted]) and not (csDestroying in Owner.ComponentState) then
    UpdateIndexes;
end;

procedure TcxRowList.UpdateIndexes;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TcxCustomRow(Items[I]).FIndex := I;
end;

{ TcxCustomRow }

constructor TcxCustomRow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndex := -1;
  FExpanded := True;
  FVisible := True;
  FHeight := -1;
  FStyles := GetStylesClass.Create(Self);
  FProperties := GetPropertiesClass.CreateEx(Self);
  FOptions := GetOptionsClass.Create(Self);
  FVisibleIndex := -1;
  FLoadedID := -1;
  FVersion := cxVerticalGridStoringVersion;
end;

destructor TcxCustomRow.Destroy;
begin
  cxClearObjectLinks(Self);
  VerticalGrid.RemoveRowFromVerticalGrid(Self);
  FreeAndNil(FViewInfo);
  FreeAndNil(FStyles);
  FreeAndNil(FProperties);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TcxCustomRow.Assign(Source: TPersistent);
begin
  if Source is TcxCustomRow then
    with TcxCustomRow(Source) do
    begin
      Self.FHeight := FHeight; //need a direct assign
      Self.FVisible := FVisible;
      Self.FExpanded := FExpanded;
      Self.FProperties.Assign(FProperties);
      Self.Styles.Assign(Styles);
    end
  else
    inherited Assign(Source);
end;

function TcxCustomRow.GetParentComponent: TComponent;
begin
  if (FParent = nil) or ((FVersion > 0) and (csReading in ComponentState)) then
    Result := VerticalGrid
  else
    Result := FParent;
end;

function TcxCustomRow.HasParent: Boolean;
begin
  Result := True;
end;

procedure TcxCustomRow.Collapse(ARecurse: Boolean);
var
  I: Integer;
begin
  if HasChildren then
  begin
    OwnerRows.BeginUpdate;
    try
      FExpanded := False;
      if ARecurse then
        for I := 0 to Count - 1 do
          Rows[I].Collapse(ARecurse);
    finally
      OwnerRows.EndUpdate;
    end
  end
  else FExpanded := False;
end;

procedure TcxCustomRow.Expand(ARecurse: Boolean);
var
  I: Integer;
begin
  if HasChildren then
  begin
    OwnerRows.BeginUpdate;
    try
      FExpanded := True;
      if ARecurse then
        for I := 0 to Count - 1 do
          Rows[I].Expand(ARecurse);
    finally
      OwnerRows.EndUpdate;
    end;
  end
  else FExpanded := True;
end;

function TcxCustomRow.GetFirstVisibleChild: TcxCustomRow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Rows[I].Visible then
    begin
      Result := Rows[I];
      break;
    end;
end;

function TcxCustomRow.GetLastVisibleChild: TcxCustomRow;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
  begin
    ARow := Rows[I];
    if ARow.Visible then
    begin
      if ARow.Expanded then ARow := ARow.GetLastVisibleChild;
      if ARow <> nil then Result := ARow else Result := Rows[I];
      break;
    end;
  end;
end;

function TcxCustomRow.HasChildren: Boolean;
begin
  Result := Count > 0;
end;

function TcxCustomRow.HasVisibleChildren: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Rows[I].Visible then
    begin
      Result := True;
      break;
    end;
end;

function TcxCustomRow.IndexOf(ARow: TcxCustomRow): Integer;
begin
  Result := -1;
  if FRows <> nil then Result := FRows.IndexOf(ARow);
end;

function TcxCustomRow.IsCategory: Boolean;
begin
  Result := False;
end;

function TcxCustomRow.IsChild(ARow: TcxCustomRow): Boolean;
begin
  Result := (ARow <> nil) and ARow.IsParent(Self)
end;

function TcxCustomRow.IsFirst: Boolean;
begin
  Result := Index = 0;
end;

function TcxCustomRow.IsFirstVisible: Boolean;
begin
  Result := Self = FParent.GetFirstVisibleChild;
end;

function TcxCustomRow.IsLast: Boolean;
begin
  Result := Index = FParent.Count - 1;
end;

function TcxCustomRow.IsLastVisible: Boolean;
var
  I: Integer;
  ARow: TcxCustomRow;
begin
  Result := False;
  with FParent do
    for I := Count - 1 downto 0 do
    begin
      ARow := Rows[I];
      if ARow.Visible then
        if ARow = Self then
        begin
          Result := True;
          break;
        end
        else
          break
    end;
end;

function TcxCustomRow.IsRootLevel: Boolean;
begin
  Result := FParent = VerticalGrid.RootRow;
end;

function TcxCustomRow.IsParent(ARow: TcxCustomRow): Boolean;
begin
  Result := False;
  while ARow <> nil do
  begin
    if ARow.FParent = Self then
    begin
      Result := True;
      break;
    end;
    ARow := ARow.FParent;
  end;
end;

procedure TcxCustomRow.MakeVisible;
begin
  VerticalGrid.Controller.MakeRowVisible(Self);
end;

procedure TcxCustomRow.RestoreDefaults;
begin
  FHeight := -1;
  Options.RestoreDefaults; // indirect call the row's changed method
end;

procedure TcxCustomRow.ChangeScale(M, D: Integer);
begin
  if Height > 0 then
    Height := MulDiv(Height, M, D);
  if FProperties <> nil then
    FProperties.ChangeScale(M, D);
end;

function TcxCustomRow.GetObjectName: string;
begin
  if VerticalGrid.FNewLoadMode then
  begin
    Result := Name;
    if Result = '' then
      Result := IntToStr(FID);
  end
  else
    Result := IntToStr(FID);
end;

procedure TcxCustomRow.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Visible' then
    AValue := Visible
  else
    if AName = 'Height' then
      AValue := Height
    else
      if AName = 'Expanded' then
        AValue := Expanded
      else
        if AName = 'AParentID' then
        begin
          if VerticalGrid.FNewLoadMode then
            AValue := FParent.GetObjectName
          else
            AValue := FParent.ID;
        end
        else
          if AName = 'Index' then
            AValue := Index
          else
            AValue := Null;
end;

function TcxCustomRow.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('AParentID');
  AProperties.Add('Index');
  AProperties.Add('Visible');
  AProperties.Add('Height');
  AProperties.Add('Expanded');
  Result := True;
end;

procedure TcxCustomRow.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Visible' then
    Visible := AValue
  else
    if AName = 'Height' then
      Height := AValue
    else
      if AName = 'Expanded' then
        Expanded := AValue
      else
        if AName = 'AParentID' then
        begin
          if VerticalGrid.FNewLoadMode then
            FLoadingParent := AValue
          else
            Parent := OwnerRows.FindRowByID(AValue);
        end
        else
          if AName = 'Index' then
            if VerticalGrid.FNewLoadMode then
              FLoadingIndex := AValue
            else
              Index := AValue;
end;

procedure TcxCustomRow.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if not (csLoading in ComponentState) then
    Changed;
end;

procedure TcxCustomRow.SetParentComponent(Value: TComponent);
begin
  if FParent <> nil then FParent.Remove(Self);
  if Value <> nil then
    if Value is TcxCustomVerticalGrid then
      TcxCustomVerticalGrid(Value).FRootRow.Add(Self)
    else
      if Value is TcxCustomRow then
        TcxCustomRow(Value).Add(Self);
end;

procedure TcxCustomRow.Add(ARow: TcxCustomRow);
begin
  Insert(GetCount, ARow);
end;

function TcxCustomRow.CanFocus: Boolean;
begin
  Result := Options.Focusing;
end;

function TcxCustomRow.CanNavigate: Boolean;
begin
  Result := CanTabStop and CanFocus;
end;

function TcxCustomRow.CanTabStop: Boolean;
begin
  Result := Options.TabStop;
end;

procedure TcxCustomRow.Changed(ARebuild: Boolean = False);
begin
  if OwnerRows <> nil then
    OwnerRows.Changed(ARebuild);
end;

procedure TcxCustomRow.CheckUsingInFind;
var
  I: Integer;
begin
  for I := 0 to GetEditContainerCount - 1 do
    GetEditContainer(I).CheckUsingInFind;
end;

procedure TcxCustomRow.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ID', ReadID, WriteID, True);
  Filer.DefineProperty('ParentID', ReadLoadedParentID, WriteLoadedParentID, True);
  Filer.DefineProperty('Index', ReadLoadedIndex, WriteLoadedIndex, True);
  Filer.DefineProperty('Version', ReadVersion, WriteVersion, True);
end;

procedure TcxCustomRow.Delete(AIndex: Integer);
var
  ARow: TcxCustomRow;
begin
  if (AIndex < 0) or (FRows = nil) or (AIndex >= GetCount) then
    cxVerticalGridError(cxSvgIndexError);
  ARow := FRows[AIndex];
  FRows.Delete(AIndex);
  ARow.FParent := nil;
  ResetOwnerCount;
end;

function TcxCustomRow.GetDefaultHeight: Integer;
begin
    Result := Max(VerticalGrid.OptionsView.RowHeight, cxTextHeight(
      VerticalGrid.Styles.GetHeaderParams(Self).Font) + ScaleFactor.Apply(cxTextOffset));
end;

function TcxCustomRow.GetEditContainer(ACellIndex: Integer): TcxCellEdit;
begin
  Result := nil;
end;

function TcxCustomRow.GetEditContainerCount: Integer;
begin
  Result := 0;
end;

function TcxCustomRow.GetPropertiesClass: TcxRowPropertiesClass;
begin
  Result := TcxCaptionRowProperties;
end;

function TcxCustomRow.GetRealHeight: Integer;
begin
  Result := Max(FHeight, GetDefaultHeight);
end;

function TcxCustomRow.GetOptionsClass: TcxRowOptionsClass;
begin
  Result := TcxRowOptions;
end;

function TcxCustomRow.GetStylesClass: TcxvgCustomRowStylesClass;
begin
  Result := TcxvgCustomRowStyles;
end;

procedure TcxCustomRow.Insert(AIndex: Integer; ARow: TcxCustomRow);
begin
  if FRows = nil then
    FRows := TcxRowList.Create(Self);
  ARow.FParent := Self;
  ARow.SetVerticalGrid(VerticalGrid);
  FRows.Insert(AIndex, ARow);
  ResetOwnerCount;
end;

function TcxCustomRow.IsHeightAssigned: Boolean;
begin
  Result := FHeight >= 0;
end;

procedure TcxCustomRow.Refresh;
begin
  if Visible and not (csDestroying in ComponentState) then
    ViewInfo.Update;
end;

procedure TcxCustomRow.Remove(ARow: TcxCustomRow);
begin
  Delete(IndexOf(ARow));
end;

procedure TcxCustomRow.RemoveAll;
begin
  while Count > 0 do Rows[0].Free;
  FreeAndNil(FRows);
  ResetOwnerCount;
end;

procedure TcxCustomRow.RemoveChildren;
begin
  if (FParent <> nil) then
  begin
    FParent.Remove(Self);
    FParent := nil;
  end;
  RemoveAll;
end;

procedure TcxCustomRow.ResetOwnerCount;
begin
  OwnerRows.FCount := -1;
end;

procedure TcxCustomRow.RestoreIndex;
begin
  Index := FLoadedIndex;
end;

procedure TcxCustomRow.RestoreParent;
begin
  Parent := OwnerRows.FindLoadedParent(FLoadedParentID);
end;

procedure TcxCustomRow.SetVerticalGrid(Value: TcxCustomVerticalGrid);
begin
  if FVerticalGrid <> Value then
  begin
    FVerticalGrid := Value;
    if Value <> nil then
      FID := Value.Rows.GetNextID;
  end;
end;

function TcxCustomRow.GetAbsoluteIndex: Integer;
begin
  Result := OwnerRows.IndexOf(Self);
end;

function TcxCustomRow.GetCount: Integer;
begin
  if FRows = nil then Result := 0 else Result := FRows.Count;
end;

function TcxCustomRow.GetFocused: Boolean;
begin
  Result := VerticalGrid.FocusedRow = Self;
end;

function TcxCustomRow.GetLevel: Integer;
var
  P: TcxCustomRow;
begin
  Result := 0;
  P := FParent;
  while (P <> nil) and (P <> VerticalGrid.RootRow) do
  begin
    P := P.FParent;
    Inc(Result);
  end;
end;

function TcxCustomRow.GetOwnerRows: TcxVerticalGridRows;
begin
  Result := FVerticalGrid.Rows;
end;

function TcxCustomRow.GetParent: TcxCustomRow;
begin
  if FParent = FVerticalGrid.FRootRow then
    Result := nil
  else
    Result := FParent;
end;

function TcxCustomRow.GetRow(Index: Integer): TcxCustomRow;
begin
  if FRows = nil then
    cxVerticalGridError(cxSvgIndexError);
  Result := FRows[Index];
end;

function TcxCustomRow.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactor(VerticalGrid);
end;

function TcxCustomRow.GetViewInfo: TcxCustomRowViewInfo;
begin
  if FViewInfo = nil then
    FViewInfo := CreateViewInfo;
  Result := FViewInfo;
end;

procedure TcxCustomRow.SetExpanded(Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    Changed(HasChildren);
  end;
end;

procedure TcxCustomRow.SetFocused(Value: Boolean);
begin
  if Value <> Focused then
    if Value then
      VerticalGrid.FocusedRow := Self
    else
      VerticalGrid.FocusedRow := nil;
end;

procedure TcxCustomRow.SetHeight(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TcxCustomRow.SetIndex(Value: Integer);
var
  AIndex: Integer;
begin
  if (FParent <> nil) and (Value >= 0) and (Value < FParent.GetCount) then
  begin
    AIndex := Index;
    if Value <> AIndex then
    begin
      FParent.FRows.Move(AIndex, Value);
      FParent.FRows.UpdateIndexes;
      Changed(True);
    end;
  end;
end;

procedure TcxCustomRow.SetOptions(Value: TcxRowOptions);
begin
  FOptions.Assign(Value);
end;

procedure TcxCustomRow.SetParent(Value: TcxCustomRow);
begin
  if Value = Self then Exit;
  if Value = nil then
    Value := FVerticalGrid.FRootRow;
  if FParent <> Value then
  begin
    if FParent <> nil then
      FParent.Remove(Self);
    FParent := Value;
    FParent.Add(Self);
    Changed(True);
  end;
end;

procedure TcxCustomRow.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    CheckUsingInFind;
    Changed(True);
  end;
end;

procedure TcxCustomRow.ReadID(Reader: TReader);
begin
  FLoadedID := Reader.ReadInteger;
end;

procedure TcxCustomRow.ReadLoadedIndex(Reader: TReader);
begin
  FLoadedIndex := Reader.ReadInteger;
end;

procedure TcxCustomRow.ReadLoadedParentID(Reader: TReader);
begin
  FLoadedParentID := Reader.ReadInteger;
end;

procedure TcxCustomRow.ReadVersion(Reader: TReader);
begin
  FVersion := Reader.ReadInteger;
end;

procedure TcxCustomRow.WriteID(Writer: TWriter);
begin
  Writer.WriteInteger(ID);
end;

procedure TcxCustomRow.WriteLoadedIndex(Writer: TWriter);
begin
  Writer.WriteInteger(Index);
end;

procedure TcxCustomRow.WriteLoadedParentID(Writer: TWriter);
var
  AParentID: Integer;
begin
  if Parent = nil then
    AParentID := -1
  else
    AParentID := Parent.ID;
  Writer.WriteInteger(AParentID);
end;

procedure TcxCustomRow.WriteVersion(Writer: TWriter);
begin
  Writer.WriteInteger(cxVerticalGridStoringVersion);
end;

{ TcxRowOptions }

constructor TcxRowOptions.Create(ARow: TcxCustomRow);
begin
  FRow := ARow;
  FCanAutoHeight := True;
  FCanMovedToCustomizationForm := True;
  FCanResized := True;
  FFocusing := True;
  FMoving := True;
  FShowExpandButton := True;
  FShowInCustomizationForm := True;
  FTabStop := True;
end;

procedure TcxRowOptions.Assign(Source: TPersistent);
begin
  if Source is TcxRowOptions then
    with TcxRowOptions(Source) do
    begin
      Self.FCanAutoHeight := CanAutoHeight;
      Self.FCanMovedToCustomizationForm := CanMovedToCustomizationForm;
      Self.FCanResized := CanResized;
      Self.FFocusing := Focusing;
      Self.FMoving := Moving;
      Self.FShowExpandButton := ShowExpandButton;
      Self.FShowInCustomizationForm := ShowInCustomizationForm;
      Self.FTabStop := TabStop;
      Self.Changed;
    end
    else
      inherited Assign(Source);
end;

procedure TcxRowOptions.RestoreDefaults;
begin
  FCanAutoHeight := True;
  FCanMovedToCustomizationForm := True;
  FCanResized := True;
  FFocusing := True;
  FMoving := True;
  FShowExpandButton := True;
  FShowInCustomizationForm := True;
  FTabStop := True;
  Changed;
end;

procedure TcxRowOptions.Changed;
begin
  FRow.Changed;
end;

procedure TcxRowOptions.SetCanAutoHeight(Value: Boolean);
begin
  if FCanAutoHeight <> Value then
  begin
    FCanAutoHeight := Value;
    Changed;
  end;
end;

procedure TcxRowOptions.SetFocusing(Value: Boolean);
begin
  if FFocusing <> Value then
  begin
    FFocusing := Value;
    Changed;
  end;
end;

procedure TcxRowOptions.SetShowExpandButton(Value: Boolean);
begin
  if FShowExpandButton <> Value then
  begin
    FShowExpandButton := Value;
    Changed;
  end;
end;

procedure TcxRowOptions.SetShowInCustomizationForm(Value: Boolean);
begin
  if FShowInCustomizationForm <> Value then
  begin
    FShowInCustomizationForm := Value;
    Changed;
  end;
end;

{ TcxCaptionRowProperties }

constructor TcxCaptionRowProperties.CreateEx(ARow: TcxCustomRow);
begin
  inherited CreateEx(ARow);
  FHeaderAlignmentHorz := taLeftJustify;
  FHeaderAlignmentVert := cxDefaultAlignmentVert;
  FImageIndex := -1;
end;

function TcxCaptionRowProperties.DefaultCaption: string;
begin
  Result := '';
end;

function TcxCaptionRowProperties.GetEditContainer: TcxCellEdit;
begin
  Result := nil;
end;

function TcxCaptionRowProperties.GetFilterableByPopupMenu: Boolean;
begin
  Result := False;
end;

function TcxCaptionRowProperties.GetFiltered: Boolean;
begin
  Result := False;
end;

procedure TcxCaptionRowProperties.RestoreDefaults;
begin
  FIsCaptionAssigned := False;
  Row.Changed;
end;

procedure TcxCaptionRowProperties.SetFiltered(AValue: Boolean);
begin
//do nothing
end;

function TcxCaptionRowProperties.GetCaption: TCaption;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TcxCaptionRowProperties.GetRealHeaderAlignmentHorz: TAlignment;
begin
  Result := HeaderAlignmentHorz;
  if Row.VerticalGrid.UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(Result);
end;

function TcxCaptionRowProperties.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned and (FCaption <> DefaultCaption);
end;

function TcxCaptionRowProperties.IsHeaderAlignmentVertStored: Boolean;
begin
  Result := FHeaderAlignmentVert <> cxDefaultAlignmentVert;
end;

procedure TcxCaptionRowProperties.SetCaption(
  const Value: TCaption);
begin
  if FIsCaptionAssigned and (Value = FCaption) then Exit;
  FCaption := Value;
  FIsCaptionAssigned := True;
  Changed;
end;

procedure TcxCaptionRowProperties.SetHeaderAlignmentHorz(
  const Value: TAlignment);
begin
  if HeaderAlignmentHorz <> Value then
  begin
    FHeaderAlignmentHorz := Value;
    Changed;
  end;
end;

procedure TcxCaptionRowProperties.SetHeaderAlignmentVert(
  const Value: TcxAlignmentVert);
begin
  if HeaderAlignmentVert <> Value then
  begin
    FHeaderAlignmentVert := Value;
    Changed;
  end;
end;

procedure TcxCaptionRowProperties.SetImageIndex(
  const Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TcxCaptionRowProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCaptionRowProperties then
  begin
    Row.OwnerRows.BeginUpdate;
    try
      with TcxCaptionRowProperties(Source) do
      begin
        Self.FCaption := FCaption;
        Self.FIsCaptionAssigned := FIsCaptionAssigned;
        Self.FImageIndex := FImageIndex;
        Self.FHeaderAlignmentHorz := FHeaderAlignmentHorz;
        Self.FHeaderAlignmentVert := FHeaderAlignmentVert;
        Self.FHint := FHint;
      end;
    finally
      Row.OwnerRows.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

{ TcxCustomRowProperties }

constructor TcxCustomRowProperties.Create(Collection: TCollection);
begin
  if Collection <> nil then
    FRow := TcxEditorPropertiesCollection(Collection).Row;
  FCollection := Collection;
  CreateEx(FRow);
end;

constructor TcxCustomRowProperties.CreateEx(ARow: TcxCustomRow);
begin
  FRow := ARow;
  inherited Create(FCollection);
end;

procedure TcxCustomRowProperties.Changed;
begin
  if FCollection <> nil then
    inherited Changed(True)
  else
    if Row.Visible then
      Row.Changed;
end;

procedure TcxCustomRowProperties.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TcxCustomRowProperties.GetOwner: TPersistent;
begin
  Result := FRow;
end;

{ TcxvgMultiRecordsOptionsView }

constructor TcxvgMultiRecordsOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FMultiRecordEvenOddContentStyle := mrcsByRow;
end;

procedure TcxvgMultiRecordsOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxvgMultiRecordsOptionsView then
  begin
    EditingControl.BeginUpdate;
    try
      FRecordsInterval := TcxvgMultiRecordsOptionsView(Source).RecordsInterval;
      FMultiRecordEvenOddContentStyle := TcxvgMultiRecordsOptionsView(Source).MultiRecordEvenOddContentStyle;
      inherited Assign(Source);
    finally
      EditingControl.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxvgMultiRecordsOptionsView.RestoreDefaults;
begin
  FRecordsInterval := 0;
  FMultiRecordEvenOddContentStyle := mrcsByRow;
  inherited RestoreDefaults;
end;

procedure TcxvgMultiRecordsOptionsView.SetRecordsInterval(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FRecordsInterval <> Value then
  begin
    FRecordsInterval := Value;
    Changed;
  end;
end;

procedure TcxvgMultiRecordsOptionsView.SetMultiRecordEvenOddContentStyle(
  const Value: TcxMultiRecordEvenOddContentStyle);
begin
  if FMultiRecordEvenOddContentStyle <> Value then
  begin
    FMultiRecordEvenOddContentStyle := Value;
    Changed;
  end;
end;

{ TcxvgOptionsView }

constructor TcxvgOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoScaleBands := True;
  FGridLineColor := clBtnShadow;
  FBandsInterval := 2;
  FPaintStyle := GetDefaultPaintStyle;
  FRowHeaderMinWidth := 24;
  FRowHeaderWidth := 100;
  FRowHeight := -1;
  FShowButtons := True;
  FShowHeaders := True;
  FGridLines := vglBoth;
  FValueMinWidth := 40;
  FValueWidth := 100;
end;

procedure TcxvgOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxvgOptionsView then
  begin
    VerticalGrid.BeginUpdate;
    try
      inherited Assign(Source);
      with TcxvgOptionsView(Source) do
      begin
        Self.PaintStyle := PaintStyle;
        Self.FGridLineColor := FGridLineColor;
        Self.FGridLineColorAssigned := FGridLineColorAssigned;
        Self.FRowHeaderMinWidth := RowHeaderMinWidth;
        Self.FRowHeight := FRowHeight;
        Self.FValueMinWidth := ValueMinWidth;
        Self.FValueWidth := ValueWidth;
        Self.FRowHeaderWidth := RowHeaderWidth;
        Self.BandsInterval := BandsInterval;
        Self.CategoryExplorerStyle := CategoryExplorerStyle;
        Self.ShowButtons := ShowButtons;
        Self.ShowEmptyRowImage := ShowEmptyRowImage;
        Self.ShowHeaders := ShowHeaders;
        Self.GridLines := GridLines;
        Self.AutoScaleBands := AutoScaleBands;
      end;
      VerticalGrid.ViewInfo.IsDirty := True;
    finally
      VerticalGrid.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxvgOptionsView.RestoreDefaults;
begin
  FGridLineColorAssigned := False;
  FRowHeight := -1;
  FBandsInterval := 2;
  Changed;
end;

procedure TcxvgOptionsView.Changed;
begin
  VerticalGrid.CheckGridModeBufferCount;
  inherited Changed;
end;

procedure TcxvgOptionsView.ChangeScale(M, D: Integer);
var
  ASavedWidth: Integer;
begin
  inherited;

  ASavedWidth := RowHeaderWidth;
  RowHeaderMinWidth := MulDiv(RowHeaderMinWidth, M, D);
  RowHeaderWidth := MulDiv(ASavedWidth, M, D);

  ASavedWidth := ValueWidth;
  ValueMinWidth := MulDiv(FValueMinWidth, M, D);
  ValueWidth := MulDiv(ASavedWidth, M, D);

  if RowHeight >= 0 then
    RowHeight := MulDiv(RowHeight, M, D);
end;

function TcxvgOptionsView.GetDefaultPaintStyle: TcxvgPaintStyle;
begin
  Result := psdotNet;
end;

function TcxvgOptionsView.IsRowHeaderFilterButtonShowedAlways: Boolean;
begin
  Result := IsItemFilterButtonShowedAlways;
end;

function TcxvgOptionsView.IsRowHeaderFilterSmartTag: Boolean;
begin
  Result := IsItemFilterSmartTag;
end;

function TcxvgOptionsView.GetGridLineColor: TColor;
const
  Colors: array[TcxvgPaintStyle] of TColor = (clBtnFace, clBtnShadow);
begin
  if FGridLineColorAssigned then
    Result := FGridLineColor
  else
    Result := Colors[FPaintStyle];
end;

function TcxvgOptionsView.GetRowHeaderFilterButtonShowMode: TcxFilterButtonShowMode;
begin
  Result := ItemFilterButtonShowMode;
end;

function TcxvgOptionsView.GetShowRowHeaderFilterButtons: TcxShowFilterButtons;
begin
  Result := ShowItemFilterButtons;
end;

function TcxvgOptionsView.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(EditingControl);
end;

function TcxvgOptionsView.IsPaintStyleStored: Boolean;
begin
  Result := PaintStyle <> GetDefaultPaintStyle;
end;

procedure TcxvgOptionsView.SetAutoScaleBands(Value: Boolean);
begin
  if FAutoScaleBands <> Value then
  begin
    FAutoScaleBands := Value;
    if Value then
      if VerticalGrid.Controller <> nil then
        VerticalGrid.Controller.Scroller.ScrollStrategy.SetTopVisibleRowIndex(0);
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetShowButtons(Value: Boolean);
begin
  if FShowButtons <> Value then
  begin
    FShowButtons := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetBandsInterval(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBandsInterval <> Value then
  begin
    FBandsInterval := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetCategoryExplorerStyle(Value: Boolean);
begin
  if FCategoryExplorerStyle <> Value then
  begin
    FCategoryExplorerStyle := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetGridLineColor(Value: TColor);
begin
  FGridLineColorAssigned := True;
  FGridLineColor := Value;
  Changed;
end;

procedure TcxvgOptionsView.SetPaintStyle(Value: TcxvgPaintStyle);
begin
  if FPaintStyle <> Value then
  begin
    FPaintStyle := Value;
    VerticalGrid.PaintStyleChanged;
  end;
end;

procedure TcxvgOptionsView.SetRowHeaderFilterButtonShowMode(AValue: TcxFilterButtonShowMode);
begin
  ItemFilterButtonShowMode := AValue;
end;

procedure TcxvgOptionsView.SetRowHeaderMinWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if FRowHeaderMinWidth <> Value then
  begin
    FRowHeaderMinWidth := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetRowHeaderWidth(Value: Integer);
begin
  Value := Max(Value, FRowHeaderMinWidth);
  if FRowHeaderWidth <> Value then
  begin
    FRowHeaderWidth := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetRowHeight(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if FRowHeight <> Value then
  begin
    FRowHeight := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetShowEmptyRowImage(Value: Boolean);
begin
  if FShowEmptyRowImage <> Value then
  begin
    FShowEmptyRowImage := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetShowHeaders(Value: Boolean);
begin
  if FShowHeaders <> Value then
  begin
    FShowHeaders := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetShowRowHeaderFilterButtons(Value: TcxShowFilterButtons);
begin
  ShowItemFilterButtons := Value;
end;

procedure TcxvgOptionsView.SetGridLines(Value: TcxvgGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed;
  end;
end;

procedure TcxvgOptionsView.SetValueMinWidth(Value: Integer);
begin
  Value := Max(cxvgMinValueWidth, Value);
  if FValueMinWidth <> Value then
  begin
    FValueMinWidth := Value;
    ValueWidth := ValueWidth;
  end;
end;

procedure TcxvgOptionsView.SetValueWidth(Value: Integer);
begin
  Value := Max(Value, ValueMinWidth);
  if FValueWidth <> Value then
  begin
    FValueWidth := Value;
    Changed;
  end;
end;

{ TcxvgMultiRecordsOptionsData }

constructor TcxvgMultiRecordsOptionsData.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAppending := True;
  FDeletingConfirmation := True;
  FDeleting := True;
  FInserting := True;
  FMultiThreadedFiltering := bDefault;
end;

procedure TcxvgMultiRecordsOptionsData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxvgMultiRecordsOptionsData then
    with TcxvgMultiRecordsOptionsData(Source) do
    begin
      Self.Appending := Appending;
      Self.Deleting := Deleting;
      Self.DeletingConfirmation := DeletingConfirmation;
      Self.Inserting := Inserting;
      Self.MultiThreadedFiltering := MultiThreadedFiltering;
    end;
end;

procedure TcxvgMultiRecordsOptionsData.Changed;
begin
  VerticalGrid.RefreshNavigators;
end;

procedure TcxvgMultiRecordsOptionsData.SetAppending(Value: Boolean);
begin
  if FAppending <> Value then
  begin
    FAppending := Value;
    Changed;
  end;
end;

procedure TcxvgMultiRecordsOptionsData.SetDeleting(Value: Boolean);
begin
  if FDeleting <> Value then
  begin
    FDeleting := Value;
    Changed;
  end;
end;

procedure TcxvgMultiRecordsOptionsData.SetInserting(Value: Boolean);
begin
  if FInserting <> Value then
  begin
    FInserting := Value;
    Changed;
  end;
end;

function TcxvgMultiRecordsOptionsData.GetMultiThreadedFiltering: TdxDefaultBoolean;
begin
  Result := VerticalGrid.DataController.MultiThreadedOptions.Filtering;
end;

function TcxvgMultiRecordsOptionsData.GetVerticalGrid: TcxVirtualVerticalGrid;
begin
   Result := TcxVirtualVerticalGrid(EditingControl);
end;

procedure TcxvgMultiRecordsOptionsData.SetMultiThreadedFiltering(
  const Value: TdxDefaultBoolean);
begin
  VerticalGrid.DataController.MultiThreadedOptions.Filtering := Value;
end;

{ TcxVerticalGridFilterBox }

function TcxVerticalGridFilterBox.GetCustomizeButtonCaption: string;
begin
  Result := cxGetResourceString(@cxSvgFilterCustomizeButtonCaption);
end;

function TcxVerticalGridFilterBox.GetDefaultText: string;
begin
  Result := cxGetResourceString(@cxSvgFilterIsEmpty);
end;

{ TcxvgDateTimeHandling }

function TcxVerticalGridDateTimeHandling.GetDefaultMonthFormat: string;
begin
  Result := cxGetResourceString(@cxSvgMonthFormat);
end;

function TcxVerticalGridDateTimeHandling.GetDefaultYearFormat: string;
begin
  Result := cxGetResourceString(@cxSvgYearFormat);
end;

{ TcxvgRowFilterPopupOptions }

function TcxVerticalGridFilteringRowPopupOptions.ApplyButtonCaption: string;
begin
  Result := cxGetResourceString(@cxSvgFilterApplyButtonCaption);
end;

{ TcxVerticalGridFiltering }

function TcxVerticalGridFiltering.GetItemExcelPopupClass: TcxControlOptionsFilteringItemExcelPopupClass;
begin
  Result := TcxVerticalGridFilteringRowExcelPopupOptions;
end;

function TcxVerticalGridFiltering.GetItemPopupClass: TcxControlOptionsFilteringItemPopupClass;
begin
  Result := TcxVerticalGridFilteringRowPopupOptions;
end;

function TcxVerticalGridFiltering.GetRowExcelPopup: TcxVerticalGridFilteringRowExcelPopupOptions;
begin
  Result := TcxVerticalGridFilteringRowExcelPopupOptions(ItemExcelPopup);
end;

function TcxVerticalGridFiltering.GetRowPopup: TcxVerticalGridFilteringRowPopupOptions;
begin
  Result := TcxVerticalGridFilteringRowPopupOptions(ItemPopup);
end;

function TcxVerticalGridFiltering.GetRowPopupMode: TdxFilterPopupWindowMode;
begin
  Result := ItemPopupMode;
end;

procedure TcxVerticalGridFiltering.SetRowExcelPopup(AValue: TcxVerticalGridFilteringRowExcelPopupOptions);
begin
  ItemExcelPopup := AValue;
end;

procedure TcxVerticalGridFiltering.SetRowPopup(AValue: TcxVerticalGridFilteringRowPopupOptions);
begin
  ItemPopup := AValue;
end;

procedure TcxVerticalGridFiltering.SetRowPopupMode(AValue: TdxFilterPopupWindowMode);
begin
  ItemPopupMode := AValue;
end;

{ TcxvgMultiRecordsOptionsBehavior }

constructor TcxvgMultiRecordsOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAllowChangeRecord := True;
end;

procedure TcxvgMultiRecordsOptionsBehavior.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxvgMultiRecordsOptionsBehavior then
  begin
    EditingControl.BeginUpdate;
    try
      AllowChangeRecord := TcxvgMultiRecordsOptionsBehavior(Source).AllowChangeRecord;
      RecordScrollMode := TcxvgMultiRecordsOptionsBehavior(Source).RecordScrollMode;
    finally
      EditingControl.EndUpdate;
    end;
  end;
end;

function TcxvgMultiRecordsOptionsBehavior.GetIncSearchItem: TcxCustomEditorRow;
var
  AItem: TcxCellEdit;
begin
  AItem := TcxCellEdit(inherited IncSearchItem);
  if AItem <> nil then
    Result := TcxCustomEditorRow(AItem.Row)
  else
    Result := nil;
end;

procedure TcxvgMultiRecordsOptionsBehavior.SetAllowChangeRecord(Value: Boolean);
begin
  if FAllowChangeRecord <> Value then
  begin
    FAllowChangeRecord := Value;
    EditingControl.LayoutChanged;
  end;
end;

procedure TcxvgMultiRecordsOptionsBehavior.SetIncSearchItem(
  Value: TcxCustomEditorRow);
begin
  if Value <> nil then
    inherited IncSearchItem := TcxCustomEditorRowProperties(Value.FProperties).EditContainer
  else
    inherited IncSearchItem := nil;
end;

procedure TcxvgMultiRecordsOptionsBehavior.SetRecordScrollMode(AValue: TcxRecordScrollMode);
begin
  if FRecordScrollMode <> AValue then
  begin
    FRecordScrollMode := AValue;
    EditingControl.LayoutChanged;
  end;
end;

{ TcxvgOptionsBehavior }

constructor TcxvgOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBandSizing := True;
  FHeaderSizing := True;
  FRowTracking := True;
  CellHints := True;
end;

procedure TcxvgOptionsBehavior.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxvgOptionsBehavior then
    with TcxvgOptionsBehavior(Source) do
    begin
      Self.BandSizing := BandSizing;
      Self.HeaderSizing := HeaderSizing;
      Self.RowFiltering := RowFiltering;
      Self.RowSizing := RowSizing;
      Self.RowTracking := RowTracking;
    end;
end;

procedure TcxvgOptionsBehavior.RestoreDefaults;
begin
  FAlwaysShowEditorAssigned := False;
end;

procedure TcxvgOptionsBehavior.InternalSetAlwaysShowEditor(Value: Boolean);
begin
  inherited AlwaysShowEditor := Value;
end;

function TcxvgOptionsBehavior.GetAlwaysShowEditor: Boolean;
begin
  if FAlwaysShowEditorAssigned then
    Result := inherited AlwaysShowEditor
  else
    Result := TcxCustomVerticalGrid(EditingControl).OptionsView.PaintStyle = psDelphi;
end;

function TcxvgOptionsBehavior.GetRowFiltering: TdxDefaultBoolean;
begin
  Result := ItemFiltering;
end;

procedure TcxvgOptionsBehavior.SetAlwaysShowEditor(Value: Boolean);
begin
  FAlwaysShowEditorAssigned := True;
  InternalSetAlwaysShowEditor(Value);
end;

procedure TcxvgOptionsBehavior.SetRowFiltering(AValue: TdxDefaultBoolean);
begin
  ItemFiltering := AValue;
end;

{ TcxVerticalGridItemsCustomizeListBox }

constructor TcxVerticalGridItemsCustomizeListBox.CreateEx(
  AOwner: TComponent; AVerticalGrid: TcxCustomVerticalGrid; IsCategoryListBox: Boolean);
begin
  inherited Create(AOwner);
  FVerticalGrid := AVerticalGrid;
  FVerticalGrid.ScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
  FIsCategoryListBox := IsCategoryListBox;
  ListStyle := lbOwnerDrawFixed;
  DoubleBuffered := True;
  FDragAndDropItemIndex := -1;
  UpdateItemHeight;
end;

destructor TcxVerticalGridItemsCustomizeListBox.Destroy;
begin
  FVerticalGrid.ScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  inherited Destroy;
end;

procedure TcxVerticalGridItemsCustomizeListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if IsOwnerDragDrop(Source) then
    VerticalGrid.DragDrop(Source, X, Y);
end;

procedure TcxVerticalGridItemsCustomizeListBox.CalcHeaderViewInfo(
  const R: TRect; AHeaderInfo: TcxCustomRowHeaderInfo);
begin
  AHeaderInfo.Calc(R, VerticalGrid.ViewInfo, nil, False);
end;

procedure TcxVerticalGridItemsCustomizeListBox.DragOver(
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsOwnerDragDrop(Source) and DragRow.Options.CanMovedToCustomizationForm;
end;

function TcxVerticalGridItemsCustomizeListBox.DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean;

  procedure InternalDrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect);
  var
    AHeaderInfo: TcxCustomRowHeaderInfo;
    APrevCanvas: TcxCanvas;
    AIsRightToLeftConverted: Boolean;
  begin
    AHeaderInfo := Items.Objects[AIndex] as TcxCustomRowHeaderInfo;
    APrevCanvas := Painter.Canvas;
    try
      Painter.Canvas := ACanvas;
      AIsRightToLeftConverted := AHeaderInfo.IsRightToLeftConverted;
      CalcHeaderViewInfo(ARect, AHeaderInfo);
      if AIsRightToLeftConverted then
        AHeaderInfo.RightToLeftConversion(ARect);
      if ItemIndex = AIndex then
        AHeaderInfo.MakeSelectedViewParams(Focused);
      Painter.DrawHeaderDragImage(AHeaderInfo);
    finally
      Painter.Canvas := APrevCanvas;
    end;
  end;

begin
  Result := inherited DrawItem(ACanvas, AIndex, ARect, AState);
  if not Result then
  begin
    InternalDrawItem(ACanvas, AIndex, ARect);
    Result := True;
  end;
end;

procedure TcxVerticalGridItemsCustomizeListBox.InitDragAndDropObject;
var
  AHeaderInfo: TcxCustomRowHeaderInfo;
  R: TRect;
  AIsRightToLeftConverted: Boolean;
begin
  with VerticalGrid.Controller do
    if IsEditing then EditingController.HideEdit(False);
  AHeaderInfo := TcxCustomRowHeaderInfo(Items.Objects[FDragAndDropItemIndex]);
  R := cxRectOffset(ItemRect(FDragAndDropItemIndex), -FMouseDownPos.X, -FMouseDownPos.Y);
  AIsRightToLeftConverted := AHeaderInfo.IsRightToLeftConverted;
  CalcHeaderViewInfo(R, AHeaderInfo);
  if AIsRightToLeftConverted then
    AHeaderInfo.RightToLeftConversion(R);
  VerticalGrid.Controller.FDragRow := AHeaderInfo.Row;
  VerticalGrid.DragPos := cxPointOffset(R.TopLeft, FOffset);
  VerticalGrid.BeginDrag(True);
end;

function TcxVerticalGridItemsCustomizeListBox.IsOwnerDragDrop(ADragObject: TObject): Boolean;
begin
  Result := (ADragObject is TcxvgDragRowObject) and (TcxvgDragRowObject(ADragObject).Row = DragRow);
end;

procedure TcxVerticalGridItemsCustomizeListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: Integer;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then
  begin
    AItem := ItemAtPos(Point(X, Y), True);
    if AItem <> - 1 then
    begin
      R := ItemRect(AItem);
      FOffset := Point(X - R.TopLeft.X, Y - R.TopLeft.Y);
      FDragging := True;
      FDragAndDropItemIndex := ItemIndex;
      FMouseDownPos := Point(X, Y);
      Invalidate;
    end;
  end;
end;

procedure TcxVerticalGridItemsCustomizeListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (FDragAndDropItemIndex <> -1) and not IsPointInDragDetectArea(FMouseDownPos, X, Y) then
  begin
    InitDragAndDropObject;
    FDragAndDropItemIndex := -1;
  end;
end;

procedure TcxVerticalGridItemsCustomizeListBox.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FDragging := False;
  FDragAndDropItemIndex := -1;
  Invalidate;
  VerticalGrid.Controller.CheckEdit;
end;

procedure TcxVerticalGridItemsCustomizeListBox.RefreshList;

   procedure CheckRow(ARow: TcxCustomRow; AIsCategory: Boolean);
   begin
     if (ARow.IsCategory = AIsCategory) and not ARow.Visible and
       ARow.Options.ShowInCustomizationForm then
         Items.AddObject('', ARow.ViewInfo.HeaderInfo);
   end;

var
  I: Integer;
  ASaveIndex: Integer;
begin
  ASaveIndex := ItemIndex;
  Items.Clear;
  Items.BeginUpdate;
  with FVerticalGrid do
  try
    for I := 0 to Rows.Count - 1 do
      CheckRow(Rows[I], FIsCategoryListBox);
  finally
    if FDragging then
      ItemIndex := ASaveIndex;
    Items.EndUpdate;
  end;
  Invalidate;
end;

procedure TcxVerticalGridItemsCustomizeListBox.UpdateItemHeight;
begin
  ItemHeight := cxTextHeight(VerticalGrid.Font) + VerticalGrid.ScaleFactor.Apply(cxTextOffset) * 2;
end;

procedure TcxVerticalGridItemsCustomizeListBox.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  PostMessage(Handle, DXM_RECALCULATE, 0, 0);
end;

procedure TcxVerticalGridItemsCustomizeListBox.DXMRecalculate(var Message: TWMDrawItem);
begin
  UpdateItemHeight;
end;

procedure TcxVerticalGridItemsCustomizeListBox.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FDragging := False;
  ItemIndex := -1;
  FDragAndDropItemIndex := -1;
  Invalidate;
end;

function TcxVerticalGridItemsCustomizeListBox.GetDragRow: TcxCustomRow;
begin
  Result := VerticalGrid.Controller.DragRow;
end;

function TcxVerticalGridItemsCustomizeListBox.GetPainter: TcxvgPainter;
begin
  Result := VerticalGrid.Painter;
end;

{ TcxVerticalGridCustomizationForm }

constructor TcxVerticalGridCustomizationForm.CreateEx(AOwner: TcxVerticalGridCustomizing);
begin
  CreateNew(nil);
  FOwner := AOwner;
  FHookTimer := TcxTimer.Create(Self);
  FHookTimer.Enabled := False;
  FHookTimer.Interval := 10;
  FHookTimer.OnTimer := HookTimerHandler;
  Visible := False;
end;

destructor TcxVerticalGridCustomizationForm.Destroy;
begin
  Customizing.FinishCustomizing(BoundsRect);
  FreeAndNil(FHookTimer);
  inherited Destroy;
end;

procedure TcxVerticalGridCustomizationForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_POPUP;
    if not VerticalGrid.IsDestroying then
      WndParent := VerticalGrid.Handle
    else
      WndParent := 0;
  end;
end;

procedure TcxVerticalGridCustomizationForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  Action := caFree;
end;

function TcxVerticalGridCustomizationForm.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Customizing.VerticalGrid;
end;

procedure TcxVerticalGridCustomizationForm.HookTimerHandler(Sender: TObject);
begin
  if IsIconic(Application.Handle) then
    Visible := False
  else
    if not IsControlVisible(VerticalGrid) then
      VerticalGrid.Customizing.Visible := False
    else
      if not Visible then
      begin
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
        Visible := True;
      end;
end;

{ TcxVerticalGridCustomizing }

constructor TcxVerticalGridCustomizing.Create(AOwner: TPersistent);
begin
  FVerticalGrid := TcxCustomVerticalGrid(AOwner);
  FRowCount := 10;
  FLastPosition := cxInvisiblePoint;
  FShowCategoryButtons := True;
  FShowCategoryTab := True;
end;

destructor TcxVerticalGridCustomizing.Destroy;
begin
  Visible := False;
  inherited Destroy;
end;

procedure TcxVerticalGridCustomizing.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridCustomizing then
  begin
    RowCount := TcxVerticalGridCustomizing(Source).RowCount;
    Visible := TcxVerticalGridCustomizing(Visible).Visible;
  end;
end;

procedure TcxVerticalGridCustomizing.MakeCategorySheetVisible;
begin
  if Visible and ((FTabSheetCategories <> nil) and (FCategoryListBox.Parent = FTabSheetCategories)) then
    FPageControl.ActivePage := FTabSheetCategories;
end;

procedure TcxVerticalGridCustomizing.MakeRowSheetVisible;
begin
  if Visible and (FRowListBox.Parent = FTabSheetRows) then
    FPageControl.ActivePage := FTabSheetRows;
end;

procedure TcxVerticalGridCustomizing.AdjustControls;
begin
  with VerticalGrid do
  begin
    FPageControl.Parent := FForm;
    FPageControl.BoundsRect := cxTextRect(FForm.ClientRect);
    FPageControl.Anchors := [akTop, akLeft, akRight, akBottom];
    SetControlParent(FTabSheetRows, FPageControl);
    FTabSheetRows.PageControl := FPageControl;
    SetControlParent(FRowListBox, FTabSheetRows);
    if Assigned(FTabSheetCategories) then
    begin
      SetControlParent(FTabSheetCategories, FPageControl);
      FTabSheetCategories.PageControl := FPageControl;
      SetControlParent(FCategoryListBox, FTabSheetCategories);
    end;
  end;
end;

function TcxVerticalGridCustomizing.CanDrop(const P: TPoint): Boolean;

  function CheckListBox(AListBox: TcxVerticalGridItemsCustomizeListBox): Boolean;
  begin
    if AListBox = nil then
      Result := False
    else
      with AListBox do
        Result := Visible and cxRectPtIn(ClientRect, ScreenToClient(P));
  end;

begin
  Result := Visible and (VerticalGrid.Controller.DragRow <> nil);
  if Result then
  begin
    if VerticalGrid.Controller.DragRow.IsCategory then
      Result := CheckListBox(FCategoryListBox)
    else
      Result := CheckListBox(FRowListBox);
  end;
end;

procedure TcxVerticalGridCustomizing.CreateCategoryPanel;

  function CreateButton(ALeft, ATabOrder: Integer; const ACaption: string; AOnClick: TNotifyEvent): TcxButton;
  begin
    Result := TcxButton.Create(FForm);
    Result.Left := ALeft;
    Result.Top := 7;
    Result.Width := 75;
    Result.Height := 25;
    Result.Caption := ACaption;
    Result.TabOrder := ATabOrder;
    Result.LookAndFeel.MasterLookAndFeel := VerticalGrid.LookAndFeel;
    Result.Enabled := ATabOrder = 0;
    Result.OnClick := AOnClick;
    Result.Parent := FPanel;
  end;

begin
  FPanel := TPanel.Create(FForm);
  FPanel.Align := alBottom;
  FPanel.Height := 35;
  FPanel.BevelOuter := bvNone;
  FPanel.Visible := FShowCategoryButtons;
  FPanel.Parent := FTabSheetCategories;
  FButtonNew := CreateButton(7, 0, cxGetResourceString(@cxSvgCustomizeNewCategory), CreateCategoryClick);
  FButtonDelete := CreateButton(90, 1, cxGetResourceString(@cxSvgCustomizeDeleteCategory), DeleteCategoryClick);
end;

procedure TcxVerticalGridCustomizing.CreateCategoryRow;
var
  ARow: TcxCategoryRow;
  S: string;
begin
  if cxShowNewCategoryForm(Form, S, VerticalGrid.LookAndFeel) then
  begin
    VerticalGrid.BeginUpdate;
    try
      ARow := TcxCategoryRow(VerticalGrid.Add(TcxCategoryRow));
      ARow.Visible := False;
      ARow.Properties.Caption := S;
      ARow.Name := CreateUniqueName(VerticalGrid.Owner, VerticalGrid, ARow, 'Tcx', 'RuntimeCreatedRow');
    finally
      Modified;
      VerticalGrid.EndUpdate;
    end;
  end;
end;

procedure TcxVerticalGridCustomizing.CreateControls;
begin
  FRowListBox := TcxVerticalGridItemsCustomizeListBox.CreateEx(FForm, VerticalGrid, False);
  FPageControl := TcxPageControl.Create(FForm);
  FPageControl.Properties.HotTrack := True;
  FPageControl.LookAndFeel.MasterLookAndFeel := VerticalGrid.LookAndFeel;
  FTabSheetRows := TcxTabSheet.Create(FPageControl);
  FTabSheetRows.Caption := cxGetResourceString(@cxSvgCustomizeRowsCaption);
  FTabSheetRows.TabVisible := True;
  if FShowCategoryTab then
  begin
    FCategoryListBox := TcxVerticalGridItemsCustomizeListBox.CreateEx(FForm, VerticalGrid, True);
    FCategoryListBox.OnClick := UpdateButtons;
    FTabSheetCategories := TcxTabSheet.Create(FPageControl);
    FTabSheetCategories.Caption := cxGetResourceString(@cxSvgCustomizeCategoriesCaption);
    FTabSheetCategories.TabVisible := True;
    CreateCategoryPanel;
  end;
  AdjustControls;
end;

procedure TcxVerticalGridCustomizing.FinishCustomizing(const ABounds: TRect);

  function CheckIndex(AListBox: TcxListBox): Integer;
  begin
    if (AListBox = nil) or (AListBox.Parent = nil) then
      Result := -1
    else
      Result := AListBox.ItemIndex;
  end;

begin
  FLastHeaderIndex := CheckIndex(FRowListBox);
  FLastBandIndex := CheckIndex(FCategoryListBox);
  FForm := nil;
  VerticalGrid.SetCustomization(False);
end;

procedure TcxVerticalGridCustomizing.BiDiModeChanged;
begin
  if FForm <> nil then
  begin
    FForm.BiDiMode := VerticalGrid.BiDiMode;
    FRowListBox.BiDiMode := VerticalGrid.BiDiMode;
    FCategoryListBox.BiDiMode := VerticalGrid.BiDiMode;
  end;
end;

procedure TcxVerticalGridCustomizing.CreateCustomizingForm;
var
  I: Integer;
const
  UnusedItems: array[0..4, 0..1] of Integer =
    ((7, MF_BYPOSITION), (5, MF_BYPOSITION), (SC_MAXIMIZE, MF_BYCOMMAND),
    (SC_MINIMIZE, MF_BYCOMMAND), (SC_RESTORE, MF_BYCOMMAND));
begin
  FForm := TcxVerticalGridCustomizationForm.CreateEx(Self);
  FForm.Visible := False;
  FForm.Caption := cxGetResourceString(@cxSvgCustomizeCaption);
  FForm.BorderStyle := bsSizeToolWin;
  FForm.BorderIcons := [biSystemMenu];
  FForm.ScaleForPPI(VerticalGrid.ScaleFactor.TargetDPI);
  FForm.Font.Assign(VerticalGrid.Font);
  FForm.ClientWidth := SizeDelta.cx * cxTextWidth(FForm.Font, '0');
  FLineHeight := cxTextHeight(FForm.Font) + SizeDelta.cy;
  FForm.ClientHeight := (RowCount + 1) * FLineHeight;
  FForm.Constraints.MinWidth := FForm.Width;
  FForm.Color := clBtnFace;
  for I := 0 to High(UnusedItems) do
    DeleteMenu(GetSystemMenu(FForm.Handle, False), UnusedItems[I, 0], UnusedItems[I, 1]);
  CreateControls;
end;

procedure TcxVerticalGridCustomizing.DisplayCustomizingForm;
var
  AParentForm: TForm;
begin
  FForm.Show;
  AParentForm := GetParentForm(VerticalGrid) as TForm;
  if AParentForm.FormStyle = fsStayOnTop then
    SetWindowPos(FForm.Handle, Application.Handle, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
  FForm.HookTimer.Enabled := True;
end;

procedure TcxVerticalGridCustomizing.HideCustomizingForm;
begin
  if Visible then
  begin
    FForm.HookTimer.Enabled := False;
    FLastPosition := FForm.BoundsRect.TopLeft;
    FreeAndNil(FForm);
  end;
end;

procedure TcxVerticalGridCustomizing.ShowCustomizingForm;
var
  P: TPoint;
  R: TRect;
begin
  if Visible then
    Exit;
  if FForm = nil then
    CreateCustomizingForm;
  FForm.BiDiMode := VerticalGrid.BiDiMode;
  if not cxPointIsEqual(CustomizingPos, cxInvisiblePoint) then
  begin
    FForm.Left := CustomizingPos.X;
    FForm.Top := CustomizingPos.Y;
  end
  else
  begin
    R := Screen.DesktopRect;
    if not VerticalGrid.UseRightToLeftAlignment then
      P := VerticalGrid.ClientToScreen(cxPoint(VerticalGrid.Width, 0))
    else
    begin
      P := VerticalGrid.ClientToScreen(cxPoint(0, 0));
      P.X := P.X - FForm.Width;
    end;
    if P.X + FForm.Width > R.Right then
      P.X := R.Right - FForm.Width;
    if P.Y + FForm.Height > R.Bottom then
      P.Y := R.Bottom - FForm.Height;
    if P.X < R.Left then
      P.X := R.Left;
    if P.Y < R.Top then
      P.Y := R.Top;
  end;
  ValidateListBox(FRowListBox, FLastHeaderIndex);
  ValidateListBox(FCategoryListBox, FLastBandIndex);
  FForm.SetBounds(P.X, P.Y, FForm.Width, FForm.Height);
  DisplayCustomizingForm;
  VerticalGrid.SetCustomization(True);
end;

procedure TcxVerticalGridCustomizing.LookAndFeelChanged;
begin
  FCategoryListBox.Invalidate;
  FRowListBox.Invalidate;
  FPageControl.Invalidate;
end;

procedure TcxVerticalGridCustomizing.SetControlParent(AControl, AParent: TWinControl);
begin
  AControl.Parent := AParent;
  if AParent <> nil then
  begin
    AControl.Align := alClient;
    AControl.BiDiMode := AParent.BiDiMode;
    TControlAccess(AControl).ParentFont := True;
    TControlAccess(AControl).ParentColor := True;
  end;
end;

function TcxVerticalGridCustomizing.SizeDelta: TSize;
begin
  Result := cxSize(31, 4);
end;

procedure TcxVerticalGridCustomizing.ComponentRemoved(Sender: TObject);
begin
end;

procedure TcxVerticalGridCustomizing.Modified;
begin
  Update;
end;

procedure TcxVerticalGridCustomizing.ValidateListBox(
  AListBox: TcxVerticalGridItemsCustomizeListBox; AIndex: Integer);
begin
  if (AListBox = nil) or (AListBox.Parent = nil) then Exit;
  AListBox.RefreshList;
  if (AIndex >= 0) and (AIndex < AListBox.Items.Count) then
    AListBox.ItemIndex := AIndex;
end;

procedure TcxVerticalGridCustomizing.Update;
begin
  if (FRowListBox <> nil) and (FRowListBox.Parent <> nil) then
    FRowListBox.RefreshList;
  if (FCategoryListBox <> nil) and (FCategoryListBox.Parent <> nil) then
  begin
    FCategoryListBox.RefreshList;
    UpdateButtons(nil);
  end;
end;

procedure TcxVerticalGridCustomizing.CreateCategoryClick(Sender: TObject);
begin
  CreateCategoryRow;
end;

procedure TcxVerticalGridCustomizing.DeleteCategoryClick(Sender: TObject);
var
  ARowHeader: TcxCustomRowHeaderInfo;
begin
  with CategoryListBox do
    if ItemIndex >= 0 then
    begin
      ARowHeader := TcxCustomRowHeaderInfo(Items.Objects[ItemIndex]);
      Items.Delete(ItemIndex);
      ARowHeader.Row.Free;
    end;
  Modified;
end;

function TcxVerticalGridCustomizing.GetVisible: Boolean;
begin
  Result := (FForm <> nil) and FForm.Visible;
end;

procedure TcxVerticalGridCustomizing.SetRowCount(Value: Integer);
begin
  FRowCount := Max(Value, 2);
end;

procedure TcxVerticalGridCustomizing.SetShowCategoryButtons(const Value: Boolean);
begin
  FShowCategoryButtons := Value;
  if FPanel <> nil then
    FPanel.Visible := Value;
end;

procedure TcxVerticalGridCustomizing.SetVisible(Value: Boolean);

  function CanVisible: Boolean;
  begin
    Result := VerticalGrid.Rows.Count > 0;
  end;

begin
  if Value and not CanVisible then
    Exit;
  if Value then
    ShowCustomizingForm
  else
    HideCustomizingForm;
end;

procedure TcxVerticalGridCustomizing.UpdateButtons(Sender: TObject);
begin
  FButtonDelete.Enabled := FCategoryListBox.ItemIndex <> -1;
end;

initialization
  RegisterClasses([TcxCategoryRow, TcxEditorRow, TcxMultiEditorRow]);
  RegisterClasses([TcxCaptionRowProperties, TcxEditorRowProperties, TcxMultiEditorRowProperties]);

  Screen.Cursors[crcxInspectorInsert] := LoadCursor(HInstance, cxInspectorInsertCursor);
  Screen.Cursors[crcxInspectorAddChild] := LoadCursor(HInstance, cxInspectorAddChildCursor);
  Screen.Cursors[crcxInspectorAdd] := LoadCursor(HInstance, cxInspectorAddCursor);
  Screen.Cursors[crcxInspectorHide] := LoadCursor(HInstance, cxInspectorHideCursor);
  Screen.Cursors[crcxInspectorNoDrag] := LoadCursor(HInstance, cxInspectorNoDragCursor);

finalization
  DestroyCursor(Screen.Cursors[crcxInspectorInsert]);
  DestroyCursor(Screen.Cursors[crcxInspectorAddChild]);
  DestroyCursor(Screen.Cursors[crcxInspectorAdd]);
  DestroyCursor(Screen.Cursors[crcxInspectorHide]);
  DestroyCursor(Screen.Cursors[crcxInspectorNoDrag]);
end.

