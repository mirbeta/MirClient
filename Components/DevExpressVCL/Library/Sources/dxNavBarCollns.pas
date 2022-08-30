{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxNavBarCollns;

{$I cxVer.inc}

interface

uses
  Windows, Graphics, Controls, ExtCtrls, ActnList, Messages, RTLConsts,
  Generics.Defaults, Generics.Collections, Classes, IniFiles,
  dxNavBarBase, dxNavBarStyles, cxGeometry,
  dxCore, dxCoreClasses, cxAccessibility, cxControls, cxClasses, Themes, cxGraphics;

type
  TdxNavBarItem = class;
  TdxNavBarGroup = class;

  TdxNavBarItemLink = class(TCollectionItem,
    IUnknown,
    IdxAdornerTargetElement)
  private
    FData: TdxNativeInt;
    FIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FItem: TdxNavBarItem;
    FLoadedPosition: Integer;
    function GetGroup: TdxNavBarGroup;
    function GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    procedure SetPosition(Value: Integer);
    procedure SetSelected(Value: Boolean);
    function GetPosition: Integer;

    procedure ReadPosition(Reader: TReader);
    procedure WritePosition(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure InitiateAction;
    function IsAncestorEqual(AItemLink: TdxNavBarItemLink): Boolean;
    procedure SetIndex(Value: Integer); override;

    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;

    // IcxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function CanSelect: Boolean;
    function IsSelected: Boolean;
    property Data: TdxNativeInt read FData write FData;
    property Group: TdxNavBarGroup read GetGroup;
    property IAccessibilityHelper: IdxNavBarAccessibilityHelper read
      GetIAccessibilityHelper;
    property Position: Integer read GetPosition write SetPosition;
    property Selected: Boolean read IsSelected write SetSelected;
  published
    property Item: TdxNavBarItem read FItem write FItem;
  end;

  TdxNavBarItemLinks = class(TCollection)
  private
    FGroup: TdxNavBarGroup;
  protected
    function GetOwner: TPersistent; override;
    function IsAncestorEqual(AItemLinks: TdxNavBarItemLinks): Boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function GetItem(Index: Integer): TdxNavBarItemLink;
    procedure SetItem(Index: Integer; Value: TdxNavBarItemLink);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGroup: TdxNavBarGroup);
    function Add: TdxNavBarItemLink;

    property Group: TdxNavBarGroup read FGroup;
    property Items[Index: Integer]: TdxNavBarItemLink read GetItem write SetItem; default;
  end;

  TdxNavBarGroupControlState = (gcsOverSizeGrip);
  TdxNavBarGroupControlStates = set of TdxNavBarGroupControlState;

  TdxNavBarGroupControl = class(TCustomPanel,
    IdxAdornerRootTargetElement,
    IdxAdornerTargetElement)
  private
    FInternalState: TdxNavBarGroupControlStates;
    FGroup: TdxNavBarGroup;
    FGroupIndex: Integer;
    FOriginalCursor: TCursor;
    FOriginalHeight: Integer;
    FMinHeight: Integer;
    FUpdateLock: Integer;
    FUseStyle: Boolean;

    function GetInternalNavBar: TControl;
    procedure SetOriginalHeight(const Value: Integer);
    procedure SetUseStyle(const Value: Boolean);
    function SizeGripRect: TRect;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    function GetMouseOverSizeGrip: Boolean;
    procedure SetMouseOverSizeGrip(AValue: Boolean);

    property MouseOverSizeGrip: Boolean read GetMouseOverSizeGrip write SetMouseOverSizeGrip;
  protected
  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ReadOriginalHeight(Reader: TReader);
    procedure ReadGroupIndex(Reader: TReader);
    procedure RequestAlign; override;
    procedure WriteOriginalHeight(Writer: TWriter);
    procedure WriteGroupIndex(Writer: TWriter);
    procedure ReadState(Reader: TReader); override;
    procedure SetParent(AParent: TWinControl); override;

    procedure WndProc(var Message: TMessage); override;

    procedure Paint; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateGroup;

    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AdjustControl(ARect: TRect; AVisible: Boolean); virtual;
    procedure BeginResize(AControl: TControl; AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
      MaxHeight: Integer); override;
    procedure DrawSizeGrip(ACanvas: TCanvas; const ARect: TRect);
    function GetMinHeight: Integer;
    function GetSizeGripRect(AControl: TControl): TRect;
    function IsOnPopupControl: Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property OriginalHeight: Integer read FOriginalHeight write SetOriginalHeight;

    property Group: TdxNavBarGroup read FGroup write FGroup;
  published
    property Alignment;
    property BiDiMode;
    property Caption;
    property Color;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseStyle: Boolean read FUseStyle write SetUseStyle default False;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TdxNavBarCustomItem = class(TdxNavBarComponentCollectionItem)
  private
    FCaption: string;
    FHint: string;
    FLargeImageIndex: TcxImageIndex;
    FSmallImageIndex: TcxImageIndex;

    FOnClick: TNotifyEvent;

    procedure SetCaption(const Value: string);
    procedure SetHint(const Value: string);
    procedure SetLargeImageIndex(const Value: TcxImageIndex);
    procedure SetSmallImageIndex(const Value: TcxImageIndex);
    procedure SetVisible(const Value: Boolean);
  protected
    FVisible: Boolean;

    procedure DoSetVisible(Value: Boolean); virtual;

    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    property LargeImageIndex: TcxImageIndex read FLargeImageIndex write SetLargeImageIndex default -1;
    property SmallImageIndex: TcxImageIndex read FSmallImageIndex write SetSmallImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxNavBarGroupExpandOptions }

  TdxNavBarGroupExpansionOptions = class(TcxOwnedPersistent)
  private
    FAllowMultipleGroupExpansion: Boolean;
    FExpandable: Boolean;
    FExpanded: Boolean;
    FShowExpandButton: Boolean;

    function GetGroup: TdxNavBarGroup;
    procedure SetAllowMultipleGroupExpansion(const Value: Boolean);
    procedure SetExpanded(AValue: Boolean);
    procedure SetShowExpandButton(AValue: Boolean);

    property Group: TdxNavBarGroup read GetGroup;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AllowMultipleGroupExpansion: Boolean read FAllowMultipleGroupExpansion write SetAllowMultipleGroupExpansion default True;
    property Expandable: Boolean read FExpandable write FExpandable default True;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ShowExpandButton: Boolean read FShowExpandButton write SetShowExpandButton default True;
  end;

  { TdxNavBarGroupControlOptions }

  TdxNavBarGroupControlOptions = class(TcxOwnedPersistent)
  private
    FAllowControlResizing: Boolean;
    FUseControl: Boolean;
    FShowControl: Boolean;

    function GetGroup: TdxNavBarGroup;
    procedure SetAllowControlResizing(AValue: Boolean);
    procedure SetShowControl(AValue: Boolean);
    procedure SetUseControl(AValue: Boolean);

    property Group: TdxNavBarGroup read GetGroup;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AllowControlResizing: Boolean read FAllowControlResizing write SetAllowControlResizing default False;
    property ShowControl: Boolean read FShowControl write SetShowControl default False;
    property UseControl: Boolean read FUseControl write SetUseControl default False;
  end;

  { TdxNavBarGroup }

  TdxNavBarGroup = class(TdxNavBarCustomItem,
    IdxNavigationItem,
    IdxAdornerTargetElement)
  private
    FAlign: TcxTopBottom;
    FCaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FChildren: TdxFastObjectList;
    FControl: TdxNavBarGroupControl;
    FCustomStyles: TdxNavBarGroupCustomStyles;
    FIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FIsSettingTopVisibleIndex: Boolean;
    FLinkContainerIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FLinks: TdxNavBarItemLinks;
    FLinksUseSmallImages: Boolean;
    FLoadedSelectedLinkIndex: Integer;
    FOptionsGroupControl: TdxNavBarGroupControlOptions;
    FOptionsExpansion: TdxNavBarGroupExpansionOptions;
    FParent: TdxNavBarGroup;
    FLoadedParentGroupIndex: Integer;
    FLoadedPosition: Integer;
    FSelectedLink: TdxNavBarItemLink;
    FShowAsIconView: Boolean;
    FShowCaption: Boolean;
    FTopVisibleLinkIndex: Integer;
    FUseRestSpace: Boolean;
    FUseSmallImages: Boolean;
    FVisibleForCustomization: Boolean;

    FOnCollapsed: TNotifyEvent;
    FOnExpanded: TNotifyEvent;
    FOnSelectedLinkChanged: TNotifyEvent;
    FOnTopVisibleLinkChanged: TNotifyEvent;

    function GetCaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetChildCount: Integer;
    function GetChildObjects(Index: Integer): TObject;
    function GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetLevel: Integer;
    function GetLinkContainerIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetLinkCount: Integer;
    function GetLink(Index: Integer): TdxNavBarItemLink;
    function GetSelectedLinkIndex: Integer;
    procedure SetAlign(const Value: TcxTopBottom);
    procedure SetLinksUseSmallImages(const Value: Boolean);
    procedure SetPosition(const Value: Integer);
    procedure SetSelectedLinkIndex(Value: Integer);
    procedure SetShowAsIconView(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetTopVisibleLinkIndex(AValue: Integer);
    procedure SetUseRestSpace(Value: Boolean);
    procedure SetUseSmallImages(const Value: Boolean);

    // OptionsGroupControl
    function GetShowControl: Boolean;
    function GetUseControl: Boolean;
    procedure SetOptionsGroupControl(const Value: TdxNavBarGroupControlOptions);
    procedure SetShowControl(const Value: Boolean);
    procedure SetUseControl(const Value: Boolean);

    // OptionsExpansion
    function GetExpandable: Boolean;
    function GetExpanded: Boolean;
    function GetShowExpandButton: Boolean;
    procedure SetOptionsExpansion(const Value: TdxNavBarGroupExpansionOptions);
    procedure SetExpandable(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetShowExpandButton(const Value: Boolean);

    // Styles
    function GetCustomStyle(Index: Integer): TdxNavBarStyleItem;
    procedure SetCustomStyles(Value: TdxNavBarGroupCustomStyles);
    procedure SetCustomStyle(Index: Integer; const Value: TdxNavBarStyleItem);
    function GetPosition: Integer;

    function AllowChildGroups: Boolean;
    function CompareChildren(AItem1, AItem2: Pointer): Integer;
    procedure DoSetTopVisibleLinkIndex(AValue: Integer; AIsCorrection: Boolean);
    function GetParentGroupIndex: Integer;
  protected
    procedure CorrectTopVisibleIndex(AValue: Integer);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyAccessibilityHelpers;
    procedure DoSetVisible(Value: Boolean); override;
    function GetAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass; virtual;
    function GetCaptionPanelAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass; virtual;
    function GetLinkContainerAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass; virtual;
    procedure InitiateActions; override;
    procedure Loaded; override;
    procedure SetIndex(AValue: Integer); override;
    procedure SortChildren;

    procedure ReadLinks(Reader: TReader);
    procedure ReadParentGroupIndex(Reader: TReader);
    procedure ReadPosition(Reader: TReader);
    procedure WriteLinks(Writer: TWriter);
    procedure WriteParentGroupIndex(Writer: TWriter);
    procedure WritePosition(Writer: TWriter);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;

    procedure AllowMultipleGroupExpansionChanged;
    procedure StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);

    procedure DoExpand;
    procedure DoExpanded; dynamic;
    procedure DoCollapse;
    procedure DoCollapsed; dynamic;
    procedure DoSelectedLinkChanged; dynamic;
    procedure DoTopVisibleLinkChanged; dynamic;

    procedure CreateControl; virtual;
    procedure DestroyControl; virtual;

    procedure LinksChanged(ALink: TdxNavBarItemLink);

    // IdxNavigationItem
    function IdxNavigationItem.GetID = GetNavigationItemID;
    function IdxNavigationItem.GetText = GetNavigationItemText;
    function IdxNavigationItem.GetImageIndex = GetNavigationItemImageIndex;
    function GetNavigationItemID: Integer;
    function GetNavigationItemText: string;
    function GetNavigationItemImageIndex: TcxImageIndex;

    // IcxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    procedure GetAdornerTargetElements(AList: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;

    procedure AcceptControl(AControl: TdxNavBarGroupControl); virtual;
    procedure ReleaseControl; virtual;

    function CheckSiblingGroups: Boolean;
    procedure ClearLinks;
    function CreateLink(AItem: TdxNavBarItem): TdxNavBarItemLink;
    function FindLink(AItem: TdxNavBarItem): TdxNavBarItemLink;
    function InsertLink(AItem: TdxNavBarItem; APosition: Integer): TdxNavBarItemLink;
    function HasAsChild(AGroup: TdxNavBarGroup; AIsRecursive: Boolean = True): Boolean;
    procedure MoveLink(ALink: TdxNavBarItemLink; APosition: Integer);
    procedure RemoveLink(AIndex: Integer); overload;
    procedure RemoveLink(ALink: TdxNavBarItemLink); overload;
    procedure RemoveLinks(AItem: TdxNavBarItem);
    procedure MoveTo(AParent: TdxNavBarGroup; APosition: Integer);

    procedure LoadFromIniFile(AStorage: TCustomIniFile; const ASection: string; ALoadStyles: Boolean);
    procedure SaveToIniFile(AStorage: TCustomIniFile; const ASection: string; ASaveStyles: Boolean);

    property CaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper read GetCaptionPanelIAccessibilityHelper;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TObject read GetChildObjects;
    property Control: TdxNavBarGroupControl read FControl;
    property IAccessibilityHelper: IdxNavBarAccessibilityHelper read GetIAccessibilityHelper;
    property Level: Integer read GetLevel;
    property LinkContainerIAccessibilityHelper: IdxNavBarAccessibilityHelper read GetLinkContainerIAccessibilityHelper;
    property LinkCount: Integer read GetLinkCount;
    property Links[Index: Integer]: TdxNavBarItemLink read GetLink;
    property Parent: TdxNavBarGroup read FParent;
    property Position: Integer read GetPosition write SetPosition;
    property SelectedLink: TdxNavBarItemLink read FSelectedLink;
  published
    property Align: TcxTopBottom read FAlign write SetAlign default vaTop;
    property Caption;
    property Hint;
    property LargeImageIndex;
    property LinksUseSmallImages: Boolean read FLinksUseSmallImages write SetLinksUseSmallImages default True;
    property SelectedLinkIndex: Integer read GetSelectedLinkIndex write SetSelectedLinkIndex;
    property ShowAsIconView: Boolean read FShowAsIconView write SetShowAsIconView default False;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property SmallImageIndex;
    property TopVisibleLinkIndex: Integer read FTopVisibleLinkIndex write SetTopVisibleLinkIndex;
    property UseRestSpace: Boolean read FUseRestSpace write SetUseRestSpace default False;
    property UseSmallImages: Boolean read FUseSmallImages write SetUseSmallImages default True;
    property Visible;
    property VisibleForCustomization: Boolean read FVisibleForCustomization write FVisibleForCustomization default True;

    // OptionsGroupControl
    property OptionsGroupControl: TdxNavBarGroupControlOptions read FOptionsGroupControl write SetOptionsGroupControl;
    property ShowControl: Boolean read GetShowControl write SetShowControl stored False;
    property UseControl: Boolean read GetUseControl write SetUseControl stored False;

    // OptionsExpand
    property OptionsExpansion: TdxNavBarGroupExpansionOptions read FOptionsExpansion write SetOptionsExpansion;
    property Expandable: Boolean read GetExpandable write SetExpandable stored False;
    property Expanded: Boolean read GetExpanded write SetExpanded stored False;
    property ShowExpandButton: Boolean read GetShowExpandButton write SetShowExpandButton stored False;

    // Styles
    property CustomStyles: TdxNavBarGroupCustomStyles read FCustomStyles write SetCustomStyles;
    property StyleBackground: TdxNavBarStyleItem index 0 read GetCustomStyle write SetCustomStyle stored False;
    property StyleControl: TdxNavBarStyleItem index 1 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeader: TdxNavBarStyleItem index 2 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeaderActive: TdxNavBarStyleItem index 3 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeaderActiveHotTracked: TdxNavBarStyleItem index 4 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeaderActivePressed: TdxNavBarStyleItem index 5 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeaderHotTracked: TdxNavBarStyleItem index 6 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHeaderPressed: TdxNavBarStyleItem index 7 read GetCustomStyle write SetCustomStyle stored False;

    // Events
    property OnClick;
    property OnCollapsed: TNotifyEvent read FOnCollapsed write FOnCollapsed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnSelectedLinkChanged: TNotifyEvent read FOnSelectedLinkChanged write FOnSelectedLinkChanged;
    property OnTopVisibleLinkChanged: TNotifyEvent read FOnTopVisibleLinkChanged write FOnTopVisibleLinkChanged;
  end;

  TdxNavBarGroupClass = class of TdxNavBarGroup;

  TdxNavBarLinksChangeEvent = procedure (Sender: TObject; ALink: TdxNavBarItemLink) of object;

  TdxNavBarCustomItems = class(TdxNavBarComponentCollection)
  protected
    function GetItem(Index: Integer): TdxNavBarCustomItem;
    procedure SetItem(Index: Integer; Value: TdxNavBarCustomItem);
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    function Add: TdxNavBarCustomItem; overload;
    function ItemByCaption(const ACaption: string): TdxNavBarCustomItem;
    property Items[Index: Integer]: TdxNavBarCustomItem read GetItem write SetItem;
  end;

  TdxNavBarGroups = class(TdxNavBarCustomItems, IUnknown, IEnumerable<IdxNavigationItem>, IEnumerable)
  private
    type
      TGroupPosComparer = class(TInterfacedObject, IComparer<TdxNavBarGroup>)
      private
        FOwner: TdxNavBarGroups;
      public
        constructor Create(AOwner: TdxNavBarGroups);
        function Compare(const Left, Right: TdxNavBarGroup): Integer;
      end;
      TNavigationItemEnumerator = class(TInterfacedObject, IEnumerator<IdxNavigationItem>, IEnumerator)
      private
        FIndex: Integer;
        FList: TList<TdxNavBarGroup>;
      public
        constructor Create(AList: TList<TdxNavBarGroup>);
        function GetCurrent: TObject;
        function IEnumerator<IdxNavigationItem>.GetCurrent = GetCurrentNavigationItem;
        function GetCurrentNavigationItem: IdxNavigationItem;
        function MoveNext: Boolean;
        procedure Reset;
      end;
  private
    FGroupPosComparer: TGroupPosComparer;
    FJustExpandedGroup: TdxNavBarGroup;
    FNeedGroupsReposition: Boolean;
    FRootGroups: TList<TdxNavBarGroup>;
    FOnLinksChange: TdxNavBarLinksChangeEvent;
    function AllowChildGroups: Boolean;
    procedure AssignGroupPositions;
    procedure DoSort;
    function GetRootGroupCount: Integer;
    function GetRootGroups(Index: Integer): TdxNavBarGroup;
    procedure ResetGroupPositions;
    procedure UpdateGroupPositions;
  protected
    function GetItem(Index: Integer): TdxNavBarGroup;
    procedure SetItem(Index: Integer; Value: TdxNavBarGroup);
    procedure DoLinksChanged(ALink: TdxNavBarItemLink); dynamic;
    procedure ExpandStateChanged(AGroup: TdxNavBarGroup);
    procedure Notify(AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); override;

    procedure SetGroupOrder(AOrder: TList<TdxNavBarGroup>); // for internal use
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IEnumerable
    function GetEnumerator: IEnumerator;
    // IEnumerable<IdxNavigationItem>
    function IEnumerable<IdxNavigationItem>.GetEnumerator = GetNavigationItemEnumerator;
    function GetNavigationItemEnumerator: IEnumerator<IdxNavigationItem>;
  public
    constructor Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass); override;
    destructor Destroy; override;
    function Add: TdxNavBarGroup;
    procedure AllowMultipleGroupExpansionChanged;
    procedure CollapseSiblings(AGroup: TdxNavBarGroup);
    procedure InitiateActions; override;
    procedure LoadFromIniFile(AStorage: TCustomIniFile; ALoadStyles: Boolean);
    procedure Loaded;
    procedure Updated;
    procedure Updating;
    procedure SaveToIniFile(AStorage: TCustomIniFile; ASaveStyles: Boolean);
    procedure StructureChanged;
    property Items[Index: Integer]: TdxNavBarGroup read GetItem write SetItem; default;
    property JustExpandedGroup: TdxNavBarGroup read FJustExpandedGroup write FJustExpandedGroup;
    property RootGroupCount: Integer read GetRootGroupCount;
    property RootGroups[Index: Integer]: TdxNavBarGroup read GetRootGroups;
    property OnLinksChange: TdxNavBarLinksChangeEvent read FOnLinksChange write FOnLinksChange;
  end;

  TdxNavBarItemActionLink = class(TActionLink)
  private
    FClient: TdxNavBarItem;
  protected
    procedure AssignClient(AClient: TObject); override;

    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;

    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
  end;

  { TdxNavBarItemCalculator }

  TdxNavBarCustomGroupItemCalculatorClass = class of TdxNavBarCustomGroupItemCalculator;
  TdxNavBarCustomGroupItemCalculator = class
  public
    class procedure CalculateBounds(X, Y: Integer; AScaleFactor: TdxScaleFactor; var ALinkViewInfo); virtual;
  end;

  { TdxNavBarItem }

  TdxNavBarItem = class(TdxNavBarCustomItem)
  private
    FActionLink: TdxNavBarItemActionLink;
    FEnabled: Boolean;
    FCustomStyles: TdxNavBarItemCustomStyles;

    function GetAction: TBasicAction;
    procedure SetAction(Value: TBasicAction);
    procedure SetEnabled(const Value: Boolean);

    // Styles
    function GetCustomStyle(Index: Integer): TdxNavBarStyleItem;
    procedure SetCustomStyles(Value: TdxNavBarItemCustomStyles);
    procedure SetCustomStyle(Index: Integer; const Value: TdxNavBarStyleItem);

    procedure DoActionChange(Sender: TObject);
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    function CanSelect: Boolean; virtual;
    class function GetCalculatorClass: TdxNavBarCustomGroupItemCalculatorClass; virtual;
  protected
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Draw(const APainter, ALinkViewInfo); virtual;
    procedure LoadFromIniFile(AStorage: TCustomIniFile; const ASection: string; ALoadStyles: Boolean);
    procedure RemoveAllLinks;

    property ActionLink: TdxNavBarItemActionLink read FActionLink;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored;
    property Hint stored IsHintStored;
    property LargeImageIndex stored IsImageIndexStored;
    property SmallImageIndex stored IsImageIndexStored;
    property Visible stored IsVisibleStored;

    // Styles
    property CustomStyles: TdxNavBarItemCustomStyles read FCustomStyles write SetCustomStyles;
    property Style: TdxNavBarStyleItem index 0 read GetCustomStyle write SetCustomStyle stored False;
    property StyleDisabled: TdxNavBarStyleItem index 1 read GetCustomStyle write SetCustomStyle stored False;
    property StyleHotTracked: TdxNavBarStyleItem index 2 read GetCustomStyle write SetCustomStyle stored False;
    property StylePressed: TdxNavBarStyleItem index 3 read GetCustomStyle write SetCustomStyle stored False;

    property OnClick;
  end;

  TdxNavBarItemClass = class of TdxNavBarItem;

  TdxNavBarItems = class(TdxNavBarCustomItems)
  protected
    function GetItem(Index: Integer): TdxNavBarItem;
    procedure SetItem(Index: Integer; Value: TdxNavBarItem);
    procedure Notify(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
  public
    function Add: TdxNavBarItem; overload;
    function Add(AItemClass: TcxComponentCollectionItemClass{TdxNavBarItemClass}): TdxNavBarItem; overload;
    procedure LoadFromIniFile(AStorage: TCustomIniFile; ALoadStyles: Boolean);
    property Items[Index: Integer]: TdxNavBarItem read GetItem write SetItem; default;
  end;

implementation

uses
  SysUtils, Math, Types, Forms,
  dxNavBarConsts, dxNavBarAccessibility, dxNavBarOffice11Views, dxNavBar;

type
  TdxNavBarPainterAccess = class(TdxNavBarPainter);
  TdxCustomNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarPopupControlAccess = class(TdxNavBarPopupControl);

function GetNavBar(AComponentCollectionItem: TcxComponentCollectionItem): TdxCustomNavBar;
begin
  Result := AComponentCollectionItem.GetParentComponent as TdxCustomNavBar;
end;

{ TdxNavBarItemLink }

constructor TdxNavBarItemLink.Create(Collection: TCollection);
begin
  inherited;
  if Group <> nil then Group.LinksChanged(Self);
end;

destructor TdxNavBarItemLink.Destroy;
begin
  if (Group <> nil) and GetNavBar(Group).IsDesigning then
  begin
    GetNavBar(Group).Painter.InvalidateViewInfo(doRecreate);
    SetCollection(nil);
  end;
  Selected := False;
  NavBarAccessibleObjectOwnerObjectDestroyed(FIAccessibilityHelper);
  FItem := nil;
  if (Group <> nil) and not Group.IsDestroying then
    Group.LinksChanged(Self);
  inherited Destroy;
end;

procedure TdxNavBarItemLink.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarItemLink then
  begin
    Data := TdxNavBarItemLink(Source).Data;
    Item := TdxNavBarItemLink(Source).Item;
  end
  else
    inherited;
end;

function TdxNavBarItemLink.CanSelect: Boolean;
begin
  Result := (Item <> nil) and Item.CanSelect;
end;

function TdxNavBarItemLink.IsSelected: Boolean;
begin
  Result := (Group <> nil) and (Group.SelectedLink = Self);
end;

procedure TdxNavBarItemLink.DefineProperties(Filer: TFiler);

  function NeedWritePosition: Boolean;
  begin
    Result := Group.AllowChildGroups;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Position', ReadPosition, WritePosition, NeedWritePosition);
end;

procedure TdxNavBarItemLink.InitiateAction;
begin
  if (Item <> nil) and (Item.ActionLink <> nil) then Item.ActionLink.Update;
end;

function TdxNavBarItemLink.IsAncestorEqual(AItemLink: TdxNavBarItemLink): Boolean;
begin
  Result := (AItemLink = Self) or (AItemLink <> nil) and (
    (Item = nil) and (AItemLink.Item = nil) or
    (Item <> nil) and (AItemLink.Item <> nil) and (Item.Name = AItemLink.Item.Name)
  );
end;

procedure TdxNavBarItemLink.SetIndex(Value: Integer);
begin
  Collection.BeginUpdate;
  try
    inherited SetIndex(Value);
    if not Group.AllowChildGroups then
      Position := Index;
  finally
    Collection.EndUpdate;
  end;
end;

function TdxNavBarItemLink._AddRef: Integer;
begin
  Result := -1;
end;

function TdxNavBarItemLink._Release: Integer;
begin
  Result := -1;
end;

function TdxNavBarItemLink.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxNavBarItemLink.GetAdornerTargetElementBounds: TRect;
var
  AViewInfo: TdxNavBarLinkViewInfo;
begin
  AViewInfo := GetNavBar(Item).ViewInfo.GetLinkViewInfoByLink(Self);
  if AViewInfo <> nil then
    Result := AViewInfo.Rect
  else
    Result := cxNullRect;
end;

function TdxNavBarItemLink.GetAdornerTargetElementControl: TWinControl;
begin
  Result := GetNavBar(Item);
end;

function TdxNavBarItemLink.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Item.Visible and (GetNavBar(Item).ViewInfo.GetLinkViewInfoByLink(Self) <> nil);
end;

function TdxNavBarItemLink.GetGroup: TdxNavBarGroup;
begin
  if Collection is TdxNavBarItemLinks then
    Result := (Collection as TdxNavBarItemLinks).Group
  else
    Result := nil;
end;

function TdxNavBarItemLink.GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := NavBarGetAccessibilityHelper(
      TdxNavBarItemLinkAccessibilityHelper.Create(Self,
        Group.Collection.ParentComponent as TdxCustomNavBar));
  Result := FIAccessibilityHelper;
end;

function TdxNavBarItemLink.GetPosition: Integer;
begin
  Result := Group.FChildren.IndexOf(Self);
end;

procedure TdxNavBarItemLink.ReadPosition(Reader: TReader);
begin
  FLoadedPosition := Reader.ReadInteger;
end;

procedure TdxNavBarItemLink.WritePosition(Writer: TWriter);
begin
  Writer.WriteInteger(Position);
end;

procedure TdxNavBarItemLink.SetPosition(Value: Integer);
begin
  Group.MoveLink(Self, Value);
end;

procedure TdxNavBarItemLink.SetSelected(Value: Boolean);
begin
  if (Selected <> Value) and (Group <> nil) and not Group.IsDestroying then
    if Value then
      Group.SelectedLinkIndex := Index
    else
      Group.SelectedLinkIndex := -1;
end;

{ TdxNavBarItemLinks }

constructor TdxNavBarItemLinks.Create(AGroup: TdxNavBarGroup);
begin
  FGroup := AGroup;
  inherited Create(TdxNavBarItemLink);
end;

function TdxNavBarItemLinks.Add: TdxNavBarItemLink;
begin
  Result := inherited Add as TdxNavBarItemLink;
end;

function TdxNavBarItemLinks.GetOwner: TPersistent;
begin
  Result := Group;
end;

function TdxNavBarItemLinks.IsAncestorEqual(AItemLinks: TdxNavBarItemLinks): Boolean;
var
  I: Integer;
begin
  Result := Self = AItemLinks;
  if Result then
    Exit;
  Result := (AItemLinks <> nil) and (Count = AItemLinks.Count);
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsAncestorEqual(AItemLinks[I]);
      if not Result then
        Break;
    end;
end;

procedure TdxNavBarItemLinks.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded: Group.FChildren.Add(Item);
    cnExtracting: Group.FChildren.Remove(Item);
  end;
end;

function TdxNavBarItemLinks.GetItem(Index: Integer): TdxNavBarItemLink;
begin
  Result := inherited GetItem(Index) as TdxNavBarItemLink;
end;

procedure TdxNavBarItemLinks.SetItem(Index: Integer; Value: TdxNavBarItemLink);
begin
  inherited SetItem(Index, Value);
end;

procedure TdxNavBarItemLinks.Update(Item: TCollectionItem);
begin
  Group.Changed(Item = nil);
end;

{ TdxNavBarGroupControl }

constructor TdxNavBarGroupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGroupIndex := -1;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Color := clBtnFace;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  DoubleBuffered := True;
  ParentBackground := False;
  UseDockManager := False;
  FOriginalCursor := Cursor;
end;

destructor TdxNavBarGroupControl.Destroy;
begin
  FGroup := nil;
  inherited Destroy;
end;

procedure TdxNavBarGroupControl.AdjustControl(ARect: TRect; AVisible: Boolean);
var
  AOriginalHeight: Integer;
begin
  BeginUpdate;
  try
    AOriginalHeight := FOriginalHeight;
    Visible := AVisible;
    BoundsRect := ARect;
    FOriginalHeight := AOriginalHeight;
  finally
    EndUpdate;
  end;
end;

procedure TdxNavBarGroupControl.BeginResize(AControl: TControl; AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
begin
  if IsOnPopupControl then
    TdxNavBarPopupControlAccess(GetParentForm(Self)).BeginResize(AControl, AButton, AShift, APoint);
end;

procedure TdxNavBarGroupControl.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: Integer);
begin
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
  FMinHeight := MinHeight;
end;

procedure TdxNavBarGroupControl.DrawSizeGrip(ACanvas: TCanvas; const ARect: TRect);
begin
  if GetInternalNavBar <> nil then
    TdxNavBarOffice11NavPanePainter(TdxCustomNavBar(GetInternalNavBar).Painter).DrawSizeGrip(ACanvas, ARect);
end;

function TdxNavBarGroupControl.GetMinHeight: Integer;
begin
  Result := FMinHeight;
end;

function TdxNavBarGroupControl.GetSizeGripRect(AControl: TControl): TRect;
begin
  Result := cxNullRect;
  if GetInternalNavBar <> nil then
  begin
    Result := cxRectOffset(TdxNavBarOffice11NavPaneViewInfo(TdxCustomNavBar(GetInternalNavBar).Painter.ViewInfo).SizeGripRect,
      cxPointInvert(cxClientToParent(AControl, Result.TopLeft, TdxCustomNavBar(GetInternalNavBar))));
  end;
end;

function TdxNavBarGroupControl.IsOnPopupControl: Boolean;
var
  AParentForm: TCustomForm;
begin
  AParentForm := GetParentForm(Self);
  Result :=  AParentForm  is TdxNavBarPopupControl;
end;

procedure TdxNavBarGroupControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FOriginalHeight := AHeight;
  if (Visible or (FUpdateLock > 0) and HandleAllocated) and
    ((ALeft <> Left) or (ATop <> Top) or
    (AWidth <> Width) or (AHeight <> Height)) then
  begin
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    UpdateGroup;
  end;
end;

{$IFDEF DELPHIBERLIN}
procedure TdxNavBarGroupControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TdxNavBarGroupControl.ChangeScale(M, D: Integer);
{$ENDIF}
var
  AOriginalHeight: Integer;
begin
  if not (csLoading in ComponentState) or (sfHeight in ScalingFlags) then
    AOriginalHeight := MulDiv(FOriginalHeight, M, D)
  else
    AOriginalHeight := FOriginalHeight;

  inherited;

  FOriginalHeight := AOriginalHeight;
end;

procedure TdxNavBarGroupControl.DefineProperties(Filer: TFiler);

  function NeedWriteGroupIndex: Boolean;
  begin
    Result := (Group <> nil) and (
      not (Filer.Ancestor is TdxNavBarGroupControl) or
      (TdxNavBarGroupControl(Filer.Ancestor).Group = nil) or
      (Group.Index <> TdxNavBarGroupControl(Filer.Ancestor).Group.Index)
    );
  end;

  function NeedWriteOriginalHeight: Boolean;
  begin
    Result := not (Filer.Ancestor is TdxNavBarGroupControl) or
      (OriginalHeight <> TdxNavBarGroupControl(Filer.Ancestor).OriginalHeight);
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('GroupIndex', ReadGroupIndex, WriteGroupIndex,
    NeedWriteGroupIndex);
  Filer.DefineProperty('OriginalHeight', ReadOriginalHeight,
    WriteOriginalHeight, NeedWriteOriginalHeight);
end;

procedure TdxNavBarGroupControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if IsOnPopupControl and PtInRect(SizeGripRect, cxPoint(X, Y)) then
    BeginResize(Self, Button, Shift, cxPoint(X, Y));
end;

procedure TdxNavBarGroupControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseOverSizeGrip := IsOnPopupControl and PtInRect(SizeGripRect, cxPoint(X, Y));
end;

procedure TdxNavBarGroupControl.ReadOriginalHeight(Reader: TReader);
begin
  FOriginalHeight := Reader.ReadInteger;
end;

procedure TdxNavBarGroupControl.ReadGroupIndex(Reader: TReader);
begin
  FGroupIndex := Reader.ReadInteger;
end;

procedure TdxNavBarGroupControl.RequestAlign;
begin
//  fixed bug: SC-B94216
end;

procedure TdxNavBarGroupControl.WriteOriginalHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FOriginalHeight);
end;

procedure TdxNavBarGroupControl.WriteGroupIndex(Writer: TWriter);
begin
  Writer.WriteInteger(Group.Index);
end;

procedure TdxNavBarGroupControl.ReadState(Reader: TReader);
var
  AGroup: TdxNavBarGroup;
begin
  inherited ReadState(Reader);
  if not (Reader.Parent is TdxCustomNavBar) then
    raise EdxException.Create(cxGetResourceString(@sdxInvalideGroupControl));
  if (0 > FGroupIndex) or (FGroupIndex > TdxCustomNavBar(Reader.Parent).Groups.Count - 1) then
    raise EdxException.Create(cxGetResourceString(@sdxInvalideGroupControl));
  AGroup := TdxCustomNavBar(Reader.Parent).Groups[FGroupIndex];
  if (AGroup.Control <> nil) and (AGroup.Control <> Self) then
    raise EdxException.Create(cxGetResourceString(@sdxInvalideGroupControl));
  Group := AGroup;
  Group.FControl := Self;
  if not Group.UseControl then // ???
  begin
    Group.OptionsGroupControl.FUseControl := True;
    Group.OptionsGroupControl.FShowControl := True;
  end;
//  Visible := False; ???
end;

procedure TdxNavBarGroupControl.SetParent(AParent: TWinControl);
begin
  if (AParent = nil) or ((AParent is TdxCustomNavBar) and (csLoading in ComponentState)) or
    ((Group <> nil) and (AParent = Group.GetParentComponent)) then
    inherited SetParent(AParent)
  else
    raise EdxException.Create(cxGetResourceString(@sdxInvalideGroupControl));
end;

procedure TdxNavBarGroupControl.Paint;
begin
  if UseStyle then
  begin
    if TdxCustomNavBar(Parent).Painter.ViewInfo.GetGroupViewInfoByGroup(Group) = nil then
    begin
      TdxCustomNavBar(Parent).Painter.InvalidateViewInfo(doRecreate);
      TdxNavBarPainterAccess(TdxCustomNavBar(Parent).Painter).CheckDrawParamChanges;
    end;
    if TdxCustomNavBar(Parent).Painter.ViewInfo.GetGroupViewInfoByGroup(Group) <> nil then
      TdxCustomNavBar(Parent).Painter.DrawGroupControl(Canvas, ClientRect, TdxCustomNavBar(Parent).Painter.ViewInfo.GetGroupViewInfoByGroup(Group));
  end
  else
    inherited;
  if IsOnPopupControl then
    DrawSizeGrip(Canvas, SizeGripRect);
end;

procedure TdxNavBarGroupControl.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TdxNavBarGroupControl.EndUpdate;
begin
  Dec(FUpdateLock);
end;

procedure TdxNavBarGroupControl.UpdateGroup;
begin
  if (FUpdateLock = 0) and (Group <> nil) then
    Group.Changed(True);
end;

function TdxNavBarGroupControl.GetAdornerTargetElementBounds: TRect;
begin
  Result := ClientRect;
end;

function TdxNavBarGroupControl.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Self;
end;

function TdxNavBarGroupControl.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

function TdxNavBarGroupControl.GetInternalNavBar: TControl;
var
  AControl: TWinControl;
begin
  Result := nil;
  if (Parent is TdxCustomNavBar) then
  begin
    AControl := Self;
    while (AControl.Parent <> nil) and not (AControl.Parent is TdxNavBarPopupControl) do AControl := AControl.Parent;
    if (AControl.Parent is TdxNavBarPopupControl) and (AControl is TdxCustomNavBar) then
      Result := AControl;
  end;
end;

procedure TdxNavBarGroupControl.SetOriginalHeight(const Value: Integer);
begin
  if (FOriginalHeight <> Value) and (Value > 0) then
  begin
    FOriginalHeight := Value;
    if not (csLoading in ComponentState) then
      Height := FOriginalHeight;
  end;
end;

procedure TdxNavBarGroupControl.SetUseStyle(const Value: Boolean);
begin
  if FUseStyle <> Value then
  begin
    FUseStyle := Value;
    Invalidate;
  end;
end;

function TdxNavBarGroupControl.SizeGripRect: TRect;
begin
  Result := GetSizeGripRect(Self);
end;

procedure TdxNavBarGroupControl.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_UNTHEMECONTROL:
      if (csDesigning in ComponentState) and cxIsVCLThemesAvailable then
        Exit;     // B187436
  end;
  inherited;
end;

function TdxNavBarGroupControl.GetMouseOverSizeGrip: Boolean;
begin
  Result := gcsOverSizeGrip in FInternalState;
end;

procedure TdxNavBarGroupControl.SetMouseOverSizeGrip(AValue: Boolean);
begin
  if MouseOverSizeGrip <> AValue then
    if AValue then
    begin
      FOriginalCursor := Cursor;
      Cursor := crSizeWE;
      Include(FInternalState, gcsOverSizeGrip);
    end
    else
    begin
      Cursor := FOriginalCursor;
      Exclude(FInternalState, gcsOverSizeGrip);
    end;
end;

procedure TdxNavBarGroupControl.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  UpdateGroup;
end;

{ TdxNavBarCustomItem }

constructor TdxNavBarCustomItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLargeImageIndex := -1;
  FSmallImageIndex := -1;
  FVisible := True;
end;

procedure TdxNavBarCustomItem.Assign(Source: TPersistent);
var
  ANavBarCustomItem: TdxNavBarCustomItem;
begin
  if Source is TdxNavBarCustomItem then
  begin
    ANavBarCustomItem := TdxNavBarCustomItem(Source);
    LargeImageIndex := ANavBarCustomItem.LargeImageIndex;
    SmallImageIndex := ANavBarCustomItem.SmallImageIndex;
    Visible := ANavBarCustomItem.Visible;
  end
  else
    inherited;
end;

function TdxNavBarCustomItem.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxNavBarCustomItem.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TdxNavBarCustomItem.DoSetVisible(Value: Boolean);
begin
  FVisible := Value;
  Changed(True);
end;

procedure TdxNavBarCustomItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TdxNavBarCustomItem.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
//    Changed;
  end;
end;

procedure TdxNavBarCustomItem.SetLargeImageIndex(const Value: TcxImageIndex);
begin
  if FLargeImageIndex <> Value then
  begin
    FLargeImageIndex := Value;
    Changed(False);
  end;
end;

procedure TdxNavBarCustomItem.SetSmallImageIndex(const Value: TcxImageIndex);
begin
  if FSmallImageIndex <> Value then
  begin
    FSmallImageIndex := Value;
    Changed(False);
  end;
end;

procedure TdxNavBarCustomItem.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
    DoSetVisible(Value);
end;

{ TdxNavBarGroupExpandOptions }

constructor TdxNavBarGroupExpansionOptions.Create(AOwner: TPersistent);
begin
  inherited;
  FAllowMultipleGroupExpansion := True;
  FExpandable := True;
  FExpanded := True;
  FShowExpandButton := True;
end;

procedure TdxNavBarGroupExpansionOptions.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarGroupExpansionOptions then
  begin
    AllowMultipleGroupExpansion := TdxNavBarGroupExpansionOptions(Source).AllowMultipleGroupExpansion;
    Expandable := TdxNavBarGroupExpansionOptions(Source).Expandable;
    Expanded := TdxNavBarGroupExpansionOptions(Source).Expanded;
    ShowExpandButton := TdxNavBarGroupExpansionOptions(Source).ShowExpandButton;
  end
  else
    inherited;
end;

function TdxNavBarGroupExpansionOptions.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(Owner);
end;

procedure TdxNavBarGroupExpansionOptions.SetAllowMultipleGroupExpansion(
  const Value: Boolean);
begin
  if FAllowMultipleGroupExpansion <> Value then
  begin
    FAllowMultipleGroupExpansion := Value;
    Group.AllowMultipleGroupExpansionChanged;
  end;
end;

procedure TdxNavBarGroupExpansionOptions.SetExpanded(AValue: Boolean);
begin
  if FExpanded <> AValue then
  begin
    FExpanded := AValue;
    if FExpanded then
      Group.DoExpand
    else
      Group.DoCollapse;
  end;
end;

procedure TdxNavBarGroupExpansionOptions.SetShowExpandButton(AValue: Boolean);
begin
   if FShowExpandButton <> AValue then
   begin
     FShowExpandButton := AValue;
     Group.Changed(False);
   end;
end;

{ TdxNavBarGroupControlOptions }

procedure TdxNavBarGroupControlOptions.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarGroupControlOptions then
  begin
    AllowControlResizing := TdxNavBarGroupControlOptions(Source).AllowControlResizing;
    UseControl := TdxNavBarGroupControlOptions(Source).UseControl;
    ShowControl := TdxNavBarGroupControlOptions(Source).ShowControl;
  end
  else
    inherited Assign(Source);
end;

function TdxNavBarGroupControlOptions.GetGroup: TdxNavBarGroup;
begin
  Result := TdxNavBarGroup(Owner);
end;

procedure TdxNavBarGroupControlOptions.SetAllowControlResizing(AValue: Boolean);
begin
  if FAllowControlResizing <> AValue then
  begin
    FAllowControlResizing := AValue;
    Group.Changed(True);
  end;
end;

procedure TdxNavBarGroupControlOptions.SetShowControl(AValue: Boolean);
begin
  if FShowControl <> AValue then
  begin
    FShowControl := AValue;
    if FShowControl and not FUseControl and not Group.IsLoading then
    begin
      FUseControl := True;
      Group.CreateControl;
    end;
    Group.Changed(True);
  end;
end;

procedure TdxNavBarGroupControlOptions.SetUseControl(AValue: Boolean);
begin
  if FUseControl <> AValue then
  begin
    FUseControl := AValue;
    if not Group.IsLoading then
      if FUseControl then
      begin
        FShowControl := True;
        Group.CreateControl;
      end
      else
      begin
        FShowControl := False;
        Group.DestroyControl;
      end;
    Group.Changed(True);
  end;
end;

{ TdxNavBarGroup }

constructor TdxNavBarGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChildren := TdxFastObjectList.Create(False);
  FLinks := TdxNavBarItemLinks.Create(Self);
  FOptionsGroupControl := TdxNavBarGroupControlOptions.Create(Self);
  FOptionsExpansion := TdxNavBarGroupExpansionOptions.Create(Self);
  FCustomStyles := TdxNavBarGroupCustomStyles.Create(Self);
  FCustomStyles.OnChange := StyleChanged;
  FLinksUseSmallImages := True;
  FLoadedSelectedLinkIndex := -1;
  FShowCaption := True;
  FTopVisibleLinkIndex := 0;
  FUseSmallImages := True;
  FVisibleForCustomization := True;

  FLoadedParentGroupIndex := -1;
end;

destructor TdxNavBarGroup.Destroy;

  procedure DestroyChildGroups;
  var
    I: Integer;
    AChild: TObject;
  begin
    Collection.BeginUpdate;
    try
      for I := FChildren.Count - 1 downto 0 do
      begin
        AChild := FChildren[I];
        FChildren.Delete(I);
        AChild.Free;
      end;
    finally
      Collection.EndUpdate(False);
    end;
  end;

begin
  DestroyAccessibilityHelpers;
  FreeAndNil(FCustomStyles);
  DestroyControl;
  FreeAndNil(FOptionsExpansion);
  FreeAndNil(FOptionsGroupControl);
  FreeAndNil(FLinks);
  DestroyChildGroups;
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxNavBarGroup.Assign(Source: TPersistent);
var
  ASourceGroup: TdxNavBarGroup;
begin
  if Source is TdxNavBarGroup then
  begin
    inherited Assign(Source);
    ASourceGroup := TdxNavBarGroup(Source);
    Caption := ASourceGroup.Caption;
    LinksUseSmallImages := ASourceGroup.LinksUseSmallImages;
    SelectedLinkIndex := ASourceGroup.SelectedLinkIndex;
    ShowAsIconView := ASourceGroup.ShowAsIconView;
    ShowCaption := ASourceGroup.ShowCaption;
    TopVisibleLinkIndex := ASourceGroup.TopVisibleLinkIndex;
    UseRestSpace := ASourceGroup.UseRestSpace;
    VisibleForCustomization := ASourceGroup.VisibleForCustomization;
    Hint := ASourceGroup.Hint;
    OptionsGroupControl := ASourceGroup.OptionsGroupControl;
    OptionsExpansion := ASourceGroup.OptionsExpansion;
    CustomStyles := ASourceGroup.CustomStyles;
  end
  else
    inherited Assign(Source);
end;

function TdxNavBarGroup.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomNavBar).Groups;
end;

procedure TdxNavBarGroup.ClearLinks;
begin
  FLinks.Clear;
end;

function TdxNavBarGroup.CreateLink(AItem: TdxNavBarItem): TdxNavBarItemLink;
begin
  if AItem.Collection.ParentComponent = Collection.ParentComponent then
  begin
    FLinks.BeginUpdate;
    try
      Result := FLinks.Add;
      Result.Item := AItem;
    finally
      FLinks.EndUpdate;
    end;
  end
  else
    raise EdxException.CreateFmt(cxGetResourceString(@sdxInvalidLink), [AItem.Name, Name]);
end;

function TdxNavBarGroup.FindLink(AItem: TdxNavBarItem): TdxNavBarItemLink;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to LinkCount - 1 do
    if Links[I].Item = AItem then
    begin
      Result := Links[I];
      break;
    end;
end;

function TdxNavBarGroup.InsertLink(AItem: TdxNavBarItem; APosition: Integer): TdxNavBarItemLink;
begin
  FLinks.BeginUpdate;
  try
    Result := CreateLink(AItem);
    MoveLink(Result, APosition);
  finally
    FLinks.EndUpdate;
  end;
end;

function TdxNavBarGroup.HasAsChild(AGroup: TdxNavBarGroup; AIsRecursive: Boolean = True): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ChildCount - 1 do
    if (Children[I] is TdxNavBarGroup) and
      ((Children[I] = AGroup) or AIsRecursive and TdxNavBarGroup(Children[I]).HasAsChild(AGroup, True)) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TdxNavBarGroup.MoveLink(ALink: TdxNavBarItemLink; APosition: Integer);
var
  ACurrentPos: Integer;
begin
  if APosition < FChildren.Count then
  begin
    ACurrentPos := ALink.Position;
    if ACurrentPos <> APosition then
    begin
      FLinks.BeginUpdate;
      try
        FChildren.Move(ACurrentPos, APosition);
        if not AllowChildGroups then
          ALink.Index := APosition;
      finally
        FLinks.EndUpdate;
      end;
    end;
  end
  else
end;

procedure TdxNavBarGroup.RemoveLink(AIndex: Integer);
begin
  FLinks.Delete(AIndex);
end;

procedure TdxNavBarGroup.RemoveLink(ALink: TdxNavBarItemLink);
begin
  FLinks.Delete(ALink.Index);
end;

procedure TdxNavBarGroup.RemoveLinks(AItem: TdxNavBarItem);
var
  I: Integer;
begin
  I := 0;
  while I < LinkCount do
    if Links[I].Item = AItem then
      RemoveLink(I)
    else
      Inc(I);
end;

procedure TdxNavBarGroup.MoveTo(AParent: TdxNavBarGroup; APosition: Integer);
var
  ANavBar: TdxCustomNavBar;
begin
  Collection.BeginUpdate;
  try
    if not AllowChildGroups then
      AParent := nil;
    if AParent <> Parent then
    begin
      if HasAsChild(AParent) then
        raise EdxException.Create('You cannot create recursive groups');
      if Parent = nil then
        (Collection as TdxNavBarGroups).FRootGroups.Remove(Self)
      else
        Parent.FChildren.Remove(Self);
      FParent := AParent;
      if Parent = nil then
        (Collection as TdxNavBarGroups).FRootGroups.Insert(APosition, Self)
      else
      begin
        Parent.FChildren.Insert(APosition, Self);
        ANavBar := GetNavBar(Self);
        if ANavBar.ActiveGroup = Self then
          TdxCustomNavBarAccess(ANavBar).TrySetActiveGroup(Parent);
      end;
      CheckSiblingGroups;
    end
    else
      Position := APosition;
  finally
    Collection.EndUpdate;
  end;
end;

procedure TdxNavBarGroup.LoadFromIniFile(AStorage: TCustomIniFile;
  const ASection: string; ALoadStyles: Boolean);
var
  I, ALinkCount: Integer;
  AUseControl: Boolean;
  AControlName: string;
  AControl: TCustomControl;
  AItemIndex: Integer;
  ANavBar: TdxCustomNavBar;
  AStyles: TdxNavBarStyleRepository;
  ALink: TdxNavBarItemLink;
begin
  ANavBar := GetNavBar(Self);
  Caption := AStorage.ReadString(ASection, 'Caption', Caption);
  Expanded := AStorage.ReadBool(ASection, 'Expanded', Expanded);
  Hint := AStorage.ReadString(ASection, 'Hint', Hint);
  LargeImageIndex := AStorage.ReadInteger(ASection, 'LargeImageIndex', LargeImageIndex);
  LinksUseSmallImages := AStorage.ReadBool(ASection, 'LinksUseSmallImages', LinksUseSmallImages);
  ClearLinks;
  ALinkCount := AStorage.ReadInteger(ASection, 'LinkCount', 0);
  for I := 0 to ALinkCount - 1 do
  begin
    AItemIndex :=
      AStorage.ReadInteger(ASection, 'Link' + IntToStr(I) + 'ItemIndex', -1);
    if InRange(AItemIndex, 0, ANavBar.Items.Count - 1) then
    begin
      ALink := CreateLink(ANavBar.Items[AItemIndex]);
      if AllowChildGroups then
        ALink.FLoadedPosition := AStorage.ReadInteger(ASection, 'Link' + IntToStr(I) + 'Position', -1);
    end;
  end;
  if AllowChildGroups then
  begin
    FLoadedPosition := AStorage.ReadInteger(ASection, 'Position', Position);
    FLoadedParentGroupIndex := AStorage.ReadInteger(ASection, 'ParentGroupIndex', GetParentGroupIndex);
  end;
  SelectedLinkIndex := AStorage.ReadInteger(ASection, 'SelectedLinkIndex', SelectedLinkIndex);
  ShowAsIconView := AStorage.ReadBool(ASection, 'ShowAsIconView', ShowAsIconView);
  ShowExpandButton := AStorage.ReadBool(ASection, 'ShowExpandButton', ShowExpandButton);
  SmallImageIndex := AStorage.ReadInteger(ASection, 'SmallImageIndex', SmallImageIndex);
  if ALoadStyles and (ANavBar.Styles.Count > 0) then
  begin
    AStyles := ANavBar.Styles;
    if AStorage.ValueExists(ASection, 'StyleBackgroundStyleIndex') then
      StyleBackground := AStyles[AStorage.ReadInteger(ASection, 'StyleBackgroundStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleHeaderStyleIndex') then
      StyleHeader := AStyles[AStorage.ReadInteger(ASection, 'StyleHeaderStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleHeaderActiveStyleIndex') then
      StyleHeaderActive := AStyles[AStorage.ReadInteger(ASection, 'StyleHeaderActiveStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleHeaderHotTrackedStyleIndex') then
      StyleHeaderHotTracked := AStyles[AStorage.ReadInteger(ASection, 'StyleHeaderHotTrackedStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleHeaderPressedStyleIndex') then
      StyleHeaderPressed := AStyles[AStorage.ReadInteger(ASection, 'StyleHeaderPressedStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'ChildCaption') then
      CustomStyles.ChildCaption := AStyles[AStorage.ReadInteger(ASection, 'ChildCaptionStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'ChildCaptionHotTracked') then
      CustomStyles.ChildCaptionHotTracked := AStyles[AStorage.ReadInteger(ASection, 'ChildCaptionHotTrackedStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'ChildCaptionPressed') then
      CustomStyles.ChildCaptionPressed := AStyles[AStorage.ReadInteger(ASection, 'ChildCaptionPressedStyleIndex', 0)];
  end;
  Tag := AStorage.ReadInteger(ASection, 'Tag', Tag);
  TopVisibleLinkIndex := AStorage.ReadInteger(ASection, 'TopVisibleLinkIndex', TopVisibleLinkIndex);
  UseSmallImages := AStorage.ReadBool(ASection, 'UseSmallImages', UseSmallImages);
  Visible := AStorage.ReadBool(ASection, 'Visible', Visible);

  AUseControl := AStorage.ReadBool(ASection, 'UseControl', UseControl);
  if AUseControl then
  begin
    AControlName := AStorage.ReadString(ASection, 'ControlName', '');
    AControl := TCustomControl(Owner.FindComponent(AControlName));
    if AControl is TdxNavBarGroupControl then
    begin
      AcceptControl(AControl as TdxNavBarGroupControl);
      UseControl := AUseControl;
      ShowControl := AStorage.ReadBool(ASection, 'ShowControl', ShowControl);
    end;
  end;
  if Control = nil then
    UseControl := False;
end;

procedure TdxNavBarGroup.SaveToIniFile(AStorage: TCustomIniFile; const ASection: string; ASaveStyles: Boolean);
var
  I: Integer;
begin
  with AStorage do
  begin
    WriteString(ASection, 'Caption', Caption);
    WriteBool(ASection, 'Expanded', Expanded);
    WriteString(ASection, 'Hint', Hint);
    WriteInteger(ASection, 'LargeImageIndex', LargeImageIndex);
    WriteInteger(ASection, 'LinkCount', LinkCount);
    if AllowChildGroups then
    begin
      WriteInteger(ASection, 'ParentGroupIndex', GetParentGroupIndex);
      WriteInteger(ASection, 'Position', Position);
    end;
    for I := 0 to LinkCount - 1 do
      if Links[I].Item <> nil then
      begin
        WriteInteger(ASection, 'Link' + IntToStr(I) + 'ItemIndex', Links[I].Item.Index);
        if AllowChildGroups then
          WriteInteger(ASection, 'Link' + IntToStr(I) + 'Position', Links[I].Item.Index);
      end;
    WriteBool(ASection, 'LinksUseSmallImages', LinksUseSmallImages);
    WriteString(ASection, 'Name', Name);
    WriteInteger(ASection, 'SelectedLinkIndex', SelectedLinkIndex);
    WriteBool(ASection, 'ShowControl', ShowControl);
    WriteBool(ASection, 'ShowAsIconView', ShowAsIconView);
    WriteBool(ASection, 'ShowExpandButton', ShowExpandButton);
    WriteInteger(ASection, 'SmallImageIndex', SmallImageIndex);
    if ASaveStyles and (GetNavBar(Self).Styles.Count > 0) then
    begin
      if StyleBackground <> nil then
        WriteInteger(ASection, 'StyleBackgroundStyleIndex', StyleBackground.Index);
      if StyleHeader <> nil then
        WriteInteger(ASection, 'StyleHeaderStyleIndex', StyleHeader.Index);
      if StyleHeaderActive <> nil then
        WriteInteger(ASection, 'StyleHeaderActiveStyleIndex', StyleHeaderActive.Index);
      if StyleHeaderHotTracked <> nil then
        WriteInteger(ASection, 'StyleHeaderHotTrackedStyleIndex', StyleHeaderHotTracked.Index);
      if StyleHeaderPressed <> nil then
        WriteInteger(ASection, 'StyleHeaderPressedStyleIndex', StyleHeaderPressed.Index);
      if CustomStyles.ChildCaption <> nil then
        WriteInteger(ASection, 'ChildCaptionStyleIndex', CustomStyles.ChildCaption.Index);
      if CustomStyles.ChildCaptionHotTracked <> nil then
        WriteInteger(ASection, 'ChildCaptionHotTrackedStyleIndex', CustomStyles.ChildCaptionHotTracked.Index);
      if CustomStyles.ChildCaptionPressed <> nil then
        WriteInteger(ASection, 'ChildCaptionPressedStyleIndex', CustomStyles.ChildCaptionPressed.Index);
    end;
    WriteInteger(ASection, 'Tag', Tag);
    WriteInteger(ASection, 'TopVisibleLinkIndex', TopVisibleLinkIndex);
    WriteBool(ASection, 'UseSmallImages', UseSmallImages);
    WriteBool(ASection, 'Visible', Visible);

    WriteBool(ASection, 'UseControl', UseControl);
    if UseControl then
    begin
      WriteBool(ASection, 'ShowControl', ShowControl);
      if Control <> nil then
        WriteString(ASection, 'ControlName', Control.Name);
    end;
  end;
end;

procedure TdxNavBarGroup.CorrectTopVisibleIndex(AValue: Integer);
begin
  DoSetTopVisibleLinkIndex(AValue, True);
end;

procedure TdxNavBarGroup.DefineProperties(Filer: TFiler);

  function NeedWriteParentGroupIndex: Boolean;
  var
    AAncestorGroup: TdxNavBarGroup;
  begin
    if not AllowChildGroups then
    begin
      Result := False;
      Exit;
    end;
    Result := Filer.Ancestor = nil;
    if not Result then
    begin
      AAncestorGroup := (Filer.Ancestor as TdxNavBarGroup);
      Result := (AAncestorGroup.Parent = nil) and (Parent <> nil) or
        (AAncestorGroup.Parent <> nil) and (Parent = nil) or
        (Parent <> nil) and (Parent.Name <> AAncestorGroup.Parent.Name);
    end;
  end;

  function NeedWritePosition: Boolean;
  begin
    Result := AllowChildGroups and ((Filer.Ancestor = nil) or
     ((Filer.Ancestor as TdxNavBarGroup).Position <> Position));
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Links', ReadLinks, WriteLinks, True);
  Filer.DefineProperty('ParentGroupIndex', ReadParentGroupIndex, WriteParentGroupIndex, NeedWriteParentGroupIndex);
  Filer.DefineProperty('Position', ReadPosition, WritePosition, NeedWritePosition);
end;

procedure TdxNavBarGroup.DestroyAccessibilityHelpers;
begin
  NavBarAccessibleObjectOwnerObjectDestroyed(FCaptionPanelIAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FIAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FLinkContainerIAccessibilityHelper);
end;

procedure TdxNavBarGroup.DoSetVisible(Value: Boolean);
var
  AActiveGroup: TdxNavBarGroup;
  ANavBar: TdxCustomNavBar;
begin
  if not IsLoading then
    AActiveGroup := GetNavBar(Self).ActiveGroup
  else
    AActiveGroup := nil;

  FVisible := Value;
  if Control <> nil then
    Control.Visible := FVisible;
  Changed(True);


  if not IsLoading then
  begin
    ANavBar := GetNavBar(Self);
    if FVisible and (AActiveGroup = nil) then
      TdxCustomNavBarAccess(ANavBar).TrySetActiveGroup(Self)
    else
      if not FVisible and (AActiveGroup = Self) then
        TdxCustomNavBarAccess(ANavBar).TrySetActiveGroup(nil);
  end;
end;

function TdxNavBarGroup.GetAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarGroupAccessibilityHelper;
end;

function TdxNavBarGroup.GetCaptionPanelAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarGroupCaptionPanelAccessibilityHelper;
end;

function TdxNavBarGroup.GetLinkContainerAccessibilityHelperClass: TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarItemLinkContainerAccessibilityHelper;
end;

procedure TdxNavBarGroup.Loaded;
begin
  inherited;
  if FControl = nil then // ???
  begin
    OptionsGroupControl.FUseControl := False;
    OptionsGroupControl.FShowControl := False;
  end;
  SelectedLinkIndex := FLoadedSelectedLinkIndex;
end;

procedure TdxNavBarGroup.SetIndex(AValue: Integer);
begin
  inherited SetIndex(AValue);
  if not AllowChildGroups then
    Position := Index;
end;

procedure TdxNavBarGroup.SortChildren;
var
  I: Integer;
begin
  if AllowChildGroups then
    for I := 0 to ChildCount - 1 do
      if Children[I] is TdxNavBarGroup then
        TdxNavBarGroup(Children[I]).SortChildren;
  FChildren.Sort(CompareChildren);
end;

procedure TdxNavBarGroup.ReadLinks(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FLinks);
  Changed(True);
end;

procedure TdxNavBarGroup.ReadParentGroupIndex(Reader: TReader);
begin
  FLoadedParentGroupIndex := Reader.ReadInteger;
  if csUpdating in ComponentState then
    (Collection as TdxNavBarGroups).FNeedGroupsReposition := True;
end;

procedure TdxNavBarGroup.ReadPosition(Reader: TReader);
begin
  FLoadedPosition := Reader.ReadInteger;
  if csUpdating in ComponentState then
    (Collection as TdxNavBarGroups).FNeedGroupsReposition := True;
end;

procedure TdxNavBarGroup.WriteLinks(Writer: TWriter);
begin
  Writer.WriteCollection(FLinks);
end;

procedure TdxNavBarGroup.WriteParentGroupIndex(Writer: TWriter);
begin
  Writer.WriteInteger(GetParentGroupIndex);
end;

procedure TdxNavBarGroup.WritePosition(Writer: TWriter);
begin
  Writer.WriteInteger(Position);
end;

procedure TdxNavBarGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent is TdxNavBarItem then
      RemoveLinks(AComponent as TdxNavBarItem);
    if AComponent = Control then
      UseControl := False;
  end;
end;

procedure TdxNavBarGroup.SetName(const NewName: TComponentName);
var
  L: Integer;
  OldName, NewControlName: string;
  ChangeCaption: Boolean;
begin
  ChangeCaption := not IsLoading and (Name = Caption);
  OldName := Name;
  L := Length(OldName);
  inherited SetName(NewName);
  if (Control <> nil) and (csDesigning in ComponentState) and not IsLoading then
  begin
    NewControlName := Control.Name;
    if Pos(OldName, NewControlName) = 1 then
    begin
      Delete(NewControlName, 1, L);
      Insert(Name, NewControlName, 1);
      try
        Control.Name := NewControlName;
      except
        on EComponentError do ; {Ignore rename errors }
      end;
    end;
  end;
  if ChangeCaption then Caption := NewName;
  if OldName <> Name then
    Changed(False);
end;

procedure TdxNavBarGroup.AllowMultipleGroupExpansionChanged;
var
  I: Integer;
begin
  Collection.BeginUpdate;
  try
    for I := 0 to ChildCount - 1 do
      if (Children[I] is TdxNavBarGroup) and
         TdxNavBarGroup(Children[I]).CheckSiblingGroups then
        Break;
  finally
    Collection.EndUpdate;
  end;
end;

procedure TdxNavBarGroup.StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);
begin
  Changed(True);
end;

procedure TdxNavBarGroup.InitiateActions;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    Links[I].InitiateAction;
end;

procedure TdxNavBarGroup.DoExpand;
begin
  TdxNavBarGroups(Collection).JustExpandedGroup := Self;
  (Collection as TdxNavBarGroups).ExpandStateChanged(Self);
  DoExpanded;
end;

procedure TdxNavBarGroup.DoExpanded;
begin
  if Assigned(FOnExpanded) then
    FOnExpanded(Self);
end;

procedure TdxNavBarGroup.DoCollapse;
begin
  (Collection as TdxNavBarGroups).ExpandStateChanged(Self);
  DoCollapsed;
end;

procedure TdxNavBarGroup.DoCollapsed;
begin
  if Assigned(FOnCollapsed) then
    FOnCollapsed(Self);
end;

procedure TdxNavBarGroup.DoSelectedLinkChanged;
begin
  if Assigned(FOnSelectedLinkChanged) then
    FOnSelectedLinkChanged(Self);
end;

procedure TdxNavBarGroup.DoTopVisibleLinkChanged;
begin
  if Assigned(FOnTopVisibleLinkChanged) then
    FOnTopVisibleLinkChanged(Self);
end;

procedure TdxNavBarGroup.CreateControl;
begin
  if IsLoading then exit;
  if (FControl = nil) and (Collection <> nil) then
  begin
    FControl := TdxNavBarGroupControl.Create(Owner);
    FControl.Group := Self;
    if Name <> '' then FControl.Name := Name + 'Control';
    FControl.Parent := TWinControl(GetParentComponent);
  end
end;

procedure TdxNavBarGroup.DestroyControl;
begin
  if (FControl <> nil) and not (csDestroying in FControl.ComponentState) then
    FControl.Free;
  FControl := nil;
end;

procedure TdxNavBarGroup.AcceptControl(AControl: TdxNavBarGroupControl);
begin
  if (Collection = nil) or (AControl.Parent <> GetParentComponent) then exit;
  FControl := AControl;
  FControl.Group := Self;
end;

procedure TdxNavBarGroup.ReleaseControl;
begin
  if FControl <> nil then
  begin
    FControl.Group := nil;
    FControl := nil;
  end;
end;

function TdxNavBarGroup.CheckSiblingGroups: Boolean;
begin
  Result := Expanded;
  if Result then
    if (Parent = nil) and not GetNavBar(Self).OptionsBehavior.Common.AllowMultipleGroupExpansion or
      (Parent <> nil) and not Parent.OptionsExpansion.AllowMultipleGroupExpansion then
      (Collection as TdxNavBarGroups).CollapseSiblings(Self);
end;

procedure TdxNavBarGroup.LinksChanged(ALink: TdxNavBarItemLink);
begin
  (Collection as TdxNavBarGroups).DoLinksChanged(ALink);
end;

function TdxNavBarGroup.GetCaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  if FCaptionPanelIAccessibilityHelper = nil then
    FCaptionPanelIAccessibilityHelper := NavBarGetAccessibilityHelper(
      GetCaptionPanelAccessibilityHelperClass.Create(Self,
        Collection.ParentComponent as TdxCustomNavBar));
  Result := FCaptionPanelIAccessibilityHelper;
end;

function TdxNavBarGroup.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TdxNavBarGroup.GetChildObjects(Index: Integer): TObject;
begin
  Result := FChildren[Index];
end;

function TdxNavBarGroup.GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := NavBarGetAccessibilityHelper(
      GetAccessibilityHelperClass.Create(Self,
        Collection.ParentComponent as TdxCustomNavBar));
  Result := FIAccessibilityHelper;
end;

function TdxNavBarGroup.GetLevel: Integer;
begin
  if Parent <> nil then
    Result := Parent.Level + 1
  else
    Result := 0;
end;

function TdxNavBarGroup.GetLinkContainerIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  if FLinkContainerIAccessibilityHelper = nil then
    FLinkContainerIAccessibilityHelper := NavBarGetAccessibilityHelper(
      GetLinkContainerAccessibilityHelperClass.Create(Self,
        Collection.ParentComponent as TdxCustomNavBar));
  Result := FLinkContainerIAccessibilityHelper;
end;

function TdxNavBarGroup.GetLinkCount: Integer;
begin
  Result := FLinks.Count;
end;

function TdxNavBarGroup.GetNavigationItemImageIndex: TcxImageIndex;
begin
  if UseSmallImages then
    Result := SmallImageIndex
  else
    Result := LargeImageIndex;
end;

function TdxNavBarGroup.GetAdornerTargetElementBounds: TRect;

  function GetGroupViewInfoByGroup(AGroup: TdxNavBarGroup): TdxNavBarGroupViewInfo;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to GetNavBar(Self).ViewInfo.GroupCount - 1 do
    begin
      Result := GetNavBar(Self).ViewInfo.Groups[I].GetGroupViewInfoByGroup(AGroup);
      if (Result <> nil) and not cxRectIsEmpty(Result.CaptionRect) then
        Break
      else
        Result := nil;
    end;
  end;

var
  AViewInfo: TdxNavBarGroupViewInfo;
begin
  AViewInfo := GetGroupViewInfoByGroup(Self);
  if AViewInfo <> nil then
    Result := AViewInfo.CaptionRect
  else
    Result := cxNullRect;
end;

function TdxNavBarGroup.GetAdornerTargetElementControl: TWinControl;
begin
  Result := GetNavBar(Self);
end;

function TdxNavBarGroup.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible and (GetNavBar(Self).ViewInfo.GetGroupViewInfoByGroup(Self) <> nil);
end;

procedure TdxNavBarGroup.GetAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Links[I].Item <> nil then
      AList.AddObject(Links[I].Item.Name, Links[I]);
end;

function TdxNavBarGroup.GetNavigationItemID: Integer;
begin
  Result := ID;
end;

function TdxNavBarGroup.GetNavigationItemText: string;
begin
  Result := Caption;
end;

function TdxNavBarGroup.GetPosition: Integer;
begin
  if Parent = nil then
    Result := (Collection as TdxNavBarGroups).FRootGroups.IndexOf(Self)
  else
    Result := Parent.FChildren.IndexOf(Self);
end;

function TdxNavBarGroup.AllowChildGroups: Boolean;
begin
  Result := GetNavBar(Self).OptionsBehavior.Common.AllowChildGroups;
end;

function TdxNavBarGroup.CompareChildren(AItem1,
  AItem2: Pointer): Integer;

  function GetPosition(AItem: Pointer): Integer;
  begin
    if AllowChildGroups then
      if TObject(AItem) is TdxNavBarItemLink then
        Result := TdxNavBarItemLink(AItem).FLoadedPosition
      else
        if TObject(AItem) is TdxNavBarGroup then
          Result := TdxNavBarGroup(AItem).FLoadedPosition
        else
          raise EdxException.Create('Unknown child item type')
    else
      Result := (TObject(AItem) as TdxNavBarItemLink).Index;
  end;

var
  APosition1, APosition2: Integer;
begin
  APosition1 := GetPosition(AItem1);
  APosition2 := GetPosition(AItem2);
  if APosition1 = APosition2 then
    Result := 0
  else
    if APosition1 < APosition2 then
      Result := -1
    else
      Result := 1;
end;

procedure TdxNavBarGroup.DoSetTopVisibleLinkIndex(AValue: Integer; AIsCorrection: Boolean);
var
  APrevTopVisibleIndex: Integer;
begin
  AValue := Min(AValue, LinkCount - 1);
  AValue := Max(AValue, 0);
  if FTopVisibleLinkIndex <> AValue then
  begin
    APrevTopVisibleIndex := FTopVisibleLinkIndex;
    FTopVisibleLinkIndex := AValue;
    if not AIsCorrection then
      Changed(False);
    if not FIsSettingTopVisibleIndex or
      not AIsCorrection and (FTopVisibleLinkIndex <> APrevTopVisibleIndex) then
      DoTopVisibleLinkChanged;
  end;
end;

function TdxNavBarGroup.GetParentGroupIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.Index
  else
    Result := -1;
end;

function TdxNavBarGroup.GetLink(Index: Integer): TdxNavBarItemLink;
begin
  Result := FLinks[Index];
end;

function TdxNavBarGroup.GetSelectedLinkIndex: Integer;
begin
  if IsLoading then
    Result := FLoadedSelectedLinkIndex
  else
    if SelectedLink = nil then
      Result := -1
    else
      Result := SelectedLink.Index;
end;

procedure TdxNavBarGroup.SetAlign(const Value: TcxTopBottom);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    Changed(True);
  end;
end;

function TdxNavBarGroup.GetExpandable: Boolean;
begin
  Result := OptionsExpansion.Expandable;
end;

function TdxNavBarGroup.GetExpanded: Boolean;
begin
  Result := OptionsExpansion.Expanded;
end;

function TdxNavBarGroup.GetShowExpandButton: Boolean;
begin
  Result := OptionsExpansion.ShowExpandButton;
end;

procedure TdxNavBarGroup.SetOptionsExpansion(const Value: TdxNavBarGroupExpansionOptions);
begin
  if FOptionsExpansion <> Value then
    FOptionsExpansion.Assign(Value);
end;

procedure TdxNavBarGroup.SetExpandable(const Value: Boolean);
begin
  OptionsExpansion.Expandable := Value;
end;

procedure TdxNavBarGroup.SetExpanded(const Value: Boolean);
begin
  if Value and (Parent <> nil) then
    Parent.Expanded := True;
  OptionsExpansion.Expanded := Value;
end;

procedure TdxNavBarGroup.SetShowExpandButton(const Value: Boolean);
begin
  OptionsExpansion.ShowExpandButton := Value;
end;

procedure TdxNavBarGroup.SetSelectedLinkIndex(Value: Integer);
begin
  if IsLoading then
    FLoadedSelectedLinkIndex := Value
  else
  begin
    if (Value < -1) or not GetNavBar(Self).AllowSelectLinks then
      Value := -1
    else
      if Value > LinkCount - 1 then
        Value := LinkCount - 1;

    if SelectedLinkIndex <> Value then
    begin
      if not GetNavBar(Self).EachGroupHasSelectedLink and
        (SelectedLinkIndex = -1) and (Value <> -1) then
        GetNavBar(Self).DeSelectLinks;
      if Value = -1 then
        FSelectedLink := nil
      else
        FSelectedLink := Links[Value];
      Changed(False);
      DoSelectedLinkChanged;
    end;
  end;
end;

procedure TdxNavBarGroup.SetShowAsIconView(const Value: Boolean);
begin
  if FShowAsIconView <> Value then
  begin
    FShowAsIconView := Value;
    Changed(False);
  end;
end;

procedure TdxNavBarGroup.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Changed(True);
  end;
end;

procedure TdxNavBarGroup.SetTopVisibleLinkIndex(AValue: Integer);
begin
  if IsLoading then
    FTopVisibleLinkIndex := AValue
  else
  begin
    FIsSettingTopVisibleIndex := True;
    try
      DoSetTopVisibleLinkIndex(AValue, False);
    finally
      FIsSettingTopVisibleIndex := False;
    end;
  end;
end;

procedure TdxNavBarGroup.SetUseRestSpace(Value: Boolean);
begin
  if FUseRestSpace <> Value then
  begin
    FUseRestSpace := Value;
    if Expanded then
      Changed(True);
  end;
end;

procedure TdxNavBarGroup.SetLinksUseSmallImages(const Value: Boolean);
begin
  if FLinksUseSmallImages <> Value then
  begin
    FLinksUseSmallImages := Value;
    Changed(False);
  end;
end;

procedure TdxNavBarGroup.SetPosition(const Value: Integer);
begin
  if Position <> Value then
  begin
    if Parent <> nil then
      Parent.FChildren.Move(Position, Value)
    else
    begin
      (Collection as TdxNavBarGroups).FRootGroups.Move(Position, Value);
      if not AllowChildGroups then
        Index := Value;
    end;
    Changed(True);
  end;
end;

procedure TdxNavBarGroup.SetUseSmallImages(const Value: Boolean);
begin
  if FUseSmallImages <> Value then
  begin
    FUseSmallImages := Value;
    Changed(False);
  end;
end;

function TdxNavBarGroup.GetShowControl: Boolean;
begin
  Result := OptionsGroupControl.ShowControl;
end;

function TdxNavBarGroup.GetUseControl: Boolean;
begin
  Result := OptionsGroupControl.UseControl;
end;

procedure TdxNavBarGroup.SetOptionsGroupControl(const Value: TdxNavBarGroupControlOptions);
begin
  if FOptionsGroupControl <> Value then
    FOptionsGroupControl.Assign(Value);
end;

procedure TdxNavBarGroup.SetShowControl(const Value: Boolean);
begin
  OptionsGroupControl.ShowControl := Value;
end;

procedure TdxNavBarGroup.SetUseControl(const Value: Boolean);
begin
  OptionsGroupControl.UseControl := Value;
end;

procedure TdxNavBarGroup.SetCustomStyles(Value: TdxNavBarGroupCustomStyles);
begin
  FCustomStyles.Assign(Value);
end;

function TdxNavBarGroup.GetCustomStyle(Index: Integer): TdxNavBarStyleItem;
begin
  Result := CustomStyles.Styles[Index];
end;

procedure TdxNavBarGroup.SetCustomStyle(Index: Integer; const Value: TdxNavBarStyleItem);
begin
  CustomStyles.Styles[Index] := Value;
end;

{ TdxNavBarCustomItems }

function TdxNavBarCustomItems.Add: TdxNavBarCustomItem;
begin
  Result := inherited Add as TdxNavBarCustomItem;
end;

function TdxNavBarCustomItems.ItemByCaption(const ACaption: string): TdxNavBarCustomItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Caption = ACaption then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxNavBarCustomItems.GetItem(Index: Integer): TdxNavBarCustomItem;
begin
  Result := inherited GetItem(Index) as TdxNavBarCustomItem;
end;

procedure TdxNavBarCustomItems.SetItem(Index: Integer; Value: TdxNavBarCustomItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TdxNavBarCustomItems.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  inherited SetItemName(AItem, Count);
end;

{ TdxNavBarGroups }

{ TdxNavBarGroups.TGroupPosComparer }

constructor TdxNavBarGroups.TGroupPosComparer.Create(AOwner: TdxNavBarGroups);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxNavBarGroups.TGroupPosComparer.Compare(const Left,
  Right: TdxNavBarGroup): Integer;
begin
  if FOwner.AllowChildGroups then
    if Left.FLoadedPosition = Right.FLoadedPosition then
      Result := 0
    else
      if Left.FLoadedPosition < Right.FLoadedPosition then
        Result := -1
      else
        Result := 1
  else
    if Left.Index = Right.Index then
      Result := 0
    else
      if Left.Index < Right.Index then
        Result := -1
      else
        Result := 1;
end;

{ TdxNavBarGroups.TNavigationItemEnumerator }

constructor TdxNavBarGroups.TNavigationItemEnumerator.Create(AList: TList<TdxNavBarGroup>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TdxNavBarGroups.TNavigationItemEnumerator.GetCurrent: TObject;
begin
  Result := FList[FIndex];
end;

function TdxNavBarGroups.TNavigationItemEnumerator.GetCurrentNavigationItem: IdxNavigationItem;
begin
  Supports(GetCurrent, IdxNavigationItem, Result);
end;

function TdxNavBarGroups.TNavigationItemEnumerator.MoveNext: Boolean;
begin
  repeat
   Inc(FIndex);
   Result := FIndex < FList.Count;
  until not Result or FList[FIndex].Visible;
end;

procedure TdxNavBarGroups.TNavigationItemEnumerator.Reset;
begin
  FIndex := -1;
end;

constructor TdxNavBarGroups.Create(AParentComponent: TComponent;
  AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);
  FGroupPosComparer := TGroupPosComparer.Create(Self);
  FRootGroups := TList<TdxNavBarGroup>.Create;
end;

destructor TdxNavBarGroups.Destroy;
begin
  FreeAndNil(FRootGroups);
  FreeAndNil(FGroupPosComparer);
  inherited Destroy;
end;

function TdxNavBarGroups.Add: TdxNavBarGroup;
begin
  Result := inherited Add as TdxNavBarGroup;
end;

procedure TdxNavBarGroups.AllowMultipleGroupExpansionChanged;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to RootGroupCount - 1 do
      if RootGroups[I].CheckSiblingGroups then
        Break;
  finally
    EndUpdate;
  end;
end;

procedure TdxNavBarGroups.CollapseSiblings(AGroup: TdxNavBarGroup);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I] <> AGroup) and (Items[I].Parent = AGroup.Parent) then
      Items[I].Expanded := False;
end;

procedure TdxNavBarGroups.InitiateActions;
begin
  inherited InitiateActions;
end;

procedure TdxNavBarGroups.LoadFromIniFile(AStorage: TCustomIniFile; ALoadStyles: Boolean);

  function GetCustomItem(ACollection: TdxNavBarCustomItems; const ASection: string): TdxNavBarCustomItem;
  var
    AItemIdentifier: string;
  begin
    Result := nil;
    AItemIdentifier := AStorage.ReadString(ASection, 'Name', '');
    if AItemIdentifier <> '' then
      Result := TdxNavBarCustomItem(ACollection.ItemByName(AItemIdentifier))
    else
    begin
      AItemIdentifier := AStorage.ReadString(ASection, 'Caption', '');
      if AItemIdentifier <> '' then
        Result := ACollection.ItemByCaption(AItemIdentifier);
    end;
    if Result = nil then
      Result := ACollection.Add;
  end;

  procedure ReadGroup(AIndex: Integer);
  var
    ASection: string;
    AGroup: TdxNavBarGroup;
  begin
    ASection := 'Group' + IntToStr(AIndex);
    AGroup := TdxNavBarGroup(GetCustomItem(Self, ASection));
    AGroup.LoadFromIniFile(AStorage, ASection, ALoadStyles);
    AGroup.Index := AIndex;
  end;

type
  TdxNavBarGroupControlState = record
    Group: TdxNavBarGroup;
    Control: TdxNavBarGroupControl;
  end;

var
  ACount, I: Integer;
  AGroup: TdxNavBarGroup;
  AGroupControl: TdxNavBarGroupControl;
  AGroupControlStates: array of TdxNavBarGroupControlState;
begin
  if AStorage.ValueExists('Layout', 'GroupCount') then
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].UseControl and (Items[I].Control <> nil) then
      begin
        SetLength(AGroupControlStates, Length(AGroupControlStates) + 1);
        with AGroupControlStates[High(AGroupControlStates)] do
        begin
          Group := Items[I];
          Control := Items[I].Control;
        end;
        Items[I].ReleaseControl;
      end;
      Items[I].FLoadedPosition := Items[I].Position;
      Items[I].FLoadedParentGroupIndex := Items[I].GetParentGroupIndex;
    end;
    ACount := AStorage.ReadInteger('Layout', 'GroupCount', Count);
    for I := 0 to ACount - 1 do
      ReadGroup(I);
    UpdateGroupPositions;
    for I := 0 to High(AGroupControlStates) do
    begin
      AGroupControl := AGroupControlStates[I].Control;
      AGroup := AGroupControlStates[I].Group;
      if AGroupControl.Group = nil then
        if AGroup.UseControl and (AGroup.Control = nil) then
          AGroup.AcceptControl(AGroupControl)
        else
          AGroupControl.Free;
    end;
  end;
end;

procedure TdxNavBarGroups.Loaded;
begin
  if AllowChildGroups then
  begin
    BeginUpdate;
    try
      AssignGroupPositions;
      DoSort;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxNavBarGroups.StructureChanged;
begin
  if not AllowChildGroups then
  begin
    BeginUpdate;
    try
      ResetGroupPositions;
      DoSort;
    finally
      EndUpdate;
    end;
  end;
end;

function TdxNavBarGroups.GetItem(Index: Integer): TdxNavBarGroup;
begin
  Result := inherited GetItem(Index) as TdxNavBarGroup;
end;

procedure TdxNavBarGroups.SetItem(Index: Integer; Value: TdxNavBarGroup);
begin
  inherited SetItem(Index, Value);
end;

procedure TdxNavBarGroups.DoLinksChanged(ALink: TdxNavBarItemLink);
begin
  if UpdateCount = 0 then
    if Assigned(FOnLinksChange) then FOnLinksChange(Self, ALink);
end;

procedure TdxNavBarGroups.ExpandStateChanged(AGroup: TdxNavBarGroup);
begin
  if UpdateCount = 0 then
    (ParentComponent as TdxCustomNavBar).ExpandStateChanged(AGroup)
  else
    AGroup.CheckSiblingGroups;
end;

procedure TdxNavBarGroups.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
var
  AGroup: TdxNavBarGroup;
begin
  if AAction = ccnExtracted then
  begin
    if AItem = FJustExpandedGroup then
      FJustExpandedGroup := nil;
    if AItem <> nil then
    begin
      AGroup := AItem as TdxNavBarGroup;
      AGroup.DestroyAccessibilityHelpers;
      if AGroup.Parent <> nil then
        AGroup.Parent.FChildren.Remove(AGroup)
      else
        if FRootGroups <> nil then
          FRootGroups.Remove(AGroup);
    end;
  end
  else
    if AAction = ccnAdded then
      FRootGroups.Add(AItem as TdxNavBarGroup);
end;

procedure TdxNavBarGroups.SetGroupOrder(AOrder: TList<TdxNavBarGroup>);
begin
  FRootGroups.Clear;
  FRootGroups.AddRange(AOrder);
end;

function TdxNavBarGroups.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxNavBarGroups._AddRef: Integer;
begin
  Result := -1;
end;

function TdxNavBarGroups._Release: Integer;
begin
  Result := -1;
end;

function TdxNavBarGroups.GetNavigationItemEnumerator: IEnumerator<IdxNavigationItem>;
begin
  Result := TNavigationItemEnumerator.Create(FRootGroups);
end;

function TdxNavBarGroups.GetEnumerator: IEnumerator;
begin
  Result := GetNavigationItemEnumerator;
end;

procedure TdxNavBarGroups.Updated;
begin
  if FNeedGroupsReposition then
    UpdateGroupPositions;
end;

procedure TdxNavBarGroups.Updating;
begin
  FNeedGroupsReposition := False;
end;

procedure TdxNavBarGroups.SaveToIniFile(AStorage: TCustomIniFile; ASaveStyles: Boolean);
var
  I: Integer;
begin
  AStorage.WriteInteger('Layout', 'GroupCount', Count);
  for I := 0 to Count - 1 do
    Items[I].SaveToIniFile(AStorage, 'Group' + IntToStr(Items[I].Index), ASaveStyles);
end;

function TdxNavBarGroups.AllowChildGroups: Boolean;
begin
  Result := (ParentComponent as TdxCustomNavBar).OptionsBehavior.Common.AllowChildGroups;
end;

procedure TdxNavBarGroups.AssignGroupPositions;
var
  I: Integer;
  AGroup: TdxNavBarGroup;
  AParentIndex: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AGroup := Items[I];
    AParentIndex := AGroup.FLoadedParentGroupIndex;
    if AParentIndex >= 0 then
      AGroup.MoveTo(Items[AParentIndex], Min(AGroup.FLoadedPosition, Items[AParentIndex].ChildCount));
  end;
end;

procedure TdxNavBarGroups.DoSort;
var
  I: Integer;
begin
  FRootGroups.Sort(FGroupPosComparer);
  for I := 0 to FRootGroups.Count - 1 do
    FRootGroups[I].SortChildren;
end;

function TdxNavBarGroups.GetRootGroupCount: Integer;
begin
  Result := FRootGroups.Count;
end;

function TdxNavBarGroups.GetRootGroups(Index: Integer): TdxNavBarGroup;
begin
  Result := FRootGroups[Index];
end;

procedure TdxNavBarGroups.ResetGroupPositions;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].MoveTo(nil, I);
end;

procedure TdxNavBarGroups.UpdateGroupPositions;
begin
  BeginUpdate;
  try
    ResetGroupPositions;
    AssignGroupPositions;
    DoSort;
  finally
    EndUpdate;
  end;
end;

{ TdxNavBarItemActionLink }

procedure TdxNavBarItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TdxNavBarItem;
end;

function TdxNavBarItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TdxNavBarItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TdxNavBarItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TdxNavBarItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.LargeImageIndex = (Action as TCustomAction).ImageIndex) and
    (FClient.SmallImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TdxNavBarItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TdxNavBarItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TdxNavBarItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TdxNavBarItemActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TdxNavBarItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    FClient.LargeImageIndex := Value;
    FClient.SmallImageIndex := Value;
  end;
end;

procedure TdxNavBarItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

{ TdxNavBarItemCalculator }

class procedure TdxNavBarCustomGroupItemCalculator.CalculateBounds(
  X, Y: Integer; AScaleFactor: TdxScaleFactor; var ALinkViewInfo);
begin
  // do nothing
end;

{ TdxNavBarItem }

constructor TdxNavBarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomStyles := TdxNavBarItemCustomStyles.Create(Self);
  FCustomStyles.OnChange := StyleChanged;
  FEnabled := True;
end;

destructor TdxNavBarItem.Destroy;
begin
  FreeAndNil(FCustomStyles);
  FreeAndNil(FActionLink);
  inherited Destroy;
end;

procedure TdxNavBarItem.Assign(Source: TPersistent);
var
  ANavBarItem: TdxNavBarItem;
begin
  if Source is TdxNavBarItem then
  begin
    ANavBarItem := TdxNavBarItem(Source);
    Action := ANavBarItem.Action;
    inherited;
    Caption := ANavBarItem.Caption;
    Enabled := ANavBarItem.Enabled;
    Hint := ANavBarItem.Hint;
    CustomStyles := ANavBarItem.CustomStyles;
    OnClick := ANavBarItem.OnClick;
  end
  else
    inherited;
end;

function TdxNavBarItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomNavBar).Items;
end;

procedure TdxNavBarItem.Draw(const APainter, ALinkViewInfo);
begin
  TdxNavBarPainter(APainter).DrawItem(TdxNavBarLinkViewInfo(ALinkViewInfo));
end;

procedure TdxNavBarItem.LoadFromIniFile(AStorage: TCustomIniFile;
  const ASection: string; ALoadStyles: Boolean);
var
  AStyles: TdxNavBarStyleRepository;
begin
  Caption := AStorage.ReadString(ASection, 'Caption', Caption);
  Enabled := AStorage.ReadBool(ASection, 'Enabled', Enabled);
  Hint := AStorage.ReadString(ASection, 'Hint', Hint);
  LargeImageIndex := AStorage.ReadInteger(ASection, 'LargeImageIndex',
    LargeImageIndex);
  SmallImageIndex := AStorage.ReadInteger(ASection, 'SmallImageIndex',
    SmallImageIndex);
  AStyles := GetNavBar(Self).Styles;
  if ALoadStyles and (AStyles.Count > 0) then
  begin
    if AStorage.ValueExists(ASection, 'StyleStyleIndex') then
      Style := AStyles[AStorage.ReadInteger(ASection, 'StyleStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleDisabledStyleIndex') then
      StyleDisabled := AStyles[AStorage.ReadInteger(ASection, 'StyleDisabledStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StyleHotTrackedStyleIndex') then
      StyleHotTracked := AStyles[AStorage.ReadInteger(ASection, 'StyleHotTrackedStyleIndex', 0)];
    if AStorage.ValueExists(ASection, 'StylePressedStyleIndex') then
      StylePressed := AStyles[AStorage.ReadInteger(ASection, 'StylePressedStyleIndex', 0)];
  end;
  Tag := AStorage.ReadInteger(ASection, 'Tag', Tag);
  Visible := AStorage.ReadBool(ASection, 'Visible', Visible);
end;

procedure TdxNavBarItem.RemoveAllLinks;
var
  I: Integer;
  ANavBar: TdxCustomNavBar;
begin
  ANavBar := GetNavBar(Self);
  ANavBar.BeginUpdate;
  try
    for I := 0 to ANavBar.Groups.Count - 1 do
      ANavBar.Groups[I].RemoveLinks(Self);
  finally
    ANavBar.EndUpdate;
  end;
end;

procedure TdxNavBarItem.Loaded;
begin
  inherited;
  if Action <> nil then ActionChange(Action, True);
end;

procedure TdxNavBarItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Action) then
    Action := nil;
end;

procedure TdxNavBarItem.SetName(const NewName: TComponentName);
var
  ChangeCaption: Boolean;
begin
  ChangeCaption := not (csLoading in ComponentState) and (Name = Caption);
  inherited SetName(NewName);
  if ChangeCaption then Caption := NewName;
  Changed(False);
end;

procedure TdxNavBarItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or ((Self.LargeImageIndex = -1) and (Self.SmallImageIndex = -1))then
      begin
        Self.LargeImageIndex := ImageIndex;
        Self.SmallImageIndex := ImageIndex;
      end;
      if not CheckDefaults or (Self.Visible) then
        Self.Visible := Visible;
    end;
end;

function TdxNavBarItem.CanSelect: Boolean;
begin
  Result := Enabled;
end;

class function TdxNavBarItem.GetCalculatorClass: TdxNavBarCustomGroupItemCalculatorClass;
begin
  Result := TdxNavBarItemCalculator;
end;

procedure TdxNavBarItem.StyleChanged(Sender: TObject; AType: TdxNavBarChangeType);
begin
  Changed(False);
end;

function TdxNavBarItem.GetAction: TBasicAction;
begin
  if FActionLink = nil then Result := nil
  else Result := FActionLink.Action;
end;

procedure TdxNavBarItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    if FActionLink <> nil then
      FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if (Value is TCustomAction) and (csDesigning in Value.ComponentState) then
      TCustomAction(Value).DisableIfNoHandler := False;
    if FActionLink = nil then
      FActionLink := TdxNavBarItemActionLink.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;

procedure TdxNavBarItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

function TdxNavBarItem.GetCustomStyle(Index: Integer): TdxNavBarStyleItem;
begin
  Result := CustomStyles.Styles[Index];
end;

procedure TdxNavBarItem.SetCustomStyles(Value: TdxNavBarItemCustomStyles);
begin
  FCustomStyles.Assign(Value);
end;

procedure TdxNavBarItem.SetCustomStyle(Index: Integer; const Value: TdxNavBarStyleItem);
begin
  CustomStyles.Styles[Index] := Value;
end;

procedure TdxNavBarItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TdxNavBarItem.IsCaptionStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TdxNavBarItem.IsEnabledStored: Boolean;
begin
  Result := not Enabled and ((FActionLink = nil) or not FActionLink.IsEnabledLinked);
end;

function TdxNavBarItem.IsHintStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TdxNavBarItem.IsImageIndexStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TdxNavBarItem.IsVisibleStored: Boolean;
begin
  Result := (FActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

{ TdxNavBarItems }

function TdxNavBarItems.Add: TdxNavBarItem;
begin
  Result := inherited Add as TdxNavBarItem;
end;

function TdxNavBarItems.Add(AItemClass: TcxComponentCollectionItemClass{TdxNavBarItemClass}): TdxNavBarItem;
begin
  Result := inherited Add(AItemClass) as TdxNavBarItem;
end;

procedure TdxNavBarItems.LoadFromIniFile(AStorage: TCustomIniFile; ALoadStyles: Boolean);

  function GetCustomItem(ACollection: TdxNavBarCustomItems; const ASection: string): TdxNavBarCustomItem;
  var
    AItemIdentifier: string;
  begin
    Result := nil;
    AItemIdentifier := AStorage.ReadString(ASection, 'Name', '');
    if AItemIdentifier <> '' then
      Result := TdxNavBarCustomItem(ACollection.ItemByName(AItemIdentifier))
    else
    begin
      AItemIdentifier := AStorage.ReadString(ASection, 'Caption', '');
      if AItemIdentifier <> '' then
        Result := ACollection.ItemByCaption(AItemIdentifier);
    end;
    if Result = nil then
      Result := ACollection.Add;
  end;

  procedure ReadItem(AIndex: Integer);
  var
    AItem: TdxNavBarItem;
    ASection: string;
  begin
    ASection := 'Item' + IntToStr(AIndex);
    AItem := TdxNavBarItem(GetCustomItem(Self, ASection));
    AItem.LoadFromIniFile(AStorage, ASection, ALoadStyles);
  end;

var
  ACount, I: Integer;
begin
  if AStorage.ValueExists('Layout', 'ItemCount') then
  begin
    ACount := AStorage.ReadInteger('Layout', 'ItemCount', Count);
    for I := 0 to ACount - 1 do
      ReadItem(I);
  end;
end;

function TdxNavBarItems.GetItem(Index: Integer): TdxNavBarItem;
begin
  Result := inherited GetItem(Index) as TdxNavBarItem;
end;

procedure TdxNavBarItems.Notify(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  if AAction in [ccnExtracting, ccnDeleting] then
    (AItem as TdxNavBarItem).RemoveAllLinks;
end;

procedure TdxNavBarItems.SetItem(Index: Integer; Value: TdxNavBarItem);
begin
  inherited SetItem(Index, Value);
end;

initialization
  RegisterClasses([TdxNavBarItemLink, TdxNavBarItemLinks,  TdxNavBarGroupControl, TdxNavBarGroup, TdxNavBarItem]);

end.
