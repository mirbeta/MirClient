{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uTreeIntf;

interface

uses
  Windows, CommCtrl, ActiveX, ComObj, Messages, SysUtils, Classes, Graphics,
  Variants, Controls, Forms, Dialogs, ExtCtrls, ComCtrls, uDesignIntf,
  uDesignEditors, uDesignMenus, TypInfo, Contnrs, IniFiles, Menus, ImgList,
  uStructureViewAPI, AppEvnts;

type

{ TSprig }

{ sprig \Sprig\, n.
  [AS. sprec; akin to Icel. sprek a stick. Cf. Spray a branch.]
  1. A small shoot or twig of a tree or other plant; a spray; as, a sprig of
     laurel or of parsley.
  2. A youth; a lad; -- used humorously or in slight disparagement.
     A sprig whom I remember, with a whey-face and a satchel, not so many
     years ago. --Sir W. Scott.
  3. A brad, or nail without a head.
  4. (Naut.) A small eyebolt ragged or barbed at the point.
  5. A leaf in Delphi's object treeview
  Source: Webster's Revised Unabridged Dictionary, well sort of anyway }

  TSprig = class;
  TSprigClass = class of TSprig;
  TSprigAction = procedure(AItem: TSprig) of object;
  TSprigIndex = class;
  TRootSprig = class;
  TRootSprigClass = class of TRootSprig;
  TSprigTreeNode = class;
  ISprigDesigner = interface;

  ISprigCollection = interface
    ['{0B6ABAEE-E1A4-4DAC-8E20-C6B741A5082D}']
    function RootSprigAssigned: Boolean;
    function RootSprig: TRootSprig;
    function GetSprigDesigner: ISprigDesigner;
    procedure SetSprigDesigner(const ASprigDesigner: ISprigDesigner);
    property SprigDesigner: ISprigDesigner read GetSprigDesigner write SetSprigDesigner;
  end;

  ISprigDesigner = interface
    ['{6AC141E3-2FBE-425E-B299-AB29E7DF3FBB}']
    function GetTreeView: TCustomTreeView;
    procedure BeforeItemsModified;
    procedure AfterItemsModified;
    function GetRootSprig: TRootSprig;
    procedure SetRootSprig(ARootSprig: TRootSprig);
    property RootSprig: TRootSprig read GetRootSprig write SetRootSprig;
  end;

  ISprig = interface
    ['{156098B8-0BCD-4550-8174-37B3C38918F4}']
    function GetSprig: TSprig;
  end;

  TInformant = class(TObject)
  private
    FNotifyList: TList;
    FDisableNotify: Integer;
    FNotifyNeeded: Boolean;
    FDestroying: Boolean;
  protected
    procedure Changed(AObj: TInformant); virtual;
  public
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    property Destroying: Boolean read FDestroying;

    procedure DisableNotify;
    procedure EnableNotify;
    procedure Notification;
    procedure Notify(AObj: TInformant);
    procedure Unnotify(AObj: TInformant);
  end;

  TSprigStructureNode = class(TInterfacedObject, IDispatch, ISupportErrorInfo,
    IOTAStructureNode, IOTASortableStructureNode, ISprig)
  private
    FSprig: TSprig;
    FData: Integer;
    FSelected, FFocused: Boolean;
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
    { IOTAStructureNode }
    function AddChildNode(const ANode: IOTAStructureNode; Index: Integer = -1): Integer; safecall;
    function get_Caption: WideString; safecall;
    function get_ChildCount: Integer; safecall;
    function get_Child(Index: Integer): IOTAStructureNode; safecall;
    function get_Data: Integer; safecall;
    function get_EditCaption: WideString; safecall;
    function get_Expanded: WordBool; safecall;
    function get_Focused: WordBool; safecall;
    function get_Hint: WideString; safecall;
    function get_ImageIndex: Integer; safecall;
    function get_Name: WideString; safecall;
    function get_Parent: IOTAStructureNode; safecall;
    function get_Selected: WordBool; safecall;
    function get_StateIndex: Integer; safecall;
    procedure RemoveChildNode(Index: Integer); safecall;
    procedure set_Data(Value: Integer); safecall;
    procedure set_Expanded(Value: WordBool); safecall;
    procedure set_Focused(Value: WordBool); safecall;
    procedure set_Selected(Value: WordBool); safecall;
    { IOTASortableStructureNode }
    function Get_SortByIndex: WordBool; safecall;
    function Get_ItemIndex: Integer; safecall;
    { ISprig }
    function GetSprig: TSprig;
  public
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HRESULT; override;
  public
    constructor Create(ASprig: TSprig);
    procedure ClearSprig;
  end;

  TSprigDeleteStyle = (dsNormal, dsIgnore, dsAbort, dsCustom);

  TSprig = class(TInformant)
  private
    FRoot: TRootSprig;
    FParent: TSprig;
    FList: TObjectList;
    FItem: TPersistent;
    FTreeNode: TTreeNode;
    FImageIndex: TImageIndex;
    FCaption: string;
    FExpanded, FInvalid, FCollectionsDone, FHidden, FHiddenTested: Boolean;
    FSprigStructureNode: TSprigStructureNode;
    FStructureNode: IOTAStructureNode;
    procedure SetExpanded(const Value: Boolean);
  protected
    function GetItem(Index: Integer): TSprig;
    function UniqueName: string; virtual;
    function CaptionFor(const AName: string; const ALabel: string = '';
      const AClass: string = ''): string;
    procedure ReparentChildren;
    procedure SelectItems(const AItems: array of TPersistent; ARuntimeChange: Boolean = True); virtual;
    procedure RuntimeChange; virtual;
    procedure DesigntimeChange; virtual;
    function FindItem(AItem: TPersistent; Recurse: Boolean): TSprig; virtual;
    function FindItemByName(const AName: string; AClass: TClass; Recurse: Boolean): TSprig; virtual;
    function FindItemByPath(const APath: string; Recurse: Boolean = True): TSprig; virtual;
    function GetDesigner(out ADesigner: IDesigner): Boolean; virtual;
    function GetImageIndex: TImageIndex; virtual;
    procedure SetImageIndex(const Value: TImageIndex); virtual;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    function GetStateIndex: TImageIndex; virtual;
    procedure BeginUpdate; virtual;
    procedure EnsureUpdate; virtual;
    procedure EndUpdate; virtual;
    function GetAddType(Index: Integer): string; virtual;
  public
    constructor Create(AItem: TPersistent); overload; virtual;
    destructor Destroy; override;
    procedure Invalidate;

    function Transient: Boolean; virtual;
    function AnyProblems: Boolean; virtual;
    property Invalid: Boolean read FInvalid;

    property Item: TPersistent read FItem;
    function Hidden: Boolean; virtual;
    function Ghosted: Boolean; virtual;
    function FocusItem: TPersistent; virtual;
    function ItemClass: TClass; virtual;
    function Owner: TSprig; virtual;

    procedure VisualRefresh; virtual;
    function StructureNodeFor: IOTAStructureNode; virtual;
    property StructureNode: IOTAStructureNode read FStructureNode;
    function TreeNodeFor(ATreeView: TCustomTreeView): TTreeNode; virtual;
    property TreeNode: TTreeNode read FTreeNode;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
    property StateIndex: TImageIndex read GetStateIndex;
    procedure ClearTreeNode; overload;
    procedure ClearTreeNode(ARecurse: Boolean; AFreeNode: Boolean = True); overload;

    function Name: string; virtual;
    function Caption: string; virtual;
    function Hint: string; virtual;

    procedure PrepareMenu(const AItems: IMenuItems); virtual;
    function ShowRegisteredMenus: Boolean; virtual;

    function DragClass: TClass;
    function DragOver(AItem: TSprig): Boolean; virtual;
    function DragOverTo(AParent: TSprig): Boolean; virtual;
    function DragDrop(AItem: TSprig): Boolean; virtual;
    function DragDropTo(AParent: TSprig): Boolean; virtual;
    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; virtual;
    class function PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean; virtual;

    function Add(AItem: TSprig): TSprig;
    function Find(AItem: TPersistent; Recurse: Boolean = True): TSprig; overload;
    function Find(const AName: string; Recurse: Boolean = True): TSprig; overload;
    function Find(const AName: string; AClass: TClass; Recurse: Boolean = True): TSprig; overload;
    function FindPath(const APath: string; Recurse: Boolean = True): TSprig;
    function IndexOf(AItem: TSprig): Integer;
    procedure SetIndexOf(AItem: TSprig; Index: Integer);
    procedure ForEach(ABefore: TSprigAction; AAfter: TSprigAction = nil);
    procedure ClearUnneededSprigs;

    function DeleteStyle: TSprigDeleteStyle; virtual;
    function CustomDelete: Boolean; virtual;

    function CanMove(AUp: Boolean): Boolean; virtual;
    function Move(AUp: Boolean): Boolean; virtual;
    function CanAdd: Boolean; virtual;
    function AddTypeCount: Integer; virtual;
    property AddTypes[Index: Integer]: string read GetAddType;
    procedure AddType(Index: Integer); virtual;

    procedure SortItems; virtual;
    function SortByIndex: Boolean; virtual;
    function IncludeIndexInCaption: Boolean; virtual;
    function ItemIndex: Integer; virtual;
    function CopyGlyph(ABitmap: TBitmap): Boolean; virtual;

    property Root: TRootSprig read FRoot;
    property Parent: TSprig read FParent;
    function Parents(ASprig: TSprig): Boolean;
    function Path: string;
    property Items[Index: Integer]: TSprig read GetItem; default;
    function Count: Integer;
    property Index: Integer read GetIndex write SetIndex;
    procedure Reparent; virtual;
    function Construct(AClass: TComponentClass): TComponent; virtual;

    function SeekParent(AItem: TPersistent; Recurse: Boolean = True): TSprig; overload;
    function SeekParent(const AName: string; Recurse: Boolean = True): TSprig; overload;
    function SeekParent(const AName: string; AClass: TClass; Recurse: Boolean = True): TSprig; overload;
    class function ParentProperty: string; virtual;
    procedure FigureParent; virtual;
    procedure FigureChildren; virtual;

    function ParentOrphanedSprigs: Boolean; virtual;
  end;

  // a sprig that represents something that doesn't actually exist
  TAbstractSprig = class(TSprig)
  public
    function Ghosted: Boolean; override;
  end;

  // an abstract sprig that only exists if it has children
  TTransientSprig = class(TAbstractSprig)
  public
    function Transient: Boolean; override;
  end;

  // collection variants of the above
  TAbstractCollectionSprig = class(TAbstractSprig)
  public
    constructor Create(AItem: TPersistent); override;
  end;
  TTransientCollectionSprig = class(TTransientSprig)
  public
    constructor Create(AItem: TPersistent); override;
  end;

  // a sprig that points to a persistent
  TPersistentSprig = class(TSprig)
  end;

  // a sprig that points to a component
  TComponentSprig = class(TPersistentSprig)
  private
    FOwner: TSprig;
  public
    constructor Create(AItem: TPersistent); override;
    constructor Create(AItem: TPersistent; AOwner: TSprig); overload;
    function UniqueName: string; override;
    function Owner: TSprig; override;
    //function ShowRegisteredMenus: Boolean; override;
    // TSprig's implimentation of FigureParent is TComponent aware
  end;
  TComponentSprigClass = class of TComponentSprig;

  TRootSprigStructureContext = class(TSprigStructureNode, IDispatch,
    ISupportErrorInfo, IOTAStructureContext, IOTAStructureNode,
    IDesignNotification, IOTADragStructureContext,
    IOTAStructureContextEditActions, IOTAStructureContextKeyHandler,
    IOTAStructureContextToolBar, IOTAStructureNodeStatePreserver, IOTAStructureContext110)
  private
    FRootSprig: TRootSprig;
    FSelectLocks: Integer;
    FPopup: IPopupMenu;
    FUpdateEditState: Boolean;
    FLastEditState: TEditState;
    FPendingClearSelection: Boolean;
    FAppEvents: TApplicationEvents;
    procedure HandleAddType(Sender: TObject);
    procedure LockSelect;
    function SelectLocked: Boolean;
    procedure UnlockSelect;
    procedure StructureChanged(const ADesigner: IDesigner);
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure SetPendingClearSelection(AValue: Boolean);
 protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; overload; stdcall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
    { IOTAStructureContext }
    function get_ContextIdent: WideString; safecall;
    function get_StructureType: WideString; safecall;
    function get_ViewOptions: Integer; safecall;
    procedure AddRootNode(const ANode: IOTAStructureNode; Index: Integer); safecall;
    procedure RemoveRootNode(const ANode: IOTAStructureNode); safecall;
    function Get_RootNodeCount: Integer; safecall;
    function GetRootStructureNode(Index: Integer): IOTAStructureNode; safecall;
    procedure NodeEdited(const Node: IOTAStructureNode); safecall;
    procedure NodeFocused(const Node: IOTAStructureNode); safecall;
    procedure NodeSelected(const Node: IOTAStructureNode); safecall;
    procedure DefaultNodeAction(const Node: IOTAStructureNode); safecall;
    function SameContext(const AContext: IOTAStructureContext): WordBool; safecall;
    procedure InitPopupMenu(const Node: IOTAStructureNode;
      const PopupMenu: IOTAStructureNodeMenuItem); safecall;
    { IOTAStructureContex110 }
    procedure ContextActivated; safecall;
    { IOTAStructureNode }
    function get_ChildCount: Integer; safecall;
    function get_Child(Index: Integer): IOTAStructureNode; safecall;
    { IOTADragStructureContext }
    function DragAllowed(const Node: IOTAStructureNode): WordBool; safecall;
    procedure DragDrop(const Node: IOTAStructureNode;
      DataObject: OleVariant; const FormatArray: WideString;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer); safecall;
    function DragOver(const Node: IOTAStructureNode;
      DataObject: OleVariant; State: TOTADragState;
      X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer): WordBool; safecall;
    function GetDataObject: OleVariant; safecall;
    { IOTAStructureContextEditActions }
    function EditAction(Action: Integer): WordBool; safecall;
    function GetEditState: Integer; safecall;
    { IOTAStructureContextKeyHandler }
    procedure KeyDown(const Node: IOTAStructureNode; KeyState: Integer; var KeyCode: Word); safecall;
    procedure KeyPressed(const Node: IOTAStructureNode; KeyState: Integer; var KeyChar: Word); safecall;
    { IOTAStructureContextToolbar }
    function Get_ButtonCount: Integer; safecall;
    function GetButtonCaption(Index: Integer): WideString; safecall;
    function GetButtonEnabled(Index: Integer): WordBool; safecall;
    function GetButtonEnableDropDown(Index: Integer): WordBool; safecall;
    function GetButtonHasDropDown(Index: Integer): WordBool; safecall;
    function GetButtonHint(Index: Integer): WideString; safecall;
    function GetButtonImageIndex(Index: Integer): Integer; safecall;
    function GetButtonMenu(Index: Integer): IOTAStructureNodeMenuItem; safecall;
    function GetButtonSeparator(Index: Integer): WordBool; safecall;
    function GetButtonVisible(Index: Integer): WordBool; safecall;
    procedure Invoke(Index: Integer); overload; safecall;
    { IOTAStructureNodeStatePreserver }
    function Get_NodePreservationMode: TOTANodePreservationMode; safecall;
    procedure PreserveNodeStates; safecall;
    procedure RestoreNodeState(const Node: IOTAStructureNode); safecall;
    { IDesignNotification }
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
  public
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HRESULT; override;
  public
    constructor Create(ARootSprig: TRootSprig);
    destructor Destroy; override;

    procedure ClearRootSprig;
  end;

  TRootSprig = class(TPersistentSprig)
  private
    FIndex: TSprigIndex;
    FNamedItems,
    FPathedItems: TList;
    FRepopulating,
    FParentChanges: Boolean;
    FSprigDesigner: ISprigDesigner;
    FDesigner: IDesigner;
    FRepopulateNeeded: Boolean;
    FNeedUpdate: Boolean;
    FUpdateLocks: Integer;
    FRootStructureContext: TRootSprigStructureContext;
    FStructureContext: IOTAStructureContext;
    procedure ValidateParent(AItem: TSprig);
    procedure PreRefreshTreeView(AItem: TSprig);
    procedure PostRefreshTreeView(AItem: TSprig);
    procedure DepopulateTreeView(AItem: TSprig);
    //procedure RestoreExpandState(AItem: TSprig);
    procedure StoreExpandState(AItem: TSprig);
    procedure SetSprigDesigner(const ASprigDesigner: ISprigDesigner);
    procedure SelectionSurvey(out ADeleteStyle: TSprigDeleteStyle; out AAllVisible: Boolean);
  protected
    function FindItem(AItem: TPersistent; Recurse: Boolean = True): TSprig; override;
    function FindItemByName(const AName: string; AClass: TClass; Recurse: Boolean): TSprig; override;
    function FindItemByPath(const APath: string; Recurse: Boolean = True): TSprig; override;
    procedure AddItem(ASprig: TSprig);
    procedure RemoveItem(ASprig: TSprig);
    function GetDesigner(out ADesigner: IDesigner): Boolean; override;
    function GetAddType(Index: Integer): String; override;
    function SelectedSprig(var ASprig: TSprig): Boolean;
  public
    constructor Create(AItem: TPersistent); override;
    destructor Destroy; override;
    procedure FigureParent; override;

    property SprigDesigner: ISprigDesigner read FSprigDesigner write SetSprigDesigner;
    property Designer: IDesigner read FDesigner write FDesigner;

    property Repopulating: Boolean read FRepopulating;
    function Repopulate: Boolean;
    function TreeView: TCustomTreeView;
    procedure RefreshTreeView;
    procedure StoreTreeState;
    procedure BeginUpdate; override;
    procedure EnsureUpdate; override;
    procedure EndUpdate; override;

    procedure ItemDeleted(AItem: TPersistent);
    procedure ItemInserted;
    procedure ItemsModified(AForceRepopulate: Boolean = True);

    procedure RuntimeChange; override;
    procedure DesigntimeChange; override;
    procedure SelectItems(const AItems: array of TPersistent; ARuntimeChange: Boolean = True); override;

    function StructureNodeFor: IOTAStructureNode; override;
    function StructureContextFor: IOTAStructureContext;

    // these are not used to operate on the root but its children
    function CanMove(AUp: Boolean): Boolean; override;
    function Move(AUp: Boolean): Boolean; override;
    function CanAdd: Boolean; override;
    procedure AddType(Index: Integer); override;
    function AddTypeCount: Integer; override;

    function EditAction(Action: TEditAction): Boolean;
    function GetEditState: TEditState;

    function DeleteStyle: TSprigDeleteStyle; override;

    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; override;
    function AcceptsClass(AClass: TClass): Boolean; virtual;

    property RepopulateNeeded: Boolean read FRepopulateNeeded write FRepopulateNeeded;
  end;

  TSprigTreeNode = class(TTreeNode)
  public
    destructor Destroy; override;
  end;

  TSprigIndex = class(TObject)
  private
    FList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ASprig: TSprig);
    procedure Remove(ASprig: TSprig);
    function Find(AItem: TPersistent): TSprig;
  end;

  TPropertySprig = class(TPersistentSprig)
  public
    function Ghosted: Boolean; override;
    function DeleteStyle: TSprigDeleteStyle; override;
  end;

  TCollectionSprig = class(TPropertySprig)
  private
    FPropName: string;
    FOwner: TSprig;
  protected
    function GetAddType(Index: Integer): string; override;
  public
    constructor Create(AItem: TPersistent); override;
    function Name: string; override;
    function Caption: string; override;
    procedure FigureParent; override;
    procedure FigureChildren; override;
    function Owner: TSprig; override;

    function SortByIndex: Boolean; override;
    procedure AddType(Index: Integer); override;
    function AddTypeCount: Integer; override;

    function DeleteStyle: TSprigDeleteStyle; override;
    function CustomDelete: Boolean; override;
  end;

  TCollectionItemSprig = class(TPersistentSprig)
  private
    FOwner: TSprig;
  protected
    function GetAddType(Index: Integer): string; override;
  public
    function Name: string; override;
    procedure FigureParent; override;
    function Owner: TSprig; override;
    function Ghosted: Boolean; override;
    function ItemIndex: Integer; override;
    function IncludeIndexInCaption: Boolean; override;
    function DragOverTo(AParent: TSprig): Boolean; override;
    function DragDropTo(AParent: TSprig): Boolean; override;
    procedure AddType(Index: Integer); override;
    function AddTypeCount: Integer; override;
  end;

  TSprigType = class(TObject)
  private
    FGroup: Integer;
    FClass: TClass;
    FSprigClass: TSprigClass;
  public
    constructor Create(const AClass: TClass; const ASprigClass: TSprigClass);
    function Score(const AClass: TClass): Integer;
    property SprigClass: TSprigClass read FSprigClass;
  end;

  TGUIDArray = array of TGUID;

  TSprigIntfType = class(TObject)
  private
    FGroup: Integer;
    FInterfaces: TGUIDArray;
    FSprigClass: TSprigClass;
  public
    constructor Create(const AInterfaces: TGUIDArray; const ASprigClass: TSprigClass);
    function Match(const AClass: TClass): Boolean;
    property SprigClass: TSprigClass read FSprigClass;
  end;

  TSprigTypeList = class(TObject)
  private
    FList: TObjectList;
    FLastClass: TClass;
    FLastSprigClass: TSprigClass;

    FInterfaceList: TObjectList;
  protected
    procedure ClearCache;
    function MatchCache(const AClass: TClass): TSprigClass;
    function MatchClass(const AClass: TClass): TSprigClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Match(const AClass: TClass): TSprigClass;
    procedure Add(const AClass: TClass; const ASprigClass: TSprigClass); overload;
    procedure Add(const AInterfaces: TGUIDArray; const ASprigClass: TSprigClass); overload;
    procedure FreeEditorGroup(AGroup: Integer);
  end;

  TDragSprigs = class(TDragControlObjectEx)
  private
    FSprigs: TList;
    function GetSprig(Index: Integer): TSprig;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    procedure Add(ASprig: TSprig);
    function Count: Integer;
    property Sprigs[Index: Integer]: TSprig read GetSprig;
  end;

procedure RegisterSprigType(const AClass: TClass; ASprigClass: TSprigClass); overload;
procedure RegisterSprigType(const AInterfaces: TGUIDArray; ASprigClass: TSprigClass); overload;

function FindBestSprigClass(AClass: TClass): TSprigClass; overload;
function FindBestSprigClass(AClass: TClass; AMinimumSprigClass: TSprigClass): TSprigClass; overload;

procedure RegisterRootSprigType(const AClass: TClass; ASprigClass: TRootSprigClass); overload;
procedure RegisterRootSprigType(const AInterfaces: TGUIDArray; ASprigClass: TRootSprigClass); overload;

function FindBestRootSprigClass(AClass: TClass): TRootSprigClass; overload;
function FindBestRootSprigClass(AClass: TClass; AMinimumSprigClass: TRootSprigClass): TRootSprigClass; overload;

var
  GShowClassNameInTreeView: Boolean = False;
  GImageOffset: TImageIndex = -1;
  GStateImageOffset: TImageIndex = -1;
  GToolButtonImageOffset: TImageIndex = -1;
  GSprigClipboardFormat: Integer;

const
  CFakeSprigImage = 0;
  CFakeCollectionSprigImage = 1;
  CPersistentSprigImage = 2;
  CCollectionSprigImage = 3;
  CComponentSprigImage = 4;
  CDataModuleSprigImage = 5;
  CControlSprigImage = 6;
  CUIControlSprigImage = 7;
  CUIContainerSprigImage = 8;
  CFormSprigImage = 9;
  CGhostedOffset = 10;

  CNoStateImage = 0;
  CCheckOutStateImage = 1;

  CCollectionName = '<Collection.%s>'; // DO NOT LOCALIZE
  CSprigClipboardFormat = 'Borland.Sprig'; // DO NOT LOCALIZE

const
  CUIControlImageIndex: array [Boolean] of Integer = (CUIControlSprigImage,
                                                      CUIContainerSprigImage);

type
  TRootSprigList = class(TObject)
  private
    FList: TBucketList;
  public
    constructor Create;
    destructor Destroy; override;
    function FindRoot(const ADesigner: IDesigner; out ARootSprig: TRootSprig): Boolean;

    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
  end;

function RootSprigList: TRootSprigList;
procedure InitDesignNotification;
procedure DoneDesignNotification;

type
  TCopySprigGlyphFunc = function(ASprig: TSprig; ABitmap: TBitmap): Boolean of object;

var
  CopySprigGlyphFunc: TCopySprigGlyphFunc;

implementation

uses
  Math, uToolsAPI, uDesignConst;

type
  TDesignNotificationHandler = class(TInterfacedObject, IDesignNotification)
  protected
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections);
  end;

  IDragSprigs = interface
    ['{FBBF3081-5D19-43DB-81C3-86F534A2BF36}']
    function Add(ASprig: TSprig): Integer;
    function GetCount: Integer;
    function GetSprig(Index: Integer): TSprig;
    procedure Remove(ASprig: TSprig);

    property Count: Integer read GetCount;
    property Sprigs[Index: Integer]: TSprig read GetSprig;
  end;

  TInternalDragSprigs = class(TStreamAdapter, IDragSprigs)
  private
    FDragSprigs: TList;
  protected
    function Add(ASprig: TSprig): Integer;
    function GetCount: Integer;
    function GetSprig(Index: Integer): TSprig;
    procedure Remove(ASprig: TSprig);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TStructureMenuItem = class(TInterfacedObject, IDispatch, ISupportErrorInfo,
    IOTAStructureNodeMenuItem)
  private
    FItem: IMenuItem;
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; overload; stdcall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
    { IOTAStructureNodeMenuItem }
    procedure DeleteItem(Index: Integer); safecall;
    function get_Caption: WideString; safecall;
    function get_Checked: WordBool; safecall;
    function get_Count: Integer; safecall;
    function get_Enabled: WordBool; safecall;
    function get_GroupIndex: Integer; safecall;
    function get_ImageIndex: Integer; safecall;
    function get_Item(Index: Integer): IOTAStructureNodeMenuItem; safecall;
    function get_Name: WideString; safecall;
    function get_RadioItem: WordBool; safecall;
    function get_ShortCut: Integer; safecall;
    function get_Visible: WordBool; safecall;
    procedure InsertItem(Index: Integer; const Item: IOTAStructureNodeMenuItem); safecall;
    procedure Invoke; overload; safecall;
  public
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HRESULT; override;    
  public
    constructor Create(const AItem: IMenuItem);
  end;

var
  InternalSprigTypeList: TSprigTypeList = nil;
  InternalRootSprigTypeList: TSprigTypeList = nil;

procedure RegisterSprigType(const AClass: TClass; ASprigClass: TSprigClass);
begin
  if InternalSprigTypeList = nil then
    InternalSprigTypeList := TSprigTypeList.Create;
  InternalSprigTypeList.Add(AClass, ASprigClass);
end;

procedure RegisterSprigType(const AInterfaces: TGUIDArray; ASprigClass: TSprigClass);
begin
  if InternalSprigTypeList = nil then
    InternalSprigTypeList := TSprigTypeList.Create;
  InternalSprigTypeList.Add(AInterfaces, ASprigClass);
end;


function FindBestSprigClass(AClass: TClass): TSprigClass;
begin
  Result := FindBestSprigClass(AClass, TSprig);
end;

function FindBestSprigClass(AClass: TClass;
  AMinimumSprigClass: TSprigClass): TSprigClass;
begin
  Result := nil;
  if InternalSprigTypeList <> nil then
  begin
    Result := InternalSprigTypeList.Match(AClass);
    if (Result <> nil) and not Result.InheritsFrom(AMinimumSprigClass) then
      Result := nil;
  end;
end;

procedure RegisterRootSprigType(const AClass: TClass; ASprigClass: TRootSprigClass);
begin
  if InternalRootSprigTypeList = nil then
    InternalRootSprigTypeList := TSprigTypeList.Create;
  InternalRootSprigTypeList.Add(AClass, ASprigClass);
end;

procedure RegisterRootSprigType(const AInterfaces: TGUIDArray; ASprigClass: TRootSprigClass);
begin
  if InternalRootSprigTypeList = nil then
    InternalRootSprigTypeList := TSprigTypeList.Create;
  InternalRootSprigTypeList.Add(AInterfaces, ASprigClass);
end;


function FindBestRootSprigClass(AClass: TClass): TRootSprigClass;
begin
  Result := FindBestRootSprigClass(AClass, TRootSprig);
end;

function FindBestRootSprigClass(AClass: TClass;
  AMinimumSprigClass: TRootSprigClass): TRootSprigClass;
begin
  Result := nil;
  if InternalRootSprigTypeList <> nil then
  begin
    Result := TRootSprigClass(InternalRootSprigTypeList.Match(AClass));
    if (Result <> nil) and not Result.InheritsFrom(AMinimumSprigClass) then
      Result := nil;
  end;
end;

procedure FlushSprigTypes(AGroup: Integer);
begin
  if InternalRootSprigTypeList <> nil then
    InternalRootSprigTypeList.FreeEditorGroup(AGroup);
  if InternalSprigTypeList <> nil then
    InternalSprigTypeList.FreeEditorGroup(AGroup);
end;

{ TInformant }

procedure TInformant.BeforeDestruction;
begin
  FDestroying := True;
  Notification;
  inherited;
end;

procedure TInformant.Changed(AObj: TInformant);
begin
  if AObj.Destroying then
    AObj.Unnotify(Self);
end;

destructor TInformant.Destroy;
begin
  FreeAndNil(FNotifyList);
  inherited;
end;

procedure TInformant.DisableNotify;
begin
  Inc(FDisableNotify);
end;

procedure TInformant.EnableNotify;
begin
  Dec(FDisableNotify);
  if (FDisableNotify = 0) and FNotifyNeeded then
    Notification;
end;

procedure TInformant.Notification;
var
  I: Integer;
begin
  if (FDisableNotify = 0) and (FNotifyList <> nil) then
  begin
    for I := FNotifyList.Count - 1 downto 0 do
      TInformant(FNotifyList[I]).Changed(Self);
    FNotifyNeeded := False;
  end
  else
    FNotifyNeeded := True;
end;

procedure TInformant.Notify(AObj: TInformant);
begin
  if FNotifyList = nil then
    FNotifyList := TList.Create;

  if FNotifyList.IndexOf(AObj) = -1 then
  begin
    FNotifyList.Add(AObj);
    AObj.Notify(Self);
  end;
end;

procedure TInformant.Unnotify(AObj: TInformant);
var
  I: Integer;
begin
  if FNotifyList <> nil then
  begin
    I := FNotifyList.IndexOf(AObj);
    if I <> -1 then
    begin
      FNotifyList.Delete(I);
      AObj.Unnotify(Self);
    end;
    if (FNotifyList <> nil) and
       (FNotifyList.Count = 0) then
      FreeAndNil(FNotifyList);
  end;
end;

{ TSprig }

function TSprig.Add(AItem: TSprig): TSprig;
var
  LIndex: Integer;
begin
  // hey is it already in us?
  if (AItem.Parent <> Self) and
     (not AItem.Parents(Self)) then
  begin

    // remove the item from its old parent and clear any tree nodes it may have
    if (AItem.Parent <> nil) and
       (AItem.Parent.FList <> nil) then
    begin
      AItem.ClearTreeNode;
      AItem.Parent.FList.Extract(AItem);
    end;

    // make sure we have a list
    if not Assigned(FList) then
      FList := TObjectList.Create;

    // add it to our list
    if SortByIndex then
    begin
      LIndex := 0;
      while (LIndex < Count) and (Items[LIndex].ItemIndex < AItem.ItemIndex) do
        Inc(LIndex);
    end else
      LIndex := Count;
    FList.Insert(LIndex, AItem);

    // populate its parent
    AItem.FParent := Self;

    // populate the root?
    if AItem.Root = nil then
    begin
      AItem.FRoot := Root;

      // add it to the root's index?
      if Root <> nil then
        Root.AddItem(AItem);
    end;

    // we changed something!
    Root.FRepopulateNeeded := True;

    {AItem.FRoot := Root;
    if (AItem.Root = nil) and
       (Self is TRootSprig) then
      AItem.FRoot := TRootSprig(Self);

    // remove ourselve from the root index
    if AItem.FRoot <> nil then
      AItem.FRoot.FIndex.Add(Self);}
  end;

  // return it
  Result := AItem;
end;

function TSprig.AnyProblems: Boolean;
var
  LProp: PPropInfo;
begin
  Result := False;
  if ParentProperty <> '' then
  begin
    LProp := GetPropInfo(Item, ParentProperty, [tkClass]);
    Result := (LProp = nil) or
              (GetObjectProp(Item, LProp, TPersistent) = nil);
  end;
end;

function TSprig.Caption: string;
begin
  Result := CaptionFor(Name);
  if IncludeIndexInCaption then
    Result := Format('%d - %s', [ItemIndex, Result]); // DO NOT LOCALIZE
end;

procedure TSprig.ClearTreeNode(ARecurse, AFreeNode: Boolean);
var
  I: Integer;
  LNode: TTreeNode;
begin
  EnsureUpdate;

  // first do our children
  if ARecurse then
    for I := Count - 1 downto 0 do
      Items[I].ClearTreeNode(True);

  // now do ourself
  if TreeNode <> nil then
  begin
    LNode := TreeNode;
    FTreeNode := nil;
    LNode.Data := nil;
    if AFreeNode and not (csDestroying in LNode.TreeView.ComponentState) then
      LNode.Delete;
  end;
end;

procedure TSprig.ClearTreeNode;
begin
  ClearTreeNode(True, True);
end;

procedure TSprig.ClearUnneededSprigs;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with Items[I] do
    begin
      ClearUnneededSprigs;
      if (Transient and (Count = 0)) or
         Invalid then
        Free;
    end;
end;

function TSprig.Count: Integer;
begin
  Result := 0;
  if Assigned(FList) then
    Result := FList.Count;
end;

constructor TSprig.Create(AItem: TPersistent);
const
  CImageIndex: array [Boolean] of TImageIndex = (CFakeSprigImage, CPersistentSprigImage);
begin
  inherited Create;
  FItem := AItem;
  FHiddenTested := Item = nil;
  FHidden := Item = nil;
  ImageIndex := CImageIndex[Item <> nil];
end;

destructor TSprig.Destroy;
begin
  // just in case it hasn't happen already
  Invalidate;

  // we know nothing!
  FItem := nil;

  // remove ourselves from the tree
  ClearTreeNode(False);

  // remove ourselves from the parent
  if (Parent <> nil) and (Parent.FList <> nil) then
    Parent.FList.Extract(Self);
  FParent := nil;

  // wipe out the lists
  if FList <> nil then
  begin
    while FList.Count > 0 do
      FList.Last.Free;
    FreeAndNil(FList);
  end;

  if FSprigStructureNode <> nil then
    FSprigStructureNode.ClearSprig;
  FStructureNode := nil;
  // remove ourselves
  inherited;
end;

function TSprig.DragClass: TClass;
begin
  if Item <> nil then
    Result := Item.ClassType
  else
    Result := ClassType;
end;

function TSprig.DragDrop(AItem: TSprig): Boolean;
begin
  Result := False;
  if Assigned(AItem) then
    Result := AItem.DragDropTo(Self);
end;

function TSprig.DragDropTo(AParent: TSprig): Boolean;
var
  LProp: PPropInfo;
begin
  Result := False;
  if (ParentProperty <> '') and
     (AParent <> Parent) then
  begin
    LProp := GetPropInfo(Item, ParentProperty, [tkClass]);
    if LProp <> nil then
    begin
      if AParent is TRootSprig then
        SetObjectProp(Item, LProp, nil)
      else
        SetObjectProp(Item, LProp, AParent.Item);
      Result := True;
    end;
  end;
end;

function TSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := False;
  if AItem <> nil then
    Result := AItem.DragOverTo(Self);
end;

function TSprig.DragOverTo(AParent: TSprig): Boolean;
var
  LProp: PPropInfo;
begin
  Result := False;
  if ParentProperty <> '' then
  begin
    LProp := GetPropInfo(Item, ParentProperty, [tkClass]);
    if LProp <> nil then
      Result := (AParent is TRootSprig) or
                (AParent.Item is GetTypeData(LProp^.PropType^)^.ClassType);
  end;
end;

function TSprig.Find(AItem: TPersistent; Recurse: Boolean): TSprig;
begin
  Result := FindItem(AItem, Recurse);
end;

function TSprig.FindItem(AItem: TPersistent; Recurse: Boolean): TSprig;
var
  I: Integer;
  LItem: TSprig;
begin
  Result := nil;
  if AItem <> nil then
    if AItem = Item then
      Result := Self
    else
      for I := 0 to Count - 1 do
      begin
        LItem := Items[I];
        if LItem.Item = AItem then
        begin
          Result := LItem;
          Break;
        end
        else if Recurse then
        begin
          Result := LItem.FindItem(AItem, True);
          if Result <> nil then
            Break;
        end;
      end;
end;

function TSprig.Find(const AName: string; Recurse: Boolean): TSprig;
begin
  Result := FindItemByName(AName, nil, Recurse);
end;

function TSprig.Find(const AName: string; AClass: TClass; Recurse: Boolean): TSprig;
begin
  Result := FindItemByName(AName, AClass, Recurse);
end;

function TSprig.FindItemByName(const AName: string; AClass: TClass; Recurse: Boolean): TSprig;
var
  I: Integer;
  LItem: TSprig;
begin
  Result := nil;
  if AName <> '' then

    // if class is nil then just check name
    if AClass = nil then
    begin
      if AnsiSameText(Name, AName) then
        Result := Self
      else
        for I := 0 to Count - 1 do
        begin
          LItem := Items[I];
          if AnsiSameText(LItem.Name, AName) then
          begin
            Result := LItem;
            Break;
          end
          else if Recurse then
          begin
            Result := LItem.FindItemByName(AName, nil, True);
            if Result <> nil then
              Break;
          end;
        end;
    end

    // use both name and class then
    else
    begin
      if (Item is AClass) and
         AnsiSameText(Name, AName) then
        Result := Self
      else
        for I := 0 to Count - 1 do
        begin
          LItem := Items[I];
          if (LItem.Item is AClass) and
             AnsiSameText(LItem.Name, AName) then
          begin
            Result := LItem;
            Break;
          end
          else if Recurse then
          begin
            Result := LItem.FindItemByName(AName, AClass, True);
            if Result <> nil then
              Break;
          end;
        end;
    end;

end;

function TSprig.FindPath(const APath: string; Recurse: Boolean): TSprig;
begin
  Result := FindItemByPath(APath, Recurse);
end;

function TSprig.FindItemByPath(const APath: string; Recurse: Boolean = True): TSprig;
var
  I: Integer;
  LItem: TSprig;
begin
  Result := nil;
  if APath <> '' then
    if AnsiSameText(Path, APath) then
      Result := Self
    else
      for I := 0 to Count - 1 do
      begin
        LItem := Items[I];
        if AnsiSameText(LItem.Path, APath) then
        begin
          Result := LItem;
          Break;
        end
        else if Recurse then
        begin
          Result := LItem.FindPath(APath, True);
          if Result <> nil then
            Break;
        end;
      end;
end;

procedure TSprig.ForEach(ABefore, AAfter: TSprigAction);
var
  I: Integer;
begin
  if not Invalid then
  begin
    if Assigned(ABefore) then
      ABefore(Self);
    for I := Count - 1 downto 0 do
      Items[I].ForEach(ABefore, AAfter);
    if Assigned(AAfter) then
      AAfter(Self);
  end;
end;

function TSprig.GetItem(Index: Integer): TSprig;
begin
  Result := nil;
  if Assigned(FList) then
    Result := TSprig(FList[Index]);
end;

function TSprig.Hint: string;
begin
  Result := '';
end;

function TSprig.GetIndex: Integer;
begin
  Result := -1;
  if Parent <> nil then
    Result := Parent.IndexOf(Self);
end;

procedure TSprig.SetIndex(Value: Integer);
begin
  if Parent <> nil then
    Parent.SetIndexOf(Self, Value);
end;

function TSprig.IndexOf(AItem: TSprig): Integer;
begin
  Result := -1;
  if Assigned(FList) then
    Result := FList.IndexOf(AItem);
end;

procedure TSprig.SetIndexOf(AItem: TSprig; Index: Integer);
var
  LIndex: Integer;
begin
  if Assigned(FList) then
  begin
    LIndex := IndexOf(AItem);
    if LIndex > -1 then
      FList.Move(IndexOf(AItem), Max(0, Min(Index, FList.Count - 1)));
  end;
end;

function TSprig.UniqueName: string;
begin
  Result := '';
  if Item <> nil then
    Result := Item.GetNamePath;
end;

function TSprig.Name: string;
begin
  Result := UniqueName;
end;

procedure TSprig.PrepareMenu(const AItems: IMenuItems);
begin
  //
end;

function TSprig.SeekParent(AItem: TPersistent; Recurse: Boolean): TSprig;
var
  LComponent: TComponent;
begin
  Result := nil;
  if AItem <> nil then
  begin
    if (Parent <> nil) and
       (Parent.Item = AItem) then
      Result := Parent

    else if Owner <> nil then
      Result := Owner.Find(AItem, Recurse);

    if Result = nil then
    begin
      if Root <> nil then
        Result := Root.Find(AItem, Recurse);
        
      if Result = nil then
      begin
        if (Root <> nil) and (AItem is TComponent) then
        begin
          LComponent := TComponent(AItem);
          while (LComponent.Owner <> nil) and (Result = nil) do
          begin
            Result := Root.Find(LComponent.Owner, Recurse);
            LComponent := LComponent.Owner;
          end;
          if (Result = nil) or not Result.ParentOrphanedSprigs then
            Result := Root;
        end
        else
          Result := Root;
      end;
    end;
  end
  else
    Result := Root;
end;

function TSprig.SeekParent(const AName: string; Recurse: Boolean): TSprig;
begin
  Result := nil;
  if AName <> '' then
  begin
    if (Parent <> nil) and
       AnsiSameText(Parent.Name, AName) then
      Result := Parent

    else if Owner <> nil then
      Result := Owner.Find(AName, Recurse);

    if Result = nil then
    begin
      if Root <> nil then
        Result := Root.Find(AName, Recurse);
      if Result = nil then
        Result := Root;
    end;
  end
  else
    Result := Root;
end;

function TSprig.SeekParent(const AName: string; AClass: TClass;
  Recurse: Boolean): TSprig;
begin
  Result := Root;
  if (AName <> '') and
     (AClass <> nil) then
  begin
    if (Parent <> nil) and
       (Parent.Item <> nil) and
       (Parent.Item is AClass) and
       AnsiSameText(Parent.Name, AName) then
      Result := Parent

    else if Owner <> nil then
      Result := Owner.Find(AName, AClass, Recurse);

    if Result = nil then
    begin
      if Root <> nil then
        Result := Root.Find(AName, AClass, Recurse);
      if Result = nil then
        Result := Root;
    end;
  end
  else
    Result := Root;
end;

function TSprig.Transient: Boolean;
begin
  Result := False;
end;

function TSprig.TreeNodeFor(ATreeView: TCustomTreeView): TTreeNode;
//var
//  LParent: TTreeNode;
begin
//  if TreeNode = nil then
//  begin
//    EnsureUpdate;
//    LParent := nil;
//    if Parent <> nil then
//      LParent := Parent.TreeNode;
//    FTreeNode := THackTreeView(ATreeView).Items.AddNode(
//      TSprigTreeNode.Create(THackTreeView(ATreeView).Items),
//      LParent, Caption, Self, naAddChild);
//    //FTreeNode := THackTreeView(ATreeView).Items.AddChildObject(LParent, Caption, Self);
//  end;
  Result := TreeNode;
end;

procedure TSprig.VisualRefresh;

  function Trimmed(const AText: string): string;
  begin
    if Length(AText) >= 80 then
      Result := Copy(AText, 1, 76) + '... ' { Do not localize }
    else
      Result := AText;
  end;

begin
  if TreeNode <> nil then
    with TreeNode do
    begin
      if Self.FCaption <> Self.Caption then
        Self.FCaption := Trimmed(Self.Caption);
      Text := Self.FCaption;
      ImageIndex := Self.ImageIndex;
      SelectedIndex := Self.ImageIndex;
      StateIndex := Self.StateIndex;
      //HasChildren := Self.Count > 0;
    end;
end;

function TSprig.CaptionFor(const AName, ALabel, AClass: string): string;
begin
  Result := AName;
  if ALabel <> '' then
  begin
    if Result = '' then
      Result := '<?>'; // DO NOT LOCALIZE
    Result := Format('%s {%s}', [Result, ALabel]); // DO NOT LOCALIZE
  end;
  if GShowClassNameInTreeView then
  begin
    if AClass = '' then
      Result := Format('%s (%s)', [Result, AClass]) // DO NOT LOCALIZE
    else if Item <> nil then
      Result := Format('%s (%s)', [Result, Item.ClassName]); // DO NOT LOCALIZE
  end;
end;

function TSprig.PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean;
begin
  Result := False;
  if ASprigClass <> nil then
    Result := ASprigClass.PaletteOverTo(Self, AClass);
end;

class function TSprig.PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean;
var
  LProp: PPropInfo;
begin
  Result := False;
  if ParentProperty <> '' then
  begin
    LProp := GetPropInfo(AClass.ClassInfo, ParentProperty, [tkClass]);
    if LProp <> nil then
      Result := (AParent is TRootSprig) or
                (AParent.Item is GetTypeData(LProp^.PropType^)^.ClassType);
  end;
end;

function TSprig.Path: string;
begin
  Result := UniqueName;
  if Parent <> nil then
    Result := Format('%s\%s', [Parent.Path, Result]); // DO NOT LOCALIZE
end;

function TSprig.DeleteStyle: TSprigDeleteStyle;
const
  cDeleteStyle: array [Boolean] of TSprigDeleteStyle = (dsAbort, dsNormal);
begin
  Result := cDeleteStyle[Item <> nil];
end;

function TSprig.CustomDelete: Boolean;
begin
  Result := False;
end;

function TSprig.ItemClass: TClass;
begin
  Result := nil;
  if Item <> nil then
    Result := Item.ClassType;
end;

class function TSprig.ParentProperty: string;
begin
  Result := '';
end;

procedure TSprig.FigureParent;
var
  LProp: PPropInfo;
  LParentItem: TPersistent;
begin
  // assume nowhere!
  LParentItem := nil;

  // if we actually point to something
  if Item <> nil then
  begin

    // parent property based?
    if ParentProperty <> '' then
    begin
      LProp := GetPropInfo(Item, ParentProperty, [tkClass]);
      if LProp <> nil then
        LParentItem := TPersistent(GetObjectProp(Item, LProp, TPersistent));
    end;

    // still nothing but we have a component
    if (LParentItem = nil) and
       (Item is TComponent) then
      LParentItem := TComponent(Item).GetParentComponent;
  end;

  // plug in!
  if LParentItem <> nil then
    SeekParent(LParentItem).Add(Self)
  else if Owner <> nil then
    Owner.Add(Self)
  else
    Root.Add(Self);
end;

procedure TSprig.FigureChildren;
var
  LProps: TPropList;
  LProp: TObject;
  LPropCount, I: Integer;
  LParent: TSprig;
  LParentClass: TSprigClass;
begin
  // something to do?
  if (Item <> nil) and not FCollectionsDone then
  begin
    FCollectionsDone := True;

    // grab the list of properties
    LPropCount := GetPropList(Item.ClassInfo, [tkClass], @LProps);

    // we need to make this a optimized as possible
    for I := 0 to LPropCount - 1 do
    begin

      // got a collection?
      LProp := TObject(GetOrdProp(Item, LProps[I]));
      if (LProp is TCollection) and
         (GetUltimateOwner(TCollection(LProp)) <> nil) then
      begin

        // does it exists already?
        LParent := Find(TCollection(LProp), False);
        if LParent = nil then
        begin
          LParentClass := FindBestSprigClass(TCollection(LProp).ClassType, TCollectionSprig);
          if LParentClass <> nil then
          begin
            LParent := LParentClass.Create(TCollection(LProp));
            TCollectionSprig(LParent).FPropName := GetPropName(LProps[I]);
            TCollectionSprig(LParent).FOwner := Self;

            // made some additions
            Add(LParent);
          end;
        end;
      end;
    end;
  end;
end;

function TSprig.FocusItem: TPersistent;
begin
  Result := Item;
  if (Result = nil) and
     (Parent <> nil) then
    Result := Parent.FocusItem;
end;

{function SortBySprigItemIndex(Node1, Node2: TTreeNode; lParam: Integer): Integer; stdcall;
begin
  Result := TSprig(Node1.Data).ItemIndex -
            TSprig(Node2.Data).ItemIndex;
end;}

function SortBySprigItemIndex(Item1, Item2: Pointer): Integer;
begin
  if (TSprig(Item1).Item <> nil) and (TSprig(Item2).Item <> nil) then
    Result := TSprig(Item1).ItemIndex -
              TSprig(Item2).ItemIndex
  else
    Result := 0;
end;

procedure TSprig.SortItems;
begin
  if (FList <> nil) and (FList.Count > 0) and SortByIndex then
    FList.Sort(SortBySprigItemIndex);
{  if (TreeNode <> nil) and
     (TreeNode.HasChildren) then
    if SortByIndex then
      TreeNode.CustomSort(@SortBySprigItemIndex, 0)
    else
      TreeNode.CustomSort(nil, 0);}
end;

function TSprig.SortByIndex: Boolean;
begin
  Result := False;
end;

function TSprig.ItemIndex: Integer;
begin
  Result := 0;
end;

procedure TSprig.Reparent;
begin
  // we don't care
end;

function TSprig.ShowRegisteredMenus: Boolean;
begin
  Result := Item <> nil;
end;

procedure TSprig.ReparentChildren;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Reparent;
end;

function TSprig.IncludeIndexInCaption: Boolean;
begin
  Result := False;
end;

procedure TSprig.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if Expanded and
       (Parent <> nil) then
      Parent.Expanded := True;
  end;
end;

procedure TSprig.Invalidate;
var
  I: Integer;
begin
  if not Invalid then
  begin
    FInvalid := True;

    // remove ourselve from the root index
    if Root <> nil then
      Root.RemoveItem(Self);

    // don't point there anymore
    FItem := nil;
    for I := Count - 1 downto 0 do
      Items[I].Invalidate;
  end;
end;

procedure TSprig.SelectItems(const AItems: array of TPersistent; ARuntimeChange: Boolean);
begin
  if Root <> nil then
    Root.SelectItems(AItems, ARuntimeChange);
end;

function TSprig.Parents(ASprig: TSprig): Boolean;
begin
  repeat
    Result := ASprig = Self;
    ASprig := ASprig.Parent;
  until (Result = True) or
        (ASprig = nil);
end;

function TSprig.ParentOrphanedSprigs: Boolean;
begin
  Result := False;
end;

procedure TSprig.DesigntimeChange;
begin
  if Root <> nil then
    Root.DesigntimeChange;
end;

procedure TSprig.RuntimeChange;
begin
  if Root <> nil then
    Root.RuntimeChange;
end;

function TSprig.GetDesigner(out ADesigner: IDesigner): Boolean;
begin
  if Root <> nil then
    Result := Root.GetDesigner(ADesigner)
  else
    Result := False;
end;

function TSprig.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
  if Ghosted then
    Inc(Result, CGhostedOffset);
end;

procedure TSprig.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

function TSprig.GetStateIndex: TImageIndex;
const
  CStateIndex: array [Boolean] of TImageIndex = (CNoStateImage, CCheckOutStateImage);
begin
  Result := CStateIndex[AnyProblems];
end;

function TSprig.Construct(AClass: TComponentClass): TComponent;
var
  LDesigner: IDesigner;
  LParent: TPersistent;
begin
  Result := nil;
  if (Item <> nil) and
     (Item is TComponent) then
    LParent := Item
  else if Owner <> nil then
    LParent := Owner.Item
  else
    LParent := Root.Item;
  if (LParent is TComponent) and
     GetDesigner(LDesigner) then
    Result := LDesigner.CreateComponent(AClass, TComponent(LParent), 0, 0, 0, 0);
end;

procedure TSprig.BeginUpdate;
begin
  if Root <> nil then
    Root.BeginUpdate;
end;

procedure TSprig.EndUpdate;
begin
  if Root <> nil then
    Root.EndUpdate;
end;

procedure TSprig.EnsureUpdate;
begin
  if Root <> nil then
    Root.EnsureUpdate;
end;

function TSprig.Hidden: Boolean;
var
  LDesigner: IDesigner;
begin
  if not FHiddenTested and
     GetDesigner(LDesigner) then
  begin
    FHiddenTested := True;
    FHidden := not (Item is TComponent) or
               LDesigner.IsComponentHidden(TComponent(Item));
  end;
  Result := FHidden;
end;

function TSprig.Ghosted: Boolean;
begin
  Result := Hidden;
end;

function TSprig.CanMove(AUp: Boolean): Boolean;
var
  LIndex: Integer;
  LSibling: TSprig;
begin
  Result := Assigned(Parent) and Parent.SortByIndex and
            Assigned(FStructureNode) and Assigned(FStructureNode.Parent);
  if Result then
  begin
    LSibling := nil;
    LIndex := Parent.IndexOf(Self);
    if AUp and (LIndex > 0) then
      LSibling := Parent[LIndex - 1]
    else if not AUp and (LIndex < Parent.Count - 1) then
      LSibling := Parent[LIndex + 1];
    Result := Assigned(LSibling) and LSibling.DragOver(Self);
  end;
end;

function TSprig.Move(AUp: Boolean): Boolean;
var
  LIndex: Integer;
  LSibling: TSprig;
begin
  BeginUpdate;
  try
    LIndex := Parent.IndexOf(Self);
    if AUp then
      LSibling := Parent[LIndex - 1]
    else
      LSibling := Parent[LIndex + 1];
    Result := LSibling.DragDrop(Self);
    if Result then
      SelectItems([Item]);
  finally
    EndUpdate;
  end;
end;

procedure TSprig.AddType(Index: Integer);
begin
  //
end;

function TSprig.AddTypeCount: Integer;
begin
  Result := 0;
end;

function TSprig.CanAdd: Boolean;
begin
  Result := AddTypeCount > 0;
end;

function TSprig.GetAddType(Index: Integer): string;
begin
  Result := '';
end;

function TSprig.Owner: TSprig;
begin
  Result := nil;
end;

function TSprig.CopyGlyph(ABitmap: TBitmap): Boolean;
begin
  Result := Assigned(CopySprigGlyphFunc) and
            CopySprigGlyphFunc(Self, ABitmap);
end;

function TSprig.StructureNodeFor: IOTAStructureNode;
begin
  if FSprigStructureNode = nil then
  begin
    EnsureUpdate;
    FSprigStructureNode := TSprigStructureNode.Create(Self);
    FStructureNode := FSprigStructureNode as IOTAStructureNode;
  end;
  Result := FStructureNode;
end;

type
  TRootSprigStructureNode = class(TSprigStructureNode, IOTAStructureNode)
  protected
    function get_Parent: IOTAStructureNode; safecall;
  end;

{ TRootSprig }

procedure TRootSprig.AddItem(ASprig: TSprig);
begin
  if ASprig.Item <> nil then
    FIndex.Add(ASprig);
end;

function TRootSprig.DeleteStyle: TSprigDeleteStyle;
begin
  Result := dsAbort;
end;

procedure TRootSprig.SelectionSurvey(out ADeleteStyle: TSprigDeleteStyle; out AAllVisible: Boolean);
var
  LNode: IOTAStructureNode;
  LSprig: TSprig;
  LISprig: ISprig;
  LAbort, LAllCustom, LAllNormal: Boolean;
  LStructureView: IOTAStructureView;
begin
  AAllVisible := True;
  LAbort := False;
  LAllCustom := True;
  LAllNormal := True;
  if Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
  begin
    LNode := LStructureView.GetFirstSelected;
    while LNode <> nil do
    begin
      if Supports(LNode, ISprig, LISprig) then
        LSprig := LISprig.GetSprig
      else
        LSprig := nil;
      if LSprig <> nil then
      begin

        // calculate if all are visible?
        AAllVisible := AAllVisible and not LSprig.Hidden;

        // calculate delete style
        case LSprig.DeleteStyle of
          dsNormal:
            LAllCustom := False;
          dsIgnore:;
          dsAbort:
            LAbort := True;
          dsCustom:
            LAllNormal := False;
        end;
      end;
      LNode := LStructureView.GetNextSelected(LNode);
    end;
  end;
(*
  for I := 0 to TreeView.SelectionCount - 1 do
  begin
    LSprig := TSprig(TreeView.Selections[I].Data);
    if LSprig <> nil then
    begin

      // calculate if all are visible?
      AAllVisible := AAllVisible and not LSprig.Hidden;

      // calculate delete style
      case LSprig.DeleteStyle of
        dsNormal:
          LAllCustom := False;
        dsIgnore:;
        dsAbort:
          LAbort := True;
        dsCustom:
          LAllNormal := False;
      end;
    end;
  end;
*)
  ADeleteStyle := dsAbort;
  if not LAbort then
    if LAllNormal then
      ADeleteStyle := dsNormal
    else if LAllCustom then
      ADeleteStyle := dsCustom;
end;

function TRootSprig.EditAction(Action: TEditAction): Boolean;

  function DoCustomDelete(out ASprig: TSprig): Boolean;
  var
    LSprig: TSprig;
    LStructureView: IOTAStructureView;
    LOTASprig: IOTAStructureNode;
    LISprig: ISprig;
  begin
    Result := False;
    ASprig := nil;
    if Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
    begin
      LOTASprig := LStructureView.GetFirstSelected;
      while LOTASprig <> nil do
      begin
        if Supports(LOTASprig, ISprig, LISprig) then
        begin
          LSprig := LISprig.GetSprig;
          Result := LSprig.CustomDelete or Result;
          if not LSprig.Invalid then
            ASprig := LSprig;
        end;
        LOTASprig := LStructureView.GetNextSelected(LOTASprig);
      end;
    end;
  end;

var
  LEditQuery: IDesignEditQuery;
  LDeleteStyle: TSprigDeleteStyle;
  LAllVisible: Boolean;
  LSprig: TSprig;
begin
  Result := False;
  if Supports(Designer, IDesignEditQuery, LEditQuery) then
  begin

    // one we care about?
    if Action in [eaDelete, eaCut, eaCopy] then
    begin
      SelectionSurvey(LDeleteStyle, LAllVisible);

      // delete
      if Action = eaDelete then
        case LDeleteStyle of
          dsNormal:
            begin
              Designer.DeleteSelection(True);
              Result := True;
            end;
          dsCustom:
            begin
              Result := DoCustomDelete(LSprig);
              if Result then
                if (LSprig <> nil) and
                   (LSprig.Item <> nil) then
                  SelectItems([LSprig.Item], True)
                else
                  RuntimeChange;
            end;
        else
          Result := False;
        end

      // cut/copy
      else if (LDeleteStyle = dsNormal) and LAllVisible then
        Result := LEditQuery.EditAction(Action);
    end
    else
      Result := LEditQuery.EditAction(Action);
  end;
end;

function TRootSprig.GetEditState: TEditState;
var
  LEditQuery: IDesignEditQuery;
  LDeleteStyle: TSprigDeleteStyle;
  LAllVisible: Boolean;
begin
  Result := [];
  if Supports(Designer, IDesignEditQuery, LEditQuery) then
  begin
    Result := LEditQuery.GetEditState;

    Result := Result - [esCanEditOle, esCanCreateTemplate];

    SelectionSurvey(LDeleteStyle, LAllVisible);
    if LDeleteStyle = dsAbort then
      Result := Result - [esCanDelete];
    if not LAllVisible then
      Result := Result - [esCanCopy, esCanCut, esCanPaste];
  end;
end;

function TRootSprig.SelectedSprig(var ASprig: TSprig): Boolean;
var
  LStructureView: IOTAStructureView;
  LSprig: ISprig;
begin
  Result := Supports(BorlandIDEServices, IOTAStructureView, LStructureView) and
            (LStructureView.GetSelectedCount = 1) and
            (LStructureView.GetFirstSelected <> FStructureNode);
  if Result and Supports(LStructureView.GetFirstSelected, ISprig, LSprig) then
  begin
    ASprig := LSprig.GetSprig;
    Result := ASprig <> nil;
  end else
    Result := False;
end;

function TRootSprig.CanMove(AUp: Boolean): Boolean;
var
  LSprig: TSprig;
begin
  Result := SelectedSprig(LSprig) and
            LSprig.CanMove(AUp);
end;

function TRootSprig.Move(AUp: Boolean): Boolean;
var
  LSprig: TSprig;
begin
  Result := SelectedSprig(LSprig) and
            LSprig.Move(AUp);
end;

procedure TRootSprig.AddType(Index: Integer);
var
  LSprig: TSprig;
begin
  if SelectedSprig(LSprig) then
    LSprig.AddType(Index);
end;

function TRootSprig.AddTypeCount: Integer;
var
  LSprig: TSprig;
begin
  Result := 0;
  if SelectedSprig(LSprig) then
    Result := LSprig.AddTypeCount;
end;

function TRootSprig.GetAddType(Index: Integer): String;
var
  LSprig: TSprig;
begin
  Result := '';
  if SelectedSprig(LSprig) then
    Result := LSprig.GetAddType(Index);
end;

function TRootSprig.CanAdd: Boolean;
var
  LSprig: TSprig;
begin
  Result := SelectedSprig(LSprig) and
            LSprig.CanAdd;
end;

constructor TRootSprig.Create(AItem: TPersistent);
begin
  inherited;
  FRoot := Self;
  FIndex := TSprigIndex.Create;
  FNamedItems := TList.Create;
  FPathedItems := TList.Create;
  FRepopulateNeeded := True;
end;

procedure TRootSprig.DesigntimeChange;
{var
  LDesigner: IDesigner;}
begin
{  if GetDesigner(LDesigner) then
    LDesigner.Modified;}
    //!!
end;

destructor TRootSprig.Destroy;
begin
  SprigDesigner := nil;
  inherited;
  if FRootStructureContext <> nil then
    FRootStructureContext.ClearRootSprig;
  FStructureContext := nil;
  FreeAndNil(FIndex);
  FreeAndNil(FNamedItems);
  FreeAndNil(FPathedItems);
end;

procedure TRootSprig.FigureParent;
begin
  // we do nothing
end;

function TRootSprig.FindItem(AItem: TPersistent; Recurse: Boolean): TSprig;
begin
  if AItem = Item then
    Result := Self
  else if not Recurse then
    Result := inherited FindItem(AItem, False)
  else
    Result := FIndex.Find(AItem);
end;

function TRootSprig.FindItemByName(const AName: string; AClass: TClass;
  Recurse: Boolean): TSprig;
  function MatchingItem(ASprig: TSprig): Boolean;
  begin
    Result := AnsiSameText(ASprig.Name, AName) and
              ((AClass = nil) or
               (ASprig.Item is AClass));
  end;
var
  I: Integer;
begin
  if MatchingItem(Self) then
    Result := Self
  else
  begin
    Result := nil;
    for I := 0 to FNamedItems.Count - 1 do
      if MatchingItem(TSprig(FNamedItems[I])) then
      begin
        Result := TSprig(FNamedItems[I]);
        Break;
      end;                                               
    if Result = nil then
    begin
      Result := inherited FindItemByName(AName, AClass, Recurse);
      if Result <> nil then
        FNamedItems.Add(Result);
    end;
  end;
end;

function TRootSprig.FindItemByPath(const APath: string;
  Recurse: Boolean): TSprig;
var
  I: Integer;
begin
  if AnsiSameText(Path, APath) then
    Result := Self
  else
  begin
    Result := nil;
    for I := 0 to FPathedItems.Count - 1 do
      if AnsiSameText(TSprig(FPathedItems[I]).Path, APath) then
      begin
        Result := TSprig(FPathedItems[I]);
        Break;
      end;
    if Result = nil then
    begin
      Result := inherited FindItemByPath(APath, Recurse);
      if Result <> nil then
        FPathedItems.Add(Result);
    end;
  end;
end;

procedure TRootSprig.SelectItems(const AItems: array of TPersistent; ARuntimeChange: Boolean);
var
  LDesigner: IDesigner;
  LSelections: IDesignerSelections;
  I: Integer;
begin
  if GetDesigner(LDesigner) then
  begin
    if ARuntimeChange then
      LDesigner.Modified;
    LSelections := CreateSelectionlist;
    for I := Low(AItems) to High(AItems) do
      LSelections.Add(AItems[I]);
    LDesigner.SetSelections(LSelections);
  end;
end;

function TRootSprig.PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean;
begin
  Result := True;
end;

procedure TRootSprig.RemoveItem(ASprig: TSprig);
begin
  if ASprig.Item <> nil then
    FIndex.Remove(ASprig);
  FNamedItems.Remove(ASprig);
  FPathedItems.Remove(ASprig);
end;

procedure TRootSprig.ValidateParent(AItem: TSprig);
var
  LParent: TSprig;
begin
  if not AItem.Invalid then
  begin

    // figure out the parent
    LParent := AItem.Parent;
    AItem.FigureParent;
    FParentChanges := FParentChanges or (LParent <> AItem.Parent);

    // figure out the children
    AItem.FigureChildren;
  end;
end;

function TRootSprig.Repopulate: Boolean;
var
  LToDo: TList;

  procedure ValidateSprigs(ASprig: TSprig);
  var
    I: Integer;
  begin
    // only if the sprig is valid
    if not ASprig.Invalid then
    begin
      // expando?
      StoreExpandState(ASprig);

      // remove it from the todo list?
      if ASprig.Item <> nil then
        LToDo.Remove(ASprig.Item);
    end;

    // now validate the children
    for I := ASprig.Count - 1 downto 0 do
      ValidateSprigs(ASprig[I]);

    // now the sprig itself
    if ASprig.Invalid then
      ASprig.Free;
  end;

  procedure RemoveInvalidSprigs(ASprig: TSprig);
  var
    I: Integer;
  begin
    for I := ASprig.Count - 1 downto 0 do
      RemoveInvalidSprigs(ASprig);
    if ASprig.Invalid then
      ASprig.Free;
  end;
var
  I: Integer;
  LSprigClass: TSprigClass;
  LSprig: TSprig;
  LItem: TComponent;
begin
  // assume no additions
  Result := False;
  if FRepopulateNeeded then
  begin
    BeginUpdate;
    FRepopulating := True;
    LToDo := TList.Create;
    try

      // For each component, add to the ToDo list
      with TComponent(Item) do
        for I := 0 to ComponentCount - 1 do
        begin
          LItem := Components[I];
          if not (csTransient in LItem.ComponentStyle) and
             (csDesigning in LItem.ComponentState) and
             not (csDestroying in LItem.ComponentState) then
            LToDo.Add(Components[I]);
        end;

      // clear the invalid items
      ValidateSprigs(Self);

      // For each item in the ToDo list
      for I := 0 to LToDo.Count - 1 do
      begin

        // Find best sprig class
        LSprigClass := FindBestSprigClass(TComponent(LToDo[I]).ClassType, TComponentSprig);

        // Create the sprig at the root
        if LSprigClass <> nil then
        begin
          LSprig := LSprigClass.Create(TComponent(LToDo[I]));
          TComponentSprig(LSprig).FOwner := Self;

          // made some additions
          Add(LSprig);
          Result := True;
        end;
      end;

      // For each sprig until there are no more parent changes
      repeat
        FParentChanges := False;
        ForEach(ValidateParent);
      until not FParentChanges;

      // prune the tree of sprigs (transient or any remaining invalid ones
      ClearUnneededSprigs;

      // make sure we are expanded
      FExpanded := True;

    finally
      // clean up
      LToDo.Free;
      FRepopulateNeeded := False;
      FRepopulating := False;
      EndUpdate;
    end;
  end;
end;

procedure TRootSprig.RuntimeChange;
var
  LDesigner: IDesigner;
begin
  if GetDesigner(LDesigner) then
    LDesigner.Modified;
end;

procedure TRootSprig.PreRefreshTreeView(AItem: TSprig);
begin
//  with AItem do
//  begin
//    TreeNodeFor(TreeView);
//    VisualRefresh;
//  end;
end;

procedure TRootSprig.PostRefreshTreeView(AItem: TSprig);
begin
  with AItem do
  begin
    SortItems;
//    RestoreExpandState(AItem);
  end;
end;

(*
procedure TRootSprig.RestoreExpandState(AItem: TSprig);
{var
  I: Integer;}
  procedure MakeExpanded(ANode: TTreeNode);
  begin
    if ANode <> nil then
    begin
      if not ANode.Expanded then
        ANode.Expanded := True;
      MakeExpanded(ANode.Parent);
    end;
  end;
begin
  if AItem.TreeNode <> nil then
  begin
    {if FExpandedItems.Count > 0 then
    begin
      I := FExpandedItems.IndexOf(AItem.Path);
      if I >= 0 then
      begin
        FExpandedItems.Delete(I);
        AItem.Expanded := True;
      end;
    end;}
    if AItem.Expanded or
       (AItem = Self) then
      MakeExpanded(AItem.TreeNode);
  end;
end;
*)
procedure TRootSprig.StoreExpandState(AItem: TSprig);
begin
{  with AItem do
    Expanded := (TreeNode <> nil) and
                (TreeNode.Expanded) and
                (TreeNode.IsVisible);}
end;

procedure TRootSprig.StoreTreeState;
begin
  if TreeView <> nil then
    ForEach(StoreExpandState);
end;

procedure TRootSprig.DepopulateTreeView(AItem: TSprig);
begin
  with AItem do
  begin
    Expanded := (TreeNode <> nil) and
                (TreeNode.Expanded) and
                (TreeNode.IsVisible);
    ClearTreeNode;
  end;
end;

procedure TRootSprig.RefreshTreeView;
begin
  BeginUpdate;
  try
    if RepopulateNeeded then
      Repopulate;
//    if TreeView <> nil then
      ForEach(PreRefreshTreeView, PostRefreshTreeView);
  finally
    EndUpdate;
  end;
end;

function TRootSprig.GetDesigner(out ADesigner: IDesigner): Boolean;
begin
  ADesigner := Designer;
  Result := ADesigner <> nil;
end;

procedure TRootSprig.ItemDeleted(AItem: TPersistent);
var
  LSprig: TSprig;
begin
  LSprig := Find(AItem);
  if (LSprig <> nil) and
     (LSprig <> Self) and
     (not LSprig.Invalid) then
  begin
    LSprig.Invalidate;
    FRepopulateNeeded := True;
  end;
end;

procedure TRootSprig.ItemInserted;
begin
  FRepopulateNeeded := True;
end;

procedure TRootSprig.ItemsModified(AForceRepopulate: Boolean);
begin
  if AForceRepopulate then
    FRepopulateNeeded := True;
{  if SprigDesigner <> nil then
  begin
    SprigDesigner.BeforeItemsModified;
    try
      RefreshTreeView;
    finally
      SprigDesigner.AfterItemsModified;
    end;
  end;}
  ForEach(PreRefreshTreeView, PostRefreshTreeView);
end;

function TRootSprig.AcceptsClass(AClass: TClass): Boolean;
begin
  Result := AClass.InheritsFrom(TComponent);
end;

procedure TRootSprig.BeginUpdate;
begin
  Inc(FUpdateLocks);
end;

procedure TRootSprig.EndUpdate;
var
  LStructureView: IOTAStructureView;
  ASelection: IDesignerSelections;
  I: Integer;
  LPersistent: TPersistent;
  LSprig: TSprig;
  LNode: IOTAStructureNode;
begin
  if FUpdateLocks > 0 then
  begin
    Dec(FUpdateLocks);
    if (FUpdateLocks = 0) and (FNeedUpdate) then
    begin
//      if TreeView <> nil then
//        THackTreeView(TreeView).Items.EndUpdate;

      if Supports(BorlandIDEServices, IOTAStructureView, LStructureView) and
        (LStructureView.GetStructureContext = FStructureContext) then
      begin
        // Fix for 232007
        ASelection := TDesignerSelections.Create;
        Designer.GetSelections(ASelection);
        LStructureView.StructureChanged(False);

        if (LStructureview.GetFirstSelected = nil) and (ASelection.Count > 0) then
        begin
          for I := 0 to ASelection.Count - 1 do
          begin
            LPersistent := ASelection.Items[I];
            LSprig := Find(LPersistent, True);
            if (LSprig <> nil) and (not LSprig.Invalid) then
            begin
              LNode := LSprig.StructureNodeFor;
              if Assigned(LNode) then
                LStructureView.SelectNode(LNode);
            end;
          end;
        end;
        FNeedUpdate := False;
      end;
    end;
  end;
end;

procedure TRootSprig.EnsureUpdate;
begin
  if (FUpdateLocks > 0) and (not FNeedUpdate) then
  begin
//    if TreeView <> nil then
//      THackTreeView(TreeView).Items.BeginUpdate;
    FNeedUpdate := True;
  end;
end;

procedure TRootSprig.SetSprigDesigner(const ASprigDesigner: ISprigDesigner);
var
  LSprigDesigner: ISprigDesigner;
begin
  if SprigDesigner <> nil then
  begin
    Assert(FUpdateLocks = 0);
    ForEach(nil, DepopulateTreeView);
    LSprigDesigner := SprigDesigner;
    FSprigDesigner := nil;
    LSprigDesigner.RootSprig := nil;
    //LSprigDesigner.Collection := nil;
  end;
  FSprigDesigner := ASprigDesigner;
  FUpdateLocks := 0;
  FNeedUpdate := False;
  if SprigDesigner <> nil then
    RefreshTreeView;
end;

function TRootSprig.TreeView: TCustomTreeView;
begin
  Result := nil;
  if SprigDesigner <> nil then
    Result := SprigDesigner.GetTreeView;
end;

function TRootSprig.StructureContextFor: IOTAStructureContext;
begin
  if FRootStructureContext = nil then
  begin
    FRootStructureContext := TRootSprigStructureContext.Create(Self);
    FStructureContext := FRootStructureContext as IOTAStructureContext;
  end;
  if RepopulateNeeded then
    Repopulate;
  Result := FStructureContext;
end;

function TRootSprig.StructureNodeFor: IOTAStructureNode;
begin
  if FSprigStructureNode = nil then
  begin
    FSprigStructureNode := TRootSprigStructureNode.Create(Self);
    FStructureNode := FSprigStructureNode as IOTAStructureNode;
  end;
  Result := StructureNode;
end;

{ TSprigType }

constructor TSprigType.Create(const AClass: TClass; const ASprigClass: TSprigClass);
begin
  inherited Create;
  FClass := AClass;
  FSprigClass := ASprigClass;
  FGroup := CurrentGroup;
end;

function TSprigType.Score(const AClass: TClass): Integer;
begin
  Result := High(Integer);
  if AClass.InheritsFrom(FClass) then
    Result := CountGenerations(FClass, AClass);
end;

{ TSprigIntfType }

constructor TSprigIntfType.Create(const AInterfaces: TGUIDArray;
  const ASprigClass: TSprigClass);
begin
  inherited Create;
  FInterfaces := AInterfaces;
  FSprigClass := ASprigClass;
  FGroup := CurrentGroup;
end;

function TSprigIntfType.Match(const AClass: TClass): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FInterfaces) - 1 do
    if not Supports(AClass, FInterfaces[I]) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

{ TSprigTypeList }

procedure TSprigTypeList.Add(const AClass: TClass; const ASprigClass: TSprigClass);
begin
  FList.Insert(0, TSprigType.Create(AClass, ASprigClass));
end;

procedure TSprigTypeList.Add(const AInterfaces: TGUIDArray;
  const ASprigClass: TSprigClass);
begin
  FInterfaceList.Insert(0, TSprigIntfType.Create(AInterfaces, ASprigClass));
end;

procedure TSprigTypeList.ClearCache;
begin
  FLastClass := nil;
  FLastSprigClass := nil;
end;

constructor TSprigTypeList.Create;
begin
  inherited;
  FList := TObjectList.Create;
  FInterfaceList := TObjectList.Create;
end;

destructor TSprigTypeList.Destroy;
begin
  FList.Free;
  FInterfaceList.Free;
  inherited;
end;

procedure TSprigTypeList.FreeEditorGroup(AGroup: Integer);
var
  I: Integer;
begin
  ClearCache;
  for I := FList.Count - 1 downto 0 do
    if TSprigType(FList[I]).FGroup = AGroup then
      FList.Delete(I);
  for I := FInterfaceList.Count - 1 downto 0 do
    if TSprigIntfType(FInterfaceList[I]).FGroup = AGroup then
      FInterfaceList.Delete(I);
end;

function TSprigTypeList.Match(const AClass: TClass): TSprigClass;
begin
  Result := MatchCache(AClass);
  if Result = nil then
    Result := MatchClass(AClass);
end;

function TSprigTypeList.MatchCache(const AClass: TClass): TSprigClass;
begin
  Result := nil;
  if FLastClass = AClass then
    Result := FLastSprigClass;
end;

function TSprigTypeList.MatchClass(const AClass: TClass): TSprigClass;
var
  I, LBestScore, LScore: Integer;
begin
  Result := nil;
  for I := 0 to FInterfaceList.Count - 1 do
    if TSprigIntfType(FInterfaceList[I]).Match(AClass) then
    begin
      Result := TSprigIntfType(FInterfaceList[I]).SprigClass;
      Break;
    end;
  if Result = nil then
  begin
    LBestScore := High(Integer);
    for I := 0 to FList.Count - 1 do
    begin
      LScore := TSprigType(FList[I]).Score(AClass);
      if LScore < LBestScore then
      begin
        LBestScore := LScore;
        Result := TSprigType(FList[I]).SprigClass;
      end;
    end;
  end;
  if Result <> nil then
  begin
    FLastClass := AClass;
    FLastSprigClass := Result;
  end;
end;

{ TDragSprig }

procedure TDragSprigs.Add(ASprig: TSprig);
begin
  FSprigs.Add(ASprig);
end;

constructor TDragSprigs.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FSprigs := TList.Create;
end;

destructor TDragSprigs.Destroy;
begin
  FSprigs.Free;
  inherited;
end;

function TDragSprigs.GetSprig(Index: Integer): TSprig;
begin
  Result := TSprig(FSprigs[Index]);
end;

function TDragSprigs.Count: Integer;
begin
  Result := FSprigs.Count;
end;

{ TPropertySprig }

function TPropertySprig.DeleteStyle: TSprigDeleteStyle;
begin
  Result := dsAbort;
end;

function TPropertySprig.Ghosted: Boolean;
begin
  Result := False;
end;

{ TCollectionSprig }

function TCollectionSprig.DeleteStyle: TSprigDeleteStyle;
begin
  Result := dsCustom;
end;

function TCollectionSprig.CustomDelete: Boolean;
begin
  Result := TCollection(Item).Count > 0;
  if Result then
    TCollection(Item).Clear;
end;

function TCollectionSprig.Caption: string;
begin
  Result := CaptionFor(FPropName);
end;

procedure TCollectionSprig.FigureParent;
begin
  SeekParent(FOwner.Item);
end;

function TCollectionSprig.SortByIndex: Boolean;
begin
  Result := True;
end;

function TCollectionSprig.Name: string;
begin
  Result := Format(CCollectionName, [FPropName]);
end;

constructor TCollectionSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CCollectionSprigImage;
end;

procedure TCollectionSprig.AddType(Index: Integer);
begin
  SelectItems([TCollection(Item).Add]);
end;

function TCollectionSprig.AddTypeCount: Integer;
begin
  Result := 1;
end;

resourcestring
  sAddCaption = 'Add item';

function TCollectionSprig.GetAddType(Index: Integer): string;
begin
  case Index of
    0: Result := sAddCaption;
  end;
end;

function TCollectionSprig.Owner: TSprig;
begin
  Result := FOwner;
end;

procedure TCollectionSprig.FigureChildren;
var
  I: Integer;
  LChildItem: TCollectionItem;
  LChild: TSprig;
  LChildClass: TSprigClass;
begin
  // let it go first
  inherited;

  // now lets loop through the component items
  for I := 0 to TCollection(Item).Count - 1 do
  begin

    // find the best class
    LChildItem := TCollection(Item).Items[I];
    LChild := Find(LChildItem, False);

    // if not then create it
    if LChild = nil then
    begin
      LChildClass := FindBestSprigClass(LChildItem.ClassType, TCollectionItemSprig);
      if LChildClass <> nil then
      begin
        LChild := LChildClass.Create(LChildItem);
        TCollectionItemSprig(LChild).FOwner := Self;

        // made some additions
        Add(LChild);
      end;
    end;
  end;
end;

{ TCollectionItemSprig }

procedure TCollectionItemSprig.FigureParent;
begin
  SeekParent(FOwner.Item);
end;

function TCollectionItemSprig.Name: string;
begin
  Result := TCollectionItem(Item).DisplayName;
end;

function TCollectionItemSprig.ItemIndex: Integer;
begin
  Result := TCollectionItem(Item).Index;
end;

function TCollectionItemSprig.DragDropTo(AParent: TSprig): Boolean;
var
  LOrigIndex: Integer;
begin
  LOrigIndex := ItemIndex;
  if AParent.Parent = Parent then
  begin
    TCollectionItem(Item).Index := TCollectionItem(AParent.Item).Index;
    Parent.SetIndexOf(Self, ItemIndex);
  end;
  Result := LOrigIndex <> ItemIndex;
end;

function TCollectionItemSprig.DragOverTo(AParent: TSprig): Boolean;
begin
  Result := AParent.Parent = Parent;
end;

function TCollectionItemSprig.IncludeIndexInCaption: Boolean;
begin
  Result := True;
end;

procedure TCollectionItemSprig.AddType(Index: Integer);
begin
  Parent.AddType(Index);
end;

function TCollectionItemSprig.AddTypeCount: Integer;
begin
  Result := Parent.AddTypeCount;
end;

function TCollectionItemSprig.GetAddType(Index: Integer): string;
begin
  Result := Parent.AddTypes[Index];
end;

function TCollectionItemSprig.Owner: TSprig;
begin
  Result := FOwner;
end;

function TCollectionItemSprig.Ghosted: Boolean;
begin
  Result := False;
end;

{ TSprigIndex }

procedure TSprigIndex.Add(ASprig: TSprig);
var
  I, L: Integer;
begin
  L := WordRec(LongRec(ASprig.Item).Lo).Hi; // grab xxxxLLxx byte
  if FList[L] = nil then
    FList[L] := TList.Create;
  for I := 0 to TList(FList[L]).Count - 1 do
    if TList(FList[L]).Items[I] = ASprig then
      Assert(False);
  TList(FList[L]).Add(ASprig);
end;

constructor TSprigIndex.Create;
begin
  inherited;
  FList := TObjectList.Create;
  FList.Count := 256;
end;

destructor TSprigIndex.Destroy;
begin
  FList.Free;
  inherited;
end;

function TSprigIndex.Find(AItem: TPersistent): TSprig;
var
  I, L: Integer;
begin
  Result := nil;
  L := WordRec(LongRec(AItem).Lo).Hi; // grab xxxxLLxx byte
  if FList[L] <> nil then
    for I := 0 to TList(FList[L]).Count - 1 do
      if TSprig(TList(FList[L]).Items[I]).Item = AItem then
      begin
        Result := TSprig(TList(FList[L]).Items[I]);
        Break;
      end;
end;

procedure TSprigIndex.Remove(ASprig: TSprig);
var
  I, L: Integer;
begin
  L := WordRec(LongRec(ASprig.Item).Lo).Hi; // grab xxxxLLxx byte
  if FList[L] <> nil then
  begin
    for I := 0 to TList(FList[L]).Count - 1 do
      if TList(FList[L]).Items[I] = ASprig then
      begin
        TList(FList[L]).Delete(I);
        Break;
      end;
    if TList(FList[L]).Count = 0 then
      FList[L] := nil; // this will free and nil the sub-list reference
  end;
end;

{ TSprigTreeNode }

destructor TSprigTreeNode.Destroy;
begin
  if Data <> nil then
    TSprig(Data).ClearTreeNode(True, False);
  inherited;
end;

{ TComponentSprig }

constructor TComponentSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CComponentSprigImage;
end;

constructor TComponentSprig.Create(AItem: TPersistent; AOwner: TSprig);
begin
  Create(AItem);
  FOwner := AOwner;
end;

resourcestring
  SUnnamedItemCaption = '<Components[%d]>';

//function TComponentSprig.ShowRegisteredMenus: Boolean;
//begin
//  Result := not Hidden;
//end;

function TComponentSprig.Owner: TSprig;
begin
  Result := FOwner;
end;

function TComponentSprig.UniqueName: string;
begin
  if Item <> nil then
  begin
    Result := TComponent(Item).Name;
    if Result = '' then
      Result := Format(SUnnamedItemCaption, [TComponent(Item).ComponentIndex]);
  end
  else
    Result := inherited UniqueName;
end;

{ TAbstractSprig }

function TAbstractSprig.Ghosted: Boolean;
begin
  Result := False;
end;

{ TTransientSprig }

function TTransientSprig.Transient: Boolean;
begin
  Result := True;
end;

{ TAbstractCollectionSprig }

constructor TAbstractCollectionSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CFakeCollectionSprigImage;
end;

{ TTransientCollectionSprig }

constructor TTransientCollectionSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CFakeCollectionSprigImage;
end;


{ RootSprigList support }

var
  InternalRootSprigList: TRootSprigList = nil;
  DesignNotification: IDesignNotification;

function RootSprigList: TRootSprigList;
begin
  if InternalRootSprigList = nil then
    InternalRootSprigList := TRootSprigList.Create;
  Result := InternalRootSprigList;
end;

procedure InitDesignNotification;
begin
  if DesignNotification = nil then
  begin
    DesignNotification := TDesignNotificationHandler.Create as IDesignNotification;
    RegisterDesignNotification(DesignNotification);
  end;
end;

procedure DoneDesignNotification;
begin
  if DesignNotification <> nil then
    UnregisterDesignNotification(DesignNotification);
  DesignNotification := nil;
end;

{ TRootSprigList }

constructor TRootSprigList.Create;
begin
  inherited;
  FList := TBucketList.Create;
end;

procedure TRootSprigList.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
var
  LRootSprig: Pointer;
  LRoot: TComponent;
begin
  if ADesigner <> nil then
  begin
    LRoot := ADesigner.Root;
    if (LRoot <> nil) and FList.Find(LRoot, LRootSprig) then
    begin
      FList.Remove(LRoot);
      TRootSprig(LRootSprig).Designer := nil;
      TRootSprig(LRootSprig).SprigDesigner := nil;
      FreeAndNil(LRootSprig);
    end;
  end;
end;

procedure TRootSprigList.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
var
  LRoot: TComponent;
  LRootSprigClass: TRootSprigClass;
  LRootSprig: TRootSprig;
begin
  if ADesigner <> nil then
  begin
    LRoot := ADesigner.Root;
    if (LRoot <> nil) and not FList.Exists(LRoot) then
    begin
      LRootSprigClass := FindBestRootSprigClass(LRoot.ClassType);
      LRootSprig := LRootSprigClass.Create(LRoot);
      LRootSprig.Designer := ADesigner;
      FList.Add(LRoot, LRootSprig);
    end;
  end;
end;

destructor TRootSprigList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TRootSprigList.FindRoot(const ADesigner: IDesigner;
  out ARootSprig: TRootSprig): Boolean;
var
  LRootSprig: Pointer;
begin
  Result := (ADesigner <> nil) and FList.Find(ADesigner.Root, LRootSprig);
  if Result then
    ARootSprig := TRootSprig(LRootSprig);
end;

procedure TRootSprigList.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
var
  LRootSprig: TRootSprig;
begin
  if FindRoot(ADesigner, LRootSprig) then
  begin
    LRootSprig.ItemDeleted(AItem);
    if (LRootSprig.FRootStructureContext <> nil) and (LRootSprig.FUpdateLocks = 0) then
      LRootSprig.FRootStructureContext.StructureChanged(ADesigner);
  end;
end;

procedure TRootSprigList.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
var
  LRootSprig: TRootSprig;
begin
  if FindRoot(ADesigner, LRootSprig) then
  begin
    LRootSprig.ItemInserted;
    if (LRootSprig.FRootStructureContext <> nil) and (LRootSprig.FUpdateLocks = 0) then
      LRootSprig.FRootStructureContext.StructureChanged(ADesigner);
  end;
end;

procedure TRootSprigList.ItemsModified(const ADesigner: IDesigner);
var
  LRootSprig: TRootSprig;
begin
  if FindRoot(ADesigner, LRootSprig) then
  begin
    LRootSprig.ItemsModified;
    if (LRootSprig.FRootStructureContext <> nil) and (LRootSprig.FUpdateLocks = 0) then
      LRootSprig.FRootStructureContext.StructureChanged(ADesigner);
  end;
end;

{ TSprigStructureNode }

function TSprigStructureNode.AddChildNode(const ANode: IOTAStructureNode; Index: Integer = -1): Integer;
begin
  Result := -1;
end;

procedure TSprigStructureNode.ClearSprig;
begin
  FSprig := nil;
end;

constructor TSprigStructureNode.Create(ASprig: TSprig);
begin
  inherited Create;
  FSprig := ASprig;
end;

function TSprigStructureNode.get_Caption: WideString;
begin
  if FSprig <> nil then
    Result := FSprig.Caption
  else
    Result := '';
end;

function TSprigStructureNode.get_Child(Index: Integer): IOTAStructureNode;
begin
  if FSprig <> nil then
    Result := FSprig[Index].StructureNodeFor
  else
    Result := nil;
end;

function TSprigStructureNode.get_ChildCount: Integer;
begin
  if FSprig <> nil then
    Result := FSprig.Count
  else
    Result := 0;
end;

function TSprigStructureNode.get_Data: Integer;
begin
  Result := FData;
end;

function TSprigStructureNode.get_EditCaption: WideString;
begin
  Result := get_Caption;
end;

function TSprigStructureNode.get_Expanded: WordBool;
begin
  if FSprig <> nil then
    Result := FSprig.Expanded
  else
    Result := False;
end;

function TSprigStructureNode.get_Focused: WordBool;
begin
  Result := FFocused;
end;

function TSprigStructureNode.get_Hint: WideString;
begin
  if FSprig <> nil then
    Result := FSprig.Hint
  else
    Result := '';
end;

function TSprigStructureNode.get_ImageIndex: Integer;
begin
  if (FSprig <> nil) and (GImageOffset > -1) then
    Result := FSprig.ImageIndex + GImageOffset
  else
    Result := -1
end;

function TSprigStructureNode.get_Name: WideString;
begin
  if FSprig <> nil then
    Result := FSprig.Name
  else
    Result := '';
end;

function TSprigStructureNode.get_Parent: IOTAStructureNode;
begin
  if (FSprig <> nil) and (FSprig.Parent <> nil) then
    Result := FSprig.Parent.StructureNodeFor
  else
    Result := nil;
end;

function TSprigStructureNode.get_Selected: WordBool;
begin
  Result := FSelected;
end;

function TSprigStructureNode.get_StateIndex: Integer;
begin
  if FSprig <> nil then
    Result := FSprig.StateIndex
  else
    Result := -1;
  if (Result = 0) or (GStateImageOffset = -1) then
    Result := -1
  else
    Inc(Result, GStateImageOffset);
end;

function TSprigStructureNode.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSprigStructureNode.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSprigStructureNode.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSprigStructureNode.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  Result := S_OK;
end;

function TSprigStructureNode.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TSprigStructureNode.RemoveChildNode(Index: Integer);
begin
  {}
end;

procedure TSprigStructureNode.set_Data(Value: Integer);
begin
  FData := Value;
end;

procedure TSprigStructureNode.set_Expanded(Value: WordBool);
begin
  if FSprig <> nil then
    FSprig.Expanded := Value;
end;

procedure TSprigStructureNode.set_Focused(Value: WordBool);
begin
  FFocused := Value;
end;

procedure TSprigStructureNode.set_Selected(Value: WordBool);
begin
  FSelected := Value;
end;

function TSprigStructureNode.GetSprig: TSprig;
begin
  Result := FSprig;
end;

function TSprigStructureNode.Get_SortByIndex: WordBool;
begin
  Result := FSprig.SortByIndex;
end;

function TSprigStructureNode.Get_ItemIndex: Integer;
begin
  Result := FSprig.ItemIndex;
end;

function TSprigStructureNode.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr,
                                   ISprig, '', '');
end;

{ TRootSprigStructureContext }

procedure TRootSprigStructureContext.ClearRootSprig;
begin
  FRootSprig := nil;
  UnregisterDesignNotification(Self as IDesignNotification);
end;

procedure TRootSprigStructureContext.ContextActivated; safecall;
var
  LDesigner: IDesigner;
  LSelections: IDesignerSelections;
begin
  SetPendingClearSelection(False);
  if FRootSprig <> nil then
  begin
    LDesigner := FRootSprig.Designer;
    LSelections := CreateSelectionList;
    LDesigner.GetSelections(LSelections);
    SelectionChanged(LDesigner, LSelections);
  end;
end;

constructor TRootSprigStructureContext.Create(ARootSprig: TRootSprig);
begin
  inherited Create(ARootSprig);
  FRootSprig := ARootSprig;
  FUpdateEditState := True;
  RegisterDesignNotification(Self as IDesignNotification);
end;

procedure TRootSprigStructureContext.SetPendingClearSelection(AValue: Boolean);
begin
  FPendingClearSelection := AValue;
  if AValue then
    if FAppEvents = nil then
    begin
      FAppEvents := TApplicationEvents.Create(nil);
      FAppEvents.OnIdle := Idle;
    end;
end;

procedure TRootSprigStructureContext.Idle(Sender: TObject; var Done: Boolean);
var
  LStructureView: IOTAStructureView110;
  LDesigner: IDesigner;
begin
  if FPendingClearSelection then
  begin
    SetPendingClearSelection(False);
    LStructureView := BorlandIDEServices as IOTAStructureView110;
    if LStructureView.IsContextChanging then
      // Don't change designer selection.  Raid 244029.
      Exit;

    if (FRootSprig <> nil) and (LStructureView.GetSelectedCount = 0) then
    begin
      LDesigner := FRootSprig.Designer;
      if LDesigner <> nil then
        LDesigner.NoSelection;
    end;
  end;
end;

function TRootSprigStructureContext.get_ContextIdent: WideString;
begin
  if FRootSprig <> nil then
    Result := FRootSprig.Name
  else
    Result := '';
end;

function TRootSprigStructureContext.get_StructureType: WideString;
begin
  Result := DesignerStructureType;
end;

function TRootSprigStructureContext.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRootSprigStructureContext.GetRootStructureNode(Index: Integer): IOTAStructureNode;
begin
  if FRootSprig <> nil then
  begin
    Result := Self as IOTAStructureNode;
    if FRootSprig.RepopulateNeeded then
      FRootSprig.Repopulate;
  end else
    Result := nil;
end;

function TRootSprigStructureContext.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRootSprigStructureContext.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TRootSprigStructureContext.InitPopupMenu(const Node: IOTAStructureNode;
  const PopupMenu: IOTAStructureNodeMenuItem);
var
  I: Integer;
  LISprig: ISprig;
  LSprig: TSprig;
  LStructureView: IOTAStructureView;
  LDesignLocalMenu: IDesignLocalMenu;
  LPopup: IPopupMenu;
begin
  if Supports(Node, ISprig, LISprig) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) and
    Supports(FindRootDesigner(FRootSprig.Item), IDesignLocalMenu, LDesignLocalMenu) then
  begin
    LSprig := LISprig.GetSprig;
    if (LSprig = nil) or LSprig.ShowRegisteredMenus then
      LPopup := LDesignLocalMenu.BuildLocalMenu([lmModule, lmSelection, lmComponent])
    else
      LPopup := LDEsignLocalMenu.BuildLocalMenu([]);
    if LPopup <> nil then
    begin
      if LSprig <> nil then
      begin
        LSprig.PrepareMenu(LPopup.Items);
        if LSprig.AddTypeCount > 0 then
          with LPopup.Items do
          begin
            InsertLine;
            for I := LSprig.AddTypeCount - 1 downto 0 do
              InsertItem(LSprig.AddTypes[I], 0, False, True, HandleAddType).Tag := I;
          end;
      end;
      if (LPopup.Items.Count > 0) and (PopupMenu.Count > 0) then
        LPopup.Items.InsertLine(0);
      for I := LPopup.Items.Count - 1 downto 0 do
        PopupMenu.InsertItem(0, TStructureMenuItem.Create(LPopup.Items.Items[I]));
    end;
    FPopup := LPopup;
  end;
end;

function TRootSprigStructureContext.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  Result := S_OK;
end;

function TRootSprigStructureContext.Invoke(DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult,
  ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TRootSprigStructureContext.NodeEdited(const Node: IOTAStructureNode);
begin

end;

procedure TRootSprigStructureContext.NodeFocused(const Node: IOTAStructureNode);
begin

end;

procedure TRootSprigStructureContext.NodeSelected(const Node: IOTAStructureNode);
var
  LStructureView: IOTAStructureView110;
  LDesigner: IDesigner;
  LSelections, OSelections: IDesignerSelections;
  LSprig: TSprig;
  LItem: TPersistent;
  LNode: IOTAStructureNode;
  LSprigIntf: ISprig;

  function SameSelections(const Selection1, Selection2: IDesignerSelections): Boolean;
  var
    I: Integer;

    function ContainsItem(const Selection: IDesignerSelections; Item: TPersistent): Boolean;
    var
      I: Integer;
    begin
      Result := True;
      for I := 0 to Selection.Count - 1 do
        if Selection[I] = Item then
          Exit;
      Result := False;
    end;

  begin
    Result := Selection1.Equals(Selection2);
    if not Result and (Selection1.Count > 0) and (Selection2.Count > 0) and (Selection1.Count = Selection2.Count) then
    begin
      for I := 0 to Selection1.Count - 1 do
        if not ContainsItem(Selection2, Selection1[I]) then
          Exit;
      Result := True;
    end;
  end;

begin
  if not SelectLocked and (FRootSprig <> nil) then
  begin
    LockSelect;
    try
      LStructureView := BorlandIDEServices as IOTAStructureView110;
      if LStructureView.IsContextChanging then
        // Don't change designer selection.  Raid 244029.
        Exit;

      LDesigner := FRootSprig.Designer;
      if LStructureView.GetSelectedCount = 0 then
        //LDesigner.NoSelection
        // When the structure view node is selected by key down or mouse click, the tree selection is
        // cleared before a tree node is selected.  This causes excessive painting of the object
        // inspector.  Reduce paints by clearing (if necessary) the designer selection after a change in
        // tree selection has been completed.
        SetPendingClearSelection(True)
      else
      begin
        SetPendingClearSelection(False);
        LSelections := CreateSelectionList;
        LNode := LStructureView.GetFirstSelected;
        while LNode <> nil do
        begin
          if Supports(LNode, ISprig, LSprigIntf) then
          begin
            LSprig := LSprigIntf.GetSprig;
            LItem := LSprig.Item;
            if LItem <> nil then
              LSelections.Add(LItem);
          end;
          LNode := LStructureView.GetNextSelected(LNode);
        end;
        OSelections := CreateSelectionList;
        LDesigner.GetSelections(OSelections);
        if not SameSelections(OSelections, LSelections) then
          if LSelections.Count = 0 then
            LDesigner.NoSelection
          else
            LDesigner.SetSelections(LSelections);
      end;
    finally
      UnlockSelect;
    end;
  end;
end;

procedure TRootSprigStructureContext.PreserveNodeStates;
begin
  { In onpIntegrated mode, this method isn't called }
end;

function TRootSprigStructureContext.SameContext(const AContext: IOTAStructureContext): WordBool;
begin
  Result := AContext = (Self as IOTAStructureContext);
end;

function TRootSprigStructureContext.get_Child(Index: Integer): IOTAStructureNode;
begin
  if (Index = 0) and (FRootSprig <> nil) then
    Result := FRootSprig.StructureNodeFor
  else
    Result := nil;
end;

function TRootSprigStructureContext.get_ChildCount: Integer;
begin
  if FRootSprig <> nil then
    Result := 1
  else
    Result := 0;
end;

procedure TRootSprigStructureContext.LockSelect;
begin
  Inc(FSelectLocks);
end;

function TRootSprigStructureContext.SelectLocked: Boolean;
begin
  Result := FSelectLocks > 0;
end;

procedure TRootSprigStructureContext.UnlockSelect;
begin
  Assert(SelectLocked);
  Dec(FSelectLocks);
end;

procedure TRootSprigStructureContext.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
  {}
end;

procedure TRootSprigStructureContext.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin
  {}
end;

destructor TRootSprigStructureContext.Destroy;
begin
  FAppEvents.Free;
  inherited;
end;

procedure TRootSprigStructureContext.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
//  StructureChanged(ADesigner);
end;

procedure TRootSprigStructureContext.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
//  StructureChanged(ADesigner);
end;

procedure TRootSprigStructureContext.ItemsModified(const ADesigner: IDesigner);
begin
//  StructureChanged(ADesigner);
end;

procedure TRootSprigStructureContext.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
var
  LStructureView: IOTAStructureView;
  LPersistent: TPersistent;
  LSprig: TSprig;
  LNode: IOTAStructureNode;
  I: Integer;
begin
  if not SelectLocked and (ADesigner <> nil) and (FRootSprig <> nil) and
    (FRootSprig.Designer = ADesigner) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) and
    (LStructureView.GetStructureContext <> nil) and
    LStructureView.GetStructureContext.SameContext(Self as IOTAStructureContext) then
  begin
    SetPendingClearSelection(False);
    LockSelect;
    try
      FRootSprig.BeginUpdate;
      try
        if ASelection.Count > 0 then
        begin
          LStructureView.ClearSelection;
          FRootSprig.Repopulate;
          //FSyncSelectRequested := False;
          for I := 0 to ASelection.Count - 1 do
          begin
            LPersistent := ASelection.Items[I];
            LSprig := FRootSprig.Find(LPersistent, True);
            if (LSprig <> nil) and (not LSprig.Invalid) then
            begin
              LNode := LSprig.StructureNodeFor;
              if Assigned(LNode) then
                LStructureView.SelectNode(LNode);
            end;
//            else
//              FSyncSelectRequested := True;
          end;
        end;

        { HACK FOR CLX }
(*        if FSelectedNode <> nil then
          FSelectedNode.MakeVisible
        else if TreeView.Selected <> nil then
          TreeView.Selected.MakeVisible;*)
      finally
        FRootSprig.EndUpdate;
      end;
    finally
      UnlockSelect;
    end;
  end;
end;

procedure TRootSprigStructureContext.StructureChanged(const ADesigner: IDesigner);
var
  LStructureView: IOTAStructureView;
begin
  if (FRootSprig <> nil) and (ADesigner = FRootSprig.Designer) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
  begin
    FUpdateEditState := True;
    LStructureView.StructureChanged(True);
  end;
end;

function TRootSprigStructureContext.get_ViewOptions: Integer;
begin
  Result := voMultiSelect{ or voSiblingSelectConstraint};
end;

function TRootSprigStructureContext.DragAllowed(const Node: IOTAStructureNode): WordBool;
begin
  Result := True;
end;

procedure TRootSprigStructureContext.DragDrop(const Node: IOTAStructureNode;
  DataObject: OleVariant; const FormatArray: WideString;
  X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer);
var
  I: Integer;
  LSprig: ISprig;
  LTo, LFrom: TSprig;
  LStgMedium: TStgMedium;
  LFormatEtc: TFormatEtc;
  LDragSprigs: IDragSprigs;
  LArray: array of TPersistent;
  LChanges: Boolean;
  LDataObject: IDataObject;
begin
  if VarSupports(DataObject, IDataObject, LDataObject) then
  begin
    FillChar(LFormatEtc, SizeOf(LFormatEtc), 0);
    FillChar(LStgMedium, SizeOf(LStgMedium), 0);
    LFormatEtc.cfFormat := GSprigClipboardFormat;
    LFormatEtc.dwAspect := DVASPECT_CONTENT;
    LFormatEtc.tymed := TYMED_ISTREAM;
    if (LDataObject.GetData(LFormatEtc, LStgMedium) = S_OK) and
      Supports(IInterface(LStgMedium.stm), IDragSprigs, LDragSprigs) and
      Supports(Node, ISprig, LSprig) and (LSprig.GetSprig <> nil) then
    begin
      LTo := LSprig.GetSprig;
      FRootSprig.StoreTreeState;
      FRootSprig.BeginUpdate;
      try
        LChanges := False;
        SetLength(LArray, LDragSprigs.Count);
        for I := 0 to LDragSprigs.Count - 1 do
        begin
          LFrom := LDragSprigs.Sprigs[I];
          LArray[I] := LFrom.Item;
          LChanges := LTo.DragDrop(LFrom) or LChanges;
        end;
        if LChanges then
          FRootSprig.RepopulateNeeded := True;
        FRootSprig.SelectItems(LArray, LChanges);
      finally
        FUpdateEditState := True;
        FRootSprig.EndUpdate;
      end;
    end;
  end;
end;

function TRootSprigStructureContext.DragOver(const Node: IOTAStructureNode;
  DataObject: OleVariant; State: TOTADragState;
  X, Y: Integer; KeyState: Integer; Mode: TOTADropMode; var Effect: Integer): WordBool;
var
  LDataObject: IDataObject;
  LSprig: ISprig;
  LNode: IOTAStructureNode;
  LStgMedium: TStgMedium;
  LFormatEtc: TFormatEtc;
  LDragSprigs: IDragSprigs;
  LStructureView: IOTAStructureView;
  LTo, LFrom: TSprig;
  I: Integer;
begin
  Result := False;
  if VarSupports(DataObject, IDataObject, LDataObject) then
  begin
    FillChar(LFormatEtc, SizeOf(LFormatEtc), 0);
    FillChar(LStgMedium, SizeOf(LStgMedium), 0);
    LFormatEtc.cfFormat := GSprigClipboardFormat;
    LFormatEtc.dwAspect := DVASPECT_CONTENT;
    LFormatEtc.tymed := TYMED_ISTREAM;
    if (State = odsDragEnter) and (LDataObject.QueryGetData(LFormatEtc) <> S_OK) then
    begin
      LDragSprigs := TInternalDragSprigs.Create as IDragSprigs;
      LStgMedium.tymed := TYMED_ISTREAM;
      IInterface(LStgMedium.stm) := LDragSprigs;
      if LDataObject.SetData(LFormatEtc, LStgMedium, True) = S_OK then
        if Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
        begin
          LNode := LStructureView.GetFirstSelected;
          while LNode <> nil do
          begin
            if Supports(LNode, ISprig, LSprig) then
              LDragSprigs.Add(LSprig.GetSprig);
            LNode := LStructureView.GetNextSelected(LNode);
          end;
        end;
    end;
    if (State <> odsDragLeave) and (Mode = odmOnNode) and
      (LDataObject.GetData(LFormatEtc, LStgMedium) = S_OK) then
      if Supports(IInterface(LStgMedium.stm), IDragSprigs, LDragSprigs) and
        Supports(Node, ISprig, LSprig) then
      begin
        LTo := LSprig.GetSprig;
        if LTo <> nil then
        begin
          Result := True;
          for I := 0 to LDragSprigs.Count - 1 do
          begin
            LFrom := LDragSprigs.Sprigs[I];
            Result := Result and
                      (not LFrom.Parents(LTo)) and
                      (LTo.DragOver(LFrom));
            if not Result then
              Break;
          end;
          if Result then
            Effect := DROPEFFECT_MOVE;
        end;
      end;
  end;
end;

function TRootSprigStructureContext.GetDataObject: OleVariant;
begin
  Result := Variants.Null;
end;

procedure TRootSprigStructureContext.HandleAddType(Sender: TObject);
begin
  with TMenuItem(Sender) do
    FRootSprig.AddType(Tag);
end;

function TRootSprigStructureContext.EditAction(Action: Integer): WordBool;
begin
  if FRootSprig <> nil then
    Result := FRootSprig.EditAction(TEditAction(Action))
  else
    Result := False;
end;

function TRootSprigStructureContext.GetEditState: Integer;
begin
  if not FUpdateEditState then
  begin
    Result := Integer(Word(FLastEditState));
    Exit;
  end;
  if FRootSprig <> nil then
    FLastEditState := FRootSprig.GetEditState
  else
    FLastEditState := [];
  FUpdateEditState := False;
  Result := Integer(Word(FLastEditState));
end;

procedure TRootSprigStructureContext.KeyDown(const Node: IOTAStructureNode;
  KeyState: Integer; var KeyCode: Word);
begin
  if (KeyCode = VK_DELETE) and (esCanDelete in TEditState(Word(GetEditState))) and
    (KeyState and (MK_SHIFT or MK_CONTROL) = 0) then
  begin
    EditAction(Ord(eaDelete));
    KeyCode := 0;
  end else if (KeyCode = VK_F1) then
  begin
  end;
end;

resourcestring
  sAdd = 'Add';
  sDelete = 'Delete';
  sUp = 'Up';
  sDown = 'Down';
  sHintAdd = 'New Item';
  sHintDelete = 'Delete';
  sHintMoveUp = 'Move Up';
  sHintMoveDown = 'Move Down';

procedure TRootSprigStructureContext.KeyPressed(const Node: IOTAStructureNode;
  KeyState: Integer; var KeyChar: Word);
begin

end;

function TRootSprigStructureContext.Get_ButtonCount: Integer;
begin
  Result := 5;
end;

function TRootSprigStructureContext.GetButtonCaption(Index: Integer): WideString;
begin
  case Index of
   0: Result := sAdd;
   1: Result := sDelete;
   3: Result := sUp;
   4: Result := sDown;
  else
    Result := '';
  end;
end;

function TRootSprigStructureContext.GetButtonEnabled(Index: Integer): WordBool;
begin
  if FRootSprig <> nil then
    case Index of
     0: Result := FRootSprig.CanAdd;
     1: Result := esCanDelete in FRootSprig.GetEditState;
     3: Result := FRootSprig.CanMove(True);
     4: Result := FRootSprig.CanMove(False);
    else
      Result := False;
    end
  else
    Result := False;
end;

function TRootSprigStructureContext.GetButtonEnableDropDown(Index: Integer): WordBool;
begin
  Result := (Index = 0) and (FRootSprig <> nil) and (FRootSprig.AddTypeCount > 1);
end;

function TRootSprigStructureContext.GetButtonHasDropDown(Index: Integer): WordBool;
begin
  Result := (Index = 0) and (FRootSprig <> nil) and (FRootSprig.AddTypeCount > 1);
end;

function TRootSprigStructureContext.GetButtonHint(Index: Integer): WideString;
begin
  case Index of
   0: Result := sHintAdd;
   1: Result := sHintDelete;
   3: Result := sHintMoveUp;
   4: Result := sHintMoveDown;
  else
    Result := '';
  end;
end;

function TRootSprigStructureContext.GetButtonImageIndex(Index: Integer): Integer;
begin
  if GToolButtonImageOffset > -1 then
  begin
    if Index > 2 then
      Dec(Index);
    Result := Index + GToolButtonImageOffset + 4;
  end else
    Result := -1;
end;

function TRootSprigStructureContext.GetButtonMenu(Index: Integer): IOTAStructureNodeMenuItem;
begin
  Result := nil;
end;

function TRootSprigStructureContext.GetButtonSeparator(Index: Integer): WordBool;
begin
  Result := Index = 2;
end;

function TRootSprigStructureContext.GetButtonVisible(Index: Integer): WordBool;
begin
  Result := True;
end;

procedure TRootSprigStructureContext.Invoke(Index: Integer);
var
  LStructureView: IOTAStructureView;
begin
  if (FRootSprig <> nil) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
  begin
    LStructureView.BeginUpdate;
    try
      case Index of
        0: FRootSprig.AddType(0);
        1: FRootSprig.EditAction(eaDelete);
        3: FRootSprig.RepopulateNeeded := FRootSprig.Move(True);
        4: FRootSprig.RepopulateNeeded := FRootSprig.Move(False);
      end;
    finally
      if FRootSprig.RepopulateNeeded then
        LStructureView.StructureChanged(False);
      LStructureView.EndUpdate;
    end;
  end;
end;

procedure TRootSprigStructureContext.DefaultNodeAction(const Node: IOTAStructureNode);
var
  LStructureView: IOTAStructureView;
  LSprig: ISprig;
begin
  if Supports(BorlandIDEServices, IOTAStructureView, LStructureView) and
    Supports(Node, ISprig, LSprig)  then
  begin
    if (LStructureView.GetSelectedCount = 1) and
      (LStructureView.GetFirstSelected = Node) and
      (LSprig.GetSprig.Item is TComponent) and
      (FRootSprig <> nil) then
      FRootSprig.Designer.Edit(TComponent(LSprig.GetSprig.Item));
  end;
end;

procedure TRootSprigStructureContext.AddRootNode(
  const ANode: IOTAStructureNode; Index: Integer);
begin
  { do nothing }
end;

function TRootSprigStructureContext.Get_NodePreservationMode: TOTANodePreservationMode;
begin
  Result := onpIntegrated;
end;

function TRootSprigStructureContext.Get_RootNodeCount: Integer;
begin
  if FRootSprig <> nil then
    Result := 1
  else
    Result := 0;
end;

procedure TRootSprigStructureContext.RemoveRootNode(
  const ANode: IOTAStructureNode);
begin
  { do nothing }
end;

procedure TRootSprigStructureContext.RestoreNodeState(const Node: IOTAStructureNode);
begin
   { In onpIntegrated mode, this method isn't called }
end;

function TRootSprigStructureContext.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr,
                                   IOTAStructureContextToolbar, '', '');
end;

{ TRootSprigStructureNode }

function TRootSprigStructureNode.get_Parent: IOTAStructureNode;
begin
  if FSprig is TRootSprig then
    Result := TRootSprig(FSprig).StructureContextFor.GetRootStructureNode(0)
  else
    Result := nil;
end;

{ TInternalDragSprigs }

function TInternalDragSprigs.Add(ASprig: TSprig): Integer;
begin
  Result := FDragSprigs.Add(ASprig);
end;

constructor TInternalDragSprigs.Create;
begin
  inherited Create(TMemoryStream.Create, soOwned);
  FDragSprigs := TList.Create;
end;

destructor TInternalDragSprigs.Destroy;
begin
  FDragSprigs.Free;
  inherited;
end;

function TInternalDragSprigs.GetCount: Integer;
begin
  Result := FDragSprigs.Count;
end;

function TInternalDragSprigs.GetSprig(Index: Integer): TSprig;
begin
  Result := FDragSprigs[Index];
end;

procedure TInternalDragSprigs.Remove(ASprig: TSprig);
begin
  FDragSprigs.Remove(ASprig);
end;

{ TStructureMenuItem }

constructor TStructureMenuItem.Create(const AItem: IMenuItem);
begin
  inherited Create;
  FItem := AItem;
end;

procedure TStructureMenuItem.DeleteItem(Index: Integer);
begin
  {}
end;

function TStructureMenuItem.get_Caption: WideString;
begin
  Result := FItem.Caption;
end;

function TStructureMenuItem.get_Checked: WordBool;
begin
  Result := FItem.Checked;
end;

function TStructureMenuItem.get_Count: Integer;
begin
  Result := FItem.Count;
end;

function TStructureMenuItem.get_Enabled: WordBool;
begin
  Result := FItem.Enabled;
end;

function TStructureMenuItem.get_ImageIndex: Integer;
begin
  Result := -1;
end;

function TStructureMenuItem.get_Item(Index: Integer): IOTAStructureNodeMenuItem;
begin
  Result := TStructureMenuItem.Create(FItem.Items[Index]) as IOTAStructureNodeMenuItem;
end;

function TStructureMenuItem.get_Name: WideString;
begin
  Result := FItem.Name;
end;

function TStructureMenuItem.get_ShortCut: Integer;
begin
  Result := FItem.Shortcut;
end;

function TStructureMenuItem.get_Visible: WordBool;
begin
  Result := FItem.Visible;
end;

function TStructureMenuItem.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStructureMenuItem.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStructureMenuItem.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TStructureMenuItem.InsertItem(Index: Integer; const Item: IOTAStructureNodeMenuItem);
begin
  {}
end;

function TStructureMenuItem.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  Result := S_OK;
end;

procedure TStructureMenuItem.Invoke;
var
  LItem: IMenuItem2;
  LStructureView: IOTAStructureView;
begin
  if Supports(FItem, IMenuItem2, LItem) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
  begin
    LStructureView.BeginUpdate;
    try
      LItem.Click;
    finally
      LStructureView.EndUpdate;
    end;
  end;
end;

function TStructureMenuItem.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStructureMenuItem.get_GroupIndex: Integer;
begin
  Result := FItem.GroupIndex;
end;

function TStructureMenuItem.get_RadioItem: WordBool;
begin
  Result := FItem.RadioItem;
end;

function TStructureMenuItem.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr,
                                   IOTAStructureNodeMenuItem, '', '');
end;

{ TDesignNotificationHandler }

procedure TDesignNotificationHandler.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  RootSprigList.DesignerClosed(ADesigner, AGoingDormant);
end;

procedure TDesignNotificationHandler.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
begin
  RootSprigList.DesignerOpened(ADesigner, AResurrecting);
end;

procedure TDesignNotificationHandler.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  RootSprigList.ItemDeleted(ADesigner, AItem);
end;

procedure TDesignNotificationHandler.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  RootSprigList.ItemInserted(ADesigner, AItem);
end;

procedure TDesignNotificationHandler.ItemsModified(const ADesigner: IDesigner);
begin
  RootSprigList.ItemsModified(ADesigner);
end;

procedure TDesignNotificationHandler.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
var
  LServices: IOTAServices;
  LStructureView: IOTAStructureView;
  LCurContext, LStructureContext: IOTAStructureContext;
  LRootSprig: TRootSprig;
begin
  if Supports(BorlandIDEServices, IOTAServices, LServices) and
    Supports(BorlandIDEServices, IOTAStructureView, LStructureView) then
  begin
    if not LServices.GetEnvironmentOptions.GetOptionValue('InternalEmbeddedDesigner') then
      if (RootSprigList <> nil) and RootSprigList.FindRoot(ADesigner, LRootSprig) then
      begin
        LStructureContext := LRootSprig.StructureContextFor;
        LCurContext := LStructureView.GetStructureContext;
        if (LCurContext = nil) or not LCurContext.SameContext(LStructureContext) then
          LStructureView.SetStructureContext(LStructureContext);
      end;
  end;
end;

initialization
  NotifyGroupChange(FlushSprigTypes);
  GSprigClipboardFormat := RegisterClipboardFormat(CSprigClipboardFormat);

finalization
  FreeAndNil(InternalRootSprigList);
  FreeAndNil(InternalSprigTypeList);
  FreeAndNil(InternalRootSprigTypeList);
  UnNotifyGroupChange(FlushSprigTypes);
end.
