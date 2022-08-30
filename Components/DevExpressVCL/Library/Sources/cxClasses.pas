{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit cxClasses;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, ShellAPI, TypInfo, SysUtils, Classes, Forms, Controls, IniFiles,
  Contnrs, SyncObjs, dxCore, dxCoreClasses, dxMessages, cxGeometry, dxGenerics;

type
  TcxAlignmentVert = (vaTop, vaBottom, vaCenter);
  TcxTopBottom = vaTop..vaBottom;
  TcxCollectionOperation = (copAdd, copDelete, copChanged);
  TcxDirection = (dirNone, dirLeft, dirRight, dirUp, dirDown);
  TcxGetComponent = function(ACaller: TComponent; Index: Integer): TComponent;
  TcxGetCaptionForIntegerItemFunc = function(AItem: Integer): string;
  TcxPosition = (posNone, posLeft, posRight, posTop, posBottom);
  TdxSkinName = type string;
  TdxIntegerIndexes = array of Integer;

const
{$IFDEF USETOPVERTICALALIGNMENTASDEFAULT}
  cxDefaultAlignmentVert: TcxAlignmentVert = vaTop;
{$ELSE}
  cxDefaultAlignmentVert: TcxAlignmentVert = vaCenter;
{$ENDIF}

type
  TcxTag = TdxNativeInt;

  { IcxDesignSelectionChanged }

  IcxDesignSelectionChanged = interface
  ['{66B3AA59-1EBD-4135-AB18-E980F9C970F3}']
    procedure DesignSelectionChanged(ASelection: TList);
  end;

  { IcxDesignHelper }

  IcxDesignHelper = interface
  ['{4C78CC4F-699B-43BD-94AC-E3BD2233F7A1}']
    procedure AddSelectionChangedListener(AObject: TPersistent);
    function CanAddComponent(AOwner: TComponent): Boolean;
    function CanDeleteComponent(AOwner: TComponent; AComponent: TComponent): Boolean;
    procedure ChangeSelection(AOwner: TComponent; AObject: TPersistent);
    function IsObjectSelected(AOwner: TComponent; AObject: TPersistent): Boolean;
    procedure Modified;
    procedure RemoveSelectionChangedListener(AObject: TPersistent);
    procedure SelectObject(AOwner: TComponent; AObject: TPersistent; AClearSelection: Boolean = True;
      AActivateOwner: Boolean = True);
    procedure ShowComponentDefaultEventHandler(AComponent: TComponent);
    function UniqueName(AOwner: TComponent; const ABaseName: string): string;
    procedure UnselectObject(AOwner: TComponent; AObject: TPersistent);
  end;

  { IdxManager }

  IdxManager = interface
  ['{E82263AF-4C67-44BE-AD3E-3F8CEF246A3B}']
    function GetParentForm: TCustomForm;
  end;

  { IdxManagedObject }

  IdxManagedObject = interface
  ['{AB78C3D2-400B-407F-A988-C015DDCEFED3}']
    function GetManager: IdxManager;
  end;

  { IdxAdornerRootTargetElement }

  IdxAdornerRootTargetElement = interface
  ['{23C48E23-DCDF-43C2-B855-351ED49B1227}']
  end;

  { IdxAdornerTargetElement }

  IdxAdornerTargetElement = interface
  ['{FF5950DC-2CE8-4206-BB6A-13635D78F551}']
    function GetControl: TWinControl;
    function GetBounds: TRect;
    function GetVisible: Boolean;
  end;

  { IdxAdornerTargetElementCollection }

  IdxAdornerTargetElementCollection = interface
  ['{315F65EF-DC25-417E-8A4F-26060549DCC7}']
    procedure GetElements(AList: TStrings);
  end;

  { TcxCustomComponent }

  TcxCustomComponent = class(TComponent,
    IdxAdornerTargetElementCollection)
  protected
    // IdxAdornerTargetElementCollection
    procedure IdxAdornerTargetElementCollection.GetElements = GetAdornerTargetElements;
    procedure GetAdornerTargetElements(AList: TStrings); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  { TcxComponent}

  TcxComponent = class(TcxCustomComponent)
  strict private
    FFakeComponentLink1: TComponent;
    FFakeComponentLink2: TComponent;
    FFakeComponentLink3: TComponent;

    function GetFakeComponentLinkCount: Integer;
    function GetIsDesigning: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    procedure SetFakeComponentLink(Index: Integer; Value: TComponent);
  protected
    procedure GetFakeComponentLinks(AList: TList); virtual;
    procedure Loaded; override;
    procedure UpdateFakeLinks;
  public
    destructor Destroy; override;

    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
  published
    property FakeComponentLink1: TComponent read FFakeComponentLink1 write FFakeComponentLink1 stored False;
    property FakeComponentLink2: TComponent read FFakeComponentLink2 write FFakeComponentLink2 stored False;
    property FakeComponentLink3: TComponent read FFakeComponentLink3 write FFakeComponentLink3 stored False;
  end;

  { TcxComponentHolder }

  TcxComponentHolder = class(TComponent)
  private
    FComponent: TComponent;
    FOnAfterComponentChange: TNotifyEvent;
    FOnBeforeComponentChange: TNotifyEvent;
    procedure SetComponent(AValue: TComponent);
  protected
    procedure DoAfterComponentChange;
    procedure DoBeforeComponentChange;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    //
    property Component: TComponent read FComponent write SetComponent;
    property OnAfterComponentChange: TNotifyEvent read FOnAfterComponentChange write FOnAfterComponentChange;
    property OnBeforeComponentChange: TNotifyEvent read FOnBeforeComponentChange write FOnBeforeComponentChange;
  end;

  { TcxTimer }

  TcxTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FEventID: TdxNativeUInt;
    FInterval: Cardinal;
    FTimerOn: Boolean;

    FOnTimer: TNotifyEvent;

    function CanSetTimer: Boolean;
    procedure KillTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetTimer;
    procedure SetTimerOn(Value: Boolean);
    procedure UpdateTimer;
    property TimerOn: Boolean read FTimerOn write SetTimerOn;
  protected
    procedure TimeOut; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  { IcxScalableComponent }

  IcxScalableComponent = interface
  ['{70931963-6520-4B20-A9FD-06EE20C9EEF1}']
    procedure ChangeScale(M, D: Integer);
    procedure ScaleForPPI(TargetPPI: Integer);
  end;

  { TcxScalableComponent }

  TcxScalableComponent = class(TcxComponent,
    IdxScaleFactor,
    IcxScalableComponent)
  strict private
    FScalable: Boolean;
    FScaleFactor: TdxScaleFactor;

    procedure ReadPixelsPerInch(Reader: TReader);
    procedure WritePixelsPerInch(Writer: TWriter);
    function GetPixelsPerInch: Integer;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    // IcxScalableComponent
    procedure ChangeScale(M, D: Integer); virtual;

    property PixelsPerInch: Integer read GetPixelsPerInch;
    property Scalable: Boolean read FScalable write FScalable default True;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleForPPI(TargetPPI: Integer);
  end;

  { TcxInterfacedCollectionItem }

  TcxInterfacedCollectionItem = class(TCollectionItem, IUnknown)
  strict private
    FOwnerInterface: IUnknown;
  protected
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  public
    procedure AfterConstruction; override;
  end;

  { TcxHashedStringList }

  TcxHashedStringList = class
  strict private
    FList: TStringList;

    function GetCount: Integer;
    function GetHash(Index: Integer): Integer;
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual;
    function IndexOf(const S: string): Integer; virtual;
    procedure Clear; virtual;
    //
    property Count: Integer read GetCount;
    property Hash[Index: Integer]: Integer read GetHash;
    property Strings[Index: Integer]: string read GetString write SetString;
  end;

  { TcxOwnedInterfacedCollection }

  TcxCollectionNotifyEvent = procedure (Sender: TObject; AItem: TCollectionItem) of Object;
  TcxCollectionCompareEvent = function(ASender: TObject; AItem1, AItem2: TCollectionItem): Integer of object;
  TcxCollectionInternalState = (cisDestroying, cisUpdating, cisSorting);
  TcxCollectionInternalStates = set of TcxCollectionInternalState;

  TcxOwnedInterfacedCollection = class(TOwnedCollection, IUnknown)
  private
    FInternalState: TcxCollectionInternalStates;
    FSorted: Boolean;
    FOnChange: TcxCollectionNotifyEvent;
    FOnCompare: TcxCollectionCompareEvent;

    function CompareItems(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
    function GetIsDestroying: Boolean;
    procedure SetSorted(AValue: Boolean);
  protected
    procedure Update(Item: TCollectionItem); override;

    function DoCompareItems(AItem1, AItem2: TcxInterfacedCollectionItem): Integer; virtual;
    procedure Sort;

    property Sorted: Boolean read FSorted write SetSorted;

    //  IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property IsDestroying: Boolean read GetIsDestroying;
    property OnCompare: TcxCollectionCompareEvent read FOncompare write FOnCompare;
  public
    destructor Destroy; override;

    property OnChange: TcxCollectionNotifyEvent read FOnChange write FOnChange;
  end;

  { TcxObjectList }

  TcxObjectList = class(TList)
  private
    FOwnObjects: Boolean;
    function GetItem(Index: Integer): TObject;
  protected
    procedure FreeItem(AIndex: Integer); virtual;
  public
    constructor Create(AOwnObjects: Boolean = True);
    procedure Clear; override;
    procedure FreeAndDelete(Index: Integer);
    function FreeAndRemove(AObject: TObject): Integer;

    property Items[Index: Integer]: TObject read GetItem; default;
    property OwnObjects: Boolean read FOwnObjects;
  end;

  { TcxEventHandlerCollection }

  TcxEventHandler = procedure (Sender: TObject; const AEventArgs) of object;

  TcxEventHandlerCollection = class(TObject)
  private
    FEvents: array of TcxEventHandler;
    procedure Delete(AIndex: Integer);
    function IndexOf(AEvent: TcxEventHandler): Integer;
  public
    procedure Add(AEvent: TcxEventHandler);
    procedure CallEvents(Sender: TObject; const AEventArgs); overload;
    procedure CallEvents(Sender: TObject); overload;
    procedure Remove(AEvent: TcxEventHandler);
  end;

  { TcxRegisteredClassList }

  TcxRegisteredClassListItemData = class
  public
    ItemClass: TClass;
    RegisteredClass: TClass;
  end;

  TcxRegisteredClassList = class
  private
    FItems: TdxFastList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxRegisteredClassListItemData;
  protected
    function Find(AItemClass: TClass; var AIndex: Integer): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FindClass(AItemClass: TClass): TClass;
    procedure Register(AItemClass, ARegisteredClass: TClass); virtual;
    procedure Unregister(AItemClass, ARegisteredClass: TClass); virtual;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxRegisteredClassListItemData read GetItem; default;
  end;

  { TcxRegisteredClasses } // TODO: Name

  TcxRegisteredClasses = class
  private
    FItems: TStringList;
    FRegisterClasses: Boolean;
    FSorted: Boolean;
    function GetCount: Integer;
    function GetDescription(Index: Integer): string;
    function GetHint(Index: Integer): string;
    function GetItem(Index: Integer): TClass;
    procedure SetSorted(Value: Boolean);
  protected
    function CompareItems(AIndex1, AIndex2: Integer): Integer; virtual;
    procedure Sort; virtual;
  public
    constructor Create(ARegisterClasses: Boolean = False);
    destructor Destroy; override;
    procedure Clear;
    function FindByClassName(const AClassName: string): TClass;
    function FindByDescription(const ADescription: string): TClass;
    function GetDescriptionByClass(AClass: TClass): string;
    function GetHintByClass(AClass: TClass): string;
    function GetIndexByClass(AClass: TClass): Integer;
    procedure Register(AClass: TClass; const ADescription: string);
    procedure Unregister(AClass: TClass);
    property Count: Integer read GetCount;
    property Descriptions[Index: Integer]: string read GetDescription;
    property Hints[Index: Integer]: string read GetHint;
    property Items[Index: Integer]: TClass read GetItem; default;
    property RegisterClasses: Boolean read FRegisterClasses write FRegisterClasses;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  { TcxAutoWidthObject }

  TcxAutoWidthItem = class
  public
    MinWidth: Integer;
    Width: Integer;
    Fixed: Boolean;
    AutoWidth: Integer;
    constructor Create;
  end;

  TcxAutoWidthObject = class
  private
    FAvailableWidth: Integer;
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxAutoWidthItem;
    function GetWidth: Integer;
  protected
    procedure Clear;
  public
    constructor Create(ACount: Integer);
    destructor Destroy; override;
    function AddItem: TcxAutoWidthItem;
    procedure Calculate;
    property AvailableWidth: Integer read FAvailableWidth write FAvailableWidth;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxAutoWidthItem read GetItem; default;
    property Width: Integer read GetWidth;
  end;

  { TcxAlignment }

  TcxAlignment = class(TPersistent)
  private
    FDefaultHorz: TAlignment;
    FDefaultVert: TcxAlignmentVert;
    FHorz: TAlignment;
    FIsHorzAssigned: Boolean;
    FIsVertAssigned: Boolean;
    FOwner: TPersistent;
    FUseAssignedValues: Boolean;
    FVert: TcxAlignmentVert;
    FOnChanged: TNotifyEvent;
    function IsHorzStored: Boolean;
    function IsVertStored: Boolean;
    procedure SetHorz(const Value: TAlignment);
    procedure SetVert(const Value: TcxAlignmentVert);
  protected
    procedure DoChanged;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AUseAssignedValues: Boolean = False;
      ADefaultHorz: TAlignment = taLeftJustify;
      ADefaultVert: TcxAlignmentVert = vaTop); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Horz: TAlignment read FHorz write SetHorz stored IsHorzStored;
    property Vert: TcxAlignmentVert read FVert write SetVert stored IsVertStored;
  end;

  { Object Links }

  TcxObjectLink = class
    Ref: TObject;
  end;

  TcxObjectLinkController = class
  strict private
    FLinks: TdxHashSet<TcxObjectLink>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddLink(AObject: TObject): TcxObjectLink;
    procedure RemoveLink(ALink: TcxObjectLink);
    procedure ClearLinks(AObject: TObject);
  end;

  { MRU items support }

  TcxMRUItemClass = class of TcxMRUItem;

  TcxMRUItem = class
  public
    function Equals(AItem: TcxMRUItem): Boolean; reintroduce; virtual; abstract;
  end;

  TcxMRUItems = class
  private
    FAllowDuplicates: Boolean;
    FItems: TList;
    FMaxCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxMRUItem;
    procedure SetCount(Value: Integer);
    procedure SetMaxCount(Value: Integer);
  protected
    procedure Delete(AIndex: Integer);
    procedure UpdateCount;

    property AllowDuplicates: Boolean read FAllowDuplicates write FAllowDuplicates;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AItem: TcxMRUItem);
    procedure ClearItems;
    function IndexOf(AItem: TcxMRUItem): Integer;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TcxMRUItem read GetItem; default;
    property MaxCount: Integer read FMaxCount write SetMaxCount;
  end;

  { Open list }

  TcxOpenList = class(TList)
  private
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; Value: TObject);
  public
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  { TcxComponentCollectionItem }

  TcxComponentCollection = class;

  TcxComponentCollectionItem = class(TcxCustomComponent)
  private
    FCollection: TcxComponentCollection;
    FID: Integer;
  protected
    procedure AddToCollection(ACollection: TcxComponentCollection);
    procedure Changed(AAllItems: Boolean);
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; virtual; abstract;
    function GetDisplayName: string; virtual;
    function GetIndex: Integer;
    procedure RemoveFromCollection(ACollection: TcxComponentCollection);
    procedure SetCollection(AValue: TcxComponentCollection); virtual;
    procedure SetIndex(AValue: Integer); virtual;
  public
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetParentComponent(Value: TComponent); override;

    property Collection: TcxComponentCollection read FCollection write SetCollection;
    property DisplayName: string read GetDisplayName;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
  end;

  TcxComponentCollectionItemClass = class of TcxComponentCollectionItem;

  { TcxComponentCollection }

  TcxComponentCollectionNotification = (ccnAdded, ccnChanged, ccnExtracting, ccnExtracted, ccnDeleting);
  TcxComponentCollectionChangeEvent = procedure(Sender: TObject;
    AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification) of object;

  TcxComponentCollection = class(TPersistent)
  private
    FItemClass: TcxComponentCollectionItemClass;
    FItems: TList;
    FNextID: Integer;
    FParentComponent: TComponent;
    FUpdateCount: Integer;
    FOnChange: TcxComponentCollectionChangeEvent;
  protected
    function GetOwner: TPersistent; override;

    function AddItem(AItemClass: TcxComponentCollectionItemClass; AParentComponent, AOwner: TComponent): TcxComponentCollectionItem; overload;
    function AddInternalItem: TcxComponentCollectionItem;
    function GetCount: Integer;
    procedure InsertItem(AItem: TcxComponentCollectionItem);
    procedure RemoveItem(AItem: TcxComponentCollectionItem);

    procedure Changed(AItem: TcxComponentCollectionItem = nil; AAction: TcxComponentCollectionNotification = ccnChanged);
    function GetItem(AIndex: Integer): TcxComponentCollectionItem;
    function GetParentControl: TControl; virtual;
    function GetItemPrefixName: string; virtual;
    function GetItemSuffixName: string; virtual;
    procedure Notify(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); virtual;
    procedure ReadOldCollection(AReader: TReader);
    procedure SetItem(AIndex: Integer; Value: TcxComponentCollectionItem);
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); virtual;
    procedure Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); virtual;

    property NextID: Integer read FNextID;
    property UpdateCount: Integer read FUpdateCount;
    property ItemClass: TcxComponentCollectionItemClass read FItemClass;
  public
    constructor Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add: TcxComponentCollectionItem; overload;
    function Add(AItemClass: TcxComponentCollectionItemClass): TcxComponentCollectionItem; overload;
    procedure BeginUpdate; virtual;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure EndUpdate(AForceUpdate: Boolean = True); virtual;
    function FindItemByID(ID: Integer): TcxComponentCollectionItem;
    function IndexOf(AItem: TcxComponentCollectionItem): Integer;
    function Insert(AIndex: Integer): TcxComponentCollectionItem;
    procedure Remove(AItem: TcxComponentCollectionItem);

    property Count: Integer read GetCount;
    property ParentComponent: TComponent read FParentComponent;
    property Items[AIndex: Integer]: TcxComponentCollectionItem read GetItem write SetItem; default;
    property OnChange: TcxComponentCollectionChangeEvent read FOnChange write FOnChange;
  end;

  { TcxInterfacedComponentCollection }

  TcxInterfacedComponentCollection = class(TcxComponentCollection, IUnknown)
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  { TcxDialogMetricsInfo }

  IcxDialogMetricsInfoData = interface
    ['{DBFB3040-4677-4C9D-913C-45A1EE41E35A}']
    function GetInfoData: Pointer;
    function GetInfoDataSize: Integer;
    procedure SetInfoData(AData: Pointer);
  end;

  IcxDialogMetricsClientSize = interface
    ['{A68C1688-1F54-4E26-8716-A95AB50A7711}']
    function GetClientSize: TSize;
    procedure SetClientSize(const Value: TSize);
  end;

  TcxDialogMetricsInfo = class
  strict private
    FClientSize: TSize;
    FData: Pointer;
    FDialogClass: TClass;
    FLeft: Integer;
    FMaximized: Boolean;
    FTop: Integer;

    procedure FreeCustomData;
  protected
    procedure Restore(AForm: TForm);
    procedure Store(AForm: TForm);
  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;
    //
    property ClientSize: TSize read FClientSize;
    property Data: Pointer read FData;
    property DialogClass: TClass read FDialogClass;
    property Left: Integer read FLeft;
    property Maximized: Boolean read FMaximized;
    property Top: Integer read FTop;
  end;

  { TcxDialogsMetricsStore }

  TcxDialogsMetricsStore = class
  private
    FDefaultPosition: TPosition;
    FMetrics: TcxObjectList;
  protected
    function CreateMetrics(AForm: TForm): TcxDialogMetricsInfo;
    function FindMetrics(AForm: TForm): Integer;
    property Metrics: TcxObjectList read FMetrics;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure InitDialog(AForm: TForm);
    procedure StoreMetrics(AForm: TForm);

    property DefaultPosition: TPosition read FDefaultPosition write FDefaultPosition default poMainFormCenter;
  end;

  { TcxEvent }

  TcxEvent = class(TObject)
  private
    FHandle: THandle;
    function WaitForEvent(ATimeOut: Cardinal): Cardinal;
  public
    constructor Create(AManualReset, AInitialState: Boolean);
    destructor Destroy; override;
    function WaitFor(TimeOut: Cardinal): TWaitResult;
    procedure ResetEvent;
    procedure SetEvent;
    //
    property Handle: THandle read FHandle;
  end;

  { TcxCustomThread }

  TcxCustomThread = class(TThread)
  private
    FException: Exception;
    procedure DoHandleException;
  protected
    procedure HandleException; virtual;
    procedure ResetException;
  end;

  { TcxThread }

  TcxThread = class(TcxCustomThread)
  private
    FCheckingForPause: Boolean;
    FDestroying: Boolean;
    FPaused: Boolean;
    FPauseEvent: TcxEvent;
    FRTLCriticalSection: TRTLCriticalSection;
    FWaitForPauseEvent: TcxEvent;
    function GetRunning: Boolean;
  protected
    procedure CheckForPause;

    property Destroying: Boolean read FDestroying;
    property Paused: Boolean read FPaused;
    property Running: Boolean read GetRunning;
  public
    constructor Create(ACreateSuspended: Boolean; ACreatePaused: Boolean = False);
    destructor Destroy; override;
    procedure Pause(AWaitForPause: Boolean = False); virtual;
    procedure Unpause; virtual;
  end;

  { TcxComponentList }

  TcxComponentListNotifyEvent = procedure (Sender: TObject; AComponent: TComponent; AAction: TListNotification) of object;
  TcxComponentListChangeEvent = procedure (Sender: TObject; AComponent: TComponent; AAction: TcxComponentCollectionNotification) of object;

  TcxComponentList = class(TComponentList)
  private
    FUpdateCount: Integer;
    FOnComponentListChanged: TcxComponentListChangeEvent;
    FOnNotify: TcxComponentListNotifyEvent;
  protected
    procedure DoNotify(AItem: TComponent; AAction: TListNotification); virtual;
    function GetItemClass: TClass; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Update(AItem: TComponent = nil; AAction: TcxComponentCollectionNotification = ccnChanged);
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure Clear; override;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    property OnComponentListChanged: TcxComponentListChangeEvent read FOnComponentListChanged write FOnComponentListChanged;
    property OnNotify: TcxComponentListNotifyEvent read FOnNotify write FOnNotify;
  end;

  { TcxCustomProgressCalculationHelper }

  TcxCustomProgressCalculationHelper = class
  strict private
    FIsAborting: Boolean;
    FProgress: Integer;
    FStage: Integer;
    FStageCount: Integer;
    FStageTaskCount: Integer;
    FStageTaskNumber: Integer;

    procedure CalculateProgress;
    procedure SetProgress(AValue: Integer);
  protected
    procedure ProgressChanged; virtual; abstract;
  public
    constructor Create(AStageCount: Integer);
    procedure BeforeDestruction; override;
    procedure BeginStage(ATaskCount: Integer);
    procedure EndStage;
    procedure NextTask(ASkipCount: Integer = 1);
    procedure SetTaskNumber(AValue: Integer);
    procedure SkipStage;
    //
    property Progress: Integer read FProgress;
    property Stage: Integer read FStage;
  end;

  { TdxMemIniFile }

  TdxMemIniFile = class(TMemIniFile)
  public
    constructor Create; overload;
    constructor Create(AStream: TStream); overload;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream); overload;
    procedure SaveToStream(AStream: TStream; AEncoding: TEncoding); overload;
  end;

  { IcxProgress }

  IcxProgress = interface
  ['{B54F46D5-7BF5-4B5F-9868-4A042E0E975A}']
    procedure OnProgress(Sender: TObject; Percent: Integer);
  end;

  { TcxProgressCalculationHelper }

  TcxProgressEvent = procedure (Sender: TObject; Percent: Integer) of object;

  TcxProgressCalculationHelper = class(TcxCustomProgressCalculationHelper)
  strict private
    FEventSender: TObject;

    FOnProgress: TcxProgressEvent;
  protected
    procedure ProgressChanged; override;
  public
    constructor Create(AStageCount: Integer; AEventSender: TObject; AProgressEvent: TcxProgressEvent);
  end;

  { TdxLanguages }

  TcxNotifyProcedure = procedure(Sender: TObject);
  TcxNotifyEvent = procedure(Sender: TObject; ANotificationText: string) of object;

  TcxLangRec = packed record
    FName: string;
    FLCID: LCID;
    FExt: string;
    FLocaleName: string;
  end;

  TdxLanguages = class
  private
    FSysLangs: array of TcxLangRec;
    function LocalesCallback(LocaleID: PChar): Integer; stdcall;
    function GetCount: integer;
    function GetExt(Index: Integer): string;
    function GetID(Index: Integer): string;
    function GetLocaleID(Index: Integer): LCID;
    function GetLocaleName(Index: Integer): string;
    function GetName(Index: Integer): string;
    function GetNameFromLocaleID(ID: LCID): string;
    function GetNameFromLCID(const ID: string): string;
  public
    constructor Create;
    function GetDefaultLanguageLCID: LCID;
    function GetLCID(const AName: string): LCID;
    function IndexOf(ID: LCID): Integer; overload;
    function IndexOf(const ALocaleName: string): Integer; overload;
    property Count: Integer read GetCount;
    property Name[Index: Integer]: string read GetName;
    property NameFromLCID[const ID: string]: string read GetNameFromLCID;
    property NameFromLocaleID[ID: LCID]: string read GetNameFromLocaleID;
    property ID[Index: Integer]: string read GetID;
    property LocaleID[Index: Integer]: LCID read GetLocaleID;
    property LocaleName[Index: Integer]: string read GetLocaleName;
    property Ext[Index: Integer]: string read GetExt;
  end;

  { TdxLanguageID }

  TdxLanguageID = class sealed
  public const
    None = 0;
    Arabic = 1025;
    Azerbaijani = 1068;
    Bulgarian = 1026;
    Catalan = 1027;
    Czech = 1029;
    Danish = 1030;
    German = 1031;
    Greek = 1032;
    English = 1033;
    Spanish = 1034;
    Basque = 1069;
    Galician = 1110;
    French = 1036;
    Croatian = 1050;
    Hungarian = 1038;
    Italian = 1040;
    Latvian = 1062;
    Dutch = 1043;
    Polish = 1045;
    Portuguese = 2070;
    Russian = 1049;
    Swedish = 1053;
    Turkish = 1055;
    Ukrainian = 1058;
  strict private
    type
      TLangRec = record
        ShortName: PWideChar;
        LanguageID: Integer;
      end;
    const
      LangMap: array[0..23] of TLangRec = (
        (ShortName: 'ar'; LanguageID: TdxLanguageID.Arabic),
        (ShortName: 'az'; LanguageID: TdxLanguageID.Azerbaijani),
        (ShortName: 'bg'; LanguageID: TdxLanguageID.Bulgarian),
        (ShortName: 'ca'; LanguageID: TdxLanguageID.Catalan),
        (ShortName: 'cs'; LanguageID: TdxLanguageID.Czech),
        (ShortName: 'da'; LanguageID: TdxLanguageID.Danish),
        (ShortName: 'de'; LanguageID: TdxLanguageID.German),
        (ShortName: 'el'; LanguageID: TdxLanguageID.Greek),
        (ShortName: 'en'; LanguageID: TdxLanguageID.English),
        (ShortName: 'es'; LanguageID: TdxLanguageID.Spanish),
        (ShortName: 'eu'; LanguageID: TdxLanguageID.Basque),
        (ShortName: 'gl'; LanguageID: TdxLanguageID.Galician),
        (ShortName: 'fr'; LanguageID: TdxLanguageID.French),
        (ShortName: 'hr'; LanguageID: TdxLanguageID.Croatian),
        (ShortName: 'hu'; LanguageID: TdxLanguageID.Hungarian),
        (ShortName: 'it'; LanguageID: TdxLanguageID.Italian),
        (ShortName: 'lv'; LanguageID: TdxLanguageID.Latvian),
        (ShortName: 'nl'; LanguageID: TdxLanguageID.Dutch),
        (ShortName: 'pl'; LanguageID: TdxLanguageID.Polish),
        (ShortName: 'pt'; LanguageID: TdxLanguageID.Portuguese),
        (ShortName: 'ru'; LanguageID: TdxLanguageID.Russian),
        (ShortName: 'sv'; LanguageID: TdxLanguageID.Swedish),
        (ShortName: 'tr'; LanguageID: TdxLanguageID.Turkish),
        (ShortName: 'uk'; LanguageID: TdxLanguageID.Ukrainian));
  public
    class function GetLanguageID(const ALanguage: PWideChar): Integer; static;
  end;

// component dialogs metrics storage
function cxDialogsMetricsStore: TcxDialogsMetricsStore;

function cxAddObjectLink(AObject: TObject): TcxObjectLink;
procedure cxRemoveObjectLink(ALink: TcxObjectLink);
procedure cxClearObjectLinks(AObject: TObject);
procedure cxAddFreeNotification(ARecipient, ASender: TComponent); inline;
procedure cxRemoveFreeNotification(ARecipient, ASender: TComponent); inline;
function cxIsDestroying(AComponent: TComponent; ARecursive: Boolean = False): Boolean;
procedure cxBroadcastRemoveNotifications(AComponent: TComponent);

procedure CallNotify(ANotifyEvent: TNotifyEvent; ASender: TObject);
function ClassInheritsFrom(AClass: TClass; const AParentClassName: string): Boolean;
procedure FillStringsWithEnumTypeValues(AStrings: TStrings; ATypeInfo: PTypeInfo;
  AGetTypeItemCaption: TcxGetCaptionForIntegerItemFunc);
function GetPersistentOwner(APersistent: TPersistent): TPersistent;
function GetSubobjectName(AObject, ASubobject: TPersistent): string;
function GetValidName(AComponent: TComponent; const AName: string;
  AIsBaseName: Boolean = False): string;
procedure RenameComponents(ACaller, AOwner: TComponent;
  ANewName: TComponentName; const AOldName: TComponentName;
  AComponentCount: Integer; AGetComponent: TcxGetComponent);
function RoundDiv(I1, I2: Integer): Integer;
function Size(cx, cy: Longint): TSize;
procedure SwapIntegers(var I1, I2: Integer);
function GetRangeCenter(ABound1, ABound2: Integer): Integer;
function StreamsEqual(AStream1, AStream2: TMemoryStream): Boolean;
function GetCorrectPath(const S: string): string;

function cxGetClassName(AWnd: THandle): string;
function cxGetUnitName(AClass: TClass): string;
function cxStrCharLength(const AStr: string; AIndex: Integer = 1): Integer;
function cxNextCharPos(const AStr: string; AIndex: Integer): Integer;
function cxPrevCharPos(const AStr: string; AIndex: Integer): Integer;

function dxLanguages: TdxLanguages;

function CreateUniqueName(AOwnerForm, AOwnerComponent, AComponent: TComponent;
  const APrefixName, ASuffixName: string; ABaseIndex: Integer = -1): string;
function cxGenerateComponentName(AOwnerComponent: TComponent;
  const AClassName, APrefixName, ASuffixName: string; ANumber: Integer): string;

function cxSign(const AValue: Double): Integer;
function cxSendStructMessage(AHandle: THandle; AMsg: UINT; WParam: WPARAM; const LParam): LRESULT; overload;
function cxSendStructMessage(AHandle: THandle; AMsg: UINT; const WParam; LParam: LParam): LRESULT; overload;

function cxCreateTimer(AOnTimer: TNotifyEvent; AInterval: Integer = 1000; AEnabled: Boolean = True): TcxTimer;

var
  cxDesignHelper: IcxDesignHelper;

implementation

uses
  RTLConsts, Math, Graphics, cxGraphics, cxControls, dxHashUtils, dxHash, dxDPIAwareUtils;

type
  TPersistentAccess = class(TPersistent);
  TComponentAccess = class(TComponent);
  TReaderAccess = class(TReader);

  TcxTimerWindow = class(TcxMessageWindow)
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

var
  FUnitIsFinalized: Boolean;
  FObjectLinkController: TcxObjectLinkController;
  FDialogsMetrics: TcxDialogsMetricsStore;
  FcxTimerWindow: TcxTimerWindow;
  FActiveTimerList: TdxFastList;
  FdxLanguages, FTmpLanguages: TdxLanguages;

function cxDialogsMetricsStore: TcxDialogsMetricsStore;
begin
  if FDialogsMetrics = nil then
    FDialogsMetrics := TcxDialogsMetricsStore.Create;
  Result := FDialogsMetrics;
end;

function cxAddObjectLink(AObject: TObject): TcxObjectLink;
begin
  if AObject <> nil then
    Result := FObjectLinkController.AddLink(AObject)
  else
    Result := nil;
end;

procedure cxRemoveObjectLink(ALink: TcxObjectLink);
begin
  if ALink <> nil then
    FObjectLinkController.RemoveLink(ALink);
end;

procedure cxClearObjectLinks(AObject: TObject);
begin
  if FObjectLinkController <> nil then
    FObjectLinkController.ClearLinks(AObject);
end;

procedure cxAddFreeNotification(ARecipient, ASender: TComponent);
begin
  if ASender <> nil then
    ASender.FreeNotification(ARecipient);
end;

procedure cxRemoveFreeNotification(ARecipient, ASender: TComponent);
begin
  if ASender <> nil then
    ASender.RemoveFreeNotification(ARecipient);
end;

function cxIsDestroying(AComponent: TComponent; ARecursive: Boolean = False): Boolean;
begin
  Result := (AComponent <> nil) and ((csDestroying in AComponent.ComponentState) or ARecursive and cxIsDestroying(AComponent.Owner));
end;

procedure cxBroadcastRemoveNotifications(AComponent: TComponent);
begin
  TComponentAccess(AComponent).RemoveFreeNotifications;

  if AComponent.Owner <> nil then
    TComponentAccess(AComponent.Owner).Notification(AComponent, opRemove);
end;

procedure InternalEnumLanguages(ALocaleID: PChar); stdcall;
begin
  FTmpLanguages.LocalesCallback(ALocaleID);
end;

function GetShortHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, 1, I - 1);
end;

function GetLongHint(const Hint: string): string;
var
  I: Integer;
begin
  I := AnsiPos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, I + 1, Maxint);
end;

function cxGetClassName(AWnd: THandle): string;
var
  ABuffer: array[0..256] of Char;
begin
  FillChar(ABuffer, SizeOf(ABuffer), 0);
  if GetClassName(AWnd, @ABuffer[0], Length(ABuffer)) > 0 then
    Result := ABuffer
  else
    Result := '';
end;

function GetPChar(const AStr: string; AIndex: Integer): PChar;
begin
  Result := PChar(AStr) + AIndex - 1;
end;

function cxStrCharLength(const AStr: string; AIndex: Integer = 1): Integer;
begin
  Result := Integer(CharNext(GetPChar(AStr, AIndex))) - Integer(GetPChar(AStr, AIndex));
end;

function cxNextCharPos(const AStr: string; AIndex: Integer): Integer;
begin
  Result := Integer(CharNext(GetPChar(AStr, AIndex))) - Integer(GetPChar(AStr, 1)) + 1;
end;

function cxPrevCharPos(const AStr: string; AIndex: Integer): Integer;
begin
  Result := Integer(CharPrev(GetPChar(AStr, 1), GetPChar(AStr, AIndex))) - Integer(GetPChar(AStr, 1)) + 1;
end;

{ TcxCustomComponent }

procedure TcxCustomComponent.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TcxCustomComponent.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxBroadcastRemoveNotifications(Self);
end;

procedure TcxCustomComponent.GetAdornerTargetElements(AList: TStrings);
begin
//do nothing
end;

{ TcxComponent }

destructor TcxComponent.Destroy;
begin
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TcxComponent.Loaded;
begin
  inherited Loaded;
  UpdateFakeLinks;
end;

procedure TcxComponent.GetFakeComponentLinks(AList: TList);
begin
end;

procedure TcxComponent.UpdateFakeLinks;
var
  I: Integer;
  AList: TList;
begin
  if not IsDesigning or IsDestroying or (Owner = nil) then Exit;
  AList := TList.Create;
  try
    GetFakeComponentLinks(AList);
    for I := 0 to GetFakeComponentLinkCount - 1 do
      if I < AList.Count then
        SetFakeComponentLink(I, TComponent(AList[I]))
      else
        SetFakeComponentLink(I, nil);
  finally
    AList.Free;
  end;
end;

function TcxComponent.GetFakeComponentLinkCount: Integer;
begin
  Result := 3;
end;

function TcxComponent.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TcxComponent.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxComponent.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TcxComponent.SetFakeComponentLink(Index: Integer; Value: TComponent);
begin
  case Index of
    0: FFakeComponentLink1 := Value;
    1: FFakeComponentLink2 := Value;
    2: FFakeComponentLink3 := Value;
  end;
end;

{ TcxTimer }

function ActiveTimerList: TdxFastList;
begin
  if (FActiveTimerList = nil) and not FUnitIsFinalized then
    FActiveTimerList := TdxFastList.Create;
  Result := FActiveTimerList;
end;

function cxTimerWindow: TcxTimerWindow;
begin
  if (FcxTimerWindow = nil) and not FUnitIsFinalized then
    FcxTimerWindow := TcxTimerWindow.Create;
  Result := FcxTimerWindow;
end;

procedure TcxTimerWindow.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_TIMER then
  begin
    if ActiveTimerList.IndexOf(Pointer(Message.WParam)) <> -1 then
      TcxTimer(Message.WParam).TimeOut;
  end
  else
    inherited WndProc(Message);
end;

constructor TcxTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  FEventID := TdxNativeUInt(Self);
end;

destructor TcxTimer.Destroy;
begin
  KillTimer;
  inherited Destroy;
end;

procedure TcxTimer.Reset;
begin
  if Enabled then
    UpdateTimer;
end;

procedure TcxTimer.TimeOut;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

function TcxTimer.CanSetTimer: Boolean;
begin
  Result := FEnabled and not TimerOn and Assigned(FOnTimer);
end;

procedure TcxTimer.KillTimer;
begin
  if TimerOn then
  begin
    TimerOn := False;
    if cxTimerWindow <> nil then
      Windows.KillTimer(cxTimerWindow.Handle, FEventID);
  end;
end;

procedure TcxTimer.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TcxTimer.SetInterval(Value: Cardinal);
begin
  if (FInterval <> Value) and (Value > 0) then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TcxTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TcxTimer.SetTimer;
begin
  if CanSetTimer then
    TimerOn := Windows.SetTimer(cxTimerWindow.Handle, FEventID, FInterval, nil) <> 0;
end;

procedure TcxTimer.SetTimerOn(Value: Boolean);
begin
  if FTimerOn <> Value then
  begin
    if ActiveTimerList <> nil then
      if Value then
        ActiveTimerList.Add(Pointer(FEventID))
      else
        ActiveTimerList.Remove(Pointer(FEventID));
    FTimerOn := Value;
  end;
end;

procedure TcxTimer.UpdateTimer;
begin
  KillTimer;
  SetTimer;
end;

{ TcxScalableComponent }

constructor TcxScalableComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScalable := True;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(dxGetCurrentDPI(Owner), dxDefaultDPI);
end;

destructor TcxScalableComponent.Destroy;
begin
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxScalableComponent.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('PixelsPerInch', ReadPixelsPerInch, WritePixelsPerInch, True);
end;

procedure TcxScalableComponent.Loaded;
var
  ATargetDPI: Integer;
begin
  inherited Loaded;
  ATargetDPI := dxGetCurrentDPI(Owner);
  ScaleForPPI(ATargetDPI);
  ScaleFactor.Assign(ATargetDPI, dxDefaultDPI);
end;

procedure TcxScalableComponent.ChangeScale(M, D: Integer);
begin
  ScaleFactor.Change(M, D);
end;

procedure TcxScalableComponent.ScaleForPPI(TargetPPI: Integer);
begin
  if Scalable and (TargetPPI <> PixelsPerInch) then
    ChangeScale(TargetPPI, PixelsPerInch);
end;

procedure TcxScalableComponent.ReadPixelsPerInch(Reader: TReader);
begin
  ScaleFactor.Assign(Reader.ReadInteger, dxDefaultDPI);
end;

procedure TcxScalableComponent.WritePixelsPerInch(Writer: TWriter);
begin
  Writer.WriteInteger(PixelsPerInch);
end;

function TcxScalableComponent.GetPixelsPerInch: Integer;
begin
  Result := ScaleFactor.Apply(dxDefaultDPI);
end;

function TcxScalableComponent.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

{ TcxInterfacedCollectionItem }

procedure TcxInterfacedCollectionItem.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetOwner <> nil then
    GetOwner.GetInterface(IUnknown, FOwnerInterface);
end;

function TcxInterfacedCollectionItem._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := -1;
end;

function TcxInterfacedCollectionItem._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
    Result := -1;
end;

function TcxInterfacedCollectionItem.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

{ TcxOwnedInterfacedCollection }

destructor TcxOwnedInterfacedCollection.Destroy;
begin
  Include(FInternalState, cisDestroying);
  inherited;
end;

procedure TcxOwnedInterfacedCollection.Update(Item: TCollectionItem);
begin
  if cisUpdating in FInternalState then Exit;
  Include(FInternalState, cisUpdating);
  try
    if Sorted and (Item = nil) then
      Sort;
    inherited;
    if not IsDestroying and Assigned(OnChange) then
      OnChange(Self, Item);
  finally
    Exclude(FInternalState, cisUpdating);
  end;
end;

type
  TcxStringListCompareEvent = function(AList: TStringList; AIndex1, AIndex2: Integer): Integer of object;

  TcxStringList = class(TStringList)
  private
    FOnCompare: TcxStringListCompareEvent;
  public
    procedure Sort; override;
    property OnCompare: TcxStringListCompareEvent read FOnCompare write FOnCompare;
  end;

{ TcxStringList }

function cxStringListCompareStrings(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
begin
  Result := TcxStringList(AList).OnCompare(AList, AIndex1, AIndex2);
end;

procedure TcxStringList.Sort;
begin
  if Assigned(OnCompare) then
    CustomSort(cxStringListCompareStrings)
  else
    inherited;
end;

function TcxOwnedInterfacedCollection.DoCompareItems(AItem1, AItem2: TcxInterfacedCollectionItem): Integer;
begin
  Result := Sign(AItem1.Index - AItem2.Index);
end;

procedure TcxOwnedInterfacedCollection.Sort;
var
  AList: TcxStringList;
  I: Integer;
begin
  if (UpdateCount > 0) or (cisSorting in FInternalState) then Exit;
  Include(FInternalState, cisSorting);
  try
    AList := TcxStringList.Create;
    try
      for I := 0 to Count - 1 do
        AList.AddObject(IntToStr(I), Items[I]);

      AList.OnCompare := CompareItems;
      AList.Sort;

      BeginUpdate;
      try
        for I := 0 to AList.Count - 1 do
          TcxInterfacedCollectionItem(AList.Objects[I]).Index := I;
      finally
        EndUpdate;
      end;
    finally
      AList.Free;
    end;
  finally
    Exclude(FInternalState, cisSorting);
  end;
end;

function TcxOwnedInterfacedCollection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TcxOwnedInterfacedCollection._AddRef: Integer;
begin
  Result := -1;
end;

function TcxOwnedInterfacedCollection._Release: Integer;
begin
  Result := -1;
end;

function TcxOwnedInterfacedCollection.CompareItems(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
var
  AItem1, AItem2: TcxInterfacedCollectionItem;
begin
  AItem1 := TcxInterfacedCollectionItem(AList.Objects[AIndex1]);
  AItem2 := TcxInterfacedCollectionItem(AList.Objects[AIndex2]);
  if Assigned(OnCompare) then
    Result := OnCompare(Self, AItem1, AItem2)
  else
    Result := DoCompareItems(AItem1, AItem2);
end;

function TcxOwnedInterfacedCollection.GetIsDestroying: Boolean;
begin
  Result := cisDestroying in FInternalState;
end;

procedure TcxOwnedInterfacedCollection.SetSorted(AValue: Boolean);
begin
  if Sorted <> AValue then
  begin
    FSorted := AValue;
    if Sorted then
      Sort;
  end;
end;

{ TcxObjectList }

constructor TcxObjectList.Create(AOwnObjects: Boolean = True);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
end;

procedure TcxObjectList.Clear;
var
  I: Integer;
begin
  if OwnObjects then
  begin
    for I := 0 to Count - 1 do
      FreeItem(I);
  end;
  inherited Clear;
end;

procedure TcxObjectList.FreeAndDelete(Index: Integer);
begin
  Items[Index].Free;
  Delete(Index);
end;

function TcxObjectList.FreeAndRemove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if Result >= 0 then
    FreeAndDelete(Result);
end;

procedure TcxObjectList.FreeItem(AIndex: Integer);
begin
  Items[AIndex].Free;
end;

function TcxObjectList.GetItem(Index: Integer): TObject;
begin
  Result := inherited Items[Index];
end;

{ TcxEventHandlerCollection }

procedure TcxEventHandlerCollection.Add(AEvent: TcxEventHandler);
var
  ALength: Integer;
begin
  if IndexOf(AEvent) <> -1 then Exit;
  ALength := Length(FEvents);
  SetLength(FEvents, ALength + 1);
  FEvents[ALength] := AEvent;
end;

procedure TcxEventHandlerCollection.CallEvents(Sender: TObject; const AEventArgs);
var
  I: Integer;
begin
  for I := Low(FEvents) to High(FEvents) do
    FEvents[I](Sender, AEventArgs);
end;

procedure TcxEventHandlerCollection.CallEvents(Sender: TObject);
var
  AEventArgs: Pointer;
begin
  AEventArgs := nil;
  CallEvents(Sender, AEventArgs);
end;

procedure TcxEventHandlerCollection.Delete(AIndex: Integer);
var
  ALength, I: Integer;
begin
  ALength := Length(FEvents);
  if (AIndex < 0) or (AIndex >= ALength) then Exit;
  for I := AIndex to ALength - 2 do
    FEvents[I] := FEvents[I + 1];
  SetLength(FEvents, ALength - 1);
end;

function TcxEventHandlerCollection.IndexOf(AEvent: TcxEventHandler): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FEvents) to High(FEvents) do
    if dxSameMethods(TMethod(AEvent), TMethod(FEvents[I])) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TcxEventHandlerCollection.Remove(AEvent: TcxEventHandler);
begin
  Delete(IndexOf(AEvent));
end;

{ TcxRegisteredClassList }

constructor TcxRegisteredClassList.Create;
begin
  inherited Create;
  FItems := TdxFastList.Create;
end;

destructor TcxRegisteredClassList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxRegisteredClassList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TcxRegisteredClassListItemData(FItems[I]).Free;
  FItems.Clear;
end;

function TcxRegisteredClassList.FindClass(AItemClass: TClass): TClass;
var
  AIndex: Integer;
begin
  if Find(AItemClass, AIndex) then
    Result := Items[AIndex].RegisteredClass
  else
    Result := nil;
end;

procedure TcxRegisteredClassList.Register(AItemClass, ARegisteredClass: TClass);
var
  AIndex: Integer;
  AData: TcxRegisteredClassListItemData;
begin
  AIndex := -1;
  AData := TcxRegisteredClassListItemData.Create;
  AData.ItemClass := AItemClass;
  AData.RegisteredClass := ARegisteredClass;
  if Find(AItemClass, AIndex) then
    FItems.Insert(AIndex + 1, AData)
  else
    if AIndex <> -1 then
      FItems.Insert(AIndex, AData)
    else
      FItems.Add(AData);
end;

procedure TcxRegisteredClassList.Unregister(AItemClass, ARegisteredClass: TClass);
var
  I: Integer;
  AData: TcxRegisteredClassListItemData;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    AData := Items[I];
    if (AData.ItemClass = AItemClass) and (AData.RegisteredClass = ARegisteredClass) then
    begin
      AData.Free;
      FItems.Delete(I);
    end;
  end;
end;

function TcxRegisteredClassList.Find(AItemClass: TClass; var AIndex: Integer): Boolean;
var
  I: Integer;
  AData: TcxRegisteredClassListItemData;
begin
  Result := False;
  for I := FItems.Count - 1 downto 0 do
  begin
    AData := Items[I];
    if AItemClass.InheritsFrom(AData.ItemClass) then
    begin
      AIndex := I;
      Result := True;
      Break;
    end
    else
      if AData.ItemClass.InheritsFrom(AItemClass) then
        AIndex := I;
  end;
end;

function TcxRegisteredClassList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxRegisteredClassList.GetItem(Index: Integer): TcxRegisteredClassListItemData;
begin
  Result := TcxRegisteredClassListItemData(FItems[Index]);
end;

{ TcxRegisteredClasses }

type
  TcxRegisteredClassesStringList = class(TStringList)
  public
    Owner: TcxRegisteredClasses;
  end;

constructor TcxRegisteredClasses.Create(ARegisterClasses: Boolean = False);
begin
  inherited Create;
  FRegisterClasses := ARegisterClasses;
  FItems := TcxRegisteredClassesStringList.Create;
  TcxRegisteredClassesStringList(FItems).Owner := Self;
end;

destructor TcxRegisteredClasses.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TcxRegisteredClasses.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxRegisteredClasses.GetDescription(Index: Integer): string;
begin
  Result := GetShortHint(FItems[Index]);
end;

function TcxRegisteredClasses.GetHint(Index: Integer): string;
begin
  Result := GetLongHint(FItems[Index]);
end;

function TcxRegisteredClasses.GetItem(Index: Integer): TClass;
begin
  Result := TClass(FItems.Objects[Index]);
end;

procedure TcxRegisteredClasses.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then Sort;
  end;
end;

function TcxRegisteredClasses.CompareItems(AIndex1, AIndex2: Integer): Integer;
begin
  Result := AnsiCompareText(Descriptions[AIndex1], Descriptions[AIndex2]);
end;

function SortClasses(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := TcxRegisteredClassesStringList(List).Owner.CompareItems(Index1, Index2);
end;

procedure TcxRegisteredClasses.Sort;
begin
  FItems.CustomSort(SortClasses);
end;

procedure TcxRegisteredClasses.Clear;
begin
  FItems.Clear;
end;

function TcxRegisteredClasses.FindByClassName(const AClassName: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ClassName = AClassName then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TcxRegisteredClasses.FindByDescription(const ADescription: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  if ADescription <> '' then
    for I := 0 to Count - 1 do
    begin
      if Descriptions[I] = ADescription then
      begin
        Result := Items[I];
        Break;
      end;
    end;
end;

function TcxRegisteredClasses.GetDescriptionByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Descriptions[AIndex];
end;

function TcxRegisteredClasses.GetHintByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Hints[AIndex];
end;

function TcxRegisteredClasses.GetIndexByClass(AClass: TClass): Integer;
begin
  Result := FItems.IndexOfObject(TObject(AClass));
end;

procedure TcxRegisteredClasses.Register(AClass: TClass; const ADescription: string);
begin
  if GetIndexByClass(AClass) = -1 then
  begin
    FItems.AddObject(ADescription, TObject(AClass));
    if FSorted then Sort;
    if FRegisterClasses then RegisterClass(TPersistentClass(AClass));
  end;
end;

procedure TcxRegisteredClasses.Unregister(AClass: TClass);
var
  I: Integer;
begin
  I := GetIndexByClass(AClass);
  if I <> -1 then
    FItems.Delete(I);
end;

{ TcxAutoWidthItem }

constructor TcxAutoWidthItem.Create;
begin
  inherited;
  AutoWidth := -1;
end;

{ TcxAutoWidthObject }

constructor TcxAutoWidthObject.Create(ACount: Integer);
begin
  inherited Create;
  FItems := TList.Create;
  FItems.Capacity := ACount;
end;

destructor TcxAutoWidthObject.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TcxAutoWidthObject.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxAutoWidthObject.GetItem(Index: Integer): TcxAutoWidthItem;
begin
  Result := TcxAutoWidthItem(FItems[Index]);
end;

function TcxAutoWidthObject.GetWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].Width);
end;

procedure TcxAutoWidthObject.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do Items[I].Free;
end;

function TcxAutoWidthObject.AddItem: TcxAutoWidthItem;
begin
  Result := TcxAutoWidthItem.Create;
  FItems.Add(Result);
end;

procedure TcxAutoWidthObject.Calculate;
var
  AAvailableWidth, AWidth, ANewAvailableWidth, ANewWidth, AOffset, I,
    AItemAutoWidth: Integer;
  AAssignAllWidths, AItemWithMinWidthFound: Boolean;

  procedure RemoveItemFromCalculation(AItem: TcxAutoWidthItem);
  begin
    with AItem do
    begin
      Dec(ANewAvailableWidth, AutoWidth);
      Dec(ANewWidth, Width);
    end;
  end;

  procedure ProcessFixedItems;
  var
    I: Integer;

    procedure ProcessItem(AItem: TcxAutoWidthItem);
    begin
      with AItem do
        if Fixed then
        begin
          AutoWidth := Width;
          RemoveItemFromCalculation(AItem);
        end;
    end;

  begin
    for I := 0 to Count - 1 do ProcessItem(Items[I]);
  end;

  {procedure ProcessFixedColumns;
  var
    AFixedIndex, I: Integer;
  begin
    if not (gcsColumnSizing in GridDefinition.Controller.State) then Exit;
    AFixedIndex :=
      (GridDefinition.Controller.DragAndDropObject as TcxGridColumnHeaderSizingObject).Column.VisibleIndex;
    if AFixedIndex = Count - 1 then Exit;
    for I := 0 to Count - 1 do
      if I <= AFixedIndex then
      begin
        AColumnWidth := Items[I].CalculateWidth;
        Items[I].Width := AColumnWidth;
        Dec(AAvailableWidth, AColumnWidth);
        Dec(AWidth, AColumnWidth);
      end;
  end;}

  procedure ProcessItem(AItem: TcxAutoWidthItem);

    function CalculateItemAutoWidth: Integer;
    begin
      Result :=
        MulDiv(AOffset + AItem.Width, AAvailableWidth, AWidth) -
        MulDiv(AOffset, AAvailableWidth, AWidth);
    end;

  begin
    AItemAutoWidth := CalculateItemAutoWidth;
    if AAssignAllWidths then
      AItem.AutoWidth := AItemAutoWidth
    else
      if AItemAutoWidth <= AItem.MinWidth then
      begin
        AItem.AutoWidth := AItem.MinWidth;
        RemoveItemFromCalculation(AItem);
        AItemWithMinWidthFound := True;
      end;
    Inc(AOffset, AItem.Width);
  end;

begin
  AAvailableWidth := FAvailableWidth;
  AWidth := Width;

  ANewAvailableWidth := AAvailableWidth;
  ANewWidth := AWidth;
  ProcessFixedItems;
  AAssignAllWidths := False;
  repeat
    AAvailableWidth := ANewAvailableWidth;
    AWidth := ANewWidth;
    AOffset := 0;
    AItemWithMinWidthFound := False;

    for I := 0 to Count - 1 do
      if Items[I].AutoWidth = -1 then ProcessItem(Items[I]);

    if not AItemWithMinWidthFound then
      AAssignAllWidths := not AAssignAllWidths;
  until (ANewWidth = 0) or not AItemWithMinWidthFound and not AAssignAllWidths;
end;

{ TcxAlignment }

constructor TcxAlignment.Create(AOwner: TPersistent; AUseAssignedValues: Boolean = False;
  ADefaultHorz: TAlignment = taLeftJustify; ADefaultVert: TcxAlignmentVert = vaTop);
begin
  inherited Create;
  FOwner := AOwner;
  FUseAssignedValues := AUseAssignedValues;
  FDefaultHorz := ADefaultHorz;
  FDefaultVert := ADefaultVert;
  FHorz := FDefaultHorz;
  FVert := FDefaultVert;
end;

procedure TcxAlignment.Assign(Source: TPersistent);
var
  AChanged: Boolean;
begin
  if Source is TcxAlignment then
    with Source as TcxAlignment do
    begin
      AChanged := Self.FHorz <> FHorz;
      Self.FHorz := FHorz;
      AChanged := AChanged or (Self.FVert <> FVert);
      Self.FVert := FVert;
      Self.FIsHorzAssigned := FIsHorzAssigned;
      Self.FIsVertAssigned := FIsVertAssigned;
      if AChanged then
        Self.DoChanged;
    end
  else
    inherited Assign(Source);
end;

procedure TcxAlignment.Reset;
var
  AChanged: Boolean;
begin
  FIsHorzAssigned := False;
  FIsVertAssigned := False;
  AChanged := FHorz <> FDefaultHorz;
  FHorz := FDefaultHorz;
  AChanged := AChanged or (FVert <> FDefaultVert);
  FVert := FDefaultVert;
  if AChanged then
    DoChanged;
end;

procedure TcxAlignment.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TcxAlignment.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TcxAlignment.IsHorzStored: Boolean;
begin
  if FUseAssignedValues then
    Result := FIsHorzAssigned
  else
    Result := FHorz <> FDefaultHorz;
end;

function TcxAlignment.IsVertStored: Boolean;
begin
  if FUseAssignedValues then
    Result := FIsVertAssigned
  else
    Result := FVert <> FDefaultVert;
end;

procedure TcxAlignment.SetHorz(const Value: TAlignment);
begin
  FIsHorzAssigned := True;
  if Value <> FHorz then
  begin
    FHorz := Value;
    DoChanged;
  end;
end;

procedure TcxAlignment.SetVert(const Value: TcxAlignmentVert);
begin
  FIsVertAssigned := True;
  if Value <> FVert then
  begin
    FVert := Value;
    DoChanged;
  end;
end;

{ TcxObjectLinkController }

constructor TcxObjectLinkController.Create;
begin
  inherited Create;
  FLinks := TdxHashSet<TcxObjectLink>.Create;
end;

destructor TcxObjectLinkController.Destroy;
begin
  FreeAndNil(FLinks);
  inherited Destroy;
end;

function TcxObjectLinkController.AddLink(AObject: TObject): TcxObjectLink;
begin
  Result := TcxObjectLink.Create;
  Result.Ref := AObject;
  FLinks.Include(Result);
end;

procedure TcxObjectLinkController.RemoveLink(ALink: TcxObjectLink);
begin
  if ALink.Ref <> nil then
    FLinks.Exclude(ALink);
  ALink.Free;
end;

procedure TcxObjectLinkController.ClearLinks(AObject: TObject);
var
  ALink: TcxObjectLink;
  ARemovedLinks: TdxFastList;
  I: Integer;
begin
  ARemovedLinks := nil;
  for ALink in FLinks do
  begin
    if ALink.Ref = AObject then
    begin
      ALink.Ref := nil;
      if ARemovedLinks = nil then
        ARemovedLinks := TdxFastList.Create;
      ARemovedLinks.Add(ALink);
    end;
  end;
  if ARemovedLinks <> nil then
  try
    for I := 0 to ARemovedLinks.Count - 1 do
      FLinks.Exclude(TcxObjectLink(ARemovedLinks.List[I]));
  finally
    ARemovedLinks.Free;
  end;
end;

{ TcxMRUItems }

constructor TcxMRUItems.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxMRUItems.Destroy;
begin
  ClearItems;
  FreeAndNil(FItems);
  inherited;
end;

function TcxMRUItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxMRUItems.GetItem(Index: Integer): TcxMRUItem;
begin
  Result := TcxMRUItem(FItems[Index]);
end;

procedure TcxMRUItems.SetCount(Value: Integer);
var
  I: Integer;
begin
  if Value < Count then
    for I := Count - 1 downto Value do
      Delete(I);
end;

procedure TcxMRUItems.SetMaxCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxCount <> Value then
  begin
    FMaxCount := Value;
    UpdateCount;
  end;
end;

procedure TcxMRUItems.Delete(AIndex: Integer);
begin
  Items[AIndex].Free;
  FItems.Delete(AIndex);
end;

procedure TcxMRUItems.UpdateCount;
begin
  if MaxCount <> 0 then Count := MaxCount;
end;

procedure TcxMRUItems.Add(AItem: TcxMRUItem);
var
  AIndex: Integer;
begin
  if AllowDuplicates then
    AIndex := -1
  else
    AIndex := IndexOf(AItem);
  if AIndex = -1 then
  begin
    FItems.Insert(0, AItem);
    UpdateCount;
  end
  else
  begin
    FItems.Move(AIndex, 0);
    AItem.Free;
  end;
end;

procedure TcxMRUItems.ClearItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Delete(I);
end;

function TcxMRUItems.IndexOf(AItem: TcxMRUItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Equals(AItem) then Exit;
  Result := -1;
end;

{ TcxOpenList }

function TcxOpenList.GetItem(Index: Integer): TObject;
begin
  Result := TObject(inherited Items[Index]);
end;

procedure TcxOpenList.SetItem(Index: Integer; Value: TObject);
begin
  Count := Max(Count, 1 + Index);
  inherited Items[Index] := Value;
end;

{ TcxComponentCollectionItem }

destructor TcxComponentCollectionItem.Destroy;
begin
  SetCollection(nil);
  inherited Destroy;
end;

procedure TcxComponentCollectionItem.Changed(AAllItems: Boolean);
begin
  if not (csDestroying in ComponentState) and (Collection <> nil) then
    if AAllItems then
      Collection.Changed
    else
      Collection.Changed(Self);
end;

function TcxComponentCollectionItem.GetDisplayName: string;
begin
  Result := Name;
end;

function TcxComponentCollectionItem.GetParentComponent: TComponent;
begin
  if Collection <> nil then
    Result := Collection.ParentComponent
  else
    Result := inherited GetParentComponent;
end;

function TcxComponentCollectionItem.HasParent: Boolean;
begin
  Result := GetParentComponent <> nil;
end;

procedure TcxComponentCollectionItem.SetParentComponent(Value: TComponent);
begin
  Collection := GetCollectionFromParent(Value);
end;

procedure TcxComponentCollectionItem.SetCollection(AValue: TcxComponentCollection);
begin
  if Collection <> AValue then
  begin
    RemoveFromCollection(Collection);
    AddToCollection(AValue);
  end;
end;

procedure TcxComponentCollectionItem.SetIndex(AValue: Integer);
var
  ACurIndex: Integer;
begin
  ACurIndex := GetIndex;
  if (ACurIndex >= 0) and (ACurIndex <> AValue) then
  begin
    Collection.FItems.Move(ACurIndex, AValue);
    Changed(True);
  end;
end;

procedure TcxComponentCollectionItem.AddToCollection(ACollection: TcxComponentCollection);
begin
  if ACollection <> nil then
    ACollection.InsertItem(Self);
end;

function TcxComponentCollectionItem.GetIndex: Integer;
begin
  if Collection <> nil then
    Result := Collection.FItems.IndexOf(Self)
  else
    Result := -1;
end;

procedure TcxComponentCollectionItem.RemoveFromCollection(ACollection: TcxComponentCollection);
begin
  if ACollection <> nil then
    ACollection.RemoveItem(Self);
end;

{ TcxComponentCollection }

constructor TcxComponentCollection.Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create;
  FParentComponent := AParentComponent;
  FItemClass := AItemClass;
  FItems := TList.Create;
end;

destructor TcxComponentCollection.Destroy;
begin
  FUpdateCount := 1;
  if FItems <> nil then
    Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TcxComponentCollection.Assign(Source: TPersistent);
var
  I: Integer;
  AItem: TcxComponentCollectionItem;
begin
  if Source is TcxComponentCollection then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TcxComponentCollection(Source).Count - 1 do
      begin
        AItem := TcxComponentCollection(Source).Items[I];
        Add(TcxComponentCollectionItemClass(AItem.ClassType)).Assign(TcxComponentCollection(Source).Items[I]);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxComponentCollection.Add: TcxComponentCollectionItem;
begin
  Result := Add(FItemClass);
end;

function TcxComponentCollection.Add(AItemClass: TcxComponentCollectionItemClass): TcxComponentCollectionItem;
begin
  Result := AddItem(AItemClass, ParentComponent, ParentComponent.Owner);
  SetItemName(Result);
end;

procedure TcxComponentCollection.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcxComponentCollection.Clear;
begin
  if FItems.Count = 0 then Exit;
  BeginUpdate;
  try
    while FItems.Count > 0 do
      TObject(FItems.Last).Free;
  finally
    EndUpdate;
  end;
end;

procedure TcxComponentCollection.Delete(AIndex: Integer);
begin
  Notify(Items[AIndex], ccnDeleting);
  Items[AIndex].Free;
end;

procedure TcxComponentCollection.EndUpdate(AForceUpdate: Boolean = True);
begin
  Dec(FUpdateCount);
  if AForceUpdate then
    Changed;
end;

function TcxComponentCollection.FindItemByID(ID: Integer): TcxComponentCollectionItem;
var
  I: Integer;
begin
  for I := 0 to FItems.Count-1 do
  begin
    Result := Items[I];
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

function TcxComponentCollection.IndexOf(AItem: TcxComponentCollectionItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

function TcxComponentCollection.Insert(AIndex: Integer): TcxComponentCollectionItem;
begin
  Result := Add;
  Result.Index := AIndex;
end;

procedure TcxComponentCollection.Remove(AItem: TcxComponentCollectionItem);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AItem);
  if AIndex > -1 then
    Delete(AIndex);
end;

procedure TcxComponentCollection.InsertItem(AItem: TcxComponentCollectionItem);
begin
  if not (AItem is FItemClass) then
    Exit;
  FItems.Add(AItem);
  AItem.FCollection := Self;
  AItem.FID := FNextID;
  Inc(FNextID);
  Notify(AItem, ccnAdded);
  Changed(AItem, ccnAdded);
end;

procedure TcxComponentCollection.RemoveItem(AItem: TcxComponentCollectionItem);
begin
  Notify(AItem, ccnExtracting);
  FItems.Remove(AItem);
  AItem.FCollection := nil;
  Notify(AItem, ccnExtracted);
  Changed(AItem, ccnExtracted);
end;

function TcxComponentCollection.AddItem(AItemClass: TcxComponentCollectionItemClass; AParentComponent, AOwner: TComponent): TcxComponentCollectionItem;
begin
  Result := AItemClass.Create(AOwner);
  Result.SetParentComponent(AParentComponent);
end;

function TcxComponentCollection.AddInternalItem: TcxComponentCollectionItem;
begin
  Result := AddItem(FItemClass, ParentComponent, nil);
end;

procedure TcxComponentCollection.Changed(AItem: TcxComponentCollectionItem = nil;
  AAction: TcxComponentCollectionNotification = ccnChanged);
begin
  if FUpdateCount = 0 then
    Update(AItem, AAction);
end;

function TcxComponentCollection.GetItem(AIndex: Integer): TcxComponentCollectionItem;
begin
  Result := TcxComponentCollectionItem(FItems[AIndex]);
end;

function TcxComponentCollection.GetParentControl: TControl;
begin
  Result := nil;
end;

function TcxComponentCollection.GetItemPrefixName: string;
begin
  Result := '';
end;

function TcxComponentCollection.GetItemSuffixName: string;
begin
  Result := '';
end;

function TcxComponentCollection.GetOwner: TPersistent;
begin
  Result := ParentComponent;
end;

procedure TcxComponentCollection.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
end;

procedure TcxComponentCollection.ReadOldCollection(AReader: TReader);
var
  AItem: TcxComponentCollectionItem;
  I: Integer;
begin
  AReader.ReadValue;
  I := 0;
  while not AReader.EndOfList do
  begin
    if AReader.NextValue in [vaInt8, vaInt16, vaInt32] then
      AReader.ReadInteger;
    if I < Count then
      AItem := Items[I]
    else
      AItem := Add;
    Inc(I);
    AReader.ReadListBegin;
    while not AReader.EndOfList do
      TReaderAccess(AReader).ReadProperty(AItem);
    AReader.ReadListEnd;
  end;
  AReader.ReadListEnd;
end;

procedure TcxComponentCollection.SetItem(AIndex: Integer; Value: TcxComponentCollectionItem);
begin
  Items[AIndex].Assign(Value);
end;

procedure TcxComponentCollection.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);

  function GetOwnerForm(AComponent: TComponent): TCustomForm;
  begin
    Result := nil;
    while AComponent.Owner <> nil do
    begin
      AComponent := AComponent.Owner;
      if AComponent is TCustomForm then
        Result := TCustomForm(AComponent);
    end;
  end;

var
  AOwnerParent: TComponent;
begin
  AOwnerParent := nil;
  if GetParentControl <> nil then
    AOwnerParent := GetParentForm(GetParentControl);
  if AOwnerParent = nil then
    AOwnerParent := GetOwnerForm(ParentComponent);
  if AOwnerParent = nil then
    AOwnerParent := Application;
  AItem.Name := CreateUniqueName(AOwnerParent, ParentComponent, AItem, GetItemPrefixName, GetItemSuffixName, ABaseIndex);
end;

procedure TcxComponentCollection.Update(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  if Assigned(OnChange) then
    OnChange(Self, AItem, AAction);
end;

function TcxComponentCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TcxInterfacedComponentCollection }

function TcxInterfacedComponentCollection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TcxInterfacedComponentCollection._AddRef: Integer;
begin
  Result := -1;
end;

function TcxInterfacedComponentCollection._Release: Integer;
begin
  Result := -1;
end;

{ functions }

procedure CallNotify(ANotifyEvent: TNotifyEvent; ASender: TObject);
begin
  dxCallNotify(ANotifyEvent, ASender);
end;

function ClassInheritsFrom(AClass: TClass; const AParentClassName: string): Boolean;
var
  AParentClass: TClass;
begin
  AParentClass := AClass;
  repeat
    Result := AParentClass.ClassName = AParentClassName;
    if Result then Break;
    AParentClass := AParentClass.ClassParent;
  until AParentClass = nil;
end;

procedure FillStringsWithEnumTypeValues(AStrings: TStrings; ATypeInfo: PTypeInfo;
  AGetTypeItemCaption: TcxGetCaptionForIntegerItemFunc);
var
  ATypeData: PTypeData;
  I: Integer;
  S: string;
begin
  ATypeData := GetTypeData(ATypeInfo);
  AStrings.BeginUpdate;
  try
    for I := ATypeData.MinValue to ATypeData.MaxValue do
    begin
      S := AGetTypeItemCaption(I);
      if S <> '' then
        AStrings.AddObject(S, TObject(I));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

function GetPersistentOwner(APersistent: TPersistent): TPersistent;
begin
  Result := TPersistentAccess(APersistent).GetOwner;
end;

function GetSubobjectName(AObject, ASubobject: TPersistent): string;
var
  APropList: PPropList;
  I: Integer;
begin
  Result := '';
  I := GetPropList(AObject.ClassInfo, [tkClass], nil);
  GetMem(APropList, I * SizeOf(PPropInfo));
  GetPropList(AObject.ClassInfo, [tkClass], APropList);
  try
    for I := 0 to I - 1 do
      if APropList[I].PropType^ = ASubobject.ClassInfo then
      begin
        Result := dxShortStringToString(APropList[I].Name);
        Break;
      end;
  finally
    FreeMem(APropList);
  end;
end;

function GetValidName(AComponent: TComponent; const AName: string;
  AIsBaseName: Boolean = False): string;
var
  AOwner: TComponent;
  I: Integer;

  function GetNextName: string;
  begin
    Result := AName + IntToStr(I);
    Inc(I);
  end;

begin
  Result := AName;
  AOwner := AComponent.Owner;
  if AOwner = nil then Exit;
  I := 1;
  if AIsBaseName then Result := GetNextName;
  while AOwner.FindComponent(Result) <> nil do
    Result := GetNextName;
end;

procedure RenameComponents(ACaller, AOwner: TComponent;
  ANewName: TComponentName; const AOldName: TComponentName;
  AComponentCount: Integer; AGetComponent: TcxGetComponent);
var
  I: Integer;
  AComponent: TComponent;
  AComponentName, ANamePrefix: TComponentName;
begin
  // Components introduced in an ancestor will be renamed by IDE.
  // We cannot rename components introduced in a successor because
  // IDE will not refresh source code in a successor.
  if csAncestor in ACaller.ComponentState then Exit;
  for I := 0 to AComponentCount - 1 do
  begin
    AComponent := AGetComponent(ACaller, I);
    if (AComponent.Owner = AOwner) {and not (csAncestor in AComponent.ComponentState)} then
    begin
      AComponentName := AComponent.Name;
      if Length(AComponentName) > Length(AOldName) then
      begin
        ANamePrefix := Copy(AComponentName, 1, Length(AOldName));
        if CompareText(AOldName, ANamePrefix) = 0 then
        begin
          Delete(AComponentName, 1, Length(AOldName));
          Insert(ANewName, AComponentName, 1);
          try
            AComponent.Name := AComponentName;
          except
            on EComponentError do { Ignore rename errors };
          end;
        end;
      end;
    end;
  end;
end;

function RoundDiv(I1, I2: Integer): Integer;
begin
  Result := I1 div I2 + Ord(I1 mod I2 <> 0);
end;

function Size(cx, cy: Longint): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

procedure SwapIntegers(var I1, I2: Integer);
var
  I: Integer;
begin
  I := I1;
  I1 := I2;
  I2 := I;
end;

function GetRangeCenter(ABound1, ABound2: Integer): Integer;
begin
  if ABound1 + ABound2 > 0 then
    Result := (ABound1 + ABound2) div 2
  else
    Result := (ABound1 + ABound2 - 1) div 2;
end;

function StreamsEqual(AStream1, AStream2: TMemoryStream): Boolean;
begin
  Result := (AStream1.Size = AStream2.Size) and
    CompareMem(AStream1.Memory, AStream2.Memory, AStream1.Size);
end;

function GetCorrectPath(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if Result[I] = '/' then
      Result[I] := PathDelim;
end;

function dxLanguages: TdxLanguages;
begin
  if FdxLanguages = nil then
    FdxLanguages := TdxLanguages.Create;
  Result := FdxLanguages;
end;

function cxGetUnitName(AClass: TClass): string;
var
  AClassInfo: Pointer;
begin
  AClassInfo := AClass.ClassInfo;
  if AClassInfo <> nil then // Object must inherits from TPersistent
    Result := dxShortStringToString(GetTypeData(AClassInfo).UnitName)
  else
    Result := '';
end;

function CreateUniqueName(AOwnerForm, AOwnerComponent, AComponent: TComponent;
  const APrefixName, ASuffixName: string; ABaseIndex: Integer = -1): string;

  function GetMainOwner(AComponent: TComponent): TComponent;
  begin
    Result := AComponent;
    while Result.Owner <> nil do
      Result := Result.Owner;
  end;

  function IsUnique(AComponent: TComponent; const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    with AComponent do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] <> AComponent) and
          ((CompareText(Components[I].Name, AName) = 0) or not IsUnique(Components[I], AName))  then
        begin
          Result := False;
          Break;
        end;
  end;

var
  I: Integer;
  AMainOwner: TComponent;
begin
  if AOwnerForm = nil then
    AMainOwner := GetMainOwner(AComponent)
  else
    AMainOwner := AOwnerForm;
  if ABaseIndex < 0 then
    ABaseIndex := Ord(ASuffixName = '');
  for I := ABaseIndex to MaxInt do
  begin
    Result := cxGenerateComponentName(AOwnerComponent,
      AComponent.ClassName, APrefixName, ASuffixName, I);
    if IsUnique(AMainOwner, Result) then
      Break;
  end;
end;

function cxGenerateComponentName(AOwnerComponent: TComponent;
  const AClassName, APrefixName, ASuffixName: string; ANumber: Integer): string;
var
  S: string;

  procedure CheckName(var AName: string);
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(AName) do
      if dxCharInSet(AName[I], ['A'..'Z','a'..'z','_','0'..'9']) then
        Inc(I)
      else
        if dxCharInSet(AName[I], LeadBytes) then
          Delete(AName, I, 2)
        else
          Delete(AName, I, 1);
  end;

begin
  S := ASuffixName;
  CheckName(S);
  if ((S = '') or dxCharInSet(S[1], ['0'..'9'])) and (AClassName <> '') then
  begin
    if (APrefixName <> '') and
      (CompareText(APrefixName, Copy(AClassName, 1, Length(APrefixName))) = 0) then
      S := Copy(AClassName, Length(APrefixName) + 1, Length(AClassName)) + S
    else
    begin
      S := AClassName + S;
      if S[1] = 'T' then Delete(S, 1, 1);
    end;
  end;

  if AOwnerComponent <> nil then
    Result := AOwnerComponent.Name + S
  else
    Result := S;

  if ANumber > 0 then
    Result := Result + IntToStr(ANumber);
end;

function cxSign(const AValue: Double): Integer;
begin
  Result := IfThen(AValue >= 0, 1, -1);
end;

function cxSendStructMessageEx(AHandle: THandle; AMsg: UINT; const AStructure; AParam: LPARAM; AStructureIsLParam: Boolean): LRESULT; overload;
begin
  if AStructureIsLParam then
    Result := SendMessage(AHandle, AMsg, AParam, LPARAM(@AStructure))
  else
    Result := SendMessage(AHandle, AMsg, LPARAM(@AStructure), AParam);
end;

function cxSendStructMessage(AHandle: THandle; AMsg: UINT; WParam: WPARAM; const LParam): LRESULT; overload;
begin
  Result := cxSendStructMessageEx(AHandle, AMsg, LParam, WParam, True);
end;

function cxSendStructMessage(AHandle: THandle; AMsg: UINT; const WParam; LParam: LParam): LRESULT; overload;
begin
  Result := cxSendStructMessageEx(AHandle, AMsg, WParam, LParam, False);
end;

function cxCreateTimer(AOnTimer: TNotifyEvent; AInterval: Integer; AEnabled: Boolean): TcxTimer;
begin
  Result := TcxTimer.Create(nil);
  Result.Enabled := AEnabled;
  Result.Interval := AInterval;
  Result.OnTimer := AOnTimer;
end;

{ TcxDialogMetricsInfo }

constructor TcxDialogMetricsInfo.Create(AForm: TForm);
begin
  Store(AForm);
end;

destructor TcxDialogMetricsInfo.Destroy;
begin
  FreeCustomData;
  inherited Destroy;
end;

procedure TcxDialogMetricsInfo.Restore(AForm: TForm);
var
  IData: IcxDialogMetricsInfoData;
  IClientSize: IcxDialogMetricsClientSize;
  AClientSize: TSize;
  M, D: Integer;
begin
  if FMaximized then
  begin
    ShowWindow(AForm.Handle, WS_MAXIMIZE);
    AForm.WindowState := wsMaximized;
  end
  else
  begin
    AForm.Left := FLeft;
    AForm.Top := FTop;

    if dxGetCurrentScaleFactor(AForm, M, D) then
      AClientSize := cxSizeScale(ClientSize, M, D)
    else
      AClientSize := ClientSize;

    if AForm.BorderStyle in [bsSizeable, bsSizeToolWin] then
    begin
      if Supports(AForm, IcxDialogMetricsClientSize, IClientSize) then
        IClientSize.SetClientSize(AClientSize)
      else
      begin
        AForm.ClientHeight := AClientSize.cy;
        AForm.ClientWidth := AClientSize.cx;
      end;
    end;
  end;
  if Supports(AForm, IcxDialogMetricsInfoData, IData) and (FData <> nil) then
    IData.SetInfoData(FData);
end;

procedure TcxDialogMetricsInfo.Store(AForm: TForm);
var
  IClientSize: IcxDialogMetricsClientSize;
  IData: IcxDialogMetricsInfoData;
  M, D: Integer;
begin
  FDialogClass := AForm.ClassType;
  FLeft := AForm.Left;
  FTop := AForm.Top;
  if Supports(AForm, IcxDialogMetricsClientSize, IClientSize) then
    FClientSize := IClientSize.GetClientSize
  else
    FClientSize := cxSize(AForm.ClientWidth, AForm.ClientHeight);

  if dxGetCurrentScaleFactor(AForm, M, D) then
    FClientSize := cxSizeScale(FClientSize, D, M);

  FMaximized := AForm.WindowState = wsMaximized;
  FreeCustomData;
  if Supports(AForm, IcxDialogMetricsInfoData, IData) and (IData.GetInfoDataSize > 0) then
  begin
    GetMem(FData, IData.GetInfoDataSize);
    Move(IData.GetInfoData^, FData^, IData.GetInfoDataSize);
  end;
end;

procedure TcxDialogMetricsInfo.FreeCustomData;
begin
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;
end;

{ TcxCustomThread }

procedure TcxCustomThread.DoHandleException;
begin
  if FException is Exception then
    Application.ShowException(FException)
  else
    SysUtils.ShowException(FException, nil);
end;

procedure TcxCustomThread.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    ResetException;
  end;
end;

procedure TcxCustomThread.ResetException;
begin
  FException := nil;
end;

{ TcxThread }

constructor TcxThread.Create(ACreateSuspended: Boolean; ACreatePaused: Boolean = False);
begin
  inherited Create(ACreateSuspended);
  InitializeCriticalSection(FRTLCriticalSection);
  FPauseEvent := TcxEvent.Create(True, True);
  FWaitForPauseEvent := TcxEvent.Create(True, True);
  if ACreatePaused then
    Pause;
end;

destructor TcxThread.Destroy;
begin
  FDestroying := True;
  Unpause;
  FreeAndNil(FWaitForPauseEvent);
  FreeAndNil(FPauseEvent);
  DeleteCriticalSection(FRTLCriticalSection);
  inherited Destroy;
end;

procedure TcxThread.CheckForPause;
begin
  FCheckingForPause := True;
  if Assigned(FWaitForPauseEvent) then
    FWaitForPauseEvent.SetEvent;
  if Assigned(FPauseEvent) then
    FPauseEvent.WaitFor(INFINITE);
  FCheckingForPause := False;
end;

function TcxThread.GetRunning: Boolean;
begin
  Result := not (Paused or Destroying or Terminated);
end;

procedure TcxThread.Pause(AWaitForPause: Boolean = False);
begin
  if Running then
  begin
    EnterCriticalSection(FRTLCriticalSection);
    FPaused := True;
    if not FCheckingForPause then
      FWaitForPauseEvent.ResetEvent;
    FPauseEvent.ResetEvent;
    LeaveCriticalSection(FRTLCriticalSection);
    if AWaitForPause then
      FWaitForPauseEvent.WaitFor(INFINITE);
  end;
end;

procedure TcxThread.Unpause;
begin
  if FPaused then
  begin
    EnterCriticalSection(FRTLCriticalSection);
    FPaused := False;
    FPauseEvent.SetEvent;
    LeaveCriticalSection(FRTLCriticalSection);
  end;
end;

{ TcxEvent }

constructor TcxEvent.Create(AManualReset, AInitialState: Boolean);
begin
  inherited Create;
  FHandle := CreateEvent(nil, AManualReset, AInitialState, '');
end;

destructor TcxEvent.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

procedure TcxEvent.ResetEvent;
begin
  Windows.ResetEvent(Handle);
end;

procedure TcxEvent.SetEvent;
begin
  Windows.SetEvent(Handle);
end;

function TcxEvent.WaitFor(TimeOut: Cardinal): TWaitResult;
begin
  case WaitForEvent(TimeOut) of
    WAIT_OBJECT_0:
      Result := wrSignaled;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_ABANDONED:
      Result := wrAbandoned
    else
      Result := wrError;
  end;
end;

function TcxEvent.WaitForEvent(ATimeOut: Cardinal): Cardinal;
var
  AHandles: array[0..1] of THandle;
  AMsg: TMsg;
begin
  if GetCurrentThreadID <> MainThreadID then
    Result := WaitForSingleObject(Handle, ATimeOut)
  else
  begin
    Result := 0;
    AHandles[0] := Handle;
    AHandles[1] := SyncEvent;
    repeat
      if Result = WAIT_OBJECT_0 + 2 then
        PeekMessage(AMsg, 0, 0, 0, PM_NOREMOVE);
      Result := MsgWaitForMultipleObjects(2, AHandles, False, 1000, QS_SENDMESSAGE);
      if Result = WAIT_FAILED then
        Break;
      if Result = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
    until Result = WAIT_OBJECT_0;
  end;
end;

{ TcxComponentList }

constructor TcxComponentList.Create;
begin
  inherited Create(False);
end;

destructor TcxComponentList.Destroy;
begin
  FUpdateCount := 1;
  inherited Destroy;
end;

procedure TcxComponentList.Clear;
begin
  if Count > 0 then
  begin
    BeginUpdate;
    try
      while Count > 0 do
        Delete(0);
    finally
      EndUpdate;
    end;
  end;
  inherited; // to free memory
end;

procedure TcxComponentList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcxComponentList.CancelUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TcxComponentList.EndUpdate;
begin
  Dec(FUpdateCount);
  Update;
end;

procedure TcxComponentList.DoNotify(AItem: TComponent; AAction: TListNotification);

  function ConvertNotificaton(ANotification: TListNotification): TcxComponentCollectionNotification;
  begin
    case ANotification of
      lnAdded: Result := ccnAdded;
      lnExtracted: Result := ccnExtracted;
    else {lnDeleted}
      Result := ccnDeleting;
    end;
  end;

begin
  inherited Notify(AItem, AAction);
  if Assigned(OnNotify) then
    OnNotify(Self, AItem, AAction);
  Update(AItem, ConvertNotificaton(AAction));
end;

function TcxComponentList.GetItemClass: TClass;
begin
  Result := TComponent;
end;

procedure TcxComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if TObject(Ptr) is GetItemClass then
    DoNotify(Ptr, Action)
  else
    if Action = lnAdded then
      Extract(Ptr);
end;

procedure TcxComponentList.Update(AItem: TComponent = nil;
  AAction: TcxComponentCollectionNotification = ccnChanged);
begin
  if (FUpdateCount = 0) and Assigned(OnComponentListChanged) then
    OnComponentListChanged(Self, AItem, AAction);
end;

{ TcxCustomProgressCalculationHelper }

constructor TcxCustomProgressCalculationHelper.Create(AStageCount: Integer);
begin
  inherited Create;
  FStageCount := AStageCount;
  FStageTaskCount := 1;
end;

procedure TcxCustomProgressCalculationHelper.BeforeDestruction;
begin
  SetProgress(100);
  inherited BeforeDestruction;
end;

procedure TcxCustomProgressCalculationHelper.BeginStage(ATaskCount: Integer);
begin
  FStage := Stage + 1;
  if FStage > FStageCount then
    raise EInvalidOperation.Create(ClassName);
  FStageTaskNumber := 1;
  FStageTaskCount := ATaskCount;
  CalculateProgress;
end;

procedure TcxCustomProgressCalculationHelper.EndStage;
begin
  FStageTaskNumber := FStageTaskCount;
  CalculateProgress;
end;

procedure TcxCustomProgressCalculationHelper.NextTask(ASkipCount: Integer = 1);
begin
  SetTaskNumber(FStageTaskNumber + ASkipCount);
end;

procedure TcxCustomProgressCalculationHelper.SetTaskNumber(AValue: Integer);
begin
  FStageTaskNumber := Min(Max(AValue, 0), FStageTaskCount);
  CalculateProgress;
end;

procedure TcxCustomProgressCalculationHelper.SkipStage;
begin
  BeginStage(1);
  EndStage;
end;

procedure TcxCustomProgressCalculationHelper.CalculateProgress;
begin
  if (FStageTaskCount > 0) and (FStageCount > 0) then
    SetProgress(Trunc(100 * ((Stage - 1) + FStageTaskNumber / FStageTaskCount) / FStageCount));
end;

procedure TcxCustomProgressCalculationHelper.SetProgress(AValue: Integer);
begin
  if FProgress <> AValue then
  begin
    FProgress := AValue;
    if not FIsAborting then
    try
      ProgressChanged;
    except
      FIsAborting := True;
      raise;
    end;
  end;
end;

{ TdxMemIniFile }

constructor TdxMemIniFile.Create;
begin
  Create('');
end;

constructor TdxMemIniFile.Create(AStream: TStream);
begin
  Create;
  LoadFromStream(AStream);
end;

procedure TdxMemIniFile.LoadFromStream(AStream: TStream);
var
  AData: TStringList;
begin
  AData := TStringList.Create;
  try
    AData.LoadFromStream(AStream);
    SetStrings(AData);
  finally
    AData.Free;
  end;
end;

procedure TdxMemIniFile.SaveToStream(AStream: TStream);
begin
  SaveToStream(AStream, Encoding);
end;

procedure TdxMemIniFile.SaveToStream(AStream: TStream; AEncoding: TEncoding);
var
  AData: TStringList;
begin
  AData := TStringList.Create;
  try
    GetStrings(AData);
    AData.SaveToStream(AStream, AEncoding);
  finally
    AData.Free;
  end;
end;

{ TcxProgressCalculationHelper }

constructor TcxProgressCalculationHelper.Create(AStageCount: Integer; AEventSender: TObject; AProgressEvent: TcxProgressEvent);
begin
  inherited Create(AStageCount);
  FEventSender := AEventSender;
  FOnProgress := AProgressEvent;
end;

procedure TcxProgressCalculationHelper.ProgressChanged;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FEventSender, Progress);
end;

{ TdxLanguages }

constructor TdxLanguages.Create;
begin
  inherited;
  FTmpLanguages := Self;
  EnumSystemLocales(@InternalEnumLanguages, LCID_SUPPORTED);
end;

function TdxLanguages.GetDefaultLanguageLCID: DWORD;
begin
  Result := GetSystemDefaultLCID;
end;

function TdxLanguages.GetLCID(const AName: string): DWORD;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if AnsiCompareText(AName, Name[I]) = 0 then
    begin
      Result := LocaleID[I];
      Break;
    end;
  end;
end;

function TdxLanguages.IndexOf(ID: LCID): Integer;
begin
  for Result := Low(FSysLangs) to High(FSysLangs) do
    if FSysLangs[Result].FLCID = ID then Exit;
  Result := -1;
end;

function TdxLanguages.IndexOf(const ALocaleName: string): Integer;
begin
  for Result := Low(FSysLangs) to High(FSysLangs) do
    if CompareText(FSysLangs[Result].FLocaleName, ALocaleName) = 0 then
      Exit;
  Result := -1;
end;

function TdxLanguages.LocalesCallback(LocaleID: PChar): Integer; stdcall;
var
  AID: LCID;
  AShortLangName: string;
begin
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  AShortLangName := dxGetLocaleInfo(AID, LOCALE_SABBREVLANGNAME, '');
  if AShortLangName <> '' then
  begin
    SetLength(FSysLangs, Length(FSysLangs) + 1);
    with FSysLangs[High(FSysLangs)] do
    begin
      FName := dxGetLocaleInfo(AID, LOCALE_SLANGUAGE, '');
      FLocaleName := Format('%s-%s', [dxGetLocaleInfo(AID, LOCALE_SISO639LANGNAME),
        dxGetLocaleInfo(AID, LOCALE_SISO3166CTRYNAME)]);
      FLCID := AID;
      FExt := AShortLangName;
    end;
  end;
  Result := 1;
end;

function TdxLanguages.GetCount: integer;
begin
  Result := High(FSysLangs) + 1;
end;

function TdxLanguages.GetExt(Index: Integer): string;
begin
  Result := FSysLangs[Index].FExt;
end;

function TdxLanguages.GetID(Index: Integer): string;
begin
  Result := HexDisplayPrefix + IntToHex(FSysLangs[Index].FLCID, 8);
end;

function TdxLanguages.GetLocaleID(Index: Integer): LCID;
begin
  Result := FSysLangs[Index].FLCID;
end;

function TdxLanguages.GetLocaleName(Index: Integer): string;
begin
  Result := FSysLangs[Index].FLocaleName;
end;

function TdxLanguages.GetName(Index: Integer): string;
begin
  Result := FSysLangs[Index].FName;
end;

function TdxLanguages.GetNameFromLocaleID(ID: LCID): string;
const
  SUnknown = '<unknown>';
var
  AIndex: Integer;
begin
  Result := SUnknown;
  AIndex := IndexOf(ID);
  if AIndex <> - 1 then Result := Name[AIndex];
  if Result = '' then Result := SUnknown;
end;

function TdxLanguages.GetNameFromLCID(const ID: string): string;
begin
  Result := NameFromLocaleID[StrToIntDef(ID, 0)];
end;

{ TcxDialogsMetricsStore }

constructor TcxDialogsMetricsStore.Create;
begin
  inherited;
  FMetrics := TcxObjectList.Create;
  FDefaultPosition := poMainFormCenter;
end;

destructor TcxDialogsMetricsStore.Destroy;
begin
  FMetrics.Free;
  inherited Destroy;
end;

procedure TcxDialogsMetricsStore.InitDialog(AForm: TForm);
begin
  if FindMetrics(AForm) >= 0 then
  begin
    AForm.Position := poDesigned;
    TcxDialogMetricsInfo(FMetrics[FindMetrics(AForm)]).Restore(AForm)
  end
  else
  begin
    AForm.Position := DefaultPosition;
    FMetrics.Add(CreateMetrics(AForm));
  end;
end;

procedure TcxDialogsMetricsStore.StoreMetrics(AForm: TForm);
begin
  if FindMetrics(AForm) >= 0 then
    TcxDialogMetricsInfo(FMetrics[FindMetrics(AForm)]).Store(AForm)
end;

function TcxDialogsMetricsStore.CreateMetrics(AForm: TForm): TcxDialogMetricsInfo;
begin
  Result := TcxDialogMetricsInfo.Create(AForm);
end;

function TcxDialogsMetricsStore.FindMetrics(AForm: TForm): Integer;
begin
  Result := FMetrics.Count - 1;
  while Result >= 0 do
  begin
    if TcxDialogMetricsInfo(FMetrics[Result]).DialogClass = AForm.ClassType then
      Break;
    Dec(Result);
  end;
end;

{ TcxHashedStringList }

constructor TcxHashedStringList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TcxHashedStringList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TcxHashedStringList.Add(const S: string): Integer;
begin
  Result := FList.Add(S)
end;

procedure TcxHashedStringList.Clear;
begin
  FList.Clear;
end;

function TcxHashedStringList.IndexOf(const S: string): Integer;
var
  I, AStrHash: Integer;
begin
  Result := -1;
  AStrHash := dxElfHash(S);
  for I := 0 to Count - 1 do
    if Hash[I] = AStrHash then
    begin
      if (Length(S) = Length(Strings[I])) and SameText(S, Strings[I]) then
      begin
        Result := I;
        Break;
      end;
    end;
end;

function TcxHashedStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxHashedStringList.GetHash(Index: Integer): Integer;
begin
  Result := Integer(FList.Objects[Index]);
  if Result = 0 then
  begin
    Result := dxElfHash(Strings[Index]);
    FList.Objects[Index] := TObject(Result);
  end;
end;

function TcxHashedStringList.GetString(Index: Integer): string;
begin
  Result := FList.Strings[Index];
end;

procedure TcxHashedStringList.SetString(Index: Integer; const AValue: string);
begin
  FList.Strings[Index] := AValue;
  FList.Objects[Index] := nil;
end;

{ TcxComponentHolder }

constructor TcxComponentHolder.Create;
begin
  inherited Create(nil);
end;

procedure TcxComponentHolder.DoAfterComponentChange;
begin
  if Assigned(OnAfterComponentChange) then
    OnAfterComponentChange(Self);
end;

procedure TcxComponentHolder.DoBeforeComponentChange;
begin
  if Assigned(OnBeforeComponentChange) then
    OnBeforeComponentChange(Self);
end;

procedure TcxComponentHolder.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Component) then
    Component := nil;
end;

procedure TcxComponentHolder.SetComponent(AValue: TComponent);
begin
  if AValue <> FComponent then
  begin
    DoBeforeComponentChange;
    if FComponent <> nil then
      FComponent.RemoveFreeNotification(Self);
    FComponent := AValue;
    if FComponent <> nil then
      FComponent.FreeNotification(Self);
    DoAfterComponentChange;
  end;
end;

{ TdxLanguageID }

class function TdxLanguageID.GetLanguageID(const ALanguage: PWideChar): Integer;
var
  I: Integer;
begin
  Result := TdxLanguageID.None;
  for I := 0 to High(TdxLanguageID.LangMap) do
    if StrLComp(ALanguage, TdxLanguageID.LangMap[I].ShortName, 2) = 0 then
    begin
      Result := TdxLanguageID.LangMap[I].LanguageID;
      Break;
    end;
end;

initialization
  FUnitIsFinalized := False;
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxComponent, TControl);
  GroupDescendentsWith(TcxCustomComponent, TControl);
  FObjectLinkController := TcxObjectLinkController.Create;

finalization
  FUnitIsFinalized := True;
  FreeAndNil(FdxLanguages);
  FreeAndNil(FDialogsMetrics);
  FreeAndNil(FActiveTimerList);
  FreeAndNil(FcxTimerWindow);
  FreeAndNil(FObjectLinkController);

end.
