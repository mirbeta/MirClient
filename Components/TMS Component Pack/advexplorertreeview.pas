{*************************************************************************}
{ TAdvExplorerTreeview component                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2008 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvExplorerTreeview;

{$R ADVEXPLORERTREEVIEW.RES}

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Math,
  Dialogs, StdCtrls, Buttons, CommCtrl, ImgList, RTLConsts, ComStrs, ExtCtrls,
  Menus, ShellAPI, AdvStyleIF, Types
  {$IFDEF TMS_GDIP}
  , AdvGDIP
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0   : First release
  // v1.0.0.1   : Fixed : issue with font in dropdownlist
  // v1.0.1.0   : Improved : synchronising with file system changes
  //            : Improved : default settings AutoComplete, AutoDropDownFill to true
  //            : Improved : handling control resizing
  //            : Fixed : issue with Clear method
  // v1.0.1.1   : Fixed : issue with refreshing subfolders
  // v1.0.2.0   : New : support to show hidden folders
  //            : Fixed : issue with readonly support
  // v1.1.0.0   : New : Terminal, Vista, Windows 7 styles added
  // v1.1.0.1   : Fixed : issue with OnSelect event triggering
  // v1.2.0.0   : New : Built in support for Office 2010 colors
  // v1.3.0.0   : New : EditStyle property added to allow entering non list item values
  // v1.3.1.0   : New : DropDownButton property added
  // v1.3.2.0   : New : Event OnNodePopulate added
  // v1.3.2.1   : Fixed : Issue with SetSelectedFolder in aeFolder mode
  // v1.3.3.0   : New : Windows 8, Office 2013 styles added
  // v1.4.0.0   : New : Windows 10, Office 2016 styles added

  NODE_SEP = '\';
  DROPDOWNBTN_WIDTH = 15;
  REFRESHBTN_WIDTH = 24;
  LEFTICON_WIDTH = 22;
  DwBUTTON_WIDTH = 14;

type
  TCustomExplorerTreeview = class;
  ETreeViewError = class(Exception);

  TAdvTreeNode = class;
  TAdvTreeNodes = class;

  {TDropForm}
  TDropForm = class(TForm)
  private
    FSizeable: Boolean;
    FDroppedDown: Boolean;
    FExplorerTreeview: TCustomExplorerTreeview;
    procedure WMClose(var Msg:TMessage); message WM_CLOSE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    function GetParentWnd: HWnd;
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property ExplorerTreeview: TCustomExplorerTreeview read FExplorerTreeview write FExplorerTreeview;
    property Sizeable: Boolean read FSizeable write FSizeable;
  end;


  TExplorerTreeviewListBox = class(TCustomListBox)
  private
    FExplorerTreeview: TCustomExplorerTreeview;
    FOnSelect: TNotifyEvent;
    FShowImages: Boolean;
    FMouseInControl: Boolean;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure SetShowImages(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MoveSelect(Offset: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;

    property ExplorerTreeview: TCustomExplorerTreeview read FExplorerTreeview write FExplorerTreeview;
    property ShowImages: Boolean read FShowImages write SetShowImages default true;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TDropDownButton = class(TGraphicControl)
  private
    FHot, FDown: Boolean;
    FExplorerTreeview: TCustomExplorerTreeview;
    FGlyph: TBitmap;
    FImageIndex: Integer;
    procedure WMLButtonDown(var Msg:TMessage); message WM_LBUTTONDOWN;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure OnGlyphChanged(Sender: TObject);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetDown(const Value: Boolean);
    procedure setImageIndex(const Value: Integer);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawButton;
    function IsActive: Boolean;

    property Down: Boolean read FDown write SetDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;

    property ExplorerTreeview: TCustomExplorerTreeview read FExplorerTreeview;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property ImageIndex: Integer read FImageIndex write setImageIndex default -1;
  published
  end;

  TLeftIcon = class(TGraphicControl)
  private
    FExplorerTreeview: TCustomExplorerTreeview;
    FGlyph: TBitmap;
    procedure OnGlyphChanged(Sender: TObject);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure SetGlyph(const Value: TBitmap);
  protected
    procedure Paint; override;
    procedure DrawIcon;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ExplorerTreeview: TCustomExplorerTreeview read FExplorerTreeview;
    property Glyph: TBitmap read FGlyph write SetGlyph;
  published
  end;

  TNodeButton = class(TGraphicControl)
  private
    FHot, FDown, FDwBtnHot: Boolean;
    FExplorerTreeview: TCustomExplorerTreeview;
    FGlyph: TBitmap;
    FNode: TAdvTreeNode;
    FScrollButton: Boolean;
    FOffsetX: Integer;
    FShowText: Boolean;
    FInternalClick: Boolean;
    procedure OnMenuItemClick(Sender: TObject);
    procedure WMLButtonDown(var Msg:TMessage); message WM_LBUTTONDOWN;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetNode(const Value: TAdvTreeNode);
    procedure SetDown(const Value: Boolean);
    procedure SetScrollButton(const Value: Boolean);
    procedure SetShowText(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawButton;
    procedure UpdateSize;
    procedure ButtonClick;
    procedure DropDownPress;
    function IsSplitButton: Boolean;
    function IsActive: Boolean;
    function GetIndex: Integer;
    procedure DoNodePopulate(Node: TAdvTreeNode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;

    property ExplorerTreeview: TCustomExplorerTreeview read FExplorerTreeview;
    property Glyph: TBitmap read FGlyph write SetGlyph;

    property Node: TAdvTreeNode read FNode write SetNode;
    property Down: Boolean read FDown write SetDown default false;

    property ScrollButton: Boolean read FScrollButton write SetScrollButton default false; // <<
    property ShowText: Boolean read FShowText write SetShowText default true;
  published
  end;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): TAdvTreeNode;
    procedure SetItemsEx(Index: Integer; const Value: TAdvTreeNode);
  public
    procedure AssignList(ListA: TList);
    property Items[Index: Integer]: TAdvTreeNode read GetItemsEx write SetItemsEx; default;
  end;

{ TAdvTreeNode }

  TAdvExplorerTreeviewMode = (aeCustom, aeSystem, aeFolder);
  TBaseNode = (bnDesktop, bnFolder);

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    Text: string[255];
  end;

  TNodeDataInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    TextLen: Byte;
    // WideString Text of TextLen chars follows
  end;

  TNodePopulateEvent = procedure(Sender: TObject; ANode: TAdvTreeNode) of object;

  TAdvTreeNodeClass = class of TAdvTreeNode;
  
  TAdvTreeNode = class(TPersistent)
  private
    FOwner: TAdvTreeNodes;
    FText: string;
    FData: TObject;
    FImageIndex: TImageIndex;
    FDeleting: Boolean;
    FFirstChild: TAdvTreeNode;
    FNextSibling: TAdvTreeNode;
    FPrevSibling: TAdvTreeNode;
    FParentNode: TAdvTreeNode;
    FNodeButton: TNodeButton;
    FShowText: Boolean;
    FVirtualParent: Boolean;
    function GetLevel: Integer;
    function GetParent: TAdvTreeNode;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TAdvTreeNode;
    function GetCount: Integer;
    function GetTreeView: TCustomExplorerTreeview;
    function IsEqual(Node: TAdvTreeNode): Boolean;
    procedure SetData(Value: TObject);
    procedure SetItem(Index: Integer; Value: TAdvTreeNode);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetText(const S: string);
    function GetHasChildren: Boolean;
    procedure SetNodeButton(const Value: TNodeButton);
    procedure SetShowText(const Value: Boolean);
    procedure SetVirtualParent(const Value: Boolean);
    procedure ReadData(Stream: TStream; Info: PNodeInfo);
    procedure WriteData(Stream: TStream; Info: PNodeInfo);
  protected
    function CompareCount(CompareMe: Integer): Boolean;
    function IndexOf(Value: TAdvTreeNode): Integer;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property Item[Index: Integer]: TAdvTreeNode read GetItem write SetItem; default;

    property Button: TNodeButton read FNodeButton write SetNodeButton;
    property TreeView: TCustomExplorerTreeview read GetTreeView;
    property Owner: TAdvTreeNodes read FOwner;
  public
    constructor Create(AOwner: TAdvTreeNodes);
    destructor Destroy; override;

    function AddFirstChild(const S: string): TAdvTreeNode; overload;
    function AddFirstChild(Data: TObject; const S: string): TAdvTreeNode; overload;
    function AddChild(const S: string): TAdvTreeNode; overload;
    function AddChild(Data: TObject; const S: string): TAdvTreeNode; overload;
    procedure Assign(Source: TPersistent); override;
    
    procedure Delete;
    procedure DeleteChildren;

    function getFirstChild: TAdvTreeNode;
    function GetLastChild: TAdvTreeNode;
    function GetNextChild(Value: TAdvTreeNode): TAdvTreeNode;
    function getNextSibling: TAdvTreeNode;
    function GetPrevChild(Value: TAdvTreeNode): TAdvTreeNode;
    function getPrevSibling: TAdvTreeNode;
    function HasAsParent(Value: TAdvTreeNode): Boolean;
    function IsFirstNode: Boolean;
    function IsFirstChild: Boolean;

    property Deleting: Boolean read FDeleting;
    property HasChildren: Boolean read GetHasChildren;
    property Level: Integer read GetLevel;
    property Parent: TAdvTreeNode read GetParent;
    property Text: string read FText write SetText;
    property Data: TObject read FData write SetData;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ShowText: Boolean read FShowText write SetShowText default true;
    property VirtualParent: Boolean read FVirtualParent write SetVirtualParent default false;
  end;

{ TAdvTreeNodes }

  TAdvTreeNodes = class(TPersistent)
  private
    FOwner: TCustomExplorerTreeview;
    FFirstNode: TAdvTreeNode;
    FNodeList: TDbgList;
    FDeleting: Boolean;
    FReading: Boolean;
    function GetNodeFromIndex(Index: Integer): TAdvTreeNode;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetItem(Index: Integer; Value: TAdvTreeNode);
    function GetCount: Integer;
    function InsertNode(ParentNode: TAdvTreeNode; DesNode: TAdvTreeNode; InsertBefore: Boolean): TAdvTreeNode;
    function IsVeryFirstNode(Node: TAdvTreeNode): Boolean;
    property Reading: Boolean read FReading;
  public
    constructor Create(AOwner: TCustomExplorerTreeview);
    destructor Destroy; override;
    function AddFirst(const S: string): TAdvTreeNode; overload;
    function AddFirst(Data: TObject; const S: string): TAdvTreeNode; overload;
    function Add(const S: string): TAdvTreeNode; overload;
    function Add(Data: TObject; const S: string): TAdvTreeNode; overload;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Node: TAdvTreeNode);
    function GetFirstNode: TAdvTreeNode;
    function GetLastNode: TAdvTreeNode;
    function Insert(Sibling: TAdvTreeNode; const S: string): TAdvTreeNode;
    function InsertObject(Sibling: TAdvTreeNode; const S: string; Data: TObject): TAdvTreeNode;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TAdvTreeNode read GetNodeFromIndex; default;
    property Owner: TCustomExplorerTreeview read FOwner;
  end;

  TButtonAppearance = class(TPersistent)
  private
    FBorderColorHot: TColor;
    FColorHot: TColor;
    FArrowColorHot: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorHotTo: TColor;
    FColorHotTo: TColor;

    FArrowColorDown: TColor;
    FColorDownTo: TColor;
    FColorDown: TColor;
    FOnChange: TNotifyEvent;
    FColorMirrorDownTo: TColor;
    FColorMirrorDown: TColor;
    FBorderColorDown: TColor;
    FFont: TFont;
    FColorMirrorNodeHotTo: TColor;
    FBorderColorNodeHot: TColor;
    FColorNodeHot: TColor;
    FColorMirrorNodeHot: TColor;
    FColorNodeHotTo: TColor;
    procedure OnFontChanged(Sender: TObject);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorMirrorDown(const Value: TColor);
    procedure SetColorMirrorDownTo(const Value: TColor);
    procedure SetArrowColorDown(const Value: TColor);
    procedure SetArrowColorHot(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetBorderColorHot(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorMirrorHot(const Value: TColor);
    procedure SetColorMirrorHotTo(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBorderColorNodeHot(const Value: TColor);
    procedure SetColorMirrorNodeHot(const Value: TColor);
    procedure SetColorMirrorNodeHotTo(const Value: TColor);
    procedure SetColorNodeHot(const Value: TColor);
    procedure SetColorNodeHotTo(const Value: TColor);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArrowColorDown: TColor read FArrowColorDown write SetArrowColorDown default clBlack;
    property ArrowColorHot: TColor read FArrowColorHot write SetArrowColorHot default clBlack;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown default $008B622C;
    property BorderColorHot: TColor read FBorderColorHot write SetBorderColorHot default $008B5816;
    property BorderColorNodeHot: TColor read FBorderColorNodeHot write SetBorderColorNodeHot default $008F8F8E;
    property ColorDown: TColor read FColorDown write SetColorDown default $00FCF1E4;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo default $00F7E7C9;
    property ColorMirrorDown: TColor read FColorMirrorDown write SetColorMirrorDown default $00EDCE93;
    property ColorMirrorDownTo: TColor read FColorMirrorDownTo write SetColorMirrorDownTo default $00DDB66D;
    property ColorHot: TColor read FColorHot write SetColorHot default $00FBEDD3;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo default $00FAE9C7;
    property ColorMirrorHot: TColor read FColorMirrorHot write SetColorMirrorHot default $00F7D89C;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write SetColorMirrorHotTo default $00F5D089;
    property ColorNodeHot: TColor read FColorNodeHot write SetColorNodeHot default $00F2F2F2;
    property ColorNodeHotTo: TColor read FColorNodeHotTo write SetColorNodeHotTo default $00EEEEEE;
    property ColorMirrorNodeHot: TColor read FColorMirrorNodeHot write SetColorMirrorNodeHot default $00D9D9D9;
    property ColorMirrorNodeHotTo: TColor read FColorMirrorNodeHotTo write SetColorMirrorNodeHotTo default $00D2D2D2;
    property Font: TFont read FFont write SetFont;
  end;

  TExpTreeviewAppearance = class(TPersistent)
  private
    FColor: TColor;
    FFocusColor: TColor;
    FOnChange: TNotifyEvent;
    FFocusOuterBorderColor: TColor;
    FInnerBorderColor: TColor;
    FOuterBorderColor: TColor;
    FFocusInnerBorderColor: TColor;
    FButtonAppearance: TButtonAppearance;
    FHotColor: TColor;
    FInnerMostBorderColor: TColor;
    procedure OnButtonAppearanceChanged(Sender: TObject);
    procedure SetColor(const Value: TColor);
    procedure SetFocusColor(const Value: TColor);
    procedure Changed;
    procedure SetFocusInnerBorderColor(const Value: TColor);
    procedure SetFocusOuterBorderColor(const Value: TColor);
    procedure SetInnerBorderColor(const Value: TColor);
    procedure SetOuterBorderColor(const Value: TColor);
    procedure SetButtonAppearance(const Value: TButtonAppearance);
    procedure SetHotColor(const Value: TColor);
    procedure SetInnerMostBorderColor(const Value: TColor);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ButtonAppearance: TButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property OuterBorderColor: TColor read FOuterBorderColor write SetOuterBorderColor default clNone;
    property InnerBorderColor: TColor read FInnerBorderColor write SetInnerBorderColor default $B99D7F;
    property InnerMostBorderColor: TColor read FInnerMostBorderColor write SetInnerMostBorderColor default clNone;
    property FocusOuterBorderColor: TColor read FFocusOuterBorderColor write SetFocusOuterBorderColor default clNone;
    property FocusInnerBorderColor: TColor read FFocusInnerBorderColor write SetFocusInnerBorderColor default $B99D7F;
    property Color: TColor read FColor write SetColor default $00FAF0E6;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clWhite;
    property HotColor: TColor read FHotColor write SetHotColor default $00FFF9F4;
  end;

  { TCustomExplorerTreeview}
  TEditStyle = (esList, esEdit);
  TNodeEvent = procedure(Sender: TObject; Node: TAdvTreeNode) of object;
  TPopulateChildEvent = procedure (Sender: TObject; ParentNode: TAdvTreeNode; Path: string; var PopulateAllowed: Boolean) of object;

  TCustomExplorerTreeview = class(TCustomEdit, ITMSStyle)
  private
    FTMSStyle: TTMSStyle;
    FOnAddition: TNodeEvent;
    FOnDeletion: TNodeEvent;
    FTreeNodes: TAdvTreeNodes;
    FImages: TImageList;
    FSelectedNode: TAdvTreeNode;
    FOldSelected: TAdvTreeNode;
    FRefreshButton: Boolean;
    FAppearance: TExpTreeviewAppearance;
    FDropDownButton: TDropDownButton;
    FRefreshBtn: TDropDownButton;
    FMouseInControl: Boolean;
    FShowImage: Boolean;
    FLeftIcon: TLeftIcon;
    FDropForm: TDropForm;
    FListBox: TExplorerTreeviewListBox;
    FMaxDropHeight: Integer;
    FAutoComForm: TDropForm;
    FAutoComListBox: TExplorerTreeviewListBox;
    FNodeButtons: TList;
    FRefreshGlyph: TBitmap;
    FDropDownList: TDbgList;
    FText: string;
    FTimer: TTimer;
    FOnRefreshClick: TNotifyEvent;
    FCloseClick: Boolean;
    FAutoComplete: Boolean;
    FAutoCompleteList: TStringList;  // string = heirarchical Node text; object = Node
    FEditorEnabled: Boolean;
    FOnBeforeDropDown: TNotifyEvent;
    FMode: TAdvExplorerTreeviewMode;
    FOnShowEdit: TNotifyEvent;
    FOnHideEdit: TNotifyEvent;
    FRefreshImageIndex: Integer;
    FInternalImages: TImageList;
    FFolderPath: string;
    FOnPopulateChildNode: TPopulateChildEvent;
    FOnSelect: TNodeEvent;
    FAutoDropDownFill: Boolean;
    FMyComputerNode: TAdvTreeNode;
    FUpdateCount: Integer;
    FNewSelectedFolderPath: string;
    FShowHiddenFolders: Boolean;
    FEditMode: Boolean;
    FIsInternal: Boolean;
    FBufferedDraw: Boolean;
    FEditStyle: TEditStyle;
    FUpdateInternalText: Boolean;
    FShowAutoCL: Boolean;
    FAllowHook: Boolean;
    FCaptureChangeCancels: Boolean;
    FOnNodePopulate: TNodePopulateEvent;
    procedure OnLeftIconDblClick(Sender: TObject);
    procedure OnLeftIconClick(Sender: TObject);
    procedure OnRefreshBtnClick(Sender: TObject);
    procedure OnDropDownBtnClick(Sender: TObject);
    procedure OnDropDownBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnAppearanceChanged(Sender: TObject);
    procedure OnDropFormDeactivate(Sender: TObject);
    procedure OnListBoxClick(Sender: TObject);
    procedure OnListBoxSelect(Sender: TObject);
    procedure OnListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnListBoxKeyKeyPress(Sender: TObject; var Key: Char);
    procedure OnRefreshGlyphChanged(Sender: TObject);
    procedure OnTimerTime(Sender: TObject);
    procedure OnAutoCompFormDeactivate(Sender: TObject);
    procedure OnAutoCompListBoxClick(Sender: TObject);
    procedure OnAutoCompListBoxSelect(Sender: TObject);
    procedure OnAutoCompListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure OnAutoCompListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMSysKeyDown(var Msg:TWMKeydown); message WM_SYSKEYDOWN;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SeTAdvTreeNodes(const Value: TAdvTreeNodes);
    function GetSelectedNode: TAdvTreeNode;
    procedure SetSelectedNode(const Value: TAdvTreeNode);
    procedure SetRefreshButton(const Value: Boolean);
    procedure SetAppearance(const Value: TExpTreeviewAppearance);
    procedure SetShowImage(const Value: Boolean);
    function GetDropDownCount: integer;
    function GetDropDownList(index: integer): TAdvTreeNode;
    procedure SetMaxDropHeight(const Value: Integer);
    procedure SetRefreshGlyph(const Value: TBitmap);
    function GetTextEx: string;
    procedure SetTextEx(const Value: string);
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetMode(const Value: TAdvExplorerTreeviewMode);
    procedure SetRefreshImageIndex(const Value: Integer);
    function GetCurrentImages: TImageList;
    procedure SetFolderPath(const Value: string);
    function GetIconSize: TSize;
    procedure UpdateImage;
    procedure SetAutoDropDownFill(const Value: Boolean);
    procedure SetEditorEnabled(const Value: Boolean);
    procedure SetEditMode(const Value: Boolean);
    procedure SetDirectText(Value: string);
    function GetDropDownButton: Boolean;
    procedure SetDropDownButton(const Value: Boolean);
  protected
    function GetVersionNr: Integer; virtual;
    procedure SetEditRect; virtual;
    function CreateNode: TAdvTreeNode; virtual;
    function CreateNodes: TAdvTreeNodes; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WndProc(var Message: TMessage); override;
    function GetParentForm(Control: TControl): TCustomForm; virtual;
    procedure Change; override;
    procedure DoEnter; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure InvalidateDropDownButton;

    procedure DoDelete(Node: TAdvTreeNode); dynamic;
    procedure DoAdded(Node: TAdvTreeNode); dynamic;
    procedure UpdateNodeInAutoComList(Node: TAdvTreeNode);
    procedure AddInAutoComList(Node: TAdvTreeNode);
    procedure DeleteFromAutoComList(Node: TAdvTreeNode);

    procedure ClickButton(Button: TNodeButton; RealClick: Boolean = false);
    procedure CancelMenu;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;

    function IsUpdating: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetMinHeight: Integer;
    procedure AddWithChildrenToAutoCompleteList(Node: TAdvTreeNode);
    procedure PopulateAutoCompListBox;
    procedure ShowAutoCompList;
    procedure HideAutoCompList;
    function IsDroppedDown: Boolean;
    function IsMyNode(Node: TAdvTreeNode): Boolean;
    procedure UpdateNodeButtonsPositions;
    procedure RemoveNodeButtons;
    procedure GenerateNodeButtons;
    function IsFocused: Boolean;
    procedure PopulateListBox;
    procedure ShowDropDownList;
    procedure HideDropDownList;
    procedure UpdateButtonsPosition;
    procedure UpdateDropDownRefreshBtnsPos;
    procedure UpdateLeftIconPos;
    function GetBorderWidth: Integer;
    function GetNodeButtonsRect: TRect;
    function GetRefreshButtonRect: TRect;
    function GetDropDownButtonRect: TRect;
    function GetLeftIconRect: TRect;
    function GetHierarchicalNodeText(N: TAdvTreeNode; IncludeFolderPath: Boolean = True): string;   // text from first parent to this node N
    procedure DrawBackGround;
    property IsInternal: Boolean read FIsInternal write FIsInternal;
    property CurrentImages: TImageList read GetCurrentImages;
    property Appearance: TExpTreeviewAppearance read FAppearance write SetAppearance;
    property Images: TImageList read FImages write FImages;
    property Version: string read GetVersion write SetVersion;
    property Items: TAdvTreeNodes read FTreeNodes write SeTAdvTreeNodes;
    property RefreshButton: Boolean read FRefreshButton write SetRefreshButton default true;
    property DropDownButton: Boolean read GetDropDownButton write SetDropDownButton default true;
    property ShowImage: Boolean read FShowImage write SetShowImage default True;
    property MaxDropHeight: Integer read FMaxDropHeight write SetMaxDropHeight default 150;
    property SelectedNode: TAdvTreeNode read GetSelectedNode write SetSelectedNode;
    property RefreshGlyph: TBitmap read FRefreshGlyph write SetRefreshGlyph;
    property Text: string read GetTextEx write SetTextEx;
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default True;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default False;
    property Mode: TAdvExplorerTreeviewMode read FMode write SetMode default aeSystem;
    property FolderPath: string read FFolderPath write SetFolderPath;
    property RefreshImageIndex: Integer read FRefreshImageIndex write SetRefreshImageIndex default -1;
    property AutoDropDownFill: Boolean read FAutoDropDownFill write SetAutoDropDownFill default True;
    property ShowHiddenFolders: Boolean read FShowHiddenFolders write FShowHiddenFolders default False;
    property EditMode: Boolean read FEditMode write SetEditMode;
    property EditStyle: TEditStyle read FEditStyle write FEditStyle default esList;

    property AllowHook: Boolean read FAllowHook write FAllowHook default true;

    property OnRefreshClick: TNotifyEvent read FOnRefreshClick write FOnRefreshClick;
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnShowEdit: TNotifyEvent read FOnShowEdit write FOnShowEdit;
    property OnHideEdit: TNotifyEvent read FOnHideEdit write FOnHideEdit;
    property OnPopulateChildNode: TPopulateChildEvent read FOnPopulateChildNode write FOnPopulateChildNode;
    property OnSelect: TNodeEvent read FOnSelect write FOnSelect;
    property OnAddition: TNodeEvent read FOnAddition write FOnAddition;
    property OnDeletion: TNodeEvent read FOnDeletion write FOnDeletion;
    property OnNodePopulate: TNodePopulateEvent read FOnNodePopulate write FOnNodePopulate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure LoadDirectoryStructure(BaseNode: TBaseNode; path: string; ClearOldNodes: Boolean);
    procedure AddSubFolders(path: string; ParentNode: TAdvTreeNode);  // Add Sub folders not all but immediate subfolders
    function GetNodePath(Node: TAdvTreeNode): string;
    procedure RefreshSubFolders(ParentNode: TAdvTreeNode);  // Read immediate subfolders
    procedure Clear; override;

    function GetSelectedFolder: string;
    procedure SetSelectedFolder(Path: string; Immediately: Boolean = True);

    procedure PopulateAutoCompleteList;
    procedure AddToDropDownList(Node: TAdvTreeNode);
    procedure RemoveFromDropDownList(Node: TAdvTreeNode); overload;
    procedure RemoveFromDropDownList(Index: Integer); overload;
    procedure ClearDropDownList;
    property DropDownListCount: integer read GetDropDownCount;
    property DropDownList[index: integer]: TAdvTreeNode read GetDropDownList;

    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;

    property BufferedDraw: Boolean read FBufferedDraw write FBufferedDraw;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvExplorerTreeview = class(TCustomExplorerTreeview)
  public
    property SelectedNode;
    property Text;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Appearance;
    property AutoComplete;
    property AutoDropDownFill;
    property DropDownButton;
    property EditorEnabled;
    property EditStyle;
    property Font;
    property Height;
    property Images;
    property Items;
    property MaxDropHeight;
    property MaxLength;
    property Mode;
    property FolderPath;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RefreshButton;
    property RefreshGlyph;
    property RefreshImageIndex;
    property ShowImage;
    property ShowHiddenFolders;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Version;
    property Width;
    property OnRefreshClick;
    property OnBeforeDropDown;
    property OnShowEdit;
    property OnHideEdit;
    property OnPopulateChildNode;
    property OnSelect;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNodePopulate;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

var
  WM_ET_HIDEDROPDOWN: Word;
  WM_ET_SETFOLDERPATH: Word;
  WM_ET_TRACKKEYPRESS: Word;

implementation

uses
  ShlObj, ComObj
  {$IFDEF DELPHI2007_LVL}
  ,uxTheme
  {$ENDIF}
  ;

function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
  
//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if (ToColor = clNone) then
  begin
    Canvas.Brush.Color := FromColor;
    Canvas.Pen.Color := FromColor;
    Canvas.FillRect(R);
    Exit;
  end;

  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure DrawVistaGradient(Canvas: TCanvas; R: TRect; GradHeight: Integer; FC, TC, MFC, MTC, PC: TColor; Down: Boolean; BothSideBorder: Boolean = false);
var
  R1, R2: TRect;
begin
  R1 := Rect(R.Left, R.Top, R.Right, R.Top + GradHeight + 1);
  R2 := Rect(R.Left, R.Top + GradHeight, R.Right, R.Bottom);
  if (MFC <> clNone) and (MTC <> clNone) then
  begin
    DrawGradient(Canvas, FC, TC, 40, R1, False);
    DrawGradient(Canvas, MFC, MTC, 40, R2, False);
  end
  else
    DrawGradient(Canvas, FC, TC, 40, R, False);

  if (PC <> clNone) then
  begin
    Canvas.Pen.Color := PC;
    if Down then
    begin
      Canvas.Brush.Style := bsClear;
      R2 := R;
      InflateRect(R2, -1, -1);
      R2.Bottom := R2.Bottom + 5;
      Canvas.Pen.Color := BlendColor(PC, clWhite, 50);
      Canvas.Rectangle(R2);
      
      Canvas.Pen.Color := PC;
      Canvas.Rectangle(R);
    end
    else
    begin
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Left, R.Bottom);
      if BothSideBorder then
      begin
        Canvas.MoveTo(R.Right - 1, R.Top);
        Canvas.LineTo(R.Right - 1, R.Bottom);
        R.Right := R.Right - 1;
      end;
      R.Left := R.Left + 1;
      Canvas.Pen.Color := clWhite;//BlendColor(PC, clWhite, 50);
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R);
    end;
  end;
end;

//------------------------------------------------------------------------------

// Draw Auto centered Arrow
procedure DrawArrow(Canvas: TCanvas; R: TRect; ArClr, ArShad: TColor; LeftDir: Boolean);  // Dir : true = Left; False = down
var
  ArP: TPoint;
  i, j: Integer;
begin
  if not LeftDir then      // Down direction
  begin
    j := 6;
    i := R.Right - R.Left;
    if (ArShad <> clNone) then
      j := j + 0;
    ArP.X := R.Left + (i - j) div 2;
    j := 4;
    i := R.Bottom - R.Top;
    if (ArShad <> clNone) then
      j := j + 0;
    ArP.Y := R.Top + (i - j) div 2;
    
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 7, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 6, ArP.Y + 1);
    Canvas.MoveTo(ArP.X + 2, ArP.Y + 2);
    Canvas.LineTo(ArP.X + 5, ArP.Y + 2);
    Canvas.Pixels[ArP.X + 3, ArP.Y + 3] := ArClr;
    if (ArShad <> clNone) then
    begin
      Canvas.Pen.Color := ArShad;
      Canvas.MoveTo(ArP.X - 1, ArP.Y - 1);
      Canvas.LineTo(ArP.X + 8, ArP.Y - 1);
      Canvas.Pixels[ArP.X - 1, ArP.Y] := ArShad;
      //Canvas.Pixels[ArP.X - 1, ArP.Y + 1] := ArShad;
      Canvas.Pixels[ArP.X + 7, ArP.Y] := ArShad;
      //Canvas.Pixels[ArP.X + 7, ArP.Y + 1] := ArShad;

      Canvas.Pixels[ArP.X, ArP.Y + 1] := ArShad;
      Canvas.Pixels[ArP.X + 1, ArP.Y + 2] := ArShad;
      Canvas.Pixels[ArP.X + 2, ArP.Y + 3] := ArShad;
      Canvas.Pixels[ArP.X + 3, ArP.Y + 4] := ArShad;
      Canvas.Pixels[ArP.X + 6, ArP.Y + 1] := ArShad;
      Canvas.Pixels[ArP.X + 5, ArP.Y + 2] := ArShad;
      Canvas.Pixels[ArP.X + 4, ArP.Y + 3] := ArShad;
    end;
  end
  else
  begin
    j := 4;
    i := R.Right - R.Left;
    ArP.X := R.Left + (i - j) div 2;
    j := 6;
    i := R.Bottom - R.Top;
    ArP.Y := R.Top + (i - j) div 2;

    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X, ArP.Y + 7);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 1, ArP.Y + 6);
    Canvas.MoveTo(ArP.X + 2, ArP.Y + 2);
    Canvas.LineTo(ArP.X + 2, ArP.Y + 5);
    Canvas.Pixels[ArP.X + 3, ArP.Y + 3] := ArClr;
  end;
end;

//------------------------------------------------------------------------------

// Draw Auto centered Scroll Arrow
procedure DrawScrollArrow(Canvas: TCanvas; R: TRect; ArClr: TColor);
var
  ArP: TPoint;
  i, h, w: Integer;

  procedure DrawSingleArrow;
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y + 2);
    Canvas.LineTo(ArP.X + 2, ArP.Y + 2);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 3, ArP.Y + 1);
    Canvas.MoveTo(ArP.X + 2, ArP.Y);
    Canvas.LineTo(ArP.X + 4, ArP.Y);

    Canvas.MoveTo(ArP.X + 1, ArP.Y + 3);
    Canvas.LineTo(ArP.X + 3, ArP.Y + 3);
    Canvas.MoveTo(ArP.X + 2, ArP.Y + 4);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 4);
  end;
begin
  w := 7;
  i := R.Right - R.Left - w;
  ArP.X := R.Left + (i div 2);
  h := 5;
  i := R.Bottom - R.Top - h;
  ArP.Y := R.Top + (i div 2);
  DrawSingleArrow;
  ArP.X := ArP.X + 4;
  DrawSingleArrow;
end;
//------------------------------------------------------------------------------

{ TDropForm }

constructor TDropForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
end;

//------------------------------------------------------------------------------

constructor TDropForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  if (AOwner is TCustomExplorerTreeview) then
    FExplorerTreeview := TCustomExplorerTreeview(AOwner)
  else
    raise Exception.Create('Invalid parent');
end;

//------------------------------------------------------------------------------

procedure TDropForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  { Create a sizeable window with no caption }
  if FSizeable then
    Params.Style := WS_ThickFrame or WS_PopUp or WS_Border;
end;

//------------------------------------------------------------------------------

function TDropForm.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

//------------------------------------------------------------------------------

procedure TDropForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMActivate(var Message: TWMActivate);
begin
  if Message.Active = integer(False) then
  begin
    {if HideOnDeActivate and Visible then
    begin
      FHideTimer.Enabled := true;
    end;}
  end
  else if Assigned(FExplorerTreeview) then
  begin
    if Self.Visible then
    begin
      //FExplorerTreeview.FAutoComListBox.SetFocus;
      FExplorerTreeview.SetFocus;
      SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
    end
    else
    begin
    
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMClose(var Msg: TMessage);
begin
  inherited;
  //self.Free;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMKeyDown(var Msg: TWMKeydown);
begin
  if (Msg.CharCode = VK_UP) then
  begin
    inherited;
    Exit;
  end;

  if (Msg.CharCode = VK_DOWN) then
  begin
    inherited;
    Exit;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

destructor TDropForm.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if Sizeable and Visible and FDroppedDown and ((Message.XPos < Left + 5) or (Message.YPos < Top + 5)) then
  begin
    Message.Result := 0;
    Exit;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropForm.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

{ TAdvTreeNode }

procedure TreeViewError(const Msg: string);
begin
  raise ETreeViewError.Create(Msg);
end;

//------------------------------------------------------------------------------

procedure TreeViewErrorFmt(const Msg: string; Format: array of const);
begin
  raise ETreeViewError.CreateFmt(Msg, Format);
end;

//------------------------------------------------------------------------------

constructor TAdvTreeNode.Create(AOwner: TAdvTreeNodes);
begin
  inherited Create;
  FOwner := AOwner;
  FShowText := true;
  FImageIndex := -1;
end;

//------------------------------------------------------------------------------

destructor TAdvTreeNode.Destroy;
var
  N, PN, NN: TAdvTreeNode;
  i: Integer;
begin
  FDeleting := True;
  Owner.Owner.DoDelete(Self);
  
  DeleteChildren;
  //if Owner.Owner <> nil then
    //Owner.Owner.FSelections.Remove(Self);

  N := Parent;
  if (((N <> nil) and (not N.Deleting)) or (N = nil)) and (not Owner.FDeleting) and Assigned(Owner) and Assigned(Owner.Owner) and not (csDestroying in Owner.Owner.ComponentState) then
  begin
    PN := getPrevSibling;
    NN := getNextSibling;

    if not Assigned(NN) then
    begin
      if IsFirstNode then
        Owner.FFirstNode := nil
      else if Assigned(Parent) and IsFirstChild then
        Parent.FFirstChild := nil
      else
      begin
        if Assigned(PN) then
          PN.FNextSibling := nil;
      end;  
    end
    else if not Assigned(PN) then
    begin
      if IsFirstNode then
        Owner.FFirstNode := NN
      else if Assigned(Parent) and IsFirstChild then
        Parent.FFirstChild := NN;
    end
    else     
    begin
      PN.FNextSibling := NN;
      NN.FPrevSibling := PN;
    end;
  end;

  if not Assigned(Parent) then
  begin
    i := Owner.FNodeList.IndexOf(Self);
    if (i >= 0) then
      Owner.FNodeList.Delete(i);
  end;

  if Assigned(Owner) and Assigned(Owner.Owner) and (not Owner.FDeleting) and not (csDestroying in Owner.Owner.ComponentState) then
  begin
    i := Owner.Owner.FDropDownList.IndexOf(Self);
    if (i >= 0) then
      Owner.Owner.FDropDownList.Delete(i);
  end;  
  //if Owner.Owner <> nil then
    //Owner.Owner.Delete(Self);
  Data := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetTreeView: TCustomExplorerTreeview;
begin
  Result := Owner.Owner;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.HasAsParent(Value: TAdvTreeNode): Boolean;
begin
  if Value <> Nil then
  begin
    if Parent = nil then Result := False
    else if Parent = Value then Result := True
    else Result := Parent.HasAsParent(Value);
  end
  else Result := True;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetText(const S: string);
begin
  if not Deleting and (S <> Text) then
  begin
    FText := S;
    if Assigned(Owner) and Assigned(Owner) then
      Owner.Owner.UpdateNodeInAutoComList(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetVirtualParent(const Value: Boolean);
begin
  FVirtualParent := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetData(Value: TObject);
begin
  if not Deleting and (Value <> Data) then
  begin
    FData := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetImageIndex(Value: TImageIndex);
begin
  if (Value <> FImageIndex) then
  begin
    FImageIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.CompareCount(CompareMe: Integer): Boolean;
var
  Count: integer;
  Node: TAdvTreeNode;
Begin
  Count := 0;
  Result := False;
  Node := GetFirstChild;
  while Node <> nil do
  begin
    Inc(Count);
    Node := Node.GetNextChild(Node);
    if Count > CompareMe then
      Exit;
  end;
  if Count = CompareMe then
    Result := True;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetHasChildren: Boolean;
begin
  Result := Assigned(FFirstChild);
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetParent: TAdvTreeNode;
begin
  Result := FParentNode;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetNextSibling: TAdvTreeNode;
begin
  Result := FNextSibling;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetPrevSibling: TAdvTreeNode;
begin
  Result := FPrevSibling;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetNextChild(Value: TAdvTreeNode): TAdvTreeNode;
begin
  if Value <> nil then Result := Value.GetNextSibling
  else Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetPrevChild(Value: TAdvTreeNode): TAdvTreeNode;
begin
  if Value <> nil then Result := Value.GetPrevSibling
  else Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetFirstChild: TAdvTreeNode;
begin
  Result := FFirstChild;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetLastChild: TAdvTreeNode;
var
  Node: TAdvTreeNode;
begin
  Result := GetFirstChild;
  if Result <> nil then
  begin
    Node := Result;
    repeat
      Result := Node;
      Node := Result.GetNextSibling;
    until Node = nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetIndex: Integer;
var
  Node: TAdvTreeNode;
begin
  Result := -1;
  Node := Self;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetPrevSibling;
  end;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetItem(Index: Integer): TAdvTreeNode;
begin
  Result := GetFirstChild;
  while (Result <> nil) and (Index > 0) do
  begin
    Result := GetNextChild(Result);
    Dec(Index);
  end;
  if Result = nil then TreeViewError(Format(SListIndexError, [Index]));
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetItem(Index: Integer; Value: TAdvTreeNode);
begin
  item[Index].Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetNodeButton(const Value: TNodeButton);
begin
  FNodeButton := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.SetShowText(const Value: Boolean);
begin
  FShowText := Value;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.IndexOf(Value: TAdvTreeNode): Integer;
var
  Node: TAdvTreeNode;
begin
  Result := -1;
  Node := GetFirstChild;
  while (Node <> nil) do
  begin
    Inc(Result);
    if Node = Value then Break;
    Node := GetNextChild(Node);
  end;
  if Node = nil then Result := -1;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetCount: Integer;
var
  Node: TAdvTreeNode;
begin
  Result := 0;
  Node := GetFirstChild;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetNextChild(Node);
  end;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.IsFirstNode: Boolean;
begin
  Result := Self = Owner.FFirstNode; // not Deleting and (Parent = nil) and (GetPrevSibling = nil);
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.IsFirstChild: Boolean;
begin
  Result := IsFirstNode or (Assigned(Parent) and (Parent.getFirstChild = Self));
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.GetLevel: Integer;
var
  Node: TAdvTreeNode;
begin
  Result := 0;
  Node := Parent;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.Parent;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.Delete;
begin
  if not Deleting then
    Free;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.DeleteChildren;
var
  ChildList: TDbgList;
  N: TAdvTreeNode;
  i: Integer;
begin
  ChildList := TDbgList.Create;
  N := GetFirstChild;
  if N <> nil then
  begin
    repeat
      ChildList.Add(N);
      N := N.GetNextSibling;
    until N = nil;
  end;

  for i := ChildList.Count - 1 downto 0 do
  begin
    ChildList[i].Delete;
  end;
  ChildList.Free;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.AddChild(const S: string): TAdvTreeNode;
begin
  Result := AddChild(nil, S);
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.AddChild(Data: TObject; const S: string): TAdvTreeNode;
var
  N: TAdvTreeNode;
begin
  N := GetLastChild;
  if Assigned(N) then
    Result := Owner.InsertNode(nil, N, False)
  else
    Result := Owner.InsertNode(Self, nil, False);
  Result.Text := S;
  Result.Data := Data;
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.AddFirstChild(const S: string): TAdvTreeNode;
begin
  Result := AddFirstChild(nil, S);
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.AddFirstChild(Data: TObject; const S: string): TAdvTreeNode;
begin
  if not Assigned(FFirstChild) then
    Result := Owner.InsertNode(Self, nil, True)
  else
    Result := Owner.InsertNode(nil, FFirstChild, True);
  Result.Text := S;
  Result.Data := Data;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.Assign(Source: TPersistent);
var
  Node: TAdvTreeNode;
begin
  if not Deleting and (Source is TAdvTreeNode) then
  begin
    Node := TAdvTreeNode(Source);
    Text := Node.Text;
    Data := Node.Data;
    ImageIndex := Node.ImageIndex;
    ShowText := Node.ShowText;
    VirtualParent := Node.VirtualParent;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TAdvTreeNode.IsEqual(Node: TAdvTreeNode): Boolean;
begin
  Result := (Text = Node.Text) and (Data = Node.Data);
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.ReadData(Stream: TStream; Info: PNodeInfo);
var
  I, Size, ItemCount: Integer;
  LNode: TAdvTreeNode;
begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Info^, Size);
  Text := string(Info^.Text);
  ImageIndex := Info^.ImageIndex;

  ItemCount := Info^.Count;
  for I := 0 to ItemCount - 1 do
  begin
    LNode := AddChild(''); //Owner.Add(Self, '');
    LNode.FParentNode := Self;
    LNode.ReadData(Stream, Info);
    //Owner.Owner.Added(LNode);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNode.WriteData(Stream: TStream; Info: PNodeInfo);
var
  I, Size, L, ItemCount: Integer;
begin
  L := Length(Text);
  if L > 255 then L := 255;
  Size := SizeOf(TNodeInfo) + L - 255;
  Info^.Text := shortstring(Text);
  Info^.ImageIndex := ImageIndex;

  ItemCount := Count;
  Info^.Count := ItemCount;
  Stream.WriteBuffer(Size, SizeOf(Size));
  Stream.WriteBuffer(Info^, Size);
  for I := 0 to ItemCount - 1 do
    Item[I].WriteData(Stream, Info);
end;

//------------------------------------------------------------------------------

{ TAdvTreeNodes }

constructor TAdvTreeNodes.Create(AOwner: TCustomExplorerTreeview);
begin
  inherited Create;
  FOwner := AOwner;
  FFirstNode := nil;
  FNodeList :=  TDbgList.Create;
end;

//------------------------------------------------------------------------------

destructor TAdvTreeNodes.Destroy;
begin
  FDeleting := True;
  Clear;
  FNodeList.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.Delete(Node: TAdvTreeNode);
begin
  Node.Delete;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.Clear;
var
  i: Integer;
begin
  for i := FNodeList.Count - 1 downto 0 do
    FNodeList[i].Delete;

  FNodeList.Clear;
  if Assigned(Owner) then
    Owner.SelectedNode := nil;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.GetNodeFromIndex(Index: Integer): TAdvTreeNode;
begin
  Result := nil;
  if (Index >= 0) and (Index < FNodeList.Count) then
    Result := FNodeList[Index];
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.SetItem(Index: Integer; Value: TAdvTreeNode);
begin
  if (Index >= 0) and (Index < FNodeList.Count) then
    GetNodeFromIndex(Index).Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.Insert(Sibling: TAdvTreeNode; const S: string): TAdvTreeNode;
begin
  Result := InsertObject(Sibling, S, nil);
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.InsertObject(Sibling: TAdvTreeNode; const S: string;
  Data: TObject): TAdvTreeNode;
begin
  Result := InsertNode(nil, Sibling, True);
  if Assigned(Result) then
  begin
    Result.Text := S;
    Result.Data := Data;
  end;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.IsVeryFirstNode(Node: TAdvTreeNode): Boolean;
begin
  Result := Assigned(Node) and (Node = FFirstNode);  
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.GetFirstNode: TAdvTreeNode;
begin
  Result := FFirstNode;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.GetLastNode: TAdvTreeNode;
begin
  Result := FFirstNode;
  if Assigned(Result) then
    while (Result.getNextSibling <> nil) do
      Result := Result.getNextSibling;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.Add(const S: string): TAdvTreeNode;
begin
  Result := Add(nil, S);
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.Add(Data: TObject; const S: string): TAdvTreeNode;
begin
  Result := InsertNode(nil, GetLastNode, False);
  Result.Text := S;
  Result.Data := Data;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.AddFirst(const S: string): TAdvTreeNode;
begin
  Result := AddFirst(nil, S);
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.AddFirst(Data: TObject; const S: string): TAdvTreeNode;
begin
  if not Assigned(FFirstNode) then
  begin
    //Result := Owner.CreateNode;
    //FFirstNode := Result
    Result := InsertNode(nil, nil, True);
  end
  else
  begin
    Result := InsertNode(nil, FFirstNode, True);
  end;
  Result.Text := S;
  Result.Data := Data;
end;

//------------------------------------------------------------------------------

function TAdvTreeNodes.InsertNode(ParentNode: TAdvTreeNode; DesNode: TAdvTreeNode; InsertBefore: Boolean): TAdvTreeNode;
var
  N: TAdvTreeNode;
begin
  Result := nil;
  if not Assigned(DesNode) then
  begin
    if Assigned(ParentNode) then
    begin
      N := ParentNode.GetLastChild;
      if Assigned(N) then
      begin
        InsertNode(nil, N, False);
        Exit;
      end
      else  // First Child Node
      begin
        Result := Owner.CreateNode;
        ParentNode.FFirstChild := Result;
        Result.FParentNode := ParentNode;
      end;  
    end
    else
    begin
      if not Assigned(FFirstNode) then
      begin
        Result := Owner.CreateNode;
        FFirstNode := Result;  // very first Node
        FNodeList.Add(Result);
      end;
    end;
  end
  else
  begin
    Result := Owner.CreateNode;
    if InsertBefore then
    begin
      if IsVeryFirstNode(DesNode) then
      begin
        FFirstNode := Result;
        Result.FNextSibling := DesNode;
        DesNode.FPrevSibling := Result;
        FNodeList.Add(Result);
      end
      else
      begin
        if DesNode.IsFirstNode and Assigned(DesNode.Parent) then
        begin
          DesNode.Parent.FFirstChild := Result;
          Result.FNextSibling := DesNode;
          DesNode.FPrevSibling := Result;
          Result.FParentNode := DesNode.Parent;
        end
        else
        begin
          Result.FPrevSibling := DesNode.FPrevSibling;
          Result.FNextSibling := DesNode;
          DesNode.FPrevSibling := Result;
          Result.FParentNode := DesNode.Parent;
          if Assigned(Result.FPrevSibling) then
            Result.FPrevSibling.FNextSibling := Result;
        end;
      end;
    end
    else // Insert after DesNode
    begin
      Result.FNextSibling := DesNode.FNextSibling;
      Result.FPrevSibling := DesNode;
      DesNode.FNextSibling := Result;
      if Assigned(Result.FNextSibling) then
        Result.FNextSibling.FPrevSibling := Result;
      Result.FParentNode := DesNode.Parent;
      if not Assigned(DesNode.Parent) then  // Root Node
        FNodeList.Add(Result);
    end;
  end;
  if Assigned(Result) and not(csLoading in FOwner.ComponentState) then
    Owner.DoAdded(Result);
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.Assign(Source: TPersistent);
var
  TreeNodes: TAdvTreeNodes;
  MemStream: TMemoryStream;
begin
  if Source is TAdvTreeNodes then
  begin
    TreeNodes := TAdvTreeNodes(Source);
    Clear;
    MemStream := TMemoryStream.Create;
    try
      TreeNodes.WriteData(MemStream);
      MemStream.Position := 0;
      ReadData(MemStream);
    finally
      MemStream.Free;
    end;
  end
  else inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.DefineProperties(Filer: TFiler);

  function WriteNodes: Boolean;
  var
    I: Integer;
    Nodes: TAdvTreeNodes;
  begin
    Nodes := TAdvTreeNodes(Filer.Ancestor);
    if Nodes = nil then
      Result := Count > 0
    else if Nodes.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Item[I].IsEqual(Nodes[I]);
        if Result then
          Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes);
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.ReadData(Stream: TStream);
var
  I, Count: Integer;
  NodeInfo: TNodeInfo;
  LNode: TAdvTreeNode;
begin
  FReading := True;
  try
    Clear;
    Stream.ReadBuffer(Count, SizeOf(Count));
    for I := 0 to Count - 1 do
    begin
      LNode := Add('');
      LNode.ReadData(Stream, @NodeInfo);
      Owner.DoAdded(LNode);
    end;
  finally
    FReading := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTreeNodes.WriteData(Stream: TStream);
var
  I: Integer;
  Node: TAdvTreeNode;
  NodeInfo: TNodeInfo;
begin
  I := 0;
  Node := GetFirstNode;
  while Node <> nil do
  begin
    Inc(I);
    Node := Node.GetNextSibling;
  end;
  Stream.WriteBuffer(I, SizeOf(I));
  Node := GetFirstNode;
  while Node <> nil do
  begin
    Node.WriteData(Stream, @NodeInfo);
    Node := Node.GetNextSibling;
  end;
end;

//------------------------------------------------------------------------------

{ TButtonAppearance }

procedure TButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TButtonAppearance) then
  begin
    FColorHot := TButtonAppearance(Source).ColorHot;
    FColorHotTo := TButtonAppearance(Source).ColorHotTo;
    FColorMirrorHot := TButtonAppearance(Source).ColorMirrorHot;
    FColorMirrorHotTo := TButtonAppearance(Source).ColorMirrorHotTo;

    FBorderColorHot := TButtonAppearance(Source).FBorderColorHot;
    FArrowColorHot := TButtonAppearance(Source).FArrowColorHot;

    FArrowColorDown := TButtonAppearance(Source).FArrowColorDown;
    FColorDownTo := TButtonAppearance(Source).FColorDownTo;
    FColorDown := TButtonAppearance(Source).FColorDown;
    FColorMirrorDownTo := TButtonAppearance(Source).FColorMirrorDownTo;
    FColorMirrorDown := TButtonAppearance(Source).FColorMirrorDown;
    FBorderColorDown := TButtonAppearance(Source).FBorderColorDown;
    FFont.Assign(TButtonAppearance(Source).Font);
    FColorMirrorNodeHotTo := TButtonAppearance(Source).FColorMirrorNodeHotTo;
    FBorderColorNodeHot := TButtonAppearance(Source).FBorderColorNodeHot;
    FColorNodeHot := TButtonAppearance(Source).FColorNodeHot;
    FColorMirrorNodeHot := TButtonAppearance(Source).FColorMirrorNodeHot;
    FColorNodeHotTo := TButtonAppearance(Source).FColorNodeHotTo;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TButtonAppearance.Create;
begin
  inherited;
  FColorDown := RGB(228, 241, 252);
  FColorDownTo := RGB(201, 231, 247);
  FColorMirrorDown := RGB(147, 206, 237);
  FColorMirrorDownTo := RGB(109, 182, 221);
  FFont := TFont.Create;
  FFont.OnChange := OnFontChanged;
  FArrowColorHot := clBlack;
  FArrowColorDown := clBlack;
  FBorderColorHot := RGB(22, 88, 139);
  FColorHot := RGB(211, 237, 251);
  FColorHotTo := RGB(199, 233, 250);
  FColorMirrorHot := RGB(156, 216, 247);
  FColorMirrorHotTo := RGB(137, 208, 245);
  FBorderColorDown := RGB(44, 98, 139);

  FBorderColorNodeHot := RGB(142, 143, 143);
  FColorNodeHot := RGB(242, 242, 242);
  FColorNodeHotTo := RGB(238, 238, 238);
  FColorMirrorNodeHot := RGB(217, 217, 217);
  FColorMirrorNodeHotTo := RGB(210, 210, 210);
end;

//------------------------------------------------------------------------------

destructor TButtonAppearance.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDown(const Value: TColor);
begin
  if (FColorDown <> Value) then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorDown(const Value: TColor);
begin
  if (FColorMirrorDown <> Value) then
  begin
    FColorMirrorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorDownTo(const Value: TColor);
begin
  if (FColorMirrorDownTo <> Value) then
  begin
    FColorMirrorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorDownTo(const Value: TColor);
begin
  if (FColorDownTo <> Value) then
  begin
    FColorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.OnFontChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetArrowColorDown(const Value: TColor);
begin
  FArrowColorDown := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetArrowColorHot(const Value: TColor);
begin
  FArrowColorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorDown(const Value: TColor);
begin
  FBorderColorDown := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorHot(const Value: TColor);
begin
  FBorderColorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorHot(const Value: TColor);
begin
  FColorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorHotTo(const Value: TColor);
begin
  FColorHotTo := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorHot(const Value: TColor);
begin
  FColorMirrorHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorHotTo(const Value: TColor);
begin
  FColorMirrorHotTo := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetBorderColorNodeHot(const Value: TColor);
begin
  FBorderColorNodeHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorNodeHot(const Value: TColor);
begin
  FColorMirrorNodeHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorMirrorNodeHotTo(const Value: TColor);
begin
  FColorMirrorNodeHotTo := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorNodeHot(const Value: TColor);
begin
  FColorNodeHot := Value;
end;

//------------------------------------------------------------------------------

procedure TButtonAppearance.SetColorNodeHotTo(const Value: TColor);
begin
  FColorNodeHotTo := Value;
end;

//------------------------------------------------------------------------------

{ TExpTreeviewAppearance }

procedure TExpTreeviewAppearance.Assign(Source: TPersistent);
begin
  if (Source is TExpTreeviewAppearance) then
  begin
    FColor := (Source as TExpTreeviewAppearance).Color;
    FFocusColor := (Source as TExpTreeviewAppearance).FocusColor;
    FFocusOuterBorderColor := (Source as TExpTreeviewAppearance).FFocusOuterBorderColor;
    FInnerBorderColor:= (Source as TExpTreeviewAppearance).FInnerBorderColor;
    FOuterBorderColor:= (Source as TExpTreeviewAppearance).FOuterBorderColor;
    FInnerMostBorderColor := (Source as TExpTreeviewAppearance).InnerMostBorderColor;
    FFocusInnerBorderColor:= (Source as TExpTreeviewAppearance).FFocusInnerBorderColor;
    FButtonAppearance.Assign((Source as TExpTreeviewAppearance).ButtonAppearance);
    FHotColor:= (Source as TExpTreeviewAppearance).FHotColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TExpTreeviewAppearance.Create;
begin
  inherited;
  FColor := RGB(230, 240, 250);
  FFocusColor := clWhite;
  FHotColor := RGB(244, 249, 255);
  FOuterBorderColor := clNone;
  FInnerBorderColor := $B99D7F;
  FFocusOuterBorderColor := clNone;
  FFocusInnerBorderColor := $B99D7F;
  FInnerMostBorderColor := clNone;
  FButtonAppearance := TButtonAppearance.Create;
  FButtonAppearance.OnChange := OnButtonAppearanceChanged;
end;

//------------------------------------------------------------------------------

destructor TExpTreeviewAppearance.Destroy;
begin
  FButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.OnButtonAppearanceChanged(
  Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetButtonAppearance(
  const Value: TButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetFocusColor(const Value: TColor);
begin
  if (FFocusColor <> Value) then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetFocusInnerBorderColor(
  const Value: TColor);
begin
  if (FFocusInnerBorderColor <> Value) then
  begin
    FFocusInnerBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetFocusOuterBorderColor(
  const Value: TColor);
begin
  if (FFocusOuterBorderColor <> Value) then
  begin
    FFocusOuterBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetHotColor(const Value: TColor);
begin
  if (FHotColor <> Value) then
  begin
    FHotColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetInnerBorderColor(const Value: TColor);
begin
  if (FInnerBorderColor <> Value) then
  begin
    FInnerBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetInnerMostBorderColor(
  const Value: TColor);
begin
  if (FInnerMostBorderColor <> Value) then
  begin
    FInnerMostBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TExpTreeviewAppearance.SetOuterBorderColor(const Value: TColor);
begin
  if (FOuterBorderColor <> Value) then
  begin
    FOuterBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TCustomExplorerTreeview }

constructor TCustomExplorerTreeview.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  Ctl3D := False;
  BorderStyle := bsNone;
  FTreeNodes := CreateNodes;
  FSelectedNode := nil;
  FOldSelected := nil;
  FFolderPath := '';
  Text := '';
  FShowHiddenFolders := False;

  FInternalImages := nil;

  ControlStyle := ControlStyle - [csSetCaption];

  FEditorEnabled := False;

  FNodeButtons := TList.Create;
  FText := Text;

  FAppearance := TExpTreeviewAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;

  FRefreshGlyph := TBitmap.Create;
  FRefreshGlyph.LoadFromResourceName(HInstance, 'REFRESH_BTN');
  FRefreshGlyph.OnChange := OnRefreshGlyphChanged;
  FRefreshBtn := nil;
  FRefreshImageIndex := -1;

  RefreshButton := True;

  FDropDownButton := TDropDownButton.Create(Self);
  FDropDownButton.Parent := Self;
  FDropDownButton.OnMouseDown := OnDropDownBtnMouseDown;
  FDropDownButton.OnClick := OnDropDownBtnClick;
  FDropDownButton.Visible := True;

  UpdateDropDownRefreshBtnsPos;

  FLeftIcon := TLeftIcon.Create(Self);
  FLeftIcon.Parent := Self;
  FLeftIcon.OnClick := OnLeftIconClick;
  FLeftIcon.OnDblClick := OnLeftIconDblClick;
  FLeftIcon.Visible := True;

  ShowImage := True;

  FMaxDropHeight := 150;

  FDropDownList := TDbgList.Create;

  FAutoCompleteList := TStringList.Create;
  FAutoCompleteList.Duplicates := dupIgnore;
  FAutoCompleteList.Sorted := True;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnTimerTime;

  FMyComputerNode := nil;
  FMode := aeSystem;

  FAutoComplete := True;
  FAutoDropDownFill := True;
  
  FEditStyle := esList;

  FAllowHook := True;
  
  Width := 300;
  Height := 25;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CreateWnd;
begin
  inherited;
  SetEditRect;
  self.ReadOnly := not EditorEnabled;
end;

//------------------------------------------------------------------------------

destructor TCustomExplorerTreeview.Destroy;
begin
  RemoveNodeButtons;
  Items.Clear;
  if Assigned(FRefreshBtn) then
    FRefreshBtn.Free;
  FDropDownButton.Free;
  FAppearance.Free;
  FLeftIcon.Free;

  FNodeButtons.Free;
  FRefreshGlyph.Free;
  FDropDownList.Free;
  FAutoCompleteList.Free;
  FTimer.Free;
  if Assigned(FInternalImages) then
    FInternalImages.Free;
  Items.Free;  
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.DestroyWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.DoDelete(Node: TAdvTreeNode);
begin
  DeleteFromAutoComList(Node);
  if Assigned(FOnDeletion) then FOnDeletion(Self, Node);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.DoAdded(Node: TAdvTreeNode);
begin
  AddInAutoComList(Node);
  if Assigned(FOnAddition) then FOnAddition(Self, Node);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateNodeInAutoComList(Node: TAdvTreeNode);
var
  i: Integer;
begin
  if AutoComplete and Assigned(Node) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    i := FAutoCompleteList.IndexOfObject(Node);
    if (i >= 0) then
    begin
      FAutoCompleteList.Delete(i);
      FAutoCompleteList.AddObject(GetHierarchicalNodeText(Node), Node);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.AddInAutoComList(Node: TAdvTreeNode);
begin
  if AutoComplete and Assigned(Node) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    AddWithChildrenToAutoCompleteList(Node);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TCustomExplorerTreeview.DeleteFromAutoComList(Node: TAdvTreeNode);
var
  i: Integer;
begin
  if AutoComplete and Assigned(Node) and Assigned(FAutoCompleteList) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) and not (csDestroying in ComponentState) then
  begin
    i := FAutoCompleteList.IndexOfObject(Node);
    if (i >= 0) then
      FAutoCompleteList.Delete(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.AddToDropDownList(Node: TAdvTreeNode);
begin
  if IsMyNode(Node) and (FDropDownList.IndexOf(Node) < 0) then
  begin
    FDropDownList.Add(Node);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.RemoveFromDropDownList(Node: TAdvTreeNode);
var
  i: Integer;
begin
  i := FDropDownList.IndexOf(Node);
  if (i >= 0) then
    RemoveFromDropDownList(i);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.RemoveFromDropDownList(Index: Integer);
begin
  if (Index >= 0) and (Index < FDropDownList.Count) then
    FDropDownList.Delete(Index);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.Clear;
begin
  inherited;
  if Assigned(Items) then
    Items.Clear;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.ClearDropDownList;
begin
  FDropDownList.Clear;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetDropDownCount: integer;
begin
  Result := FDropDownList.Count;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetDropDownList(index: integer): TAdvTreeNode;
begin
  Result := nil;
  if (Index >= 0) and (Index < FDropDownList.Count) then
    Result := FDropDownList[Index];
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.ClickButton(Button: TNodeButton;
  RealClick: Boolean);
var
  P: TPoint;
begin
  FCaptureChangeCancels := False;
  //P := Button.ClientToScreen(Point(Button.Width - 3, 2));
  P := Point(Button.Width - 3, 2);
  Button.FInternalClick := True;
  if not RealClick or true then
  begin
    if (Button.IsSplitButton) then
      PostMessage(Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(p.x, p.y));
  end
  else
  begin
    if (Button.IsSplitButton) then
    begin
      mouse_event( MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0 );
      {$IFNDEF DELPHI2006_LVL}
      mouse_event( MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 );
      {$ENDIF}
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CancelMenu;
begin
  MouseCapture := False;
  FCaptureChangeCancels := False;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.Change;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CMEnter(var Message: TCMGotFocus);
begin
  if EditorEnabled then
  begin
    EditMode := True;
    //RemoveNodeButtons;

    if AutoSelect and not (csLButtonDown in ControlState) then
      SelectAll;
  end;

  inherited;

  if EditorEnabled and Assigned(OnShowEdit) then
    OnShowEdit(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CMExit(var Message: TCMExit);
begin
  inherited;
  if EditorEnabled then
  begin
    EditMode := False;
    //GenerateNodeButtons;
    if Assigned(OnHideEdit) then
      OnHideEdit(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := True;
  if not Focused or not EditorEnabled then
    Color := Appearance.HotColor;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := False;
  //if (not Focused or not EditorEnabled) and ((EditorEnabled and (EditStyle = esEdit) and Assigned(SelectedNode)) or ((EditStyle = esList))) then
    Color := Appearance.Color;
  Invalidate;  
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.DoEnter;
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetParentForm(
  Control: TControl): TCustomForm;
begin
  Result := nil;
  if Assigned(Control) then
    if Control is TCustomForm then
    begin
      Result := Control as TCustomForm;
      Exit;
    end else
    begin
      if Assigned(Control.Parent) then
        Result := GetParentForm(Control.Parent);
    end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetSelectedNode: TAdvTreeNode;
begin
  Result := FSelectedNode;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetSelectedNode(const Value: TAdvTreeNode);
var
  N: TAdvTreeNode;
  s: string;
begin
  if (Value <> FSelectedNode) then
  begin
    RemoveNodeButtons;
    FOldSelected := FSelectedNode;
    FSelectedNode := Value;
    s := GetHierarchicalNodeText(FSelectedNode, False);
    if Assigned(SelectedNode) and (Mode = aeFolder) and (SelectedNode = Items.GetFirstNode) then
      s := '';
    {if IsFocused and not Assigned(FSelectedNode) then
    begin
    end
    else}
      SetDirectText(s);
    if Assigned(SelectedNode) then
    begin
      N := SelectedNode.getFirstChild;
      if not Assigned(N) and (Mode in [aeSystem, aeFolder]) then
        AddSubFolders(GetHierarchicalNodeText(FSelectedNode), SelectedNode);
    end;

    if not IsUpdating then
    begin
      if not IsFocused or (not EditMode) then
        GenerateNodeButtons
      else
        UpdateImage;
    end;

    if Assigned(FSelectedNode) and AutoDropDownFill and (Mode in [aeSystem, aeFolder]) then
    begin
      AddToDropDownList(FSelectedNode);
      N := FSelectedNode.getFirstChild;
      while (N <> nil) do
      begin
        AddToDropDownList(N);
        N := N.getNextSibling;
      end;
    end;

    if Assigned(FSelectedNode) and Assigned(FOnSelect) and not IsUpdating and not IsInternal then
      FOnSelect(Self, FSelectedNode);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TCustomExplorerTreeview.RefreshSubFolders(
  ParentNode: TAdvTreeNode);
var
  s: string;
begin
  if not Assigned(ParentNode) or not IsMyNode(ParentNode) then
    Exit;

  if (Mode in [aeFolder, aeSystem]) then
  begin
    s := GetNodePath(SelectedNode);
    SelectedNode := nil;
    ParentNode.DeleteChildren;

    if (Items.GetFirstNode = ParentNode) then  // Parent and virtual Node
      AddSubFolders(FolderPath, ParentNode)
    else
      AddSubFolders(GetNodePath(ParentNode), ParentNode);

    SetSelectedFolder(s);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateNodeButtonsPositions;
var
  i, x: Integer;
  R: TRect;
begin
  R := GetNodeButtonsRect;
  x := R.Left;
  for I := FNodeButtons.Count - 1 downto 0 do
  begin
    with TNodeButton(FNodeButtons[i]) do
    begin
      Top := R.Top;
      Left := x;
      x := x + Width;
      Height := R.Bottom - R.Top;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.RemoveNodeButtons;
var
  i: Integer;
begin
  for i := 0 to FNodeButtons.Count - 1 do
  begin
    with TNodeButton(FNodeButtons[i]) do
    begin
      if Assigned(Node) then
        Node.Button := nil;
      Free;
    end;
  end;
  FNodeButtons.Clear;

  if not (csDestroying in ComponentState) then
  begin
    inherited Text := FText;
    SelectAll;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.GenerateNodeButtons;
  function CreateNodeButton: TNodeButton;
  begin
    Result := TNodeButton.Create(Self);
    Result.Parent := Self;
    Result.Visible := True;
  end;
var
  NB: TNodeButton;
  N: TAdvTreeNode;
  s: Integer;
  R: TRect;
  //bmp: TBitmap;
begin
  RemoveNodeButtons;
  if Assigned(FSelectedNode) then
  begin
    R := GetNodeButtonsRect;
    s := R.Right - R.Left;  // available space for buttons
    if (S < DwBUTTON_WIDTH) then  // not enough space to even display scroll button
      Exit;
      
    N := FSelectedNode;
    while (N <> nil) do
    begin
      NB := CreateNodeButton;
      NB.Node := N;
      N := N.GetParent;

      FNodeButtons.Add(NB);
      if (NB.Width > s) or (Assigned(N) and (NB.Width > s - DwBUTTON_WIDTH)) then
      begin
        NB.ScrollButton := True;
        Break;
      end;
      s := s - NB.Width;
    end;
    UpdateNodeButtonsPositions;
    UpdateImage;

    inherited Text := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateImage;
begin
  if ShowImage and Assigned(CurrentImages) and Assigned(FSelectedNode) and (FSelectedNode.ImageIndex >= 0) then
  begin
    {bmp := TBitmap.Create;
    bmp.Width := Images.Width;
    bmp.Height := Images.Height;
    bmp.Transparent := True;
    Images.Draw(bmp.Canvas, 0, 0, FSelectedNode.ImageIndex);}
    if Assigned(FleftIcon) and Assigned(FleftIcon.Glyph) then
    begin
      FleftIcon.Glyph.Canvas.Brush.Color := clWhite; //clFuchsia;
      FleftIcon.Glyph.Canvas.FillRect(FleftIcon.ClientRect);
      FleftIcon.Glyph.Assign(nil);
    end;
    CurrentImages.GetBitmap(FSelectedNode.ImageIndex, FleftIcon.Glyph);
    //bmp.Free;
  end;

end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;

  {if (Key = VK_ESCAPE) then
  begin
    SelectedNode := FOldSelected;
  end
  else
  begin
    SelectedNode := nil;
  end;}
  
  if AutoComplete then
  begin
    if (Key = VK_DELETE) then
    begin
      {if SelLength > 0 then
      begin
        FCurrentSearch := AnsiUpperCase(Copy(Text,1,SelStart));
      end
      else
        FCurrentSearch := ''; }
    end
    else if Assigned(FAutoComListBox) then
    begin
      {if (Key = VK_UP) then
        FAutoComListBox.MoveSelect(-1);

      if (Key = VK_DOWN) then
        FAutoComListBox.MoveSelect(1);}
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.KeyPress(var Key: Char);
begin
  if not EditorEnabled then
  begin
    Key := #0;
    Exit;
  end;

  if Key = #27 then
  begin
    inherited;
    Exit;
  end;

  if EditorEnabled and (EditStyle = esEdit) and (Key <> #38) and (Key <> #40) then
    FUpdateInternalText := True;
  
  if not AutoComplete then
  begin
    inherited;
    Exit;
  end;
  
 if Assigned(FAutoComListBox) then
  begin
    if (Key = #38 {VK_UP}) then
    begin
      FAutoComListBox.MoveSelect(-1);
      Exit;
    end;

    if (Key = #40 {VK_DOWN}) then
    begin
      FAutoComListBox.MoveSelect(1);
      Exit;
    end;
  end;

  inherited;

  //if FCurrentSearch[Length(FCurrentSearch)] = '\' then
    //system.Delete(FCurrentSearch, Length(FCurrentSearch), 1);

  //ShowAutoCompList;
  FShowAutoCL := True;
  PostMessage(Handle, WM_ET_TRACKKEYPRESS, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and not Focused then
  begin
    if (Appearance.FocusColor <> clNone) then
      inherited Color := Appearance.Color;
  end;

  if not (csDesigning in ComponentState) then
  begin
    if (mode = aeSystem) then
      LoadDirectoryStructure(bnDesktop, '', True)
    else if (mode = aeFolder) and (FolderPath <> '') then
      LoadDirectoryStructure(bnFolder, FolderPath, True);
  end;

  if FAutoComplete and not (csDesigning in ComponentState) then
    PopulateAutoCompleteList;
  
  if not Assigned(SelectedNode) and (Text = '') then
    SelectedNode := Items.GetFirstNode;

  if FAutoDropDownFill and not (csDesigning in ComponentState) then
  begin
    FAutoDropDownFill := False;
    AutoDropDownFill := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  
  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = Images) then
    begin
      Images := nil;
      RefreshImageIndex := -1;
    end
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnDropDownBtnClick(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.IsDroppedDown: Boolean;
begin
  Result := Assigned(FDropForm) and FDropForm.Visible;  
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnDropDownBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FDropForm) and not FCloseClick then
    ShowDropDownList;
  FCloseClick := False;  
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnDropFormDeactivate(Sender: TObject);
var
  pt: TPoint;
  r: TRect;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  r := GetDropDownButtonRect;
  FCloseClick := PtInRect(r, pt);

  HideDropDownList;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnLeftIconDblClick(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnListBoxSelect(Sender: TObject);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnListBoxClick(Sender: TObject);
begin
  if Assigned(FListBox) and (FListBox.ItemIndex >= 0) then
  begin
    SelectedNode := TAdvTreeNode(FListBox.Items.Objects[FListBox.ItemIndex]);
    SelectAll;
  end;

  HideDropDownList;
  //PostMessage(Handle, WM_ET_HIDEDROPDOWN, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnRefreshBtnClick(Sender: TObject);
begin
  EditMode := False;
  if Assigned(OnRefreshClick) then
    OnRefreshClick(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnRefreshGlyphChanged(Sender: TObject);
begin
  if Assigned(FRefreshBtn) then  
    FRefreshBtn.Glyph.Assign(FRefreshGlyph);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetComponentStyle(AStyle: TTMSStyle);
var
  i:integer;
  FIsComCtl6: boolean;
  brdr: TColor;
begin
  FTMSStyle := AStyle;
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);

  if FIsComCtl6 then
    brdr := $B99D7F
  else
    brdr := clBlack;

  with Appearance do
  begin
    case AStyle of
      tsOffice2003Blue:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $00FFD2AF;
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $00FFD2AF;
        //FocusColor := $00FFD2AF;
        FocusColor := clWhite;
        HotColor := $00FFD2AF;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsOffice2003Silver:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $00E6D8D8;
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := $00E6D8D8;
        //FocusColor := $00E6D8D8;
        FocusColor := clWhite;
        HotColor := $00E6D8D8;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsOffice2003Olive:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := RGB(225, 234, 185);
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := RGB(225, 234, 185);
        //FocusColor := RGB(225, 234, 185);
        FocusColor := clWhite;
        HotColor := RGB(225, 234, 185);
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsOffice2003Classic:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $00F2F2F2;
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := $00F2F2F2;
        FocusColor := $00F2F2F2;
        HotColor := $00F2F2F2;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $808080;
          BorderColorHot := $808080;
          BorderColorNodeHot := $808080;
          ColorDown := $B59285;
          ColorDownTo := $B59285;
          ColorMirrorDown := $B59285;
          ColorMirrorDownTo := $B59285;
          ColorHot := $D2BDB6;
          ColorHotTo := $D2BDB6;
          ColorMirrorHot := $D2BDB6;
          ColorMirrorHotTo := $D2BDB6;
          ColorNodeHot := $B59285;
          ColorNodeHotTo := $B59285;
          ColorMirrorNodeHot := $B59285;
          ColorMirrorNodeHotTo := $B59285;
        end;
      end;
      tsOffice2007Silver:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := RGB(241, 244, 248);
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := RGB(241, 244, 248);
        FocusColor := RGB(241, 244, 248);
        HotColor := RGB(241, 244, 248);
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsOffice2007Luna:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $00F3E5DA;
        }

        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := $00F3E5DA;
        //FocusColor := $00F3E5DA;
        FocusColor := clWhite;
        HotColor := $00F3E5DA;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $5C534C;
        }
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        //Color := $5C534C;
        //FocusColor := $5C534C;
        //HotColor := $5C534C;
        Color := clGray;
        FocusColor := clWhite;
        HotColor := clSilver;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $AAD9FF;
          ColorDownTo := $6EBBFF;
          ColorMirrorDown := $42AEFE;
          ColorMirrorDownTo := $7AE1FE;
          ColorHot := $EBFDFF;
          ColorHotTo := $ACECFF;
          ColorMirrorHot := $59DAFF;
          ColorMirrorHotTo := $A4E9FF;
          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $ACECFF;
          ColorMirrorNodeHot := $59DAFF;
          ColorMirrorNodeHotTo := $A4E9FF;
        end;
      end;
      tsWindowsXP:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $00B6B6B6;
        }
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := $00B6B6B6;
        FocusColor := $00B6B6B6;
        HotColor := $00B6B6B6;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := clInactiveCaption;
          ColorDownTo := clInactiveCaption;
          ColorMirrorDown := clInactiveCaption;
          ColorMirrorDownTo := clInactiveCaption;

          ColorHot := $EFD3C6;
          ColorHotTo := $EFD3C6;
          ColorMirrorHot := $EFD3C6;
          ColorMirrorHotTo := $EFD3C6;

          ColorNodeHot := $EFD3C6;
          ColorNodeHotTo := $EFD3C6;
          ColorMirrorNodeHot := $EFD3C6;
          ColorMirrorNodeHotTo := $EFD3C6;
          {
          ColorNodeHot := clInactiveCaption;
          ColorNodeHotTo := clInactiveCaption;
          ColorMirrorNodeHot := clInactiveCaption;
          ColorMirrorNodeHotTo := clInactiveCaption;
          }
        end;
      end;
      tsWhidbey:
      begin
        {
        OuterBorderColor := clSilver;
        InnerBorderColor := clBlack;
        FocusouterBorderColor := clSilver;
        FocusInnerBorderColor := clBlack;
        InnerMostBorderColor := $F5F9FA;
        }
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;

        Color := $F5F9FA;
        FocusColor := $F5F9FA;
        HotColor := $F5F9FA;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $99CEDB;
          BorderColorHot := $99CEDB;
          BorderColorNodeHot := $99CEDB;
          ColorDown := $94E6FB;
          ColorDownTo := $94E6FB;
          ColorMirrorDown := $94E6FB;
          ColorMirrorDownTo := $94E6FB;
          ColorHot := $EBFDFF;
          ColorHotTo := $EBFDFF;
          ColorMirrorHot := $EBFDFF;
          ColorMirrorHotTo := $EBFDFF;

          ColorNodeHot := $EBFDFF;
          ColorNodeHotTo := $EBFDFF;
          ColorMirrorNodeHot := $EBFDFF;
          ColorMirrorNodeHotTo := $EBFDFF;
          {
          ColorNodeHot := $94E6FB;
          ColorNodeHotTo := $94E6FB;
          ColorMirrorNodeHot := $94E6FB;
          ColorMirrorNodeHotTo := $94E6FB;
          }
        end;
      end;
      tsWindowsVista:
      begin
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $FDF8F1;

        FocusColor := clWhite;
        HotColor := $FCEFD5;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $FEDF9A;
          BorderColorHot := $FCF2DA;
          BorderColorNodeHot := $FCF2DA;
          ColorDown := $FEF9F0;
          ColorDownTo := $FDF0D7;
          ColorMirrorDown := clNone;
          ColorMirrorDownTo := clNone;
          ColorHot := $FFFDF9;
          ColorHotTo := $FFFAF0;
          ColorMirrorHot := clNone;
          ColorMirrorHotTo := clNone;
          ColorNodeHot := $FFFDF9;
          ColorNodeHotTo := $FFFAF0;
          ColorMirrorNodeHot := clNone;
          ColorMirrorNodeHotTo := clNone;
        end;
      end;
      tsWindows7:
      begin
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $FCEBDC;

        FocusColor := clWhite;
        HotColor := $FCDBC1;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $CEA27D;
          BorderColorHot := $FBD6B8;
          BorderColorNodeHot := $FBD6B8;
          ColorDown := $FCEBDC;
          ColorDownTo := $FCDBC1;
          ColorMirrorDown := clNone;
          ColorMirrorDownTo := clNone;
          ColorHot := $FDFBFA;
          ColorHotTo := $FDF3EB;
          ColorMirrorHot := clNone;
          ColorMirrorHotTo := clNone;
          ColorNodeHot := $FDFBFA;
          ColorNodeHotTo := $FDF3EB;
          ColorMirrorNodeHot := clNone;
          ColorMirrorNodeHotTo := clNone;
        end;
      end;
      tsTerminal:
      begin
        OuterBorderColor := clNone;
        InnerBorderColor := brdr;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := clBtnFace;

        FocusColor := clWhite;
        HotColor := clHighLight;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := clGray;
          BorderColorHot := clGray;
          BorderColorNodeHot := clGray;
          ColorDown := clHighLight;
          ColorDownTo := clHighLight;
          ColorMirrorDown := clNone;
          ColorMirrorDownTo := clNone;
          ColorHot := clSilver;
          ColorHotTo := clSilver;
          ColorMirrorHot := clNone;
          ColorMirrorHotTo := clNone;
          ColorNodeHot := clSilver;
          ColorNodeHotTo := clSilver;
          ColorMirrorNodeHot := clNone;
          ColorMirrorNodeHotTo := clNone;
        end;
      end;
      tsOffice2010Blue:
      begin
        OuterBorderColor := $C7B29F;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $FDF5ED;

        FocusColor := clWhite;
        HotColor := $EAD3BF;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $308AC2;
          BorderColorHot := $34C2F0;
          BorderColorNodeHot := $34C2F0;
          ColorDown := $65C7F5;
          ColorDownTo := $8AE4FF;
          ColorMirrorDown := $8AE4FF;
          ColorMirrorDownTo := $65C7F5;
          ColorHot := $DFF7FD;
          ColorHotTo := $79DFFB;
          ColorMirrorHot := $79DFFB;
          ColorMirrorHotTo := $DFF7FD;
          ColorNodeHot := $DFF7FD;
          ColorNodeHotTo := $79DFFB;
          ColorMirrorNodeHot := $79DFFB;
          ColorMirrorNodeHotTo := $DFF7FD;
        end;
      end;
      tsOffice2010Silver:
      begin
        OuterBorderColor := $D2CDC8;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $FFFFFF;

        FocusColor := clWhite;
        HotColor := $D4CFCB;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $308AC2;
          BorderColorHot := $34C2F0;
          BorderColorNodeHot := $34C2F0;
          ColorDown := $65C7F5;
          ColorDownTo := $8AE4FF;
          ColorMirrorDown := $8AE4FF;
          ColorMirrorDownTo := $65C7F5;
          ColorHot := $DFF7FD;
          ColorHotTo := $79DFFB;
          ColorMirrorHot := $79DFFB;
          ColorMirrorHotTo := $DFF7FD;
          ColorNodeHot := $DFF7FD;
          ColorNodeHotTo := $79DFFB;
          ColorMirrorNodeHot := $79DFFB;
          ColorMirrorNodeHotTo := $DFF7FD;
        end;
      end;
      tsOffice2010Black:
      begin
        OuterBorderColor := $6D6D6D;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $C6C6C6;

        FocusColor := clWhite;
        HotColor := $656565;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $308AC2;
          BorderColorHot := $34C2F0;
          BorderColorNodeHot := $34C2F0;
          ColorDown := $65C7F5;
          ColorDownTo := $8AE4FF;
          ColorMirrorDown := $8AE4FF;
          ColorMirrorDownTo := $65C7F5;
          ColorHot := $DFF7FD;
          ColorHotTo := $79DFFB;
          ColorMirrorHot := $79DFFB;
          ColorMirrorHotTo := $DFF7FD;
          ColorNodeHot := $DFF7FD;
          ColorNodeHotTo := $79DFFB;
          ColorMirrorNodeHot := $79DFFB;
          ColorMirrorNodeHotTo := $DFF7FD;
        end;
      end;
     tsWindows8, tsWindows10:
      begin
        OuterBorderColor := $E4E3E2;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $F7F6F5;

        FocusColor := clWhite;
        HotColor := $FFFDFD;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;

          BorderColorDown := $E4A262;
          BorderColorHot := $F9CEA4;
          BorderColorNodeHot := $F9CEA4;

          ColorDown := $F7E0C9;
          ColorDownTo := $F7E0C9;
          ColorMirrorDown := clNone;
          ColorMirrorDownTo := clNone;

          ColorHot := $F7EFE8;
          ColorHotTo := $F7EFE8;
          ColorMirrorHot := clNone;
          ColorMirrorHotTo := clNone;

          ColorNodeHot := $F7EFE8;
          ColorNodeHotTo := $F7EFE8;
          ColorMirrorNodeHot := $F7EFE8;
          ColorMirrorNodeHotTo := $F7EFE8;
        end;
      end;
     tsOffice2013White:
      begin
        OuterBorderColor := $D4D4D4;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := clWhite;

        FocusColor := clWhite;
        HotColor := clWhite;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $E59D56;
          BorderColorHot := $EAB47E;
          BorderColorNodeHot := $EAB47E;
          ColorDown := $FCE2C8;
          ColorDownTo := $FCE2C8;
          ColorMirrorDown := $FCE2C8;
          ColorMirrorDownTo := $FCE2C8;
          ColorHot := $FCF0E4;
          ColorHotTo := $FCF0E4;
          ColorMirrorHot := $FCF0E4;
          ColorMirrorHotTo := $FCF0E4;
          ColorNodeHot := $FCF0E4;
          ColorNodeHotTo := $FCF0E4;
          ColorMirrorNodeHot := $FCF0E4;
          ColorMirrorNodeHotTo := $FCF0E4;
        end;
      end;
      tsOffice2013LightGray:
      begin
        OuterBorderColor := $C6C6C6;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $F6F6F6;

        FocusColor := clWhite;
        HotColor := $FAFAFA;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $E59D56;
          BorderColorHot := $EAB47E;
          BorderColorNodeHot := $EAB47E;
          ColorDown := $FCE2C8;
          ColorDownTo := $FCE2C8;
          ColorMirrorDown := $FCE2C8;
          ColorMirrorDownTo := $FCE2C8;
          ColorHot := $FCF0E4;
          ColorHotTo := $FCF0E4;
          ColorMirrorHot := $FCF0E4;
          ColorMirrorHotTo := $FCF0E4;
          ColorNodeHot := $FCF0E4;
          ColorNodeHotTo := $FCF0E4;
          ColorMirrorNodeHot := $FCF0E4;
          ColorMirrorNodeHotTo := $FCF0E4;
       end;
      end;
      tsOffice2013Gray:
      begin
        OuterBorderColor := $ABABAB;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $E5E5E5;

        FocusColor := clWhite;
        HotColor := $F3F3F3;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $E59D56;
          BorderColorHot := $EAB47E;
          BorderColorNodeHot := $EAB47E;
          ColorDown := $FCE2C8;
          ColorDownTo := $FCE2C8;
          ColorMirrorDown := $FCE2C8;
          ColorMirrorDownTo := $FCE2C8;
          ColorHot := $FCF0E4;
          ColorHotTo := $FCF0E4;
          ColorMirrorHot := $FCF0E4;
          ColorMirrorHotTo := $FCF0E4;
          ColorNodeHot := $FCF0E4;
          ColorNodeHotTo := $FCF0E4;
          ColorMirrorNodeHot := $FCF0E4;
          ColorMirrorNodeHotTo := $FCF0E4;
       end;
      end;
     tsOffice2016White:
      begin
        OuterBorderColor := $D4D4D4;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := clWhite;

        FocusColor := clWhite;
        HotColor := clWhite;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $E3BDA3;
          BorderColorHot := $F2E1D5;
          BorderColorNodeHot := $F2E1D5;
          ColorDown := $E3BDA3;
          ColorDownTo := $E3BDA3;
          ColorMirrorDown := $E3BDA3;
          ColorMirrorDownTo := $E3BDA3;
          ColorHot := $F2E1D5;
          ColorHotTo := $F2E1D5;
          ColorMirrorHot := $F2E1D5;
          ColorMirrorHotTo := $F2E1D5;
          ColorNodeHot := $F2E1D5;
          ColorNodeHotTo := $F2E1D5;
          ColorMirrorNodeHot := $F2E1D5;
          ColorMirrorNodeHotTo := $F2E1D5;
        end;
      end;
      tsOffice2016Gray:
      begin
        OuterBorderColor := $444444;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $B2B2B2;

        FocusColor := $B2B2B2;
        HotColor := $B2B2B2;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $E3BDA3;
          BorderColorHot := $F2E1D5;
          BorderColorNodeHot := $F2E1D5;
          ColorDown := $E3BDA3;
          ColorDownTo := $E3BDA3;
          ColorMirrorDown := $E3BDA3;
          ColorMirrorDownTo := $E3BDA3;
          ColorHot := $F2E1D5;
          ColorHotTo := $F2E1D5;
          ColorMirrorHot := $F2E1D5;
          ColorMirrorHotTo := $F2E1D5;
          ColorNodeHot := $F2E1D5;
          ColorNodeHotTo := $F2E1D5;
          ColorMirrorNodeHot := $F2E1D5;
          ColorMirrorNodeHotTo := $F2E1D5;
       end;
      end;
      tsOffice2016Black:
      begin
        OuterBorderColor := $4E4E4E;
        InnerBorderColor := clNone;
        FocusouterBorderColor := clNone;
        FocusInnerBorderColor := brdr;
        InnerMostBorderColor := clNone;
        Color := $444444;

        FocusColor := $444444;
        HotColor := $444444;
        with ButtonAppearance do
        begin
          ArrowColorDown := clBlack;
          ArrowColorHot := clBlack;
          BorderColorDown := $444444;
          BorderColorHot := $6A6A6A;
          BorderColorNodeHot := $6A6A6A;
          ColorDown := $444444;
          ColorDownTo := $444444;
          ColorMirrorDown := $444444;
          ColorMirrorDownTo := $444444;
          ColorHot := $6A6A6A;
          ColorHotTo := $6A6A6A;
          ColorMirrorHot := $6A6A6A;
          ColorMirrorHotTo := $6A6A6A;
          ColorNodeHot := $6A6A6A;
          ColorNodeHotTo := $6A6A6A;
          ColorMirrorNodeHot := $6A6A6A;
          ColorMirrorNodeHotTo := $6A6A6A;
       end;


      end;


      tsCustom: ;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAutoCompFormDeactivate(Sender: TObject);
begin
  //if not Self.Focused then
    HideAutoCompList;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAutoCompListBoxClick(Sender: TObject);
begin
  if Assigned(FAutoComListBox) and (FAutoComListBox.ItemIndex >= 0) then
  begin
    SelectedNode := TAdvTreeNode(FAutoComListBox.Items.Objects[FAutoComListBox.ItemIndex]);
    SelectAll;
  end;

  HideAutoCompList;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAutoCompListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FAutoComListBox) then
  begin
    if (Key = VK_DELETE) then
    begin
      if SelLength > 0 then
      begin
        inherited Text := AnsiUpperCase(Copy(inherited Text,1,SelStart));
      end
      else
        inherited Text := '';
    end
    else if Assigned(FAutoComListBox) then
    begin
      if (Key = VK_UP) then
        FAutoComListBox.MoveSelect(-1);

      if (Key = VK_DOWN) then
        FAutoComListBox.MoveSelect(1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAutoCompListBoxKeyPress(Sender: TObject;
  var Key: Char);

  procedure SetMyText(Value: String);
  begin
    if EditorEnabled and (EditStyle = esEdit) then
      SetDirectText(Value)
    else
      inherited Text := Value;  
  end;

var
  s: string;
begin
  if not Assigned(FAutoComListBox) then
    Exit;

  if Key = #27 then
  begin
    HideAutoCompList;
    Self.SetFocus;
    SelectAll;
    Exit;
  end
  else if (Key = #8) then
  begin
    s := inherited Text;
    if Length(s) > 0 then
    begin
      system.Delete(s, Length(s), 1);
      SetMyText(s);
      ShowAutoCompList;
    end;
    Exit;
  end
  else if (Key = #46) then  // delete
  begin
    if SelLength > 0 then
    begin
      SetMyText(AnsiUpperCase(Copy(inherited Text,1,SelStart)));
    end
    else
      SetMyText('');
    Exit;
  end
  else if (Key = #13 {VK_ENTER}) then
  begin
    if (FAutoComListBox.ItemIndex >= 0) then
    begin
      if (SelectedNode = TAdvTreeNode(FAutoComListBox.Items.Objects[FAutoComListBox.ItemIndex])) then
        SetDirectText(GetHierarchicalNodeText(SelectedNode))
      else
        SelectedNode := TAdvTreeNode(FAutoComListBox.Items.Objects[FAutoComListBox.ItemIndex]);
    end;
    HideAutoCompList;
    Self.SetFocus;
    SelectAll;
    Exit;
  end;

  SetMyText(inherited Text + Key);

  ShowAutoCompList;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAutoCompListBoxSelect(Sender: TObject);
begin
  {if Assigned(FAutoComListBox) and (FAutoComListBox.ItemIndex >= 0) then
  begin
    SelectedNode := TAdvTreeNode(FAutoComListBox.Items.Objects[FAutoComListBox.ItemIndex]);
    SelectAll;
  end;

  HideAutoCompList;}
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetAutoComplete(const Value: Boolean);
begin
  if (FAutoComplete <> Value) then
  begin
    FAutoComplete := Value;

    if FAutoComplete and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      PopulateAutoCompleteList;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.AddWithChildrenToAutoCompleteList(Node: TAdvTreeNode);
var
  N: TAdvTreeNode;
begin
  if Assigned(Node) then
  begin
    FAutoCompleteList.AddObject(GetHierarchicalNodeText(Node), Node);
    N := Node.getFirstChild;
    while (N <> nil) do
    begin
      //FAutoCompleteList.AddObject(GetHierarchicalNodeText(N), N);
      AddWithChildrenToAutoCompleteList(N);
      N := N.getNextSibling;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.PopulateAutoCompleteList;
var
  N: TAdvTreeNode;
begin
  if AutoComplete then
  begin
    FAutoCompleteList.Clear;

    N := Items.GetFirstNode;
    if Assigned(N) then
    begin
      while (N <> nil) do
      begin
        AddWithChildrenToAutoCompleteList(N);
        N := N.getNextSibling;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.PopulateAutoCompListBox;
var
  i: Integer;
  s, CurrentSearch: string;
  AddAll: Boolean;
begin
  if Assigned(FAutoComListBox) then
  begin
    FAutoComListBox.Items.Clear;
    AddAll := True;
    CurrentSearch := UpperCase(inherited Text);
    if Length(CurrentSearch) > 0 then
    begin
      if CurrentSearch[Length(CurrentSearch)] = '\' then
        system.Delete(CurrentSearch, Length(CurrentSearch), 1);
      AddAll := False;
    end;
    
    for i := 0 to FAutoCompleteList.Count - 1 do
    begin
      s := FAutoCompleteList[i];
      if AddAll or (AnsiPos(CurrentSearch, UpperCase(s)) = 1) then
      begin
        FAutoComListBox.Items.AddObject(FAutoCompleteList[i], FAutoCompleteList.Objects[i]);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.HideAutoCompList;
begin
  if Assigned(FAutoComForm) then
    PostMessage(FAutoComForm.Handle, WM_CLOSE,0,0);
  FAutoComForm := nil;
  FAutoComListBox := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.ShowAutoCompList;
var
  P: TPoint;
  h: Integer;
  {$IFDEF DELPHI9_LVL}
  w: Integer;
  {$ENDIF}
begin
  if not AutoComplete then
    Exit;

  if not Assigned(FAutoComForm) then
  begin
    FAutoComForm := TDropForm.CreateNew(self,0);
    FAutoComForm.Visible := False;
    FAutoComForm.Sizeable := False;
    FAutoComForm.BorderStyle := bsNone;
    FAutoComForm.FormStyle := fsStayOnTop;
    FAutoComForm.Width := GetNodeButtonsRect.Right;
    FAutoComForm.Height := FMaxDropHeight;
    FAutoComForm.OnDeactivate := OnAutoCompFormDeactivate;
    FAutoComForm.OnKeyPress := OnAutoCompListBoxKeyPress;
    FAutoComForm.OnKeyDown := OnAutoCompListBoxKeyDown;
  end;

  if not Assigned(FAutoComListBox) then
  begin
    FAutoComListBox := TExplorerTreeviewListBox.Create(FAutoComForm);
    FAutoComListBox.Parent := FAutoComForm;
    FAutoComListBox.ExplorerTreeview := Self;
    FAutoComListBox.Ctl3D := False;
    FAutoComListBox.Align := alClient;
    FAutoComListBox.Visible := True;
    FAutoComListBox.OnClick := OnAutoCompListBoxClick;
    FAutoComListBox.OnKeyPress := OnAutoCompListBoxKeyPress;
    FAutoComListBox.OnKeyDown := OnAutoCompListBoxKeyDown;
    FAutoComListBox.ShowImages := False;
  end;
  FAutoComListBox.OnSelect := nil;
  PopulateAutoCompListBox;

  if (FAutoComListBox.Count <= 0) then
  begin
    HideAutoCompList;
    Exit;
  end
  else
  begin
    FAutoComListBox.ItemIndex := 0;
  end;
  FAutoComListBox.OnSelect := OnAutoCompListBoxSelect;

  h := FAutoComListBox.Count * FAutoComListBox.ItemHeight + 2;
  FAutoComForm.Height := Min(FMaxDropHeight, h);

  {$IFDEF DELPHI9_LVL}
  w := FAutoComForm.Width;
  {$ENDIF}
  h := FAutoComForm.Height;

  P := Point(GetNodeButtonsRect.Left - 4, 0);
  P := ClientToScreen(P);
  FAutoComForm.Left := P.x;

  if P.y + FAutoComForm.Height >= GetSystemMetrics(SM_CYSCREEN) then
    FAutoComForm.Top := P.y - h
  else
    FAutoComForm.Top := P.y + self.Height;

  {$IFDEF DELPHI9_LVL}
  FAutoComForm.Width := 0;
  FAutoComForm.Height := 0;
  {$ENDIF}

  FAutoComForm.Show;

  {$IFDEF DELPHI9_LVL}
  FAutoComForm.Left := p.x;
  if P.y + h >= GetSystemMetrics(SM_CYSCREEN) then
    FAutoComForm.Top := P.y - h
  else
    FAutoComForm.Top := P.y + self.Height - 4;

  FAutoComForm.Width := w;
  FAutoComForm.Height := h;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.PopulateListBox;
var
  i: Integer;
begin
  if Assigned(FListBox) then
  begin
    FListBox.Items.Clear;
    for i := 0 to FDropDownList.Count - 1 do
    begin
      FListBox.Items.AddObject(GetHierarchicalNodeText(FDropDownList[i], False), FDropDownList[i]);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.ShowDropDownList;
var
  P: TPoint;
  h: Integer;
  {$IFDEF DELPHI9_LVL}
  w: Integer;
  {$ENDIF}
begin
  if Assigned(OnBeforeDropDown) then
    OnBeforeDropDown(Self);
    
  FDropForm := TDropForm.CreateNew(self,0);
  FDropForm.Visible := False;
  FDropForm.Sizeable := False;
  FDropForm.BorderStyle := bsNone;
  FDropForm.FormStyle := fsStayOnTop;
  FDropForm.Width := GetDropDownButtonRect.Right;
  FDropForm.Height := FMaxDropHeight; 
  FDropForm.OnDeactivate := OnDropFormDeactivate;
  FDropForm.OnKeyDown := OnListBoxKeyDown;
  FDropForm.OnKeyPress := OnListBoxKeyKeyPress;

  FListBox := TExplorerTreeviewListBox.Create(FDropForm);
  FListBox.Parent := FDropForm;
  FListBox.ExplorerTreeview := Self;
  FListBox.Ctl3D := False;
  FListBox.Align := alClient;
  FListBox.Font.Assign(Self.Font);
  FListBox.Canvas.Font.Assign(FListBox.Font);
  PopulateListBox;
  if Assigned(CurrentImages) then
    FListBox.ItemHeight := CurrentImages.Height;
  if (FListBox.Canvas.TextHeight('Wy') > FListBox.ItemHeight - 1) then
    FListBox.ItemHeight := FListBox.Canvas.TextHeight('Wy');
  FListBox.Visible := True;
  FListBox.OnClick := OnListBoxClick;
  FListBox.OnKeyDown := OnListBoxKeyDown;
  FListBox.OnKeyPress := OnListBoxKeyKeyPress;
  if (FListBox.Count <= 0) then
  begin
    HideDropDownList;
    Exit;
  end
  else
  begin
    h := FListBox.Items.IndexOf(GetHierarchicalNodeText(SelectedNode));
    h := Max(0, h);
    FListBox.ItemIndex := h;
  end;
  FListBox.OnSelect := OnListBoxSelect;

  h := FListBox.Items.Count * FListBox.ItemHeight + 2;
  FDropForm.Height := Min(FMaxDropHeight, h);

  h := FDropForm.Height;

  P := Point(0, 0);
  P := ClientToScreen(P);
  FDropForm.Left := P.x;

  if P.y + FDropForm.Height >= GetSystemMetrics(SM_CYSCREEN) then
    FDropForm.Top := P.y - h
  else
    FDropForm.Top := P.y + self.Height;

  {$IFDEF DELPHI9_LVL}
  w := FDropForm.Width;
  FDropForm.Width := 0;
  FDropForm.Height := 0;
  {$ENDIF}

  FDropForm.Show;

  {$IFDEF DELPHI9_LVL}
  FDropForm.Left := p.x;
  if P.y + h >= GetSystemMetrics(SM_CYSCREEN) then
    FDropForm.Top := P.y - h
  else
    FDropForm.Top := P.y + self.Height;

  FDropForm.Width := w;
  FDropForm.Height := h;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.HideDropDownList;
begin
  if Assigned(FDropForm) then
  begin
    PostMessage(FDropForm.Handle, WM_CLOSE,0,0);
    FDropDownButton.Down := False;
  end;
  FDropForm := nil;
  FListBox := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMChar(var Msg: TWMChar);
begin
  if Msg.CharCode = Ord(#13) then
    Msg.Result :=0
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMCut(var Message: TWMCut);
var
  ch: Char;
begin
  inherited;

  ch := #0;
  Keypress(ch);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  //Message.Result := {DLGC_WANTTAB or }DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  if (msg.CharCode in [VK_DOWN, VK_F4]) then
  begin
    if not Assigned(FDropForm) and not FCloseClick then
      ShowDropDownList;
    FCloseClick := False;
  end;  

  if (msg.CharCode in [VK_DOWN, VK_F4]) or ((msg.CharCode in [Ord('A')..Ord('Z'), Ord('1')..Ord('9')])) then
  begin
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if EditorEnabled and (Appearance.FocusColor <> clNone) and not (csDesigning in ComponentState) then
  begin
    inherited Color := Appearance.Color;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMPaste(var Message: TWMPaste);
var
  ch: Char;
begin
  inherited;

  ch := #0;
  Keypress(ch);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if EditorEnabled and (Appearance.FocusColor <> clNone) and not (csDesigning in ComponentState) then
  begin
    EditMode := True;
    inherited Color := Appearance.FocusColor;
    SendMessage(Handle, EM_SETSEL, Length(inherited Text), Length(inherited Text));
  end;
  if not EditorEnabled and HandleAllocated and not (csDesigning in ComponentState) then
    HideCaret(Handle);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMSize(var Message: TWMSize);
var
  N: TAdvTreeNode;
  {MinHeight: Integer;
  Dist:integer;}
begin
  inherited;

  SetEditRect;
  UpdateButtonsPosition;

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    IsInternal := true;
    N := SelectedNode;
    SelectedNode := nil;
    SelectedNode := N;
    IsInternal := false;
  end;

  (*if BorderStyle = bsNone then
    Dist := 2
  else
    Dist := 4;

  MinHeight := GetMinHeight;
  { Windows text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }

  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    {if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - 21 + 0, 0, 17, Height - Dist)
    else
      FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;}
  end;
  *)
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMSysKeyDown(var Msg: TWMKeydown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnTimerTime(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WndProc(var Message: TMessage);
begin
  inherited;

  if (Message.Msg = WM_ET_HIDEDROPDOWN) then
    HideDropDownList;

  if (Message.Msg = WM_ET_SETFOLDERPATH) then
    SetFolderPath(FNewSelectedFolderPath);

  if (Message.Msg = WM_ET_TRACKKEYPRESS) then
  begin
    if FUpdateInternalText then    
    begin
      FText := inherited Text;
      FUpdateInternalText := False;
    end;

    if FShowAutoCL then       
    begin
      ShowAutoCompList;
      FShowAutoCL := False;
    end;
  end;   
end;

procedure TCustomExplorerTreeview.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetEditRect;
var
  Loc: TRect;
  h: Integer;
  DC: HDC;
  Canvas: TCanvas;
begin
  if not HandleAllocated then
    Exit;

  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  //Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := GetDropDownButtonRect.Left - 2; //ClientWidth - DROPDOWNBTN_WIDTH;

  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    Canvas.Font.Assign(Font);
    h := Canvas.TextHeight('Wg') + 2;
    Canvas.Free;
  finally
    ReleaseDC(Handle,DC);
  end;

  Loc.Top := 3 + (GetNodeButtonsRect.Bottom - GetNodeButtonsRect.Top - h) div 2;
  Loc.Bottom := Loc.Top + h;
  //Loc.Top := 5;
  Loc.Left := 3;
  
  if ShowImage then
    Loc.Left := Loc.Left + GetIconSize.cx;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetMaxDropHeight(const Value: Integer);
begin
  if (Value > 50) then
    FMaxDropHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetMode(
  const Value: TAdvExplorerTreeviewMode);
begin
  if (FMode <> Value) then
  begin
    FMode := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetShowImage(const Value: Boolean);
begin
  if (FShowImage <> Value) then
  begin
    FShowImage := Value;
    SetEditRect;
    UpdateLeftIconPos;
    UpdateNodeButtonsPositions;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetTextEx(const Value: string);
var
  s: string;
begin
  if EditorEnabled and (EditStyle = esEdit) and not (csDesigning in ComponentState) then
  begin
    if Assigned(SelectedNode) then
    begin
      s := GetHierarchicalNodeText(SelectedNode, False);
      if Assigned(SelectedNode) and (Mode = aeFolder) and (SelectedNode = Items.GetFirstNode) then
        s := '';
      if (AnsiUpperCase(Value) = AnsiUpperCase(s)) then
      begin
        // do nothing
      end
      else
      begin
        SetDirectText(Value);
        RemoveNodeButtons;
        if Assigned(FSelectedNode) then
          FOldSelected := FSelectedNode;
        FSelectedNode := nil;
      end;
    end
    else
    begin
      SetDirectText(Value);
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetDirectText(Value: string);
begin
  FText := Value;
  inherited Text := FText;
end;

procedure TCustomExplorerTreeview.SetDropDownButton(const Value: Boolean);
begin
  if (FDropDownButton.Visible <> value) then
  begin
    FDropDownButton.Visible := value;

    if Value then
    begin
      FDropDownButton.Width := DROPDOWNBTN_WIDTH;
      FDropDownButton.Left := FDropDownButton.Left - DROPDOWNBTN_WIDTH;
    end
    else
    begin
      FDropDownButton.Left := FDropDownButton.Left + FDropDownButton.Width;
      FDropDownButton.Width := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetTextEx: string;
begin
  if EditMode then
    Result := inherited Text
  else
    Result := FText;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SeTAdvTreeNodes(const Value: TAdvTreeNodes);
begin
  Items.Assign(Value);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.CreateNode: TAdvTreeNode;
var
  LClass: TAdvTreeNodeClass;
begin
  LClass := TAdvTreeNode;
  Result := LClass.Create(Items);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.CreateNodes: TAdvTreeNodes;
begin
  Result := TAdvTreeNodes.Create(Self);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetHierarchicalNodeText(N: TAdvTreeNode; IncludeFolderPath: Boolean = True): string;
begin
  Result := '';
  if Assigned(N) then
  begin
    Result := N.Text;
    if N.VirtualParent then
      Exit;
      
    N := N.Parent;
    if Assigned(N) then
    begin
      while (N <> nil) and not N.VirtualParent do
      begin
        if (Length(N.Text) > 0) and (N.Text[Length(N.Text)] = NODE_SEP) then
          Result := N.Text + Result
        else
          Result := N.Text + NODE_SEP + Result;
        N := N.Parent;
      end;
    end
    else
      Result := Result + NODE_SEP;

    if IncludeFolderPath and (Mode = aeFolder) then
    begin
      if (Length(FolderPath) > 0) and (FolderPath[Length(FolderPath)] <> NODE_SEP) then
        Result := NODE_SEP + Result;
      Result := FolderPath + Result;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateButtonsPosition;
begin
  UpdateLeftIconPos;
  UpdateDropDownRefreshBtnsPos;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateDropDownRefreshBtnsPos;
var
  R: TRect;
begin
  if Assigned(FDropDownButton) then
  begin
    R := GetDropDownButtonRect;
    FDropDownButton.Width := R.Right - R.Left;
    FDropDownButton.Height := R.Bottom - R.Top;
    FDropDownButton.Left := R.Left;
    FDropDownButton.Top := R.Top;
  end;

  if Assigned(FRefreshBtn) then
  begin
    R := GetRefreshButtonRect;
    FRefreshBtn.Width := R.Right - R.Left;
    FRefreshBtn.Height := R.Bottom - R.Top;
    FRefreshBtn.Left := R.Left;
    FRefreshBtn.Top := R.Top;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetLeftIconRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ShowImage then
    Result := Rect(2, 2, 2 + GetIconSize.cx, Self.Height - 2);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.UpdateLeftIconPos;
var
  R: TRect;
begin
  if Assigned(FLeftIcon) then
  begin
    R := GetLeftIconRect;
    FLeftIcon.Width := R.Right - R.Left;
    FLeftIcon.Height := R.Bottom - R.Top;
    FLeftIcon.Top := R.Top;
    FLeftIcon.Left := R.Left;
    FLeftIcon.Visible := ShowImage;    
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetNodeButtonsRect: TRect;
begin
  Result := GetDropDownButtonRect;
  Result.Right := Result.Left - 2;
  if ShowImage then
    Result.Left := GetLeftIconRect.Right + 1
  else
    Result.Left := 2;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetDropDownButton: Boolean;
begin
  Result := FDropDownButton.Visible;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetDropDownButtonRect: TRect;
begin
  if RefreshButton then
  begin
    Result := GetRefreshButtonRect;
    Result.Right := Result.Left;
    Result.Left := Result.Right - DROPDOWNBTN_WIDTH;
  end
  else
    Result := Rect(Width - DROPDOWNBTN_WIDTH - 2, GetBorderWidth, Width - 2, Height - GetBorderWidth);

  if not DropDownButton then
    Result.Left := Result.Right;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetRefreshButtonRect: TRect;
var
  i: Integer;
begin
  i := GetBorderWidth;
  if RefreshButton then
    Result := Rect(Width - REFRESHBTN_WIDTH - 2, i, Width - 2, Height - i)
  else
    Result := Rect(-1, -1, -1, -1);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetBorderWidth: Integer;
var
  BrIn, BrOut: TColor;
begin
  Result := 0;
  if Focused then
  begin
    BrIn := Appearance.FocusInnerBorderColor;
    BrOut := Appearance.FocusOuterBorderColor;
  end
  else
  begin
    BrIn := Appearance.InnerBorderColor;
    BrOut := Appearance.OuterBorderColor;
  end;

  if (BrOut <> clNone) then
    Inc(Result);
  if (BrIn <> clNone) then
    Inc(Result);
  {if (Appearance.InnerMostBorderColor <> clNone) then
    Inc(Result);}

  if (Result <= 0) then
    Result := 1;
  if (Result > 2) then
    Result := 2;  
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMNCPaint(var Message: TMessage);
begin
  inherited;
  //NCPaintProc;
  //Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.WMPaint(var Message: TWMPaint);
begin
  inherited;
  {$IFDEF DELPHI_UNICODE}
  FBufferedDraw := (csGlassPaint in ControlState) or FBufferedDraw;
  {$ENDIF}
  DrawBackGround;
end;

//------------------------------------------------------------------------------

{$IFDEF TMS_GDIP}
procedure DrawGDIPRect(g: TGPGraphics; Canvas: TCanvas; Clr: TColor; R: TRect);
var
  path:TGPGraphicsPath;
  X,Y,Width,Height: integer;
  graphics: TGPGraphics;
  pen: TGPPen;
begin
  if (not Assigned(g) and not Assigned(Canvas)) then
    Exit;

  graphics := g;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  X := R.Left;
  Y := R.Top;
  width := R.Right - R.Left;
  height := R.Bottom - R.Top;

  pen := TGPPen.Create(ColorToARGB(Clr),1);
  path := TGPGraphicsPath.Create;
  path.AddLine(X, Y, X + width, Y);
  path.AddLine(X + width, Y, X + width, Y + height);
  path.AddLine(X + width, Y + height, X, Y + height);
  path.AddLine(X, Y + height, X, Y);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
  pen.Free;
  if not Assigned(g) then
    graphics.Free;
end;

procedure DrawGDIPLine(g: TGPGraphics; Canvas: TCanvas; Clr: TColor; X1, Y1, X2, Y2: integer);
var
  path:TGPGraphicsPath;
  graphics: TGPGraphics;
  pen: TGPPen;
begin
  if (not Assigned(g) and not Assigned(Canvas)) then
    Exit;

  graphics := g;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;
  pen := TGPPen.Create(ColorToARGB(Clr),1);
  path := TGPGraphicsPath.Create;
  path.AddLine(X1, Y1, X2, Y2);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
  pen.Free;
  if not Assigned(g) then
    graphics.Free;
end;

{$ENDIF}

procedure TCustomExplorerTreeview.DrawBackGround;
var
  DC: HDC;
  Canvas: TCanvas;
  BrIn, BrOut: TColor;
  R: TRect;
  i, j: Integer;
begin
  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    if Focused then
    begin
      BrIn := Appearance.FocusInnerBorderColor;
      BrOut := Appearance.FocusOuterBorderColor;
    end
    else
    begin
      BrIn := Appearance.InnerBorderColor;
      BrOut := Appearance.OuterBorderColor;
    end;

    R := ClientRect;
    Canvas.Brush.Style := bsClear;
    if (BrOut <> clNone) then
    begin
    {$IFDEF TMS_GDIP}
      if FBufferedDraw then
        DrawGDIPRect(nil, Canvas, BrOut, R)
      else
    {$ENDIF}
      begin
        Canvas.Pen.Color := BrOut;
        Canvas.Rectangle(R);
      end;
      {
      Canvas.Pixels[R.Left, R.Top] := Color;
      Canvas.Pixels[R.Right-1, R.Top] := Color;
      Canvas.Pixels[R.Left, R.Bottom-1] := Color;
      Canvas.Pixels[R.Right-1, R.Bottom-1] := Color;
      }
      InflateRect(R, -1, -1);
    end;
    if (BrIn <> clNone) then
    begin
      {$IFDEF TMS_GDIP}
      if FBufferedDraw then
        DrawGDIPRect(nil, Canvas, BrIn, Rect(R.left, R.Top, R.right - 1, R.Bottom - 1))
      else
      {$ENDIF}
      begin
        Canvas.Pen.Color := BrIn;
        Canvas.Rectangle(R);
      end;
      {
      Canvas.Pixels[R.Left, R.Top] := Color;
      Canvas.Pixels[R.Right-1, R.Top] := Color;
      Canvas.Pixels[R.Left, R.Bottom-1] := Color;
      Canvas.Pixels[R.Right-1, R.Bottom-1] := Color;
      }
      InflateRect(R, -1, -1);
    end;

    if not FMouseInControl and not IsFocused and (Appearance.InnerMostBorderColor <> clNone) then
    begin
      {$IFDEF TMS_GDIP}
      if FBufferedDraw then
        DrawGDIPRect(nil, Canvas, Appearance.InnerMostBorderColor, Rect(R.left, R.Top, R.right - 1, R.Bottom - 1))
      else
      {$ENDIF}
      begin
        Canvas.Pen.Color := Appearance.InnerMostBorderColor;
        Canvas.Rectangle(R);
      end;

      Canvas.Pixels[R.Left, R.Top] := Color;
      Canvas.Pixels[R.Right-1, R.Top] := Color;
      Canvas.Pixels[R.Left, R.Bottom-1] := Color;
      Canvas.Pixels[R.Right-1, R.Bottom-1] := Color;
    end;

    if FRefreshButton then
    begin
      R.Left := GetRefreshButtonRect.Left;
      if (BrIn = clNone) then
      begin
        if (BrOut <> clNone) then
          BrIn := BrOut
        else if (Appearance.InnerMostBorderColor <> clNone) then
          BrIn := Appearance.InnerMostBorderColor
        else
          BrIn := clBlack;
      end;
      {$IFDEF TMS_GDIP}
      if FBufferedDraw then
        DrawGDIPLine(nil, Canvas, BrIn, R.left, R.Top, R.Left, R.Bottom)
      else
      {$ENDIF}
      begin
        Canvas.Pen.Color := BrIn;
        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Left, R.Bottom);
      end;
    end;

    Canvas.Free;
  finally
    ReleaseDC(Handle,DC);
  end;

  // repaint Active button
  j := -1;
  for i:= 0 to FNodeButtons.Count - 1 do
  begin
    if TNodeButton(FNodeButtons[i]).IsActive then
    begin
      j := i;
      Break;
    end;
  end;
  if (j >= 0) then
    TNodeButton(FNodeButtons[j]).DrawButton;

  if FDropDownButton.IsActive then
    FDropDownButton.DrawButton;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetRefreshButton(const Value: Boolean);
begin
  if (FRefreshButton <> Value) then
  begin
    FRefreshButton := Value;
    //RecreateWnd;
    if FRefreshButton then
    begin
      if not Assigned(FRefreshBtn) then
      begin
        FRefreshBtn := TDropDownButton.Create(Self);
        FRefreshBtn.Parent := Self;
        FRefreshBtn.OnClick := OnRefreshBtnClick;
        FRefreshBtn.Visible := True;
        FRefreshBtn.Glyph.Assign(FRefreshGlyph);
        FRefreshBtn.ImageIndex := RefreshImageIndex;
      end;
    end
    else if Assigned(FRefreshBtn) then
    begin
      FreeAndNil(FRefreshBtn);
    end;
    
    UpdateDropDownRefreshBtnsPos;
    if not (csDesigning in ComponentState) then
      SetEditRect;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetRefreshGlyph(const Value: TBitmap);
begin
  FRefreshGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetRefreshImageIndex(const Value: Integer);
begin
  if (FRefreshImageIndex <> Value) then
  begin
    FRefreshImageIndex := Value;
    if Assigned(FRefreshBtn) then
      FRefreshBtn.ImageIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetAppearance(
  const Value: TExpTreeviewAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnAppearanceChanged(Sender: TObject);
var
  P: TPoint;
  R: TRect;
begin
  if (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    Exit;

  if (csDesigning in ComponentState) then
    Color := Appearance.Color
  else
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    R := ClientRect;
    if PtInRect(R, p) then
    begin
      if not Focused or not EditorEnabled then
        Color := Appearance.HotColor;
    end
    else
    begin
      if not Focused or not EditorEnabled then
        Color := Appearance.Color;
    end;
  end;

  UpdateNodeButtonsPositions;
  UpdateDropDownRefreshBtnsPos;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not EditMode and EditorEnabled then
    EditMode := True;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.InvalidateDropDownButton;
var
  R: TRect;
begin
  R := GetDropDownButtonRect;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.IsFocused: Boolean;
begin
  Result := EditorEnabled and (Focused or (Assigned(FDropForm) and (FDropForm.Focused)) or (Assigned(FListBox) and (FListBox.Focused))
            or (Assigned(FAutoComForm) and (FAutoComForm.Focused)) or (Assigned(FAutoComListBox) and (FAutoComListBox.Focused)));
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.IsMyNode(Node: TAdvTreeNode): Boolean;
begin
  Result := False;
  if Assigned(Node) and Assigned(Node.Owner) and Assigned(Node.Owner.Owner) then  
    Result := Node.Owner.Owner = self;
end;

//------------------------------------------------------------------------------

function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList;
  ForParsing: Boolean): string;
var
  StrRet: TStrRet;
  Flags: Integer;
  wstr: string;
begin
  Result := '';
  if ForParsing then
    Flags := SHGDN_FORPARSING
  else
    Flags := SHGDN_NORMAL;

  ShellFolder.GetDisplayNameOf(PIDL, Flags, StrRet);

  wstr := string(StrRet.cStr);

  case StrRet.uType of
  {$IFNDEF DELPHI_UNICODE}
    STRRET_CSTR:
      SetString(Result, PAnsiChar(wStr), Length(wstr));
  {$ENDIF}
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;

//------------------------------------------------------------------------------

function RemoveTailSep(path: string): string;
begin
  Result := path;
  if (Length(path) > 0) and (path[Length(path)] = NODE_SEP) then
    Result := Copy(Path,1, Length(path) - 1);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.LoadDirectoryStructure(BaseNode: TBaseNode; path: string; ClearOldNodes: Boolean);
  
  procedure AddChildren(var ParentFolder: IShellFolder; var PNode: TAdvTreeNode);
  var
    ChildPidl: PITEMIDLIST;
    EnumIDList: IEnumIDList;
    Flags : UINT;
    Dummy: ULong;
    s: string;
    CNode: TAdvTreeNode;
    //ChildFolder: IShellFolder;
    FileInfo : SHFILEINFO;
    //FileData: TWin32FindData;
    Icon: TIcon;
    c: Cardinal;
    IsDrive: Boolean;
  begin
    if Assigned(ParentFolder) and Assigned(PNode) then
    begin
      Icon := TIcon.Create;
      Flags := SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN or SHCONTF_FOLDERS;
      OleCheck(ParentFolder.EnumObjects(application.Handle, Flags, EnumIDList));
      while (EnumIdList.Next(1, ChildPidl, Dummy) = NOERROR) do
      begin
        IsDrive := False;
        Dummy := SFGAO_FOLDER;
        Parentfolder.GetAttributesOf(1, ChildPidl, Dummy);
        if ((Dummy and SFGAO_FOLDER) <> SFGAO_FOLDER) {or ((Dummy and SFGAO_BROWSABLE) <> SFGAO_BROWSABLE)} then
          Continue;

        //s := GetDisplayName(Parentfolder, ChildPidl, True);
        //if not DirectoryExists(s) then
          //Continue;

        s := GetDisplayName(Parentfolder, ChildPidl, True);
        if (Length(s) > 0) and (s[Length(s)] = '\') then
        begin
          // Add Node's extra caption here for Drives ie: Local Drive (C:)

          SHGetFileInfo(PChar(s), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON);
          Icon.Handle := FileInfo.hIcon;
          IsDrive := True;
        end
        else
          s := '';

        if (s = '') then
          s := GetDisplayName(Parentfolder, ChildPidl, false);

        CNode := nil;
        if (Trim(s) <> '') then
          CNode := PNode.AddChild(s);

        {FillChar(FileData, SizeOf(FileData), #0);
        OleCheck(SHGetFileInfo(LPCSTR(ChildPidl), 0,FileInfo, SizeOf(FileInfo),SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_EXETYPE));
        }

        if Assigned(CNode) then
        begin
          if (IsDrive) then
          begin
            if not Icon.Empty and (Icon.Width = FInternalImages.Width) then
              CNode.ImageIndex := FInternalImages.AddIcon(Icon)
            else
              CNode.ImageIndex := -1;
          end
          else
          begin
            {$IFDEF DELPHI_UNICODE}
            c := SHGetFileInfo(LPWSTR(ChildPidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
            {$ENDIF}
            {$IFNDEF DELPHI_UNICODE}
            c := SHGetFileInfo(LPCSTR(ChildPidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
            {$ENDIF}
            if (c > 0) then
            begin
              Icon.Handle := FileInfo.hIcon;
              if not Icon.Empty and (Icon.Width = FInternalImages.Width) then
                CNode.ImageIndex := FInternalImages.AddIcon(Icon)
              else
                CNode.ImageIndex := -1;
            end;
          end;
        end;

        (*
        //OutputDebugString(PChar('--------------: '+ CNode.Text));

        {Dummy := SFGAO_FILESYSANCESTOR;
        Parentfolder.GetAttributesOf(1, ChildPidl, Dummy);
        if ((Dummy and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR) then
          Continue;}

        //-- excluding Removable storage
        Dummy := SFGAO_REMOVABLE;
        Parentfolder.GetAttributesOf(1, ChildPidl, Dummy);
        if (Dummy = SFGAO_REMOVABLE) then
          Continue;
        //---
        OleCheck(ParentFolder.BindToObject(ChildPidl, Nil,IID_IShellFolder,Pointer(ChildFolder)));
        AddChildren(ChildFolder, CNode);
        *)
        GlobalFreePtr(ChildPidl);
      end;

      Icon.Free;
    end;
  end;
  
var
  DeskTopFolder, MyComputerFolder: IShellFolder;
  FileInfo: SHFILEINFO;
  Node: TAdvTreeNode;
  Pidl: PITEMIDLIST;
  Icon: TIcon;
  s, fn: string;
  i: integer;
  SearchRec: TSearchRec;
  EnumIDList: IEnumIDList;
  Flags : UINT;
  ChildPidl: PITEMIDLIST;
  Dummy: ULong;
begin
  if ClearOldNodes then
    Clear;

  if not Assigned(FInternalImages) then
  begin
    FInternalImages := TImageList.Create(Self);
    {$IFDEF DELPHI_UNICODE}
    FInternalImages.Handle := SHGetFileInfo(LPWSTR(''), 0, FileInfo, SizeOf(FileInfo),SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    FInternalImages.Handle := SHGetFileInfo(LPCSTR(''), 0, FileInfo, SizeOf(FileInfo),SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    FInternalImages.DrawingStyle := dsTransparent;
    FInternalImages.ShareImages := True;
  end;

  if (BaseNode = bnDesktop) then
  begin
    Icon := TIcon.Create;
    // Desktop
    OleCheck(SHGetDesktopFolder(DeskTopFolder));
    OleCheck(SHGetSpecialFolderLocation(application.Handle, CSIDL_DESKTOP,pidl));
    {$IFDEF DELPHI_UNICODE}
    OleCheck(SHGetFileInfo(LPWSTR(Pidl), 0,FileInfo, SizeOf(FileInfo),SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
             SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_EXETYPE));
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    OleCheck(SHGetFileInfo(LPCSTR(Pidl), 0,FileInfo, SizeOf(FileInfo),SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
             SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_EXETYPE));
    {$ENDIF}

    Node := Items.AddFirst(FileInfo.szDisplayName);
    Node.ShowText := False;
    Node.VirtualParent := True;
    // Add Icon
    {$IFDEF DELPHI_UNICODE}
    SHGetFileInfo(LPWSTR(Pidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    SHGetFileInfo(LPCSTR(Pidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    Icon.Handle := FileInfo.hIcon;
    Node.ImageIndex := FInternalImages.AddIcon(Icon);

    //AddChildren(DesktopFolder, FirstNode);
    // My Computer
    OleCheck(SHGetSpecialFolderLocation(application.Handle, CSIDL_DRIVES,pidl));
    OleCheck(DeskTopFolder.BindToObject(pidl, Nil,IID_IShellFolder,Pointer(MyComputerFolder)));

    {$IFDEF DELPHI_UNICODE}
    OleCheck(SHGetFileInfo(LPWSTR(Pidl), 0,FileInfo, SizeOf(FileInfo),SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
             SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_EXETYPE));
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    OleCheck(SHGetFileInfo(LPCSTR(Pidl), 0,FileInfo, SizeOf(FileInfo),SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
             SHGFI_SMALLICON or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_EXETYPE));
    {$ENDIF}
    Node := Node.AddChild(FileInfo.szDisplayName);
    Node.VirtualParent := True;
    FMyComputerNode := Node;
    // Add Icon
    {$IFDEF DELPHI_UNICODE}
    SHGetFileInfo(LPWSTR(Pidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    SHGetFileInfo(LPCSTR(Pidl), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    {$ENDIF}
    Icon.Handle := FileInfo.hIcon;
    Node.ImageIndex := FInternalImages.AddIcon(Icon);

    AddChildren(MyComputerFolder, Node);
    SelectedNode := Node;

    GlobalFreePtr(Pidl);
    Icon.Free;
  end
  else // BaseNode = bnFolder
  begin
    if not DirectoryExists(Path) then
    begin
      if (csLoading in ComponentState) then
        Exit;
        
      raise Exception.Create('Invalid Folder Path');
    end;

    s := RemoveTailSep(ExtractFileDrive(Path));
    Path := RemoveTailSep(Path);
    if UpperCase(s) = UpperCase(Path) then  // Drive as path
    begin
      Icon := TIcon.Create;
      // Desktop
      OleCheck(SHGetDesktopFolder(DeskTopFolder));
      // My Computer
      OleCheck(SHGetSpecialFolderLocation(application.Handle, CSIDL_DRIVES,pidl));
      OleCheck(DeskTopFolder.BindToObject(pidl, Nil,IID_IShellFolder,Pointer(MyComputerFolder)));

      Flags := SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN or SHCONTF_FOLDERS;
      OleCheck(MyComputerFolder.EnumObjects(application.Handle, Flags, EnumIDList));
      while (EnumIdList.Next(1, ChildPidl, Dummy) = NOERROR) do
      begin
        Dummy := SFGAO_FOLDER;
        MyComputerFolder.GetAttributesOf(1, ChildPidl, Dummy);
        if ((Dummy and SFGAO_FOLDER) <> SFGAO_FOLDER) then
          Continue;

        s := GetDisplayName(MyComputerFolder, ChildPidl, True);

        if (Length(s) > 0) and (s[Length(s)] = '\') and (s = Uppercase(Path) + '\') then
        begin
          Node := Items.Add(s);
          SHGetFileInfo(PChar(s), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON);
          Icon.Handle := FileInfo.hIcon;
          Node.ImageIndex := FInternalImages.AddIcon(Icon);
          Break;
        end;
      end;  
      Icon.Free;
    end
    else
    begin
      fn := '';
      for i := Length(Path) downto 1 do
      begin
        if (Path[i] = NODE_SEP) then
        begin
          if (i = Length(Path)) then
            Continue
          else
            Break;
        end;
        fn := Path[i] + fn;
      end;

      if (Length(fn) > 0) then
      begin
        s := Copy(path, 1, Length(Path) - Length(fn));

        Icon := TIcon.Create;
        i := FindFirst(s + '*.*', FaDirectory, SearchRec);
        while i = 0 do
          begin
          application.ProcessMessages;
          if ((SearchRec.Attr and FaDirectory) = FaDirectory) and (not((SearchRec.Attr and faHidden) = faHidden) or ShowHiddenFolders) then
          begin
            //SHGetFileInfo(PChar(s + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_DISPLAYNAME);
            if (SearchRec.Name{FileInfo.szDisplayName} <> '') and (UpperCase(SearchRec.Name{FileInfo.szDisplayName}) = UpperCase(fn)) then
            begin
              Node := Items.Add(SearchRec.Name{FileInfo.szDisplayName});
              //SHGetFileInfo(PChar(Path + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_TYPENAME);
              SHGetFileInfo(PChar(s + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON);
              Icon.Handle := FileInfo.hIcon;
              Node.ImageIndex := FInternalImages.AddIcon(Icon);
              Break;
            end;
          end;
          i := FindNext(SearchRec);
        end;
        Icon.Free;
      end;

    end;

    if Assigned(Node) then
    begin
      Node.VirtualParent := True;
      Node.ShowText := False;
      path := path + NODE_SEP;
      AddSubFolders(path, Node);
      SelectedNode := Items.GetFirstNode;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.AddSubFolders(path: string;
  ParentNode: TAdvTreeNode);
var
  i: integer;
  Icon: TIcon;
  SearchRec: TSearchRec;
  FileInfo: SHFILEINFO;
  Node: TAdvTreeNode;
  Allow: Boolean;
begin
  if (path = '') then
    Exit;

  if (Length(path) > 0) and (path[Length(path)] <> NODE_SEP) then
    path := path + NODE_SEP;

  Allow := True;  
  if Assigned(FOnPopulateChildNode) then
    OnPopulateChildNode(Self, ParentNode, Path, Allow);
    
  if not Allow then
    Exit;
    
  Icon := TIcon.Create;
  i := FindFirst(Path + '*.*', FaAnyFile, SearchRec);
  while i = 0 do
    begin
    application.ProcessMessages;
    if ((SearchRec.Attr and FaDirectory) = FaDirectory) and (not((SearchRec.Attr and faHidden) = faHidden) or ShowHiddenFolders) then
    begin
      SHGetFileInfo(PChar(Path + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_DISPLAYNAME);
      if (FileInfo.szDisplayName <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if not Assigned(ParentNode) then
          Node := Items.Add(SearchRec.Name{FileInfo.szDisplayName})
        else
          Node := ParentNode.AddChild(SearchRec.Name{FileInfo.szDisplayName});
        //SHGetFileInfo(PChar(Path + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_TYPENAME);
        SHGetFileInfo(PChar(Path + SearchRec.Name), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON);
        Icon.Handle := FileInfo.hIcon;
        Node.ImageIndex := FInternalImages.AddIcon(Icon);
        if AutoComplete then
          AddWithChildrenToAutoCompleteList(Node);
      end;
    end;
    i := FindNext(SearchRec);
  end;
  Icon.Free;

  {if Assigned(ParentNode) then
  begin
    if Assigned(ParentNode.Button) then
    begin
      ParentNode.Button.UpdateSize;
      UpdateNodeButtonsPositions;
    end;
  end;}
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetNodePath(Node: TAdvTreeNode): string;
begin
  Result := GetHierarchicalNodeText(Node);
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TCustomExplorerTreeview.GetCurrentImages: TImageList;
begin
  if (Mode in [aeSystem, aeFolder]) then
    Result := FInternalImages
  else
    Result := Images;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(FListBox) then
  begin
    if (Key = VK_UP) and (FListBox.ItemIndex > 0) then
      FListBox.MoveSelect(-1);

    if (Key = VK_DOWN) then
      FListBox.MoveSelect(1);

    if (Key = VK_ESCAPE) then
    begin
      HideDropDownList;
      Self.SetFocus;
      SelectAll;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnListBoxKeyKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not Assigned(FListBox) then
    Exit;

  if (Key = #13 {VK_ENTER}) then
  begin
    if (FListBox.ItemIndex >= 0) then
    begin
      if (SelectedNode = TAdvTreeNode(FListBox.Items.Objects[FListBox.ItemIndex])) then
        SetDirectText(GetHierarchicalNodeText(SelectedNode))
      else
        SelectedNode := TAdvTreeNode(FListBox.Items.Objects[FListBox.ItemIndex]);
    end;
    HideDropDownList;
    Self.SetFocus;
    SelectAll;
    Exit;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetFolderPath(const Value: string);
begin
  if (FFolderPath <> Value) then
  begin
    FFolderPath := Value;
    if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Clear;
      Items.Clear;
      LoadDirectoryStructure(bnFolder, Value, True);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetIconSize: TSize;
begin
  Result.cx := LEFTICON_WIDTH;
  Result.cy := (Height - 2);
  if (Mode in [aeSystem, aeFolder]) and Assigned(FInternalImages) then
    Result.cx := FInternalImages.Width + 6
  else if Assigned(Images) then
    Result.cx := Images.Width + 6;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetAutoDropDownFill(
  const Value: Boolean);
var
  N: TAdvTreeNode;  
begin
  if (FAutoDropDownFill <> Value) then
  begin
    FAutoDropDownFill := Value;

    if Assigned(FSelectedNode) and FAutoDropDownFill and (Mode in [aeSystem, aeFolder]) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      AddToDropDownList(FSelectedNode);
      N := FSelectedNode.getFirstChild;
      while (N <> nil) do
      begin
        AddToDropDownList(N);
        N := N.getNextSibling;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.GetSelectedFolder: string;
begin
  Result := '';
  if Assigned(FSelectedNode) and (Mode in [aeSystem, aeFolder]) then
    Result := GetNodePath(FSelectedNode);
end;

//------------------------------------------------------------------------------

procedure ParseFolders(Path: string; var SL: TStringList);
var
  s: string;
  i: Integer;
begin
  s := '';
  for i := 1 to Length(Path) do
  begin
    if (Path[i] = NODE_SEP) then
    begin
      if (s <> '') then
        SL.Add(s);
      s := '';
    end
    else
      s := s + Path[i];
  end;
  if (s <> '') then
    SL.Add(s);
end;

function RemoveBackSlash(s: string): string;
begin
  if (Length(s) > 0) and (s[Length(s)] = '\') then
    Delete(s,Length(s), 1);

  Result := s;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetSelectedFolder(Path: string; Immediately: Boolean = True);
var
  sl: TStringList;
  N, N2, LastRightN, OldLastRightN: TAdvTreeNode;
  i, j: Integer;
  RightPath: Boolean;
begin
  if (Path = '') or (not (Mode in [aeSystem, aeFolder])) or (((FolderPath = '') or not DirectoryExists(FolderPath)) and (Mode = aeFolder)) then
    Exit;

  if not Immediately then
  begin
    FNewSelectedFolderPath := Path;
    PostMessage(Handle, WM_ET_SETFOLDERPATH, 0, 0);
    Exit;
  end;

  if DirectoryExists(Path) then
  begin
    BeginUpdate;
    try
      sl := TStringList.Create;
      ParseFolders(Path, sl);

      if (sl.Count > 0) then
      begin
        N := Items.GetFirstNode;
        if not Assigned(N) then
          Exit;

        if (Mode = aeFolder) then
        begin

          if (UpperCase(Path) = RemoveBackSlash(UpperCase(FolderPath))) then
          begin
            SelectedNode := N;
            Exit;
          end;

          //-- searching Folder Node in given Path
          j := -1;
          for i := 0 to sl.Count - 1 do
          begin
            if (UpperCase(sl[i]) = RemoveBackSlash(UpperCase(N.Text))) then
            begin
              j := i;
              Break;
            end;
          end;

          if (j >= 0) then   // FolderPath Node found in given path
          begin
            N := N.getFirstChild;
            Inc(j);
            if (j >= sl.Count) then
              Exit;

            while (N <> nil) do
            begin
              N2 := N;
              SelectedNode := N;
              RightPath := False;
              while (N2 <> nil) do
              begin
                if (UpperCase(sl[j]) = UpperCase(N2.Text)) then
                begin
                  RightPath := True;
                  Inc(j);
                  SelectedNode := N2;
                  if (j >= sl.Count) then
                  begin
                    //SelectedNode := N2;
                    Exit;
                  end;
                  N := N2;
                  Break;
                end;
                N2 := N2.getNextSibling;
              end;

              if not RightPath then
                Exit;

              N := N.getFirstChild;
            end;
          end;
        end
        else if (Mode = aeSystem) then
        begin
          if Assigned(FMyComputerNode) then
          begin
            N := FMyComputerNode.getFirstChild;
            j := 0;
            LastRightN := nil;
            OldLastRightN := nil;
            while (N <> nil) do  // checking children
            begin
              N2 := N;
              SelectedNode := N;
              RightPath := False;
              while (N2 <> nil) do  // checking siblings
              begin
                if (UpperCase(sl[j]) = UpperCase(RemoveTailSep(N2.Text))) then
                begin
                  RightPath := True;
                  Inc(j);
                  if (j >= sl.Count) then
                  begin
                    SelectedNode := N2;
                    Exit;
                  end;
                  N := N2;
                  SelectedNode := N;
                  LastRightN := N;
                  Break;
                end;
                N2 := N2.getNextSibling;
              end;

              //--- If path not found, another try by refreshing Sub Folders
              if not RightPath and (N2 = nil) and (LastRightN <> nil) and (LastRightN <> OldLastRightN) then
              begin                                     // avoiding infinite calls
                RefreshSubFolders(LastRightN);
                N := LastRightN;
                RightPath := True;
                OldLastRightN := LastRightN;
              end;
              //---

              if not RightPath then
                Exit;

              N := N.getFirstChild;
            end;
          end;
        end;
      end;
    finally
      EndUpdate;
      sl.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.BeginUpdate;
begin
  if not Visible then
    Exit;

  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.EndUpdate;
var
  N: TAdvTreeNode;
begin
  if not Visible then
    Exit;

  if FUpdateCount > 0 then
    Dec(FUpdateCount);

  if (FUpdateCount = 0) then
  begin
    N := SelectedNode;
    SelectedNode := nil;
    SelectedNode := N;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TCustomExplorerTreeview.IsUpdating: Boolean;
begin
  Result := (FUpdateCount > 0);
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetEditorEnabled(const Value: Boolean);
begin
  if (FEditorEnabled <> Value) then
  begin
    FEditorEnabled := Value;
    self.ReadOnly := not EditorEnabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.SetEditMode(const Value: Boolean);
var
  s: string;
begin
  if (FEditMode <> Value) and EditorEnabled and not (csDesigning in ComponentState) then
  begin
    FEditMode := Value;
    if FEditMode then
    begin
      RemoveNodeButtons;
      if (Appearance.FocusColor <> clNone) and not (csDesigning in ComponentState) then
      begin
        inherited Color := Appearance.FocusColor;
        ShowCaret(Handle);
      end;
    end
    else
    begin
      if EditorEnabled and (EditStyle = esEdit) and not (csDesigning in ComponentState) then
      begin
        if Assigned(SelectedNode) then
        begin
          s := GetHierarchicalNodeText(SelectedNode, False);
          if Assigned(SelectedNode) and (Mode = aeFolder) and (SelectedNode = Items.GetFirstNode) then
            s := '';
          if (AnsiUpperCase(inherited Text) = AnsiUpperCase(s)) then
            GenerateNodeButtons
          else
          begin
            if Assigned(FSelectedNode) then
              FOldSelected := FSelectedNode;
            FSelectedNode := nil;
            FText := inherited Text;

            if not IsUpdating then
              UpdateImage;
          end;
        end;
      end
      else
        GenerateNodeButtons;

      if EditorEnabled and (Appearance.FocusColor <> clNone) and HandleAllocated and not (csDesigning in ComponentState) then
      begin
        inherited Color := Appearance.Color;
        HideCaret(Handle);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomExplorerTreeview.OnLeftIconClick(Sender: TObject);
begin
  EditMode := False;
end;

//------------------------------------------------------------------------------

{ TDropDownButton }

constructor TDropDownButton.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner is TCustomExplorerTreeview) then
    FExplorerTreeview := TCustomExplorerTreeview(AOwner)
  else
    raise Exception.Create('Invalid parent');

  FGlyph := TBitmap.Create;
  FGlyph.OnChange := OnGlyphChanged;
  FImageIndex := -1;  
  Cursor := crArrow;
end;

//------------------------------------------------------------------------------

destructor TDropDownButton.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDropDownButton.Click;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDropDownButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := True;
    DrawButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := False;
    Invalidate;
  end;  
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if (Button = mbLeft) then
  begin
    FDown := True;
    DrawButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDropDownButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) or not Assigned(ExplorerTreeview) then
    Exit;

  if FDown and not ExplorerTreeview.IsDroppedDown then
  begin
    FDown := False;
    if ExplorerTreeview.FBufferedDraw and ExplorerTreeview.EditorEnabled then
      DrawButton
    else
      Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.SetDown(const Value: Boolean);
begin
  FDown := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.Paint;
begin
  DrawButton;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.DrawButton;
var
  R: TRect;
  Clr, ClrTo, MClr, MClrTo, BrClr, ArClr, ArShad: TColor;
  X, Y: Integer;
  imges: TImageList;

  MemDC: HDC;
  Rb: TRect;
{$IFDEF DELPHI2007_LVL}
  PaintBuffer: HPAINTBUFFER;
{$ENDIF}
  bmp: TBitmap;
  aCanvas: TCanvas;
  BufferedDraw: Boolean;
begin
  if not Assigned(ExplorerTreeview) then
    Exit;

  R := ClientRect;

  Rb := ClientRect;
{$IFDEF DELPHI2007_LVL}
  BufferedDraw := ExplorerTreeview.FBufferedDraw;
{$ELSE}
  BufferedDraw := False;
{$ENDIF}

  if BufferedDraw then
  begin
    bmp := TBitmap.Create;
    bmp.Height := Rb.Bottom - Rb.Top;
    bmp.Width := Rb.Right - Rb.Left;
    bmp.Canvas.CopyMode := cmSrcCopy;
    bmp.Canvas.CopyRect(Rb, Canvas, Rb);
    aCanvas := bmp.Canvas;

  {$IFDEF DELPHI2007_LVL}
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rb, BPBF_DIB{BPBF_TOPDOWNDIB}, nil, MemDC);
  {$ELSE}
    MemDC := 0;
  {$ENDIF}
  end
  else
  begin
    bmp := nil;
  {$IFDEF DELPHI2007_LVL}
    PaintBuffer := 0;
  {$ENDIF}
    MemDC := 0;
    aCanvas := Canvas;
  end;

  try
    if BufferedDraw then
      Canvas.Handle := MemDC;

    ArClr := clBlack;
    ArShad := clWhite;
    if FHot or FDown then
    begin
      with ExplorerTreeview do
      if FDown then
      begin
        Clr := Appearance.ButtonAppearance.ColorDown;
        ClrTo := Appearance.ButtonAppearance.ColorDownTo;
        MClr := Appearance.ButtonAppearance.ColorMirrorDown;
        MClrTo := Appearance.ButtonAppearance.ColorMirrorDownTo;
        BrClr := Appearance.ButtonAppearance.BorderColorDown;
        ArClr := Appearance.ButtonAppearance.ArrowColorDown;
        ArShad := clNone;
      end
      else
      begin
        Clr := Appearance.ButtonAppearance.ColorHot;
        ClrTo := Appearance.ButtonAppearance.ColorHotTo;
        MClr := Appearance.ButtonAppearance.ColorMirrorHot;
        MClrTo := Appearance.ButtonAppearance.ColorMirrorHotTo;
        BrClr := Appearance.ButtonAppearance.BorderColorHot;
        ArClr := Appearance.ButtonAppearance.ArrowColorHot;
      end;

      DrawVistaGradient(aCanvas, R, (R.Bottom - R.Top) div 2, Clr, ClrTo, MClr, MClrTo, BrClr, FDown);
    end;

    imges := ExplorerTreeview.Images;
    if Assigned(imges) and (ImageIndex >= 0) then
    begin
      X := R.Left + (((R.Right - R.Left) - imges.Width) div 2);
      Y := R.Top + (((R.Bottom - R.Top) - imges.Height) div 2);
      if FDown then
        Y := Y + 2;
      ExplorerTreeview.CurrentImages.Draw(aCanvas, X, Y, ImageIndex);
    end
    else if Assigned(FGlyph) and (not FGlyph.Empty) then
    begin
      X := R.Left + (((R.Right - R.Left) - FGlyph.Width) div 2);
      Y := R.Top + (((R.Bottom - R.Top) - FGlyph.Height) div 2);
      if FDown then
        Y := Y + 2;
      FGlyph.Transparent := True;
      aCanvas.Draw(X, Y, FGlyph);
    end
    else
    begin
      if FDown then
        R.Top := R.Top + 2;
      DrawArrow(aCanvas, R, ArClr, ArShad, False);
    end;


    if BufferedDraw then
    begin
      Canvas.Draw(0, 0, bmp);
      bmp.free;

    {$IFDEF DELPHI2007_LVL}
    {$IFDEF DELPHI_UNICODE}
      BufferedPaintMakeOpaque(PaintBuffer, R);
    {$ELSE}
      BufferedPaintMakeOpaque(PaintBuffer, @R);
    {$ENDIF}
    {$ENDIF}
    end;
  finally
    {$IFDEF DELPHI2007_LVL}
    if BufferedDraw then
      EndBufferedPaint(PaintBuffer, True);
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

function TDropDownButton.IsActive: Boolean;
begin
  Result := FHot or FDown;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.WMLButtonDown(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.setImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDropDownButton.OnGlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TLeftIcon }

procedure TLeftIcon.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TLeftIcon.Create(AOwner: TComponent);
begin
  inherited;

  if (AOwner is TCustomExplorerTreeview) then
    FExplorerTreeview := TCustomExplorerTreeview(AOwner)
  else
    raise Exception.Create('Invalid parent');

  Cursor := crArrow;

  FGlyph := TBitmap.Create;
  FGlyph.OnChange := OnGlyphChanged;
end;

//------------------------------------------------------------------------------

destructor TLeftIcon.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TLeftIcon.DrawIcon;
var
  R: TRect;
  X, Y: Integer;
begin
  if not Assigned(ExplorerTreeview) then
    Exit;

  if Assigned(FGlyph) and (not FGlyph.Empty) then
  begin
    R := ClientRect;
    X := R.Left + (((R.Right - R.Left) - FGlyph.Width) div 2);
    Y := R.Top + (((R.Bottom - R.Top) - FGlyph.Height) div 2) - 1;
    if ExplorerTreeview.Focused then
      Y := Y + 1;
    FGlyph.Transparent := True;
    Canvas.Draw(X, Y, FGlyph);
  end;
end;

//------------------------------------------------------------------------------

procedure TLeftIcon.OnGlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TLeftIcon.Paint;
begin
  inherited;
  DrawIcon;
end;

//------------------------------------------------------------------------------

procedure TLeftIcon.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

{ AdvExplorerTreeview menu like behavior support }

var
  AETMenuHook: HHOOK;
  InitDone: Boolean = False;
  AETreeview: TCustomExplorerTreeview;
  NodeButtonIndex: Integer;
  LastMousePos: TPoint;
  StillModal: Boolean;

function AETMenuGetMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
var
  P: TPoint;
  Target: TControl;
  MouseTarget: Boolean;
begin
  MouseTarget := false;
  Result := CallNextHookEx(AETMenuHook, Code, WParam, LParam(@Msg));
  if Result <> 0 then Exit;
  if (Code = MSGF_MENU) then
  begin
    Target := nil;
    if not InitDone then
    begin
      InitDone := True;
      PostMessage(Msg.Hwnd, WM_KEYDOWN, VK_DOWN, 0);
    end;
    case Msg.Message of
      WM_MENUSELECT:
        begin
        end;
      WM_SYSKEYDOWN:
        if Msg.WParam = VK_MENU then
        begin
          AETreeview.CancelMenu;
          Exit;
        end;
      WM_KEYDOWN:
        begin
        end;
      WM_MOUSEMOVE:
        begin
          P := Msg.pt;
          if (P.X <> LastMousePos.X) or (P.Y <> LastMousePos.Y) then
          begin
            Target := FindDragTarget(P, False);
            LastMousePos := P;
            MouseTarget := true;
          end;
        end;
    end;
    if (Target <> nil) and (Target is TNodeButton) and (TNodeButton(Target).IsSplitButton) then
    begin
      with TNodeButton(Target) do
        if (TNodeButton(Target).GetIndex <> NodeButtonIndex) and (Parent <> nil) and
          Parent.HandleAllocated then
        begin
          StillModal := True;
          AETreeview.FCaptureChangeCancels := False;
          AETreeview.ClickButton(TNodeButton(Target), MouseTarget);
        end;
    end;
  end;
end;

procedure InitAETMenuHooks;
begin
  StillModal := False;
  GetCursorPos(LastMousePos);
  if AETMenuHook = 0 then
    AETMenuHook := SetWindowsHookEx(WH_MSGFILTER, @AETMenuGetMsgHook, 0,
      GetCurrentThreadID);
end;

procedure ReleaseAETMenuHooks;
begin
  if AETMenuHook <> 0 then
    UnhookWindowsHookEx(AETMenuHook);
  AETMenuHook := 0;
  AETreeview := nil;
  NodeButtonIndex := -1;
  InitDone := False;
end;

//------------------------------------------------------------------------------

{ TNodeButton }

constructor TNodeButton.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner is TCustomExplorerTreeview) then
    FExplorerTreeview := TCustomExplorerTreeview(AOwner)
  else
    raise Exception.Create('Invalid parent');

  FNode := nil;
  FOffsetX := 4;
  FShowText := True;

  FGlyph := TBitmap.Create;

  Cursor := crArrow;
end;

//------------------------------------------------------------------------------

destructor TNodeButton.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

procedure TNodeButton.DoNodePopulate(Node: TAdvTreeNode);
begin
  if Assigned(FExplorerTreeview.OnNodePopulate) then
    FExplorerTreeview.OnNodePopulate(FExplorerTreeview, Node);
end;

//------------------------------------------------------------------------------

procedure TNodeButton.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TNodeButton.ButtonClick;
begin
  if Assigned(Node) and Assigned(ExplorerTreeview) and IsSplitButton then
  begin
    ExplorerTreeview.SelectedNode := Node;
  end;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.Click;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TNodeButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FHot := False;
  FDwBtnHot := False;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button = mbLeft) then
  begin
    FDown := True;
    DrawButton;

    if ((X > Width - DwBUTTON_WIDTH) and IsSplitButton) or ScrollButton then
      DropDownPress;
  end;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ((X < (Width - DwBUTTON_WIDTH)) and ShowText) or not IsSplitButton then
  begin
    if not FHot then
    begin
      FHot := True;
      DrawButton;
    end;
  end
  else
  begin
    if not FDwBtnHot then
    begin
      FHot := False;
      FDwBtnHot := True;
      DrawButton;
    end
    else if FHot then
    begin
      FHot := False;
      DrawButton;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (FDown) then
  begin
    FDown := False;
    if ExplorerTreeview.FBufferedDraw and ExplorerTreeview.EditorEnabled then
      DrawButton
    else
      Invalidate;
  end;
  
  if (X < (Width - DwBUTTON_WIDTH)) or not IsSplitButton then
  begin
    ButtonClick;
  end;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.SetDown(const Value: Boolean);
begin
  FDown := Value;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TNodeButton.SetNode(const Value: TAdvTreeNode);
begin
  FNode := Value;
  if Assigned(FNode) then
  begin
    ShowText := FNode.ShowText;

    if Assigned(ExplorerTreeview) and (ExplorerTreeview.SelectedNode = Node) then
      ShowText := True;
  end;
  UpdateSize;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.SetScrollButton(const Value: Boolean);
begin
  FScrollButton := Value;
  UpdateSize;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.SetShowText(const Value: Boolean);
begin
  FShowText := Value;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.UpdateSize;
var
  R: TRect;
  w: Integer;
begin
  if not Assigned(ExplorerTreeview) or not Assigned(Node) then
    Exit;

  Height := ExplorerTreeview.GetNodeButtonsRect.Bottom - ExplorerTreeview.GetNodeButtonsRect.Top;
  if ScrollButton or not ShowText then
    Width := DwBUTTON_WIDTH
  else
  begin
    w := 0;
    if (Node.Text <> '') then
    begin
      Canvas.Font.Assign(ExplorerTreeview.Appearance.ButtonAppearance.Font);
      R := Rect(0, 0, 1000, 100);
      DrawText(Canvas.Handle,PChar(Node.Text),Length(Node.Text), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_TOP);
      w := R.Right + FOffsetX * 2;
    end;
    
    if IsSplitButton then
      w := w + DwBUTTON_WIDTH;
      
    Width := Max(DwBUTTON_WIDTH, w); 
  end;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.Paint;
begin
  DrawButton;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.DrawButton;
var
  R1, R2: TRect;
  Clr, ClrTo, MClr, MClrTo, BrClr, ArClr, ArShad: TColor;
  X, Y: Integer;
  IsSplit: Boolean;

  MemDC: HDC;
  R: TRect;
{$IFDEF DELPHI2007_LVL}
  PaintBuffer: HPAINTBUFFER;
{$ENDIF}
  bmp: TBitmap;
  aCanvas: TCanvas;
  BufferedDraw: Boolean;
begin
  if not Assigned(ExplorerTreeview) or not Assigned(Node) then
    Exit;

  R1 := ClientRect;
  R2 := R1;

  R := ClientRect;

{$IFDEF DELPHI2007_LVL}
  BufferedDraw := ExplorerTreeview.FBufferedDraw;
{$ELSE}
  BufferedDraw := False;
{$ENDIF}

  if BufferedDraw then
  begin
    bmp := TBitmap.Create;
    bmp.Height := R1.Bottom - R1.Top;
    bmp.Width := R1.Right - R1.Left;
    bmp.Canvas.CopyMode := cmSrcCopy;
    bmp.Canvas.CopyRect(R1, Canvas, R1);
    aCanvas := bmp.Canvas;

  {$IFDEF DELPHI2007_LVL}
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, R, BPBF_DIB{BPBF_TOPDOWNDIB}, nil, MemDC);
  {$ELSE}
    MemDC := 0;
  {$ENDIF}
  end
  else
  begin
    bmp := nil;
  {$IFDEF DELPHI2007_LVL}
    PaintBuffer := 0;
  {$ENDIF}
    MemDC := 0;
    aCanvas := Canvas;
  end;

  try
    if BufferedDraw then
      Canvas.Handle := MemDC;

    if IsSplitButton and not ScrollButton and ShowText then
    begin
      R2.Left := R2.Right - DwBUTTON_WIDTH;
      R1.Right := R2.Left;
    end;

    ArClr := clBlack;
    ArShad := clWhite;
    Clr := clNone;
    ClrTo := clNone;
    MClr := clNone;
    MClrTo := clNone;
    BrClr := clNone;
    IsSplit := IsSplitButton;

    if FHot or FDown or FDwBtnHot then
    begin
      with ExplorerTreeview do
      if FDown then
      begin
        Clr := Appearance.ButtonAppearance.ColorDown;
        ClrTo := Appearance.ButtonAppearance.ColorDownTo;
        MClr := Appearance.ButtonAppearance.ColorMirrorDown;
        MClrTo := Appearance.ButtonAppearance.ColorMirrorDownTo;
        BrClr := Appearance.ButtonAppearance.BorderColorDown;
        ArClr := Appearance.ButtonAppearance.ArrowColorDown;
        ArShad := clNone;
      end
      else if FHot or FDwBtnHot then
      begin
        Clr := Appearance.ButtonAppearance.ColorHot;
        ClrTo := Appearance.ButtonAppearance.ColorHotTo;
        MClr := Appearance.ButtonAppearance.ColorMirrorHot;
        MClrTo := Appearance.ButtonAppearance.ColorMirrorHotTo;
        BrClr := Appearance.ButtonAppearance.BorderColorHot;
        ArClr := Appearance.ButtonAppearance.ArrowColorHot;
      end;

      if ScrollButton or not ShowText then
        DrawVistaGradient(aCanvas, R1, (R1.Bottom - R1.Top) div 2, Clr, ClrTo, MClr, MClrTo, BrClr, FDown, True)
      else
      begin
        if IsSplit then
        begin
          // Dw part
          DrawVistaGradient(aCanvas, R2, (R2.Bottom - R2.Top) div 2, Clr, ClrTo, MClr, MClrTo, BrClr, FDown, True);
        end;

        if FDwBtnHot and not FHot and not FDown then
        begin
          with ExplorerTreeview do
          begin
            Clr := Appearance.ButtonAppearance.ColorNodeHot;
            ClrTo := Appearance.ButtonAppearance.ColorNodeHotTo;
            MClr := Appearance.ButtonAppearance.ColorMirrorNodeHot;
            MClrTo := Appearance.ButtonAppearance.ColorMirrorNodeHotTo;
            BrClr := Appearance.ButtonAppearance.BorderColorNodeHot;
          end;
        end;

        // Node part
        DrawVistaGradient(aCanvas, R1, (R1.Bottom - R1.Top) div 2, Clr, ClrTo, MClr, MClrTo, BrClr, FDown, not IsSplit);
      end;
    end;

    // draw text
    if (Node.Text <> '') and not ScrollButton and ShowText then
    begin
      if FDown then
        R1.Top := R1.Top + 2;
      aCanvas.Font.Assign(ExplorerTreeview.Appearance.ButtonAppearance.Font);
      aCanvas.Brush.Style := bsClear;
      DrawText(aCanvas.Handle, PChar(Node.Text), Length(Node.Text), r1, DT_VCENTER or DT_SINGLELINE or DT_CENTER);
    end;

    if ScrollButton then
    begin
      DrawScrollArrow(aCanvas, ClientRect, ArClr);
    end
    else if not ScrollButton and IsSplitButton and ShowText then
    begin
      if FDown then
        R2.Top := R2.Top + 2;
      DrawArrow(aCanvas, R2, ArClr, ArShad, not FDown);
    end
    else if not ShowText then
    begin
      R2 := ClientRect;
      if FDown then
        R2.Top := R2.Top + 2;
      DrawArrow(aCanvas, R2, ArClr, ArShad, not FDown);
    end;


    if Assigned(FGlyph) and (not FGlyph.Empty) then
    begin
      X := R1.Left + (((R1.Right - R1.Left) - FGlyph.Width) div 2);
      Y := R1.Top + (((R1.Bottom - R1.Top) - FGlyph.Height) div 2);
      if FDown then
        Y := Y + 2;
      aCanvas.Draw(X, Y, FGlyph);
    end
    else
    begin
      //if FDown then
        //R1.Top := R1.Top + 2;
      //DrawArrow(Canvas, R1, ArClr, ArShad, False);
    end;

    if BufferedDraw then
    begin
      Canvas.Draw(0, 0, bmp);
      bmp.free;

    {$IFDEF DELPHI2007_LVL}
    {$IFDEF DELPHI_UNICODE}
      BufferedPaintMakeOpaque(PaintBuffer, R);
    {$ELSE}
      BufferedPaintMakeOpaque(PaintBuffer, @R);
    {$ENDIF}
    {$ENDIF}
    end;

  finally
    {$IFDEF DELPHI2007_LVL}
    if BufferedDraw then
      EndBufferedPaint(PaintBuffer, True);
    {$ENDIF}
  end;

end;

//------------------------------------------------------------------------------

procedure TNodeButton.DropDownPress;
var
  Menu: TPopupMenu;
  MI: TMenuItem;
  N, NP: TAdvTreeNode;
  P: TPoint;
  R: TRect;
  Hook: Boolean;

  procedure AddSeparator;
  begin
    MI := TMenuItem.Create(Menu);
    MI.Caption := '-';
    Menu.Items.Add(MI);
  end;

begin
  if not Assigned(Node) or not Assigned(ExplorerTreeview) then
    Exit;

  Menu := TPopupMenu.Create(Owner);
  Menu.Images := ExplorerTreeview.CurrentImages;

  NP := Node;

  DoNodePopulate(NP);

  if ScrollButton then
  begin
    if Assigned(NP) and (ExplorerTreeview.SelectedNode <> Node) then
    begin
      N := NP;
      while (N <> nil) do
      begin
        MI := TMenuItem.Create(Menu);
        MI.Caption := N.Text;
        MI.ImageIndex := N.ImageIndex;
        MI.Tag := Integer(N);
        MI.OnClick := OnMenuItemClick;
        Menu.Items.Add(MI);
        N := N.GetParent;
      end;
    end;

    NP := ExplorerTreeview.Items.GetFirstNode;
    if not Assigned(NP) then
      NP := Node;
    AddSeparator;
  end
  else if (Node.Parent = nil) and (ExplorerTreeview.SelectedNode <> Node) then
  begin
    MI := TMenuItem.Create(Menu);
    MI.Caption := Node.Text;
    MI.ImageIndex := Node.ImageIndex;
    MI.Tag := Integer(Node);
    MI.OnClick := OnMenuItemClick;
    Menu.Items.Add(MI);
    AddSeparator;
  end;

  N := NP.getFirstChild;
  while (N <> nil) do
  begin
    MI := TMenuItem.Create(Menu);
    MI.Caption := N.Text;
    MI.ImageIndex := N.ImageIndex;
    MI.Tag := Integer(N);
    MI.OnClick := OnMenuItemClick;
    Menu.Items.Add(MI);
    N := N.getNextSibling;
  end;
  P := Point(Width div 2, Height);
  P := ClientToScreen(P);

  Hook := False;
  try
    if ExplorerTreeview.AllowHook then
    begin
      ExplorerTreeview.FCaptureChangeCancels := False;
      if (Menu.Items.Count > 0) then
      begin
        NodeButtonIndex := GetIndex;
        AETreeview := ExplorerTreeview;
        InitAETMenuHooks;
        Hook := True;
      end;
    end;

    Menu.Popup(P.X, P.Y);
    
  finally
    if Hook then ReleaseAETMenuHooks;
  end;

  if (FDown) then
  begin
    FDown := False;
    GetCursorPos(p);
    p := ScreenToClient(p);
    R := ClientRect;
    FHot := PtInRect(r, p);
    R.Left := Width - DwBUTTON_WIDTH;
    FDwBtnHot := PtInRect(r, p);
    Invalidate;
  end;  
end;

//------------------------------------------------------------------------------

function TNodeButton.GetIndex: Integer;
begin
  Result := -1;
  if Assigned(ExplorerTreeview) then
  Result := ExplorerTreeview.FNodeButtons.IndexOf(Self);
end;

//------------------------------------------------------------------------------

procedure TNodeButton.OnMenuItemClick(Sender: TObject);
var
  MI: TMenuItem;
begin
  if not Assigned(Sender) or not (Sender is TMenuITem) or not Assigned(ExplorerTreeview) then
    Exit;
    
  MI := TMenuITem(Sender);
  ExplorerTreeview.SelectedNode := TAdvTreeNode(Pointer(MI.Tag));
end;

//------------------------------------------------------------------------------

function TNodeButton.IsActive: Boolean;
begin
  Result := FHot or FDown or FDwBtnHot;
end;

//------------------------------------------------------------------------------

function TNodeButton.IsSplitButton: Boolean;
begin
  Result := False;
  if Assigned(Node) and not ScrollButton then
    Result := Node.getFirstChild <> nil;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.WMLButtonDown(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TNodeButton.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------

{ TExplorerTreeviewListBox }

procedure TExplorerTreeviewListBox.Click;
begin
  inherited; 
end;

//------------------------------------------------------------------------------

constructor TExplorerTreeviewListBox.Create(AOwner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawFixed;
  FShowImages := True;
end;

//------------------------------------------------------------------------------

destructor TExplorerTreeviewListBox.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseInControl := True;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseInControl := False;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  N: TAdvTreeNode;
  Offset: Integer;
begin
  if not Assigned(ExplorerTreeview) or not Assigned(Items.Objects[Index]) or not(Items.Objects[Index] is TAdvTreeNode) then
  begin
    inherited;
    Exit;
  end;

  R := Rect;
  Canvas.FillRect(Rect);       { clear the rectangle }
  Offset := 5;

  // Draw Image
  N := TAdvTreeNode(Items.Objects[Index]);
  if ShowImages and Assigned(ExplorerTreeview.CurrentImages) and Assigned(N) then
  begin
    if (N.ImageIndex >= 0) then
    begin
      ExplorerTreeview.CurrentImages.Draw(Canvas, R.Left + 2, R.Top + (R.Bottom - R.Top - ExplorerTreeview.CurrentImages.Height) div 2, N.ImageIndex);
    end;
    R.Left := R.Left + ExplorerTreeview.CurrentImages.Width;
  end;

  // Draw Text
  //Canvas.TextOut(R.Left + Offset, R.Top, Items[Index]);
  R.Left := R.Left + Offset;
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), R, DT_VCENTER or DT_SINGLELINE or DT_LEFT);
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  P: TPoint;
begin
  inherited;
  P := Point(X, y);
  i := ItemAtPos(P, True);
  if (i >= 0) then
  begin
    if not Selected[i] then
      Selected[i] := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  P: TPoint;
begin
  inherited;
  P := Point(X, Y);
  i := ItemAtPos(P, True);
  if (i >= 0) then
    ItemIndex := i;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.MoveSelect(Offset: Integer);
begin
  if ItemIndex + Offset < Count then
    ItemIndex := ItemIndex + Offset;
    //Selected[ItemIndex + Offset] := True;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := {DLGC_WANTTAB or }DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TExplorerTreeviewListBox.WMKeyDown(var Msg: TWMKeydown);
begin
  {if (Msg.CharCode = VK_UP) then
  begin
    MoveSelect(-1);
    Msg.Result := 0;
  end;

  if (Msg.CharCode = VK_DOWN) then
  begin
    MoveSelect(1);
    Msg.Result := 0;
  end;}
  inherited;
end;

//------------------------------------------------------------------------------

{ TDbgList }

procedure TDbgList.AssignList(ListA: TList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ListA.Count - 1 do
    Add(ListA[I]);
end;

//------------------------------------------------------------------------------

function TDbgList.GetItemsEx(Index: Integer): TAdvTreeNode;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: TAdvTreeNode);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

//------------------------------------------------------------------------------

initialization
  WM_ET_HIDEDROPDOWN := RegisterWindowMessage('ET_HIDEDROPDOWN');
  WM_ET_SETFOLDERPATH := RegisterWindowMessage('ET_SETFOLDERPATH');
  WM_ET_TRACKKEYPRESS := RegisterWindowMessage('ET_TRACKKEYPRESS');
end.

