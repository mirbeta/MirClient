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

{$DEFINE USETCXSCROLLBAR}

unit cxTreeView;

{$I cxVer.inc}

interface

uses
  Windows, Types, Classes, ComCtrls, CommCtrl, Controls, Forms, ImgList, Menus,
  Messages, StdCtrls, SysUtils,
  cxClasses, cxContainer, cxControls, cxGraphics, cxLookAndFeels, cxScrollBar,
  cxEdit, cxExtEditConsts, dxComCtrlsUtils, dxTypeHelpers;

type
  TcxCustomTreeView = class;
  TcxTreeViewContainer = class;

  { TcxBaseInnerTreeView }

  TcxBaseInnerTreeView = class(TTreeView, IUnknown, IcxContainerInnerControl)
  private
    FCanvas: TcxCanvas;
    FScrollUIActivityHelper: TdxTouchScrollUIActivityHelper;

    procedure HScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure VScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure TVMHitTest(var Message: TMessage); message TVM_HITTEST;
  protected
    FContainer: TcxTreeViewContainer;

    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function GetClientOrigin: TPoint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure WndProc(var Message: TMessage); override;

    property Canvas: TcxCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure DefaultHandler(var Message); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;
  TcxInnerTreeViewClass = class of TcxBaseInnerTreeView;

  { TcxCustomInnerTreeView }

  TcxCustomInnerTreeView = class(TcxBaseInnerTreeView)
  private
    FIsRedrawLocked: Boolean;
    FItemHeight: Integer;
    FLookAndFeel: TcxLookAndFeel;

    function GetContainer: TcxCustomTreeView;
    procedure SetItemHeight(Value: Integer);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure WMSetRedraw(var Message: TWMSetRedraw); message WM_SETREDRAW;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure DestroyWindowHandle; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    property Container: TcxCustomTreeView read GetContainer;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    procedure Expand(Node: TTreeNode); override;
    procedure Change(Node: TTreeNode); override;
    procedure Collapse(Node: TTreeNode); override;
    procedure UpdateItemHeight;
    property IsRedrawLocked: Boolean read FIsRedrawLocked;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default -1;
  end;

  TcxCustomInnerTreeViewClass = class of TcxCustomInnerTreeView;

  { TcxTreeViewContainer }

  TcxInnerTreeViewWndProcEvent = procedure(ATreeView: TTreeView; var Message: TMessage; var ADone: Boolean) of object;

  TcxTreeViewContainer = class(TcxCustomEditContainer)
  private
    FInnerTreeView: TcxBaseInnerTreeView;
    FOnInnerTreeViewWndProc: TcxInnerTreeViewWndProcEvent;

    function GetAutoExpand: Boolean;
    function GetChangeDelay: Integer;
    function GetHideSelection: Boolean;
    function GetReadOnly: Boolean;
    function GetRightClickSelect: Boolean;
    function GetShowButtons: Boolean;
    function GetShowLines: Boolean;
    function GetShowRoot: Boolean;
    function GetStateImages: TCustomImageList;
    procedure SetAutoExpand(Value: Boolean);
    procedure SetChangeDelay(Value: Integer);
    procedure SetHideSelection(Value: Boolean);
    procedure SetRightClickSelect(Value: Boolean);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetStateImages(Value: TCustomImageList);
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure CorrectAlignControlRect(var R: TRect); override;
    function DoInnerTreeViewWndProcHandler(var Message: TMessage): Boolean; virtual;
    function GetInnerTreeViewClass: TcxInnerTreeViewClass; virtual;
    function GetScrollBarBounds(const AScrollBarRect: TRect): TRect; override;
    function HandleInnerContolGestures: Boolean; override;
    procedure InitializeInternalTreeView; virtual;
    procedure InternalInitTreeView; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function NeedsScrollBars: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetReadOnly(Value: Boolean); virtual;
    function UseSystemRightToLeftLayoutForInnerControl: Boolean; override;

    property InnerTreeView: TcxBaseInnerTreeView read FInnerTreeView;
    property OnInnerTreeViewWndProc: TcxInnerTreeViewWndProcEvent read FOnInnerTreeViewWndProc write FOnInnerTreeViewWndProc;
    //
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand  default False;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean read GetRightClickSelect write SetRightClickSelect default False;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons default True;
    property ShowLines: Boolean read GetShowLines write SetShowLines default True;
    property ShowRoot: Boolean read GetShowRoot write SetShowRoot default True;
    property StateImages: TCustomImageList read GetStateImages write SetStateImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TcxCustomTreeView }

  TcxCustomTreeView = class(TcxTreeViewContainer)
  private
    procedure CheckInnerTreeViewAlign;
    function GetHotTrack: Boolean;
    function GetImages: TCustomImageList;
    function GetItemHeight: Integer;
    function GetTreeNodes: TTreeNodes;
    function GetIndent: Integer;
    function GetMultiSelect: Boolean;
    function GetMultiSelectStyle: TMultiSelectStyle;
    function GetOnCreateNodeClass: TTVCreateNodeClassEvent;
    procedure SetMultiSelectStyle(Value: TMultiSelectStyle);
    procedure SetOnCreateNodeClass(Value: TTVCreateNodeClassEvent);
    function GetOnAddition: TTVExpandedEvent;
    function GetOnCancelEdit: TTVChangedEvent;
    function GetRowSelect: Boolean;
    function GetSortType: TSortType;
    function GetToolTips: Boolean;
    function GetTreeViewCanvas: TcxCanvas;
    function GetOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
    function GetOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
    function GetOnChange: TTVChangedEvent;
    function GetOnChanging: TTVChangingEvent;
    function GetOnCollapsed: TTVExpandedEvent;
    function GetOnCollapsing: TTVCollapsingEvent;
    function GetOnCompare: TTVCompareEvent;
    function GetOnCustomDraw: TTVCustomDrawEvent;
    function GetOnCustomDrawItem: TTVCustomDrawItemEvent;
    function GetOnDeletion: TTVExpandedEvent;
    function GetOnEditing: TTVEditingEvent;
    function GetOnEdited: TTVEditedEvent;
    function GetOnExpanding: TTVExpandingEvent;
    function GetOnExpanded: TTVExpandedEvent;
    function GetOnGetImageIndex: TTVExpandedEvent;
    function GetOnGetSelectedIndex: TTVExpandedEvent;
    function GetDropTarget: TTreeNode;
    function GetSelected: TTreeNode;
    function GetTopItem: TTreeNode;
    function GetSelectionCount: Cardinal;
    function GetSelection(Index: Integer): TTreeNode;
    procedure SetMultiSelect(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetTreeNodes(Value: TTreeNodes);
    procedure SetIndent(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetRowSelect(Value: Boolean);
    procedure SetSortType(Value: TSortType);
    procedure SetToolTips(Value: Boolean);
    procedure SetOnAddition(Value: TTVExpandedEvent);
    procedure SetOnCancelEdit(Value: TTVChangedEvent);
    procedure SetOnAdvancedCustomDraw(Value: TTVAdvancedCustomDrawEvent);
    procedure SetOnAdvancedCustomDrawItem(Value: TTVAdvancedCustomDrawItemEvent);
    procedure SetOnChange(Value: TTVChangedEvent);
    procedure SetOnChanging(Value: TTVChangingEvent);
    procedure SetOnCollapsed(Value: TTVExpandedEvent);
    procedure SetOnCollapsing(Value: TTVCollapsingEvent);
    procedure SetOnCompare(Value: TTVCompareEvent);
    procedure SetOnCustomDraw(Value: TTVCustomDrawEvent);
    procedure SetOnCustomDrawItem(Value: TTVCustomDrawItemEvent);
    procedure SetOnDeletion(Value: TTVExpandedEvent);
    procedure SetOnEditing(Value: TTVEditingEvent);
    procedure SetOnEdited(Value: TTVEditedEvent);
    procedure SetOnExpanding(Value: TTVExpandingEvent);
    procedure SetOnExpanded(Value: TTVExpandedEvent);
    procedure SetOnGetImageIndex(Value: TTVExpandedEvent);
    procedure SetOnGetSelectedIndex(Value: TTVExpandedEvent);
    procedure SetDropTarget(Value: TTreeNode);
    procedure SetSelected(Value: TTreeNode);
    procedure SetTopItem(Value: TTreeNode);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure FontChanged; override;
    function IsReadOnly: Boolean; override;
    procedure DoSetSize; override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure WriteState(Writer: TWriter); override;

    function GetInnerTreeView: TcxCustomInnerTreeView; virtual;
    function GetInnerTreeViewClass: TcxInnerTreeViewClass; override;
    class function GetTreeViewClass: TcxCustomInnerTreeViewClass; virtual;
    procedure InitializeInternalTreeView; override;
    {TreeView}
    function CanChange(Node: TTreeNode): Boolean; virtual;
    function CanCollapse(Node: TTreeNode): Boolean; virtual;
    function CanEdit(Node: TTreeNode): Boolean; virtual;
    function CanExpand(Node: TTreeNode): Boolean; virtual;
    procedure Collapse(Node: TTreeNode);
    procedure DoScrollUIModeChanged; override;
    procedure Expand(Node: TTreeNode);
    //

    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read GetImages write SetImages;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight default -1;
    property Items: TTreeNodes read GetTreeNodes write SetTreeNodes;
    property Indent: Integer read GetIndent write SetIndent default 19;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
    property MultiSelectStyle: TMultiSelectStyle read GetMultiSelectStyle
      write SetMultiSelectStyle default [msControlSelect];
    property RowSelect: Boolean read GetRowSelect write SetRowSelect default False;
    property SortType: TSortType read GetSortType write SetSortType default stNone;
    property ToolTips: Boolean read GetToolTips write SetToolTips default True;
    property OnAddition: TTVExpandedEvent read GetOnAddition write SetOnAddition;
    property OnCancelEdit: TTVChangedEvent read GetOnCancelEdit write SetOnCancelEdit;
    property OnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent read GetOnAdvancedCustomDraw write SetOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent read GetOnAdvancedCustomDrawItem write SetOnAdvancedCustomDrawItem;
    property OnChange: TTVChangedEvent read GetOnChange write SetOnChange;
    property OnChanging: TTVChangingEvent read GetOnChanging write SetOnChanging;
    property OnCollapsed: TTVExpandedEvent read GetOnCollapsed write SetOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read GetOnCollapsing write SetOnCollapsing;
    property OnCompare: TTVCompareEvent read GetOnCompare write SetOnCompare;
    property OnCustomDraw: TTVCustomDrawEvent read GetOnCustomDraw write SetOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read GetOnCustomDrawItem write SetOnCustomDrawItem;
    property OnDeletion: TTVExpandedEvent read GetOnDeletion write SetOnDeletion;
    property OnEditing: TTVEditingEvent read GetOnEditing write SetOnEditing;
    property OnEdited: TTVEditedEvent read GetOnEdited write SetOnEdited;
    property OnExpanding: TTVExpandingEvent read GetOnExpanding write SetOnExpanding;
    property OnExpanded: TTVExpandedEvent read GetOnExpanded write SetOnExpanded;
    property OnGetImageIndex: TTVExpandedEvent read GetOnGetImageIndex write SetOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read GetOnGetSelectedIndex write SetOnGetSelectedIndex;
    property OnCreateNodeClass: TTVCreateNodeClassEvent read GetOnCreateNodeClass write SetOnCreateNodeClass;
  public
    constructor Create(AOwner: TComponent); override;
    function AlphaSort(ARecurse: Boolean = True): Boolean;
    function CustomSort(SortProc: TTVCompare; Data: Longint; ARecurse: Boolean = True): Boolean;
    procedure FullCollapse;
    procedure FullExpand;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetNodeAt(X, Y: Integer): TTreeNode;
    function IsEditing: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetScrollBarsParameters(AIsScrolling: Boolean = False); override;
    property DropTarget: TTreeNode read GetDropTarget write SetDropTarget;
    property Selected: TTreeNode read GetSelected write SetSelected;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;
    property TreeViewCanvas: TcxCanvas read GetTreeViewCanvas;
    procedure Select(Node: TTreeNode; ShiftState: TShiftState = []); overload; virtual;
    procedure Select(const Nodes: array of TTreeNode); overload; virtual;
    procedure Select(Nodes: TList); overload; virtual;
    procedure Deselect(Node: TTreeNode); virtual;
    procedure Subselect(Node: TTreeNode; Validate: Boolean = False); virtual;
    property SelectionCount: Cardinal read GetSelectionCount;
    property Selections[Index: Integer]: TTreeNode read GetSelection;
    procedure ClearSelection(KeepPrimary: Boolean = False); virtual;
    function GetSelections(AList: TList): TTreeNode;
    function FindNextToSelect: TTreeNode; virtual;
    property InnerTreeView: TcxCustomInnerTreeView read GetInnerTreeView;
  end;

  { TcxTreeView }

  TcxTreeView = class(TcxCustomTreeView)
  public
    property TreeViewCanvas;
    property OnInnerTreeViewWndProc;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Height default 100;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width default 120;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnContextPopup;
    property AutoExpand;
    property ChangeDelay;
    property HideSelection;
    property HotTrack;
    property Images;
    property ItemHeight;
    property Items;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property ToolTips;
    property OnAddition;
    property OnCancelEdit;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDeletion;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnCreateNodeClass;
  end;

function cxTreeViewGetHitNode(ATreeView: TcxTreeView; const ACursorPos: TPoint): TTreeNode;

implementation

uses
  Graphics, dxCore, cxGeometry, cxLookAndFeelPainters;

function cxTreeViewGetHitNode(ATreeView: TcxTreeView; const ACursorPos: TPoint): TTreeNode;
begin
  Result := dxComCtrlsUtils.cxTreeViewGetHitNode(ATreeView.InnerTreeView,
    cxPointOffset(ACursorPos, ATreeView.BoundsRect.TopLeft, False));
end;

{ TcxBaseInnerTreeView }

constructor TcxBaseInnerTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TcxCanvas.Create(inherited Canvas);
  BorderStyle := bsNone;
  ControlStyle := ControlStyle + [csDoubleClicks{$IFDEF DELPHI16}, csOverrideStylePaint{$ENDIF}];
  ParentFont := True;
  FScrollUIActivityHelper := TdxTouchScrollUIActivityHelper.Create;
end;

destructor TcxBaseInnerTreeView.Destroy;
begin
  FreeAndNil(FScrollUIActivityHelper);
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

function TcxBaseInnerTreeView.CanFocus: Boolean;
begin
  Result := FContainer.CanFocus;
end;

procedure TcxBaseInnerTreeView.DefaultHandler(var Message);
begin
  if (FContainer = nil) or
    not FContainer.InnerControlDefaultHandler(TMessage(Message)) then
      inherited DefaultHandler(Message);
end;

procedure TcxBaseInnerTreeView.DragDrop(Source: TObject; X, Y: Integer);
begin
  if FContainer <> nil then
    FContainer.DragDrop(Source, Left + X, Top + Y);
end;

procedure TcxBaseInnerTreeView.Click;
begin
  inherited Click;
  if FContainer <> nil then
    FContainer.Click;
end;

procedure TcxBaseInnerTreeView.DblClick;
begin
  inherited DblClick;
  if FContainer <> nil then
    FContainer.DblClick;
end;

function TcxBaseInnerTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
   MousePos: TPoint): Boolean;
begin
  Result := (FContainer <> nil) and
    FContainer.DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TcxBaseInnerTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if FContainer <> nil then
    FContainer.DragOver(Source, Left + X, Top + Y, State, Accept);
end;

procedure TcxBaseInnerTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FContainer <> nil then
    FContainer.KeyDown(Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TcxBaseInnerTreeView.KeyPress(var Key: Char);
begin
  if Key = Char(VK_TAB) then
    Key := #0;
  if FContainer <> nil then
    FContainer.KeyPress(Key);
  if Word(Key) = VK_RETURN then
    Key := #0;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TcxBaseInnerTreeView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_TAB then
    Key := 0;
  if FContainer <> nil then
    FContainer.KeyUp(Key, Shift);
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TcxBaseInnerTreeView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FContainer <> nil then
    with FContainer do
    begin
      InnerControlMouseDown := True;
      try
        MouseDown(Button, Shift, X + Self.Left, Y + Self.Top);
      finally
        InnerControlMouseDown := False;
      end;
    end;
end;

procedure TcxBaseInnerTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FContainer <> nil then
    FContainer.MouseMove(Shift, X + Left, Y + Top);
end;

procedure TcxBaseInnerTreeView.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FContainer <> nil then
    FContainer.MouseUp(Button, Shift, X + Left, Y + Top);
end;

procedure TcxBaseInnerTreeView.MouseEnter(AControl: TControl);
begin
end;

procedure TcxBaseInnerTreeView.MouseLeave(AControl: TControl);
begin
  if FContainer <> nil then
    FContainer.ShortRefreshContainer(True);
end;

procedure TcxBaseInnerTreeView.WndProc(var Message: TMessage);
begin
  if FContainer <> nil then
  begin
    if FContainer.IsTouchScrollUIMode and
      FScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
      FContainer.ShowTouchScrollUI(FContainer, True);

    if FContainer.InnerControlMenuHandler(Message)
        or FContainer.DoInnerTreeViewWndProcHandler(Message) then
      Exit;

    if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
      (FContainer.DragMode = dmAutomatic) and (FContainer.DragKind = dkDock) and
      not FContainer.IsDesigning then
    begin
      FContainer.BeginAutoDrag;
      Exit;
    end;
  end;

  inherited WndProc(Message);
  case Message.Msg of
    WM_HSCROLL,
    WM_MOUSEWHEEL,
    WM_VSCROLL,
    WM_WINDOWPOSCHANGED,
    CM_WININICHANGE,
    TVM_ENSUREVISIBLE,
    TVM_EXPAND,
    TVM_INSERTITEM,
    TVM_SELECTITEM:
      FContainer.SetScrollBarsParameters;
  end;
end;

procedure TcxBaseInnerTreeView.HScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  CallWindowProc(DefWndProc, Handle, WM_HSCROLL, Word(ScrollCode) +
    Word(ScrollPos) shl 16, FContainer.GetScrollBarHandle(sbHorizontal));
  ScrollPos := GetScrollPos(Handle, SB_HORZ);
end;

procedure TcxBaseInnerTreeView.VScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  AScrollInfo: TScrollInfo;
begin
  if (ScrollCode in [scPosition, scTrack]) and (Win32MajorVersion >= 6) then
  begin
    AScrollInfo.cbSize := SizeOf(AScrollInfo);
    AScrollInfo.fMask := SIF_POS;
    AScrollInfo.nPos := ScrollPos;
    SetScrollInfo(Handle, SB_VERT, AScrollInfo, True);
  end;
  CallWindowProc(DefWndProc, Handle, WM_VSCROLL, Word(ScrollCode) +
    Word(ScrollPos) shl 16, FContainer.GetScrollBarHandle(sbVertical));
  ScrollPos := GetScrollPos(Handle, SB_VERT);
end;

function TcxBaseInnerTreeView.GetClientOrigin: TPoint;
begin
  Result := inherited;
  if UseRightToLeftAlignment then
    Dec(Result.X, ClientWidth);
end;

function TcxBaseInnerTreeView.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxBaseInnerTreeView.GetControlContainer: TcxContainer;
begin
  Result := FContainer;
end;

procedure TcxBaseInnerTreeView.WMGestureNotify(var Message: TWMGestureNotify);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.Unused, LPARAM(Message.NotifyStruct));
end;

procedure TcxBaseInnerTreeView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if FContainer <> nil then
    with Message do
    begin
      Result := Result or DLGC_WANTCHARS;
      if GetKeyState(VK_CONTROL) >= 0 then
        Result := Result or DLGC_WANTTAB;
    end;
end;

procedure TcxBaseInnerTreeView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (FContainer <> nil) and not FContainer.IsDestroying then
    FContainer.FocusChanged;
end;

procedure TcxBaseInnerTreeView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if not FContainer.IsTouchScrollUIMode and UsecxScrollBars and not FContainer.ScrollBarsCalculating then
    FContainer.SetScrollBarsParameters;
end;

procedure TcxBaseInnerTreeView.WMNCPaint(var Message: TWMNCPaint);
begin
  if UsecxScrollBars and not FContainer.IsTouchScrollUIMode then
  begin
    Message.Result := 1;
    if FContainer.HScrollBar.Visible and FContainer.VScrollBar.Visible then
      cxFillSizeGrip(FContainer);
    if FContainer.HScrollBar.Visible and (FContainer.HScrollBar.Control <> nil) then
      FContainer.HScrollBar.Control.Repaint;
    if FContainer.VScrollBar.Visible and (FContainer.VScrollBar.Control <> nil) then
      FContainer.VScrollBar.Control.Repaint;
  end
  else
  begin
    inherited;
    Exit;
  end;
end;

procedure TcxBaseInnerTreeView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (FContainer <> nil) and not FContainer.IsDestroying and
    not(csDestroying in ComponentState) and
    (Message.FocusedWnd <> FContainer.Handle) then
    FContainer.FocusChanged;
end;

procedure TcxBaseInnerTreeView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not FContainer.IsTouchScrollUIMode and not (csDestroying in ComponentState) and
    FContainer.HScrollBarVisible and
    FContainer.VScrollBarVisible then
      cxRedrawNCRect(Handle, GetSizeGripRect(Self));
end;

procedure TcxBaseInnerTreeView.CMBiDiModeChanged(var Message: TMessage);
var
  APrevWParam: Integer;
begin
  APrevWParam := Message.WParam;
  try
    Message.wParam := 1;
    inherited;
  finally
    Message.wParam := APrevWParam;
  end;
end;

procedure TcxBaseInnerTreeView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxBaseInnerTreeView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxBaseInnerTreeView.CMRecreateWnd(var Message: TMessage);
var
  AHasHandle: Boolean;
begin
  AHasHandle := HandleAllocated;
  inherited;
  if AHasHandle then
    HandleNeeded;
end;

procedure TcxBaseInnerTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if UseRightToLeftAlignment and FContainer.UseSystemRightToLeftLayoutForInnerControl then
    Params.ExStyle := (Params.ExStyle or WS_EX_LAYOUTRTL or WS_EX_NOINHERITLAYOUT) and
      not (WS_EX_LEFTSCROLLBAR or WS_EX_RIGHT or WS_EX_RTLREADING);
end;

procedure TcxBaseInnerTreeView.TVMHitTest(var Message: TMessage);
begin
  if IsWindowEnabledEx(Handle) then
    inherited
  else
    Message.Result := 0;
end;

{ TcxCustomInnerTreeView }

constructor TcxCustomInnerTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemHeight := -1;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
end;

destructor TcxCustomInnerTreeView.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TcxCustomInnerTreeView.DestroyWindowHandle;
begin
  FIsRedrawLocked := False;
  inherited DestroyWindowHandle;
end;

procedure TcxCustomInnerTreeView.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  UpdateItemHeight;
  Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.WMFontChange(var Message: TMessage);
begin
  inherited;
  if not Container.ScrollBarsCalculating then
    Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Dragging then
  begin
    CancelDrag;
    Container.BeginDrag(False);
  end;
end;

function TcxCustomInnerTreeView.GetContainer: TcxCustomTreeView;
begin
  Result := TcxCustomTreeView(FContainer);
end;

procedure TcxCustomInnerTreeView.SetItemHeight(Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    UpdateItemHeight;
  end;
end;

procedure TcxCustomInnerTreeView.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxCustomInnerTreeView.WMSetRedraw(var Message: TWMSetRedraw);
begin
  inherited;
  FIsRedrawLocked := Message.Redraw = 0;
  if not (csDestroying in ComponentState) and not FIsRedrawLocked then
    Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.CNNotify(var Message: TWMNotify);
begin
  inherited;
  if Message.NMHdr.code = TVN_DELETEITEM then
    Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.Change(Node: TTreeNode);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.Collapse(Node: TTreeNode);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxCustomInnerTreeView.UpdateItemHeight;
begin
  if HandleAllocated then
    TreeView_SetItemHeight(Handle, ItemHeight);
end;

{ TcxTreeViewContainer }

constructor TcxTreeViewContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InternalInitTreeView;
  InitializeInternalTreeView;
  InnerControl := FInnerTreeView;
end;

destructor TcxTreeViewContainer.Destroy;
begin
  FreeAndNil(FInnerTreeView);
  inherited;
end;

function TcxTreeViewContainer.AllowTouchScrollUIMode: Boolean;
begin
  Result := False;
end;

procedure TcxTreeViewContainer.CorrectAlignControlRect(var R: TRect);
begin
  R := cxRectContent(R, GetBorderExtent);
end;

function TcxTreeViewContainer.DoInnerTreeViewWndProcHandler(var Message: TMessage): Boolean;
begin
  Result := False;
  if Assigned(FOnInnerTreeViewWndProc) then
    FOnInnerTreeViewWndProc(InnerTreeView, Message, Result);
end;

function TcxTreeViewContainer.GetInnerTreeViewClass: TcxInnerTreeViewClass;
begin
  Result := TcxBaseInnerTreeView;
end;

function TcxTreeViewContainer.HandleInnerContolGestures: Boolean;
begin
  Result := True;
end;

procedure TcxTreeViewContainer.InternalInitTreeView;
begin
  FInnerTreeView := GetInnerTreeViewClass.Create(Self);
end;

procedure TcxTreeViewContainer.InitializeInternalTreeView;
begin
  FInnerTreeView.Parent := Self;
  FInnerTreeView.FContainer := Self;
  FInnerTreeView.ParentShowHint := False;
end;

procedure TcxTreeViewContainer.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if FInnerTreeView <> nil then
    FInnerTreeView.RecreateWnd;
end;

function TcxTreeViewContainer.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxTreeViewContainer.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if not Enabled then
    Exit;
{$IFDEF USETCXSCROLLBAR}
  if AScrollBarKind = sbHorizontal then
    FInnerTreeView.HScrollHandler(Self, AScrollCode, AScrollPos)
  else
    FInnerTreeView.VScrollHandler(Self, AScrollCode, AScrollPos);
  SetScrollBarsParameters;
{$ENDIF}
end;

procedure TcxTreeViewContainer.SetReadOnly(Value: Boolean);
begin
  InnerTreeView.ReadOnly := Value;
end;

function TcxTreeViewContainer.UseSystemRightToLeftLayoutForInnerControl: Boolean;
begin
  Result := True;
end;

function TcxTreeViewContainer.GetAutoExpand: Boolean;
begin
  Result := InnerTreeView.AutoExpand;
end;

function TcxTreeViewContainer.GetChangeDelay: Integer;
begin
  Result := InnerTreeView.ChangeDelay;
end;

function TcxTreeViewContainer.GetHideSelection: Boolean;
begin
  Result := InnerTreeView.HideSelection;
end;

function TcxTreeViewContainer.GetReadOnly: Boolean;
begin
  Result := InnerTreeView.ReadOnly;
end;

function TcxTreeViewContainer.GetRightClickSelect: Boolean;
begin
  Result := InnerTreeView.RightClickSelect;
end;

function TcxTreeViewContainer.GetScrollBarBounds(const AScrollBarRect: TRect): TRect;
begin
  if UseRightToLeftScrollBar and not IsWinVistaOrLater then // #Ch WinXp TreeView RTL bug
  begin
    Result := dxMapWindowRect(0, Handle, AScrollBarRect);
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
    Result.Right := AScrollBarRect.Width;
    Result.Bottom := AScrollBarRect.Height;
    if (Result.Left < 0) or (Result.Right > Width) or
      (Result.Top < 0) or (Result.Bottom > Height) then
        Result := cxEmptyRect;
  end
  else
    Result := inherited GetScrollBarBounds(AScrollBarRect);
end;

function TcxTreeViewContainer.GetShowButtons: Boolean;
begin
  Result := InnerTreeView.ShowButtons;
end;

function TcxTreeViewContainer.GetShowLines: Boolean;
begin
  Result := InnerTreeView.ShowLines;
end;

function TcxTreeViewContainer.GetShowRoot: Boolean;
begin
  Result := InnerTreeView.ShowRoot;
end;

function TcxTreeViewContainer.GetStateImages: TCustomImageList;
begin
  Result := InnerTreeView.StateImages;
end;

procedure TcxTreeViewContainer.SetAutoExpand(Value: Boolean);
begin
  InnerTreeView.AutoExpand := Value;
end;

procedure TcxTreeViewContainer.SetChangeDelay(Value: Integer);
begin
  InnerTreeView.ChangeDelay := Value;
end;

procedure TcxTreeViewContainer.SetHideSelection(Value: Boolean);
begin
  InnerTreeView.HideSelection := Value;
end;

procedure TcxTreeViewContainer.SetRightClickSelect(Value: Boolean);
begin
  InnerTreeView.RightClickSelect := Value;
end;

procedure TcxTreeViewContainer.SetShowButtons(Value: Boolean);
begin
  InnerTreeView.ShowButtons := Value;
end;

procedure TcxTreeViewContainer.SetShowLines(Value: Boolean);
begin
  InnerTreeView.ShowLines := Value;
end;

procedure TcxTreeViewContainer.SetShowRoot(Value: Boolean);
begin
  InnerTreeView.ShowRoot := Value;
end;

procedure TcxTreeViewContainer.SetStateImages(Value: TCustomImageList);
begin
  InnerTreeView.StateImages := Value;
end;

{ TcxCustomTreeView }

constructor TcxCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 120;
  Height := 100;
end;

function TcxCustomTreeView.GetInnerTreeView: TcxCustomInnerTreeView;
begin
  Result := TcxCustomInnerTreeView(FInnerTreeView);
end;

function TcxCustomTreeView.GetInnerTreeViewClass: TcxInnerTreeViewClass;
begin
  Result := GetTreeViewClass;
end;

class function TcxCustomTreeView.GetTreeViewClass: TcxCustomInnerTreeViewClass;
begin
  Result := TcxCustomInnerTreeView;
end;

procedure TcxCustomTreeView.InitializeInternalTreeView;
begin
  inherited InitializeInternalTreeView;
  InnerTreeView.AutoSize := False;
  CheckInnerTreeViewAlign;
  InnerTreeView.BorderStyle := bsNone;
  InnerTreeView.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
end;

procedure TcxCustomTreeView.FontChanged;
begin
  inherited FontChanged;
  SetSize;
  InnerTreeView.Invalidate;
end;

function TcxCustomTreeView.IsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

procedure TcxCustomTreeView.DoSetSize;
begin
  if not IsLoading then
    inherited;
end;

procedure TcxCustomTreeView.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    inherited SetReadOnly(Value);
    DataSetChange;
  end;
end;

procedure TcxCustomTreeView.WriteState(Writer: TWriter);
begin
  InnerTreeView.HandleNeeded;
  inherited;
end;

procedure TcxCustomTreeView.CNNotify(var Message: TWMNotify);
begin
  if InnerTreeView <> nil then
  begin
    InnerTreeView.CNNotify(Message);
    Exit;
  end;
  inherited;
end;

function TcxCustomTreeView.AlphaSort(ARecurse: Boolean = True): Boolean;
begin
  Result := InnerTreeView.AlphaSort(ARecurse);
end;

function TcxCustomTreeView.CustomSort(SortProc: TTVCompare;
  Data: Longint; ARecurse: Boolean = True): Boolean;
begin
  Result := InnerTreeView.CustomSort(SortProc, Data, ARecurse);
end;

procedure TcxCustomTreeView.FullCollapse;
begin
  InnerTreeView.FullCollapse;
end;

procedure TcxCustomTreeView.FullExpand;
begin
  InnerTreeView.FullExpand;
end;

function TcxCustomTreeView.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result := InnerTreeView.GetHitTestInfoAt(X - InnerTreeView.Left,
    Y - InnerTreeView.Top);
end;

function TcxCustomTreeView.GetNodeAt(X, Y: Integer): TTreeNode;
begin
  Result := InnerTreeView.GetNodeAt(X - InnerTreeView.Left, Y - InnerTreeView.Top);
end;

function TcxCustomTreeView.IsEditing: Boolean;
begin
  Result := InnerTreeView.IsEditing;
end;

procedure TcxCustomTreeView.LoadFromFile(const FileName: string);
begin
  InnerTreeView.LoadFromFile(FileName);
end;

procedure TcxCustomTreeView.LoadFromStream(Stream: TStream);
begin
  InnerTreeView.LoadFromStream(Stream);
end;

procedure TcxCustomTreeView.SaveToFile(const FileName: string);
begin
  InnerTreeView.SaveToFile(FileName);
end;

procedure TcxCustomTreeView.SaveToStream(Stream: TStream);
begin
  InnerTreeView.SaveToStream(Stream);
end;

procedure TcxCustomTreeView.SetScrollBarsParameters(AIsScrolling: Boolean = False);
begin
  if (InnerTreeView <> nil) and not InnerTreeView.IsRedrawLocked then
    inherited SetScrollBarsParameters(AIsScrolling);
end;

procedure TcxCustomTreeView.Select(Node: TTreeNode; ShiftState: TShiftState = []);
begin
  InnerTreeView.Select(Node, ShiftState);
end;

procedure TcxCustomTreeView.Select(const Nodes: array of TTreeNode);
begin
  InnerTreeView.Select(Nodes);
end;

procedure TcxCustomTreeView.Select(Nodes: TList);
begin
  InnerTreeView.Select(Nodes);
end;

procedure TcxCustomTreeView.Deselect(Node: TTreeNode);
begin
  InnerTreeView.Deselect(Node);
end;

procedure TcxCustomTreeView.Subselect(Node: TTreeNode; Validate: Boolean = False);
begin
  InnerTreeView.Subselect(Node, Validate);
end;

procedure TcxCustomTreeView.ClearSelection(KeepPrimary: Boolean = False);
begin
  InnerTreeView.ClearSelection(KeepPrimary);
end;

function TcxCustomTreeView.GetSelections(AList: TList): TTreeNode;
begin
  Result := InnerTreeView.GetSelections(AList);
end;

function TcxCustomTreeView.FindNextToSelect: TTreeNode;
begin
  Result := InnerTreeView.FindNextToSelect;
end;

function TcxCustomTreeView.GetHotTrack: Boolean;
begin
  Result := InnerTreeView.HotTrack;
end;

function TcxCustomTreeView.GetImages: TCustomImageList;
begin
  Result := InnerTreeView.Images;
end;

function TcxCustomTreeView.GetItemHeight: Integer;
begin
  Result := InnerTreeView.ItemHeight;
end;

function TcxCustomTreeView.GetTreeNodes: TTreeNodes;
begin
  Result := InnerTreeView.Items;
end;

function TcxCustomTreeView.GetIndent: Integer;
begin
  Result := InnerTreeView.Indent;
end;

function TcxCustomTreeView.GetMultiSelect: Boolean;
begin
  Result := InnerTreeView.MultiSelect;
end;

function TcxCustomTreeView.GetMultiSelectStyle: TMultiSelectStyle;
begin
  Result := InnerTreeView.MultiSelectStyle;
end;

function TcxCustomTreeView.GetRowSelect: Boolean;
begin
  Result := InnerTreeView.RowSelect;
end;

function TcxCustomTreeView.GetSortType: TSortType;
begin
  Result := InnerTreeView.SortType;
end;

function TcxCustomTreeView.GetToolTips: Boolean;
begin
  Result := InnerTreeView.ToolTips;
end;

function TcxCustomTreeView.GetTreeViewCanvas: TcxCanvas;
begin
  Result := InnerTreeView.Canvas;
end;

function TcxCustomTreeView.GetOnAddition: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnAddition;
end;

function TcxCustomTreeView.GetOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
begin
  Result := InnerTreeView.OnAdvancedCustomDraw;
end;

function TcxCustomTreeView.GetOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
begin
  Result := InnerTreeView.OnAdvancedCustomDrawItem;
end;

function TcxCustomTreeView.GetOnCancelEdit: TTVChangedEvent;
begin
  Result := InnerTreeView.OnCancelEdit;
end;

function TcxCustomTreeView.GetOnChange: TTVChangedEvent;
begin
  Result := InnerTreeView.OnChange;
end;

function TcxCustomTreeView.GetOnChanging: TTVChangingEvent;
begin
  Result := InnerTreeView.OnChanging;
end;

function TcxCustomTreeView.GetOnCollapsed: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnCollapsed;
end;

function TcxCustomTreeView.GetOnCollapsing: TTVCollapsingEvent;
begin
  Result := InnerTreeView.OnCollapsing;
end;

function TcxCustomTreeView.GetOnCompare: TTVCompareEvent;
begin
  Result := InnerTreeView.OnCompare;
end;

function TcxCustomTreeView.GetOnCustomDraw: TTVCustomDrawEvent;
begin
  Result := InnerTreeView.OnCustomDraw;
end;

function TcxCustomTreeView.GetOnCustomDrawItem: TTVCustomDrawItemEvent;
begin
  Result := InnerTreeView.OnCustomDrawItem;
end;

function TcxCustomTreeView.GetOnDeletion: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnDeletion;
end;

function TcxCustomTreeView.GetOnEditing: TTVEditingEvent;
begin
  Result := InnerTreeView.OnEditing;
end;

function TcxCustomTreeView.GetOnEdited: TTVEditedEvent;
begin
  Result := InnerTreeView.OnEdited;
end;

function TcxCustomTreeView.GetOnExpanding: TTVExpandingEvent;
begin
  Result := InnerTreeView.OnExpanding;
end;

function TcxCustomTreeView.GetOnExpanded: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnExpanded;
end;

function TcxCustomTreeView.GetOnGetImageIndex: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnGetImageIndex;
end;

function TcxCustomTreeView.GetOnGetSelectedIndex: TTVExpandedEvent;
begin
  Result := InnerTreeView.OnGetSelectedIndex;
end;

function TcxCustomTreeView.GetOnCreateNodeClass: TTVCreateNodeClassEvent;
begin
  Result := InnerTreeView.OnCreateNodeClass;
end;

function TcxCustomTreeView.GetDropTarget: TTreeNode;
begin
  Result := InnerTreeView.DropTarget;
end;

function TcxCustomTreeView.GetSelected: TTreeNode;
begin
  Result := InnerTreeView.Selected;
end;

function TcxCustomTreeView.GetTopItem: TTreeNode;
begin
  Result := InnerTreeView.TopItem;
end;

function TcxCustomTreeView.GetSelectionCount: Cardinal;
begin
  Result := InnerTreeView.SelectionCount;
end;

function TcxCustomTreeView.GetSelection(Index: Integer): TTreeNode;
begin
  Result := InnerTreeView.Selections[Index];
end;

procedure TcxCustomTreeView.SetHotTrack(Value: Boolean);
begin
  InnerTreeView.HotTrack := Value;
end;

procedure TcxCustomTreeView.SetImages(Value: TCustomImageList);
begin
  InnerTreeView.Images := Value;
end;

procedure TcxCustomTreeView.SetTreeNodes(Value: TTreeNodes);
begin
  InnerTreeView.Items := Value;
end;

procedure TcxCustomTreeView.SetIndent(Value: Integer);
begin
  InnerTreeView.Indent := Value;
end;

procedure TcxCustomTreeView.SetItemHeight(Value: Integer);
begin
  InnerTreeView.ItemHeight := Value;
end;

procedure TcxCustomTreeView.SetMultiSelect(Value: Boolean);
begin
  InnerTreeView.MultiSelect := Value;
end;

procedure TcxCustomTreeView.SetMultiSelectStyle(Value: TMultiSelectStyle);
begin
  InnerTreeView.MultiSelectStyle := Value;
end;

procedure TcxCustomTreeView.SetRowSelect(Value: Boolean);
begin
  InnerTreeView.RowSelect := Value;
end;

procedure TcxCustomTreeView.SetSortType(Value: TSortType);
begin
  InnerTreeView.SortType := Value;
end;

procedure TcxCustomTreeView.SetToolTips(Value: Boolean);
begin
  InnerTreeView.ToolTips := Value;
end;

procedure TcxCustomTreeView.SetOnAddition(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnAddition := Value;
end;

procedure TcxCustomTreeView.SetOnAdvancedCustomDraw(Value: TTVAdvancedCustomDrawEvent);
begin
  InnerTreeView.OnAdvancedCustomDraw := Value;
end;

procedure TcxCustomTreeView.SetOnAdvancedCustomDrawItem(Value: TTVAdvancedCustomDrawItemEvent);
begin
  InnerTreeView.OnAdvancedCustomDrawItem := Value;
end;

procedure TcxCustomTreeView.SetOnCancelEdit(Value: TTVChangedEvent);
begin
  InnerTreeView.OnCancelEdit := Value;
end;

procedure TcxCustomTreeView.SetOnChange(Value: TTVChangedEvent);
begin
  InnerTreeView.OnChange := Value;
end;

procedure TcxCustomTreeView.SetOnChanging(Value: TTVChangingEvent);
begin
  InnerTreeView.OnChanging := Value;
end;

procedure TcxCustomTreeView.SetOnCollapsed(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnCollapsed := Value;
end;

procedure TcxCustomTreeView.SetOnCollapsing(Value: TTVCollapsingEvent);
begin
  InnerTreeView.OnCollapsing := Value;
end;

procedure TcxCustomTreeView.SetOnCompare(Value: TTVCompareEvent);
begin
  InnerTreeView.OnCompare := Value;
end;

procedure TcxCustomTreeView.SetOnCustomDraw(Value: TTVCustomDrawEvent);
begin
  InnerTreeView.OnCustomDraw := Value;
end;

procedure TcxCustomTreeView.SetOnCustomDrawItem(Value: TTVCustomDrawItemEvent);
begin
  InnerTreeView.OnCustomDrawItem := Value;
end;

procedure TcxCustomTreeView.SetOnDeletion(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnDeletion := Value;
end;

procedure TcxCustomTreeView.SetOnEditing(Value: TTVEditingEvent);
begin
  InnerTreeView.OnEditing := Value;
end;

procedure TcxCustomTreeView.SetOnEdited(Value: TTVEditedEvent);
begin
  InnerTreeView.OnEdited := Value;
end;

procedure TcxCustomTreeView.SetOnExpanding(Value: TTVExpandingEvent);
begin
  InnerTreeView.OnExpanding := Value;
end;

procedure TcxCustomTreeView.SetOnExpanded(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnExpanded := Value;
end;

procedure TcxCustomTreeView.SetOnGetImageIndex(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnGetImageIndex := Value;
end;

procedure TcxCustomTreeView.SetOnGetSelectedIndex(Value: TTVExpandedEvent);
begin
  InnerTreeView.OnGetSelectedIndex := Value;
end;

procedure TcxCustomTreeView.SetOnCreateNodeClass(Value: TTVCreateNodeClassEvent);
begin
  InnerTreeView.OnCreateNodeClass := Value;
end;

procedure TcxCustomTreeView.SetDropTarget(Value: TTreeNode);
begin
  InnerTreeView.DropTarget := Value;
end;

procedure TcxCustomTreeView.SetSelected(Value: TTreeNode);
begin
  InnerTreeView.Selected := Value;
end;

procedure TcxCustomTreeView.SetTopItem(Value: TTreeNode);
begin
  InnerTreeView.TopItem := Value;
end;

function TcxCustomTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := InnerTreeView.CanEdit(Node);
end;

function TcxCustomTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := InnerTreeView.CanChange(Node);
end;

function TcxCustomTreeView.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := InnerTreeView.CanCollapse(Node);
end;

function TcxCustomTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := InnerTreeView.CanExpand(Node);
end;

procedure TcxCustomTreeView.CheckInnerTreeViewAlign;
begin
  if IsTouchScrollUIMode then
    InnerTreeView.Align := alNone
  else
    InnerTreeView.Align := alClient;
end;

procedure TcxCustomTreeView.Collapse(Node: TTreeNode);
begin
  InnerTreeView.Collapse(Node);
end;

procedure TcxCustomTreeView.DoScrollUIModeChanged;
begin
  CheckInnerTreeViewAlign;
end;

procedure TcxCustomTreeView.Expand(Node: TTreeNode);
begin
  InnerTreeView.Expand(Node);
end;

end.
