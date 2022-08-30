{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl common routines                     }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutSelection;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Graphics, Classes, Controls, cxClasses, cxGraphics,
  StdCtrls, dxCoreClasses, cxControls, Contnrs;

const
  dxLayoutSelectionBackgroundColor: TColor = $582801;
  dxLayoutSelectionBorderColor: TColor = $BD8753;
  dxLayoutSelectionAlphaChannel: Byte = 80;
  dxLayoutSelectionBorderMarkerBackgroundColor: TColor = clWhite;

type
  TdxSelectionAction = (saAdded, saChanged, saExtracted);

  TdxSelectionLayer = class;

  { IdxSelectionChanged }

  IdxSelectionChanged = interface
  ['{ECB1A3AE-1C91-4E5F-8ADC-34120676B1CA}']
    procedure SelectionChanged(ASelection: TList; AAction: TdxSelectionAction);
  end;

  { IdxLayoutSelectableItem }

  IdxLayoutSelectableItem = interface
  ['{2887168D-78EB-44A0-A440-E284B01BE407}']
    function CanDelete: Boolean;
    function IsOwner(AOwner: TComponent): Boolean;
    procedure MakeVisible;
    procedure SelectComponent(AShift: TShiftState = []);
    procedure SelectParent;
    procedure SelectionChanged;
    function IsVisible: Boolean;
  end;

  { IdxLayoutDesignerHelper }

  IdxLayoutDesignerHelper = interface
  ['{0FF95B61-5074-49E4-99C6-1A8BB34F1547}']
    procedure AddSelectionChangedListener(AListener: TPersistent);
    function IsActive: Boolean;
    function CanDeleteComponent(AComponent: TComponent): Boolean;
    function CanModify: Boolean;
    function CanProcessKeyboard: Boolean;
    procedure ClearSelection;
    procedure DeleteSelection;
    procedure DeleteComponent(AComponent: TComponent);
    procedure DeleteComponents(AList: TComponentList);
    procedure GetSelection(AList: TList);
    function IsComponentSelected(AComponent: TPersistent): Boolean;
    procedure RemoveSelectionChangedListener(AListener: TPersistent);
    procedure SelectComponent(AComponent: TPersistent; AShift: TShiftState = []);
    procedure SetSelection(AList: TList);
    function UniqueName(const BaseName: string): string;
  end;

  { TdxLayoutRunTimeSelectionHelper }

  TdxLayoutRunTimeSelectionHelper = class(TcxOwnedPersistent, IdxLayoutDesignerHelper)
  private
    FListeners: TList;
    FRefCount: Integer;
    FSelectionList: TcxComponentList;
    function GetComponent: TComponent;
    procedure SelectionListNotifyHandler(Sender: TObject; AComponent: TComponent; AAction: TListNotification);
    procedure SelectionListChangedHandler(Sender: TObject; AComponent: TComponent; AAction: TcxComponentCollectionNotification);
  protected
    function GetComponentClass(AComponent: TComponent): TComponentClass;
    procedure NotifyListeners(AList: TList; AAction: TdxSelectionAction);
    procedure SelectionListNotify(AComponent: TComponent; AAction: TListNotification);

    //IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property Component: TComponent read GetComponent;
    property Listeners: TList read FListeners;
    property SelectionList: TcxComponentList read FSelectionList;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    // IdxLayoutControlSelectionController
    procedure AddSelectionChangedListener(AListener: TPersistent); virtual;
    procedure RemoveSelectionChangedListener(AListener: TPersistent); virtual;

    //IcxLayoutControlDesignerHelper
    function IsActive: Boolean; virtual;
    function CanDeleteComponent(AComponent: TComponent): Boolean; virtual;
    function CanModify: Boolean; virtual;
    function CanProcessKeyboard: Boolean; virtual;
    procedure ClearSelection; virtual;
    procedure DeleteComponent(AComponent: TComponent); virtual;
    procedure DeleteComponents(AList: TComponentList); virtual;
    procedure DeleteSelection; virtual;
    procedure GetSelection(AList: TList); virtual;
    function IsComponentSelected(AComponent: TPersistent): Boolean; virtual;
    procedure SelectComponent(AComponent: TPersistent; AShift: TShiftState = []); virtual;
    procedure SetSelection(AList: TList); virtual;
    function UniqueName(const BaseName: string): string; virtual;
  end;

  TdxLayoutRunTimeSelectionHelperClass = class of TdxLayoutRunTimeSelectionHelper;

  TdxSelectionLayerInplaceEdit = class(TEdit)
  private
    FClickTime: Longint;
    FHiding: Boolean;
    FSelectionLayer: TdxSelectionLayer;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message: TMessage); override;

    property SelectionLayer: TdxSelectionLayer read FSelectionLayer;
  public
    constructor Create(ASelectionLayer: TdxSelectionLayer); reintroduce; virtual;
    procedure Hide;
    procedure SetFocus; override;
    procedure Show(const ABounds: TRect; const AText: string);
  end;

  { TdxSelectionLayer }

  TdxSelectionLayerHitTestEvent = procedure(ASender: TObject; var AIsTransparent: Boolean) of object;
  TdxSelectionLayerEndRenameEvent = procedure(ASender: TObject; const AText: string; AAccept: Boolean) of object;

  TdxSelectionLayer = class(TCustomControl)
  private
    FEditorMode: Boolean;
    FInplaceEditBounds: TRect;
    FInplaceEdit: TdxSelectionLayerInplaceEdit;
    FParentControl: TcxControl;
    FStartOffset: TPoint;
    FSelectionImage: TcxAlphaBitmap;
    FcxCanvas: TcxCanvas;

    FOnEndRename: TdxSelectionLayerEndRenameEvent;
    FOnHide: TNotifyEvent;
    FOnHitTest: TdxSelectionLayerHitTestEvent;
    FOnShow: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    procedure SetParentControl(AValue: TcxControl);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;

    procedure CheckInplaceEditorPosition;
    procedure DoHide; virtual;
    function DoHitTest: Boolean; virtual;
    procedure DoShow; virtual;
    procedure DoUpdate; virtual;
    procedure DoEndRename(const AText: string; AAccept: Boolean); virtual;
    function GetParentOffset: TPoint;

    property cxCanvas: TcxCanvas read FcxCanvas;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AParentControl: TcxControl; AParentWindow: HWND); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure InvalidateRect(const R: TRect);
    procedure MoveTo(const P: TPoint);
    procedure Hide;
    procedure Show;
    procedure UpdateContent;

    procedure BeginRename(const ABounds: TRect; const AText: string; const AFont: TFont);
    procedure EndRename(AAccept: Boolean);

    property EditorMode: Boolean read FEditorMode;
    property SelectionImage: TcxAlphaBitmap read FSelectionImage;
    property ParentControl: TcxControl read FParentControl write SetParentControl;
    property OnEndRename: TdxSelectionLayerEndRenameEvent read FOnEndRename write FOnEndRename;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnHitTest: TdxSelectionLayerHitTestEvent read FOnHitTest write FOnHitTest;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses
  SysUtils, Types, cxGeometry, Forms, dxOffice11, dxLayoutContainer;

type
  TcxControlAccess = class(TcxControl);

{ TdxLayoutControlRunTimeSelectionHelper }

constructor TdxLayoutRunTimeSelectionHelper.Create(AOwner: TPersistent);
begin
  inherited;
  FListeners := TList.Create;
  FSelectionList := TcxComponentList.Create;
  SelectionList.OnNotify := SelectionListNotifyHandler;
  SelectionList.OnComponentListChanged := SelectionListChangedHandler;
end;

destructor TdxLayoutRunTimeSelectionHelper.Destroy;
begin
  FreeAndNil(FSelectionList);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TdxLayoutRunTimeSelectionHelper.AddSelectionChangedListener(AListener: TPersistent);
begin
  if (AListener <> nil) and Supports(AListener, IdxSelectionChanged) and
      (Listeners.IndexOf(AListener) < 0) then
    Listeners.Add(AListener);
end;

procedure TdxLayoutRunTimeSelectionHelper.RemoveSelectionChangedListener(AListener: TPersistent);
begin
  Listeners.Remove(AListener);
end;

function TdxLayoutRunTimeSelectionHelper.IsActive: Boolean;
begin
  Result := True;
end;

function TdxLayoutRunTimeSelectionHelper.CanDeleteComponent(AComponent: TComponent): Boolean;
var
  ASelectableItem: IdxLayoutSelectableItem;
begin
  Result := not (csDestroying in AComponent.ComponentState) and
    Supports(AComponent, IdxLayoutSelectableItem, ASelectableItem) and
    ASelectableItem.CanDelete;
end;

function TdxLayoutRunTimeSelectionHelper.CanModify: Boolean;
begin
  Result := True;
end;

function TdxLayoutRunTimeSelectionHelper.CanProcessKeyboard: Boolean;
begin
  Result := False;
end;

procedure TdxLayoutRunTimeSelectionHelper.ClearSelection;
begin
  SelectionList.Clear;
end;

procedure TdxLayoutRunTimeSelectionHelper.DeleteComponent(AComponent: TComponent);
begin
  AComponent.Free;
end;

procedure TdxLayoutRunTimeSelectionHelper.DeleteComponents(AList: TComponentList);

  procedure CheckDeleteItems(AList: TObjectList);
  var
    I: Integer;
    AIntf: IdxLayoutSelectableItem;
  begin
    for I := AList.Count - 1 downto 0 do
      if not Supports(AList[I], IdxLayoutSelectableItem, AIntf) or not AIntf.CanDelete then
        AList.Extract(AList[I]);
  end;


begin
  CheckDeleteItems(AList);
  while AList.Count > 0 do
  begin
      DeleteComponent(AList[0]);
  end;
end;

procedure TdxLayoutRunTimeSelectionHelper.DeleteSelection;
var
  AList: TcxComponentList;
begin
  AList := TcxComponentList.Create(True);
  try
    GetSelection(AList);
    DeleteComponents(AList);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutRunTimeSelectionHelper.GetSelection(AList: TList);
var
  I: Integer;
begin
  for I := 0 to SelectionList.Count - 1 do
    AList.Add(SelectionList[I]);
end;

function TdxLayoutRunTimeSelectionHelper.IsComponentSelected(AComponent: TPersistent): Boolean;
begin
  Result := SelectionList.IndexOf(TComponent(AComponent)) <> -1;
end;

procedure TdxLayoutRunTimeSelectionHelper.SelectComponent(
  AComponent: TPersistent; AShift: TShiftState);
begin
  SelectionList.BeginUpdate;
  try
    if (ssCtrl in AShift) and IsComponentSelected(AComponent) then
      SelectionList.Delete(SelectionList.IndexOf(TComponent(AComponent)))
    else
    begin
      if [ssCtrl, ssShift] * AShift = [] then
        ClearSelection;
      if not IsComponentSelected(AComponent) then
        SelectionList.Add(TComponent(AComponent));
    end;
  finally
    SelectionList.EndUpdate;
  end;
end;

procedure TdxLayoutRunTimeSelectionHelper.SetSelection(AList: TList);
var
  I: Integer;
begin
  SelectionList.BeginUpdate;
  try
    ClearSelection;
    for I := 0 to AList.Count - 1 do
      if SelectionList.IndexOf(AList[I]) = -1 then
        SelectionList.Add(AList[I]);
  finally
    SelectionList.EndUpdate;
  end;
end;

function TdxLayoutRunTimeSelectionHelper.UniqueName(const BaseName: string): string;
begin
  Result := '';
end;

function TdxLayoutRunTimeSelectionHelper.GetComponentClass(AComponent: TComponent): TComponentClass;
begin
  Result := TComponentClass(AComponent.ClassType);
end;

procedure TdxLayoutRunTimeSelectionHelper.NotifyListeners(AList: TList; AAction: TdxSelectionAction);
var
  I: Integer;
  AIntf: IdxSelectionChanged;
begin
  for I := 0 to Listeners.Count - 1 do
    if Supports(TObject(Listeners[I]), IdxSelectionChanged, AIntf) then
    begin
      AIntf.SelectionChanged(AList, AAction);
      AIntf := nil;
    end;
end;

procedure TdxLayoutRunTimeSelectionHelper.SelectionListNotify(AComponent: TComponent;
  AAction: TListNotification);
var
  ASelectableItem: IdxLayoutSelectableItem;
begin
  if not (csDestroying in AComponent.ComponentState) and
      Supports(AComponent, IdxLayoutSelectableItem, ASelectableItem) then
    ASelectableItem.SelectionChanged;
end;

function TdxLayoutRunTimeSelectionHelper.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TdxLayoutRunTimeSelectionHelper._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TdxLayoutRunTimeSelectionHelper._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TdxLayoutRunTimeSelectionHelper.GetComponent: TComponent;
begin
  Result := TComponent(Owner);
end;

procedure TdxLayoutRunTimeSelectionHelper.SelectionListNotifyHandler(Sender: TObject;
  AComponent: TComponent; AAction: TListNotification);
begin
  SelectionListNotify(AComponent, AAction);
end;

procedure TdxLayoutRunTimeSelectionHelper.SelectionListChangedHandler(Sender: TObject;
  AComponent: TComponent; AAction: TcxComponentCollectionNotification);
const
  Action: array[TcxComponentCollectionNotification] of TdxSelectionAction = (saAdded, saChanged, saExtracted, saExtracted, saExtracted);
var
  AList: TList;
begin
  if AAction in [ccnAdded, ccnChanged, ccnExtracted] then
  begin
    AList := TList.Create;
    try
      GetSelection(AList);
      NotifyListeners(AList, Action[AAction]);
    finally
      AList.Free;
    end;
  end;
end;

{ TdxLayoutControlInplaceEdit }

constructor TdxSelectionLayerInplaceEdit.Create(
  ASelectionLayer: TdxSelectionLayer);
begin
  inherited Create(nil);
  FSelectionLayer := ASelectionLayer;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  BorderWidth := 1;
  DoubleBuffered := False;
  Parent := ASelectionLayer;
end;

procedure TdxSelectionLayerInplaceEdit.CMShowingChanged(var Message: TMessage);
begin
// do nothing
end;

procedure TdxSelectionLayerInplaceEdit.DblClick;
begin
  SelectionLayer.DblClick;
end;

function TdxSelectionLayerInplaceEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := SelectionLayer.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TdxSelectionLayerInplaceEdit.Hide;
begin
  FHiding := True;
  try
    if HandleAllocated and IsWindowVisible(Handle) then
    begin
      Invalidate;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
        SWP_NOREDRAW);
      if Focused then
        Windows.SetFocus(SelectionLayer.Handle);
    end;
  finally
    FHiding := False;
  end;
end;

procedure TdxSelectionLayerInplaceEdit.SetFocus;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

procedure TdxSelectionLayerInplaceEdit.Show(const ABounds: TRect; const AText: string);
begin
  Text := AText;
  BoundsRect := ABounds;
  ShowWindow(Handle, SW_SHOWNORMAL);
  SetFocus;
  SelectAll;
end;

procedure TdxSelectionLayerInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    SelectionLayer.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    ASelectionLayerKeyDown: TKeyEvent;
  begin
    ASelectionLayerKeyDown := SelectionLayer.OnKeyDown;
    if Assigned(ASelectionLayerKeyDown) then
      ASelectionLayerKeyDown(SelectionLayer, Key, Shift);
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

begin
  case Key of
    VK_INSERT:
      if Shift = [] then
        SendToParent;
    VK_F2:
      SelectionLayer.EndRename(True);
    VK_DELETE:
      if Ctrl then
        SendToParent;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TdxSelectionLayerInplaceEdit.KeyPress(var Key: Char);
begin
  case Key of
    #9, #13, #27:
      begin
        SelectionLayer.EndRename(Key <> #27);
        Key := #0;
      end;
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TdxSelectionLayerInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  SelectionLayer.KeyUp(Key, Shift);
end;

procedure TdxSelectionLayerInplaceEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TdxSelectionLayerInplaceEdit.WMPaint(var Message: TWMPaint);

  procedure DrawBorders(DC: HDC);
  var
    R: TRect;
  begin
    R := ClientRect;
    R.Right := R.Right + 2 * BorderWidth;
    R.Bottom := R.Bottom + 2 * BorderWidth;
    FrameRectByColor(DC, R, dxLayoutSelectionBorderColor);
  end;

var
  ADC: HDC;
begin
  inherited;

  if Message.DC <> 0 then
    DrawBorders(Message.DC)
  else
  begin
    ADC := GetWindowDC(Handle);
    DrawBorders(ADC);
    ReleaseDC(Handle, ADC);
  end;
end;

procedure TdxSelectionLayerInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(SelectionLayer) then
          Dispatch(Message);
        Exit;
      end;
    WM_KILLFOCUS:
      if not FHiding then
        SelectionLayer.EndRename(not (csDestroying in ComponentState));
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

{ TdxSelectionLayer }

constructor TdxSelectionLayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FSelectionImage := TcxAlphaBitmap.Create;
  FcxCanvas := TcxCanvas.Create(Canvas);
  Visible := False;
end;

constructor TdxSelectionLayer.Create(AParentControl: TcxControl; AParentWindow: HWND);
begin
  CreateParented(AParentWindow);
  ParentControl := AParentControl;
end;

destructor TdxSelectionLayer.Destroy;
begin
  Hide;
  FreeAndNil(FInplaceEdit);
  FreeAndNil(FcxCanvas);
  FreeAndNil(FSelectionImage);
  inherited Destroy;
end;

procedure TdxSelectionLayer.Paint;

  procedure DrawParentControl(AParent: TWinControl; ABitmap: TcxBitmap32);
  var
    AOffset: TPoint;
    AList: TList;
    I: Integer;
    ARect: TRect;
  begin
    AOffset := cxGetClientOffset(AParent.Handle);
    AList := TList.Create;
    try
      AList.Add(Pointer(Handle));
      cxPaintTo(AParent, ABitmap.cxCanvas, cxPointInvert(AOffset), BoundsRect, AList);
      AList.Clear;
      (Owner as IdxLayoutDesignerHelper).GetSelection(AList);
      if AList.Count > 1 then
        for I := 0 to AList.Count - 1 do
          if not Supports(TObject(AList[I]), IdxLayoutSelectableItem) and (TObject(AList[I]) is TWinControl) and (IsParent(AParent, TWinControl(AList[I])) or (AList[I] = AParent)) then
          begin
            ARect := cxGetWindowBounds(TWinControl(AList[I]));
            if AList[I] <> AParent then
              ARect := cxRectOffset(ARect, TWinControl(AList[I]).ClientToParent(Point(0, 0), AParent));
            ABitmap.cxCanvas.FillRect(cxRectBounds(ARect.Left, ARect.Top, 4, 4), clBlack);
            ABitmap.cxCanvas.FillRect(cxRectBounds(ARect.Right - 4, ARect.Top, 4, 4), clBlack);
            ABitmap.cxCanvas.FillRect(cxRectBounds(ARect.Left, ARect.Bottom - 4, 4, 4), clBlack);
            ABitmap.cxCanvas.FillRect(cxRectBounds(ARect.Right - 4, ARect.Bottom - 4, 4, 4), clBlack);
          end;
    finally
      AList.Free;
    end;
  end;

var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(ClientRect);
  try
    if ParentControl <> nil then
      DrawParentControl(ParentControl, ABitmap)
    else
      DrawParentControl(Parent, ABitmap);

    cxAlphaBlend(ABitmap.cxCanvas.Handle, SelectionImage.cxCanvas.Handle, ClientRect, ClientRect);
    cxBitBlt(cxCanvas.Handle, ABitmap.cxCanvas.Handle, ClientRect, cxNullPoint, SRCCOPY);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxSelectionLayer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SelectionImage.SetSize(Width, Height);
end;

procedure TdxSelectionLayer.InvalidateRect(const R: TRect);
begin
  if HandleAllocated then
    cxInvalidateRect(Handle, R, False);
end;

procedure TdxSelectionLayer.MoveTo(const P: TPoint);
begin
  SetBounds(P.X, P.Y, Width, Height);
end;

procedure TdxSelectionLayer.Hide;
begin
  EndRename(False);
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    ShowWindow(Handle, SW_HIDE);
    DoHide;
  end;
end;

procedure TdxSelectionLayer.Show;
begin
  if not IsWindowVisible(Handle) then
  begin
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    DoShow;
  end;
end;

procedure TdxSelectionLayer.BeginRename(const ABounds: TRect; const AText: string; const AFont: TFont);
begin
  FEditorMode := True;
  if FInplaceEdit = nil then
    FInplaceEdit := TdxSelectionLayerInplaceEdit.Create(Self);
  FInplaceEdit.Font := AFont;
  FInplaceEditBounds := ABounds;
  FStartOffset := GetParentOffset;
  FInplaceEdit.Show(ABounds, AText);
end;

procedure TdxSelectionLayer.EndRename(AAccept: Boolean);
begin
  FEditorMode := False;
  if FInplaceEdit <> nil then
  try
    DoEndRename(FInplaceEdit.Text, AAccept);
  finally
    FInplaceEdit.Hide;
  end;
  Invalidate;
end;

procedure TdxSelectionLayer.UpdateContent;
begin
  SelectionImage.RecoverAlphaChannel(0);
  Show;
  DoUpdate;
  CheckInplaceEditorPosition;
  Invalidate;
end;

procedure TdxSelectionLayer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not WS_POPUP or WS_CHILD;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
  if ParentControl <> nil then
    Params.WndParent := ParentControl.Handle;
end;

procedure TdxSelectionLayer.WndProc(var Message: TMessage);

  function GetParent: TWinControl;
  begin
    if ParentControl <> nil then
      Result := ParentControl
    else
      Result := Parent;
  end;

var
  AOriginal: WPARAM;
  AParent: TWinControl;
begin
  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST:
      begin
        AParent := GetParent;
        if AParent <> nil then
        begin
          TWMMouse(Message).Pos := PointToSmallPoint(cxPointOffset(SmallPointToPoint(TWMMouse(Message).Pos), BoundsRect.TopLeft));
          Message.Result := AParent.Perform(Message.Msg, Message.WParam, Message.LParam);
        end
        else
          inherited;
      end;
    WM_SETCURSOR:
      begin
        AParent := GetParent;
        if AParent <> nil then
        begin
          AOriginal := Message.WParam;
          Message.WParam := AParent.Handle;
          Message.Result := AParent.Perform(Message.Msg, Message.WParam, Message.LParam);
          Message.WParam := AOriginal;
        end
        else
          inherited;
      end;
  else
    inherited;
  end;
end;

procedure TdxSelectionLayer.CheckInplaceEditorPosition;
var
  R: TRect;
  P: TPoint;
begin
  if not EditorMode then Exit;
  R := FInplaceEditBounds;
  P := GetParentOffset;
  OffsetRect(R, FStartOffset.X - P.X, 0);
  OffsetRect(R, 0, FStartOffset.Y - P.Y);
  MoveWindow(FInplaceEdit.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, False);
end;

procedure TdxSelectionLayer.DoHide;
begin
  CallNotify(FOnHide, Self);
end;

function TdxSelectionLayer.DoHitTest: Boolean;
begin
  Result := False;
  if Assigned(FOnHitTest) then
    FOnHitTest(Self, Result);
end;

procedure TdxSelectionLayer.DoShow;
begin
  CallNotify(FOnShow, Self);
end;

procedure TdxSelectionLayer.DoUpdate;
begin
  CallNotify(FOnUpdate, Self);
end;

procedure TdxSelectionLayer.DoEndRename(const AText: string; AAccept: Boolean);
begin
  if Assigned(FOnEndRename) then
    FOnEndRename(Self, AText, AAccept);
end;

function TdxSelectionLayer.GetParentOffset: TPoint;
begin
  Result := cxNullPoint;
  with TcxControlAccess(ParentControl) do
  begin
    if HScrollBarVisible then
      Result.X := HScrollBar.Position;
    if VScrollBarVisible then
      Result.Y := VScrollBar.Position;
  end;
end;

procedure TdxSelectionLayer.SetParentControl(AValue: TcxControl);
begin
  if FParentControl <> AValue then
  begin
    FParentControl := AValue;
    RecreateWnd;
  end;
end;

procedure TdxSelectionLayer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxSelectionLayer.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if DoHitTest then
    Message.Result := HTTRANSPARENT
  else
    inherited;
end;

end.
