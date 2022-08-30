{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Platform.Win.Control;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, StdCtrls,

  dxCore, dxCoreClasses, cxGeometry, dxMessages, cxGraphics, cxScrollBar,
  dxTouch, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, cxControls,
  dxRichEdit.Utils.Graphics;

type
  { TdxVCLControl }

  TdxVCLControl = class(TcxCustomControl,
    IcxLookAndFeelContainer,
    IdxSkinSupport,
    IdxLocalizerListener,
    IdxSystemInfoListener,
    IdxGestureClient,
    IdxGestureOwner,
    IdxZoomClient)
  private
    FActiveCanvas: TcxCanvas;
    FActivateType: TcxControlActivateType;
    FBorderStyle: TcxControlBorderStyle;
    FCanvas: TcxCanvas;
    FGraphics: TdxGraphics;
    FCreatingWindow: Boolean;
    FFocusOnClick: Boolean;
    FFontListenerList: IInterfaceList;
    FGestureHelper: TdxGestureHelper;
    FIsPopupMenuShown: Boolean;
    FInternalScaleFactor: TdxScaleFactor;
    FKeys: TcxKeys;
    FLookAndFeel: TcxLookAndFeel;
    FMouseButtonPressed: Boolean;
    FMouseCaptureObject: TObject;
    FMouseDownPos: TPoint;
    FMouseRightButtonReleased: Boolean;
    FPopupMenu: TComponent;
    FIsScrollingContent: Boolean;
    FOnFocusChanged: TNotifyEvent;

    function GetActiveCanvas: TcxCanvas;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    procedure SetBorderStyle(Value: TcxControlBorderStyle);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetKeys(Value: TcxKeys);
    procedure SetMouseCaptureObject(Value: TObject);
    procedure SetPopupMenu(Value: TComponent);
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure StandardPaintHandler(var Message: TWMPaint);
    procedure FullBufferedPaint(var Message: TWMPaint);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure CMNCSizeChanged(var Message: TMessage); message DXM_NCSIZECHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyDown); message CN_KEYUP;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
    procedure CreateScrollBars;
    procedure DestroyScrollBars;
  protected
    FBounds: TRect;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function IsNeedFullBufferedPaint: Boolean; virtual;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DestroyWindowHandle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures;
      var Options: TInteractiveGestureOptions); override;
    function GetDefaultInteractiveGestures: TInteractiveGestures; virtual;
    function GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions; virtual;
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    function CanProcessScrollEvents: Boolean; virtual;
    function IsDefaultGesture(AGestureID: Integer): Boolean; virtual;
    function IsGestureHelperMessage(var Message: TMessage): Boolean; virtual;
    function IsGestureScrolling: Boolean; virtual;
    function GetDefaultPanOptions: Integer; virtual;
    procedure ScrollContentByGesture(ADeltaX, ADeltaY: Integer); virtual;

    function DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean; virtual;
    function GetPopupMenu: TPopupMenu; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; virtual;
    function IsDoubleBufferedNeeded: Boolean; virtual;
    function IsMenuKey(var Message: TWMKey): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParentBackground(Value: Boolean); override;

    procedure Paint; override;
    procedure PaintNonClientArea(ACanvas: TcxCanvas); virtual;
    procedure PaintWindow(DC: HDC); override;

    procedure BringInternalControlsToFront; virtual;
    procedure CancelMouseOperations; virtual;
    procedure DoCancelMode; dynamic;
    procedure DoPaint; virtual;
    procedure DrawBorder(ACanvas: TcxCanvas); virtual;
    function GetBorderSize: Integer; virtual;
    function GetBounds: TRect; virtual;
    function GetClientBounds: TRect; virtual;
    function GetClientOffsets: TRect; virtual;
    function GetCurrentCursor(X, Y: Integer): TCursor; virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; dynamic;
    function GetIsDesigning: Boolean; virtual;
    function GetIsFocused: Boolean; virtual;
    function GetMouseCursorClientPos: TPoint;
    function GetPaintBlackOpaqueOnGlass: Boolean; virtual;
    function GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass; virtual;
    function GetSizeGripBounds: TRect; virtual;
    procedure InitializeControl; virtual;
    procedure FinalizeControl; virtual;
    class procedure InvalidateControl(AControl: TWinControl; ANeedInvalidateSelf, ANeedInvalidateChildren: Boolean);
    procedure FocusEnter; virtual;
    procedure FocusLeave; virtual;
    procedure SetPaintRegion; virtual;
    procedure UpdateStatusHint(const APoint: TPoint); virtual;

    function AllowCompositionPainting: Boolean; virtual;
    function CanFocusOnClick: Boolean; overload; virtual;
    function CanFocusOnClick(X, Y: Integer): Boolean; overload; virtual;
    function FocusWhenChildIsClicked(AChild: TControl): Boolean; virtual;
    function MayFocus: Boolean; dynamic;
    function NeedRedrawOnResize: Boolean; virtual;
    function UpdateMousePositionIfControlMoved: Boolean; virtual;

    procedure BoundsChanged; virtual;
    procedure ParentBackgroundChanged; virtual;
    procedure EnabledChanged; virtual;
    procedure FocusChanged; virtual;
    procedure FontChanged; virtual;
    procedure TextChanged; virtual;
    procedure VisibleChanged; virtual;

    // IdxGestureClient
    function AllowGesture(AGestureId: Integer): Boolean; virtual;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; virtual;
    procedure BeginGestureScroll(APos: TPoint); virtual;
    procedure EndGestureScroll; virtual;
    function GetPanOptions: Integer; virtual;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function IsPanArea(const APoint: TPoint): Boolean; virtual;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; virtual;
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; virtual;
    function IdxGestureOwner.GetHandle = GetGestureClientHandle;
    function GetGestureClientHandle: THandle; virtual;
    function IsGestureTarget(AWnd: THandle): Boolean; virtual;
    // IdxZoomClient
    procedure Zoom(ADelta: Integer; var AHandled: Boolean); virtual;
    // IcxLookAndFeelContainer
    function IcxLookAndFeelContainer.GetLookAndFeel = GetLookAndFeelValue;
    function GetLookAndFeelValue: TcxLookAndFeel; virtual;
    // IcxScrollBarOwner
    function GetControl: TWinControl;
    function GetScrollBarLookAndFeel: TcxLookAndFeel; virtual;

    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual;
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues);
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure ScaleFactorChanged; override;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;

    procedure DoScrolling;
    function IsScrollBarsArea(const APoint: TPoint): Boolean; virtual;
    function IsScrollBarsCapture: Boolean; virtual;
    function IsSizeGripArea(const APoint: TPoint): Boolean; virtual;
    procedure ScrollContent(ADirection: TcxDirection); virtual;

    property GestureHelper: TdxGestureHelper read FGestureHelper;

    property ActivateType: TcxControlActivateType read FActivateType write FActivateType;
    property BorderSize: Integer read GetBorderSize;
    property BorderStyle: TcxControlBorderStyle read FBorderStyle write SetBorderStyle;
    property CreatingWindow: Boolean read FCreatingWindow;
    property FocusOnClick: Boolean read FFocusOnClick write FFocusOnClick default True;
    property FontListenerList: IInterfaceList read FFontListenerList;
    property Keys: TcxKeys read FKeys write SetKeys;
    property MouseButtonPressed: Boolean read FMouseButtonPressed;
    property MouseRightButtonReleased: Boolean read FMouseRightButtonReleased write FMouseRightButtonReleased;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
    property IsScrollingContent: Boolean read FIsScrollingContent;
    property IsPopupMenuShown: Boolean read FIsPopupMenuShown;
    property ParentBackground default True;
    property SizeGripBounds: TRect read GetSizeGripBounds;

    property InternalScaleFactor: TdxScaleFactor read FInternalScaleFactor;
    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanFocusEx: Boolean; virtual;
    procedure DefaultHandler(var Message); override;
    procedure InvalidateRect(const R: TRect; EraseBackground: Boolean);
    procedure InvalidateRgn(ARegion: TcxRegion; EraseBackground: Boolean);
    procedure InvalidateWithChildren;
    function IsMouseInPressedArea(X, Y: Integer): Boolean; virtual;
    procedure PostMouseMove(AMousePos: TPoint); overload;
    procedure PostMouseMove; overload;
    procedure SetFocus; override;
    procedure UpdateWithChildren;

    procedure AddFontListener(AListener: IcxFontListener);
    procedure RemoveFontListener(AListener: IcxFontListener);

    // IdxLocalizerListener
    procedure TranslationChanged; virtual;

    // IdxSystemInfoListener
    procedure IdxSystemInfoListener.Changed = SystemInfoChanged;
    procedure SystemInfoChanged(AParameter: Cardinal); virtual;

    property ActiveCanvas: TcxCanvas read GetActiveCanvas;
    property Bounds: TRect read GetBounds;
    property Canvas: TcxCanvas read FCanvas;
    property ClientBounds: TRect read GetClientBounds;
    property Graphics: TdxGraphics read FGraphics;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsFocused: Boolean read GetIsFocused;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property MouseCaptureObject: TObject read FMouseCaptureObject write SetMouseCaptureObject;
    property MouseDownPos: TPoint read FMouseDownPos write FMouseDownPos;

    property TabStop default True;
  end;

implementation

uses
  Types,
  Consts, ActnList, Math, dxUxTheme, dxThemeConsts, cxLibraryConsts,
  cxLibraryStrs, UxTheme, DWMApi, cxDWMApi, cxFormats, dxThemeManager, dxTypeHelpers;

type
  TControlAccess = class(TControl);
  TControlActionLinkAccess = class(TControlActionLink);

{ TdxCustomRichEditControl }

constructor TdxVCLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalScaleFactor := TdxScaleFactor.Create;
  FInternalScaleFactor.Assign(96, Trunc(TdxGraphicsDpi.Pixel));
  FCanvas := TcxControlCanvas.Create(inherited Canvas);
  FFocusOnClick := True;
  ParentDoubleBuffered := False;
  FDoubleBuffered := True;
  FFontListenerList := TInterfaceList.Create;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChangeHandler;
  CreateScrollBars;
  TabStop := MayFocus;
  FGestureHelper := TdxGestureHelper.Create(Self);

  Touch.InteractiveGestures := GetDefaultInteractiveGestures;
  Touch.InteractiveGestureOptions := GetDefaultInteractiveGestureOptions;

  dxResourceStringsRepository.AddListener(Self);
end;

destructor TdxVCLControl.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  FreeAndNil(FGestureHelper);
  EndDrag(False);
  DestroyScrollBars;
  FFontListenerList := nil;
  FreeAndNil(FActiveCanvas);
  FCanvas.Free;
  FreeAndNil(FLookAndFeel);
  cxClearObjectLinks(Self);
  FreeAndNil(FInternalScaleFactor);
  inherited Destroy;
end;

function TdxVCLControl.GetActiveCanvas: TcxCanvas;
begin
  if HandleAllocated then
  begin
    FreeAndNil(FActiveCanvas);
    Result := Canvas;
  end
  else
  begin
    if FActiveCanvas = nil then
      FActiveCanvas := TcxScreenCanvas.Create;
    Result := FActiveCanvas;
  end;
end;

function TdxVCLControl.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxVCLControl.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TdxVCLControl.SetBorderStyle(Value: TcxControlBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    BoundsChanged;
  end;
end;

procedure TdxVCLControl.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  LookAndFeel.Assign(Value);
end;

procedure TdxVCLControl.SetKeys(Value: TcxKeys);
begin
  if FKeys <> Value then
  begin
    FKeys := Value;
  end;
end;

procedure TdxVCLControl.SetMouseCaptureObject(Value: TObject);
var
  AIMouseCaptureObject: IcxMouseCaptureObject;
begin
  if FMouseCaptureObject <> Value then
  begin
    if (FMouseCaptureObject <> nil) and
      Supports(FMouseCaptureObject, IcxMouseCaptureObject, AIMouseCaptureObject) then
      AIMouseCaptureObject.DoCancelMode;
    FMouseCaptureObject := Value;
    MouseCapture := FMouseCaptureObject <> nil;
  end;
end;

procedure TdxVCLControl.SetPopupMenu(Value: TComponent);
begin
  if not IsPopupMenu(Value) then
    Value := nil;
  if FPopupMenu <> Value then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
  end;
end;

procedure TdxVCLControl.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  DoCancelMode;
end;

procedure TdxVCLControl.WMContextMenu(var Message: TWMContextMenu);
begin
  if IsScrollBarsCapture or IsScrollBarsArea(ScreenToClient(SmallPointToPoint(Message.Pos))) then
    Message.Result := 1
  else
  begin
    try
      FIsPopupMenuShown := True;
      inherited;
    finally
      FIsPopupMenuShown := False;
    end;
  end;
end;

procedure TdxVCLControl.WMDestroy(var Message: TWMDestroy);
begin
  FinalizeControl;
  inherited;
end;

procedure TdxVCLControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxVCLControl.WMGetDlgCode(var Message: TWMGetDlgCode);
const
  DlgCodes: array[TcxKey] of Integer =
    (DLGC_WANTALLKEYS, DLGC_WANTARROWS, DLGC_WANTCHARS, DLGC_WANTTAB);
var
  I: TcxKey;
  Res: Integer;
begin
  Res := 0;
  for I := Low(TcxKey) to High(TcxKey) do
    if (I in FKeys) and ((I <> kTab) or (GetAsyncKeyState(VK_CONTROL) >= 0)) then
      Inc(Res, DlgCodes[I]);
  Message.Result := Res;
end;

procedure TdxVCLControl.StandardPaintHandler(var Message: TWMPaint);
var
  AClipRgn: HRgn;
  AHasClipRgn: Boolean;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := Message.DC;
  if DC = 0 then
  begin
    AClipRgn := CreateRectRgn(0, 0, 0, 0);
    try
      AHasClipRgn := dxGetUpdateRgn(Self, AClipRgn);
      DC := BeginPaint(Handle, PS);
      if AHasClipRgn then
        SelectClipRgn(DC, AClipRgn);
    finally
      DeleteObject(AClipRgn);
    end;
  end;
  try
    PaintWindow(DC)
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end
end;

procedure TdxVCLControl.WMPaint(var Message: TWMPaint);
begin
  if IsNeedFullBufferedPaint then
    FullBufferedPaint(Message)
  else
  begin
    if (Message.DC <> 0) or not IsDoubleBufferedNeeded then
      StandardPaintHandler(Message)
    else
      if IsCompositionEnabled and AllowCompositionPainting then
        dxPaintWindowOnGlass(Handle, GetPaintBlackOpaqueOnGlass)
      else
        dxBufferedPaintControl(Self);
  end;
end;

procedure TdxVCLControl.FullBufferedPaint(var Message: TWMPaint);
var
  ADC, AMemoryDC: HDC;
  AClipRgn: HRgn;
  AHasClipRgn: Boolean;
  AMemoryBitmap, AOldBitmap: HBITMAP;
  APaintStruct: TPaintStruct;
begin
  if (Message.DC <> 0) or not IsDoubleBufferedNeeded then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    ADC := BeginPaint(Handle, APaintStruct);
    AMemoryDC := CreateCompatibleDC(ADC);
    AClipRgn := CreateRectRgn(0, 0, 0, 0);
    AHasClipRgn := dxGetUpdateRgn(Self, AClipRgn);
    AMemoryBitmap := CreateCompatibleBitmap(ADC, Width, Height);
    try
      AOldBitmap := SelectObject(AMemoryDC, AMemoryBitmap);
      try
        if AHasClipRgn then
          SelectClipRgn(AMemoryDC, AClipRgn);
        Perform(WM_ERASEBKGND, AMemoryDC, AMemoryDC);
        PaintWindow(AMemoryDC);
        Message.DC := 0;
        BitBlt(ADC, 0, 0, Width, Height, AMemoryDC, 0, 0, SRCCOPY);
      finally
        SelectObject(AMemoryDC, AOldBitmap);
      end;
    finally
      EndPaint(Handle, APaintStruct);
      DeleteDC(AMemoryDC);
      DeleteObject(AMemoryBitmap);
      DeleteObject(AClipRgn);
    end;
  end;
end;

procedure TdxVCLControl.WMSetCursor(var Message: TWMSetCursor);

  function InternalSetCursor: Boolean;
  var
    P: TPoint;
    ACursor: TCursor;
  begin
    ACursor := crDefault;
    if Screen.Cursor = crDefault then
    begin
      P := GetMouseCursorClientPos;
      if SizeGripBounds.Contains(P) then
        ACursor := crArrow;
    end;
    Result := ACursor <> crDefault;
    if Result then
      SetCursor(Screen.Cursors[ACursor]);
  end;

begin
  if (Message.CursorWnd <> Handle) or not InternalSetCursor then
    inherited;
end;

procedure TdxVCLControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  with Message do
    if Result = 0 then
      Result := Integer(GetDesignHitTest(XPos, YPos, KeysToShiftState(Keys)));
end;

procedure TdxVCLControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  EnabledChanged;
end;

procedure TdxVCLControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FontChanged;
end;

procedure TdxVCLControl.CMInvalidate(var Message: TMessage);
begin
  if HandleAllocated and not IsDestroying then
    InvalidateControl(Self, Message.WParam = 0, NeedRedrawOnResize);
end;

procedure TdxVCLControl.CMNCSizeChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TdxVCLControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  TextChanged;
end;

procedure TdxVCLControl.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  VisibleChanged;
end;

procedure TdxVCLControl.CNKeyDown(var Message: TWMKeyDown);
var
  AMask: Integer;
begin
  with Message do
  begin
    Result := 1;
    UpdateUIState(Message.CharCode);
    if IsMenuKey(Message) then Exit;
    if not IsDesigning then
    begin
      if Perform(CM_CHILDKEY, CharCode, LPARAM(Self)) <> 0 then
        Exit;
      AMask := 0;
      case CharCode of
        VK_TAB:
          AMask := DLGC_WANTTAB;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          AMask := DLGC_WANTARROWS;
        VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
          AMask := DLGC_WANTALLKEYS;
      end;
      if (AMask <> 0) and
        (Perform(CM_WANTSPECIALKEY, CharCode, 0) = 0) and
        (Perform(WM_GETDLGCODE, 0, 0) and AMask = 0) and
        (GetParentForm(Self).Perform(CM_DIALOGKEY,
        CharCode, KeyData) <> 0) then Exit;
    end;
    Result := 0;
  end;
end;

procedure TdxVCLControl.CNKeyUp(var Message: TWMKeyDown);
begin
    inherited;
end;

procedure TdxVCLControl.CNSysKeyDown(var Message: TWMKeyDown);
begin
  with Message do
  begin
    Result := 1;
    if IsMenuKey(Message) then Exit;
    if not IsDesigning then
    begin
      if Perform(CM_CHILDKEY, CharCode, LPARAM(Self)) <> 0 then
        Exit;
      if GetParentForm(Self).Perform(CM_DIALOGKEY, CharCode, KeyData) <> 0 then Exit;
    end;
    Result := 0;
  end;
end;

procedure TdxVCLControl.CreateScrollBars;
begin
  dxSystemInfo.AddListener(Self);
end;

procedure TdxVCLControl.DestroyScrollBars;
begin
  dxSystemInfo.RemoveListener(Self);
end;

procedure TdxVCLControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    if not CheckDefaults or (Self.Hint = '') then
      Self.Hint := TCustomAction(Sender).Hint;
end;

procedure TdxVCLControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    Style := Style or WS_CLIPCHILDREN;
end;

procedure TdxVCLControl.CreateWnd;
begin
  FCreatingWindow := True;
  try
    inherited;
    InitializeControl;
  finally
    FCreatingWindow := False;
  end;
end;

function TdxVCLControl.IsNeedFullBufferedPaint: Boolean;
begin
  Result := True;
end;

procedure TdxVCLControl.Resize;
begin
  inherited;
  BoundsChanged;
end;

procedure TdxVCLControl.WndProc(var Message: TMessage);

  function GetMousePos: TPoint;
  begin
    if HandleAllocated and ((Width > 32768) or (Height > 32768)) then
      Result := ScreenToClient(GetMouseCursorPos)
    else
      Result := SmallPointToPoint(TWMMouse(Message).Pos);
  end;

  function GetMouseButton: TMouseButton;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN:
        Result := mbLeft;
      WM_RBUTTONDOWN:
        Result := mbRight;
    else
      Result := mbMiddle;
    end;
  end;

  procedure DoAfterMouseDown;
  begin
  end;

  function IsScrollBarsManagerMessage: Boolean;
  begin
    Result := False;
  end;

var
  ALink: TcxObjectLink;
begin
  if CanProcessScrollEvents and (IsGestureHelperMessage(Message) or IsScrollBarsManagerMessage) then
    Exit;
  ALink := cxAddObjectLink(Self);
  try
    if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging and
      (IsDesigning and GetDesignHitTest(GetMousePos.X, GetMousePos.Y, [ssLeft]) or
       not IsDesigning and (DragMode = dmAutomatic)) then
    begin
      if not IsControlMouseMsg(TWMMouse(Message)) then
      begin
        ControlState := ControlState + [csLButtonDown];
        Dispatch(Message);
        ControlState := ControlState - [csLButtonDown];
      end;
      Exit;
    end;
    if Message.Msg = WM_RBUTTONUP then
      FMouseRightButtonReleased := True;
    inherited;
  finally
    try
      if (ALink.Ref <> nil) and not IsDestroying then
      begin
        case Message.Msg of
          WM_RBUTTONUP:
            FMouseRightButtonReleased := False;
          WM_SETFOCUS:
            begin
              FocusEnter;
              FocusChanged;
            end;
          WM_KILLFOCUS:
            begin
              FocusLeave;
              FocusChanged;
            end;
        end;
        DoAfterMouseDown;
      end;
    finally
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

procedure TdxVCLControl.DestroyWindowHandle;
begin
  inherited DestroyWindowHandle;
  ControlState := ControlState - [csClicked];
end;

procedure TdxVCLControl.BeginGestureScroll(APos: TPoint);
begin
end;

procedure TdxVCLControl.EndGestureScroll;
begin
end;

procedure TdxVCLControl.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  inherited;
  FGestureHelper.DoGesture(EventInfo, Handled);
end;

procedure TdxVCLControl.DoGetGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  inherited;
  FGestureHelper.CheckGestureOptions(Gestures, Options);
end;

function TdxVCLControl.GetDefaultInteractiveGestures: TInteractiveGestures;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to dxSupportedGestureCount - 1 do
    if IsDefaultGesture(dxSupportedGestureIDs[I]) then
      Include(Result, GetInteractiveGestureByGestureID(dxSupportedGestureIDs[I]));
end;

function TdxVCLControl.GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions;
begin
  Result := [igoParentPassthrough] + GetInteractiveGestureOptionsByPanOptions(GetDefaultPanOptions);
end;

function TdxVCLControl.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  case AProperty of
    tpInteractiveGestures: Result := Touch.InteractiveGestures <> GetDefaultInteractiveGestures;
    tpInteractiveGestureOptions: Result := Touch.InteractiveGestureOptions <> GetDefaultInteractiveGestureOptions;
  else
    Result := inherited IsTouchPropertyStored(AProperty);
  end;
end;

procedure TdxVCLControl.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  ScrollContentByGesture(ADeltaX, ADeltaY);
end;

function TdxVCLControl.CanProcessScrollEvents: Boolean;
begin
  Result := Enabled;
end;

function TdxVCLControl.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := AGestureID in [GID_PAN, GID_PRESSANDTAP, GID_ZOOM];
end;

function TdxVCLControl.IsGestureHelperMessage(var Message: TMessage): Boolean;
begin
  Result := (FGestureHelper <> nil) and FGestureHelper.HandleMessage(Message);
end;

function TdxVCLControl.IsGestureScrolling: Boolean;
begin
  Result := FGestureHelper.IsPanning;
end;

function TdxVCLControl.GetDefaultPanOptions: Integer;
begin
  Result := dxTouchPanOptions;
end;

procedure TdxVCLControl.ScrollContentByGesture(ADeltaX, ADeltaY: Integer);
begin
end;

function TdxVCLControl.DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean;
begin
  Result := ShowPopupMenu(Self, AMenu, X, Y);
end;

function TdxVCLControl.GetPopupMenu: TPopupMenu;
begin
  if FPopupMenu is TPopupMenu then
    Result := TPopupMenu(FPopupMenu)
  else
    Result := nil;
end;

function TdxVCLControl.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TdxVCLControl.IsMenuKey(var Message: TWMKey): Boolean;
var
  AControl: TWinControl;
  AParentForm: TCustomForm;
  AControlPopupMenu: TPopupMenu;
begin
  Result := True;
  if not IsDesigning then
  begin
    AControl := Self;
    repeat
      AControlPopupMenu := TControlAccess(AControl).GetPopupMenu;
      if Assigned(AControlPopupMenu) and (AControlPopupMenu.WindowHandle <> 0) and
        AControlPopupMenu.IsShortCut(Message) then Exit;
      if (AControl is TdxVCLControl) and
        IsPopupMenuShortCut(TdxVCLControl(AControl).PopupMenu, Message) then Exit;
      AControl := AControl.Parent;
    until AControl = nil;
    AParentForm := GetParentForm(Self);
    if (AParentForm <> nil) and AParentForm.IsShortCut(Message) then Exit;
  end;
  with Message do
    if SendAppMessage(CM_APPKEYDOWN, CharCode, KeyData) <> 0 then Exit;
  Result := False;
end;

function TdxVCLControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := DoubleBuffered or IsWinSevenOrLater;
end;

procedure TdxVCLControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

procedure TdxVCLControl.SetParentBackground(Value: Boolean);
var
  AParentBackgroundChanged: Boolean;
begin
  AParentBackgroundChanged := Value <> ParentBackground;
  inherited;
  if AParentBackgroundChanged then
    ParentBackgroundChanged;
end;

procedure TdxVCLControl.Paint;
var
  AIntf: IcxLockedStatePaint;
  ATopMostControl: TWinControl;
  P: TPoint;
begin
  if IsLoading then Exit;
  if Supports(Self, IcxLockedStatePaint, AIntf) and (AIntf.GetImage <> nil) then
  begin
    ATopMostControl := AIntf.GetTopmostControl;
    if ATopmostControl = Self then
      ActiveCanvas.Canvas.StretchDraw(ClientRect, AIntf.GetImage)
    else
    begin
      P := ClientToParent(cxNullPoint, ATopMostControl);
      cxBitBlt(ActiveCanvas.Handle, AIntf.GetImage.cxCanvas.Handle, ClientRect, P, SRCCOPY);
    end;
  end
  else
  begin
    FGraphics := TdxGraphics.CreateFromHdc(Canvas.Handle);
    try
      DoPaint;
    finally
      FreeAndNil(FGraphics);
    end;
  end;
end;

procedure TdxVCLControl.PaintNonClientArea(ACanvas: TcxCanvas);
begin
end;

procedure TdxVCLControl.PaintWindow(DC: HDC);


begin
  if IsLoading then Exit;
  if Canvas.Canvas.HandleAllocated then
  begin
    Canvas.SaveDC;
    try
      inherited;
    finally
      Canvas.RestoreDC;
    end;
  end
  else
    inherited;
end;

procedure TdxVCLControl.DoScrolling;
begin
  cxProcessControlScrollingOnMiddleButton(Self, False, True, ScrollContent, FIsScrollingContent);
end;

procedure TdxVCLControl.BoundsChanged;
begin
end;

procedure TdxVCLControl.ParentBackgroundChanged;
begin
end;

procedure TdxVCLControl.EnabledChanged;
begin
end;

procedure TdxVCLControl.VisibleChanged;
begin
end;

procedure TdxVCLControl.BringInternalControlsToFront;
begin
end;

procedure TdxVCLControl.CancelMouseOperations;
begin
  MouseCaptureObject := nil;
end;

function TdxVCLControl.AllowCompositionPainting: Boolean;
begin
  Result := True;
end;

function TdxVCLControl.CanFocusOnClick: Boolean;
begin
  Result := not IsDesigning and FFocusOnClick and MayFocus and CanFocus and IsWindowVisible(Handle);
end;

function TdxVCLControl.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := CanFocusOnClick;
end;

procedure TdxVCLControl.DoCancelMode;
begin
  FMouseButtonPressed := False;
  MouseCaptureObject := nil;
end;

procedure TdxVCLControl.DoPaint;
begin
  if FBorderStyle = cxcbsDefault then
  begin
    DrawBorder(Canvas);
    Canvas.IntersectClipRect(cxRectInflate(Bounds, -BorderSize));
    SetPaintRegion;
  end;
end;

procedure TdxVCLControl.DrawBorder(ACanvas: TcxCanvas);
begin
  LookAndFeelPainter.DrawBorder(ACanvas, Bounds);
end;

procedure TdxVCLControl.FocusChanged;
begin
  if Assigned(FOnFocusChanged) then FOnFocusChanged(Self);
end;

function TdxVCLControl.FocusWhenChildIsClicked(AChild: TControl): Boolean;
begin
  Result := CanFocusOnClick;
end;

procedure TdxVCLControl.FontChanged;
var
  I: Integer;
  AIntf: IcxLockedStateFontChanged;
begin
  if Supports(Self, IcxLockedStateFontChanged, AIntf) then
    AIntf.FontChanged(Font);
  for I := 0 to FFontListenerList.Count - 1 do
    IcxFontListener(FFontListenerList[I]).Changed(Self, Font);
  Invalidate;
end;

function TdxVCLControl.GetBorderSize: Integer;
begin
  Result := IfThen(FBorderStyle = cxcbsDefault, LookAndFeelPainter.BorderSize);
end;

function TdxVCLControl.GetBounds: TRect;
begin
  if IsRectEmpty(FBounds) then
    if HandleAllocated then
      Result := ClientRect
    else
      Result.InitSize(0, 0, Width, Height)
  else
    Result := FBounds;
end;

function TdxVCLControl.GetClientBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, -BorderSize, -BorderSize);
end;

function TdxVCLControl.GetClientOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TdxVCLControl.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  if IsScrollBarsArea(Point(X, Y)) then
    Result := crArrow
  else
    if IsDesigning then
      Result := crDefault
    else
      Result := Cursor;
end;

function TdxVCLControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
   Result := False;
end;

function TdxVCLControl.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxVCLControl.GetIsFocused: Boolean;
begin
  Result := Focused;
end;

function TdxVCLControl.GetMouseCursorClientPos: TPoint;
begin
  Result := ScreenToClient(GetMouseCursorPos);
end;

function TdxVCLControl.GetPaintBlackOpaqueOnGlass: Boolean;
begin
  Result := csPaintBlackOpaqueOnGlass in ControlStyle;
end;

function TdxVCLControl.GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass;
begin
  Result := TcxControlScrollBar;
end;

function TdxVCLControl.GetSizeGripBounds: TRect;
begin
  Result := cxEmptyRect;
end;

procedure TdxVCLControl.InitializeControl;
begin
end;

procedure TdxVCLControl.FinalizeControl;
begin
end;

class procedure TdxVCLControl.InvalidateControl(AControl: TWinControl; ANeedInvalidateSelf, ANeedInvalidateChildren: Boolean);

  function NeedInvalidateControl(AControl: TControl): Boolean;
  var
    AcxTransparentControl: IcxTransparentControl;
  begin
    if Supports(AControl, IcxTransparentControl, AcxTransparentControl) then
      Result := AcxTransparentControl.IsTransparentRegionsPresent
    else
      Result := (AControl is TWinControl) and
        (not (csOpaque in AControl.ControlStyle) or (csParentBackground in AControl.ControlStyle));
  end;

var
  I: Integer;
begin
  if AControl.HandleAllocated then
  begin
    if AControl.Parent <> nil then
      AControl.Parent.Perform(CM_INVALIDATE, 1, 0);
    if ANeedInvalidateSelf then
    begin
      cxInvalidateRect(AControl.Handle, not (csOpaque in AControl.ControlStyle));
      if ANeedInvalidateChildren then
      begin
        for I := 0 to AControl.ControlCount - 1 do
          if NeedInvalidateControl(AControl.Controls[I]) then
            AControl.Controls[I].Invalidate;
      end;
    end;
  end;
end;

function TdxVCLControl.MayFocus: Boolean;
begin
  Result := True;
end;

procedure TdxVCLControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ALink: TcxObjectLink;
  AOriginalBounds: TRect;
begin
  FMouseDownPos := Point(X, Y);
  ALink := cxAddObjectLink(Self);
  try
    if CanFocusOnClick(X, Y) and not (ssDouble in Shift) then
    begin
      AOriginalBounds := BoundsRect;
      FActivateType := atByMouse;
      SetFocus;
      if ALink.Ref = nil then Exit;
      if (GetParentForm(Self) <> nil) and (GetParentForm(Self).ActiveControl = Self) and
        not IsFocused then
        Windows.SetFocus(Handle);
      if UpdateMousePositionIfControlMoved then
      begin
        Inc(X, AOriginalBounds.Left - Left);
        Inc(Y, AOriginalBounds.Top - Top);
      end;
    end;
    if ALink.Ref = nil then Exit;
    inherited;
  finally
    if ALink.Ref <> nil then
      if MouseCapture then
        FMouseButtonPressed := True;
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TdxVCLControl.FocusEnter;
begin
//do nothing
end;

procedure TdxVCLControl.FocusLeave;
begin
//do nothing
end;

procedure TdxVCLControl.SetPaintRegion;
begin
  Canvas.IntersectClipRect(ClientBounds);
end;

procedure TdxVCLControl.UpdateStatusHint(const APoint: TPoint);
begin
  // do nothing
end;

function TdxVCLControl.NeedRedrawOnResize: Boolean;
begin
  Result := False;
end;

procedure TdxVCLControl.TextChanged;
begin
end;

function TdxVCLControl.UpdateMousePositionIfControlMoved: Boolean;
begin
  Result := True;
end;

function TdxVCLControl.AllowGesture(AGestureId: Integer): Boolean;
begin
   Result := IsDefaultGesture(AGestureId);
end;

function TdxVCLControl.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbVertical;
end;

function TdxVCLControl.GetPanOptions: Integer;
begin
  Result := GetPanOptionsByInteractiveGestureOptions(Touch.InteractiveGestureOptions);
  if igPan in Touch.InteractiveGestures then
    Result := GC_PAN or Result;
end;

function TdxVCLControl.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ClientBounds, APoint) and not IsScrollBarsArea(APoint);
end;

function TdxVCLControl.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TdxVCLControl.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := Self;
end;

function TdxVCLControl.GetGestureClientHandle: THandle;
begin
  Result := Handle;
end;

function TdxVCLControl.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := AWnd = Handle;
end;

procedure TdxVCLControl.Zoom(ADelta: Integer; var AHandled: Boolean);
begin
  AHandled := True;
end;

function TdxVCLControl.GetLookAndFeelValue: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

function TdxVCLControl.GetControl: TWinControl;
begin
  Result := Self;
end;

function TdxVCLControl.GetScrollBarLookAndFeel: TcxLookAndFeel;
begin
  Result := FLookAndFeel;
end;

function TdxVCLControl.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

procedure TdxVCLControl.LookAndFeelChangeHandler(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  if not (csDestroying in (Application.ComponentState + ComponentState)) then
    LookAndFeelChanged(Sender, AChangedValues);
end;

procedure TdxVCLControl.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
end;

function TdxVCLControl.IsScrollBarsArea(const APoint: TPoint): Boolean;
begin
  Result := IsSizeGripArea(APoint);
end;

function TdxVCLControl.IsScrollBarsCapture: Boolean;
begin
  Result := False;
end;

function TdxVCLControl.IsSizeGripArea(const APoint: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxVCLControl.ScaleFactorChanged;
begin
  FInternalScaleFactor.Assign(ScaleFactor.Numerator * 96, ScaleFactor.Denominator * Trunc(TdxGraphicsDpi.Pixel));
  inherited ScaleFactorChanged;
end;

procedure TdxVCLControl.ScrollContent(ADirection: TcxDirection);
begin
end;

function TdxVCLControl.CanFocusEx: Boolean;
var
  AParentForm: TCustomForm;
begin
  AParentForm := GetParentForm(Self);
  Result := CanFocus and ((AParentForm = nil) or
    AParentForm.CanFocus and AParentForm.Enabled and AParentForm.Visible);
end;

procedure TdxVCLControl.DefaultHandler(var Message);
begin
  if not TcxControlDefaultHandlerHelper.Process(Message) then
    inherited DefaultHandler(Message);
end;

procedure TdxVCLControl.InvalidateRect(const R: TRect; EraseBackground: Boolean);
begin
  if HandleAllocated then
    cxInvalidateRect(Handle, R, EraseBackground);
end;

procedure TdxVCLControl.InvalidateRgn(ARegion: TcxRegion; EraseBackground: Boolean);
begin
  if HandleAllocated and (ARegion <> nil) and not ARegion.IsEmpty then
    Windows.InvalidateRgn(Handle, ARegion.Handle, EraseBackground);
end;

procedure TdxVCLControl.InvalidateWithChildren;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_NOERASE);
end;

function TdxVCLControl.IsMouseInPressedArea(X, Y: Integer): Boolean;
begin
  Result := IsPointInDragDetectArea(MouseDownPos, X, Y);
end;

procedure TdxVCLControl.PostMouseMove;
begin
  if HandleAllocated then
    PostMouseMove(ScreenToClient(GetMouseCursorPos));
end;

procedure TdxVCLControl.PostMouseMove(AMousePos: TPoint);
begin
  if HandleAllocated and (GetCapture = 0) then
    with AMousePos do
      PostMessage(Handle, WM_MOUSEMOVE, 0, MakeLParam(X, Y));
end;

procedure TdxVCLControl.SetFocus;
begin
  inherited SetFocus;
  FActivateType := atOther;
end;

procedure TdxVCLControl.UpdateWithChildren;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

procedure TdxVCLControl.AddFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Add(AListener);
end;

procedure TdxVCLControl.RemoveFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Remove(AListener);
end;

procedure TdxVCLControl.TranslationChanged;
begin
end;

procedure TdxVCLControl.SystemInfoChanged(AParameter: Cardinal);
begin
  if HandleAllocated then
    SendNotifyMessage(Handle, DXM_NCSIZECHANGED, 0, 0);
end;

end.
