unit M2ComponentDesignContainer;

interface
  uses Windows, Messages, Classes, Sysutils, Controls, Graphics, Forms;

type
  TDsnIOState = (dioBeforeRead, dioAfterRead, dioBeforeWrite, dioAfterWrite);

  TFormDesignContainer = class(TCustomControl)
  private
    FForm: TCustomForm;
    FRulerWidth: TPoint;
    FScrollPos: TPoint;
    FFlatScrollBars: Boolean;
    FBorderStyle: TBorderStyle;
    FDragging: Boolean;
    FDragDelta: TPoint;
    FDragSizeType: TCursor;
    FHideFormBorders: Boolean;
    FSavedStyle: LongWord;
    FShowRuler: Boolean;
    FShowFrame: Boolean;
    FFrameSize: integer;
    procedure SetForm(const Value: TCustomForm);
    procedure DetachForm;
    procedure AttachForm;
    procedure SetScrollPos(Value: TPoint);
    function GetScrollMax: TPoint;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMEraseBkgnd(var Message: TWMErasebkgnd); message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TWMErasebkgnd); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetFlatScrollBars(const Value: Boolean);
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure SetBorderStyle(const Value: TBorderStyle);
    function SizeHitTest(X, Y: integer): TCursor;
    procedure SetHideFormBorders(const Value: Boolean);
    procedure HideBorder;
    procedure RestoreBorder;
    function GetFormOrigin: TPoint;
    procedure SetShowRuler(const Value: Boolean);
    procedure SetShowFrame(const Value: Boolean);
    procedure SetFrameSize(const Value: integer);
    procedure ResetContainer;
  public
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function IsHiddenBorder: Boolean;
    procedure DrawRulers;
    procedure DrawFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoSizing(var Rect: TRect);
    procedure AdjustScroll;
    procedure PrepareIO(State: TDsnIOState);

    property Form: TCustomForm read FForm write SetForm;
    property ScrollPos: TPoint read FScrollPos write SetScrollPos;
    property FormOrigin: TPoint read GetFormOrigin;
    property InDragging: boolean read FDragging;
    procedure SetFocus; override;
  published
    property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property HideFormBorders: Boolean read FHideFormBorders write SetHideFormBorders default False;
    property ShowRuler: Boolean read FShowRuler write SetShowRuler default True;
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default True;
    property FrameSize: integer read FFrameSize write SetFrameSize default 8;
    property Anchors;
    property Align;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation
  uses FlatSB, uDesignIntf;

{ TFormDesignContainer }

constructor TFormDesignContainer.Create(AOwner: TComponent);
begin
  inherited;
  FRulerWidth := Point(27, 23);
  FFlatScrollBars := False;
  FBorderStyle := bsNone;
  Color := clWindow;
  FShowRuler := True;
  FShowFrame := True;
  FFrameSize := 8;
end;

destructor TFormDesignContainer.Destroy;
begin
  inherited;
end;

procedure TFormDesignContainer.CreateWnd;
begin
  inherited;
  if FFlatScrollBars then
    InitializeFlatSB(Handle);
  AttachForm;
  AdjustScroll;
end;

procedure TFormDesignContainer.DestroyWnd;
begin
  DetachForm;
  if FFlatScrollBars then
    UninitializeFlatSB(Handle);
  inherited;
end;

procedure TFormDesignContainer.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited;
  Params.Style := Params.Style or BorderStyles[BorderStyle] or WS_HSCROLL or WS_VSCROLL;
end;

procedure TFormDesignContainer.AttachForm;
begin
  if (FForm <> nil) and HandleAllocated then
   begin
    SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE);
    Windows.SetParent(FForm.Handle, Handle);
    SetWindowPos(FForm.Handle, HWND_TOP, FormOrigin.X, FormOrigin.Y,
            FForm.Width, FForm.Height,
            SWP_SHOWWINDOW{ or SWP_NOACTIVATE});
    HideBorder;
    AdjustScroll;
    Windows.SetFocus(Handle);
   end;
end;

procedure TFormDesignContainer.DetachForm;
begin
  if FForm <> nil then
   begin
    Windows.ShowWindow(FForm.Handle, SW_HIDE);
    Windows.SetParent(FForm.Handle, 0);
    RestoreBorder;
    AdjustScroll;
   end;
end;

procedure TFormDesignContainer.HideBorder;
var cw, ch, stl: LongWord;
begin
  if IsHiddenBorder and (FForm <> nil) then
    begin
      cw := FForm.ClientWidth;
      ch := FForm.ClientHeight;
      stl := GetWindowLong(FForm.Handle, GWL_STYLE);
      if FSavedStyle = 0 then
        FSavedStyle := stl;
      stl := stl and not (WS_CAPTION or WS_OVERLAPPED or WS_SYSMENU or WS_TABSTOP
             or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_BORDER or WS_THICKFRAME);
      SetWindowLong(FForm.Handle, GWL_STYLE, stl);
      SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0,
              SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
      FForm.Width := cw;
      FForm.Height := ch;
    end;
end;

procedure TFormDesignContainer.RestoreBorder;
var cw, ch: integer;
    R, Rc: TRect;
begin
  if (FSavedStyle <> 0) and (FForm <> nil) then
    begin
      cw := FForm.Width;
      ch := FForm.Height;
      SetWindowLong(FForm.Handle, GWL_STYLE, FSavedStyle);
      FSavedStyle := 0;
      SetWindowPos(FForm.Handle, 0, 0, 0, cw + 10, ch + 40,
              SWP_NOMOVE or {SWP_NOSIZE or }SWP_NOZORDER or SWP_FRAMECHANGED or SWP_HIDEWINDOW);
      GetWindowRect(FForm.Handle, R);
      Rc := FForm.ClientRect;
      with FForm do
        begin
          cw := (R.Right - R.Left) - (Rc.Right - Rc.Left) + cw;
          ch := (R.Bottom - R.Top) - (Rc.Bottom - Rc.Top) + ch;
          SetBounds(Left, Top, cw, ch);
        end;
    end;
end;

procedure TFormDesignContainer.SetHideFormBorders(const Value: Boolean);
begin
  if FHideFormBorders = Value then Exit;
  FHideFormBorders := Value;
  if Value then HideBorder else
    if not IsHiddenBorder then
      RestoreBorder;
end;

function TFormDesignContainer.IsHiddenBorder: Boolean;
var
  dsn: IDesigner;
begin
  Result := FHideFormBorders or
            (Form <> nil) and
            (Form.Designer <> nil) and
            (Form.Designer.QueryInterface(IDesigner, dsn) = S_OK) and
            (dsn.GetRoot <> Form);
end;

procedure TFormDesignContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Form) then
    FForm := nil;
end;

procedure TFormDesignContainer.SetForm(const Value: TCustomForm);
begin
  if FForm = Value then Exit;
  if FForm <> nil then
  InsertComponent(Value);
  DetachForm;
  FForm := Value;
  AttachForm;
  if FForm <> nil then
   FForm.FreeNotification(Self);
end;

procedure TFormDesignContainer.DrawRulers;
var p, n, l, step, lbstep, lbfreq, lbfreq2, frm_offs: integer;
    sz: TSize;
begin
  if FShowFrame then
    frm_offs := FFrameSize
  else
    frm_offs := 0;
  // horizontal ruler
  step := 5;
  lbstep := 100;
  lbfreq := 20;
  lbfreq2 := 10;

  p := FRulerWidth.X + frm_offs;
  n := 0;
  l := 0;
  while p < ClientWidth + FScrollPos.X do
   begin
     if n mod lbfreq = 0 then
      begin
        Canvas.MoveTo(p, FRulerWidth.Y - 2);
        Canvas.LineTo(p, FRulerWidth.Y - 20);
        sz := Canvas.TextExtent(IntToStr(l));
        Canvas.TextOut(p + 4, 2, IntToStr(l));
        Inc(l, lbstep);
      end else
     if n mod lbfreq2 = 0 then
      begin
       Canvas.MoveTo(p, FRulerWidth.Y - 2);
       Canvas.LineTo(p, FRulerWidth.Y - 10);
      end else
      begin
       Canvas.MoveTo(p, FRulerWidth.Y - 2);
       Canvas.LineTo(p, FRulerWidth.Y - 5);
      end;
     Inc(n);
     Inc(p, step);
   end;
  // vertical ruler
  p := FRulerWidth.Y + frm_offs;
  n := 0;
  l := 0;
  while p < ClientHeight + FScrollPos.Y do
   begin
     if n mod lbfreq = 0 then
      begin
        Canvas.MoveTo(FRulerWidth.X - 2, p);
        Canvas.LineTo(FRulerWidth.X - 20, p);
        sz := Canvas.TextExtent(IntToStr(l));
        Canvas.TextOut(2, p + 2, IntToStr(l));
        Inc(l, lbstep);
      end else
     if n mod lbfreq2 = 0 then
      begin
        Canvas.MoveTo(FRulerWidth.X - 2, p);
        Canvas.LineTo(FRulerWidth.X - 10, p);
      end else
      begin
        Canvas.MoveTo(FRulerWidth.X - 2, p);
        Canvas.LineTo(FRulerWidth.X - 5, p);
      end;
     Inc(n);
     Inc(p, step);
   end;
end;

procedure DrawPatternRect(Canvas: TCanvas; R: TRect; Size: integer);
begin
  Canvas.Brush.Bitmap := AllocPatternBitmap(clWhite, clGray);
  Canvas.FillRect(Rect(R.Left - Size, R.Top - Size, R.Left, R.Bottom + Size));
  Canvas.FillRect(Rect(R.Right, R.Top - Size, R.Right + Size, R.Bottom + Size));
  Canvas.FillRect(Rect(R.Left, R.Top - Size, R.Right, R.Top));
  Canvas.FillRect(Rect(R.Left, R.Bottom, R.Right, R.Bottom + Size));
end;

procedure TFormDesignContainer.DrawFrame;
  procedure DrawMark(X, Y: integer; C: TColor);
  begin
    Canvas.Brush.Color := C;
    Canvas.Rectangle(X, Y, X + FFrameSize, Y + FFrameSize);
  end;
var OX, OY: integer;
begin
  if not FDragging then
  begin
    OX := FormOrigin.X;// - FScrollPos.X;
    OY := FormOrigin.Y;// - FScrollPos.Y;
    DrawPatternRect(Canvas, Bounds(OX, OY, FForm.Width, FForm.Height), FFrameSize);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    DrawMark(OX - FFrameSize, OY - FFrameSize, clGray);
    DrawMark(OX - FFrameSize, OY + (FForm.Height - FFrameSize) div 2, clGray);
    DrawMark(OX - FFrameSize, OY + FForm.Height, clGray);
    DrawMark(OX + (FForm.Width - FFrameSize) div 2, OY - FFrameSize, clGray);
    DrawMark(OX + FForm.Width, OY - FFrameSize, clGray);
    DrawMark(OX + FForm.Width, OY + FForm.Height, clWhite);
  end;
end;

procedure TFormDesignContainer.Paint;
begin
  if Form <> nil then
    ExcludeClipRect(Canvas.Handle,
      FormOrigin.X - FScrollPos.X,
      FormOrigin.Y - FScrollPos.Y,
      FormOrigin.X - FScrollPos.X + FForm.Width,
      FormOrigin.Y - FScrollPos.Y + FForm.Height);

  SetWindowOrgEx(Canvas.Handle, FScrollPos.X, FScrollPos.Y, nil);

  Canvas.Brush.Color := Color;
  Canvas.Font := Font;
  Canvas.FillRect(Bounds(FScrollPos.X, FScrollPos.Y, ClientWidth, ClientHeight));

  if FShowRuler then
    DrawRulers;

  if FShowFrame and Assigned(FForm) and not FDragging then
    DrawFrame;
end;

function TFormDesignContainer.GetScrollMax: TPoint;
begin
  Result.X := FForm.Width + FormOrigin.X - ClientWidth + 20;
  Result.Y := FForm.Height + FormOrigin.Y - ClientHeight + 20;
  if Result.X < 0 then Result.X := 0;
  if Result.Y < 0 then Result.Y := 0;
end;

procedure TFormDesignContainer.AdjustScroll;
var si: TScrollInfo;
    maxp: TPoint;
begin
  if not HandleAllocated or FDragging then Exit;

  if FForm = nil then
   begin
    if FFlatScrollBars then
      begin
       FlatSB_ShowScrollBar(Handle, SB_HORZ, False);
       FlatSB_ShowScrollBar(Handle, SB_VERT, False);
      end
    else
      begin
       ShowScrollBar(Handle, SB_HORZ, False);
       ShowScrollBar(Handle, SB_VERT, False);
      end;
    Exit;
   end;

  maxp := GetScrollMax;

  si.cbSize := sizeof(TScrollInfo);
  si.fMask := SIF_ALL;
  si.nMin := 0;
  si.nPage := ClientHeight;

  si.nMax := maxp.Y + ClientHeight - 1;
  si.nPos := FScrollPos.Y;
  if FFlatScrollBars then
    FlatSB_SetScrollInfo(Handle, SB_VERT, si, True)
  else
    SetScrollInfo(Handle, SB_VERT, si, True);

  si.nPage := ClientWidth;
  si.nMax := maxp.X + ClientWidth - 1;
  si.nPos := FScrollPos.X;
  if FFlatScrollBars then
    FlatSB_SetScrollInfo(Handle, SB_HORZ, si, True)
  else
    SetScrollInfo(Handle, SB_HORZ, si, True);

  ScrollPos := ScrollPos;
end;

procedure TFormDesignContainer.SetScrollPos(Value: TPoint);
var maxp: TPoint;
begin
  if (FForm = nil) or not HandleAllocated {or FSelfChanging }then  Exit;

  maxp := GetScrollMax;

  if Value.X < 0 then Value.X := 0 else
   if Value.X > maxp.X then Value.X := maxp.X;

  if Value.Y < 0 then Value.Y := 0 else
   if Value.Y > maxp.Y then Value.Y := maxp.Y;

  maxp := ClientToScreen(Point(FormOrigin.X - Value.X, FormOrigin.Y - Value.Y));

  MoveWindow(FForm.Handle,
     FormOrigin.X - Value.X,
     FormOrigin.Y - Value.Y,
     FForm.Width, FForm.Height, True);
  if (Value.X <> FScrollPos.X) or (Value.Y <> FScrollPos.Y) then
   begin
    FScrollPos := Value;
    AdjustScroll;
    Paint;
//    Invalidate;
   end;
end;

procedure TFormDesignContainer.Resize;
begin
  inherited;
  AdjustScroll;
end;

procedure TFormDesignContainer.WMHScroll(var Message: TWMHScroll);
var p: TPoint;
begin
  p := ScrollPos;
  case Message.ScrollCode of
    SB_LINEUP:    Dec(p.X, 1);
    SB_LINEDOWN:  Inc(p.X, 1);
    SB_PAGEUP:    Dec(p.X, 10);
    SB_PAGEDOWN:  Inc(p.X, 10);
    SB_TOP:       p.X := 0;
    SB_BOTTOM:    p.X := GetScrollMax.X;
    SB_THUMBTRACK:p.x := Message.Pos;
   end;
  Message.Result := 0;
  ScrollPos := p;
end;

procedure TFormDesignContainer.WMVScroll(var Message: TWMVScroll);
var p: TPoint;
begin
  p := ScrollPos;
  case Message.ScrollCode of
    SB_LINEUP:    Dec(p.Y, 1);
    SB_LINEDOWN:  Inc(p.Y, 1);
    SB_PAGEUP:    Dec(p.Y, 10);
    SB_PAGEDOWN:  Inc(p.Y, 10);
    SB_TOP:       p.Y := 0;
    SB_BOTTOM:    p.Y := GetScrollMax.Y;
    SB_THUMBTRACK:p.Y := Message.Pos;
   end;
  Message.Result := 0;
  ScrollPos := p;
end;

procedure TFormDesignContainer.WMActivate(var Message: TWMActivate);
begin
  if FForm<>nil then
    SendMessage(FForm.Handle, WM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);
  inherited;
end;

procedure TFormDesignContainer.WMEraseBkgnd(var Message: TWMErasebkgnd);
begin
  Message.Result := 1;
end;

procedure TFormDesignContainer.SetFlatScrollBars(const Value: Boolean);
begin
  if FFlatScrollBars <> Value then
   begin
     FFlatScrollBars := Value;
     if HandleAllocated then
       begin
        if not FFlatScrollBars then
           UninitializeFlatSB(Handle)
        else
           InitializeFlatSB(Handle);
       end;
   end;
end;

procedure TFormDesignContainer.WndProc(var Message: TMessage);
begin
  if FForm = nil then inherited else
  with Message do
    case Msg of
      WM_KEYDOWN,
      WM_KEYUP,
      WM_CHAR:
      Result := FForm.Perform(Msg, WParam, LParam);
      else inherited;
    end;
end;

procedure TFormDesignContainer.DoSizing(var Rect: TRect);
var w, h: integer;
begin
  w := Rect.Right - Rect.Left;
  h := Rect.Bottom - Rect.Top;
  AdjustScroll;
  Rect.Left := ClientOrigin.X + FormOrigin.X - FScrollPos.X;
  Rect.Top := ClientOrigin.Y + FormOrigin.Y - FScrollPos.Y;
  Rect.Right := Rect.Left + w;
  Rect.Bottom := Rect.Top + h;
end;

procedure TFormDesignContainer.CMFontChanged(var Message: TWMErasebkgnd);
begin
  Invalidate;
end;

procedure TFormDesignContainer.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TFormDesignContainer.SizeHitTest(X, Y: integer): TCursor;
var bx, by: Boolean;
begin
  Result := crDefault;
  if (FForm = nil) then Exit;// or
//     (FForm.Designer <> nil) and (FForm.Designer as {$IFDEF EC_VCL5}IFormDesigner{$ELSE}IDesigner{$ENDIF}).IsSourceReadOnly then Exit;
  X := X + ScrollPos.X - FForm.Width - FormOrigin.X;
  Y := Y + ScrollPos.Y - FForm.Height- FormOrigin.Y;
  bx := (X <= 5) and (X >= 0) and (Y <= 5) and (Y > - FForm.Height);
  by := (Y <= 5) and (Y >= 0) and (X <= 5) and (X > - FForm.Width);
  if bx and by then
    Result := crSizeNWSE else
  if bx then
    Result := crSizeWE else
  if by then
    Result := crSizeNS;
end;

procedure TFormDesignContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
var NW, NH, sX, sY: integer;
    R: TRect;
begin
  inherited;
  if FForm = nil then Exit;
  if FDragging then
    begin
      if (FDragSizeType = crSizeNWSE) or (FDragSizeType = crSizeWE) then
         NW := ScrollPos.X + X - FDragDelta.X  - FormOrigin.X
      else
         NW := FForm.Width;
      if (FDragSizeType = crSizeNWSE) or (FDragSizeType = crSizeNS) then
         NH := ScrollPos.Y + Y - FDragDelta.Y  - FormOrigin.Y
      else
         NH := FForm.Height;
      R := FForm.BoundsRect;
      sX := FormOrigin.X - ScrollPos.X;
      sY := FormOrigin.Y - ScrollPos.Y;
      OffsetRect(R, -R.Left + sX, -R.top + sY);
      MoveWindow(FForm.Handle, sX, sY, NW, NH, True);
      Windows.SetCursor(Screen.Cursors[FDragSizeType]);
      Canvas.Brush.Color := Color;
      ExcludeClipRect(Canvas.Handle, sX, sY, sX + FForm.Width, sY + FForm.Height);
      Canvas.FillRect(R);
    end else
      Windows.SetCursor(Screen.Cursors[SizeHitTest(X, Y)]);
end;

procedure TFormDesignContainer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FDragSizeType := SizeHitTest(X, Y);
  if FDragSizeType <> crDefault then
   begin
    FDragDelta  := Point(X + ScrollPos.X - FForm.Width - FormOrigin.X,
                        Y + ScrollPos.Y - FForm.Height - FormOrigin.Y);
    FDragging   := True;
    SetCapture(Handle);
    Windows.SetCursor(Screen.Cursors[FDragSizeType]);
    if FShowFrame then
      Paint;
   end;
end;

procedure TFormDesignContainer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FDragging then
   begin
    FDragging := False;
    ReleaseCapture;
    AdjustScroll;
    if FForm.Designer <> nil then
      FForm.Designer.Modified;
    if FShowFrame then
      Paint;
   end;
end;

function TFormDesignContainer.GetFormOrigin: TPoint;
  procedure Add(var p1: TPoint; const p2: TPoint);
  begin
    p1.X := p1.X + p2.X;
    p1.Y := p1.Y + p2.Y;
  end;
begin
  Result := Point(0, 0);
  if FShowRuler then
    Add(Result, FRulerWidth);
  if FShowFrame then
    Add(Result, Point(FFrameSize, FFrameSize));
end;

procedure TFormDesignContainer.ResetContainer;
begin
  ScrollPos := Point(0, 0);
  AdjustScroll;
  Paint;
end;

procedure TFormDesignContainer.SetShowRuler(const Value: Boolean);
begin
  if FShowRuler <> Value then
    begin
      FShowRuler := Value;
      ResetContainer;
    end;
end;

procedure TFormDesignContainer.SetShowFrame(const Value: Boolean);
begin
  if FShowFrame <> Value then
    begin
      FShowFrame := Value;
      ResetContainer;
    end;
end;

procedure TFormDesignContainer.SetFrameSize(const Value: integer);
begin
  if (FFrameSize <> Value) and (Value > 0) then
    begin
      FFrameSize := Value;
      ResetContainer;
    end;
end;

procedure TFormDesignContainer.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS or
                    DLGC_WANTARROWS or
                    DLGC_WANTCHARS or
                    DLGC_WANTMESSAGE or
                    DLGC_WANTTAB;
end;

procedure TFormDesignContainer.SetFocus;
begin
  inherited;
  if FForm<>nil then
    SendMessage(FForm.Handle, WM_NCACTIVATE, 1, 0);
end;

procedure TFormDesignContainer.PrepareIO(State: TDsnIOState);
begin
  case State of
    dioBeforeRead,
    dioBeforeWrite:
      begin
        LockWindowUpdate(Handle);
        RestoreBorder;
      end;
    dioAfterRead,
    dioAfterWrite:
      begin
        LockWindowUpdate(0);
        HideBorder;
      end;
  end;
end;

end.
