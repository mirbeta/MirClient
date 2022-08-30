unit uPropertyInplaceEdit;

interface
  uses Windows, Classes, Messages, SysUtils, Controls,
  StdCtrls, Forms, Grids, uDesignIntf, Themes, uVCLEditors, uPropertyManager;

  type
  TuPropertyInplaceEdit  = class(TInplaceEdit, IPropertyHost, IPropertyHost20)
  private
    FActiveProperty: TuPropertyRec;
    FActiveList: TWinControl;
    FButtonWidth: Integer;
    FDefaultPickList: TCustomListbox;
    FEditStyle: TEditStyle;
    FDropDownRows: Integer;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FPickListLoaded: Boolean;
    FOnGetPickListitems: TOnGetPickListItems;
//    FOnEditButtonClick: TNotifyEvent;
    FMouseInControl: Boolean;
    function CurrentRect: TRect;
    function GetPickList: TCustomListbox;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CancelMode;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure SetPropertyValue;
    procedure SetActiveProperty(const Value: TuPropertyRec);
  protected
    procedure DoDropDown; dynamic;
    {IPropertyHost}
    procedure DropDownControl(Control: TPersistent);
    procedure CloseDropDown;
    {IPropertyHost20}
    function GetDropDownWidth: Integer;

    procedure BoundsChanged; override;
    function ButtonRect: TRect;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DblClick; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditButtonClick; virtual;
    procedure DropDown; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function OverButton(const P: TPoint): Boolean;
    procedure PaintWindow(DC: HDC); override;
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure RestoreContents;
    procedure UpdateProperty;
    property ActiveProperty: TuPropertyRec read FActiveProperty write SetActiveProperty;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows;
    property EditStyle: TEditStyle read FEditStyle;
    property ListVisible: Boolean read FListVisible write FListVisible;
    property DefaultPickList: TCustomListbox read GetPickList;
    property PickListLoaded: Boolean read FPickListLoaded write FPickListLoaded;
    property Pressed: Boolean read FPressed;
//    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
//      write FOnEditButtonClick;
    property OnGetPickListitems: TOnGetPickListItems read FOnGetPickListitems
      write FOnGetPickListitems;
  end;

  TControlCrack = class(TControl);

implementation
  uses uDesignEditors, uDesignConst, TypInfo;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

type
  TGridCrack  = class(TCustomGrid);
  TPropertyEditorCrack  = class(TPropertyEditor);

  IPropertyList = interface
    ['{D871D642-0B27-4BD8-BCB4-3595D83B2B6F}']
    function NextValue: String;   //获取下一个值
    function ValueCount: Integer;      //值的个数
    procedure SetProperty(Value: IProperty; const DropDownRows: Integer);///设置当前操作的属性
  end;

  TPopupListbox = class(TCustomListbox, IPropertyList)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
    FProperty: IProperty;
    FListDraw: ICustomPropertyListDrawing;
    FCustomDraw: boolean;
    FDefaultValue: String;
    FDropDownRows: Integer;
    procedure LoadValues;
    procedure SelectDefault;
    procedure GetStrProc(const S: string);
    procedure GetWideStrProc(const S: WideString);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NextValue: String;   //获取下一个值
    function ValueCount: Integer;
    procedure SetProperty(Value: IProperty; const DropDownRows: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TPopupListbox.Create(AOwner: TComponent);
begin
  inherited;
  FProperty  :=  nil;
end;

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER {or WS_SIZEBOX};
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

destructor TPopupListbox.Destroy;
begin
  FProperty  :=  nil;
  inherited;
end;

procedure TPopupListbox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if FCustomDraw then
    FListDraw.ListDrawValue(Items[Index], Canvas, ItemRect(Index), ItemIndex=Index)
  else
    inherited;
end;

procedure TPopupListbox.GetStrProc(const S: string);
begin
  Items.Add(S);
end;

procedure TPopupListbox.GetWideStrProc(const S: WideString);
begin
  Items.Add(S);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..High(Char):
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then
          FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
//        SendTextMessage(Handle, LB_SelectString, WORD(-1), FSearchText);
        SendMessage(Handle, LB_SelectString, WORD(-1), Windows.LPARAM(PWideChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.LoadValues;
const DefaultItemH  = 16; //默认高度
var
  I: Integer;
  IH, MH, IW, MW: Integer;
  wp: IWideProperty;
begin
  Clear;
  if FProperty<>nil then
  begin
    Items.BeginUpdate;
    if Supports(FProperty, IWideProperty, wp) then
      (wp as TWideStringProperty).GetValues(GetWideStrProc)
    else
      FProperty.GetValues(GetStrProc);
    Items.EndUpdate;
  end;
  Sorted  :=  paSortList in FProperty.GetAttributes;
  SelectDefault;
  if FCustomDraw then
  begin
    MH :=  0;
    MW :=  0;
    for I := 0 to self.Items.Count - 1 do
    begin
      IW  :=  Canvas.TextWidth(Items[I]);
      IH  :=  Canvas.TextHeight('jH')+2;
      FListDraw.ListMeasureHeight(Items[0], Canvas, IH);
      FListDraw.ListMeasureWidth(Items[0], Canvas, IW);
      if MH<IH then MH  :=  IH;
      if MW<IW then MW  :=  IW;
    end;
    if ItemHeight<MH then
      ItemHeight  :=  MH;
    if Width<MW then
      Width :=  MW;
  end
  else
  begin
    MW := Width;
    for I := 0 to Items.Count - 1 do
    begin
      IW := Canvas.TextWidth(Items[I]);
      if IW > MW then MW := IW;
    end;
    Width := MW;
  end;
  if (FDropDownRows>0) and (Items.Count>=FDropDownRows) then
    Height  :=  FDropDownRows * ItemHeight  + 4
  else
    Height  :=  Items.Count * ItemHeight + 4;
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Value: String;
  wp: IWideProperty;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
  begin
    Value :=  Items[ItemIndex];
    if Supports(FProperty, IWideProperty, wp) then
      wp.SetValue(Value)
    else
      FProperty.SetValue(Value);
//    TuPropertyInplaceEdit(Owner).UpdateProperty;
  end;
  TuPropertyInplaceEdit(Owner).CloseDropDown;
end;

function TPopupListbox.NextValue: String;
var
  Idx: Integer;
begin
  Idx :=  ItemIndex + 1;
  if Idx>Items.Count-1 then
    Idx :=  0;
  ItemIndex :=  Idx;
  Result  :=  Items[ItemIndex];
end;

procedure TPopupListbox.SelectDefault;
var
  I: Integer;
begin
  ItemIndex :=  -1;
  for I := 0 to Items.Count - 1 do
    if SameText(FDefaultValue, Items.Strings[I]) then
    begin
      ItemIndex :=  I;
      Break;
    end;
end;

procedure TPopupListbox.SetProperty(Value: IProperty; const DropDownRows: Integer);
const DrawState: array[boolean] of TListBoxStyle=(lbStandard, lbOwnerDrawVariable);
var
  wp: IWideProperty;
begin
  FDropDownRows :=  DropDownRows;
  FProperty     :=  Value;
  FCustomDraw   :=  Supports(FProperty, ICustomPropertyListDrawing, FListDraw);
  Style         :=  DrawState[FCustomDraw];
  if Supports(FProperty, IWideProperty, wp) then
    FDefaultValue :=  wp.GetValue
  else
    FDefaultValue :=  FProperty.GetValue;
  LoadValues;
end;

function TPopupListbox.ValueCount: Integer;
begin
  Result  :=  Count;
end;

{ TuPropertyInplaceEdit }

procedure TuPropertyInplaceEdit.BoundsChanged;
var
  R, CellR, ProRect: TRect;
  Draw80: ICustomPropertyDrawing80;
  _CutLeft: Integer;
begin
  CellR     :=  CurrentRect;
  _CutLeft  :=  0;
  if (FActiveProperty<>nil) and Supports(FActiveProperty._Property, ICustomPropertyDrawing80, Draw80) then
  begin
    ProRect   :=  Draw80.PropDrawValueRect(CellR);
    _CutLeft  :=  ProRect.Right - ProRect.Left;
  end;
  with CellR do
      SetWindowPos(Handle, HWND_TOP, CellR.Left+_CutLeft, Top, Right - Left - _CutLeft, Bottom - Top,
        SWP_SHOWWINDOW or SWP_NOREDRAW);
  SetRect(R, 2, 2, Width - 2, Height);
  if EditStyle <> esSimple then
    if not Grid.UseRightToLeftAlignment then
      Dec(R.Right, ButtonWidth)
    else
      Inc(R.Left, ButtonWidth - 2);
//  SendStructMessage(Handle, EM_SETRECTNP, 0, R);
  SendMessage(Handle, EM_SETRECTNP, 0, Windows.LPARAM(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

function TuPropertyInplaceEdit.ButtonRect: TRect;
begin
  if not Grid.UseRightToLeftAlignment then
    Result := Rect(Width - ButtonWidth, 0, Width, Height)
  else
    Result := Rect(0, 0, ButtonWidth, Height);
end;

procedure TuPropertyInplaceEdit.CloseDropDown;
var
  oldValue: String;
begin
  CloseUp(True);
  oldValue  :=  Text;
  TGridCrack(Grid).InvalidateEditor;
  TGridCrack(Grid).InvalidateGrid;
  if not SameText(oldValue, Text) then
  begin
    TGridCrack(Grid).InvalidateGrid;
    TGridCrack(Grid).DrawCell(1, TGridCrack(Grid).Row,
      TGridCrack(Grid).CellRect(1, TGridCrack(Grid).Row), [gdSelected]);
    if FActiveProperty.ISReference then //如果是Component属性,则需要通知改变其子属性列表
      FActiveProperty.RefreshReferenceProperties;
  end;
end;

procedure TuPropertyInplaceEdit.CloseUp(Accept: Boolean);
begin
  if ListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
  end;
end;

procedure TuPropertyInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> ActiveList) then
    CloseUp(False);
end;

procedure TuPropertyInplaceEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  if ThemeServices.ThemesEnabled and not FMouseInControl then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TuPropertyInplaceEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if ThemeServices.ThemesEnabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

constructor TuPropertyInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth      := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle        := esSimple;
  FActiveProperty   :=  nil;
  FActiveList       :=  nil;
  FDefaultPickList  :=  nil;
end;

function TuPropertyInplaceEdit.CurrentRect: TRect;
begin
  Result  :=  TGridCrack(Grid).CellRect(TGridCrack(Grid).Col, TGridCrack(Grid).Row);
end;

procedure TuPropertyInplaceEdit.DblClick;

  function GetMethodName(AMethod: TMethodProperty): String;
  var
    I: Integer;
    TrimmedEventName: String;
  begin
    if AMethod.GetComponent(0) = AMethod.Designer.GetRoot then
    begin
      Result := AMethod.Designer.GetRootClassName;
      if (Result <> '') and (Result[1] = 'T') then
        Delete(Result, 1, 1);
    end
    else
    begin
      Result := AMethod.Designer.GetObjectName(AMethod.GetComponent(0));
      for I := Length(Result) downto 1 do
        if Result[I] in ['.', '[', ']', '-', '>'] then
          Delete(Result, I, 1);
    end;
    if Result = '' then
      raise EDesignPropertyError.CreateRes(@SCannotCreateName);

    TrimmedEventName  :=  AMethod.GetName;
    if (Length(TrimmedEventName) >= 2) and
      (TrimmedEventName[1] in ['O', 'o']) and (TrimmedEventName[2] in ['N', 'n']) then
      Delete(TrimmedEventName,1,2);
    Result := Result + TrimmedEventName;
  end;

var
  Index: Integer;
  ListValue: string;

  mp: TMethodProperty;
  m: TMethod;
  methodName: String;
begin
  if TObject(FActiveProperty._Property) is TMethodProperty then
  begin
    mp :=  TMethodProperty(FActiveProperty._Property);
    if mp.GetValue='' then
    begin
      methodName  :=  GetMethodName(mp);
      if mp.Designer.MethodExists(methodName) then
        mp.Designer.ShowMethod(methodName)
      else
      begin
          m :=  mp.Designer.CreateMethod(methodName, GetTypeData(mp.GetPropType));
          if (m.Code<>nil) and (m.Data<>nil) then
            TPropertyEditorCrack(mp).SetMethodValue(m);
      end;
    end
    else
      mp.Designer.ShowMethod(mp.GetValue);
    mp :=  nil;
  end
  else
    case EditStyle of
      esSimple:   inherited;
      esEllipsis: FActiveProperty._Property.Edit;
      esPickList:
      begin
        if paDialog in FActiveProperty._Property.GetAttributes then
        begin
          FActiveProperty._Property.Edit;
          TPropertyEditor(FActiveProperty._Property).Designer.Modified;
        end
        else
        begin
          (ActiveList as IPropertyList).SetProperty(FActiveProperty._Property, FDropDownRows);
          if (ActiveList as IPropertyList).ValueCount>0 then
          begin
            ListValue :=  (ActiveList as IPropertyList).NextValue;
            Text      :=  ListValue;
            FActiveProperty._Property.SetValue(ListValue);
            SetPropertyValue;
          end;
        end;
      end;
    end;
  TGridCrack(Grid).InvalidateEditor;
  TGridCrack(Grid).DrawCell(TGridCrack(Grid).Col, TGridCrack(Grid).Row,
                            TGridCrack(Grid).CellRect(1, TGridCrack(Grid).Row), [gdSelected]);
  SelectAll;
end;

procedure TuPropertyInplaceEdit.DoDropDown;
var
  P: TPoint;
  R: TRect;
  X,Y: Integer;
begin
  (ActiveList as IPropertyList).SetProperty(FActiveProperty._Property, FDropDownRows);
  if (ActiveList as IPropertyList).ValueCount>0 then
  begin
    TControlCrack(ActiveList).Color :=  Color;
    TControlCrack(ActiveList).Font  :=  Font;
    R :=  CurrentRect;
    if ActiveList.Width<(R.Right-R.Left) then
      ActiveList.Width  :=  R.Right - R.Left;
    P := Parent.ClientToScreen(Point(R.Left, R.Top));
    Y :=  P.Y + Height;
    X :=  P.X;
    if X + ActiveList.Width > Screen.Width then X :=  Screen.Width - ActiveList.Width;
    if Y + ActiveList.Height > Screen.Height then Y := P.Y - ActiveList.Height;
    SetWindowPos(ActiveList.Handle, HWND_TOP, X, Y, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

procedure TuPropertyInplaceEdit.DoDropDownKeys(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if ListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if ListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TuPropertyInplaceEdit.DoEditButtonClick;
begin
  if FActiveProperty<>nil then
    FActiveProperty._Property.Edit;
end;

procedure TuPropertyInplaceEdit.DropDown;
var
  p80: IProperty80;
begin
  if not ListVisible then
  begin
    ActiveList.Width := Width;
    if Supports(FActiveProperty._Property, IProperty80, p80) then
      p80.Edit(Self, False);
    DoDropDown;
  end;
end;

procedure TuPropertyInplaceEdit.DropDownControl(Control: TPersistent);
begin
  if (Control is TWinControl) and Supports(Control, IPropertyList) then
  begin
    FActiveList  :=  Control as TWinControl;
    DoDropDown;
  end;
end;

function TuPropertyInplaceEdit.GetDropDownWidth: Integer;
var
  R: TRect;
begin
  R :=  ClientRect;
  Result  :=  R.Right - R.Left;
end;

function TuPropertyInplaceEdit.GetPickList: TCustomListbox;
var
  PopupListbox: TPopupListbox;
begin
  if not Assigned(FDefaultPickList) then
  begin
    PopupListbox := TPopupListbox.Create(Self);
    PopupListbox.Visible := False;
    PopupListbox.Parent := Self;
    PopupListbox.IntegralHeight := True;
    PopupListbox.ItemHeight := 11;
    FDefaultPickList := PopupListBox;
  end;
  Result := FDefaultPickList;
end;

procedure TuPropertyInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    DoEditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TuPropertyInplaceEdit.KeyPress(var Key: Char);
begin
  inherited;
end;

procedure TuPropertyInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (FActiveProperty<>nil) and ((Key=13) or (paAutoUpdate in FActiveProperty._Property.GetAttributes)) then
    SetPropertyValue
  else
    inherited;
end;

procedure TuPropertyInplaceEdit.ListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(ActiveList.ClientRect, Point(X, Y)));
end;

procedure TuPropertyInplaceEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if ListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(ActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TuPropertyInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if ListVisible then
    begin
      ListPos := ActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(ActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        SendMessage(ActiveList.Handle, WM_LBUTTONDOWN, 0, PointToLParam(ListPos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TuPropertyInplaceEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := Pressed;
  StopTracking;
  if (Button = mbLeft) and (EditStyle = esEllipsis) and WasPressed then
    DoEditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TuPropertyInplaceEdit.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TuPropertyInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
  Details: TThemedElementDetails;
begin
  if EditStyle <> esSimple then
  begin
    R := ButtonRect;
    Flags := 0;
    case EditStyle of
      esPickList:
        begin
          if ThemeServices.ThemesEnabled then
          begin
            if ActiveList = nil then
              Details := ThemeServices.GetElementDetails(tcDropDownButtonDisabled)
            else
              if Pressed then
                Details := ThemeServices.GetElementDetails(tcDropDownButtonPressed)
              else
                if FMouseInControl then
                  Details := ThemeServices.GetElementDetails(tcDropDownButtonHot)
                else
                  Details := ThemeServices.GetElementDetails(tcDropDownButtonNormal);
            ThemeServices.DrawElement(DC, Details, R);
          end
          else
          begin
            if ActiveList = nil then
              Flags := DFCS_INACTIVE
            else if Pressed then
              Flags := DFCS_FLAT or DFCS_PUSHED;
            DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
          end;
        end;
      esEllipsis:
        begin
          if ThemeServices.ThemesEnabled then
          begin
            if Pressed then
              Details := ThemeServices.GetElementDetails(tbPushButtonPressed)
            else
              if FMouseInControl then
                Details := ThemeServices.GetElementDetails(tbPushButtonHot)
              else
                Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
            ThemeServices.DrawElement(DC, Details, R);
          end
          else
          begin
            if Pressed then Flags := BF_FLAT;
            DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
          end;

          X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(Pressed);
          Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(Pressed);
          W := ButtonWidth shr 3;
          if W = 0 then W := 1;
          PatBlt(DC, X, Y, W, W, BLACKNESS);
          PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
          PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
        end;
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TuPropertyInplaceEdit.RestoreContents;
begin
  Reset;
//  TGridCrack(Grid).UpdateText;
end;

procedure TuPropertyInplaceEdit.SetActiveProperty(const Value: TuPropertyRec);
begin
  if FActiveProperty<>Value then
  begin
    FActiveProperty :=  Value;
    MaxLength       :=  Value._Property.GetEditLimit;
  end;
end;

procedure TuPropertyInplaceEdit.SetPropertyValue;
var
  wp: IWideProperty;
begin
  if Modified and (FActiveProperty<>nil) then
  begin
    try
      if Supports(FActiveProperty._Property, IWideProperty, wp) then
      begin
        wp.SetValue(Text);
        wp  :=  nil;
      end
      else
        FActiveProperty._Property.SetValue(Text);
      if FActiveProperty.ISReference then //如果是Component属性,则需要通知改变其子属性列表
        FActiveProperty.RefreshReferenceProperties;
      FActiveProperty._Property.Revert;
    except
      Text  :=  FActiveProperty._Property.GetValue;
    end;
    Modified  :=  False;
  end;
end;

procedure TuPropertyInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TuPropertyInplaceEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TuPropertyInplaceEdit.UpdateContents;
begin
  ActiveList := nil;
  PickListLoaded := False;
  FEditStyle := TGridCrack(Grid).GetEditStyle(TGridCrack(Grid).Col, TGridCrack(Grid).Row);
  if EditStyle = esPickList then
    ActiveList := DefaultPickList;
  inherited UpdateContents;
end;

procedure TuPropertyInplaceEdit.UpdateProperty;
begin
  SetPropertyValue;
end;

procedure TuPropertyInplaceEdit.WMCancelMode(var Message: TWMCancelMode);
begin
  StopTracking;
  inherited;
end;

procedure TuPropertyInplaceEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  if not SysLocale.FarEast then
  begin
    inherited;
  end else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.FocusedWnd) <> Grid.Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
  SetPropertyValue;
end;

procedure TuPropertyInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (EditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TuPropertyInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TuPropertyInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (EditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TuPropertyInplaceEdit.WndProc(var Message: TMessage);
var
  TheChar: Word;
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle = esPickList then
      with TWMKey(Message) do
      begin
        TheChar := CharCode;
        DoDropDownKeys(TheChar, KeyDataToShiftState(KeyData));
        CharCode := TheChar;
        if (CharCode <> 0) and ListVisible then
        begin
          with Message do
            SendMessage(ActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

end.
