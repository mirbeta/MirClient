unit uPropertyGrid;

interface

uses
  Classes,
  Sysutils,
  Grids,
  Windows, Messages, Graphics, Controls,
  StdCtrls, uPropertyManager, uDesignIntf, TypInfo, Dialogs;

type
  TRttiGridStyle  = (rsStandard, rsFlat);
  TRDOnDrawProperty = procedure(AProperty: IProperty; var Value: String) of Object;
  TRttiGridType = (rgProperties, rgMethods); //指定表格所属的
  TuCustomPropertyGrid = class(TCustomDrawGrid, IuRttiPainter)
  private
    FRttiGridType: TRttiGridType;
    FExpandProp: TuExpandProperties;
    FSelProp: TuPropertyRec;
    FMinNameWidth,    //属性名称最小宽度
    FMinValueWidth,   //属性值最小宽度
    FOldX: Integer;
    FInMidline: boolean;
    FCanShowEditor,
    FInDrag: Boolean;
    FNameWidth: Integer;
    FValueWidth: Integer;
    FOnDrawPropertyName,
    FOnDrawPropertyValue: TRDOnDrawProperty;
    FPropertyManager: TuCustomPropertiesManager;
    FGridStyle: TRttiGridStyle;
    procedure SetNameWidth(const Value: Integer);
    procedure SetValueWidth(const Value: Integer);
    function SetColumWidth(const PropWidth: Integer; const AutoCorrect: boolean=False): boolean;
    procedure SetPropertyManager(const Value: TuCustomPropertiesManager);
    procedure SetRttiGridType(const Value: TRttiGridType);
    procedure RebuildGrid;
    procedure SetGridStyle(const Value: TRttiGridStyle);
  protected
    procedure PaintProperties(Path: TuLastPropertyPath; const SameClass: boolean=False);
    procedure ChangeProperties;
    function GetActivePropertyRec: TuPropertyRec;
    procedure IuRttiPainter.Refresh=DoPainrerRefresh;
    procedure DoPainrerRefresh;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);

    function GetStateRect(const ARow: Integer): TRect;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure SetFocus; override;
    procedure Resize; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function CanEditModify: Boolean; override;
    procedure DblClick; override;
    function GetEditText(ACol, ARow: Longint): string; override;

    function CurrentProperty: IProperty;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property MinNameWidth: Integer read FMinNameWidth write FMinNameWidth;
    property MinValueWidth: Integer read FMinValueWidth write FMinValueWidth;
    property NameWidth: Integer read FNameWidth write SetNameWidth;
    property ValueWidth: Integer read FValueWidth write SetValueWidth;
    property PropertyManager: TuCustomPropertiesManager read FPropertyManager write SetPropertyManager;
    property GridType: TRttiGridType read FRttiGridType write SetRttiGridType;
    property GridStyle: TRttiGridStyle read FGridStyle write SetGridStyle;
    property OnDrawPropertyName: TRDOnDrawProperty read FOnDrawPropertyName write FOnDrawPropertyName;
    property OnDrawPropertyValue: TRDOnDrawProperty read FOnDrawPropertyValue write FOnDrawPropertyValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
    procedure InvalidateEditor;
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateRow(ARow: Longint);
    procedure HideEditor;
    procedure ShowEditor;
  end;

  TuPropertyGrid = class(TuCustomPropertyGrid)
  published
    property PropertyManager;
    property GridType;
    property Align;
    property OnDrawPropertyName;
    property OnDrawPropertyValue;
  end;

implementation
  uses uPropertyInplaceEdit, uVCLEditors, uDesignEditors;

{ TCustomWorkDrawGrid }
const
  ReadOnlyColorV  = clGray;
  ReferenceColorV = clGreen;
  PersistentColorV= clMaroon;
  StandardColorV  = clBlack;
  ValueColorV     = clNavy;
type
  TColorKind  = (ReadOnlyColor, ReferenceColor, PersistentColor, StandardColor, ValueColor);
  TRDCrackPropertiesManager  = class(TuCustomPropertiesManager);

const
  ColorMap: array[TColorKind] of TColor = (ReadOnlyColorV, ReferenceColorV, PersistentColorV, StandardColorV, ValueColorV);

//画出含有子属性的属性前置标志
procedure DrawPropertyState(Canvas: TCanvas; ARect: TRect; const Level: Integer; Expand: boolean);
const ExpandStr: array[boolean] of Char = ('-', '+');
var
  _Top, _Left, _FH: Integer;
  R: TRect;
begin
  _FH       :=  9;
  _Top      :=  ARect.Top  + Round((ARect.Bottom-ARect.Top-_FH)/2);
  _Left     :=  ARect.Left + 4 + (Level*12);
  R.Left    :=  _Left;
  R.Top     :=  _Top;
  R.Bottom  :=  R.Top   + _FH;
  R.Right   :=  R.Left  + _FH;
  //画外边方框
  DrawEdge(Canvas.Handle, R, EDGE_BUMP, BF_RECT or BF_FLAT);
  ///先画一横
  Canvas.Pen.Color  :=  clBlack;
  Canvas.MoveTo(R.Left + 2, R.Top + 4);
  Canvas.LineTo(R.Right-2, R.Top + 4);
  ///如果出于收起状态,则再画一竖
  if not Expand then
  begin
    Canvas.MoveTo(R.Left + 4, R.Top+2);
    Canvas.LineTo(R.Left + 4, R.Bottom-2);
  end;
end;

function GetColorMap(const Prop: TuPropertyRec): TColorKind;
begin
  if Prop.ISReference then
    Result  :=  PersistentColor
  else if Prop.ISReferenceSubProperty then
    Result  :=  ReferenceColor
  else if ([paReadOnly, paSubProperties, paDialog]*Prop._Property.GetAttributes=[paReadOnly]) then
    Result  :=  ReadOnlyColor
  else
    Result  :=  StandardColor;
end;

procedure DrawPropName(ACanvas: TCanvas; Rect: TRect; const Level: Integer; const Value: String; Color: TColorKind=StandardColor);
var
  FH: Integer;
  _Top: Integer;
  _Left:  Integer;
  TxtR: TRect;
begin
  FH          :=  ACanvas.TextHeight(Value);
  _Top        :=  Rect.Top  + Round((Rect.Bottom-Rect.Top - FH)/2);
  _Left       :=  Rect.Left + 2 + (Level+1)*14;
  TxtR.Top    :=  _Top;
  TxtR.Left   :=  _Left;
  TxtR.Bottom :=  TxtR.Top  + FH;
  TxtR.Right  :=  Rect.Right - 2;
  ACanvas.Font.Color  :=  ColorMap[Color];
  ACanvas.TextRect(TxtR, TxtR.Left, TxtR.Top, Value);
end;

procedure DrawPropValue(ACanvas: TCanvas; ARect: TRect; const Value: String; ReadOnly: boolean;
  const ISDefault: boolean);
const FontStyle: array[boolean] of TFontStyles=([fsBold], []);
    FontColor: array[boolean] of TColor=(ValueColorV, ReadOnlyColorV);
var
  _Top: Integer;
  _Left:  Integer;
begin
  ACanvas.Font.Color  :=  FontColor[ReadOnly];
  ACanvas.Font.Style  :=  FontStyle[ISDefault];
  _Top  :=  ARect.Top  + Round((ARect.Bottom-ARect.Top - ACanvas.TextHeight(Value))/2);
  _Left :=  ARect.Left + 2;
  ACanvas.TextOut(_Left, _Top, Value);
end;

function TuCustomPropertyGrid.CanEditModify: Boolean;
var
  Prop: TuPropertyRec;
begin
  Result  :=  False;
  if (FExpandProp=nil) or (FExpandProp.Count=0) or (Row>FExpandProp.Count-1) then Exit;

  if Col=1 then
  begin
    Prop  :=  FExpandProp.Items[Row];
    if Prop<>nil then
      Result  :=  not (paReadOnly in Prop._Property.GetAttributes);
  end;
end;

procedure TuCustomPropertyGrid.ChangeProperties;
begin
  if FExpandProp<>nil then
    RowCount  :=  FExpandProp.Count
  else
    RowCount  :=  1;
  InvalidateEditor;
end;

constructor TuCustomPropertyGrid.Create(AOwner: TComponent);
begin
  inherited;
  FRttiGridType   :=  rgProperties;
  ScrollBars      :=  ssVertical;
  FInMidline      :=  False;
  FInDrag         :=  False;
  FCanShowEditor  :=  True;
  FixedCols       :=  0;
  FixedRows       :=  0;
  ColCount        :=  2;
  RowCount        :=  1;
  FMinNameWidth   :=  120;
  FMinValueWidth  :=  40;
  DefaultRowHeight:=  16;
  ColWidths[0]    :=  170;
  ColWidths[1]    :=  145;
  Options         :=  Options - [goRangeSelect] + [goEditing, goAlwaysShowEditor];
  FPropertyManager  :=  nil;
  FExpandProp       :=  nil;
end;

function TuCustomPropertyGrid.CreateEditor: TInplaceEdit;
begin
  Result := TuPropertyInplaceEdit.Create(Self);
  (Result as TuPropertyInplaceEdit).DropDownRows        :=  8;
end;

function TuCustomPropertyGrid.CurrentProperty: IProperty;
begin
  Result  :=  nil;
  if FSelProp<>nil then
    Result  :=  FSelProp._Property;
end;

procedure TuCustomPropertyGrid.DblClick;
var
  P: TPoint;
  ValueR, PropR: TRect;
begin
  inherited;
  if FSelProp<>nil then
  begin
    ValueR  :=  CellRect(1, Row);
    PropR   :=  CellRect(0, Row);
    GetCursorPos(P);
    P :=  ScreenToClient(P);
    if ptInRect(ValueR, P) then
    begin
      if paDialog in FSelProp._Property.GetAttributes then
        FSelProp._Property.Edit;
    end
    else if ptInRect(PropR, P) then
    begin
      FSelProp.Expand  :=  not FSelProp.Expand;
      InvalidateGrid;
    end;
  end;
end;

destructor TuCustomPropertyGrid.Destroy;
begin
  if FPropertyManager<>nil then
    case FRttiGridType of
      rgProperties: FPropertyManager.PropertyPainter  :=  nil;
      rgMethods:    FPropertyManager.MethodPainter    :=  nil;
    end;
  inherited;
end;

function TuCustomPropertyGrid.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if InplaceEditor.Visible and (TopRow+VisibleRowCount<RowCount) then
    InvalidateEditor;
  SendMessage(Handle, WM_VSCroll, SB_LINEDOWN, 0);
  Result := true;
end;

function TuCustomPropertyGrid.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if InplaceEditor.Visible and (TopRow<>0) then
    InvalidateEditor;
  SendMessage(Handle, WM_VSCroll, SB_LINEUP, 0);
  Result := true;
end;

procedure TuCustomPropertyGrid.DoPainrerRefresh;
begin
  InvalidateGrid;
end;

procedure TuCustomPropertyGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);

  procedure DrawActivePropName;
  begin
    Canvas.Brush.Color  :=  cl3DLight;
    Canvas.FillRect(ARect);
    Canvas.Pen.Color :=  clBtnface;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(Arect.Right, ARect.Top);
    Canvas.MoveTo(ARect.Left, ARect.Bottom);
    Canvas.LineTo(Arect.Right, ARect.Bottom);
  end;

  function GetValue(AProperty: IProperty): String;
  var
    wp: IWideProperty;
  begin
    Result  :=  AProperty.GetValue;
    if Supports(AProperty, IWideProperty, wp) then
      Result  :=  wp.GetValue;
    wp  :=  nil;
  end;

var
  R: TRect;
  Prop: TuPropertyRec;
  Value: String;
  FDrawing: ICustomPropertyDrawing;
  FDrawing80: ICustomPropertyDrawing80;
  ISDefValue: boolean;
  IP70: IProperty70;
  ValueRect: TRect;
begin
  if ACol=0 then
  begin
    R         :=  ARect;
    R.Left    :=  ARect.Right;
    R.Right   :=  R.Left+2;
    R.Bottom  :=  R.Bottom  + 1;
    DrawEdge(Canvas.Handle, r, EDGE_ETCHED, BF_LEFT or BF_RIGHT);
  end
  else
  begin
    Canvas.Brush.Color  :=  Color;
    Canvas.FillRect(ARect);
  end;
  if (ARow=Row) and (ACol=0) then
    DrawActivePropName;

  if Assigned(FExpandProp) and (ARow<FExpandProp.Count) then
  begin
    Prop  :=  FExpandProp.Items[ARow];
    if (Prop<>nil) and (Prop._Property<>nil) then
      case ACol of
        0:
        begin
          Value :=  Prop._Property.GetName;
          if Assigned(FOnDrawPropertyName) then
            FOnDrawPropertyName(Prop._Property, Value);

          if Prop.HasChild then
            DrawPropertyState(Canvas, ARect, Prop.OwnerProperty.Level, Prop.Expand);
          DrawPropName(Canvas, ARect, Prop.OwnerProperty.Level, Value, GetColorMap(Prop));
        end;
        1:
        begin
          Value :=  GetValue(Prop._Property);//TPropertyEditor(Prop._Property).GetValue;
          if Assigned(FOnDrawPropertyValue) then
            FOnDrawPropertyValue(Prop._Property, Value);
          if Supports(Prop._Property, IProperty70, IP70) then
            ISDefValue  :=  IP70.IsDefault;
          IP70  :=  nil;
          ValueRect :=  ARect;

          if Supports(Prop._Property, ICustomPropertyDrawing, FDrawing) then
          begin
            if Supports(Prop._Property, ICustomPropertyDrawing80, FDrawing80) then
            begin
              ValueRect :=  FDrawing80.PropDrawValueRect(ARect);
              ARect.Left  :=  ValueRect.Right;
            end;
            FDrawing.PropDrawValue(Canvas, ValueRect, Row=ARow);
          end;
          DrawPropValue(Canvas, ARect, Value, not Prop.ISReference and
              ([paReadOnly, paSubProperties, paDialog]*Prop._Property.GetAttributes=[paReadOnly]),
              ISDefValue);
        end;
      end;
  end
  else
    inherited;
end;

procedure TuCustomPropertyGrid.FocusCell(ACol, ARow: Integer;
  MoveAnchor: Boolean);
begin
  inherited FocusCell(ACol, ARow, MoveAnchor);
end;

function TuCustomPropertyGrid.GetActivePropertyRec: TuPropertyRec;
begin
  Result  :=  FSelProp;
end;

function TuCustomPropertyGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
  Prop: TuPropertyRec;
  AttriB: TPropertyAttributes;
begin
  case ACol of
    0:  Result  :=  esSimple;
    1:
      if (FExpandProp<>nil) and (FExpandProp.Count>0) and (ARow<FExpandProp.Count) then
      begin
        Prop    :=  FExpandProp.Items[ARow];
        if Prop<>nil then
        begin
          AttriB  :=  Prop._Property.GetAttributes;
          if paValueList in AttriB then
            Result  :=  esPickList
          else if paDialog in AttriB then
            Result  :=  esEllipsis
          else
            Result  :=  esSimple;
        end;
      end;
  end;
end;

function TuCustomPropertyGrid.GetEditText(ACol, ARow: Integer): string;
var
  Prop: TuPropertyRec;
begin
  if (FExpandProp<>nil) and (FExpandProp.Count>0) and (ARow<FExpandProp.Count) then
  begin
    Prop  :=  FExpandProp.Items[ARow];
    if Prop<>nil then
      case ACol of
        0:  Result  :=  Prop._Property.GetName;
        1:  Result  :=  Prop._Property.GetValue;
      end;
  end;
end;

function TuCustomPropertyGrid.GetStateRect(const ARow: Integer): TRect;
var
  Lv: Integer;
  R: TRect;
begin
  if FSelProp<>nil then
  begin
    R   :=  CellRect(0, ARow);
    Lv  :=  FSelProp.OwnerProperty.Level;
    Result.Left :=  R.Left + 4 + Lv*12;
    Result.Right  :=  Result.Left + 9;
    Result.Top    :=  R.Top + Round((R.Bottom-R.Top-9)/2);
    Result.Bottom :=  Result.Top  + 9;
  end;
end;

procedure TuCustomPropertyGrid.HideEditor;
begin
  inherited  HideEditor;
end;

procedure TuCustomPropertyGrid.InvalidateCell(ACol, ARow: Integer);
begin
  inherited InvalidateCell(ACol, ARow);
end;

procedure TuCustomPropertyGrid.InvalidateCol(ACol: Integer);
begin
  inherited InvalidateCol(ACol);
end;

procedure TuCustomPropertyGrid.InvalidateEditor;
begin
  inherited InvalidateEditor;
end;

procedure TuCustomPropertyGrid.InvalidateRow(ARow: Integer);
begin
  inherited InvalidateRow(ARow);
end;

procedure TuCustomPropertyGrid.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin
  if not InplaceEditor.Visible then
    FocusCell(1, Row, True);
  InplaceEditor.Text :=  EditKey;
  InplaceEditor.SetFocus;
  //光标位置
  InplaceEditor.SelStart  :=  Length(InplaceEditor.Text);
  InplaceEditor.SelLength :=  0;
end;

procedure TuCustomPropertyGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Handled: boolean;
  IPropMsg: ICustomPropertyMessage;
  NameR,
  CustomR: TRect;
begin
//  inherited;
  if FInMidline then
  begin
    FOldX   :=  X;
    FInDrag :=  True;
  end
  else
  begin
    Handled :=  false;
    if (CurrentProperty<>nil) and Supports(CurrentProperty, ICustomPropertyDrawing) and Supports(CurrentProperty, ICustomPropertyMessage, IPropMsg) then
    begin
      NameR         :=  CellRect(0, Row);
      CustomR       :=  CellRect(1, Row);
      CustomR.Right :=  CustomR.Left  + CustomR.Bottom - CustomR.Top;
      IPropMsg.MouseDown(Button, Shift, X, Y, ptInRect(NameR, Point(X, Y)), CustomR, Handled);
    end;
    if not Handled then
      inherited;
  end;
end;

procedure TuCustomPropertyGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
const MidLineCusor: array[boolean] of TCursor=(crDefault, crHSplit);
var
  R, FirstR:  TRect;
  UseableW,
  NewNameW,
  NewValueW: Integer;

  Handled: boolean;
  IPropMsg: ICustomPropertyMessage;
  NameR,
  CustomR: TRect;
begin
  if not FInDrag then
  begin
    FirstR      :=  BoxRect(0, 0, 0, RowCount-1);
    R.Left      :=  FirstR.Right-1;
    R.Right     :=  FirstR.Right+2;
    R.Top       :=  FirstR.Top;
    R.Bottom    :=  FirstR.Bottom;
    FInMidline  :=  ptInRect(R, Point(X, Y));
    Cursor      :=  MidLineCusor[FInMidline];
    Handled     :=  False;
    if not FInMidline then
      if (CurrentProperty<>nil) and Supports(CurrentProperty, ICustomPropertyDrawing) and Supports(CurrentProperty, ICustomPropertyMessage, IPropMsg) then
      begin
        NameR :=  CellRect(0, Row);
        CustomR :=  CellRect(1, Row);
        CustomR.Right :=  CustomR.Left  + CustomR.Bottom - CustomR.Top;
        IPropMsg.MouseMove(Shift, X, Y, ptInRect(NameR, Point(X, Y)), CustomR, Handled);
      end;
    if not Handled then
      inherited;
  end
  else
  begin
    NewNameW  :=  ColWidths[0] + (X-FOldX);
    if SetColumWidth(NewNameW) then
      FOldX :=  X;
  end;
end;

function CBRect(const ItemRect: TRect): TRect;
begin
  Result := Rect(ItemRect.Right + 2, ItemRect.Top,
    itemrect.Right + Itemrect.Bottom - ItemRect.Top + 2, ItemRect.Bottom);
end;

procedure TuCustomPropertyGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Handled: boolean;
  IPropMsg: ICustomPropertyMessage;
  NameR,
  CustomR: TRect;
begin
  Handled :=  false;
  if not FInDrag and (CurrentProperty<>nil) and
    Supports(CurrentProperty, ICustomPropertyDrawing) and
    Supports(CurrentProperty, ICustomPropertyMessage, IPropMsg) then
  begin
    NameR         :=  CellRect(0, Row);
    IPropMsg.MouseUp(Button, Shift, X, Y, ptInRect(NameR, Point(X, Y)), NameR, Handled);
    if Handled then
    begin
      InvalidateEditor;
      if CurrentProperty.GetPropType^.Kind in [tkSet, tkEnumeration] then
        InvalidateGrid;
      DrawCell(Col, Row, CellRect(Col, Row), [gdSelected]);
    end;
  end;

  FInDrag :=  False;
  inherited;
end;

procedure TuCustomPropertyGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FPropertyManager) then
  begin
    FPropertyManager  :=  nil;
    RebuildGrid;
  end;
  inherited;
end;

procedure TuCustomPropertyGrid.PaintProperties(Path: TuLastPropertyPath; const SameClass: boolean);

  //从保存的路径中查找新的表是否含有相同的路径
  function FindActiveItem: Integer;
  var
    I, J, _EIdx: Integer;
    LPropInfo, RPropInfo: PPropInfo;
  begin
    Result  :=  0;
    _EIdx   :=  0;
    for I := 0 to Path.Count - 1 do
    begin
      LPropInfo  :=  Path.Items[I];
      for J := _EIdx to FExpandProp.Count - 1 do
      begin
        RPropInfo :=   FExpandProp.Items[J]._Property.GetPropInfo;
        if SameText(LPropInfo^.Name, RPropInfo^.Name) and (LPropInfo^.PropType=RPropInfo^.PropType) then
        begin
          if I = Path.Count-1 then
          begin
            Result  :=  J;
            Exit;
          end;
          Break;
        end;
        _EIdx :=  J;
      end;
      if J=FExpandProp.Count-1 then Exit;
    end;
  end;
var
  _LTimeRow: Integer;
begin
  _LTimeRow   :=  Row;
  FSelProp    :=  nil;
  ChangeProperties;
  InvalidateGrid;

  FCanShowEditor  :=  False;
  HideEditor;
  if SameClass then
    SelectCell(1, _LTimeRow) //如果和上次类型一致则直接选择上次的行做当前行
//    FocusCell(1, _LTimeRow, False)
  else
    SelectCell(1, FindActiveItem);  //如果不是则查找是否包含同样属性的行
//    FocusCell(1, FindActiveItem, False);
  FCanShowEditor  :=  True;
end;

procedure TuCustomPropertyGrid.RebuildGrid;
begin
  if FPropertyManager<>nil then
    case FRttiGridType of
      rgProperties:
      begin
        FExpandProp  :=  FPropertyManager.ExpandProperties;
        FPropertyManager.PropertyPainter  :=  Self;
      end;
      rgMethods:
      begin
        FExpandProp  :=  FPropertyManager.ExpandMethodProperties;
        FPropertyManager.MethodPainter    :=  Self;
      end;
    end
  else
    FExpandProp :=  nil;
  ChangeProperties;
  InvalidateEditor;
  InvalidateGrid;
end;

procedure TuCustomPropertyGrid.Resize;
begin
  inherited;
  SetColumWidth(ColWidths[0], True);
end;

function TuCustomPropertyGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  OldRow: Integer;
  P: TPoint;
  R: TRect;
begin
  OldRow  :=  Row;
  Result  :=  ACol<>0;
  if not Result then
    FocusCell(1, ARow, True);
  if (FExpandProp<>nil) and (FExpandProp.Count>0) and (ARow<FExpandProp.Count) then
  begin
    FSelProp :=  FExpandProp.Items[ARow];

    if (ACol=0) and FSelProp.HasChild then
    begin
      GetCursorPos(P);
      P :=  ScreenToClient(P);
      R :=  GetStateRect(ARow);
      if ptInRect(R, P) then
      begin
        FSelProp.Expand  :=  not FSelProp.Expand;
        InvalidateGrid;
      end;
    end;

    if FCanShowEditor then
    begin
      ShowEditor;
      if InplaceEditor<>nil then
      begin
        (InplaceEditor as TuPropertyInplaceEdit).UpdateProperty;
        (InplaceEditor as TuPropertyInplaceEdit).ActiveProperty  :=  FSelProp;
        InplaceEditor.SetFocus;
      end;
    end;
  end;
  InvalidateRow(ARow);
  InvalidateRow(OldRow);
end;

function TuCustomPropertyGrid.SetColumWidth(const PropWidth: Integer; const AutoCorrect: boolean): boolean;
var
  UseableW,
  NewNameW,
  NewValueW: Integer;
begin
  UseableW  :=  ClientWidth - 1;
  NewNameW  :=  PropWidth;
  NewValueW :=  UseableW  - NewNameW;
  Result    :=  AutoCorrect or not ((NewNameW<FMinNameWidth) or (NewValueW<FMinValueWidth));
  if not ((NewNameW<FMinNameWidth) or (NewValueW<FMinValueWidth)) then
  begin
    ColWidths[0]  :=  NewNameW;
    ColWidths[1]  :=  NewValueW;
  end
  else
    if AutoCorrect then
    begin
      if NewNameW<FMinNameWidth then
      begin
        NewNameW  :=  FMinNameWidth;
        NewValueW :=  UseableW  - NewNameW;
      end
      else
      begin
        NewValueW :=  FMinValueWidth;
        NewNameW  :=  UseableW - NewValueW;
      end;
      ColWidths[0]  :=  NewNameW;
      ColWidths[1]  :=  NewValueW;
    end;
end;

procedure TuCustomPropertyGrid.SetFocus;
begin
  if FPropertyManager<>nil then
    TRDCrackPropertiesManager(FPropertyManager).FActivePainter  :=  Self;
  inherited;
end;

procedure TuCustomPropertyGrid.SetGridStyle(const Value: TRttiGridStyle);
begin
  if FGridStyle<>Value then
  begin
    FGridStyle := Value;
    Invalidate;
  end;
end;

procedure TuCustomPropertyGrid.SetNameWidth(const Value: Integer);
begin
  FNameWidth := Value;
  ColWidths[0] :=  Value;
end;

procedure TuCustomPropertyGrid.SetPropertyManager(
  const Value: TuCustomPropertiesManager);
begin
  if Value<>FPropertyManager then
  begin
    FPropertyManager := Value;
    if FPropertyManager<>nil then
      FPropertyManager.FreeNotification(Self);
    RebuildGrid;
    FocusCell(1, 0, True);
  end;
end;

procedure TuCustomPropertyGrid.SetRttiGridType(const Value: TRttiGridType);
begin
  if Value<>FRttiGridType then
  begin
    FRttiGridType := Value;
    RebuildGrid;
  end;
end;

procedure TuCustomPropertyGrid.SetValueWidth(const Value: Integer);
begin
  FValueWidth := Value;
  ColWidths[1] :=  Value;
end;

procedure TuCustomPropertyGrid.ShowEditor;
begin
  inherited  ShowEditor;
end;

end.
