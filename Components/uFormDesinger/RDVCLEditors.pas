unit RDVCLEditors;

interface
uses
  Messages, Types, Classes, Windows, Graphics, Menus, Controls, Forms, StdCtrls,
  SysUtils, DesignIntf, DesignEditors, DesignMenus, ComCtrls, Dialogs, DesignConst,
  RTLConsts, ThemeS, TypInfo;

var
  FontNamePropertyDisplayFontNames: Boolean = False;

type
  //下拉的时候弹出的框最后返回的值
  IPropertyList = interface
    ['{D871D642-0B27-4BD8-BCB4-3595D83B2B6F}']
    function NextValue: String;   //获取下一个值
    function ValueCount: Integer;      //值的个数
    procedure SetProperty(Value: IProperty; const DropDownRows: Integer);///设置当前操作的属性
  end;

  //指定在非焦点
  ICustomPropertyDrawing  = interface(IInterface)
    ['{8B1F48A3-306D-4A1B-A6B5-2725238FB0EE}']
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  //说明
  ICustomPropertyDrawing80 = interface(ICustomPropertyDrawing)
    ['{BC5951B0-BE77-4D9C-BF5E-AE980FED1B5D}']
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

  ICustomPropertyListDrawing = interface
    ['{BE2B8CF7-DDCA-4D4B-BE26-2396B969F8E0}']
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  ICustomPropertyMessage  = interface(IInterface)
    ['{E383EEEC-31C4-47A8-9A4A-5EF31836F325}']
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
  end;

  TFontNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TFontCharsetProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TImeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TFontProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TModalResultProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TShortCutProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBooleanProperty  = class(TBoolProperty, ICustomPropertyDrawing,
    ICustomPropertyDrawing80, ICustomPropertyMessage)
  protected
    function CBRect(const ItemRect: TRect) : TRect;
  protected
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
  end;

  TColorProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyDrawing80,
    ICustomPropertyListDrawing)
  protected
    function PaintColorBox(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean): TRect;

    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  public
    procedure Edit; overload; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

  TRDSetProperty = class(DesignEditors.TSetProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TRDSetElementProperty = class(DesignEditors.TSetElementProperty,
    ICustomPropertyDrawing, ICustomPropertyDrawing80, ICustomPropertyMessage)
  private
    FBit : TBit;
  protected
    function CBRect(const ItemRect: TRect) : TRect;
  public
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;

    // ICustomPropertyMessage
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); reintroduce;
  end;

  TStringsProperty  = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TWideStringsProperty  = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TCursorProperty = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  TBrushStyleProperty = class(TEnumProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

  TPenStyleProperty = class(TEnumProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  public
    procedure GetValues(Proc: TGetStrProc); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

  TCaptionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  var
    CF_COMPONENTS: Word;
    CF_COMPONENT: Word;
  procedure CopyStreamToClipboard(S: TMemoryStream);
  function GetClipboardStream: TMemoryStream;

implementation
  uses WideStrings, ExtCtrls, RDStringsEdt, Math, ClipBrd;

const
  { context ids for the Font editor and the Color Editor, etc. }
  hcDFontEditor       = 25000;
  hcDColorEditor      = 25010;

procedure CopyStreamToClipboard(S: TMemoryStream);
var
  T: TMemoryStream;
  I: TValueType;
  V: Integer;

  procedure CopyToClipboard(Format: Word; S: TMemoryStream);
  var
    Handle: THandle;
    Mem: Pointer;
  begin
    Handle := GlobalAlloc(GMEM_MOVEABLE, S.Size);
    Mem := GlobalLock(Handle);
    Move(S.Memory^, Mem^, S.Size);
    GlobalUnlock(Handle);
    Clipboard.SetAsHandle(Format, Handle);
  end;

begin
  Clipboard.Open;
  try
    CopyToClipboard(CF_COMPONENTS, S);
    S.Position := 0;
    T := TMemoryStream.Create;
    try
      repeat
        S.Read(I, SizeOf(I));
        S.Seek(-SizeOf(I), 1);
        if I = vaNull then Break;
        ObjectBinaryToText(S, T);
      until False;
      V := 0;
      T.Write(V, 1);
      CopyToClipboard(CF_TEXT, T);
    finally
      T.Free;
    end;
  finally
    Clipboard.Close;
  end;
end;

function GetClipboardStream: TMemoryStream;
var
  S, T: TMemoryStream;
  Handle: THandle;
  Mem: Pointer;
  Format: Word;
  V: TValueType;

  function AnotherObject(S: TStream): Boolean;
  var
    Buffer: array[0..255] of AnsiChar;
    Position: Integer;
  begin
    Position := S.Position;
    Buffer[S.Read(Buffer, SizeOf(Buffer))-1] := #0;
    S.Position := Position;
    Result := PossibleStream(string(Buffer));
  end;

begin
  Result := TMemoryStream.Create;
  try
    if Clipboard.HasFormat(CF_COMPONENTS) then
      Format := CF_COMPONENTS else
      Format := CF_TEXT;
    Clipboard.Open;
    try
      Handle := Clipboard.GetAsHandle(Format);
      Mem := GlobalLock(Handle);
      try
        Result.Write(Mem^, GlobalSize(Handle));
      finally
        GlobalUnlock(Handle);
      end;
    finally
      Clipboard.Close;
    end;
    Result.Position := 0;
    if Format = CF_TEXT then
    begin
      S := TMemoryStream.Create;
      try
        while AnotherObject(Result) do ObjectTextToBinary(Result, S);
        V := vaNull;
        S.Write(V, SizeOf(V));
        T := Result;
        Result := nil;
        T.Free;
      except
        S.Free;
        raise;
      end;
      Result := S;
      Result.Position := 0;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure DefaultDrawPropName(ACanvas: TCanvas; Rect: TRect; const Level: Integer; const Value: String);
var
  _Top: Integer;
  _Left:  Integer;
begin
  _Top  :=  Rect.Top  + Round((Rect.Bottom-Rect.Top - ACanvas.TextHeight(Value))/2);
  _Left :=  Rect.Left + 2 + (Level+1)*14;
  ACanvas.TextOut(_Left, _Top, Value);
end;

procedure DefaultDrawPropValue(ACanvas: TCanvas; ARect: TRect; const Value: String);
var
  _Top: Integer;
  _Left:  Integer;
begin
  _Top  :=  ARect.Top  + Round((ARect.Bottom-ARect.Top - ACanvas.TextHeight(Value))/2);
  _Left :=  ARect.Left + 2;
  ACanvas.TextOut(_Left, _Top, Value);
end;

type
  TCanvasCrack = Class(TCanvas);

function TextExtentW(Canvas: TCanvas; const Text: Widestring): TSize;
begin
  with TCanvasCrack(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid]);
    Result.cX := 0;
    Result.cY := 0;
    Windows.GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result);
    Changed;
  end;
end;

function TextWidthW(Canvas: TCanvas; const Text: Widestring): Integer; inline;
begin
  Result := TextExtentW(Canvas, Text).cX;
end;

procedure TextRectW(Canvas: TCanvas; Rect: TRect; X, Y: Integer;
  const Text: WideString);
var
  Options: Longint;
begin
  with TCanvasCrack(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and
       (CanvasOrientation = coRightToLeft) then Inc(X, TextWidthW(Canvas, Text) + 1);
    Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
      Length(Text), nil);
    Changed;
  end;
end;

procedure DefaultPropertyListDrawValue(const Value: string; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean); overload;
begin
  Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Value);
end;

procedure DefaultPropertyListDrawValue(const Value: WideString; Canvas: TCanvas;
  const Rect: TRect; Selected: Boolean); overload;
begin
  TextRectW(Canvas, Rect, Rect.Left + 1, Rect.Top + 1, Value);
end;

{ TFontNameProperty }

function TFontNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TFontNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count - 1 do Proc(Screen.Fonts[I]);
end;

{ TFontCharsetProperty }

function TFontCharsetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList];
end;

function TFontCharsetProperty.GetValue: string;
begin
  if not CharsetToIdent(TFontCharset(GetOrdValue), Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TFontCharsetProperty.GetValues(Proc: TGetStrProc);
begin
  GetCharsetValues(Proc);
end;

procedure TFontCharsetProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCharset(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TImeNameProperty }

function TImeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TImeNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Imes.Count - 1 do Proc(Screen.Imes[I]);
end;

{ TFontProperty }

procedure TFontProperty.Edit;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist];
    if FontDialog.Execute then SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

function TFontProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TModalResultProperty }
const
  ModalResults: array[mrNone..mrClose] of string = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll',
    'mrClose');

function TModalResultProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TModalResultProperty.GetValue: string;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do Proc(ModalResults[I]);
end;

procedure TModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
  begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(ModalResults) to High(ModalResults) do
    if CompareText(ModalResults[I], Value) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

{ TShortCutProperty }
const
  ShortCuts: array[0..108] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);

function TShortCutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TShortCutProperty.GetValue: string;
var
  CurValue: TShortCut;
begin
  CurValue := GetOrdValue;
  if CurValue = scNone then
    Result := srNone else
    Result := ShortCutToText(CurValue);
end;

procedure TShortCutProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(srNone);
  for I := 1 to High(ShortCuts) do Proc(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutProperty.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, srNone) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(NewValue);
end;

{ TBooleanProperty }

function TBooleanProperty.CBRect(const ItemRect: TRect): TRect;
begin
  Result := Rect(ItemRect.Right + 2, ItemRect.Top,
    itemrect.Right + Itemrect.Bottom - ItemRect.Top + 2, ItemRect.Bottom);
end;

procedure TBooleanProperty.HintShow(var HintInfo: THintInfo;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseMove(Shift: TShiftState; X, Y: Integer;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TBooleanProperty.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
  if paReadOnly in GetAttributes then Exit;
  if PtInRect(ItemRect, Point(x,y)) then
  begin
    SetOrdValue(1-GetOrdValue());
    Handled := True;
  end;
end;

procedure TBooleanProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
end;


function TBooleanProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result  :=  ARect;
end;

procedure DrawCheckbox(ACanvas: TCanvas; ARect : TRect;
  ASelected, AEnabled, AAllEqual, AValue: Boolean);
const
  ThemeStyles : array[TCheckBoxState] of array[Boolean] of TThemedButton =(
    (tbCheckBoxUncheckedDisabled, tbCheckBoxUnCheckedNormal),
    (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedNormal),
    (tbCheckBoxMixedDisabled, tbCheckBoxMixedNormal)
  );

  UnThemedStyles : array[TCheckBoxState] of array[Boolean] of Cardinal =(
    (DFCS_BUTTONCHECK or DFCS_INACTIVE, DFCS_BUTTONCHECK),
    (DFCS_CHECKED or DFCS_INACTIVE, DFCS_CHECKED),
    (DFCS_BUTTON3STATE or DFCS_INACTIVE,  DFCS_BUTTON3STATE)
  );

var State : TCheckBoxState;
begin
  if AAllEqual = false then
    State := cbGrayed
  else if AValue then
    State := cbChecked
  else
    State := cbUnchecked;

  if ThemeServices.ThemesEnabled then
    ThemeServices.DrawElement(ACanvas.Handle,
      ThemeServices.GetElementDetails(ThemeStyles[State][AEnabled]), ARect)
  else
    DrawFrameControl(ACanvas.Handle, ARect,
      DFC_BUTTON, UnThemedStyles[State][AEnabled]);
end;

procedure TBooleanProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
var
  stateR,
  valueR: TRect;
begin
  stateR.Left :=  ARect.Left;
  stater.Top  :=  ARect.Top;
  stater.Right  :=  ARect.Left  + ARect.Bottom - ARect.Top;
  stater.Bottom :=  ARect.Bottom;
  DrawCheckbox(ACanvas, stateR , ASelected, not (paReadOnly in GetAttributes), AllEqual, Boolean(GetOrdValue()));
//  valueR        :=  ARect;
//  valueR.Left   :=  stater.Right;
//  DefaultDrawPropValue(ACanvas, valueR, Value);
end;

function TBooleanProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

{}

{ TColorProperty }

procedure TColorProperty.Edit;
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Application);
  try
    ColorDialog.Color       := GetOrdValue;
    ColorDialog.HelpContext := hcDColorEditor;
    ColorDialog.Options     := [cdShowHelp, cdFullOpen, cdAnyColor];
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
  finally
    FreeAndNil(ColorDialog);
  end;
end;

function TColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TColorProperty.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TColorProperty.GetValues(Proc: TGetStrProc);
begin
  GetColorValues(Proc);
end;

procedure TColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  ValueRect: TRect;
begin
  ValueRect := PaintColorBox(Value, ACanvas, ARect, ASelected);
  DefaultPropertyListDrawValue(Value, ACanvas, ValueRect, ASelected);
end;

procedure TColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin

end;

procedure TColorProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas;
  var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

function TColorProperty.PaintColorBox(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean): TRect;

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up and do the work
    Brush.Color := StringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    Result := Rect(Right, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;

procedure TColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
//  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TColorProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result  :=  ARect;
end;

procedure TColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
var
  ColorR,
  valueR: TRect;
begin
  ColorR.Left :=  ARect.Left;
  ColorR.Top  :=  ARect.Top;
  ColorR.Right  :=  ARect.Left  + ARect.Bottom - ARect.Top;
  ColorR.Bottom :=  ARect.Bottom;
  if GetVisualValue <> '' then
    PaintColorBox(GetVisualValue, ACanvas, ColorR, ASelected);

//  valueR        :=  ARect;
//  valueR.Left   :=  ColorR.Right;
//  DefaultDrawPropValue(ACanvas, valueR, Value);

//    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
//  else
//    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

function TColorProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

procedure TColorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

{ TRDSetProperty }

function TRDSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
end;

procedure TRDSetProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  E: IProperty;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType^)^ do
    for I := MinValue to MaxValue do
    begin
      E := TRDSetElementProperty.Create(Self, I);
      Proc(E);
      E := nil;
    end;
end;

{ TRDSetElementProperty }

function TRDSetElementProperty.CBRect(const ItemRect: TRect): TRect;
begin
  Result := Rect(ItemRect.Right + 2, ItemRect.Top,
    itemrect.Right + Itemrect.Bottom - ItemRect.Top + 2, ItemRect.Bottom);
end;

constructor TRDSetElementProperty.Create(Parent: TPropertyEditor;
  AElement: Integer);
var
  MinValue: integer;
begin
  inherited;
  MinValue := GetTypeData(GetTypeData(GetPropType).CompType^).MinValue;
  FBit := AElement - MinValue;
end;

procedure TRDSetElementProperty.HintShow(var HintInfo: THintInfo;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TRDSetElementProperty.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TRDSetElementProperty.MouseMove(Shift: TShiftState; X, Y: Integer;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TRDSetElementProperty.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
var
  S: TIntegerSet;
begin
  Handled := False;
  if paReadOnly in GetAttributes then Exit;
  if PtInRect(ItemRect, Point(x,y)) then
  begin
    Integer(S) := GetOrdValue;
    if FBit in S then
      Exclude(S, FBit)
    else
      Include(S, FBit);
    SetOrdValue(Integer(S));
    Handled := True;
  end;
end;

procedure TRDSetElementProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin

end;

function TRDSetElementProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result  :=  ARect;
end;

procedure TRDSetElementProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  stateR,
  valueR: TRect;
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  stateR.Left :=  ARect.Left;
  stater.Top  :=  ARect.Top;
  stater.Right  :=  ARect.Left  + ARect.Bottom - ARect.Top;
  stater.Bottom :=  ARect.Bottom;
  DrawCheckbox(ACanvas, stateR , ASelected, not (paReadOnly in GetAttributes), AllEqual, FBit in S);
//  valueR        :=  ARect;
//  valueR.Left   :=  stater.Right;
//  DefaultDrawPropValue(ACanvas, valueR, Value);
end;

function TRDSetElementProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

{ TStringsProperty }

procedure TStringsProperty.Edit;
var
  Data: PStringsData;
begin
  new(Data);
  Data^.Caption :=  Designer.GetObjectName(GetComponent(0)) + '.' + GetName;
  Data^.Lines   :=  TStrings(GetOrdValue).Text;
  if RDStringsEdt.ShowStringsEditor(Data) then
    TStrings(GetOrdValue).Text  :=  Data^.Lines;
  Dispose(Data);
end;

function TStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paReadOnly, paDialog];
end;

{ TWideStringsProperty }

procedure TWideStringsProperty.Edit;
var
  Data: PStringsData;
begin
  new(Data);
  Data^.Caption :=  Designer.GetObjectName(GetComponent(0)) + '.' + GetName;
  Data^.Lines   :=  TWideStrings(GetOrdValue).Text;
  if RDStringsEdt.ShowStringsEditor(Data) then
    TWideStrings(GetOrdValue).Text  :=  Data^.Lines;
  Dispose(Data);
end;

function TWideStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paReadOnly, paDialog];
end;

{ TCursorProperty }

function TCursorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TCursorProperty.GetValue: string;
begin
  Result := CursorToString(TCursor(GetOrdValue));
end;

procedure TCursorProperty.GetValues(Proc: TGetStrProc);
begin
  GetCursorValues(Proc);
end;

procedure TCursorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  CursorIndex: Integer;
  CursorHandle: THandle;
begin
  Right := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;
  with ACanvas do
  begin
    if not IdentToCursor(Value, CursorIndex) then
      CursorIndex := StrToInt(Value);
    ACanvas.FillRect(ARect);
    CursorHandle := Screen.Cursors[CursorIndex];
    if CursorHandle <> 0 then
      DrawIconEx(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, CursorHandle,
        0, 0, 0, 0, DI_NORMAL or DI_DEFAULTSIZE);
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TCursorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight('Wg'), GetSystemMetrics(SM_CYCURSOR) + 4);
end;

procedure TCursorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

procedure TCursorProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCursor(Value, NewValue) then
    SetOrdValue(NewValue)
  else inherited SetValue(Value);
end;

{ TBrushStyleProperty }

procedure TBrushStyleProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldBrushStyle := Brush.Style;

    // frame things
    Pen.Color := Brush.Color;
    Brush.Color := clWindow;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up
    Pen.Color := clWindowText;
    Brush.Style := TBrushStyle(GetEnumValue(GetPropInfo^.PropType^, Value));

    // bsClear hack
    if Brush.Style = bsClear then
    begin
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := clWindowText;

    // ok on with the show
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TBrushStyleProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin

end;

procedure TBrushStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('A') {* 2};
end;

procedure TBrushStyleProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin

end;

procedure TBrushStyleProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin

end;

{ TPenStyleProperty }

procedure TPenStyleProperty.GetValues(Proc: TGetStrProc);
var
  LStyle: TPenStyle;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  for LStyle := Low(TPenStyle) to High(TPenStyle) do
    if (LStyle <> psUserStyle) and (LStyle <> psAlternate) then
      Proc(GetEnumName(EnumType, Integer(LStyle)));
end;

procedure TPenStyleProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Right, Top: Integer;
  OldPenColor, OldBrushColor: TColor;
  OldPenStyle: TPenStyle;
begin
  Right := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
  Top := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldPenStyle := Pen.Style;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // white out the background
    Pen.Color := clWindowText;
    Brush.Color := clWindow;
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // set thing up and do work
    Pen.Color := clWindowText;
    Pen.Style := TPenStyle(GetEnumValue(GetPropInfo^.PropType^, Value));
    MoveTo(ARect.Left + 1, Top);
    LineTo(Right - 1, Top);
    MoveTo(ARect.Left + 1, Top + 1);
    LineTo(Right - 1, Top + 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Style := OldPenStyle;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top,
      ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TPenStyleProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin

end;

procedure TPenStyleProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('X') * 2;
end;

procedure TPenStyleProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin

end;

procedure TPenStyleProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin

end;

{ TCaptionProperty }

function TCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paAutoUpdate, paRevertable];
end;

initialization
//  DesignEditors.RegisterDefaultPropertyEditor(tkSet, TRDSetProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringsProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TWideStrings), nil, '', TWideStringsProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TCursor), nil, '', TCursorProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TPenStyle), nil, '', TPenStyleProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TBrushStyle), nil, '', TBrushStyleProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TColor), nil, '', TColorProperty);
  DesignIntf.RegisterPropertyEditor(TypeInfo(TCaption), nil, '', TCaptionProperty);

end.
