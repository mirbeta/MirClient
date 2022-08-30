{***********************************************************************}
{ TAdvSmoothCalculatorDropDown component                                }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2012                                           }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvSmoothCalculatorDropDown;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AdvSmoothEditButton, AdvSmoothEdit, AdvSmoothCalculator, AdvStyleIF;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // Version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Improved : Event OnExtraButtonClick exposed
  // v1.0.0.2 : Fixed : Issue with DropDown Parent
  //          : Improved : Changing text in edit reflected in calculator
  // v1.0.0.3 : Improved : Support for money edit type

type
  TAdvSmoothCalculatorDropDown = class;

  TAdvSmoothCalculatorDD = class(TPersistent)
  private
    FOwner: TAdvsmoothCalculatorDropDown;
    FWidth: integer;
    FHeight: integer;
    FCalculatorLook: TCalculatorLook;
    FExtraButtons: TExtraButtons;
    FPrecision: SmallInt;
    FAutoPrecision: Boolean;
    procedure ExtraButtonsChanged(Sender: TObject);
    procedure CalculatorLookChanged(Sender: TObject);
    procedure SetPrecision(const Value: SmallInt);
    procedure SetAutoPrecision(const Value: Boolean);
  protected
    procedure Update;
  public
    constructor Create(AOwner: TAdvSmoothCalculatorDropDown);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Precision: SmallInt read FPrecision write SetPrecision;
    property CalculatorLook: TCalculatorLook read FCalculatorLook write FCalculatorLook;
    property ExtraButtons: TExtraButtons read FExtraButtons write FExtraButtons;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property AutoPrecision: Boolean read FAutoPrecision write SetAutoPrecision;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothCalculatorDropDown = class(TAdvSmoothEditBtn, ITMSStyle, ITMSTones)
  private
    FTimer: TTimer;
    FDesignTime: Boolean;
    FCalc: TAdvSmoothCalculator;
    FDeactivating: boolean;
    FIgnoreSelect: boolean;
    CalcParent : TForm;
    CancelThisBtnClick : Boolean;
    FHideCalculatorAfterSelection: boolean;
    FCalculator: TAdvSmoothCalculatorDD;
    fOnExtraButtonClick: TExtraButtonClickEvent;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure HideParent;
    procedure InitEvents;
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    procedure SetCalculator(const Value: TAdvSmoothCalculatorDD);
    { Private declarations }
  protected
    procedure ValueChanged(Sender: TObject);
    function IsDropDownVisible: Boolean; override;
    function GetVersionNr: Integer; override;
    { Protected declarations }
    procedure BtnClick(Sender: TObject); override;
    procedure CalcParentDeactivate(Sender: TObject);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure TimerEvent(Sender: TObject);
    property Calc: TAdvSmoothCalculator read FCalc write FCalc;
    procedure CalcValueChanged(Sender: TObject; Value: Double);
    procedure ExtraButtonClick(Sender:TObject; idx:integer; var value:extended);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure CancelBtnClick;
    destructor Destroy; override;
    procedure DropDown; virtual;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    { Published declarations }
    property TabOrder;
    property TabStop default true;
    property HideCalculatorAfterSelection : boolean read FHideCalculatorAfterSelection
      write FHideCalculatorAfterSelection;
    property Calculator: TAdvSmoothCalculatorDD read FCalculator write SetCalculator;
    property OnExtraButtonClick: TExtraButtonClickEvent read fOnExtraButtonClick write fOnExtraButtonClick;
  end;

implementation

{$I DELPHIXE.INC}

{ TAdvSmoothCalculatorDropDown }

procedure TAdvSmoothCalculatorDropDown.DropDown;
var
  CalPos: TPoint;
  r: TRect;

  function Min(a,b: Integer): Integer;
  begin
    if (a > b) then
      Result := b
    else
      Result := a;
  end;

 function GetParentWnd: HWnd;
  var
    Last, P: HWnd;
  begin
    Result := 0;
    if Owner <> nil then
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
  end;

begin
  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      CalcParent.FormStyle := fsStayOnTop;
  end
  else
    CalcParent.FormStyle := fsStayOnTop;

  CalPos.x := -2;
  CalPos.y := Height - 3;
  CalPos := ClientToScreen(CalPos);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...

  Calc.CalculatorLook.Assign(Calculator.CalculatorLook);
  Calc.ExtraButtons.Assign(Calculator.ExtraButtons);
  Calc.Width := Calculator.Width;
  Calc.Height := Calculator.Height;
  Calc.Precision := Calculator.Precision;
  Calc.AutoPrecision := Calculator.AutoPrecision;
  if (CalPos.y + FCalc.Height > r.Bottom) then
    CalPos.Y := CalPos.Y - FCalc.Height - Height + 3;

  if (CalPos.x + FCalc.Width > r.right) then
    CalPos.x := CalPos.x - (FCalc.Width - Width);

  CalcParent.Width := 0;
  CalcParent.Height := 0;

  CalcParent.Show;

  CalcParent.Left := CalPos.x;
  CalcParent.Top := CalPos.y;
  CalcParent.Width := FCalc.Width;
  CalcParent.Height := FCalc.Height;

  FCalc.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
end;

procedure TAdvSmoothCalculatorDropDown.ExtraButtonClick(Sender: TObject;
  idx: integer; var value: extended);
begin
  if Assigned(OnExtraButtonClick) then
    OnExtraButtonClick(Sender, idx, value);
end;

procedure TAdvSmoothCalculatorDropDown.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothCalculatorDropDown.SetCalculator(
  const Value: TAdvSmoothCalculatorDD);
begin
  if FCalculator <> value then
  begin
    FCalculator := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalculatorDropDown.SetColorTones(ATones: TColorTones);
begin
  Calc.SetColorTones(ATones);
  Calculator.CalculatorLook.Assign(Calc.CalculatorLook);
  Calculator.ExtraButtons.Assign(Calc.ExtraButtons);
  Calculator.Width := Calc.Width;
  Calculator.Height := Calc.Height;
end;

procedure TAdvSmoothCalculatorDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  case AStyle of
    tsOffice2003Blue: Calc.SetComponentStyle(tsOffice2003Blue);
    tsOffice2003Olive: Calc.SetComponentStyle(tsOffice2003Olive);
    tsOffice2003Silver: Calc.SetComponentStyle(tsOffice2003Silver);
    tsOffice2003Classic: Calc.SetComponentStyle(tsOffice2003Classic);
    tsOffice2007Luna: Calc.SetComponentStyle(tsOffice2007Luna);
    tsOffice2007Obsidian: Calc.SetComponentStyle(tsOffice2007Obsidian);
    tsOffice2007Silver: Calc.SetComponentStyle(tsOffice2007Silver);
    tsWindowsXP: Calc.SetComponentStyle(tsWindowsXP);
    tsWhidbey: Calc.SetComponentStyle(tsWhidbey);
    tsWindowsVista: Calc.SetComponentStyle(tsWindowsVista);
    tsWindows7: Calc.SetComponentStyle(tsWindows7);
    tsTerminal: Calc.SetComponentStyle(tsTerminal);
    tsOffice2010Blue: Calc.SetComponentStyle(tsOffice2010Blue);
    tsOffice2010Silver: Calc.SetComponentStyle(tsOffice2010Silver);
    tsOffice2010Black: Calc.SetComponentStyle(tsOffice2010Black);
  end;

  Calculator.CalculatorLook.Assign(Calc.CalculatorLook);
  Calculator.ExtraButtons.Assign(Calc.ExtraButtons);
  Calculator.Width := Calc.Width;
  Calculator.Height := Calc.Height;
end;

procedure TAdvSmoothCalculatorDropDown.BtnClick(Sender: TObject);
begin
  CancelThisBtnClick := False;

  inherited;

  if CancelThisBtnClick then
    Exit;

  if FDeactivating then
  begin
    FDeactivating := false;
    Exit;
  end;

  if Assigned(CalcParent) then
  begin
    if CalcParent.Visible then
    begin
      FDeactivating := true;
      CalcParent.Hide;
      Exit;
    end
    else
      DropDown;
  end
  else
    DropDown;
end;

procedure TAdvSmoothCalculatorDropDown.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TAdvSmoothCalculatorDropDown.Create(AOwner: TComponent);
begin
  inherited;
  OnChange := ValueChanged;
  Text := '';
  CalcParent := TForm.Create(Self);
  CalcParent.BorderStyle := bsNone;

  CalcParent.Width := 0;
  CalcParent.Height := 0;

  FCalc := TAdvSmoothCalculator.Create(Self);
  FCalc.Parent := CalcParent;
  FCalc.Name := self.Name +'calc_';
  FCalc.TabStop := true;
  FCalc.OnExtraButtonClick := ExtraButtonClick;
  FIgnoreSelect := false;

  FCalculator := TAdvSmoothCalculatorDD.Create(Self);
  CalcParent.OnDeactivate := CalcParentDeactivate;
  Width := 108;
  FHideCalculatorAfterSelection := True;
  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := 100;
end;

destructor TAdvSmoothCalculatorDropDown.Destroy;
begin
  FTimer.Free;
  FCalculator.Free;
  FCalc.Free;
  CalcParent.Free;
  inherited;
end;

procedure TAdvSmoothCalculatorDropDown.TimerEvent(Sender: TObject);
begin
  FDeactivating := false;
  FTimer.Enabled :=false;
end;

procedure TAdvSmoothCalculatorDropDown.ValueChanged(Sender: TObject);
var
  d: Double;
begin
  if Text <> '' then
  begin
    try
      if TryStrToFloat(Text, d) then
        Calc.FloatValue := D
      else if (Pos(DecimalSeparator, Text) > 0) or (Pos(ThousandSeparator, Text) > 0) then
        Calc.FloatValue := FloatValue
      else
        Calc.FloatValue := 0;

    except
      Calc.FloatValue := 0;
    end;
  end;
end;

procedure TAdvSmoothCalculatorDropDown.HideParent;
begin
  if (csDesigning in ComponentState) then
    Exit;

  FDeactivating := false;
  if CalcParent.HandleAllocated then
  begin
    CalcParent.Hide;
    try
      SetFocus;
    except
    end;
  end;
end;

procedure TAdvSmoothCalculatorDropDown.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (key = VK_DOWN) and (ssAlt in Shift) then
  begin
    if CalcParent.Visible then
      HideParent
    else
      BtnClick(Self);
    Exit;
  end;

  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
  begin
    if CalcParent.Visible then
      HideParent
    else
      BtnClick(Self);
  end;
end;

procedure TAdvSmoothCalculatorDropDown.InitEvents;
begin
  FCalc.OnValueChanged := CalcValueChanged;
end;

function TAdvSmoothCalculatorDropDown.IsDropDownVisible: Boolean;
begin
  Result := false;
  if Assigned(CalcParent) then
    Result := CalcParent.Visible;
end;

procedure TAdvSmoothCalculatorDropDown.Loaded;
begin
  inherited;
  InitEvents;
  Button.Enabled := not ReadOnly;
end;

procedure TAdvSmoothCalculatorDropDown.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothCalculatorDropDown.CalcParentDeactivate(Sender: TObject);
begin
  FDeactivating := true;
  (Sender as TForm).Hide;
  FTimer.Enabled := true;
end;

procedure TAdvSmoothCalculatorDropDown.CalcValueChanged(Sender: TObject;
  Value: Double);
begin
  if (Calculator.Precision >= 0) then
    Text := Format('%.' + inttostr(Calc.Precision) + 'n', [value])
  else
    Text := Format('%g', [Value]);
end;

procedure TAdvSmoothCalculatorDropDown.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvSmoothCalculatorDropDown.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
    Button.SetFocus;
end;

procedure TAdvSmoothCalculatorDropDown.CreateWnd;
begin
  inherited;
  InitEvents;
  FloatValue := 0;
end;

function TAdvSmoothCalculatorDropDown.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

function TAdvSmoothCalculatorDropDown.GetThemeID: String;
begin
  Result := ClassName;
end;

procedure TAdvSmoothCalculatorDropDown.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TAdvSmoothCalculatorDropDown.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TAdvSmoothCalculatorDD }

procedure TAdvSmoothCalculatorDD.Assign(Source: TPersistent);
begin
  CalculatorLook.Assign((Source as TAdvSmoothCalculatorDD).CalculatorLook);
  ExtraButtons.Assign((Source as TAdvSmoothCalculatorDD).ExtraButtons);
  Width := (Source as TAdvSmoothCalculatorDD).Width;
  Height := (Source as TAdvSmoothCalculatorDD).Height;
  Precision := (Source as TAdvSmoothCalculatorDD).Precision;
  AutoPrecision := (Source as TAdvSmoothCalculatorDD).AutoPrecision;
end;

procedure TAdvSmoothCalculatorDD.CalculatorLookChanged(Sender: TObject);
begin
  FOwner.FCalc.CalculatorLook.Assign(CalculatorLook);
end;

constructor TAdvSmoothCalculatorDD.Create(AOwner: TAdvSmoothCalculatorDropDown);
begin
  FOwner := AOwner;
  FCalculatorLook := TCalculatorLook.Create;
  FCalculatorLook.OnChange := CalculatorLookChanged;
  FExtraButtons := TExtraButtons.Create(AOwner.FCalc);
  FExtraButtons.OnChange := ExtraButtonsChanged;
  FWidth := FOwner.FCalc.Width;
  FHeight := FOwner.FCalc.Height;
  FPrecision := FOwner.FCalc.Precision;
  FAutoPrecision := FOwner.FCalc.AutoPrecision;
end;

destructor TAdvSmoothCalculatorDD.Destroy;
begin
  FCalculatorLook.Free;
  FExtraButtons.Free;
  inherited;
end;

procedure TAdvSmoothCalculatorDD.ExtraButtonsChanged(Sender: TObject);
begin
  FOwner.FCalc.ExtraButtons.Assign(ExtraButtons);
end;

procedure TAdvSmoothCalculatorDD.SetAutoPrecision(const Value: Boolean);
begin
  if FAutoPrecision <> Value then
    FAutoPrecision := Value;
end;

procedure TAdvSmoothCalculatorDD.SetPrecision(const Value: SmallInt);
begin
  FPrecision := Value;
  if Assigned(Fowner) then
  begin
    if (FPrecision >= 0) then
      FOwner.Text := Format('%.' + inttostr(FPrecision) +'n', [Fowner.Calc.FloatValue])
    else
      FOwner.Text := Format('%g', [Fowner.Calc.FloatValue]);
  end;
end;

procedure TAdvSmoothCalculatorDD.Update;
begin
  FOwner.FCalc.Width := FOwner.Calculator.Width;
  FOwner.FCalc.Height := FOwner.Calculator.Height;
  FOwner.FCalc.CalculatorLook.Assign(FOwner.Calculator.CalculatorLook);
  FOwner.FCalc.ExtraButtons.Assign(FOwner.Calculator.ExtraButtons);
  FOwner.FCalc.Precision := FOwner.Calculator.Precision;
  FOwner.FCalc.AutoPrecision := FOwner.Calculator.AutoPrecision;
end;

end.
