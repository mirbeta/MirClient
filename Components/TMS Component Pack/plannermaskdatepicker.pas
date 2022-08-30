{***********************************************************************}
{ TPlannerDatePicker component                                          }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2014                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

{$I TMSDEFS.INC}

unit PlannerMaskDatePicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  AdvMEdBtn, PlannerCal, MaskUtils;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // Version history
  // 1.4.0.0 : Property Calendar.InActiveDays added
  // 1.5.0.0 : AutoThemeAdapt in Calendar added
  //         : XP style dropdown button added
  //         : Hover date color in Calendar added
  // 1.5.0.1 : Improved TPlannerDBMaskDatePicker mask handling
  // 1.5.0.2 : Fixed : issue with use on forms with fsStayOnTop formstyle set
  // 1.5.0.3 : Fixed : issue with OnInvalidDate event
  // 1.5.0.4 : Improved : check for empty / incomplete entered dates
  // 1.5.0.5 : Fixed : issue with persisting calendar settings
  // 1.5.0.6 : Fixed : issue with position of calendar for runtime created control
  // 1.5.0.7 : Fixed : issue with updating PlannerDBMaskDatePicker
  // 1.5.1.0 : New : DroppedDown public property added

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerMaskDatePicker = class(TAdvMaskEditBtn)
  private
    FPlannerCalendar: TPlannerCalendar;
    FPlannerParent : TForm;
    CancelThisBtnClick : Boolean;
    FHideCalendarAfterSelection: boolean;
    FOnDaySelect: TDaySelectEvent;
    FOnInvalidDate: TNotifyEvent;
    FDroppedDown: boolean;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    function GetOnGetDateHint: TGetDateEvent;
    function GetOnGetDateHintString: TGetDateEventHint;
    procedure SetOnGetDateHint(const Value: TGetDateEvent);
    procedure SetOnGetDateHintString(const Value: TGetDateEventHint);
    procedure HideParent;
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    function GetOnGetEventProp: TEventPropEvent;
    procedure SetOnGetEventProp(const Value: TEventPropEvent);
    function GetOnWeekSelect: TNotifyEvent;
    procedure SetOnWeekSelect(const Value: TNotifyEvent);
    function GetOnAllDaySelect: TNotifyEvent;
    procedure SetOnAllDaySelect(const Value: TNotifyEvent);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    procedure SetDroppedDown(const Value: boolean);
    { Private declarations }
  protected
    function GetVersionNr: Integer; override;
    { Protected declarations }
    procedure InitEvents; virtual;
    procedure BtnClick(Sender: TObject); override;
    procedure PlannerParentDeactivate(Sender: TObject);
    procedure PlannerCalendarDaySelect(Sender: TObject; SelDate: TDateTime);
    procedure PlannerCalendarKeyPress(Sender: TObject; var Key: Char);
    procedure PlannerCalendarKeyDown(Sender: TObject; var Key: Integer;
      Shift: TShiftState);
    // methods to do correct streaming, because the planner calendar is
    // stored on a hidden form
    function GetChildParent : TComponent; override;
    function GetChildOwner : TComponent; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DaySelect; virtual;
    procedure ValidateError; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoExit; override;

    procedure DropDown; virtual;
    procedure CreateWnd; override;
    procedure CancelBtnClick;
    property Date: TDateTime read GetDate write SetDate;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property DroppedDown: boolean read FDroppedDown write SetDroppedDown;
  published
    { Published declarations }
    property Calendar : TPlannerCalendar read FPlannerCalendar
      write FPlannerCalendar;
    property HideCalendarAfterSelection : boolean read FHideCalendarAfterSelection
      write FHideCalendarAfterSelection;
    property OnGetDateHint: TGetDateEvent read GetOnGetDateHint
      write SetOnGetDateHint;
    property OnGetDateHintString: TGetDateEventHint read GetOnGetDateHintString
      write SetOnGetDateHintString;
    property OnGetEventProp: TEventPropEvent read GetOnGetEventProp
      write SetOnGetEventProp;
    property OnWeekSelect: TNotifyEvent read GetOnWeekSelect write SetOnWeekSelect;
    property OnAllDaySelect: TNotifyEvent read GetOnAllDaySelect write SetOnAllDaySelect;
    property OnDaySelect: TDaySelectEvent read FOnDaySelect write FOnDaySelect;
    property OnInvalidDate: TNotifyEvent read FOnInvalidDate write FOnInvalidDate;
  end;

implementation

uses
  StdCtrls, Graphics;

{$I DELPHIXE.INC}

{ TPlannerDatePicker }

procedure TPlannerMaskDatePicker.DropDown;
var
  PlannerPosition : TPoint;
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
    P := GetParent((Owner as TWinControl).Handle);
    Last := P;
    while P <> 0 do
    begin
      Last := P;
      P := GetParent(P);
    end;
    Result := Last;
  end;


begin
  FPlannerParent.Visible := false;

  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      FPlannerParent.FormStyle := fsStayOnTop;
  end
  else
    FPlannerParent.FormStyle := fsStayOnTop;

  // Set planner position
  PlannerPosition.x := -2;
  PlannerPosition.y := Height - 3;
  PlannerPosition := ClientToScreen(PlannerPosition);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...

  if (plannerposition.y + FPlannerCalendar.Height > r.Bottom) then
    plannerposition.Y := plannerposition.Y - FPlannerCalendar.Height - Height + 3;

  if (plannerposition.x + FPlannerCalendar.Width > r.right) then
    plannerposition.x := plannerposition.x - (FPlannerCalendar.Width - Width);

  FPlannerParent.Width := 0;
  FPlannerParent.Height := 0;
  FPlannerParent.Left := - 300;
  FPlannerParent.Top := - 300;
  FPlannerParent.Visible := true;

  // Set planner date

  if FPlannerCalendar.MultiSelect then
    Text := FPlannerCalendar.DatesAsText
  else
  begin
    try
      if (Text = '') or (Pos('  ',Text) in [1..5]) then
        FPlannerCalendar.Date := Now
      else
        try
          FPlannerCalendar.Date := StrToDate(Text);
        except
          if (FPlannerCalendar.Date = 0) then
            FPlannerCalendar.Date := Now;  //; keep old date set active
        end;
    except
      on Exception do
         Text := FPlannerCalendar.DatesAsText;
    end;
  end;

  MoveWindow(FPlannerParent.Handle, plannerposition.X, plannerposition.Y, 0, 0, true);

  FPlannerCalendar.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
  FDroppedDown := true;
end;


procedure TPlannerMaskDatePicker.BtnClick(Sender: TObject);
begin
  SetFocus;
  CancelThisBtnClick := False;
  inherited;
  // call event OnClick - the user can cancel calendar appearance of calendar by calling .CancelBtnClick
  if CancelThisBtnClick then
    Exit;
  DropDown;
end;

procedure TPlannerMaskDatePicker.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TPlannerMaskDatePicker.Create(AOwner: TComponent);
begin
  inherited;
  // Make planner parent form and a planner, put planner on parent form
  Text := '';
  FPlannerParent := TForm.Create(Self);
  FPlannerParent.BorderStyle := bsNone;

  FPlannerCalendar := TPlannerCalendar.Create(Self);
  FPlannerCalendar.Parent := FPlannerParent;
  FPlannerCalendar.Name := self.Name +'mcal'+inttostr(AOwner.ComponentCount)+'_';

  FPlannerParent.Autosize := True;

  FPlannerParent.OnDeactivate := PlannerParentDeactivate;
  FPlannerCalendar.OnDaySelect := PlannerCalendarDaySelect;

  Width := FPlannerCalendar.Width;
  FHideCalendarAfterSelection := True;

  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));

  // Make the button NOT change the focus
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;
end;


destructor TPlannerMaskDatePicker.Destroy;
begin
  FPlannerCalendar.Free;
  FPlannerParent.Free;
  inherited;
end;

function TPlannerMaskDatePicker.GetChildOwner: TComponent;
begin
  Result := FPlannerParent;
end;

function TPlannerMaskDatePicker.GetChildParent: TComponent;
begin
  Result := FPlannerParent;
end;

procedure TPlannerMaskDatePicker.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  Proc(FPlannerCalendar);
  FPlannerCalendar.Parent := FPlannerParent;
end;

function TPlannerMaskDatePicker.GetOnGetDateHint: TGetDateEvent;
begin
  Result := FPlannerCalendar.OnGetDateHint;
end;

function TPlannerMaskDatePicker.GetOnGetDateHintString: TGetDateEventHint;
begin
  Result := FPlannerCalendar.OnGetDateHintString;
end;

procedure TPlannerMaskDatePicker.HideParent;
begin
  FPlannerParent.Hide;
  try
    SetFocus;
  except
  end;
end;

procedure TPlannerMaskDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
    if FPlannerParent.Visible then
      HideParent
    else
      BtnClick(Self);
end;

procedure TPlannerMaskDatePicker.InitEvents;
begin
  FPlannerCalendar.OnDaySelect := PlannerCalendarDaySelect;
  FPlannerCalendar.OnKeyPress := PlannerCalendarKeypress;
end;

procedure TPlannerMaskDatePicker.Loaded;
var
  APlannerCalendar: TPlannerCalendar;
begin
  inherited;

  //if (not (csDesigning in ComponentState)) then
    if FPlannerParent.ComponentCount > 0 then
    begin
      APlannerCalendar := (FPlannerParent.Components[0] as TPlannerCalendar);
      APlannerCalendar.OnGetDateHint := FPlannerCalendar.OnGetDateHint;
      APlannerCalendar.OnGetDateHintString := FPlannerCalendar.OnGetDateHintString;
      FPlannerCalendar.Free;
      FPlannerCalendar := APlannerCalendar;
      InitEvents;
    end;
end;

procedure TPlannerMaskDatePicker.PlannerCalendarDaySelect(Sender: TObject; SelDate: TDateTime);
begin
  if not ReadOnly then
  begin
    Text := FPlannerCalendar.DatesAsText;
  end;

  if FHideCalendarAfterSelection then
    HideParent;

  if Assigned(FOnDaySelect) then
    FOnDaySelect(Self,SelDate);
end;

procedure TPlannerMaskDatePicker.DaySelect;
begin
end;

procedure TPlannerMaskDatePicker.PlannerCalendarKeyDown(Sender: TObject;
  var Key: Integer; Shift: TShiftState);
begin
  if Key = VK_F4 then
    HideParent;
end;

procedure TPlannerMaskDatePicker.PlannerCalendarKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) then
  begin
    PlannerCalendarDaySelect(Sender, FPlannerCalendar.Date);
  end;
  if Key = #27 then
  begin
    HideParent;
  end;
end;

procedure TPlannerMaskDatePicker.PlannerParentDeactivate(Sender: TObject);
begin
  (Sender as TForm).Hide;
  FDroppedDown := false;
end;

procedure TPlannerMaskDatePicker.SetOnGetDateHint(const Value: TGetDateEvent);
begin
  FPlannerCalendar.OnGetDateHint := Value;
end;

procedure TPlannerMaskDatePicker.SetOnGetDateHintString(
  const Value: TGetDateEventHint);
begin
  FPlannerCalendar.OnGetDateHintString := Value;
end;

function TPlannerMaskDatePicker.GetOnGetEventProp: TEventPropEvent;
begin
  Result := FPlannerCalendar.OnGetEventProp;
end;

procedure TPlannerMaskDatePicker.SetOnGetEventProp(
  const Value: TEventPropEvent);
begin
  FPlannerCalendar.OnGetEventProp := Value;
end;


procedure TPlannerMaskDatePicker.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
    Button.SetFocus;
end;

procedure TPlannerMaskDatePicker.Change;
var
  dt: TDateTime;
begin
  try
    if IsMasked and (Text = MaskDoFormatText(EditMask,'',MaskGetMaskBlank(EditMask))) then
    begin
      Calendar.Date := 0;
      Exit;
    end;

    if (Text = '') or (Pos(DateSeparator, Text) = 0) then
      Calendar.Date := 0
    else
    begin
      TryStrToDate(Text,dt);
      Calendar.Date := dt;
    end;
  except
  end;
  inherited;
end;

procedure TPlannerMaskDatePicker.CreateWnd;
begin
  inherited;
  InitEvents;
end;

function TPlannerMaskDatePicker.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TPlannerMaskDatePicker.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TPlannerMaskDatePicker.GetOnWeekSelect: TNotifyEvent;
begin
  Result := FPlannerCalendar.OnWeekSelect;
end;

procedure TPlannerMaskDatePicker.SetOnWeekSelect(const Value: TNotifyEvent);
begin
  FPlannerCalendar.OnWeekSelect := Value;
end;

function TPlannerMaskDatePicker.GetOnAllDaySelect: TNotifyEvent;
begin
  Result := FPlannerCalendar.OnAllDaySelect;
end;

procedure TPlannerMaskDatePicker.SetOnAllDaySelect(const Value: TNotifyEvent);
begin
  FPlannerCalendar.OnAllDaySelect := Value;
end;

function TPlannerMaskDatePicker.GetDate: TDateTime;
begin
  Result := FPlannerCalendar.Date;
end;

procedure TPlannerMaskDatePicker.SetDate(const Value: TDateTime);
begin
  FPlannerCalendar.Date := Value;
  Text := DateToStr(Value);
end;

procedure TPlannerMaskDatePicker.SetDroppedDown(const Value: boolean);
begin
  FDroppedDown := Value;

  if DroppedDown then
    DropDown
  else
    SetFocus;
end;

procedure TPlannerMaskDatePicker.DoExit;
var
  dt: TDateTime;
begin
  inherited;
  try
    if (Text <> '') then
    begin
      dt := StrToDate(Text);
      Calendar.Date := dt;
    end;
  except
    if Assigned(FOnInvalidDate) then
      FOnInvalidDate(Self)
  end;
end;

function TPlannerMaskDatePicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPlannerMaskDatePicker.ValidateError;
begin
  if Assigned(FOnInvalidDate) then
    FOnInvalidDate(Self)
  else
    inherited;  
end;

initialization
  RegisterClass(TPlannerMaskDatePicker);

end.
