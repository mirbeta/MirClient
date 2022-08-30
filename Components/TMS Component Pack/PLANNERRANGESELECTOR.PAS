{***********************************************************************}
{ TPlannerDatePicker component                                          }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2014                                           }
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

unit PlannerRangeSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  AdvMEdBtn, PlannerCal, MaskUtils, Mask, StdCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // Version history
  // 1.0.0.0 : First release

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerRangeSelector = class(TAdvMaskEditBtn)
  private
    FPlannerCalendar: TPlannerCalendar;
    FPlannerParent: TForm;
    CancelThisBtnClick: Boolean;
    FHideCalendarAfterSelection: boolean;
    FOnRangeSelect: TRangeSelectEvent;
    FOnInvalidDate: TNotifyEvent;
    FDroppedDown: boolean;
    FEdit: TMaskEdit;
    FSeparator: TStaticText;
    FCalendarMouseDown: boolean;
    FCalendarKeyDown: boolean;
    FDownDate: TDateTime;
    FInternalChange: boolean;
    FOrigStart,FOrigEnd: TDateTime;
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
    procedure SetDroppedDown(const Value: boolean);
    function GetDateEnd: TDateTime;
    function GetDateStart: TDateTime;
    procedure SetDateEnd(const Value: TDateTime);
    procedure SetDateStart(const Value: TDateTime);
    function GetEditMask: TEditMask;
    procedure SetEditMask(const Value: TEditMask);
    { Private declarations }
  protected
    function GetVersionNr: Integer; override;
    { Protected declarations }
    procedure InitEvents; virtual;
    procedure CreateSubControls;
    procedure UpdateEditSize;
    procedure UpdateSeparatorSize;
    function GetEditExtraSpace: integer; override;

    procedure BtnClick(Sender: TObject); override;
    procedure PlannerParentDeactivate(Sender: TObject);
    procedure PlannerCalendarRangeSelect(Sender: TObject; StartDate, EndDate: TDateTime);
    procedure PlannerCalendarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlannerCalendarMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PlannerCalendarMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlannerCalendarKeyPress(Sender: TObject; var Key: Char);
    procedure PlannerCalendarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // methods to do correct streaming, because the planner calendar is
    // stored on a hidden form
    function GetChildParent : TComponent; override;
    function GetChildOwner : TComponent; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ValidateError; override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoExit; override;

    procedure DropDown; virtual;
    procedure CreateWnd; override;
    procedure CancelBtnClick;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property DroppedDown: boolean read FDroppedDown write SetDroppedDown;
  published
    { Published declarations }
    property Calendar : TPlannerCalendar read FPlannerCalendar write FPlannerCalendar;
    property DateStart: TDateTime read GetDateStart write SetDateStart;
    property DateEnd: TDateTime read GetDateEnd write SetDateEnd;
    property EditMask: TEditMask read GetEditMask write SetEditMask;

    property HideCalendarAfterSelection : boolean read FHideCalendarAfterSelection
      write FHideCalendarAfterSelection;
    property OnGetDateHint: TGetDateEvent read GetOnGetDateHint
      write SetOnGetDateHint;
    property OnGetDateHintString: TGetDateEventHint read GetOnGetDateHintString
      write SetOnGetDateHintString;
    property OnGetEventProp: TEventPropEvent read GetOnGetEventProp
      write SetOnGetEventProp;
    property OnWeekSelect: TNotifyEvent read GetOnWeekSelect write SetOnWeekSelect;
    property OnRangeSelect: TRangeSelectEvent read FOnRangeSelect write FOnRangeSelect;
    property OnInvalidDate: TNotifyEvent read FOnInvalidDate write FOnInvalidDate;
  end;

implementation

uses
  Graphics;

const
  SEPARATOR_WIDTH = 5;

{$I DELPHIXE.INC}

{ TPlannerRangeSelector }

procedure TPlannerRangeSelector.DropDown;
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
  FCalendarMouseDown := false;
  FCalendarKeyDown := false;

  FOrigStart := DateStart;
  FOrigEnd := DateEnd;

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

  FPlannerCalendar.MultiSelect := true;
  FPlannerCalendar.RightClickSelect := true;

  FPlannerCalendar.Dates.Clear;
  FPlannerCalendar.Dates.AddRange(DateStart, DateEnd);

  MoveWindow(FPlannerParent.Handle, plannerposition.X, plannerposition.Y, 0, 0, true);

  FPlannerCalendar.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
  FDroppedDown := true;
end;


procedure TPlannerRangeSelector.BtnClick(Sender: TObject);
begin
  SetFocus;
  CancelThisBtnClick := False;
  inherited;
  // call event OnClick - the user can cancel calendar appearance of calendar by calling .CancelBtnClick
  if CancelThisBtnClick then
    Exit;
  DropDown;
end;

procedure TPlannerRangeSelector.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TPlannerRangeSelector.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := nil;
  FInternalChange := false;

  // Make planner parent form and a planner, put planner on parent form
  FPlannerParent := TForm.Create(Self);
  FPlannerParent.BorderStyle := bsNone;

  FPlannerCalendar := TPlannerCalendar.Create(Self);
  FPlannerCalendar.Parent := FPlannerParent;
  FPlannerCalendar.Name := self.Name +'mcal'+inttostr(AOwner.ComponentCount)+'_';
  InitEvents;

  FPlannerParent.Autosize := True;

  Width := FPlannerCalendar.Width;
  FHideCalendarAfterSelection := True;

  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));

  // Make the button NOT change the focus
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;
  Width := 150;
  Text := DateToStr(Now);
end;


destructor TPlannerRangeSelector.Destroy;
begin
  FPlannerCalendar.Free;
  FPlannerParent.Free;
  inherited;
end;

function TPlannerRangeSelector.GetChildOwner: TComponent;
begin
  Result := FPlannerParent;
end;

function TPlannerRangeSelector.GetChildParent: TComponent;
begin
  Result := FPlannerParent;
end;

procedure TPlannerRangeSelector.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  Proc(FPlannerCalendar);
  FPlannerCalendar.Parent := FPlannerParent;
end;

function TPlannerRangeSelector.GetOnGetDateHint: TGetDateEvent;
begin
  Result := FPlannerCalendar.OnGetDateHint;
end;

function TPlannerRangeSelector.GetOnGetDateHintString: TGetDateEventHint;
begin
  Result := FPlannerCalendar.OnGetDateHintString;
end;

procedure TPlannerRangeSelector.HideParent;
begin
  FPlannerParent.Hide;
  try
    SetFocus;
  except
  end;
end;

procedure TPlannerRangeSelector.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
    if FPlannerParent.Visible then
      HideParent
    else
      BtnClick(Self);
end;

procedure TPlannerRangeSelector.InitEvents;
begin
  FPlannerCalendar.OnRangeSelect := PlannerCalendarRangeSelect;
  FPlannerCalendar.OnKeyPress := PlannerCalendarKeypress;
  FPlannerCalendar.OnKeyDown := PlannerCalendarKeyDown;
  FPlannerCalendar.OnMouseMove := PlannerCalendarMouseMove;
  FPlannerCalendar.OnMouseDown := PlannerCalendarMouseDown;
  FPlannerCalendar.OnMouseUp := PlannerCalendarMouseUp;
end;

procedure TPlannerRangeSelector.Loaded;
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

procedure TPlannerRangeSelector.PlannerCalendarKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FCalendarKeyDown := true;

  if (Key = VK_F4) or (Key = VK_RETURN) or (Key = VK_SPACE) then
    HideParent;
end;

procedure TPlannerRangeSelector.PlannerCalendarKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #27 then
  begin
    DateStart := FOrigStart;
    DateEnd := FOrigEnd;
    HideParent;
  end;
end;

procedure TPlannerRangeSelector.PlannerCalendarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dt: TDateTime;
begin
  FCalendarKeyDown := false;
  if FPlannerCalendar.DateAtXY(x,y,dt) then
  begin
    if dt <> 0 then
    begin
      DateStart := dt;
      DateEnd := dt;
      FDownDate := dt;
      FCalendarMouseDown := true;
    end;
  end;
end;

procedure TPlannerRangeSelector.PlannerCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dt: TDateTime;

begin

  if FCalendarMouseDown and FPlannerCalendar.DateAtXY(x,y,dt) and not (ssShift in Shift) then
  begin
    if dt <> 0 then
    begin
      if dt > FDownDate then
      begin
        DateStart := FDownDate;
        DateEnd := dt;
      end
      else
      begin
        DateEnd := FDownDate;
        DateStart := dt;
      end;
    end;
  end;
end;

procedure TPlannerRangeSelector.PlannerCalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCalendarMouseDown := false;
end;

procedure TPlannerRangeSelector.PlannerParentDeactivate(Sender: TObject);
begin
  (Sender as TForm).Hide;
  FDroppedDown := false;
end;

procedure TPlannerRangeSelector.PlannerCalendarRangeSelect(Sender: TObject; StartDate,
  EndDate: TDateTime);
begin
  if not ReadOnly then
  begin
    FInternalChange := true;
    try
      DateStart := StartDate;
      DateEnd := EndDate;
    finally
      FInternalChange := false;
    end;
  end;

  if Assigned(OnRangeSelect) then
    OnRangeSelect(Self, DateStart, DateEnd);

  if FHideCalendarAfterSelection and not FCalendarKeyDown then
    HideParent;
end;

procedure TPlannerRangeSelector.Resize;
begin
  inherited;
  UpdateEditSize;
  UpdateSeparatorSize;
end;

procedure TPlannerRangeSelector.SetOnGetDateHint(const Value: TGetDateEvent);
begin
  FPlannerCalendar.OnGetDateHint := Value;
end;

procedure TPlannerRangeSelector.SetOnGetDateHintString(
  const Value: TGetDateEventHint);
begin
  FPlannerCalendar.OnGetDateHintString := Value;
end;

function TPlannerRangeSelector.GetOnGetEventProp: TEventPropEvent;
begin
  Result := FPlannerCalendar.OnGetEventProp;
end;

procedure TPlannerRangeSelector.SetOnGetEventProp(
  const Value: TEventPropEvent);
begin
  FPlannerCalendar.OnGetEventProp := Value;
end;

procedure TPlannerRangeSelector.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
    Button.SetFocus;
end;

procedure TPlannerRangeSelector.Change;
var
  dt: TDateTime;
begin
  if not FInternalChange then
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
  end;

  inherited;
end;

procedure TPlannerRangeSelector.CreateWnd;
begin
  inherited;
  CreateSubControls;
  InitEvents;
end;

function TPlannerRangeSelector.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TPlannerRangeSelector.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TPlannerRangeSelector.GetOnWeekSelect: TNotifyEvent;
begin
  Result := FPlannerCalendar.OnWeekSelect;
end;

procedure TPlannerRangeSelector.SetOnWeekSelect(const Value: TNotifyEvent);
begin
  FPlannerCalendar.OnWeekSelect := Value;
end;

function TPlannerRangeSelector.GetDateEnd: TDateTime;
begin
  if Assigned(FEdit) then
    TryStrToDate(FEdit.Text, Result);
end;

function TPlannerRangeSelector.GetDateStart: TDateTime;
begin
  TryStrToDate(Text, Result);
end;

function TPlannerRangeSelector.GetEditExtraSpace: integer;
begin
  Result := 6 + SEPARATOR_WIDTH + (Width - ButtonWidth - ButtonWidth - 4 - SEPARATOR_WIDTH) div 2;
end;

function TPlannerRangeSelector.GetEditMask: TEditMask;
begin
  Result := inherited EditMask;
end;

procedure TPlannerRangeSelector.SetDateEnd(const Value: TDateTime);
var
  dt: TDateTime;
begin
  if Assigned(FEdit) then
  begin
    dt := Value;

    if not (csLoading in ComponentState) then
    begin
      if dt < DateStart then
        dt := DateStart;
    end;

    FEdit.Text := DateToStr(dt);
  end;
end;

procedure TPlannerRangeSelector.SetDateStart(const Value: TDateTime);
var
  dt: TDateTime;
begin
  dt := Value;

  if not (csLoading in ComponentState) then
  begin
    if (dt > DateEnd) then
      dt := DateEnd;
  end;

  Text := DateToStr(dt);
end;

procedure TPlannerRangeSelector.SetDroppedDown(const Value: boolean);
begin
  FDroppedDown := Value;

  if DroppedDown then
    DropDown
  else
    SetFocus;
end;

procedure TPlannerRangeSelector.SetEditMask(const Value: TEditMask);
begin
  inherited EditMask := Value;
  if Assigned(FEdit) then
    FEdit.EditMask := Value;
end;

procedure TPlannerRangeSelector.DoExit;
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

function TPlannerRangeSelector.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPlannerRangeSelector.ValidateError;
begin
  if Assigned(FOnInvalidDate) then
    FOnInvalidDate(Self)
  else
    inherited;
end;

procedure TPlannerRangeSelector.UpdateEditSize;
begin
  if Assigned(FEdit) then
  begin
    FEdit.Left := 2 + SEPARATOR_WIDTH + (Width - ButtonWidth - 4 - SEPARATOR_WIDTH) div 2;
    FEdit.Width := (Width - ButtonWidth - 4 - SEPARATOR_WIDTH) div 2 - 2;
  end;
end;

procedure TPlannerRangeSelector.UpdateSeparatorSize;
begin
  if Assigned(FSeparator) then
  begin
    FSeparator.Left := (Width - ButtonWidth - 4 - SEPARATOR_WIDTH) div 2;
    FSeparator.Width := SEPARATOR_WIDTH;
  end;
end;

procedure TPlannerRangeSelector.CreateSubControls;
begin
  if not Assigned(FEdit) then
  begin
    FEdit := TMaskEdit.Create(Self);
    FEdit.Parent := self;
    FEdit.BorderStyle := bsNone;
    UpdateEditSize;
    FEdit.Top := 2;
    FEdit.Text := DateToStr(Now);
  end;
  if not Assigned(FSeparator) then
  begin
    FSeparator := TStaticText.Create(Self);
    FSeparator.Parent := self;
    FSeparator.Top := 2;
    FSeparator.Caption := ':';
    UpdateSeparatorSize;
  end;
end;

initialization
  RegisterClass(TPlannerRangeSelector);

end.
