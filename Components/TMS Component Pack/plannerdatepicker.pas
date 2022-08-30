{***********************************************************************}
{ TPlannerDatePicker component                                          }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2015                                      }
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

unit PlannerDatePicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, AdvEdBtn, PlannerCal, AdvStyleIF, Variants;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // Version history
  // 1.4.0.0 : Property InActiveDays added
  // 1.5.0.0 : AutoThemeAdapt in Calendar added
  //         : XP style dropdown button added
  //         : Hover date color in Calendar added
  // 1.6.0.0 : New : ISO Week number support added (YearStartAt.ISOWeekNumber)
  //           New : OnCellDraw event added for TPlannerCalendarGroup
  //           New : OnDblClick event added for TPlannerCalendarGroup
  //           New : VS.NET (Whidbey) appearance style
  // 1.6.1.0 : New : support for Office 2007 silver style added
  // 1.6.2.0 : Improved : enhanced keyboard event routing from calendardropdown to datepicker control
  // 1.6.2.1 : Fixed : issue with initialization null date
  // 1.6.2.2 : Fixed : issue handling Clear method
  // 1.6.2.3 : Fixed : issue with GetDate proc
  // 1.6.2.4 : Fixed : issue with handling F4 key to show calendar
  // 1.7.0.0 : New : Terminal, Windows Vista, Windows 7 styles
  // 1.7.0.1 : Fixed : issue when using datepicker with year 9999
  // 1.7.0.2 : Fixed : issue with ReadOnly = true
  // 1.8.0.0 : New : Support for Office 2010 color settings
  // 1.8.0.1 : Fixed : Issue with focus handling when EditorEnabled = false & ReadOnly = true
  // 1.8.1.0 : New : BorderColor property added
  // 1.8.2.0 : New : DroppedDown public property added
  // 1.8.3.0 : Improved : Date formats supported
  // 1.9.0.0 : New : Windows 10, Office 2016 styles added


type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerDatePicker = class(TAdvEditBtn, ITMSStyle)
  private
    { Private declarations }
    FTMSStyle: TTMSStyle;
    FPlannerCalendar: TPlannerCalendar;
    APlannerCalendar: TPlannerCalendar;
    PlannerParent : TForm;
    CancelThisBtnClick : Boolean;
    FHideCalendarAfterSelection: boolean;
    FOnDaySelect: TDaySelectEvent;
    FDroppedDown: boolean;
    function GetOnGetDateHint: TGetDateEvent;
    function GetOnGetDateHintString: TGetDateEventHint;
    procedure SetOnGetDateHint(const Value: TGetDateEvent);
    procedure SetOnGetDateHintString(const Value: TGetDateEventHint);
    procedure HideParent;
    procedure InitEvents;
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
    function GetText(): string;
    procedure SetText(const Value: string);
    procedure SetDroppedDown(const Value: boolean);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; override;
    procedure BtnClick(Sender: TObject); override;
    procedure PlannerParentDeactivate(Sender: TObject);
    procedure PlannerCalendarDaySelect(Sender: TObject; SelDate: TDateTime);
    procedure PlannerCalendarKeyPress(Sender: TObject; var Key: Char);
    procedure PlannerCalendarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PlannerCalendarKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);      
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    // methods to do correct streaming, because the planner calendar is
    // stored on a hidden form
    function GetChildParent : TComponent; override;
    function GetChildOwner : TComponent; override;
    procedure Loaded; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure DaySelect; virtual;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure CancelBtnClick;
    destructor Destroy; override;
    procedure DropDown; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property Parent: TWinControl read GetParentEx write SetParentEx;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    function IsDateValid(): Boolean;
    property Text: string read GetText write SetText;
    property DroppedDown: boolean read FDroppedDown write SetDroppedDown;
  published
    { Published declarations }
    property Calendar : TPlannerCalendar read FPlannerCalendar write FPlannerCalendar;
    property TabOrder;
    property TabStop;
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
  end;

implementation

{$I DELPHIXE.INC}

{ TPlannerDatePicker }

procedure TPlannerDatePicker.DropDown;
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

  function CheckDate(dt: TDateTime): TDateTime;
  begin
    Result := dt;
    if Calendar.MinDate.Use then
      if dt < Calendar.MinDate.Date then
        Result := Calendar.MinDate.Date;
    if Calendar.MaxDate.Use then
      if dt > Calendar.MaxDate.Date then
        Result := Calendar.MinDate.Date;
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
  // Set planner position

  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      PlannerParent.FormStyle := fsStayOnTop;
  end
  else
    PlannerParent.FormStyle := fsStayOnTop;

  PlannerPosition.x := -2;
  PlannerPosition.y := Height - 3;
  PlannerPosition := ClientToScreen(PlannerPosition);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...

  if (plannerposition.y + FPlannerCalendar.Height > r.Bottom) then
    plannerposition.Y := plannerposition.Y - FPlannerCalendar.Height - Height + 3;

  if (plannerposition.x + FPlannerCalendar.Width > r.right) then
    plannerposition.x := plannerposition.x - (FPlannerCalendar.Width - Width);

  PlannerParent.Visible := false;
  PlannerParent.Width := 0;
  PlannerParent.Height := 0;
  PlannerParent.Left := - 200;
  PlannerParent.Top := - 200;
  PlannerParent.Visible := true;

  // Set planner date
  if FPlannerCalendar.MultiSelect then
    Text := FPlannerCalendar.DatesAsText
  else
  begin
    try
      if (Text = '') then
        FPlannerCalendar.Date := CheckDate(Now)
      else
        FPlannerCalendar.Date := VarToDateTime(Text);
    except
      on Exception do
         Text := FPlannerCalendar.DatesAsText;
    end;
  end;

  MoveWindow(PlannerParent.Handle, plannerposition.X, plannerposition.Y, 0, 0, true);
  //PlannerParent.Show;
  FPlannerCalendar.SetFocus;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);

  FDroppedDown := true;
end;

procedure TPlannerDatePicker.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  case AStyle of
  tsOffice2003Blue: Calendar.Style := psOffice2003Blue;
  tsOffice2003Olive: Calendar.Style := psOffice2003Olive;
  tsOffice2003Silver: Calendar.Style := psOffice2003Silver;
  tsOffice2003Classic: Calendar.Style := psOffice2003Classic;
  tsOffice2007Luna: Calendar.Style := psOffice2007Luna;
  tsOffice2007Obsidian: Calendar.Style := psOffice2007Obsidian;
  tsOffice2007Silver: Calendar.Style := psOffice2007Silver;
  tsWindowsXP: Calendar.Style := psWindowsXP;
  tsWhidbey: Calendar.Style := psWhidbey;
  tsWindowsVista: Calendar.Style := psWindowsVista;
  tsWindows7: Calendar.Style := psWindows7;
  tsTerminal: Calendar.Style := psTerminal;
  tsOffice2010Blue: Calendar.Style := psOffice2010Blue;
  tsOffice2010Silver: Calendar.Style := psOffice2010Silver;
  tsOffice2010Black: Calendar.Style := psOffice2010Black;
  tsWindows8: Calendar.Style := psWindows8;
  tsOffice2013White: Calendar.Style := psOffice2013White;
  tsOffice2013LightGray: Calendar.Style := psOffice2013LightGray;
  tsOffice2013Gray: Calendar.Style := psOffice2013Gray;
  tsWindows10: Calendar.Style := psWindows10;
  tsOffice2016White: Calendar.Style := psOffice2016White;
  tsOffice2016Gray: Calendar.Style := psOffice2016Gray;
  tsOffice2016Black: Calendar.Style := psOffice2016Black;
  end;

end;


procedure TPlannerDatePicker.BtnClick(Sender: TObject);
begin
  CancelThisBtnClick := False;
  inherited;
  // call event OnClick - the user can cancel calendar appearance of calendar by calling .CancelBtnClick
  if CancelThisBtnClick then
    Exit;

  if not ReadOnly then
    DropDown;
end;

procedure TPlannerDatePicker.CancelBtnClick;
begin
  CancelThisBtnClick := True;
end;

constructor TPlannerDatePicker.Create(AOwner: TComponent);
begin
  inherited;
  // Make planner parent form and a planner, put planner on parent form
  Text := '';
  PlannerParent := TForm.Create(Self);
  PlannerParent.BorderStyle := bsNone;
  FPlannerCalendar := TPlannerCalendar.Create(Self);
  FPlannerCalendar.Parent := PlannerParent;

  FPlannerCalendar.Name := self.Name +'cal'+inttostr(AOwner.ComponentCount)+'_';
  PlannerParent.Autosize := True;
  PlannerParent.OnDeactivate := PlannerParentDeactivate;
  FPlannerCalendar.OnDaySelect := PlannerCalendarDaySelect;
  Width := FPlannerCalendar.Width;
  FHideCalendarAfterSelection := True;
  Button.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO));
  // Make the button NOT change the focus
  Button.FocusControl := nil;
  ButtonStyle := bsDropDown;
end;


procedure TPlannerDatePicker.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TPlannerDatePicker.Destroy;
begin
  FPlannerCalendar.Free;
  PlannerParent.Free;
  inherited;
end;

function TPlannerDatePicker.GetChildOwner: TComponent;
begin
  Result := PlannerParent;
end;

function TPlannerDatePicker.GetChildParent: TComponent;
begin
  Result := PlannerParent;
end;

procedure TPlannerDatePicker.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  Proc(FPlannerCalendar);
  FPlannerCalendar.Parent := PlannerParent;
end;

function TPlannerDatePicker.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TPlannerDatePicker.GetOnGetDateHint: TGetDateEvent;
begin
  Result := FPlannerCalendar.OnGetDateHint;
end;

function TPlannerDatePicker.GetOnGetDateHintString: TGetDateEventHint;
begin
  Result := FPlannerCalendar.OnGetDateHintString;
end;

procedure TPlannerDatePicker.HideParent;
begin
  PlannerParent.Hide;
  try
    SetFocus;
  except
  end;  
end;

procedure TPlannerDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  s:string;
  ss: integer;
  ssl: integer;
  dfus: boolean;

begin
  inherited;

  if (key = VK_DOWN) and (ssAlt in Shift) then
  begin
    if PlannerParent.Visible then
      HideParent
    else
      BtnClick(Self);
    Exit;
  end;
 
  if (key = VK_F4) and not (ssAlt in Shift) and not (ssCtrl in Shift) then
  begin
    //if PlannerParent.Visible then
    //  HideParent
    //else
    //begin
      BtnClick(Self);
    //end;
  end;

  dfus := pos('M',Uppercase(ShortDateFormat)) < pos('D',Uppercase(ShortDateFormat));

  if ReadOnly then
    Exit;

  case key of
  VK_DOWN:
    begin
      s := Text;
      ss := SelStart;
      ssl := SelStart;
      if (ss > pos(DateSeparator,s)) then
      begin
        ss := ss - pos(DateSeparator,s);
        Delete(s,1,pos(DateSeparator,s));
        if (ss > pos(DateSeparator,s)) then
        begin
          FPlannerCalendar.Date := Date;
          FPlannerCalendar.ChangeYear(-1);
          Date := FPlannerCalendar.Date;
        end
        else
        begin
          FPlannerCalendar.Date := Date;

          if dfus then
            Date := Date -1
          else
          begin
            FPlannerCalendar.ChangeMonth(-1);
            Date := FPlannerCalendar.Date;
          end;
        end;
      end
      else
      begin
        if dfus then
        begin
          FPlannerCalendar.ChangeMonth(-1);
          Date := FPlannerCalendar.Date;
        end
        else
          Date := Date - 1;
      end;

      SelStart := ssl;

    end;
  VK_UP:
    begin
      s := Text;
      ss := SelStart;
      ssl := SelStart;
      if (ss > pos(DateSeparator,s)) then
      begin
        ss := ss - pos(DateSeparator,s);
        Delete(s,1,pos(DateSeparator,s));
        if (ss > pos(DateSeparator,s)) then
        begin
          FPlannerCalendar.Date := Date;
          FPlannerCalendar.ChangeYear(+1);
          Date := FPlannerCalendar.Date;
        end
        else
        begin
          FPlannerCalendar.Date := Date;
          if dfus then
            Date := Date + 1
          else
          begin
            FPlannerCalendar.ChangeMonth(+1);
            Date := FPlannerCalendar.Date;
          end;
        end;
      end
      else
      begin
        if dfus then
        begin
          FPlannerCalendar.ChangeMonth(+1);
          Date := FPlannerCalendar.Date;
        end
        else
          Date := Date + 1;
      end;
      SelStart := ssl;
    end;
  end;
end;

procedure TPlannerDatePicker.InitEvents;
begin
  FPlannerCalendar.OnDaySelect := PlannerCalendarDaySelect;
  FPlannerCalendar.OnKeyPress := PlannerCalendarKeypress;
  FPlannerCalendar.OnKeyUp := PlannerCalendarKeyUp;
  FPlannerCalendar.OnKeyDown := PlannerCalendarKeyDown;  
end;

procedure TPlannerDatePicker.Loaded;
begin
  inherited;

  //if (not (csDesigning in ComponentState)) then
    if PlannerParent.ComponentCount > 0 then
    begin
      APlannerCalendar := (PlannerParent.Components[0] as TPlannerCalendar);
      APlannerCalendar.OnGetDateHint := FPlannerCalendar.OnGetDateHint;
      APlannerCalendar.OnGetDateHintString := FPlannerCalendar.OnGetDateHintString;
      APlannerCalendar.Color := FPlannerCalendar.Color;
      FPlannerCalendar.Free;
      FPlannerCalendar := APlannerCalendar;
      InitEvents;
    end;
end;

procedure TPlannerDatePicker.PlannerCalendarDaySelect(Sender: TObject; SelDate: TDateTime);
begin
  Text := FPlannerCalendar.DatesAsText;

  if FHideCalendarAfterSelection then
    HideParent;
    
  DaySelect;
  if Assigned(FOnDaySelect) then
    FOnDaySelect(Self,SelDate);
end;

procedure TPlannerDatePicker.DaySelect;
begin
end;

procedure TPlannerDatePicker.PlannerCalendarKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
//  if Key = VK_F4 then
//    HideParent;

  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end;


procedure TPlannerDatePicker.PlannerCalendarKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;


procedure TPlannerDatePicker.PlannerCalendarKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
  if (Key = #13) then
  begin
    PlannerCalendarDaySelect(Sender, FPlannerCalendar.Date);
  end;
  if Key = #27 then
  begin
    HideParent;
  end;
end;

procedure TPlannerDatePicker.PlannerParentDeactivate(Sender: TObject);
begin
  (Sender as TForm).Hide;
  FDroppedDown := false;
end;

procedure TPlannerDatePicker.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TPlannerDatePicker.SetOnGetDateHint(const Value: TGetDateEvent);
begin
  FPlannerCalendar.OnGetDateHint := Value;
end;

procedure TPlannerDatePicker.SetOnGetDateHintString(
  const Value: TGetDateEventHint);
begin
  FPlannerCalendar.OnGetDateHintString := Value;
end;

function TPlannerDatePicker.GetOnGetEventProp: TEventPropEvent;
begin
  Result := FPlannerCalendar.OnGetEventProp;
end;

procedure TPlannerDatePicker.SetOnGetEventProp(
  const Value: TEventPropEvent);
begin
  FPlannerCalendar.OnGetEventProp := Value;
end;


procedure TPlannerDatePicker.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled then
    inherited
  else
  begin
    if Button.Enabled then
      Button.SetFocus;
  end;
end;

procedure TPlannerDatePicker.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TPlannerDatePicker.Change;
var
  dt: TDateTime;
begin
  inherited;
  // try to extract the date
  try
    if (Text = '') or
       (Pos(DateSeparator, Text) = 0) or
       (Text = '  /  /  ') then
	  
	  if Assigned(Calendar) then
        Calendar.Date := 0
    else
    begin
      dt := VarToDateTime(Text);

	  if Assigned(Calendar) then
        Calendar.Date := dt;
    end;
  except
  end;
end;

procedure TPlannerDatePicker.CreateWnd;
begin
  inherited;
  InitEvents;
end;

function TPlannerDatePicker.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TPlannerDatePicker.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
  InitEvents;
end;

function TPlannerDatePicker.GetOnWeekSelect: TNotifyEvent;
begin
  Result := FPlannerCalendar.OnWeekSelect;
end;

procedure TPlannerDatePicker.SetOnWeekSelect(const Value: TNotifyEvent);
begin
  FPlannerCalendar.OnWeekSelect := Value;
end;

function TPlannerDatePicker.GetOnAllDaySelect: TNotifyEvent;
begin
  Result := FPlannerCalendar.OnAllDaySelect;
end;

procedure TPlannerDatePicker.SetOnAllDaySelect(const Value: TNotifyEvent);
begin
  FPlannerCalendar.OnAllDaySelect := Value;
end;

function TPlannerDatePicker.IsDateValid(): Boolean;
begin
	Result := False;
	try
	  VarToDateTime(Trim(Text));
	  Result := True;
	except
	end;
end;

function TPlannerDatePicker.GetDate: TDateTime;
begin
  if Length(Trim(Text)) = 0 then
    Result := 0
  else
    try
      Result := VarToDateTime(Trim(Text));
      //Result := StrToDateDef(Trim(Text),0);
    except
      Result := 0;
    end;
end;

procedure TPlannerDatePicker.SetDate(const Value: TDateTime);
begin
  FPlannerCalendar.Date := Value;
  if Value = 0 then
    Text := ''
  else
    Text := DateToStr(Value);
end;


procedure TPlannerDatePicker.SetDroppedDown(const Value: boolean);
begin
  FDroppedDown := Value;

  if FDroppedDown then
    DropDown
  else
    SetFocus;
end;

function TPlannerDatePicker.GetText(): string;
begin
	Result := inherited Text;
end;

procedure TPlannerDatePicker.SetText(const Value: string);
begin
	try
		inherited Text := Value;
	  except
		if not (csLoading in ComponentState) then
			raise;
	  end;
end;

function TPlannerDatePicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

initialization
  {$IFDEF ISDELPHI}
  try
    RegisterClass(TPlannerDatePicker);
  except
  end;
  {$ENDIF}

{$IFDEF FREEWARE}
{$IFNDEF VER170}
  if  (FindWindow('TApplication', nil) = 0) OR
      (FindWindow('TAlignPalette', nil) = 0) OR
      (FindWindow('TPropertyInspector', nil) = 0) OR
      (FindWindow('TAppBuilder', nil) = 0) then
  begin
    ShowMessage('TMS Planner Calendars and DatePickers trial version');
  end
{$ENDIF}
{$ENDIF}

end.
