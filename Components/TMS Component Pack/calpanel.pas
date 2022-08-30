{********************************************************************}
{ CALPANEL component                                                 }
{ for Delphi & C++ Builder                                           }
{                                                                    }
{ written by TMS Software                                            }
{           copyright © 1997-2012                                    }
{           Email : info@tmssoftware.com                             }
{           Website : http://www.tmssoftware.com                     }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{code remains property of the writer and may not be distributed      }
{freely as such.                                                     }
{********************************************************************}

{$I TMSDEFS.INC}

unit CalPanel;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

resourcestring
  selstr = 'Select month';

const
  adaysinmonth: array[1..13] of word = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 29);
  monames: array[1..12] of string[5] = ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUNE', 'JULY', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  labelx = 30;
  labelw = 65;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TDayStr = string;
  TMonthStr = string;

  TDayArray = array[1..14] of TDayStr;
  TMonthArray = array[1..12] of TMonthStr;

  TDaySelectEvent = procedure(Sender: TObject; SelDate: tDateTime) of object;
  TDateChangeEvent = procedure(Sender: TObject; origDate, newDate: tDateTime) of object;
  TMonthChangeEvent = procedure(Sender: TObject; origDate, newDate: tDateTime) of object;

  TGetDateEvent = procedure(Sender: TObject; dt: tdatetime; var isEvent: boolean) of object;
  TGetDateEventHint = procedure(Sender: TObject; dt: tdatetime; var isEvent: boolean; var EventHint: string) of object;

  TYearStartAt = class(TPersistent)
  private
    FStartDay: integer;
    FStartMonth: integer;
    procedure SetStartDay(d: integer);
    procedure SetStartMonth(m: integer);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property StartDay: integer read FStartDay write SetStartDay;
    property StartMonth: integer read FStartMonth write SetStartMonth;
  end;

  TNameofMonths = class(TPersistent)
  private
    FJanuary: TMonthStr;
    FFebruary: TMonthStr;
    FMarch: TMonthStr;
    FApril: TMonthStr;
    FMay: TMonthStr;
    FJune: TMonthStr;
    FJuly: TMonthStr;
    FAugust: TMonthStr;
    FSeptember: TMonthStr;
    FOctober: TMonthStr;
    FNovember: TMonthStr;
    FDecember: TMonthStr;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetMonth(i: integer): string;
  published
    property January: TMonthStr read FJanuary write FJanuary;
    property February: TMonthStr read FFebruary write FFebruary;
    property March: TMonthStr read FMarch write FMarch;
    property April: TMonthStr read FApril write FApril;
    property May: TMonthStr read FMay write FMay;
    property June: TMonthStr read FJune write FJune;
    property July: TMonthStr read FJuly write FJuly;
    property August: TMonthStr read FAugust write FAugust;
    property September: TMonthStr read FSeptember write FSeptember;
    property October: TMonthStr read FOctober write FOctober;
    property November: TMonthStr read FNovember write FNovember;
    property December: TMonthStr read FDecember write FDecember;
  end;

  TNameofDays = class(TPersistent)
  private
    FMonday: TDayStr;
    FTuesday: TDayStr;
    FWednesday: TDayStr;
    FThursday: TDayStr;
    FFriday: TDayStr;
    FSaturday: TDayStr;
    FSunday: TDayStr;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function GetDay(i: integer): string;
  published
    property Monday: TDayStr read FMonday write FMonday;
    property Tuesday: TDayStr read FTuesday write FTuesday;
    property Wednesday: TDayStr read FWednesday write FWednesday;
    property Thursday: TDayStr read FThursday write FThursday;
    property Friday: TDayStr read FFriday write FFriday;
    property Saturday: TDayStr read FSaturday write FSaturday;
    property Sunday: TDayStr read FSunday write FSunday;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCalPanel = class(TCustomPanel)
  private
    xoffset, yoffset: integer;
    seldate: tdatetime;
    thedate: tdatetime;
    initdate: tdatetime;
    showhintassigned: boolean;
    showhintbusy: boolean;
    fLastHintPos: tpoint;
    dx, dy: word;
    lblx1, lblx2: word;
    fFont: tfont;
    xposin, yposin: integer;
    flgl, flgr, flgla: boolean;
    labels: string;
    eventhint: string;
    FMonthSelect: boolean;
    FEventHints: boolean;
    FEventDayColor: TColor;
    FYearStartAt: TYearStartAt;
    FNameofDays: TNameofDays;
    FNameofMonths: TNameofMonths;
    FTextcolor: TColor;
    FSelectcolor: TColor;
    FInverscolor: TColor;
    FWeekendcolor: TColor;
    FShowWeeks: boolean;
    FStartDay: integer;
    FDay, FMonth, FYear: word;
    FDayMode: boolean;
    FYearChanged: boolean;
    FOnDaySelect: TDaySelectEvent;
    FOnMonthSelect: TNotifyEvent;
    FOnGetDateEvent: TGetDateEvent;
    FOnGetDateEventHint: TGetDateEventHint;
    FOnDateChange: TDateChangeEvent;
    FOnMonthChange: TMonthChangeEvent;
    procedure WMKeyDown(var Msg: TWMKeydown); message wm_keydown;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetLabel(mo, ye: word);
    procedure ChangeMonth(dx: integer);
    procedure ChangeYear(dx: integer);
    function DaysInMonth(mo, ye: word): word;
    function DateToWeek(da, mo, ye: word): integer;
    procedure PaintArrowLeft;
    procedure PaintArrowRight;
    procedure PaintLabel;
    procedure PaintProc;
    procedure ToggleDayMode;
    procedure SetTextColor(aColor: TColor);
    procedure SetInversColor(aColor: TColor);
    procedure SetWeekendColor(aColor: TColor);
    procedure SetSelectColor(aColor: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetFont(value: tFont);
    procedure SetNameofDays(aNameofDays: TNameofDays);
    procedure SetNameofMonths(aNameofMonths: TNameofMonths);
    procedure SetShowWeeks(aValue: boolean);
    procedure SetStartDay(avalue: integer);
    procedure SetCalDay(avalue: word);
    procedure SetCalMonth(avalue: word);
    procedure SetCalYear(avalue: word);
    function GetCalDay: word;
    function GetCalMonth: word;
    function GetCalYear: word;
    procedure SetDayMode(aValue: boolean);
    function XYToDate(X, Y: integer; change: boolean): tdatetime;
    function DateToXY(dt: tdatetime): tpoint;
    procedure ShowHintProc(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    function GetDateProc: tDatetime;
    procedure SetDateProc(const Value: tDatetime);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDate(da, mo, ye: word);
    procedure GetDate(var da, mo, ye: word);
    property Date: tDatetime read GetDateProc write SetDateProc;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
  published
    property Align;
    property Constraints;
    property TextColor: tColor read FTextColor write SetTextColor;
    property SelectColor: tColor read FSelectColor write SetSelectColor;
    property InversColor: tColor read FInversColor write SetInversColor;
    property WeekendColor: tColor read FWeekendColor write SetWeekendColor;
    property NameofDays: TNameofDays read FNameofDays write SetNameofDays;
    property NameofMonths: TNameofMonths read FNameofMonths write SetNameofMonths;
    property YearStartAt: TYearStartAt read FYearStartAt write FYearStartAt;
    property ShowWeeks: boolean read FShowWeeks write SetShowWeeks;
    property StartDay: integer read FStartDay write SetStartDay;
    property DayMode: boolean read FDayMode write SetDayMode;
    property Day: word read GetCalDay write SetCalDay default 1;
    property Month: word read GetCalMonth write SetCalMonth default 1;
    property Year: word read GetCalYear write SetCalYear default 1;
    property MonthSelect: boolean read fMonthSelect write fMonthSelect;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property ShowHint;
    property ParentShowHint;
    property EventHints: boolean read fEventHints write fEventHints;
    property EventDayColor: TColor read fEventDayColor write fEventDayColor;
    property OnDaySelect: TDaySelectEvent read FOnDaySelect write FOnDaySelect;
    property OnMonthSelect: TNotifyEvent read FOnMonthSelect write FOnMonthSelect;
    property OnGetDateHint: TGetDateEvent read FOnGetDateEvent write FOnGetDateEvent;
    property OnGetDateHintString: TGetDateEventHint read FOnGetDateEventHint write FOnGetDateEventHint;
    property OnMonthChange: TMonthChangeEvent read FOnMonthChange write FOnMonthChange;
    property OnDateChange: TDateChangeEvent read FOnDateChange write FOnDateChange;
    property TabStop;
    property TabOrder;
    property Font: TFont read FFont write SetFont;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

constructor TCalPanel.Create(AOwner: TComponent);
{$IFNDEF WIN32}
var
  tmp: tshowhintevent;
{$ENDIF}
begin
  inherited Create(aOwner);
  FNameofDays := TNameofDays.Create;
  FNameofMonths := TNameofMonths.Create;
  FYearStartAt := TYearStartAt.Create;
  fFont := TFont.Create;
  FMonthSelect := true;
  xoffset := 0;
  yoffset := 16;
  thedate := now;
  seldate := thedate;
  daymode := true;
  self.ChangeMonth(0);
  flgl := false;
  flgr := false;
  flgla := false;
  width := 132;
  height := 135;
  FSelectColor := clTeal;
  FInversColor := clWhite;
  FTextColor := clBlack;
  FWeekendColor := clRed;
  FStartDay := 7;
  DecodeDate(theDate, FYear, FMonth, FDay);
  caption := '';
  showhintassigned := false;
  showhintbusy := false;

  FLastHintPos := point(-1, -1);
  Font.onChange := FontChanged;
end;

destructor TCalPanel.Destroy;
begin
  FNameofDays.Destroy;
  FNameofMonths.Destroy;
  FYearStartAt.Destroy;
  FFont.Free;
  inherited Destroy;
end;

procedure TCalPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCalPanel.SetFont(value: tFont);
begin
  FFont.Assign(value);
  Canvas.Font.Assign(fFont);
end;

procedure TCalPanel.FontChanged(sender: TObject);
begin
  Canvas.Font.assign(font);
  SetShowWeeks(FShowWeeks);
  Width := 10 + xoffset + (6 + self.canvas.textwidth('99')) * 7;
  Height := 10 + yoffset + (4 + self.canvas.textheight('9')) * 7;
  Repaint;
end;

procedure TCalPanel.ShowHintProc(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if FEventHints then
  begin
    CanShow := (eventhint <> '') and not ((fLastHintPos.x = -1) or (fLastHintPos.y = -1));
    ShowHintBusy := canshow;
    if canshow then
    begin
      hintinfo.hintpos.x := (fLastHintPos.x + 1) * dx;
      hintinfo.hintpos.y := fLastHintPos.y * dy + yoffset;
      hintstr := EventHint;
      hintinfo.hintpos := ClientToScreen(hintinfo.hintpos);
    end;
  end;
end;

procedure TCalPanel.DoEnter;
begin
  inherited DoEnter;
  Repaint;
end;

procedure TCalPanel.DoExit;
begin
  inherited DoExit;
  Repaint;
end;

procedure TCalPanel.Loaded;
begin
  inherited Loaded;
  seldate := encodedate(fyear, fmonth, fday);
  thedate := seldate;
end;


procedure TCalPanel.SetTextColor(aColor: TColor);
begin
  fTextColor := Acolor;
  self.repaint;
end;

procedure TCalPanel.SetInversColor(aColor: TColor);
begin
  fInversColor := Acolor;
  self.repaint;
end;

procedure TCalPanel.SetWeekendColor(aColor: TColor);
begin
  fWeekendColor := Acolor;
  self.repaint;
end;

procedure TCalPanel.SetSelectColor(aColor: TColor);
begin
  fSelectColor := Acolor;
  self.repaint;
end;


procedure TCalPanel.SetLabel(mo, ye: word);
begin
  if daymode then labels := FNameofMonths.GetMonth(mo) + ' ' + inttostr(ye) else
    labels := inttostr(ye);
end;

function TCalPanel.DaysInMonth(mo, ye: word): word;
begin
  if (mo <> 2) then DaysInMonth := adaysinmonth[mo] else
  begin
    if (ye mod 4 = 0) then DaysInMonth := 29 else
      DaysInMonth := 28;
    if (ye mod 100 = 0) then DaysInMonth := 28;
    if (ye mod 400 = 0) then DaysInmonth := 29;
  end;
end;


function TCalPanel.DateToWeek(da, mo, ye: word): integer;
var
  days1, days2: real;
  d1, d2: tdatetime;
  fday: integer;

begin
  result := 1;
  if not assigned(FYearStartAt) then exit;

  d2 := encodedate(ye, mo, da);
  if (FYearStartAt.FStartMonth > mo) then dec(ye)
  else
    if ((FYearStartAt.FStartMonth = mo) and (FYearStartAt.FStartDay > da)) then dec(ye);

  d1 := encodedate(ye, FYearStartAt.FStartMonth, FYearStartAt.FStartDay);
  fday := dayofweek(d1);

  days2 := int(d2);
  days1 := int(d1);

  days1 := (days2 - days1) + (fday - 2);

  DateToWeek := (trunc(days1) div 7) + 1;
end;

procedure TCalPanel.SetDayMode(aValue: boolean);
begin
  FDayMode := aValue;
  self.repaint;
end;

procedure TCalPanel.SetStartDay(aValue: integer);
begin
  if aValue < 1 then aValue := 1;
  if aValue > 7 then aValue := 7;
  fStartDay := aValue;
  self.repaint;
end;

procedure TCalPanel.SetShowWeeks(aValue: boolean);
begin
  if aValue then
  begin
    xoffset := self.canvas.textwidth('999');
    width := xoffset + 10 + (6 + self.canvas.textwidth('99')) * 7;
  end
  else
    if not (csLoading in ComponentState) then
    begin
      xoffset := 0;
      width := 10 + xoffset + (6 + self.canvas.textwidth('99')) * 7;
    end;
  FShowWeeks := Avalue;
  self.repaint;
end;

procedure TCalPanel.SetCalDay(aValue: word);
begin
  try
    SetDate(aValue, FMonth, FYear);
    FDay := aValue;
  except
    messagedlg('Invalid date', mtError, [mbok], 0);
  end;
  repaint;
end;

procedure TCalPanel.SetCalMonth(aValue: word);
begin
  try
    SetDate(FDay, aValue, FYear);
    FMonth := aValue;
  except
    messagedlg('Invalid date', mtError, [mbok], 0);
  end;
  repaint;
end;

procedure TCalPanel.SetCalYear(aValue: word);
begin
  try
    SetDate(FDay, FMonth, aValue);
    FYear := aValue;
  except
    messagedlg('Invalid date', mtError, [mbok], 0);
  end;
  repaint;
end;

function TCalPanel.GetCalDay: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  result := da;
end;

function TCalPanel.GetCalMonth: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  result := mo;
end;

function TCalPanel.GetCalYear: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  result := ye;
end;


procedure TCalPanel.SetNameofDays(ANameofDays: TNameofDays);
begin
  FNameofDays := ANameofDays;
  self.repaint;
end;


procedure TCalPanel.SetNameofMonths(ANameofMonths: TNameofMonths);
begin
  FNameofMonths := ANameofMonths;
  self.repaint;
end;

procedure TCalPanel.ChangeMonth(dx: integer);
var
  ye, mo, da: word;
begin

  decodedate(thedate, ye, mo, da);
  mo := mo + dx;

  if (mo = 13) then
  begin
    inc(ye);
    mo := 1;
  end;

  if (mo = 0) then
  begin
    dec(ye);
    mo := 12;
  end;

  if (da > daysinmonth(mo, ye)) then da := daysinmonth(mo, ye);
  thedate := encodedate(ye, mo, da);
  seldate := thedate;
  FYearChanged := true;
  self.SetLabel(mo, ye);
  self.repaint;
end;


procedure TCalPanel.ChangeYear(dx: integer);
var
  ye, mo, da: word;

begin
  decodedate(thedate, ye, mo, da);
  ye := ye + dx;

  thedate := encodedate(ye, mo, da);
  seldate := thedate;
  FYearChanged := true;
  self.SetLabel(mo, ye);

  self.repaint;
end;

procedure TCalPanel.PaintArrowLeft;
begin
  with self.canvas do
  begin
    if flgl then
    begin
      brush.color := fselectcolor;
      pen.color := fselectcolor;
    end
    else
    begin
      brush.color := ftextcolor;
      pen.color := ftextcolor;
    end;

    if daymode then
      polygon([Point(xoffset + 10, 1), Point(xoffset + 5, 6), Point(xoffset + 10, 11)])
    else
      polygon([Point(10, 1), Point(5, 6), Point(10, 11)]);

    brush.color := self.color;
  end;
end;

procedure TCalPanel.PaintArrowRight;
begin
  with self.canvas do
  begin
    if flgr then
    begin
      brush.color := fselectcolor;
      pen.color := fselectcolor;
    end
    else
    begin
      brush.color := textcolor;
      pen.color := textcolor;
    end;
    polygon([Point(width - 10, 1), Point(width - 10, 11), Point(width - 5, 6)]);
    brush.color := self.Color;
  end;
end;

procedure TCalPanel.PaintLabel;
var
  buf: array[0..12] of char;
  l: longint;
  xofs: integer;
begin
  with self.Canvas do
  begin
    strpcopy(buf, labels);

    l := textwidth(labels);

    if flgla then font.color := fselectcolor else font.color := ftextcolor;
    setbkmode(handle, TRANSPARENT);

    if daymode then xofs := xoffset else xofs := 0;

    textout(xofs + ((self.width - loword(l) - xofs) shr 1), 0, labels);
    font.color := ftextcolor;
    lblx1 := (self.width - loword(l) - xofs) shr 1;
    lblx2 := lblx1 + loword(l);
  end;
end;

procedure TCalPanel.PaintProc;
var
  i, j: word;
  d: tdatetime;
  da, mo, ye, pmo, pye, nmo, nye, sda, cda, cmo, cye: word;
  fd: integer;
  tmpstr: string;
  isEvent: boolean;
  r: trect;
  oldStyle: TFontStyles;

  function SmallCaps(s: string): string;
  var
    buf: array[0..10] of char;
  begin
    strpcopy(buf, s);
    strlower(buf);
    s := strpas(buf);
    s[1] := upcase(s[1]);
    SmallCaps := s;
  end;

begin
  if not assigned(FNameofDays) then exit;
  if not assigned(FNameofMonths) then exit;

  decodedate(seldate, ye, mo, sda);
  decodedate(thedate, ye, mo, da);
  decodedate(now, cye, cmo, cda);

  self.Canvas.Font.Assign(fFont);

  with self.Canvas do
  begin
    dx := textwidth('99') + 6;
    dy := textheight('99') + 4;
  end;

  self.PaintArrowLeft;
  self.PaintArrowRight;
  self.PaintLabel;

  if daymode then
  begin
    d := encodedate(ye, mo, 1);

   {first day of the month}

    fd := dayofweek(d) - 1 - startday;

    if (fd < 0) then fd := fd + 7;
    if (fd > 7) then fd := fd - 7;

    if (mo = 1) then
    begin
      pmo := 12;
      pye := ye - 1;
    end
    else
    begin
      pmo := mo - 1;
      pye := ye;
    end;

    if (mo = 12) then
    begin
      nmo := 1;
      nye := ye + 1;
    end
    else
    begin
      nmo := mo + 1;
      nye := ye;
    end;

    with self.Canvas do
    begin
      font.color := ftextcolor;
      setbkmode(handle, TRANSPARENT);

      if fShowWeeks then
      begin
        pen.color := ftextcolor;
          //moveto(18,dy-3+yoffset);
          //lineto(18,height);
        moveto(xoffset - 2, dy - 3 + yoffset);
        lineto(xoffset - 2, height);


        j := datetoweek(1, mo, ye);
        for i := 1 to 6 do
        begin
          if (i + j - 1) > 53 then tmpstr := inttostr(i + j - 1 - 53) else
            tmpstr := inttostr(i + j - 1);

          textout(4, i * dy + yoffset, tmpstr);
        end;
      end;

     {draw days here}
      settextalign(handle, TA_RIGHT);

      for i := 1 to 7 do
        textout(i * dx + xoffset, yoffset, ' ' + FNameofDays.GetDay(i + startday - 1));

     {draw line under days}
      pen.color := clblack;
      moveto(xoffset + 0, dy - 5 + yoffset);
      lineto(xoffset + 7 * dx + 6, dy - 5 + yoffset);
      pen.color := clwhite;
      moveto(xoffset + 0, dy - 4 + yoffset);
      lineto(xoffset + 7 * dx + 6, dy - 4 + yoffset);
      pen.color := clblack;

      oldStyle := font.style;

     {draw numbers}
      for i := 1 to 7 do
        for j := 1 to 6 do
        begin
          font.style := [];
          isEvent := false;

          settextalign(canvas.handle, TA_RIGHT);

          if (fd >= (i + (j - 1) * 7)) then
          begin
            if assigned(fOnGetDateEvent) then
              fOnGetDateEvent(self, encodedate(pye, pmo, (i + (j - 1) * 7)), isEvent);

            font.color := finverscolor;
            if isEvent then font.style := [fsBold];
            if isEvent then font.color := fEventDayColor;

            setbkmode(handle, TRANSPARENT);
            textout(xoffset + i * dx, j * dy + yoffset, inttostr(daysinmonth(pmo, pye) - (fd - i)));
          end
          else
          begin
            if ((i + (j - 1) * 7 - fd) > daysinmonth(mo, ye)) then
            begin
              font.color := finverscolor;

              if assigned(fOnGetDateEvent) then
                fOnGetDateEvent(self, encodedate(nye, nmo, i + (j - 1) * 7 - fd - daysinmonth(mo, ye)), isEvent);

              if isEvent then font.style := [fsBold];
              if isEvent then font.color := fEventDayColor;

              setbkmode(handle, TRANSPARENT);
              textout(xoffset + i * dx, j * dy + yoffset, inttostr(i + (j - 1) * 7 - fd - daysinmonth(mo, ye)));
            end
            else
            begin
              if assigned(fOnGetDateEvent) then
                fOnGetDateEvent(self, encodedate(ye, mo, (i + (j - 1) * 7 - fd)), isEvent);

              if isEvent then font.style := [fsBold];
              if isEvent then font.color := fEventDayColor;

              if (sda = i + (j - 1) * 7 - fd) then
              begin
                brush.color := fselectcolor;
                pen.color := fselectcolor;
                font.color := finverscolor;

                textout(xoffset + i * dx, j * dy + yoffset, inttostr(i + (j - 1) * 7 - fd));

                brush.color := self.color;
                pen.color := ftextcolor;

                if (getfocus = self.handle) then
                begin
                  r.right := xoffset + i * dx + 2;
                  r.top := j * dy + yoffset - 2;
                  r.bottom := r.top + dy;
                  r.left := r.right - dx + 2;
                  pen.color := clBlack;
                  Drawfocusrect(r);
                end;

              end
              else
              begin {check to see if weekend day here}
                if not isevent then
                begin
                  if (dayofweek(encodedate(ye, mo, i + (j - 1) * 7 - fd)) in [1, 7]) then
                    font.color := fweekendcolor
                  else
                    font.color := ftextcolor;
                end;

                setbkmode(handle, TRANSPARENT);
                textout(xoffset + i * dx, j * dy + yoffset, inttostr(i + (j - 1) * 7 - fd));

              end;

            end;
          end;

          if (cda = i + (j - 1) * 7 - fd) and (cmo = mo) and (cye = ye) then
          begin
            pen.color := clgray;
            moveto(xoffset + (i - 1) * dx + 4, (j + 1) * dy - 4 + yoffset);
            lineto(xoffset + (i - 1) * dx + 4, j * dy - 2 + yoffset);
            lineto(xoffset + i * dx + 2, j * dy - 2 + yoffset);
            pen.color := clwhite;
            lineto(xoffset + i * dx + 2, (j + 1) * dy - 4 + yoffset);
            lineto(xoffset + (i - 1) * dx + 4, (j + 1) * dy - 4 + yoffset);
            pen.color := clblack;
          end;


        end;

      font.style := oldStyle;
    end;
  end
  else
  begin
    with self.Canvas do
    begin

     {draw days here}
      textout((width - textwidth(selstr)) shr 1, yoffset, selstr);

     {draw line under days}
      pen.color := clblack;
      moveto(0, dy - 5 + yoffset);
      lineto(width, dy - 5 + yoffset);
      pen.color := clWhite;
      moveto(0, dy - 4 + yoffset);
      lineto(width, dy - 4 + yoffset);
      pen.color := clblack;

      dx := width div 4;
      dy := (height - yoffset) div 4;

      for i := 1 to 3 do
        for j := 1 to 4 do
        begin
          if j + (i - 1) * 4 = mo then
          begin
            brush.color := fselectcolor;
            pen.color := fselectcolor;
            font.color := finverscolor;
            textout((j - 1) * dx + 5, yoffset - 10 + dy * i, SmallCaps(FNameofMonths.GetMonth(j + (i - 1) * 4)));
            brush.color := self.color;
            pen.color := ftextcolor;
            font.color := ftextcolor;
            setbkmode(handle, TRANSPARENT);
          end
          else
            textout((j - 1) * dx + 5, dy * i + yoffset - 10, SmallCaps(FNameofMonths.Getmonth(j + (i - 1) * 4)));
        end;
    end;
  end;
end;

procedure TCalPanel.ToggleDayMode;
var
  da, mo, ye: word;
begin
  daymode := not daymode;
  decodedate(seldate, ye, mo, da);
  self.SetLabel(mo, ye);
  self.repaint;
end;


procedure TCalPanel.SetDate(da, mo, ye: word);
begin
  thedate := encodedate(ye, mo, da);
  seldate := theDate;
  self.setlabel(mo, ye);
  initdate := seldate;
  self.repaint;
end;

procedure TCalPanel.GetDate(var da, mo, ye: word);
begin
  decodedate(seldate, ye, mo, da);
end;

function TCalPanel.DateToXY(dt: tdatetime): tpoint;
var
  ye, mo, da: word;
  tmpdt: tdatetime;
  fd: integer;
  rx, ry: integer;

begin
  decodedate(dt, ye, mo, da);
  tmpdt := encodedate(ye, mo, 1);

  fd := dayofweek(tmpdt) - 1 - startday;

  if fd < 0 then fd := fd + 7;
  if fd > 7 then fd := fd - 7;


  tmpdt := tmpdt - fd; {this is the first day of the calendar}
  fd := round(dt - tmpdt) + 1;

  rx := (fd mod 7);
  ry := (fd div 7) + 1;
  if (rx = 0) then
  begin
    rx := 7;
    dec(ry);
  end;
  result.x := rx;
  result.y := ry;
end;

function TCalPanel.XYToDate(X, Y: integer; change: boolean): tdatetime;
var
  ye, mo, da: word;
  xcal, ycal: integer;
  sda, fd: integer;
  tmpdt: tdatetime;
begin
  xposin := x;
  yposin := y;
  xcal := 0;
  ycal := 0;

  decodedate(seldate, ye, mo, da);

  tmpdt := encodedate(ye, mo, 1);

  fd := dayofweek(tmpdt) - 1 - startday;

  if (fd < 0) then fd := fd + 7;
  if (fd > 7) then fd := fd - 7;

  if (dx > 0) and (dy > 0) then
  begin
    xcal := x div dx;
    ycal := ((y - yoffset) - dy) div dy;
  end;

  if (xcal > 6) then xcal := 6;
  if (ycal > 5) then ycal := 5;

 {
 messagedlg('xcal='+inttostr(xcal)+' ycal='+inttostr(ycal)+' fd='+inttostr(fd),mtinformation,[mbOK],0);
 }

  sda := xcal + 7 * ycal - fd + 1;

  if (sda < 1) then
  begin
    dec(mo);
    if (mo = 0) then
    begin
      mo := 12;
      dec(ye);
    end;
    sda := daysinmonth(mo, ye) + sda;
    if change then changemonth(-1);
  end;
  if (sda > daysinmonth(mo, ye)) then
  begin
    sda := sda - daysinmonth(mo, ye);
    inc(mo);
    if (mo > 12) then
    begin
      mo := 1;
      inc(ye);
    end;
    if change then changemonth(+1);
  end;

  da := sda;
 {
  messagedlg('da = '+inttostr(da),mtinformation,[mbOK],0);
 }
  result := encodedate(ye, mo, da);
end;


procedure TCalPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dt: tdatetime;
  isEvent: boolean;
  newpt: tpoint;
  xofs: Integer;

begin
  if daymode then x := x - xoffset;

  if daymode then xofs := 0 else xofs := xoffset;

  if (x >= lblx1) and (x <= lblx2) and (y > 0) and (y < 15) and fMonthSelect then
  begin
    if flgla = false then
    begin
      flgla := true;
      self.PaintLabel;
    end;
  end
  else
    if flgla then
    begin
      flgla := false;
      self.PaintLabel;
    end;

  if (x > 0) and (x < 15) and (y > 0) and (y < 15) then
  //if (x>5) and (x<15) and (y>0) and (y<15) then
  begin
    if flgl = false then
    begin
      flgl := true;
      self.PaintArrowLeft;
    end;
  end
  else
    if flgl then
    begin
      flgl := false;
      self.PaintArrowLeft;
    end;

  if (x + xofs > width - 15) and (x + xofs < width) and (y > 0) and (y < 15) then
  begin
    if flgr = false then
    begin
      flgr := true;
      self.PaintArrowRight;
    end;
  end
  else
    if flgr then
    begin
      flgr := false;
      self.PaintArrowRight;
    end;

  EventHint := '';
  if daymode and (y > dy + yoffset) and fEventHints then
  begin
    dt := XYToDate(X, Y, false);
    if (dx > 0) and (dy > 0) then
    begin
      newpt.x := x div dx;
      newpt.y := ((y - yoffset) - dy) div dy;
    end;

    if ((newpt.x <> flasthintpos.x) or
      (newpt.y <> flasthintpos.y)) and (showhintbusy) then
    begin
      Application.CancelHint;
      showhintbusy := false;
    end;

    isEvent := false;
    if Assigned(fOnGetDateEventHint) then
      FOnGetDateEventHint(self, dt, isEvent, EventHint);

    FLastHintPos := newpt;
  end;

end;

procedure TCalPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ye, mo, da, omo, nmo: word;
  ycal: integer;
  fd: integer;
  xofs: integer;
  origdate: tdatetime;
  r: trect;
  pt: tpoint;

begin
  if (Button <> mbLeft) then exit;
  origdate := seldate;
  xposin := $7FFF;
  yposin := $7FFF;

  if not (getfocus = self.handle) then self.setfocus;

  if daymode then x := x - xoffset;
  if daymode then xofs := 0 else xofs := xoffset;

  if (x >= lblx1) and (x <= lblx2) and (y > 0) and (y < 15) and fMonthSelect then
  begin
    Self.toggleDayMode;
    if assigned(FOnMonthSelect) then FonMonthSelect(self);
    exit;
  end;

  if (x > 0) and (x < 15) and (y > 0) and (y < 15) then
 //if (x>5) and (x<15) and (y>0) and (y<15) then
  begin
    if daymode then self.ChangeMonth(-1) else self.ChangeYear(-1);
    exit;
  end;

  if (x + xoffset > width - 15 + xofs) and (x + xoffset < width + xofs) and (y > 0) and (y < 15) then
  begin
    if daymode then self.ChangeMonth(1) else self.ChangeYear(1);
    exit;
  end;

  if daymode then
  begin
    if (y > dy + yoffset) then
    begin
      seldate := XYToDate(X - 3, Y, true);
      thedate := seldate;
      decodedate(seldate, ye, nmo, da);
      decodedate(origdate, ye, omo, da);
      if omo = nmo then
      begin
        pt := datetoxy(origdate);
        if pt.x = 0 then pt.x := 7;
        r.top := yoffset + (pt.y) * dy - 2;
        r.bottom := r.top + dy;
        r.left := xoffset + (pt.x - 1) * dx;
        r.right := r.left + dx + 2;
        invalidaterect(self.handle, @r, TRUE);
        pt := datetoxy(thedate);
        if pt.x = 0 then pt.x := 7;
        r.top := yoffset + (pt.y) * dy - 2;
        r.bottom := r.top + dy;
        r.left := xoffset + (pt.x - 1) * dx;
        r.right := r.left + dx + 2;
        invalidaterect(self.handle, @r, TRUE);
      end;
//     else
      self.Repaint;
    end;
  end
  else
  begin
    with self do
    begin
      if (y > yoffset + 12) then
      {
      if (y+10>((height-yoffset) div 4)) then
      }
      begin
        decodedate(seldate, ye, mo, da);

        fd := 1 + (x div (width div 4));
        ycal := 4 * ((y - yoffset + 10) div ((height - yoffset) div 4) - 1);
        if ycal < 0 then ycal := 0;
        fd := fd + ycal;

        mo := fd;
        if (mo > 12) then mo := mo - 4;

        seldate := encodedate(ye, mo, da);

        thedate := seldate;

        daymode := true;

        self.SetLabel(mo, ye);
        self.repaint;
      end;
    end;
  end;

end;

procedure TCalPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    seldate := initdate;
     {
     self.close;
     }
  end;

  if daymode then x := x - xoffset;

  if (abs(x - xposin) < 4) and (abs(y - yposin) < 4) or FYearChanged then
  begin
    FYearChanged := false;
    if assigned(FOnDaySelect) then FOnDaySelect(self, seldate);
    {
    self.modalresult:=mrOK;
    }
  end;
end;

procedure TCalPanel.Paint;
begin
  caption := '';
  inherited Paint;
  self.PaintProc;
end;

procedure TCalPanel.KeyPress(var Key: char);
begin
  if (key = #32) then
  begin
    if fMonthSelect then self.toggledaymode;
  end;
  if (key = #27) then
  begin
    seldate := initdate;
    {
    self.close;
    }
  end;
  if (key = #13) then
  begin
    {
    self.modalresult:=mrOK;
    }
  end;
end;

procedure TCalPanel.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if msg.CharCode in [vk_up, vk_down, vk_left, vk_right] then msg.result := 1;
end;

procedure TCalPanel.WMKeyDown(var Msg: TWMKeydown);
var
  da, nmo, omo, ye: word;
  origdate: tdatetime;
  pt: tpoint;
  r: trect;

begin
  origdate := SelDate;
  pt := DateToXY(seldate);

  if daymode then
  begin
    decodedate(thedate, ye, omo, da);
    case msg.charcode of
      vk_left: thedate := thedate - 1;
      vk_right: thedate := thedate + 1;
      vk_up: thedate := thedate - 7;
      vk_down: thedate := thedate + 7;
    end;
    if msg.charcode in [vk_up, vk_down, vk_left, vk_right] then
    begin
      seldate := thedate;
      decodedate(thedate, ye, nmo, da);
      self.setlabel(nmo, ye);
      if omo = nmo then
      begin
        pt := datetoxy(origdate);
        if pt.x = 0 then pt.x := 7;
        r.top := yoffset + (pt.y) * dy - 2;
        r.bottom := r.top + dy;
        r.left := xoffset + (pt.x - 1) * dx;
        r.right := r.left + dx + 2;
        invalidaterect(self.handle, @r, TRUE);
        pt := datetoxy(thedate);
        if pt.x = 0 then pt.x := 7;
        r.top := yoffset + (pt.y) * dy - 2;
        r.bottom := r.top + dy;
        r.left := xoffset + (pt.x - 1) * dx;
        r.right := r.left + dx + 2;
        invalidaterect(self.handle, @r, TRUE);
      end
      else
        self.Repaint;
    end;
  end;

  if (msg.charcode = vk_prior) then
  begin
    self.changemonth(-1);
  end;
  if (msg.charcode = vk_next) then
  begin
    self.changemonth(+1);
  end;

  if msg.charcode in [vk_right, vk_up, vk_down, vk_left, vk_prior, vk_next] then
  begin
    if assigned(fOnDateChange) then
      fOnDateChange(self, origdate, seldate);
  end;

  if msg.charcode in [vk_prior, vk_next] then
  begin
    if assigned(fOnMonthChange) then
      fOnMonthChange(self, origdate, seldate);
  end;

  if msg.charcode in [vk_up, vk_left, vk_right, vk_down, vk_next, vk_prior] then msg.result := 0;
end;

function TCalPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCalPanel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCalPanel.SetVersion(const Value: string);
begin

end;

constructor TNameofDays.Create;
begin
  inherited Create;
  {$IFDEF DELPHIXE_LVL}
  FSunday := FormatSettings.ShortDayNames[1];
  FMonday := FormatSettings.ShortDayNames[2];
  FTuesday := FormatSettings.ShortDayNames[3];
  FWednesday := FormatSettings.ShortDayNames[4];
  FThursday := FormatSettings.ShortDayNames[5];
  FFriday := FormatSettings.ShortDayNames[6];
  FSaturday := FormatSettings.ShortDayNames[7];
  {$ENDIF}

  {$IFNDEF DELPHIXE_LVL}
  FSunday := ShortDayNames[1];
  FMonday := ShortDayNames[2];
  FTuesday := ShortDayNames[3];
  FWednesday := ShortDayNames[4];
  FThursday := ShortDayNames[5];
  FFriday := ShortDayNames[6];
  FSaturday := ShortDayNames[7];
  {$ENDIF}
end;

destructor TNameofDays.Destroy;
begin
  inherited Destroy;
end;

function TNameofDays.GetDay(i: integer): string;
begin
  case i of
    1, 8: result := fmonday;
    2, 9: result := ftuesday;
    3, 10: result := fwednesday;
    4, 11: result := fthursday;
    5, 12: result := ffriday;
    6, 13: result := fsaturday;
    7, 14: result := fsunday;
  else
    result := '';
  end;
end;

constructor TNameofMonths.Create;
begin
  inherited Create;
  {$IFDEF DELPHIXE_LVL}
  FJanuary := FormatSettings.Shortmonthnames[1];
  FFebruary := FormatSettings.Shortmonthnames[2];
  FMarch := FormatSettings.Shortmonthnames[3];
  FApril := FormatSettings.Shortmonthnames[4];
  FMay := FormatSettings.Shortmonthnames[5];
  FJune := FormatSettings.Shortmonthnames[6];
  FJuly := FormatSettings.Shortmonthnames[7];
  FAugust := FormatSettings.Shortmonthnames[8];
  FSeptember := FormatSettings.Shortmonthnames[9];
  FOctober := FormatSettings.Shortmonthnames[10];
  FNovember := FormatSettings.Shortmonthnames[11];
  FDecember := FormatSettings.Shortmonthnames[12];
  {$ENDIF}

  {$IFNDEF DELPHIXE_LVL}
  FJanuary := Shortmonthnames[1];
  FFebruary := Shortmonthnames[2];
  FMarch := Shortmonthnames[3];
  FApril := Shortmonthnames[4];
  FMay := Shortmonthnames[5];
  FJune := Shortmonthnames[6];
  FJuly := Shortmonthnames[7];
  FAugust := Shortmonthnames[8];
  FSeptember := Shortmonthnames[9];
  FOctober := Shortmonthnames[10];
  FNovember := Shortmonthnames[11];
  FDecember := Shortmonthnames[12];
  {$ENDIF}
end;

destructor TNameofMonths.Destroy;
begin
  inherited Destroy;
end;

function TNameofMonths.GetMonth(i: integer): string;
begin
  case i of
    1: result := FJanuary;
    2: result := FFebruary;
    3: result := FMarch;
    4: result := FApril;
    5: result := FMay;
    6: result := FJune;
    7: result := FJuly;
    8: result := FAugust;
    9: result := FSeptember;
    10: result := FOctober;
    11: result := FNovember;
    12: result := FDecember;
  else
    result := '';
  end;
end;

constructor TYearStartAt.Create;
begin
  inherited Create;
  FStartDay := 1;
  FStartMonth := 1;
end;

destructor TYearStartAt.Destroy;
begin
  inherited Destroy;
end;

procedure TYearStartAt.SetStartDay(d: integer);
begin
  if (d <= 0) or (d > 31) then
  begin
    messagedlg('Invalid day. Should be in [1..31]', mtError, [mbOK], 0);
    exit;
  end;
  FStartDay := d;
end;

procedure TYearStartAt.SetStartMonth(m: integer);
begin
  if (m <= 0) or (m > 12) then
  begin
    messagedlg('Invalid month. Should be in [1..12]', mtError, [mbOK], 0);
    exit;
  end;
  FStartMonth := m;
end;

function TCalPanel.GetDateProc: tDatetime;
begin
  result := seldate;
end;

procedure TCalPanel.SetDateProc(const Value: tDatetime);
begin
  decodedate(value, fYear, fMonth, fDay);
  setdate(fDay, fMonth, fYear);
end;

procedure TCalPanel.CMHintShow(var Msg: TMessage);
type
  PHintInfo = ^THintInfo;
var
  hi: PHintInfo;
  CanShow: Boolean;

begin
  hi := PHintInfo(Msg.LParam);
  ShowHintProc(hi^.HintStr, CanShow, hi^);
  if not CanShow then
    hi^.HintStr := '';
  inherited;
end;

end.
