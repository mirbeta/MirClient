{***************************************************************************}
{ TCALCOMP component                                                        }
{ for Delphi  & C++Builder                                                  }
{                                                                           }
{ written by TMS Software                                                   }
{          copyright © 1998-2012                                            }
{          Email : info@tmssoftware.com                                     }
{          Website : http://www.tmssoftware.com                             }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$IFDEF VER110}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125}
 {$ObjExportAll On}
{$ENDIF}

{$I TMSDEFS.INC}

unit CalComp;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Moneycal
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
   TShowMethod  = (smTop,smBottom,smDependent);
   TStartDay = (sdSunday, sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday);

type
  TYearStartAt = class(TPersistent)
  private
    FStartDay:integer;
    FStartMonth:integer;
    procedure SetStartDay(d:integer);
    procedure SetStartMonth(m:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property StartDay: integer read FStartDay write SetStartDay;
    property StartMonth: integer read FStartMonth write SetStartMonth;
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
    procedure Assign(Source: TPersistent); override;
  published
    property Monday: TDayStr read FMonday write FMonday;
    property Tuesday: TDayStr read FTuesday write FTuesday;
    property Wednesday: TDayStr read FWednesday write FWednesday;
    property Thursday: TDayStr read FThursday write FThursday;
    property Friday: TDayStr read FFriday write FFriday;
    property Saturday: TDayStr read FSaturday write FSaturday;
    property Sunday: TDayStr read FSunday write FSunday;
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
    procedure Assign(Source: TPersistent); override;
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

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCalComp = class(TComponent)
  private
    CalForm:TCalForm;
    FDay,FMonth,FYear:word;
    FTop,FLeft:word;
    FAlignControl:TWinControl;
    FShowMethod:tShowMethod;
    {
    FStartofWeek:word;
    }
    FFont: TFont;
    FColor: TColor;
    FTextColor: TColor;
    FSelectColor: TColor;
    FInversColor: TColor;
    FWeekendColor: TColor;
    FNameofDays: TNameofDays;
    FNameofMonths: TNameofMonths;
    FShowWeeks: boolean;
    FFirstday: TStartDay;
    FYearStartAt: TYearStartAt;
    FSelDateFrom: TDatetime;
    FSelDateTo: TDatetime;
    function GetWeek: word;
    {
    procedure SetStartofWeek(aday:word);
    }
    function GetSelDate:tdatetime;
    procedure SetSelDate(adate:tdatetime);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetNameOfDays(const ANameOfDays: TNameOfDays);
    procedure SetNameOfMonths(const ANameOfMonths: TNameOfMonths);
    procedure SetYearStartAt(const AYearStartAt: TYearStartAt);
    procedure SetAlignControl(AControl:TWincontrol);
    procedure SetFont(const value: TFont);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
//    procedure Notification(AComponent:TComponent;Operation:TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property Week:word read GetWeek;
    property SelDate :TDatetime read GetSelDate write SetSelDate;
    property SelDateFrom:TDatetime read FSelDateFrom write FSelDateFrom;
    property SelDateTo:tdatetime read FSelDateTo write FSelDateTo;
  published
    property AlignControl:TWinControl read FAlignControl write SetAlignControl;
    property Day: word read FDay write FDay default 1;
    property Month: word read FMonth write FMonth default 1;
    property Year: word read FYear write FYear default 1;
    property CalTop:word read FTop write FTop;
    property CalLeft:word read FLeft write FLeft;
    property Color: TColor read FColor write FColor;
    property Font:TFont read FFont write SetFont;
    property TextColor: TColor read FTextColor write FTextColor;
    property SelectColor: TColor read FSelectColor write FSelectColor;
    property InversColor: TColor read FInversColor write FInversColor;
    property NameofDays: TNameofDays read FNameofDays write SetNameofDays;
    property NameofMonths: TNameofMonths read FNameofMonths write SetNameofMonths;

    property WeekendColor: TColor read FWeekendColor write FWeekendColor;
    {
    property Startofweek:word read fStartofWeek write fStartofweek;
    }
    property ShowMethod:TShowMethod read FShowMethod write FShowMethod;
    property ShowWeeks: boolean read FShowWeeks write FShowWeeks;
    property Firstday:TStartDay read FFirstDay write FFirstDay;
    property Version: string read GetVersion write SetVersion;
    property YearStartAt:TYearStartAt read FYearStartAt write SetYearStartAt;
  end;

implementation

procedure TYearStartAt.Assign(Source: TPersistent);
begin
  if (Source is TYearStartAt) then
  begin
    FStartDay := (Source as TYearStartAt).StartDay;
    FStartMonth := (Source as TYearStartAt).StartMonth;
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

procedure TYearStartAt.SetStartDay(d:integer);
begin
  if (d<=0) or (d>31) then
  begin
    MessageDlg('Invalid day. Should be in [1..31]',mtError,[mbOK],0);
    Exit;
  end;
  FStartDay := d;
end;

procedure TYearStartAt.SetStartMonth(m:integer);
begin
  if (m<=0) or (m>12) then
  begin
    Messagedlg('Invalid month. Should be in [1..12]',mtError,[mbOK],0);
    exit;
  end;
  FStartMonth:=m;
end;

procedure TNameofDays.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TNameOfDays) then
  begin
    FMonday := (Source as TNameOfDays).Monday;
    FTuesday := (Source as TNameOfDays).TuesDay;
    FWednesday := (Source as TNameOfDays).WednesDay;
    FThursday := (Source as TNameOfDays).Thursday;
    FFriday  := (Source as TNameOfDays).Friday;
    FSaturday := (Source as TNameOfDays).Saturday;
    FSunday := (Source as TNameOfDays).Sunday;
  end;
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

procedure TNameOfMonths.Assign(Source: TPersistent);
begin
  if (Source is TNameOfMonths) then
  begin
    FJanuary := (Source as TNameOfMonths).January;
    FFebruary := (Source as TNameOfMonths).February;
    FMarch := (Source as TNameOfMonths).March;
    FApril := (Source as TNameOfMonths).April;
    FMay := (Source as TNameOfMonths).May;
    FJune := (Source as TNameOfMonths).June;
    FJuly := (Source as TNameOfMonths).July;
    FAugust := (Source as TNameOfMonths).August;
    FSeptember := (Source as TNameOfMonths).September;
    FNovember := (Source as TNameOfMonths).November;
    FDecember := (Source as TNameOfMonths).December;
  end;
end;

constructor TCalComp.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);

  FNameofDays := TNameofDays.Create;
  FNameofMonths := TNameofMonths.Create;
  FYearStartAt := TYearStartAt.Create;
  FFont := TFont.Create;
{
  DecodeDate(now,fyear,fmonth,fday);
  FFirstDay := sdSunday;
  FColor := clBtnFace;
  FTextColor := clBlack;
  FInversColor := clWhite;
  FSelectColor := clHighlight;
  FWeekendColor := clBlack;
  FSelDateFrom := -1;
  FSelDateTo := -1;
}
end;

destructor TCalComp.Destroy;
begin
  FNameofDays.Free;
  FNameofMonths.Free;
  FYearStartAt.Free;
  FFont.Free;
  inherited;
end;

{
procedure TCalComp.Setstartofweek(aday:TStartDay);
begin
 fStartofweek:=aday;
 if (fStartOfWeek<1) then fStartofweek:=1;
 if (fStartOfWeek>7) then fStartofweek:=7;
end;
}

procedure TCalComp.SetAlignControl(AControl:TWinControl);
begin
  FAlignControl := AControl;
end;

procedure TCalcomp.Notification(AComponent:TComponent;AOperation:TOperation);
begin
  if (AOperation = opRemove) then
  begin
    if (AComponent = FAlignControl) then
      FAlignControl := nil;
  end;

  inherited;
end;

function TCalComp.GetWeek:word;
var
  days1,days2:real;
  d1,d2:tdatetime;
  firstday:integer;

begin
  d2:=encodedate(fyear,fmonth,fday);
  d1:=encodedate(fyear,1,1);
  firstday:=dayofweek(d1);
  days2:=int(d2);
  days1:=int(d1);
  days1:=(days2-days1)+(firstday-2);
  GetWeek:=(trunc(days1) div 7)+1;
end;

function TCalComp.GetSelDate:tdatetime;
begin
  Result := EncodeDate(Fyear,FMonth,FDay);
end;

procedure TCalComp.SetFont(const value: TFont);
begin
  FFont.Assign(value);
end;

procedure TCalComp.SetSelDate(ADate:TDateTime);
begin
  Decodedate(ADate,Fyear,FMonth,FDay);
end;

function TCalComp.Execute: Boolean;
var
  pt:TPoint;
  d: TDayArray;
  m: TMonthArray;
begin
  { Create dialog in memory }
  CalForm := TCalForm.Create(Application);
  CalForm.Font.Assign(Font);

  { Set dialog strings }
  with FNameofDays do
  begin
    d[1]:=fmonday;
    d[2]:=ftuesday;
    d[3]:=fwednesday;
    d[4]:=fthursday;
    d[5]:=ffriday;
    d[6]:=fsaturday;
    d[7]:=fsunday;
    CalForm.SetNameofDays(d);
  end;
  with FNameofMonths do
  begin
    m[1]:=FJanuary;
    m[2]:=FFebruary;
    m[3]:=FMarch;
    m[4]:=FApril;
    m[5]:=FMay;
    m[6]:=FJune;
    m[7]:=FJuly;
    m[8]:=FAugust;
    m[9]:=FSeptember;
    m[10]:=FOctober;
    m[11]:=FNovember;
    m[12]:=FDecember;
    CalForm.SetNameofMonths(m);
  end;

  CalForm.SetDate(fday,fmonth,fyear);

  case fFirstday of
  sdMonday:CalForm.SetStartDay(1);
  sdTuesday:CalForm.SetStartDay(2);
  sdWednesday:CalForm.SetStartDay(3);
  sdThursday:CalForm.SetStartDay(4);
  sdFriday:CalForm.SetStartDay(5);
  sdSaturday:CalForm.SetStartDay(6);
  sdSunday:CalForm.SetStartDay(7);
  end;

  CalForm.SetColors(fTextColor,fSelectColor,fInversColor,fWeekendcolor);
  CalForm.Color:=FColor;
  CalForm.SetWeeks(FShowWeeks);
  CalForm.SetStarts(FYearStartAt.FStartDay,FYearStartAt.FStartMonth);
  CalForm.FromDate:=FSelDateFrom;
  CalForm.ToDate:=FSelDateTo;

  {to do : choose align up or down ... }
  if (FAlignControl<>nil) then
  begin
    pt.x:=Faligncontrol.left;
    pt.y:=FAlignControl.top;
    clienttoscreen(FAlignControl.Parent.handle,pt);
    CalForm.Left:=pt.x;

    case FShowMethod of
      smBottom:CalForm.Top:=pt.y+FAlignControl.Height;
      smTop:CalForm.Top:=pt.y-CalForm.height;
      smDependent:
        begin
          if (pt.y+FAlignControl.height+Calform.height>getsystemmetrics(SM_CYSCREEN)) then
            CalForm.top:=pt.y-CalForm.height
          else
            CalForm.top:=pt.y+FAlignControl.Height;
        end;
     end;
     {end of case}
  end
  else
  begin
    CalForm.top:=FTop;
    CalForm.left:=FLeft;
  end;

  try
    Execute := (CalForm.ShowModal=mrOK);
    CalForm.GetDate(fday,fmonth,fyear);
  finally
    CalForm.Free;
  end;
end;

function TCalComp.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCalComp.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCalComp.SetVersion(const Value: string);
begin

end;

procedure TCalComp.SetYearStartAt(const AYearStartAt: TYearStartAt);
begin
  FYearStartAt.Assign(AYearStartAt);
end;

procedure TCalComp.SetNameOfDays(const ANameOfDays: TNameOfDays);
begin
  FNameOfDays.Assign(ANameOfDays);
end;

procedure TCalComp.SetNameOfMonths(const ANameOfMonths: TNameOfMonths);
begin
  FNameOfMonths.Assign(ANameOfMonths);
end;

end.

