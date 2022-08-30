{*********************************************************************}
{ TMonthCalendar                                                      }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2005 - 2011                                            }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit MonthCal;

{$IFDEF VER120}
  CANNOT BE INSTALLED ON DELPHI 4 : ALREADY EXISTS IN THE WIN32 TAB
{$ENDIF}

{$IFDEF VER130}
  CANNOT BE INSTALLED ON DELPHI 4 : ALREADY EXISTS IN THE WIN32 TAB
{$ENDIF}


{$IFDEF VER110}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125}
 {$ObjExportAll On}
{$ENDIF}


interface



{$DEFINE noDEBUG}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  commctrl, comctrls, stdctrls {$IFDEF DEBUG}, dbugintf {$ENDIF};

  {
  MONTHCAL_CLASS = 'SysMonthCal32';
  MCS_DAYSTATE = $0001;
  MCS_MULTISELECT = $0002;
  MCS_WEEKNUMBERS = $0004;
  MCS_NOTODAY = $0010;
  }

const
  MAX_MONTHS=12;

  MCS_NOTODAYCIRCLE = $0010;

  MCN_SELCHANGE     = (MCN_FIRST + 1);
  MCN_GETDAYSTATE   = (MCN_FIRST + 3);
  MCN_SELECT        = (MCN_FIRST + 4);


type
  PMONTHDAYSTATE = ^integer;

  PNMDayState = ^TNMDayState;
  TNMDayState = record
                 nmhdr:TNmHdr;
                 st:TSystemTime;
                 cDayState:integer;
                 prgDayState:PMONTHDAYSTATE; // pointer to cDayState daystates
                end;

  PNMSELCHANGE = ^TNMSELCHANGE;
  TNMSELCHANGE = record
                  nmhdr:TNmHdr;
                  stselstart:TSystemTime;
                  stselend:TSystemTime;
                 end;

  EMonthCalendarError = class(Exception);

  TNMSelChangeEvent = procedure(Sender:TObject;dt1,dt2:tdatetime) of object;
  TNMSelectEvent = procedure(Sender:TObject;dt1,dt2:tdatetime) of object;

  TNMSetDayStateEvent = procedure(Sender:TObject;dt:tdatetime;var daystates:integer) of object;

  TMonthCalendar = class;

  TMonthCalendarColors = class(TPersistent)
  private
    Owner: TMonthCalendar;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FMonthBackColor: TColor;
    FTrailingTextColor: TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    procedure SetAllColors;
  public
    constructor Create(AOwner: TMonthCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read FMonthBackColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read FTrailingTextColor
      write SetColor default clInactiveCaptionText;
  end;

  TMonthCalendar = class(TWinControl)
  private
    { Private declarations }
   FCalColors: TMonthCalendarColors;
   FNotoday:boolean;
   FNotodayCircle:boolean;
   FMultiSelect:boolean;
   FWeekNumbers:boolean;
   FDate:tDate;
   FMaxDate: TDate;
   FMinDate: TDate;
   FMaxSelCount:integer;
   FVisible: boolean;
   FOnSelChange:TNMSelChangeEvent;
   FOnSelect:TNMSelectEvent;
   FOnSetDayState:TNMSetDayStateEvent;
   procedure SetCalColors(Value: TMonthCalendarColors);
   procedure SetNotoday(avalue:boolean);
   procedure SetNotodayCircle(avalue:boolean);
   procedure SetMultiSelect(avalue:boolean);
   procedure SetWeekNumbers(avalue:boolean);
   procedure SetDate(avalue:TDate);
   procedure SetMinDate(avalue:TDate);
   procedure SetMaxDate(avalue:TDate);
   procedure SetMaxSelCount(avalue:integer);
   procedure SetVisible(avalue:boolean);
   function GetMaxSelCount:integer;
   procedure SetFirstDay(avalue:integer);
   function GetFirstDay:integer;
   procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;

  protected
   procedure CreateParams(var Params: TCreateParams); override;
   procedure CreateWnd; override;
   procedure Loaded; override;
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    { Protected declarations }
  public
   constructor Create(aOwner:tComponent); override;
   destructor Destroy; override;
   procedure UpdateDayState;
    { Public declarations }
  published
   property CalColors: TMonthCalendarColors read FCalColors write SetCalColors;
    { Published declarations }
   property Notoday:boolean read FNotoday write SetNotoday;
   property NotodayCircle:boolean read FNotodayCircle write SetNotodayCircle;
   property MultiSelect:boolean read FMultiselect write SetMultiselect;
   property WeekNumbers:boolean read FWeekNumbers write Setweeknumbers;
   property Date:tDate read FDate write SetDate;
   property MaxDate:tDate read FMaxDate write SetMaxDate;
   property MinDate:tDate read FMinDate write SetMinDate;
   property OnSelChange:TNMSelChangeEvent read FOnSelChange write FOnSelChange;
   property OnSelect:TNMSelectEvent read FOnSelect write FOnSelect;
   property OnSetDayState:TNMSetDayStateEvent read FOnSetDayState write FOnSetDayState;
   property MaxSelCount:integer read fMaxSelCount write SetMaxSelCount;
   property FirstDayOfWeek:integer read GetFirstDay write SetFirstDay;
   property TabStop default True;
   property Visible:boolean read FVisible write SetVisible default true;
  end;

procedure Register;

implementation

function GetFileVersion(filename:string):integer;
var
 filehandle:DWORD;
 l:integer;
 pvs:PVSFixedFileInfo;
 lptr:uint;
 querybuf:array[0..255] of char;
 buf:pchar;
begin
 result:=-1;

 strpcopy(querybuf,filename);
 l:=getfileversioninfosize(querybuf,filehandle);
 if (l>0) then
  begin
   getmem(buf,l);
   getfileversioninfo(querybuf,filehandle,l,buf);
   if verqueryvalue(buf,'\',pointer(pvs),lptr) then
    begin
     if (pvs^.dwSignature=$FEEF04BD) then
      begin
       result:=pvs^.dwFileVersionMS;
      end;
    end;
   freemem(buf);
  end;
end;

constructor TMonthCalendarColors.Create(AOwner:TMonthCalendar);
begin
  Owner := AOwner;
  FBackColor := clWindow;
  FTextColor := clWindowText;
  FTitleBackColor := clActiveCaption;
  FTitleTextColor := clWhite;
  FMonthBackColor := clWhite;
  FTrailingTextColor := clInactiveCaptionText;
end;

procedure TMonthCalendarColors.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then SourceName := 'nil'
  else SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TMonthCalendarColors) then
    raise EConvertError.CreateFmt('Assign error', [SourceName, ClassName]);
  FBackColor := TMonthCalendarColors(Source).BackColor;
  FTextColor := TMonthCalendarColors(Source).TextColor;
  FTitleBackColor := TMonthCalendarColors(Source).TitleBackColor;
  FTitleTextColor := TMonthCalendarColors(Source).TitleTextColor;
  FMonthBackColor := TMonthCalendarColors(Source).MonthBackColor;
  FTrailingTextColor := TMonthCalendarColors(Source).TrailingTextColor;
end;

const
  ColorIndex: array[0..5] of Integer = (MCSC_BACKGROUND, MCSC_TEXT,
    MCSC_TITLEBK, MCSC_TITLETEXT, MCSC_MONTHBK, MCSC_TRAILINGTEXT);

procedure TMonthCalendarColors.SetColor(Index: Integer; Value: TColor);
begin
  MonthCal_SetColor(Owner.Handle, ColorIndex[Index], ColorToRGB(Value));
  case Index of
    0: FBackColor := Value;
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FMonthBackColor := Value;
    5: FTrailingTextColor := Value;
  end;
 {$IFDEF DEBUG}
 senddebug('in setcolor');
 {$ENDIF}
end;

procedure TMonthCalendarColors.SetAllColors;
begin
  SetColor(0, FBackColor);
  SetColor(1, FTextColor);
  SetColor(2, FTitleBackColor);
  SetColor(3, FTitleTextColor);
  SetColor(4, FMonthBackColor);
  SetColor(5, FTrailingTextColor);
end;

procedure TMonthCalendar.SetCalColors(Value: TMonthCalendarColors);
begin
 if FCalColors<>Value then FCalColors.Assign(Value);
end;

constructor TMonthCalendar.Create(AOwner: TComponent);
begin
 {check for commctl32.dll version}
  if (getfileversion('COMCTL32.DLL')<$00040046) then
    begin
     raise EMonthCalendarError.Create('COMCTL32.DLL version 4.70 or higher required');
    end;

  CheckCommonControl(ICC_DATE_CLASSES);

  {
  MessageDlg('in create',mtWarning,[mbOK],0);
  }

  FCalColors := TMonthCalendarColors.Create(Self);

  FVisible:=true;
  FDate := int(Now);

  {
  FShowCheckbox := False;
  FChecked := True;
  }

  inherited Create(AOwner);
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csFixedHeight];

  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := 186;
  Height:=186;
  {
  AdjustHeight;
  }
end;

destructor TMonthCalendar.Destroy;
begin
  FCalColors.Free;
  inherited Destroy;
end;

procedure TMonthCalendar.CreateParams(var Params: TCreateParams);
const
  Formats: array[TDTDateFormat] of Integer = (DTS_SHORTDATEFORMAT,
    DTS_LONGDATEFORMAT);
begin
  inherited CreateParams(Params);

  CreateSubClass(Params,MONTHCAL_CLASS);

  {
  MessageDlg('in createparams',mtWarning,[mbOK],0);
  }

  with Params do
  begin
    {$IFDEF DEBUG}
    senddebug('creating control here');
    {$ENDIF}
    {
    Style := Style or Formats[FDateFormat];
    if FDateMode = dmUpDown then Style := Style or DTS_UPDOWN;
    if FKind = dtkTime then Style := Style or DTS_TIMEFORMAT;
    if FCalAlignment = dtaRight then Style := Style or DTS_RIGHTALIGN;
    if FParseInput then Style := Style or DTS_APPCANPARSE;
    if FShowCheckbox then Style := Style or DTS_SHOWNONE;
    style:=0;
    }

    if FNotodayCircle then style:=style or MCS_NOTODAY;
    if FNotoday then style:=style or MCS_NOTODAYCIRCLE;
    if FMultiSelect then style:=style or MCS_MULTISELECT;
    if FWeekNumbers then style:=style or MCS_WEEKNUMBERS;

    style:=style or WS_BORDER or WS_CHILD or WS_VISIBLE or MCS_DAYSTATE;

    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW) or
      CS_DBLCLKS;
  end;
end;

procedure TMonthCalendar.CreateWnd;
var
 prc:trect;
begin
  inherited CreateWnd;

  {
  MessageDlg('in createwnd',mtWarning,[mbOK],0);
  }

  MonthCal_GetMinReqRect(self.handle,prc);
  self.width:=prc.right-prc.left;
  self.height:=prc.bottom-prc.top;
  setwindowpos(self.handle, 0,self.left,self.top,self.width,self.height,SWP_SHOWWINDOW);
  FCalColors.SetAllColors;

  {
  if multiselect then SetMaxSe
  lCount(fMaxSelCount);
  }
  {$IFDEF DEBUG}
  senddebug('getting maxsel here '+inttostr(GetMaxSelCount));
  {$ENDIF}

  self.repaint;

  {
  SetMonthCalendar(FMonthCalendar);
  SetChecked(FChecked);
  }
end;

procedure TMonthCalendar.UpdateDayState;
var
 mds:array[1..MAX_MONTHS] of integer;
 i,j:integer;
 ye,mo,da:word;
 dt:tdatetime;

begin
 fillchar(mds,sizeof(mds),0);

 if assigned(OnSetDayState) then
  begin
   decodedate(self.date,ye,mo,da);
   da:=1;
   dec(mo);
   if (mo=0) then
    begin
     dec(ye);
     mo:=12;
    end;

   for i:=1 to 3 do
    begin
     dt:=encodedate(ye,mo,da);
     j:=0;
     OnSetDayState(self,dt,j);
     mds[i]:=j;
     decodedate(dt,ye,mo,da);
     inc(mo);
     if (mo=13) then
      begin
       mo:=1;
       inc(ye);
      end;
    end;
  end;

  Sendmessage(Handle,MCM_SETDAYSTATE,3,LParam(@mds));
end;

procedure TMonthCalendar.Loaded;

begin
 inherited Loaded;
 {$IFDEF DEBUG}
 senddebug('maxselcount='+inttostr(fMaxSelCount));
 if MultiSelect then senddebug('is multiselect');
 {$ENDIF}
 self.setMaxSelCount(fMaxSelCount);
 {$IFDEF DEBUG}
 senddebug('retrieved maxselcount = '+inttostr(getmaxselcount));
 {$ENDIF}

 self.UpdateDayState;
 self.repaint;
end;

procedure TMonthCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited;
end;

procedure TMonthCalendar.CNNotify(var Message: TWMNotify);
var
 mds:array[1..MAX_MONTHS] of integer;
 i,j:integer;
 dt1,dt2:tdatetime;
 st:tsystemtime;

 function IsBlankSysTime(ST: TSystemTime): Boolean;
 begin
  with ST do
    Result := (wYear = 0) and (wMonth = 0) and (wDayOfWeek = 0) and
      (wDay = 0) and (wHour = 0) and (wMinute = 0) and (wSecond = 0) and
      (wMilliseconds = 0);
 end;

begin
 with Message.NMHdr^, Message do
 begin
  case code of
  MCN_GETDAYSTATE:begin
                   st:=PNMDayState(NMHdr)^.st;
                   for i:=1 to PNMDayState(NMHdr)^.cDayState do
                    begin
                     mds[i]:=0;
                     if assigned(OnSetDayState) then
                      begin
                        dt1:=encodedate(st.wYear,st.wMonth,st.wDay);
                        j:=0;
                        OnSetDayState(self,dt1,j);
                        mds[i]:=j;
                        PNMDayState(NMHdr)^.prgDayState:=@mds;
                        inc(st.wmonth);
                        if (st.wmonth=13) then
                         begin
                          inc(st.wyear);
                          st.wmonth:=1;
                         end;
                      end;
                    end;


                    PNMDayState(NMHdr)^.prgDayState:=@mds;


                  end;
  MCN_SELCHANGE:if not IsBlankSysTime(PNMSelChange(NMHdr)^.stselstart) then
                 begin
                  st:=PNMSelChange(NMHdr)^.stselstart;
                  dt1:=encodedate(st.wYear,st.wMonth,st.wDay);

                  if not IsBlankSysTime(PNMSelChange(NMHdr)^.stselend) then
                   begin
                    st:=PNMSelChange(NMHdr)^.stselend;
                    dt2:=encodedate(st.wYear,st.wMonth,st.wDay);
                   end
                  else dt2:=dt1;

                 if assigned(OnSelChange) then OnSelChange(self,dt1,dt2);
              end;
  MCN_SELECT:if not IsBlankSysTime(PNMSelChange(NMHdr)^.stselstart) then
              begin
               st:=PNMSelChange(NMHdr)^.stselstart;
               dt1:=encodedate(st.wYear,st.wMonth,st.wDay);
               fDate:=dt1;
               if not IsBlankSysTime(PNMSelChange(NMHdr)^.stselend) then
                begin
                 st:=PNMSelChange(NMHdr)^.stselend;
                 dt2:=encodedate(st.wYear,st.wMonth,st.wDay);
                end
               else dt2:=dt1;
               if assigned(OnSelect) then OnSelect(self,dt1,dt2);
              end;
  end;
 end;
end;

procedure TMonthCalendar.SetNotoday(avalue:boolean);
var
 l:longint;
begin
 if FNotoday<>avalue then
  begin
   FNotoday:=avalue;
   l:=getwindowlong(self.handle,GWL_STYLE);
   if avalue then l:=l or MCS_NOTODAYCIRCLE else l:=l and not MCS_NOTODAYCIRCLE;
   setwindowlong(self.handle,GWL_STYLE,l);
  end;
end;

procedure TMonthCalendar.SetNotodayCircle(avalue:boolean);
var
 l:longint;
begin
 if FNotodayCircle<>avalue then
   begin
    FNotodayCircle:=avalue;
    l:=getwindowlong(self.handle,GWL_STYLE);
    if avalue then l:=l or MCS_NOTODAY else l:=l and not MCS_NOTODAY;
    setwindowlong(self.handle,GWL_STYLE,l);
   end;
end;

procedure TMonthCalendar.SetMultiSelect(avalue:boolean);
var
 l:longint;

begin
 if (FMultiSelect<>avalue) then
  begin
   FMultiSelect:=avalue;

   RecreateWnd;

   {
   l:=getwindowlong(self.handle,GWL_STYLE);
   if avalue then l:=l or MCS_MULTISELECT else l:=l and not MCS_MULTISELECT;
   setwindowlong(self.handle,GWL_STYLE,l);

   l:=getwindowlong(self.handle,GWL_STYLE);

   senddebug('style = '+inttohex(l,8));
   senddebug('range = '+inttostr(maxselcount));
   maxselcount:=7;
   }
  end;
end;

procedure TMonthCalendar.SetWeekNumbers(avalue:boolean);
var
 l:longint;

begin
 if FWeekNumbers<>avalue then
  begin
   FWeekNumbers:=avalue;
   l:=getwindowlong(self.handle,GWL_STYLE);
   if avalue then l:=l or MCS_WEEKNUMBERS else l:=l and not MCS_WEEKNUMBERS;
   setwindowlong(self.handle,GWL_STYLE,l);
  end;
end;

procedure TMonthCalendar.SetDate(avalue:TDate);
var
 ST: TSystemTime;
 dt:tdatetime;
begin
 dt:=avalue;
 {
 if (aValue > FMaxDate) then
 raise EDateTimeError.CreateFmt('Larger than max. value',[DateToStr(FMaxDate)]);

 if (aValue < FMinDate) then
 raise EDateTimeError.CreateFmt('Smaller than min. value',[DateToStr(FMaxDate)]);
 }

 DateTimeToSystemTime(dt,ST);

 if MonthCal_SetCurSel(Handle, ST) then FDate := aValue;
end;

procedure TMonthCalendar.SetVisible(avalue:boolean);
begin
 if (avalue<>FVisible) then
  if avalue then showwindow(self.handle,SW_SHOW) else showwindow(self.handle,SW_HIDE);
 FVisible:=avalue;
end;

procedure TMonthCalendar.SetMaxSelCount(avalue:integer);
begin
 if (fMaxSelCount<>avalue) and (multiselect) then
   begin
    MonthCal_SetMaxSelCount(self.handle,avalue);
    fMaxSelCount:=avalue;
   end;
end;

function TMonthCalendar.GetMaxSelCount:integer;
begin
 result:=MonthCal_GetMaxSelCount(handle);
end;

procedure TMonthCalendar.SetMinDate(avalue:TDate);
var
 ST:array[0..1] of TSystemTime;
 dt:tdatetime;
begin
 dt:=avalue;
 DateTimeToSystemTime(dt,ST[0]);
 if MonthCal_SetRange(Handle,GDTR_MIN,@ST) then FMinDate:=aValue;
end;

procedure TMonthCalendar.SetMaxDate(avalue:TDate);
var
 ST:array[0..1] of TSystemTime;
 dt:tdatetime;
begin
 dt:=avalue;
 DateTimeToSystemTime(dt,ST[1]);
 if MonthCal_SetRange(Handle,GDTR_MAX,@ST) then FMaxDate:=aValue;
end;

procedure TMonthCalendar.SetFirstDay(avalue:integer);
begin
 if (avalue<7) then MonthCal_SetFirstDayOfWeek(handle,avalue);
end;

function TMonthCalendar.GetFirstDay:integer;
begin
 result:=MonthCal_GetFirstDayOfWeek(handle) and $0ff;
end;

procedure Register;
begin
  RegisterComponents('TMS', [TMonthCalendar]);
end;

end.
