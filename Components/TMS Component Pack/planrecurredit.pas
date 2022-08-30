{$I TMSDEFS.INC}
{***********************************************************************}
{ TDBPlanner component                                                  }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit PlanRecurrEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, PlanRecurr, PlanUtil;

type
  TRecurrEdit = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Freq: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Notebook1: TNotebook;
    rDay: TRadioButton;
    rWeekDay: TRadioButton;
    cMon: TCheckBox;
    cTue: TCheckBox;
    cWed: TCheckBox;
    cThu: TCheckBox;
    cFri: TCheckBox;
    cSat: TCheckBox;
    cSun: TCheckBox;
    rMonthDay: TRadioButton;
    rSpecialDay: TRadioButton;
    cWeekNum: TComboBox;
    cDay: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    rYearDay: TRadioButton;
    cYearDay: TComboBox;
    rYearSpecialDay: TRadioButton;
    cYearWeekNum: TComboBox;
    yck1: TCheckBox;
    yck2: TCheckBox;
    yck3: TCheckBox;
    yck4: TCheckBox;
    yck5: TCheckBox;
    yck6: TCheckBox;
    yck7: TCheckBox;
    yck8: TCheckBox;
    yck9: TCheckBox;
    yck10: TCheckBox;
    yck11: TCheckBox;
    yck12: TCheckBox;
    Interval: TEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    rInfinite: TRadioButton;
    rUntil: TRadioButton;
    rUntilDate: TRadioButton;
    cDate: TDateTimePicker;
    cOccur: TEdit;
    exsd: TDateTimePicker;
    ExList: TListBox;
    Button3: TButton;
    Button4: TButton;
    exst: TDateTimePicker;
    exed: TDateTimePicker;
    exet: TDateTimePicker;
    Button5: TButton;
    Label15: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FreqClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FRecurrency: string;
    procedure SetRecurrency(const Value:string);
  public
    { Public declarations }
    ExDates: TDateItems;
    property Recurrency: string read FRecurrency write SetRecurrency;
  end;


  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerRecurrencyEditor = class(TComponent)
  private
    FRecurrency: string;
    FLanguage: TRecurrencyDialogLanguage;
    procedure SetLanguage(const Value: TRecurrencyDialogLanguage);
  public
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Recurrency: string read FRecurrency write FRecurrency;
    property LanguageSettings: TRecurrencyDialogLanguage read FLanguage write SetLanguage;
  end;

var
  RecurrEdit: TRecurrEdit;

implementation

{$R *.dfm}

procedure TRecurrEdit.SetRecurrency(const Value: string);
var
  rh: TRecurrencyHandler;
  i: Integer;
  dn: Boolean;
  ed: string;
begin
  FRecurrency := Value;

  if Value = '' then
  begin
    Freq.ItemIndex := 0;
    Exit;
  end;

  rh := TRecurrencyHandler.Create;

  rh.Recurrency := Value;
  rh.Parse;

  Freq.ItemIndex := Integer(rh.Frequency) + 1;
  Interval.Text := IntToStr(rh.Interval);

  rInfinite.Checked := (rh.RepeatCount = 0) and (rh.RepeatUntil = 0);
  rUntil.Checked := (rh.RepeatCount > 0) and (rh.RepeatUntil = 0);
  rUntilDate.Checked := (rh.RepeatUntil > 0);

  cOccur.Text := IntToStr(rh.RepeatCount);

  if rUntilDate.Checked then
    cDate.Date := rh.RepeatUntil
  else
    cDate.Date := Now;

  cMon.Checked := false;
  cTue.Checked := false;
  cWed.Checked := false;
  cThu.Checked := false;
  cFri.Checked := false;
  cSat.Checked := false;
  cSun.Checked := false;

  rDay.Checked := true;
  rYearDay.Checked := true;
  rMonthDay.Checked := true;

  case Integer(rh.Frequency) of
  0:begin
    end;
  1:begin
      if (2 in rh.Days) and (3 in rh.Days) and (4 in rh.Days) and (5 in rh.Days) and (6 in rh.Days) then
        rWeekDay.Checked := true
      else
        rDay.Checked := true;
    end;
  2:begin
      cMon.Checked := 2 in rh.Days;
      cTue.Checked := 3 in rh.Days;
      cWed.Checked := 4 in rh.Days;
      cThu.Checked := 5 in rh.Days;
      cFri.Checked := 6 in rh.Days;
      cSat.Checked := 7 in rh.Days;
      cSun.Checked := 1 in rh.Days;
    end;
  3:begin
      dn := false;
      for i := 1 to 7 do
        if rh.DayNum[i] <> 0 then
          dn := true;

      if (dn) then
      begin
        rSpecialDay.Checked := true;

        if (2 in rh.Days) and (3 in rh.Days) and (4 in rh.Days) and (5 in rh.Days) and (6 in rh.Days) then
        begin
          cDay.ItemIndex := 0;
          cWeekNum.ItemIndex := rh.DayNum[2] - 1;
        end
        else
          if (1 in rh.Days) and (7 in rh.Days) then
          begin
            cDay.ItemIndex := 1;
            cWeekNum.ItemIndex := rh.DayNum[1] - 1;
          end
          else
          begin
            if 1 in rh.Days then 
            begin
              cDay.ItemIndex := 8;
              cWeekNum.ItemIndex := rh.DayNum[1] - 1;
            end;
            if 2 in rh.Days then
            begin
              cDay.ItemIndex := 2;
              cWeekNum.ItemIndex := rh.DayNum[2] - 1;
            end;
            if 3 in rh.Days then
            begin
              cDay.ItemIndex := 3;
              cWeekNum.ItemIndex := rh.DayNum[3] - 1;
            end;
            if 4 in rh.Days then
            begin
              cDay.ItemIndex := 4;
              cWeekNum.ItemIndex := rh.DayNum[4] - 1;
            end;
            if 5 in rh.Days then
            begin
              cDay.ItemIndex := 5;
              cWeekNum.ItemIndex := rh.DayNum[5] - 1;
            end;
            if 6 in rh.Days then
            begin
              cDay.ItemIndex := 6;
              cWeekNum.ItemIndex := rh.DayNum[6] - 1;
            end;
            if 7 in rh.Days then
            begin
              cDay.ItemIndex := 7;
              cWeekNum.ItemIndex := rh.DayNum[7] - 1;
            end;
          end;

      end;
    end;
  4:begin
      dn := false;
      for i := 1 to 7 do
        if rh.DayNum[i] <> 0 then
          dn := true;

      if (dn) then
      begin
        rYearSpecialDay.Checked := true;

        if (2 in rh.Days) and (3 in rh.Days) and (4 in rh.Days) and (5 in rh.Days) and (6 in rh.Days) then
        begin
          cYearDay.ItemIndex := 0;
          cYearWeekNum.ItemIndex := rh.DayNum[2] - 1;
        end
        else
          if (1 in rh.Days) and (7 in rh.Days) then
          begin
            cYearDay.ItemIndex := 1;
            cYearWeekNum.ItemIndex := rh.DayNum[1] - 1;
          end
          else
          begin
            if 1 in rh.Days then 
            begin
              cYearDay.ItemIndex := 8;
              cYearWeekNum.ItemIndex := rh.DayNum[1] - 1;
            end;
            if 2 in rh.Days then
            begin
              cYearDay.ItemIndex := 2;
              cYearWeekNum.ItemIndex := rh.DayNum[2] - 1;
            end;
            if 3 in rh.Days then
            begin
              cYearDay.ItemIndex := 3;
              cYearWeekNum.ItemIndex := rh.DayNum[3] - 1;
            end;
            if 4 in rh.Days then
            begin
              cYearDay.ItemIndex := 4;
              cYearWeekNum.ItemIndex := rh.DayNum[4] - 1;
            end;
            if 5 in rh.Days then
            begin
              cYearDay.ItemIndex := 5;
              cYearWeekNum.ItemIndex := rh.DayNum[5] - 1;
            end;
            if 6 in rh.Days then
            begin
              cYearDay.ItemIndex := 6;
              cYearWeekNum.ItemIndex := rh.DayNum[6] - 1;
            end;
            if 7 in rh.Days then 
            begin
              cYearDay.ItemIndex := 7;
              cYearWeekNum.ItemIndex := rh.DayNum[7] - 1;
            end;
          end;
      end;

      yck1.Checked := rh.Months.HasValue(1);
      yck2.Checked := rh.Months.HasValue(2);
      yck3.Checked := rh.Months.HasValue(3);
      yck4.Checked := rh.Months.HasValue(4);
      yck5.Checked := rh.Months.HasValue(5);
      yck6.Checked := rh.Months.HasValue(6);
      yck7.Checked := rh.Months.HasValue(7);
      yck8.Checked := rh.Months.HasValue(8);
      yck9.Checked := rh.Months.HasValue(9);
      yck10.Checked := rh.Months.HasValue(10);
      yck11.Checked := rh.Months.HasValue(11);
      yck12.Checked := rh.Months.HasValue(12);

    end;
  end;

  for i := 1 to rh.ExDates.Count do
  begin
    if (Frac(rh.ExDates.Items[i - 1].StartDate) = 0) and (Frac(rh.ExDates.Items[i - 1].EndDate) = 0) then
      ed := DateToStr(rh.ExDates.Items[i - 1].StartDate)+ ' to ' +DateToStr(rh.ExDates.Items[i - 1].EndDate)    
    else
      ed := DateToStr(rh.ExDates.Items[i - 1].StartDate)+ ' - ' + TimeToStr(rh.ExDates.Items[i - 1].StartDate) + ' to ' +DateToStr(rh.ExDates.Items[i - 1].EndDate)+ ' - ' + TimeToStr(rh.ExDates.Items[i - 1].EndDate);
    ExList.Items.Add(ed);
    with ExDates.Add do
    begin
      StartDate := rh.ExDates.Items[i - 1].StartDate;
      EndDate := rh.ExDates.Items[i - 1].EndDate;
    end;
  end;

  rh.Free;
end;

procedure TRecurrEdit.Button1Click(Sender: TObject);
var
  rh: TRecurrencyHandler;
  i,e: Integer;
  dn: TDayArray;
begin
  if Freq.ItemIndex = 0 then
  begin
    FRecurrency := '';
    Exit;
  end;

  rh := TRecurrencyHandler.Create;
  rh.Frequency := TRecurrencyFrequency(Freq.ItemIndex - 1);

  for i := 1 to ExDates.Count do
  begin
    with rh.ExDates.Add do
    begin
      StartDate := ExDates.Items[i - 1].StartDate;
      EndDate := ExDates.Items[i - 1].EndDate;
    end;
  end;

  val( Interval.Text,i,e);
  rh.Interval := i;
  val(cOccur.Text, i, e);

  if rInfinite.Checked then
  begin
    rh.RepeatCount := 0;
    rh.RepeatUntil := 0;
  end
  else
    if rUntil.Checked then
      rh.RepeatCount := i
    else
      rh.RepeatCount := 0;

  if rUntilDate.Checked then
    rh.RepeatUntil := Int(cDate.Date)
  else
    rh.RepeatUntil := 0;

  rh.Days := [];

  if rh.Frequency = rfDaily then
  begin
    if rWeekDay.Checked then
      rh.Days := [2,3,4,5,6];
  end;

  if rh.Frequency = rfWeekly then
  begin
    if cMon.Checked then
      rh.Days := rh.Days + [2];
    if cTue.Checked then
      rh.Days := rh.Days + [3];
    if cWed.Checked then
      rh.Days := rh.Days + [4];
    if cThu.Checked then
      rh.Days := rh.Days + [5];
    if cFri.Checked then
      rh.Days := rh.Days + [6];
    if cSat.Checked then
      rh.Days := rh.Days + [7];
    if cSun.Checked then
      rh.Days := rh.Days + [1];
  end;

  if rh.Frequency = rfMonthly then
  begin
    if (rSpecialDay.Checked) then
    begin
      for i := 1 to 7 do dn[i] := 0;

      case cDay.ItemIndex of
      0:begin
          rh.Days := [2,3,4,5,6];
          dn[2] := cWeekNum.ItemIndex + 1;
          dn[3] := cWeekNum.ItemIndex + 1;
          dn[4] := cWeekNum.ItemIndex + 1;
          dn[5] := cWeekNum.ItemIndex + 1;
          dn[6] := cWeekNum.ItemIndex + 1;
        end;
      1:begin
          rh.Days := [1,7];
          dn[1] := cWeekNum.ItemIndex + 1;
          dn[7] := cWeekNum.ItemIndex + 1;
        end;
      2:begin
          rh.Days := [2];
          dn[2] := cWeekNum.ItemIndex + 1;
        end;
      3:begin
          rh.Days := [3];
          dn[3] := cWeekNum.ItemIndex + 1;
        end;
      4:begin
          rh.Days := [4];
          dn[4] := cWeekNum.ItemIndex + 1;
        end;
      5:begin
          rh.Days := [5];
          dn[5] := cWeekNum.ItemIndex + 1;
        end;
      6:begin
          rh.Days := [6];
          dn[6] := cWeekNum.ItemIndex + 1;
        end;
      7:begin
          rh.Days := [7];
          dn[7] := cWeekNum.ItemIndex + 1;
        end;
      8:begin
          rh.Days := [1];
          dn[1] := cWeekNum.ItemIndex + 1;
        end;

      end;
      for i := 1 to 7 do
        rh.DayNum[i] := dn[i];
    end;
  end;

  if rh.Frequency = rfYearly then
  begin
    if (rYearSpecialDay.Checked) then
    begin
      for i := 1 to 7 do dn[i] := 0;

      case cYearDay.ItemIndex of
      0:begin
          rh.Days := [2,3,4,5,6];
          dn[2] := cYearWeekNum.ItemIndex + 1;
          dn[3] := cYearWeekNum.ItemIndex + 1;
          dn[4] := cYearWeekNum.ItemIndex + 1;
          dn[5] := cYearWeekNum.ItemIndex + 1;
          dn[6] := cYearWeekNum.ItemIndex + 1;
        end;
      1:begin
          rh.Days := [1,7];
          dn[1] := cYearWeekNum.ItemIndex + 1;
          dn[7] := cYearWeekNum.ItemIndex + 1;
        end;
      2:begin
          rh.Days := [2];
          dn[2] := cYearWeekNum.ItemIndex + 1;
        end;
      3:begin
          rh.Days := [3];
          dn[3] := cYearWeekNum.ItemIndex + 1;
        end;
      4:begin
          rh.Days := [4];
          dn[4] := cYearWeekNum.ItemIndex + 1;
        end;
      5:begin
          rh.Days := [5];
          dn[5] := cYearWeekNum.ItemIndex + 1;
        end;
      6:begin
          rh.Days := [6];
          dn[6] := cYearWeekNum.ItemIndex + 1;
        end;
      7:begin
          rh.Days := [7];
          dn[7] := cYearWeekNum.ItemIndex + 1;
        end;
      8:begin
          rh.Days := [1];
          dn[1] := cYearWeekNum.ItemIndex + 1;
        end;
        
      end;
      for i := 1 to 7 do
        rh.DayNum[i] := dn[i];
    end;

    rh.Months.Clear;
    if yck1.Checked then rh.Months.Add(1);
    if yck2.Checked then rh.Months.Add(2);
    if yck3.Checked then rh.Months.Add(3);
    if yck4.Checked then rh.Months.Add(4);
    if yck5.Checked then rh.Months.Add(5);
    if yck6.Checked then rh.Months.Add(6);
    if yck7.Checked then rh.Months.Add(7);
    if yck8.Checked then rh.Months.Add(8);
    if yck9.Checked then rh.Months.Add(9);
    if yck10.Checked then rh.Months.Add(10);
    if yck11.Checked then rh.Months.Add(11);
    if yck12.Checked then rh.Months.Add(12);
  end;

  FRecurrency := rh.Compose;
  rh.Free;
end;

procedure TRecurrEdit.FreqClick(Sender: TObject);
begin
  Notebook1.PageIndex := Freq.ItemIndex;
end;

{ RecurrencyEditor }

constructor TPlannerRecurrencyEditor.Create(AOwner: TComponent);
begin
  inherited;
  FLanguage := TRecurrencyDialogLanguage.Create;
end;

destructor TPlannerRecurrencyEditor.Destroy;
begin
  FLanguage.Free;
  inherited;
end;

function TPlannerRecurrencyEditor.Execute: Boolean;
var
  RE: TRecurrEdit;
begin
  RE := TRecurrEdit.Create(Application);

  RE.Caption := FLanguage.Caption;
  RE.TabSheet1.Caption := FLanguage.Settings;
  RE.Tabsheet2.Caption := FLanguage.Exceptions;
  RE.Freq.Caption := FLanguage.RecurrencyPattern;
  RE.GroupBox1.Caption := FLanguage.PatternDetails;
  RE.GroupBox2.Caption := FLanguage.Range;

  RE.rInfinite.Caption := FLanguage.RangeInfinite;
  RE.rUntil.Caption := FLanguage.RangeFor;
  RE.rUntilDate.Caption := FLanguage.RangeUntil;

  RE.Label1.Caption := FLanguage.Interval;
  RE.Label2.Caption := FLanguage.RangeOccurences;
  RE.Label15.Caption := FLanguage.Exceptions;

  RE.Freq.Items[0] := FLanguage.FreqNone;
  RE.Freq.Items[1] := FLanguage.FreqHourly;
  RE.Freq.Items[2] := FLanguage.FreqDaily;
  RE.Freq.Items[3] := FLanguage.FreqWeekly;
  RE.Freq.Items[4] := FLanguage.FreqMonthly;
  RE.Freq.Items[5] := FLanguage.FreqYearly;

  RE.Button1.Caption := FLanguage.ButtonOK;
  RE.Button2.Caption := FLanguage.ButtonCancel;
  RE.Button3.Caption := FLanguage.ButtonAdd;
  RE.Button4.Caption := FLanguage.ButtonRemove;
  RE.Button5.Caption := FLanguage.ButtonClear;

  RE.cWeekNum.Items[0] := FLanguage.EveryFirst;
  RE.cWeekNum.Items[1] := FLanguage.EverySecond;
  RE.cWeekNum.Items[2] := FLanguage.EveryThird;
  RE.cWeekNum.Items[3] := FLanguage.EveryFourth;

  RE.cYearWeekNum.Items[0] := FLanguage.EveryFirst;
  RE.cYearWeekNum.Items[1] := FLanguage.EverySecond;
  RE.cYearWeekNum.Items[2] := FLanguage.EveryThird;
  RE.cYearWeekNum.Items[3] := FLanguage.EveryFourth;

  RE.cDay.Items[0] := FLanguage.DayWeekday;
  RE.cDay.Items[1] := FLanguage.DayWeekend;
  RE.cDay.Items[2] := FLanguage.DayMonday;
  RE.cDay.Items[3] := FLanguage.DayTuesday;
  RE.cDay.Items[4] := FLanguage.DayWednesday;
  RE.cDay.Items[5] := FLanguage.DayThursday;
  RE.cDay.Items[6] := FLanguage.DayFriday;
  RE.cDay.Items[7] := FLanguage.DaySaturday;
  RE.cDay.Items[8] := FLanguage.DaySunday;

  RE.cYearDay.Items[0] := FLanguage.DayWeekday;
  RE.cYearDay.Items[1] := FLanguage.DayWeekend;
  RE.cYearDay.Items[2] := FLanguage.DayMonday;
  RE.cYearDay.Items[3] := FLanguage.DayTuesday;
  RE.cYearDay.Items[4] := FLanguage.DayWednesday;
  RE.cYearDay.Items[5] := FLanguage.DayThursday;
  RE.cYearDay.Items[6] := FLanguage.DayFriday;
  RE.cYearDay.Items[7] := FLanguage.DaySaturday;
  RE.cYearDay.Items[8] := FLanguage.DaySunday;

  RE.rDay.Caption := FLanguage.EveryDay;
  RE.rWeekDay.Caption := FLanguage.EveryWeekDay;

  RE.cMon.Caption := Copy(FLanguage.DayMonday,1,3);
  RE.cTue.Caption := Copy(FLanguage.DayTuesday,1,3);
  RE.cWed.Caption := Copy(FLanguage.DayWednesday,1,3);
  RE.cThu.Caption := Copy(FLanguage.DayThursday,1,3);
  RE.cFri.Caption := Copy(FLanguage.DayFriday,1,3);
  RE.cSat.Caption := Copy(FLanguage.DaySaturday,1,3);
  RE.cSun.Caption := Copy(FLanguage.DaySunday,1,3);

  RE.Label3.Caption := FLanguage.MonthJanuary;
  RE.Label4.Caption := FLanguage.MonthFebruary;
  RE.Label5.Caption := FLanguage.MonthMarch;
  RE.Label6.Caption := FLanguage.MonthApril;
  RE.Label7.Caption := FLanguage.MonthMay;
  RE.Label8.Caption := FLanguage.MonthJune;
  RE.Label9.Caption := FLanguage.MonthJuly;
  RE.Label10.Caption := FLanguage.MonthAugust;
  RE.Label11.Caption := FLanguage.MonthSeptember;
  RE.Label12.Caption := FLanguage.MonthOctober;
  RE.Label13.Caption := FLanguage.MonthNovember;
  RE.Label14.Caption := FLanguage.MonthDecember;

  RE.rMonthDay.Caption := FLanguage.EveryMonthDay;
  RE.rYearDay.Caption := FLanguage.EveryYearDay;
  RE.rSpecialDay.Caption := FLanguage.Every;
  RE.rYearSpecialDay.Caption := FLanguage.Every;

  RE.cDate.DateTime := Now;
  RE.Recurrency := Recurrency;
  Result := RE.ShowModal = mrOk;
  Recurrency := RE.Recurrency;
  RE.Free;
end;

procedure TRecurrEdit.Button5Click(Sender: TObject);
begin
  ExList.Items.Clear;
  ExDates.Clear;
end;

procedure TRecurrEdit.Button3Click(Sender: TObject);
var
  ed: string;

begin
  if exst.Checked and exet.Checked then
    ed := datetostr(exsd.Date)+ ' - ' + timetostr(exst.Time) + ' to ' +datetostr(exed.date)+ ' - ' + timetostr(exet.Time)
  else
    ed := datetostr(exsd.Date)+ ' to ' +datetostr(exed.date);


  if ExList.Items.IndexOf(ed) = -1 then
  begin
    ExList.Items.Add(ed);
    with ExDates.Add do
    begin
      if exst.Checked and exet.Checked then
      begin
        StartDate := int(exsd.Date) + frac(exst.Time);
        EndDate := int(exed.Date) + frac(exet.Time);
      end
      else
      begin
        StartDate := int(exsd.Date);
        EndDate := int(exed.Date);
      end;
    end;
  end;
end;

procedure TRecurrEdit.Button4Click(Sender: TObject);
var
  idx: Integer;
begin
  idx := ExList.ItemIndex;
  if idx >= 0 then
  begin
    ExList.Items.Delete(idx);
    ExDates.Delete(idx);
  end;
end;

procedure TRecurrEdit.FormCreate(Sender: TObject);
begin
  ExDates := TDateItems.Create;
  exsd.Date := Now;
  exed.Date := Now;
  exst.Checked := false;
  exet.Checked := false;
end;

procedure TRecurrEdit.FormDestroy(Sender: TObject);
begin
  ExDates.Free;
end;

procedure TPlannerRecurrencyEditor.SetLanguage(
  const Value: TRecurrencyDialogLanguage);
begin
  FLanguage.Assign(Value);
end;

end.
