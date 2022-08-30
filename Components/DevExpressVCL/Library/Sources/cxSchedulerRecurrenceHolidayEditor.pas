{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxSchedulerRecurrenceHolidayEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus,
  dxCore, cxStyles, cxGraphics, cxEdit, cxControls,
  cxContainer, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCalendar,
  ComCtrls, cxLookAndFeelPainters, cxButtons, cxCheckBox, cxRadioGroup,
  cxSpinEdit, cxTimeEdit, cxGroupBox, cxSchedulerStorage, cxLookAndFeels,
  cxSchedulerUtils, cxDateUtils, cxLabel, dxBevel,
  cxSchedulerRecurrence, cxSchedulerHolidays, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxForms;

type

  { TcxSchedulerCustomRecurrenceHolidayEditor }

  TcxSchedulerCustomRecurrenceHolidayEditor = class(TdxForm)
  private
    FHoliday: TcxSchedulerHolidaysLocationHoliday;
    FIsInternalActivation: Boolean;
    FReadOnly: Boolean;
  protected
    FModified: Boolean;
    FIsCreation: Boolean;

    procedure ApplyChanges; virtual;
    function GetFormColor: TColor; virtual;
    procedure InitializeControls; virtual;
    procedure LoadHolidayValuesIntoControls; virtual;
    procedure LoadValuesIntoTimeControls(const AStart, AFinish: TDateTime; AllDayHoliday: Boolean); virtual;
    procedure SaveChanges; virtual;
    procedure SetReadOnly(AValue: Boolean); virtual;
    procedure UpdateHolidayValuesFromControls; virtual;
  public
    constructor CreateEx(AHoliday: TcxSchedulerHolidaysLocationHoliday); virtual;

    function ShowModal: Integer; override;

    property Holiday: TcxSchedulerHolidaysLocationHoliday read FHoliday;
    property IsInternalActivation: Boolean read FIsInternalActivation write FIsInternalActivation;
    property Modified: Boolean read FModified;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  { TcxSchedulerRecurrenceHolidayEditorForm }

  TcxSchedulerRecurrenceHolidayEditorForm = class(TcxSchedulerCustomRecurrenceHolidayEditor)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    btnRemove: TcxButton;
    cbDay: TcxComboBox;
    cbDay1: TcxComboBox;
    cbDayOfWeek1: TcxCheckBox;
    cbDayOfWeek2: TcxCheckBox;
    cbDayOfWeek3: TcxCheckBox;
    cbDayOfWeek4: TcxCheckBox;
    cbDayOfWeek5: TcxCheckBox;
    cbDayOfWeek6: TcxCheckBox;
    cbDayOfWeek7: TcxCheckBox;
    cbMonths: TcxComboBox;
    cbMonths1: TcxComboBox;
    cbWeek: TcxComboBox;
    cbWeek1: TcxComboBox;
    deEndBy: TcxDateEdit;
    deStart: TcxDateEdit;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    gbPattern: TdxLayoutGroup;
    gbRange: TdxLayoutGroup;
    lbDay: TdxLayoutItem;
    lbMonths: TdxLayoutItem;
    lbMonths1: TdxLayoutItem;
    lbOccurrences: TdxLayoutItem;
    lbOf: TdxLayoutItem;
    lbOfEvery: TdxLayoutItem;
    lbOfEvery1: TdxLayoutItem;
    lbRecurEvery: TdxLayoutItem;
    lbStart1: TdxLayoutItem;
    lbWeeksOn: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    meDay: TcxMaskEdit;
    meDayOfMonth: TcxMaskEdit;
    meEndAfter: TcxMaskEdit;
    meNumMonth: TcxMaskEdit;
    meNumMonth1: TcxMaskEdit;
    meNumOfDay: TcxMaskEdit;
    meNumOfWeek: TcxMaskEdit;
    pnlDaily: TdxLayoutGroup;
    pnlMonthly: TdxLayoutGroup;
    pnlMonthlyDay: TdxLayoutGroup;
    pnlMonthlyThe: TdxLayoutGroup;
    pnlPeriodicity: TdxLayoutGroup;
    pnlViewsHost: TdxLayoutGroup;
    pnlWeekly: TdxLayoutGroup;
    pnlYearly: TdxLayoutGroup;
    rbDaily: TcxRadioButton;
    rbDay: TcxRadioButton;
    rbEndAfter: TcxRadioButton;
    rbEndBy: TcxRadioButton;
    rbEvery: TcxRadioButton;
    rbEvery1: TcxRadioButton;
    rbEveryWeekday: TcxRadioButton;
    rbMonthly: TcxRadioButton;
    rbNoEndDate: TcxRadioButton;
    rbThe: TcxRadioButton;
    rbThe1: TcxRadioButton;
    rbWeekly: TcxRadioButton;
    rbYearly: TcxRadioButton;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;

    procedure SelectPeriodicityClick(Sender: TObject);
    procedure meEndAfterPropertiesChange(Sender: TObject);
    procedure deEndByPropertiesChange(Sender: TObject);
    procedure rbNoEndDateClick(Sender: TObject);
    procedure meEndAfterExit(Sender: TObject);
    procedure ValidateNumber(Sender: TObject);
    procedure meDayPropertiesChange(Sender: TObject);
    procedure rbEveryWeekdayClick(Sender: TObject);
    procedure SetDayRadioButtonChecked(Sender: TObject);
    procedure SetTheRadioButtonChecked(Sender: TObject);
    procedure deStartPropertiesEditValueChanged(Sender: TObject);
    procedure DoChange(Sender: TObject);
    procedure cbMonthsPropertiesChange(Sender: TObject);
    procedure cbWeek1PropertiesChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure deEndByPropertiesEditValueChanged(Sender: TObject);
    procedure meEndAfterPropertiesEditValueChanged(Sender: TObject);
  private
    FLocked: Boolean;
    procedure CheckViewRecurrence(AResetPattern: Boolean);
    procedure FillDays;
    procedure FillMonths;
    procedure FillWeeks;
    function GetActivePattern: TcxRecurrence;
    function GetRecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo;
    function GetDayOfWeekIndex: Integer;
    function GetMonthIndex: Integer;
    function GetNumber(const S: string): Integer;
    function GetWeekIndex: Integer;
    function IsValidDailyPattern: Boolean;
    function IsValidMonthlyPattern: Boolean;
    function IsValidWeeklyPattern: Boolean;
    function IsValidYearlyPattern: Boolean;
    procedure ResetDailyTab;
    procedure ResetMonthlyTab;
    procedure ResetWeeklyTab;
    procedure ResetYearlyTab;
    procedure SelectDaysOfWeek(ADays: TDays);
    procedure SetActivePattern(Value: TcxRecurrence);
    procedure SetDailyPatternFromHoliday;
    procedure SetDateWithoutChangeHoliday(AEdit: TcxDateEdit; ADate: TDateTime);
    procedure SetItemIndexWithoutChangeHoliday(AEdit: TcxComboBox; AIndex: Integer);
    procedure SetMonthlyPatternFromHoliday;
    procedure SetPatternFromHoliday;
    procedure SetTextWithoutChangeHoliday(AEdit: TcxCustomMaskEdit; const AText: string);
    procedure SetupDaysOfWeek;
    procedure SetWeeklyPatternFromHoliday;
    procedure SetYearlyPatternFromHoliday;
  protected
    procedure ApplyChanges; override;
    procedure FillCombos; virtual;
    function GetFormColor: TColor; override;
    function GetRecurrenceInfoValidStatus: TcxRecurrenceValidStatus; virtual;
    procedure InitializeControls; override;
    function IsValid: Boolean;
    procedure LoadHolidayValuesIntoControls; override;

    procedure SaveChanges; override;
    procedure SaveHoliday(AHoliday: TcxSchedulerHolidaysLocationHoliday);
    procedure SaveRecurrenceInfo(AInfo: TcxSchedulerHolidayRecurrenceInfo); virtual;

    procedure SaveDailyParams(AInfo: TcxSchedulerHolidayRecurrenceInfo);
    procedure SaveWeeklyParams(AInfo: TcxSchedulerHolidayRecurrenceInfo);
    procedure SaveMonthlyParams(AInfo: TcxSchedulerHolidayRecurrenceInfo);
    procedure SaveYearlyParams(AInfo: TcxSchedulerHolidayRecurrenceInfo);

    procedure SetCaptions; virtual;
    procedure SetEndAfter; virtual;
    procedure SetEndDate; virtual;
    procedure SetHolidayRecurrenceFinish; virtual;
    procedure SetReadOnly(AValue: Boolean); override;

    procedure UpdateRange;

    property ActivePattern: TcxRecurrence read GetActivePattern write SetActivePattern;
    property RecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo read GetRecurrenceInfo;
    property Locked: Boolean read FLocked write FLocked;
  public
    property Holiday;
    property Modified;
    property ReadOnly;
  end;

  TcxSchedulerRecurrenceHolidayEditor = TcxSchedulerRecurrenceHolidayEditorForm; //workaround for Delphi 2005

  TcxSchedulerRecurrenceHolidayEditorClass = class of TcxSchedulerCustomRecurrenceHolidayEditor;

implementation

uses
  Variants, DateUtils, cxSchedulerStrs, cxFormats, cxSchedulerDialogs;

{$R *.dfm}

function cxDayTypeFromType(AIndex: Integer): TcxDayType;
begin
  if AIndex < 0 then AIndex := 0;
  Inc(AIndex);
  if AIndex >= Byte(cxdtSaturday) then
    Result := cxdtSaturday
  else
    Result := TcxDayType(AIndex);
end;

function cxDayNumberFromRange(AIndex: Integer): Integer;
begin
  Result := AIndex + 1;
end;

{ TcxSchedulerCustomRecurrenceHolidayEditor }

constructor TcxSchedulerCustomRecurrenceHolidayEditor.CreateEx(
  AHoliday: TcxSchedulerHolidaysLocationHoliday);
begin
  inherited Create(nil);
  FIsCreation := True;
  FHoliday := AHoliday;
  InitializeControls;
  LoadHolidayValuesIntoControls;
  FModified := False;
  FIsCreation := False;
end;

function TcxSchedulerCustomRecurrenceHolidayEditor.ShowModal: Integer;
begin
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
  cxDialogsMetricsStore.InitDialog(Self);
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
  ApplyChanges;
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.ApplyChanges;
begin
end;

function TcxSchedulerCustomRecurrenceHolidayEditor.GetFormColor: TColor;
begin
  Result := clBtnFace;
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.InitializeControls;
begin
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.LoadHolidayValuesIntoControls;
begin
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.LoadValuesIntoTimeControls(
  const AStart, AFinish: TDateTime; AllDayHoliday: Boolean);
begin
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.SaveChanges;
begin
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.SetReadOnly(AValue: Boolean);
begin
  FReadOnly := AValue;
end;

procedure TcxSchedulerCustomRecurrenceHolidayEditor.UpdateHolidayValuesFromControls;
begin
end;

{ TcxSchedulerRecurrenceHolidayEditorForm }

procedure TcxSchedulerRecurrenceHolidayEditorForm.ApplyChanges;
begin
  case ModalResult of
    mrOk:
      if not ReadOnly then
        SaveChanges;
    mrAbort:
      begin
        Holiday.RemoveRecurrence;
        FModified := True;
      end;
    mrCancel:
      FModified := False;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.FillCombos;
begin
  FillDays;
  FillMonths;
  FillWeeks;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetFormColor: TColor;
begin
  Result := deStart.Style.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetRecurrenceInfoValidStatus: TcxRecurrenceValidStatus;
var
  AHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  AHoliday := TcxSchedulerHolidaysLocationHoliday.Create(Holiday.Collection);
  try
    SaveHoliday(AHoliday);
    Result := AHoliday.RecurrenceInfo.GetValidStatus;
  finally
    AHoliday.Free;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.InitializeControls;
begin
  SetCaptions;
  FillCombos;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.IsValid: Boolean;
begin
  case ActivePattern of
    cxreDaily:
      Result := IsValidDailyPattern;
    cxreWeekly:
      Result := IsValidWeeklyPattern;
    cxreMonthly:
      Result := IsValidMonthlyPattern;
    else //cxreYearly
      Result := IsValidYearlyPattern;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.LoadHolidayValuesIntoControls;
begin
  FLocked := True;
  SetDateWithoutChangeHoliday(deStart, dxDateOf(Holiday.Date));
  SetPatternFromHoliday;
  SetHolidayRecurrenceFinish;
  FLocked := False;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveHoliday(
  AHoliday: TcxSchedulerHolidaysLocationHoliday);
begin
  if AHoliday <> Holiday then
    AHoliday.Assign(Holiday);
  SaveRecurrenceInfo(AHoliday.RecurrenceInfo);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveRecurrenceInfo(
  AInfo: TcxSchedulerHolidayRecurrenceInfo);
var
  AStart: TDateTime;
begin
  AStart := deStart.Date;
  AInfo.Holiday.Date := AStart;
  AInfo.Recurrence := ActivePattern;
  if rbNoEndDate.Checked then
    AInfo.Count := -1
  else if rbEndAfter.Checked then
    AInfo.Count := GetNumber(meEndAfter.Text)
  else
  begin
    AInfo.Count := 0;
    AInfo.Finish := deEndBy.Date;
  end;
  case ActivePattern of
    cxreDaily:   SaveDailyParams(AInfo);
    cxreWeekly:  SaveWeeklyParams(AInfo);
    cxreMonthly: SaveMonthlyParams(AInfo);
    cxreYearly:  SaveYearlyParams(AInfo);
  end;
  if not Locked then
    AInfo.Validate;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveChanges;
begin
  inherited;
  if not Holiday.IsRecurring then
    FModified := True;
  SaveHoliday(Holiday);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveDailyParams(
  AInfo: TcxSchedulerHolidayRecurrenceInfo);
begin
  if rbEvery.Checked then
  begin
    AInfo.DayType := cxdtEveryDay;
    AInfo.Periodicity := GetNumber(meDay.Text);
  end
  else
  begin
    AInfo.DayType := cxdtWeekDay;
    AInfo.Periodicity := 1;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveWeeklyParams(
  AInfo: TcxSchedulerHolidayRecurrenceInfo);

  procedure Check(ACheckBox: TcxCheckBox; var AOccurDays: TDays);
  begin
    if ACheckBox.Checked then
      Include(AOccurDays, TDay(ACheckBox.Tag - 1));
  end;

var
  AOccurDays: TDays;
begin
  AInfo.Periodicity := GetNumber(meNumOfWeek.Text);
  AOccurDays := [];
  Check(cbDayOfWeek1, AOccurDays);
  Check(cbDayOfWeek2, AOccurDays);
  Check(cbDayOfWeek3, AOccurDays);
  Check(cbDayOfWeek4, AOccurDays);
  Check(cbDayOfWeek5, AOccurDays);
  Check(cbDayOfWeek6, AOccurDays);
  Check(cbDayOfWeek7, AOccurDays);
  AInfo.OccurDays := AOccurDays;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveMonthlyParams(
  AInfo: TcxSchedulerHolidayRecurrenceInfo);
begin
  if rbDay.Checked then
  begin
    AInfo.DayType := cxdtDay;
    AInfo.DayNumber := GetNumber(meNumOfDay.Text);
    AInfo.Periodicity := GetNumber(meNumMonth.Text);
  end
  else
  begin
    AInfo.DayType := cxDayTypeFromType(cbDay.ItemIndex);
    AInfo.DayNumber := cxDayNumberFromRange(cbWeek.ItemIndex);
    AInfo.Periodicity := GetNumber(meNumMonth1.Text);
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SaveYearlyParams(
  AInfo: TcxSchedulerHolidayRecurrenceInfo);
begin
  if rbEvery1.Checked then
  begin
    AInfo.DayType := cxdtDay;
    AInfo.Periodicity := cbMonths.ItemIndex + 1;
    AInfo.DayNumber := GetNumber(meDayOfMonth.Text);
  end
  else
  begin
    AInfo.DayType := cxDayTypeFromType(cbDay1.ItemIndex);
    AInfo.DayNumber := cxDayNumberFromRange(cbWeek1.ItemIndex);
    AInfo.Periodicity := cbMonths1.ItemIndex + 1;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetCaptions;
begin
  Caption := cxGetResourceString(@scxRecurrenceHolidayCaption);
  // pattern
  gbPattern.Caption := cxGetResourceString(@scxRecurrencePattern);
  rbDaily.Caption := cxGetResourceString(@scxDaily);
  rbWeekly.Caption := cxGetResourceString(@scxWeekly);
  rbMonthly.Caption := cxGetResourceString(@scxMonthly);
  rbYearly.Caption := cxGetResourceString(@scxYearly);
  // daily
  rbEvery.Caption := cxGetResourceString(@scxEvery);
  rbEveryWeekday.Caption := cxGetResourceString(@scxEveryWeekDay);
  lbDay.Caption := cxGetResourceString(@scxDay1);
  // weekly
  lbRecurEvery.Caption := cxGetResourceString(@scxRecurEvery);
  lbWeeksOn.Caption := cxGetResourceString(@scxWeeksOn);
  SetupDaysOfWeek;
  // monthly
  lbOfEvery.Caption := cxGetResourceString(@scxOfEvery);
  lbOfEvery1.Caption := lbOfEvery.Caption;
  lbMonths.Caption := cxGetResourceString(@scxMonths);
  lbMonths1.Caption := lbMonths.Caption;
  rbDay.Caption := cxGetResourceString(@scxDay);
  rbThe.Caption := cxGetResourceString(@scxThe);
  // yearly
  rbEvery1.Caption := rbEvery.Caption;
  rbThe1.Caption := rbThe.Caption;
  lbOf.Caption := cxGetResourceString(@scxOf);
  // range of recurrence
  gbRange.Caption := cxGetResourceString(@scxRangeOfRecurrence);
  lbStart1.Caption := cxGetResourceString(@scxStart1);
  rbNoEndDate.Caption := cxGetResourceString(@scxNoEndDate);
  rbEndAfter.Caption := cxGetResourceString(@scxEndAfter);
  rbEndBy.Caption := cxGetResourceString(@scxEndBy);
  lbOccurrences.Caption := cxGetResourceString(@scxOccurences);
  // buttons
  btnOk.Caption := cxGetResourceString(@scxOk);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
  btnRemove.Caption := cxGetResourceString(@scxRemoveRecur);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetEndAfter;
var
  ACount: Integer;
  AHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  AHoliday := TcxSchedulerHolidaysLocationHoliday.Create(Holiday.Collection);
  try
    SaveHoliday(AHoliday);
    with TcxSchedulerHolidayCalculator.Create(AHoliday, 0, cxMaxDate) do
    try
      ACount := GetOccurrenceCount(deEndBy.Date);
    finally
      Free;
    end;
  finally
    AHoliday.Free;
  end;
  if ACount < 1 then
    ACount := 1;
  SetTextWithoutChangeHoliday(meEndAfter, IntToStr(ACount));
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetEndDate;
var
  ADate: TDateTime;
  AHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  AHoliday := TcxSchedulerHolidaysLocationHoliday.Create(Holiday.Collection);
  try
    AHoliday.Date := Holiday.Date;
    SaveHoliday(AHoliday);
    AHoliday.RecurrenceInfo.Count := GetNumber(meEndAfter.Text);
    ADate := AHoliday.RecurrenceInfo.GetEndDate;
  finally
    AHoliday.Free;
  end;
  SetDateWithoutChangeHoliday(deEndBy, ADate);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetHolidayRecurrenceFinish;
var
  ACount: Integer;
  AHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  AHoliday := TcxSchedulerHolidaysLocationHoliday.Create(Holiday.Collection);
  try
    SaveHoliday(AHoliday);
    with TcxSchedulerHolidayCalculator.Create(AHoliday, 0, cxMaxDate) do
    try
      ACount := RecurrenceInfo.Count;
      if ACount < 0 then ACount := 10;
      if ACount > 0 then
      begin
        if RecurrenceInfo.Count < 0 then
          rbNoEndDate.Checked := True
        else
          rbEndAfter.Checked := True;
        CalcOccurrence(ACount);
        SetDateWithoutChangeHoliday(deEndBy, dxDateOf(OccurrenceStart));
      end
      else
      begin
        rbEndBy.Checked := True;
        SetDateWithoutChangeHoliday(deEndBy, dxDateOf(RecurrenceInfo.Finish));
        ACount := GetOccurrenceCount(RecurrenceInfo.Finish);
        if ACount < 1 then ACount := 1;
      end;
      SetTextWithoutChangeHoliday(meEndAfter, IntToStr(ACount));
    finally
      Free;
    end;
  finally
    AHoliday.Free;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetReadOnly(
  AValue: Boolean);
begin
  inherited;
  gbPattern.Enabled := not AValue;
  gbRange.Enabled := not AValue;
  btnCancel.Enabled := not AValue;
  btnRemove.Enabled := not AValue;
  btnOk.Enabled := not AValue;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.UpdateRange;
begin
  if rbEndBy.Checked then
    SetEndAfter
  else
    SetEndDate;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.CheckViewRecurrence(AResetPattern: Boolean);
begin
  if AResetPattern then
  begin
    if rbDaily.Checked then
      ResetDailyTab;
    if rbWeekly.Checked then
      ResetWeeklyTab;
    if rbMonthly.Checked then
      ResetMonthlyTab;
    if rbYearly.Checked then
      ResetYearlyTab;
  end;
  pnlDaily.Visible := rbDaily.Checked;
  pnlWeekly.Visible := rbWeekly.Checked;
  pnlMonthly.Visible := rbMonthly.Checked;
  pnlYearly.Visible := rbYearly.Checked;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.FillDays;
var
  S: TStrings;
  I: Integer;
begin
  S := cbDay.Properties.Items;
  S.BeginUpdate;
  try
    S.Add(cxGetResourceString(@scxDay1));
    S.Add(cxGetResourceString(@scxWeekday));
    S.Add(cxGetResourceString(@scxWeekendday));
    for I := 1 to 7 do
      S.Add(dxFormatSettings.LongDayNames[I]);
  finally
    S.EndUpdate;
  end;
  cbDay1.Properties.Items.Assign(S);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.FillMonths;
var
  S: TStrings;
  I: Integer;
begin
  S := cbMonths.Properties.Items;
  S.BeginUpdate;
  try
    for I := 1 to 12 do
      S.Add(dxFormatSettings.LongMonthNames[I]);
  finally
    S.EndUpdate;
  end;
  cbMonths1.Properties.Items.Assign(S);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.FillWeeks;
var
  S: TStrings;
  I: Integer;
begin
  S := cbWeek.Properties.Items;
  S.BeginUpdate;
  try
    for I := 0 to 4 do
      S.Add(cxGetResourceString(sRangeNames[I]));
  finally
    S.EndUpdate;
  end;
  cbWeek1.Properties.Items.Assign(S);
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetActivePattern: TcxRecurrence;
begin
  if rbDaily.Checked then
    Result := cxreDaily
  else
    if rbWeekly.Checked then
      Result := cxreWeekly
    else
      if rbMonthly.Checked then
        Result := cxreMonthly
      else
        Result := cxreYearly;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetRecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo;
begin
  Result := Holiday.RecurrenceInfo;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetDayOfWeekIndex: Integer;
begin
  Result := DayOfWeek(RecurrenceInfo.Start) + 2;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetMonthIndex: Integer;
begin
  Result := MonthOf(RecurrenceInfo.Start) - 1;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetNumber(const S: string): Integer;
begin
  Result := StrToIntDef(Trim(S), 1);
end;

function TcxSchedulerRecurrenceHolidayEditorForm.GetWeekIndex: Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(RecurrenceInfo.Start, Y, M, D);
  Result := D div 7;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.IsValidDailyPattern: Boolean;
begin
  Result := GetNumber(meDay.Text) >= 1;
  if not Result then
  begin
    ActiveControl := meDay;
    Exit;
  end;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.IsValidMonthlyPattern: Boolean;
begin
  Result := False;
  if not (GetNumber(meNumOfDay.Text) in [1..31]) then
  begin
    ActiveControl := meNumOfDay;
    Exit;
  end;
  if GetNumber(meNumMonth.Text) < 1 then
  begin
    ActiveControl := meNumMonth;
    Exit;
  end;
  if GetNumber(meNumMonth1.Text) < 1 then
  begin
    ActiveControl := meNumMonth1;
    Exit;
  end;
  Result := True;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.IsValidWeeklyPattern: Boolean;
begin
  Result := False;
  if GetNumber(meNumOfWeek.Text) < 1 then
  begin
    ActiveControl := meNumOfWeek;
    Exit;
  end;
  if not (cbDayOfWeek1.Checked or cbDayOfWeek2.Checked or
    cbDayOfWeek3.Checked or cbDayOfWeek4.Checked or cbDayOfWeek5.Checked or
    cbDayOfWeek6.Checked or cbDayOfWeek7.Checked) then
  begin
    ActiveControl := cbDayOfWeek1;
    Exit;
  end;
  Result := True;
end;

function TcxSchedulerRecurrenceHolidayEditorForm.IsValidYearlyPattern: Boolean;
var
  ADay: Integer;
begin
  ADay := GetNumber(meDayOfMonth.Text);
  Result := (ADay >=1) and (ADay <= DaysPerMonth(2000, cbMonths.ItemIndex + 1));
  if not Result then
    ActiveControl := meDayOfMonth;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.ResetDailyTab;
begin
  SetTextWithoutChangeHoliday(meDay, '1');
  rbEvery.Checked := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.ResetMonthlyTab;
begin
  SetTextWithoutChangeHoliday(meNumOfDay, IntToStr(DayOf(RecurrenceInfo.Start)));
  SetTextWithoutChangeHoliday(meNumMonth, '1');
  SetTextWithoutChangeHoliday(meNumMonth1, '1');
  cbWeek.ItemIndex := GetWeekIndex;
  cbDay.ItemIndex := GetDayOfWeekIndex;
  rbDay.Checked := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.ResetWeeklyTab;
begin
  SetTextWithoutChangeHoliday(meNumOfWeek, '1');
  SelectDaysOfWeek([TDay(DayOfWeek(RecurrenceInfo.Start) - 1)]);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.ResetYearlyTab;
begin
  cbDay1.ItemIndex := GetDayOfWeekIndex;
  meDayOfMonth.Text := IntToStr(DayOf(RecurrenceInfo.Start));
  cbWeek1.ItemIndex := GetWeekIndex;
  cbMonths.ItemIndex := GetMonthIndex;
  cbMonths1.ItemIndex := GetMonthIndex;
  rbEvery1.Checked := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SelectDaysOfWeek(ADays: TDays);

  procedure Check(ACheckBox: TcxCheckBox);
  begin
    ACheckBox.Checked := TDay(ACheckBox.Tag - 1) in ADays;
  end;

begin
  Check(cbDayOfWeek1);
  Check(cbDayOfWeek2);
  Check(cbDayOfWeek3);
  Check(cbDayOfWeek4);
  Check(cbDayOfWeek5);
  Check(cbDayOfWeek6);
  Check(cbDayOfWeek7);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetActivePattern(
  Value: TcxRecurrence);
begin
  UpdateRange;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetDailyPatternFromHoliday;
begin
  rbDaily.Checked := True;
  CheckViewRecurrence(False);
  if RecurrenceInfo.DayType = cxdtWeekDay then
  begin
    rbEveryWeekday.Checked := True;
    SetTextWithoutChangeHoliday(meDay, '1');
  end
  else
  begin
    rbEvery.Checked := True;
    SetTextWithoutChangeHoliday(meDay, IntToStr(RecurrenceInfo.Periodicity));
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetDateWithoutChangeHoliday(
  AEdit: TcxDateEdit; ADate: TDateTime);
var
  AOnChange, AOnEditValueChanged: TNotifyEvent;
begin
  AOnChange := AEdit.Properties.OnChange;
  AOnEditValueChanged := AEdit.Properties.OnEditValueChanged;
  AEdit.Properties.OnChange := nil;
  AEdit.Properties.OnEditValueChanged := nil;
  AEdit.Date := ADate;
  AEdit.Properties.OnChange := AOnChange;
  AEdit.Properties.OnEditValueChanged := AOnEditValueChanged;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetItemIndexWithoutChangeHoliday(
  AEdit: TcxComboBox; AIndex: Integer);
var
  AOnChange: TNotifyEvent;
begin
  AOnChange := AEdit.Properties.OnChange;
  AEdit.Properties.OnChange := nil;
  AEdit.ItemIndex := AIndex;
  AEdit.Properties.OnChange := AOnChange;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetMonthlyPatternFromHoliday;
var
  AType: TcxDayType;
  ANum: Integer;
begin
  ResetMonthlyTab;
  rbMonthly.Checked := True;
  CheckViewRecurrence(False);
  AType := RecurrenceInfo.DayType;
  if AType = cxdtDay then
  begin
    rbDay.Checked := True;
    SetTextWithoutChangeHoliday(meNumOfDay, IntToStr(RecurrenceInfo.DayNumber));
    SetTextWithoutChangeHoliday(meNumMonth, IntToStr(RecurrenceInfo.Periodicity));
  end
  else
  begin
    rbThe.Checked := True;
    SetTextWithoutChangeHoliday(meNumMonth1, IntToStr(RecurrenceInfo.Periodicity));
    ANum := RecurrenceInfo.DayNumber;
    if Integer(AType) >= 1 then Dec(Byte(AType));
    SetItemIndexWithoutChangeHoliday(cbWeek, ANum - 1);
    SetItemIndexWithoutChangeHoliday(cbDay, Byte(AType));
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetPatternFromHoliday;
begin
  ActivePattern := RecurrenceInfo.Recurrence;
  case RecurrenceInfo.Recurrence of
    cxreDaily:   SetDailyPatternFromHoliday;
    cxreWeekly:  SetWeeklyPatternFromHoliday;
    cxreMonthly: SetMonthlyPatternFromHoliday;
    cxreYearly:  SetYearlyPatternFromHoliday;
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetupDaysOfWeek;

  procedure SetDayCheckBox(ACheck: TcxCheckBox);
  var
    AIndex: Integer;
  begin
    AIndex := ACheck.Tag;
    Inc(AIndex, TcxSchedulerDateTimeHelper.StartOfWeek);
    if AIndex > 7 then Dec(AIndex, 7);
    ACheck.Caption := dxFormatSettings.LongDayNames[AIndex];
    ACheck.Tag := AIndex;
  end;

begin
  SetDayCheckBox(cbDayOfWeek1);
  SetDayCheckBox(cbDayOfWeek2);
  SetDayCheckBox(cbDayOfWeek3);
  SetDayCheckBox(cbDayOfWeek4);
  SetDayCheckBox(cbDayOfWeek5);
  SetDayCheckBox(cbDayOfWeek6);
  SetDayCheckBox(cbDayOfWeek7);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetTextWithoutChangeHoliday(
  AEdit: TcxCustomMaskEdit; const AText: string);
var
  AOnChange, AOnEditValueChanged: TNotifyEvent;
  AOnValidate: TcxEditValidateEvent;
begin
  AOnChange := AEdit.Properties.OnChange;
  AOnEditValueChanged := AEdit.Properties.OnEditValueChanged;
  AOnValidate := AEdit.Properties.OnValidate;
  AEdit.Properties.OnChange := nil;
  AEdit.Properties.OnEditValueChanged := nil;
  AEdit.Text := AText;
  AEdit.Properties.OnChange := AOnChange;
  AEdit.Properties.OnEditValueChanged := AOnEditValueChanged;
  AEdit.Properties.OnValidate := AOnValidate;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetWeeklyPatternFromHoliday;
begin
  rbWeekly.Checked := True;
  CheckViewRecurrence(False);
  meNumOfWeek.Text := IntToStr(RecurrenceInfo.Periodicity);
  if Holiday.IsRecurring and (Holiday.RecurrenceInfo.GetValidStatus = rvsValid) then
    SelectDaysOfWeek(RecurrenceInfo.OccurDays)
  else
    SelectDaysOfWeek([TDay(DayOfWeek(Holiday.Date) - 1)]);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetYearlyPatternFromHoliday;
var
  AType: TcxDayType;
  ANum: Integer;
begin
  rbYearly.Checked := True;
  CheckViewRecurrence(False);
  AType := RecurrenceInfo.DayType;
  ANum := RecurrenceInfo.DayNumber;
  if AType = cxdtDay then
  begin
    rbEvery1.Checked := True;
    cbMonths.ItemIndex := RecurrenceInfo.Periodicity - 1;
    SetTextWithoutChangeHoliday(meDayOfMonth, IntToStr(ANum));
  end
  else
  begin
    rbThe1.Checked := True;
    cbMonths1.ItemIndex := RecurrenceInfo.Periodicity - 1;
    if Integer(AType) >= 1 then Dec(Byte(AType));
    SetItemIndexWithoutChangeHoliday(cbWeek1, ANum - 1);
    SetItemIndexWithoutChangeHoliday(cbDay1, Byte(AType));
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SelectPeriodicityClick(
  Sender: TObject);
begin
  if Locked then Exit;
  CheckViewRecurrence(True);
  ActivePattern := TcxRecurrence(TcxRadioButton(Sender).Tag);
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.meEndAfterPropertiesChange(
  Sender: TObject);
begin
  rbEndAfter.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.deEndByPropertiesChange(
  Sender: TObject);
begin
  rbEndBy.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.rbNoEndDateClick(
  Sender: TObject);
begin
  SetTextWithoutChangeHoliday(meEndAfter, '10');
  SetEndDate;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.meEndAfterExit(
  Sender: TObject);
begin
  ValidateNumber(Sender);
  SetEndDate;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.ValidateNumber(
  Sender: TObject);
begin
  with TcxMaskEdit(Sender) do
  begin
    if GetNumber(Text) <= 0 then
    begin
      MessageDlg(cxGetResourceString(@scxInvalidNumber), mtError, [mbOK], 0);
      Text := '1';
    end
    else
      Text := IntToStr(GetNumber(Text));
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.meDayPropertiesChange(
  Sender: TObject);
begin
  rbEvery.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.rbEveryWeekdayClick(
  Sender: TObject);
begin
  SetTextWithoutChangeHoliday(meDay, '1');
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetDayRadioButtonChecked(
  Sender: TObject);
begin
  rbDay.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.SetTheRadioButtonChecked(
  Sender: TObject);
begin
  rbThe.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.deStartPropertiesEditValueChanged(
  Sender: TObject);
begin
  UpdateRange;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.DoChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.cbMonthsPropertiesChange(
  Sender: TObject);
begin
  rbEvery1.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.cbWeek1PropertiesChange(
  Sender: TObject);
begin
  rbThe1.Checked := True;
  FModified := True;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.btnOkClick(Sender: TObject);
begin
  if ReadOnly then
    ModalResult := mrOk
  else
  begin
    if IsValid then
    begin
      case GetRecurrenceInfoValidStatus of
        rvsValid:
            ModalResult := mrOk;
        rvsReplaceOccurrenceDate:
          if (MessageDlg(Format(cxGetResourceString(@scxReplaceOccurrenceDate),
            [meNumOfDay.Text]), mtWarning, [mbOK,mbCancel], 0) = mrOk) then ModalResult := mrOk;
        rvsInvalidDuration:
          MessageDlg(cxGetResourceString(@scxInvalidRecurrenceDuration), mtWarning, [mbOK], 0);
        else
          MessageDlg(cxGetResourceString(@scxWrongPattern), mtWarning, [mbOK], 0);
       end
    end
    else
      MessageDlg(cxGetResourceString(@scxWrongPattern), mtWarning, [mbOK], 0);
  end;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.deEndByPropertiesEditValueChanged(
  Sender: TObject);
begin
  if Locked then Exit;
  SetEndAfter;
end;

procedure TcxSchedulerRecurrenceHolidayEditorForm.meEndAfterPropertiesEditValueChanged(
  Sender: TObject);
begin
  if Locked then Exit;
  SetEndDate;
end;

end.
