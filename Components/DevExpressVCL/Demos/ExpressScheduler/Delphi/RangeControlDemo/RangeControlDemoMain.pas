unit RangeControlDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DemoBasicMain, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, Menus, cxStyles, cxEdit,
  cxScheduler, cxSchedulerStorage, cxSchedulerCustomControls,
  cxSchedulerCustomResourceView, cxSchedulerDayView, cxSchedulerAgendaView,
  cxSchedulerDateNavigator, cxSchedulerHolidays, cxSchedulerTimeGridView,
  cxSchedulerUtils, cxSchedulerWeekView, cxSchedulerYearView,
  cxSchedulerGanttView, cxSchedulerRecurrence, cxClasses, ExtCtrls,
  ComCtrls, StdCtrls, cxSplitter, dxRangeControl,
  cxSchedulerRangeControlClientProperties, cxContainer, cxGroupBox, cxCheckBox,
  cxLabel, cxSpinEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit;

type
  TRangeControlDemoMainForm = class(TDemoBasicMainForm)
    SchedulerStorage: TcxSchedulerStorage;
    dxRangeControl1: TdxRangeControl;
    cxSplitter1: TcxSplitter;
    cxGroupBox1: TcxGroupBox;
    cxCheckBox1: TcxCheckBox;
    cxCheckBox2: TcxCheckBox;
    cxCheckBox3: TcxCheckBox;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxComboBox1: TcxComboBox;
    cxSpinEdit1: TcxSpinEdit;
    cxLabel3: TcxLabel;
    cxSpinEdit2: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure cxComboBox1PropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox1PropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox3PropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox2PropertiesEditValueChanged(Sender: TObject);
    procedure cxSpinEdit1PropertiesEditValueChanged(Sender: TObject);
    procedure cxSpinEdit2PropertiesEditValueChanged(Sender: TObject);
  private
    { Private declarations }
  public
    function RangeControlProperties: TcxSchedulerRangeControlClientProperties;
    { Public declarations }
  end;

var
  RangeControlDemoMainForm: TRangeControlDemoMainForm;

implementation

uses
  Math;

{$R *.dfm}

procedure TRangeControlDemoMainForm.cxCheckBox1PropertiesEditValueChanged(
  Sender: TObject);
begin
  if cxCheckBox1.Checked then
    RangeControlProperties.AutoAdjustments := RangeControlProperties.AutoAdjustments + [aaClient]
  else
    RangeControlProperties.AutoAdjustments := RangeControlProperties.AutoAdjustments - [aaClient];
end;

procedure TRangeControlDemoMainForm.cxCheckBox2PropertiesEditValueChanged(
  Sender: TObject);
begin
  RangeControlProperties.AutoFormatScaleCaptions := cxCheckBox2.Checked;
end;

procedure TRangeControlDemoMainForm.cxCheckBox3PropertiesEditValueChanged(
  Sender: TObject);
begin
  if cxCheckBox3.Checked then
    RangeControlProperties.AutoAdjustments := RangeControlProperties.AutoAdjustments + [aaRangeControl]
  else
    RangeControlProperties.AutoAdjustments := RangeControlProperties.AutoAdjustments - [aaRangeControl];
end;

procedure TRangeControlDemoMainForm.cxComboBox1PropertiesEditValueChanged(
  Sender: TObject);
begin
  RangeControlProperties.DataDisplayType :=  TcxSchedulerRangeControlDataDisplayType(cxComboBox1.ItemIndex);
end;

procedure TRangeControlDemoMainForm.cxSpinEdit1PropertiesEditValueChanged(
  Sender: TObject);
begin
  RangeControlProperties.ThumbnailHeight := cxSpinEdit1.EditingValue;
end;

procedure TRangeControlDemoMainForm.cxSpinEdit2PropertiesEditValueChanged(
  Sender: TObject);
begin
  RangeControlProperties.ScaleIntervalMinWidth := cxSpinEdit2.EditingValue;
end;

procedure TRangeControlDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  Randomize;
  dxRangeControl1.BeginUpdate;
  GenerateRandomEvents(1000, False, SchedulerStorage, EventLabelColors[0]);
  dxRangeControl1.VisibleRangeMinValue := Date - 10;
  dxRangeControl1.VisibleRangeMaxValue := Date + 10;
  dxRangeControl1.EndUpdate;
end;

function TRangeControlDemoMainForm.RangeControlProperties: TcxSchedulerRangeControlClientProperties;
begin
  Result := dxRangeControl1.ClientProperties as TcxSchedulerRangeControlClientProperties;
end;

end.
