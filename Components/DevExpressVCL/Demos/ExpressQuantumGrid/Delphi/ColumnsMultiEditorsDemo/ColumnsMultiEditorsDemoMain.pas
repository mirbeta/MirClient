unit ColumnsMultiEditorsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, cxControls, cxGridCustomTableView, cxGridTableView, cxGridCustomView,
  cxLookAndFeels, cxData, cxDBData, cxClasses, cxEditRepositoryItems,
  cxDataStorage, StdCtrls, cxEdit, ActnList, ImgList, Menus, ComCtrls,
  cxGridLevel, cxStyles, cxGraphics, cxCustomData, cxGrid, cxFilter,
  cxLookAndFeelPainters, cxDateUtils, BaseForm;

type
  TColumnsMultiEditorsDemoMainForm = class(TfmBaseForm)   
    miOptions: TMenuItem;
    EditRepository: TcxEditRepository;
    ImageComboLanguages: TcxEditRepositoryImageComboBoxItem;
    ImageComboCommunication: TcxEditRepositoryImageComboBoxItem;
    SpinItemYears: TcxEditRepositorySpinItem;
    DateItemStartWorkFrom: TcxEditRepositoryDateItem;
    Grid: TcxGrid;
    tvSkills: TcxGridTableView;
    clnName: TcxGridColumn;
    clnSkill: TcxGridColumn;
    clnGrade: TcxGridColumn;
    lvSkills: TcxGridLevel;
    miEditButtons: TMenuItem;
    miEditButtonsNever: TMenuItem;
    miEditButtonsForFocusedRecord: TMenuItem;
    miEditButtonsAlways: TMenuItem;
    procedure miAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clnGradeGetProperties(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord;
      var AProperties: TcxCustomEditProperties);
    procedure miEditButtonsAlwaysClick(Sender: TObject);
    procedure miEditButtonsFocusedRecordClick(Sender: TObject);
    procedure miEditButtonsNeverClick(Sender: TObject);
    procedure DateItemStartWorkFromPropertiesGetDayOfWeekState(
      Sender: TObject; ADayOfWeek: TDay; AState: TCustomDrawState;
      AFont: TFont; var ABackgroundColor: TColor);
  private
  end;

var
  ColumnsMultiEditorsDemoMainForm: TColumnsMultiEditorsDemoMainForm;

implementation

{$R *.dfm}

uses
  AboutDemoForm, ColumnsMultiEditorsDemoDS;

procedure TColumnsMultiEditorsDemoMainForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TColumnsMultiEditorsDemoMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TColumnsMultiEditorsDemoMainForm.FormCreate(Sender: TObject);
begin
  tvSkills.BeginUpdate;
  try
    clnSkill.DataBinding.ValueTypeClass := TcxStringValueType;
    clnGrade.DataBinding.ValueTypeClass := TcxVariantValueType;
    clnName.DataBinding.ValueTypeClass := TcxStringValueType;
  finally
    tvSkills.EndUpdate;
  end;
  tvSkills.DataController.CustomDataSource := TSkillDataSource.Create(tvSkills,
        ImageComboLanguages.Properties.Items.Count,
        ImageComboCommunication.Properties.Items.Count);
  tvSkills.DataController.CustomDataSource.DataChanged;
  tvSkills.DataController.Groups.FullExpand;
end;

procedure TColumnsMultiEditorsDemoMainForm.FormDestroy(Sender: TObject);
begin
  tvSkills.DataController.CustomDataSource.Free;
end;

procedure TColumnsMultiEditorsDemoMainForm.clnGradeGetProperties(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
begin
  case ARecord.RecordIndex mod SkillCount of
    0: AProperties := SpinItemYears.Properties;
    1, 2: AProperties := ImageComboLanguages.Properties;
    3: AProperties := ImageComboCommunication.Properties;
    4: AProperties := DateItemStartWorkFrom.Properties;
  end;
end;

procedure TColumnsMultiEditorsDemoMainForm.miEditButtonsAlwaysClick(
  Sender: TObject);
begin
  if tvSkills.OptionsView.ShowEditButtons <> gsebAlways then
    tvSkills.OptionsView.ShowEditButtons := gsebAlways;
end;

procedure TColumnsMultiEditorsDemoMainForm.miEditButtonsFocusedRecordClick(
  Sender: TObject);
begin
  if tvSkills.OptionsView.ShowEditButtons <> gsebForFocusedRecord then
    tvSkills.OptionsView.ShowEditButtons := gsebForFocusedRecord;
end;

procedure TColumnsMultiEditorsDemoMainForm.miEditButtonsNeverClick(
  Sender: TObject);
begin
  if tvSkills.OptionsView.ShowEditButtons <> gsebNever then
    tvSkills.OptionsView.ShowEditButtons := gsebNever;
end;

procedure TColumnsMultiEditorsDemoMainForm.DateItemStartWorkFromPropertiesGetDayOfWeekState(
  Sender: TObject; ADayOfWeek: TDay; AState: TCustomDrawState;
  AFont: TFont; var ABackgroundColor: TColor);
begin
  inherited;
  if ADayOfWeek in [dSaturday, dSunday] then
  begin
    AFont.Color := clRed;
    if not(cdsGrayed in AState) then
      AFont.Style := [fsBold];
  end;
end;

end.
