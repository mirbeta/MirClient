unit EditorsValidationDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls,
  Dialogs, StdCtrls, Menus, BaseForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxClasses, cxCheckBox,
  cxButtons, cxLabel, cxDropDownEdit, cxMaskEdit, cxSpinEdit, dxScreenTip, dxCustomHint,
  cxHint, cxGroupBox, ExtCtrls, ImgList, dxToggleSwitch;

type
  TEditorsValidationDemoMainForm = class(TfmBaseForm)
    btValidate: TcxButton;
    cbAddress: TcxComboBox;
    cbValidationAllowLoseFocus: TcxCheckBox;
    cbValidationRaiseException: TcxCheckBox;
    cbValidationShowErrorIcons: TcxCheckBox;
    cxGroupBox1: TcxGroupBox;
    cxHintStyleController: TcxHintStyleController;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel5: TcxLabel;
    cxSpinEdit: TcxSpinEdit;
    dxScreenTipRepository: TdxScreenTipRepository;
    edEMail: TcxTextEdit;
    edNotEmpty: TcxTextEdit;
    edPerson: TcxTextEdit;
    Panel1: TPanel;
    stError: TdxScreenTip;
    stInfo: TdxScreenTip;
    stWarning: TdxScreenTip;
    CustomIconList: TcxImageList;
    stCustom: TdxScreenTip;
    cxLabel6: TcxLabel;
    dxToggleSwitch1: TdxToggleSwitch;
    procedure btValidateClick(Sender: TObject);
    procedure cbAddressPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cxHintStyleControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure cxSpinEditPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure edEMailPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure edNotEmptyPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure edPersonPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure FormShow(Sender: TObject);
    procedure InitializeEditors(Sender: TObject);
    procedure dxToggleSwitch1PropertiesChange(Sender: TObject);
  private
    procedure Validation;
  end;

var
  EditorsValidationDemoMainForm: TEditorsValidationDemoMainForm;

implementation

{$R *.dfm}

uses
  cxVariants, cxRegExpr, dxGDIPlusClasses, dxGDIPlusAPI, dxMessages;

procedure TEditorsValidationDemoMainForm.btValidateClick(Sender: TObject);
begin
  Validation;
end;

procedure TEditorsValidationDemoMainForm.cbAddressPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := cbAddress.ItemIndex < 0;
  ErrorText := 'Please select an address from the list';
end;

procedure TEditorsValidationDemoMainForm.cxHintStyleControllerShowHintEx(Sender: TObject; var Caption, HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  CanShow := HintInfo.HintStr > ' ';
end;

procedure TEditorsValidationDemoMainForm.cxSpinEditPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AValue: Integer;
begin
  AValue := StrToIntDef(VarToStr(DisplayValue), 0);
  Error := (AValue < 1) or (AValue > 100);
  ErrorText := 'Please enter a value between 1 and 100';
end;

procedure TEditorsValidationDemoMainForm.dxToggleSwitch1PropertiesChange(Sender: TObject);
const
  BooleanToLeftToLeftRight: array[Boolean] of TLeftRight = (taLeftJustify, taRightJustify);
begin
  cbAddress.Properties.ValidationErrorIconAlignment := BooleanToLeftToLeftRight[dxToggleSwitch1.Checked];
  cxSpinEdit.Properties.ValidationErrorIconAlignment := BooleanToLeftToLeftRight[dxToggleSwitch1.Checked];
  edEMail.Properties.ValidationErrorIconAlignment := BooleanToLeftToLeftRight[dxToggleSwitch1.Checked];
  edNotEmpty.Properties.ValidationErrorIconAlignment := BooleanToLeftToLeftRight[dxToggleSwitch1.Checked];
  edPerson.Properties.ValidationErrorIconAlignment := BooleanToLeftToLeftRight[dxToggleSwitch1.Checked];
end;

procedure TEditorsValidationDemoMainForm.edEMailPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := Pos('@', VarToStr(DisplayValue)) = 0;
  ErrorText := 'Please enter a valid email address';
end;

procedure TEditorsValidationDemoMainForm.edNotEmptyPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := VarToStr(DisplayValue) = '';
  ErrorText := 'Please enter a value';
end;

procedure TEditorsValidationDemoMainForm.edPersonPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
begin
  Error := (VarToStr(DisplayValue) = '') or not IsTextValid(VarToStr(DisplayValue), '(Dr\. | Mr\. | Mrs\. | Miss | Ms\.) '' '' .+');
  ErrorText := 'Please enter a valid person name';
end;

procedure TEditorsValidationDemoMainForm.FormShow(Sender: TObject);
begin
  InitializeEditors(Self);
  Validation;
end;

procedure TEditorsValidationDemoMainForm.InitializeEditors(Sender: TObject);
var
  ACustomIcon: TBitmap;
  AValidationOptions: TcxEditValidationOptions;
begin
  edEMail.Properties.ErrorIcon.Assign(cxEditWarningIcon);
  cbAddress.Properties.ErrorIcon.Assign(cxEditInfoIcon);

  ACustomIcon := TBitmap.Create;
  try
    CustomIconList.GetImage(0, ACustomIcon);
    edPerson.Properties.ErrorIcon.SetBitmap(ACustomIcon);
  finally
    ACustomIcon.Free;
  end;

  AValidationOptions := [];
  if cbValidationRaiseException.Checked then
    Include(AValidationOptions, evoRaiseException);
  if cbValidationShowErrorIcons.Checked then
    Include(AValidationOptions, evoShowErrorIcon);
  if cbValidationAllowLoseFocus.Checked then
    Include(AValidationOptions, evoAllowLoseFocus);

  cbAddress.Properties.ValidationOptions := AValidationOptions;
  cxSpinEdit.Properties.ValidationOptions := AValidationOptions;
  edEMail.Properties.ValidationOptions := AValidationOptions;
  edNotEmpty.Properties.ValidationOptions := AValidationOptions;
  edPerson.Properties.ValidationOptions := AValidationOptions;
end;

procedure TEditorsValidationDemoMainForm.Validation;
begin
  edNotEmpty.ValidateEdit;
  cxSpinEdit.ValidateEdit;
  edEMail.ValidateEdit;
  cbAddress.ValidateEdit;
  edPerson.ValidateEdit;
end;

end.
