unit EditorsInPlaceValidationDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Controls,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, DemoBasicMain, cxContainer, cxEdit, cxTextEdit, cxStyles, cxMaskEdit,
  cxCalendar, cxCurrencyEdit, cxMemo, cxInplaceContainer, cxDBLookupComboBox,
  cxTimeEdit, cxCalc, cxSpinEdit, cxImageComboBox, cxDropDownEdit, cxEditRepositoryItems,
  cxRadioGroup, cxImage, cxBlobEdit, cxCheckBox, cxHyperLinkEdit, cxButtonEdit, cxMRUEdit,
  cxGraphics, cxCustomData, cxDataUtils, cxVGrid, cxDBVGrid, cxClasses, Variants,
  cxLookAndFeelPainters, dxScreenTip, dxCustomHint, cxHint;

type
  TEditorsInPlaceValidationDemoMainForm = class(TDemoBasicMainForm)
    miValidationAllowLoseFocus: TMenuItem;
    miValidationRaiseException: TMenuItem;
    miValidationShowErrorIcon: TMenuItem;
    ValidationOptions1: TMenuItem;
    miShowEditButtons: TMenuItem;
    miEditBtnsAlways: TMenuItem;
    miEditBtnsFocused: TMenuItem;
    miEditBtnsNever: TMenuItem;
    N1: TMenuItem;
    VerticalGrid: TcxDBVerticalGrid;
    VerticalGridFirstName: TcxDBEditorRow;
    VerticalGridLastName: TcxDBEditorRow;
    VerticalGridAddress: TcxDBEditorRow;
    VerticalGridPhoneNumber: TcxDBEditorRow;
    VerticalGridEmail: TcxDBEditorRow;
    cxHintStyleController: TcxHintStyleController;
    dxScreenTipRepository: TdxScreenTipRepository;
    stGrid: TdxScreenTip;
    icCustomIconList: TcxImageCollection;
    icCustomIcon1: TcxImageCollectionItem;
    procedure miShowEditBtnsClick(Sender: TObject);
    procedure cxHintStyleControllerShowHintEx(Sender: TObject; var Caption,
      HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormCreate(Sender: TObject);
    procedure VerticalGridFirstNamePropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure VerticalGridLastNamePropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure VerticalGridPhoneNumberPropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure VerticalGridAddressPropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure VerticalGridEmailPropertiesValidateDrawValue(
      Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure VerticalGridFirstNameEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure VerticalGridLastNameEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure VerticalGridAddressEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure VerticalGridPhoneNumberEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure VerticalGridEmailEditPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
  private
    function DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoEmailValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoFirstNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoLastNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoPhoneNumberValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function GetPersonFullName(AEditorRowProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): string;
  published
    procedure InitializeEditors(Sender: TObject);
  end;

var
  EditorsInPlaceValidationDemoMainForm: TEditorsInPlaceValidationDemoMainForm;

implementation

uses EditorsInPlaceValidationDemoData, ShellAPI, cxRegExpr, dxMessages;

{$R *.dfm}

procedure TEditorsInPlaceValidationDemoMainForm.cxHintStyleControllerShowHintEx(
  Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  stGrid.Header.Glyph.Assign(nil);
  if miValidationShowErrorIcon.Checked and (TObject(HintInfo.HintData) is TcxRowValueInfo) then
    case TcxRowValueInfo(HintInfo.HintData).EditViewInfo.ErrorData.ErrorType of
      eetError:
        stGrid.Header.Glyph.Assign(cxEditErrorIcon);
      eetWarning:
        stGrid.Header.Glyph.Assign(cxEditWarningIcon);
      eetInfo:
        stGrid.Header.Glyph.Assign(cxEditInfoIcon);
      eetCustom:
        stGrid.Header.Glyph.Assign(icCustomIcon1.Picture.Bitmap);
    end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.FormCreate(Sender: TObject);
begin
  InitializeEditors(Self);
end;

procedure TEditorsInPlaceValidationDemoMainForm.InitializeEditors(
  Sender: TObject);
var
  AValidationOptions: TcxEditValidationOptions;
begin
  AValidationOptions := [];
  if miValidationRaiseException.Checked then
    Include(AValidationOptions, evoRaiseException);
  if miValidationShowErrorIcon.Checked then
    Include(AValidationOptions, evoShowErrorIcon);
  if miValidationAllowLoseFocus.Checked then
    Include(AValidationOptions, evoAllowLoseFocus);

  VerticalGridFirstName.Properties.EditProperties.ValidationOptions := AValidationOptions;
  VerticalGridLastName.Properties.EditProperties.ValidationOptions := AValidationOptions;
  VerticalGridAddress.Properties.EditProperties.ValidationOptions := AValidationOptions;
  VerticalGridPhoneNumber.Properties.EditProperties.ValidationOptions := AValidationOptions;
  VerticalGridEmail.Properties.EditProperties.ValidationOptions := AValidationOptions;
end;

procedure TEditorsInPlaceValidationDemoMainForm.miShowEditBtnsClick(
  Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  VerticalGrid.OptionsView.ShowEditButtons := TcxEditingControlEditShowButtons(TMenuItem(Sender).Tag);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridAddressEditPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoAddressValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridAddressPropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoAddressValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetInfo;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridEmailEditPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoEmailValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridEmailPropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoEmailValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetCustom;
    AData.ErrorIcon.Assign(icCustomIcon1.Picture.Bitmap);
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridFirstNameEditPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoFirstNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridFirstNamePropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoFirstNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridLastNameEditPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoLastNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridLastNamePropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoLastNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridPhoneNumberEditPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoPhoneNumberValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.VerticalGridPhoneNumberPropertiesValidateDrawValue(
  Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer;
  const AValue: Variant; AData: TcxEditValidateInfo);
var
  AFullName: string;
  AErrorText: TCaption;
begin
  if DoPhoneNumberValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetWarning;
    AFullName := GetPersonFullName(Sender, ARecordIndex);
    if Trim(AFullName) > '' then
      AData.ErrorText := AErrorText + ' for ' + AFullName
    else
      AData.ErrorText := AErrorText;
  end;
end;

function TEditorsInPlaceValidationDemoMainForm.DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
begin
  Result := (VerticalGridAddress.Properties.EditProperties as TcxComboBoxProperties).Items.IndexOf(VarToStr(AValue)) = -1;
  if Result then
    AErrorText := 'Please select an address from the list';
end;

function TEditorsInPlaceValidationDemoMainForm.DoEmailValidate(const AValue: Variant;
  var AErrorText: TCaption): Boolean;
var
  S: string;
begin
  S := VarToStr(AValue);
  Result := (S <> '') and not IsTextFullValid(S, '[A-z0-9_-]+@[A-z0-9_-]+\.[A-z0-9_-]+(\.[A-z]+)*');
  if Result then
    AErrorText := 'Please enter a valid email address';
end;

function TEditorsInPlaceValidationDemoMainForm.DoFirstNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
begin
  Result := VarToStr(AValue) = '';
  if Result then
    AErrorText := 'Please enter a value';
end;

function TEditorsInPlaceValidationDemoMainForm.DoLastNameValidate(const AValue: Variant;
  var AErrorText: TCaption): Boolean;
begin
  Result := VarToStr(AValue) = '';
  if Result then
    AErrorText := 'Please enter a value';
end;

function TEditorsInPlaceValidationDemoMainForm.DoPhoneNumberValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
var
  S: string;
begin
  S := VarToStr(AValue);
  Result := (S = '') or not IsTextValid(S, '(\(\d\d\d\)'' '')?\d\d\d-\d\d\d\d');
  if Result then
    AErrorText := 'Please enter a valid phone number';
end;

function TEditorsInPlaceValidationDemoMainForm.GetPersonFullName(AEditorRowProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer): string;
var
  AFirstName, ALastName: string;
begin
  AFirstName := VarToStr(VerticalGridFirstName.Properties.Values[ARecordIndex]);
  ALastName := VarToStr(VerticalGridLastName.Properties.Values[ARecordIndex]);
  if (Trim(AFirstName) > '') and (Trim(ALastName) > '') then
    Result := Format('%s %s', [AFirstName, ALastName])
  else
    Result := AFirstName + ALastName;
end;

end.
