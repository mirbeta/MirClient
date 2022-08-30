unit EditorsInPlaceValidationDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls,
  Dialogs, BaseForm, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxNavigator, cxGridCustomPopupMenu, cxGridPopupMenu, cxGridCustomTableView,
  cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, Menus, StdCtrls, cxTextEdit, cxGridCardView,
  ComCtrls, dxGDIPlusClasses, dxScreenTip, dxCustomHint, cxHint, ImgList, cxDropDownEdit;

type
  TEditorsInPlaceValidationDemoMainForm = class(TfmBaseForm)
    cxGrid: TcxGrid;
    cxGridLevel: TcxGridLevel;
    cxGridPopupMenu1: TcxGridPopupMenu;
    cxGridTableView: TcxGridTableView;
    cxGridTableViewColumnAddress: TcxGridColumn;
    cxGridTableViewColumnEmail: TcxGridColumn;
    cxGridTableViewColumnFirstName: TcxGridColumn;
    cxGridTableViewColumnLastName: TcxGridColumn;
    cxGridTableViewColumnPhoneNumber: TcxGridColumn;
    cxHintStyleController: TcxHintStyleController;
    dxScreenTipRepository: TdxScreenTipRepository;
    miOptions: TMenuItem;
    miValidationAllowLoseFocus: TMenuItem;
    miValidationRaiseException: TMenuItem;
    miValidationShowErrorIcon: TMenuItem;
    stGrid: TdxScreenTip;
    ValidationOptions1: TMenuItem;
    icCustomIconList: TcxImageCollection;
    icCustomIcon1: TcxImageCollectionItem;
    procedure cxGridTableViewColumnAddressPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    procedure cxGridTableViewColumnAddressValidateDrawValue(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxGridTableViewColumnEmailPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    procedure cxGridTableViewColumnEmailValidateDrawValue(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxGridTableViewColumnFirstNamePropertiesValidate(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    procedure cxGridTableViewColumnFirstNameValidateDrawValue(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxGridTableViewColumnLastNamePropertiesValidate(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    procedure cxGridTableViewColumnLastNameValidateDrawValue(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxGridTableViewColumnPhoneNumberPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);
    procedure cxGridTableViewColumnPhoneNumberValidateDrawValue(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxHintStyleControllerShowHintEx(Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure FormCreate(Sender: TObject);
  private
    function DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoEmailValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoFirstNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoLastNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoPhoneNumberValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function GetPersonFullName(ARecord: TcxCustomGridRecord): string;
  published
    procedure InitializeEditors(Sender: TObject);
  end;

var
  EditorsInPlaceValidationDemoMainForm: TEditorsInPlaceValidationDemoMainForm;

implementation

{$R *.dfm}

uses
  cxRegExpr, cxGridRows;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnAddressPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := DoAddressValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnAddressValidateDrawValue(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoAddressValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetInfo;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnEmailPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := DoEmailValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnEmailValidateDrawValue(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
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

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnFirstNamePropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := DoFirstNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnFirstNameValidateDrawValue(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoFirstNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnLastNamePropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := DoLastNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnLastNameValidateDrawValue(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoLastNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnPhoneNumberPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
var
  AFullName: string;
begin
  Error := DoPhoneNumberValidate(DisplayValue, ErrorText);
  AFullName := GetPersonFullName(cxGridTableView.ViewData.EditingRecord);
  if Trim(AFullName) > '' then
    ErrorText := ErrorText + ' for ' + AFullName;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxGridTableViewColumnPhoneNumberValidateDrawValue(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; const AValue: Variant; AData: TcxEditValidateInfo);
var
  AFullName: string;
  AErrorText: TCaption;
begin
  if DoPhoneNumberValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetWarning;
    AFullName := GetPersonFullName(ARecord);
    if Trim(AFullName) > '' then
      AData.ErrorText := AErrorText + ' for ' + AFullName
    else
      AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxHintStyleControllerShowHintEx(Sender: TObject; var Caption,
  HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
  stGrid.Header.Glyph.Assign(nil);
  if GetMenuItemChecked(miValidationShowErrorIcon) and (TObject(HintInfo.HintData) is TcxGridDataCellViewInfo) then
    case TcxGridDataCellViewInfo(HintInfo.HintData).EditViewInfo.ErrorData.ErrorType of
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

procedure TEditorsInPlaceValidationDemoMainForm.InitializeEditors(Sender: TObject);
var
  AValidationOptions: TcxEditValidationOptions;
begin
  AValidationOptions := [];
  if GetMenuItemChecked(miValidationRaiseException) then
    Include(AValidationOptions, evoRaiseException);
  if GetMenuItemChecked(miValidationShowErrorIcon) then
    Include(AValidationOptions, evoShowErrorIcon);
  if GetMenuItemChecked(miValidationAllowLoseFocus) then
    Include(AValidationOptions, evoAllowLoseFocus);

  cxGridTableViewColumnFirstName.Properties.ValidationOptions := AValidationOptions;
  cxGridTableViewColumnLastName.Properties.ValidationOptions := AValidationOptions;
  cxGridTableViewColumnAddress.Properties.ValidationOptions := AValidationOptions;
  cxGridTableViewColumnPhoneNumber.Properties.ValidationOptions := AValidationOptions;
  cxGridTableViewColumnEmail.Properties.ValidationOptions := AValidationOptions;
end;

function TEditorsInPlaceValidationDemoMainForm.DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
begin
  Result := (cxGridTableViewColumnAddress.Properties as TcxComboBoxProperties).Items.IndexOf(VarToStr(AValue)) = -1;
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

function TEditorsInPlaceValidationDemoMainForm.GetPersonFullName(ARecord: TcxCustomGridRecord): string;
var
  AFirstName, ALastName: string;
begin
  AFirstName := VarToStr(ARecord.Values[cxGridTableViewColumnFirstName.Index]);
  ALastName := VarToStr(ARecord.Values[cxGridTableViewColumnLastName.Index]);
  if (Trim(AFirstName) > '') and (Trim(ALastName) > '') then
    Result := Format('%s %s', [AFirstName, ALastName])
  else
    Result := AFirstName + ALastName;
end;

end.
