unit EditorsInPlaceValidationDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls,
{$IFDEF EXPRESSBARS}
  dxBar, dxStatusBar,
{$ENDIF}
  DemoBasicMain,
  cxContainer, cxEdit, cxTextEdit, cxStyles, cxTL,
  cxMaskEdit, cxCalendar, cxCurrencyEdit, cxMemo, cxInplaceContainer,
  cxDBTL, cxTLData, cxDBLookupComboBox, cxTimeEdit, cxCalc, cxSpinEdit,
  cxImageComboBox, cxDropDownEdit, cxEditRepositoryItems, cxRadioGroup,
  cxImage, cxBlobEdit, cxCheckBox, cxHyperLinkEdit, cxButtonEdit, cxMRUEdit,
  cxGraphics, cxCustomData, cxDataUtils, cxLookAndFeelPainters, cxTLdxBarBuiltInMenu,
  cxRegExpr, cxClasses, dxScreenTip, dxCustomHint, cxHint;

type
  TEditorsInPlaceValidationDemoMainForm = class(TDemoBasicMainForm)
    miValidationOptions: TMenuItem;
    miValidationRaiseException: TMenuItem;
    miValidationShowErrorIcon: TMenuItem;
    miValidationAllowLoseFocus: TMenuItem;
    N1: TMenuItem;
    cxTreeList1: TcxTreeList;
    cxTreeList1ColumnFirstName: TcxTreeListColumn;
    cxTreeList1ColumnLastName: TcxTreeListColumn;
    cxTreeList1ColumnAddress: TcxTreeListColumn;
    cxTreeList1ColumnPhoneNumber: TcxTreeListColumn;
    cxTreeList1ColumnEmail: TcxTreeListColumn;
    icCustomIconList: TcxImageCollection;
    icCustomIcon1: TcxImageCollectionItem;
    cxHintStyleController: TcxHintStyleController;
    dxScreenTipRepository: TdxScreenTipRepository;
    stGrid: TdxScreenTip;
    procedure cxTreeList1ColumnFirstNameValidateDrawValue(
      Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxTreeList1ColumnFirstNamePropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cxTreeList1ColumnLastNameValidateDrawValue(
      Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxTreeList1ColumnLastNamePropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cxTreeList1ColumnAddressValidateDrawValue(
      Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxTreeList1ColumnAddressPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cxTreeList1ColumnPhoneNumberValidateDrawValue(
      Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxTreeList1ColumnPhoneNumberPropertiesValidate(
      Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cxTreeList1ColumnEmailValidateDrawValue(
      Sender: TcxTreeListColumn; ANode: TcxTreeListNode;
      const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxTreeList1ColumnEmailPropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cxHintStyleControllerShowHintEx(Sender: TObject; var Caption,
      HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  private
    function DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoEmailValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoFirstNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoLastNameValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
    function DoPhoneNumberValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
  published
    procedure InitializeEditors(Sender: TObject);
  end;

var
  EditorsInPlaceValidationDemoMainForm: TEditorsInPlaceValidationDemoMainForm;

implementation

uses
  ShellAPI;

{$R *.dfm}

function GetMenuItemChecked(AMenuItem: TObject): Boolean;
begin
{$IFDEF EXPRESSBARS}
  if AMenuItem is TdxBarButton then
    Result := TdxBarButton(AMenuItem).Down
  else
{$ENDIF}
   Result := (AMenuItem as TMenuItem).Checked;
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

  cxTreeList1ColumnFirstName.Properties.ValidationOptions := AValidationOptions;
  cxTreeList1ColumnLastName.Properties.ValidationOptions := AValidationOptions;
  cxTreeList1ColumnAddress.Properties.ValidationOptions := AValidationOptions;
  cxTreeList1ColumnPhoneNumber.Properties.ValidationOptions := AValidationOptions;
  cxTreeList1ColumnEmail.Properties.ValidationOptions := AValidationOptions;
end;

function TEditorsInPlaceValidationDemoMainForm.DoAddressValidate(const AValue: Variant; var AErrorText: TCaption): Boolean;
begin
  Result := VarToStr(AValue) = '';
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
    AErrorText := 'Please enter a valid email';
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

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnFirstNameValidateDrawValue(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant;
  AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoFirstNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnFirstNamePropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoFirstNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnLastNameValidateDrawValue(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant;
  AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoLastNameValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetError;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnLastNamePropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoLastNameValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnAddressValidateDrawValue(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant;
  AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoAddressValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetInfo;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxHintStyleControllerShowHintEx(
  Sender: TObject; var Caption, HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  stGrid.Header.Glyph.Assign(nil);
  if miValidationShowErrorIcon.Checked and (TObject(HintInfo.HintData) is TcxTreeListEditCellViewInfo) then
    case TcxTreeListEditCellViewInfo(HintInfo.HintData).EditViewInfo.ErrorData.ErrorType of
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

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnAddressPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoAddressValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnPhoneNumberValidateDrawValue(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant;
  AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoPhoneNumberValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetWarning;
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnPhoneNumberPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoPhoneNumberValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnEmailValidateDrawValue(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; const AValue: Variant;
  AData: TcxEditValidateInfo);
var
  AErrorText: TCaption;
begin
  if DoEmailValidate(AValue, AErrorText) then
  begin
    AData.ErrorType := eetCustom;
    AData.ErrorIcon.Assign(icCustomIconList.Items[0].Picture.Bitmap);
    AData.ErrorText := AErrorText;
  end;
end;

procedure TEditorsInPlaceValidationDemoMainForm.cxTreeList1ColumnEmailPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := DoEmailValidate(DisplayValue, ErrorText);
end;

procedure TEditorsInPlaceValidationDemoMainForm.FormCreate(
  Sender: TObject);
begin
  inherited;
  InitializeEditors(Self);
end;

end.
