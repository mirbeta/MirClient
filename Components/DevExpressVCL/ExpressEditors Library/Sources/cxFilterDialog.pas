{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxFilterDialog;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, Graphics, ExtCtrls, Forms, StdCtrls, SysUtils,
  dxCore, cxButtonEdit, cxButtons, cxContainer, cxControls, cxDataStorage,
  cxDropDownEdit, cxEdit, cxFilter, cxFilterConsts, cxFilterControlUtils,
  cxLookAndFeels, cxMaskEdit, cxTextEdit, cxLookAndFeelPainters, cxRadioGroup,
  cxGraphics, Menus, cxLabel, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses,
  dxLayoutControl, dxLayoutLookAndFeels, dxForms;

type
  TcxFilterDialogClass = class of TcxFilterDialog;

  TcxFilterDialog = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbOperator1: TcxComboBox;
    cbOperator2: TcxComboBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lblColumnCaption: TcxLabel;
    lblEdit1PlaceHolder: TdxLayoutItem;
    lblEdit2PlaceHolder: TdxLayoutItem;
    lblSeries: TdxLayoutLabeledItem;
    lblSingle: TdxLayoutLabeledItem;
    lblTitle: TdxLayoutLabeledItem;
    rbAnd: TcxRadioButton;
    rbOr: TcxRadioButton;

    procedure cbOperator1Click(Sender: TObject);
    procedure cbOperator2PropertiesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCriteria: TcxFilterCriteria;
    FDisplayValue1: TCaption;
    FDisplayValue2: TCaption;
    FEdit1: TcxCustomEdit;
    FEdit2: TcxCustomEdit;
    FEditProperties: TcxCustomEditProperties;
    FFilterEditHelper: TcxCustomFilterEditHelperClass;
    FItemLink: TObject;
    FValue1: Variant;
    FValue2: Variant;
    FValueTypeClass: TcxValueTypeClass;
  protected
    procedure AddFilterItem(AParent: TcxFilterCriteriaItemList; AComboBox: TcxComboBox; AValue: Variant; ADisplayValue: string);
    procedure CheckWildcardDescriptionVisibility;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetFilterValues(AEdit: TcxCustomEdit; var AValue: Variant; var ADisplayValue: TCaption);
    function GetOperatorComboBox(AEdit: TcxCustomEdit): TcxComboBox; virtual;
    function GetSupportedOperators: TcxFilterControlOperators; virtual;
    procedure InitControls(const ACriteriaItemCaption: string); virtual;
    procedure InitControlValues; virtual;
    procedure InitEdits(AComboBox: TcxComboBox; AEdit: TcxCustomEdit; AItem: TcxFilterCriteriaItem);
    procedure InitLookAndFeel(ALookAndFeel: TcxLookAndFeel); virtual;
    procedure SetEditValidChars(AEdit: TcxCustomEdit);
    procedure ValidateValue(AEdit: TcxCustomEdit; var AValue: Variant);

    property DisplayValue1: TCaption read FDisplayValue1;
    property DisplayValue2: TCaption read FDisplayValue2;
    property Edit1: TcxCustomEdit read FEdit1;
    property Edit2: TcxCustomEdit read FEdit2;
    property FilterEditHelper: TcxCustomFilterEditHelperClass read FFilterEditHelper;
    property SupportedOperators: TcxFilterControlOperators read GetSupportedOperators;
    property Value1: Variant read FValue1;
    property Value2: Variant read FValue2;
  public
    constructor Create(ACriteria: TcxFilterCriteria; AItemLink: TObject;
      AEditProperties: TcxCustomEditProperties; const ACriteriaItemCaption: string;
      AValueTypeClass: TcxValueTypeClass; ALookAndFeel: TcxLookAndFeel; AFont: TFont); reintroduce; virtual;
    procedure ApplyChanges; virtual;
    class function GetFilterEditHelper(AEditProperties: TcxCustomEditProperties): TcxCustomFilterEditHelperClass;

    property Criteria: TcxFilterCriteria read FCriteria;
    property EditProperties: TcxCustomEditProperties read FEditProperties;
    property ItemLink: TObject read FItemLink;
    property ValueTypeClass: TcxValueTypeClass read FValueTypeClass;
  end;

var
  cxFilterDialogClass: TcxFilterDialogClass = TcxFilterDialog;

function IsFilterControlDialogNeeded(ACriteria: TcxFilterCriteria): Boolean;
function ShowFilterDialog(ACriteria: TcxFilterCriteria; AItemLink: TObject; AEditProperties: TcxCustomEditProperties; const ACriteriaItemCaption: string; AValueTypeClass: TcxValueTypeClass;
    ALookAndFeel: TcxLookAndFeel = nil; AFont: TFont = nil; AControl: TControl = nil): Boolean;

implementation

{$R *.dfm}

uses
  cxFilterControlStrs;

function IsFilterControlDialogNeeded(ACriteria: TcxFilterCriteria): Boolean;
const
  SupportedKinds: TcxFilterOperatorKinds = [foEqual..foNotLike, foContains..foEndsWith];
var
  AItemLinks: TList;

  function SimpleList(AList: TcxFilterCriteriaItemList): Boolean;
  var
    I: Integer;
    AItem: TcxCustomFilterCriteriaItem;
    AItemLink: TObject;
  begin
    Result := False;
    with AList do
    begin
      if (Count <> 2) or not (BoolOperatorKind in [fboAnd, fboOr]) then
        Exit;
      for I := 0 to Count - 1 do
      begin
        AItem := Items[I];
        if AItem.IsItemList or not (TcxFilterCriteriaItem(AItem).OperatorKind in SupportedKinds) then
          Exit;
      end;
      AItemLink := TcxFilterCriteriaItem(Items[0]).ItemLink;
      Result := (AItemLink = TcxFilterCriteriaItem(Items[1]).ItemLink) and
        (AItemLinks.IndexOf(AItemLink) = -1);
      if Result then
        AItemLinks.Add(AItemLink);
    end;
  end;

  function SimpleItem(AItem: TcxCustomFilterCriteriaItem): Boolean;
  begin
    if AItem.IsItemList then
      Result := SimpleList(TcxFilterCriteriaItemList(AItem))
    else
      with TcxFilterCriteriaItem(AItem) do
      begin
        Result := (OperatorKind in SupportedKinds) and (AItemLinks.IndexOf(ItemLink) = -1);
        if Result then
          AItemLinks.Add(ItemLink);
      end;
  end;

var
  I: Integer;
begin
  AItemLinks := TList.Create;
  try
    Result := ACriteria.Root.BoolOperatorKind <> fboAnd;
    if not Result then
      for I := 0 to ACriteria.Root.Count - 1 do
      begin
        Result := not SimpleItem(ACriteria.Root.Items[I]);
        if Result then Break;
      end;
  finally
    AItemLinks.Free;
  end;
end;

function ShowFilterDialog(ACriteria: TcxFilterCriteria;
  AItemLink: TObject; AEditProperties: TcxCustomEditProperties;
  const ACriteriaItemCaption: string; AValueTypeClass: TcxValueTypeClass;
  ALookAndFeel: TcxLookAndFeel = nil; AFont: TFont = nil; AControl: TControl = nil): Boolean;
var
  ADialog: TcxFilterDialog;
begin
  Result := False;
  if cxFilterDialogClass.GetFilterEditHelper(AEditProperties) = nil then
    Exit;
  ADialog := cxFilterDialogClass.Create(ACriteria, AItemLink, AEditProperties,
    ACriteriaItemCaption, AValueTypeClass, ALookAndFeel, AFont);
  try
    if AControl <> nil then
      ADialog.BiDiMode := AControl.BiDiMode;
    Result := ADialog.ShowModal = mrOK;
    if Result then ADialog.ApplyChanges;
  finally
    ADialog.Free;
  end;
end;

procedure FillOperatorComboBox(AComboBox: TcxComboBox; AOperators: TcxFilterControlOperators);
var
  AOperator: TcxFilterControlOperator;
begin
  for AOperator := Low(TcxFilterControlOperator) to High(TcxFilterControlOperator) do
    if AOperator in AOperators then
      AComboBox.Properties.Items.AddObject(GetFilterControlOperatorText(AOperator), TObject(AOperator));
end;

function GetOperatorIndex(AComboBox: TcxComboBox; AOperator: TcxFilterControlOperator): Integer;
begin
  Result := AComboBox.Properties.Items.IndexOfObject(TObject(AOperator));
end;

function GetOperator(AComboBox: TcxComboBox): TcxFilterControlOperator;

  function GetStartItemIndex(AComboBox: TcxComboBox): Integer;
  begin
    if AComboBox.Properties.Items[0] = '' then
      Result := 0
    else
      Result := -1;
  end;

begin
  with AComboBox do
    if (ItemIndex = -1) or (ItemIndex = GetStartItemIndex(AComboBox)) then
      Result := fcoNone
    else
      Result := TcxFilterControlOperator(Properties.Items.Objects[ItemIndex])
end;

{ TcxFilterDialog }

constructor TcxFilterDialog.Create(ACriteria: TcxFilterCriteria; AItemLink: TObject;
  AEditProperties: TcxCustomEditProperties; const ACriteriaItemCaption: string;
  AValueTypeClass: TcxValueTypeClass; ALookAndFeel: TcxLookAndFeel; AFont: TFont);
begin
  inherited Create(Application);
  FCriteria := ACriteria;
  FItemLink := AItemLink;
  FEditProperties := AEditProperties;
  FValueTypeClass := AValueTypeClass;
  FFilterEditHelper := GetFilterEditHelper(FEditProperties);

  Caption := cxGetResourceString(@cxSFilterDialogCaption);
  InitControls(ACriteriaItemCaption);
  if ALookAndFeel <> nil then
    InitLookAndFeel(ALookAndFeel);
  DialogApplyFont(Self, AFont, AItemLink);
  InitControlValues;
end;

procedure TcxFilterDialog.AddFilterItem(AParent: TcxFilterCriteriaItemList;
  AComboBox: TcxComboBox; AValue: Variant; ADisplayValue: string);
var
  AOperator: TcxFilterControlOperator;
begin
  AOperator := GetOperator(AComboBox);
  if AOperator = fcoNone then Exit;

  if AOperator in [fcoBlanks, fcoNonBlanks] then
    AValue := Null;
  if VarIsNull(AValue) or (VarIsStr(AValue) and (AValue = '')) then
    ADisplayValue := cxGetResourceString(@cxSFilterBlankCaption);

  Criteria.AddItem(AParent, ItemLink, GetFilterOperatorKind(AOperator), AValue, ADisplayValue);
end;

procedure TcxFilterDialog.CheckWildcardDescriptionVisibility;

  function CheckCombo(AComboBox: TcxComboBox): Boolean;
  begin
    Result := GetOperator(AComboBox) in [fcoLike, fcoNotLike];
  end;

begin
  lblSingle.Visible := CheckCombo(cbOperator1) or CheckCombo(cbOperator2);
  lblSeries.Visible := lblSingle.Visible;
end;

procedure TcxFilterDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
// rollback  CorrectDlgParams(Params);
end;

procedure TcxFilterDialog.GetFilterValues(AEdit: TcxCustomEdit; var AValue: Variant;
  var ADisplayValue: TCaption);
begin
  AValue := Null;
  ADisplayValue := '';
  if GetOperatorComboBox(AEdit).ItemIndex <> -1 then
    FilterEditHelper.GetFilterValue(AEdit, EditProperties, AValue, ADisplayValue);
  ValidateValue(AEdit, AValue);
end;

function TcxFilterDialog.GetOperatorComboBox(AEdit: TcxCustomEdit): TcxComboBox;
begin
  if AEdit = FEdit1 then
    Result := cbOperator1
  else
    Result := cbOperator2;
end;

function TcxFilterDialog.GetSupportedOperators: TcxFilterControlOperators;
begin
  Result := FilterEditHelper.GetSupportedFilterOperators(EditProperties, ValueTypeClass);
  if not Criteria.SupportedLike then
    Result := Result - [fcoLike, fcoNotLike, fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
end;

procedure TcxFilterDialog.InitControls(const ACriteriaItemCaption: string);

  function CreateValueEdit(APlaceHolder: TdxLayoutItem; AOperatorComboBox: TcxComboBox): TcxCustomEdit;
  begin
    Result := FilterEditHelper.GetFilterEdit(EditProperties);
    APlaceHolder.ControlOptions.OriginalWidth := 100;
    APlaceHolder.Control := Result;
  end;

begin
  lblTitle.Caption := cxGetResourceString(@cxSFilterDialogRows);
  lblColumnCaption.Caption := ACriteriaItemCaption + ' ';

  rbAnd.Caption := cxGetResourceString(@cxSFilterDialogOperationAnd);
  rbOr.Caption := cxGetResourceString(@cxSFilterDialogOperationOr);
  lblSingle.Caption := cxGetResourceString(@cxSFilterDialogUse) + ' ' +
    Criteria.UnderscoreWildcard + ' ' + cxGetResourceString(@cxSFilterDialogSingleCharacter);
  lblSeries.Caption := cxGetResourceString(@cxSFilterDialogUse) + ' ' +
    Criteria.PercentWildcard + ' ' + cxGetResourceString(@cxSFilterDialogCharactersSeries);
  btnOK.Caption := cxGetResourceString(@cxSFilterControlDialogActionOkCaption);
  btnCancel.Caption := cxGetResourceString(@cxSFilterControlDialogActionCancelCaption);

  // Operator ComboBoxes
  FillOperatorComboBox(cbOperator1, SupportedOperators);
  with cbOperator1.Properties do
    DropDownRows := Items.Count;
  with cbOperator2.Properties do
  begin
    Items.Assign(cbOperator1.Properties.Items);
    Items.Insert(0, '');
    DropDownRows := Items.Count;
  end;

  // Value Edits
  FEdit1 := CreateValueEdit(lblEdit1PlaceHolder, cbOperator1);
  rbAnd.TabOrder := FEdit1.TabOrder + 1;
  FEdit2 := CreateValueEdit(lblEdit2PlaceHolder, cbOperator2);
end;

procedure TcxFilterDialog.InitControlValues;
var
  AItem: TcxFilterCriteriaItem;
begin
  // default values
  if (fcoLike in SupportedOperators) and (ValueTypeClass <> nil) and ValueTypeClass.IsString then
    cbOperator1.ItemIndex := GetOperatorIndex(cbOperator1, fcoLike)
  else
    if fcoEqual in SupportedOperators then
      cbOperator1.ItemIndex := GetOperatorIndex(cbOperator1, fcoEqual)
    else
      cbOperator1.ItemIndex := 0;
  cbOperator2.ItemIndex := 0;

  // current values
  AItem := Criteria.FindItemByItemLink(ItemLink);
  if AItem <> nil then
  begin
    InitEdits(cbOperator1, FEdit1, AItem);
    if (AItem.Parent <> Criteria.Root) and (AItem.Parent.Items[1] <> nil) then
    begin
      rbAnd.Checked := AItem.Parent.BoolOperatorKind = fboAnd;
      rbOr.Checked := not rbAnd.Checked;
      AItem := TcxFilterCriteriaItem(AItem.Parent.Items[1]);
      InitEdits(cbOperator2, FEdit2, AItem);
    end;
  end;

  SetEditValidChars(FEdit1);
  SetEditValidChars(FEdit2);
  CheckWildcardDescriptionVisibility;
end;

procedure TcxFilterDialog.InitEdits(AComboBox: TcxComboBox; AEdit: TcxCustomEdit;
  AItem: TcxFilterCriteriaItem);
var
  AOperator: TcxFilterControlOperator;
begin
  AOperator := GetFilterControlOperator(AItem.OperatorKind, AItem.ValueIsNull(AItem.Value));
  AComboBox.ItemIndex := GetOperatorIndex(AComboBox, AOperator);
  FilterEditHelper.SetFilterValue(AEdit, EditProperties, AItem.Value);
end;

procedure TcxFilterDialog.InitLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  cbOperator1.Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  FEdit1.Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  rbAnd.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  rbOr.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  cbOperator2.Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  FEdit2.Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  btnOK.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  btnCancel.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
end;

procedure TcxFilterDialog.SetEditValidChars(AEdit: TcxCustomEdit);
var
  AWildcardChars: set of AnsiChar;
begin
  AWildcardChars := [AnsiChar(Criteria.UnderscoreWildcard), AnsiChar(Criteria.PercentWildcard)];
  if AEdit is TcxCustomTextEdit then
    with TcxCustomTextEdit(AEdit) do
      if GetOperator(GetOperatorComboBox(AEdit)) in [fcoLike, fcoNotLike,
        fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith] then
        ActiveProperties.ValidChars := ActiveProperties.ValidChars + AWildcardChars
      else
        ActiveProperties.ValidChars := ActiveProperties.ValidChars - AWildcardChars;
end;

procedure TcxFilterDialog.ValidateValue(AEdit: TcxCustomEdit; var AValue: Variant);
begin
  FilterControlValidateValue(AEdit, AValue, GetOperator(GetOperatorComboBox(AEdit)),
    ValueTypeClass, FilterEditHelper);
end;

procedure TcxFilterDialog.ApplyChanges;
var
  AItemList: TcxFilterCriteriaItemList;
begin
  Criteria.BeginUpdate;
  try
    Criteria.Root.BoolOperatorKind := fboAnd;
    Criteria.RemoveItemByItemLink(ItemLink);
    if cbOperator2.ItemIndex <> 0 then
    begin
      AItemList := Criteria.Root.AddItemList(TcxFilterBoolOperatorKind(rbOr.Checked));
      AddFilterItem(AItemList, cbOperator1, FValue1, FDisplayValue1);
      AddFilterItem(AItemList, cbOperator2, FValue2, FDisplayValue2);
    end
    else
      AddFilterItem(nil, cbOperator1, FValue1, FDisplayValue1);
  finally
    Criteria.EndUpdate;
  end;
end;

class function TcxFilterDialog.GetFilterEditHelper(AEditProperties: TcxCustomEditProperties): TcxCustomFilterEditHelperClass;
begin
  Result := FilterEditsController.FindHelper(AEditProperties.ClassType);
end;

procedure TcxFilterDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOK then Exit;

  CanClose := False;

  btnOK.SetFocus;

  GetFilterValues(FEdit1, FValue1, FDisplayValue1);
  GetFilterValues(FEdit2, FValue2, FDisplayValue2);

  CanClose := True;
end;

procedure TcxFilterDialog.cbOperator1Click(Sender: TObject);
begin
  SetEditValidChars(FEdit1);
  CheckWildcardDescriptionVisibility;
end;

procedure TcxFilterDialog.cbOperator2PropertiesChange(Sender: TObject);  //!!!
begin
  SetEditValidChars(FEdit2);
  CheckWildcardDescriptionVisibility;
end;

end.
