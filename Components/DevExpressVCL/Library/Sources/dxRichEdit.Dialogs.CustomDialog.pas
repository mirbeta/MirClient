{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.CustomDialog;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Generics.Defaults, Generics.Collections,
  Controls, Forms, Dialogs, dxForms,
  cxClasses, cxDropDownEdit, cxRadioGroup, cxTextEdit, dxCoreClasses,
  cxSpinEdit, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels,
  dxLayoutContainer, dxLayoutControl, dxMeasurementUnitEdit, dxSpellCheckerCore,

  dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types;

type

  TdxMeasurementType = (mtCustom, mtPoint, mtInch, mtMillimeter, mtCentimeter);

  { TdxItemValueContainer }

  TdxItemValueContainer = class
  private
    FValue: Integer;
  public
    constructor Create(const AValue: Integer);
    property Value: Integer read FValue;
  end;

  { TdxRichEditCustomDialogForm }

  TdxRichEditCustomDialogForm = class(TdxForm, IdxDisabledSpellChecking)
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
  private
    FControl: IdxRichEditControl;
    FController: TdxFormController;
    FItemValueContainerList: TdxObjectList<TdxItemValueContainer>;
    FMeasurementEditors: TDictionary<TdxMeasurementUnitEdit, TdxMeasurementUnitEditHelper>;
    FMeasurementTypes: TObjectDictionary<TdxMeasurementUnitEditHelper, TdxMeasurementType>;
    FUnitType: TdxMeasurementUnit;
    FUnitTypeDescription: string;
  protected
    procedure DoClose(var Action: TCloseAction); override;

    procedure ApplyLocalization; dynamic; abstract;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; virtual; abstract;
    function GetUnitConverter: TdxDocumentModelUnitConverter; virtual;
    class function GetUnitTypeDescription(AUnitType: TdxMeasurementUnit): string;
    procedure Populate(ACombobox: TcxCustomComboBox; AProc: TProc<TcxCustomComboBox>); overload;
    procedure Populate(ARadioGroup: TcxRadioGroup; AProc: TProc<TcxRadioGroup>); overload;
    procedure InitializeForm; dynamic;
    procedure SetLookAndFeel; virtual;
    procedure SetController(AController: TdxFormController);

    procedure AddItemValue(AItems: TStrings; const AText: string; const AValue: Integer);
    function GetItemValueContainer(AComboBox: TcxCustomComboBox): TdxItemValueContainer; inline;
    function TryGetItemValue(AComboBox: TcxCustomComboBox; out AValue: Integer): Boolean;
    procedure UpdateSelectedIndex(AComboBox: TcxCustomComboBox; const AValue: Integer);

    function DoValidate(AEditor: TcxSpinEdit; AMinValue: integer = 0; AMaxValue: integer = 0): Boolean;

    class function IsMetric: Boolean; static;
    class function ListSeparator: Char; inline;

    procedure InitializeMeasurementUnitEdit(AEditor: TdxMeasurementUnitEdit; AType: TdxMeasurementType;
      AHelper: TdxMeasurementUnitEditHelper);
    procedure RemoveMeasurementUnitEdit(AEditor: TdxMeasurementUnitEdit);
    function GetMeasurementUnitEditHelper(AEditor: TdxMeasurementUnitEdit): TdxMeasurementUnitEditHelper;
    function GetValueFromEditor(AEditor: TdxMeasurementUnitEdit; ACorrectRange: Boolean = True): Variant;
    function TryGetValueFromEditor(AEditor: TdxMeasurementUnitEdit; out AValue: Integer; ACorrectRange: Boolean = True): Boolean; overload;
    function InchesToUIUnit(AValue: Single): Single;
    function ModelUnitsToUIUnit(AValue: Single): Single;
    procedure SetValueToEditor(AEditor: TdxMeasurementUnitEdit; const AValue: Variant); overload;
    procedure SetValueToEditor(AEditor: TdxMeasurementUnitEdit; const AValue: TdxNullableInteger); overload;
    class function ToMeasurementType(AUnitType: TdxMeasurementUnit): TdxMeasurementType;
    procedure MeasurementUnitEditIncrementValueHandler(Sender: TObject; AButton: TcxSpinEditButton;
      var AValue: Variant; var AHandled: Boolean);
    procedure MeasurementUnitEditValidateHandler(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);

    procedure SubscribeControlsEvents; virtual;
    procedure UnsubscribeControlsEvents; virtual;
    procedure UpdateFormCore; virtual;
    procedure UpdateForm;

    property Control: IdxRichEditControl read FControl;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
    property UnitType: TdxMeasurementUnit read FUnitType;
    property UnitTypeDescription: string read FUnitTypeDescription;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(AControllerParameters: TdxFormControllerParameters); virtual;
    property Controller: TdxFormController read FController;
  end;

implementation

uses
  Contnrs, dxCore, Math,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Strs,
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

type
  TdxCustomLayoutLookAndFeelAccess = class(TdxCustomLayoutLookAndFeel);
  TdxTouchScrollUIModeManagerAccess = class(TdxTouchScrollUIModeManager);
  TdxHybridScrollbarManagersAccess = class(TdxHybridScrollbarManagers);

{ TdxRichEditTabsCustomDialogForm }

procedure TdxRichEditCustomDialogForm.AddItemValue(AItems: TStrings; const AText: string; const AValue: Integer);
var
  AItemValueContainer: TdxItemValueContainer;
begin
  if FItemValueContainerList = nil then
    FItemValueContainerList := TdxObjectList<TdxItemValueContainer>.Create;
  AItemValueContainer := TdxItemValueContainer.Create(AValue);
  FItemValueContainerList.Add(AItemValueContainer);
  AItems.AddObject(AText, AItemValueContainer);
end;

constructor TdxRichEditCustomDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMeasurementEditors := TDictionary<TdxMeasurementUnitEdit, TdxMeasurementUnitEditHelper>.Create;
  FMeasurementTypes := TObjectDictionary<TdxMeasurementUnitEditHelper, TdxMeasurementType>.Create([doOwnsKeys]);
end;

destructor TdxRichEditCustomDialogForm.Destroy;
begin
  FreeAndNil(FItemValueContainerList);
  FreeAndNil(FMeasurementEditors);
  FreeAndNil(FMeasurementTypes);
  FreeAndNil(FController);
  inherited Destroy;
end;

function TdxRichEditCustomDialogForm.DoValidate(AEditor: TcxSpinEdit; AMinValue, AMaxValue: integer): Boolean;
var
  ANewValue: Integer;
begin
  if not TryStrToInt(AEditor.Text, ANewValue) then
  begin
    AEditor.SetFocus;
    MessageDlg(cxGetResourceString(@sdxRichEditInvalidNumber), mtWarning, [mbOK], 0);
    Exit(False);
  end;
  if AMinValue = 0 then
    AMinValue := Trunc(AEditor.Properties.MinValue);
  if AMaxValue = 0 then
    AMaxValue := Trunc(AEditor.Properties.MaxValue);
  if (AMaxValue <> 0) and ((ANewValue < AMinValue) or (ANewValue > AMaxValue)) then
  begin
    AEditor.SetFocus;
    MessageDlg(Format(cxGetResourceString(@sdxRichEditInvalidSize), [AMinValue, AMaxValue]), mtWarning, [mbOK], 0);
    Result := False;
  end
  else
    Result := True;
end;

function TdxRichEditCustomDialogForm.GetItemValueContainer(AComboBox: TcxCustomComboBox): TdxItemValueContainer;
begin
  Result := TdxItemValueContainer(AComboBox.ItemObject);
end;

function TdxRichEditCustomDialogForm.GetMeasurementUnitEditHelper(
  AEditor: TdxMeasurementUnitEdit): TdxMeasurementUnitEditHelper;
begin
  FMeasurementEditors.TryGetValue(AEditor, Result);
end;

procedure TdxRichEditCustomDialogForm.DoClose(var Action: TCloseAction);
begin
  TdxTouchScrollUIModeManagerAccess.CheckUIVisibility(Handle);
  TdxHybridScrollbarManagersAccess.CheckScrollbarsVisibility(Handle);
  inherited DoClose(Action);
end;

function TdxRichEditCustomDialogForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Control.DocumentModel.UnitConverter;
end;

class function TdxRichEditCustomDialogForm.GetUnitTypeDescription(AUnitType: TdxMeasurementUnit): string;
begin
  case AUnitType of
    TdxMeasurementUnit.Inch:
      Result := cxGetResourceString(@sdxRichEditUnitsInches);
    TdxMeasurementUnit.Millimeter:
      Result := cxGetResourceString(@sdxRichEditUnitsMillimeters);
    TdxMeasurementUnit.Centimeter:
      Result := cxGetResourceString(@sdxRichEditUnitsCentimeters);
    TdxMeasurementUnit.Point:
      Result := cxGetResourceString(@sdxRichEditUnitsPoints);
    else
      Result := ''
  end;
end;

function TdxRichEditCustomDialogForm.GetValueFromEditor(AEditor: TdxMeasurementUnitEdit;
  ACorrectRange: Boolean = True): Variant;
var
  AValue: Variant;
  AHelper: TdxMeasurementUnitEditHelper;
begin
  Result := Null;
  AHelper := GetMeasurementUnitEditHelper(AEditor);
  AValue := AHelper.GetValueFromText(AEditor.Text, ACorrectRange);
  if not VarIsNull(AValue) then
    case FMeasurementTypes[AHelper] of
      TdxMeasurementType.mtCustom:
        Result := AValue;
      TdxMeasurementType.mtPoint:
        Result := Trunc(UnitConverter.PointsToModelUnitsF(AValue));
      TdxMeasurementType.mtInch:
        Result := Trunc(UnitConverter.InchesToModelUnitsF(AValue));
      TdxMeasurementType.mtMillimeter:
        Result := Trunc(UnitConverter.MillimetersToModelUnitsF(AValue));
      TdxMeasurementType.mtCentimeter:
        Result := Trunc(UnitConverter.CentimetersToModelUnitsF(AValue));
    end;
end;

function TdxRichEditCustomDialogForm.InchesToUIUnit(AValue: Single): Single;
var
  AUIUnit: TdxUIUnit;
begin
  AUIUnit := UnitConverter.ToUIUnitF(UnitConverter.InchesToModelUnitsF(AValue), UnitType);
  Result := AUIUnit.Value;
end;

procedure TdxRichEditCustomDialogForm.Initialize(AControllerParameters: TdxFormControllerParameters);
begin
  FController := CreateController(AControllerParameters);
  ApplyLocalization;
  FControl := AControllerParameters.Control;
  FUnitType := Control.InnerControl.UIUnit;
  if FUnitType = TdxMeasurementUnit.Document then
    FUnitType := TdxMeasurementUnit.Inch;
  FUnitTypeDescription := GetUnitTypeDescription(FUnitType);
  SetLookAndFeel;
  InitializeForm;
  UpdateForm;
end;

procedure TdxRichEditCustomDialogForm.Populate(ACombobox: TcxCustomComboBox; AProc: TProc<TcxCustomComboBox>);
var
  ASavedIndex: Integer;
begin
  ACombobox.Properties.Items.BeginUpdate;
  try
    ASavedIndex := ACombobox.ItemIndex;
    ACombobox.Properties.Items.Clear;
    AProc(ACombobox);
    ACombobox.ItemIndex := Max(ASavedIndex, 0);
  finally
    ACombobox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxRichEditCustomDialogForm.InitializeForm;
begin
end;

procedure TdxRichEditCustomDialogForm.InitializeMeasurementUnitEdit(AEditor: TdxMeasurementUnitEdit;
  AType: TdxMeasurementType; AHelper: TdxMeasurementUnitEditHelper);
begin
  FMeasurementTypes.AddOrSetValue(AHelper, AType);
  FMeasurementEditors.AddOrSetValue(AEditor, AHelper);
  AEditor.ActiveProperties.OnIncrementValue := MeasurementUnitEditIncrementValueHandler;
  AEditor.ActiveProperties.OnValidate := MeasurementUnitEditValidateHandler;
  AEditor.ActiveProperties.MinValue := AHelper.MinValue;
  AEditor.ActiveProperties.MaxValue := AHelper.MaxValue;
  AEditor.ActiveProperties.ExceptionOnInvalidInput := True;
end;

class function TdxRichEditCustomDialogForm.IsMetric: Boolean;
begin
{$WARNINGS OFF}
  Result := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, '0') = '0';
{$WARNINGS ON}
end;

class function TdxRichEditCustomDialogForm.ListSeparator: Char;
begin
  Result := FormatSettings.ListSeparator;
end;

procedure TdxRichEditCustomDialogForm.MeasurementUnitEditIncrementValueHandler(Sender: TObject;
  AButton: TcxSpinEditButton; var AValue: Variant; var AHandled: Boolean);
var
  AEditor: TdxMeasurementUnitEdit;
begin
  AEditor := Sender as TdxMeasurementUnitEdit;
  AHandled := GetMeasurementUnitEditHelper(AEditor).IncrementValue(AButton, AValue);
end;

procedure TdxRichEditCustomDialogForm.MeasurementUnitEditValidateHandler(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  AEditor: TdxMeasurementUnitEdit;
  AMeasurementUnitEditHelper: TdxMeasurementUnitEditHelper;
  AMeasurementType: TdxMeasurementType;
  AValue: Variant;
  AFloatValue: Double;
  AMinStr, AMaxStr: string;
begin
  AEditor := TdxMeasurementUnitEdit(Sender);
  AValue := GetValueFromEditor(AEditor, False);
  AMeasurementUnitEditHelper := GetMeasurementUnitEditHelper(AEditor);
  if VarIsNull(AValue) then
    Error := True
  else
  begin
    AMeasurementType := FMeasurementTypes[AMeasurementUnitEditHelper];
    if AMeasurementType = TdxMeasurementType.mtCustom then
      AFloatValue := AValue
    else
      AFloatValue := ModelUnitsToUIUnit(AValue);
    Error := (CompareValue(AFloatValue, AMeasurementUnitEditHelper.MinValue) < 0) or
             (CompareValue(AFloatValue, AMeasurementUnitEditHelper.MaxValue) > 0);
  end;
  if Error then
  begin
    AMinStr := AMeasurementUnitEditHelper.GetTextFromValue(AMeasurementUnitEditHelper.MinValue);
    AMaxStr := AMeasurementUnitEditHelper.GetTextFromValue(AMeasurementUnitEditHelper.MaxValue);
    ErrorText := Format(cxGetResourceString(@sdxRichEditInvalidMeasurementValue), [AMinStr, AMaxStr]);
  end;
end;

function TdxRichEditCustomDialogForm.ModelUnitsToUIUnit(AValue: Single): Single;
var
  AUIUnit: TdxUIUnit;
begin
  AUIUnit := UnitConverter.ToUIUnitF(AValue, UnitType);
  Result := AUIUnit.Value;
end;

procedure TdxRichEditCustomDialogForm.Populate(ARadioGroup: TcxRadioGroup; AProc: TProc<TcxRadioGroup>);
begin
  ARadioGroup.Properties.Items.BeginUpdate;
  try
    ARadioGroup.Properties.Items.Clear;
    AProc(ARadioGroup);
  finally
    ARadioGroup.Properties.Items.EndUpdate;
  end;
end;

procedure TdxRichEditCustomDialogForm.RemoveMeasurementUnitEdit(AEditor: TdxMeasurementUnitEdit);
var
  AHelper: TdxMeasurementUnitEditHelper;
begin
  AHelper := GetMeasurementUnitEditHelper(AEditor);
  FMeasurementTypes.Remove(AHelper);
  FMeasurementEditors.Remove(AEditor);
end;

procedure TdxRichEditCustomDialogForm.SetValueToEditor(AEditor: TdxMeasurementUnitEdit; const AValue: Variant);
var
  AText: string;
  AMeasurementType: TdxMeasurementType;
  AHelper: TdxMeasurementUnitEditHelper;
begin
  AText := '';
  if not VarIsNull(AValue) then
  begin
    AHelper := GetMeasurementUnitEditHelper(AEditor);
    if AHelper <> nil then
    begin
      AMeasurementType := FMeasurementTypes[AHelper];
      case AMeasurementType of
        TdxMeasurementType.mtCustom:
          AText := AHelper.GetTextFromValue(AValue);
        TdxMeasurementType.mtPoint:
          AText := AHelper.GetTextFromValue(UnitConverter.ModelUnitsToPointsF(AValue));
        TdxMeasurementType.mtInch:
          AText := AHelper.GetTextFromValue(UnitConverter.ModelUnitsToInchesF(AValue));
        TdxMeasurementType.mtMillimeter:
          AText := AHelper.GetTextFromValue(UnitConverter.ModelUnitsToMillimetersF(AValue));
        TdxMeasurementType.mtCentimeter:
          AText := AHelper.GetTextFromValue(UnitConverter.ModelUnitsToCentimetersF(AValue));
      end;
    end;
  end;
  AEditor.Text := AText;
end;

procedure TdxRichEditCustomDialogForm.SetController(AController: TdxFormController);
begin
  FreeAndNil(FController);
  FController := AController;
end;

procedure TdxRichEditCustomDialogForm.SetLookAndFeel;
var
  AOwnerLookAndFeel: TcxLookAndFeel;
  I: Integer;
  ACustomLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  if Owner is TdxVCLControl then
    AOwnerLookAndFeel := TdxVCLControl(Owner).LookAndFeel
  else
    if Owner is TdxRichEditCustomDialogForm then
      AOwnerLookAndFeel := TdxCustomLayoutLookAndFeelAccess(TdxRichEditCustomDialogForm(Owner).dxLayoutControl1.LookAndFeel).LookAndFeel
    else
      Exit;
  for I := 0 to dxLayoutLookAndFeelList1.Count - 1 do
  begin
    ACustomLookAndFeel := Safe<TdxLayoutCxLookAndFeel>.Cast(dxLayoutLookAndFeelList1.Items[I]);
    if ACustomLookAndFeel <> nil then
      ACustomLookAndFeel.LookAndFeel.MasterLookAndFeel := AOwnerLookAndFeel;
  end;
end;

procedure TdxRichEditCustomDialogForm.SetValueToEditor(AEditor: TdxMeasurementUnitEdit;
  const AValue: TdxNullableInteger);
var
  AVariant: Variant;
begin
  if AValue.IsNull then
    AVariant := Null
  else
    AVariant := AValue.Value;
  SetValueToEditor(AEditor, AVariant);
end;

procedure TdxRichEditCustomDialogForm.SubscribeControlsEvents;
begin
end;

class function TdxRichEditCustomDialogForm.ToMeasurementType(AUnitType: TdxMeasurementUnit): TdxMeasurementType;
begin
  case AUnitType of
    TdxMeasurementUnit.Inch:
      Result := TdxMeasurementType.mtInch;
    TdxMeasurementUnit.Millimeter:
      Result := TdxMeasurementType.mtMillimeter;
    TdxMeasurementUnit.Centimeter:
      Result := TdxMeasurementType.mtCentimeter;
    TdxMeasurementUnit.Point:
      Result := TdxMeasurementType.mtPoint;
    else
      Result := TdxMeasurementType.mtCustom
  end;
end;

function TdxRichEditCustomDialogForm.TryGetItemValue(AComboBox: TcxCustomComboBox;
  out AValue: Integer): Boolean;
var
  AItemValueContainer: TdxItemValueContainer;
begin
  AItemValueContainer := GetItemValueContainer(AComboBox);
  Result := AItemValueContainer <> nil;
  if Result then
    AValue := AItemValueContainer.Value
  else
    AValue := -1;
end;

function TdxRichEditCustomDialogForm.TryGetValueFromEditor(AEditor: TdxMeasurementUnitEdit; out AValue: Integer;
  ACorrectRange: Boolean): Boolean;
var
  V: Variant;
begin
  V := GetValueFromEditor(AEditor, ACorrectRange);
  Result := VarIsNumeric(V);
  if Result then
  begin
    if VarIsOrdinal(V) then
      AValue := V
    else
      AValue := Trunc(V);
  end
  else
    AValue := 0;
end;

procedure TdxRichEditCustomDialogForm.UnsubscribeControlsEvents;
begin
end;

procedure TdxRichEditCustomDialogForm.UpdateForm;
begin
  UnsubscribeControlsEvents;
  try
    UpdateFormCore;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditCustomDialogForm.UpdateFormCore;
begin
end;

procedure TdxRichEditCustomDialogForm.UpdateSelectedIndex(AComboBox: TcxCustomComboBox;
  const AValue: Integer);
var
  I: Integer;
  AItems: TStrings;
  AItemValueContainer: TdxItemValueContainer;
begin
  if AValue = -1 then
  begin
    AComboBox.EditValue := Null;
    AComboBox.ItemIndex := -1;
  end
  else
  begin
    AComboBox.EditValue := Null;
    AItems := AComboBox.Properties.Items;
    for I := 0 to AItems.Count - 1 do
    begin
      AItemValueContainer := AItems.Objects[I] as TdxItemValueContainer;
      if (AItemValueContainer <> nil) and (AItemValueContainer.Value = AValue) then
      begin
        AComboBox.ItemIndex := I;
        Exit;
      end;
    end;
  end;
end;

{ TdxItemValue<T> }

constructor TdxItemValueContainer.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

end.
