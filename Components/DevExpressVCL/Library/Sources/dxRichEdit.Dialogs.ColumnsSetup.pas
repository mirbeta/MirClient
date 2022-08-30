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

unit dxRichEdit.Dialogs.ColumnsSetup;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxGraphics, cxControls, cxLookAndFeels, cxEdit,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, dxLayoutContainer, StdCtrls, cxButtons, dxLayoutControl,
  dxLayoutcxEditAdapters, cxContainer, cxScrollBox, cxLabel, cxCheckBox, cxTextEdit, cxMaskEdit, cxSpinEdit,
  dxMeasurementUnitEdit, cxDropDownEdit, dxLayoutLookAndFeels, cxClasses,

  dxRichEdit.Utils.Properties,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.ColumnsSetupFormController;

type
  TdxRichEditColumnsSetupDialogForm = class;

  TOnFinalizeEditor = procedure(ASender: TdxMeasurementUnitEdit) of object;

  { TdxColumnInfoEditHelper }

  TdxColumnInfoEditHelper = class
  private const
    edtIndexWidth = 34;
    edtWidthWidth = 80;
    edtSpacing = 80;
  private
    FOwner: TdxRichEditColumnsSetupDialogForm;
    FEdtIndex: TcxLabel;
    FEdtWidth: TdxMeasurementUnitEdit;
    FEdtSpacing: TdxMeasurementUnitEdit;
    FAllowSpacing: Boolean;
    FColumnInfo: TdxColumnInfoUI;
    FOnWidthChanged: TNotifyEvent;
    FOnSpacingChanged: TNotifyEvent;
    FAllowWidth: Boolean;
    FOnFinalizeEdit: TOnFinalizeEditor;
    procedure SetAllowSpacing(const Value: Boolean);
    procedure SetAllowWidth(const Value: Boolean);
    procedure SetColumnInfo(const Value: TdxColumnInfoUI);
    procedure SetTop(const Value: Integer);
    function GetHeight: Integer;
    function GetTop: Integer;
  protected
    procedure SubscribeControlsEvents;
    procedure UnsubscribeControlsEvents;
    procedure DoWidthChanged;
    procedure DoSpacingChanged;
    procedure FinalizeEdit(AEditor: TdxMeasurementUnitEdit);
    procedure WidthChanged(Sender: TObject);
    procedure SpacingChanged(Sender: TObject);

    property OwnerForm: TdxRichEditColumnsSetupDialogForm read FOwner;
  public
    constructor Create(AOwner: TdxRichEditColumnsSetupDialogForm; AColumnInfo: TdxColumnInfoUI;
      AValueUnitConverter: TdxDocumentModelUnitConverter);
    destructor Destroy; override;
    procedure SetParent;
    function IsValid: Boolean;
    procedure UpdateControl;

    property AllowWidth: Boolean read FAllowWidth write SetAllowWidth;
    property AllowSpacing: Boolean read FAllowSpacing write SetAllowSpacing;
    property ColumnInfo: TdxColumnInfoUI read FColumnInfo write SetColumnInfo;
    property Height: Integer read GetHeight;
    property Top: Integer read GetTop write SetTop;
    property OnWidthChanged: TNotifyEvent read FOnWidthChanged write FOnWidthChanged;
    property OnSpacingChanged: TNotifyEvent read FOnSpacingChanged write FOnSpacingChanged;
    property OnFinalizeEdit: TOnFinalizeEditor read FOnFinalizeEdit write FOnFinalizeEdit;
  end;

  { TdxRichEditColumnsSetupDialogForm }

  TdxRichEditColumnsSetupDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnColumnsPresetLeft: TcxButton;
    btnColumnsPresetOne: TcxButton;
    btnColumnsPresetRight: TcxButton;
    btnColumnsPresetThree: TcxButton;
    btnColumnsPresetTwo: TcxButton;
    btnOk: TcxButton;
    cbApplyTo: TcxComboBox;
    cbEqualColumnWidth: TcxCheckBox;
    cbLineBetween: TcxCheckBox;
    cbStartNewColumn: TcxCheckBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    edtColumnCount: TcxSpinEdit;
    lcgPresets: TdxLayoutGroup;
    lcgWidthSpacing: TdxLayoutGroup;
    lciApplyTo: TdxLayoutItem;
    lciColumnCount: TdxLayoutItem;
    lciColumnsPresetLeft: TdxLayoutItem;
    lciColumnsPresetOne: TdxLayoutItem;
    lciColumnsPresetRight: TdxLayoutItem;
    lciColumnsPresetThree: TdxLayoutItem;
    lciColumnsPresetTwo: TdxLayoutItem;
    lciStartNewColumn: TdxLayoutItem;
    lblColumnNumber: TdxLayoutLabeledItem;
    lblWidth: TdxLayoutLabeledItem;
    lblSpacing: TdxLayoutLabeledItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    lgIndex: TdxLayoutGroup;
    lgWidth: TdxLayoutGroup;
    lgSpacing: TdxLayoutGroup;
    AlignmentConstraint1: TdxLayoutAlignmentConstraint;
    AlignmentConstraint2: TdxLayoutAlignmentConstraint;
    procedure ColumnCountChange(Sender: TObject);
    procedure edtColumnCountKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private const
    PresetControlCount = 5;
  private
    FPresets: array[0..PresetControlCount - 1] of TdxColumnsInfoPreset;
    FPresetControls: array[0..PresetControlCount - 1] of TcxButton;
    FEditors: TdxList<TdxColumnInfoEditHelper>;
    FColumnsInfo: TdxColumnsInfoUI;
    procedure AddApplyToComboItem(AItems: TStrings; AApplyType: TdxSectionPropertiesApplyType);
    procedure PopulateApplyTo;
    procedure ColumnsPresetChecked(Sender: TObject);
    function GetController: TdxColumnsSetupFormController; inline;

    procedure EqualColumnWidthChange(Sender: TObject);
    class function GetCheckBoxState(const AValue: TdxNullableBoolean): TcxCheckBoxState; static;
    class function GetEdtColumnCountValue(const AValue: TdxNullableInteger): Integer; static;
    procedure SetColumnsInfo(const Value: TdxColumnsInfoUI);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure FreePresets;
    function GetUnitConverter: TdxDocumentModelUnitConverter; override;
    procedure InitializeForm; override;

    procedure CommitValuesToController;
    procedure InitPresetControls;
    procedure InitializeEdit(AEditor: TdxMeasurementUnitEdit);
    function IsValid: Boolean;
    procedure SetApplyToComboInitialValue(ACombo: TcxComboBox);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;

    procedure ApplyEditorsAvailability;
    procedure ChangeWidth(Sender: TObject);
    procedure ChangeSpacing(Sender: TObject);
    procedure CreateColumnEditor(AInfo: TdxColumnInfoUI);
    procedure EnableEditors(AFrom: Integer; ATo: Integer; AEnabled: Boolean);
    procedure SubscribeColumnsEditorsEvents;
    procedure SubscribeColumnsEditorEvents(const AControl: TdxColumnInfoEditHelper);
    procedure UnSubscribeColumnsEditorEvents(const AControl: TdxColumnInfoEditHelper);
    procedure UnsubscribeColumnsEditorsEvents;
    procedure UpdateEditorsCount;
    procedure UpdateEditor(AControl: TdxColumnInfoEditHelper; AInfo: TdxColumnInfoUI);
    procedure UpdateColumnsEditors;

    property ColumnsInfo: TdxColumnsInfoUI read FColumnsInfo write SetColumnsInfo;
  public

    destructor Destroy; override;
    property Controller: TdxColumnsSetupFormController read GetController;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils;

{$R *.dfm}

{ TdxRichEditColumnsSetupDialogForm }

destructor TdxRichEditColumnsSetupDialogForm.Destroy;
begin
  FreePresets;
  FEditors.Free;
  inherited Destroy;
end;

function TdxRichEditColumnsSetupDialogForm.GetController: TdxColumnsSetupFormController;
begin
  Result := TdxColumnsSetupFormController(inherited Controller);
end;

procedure TdxRichEditColumnsSetupDialogForm.AddApplyToComboItem(AItems: TStrings;
  AApplyType: TdxSectionPropertiesApplyType);
var
  AText: string;
begin
  AText := cxGetResourceString(dxSectionPropertiesApplyToNames[AApplyType]);
  AddItemValue(AItems, AText, Ord(AApplyType));
end;

procedure TdxRichEditColumnsSetupDialogForm.ApplyEditorsAvailability;
begin
  if FColumnsInfo.ColumnCount.IsNull then
    Exit;
  EnableEditors(0, FColumnsInfo.ColumnCount.Value - 1, False);
  if FColumnsInfo.EqualColumnWidth.IsNull then
    Exit;
  if FColumnsInfo.EqualColumnWidth.Value and (FColumnsInfo.ColumnCount.Value > 0) then
    EnableEditors(0, 0, True)
  else
    EnableEditors(0, FColumnsInfo.ColumnCount.Value - 1, True);
  FEditors[FColumnsInfo.ColumnCount.Value - 1].AllowSpacing := False;
end;

procedure TdxRichEditColumnsSetupDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  cbLineBetween.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogLineBetween);
  lblColumnNumber.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnNumber);
  lblWidth.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogWidth);
  lblSpacing.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogSpacing);
  cbEqualColumnWidth.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogEqualColumnWidth);
  cbStartNewColumn.Caption := cxGetResourceString(@sdxRichEditColumnsSetupDialogStartNewColumn);
  lcgWidthSpacing.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogWidthSpacing);
  lcgPresets.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogPresets);
  lciColumnsPresetOne.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnsPresetOne);
  lciColumnsPresetTwo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnsPresetTwo);
  lciColumnsPresetThree.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnsPresetThree);
  lciColumnsPresetLeft.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnsPresetLeft);
  lciColumnsPresetRight.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnsPresetRight);
  lciColumnCount.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogColumnCount);
  lciApplyTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditColumnsSetupDialogApplyTo);
end;

procedure TdxRichEditColumnsSetupDialogForm.EnableEditors(AFrom, ATo: Integer; AEnabled: Boolean);
var
  I: Integer;
begin
  for I := AFrom to ATo do
  begin
    FEditors[I].AllowWidth := AEnabled;
    FEditors[I].AllowSpacing := AEnabled;
  end;
end;

procedure TdxRichEditColumnsSetupDialogForm.EqualColumnWidthChange(Sender: TObject);
begin
  Controller.SetEqualColumnWidth(cbEqualColumnWidth.Checked);
  UpdateForm;
end;

procedure TdxRichEditColumnsSetupDialogForm.ColumnsPresetChecked(Sender: TObject);
var
  AControl: TComponent;
begin
  UnsubscribeControlsEvents;
  try
    AControl := TComponent(Sender);
    Controller.ApplyPreset(FPresets[AControl.Tag]);
    UpdateFormCore;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditColumnsSetupDialogForm.CommitValuesToController;
var
  AApplyType: Integer;
begin
  if TryGetItemValue(cbApplyTo, AApplyType) then
    Controller.ApplyType := TdxSectionPropertiesApplyType(AApplyType);
end;

procedure TdxRichEditColumnsSetupDialogForm.CreateColumnEditor(AInfo: TdxColumnInfoUI);
var
  AEditor: TdxColumnInfoEditHelper;
begin
  AEditor := TdxColumnInfoEditHelper.Create(Self, AInfo, UnitConverter);
  AEditor.OnFinalizeEdit := RemoveMeasurementUnitEdit;
  InitializeEdit(AEditor.FEdtWidth);
  InitializeEdit(AEditor.FEdtSpacing);
  AEditor.UpdateControl;
  AEditor.SetParent;
  FEditors.Add(AEditor);
end;

function TdxRichEditColumnsSetupDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxColumnsSetupFormController.Create(AControllerParameters as TdxColumnsSetupFormControllerParameters);
end;

procedure TdxRichEditColumnsSetupDialogForm.edtColumnCountKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['e', 'E', '-']) then
    Key := #0;
end;

procedure TdxRichEditColumnsSetupDialogForm.ChangeSpacing(Sender: TObject);
begin
  FColumnsInfo.RecalculateColumnsBySpacingAfterIndex((TdxColumnInfoEditHelper(Sender)).ColumnInfo.Number - 1);
  UpdateColumnsEditors;
end;

procedure TdxRichEditColumnsSetupDialogForm.ChangeWidth(Sender: TObject);
begin
  FColumnsInfo.RecalculateColumnsByWidthAfterIndex((TdxColumnInfoEditHelper(Sender)).ColumnInfo.Number - 1);
  UpdateColumnsEditors;
end;

procedure TdxRichEditColumnsSetupDialogForm.CMDialogChar(var Message: TCMDialogChar);

  function SetFocusFirstItem(AGroup: TdxLayoutGroup): Boolean;
  begin
    Result := AGroup.Count > 0;
    if Result then
    begin
      Result := AGroup.Items[0] is TdxLayoutItem;
      if Result and TWinControl(TdxLayoutItem(AGroup.Items[0]).Control).CanFocus then
        TWinControl(TdxLayoutItem(AGroup.Items[0]).Control).SetFocus;
    end;
  end;

begin
  if (IsAccel(Message.CharCode, lblColumnNumber.Caption) and SetFocusFirstItem(lgIndex)) or
    (IsAccel(Message.CharCode, lblWidth.Caption) and SetFocusFirstItem(lgWidth)) or
    (IsAccel(Message.CharCode, lblSpacing.Caption) and SetFocusFirstItem(lgSpacing)) then
    Message.Result := 1
  else
    inherited;
end;

procedure TdxRichEditColumnsSetupDialogForm.ColumnCountChange(Sender: TObject);
begin
  if (Controller.ColumnsInfo.Columns.Count >= 0) and (Controller.ColumnsInfo.Columns[0].Spacing = 0) then
    Controller.ColumnsInfo.Columns[0].Spacing := TdxColumnsInfoPreset.Spacing;
  Controller.ChangeColumnCount(edtColumnCount.Value);
  UpdateForm;
end;

procedure TdxRichEditColumnsSetupDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    Exit;
  if IsValid then
  begin
    CommitValuesToController;
    Controller.ApplyChanges;
  end
  else
    CanClose := False;
end;

procedure TdxRichEditColumnsSetupDialogForm.FreePresets;
var
  I: Integer;
begin
  for I := Low(FPresets) to High(FPresets) do
    FreeAndNil(FPresets[I]);
end;

class function TdxRichEditColumnsSetupDialogForm.GetCheckBoxState(
  const AValue: TdxNullableBoolean): TcxCheckBoxState;
begin
 if AValue.IsNull then
   Result := TcxCheckBoxState.cbsGrayed
 else
   if AValue.Value then
     Result := TcxCheckBoxState.cbsChecked
   else
     Result := TcxCheckBoxState.cbsUnchecked;
end;

class function TdxRichEditColumnsSetupDialogForm.GetEdtColumnCountValue(const AValue: TdxNullableInteger): Integer;
begin
  if AValue.IsNull then
    Result := 0
  else
    Result := AValue.Value;
end;

function TdxRichEditColumnsSetupDialogForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Controller.ValueUnitConverter;
end;

procedure TdxRichEditColumnsSetupDialogForm.InitializeEdit(AEditor: TdxMeasurementUnitEdit);
begin
  InitializeMeasurementUnitEdit(AEditor, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      0, ModelUnitsToUIUnit(Controller.ColumnsInfo.PageWidth)));
end;

procedure TdxRichEditColumnsSetupDialogForm.InitializeForm;
begin
  FEditors := TdxObjectList<TdxColumnInfoEditHelper>.Create;
  InitPresetControls;
  ColumnsInfo := Controller.ColumnsInfo;

  SetApplyToComboInitialValue(cbApplyTo);
  PopulateApplyTo;
end;

procedure TdxRichEditColumnsSetupDialogForm.InitPresetControls;
begin
  FPresets[0] := TdxSingleColumnsInfoPreset.Create;
  FPresets[1] := TdxTwoUniformColumnsInfoPreset.Create;
  FPresets[2] := TdxThreeUniformColumnsInfoPreset.Create;
  FPresets[3] := TdxLeftNarrowColumnsInfoPreset.Create;
  FPresets[4] := TdxRightNarrowColumnsInfoPreset.Create;
  FPresetControls[0] := btnColumnsPresetOne;
  FPresetControls[1] := btnColumnsPresetTwo;
  FPresetControls[2] := btnColumnsPresetThree;
  FPresetControls[3] := btnColumnsPresetLeft;
  FPresetControls[4] := btnColumnsPresetRight;
end;

function TdxRichEditColumnsSetupDialogForm.IsValid: Boolean;
var
  I: Integer;
  AColumnInfoEditHelper: TdxColumnInfoEditHelper;
begin
  for I := 0 to FEditors.Count - 1 do
  begin
    AColumnInfoEditHelper := FEditors[I];
    if not AColumnInfoEditHelper.IsValid then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxRichEditColumnsSetupDialogForm.PopulateApplyTo;
begin
  Populate(cbApplyTo, procedure (AComboBox: TcxCustomComboBox)
  begin
    AddApplyToComboItem(AComboBox.Properties.Items, TdxSectionPropertiesApplyType.WholeDocument);
    AddApplyToComboItem(AComboBox.Properties.Items, TdxSectionPropertiesApplyType.CurrentSection);
    AddApplyToComboItem(AComboBox.Properties.Items, TdxSectionPropertiesApplyType.SelectedSections);
    AddApplyToComboItem(AComboBox.Properties.Items, TdxSectionPropertiesApplyType.ThisPointForward);
  end);
end;

procedure TdxRichEditColumnsSetupDialogForm.SetApplyToComboInitialValue(ACombo: TcxComboBox);
begin
  UpdateSelectedIndex(ACombo, Ord(Controller.ApplyType));
end;

procedure TdxRichEditColumnsSetupDialogForm.SetColumnsInfo(const Value: TdxColumnsInfoUI);
begin
  FColumnsInfo := Value;
  UpdateColumnsEditors;
end;

procedure TdxRichEditColumnsSetupDialogForm.SubscribeColumnsEditorEvents(const AControl: TdxColumnInfoEditHelper);
begin
  AControl.SubscribeControlsEvents;
  AControl.OnWidthChanged := ChangeWidth;
  AControl.OnSpacingChanged := ChangeSpacing;
end;

procedure TdxRichEditColumnsSetupDialogForm.SubscribeColumnsEditorsEvents;
var
  I: Integer;
begin
  for I := 0 to FEditors.Count - 1 do
    SubscribeColumnsEditorEvents(FEditors[I]);
end;

procedure TdxRichEditColumnsSetupDialogForm.SubscribeControlsEvents;
var
  AControl: TcxButton;
begin
  for AControl in FPresetControls do
    AControl.OnClick := ColumnsPresetChecked;

  edtColumnCount.Properties.OnChange := ColumnCountChange;
  cbEqualColumnWidth.Properties.OnChange := EqualColumnWidthChange;
end;

procedure TdxRichEditColumnsSetupDialogForm.UnSubscribeColumnsEditorEvents(const AControl: TdxColumnInfoEditHelper);
begin
  AControl.UnsubscribeControlsEvents;
  AControl.OnWidthChanged := nil;
  AControl.OnSpacingChanged := nil;
end;

procedure TdxRichEditColumnsSetupDialogForm.UnsubscribeColumnsEditorsEvents;
var
  I: Integer;
begin
  for I := 0 to FEditors.Count - 1 do
    UnSubscribeColumnsEditorEvents(FEditors[I]);
end;

procedure TdxRichEditColumnsSetupDialogForm.UnsubscribeControlsEvents;
var
  AControl: TcxButton;
begin
  for AControl in FPresetControls do
    AControl.OnClick := nil;

  edtColumnCount.Properties.OnChange := nil;
  cbEqualColumnWidth.Properties.OnChange := nil;
end;

procedure TdxRichEditColumnsSetupDialogForm.UpdateColumnsEditors;
var
  AColumns: TdxList<TdxColumnInfoUI>;
  ACount, I: Integer;
begin
  UnSubscribeColumnsEditorsEvents;
  try

    UpdateEditorsCount;
    ApplyEditorsAvailability;

    AColumns := FColumnsInfo.Columns;
    Assert(FEditors.Count = AColumns.Count);
    ACount := AColumns.Count;
    for I := 0 to ACount - 1 do
      UpdateEditor(FEditors[I], AColumns[I]);
  finally
    SubscribeColumnsEditorsEvents;
  end;
end;

procedure TdxRichEditColumnsSetupDialogForm.UpdateEditor(AControl: TdxColumnInfoEditHelper; AInfo: TdxColumnInfoUI);
begin
  AControl.ColumnInfo := AInfo;
end;

procedure TdxRichEditColumnsSetupDialogForm.UpdateEditorsCount;
var
  I: Integer;
begin
  for I := FEditors.Count - 1 downto FColumnsInfo.Columns.Count do
    FEditors.Remove(FEditors[I]);

  for I := FEditors.Count to FColumnsInfo.Columns.Count - 1 do
    CreateColumnEditor(FColumnsInfo.Columns[I]);
end;

procedure TdxRichEditColumnsSetupDialogForm.UpdateFormCore;
var
  I: Integer;
  AIsChecked: Boolean;
begin
  cbEqualColumnWidth.State := GetCheckBoxState(Controller.ColumnsInfo.EqualColumnWidth);
  edtColumnCount.Value := GetEdtColumnCountValue(Controller.ColumnsInfo.ColumnCount);
  UpdateColumnsEditors;

  for I := Low(FPresets) to High(FPresets) do
  begin
    AIsChecked := FPresets[I].MatchTo(Controller.ColumnsInfo);
    FPresetControls[I].SpeedButtonOptions.Down := AIsChecked;
    FPresetControls[I].TabStop := AIsChecked;
  end;
end;

{ TdxColumnInfoEditHelper }

constructor TdxColumnInfoEditHelper.Create(AOwner: TdxRichEditColumnsSetupDialogForm; AColumnInfo: TdxColumnInfoUI;
  AValueUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create;
  FOwner := AOwner;
  FEdtIndex := TcxLabel.Create(AOwner);
  FEdtIndex.AutoSize := False;
  FEdtIndex.Properties.Alignment.Horz := taCenter;
  FEdtIndex.Properties.Alignment.Vert := taVCenter;
  FEdtIndex.Width := edtIndexWidth;
  FEdtIndex.Transparent := False;
  FEdtIndex.Style.TransparentBorder := False;

  FEdtWidth := TdxMeasurementUnitEdit.Create(AOwner);
  FEdtWidth.Width := edtWidthWidth;
  FEdtWidth.Style.TransparentBorder := False;

  FEdtSpacing := TdxMeasurementUnitEdit.Create(AOwner);
  FEdtSpacing.Width := edtSpacing;
  FEdtSpacing.Style.TransparentBorder := False;

  FEdtIndex.Height := FEdtSpacing.Height;

  FAllowWidth := True;
  FAllowSpacing := True;
  FColumnInfo := AColumnInfo;
end;

destructor TdxColumnInfoEditHelper.Destroy;
begin
  FinalizeEdit(FEdtWidth);
  FinalizeEdit(FEdtSpacing);
  FreeAndNil(FEdtIndex);
  FreeAndNil(FEdtWidth);
  FreeAndNil(FEdtSpacing);
  inherited Destroy;
end;

procedure TdxColumnInfoEditHelper.DoSpacingChanged;
begin
  if Assigned(FOnSpacingChanged) then
    FOnSpacingChanged(Self);
end;

procedure TdxColumnInfoEditHelper.DoWidthChanged;
begin
  if Assigned(FOnWidthChanged) then
    FOnWidthChanged(Self);
end;

procedure TdxColumnInfoEditHelper.FinalizeEdit(AEditor: TdxMeasurementUnitEdit);
begin
  if Assigned(FOnFinalizeEdit) then
    FOnFinalizeEdit(AEditor);
end;

function TdxColumnInfoEditHelper.GetHeight: Integer;
begin
  Result := FEdtIndex.Height;
end;

function TdxColumnInfoEditHelper.GetTop: Integer;
begin
  Result := FEdtIndex.Top;
end;

function TdxColumnInfoEditHelper.IsValid: Boolean;
begin
  Result := FEdtWidth.ValidateEdit(True) and FEdtSpacing.ValidateEdit(True);
end;

procedure TdxColumnInfoEditHelper.SetAllowSpacing(const Value: Boolean);
begin
  FAllowSpacing := Value;
  FEdtSpacing.ParentColor := not Value;
  FEdtSpacing.Properties.ReadOnly := not Value;
end;

procedure TdxColumnInfoEditHelper.SetAllowWidth(const Value: Boolean);
begin
  FAllowWidth := Value;
  FEdtWidth.ParentColor := not Value;
  FEdtWidth.Properties.ReadOnly := not Value;
end;

procedure TdxColumnInfoEditHelper.SetColumnInfo(const Value: TdxColumnInfoUI);
begin
  FColumnInfo := Value;
  UpdateControl;
end;

procedure TdxColumnInfoEditHelper.SetParent;
begin
  FOwner.dxLayoutControl1.BeginUpdate;
  try
    FOwner.lgWidth.CreateItemForControl(FEdtWidth);
    FOwner.lgSpacing.CreateItemForControl(FEdtSpacing);
    FOwner.lgIndex.CreateItemForControl(FEdtIndex);
    FEdtIndex.Height := FEdtSpacing.Height;
    FEdtIndex.Style.BorderStyle := ebsSingle;
  finally
    FOwner.dxLayoutControl1.EndUpdate;
  end;
end;

procedure TdxColumnInfoEditHelper.SetTop(const Value: Integer);
begin
  FEdtIndex.Top := Value;
  FEdtWidth.Top := Value;
  FEdtSpacing.Top := Value;
end;

procedure TdxColumnInfoEditHelper.SpacingChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := TdxRichEditColumnsSetupDialogForm(OwnerForm).GetValueFromEditor(FEdtSpacing);
  if not VarIsNull(AValue) then
  begin
    ColumnInfo.Spacing := AValue;
    DoSpacingChanged;
  end;
end;

procedure TdxColumnInfoEditHelper.SubscribeControlsEvents;
begin
  FEdtWidth.Properties.OnChange := WidthChanged;
  FEdtSpacing.Properties.OnChange := SpacingChanged;
end;

procedure TdxColumnInfoEditHelper.UnsubscribeControlsEvents;
begin
  FEdtWidth.Properties.OnChange := nil;
  FEdtSpacing.Properties.OnChange := nil;
end;

procedure TdxColumnInfoEditHelper.UpdateControl;
begin
  UnsubscribeControlsEvents;
  try
    TdxRichEditColumnsSetupDialogForm(OwnerForm).SetValueToEditor(FEdtWidth, ColumnInfo.Width);
    TdxRichEditColumnsSetupDialogForm(OwnerForm).SetValueToEditor(FEdtSpacing, ColumnInfo.Spacing);
    FEdtIndex.Caption := Format('%d:', [ColumnInfo.Number]);
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxColumnInfoEditHelper.WidthChanged(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := TdxRichEditColumnsSetupDialogForm(OwnerForm).GetValueFromEditor(FEdtWidth);
  if not VarIsNull(AValue) then
  begin
    ColumnInfo.Width := AValue;
    DoWidthChanged;
  end;
end;

end.
