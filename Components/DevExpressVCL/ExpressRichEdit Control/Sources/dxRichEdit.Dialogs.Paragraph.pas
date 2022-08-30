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

unit dxRichEdit.Dialogs.Paragraph;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, dxLayoutLookAndFeels, cxClasses, ActnList, dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, Menus, dxLayoutControlAdapters,
  StdCtrls, cxButtons, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxSpinEdit, cxCheckBox, dxMeasurementUnitEdit,
  dxRichEdit.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.ParagraphFormController,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Utils.Types;

type

  TdxComboBoxItemIndex = Integer;

  TdxRichEditParagraphDialogForm = class(TdxRichEditCustomDialogForm, IdxFormOwner)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    btnTabs: TcxButton;
    cbContextualSpacing: TcxCheckBox;
    cbKeepLinesTogether: TcxCheckBox;
    cbPageBreakBefore: TcxCheckBox;
    ccbFirstLineIndentType: TcxComboBox;
    ccbLineSpacing: TcxComboBox;
    ccbOutlineLevel: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group10: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    dxLayoutControl1Group7: TdxLayoutGroup;
    dxLayoutControl1Group8: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group9: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutItem;
    dxLayoutControl1Item17: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    edtAlignment: TcxComboBox;
    lbGeneral: TdxLayoutSeparatorItem;
    lbIndentation: TdxLayoutSeparatorItem;
    lbPagination: TdxLayoutSeparatorItem;
    lbSpacing: TdxLayoutSeparatorItem;
    lcgIndentsAndSpacing: TdxLayoutGroup;
    lcgLineAndPageBreaks: TdxLayoutGroup;
    lciAfter: TdxLayoutItem;
    lciAlignment: TdxLayoutItem;
    lciAtSpacing: TdxLayoutItem;
    lciBefore: TdxLayoutItem;
    lciBy: TdxLayoutItem;
    lciLeft: TdxLayoutItem;
    lciLineSpacing: TdxLayoutItem;
    lciOutlineLevel: TdxLayoutItem;
    lciRight: TdxLayoutItem;
    lciSpecial: TdxLayoutItem;
    lciTabs: TdxLayoutItem;
    seAtSpacing: TdxMeasurementUnitEdit;
    seFirstLineIndent: TdxMeasurementUnitEdit;
    seLeftIndent: TdxMeasurementUnitEdit;
    seRightIndent: TdxMeasurementUnitEdit;
    seSpacingAfter: TdxMeasurementUnitEdit;
    seSpacingBefore: TdxMeasurementUnitEdit;
    procedure btnOKClick(Sender: TObject);
    procedure btnTabsClick(Sender: TObject);
    procedure ccbFirstLineIndentTypePropertiesEditValueChanged(Sender: TObject);
    procedure seFirstLineIndentPropertiesPropertiesChange(Sender: TObject);
    procedure seFirstLineIndentPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure ccbLineSpacingPropertiesEditValueChanged(Sender: TObject);
    procedure seAtSpacingPropertiesChange(Sender: TObject);
  private const
      MinMultiplierValue = 0.06;
      MaxMultiplierValue = 132;
      DefaultLineSpacingExactlyValue = 240;
      DefaultLineSpacingMultipleValue = 3.0;
  private
    FLastAtValueExactly: Variant;
    FLastAtValueMultiple: Variant;
    FLastLineSpacingType: TdxNullableParagraphLineSpacing;
    FLockCount: Integer;
    function IsLineSpacingInsignificant(const ALineSpacingType: TdxNullableParagraphLineSpacing): Boolean;
    procedure PopulateAlignment;
    procedure PopulateFirstLineIndent;
    procedure PopulateLineSpacing;
    procedure PopulateOutlineLevel;
    procedure RefreshLineSpacingMeasurementUnit(const ALineSpacingType: TdxNullableParagraphLineSpacing;
      AForceUpdate: Boolean = False);
    function GetController: TdxParagraphFormController;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    function GetUnitConverter: TdxDocumentModelUnitConverter; override;
    procedure InitializeForm; override;

    procedure LoadAlignment;
    procedure LoadContextualSpacing;
    procedure LoadKeepLinesTogether;
    procedure LoadOutlineLevel;
    procedure LoadPageBreakBefore;
    procedure LoadParagraphIndentation;
    procedure LoadParagraphSpacing;
    procedure LoadParagraphPropertiesIntoControls;

    procedure UpdateAlignment;
    procedure UpdateContextualSpacing;
    procedure UpdateKeepLinesTogether;
    procedure UpdateOutlineLevel;
    procedure UpdatePageBreakBefore;
    procedure UpdateParagraphIndentation;
    procedure UpdateParagraphSpacing;
    procedure UpdateParagraphPropertiesFromControls;

    function ValidateFromControls: Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SubscribeEvents;
    procedure UnsubscribeEvents;

    property LastAtValueExactly: Variant read FLastAtValueExactly;
    property LastAtValueMultiple: Variant read FLastAtValueMultiple;
  public
    destructor Destroy; override;
    procedure Initialize(AControllerParameters: TdxFormControllerParameters); override;

    property Controller: TdxParagraphFormController read GetController;
  end;

implementation

uses
  Math,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxMeasurementUnits,
  dxRichEdit.Commands.Dialogs;

{$R *.dfm}

{ TdxRichEditParagraphDialogForm }

destructor TdxRichEditParagraphDialogForm.Destroy;
begin
  inherited Destroy;
end;

procedure TdxRichEditParagraphDialogForm.btnOKClick(Sender: TObject);
begin
  if not ValidateFromControls then
    Exit;
  UpdateParagraphPropertiesFromControls;
  Controller.ApplyChanges;
  ModalResult := mrOk;
end;

procedure TdxRichEditParagraphDialogForm.btnTabsClick(Sender: TObject);
var
  AShowTabsFormCommand: TdxShowTabsFormCommand;
begin
  AShowTabsFormCommand := TdxShowTabsFormCommand.Create(Control, Self);
  try
    AShowTabsFormCommand.Execute;
    Controller.ApplyChanges;
    ModalResult := mrOk;
  finally
    AShowTabsFormCommand.Free;
  end;
end;

procedure TdxRichEditParagraphDialogForm.ccbFirstLineIndentTypePropertiesEditValueChanged(Sender: TObject);
begin
  BeginUpdate;
  try
    if (ccbFirstLineIndentType.ItemIndex <= Ord(TdxParagraphFirstLineIndent.None)) then
      SetValueToEditor(seFirstLineIndent, Null)
    else
    begin
      if seFirstLineIndent.Text = '' then
        SetValueToEditor(seFirstLineIndent, 720);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditParagraphDialogForm.seFirstLineIndentPropertiesPropertiesChange(Sender: TObject);
begin
  BeginUpdate;
  try
    if Trim(seFirstLineIndent.Text) = '' then
      ccbFirstLineIndentType.ItemIndex := Ord(TdxParagraphFirstLineIndent.None)
    else
      if ccbFirstLineIndentType.ItemIndex <= Ord(TdxParagraphFirstLineIndent.None) then
        ccbFirstLineIndentType.ItemIndex := Ord(TdxParagraphFirstLineIndent.Indented);
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditParagraphDialogForm.seFirstLineIndentPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
begin
  if ccbFirstLineIndentType.ItemIndex > Ord(TdxParagraphFirstLineIndent.None) then
    MeasurementUnitEditValidateHandler(Sender, DisplayValue, ErrorText, Error);
end;

procedure TdxRichEditParagraphDialogForm.ccbLineSpacingPropertiesEditValueChanged(Sender: TObject);
var
  V: Variant;
  ALineSpacingType: TdxNullableParagraphLineSpacing;
begin
  BeginUpdate;
  try
    ALineSpacingType.Reset;
    if ccbLineSpacing.ItemIndex >= 0 then
      ALineSpacingType.Value := TdxParagraphLineSpacing(ccbLineSpacing.ItemIndex);

    if not FLastLineSpacingType.IsNull and (FLastLineSpacingType.Value in [TdxParagraphLineSpacing.Multiple,
      TdxParagraphLineSpacing.Exactly, TdxParagraphLineSpacing.AtLeast]) then
    begin
      V := GetValueFromEditor(seAtSpacing);
      if not VarIsNull(V) then
        if FLastLineSpacingType.Value = TdxParagraphLineSpacing.Multiple then
          FLastAtValueMultiple := V
        else
          FLastAtValueExactly := V;
    end;

    RefreshLineSpacingMeasurementUnit(ALineSpacingType);
    if IsLineSpacingInsignificant(ALineSpacingType) then
      SetValueToEditor(seAtSpacing, Null)
    else
      if (seAtSpacing.Text = '') or not IsLineSpacingInsignificant(FLastLineSpacingType) then
        case ALineSpacingType.Value of
          TdxParagraphLineSpacing.Multiple:
            SetValueToEditor(seAtSpacing, LastAtValueMultiple);
          TdxParagraphLineSpacing.Exactly, TdxParagraphLineSpacing.AtLeast:
            SetValueToEditor(seAtSpacing, LastAtValueExactly);
        end;
    FLastLineSpacingType := ALineSpacingType;
  finally
    EndUpdate;
  end;
end;

function TdxRichEditParagraphDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxParagraphFormController.Create(AControllerParameters as TdxParagraphFormControllerParameters);
end;

procedure TdxRichEditParagraphDialogForm.seAtSpacingPropertiesChange(Sender: TObject);
var
  ALineSpacingType: TdxNullableParagraphLineSpacing;
begin
  ALineSpacingType.Reset;
  if ccbLineSpacing.ItemIndex >= 0 then
    ALineSpacingType.Value := TdxParagraphLineSpacing(ccbLineSpacing.ItemIndex);
  if ALineSpacingType.IsNull or not (ALineSpacingType.Value in
    [TdxParagraphLineSpacing.Multiple, TdxParagraphLineSpacing.Exactly, TdxParagraphLineSpacing.AtLeast]) then
    ccbLineSpacing.ItemIndex := Ord(TdxParagraphLineSpacing.Multiple);
end;

procedure TdxRichEditParagraphDialogForm.Initialize(AControllerParameters: TdxFormControllerParameters);
var
  AMinIndent, AMaxIndent: Single;
begin
  inherited Initialize(AControllerParameters);

  AMinIndent := InchesToUIUnit(-22);
  AMaxIndent := InchesToUIUnit(22);
  InitializeMeasurementUnitEdit(seLeftIndent, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, AMinIndent, AMaxIndent));
  InitializeMeasurementUnitEdit(seRightIndent, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, AMinIndent, AMaxIndent));
  InitializeMeasurementUnitEdit(seFirstLineIndent, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, 0, AMaxIndent));

  InitializeMeasurementUnitEdit(seSpacingBefore, TdxMeasurementType.mtPoint,
    TdxMeasurementUnitEditHelper.Create(GetUnitTypeDescription(TdxMeasurementUnit.Point), 6, 2, 0, 1584));
  InitializeMeasurementUnitEdit(seSpacingAfter, TdxMeasurementType.mtPoint,
    TdxMeasurementUnitEditHelper.Create(GetUnitTypeDescription(TdxMeasurementUnit.Point), 6, 2, 0, 1584));
  RefreshLineSpacingMeasurementUnit(Controller.LineSpacingType, True);
  FLastLineSpacingType := Controller.LineSpacingType;

  LoadParagraphPropertiesIntoControls;
end;

procedure TdxRichEditParagraphDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditParagraphDialogForm);
  lbGeneral.Caption := cxGetResourceString(@sdxRichEditParagraphDialogGeneral);
  lbIndentation.Caption := cxGetResourceString(@sdxRichEditParagraphDialogIndentation);
  lbSpacing.Caption := cxGetResourceString(@sdxRichEditParagraphDialogSpacing);
  cbContextualSpacing.Caption := cxGetResourceString(@sdxRichEditParagraphDialogDontAddSpace);
  lbPagination.Caption := cxGetResourceString(@sdxRichEditParagraphDialogPagination);
  cbPageBreakBefore.Caption := cxGetResourceString(@sdxRichEditParagraphDialogPageBreakBefore);
  cbKeepLinesTogether.Caption := cxGetResourceString(@sdxRichEditParagraphDialogKeepLinesTogether);
  btnTabs.Caption := cxGetResourceString(@sdxRichEditParagraphDialogButtonTabs);
  btnOK.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lcgIndentsAndSpacing.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogIndentsAndSpacing);
  lcgLineAndPageBreaks.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogLineAndPageBreaks);
  lciAlignment.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogAlignment);
  lciOutlineLevel.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogOutlineLevel);
  lciLeft.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogLeft);
  lciRight.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogRight);
  lciSpecial.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogSpecial);
  lciBy.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogBy);
  lciBefore.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogBefore);
  lciLineSpacing.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogLineSpacing);
  lciAfter.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogAfter);
  lciAtSpacing.CaptionOptions.Text := cxGetResourceString(@sdxRichEditParagraphDialogAt);

  PopulateAlignment;
  PopulateFirstLineIndent;
  PopulateLineSpacing;
  PopulateOutlineLevel;
end;

function TdxRichEditParagraphDialogForm.GetController: TdxParagraphFormController;
begin
  Result := TdxParagraphFormController(inherited Controller);
end;

function TdxRichEditParagraphDialogForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Controller.UnitConverter;
end;

procedure TdxRichEditParagraphDialogForm.InitializeForm;
begin
  FLastAtValueExactly := DefaultLineSpacingExactlyValue;
  FLastAtValueMultiple := DefaultLineSpacingMultipleValue;

  btnTabs.Visible := Controller.CanEditTabs;
end;

procedure TdxRichEditParagraphDialogForm.LoadAlignment;
begin
  if Controller.Alignment.IsNull then
    edtAlignment.ItemIndex := -1
  else
    edtAlignment.ItemIndex := Ord(Controller.Alignment.Value);
end;

procedure TdxRichEditParagraphDialogForm.LoadContextualSpacing;
begin
  if Controller.ContextualSpacing.IsNull then
  begin
    cbContextualSpacing.Properties.AllowGrayed := True;
    cbContextualSpacing.State := TcxCheckBoxState.cbsGrayed;
  end
  else
    cbContextualSpacing.Checked := Controller.ContextualSpacing.Value;
end;

procedure TdxRichEditParagraphDialogForm.LoadKeepLinesTogether;
begin
  if Controller.KeepLinesTogether.IsNull then
  begin
    cbKeepLinesTogether.Properties.AllowGrayed := True;
    cbKeepLinesTogether.State := TcxCheckBoxState.cbsGrayed;
  end
  else
    cbKeepLinesTogether.Checked := Controller.KeepLinesTogether.Value;
end;

procedure TdxRichEditParagraphDialogForm.LoadOutlineLevel;
var
  ALevel: Integer;
begin
  if not Controller.OutlineLevel.IsNull then
  begin
    ALevel := Controller.OutlineLevel.Value;
    if (ALevel < 0) or (ALevel > 9) then
      ALevel := 0;
    ccbOutlineLevel.ItemIndex := ALevel;
  end
  else
    ccbOutlineLevel.ItemIndex := -1;
end;

procedure TdxRichEditParagraphDialogForm.LoadPageBreakBefore;
begin
  if Controller.PageBreakBefore.IsNull then
  begin
    cbPageBreakBefore.Properties.AllowGrayed := True;
    cbPageBreakBefore.State := TcxCheckBoxState.cbsGrayed;
  end
  else
    cbPageBreakBefore.Checked := Controller.PageBreakBefore.Value;
end;

procedure TdxRichEditParagraphDialogForm.LoadParagraphIndentation;
begin
  BeginUpdate;
  try
    if Controller.LeftIndent.IsNull or (Controller.FirstLineIndentType.IsNull or
      (Controller.FirstLineIndentType.Value = TdxParagraphFirstLineIndent.None)) then
    begin
      if Controller.LeftIndent.IsNull then
        SetValueToEditor(seLeftIndent, Null)
      else
        SetValueToEditor(seLeftIndent, Controller.LeftIndent.Value);
    end
    else
      if not Controller.FirstLineIndent.IsNull and not Controller.FirstLineIndentType.IsNull and
        (Controller.FirstLineIndentType.Value = TdxParagraphFirstLineIndent.Hanging) then
        SetValueToEditor(seLeftIndent, Controller.LeftIndent.Value - Controller.FirstLineIndent.Value)
      else
        SetValueToEditor(seLeftIndent, Controller.LeftIndent.Value);

    if Controller.RightIndent.IsNull then
      SetValueToEditor(seRightIndent, Null)
    else
      SetValueToEditor(seRightIndent, Controller.RightIndent.Value);

    if Controller.FirstLineIndent.IsNull or (not Controller.FirstLineIndentType.IsNull and
      (Controller.FirstLineIndentType.Value = TdxParagraphFirstLineIndent.None)) then
      SetValueToEditor(seFirstLineIndent, Null)
    else
      SetValueToEditor(seFirstLineIndent, Controller.FirstLineIndent.Value);

    if Controller.FirstLineIndentType.IsNull then
      ccbFirstLineIndentType.ItemIndex := -1
    else
      ccbFirstLineIndentType.ItemIndex := Ord(Controller.FirstLineIndentType.Value);
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditParagraphDialogForm.LoadParagraphSpacing;
begin
  BeginUpdate;
  try
    if Controller.SpacingAfter.IsNull then
      SetValueToEditor(seSpacingAfter, Null)
    else
      SetValueToEditor(seSpacingAfter, Controller.SpacingAfter.Value);

    if Controller.SpacingBefore.IsNull then
      SetValueToEditor(seSpacingBefore, Null)
    else
      SetValueToEditor(seSpacingBefore, Controller.SpacingBefore.Value);

    if Controller.LineSpacingType.IsNull then
      ccbLineSpacing.ItemIndex := -1
    else
      ccbLineSpacing.ItemIndex := Ord(Controller.LineSpacingType.Value);
    RefreshLineSpacingMeasurementUnit(Controller.LineSpacingType);

    if Controller.LineSpacing.IsNull or IsLineSpacingInsignificant(Controller.LineSpacingType) then
      SetValueToEditor(seAtSpacing, Null)
    else
      SetValueToEditor(seAtSpacing, Controller.LineSpacing.Value);
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditParagraphDialogForm.LoadParagraphPropertiesIntoControls;
begin
  BeginUpdate;
  try
    LoadParagraphIndentation;
    LoadParagraphSpacing;
    LoadAlignment;
    LoadOutlineLevel;
    LoadKeepLinesTogether;
    LoadPageBreakBefore;
    LoadContextualSpacing;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditParagraphDialogForm.UpdateAlignment;
begin
  if edtAlignment.ItemIndex >= 0 then
    Controller.Alignment.Value := TdxParagraphAlignment(edtAlignment.ItemIndex)
  else
    Controller.Alignment.Reset
end;

procedure TdxRichEditParagraphDialogForm.UpdateContextualSpacing;
begin
  if not cbContextualSpacing.Properties.AllowGrayed or (cbContextualSpacing.State <> TcxCheckBoxState.cbsGrayed) then
    Controller.ContextualSpacing.Value := cbContextualSpacing.Checked;
end;

procedure TdxRichEditParagraphDialogForm.UpdateKeepLinesTogether;
begin
  if not cbKeepLinesTogether.Properties.AllowGrayed or (cbKeepLinesTogether.State <> TcxCheckBoxState.cbsGrayed) then
    Controller.KeepLinesTogether.Value := cbKeepLinesTogether.Checked;
end;

procedure TdxRichEditParagraphDialogForm.UpdateOutlineLevel;
begin
  if ccbOutlineLevel.ItemIndex >= 0 then
    Controller.OutlineLevel.Value := ccbOutlineLevel.ItemIndex
  else
    Controller.OutlineLevel.Reset;
end;

procedure TdxRichEditParagraphDialogForm.UpdatePageBreakBefore;
begin
  if not cbPageBreakBefore.Properties.AllowGrayed or (cbPageBreakBefore.State <> TcxCheckBoxState.cbsGrayed) then
    Controller.PageBreakBefore.Value := cbPageBreakBefore.Checked;
end;

procedure TdxRichEditParagraphDialogForm.UpdateParagraphIndentation;

  procedure UpdateLeftIndent;
  var
    AValue: Variant;
  begin
    AValue := GetValueFromEditor(seLeftIndent);
    if VarIsNull(AValue) then
      Controller.LeftIndent.Reset
    else
      Controller.LeftIndent.Value := AValue;
  end;

  procedure UpdateRightIndent;
  var
    AValue: Variant;
  begin
    AValue := GetValueFromEditor(seRightIndent);
    if VarIsNull(AValue) then
      Controller.RightIndent.Reset
    else
      Controller.RightIndent.Value := AValue;
  end;

  procedure UpdateFirstLineIndentType;
  var
    AItemIndex: Integer;
  begin
    AItemIndex := ccbFirstLineIndentType.ItemIndex;
    if AItemIndex >= 0 then
      Controller.FirstLineIndentType.Value := TdxParagraphFirstLineIndent(AItemIndex)
    else
      Controller.FirstLineIndentType.Reset
  end;

  procedure UpdateFirstLineIndent;
  var
    AValue: Variant;
  begin
    AValue := GetValueFromEditor(seFirstLineIndent);
    if VarIsNull(AValue) or Controller.FirstLineIndentType.IsNull or
      (Controller.FirstLineIndentType.Value = TdxParagraphFirstLineIndent.None) then
      Controller.FirstLineIndent.Reset
    else
      Controller.FirstLineIndent.Value := AValue;
  end;

begin
  UpdateLeftIndent;
  UpdateRightIndent;
  UpdateFirstLineIndentType;
  UpdateFirstLineIndent;

  if not Controller.FirstLineIndent.IsNull and (Controller.FirstLineIndent.Value = 0) then
    Controller.FirstLineIndentType.Value := TdxParagraphFirstLineIndent.None;

  if not Controller.FirstLineIndentType.IsNull and
    (Controller.FirstLineIndentType.Value = TdxParagraphFirstLineIndent.Hanging) and
    not Controller.FirstLineIndent.IsNull and not Controller.LeftIndent.IsNull then
      Controller.LeftIndent.Value := Controller.LeftIndent.Value + Controller.FirstLineIndent.Value;
end;

procedure TdxRichEditParagraphDialogForm.UpdateParagraphSpacing;

  procedure UpdateSpacingBefore;
  var
    V: Variant;
  begin
    V := GetValueFromEditor(seSpacingBefore);
    if VarIsNull(V) then
      Controller.SpacingBefore.Reset
    else
      Controller.SpacingBefore.Value := V;
  end;

  procedure UpdateSpacingAfter;
  var
    V: Variant;
  begin
    V := GetValueFromEditor(seSpacingAfter);
    if VarIsNull(V) then
      Controller.SpacingAfter.Reset
    else
      Controller.SpacingAfter.Value := V;
  end;

  procedure UpdateLineSpacingType;
  var
    AItemIndex: Integer;
  begin
    AItemIndex := ccbLineSpacing.ItemIndex;
    if AItemIndex >= 0 then
      Controller.LineSpacingType.Value := TdxParagraphLineSpacing(AItemIndex)
    else
      Controller.LineSpacingType.Reset;
  end;

  procedure UpdateLineSpacing;
  var
    AValue: Variant;
  begin
    AValue := GetValueFromEditor(seAtSpacing);
    if VarIsNull(AValue) then
      Controller.LineSpacing.Reset
    else
      Controller.LineSpacing.Value := AValue;
  end;

begin
  UpdateSpacingBefore;
  UpdateSpacingAfter;
  UpdateLineSpacingType;
  UpdateLineSpacing;

  if not Controller.LineSpacing.IsNull and not Controller.LineSpacingType.IsNull and
    (Controller.LineSpacingType.Value = TdxParagraphLineSpacing.Multiple) then
  begin
    if SameValue(Controller.LineSpacing.Value, 1) then
      Controller.LineSpacingType.Value := TdxParagraphLineSpacing.Single
    else
      if SameValue(Controller.LineSpacing.Value, 1.5) then
        Controller.LineSpacingType.Value := TdxParagraphLineSpacing.Sesquialteral
      else
        if SameValue(Controller.LineSpacing.Value, 2) then
          Controller.LineSpacingType.Value := TdxParagraphLineSpacing.Double;
  end;
  if not Controller.LineSpacingType.IsNull and not (Controller.LineSpacingType.Value in
    [TdxParagraphLineSpacing.Multiple, TdxParagraphLineSpacing.Exactly, TdxParagraphLineSpacing.AtLeast]) then
    Controller.LineSpacing.Reset;
end;

procedure TdxRichEditParagraphDialogForm.UpdateParagraphPropertiesFromControls;
begin
  UnsubscribeEvents;
  try
    UpdateAlignment;
    UpdateContextualSpacing;
    UpdateKeepLinesTogether;
    UpdatePageBreakBefore;
    UpdateOutlineLevel;
    UpdateParagraphIndentation;
    UpdateParagraphSpacing;
  finally
    SubscribeEvents;
  end;
end;

function TdxRichEditParagraphDialogForm.ValidateFromControls: Boolean;

  function DoValidateEditor(AEditor: TdxMeasurementUnitEdit): Boolean;
  var
    AValue: Variant;
    AHelper: TdxMeasurementUnitEditHelper;
  begin
    Result := (Trim(AEditor.Text) = '');
    if not Result then
    begin
      AValue := GetValueFromEditor(AEditor, False);
      if VarIsNull(AValue) then
      begin
        Result := False;
        MessageDlg(cxGetResourceString(@sdxRichEditInvalidMeasurement), mtWarning, [mbOK], 0);
      end
      else
        if VarIsNull(GetValueFromEditor(AEditor)) then
        begin
          Result := False;
          AHelper := GetMeasurementUnitEditHelper(AEditor);
          MessageDlg(
            Format(cxGetResourceString(@sdxRichEditInvalidMeasurementValue),
              [AHelper.GetTextFromValue(AHelper.MinValue), AHelper.GetTextFromValue(AHelper.MaxValue)]),
            mtWarning, [mbOK], 0);
        end
        else
          Result := True;
    end;
    if not Result then
      AEditor.SetFocus;
  end;

begin
  Result := DoValidateEditor(seLeftIndent) and DoValidateEditor(seRightIndent) and
    DoValidateEditor(seFirstLineIndent) and DoValidateEditor(seSpacingBefore) and DoValidateEditor(seSpacingAfter) and
    DoValidateEditor(seAtSpacing);
end;

procedure TdxRichEditParagraphDialogForm.BeginUpdate;
begin
  if FLockCount = 0 then
    UnsubscribeEvents;
  Inc(FLockCount);
end;

procedure TdxRichEditParagraphDialogForm.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
    SubscribeEvents;
end;

procedure TdxRichEditParagraphDialogForm.SubscribeEvents;
begin
  ccbFirstLineIndentType.Properties.OnEditValueChanged := ccbFirstLineIndentTypePropertiesEditValueChanged;
  seFirstLineIndent.Properties.OnChange := seFirstLineIndentPropertiesPropertiesChange;
  seFirstLineIndent.Properties.OnValidate := seFirstLineIndentPropertiesValidate;
  ccbLineSpacing.Properties.OnEditValueChanged := ccbLineSpacingPropertiesEditValueChanged;
  seAtSpacing.Properties.OnChange := seAtSpacingPropertiesChange;
end;

procedure TdxRichEditParagraphDialogForm.UnsubscribeEvents;
begin
  ccbFirstLineIndentType.Properties.OnChange := nil;
  seFirstLineIndent.Properties.OnChange := nil;
  seFirstLineIndent.Properties.OnValidate := nil;
  ccbLineSpacing.Properties.OnChange := nil;
  seAtSpacing.Properties.OnChange := nil;
end;

function TdxRichEditParagraphDialogForm.IsLineSpacingInsignificant(
  const ALineSpacingType: TdxNullableParagraphLineSpacing): Boolean;
begin
  Result := ALineSpacingType.IsNull or
    (ALineSpacingType.Value = TdxParagraphLineSpacing.Single) or
    (ALineSpacingType.Value = TdxParagraphLineSpacing.Double) or
    (ALineSpacingType.Value = TdxParagraphLineSpacing.Sesquialteral);
end;

procedure TdxRichEditParagraphDialogForm.PopulateAlignment;
begin
  Populate(edtAlignment, procedure(ACombobox: TcxCustomComboBox)
    var
      AAlignment: TdxParagraphAlignment;
    begin
      for AAlignment := Low(TdxParagraphAlignment) to High(TdxParagraphAlignment) do
        ACombobox.Properties.Items.Add(cxGetResourceString(dxParagraphAlignmentNames[AAlignment]));
    end);
end;

procedure TdxRichEditParagraphDialogForm.PopulateFirstLineIndent;
begin
  Populate(ccbFirstLineIndentType, procedure(ACombobox: TcxCustomComboBox)
    var
      AFirstLineIndent: TdxParagraphFirstLineIndent;
    begin
      for AFirstLineIndent := Low(TdxParagraphFirstLineIndent) to High(TdxParagraphFirstLineIndent) do
        ACombobox.Properties.Items.Add(cxGetResourceString(dxParagraphFirstLineIndentNames[AFirstLineIndent]));
    end);
end;

procedure TdxRichEditParagraphDialogForm.PopulateLineSpacing;
begin
  Populate(ccbLineSpacing, procedure(ACombobox: TcxCustomComboBox)
    var
      ALineSpacing: TdxParagraphLineSpacing;
    begin
      for ALineSpacing := Low(TdxParagraphLineSpacing) to High(TdxParagraphLineSpacing) do
        ACombobox.Properties.Items.Add(cxGetResourceString(dxParagraphLineSpacingNames[ALineSpacing]));
    end);
end;

procedure TdxRichEditParagraphDialogForm.PopulateOutlineLevel;
begin
  Populate(ccbOutlineLevel, procedure(ACombobox: TcxCustomComboBox)
    var
      AIndex: Integer;
    begin
      for AIndex := Low(dxParagraphOutlineLevelNames) to High(dxParagraphOutlineLevelNames) do
        ACombobox.Properties.Items.Add(cxGetResourceString(dxParagraphOutlineLevelNames[AIndex]));
    end);
end;

procedure TdxRichEditParagraphDialogForm.RefreshLineSpacingMeasurementUnit(
  const ALineSpacingType: TdxNullableParagraphLineSpacing; AForceUpdate: Boolean = False);
begin
  if not AForceUpdate and (ALineSpacingType.IsNull = FLastLineSpacingType.IsNull) and
    (ALineSpacingType.IsNull or (ALineSpacingType.Value = FLastLineSpacingType.Value)) then
    Exit;
  if not ALineSpacingType.IsNull and
    (ALineSpacingType.Value in [TdxParagraphLineSpacing.Exactly, TdxParagraphLineSpacing.AtLeast]) then
    InitializeMeasurementUnitEdit(seAtSpacing, TdxMeasurementType.mtPoint,
      TdxMeasurementUnitEditHelper.Create(GetUnitTypeDescription(TdxMeasurementUnit.Point), 1, 2, 1, 1584))
  else
    InitializeMeasurementUnitEdit(seAtSpacing, TdxMeasurementType.mtCustom,
      TdxMeasurementUnitEditHelper.Create('', 0.5, 2, MinMultiplierValue, MaxMultiplierValue));
end;

end.

