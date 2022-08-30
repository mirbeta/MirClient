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

unit dxRichEdit.Dialogs.FloatingObjectLayoutForm;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  dxCore, dxCoreClasses, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxSpinEdit, dxMeasurementUnitEdit, cxTextEdit, cxMaskEdit, cxLabel,
  cxDropDownEdit, StdCtrls, cxRadioGroup, cxCheckBox, cxButtons,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.FloatingObjectLayoutFormController,
  dxRichEdit.DocumentModel.FloatingObjectFormatting;

type
  TdxFloatingObjectLayoutDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    btnPresetControlBehind: TcxButton;
    btnPresetControlInFrontOf: TcxButton;
    btnPresetControlSquare: TcxButton;
    btnPresetControlThought: TcxButton;
    btnPresetControlTight: TcxButton;
    btnPresetControlTopAndBottom: TcxButton;
    btnReset: TcxButton;
    cbLock: TcxCheckBox;
    cbLockAspectRatio: TcxCheckBox;
    cmbHorizontalAbsolutePositionRightOf: TcxComboBox;
    cmbHorizontalAlignment: TcxComboBox;
    cmbHorizontalPositionType: TcxComboBox;
    cmbVerticalAbsolutePositionBelow: TcxComboBox;
    cmbVerticalAlignment: TcxComboBox;
    cmbVerticalPositionType: TcxComboBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    lblHorizontal: TdxLayoutSeparatorItem;
    lblHorizontalPositionType: TdxLayoutItem;
    lblOriginalSizeHeightValue: TcxLabel;
    lblOriginalSizeWidth: TdxLayoutItem;
    lblOriginalSizeWidthValue: TcxLabel;
    lcgTabControl: TdxLayoutGroup;
    lcgTabPagePosition: TdxLayoutGroup;
    lcgTabPageSize: TdxLayoutGroup;
    lcgTabPageTextWrapping: TdxLayoutGroup;
    lciBottom: TdxLayoutItem;
    lciDistance: TdxLayoutSeparatorItem;
    lciHeight: TdxLayoutSeparatorItem;
    lciHeightAbsolute: TdxLayoutItem;
    lciHorizontalAbsolutePosition: TdxLayoutItem;
    lciLeft: TdxLayoutItem;
    lciOptions: TdxLayoutSeparatorItem;
    lciOriginalSize: TdxLayoutSeparatorItem;
    lciOriginalSizeHeight: TdxLayoutItem;
    lciPresetControlBehind: TdxLayoutItem;
    lciPresetControlInFrontOf: TdxLayoutItem;
    lciPresetControlSquare: TdxLayoutItem;
    lciPresetControlThought: TdxLayoutItem;
    lciPresetControlTight: TdxLayoutItem;
    lciPresetControlTopAndBottom: TdxLayoutItem;
    lciRight: TdxLayoutItem;
    lciRotate: TdxLayoutSeparatorItem;
    lciRotation: TdxLayoutItem;
    lciScale: TdxLayoutSeparatorItem;
    lciTextWrapSideLargestOnly: TdxLayoutItem;
    lciTextWrapSideLeftOnly: TdxLayoutItem;
    lciTextWrapSideRightOnly: TdxLayoutItem;
    lciTop: TdxLayoutItem;
    lciVertical: TdxLayoutSeparatorItem;
    lciVerticalAbsolutePosition: TdxLayoutItem;
    lciVerticalPositionType: TdxLayoutItem;
    lciWidth: TdxLayoutSeparatorItem;
    lciWidthAbsolute: TdxLayoutItem;
    lciWrappingStyle: TdxLayoutSeparatorItem;
    lciWrapText: TdxLayoutSeparatorItem;
    lgHorizontalGroup: TdxLayoutGroup;
    lgTextWrapSide: TdxLayoutAutoCreatedGroup;
    rbHorizontalAbsolutePositionItem: TcxRadioButton;
    rbHorizontalAlignmentItem: TcxRadioButton;
    rbTextWrapSideLargestOnly: TcxRadioButton;
    rbTextWrapSideLeftOnly: TcxRadioButton;
    rbTextWrapSideRightOnly: TcxRadioButton;
    rbVerticalAbsolutePositionItem: TcxRadioButton;
    rbVerticalAlignmentItem: TcxRadioButton;
    rgTextWrapSideBothSides: TcxRadioButton;
    seBottom: TdxMeasurementUnitEdit;
    seHeightAbs: TdxMeasurementUnitEdit;
    seHorizontalAbsolutePosition: TdxMeasurementUnitEdit;
    seLeft: TdxMeasurementUnitEdit;
    seRight: TdxMeasurementUnitEdit;
    seRotation: TdxMeasurementUnitEdit;
    seTop: TdxMeasurementUnitEdit;
    seVerticalAbsolutePosition: TdxMeasurementUnitEdit;
    seWidthAbs: TdxMeasurementUnitEdit;
    procedure btnResetClick(Sender: TObject);
    procedure rbHorizontalAlignmentItemClick(Sender: TObject);
    procedure rbVerticalAlignmentItemClick(Sender: TObject);
    procedure cmbHorizontalAlignmentPropertiesChange(Sender: TObject);
    procedure cmbHorizontalPositionTypePropertiesChange(Sender: TObject);
    procedure cmbHorizontalAbsolutePositionRightOfPropertiesChange(Sender: TObject);
    procedure cmbVerticalAlignmentPropertiesChange(Sender: TObject);
    procedure cmbVerticalPositionTypePropertiesChange(Sender: TObject);
    procedure cmbVerticalAbsolutePositionBelowPropertiesChange(Sender: TObject);
    procedure seHorizontalAbsolutePositionPropertiesChange(Sender: TObject);
    procedure seVerticalAbsolutePositionPropertiesChange(Sender: TObject);
    procedure cbLockPropertiesChange(Sender: TObject);
    procedure btnPresetControlClick(Sender: TObject);
    procedure rgTextWrapSideClick(Sender: TObject);
    procedure seTopPropertiesChange(Sender: TObject);
    procedure seBottomPropertiesChange(Sender: TObject);
    procedure seLeftPropertiesChange(Sender: TObject);
    procedure seRightPropertiesChange(Sender: TObject);
    procedure cbLockAspectRatioClick(Sender: TObject);
    procedure seHeightAbsoluteValueChanged(Sender: TObject);
    procedure seWidthAbsoluteValueChanged(Sender: TObject);
    procedure seRotationPropertiesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private const
    PresetControlCount = 6;
  private type
    TdxPresetControlInfo = record
      Control: TcxButton;
      InfoPreset: TdxTextWrapTypeInfoPreset;
    end;
  private
    FPresetControls: array [0..PresetControlCount - 1] of TdxPresetControlInfo;
    function GetController: TdxFloatingObjectLayoutOptionsFormController; inline;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    procedure SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    function GetUnitConverter: TdxDocumentModelUnitConverter; override;
    class function IndexToTextWrapSide(AValue: Integer): TdxFloatingObjectTextWrapSide; static;

    procedure InitializeForm; override;
    procedure InitializeFormInitPresetControls;
    procedure InitializeFormHorizontalAlignmentComboBox;
    procedure InitializeFormAddItemInHorizontalPositionAlignmentComboBox(AItems: TStrings;
      AHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment);
    procedure InitializeFormFillHorizontalPositionTypeComboBox(ACombobox: TcxCustomComboBox);
    procedure InitializeFormAddItemToHorizontalPositionTypeComboBox(AItems: TStrings;
      AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType);
    procedure InitializeFormFillVerticalAlignmentComboBox;
    procedure InitializeFormAddItemInVerticalPositionAlignmentComboBox(AItems: TStrings;
      AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment);
    procedure InitializeFormFillVerticalPositionTypeComboBox(AComboBox: TcxCustomComboBox;
      AUseVerticalPositionTypeParagraph: Boolean);
    procedure InitializeFormAddItemToVerticalPositionTypeComboBox(AItems: TStrings;
      AVerticalPositionType: TdxFloatingObjectVerticalPositionType);

    procedure UpdateFormCore; override;
    procedure UpdateTabPagePosition;
    procedure UpdateTabPagePositionHorizontalControls;
    procedure UpdateTabPagePositionVerticalControls;
    procedure UpdateTabPagePositionUpdateSelectedIndex(AComboBox: TcxCustomComboBox; const AValue: Integer); overload;
    procedure UpdateTabPagePositionUpdateSelectedIndex(AComboBox: TcxCustomComboBox; const AValue: Integer; const ADefaultValue: Integer); overload;
    procedure UpdateTabPageTextWrapping;
    procedure UpdateTabPageSize;
    procedure UpdateTabPageTextWrappingDistanceControls;
    procedure UpdateRichTextIndentEdit(AEdit: TdxMeasurementUnitEdit; AAllowNegativeValues: Boolean; const AValue: TdxNullableInteger); overload;
    procedure UpdateRichTextIndentEdit(AEdit: TdxMeasurementUnitEdit; AAllowNegativeValues: Boolean; AValue: Integer); overload;

    procedure EnableHorizontalPositionControls(AEnableAlignmentRow, AEnableAbsolutePositionRow: Boolean);
    procedure EnableVerticalPositionControls(AEnableAlignmentRow, AEnableAbsolutePositionRow: Boolean);
    function GetXCoordinate: Integer;
    function GetYCoordinate: Integer;
    function CalculateOffsetX(ANewHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): Integer;
    function CalculateOffsetY(ANewVerticalPositionType: TdxFloatingObjectVerticalPositionType): Integer;
    procedure PresetControlsChecked;
    procedure WrapTextAndDistanceControlsEnabled(AIsHorizontalControlsEnabled, AIsVerticalControlsEnabled: Boolean);

    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    function IsValid: Boolean;

    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide write SetTextWrapSide;
  public
    property Controller: TdxFloatingObjectLayoutOptionsFormController read GetController;
  end;

implementation

uses
  Math, dxTypeHelpers,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Exceptions;

{$R *.dfm}

const
  FloatingObjectSquareTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.Square; IsBehindDocument: False);
  FloatingObjectTightTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.Tight; IsBehindDocument: False);
  FloatingObjectThroughTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.Through; IsBehindDocument: False);
  FloatingObjectTopAndBottomTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.TopAndBottom; IsBehindDocument: False);
  FloatingObjectBehindTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.None; IsBehindDocument: True);
  FloatingObjectInFrontOfTextWrapTypePreset: TdxTextWrapTypeInfoPreset = (TextWrapType: TdxFloatingObjectTextWrapType.None; IsBehindDocument: False);

{ TdxFloatingObjectLayoutFormDialog }

function TdxFloatingObjectLayoutDialogForm.GetController: TdxFloatingObjectLayoutOptionsFormController;
begin
  Result := TdxFloatingObjectLayoutOptionsFormController(inherited Controller);
end;

procedure TdxFloatingObjectLayoutDialogForm.btnPresetControlClick(Sender: TObject);
var
  APreset: TdxTextWrapTypeInfoPreset;
begin
  UnsubscribeControlsEvents;
  try
    APreset := FPresetControls[TComponent(Sender).Tag].InfoPreset;

    if (Controller.TextWrapType = TdxFloatingObjectTextWrapType.Inline) and
      (APreset.TextWrapType <> TdxFloatingObjectTextWrapType.Inline) then
    begin
      Controller.ApplyPreset(APreset);
      UpdateFormCore;
    end
    else
    begin
      Controller.ApplyPreset(APreset);
      PresetControlsChecked;
    end;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.btnResetClick(Sender: TObject);
var
  ARotationValue: Integer;
begin
  UnsubscribeControlsEvents;
  try
    Controller.ResetActualHeight;
    SetValueToEditor(seHeightAbs, Controller.ActualHeight);

    Controller.ResetActualWidth;
    SetValueToEditor(seWidthAbs, Controller.ActualWidth);

    Controller.ResetRotation;
    if Controller.Rotation.HasValue then
      ARotationValue := Controller.Rotation.Value
    else
      ARotationValue := 0;
    SetValueToEditor(seRotation, Controller.DocumentModel.UnitConverter.ModelUnitsToDegree(ARotationValue));
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialog);
  rbHorizontalAbsolutePositionItem.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePositionItem);
  rbHorizontalAlignmentItem.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHorizontalAlignmentItem);
  rbVerticalAbsolutePositionItem.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogVerticalAbsolutePositionItem);
  rbVerticalAlignmentItem.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogVerticalAlignmentItem);
  cbLock.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogLock);
  rgTextWrapSideBothSides.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTextWrapSideBothSides);
  rbTextWrapSideLeftOnly.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTextWrapSideLeftOnly);
  rbTextWrapSideRightOnly.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTextWrapSideRightOnly);
  rbTextWrapSideLargestOnly.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTextWrapSideLargestOnly);
  cbLockAspectRatio.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogLockAspectRatio);
  btnReset.Caption := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogButtonReset);
  btnOK.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lcgTabPagePosition.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTabPagePosition);
  lcgTabPageTextWrapping.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTabPageTextWrapping);
  lcgTabPageSize.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTabPageSize);
  lblHorizontal.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHorizontal);
  lciVertical.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogVertical);
  lblHorizontalPositionType.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHorizontalPositionType);
  lciHorizontalAbsolutePosition.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHorizontalAbsolutePosition);
  lciOptions.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogOptions);
  lciVerticalAbsolutePosition.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogVerticalAbsolutePosition);
  lciVerticalPositionType.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogVerticalPositionType);
  lciWrappingStyle.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogWrappingStyle);
  lciPresetControlSquare.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlSquare);
  lciPresetControlTight.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlTight);
  lciPresetControlThought.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlThought);
  lciPresetControlTopAndBottom.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlTopAndBottom);
  lciPresetControlBehind.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlBehind);
  lciPresetControlInFrontOf.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogPresetControlInFrontOf);
  lciWrapText.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogWrapText);
  lciDistance.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogDistance);
  lciTop.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogTop);
  lciBottom.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogBottom);
  lciLeft.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogLeft);
  lciRight.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogRight);
  lciHeight.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHeight);
  lciHeightAbsolute.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogHeightAbsolute);
  lciWidth.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogWidth);
  lciWidthAbsolute.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogWidthAbsolute);
  lciRotate.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogRotate);
  lciRotation.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogRotation);
  lciScale.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogScale);
  lciOriginalSize.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogOriginalSize);
  lblOriginalSizeWidth.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogOriginalSizeWidth);
  lciOriginalSizeHeight.CaptionOptions.Text := cxGetResourceString(@sdxFloatingObjectLayoutFormDialogOriginalSizeHeight);
end;

function TdxFloatingObjectLayoutDialogForm.CalculateOffsetX(
  ANewHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): Integer;
var
  ACalculator: TdxFloatingObjectHorizontalPositionCalculator;
  X: Integer;
begin
  ACalculator := TdxFloatingObjectHorizontalPositionCalculator.Create(Controller.ToDocumentLayoutUnitConverter);
  try
    X := GetXCoordinate;
    if  X = MinInt then
      Exit(MinInt);
    Result := ACalculator.CalculateFloatingObjectOffsetX(ANewHorizontalPositionType, X, Controller.PlacementInfo);
  finally
    ACalculator.Free;
  end;
end;

function TdxFloatingObjectLayoutDialogForm.CalculateOffsetY(
  ANewVerticalPositionType: TdxFloatingObjectVerticalPositionType): Integer;
var
  ACalculator: TdxFloatingObjectVerticalPositionCalculator;
  Y: Integer;
begin
  ACalculator := TdxFloatingObjectVerticalPositionCalculator.Create(Controller.ToDocumentLayoutUnitConverter);
  try
    Y := GetYCoordinate;
    if  Y = MinInt then
      Exit(MinInt);
    Result := ACalculator.CalculateFloatingObjectOffsetY(ANewVerticalPositionType, Y, Controller.PlacementInfo);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.cbLockAspectRatioClick(Sender: TObject);
begin
  Controller.LockAspectRatio := cbLockAspectRatio.Checked;
  seHeightAbsoluteValueChanged(nil);
  seWidthAbsoluteValueChanged(nil);
end;

procedure TdxFloatingObjectLayoutDialogForm.cbLockPropertiesChange(Sender: TObject);
begin
  Controller.Locked := cbLock.Checked;
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbHorizontalAbsolutePositionRightOfPropertiesChange(Sender: TObject);
var
  AOffsetX: Integer;
  ANewHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  AOffsetX := 0;
  ANewHorizontalPositionType := TdxFloatingObjectHorizontalPositionType(GetItemValueContainer(cmbHorizontalAbsolutePositionRightOf).Value);

  if (ANewHorizontalPositionType <> TdxFloatingObjectHorizontalPositionType.Character) and
    (Controller.HorizontalPositionType <> TdxFloatingObjectHorizontalPositionType.Character) then
    AOffsetX := CalculateOffsetX(ANewHorizontalPositionType);

  Controller.HorizontalPositionType := ANewHorizontalPositionType;

  if AOffsetX <> MinInt then
  begin
    SetValueToEditor(seHorizontalAbsolutePosition, AOffsetX);
    seHorizontalAbsolutePositionPropertiesChange(Self);
  end
  else
    SetValueToEditor(seHorizontalAbsolutePosition, -1);
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbHorizontalAlignmentPropertiesChange(Sender: TObject);
begin
  Controller.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment(GetItemValueContainer(cmbHorizontalAlignment).Value);
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbHorizontalPositionTypePropertiesChange(Sender: TObject);
begin
  Controller.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType(GetItemValueContainer(cmbHorizontalPositionType).Value);
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbVerticalAbsolutePositionBelowPropertiesChange(Sender: TObject);
var
  AOffsetY: Integer;
  ANewVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  AOffsetY := 0;
  ANewVerticalPositionType := TdxFloatingObjectVerticalPositionType(GetItemValueContainer(cmbVerticalAbsolutePositionBelow).Value);

  if (ANewVerticalPositionType <> TdxFloatingObjectVerticalPositionType.Line) and
    (Controller.VerticalPositionType <> TdxFloatingObjectVerticalPositionType.Line) then
    AOffsetY := CalculateOffsetY(ANewVerticalPositionType);

  Controller.VerticalPositionType := ANewVerticalPositionType;

  if AOffsetY <> MinInt then
  begin
    SetValueToEditor(seVerticalAbsolutePosition, AOffsetY);
    seVerticalAbsolutePositionPropertiesChange(Self);
  end
  else
    SetValueToEditor(seVerticalAbsolutePosition, -1);
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbVerticalAlignmentPropertiesChange(Sender: TObject);
begin
  Controller.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment(GetItemValueContainer(cmbVerticalAlignment).Value);
end;

procedure TdxFloatingObjectLayoutDialogForm.cmbVerticalPositionTypePropertiesChange(Sender: TObject);
begin
  Controller.VerticalPositionType := TdxFloatingObjectVerticalPositionType(GetItemValueContainer(cmbVerticalPositionType).Value);
end;

function TdxFloatingObjectLayoutDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxFloatingObjectLayoutOptionsFormController.Create(AControllerParameters as TdxFloatingInlineObjectLayoutOptionsFormControllerParameters);
end;

procedure TdxFloatingObjectLayoutDialogForm.seHorizontalAbsolutePositionPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  Controller.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.None;
  AValue := GetValueFromEditor(seHorizontalAbsolutePosition);
  if not VarIsNull(AValue) then
    Controller.OffsetX := AValue;
end;

procedure TdxFloatingObjectLayoutDialogForm.seVerticalAbsolutePositionPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  Controller.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment.None;
  AValue := GetValueFromEditor(seVerticalAbsolutePosition);
  if not VarIsNull(AValue) then
    Controller.OffsetY := AValue;
end;

function TdxFloatingObjectLayoutDialogForm.GetTextWrapSide: TdxFloatingObjectTextWrapSide;
begin
  if rbTextWrapSideLargestOnly.Checked then
    Result := TdxFloatingObjectTextWrapSide.Largest
  else
    if rbTextWrapSideRightOnly.Checked then
      Result := TdxFloatingObjectTextWrapSide.Right
    else
      if rbTextWrapSideLeftOnly.Checked then
        Result := TdxFloatingObjectTextWrapSide.Left
      else
        Result := TdxFloatingObjectTextWrapSide.Both;
end;

function TdxFloatingObjectLayoutDialogForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Controller.FloatingObjectRichTextIndentEditProperties.ValueUnitConverter;
end;


function TdxFloatingObjectLayoutDialogForm.GetXCoordinate: Integer;
var
  AOffset: Integer;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  if Controller.OffsetX.IsNull then
    Exit(MinInt);

  AOffset := Controller.ToDocumentLayoutUnitConverter.ToLayoutUnits(Controller.OffsetX.Value);
  APlacementInfo := Controller.PlacementInfo;
  case Controller.HorizontalPositionType.Value of
    TdxFloatingObjectHorizontalPositionType.LeftMargin,
    TdxFloatingObjectHorizontalPositionType.InsideMargin,
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := APlacementInfo.PageBounds.Left + AOffset;
    TdxFloatingObjectHorizontalPositionType.Column:
      Result := APlacementInfo.ColumnBounds.Left + AOffset;
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := APlacementInfo.PageClientBounds.Left + AOffset;
    TdxFloatingObjectHorizontalPositionType.OutsideMargin,
    TdxFloatingObjectHorizontalPositionType.RightMargin:
      Result := APlacementInfo.PageClientBounds.Right + AOffset;
    TdxFloatingObjectHorizontalPositionType.Character:
      Result := APlacementInfo.OriginX + AOffset;
    else
    begin
      TdxRichEditExceptions.ThrowInternalException;
      Result := MinInt;
    end;
  end;
end;

function TdxFloatingObjectLayoutDialogForm.GetYCoordinate: Integer;
var
  AOffset: Integer;
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  if Controller.OffsetY.IsNull then
    Exit(MinInt);

  AOffset := Controller.ToDocumentLayoutUnitConverter.ToLayoutUnits(Controller.OffsetY.Value);
  APlacementInfo := Controller.PlacementInfo;
  case Controller.VerticalPositionType.Value of
    TdxFloatingObjectVerticalPositionType.Paragraph,
    TdxFloatingObjectVerticalPositionType.Line:
      Result := APlacementInfo.OriginY + AOffset;
    TdxFloatingObjectVerticalPositionType.Page,
    TdxFloatingObjectVerticalPositionType.OutsideMargin,
    TdxFloatingObjectVerticalPositionType.InsideMargin,
    TdxFloatingObjectVerticalPositionType.TopMargin:
      Result := APlacementInfo.PageBounds.Y + AOffset;
    TdxFloatingObjectVerticalPositionType.BottomMargin:
      Result := APlacementInfo.PageBounds.Bottom + AOffset;
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := APlacementInfo.ColumnBounds.Y + AOffset;
    else
    begin
      TdxRichEditExceptions.ThrowInternalException;
      Result := MinInt;
    end;
  end;
end;

class function TdxFloatingObjectLayoutDialogForm.IndexToTextWrapSide(AValue: Integer): TdxFloatingObjectTextWrapSide;
begin
  Result := TdxFloatingObjectTextWrapSide(AValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeForm;
begin
  InitializeFormInitPresetControls;
  InitializeFormHorizontalAlignmentComboBox;
  Populate(cmbHorizontalPositionType, InitializeFormFillHorizontalPositionTypeComboBox);
  cmbHorizontalAbsolutePositionRightOf.Properties.Items.Assign(cmbHorizontalPositionType.Properties.Items);

  InitializeFormFillVerticalAlignmentComboBox;
  InitializeFormFillVerticalPositionTypeComboBox(cmbVerticalPositionType, False);
  InitializeFormFillVerticalPositionTypeComboBox(cmbVerticalAbsolutePositionBelow, True);

  InitializeMeasurementUnitEdit(seRotation, TdxMeasurementType.mtCustom,
    TdxMeasurementUnitEditHelper.Create('°', 1, 1, -3600, 3600));
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormAddItemInHorizontalPositionAlignmentComboBox(AItems: TStrings;
  AHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment);
begin
  AddItemValue(AItems, Controller.HorizontalPositionAlignmentTable[AHorizontalPositionAlignment], Ord(AHorizontalPositionAlignment));
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormAddItemInVerticalPositionAlignmentComboBox(AItems: TStrings;
  AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment);
begin
  AddItemValue(AItems, Controller.VerticalPositionAlignmentTable[AVerticalPositionAlignment], Ord(AVerticalPositionAlignment));
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormAddItemToHorizontalPositionTypeComboBox(AItems: TStrings;
  AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType);
begin
  AddItemValue(AItems, Controller.HorizontalPositionTypeTable[AHorizontalPositionType], Ord(AHorizontalPositionType));
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormAddItemToVerticalPositionTypeComboBox(AItems: TStrings;
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType);
begin
  AddItemValue(AItems, Controller.VerticalPositionTypeTable[AVerticalPositionType], Ord(AVerticalPositionType));
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormFillHorizontalPositionTypeComboBox(
  ACombobox: TcxCustomComboBox);
begin
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.Margin);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.Page);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.Column);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.Character);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.LeftMargin);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.RightMargin);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.InsideMargin);
  InitializeFormAddItemToHorizontalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionType.OutsideMargin);
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormFillVerticalAlignmentComboBox;
begin
  Populate(cmbVerticalAlignment, procedure(ACombobox: TcxCustomComboBox)
    begin
      InitializeFormAddItemInVerticalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionAlignment.Top);
      InitializeFormAddItemInVerticalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionAlignment.Center);
      InitializeFormAddItemInVerticalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionAlignment.Bottom);
      InitializeFormAddItemInVerticalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionAlignment.Inside);
      InitializeFormAddItemInVerticalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionAlignment.Outside);
    end);
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormFillVerticalPositionTypeComboBox(
  AComboBox: TcxCustomComboBox; AUseVerticalPositionTypeParagraph: Boolean);
begin
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.Margin);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.Page);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.Line);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.TopMargin);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.BottomMargin);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.InsideMargin);
  InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.OutsideMargin);

  if AUseVerticalPositionTypeParagraph then
    InitializeFormAddItemToVerticalPositionTypeComboBox(ACombobox.Properties.Items, TdxFloatingObjectVerticalPositionType.Paragraph);
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormHorizontalAlignmentComboBox;
begin
  Populate(cmbHorizontalAlignment, procedure(ACombobox: TcxCustomComboBox)
    begin
      InitializeFormAddItemInHorizontalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionAlignment.Left);
      InitializeFormAddItemInHorizontalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionAlignment.Center);
      InitializeFormAddItemInHorizontalPositionAlignmentComboBox(ACombobox.Properties.Items, TdxFloatingObjectHorizontalPositionAlignment.Right);
    end);
end;

procedure TdxFloatingObjectLayoutDialogForm.InitializeFormInitPresetControls;

  procedure InitializePresetControl(AIndex: Integer; AControl: TcxButton; const APreset: TdxTextWrapTypeInfoPreset);
  begin
    AControl.Tag := AIndex;
    FPresetControls[AIndex].Control := AControl;
    FPresetControls[AIndex].InfoPreset := APreset;
  end;

begin
  InitializePresetControl(0, btnPresetControlSquare, FloatingObjectSquareTextWrapTypePreset);
  InitializePresetControl(1, btnPresetControlTight, FloatingObjectTightTextWrapTypePreset);
  InitializePresetControl(2, btnPresetControlThought, FloatingObjectThroughTextWrapTypePreset);
  InitializePresetControl(3, btnPresetControlTopAndBottom, FloatingObjectTopAndBottomTextWrapTypePreset);
  InitializePresetControl(4, btnPresetControlBehind, FloatingObjectBehindTextWrapTypePreset);
  InitializePresetControl(5, btnPresetControlInFrontOf, FloatingObjectInFrontOfTextWrapTypePreset);
end;

function TdxFloatingObjectLayoutDialogForm.IsValid: Boolean;

  function IsValidEdit(AEdit: TcxCustomEdit): Boolean;
  begin
    if AEdit.Enabled then
      Result := AEdit.ValidateEdit(True)
    else
      Result := True;
  end;

begin
  Result := IsValidEdit(seHorizontalAbsolutePosition) and IsValidEdit(seVerticalAbsolutePosition) and
    IsValidEdit(seTop) and IsValidEdit(seBottom) and IsValidEdit(seLeft) and IsValidEdit(seRight) and
    IsValidEdit(seHeightAbs) and IsValidEdit(seWidthAbs) and IsValidEdit(seRotation);
end;

procedure TdxFloatingObjectLayoutDialogForm.PresetControlsChecked;
var
  I: Integer;
  ATextWrapTypePreset: TdxTextWrapTypeInfoPreset;
  AIsSelected: Boolean;
begin
  for I := 0 to PresetControlCount - 1 do
  begin
    ATextWrapTypePreset := FPresetControls[I].InfoPreset;
    if (Controller.TextWrapType = TdxFloatingObjectTextWrapType.None) and (ATextWrapTypePreset.TextWrapType = TdxFloatingObjectTextWrapType.None) then
      AIsSelected := (Controller.IsBehindDocument = ATextWrapTypePreset.IsBehindDocument)
    else
      AIsSelected := (ATextWrapTypePreset.TextWrapType = Controller.TextWrapType);

    FPresetControls[I].Control.SpeedButtonOptions.Down := AIsSelected;
    FPresetControls[I].Control.TabStop := AIsSelected;

    if ((Controller.TextWrapType = TdxFloatingObjectTextWrapType.Tight)) or ((Controller.TextWrapType = TdxFloatingObjectTextWrapType.Through)) then
      WrapTextAndDistanceControlsEnabled(True, False)
    else
      if Controller.TextWrapType = TdxFloatingObjectTextWrapType.Square then
        WrapTextAndDistanceControlsEnabled(True, True)
      else
        if Controller.TextWrapType = TdxFloatingObjectTextWrapType.TopAndBottom then
          WrapTextAndDistanceControlsEnabled(False, True)
        else
          if Controller.TextWrapType = TdxFloatingObjectTextWrapType.None then
            WrapTextAndDistanceControlsEnabled(False, False);
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.rbHorizontalAlignmentItemClick(Sender: TObject);
var
  ASelectedIndex: Integer;
  AIsHorizontalAbsolutePosition: Boolean;
begin
  ASelectedIndex := (Sender as TComponent).Tag;
  AIsHorizontalAbsolutePosition := (ASelectedIndex <> 0);
  Controller.IsHorizontalAbsolutePosition := AIsHorizontalAbsolutePosition;
  EnableHorizontalPositionControls(not AIsHorizontalAbsolutePosition, AIsHorizontalAbsolutePosition);

  if AIsHorizontalAbsolutePosition then
  begin
    seHorizontalAbsolutePositionPropertiesChange(Self);
    cmbHorizontalAbsolutePositionRightOfPropertiesChange(Self);
  end
  else
  begin
    cmbHorizontalAlignmentPropertiesChange(Self);
    cmbHorizontalPositionTypePropertiesChange(Self);
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.rbVerticalAlignmentItemClick(Sender: TObject);
var
  ASelectedIndex: Integer;
  AIsVerticalAbsolutePosition: Boolean;
begin
  ASelectedIndex := (Sender as TComponent).Tag;
  AIsVerticalAbsolutePosition := (ASelectedIndex <> 0);
  Controller.IsVerticalAbsolutePosition := AIsVerticalAbsolutePosition;
  EnableVerticalPositionControls(not AIsVerticalAbsolutePosition, AIsVerticalAbsolutePosition);

  if AIsVerticalAbsolutePosition then
  begin
    seVerticalAbsolutePositionPropertiesChange(Self);
    cmbVerticalAbsolutePositionBelowPropertiesChange(Self);
  end
  else
  begin
    cmbVerticalAlignmentPropertiesChange(Self);
    cmbVerticalPositionTypePropertiesChange(Self);
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.rgTextWrapSideClick(Sender: TObject);
var
  ATag: Integer;
begin
  ATag := (Sender as TComponent).Tag;
  Controller.TextWrapSide := IndexToTextWrapSide(ATag);
end;

procedure TdxFloatingObjectLayoutDialogForm.seBottomPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seBottom);
  if not VarIsNull(AValue) then
    Controller.BottomDistance := AValue;
end;

procedure TdxFloatingObjectLayoutDialogForm.seHeightAbsoluteValueChanged(Sender: TObject);
var
  ANewHeight: Variant;
begin
  UnsubscribeControlsEvents;
  ANewHeight := GetValueFromEditor(seHeightAbs);
  if VarIsNull(ANewHeight) then
    ANewHeight := 0;
  Controller.RecalculateSizeDependingOnHeight(cbLockAspectRatio.Checked, ANewHeight);
  SetValueToEditor(seWidthAbs, Controller.ActualWidth);
  SubscribeControlsEvents;
end;

procedure TdxFloatingObjectLayoutDialogForm.seLeftPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seLeft);
  if not VarIsNull(AValue) then
    Controller.LeftDistance := AValue;
end;

procedure TdxFloatingObjectLayoutDialogForm.seRightPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seRight);
  if not VarIsNull(AValue) then
    Controller.RightDistance := AValue;
end;

procedure TdxFloatingObjectLayoutDialogForm.seRotationPropertiesChange(Sender: TObject);
var
  AValue: Integer;
begin
  UnsubscribeControlsEvents;
  try
    AValue := GetValueFromEditor(seRotation);
    Controller.Rotation := Controller.DocumentModel.UnitConverter.DegreeToModelUnits(AValue);
    if (Controller.Rotation <> 0) and (Controller.TextWrapType = TdxFloatingObjectTextWrapType.Inline) then
    begin
      UnsubscribeControlsEvents;

      Controller.TextWrapType := TdxFloatingObjectTextWrapType.None;
      Controller.IsBehindDocument := False;
      UpdateFormCore;
    end;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.seTopPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seTop);
  if not VarIsNull(AValue) then
    Controller.TopDistance := AValue;
end;

procedure TdxFloatingObjectLayoutDialogForm.SetTextWrapSide(const Value: TdxFloatingObjectTextWrapSide);
begin
  case Value of
    TdxFloatingObjectTextWrapSide.Both: rgTextWrapSideBothSides.Checked := True;
    TdxFloatingObjectTextWrapSide.Left: rbTextWrapSideLeftOnly.Checked := True;
    TdxFloatingObjectTextWrapSide.Right: rbTextWrapSideRightOnly.Checked := True;
    TdxFloatingObjectTextWrapSide.Largest: rbTextWrapSideLargestOnly.Checked := True;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.seWidthAbsoluteValueChanged(Sender: TObject);
var
  ANewWidth: Variant;
begin
  UnsubscribeControlsEvents;
  ANewWidth := GetValueFromEditor(seWidthAbs);
  if VarIsNull(ANewWidth) then
    ANewWidth := 0;
  Controller.RecalculateSizeDependingOnWidth(cbLockAspectRatio.Checked, ANewWidth);
  SetValueToEditor(seHeightAbs, Controller.ActualHeight);
  SubscribeControlsEvents;
end;

procedure TdxFloatingObjectLayoutDialogForm.SubscribeControlsEvents;
var
  I: Integer;
begin
  rbHorizontalAlignmentItem.OnClick := rbHorizontalAlignmentItemClick;
  rbHorizontalAbsolutePositionItem.OnClick := rbHorizontalAlignmentItemClick;
  rbVerticalAlignmentItem.OnClick := rbVerticalAlignmentItemClick;
  rbVerticalAbsolutePositionItem.OnClick := rbVerticalAlignmentItemClick;
  cmbHorizontalAlignment.Properties.OnChange := cmbHorizontalAlignmentPropertiesChange;
  cmbHorizontalPositionType.Properties.OnChange := cmbHorizontalPositionTypePropertiesChange;
  cmbHorizontalAbsolutePositionRightOf.Properties.OnChange := cmbHorizontalAbsolutePositionRightOfPropertiesChange;
  cmbVerticalAlignment.Properties.OnChange := cmbVerticalAlignmentPropertiesChange;
  cmbVerticalPositionType.Properties.OnChange := cmbVerticalPositionTypePropertiesChange;
  cmbVerticalAbsolutePositionBelow.Properties.OnChange := cmbVerticalAbsolutePositionBelowPropertiesChange;
  seHorizontalAbsolutePosition.Properties.OnChange := seHorizontalAbsolutePositionPropertiesChange;
  seVerticalAbsolutePosition.Properties.OnChange := seVerticalAbsolutePositionPropertiesChange;
  cbLock.Properties.OnChange := cbLockPropertiesChange;
  rgTextWrapSideBothSides.OnClick := rgTextWrapSideClick;
  rbTextWrapSideLeftOnly.OnClick := rgTextWrapSideClick;
  rbTextWrapSideRightOnly.OnClick := rgTextWrapSideClick;
  rbTextWrapSideLargestOnly.OnClick := rgTextWrapSideClick;
  seTop.Properties.OnChange := seTopPropertiesChange;
  seBottom.Properties.OnChange := seBottomPropertiesChange;
  seLeft.Properties.OnChange := seLeftPropertiesChange;
  seRight.Properties.OnChange := seRightPropertiesChange;

  cbLockAspectRatio.OnClick := cbLockAspectRatioClick;
  seHeightAbs.Properties.OnChange := seHeightAbsoluteValueChanged;
  seWidthAbs.Properties.OnChange := seWidthAbsoluteValueChanged;
  seRotation.Properties.OnChange := seRotationPropertiesChange;

  for I := 0 to PresetControlCount - 1 do
    FPresetControls[I].Control.OnClick := btnPresetControlClick;
end;

procedure TdxFloatingObjectLayoutDialogForm.UnsubscribeControlsEvents;
var
  I: Integer;
begin
  rbHorizontalAlignmentItem.OnClick := nil;
  rbHorizontalAbsolutePositionItem.OnClick := nil;
  rbVerticalAlignmentItem.OnClick := nil;
  rbVerticalAbsolutePositionItem.OnClick := nil;
  cmbHorizontalAlignment.Properties.OnChange := nil;
  cmbHorizontalPositionType.Properties.OnChange := nil;
  cmbHorizontalAbsolutePositionRightOf.Properties.OnChange := nil;
  cmbVerticalAlignment.Properties.OnChange := nil;
  cmbVerticalPositionType.Properties.OnChange := nil;
  cmbVerticalAbsolutePositionBelow.Properties.OnChange := nil;
  seHorizontalAbsolutePosition.Properties.OnChange := nil;
  seVerticalAbsolutePosition.Properties.OnChange := nil;
  cbLock.Properties.OnChange := nil;
  rgTextWrapSideBothSides.OnClick := nil;
  rbTextWrapSideLeftOnly.OnClick := nil;
  rbTextWrapSideRightOnly.OnClick := nil;
  rbTextWrapSideLargestOnly.OnClick := nil;
  seTop.Properties.OnChange := nil;
  seBottom.Properties.OnChange := nil;
  seLeft.Properties.OnChange := nil;
  seRight.Properties.OnChange := nil;

  cbLockAspectRatio.OnClick := nil;
  seHeightAbs.Properties.OnChange := nil;
  seWidthAbs.Properties.OnChange := nil;
  seRotation.Properties.OnChange := nil;

  for I := 0 to PresetControlCount - 1 do
    FPresetControls[I].Control.OnClick := nil;
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateFormCore;
begin
  UpdateTabPagePosition;
  UpdateTabPageTextWrapping;
  UpdateTabPageSize;
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateRichTextIndentEdit(AEdit: TdxMeasurementUnitEdit;
  AAllowNegativeValues: Boolean; const AValue: TdxNullableInteger);
var
  AIntValue: Integer;
begin
  if AValue.HasValue then
    AIntValue := AValue.Value
  else
    AIntValue := 0;
  UpdateRichTextIndentEdit(AEdit, AAllowNegativeValues, AIntValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateRichTextIndentEdit(AEdit: TdxMeasurementUnitEdit;
  AAllowNegativeValues: Boolean; AValue: Integer);
var
  AProperties: TdxFloatingObjectRichTextIndentEditProperties;
  AMinValue, AMaxValue: Integer;
begin
  AProperties := Controller.FloatingObjectRichTextIndentEditProperties;

  AMinValue := AProperties.GetMinValue(AAllowNegativeValues);
  AMaxValue := AProperties.MaxValue;
  InitializeMeasurementUnitEdit(AEdit, ToMeasurementType(AProperties.DefaultUnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, ModelUnitsToUIUnit(AMinValue), ModelUnitsToUIUnit(AMaxValue)));

  SetValueToEditor(AEdit, AValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPagePosition;
begin
  if Controller.TextWrapType = TdxFloatingObjectTextWrapType.Inline then
    lcgTabPagePosition.Enabled := False
  else
  begin
    lcgTabPagePosition.Enabled := True;
    UpdateTabPagePositionHorizontalControls;
    UpdateTabPagePositionVerticalControls;
    cbLock.Checked := Controller.Locked = True;
  end;
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPagePositionHorizontalControls;
begin
  if Controller.HorizontalPositionAlignment.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalAlignment,
      Ord(Controller.HorizontalPositionAlignment.Value), Ord(TdxFloatingObjectHorizontalPositionAlignment.Left))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalAlignment, -1, Ord(TdxFloatingObjectHorizontalPositionAlignment.Left));

  if Controller.HorizontalPositionType.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalPositionType, Ord(Controller.HorizontalPositionType.Value))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalPositionType, -1);

  if Controller.HorizontalPositionType.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalAbsolutePositionRightOf, Ord(Controller.HorizontalPositionType.Value))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbHorizontalAbsolutePositionRightOf, -1);
  if Controller.HorizontalPositionAlignment.HasValue then
    if Controller.HorizontalPositionAlignment.Value = TdxFloatingObjectHorizontalPositionAlignment.None then
      rbHorizontalAbsolutePositionItem.Checked := True
    else
      rbHorizontalAlignmentItem.Checked := True
  else
    lgHorizontalGroup.Enabled := False;
  EnableHorizontalPositionControls(rbHorizontalAlignmentItem.Checked, not rbHorizontalAlignmentItem.Checked);
  UpdateRichTextIndentEdit(seHorizontalAbsolutePosition, True, Controller.OffsetX);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPagePositionUpdateSelectedIndex(AComboBox: TcxCustomComboBox;
  const AValue: Integer);
begin
  UpdateSelectedIndex(AComboBox, AValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPagePositionUpdateSelectedIndex(AComboBox: TcxCustomComboBox;
  const AValue: Integer; const ADefaultValue: Integer);
begin
  if AValue <> - 1 then
    UpdateSelectedIndex(AComboBox, AValue);
  if AComboBox.ItemIndex = -1 then
    UpdateSelectedIndex(AComboBox, ADefaultValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPagePositionVerticalControls;
begin
  if Controller.VerticalPositionAlignment.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalAlignment,
      Ord(Controller.VerticalPositionAlignment.Value), Ord(TdxFloatingObjectVerticalPositionAlignment.Top))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalAlignment,
      -1, Ord(TdxFloatingObjectVerticalPositionAlignment.Top));

  if Controller.VerticalPositionType.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalPositionType,
      Ord(Controller.VerticalPositionType.Value), Ord(TdxFloatingObjectVerticalPositionType.Page))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalPositionType,
      -1, Ord(TdxFloatingObjectVerticalPositionType.Page));

  if Controller.VerticalPositionType.HasValue then
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalAbsolutePositionBelow,
      Ord(Controller.VerticalPositionType.Value))
  else
    UpdateTabPagePositionUpdateSelectedIndex(cmbVerticalAbsolutePositionBelow, -1);

  if Controller.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
    rbVerticalAlignmentItem.Checked := True
  else
    rbVerticalAbsolutePositionItem.Checked := True;
  EnableVerticalPositionControls(rbVerticalAlignmentItem.Checked, not rbVerticalAlignmentItem.Checked);
  UpdateRichTextIndentEdit(seVerticalAbsolutePosition, True, Controller.OffsetY);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPageSize;
var
  ARotationValue: Integer;
begin
  cbLockAspectRatio.Checked := Controller.LockAspectRatio;
  UpdateRichTextIndentEdit(seHeightAbs, False, Controller.ActualHeight);
  UpdateRichTextIndentEdit(seWidthAbs, False, Controller.ActualWidth);
  lblOriginalSizeHeightValue.Caption := Controller.OriginalHeightAsString;
  lblOriginalSizeWidthValue.Caption := Controller.OriginalWidthAsString;
  if Controller.Rotation.HasValue then
    ARotationValue := Controller.DocumentModel.UnitConverter.ModelUnitsToDegree(Controller.Rotation.Value)
  else
    ARotationValue := 0;
  SetValueToEditor(seRotation, ARotationValue);
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPageTextWrapping;
begin
  WrapTextAndDistanceControlsEnabled(True, True);
  PresetControlsChecked;
  if Controller.TextWrapSide.HasValue then
    TextWrapSide := Controller.TextWrapSide.Value;

  UpdateTabPageTextWrappingDistanceControls;
end;

procedure TdxFloatingObjectLayoutDialogForm.UpdateTabPageTextWrappingDistanceControls;
begin
  UpdateRichTextIndentEdit(seTop, False, Controller.TopDistance);
  UpdateRichTextIndentEdit(seBottom, False, Controller.BottomDistance);
  UpdateRichTextIndentEdit(seLeft, False, Controller.LeftDistance);
  UpdateRichTextIndentEdit(seRight, False, Controller.RightDistance);
end;

procedure TdxFloatingObjectLayoutDialogForm.WrapTextAndDistanceControlsEnabled(AIsHorizontalControlsEnabled,
  AIsVerticalControlsEnabled: Boolean);
begin
  lciLeft.Enabled := AIsHorizontalControlsEnabled;
  lciRight.Enabled := AIsHorizontalControlsEnabled;
  lgTextWrapSide.Enabled := AIsHorizontalControlsEnabled;

  lciTop.Enabled := AIsVerticalControlsEnabled;
  lciBottom.Enabled := AIsVerticalControlsEnabled;
end;

procedure TdxFloatingObjectLayoutDialogForm.EnableHorizontalPositionControls(AEnableAlignmentRow,
  AEnableAbsolutePositionRow: Boolean);
begin
  cmbHorizontalAlignment.Enabled := AEnableAlignmentRow;
  lblHorizontalPositionType.Enabled := AEnableAlignmentRow;

  seHorizontalAbsolutePosition.Enabled := AEnableAbsolutePositionRow;
  lciHorizontalAbsolutePosition.Enabled := AEnableAbsolutePositionRow;
end;

procedure TdxFloatingObjectLayoutDialogForm.EnableVerticalPositionControls(AEnableAlignmentRow,
  AEnableAbsolutePositionRow: Boolean);
begin
  cmbVerticalAlignment.Enabled := AEnableAlignmentRow;
  lciVerticalPositionType.Enabled := AEnableAlignmentRow;

  seVerticalAbsolutePosition.Enabled := AEnableAbsolutePositionRow;
  lciVerticalAbsolutePosition.Enabled := AEnableAbsolutePositionRow;
end;

procedure TdxFloatingObjectLayoutDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := IsValid;
  if CanClose then
    Controller.ApplyChanges;
end;

end.
