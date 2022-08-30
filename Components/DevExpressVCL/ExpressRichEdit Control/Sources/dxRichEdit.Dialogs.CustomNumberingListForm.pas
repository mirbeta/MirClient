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

unit dxRichEdit.Dialogs.CustomNumberingListForm;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, dxCoreClasses, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, Menus, dxLayoutControlAdapters, StdCtrls, cxButtons,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit,
  dxRichEdit.View.Core, dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.NumberingFormController, dxLayoutLookAndFeels, cxClasses;

type
  TdxRichEditCustomNumberingListForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnFont: TcxButton;
    btnOk: TcxButton;
    edtAligned: TdxMeasurementUnitEdit;
    edtIndent: TdxMeasurementUnitEdit;
    procedure btnFontClick(Sender: TObject);
    procedure edtAlignedIndentPropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  protected
    procedure ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties; AData: TObject);
    procedure ApplyLocalization; override;
    function CalculateFirstLineIndent: Integer;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    function GetController: TdxBulletedListFormController;
    function GetUnitConverter: TdxDocumentModelUnitConverter; override;
    procedure InitializeForm; override;
    function IsValidEdit(AEdit: TcxCustomEdit): Boolean;
    function IsValid: Boolean;
    procedure LoadNumberingPosition;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    function UpdateFirstLineIndent(ANewValue: Integer): Integer;
    procedure UpdateFormCore; override;
    property Controller: TdxBulletedListFormController read GetController;
  public
    procedure ApplyChanges; virtual;
    procedure ShowFontForm(const AControl: IdxRichEditControl; ASourceProperties: TdxCharacterProperties);
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Control,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.ParagraphFormatting;

{$R *.dfm}

type
  TdxFontPropertiesModifierAccess = class(TdxFontPropertiesModifier);

{ TdxRichEditCustomNumberingListForm }

procedure TdxRichEditCustomNumberingListForm.ApplyChanges;
begin
  Controller.ApplyChanges;
  ModalResult := mrOk;
end;

procedure TdxRichEditCustomNumberingListForm.ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties;
  AData: TObject);
var
  AModified: TdxFontPropertiesModifier;
begin
  AModified := TdxFontPropertiesModifier.Create(AProperties);
  try
    TdxFontPropertiesModifierAccess(AModified).ApplyCharacterProperties(TdxCharacterProperties(AData));
  finally
    AModified.Free;
  end;
end;

procedure TdxRichEditCustomNumberingListForm.ApplyLocalization;
begin
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  btnFont.Caption := cxGetResourceString(@sdxRichEditCustomNumberingListButtonFont);
end;

procedure TdxRichEditCustomNumberingListForm.btnFontClick(Sender: TObject);
begin
  ShowFontForm(Control, Controller.CharacterProperties);
end;

function TdxRichEditCustomNumberingListForm.CalculateFirstLineIndent: Integer;
var
  AParagraphProperties: TdxParagraphProperties;
begin
  AParagraphProperties := Controller.EditedLevel.ParagraphProperties;
  if AParagraphProperties.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    Result := Controller.LeftIndent - Controller.FirstLineIndent
  else
    Result := Controller.FirstLineIndent + Controller.LeftIndent;
end;

function TdxRichEditCustomNumberingListForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxBulletedListFormController.Create(AControllerParameters as TdxBaseNumberingListFormControllerParameters);
end;

procedure TdxRichEditCustomNumberingListForm.edtAlignedIndentPropertiesChange(Sender: TObject);
begin
  Controller.AssignControllerIndentValues(GetValueFromEditor(edtAligned), GetValueFromEditor(edtIndent));
end;

procedure TdxRichEditCustomNumberingListForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if Assigned(Owner) then
    if ModalResult = mrCancel then
      (Owner as TCustomForm).Show
    else
      if IsValid then
      begin
        ApplyChanges;
        (Owner as TCustomForm).Close;
      end
      else
        CanClose := False;
end;

procedure TdxRichEditCustomNumberingListForm.FormShow(Sender: TObject);
begin
  if Assigned(Owner) then
    (Owner as TCustomForm).Hide;
end;

function TdxRichEditCustomNumberingListForm.GetController: TdxBulletedListFormController;
begin
  Result := TdxBulletedListFormController(inherited Controller);
end;

function TdxRichEditCustomNumberingListForm.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Control.DocumentModel.UnitConverter;
end;

procedure TdxRichEditCustomNumberingListForm.InitializeForm;
begin
  InitializeMeasurementUnitEdit(edtAligned, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, InchesToUIUnit(-4.5), InchesToUIUnit(4.5)));
  InitializeMeasurementUnitEdit(edtIndent, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2, InchesToUIUnit(-4.5), InchesToUIUnit(4.5)));
  LoadNumberingPosition;
end;

function TdxRichEditCustomNumberingListForm.IsValid: Boolean;
begin
  Result := IsValidEdit(edtAligned) and IsValidEdit(edtIndent);
end;

function TdxRichEditCustomNumberingListForm.IsValidEdit(AEdit: TcxCustomEdit): Boolean;
begin
  if AEdit.Enabled then
    Result := AEdit.ValidateEdit(True)
  else
    Result := True;
end;

procedure TdxRichEditCustomNumberingListForm.LoadNumberingPosition;
begin
  SetValueToEditor(edtAligned, CalculateFirstLineIndent);
  SetValueToEditor(edtIndent, Controller.LeftIndent);
end;

procedure TdxRichEditCustomNumberingListForm.ShowFontForm(const AControl: IdxRichEditControl;
  ASourceProperties: TdxCharacterProperties);
var
  AEditedProperties: TdxMergedCharacterProperties;
begin
  AEditedProperties := TdxMergedCharacterProperties.Create(ASourceProperties.Info.Info, ASourceProperties.Info.Options);
  try
    AControl.ShowFontForm(AEditedProperties, ApplyCharacterProperties, ASourceProperties);
  finally
    AEditedProperties.Free;
  end;
end;

procedure TdxRichEditCustomNumberingListForm.SubscribeControlsEvents;
begin
  edtAligned.Properties.OnChange := edtAlignedIndentPropertiesChange;
  edtIndent.Properties.OnChange := edtAlignedIndentPropertiesChange;
end;

procedure TdxRichEditCustomNumberingListForm.UnsubscribeControlsEvents;
begin
  edtAligned.Properties.OnChange := nil;
  edtIndent.Properties.OnChange := nil;
end;

function TdxRichEditCustomNumberingListForm.UpdateFirstLineIndent(ANewValue: Integer): Integer;
begin
  Result := Abs(ANewValue - Controller.LeftIndent);
end;

procedure TdxRichEditCustomNumberingListForm.UpdateFormCore;
begin
  SetValueToEditor(edtAligned, CalculateFirstLineIndent);
  SetValueToEditor(edtIndent, Controller.LeftIndent);
end;

end.
