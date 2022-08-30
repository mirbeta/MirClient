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

unit dxRichEdit.Dialogs.EditStyle;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  dxLayoutContainer, cxLabel, dxLayoutControl, dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses,
  StdCtrls, cxButtons, cxFontNameComboBox, dxColorEdit, dxCoreGraphics,
  ActnList, ImgList, ExtCtrls, dxCore, dxCoreClasses, dxGDIPlusAPI, dxGDIPlusClasses,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEditFontNameComboBox,
  dxRichEdit.Actions,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.EditStyleController,
  dxRichEdit.Control,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting;

type
  TdxRichEditEditStyleDialogForm = class(TdxRichEditCustomDialogForm)
    aDecrementIndent: TAction;
    aFontDialog: TAction;
    aIncrementIndent: TAction;
    alActions: TActionList;
    aParagraphDialog: TAction;
    aSetDoubleParagraphSpacing: TAction;
    aSetSesquialteralParagraphSpacing: TAction;
    aSetSingleParagraphSpacing: TAction;
    aSpacingDecrease: TAction;
    aSpacingIncrease: TAction;
    aTabsDialog: TAction;
    aToggleFontBold: TAction;
    aToggleFontItalic: TAction;
    aToggleFontUnderline: TAction;
    aToggleParagraphAlignmentCenter: TAction;
    aToggleParagraphAlignmentJustify: TAction;
    aToggleParagraphAlignmentLeft: TAction;
    aToggleParagraphAlignmentRight: TAction;
    btnCancel: TcxButton;
    btnDecreaseParagraphSpacing: TcxButton;
    btnDecrementIndent: TcxButton;
    btnDoubleParagraphSpacing: TcxButton;
    btnFormat: TcxButton;
    btnIncreaseParagraphSpacing: TcxButton;
    btnIncrementIndent: TcxButton;
    btnOk: TcxButton;
    btnSesquialteralParagraphSpacing: TcxButton;
    btnSingleParagraphSpacing: TcxButton;
    btnToggleFontBold: TcxButton;
    btnToggleFontItalic: TcxButton;
    btnToggleFontUnderline: TcxButton;
    btnToggleParagraphAlignmentCenter: TcxButton;
    btnToggleParagraphAlignmentJustify: TcxButton;
    btnToggleParagraphAlignmentLeft: TcxButton;
    btnToggleParagraphAlignmentRight: TcxButton;
    cmbCurrentStyle: TcxComboBox;
    cmbFontColor: TdxColorEdit;
    cmbFontEdit: TdxRichEditFontNameComboBox;
    cmbFontSize: TcxComboBox;
    cmbNextStyle: TcxComboBox;
    cmbParent: TcxComboBox;
    dxLayoutBarLookAndFeel: TdxLayoutStandardLookAndFeel;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item12: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutSeparatorItem;
    dxLayoutControl1Item15: TdxLayoutItem;
    dxLayoutControl1Item16: TdxLayoutItem;
    dxLayoutControl1Item17: TdxLayoutItem;
    dxLayoutControl1Item18: TdxLayoutItem;
    dxLayoutControl1Item19: TdxLayoutItem;
    dxLayoutControl1Item20: TdxLayoutItem;
    dxLayoutControl1Item21: TdxLayoutItem;
    dxLayoutControl1Item22: TdxLayoutItem;
    dxLayoutControl1Item23: TdxLayoutItem;
    dxLayoutControl1Item24: TdxLayoutItem;
    dxLayoutControl1Item25: TdxLayoutItem;
    dxLayoutControl1Item26: TdxLayoutItem;
    dxLayoutControl1Item27: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem3: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem4: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem5: TdxLayoutSeparatorItem;
    edtName: TcxTextEdit;
    ilActions: TcxImageList;
    lblFormatting: TdxLayoutSeparatorItem;
    lblProperties: TdxLayoutSeparatorItem;
    lblSelectedStyle: TdxLayoutSeparatorItem;
    lcgBarFontFormatting: TdxLayoutGroup;
    lciCurrentStyle: TdxLayoutItem;
    lcilName: TdxLayoutItem;
    lcilStyleBasedOn: TdxLayoutItem;
    lciStyleForFollowingParagraph: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    miFontDialog: TMenuItem;
    miParagraphDialog: TMenuItem;
    miTabsDialog: TMenuItem;
    pmFormat: TPopupMenu;
    PreviewRichEditControl: TdxRichEditControl;
    procedure aSpacingIncreaseExecute(Sender: TObject);
    procedure aSpacingDecreaseExecute(Sender: TObject);
    procedure aIncrementIndentExecute(Sender: TObject);
    procedure aDecrementIndentExecute(Sender: TObject);
    procedure aFontDialogExecute(Sender: TObject);
    procedure aParagraphDialogExecute(Sender: TObject);
    procedure aTabsDialogExecute(Sender: TObject);
    procedure FontEditPropertiesChange(Sender: TObject);
    procedure FontSizePropertiesChange(Sender: TObject);
    procedure FontColorPropertiesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetCharacterStyleParent: TdxCharacterStyle; inline;
    function GetCharacterProperties: TdxCharacterProperties; inline;
    function GetController: TdxEditStyleFormController; inline;
    function GetIntermediateParagraphStyle: TdxParagraphStyle; inline;
    function GetIntermediateCharacterStyle: TdxCharacterStyle; inline;
    function GetIsParagraphStyle: Boolean; inline;
    function GetParagraphProperties: TdxParagraphProperties; inline;
  protected
    procedure ApplyLocalization; override;
    procedure InitializeForm; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;

    procedure ButtonsEnabled(AValue: Boolean);
    procedure FillStyleCombo(AComboBox: TcxCustomComboBox); overload;
    procedure FillStyleCombo(AComboBox: TcxComboBox; AStyleCollection: TdxStyleCollectionBase; AMatch: TFunc<TdxStyleBase, Boolean>); overload;
    procedure FillCurrentStyleCombo(AComboBox: TcxCustomComboBox; AStyles: TdxStyleCollectionBase);
    procedure PopulateFontSize;

    procedure CurrentStylePropertiesChange(Sender: TObject);
    procedure NameStylePropertiesChange(Sender: TObject);
    procedure NextStylePropertiesChange(Sender: TObject);
    procedure ParentStylePropertiesChange(Sender: TObject);
    procedure SetParagraphSpacingExecute(Sender: TObject);
    procedure ToggleParagraphAlignmentExecute(Sender: TObject);
    procedure ToggleFontBoldExecute(Sender: TObject);
    procedure ToggleFontItalicExecute(Sender: TObject);
    procedure ToggleFontUnderlineExecute(Sender: TObject);

    procedure ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties; AData: TObject);
    procedure ApplyParagraphProperties(AProperties: TdxMergedParagraphProperties; AData: TObject);
    procedure ApplyTabsProperties(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer; AData: TObject);
    procedure BarEnabled(AValue: Boolean);
    function IsCharacterParentValid(AStyle: TdxStyleBase): Boolean;
    function IsParagraphParentValid(AStyle: TdxStyleBase): Boolean;
    function IsNextValid(AStyle: TdxStyleBase): Boolean;
    procedure UpdateFormCore; override;
    procedure UpdateRichEditBars;
    procedure UpdateCharacterBars(const AMergedCharacterProperties: TdxCharacterFormattingInfo);
    procedure SubscribeControlsEvents; override;
    procedure SubscribeParagraphAlignmentEvents;
    procedure SubscribeParagraphLineSpacingEvents;
    procedure SubscribeToggleButtonsEvents;
    procedure UnsubscribeControlsEvents; override;
    procedure UnsubscribeParagraphAlignmentEvents;
    procedure UnsubscribeParagraphLineSpacingEvents;
    procedure UnsubscribeToggleButtonsEvents;

    property IntermediateCharacterStyle: TdxCharacterStyle read GetIntermediateCharacterStyle;
    property IntermediateParagraphStyle: TdxParagraphStyle read GetIntermediateParagraphStyle;
    property CharacterStyleParent: TdxCharacterStyle read GetCharacterStyleParent;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property IsParagraphStyle: Boolean read GetIsParagraphStyle;
  public
    property Controller: TdxEditStyleFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Dialogs.EditStyleHelper,
  dxRichEdit.View.Core,
  dxRichEdit.Commands.Dialogs,
  dxRichEdit.Utils.Exceptions.Strs;

{$R *.dfm}

{ TdxRichEditEditStyleDialogForm }

function TdxRichEditEditStyleDialogForm.GetController: TdxEditStyleFormController;
begin
  Result := TdxEditStyleFormController(inherited Controller);
end;

procedure TdxRichEditEditStyleDialogForm.ApplyCharacterProperties(AProperties: TdxMergedCharacterProperties;
  AData: TObject);
begin
  Controller.CopyCharacterPropertiesFromMerged(AProperties);
  UpdateRichEditBars;
end;

procedure TdxRichEditEditStyleDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditEditStyleDialogForm);
  lblSelectedStyle.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogSelectedStyle);
  lblProperties.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogProperties);
  lblFormatting.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogFormatting);
  btnFormat.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogButtonFormat);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lcilName.CaptionOptions.Text := cxGetResourceString(@sdxRichEditEditStyleDialogName);
  lcilStyleBasedOn.CaptionOptions.Text := cxGetResourceString(@sdxRichEditEditStyleDialogStyleBasedOn);
  lciStyleForFollowingParagraph.CaptionOptions.Text := cxGetResourceString(@sdxRichEditEditStyleDialogStyleForFollowingParagraph);
  lciCurrentStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditEditStyleDialogCurrentStyle);
  aToggleFontBold.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontBold);
  aToggleFontBold.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontBoldHint);
  aToggleFontItalic.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontItalic);
  aToggleFontItalic.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontItalicHint);
  aToggleFontUnderline.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontUnderline);
  aToggleFontUnderline.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleFontUnderlineHint);
  aToggleParagraphAlignmentLeft.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentLeft);
  aToggleParagraphAlignmentLeft.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentLeftHint);
  aToggleParagraphAlignmentCenter.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentCenter);
  aToggleParagraphAlignmentCenter.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentCenterHint);
  aToggleParagraphAlignmentRight.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentRight);
  aToggleParagraphAlignmentRight.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentRightHint);
  aToggleParagraphAlignmentJustify.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentJustify);
  aToggleParagraphAlignmentJustify.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogToggleParagraphAlignmentJustifyHint);
  aIncrementIndent.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogIncrementIndent);
  aIncrementIndent.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogIncrementIndentHint);
  aDecrementIndent.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogDecrementIndent);
  aDecrementIndent.Hint := cxGetResourceString(@sdxRichEditEditStyleDialogDecrementIndentHint);
  aFontDialog.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogFontDialog);
  aParagraphDialog.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogParagraphDialog);
  aTabsDialog.Caption := cxGetResourceString(@sdxRichEditEditStyleDialogTabsDialog);
end;

procedure TdxRichEditEditStyleDialogForm.ApplyParagraphProperties(AProperties: TdxMergedParagraphProperties;
  AData: TObject);
begin
  Controller.CopyParagraphPropertiesFromMerged(AProperties);
  Controller.ApplyTabsProperties(PreviewRichEditControl);
  UpdateRichEditBars;
end;

procedure TdxRichEditEditStyleDialogForm.ApplyTabsProperties(ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer;
  AData: TObject);
begin
  Controller.IntermediateParagraphStyle.Tabs.SetTabs(ATabInfo);
  UpdateRichEditBars;
end;

procedure TdxRichEditEditStyleDialogForm.aDecrementIndentExecute(Sender: TObject);
begin
  Controller.DecreaseIndent;
end;

procedure TdxRichEditEditStyleDialogForm.aFontDialogExecute(Sender: TObject);
var
  AControl: IdxRichEditControl;
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AControl := Controller.Control;
  if IsParagraphStyle then
    AMergedProperties := Controller.IntermediateParagraphStyle.GetMergedWithDefaultCharacterProperties
  else
    AMergedProperties := Controller.IntermediateCharacterStyle.GetMergedWithDefaultCharacterProperties;
  try
    AControl.ShowFontForm(AMergedProperties, ApplyCharacterProperties, nil);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.aIncrementIndentExecute(Sender: TObject);
begin
  Controller.IncreaseIndent;
end;

procedure TdxRichEditEditStyleDialogForm.aParagraphDialogExecute(Sender: TObject);
var
  AControl: IdxRichEditControl;
  AMergedProperties: TdxMergedParagraphProperties;
begin
  AControl := PreviewRichEditControl;
  PreviewRichEditControl.Enabled := True;
  try
    AMergedProperties := Controller.IntermediateParagraphStyle.GetMergedWithDefaultParagraphProperties;
    try
      AControl.ShowParagraphForm(AMergedProperties, ApplyParagraphProperties, nil);
    finally
      AMergedProperties.Free;
    end;
  finally
    PreviewRichEditControl.Enabled := False;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.aSpacingDecreaseExecute(Sender: TObject);
begin
  Controller.DecreaseSpacing;
end;

procedure TdxRichEditEditStyleDialogForm.aSpacingIncreaseExecute(Sender: TObject);
begin
  Controller.IncreaseSpacing;
end;

procedure TdxRichEditEditStyleDialogForm.aTabsDialogExecute(Sender: TObject);
var
  AControl: IdxRichEditControl;
  AInfo: TdxTabFormattingInfo;
  AModel: TdxDocumentModel;
  ADefaultTabWidth: Integer;
begin
  AControl := Controller.Control;
  AInfo := Controller.IntermediateParagraphStyle.GetTabs;
  try
    AModel := AControl.InnerControl.DocumentModel;
    ADefaultTabWidth := AModel.DocumentProperties.DefaultTabWidth;

    AControl.ShowTabsForm(AInfo, ADefaultTabWidth, ApplyTabsProperties, nil);
  finally
    AInfo.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.BarEnabled(AValue: Boolean);
begin
  aDecrementIndent.Enabled := AValue;
  aIncrementIndent.Enabled := AValue;
  aSetDoubleParagraphSpacing.Enabled := AValue;
  aSetSesquialteralParagraphSpacing.Enabled := AValue;
  aSetSingleParagraphSpacing.Enabled := AValue;
  aToggleFontBold.Enabled := AValue;
  aToggleFontItalic.Enabled := AValue;
  aToggleFontUnderline.Enabled := AValue;
  aToggleParagraphAlignmentCenter.Enabled := AValue;
  aToggleParagraphAlignmentJustify.Enabled := AValue;
  aToggleParagraphAlignmentLeft.Enabled := AValue;
  aToggleParagraphAlignmentRight.Enabled := AValue;
end;

procedure TdxRichEditEditStyleDialogForm.ButtonsEnabled(AValue: Boolean);
begin
  cmbNextStyle.Enabled := AValue;
  aParagraphDialog.Enabled := AValue;
  aTabsDialog.Enabled := AValue;
end;

procedure TdxRichEditEditStyleDialogForm.FontColorPropertiesChange(Sender: TObject);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AAccessor := Controller.GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeForeColor(TdxAlphaColors.FromColor(cmbFontColor.ColorValue));
  finally
    AAccessor.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.FontSizePropertiesChange(Sender: TObject);
var
  AText: string;
  AValue: Integer;
  AAccessor: TdxCharacterPropertyAccessor;
begin
  if TdxEditStyleHelper.IsFontSizeValid(cmbFontSize.EditValue, AText, AValue) then
  begin
    AAccessor := Controller.GetCharacterPropertyAccessor;
    try
      AAccessor.ChangeDoubleFontSize(AValue);
    finally
      AAccessor.Free;
    end;
  end
  else
  begin
    Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
    cmbFontSize.EditValue := CharacterProperties.DoubleFontSize / 2;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AText: string;
begin
  if ModalResult <> mrOk then
    Exit;
  if Controller.IsValidName(edtName.Text) then
    Controller.ApplyChanges
  else
  begin
    CanClose := False;
    AText := cxGetResourceString(@sdxRichEditExceptionParagraphStyleNameAlreadyExists);
    Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
  end;
end;

procedure TdxRichEditEditStyleDialogForm.FontEditPropertiesChange(Sender: TObject);
var
  AText: string;
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AAccessor := Controller.GetCharacterPropertyAccessor;
  try
    AText := cmbFontEdit.EditText;
    AAccessor.ChangeFontName(AText);
  finally
    AAccessor.Free;
  end;
end;

function TdxRichEditEditStyleDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxEditStyleFormController.Create(PreviewRichEditControl, AControllerParameters as TdxEditStyleFormControllerParameters);
end;

procedure TdxRichEditEditStyleDialogForm.CurrentStylePropertiesChange(Sender: TObject);
var
  AStyle: IdxStyle;
  AStyleName: string;
begin
  Supports(cmbCurrentStyle.ItemObject, IdxStyle, AStyle);
  if AStyle = nil then
    Exit;
  cmbCurrentStyle.Properties.OnChange := nil;
  AStyleName := AStyle.StyleName;
  if cmbCurrentStyle.ItemObject is TdxParagraphStyle then
    Controller.Parameters.ParagraphSourceStyle := Controller.Model.ParagraphStyles.GetStyleByName(AStyleName) as TdxParagraphStyle
  else
    Controller.Parameters.CharacterSourceStyle := Controller.Model.CharacterStyles.GetStyleByName(AStyleName) as TdxCharacterStyle;

  SetController(CreateController(Controller.Parameters));
  cmbCurrentStyle.Properties.Items.Clear;
  cmbParent.Properties.Items.Clear;
  cmbNextStyle.Properties.Items.Clear;
  cmbNextStyle.ItemIndex := -1;
  InitializeForm;
  cmbCurrentStyle.Properties.OnChange := CurrentStylePropertiesChange;
  UpdateForm;
end;

procedure TdxRichEditEditStyleDialogForm.FillCurrentStyleCombo(AComboBox: TcxCustomComboBox; AStyles: TdxStyleCollectionBase);
var
  I: Integer;
  AStyle: TdxStyleBase;
begin
  for I := 0 to AStyles.Count - 1 do
  begin
    AStyle := AStyles[I];
    if not AStyle.Deleted then
      AComboBox.Properties.Items.AddObject(AStyle.StyleName, AStyle);
  end;
end;

procedure TdxRichEditEditStyleDialogForm.FillStyleCombo(AComboBox: TcxComboBox;
  AStyleCollection: TdxStyleCollectionBase; AMatch: TFunc<TdxStyleBase, Boolean>);
var
  ACollection: TStrings;
  ACount, I: Integer;
  AStyle: TdxStyleBase;
begin
  ACollection := AComboBox.Properties.Items;
  ACollection.BeginUpdate;
  try
    ACount := AStyleCollection.Count;
    for I := 0 to ACount - 1 do
    begin
      AStyle := AStyleCollection[I];
      if not AStyle.Deleted and AMatch(AStyle) then
        ACollection.AddObject(AStyle.StyleName, AStyle);
    end;
  finally
    ACollection.EndUpdate;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.FillStyleCombo(AComboBox: TcxCustomComboBox);
var
  AModel: TdxDocumentModel;
begin
  AModel := Controller.Control.InnerControl.DocumentModel;
  FillCurrentStyleCombo(AComboBox, AModel.ParagraphStyles);
  FillCurrentStyleCombo(AComboBox, AModel.CharacterStyles);
end;

function TdxRichEditEditStyleDialogForm.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := Controller.CharacterProperties;
end;

function TdxRichEditEditStyleDialogForm.GetCharacterStyleParent: TdxCharacterStyle;
begin
  if cmbParent.ItemIndex >= 0 then
    Result := TdxCharacterStyle(cmbParent.ItemObject)
  else
    Result := nil;
end;

function TdxRichEditEditStyleDialogForm.GetIntermediateCharacterStyle: TdxCharacterStyle;
begin
  Result := Controller.IntermediateCharacterStyle;
end;

function TdxRichEditEditStyleDialogForm.GetIntermediateParagraphStyle: TdxParagraphStyle;
begin
  Result := Controller.IntermediateParagraphStyle;
end;

function TdxRichEditEditStyleDialogForm.GetIsParagraphStyle: Boolean;
begin
  Result := Controller.IsParagraphStyle;
end;

function TdxRichEditEditStyleDialogForm.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := IntermediateParagraphStyle.ParagraphProperties;
end;

procedure TdxRichEditEditStyleDialogForm.InitializeForm;
var
  AStyleCollection: TdxParagraphStyleCollection;
begin
  PreviewRichEditControl.Enabled := False;
  Controller.FillTempRichEdit(PreviewRichEditControl);

  Controller.ChangePreviewControlCurrentStyle(PreviewRichEditControl);
  if Controller.Parameters.Control.InnerControl <> nil then
    PreviewRichEditControl.MeasurementUnit := Controller.Parameters.Control.InnerControl.UIUnit;
  edtName.Text := Controller.StyleName;
  PopulateFontSize;
  Populate(cmbCurrentStyle, FillStyleCombo);
  if IsParagraphStyle then
  begin
    ButtonsEnabled(True);
    AStyleCollection := (Controller.ParagraphSourceStyle.DocumentModel as TdxDocumentModel).ParagraphStyles;
    FillStyleCombo(cmbParent, AStyleCollection, IsParagraphParentValid);
    FillStyleCombo(cmbNextStyle, AStyleCollection, IsNextValid);
  end
  else
  begin
    cmbParent.Properties.Items.Add(cxGetResourceString(@sdxRichEditEditStyleDialogEmptyParentStyle));
    FillStyleCombo(cmbParent, (Controller.CharacterSourceStyle.DocumentModel as TdxDocumentModel).CharacterStyles, IsCharacterParentValid);
    ButtonsEnabled(False);
  end;
  UpdateRichEditBars;
end;

function TdxRichEditEditStyleDialogForm.IsCharacterParentValid(AStyle: TdxStyleBase): Boolean;
begin
  Result := Controller.CharacterSourceStyle.IsParentValid(AStyle);
end;

function TdxRichEditEditStyleDialogForm.IsNextValid(AStyle: TdxStyleBase): Boolean;
begin
  Result := True;
end;

function TdxRichEditEditStyleDialogForm.IsParagraphParentValid(AStyle: TdxStyleBase): Boolean;
begin
  Result := Controller.ParagraphSourceStyle.IsParentValid(AStyle);
end;

procedure TdxRichEditEditStyleDialogForm.NameStylePropertiesChange(Sender: TObject);
begin
  Controller.StyleName := edtName.Text;
end;

procedure TdxRichEditEditStyleDialogForm.NextStylePropertiesChange(Sender: TObject);
begin
  if IsParagraphStyle then
    Controller.IntermediateParagraphStyle.NextParagraphStyle := TdxParagraphStyle(cmbNextStyle.ItemObject);
end;

procedure TdxRichEditEditStyleDialogForm.ParentStylePropertiesChange(Sender: TObject);
begin
  if IsParagraphStyle then
    Controller.IntermediateParagraphStyle.Parent := TdxParagraphStyle(cmbParent.ItemObject)
  else
    Controller.IntermediateCharacterStyle.Parent := CharacterStyleParent;
  UpdateRichEditBars;
end;

procedure TdxRichEditEditStyleDialogForm.PopulateFontSize;
begin
  Populate(cmbFontSize, procedure(AComboBox: TcxCustomComboBox)
    var
      AFontSizes: TdxPredefinedFontSizeCollection;
      AFontSize: Integer;
      I: Integer;
    begin
      AFontSizes := Control.InnerControl.PredefinedFontSizeCollection;
      for I := 0 to AFontSizes.Count - 1 do
      begin
        AFontSize := AFontSizes[I];
        AComboBox.Properties.Items.Add(IntToStr(AFontSize));
      end;
    end);
end;

procedure TdxRichEditEditStyleDialogForm.SetParagraphSpacingExecute(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;
  UnsubscribeParagraphLineSpacingEvents;
  AAction.Checked := True;
  ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing(AAction.Tag);
  SubscribeParagraphLineSpacingEvents;
end;

procedure TdxRichEditEditStyleDialogForm.SubscribeControlsEvents;
begin
  cmbCurrentStyle.Properties.OnChange := CurrentStylePropertiesChange;
  edtName.Properties.OnChange := NameStylePropertiesChange;
  cmbParent.Properties.OnChange := ParentStylePropertiesChange;
  if IsParagraphStyle then
    cmbNextStyle.Properties.OnChange := NextStylePropertiesChange;
  cmbFontSize.Properties.OnEditValueChanged := FontSizePropertiesChange;
end;

procedure TdxRichEditEditStyleDialogForm.SubscribeParagraphAlignmentEvents;
begin
  aToggleParagraphAlignmentCenter.OnExecute := ToggleParagraphAlignmentExecute;
  aToggleParagraphAlignmentJustify.OnExecute := ToggleParagraphAlignmentExecute;
  aToggleParagraphAlignmentLeft.OnExecute := ToggleParagraphAlignmentExecute;
  aToggleParagraphAlignmentRight.OnExecute := ToggleParagraphAlignmentExecute;
end;

procedure TdxRichEditEditStyleDialogForm.SubscribeParagraphLineSpacingEvents;
begin
  aSetSingleParagraphSpacing.OnExecute := SetParagraphSpacingExecute;
  aSetSesquialteralParagraphSpacing.OnExecute := SetParagraphSpacingExecute;
  aSetDoubleParagraphSpacing.OnExecute := SetParagraphSpacingExecute;
end;

procedure TdxRichEditEditStyleDialogForm.SubscribeToggleButtonsEvents;
begin
  aToggleFontBold.OnExecute := ToggleFontBoldExecute;
  aToggleFontItalic.OnExecute := ToggleFontItalicExecute;
  aToggleFontUnderline.OnExecute := ToggleFontUnderlineExecute;
end;

procedure TdxRichEditEditStyleDialogForm.ToggleFontBoldExecute(Sender: TObject);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AAccessor := Controller.GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontBold(aToggleFontBold.Checked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.ToggleFontItalicExecute(Sender: TObject);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AAccessor := Controller.GetCharacterPropertyAccessor;
  try
    AAccessor.ChangeFontItalic(aToggleFontItalic.Checked);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.ToggleFontUnderlineExecute(Sender: TObject);
var
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AAccessor := Controller.GetCharacterPropertyAccessor;
  try
    if aToggleFontUnderline.Checked then
      AAccessor.ChangeFontUnderlineType(TdxUnderlineType.Single)
    else
      AAccessor.ChangeFontUnderlineType(TdxUnderlineType.None);
  finally
    AAccessor.Free;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.ToggleParagraphAlignmentExecute(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;
  UnsubscribeParagraphAlignmentEvents;
  AAction.Checked := True;
  ParagraphProperties.Alignment := TdxParagraphAlignment(AAction.Tag);
  SubscribeParagraphAlignmentEvents;
end;

procedure TdxRichEditEditStyleDialogForm.UnsubscribeControlsEvents;
begin
  cmbCurrentStyle.Properties.OnChange := nil;
  edtName.Properties.OnChange := nil;
  cmbParent.Properties.OnChange := nil;
  if IsParagraphStyle then
    cmbNextStyle.Properties.OnChange := nil;
  cmbFontSize.Properties.OnEditValueChanged := nil;
end;

procedure TdxRichEditEditStyleDialogForm.UnsubscribeParagraphAlignmentEvents;
begin
  aToggleParagraphAlignmentCenter.OnExecute := nil;
  aToggleParagraphAlignmentJustify.OnExecute := nil;
  aToggleParagraphAlignmentLeft.OnExecute := nil;
  aToggleParagraphAlignmentRight.OnExecute := nil;
end;

procedure TdxRichEditEditStyleDialogForm.UnsubscribeParagraphLineSpacingEvents;
begin
  aSetSingleParagraphSpacing.OnExecute := nil;
  aSetSesquialteralParagraphSpacing.OnExecute := nil;
  aSetDoubleParagraphSpacing.OnExecute := nil;
end;

procedure TdxRichEditEditStyleDialogForm.UnsubscribeToggleButtonsEvents;
begin
  aToggleFontBold.OnExecute := nil;
  aToggleFontItalic.OnExecute := nil;
  aToggleFontUnderline.OnExecute := nil;
end;

procedure TdxRichEditEditStyleDialogForm.UpdateCharacterBars(const AMergedCharacterProperties: TdxCharacterFormattingInfo);
begin
  UnsubscribeToggleButtonsEvents;
  aToggleFontBold.Checked := AMergedCharacterProperties.FontBold;
  aToggleFontItalic.Checked := AMergedCharacterProperties.FontItalic;
  aToggleFontUnderline.Checked := AMergedCharacterProperties.FontUnderlineType = TdxUnderlineType.Single;
  SubscribeToggleButtonsEvents;
  cmbFontEdit.EditValue := AMergedCharacterProperties.FontName;
  cmbFontColor.ColorValue := TdxAlphaColors.ToColor(AMergedCharacterProperties.ForeColor);
  cmbFontSize.EditValue := AMergedCharacterProperties.DoubleFontSize / 2;
end;

procedure TdxRichEditEditStyleDialogForm.UpdateFormCore;
begin
  edtName.Text := Controller.StyleName;
  if IsParagraphStyle then
  begin
    cmbCurrentStyle.ItemObject := Controller.ParagraphSourceStyle;
    cmbParent.ItemObject := Controller.ParagraphSourceStyle.Parent;
    cmbNextStyle.ItemObject := Controller.ParagraphSourceStyle.NextParagraphStyle;
  end
  else
  begin
    cmbCurrentStyle.ItemObject := Controller.CharacterSourceStyle;
    if Controller.CharacterSourceStyle.Parent = nil then
      cmbParent.ItemIndex := 0
    else
      cmbParent.ItemObject := Controller.CharacterSourceStyle.Parent;
  end;
end;

procedure TdxRichEditEditStyleDialogForm.UpdateRichEditBars;
var
  AParagraphProperties: TdxParagraphFormattingInfo;
  AMergedCharacterProperties: TdxCharacterFormattingInfo;
  AAlignment: TdxParagraphAlignment;
  ASpacing: TdxParagraphLineSpacing;
begin
  if Controller.IsParagraphStyle then
  begin
    BarEnabled(True);

    AMergedCharacterProperties := Controller.GetIntermediateMergedCharacterProperties(IntermediateParagraphStyle);
    try
      UpdateCharacterBars(AMergedCharacterProperties);
    finally
      AMergedCharacterProperties.Free;
    end;

    AParagraphProperties := Controller.GetIntermediateMergedParagraphProperties(IntermediateParagraphStyle);
    try
      AAlignment := AParagraphProperties.Alignment;
    finally
      AParagraphProperties.Free;
    end;
    ASpacing := ParagraphProperties.LineSpacingType;
    UnsubscribeParagraphAlignmentEvents;
    case AAlignment of
      TdxParagraphAlignment.Left:
        aToggleParagraphAlignmentLeft.Checked := True;
      TdxParagraphAlignment.Right:
        aToggleParagraphAlignmentRight.Checked := True;
      TdxParagraphAlignment.Center:
        aToggleParagraphAlignmentRight.Checked := True;
      TdxParagraphAlignment.Justify:
        aToggleParagraphAlignmentJustify.Checked := True;
    end;
    SubscribeParagraphAlignmentEvents;

    UnsubscribeParagraphLineSpacingEvents;
    aSetSingleParagraphSpacing.Checked := ASpacing = TdxParagraphLineSpacing.Single;
    aSetSesquialteralParagraphSpacing.Checked := ASpacing = TdxParagraphLineSpacing.Sesquialteral;
    aSetDoubleParagraphSpacing.Checked := ASpacing = TdxParagraphLineSpacing.Double;
    SubscribeParagraphLineSpacingEvents;
  end
  else
  begin
    AMergedCharacterProperties := Controller.GetIntermediateMergedCharacterProperties(IntermediateCharacterStyle);
    try
      UpdateCharacterBars(AMergedCharacterProperties);
    finally
      AMergedCharacterProperties.Free;
    end;

    BarEnabled(False);
  end;
end;

end.
