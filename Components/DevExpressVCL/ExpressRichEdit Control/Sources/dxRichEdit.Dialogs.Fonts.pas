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

unit dxRichEdit.Dialogs.Fonts;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, ActnList, Menus,
  Controls, Forms, Dialogs, dxCore, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels,
  cxClasses, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxDropDownEdit, dxCoreClasses, cxTextEdit, cxMaskEdit, cxFontNameComboBox, dxLayoutContainer, StdCtrls, dxLayoutControl,
  cxLabel, dxColorEdit, dxLayoutLookAndFeels, cxCheckBox, cxButtons, dxCoreGraphics, cxListBox,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Dialogs.UnderlinePainter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEditFontNameComboBox,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Control,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.FontsFormController,
  dxRichEdit.Dialogs.FontsHelper,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.DocumentModel.Styles;

type
  TdxRichEditFontDialogForm = class(TdxRichEditCustomDialogForm, IdxRichEditFontDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbAllCaps: TcxCheckBox;
    cbDoubleStrikethrough: TcxCheckBox;
    cbHidden: TcxCheckBox;
    cbStrikethrough: TcxCheckBox;
    cbSubscript: TcxCheckBox;
    cbSuperscript: TcxCheckBox;
    cbUnderlineWordsOnly: TcxCheckBox;
    cmbFontColor: TdxColorEdit;
    cmbFontName: TcxTextEdit;
    cmbFontSize: TcxTextEdit;
    cmbFontStyle: TcxTextEdit;
    cmbUnderlineColor: TdxColorEdit;
    cmbUnderlineStyle: TcxComboBox;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group10: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Group5: TdxLayoutGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    dxLayoutControl1Group7: TdxLayoutGroup;
    dxLayoutControl1Group8: TdxLayoutGroup;
    dxLayoutControl1Group9: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item12: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    liFontNameWarning: TdxLayoutLabeledItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    lbFontName: TcxListBox;
    lbFontSize: TcxListBox;
    lbFontStyle: TcxListBox;
    lblEffects: TdxLayoutSeparatorItem;
    lblPreview: TdxLayoutSeparatorItem;
    lciFontColor: TdxLayoutItem;
    lciFontName: TdxLayoutItem;
    lciFontSize: TdxLayoutItem;
    lciFontStyle: TdxLayoutItem;
    lciUnderlineColor: TdxLayoutItem;
    lciUnderlineStyle: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    srePreview: TdxSimpleRichEditControl;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private const
    AutoButtonIndex = 1;
  private
    procedure AddAutoButton(AColorComboBox: TdxColorEdit);
    procedure cbColorComboBoxButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure DoSetUnderlineStyles;
    function GetFontName: TFontName;
    function GetFontUnderlineType: TdxNullableUnderlineType;
    function GetController: TdxFontFormController;
    function GetFontNameAllowed: Boolean;
    function GetFontStyleEdit: TdxFontStyleEditHelper;
    function GetFontEffectsEdit: TdxRichEditFontEffectsHelper;
    function GetUnderlineStyleHelper: TdxUnderlinePainterHelper;
    function GetFontForeColor: TdxNullableColor;
    function GetFontUnderlineColor: TdxNullableColor;
    function GetFontSize: TdxNullableSingle;
    function GetOnSomeChildControlEditValueChanged: TNotifyEvent;
    function GetOnFontSizeValidating: TcxEditValidateEvent;
    procedure SetFontForeColor(const Value: TdxNullableColor);
    procedure SetFontName(const Value: TFontName);
    procedure SetFontNameAllowed(const Value: Boolean);
    procedure SetFontSize(const Value: TdxNullableSingle);
    procedure SetFontUnderlineColor(const Value: TdxNullableColor);
    procedure SetFontUnderlineType(const Value: TdxNullableUnderlineType);
    procedure SetOnSomeChildControlEditValueChanged(const Value: TNotifyEvent);
    procedure SetOnFontSizeValidating(const Value: TcxEditValidateEvent);
    function GetOnFontStyleValidating: TcxEditValidateEvent;
    procedure SetOnFontStyleValidating(const Value: TcxEditValidateEvent);
  protected
    FStyle: TdxCharacterStyle;
    FFontNameHelper: TdxRichEditFontNameHelper;
    FFontSizeHelper: TdxRichEditFontSizeHelper;
    FFontStyleEditHelper: TdxFontStyleEditHelper;
    FFontEffectsHelper: TdxRichEditFontEffectsHelper;
    FUnderlineStyleHelper: TdxUnderlinePainterHelper;
    FRichEditFontHelper: TdxRichEditFontHelper;
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure FontControlChanged(Sender: TObject);
    procedure InitializeForm; override;
    procedure SubscribeControlsEvents; override;
    procedure UpdateRichEditFontControl;
    procedure UpdateFormCore; override;
    procedure UpdateFontNameWarning;
    procedure UpdatePreviewControl;
    procedure UnsubscribeControlsEvents; override;

    property OnFontStyleValidating: TcxEditValidateEvent read GetOnFontStyleValidating write SetOnFontStyleValidating;
    property OnFontSizeValidating: TcxEditValidateEvent read GetOnFontSizeValidating write SetOnFontSizeValidating;
    property OnSomeChildControlEditValueChanged: TNotifyEvent read GetOnSomeChildControlEditValueChanged write SetOnSomeChildControlEditValueChanged;
  public
    destructor Destroy; override;
    property Controller: TdxFontFormController read GetController;
    property FontName: TFontName read GetFontName write SetFontName;
    property FontNameAllowed: Boolean read GetFontNameAllowed write SetFontNameAllowed;
    property FontSize: TdxNullableSingle read GetFontSize write SetFontSize;
    property FontForeColor: TdxNullableColor read GetFontForeColor write SetFontForeColor;
    property FontUnderlineType: TdxNullableUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property FontUnderlineColor: TdxNullableColor read GetFontUnderlineColor write SetFontUnderlineColor;

    property FontEffectsEdit: TdxRichEditFontEffectsHelper read GetFontEffectsEdit;
    property FontSizeHelper: TdxRichEditFontSizeHelper read FFontSizeHelper;
    property FontStyleEdit: TdxFontStyleEditHelper read GetFontStyleEdit;
    property FontUnderlineStyleEdit: TdxUnderlinePainterHelper read GetUnderlineStyleHelper;
  end;

implementation

uses
  cxDrawTextUtils,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Options,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Types, dxRichEdit.Platform.Win.FontCache;

{$R *.dfm}

{ TdxRichEditFontDialogForm }

procedure TdxRichEditFontDialogForm.AddAutoButton(AColorComboBox: TdxColorEdit);
var
  AItem: TcxEditButton;
begin
  AItem := AColorComboBox.Properties.Buttons.Add;
  AItem.Kind := bkText;
  AItem.Caption := cxGetResourceString(@sdxRichEditFontDialogButtonColorAuto);
  AItem.Width := cxTextWidth(AColorComboBox.Style.GetVisibleFont, AItem.Caption) + 8 * cxTextSpace;
  AColorComboBox.Properties.OnButtonClick := cbColorComboBoxButtonClick;
end;

procedure TdxRichEditFontDialogForm.cbColorComboBoxButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = AutoButtonIndex then
    (Sender as TdxColorEdit).EditValue := Null;
end;

procedure TdxRichEditFontDialogForm.DoSetUnderlineStyles;
var
  ARichEditControl: TdxCustomRichEditControl;
begin
  ARichEditControl := Control as TdxCustomRichEditControl;
  if Assigned(ARichEditControl) then
    FUnderlineStyleHelper.UnderlineRepository := ARichEditControl.DocumentModel.UnderlineRepository;

  FUnderlineStyleHelper.Subscribe(cmbUnderlineStyle);
end;

function TdxRichEditFontDialogForm.GetController: TdxFontFormController;
begin
  Result := TdxFontFormController(inherited Controller);
end;

function TdxRichEditFontDialogForm.GetFontEffectsEdit: TdxRichEditFontEffectsHelper;
begin
  Result := FFontEffectsHelper;
end;

function TdxRichEditFontDialogForm.GetFontForeColor: TdxNullableColor;
begin
  if VarIsNull(cmbFontColor.EditValue) then
    Result := TdxNullableColor.Null
  else
    Result := TdxAlphaColors.FromColor(cmbFontColor.ColorValue);
end;

function TdxRichEditFontDialogForm.GetFontName: TFontName;
begin
  Result := cmbFontName.Text;
end;

function TdxRichEditFontDialogForm.GetFontNameAllowed: Boolean;
begin
  Result := cmbFontName.Enabled;
end;

function TdxRichEditFontDialogForm.GetFontSize: TdxNullableSingle;
var
  AValue: Integer;
  AIsValidValue: Boolean;
begin
  AIsValidValue := TdxRichEditFontSizeHelper.TryGetHalfSizeValue(cmbFontSize.EditingValue, AValue);
  if AIsValidValue then
    Result := AValue / 2.0
  else
    Result := TdxNullableSingle.Null;
end;

function TdxRichEditFontDialogForm.GetFontStyleEdit: TdxFontStyleEditHelper;
begin
  Result := FFontStyleEditHelper;
end;

function TdxRichEditFontDialogForm.GetFontUnderlineColor: TdxNullableColor;
begin
  if VarIsNull(cmbUnderlineColor.EditValue) then
    Result := TdxNullableColor.Null
  else
    Result := TdxAlphaColors.FromColor(cmbUnderlineColor.ColorValue);
end;

function TdxRichEditFontDialogForm.GetFontUnderlineType: TdxNullableUnderlineType;
begin
  Result := FUnderlineStyleHelper.UnderlineType;
end;

function TdxRichEditFontDialogForm.GetOnFontSizeValidating: TcxEditValidateEvent;
begin
  Result := cmbFontSize.Properties.OnValidate;
end;

function TdxRichEditFontDialogForm.GetOnFontStyleValidating: TcxEditValidateEvent;
begin
  Result := cmbFontStyle.Properties.OnValidate;
end;

function TdxRichEditFontDialogForm.GetOnSomeChildControlEditValueChanged: TNotifyEvent;
begin
  Result := FFontStyleEditHelper.OnChange;
end;

function TdxRichEditFontDialogForm.GetUnderlineStyleHelper: TdxUnderlinePainterHelper;
begin
  Result := FUnderlineStyleHelper;
end;

procedure TdxRichEditFontDialogForm.SetFontForeColor(const Value: TdxNullableColor);
begin
  if Value.IsNull then
    cmbFontColor.EditValue := Null
  else
    cmbFontColor.ColorValue := TdxAlphaColors.ToColor(Value.Value);
end;

procedure TdxRichEditFontDialogForm.SetFontName(const Value: TFontName);
begin
  cmbFontName.Text := Value;
end;

procedure TdxRichEditFontDialogForm.SetFontNameAllowed(const Value: Boolean);
begin
  cmbFontName.Enabled := Value;
end;

procedure TdxRichEditFontDialogForm.SetFontSize(const Value: TdxNullableSingle);
begin
  if Value.IsNull then
    cmbFontSize.EditValue := ''
  else
    cmbFontSize.EditValue := Value.Value;
end;

procedure TdxRichEditFontDialogForm.SetFontUnderlineColor(const Value: TdxNullableColor);
begin
  if Value.IsNull then
    cmbUnderlineColor.EditValue := Null
  else
    cmbUnderlineColor.ColorValue := TdxAlphaColors.ToColor(Value.Value);
end;

procedure TdxRichEditFontDialogForm.SetFontUnderlineType(const Value: TdxNullableUnderlineType);
begin
  FUnderlineStyleHelper.UnderlineType := Value;
end;

procedure TdxRichEditFontDialogForm.SetOnFontSizeValidating(const Value: TcxEditValidateEvent);
begin
  cmbFontSize.Properties.OnValidate := Value;
end;

procedure TdxRichEditFontDialogForm.SetOnFontStyleValidating(const Value: TcxEditValidateEvent);
begin
  cmbFontStyle.Properties.OnValidate := Value;
end;

procedure TdxRichEditFontDialogForm.SetOnSomeChildControlEditValueChanged(const Value: TNotifyEvent);
begin
  FFontNameHelper.OnChange := Value;
  FFontStyleEditHelper.OnChange := Value;
  FFontSizeHelper.OnChange := Value;
  cmbUnderlineStyle.Properties.OnChange := Value;
  cmbFontColor.Properties.OnChange := Value;
  cmbUnderlineColor.Properties.OnChange := Value;
  FFontEffectsHelper.OnEffectsChanged := Value;
end;

procedure TdxRichEditFontDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditFontDialogForm);
  lblEffects.Caption := cxGetResourceString(@sdxRichEditFontDialogEffects);
  lblPreview.Caption := cxGetResourceString(@sdxRichEditFontDialogPreview);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lciFontName.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogFontName);
  lciFontStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogFontStyle);
  lciFontSize.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogFontSize);
  lciFontColor.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogFontColor);
  lciUnderlineStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogUnderlineStyle);
  lciUnderlineColor.CaptionOptions.Text := cxGetResourceString(@sdxRichEditFontDialogUnderlineColor);
  cbStrikethrough.Caption := cxGetResourceString(@sdxRichEditFontDialogStrikeout);
  cbDoubleStrikethrough.Caption := cxGetResourceString(@sdxRichEditFontDialogDoubleStrikeout);
  cbUnderlineWordsOnly.Caption := cxGetResourceString(@sdxRichEditFontDialogUnderlineWordsOnly);
  cbSuperscript.Caption := cxGetResourceString(@sdxRichEditFontDialogSuperscript);
  cbSubscript.Caption := cxGetResourceString(@sdxRichEditFontDialogSubscript);
  cbAllCaps.Caption := cxGetResourceString(@sdxRichEditFontDialogAllCaps);
  cbHidden.Caption := cxGetResourceString(@sdxRichEditFontDialogHidden);
end;

function TdxRichEditFontDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxFontFormController.Create(AControllerParameters as TdxFontFormControllerParameters);
end;

procedure TdxRichEditFontDialogForm.FontControlChanged(Sender: TObject);
begin
  Controller.AllCaps := FRichEditFontHelper.AllCaps;
  Controller.Script := FRichEditFontHelper.Script;
  Controller.FontStrikeoutType := FRichEditFontHelper.Strikeout;
  Controller.FontName := FRichEditFontHelper.FontName;
  Controller.FontBold := FRichEditFontHelper.FontBold;
  Controller.FontItalic := FRichEditFontHelper.FontItalic;
  if not FRichEditFontHelper.FontSize.IsNull then
    Controller.DoubleFontSize := Trunc(FRichEditFontHelper.FontSize.Value * 2)
  else
    Controller.DoubleFontSize := TdxNullableInteger.Null;
  Controller.ForeColor := FRichEditFontHelper.FontForeColor;
  if (FRichEditFontHelper.FontUnderlineType = Controller.FontUnderlineType) and (Controller.FontUnderlineType <> TdxUnderlineType.None) then
    Controller.UnderlineColor := FRichEditFontHelper.FontUnderlineColor;
  Controller.Hidden := FRichEditFontHelper.Hidden;
  Controller.SetFontUnderline(FRichEditFontHelper.FontUnderlineType, FRichEditFontHelper.UnderlineWordsOnly);
  UpdateForm;
end;

procedure TdxRichEditFontDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := FFontStyleEditHelper.Validate and FFontSizeHelper.Validate;
    if CanClose then
    begin
      FRichEditFontHelper.DoFontControlChanged;
      Controller.ApplyChanges;
    end;
  end;
end;

procedure TdxRichEditFontDialogForm.InitializeForm;
var
  AIndex: Integer;
begin
  inherited InitializeForm;
  AddAutoButton(cmbFontColor);
  AddAutoButton(cmbUnderlineColor);

  FUnderlineStyleHelper := TdxUnderlinePainterHelper.Create;
  FFontNameHelper := TdxRichEditFontNameHelper.Create(cmbFontName, lbFontName);
  FFontNameHelper.PopulateItems;
  FFontStyleEditHelper := TdxFontStyleEditHelper.Create(cmbFontStyle, lbFontStyle);
  FFontEffectsHelper := TdxRichEditFontEffectsHelper.Create(cbAllCaps, cbDoubleStrikethrough, cbHidden, cbStrikethrough,
    cbSubscript, cbSuperscript, cbUnderlineWordsOnly);
  FFontSizeHelper := TdxRichEditFontSizeHelper.Create(cmbFontSize, lbFontSize);
  FRichEditFontHelper := TdxRichEditFontHelper.Create(Self);
  DoSetUnderlineStyles;

  FStyle := TdxCharacterStyle.Create(srePreview.DocumentModel, nil, 'DXCustomStyle');
  AIndex := srePreview.DocumentModel.CharacterStyles.Add(FStyle);
  srePreview.DocumentModel.ActivePieceTable.Runs[0].CharacterStyleIndex := AIndex;

  srePreview.Views.Simple.HidePartiallyVisibleRow := True;
end;

procedure TdxRichEditFontDialogForm.SubscribeControlsEvents;
begin
  FRichEditFontHelper.OnFontControlChanged := FontControlChanged;
end;

procedure TdxRichEditFontDialogForm.UpdateRichEditFontControl;
var
  ARichEditControl: TdxCustomRichEditControl;
  ARestrictions: TdxCharacterFormattingDetailedOptions;
  AUnderlineColorAllowed: Boolean;
begin
  ARichEditControl := Control as TdxCustomRichEditControl;
  if Assigned(ARichEditControl) then
  begin
    ARestrictions := ARichEditControl.DocumentModel.DocumentCapabilities.CharacterFormattingDetailed;
    if FFontEffectsHelper.CharacterFormattingDetailedOptions = nil then
      FFontEffectsHelper.CharacterFormattingDetailedOptions := ARestrictions;
    cmbFontColor.Enabled := ARestrictions.ForeColorAllowed;
    AUnderlineColorAllowed := ARestrictions.UnderlineColorAllowed;
    cmbFontSize.Enabled := ARestrictions.FontSizeAllowed;
    cmbFontName.Enabled := ARestrictions.FontNameAllowed;
    cmbUnderlineStyle.Enabled := ARestrictions.FontUnderlineAllowed;
  end
  else
    AUnderlineColorAllowed := True;
  FRichEditFontHelper.RichEditControl := ARichEditControl;
  FFontSizeHelper.RichEditControl := Control;

  FFontStyleEditHelper.FontFamilyName := cmbFontName.Text;
  lciUnderlineColor.Enabled := AUnderlineColorAllowed and not FUnderlineStyleHelper.UnderlineType.IsNull and
    (FUnderlineStyleHelper.UnderlineType <> TdxUnderlineType.None);
end;

procedure TdxRichEditFontDialogForm.UpdateFontNameWarning;
var
  AFontInfo: TdxTrueTypeFontInfo;
  AFontStyle: TdxSupportedFontStyle;
begin
  if lbFontName.ItemIndex < 0 then
    liFontNameWarning.Caption := cxGetResourceString(@sdxRichEditFontDialogFontNotInstalled)
  else
  begin
    AFontInfo := lbFontName.ItemObject as TdxTrueTypeFontInfo;
    AFontStyle := TdxFontStyleEditHelper.ToSupportedFontStyle(Controller.FontBold, Controller.FontItalic);
    if FontStyleEdit.FontStyles.HasValue and
      not AFontInfo.StylesInfo.NativeStyles[AFontStyle] then
      liFontNameWarning.Caption := cxGetResourceString(@sdxRichEditFontDialogFontStyleImitated)
    else
      liFontNameWarning.Caption := cxGetResourceString(@sdxRichEditFontDialogPrintNotes);
  end;
end;

procedure TdxRichEditFontDialogForm.UpdateFormCore;
begin
  FRichEditFontHelper.BeginUpdate;
  UpdateRichEditFontControl;
  FRichEditFontHelper.AllCaps := Controller.AllCaps;
  FRichEditFontHelper.Script := Controller.Script;
  FRichEditFontHelper.Strikeout := Controller.FontStrikeoutType;
  FRichEditFontHelper.FontName := Controller.FontName;
  FRichEditFontHelper.FontBold := Controller.FontBold;
  FRichEditFontHelper.FontItalic := Controller.FontItalic;
  if not Controller.DoubleFontSize.IsNull then
    FRichEditFontHelper.FontSize := Controller.DoubleFontSize.Value / 2.0
  else
    FRichEditFontHelper.FontSize := TdxNullableSingle.Null;
  FRichEditFontHelper.FontForeColor := Controller.ForeColor;
  FRichEditFontHelper.FontUnderlineColor := Controller.UnderlineColor;
  FRichEditFontHelper.FontUnderlineType := Controller.FontUnderlineType;
  FRichEditFontHelper.Hidden := Controller.Hidden;
  FRichEditFontHelper.UnderlineWordsOnly := Controller.UnderlineWordsOnly;
  UpdatePreviewControl;
  UpdateFontNameWarning;
  FRichEditFontHelper.EndUpdate;
end;

procedure TdxRichEditFontDialogForm.UpdatePreviewControl;
var
  APieceTable: TdxPieceTable;
  ATable: TdxTable;
  ARow: TdxTableRow;
  AHeight: TdxHeightUnit;
  AParagraph: TdxParagraph;
begin
  srePreview.DocumentModel.BeginUpdate;
  try
    APieceTable := srePreview.DocumentModel.MainPieceTable;
    ATable := APieceTable.Tables.First;
    if ATable <> nil then
      APieceTable.DeleteTableWithContent(ATable);
    ATable := APieceTable.InsertTable(0, 1, 1, TdxTableAutoFitBehaviorType.AutoFitToWindow, MinInt, MinInt, False, False);
    ARow := ATable.Rows[0];
    AHeight := ARow.Properties.Height;
    AHeight.&Type := TdxHeightUnitType.Minimum;
    AHeight.Value := srePreview.DocumentModel.UnitConverter.PixelsToModelUnits(srePreview.Height, Screen.PixelsPerInch);

    ARow.Cells[0].Properties.VerticalAlignment := TdxVerticalAlignment.Center;

    AParagraph := APieceTable.Paragraphs[0];
    AParagraph.FirstLineIndent := 0;
    AParagraph.LeftIndent := 0;
    AParagraph.Alignment := TdxParagraphAlignment.Center;
    if FontName <> '' then
    begin
      if not Controller.AllCaps.IsNull then
        FStyle.CharacterProperties.AllCaps := Controller.AllCaps.Value;
      if not Controller.Script.IsNull then
        FStyle.CharacterProperties.Script := Controller.Script.Value;
      if not Controller.FontStrikeoutType.IsNull then
        FStyle.CharacterProperties.FontStrikeoutType := Controller.FontStrikeoutType.Value;
      FStyle.CharacterProperties.FontName := Controller.FontName;
      if not Controller.FontBold.IsNull then
        FStyle.CharacterProperties.FontBold := Controller.FontBold.Value;
      if not Controller.FontItalic.IsNull then
        FStyle.CharacterProperties.FontItalic := Controller.FontItalic.Value;
      if not Controller.DoubleFontSize.IsNull then
        FStyle.CharacterProperties.DoubleFontSize := Controller.DoubleFontSize.Value;
      if not Controller.ForeColor.IsNull then
        FStyle.CharacterProperties.ForeColor := Controller.ForeColor.Value;
      if not Controller.UnderlineColor.IsNull then
        FStyle.CharacterProperties.UnderlineColor := Controller.UnderlineColor.Value;
      if not Controller.FontUnderlineType.IsNull then
        FStyle.CharacterProperties.FontUnderlineType := Controller.FontUnderlineType.Value;
      if not Controller.UnderlineWordsOnly.IsNull then
        FStyle.CharacterProperties.UnderlineWordsOnly := Controller.UnderlineWordsOnly.Value;
      APieceTable.InsertPlainText(0, FontName);
    end;
  finally
    srePreview.DocumentModel.EndUpdate;
  end;
end;

procedure TdxRichEditFontDialogForm.UnsubscribeControlsEvents;
begin
  FRichEditFontHelper.OnFontControlChanged := nil;
end;

destructor TdxRichEditFontDialogForm.Destroy;
begin
  FRichEditFontHelper.Free;
  FFontEffectsHelper.Free;
  FFontStyleEditHelper.Free;
  FUnderlineStyleHelper.Free;
  FFontSizeHelper.Free;
  FFontNameHelper.Free;
  inherited Destroy;
end;

end.
