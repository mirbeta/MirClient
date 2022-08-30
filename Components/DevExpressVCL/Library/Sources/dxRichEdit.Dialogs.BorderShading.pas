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

unit dxRichEdit.Dialogs.BorderShading;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus, Buttons,
  dxCore, dxCoreGraphics, cxGeometry, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, StdCtrls, cxButtons,
  cxContainer, cxEdit, cxListBox, dxLayoutcxEditAdapters, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  dxColorEdit, cxLabel, ImgList, cxImageComboBox,
  dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.BorderShadingController,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.BorderShadingHelper,
  dxRichEditBorderLineWeightComboBox, cxImageList, cxCustomListBox;

type
  TdxRichEditBorderShadingDialogForm = class(TdxRichEditCustomDialogForm)
    btnAll: TcxButton;
    btnBox: TcxButton;
    btnCancel: TcxButton;
    btnCustom: TcxButton;
    btnGrid: TcxButton;
    btnHorizontBorderDown: TcxButton;
    btnHorizontBorderIn: TcxButton;
    btnHorizontBorderUp: TcxButton;
    btnNone: TcxButton;
    btnOk: TcxButton;
    btnOptions: TcxButton;
    btnVerticalBorderIn: TcxButton;
    btnVerticalBorderLeft: TcxButton;
    btnVerticalBorderRight: TcxButton;
    cmbApplyTo: TcxComboBox;
    cmbBorderLineColor: TdxColorEdit;
    cmbBorderLineWeight: TdxBorderLineWeightComboBox;
    cmbShadingColor: TdxColorEdit;
    dxLayoutControl1Group1: TdxLayoutGroup;
    dxLayoutControl1Group10: TdxLayoutGroup;
    dxLayoutControl1Group11: TdxLayoutGroup;
    dxLayoutControl1Group12: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Group5: TdxLayoutGroup;
    dxLayoutControl1Group6: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group7: TdxLayoutGroup;
    dxLayoutControl1Group8: TdxLayoutGroup;
    dxLayoutControl1Group9: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item12: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    dxLayoutControl1Item18: TdxLayoutItem;
    dxLayoutControl1Item19: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item20: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    dxLayoutControl1SeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutControl1SeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutControl1SeparatorItem3: TdxLayoutSeparatorItem;
    ilBorderLineStyle: TcxImageList;
    lbBorderLineStyle: TcxListBox;
    lblBordersPreview: TdxLayoutSeparatorItem;
    lblBordersPreviewTxt: TcxLabel;
    lblShadingFill: TdxLayoutSeparatorItem;
    lblShadingPreview: TdxLayoutSeparatorItem;
    lbShadingPreviewTxt: TcxLabel;
    lcgBorders: TdxLayoutGroup;
    lcgShading: TdxLayoutGroup;
    lcgTabControl: TdxLayoutGroup;
    lciAll: TdxLayoutItem;
    lciApplyTo: TdxLayoutItem;
    lciBorderLineColor: TdxLayoutItem;
    lciBorderLineStyle: TdxLayoutItem;
    lciBorderLineWeight: TdxLayoutItem;
    lciBox: TdxLayoutItem;
    lciCustom: TdxLayoutItem;
    lciGrid: TdxLayoutItem;
    lciNone: TdxLayoutItem;
    lciOptions: TdxLayoutItem;
    pbBordersPreview: TPaintBox;
    pbShadingPreview: TPaintBox;
    procedure btnOkClick(Sender: TObject);
    procedure pbBordersPreviewPaint(Sender: TObject);
    procedure lcgTabControlTabChanged(Sender: TObject);
  private
    FBorderShadingDialogHelper: TdxRichEditBorderShadingDialogHelper;
    FBorderUserControl: TdxBorderShadingUserControlHelper;
    FPreviewBorderControl: TdxPreviewBorderShadingControlHelper;
    procedure PopulateApplyTo;
    function GetController: TdxBorderShadingFormController;

    procedure btnNoneClick(Sender: TObject);
    procedure btnBoxClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnGridClick(Sender: TObject);
    procedure btnCustomClick(Sender: TObject);
    procedure BorderUserControl_BorderLineUpChanged(Sender: TObject);
    procedure BorderUserControl_BorderLineDownChanged(Sender: TObject);
    procedure BorderUserControl_BorderLineHorizontalInChanged(Sender: TObject);
    procedure BorderUserControl_BorderLineLeftChanged(Sender: TObject);
    procedure BorderUserControl_BorderLineRightChanged(Sender: TObject);
    procedure BorderUserControl_BorderLineVerticalInChanged(Sender: TObject);
    procedure BorderLineStyleDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState);
    procedure BorderLineStyleChange(Sender: TObject);
    procedure BorderLineColorPropertiesChange(Sender: TObject);
    procedure BorderLineWeightPropertiesChange(Sender: TObject);
    procedure DoBorderChange;
    procedure ShadingColorPropertiesChange(Sender: TObject);
    procedure PreviewBorderControlChange(Sender: TObject);
    procedure PreviewShadingControlChange(Sender: TObject);
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;
    procedure InitializePreviewControl(AControl: TdxBorderShadingUserControlHelper);
    procedure SetButtonState;
    procedure SetInitialBorder(const ABorder: TdxBorderInfo);
    function IsDrawColumns: Boolean;
    function IsDrawParagraph: Boolean;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
  public
    destructor Destroy; override;
    property Controller: TdxBorderShadingFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils,
  dxRichEdit.Control;

{$R *.dfm}

{ TdxRichEditBorderShadingDialogForm }

procedure TdxRichEditBorderShadingDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lblBordersPreview.Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogPreview);
  lblBordersPreviewTxt.Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogPreviewTxt);
  btnOptions.Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogButtonOptions);
  lblShadingFill.Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogShadingFill);
  lblShadingPreview.Caption := cxGetResourceString(@sdxRichEditBorderShadingDialogPreview);
  lcgBorders.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogBorders);
  lcgShading.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogShading);
  lciNone.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogNone);
  lciBox.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogBox);
  lciAll.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogAll);
  lciGrid.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogGrid);
  lciCustom.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogCustom);
  lciBorderLineStyle.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogBorderLineStyle);
  lciBorderLineColor.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogBorderLineColor);
  lciBorderLineWeight.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogBorderLineWeight);
  lciApplyTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBorderShadingDialogApplyTo);

  PopulateApplyTo;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderLineColorPropertiesChange(Sender: TObject);
var
  AColor: TColor;
begin
  AColor := cmbBorderLineColor.ColorValue;
  FBorderShadingDialogHelper.Color := TdxAlphaColors.FromColor(AColor);
  DoBorderChange;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderLineStyleChange(Sender: TObject);
var
  ABorder: TdxBorderInfo;
begin
  ABorder := TdxBorderInfo(lbBorderLineStyle.Items.Objects[lbBorderLineStyle.ItemIndex]).Clone;
  try
    if Assigned(FBorderShadingDialogHelper.Border) then
    begin
      ABorder.Color := FBorderShadingDialogHelper.Color;
      ABorder.Width := FBorderShadingDialogHelper.Width;
    end;
    SetInitialBorder(ABorder);
    DoBorderChange;
  finally
    ABorder.Free;
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderLineStyleDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  ABorderInfo: TdxBorderInfo;
begin
  ACanvas.FillRect(ARect);
  ABorderInfo := AControl.Items.Objects[AIndex] as TdxBorderInfo;
  TdxBorderLineWeightPainter.DrawBorderLineItem(ABorderInfo, ACanvas, ARect, UnitConverter, False);

  if (AIndex = AControl.ItemIndex) and not AControl.Focused then
    ACanvas.DrawFocusRect(ARect);
end;

procedure TdxRichEditBorderShadingDialogForm.BorderLineWeightPropertiesChange(Sender: TObject);
var
  AItem: TcxImageComboBoxItem;
begin
  AItem := cmbBorderLineWeight.Properties.Items[cmbBorderLineWeight.ItemIndex];
  FBorderShadingDialogHelper.Width := AItem.Value;
  DoBorderChange;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineDownChanged(Sender: TObject);
begin
  Controller.BorderLineDown := FBorderUserControl.BorderLineDown;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineHorizontalInChanged(Sender: TObject);
begin
  Controller.BorderLineHorizontalIn := FBorderUserControl.BorderLineHorizontalIn;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineLeftChanged(Sender: TObject);
begin
  Controller.BorderLineLeft := FBorderUserControl.BorderLineLeft;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineRightChanged(Sender: TObject);
begin
  Controller.BorderLineRight := FBorderUserControl.BorderLineRight;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineUpChanged(Sender: TObject);
begin
  Controller.BorderLineUp := FBorderUserControl.BorderLineUp;
end;

procedure TdxRichEditBorderShadingDialogForm.BorderUserControl_BorderLineVerticalInChanged(Sender: TObject);
begin
  Controller.BorderLineVerticalIn := FBorderUserControl.BorderLineVerticalIn;
end;

procedure TdxRichEditBorderShadingDialogForm.btnAllClick(Sender: TObject);
begin
  if btnAll.SpeedButtonOptions.Down then
  begin
    FBorderUserControl.SetMode(TdxSetMode.None);
    FBorderUserControl.SetMode(TdxSetMode.All);
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.btnBoxClick(Sender: TObject);
begin
  if btnBox.SpeedButtonOptions.Down then
  begin
    FBorderUserControl.SetMode(TdxSetMode.All);
    FBorderUserControl.SetMode(TdxSetMode.None);
    FBorderUserControl.SetMode(TdxSetMode.Box);
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.btnCustomClick(Sender: TObject);
begin
  if btnCustom.SpeedButtonOptions.Down then
    FBorderUserControl.SetMode(TdxSetMode.Custom);
end;

procedure TdxRichEditBorderShadingDialogForm.btnGridClick(Sender: TObject);
begin
  if btnGrid.SpeedButtonOptions.Down then
  begin
    FBorderUserControl.SetMode(TdxSetMode.None);
    FBorderUserControl.SetMode(TdxSetMode.Grid);
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.btnNoneClick(Sender: TObject);
begin
  if btnNone.SpeedButtonOptions.Down then
  begin
    FBorderUserControl.SetMode(TdxSetMode.All);
    FBorderUserControl.SetMode(TdxSetMode.None);
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

function TdxRichEditBorderShadingDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxBorderShadingFormController.Create(AControllerParameters as TdxBorderShadingFormControllerParameters);
end;

destructor TdxRichEditBorderShadingDialogForm.Destroy;
begin
  FBorderShadingDialogHelper.Free;
  FBorderUserControl.Free;
  FPreviewBorderControl.Free;
  inherited Destroy;
end;

procedure TdxRichEditBorderShadingDialogForm.DoBorderChange;
begin
  FBorderUserControl.CurrentBorderInfo := FBorderShadingDialogHelper.Border;
  pbBordersPreview.Invalidate;
end;

function TdxRichEditBorderShadingDialogForm.GetController: TdxBorderShadingFormController;
begin
  Result := TdxBorderShadingFormController(inherited Controller);
end;

procedure TdxRichEditBorderShadingDialogForm.InitializeForm;
begin
  FBorderShadingDialogHelper := TdxRichEditBorderShadingDialogHelper.Create;
  FBorderShadingDialogHelper.Control := Control as TdxCustomRichEditControl;
  FBorderShadingDialogHelper.PopulateBorderLineStyle(ilBorderLineStyle, lbBorderLineStyle.Items);

  FPreviewBorderControl := TdxPreviewBorderShadingControlHelper.Create;
  FPreviewBorderControl.Control := Control as TdxCustomRichEditControl;

  FBorderUserControl := TdxBorderShadingUserControlHelper.Create(FPreviewBorderControl);
  FBorderUserControl.Initialize(btnHorizontBorderUp, btnHorizontBorderDown, btnHorizontBorderIn, btnVerticalBorderLeft,
    btnVerticalBorderRight, btnVerticalBorderIn);
  FBorderUserControl.DocumentModel := Controller.DocumentModel;

  SetInitialBorder(Controller.GetInitialBorder);
  InitializePreviewControl(FBorderUserControl);
  FBorderUserControl.ButtonsVisible := True;

  cmbShadingColor.ColorValue := TdxAlphaColors.ToColor(Controller.GetActiveColor);

  SetButtonState;
  FBorderUserControl.Mode := Controller.SetModeButton;
end;

procedure TdxRichEditBorderShadingDialogForm.InitializePreviewControl(AControl: TdxBorderShadingUserControlHelper);
begin
  AControl.BeginUpdate;
  try
    AControl.BorderLineUp := Controller.BorderLineUp;
    AControl.BorderLineDown := Controller.BorderLineDown;
    AControl.BorderLineHorizontalIn := Controller.BorderLineHorizontalIn;
    AControl.BorderLineLeft := Controller.BorderLineLeft;
    AControl.BorderLineRight := Controller.BorderLineRight;
    AControl.BorderLineVerticalIn := Controller.BorderLineVerticalIn;
    AControl.BorderLineHorizontalInVisible := Controller.BorderLineHorizontalInVisible;
    AControl.BorderLineVerticalInVisible := Controller.BorderLineVerticalInVisible;
    AControl.FillColor := Controller.GetActiveColor;
    AControl.DrawColumns := IsDrawColumns;
    if not AControl.DrawColumns then
      AControl.DrawParagraph := IsDrawParagraph;
  finally
    AControl.EndUpdate;
  end;
end;

function TdxRichEditBorderShadingDialogForm.IsDrawColumns: Boolean;
begin
  Result := Controller.BorderLineVerticalInVisible;
end;

function TdxRichEditBorderShadingDialogForm.IsDrawParagraph: Boolean;
begin
  Result := Controller.BorderLineHorizontalInVisible;
end;

procedure TdxRichEditBorderShadingDialogForm.lcgTabControlTabChanged(Sender: TObject);
begin
  if lcgTabControl.ItemIndex = 0 then
    FPreviewBorderControl.OnChange := PreviewBorderControlChange
  else
    FPreviewBorderControl.OnChange := PreviewShadingControlChange;
end;

procedure TdxRichEditBorderShadingDialogForm.pbBordersPreviewPaint(Sender: TObject);
var
  APreview: TPaintBox;
begin
  APreview := Sender as TPaintBox;
  cxPaintCanvas.BeginPaint(APreview.Canvas);
  try
    FPreviewBorderControl.Bounds := APreview.ClientRect;
    FPreviewBorderControl.Draw(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.PopulateApplyTo;
begin
  Populate(cmbApplyTo, procedure(AComboBox: TcxCustomComboBox)
  var
    P: Pointer;
  begin
    for P in dxShadingApplyToNames do
      AComboBox.Properties.Items.Add(cxGetResourceString(P));
  end);
end;

procedure TdxRichEditBorderShadingDialogForm.PreviewBorderControlChange(Sender: TObject);
begin
  pbBordersPreview.Invalidate;
end;

procedure TdxRichEditBorderShadingDialogForm.PreviewShadingControlChange(Sender: TObject);
begin
  pbShadingPreview.Invalidate;
end;

procedure TdxRichEditBorderShadingDialogForm.SetButtonState;
begin
  case Controller.SetModeButton of
    TdxSetMode.All:
      TdxBorderShadingUserControlHelper.SetButtonDown(btnAll, True);
    TdxSetMode.Box:
      TdxBorderShadingUserControlHelper.SetButtonDown(btnBox, True);
    TdxSetMode.Grid:
      TdxBorderShadingUserControlHelper.SetButtonDown(btnGrid, True);
    TdxSetMode.Custom:
      TdxBorderShadingUserControlHelper.SetButtonDown(btnCustom, True);
    TdxSetMode.None:
      TdxBorderShadingUserControlHelper.SetButtonDown(btnNone, True);
  end;
end;

procedure TdxRichEditBorderShadingDialogForm.SetInitialBorder(const ABorder: TdxBorderInfo);

  procedure SetBorderLineStyleIndex(AStyle: TdxBorderLineStyle);
  var
    I: Integer;
    AIndex: Integer;
    ABorder: TdxBorderInfo;
  begin
    AIndex := -1;
    for I := 0 to lbBorderLineStyle.Items.Count - 1 do
    begin
      ABorder := TdxBorderInfo(lbBorderLineStyle.Items.Objects[I]);
      if ABorder.Style = AStyle then
      begin
        AIndex := I;
        Break;
      end;
      if ABorder.Style = TdxBorderLineStyle.Single then
        AIndex := I;
    end;
    lbBorderLineStyle.ItemIndex := AIndex;
  end;

begin
  FBorderShadingDialogHelper.Border := ABorder;
  TdxBorderLineWeightPainter.PopulateBorderLineWeight(cmbBorderLineWeight.Properties, ABorder.Style, ABorder.Color,
    UnitConverter, FBorderShadingDialogHelper.BordersLineWeights);
  FBorderUserControl.CurrentBorderInfo := ABorder;
  SetBorderLineStyleIndex(ABorder.Style);
  cmbBorderLineColor.ColorValue := TdxAlphaColors.ToColor(ABorder.Color);
  cmbBorderLineWeight.EditValue := ABorder.Width;
end;

procedure TdxRichEditBorderShadingDialogForm.ShadingColorPropertiesChange(Sender: TObject);
var
  AColor: TColor;
  AAlphaColor: TdxAlphaColor;
begin
  AColor := cmbShadingColor.ColorValue;
  AAlphaColor := TdxAlphaColors.FromColor(AColor);
  Controller.FillColor := AAlphaColor;
  FPreviewBorderControl.FillColor := AAlphaColor;
end;

procedure TdxRichEditBorderShadingDialogForm.SubscribeControlsEvents;
begin
  btnNone.OnClick := btnNoneClick;
  btnBox.OnClick := btnBoxClick;
  btnAll.OnClick := btnAllClick;
  btnGrid.OnClick := btnGridClick;
  btnCustom.OnClick := btnCustomClick;
  FBorderUserControl.OnBorderLineUpChanged := BorderUserControl_BorderLineUpChanged;
  FBorderUserControl.OnBorderLineDownChanged := BorderUserControl_BorderLineDownChanged;
  FBorderUserControl.OnBorderLineHorizontalInChanged := BorderUserControl_BorderLineHorizontalInChanged;
  FBorderUserControl.OnBorderLineLeftChanged := BorderUserControl_BorderLineLeftChanged;
  FBorderUserControl.OnBorderLineRightChanged := BorderUserControl_BorderLineRightChanged;
  FBorderUserControl.OnBorderLineVerticalInChanged := BorderUserControl_BorderLineVerticalInChanged;
  FPreviewBorderControl.OnChange := PreviewBorderControlChange;
  lbBorderLineStyle.OnDrawItem := BorderLineStyleDrawItem;
  lbBorderLineStyle.OnClick := BorderLineStyleChange;
  cmbBorderLineColor.Properties.OnChange := BorderLineColorPropertiesChange;
  cmbBorderLineWeight.Properties.OnChange := BorderLineWeightPropertiesChange;
  cmbShadingColor.Properties.OnChange := ShadingColorPropertiesChange;
end;

procedure TdxRichEditBorderShadingDialogForm.UnsubscribeControlsEvents;
begin
  btnNone.OnClick := nil;
  btnBox.OnClick := nil;
  btnAll.OnClick := nil;
  btnGrid.OnClick := nil;
  btnCustom.OnClick := nil;
  FBorderUserControl.OnBorderLineUpChanged := nil;
  FBorderUserControl.OnBorderLineDownChanged := nil;
  FBorderUserControl.OnBorderLineHorizontalInChanged := nil;
  FBorderUserControl.OnBorderLineLeftChanged := nil;
  FBorderUserControl.OnBorderLineRightChanged := nil;
  FBorderUserControl.OnBorderLineVerticalInChanged := nil;
  lbBorderLineStyle.OnDrawItem := nil;
  lbBorderLineStyle.OnClick := nil;
  cmbBorderLineColor.Properties.OnChange := nil;
  cmbBorderLineWeight.Properties.OnChange := nil;
  cmbShadingColor.Properties.OnChange := nil;
end;

end.
