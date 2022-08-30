{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetContainerCustomizationDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ActnList,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControl, cxClasses,
  dxLayoutLookAndFeels, dxLayoutControlAdapters, cxButtons, dxSpreadSheetCore, cxRadioGroup, dxColorDialog,
  dxCoreGraphics, dxGDIPlusClasses, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxImage, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxImageComboBox, ExtCtrls, dxSpreadSheetContainerCustomizationDialogHelpers, cxGroupBox, cxCalc,
  cxLabel, cxSpinEdit, cxCheckBox, dxSpreadSheetContainers, cxMemo, dxForms;

type

  { TdxSpreadSheetContainerCustomizationDialogForm }

  TdxSpreadSheetContainerCustomizationDialogForm = class(TdxForm)
    acTextureFillLoad: TAction;
    acTextureFillSave: TAction;
    alActions: TActionList;
    btnCancel: TcxButton;
    btnGradientFillAddStop: TcxButton;
    btnGradientFillColor: TcxButton;
    btnGradientFillRemoveStop: TcxButton;
    btnGradientLineAddStop: TcxButton;
    btnGradientLineColor: TcxButton;
    btnGradientLineRemoveStop: TcxButton;
    btnOK: TcxButton;
    btnReset: TcxButton;
    btnSolidFillColor: TcxButton;
    btnSolidLineColor: TcxButton;
    btnTextFont: TcxButton;
    btnTextureFillLoad: TcxButton;
    btnTextureFillSave: TcxButton;
    cbLockAspectRatio: TcxCheckBox;
    cbRelativeToPictureSize: TcxCheckBox;
    cbTextBoxAutoSize: TcxCheckBox;
    cbTextBoxHorzAlign: TcxComboBox;
    cbTextBoxVertAlign: TcxComboBox;
    cbTextBoxWordWrap: TcxCheckBox;
    ccbGradientFillDirection: TcxComboBox;
    ccbGradientLineDirection: TcxComboBox;
    ccbLineStyle: TcxComboBox;
    ceLineWidth: TcxSpinEdit;
    ColorDialog: TdxColorDialog;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    fdTextBoxFont: TFontDialog;
    imTextureFill: TcxImage;
    lbCrop: TcxLabel;
    lbOriginalSize: TcxLabel;
    lbPosition: TcxLabel;
    lbScale: TcxLabel;
    lbSizeAndRotate: TcxLabel;
    lbTextAlignment: TcxLabel;
    lbTextPadding: TcxLabel;
    lcgCrop: TdxLayoutGroup;
    lcgFill: TdxLayoutGroup;
    lcgGradientFill: TdxLayoutGroup;
    lcgGradientLine: TdxLayoutGroup;
    lcgLine: TdxLayoutGroup;
    lcgOriginalSize: TdxLayoutGroup;
    lcgProperties: TdxLayoutGroup;
    lcgSize: TdxLayoutGroup;
    lcgSolidFill: TdxLayoutItem;
    lcgSolidLine: TdxLayoutItem;
    lcgTabs: TdxLayoutGroup;
    lcgText: TdxLayoutGroup;
    lcgTextBox: TdxLayoutGroup;
    lcgTextureFill: TdxLayoutAutoCreatedGroup;
    lciCrop: TdxLayoutItem;
    lciCropBottom: TdxLayoutItem;
    lciCropLeft: TdxLayoutItem;
    lciCropRight: TdxLayoutItem;
    lciCropTop: TdxLayoutItem;
    lciGradientFillDirection: TdxLayoutItem;
    lciGradientFillStops: TdxLayoutItem;
    lciGradientLineDirection: TdxLayoutItem;
    lciGradientLineStops: TdxLayoutItem;
    lciHeight: TdxLayoutItem;
    lciLinePenStyle: TdxLayoutGroup;
    lciLineStyle: TdxLayoutItem;
    lciLineWidth: TdxLayoutItem;
    lciOriginalSize: TdxLayoutItem;
    lciPenGradientLine: TdxLayoutItem;
    lciPenNoLine: TdxLayoutItem;
    lciPenSolidLine: TdxLayoutItem;
    lciRelativeToPictureSize: TdxLayoutItem;
    lciRotation: TdxLayoutItem;
    lciScaleHeight: TdxLayoutItem;
    lciScaleWidth: TdxLayoutItem;
    lciTextBoxHorzAlign: TdxLayoutItem;
    lciTextBoxVertAlign: TdxLayoutItem;
    lciTextBoxWordWrap: TdxLayoutItem;
    lciTextPaddingBottom: TdxLayoutItem;
    lciTextPaddingLeft: TdxLayoutItem;
    lciTextPaddingRight: TdxLayoutItem;
    lciTextPaddingTop: TdxLayoutItem;
    lciWidth: TdxLayoutItem;
    lclOriginalSize: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup10: TdxLayoutAutoCreatedGroup;
    lcMainGroup11: TdxLayoutGroup;
    lcMainGroup12: TdxLayoutAutoCreatedGroup;
    lcMainGroup13: TdxLayoutGroup;
    lcMainGroup14: TdxLayoutAutoCreatedGroup;
    lcMainGroup15: TdxLayoutAutoCreatedGroup;
    lcMainGroup16: TdxLayoutGroup;
    lcMainGroup17: TdxLayoutGroup;
    lcMainGroup18: TdxLayoutGroup;
    lcMainGroup19: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutAutoCreatedGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainGroup5: TdxLayoutAutoCreatedGroup;
    lcMainGroup6: TdxLayoutAutoCreatedGroup;
    lcMainGroup7: TdxLayoutGroup;
    lcMainGroup8: TdxLayoutGroup;
    lcMainGroup9: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem10: TdxLayoutItem;
    lcMainItem12: TdxLayoutItem;
    lcMainItem13: TdxLayoutItem;
    lcMainItem14: TdxLayoutItem;
    lcMainItem16: TdxLayoutItem;
    lcMainItem17: TdxLayoutItem;
    lcMainItem18: TdxLayoutItem;
    lcMainItem19: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem20: TdxLayoutItem;
    lcMainItem21: TdxLayoutItem;
    lcMainItem22: TdxLayoutItem;
    lcMainItem23: TdxLayoutItem;
    lcMainItem24: TdxLayoutItem;
    lcMainItem25: TdxLayoutItem;
    lcMainItem26: TdxLayoutItem;
    lcMainItem27: TdxLayoutItem;
    lcMainItem28: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem31: TdxLayoutItem;
    lcMainItem33: TdxLayoutItem;
    lcMainItem34: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainItem8: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainSeparatorItem2: TdxLayoutSeparatorItem;
    lcMainSeparatorItem3: TdxLayoutSeparatorItem;
    meText: TcxMemo;
    rbAbsolute: TcxRadioButton;
    rbGradientFill: TcxRadioButton;
    rbGradientLine: TcxRadioButton;
    rbNoFill: TcxRadioButton;
    rbNoLine: TcxRadioButton;
    rbOneCell: TcxRadioButton;
    rbSolidFill: TcxRadioButton;
    rbSolidLine: TcxRadioButton;
    rbTextureFill: TcxRadioButton;
    rbTwoCells: TcxRadioButton;
    seCropBottom: TcxSpinEdit;
    seCropLeft: TcxSpinEdit;
    seCropRight: TcxSpinEdit;
    seCropTop: TcxSpinEdit;
    seHeight: TcxSpinEdit;
    seRotation: TcxSpinEdit;
    seScaleHeight: TcxSpinEdit;
    seScaleWidth: TcxSpinEdit;
    seTextPaddingBottom: TcxSpinEdit;
    seTextPaddingLeft: TcxSpinEdit;
    seTextPaddingRight: TcxSpinEdit;
    seTextPaddingTop: TcxSpinEdit;
    seWidth: TcxSpinEdit;
    TextureOpenDialog: TOpenDialog;
    TextureSaveDialog: TSaveDialog;

    procedure acTextureFillLoadExecute(Sender: TObject);
    procedure acTextureFillSaveExecute(Sender: TObject);
    procedure acTextureFillSaveUpdate(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure btnGradientFillAddStopClick(Sender: TObject);
    procedure btnGradientFillColorClick(Sender: TObject);
    procedure btnGradientFillRemoveStopClick(Sender: TObject);
    procedure btnGradientLineAddStopClick(Sender: TObject);
    procedure btnGradientLineColorClick(Sender: TObject);
    procedure btnGradientLineRemoveStopClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnTextFontClick(Sender: TObject);
    procedure cbLockAspectRatioClick(Sender: TObject);
    procedure cbRelativeToPictureSizeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbGradientLineClick(Sender: TObject);
    procedure rbTextureFillClick(Sender: TObject);
    procedure seCropHorzPropertiesChange(Sender: TObject);
    procedure seCropVertPropertiesChange(Sender: TObject);
    procedure seHeightPropertiesChange(Sender: TObject);
    procedure seScaleHeightPropertiesChange(Sender: TObject);
    procedure seScaleWidthPropertiesChange(Sender: TObject);
    procedure seWidthPropertiesChange(Sender: TObject);
  strict private
    FContainer: TdxSpreadSheetContainer;
    FGradientFillStops: TdxSpreadSheetGradientStops;
    FGradientLineStops: TdxSpreadSheetGradientStops;
    FLockChangeSize: Boolean;
    FPrevCropMargins: TRect;
    FScaleDenominator: Integer;
    FScaleNumerator: Integer;

    function GetContainerAsComment: TdxSpreadSheetCommentContainer;
    function GetContainerAsPicture: TdxSpreadSheetPictureContainer;
    function GetContainerAsShape: TdxSpreadSheetShapeContainer;
    function GetContainerAsTextBox: TdxSpreadSheetTextBoxContainer;
    function GetContainersTextBox: TdxSpreadSheetCustomTextBox;
    function GetCropMargins: TRect;
    procedure SetCropMargins(const Value: TRect);
  protected
    procedure ApplyLocalization;

    procedure GradientFillStopsDblClickHandler(Sender: TObject);
    procedure GradientFillStopsSelectionChangedHandler(Sender: TObject);
    procedure GradientLineStopsDblClickHandler(Sender: TObject);
    procedure GradientLineStopsSelectionChangedHandler(Sender: TObject);

    procedure LoadShape(AShape: TdxSpreadSheetShape);
    procedure LoadShapeBrush(ABrush: TdxGPBrush);
    procedure LoadShapePen(APen: TdxGPPen);
    procedure LoadSizeParams;
    procedure LoadTextBox(ATextBox: TdxSpreadSheetCustomTextBox);
    procedure SaveShape(AShape: TdxSpreadSheetShape);
    procedure SaveShapeBrush(ABrush: TdxGPBrush);
    procedure SaveShapePen(APen: TdxGPPen);
    procedure SaveSizeParams;
    procedure SaveTextBox(ATextBox: TdxSpreadSheetCustomTextBox);

    procedure PopulateGradientDirection(ACombobox: TcxComboBox);
    procedure PopulateHorzAlignment(ACombobox: TcxComboBox);
    procedure PopulatePenStyle(ACombobox: TcxComboBox);
    procedure PopulateVertAlignment(ACombobox: TcxComboBox);
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure SelectColor(AButton: TcxButton);
    procedure SelectGradientStopColor(AStops: TdxSpreadSheetGradientStops; AButton: TcxButton);
    procedure SelectPage(AActivePage: Integer);
    procedure SetSpinValue(ASpinEdit: TcxSpinEdit; const AValue: Variant);

    procedure UpdateColorButtonGlyph(AButton: TcxButton); overload;
    procedure UpdateColorButtonGlyph(AButton: TcxButton; AColor: TdxAlphaColor); overload;
    procedure UpdateColorButtonGlyphs;
    procedure UpdateOriginalSizeInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize(AContainer: TdxSpreadSheetContainer);
    procedure Load;
    procedure Save;
    //
    property Container: TdxSpreadSheetContainer read FContainer;
    property ContainerAsComment: TdxSpreadSheetCommentContainer read GetContainerAsComment;
    property ContainerAsPicture: TdxSpreadSheetPictureContainer read GetContainerAsPicture;
    property ContainerAsShape: TdxSpreadSheetShapeContainer read GetContainerAsShape;
    property ContainerAsTextBox: TdxSpreadSheetTextBoxContainer read GetContainerAsTextBox;
    property ContainersTextBox: TdxSpreadSheetCustomTextBox read GetContainersTextBox;
    property CropMargins: TRect read GetCropMargins write SetCropMargins;
    property GradientFillStops: TdxSpreadSheetGradientStops read FGradientFillStops;
    property GradientLineStops: TdxSpreadSheetGradientStops read FGradientLineStops;
  end;

function ShowContainerCustomizationDialog(AContainer: TdxSpreadSheetContainer; AActivePage: Integer = 0): Boolean;
implementation

uses
  Types, dxSpreadSheetDialogStrs, Math, dxTypeHelpers, cxGeometry, dxSpreadSheetCoreHistory, dxDPIAwareUtils;

{$R *.dfm}

type
  TcxSpinEditPropertiesAccess = class(TcxSpinEditProperties);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);

function ShowContainerCustomizationDialog(AContainer: TdxSpreadSheetContainer; AActivePage: Integer = 0): Boolean;
var
  ADialog: TdxSpreadSheetContainerCustomizationDialogForm;
begin
  ADialog := TdxSpreadSheetContainerCustomizationDialogForm.Create(GetParentForm(AContainer.SpreadSheet));
  try
    ADialog.Initialize(AContainer);
    ADialog.SelectPage(AActivePage);
    ADialog.Load;
    Result := ADialog.ShowModal = mrOk;
    if Result then
      ADialog.Save;
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetContainerCustomizationDialogForm }

constructor TdxSpreadSheetContainerCustomizationDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradientFillStops := TdxSpreadSheetGradientStops.Create(lcMain);
  FGradientFillStops.OnSelectionChanged := GradientFillStopsSelectionChangedHandler;
  FGradientFillStops.OnDblClick := GradientFillStopsDblClickHandler;
  lciGradientFillStops.Control := GradientFillStops;
  FGradientFillStops.Height := 20;

  FGradientLineStops := TdxSpreadSheetGradientStops.Create(lcMain);
  FGradientLineStops.OnSelectionChanged := GradientLineStopsSelectionChangedHandler;
  FGradientLineStops.OnDblClick := GradientLineStopsDblClickHandler;
  lciGradientLineStops.Control := GradientLineStops;
  FGradientLineStops.Height := 20;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.Initialize(AContainer: TdxSpreadSheetContainer);
begin
  FContainer := AContainer;
  SetControlLookAndFeel(Self, AContainer.SpreadSheet.DialogsLookAndFeel);
  lciRotation.Visible := TdxSpreadSheetContainerAccess(AContainer).IsTransformsSupported;
  lciRelativeToPictureSize.Visible := ContainerAsPicture <> nil;
  lciCrop.Visible := ContainerAsPicture <> nil;
  lcgCrop.Visible := ContainerAsPicture <> nil;
  lcgFill.Visible := ContainerAsShape <> nil;
  lcgLine.Visible := ContainerAsShape <> nil;
  lcgOriginalSize.Visible := ContainerAsPicture <> nil;
  lciOriginalSize.Visible := ContainerAsPicture <> nil;
  lcgText.Visible := ContainersTextBox <> nil;
  lcgTextBox.Visible := ContainersTextBox <> nil;
  lciLinePenStyle.Visible := ContainerAsComment = nil;

  UpdateColorButtonGlyphs;
  ApplyLocalization;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.Load;
begin
  LoadSizeParams;
  if ContainerAsShape <> nil then
    LoadShape(ContainerAsShape.Shape);
  if ContainersTextBox <> nil then
    LoadTextBox(ContainersTextBox);
  UpdateOriginalSizeInfo;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.Save;
begin
  Container.SpreadSheet.History.BeginAction(TdxSpreadSheetHistoryChangeContainerAction);
  try
    Container.SpreadSheet.History.AddCommand(TdxSpreadSheetHistoryChangeContainerCommand.Create(Container, True));
    SaveSizeParams;
    if ContainerAsShape <> nil then
      SaveShape(ContainerAsShape.Shape);
    if ContainersTextBox <> nil then
      SaveTextBox(ContainersTextBox);
  finally
    Container.SpreadSheet.History.EndAction;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.ApplyLocalization;
const
  SizeAndRotateGroupMap: array[Boolean] of Pointer = (
    @sdxContainerCustomizationDialogSize,
    @sdxContainerCustomizationDialogSizeAndRotate
  );
begin
  Caption := cxGetResourceString(@sdxContainerCustomizationDialogCaption);
  btnOK.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonCancel);

  // Fill
  acTextureFillLoad.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonLoad);
  acTextureFillSave.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonSave);
  btnGradientFillAddStop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonAdd);
  btnGradientFillColor.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonColor);
  btnGradientFillRemoveStop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonRemove);
  btnSolidFillColor.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonColor);
  lcgFill.Caption := cxGetResourceString(@sdxContainerCustomizationDialogGroupFill);
  lciGradientFillDirection.Caption := cxGetResourceString(@sdxContainerCustomizationDialogDirection);
  lciGradientFillStops.Caption := cxGetResourceString(@sdxContainerCustomizationDialogStops);
  rbGradientFill.Caption := cxGetResourceString(@sdxContainerCustomizationDialogGradientFill);
  rbNoFill.Caption := cxGetResourceString(@sdxContainerCustomizationDialogNoFill);
  rbSolidFill.Caption := cxGetResourceString(@sdxContainerCustomizationDialogSolidFill);
  rbTextureFill.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextureFill);
  PopulateGradientDirection(ccbGradientFillDirection);

  // Line
  lcgLine.Caption := cxGetResourceString(@sdxContainerCustomizationDialogLine);
  lciLineStyle.Caption := cxGetResourceString(@sdxContainerCustomizationDialogLineStyle);
  lciLineWidth.Caption := cxGetResourceString(@sdxContainerCustomizationDialogLineWidth);
  rbGradientLine.Caption := cxGetResourceString(@sdxContainerCustomizationDialogGradientLine);
  rbNoLine.Caption := cxGetResourceString(@sdxContainerCustomizationDialogNoLine);
  rbSolidLine.Caption := cxGetResourceString(@sdxContainerCustomizationDialogSolidLine);
  btnGradientLineAddStop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonAdd);
  btnGradientLineColor.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonColor);
  btnGradientLineRemoveStop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonRemove);
  btnSolidLineColor.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonColor);
  lciGradientLineDirection.Caption := cxGetResourceString(@sdxContainerCustomizationDialogDirection);
  lciGradientLineStops.Caption := cxGetResourceString(@sdxContainerCustomizationDialogStops);
  PopulateGradientDirection(ccbGradientLineDirection);
  PopulatePenStyle(ccbLineStyle);

  // Properties
  lcgProperties.Caption := cxGetResourceString(@sdxContainerCustomizationDialogGroupProperties);
  lbPosition.Caption := cxGetResourceString(@sdxContainerCustomizationDialogPositioning);
  rbAbsolute.Caption := cxGetResourceString(@sdxContainerCustomizationDialogAbsolute);
  rbOneCell.Caption := cxGetResourceString(@sdxContainerCustomizationDialogOneCells);
  rbTwoCells.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTwoCells);

  // Size
  cbLockAspectRatio.Caption := cxGetResourceString(@sdxContainerCustomizationDialogLockAspectRatio);
  cbRelativeToPictureSize.Caption := cxGetResourceString(@sdxContainerCustomizationDialogRelativeToPictureSize);
  lbCrop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogCropFrom);
  lbScale.Caption := cxGetResourceString(@sdxContainerCustomizationDialogScale);
  lbSizeAndRotate.Caption := cxGetResourceString(SizeAndRotateGroupMap[lciRotation.Visible]);
  lcgSize.Caption := cxGetResourceString(@sdxContainerCustomizationDialogGroupSize);
  lciCropBottom.Caption := cxGetResourceString(@sdxContainerCustomizationDialogCropBottom);
  lciCropTop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogCropTop);
  lciCropLeft.Caption := cxGetResourceString(@sdxContainerCustomizationDialogCropLeft);
  lciCropRight.Caption := cxGetResourceString(@sdxContainerCustomizationDialogCropRight);
  lciHeight.Caption := cxGetResourceString(@sdxContainerCustomizationDialogHeight);
  lciRotation.Caption := cxGetResourceString(@sdxContainerCustomizationDialogRotation);
  lciScaleHeight.Caption := cxGetResourceString(@sdxContainerCustomizationDialogScaleHeight);
  lciScaleWidth.Caption := cxGetResourceString(@sdxContainerCustomizationDialogScaleWidth);
  lciWidth.Caption := cxGetResourceString(@sdxContainerCustomizationDialogWidth);
  lbOriginalSize.Caption := cxGetResourceString(@sdxContainerCustomizationDialogOriginalSize);
  btnReset.Caption := cxGetResourceString(@sdxContainerCustomizationDialogReset);
  UpdateOriginalSizeInfo;

  // Text
  lcgText.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextCaption);
  btnTextFont.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextButtonFont);

  // TextBox
  lcgTextBox.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxCaption);
  lbTextAlignment.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxAlignment);
  lbTextPadding.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxPadding);
  lciTextBoxHorzAlign.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxHorizontal);
  lciTextBoxVertAlign.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxVertical);
  lciTextPaddingBottom.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxPaddingBottom);
  lciTextPaddingLeft.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxPaddingLeft);
  lciTextPaddingRight.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxPaddingRight);
  lciTextPaddingTop.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxPaddingTop);
  cbTextBoxAutoSize.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxAutoSize);
  cbTextBoxWordWrap.Caption := cxGetResourceString(@sdxContainerCustomizationDialogTextBoxWordWrap);
  PopulateHorzAlignment(cbTextBoxHorzAlign);
  PopulateVertAlignment(cbTextBoxVertAlign);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.GradientFillStopsDblClickHandler(Sender: TObject);
begin
  if btnGradientFillColor.Enabled then
    btnGradientFillColor.Click;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.GradientFillStopsSelectionChangedHandler(Sender: TObject);
begin
  btnGradientFillColor.Tag := GradientFillStops.SelectedStopColor;
  btnGradientFillColor.Enabled := GradientFillStops.SelectedStopIndex >= 0;
  btnGradientFillRemoveStop.Enabled := GradientFillStops.SelectedStopIndex >= 0;
  UpdateColorButtonGlyph(btnGradientFillColor);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.GradientLineStopsDblClickHandler(Sender: TObject);
begin
  if btnGradientLineColor.Enabled then
    btnGradientLineColor.Click;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.GradientLineStopsSelectionChangedHandler(Sender: TObject);
begin
  btnGradientLineColor.Tag := GradientLineStops.SelectedStopColor;
  btnGradientLineColor.Enabled := GradientLineStops.SelectedStopIndex >= 0;
  btnGradientLineRemoveStop.Enabled := GradientLineStops.SelectedStopIndex >= 0;
  UpdateColorButtonGlyph(btnGradientLineColor);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.LoadShape(AShape: TdxSpreadSheetShape);
begin
  LoadShapeBrush(AShape.Brush);
  LoadShapePen(AShape.Pen);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.LoadShapeBrush(ABrush: TdxGPBrush);
begin
  rbGradientFill.Checked := ABrush.Style = gpbsGradient;
  rbNoFill.Checked := ABrush.Style = gpbsClear;
  rbSolidFill.Checked := ABrush.Style = gpbsSolid;
  rbTextureFill.Checked := ABrush.Style = gpbsTexture;

  if rbSolidFill.Checked then
  begin
    btnSolidFillColor.Tag := ABrush.Color;
    UpdateColorButtonGlyph(btnSolidFillColor);
  end
  else

  if rbTextureFill.Checked then
    imTextureFill.Picture.Graphic := ABrush.Texture
  else

  if rbGradientFill.Checked then
  begin
    ccbGradientFillDirection.ItemObject := TObject(ABrush.GradientMode);
    GradientFillStops.Brush.GradientPoints.Assign(ABrush.GradientPoints);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.LoadShapePen(APen: TdxGPPen);
begin
  ceLineWidth.Value := APen.Width;
  ccbLineStyle.ItemObject := TObject(APen.Style);

  rbSolidLine.Checked := True;
  rbNoLine.Checked := rbNoLine.Visible and (APen.Brush.Style = gpbsClear);
  rbGradientLine.Checked := rbGradientLine.Visible and (APen.Brush.Style = gpbsGradient);

  if rbSolidLine.Checked then
  begin
    btnSolidLineColor.Tag := APen.Brush.Color;
    UpdateColorButtonGlyph(btnSolidLineColor);
  end
  else

  if rbGradientLine.Checked then
  begin
    ccbGradientLineDirection.ItemObject := TObject(APen.Brush.GradientMode);
    GradientLineStops.Brush.GradientPoints.Assign(APen.Brush.GradientPoints);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.LoadSizeParams;
var
  ARect: TRect;
begin
  case Container.AnchorType of
    catAbsolute:
      rbAbsolute.Checked := True;
    catOneCell:
      if Container.AnchorPoint1.FixedToCell then
        rbOneCell.Checked := True
      else
        rbAbsolute.Checked := True;
    catTwoCell:
      if not Container.AnchorPoint1.FixedToCell then
        rbAbsolute.Checked := True
      else
        if Container.AnchorPoint2.FixedToCell then
          rbTwoCells.Checked := True
        else
          rbOneCell.Checked := True;
  end;

  seRotation.Value := Container.Transform.RotationAngle;

  ARect := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;
  seHeight.Value := ARect.Height;
  seWidth.Value := ARect.Width;
  seHeight.Tag := ARect.Height;
  seWidth.Tag := ARect.Width;

  if ContainerAsPicture <> nil then
  begin
    CropMargins := ContainerAsPicture.Picture.CropMargins;
    seHeight.Tag := seHeight.Tag + cxMarginsHeight(CropMargins);
    seWidth.Tag := seWidth.Tag + cxMarginsWidth(CropMargins);
  end;

  cbLockAspectRatio.Checked := TdxSpreadSheetContainerAccess(Container).KeepAspectUsingCornerHandles;
  seScaleHeight.Value := 100;
  seScaleWidth.Value := 100;
  if ContainerAsPicture <> nil then
    cbRelativeToPictureSize.Checked := ContainerAsPicture.RelativeResize;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.LoadTextBox(ATextBox: TdxSpreadSheetCustomTextBox);
begin
  meText.Text := ATextBox.TextAsString;
  ATextBox.Font.AssignToFont(fdTextBoxFont.Font);
  cbTextBoxHorzAlign.ItemIndex := cbTextBoxHorzAlign.Properties.Items.IndexOfObject(TObject(ATextBox.AlignHorz));
  cbTextBoxVertAlign.ItemIndex := cbTextBoxVertAlign.Properties.Items.IndexOfObject(TObject(ATextBox.AlignVert));
  seTextPaddingBottom.Value := ATextBox.ContentOffsets.Bottom;
  seTextPaddingLeft.Value := ATextBox.ContentOffsets.Left;
  seTextPaddingRight.Value := ATextBox.ContentOffsets.Right;
  seTextPaddingTop.Value := ATextBox.ContentOffsets.Top;
  cbTextBoxAutoSize.Checked := ATextBox.AutoSize;

  lciTextBoxWordWrap.Visible := ATextBox is TdxSpreadSheetTextBox;
  if ATextBox is TdxSpreadSheetTextBox then
    cbTextBoxWordWrap.Checked := TdxSpreadSheetTextBox(ATextBox).WordWrap;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SaveShape(AShape: TdxSpreadSheetShape);
begin
  SaveShapeBrush(AShape.Brush);
  SaveShapePen(AShape.Pen);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SaveShapeBrush(ABrush: TdxGPBrush);
begin
  if rbSolidFill.Checked then
  begin
    ABrush.Color := btnSolidFillColor.Tag;
    ABrush.Style := gpbsSolid;
  end
  else

  if rbTextureFill.Checked and (imTextureFill.Picture.Graphic <> nil) then
  begin
    ABrush.Texture.Assign(imTextureFill.Picture.Graphic);
    ABrush.Style := gpbsTexture;
  end
  else

  if rbGradientFill.Checked and (GradientFillStops.Brush.GradientPoints.Count > 0) then
  begin
    ABrush.Style := gpbsGradient;
    ABrush.GradientPoints.Assign(GradientFillStops.Brush.GradientPoints);
    ABrush.GradientMode := TdxGPBrushGradientMode(ccbGradientFillDirection.ItemObject);
  end
  else
    ABrush.Style := gpbsClear
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SaveShapePen(APen: TdxGPPen);
begin
  APen.Width := ceLineWidth.Value;
  APen.Style := TdxGPPenStyle(ccbLineStyle.ItemObject);

  if rbSolidLine.Checked then
  begin
    APen.Brush.Color := btnSolidLineColor.Tag;
    APen.Brush.Style := gpbsSolid;
  end
  else

  if rbGradientLine.Checked and (GradientLineStops.Brush.GradientPoints.Count > 0) then
  begin
    APen.Brush.Style := gpbsGradient;
    APen.Brush.GradientPoints.Assign(GradientLineStops.Brush.GradientPoints);
    APen.Brush.GradientMode := TdxGPBrushGradientMode(ccbGradientLineDirection.ItemObject);
  end
  else
    APen.Brush.Style := gpbsClear;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SaveSizeParams;
var
  ARect: TRect;
begin
  ARect := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;

  Container.AnchorPoint1.FixedToCell := rbOneCell.Checked or rbTwoCells.Checked;
  Container.AnchorPoint2.FixedToCell := rbTwoCells.Checked;
  if Container.AnchorPoint1.FixedToCell then
    Container.AnchorType := catTwoCell;

  if ContainerAsPicture <> nil then
  begin
    ContainerAsPicture.Picture.CropMargins := CropMargins;
    ContainerAsPicture.RelativeResize := cbRelativeToPictureSize.Checked;
  end;

  if (ARect.Width <> seWidth.Value) or (ARect.Height <> seHeight.Value) then
  begin
    ARect.Height := seHeight.Value;
    ARect.Width := seWidth.Value;
  end;
  TdxSpreadSheetContainerAccess(Container).Calculator.UpdateAnchors(ARect);
  Container.Transform.RotationAngle := seRotation.Value;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SaveTextBox(ATextBox: TdxSpreadSheetCustomTextBox);
begin
  ATextBox.Font.Assign(fdTextBoxFont.Font);
  ATextBox.TextAsString := meText.Text;
  ATextBox.AlignHorz := TAlignment(cbTextBoxHorzAlign.ItemObject);
  ATextBox.AlignVert := TVerticalAlignment(cbTextBoxVertAlign.ItemObject);
  ATextBox.ContentOffsets := Rect(seTextPaddingLeft.Value, seTextPaddingTop.Value, seTextPaddingRight.Value, seTextPaddingBottom.Value);
  ATextBox.AutoSize := cbTextBoxAutoSize.Checked;

  if ATextBox is TdxSpreadSheetTextBox then
    TdxSpreadSheetTextBox(ATextBox).WordWrap := cbTextBoxWordWrap.Checked;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.PopulateGradientDirection(ACombobox: TcxComboBox);
var
  AMode: TdxGPBrushGradientMode;
  ASavedIndex: Integer;
begin
  ACombobox.Properties.Items.BeginUpdate;
  try
    ASavedIndex := ACombobox.ItemIndex;
    ACombobox.Properties.Items.Clear;
    for AMode := Low(AMode) to High(AMode) do
      ACombobox.Properties.Items.AddObject(cxGetResourceString(dxGradientModeNames[AMode]), TObject(AMode));
    ACombobox.ItemIndex := Max(ASavedIndex, 0);
  finally
    ACombobox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.PopulateHorzAlignment(ACombobox: TcxComboBox);
const
  Map: array[TAlignment] of Pointer = (
    @sdxContainerCustomizationDialogTextBoxAlignmentLeft,
    @sdxContainerCustomizationDialogTextBoxAlignmentRight,
    @sdxContainerCustomizationDialogTextBoxAlignmentCenter
  );
var
  AIndex: TAlignment;
begin
  ACombobox.Properties.Items.BeginUpdate;
  try
    ACombobox.Properties.Items.Clear;
    for AIndex := Low(TAlignment) to High(TAlignment) do
      ACombobox.Properties.Items.AddObject(cxGetResourceString(Map[AIndex]), TObject(AIndex));
    ACombobox.ItemIndex := 0;
  finally
    ACombobox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.PopulatePenStyle(ACombobox: TcxComboBox);
var
  AStyle: TdxGPPenStyle;
  ASavedIndex: Integer;
begin
  ACombobox.Properties.Items.BeginUpdate;
  try
    ASavedIndex := ACombobox.ItemIndex;
    ACombobox.Properties.Items.Clear;
    for AStyle := Low(AStyle) to High(AStyle) do
      ACombobox.Properties.Items.AddObject(cxGetResourceString(dxPenStyleNames[AStyle]), TObject(AStyle));
    ACombobox.ItemIndex := Max(ASavedIndex, 0);
  finally
    ACombobox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.PopulateVertAlignment(ACombobox: TcxComboBox);
const
  Map: array[TVerticalAlignment] of Pointer = (
    @sdxContainerCustomizationDialogTextBoxAlignmentTop,
    @sdxContainerCustomizationDialogTextBoxAlignmentBottom,
    @sdxContainerCustomizationDialogTextBoxAlignmentCenter
  );
var
  AIndex: TVerticalAlignment;
begin
  ACombobox.Properties.Items.BeginUpdate;
  try
    ACombobox.Properties.Items.Clear;
    for AIndex := Low(TVerticalAlignment) to High(TVerticalAlignment) do
      ACombobox.Properties.Items.AddObject(cxGetResourceString(Map[AIndex]), TObject(AIndex));
    ACombobox.ItemIndex := 0;
  finally
    ACombobox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SelectColor(AButton: TcxButton);
begin
  ColorDialog.Color := AButton.Tag;
  if ColorDialog.Execute(Handle) then
  begin
    AButton.Tag := ColorDialog.Color;
    UpdateColorButtonGlyph(AButton);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  UpdateColorButtonGlyphs;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SelectGradientStopColor(
  AStops: TdxSpreadSheetGradientStops; AButton: TcxButton);
begin
  ColorDialog.Color := AButton.Tag;
  if ColorDialog.Execute(Handle) then
  begin
    AStops.SelectedStopColor := ColorDialog.Color;
    AButton.Tag := ColorDialog.Color;
    UpdateColorButtonGlyph(AButton);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SelectPage(AActivePage: Integer);
begin
  if AActivePage < 0 then
  begin
    if ContainersTextBox <> nil then
      AActivePage := lcgText.Index
    else
      AActivePage := 0;
  end;
  lcgTabs.ItemIndex := AActivePage;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SetSpinValue(ASpinEdit: TcxSpinEdit; const AValue: Variant);
begin
  ASpinEdit.Value := TcxSpinEditPropertiesAccess(ASpinEdit.Properties).CheckValueBounds(AValue);
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetContainerAsComment: TdxSpreadSheetCommentContainer;
begin
  if Container is TdxSpreadSheetCommentContainer then
    Result := TdxSpreadSheetCommentContainer(Container)
  else
    Result := nil;
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetContainerAsPicture: TdxSpreadSheetPictureContainer;
begin
  if Container is TdxSpreadSheetPictureContainer then
    Result := TdxSpreadSheetPictureContainer(Container)
  else
    Result := nil;
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetContainerAsShape: TdxSpreadSheetShapeContainer;
begin
  if Container is TdxSpreadSheetShapeContainer then
    Result := TdxSpreadSheetShapeContainer(Container)
  else
    Result := nil;
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetContainerAsTextBox: TdxSpreadSheetTextBoxContainer;
begin
  if Container is TdxSpreadSheetTextBoxContainer then
    Result := TdxSpreadSheetTextBoxContainer(Container)
  else
    Result := nil;
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetContainersTextBox: TdxSpreadSheetCustomTextBox;
begin
  if ContainerAsComment <> nil then
    Result := ContainerAsComment.TextBox
  else
    if ContainerAsTextBox <> nil then
      Result := ContainerAsTextBox.TextBox
    else
      Result := nil;
end;

function TdxSpreadSheetContainerCustomizationDialogForm.GetCropMargins: TRect;
begin
  Result := cxRect(seCropLeft.Value, seCropTop.Value, seCropRight.Value, seCropBottom.Value);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.SetCropMargins(const Value: TRect);
begin
  seCropBottom.Value := Value.Bottom;
  seCropLeft.Value := Value.Left;
  seCropRight.Value := Value.Right;
  seCropTop.Value := Value.Top;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seCropHorzPropertiesChange(Sender: TObject);
begin
  if (Sender as TcxSpinEdit).Focused then
    SetSpinValue(seWidth, seWidth.Value + cxMarginsWidth(FPrevCropMargins) - cxMarginsWidth(CropMargins));
  FPrevCropMargins := CropMargins;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seCropVertPropertiesChange(Sender: TObject);
begin
  if (Sender as TcxSpinEdit).Focused then
    SetSpinValue(seHeight, seHeight.Value + cxMarginsHeight(FPrevCropMargins) - cxMarginsHeight(CropMargins));
  FPrevCropMargins := CropMargins;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seHeightPropertiesChange(Sender: TObject);
var
  AMarginHeight: Integer;
begin
  if FLockChangeSize then
    Exit;
  AMarginHeight := cxMarginsHeight(CropMargins);
  if cbLockAspectRatio.Checked then
    SetSpinValue(seWidth, MulDiv(seHeight.Value + AMarginHeight, FScaleNumerator, FScaleDenominator) - cxMarginsWidth(CropMargins));
  if not seScaleHeight.Focused then
    SetSpinValue(seScaleHeight, MulDiv(100, seHeight.Value + AMarginHeight, seHeight.Tag));
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seWidthPropertiesChange(Sender: TObject);
var
  AMarginWidth: Integer;
begin
  if FLockChangeSize then
    Exit;
  AMarginWidth := cxMarginsWidth(CropMargins);
  if cbLockAspectRatio.Checked then
    SetSpinValue(seHeight, MulDiv(seWidth.Value + AMarginWidth, FScaleDenominator, FScaleNumerator) - cxMarginsHeight(CropMargins));
  if not seScaleWidth.Focused then
    SetSpinValue(seScaleWidth, MulDiv(100, seWidth.Value + AMarginWidth, seWidth.Tag));
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seScaleHeightPropertiesChange(Sender: TObject);
begin
  if seScaleHeight.Focused then
    SetSpinValue(seHeight, MulDiv(seHeight.Tag, seScaleHeight.Value, 100) - cxMarginsHeight(CropMargins));
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.seScaleWidthPropertiesChange(Sender: TObject);
begin
  if seScaleWidth.Focused then
    SetSpinValue(seWidth, MulDiv(seWidth.Tag, seScaleWidth.Value, 100) - cxMarginsWidth(CropMargins));
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.UpdateColorButtonGlyph(AButton: TcxButton);
begin
  UpdateColorButtonGlyph(AButton, AButton.Tag);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.UpdateColorButtonGlyph(AButton: TcxButton; AColor: TdxAlphaColor);
var
  ABitmap32: TcxBitmap32;
begin
  ABitmap32 := TcxBitmap32.CreateSize(ScaleFactor.Apply(18), ScaleFactor.Apply(18), False);
  try
    cxDrawTransparencyCheckerboard(ABitmap32.cxCanvas, ABitmap32.ClientRect, ScaleFactor.Apply(4));
    dxGPPaintCanvas.BeginPaint(ABitmap32.Canvas.Handle, ABitmap32.ClientRect);
    try
      dxGPPaintCanvas.Rectangle(ABitmap32.ClientRect, dxColorToAlphaColor(clBlack), AColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
    ABitmap32.MakeOpaque;
    AButton.Glyph.Assign(ABitmap32);
    AButton.Glyph.SourceDPI := ScaleFactor.TargetDPI;
  finally
    ABitmap32.Free;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.UpdateColorButtonGlyphs;
begin
  UpdateColorButtonGlyph(btnGradientLineColor);
  UpdateColorButtonGlyph(btnGradientFillColor);
  UpdateColorButtonGlyph(btnSolidFillColor);
  UpdateColorButtonGlyph(btnSolidLineColor);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.UpdateOriginalSizeInfo;
var
  ASize: TSize;
begin
  if ContainerAsPicture <> nil then
  begin
    if ContainerAsPicture.Picture.Image <> nil then
      ASize := ContainerAsPicture.Picture.Image.Size
    else
      ASize := cxNullSize;

    lclOriginalSize.Caption := Format(cxGetResourceString(
      @sdxContainerCustomizationDialogOriginalSizeFormatString), [ASize.cy, ASize.cx]);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.acTextureFillLoadExecute(Sender: TObject);
var
  ASmartImage: TdxGPImage;
begin
  TextureOpenDialog.Filter := cxGraphicFilter(TdxGPImage);
  if TextureOpenDialog.Execute(Handle) then
  begin
    ASmartImage := TdxGPImage.Create;
    try
      ASmartImage.LoadFromFile(TextureOpenDialog.FileName);
      imTextureFill.Picture.Graphic := ASmartImage;
    finally
      ASmartImage.Free;
    end;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.acTextureFillSaveExecute(Sender: TObject);
var
  AImageExt: string;
begin
  AImageExt := cxGraphicExtension(imTextureFill.Picture.Graphic);
  TextureSaveDialog.DefaultExt := AImageExt;
  TextureSaveDialog.Filter := UpperCase(Copy(AImageExt, 2, MaxInt)) + ' Image|*' + AImageExt + ';';
  if TextureSaveDialog.Execute(Handle) then
    imTextureFill.Picture.Graphic.SaveToFile(ChangeFileExt(TextureSaveDialog.FileName, AImageExt));
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.acTextureFillSaveUpdate(Sender: TObject);
begin
  acTextureFillSave.Enabled := rbTextureFill.Checked and (imTextureFill.Picture.Graphic <> nil);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientFillAddStopClick(Sender: TObject);
begin
  GradientFillStops.Add;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientFillColorClick(Sender: TObject);
begin
  SelectGradientStopColor(GradientFillStops, btnGradientFillColor);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientFillRemoveStopClick(Sender: TObject);
begin
  GradientFillStops.DeleteSelected;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientLineAddStopClick(Sender: TObject);
begin
  GradientLineStops.Add;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientLineColorClick(Sender: TObject);
begin
  SelectGradientStopColor(GradientLineStops, btnGradientLineColor);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnGradientLineRemoveStopClick(Sender: TObject);
begin
  GradientLineStops.DeleteSelected;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnResetClick(Sender: TObject);
begin
  CropMargins := cxNullRect;
  FLockChangeSize := True;
  try
    if ContainerAsPicture.Picture.Image <> nil then
    begin
      seHeight.Tag := ContainerAsPicture.Picture.Image.Height;
      seWidth.Tag := ContainerAsPicture.Picture.Image.Width;
    end;
    seHeight.Value := seHeight.Tag;
    seWidth.Value := seWidth.Tag;
    seScaleHeight.Value := 100;
    seScaleWidth.Value := 100;
    seRotation.Value := 0;
  finally
    FLockChangeSize := False;
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnTextFontClick(Sender: TObject);
begin
  fdTextBoxFont.Execute(Handle);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.btnColorClick(Sender: TObject);
begin
  SelectColor(Sender as TcxButton);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.cbLockAspectRatioClick(Sender: TObject);
begin
  if cbRelativeToPictureSize.Checked then
  begin
    if ContainerAsPicture.Picture.Image <> nil then
    begin
      FScaleNumerator := ContainerAsPicture.Picture.Image.Width;
      FScaleDenominator := ContainerAsPicture.Picture.Image.Height;
    end
    else
    begin
      FScaleDenominator := 1;
      FScaleNumerator := 1;
    end;
  end
  else
  begin
    FScaleNumerator := seWidth.Value;
    FScaleDenominator := seHeight.Value;
  end;
  if cbLockAspectRatio.Checked then
    Container.Restrictions := Container.Restrictions + [crNoChangeAspectUsingCornerHandles]
  else
    Container.Restrictions := Container.Restrictions - [crNoChangeAspectUsingCornerHandles];
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.cbRelativeToPictureSizeClick(Sender: TObject);
var
  ARect: TRect;
begin
  if cbRelativeToPictureSize.Checked then
  begin
    if ContainerAsPicture.Picture.Image <> nil then
      ARect := ContainerAsPicture.Picture.Image.ClientRect
    else
      ARect := cxNullRect;
  end
  else
  begin
    ARect := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;
    if ContainerAsPicture <> nil then
      ARect := cxRectInflate(ARect, ContainerAsPicture.Picture.CropMargins);
  end;

  seHeight.Tag := ARect.Height;
  seWidth.Tag := ARect.Width;
  cbLockAspectRatioClick(nil);
  seHeightPropertiesChange(nil);
  seWidthPropertiesChange(nil);
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.FormShow(Sender: TObject);
begin
  if (lcgTabs.ItemIndex = lcgText.Index) and meText.CanFocusEx then
  begin
    meText.SetFocus;
    meText.SelStart := Length(meText.Text);
  end;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.rbGradientLineClick(Sender: TObject);
begin
  lciLineWidth.Visible := not rbNoLine.Checked;
  lciLineStyle.Visible := not rbNoLine.Checked;
  lcgSolidLine.Visible := rbSolidLine.Checked;
  lcgGradientLine.Visible := rbGradientLine.Checked;
end;

procedure TdxSpreadSheetContainerCustomizationDialogForm.rbTextureFillClick(Sender: TObject);
begin
  lcgGradientFill.Visible := rbGradientFill.Checked;
  lcgSolidFill.Visible := rbSolidFill.Checked;
  lcgTextureFill.Visible := rbTextureFill.Checked;
end;

end.
