{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSPrVwOpt;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus,
  dxCore, dxPreVw, dxPSForm, dxPrnPg,
  cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxGroupBox,
  cxCheckBox, cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxColorComboBox, cxLabel, cxSpinEdit, cxButtons, cxPC, cxLookAndFeels, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, cxClasses, dxLayoutControl;

type
  PdxPreviewOptionsDlgData = ^TdxPreviewOptionsDlgData;
  TdxPreviewOptionsDlgData = record
    MarginColor: TColor;
    MeasurementUnits: TdxMeasurementUnits;
    ShowMarginsHintWhileDragging: Boolean;
    ShowMarginHints: Boolean;
    ShowMargins: Boolean;
    ZoomOnMouseRoll: Boolean;
    ZoomStep: Integer;
  end;

  TdxfmOptions = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnOk: TcxButton;
    cbxMarginColor: TcxColorComboBox;
    cbxMeasurementUnits: TcxComboBox;
    chbxShowMargins: TcxCheckBox;
    chbxShowMarginsHints: TcxCheckBox;
    chbxShowMarginsHintsWhileDragging: TcxCheckBox;
    chbxZoomOnRoll: TcxCheckBox;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup10: TdxLayoutGroup;
    tshGeneral: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    gbxShow: TdxLayoutGroup;
    gbxZoomOpt: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    lblZoomStep: TdxLayoutItem;
    lblMeasurementUnits: TdxLayoutItem;
    lblMarginsColor: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    libtnHelp: TdxLayoutItem;
    seZoomStep: TcxSpinEdit;

    procedure btnHelpClick(Sender: TObject);
    procedure FormChanged(Sender: TObject);
    procedure lblMarginsColorClick(Sender: TObject);
  private
    FControlsUpdating: Boolean;
    FData: TdxPreviewOptionsDlgData;
    FModified: Boolean;
    procedure CheckModified;
    procedure LoadStrings;
    procedure StartSettings;
    procedure UpdateControlsState;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  end;

function dxShowPSPreviewOptionsDlg(var AData: TdxPreviewOptionsDlgData): Boolean;

implementation

{$R *.DFM}

uses
  dxExtCtrls, dxPSGlbl, dxPSRes, dxPSUtl;

function dxShowPSPreviewOptionsDlg(var AData: TdxPreviewOptionsDlgData): Boolean;
var
  Dialog: TdxfmOptions;
begin
  Dialog := TdxfmOptions.Create(nil);
  try
    Dialog.FData := AData;
    Result := Dialog.Execute;
    if Result then
      AData := Dialog.FData;
  finally
    Dialog.Free;
  end;
end;

{ TdxfmOptions }

constructor TdxfmOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HelpContext := dxPSGlbl.dxhcPreviewPreferencesDlg;
  LoadStrings;
end;

procedure TdxfmOptions.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxPreferenceDlgCaption);
  gbxShow.Caption := cxGetResourceString(@sdxPreferenceDlgShow);
  tshGeneral.Caption := dxPSUtl.DropAmpersand(cxGetResourceString(@sdxPreferenceDlgTab1));
  lblMeasurementUnits.Caption := cxGetResourceString(@sdxPreferenceDlgMeasurementUnits);
  with cbxMeasurementUnits.Properties.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add(cxGetResourceString(@sdxUnitsDefaultName));
      Add(cxGetResourceString(@sdxUnitsInchesName));
      Add(cxGetResourceString(@sdxUnitsMillimetersName));
    finally
      EndUpdate;
    end;
  end;
  chbxShowMargins.Caption := cxGetResourceString(@sdxPreferenceDlgMargins);
  chbxShowMarginsHints.Caption := cxGetResourceString(@sdxPreferenceDlgMarginsHints);
  chbxShowMarginsHintsWhileDragging.Caption := cxGetResourceString(@sdxPreferenceDlgMargingWhileDragging);
  lblMarginsColor.Caption := cxGetResourceString(@sdxPreferenceDlgMarginsColor);

  gbxZoomOpt.Caption := cxGetResourceString(@sdxZoomParameters);
  chbxZoomOnRoll.Caption := cxGetResourceString(@sdxPreferenceDlgZoomScroll);
  lblZoomStep.Caption := cxGetResourceString(@sdxPreferenceDlgZoomStep);

  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
end;

procedure TdxfmOptions.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TdxfmOptions.CheckModified;
begin
  FModified := True;
  UpdateControlsState;
end;

procedure TdxfmOptions.UpdateControlsState;
begin
//  btnOk.Enabled := FModified;
  chbxZoomOnRoll.Enabled := dxPSUtl.IsIntelliMousePresent;
end;

procedure TdxfmOptions.StartSettings;
begin
  FModified := False;
  FControlsUpdating := True;
  try
    chbxShowMargins.Checked := FData.ShowMargins;
    chbxShowMarginsHints.Checked := FData.ShowMarginHints;
    chbxShowMarginsHintsWhileDragging.Checked := FData.ShowMarginsHintWhileDragging;
    chbxZoomOnRoll.Checked := FData.ZoomOnMouseRoll;

    cbxMeasurementUnits.ItemIndex := Integer(FData.MeasurementUnits);
    cbxMarginColor.ColorValue := FData.MarginColor;
    seZoomStep.Value := FData.ZoomStep;

    CheckDialogFormHelpContext(Self, libtnHelp);
  finally
    FControlsUpdating := False;
  end;
  UpdateControlsState;
end;

function TdxfmOptions.Execute: Boolean;
begin
  StartSettings;
  Result := (ShowModal = mrOk) and FModified;

  if Result then
  begin
    FData.MeasurementUnits := TdxMeasurementUnits(cbxMeasurementUnits.ItemIndex);
    FData.MarginColor := cbxMarginColor.ColorValue;
    FData.ZoomStep := seZoomStep.Value;

    FData.ShowMargins := chbxShowMargins.Checked;
    FData.ShowMarginHints := chbxShowMarginsHints.Checked;
    FData.ShowMarginsHintWhileDragging := chbxShowMarginsHintsWhileDragging.Checked;
    FData.ZoomOnMouseRoll := chbxZoomOnRoll.Checked;
  end;
end;

procedure TdxfmOptions.FormChanged(Sender: TObject);
begin
  if not FControlsUpdating then
    CheckModified;
end;

procedure TdxfmOptions.lblMarginsColorClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

end.
