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

unit dxSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  dxForms, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxRadioGroup,
  cxButtons,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetCoreDialogsStrs;

type

  { TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog }

  TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbbBarDirection: TcxComboBox;
    cbbFillMode: TcxComboBox;
    ccbPositiveBarBorderColor: TcxColorComboBox;
    cbbBorderStyle: TcxComboBox;
    ccbAxisColor: TcxColorComboBox;
    ccbNegativeBarBorderColor: TcxColorComboBox;
    ccbNegativeBarColor: TcxColorComboBox;
    ccbPositiveBarColor: TcxColorComboBox;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lcgAxis: TdxLayoutGroup;
    lcgCommon: TdxLayoutGroup;
    lcgNegativeBar: TdxLayoutGroup;
    lcgPositiveBar: TdxLayoutGroup;
    lciAxisColor: TdxLayoutItem;
    lciBarDirection: TdxLayoutItem;
    lciFillMode: TdxLayoutItem;
    lciPositiveBarBorderColor: TdxLayoutItem;
    lciPositiveBarBorderStyle: TdxLayoutItem;
    lciPositiveBarColor: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutAutoCreatedGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainItem10: TdxLayoutItem;
    lcMainItem11: TdxLayoutItem;
    lcMainItem12: TdxLayoutItem;
    lcMainItem13: TdxLayoutItem;
    lcMainItem14: TdxLayoutItem;
    lcMainItem16: TdxLayoutItem;
    lcMainItem17: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainItem8: TdxLayoutItem;
    lcMainItem9: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    rbAxisAuto: TcxRadioButton;
    rbAxisMidpoint: TcxRadioButton;
    rbAxisNone: TcxRadioButton;
    rbNegativeBarBorderColor: TcxRadioButton;
    rbNegativeBarBorderColorAuto: TcxRadioButton;
    rbNegativeBarColor: TcxRadioButton;
    rbNegativeBarColorAuto: TcxRadioButton;

    procedure cbbPositiveBarBorderStylePropertiesChange(Sender: TObject);
    procedure rbAxisAutoClick(Sender: TObject);
    procedure rbNegativeBarBorderColorClick(Sender: TObject);
    procedure rbNegativeBarColorClick(Sender: TObject);
  protected
    procedure ApplyLocalizations;
    procedure Initialize(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; ALookAndFeel: TcxLookAndFeel);
    procedure LoadStyle(AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
    procedure SaveStyle(AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
    procedure UpdateControlsState;
  end;

function ShowConditionalFormattingDataBarRuleStyleEditDialog(
  AOwner: IdxDialogOwner; AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle): Boolean; overload;
function ShowConditionalFormattingDataBarRuleStyleEditDialog(
  AOwner: TObject; AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle): Boolean; overload; // Special for CBuilder

implementation

uses
  dxCore, dxCoreClasses, Math, dxSpreadSheetTypes;

{$R *.dfm}

function GetBarDirectionName(ADirection: TdxSpreadSheetConditionalFormattingRuleDataBarDirection): string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case ADirection of
    dbdAuto:
      AMessagePtr := @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionAuto;
    dbdLeftToRight:
      AMessagePtr := @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionLeftToRight;
    dbdRightToLeft:
      AMessagePtr := @sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirectionRightToLeft;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

function GetFillModeName(AFillMode: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode): string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case AFillMode of
    dbfmSolid:
      AMessagePtr := @sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeSolid;
    dbfmGradient:
      AMessagePtr := @sdxConditionalFormattingDataBarRuleStyleEditDialogFillModeGradient;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

function ShowConditionalFormattingDataBarRuleStyleEditDialog(
  AOwner: IdxDialogOwner; AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle): Boolean;
var
  ADialog: TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog;
  AIntf: IdxSpreadSheetConditionalFormatting;
begin
  Result := False;
  if Supports(AOwner, IdxSpreadSheetConditionalFormatting, AIntf) then
  begin
    ADialog := TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.Create(AOwner.GetParentForm);
    try
      ADialog.Initialize(AIntf.GetConditionalFormatting, AOwner.GetLookAndFeel);
      ADialog.LoadStyle(AStyle);
      Result := ADialog.ShowModal = mrOk;
      if Result then
        ADialog.SaveStyle(AStyle);
    finally
      ADialog.Free;
    end;
  end;
end;

function ShowConditionalFormattingDataBarRuleStyleEditDialog(
  AOwner: TObject; AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle): Boolean;
var
  AOwnerIntf: IdxDialogOwner;
begin
  Result := Supports(AOwner, IdxDialogOwner, AOwnerIntf);
  Assert(Result);
  Result := Result and ShowConditionalFormattingDataBarRuleStyleEditDialog(AOwnerIntf, AStyle);
end;

{ TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog }

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.ApplyLocalizations;
var
  ADirection: TdxSpreadSheetConditionalFormattingRuleDataBarDirection;
  AFillMode: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode;
begin
  Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogCaption);

  lcgCommon.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogCommon);
  lciFillMode.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogFillMode);
  lciBarDirection.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogBarDirection);

  lcgPositiveBar.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBar);
  lciPositiveBarColor.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarColor);
  lciPositiveBarBorderColor.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderColor);
  lciPositiveBarBorderStyle.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogPositiveBarBorderStyle);

  lcgNegativeBar.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBar);
  rbNegativeBarColor.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColor);
  rbNegativeBarColorAuto.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarColorAuto);
  rbNegativeBarBorderColor.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColor);
  rbNegativeBarBorderColorAuto.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogNegativeBarBorderColorAuto);

  lcgAxis.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogAxis);
  rbAxisAuto.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogAxisAuto);
  rbAxisMidpoint.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogAxisMidpoint);
  rbAxisNone.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogAxisNone);
  lciAxisColor.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogAxisColor);

  btnCancel.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogButtonOk);

  cbbFillMode.Properties.Items.Clear;
  for AFillMode := Low(AFillMode) to High(AFillMode) do
    cbbFillMode.Properties.Items.AddObject(GetFillModeName(AFillMode), TObject(AFillMode));

  cbbBarDirection.Properties.Items.Clear;
  for ADirection := Low(ADirection) to High(ADirection) do
    cbbBarDirection.Properties.Items.AddObject(GetBarDirectionName(ADirection), TObject(ADirection));

  cbbBorderStyle.Properties.Items.Clear;
  cbbBorderStyle.Properties.Items.Add(cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogBordersNone));
  cbbBorderStyle.Properties.Items.Add(cxGetResourceString(@sdxConditionalFormattingDataBarRuleStyleEditDialogBordersSolid));
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.Initialize(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting; ALookAndFeel: TcxLookAndFeel);
begin
  SetControlLookAndFeel(Self, ALookAndFeel);
  ApplyLocalizations;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.LoadStyle(
  AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
begin
  cbbFillMode.ItemIndex := Max(0, cbbFillMode.Properties.Items.IndexOfObject(TObject(AStyle.FillMode)));
  cbbBarDirection.ItemIndex := Max(0, cbbBarDirection.Properties.Items.IndexOfObject(TObject(AStyle.Direction)));

  // PositiveBar
  cbbBorderStyle.ItemIndex := Ord(AStyle.PositiveBarBorderColor <> clNone);
  ccbPositiveBarBorderColor.ColorValue := AStyle.PositiveBarBorderColor;
  ccbPositiveBarColor.ColorValue := AStyle.PositiveBarColor;

  // NegativeBar
  rbNegativeBarColor.Checked := AStyle.NegativeBarColor <> clDefault;
  rbNegativeBarColorAuto.Checked := AStyle.NegativeBarColor = clDefault;
  rbNegativeBarBorderColor.Checked := AStyle.NegativeBarBorderColor <> clDefault;
  rbNegativeBarBorderColorAuto.Checked := AStyle.NegativeBarBorderColor = clDefault;
  ccbNegativeBarColor.ColorValue := AStyle.ActualNegativeBarColor;
  ccbNegativeBarBorderColor.ColorValue := AStyle.ActualNegativeBarBorderColor;

  // Axis
  rbAxisAuto.Checked := AStyle.AxisPosition = dbapAuto;
  rbAxisMidpoint.Checked := AStyle.AxisPosition = dbapMidpoint;
  rbAxisNone.Checked := AStyle.AxisPosition = dbapNone;
  ccbAxisColor.ColorValue := AStyle.AxisColor;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.SaveStyle(
  AStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
begin
  AStyle.FillMode := TdxSpreadSheetConditionalFormattingRuleDataBarFillMode(cbbFillMode.ItemObject);
  AStyle.Direction := TdxSpreadSheetConditionalFormattingRuleDataBarDirection(cbbBarDirection.ItemObject);

  // PositiveBar
  AStyle.PositiveBarColor := ccbPositiveBarColor.ColorValue;
  if cbbBorderStyle.ItemIndex = 0 then
    AStyle.PositiveBarBorderColor := clNone
  else
    AStyle.PositiveBarBorderColor := ccbPositiveBarBorderColor.ColorValue;

  // NegativeBar
  if rbNegativeBarColor.Checked then
    AStyle.NegativeBarColor := ccbNegativeBarColor.ColorValue
  else
    AStyle.NegativeBarColor := clDefault;

  if rbNegativeBarBorderColor.Checked then
    AStyle.NegativeBarBorderColor := ccbNegativeBarBorderColor.ColorValue
  else
    AStyle.NegativeBarBorderColor := clDefault;

  // Axis
  AStyle.AxisColor := ccbAxisColor.ColorValue;
  if rbAxisAuto.Checked then
    AStyle.AxisPosition := dbapAuto
  else
    if rbAxisMidpoint.Checked then
      AStyle.AxisPosition := dbapMidpoint
    else
      AStyle.AxisPosition := dbapNone;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.UpdateControlsState;
begin
  ccbPositiveBarBorderColor.Enabled := (cbbBorderStyle.ItemIndex > 0);
  ccbNegativeBarBorderColor.Enabled := (cbbBorderStyle.ItemIndex > 0) and rbNegativeBarBorderColor.Checked;
  rbNegativeBarBorderColorAuto.Enabled := cbbBorderStyle.ItemIndex > 0;
  rbNegativeBarBorderColor.Enabled := cbbBorderStyle.ItemIndex > 0;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.cbbPositiveBarBorderStylePropertiesChange(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.rbAxisAutoClick(Sender: TObject);
begin
  ccbAxisColor.Enabled := not rbAxisNone.Checked;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.rbNegativeBarBorderColorClick(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog.rbNegativeBarColorClick(Sender: TObject);
begin
  ccbNegativeBarColor.Enabled := rbNegativeBarColor.Checked;
end;

end.
