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

unit dxPSfmTtl;

interface

{$I cxVer.inc}

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  ImgList, Menus, IniFiles, dxCore, dxPSForm, cxControls, cxContainer, cxEdit, cxLabel, cxCheckBox, cxButtons, cxPC,
  cxTextEdit, cxMemo, cxGraphics, cxMaskEdit, cxDropDownEdit, cxColorComboBox, cxLookAndFeelPainters, cxGeometry,
  dxPSCore, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  cxClasses, dxLayoutControl, cxImageList;

type

  { TdxfmReportTitleProperties }

  TdxfmReportTitleProperties = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnFont: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    btnRestoreDefaults: TcxButton;
    cbxColor: TcxColorComboBox;
    cbxMode: TcxComboBox;
    cbxTextAlignX: TcxComboBox;
    cbxTextAlignY: TcxComboBox;
    chbxAdjustOnScale: TcxCheckBox;
    chbxTransparent: TcxCheckBox;
    edFont: TcxTextEdit;
    memText: TcxMemo;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    lblMode: TdxLayoutItem;
    pctlMain: TdxLayoutGroup;
    tshText: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    tshProperties: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    lblColor: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    lblTextAlignX: TdxLayoutItem;
    lblTextAlignY: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    lbbtnHelp: TdxLayoutItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutItem4: TdxLayoutItem;
    lblTransparent: TcxLabel;
    dxLayoutItem14: TdxLayoutItem;
    lblAlignment: TcxLabel;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    ilAlignments: TcxImageList;
    procedure btnFontClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure cbxTextAlignYPropertiesDrawItem(AControl: TcxCustomComboBox;
      ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblTransparentClick(Sender: TObject);
    procedure memTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TitleChanged(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    FActivePage: Integer;
    FModified: Boolean;
    FReportTitle: TdxReportTitle;
    procedure SetReportTitle(Value: TdxReportTitle);

    procedure InitializeControls;
    procedure LoadStrings;
    procedure SaveUserInput;
    procedure SetModified(Value: Boolean);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;

    property Modified: Boolean read FModified write SetModified;
  protected
    procedure BeforeConstruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    //
    property ReportTitle: TdxReportTitle read FReportTitle write SetReportTitle;
  end;

function dxShowReportTitlePropertiesDlg(AReportTitle: TdxReportTitle): Boolean;
implementation

{$R *.DFM}

uses
  Registry, dxPrnDev, dxPSRes, dxPSUtl, dxPSGlbl, dxExtCtrls, cxDrawTextUtils;

const
  sdxActivePage = 'ActivePage'; //Don't Localize

function dxShowReportTitlePropertiesDlg(AReportTitle: TdxReportTitle): Boolean;
var
  Dialog: TdxfmReportTitleProperties;
begin
  Result := False;
  if AReportTitle = nil then Exit;

  Dialog := TdxfmReportTitleProperties.Create(nil);
  try
    Dialog.ReportTitle := AReportTitle;
    Result := Dialog.Execute;
    if Result then
      AReportTitle.Assign(Dialog.ReportTitle);
  finally
    Dialog.Free;
  end;
end;

{ TdxfmReportTitleProperties }

constructor TdxfmReportTitleProperties.Create(AOwner: TComponent);
begin
  HelpContext := dxhcTitlePropertiesDlg;
  inherited Create(AOwner);
  CheckDialogFormHelpContext(Self, lbbtnHelp);
  FReportTitle := TdxReportTitle.Create(nil);
  LoadStrings;
end;

destructor TdxfmReportTitleProperties.Destroy;
begin
  FreeAndNil(FReportTitle);
  inherited Destroy;
end;

function TdxfmReportTitleProperties.Execute: Boolean;
begin
  InitializeControls;
  pctlMain.ItemIndex := FActivePage;
  Modified := False;
  Result := (ShowModal = mrOK) and Modified;
end;

procedure TdxfmReportTitleProperties.BeforeConstruction;
begin
  inherited BeforeConstruction;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmReportTitleProperties.FormClose(
  Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    SaveUserInput;
end;

procedure TdxfmReportTitleProperties.InitializeControls;
begin
  with ReportTitle do
  begin
    chbxAdjustOnScale.Checked := AdjustOnReportScale;
    cbxColor.ColorValue := Color;
    FontInfoToText(Font, edFont);
    cbxMode.ItemIndex := Integer(Mode);
    memText.Text := Text;
    cbxTextAlignX.ItemIndex := Integer(TextAlignX);
    cbxTextAlignY.ItemIndex := Integer(TextAlignY);
    chbxTransparent.Checked := Transparent;
    cbxColor.Enabled := not chbxTransparent.Checked;
  end;
end;

procedure TdxfmReportTitleProperties.SetModified(Value: Boolean);
begin
  FModified := Value;
  cbxColor.Enabled := not chbxTransparent.Checked;
end;

procedure TdxfmReportTitleProperties.TitleChanged(Sender: TObject);
begin
  cbxColor.Enabled := not chbxTransparent.Checked;
  Modified := True;
end;

procedure TdxfmReportTitleProperties.lblTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxfmReportTitleProperties.SetReportTitle(Value: TdxReportTitle);
begin
  FReportTitle.Assign(Value);
end;

procedure TdxfmReportTitleProperties.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxReportTitleDlgCaption);
  btnOK.Caption := cxGetResourceString(@sdxBtnOkAccelerated);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);

  lblMode.Caption := cxGetResourceString(@sdxMode);
  cbxMode.Clear;
  cbxMode.Properties.Items.Add(cxGetResourceString(@sdxTitleModeNone));
  cbxMode.Properties.Items.Add(cxGetResourceString(@sdxTitleModeOnFirstPage));
  cbxMode.Properties.Items.Add(cxGetResourceString(@sdxTitleModeOnEveryTopPage));

  tshText.Caption := cxGetResourceString(@sdxText);
  tshProperties.Caption := cxGetResourceString(@sdxProperties);
  lblColor.Caption := cxGetResourceString(@sdxColor);
  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  chbxAdjustOnScale.Caption := cxGetResourceString(@sdxAdjustOnScale);

  lblAlignment.Caption := DropAmpersand(cxGetResourceString(@sdxAlignment));

  lblTextAlignX.Caption := cxGetResourceString(@sdxTextAlignHorz) + ':';
  cbxTextAlignX.Clear;
  cbxTextAlignX.Properties.Items.Add(cxGetResourceString(@sdxTextAlignLeft));
  cbxTextAlignX.Properties.Items.Add(cxGetResourceString(@sdxTextAlignCenter));
  cbxTextAlignX.Properties.Items.Add(cxGetResourceString(@sdxTextAlignRight));

  lblTextAlignY.Caption := cxGetResourceString(@sdxTextAlignVert) + ':';
  cbxTextAlignY.Clear;
  cbxTextAlignY.Properties.Items.Add(cxGetResourceString(@sdxTextAlignTop));
  cbxTextAlignY.Properties.Items.Add(cxGetResourceString(@sdxTextAlignVCenter));
  cbxTextAlignY.Properties.Items.Add(cxGetResourceString(@sdxTextAlignBottom));

  lblTransparent.Caption := cxGetResourceString(@sdxTransparent);
end;

procedure TdxfmReportTitleProperties.SaveUserInput;
begin
  with FReportTitle do
  begin
    AdjustOnReportScale := chbxAdjustOnScale.Checked;
    Color := cbxColor.ColorValue;
    Mode := TdxReportTitleMode(cbxMode.ItemIndex);
    Text := memText.Text;
    TextAlignX := TcxTextAlignX(cbxTextAlignX.ItemIndex);
    TextAlignY := TcxTextAlignY(cbxTextAlignY.ItemIndex);
    Transparent := chbxTransparent.Checked;
  end;
end;

procedure TdxfmReportTitleProperties.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pctlMain.Count - 1 do
    if IsAccel(Message.CharCode, pctlMain.Items[I].Caption) then
    begin
      Message.Result := 1;
      pctlMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TdxfmReportTitleProperties.btnFontClick(Sender: TObject);
begin
  FontDialog.Font := ReportTitle.Font;
  if FontDialog.Execute then
  begin
    ReportTitle.Font := FontDialog.Font;
    FontInfoToText(ReportTitle.Font, edFont);
    TitleChanged(nil);
  end;
end;

procedure TdxfmReportTitleProperties.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TdxfmReportTitleProperties.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  FActivePage := AIniFile.ReadInteger(ASectionName, sdxActivePage, FActivePage);
end;

procedure TdxfmReportTitleProperties.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  AIniFile.WriteInteger(ASectionName, sdxActivePage, pctlMain.ItemIndex);
end;

procedure TdxfmReportTitleProperties.btnRestoreDefaultsClick(Sender: TObject);
begin
  ReportTitle.RestoreDefaults;
  InitializeControls;
  TitleChanged(nil);
end;

procedure TdxfmReportTitleProperties.cbxTextAlignYPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var
  AImageRect: TRect;
  ATextRect: TRect;
begin
  ACanvas.FillRect(ARect);

  AImageRect := cxRectSetWidth(ARect, cxRectHeight(ARect));
  AImageRect := cxRectCenter(AImageRect, dxGetImageSize(ilAlignments, ScaleFactor));
  cxDrawImage(ACanvas, AImageRect, nil, ilAlignments, 3 * TTagToInt(AControl.Tag) + AIndex, True, nil, ScaleFactor);

  ATextRect := ARect;
  ATextRect.Left := AImageRect.Right + ScaleFactor.Apply(cxTextOffset);
  ACanvas.DrawTexT(AControl.Properties.Items[AIndex], ATextRect, cxAlignVCenter);
end;

procedure TdxfmReportTitleProperties.memTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and cxShiftStateMoveOnly(Shift) then
    ModalResult := mrCancel;
end;

end.
