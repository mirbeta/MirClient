{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxHintEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  Messages, StdCtrls, SysUtils, cxButtons, cxCheckBox, cxCheckListBox,
  cxColorComboBox, cxContainer, cxControls, cxDropDownEdit, cxEdit,
  cxExtEditConsts, cxFontNameComboBox, cxGroupBox, cxHint, cxLabel, cxListBox,
  cxListView, cxLookAndFeelPainters, cxLookandFeels, cxMaskEdit, cxMemo,
  cxRadioGroup, cxSpinButton, cxSpinEdit, cxSplitter, cxTextEdit, cxTrackBar,
  cxGraphics, Menus, cxClasses, dxCustomHint;

type
  TcxHintStyleEditor = class(TForm)
    cxBtnOk: TcxButton;
    cxBtnCancel: TcxButton;
    cxCbStandard: TcxCheckBox;
    cxGbHintFont: TcxGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cxFnHint: TcxFontNameComboBox;
    cxClbHintFontStyles: TcxCheckListBox;
    cxLbHfSize: TcxListBox;
    cxCcbHintFontColour: TcxColorComboBox;
    cxgbHintCapFont: TcxGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cxFnHinCap: TcxFontNameComboBox;
    cxClbHintCapFontStyles: TcxCheckListBox;
    cxLbHcfSize: TcxListBox;
    cxCcbHintCapColor: TcxColorComboBox;
    cxGbHintShape: TcxGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    cxCbHintCalloutPos: TcxComboBox;
    cxcbHintRounded: TcxCheckBox;
    cxSeHintRadius: TcxSpinEdit;
    cxGbHintIcons: TcxGroupBox;
    Label11: TLabel;
    cxCbHintIconType: TcxComboBox;
    cxRbHIDef: TcxRadioButton;
    cxRbHILarge: TcxRadioButton;
    cxRbHISmall: TcxRadioButton;
    cxGbHintPause: TcxGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    cxSeHintHidePause: TcxSpinEdit;
    cxSeHintPause: TcxSpinEdit;
    cxSeShortHintPause: TcxSpinEdit;
    lblHintColour: TLabel;
    cxCcbHintColour: TcxColorComboBox;
    cxGbHintAnimation: TcxGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    cxCbHintAniStyle: TcxComboBox;
    cxSeHintDelay: TcxSpinEdit;
    cxGbPreview: TcxGroupBox;
    cxHsc: TcxHintStyleController;
    pnlPreview: TPanel;
    cxEditStyleController1: TcxEditStyleController;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cxFnHintClick(Sender: TObject);
    procedure cxCbHintCalloutPosClick(Sender: TObject);
    procedure cxSeHintRadiusPropertiesChange(Sender: TObject);
    procedure cxcbHintRoundedPropertiesChange(Sender: TObject);
    procedure cxCbHintAniStylePropertiesChange(Sender: TObject);
    procedure cxSeHintDelayPropertiesChange(Sender: TObject);
    procedure cxCbHintIconTypeClick(Sender: TObject);
    procedure cxRbHIDefClick(Sender: TObject);
    procedure cxSeHintHidePausePropertiesChange(Sender: TObject);
    procedure cxSeHintPausePropertiesChange(Sender: TObject);
    procedure cxSeShortHintPausePropertiesEditValueChanged(Sender: TObject);
    procedure cxClbHintFontStylesClickCheck(Sender: TObject;
      AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure cxCcbHintFontColourClick(Sender: TObject);
    procedure cxCcbHintCapColorClick(Sender: TObject);
    procedure cxFnHintPropertiesChange(Sender: TObject);
    procedure cxLbHfSizeClick(Sender: TObject);
    procedure cxLbHcfSizeClick(Sender: TObject);
    procedure cxFnHinCapPropertiesChange(Sender: TObject);
    procedure cxCbStandardClick(Sender: TObject);
    procedure cxFnHinCapClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cxCcbHintColourPropertiesEditValueChanged(Sender: TObject);
  private
    fViewerFontSizes: TStringList;
    fHintHorz: Integer;
    fHintVert: Integer;
    fFormLoaded: Boolean;
    procedure SetHsc(const Value: TcxHintStyleController);
    procedure RefreshHint;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    function GetHintStyle: TcxHintStyle;
  public
    procedure SetHintController(aDest: TcxHintStyleController);
    procedure LoadHintCtrls;
    property HintController: TCxHintStyleController read cxHsc write SetHsc;
    property HintStyle: TcxHintStyle read GetHintStyle;
  end;

var
  cxHintStyleEditor: TcxHintStyleEditor;

procedure ShowHintStyleEditor(AHintStyleController: TcxHintStyleController);

implementation

{$R *.dfm}

resourcestring
  SHintCaption = 'Example';
  SHintText    = 'Preview Hint';

procedure ShowHintStyleEditor(AHintStyleController: TcxHintStyleController);
var
  AHintStyleEditor: TcxHintStyleEditor;
  AOwnerCaption: string;
  APrevApplicationOnShowHint: TShowHintEvent;
begin
  APrevApplicationOnShowHint := Application.OnShowHint;
  AHintStyleEditor := TcxHintStyleEditor.Create(Application);
  with AHintStyleEditor do
  try
    HintController := AHintStyleController;
    if AHintStyleController.Owner <> nil then
      AOwnerCaption := AHintStyleController.Owner.Name + '.'
    else
      AOwnerCaption := '';
    Caption := Format('%s%s - Hint Editor',[AOwnerCaption, AHintStyleController.Name]);

    if ShowModal = mrOK then
    begin
      AHintStyleController.Assign(cxHsc);
      SetDesignerModified(AHintStyleController);
    end;

    HintController.HideHint;
  finally
    Application.OnShowHint := APrevApplicationOnShowHint;
    Free;
  end;
end;

procedure TcxHintStyleEditor.FormCreate(Sender: TObject);
begin
  fViewerFontSizes := TStringList.Create;
  SetControlLookAndFeel(Self, cxEditStyleController1.Style.LookAndFeel);
end;

procedure TcxHintStyleEditor.FormDestroy(Sender: TObject);
begin
  cxHsc.HideHint;
  fViewerFontSizes.Free;
end;

procedure TcxHintStyleEditor.cxCbStandardClick(Sender: TObject);
begin
  HintStyle.Standard := cxCbStandard.Checked;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxFnHintClick(Sender: TObject);
begin
  HintStyle.Font.Name := cxFnHint.FontName;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxFnHinCapClick(Sender: TObject);
begin
  HintStyle.CaptionFont.Name := cxFnHinCap.FontName;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCbHintCalloutPosClick(Sender: TObject);
begin
  HintStyle.CallOutPosition := TcxCallOutPosition(cxCbHintCalloutPos.ItemIndex);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxSeHintRadiusPropertiesChange(Sender: TObject);
begin
  HintStyle.RoundRadius := cxSeHintRadius.Value;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxcbHintRoundedPropertiesChange(Sender: TObject);
begin
  HintStyle.Rounded := cxcbHintRounded.Checked;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCbHintAniStylePropertiesChange(Sender: TObject);
begin
  HintStyle.Animate := TcxHintAnimate(cxCbHintAniStyle.ItemIndex);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxSeHintDelayPropertiesChange(Sender: TObject);
begin
  HintStyle.AnimationDelay := Integer(cxSeHintDelay.Value);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCbHintIconTypeClick(Sender: TObject);
begin
  HintStyle.IconType := TcxHintIconType(cxCbHintIconType.ItemIndex);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxRbHIDefClick(Sender: TObject);
begin
  HintStyle.IconSize := TcxHintIconSize(Integer(TcxRadioButton(Sender).Tag));
  RefreshHint;
end;

procedure TcxHintStyleEditor.SetHsc(const Value: TCxHintStyleController);
begin
  cxHsc.Assign(Value);
end;

procedure TcxHintStyleEditor.SetHintController(aDest: TCxHintStyleController);
begin
  aDest.HintStyle.Assign(cxHsc.HintStyle);
  aDest.HintShortPause := cxHsc.HintShortPause;
  aDest.HintPause := cxHsc.HintPause;
  aDest.HintHidePause := cxHsc.HintHidePause;
end;

procedure TcxHintStyleEditor.LoadHintCtrls;

  function StyleToEditValue(aFontStyle : TFontStyles) : Integer;
  begin
    Result := 0;
    if fsBold in aFontStyle then
      Result := 1;
    if fsItalic in aFontStyle then
      Inc(Result,2);
    if fsUnderline in aFontStyle then
      Inc(Result,4);
    if fsStrikeOut in aFontStyle then
      Inc(Result,8);
  end;

begin
  cxCbStandard.Checked := HintStyle.Standard;
  cxFnHint.FontName := HintStyle.Font.Name;
  while not cxFnHint.Properties.LoadFontComplete do
    Application.ProcessMessages;
  cxCcbHintFontColour.ColorValue := HintStyle.Font.Color;
  cxClbHintFontStyles.EditValue := StyleToEditValue(HintStyle.Font.Style);
  cxFnHinCap.FontName := HintStyle.CaptionFont.Name;
  while not cxFnHinCap.Properties.LoadFontComplete do
    Application.ProcessMessages;
  cxCcbHintCapColor.ColorValue := HintStyle.CaptionFont.Color;
  cxClbHintCapFontStyles.EditValue := StyleToEditValue(HintStyle.CaptionFont.Style);
  cxCcbHintColour.ColorValue := HintStyle.Color;
  cxCbHintRounded.Checked := HintStyle.Rounded;
  cxCbHintCalloutPos.ItemIndex := Ord(HintStyle.CallOutPosition);
  cxSeHintRadius.Value := HintStyle.RoundRadius;
  cxCbHintAniStyle.ItemIndex := Ord(HintStyle.Animate);
  cxSeHintDelay.Value := Variant(HintStyle.AnimationDelay);
  cxCbHintIconType.ItemIndex := Ord(HintStyle.IconType);
  cxSeHintHidePause.Value := cxHsc.HintHidePause;
  cxSeHintPause.Value := cxHsc.HintPause;
  cxSeShortHintPause.Value := cxHsc.HintShortPause;
  case Ord(HintStyle.IconSize) of
    0: cxRbHiDef.Checked := True;
    1: cxRbHiLarge.Checked := True;
    2: cxRbHiSmall.Checked := True;
  end;
  cxLbhfSize.ItemIndex := cxLbhFSize.Items.IndexOf(IntToStr(HintStyle.Font.Size));
  cxLbhcfSize.ItemIndex := cxLbhcFSize.Items.IndexOf(IntToStr(HintStyle.CaptionFont.Size));
End;

procedure TcxHintStyleEditor.cxSeHintHidePausePropertiesChange(
  Sender: TObject);
begin
  cxHsc.HintHidePause := cxSeHintHidePause.Value;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxSeHintPausePropertiesChange(
  Sender: TObject);
begin
  cxHsc.HintPause := cxSeHintPause.Value;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxSeShortHintPausePropertiesEditValueChanged(
  Sender: TObject);
begin
  cxHsc.HintShortPause := cxSeShortHintPause.Value;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCcbHintFontColourClick(Sender: TObject);
begin
  HintStyle.Font.Color := cxCcbHintFontColour.ColorValue;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCcbHintCapColorClick(Sender: TObject);
begin
  HintStyle.CaptionFont.Color := cxCcbHintCapColor.ColorValue;
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxLbHfSizeClick(Sender: TObject);
begin
  with cxLbHfSize do
    HintStyle.Font.Size := StrToInt(Items[ItemIndex]);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxLbHcfSizeClick(Sender: TObject);
begin
  with cxLbHcfSize do
    HintStyle.CaptionFont.Size := StrToInt(Items[ItemIndex]);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxFnHinCapPropertiesChange(
  Sender: TObject);
begin
  GetFontSizes(cxFnHinCap.FontName,cxLbHcfSize.Items);
end;

procedure TcxHintStyleEditor.cxFnHintPropertiesChange(Sender: TObject);
begin
  GetFontSizes(cxFnHint.FontName,cxLbHfSize.Items);
end;

procedure TcxHintStyleEditor.RefreshHint;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    cxHsc.HideHint;
    cxHsc.ShowHint(fHintHorz, fHintVert, SHintCaption, SHintText);
  end;
end;

procedure TcxHintStyleEditor.WMActivate(var Message: TWMActivate);
begin
  if Message.Active = WA_INACTIVE then
    cxHsc.HideHint
  else
    RefreshHint;
end;

procedure TcxHintStyleEditor.WMMove(var Message: TWMMove);
begin
  inherited;
  if fFormLoaded then
  begin
    fHintHorz := pnlPreview.ClientOrigin.X + (pnlPreview.Width div 2 - cxHsc.GetHintWidth(SHintText) div 2);
    fHintVert := pnlPreview.ClientOrigin.Y + (pnlPreview.Height div 2 - cxHsc.GetHintHeight(SHintText) div 2);
    RefreshHint;
  end;
end;

procedure TcxHintStyleEditor.FormShow(Sender: TObject);
begin
  fHintHorz := pnlPreview.ClientOrigin.X + (pnlPreview.Width div 2 - cxHsc.GetHintWidth(SHintText) div 2);
  fHintVert := pnlPreview.ClientOrigin.Y + (pnlPreview.Height div 2 - cxHsc.GetHintHeight(SHintText) div 2);

  fFormLoaded := True;
  LoadHintCtrls;
end;

function TcxHintStyleEditor.GetHintStyle: TcxHintStyle;
begin
  Result := cxHsc.HintStyle as TcxHintStyle;
end;

procedure TcxHintStyleEditor.FormActivate(Sender: TObject);
begin
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxClbHintFontStylesClickCheck(Sender: TObject;
  AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);

  function ChangeFontStyles(AFontStyles: TFontStyles): TFontStyles;
  const
    AFontStylesByIndex: array [0..3] of TFontStyle =
      (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  begin
    Result := AFontStyles;
    case ANewState of
      cbsUnchecked:
        Exclude(Result, AFontStylesByIndex[AIndex]);
      cbsChecked:
        Include(Result, AFontStylesByIndex[AIndex]);
    end;
  end;

begin
  if TcxCheckListBox(Sender).Tag = 0 then
    HintStyle.Font.Style := ChangeFontStyles(HintStyle.Font.Style)
  else
    HintStyle.CaptionFont.Style :=
      ChangeFontStyles(HintStyle.CaptionFont.Style);
  RefreshHint;
end;

procedure TcxHintStyleEditor.cxCcbHintColourPropertiesEditValueChanged(
  Sender: TObject);
begin
  HintStyle.Color := cxCcbHintColour.ColorValue;
  RefreshHint;
end;

end.
