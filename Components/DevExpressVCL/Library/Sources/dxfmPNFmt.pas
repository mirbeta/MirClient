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

unit dxfmPNFmt;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  dxPSForm, dxPSGlbl, dxCore, Menus, cxLookAndFeelPainters, cxGraphics,
  cxSpinEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxButtons,
  cxRadioGroup, cxControls, cxContainer, cxEdit, cxLabel, dxExtCtrls,
  cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutLookAndFeels, cxClasses,
  dxLayoutControl;

type
  TdxfmPageNumberFormat = class(TCustomdxPSForm)
    btnOK: TcxButton;
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    cbxPageNumberingFormat: TcxComboBox;
    btnDefault: TcxButton;
    rbtnContinueFromPrevSection: TcxRadioButton;
    rbtnStartAt: TcxRadioButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLaoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutItem4: TdxLayoutItem;
    lblPageNumbering: TcxLabel;
    bvlStartAtHolder: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    lgDefault: TdxLayoutGroup;
    lblPageNumberFormat: TcxLabel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    procedure cbxPageNumberingFormatChange(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure lblPageNumberFormatClick(Sender: TObject);
    procedure rbtnStartAtClick(Sender: TObject);
    procedure rbtnContinueFromPrevSectionClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    FseStartAt: TdxPSValueEdit;
    FAllowContinueFromPrevSection: Boolean;
    FControlsUpdating: Boolean;
    FContinueFromPrevSection: Boolean;
    FModified: Boolean;
    FPageNumberFormats: TStrings;
    FPageNumberFormat: TdxPageNumberFormat;
    FStartPageIndex: Integer;
    FSetPageNumberingFormatAsDefault: Boolean;
    procedure CheckModified;
    procedure CreateControls;
    procedure Initialize;
    procedure LoadStrings;
    procedure SetContinueFromPrevSection(Value: Boolean);
    procedure SetPageNumberFormats(Value: TStrings);
    procedure StartAtChanged(Sender: TObject);
    procedure StartatExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property AllowContinueFromPrevSection: Boolean read FAllowContinueFromPrevSection write FAllowContinueFromPrevSection;
    property ContinueFromPrevSection: Boolean read FContinueFromPrevSection write SetContinueFromPrevSection;
    property PageNumberFormats: TStrings read FPageNumberFormats write SetPageNumberFormats;
    property PageNumberFormat: TdxPageNumberFormat read FPageNumberFormat write FPageNumberFormat;
    property SetPageNumberingFormatAsDefault: Boolean read FSetPageNumberingFormatAsDefault write FSetPageNumberingFormatAsDefault;
    property StartPageIndex: Integer read FStartPageIndex write FStartPageIndex;
  end;

  PdxPageNumberFormatDlgData = ^TdxPageNumberFormatDlgData;
  TdxPageNumberFormatDlgData = record
    PageNumberFormats: TStrings;
    PageNumberFormat: TdxPageNumberFormat;
    ContinueFromPrevSection: Boolean;
    StartPageIndex: Integer;
    AllowContinueFromPrevSection: Boolean;
    ShowAsDefaultButton: Boolean;
    SetPageNumberFormatAsDefault: Boolean;
    HelpContext: THelpContext;
  end;

function dxShowPageNumberFormatDlg(var AFormatsDlgData: TdxPageNumberFormatDlgData): Boolean;

implementation

{$R *.DFM}

uses
  Registry, dxPSRes, dxPSUtl;

function dxShowPageNumberFormatDlg(var AFormatsDlgData: TdxPageNumberFormatDlgData): Boolean;
var
  Dialog: TdxfmPageNumberFormat;
begin
  Result := False;
  if AFormatsDlgData.PageNumberFormats = nil then Exit;

  Dialog := TdxfmPageNumberFormat.Create(nil);
  try
    Dialog.AllowContinueFromPrevSection := AFormatsDlgData.AllowContinueFromPrevSection;
    Dialog.FContinueFromPrevSection := AFormatsDlgData.ContinueFromPrevSection;
    Dialog.PageNumberFormats := AFormatsDlgData.PageNumberFormats;
    Dialog.FPageNumberFormat := AFormatsDlgData.PageNumberFormat;
    Dialog.FStartPageIndex := AFormatsDlgData.StartPageIndex;
    Dialog.lgDefault.Visible := AFormatsDlgData.ShowAsDefaultButton;
    if AFormatsDlgData.HelpContext <> 0 then
      Dialog.HelpContext := AFormatsDlgData.HelpContext;
    Result := Dialog.Execute;
    if Result then
    begin
      AFormatsDlgData.PageNumberFormat := Dialog.PageNumberFormat;
      AFormatsDlgData.ContinueFromPrevSection := Dialog.ContinueFromPrevSection;
      AFormatsDlgData.StartPageIndex := Dialog.StartPageIndex;
      AFormatsDlgData.SetPageNumberFormatAsDefault := Dialog.SetPageNumberingFormatAsDefault;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TdxfmPageNumberFormat }

constructor TdxfmPageNumberFormat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HelpContext := dxhcPageNumberFormatDlg;
  CheckDialogFormHelpContext(Self, libtnHelp);

  FPageNumberFormats := TStringList.Create;
  CreateControls;
  LoadStrings;
end;

destructor TdxfmPageNumberFormat.Destroy;
begin
  FreeAndNil(FPageNumberFormats);
  inherited;
end;

function TdxfmPageNumberFormat.Execute: Boolean;
begin
  Initialize;
  FModified := False;
  Result := (ShowModal = mrOK) and FModified;
end;

procedure TdxfmPageNumberFormat.CheckModified;
begin
  FModified := True;
end;

procedure TdxfmPageNumberFormat.CreateControls;
begin
  FseStartAt := TdxPSValueEdit.Create(Self);
  bvlStartAtHolder.Control := FseStartAt;
  FseStartAt.Properties.MinValue := 1;
  FseStartAt.Properties.MaxValue := 10000;
  FseStartAt.Properties.OnChange := StartAtChanged;
  FseStartAt.OnExit := StartAtExit;
  FseStartAt.Value := 1;
end;

procedure TdxfmPageNumberFormat.Initialize;
begin
  FControlsUpdating := True;
  try
    with cbxPageNumberingFormat.Properties do
    begin
      Items.BeginUpdate;
      try
        Items.Clear;
        Items := FPageNumberFormats;
        cbxPageNumberingFormat.ItemIndex := Integer(PageNumberFormat);
      finally
        Items.EndUpdate;
      end;
    end;
    rbtnContinueFromPrevSection.Enabled := AllowContinueFromPrevSection;
    //rbtnStartAt.Enabled := AllowContinueFromPrevSection;
    rbtnContinueFromPrevSection.Checked := ContinueFromPrevSection;
    rbtnStartAt.Checked := not ContinueFromPrevSection;

    FseStartAt.ValueType := TdxPSValueType(PageNumberFormat);
    if AllowContinueFromPrevSection and ContinueFromPrevSection then
      FseStartAt.Text := ''
    else
      FseStartAt.Value := StartPageIndex;
  finally
    FControlsUpdating := False;
  end;
end;

procedure TdxfmPageNumberFormat.SetContinueFromPrevSection(Value: Boolean);
begin
  if FContinueFromPrevSection <> Value then
  begin
    FContinueFromPrevSection := Value;
    FControlsUpdating := True;
    try
      rbtnContinueFromPrevSection.Checked := FContinueFromPrevSection;
      rbtnStartAt.Checked := not FContinueFromPrevSection;
      if FContinueFromPrevSection then
        FseStartAt.Text := ''
      else
        FseStartAt.Value := 1;
    finally
      FControlsUpdating := False;
    end;
  end;
end;

procedure TdxfmPageNumberFormat.SetPageNumberFormats(Value: TStrings);
begin
  FPageNumberFormats.Assign(Value);
end;

procedure TdxfmPageNumberFormat.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxPNFormatsCaption);
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnDefault.Caption := cxGetResourceString(@sdxBtnDefault);
  lblPageNumberFormat.Caption := cxGetResourceString(@sdxPNFormatsNumberFormat);
  lblPageNumbering.Caption := cxGetResourceString(@sdxPageNumbering);
  rbtnContinueFromPrevSection.Caption := cxGetResourceString(@sdxPNFormatsContinueFromPrevious);
  rbtnStartAt.Caption := cxGetResourceString(@sdxPNFormatsStartAt);
end;

procedure TdxfmPageNumberFormat.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TdxfmPageNumberFormat.cbxPageNumberingFormatChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FControlsUpdating := True;
  try
    FseStartAt.ValueType := TdxPSValueType(TcxComboBox(Sender).ItemIndex);
    FPageNumberFormat := TdxPageNumberFormat(TcxComboBox(Sender).ItemIndex);
  finally
    FControlsUpdating := False;
  end;
  CheckModified;
end;

procedure TdxfmPageNumberFormat.StartAtChanged(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  rbtnStartAt.Checked := True;
  CheckModified;
end;

procedure TdxfmPageNumberFormat.StartAtExit(Sender: TObject);
begin
  FStartPageIndex := TdxPSValueEdit(Sender).Value;
end;

procedure TdxfmPageNumberFormat.btnDefaultClick(Sender: TObject);
var
  S : string;
begin
  S := Format(cxGetResourceString(@sdxPNFormatsChangeDefaultFormat),
    [cbxPageNumberingFormat.Properties.Items[cbxPageNumberingFormat.ItemIndex]]);
  if MessageQuestion(S) then
  begin
    FSetPageNumberingFormatAsDefault := True;
    CheckModified;
  end;
end;

procedure TdxfmPageNumberFormat.lblPageNumberFormatClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

procedure TdxfmPageNumberFormat.rbtnStartAtClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  ContinueFromPrevSection := False;
  ActiveControl := FseStartAt;
  CheckModified;
end;

procedure TdxfmPageNumberFormat.rbtnContinueFromPrevSectionClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  ContinueFromPrevSection := True;
  CheckModified;
end;

end.
