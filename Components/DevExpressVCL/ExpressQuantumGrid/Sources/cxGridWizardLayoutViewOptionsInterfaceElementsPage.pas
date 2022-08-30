{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWizardLayoutViewOptionsInterfaceElementsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, StdCtrls, cxRadioGroup, cxLabel, cxCheckBox, ExtCtrls, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridLayoutView, cxGridCustomTableView, dxLayoutLookAndFeels,
  cxGridCustomLayoutView, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCheckComboBox, cxNavigator;

type
  { TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame }

  TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame = class(TcxGridWizardCustomPageFrame)
    chbNavigator: TcxCheckBox;
    chbRecordCaption: TcxCheckBox;
    chcbNavigatorButtons: TcxCheckComboBox;
    lciNavigator: TdxLayoutItem;
    lciNavigatorButtons: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lciRecordCaption: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    pnPreviewGrid: TPanel;
    procedure RefreshPreviewGrid(Sender: TObject);
    procedure chbNavigatorClick(Sender: TObject);
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    procedure PopulateNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
    procedure SaveNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame }

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.ApplyLocalization;
begin
  chbNavigator.Caption := cxGetResourceString(@scxgwUIElementsPageNavigator);
  chbNavigator.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorHint);

  lciNavigatorButtons.Caption := cxGetResourceString(@scxgwUIElementsPageNavigatorButtons);
  chcbNavigatorButtons.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorButtonsHint);

  chbRecordCaption.Caption := cxGetResourceString(@scxgwUIElementsPageRecordCaption);
  chbRecordCaption.Hint := cxGetResourceString(@scxgwUIElementsPageRecordCaptionHint);
end;

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.ApplySettings;
var
  AView: TcxGridLayoutView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridLayoutView(Helper.GridView);
  AView.Navigator.Visible := chbNavigator.Checked;
  SaveNavigatorButtons(AView.Navigator.Buttons);
  AView.OptionsView.RecordCaption.Visible := chbRecordCaption.Checked;
end;

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.LoadSettings;
var
  AView: TcxGridLayoutView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridLayoutView(Helper.GridView);
  chbNavigator.Checked := AView.Navigator.Visible;
  chcbNavigatorButtons.Enabled := chbNavigator.Checked;
  PopulateNavigatorButtons(AView.Navigator.Buttons);
  chbRecordCaption.Checked := AView.OptionsView.RecordCaption.Visible;
end;

function TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageDescription);
end;

function TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageTitle);
end;

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.PopulateNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
const
  CheckBoxStateMap: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  I: Integer;
begin
  chcbNavigatorButtons.Properties.Items.BeginUpdate;
  try
    chcbNavigatorButtons.Properties.Items.Clear;
    for I := 0 to NavigatorButtonCount - 1 do
    begin
      chcbNavigatorButtons.Properties.Items.Add.Description := scxGridWizardNavigatorButtonsNames[I];
      chcbNavigatorButtons.States[I] := CheckBoxStateMap[AButtons.Buttons[I].Visible];
    end;
  finally
    chcbNavigatorButtons.Properties.Items.EndUpdate;
  end;
end;

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.SaveNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
const
  CheckBoxStateMap: array [TcxCheckBoxState] of Boolean = (False, True, False);
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    AButtons.Buttons[I].Visible := CheckBoxStateMap[chcbNavigatorButtons.States[I]];
end;

{ Events }

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

procedure TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame.chbNavigatorClick(Sender: TObject);
begin
  chcbNavigatorButtons.Enabled := chbNavigator.Checked;
end;

end.
