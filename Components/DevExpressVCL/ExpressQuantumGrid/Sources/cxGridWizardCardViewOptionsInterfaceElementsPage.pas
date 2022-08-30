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

unit cxGridWizardCardViewOptionsInterfaceElementsPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutContainer, cxCheckBox, StdCtrls, cxRadioGroup, cxLabel, ExtCtrls, dxLayoutControl,
  cxGridWizardCustomPage, cxGridWizardStrs, cxGridCustomTableView, cxGridCardView, dxLayoutLookAndFeels,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCheckComboBox, cxNavigator;

type
  { TcxGridWizardCardViewOptionsInterfaceElementsPageFrame }

  TcxGridWizardCardViewOptionsInterfaceElementsPageFrame = class(TcxGridWizardCustomPageFrame)
    chbEmptyRows: TcxCheckBox;
    chbNavigator: TcxCheckBox;
    chcbNavigatorButtons: TcxCheckComboBox;
    lbCardExpandButtonAlignment: TcxLabel;
    lbOther: TcxLabel;
    lbRowLayout: TcxLabel;
    lciAlignmentLeft: TdxLayoutItem;
    lciAlignmentRight: TdxLayoutItem;
    lciCardExpandButtonAlignment: TdxLayoutItem;
    lciEmptyRows: TdxLayoutItem;
    lciNavigator: TdxLayoutItem;
    lciNavigatorButtons: TdxLayoutItem;
    lcInterfaceElementsPageGroup12: TdxLayoutGroup;
    lcInterfaceElementsPageGroup13: TdxLayoutGroup;
    lcInterfaceElementsPageGroup2: TdxLayoutGroup;
    lcInterfaceElementsPageGroup6: TdxLayoutGroup;
    lcInterfaceElementsPageSeparatorItem1: TdxLayoutSeparatorItem;
    lcInterfaceElementsPageSeparatorItem4: TdxLayoutSeparatorItem;
    lciOther: TdxLayoutItem;
    lciPreviewGrid: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainGroup4: TdxLayoutGroup;
    lcMainGroup5: TdxLayoutGroup;
    lcMainGroup6: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    pnPreviewGrid: TPanel;
    rbAlignmentLeft: TcxRadioButton;
    rbAlignmentRight: TcxRadioButton;
    rbLayoutHorizontal: TcxRadioButton;
    rbLayoutVertical: TcxRadioButton;
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

{ TcxGridWizardCardViewOptionsInterfaceElementsPageFrame }

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.ApplyLocalization;
begin
  lbCardExpandButtonAlignment.Caption := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignment);
  lbCardExpandButtonAlignment.Hint := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignmentHint);

  rbAlignmentLeft.Caption := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignmentLeft);
  rbAlignmentLeft.Hint := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignmentLeftHint);

  rbAlignmentRight.Caption := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignmentRight);
  rbAlignmentRight.Hint := cxGetResourceString(@scxgwUIElementsCardViewExpandButtonAlignmentRightHint);

  lbOther.Caption := cxGetResourceString(@scxgwCommonGroupCaptionOthers);

  chbEmptyRows.Caption := cxGetResourceString(@scxgwUIElementsCardViewEmptyRows);
  chbEmptyRows.Hint := cxGetResourceString(@scxgwUIElementsCardViewEmptyRowsHint);

  chbNavigator.Caption := cxGetResourceString(@scxgwUIElementsPageNavigator);
  chbNavigator.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorHint);

  lciNavigatorButtons.Caption := cxGetResourceString(@scxgwUIElementsPageNavigatorButtons);
  chcbNavigatorButtons.Hint := cxGetResourceString(@scxgwUIElementsPageNavigatorButtonsHint);

  lbRowLayout.Caption := cxGetResourceString(@scxgwUIElementsCardViewRowLayout);
  lbRowLayout.Hint := cxGetResourceString(@scxgwUIElementsCardViewRowLayoutHint);

  rbLayoutHorizontal.Caption := cxGetResourceString(@scxgwUIElementsCardViewRowLayoutHorizontal);
  rbLayoutHorizontal.Hint := cxGetResourceString(@scxgwUIElementsCardViewRowLayoutHorizontalHint);

  rbLayoutVertical.Caption := cxGetResourceString(@scxgwUIElementsCardViewRowLayoutVertical);
  rbLayoutVertical.Hint := cxGetResourceString(@scxgwUIElementsCardViewRowLayoutVerticalHint);
end;

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.ApplySettings;
var
  AView: TcxGridCardView;
begin
  Helper.Assign(PreviewGrid.ActiveView);

  AView := TcxGridCardView(Helper.GridView);
  if rbAlignmentLeft.Checked then
    AView.OptionsView.CardExpandButtonAlignment := cebaLeft
  else
    AView.OptionsView.CardExpandButtonAlignment := cebaRight;

  if rbLayoutHorizontal.Checked then
    AView.RowLayout := rlHorizontal
  else
    AView.RowLayout := rlVertical;

  AView.OptionsView.EmptyRows := chbEmptyRows.Checked;
  AView.Navigator.Visible := chbNavigator.Checked;
  SaveNavigatorButtons(AView.Navigator.Buttons);
end;

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.LoadSettings;
var
  AView: TcxGridCardView;
begin
  PreviewGrid.Parent := pnPreviewGrid;
  Helper.PreparePreview(PreviewGrid.ActiveView);
  PreviewGrid.Enabled := True;

  AView := TcxGridCardView(Helper.GridView);
  rbAlignmentLeft.Checked := AView.OptionsView.CardExpandButtonAlignment = cebaLeft;
  rbAlignmentRight.Checked := AView.OptionsView.CardExpandButtonAlignment = cebaRight;
  chbEmptyRows.Checked := AView.OptionsView.EmptyRows;
  chbNavigator.Checked := AView.Navigator.Visible;
  chcbNavigatorButtons.Enabled := chbNavigator.Checked;
  PopulateNavigatorButtons(AView.Navigator.Buttons);
  rbLayoutHorizontal.Checked := AView.RowLayout = rlHorizontal;
  rbLayoutVertical.Checked := AView.RowLayout = rlVertical;
end;

function TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageDescription);
end;

function TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwUIElementsPageTitle);
end;

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.PopulateNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
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

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.SaveNavigatorButtons(AButtons: TcxGridViewNavigatorButtons);
const
  CheckBoxStateMap: array [TcxCheckBoxState] of Boolean = (False, True, False);
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    AButtons.Buttons[I].Visible := CheckBoxStateMap[chcbNavigatorButtons.States[I]];
end;

{ Events }

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.RefreshPreviewGrid(Sender: TObject);
begin
  RefreshPreviewGridContent;
end;

procedure TcxGridWizardCardViewOptionsInterfaceElementsPageFrame.chbNavigatorClick(Sender: TObject);
begin
  chcbNavigatorButtons.Enabled := chbNavigator.Checked;
end;

end.
