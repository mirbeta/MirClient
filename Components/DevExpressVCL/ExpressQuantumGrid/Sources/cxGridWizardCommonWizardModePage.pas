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

unit cxGridWizardCommonWizardModePage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, ExtCtrls,
  dxCore, cxGraphics, cxControls,
  cxClasses, cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, dxLayoutContainer, dxLayoutControlAdapters, StdCtrls,
  cxRadioGroup, cxContainer, cxEdit, cxListView, dxLayoutControl, cxGrid, cxGridCustomView, dxWizardControl,
  cxGridWizardCustomPage, cxGridWizardStrs, dxLayoutcxEditAdapters, cxLabel, cxImage,
  dxGDIPlusClasses, cxCheckBox, dxLayoutLookAndFeels;

type
  { TcxGridWizardCommonWizardModePageFrame }

  TcxGridWizardCommonWizardModePageFrame = class(TcxGridWizardCustomPageFrame)
    chbDeletePresentStructure: TcxCheckBox;
    imMultiLevelStructure: TImage;
    imSingleLevelStructure: TImage;
    lcgMultiLevelStructureGroup: TdxLayoutGroup;
    lcgSingleLevelStructureGroup: TdxLayoutGroup;
    lciDeletePresentStructure: TdxLayoutItem;
    lciMultiLevelStructure: TdxLayoutItem;
    lciMultiLevelStructureImage: TdxLayoutItem;
    lciSingleLevelStructure: TdxLayoutItem;
    lciSingleLevelStructureImage: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    lcWelcomePageGroup1: TdxLayoutGroup;
    lcWelcomePageGroup2: TdxLayoutGroup;
    lcWelcomePageSeparatorItem: TdxLayoutSeparatorItem;
    rbMultiLevelStructure: TcxRadioButton;
    rbSingleLevelStructure: TcxRadioButton;
    procedure imMultiLevelStructureClick(Sender: TObject);
    procedure imSingleLevelStructureClick(Sender: TObject);
  protected
    function GetCanFinishWizard: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

{$R *.dfm}

{ TcxGridWizardCommonWizardModePageFrame }

procedure TcxGridWizardCommonWizardModePageFrame.ApplyLocalization;
begin
  rbSingleLevelStructure.Caption := cxGetResourceString(@scxgwWizardModePageSingleLevelStructure);
  rbMultiLevelStructure.Caption := cxGetResourceString(@scxgwWizardModePageMultiLevelStructure);
  chbDeletePresentStructure.Caption := cxGetResourceString(@scxgwWizardModePageDeletePresentStructure);
end;

procedure TcxGridWizardCommonWizardModePageFrame.ApplySettings;
begin
  IsMultiLevelStructure := rbMultiLevelStructure.Checked;
  SetDeleteExistingStructure(chbDeletePresentStructure.Checked);
end;

procedure TcxGridWizardCommonWizardModePageFrame.LoadSettings;
begin
  lciDeletePresentStructure.Visible := CustomizedGrid.Levels.Count > 0;
  lcWelcomePageSeparatorItem.Visible := CustomizedGrid.Levels.Count > 0;
  UpdateOwnerButtonsState;
end;

function TcxGridWizardCommonWizardModePageFrame.GetCanFinishWizard: Boolean;
begin
  Result := False;
end;

function TcxGridWizardCommonWizardModePageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwWizardModePageDescription);
end;

function TcxGridWizardCommonWizardModePageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwWizardModePageTitle);
end;

{ Events }

procedure TcxGridWizardCommonWizardModePageFrame.imMultiLevelStructureClick(Sender: TObject);
begin
  rbMultiLevelStructure.Checked := True;
end;

procedure TcxGridWizardCommonWizardModePageFrame.imSingleLevelStructureClick(Sender: TObject);
begin
  rbSingleLevelStructure.Checked := True;
end;

end.
