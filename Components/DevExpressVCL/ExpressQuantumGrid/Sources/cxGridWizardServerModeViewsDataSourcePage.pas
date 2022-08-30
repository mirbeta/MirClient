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

unit cxGridWizardServerModeViewsDataSourcePage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, StdCtrls, Menus,
  dxCore, cxGraphics, cxControls, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, cxCheckBox,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs, cxGridLevel,
  cxGridCustomView, dxBevel, cxGroupBox, dxLayoutLookAndFeels, cxLabel, dxLayoutControlAdapters,
  cxButtons, dxServerModeData, cxGridServerModeDataDefinitions, cxRadioGroup, cxCheckComboBox,
  cxGridWizardServerModeViewHelper, cxMemo;

type
  { TcxGridWizardServerModeViewsDataSourcePageFrame }

  TcxGridWizardServerModeViewsDataSourcePageFrame = class(TcxGridWizardCustomPageFrame)
    btnTestConnection: TcxButton;
    cbDataSource: TcxComboBox;
    lbDataSource: TcxLabel;
    lcgCommonGroup: TdxLayoutGroup;
    lciDataSource: TdxLayoutItem;
    lciTestConnection: TdxLayoutItem;
    lcMainItem1: TdxLayoutItem;
    procedure btnTestConnectionClick(Sender: TObject);
    procedure cbDataSourcePropertiesChange(Sender: TObject);
  private
    function GetDataSource: TdxServerModeCustomDataSource;
    function GetServerModeHelper: IcxGridWizardServerModeViewHelper;
  protected
    function GetCanJumpToNextPage: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    procedure PopulateDataSources;
    procedure UpdateControlStates;

    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
    property ServerModeHelper: IcxGridWizardServerModeViewHelper read GetServerModeHelper;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  Math, StrUtils;

{$R *.dfm}

type
  TdxServerModeCustomDataSourceAccess = class(TdxServerModeCustomDataSource);

function CheckDataSourceConnection(ADataSource: TdxServerModeCustomDataSource): Boolean;
begin
  Result := ADataSource.Connection <> nil;
end;

{ TcxGridWizardServerModeViewsDataSourcePageFrame }

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.ApplyLocalization;
begin
  lbDataSource.Caption := cxGetResourceString(@scxgwCommonDataSource);
  lbDataSource.Hint := cxGetResourceString(@scxgwCommonDataSourceHint);
  btnTestConnection.Caption := cxGetResourceString(@scxgwServerModeDataSourcePageActivate);
end;

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.ApplySettings;
begin
  // do nothing
end;

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.LoadSettings;
begin
  if Tag = 0 then
  begin
    PopulateDataSources;
    Tag := 1;
  end;
  cbDataSource.ItemIndex := Max(0, cbDataSource.Properties.Items.IndexOfObject(DataSource));
  UpdateControlStates;
end;

function TcxGridWizardServerModeViewsDataSourcePageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := (DataSource <> nil) and DataSource.Active;
end;

function TcxGridWizardServerModeViewsDataSourcePageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwServerModeDataSourcePageDescription);
end;

function TcxGridWizardServerModeViewsDataSourcePageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwServerModeDataSourcePageTitle);
end;

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.PopulateDataSources;
begin
  Helper.PopulateComponents(TdxServerModeCustomDataSource, @CheckDataSourceConnection, cbDataSource.Properties.Items);
  cbDataSource.ItemIndex := -1;
end;

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.UpdateControlStates;
begin
  lciTestConnection.Enabled := (DataSource <> nil) and not DataSource.Active;
  UpdateOwnerButtonsState;
end;

function TcxGridWizardServerModeViewsDataSourcePageFrame.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := ServerModeHelper.GetSelectedDataSource;
end;

function TcxGridWizardServerModeViewsDataSourcePageFrame.GetServerModeHelper: IcxGridWizardServerModeViewHelper;
begin
  Result := Helper as IcxGridWizardServerModeViewHelper;
end;

{ Events }

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.btnTestConnectionClick(Sender: TObject);
begin
  if DataSource <> nil then
  begin
    ShowHourglassCursor;
    try
      DataSource.Active := True;
      UpdateControlStates;
    finally
      HideHourglassCursor;
    end;
  end;
end;

procedure TcxGridWizardServerModeViewsDataSourcePageFrame.cbDataSourcePropertiesChange(Sender: TObject);
begin
  ServerModeHelper.SelectDataSource(TdxServerModeCustomDataSource(cbDataSource.ItemObject));
  UpdateControlStates;
end;

end.
