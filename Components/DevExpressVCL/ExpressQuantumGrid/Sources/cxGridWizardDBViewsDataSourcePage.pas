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

unit cxGridWizardDBViewsDataSourcePage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB,
  dxCore, cxGraphics, cxControls, cxClasses,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, cxCheckBox,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs, cxGridLevel,
  cxGridCustomView, cxGridDBDataDefinitions, dxBevel, cxGroupBox, dxLayoutLookAndFeels, cxLabel, cxDBData, cxCheckComboBox;

type
  { TcxGridWizardDBViewsDataSourcePageFrame }

  TcxGridWizardDBViewsDataSourcePageFrame = class(TcxGridWizardCustomPageFrame)
    cbDataSource: TcxComboBox;
    cbDetailKeyField: TcxCheckComboBox;
    cbKeyField: TcxCheckComboBox;
    cbMasterKeyField: TcxCheckComboBox;
    cbMasterView: TcxComboBox;
    chbDetail: TcxCheckBox;
    lbCommon: TcxLabel;
    lbDataSource: TcxLabel;
    lbDetailKeyField: TcxLabel;
    lbKeyFields: TcxLabel;
    lbMasterKeyFields: TcxLabel;
    lbMasterView: TcxLabel;
    lcDataSourcePageGroup1: TdxLayoutGroup;
    lcDataSourcePageGroup3: TdxLayoutGroup;
    lcDataSourcePageGroup6: TdxLayoutGroup;
    lcDataSourcePageSeparatorItem: TdxLayoutSeparatorItem;
    lcDataSourcePageSeparatorItem1: TdxLayoutSeparatorItem;
    lcgCommonCaptionGroup: TdxLayoutGroup;
    lcgCommonGroup: TdxLayoutGroup;
    lcgDetailGroup: TdxLayoutGroup;
    lcgIsDetailGroup: TdxLayoutGroup;
    lciCommon: TdxLayoutItem;
    lciDataSource: TdxLayoutItem;
    lciDetail: TdxLayoutItem;
    lciDetailKeyField: TdxLayoutItem;
    lciKeyField: TdxLayoutItem;
    lciMasterKeyField: TdxLayoutItem;
    lciMasterView: TdxLayoutItem;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
    procedure cbDataSourcePropertiesChange(Sender: TObject);
    procedure cbDetailKeyFieldPropertiesChange(Sender: TObject);
    procedure cbKeyFieldPropertiesChange(Sender: TObject);
    procedure cbMasterKeyFieldPropertiesChange(Sender: TObject);
    procedure cbMasterViewPropertiesChange(Sender: TObject);
    procedure chbDetailPropertiesChange(Sender: TObject);
  private
    function CanBeMasterView(AView: TcxCustomGridView): Boolean;
    function GetDataController: TcxDBDataController;
  protected
    function GetCanJumpToNextPage: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    procedure CheckMultiLevelStructure;
    procedure LoadPageContent;
    procedure PopulateDataSetFields(ADataSource: TDataSource; AComboBox: TcxCheckComboBox);
    procedure PopulateDataSources;
    procedure PopulateMasterViewDataSetFields;
    procedure PopulateMasterViews;

    property DataController: TcxDBDataController read GetDataController;
  public
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  Math, StrUtils, cxGridWizardCustomHelper;

{$R *.dfm}

function CheckDataSet(ADataSource: TDataSource): Boolean;
begin
  Result := ADataSource.DataSet <> nil;
end;

{ TcxGridWizardDBViewsDataSourcePageFrame }

procedure TcxGridWizardDBViewsDataSourcePageFrame.ApplyLocalization;
begin
  lbDataSource.Caption := cxGetResourceString(@scxgwCommonDataSource);
  lbDataSource.Hint := cxGetResourceString(@scxgwCommonDataSourceHint);

  lbKeyFields.Caption := cxGetResourceString(@scxgwCommonKeyFields);
  lbKeyFields.Hint := cxGetResourceString(@scxgwCommonKeyFieldsHint);

  lbCommon.Caption := cxGetResourceString(@scxgwCommonGroupCaptionCommon);
  chbDetail.Caption := cxGetResourceString(@scxgwDataSourcePageIsDetailView);

  lbMasterView.Caption := cxGetResourceString(@scxgwDataSourcePageMasterView);
  lbMasterView.Hint := cxGetResourceString(@scxgwDataSourcePageMasterViewHint);

  lbMasterKeyFields.Caption := cxGetResourceString(@scxgwDataSourcePageMasterViewKeyFieldNames);
  lbMasterKeyFields.Hint := cxGetResourceString(@scxgwDataSourcePageMasterViewKeyFieldNamesHint);

  lbDetailKeyField.Caption := cxGetResourceString(@scxgwDataSourcePageDetailKeyFieldNames);
  lbDetailKeyField.Hint := cxGetResourceString(@scxgwDataSourcePageDetailKeyFieldNamesHint);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.ApplySettings;
begin
  SetIsDetail(chbDetail.Checked);
  SetMasterGridView(cbMasterView.ItemObject as TcxCustomGridView);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.LoadSettings;
begin
  LoadPageContent;
  SetCheckComboBoxValue(cbKeyField, DataController.KeyFieldNames);
  SetCheckComboBoxValue(cbDetailKeyField, DataController.DetailKeyFieldNames);
  if IsMultiLevelStructure and IsDetailViewCustomizing then
    cbMasterView.ItemIndex := cbMasterView.Properties.Items.IndexOf(MultiLevelStructureMasterViewName);
end;

function TcxGridWizardDBViewsDataSourcePageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := (cbDataSource.Text <> '') and not (chbDetail.Checked and (cbMasterView.Text = ''));
end;

function TcxGridWizardDBViewsDataSourcePageFrame.CanBeMasterView(AView: TcxCustomGridView): Boolean;
var
  AHelperClass: TcxGridWizardCustomHelperClass;
begin
  AHelperClass := cxGridWizardHelperInfoList.GetHelperClassBy(AView.ClassType);
  Result := (AHelperClass <> nil) and AHelperClass.CanBeMasterView and (AView <> SelectedGridView);
  if Result and (SelectedGridView <> nil) then
    Result := not AView.HasAsMaster(SelectedGridView);
end;

function TcxGridWizardDBViewsDataSourcePageFrame.GetDataController: TcxDBDataController;
begin
  Result := (Helper as IcxGridWizardHelperDBDataControllerSupport).GetDataController;
end;

function TcxGridWizardDBViewsDataSourcePageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwDataSourcePageDescription);
end;

function TcxGridWizardDBViewsDataSourcePageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwDataSourcePageTitle);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.CheckMultiLevelStructure;
var
  AIsDetail: Boolean;
begin
  if SelectedGridView <> nil then
    chbDetail.Checked := SelectedGridView.IsDetail
  else
  begin
    lcgCommonCaptionGroup.Visible := False;
    lcgIsDetailGroup.Visible := False;
    AIsDetail := IsMultiLevelStructure and IsDetailViewCustomizing;
    lcMainSpaceItem1.Visible := AIsDetail;
    lcgDetailGroup.Visible := AIsDetail;
    chbDetail.Checked := AIsDetail;
  end;
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.LoadPageContent;
begin
  if Tag = 0 then
  begin
    PopulateDataSources;
    CheckMultiLevelStructure;
    PopulateMasterViews;
    Tag := 1;
  end;
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.PopulateDataSetFields(ADataSource: TDataSource; AComboBox: TcxCheckComboBox);
var
  I: Integer;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    AComboBox.Properties.Items.Clear;
    if ADataSource <> nil then
    begin
      for I := 0 to ADataSource.DataSet.FieldCount - 1 do
        AComboBox.Properties.Items.Add.Description := ADataSource.DataSet.Fields[I].FieldName;
    end;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.PopulateDataSources;
begin
  Helper.PopulateComponents(TDataSource, @CheckDataSet, cbDataSource.Properties.Items);
  cbDataSource.ItemIndex := Max(0, cbDataSource.Properties.Items.IndexOfObject(DataController.DataSource));
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.PopulateMasterViewDataSetFields;
var
  AMasterGridViewDataController: TcxDBDataController;
begin
  AMasterGridViewDataController := TcxDBDataController((cbMasterView.ItemObject as TcxCustomGridView).DataController);
  PopulateDataSetFields(AMasterGridViewDataController.DataSource, cbMasterKeyField);
  SetCheckComboBoxValue(cbMasterKeyField, DataController.MasterKeyFieldNames);
  if cbMasterKeyField.EditValue = 0 then
    SetCheckComboBoxValue(cbMasterKeyField, AMasterGridViewDataController.KeyFieldNames);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.PopulateMasterViews;
var
  ACustomizedGridView: TcxCustomGridView;
  AMasterGridView: TcxCustomGridView;
  I: Integer;
begin
  AMasterGridView := nil;
  cbMasterView.Properties.Items.BeginUpdate;
  try
    cbMasterView.Properties.Items.Clear;
    for I := 0 to CustomizedGrid.ViewCount - 1 do
    begin
      ACustomizedGridView := CustomizedGrid.Views[I];
      if CanBeMasterView(ACustomizedGridView) then
        cbMasterView.Properties.Items.AddObject(ACustomizedGridView.Name, CustomizedGrid.Views[I]);
      if (SelectedGridView <> nil) and SelectedGridView.HasAsMaster(ACustomizedGridView) then
        AMasterGridView := ACustomizedGridView;
    end;
    if AMasterGridView <> nil then
      cbMasterView.ItemIndex := cbMasterView.Properties.Items.IndexOfObject(AMasterGridView);
  finally
    cbMasterView.Properties.Items.EndUpdate;
  end;
end;

{ Events }

procedure TcxGridWizardDBViewsDataSourcePageFrame.cbDataSourcePropertiesChange(Sender: TObject);
begin
  DataController.DataSource := TDataSource(cbDataSource.ItemObject);
  PopulateDataSetFields(DataController.DataSource, cbKeyField);
  PopulateDataSetFields(DataController.DataSource, cbDetailKeyField);
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.cbDetailKeyFieldPropertiesChange(Sender: TObject);
begin
  DataController.DetailKeyFieldNames := GetCheckComboBoxValue(cbDetailKeyField);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.cbKeyFieldPropertiesChange(Sender: TObject);
begin
  DataController.KeyFieldNames := GetCheckComboBoxValue(cbKeyField);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.cbMasterKeyFieldPropertiesChange(Sender: TObject);
begin
  DataController.MasterKeyFieldNames := GetCheckComboBoxValue(cbMasterKeyField);
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.cbMasterViewPropertiesChange(Sender: TObject);
begin
  PopulateMasterViewDataSetFields;
  UpdateOwnerButtonsState;
end;

procedure TcxGridWizardDBViewsDataSourcePageFrame.chbDetailPropertiesChange(Sender: TObject);
begin
  lcgDetailGroup.Enabled := chbDetail.Checked;
  UpdateOwnerButtonsState;
end;

end.
