{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxPivotGridOLAPConnectionDesigner;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  dxCore, cxClasses, cxLookAndFeelPainters, cxGraphics, cxDropDownEdit,
  cxCalendar, cxTextEdit, cxMaskEdit, cxButtonEdit, StdCtrls, cxRadioGroup,
  cxLabel, cxControls, cxContainer, cxEdit, cxGroupBox, cxButtons, cxLookAndFeels,
  cxPivotGridStrs, DB, ADODB, cxPivotGridOLAPDataSource, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxForms;

type
  TcxPivotGridOLAPConnectionDesignerClass = class of TfrmConnectionDesigner;
  TcxPivotGridOLAPPopulateComboProc = procedure (List: TStrings) of object;

  TfrmConnectionDesigner = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dlgOpen: TOpenDialog;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edtCube: TcxComboBox;
    edtDatabase: TcxComboBox;
    edtServer: TcxButtonEdit;
    lbConnectType: TdxLayoutItem;
    lbCube: TdxLayoutItem;
    lbDataBase: TdxLayoutItem;
    lbServer: TdxLayoutItem;
    rbAnalysisServer: TcxRadioButton;
    rbCubeFile: TcxRadioButton;

    procedure edtDatabaseChanged(Sender: TObject);
    procedure edtServerChanged(Sender: TObject);
    procedure edtServerPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure rbAnalysisServerClick(Sender: TObject);
  private
    FChangeLockCount: Integer;
    FProviderClass: TcxCustomPivotGridOLAPProviderClass;

    function IsDataValid: Boolean;
    procedure PopulateComboBox(AComboBox: TcxComboBox; AProc: TcxPivotGridOLAPPopulateComboProc);
    procedure SelectConnectionType(AUseServer: Boolean);
  protected
    property ProviderClass: TcxCustomPivotGridOLAPProviderClass read FProviderClass;
  protected
    function CreateProvider: TcxCustomPivotGridOLAPProvider;
    procedure DoShow; override;
    function GetConnectionString: WideString; virtual;
    procedure Initialize; virtual;
    procedure InitializeCatalogList; virtual;
    procedure InitializeCubeList; virtual;
    procedure Validate;
  public
    property ConnectionString: WideString read GetConnectionString;
  end;

const
   ConnectionDesignerClass: TcxPivotGridOLAPConnectionDesignerClass = TfrmConnectionDesigner;

function cxPivotGridOLAPCreateConnectionString(var ACube: string;
  AProviderClass: TcxCustomPivotGridOLAPProviderClass; ALookAndFeel: TcxLookAndFeel = nil): string; overload;
function cxPivotGridOLAPCreateConnectionString(var ACube: WideString;
  ALookAndFeel: TcxLookAndFeel = nil): WideString; overload; deprecated 'use the cxPivotGridOLAPCreateConnectionString overloaded function instead and pass a provider class as the AProviderClass parameter';
implementation

uses
  cxPivotGridOLAPOLEDBProvider;

{$R *.dfm}

type
  TcxCustomPivotGridOLAPProviderAccess = class(TcxCustomPivotGridOLAPProvider);

function cxPivotGridOLAPCreateConnectionString(var ACube: string;
  AProviderClass: TcxCustomPivotGridOLAPProviderClass; ALookAndFeel: TcxLookAndFeel = nil): string;
var
  AForm: TfrmConnectionDesigner;
begin
  Result := '';
  AForm := ConnectionDesignerClass.Create(nil);
  try
    ACube := '';
    AForm.FProviderClass := AProviderClass;
    if ALookAndFeel <> nil then
      SetControlLookAndFeel(AForm, ALookAndFeel);
    if AForm.ShowModal = mrOk then
    begin
      Result := AForm.ConnectionString;
      ACube := VarToStr(AForm.edtCube.EditValue);
    end;
  finally
    AForm.Free;
  end;
end;

function cxPivotGridOLAPCreateConnectionString(var ACube: WideString; ALookAndFeel: TcxLookAndFeel = nil): WideString;
var
  S: string;
begin
  Result := cxPivotGridOLAPCreateConnectionString(S, TcxPivotGridOLAPOLEDBProvider, ALookAndFeel);
  ACube := S;
end;

{ TfrmConnectionDesigner }

function TfrmConnectionDesigner.CreateProvider: TcxCustomPivotGridOLAPProvider;
begin
  Result := ProviderClass.Create(nil);
  try
    TcxCustomPivotGridOLAPProviderAccess(Result).ConnectionString := GetConnectionString;
    TcxCustomPivotGridOLAPProviderAccess(Result).Open;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TfrmConnectionDesigner.DoShow;
begin
  inherited DoShow;
  SelectConnectionType(rbAnalysisServer.Checked);
  Initialize;
  Validate;
end;

function TfrmConnectionDesigner.GetConnectionString: WideString;
var
  ASecurity: string;
begin
  Result := 'Provider=MSOLAP%s;Persist Security Info=False;Data Source=' + edtServer.Text;
  if rbAnalysisServer.Checked then
  begin
    if edtDatabase.Text <> '' then
      Result := Result + ';Initial Catalog=' + edtDatabase.EditValue;
    ASecurity := '';
  end
  else
    ASecurity := ';Integrated Security=SSPI';

  Result := Format(Result, [ASecurity]);
end;

procedure TfrmConnectionDesigner.Initialize;
begin
  lbConnectType.Caption := cxGetResourceString(@scxConnectUsing);
  rbAnalysisServer.Caption := cxGetResourceString(@scxAnalysisServer);
  rbCubeFile.Caption := cxGetResourceString(@scxCubeFile);
  lbServer.Caption := cxGetResourceString(@scxServer);
  lbDataBase.Caption := cxGetResourceString(@scxDatabase);
  lbCube.Caption := cxGetResourceString(@scxCube);
  btnOk.Caption := cxGetResourceString(@scxPivotGridOk);
  btnCancel.Caption := cxGetResourceString(@scxPivotGridCancel);
end;

procedure TfrmConnectionDesigner.InitializeCatalogList;
var
  AProvider: TcxCustomPivotGridOLAPProvider;
begin
  AProvider := CreateProvider;
  try
    PopulateComboBox(edtDatabase, TcxCustomPivotGridOLAPProviderAccess(AProvider).PopulateCatalogs);
    if edtDatabase.Properties.Items.Count > 0 then
      edtDatabase.Properties.DropDownListStyle := lsFixedList
    else
      edtDatabase.Properties.DropDownListStyle := lsEditList;
  finally
    AProvider.Free;
  end;
end;

procedure TfrmConnectionDesigner.InitializeCubeList;
var
  AProvider: TcxCustomPivotGridOLAPProvider;
begin
  AProvider := CreateProvider;
  try
    PopulateComboBox(edtCube, AProvider.PopulateCubeNames);
  finally
    AProvider.Free;
  end;
end;

procedure TfrmConnectionDesigner.Validate;
begin
  btnOk.Enabled := IsDataValid;
end;

function TfrmConnectionDesigner.IsDataValid: Boolean;
begin
  Result := (edtServer.Text <> '') and (rbCubeFile.Checked or ((edtDatabase.Text <> '') and (edtCube.Text <> '')));
end;

procedure TfrmConnectionDesigner.PopulateComboBox(AComboBox: TcxComboBox; AProc: TcxPivotGridOLAPPopulateComboProc);
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    AComboBox.Properties.Items.Clear;
    AProc(AComboBox.Properties.Items);
    if AComboBox.Properties.Items.Count > 0 then
      AComboBox.ItemIndex := 0
    else
      AComboBox.Text := '';
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TfrmConnectionDesigner.SelectConnectionType(AUseServer: Boolean);
begin
  Inc(FChangeLockCount);
  try
    edtServer.Properties.Buttons.Items[0].Visible := not AUseServer;
    if AUseServer then
      lbServer.Caption := cxGetResourceString(@scxServer)
    else
      lbServer.Caption := cxGetResourceString(@scxCubeFile);

    // Do not change the order
    lbDataBase.Enabled := AUseServer;
    lbCube.Enabled := AUseServer;
    edtServer.Text := '';

    edtCube.Enabled := AUseServer;
    edtCube.Properties.Items.Clear;
    edtCube.Text := '';

    edtDatabase.Enabled := AUseServer;
    edtDatabase.Properties.Items.Clear;
    edtDatabase.Text := '';
  finally
    Dec(FChangeLockCount);
    Validate;
  end;
end;

procedure TfrmConnectionDesigner.rbAnalysisServerClick(Sender: TObject);
begin
  SelectConnectionType(TComponent(Sender).Tag <> 0);
end;

procedure TfrmConnectionDesigner.edtServerPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if dlgOpen.Execute then
    edtServer.EditValue := dlgOpen.FileName;
end;

procedure TfrmConnectionDesigner.edtDatabaseChanged(Sender: TObject);
begin
  try
    if rbAnalysisServer.Checked and (FChangeLockCount = 0) then
    begin
      Inc(FChangeLockCount);
      try
        edtCube.Properties.Items.Clear;
      finally
        Dec(FChangeLockCount);
      end;
      InitializeCubeList;
    end;
  finally
    Validate;
  end;
end;

procedure TfrmConnectionDesigner.edtServerChanged(Sender: TObject);
begin
  try
    if rbAnalysisServer.Checked and (edtServer.Text <> '') and (FChangeLockCount = 0) then
    begin
      Inc(FChangeLockCount);
      try
        edtDatabase.Properties.Items.Clear;
        edtCube.Properties.Items.Clear;
      finally
        Dec(FChangeLockCount);
      end;
      InitializeCatalogList;
    end;
  finally
    Validate;
  end;
end;

end.
