{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxUIGeneratorWizard;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls, CheckLst, Menus,
  Generics.Defaults, Generics.Collections, dxUIGenerator, ActnList, dxForms;

const
  sCreateNewTarget = 'Create new %s';
  sCaptionTemplate = 'Generate Ribbon/Toolbar UI for %s';

type

  { TfrmUIGenerator }

  TfrmUIGenerator = class(TdxForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbActionLists: TComboBox;
    cbTarget: TComboBox;
    clbCategories: TCheckListBox;
    gbActionList: TGroupBox;
    gbBars: TGroupBox;
    gbTarget: TGroupBox;
    miCheckSelected: TMenuItem;
    miSelectAll: TMenuItem;
    miUncheckSelected: TMenuItem;
    N1: TMenuItem;
    pmCategories: TPopupMenu;

    procedure miCheckSelectedClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  protected
    FAdaptersForNewComponents: TList<TdxUIGeneratorAdapterClass>;
    FComponent: TComponent;

    procedure Generate;
    procedure Initialize(AComponent: TComponent);
    procedure PopulateActionLists(AOwnerForm: TComponent);
    procedure PopulateCategories(AComponent: TComponent);
    procedure PopulateTargets(AOwnerForm: TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute(AComponent: TComponent);
  end;

implementation

uses
  Math, cxControls;

{$R *.dfm}

{ TfrmUIGenerator }

constructor TfrmUIGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdaptersForNewComponents := TList<TdxUIGeneratorAdapterClass>.Create;
  clbCategories.MultiSelect := True;
end;

destructor TfrmUIGenerator.Destroy;
begin
  FreeAndNil(FAdaptersForNewComponents);
  inherited Destroy;
end;

class procedure TfrmUIGenerator.Execute(AComponent: TComponent);
var
  ADialog: TfrmUIGenerator;
begin
  ADialog := TfrmUIGenerator.Create(nil);
  try
    ADialog.Initialize(AComponent);
    if ADialog.ShowModal = mrOk then
      ADialog.Generate;
  finally
    ADialog.Free;
  end;
end;

procedure TfrmUIGenerator.Generate;
var
  AAdapter: TdxUIGeneratorAdapter;
  AAdapterClass: TdxUIGeneratorAdapterClass;
  ATarget: TComponent;
  I: Integer;
begin
  ShowHourglassCursor;
  try
    ATarget := TComponent(cbTarget.Items.Objects[cbTarget.ItemIndex]);
    if ATarget = nil then
      ATarget := FAdaptersForNewComponents[cbTarget.ItemIndex].CreateComponent(FComponent.Owner);

    if TdxUIGenerator.GetAdapter(ATarget, AAdapterClass) then
    begin
      AAdapter := AAdapterClass.Create(ATarget);
      try
        AAdapter.ActionList := TActionList(cbActionLists.Items.Objects[cbActionLists.ItemIndex]);
        AAdapter.BeginUpdate;
        try
          for I := 0 to clbCategories.Count - 1 do
          begin
            if clbCategories.Checked[I] then
              AAdapter.Generate(clbCategories.Items.Objects[I] as TdxUIGeneratorCategoryInfo);
          end;
        finally
          AAdapter.EndUpdate;
        end;
      finally
        AAdapter.Free;
      end;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TfrmUIGenerator.Initialize(AComponent: TComponent);
begin
  FComponent := AComponent;
  PopulateCategories(AComponent);
  PopulateActionLists(AComponent.Owner);
  PopulateTargets(AComponent.Owner);
  Caption := Format(sCaptionTemplate, [AComponent.Name]);
end;

procedure TfrmUIGenerator.PopulateActionLists(AOwnerForm: TComponent);
var
  AComponent: TComponent;
  I: Integer;
begin
  cbActionLists.Items.BeginUpdate;
  try
    cbActionLists.Items.Clear;
    cbActionLists.Items.Add(Format(sCreateNewTarget, [TActionList.ClassName]));

    for I := 0 to AOwnerForm.ComponentCount - 1 do
    begin
      AComponent := AOwnerForm.Components[I];
      if AComponent is TActionList then
        cbActionLists.Items.AddObject(AComponent.Name, AComponent);
    end;

    cbActionLists.ItemIndex := IfThen(cbActionLists.Items.Count > 1, 1);
  finally
    cbActionLists.Items.EndUpdate;
  end;
end;

procedure TfrmUIGenerator.PopulateCategories(AComponent: TComponent);
var
  ACategory: TdxUIGeneratorCategoryInfo;
  AInfo: TdxUIGeneratorComponentInfo;
  AInfoList: TdxUIGeneratorComponentInfoList;
  I, J: Integer;
begin
  clbCategories.Items.BeginUpdate;
  try
    clbCategories.Items.Clear;
    if TdxUIGenerator.GetComponentInfo(AComponent, AInfoList) then
    try
      for J := 0 to AInfoList.Count - 1 do
      begin
        AInfo := AInfoList[J];
        for I := 0 to AInfo.Categories.Count - 1 do
        begin
          ACategory := AInfo.Categories[I];
          clbCategories.Items.AddObject(ACategory.ToString, ACategory);
          clbCategories.Checked[clbCategories.Items.Count - 1] := True;
        end;
      end;
    finally
      AInfoList.Free;
    end;
  finally
    clbCategories.Items.EndUpdate;
  end;
end;

procedure TfrmUIGenerator.PopulateTargets(AOwnerForm: TComponent);
var
  AAdapter: TdxUIGeneratorAdapterClass;
  AComponent: TComponent;
  I: Integer;
begin
  for I := 0 to TdxUIGenerator.AdapterCount - 1 do
  begin
    AAdapter := TdxUIGenerator.Adapters[I];
    if AAdapter.CanCreateComponent(AOwnerForm) then
      FAdaptersForNewComponents.Add(AAdapter);
  end;

  cbTarget.Items.BeginUpdate;
  try
    cbTarget.Items.Clear;
    for I := 0 to FAdaptersForNewComponents.Count - 1 do
      cbTarget.Items.Add(Format(sCreateNewTarget, [FAdaptersForNewComponents[I].GetComponentClass.ClassName]));

    for I := 0 to AOwnerForm.ComponentCount - 1 do
    begin
      AComponent := AOwnerForm.Components[I];
      if TdxUIGenerator.GetAdapter(AComponent, AAdapter) then
        cbTarget.Items.AddObject(AComponent.Name, AComponent);
    end;

    cbTarget.ItemIndex := FAdaptersForNewComponents.Count - Ord(cbTarget.Items.Count <= FAdaptersForNewComponents.Count);
  finally
    cbTarget.Items.EndUpdate;
  end;
end;

procedure TfrmUIGenerator.miCheckSelectedClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to clbCategories.Count - 1 do
  begin
    if clbCategories.Selected[I] then
      clbCategories.Checked[I] := TComponent(Sender).Tag <> 0;
  end;
end;

procedure TfrmUIGenerator.miSelectAllClick(Sender: TObject);
var
  AItemIndex: Integer;
  I: Integer;
begin
  AItemIndex := clbCategories.ItemIndex;
  for I := 0 to clbCategories.Count - 1 do
    clbCategories.Selected[I] := True;
  clbCategories.ItemIndex := Max(0, AItemIndex);
end;

end.
