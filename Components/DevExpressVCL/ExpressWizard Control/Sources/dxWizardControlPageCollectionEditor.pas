{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
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

unit dxWizardControlPageCollectionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, DesignIntf, ActnList,
  dxCore, dxWizardControl, dxCustomWizardControl,
  cxClasses, cxPropEditors, dxWizardControlStrs, cxControls;

type

  { TdxWizardControlPageCollectionEditorForm  }

  TdxWizardControlPageCollectionEditorForm = class(TForm)
    acAdd: TAction;
    acDelete: TAction;
    acMoveDown: TAction;
    acMoveUp: TAction;
    alActions: TActionList;
    btnAdd: TButton;
    btnCancel: TButton;
    btnDelete: TButton;
    btnMoveDown: TButton;
    btnMoveUp: TButton;
    btnOK: TButton;
    lbPages: TListBox;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miMoveDown: TMenuItem;
    miMoveUp: TMenuItem;
    miSeparator: TMenuItem;
    pmPagesMenu: TPopupMenu;
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure lbPagesClick(Sender: TObject);
  private
    FDesigner: IDesigner;
    FWizardControl: TdxCustomWizardControl;
    function GetPageName(APage: TdxWizardControlCustomPage): string;
    procedure UpdateActionsState;
  public
    procedure LoadParams;
    procedure SaveParams;

    property WizardControl: TdxCustomWizardControl read FWizardControl;
  end;

function ExecutePageCollectionEditor(AWizardControl: TdxCustomWizardControl; ADesigner: IDesigner): Boolean;
implementation

{$R *.dfm}

function ExecutePageCollectionEditor(AWizardControl: TdxCustomWizardControl; ADesigner: IDesigner): Boolean;
var
  AForm: TdxWizardControlPageCollectionEditorForm;
begin
  AForm := TdxWizardControlPageCollectionEditorForm.Create(nil);
  try
    AForm.FWizardControl := AWizardControl;
    AForm.FDesigner := ADesigner;
    AForm.LoadParams;
    Result := AForm.ShowModal = mrOk;
    if Result then
    begin
      AForm.SaveParams;
	  SetDesignerModified(AWizardControl);
    end;
  finally
    AForm.Free;
  end;
end;

{ TdxWizardControlPageCollectionEditorForm }

procedure TdxWizardControlPageCollectionEditorForm.acAddExecute(Sender: TObject);
begin
  lbPages.AddItem(cxGetResourceString(@sdxWizardControlPageDefaultTitle), nil);
  UpdateActionsState;
end;

procedure TdxWizardControlPageCollectionEditorForm.acDeleteExecute(Sender: TObject);
begin
  lbPages.DeleteSelected;
  UpdateActionsState;
end;

procedure TdxWizardControlPageCollectionEditorForm.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  lbPages.Items.BeginUpdate;
  try
    for I := 1 to lbPages.Count - 1 do
      if lbPages.Selected[I] then
      begin
        lbPages.Items.Exchange(I, I - 1);
        lbPages.Selected[I - 1] := True;
      end;
  finally
    lbPages.Items.EndUpdate;
    UpdateActionsState;
  end;
end;

function TdxWizardControlPageCollectionEditorForm.GetPageName(APage: TdxWizardControlCustomPage): string;
begin
  Result := Format('%s [%s]', [APage.Name, APage.Header.Title]);
end;

procedure TdxWizardControlPageCollectionEditorForm.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  lbPages.Items.BeginUpdate;
  try
    for I := lbPages.Count - 2 downto 0 do
      if lbPages.Selected[I] then
      begin
        lbPages.Items.Exchange(I, I + 1);
        lbPages.Selected[I + 1] := True;
      end;
  finally
    lbPages.Items.EndUpdate;
    UpdateActionsState;
  end;
end;

procedure TdxWizardControlPageCollectionEditorForm.lbPagesClick(Sender: TObject);
begin
  if lbPages.Items.Objects[lbPages.ItemIndex] <> nil then
    TdxWizardControlPage(lbPages.Items.Objects[lbPages.ItemIndex]).Activate;
  UpdateActionsState;
end;

procedure TdxWizardControlPageCollectionEditorForm.LoadParams;
var
  I: Integer;
begin
  lbPages.Items.BeginUpdate;
  try
    lbPages.Clear;
    for I := 0 to WizardControl.PageCount - 1 do
      lbPages.AddItem(GetPageName(WizardControl.Pages[I]), WizardControl.Pages[I]);
  finally
    lbPages.Items.EndUpdate;
    UpdateActionsState;
  end;
end;

procedure TdxWizardControlPageCollectionEditorForm.SaveParams;

  procedure AddNewPages;
  var
    APage: TdxWizardControlCustomPage;
    I: Integer;
  begin
    for I := 0 to lbPages.Count - 1 do
      if lbPages.Items.Objects[I] = nil then
      begin
        APage := WizardControl.AddPage(TdxWizardControlPage);
        APage.Name := FDesigner.UniqueName(TdxWizardControlPage.ClassName);
        lbPages.Items.Objects[I] := APage;
      end;
  end;

  procedure DeletePages;
  var
    I: Integer;
  begin
    for I := WizardControl.PageCount - 1 downto 0 do
      if lbPages.Items.IndexOfObject(WizardControl.Pages[I]) = -1 then
        WizardControl.Pages[I].Free;
  end;

  procedure SetPagesOrder;
  var
    I: Integer;
  begin
    for I := 0 to lbPages.Count - 1 do
      TdxWizardControlPage(lbPages.Items.Objects[I]).PageIndex := I;
  end;

begin
  ObjectInspectorCollapseProperty;
  DeletePages;
  AddNewPages;
  SetPagesOrder;
end;

procedure TdxWizardControlPageCollectionEditorForm.UpdateActionsState;
var
  AHasSelection: Boolean;
begin
  AHasSelection := lbPages.SelCount > 0;
  acDelete.Enabled := AHasSelection;
  acMoveDown.Enabled := AHasSelection and not lbPages.Selected[lbPages.Count - 1];
  acMoveUp.Enabled := AHasSelection and not lbPages.Selected[0];
end;

end.
