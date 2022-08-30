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

unit cxEditRepositoryEditor;

{$I cxVer.inc}

interface

uses
  DesignIntf, Variants, Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxDesignWindows, StdCtrls, cxEdit, Menus, ExtCtrls;

type
  TcxEditRepositoryEditor = class(TcxDesignFormEditor)
    LBItems: TListBox;
    btAdd: TButton;
    btDelete: TButton;
    btClose: TButton;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    N1: TMenuItem;
    miSelectAll: TMenuItem;
    procedure LBItemsClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure SelectItem(AItem: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  private
    function GetEditRepository: TcxEditRepository;
    procedure UpdateButtons;
    procedure UpdateDesigner(Sender: TObject);
    procedure UpdateItems;
  protected
    procedure InitFormEditor; override;
  public
    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
    property EditRepository: TcxEditRepository read GetEditRepository;
  end;

procedure RegisterEditRepositoryItem(
  AEditRepositoryItemClass: TcxEditRepositoryItemClass; const ADescription: string);
procedure UnRegisterEditRepositoryItem(
  AEditRepositoryItemClass: TcxEditRepositoryItemClass);

procedure ShowEditRepositoryEditor(ADesigner: IDesigner;
  AEditRepository: TcxEditRepository);

procedure GenEditRepositoryItemName(AEditRepository: TcxEditRepository;
  AItem: TcxEditRepositoryItem);

implementation

{$R *.dfm}

uses
  cxClasses, cxSelectEditRepositoryItem;

var
  EditRepositoryList: TcxRegisteredClasses;
  SetupSelectData: TcxSelectRepositoryItemSetup;

procedure RegisterEditRepositoryItem(
  AEditRepositoryItemClass: TcxEditRepositoryItemClass; const ADescription: string);
begin
  EditRepositoryList.Register(AEditRepositoryItemClass, ADescription)
end;

procedure UnRegisterEditRepositoryItem(
  AEditRepositoryItemClass: TcxEditRepositoryItemClass);
begin
  EditRepositoryList.UnRegister(AEditRepositoryItemClass);
end;

procedure ShowEditRepositoryEditor(ADesigner: IDesigner;
  AEditRepository: TcxEditRepository);
begin
  ShowFormEditorClass(ADesigner, AEditRepository, TcxEditRepositoryEditor);
end;

procedure GenEditRepositoryItemName(AEditRepository: TcxEditRepository;
  AItem: TcxEditRepositoryItem);
var
  ABaseName: string;
  I: Integer;
begin
  ABaseName := AItem.GetBaseName;
  I := 1;
  while I <> -1 do
    try
      AItem.Name := ABaseName + IntToStr(I);
      I := -1;
    except
      on EComponentError do //Ignore rename errors
        Inc(I);
    end;
end;

procedure TcxEditRepositoryEditor.LBItemsClick(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to LBItems.Items.Count - 1 do
      if LBItems.Selected[I] then
        AList.Add(LBItems.Items.Objects[I]);
    SelectComponents(AList, EditRepository);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

function TcxEditRepositoryEditor.GetEditRepository: TcxEditRepository;
begin
  Result := Component as TcxEditRepository;
end;

procedure TcxEditRepositoryEditor.UpdateButtons;
begin
  btDelete.Enabled := LBItems.SelCount <> 0;
  miDelete.Enabled := btDelete.Enabled;
  miSelectAll.Enabled := btDelete.Enabled;
end;

procedure TcxEditRepositoryEditor.btDeleteClick(Sender: TObject);

  function FindItemToSelect: TObject;
  var
    I: Integer;
  begin
    Result := nil;
    with LBItems do
    begin
      if ItemIndex = -1 then Exit;
      if not Selected[ItemIndex] then
        Result := Items.Objects[ItemIndex]
      else
      begin
        for I := ItemIndex + 1 to Items.Count - 1 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
        for I := ItemIndex - 1 downto 0 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
      end;
    end;
  end;

var
  I: Integer;
  AItem: TObject;
begin
  if LBItems.SelCount > 0 then
  begin
    AItem := FindItemToSelect;
    for I := 0 to LBItems.Items.Count - 1 do
      if LBItems.Selected[I] then
        TcxEditRepositoryItem(LBItems.Items.Objects[I]).Free;
    UpdateItems;
    SelectItem(AItem);
    UpdateButtons;
    UpdateDesigner(nil);
    LBItemsClick(nil);
  end;
end;

procedure TcxEditRepositoryEditor.UpdateDesigner(Sender: TObject);
begin
  Designer.Modified;
end;

procedure TcxEditRepositoryEditor.UpdateItems;
var
  I, AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
begin
  ListBoxSaveSelection(LBItems, ASelection, AItemIndex, ATopIndex);
  try
    LBItems.Items.Clear;
    for I := 0 to EditRepository.Count - 1 do
      LBItems.Items.AddObject(EditRepository.Items[I].Name, EditRepository.Items[I]);
  finally
    ListBoxRestoreSelection(LBItems, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure TcxEditRepositoryEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  UpdateItems;
  UpdateSelection;
  UpdateButtons;
end;

procedure TcxEditRepositoryEditor.DoItemsModified;
begin
  UpdateItems;
end;

procedure TcxEditRepositoryEditor.SelectionsChanged(
  const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetSelectionList(AList);
    ListBoxSyncSelection(LBItems, AList);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

procedure TcxEditRepositoryEditor.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TcxEditRepositoryEditor.btAddClick(Sender: TObject);
var
  AEditRepositoryItemClass: TcxEditRepositoryItemClass;
  AItem: TcxEditRepositoryItem;
begin
  with SetupSelectData do
    List := EditRepositoryList;
  AEditRepositoryItemClass := GetEditRepositoryItemClass(SetupSelectData);
  if AEditRepositoryItemClass <> nil then
  begin
    with EditRepository do
      AItem := CreateItemEx(AEditRepositoryItemClass, Owner);
    GenEditRepositoryItemName(EditRepository, AItem);
    UpdateItems;
    SelectItem(AItem);
    UpdateButtons;
    UpdateDesigner(nil);
  end;
end;

procedure TcxEditRepositoryEditor.SelectItem(AItem: TObject);
begin
  LBItems.ItemIndex := LBItems.Items.IndexOfObject(AItem);
  ListBoxClearSelection(LBItems);
  if Component <> nil then
    if AItem <> nil then
      Designer.SelectComponent(TComponent(AItem))
    else
      Designer.SelectComponent(Component)
end;

procedure TcxEditRepositoryEditor.miAddClick(Sender: TObject);
begin
  btAddClick(nil);
end;

procedure TcxEditRepositoryEditor.miDeleteClick(Sender: TObject);
begin
  btDeleteClick(nil);
end;

procedure TcxEditRepositoryEditor.miSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(LBItems);
  LBItemsClick(nil);
end;

initialization
  SetupSelectData.Pos := Point(-1, -1);
  EditRepositoryList := TcxRegisteredClasses.Create;

finalization
  FreeAndNil(EditRepositoryList);

end.
