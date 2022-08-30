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

unit cxStyleRepositoryEditor;

{$I cxVer.inc}

interface

uses
  Windows, DesignIntf, Variants, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxDesignWindows, StdCtrls, cxStyles, ExtCtrls, Menus, ComCtrls;

type
  TcxStyleRepositoryEditor = class(TcxDesignFormEditor)
    lbStyles: TListBox;
    btStyleAdd: TButton;
    btStyleDelete: TButton;
    btClose: TButton;
    pnlStyles: TPanel;
    pmStyles: TPopupMenu;
    miStyleAdd: TMenuItem;
    miStyleDelete: TMenuItem;
    N1: TMenuItem;
    miStyleSelectAll: TMenuItem;
    PageControl1: TPageControl;
    tsStyles: TTabSheet;
    tsStyleSheets: TTabSheet;
    lbStyleSheets: TListBox;
    pnlStyleSheets: TPanel;
    btStyleSheetAdd: TButton;
    btStyleSheetDelete: TButton;
    Button3: TButton;
    pmStyleSheets: TPopupMenu;
    miStyleSheetAdd: TMenuItem;
    miStyleSheetDelete: TMenuItem;
    MenuItem3: TMenuItem;
    miStyleSheetSelectAll: TMenuItem;
    btnStyleSheetEdit: TButton;
    imStyleSheetEdit: TMenuItem;
    pmAddStyleSheet: TPopupMenu;
    btnStyleSheetsSave: TButton;
    SaveDialog: TSaveDialog;
    btnStyleSheetsLoad: TButton;
    OpenDialog: TOpenDialog;
    btnStyleSheetsPredefine: TButton;
    procedure lbStylesClick(Sender: TObject);
    procedure btStyleDeleteClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btStyleAddClick(Sender: TObject);
    procedure miStyleSelectAllClick(Sender: TObject);
    procedure btStyleSheetAddClick(Sender: TObject);
    procedure btStyleSheetDeleteClick(Sender: TObject);
    procedure miStyleSheetSelectAllClick(Sender: TObject);
    procedure lbStyleSheetsClick(Sender: TObject);
    procedure btnStyleSheetEditClick(Sender: TObject);
    procedure btnStyleSheetsSaveClick(Sender: TObject);
    procedure btnStyleSheetsLoadClick(Sender: TObject);
    procedure btnStyleSheetsPredefineClick(Sender: TObject);
  private
    procedure AddStyleSheets(AList: TList);
    function DoStyleGetName(AStyle: TcxStyle): string;
    procedure DoAddStyleSheetMenuItemClick(Sender: TObject);
    procedure InitAddStyleSheetMenu;
    procedure ListBoxSelectComponents(AListBox: TListBox);
    function GetStyleRepository: TcxStyleRepository;
    procedure UpdateButtons;
    procedure UpdateDesigner(Sender: TObject);
    procedure UpdateItems;
    procedure SelectItem(AListBox: TListBox; AItem: TPersistent);
    function FindItemToSelect(AListBox: TListBox): TObject;
    procedure DeleteSelectedComponents(AListBox: TListBox);
  protected
    procedure InitFormEditor; override;
  public
    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
    property StyleRepository: TcxStyleRepository read GetStyleRepository;
  end;

procedure ShowStyleRepositoryEditor(ADesigner: IDesigner;
  AStyleRepository: TcxStyleRepository);

implementation

{$R *.dfm}

uses
  cxStyleSheetEditor, cxStyleSheetsLoad, cxClasses;

procedure ShowStyleRepositoryEditor(ADesigner: IDesigner;
  AStyleRepository: TcxStyleRepository);
begin
  ShowFormEditorClass(ADesigner, AStyleRepository, TcxStyleRepositoryEditor);
end;

function TcxStyleRepositoryEditor.GetStyleRepository: TcxStyleRepository;
begin
  Result := Component as TcxStyleRepository;
end;

procedure TcxStyleRepositoryEditor.UpdateDesigner(Sender: TObject);
begin
  Designer.Modified;
end;

procedure TcxStyleRepositoryEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  UpdateItems;
  UpdateSelection;
  UpdateButtons;
  InitAddStyleSheetMenu;
end;

procedure TcxStyleRepositoryEditor.DoItemsModified;
begin
  UpdateItems;
end;

procedure TcxStyleRepositoryEditor.AddStyleSheets(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if TcxCustomStyleSheet(AList[I]).Name = '' then
      TcxCustomStyleSheet(AList[I]).Name :=
        CreateUniqueName(Component.Owner, nil, TcxCustomStyleSheet(AList[I]), '', '');
    lbStyleSheets.Items.AddObject(TcxCustomStyleSheet(AList[I]).Name,
      TcxCustomStyleSheet(AList[I]));
  end;
end;

function TcxStyleRepositoryEditor.DoStyleGetName(AStyle: TcxStyle): string;
begin
  if AStyle.Name = '' then
    Result := CreateUniqueName(Component.Owner, nil, AStyle, '', '')
  else Result := AStyle.Name;
  lbStyles.Items.AddObject(Result, AStyle);
end;

procedure TcxStyleRepositoryEditor.DoAddStyleSheetMenuItemClick(Sender: TObject);
var
  AItem: TcxCustomStyleSheet;
  AList: TList;
begin
  AList := TList.Create;
  try
    GetRegisteredStyleSheetClasses(AList);
    AItem := StyleRepository.CreateStyleSheetEx(
       TcxCustomStyleSheetClass(AList[TMenuItem(Sender).Tag]), StyleRepository.Owner);
  finally
    AList.Free;
  end;
  AItem.Name := CreateUniqueName(Component.Owner, nil, AItem, '', '');
  UpdateItems;
  SelectItem(lbStyleSheets, AItem);
  UpdateButtons;
  UpdateDesigner(nil);
end;

procedure TcxStyleRepositoryEditor.InitAddStyleSheetMenu;
var
  AList: TList;
  AMenuItem: TMenuItem;
  I: Integer;
begin
  AList := TList.Create;
  try
    GetRegisteredStyleSheetClasses(AList);
    for I := 0 to AList.Count - 1 do
    begin
      AMenuItem := TMenuItem.Create(self);
      pmAddStyleSheet.Items.Add(AMenuItem);
      AMenuItem.Caption := TcxCustomStyleSheetClass(AList[I]).ClassName;
      AMenuItem.Tag := I;
      AMenuItem.OnClick := DoAddStyleSheetMenuItemClick;
    end;
  finally
    AList.Free;
  end;
end;

procedure TcxStyleRepositoryEditor.ListBoxSelectComponents(AListBox: TListBox);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to AListBox.Items.Count - 1 do
      if AListBox.Selected[I] then
        AList.Add(AListBox.Items.Objects[I]);
    SelectComponents(AList, StyleRepository);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

procedure TcxStyleRepositoryEditor.SelectItem(AListBox: TListBox; AItem: TPersistent);
begin
  with AListBox do
    ItemIndex := Items.IndexOfObject(AItem);
  ListBoxClearSelection(AListBox);
  if Component <> nil then
    if AItem <> nil then
      Designer.SelectComponent(AItem)
    else
      Designer.SelectComponent(Component);
end;

procedure TcxStyleRepositoryEditor.UpdateButtons;
begin
  btStyleDelete.Enabled := lbStyles.SelCount <> 0;
  miStyleDelete.Enabled := btStyleDelete.Enabled;
  miStyleSelectAll.Enabled := btStyleDelete.Enabled;
  btStyleSheetDelete.Enabled := lbStyleSheets.SelCount <> 0;
  miStyleSheetDelete.Enabled := btStyleSheetDelete.Enabled;
  miStyleSheetSelectAll.Enabled := btStyleSheetDelete.Enabled;
  btnStyleSheetsSave.Enabled := btStyleSheetDelete.Enabled;
  btnStyleSheetEdit.Enabled := lbStyleSheets.SelCount = 1;
  imStyleSheetEdit.Enabled := btnStyleSheetEdit.Enabled;
end;

function TcxStyleRepositoryEditor.FindItemToSelect(AListBox: TListBox): TObject;
var
  I: Integer;
begin
  Result := nil;
  with AListBox do
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

procedure TcxStyleRepositoryEditor.UpdateItems;
var
  I, AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
begin
  ListBoxSaveSelection(lbStyles, ASelection, AItemIndex, ATopIndex);
  try
    lbStyles.Items.Clear;
    for I := 0 to StyleRepository.Count - 1 do
      lbStyles.Items.AddObject(StyleRepository.Items[I].Name, StyleRepository.Items[I]);
  finally
    ListBoxRestoreSelection(lbStyles, ASelection, AItemIndex, ATopIndex);
  end;
  ListBoxSaveSelection(lbStyleSheets, ASelection, AItemIndex, ATopIndex);
  try
    lbStyleSheets.Items.Clear;
    for I := 0 to StyleRepository. StyleSheetCount - 1 do
      lbStyleSheets.Items.AddObject(StyleRepository.StyleSheets[I].Name, StyleRepository.StyleSheets[I]);
  finally
    ListBoxRestoreSelection(lbStyleSheets, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure TcxStyleRepositoryEditor.DeleteSelectedComponents(AListBox: TListBox);
var
  I: Integer;
  AItem: TPersistent;
begin
  if AListBox.SelCount > 0 then
  begin
    AItem := FindItemToSelect(AListBox) as TPersistent;
    for I := 0 to AListBox.Items.Count - 1 do
      if AListBox.Selected[I] then
        AListBox.Items.Objects[I].Free;
    UpdateItems;
    SelectItem(AListBox, AItem);
    UpdateButtons;
    UpdateDesigner(nil);
  end;
end;

procedure TcxStyleRepositoryEditor.lbStylesClick(Sender: TObject);
begin
  ListBoxSelectComponents(lbStyles);
end;

procedure TcxStyleRepositoryEditor.lbStyleSheetsClick(Sender: TObject);
begin
  ListBoxSelectComponents(lbStyleSheets);
end;

procedure TcxStyleRepositoryEditor.btStyleDeleteClick(Sender: TObject);
begin
  DeleteSelectedComponents(lbStyles);
end;

procedure TcxStyleRepositoryEditor.btStyleSheetDeleteClick(
  Sender: TObject);
begin
  DeleteSelectedComponents(lbStyleSheets);
end;

procedure TcxStyleRepositoryEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetSelectionList(AList);
    ListBoxSyncSelection(lbStyles, AList);
    ListBoxSyncSelection(lbStyleSheets, AList);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

procedure TcxStyleRepositoryEditor.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TcxStyleRepositoryEditor.btStyleAddClick(Sender: TObject);
var
  AItem: TcxCustomStyle;
begin
  AItem := StyleRepository.CreateItemEx(TcxStyle, StyleRepository.Owner);
  AItem.Name := CreateUniqueName(Component.Owner, nil, AItem, '', '');
  UpdateItems;
  SelectItem(lbStyles, AItem);
  UpdateButtons;
  UpdateDesigner(nil);
end;

procedure TcxStyleRepositoryEditor.btStyleSheetAddClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := btStyleAdd.Left;
  pt.Y := btStyleAdd.Top + btStyleAdd.Height;
  pt := btStyleAdd.Parent.ClientToScreen(pt);
  pmAddStyleSheet.Popup(pt.X, pt.Y);
end;

procedure TcxStyleRepositoryEditor.miStyleSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(lbStyles);
  lbStylesClick(nil);
end;

procedure TcxStyleRepositoryEditor.miStyleSheetSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(lbStyleSheets);
  lbStyleSheetsClick(nil);
end;

procedure TcxStyleRepositoryEditor.btnStyleSheetEditClick(Sender: TObject);
begin
  if ShowcxStyleSheetEditor(TcxCustomStyleSheet(
    lbStyleSheets.Items.Objects[lbStyleSheets.ItemIndex]), DoStyleGetName) then
    Designer.Modified;
end;

procedure TcxStyleRepositoryEditor.btnStyleSheetsSaveClick(
  Sender: TObject);
var
  I: Integer;
  AList: TList;
begin
  if SaveDialog.Execute then
  begin
    AList := TList.Create;
    try
      for I := 0 to lbStyleSheets.Items.Count - 1 do
        if lbStyleSheets.Selected[I] then
          AList.Add(TcxCustomStyleSheet(lbStyleSheets.Items.Objects[I]));
    SaveStyleSheetsToIniFile(SaveDialog.FileName, AList);
    finally
      AList.Free;
    end;
  end;

end;

procedure TcxStyleRepositoryEditor.btnStyleSheetsLoadClick(
  Sender: TObject);
var
  AStyleSheetList: TList;
begin
  if not OpenDialog.Execute then exit;
  AStyleSheetList := TList.Create;
  try
    ShowLoadStyleSheetsFromIniFile(OpenDialog.FileName, StyleRepository,
      Component.Owner, AStyleSheetList, DoStyleGetName);
    AddStyleSheets(AStyleSheetList);
  finally
    AStyleSheetList.Free;
  end;
end;

procedure TcxStyleRepositoryEditor.btnStyleSheetsPredefineClick(
  Sender: TObject);
var
  AStyleSheetList: TList;
begin
  AStyleSheetList := TList.Create;
  try
    ShowLoadStyleSheetsFromPreDefineStyles(StyleRepository, Component.Owner,
                AStyleSheetList, DoStyleGetName);
    AddStyleSheets(AStyleSheetList);
  finally
    AStyleSheetList.Free;
  end;
end;

end.
