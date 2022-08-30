{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridEditor;

{$I cxVer.inc}

interface

uses
  DesignIntf, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxDesignWindows, cxVGrid, StdCtrls, ComCtrls, ExtCtrls,
  cxLookAndFeelPainters, cxButtons, cxDBVGrid, Menus, cxGraphics, cxLookAndFeels, cxControls, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, cxContainer, cxEdit, cxListBox, dxLayoutLookAndFeels;

type
  TcxVerticalGridEditor = class(TcxDesignFormEditor)
    btCategory: TcxButton;
    btEditor: TcxButton;
    btClose: TcxButton;
    btMultiEditor: TcxButton;
    btDelete: TcxButton;
    btClear: TcxButton;
    btCreateAll: TcxButton;
    PopupMenu: TPopupMenu;
    miEditor: TMenuItem;
    miCategory: TMenuItem;
    miMultieditor: TMenuItem;
    N1: TMenuItem;
    miDelete: TMenuItem;
    miClearAll: TMenuItem;
    N2: TMenuItem;
    miSelectAll: TMenuItem;
    btLayoutEditor: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    liCreateAll: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    lbRows: TcxListBox;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure btCloseClick(Sender: TObject);
    procedure lbRowsClick(Sender: TObject);
    procedure btCategoryClick(Sender: TObject);
    procedure btEditorClick(Sender: TObject);
    procedure btMultiEditorClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCreateAllClick(Sender: TObject);
    procedure miEditorClick(Sender: TObject);
    procedure miCategoryClick(Sender: TObject);
    procedure miMultieditorClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miClearAllClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure btLayoutEditorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLocked: Boolean;
    procedure AddRow(ARowClass: TcxCustomRowClass);
    function GetVerticalGrid: TcxCustomVerticalGrid;
    procedure SelectItem(AItem: Pointer);
    procedure UpdateButtons;
    procedure UpdateDesigner(Sender: TObject);
    procedure UpdateItems;
  protected
    procedure InitFormEditor; override;
  public
    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  end;

procedure ShowVerticalGridEditor(ADesigner: IDesigner; AVerticalGrid: TcxCustomVerticalGrid);

implementation

uses
  Types, SysUtils, cxDBData, cxVGridLayoutEditor;

{$R *.dfm}

const
  SSubStr = 'Tcx';
  SizeStore: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);

procedure ShowVerticalGridEditor(ADesigner: IDesigner;
  AVerticalGrid: TcxCustomVerticalGrid);
begin
  ShowFormEditorClass(ADesigner, AVerticalGrid, TcxVerticalGridEditor);
end;

procedure TcxVerticalGridEditor.btCategoryClick(Sender: TObject);
begin
  AddRow(TcxCategoryRow);
end;

procedure TcxVerticalGridEditor.btEditorClick(Sender: TObject);
var
  AIntf: IcxVGridDesignerRows;
begin
  if Supports(TObject(VerticalGrid), IcxVGridDesignerRows, AIntf) then
    AddRow(AIntf.GetEditorRowClass);
end;

procedure TcxVerticalGridEditor.btMultiEditorClick(Sender: TObject);
var
  AIntf: IcxVGridDesignerRows;
begin
  if Supports(TObject(VerticalGrid), IcxVGridDesignerRows, AIntf) then
    AddRow(AIntf.GetMultiEditorRowClass);
end;

procedure TcxVerticalGridEditor.btDeleteClick(Sender: TObject);

  function FindItemToSelect: Pointer;
  var
    I: Integer;
  begin
    Result := nil;
    with lbRows do
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
  AItem: Pointer;
  ARow: TcxCustomRow;
  I, J: Integer;
begin
  if lbRows.SelCount > 0 then
  begin
    FLocked := True;
    AItem := FindItemToSelect;
    VerticalGrid.BeginUpdate;
    try
      for I := lbRows.Items.Count - 1 downto 0 do
        if lbRows.Selected[I] then
        begin
          ARow := TcxCustomRow(lbRows.Items.Objects[I]);
          if not CanDeleteComponent(ARow) then
          begin
            AItem := ARow;
            continue;
          end;
          J := ARow.Index;
          while ARow.Count > 0 do
            with ARow.Rows[0] do
            begin
              Parent := ARow.Parent;
              Index := J;
              Inc(J);
            end;
          ARow.Free;
        end;
    finally
      VerticalGrid.EndUpdate;
      FLocked := False;
    end;
    if lbRows.CanFocus then lbRows.SetFocus;
    UpdateItems;
    SelectItem(AItem);
    UpdateDesigner(nil);
    lbRowsClick(nil);
  end;
end;

procedure TcxVerticalGridEditor.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TcxVerticalGridEditor.lbRowsClick(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to lbRows.Items.Count - 1 do
      if lbRows.Selected[I] then
        AList.Add(lbRows.Items.Objects[I]);
    SelectComponents(AList, VerticalGrid);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.AddRow(ARowClass: TcxCustomRowClass);
var
  ARow: TcxCustomRow;
begin
  if ARowClass <> nil then
  begin
    ARow := VerticalGrid.Add(ARowClass);
    ARow.Name := CreateUniqueName(VerticalGrid.Owner, VerticalGrid, ARow, 'Tcx', '');
    UpdateItems;
    SelectItem(ARow);
    UpdateButtons;
    UpdateDesigner(nil);
  end;
end;

function TcxVerticalGridEditor.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Component as TcxCustomVerticalGrid;
end;

procedure TcxVerticalGridEditor.SelectItem(AItem: Pointer);
begin
  with lbRows do
    ItemIndex := Items.IndexOfObject(AItem);
  ListBoxClearSelection(lbRows.InnerListBox);
  if Component <> nil then
    if (AItem <> nil) and (lbRows.ItemIndex >= 0) then
      Designer.SelectComponent(TPersistent(AItem))
    else
      Designer.SelectComponent(Component)
end;

procedure TcxVerticalGridEditor.UpdateButtons;
begin
  btDelete.Enabled := lbRows.SelCount <> 0;
  miDelete.Enabled := btDelete.Enabled;
  miSelectAll.Enabled := lbRows.Items.Count > 0;
  btClear.Enabled := miSelectAll.Enabled;
  miClearAll.Enabled := miSelectAll.Enabled;
  btLayoutEditor.Enabled := miSelectAll.Enabled;
  liCreateAll.Visible := VerticalGrid is TcxDBVerticalGrid;
  if liCreateAll.Visible then
    liCreateAll.Enabled := TcxDBVerticalGrid(VerticalGrid).DataController.Dataset <> nil;
end;

procedure TcxVerticalGridEditor.UpdateDesigner(Sender: TObject);
begin
  Designer.Modified;
end;

procedure TcxVerticalGridEditor.UpdateItems;
var
  I, AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
begin
  ListBoxSaveSelection(lbRows.InnerListBox, ASelection, AItemIndex, ATopIndex);
  try
    lbRows.Items.Clear;
    for I := 0 to VerticalGrid.Rows.Count - 1 do
      lbRows.Items.AddObject(VerticalGrid.Rows[I].Name, VerticalGrid.Rows.Items[I]);
  finally
    ListBoxRestoreSelection(lbRows.InnerListBox, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure TcxVerticalGridEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  UpdateItems;
  UpdateSelection;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.DoItemsModified;
begin
  UpdateItems;
end;

procedure TcxVerticalGridEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  if FLocked then Exit;
  AList := TList.Create;
  try
    GetSelectionList(AList);
    ListBoxSyncSelection(lbRows.InnerListBox, AList);
  finally
    AList.Free;
  end;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.btClearClick(Sender: TObject);
begin
  ListBoxSelectAll(lbRows.InnerListBox);
  btDeleteClick(nil);
  UpdateItems;
  UpdateSelection;
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.FormActivate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TcxVerticalGridEditor.btCreateAllClick(Sender: TObject);
begin
  if VerticalGrid is TcxDBVerticalGrid then
  begin
    TcxDBVerticalGrid(VerticalGrid).DataController.CreateAllItems;
    UpdateItems;
    UpdateButtons;
    UpdateDesigner(nil);
  end;
end;

procedure TcxVerticalGridEditor.miEditorClick(Sender: TObject);
begin
  btEditorClick(nil);
end;

procedure TcxVerticalGridEditor.miCategoryClick(Sender: TObject);
begin
  btCategoryClick(nil);
end;

procedure TcxVerticalGridEditor.miMultieditorClick(Sender: TObject);
begin
  btMultiEditorClick(nil);
end;

procedure TcxVerticalGridEditor.miDeleteClick(Sender: TObject);
begin
  btDeleteClick(nil);
end;

procedure TcxVerticalGridEditor.miClearAllClick(Sender: TObject);
begin
  btClearClick(nil);
end;

procedure TcxVerticalGridEditor.miSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(lbRows.InnerListBox);
  UpdateItems;
  UpdateButtons;
  UpdateDesigner(nil);
end;

procedure TcxVerticalGridEditor.btLayoutEditorClick(Sender: TObject);
begin
  ShowVerticalGridLayoutEditor(VerticalGrid);
end;

procedure TcxVerticalGridEditor.FormShow(Sender: TObject);
begin
  if SizeStore.Right <> -1 then
  begin
    Left := SizeStore.Left;
    Top := SizeStore.Top;
    Width := SizeStore.Right;
    Height := SizeStore.Bottom;
  end;
end;

procedure TcxVerticalGridEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SizeStore.Left := Left;
  SizeStore.Top := Top;
  SizeStore.Right := Width;
  SizeStore.Bottom := Height;
end;

end.
