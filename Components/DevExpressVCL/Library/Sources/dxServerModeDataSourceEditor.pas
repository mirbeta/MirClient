{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit dxServerModeDataSourceEditor;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, DesignIntf, cxDesignWindows, dxServerModeData,
  Controls, StdCtrls, cxControls, Menus;

type
  TdxServerModeDataSourceEditor = class(TcxDesignFormEditor)
    lbMain: TcxCustomizeListBox;
    pmMain: TPopupMenu;
    miAdd: TMenuItem;
    miNew: TMenuItem;
    miAddAll: TMenuItem;
    N1: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    miSelectAll: TMenuItem;
    procedure lbMainClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miAddAllClick(Sender: TObject);
    procedure lbMainDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbMainDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbMainEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbMainStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure miNewClick(Sender: TObject);
  private
    FPrevDragIndex: Integer;

    procedure AddFields(All: Boolean);
    procedure CheckFieldDelete;
    procedure CheckFieldAdd;
    function CreateFields(AFieldsList: TListBox): TField;
    function DoCreateField(const AFieldName: string): TField;
    function DoNewField: TField;
    procedure Reindex(AList: TList; ANewIndex: Integer);

    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure Delete;

    function GetDataSet: TDataSet;
    function GetDataSource: TdxServerModeCustomDataSource;
  protected
    procedure InitFormEditor; override;
    procedure UpdateContent; override;
    procedure UpdateMenuState;

    property DataSet: TDataSet read GetDataSet;
  public
    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;

    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
  end;

procedure dxShowServerModeDataSourceEditor(ADesigner: IDesigner; ADataSource: TdxServerModeCustomDataSource);

implementation

{$R *.dfm}

uses
  Forms, DesignConst, DSAdd, cxClasses, DesignWindows, DSDefine;

type
  TdxServerModeCustomDataSourceAccess = class(TdxServerModeCustomDataSource);

procedure dxShowServerModeDataSourceEditor(ADesigner: IDesigner; ADataSource: TdxServerModeCustomDataSource);
begin
  ShowFormEditorClass(ADesigner, ADataSource, TdxServerModeDataSourceEditor);
end;

{ TdxServerModeDataSourceEditor }

procedure TdxServerModeDataSourceEditor.DoItemsModified;
begin
  inherited DoItemsModified;
  UpdateContent;
end;

procedure TdxServerModeDataSourceEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  inherited SelectionsChanged(ASelection);
  if LockCount > 0 then Exit;
  AList := TList.Create;
  try
    GetSelectionList(AList);
    ListBoxSyncSelection(lbMain, AList);
  finally
    AList.Free;
  end;
end;

procedure TdxServerModeDataSourceEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  UpdateContent;
end;

procedure TdxServerModeDataSourceEditor.UpdateContent;
var
  ASelections: TStringList;
  AItemIndex, ATopIndex: Integer;
  I: Integer;
begin
  inherited UpdateContent;
  lbMain.Items.BeginUpdate;
  try
    ListBoxSaveSelection(lbMain, ASelections, AItemIndex, ATopIndex);
    try
      lbMain.Items.Clear;
      for I := 0 to DataSource.Fields.Count - 1 do
        if TdxServerModeCustomDataSourceAccess(DataSource).IsPersistentField(DataSource.Fields[I]) then
          lbMain.Items.AddObject(DataSource.Fields[I].FieldName, DataSource.Fields[I]);
    finally
      ListBoxRestoreSelection(lbMain, ASelections, AItemIndex, ATopIndex);
    end;
  finally
    lbMain.Items.EndUpdate;
  end;
  UpdateMenuState;
end;

procedure TdxServerModeDataSourceEditor.UpdateMenuState;
begin
  miAdd.Enabled := DataSource.IsConnected;
  miAddAll.Enabled := DataSource.IsConnected;
end;

procedure TdxServerModeDataSourceEditor.AddFields(All: Boolean);
var
  AAddFields: TAddFields;
  I: Integer;
  AFieldName: string;
  AField: TField;
begin
  CheckFieldAdd;
  TdxServerModeCustomDataSourceAccess(DataSource).InitializeDataSet;
  AAddFields := TAddFields.Create(Application);
  try
    for I := 0 to DataSet.FieldDefList.Count - 1 do
      with Dataset.FieldDefList[I] do
        if (FieldClass <> nil) and not (faHiddenCol in Attributes) then
        begin
          AFieldName := DataSet.FieldDefList.Strings[I];
          AField := DataSet.FindField(AFieldName);
          if (AField = nil) or
              not TdxServerModeCustomDataSourceAccess(DataSource).IsPersistentField(AField) then
            AAddFields.FieldsList.Items.Add(AFieldName);
        end;
    AAddFields.SelectAll;
    AAddFields.FieldsList.ItemIndex := 0;
    if All or (AAddFields.ShowModal <> mrCancel) then
      CreateFields(AAddFields.FieldsList);
    Designer.Modified;
    UpdateContent;
  finally
    AAddFields.Release;
  end;
end;

procedure TdxServerModeDataSourceEditor.CheckFieldDelete;
var
  I: Integer;
begin
  for I := 0 to lbMain.Items.Count-1 do
    if lbMain.Selected[I] and (csAncestor in TField(lbMain.Items.Objects[I]).ComponentState) then
      raise Exception.CreateRes(@SCantDeleteAncestor);
end;

procedure TdxServerModeDataSourceEditor.CheckFieldAdd;
begin
  if (DataSource <> nil) and (DataSource.Owner <> nil) and
    (csInline in DataSource.Owner.ComponentState) then
      raise Exception.CreateRes(@SCantAddToFrame);
end;

function TdxServerModeDataSourceEditor.CreateFields(AFieldsList: TListBox): TField;
var
  I: Integer;
  AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
  AFields: TStringList;
begin
  Result := nil;
  ListBoxSaveSelection(lbMain, ASelection, AItemIndex, ATopIndex);
  try
    Screen.Cursor := crHourGlass;
    try
      TdxServerModeCustomDataSourceAccess(DataSource).DestroyNonPersistentFields;
      AFields := TStringList.Create;
      try
        for I := 0 to AFieldsList.Items.Count - 1 do
          if AFieldsList.Selected[I] then
            AFields.Add(AFieldsList.Items[I]);
        for I := 0 to AFields.Count - 1 do
          Result := DoCreateField(AFields[I]);
      finally
        AFields.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    ListBoxRestoreSelection(lbMain, ASelection, AItemIndex, ATopIndex);
  end;
end;

function TdxServerModeDataSourceEditor.DoCreateField(const AFieldName: string): TField;
var
  AFieldDef: TFieldDef;
begin
  AFieldDef := DataSet.FieldDefList.FieldByName(AFieldName);
  Result := TdxServerModeCustomDataSourceAccess(DataSource).CreateField(AFieldDef, DataSource.Owner);
end;

function TdxServerModeDataSourceEditor.DoNewField: TField;
var
  ADefineField: TDefineField;
  AColumns: Integer;
begin
  Result := nil;
  ADefineField := TDefineField.Create(Application);
  try
    ADefineField.Designer := Designer;
    ADefineField.DataSet := DataSet;
    AColumns := 3;
    ADefineField.FieldKind.Columns := AColumns;
    if ADefineField.ShowModal = mrOk then
    begin
      Result := ADefineField.Field;
      Result.Name := TdxServerModeCustomDataSourceAccess(DataSource).GetFieldUniqueName(Result);
      Designer.Modified;
    end;
  finally
    ADefineField.Release;
  end;
end;

procedure TdxServerModeDataSourceEditor.Reindex(AList: TList; ANewIndex: Integer);
var
  I: Integer;
begin
  if AList.Count = 0 then
    Exit;
  if TField(AList[0]).Index < ANewIndex then
  begin
    for I := 0 to AList.Count - 1 do
      TField(AList[I]).Index := ANewIndex;
  end
  else
  begin
    for I := AList.Count - 1 downto 0 do
      TField(AList[I]).Index := ANewIndex;
  end;
  UpdateContent;
  Designer.Modified;
end;

procedure TdxServerModeDataSourceEditor.Copy;
var
  I: Integer;
  AComponentList: IDesignerSelections;
begin
  AComponentList := CreateDesignerSelectionList;
  try
    for I := 0 to lbMain.Items.Count - 1 do
      if lbMain.Selected[I] then
        AComponentList.Add(TComponent(lbMain.Items.Objects[I]));
    CopyComponents(DataSource.Owner, AComponentList);
  finally
    DeleteDesignerSelectionList(AComponentList);
  end;
end;

procedure TdxServerModeDataSourceEditor.Cut;
begin
  CheckFieldDelete;
  Copy;
  Delete;
end;

procedure TdxServerModeDataSourceEditor.Paste;
var
  AComponentList: IDesignerSelections;
begin
  CheckFieldAdd;
  AComponentList := CreateDesignerSelectionList;
  try
    lbMain.ClearSelection;
    PasteComponents(DataSource.Owner, DataSet, AComponentList);
    UpdateContent;
  finally
    DeleteDesignerSelectionList(AComponentList);
  end;
end;

procedure TdxServerModeDataSourceEditor.Delete;
begin
  CheckFieldDelete;
  ListBoxDeleteSelection(lbMain, True, True);
  ListBoxApplySelection(lbMain, DataSource);
  Designer.Modified;
end;

function TdxServerModeDataSourceEditor.GetDataSet: TDataSet;
begin
  Result := TdxServerModeCustomDataSourceAccess(DataSource).DataSet;
end;

function TdxServerModeDataSourceEditor.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := TdxServerModeCustomDataSource(inherited Component);
end;

procedure TdxServerModeDataSourceEditor.lbMainClick(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to lbMain.Items.Count - 1 do
      if lbMain.Selected[I] then
        AList.Add(lbMain.Items.Objects[I]);
    SelectComponents(AList, DataSource);
  finally
    AList.Free;
  end;
end;

procedure TdxServerModeDataSourceEditor.miSelectAllClick(
  Sender: TObject);
begin
  lbMain.SelectAll;
end;

procedure TdxServerModeDataSourceEditor.miCutClick(Sender: TObject);
begin
  Cut;
end;

procedure TdxServerModeDataSourceEditor.miCopyClick(Sender: TObject);
begin
  Copy;
end;

procedure TdxServerModeDataSourceEditor.miPasteClick(Sender: TObject);
begin
  Paste;
end;

procedure TdxServerModeDataSourceEditor.miDeleteClick(Sender: TObject);
begin
  Delete;
end;

procedure TdxServerModeDataSourceEditor.miAddClick(Sender: TObject);
begin
  AddFields(False);
end;

procedure TdxServerModeDataSourceEditor.miAddAllClick(Sender: TObject);
begin
  AddFields(True);
end;

procedure TdxServerModeDataSourceEditor.lbMainDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ListBoxDragOver(lbMain, Sender, Source, X, Y, State, Accept, FPrevDragIndex);
end;

procedure TdxServerModeDataSourceEditor.lbMainDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  ListBoxDragDrop(lbMain, Sender, Source, X, Y, FPrevDragIndex, Reindex);
end;

procedure TdxServerModeDataSourceEditor.lbMainEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  ListBoxEndDrag(lbMain, Sender, Target, X, Y, FPrevDragIndex);
end;

procedure TdxServerModeDataSourceEditor.lbMainStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FPrevDragIndex := -1;
end;

procedure TdxServerModeDataSourceEditor.miNewClick(Sender: TObject);
var
  AField: TField;
begin
  CheckFieldAdd;
  AField := DoNewField;
  if AField <> nil then
    SelectComponent(AField);
end;

end.
