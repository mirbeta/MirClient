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

unit cxGridWizardDBViewsSelectItemsForDisplayPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, DB, ComCtrls,
  dxCore, cxGraphics, cxClasses, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutContainer, cxContainer, cxEdit,
  cxListBox, StdCtrls, cxButtons, dxLayoutControl, cxGridWizardCustomPage, cxGridWizardStrs, dxLayoutLookAndFeels,
  cxCheckListBox, cxListView, cxGridDBDataDefinitions, cxDB, cxDBData, cxCheckBox;

type
  { TcxGridWizardDBViewsSelectItemsForDisplayPageFrame }

  TcxGridWizardDBViewsSelectItemsForDisplayPageFrame = class(TcxGridWizardCustomPageFrame)
    btnAddAllFields: TcxButton;
    btnAddField: TcxButton;
    btnDeleteAllFields: TcxButton;
    btnDeleteField: TcxButton;
    btnMoveFieldDown: TcxButton;
    btnMoveFieldUp: TcxButton;
    lciAddAllFields: TdxLayoutItem;
    lciAddField: TdxLayoutItem;
    lciDataSetFields: TdxLayoutItem;
    lciDeleteAllFields: TdxLayoutItem;
    lciDeleteField: TdxLayoutItem;
    lciGridViewFields: TdxLayoutItem;
    lciMoveFieldDown: TdxLayoutItem;
    lciMoveFieldUp: TdxLayoutItem;
    lcUsedFieldsGroup1: TdxLayoutGroup;
    lcUsedFieldsGroup2: TdxLayoutGroup;
    lvDataSetFields: TcxCheckListBox;
    lvGridViewFields: TcxCheckListBox;
    pupmDataSetFieldsCustomization: TPopupMenu;
    pupmGridViewFieldsCustomization: TPopupMenu;
    pupmitDataSetAddAll: TMenuItem;
    pupmitDataSetAddSelected: TMenuItem;
    pupmitDataSetDeselectAll: TMenuItem;
    pupmitDataSetSelectAll: TMenuItem;
    pupmitDataSetSeparator: TMenuItem;
    pupmitGridViewCheckSelected: TMenuItem;
    pupmitGridViewDeleteAll: TMenuItem;
    pupmitGridViewDeleteSelected: TMenuItem;
    pupmitGridViewDeselectAll: TMenuItem;
    pupmitGridViewMoveDown: TMenuItem;
    pupmitGridViewMoveUp: TMenuItem;
    pupmitGridViewSelectAll: TMenuItem;
    pupmitGRidViewSeparator1: TMenuItem;
    pupmitGRidViewSeparator2: TMenuItem;
    pupmitGRidViewSeparator3: TMenuItem;
    pupmitGridViewUncheckSelected: TMenuItem;
    procedure btnAddAllFieldsClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure btnDeleteAllFieldsClick(Sender: TObject);
    procedure btnDeleteFieldClick(Sender: TObject);
    procedure btnMoveFieldDownClick(Sender: TObject);
    procedure btnMoveFieldUpClick(Sender: TObject);
    procedure lvDataSetFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvDataSetFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lvGridViewFieldsClick(Sender: TObject);
    procedure lvGridViewFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvGridViewFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure pupmitDataSetDeselectAllClick(Sender: TObject);
    procedure pupmitDataSetSelectAllClick(Sender: TObject);
    procedure pupmitGridViewCheckSelectedClick(Sender: TObject);
    procedure pupmitGridViewDeselectAllClick(Sender: TObject);
    procedure pupmitGridViewSelectAllClick(Sender: TObject);
    procedure pupmitGridViewUncheckSelectedClick(Sender: TObject);
  private
    FGridViewChanged: Boolean;

    procedure ChangeCheckStateForSelected(AList: TcxCheckListBox; ACheckState: Boolean);
    procedure CheckMissingFields;
    procedure MoveSelected(ASource, ATarget: TcxCheckListBox);
    procedure PopulateDataSetFields;
    procedure PopulateGridViewFields;
    procedure ReorderDataSetFields;
    procedure SwapItems(AList: TcxCheckListBox; AIndex1, AIndex2: Integer);
    procedure UpdateControlsState;
  protected
    function GetDataSetFields: TFields;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    property DataSetFields: TFields read GetDataSetFields;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridWizardCustomHelper, Math, cxGeometry;

{$R *.dfm}

function GetSelectedCount(AList: TcxCheckListBox): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AList.Count - 1 do
  begin
    if AList.Selected[I] then
      Inc(Result);
  end;
end;

procedure SelectAll(AList: TcxCheckListBox);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList.Selected[I] := True;
end;

procedure SelectNone(AList: TcxCheckListBox);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList.Selected[I] := False;
end;

{ TcxGridWizardDBViewsSelectItemsForDisplayPageFrame }

constructor TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lvDataSetFields.InnerCheckListBox.MultiSelect := True;
  lvGridViewFields.InnerCheckListBox.MultiSelect := True;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.ApplyLocalization;
begin
  lciDataSetFields.Caption := cxGetResourceString(@scxgwSelectItemsForDisplayDataSetFields);
  lciGridViewFields.Caption := cxGetResourceString(@scxgwSelectItemsForDisplayGridViewFields);

  pupmitDataSetAddAll.Caption := cxGetResourceString(@scxgwCommonAddAll);
  pupmitDataSetAddSelected.Caption := cxGetResourceString(@scxgwCommonAddSelected);
  pupmitDataSetDeselectAll.Caption := cxGetResourceString(@scxgwCommonDeselectAll);
  pupmitDataSetSelectAll.Caption := cxGetResourceString(@scxgwCommonSelectAll);
  pupmitGridViewDeleteAll.Caption := cxGetResourceString(@scxgwCommonDeleteAll);
  pupmitGridViewDeleteSelected.Caption := cxGetResourceString(@scxgwCommonDeleteSelected);
  pupmitGridViewDeselectAll.Caption := cxGetResourceString(@scxgwCommonDeselectAll);
  pupmitGridViewSelectAll.Caption := cxGetResourceString(@scxgwCommonSelectAll);
  pupmitGridViewMoveDown.Caption := cxGetResourceString(@scxgwCommonMoveSelectedDown);
  pupmitGridViewMoveUp.Caption := cxGetResourceString(@scxgwCommonMoveSelectedUp);
  pupmitGridViewCheckSelected.Caption := cxGetResourceString(@scxgwCommonCheckSelected);
  pupmitGridViewUncheckSelected.Caption := cxGetResourceString(@scxgwCommonUncheckSelected);
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.ApplySettings;
var
  I, AItemIndex: Integer;
  AFieldName: string;
  AUnboundIndexes: array of Integer;
  ACount: Integer;
begin
  if not FGridViewChanged then
    Exit;

  ACount := 0;
  for I := 0 to Helper.ItemsCount - 1 do
    if Helper.ItemFieldName[I] = '' then
    begin
      Inc(ACount);
      SetLength(AUnboundIndexes, ACount);
      AUnboundIndexes[ACount - 1] := I;
    end;

  for I := 0 to lvDataSetFields.Items.Count - 1 do
  begin
    AFieldName := lvDataSetFields.Items[I].Text;
    if Helper.GetItemIndexByFieldName(AFieldName) >= 0 then
      Helper.DeleteItem(AFieldName);
  end;

  for I := 0 to lvGridViewFields.Items.Count - 1 do
  begin
    AFieldName := lvGridViewFields.Items[I].Text;
    AItemIndex := Helper.GetItemIndexByFieldName(AFieldName);
    if AItemIndex < 0 then
    begin
      Helper.AddItem(AFieldName);
      Helper.ChangeItemIndex(Helper.ItemsCount - 1, I);
    end
    else
      if AItemIndex <> I then
        Helper.ChangeItemIndex(AItemIndex, I);
    Helper.ItemVisible[I] := lvGridViewFields.Items[I].Checked;
  end;

  for I := 0 to ACount - 1 do
    Helper.ChangeItemIndex(lvGridViewFields.Items.Count + I, AUnboundIndexes[I]);
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.LoadSettings;
begin
  CheckMissingFields;
  PopulateDataSetFields;
  PopulateGridViewFields;
  UpdateControlsState;
  FGridViewChanged := False;
end;

function TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.GetDataSetFields: TFields;
var
  ADataControllerIntf: IcxGridWizardHelperDBDataControllerSupport;
  ADataSetIntf: IcxGridWizardHelperDataSetFieldsSupport;
begin
  if Supports(Helper, IcxGridWizardHelperDataSetFieldsSupport, ADataSetIntf) then
    Result := ADataSetIntf.GetDataSetFields
  else
    if Supports(Helper, IcxGridWizardHelperDBDataControllerSupport, ADataControllerIntf) then
      Result := ADataControllerIntf.GetDataController.DataSet.Fields
    else
      raise Exception.CreateFmt('The "%s" is not supported by the "%s"', [TFields.ClassName, Helper.ClassName]);
end;

function TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSelectItemsForDisplayPageDescription);
end;

function TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSelectItemsForDisplayPageTitle);
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.ChangeCheckStateForSelected(AList: TcxCheckListBox; ACheckState: Boolean);
var
  I: Integer;
begin
  AList.Items.BeginUpdate;
  try
    for I := 0 to AList.Items.Count - 1 do
      if AList.Selected[I] then
        lvGridViewFields.Items[I].Checked := ACheckState;
  finally
    AList.Items.EndUpdate;
    FGridViewChanged := True;
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.CheckMissingFields;
var
  I: Integer;
  AFieldName: string;
begin
  for I := Helper.ItemsCount - 1 downto 0 do
  begin
    AFieldName := Helper.ItemFieldName[I];
    if (FindField(DataSetFields, AFieldName) = nil) and (AFieldName <> '') then
      Helper.DeleteItem(I);
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.MoveSelected(ASource, ATarget: TcxCheckListBox);
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  if GetSelectedCount(ASource) > 0 then
  begin
    ATarget.Items.BeginUpdate;
    try
      for I := 0 to ASource.Items.Count - 1 do
        if ASource.Selected[I] then
        begin
          AItem := ATarget.Items.Add;
          AItem.Text := ASource.Items[I].Text;
          AItem.Checked := True;
        end;
    finally
      ATarget.Items.EndUpdate;
    end;

    ASource.Items.BeginUpdate;
    try
      for I := ASource.Items.Count - 1 downto 0 do
      begin
        if ASource.Selected[I] then
          ASource.Items.Delete(I);
      end;
    finally
      ASource.Items.EndUpdate;
      if ASource.Count > 0 then
        ASource.Selected[0] := True
      else
        ASource.ItemIndex := -1;
    end;

    FGridViewChanged := True;
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.PopulateDataSetFields;
var
  I: Integer;
  AFieldName: string;
begin
  lvDataSetFields.Items.Clear;
  for I := 0 to DataSetFields.Count - 1 do
  begin
    AFieldName := DataSetFields[I].FieldName;
    if Helper.GetItemIndexByFieldName(AFieldName) = -1 then
      lvDataSetFields.AddItem(AFieldName);
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.PopulateGridViewFields;
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  lvGridViewFields.Items.Clear;
  for I := 0 to Helper.ItemsCount - 1 do
    if Helper.ItemFieldName[I] <> '' then
    begin
      AItem := lvGridViewFields.Items.Add;
      AItem.Text := Helper.ItemFieldName[I];
      AItem.Checked := Helper.ItemVisible[I];
    end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.ReorderDataSetFields;
var
  AFieldName: string;
  AStringList: TStringList;
  I, J: Integer;
begin
  AStringList := TStringList.Create;
  try
    for I := 0 to lvDataSetFields.Items.Count - 1 do
      AStringList.Add(lvDataSetFields.Items[I].Text);
    lvDataSetFields.Clear;
    for I := 0 to DataSetFields.Count - 1 do
    begin
      AFieldName := DataSetFields[I].FieldName;
      for J := 0 to AStringList.Count - 1 do
        if SameText(AStringList.Strings[J], AFieldName) then
        begin
          lvDataSetFields.Items.Add.Text := AFieldName;
          Break;
        end;
    end;
  finally
    AStringList.Free;
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.SwapItems(AList: TcxCheckListBox; AIndex1, AIndex2: Integer);
var
  ATempChecked: Boolean;
  ATempObject: TObject;
  ATempSelected: Boolean;
  ATempText: TCaption;
begin
  ATempChecked := AList.Items[AIndex1].Checked;
  ATempObject := AList.Items[AIndex1].ItemObject;
  ATempSelected := AList.Selected[AIndex1];
  ATempText := AList.Items[AIndex1].Text;

  AList.Items[AIndex1].Text := AList.Items[AIndex2].Text;
  AList.Items[AIndex1].ItemObject := AList.Items[AIndex2].ItemObject;
  AList.Items[AIndex1].Checked := AList.Items[AIndex2].Checked;
  AList.Selected[AIndex1] := AList.Selected[AIndex2];

  AList.Items[AIndex2].Text := ATempText;
  AList.Items[AIndex2].ItemObject := ATempObject;
  AList.Items[AIndex2].Checked := ATempChecked;
  AList.Selected[AIndex2] := ATempSelected;

  FGridViewChanged := True;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.UpdateControlsState;
var
  ADataSetFieldsSelCount: Integer;
  AGridViewFieldsSelCount: Integer;
begin
  ADataSetFieldsSelCount := GetSelectedCount(lvDataSetFields);
  AGridViewFieldsSelCount := GetSelectedCount(lvGridViewFields);

  lciAddField.Enabled := ADataSetFieldsSelCount > 0;
  lciAddAllFields.Enabled := lvDataSetFields.Count > 0;
  lciDeleteField.Enabled := AGridViewFieldsSelCount > 0;
  lciDeleteAllFields.Enabled := lvGridViewFields.Count > 0;
  lciMoveFieldUp.Enabled := (AGridViewFieldsSelCount > 0) and not lvGridViewFields.Selected[0];
  lciMoveFieldDown.Enabled := (AGridViewFieldsSelCount > 0) and not lvGridViewFields.Selected[lvGridViewFields.Count - 1];

  pupmitDataSetAddSelected.Enabled := lciAddField.Enabled;
  pupmitDataSetAddAll.Enabled := lciAddAllFields.Enabled;
  pupmitDataSetDeselectAll.Enabled := ADataSetFieldsSelCount > 0;
  pupmitDataSetSelectAll.Enabled := ADataSetFieldsSelCount < lvDataSetFields.Count;

  pupmitGridViewDeleteSelected.Enabled := lciDeleteField.Enabled;
  pupmitGridViewDeleteAll.Enabled := lciDeleteAllFields.Enabled;
  pupmitGridViewCheckSelected.Enabled := AGridViewFieldsSelCount > 0;
  pupmitGridViewUncheckSelected.Enabled := AGridViewFieldsSelCount > 0;
  pupmitGridViewMoveUp.Enabled := lciMoveFieldUp.Enabled;
  pupmitGridViewMoveDown.Enabled := lciMoveFieldDown.Enabled;
  pupmitGridViewSelectAll.Enabled := AGridViewFieldsSelCount < lvGridViewFields.Count;
  pupmitGridViewDeselectAll.Enabled := AGridViewFieldsSelCount > 0;
end;

{ Events }

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnAddAllFieldsClick(Sender: TObject);
begin
  SelectAll(lvDataSetFields);
  MoveSelected(lvDataSetFields, lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnAddFieldClick(Sender: TObject);
begin
  MoveSelected(lvDataSetFields, lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnDeleteAllFieldsClick(Sender: TObject);
begin
  SelectAll(lvGridViewFields);
  MoveSelected(lvGridViewFields, lvDataSetFields);
  ReorderDataSetFields;
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnDeleteFieldClick(Sender: TObject);
begin
  MoveSelected(lvGridViewFields, lvDataSetFields);
  ReorderDataSetFields;
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnMoveFieldDownClick(Sender: TObject);
var
  I: Integer;
begin
  if (lvGridViewFields.Count > 0) and not lvGridViewFields.Selected[lvGridViewFields.Count - 1] then
  begin
    for I := lvGridViewFields.Items.Count - 2 downto 0 do
      if lvGridViewFields.Selected[I] then
        SwapItems(lvGridViewFields, I, I + 1);
  end;
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.btnMoveFieldUpClick(Sender: TObject);
var
  I: Integer;
begin
  if (lvGridViewFields.Count > 0) and not lvGridViewFields.Selected[0] then
  begin
    for I := 1 to lvGridViewFields.Items.Count - 1 do
      if lvGridViewFields.Selected[I] then
        SwapItems(lvGridViewFields, I, I - 1);
  end;
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.lvDataSetFieldsDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source as TcxDragControlObject).Control = lvGridViewFields;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.lvDataSetFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target = lvGridViewFields.InnerCheckListBox then
  begin
    MoveSelected(lvDataSetFields, lvGridViewFields);
    UpdateControlsState;
  end;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.lvGridViewFieldsClick(Sender: TObject);
begin
  UpdateControlsState;
  FGridViewChanged := True;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.lvGridViewFieldsDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ADragObject: TcxDragControlObject;
begin
  ADragObject := Source as TcxDragControlObject;
  Accept := (ADragObject.Control = lvDataSetFields) or (ADragObject.Control = lvGridViewFields);
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.lvGridViewFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  ACount: Integer;
  ASelCount: Integer;
  ATargetIndex: Integer;
  I: Integer;
begin
  if Target = lvDataSetFields.InnerCheckListBox then
    MoveSelected(lvGridViewFields, lvDataSetFields)
  else
    if Target = lvGridViewFields.InnerCheckListBox then
    begin
      ATargetIndex := lvGridViewFields.ItemAtPos(Point(X, Y), True);
      if ATargetIndex >= 0 then
      begin
        lvGridViewFields.Items.BeginUpdate;
        try
          ACount := lvGridViewFields.Items.Count;
          ASelCount := GetSelectedCount(lvGridViewFields);
          for I := 0 to ACount - 1 do
          begin
            if lvGridViewFields.Selected[I] then
              lvGridViewFields.Items.Add.Assign(lvGridViewFields.Items[I]);
          end;

          for I := ACount - 1 downto 0 do
          begin
            if lvGridViewFields.Selected[I] then
              lvGridViewFields.Items.Delete(I);
          end;

          if Y > cxRectCenter(lvGridViewFields.ItemRect(ATargetIndex)).Y then
            Inc(ATargetIndex);

          for I := ATargetIndex to (ACount - ASelCount) - 1 do
            lvGridViewFields.Items.Add.Assign(lvGridViewFields.Items[I]);

          for I := (ACount - ASelCount) - 1 downto ATargetIndex do
            lvGridViewFields.Items.Delete(I);
        finally
          lvGridViewFields.Items.EndUpdate;
        end;

        ATargetIndex := Min(ATargetIndex, lvGridViewFields.Count - ASelCount);
        lvGridViewFields.ItemIndex := ATargetIndex;
        for I := 0 to ASelCount - 1 do
          lvGridViewFields.Selected[ATargetIndex + I] := True;

        FGridViewChanged := True;
      end;
    end;

  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitDataSetDeselectAllClick(Sender: TObject);
begin
  SelectNone(lvDataSetFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitDataSetSelectAllClick(Sender: TObject);
begin
  SelectAll(lvDataSetFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitGridViewCheckSelectedClick(Sender: TObject);
begin
  ChangeCheckStateForSelected(lvGridViewFields, True);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitGridViewDeselectAllClick(Sender: TObject);
begin
  SelectNone(lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitGridViewSelectAllClick(Sender: TObject);
begin
  SelectAll(lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardDBViewsSelectItemsForDisplayPageFrame.pupmitGridViewUncheckSelectedClick(Sender: TObject);
begin
  ChangeCheckStateForSelected(lvGridViewFields, False);
  UpdateControlsState;
end;

end.
