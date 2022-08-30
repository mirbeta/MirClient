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

unit cxGridWizardUnboundViewsSelectItemsForDisplayPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, Math,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControlAdapters, ActnList, StdCtrls,
  cxButtons, cxContainer, cxEdit, cxListBox, dxLayoutControl, cxClasses, cxGeometry, cxGridWizardCustomPage,
  cxGridWizardStrs, cxGridWizardCustomHelper, dxLayoutLookAndFeels, cxGridWizardUnboundViewsEditItemDialog;

type
  { TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame }

  TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame = class(TcxGridWizardCustomPageFrame)
    acAdd: TAction;
    acDeleteAll: TAction;
    acDeleteSelected: TAction;
    acEdit: TAction;
    aclUsedItems: TActionList;
    acMoveDown: TAction;
    acMoveUp: TAction;
    Add: TMenuItem;
    btnAdd: TcxButton;
    btnDeleteAll: TcxButton;
    btnDeleteSelected: TcxButton;
    btnEdit: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    Deleteall: TMenuItem;
    Deleteselected: TMenuItem;
    dxLayoutGroup2: TdxLayoutGroup;
    Edit: TMenuItem;
    lbUsedItems: TcxListBox;
    lciAdd: TdxLayoutItem;
    lciDeleteAll: TdxLayoutItem;
    lciDeleteSelected: TdxLayoutItem;
    lciEdit: TdxLayoutItem;
    lciMoveDown: TdxLayoutItem;
    lciMoveUp: TdxLayoutItem;
    lciUsedItems: TdxLayoutItem;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutGroup;
    MoveDown: TMenuItem;
    MoveUp: TMenuItem;
    pupmUsedItems: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteAllExecute(Sender: TObject);
    procedure acDeleteSelectedExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure lbUsedItemsClick(Sender: TObject);
    procedure lbUsedItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbUsedItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    procedure CheckItemsCount;
    procedure PopulateProperties(var AStrings: TStringList);
    procedure ReorderGridViewItems;
    procedure SwapItems(const AIndex1, AIndex2: Integer);
  protected
    function GetAddDialogCaption: string;
    function GetEditDialogCaption: string;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    function IsLayoutView: Boolean;
    function IsTableView: Boolean;
  public
    procedure ApplyLocalization; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridTableView, cxGridLayoutView;

{$R *.dfm}

{ TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame }

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.ApplyLocalization;
begin
  acAdd.Caption := cxGetResourceString(@scxgwCommonAdd);
  acDeleteAll.Caption := cxGetResourceString(@scxgwCommonDeleteAll);
  acDeleteSelected.Caption := cxGetResourceString(@scxgwCommonDeleteSelected);
  acEdit.Caption := cxGetResourceString(@scxgwCommonEdit);
  acMoveDown.Caption := cxGetResourceString(@scxgwCommonMoveDown);
  acMoveUp.Caption := cxGetResourceString(@scxgwCommonMoveUp);
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.LoadSettings;
var
  I: Integer;
begin
  lbUsedItems.Items.Clear;
  for I := 0 to Helper.ItemsCount - 1 do
    lbUsedItems.AddItem(Helper.ItemCaption[I], TObject(I));
  CheckItemsCount;
  acMoveDown.Visible := not IsLayoutView;
  acMoveUp.Visible := not IsLayoutView;
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.GetAddDialogCaption: string;
begin
  if IsLayoutView then
    Result := cxGetResourceString(@scxgsUnboundsSelectItemsForDisplayPageInputQueryCaptionAdd)
  else
    if IsTableView then
      Result := cxGetResourceString(@scxgsUnboundsSelectColumnsForDisplayPageInputQueryCaptionAdd)
    else
      Result := cxGetResourceString(@scxgsUnboundsSelectRowsForDisplayPageInputQueryCaptionAdd);
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.GetEditDialogCaption: string;
begin
  if IsLayoutView then
    Result := cxGetResourceString(@scxgsUnboundsSelectItemsForDisplayPageInputQueryCaptionEdit)
  else
    if IsTableView then
      Result := cxGetResourceString(@scxgsUnboundsSelectColumnsForDisplayPageInputQueryCaptionEdit)
    else
      Result := cxGetResourceString(@scxgsUnboundsSelectRowsForDisplayPageInputQueryCaptionEdit);
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.GetPageDescription: string;
begin
  if IsLayoutView then
    Result := cxGetResourceString(@scxgwUnboundsSelectItemsForDisplayPageDescription)
  else
    if IsTableView then
      Result := cxGetResourceString(@scxgwUnboundsSelectColumnsForDisplayPageDescription)
    else
      Result := cxGetResourceString(@scxgwUnboundsSelectRowsForDisplayPageDescription);
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.GetPageTitle: string;
begin
  if IsLayoutView then
    Result := cxGetResourceString(@scxgwUnboundsSelectItemsForDisplayPageTitle)
  else
    if IsTableView then
      Result := cxGetResourceString(@scxgwUnboundsSelectColumnsForDisplayPageTitle)
    else
      Result := cxGetResourceString(@scxgwUnboundsSelectRowsForDisplayPageTitle);
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.IsLayoutView: Boolean;
begin
  Result := Helper.GridView is TcxGridLayoutView;
end;

function TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.IsTableView: Boolean;
begin
  Result := Helper.GridView is TcxGridTableView;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.CheckItemsCount;
begin
  UpdateOwnerButtonsState;
  acDeleteAll.Enabled := Helper.ItemsCount > 0;
  acDeleteSelected.Enabled := lbUsedItems.SelCount > 0;
  acEdit.Enabled := lbUsedItems.SelCount = 1;
  acMoveDown.Enabled := (lbUsedItems.SelCount > 0) and not lbUsedItems.Selected[lbUsedItems.Count - 1];
  acMoveUp.Enabled := (lbUsedItems.SelCount > 0) and not lbUsedItems.Selected[0];
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.PopulateProperties(var AStrings: TStringList);
var
  I: Integer;
  ADesc: string;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    AStrings.AddObject(cxGetResourceString(@scxgwCommonNoneSelected), nil);
    for I := 0 to GetRegisteredEditProperties.Count - 1 do
    begin
      ADesc := GetRegisteredEditProperties.Descriptions[I];
      if ADesc <> '' then
        AStrings.AddObject(ADesc, TObject(GetRegisteredEditProperties.Items[I]));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.ReorderGridViewItems;
var
  I, AOldIndex: Integer;
begin
  for I := 0 to lbUsedItems.Count - 1 do
  begin
    AOldIndex := Integer(lbUsedItems.Items.Objects[I]);
    if I < AOldIndex then
      Helper.ChangeItemIndex(AOldIndex, I);
  end;
  for I := 0 to lbUsedItems.Count - 1 do
    lbUsedItems.Items.Objects[I] := TObject(I);
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.SwapItems(const AIndex1, AIndex2: Integer);
var
  ATempSelected: Boolean;
begin
  ATempSelected := lbUsedItems.Selected[AIndex1];
  lbUsedItems.Items.Exchange(AIndex1, AIndex2);
  lbUsedItems.Selected[AIndex1] := lbUsedItems.Selected[AIndex2];
  lbUsedItems.Selected[AIndex2] := ATempSelected;
end;

{ Events }

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acAddExecute(Sender: TObject);
const
  FormatStr = '%s %d';
var
  ACaption: string;
  AProperties: TStringList;
  APropertyIndex: Integer;
begin
  ACaption := Format(FormatStr, [Helper.GetDefaultItemCaption, Helper.ItemsCount + 1]);
  AProperties := TStringList.Create;
  try
    PopulateProperties(AProperties);
    APropertyIndex := 0;
    if cxgwExecuteEditItemDialog(Self, GetAddDialogCaption, AProperties, ACaption, APropertyIndex) then
    begin
      Helper.AddItem('', ACaption, TcxCustomEditPropertiesClass(AProperties.Objects[APropertyIndex]));
      LoadSettings;
    end;
  finally
    AProperties.Free;
  end;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acDeleteAllExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbUsedItems.Count - 1 downto 0 do
    Helper.DeleteItem(I);
  LoadSettings;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acDeleteSelectedExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbUsedItems.Count - 1 downto 0 do
    if lbUsedItems.Selected[I] then
      Helper.DeleteItem(I);
  LoadSettings;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acEditExecute(Sender: TObject);
var
  ANewCaption: string;
  ANewPropertyIndex: Integer;
  AProperties: TStringList;
begin
  ANewCaption := lbUsedItems.Items[lbUsedItems.ItemIndex];
  AProperties := TStringList.Create;
  try
    PopulateProperties(AProperties);
    ANewPropertyIndex := AProperties.IndexOfObject(TObject(Helper.ItemProperties[lbUsedItems.ItemIndex]));
    if cxgwExecuteEditItemDialog(Self, GetEditDialogCaption, AProperties, ANewCaption, ANewPropertyIndex) then
    begin
      Helper.ItemCaption[lbUsedItems.ItemIndex] := ANewCaption;
      if ANewPropertyIndex > 0 then
        Helper.ItemProperties[lbUsedItems.ItemIndex] :=
          TcxCustomEditPropertiesClass(GetRegisteredEditProperties.Items[ANewPropertyIndex])
      else
        Helper.ItemProperties[lbUsedItems.ItemIndex] := nil;
      LoadSettings;
    end;
  finally
    AProperties.Free;
  end;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := lbUsedItems.Count - 2 downto 0 do
    if lbUsedItems.Selected[I] then
      SwapItems(I, I + 1);
  ReorderGridViewItems;
  CheckItemsCount;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to lbUsedItems.Count - 1 do
    if lbUsedItems.Selected[I] then
      SwapItems(I, I - 1);
  ReorderGridViewItems;
  CheckItemsCount;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.lbUsedItemsClick(Sender: TObject);
begin
  CheckItemsCount;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.lbUsedItemsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame.lbUsedItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  AAboveTargetIndex: Integer;
  ACount: Integer;
  ASelCount: Integer;
  ATargetIndex: Integer;
  I: Integer;
begin
  ATargetIndex := lbUsedItems.ItemAtPos(Point(X, Y), True);
  if ATargetIndex >= 0 then
  begin
    lbUsedItems.Items.BeginUpdate;
    try
      ACount := lbUsedItems.Items.Count;
      ASelCount := lbUsedItems.SelCount;

      if Y > cxRectCenter(lbUsedItems.ItemRect(ATargetIndex)).Y then
        Inc(ATargetIndex);

      AAboveTargetIndex := 0;
      for I := 0 to ACount - 1 do
        if lbUsedItems.Selected[I] then
        begin
          if I < ATargetIndex then
            Inc(AAboveTargetIndex);
          lbUsedItems.Items.AddObject(lbUsedItems.Items[I], lbUsedItems.Items.Objects[I]);
        end;
      Dec(ATargetIndex, AAboveTargetIndex);

      for I := ACount - 1 downto 0 do
        if lbUsedItems.Selected[I] then
          lbUsedItems.Items.Delete(I);

      for I := ATargetIndex to (ACount - ASelCount) - 1 do
        lbUsedItems.Items.AddObject(lbUsedItems.Items[I], lbUsedItems.Items.Objects[I]);

      for I := (ACount - ASelCount) - 1 downto ATargetIndex do
        lbUsedItems.Items.Delete(I);
    finally
      lbUsedItems.Items.EndUpdate;
    end;
    ATargetIndex := Min(ATargetIndex, lbUsedItems.Count - ASelCount);
    lbUsedItems.ItemIndex := ATargetIndex;
    for I := 0 to ASelCount - 1 do
      lbUsedItems.Selected[ATargetIndex + I] := True;

    ReorderGridViewItems;
  end;
  CheckItemsCount;
end;

end.
