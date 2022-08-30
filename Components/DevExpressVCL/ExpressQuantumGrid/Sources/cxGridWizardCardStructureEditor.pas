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

unit cxGridWizardCardStructureEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ImgList, ActnList,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControlAdapters,
  cxButtons, cxContainer, cxEdit, cxTreeView, dxLayoutControl, dxLayoutLookAndFeels, cxClasses,
  cxGridCustomView, cxGridCustomTableView, cxGridCardView, cxGrid, cxGridWizardStrs;

type
  { TcxGridWizardCardStructureEditorFrame }

  TcxGridWizardCardStructureEditorFrame = class(TFrame)
    btnAddCaptionRow: TcxButton;
    btnAddCategoryRow: TcxButton;
    btnEditRow: TcxButton;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    ilActions: TcxImageList;
    ilRows: TcxImageList;
    lcgActions: TdxLayoutGroup;
    lciAddCaptionRow: TdxLayoutItem;
    lciAddCategoryRow: TdxLayoutItem;
    lciEditRow: TdxLayoutItem;
    lciRows: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    miAddCaptionRow: TMenuItem;
    miAddCategoryRow: TMenuItem;
    miEditRow: TMenuItem;
    miSeparator: TMenuItem;
    pupmActions: TPopupMenu;
    tvRows: TcxTreeView;
    procedure btnAddCaptionRowClick(Sender: TObject);
    procedure btnAddCategoryRowClick(Sender: TObject);
    procedure btnEditRowClick(Sender: TObject);
    procedure tvRowsClick(Sender: TObject);
    procedure tvRowsEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure tvRowsExpanded(Sender: TObject; Node: TTreeNode);
  private
    FActualParentNode: TTreeNode;
    FIsUpdatingExpandedNodes: Boolean;
    FOnFocusedItemChangedValue: TcxGridFocusedItemChangedEvent;
    FOnLayoutChangedValue: TcxGridLayoutChangedEvent;
    FPreviewGrid: TcxCustomGrid;

    function GetPreviewView: TcxGridCardView;
  protected
    procedure AddRowToTreeView(ARow: TcxGridCardViewRow);
    procedure ApplyLocalization;
    function GetDefaultCaptionRowCaption: string;
    function GetDefaultCategoryRowCaption: string;
    function GetNodeByVisibleIndex(const AVisibleIndex: Integer): TTreeNode;
    function GetVisibleIndexByNode(ATreeNode: TTreeNode): Integer;
    procedure PreviewGridLayoutChangedHandler(Sender: TcxCustomGrid; AGridView: TcxCustomGridView);
    procedure PreviewViewFocusedItemChangedHandler(Sender: TcxCustomGridTableView;
      APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
    procedure RefreshContent;
    procedure UpdateButtonsState;
    procedure UpdateExpandedNodes;

    property PreviewView: TcxGridCardView read GetPreviewView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSettings(AGrid: TcxCustomGrid);
  end;

implementation

uses
  cxGridWizardUnboundViewsEditItemDialog, Math;

{$R *.dfm}

{ TcxGridWizardCardStructureEditorFrame }

constructor TcxGridWizardCardStructureEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ApplyLocalization;
end;

destructor TcxGridWizardCardStructureEditorFrame.Destroy;
begin
  PreviewView.OnFocusedItemChanged := FOnFocusedItemChangedValue;
  FPreviewGrid.OnLayoutChanged := FOnLayoutChangedValue;
  inherited Destroy;
end;

procedure TcxGridWizardCardStructureEditorFrame.LoadSettings(AGrid: TcxCustomGrid);
begin
  FPreviewGrid := AGrid;
  FOnLayoutChangedValue := FPreviewGrid.OnLayoutChanged;
  FPreviewGrid.OnLayoutChanged := PreviewGridLayoutChangedHandler;
  FOnFocusedItemChangedValue := PreviewView.OnFocusedItemChanged;
  PreviewView.OnFocusedItemChanged := PreviewViewFocusedItemChangedHandler;
  RefreshContent;
end;

procedure TcxGridWizardCardStructureEditorFrame.AddRowToTreeView(ARow: TcxGridCardViewRow);
const
  RowKindMap: array [TcxGridCardViewRowKind] of Integer = (0, 1, 2);
var
  ANode: TTreeNode;
begin
  if ARow.Kind = rkCategory then
    FActualParentNode := nil;
  ANode := tvRows.Items.AddChild(FActualParentNode, ARow.Caption);
  ANode.ImageIndex := RowKindMap[ARow.Kind];
  ANode.SelectedIndex := ANode.ImageIndex;
  if ARow.Kind = rkCategory then
    FActualParentNode := ANode;
end;

procedure TcxGridWizardCardStructureEditorFrame.ApplyLocalization;
begin
  btnAddCaptionRow.Caption := cxGetResourceString(@scxgwCardViewTreeViewFrameAddCaptionRow);
  btnAddCaptionRow.Hint := cxGetResourceString(@scxgwCardViewTreeViewFrameAddCaptionRowHint);
  miAddCaptionRow.Caption := btnAddCaptionRow.Caption;

  btnAddCategoryRow.Caption := cxGetResourceString(@scxgwCardViewTreeViewFrameAddCategoryRow);
  btnAddCategoryRow.Hint := cxGetResourceString(@scxgwCardViewTreeViewFrameAddCategoryRowHint);
  miAddCategoryRow.Caption := btnAddCategoryRow.Caption;

  btnEditRow.Caption := cxGetResourceString(@scxgwCardViewTreeViewFrameEditRow);
  btnEditRow.Hint := cxGetResourceString(@scxgwCardViewTreeViewFrameEditRowHint);
  miEditRow.Caption := btnEditRow.Caption;
end;

function TcxGridWizardCardStructureEditorFrame.GetDefaultCaptionRowCaption: string;
begin
  Result := cxGetResourceString(@scxgwCardViewTreeViewFrameDefaultCaptionRowCaption);
end;

function TcxGridWizardCardStructureEditorFrame.GetDefaultCategoryRowCaption: string;
begin
  Result := cxGetResourceString(@scxgwCardViewTreeViewFrameDefaultCategoryRowCaption);
end;

function TcxGridWizardCardStructureEditorFrame.GetNodeByVisibleIndex(const AVisibleIndex: Integer): TTreeNode;
var
  I, AIndex: Integer;
begin
  AIndex := -1;
  I := 0;
  while (AIndex <> AVisibleIndex) and (I < tvRows.Items.Count) do
  begin
    if tvRows.Items[I].IsVisible then
      Inc(AIndex);
    Inc(I);
  end;
  if AIndex = AVisibleIndex then
    Result := tvRows.Items[I - 1]
  else
    Result := nil;
end;

function TcxGridWizardCardStructureEditorFrame.GetVisibleIndexByNode(ATreeNode: TTreeNode): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 0;
  while (I < tvRows.Items.Count) and (ATreeNode <> tvRows.Items[I]) do
  begin
    if tvRows.Items[I].IsVisible then
      Inc(Result);
    Inc(I);
  end;
  if ATreeNode <> tvRows.Items[I] then
    Result := -1;
end;

procedure TcxGridWizardCardStructureEditorFrame.PreviewGridLayoutChangedHandler(Sender: TcxCustomGrid;
  AGridView: TcxCustomGridView);
begin
  RefreshContent;
end;

procedure TcxGridWizardCardStructureEditorFrame.PreviewViewFocusedItemChangedHandler(Sender: TcxCustomGridTableView;
  APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
begin
  tvRows.Items[AFocusedItem.VisibleIndex].Selected := True;
end;

procedure TcxGridWizardCardStructureEditorFrame.RefreshContent;
var
  I: Integer;
begin
  tvRows.Items.BeginUpdate;
  try
    FActualParentNode := nil;
    tvRows.Items.Clear;
    if PreviewView <> nil then
      for I := 0 to PreviewView.RowCount - 1 do
        if PreviewView.Rows[I].Visible then
          AddRowToTreeView(PreviewView.Rows[I]);
  finally
    tvRows.Items.EndUpdate;
    UpdateExpandedNodes;
    UpdateButtonsState;
  end;
end;

procedure TcxGridWizardCardStructureEditorFrame.UpdateButtonsState;
begin
  btnEditRow.Enabled := tvRows.SelectionCount > 0;
  miEditRow.Enabled := btnEditRow.Enabled;
end;

procedure TcxGridWizardCardStructureEditorFrame.UpdateExpandedNodes;
var
  I: Integer;
begin
  FIsUpdatingExpandedNodes := True;
  try
    for I := 0 to PreviewView.VisibleRowCount - 1 do
      if PreviewView.VisibleRows[I].Kind = rkCategory then
        GetNodeByVisibleIndex(I).Expanded := PreviewView.VisibleRows[I].Expanded;
  finally
    FIsUpdatingExpandedNodes := False;
  end;
end;

function TcxGridWizardCardStructureEditorFrame.GetPreviewView: TcxGridCardView;
begin
  Result := (FPreviewGrid.ActiveView as TcxGridCardView);
end;

{ Events }

procedure TcxGridWizardCardStructureEditorFrame.btnAddCaptionRowClick(Sender: TObject);
var
  ACaption: string;
  ARow: TcxGridCardViewRow;
begin
  ACaption := GetDefaultCaptionRowCaption;
  if cxgwExecuteEditItemDialog(Self, cxGetResourceString(@scxgwCardViewTreeViewFrameInputQueryCaptionAddCaptionRow), ACaption) then
  begin
    ARow := PreviewView.CreateRow;
    ARow.Caption := ACaption;
    ARow.Kind := rkCaption;
    ARow.Options.ShowData := False;
    RefreshContent;
  end;
end;

procedure TcxGridWizardCardStructureEditorFrame.btnAddCategoryRowClick(Sender: TObject);
var
  ACaption: string;
  ARow: TcxGridCardViewRow;
begin
  ACaption := GetDefaultCategoryRowCaption;
  if cxgwExecuteEditItemDialog(Self, cxGetResourceString(@scxgwCardViewTreeViewFrameInputQueryCaptionAddCategoryRow), ACaption) then
  begin
    ARow := PreviewView.CreateRow;
    ARow.Caption := ACaption;
    ARow.Kind := rkCategory;
    ARow.Options.ShowData := False;
    RefreshContent;
  end;
end;

procedure TcxGridWizardCardStructureEditorFrame.btnEditRowClick(Sender: TObject);
const
  KindMap: array [0..2] of TcxGridCardViewRowKind = (rkData, rkCaption, rkCategory);
  KindIndexMap: array [TcxGridCardViewRowKind] of Integer = (0, 1, 2);

  procedure PopulateKinds(var AStrings: TStringList);
  const
    KindCaptionMap: array [0..2] of string = ('Data', 'Caption', 'Category');
  var
    I: Integer;
  begin
    AStrings.BeginUpdate;
    try
      AStrings.Clear;
      for I := 0 to Length(KindMap) - 1 do
        AStrings.AddObject(KindCaptionMap[I], TObject(KindMap[I]));
    finally
      AStrings.EndUpdate;
    end;
  end;

var
  ACaption: string;
  AKinds: TStringList;
  AKindIndex: Integer;
  ARow: TcxGridCardViewRow;
begin
  ACaption := tvRows.Selected.Text;
  ARow := PreviewView.VisibleRows[GetVisibleIndexByNode(tvRows.Selected)];
  AKindIndex := KindIndexMap[ARow.Kind];
  AKinds := TStringList.Create;
  try
    PopulateKinds(AKinds);
    if cxgwExecuteEditItemDialog(Self,
      cxGetResourceString(@scxgwCardViewTreeViewFrameInputQueryCaptionEditRow),
      AKinds, ACaption, AKindIndex, cxGetResourceString(@scxgwCommonKind)) then
    begin
      ARow.Caption := ACaption;
      ARow.Kind := KindMap[AKindIndex];
      RefreshContent;
    end;
  finally
    AKinds.Free;
  end;
end;

procedure TcxGridWizardCardStructureEditorFrame.tvRowsClick(Sender: TObject);
begin
  if tvRows.Selected.Index >= 0 then
    PreviewView.VisibleRows[GetVisibleIndexByNode(tvRows.Selected)].Focused := True;
  UpdateButtonsState;
end;

procedure TcxGridWizardCardStructureEditorFrame.tvRowsEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
  PreviewView.VisibleRows[GetVisibleIndexByNode(Node)].Caption := S;
  UpdateButtonsState;
end;

procedure TcxGridWizardCardStructureEditorFrame.tvRowsExpanded(Sender: TObject; Node: TTreeNode);
begin
  if not FIsUpdatingExpandedNodes then
    PreviewView.VisibleRows[GetVisibleIndexByNode(Node)].Expanded := Node.Expanded;
end;

end.
