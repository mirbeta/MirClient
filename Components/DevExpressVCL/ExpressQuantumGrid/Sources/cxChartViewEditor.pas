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

unit cxChartViewEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, cxViewEditor, cxGridCustomView, cxGridChartView, ComCtrls, StdCtrls,
  Menus, cxControls, cxPC, cxLookAndFeelPainters, cxButtons, cxGraphics, cxLookAndFeels, dxLayoutContainer,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxContainer, cxEdit, cxListBox;

type
  TcxGridChartItemKind = (ciSeries, ciDataGroup);
  TcxGridChartItemListBoxParams = array[TcxGridChartItemKind] of Integer;

  TcxChartViewEditor = class(TcxViewEditor)
    btnItemAdd: TcxButton;
    btnItemDelete: TcxButton;
    btnItemMoveDown: TcxButton;
    btnItemMoveUp: TcxButton;
    btnItemSelectAll: TcxButton;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    lbItems: TcxListBox;
    miItemAdd: TMenuItem;
    miItemDelete: TMenuItem;
    miItemMoveDown: TMenuItem;
    miItemMoveUp: TMenuItem;
    miItemSelectAll: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    pmItems: TPopupMenu;
    pmItemsAdd: TPopupMenu;
    tcMain: TcxTabControl;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;

    procedure btnItemAddClick(Sender: TObject);
    procedure btnItemAddExClick(Sender: TObject);
    procedure btnItemDeleteClick(Sender: TObject);
    procedure btnItemMoveDownClick(Sender: TObject);
    procedure btnItemMoveUpClick(Sender: TObject);
    procedure btnItemSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure miItemAddExClick(Sender: TObject);
    procedure tcMainChange(Sender: TObject);
    procedure tcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure lbItemsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState);
  private
    FItemPrevDragIndex: Integer;
    FListBoxItemIndex: TcxGridChartItemListBoxParams;
    FListBoxTopIndex: TcxGridChartItemListBoxParams;

    function GetItemClassValue: TcxGridChartItemClass;
    function GetView: TcxGridChartView;
    procedure ReindexItemsProc(AList: TList; ANewIndex: Integer);
  protected
    IsActiveTabChanging: Boolean;

    procedure Loaded; override;

    function GetItem(Index: Integer): TcxGridChartItem; virtual;
    function GetItemClass(AItemKind: TcxGridChartItemKind): TcxGridChartItemClass; virtual;
    function GetItemCount: Integer; virtual;
    function GetItemKind: TcxGridChartItemKind; virtual;
    function GetItemName: string; virtual;
    procedure SetItemKind(Value: TcxGridChartItemKind); virtual;

    function AddItem: TcxGridChartItem;
    procedure UpdateActiveTab; virtual;
    procedure UpdateButtons; virtual;
    procedure UpdateEditor; override;
    procedure UpdateSelection; override;
    procedure UpdateItemList; virtual;

    procedure RestoreListBoxParams;
    procedure SaveListBoxParams;

    property ItemClass: TcxGridChartItemClass read GetItemClassValue;
    property ItemCount: Integer read GetItemCount;
    property ItemKind: TcxGridChartItemKind read GetItemKind write SetItemKind;
    property ItemName: string read GetItemName;
    property Items[Index: Integer]: TcxGridChartItem read GetItem;
  public
    class function GetViewByObject(APersistent: TPersistent): TcxCustomGridView; override;
    property View: TcxGridChartView read GetView;
  end;

  TcxGridChartViewStorage = class(TcxCustomGridViewStorage)
  private
    function GetDiagramArea: TcxGridChartAreaDiagram;
    function GetDiagramBar: TcxGridChartBarDiagram;
    function GetDiagramColumn: TcxGridChartColumnDiagram;
    function GetDiagramLine: TcxGridChartLineDiagram;
    function GetDiagramPie: TcxGridChartPieDiagram;
    function GetLegend: TcxGridChartLegend;
    function GetOptionsBehavior: TcxGridChartOptionsBehavior;
    function GetOptionsCustomize: TcxGridChartOptionsCustomize;
    function GetOptionsView: TcxGridChartOptionsView;
    function GetTitle: TcxGridChartTitle;
    function GetToolBox: TcxGridChartToolBox;
    function GetView: TcxGridChartView;
  protected
    property View: TcxGridChartView read GetView;
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  published
    property DiagramArea: TcxGridChartAreaDiagram read GetDiagramArea;
    property DiagramBar: TcxGridChartBarDiagram read GetDiagramBar;
    property DiagramColumn: TcxGridChartColumnDiagram read GetDiagramColumn;
    property DiagramLine: TcxGridChartLineDiagram read GetDiagramLine;
    property DiagramPie: TcxGridChartPieDiagram read GetDiagramPie;
    property Legend: TcxGridChartLegend read GetLegend;
    property OptionsBehavior: TcxGridChartOptionsBehavior read GetOptionsBehavior;
    property OptionsCustomize: TcxGridChartOptionsCustomize read GetOptionsCustomize;
    property OptionsView: TcxGridChartOptionsView read GetOptionsView;
    property Title: TcxGridChartTitle read GetTitle;
    property ToolBox: TcxGridChartToolBox read GetToolBox;
  end;

  TcxGridChartViewMenuProvider = class(TcxCustomGridViewMenuProvider)
  private
    function GetGridView: TcxGridChartView;
  protected
    procedure ActivateDiagram(Sender: TcxGridViewMenuItem);
    procedure CreateDataGroup(Sender: TcxGridViewMenuItem);
    procedure CreateSeries(Sender: TcxGridViewMenuItem);
    procedure InitLayoutItems; override;
    procedure InitStructureItems; override;
  public
    property GridView: TcxGridChartView read GetGridView;
  end;

implementation

{$R *.dfm}

uses
  Math, cxCustomData, cxDesignWindows;

const
  AddButtonCaption = '&Add';
  AddExButtonCaption = '&Add...';

function GetChartItemName(AItemKind: TcxGridChartItemKind): string;
begin
  case AItemKind of
    ciSeries:
      Result := 'Series';
    ciDataGroup:
      Result := 'DataGroup';
  else
    Result := '';
  end;
end;

function CreateChartItem(AView: TcxGridChartView; AItemKind: TcxGridChartItemKind): TcxGridChartItem;
begin
  AView.BeginUpdate;
  try
    case AItemKind of
      ciSeries:
        Result := AView.CreateSeries;
      ciDataGroup:
        Result := AView.CreateDataGroup;
    else
      Result := nil;
    end;
    Result.Name := GetViewItemUniqueName(AView, Result, GetChartItemName(AItemKind));
  finally
    AView.EndUpdate;
  end;
end;

{ TcxChartViewEditor }

function TcxChartViewEditor.GetItemClassValue: TcxGridChartItemClass;
begin
  Result := GetItemClass(ItemKind);
end;

function TcxChartViewEditor.GetView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited View);
end;

procedure TcxChartViewEditor.ReindexItemsProc(AList: TList; ANewIndex: Integer);
var
  I: Integer;
begin
  if AList.Count = 0 then Exit;
  for I := AList.Count - 1 downto 0 do
  begin
    if TcxGridChartItem(AList[I]).Index < ANewIndex then
      Dec(ANewIndex);
    TcxGridChartItem(AList[I]).Index := ANewIndex;
  end;
  UpdateDesigner;
end;

procedure TcxChartViewEditor.Loaded;
begin
  inherited;
  tcMain.TabIndex := 0;  // because it is deleted from DFM by SetupBuilder
end;

function TcxChartViewEditor.GetItem(Index: Integer): TcxGridChartItem;
begin
  case ItemKind of
    ciSeries:
      Result := View.Series[Index];
    ciDataGroup:
      Result := View.DataGroups[Index];
  else
    Result := nil;
  end;
end;

function TcxChartViewEditor.GetItemClass(AItemKind: TcxGridChartItemKind): TcxGridChartItemClass;
begin
  case AItemKind of
    ciSeries:
      Result := View.GetSeriesClass;
    ciDataGroup:
      Result := View.GetDataGroupClass;
  else
    Result := nil;
  end;
end;

function TcxChartViewEditor.GetItemCount: Integer;
begin
  case ItemKind of
    ciSeries:
      Result := View.SeriesCount;
    ciDataGroup:
      Result := View.DataGroupCount;
  else
    Result := 0;
  end;
end;

function TcxChartViewEditor.GetItemKind: TcxGridChartItemKind;
begin
  Result := TcxGridChartItemKind(tcMain.TabIndex);
end;

function TcxChartViewEditor.GetItemName: string;
begin
  Result := GetChartItemName(ItemKind);
end;

procedure TcxChartViewEditor.SetItemKind(Value: TcxGridChartItemKind);
begin
  if IsActiveTabChanging then Exit;
  if ItemKind <> Value then
  begin
    SaveListBoxParams;
    tcMain.TabIndex := Ord(Value);
    tcMainChange(nil);
  end;
end;

function TcxChartViewEditor.AddItem: TcxGridChartItem;
begin
  Result := CreateChartItem(View, ItemKind);
  SelectComponent(Result);
  UpdateDesigner;
end;

procedure TcxChartViewEditor.UpdateActiveTab;
var
  AIsItemFoundInSelection: Boolean;
  AItemKind, ASelectedItemKind: TcxGridChartItemKind;
  AList: TList;
  I: Integer;
  AObject: TObject;
begin
  AIsItemFoundInSelection := False;
  ASelectedItemKind := Low(ASelectedItemKind);
  AList := TList.Create;
  try
    GetSelectionList(AList);
    for I := 0 to AList.Count - 1 do
    begin
      AObject := TObject(AList[I]);
      if (AObject is TcxGridChartItem) and (TcxGridChartItem(AObject).GridView = View) then
        for AItemKind := Low(AItemKind) to High(AItemKind) do
          if AObject.ClassType = GetItemClass(AItemKind) then
            if AIsItemFoundInSelection and (ASelectedItemKind <> AItemKind) then
              Exit
            else
            begin
              AIsItemFoundInSelection := True;
              ASelectedItemKind := AItemKind;
              Break;
            end;
    end;
  finally
    AList.Free;
  end;
  if AIsItemFoundInSelection then
    ItemKind := ASelectedItemKind;
end;

procedure TcxChartViewEditor.UpdateButtons;
begin
  btnItemAdd.Enabled := CanAddComponent;
  btnItemDelete.Enabled := CanDeleteComponent(nil) and (lbItems.SelCount > 0);
  btnItemMoveUp.Enabled := lbItems.SelCount > 0;
  btnItemMoveDown.Enabled := lbItems.SelCount > 0;
  btnItemSelectAll.Enabled := lbItems.SelCount < lbItems.Items.Count;

  miItemAdd.Enabled := btnItemAdd.Enabled;
  miItemDelete.Enabled := btnItemDelete.Enabled;
  miItemMoveUp.Enabled := btnItemMoveUp.Enabled;
  miItemMoveDown.Enabled := btnItemMoveDown.Enabled;
  miItemSelectAll.Enabled := btnItemSelectAll.Enabled;

  if Supports(DataController, IcxGridChartViewItemsProvider) then
  begin
    btnItemAdd.Caption := AddExButtonCaption;
    btnItemAdd.OnClick := btnItemAddExClick;
  end
  else
  begin
    btnItemAdd.Caption := AddButtonCaption;
    btnItemAdd.OnClick := btnItemAddClick;
  end;
end;

procedure TcxChartViewEditor.UpdateEditor;
begin
  inherited;
  UpdateItemList;
end;

procedure TcxChartViewEditor.UpdateSelection;
begin
  inherited;
  UpdateActiveTab;
  FormEditor.ListBoxSynchronizeSelection(lbItems.InnerListBox);
  UpdateButtons;
end;

procedure TcxChartViewEditor.UpdateItemList;
var
  ASelection: TStringList;
  AItemIndex, ATopIndex, I: Integer;
  AItem: TcxGridChartItem;
  S: string;
begin
  ListBoxSaveSelection(lbItems.InnerListBox, ASelection, AItemIndex, ATopIndex);
  try
    lbItems.Items.Clear;
    for I := 0 to ItemCount - 1 do
    begin
      AItem := Items[I];
      S := AItem.Name + ' - "' + AItem.GetDisplayText + '"';
      lbItems.Items.AddObject(S, AItem);
    end;
  finally
    ListBoxRestoreSelection(lbItems.InnerListBox, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure TcxChartViewEditor.RestoreListBoxParams;
begin
  lbItems.Items.BeginUpdate;
  try
    ListBoxRestorePos(lbItems.InnerListBox, FListBoxItemIndex[ItemKind], FListBoxTopIndex[ItemKind]);
  finally
    lbItems.Items.EndUpdate;
    cxRedrawWindow(lbItems.Handle, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);  // to fix the bug with scrollbar updating
  end;
end;

procedure TcxChartViewEditor.SaveListBoxParams;
begin
  ListBoxSavePos(lbItems.InnerListBox, FListBoxItemIndex[ItemKind], FListBoxTopIndex[ItemKind]);
end;

class function TcxChartViewEditor.GetViewByObject(APersistent: TPersistent): TcxCustomGridView;
begin
  if APersistent is TcxGridChartItem then
    Result := TcxGridChartItem(APersistent).GridView
  else
    if APersistent is TcxGridChartDiagram then
      Result := TcxGridChartDiagram(APersistent).GridView
    else
      Result := inherited GetViewByObject(APersistent);
end;

procedure TcxChartViewEditor.lbItemsClick(Sender: TObject);
begin
  FormEditor.ListBoxApplySelection(lbItems.InnerListBox, View);
end;

procedure TcxChartViewEditor.btnItemAddClick(Sender: TObject);
begin
  AddItem;
end;

procedure TcxChartViewEditor.btnItemAddExClick(Sender: TObject);

  procedure InitPopupMenu(APopupMenu: TPopupMenu; const AProvider: IcxGridChartViewItemsProvider);
  var
    I: Integer;
    ACaptions: TStringList;
  begin
    APopupMenu.Items.Clear;
    APopupMenu.Items.Add(CreateMenuItem(pmItems, 'Blank', miItemAddExClick));
    APopupMenu.Items.Add(CreateMenuItem(pmItems, '-'));

    ACaptions := TStringList.Create;
    try
      AProvider.GetItemCaptions(ItemClass, ACaptions);
      for I := 0 to ACaptions.Count - 1 do
        APopupMenu.Items.Add(CreateMenuItem(pmItems, ACaptions[I], miItemAddExClick,
          True, I, AProvider.GetItem(ItemClass, I) <> nil));
    finally
      ACaptions.Free;
    end;
  end;

  function GetPopupMenuPosition(AButton: TcxButton): TPoint;
  begin
    with AButton.BoundsRect do
      Result := Point(Left, Bottom);
    Result := AButton.Parent.ClientToScreen(Result);
  end;

begin
  InitPopupMenu(pmItemsAdd, DataController as IcxGridChartViewItemsProvider);
  with GetPopupMenuPosition(btnItemAdd) do
    pmItemsAdd.Popup(X, Y);
end;

procedure TcxChartViewEditor.miItemAddExClick(Sender: TObject);
var
  AIndex: Integer;
  AItem: TcxGridChartItem;
begin
  AIndex := (Sender as TMenuItem).Tag;
  if (AIndex = -1) or not (Sender as TMenuItem).Checked then
  begin
    View.BeginUpdate;
    try
      AItem := AddItem;
      if AIndex <> -1 then
      begin
        (DataController as IcxGridChartViewItemsProvider).InitItem(AItem, AIndex);
        UpdateDesigner;
      end;
    finally
      View.EndUpdate;
    end;
  end
  else
  begin
    SelectComponent(View);
    (DataController as IcxGridChartViewItemsProvider).GetItem(ItemClass, AIndex).Free;
    UpdateDesigner;
  end;
end;

procedure TcxChartViewEditor.btnItemDeleteClick(Sender: TObject);
begin
  if lbItems.SelCount > 0 then
  begin
    BeginUpdate;
    try
      ListBoxDeleteSelection(lbItems.InnerListBox, True);
      FormEditor.ListBoxApplySelection(lbItems.InnerListBox, View);
      UpdateDesigner;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxChartViewEditor.btnItemMoveUpClick(Sender: TObject);
begin
  FItemPrevDragIndex := -1;
  ListBoxMoveItems(lbItems.InnerListBox,
    Max(0, ListBoxGetFirstSelectedIndex(lbItems.InnerListBox) - 1),
    FItemPrevDragIndex, ReindexItemsProc);
end;

procedure TcxChartViewEditor.btnItemMoveDownClick(Sender: TObject);
begin
  FItemPrevDragIndex := -1;
  ListBoxMoveItems(lbItems.InnerListBox,
    Min(lbItems.Items.Count, ListBoxGetLastSelectedIndex(lbItems.InnerListBox) + 2),
    FItemPrevDragIndex, ReindexItemsProc);
end;

procedure TcxChartViewEditor.btnItemSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(lbItems.InnerListBox);
  FormEditor.ListBoxApplySelection(lbItems.InnerListBox, View);
end;

procedure TcxChartViewEditor.lbItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FItemPrevDragIndex := -1;
end;

procedure TcxChartViewEditor.lbItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ListBoxDragOver(lbItems.InnerListBox, Sender, Source, X, Y, State, Accept, FItemPrevDragIndex);
end;

procedure TcxChartViewEditor.lbItemsDrawItem(AControl: TcxListBox;
  ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  ACanvas.FillRect(ARect);
  ACanvas.TextOut(ARect.Left + ScaleFactor.Apply(3), ARect.Top + ScaleFactor.Apply(3), AControl.Items[AIndex]);
end;

procedure TcxChartViewEditor.lbItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ListBoxDragDrop(lbItems.InnerListBox, Sender, Source, X, Y, FItemPrevDragIndex, ReindexItemsProc);
end;

procedure TcxChartViewEditor.lbItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  ListBoxEndDrag(lbItems.InnerListBox, Sender, Target, X, Y, FItemPrevDragIndex);
end;

procedure TcxChartViewEditor.FormCreate(Sender: TObject);
begin
  inherited;
  with lbItems do
  begin
    Canvas.Font := Font;
    ItemHeight := 2 * Self.ScaleFactor.Apply(3) + cxTextHeight(Canvas.Handle);
  end;
end;

procedure TcxChartViewEditor.tcMainChanging(Sender: TObject; var AllowChange: Boolean);
begin
  SaveListBoxParams;
end;

procedure TcxChartViewEditor.tcMainChange(Sender: TObject);
begin
  IsActiveTabChanging := True;
  try
    UpdateItemList;
    RestoreListBoxParams;
    UpdateSelection;
  finally
    IsActiveTabChanging := False;
  end;
end;

{ TcxGridChartViewStorage }

function TcxGridChartViewStorage.GetDiagramArea: TcxGridChartAreaDiagram;
begin
  Result := View.DiagramArea;
end;

function TcxGridChartViewStorage.GetDiagramBar: TcxGridChartBarDiagram;
begin
  Result := View.DiagramBar;
end;

function TcxGridChartViewStorage.GetDiagramColumn: TcxGridChartColumnDiagram;
begin
  Result := View.DiagramColumn;
end;

function TcxGridChartViewStorage.GetDiagramLine: TcxGridChartLineDiagram;
begin
  Result := View.DiagramLine;
end;

function TcxGridChartViewStorage.GetDiagramPie: TcxGridChartPieDiagram;
begin
  Result := View.DiagramPie;
end;

function TcxGridChartViewStorage.GetLegend: TcxGridChartLegend;
begin
  Result := View.Legend;
end;

function TcxGridChartViewStorage.GetOptionsBehavior: TcxGridChartOptionsBehavior;
begin
  Result := View.OptionsBehavior;
end;

function TcxGridChartViewStorage.GetOptionsCustomize: TcxGridChartOptionsCustomize;
begin
  Result := View.OptionsCustomize;
end;

function TcxGridChartViewStorage.GetOptionsView: TcxGridChartOptionsView;
begin
  Result := View.OptionsView;
end;

function TcxGridChartViewStorage.GetTitle: TcxGridChartTitle;
begin
  Result := View.Title;
end;

function TcxGridChartViewStorage.GetToolBox: TcxGridChartToolBox;
begin
  Result := View.ToolBox;
end;

function TcxGridChartViewStorage.GetView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited View);
end;

class function TcxGridChartViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridChartView;
end;

{ TcxGridChartViewMenuProvider }

function TcxGridChartViewMenuProvider.GetGridView: TcxGridChartView;
begin
  Result := TcxGridChartView(inherited GridView);
end;

procedure TcxGridChartViewMenuProvider.ActivateDiagram(Sender: TcxGridViewMenuItem);
var
  ADiagram: TcxGridChartDiagram;
begin
  ADiagram := GridView.FindDiagramByDisplayText(Sender.Caption);
  if not ADiagram.Active then
  begin
    ADiagram.Active := True;
    DesignerModified;
  end;
  SelectObject(ADiagram);
end;

procedure TcxGridChartViewMenuProvider.CreateDataGroup(Sender: TcxGridViewMenuItem);
begin
  ObjectCreated(CreateChartItem(GridView, ciDataGroup));
end;

procedure TcxGridChartViewMenuProvider.CreateSeries(Sender: TcxGridViewMenuItem);
begin
  ObjectCreated(CreateChartItem(GridView, ciSeries));
end;

procedure TcxGridChartViewMenuProvider.InitLayoutItems;
var
  I: Integer;
begin
  inherited;
  for I := 0 to GridView.DiagramCount - 1 do
    Items.AddItem(GridView.Diagrams[I].DisplayText, ActivateDiagram, True,
      GridView.Diagrams[I].Active);
end;

procedure TcxGridChartViewMenuProvider.InitStructureItems;
begin
  Items.AddItem('Create Series', CreateSeries, CanAddComponent(GridView.Owner));
  Items.AddItem('Create DataGroup', CreateDataGroup, CanAddComponent(GridView.Owner));
  Items.AddSeparator;
  inherited;
end;

initialization
  RegisterViewEditorClass(TcxGridChartView, TcxChartViewEditor);
  RegisterDefaultViewStorage(TcxGridChartViewStorage);
  RegisterViewMenuProviderClass(TcxGridChartView, TcxGridChartViewMenuProvider);

finalization
  UnregisterViewMenuProviderClass(TcxGridChartView, TcxGridChartViewMenuProvider);
  UnregisterDefaultViewStorage(TcxGridChartViewStorage);
  UnregisterViewEditorClass(TcxGridChartView, TcxChartViewEditor);

end.
