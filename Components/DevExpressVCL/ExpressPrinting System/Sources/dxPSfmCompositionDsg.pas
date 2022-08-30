{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSfmCompositionDsg;

interface

{$I cxVer.inc}

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, ImgList, Menus, ExtCtrls, ComCtrls, IniFiles,
  dxCore, dxPSCore, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxListView, cxPC, cxEdit, cxLabel,
  cxGraphics, cxLookAndFeels, cxGroupBox, cxCheckBox, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxLayoutLookAndFeels;

type
  TdxfmCompositionDesignWindow = class(TAbstractdxReportLinkDesignWindow)
    btnAdd: TcxButton;
    btnClose: TcxButton;
    btnDelete: TcxButton;
    btnDesign: TcxButton;
    btnHelp: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    cbStartEachItemFromNewPage: TcxCheckBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    ilItems: TcxImageList;
    lbbtnHelp: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lvItems: TcxListView;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miDesign: TMenuItem;
    miLine1: TMenuItem;
    miLine2: TMenuItem;
    miMoveDown: TMenuItem;
    miMoveUp: TMenuItem;
    miRename: TMenuItem;
    miSelectAll: TMenuItem;
    N1: TMenuItem;
    pmItems: TPopupMenu;
    pnlNoItems: TcxLabel;
    tbsItems: TdxLayoutGroup;

    procedure AddClick(Sender: TObject);
    procedure cbStartEachItemFromNewPageClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure DesignerClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvItemsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvItemsEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure lvItemsEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lvItemsResize(Sender: TObject);
    procedure lvItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure MoveDownClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure pmItemsPopup(Sender: TObject);
    procedure RenameClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
  private
    FIsRefreshing: Boolean;
    FListViewWndProc: TWndMethod;
    FSaveDragIndex: Integer;

    function GetActiveItem: TdxCompositionLinkItem;
    function GetComposition: TdxCompositionReportLink;
    function GetHasOnlyBuiltInsAreInSelection: Boolean;
    function GetIsSelected(Index: Integer): Boolean;
    function GetItem(Index: Integer): TdxCompositionLinkItem;
    function GetItemCount: Integer;
    function GetSelectedCount: Integer;
    function GetShowDescription: Boolean;
    procedure SetActiveItem(Value: TdxCompositionLinkItem);
    procedure SetIsSelected(Index: Integer; Value: Boolean);

    function CanAdd: Boolean;
    function CanDelete: Boolean;
    function CanDesign: Boolean;
    function CanEdit: Boolean;
    function CanMoveDown: Boolean;
    function CanMoveUp: Boolean;
    function CanRename: Boolean;
    function CanSelectAll: Boolean;

    procedure DeleteSelection;
    function GetSelectedItemsAsString: string;
    procedure MoveSelection(ADelta: Integer);
    procedure RefreshColumns;
    procedure RefreshList;

    procedure ListViewWndProc(var Message: TMessage);
    procedure SubClassListView;
    procedure UnsubClassListView;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure BeforeConstruction; override;
    procedure Initialize; override;
    procedure LoadStrings; override;
    procedure UpdateControlsState; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;

    property ActiveItem: TdxCompositionLinkItem read GetActiveItem write SetActiveItem;
    property Composition: TdxCompositionReportLink read GetComposition;
    property HasOnlyBuiltInsAreInSelection: Boolean read GetHasOnlyBuiltInsAreInSelection;
    property IsSelected[Index: Integer]: Boolean read GetIsSelected write SetIsSelected;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxCompositionLinkItem read GetItem;
    property SelectedCount: Integer read GetSelectedCount;
    property ShowDescription: Boolean read GetShowDescription;
  end;

implementation

{$R *.DFM}

uses
  SysUtils, CommCtrl, Math, dxPSRes, dxPSGlbl, dxPSUtl, dxPSForm, dxPSPopupMan, dxPSfmCompositionAdd, dxPSImgs;

constructor TdxfmCompositionDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcCompositionReportLinkDesigner;
  inherited Create(AOwner);
  dxPSPopupMenuController.RegisterControl(lvItems);
  dxLoadImageListFromResources(ilItems, IDIL_DXPSDESIGNWINDOWMENU);
  SubClassListView;
end;

destructor TdxfmCompositionDesignWindow.Destroy;
begin
  UnsubClassListView;
  dxPSPopupMenuController.UnregisterControl(lvItems);
  inherited Destroy;
end;

procedure TdxfmCompositionDesignWindow.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  dxLoadListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
end;

procedure TdxfmCompositionDesignWindow.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  dxSaveListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
end;

procedure TdxfmCompositionDesignWindow.MoveUpClick(Sender: TObject);
begin
  MoveSelection(-1);
end;

procedure TdxfmCompositionDesignWindow.MoveDownClick(Sender: TObject);
begin
  MoveSelection(1);
end;

procedure TdxfmCompositionDesignWindow.lvItemsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (Change = ctState) and not FIsRefreshing then
    UpdateControlsState;
end;

procedure TdxfmCompositionDesignWindow.DesignerClick(Sender: TObject);
begin
  if CanDesign then
    if ActiveItem.ReportLink.DesignReport then
      Modified := True
    else
      UpdateControlsState;
end;

procedure TdxfmCompositionDesignWindow.AddClick(Sender: TObject);
var
  Data: TdxAddItemsToCompositionDlgData;
  I: Integer;
begin
  FillChar(Data, SizeOf(TdxAddItemsToCompositionDlgData), 0);
  Data.Composition := Composition;
  if ShowDescription then
    Data.Options := [caiShowDescription];
  Data.Items := TList.Create;
  try
    if dxShowAddItemsToCompositionDlg(Data) then
    begin
      Modified := True;
      if Data.Items.Count > 1 then Composition.Items.BeginUpdate;
      try
        for I := 0 to Data.Items.Count - 1 do
          Composition.Items.AddLink(TBasedxReportLink(Data.Items[I]));
      finally
        if Data.Items.Count > 1 then Composition.Items.EndUpdate;
      end;
      RefreshList;
    end;
  finally
    Data.Items.Free;
  end;
end;

procedure TdxfmCompositionDesignWindow.DeleteClick(Sender: TObject);
begin
  if not lvItems.IsEditing and
    MessageQuestion(Format(cxGetResourceString(@sdxConfirmDeleteItem), [GetSelectedItemsAsString])) then
    DeleteSelection;
end;

procedure TdxfmCompositionDesignWindow.RenameClick(Sender: TObject);
begin
  if SelectedCount = 1 then
    lvItems.Selected.EditCaption;
end;

procedure TdxfmCompositionDesignWindow.SelectAllClick(Sender: TObject);
var
  I: Integer;
begin
  lvItems.Items.BeginUpdate;
  try
    for I := 0 to lvItems.Items.Count - 1 do
      lvItems.Items[I].Selected := True;
  finally
    lvItems.Items.EndUpdate;
  end;
end;

procedure TdxfmCompositionDesignWindow.pmItemsPopup(Sender: TObject);
begin
  miDesign.Enabled := CanDesign;
  miAdd.Enabled := CanAdd;
  miDelete.Enabled := CanDelete;
  miRename.Enabled := CanRename;
  miSelectAll.Enabled := CanSelectAll;
  miMoveUp.Enabled := CanMoveUp;
  miMoveDown.Enabled := CanMoveDown;
end;

procedure TdxfmCompositionDesignWindow.lvItemsEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := IsDesigning or not Items[Item.Index].BuiltIn;
  if AllowEdit then
    lvItems.PopupMenu := nil;
end;

procedure TdxfmCompositionDesignWindow.lvItemsResize(Sender: TObject);
var
  R: TRect;
begin
  R := lvItems.ClientRect;
  InflateRect(R, -3, -3);
  with R do
  begin
    Top := (Bottom - Top - pnlNoItems.Height) div 2;
    Bottom := Top + pnlNoItems.Height;
  end;
  pnlNoItems.BoundsRect := R;
end;

procedure TdxfmCompositionDesignWindow.lvItemsEdited(Sender: TObject;
  Item: TListItem; var S: string);
begin
  Items[Item.Index].ReportLink.Caption := S;
end;

procedure TdxfmCompositionDesignWindow.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not lvItems.IsEditing;
end;

procedure TdxfmCompositionDesignWindow.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or (Key = VK_RETURN) then
    Close;
end;

procedure TdxfmCompositionDesignWindow.lvItemsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  MoveSelection(FSaveDragIndex - TcxListView(Sender).ItemFocused.Index);
end;

procedure TdxfmCompositionDesignWindow.lvItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  Accept := Sender = cxExtractDragObjectSource(Source);
  if Accept then
  begin
    Item := TcxListView(Sender).GetItemAt(X, Y);
    Accept := (Item <> nil) and (FSaveDragIndex <> TcxListView(Sender).ItemFocused.Index);
    if Item = nil then
      FSaveDragIndex := -1
    else
      FSaveDragIndex := Item.Index;
  end;
end;

procedure TdxfmCompositionDesignWindow.lvItemsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FSaveDragIndex := -1;
end;

procedure TdxfmCompositionDesignWindow.BeforeConstruction;
begin
  inherited BeforeConstruction;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmCompositionDesignWindow.Initialize;
begin
  inherited Initialize;
  cbStartEachItemFromNewPage.Checked := Composition.StartEachItemFromNewPage;
  CheckDialogFormHelpContext(Self, lbbtnHelp);
  RefreshColumns;
  RefreshList;
  pnlNoItems.Parent := lvItems;
  lvItemsResize(nil);
end;

procedure TdxfmCompositionDesignWindow.LoadStrings;
begin
  inherited;
  tbsItems.Caption := cxGetResourceString(@sdxItems);
  pnlNoItems.Caption := cxGetResourceString(@sdxThereAreNowItemsForShow);
  cbStartEachItemFromNewPage.Caption := cxGetResourceString(@sdxCompositionStartEachItemFromNewPage);

  btnDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  btnAdd.Caption := AddEndEllipsis(cxGetResourceString(@sdxBtnAdd));
  btnDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  btnMoveUp.Caption := cxGetResourceString(@sdxBtnMoveUp);
  btnMoveDown.Caption := cxGetResourceString(@sdxBtnMoveDown);
  btnClose.Caption := cxGetResourceString(@sdxBtnClose);

  miDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  miAdd.Caption := AddEndEllipsis(cxGetResourceString(@sdxBtnAdd));
  miDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  miRename.Caption := cxGetResourceString(@sdxRename);
  miSelectAll.Caption := cxGetResourceString(@sdxSelectAll);
  miMoveUp.Caption := cxGetResourceString(@sdxBtnMoveUp);
  miMoveDown.Caption := cxGetResourceString(@sdxBtnMoveDown);
end;

procedure TdxfmCompositionDesignWindow.UpdateControlsState;
begin
  pnlNoItems.Visible := ItemCount = 0;
  btnDesign.Enabled := CanDesign;
  btnAdd.Enabled := CanAdd;
  btnDelete.Enabled := CanDelete;
  btnMoveUp.Enabled := CanMoveUp;
  btnMoveDown.Enabled := CanMoveDown;
end;

function TdxfmCompositionDesignWindow.GetActiveItem: TdxCompositionLinkItem;
begin
  if SelectedCount = 1 then
    Result := TdxCompositionLinkItem(lvItems.Selected.Data)
  else
    Result := nil;
end;

function TdxfmCompositionDesignWindow.GetComposition: TdxCompositionReportLink;
begin
  Result := TdxCompositionReportLink(ReportLink);
end;

function TdxfmCompositionDesignWindow.GetHasOnlyBuiltInsAreInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
    if IsSelected[I] and not Items[I].BuiltIn then
      Exit;
  Result := True;
end;

function TdxfmCompositionDesignWindow.GetIsSelected(Index: Integer): Boolean;
begin
  Result := lvItems.Items[Index].Selected;
end;

function TdxfmCompositionDesignWindow.GetItem(Index: Integer): TdxCompositionLinkItem;
begin
  Result := TdxCompositionLinkItem(lvItems.Items[Index].Data);
end;

function TdxfmCompositionDesignWindow.GetItemCount: Integer;
begin
  Result := lvItems.Items.Count;
end;

function TdxfmCompositionDesignWindow.GetSelectedCount: Integer;
begin
  Result := lvItems.SelCount;
end;

function TdxfmCompositionDesignWindow.GetShowDescription: Boolean;
begin
  Result := (Composition <> nil) and (coShowDescription in Composition.DesignerOptions);
end;

procedure TdxfmCompositionDesignWindow.SetActiveItem(Value: TdxCompositionLinkItem);
var
  I: Integer;
  Item: TListItem;
begin
  for I := 0 to lvItems.Items.Count - 1 do
  begin
    Item := lvItems.Items[I];
    if Item.Selected then Item.Selected := False;
  end;

  Item := lvItems.FindData(0, Value, True, True);
  if Item <> nil then
  begin
    Item.Selected := True;
    Item.Focused := True;
  end;
end;

procedure TdxfmCompositionDesignWindow.SetIsSelected(Index: Integer; Value: Boolean);
begin
  lvItems.Items[Index].Selected := Value;
end;

function TdxfmCompositionDesignWindow.CanAdd: Boolean;
begin
  Result := CanEdit;
end;

function TdxfmCompositionDesignWindow.CanDelete: Boolean;
begin
  Result := not lvItems.IsEditing and CanEdit and (SelectedCount <> 0) and
    (IsDesigning or not HasOnlyBuiltInsAreInSelection);
end;

function TdxfmCompositionDesignWindow.CanDesign: Boolean;
begin
  Result := (SelectedCount = 1) and ActiveItem.ReportLink.CheckToDesign;
end;

function TdxfmCompositionDesignWindow.CanEdit: Boolean;
begin
  Result := (Composition <> nil) and (coCanEdit in Composition.DesignerOptions);
end;

function TdxfmCompositionDesignWindow.CanMoveDown: Boolean;
var
  I, Counter: Integer;
begin
  if CanEdit then
  begin
    Counter := 0;
    for I := ItemCount - 1 downto 0 do
    begin
      if not IsSelected[I] then
      begin
        Result := Counter < SelectedCount;
        Exit;
      end;
      Inc(Counter);
    end;
  end;
  Result := False;
end;

function TdxfmCompositionDesignWindow.CanMoveUp: Boolean;
var
  I: Integer;
begin
  if CanEdit then
    for I := 0 to ItemCount - 1 do
      if not IsSelected[I] then
      begin
        Result := I < SelectedCount;
        Exit;
      end;
  Result := False;
end;

function TdxfmCompositionDesignWindow.CanRename: Boolean;
begin
  Result := (SelectedCount = 1) and (IsDesigning or not ActiveItem.BuiltIn);
end;

function TdxfmCompositionDesignWindow.CanSelectAll: Boolean;
begin
  Result := (ItemCount <> 0) and (SelectedCount <> ItemCount);
end;

procedure TdxfmCompositionDesignWindow.cbStartEachItemFromNewPageClick(Sender: TObject);
begin
  Composition.StartEachItemFromNewPage := cbStartEachItemFromNewPage.Checked;
end;

procedure TdxfmCompositionDesignWindow.DeleteSelection;
var
  SelCount, I: Integer;
begin
  SelCount := SelectedCount;
  if SelCount > 1 then Composition.Items.BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
      if IsSelected[I] and (IsDesigning or not Items[I].BuiltIn) then
        TObject(lvItems.Items[I].Data).Free;
  finally
    if SelCount > 1 then Composition.Items.EndUpdate;
  end;
  RefreshList;
  Modified := True;
end;

function TdxfmCompositionDesignWindow.GetSelectedItemsAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ItemCount - 1 do
    if IsSelected[I] and (IsDesigning or not Items[I].BuiltIn) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + '"' + lvItems.Items[I].Caption + '"';
    end;
end;

procedure TdxfmCompositionDesignWindow.MoveSelection(ADelta: Integer);

  procedure MoveDown(ADelta: Integer);
  var
    I, Index: Integer;
  begin
    for I := ItemCount - 1 downto 0 do
      if IsSelected[I] then
      begin
        Index := Items[I].Index;
        Inc(Index, ADelta);
        if Index > ItemCount - 1 then Index := ItemCount - 1;
//        while (Index < ItemCount) and IsSelected[Index] do Inc(Index);
        Items[I].Index := Index;
      end;
  end;

  procedure MoveUp(ADelta: Integer);
  var
    I, Index: Integer;
  begin
    for I := 0 to ItemCount - 1 do
      if IsSelected[I] then
      begin
        Index := Items[I].Index;
        Inc(Index, ADelta);
        if Index < 0 then Index := 0;
//        while (Index > -1) and IsSelected[Index] do Dec(Index);
        Items[I].Index := Index;
      end;
  end;

begin
  Composition.Items.BeginUpdate;
  try
    if ADelta > 0 then
      MoveDown(ADelta)
    else
      MoveUp(ADelta);
  finally
    Composition.Items.EndUpdate;
  end;
  RefreshList;
end;

procedure TdxfmCompositionDesignWindow.RefreshColumns;
var
  ColumnWidths: array of Integer;
  I: Integer;
begin
  lvItems.Columns.BeginUpdate;
  try
    SetLength(ColumnWidths, lvItems.Columns.Count);
    for I := 0 to lvItems.Columns.Count - 1 do
      ColumnWidths[I] := lvItems.Columns[I].Width;

    lvItems.Columns.Clear;
    with lvItems.Columns.Add do
    begin
      Width := 2 * //+ Ord(not(coShowDescription in Composition.DesignerOptions))) *
         (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL)) div 3 - 3;
      Caption := cxGetResourceString(@sdxItemName);
    end;

    if coShowDescription in Composition.DesignerOptions then
      with lvItems.Columns.Add do
      begin
        Width := (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL)) div 3 - 3;
        Caption := cxGetResourceString(@sdxItemDescription);
      end;

    for I := 0 to Min(lvItems.Columns.Count - 1, Length(ColumnWidths) - 1) do
      lvItems.Columns[I].Width := ColumnWidths[I];
  finally
    lvItems.Columns.EndUpdate;
  end;
end;

procedure TdxfmCompositionDesignWindow.RefreshList;
const
  Styles: array[Boolean] of TViewStyle = (vsReport, vsList);
var
  Selection: TList;
  I: Integer;
  Strings: TStrings;
begin
  lvItems.Items.BeginUpdate;
  FIsRefreshing := True;
  try
    Selection := TList.Create;
    try
      dxSaveListViewSelection(lvItems.InnerListView, Selection);
      Strings := TStringList.Create;
      try
        lvItems.Items.Clear;
        if Composition <> nil then
          Composition.GetItems(Strings, True);

        for I := 0 to Strings.Count - 1 do
          with lvItems.Items.Add do
          begin
            Caption := Strings[I];
            Data := Strings.Objects[I];
            SubItems.Add(TdxCompositionLinkItem(Data).ReportLink.Description);
          end;
      finally
        Strings.Free;
      end;
      dxRestoreListViewSelection(lvItems.InnerListView, Selection);
    finally
      Selection.Free;
    end;

    lvItems.ViewStyle := Styles[ItemCount = 0];
//    lvItems.Enabled := not FNoItemsState;
    lvItems.HideSelection := ItemCount = 0;
    if (lvItems.SelCount = 0) and (ItemCount <> 0) then
      lvItems.Selected := lvItems.Items[0];
  finally
    FIsRefreshing := False;
    lvItems.Items.EndUpdate;
  end;
  UpdateControlsState;
  lvItems.Refresh;
end;

procedure TdxfmCompositionDesignWindow.ListViewWndProc(var Message: TMessage);
begin
  FListViewWndProc(Message);
  if Message.Msg = CN_NOTIFY then
    if TWMNotify(Message).NMHdr^.Code = LVN_ENDLABELEDIT then
      lvItems.PopupMenu := pmItems;
end;

procedure TdxfmCompositionDesignWindow.SubClassListView;
begin
  lvItems.HandleNeeded;
  FListViewWndProc := lvItems.WindowProc;
  lvItems.WindowProc := ListViewWndProc;
end;

procedure TdxfmCompositionDesignWindow.UnsubClassListView;
begin
  lvItems.WindowProc := FListViewWndProc;
end;

procedure TdxfmCompositionDesignWindow.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if IsAccel(Message.CharCode, tbsItems.Caption) then
  begin
    Message.Result := 1;
    if lvItems.Enabled then ActiveControl := lvItems;
  end;
end;

end.
