{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl Look & Feel design-time form        }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutLookAndFeelListDesignForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, ActnList, Variants,
{$IFNDEF DXLAYOUTTEST}
  DesignIntf, cxDesignWindows,
{$ENDIF}
  cxControls, dxLayoutControl, dxLayoutLookAndFeels, cxGraphics,
  Menus, dxLayoutControlAdapters, cxMemo, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxLookAndFeelPainters, cxButtons, cxRichEdit, dxLayoutcxEditAdapters, cxLookAndFeels,
  dxLayoutContainer, cxClasses, cxImageList;

type
{$IFNDEF DXLAYOUTTEST}
  TdxLayoutLookAndFeelListDesignForm = class(TcxDesignFormEditor, IdxLayoutLookAndFeelsDesigner)
{$ELSE}
  TdxLayoutLookAndFeelListDesignForm = class(TForm, IdxLayoutLookAndFeelsDesigner)
{$ENDIF}
    lcMain: TdxLayoutControl;
    lbItems: TListBox;
    lcMainItem1: TdxLayoutItem;
    lcMainGroup3: TdxLayoutGroup;
    lflMain: TdxLayoutLookAndFeelList;
    lcPreview: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    alMain: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    lcMainItem5: TdxLayoutItem;
    tlbGroups: TToolBar;
    tbAddItem: TToolButton;
    tbDelete: TToolButton;
    PopupMenu1: TPopupMenu;
    ilMain: TcxImageList;
    ilMainDisabled: TcxImageList;
    cxTextEdit1: TcxTextEdit;
    lcPreviewItem1: TdxLayoutItem;
    cxMemo1: TcxMemo;
    lcPreviewItem5: TdxLayoutItem;
    cxCheckBox3: TcxCheckBox;
    lcPreviewItem2: TdxLayoutItem;
    cxTextEdit2: TcxTextEdit;
    lcPreviewItem8: TdxLayoutItem;
    cxRichEdit1: TcxRichEdit;
    lcPreviewItem3: TdxLayoutItem;
    cxButton1: TcxButton;
    lcPreviewItem4: TdxLayoutItem;
    cxButton2: TcxButton;
    lcPreviewItem6: TdxLayoutItem;
    cxButton3: TcxButton;
    lcPreviewItem9: TdxLayoutItem;
    lcPreviewGroup2: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcPreviewGroup4: TdxLayoutGroup;
    liPreview: TdxLayoutItem;
    lcPreviewGroup3: TdxLayoutGroup;
    lcPreviewGroup5: TdxLayoutGroup;
    dxLayoutWebLookAndFeel1: TdxLayoutWebLookAndFeel;
    lcPreview_Root: TdxLayoutGroup;
    lgRoot: TdxLayoutGroup;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    procedure btnAddClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure lbItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
  {$IFDEF DXLAYOUTTEST}
    FLockCount: Integer;
    FSelection: TList;
    FComponent: TComponent;
  {$ENDIF}

    function CanModify: Boolean;
    function GetListBoxItemHeight: Integer;
    function GetList: TdxLayoutLookAndFeelList;
    procedure SynchronizeListBoxSelection;
  protected
    procedure Loaded; override;
    procedure SetComponent(AValue: TComponent); {$IFNDEF DXLAYOUTTEST}override;{$ENDIF}
    procedure UpdateCaption; {$IFNDEF DXLAYOUTTEST}override;{$ENDIF}
    procedure UpdateContent; {$IFNDEF DXLAYOUTTEST}override;{$ENDIF}

    procedure DeleteItems;
    procedure Refresh(ARefreshSelection: Boolean = False);
    procedure RefreshEnableds;
    procedure RefreshListBox;
    procedure SetAddItemsActionEnabled(AEnabled: Boolean);
    procedure SetDeleteItemsActionEnabled(AEnabled: Boolean);
    procedure SynchronizeControlsSelection(AList: TList);
    procedure SynchronizeSelection(AList: TList);
    //IdxLayoutLookAndFeelsDesigner
    procedure ComponentNameChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
    procedure ItemsChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
    procedure Edit(ALookAndFeelList: TdxLayoutLookAndFeelList);

  {$IFDEF DXLAYOUTTEST}
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate(AForceUpdate: Boolean = True);
    function IsUpdateLocked: Boolean;
    procedure UpdateSelection;
    procedure SelectComponent(AComponent: TPersistent);
    procedure SelectComponents(AList: TList);
    procedure GetSelectionList(AList: TList);

    property Component: TComponent read FComponent write SetComponent;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
  {$IFDEF DXLAYOUTTEST}
    procedure SelectionsChanged;
  {$ELSE}
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
  {$ENDIF}

    property List: TdxLayoutLookAndFeelList read GetList;
  end;

procedure ShowLayoutLookAndFeelDesigner(AList: TdxLayoutLookAndFeelList);

implementation

{$R *.dfm}

uses
  Math, Contnrs, dxCore;

procedure ShowLayoutLookAndFeelDesigner(AList: TdxLayoutLookAndFeelList);
var
  AForm: TdxLayoutLookAndFeelListDesignForm;
begin
  AForm := TdxLayoutLookAndFeelListDesignForm.Create(AList);
  try
    AForm.Component := AList;
    AForm.ShowModal;
  finally
    AForm.Free;
  end;
end;

{$IFDEF DXLAYOUTTEST}
function LockListBox(AListBox: TListBox): TNotifyEvent;
begin
  Result := AListBox.OnClick;
  AListBox.OnClick := nil;
end;

procedure UnlockListBox(AListBox: TListBox; APrevOnClick: TNotifyEvent);
begin
  AListBox.OnClick := APrevOnClick;
end;

procedure ListBoxRestorePos(AListBox: TListBox; AItemIndex, ATopIndex: Integer);
var
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := LockListBox(AListBox);
  try
    if ATopIndex <> -1 then AListBox.TopIndex := ATopIndex;
    if AItemIndex <> -1 then AListBox.ItemIndex := AItemIndex;
  finally
    UnlockListBox(AListBox, APrevOnClick);
  end;
//  AListBox.Items.EndUpdate;
end;

procedure ListBoxSavePos(AListBox: TListBox; var AItemIndex, ATopIndex: Integer);
begin
  AItemIndex := AListBox.ItemIndex;
  ATopIndex := AListBox.TopIndex;
//  AListBox.Items.BeginUpdate;
end;

procedure ListBoxGetSelection(AListBox: TListBox; AList: TList);
var
  I: Integer;
begin
  for I := 0 to AListBox.Items.Count - 1 do
    if AListBox.Selected[I] then
      AList.Add(AListBox.Items.Objects[I]);
end;

procedure ListBoxSyncSelection(AListBox: TListBox; AList: TList);
var
  I, AItemIndex, ATopIndex: Integer;
  ASelected: Boolean;
  APrevOnClick: TNotifyEvent;
begin
  ListBoxSavePos(AListBox, AItemIndex, ATopIndex);
  try
    APrevOnClick := LockListBox(AListBox);
    try
      for I := 0 to AListBox.Items.Count - 1 do
      begin
        ASelected := AList.IndexOf(AListBox.Items.Objects[I]) <> -1;
        if AListBox.Selected[I] <> ASelected then
          AListBox.Selected[I] := ASelected;
      end;
    finally
      UnlockListBox(AListBox, APrevOnClick);
    end;
    if AListBox.SelCount = 1 then
      for I := 0 to AListBox.Items.Count - 1 do
        if AListBox.Selected[I] then
        begin
          AItemIndex := I;
          Break;
        end;
  finally
    ListBoxRestorePos(AListBox, AItemIndex, ATopIndex);
  end;
end;
{$ENDIF}

{ TdxLayoutLookAndFeelListDesignForm }

constructor TdxLayoutLookAndFeelListDesignForm.Create(AOwner: TComponent);

  procedure PopulatePopupMenuItems;
  var
    I: Integer;
    AItem: TMenuItem;
  begin
    for I := 0 to dxLayoutLookAndFeelDefs.Count - 1 do
    begin
      AItem := TMenuItem.Create(PopupMenu1);
      AItem.Caption := dxLayoutLookAndFeelDefs[I].Description;
      AItem.Tag := I;
      AItem.OnClick := btnAddClick;
      PopupMenu1.Items.Add(AItem);
    end;
  end;

begin
  inherited;

{$IFDEF DXLAYOUTTEST}
  FSelection := TList.Create;
{$ENDIF}
  dxLayoutLookAndFeelsDesigner := Self;
  PopupMode := pmExplicit;

  if not IsXPManifestEnabled then
  begin
    cxTransformImages(ilMain, clBtnFace);
    tlbGroups.DisabledImages := ilMainDisabled;
    cxTransformImages(ilMainDisabled, clBtnFace, False);
  end;

  PopulatePopupMenuItems;
end;

destructor TdxLayoutLookAndFeelListDesignForm.Destroy;
begin
{$IFDEF DXLAYOUTTEST}
  FreeAndNil(FSelection);
{$ENDIF}
  dxLayoutLookAndFeelsDesigner := nil;
  inherited;
end;

procedure TdxLayoutLookAndFeelListDesignForm.AfterConstruction;
begin
  inherited;
  lbItems.ItemHeight := GetListBoxItemHeight;
end;

{$IFDEF DXLAYOUTTEST}
procedure TdxLayoutLookAndFeelListDesignForm.SelectionsChanged;
{$ELSE}
procedure TdxLayoutLookAndFeelListDesignForm.SelectionsChanged(const ASelection: TDesignerSelectionList);
{$ENDIF}
var
  AList: TList;
begin
  inherited;
  if List = nil then
    Exit;
  AList := TList.Create;
  try
    GetSelectionList(AList);
    SynchronizeSelection(AList);
  finally
    AList.Free;
  end;
end;

{$IFDEF DXLAYOUTTEST}
procedure TdxLayoutLookAndFeelListDesignForm.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxLayoutLookAndFeelListDesignForm.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxLayoutLookAndFeelListDesignForm.EndUpdate(AForceUpdate: Boolean);
begin
  Dec(FLockCount);
end;

function TdxLayoutLookAndFeelListDesignForm.IsUpdateLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxLayoutLookAndFeelListDesignForm.UpdateSelection;
begin

end;

procedure TdxLayoutLookAndFeelListDesignForm.SelectComponent(AComponent: TPersistent);
begin
  FSelection.Clear;
  FSelection.Add(AComponent);
end;

procedure TdxLayoutLookAndFeelListDesignForm.SelectComponents(AList: TList);
begin
  FSelection.Assign(AList);
end;

procedure TdxLayoutLookAndFeelListDesignForm.GetSelectionList(AList: TList);
begin
  AList.Assign(FSelection);
end;
{$ENDIF}

function TdxLayoutLookAndFeelListDesignForm.CanModify: Boolean;
begin
  Result := (List <> nil) and
  {$IFNDEF DXLAYOUTTEST}
    not Designer.IsSourceReadOnly and
  {$ENDIF}
    not (csDestroying in List.ComponentState) and
    not (csInline in List.Owner.ComponentState);
end;

function TdxLayoutLookAndFeelListDesignForm.GetListBoxItemHeight: Integer;
begin
  Result := 2 + lbItems.Canvas.TextHeight('Qq') + 2;
end;

function TdxLayoutLookAndFeelListDesignForm.GetList: TdxLayoutLookAndFeelList;
begin
  Result := TdxLayoutLookAndFeelList(Component);
end;

procedure TdxLayoutLookAndFeelListDesignForm.SynchronizeListBoxSelection;
var
  AList: TList;
begin
  if IsUpdateLocked then
    Exit;
  AList := TList.Create;
  try
    ListBoxGetSelection(lbItems, AList);
    SynchronizeSelection(AList);
  finally
    AList.Free;
  end;
end;

procedure TdxLayoutLookAndFeelListDesignForm.DeleteItems;
var
  AIndex: Integer;
  AList: TObjectList;
begin
  AIndex := lbItems.ItemIndex;
  BeginUpdate;
  try
    AList := TObjectList.Create;
    try
      GetSelectionList(AList);
    finally
      AList.Free;
    end;
  finally
    EndUpdate(False);
  end;
  Refresh;
  if lbItems.Count > 0 then
  begin
    AIndex := Min(AIndex, lbItems.Count - 1);
    lbItems.ItemIndex := AIndex;
    lbItems.Selected[AIndex] := True;
  end;
  SynchronizeListBoxSelection;
end;

procedure TdxLayoutLookAndFeelListDesignForm.Loaded;
begin
  inherited Loaded;

end;

procedure TdxLayoutLookAndFeelListDesignForm.SetComponent(AValue: TComponent);
begin
  if Component <> nil then
    Component.RemoveFreeNotification(Self);
{$IFDEF DXLAYOUTTEST}
  FComponent := AValue;
{$ELSE}
  inherited;
{$ENDIF}
  if Component <> nil then
    Component.FreeNotification(Self);
  UpdateCaption;
  Refresh(True);
end;

procedure TdxLayoutLookAndFeelListDesignForm.UpdateCaption;
begin
  Caption := cxGetFullComponentName(List) + ' - Designer';
end;

procedure TdxLayoutLookAndFeelListDesignForm.UpdateContent;
begin
  Refresh(True);
end;

procedure TdxLayoutLookAndFeelListDesignForm.Refresh(ARefreshSelection: Boolean = False);
begin
  if IsUpdateLocked or (List = nil) then
    Exit;

  RefreshListBox;
  if ARefreshSelection then
    UpdateSelection;
end;

procedure TdxLayoutLookAndFeelListDesignForm.RefreshEnableds;
var
  ACanModify: Boolean;
begin
  ACanModify := CanModify;
  SetAddItemsActionEnabled(ACanModify);
  SetDeleteItemsActionEnabled(ACanModify and (lbItems.SelCount > 0));
end;

procedure TdxLayoutLookAndFeelListDesignForm.RefreshListBox;
var
  I: Integer;
  AItem: TdxCustomLayoutLookAndFeel;
begin
  with lbItems.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to List.Count - 1 do
      begin
        AItem := List[I];
        AddObject(AItem.Name, AItem);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxLayoutLookAndFeelListDesignForm.SetAddItemsActionEnabled(AEnabled: Boolean);
begin
  acAdd.Enabled := AEnabled;
end;

procedure TdxLayoutLookAndFeelListDesignForm.SetDeleteItemsActionEnabled(AEnabled: Boolean);
begin
  acDelete.Enabled := AEnabled;
end;

procedure TdxLayoutLookAndFeelListDesignForm.SynchronizeControlsSelection(AList: TList);
begin
  ListBoxSyncSelection(lbItems, AList);
end;

procedure TdxLayoutLookAndFeelListDesignForm.SynchronizeSelection(AList: TList);
begin
  lcPreview.LayoutLookAndFeel := nil;
  SelectComponents(AList);
  SynchronizeControlsSelection(AList);
  RefreshEnableds;
  lcPreview_Root.Visible := (AList.Count = 1) and (TPersistent(AList[0]) is TdxCustomLayoutLookAndFeel) and
    ((TPersistent(AList[0]) as TdxCustomLayoutLookAndFeel).List = List);
  if lcPreview_Root.Visible then
    lcPreview.LayoutLookAndFeel := TdxCustomLayoutLookAndFeel(AList[0]);
end;

procedure TdxLayoutLookAndFeelListDesignForm.ComponentNameChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
begin
  if ALookAndFeelList = List then
    UpdateCaption;
end;

procedure TdxLayoutLookAndFeelListDesignForm.ItemsChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
begin
  if ALookAndFeelList = List then
    UpdateContent;
end;

procedure TdxLayoutLookAndFeelListDesignForm.Edit(ALookAndFeelList: TdxLayoutLookAndFeelList);
begin
  Component := ALookAndFeelList;
  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_SHOWWINDOW);
end;

procedure TdxLayoutLookAndFeelListDesignForm.btnAddClick(Sender: TObject);
var
  ALookAndFeel: TdxCustomLayoutLookAndFeel;
begin
  ALookAndFeel := List.CreateItem(dxLayoutLookAndFeelDefs.Items[(Sender as TComponent).Tag]);
  SelectComponent(ALookAndFeel);
end;

procedure TdxLayoutLookAndFeelListDesignForm.lbItemsClick(Sender: TObject);
begin
  SynchronizeListBoxSelection;
end;

procedure TdxLayoutLookAndFeelListDesignForm.acDeleteExecute(Sender: TObject);
begin
  DeleteItems;
end;

procedure TdxLayoutLookAndFeelListDesignForm.lbItemsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (lbItems.SelCount > 0) then
    DeleteItems;
end;

procedure TdxLayoutLookAndFeelListDesignForm.FormShow(Sender: TObject);
begin
  liPreview.ControlOptions.OriginalWidth := lcPreview.OccupiedClientWidth + (lcPreview.Width - lcPreview.ClientWidth) + 5 {for skins};
  liPreview.ControlOptions.OriginalHeight := lcPreview.OccupiedClientHeight + (lcPreview.Height - lcPreview.ClientHeight) + 5 {for skins};
  Width := (Width - ClientWidth) + lcMain.OccupiedClientWidth;
  Height := (Height - ClientHeight) + lcMain.OccupiedClientHeight;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  lcPreview_Root.Visible := False;
  lcMain.Align := alClient;
end;

end.
