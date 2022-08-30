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

unit cxBandedTableViewEditor;

{$I cxVer.inc}

interface

uses
  Types, Variants, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls, cxEdit,
  cxGridCustomView, cxGridBandedTableView, cxGridCustomTableView, cxViewEditor, cxCustomTableViewEditor, cxListBox,
  cxLookAndFeelPainters, cxButtons, cxPC, cxControls, cxGraphics, cxLookAndFeels, dxLayoutContainer, cxTableViewEditor,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, cxContainer, dxLayoutControl;

type
  TcxBandedTableViewEditor = class(TcxTableViewEditor)
    BAddBand: TcxButton;
    BBandMoveDown: TcxButton;
    BBandMoveUp: TcxButton;
    BDeleteBand: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    LBBands: TcxListBox;
    lgBands: TdxLayoutGroup;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MIBandsAdd: TMenuItem;
    MIBandsDelete: TMenuItem;
    MIBandsMoveDown: TMenuItem;
    MIBandsMoveUp: TMenuItem;
    MIBandsSelectAll: TMenuItem;
    PMBands: TPopupMenu;

    procedure BAddBandClick(Sender: TObject);
    procedure BBandMoveDownClick(Sender: TObject);
    procedure BBandMoveUpClick(Sender: TObject);
    procedure BDeleteBandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBBandsClick(Sender: TObject);
    procedure LBBandsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBBandsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure LBBandsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure LBBandsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure LBBandsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure LBColumnsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure MIBandsSelectAllClick(Sender: TObject);
  private
    FBandsPrevDragIndex: Integer;

    function GetBands: TcxGridBands;
    procedure ReindexBandsProc(AList: TList; ANewIndex: Integer);
    procedure UpdateBandList;
  protected
    procedure DrawListBoxItem(AListBox: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; R: TRect; AParentBand: TcxGridBand); reintroduce;
    procedure SelectAllBands;
    procedure UpdateButtons; override;
    procedure UpdateEditor; override;
    procedure UpdateSelection; override;
  public
    class function GetViewByObject(APersistent: TPersistent): TcxCustomGridView; override;
    property Bands: TcxGridBands read GetBands;
  end;

  { TcxGridBandedTableViewStorage }

  TcxGridBandedTableViewStorage = class(TcxCustomGridTableViewStorage)
  private
    function GetOptionsCustomize: TcxGridBandedTableOptionsCustomize;
    function GetOptionsView: TcxGridBandedTableOptionsView;
  protected
    function BandedTableView: TcxGridBandedTableView;
  public
    class function GetViewClass: TcxCustomGridViewClass; override;
  published
    property OptionsCustomize: TcxGridBandedTableOptionsCustomize read GetOptionsCustomize;
    property OptionsView: TcxGridBandedTableOptionsView read GetOptionsView;
  end;

  { TcxGridBandedTableViewMenuProvider }

  TcxGridBandedTableViewMenuProvider = class(TcxGridTableViewMenuProvider)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure CreateBand(Sender: TcxGridViewMenuItem);
    procedure InitStructureCreationItems; override;
  public
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

implementation

uses
  cxDesignWindows, Math;

{$R *.dfm}

{ TcxGridBandedTableViewStorage }

function TcxGridBandedTableViewStorage.GetOptionsCustomize: TcxGridBandedTableOptionsCustomize;
begin
  Result := BandedTableView.OptionsCustomize;
end;

function TcxGridBandedTableViewStorage.GetOptionsView: TcxGridBandedTableOptionsView;
begin
  Result := BandedTableView.OptionsView;
end;

function TcxGridBandedTableViewStorage.BandedTableView: TcxGridBandedTableView;
begin
  Result := inherited View as TcxGridBandedTableView;
end;

class function TcxGridBandedTableViewStorage.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridBandedTableView;
end;

{ TcxBandedTableViewEditor }

class function TcxBandedTableViewEditor.GetViewByObject(APersistent: TPersistent): TcxCustomGridView;
begin
  if APersistent is TcxGridBand then
    Result := (APersistent as TcxGridBand).Bands.GridView
  else
    if APersistent is TcxGridBands then
      Result := (APersistent as TcxGridBands).GridView
    else
      Result := inherited GetViewByObject(APersistent);
end;

procedure TcxBandedTableViewEditor.DrawListBoxItem(
  AListBox: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; R: TRect; AParentBand: TcxGridBand);

  function GetParentBandInfoOffset: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to AListBox.Count - 1 do
      Result := Max(Result, ACanvas.TextWidth(AListBox.Items[I]));
    Inc(Result, ScaleFactor.Apply(30));
  end;

  function GetParentBandInfoText: string;
  begin
    Result := '[ParentBand: ' + IntToStr(AParentBand.Index);
    if AParentBand.Caption <> '' then
      Result := Result + ' - ' + AParentBand.Caption;
    Result := Result + ']';
  end;

begin
  inherited DrawListBoxItem(AListBox, ACanvas, AIndex, R);

  if AParentBand <> nil then
  begin
    Inc(R.Left, GetParentBandInfoOffset);
    ACanvas.TextOut(R.Left, R.Top, GetParentBandInfoText);
  end;
end;

procedure TcxBandedTableViewEditor.SelectAllBands;
begin
  ListBoxSelectAll(LBBands.InnerListBox);
end;

procedure TcxBandedTableViewEditor.UpdateButtons;
begin
  inherited;
  BDeleteBand.Enabled := CanDeleteComponent(nil) and (LBBands.SelCount > 0);
  BBandMoveUp.Enabled := LBBands.SelCount > 0;
  BBandMoveDown.Enabled := LBBands.SelCount > 0;

  MIBandsMoveUp.Enabled := BBandMoveUp.Enabled;
  MIBandsMoveDown.Enabled := BBandMoveDown.Enabled;
  MIBandsSelectAll.Enabled := LBBands.SelCount < LBBands.Items.Count;
end;

procedure TcxBandedTableViewEditor.UpdateEditor;
begin
  inherited;
  UpdateBandList;
end;

procedure TcxBandedTableViewEditor.UpdateSelection;
begin
  FormEditor.ListBoxSynchronizeSelection(LBBands.InnerListBox);
  inherited;
end;

function TcxBandedTableViewEditor.GetBands: TcxGridBands;
begin
  Result := (View as TcxGridBandedTableView).Bands;
end;

procedure TcxBandedTableViewEditor.ReindexBandsProc(AList: TList; ANewIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    TcxGridBand(AList[I]).Index := ANewIndex;
  UpdateDesigner;
end;

procedure TcxBandedTableViewEditor.UpdateBandList;
begin
  ListBoxLoadCollection(LBBands.InnerListBox, Bands);
end;

procedure TcxBandedTableViewEditor.BAddBandClick(Sender: TObject);
begin
  CollectionItemsAdd(LBBands.InnerListBox, Bands);
end;

procedure TcxBandedTableViewEditor.BDeleteBandClick(Sender: TObject);
begin
  CollectionItemsDelete(LBBands.InnerListBox, Bands);
end;

procedure TcxBandedTableViewEditor.LBBandsClick(Sender: TObject);
begin
  FormEditor.ListBoxApplySelection(LBBands.InnerListBox, Bands);
end;

procedure TcxBandedTableViewEditor.BBandMoveUpClick(Sender: TObject);
begin
  ListBoxMoveUpItems(LBBands.InnerListBox, FBandsPrevDragIndex, ReindexBandsProc);
end;

procedure TcxBandedTableViewEditor.BBandMoveDownClick(Sender: TObject);
begin
  ListBoxMoveDownItems(LBBands.InnerListBox, FBandsPrevDragIndex, ReindexBandsProc);
end;

procedure TcxBandedTableViewEditor.MIBandsSelectAllClick(Sender: TObject);
begin
  SelectAllBands;
  LBBandsClick(nil);
end;

// Drag Drop Bands

procedure TcxBandedTableViewEditor.LBBandsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FBandsPrevDragIndex := -1;
end;

procedure TcxBandedTableViewEditor.LBColumnsDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  DrawListBoxItem(AControl, ACanvas, AIndex, ARect, TcxGridBandedColumn(View.Items[AIndex]).Position.Band);
end;

procedure TcxBandedTableViewEditor.LBBandsDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ListBoxDragOver(LBBands.InnerListBox, Sender, Source, X, Y, State, Accept, FBandsPrevDragIndex);
end;

procedure TcxBandedTableViewEditor.LBBandsDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  DrawListBoxItem(AControl, ACanvas, AIndex, ARect, Bands[AIndex].ParentBand);
end;

procedure TcxBandedTableViewEditor.LBBandsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  ListBoxEndDrag(LBBands.InnerListBox, Sender, Target, X, Y, FBandsPrevDragIndex);
end;

procedure TcxBandedTableViewEditor.LBBandsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ListBoxDragDrop(LBBands.InnerListBox, Sender, Source, X, Y, FBandsPrevDragIndex, ReindexBandsProc);
end;

procedure TcxBandedTableViewEditor.FormCreate(Sender: TObject);
begin
  inherited;
  CalculateListBoxItemHeight(LBBands.InnerListBox);
end;

{ TcxGridBandedTableViewMenuProvider }

function TcxGridBandedTableViewMenuProvider.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableViewMenuProvider.CreateBand(Sender: TcxGridViewMenuItem);
begin
  ObjectCreated(GridView.Bands.Add);
end;

procedure TcxGridBandedTableViewMenuProvider.InitStructureCreationItems;
begin
  inherited;
  Items.AddItem('Create Band', CreateBand);
end;

initialization
  RegisterViewEditorClass(TcxGridBandedTableView, TcxBandedTableViewEditor);
  RegisterDefaultViewStorage(TcxGridBandedTableViewStorage);
  RegisterViewMenuProviderClass(TcxGridBandedTableView, TcxGridBandedTableViewMenuProvider);

finalization
  UnregisterViewMenuProviderClass(TcxGridBandedTableView, TcxGridBandedTableViewMenuProvider);
  UnregisterDefaultViewStorage(TcxGridBandedTableViewStorage);
  UnregisterViewEditorClass(TcxGridBandedTableView, TcxBandedTableViewEditor);
end.
