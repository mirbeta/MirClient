{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxFilterBox;

interface

uses
  Classes, Controls, cxClasses, dxCoreClasses, cxCustomData, dxUIElementPopupWindow, cxListBox;

type
  TdxFilterBoxMRUItemsPopup = class;

  TcxFilterBoxPosition = (fpTop, fpBottom);
  TcxFilterBoxButtonAlignment = (fbaLeft, fbaRight);
  TcxFilterBoxVisible = (fvNever, fvNonEmpty, fvAlways);

  { TdxFilterBoxMRUItem }

  TdxFilterBoxMRUItem = class(TcxMRUItem)
  private
    FFilter: TcxDataFilterCriteria;

    function GetCaption: string;
    function GetFilterStream(AFilter: TcxDataFilterCriteria): TMemoryStream;
  protected
    function StreamEquals(AStream: TMemoryStream): Boolean;
  public
    constructor Create(AFilter: TcxDataFilterCriteria);
    destructor Destroy; override;

    procedure AssignTo(AFilter: TcxDataFilterCriteria);

    function Equals(AItem: TcxMRUItem): Boolean; override;
    function FilterEquals(AFilter: TcxDataFilterCriteria): Boolean;
    function GetStream: TMemoryStream;

    property Caption: string read GetCaption;
    property Filter: TcxDataFilterCriteria read FFilter;
  end;
  TdxFilterBoxMRUItemClass = class of TdxFilterBoxMRUItem;

  { TdxFilterBoxMRUItems }

  TdxFilterBoxMRUItems = class(TcxMRUItems)
  private
    FVisibleItems: TdxFastList;

    function GetItem(Index: Integer): TdxFilterBoxMRUItem; inline;
    function GetVisibleCount: Integer; inline;
    function GetVisibleItem(Index: Integer): TdxFilterBoxMRUItem; inline;
  protected
    procedure AddCurrentFilter; virtual;
    function GetCurrentFilter: TcxDataFilterCriteria; virtual;
    function GetItemClass: TdxFilterBoxMRUItemClass; virtual;
    procedure DeleteEmptyItems; virtual;
    procedure FilterChanged; virtual;
    procedure RefreshVisibleItemsList; virtual;
    procedure SetMaxCount(AMaxCount: Integer); virtual;
    procedure VisibleCountChanged(APrevVisibleCount: Integer); virtual;

    property CurrentFilter: TcxDataFilterCriteria read GetCurrentFilter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(AFilter: TcxDataFilterCriteria);

    property Items[Index: Integer]: TdxFilterBoxMRUItem read GetItem; default;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TdxFilterBoxMRUItem read GetVisibleItem;
  end;
  TdxFilterBoxMRUItemsClass = class of TdxFilterBoxMRUItems;

  { TdxFilterBoxMRUItemsPopupListBox }

  TdxFilterBoxMRUItemsPopupListBox = class(TdxCustomCheckListBox)
  strict private
    function GetPopup: TdxFilterBoxMRUItemsPopup;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function NeedHotTrack: Boolean; override;
  public
    constructor Create(APopup: TdxFilterBoxMRUItemsPopup); reintroduce; virtual;

    property Popup: TdxFilterBoxMRUItemsPopup read GetPopup;
  end;
  TdxFilterBoxMRUItemsPopupListBoxClass = class of TdxFilterBoxMRUItemsPopupListBox;

  { TdxFilterBoxMRUItemsPopup }

  TdxFilterBoxMRUItemsPopup = class(TdxUIElementPopupWindow)
  private
    FListBox: TdxFilterBoxMRUItemsPopupListBox;

    procedure ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
  protected
    procedure ApplyFilter(AItemIndex: Integer); virtual;
    procedure AddFilterMRUItems; virtual;
    procedure ApplyFilterMRUItem(AItemIndex: Integer); virtual;
    function GetListBoxClass: TdxFilterBoxMRUItemsPopupListBoxClass; virtual;
    function GetMRUItemCount: Integer; virtual;
    function GetMRUItems: TdxFilterBoxMRUItems; virtual;
    function GetTextOffsetHorz: Integer; virtual;
    procedure InitListBox; virtual;
    procedure InitPopup; override;
    procedure UpdateInnerControlsHeight(var AClientHeight: Integer); override;

    property ListBox: TdxFilterBoxMRUItemsPopupListBox read FListBox;
    property MRUItems: TdxFilterBoxMRUItems read GetMRUItems;
  public
    constructor Create(AOwnerControl: TWinControl); override;

    property TextOffsetHorz: Integer read GetTextOffsetHorz;
  end;
  TdxFilterBoxMRUItemsPopupClass = class of TdxFilterBoxMRUItemsPopup;

implementation

uses
  Windows, SysUtils, RTLConsts, Math, cxControls;

type
  TdxCustomCheckListBoxAccess = class(TdxCustomCheckListBox);

{ TdxFilterBoxMRUItem }

constructor TdxFilterBoxMRUItem.Create(AFilter: TcxDataFilterCriteria);
begin
  inherited Create;
  FFilter := AFilter.DataController.CreateFilter;
  Filter.Assign(AFilter);
end;

destructor TdxFilterBoxMRUItem.Destroy;
begin
  FreeAndNil(FFilter);
  inherited Destroy;
end;

procedure TdxFilterBoxMRUItem.AssignTo(AFilter: TcxDataFilterCriteria);
begin
  AFilter.AssignItems(Filter);
end;

function TdxFilterBoxMRUItem.Equals(AItem: TcxMRUItem): Boolean;
begin
  Result := StreamEquals(TdxFilterBoxMRUItem(AItem).GetStream);
end;

function TdxFilterBoxMRUItem.FilterEquals(AFilter: TcxDataFilterCriteria): Boolean;
begin
  Result := StreamEquals(GetFilterStream(AFilter));
end;

function TdxFilterBoxMRUItem.GetStream: TMemoryStream;
begin
  Result := GetFilterStream(Filter);
end;

function TdxFilterBoxMRUItem.StreamEquals(AStream: TMemoryStream): Boolean;
var
  AOwnStream: TMemoryStream;
begin
  AOwnStream := GetStream;
  try
    Result := StreamsEqual(AOwnStream, AStream);
  finally
    AStream.Free;
    AOwnStream.Free;
  end;
end;

function TdxFilterBoxMRUItem.GetCaption: string;
begin
  Result := Filter.FilterCaption;
end;

function TdxFilterBoxMRUItem.GetFilterStream(AFilter: TcxDataFilterCriteria): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  AFilter.WriteData(Result);
end;

{ TdxFilterBoxMRUItems }

constructor TdxFilterBoxMRUItems.Create;
begin
  inherited Create;
  FVisibleItems := TdxFastList.Create;
end;

destructor TdxFilterBoxMRUItems.Destroy;
begin
  FVisibleItems.Free;
  inherited Destroy;
end;

function TdxFilterBoxMRUItems.GetItem(Index: Integer): TdxFilterBoxMRUItem;
begin
  Result := TdxFilterBoxMRUItem(inherited Items[Index]);
end;

function TdxFilterBoxMRUItems.GetVisibleCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TdxFilterBoxMRUItems.GetVisibleItem(Index: Integer): TdxFilterBoxMRUItem;
begin
  Result := TdxFilterBoxMRUItem(FVisibleItems[Index]);
end;

procedure TdxFilterBoxMRUItems.AddCurrentFilter;
begin
  Add(CurrentFilter);
end;

function TdxFilterBoxMRUItems.GetCurrentFilter: TcxDataFilterCriteria;
begin
  Result := nil;
end;

function TdxFilterBoxMRUItems.GetItemClass: TdxFilterBoxMRUItemClass;
begin
  Result := TdxFilterBoxMRUItem;
end;

procedure TdxFilterBoxMRUItems.DeleteEmptyItems;
var
  APrevCount, I: Integer;
begin
  APrevCount := Count;
  for I := Count - 1 downto 0 do
    if Items[I].Filter.IsEmpty then
      Delete(I);
  if Count <> APrevCount then
    RefreshVisibleItemsList;
end;

procedure TdxFilterBoxMRUItems.FilterChanged;
begin
  RefreshVisibleItemsList;
end;

procedure TdxFilterBoxMRUItems.RefreshVisibleItemsList;
var
  APrevVisibleCount: Integer;
  I: Integer;
  AItem: TdxFilterBoxMRUItem;
begin
  APrevVisibleCount := VisibleCount;
  FVisibleItems.Clear;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if not ((CurrentFilter <> nil) and AItem.FilterEquals(CurrentFilter)) then
      FVisibleItems.Add(AItem);
  end;
  if VisibleCount <> APrevVisibleCount then
    VisibleCountChanged(APrevVisibleCount);
end;

procedure TdxFilterBoxMRUItems.SetMaxCount(AMaxCount: Integer);
begin
  MaxCount := AMaxCount;
  FVisibleItems.Count := Min(VisibleCount, AMaxCount);
end;

procedure TdxFilterBoxMRUItems.VisibleCountChanged(APrevVisibleCount: Integer);
begin
//do nothing
end;

procedure TdxFilterBoxMRUItems.Add(AFilter: TcxDataFilterCriteria);
begin
  if not AFilter.IsEmpty then
  begin
    inherited Add(GetItemClass.Create(AFilter));
    RefreshVisibleItemsList;
  end;
end;

{ TdxFilterBoxMRUItemsPopupListBox }

constructor TdxFilterBoxMRUItemsPopupListBox.Create(APopup: TdxFilterBoxMRUItemsPopup);
begin
  inherited Create(APopup);
end;

procedure TdxFilterBoxMRUItemsPopupListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      Popup.CloseUp;
  end;
end;

function TdxFilterBoxMRUItemsPopupListBox.NeedHotTrack: Boolean;
begin
  Result := True;
end;

function TdxFilterBoxMRUItemsPopupListBox.GetPopup: TdxFilterBoxMRUItemsPopup;
begin
  Result := TdxFilterBoxMRUItemsPopup(Owner);
end;

{ TdxFilterBoxMRUItemsPopup }

constructor TdxFilterBoxMRUItemsPopup.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  FListBox := GetListBoxClass.Create(Self);
  InitListBox;
end;

procedure TdxFilterBoxMRUItemsPopup.ListBoxAction(Sender: TdxCustomListBox; AItemIndex: Integer);
begin
  ApplyFilterMRUItem(AItemIndex);
  CloseUp;
end;

procedure TdxFilterBoxMRUItemsPopup.ApplyFilter(AItemIndex: Integer);
begin
  TdxFilterBoxMRUItem(ListBox.Items[AItemIndex].Data).AssignTo(MRUItems.CurrentFilter);
end;

procedure TdxFilterBoxMRUItemsPopup.AddFilterMRUItems;
var
  I: Integer;
  AItem: TdxFilterBoxMRUItem;
begin
  if MRUItems = nil then
    Exit;
  ListBox.BeginUpdate;
  try
    ListBox.Clear;
    for I := 0 to MRUItems.VisibleCount - 1 do
    begin
      AItem := MRUItems.VisibleItems[I];
      ListBox.Items.AddObject(AItem.Caption, AItem);
    end;
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TdxFilterBoxMRUItemsPopup.ApplyFilterMRUItem(AItemIndex: Integer);
begin
  if MRUItems = nil then
    Exit;
  ApplyFilter(AItemIndex);
  MRUItems.AddCurrentFilter;
end;

function TdxFilterBoxMRUItemsPopup.GetListBoxClass: TdxFilterBoxMRUItemsPopupListBoxClass;
begin
  Result := TdxFilterBoxMRUItemsPopupListBox;
end;

function TdxFilterBoxMRUItemsPopup.GetMRUItemCount: Integer;
begin
  Result := 0;
end;

function TdxFilterBoxMRUItemsPopup.GetMRUItems: TdxFilterBoxMRUItems;
begin
  Result := nil;
end;

function TdxFilterBoxMRUItemsPopup.GetTextOffsetHorz: Integer;
begin
  Result := TdxCustomCheckListBoxAccess(ListBox).TextOffsets.Left;
end;

procedure TdxFilterBoxMRUItemsPopup.InitListBox;
begin
  ListBox.BorderStyle := cxcbsNone;
  ListBox.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  ListBox.ShowCheckBoxes := False;
  ListBox.Parent := Self;
  ListBox.OnAction := ListBoxAction;
end;

procedure TdxFilterBoxMRUItemsPopup.InitPopup;
begin
  inherited;
  ListBox.BeginUpdate;
  try
    AddFilterMRUItems;
    ListBox.VisibleItemCount := GetMRUItemCount;
    ListBox.AutoSize := True;
    ListBox.Constraints.MinWidth := ClientMinWidth;
  finally
    ListBox.EndUpdate;
  end;
end;

procedure TdxFilterBoxMRUItemsPopup.UpdateInnerControlsHeight(var AClientHeight: Integer);
begin
  ListBox.Height := AClientHeight;
  AClientHeight := ListBox.Height;
end;

end.
